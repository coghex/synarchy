{-# LANGUAGE Strict #-}
-- | The construction-designation PAYMENT verb (#1844), split out of
--   "Engine.Scripting.Lua.API.Construct" because it is the one place in
--   the @construction.*@ namespace that mutates a unit's inventory, and
--   because its losslessness argument is the thing worth reading on its
--   own.
module Engine.Scripting.Lua.API.Construct.Payment
    ( constructPayMaterialsFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.API.Construct (readAttemptArg)
import Engine.Scripting.Lua.API.Units.Inventory (insertAt, popFirstByNameIx)
import Item.Types (ItemInstance)
import Structure.ArtCatalog (BuildCost(..), packKindBuild)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.Construct.Attempt (ConstructAttemptId)
import World.Construct.Receipt (ConstructPayment(..), mkMaterialReceipt)
import World.Construct.Types
    (ConstructDesignation(..), ConstructTarget(..), StructurePiece(..))
import World.Generate.Coordinates (canonicalTile)
import World.Page.Types (WorldPageId(..))
import World.Types
    (WorldManager(..), WorldState(..), pageWrapWorldSize
    , wsConstructDesignationsRef)

-- | @construction.payMaterials(pageId, gx, gy, attempt, uid) → bool@ —
--   pay ONE exact attempt's material cost out of one unit's inventory
--   and record the durable RECEIPT, in a single lossless step (#1844).
--
--   This replaces #799's @setMaterialsPaid(page, gx, gy, true)@, and the
--   replacement is not cosmetic. Two things were wrong with the old
--   shape:
--
--     * The AI removed inventory FIRST and set the durable marker
--       afterwards, so a cancellation landing between the two refunded
--       nothing for a cost the worker's inventory had already lost. That
--       window used to be closed by both callers running on the single
--       Lua thread; #1844 adds a WORLD-thread invalidator, which that
--       argument does not cover.
--     * The marker was a 'Bool'. A refund therefore had to re-read the
--       pack YAML to learn what to give back, which cannot reproduce
--       what was actually spent once the pack's costs change or the pack
--       goes away.
--
--   The order here closes both. Materials leave the inventory in ONE
--   all-or-nothing atomic step (a shortfall commits nothing at all), and
--   only then is the receipt written — as a compare-and-set on the
--   designation, guarded on the attempt AND on it still being unpaid.
--   So:
--
--     * If a cancellation wins, the CAS fails and every popped instance
--       is spliced back at its original index; the canceller saw no
--       receipt, refunded nothing, and nothing was lost.
--     * If payment wins, the receipt is durable and IMMEDIATELY visible
--       to a racing @cancelDesignationForRefund@ pop, which refunds it
--       exactly once.
--
--   The cost comes from the REGISTERED build metadata (#1842's
--   catalogue), never from an argument: what a job costs is engine
--   state, and a caller that could name its own cost could write a
--   receipt for materials it never had.
--
--   Returns false — removing nothing — when the page, unit, designation
--   or build metadata is missing, when the attempt does not match, when
--   the designation is already paid or is not a structure target, or
--   when the unit cannot cover the cost.
constructPayMaterialsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructPayMaterialsFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    attArg ← readAttemptArg 4
    uidArg ← Lua.tointeger 5
    let wsc = toWorldSimCapability env
    paid ← case (pageIdArg, gxArg, gyArg, attArg, uidArg) of
        (Just pageIdBS, Just gxN, Just gyN, Just attempt, Just uidN) →
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    uid = UnitId (fromIntegral uidN)
                mgr ← readIORef (wsWorldManagerRef wsc)
                case lookup pageId (wmWorlds mgr) of
                    Nothing → pure False
                    Just ws → do
                        worldSize ← pageWrapWorldSize ws
                        let key = canonicalTile worldSize (round gxN) (round gyN)
                        designations ← readIORef (wsConstructDesignationsRef ws)
                        cat ← readIORef (rhStructureArtCatalogRef
                                           (toRenderHandoffCapability env))
                        case HM.lookup key designations of
                            Just cd
                              | cdAttempt cd ≡ attempt
                              , cdPayment cd ≡ CpUnpaid
                              , CtStructure piece ← cdTarget cd
                              , Just cost ← packKindBuild cat (spPack piece)
                                                              (spKind piece) →
                                  payFrom env ws key attempt
                                          (bcMaterials cost) uid
                            _ → pure False
        _ → pure False
    Lua.pushboolean paid
    return 1

-- | The lossless half: pop the exact instances, CAS the receipt, splice
--   them back on failure.
payFrom ∷ EngineEnv → WorldState → (Int, Int) → ConstructAttemptId
        → [(Text, Int)] → UnitId → IO Bool
payFrom env ws key attempt materials uid = do
    let wanted = [ name | (name, n) ← materials, _ ← [1 .. n] ]
    -- All or nothing, inside ONE update of the unit manager: a partial
    -- shortfall must leave the inventory byte-for-byte unchanged, so the
    -- whole removal is computed purely and only then committed.
    popped ← atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $
        \um → case HM.lookup uid (umInstances um) of
            Nothing → (um, Nothing)
            Just u  → case popAll wanted (uiInventory u) of
                Nothing → (um, Nothing)
                Just (taken, inv') →
                    ( um { umInstances =
                             HM.insert uid (u { uiInventory = inv' })
                                       (umInstances um) }
                    , Just taken )
    case popped of
        Nothing → pure False
        Just taken → do
            let receipt = mkMaterialReceipt materials
            ok ← atomicModifyIORef' (wsConstructDesignationsRef ws) $ \m →
                case HM.lookup key m of
                    Just cd | cdAttempt cd ≡ attempt
                            , cdPayment cd ≡ CpUnpaid →
                        (HM.insert key (cd { cdPayment = CpPaid receipt }) m
                        , True)
                    _ → (m, False)
            unless ok $ restore taken
            pure ok
  where
    -- The attempt went away under us. Put every instance back where it
    -- came from, so the inventory ends byte-for-byte as it started.
    --
    -- The order is REVERSE OF REMOVAL, not descending by index. Each
    -- recorded index is relative to the list AS IT WAS when that
    -- instance was popped, because 'popAll' pops from the already
    -- shortened list each time — so undoing the removals in reverse is
    -- what makes every splice land in the list its index was measured
    -- against. (Sorting by index instead is the right undo only when
    -- every index shares one original list, which is the shape the
    -- cross-owner transfer rollback has and this one does not.)
    restore taken =
        atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $
            \um → case HM.lookup uid (umInstances um) of
                Nothing → (um, ())
                Just u  →
                    let inv = foldl' (\acc (inst, i) → insertAt i inst acc)
                                     (uiInventory u) (reverse taken)
                    in ( um { umInstances =
                                HM.insert uid (u { uiInventory = inv })
                                          (umInstances um) }
                       , () )

    -- Pop every wanted def name, remembering each instance's original
    -- index. 'Nothing' the moment one is missing — the caller commits
    -- nothing in that case.
    popAll ∷ [Text] → [ItemInstance]
           → Maybe ([(ItemInstance, Int)], [ItemInstance])
    popAll [] inv = Just ([], inv)
    popAll (name : rest) inv = case popFirstByNameIx name inv of
        Nothing → Nothing
        Just (inst, i, inv') → do
            (taken, inv'') ← popAll rest inv'
            pure ((inst, i) : taken, inv'')
