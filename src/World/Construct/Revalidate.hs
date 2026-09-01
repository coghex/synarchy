{-# LANGUAGE Strict #-}
-- | Continuous validity for STRUCTURE construction designations (#1844),
--   and the shared cleanup one invalidation performs.
--
--   Admission used to be the only check a construction designation ever
--   got. Afterwards nothing re-read anything: a job survived its
--   captured surface changing, the floor under a post being removed, its
--   slot being filled by someone else, and its pack's art or build
--   metadata disappearing. This is the other half of the resolver —
--   "World.Construct.Plan" decides, and this applies that decision to
--   the live map.
--
--   Three properties are deliberate:
--
--     * SCOPED BY DEFAULT. A live mutation hook passes the canonical
--       keys whose inputs actually changed, so a tile edit does not
--       rescan every construction job on the page (requirement 9). Load
--       and catalogue reconciliation pass 'ConstructWholePage', which is
--       the bounded page-level sweep those two are explicitly allowed.
--     * UNLOADED TERRAIN RETAINS. 'PlanUnresolvedTerrain' is not a
--       refusal — the record stays, non-actionable, and is re-checked
--       when its chunk publishes. Only a RESIDENT contradiction removes
--       anything, exactly as "World.Plant.Validate" treats unknown soil.
--     * BUILDINGS ARE NOT TOUCHED. Building planning and building
--       designation invalidation are DTV-10's scope; a @CtBuilding@
--       entry is skipped here, not resolved and not removed.
--
--   Removal IS the cancellation protocol, the same way it is for plant
--   designations: the build AI drops its job the moment
--   @construction.getDesignationAt@ comes back empty, and since #1844
--   every delayed operation it may already have issued is attempt-
--   guarded, so nothing of the removed attempt can reach a successor.
module World.Construct.Revalidate
    ( ConstructScope(..)
    , ConstructRefundDeps(..)
    , constructRefundDeps
    , constructPlanWorld
    , constructPlanWorldWith
    , pruneConstructDesignations
    , revalidateConstructDesignations
    , clearConstructDesignationSlope
    , refundConstructDesignation
    , spawnReceiptItems
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import Engine.PlayerEvent.Emit (emitEventAt)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Engine.Core.State (EngineEnv, freshItemInstanceId)
import Item.Ground (spawnGroundItem)
import Item.Materialize (materializeItem, pristineItem)
import Item.Types (ItemManager(..))
import System.Random (StdGen)

import Structure.ArtCatalog (StructureArtCatalog)
import World.Construct.Apply (clearConstructSlope)
import World.Construct.Plan
    ( PlanOp(..), PlanOutcome(..), PlanResult(..), PlanWorld(..)
    , resolveStructurePlan )
import World.Construct.Receipt (MaterialReceipt, receiptItems)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructDesignations, ConstructStatus(..)
    , ConstructTarget(..), constructDesignationReceipt )
import World.Generate (globalToChunk)
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import World.Types

-- | Which designations one sweep looks at.
data ConstructScope
    = ConstructKeys ![(Int, Int)]
      -- ^ Only these canonical keys, whatever else the page holds. What
      --   every LIVE mutation hook passes: a tile edit changed the
      --   inputs of the tiles it touched and of nothing else.
    | ConstructChunks ![ChunkCoord]
      -- ^ Every designation stored in these chunks. What a per-CHUNK
      --   write passes — a settled fluid writeback replaces a whole
      --   chunk's surface map, so its scope is the chunk rather than any
      --   one tile, and enumerating the chunk's 256 tile coords to say
      --   so would cost more than filtering the (tens of) designations
      --   the page actually holds.
    | ConstructWholePage
      -- ^ Every entry. Reserved for load and catalogue reconciliation,
      --   and for chunk publication — where the set of records whose
      --   terrain just became resident is not derivable from the
      --   publication alone once eviction and the seam are in play.
    deriving (Show, Eq)

-- | Everything minting a REFUND item needs, as explicit values.
--
--   Deliberately not an 'EngineEnv': "World.Load.Stage" reconciles a
--   staged page and is documented as never touching a live env ref, and
--   the world thread must not acquire unit\/combat access merely by way
--   of minting an item — the same reason @spawnYieldItems@ takes its RNG
--   as a narrow parameter.
data ConstructRefundDeps = ConstructRefundDeps
    { crdItems   ∷ !ItemManager
    , crdLogger  ∷ !LoggerState
    , crdRng     ∷ !(IORef StdGen)
    , crdAllocId ∷ !(IO Word64)
    }

-- | The live session's refund dependencies, read once from the engine.
constructRefundDeps ∷ EngineEnv → IO ConstructRefundDeps
constructRefundDeps env = do
    im ← readReadOnlyRef (crvItemManagerRef
                            (toContentRegistriesViewCapability env))
    logger ← readIORef (ccLoggerRef (toCoreCapability env))
    pure ConstructRefundDeps
        { crdItems   = im
        , crdLogger  = logger
        , crdRng     = ucStatRNGRef (toUnitCombatCapability env)
        , crdAllocId = freshItemInstanceId env
        }

-- | Snapshot the coherent world one sweep resolves against: the tiles,
--   the read-your-writes structure staging, the designation map and the
--   registered art\/buildability catalogue, all read once.
constructPlanWorld ∷ EngineEnv → WorldState → IO PlanWorld
constructPlanWorld env ws = do
    cat ← readIORef (rhStructureArtCatalogRef (toRenderHandoffCapability env))
    constructPlanWorldWith cat ws

-- | 'constructPlanWorld' with the catalogue supplied explicitly, for the
--   staging path.
constructPlanWorldWith ∷ StructureArtCatalog → WorldState → IO PlanWorld
constructPlanWorldWith cat ws = do
    worldSize ← pageWrapWorldSize ws
    tiles ← readIORef (wsTilesRef ws)
    stage ← readIORef (wsStructureStageRef ws)
    designations ← readIORef (wsConstructDesignationsRef ws)
    pure PlanWorld
        { pwWorldSize    = worldSize
        , pwTiles        = tiles
        , pwStage        = stage
        , pwDesignations = designations
        , pwCatalog      = cat
        }

-- | Split a designation map against the current world: the records that
--   survive, and the ones whose plan is RESIDENTLY invalid, each with
--   the resolver's reason.
--
--   Pure, so the invalidation rule is testable without an engine and the
--   sweep below has exactly one decision point.
--
--   Each structure entry is resolved with 'PlanForAttempt' carrying its
--   OWN attempt id, which is what stops a designation from cancelling
--   itself for occupying its own tile (requirement 7). Its captured
--   'cdZ' is the required surface level, never a freshly derived one:
--   requirement 4 makes surface drift a cancellation, not a silent
--   vertical retarget.
pruneConstructDesignations
    ∷ PlanWorld → ConstructScope → ConstructDesignations
    → (ConstructDesignations, [((Int, Int), ConstructDesignation, Text)])
pruneConstructDesignations pw scope designations =
    let candidates = case scope of
            ConstructWholePage → HM.toList designations
            ConstructKeys keys →
                [ (k', cd)
                | (gx, gy) ← keys
                , let k' = canonicalTile (pwWorldSize pw) gx gy
                , Just cd ← [HM.lookup k' designations] ]
            ConstructChunks coords →
                let want = HS.fromList
                        [ wrapChunkCoordU (pwWorldSize pw) c | c ← coords ]
                in [ (k, cd)
                   | (k@(gx, gy), cd) ← HM.toList designations
                   , let (coord, _, _) = canonicalTileFrame (pwWorldSize pw) gx gy
                   , coord `HS.member` want ]
        lost = [ (k, cd, prReason r)
               | (k, cd) ← candidates
               , CtStructure piece ← [cdTarget cd]
                 -- Requirement 18: a designation inside its final
                 -- placement hand-off is the CLAIMANT's, not ours. Its
                 -- own accepted read-your-writes placement is visible
                 -- here the instant it is staged, and an occupancy check
                 -- would read the worker's success as an external
                 -- conflict — cancelling the job and refunding materials
                 -- that were correctly spent. The window is one Lua
                 -- callback wide (beginPlacement → place → complete), so
                 -- skipping it defers nothing meaningfully.
               , cdStatus cd ≢ CsPlacing
               , let r = resolveStructurePlan pw (PlanForAttempt (cdAttempt cd))
                             (cdZ cd) piece k
               , prOutcome r ≡ PlanVisibleInvalid
                   ∨ prOutcome r ≡ PlanMissingArt ]
    in ( foldl' (\m (k, _, _) → HM.delete k m) designations lost, lost )

-- | Re-resolve the scoped structure designations and remove every one
--   the current world contradicts, clearing each removed attempt's own
--   progress slope and refunding its receipt exactly once.
--
--   Returns the removed keys, which are also logged. Refunds land as
--   ground items on the page the designation belonged to — the same
--   place @scripts\/unit_ai_construct.lua@'s refund puts them.
--
--   Call after any successful live write that can change a designated
--   tile's resolved surface, its structure occupancy or its supporting
--   floor, and whenever terrain becomes resident or the catalogue
--   changes.
revalidateConstructDesignations
    ∷ EngineEnv → LoggerState → WorldState → ConstructScope → IO [(Int, Int)]
revalidateConstructDesignations env logger ws scope = do
    designations ← readIORef (wsConstructDesignationsRef ws)
    if HM.null designations then pure [] else do
        pw ← constructPlanWorld env ws
        removed ← atomicModifyIORef' (wsConstructDesignationsRef ws) $
            \current →
                let (kept, gone) = pruneConstructDesignations pw scope current
                in (kept, gone)
        deps ← constructRefundDeps env
        forM_ removed $ \(key@(gx, gy), cd, reason) → do
            clearConstructDesignationSlope ws key cd
            refundConstructDesignation deps ws key cd
            -- F4 (#646): a designation vanishing under the player is
            -- exactly the kind of silent outcome the action-outcome ring
            -- exists to surface. It gets its own KIND rather than
            -- borrowing @construction.designate@: nobody asked for a
            -- designation here, the world withdrew one.
            recordConstructInvalidation env gx gy reason
            -- …and, when materials really came back, a PLAYER-facing
            -- line. The F4 ring is a debug oracle; a designation the
            -- world withdrew after its cost was already spent is
            -- something the player has to be told about, exactly as the
            -- worker-side placement failure it replaces always was.
            forM_ (constructDesignationReceipt cd) $ \_ →
                emitEventAt env "unit_warning" "construct.invalidate"
                    "Construction site changed — materials returned to \
                    \the ground"
                    (Just (gx, gy))
            logDebug logger CatWorld $
                "Construct designation invalidated at (" <> tshow gx <> ","
                <> tshow gy <> "): " <> reason
        pure [ k | (k, _, _) ← removed ]

-- | One removed designation, on the F4 action-outcome ring.
recordConstructInvalidation ∷ EngineEnv → Int → Int → Text → IO ()
recordConstructInvalidation env gx gy reason = do
    gt ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    pushActionOutcome (ucActionOutcomeRef (toUnitCombatCapability env))
        ActionOutcome
            { aoTs        = gt
            , aoKind      = "construction.invalidate"
            , aoOutcome   = "rejected"
            , aoWhereX    = Just (fromIntegral gx)
            , aoWhereY    = Just (fromIntegral gy)
            , aoTarget    = Nothing
            , aoRequested = Just 1
            , aoApplied   = Just 0
            , aoDropped   = Just 1
            , aoReason    = Just reason
            , aoHandler   = Nothing
            }

-- | Reset a removed designation's corner-progress display to flat
--   (guarded inside 'clearConstructSlope' to that designation's own
--   mask, so natural\/authored slopes are untouched).
--
--   Takes a CANONICAL tile coord: every caller canonicalises before
--   touching the designation map, and 'clearConstructSlope' indexes the
--   resolved chunk with the same coord. A no-op when the chunk is not
--   loaded — the load path re-derives the display instead.
--
--   Deliberately NOT a #1858 plant revalidation point: it passes FULL
--   corners, so @applyCornerSlopeToChunk@ leaves @ctVeg@ alone.
clearConstructDesignationSlope
    ∷ WorldState → (Int, Int) → ConstructDesignation → IO ()
clearConstructDesignationSlope ws (gx, gy) cd = do
    let (coord, _) = globalToChunk gx gy
    td ← readIORef (wsTilesRef ws)
    case lookupChunk coord td of
        Nothing → pure ()
        Just lc → do
            let lc' = clearConstructSlope (gx, gy) cd lc
            atomicModifyIORef' (wsTilesRef ws) $ \w → (insertChunk lc' w, ())
            bumpQuadCacheGen ws
            writeIORef (wsZoomQuadCacheRef ws) Nothing
            writeIORef (wsBgQuadCacheRef ws)   Nothing

-- | Return a removed designation's PAID materials to the ground at its
--   own tile, from its receipt and only its receipt. A designation with
--   no receipt refunds nothing, which is the whole point of receipt
--   presence being the paid state.
refundConstructDesignation
    ∷ ConstructRefundDeps → WorldState → (Int, Int) → ConstructDesignation
    → IO ()
refundConstructDesignation deps ws (gx, gy) cd =
    forM_ (constructDesignationReceipt cd) $ \receipt →
        spawnReceiptItems deps ws
            (fromIntegral gx + 0.5) (fromIntegral gy + 0.5) receipt

-- | Spawn one receipt's materials as ground items at a world position.
--
--   Goes through 'materializeItem' like every other production mint
--   site (#1418), so a refunded item arrives with its definition's own
--   authored contents rather than a hand-built record. A material whose
--   item definition no longer exists spawns nothing: the load boundary
--   is where that is REJECTED (requirement 21), and silently dropping it
--   here would be a second, quieter policy.
spawnReceiptItems
    ∷ ConstructRefundDeps → WorldState → Float → Float → MaterialReceipt
    → IO ()
spawnReceiptItems deps ws x y receipt = do
    let im = crdItems deps
    forM_ (receiptItems receipt) $ \name →
        when (HM.member name (imDefs im)) $ do
            mInst ← materializeItem im (crdLogger deps) (crdRng deps)
                        (crdAllocId deps) pristineItem name
            forM_ mInst $ \inst →
                atomicModifyIORef' (wsGroundItemsRef ws) $ \gis →
                    let (gis', _) = spawnGroundItem inst x y gis in (gis', ())
