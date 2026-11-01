{-# LANGUAGE Strict #-}
-- | The flora condition mutation seam (#2555, EFM-7): the world thread's
--   handlers for 'World.Command.Types.WorldSetFloraCondition' and
--   'World.Command.Types.WorldClearFloraCondition'.
--
--   These are the ONLY writers of a persistent flora condition — of the
--   identity-keyed 'wsFloraConditionsRef' record and of a groundcover
--   plot's embedded record. Lua only enqueues
--   ('Engine.Scripting.Lua.API.Forage.Condition'); the world thread owns
--   the state. What a set records is decided by the pure half
--   ("World.Flora.ConditionSeam"); this module resolves the occurrence,
--   runs the transition atomically against the live store, invalidates
--   the quad cache, and records an action outcome — accepted, no-op, or
--   a rejection that names its reason. A refusal changes NOTHING and
--   leaves the cache alone.
--
--   There is deliberately no hazard producer here, no expiry sweep and no
--   reseed: a record persists until it is cleared (EFM-10 owns the rest).
module World.Thread.Command.Flora.Condition
    ( handleWorldSetFloraConditionCommand
    , handleWorldClearFloraConditionCommand
    ) where

import UPrelude
import Gameplay.Tags.Page (CropPlotStore(..))
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Engine.Core.State (EngineEnv)
import World.Types
import World.Flora.Clock (growthCalendar)
import World.Flora.Condition
    (FloraConditionRecordOf(..), lookupFloraCondition)
import World.Flora.ConditionSeam
import World.Flora.CropPlot (CropPlotOf(..))
import World.Generate.Coordinates (canonicalTileFrame)

-- | The kinds the action-outcome ring reports these under.
kindSet, kindClear ∷ Text
kindSet   = "flora.condition.set"
kindClear = "flora.condition.clear"

-- | One outcome record: @outcome@ is @accepted@, @noop@ or @rejected@.
recordOutcome ∷ EngineEnv → Text → Text → Int → Int → Maybe Text → IO ()
recordOutcome env kind outcome gx gy reason = do
    gt ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    pushActionOutcome (ucActionOutcomeRef (toUnitCombatCapability env))
        ActionOutcome
            { aoTs        = gt
            , aoKind      = kind
            , aoOutcome   = outcome
            , aoWhereX    = Just (fromIntegral gx)
            , aoWhereY    = Just (fromIntegral gy)
            , aoTarget    = Nothing
            , aoRequested = Nothing
            , aoApplied   = Nothing
            , aoDropped   = Nothing
            , aoReason    = reason
            , aoHandler   = Nothing
            }

recordRefusal ∷ EngineEnv → Text → Int → Int → SeamRefusal → IO ()
recordRefusal env kind gx gy r =
    recordOutcome env kind "rejected" gx gy (Just (renderSeamRefusal r))

-- | The page, or a "world page not found" rejection (the page can be
--   destroyed between enqueue and drain).
withPage ∷ EngineEnv → Text → WorldPageId → Int → Int
         → (WorldState → IO ()) → IO ()
withPage env kind pageId gx gy act = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordOutcome env kind "rejected" gx gy
            (Just ("world page not found: " <> unWorldPageId pageId))
        Just ws → act ws

-- | The resident occurrence @iid@ standing on the (alias) tile, with the
--   canonical tile it resolved to.
residentOccurrence ∷ WorldState → Int → Int → FloraInstanceId
                   → IO ((Int, Int), Maybe FloraInstance)
residentOccurrence ws rawGX rawGY iid = do
    worldSize ← pageWrapWorldSize ws
    let (coord, (lx, ly), (dgx, dgy)) = canonicalTileFrame worldSize rawGX rawGY
        tile = (rawGX + dgx, rawGY + dgy)
    td ← readIORef (wsTilesRef ws)
    pure ( tile
         , do lc ← lookupChunk coord td
              listToMaybe
                  [ i | i ← fcdInstances (lcFlora lc)
                      , fiInstanceId i ≡ iid
                      , fromIntegral (fiTileX i) ≡ lx
                      , fromIntegral (fiTileY i) ≡ ly ] )

handleWorldSetFloraConditionCommand
    ∷ EngineEnv → LoggerState → WorldPageId → Int → Int
    → Maybe FloraInstanceId → Text → IO ()
handleWorldSetFloraConditionCommand env logger pageId gx gy mIid causeTok =
    withPage env kindSet pageId gx gy $ \ws → case parseDeathCause causeTok of
        Nothing → recordRefusal env kindSet gx gy
                      (RefusedUnknownCause causeTok)
        Just cause → do
            cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
            (daysPerYear, _, absDay) ← growthCalendar ws
            result ← case mIid of
                Just iid → do
                    (tile, mInst) ← residentOccurrence ws gx gy iid
                    case mInst of
                        Nothing → pure (Left RefusedUnknownOccurrence)
                        Just inst →
                            atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
                                case setOccurrenceCondition cat daysPerYear
                                         tile absDay inst cause store of
                                    Left e → (store, Left e)
                                    Right store' → (store', Right ())
                Nothing → do
                    worldSize ← pageWrapWorldSize ws
                    let (_, _, (dgx, dgy)) = canonicalTileFrame worldSize gx gy
                        tile = (gx + dgx, gy + dgy)
                    -- The plot is read out of the SAME map the edit is
                    -- applied to, inside the atomic step.
                    atomicModifyIORef' (wsCropPlotsRef ws) $ \store →
                        case setPlotCondition cat daysPerYear tile absDay
                                 cause (cpsPlots store) of
                            Left e → (store, Left e)
                            Right ps → (store { cpsPlots = ps }, Right ())
            case result of
                Left e → recordRefusal env kindSet gx gy e
                Right () → do
                    bumpQuadCacheGen ws
                    logDebug logger CatWorld $
                        "Flora condition set at " <> tshow gx <> ","
                          <> tshow gy <> " cause=" <> causeTok
                    recordOutcome env kindSet "accepted" gx gy Nothing

handleWorldClearFloraConditionCommand
    ∷ EngineEnv → LoggerState → WorldPageId → Int → Int
    → Maybe FloraInstanceId → IO ()
handleWorldClearFloraConditionCommand env logger pageId gx gy mIid =
    withPage env kindClear pageId gx gy $ \ws → do
        result ← case mIid of
            Just iid → do
                (tile, mInst) ← residentOccurrence ws gx gy iid
                -- An occurrence is known if its chunk is resident OR the
                -- store holds a record whose provenance tile is this one
                -- (an evicted corpse must stay clearable).
                atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
                    let recorded = lookupFloraCondition iid store
                        known = isJust mInst
                              ∨ maybe False ((≡ tile) . fcrTile) recorded
                    in if not known
                       then (store, Left RefusedUnknownOccurrence)
                       else ( clearOccurrenceCondition iid store
                            , Right (isJust recorded) )
            Nothing → do
                worldSize ← pageWrapWorldSize ws
                let (_, _, (dgx, dgy)) = canonicalTileFrame worldSize gx gy
                    tile = (gx + dgx, gy + dgy)
                atomicModifyIORef' (wsCropPlotsRef ws) $ \store →
                    let plots = cpsPlots store
                    in case clearPlotCondition tile plots of
                        Left e → (store, Left e)
                        Right ps →
                            ( store { cpsPlots = ps }
                            , Right (maybe False (isJust . cpCondition)
                                         (HM.lookup tile plots)) )
        case result of
            Left e → recordRefusal env kindClear gx gy e
            Right changed → do
                when changed (bumpQuadCacheGen ws)
                logDebug logger CatWorld $
                    "Flora condition clear at " <> tshow gx <> ","
                      <> tshow gy <> " changed=" <> tshow changed
                if changed
                    then recordOutcome env kindClear "accepted" gx gy Nothing
                    else recordOutcome env kindClear "noop" gx gy
                             (Just "the occurrence had no explicit condition")
