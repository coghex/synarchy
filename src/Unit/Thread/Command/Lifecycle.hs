{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Command.Lifecycle
    ( handleUnitDestroyCommand
    , handleUnitClearAllCommand
    , handleUnitTeleportCommand
    , handleUnitReGroundCommand
    , lookupSurfaceZ
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Unit.Types
import Unit.Sim.Types
import World.Types (WorldManager(..), WorldState(..), LoadedChunk(..), columnIndex, lookupChunk)
import World.Generate (globalToChunk)

handleUnitDestroyCommand ∷ EngineEnv → IORef UnitThreadState → UnitId → IO ()
handleUnitDestroyCommand env utsRef uid = do
    -- Single atomic modify removes the unit from instances AND clears
    -- it from the selection set, so no observer ever sees a "selected
    -- but dead" state.
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.delete uid (umInstances um)
            , umSelected  = HS.delete uid (umSelected um)
            }, ())
    atomicModifyIORef' utsRef $ \uts →
        (uts { utsSimStates = HM.delete uid (utsSimStates uts) }, ())

handleUnitClearAllCommand ∷ EngineEnv → IORef UnitThreadState → IO ()
handleUnitClearAllCommand env utsRef = do
    -- Wipe all units + selection + sim state. Processed in queue order, so
    -- it runs AFTER any UnitSpawns queued before Exit to Menu — those
    -- insert first, then this clears, leaving no orphans (#58).
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.empty, umSelected = HS.empty }, ())
    atomicModifyIORef' utsRef $ \uts →
        (uts { utsSimStates = HM.empty }, ())

handleUnitTeleportCommand ∷ EngineEnv → IORef UnitThreadState → UnitId
                          → Float → Float → Maybe Int → IO ()
handleUnitTeleportCommand env utsRef uid gx gy mGz = do
    gz ← case mGz of
        Just z  → return z
        Nothing → do
            let gxi = floor gx ∷ Int
                gyi = floor gy ∷ Int
            mSurf ← lookupSurfaceZ env gxi gyi
            case mSurf of
                Just z  → return z
                Nothing → return 0

    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usRealX     = gx
                             , usRealY     = gy
                             , usGridZ     = gz
                             , usRealZ     = fromIntegral gz
                             , usTarget    = Nothing
                             , usState     = Idle
                             , usLocalPath = []
                             , usDrinkUntil      = Nothing
                             , usEatUntil        = Nothing
                             , usPickupUntil     = Nothing
                             , usTransitionUntil = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

    atomicModifyIORef' (unitManagerRef env) $ \um →
        let insts = umInstances um
        in case HM.lookup uid insts of
            Nothing → (um, ())
            Just inst →
                let inst' = inst { uiGridX = gx
                                 , uiGridY = gy
                                 , uiGridZ = gz
                                 , uiRealZ = fromIntegral gz
                                 }
                in (um { umInstances = HM.insert uid inst' insts }, ())

handleUnitReGroundCommand ∷ EngineEnv → IORef UnitThreadState → Int → Int → IO ()
handleUnitReGroundCommand env utsRef gx gy = do
    -- Terrain at (gx, gy) changed under our feet (dig / delete-tile).
    -- Re-snap idle units standing on that tile to the new surface;
    -- moving units re-ground themselves on every tile crossing, and
    -- transitioning/falling/climbing units are mid-state-machine and
    -- must not be teleport-snapped.
    mSurf ← lookupSurfaceZ env gx gy
    case mSurf of
        Nothing → pure ()
        Just z → do
            snapped ← atomicModifyIORef' utsRef $ \uts →
                let simStates = utsSimStates uts
                    affects ss =
                        floor (usRealX ss) ≡ gx
                      ∧ floor (usRealY ss) ≡ gy
                      ∧ usState ss ≡ Idle
                      ∧ usGridZ ss ≢ z
                    snap ss
                        | affects ss = ss { usGridZ = z
                                          , usRealZ = fromIntegral z }
                        | otherwise  = ss
                    hit = [ uid | (uid, ss) ← HM.toList simStates
                                , affects ss ]
                in (uts { utsSimStates = HM.map snap simStates }, hit)
            -- Mirror into the render-facing instances so the visual z
            -- updates this frame, same as UnitTeleport does.
            forM_ snapped $ \uid →
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, ())
                        Just inst →
                            let inst' = inst { uiGridZ = z
                                             , uiRealZ = fromIntegral z }
                            in (um { umInstances =
                                    HM.insert uid inst' (umInstances um) }, ())

-- | KNOWN FOLLOW-UP (#797 audit clause): scans wmVisible in order and
--   returns the first page with a loaded chunk at (gx, gy), rather than
--   resolving a specific page's own tiles — the same head/order
--   assumption #797 fixed in Unit.LineOfSight. Left unfixed here
--   deliberately: one caller (handleUnitTeleportCommand) has a uid and
--   could pass its page, but the other (handleUnitReGroundCommand)
--   takes only a bare (gx, gy) with no page context at all — the tile
--   edit that triggers it isn't page-tagged either — so giving this
--   helper page ownership needs signature changes up through BOTH call
--   chains, materially larger in scope than #797's LOS change. Tracked
--   as a deferred follow-up.
lookupSurfaceZ ∷ EngineEnv → Int → Int → IO (Maybe Int)
lookupSurfaceZ env gx gy = do
    wm ← readIORef (worldManagerRef env)
    go (wmVisible wm) (wmWorlds wm)
  where
    (chunkCoord, (lx, ly)) = globalToChunk gx gy
    go [] _ = return Nothing
    go (pageId:rest) worlds =
        case lookup pageId worlds of
            Nothing → go rest worlds
            Just ws → do
                td ← readIORef (wsTilesRef ws)
                case lookupChunk chunkCoord td of
                    Just lc → return $ Just ((lcSurfaceMap lc) VU.! columnIndex lx ly)
                    Nothing → go rest worlds
