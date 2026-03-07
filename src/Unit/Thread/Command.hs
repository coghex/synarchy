{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Command
    ( processAllUnitCommands
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Sim.Types
import Unit.Direction (Direction(..))
import Unit.Command.Types (UnitCommand(..))
import World.Types (WorldManager(..), WorldState(..), WorldTileData(..),
                    LoadedChunk(..), ChunkCoord(..), columnIndex, lookupChunk)
import World.Generate (globalToChunk)

-----------------------------------------------------------
-- Command Processing
-----------------------------------------------------------

processAllUnitCommands ∷ EngineEnv → IORef UnitThreadState → IO ()
processAllUnitCommands env utsRef = do
    mCmd ← Q.tryReadQueue (unitQueue env)
    case mCmd of
        Just cmd → do
            handleUnitCommand env utsRef cmd
            processAllUnitCommands env utsRef
        Nothing → return ()

handleUnitCommand ∷ EngineEnv → IORef UnitThreadState → UnitCommand → IO ()
handleUnitCommand env utsRef (UnitSpawn uid defName gx gy gz) = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup defName (umDefs um) of
        Nothing → do
            logger ← readIORef (loggerRef env)
            logWarn logger CatThread $
                "UnitSpawn: unknown def '" <> defName <> "'"
        Just def → do
            let inst = UnitInstance
                    { uiDefName    = defName
                    , uiTexture    = udTexture def
                    , uiDirSprites = udDirSprites def
                    , uiBaseWidth  = udBaseWidth def
                    , uiGridX      = gx
                    , uiGridY      = gy
                    , uiGridZ      = gz
                    , uiFacing     = DirS -- Default facing south
                    }
            atomicModifyIORef' (unitManagerRef env) $ \um' →
                (um' { umInstances = HM.insert uid inst (umInstances um') }, ())

            -- Create sim state (unchanged)
            let ss = UnitSimState
                    { usRealX  = gx
                    , usRealY  = gy
                    , usGridZ  = gz
                    , usTarget = Nothing
                    , usState  = Idle
                    , usFacing = DirS
                    }
            atomicModifyIORef' utsRef $ \uts →
                (uts { utsSimStates = HM.insert uid ss (utsSimStates uts) }, ())

handleUnitCommand env utsRef (UnitDestroy uid) = do
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.delete uid (umInstances um) }, ())
    atomicModifyIORef' utsRef $ \uts →
        (uts { utsSimStates = HM.delete uid (utsSimStates uts) }, ())

handleUnitCommand env utsRef (UnitTeleport uid gx gy mGz) = do
    gz ← case mGz of
        Just z  → return z
        Nothing → do
            let gxi = floor gx ∷ Int
                gyi = floor gy ∷ Int
            mSurf ← lookupSurfaceZ env gxi gyi
            case mSurf of
                Just z  → return z
                Nothing → return 0

    -- Update sim state
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usRealX  = gx
                             , usRealY  = gy
                             , usGridZ  = gz
                             , usTarget = Nothing
                             , usState  = Idle
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

    -- Immediate update to render state
    atomicModifyIORef' (unitManagerRef env) $ \um →
        let insts = umInstances um
        in case HM.lookup uid insts of
            Nothing → (um, ())
            Just inst →
                let inst' = inst { uiGridX = gx
                                 , uiGridY = gy
                                 , uiGridZ = gz
                                 }
                in (um { umInstances = HM.insert uid inst' insts }, ())

handleUnitCommand env utsRef (UnitMoveTo uid tx ty speed) = do
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usTarget = Just (MoveTarget tx ty speed)
                             , usState  = Walking
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitStop uid) = do
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usTarget = Nothing
                             , usState  = Idle
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

-----------------------------------------------------------
-- Surface Z lookup (same as Units.hs version)
-----------------------------------------------------------

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
