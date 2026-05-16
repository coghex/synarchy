{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Command
    ( processAllUnitCommands
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Sim.Types
import Unit.Stats (rollStat)
import Unit.Direction (Direction(..))
import Unit.Command.Types (UnitCommand(..))
import World.Types (WorldManager(..), WorldState(..), WorldTileData(..),
                    LoadedChunk(..), ChunkCoord(..), columnIndex, lookupChunk)
import World.Generate (globalToChunk)

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
            initialStats ←
                if udEagerStats def
                then atomicModifyIORef' (statRNGRef env) $ \g0 →
                    let (rolled, g') = HM.foldlWithKey'
                            (\(acc, g) name (b, r) →
                                let (v, g'') = rollStat b r g
                                in (HM.insert name v acc, g''))
                            (HM.empty, g0)
                            (udStatTemplates def)
                        -- Scale rolled strength by lean-mass ratio so
                        -- bigger / leaner characters get a strength bump
                        -- and skinny ones a penalty. Sub-linear (exp 0.7)
                        -- because strength scales with muscle cross-
                        -- section, not volume. The rolled value's
                        -- variance (sigma = range/4) can still flip
                        -- ordering — a small unit with a high roll can
                        -- beat a large unit with a low roll.
                        scaled = scaleStrengthByBody def rolled
                    in (g', scaled)
                else return HM.empty
            -- Skills always roll at spawn so they have a starting
            -- level for the addSkillXP formula to operate on.
            initialSkills ← atomicModifyIORef' (statRNGRef env) $ \g0 →
                let (rolled, g') = HM.foldlWithKey'
                        (\(acc, g) name (b, r) →
                            let (v, g'') = rollStat b r g
                            in (HM.insert name v acc, g''))
                        (HM.empty, g0)
                        (udSkillTemplates def)
                in (g', rolled)
            let inst = UnitInstance
                    { uiDefName    = defName
                    , uiTexture    = udTexture def
                    , uiDirSprites = udDirSprites def
                    , uiBaseWidth  = udBaseWidth def
                    , uiGridX      = gx
                    , uiGridY      = gy
                    , uiGridZ      = gz
                    , uiFacing     = DirS -- Default facing south
                    , uiCurrentAnim = ""  -- Phase 1: anim triggers wired in Phase 3
                    , uiAnimStart   = 0
                    , uiAnimReverse = False
                    , uiActivity    = "idle"
                    , uiStats       = initialStats
                    , uiModifiers   = HM.empty
                    , uiSkills      = initialSkills
                    }
            atomicModifyIORef' (unitManagerRef env) $ \um' →
                (um' { umInstances = HM.insert uid inst (umInstances um') }, ())

            let ss = UnitSimState
                    { usRealX     = gx
                    , usRealY     = gy
                    , usGridZ     = gz
                    , usTarget    = Nothing
                    , usState     = Idle
                    , usFacing    = DirS
                    , usLocalPath = []
                    , usReviveUntil = Nothing
                    }
            atomicModifyIORef' utsRef $ \uts →
                (uts { utsSimStates = HM.insert uid ss (utsSimStates uts) }, ())

handleUnitCommand env utsRef (UnitDestroy uid) = do
    -- Single atomic modify removes the unit from instances AND clears
    -- it from the selection set, so no observer ever sees a "selected
    -- but dead" state.
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.delete uid (umInstances um)
            , umSelected  = HS.delete uid (umSelected um)
            }, ())
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

    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usRealX     = gx
                             , usRealY     = gy
                             , usGridZ     = gz
                             , usTarget    = Nothing
                             , usState     = Idle
                             , usLocalPath = []
                             , usReviveUntil = Nothing
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
                                 }
                in (um { umInstances = HM.insert uid inst' insts }, ())

handleUnitCommand env utsRef (UnitMoveTo uid tx ty speed) = do
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                -- Collapsed and Reviving units ignore move orders so
                -- right-click on a selected one doesn't snap them
                -- upright mid-animation.
                | usState ss ≡ Collapsed → (uts, ())
                | usState ss ≡ Reviving  → (uts, ())
                | otherwise →
                    let ss' = ss { usTarget    = Just (MoveTarget tx ty speed)
                                 , usState     = Walking
                                 , usLocalPath = []
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitStop uid) = do
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usTarget    = Nothing
                             , usState     = Idle
                             , usLocalPath = []
                             , usReviveUntil = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitCollapse uid) = do
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usTarget    = Nothing
                             , usState     = Collapsed
                             , usLocalPath = []
                             , usReviveUntil = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitRevive uid) = do
    -- Only acts on Collapsed units. Compute the reviving-state anim
    -- duration up front so we know when to auto-transition back to
    -- Idle. If the unit's def doesn't define a reviving anim, the
    -- duration is 0 and the unit skips straight to Idle.
    um ← readIORef (unitManagerRef env)
    let duration = case HM.lookup uid (umInstances um) of
            Nothing   → 0
            Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
                Nothing  → 0
                Just def →
                    let key      = "reviving" ∷ Text
                        animName = HM.lookupDefault key key (udStateAnims def)
                    in case HM.lookup animName (udAnimations def) of
                        Nothing → 0
                        Just a  →
                            let counts = V.length <$> Map.elems (aFrames a)
                                maxN   = if null counts then 0 else maximum counts
                                fps    = aFps a
                            in if fps > 0 ∧ maxN > 0
                               then fromIntegral maxN / realToFrac fps ∷ Double
                               else 0
    -- Game-clock: usReviveUntil is checked against gameTimeRef in
    -- tickAllMovement, so the deadline has to be set in the same
    -- clock or pause would either skip or stall it.
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usState ss ≢ Collapsed → (uts, ())
                | duration ≤ 0 →
                    -- No reviving anim defined; just snap to Idle.
                    let ss' = ss { usState = Idle, usReviveUntil = Nothing }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())
                | otherwise →
                    let ss' = ss { usState = Reviving
                                 , usReviveUntil = Just (now + duration)
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

-- | Multiply the rolled strength by `(leanMass / avgLeanMass) ^ 0.7`,
--   where avgLeanMass comes from the def's body means and leanMass from
--   this unit's rolled body. No-op if any body attr is missing — keeps
--   units without a body block unchanged.
scaleStrengthByBody ∷ UnitDef → HM.HashMap Text Float → HM.HashMap Text Float
scaleStrengthByBody def rolled =
    case (HM.lookup "strength" rolled
         , HM.lookup "height"  rolled
         , HM.lookup "bulk"    rolled
         , HM.lookup "bodyfat" rolled
         , statMean def "height"
         , statMean def "bulk"
         , statMean def "bodyfat") of
        ( Just str, Just h, Just b, Just f
         , Just hM, Just bM, Just fM ) →
            let rolledLean = leanMassOf h b f
                avgLean    = leanMassOf hM bM fM
                ratio      = if avgLean > 0 then rolledLean / avgLean else 1
                scaled     = str * (ratio ** 0.7)
            in HM.insert "strength" scaled rolled
        _ → rolled
  where
    leanMassOf h b f = 22 * h * h * b * (1 - f)
    statMean d name  = fst <$> HM.lookup name (udStatTemplates d)

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
