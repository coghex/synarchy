{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Command
    ( processAllUnitCommands
    , recomputeBodyDerivedStats
    , injurySpeedMult
    ) where

import UPrelude
import Data.IORef (IORef)
import Engine.Core.State (EngineEnv(..))
import Unit.Sim.Types (UnitThreadState)
import Unit.Command.Types (UnitCommand(..))
import qualified Engine.Core.Queue as Q
import Unit.Thread.Command.Body
    ( recomputeBodyDerivedStats
    , injurySpeedMult
    )
import Unit.Thread.Command.Spawn (handleUnitSpawnCommand)
import Unit.Thread.Command.Lifecycle
    ( handleUnitDestroyCommand
    , handleUnitClearAllCommand
    , handleUnitTeleportCommand
    , handleUnitReGroundCommand
    )
import Unit.Thread.Command.Motion
    ( handleUnitMoveToCommand
    , handleUnitJumpCommand
    , handleUnitStopCommand
    )
import Unit.Thread.Command.Pose
    ( handleUnitCollapseCommand
    , handleUnitCrawlCommand
    , handleUnitKillCommand
    , handleUnitReviveCommand
    , handleUnitDrinkCommand
    , handleUnitEatCommand
    , handleUnitPickupCommand
    , handleUnitTransitionToCommand
    )

processAllUnitCommands ∷ EngineEnv → IORef UnitThreadState → IO ()
processAllUnitCommands env utsRef = do
    mCmd ← Q.tryReadQueue (unitQueue env)
    case mCmd of
        Just cmd → do
            handleUnitCommand env utsRef cmd
            processAllUnitCommands env utsRef
        Nothing → return ()

handleUnitCommand ∷ EngineEnv → IORef UnitThreadState → UnitCommand → IO ()
handleUnitCommand env utsRef (UnitSpawn uid defName gx gy gz factionId pageId)
  = handleUnitSpawnCommand env utsRef uid defName gx gy gz factionId pageId
handleUnitCommand env utsRef (UnitDestroy uid)
  = handleUnitDestroyCommand env utsRef uid
handleUnitCommand env utsRef UnitClearAll
  = handleUnitClearAllCommand env utsRef
handleUnitCommand env utsRef (UnitTeleport uid gx gy mGz)
  = handleUnitTeleportCommand env utsRef uid gx gy mGz
handleUnitCommand env utsRef (UnitReGround gx gy)
  = handleUnitReGroundCommand env utsRef gx gy
handleUnitCommand env utsRef (UnitMoveTo uid tx ty speed)
  = handleUnitMoveToCommand env utsRef uid tx ty speed
handleUnitCommand env utsRef (UnitJump uid tgx tgy)
  = handleUnitJumpCommand env utsRef uid tgx tgy
handleUnitCommand _env utsRef (UnitStop uid)
  = handleUnitStopCommand utsRef uid
handleUnitCommand _env utsRef (UnitCollapse uid)
  = handleUnitCollapseCommand utsRef uid
handleUnitCommand _env utsRef (UnitCrawl uid)
  = handleUnitCrawlCommand utsRef uid
handleUnitCommand env utsRef (UnitKill uid)
  = handleUnitKillCommand env utsRef uid
handleUnitCommand _env utsRef (UnitRevive uid)
  = handleUnitReviveCommand utsRef uid
handleUnitCommand env utsRef (UnitDrink uid)
  = handleUnitDrinkCommand env utsRef uid
handleUnitCommand env utsRef (UnitEat uid)
  = handleUnitEatCommand env utsRef uid
handleUnitCommand env utsRef (UnitPickup uid)
  = handleUnitPickupCommand env utsRef uid
handleUnitCommand env utsRef (UnitTransitionTo uid target stride)
  = handleUnitTransitionToCommand env utsRef uid target stride
