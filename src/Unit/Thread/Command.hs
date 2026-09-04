{-# LANGUAGE Strict #-}
module Unit.Thread.Command
    ( processAllUnitCommands
    , recomputeBodyDerivedStats
    , injurySpeedMult
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Data.IORef (IORef)
import Engine.Core.State (EngineEnv)
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
    , handleUnitSetMoveSpeedCommand
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

-- | Drain the unit command queue, stopping at the Exit-to-Menu session
--   boundary (#2291). Answers @True@ when this pass consumed a
--   'UnitEndSession' marker, which is 'Unit.Thread.unitTickWith''s
--   signal to finish the teardown and reset the session epoch once BOTH
--   queues have been drained.
--
--   The marker ends the pass rather than being handled and skipped past.
--   Whatever sits behind it in this queue was enqueued after the session
--   was torn down, so running it here — before the tick's own end-of-
--   session step — would stamp it with the OUTGOING session's clock and
--   then reset that clock out from under it. Leaving it queued costs one
--   tick and makes "everything after the boundary sees the new epoch" a
--   property of the queue rather than of how fast a player can click.
processAllUnitCommands ∷ EngineEnv → IORef UnitThreadState → IO Bool
processAllUnitCommands env utsRef = do
    mCmd ← Q.tryReadQueue (ucUnitQueue (toUnitCombatCapability env))
    case mCmd of
        Just UnitEndSession → return True
        Just cmd → do
            handleUnitCommand env utsRef cmd
            processAllUnitCommands env utsRef
        Nothing → return False

handleUnitCommand ∷ EngineEnv → IORef UnitThreadState → UnitCommand → IO ()
handleUnitCommand env utsRef (UnitSpawn uid defName gx gy gz factionId pageId)
  = handleUnitSpawnCommand env utsRef uid defName gx gy gz factionId pageId
handleUnitCommand env utsRef (UnitDestroy uid)
  = handleUnitDestroyCommand env utsRef uid
handleUnitCommand env utsRef UnitClearAll
  = handleUnitClearAllCommand env utsRef
handleUnitCommand env utsRef (UnitTeleport uid gx gy mGz)
  = handleUnitTeleportCommand env utsRef uid gx gy mGz
handleUnitCommand env utsRef (UnitReGround pageId gx gy)
  = handleUnitReGroundCommand env utsRef pageId gx gy
handleUnitCommand env utsRef (UnitMoveTo uid tx ty speed hazard)
  = handleUnitMoveToCommand env utsRef uid tx ty speed hazard
handleUnitCommand env utsRef (UnitSetMoveSpeed uid speed)
  = handleUnitSetMoveSpeedCommand env utsRef uid speed
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
-- The session boundary (#2291) is a queue POSITION, not work:
-- 'processAllUnitCommands' takes it off the queue and stops, so it never
-- reaches a handler. Matched here anyway, and only so this dispatch stays
-- total — a silent fall-through would be the same defect as a missing
-- command.
handleUnitCommand _env _utsRef UnitEndSession = return ()
