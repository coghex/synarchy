{-# LANGUAGE Strict #-}
-- | The unit thread's defensive check on the numeric payload of a
--   motion command (#2290).
--
--   Shared by the two handler modules that install one —
--   "Unit.Thread.Command.Lifecycle" for @UnitTeleport@ and
--   "Unit.Thread.Command.Motion" for @UnitMoveTo@ and
--   @UnitSetMoveSpeed@ — so all three drop the same values, and drop
--   exactly the values the scripting boundary already refuses: the
--   domain itself is 'motionCoordinateInDomain' /
--   'motionSpeedInDomain', not a second opinion written here.
module Unit.Thread.Command.MotionGuard
    ( motionPayloadOk
    ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Log (LogCategory(..), logWarn)
import Engine.Core.State (EngineEnv, loggerRef)
import Unit.Command.Types (motionCoordinateInDomain, motionSpeedInDomain)

-- | Whether a motion command's payload may be installed, warning once
--   naming the first offending field when it may not.
--
--   The scripting verbs in "Engine.Scripting.Lua.API.Units.MotionArgs"
--   are the ONLY producers of these commands today, and they refuse an
--   out-of-domain value before it is ever queued — so nothing here is
--   reachable from a call the shipped engine can make. It is kept
--   anyway because the command queue is a seam a future producer can
--   join without reading the verb it bypassed, and because the damage
--   it guards against is silent rather than loud: an installed NaN
--   target is never arrived at, cannot be recovered from by issuing
--   another order (the unit's own position is NaN by then), and is
--   persisted verbatim on the next save. Failing closed at the handler
--   is the posture the rest of the movement policy already takes — see
--   'Unit.Thread.Movement.PathAdvance''s protected-step clamp.
--
--   Coordinates and speeds are passed separately because they have
--   DIFFERENT domains: a speed of zero is legitimate (an exhausted or
--   fully encumbered unit commands one) but a negative speed steps the
--   unit away from its goal, so speeds carry the extra bound that
--   coordinates do not.
motionPayloadOk ∷ EngineEnv
                → Text              -- ^ the command's name, for the warning
                → [(Text, Float)]   -- ^ named coordinate fields
                → [(Text, Float)]   -- ^ named speed fields
                → IO Bool
motionPayloadOk env what coords speeds =
    case [ f | f@(_, v) ← coords, not (motionCoordinateInDomain v) ]
         ⧺ [ f | f@(_, v) ← speeds, not (motionSpeedInDomain v) ] of
        [] → pure True
        ((field, value) : _) → do
            logger ← readIORef (loggerRef env)
            logWarn logger CatThread $
                what <> ": " <> field <> " is out of domain ("
                <> tshow value <> ") — command dropped"
            pure False
