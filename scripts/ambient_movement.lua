-- Shared ambient-movement mechanism (#1217).
--
-- Aimless, zero-stakes movement should never choose preventable injury.
-- The engine decides that per MOVEMENT REQUEST: unit.moveTo takes a
-- hazard-policy token, and "avoid_falls" makes a step the pathing cost
-- model classifies as a real fall (drop >= fall_trigger_drop) impassable
-- for that request — in the greedy stepper and in every local-A* replan
-- alike. A request that can make no safe progress terminates, so the AI
-- resamples a different destination on a later tick instead of idling on
-- an unreachable one.
--
-- This module exists so the aimless movers share ONE mechanism rather
-- than each spelling the token themselves: acolyte/technomule `wander`
-- (unit_ai_needs.lua), `bear_wander` (bear_ai.lua) and `squirrel_wander`
-- (red_squirrel_ai.lua) today, and any future non-emergency autonomous
-- movement that wants the same guarantee.
--
-- What must NOT adopt it: purposeful and emergency movement. Player
-- commands, survival actions (water/food seeking, canteen refill),
-- flee/panic, delirium, mental breaks, forced retreat, and medic / work /
-- fetch pathing all keep the fall-permitted default. Note especially that
-- panic and delirium reach meander-speed wandering through
-- unit_ai_mental.lua's own calls to needs.wanderExecute — which is why
-- protection is selected by CALLER, never inferred from speed, species,
-- or "this looks like a meander".

local mv = require("scripts.movement_speed")

local M = {}

-- The engine's hazard-policy tokens (Unit.Pathing.Hazard). Spelled once.
M.AVOID_FALLS = "avoid_falls"
M.ALLOW_FALLS = "allow_falls"

-- Move at an explicit speed on a route that may not cross a damaging
-- drop. Returns unit.moveTo's own boolean.
function M.moveTo(uid, tx, ty, speed)
    return unit.moveTo(uid, tx, ty, speed, M.AVOID_FALLS)
end

-- The ambient wander leg: a slow meander (well below comfort, so the unit
-- ambles and recovers stamina) on a fall-free route. This is the call the
-- three shipped aimless movers make.
function M.wanderTo(uid, tx, ty)
    return M.moveTo(uid, tx, ty, mv.meander(uid))
end

return M
