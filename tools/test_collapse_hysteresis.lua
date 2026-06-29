-- Unit test for the collapse↔crawl locomotor hysteresis (#304).
--
-- Pure Lua, no engine: drives injuries.collapseWithHysteresis directly and
-- asserts the truth table. The bug was that ENTER-collapse and LEAVE-collapse
-- pivoted on the same threshold, so a downed unit hovering at the boundary
-- flapped collapsed↔crawling tick-to-tick. The fix adds a rise band: once
-- collapsed, the unit stays down until BOTH boundary inputs clear it.
--
-- Run:  luajit tools/test_collapse_hysteresis.lua   (from repo root)

package.path = "scripts/?.lua;" .. package.path
local injuries = require("injuries")

local fails = 0
local function check(desc, got, want)
    if got ~= want then
        fails = fails + 1
        print(string.format("  [FAIL] %s: got %s, want %s",
            desc, tostring(got), tostring(want)))
    else
        print(string.format("  [pass] %s", desc))
    end
end

local C = injuries.collapseWithHysteresis
assert(type(C) == "function", "injuries.collapseWithHysteresis missing")

-- ENTERING collapse (unit is on its feet / crawling): the bare knockout
-- trigger alone decides — no hysteresis on the way DOWN.
check("standing + knockedOut         -> collapse",    C("standing", true,  false), true)
check("standing + lucid              -> stay up",     C("standing", false, false), false)
check("crawling + knockedOut         -> collapse",    C("crawling", true,  true),  true)
check("crawling + lucid              -> keep crawling",C("crawling", false, true),  false)

-- THE FIX — an ALREADY-collapsed unit sitting in the hysteresis BAND
-- (knockout trigger has cleared, but the rise band has NOT) must STAY
-- collapsed. Under the old code this returned "not collapsed" the instant the
-- knockout trigger cleared, flipping the unit to crawling — the flicker bug.
check("collapsed + in band (cleared knockout, cannot rise) -> STAY collapsed",
      C("collapsed", false, false), true)
check("collapsed + still knocked out -> stay collapsed",
      C("collapsed", true,  false), true)

-- Cleared the rise band: an already-collapsed unit may finally come up
-- (to crawl if its legs are broken, else stand via checkRevive).
check("collapsed + canRise           -> leave collapse", C("collapsed", false, true), false)
check("collapsed + knockedOut but canRise (degenerate) -> leave collapse",
      C("collapsed", true,  true), false)

-- Hysteresis property: across the band [enter cleared .. rise not cleared],
-- a collapsed unit never spontaneously leaves collapse, so a value oscillating
-- inside the band cannot flap the pose.
local flaps = 0
local pose = "collapsed"
-- knockedOut jitters true/false but canRise stays false (still in band).
for i = 1, 50 do
    local knockedOut = (i % 2 == 0)        -- jitter across the enter threshold
    local collapsed = C(pose, knockedOut, false)
    local newPose = collapsed and "collapsed" or "crawling"
    if newPose ~= pose then flaps = flaps + 1 end
    pose = newPose
end
check("no flapping while jittering inside the band (50 ticks)", flaps, 0)

if fails == 0 then
    print("ALL PASS")
    os.exit(0)
else
    print(fails .. " FAILED")
    os.exit(1)
end
