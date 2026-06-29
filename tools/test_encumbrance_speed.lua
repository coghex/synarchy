-- Offline regression harness for the encumbrance speed multiplier (#305).
--
-- movement_speed.lua now scales the whole speed band by a carried-weight /
-- capacity ratio, eased by the endurance stat. This test loads the module
-- in isolation (stubbing injuries/salts so only the encumbrance term is
-- exercised) and asserts the three properties the issue calls for:
--
--   1. Same unit, empty vs near-capacity vs over-capacity → strictly
--      DECREASING speed.
--   2. Same load ratio, high vs low endurance → the high-endurance unit is
--      faster (endurance eases the curve).
--   3. Unloaded behaviour unchanged — multiplier ≈ 1.0 at light load.
--
-- Run: luajit tools/test_encumbrance_speed.lua

package.path = "./?.lua;" .. package.path

-----------------------------------------------------------
-- Stub the two sibling speed multipliers so this test isolates the
-- encumbrance term (and doesn't drag in their real dependency trees).
-----------------------------------------------------------
package.loaded["scripts.injuries"] = { speedMultiplier = function() return 1.0 end }
package.loaded["scripts.salts"]    = { speedMultiplier = function() return 1.0 end }

-----------------------------------------------------------
-- Unit-state the stubs serve: per-uid carrying_capacity, carried weight,
-- endurance, plus a fixed max_speed / agility so sprint() is well-defined.
-----------------------------------------------------------
local U = {}  -- uid -> { cap, carried, endur }

unit = {
    getMaxSpeed = function() return 10.0 end,
    getCarryingWeight = function(uid) return U[uid].carried end,
    getStat = function(uid, name)
        if name == "agility" then return U[uid].agi or 1.0 end
        if name == "endurance" then return U[uid].endur end
        if name == "carrying_capacity" then return U[uid].cap end
        return nil
    end,
}

local ms = require("scripts.movement_speed")

-----------------------------------------------------------
-- Assertion helpers
-----------------------------------------------------------
local failures = 0
local function check(cond, msg)
    if cond then
        print("  PASS  " .. msg)
    else
        print("  FAIL  " .. msg)
        failures = failures + 1
    end
end

local function approx(a, b, eps)
    return math.abs(a - b) <= (eps or 1e-6)
end

-- Configure a uid and return its encumbrance multiplier + sprint speed.
local function setUnit(uid, cap, carried, endur, agi)
    U[uid] = { cap = cap, carried = carried, endur = endur, agi = agi }
    return ms.encumbranceMultiplier(uid), ms.sprint(uid)
end

print("== #305 encumbrance speed multiplier ==")

-----------------------------------------------------------
-- 1. Strictly decreasing speed: empty < quarter < half < at-cap < over-cap.
--    capacity 100 kg, endurance 1.0 (nominal acolyte).
-----------------------------------------------------------
print("[1] strictly-decreasing travel speed as load rises (endurance 1.0)")
local mEmpty , sEmpty  = setUnit(1, 100,   0, 1.0)
local mQtr   , _       = setUnit(1, 100,  25, 1.0)   -- at the free allowance
local mHalf  , sHalf   = setUnit(1, 100,  50, 1.0)
local mFull  , sFull   = setUnit(1, 100, 100, 1.0)   -- at capacity
local mOver  , sOver   = setUnit(1, 100, 150, 1.0)   -- 1.5x over capacity

check(approx(mEmpty, 1.0), string.format("empty  -> %.3f (≈1.0, light load free)", mEmpty))
check(approx(mQtr,   1.0), string.format("25%%   -> %.3f (≈1.0, still within free allowance)", mQtr))
check(mHalf  < mEmpty,     string.format("half   -> %.3f < empty %.3f", mHalf, mEmpty))
check(mFull  < mHalf,      string.format("at-cap -> %.3f < half  %.3f", mFull, mHalf))
check(mOver  < mFull,      string.format("over   -> %.3f < at-cap %.3f", mOver, mFull))
-- Over-capacity slope must be visibly steeper than the in-capacity slope.
check((mFull - mOver) > (mHalf - mFull),
    string.format("over-cap drop (%.3f) steeper than in-cap drop (%.3f)",
        mFull - mOver, mHalf - mFull))
-- The band itself (sprint) tracks the multiplier.
check(sOver < sFull and sFull < sHalf and sHalf < sEmpty,
    "sprint speed strictly decreases with load")

-----------------------------------------------------------
-- 2. Endurance eases the curve: at the SAME ratio, higher endurance = faster.
-----------------------------------------------------------
print("[2] endurance eases the curve (same load ratio)")
local mWeak,   _ = setUnit(1, 100, 100, 0.5)   -- at-capacity, weak
local mNorm,   _ = setUnit(1, 100, 100, 1.0)   -- at-capacity, nominal
local mStrong, _ = setUnit(1, 100, 100, 2.0)   -- at-capacity, strong
check(mWeak < mNorm, string.format("at-cap: weak(0.5) %.3f < nominal(1.0) %.3f", mWeak, mNorm))
check(mNorm < mStrong, string.format("at-cap: nominal(1.0) %.3f < strong(2.0) %.3f", mNorm, mStrong))
-- Same comparison while over capacity (the steep regime) — ordering holds.
local oWeak,   _ = setUnit(1, 100, 150, 0.5)
local oStrong, _ = setUnit(1, 100, 150, 2.0)
check(oWeak < oStrong, string.format("over-cap: weak %.3f < strong %.3f", oWeak, oStrong))

-----------------------------------------------------------
-- 2b. Ambient wander (meander) must respond to load too — including the
--     case the review caught: an AGILE unit whose meander is pinned by the
--     fixed max_speed cap, not by comfort. Agility 1.2 makes the raw cap
--     bind when empty, so without the encumbrance term on that cap a 40%
--     load would amble at exactly the same speed as empty.
-----------------------------------------------------------
print("[2b] meander (ambient wander) slows with load, even when the max_speed cap binds")
setUnit(1, 100, 0,  1.0, 1.2)   -- agile, empty
local wEmpty = ms.meander(1)
setUnit(1, 100, 40, 1.0, 1.2)   -- agile, 40% load (within capacity)
local wMid = ms.meander(1)
setUnit(1, 100, 100, 1.0, 1.2)  -- agile, at capacity
local wFull = ms.meander(1)
check(wMid < wEmpty,  string.format("meander: 40%% load %.4f < empty %.4f", wMid, wEmpty))
check(wFull < wMid,   string.format("meander: at-cap %.4f < 40%% load %.4f", wFull, wMid))

-----------------------------------------------------------
-- 3. Unloaded / missing-data safety.
-----------------------------------------------------------
print("[3] unloaded + missing-capacity safety")
local mZero, _ = setUnit(1, 100, 0, 1.0)
check(approx(mZero, 1.0), string.format("zero load -> %.3f (≈1.0)", mZero))
-- No capacity stat (e.g. a def without a carry stat) -> neutral 1.0.
U[2] = { cap = nil, carried = 5, endur = 1.0 }
check(approx(ms.encumbranceMultiplier(2), 1.0), "nil capacity -> 1.0 (neutral)")
U[3] = { cap = 0, carried = 5, endur = 1.0 }
check(approx(ms.encumbranceMultiplier(3), 1.0), "zero capacity -> 1.0 (neutral, no divide-by-zero)")
-- Floor holds for an absurd overload.
local mCrush, _ = setUnit(1, 100, 1000, 0.3)
check(mCrush >= 0.20 - 1e-9, string.format("crushing overload -> %.3f >= floor 0.20", mCrush))

print("")
if failures == 0 then
    print("ALL CHECKS PASSED")
    os.exit(0)
else
    print(string.format("%d CHECK(S) FAILED", failures))
    os.exit(1)
end
