-- Offline regression harness for movement_speed.lua's WHOLE-BAND speed
-- multipliers (#305 encumbrance, extended by #1948 to all five).
--
-- movement_speed.lua scales the whole speed band by five modifiers —
-- injuries, salts, exhaustion, starvation and a carried-weight/capacity
-- ratio eased by endurance — and every gait must carry all five. This
-- test loads the module against a synthetic unit table and asserts:
--
--   1. Same unit, empty vs near-capacity vs over-capacity → strictly
--      DECREASING speed.
--   2. Same load ratio, high vs low endurance → the high-endurance unit is
--      faster (endurance eases the curve).
--  2b. The ambient meander responds to load even when the raw max_speed
--      cap (not half-comfort) is the binding branch.
--   3. Unloaded behaviour unchanged — multiplier ≈ 1.0 at light load.
--   4. (#1948) EACH of the five modifiers changes `meander` by exactly the
--      factor it contributes to the speed band, with the raw cap binding,
--      so no modifier can be hidden by which branch of the `min` wins.
--   5. (#1948) The shipped red_squirrel, at its YAML base stats and at an
--      agility roll high enough that the raw cap binds even at the
--      exhaustion floor, ambles measurably slower when fatigued.
--   6. (#1948) A unit with NO exhaustion / wound / salt / calorie /
--      carrying-capacity state meanders at exactly its pre-change speed.
--
-- Sections 4-6 compare against `preFixMeander` — the literal pre-#1948
-- formula — so a revert cannot pass silently, and so requirement 4's
-- "unchanged for a unit with no such state" is checked against the old
-- value rather than merely against itself.
--
-- Run: luajit tools/test_encumbrance_speed.lua

package.path = "./?.lua;" .. package.path

-----------------------------------------------------------
-- Unit-state the stubs serve: per-uid carrying_capacity, carried weight,
-- endurance, plus a max_speed / agility so sprint() is well-defined, an
-- arbitrary `stats` table (salt_conc / exhaustion / calories / ...) and a
-- `wounds` list. The four sibling multiplier modules are the REAL ones,
-- not stubs: injuries.lua and salts.lua require nothing at all, and
-- exhaustion.lua / starvation.lua reach only unit_stats + lib.numeric, so
-- this harness exercises their real curves rather than a paraphrase. All
-- four read as neutral 1.0 from the defaults below, which is what keeps
-- sections 1-3 measuring the encumbrance term alone.
-----------------------------------------------------------
local U = {}  -- uid -> { cap, carried, endur, agi, maxsp, stats, wounds }

unit = {
    getMaxSpeed = function(uid)
        local u = U[uid]
        return (u and u.maxsp) or 10.0
    end,
    getCarryingWeight = function(uid) return U[uid].carried end,
    getWounds = function(uid)
        local u = U[uid]
        return u and u.wounds or nil
    end,
    getStat = function(uid, name)
        local u = U[uid]
        if name == "agility" then return u.agi or 1.0 end
        if name == "endurance" then return u.endur end
        if name == "carrying_capacity" then return u.cap end
        if u.stats then return u.stats[name] end
        return nil
    end,
}

local ms = require("scripts.movement_speed")

-- The literal PRE-#1948 meander formula: the whole-band modifiers reached
-- only the half-comfort branch, while the raw max_speed branch carried
-- encumbrance alone. Sections 4-6 measure against this so the harness
-- states exactly what regressed instead of asserting a bare inequality.
local function preFixMeander(uid)
    return math.min(unit.getMaxSpeed(uid) * 0.25 * ms.encumbranceMultiplier(uid),
                    ms.comfort(uid) * 0.5)
end

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

-- Full-state setter for the #1948 sections: max_speed, agility, endurance,
-- an optional carry pair, an optional stats table (salt_conc, exhaustion /
-- max_exhaustion, calories / max_calories) and an optional wounds list.
-- Anything omitted stays ABSENT, which is exactly the neutral shape
-- requirement 4 is about.
local function setState(uid, t)
    U[uid] = {
        maxsp   = t.maxsp or 10.0,
        agi     = t.agi   or 1.0,
        endur   = t.endur or 1.0,
        cap     = t.cap,
        carried = t.carried or 0,
        stats   = t.stats,
        wounds  = t.wounds,
    }
    return uid
end

-- A leg fracture of the given severity — injuries.speedMultiplier reads
-- `severityEffective or severity` off a fracture/severed leg-like wound.
local function legFracture(sev)
    return { { kind = "fracture", part = "l_leg", severity = sev } }
end

print("== #305 encumbrance + #1948 whole-band speed multipliers ==")

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

-----------------------------------------------------------
-- 4. (#1948) EVERY whole-band modifier reaches the meander cap.
--
--    Shape: max_speed 10, agility 1.2, endurance 1.0, no load. The raw
--    branch is 2.5 x band and the half-comfort cap is 2.7 x band, so the
--    RAW branch binds for ANY band value — the band factor cancels out of
--    the comparison. That is precisely the configuration in which a
--    modifier confined to the comfort term is invisible, which is the bug.
--
--    For each modifier in isolation the harness asserts three things:
--      * ms.bandMultiplier reports the expected factor,
--      * sprint scales by exactly that factor (it contributes to the band),
--      * meander scales by exactly the SAME factor,
--    and that the raw cap is still the binding branch, so the meander
--    change cannot have leaked in through half-comfort instead.
-----------------------------------------------------------
print("[4] every whole-band modifier scales meander by its band factor (raw cap binding)")

local BAND_SHAPE = { maxsp = 10.0, agi = 1.2, endur = 1.0 }
local function bandShape(extra)
    local t = { maxsp = BAND_SHAPE.maxsp, agi = BAND_SHAPE.agi, endur = BAND_SHAPE.endur }
    for k, v in pairs(extra or {}) do t[k] = v end
    return t
end

setState(1, bandShape{})
local nBand    = ms.bandMultiplier(1)
local nSprint  = ms.sprint(1)
local nMeander = ms.meander(1)
check(approx(nBand, 1.0), string.format("neutral band -> %.4f (=1.0)", nBand))
check(approx(nMeander, 2.5), string.format("neutral meander -> %.4f (raw 10*0.25)", nMeander))

-- name, expected multiplier, state that produces it
local BAND_CASES = {
    { "injuries (leg fracture 0.6)", 0.40, { wounds = legFracture(0.6) } },
    { "salts (salt_conc 0.50)",      0.40, { stats = { salt_conc = 0.50 } } },
    { "exhaustion (empty)",          0.55, { stats = { exhaustion = 0, max_exhaustion = 100 } } },
    { "starvation (empty store)",    0.60, { stats = { calories = 0, max_calories = 1000 } } },
    { "encumbrance (at capacity)",   0.70, { cap = 100, carried = 100 } },
}

local product = 1.0
for _, case in ipairs(BAND_CASES) do
    local label, want, state = case[1], case[2], case[3]
    product = product * want
    setState(1, bandShape(state))
    local band    = ms.bandMultiplier(1)
    local sprint  = ms.sprint(1)
    local meander = ms.meander(1)
    local cap     = ms.comfort(1) * 0.5
    check(approx(band, want, 1e-9),
        string.format("%s: band %.4f (expected %.4f)", label, band, want))
    check(approx(sprint / nSprint, want, 1e-9),
        string.format("%s: sprint x%.4f of neutral", label, sprint / nSprint))
    check(approx(meander / nMeander, want, 1e-9),
        string.format("%s: meander x%.4f of neutral (SAME factor)", label, meander / nMeander))
    check(meander < cap - 1e-9,
        string.format("%s: raw cap binds (%.4f < half-comfort %.4f)", label, meander, cap))
    -- The pre-#1948 formula would have moved meander by a DIFFERENT factor
    -- (nothing at all for the four non-encumbrance modifiers).
    local pre = preFixMeander(1)
    if label:find("encumbrance") then
        check(approx(pre, meander, 1e-9),
            string.format("%s: unchanged by #1948 (already both-branch, #305)", label))
    else
        check(pre > meander + 1e-9,
            string.format("%s: pre-#1948 meander %.4f was NOT slowed (now %.4f)", label, pre, meander))
    end
end

-- All five at once: the band is their product, and meander tracks it.
setState(1, bandShape{
    wounds = legFracture(0.6),
    stats  = { salt_conc = 0.50, exhaustion = 0, max_exhaustion = 100,
               calories = 0, max_calories = 1000 },
    cap = 100, carried = 100,
})
local cBand, cMeander = ms.bandMultiplier(1), ms.meander(1)
check(approx(cBand, product, 1e-9),
    string.format("all five: band %.6f = product %.6f", cBand, product))
check(approx(cMeander / nMeander, product, 1e-9),
    string.format("all five: meander x%.6f of neutral", cMeander / nMeander))
check(cMeander < ms.comfort(1) * 0.5 - 1e-12, "all five: raw cap still binds")

-- Requirement 3 properties, on the same shape.
-- (a) meander is never above half comfort.
check(cMeander <= ms.comfort(1) * 0.5 + 1e-12, "meander <= comfort x 0.5")
-- (b) meander is NOT agility-scaled: two otherwise identical units whose
--     only difference is agility amble at the same speed while the raw
--     cap binds.
setState(1, bandShape{ agi = 1.2 })
setState(2, bandShape{ agi = 2.5 })
check(approx(ms.meander(1), ms.meander(2), 1e-12),
    string.format("meander not agility-scaled: %.4f (agi 1.2) == %.4f (agi 2.5)",
        ms.meander(1), ms.meander(2)))
check(ms.sprint(2) > ms.sprint(1), "sprint IS agility-scaled (control)")
-- (c) A wandering unit still recovers stamina: the drain tick reads
--     speed/comfort, and that ratio must not RISE under a modifier. It is
--     now invariant (both branches carry the same factor); before #1948 it
--     climbed as the band shrank.
setState(1, bandShape{})
local restedRatio = ms.meander(1) / ms.comfort(1)
setState(1, bandShape{ stats = { exhaustion = 0, max_exhaustion = 100 } })
local tiredRatio = ms.meander(1) / ms.comfort(1)
check(tiredRatio <= restedRatio + 1e-12,
    string.format("meander/comfort ratio does not rise when fatigued: %.4f -> %.4f",
        restedRatio, tiredRatio))

-----------------------------------------------------------
-- 5. (#1948) The shipped red_squirrel actually ambles slower when spent.
--
--    data/units/red_squirrel.yaml: max_speed 5.0, agility base 2.5,
--    endurance base 0.6. It owns the full exhaustion resource
--    (unit_resource_config.lua) and sends its aimless wander through
--    ambient.wanderTo -> mv.meander (red_squirrel_ai.lua), so this is the
--    exact shape the issue measured.
-----------------------------------------------------------
print("[5] shipped red_squirrel: fatigue reaches the amble")

local SQUIRREL_MAXSP = 5.0
local EXH_FLOOR = 0.55      -- exhaustion.lua FATIGUE_MIN_MULT

local function squirrel(agi, endur, exhausted)
    local stats = nil
    if exhausted then stats = { exhaustion = 0, max_exhaustion = 100 } end
    setState(1, { maxsp = SQUIRREL_MAXSP, agi = agi, endur = endur, stats = stats })
    return ms.meander(1), ms.sprint(1), preFixMeander(1)
end

-- 5a. YAML base stats.
local bRested,  bSprintR, _        = squirrel(2.5, 0.6, false)
local bTired,   bSprintT, bTiredPre = squirrel(2.5, 0.6, true)
check(approx(bRested, 1.25, 1e-9),
    string.format("base: rested meander %.4f (raw 5.0*0.25, cap binds)", bRested))
check(approx(bTired, 1.25 * EXH_FLOOR, 1e-9),
    string.format("base: fatigued meander %.4f = rested x %.2f", bTired, EXH_FLOOR))
check(bTired < bRested * 0.60,
    string.format("base: fatigued amble %.1f%% slower (was ~4%%)",
        100 * (1 - bTired / bRested)))
check(approx(bSprintT / bSprintR, EXH_FLOOR, 1e-9),
    string.format("base: sprint x%.4f — amble and sprint slow by the SAME factor",
        bSprintT / bSprintR))
check(bTiredPre > bTired * 1.5,
    string.format("base: pre-#1948 fatigued meander was %.4f, now %.4f", bTiredPre, bTired))

-- 5b. The live-measured individual from the finding (agility 2.432920): a
--     5.5% pre-fix slowdown, which a bare `fatigued < rested` assertion
--     would have accepted.
local mRested = squirrel(2.432920, 0.615896, false)
local mTired, _, mTiredPre = squirrel(2.432920, 0.615896, true)
check(mTiredPre > mRested * 0.90,
    string.format("measured roll: pre-#1948 slowdown was only %.1f%% (%.4f -> %.4f)",
        100 * (1 - mTiredPre / mRested), mRested, mTiredPre))
check(mTired < mRested * 0.60,
    string.format("measured roll: now %.1f%% slower (%.4f -> %.4f)",
        100 * (1 - mTired / mRested), mRested, mTired))

-- 5c. A high agility roll where the raw cap binds even at the exhaustion
--     floor: pre-#1948 this individual's amble did not slow AT ALL.
local hRested = squirrel(3.0, 0.6, false)
local hTired, _, hTiredPre = squirrel(3.0, 0.6, true)
check(approx(hTiredPre, hRested, 1e-9),
    string.format("agile roll: pre-#1948 fatigue changed NOTHING (%.4f -> %.4f)",
        hRested, hTiredPre))
check(approx(hTired, hRested * EXH_FLOOR, 1e-9),
    string.format("agile roll: fatigued meander %.4f = rested x %.2f", hTired, EXH_FLOOR))
check(hTired < hRested,
    string.format("agile roll: fatigued %.4f < rested %.4f", hTired, hRested))
-- Still the raw branch, so no agility scaling crept in.
check(approx(hRested, bRested, 1e-9),
    string.format("agile roll: rested amble %.4f unchanged by agility", hRested))

-----------------------------------------------------------
-- 6. (#1948, requirement 4) Neutral defaults are untouched. A def with no
--    exhaustion / wound / salt / calorie / carrying-capacity state, and a
--    unit whose resources exist but have not been ticked yet, both meander
--    at exactly the PRE-CHANGE speed.
-----------------------------------------------------------
print("[6] neutral defaults meander at exactly the pre-change speed")

local NEUTRAL_SHAPES = {
    { "bare def, raw cap binds",      { maxsp = 10.0, agi = 1.2, endur = 1.0 } },
    { "bare def, half-comfort binds", { maxsp = 10.0, agi = 0.5, endur = 1.0 } },
    { "shipped red_squirrel shape",   { maxsp = 5.0,  agi = 2.5, endur = 0.6 } },
    -- Resources present but never ticked: exhaustion.fraction defaults to
    -- 1.0 on a nil current value, starvation.fraction to nil (unaffected).
    { "not-yet-ticked resources",     { maxsp = 10.0, agi = 1.2, endur = 1.0,
                                        stats = { max_exhaustion = 100, max_calories = 1000 } } },
    -- Carry stat present, nothing carried.
    { "carry stat, empty hands",      { maxsp = 10.0, agi = 1.2, endur = 1.0,
                                        cap = 100, carried = 0 } },
    -- A wound that is not a leg fracture must not slow anything.
    { "non-leg wound",                { maxsp = 10.0, agi = 1.2, endur = 1.0,
                                        wounds = { { kind = "slash", part = "l_arm",
                                                     severity = 0.9 } } } },
}

for _, shape in ipairs(NEUTRAL_SHAPES) do
    local label, state = shape[1], shape[2]
    setState(1, state)
    local band = ms.bandMultiplier(1)
    local now, pre = ms.meander(1), preFixMeander(1)
    check(approx(band, 1.0, 1e-12), string.format("%s: band %.6f (=1.0)", label, band))
    check(approx(now, pre, 1e-12),
        string.format("%s: meander %.4f == pre-change %.4f", label, now, pre))
    check(now <= ms.comfort(1) * 0.5 + 1e-12,
        string.format("%s: meander <= comfort x 0.5", label))
end

print("")
if failures == 0 then
    print("ALL CHECKS PASSED")
    os.exit(0)
else
    print(string.format("%d CHECK(S) FAILED", failures))
    os.exit(1)
end
