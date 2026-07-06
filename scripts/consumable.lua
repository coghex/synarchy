-- Consumable drink effects (#347): quality- and temperature-scaled
-- hydration + caffeine + mood from drinking a registered fluid item
-- (coffee first). Mechanism-only, mirroring craft.execute/till.designate
-- — callable directly (debug console, tests, a future AI action) rather
-- than itself deciding WHEN to drink. Autonomous AI consumption is left
-- to a follow-up: unit_ai.lua's drink_from_canteen already owns
-- "thirst" for plain water, and whether/when a unit should reach for
-- coffee instead (or as well) is a real prioritisation question outside
-- this issue's "does drinking coffee do the right scaled things" scope.

local stats = require("scripts.unit_stats")

local consumable = {}

-- Per-held-defName config. sip_litres mirrors unit_ai.lua's canteen
-- convention (a bounded "one action" dose, not the whole container).
local EFFECTS = {
    coffee_pot = {
        sip_litres           = 0.25,
        -- Same rate as canteen water (unit_ai.lua's
        -- drink_hydration_per_litre) at quality 100; scales down with
        -- quality per the epic's "excellent hydrates more" ask.
        hydration_per_litre  = 11.0,
        -- A full-quality, still-hot litre saturates the 0..1 caffeine
        -- meter (brain.lua) in 4 sips.
        caffeine_per_litre   = 1.0,
        -- Mood swing at the quality extremes; 0 at quality_mid (the
        -- "average" quality-tier boundary — Item.Types.defaultQualityTiers,
        -- #345) so a middling cup is mood-neutral, not a small boost.
        mood_swing_per_litre = 0.3,
        quality_mid          = 50,
        -- Warmth reads as fully fresh at/above hot_temp, bottoms out
        -- at/below cold_temp. A coffee_pot brews at 100°C
        -- (data/recipes/basic_food.yaml's output_temp, #346) and cools
        -- toward ambient over real game-time (#344), so a cup left
        -- sitting genuinely reads as less fresh.
        hot_temp             = 60,
        cold_temp            = 25,
        warmth_floor         = 0.3,
    },
}

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

-- 0..1: 1 at/above hot_temp, warmth_floor at/below cold_temp, linear
-- between. nil temp (no ambient to fall back on) reads as fully fresh
-- rather than penalizing a call the engine couldn't resolve a temp for.
local function warmthFactor(cfg, temp)
    if not temp or temp >= cfg.hot_temp then return 1.0 end
    if temp <= cfg.cold_temp then return cfg.warmth_floor end
    local t = (temp - cfg.cold_temp) / (cfg.hot_temp - cfg.cold_temp)
    return cfg.warmth_floor + (1.0 - cfg.warmth_floor) * t
end

local function findFirstWithFill(uid, defName)
    local inv = unit.getInventory(uid)
    if not inv then return nil end
    for _, it in ipairs(inv) do
        if it.defName == defName and it.currentFill and it.currentFill > 0 then
            return it
        end
    end
    return nil
end

-- Drink one sip of `defName` (a container registered in EFFECTS) from
-- uid's inventory. Returns a summary table on success:
--   { sip, quality, warmth, hydration, caffeine, mood }
-- (the actual deltas applied, so a caller can verify they vary with the
-- source instance's quality/temperature), or nil + a reason string.
function consumable.drink(uid, defName)
    local cfg = EFFECTS[defName]
    if not cfg then return nil, "no consumable config for " .. tostring(defName) end

    local it = findFirstWithFill(uid, defName)
    if not it then return nil, "nothing to drink" end

    local sip = math.min(cfg.sip_litres, it.currentFill)
    if sip <= 0 then return nil, "empty" end

    local quality = it.quality or cfg.quality_mid
    local temp    = unit.getItemTemp(uid, it.instanceId)
    local warmth  = warmthFactor(cfg, temp)
    local qualityFrac = quality / 100.0

    -- Hydration: quality-scaled (excellent hydrates more than
    -- atrocious, per the epic's stated ask).
    local hydration = sip * cfg.hydration_per_litre * qualityFrac
    local maxHyd = stats.get(uid, "max_hydration")
    if maxHyd and maxHyd > 0 then
        local hyd = unit.getStat(uid, "hydration") or 0
        unit.setStat(uid, "hydration", math.min(maxHyd, hyd + hydration))
    end

    -- Caffeine (brain.lua's stimulant meter): quality- and warmth-
    -- scaled — a stone-cold cup barely jolts you.
    local caffeineGain = sip * cfg.caffeine_per_litre * qualityFrac * warmth
    local caffeine = unit.getStat(uid, "caffeine") or 0.0
    unit.setStat(uid, "caffeine", clamp(caffeine + caffeineGain, 0, 1))

    -- Mood: a one-off nudge fed into the normal drift (brain.lua) — it
    -- fades at the usual MOOD_DRIFT rate like any other mood event,
    -- rather than snapping and holding.
    local moodDelta = sip * cfg.mood_swing_per_litre
                     * (quality - cfg.quality_mid) / cfg.quality_mid
    local mood = unit.getStat(uid, "mood")
    if mood == nil then mood = 1.0 end
    unit.setStat(uid, "mood", clamp(mood + moodDelta, 0, 1))

    unit.modifyItemFill(uid, defName, -sip)
    unit.drink(uid)

    return {
        sip       = sip,
        quality   = quality,
        warmth    = warmth,
        hydration = hydration,
        caffeine  = caffeineGain,
        mood      = moodDelta,
    }
end

return consumable
