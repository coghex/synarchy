-- Consumable drink effects (#347): quality- and temperature-scaled
-- hydration + caffeine + mood from drinking a registered fluid item
-- (coffee first). Mechanism-only, mirroring craft.execute/till.designate
-- — it applies the effects of A drink rather than itself deciding WHEN
-- to drink. WHO may ask, and when, belongs to its callers.
--
-- Since #1580 the player is one of those callers: the Drink submenu
-- scripts/consumable_gestures.lua builds onto a unit-info inventory row
-- reaches `drinkInstance` below, so the effects here are on a shipped
-- game path and no longer only in tools/consumable_effects_probe.py.
-- That gesture owns the PLAYER policy (commandable faction, idle unit,
-- which exact instance) and this module owns the mechanism; nothing
-- here consults the faction or the activity, so an AI caller is not
-- gated on either.
--
-- Autonomous AI consumption is still a follow-up: unit_ai_needs.lua's
-- drink_from_canteen owns "thirst" for plain water, and whether/when a
-- unit should reach for coffee instead (or as well) is a real
-- prioritisation question outside this module's "does drinking coffee
-- do the right scaled things" scope.
--
-- Two entry points, ONE effect body:
--   drink(uid, defName)        the original mechanism call — the FIRST
--                              non-empty instance of `defName`
--                              (tools/consumable_effects_probe.py's
--                              contract, unchanged).
--   drinkInstance(uid, iid)    an EXACT held instance, named by the
--                              player. Never falls back to another
--                              instance of the same def: a merged
--                              inventory row can hold a scalding pot
--                              and a stone-cold one (#1268), and
--                              silently sipping the other one is
--                              precisely the failure the gesture's
--                              per-instance submenu exists to avoid.

local stats = require("scripts.unit_stats")

local consumable = {}

-- Per-held-defName config. sip_litres mirrors unit_ai_tunables.lua's
-- canteen convention (a bounded "one action" dose, not the whole container).
local EFFECTS = {
    coffee_pot = {
        sip_litres           = 0.25,
        -- Same rate as canteen water (unit_ai_tunables.lua's
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

local clamp = require("scripts.lib.numeric").clamp

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

-- Is `defName` a registered consumable at all? The gesture asks before
-- it offers anything, so an unregistered def produces no executable
-- action rather than an entry that fails on click.
function consumable.isRegistered(defName)
    return EFFECTS[defName] ~= nil
end

-- The EXACT held instance `instanceId`, if it is drinkable RIGHT NOW:
-- still in uid's loose inventory, of a registered def, and non-empty.
-- Returns the live item table (as unit.getInventory reports it), or nil
-- plus a reason.
--
-- `unit.getInventory` answers with the loose inventory ALONE, so an
-- equipped or accessory instance is structurally absent here and reads
-- as "gone" — which is the right answer for both: it is no longer a
-- loose item the player pointed at, and unit.modifyItemFillById would
-- not reach it either.
--
-- This is deliberately the SAME predicate the gesture filters its
-- submenu with and the one it re-checks on click, so a stale menu
-- cannot pass a check the offer would have failed.
function consumable.eligibleInstance(uid, instanceId)
    if type(instanceId) ~= "number" then return nil, "no instance" end
    local inv = unit.getInventory(uid)
    if not inv then return nil, "no unit" end
    for _, it in ipairs(inv) do
        if it.instanceId == instanceId then
            if not EFFECTS[it.defName] then
                return nil, "no consumable config for " .. tostring(it.defName)
            end
            if not it.currentFill or it.currentFill <= 0 then
                return nil, "empty"
            end
            return it
        end
    end
    return nil, "no such instance"
end

-- The shared effect body: apply one sip of `cfg` from the resolved live
-- item `it`. Both entry points below end here, so the arithmetic,
-- curves, clamps and result summary exist exactly once.
local function applySip(uid, it, cfg)
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

    -- Drain the EXACT instance the effects above were computed from —
    -- unit.modifyItemFill drains the first item matching defName, which
    -- is the wrong instance whenever an earlier, already-empty same-def
    -- container precedes the one findFirstWithFill picked (it would
    -- clamp at zero on the empty one, leaving the sipped pot untouched
    -- and re-drinkable for free).
    unit.modifyItemFillById(uid, it.instanceId, -sip)
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

-- Drink one sip of `defName` (a container registered in EFFECTS) from
-- uid's inventory. Returns a summary table on success:
--   { sip, quality, warmth, hydration, caffeine, mood }
-- (the actual deltas applied, so a caller can verify they vary with the
-- source instance's quality/temperature), or nil + a reason string.
--
-- FIRST-NON-EMPTY-BY-defName selection, unchanged: this is the
-- mechanism call tools/consumable_effects_probe.py drives, and its
-- whole point is that a caller which has no instance in hand can still
-- ask for "a coffee". A caller that DOES have one — the player's Drink
-- gesture — must use drinkInstance below instead.
function consumable.drink(uid, defName)
    local cfg = EFFECTS[defName]
    if not cfg then return nil, "no consumable config for " .. tostring(defName) end

    local it = findFirstWithFill(uid, defName)
    if not it then return nil, "nothing to drink" end

    return applySip(uid, it, cfg)
end

-- Drink one sip from the EXACT held instance `instanceId`. Same summary
-- and same reason strings as `drink` above, and the same effect body —
-- only the selection differs, and it does not fall back: an instance
-- that has been consumed, emptied, equipped or dropped since the caller
-- resolved it fails here rather than quietly sipping a sibling.
function consumable.drinkInstance(uid, instanceId)
    local it, why = consumable.eligibleInstance(uid, instanceId)
    if not it then return nil, why end
    return applySip(uid, it, EFFECTS[it.defName])
end

return consumable
