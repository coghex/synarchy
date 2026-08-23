-- The player's Drink gesture (#1580): the "Drink" entry
-- scripts/unit_info_v2_context_menu.lua hangs off a selected unit's
-- inventory row, and the only shipped path into scripts/consumable.lua.
--
-- Its own module for the same reason scripts/transfer_gestures.lua is
-- one: the context-menu module is inside the 500-line unit_info_v2_*
-- budget (tools/lua_module_budget.py), and a gesture with its own
-- eligibility rules, its own labelling and its own revalidation does
-- not belong in the file that merely lays menus out.
--
-- WHY A SUBMENU (#1268). The shared item-list widget deliberately keeps
-- tracked temperature OUT of the stack key -- an item cools
-- continuously, so keying on it would split and re-merge a row forever
-- -- and scripts/ui/item_list.lua's stackKey contract states the
-- consequence: the members of a merged row are interchangeable for the
-- temperature-INSENSITIVE single-instance actions (Equip, Contents,
-- Prioritize Repair, "Store 1"), and a temperature-SENSITIVE one may
-- NOT inherit the representative. Drinking is exactly that: warmth
-- scales the caffeine gain (scripts/consumable.lua), so a row holding a
-- scalding pot and a stone-cold one offers two genuinely different
-- drinks. So the row stays merged and the GESTURE fans out: one submenu
-- entry per eligible member, each naming its own temperature.
--
-- WHY IDLE ONLY. unit.drink queues UnitDrink, and
-- src/Unit/Thread/Command/Pose.hs ignores it unless the unit is Idle --
-- so a gesture offered to a walking unit would silently apply the stats
-- and the drain with no drinking animation at all. Offering nothing is
-- the honest presentation; queuing the drink for later is deliberately
-- out of scope (a busy unit does not consume and retains no deferred
-- order), so the player simply asks again when the unit has stopped.
--
-- WHAT THIS IS NOT: an autonomous need. unit_ai_needs.lua's
-- drink_from_canteen still owns "thirst" for plain water and is
-- untouched; coffee is a thing the PLAYER asks for.
--
-- Public API:
--   drinkEntries(uid, row)  -- 0 or 1 menu entries for an inventory row

local consumable = require("scripts.consumable")
local itemList   = require("scripts.ui.item_list")

local M = {}

local MENU_LABEL = "Drink"

-- Can this unit be told to drink RIGHT NOW? Both halves are re-asked on
-- click (requirement 5): selection is not authorization
-- (unit_info_v2_tabs.lua derives activeUid from ANY selected unit,
-- wildlife included), and a unit can start walking between the menu
-- opening and the player picking an entry.
local function commandableIdle(uid)
    if not uid then return false end
    if not faction.isPlayerCommandable(unit.getFaction(uid)) then return false end
    return unit.getActivity(uid) == "idle"
end

-- One entry's player-visible temperature, from the SAME value the
-- warmth factor is computed from -- unit.getItemTemp, the EFFECTIVE
-- temperature. Deliberately not the row's raw `temp` field: that is the
-- TRACKED iiTemp and is absent for an at-ambient item, so a label built
-- from it would say "ambient" about a pot whose effective temperature
-- the sip then scales caffeine by.
--
-- Rounded through the widget's own itemList.roundTemp so a submenu
-- entry and the row text above it present the same degree, and so an
-- absurd magnitude saturates instead of raising.
local function tempLabel(uid, iid)
    local t = unit.getItemTemp(uid, iid)
    if type(t) ~= "number" or t ~= t
       or t == math.huge or t == -math.huge then
        return "ambient"
    end
    return string.format("%d°C", itemList.roundTemp(t))
end

-- Disambiguate entries whose temperature reads identically. Two pots
-- one tenth of a degree apart present as the same rounded label, and a
-- player must still be able to say WHICH -- so a repeated label gains a
-- 1-based ordinal in the row's own membership order, which is stable
-- for as long as the menu is open. Labels that are already unique are
-- left exactly as they are, so the common case reads "42°C" and not
-- "42°C (1)".
local function disambiguate(labels)
    local seen = {}
    for _, l in ipairs(labels) do seen[l] = (seen[l] or 0) + 1 end
    local nth = {}
    local out = {}
    for i, l in ipairs(labels) do
        if seen[l] > 1 then
            nth[l] = (nth[l] or 0) + 1
            out[i] = string.format("%s (%d)", l, nth[l])
        else
            out[i] = l
        end
    end
    return out
end

-- The Drink entries for one inventory row, as a list to append to a
-- context menu: exactly one entry carrying a submenu, or NOTHING.
--
-- Nothing is offered when the row is equipped or an accessory (an
-- equipped pot is its own single-instance row -- unit_info_v2_inventory
-- passes separateEquipped, so it never merges into the loose one), when
-- the unit is not commandable or not idle, when the def is not a
-- registered consumable, or when no represented instance is still
-- drinkable. Requirement 4 is satisfied either by omitting the entry or
-- by disabling it; omitting keeps a menu free of actions the player
-- cannot take.
function M.drinkEntries(uid, row)
    if not row or row.equipped then return {} end
    if not consumable.isRegistered(row.defName) then return {} end
    if not commandableIdle(uid) then return {} end

    -- Every exact id the row stands for (#1249's `instanceIds`), not the
    -- representative alone -- that is the whole point of the fan-out.
    local ids, labels = {}, {}
    for _, iid in ipairs(itemList.rowInstanceIds(row)) do
        if consumable.eligibleInstance(uid, iid) then
            ids[#ids + 1]    = iid
            labels[#labels + 1] = tempLabel(uid, iid)
        end
    end
    if #ids == 0 then return {} end

    labels = disambiguate(labels)
    local submenu = {}
    for i, iid in ipairs(ids) do
        submenu[i] = {
            label    = labels[i],
            icon     = row.iconTex,
            callback = function()
                -- Revalidate BOTH halves against live state, never
                -- against the captured row: the menu has been open for
                -- an unbounded time and the unit may have started
                -- walking, been deselected from the player's command, or
                -- had this very pot emptied by something else.
                -- consumable.drinkInstance re-checks the instance itself
                -- and refuses rather than falling back, so a failure
                -- here changes no fill and no stat.
                if not commandableIdle(uid) then return end
                consumable.drinkInstance(uid, iid)
            end,
        }
    end
    return { { label = MENU_LABEL, submenu = submenu } }
end

return M
