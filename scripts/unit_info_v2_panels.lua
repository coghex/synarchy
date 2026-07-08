-- Unit info v2 Physical/Mental/Skill/Knowledge sub-tabs (#542 split
-- from unit_info_v2.lua).
--
-- The non-Status stat panels — static stat-row lists (Physical,
-- Mental) plus two dynamic ones whose row set depends on the unit
-- (Skill: whatever skills it has; Knowledge: the full knowledge
-- catalogue, known first). All build on top of buildIconStatPanel
-- (unit_info_v2_panel_engine.lua) and statRow (unit_info_v2_stat_defs.lua).

local knowledge    = require("scripts.knowledge")
local statDefs     = require("scripts.unit_info_v2_stat_defs")
local panelEngine  = require("scripts.unit_info_v2_panel_engine")
local L            = require("scripts.unit_info_v2_layout")

local STAT_DEFS = statDefs.STAT_DEFS
local statRow   = statDefs.statRow
local fmtNum    = statDefs.fmtNum

local M = {}

-- Body-composition breakdown for the weight row's hover tooltip:
-- splits body_mass into lean / fat / other.
local function weightHint(uid)
    local body = unit.getStat(uid, "body_mass")
    local lean = unit.getStat(uid, "lean_mass")
    local fat  = unit.getStat(uid, "fat_mass")
    if not (body and lean and fat) then
        return "(body composition not yet computed)"
    end
    local other = body - lean - fat
    return string.format(
        "Lean (muscle):  %.1f kg\n"
     .. "Fat:            %.1f kg\n"
     .. "Other (bone, organs, water): %.1f kg",
        lean, fat, other)
end

-- Physical panel: stat rows in roughly-importance order, then the
-- body-attribute rows (height + weight) absorbed from the former
-- "Attributes" panel. Combat-relevant stats up top; metabolism +
-- the body measurements get the bottom.
function M.buildPhysicalPanel(rect, uid)
    return panelEngine.buildIconStatPanel(rect, uid, {
        statRow("strength"),
        statRow("endurance"),
        statRow("reflexes"),
        statRow("constitution"),
        statRow("toughness"),
        statRow("dexterity"),
        statRow("agility"),
        statRow("metabolism"),
        { key = "height", value = function(u)
            local h = unit.getStat(u, "height")
            return h and string.format("%.2f m", h) or "?"
        end },
        { key   = "weight",
          value = function(u)
              local m = unit.getStat(u, "body_mass")
              return m and string.format("%.1f kg", m) or "?"
          end,
          tooltip = function(u)
              return {
                  text = STAT_DEFS.weight.name,
                  hint = STAT_DEFS.weight.desc .. "\n\n" .. weightHint(u),
              }
          end,
        },
    }, "stat")
end

function M.buildMentalPanel(rect, uid)
    return panelEngine.buildIconStatPanel(rect, uid, {
        statRow("intelligence"),
        statRow("perception"),
    }, "stat")
end

function M.buildSkillPanel(rect, uid)
    -- Skills are dynamic — list whatever the unit has, sorted. Each
    -- skill name doubles as the icon basename and the STAT_DEFS key,
    -- so adding a skill icon makes it pick up automatically.
    local all = unit.getAllSkills(uid) or {}
    local names = {}
    for n, _ in pairs(all) do names[#names + 1] = n end
    table.sort(names)
    local rows = {}
    for _, n in ipairs(names) do
        rows[#rows + 1] = {
            key   = n,
            value = function(u)
                local s = (unit.getAllSkills(u) or {})[n]
                return s and fmtNum(s.level) or "?"
            end,
            tooltip = (not STAT_DEFS[n]) and {
                text = n:sub(1,1):upper() .. n:sub(2),
                hint = "Skill level. Improves with practice.",
            } or nil,
        }
    end
    return panelEngine.buildIconStatPanel(rect, uid, rows, "skill")
end

-- Knowledge panel: the catalogue of knowledge TYPES (scripts/knowledge.lua),
-- KNOWN first then unknown. A known knowledge shows its icon + trained
-- level (right-aligned, like a skill), with the effective value
-- (level × intelligence) in the tooltip. A knowledge the unit hasn't learned
-- shows the "unknown" icon + a dim "Unknown" — the player sees a slot exists
-- but not what it is until it's learned (from a book/teacher).
function M.buildKnowledgePanel(rect, uid)
    local known, unknown = {}, {}
    for _, k in ipairs(knowledge.list()) do
        local kk = k   -- capture for the closures
        if unit.getKnowledge(uid, kk.id) ~= nil then
            known[#known + 1] = {
                key   = kk.icon,
                value = function(u)
                    local l = unit.getKnowledge(u, kk.id)
                    return l and fmtNum(l) or "?"
                end,
                tooltip = function(u)
                    local l   = unit.getKnowledge(u, kk.id) or 0
                    local int = unit.getStat(u, "intelligence") or 1.0
                    return { text = kk.name,
                             hint = kk.desc
                                 .. string.format(
                                    "\n\nLevel %s × intelligence %.2f = effective %s.",
                                    fmtNum(l), int, fmtNum(l * int)) }
                end,
            }
        else
            unknown[#unknown + 1] = {
                key   = knowledge.UNKNOWN_ICON,
                value = function() return "Unknown" end,
                opts  = { fontSize = L.CONDITION_FONT_SIZE,
                          color = L.CONTENT_DIM_COLOR, align = "left",
                          abbreviate = true },
                tooltip = { text = "Unknown",
                            hint = "This unit hasn't learned this knowledge.\n"
                                .. "It can be learned from a book or a teacher." },
            }
        end
    end
    -- Known capabilities lead; unlearned slots follow.
    local rows = {}
    for _, r in ipairs(known)   do rows[#rows + 1] = r end
    for _, r in ipairs(unknown) do rows[#rows + 1] = r end
    return panelEngine.buildIconStatPanel(rect, uid, rows, "knowledge")
end

return M
