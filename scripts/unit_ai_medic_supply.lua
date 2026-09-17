-- Unit AI medic SUPPLY phase (#2644 split from unit_ai_medic.lua).
--
-- Split out for the reason the unit_ai_save splits name in their own
-- headers: unit_ai_medic.lua's 500-line budget (#538,
-- tools/lua_module_budget.py), which #2644's per-medicine fetch and its
-- futile-cure bound pushed it over. The boundary is cohesive rather
-- than convenient -- everything here answers one question, namely "can
-- this patient's treatment be supplied at all, and by whom", in its
-- three parts:
--
--   * which medicine a HOLDER could hand over (holderSupply), keyed by
--     exact container instance through scripts/medical_supply.lua;
--   * which holder the medic should walk to (findKitHolder), page-
--     qualified against the asking medic;
--   * what the medic remembers when the answer is "nobody"
--     (deferPatient / deferralLifted), which is the bound on the
--     otherwise endless futile-cure loop.
--
-- Nothing here claims a patient, ranks a medic, moves, transfers or
-- treats: unit_ai_medic.lua owns all of that and is the only consumer.

local distance = require("scripts.unit_ai_core").distance
local page = require("scripts.unit_ai_page")
-- Exact-instance medical supply discovery, shared with the context menu.
local supply = require("scripts.medical_supply")

local M = {}

-- #2644: the futile-cure bound. A medic that discovers the medicine a
-- patient REQUIRES is nowhere on the page releases the claim and
-- records the patient here, so the next decision picks ordinary work
-- instead of re-acquiring the same impossible treatment at lock
-- utility -- scripts/unit_ai_tunables.lua gives treat_ally a base AND a
-- lock utility of 8.0, so a bare release is retaken immediately.
--
-- SUPPLY_RETRY_SECONDS of game time later the patient is eligible
-- again, which is why this is a deferral and not a permanent
-- blacklist: antibiotics can be crafted, looted or hauled in while the
-- medic is off doing something useful. A module constant beside
-- CLOT_ENOUGH / INFECT_TREAT_MIN rather than a tunable, because like
-- those two it is a property of the treatment itself and not a dial
-- any species config sets. A minute is long enough for the squad to
-- actually do other work and short enough that a bottle arriving in
-- the meantime is picked up promptly.
--
-- Shape: s.treatDefer = { [patientUid] = gameTime the deferral lifts }.
-- TRANSIENCE: never persisted -- see scripts/unit_ai_save_transient.lua,
-- which strips it for the reason every other AI clock is stripped.
local SUPPLY_RETRY_SECONDS = 60.0

function M.deferralLifted(st, patientUid)
    local tbl = st and st.treatDefer
    if not tbl then return true end
    local lifts = tbl[patientUid]
    if not lifts then return true end
    if engine.gameTime() < lifts then return false end
    -- Expired: drop it on read so the table cannot grow without bound
    -- over a long session, and drop the table itself once it is empty.
    tbl[patientUid] = nil
    if next(tbl) == nil then st.treatDefer = nil end
    return true
end

function M.deferPatient(st, patientUid)
    st.treatDefer = st.treatDefer or {}
    st.treatDefer[patientUid] = engine.gameTime() + SUPPLY_RETRY_SECONDS
end

-- The window itself, so a gate can assert on the bound rather than
-- restate the number.
M.SUPPLY_RETRY_SECONDS = SUPPLY_RETRY_SECONDS

local ownKit = supply.bandageKit
-- The antibiotics half of the same question (#2644). Both scans are
-- medical_supply's, so the medic and the context menu keep answering
-- "can this unit supply X" exactly alike; what the medic adds on top is
-- the holder PAGE qualification below, which the menu does not need.
local ownAntibiotics = supply.antibioticsKit

-- What one holder could supply out of `wants`, or nil for nothing:
--   kit    = the exact container to fetch THIS tick
--   covers = how many of the wanted medicines this holder carries at
--            all, across every container it holds
-- The two differ whenever bandages and antibiotics sit in separate
-- carried containers, which is ordinary (#2644): the fetch moves one
-- container per tick, while `covers` is what makes a holder stocking
-- BOTH medicines outrank a nearer one stocking only half of them.
-- Bandages are fetched first when a holder has both in separate kits:
-- the bleeder is the more urgent of the two needs, and the medic comes
-- back for the cure on the next tick.
local function holderSupply(uid, wants)
    local bKit = wants.bandage and ownKit(uid) or nil
    local aKit = wants.antibiotics and ownAntibiotics(uid) or nil
    if not bKit and not aKit then return nil end
    return { kit = bKit or aKit,
             covers = (bKit and 1 or 0) + (aKit and 1 or 0) }
end

-- Nearest unit carrying a usable kit (the technomule), to fetch from.
-- Page-qualified against the asking medic (#1673), same rule and same
-- reason as fetch.findTechnomule: unit.getAllIds reads the ACTIVE page,
-- which is not necessarily the medic's own.
--
-- `wants` (#2644) names the medicines the medic is MISSING, defaulting
-- to bandages alone -- the question this search used to be able to ask.
-- A holder covering more of them wins outright; distance only breaks
-- ties, so the medic does not make two trips when one unit nearby holds
-- everything.
function M.findKitHolder(medicUid, fromX, fromY, wants)
    local myPage = page.ofUnit(medicUid)
    if not myPage then return nil end
    wants = wants or { bandage = true }
    local best, bestD, bestCovers = nil, math.huge, 0
    for _, uid in ipairs(unit.getAllIds() or {}) do
        local got = holderSupply(uid, wants)
        if got then
            local info = unit.getInfo(uid)
            if info and page.same(myPage, info.page) then
                local d = distance(fromX, fromY, info.gridX, info.gridY)
                if got.covers > bestCovers
                   or (got.covers == bestCovers and d < bestD) then
                    best = { uid = uid, gridX = info.gridX,
                             gridY = info.gridY, kit = got.kit.defName,
                             kitInstance = got.kit.instanceId }
                    bestD, bestCovers = d, got.covers
                end
            end
        end
    end
    return best
end

return M
