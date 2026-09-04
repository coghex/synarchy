-- Medical supply discovery, by exact container instance (#2302).
--
-- A unit can legitimately hold two containers of the SAME definition:
-- looted kits, technomule stocking (#1855), an ordinary transfer. And
-- unit.getItemContents resolves the FIRST held item matching defName
-- whenever no instance id is given (#67, Inventory.hs). So a scan that
-- walks the per-instance inventory rows but asks by defName alone reads
-- the SAME container once per row -- a stocked kit sitting behind an
-- empty same-definition sibling is invisible to it.
--
-- The treatment verbs disagreed: treatBleeding / treatInfection scan the
-- real containers and take the first STOCKED one (Medical.hs kits /
-- kitHasFill), so the commit succeeded in exactly the case discovery
-- called impossible -- the medic walked off to fetch or improvised a
-- tourniquet with bandages in its own pack, and the context menu greyed
-- a row the verb would have honoured.
--
-- One scan, keyed by each row's own instanceId, shared by both
-- consumers so the greyed state and the commit cannot drift apart.
-- Same migration #1220 made for the canteen the AI drains and refills.

local M = {}

-- The first container the unit holds whose contents satisfy `holds` for
-- an item named `want`, as { defName = ..., instanceId = ... } -- the
-- exact identity, so a caller can name it to a later verb rather than
-- re-resolving it by definition. nil when it carries none.
local function findSupplyKit(uid, want, holds)
    for _, it in ipairs(unit.getInventory(uid) or {}) do
        if it.kind == "container" then
            -- The row's own instanceId is the whole point: without it
            -- every iteration of this loop asks about the first kit.
            local rows = unit.getItemContents(uid, it.defName,
                                              it.instanceId) or {}
            for _, r in ipairs(rows) do
                if r.defName == want and holds(r) then
                    return { defName = it.defName,
                             instanceId = it.instanceId }
                end
            end
        end
    end
    return nil
end

local function anyCount(r) return (r.count or 0) > 0 end
local function anyFill(r)  return (r.fill or 0) > 0 end

-- The kit the unit carries holding at least one bandage, or nil. What
-- the autonomous medic checks before fetching, and what "Treat
-- bleeding" is enabled on.
function M.bandageKit(uid)
    return findSupplyKit(uid, "bandage", anyCount)
end

-- The kit the unit carries holding antibiotics, or nil. The fill test
-- stays ANY remaining fill rather than Medical.hs's one-pill dose: that
-- pre-existing threshold difference is a separate eligibility question
-- (#2297's neighbourhood), not the instance aliasing this fixes.
function M.antibioticsKit(uid)
    return findSupplyKit(uid, "antibiotics", anyFill)
end

return M
