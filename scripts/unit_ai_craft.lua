-- Unit AI craft-bill jobs (#538 split from unit_ai.lua).
--
-- Action: craft_job  (#329). Executes standing per-station craft
-- bills (craft.addBill) against the recipe catalogue (#325) at Built
-- work stations (#326): claim a bill (engine-side atomic CAS —
-- Craft.Bills, no module-local claim registry like construct/dig),
-- source inputs + fuel (inventory → ground → mule → cargo, the #96
-- ladder plus a stockpile rung), walk beside the station, pour work
-- in (craft.addBillProgress, skill-scaled), and at 1.0 fire the craft
-- (craft.executeAt), drop the new outputs at the station, grant XP,
-- and advance the bill (craft.completeBillCycle).
--
-- Skill: the recipe's `skill` tag when present, else "smithing".
-- Material races are NOT reserved cross-unit — same self-heal as
-- construction.
--
-- Pause (#796): stops after the current cycle, not instantly. Not yet
-- working (still fetching/walking) → abort + release now. Already
-- working → finish this cycle; completeBillCycle then drops the claim
-- itself instead of chaining into another one.
-----------------------------------------------------------

local core = require("scripts.unit_ai_core")
local distance        = core.distance
local chebToFootprint = core.chebToFootprint
local grantWorkXP     = core.grantWorkXP

local fetch = require("scripts.unit_ai_fetch")
local inventoryCountOf     = fetch.inventoryCountOf
local groundCountOf        = fetch.groundCountOf
local untilStockSatisfied  = fetch.untilStockSatisfied
local findTechnomule       = fetch.findTechnomule
local fetchWantsFromGround = fetch.fetchWantsFromGround
local fetchWantsFromMule   = fetch.fetchWantsFromMule
local deliverItemWeight    = fetch.deliverItemWeight

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")

local M = {}

local function craftSkillOf(recipe)
    return recipe.skill or "smithing"
end

-- Everything one craft cycle consumes: inputs plus fuel, summed by
-- def name (mirrors Craft.Types.recipeDemands).
local function craftDemands(recipe)
    local d = {}
    for _, ing in ipairs(recipe.inputs or {}) do
        d[ing.item] = (d[ing.item] or 0) + (ing.count or 1)
    end
    if recipe.fuel then
        d[recipe.fuel.item] = (d[recipe.fuel.item] or 0)
                            + (recipe.fuel.count or 1)
    end
    return d
end

-- Is the bill held by someone else whose claim is still fresh?
-- Mirrors the engine's claimAvailable staleness rules so the utility
-- scan doesn't chase bills it can't win (the authoritative check is
-- still the atomic craft.claimBill at claim time).
local function billClaimedByOther(bill, uid, now, timeout)
    if not bill.claimant or bill.claimant == uid then return false end
    if not unit.exists(bill.claimant) then return false end
    return (now - (bill.claimedAt or 0)) <= timeout
end

-- Is this bill paused against a FRESH claim by uid? Mirrors the
-- engine's claimAvailable pause gate (#330): a paused bill refuses
-- every claimant except the one who already holds it, so a worker
-- mid-cycle on a bill that gets paused finishes that cycle, but no one
-- (including uid) may start a new claim on it. Without this check
-- findCraftBill kept nominating paused bills as candidates, and the
-- doomed craft.claimBill call at commit time (which the engine
-- correctly refuses) left the worker repeatedly picking a bill it can
-- never win instead of reachable unpaused work.
local function billPausedForUs(bill, uid)
    return bill.paused and bill.claimant ~= uid
end

-- Count of defName across BUILT storage buildings (cargo holds) on
-- the active world — the stockpile rung of the craft sourcing ladder.
-- No range limit, same rationale as the mule: stored materials are
-- worth the walk.
local function cargoCountOf(defName)
    local total = 0
    for _, bid in ipairs(building.getActiveIds() or {}) do
        if bid and building.getActivity(bid) == "built" then
            for _, it in ipairs(building.getStorage(bid) or {}) do
                if it.defName == defName then total = total + 1 end
            end
        end
    end
    return total
end

-- Walk toward the nearest border tile of a building's footprint.
-- Returns true while still walking (Chebyshev > 1 → moveTo issued),
-- false once the unit stands on or beside the footprint. Shared by
-- the craft walking phase and the cargo fetch (same approach as
-- deliver / build_nearby).
local function moveBesideBuilding(uid, info, binfo)
    local tw, th = binfo.tileW or 1, binfo.tileH or 1
    local utx, uty = math.floor(info.gridX), math.floor(info.gridY)
    if chebToFootprint(utx, uty, binfo.gridX, binfo.gridY, tw, th) <= 1 then
        return false
    end
    local bestX, bestY, bestD = nil, nil, math.huge
    for dx = -1, tw do
        for dy = -1, th do
            if dx == -1 or dx == tw or dy == -1 or dy == th then
                local nx = binfo.gridX + dx + 0.5
                local ny = binfo.gridY + dy + 0.5
                local d = distance(info.gridX, info.gridY, nx, ny)
                if d < bestD then bestX, bestY, bestD = nx, ny, d end
            end
        end
    end
    if bestX then
        unit.moveTo(uid, bestX, bestY, mv.comfort(uid))
    end
    return true
end

-- Fetch loop against cargo storage: walk beside the nearest BUILT
-- store holding any wanted def and withdraw everything wanted from it
-- in one visit (unit.withdrawFromCargo preserves the instances;
-- adjacency is this walk, per the API contract). Entries clear even
-- on shortfall, like the mule fetch — raced withdrawals and split
-- stock resolve by the caller reconciling against inventory (a
-- release + re-plan reaches the next store). Returns true while
-- still busy (walking).
local function fetchWantsFromCargo(uid, wants, info, params)
    if not next(wants) then return false end
    local best, bestD = nil, math.huge
    for _, bid in ipairs(building.getActiveIds() or {}) do
        if bid and building.getActivity(bid) == "built" then
            local has = false
            for _, it in ipairs(building.getStorage(bid) or {}) do
                if wants[it.defName] then has = true; break end
            end
            if has then
                local binfo = building.getInfo(bid)
                if binfo then
                    local d = distance(info.gridX, info.gridY,
                        binfo.gridX + (binfo.tileW or 1) / 2,
                        binfo.gridY + (binfo.tileH or 1) / 2)
                    if d < bestD then
                        binfo.bid = bid
                        best, bestD = binfo, d
                    end
                end
            end
        end
    end
    if not best then
        -- Nothing stored anywhere (someone else withdrew it).
        for k in pairs(wants) do wants[k] = nil end
        return false
    end
    if moveBesideBuilding(uid, info, best) then return true end
    unit.stop(uid)
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    for defName, count in pairs(wants) do
        for _ = 1, count do
            local w = deliverItemWeight(defName)
            if carried + w > maxW then
                engine.logWarn("fetch: unit " .. tostring(uid)
                    .. " at capacity (" .. string.format("%.1f", carried + w)
                    .. " > " .. string.format("%.1f", maxW)
                    .. " kg) — leaving rest of " .. defName .. " in cargo")
                break
            end
            if not unit.withdrawFromCargo(uid, best.bid, defName) then
                break    -- store ran out (raced another claimant)
            end
            carried = carried + w
        end
        wants[defName] = nil
    end
    return false
end

-- Can this unit source every demand right now (inventory + nearby
-- ground + mule stock + cargo storage)? Races lose gracefully at
-- fetch time; this is only the "worth claiming" filter, same as
-- construction's.
local function craftMaterialsAvailable(uid, fromX, fromY, demands, params)
    for item, need in pairs(demands) do
        local have = inventoryCountOf(uid, item)
        if have < need then
            local ground = groundCountOf(fromX, fromY, item,
                                         params.craft_scan_range)
            if have + ground < need then
                local mule = findTechnomule(fromX, fromY)
                local muleHave = mule
                    and inventoryCountOf(mule.uid, item) or 0
                if have + ground + muleHave < need
                   and have + ground + muleHave + cargoCountOf(item) < need
                then
                    return false
                end
            end
        end
    end
    return true
end

-- Nearest workable bill within craft_scan_range, or nil (station alive + Built,
-- unclaimed, stock target not met (#795), knowledge cleared, demands sourceable).
local function findCraftBill(uid, fromX, fromY, params)
    local bills = craft.getBills()
    if not bills or #bills == 0 then return nil end
    local now = engine.gameTime()
    local best, bestD = nil, params.craft_scan_range
    for _, bill in ipairs(bills) do
        if not billPausedForUs(bill, uid)
           and not billClaimedByOther(bill, uid, now, params.craft_claim_timeout)
           and not untilStockSatisfied(bill) then
            local binfo = building.getInfo(bill.station)
            if binfo and building.getActivity(bill.station) == "built" then
                local recipe = craft.get(bill.recipe)
                if recipe
                   and (not recipe.knowledge
                        or unit.getKnowledge(uid, recipe.knowledge)) then
                    local demands = craftDemands(recipe)
                    if craftMaterialsAvailable(uid, fromX, fromY,
                                               demands, params) then
                        local tw = binfo.tileW or 1
                        local th = binfo.tileH or 1
                        local d = distance(fromX, fromY,
                                           binfo.gridX + tw / 2,
                                           binfo.gridY + th / 2)
                        if d <= bestD then
                            best = { bill = bill, recipe = recipe,
                                     demands = demands, dist = d }
                            bestD = d
                        end
                    end
                end
            end
        end
    end
    return best
end

-- Release the unit's hold on its craft job. toPending hands the bill
-- back engine-side so another crafter can take it; omitted = the bill
-- is already gone (completed / cancelled) or owned by someone else.
local function releaseCraftJob(s, uid, toPending)
    if s.craftJob and toPending then
        craft.releaseBill(s.craftJob.billId)
    end
    s.craftJob = nil
    s.craftCandidate = nil
end

local function craftUtility(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return -math.huge end

    -- Active job: finite lock-in (dire needs still preempt; the
    -- engine-side claim survives the interruption, and its timeout
    -- re-opens the bill if we're gone too long). Dropped when the
    -- bill vanishes (player cancelled / finished by whoever took our
    -- expired claim) or when someone else legally owns it now.
    if s.craftJob then
        local bill = craft.getBill(s.craftJob.billId)
        if bill and (not bill.claimant or bill.claimant == uid) then
            return params.craft_lock_utility
        end
        if bill then
            -- Re-claimed by another crafter while we were preempted —
            -- walk away WITHOUT releasing their claim.
            s.craftJob = nil
            s.craftCandidate = nil
        else
            releaseCraftJob(s, uid)
        end
    end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local cand = findCraftBill(uid, info.gridX, info.gridY, params)
    if not cand then return -math.huge end

    s.craftCandidate = cand
    local distFactor = math.max(0, 1 - cand.dist / params.craft_scan_range)
    return params.craft_base_utility * distFactor
         * roles.weight(s, "craft_job")
end

local function craftExecute(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return end
    local info = unit.getInfo(uid)
    if not info then return end
    local now = engine.gameTime()

    -- Claim the scanned candidate: one atomic engine-side CAS. The
    -- loser of a race just clears its candidate and re-plans.
    if not s.craftJob then
        local cand = s.craftCandidate
        if not cand then return end
        s.craftCandidate = nil
        if untilStockSatisfied(cand.bill) then return end -- #795: stock rose
        if not craft.claimBill(cand.bill.id, uid,
                               params.craft_claim_timeout) then
            return
        end
        -- Fetch shortfalls, planned once at claim time (inventory →
        -- ground → mule → cargo). Reconciled against real inventory
        -- after the fetch phases run.
        local need, fromGround, fromMule, fromCargo = {}, {}, {}, {}
        local mule = findTechnomule(info.gridX, info.gridY)
        for item, count in pairs(cand.demands) do
            need[item] = count
            local have = inventoryCountOf(uid, item)
            local short = count - have
            if short > 0 then
                local ground = math.min(short,
                    groundCountOf(info.gridX, info.gridY, item,
                                  params.craft_scan_range))
                if ground > 0 then fromGround[item] = ground end
                local muleTake = 0
                if short - ground > 0 and mule then
                    muleTake = math.min(short - ground,
                                        inventoryCountOf(mule.uid, item))
                    if muleTake > 0 then fromMule[item] = muleTake end
                end
                if short - ground - muleTake > 0 then
                    fromCargo[item] = short - ground - muleTake
                end
            end
        end
        s.craftJob = {
            billId   = cand.bill.id,
            bid      = cand.bill.station,
            recipeId = cand.recipe.id,
            work     = cand.recipe.work or 0,
            skill    = craftSkillOf(cand.recipe),
            need     = need,
            fromGround = fromGround,
            fromMule   = fromMule,
            fromCargo  = fromCargo,
            phase    = "fetch",
        }
        return
    end

    local job = s.craftJob
    -- The bill can vanish (player cancel) or pass to another crafter
    -- (our claim expired while preempted) at any point — bail cleanly.
    local bill = craft.getBill(job.billId)
    if not bill then
        releaseCraftJob(s, uid)
        return
    end
    if bill.claimant and bill.claimant ~= uid then
        s.craftJob = nil
        return
    end
    -- Paused (#796) but not yet working (bill.working mirrors the
    -- engine's cbWorking boundary exactly): abort now — fetched inputs
    -- are already real inventory on this unit, so nothing strands.
    -- Already working falls through to finish the in-flight cycle.
    if bill.paused and not bill.working then
        releaseCraftJob(s, uid, true)
        return
    end
    -- Keep the claim fresh (claimBill by the holder is a refresh).
    craft.claimBill(job.billId, uid, params.craft_claim_timeout)

    -- Phase 1: source inputs + fuel (inventory → ground → mule →
    -- cargo storage).
    if job.phase == "fetch" then
        if fetchWantsFromGround(uid, job.fromGround, params,
                                params.craft_scan_range) then
            return
        end
        if fetchWantsFromMule(uid, job.fromMule, info, params) then
            return
        end
        if fetchWantsFromCargo(uid, job.fromCargo, info, params) then
            return
        end
        for item, count in pairs(job.need) do
            if inventoryCountOf(uid, item) < count then
                -- Sources came up short (raced / capacity) — hand the
                -- bill back for someone who can cover it.
                releaseCraftJob(s, uid, true)
                return
            end
        end
        job.phase = "walking"
    end

    -- Phase 2: stand beside the station — nearest border tile, same
    -- walk as deliver (craft.executeAt requires Chebyshev ≤ 1).
    if job.phase == "walking" then
        local binfo = building.getInfo(job.bid)
        if not binfo then          -- station demolished mid-job
            releaseCraftJob(s, uid, true)
            return
        end
        if moveBesideBuilding(uid, info, binfo) then return end
        unit.stop(uid)
        job.phase = "working"
        s.lastCraftAt = now
        -- #590: mark the bill as ACTIVELY worked only now — fetching
        -- and walking (above) never drew power; standing at the
        -- station about to pour progress does. craftOnExit/completion/
        -- release all clear this back off.
        craft.setBillWorking(job.billId, true)
        return
    end

    -- Phase 3: pour work into the bill. Progress rides the engine
    -- (persisted; a replacement crafter resumes rather than restarts)
    -- and the verbs are synchronous, so the returned value is
    -- authoritative — no local copy needed.
    if job.phase == "working" then
        -- #590: a power-drawing recipe (job.recipeId's power_draw > 0)
        -- pours no progress this tick while its station can't be
        -- satisfied — idle, not failed. A zero-power recipe always
        -- passes, wired or not. job.billId is passed so the engine
        -- excludes THIS bill's own already-registered draw before
        -- re-adding it, rather than double-counting it (or dropping
        -- some other simultaneous consumer at the same station).
        -- Reset lastCraftAt so the elapsed-time accumulator doesn't
        -- credit the unpowered gap once power returns (same guard the
        -- phase transitions above already use).
        if not power.isStationPoweredForRecipe(job.bid, job.recipeId, job.billId) then
            s.lastCraftAt = now
            return
        end
        local elapsed = now - (s.lastCraftAt or now)
        s.lastCraftAt = now
        local progress = bill.progress or 0
        if elapsed > 0 and job.work > 0 then
            -- Trade skill scales the rate (#265); #353 folds in the
            -- crafter's canonical mental effectiveness (1.00 = unchanged).
            local level = unit.getSkill(uid, job.skill) or 0.0
            local delta = params.craft_rate * (0.5 + level / 100.0)
                        * (unit.getMentalEffectiveness(uid) or 1.0)
                        * elapsed / job.work
            progress = craft.addBillProgress(job.billId, delta) or progress
        end
        if job.work <= 0 then progress = 1.0 end
        if progress >= 1.0 then
            local ok, res = craft.executeAt(uid, job.recipeId, job.bid, job.billId)
            if not ok then
                -- Inventory raced away / station broke between ticks —
                -- hand the bill back and let the next scan re-plan.
                engine.logWarn("craft_job: executeAt failed for unit "
                    .. tostring(uid) .. ": " .. tostring(res))
                releaseCraftJob(s, uid, true)
                return
            end
            -- executeAt returns the FRESH outputs' instance ids; lay
            -- exactly those down at the station (visible + sourceable)
            -- — never a same-def item the crafter already carried.
            for _, iid in ipairs(type(res) == "table" and res or {}) do
                unit.dropItemById(uid, iid)
            end
            grantWorkXP(uid, job.skill, params.craft_xp_per_craft or 0)
            local remaining = craft.completeBillCycle(job.billId)
            if remaining and remaining ~= 0
               and not (bill.paused or untilStockSatisfied(bill)) then
                job.phase = "fetch"      -- next cycle: source again
            else
                -- #795: an until-stock claim survives a satisfied cycle; release.
                releaseCraftJob(s, uid, untilStockSatisfied(bill) and not bill.paused)
            end
        end
        return
    end
end

-- Preempted mid-work (thirst, combat, player order): re-enter through
-- the walking phase so the elapsed-time accumulator restarts — same
-- guard as construction's onExit. Also clears #590's working flag: a
-- crafter walked away from the station mid-cycle is no longer drawing
-- power for it.
local function craftOnExit(uid, s, params)
    local job = s.craftJob
    if job and job.phase == "working" then
        job.phase = "walking"
        craft.setBillWorking(job.billId, false)
    end
end

M.craftUtility = craftUtility
M.craftExecute = craftExecute
M.craftOnExit  = craftOnExit

return M
