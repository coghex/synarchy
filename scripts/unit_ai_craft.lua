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
--
-- Page binding (#2325): a craft job holds one numeric billId across many
-- ticks, and bill ids are PER PAGE (every page's allocator starts at 1),
-- while the applied visible page moves on the WORLD thread. So every
-- bill call below passes the ACTING UNIT and the engine resolves the id
-- on that unit's own page -- craft.getBill(uid, id),
-- craft.releaseBill(uid, id), craft.setBillWorking(uid, id, flag),
-- craft.addBillProgress(uid, id, delta), craft.completeBillCycle(uid, id)
-- and craft.claimBill(id, uid, timeout), plus the billId handed to
-- craft.executeAt. The #1673 station guard below is NOT a substitute: it
-- is not atomic with the calls that follow it, and releaseCraftJob is
-- reachable on paths that never run it. craft.getBills() is the one
-- deliberate exception -- an ACTIVE-page discovery listing whose every
-- candidate findCraftBill re-checks against the actor's own page.
-----------------------------------------------------------

local core = require("scripts.unit_ai_core")
local distance        = core.distance
local grantWorkXP     = core.grantWorkXP

local fetch = require("scripts.unit_ai_fetch")
local inventoryCountOf     = fetch.inventoryCountOf
local groundCountOf        = fetch.groundCountOf
local untilStockSatisfied  = fetch.untilStockSatisfied
local findTechnomule       = fetch.findTechnomule
local fetchWantsFromGround = fetch.fetchWantsFromGround
local fetchWantsFromMule   = fetch.fetchWantsFromMule
local fetchWantsFromCargo  = fetch.fetchWantsFromCargo
local cargoCountOf         = fetch.cargoCountOf
local moveBesideBuilding   = fetch.moveBesideBuilding
local loadFeasible         = fetch.loadFeasible

local roles = require("scripts.unit_roles")
local page = require("scripts.unit_ai_page")
-- The shared work-clock bound (#2332). unit_ai_stall requires nothing,
-- so naming it here is cycle-free -- the same import auto-harvest makes.
local stall = require("scripts.unit_ai_stall")

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

-- Can this unit source every demand right now (inventory + nearby
-- ground + mule stock + cargo storage) AND carry the shortfall it would
-- have to fetch (#1326)? Races lose gracefully at fetch time; this is
-- only the "worth claiming" filter, same as construction's.
local function craftMaterialsAvailable(uid, fromX, fromY, demands, params)
    if not loadFeasible(uid, demands) then return false end
    local myPage = page.ofUnit(uid)      -- #1673: the actor's own page
    for item, need in pairs(demands) do
        local have = inventoryCountOf(uid, item)
        if have < need then
            local ground = groundCountOf(uid, fromX, fromY, item,
                                         params.craft_scan_range)
            if have + ground < need then
                local mule = findTechnomule(uid, fromX, fromY)
                local muleHave = mule and inventoryCountOf(mule.uid, item) or 0
                local off = have + ground + muleHave
                if off < need and off + cargoCountOf(item, myPage) < need then
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
    -- #1673: craft.getBills reads the ACTIVE page's bill store on its
    -- own, independently of the page the actor was selected from, so
    -- every station is re-checked against the ACTOR's page below.
    -- #2325 keeps it that way deliberately -- this is discovery, and a
    -- bill whose station passes the page check below necessarily came
    -- from the actor's own page's store (craft.addBill only ever queues a
    -- bill on the page its station stands on), which is what makes the
    -- id safe to hand to the actor-qualified verbs afterwards.
    local myPage = page.ofUnit(uid)
    if not myPage then return nil end
    local bills = craft.getBills()
    if not bills or #bills == 0 then return nil end
    local now = engine.gameTime()
    local best, bestD = nil, params.craft_scan_range
    for _, bill in ipairs(bills) do
        if not billPausedForUs(bill, uid)
           and not billClaimedByOther(bill, uid, now, params.craft_claim_timeout)
           and not untilStockSatisfied(bill) then
            local binfo = building.getInfo(bill.station)
            if binfo and page.same(myPage, binfo.page)
               and building.getActivity(bill.station) == "built" then
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
        -- #2325: uid binds the release to the crafter's OWN page. Every
        -- caller of this helper has one, including craftUtility's
        -- vanished-bill path, which runs no page check of its own.
        craft.releaseBill(uid, s.craftJob.billId)
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
        local bill = craft.getBill(uid, s.craftJob.billId)
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
        local mule = findTechnomule(uid, info.gridX, info.gridY)
        for item, count in pairs(cand.demands) do
            need[item] = count
            local have = inventoryCountOf(uid, item)
            local short = count - have
            if short > 0 then
                local ground = math.min(short,
                    groundCountOf(uid, info.gridX, info.gridY, item,
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
    local bill = craft.getBill(uid, job.billId)
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
    -- #1673: job.bid is a PERSISTED building reference, so a save
    -- written before this check (or a page switch mid-job) can name a
    -- station on another world. Revalidate it against the crafter's own
    -- page HERE, ahead of every phase: the fetch phase below issues its
    -- own moveTo / pickupGround / transferItemToUnit / withdrawFromCargo
    -- calls, so a check placed at the walking phase would already have
    -- let an off-page job walk the unit and move items. Re-runs every
    -- tick, since each fetch phase returns early while busy.
    local binfo = building.getInfo(job.bid)
    if not binfo or not page.same(info.page, binfo.page) then
        releaseCraftJob(s, uid, true)   -- demolished or off-page
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
        -- Station identity and page were revalidated above, ahead of
        -- the fetch phase; binfo is that same live lookup.
        if moveBesideBuilding(uid, info, binfo) then return end
        unit.stop(uid)
        job.phase = "working"
        s.lastCraftAt = now
        -- #590: mark the bill as ACTIVELY worked only now — fetching
        -- and walking (above) never drew power; standing at the
        -- station about to pour progress does. craftOnExit/completion/
        -- release all clear this back off, and since #2332 so does
        -- unit_ai_stall.suspendOrders, for a crafter knocked down at
        -- the station on a path that fires no onExit at all.
        craft.setBillWorking(uid, job.billId, true)
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
        -- #2332: the pour is charged only for one uninterrupted
        -- stretch of AI ticking. Every path that swallows a tick drops
        -- the stamp (unit_ai_stall.suspendOrders, and the phase resets
        -- above), and an interval past the shared bound -- a gap no
        -- path could announce -- charges ZERO rather than the bound.
        local elapsed = stall.workInterval(s.lastCraftAt, now)
        s.lastCraftAt = now
        local progress = bill.progress or 0
        if elapsed > 0 and job.work > 0 then
            -- Trade skill scales the rate (#265); #353 folds in the
            -- crafter's canonical mental effectiveness (1.00 = unchanged).
            local level = unit.getSkill(uid, job.skill) or 0.0
            local delta = params.craft_rate * (0.5 + level / 100.0)
                        * (unit.getMentalEffectiveness(uid) or 1.0)
                        * elapsed / job.work
            progress = craft.addBillProgress(uid, job.billId, delta)
                       or progress
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
            local remaining = craft.completeBillCycle(uid, job.billId)
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
        craft.setBillWorking(uid, job.billId, false)
    end
end

M.craftUtility = craftUtility
M.craftExecute = craftExecute
M.craftOnExit  = craftOnExit

return M
