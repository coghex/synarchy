-- | The "unit AI load reset" gate (#1329): the unit-AI family's ELEVEN
--   transient coordination registries -- the five coordinate claim
--   tables (@dig@, @chop@, @construct@, @till@, @plant@),
--   @repairClaims@ and @repairPriority@, plus #916's four same-tick
--   encounter episode overlays -- are cleared when a save load replaces
--   the session, and each coordinate key names the PAGE its tile belongs to.
--
--   None of the eleven lives in @aiState@, so none of them is persisted
--   and none was reached by @unitAi.onSaveLoaded@. Both id allocators
--   rewind across a load (@World.Load.Publish@ assigns
--   @nextItemInstanceIdRef@ straight from the save; @umNextId@ likewise),
--   so a surviving entry can attach to an unrelated entity -- and since
--   every timed table expires on @now - c.at > timeout@, a loaded clock
--   EARLIER than the session that wrote the claim never expires it at
--   all. @repairPriority@ has no timeout to begin with.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.UnitAiStall":
--   each 'it' runs one self-contained chunk via 'Lua.dostring' in a fresh
--   interpreter, asserting inside Lua via @assert()@, with a non-OK
--   'Lua.Status' surfaced as an hspec failure carrying the Lua message.
--
--   What is REAL here and what is stubbed matters, because the point is
--   to exercise the lifecycle rather than a hand-called hook. The claim
--   registries, their key builders, every module's own claim/release
--   path, the construction stale-claim sweep, and the whole
--   @snapshotAll -> prepareLoad -> applyAll@ load lifecycle (including
--   @scripts/unit_ai_save.lua@'s real component AND its reset-hook
--   registration) are the shipping code. The engine API surface those
--   reach is stubbed, as is @scripts/movement_speed@ -- it contributes
--   only a number handed straight to the @unit.moveTo@ stub, while its
--   real body drags in the entire injury/salt/exhaustion physiology
--   chain that no claim registry touches.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "unit AI load reset"'@.
module Test.Headless.Lua.UnitAiLoadReset (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Run one chunk in a fresh interpreter; a Lua error becomes an hspec
--   failure carrying the Lua message.
runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | Every case runs after this: stubbed engine globals, the real AI
--   modules, a real @lua.unit_ai@ registration, tunables, and the
--   helpers each case drives.
--
--   @claimAll@ takes all five coordinate registries at one tile on the
--   ACTIVE page through each module's own execute path; @canClaim@ asks
--   whether another unit could take the same tile, one FRESH state per
--   registry so a claim granted to one cannot mask another's refusal;
--   @claimRepair@ claims an item repair through @repairExecute@'s real
--   claim branch; @runLoad@ is the full load lifecycle.
prelude ∷ Text
prelude = lns

    [ "package.loaded['scripts.unit_ai'] = {}"
    , "PAGE, NOW, LIVE = 'A', 1000, {}"
    , "DESIG = { A = {}, B = {} }"
    , "STATUS = { A = {}, B = {} }"
    , "YAML_READS = 0"
    , "local function dk(x, y) return x .. ',' .. y end"
    , "local function desig(w, x, y) return DESIG[w] and DESIG[w][dk(x, y)] end"
    -- #1854: chop claims key on the FLORA INSTANCE, so the fixture needs
    -- a stable synthetic instance id per tile (and the reverse map the
    -- exact-instance verbs read). Deliberately page-INDEPENDENT, so the
    -- "same coordinate on two pages is two distinct claims" example
    -- still tests page qualification rather than being handed two
    -- different ids for free.
    , "IIDS, IIDTILE, IIDN, DESIG_N = {}, {}, 0, {}"
    -- A tile normally carries ONE designated plant; DESIG_N[tile] = n
    -- gives it n, which is what the co-tenant example needs.
    , "function iidOf(x, y, slot)"
    , "  local k = dk(x, y) .. '#' .. (slot or 1)"
    , "  if not IIDS[k] then"
    , "    IIDN = IIDN + 1; IIDS[k] = IIDN; IIDTILE[IIDN] = { x = x, y = y }"
    , "  end"
    , "  return IIDS[k]"
    , "end"
    , "function iidsAt(x, y)"
    , "  local out = {}"
    , "  for slot = 1, (DESIG_N[dk(x, y)] or 1) do"
    , "    out[#out + 1] = iidOf(x, y, slot)"
    , "  end"
    , "  return out"
    , "end"
    , "engine = { gameTime = function() return NOW end,"
    , "  logInfo = function() end, logWarn = function() end,"
    , "  logError = function() end, emitEventForUnit = function() end,"
    , "  loadYaml = function()"
    , "    YAML_READS = YAML_READS + 1"
    , "    return { build = { post = { materials = {}, build_work = 1.0 } } } end }"
    , "unit = { exists = function(u) return LIVE[u] == true end,"
    -- #1845: the construct lifecycle takes its page from the ACTING
    -- UNIT, not from world.getActiveWorldId, so this fixture's units
    -- have to report one. It is PAGE — these are active-page units —
    -- which keeps every case below exactly the scenario it was written
    -- as. Dropping it is not a detail: the construct path deliberately
    -- selects nothing for a unit whose page cannot be established, and
    -- six of the cases below go red.
    , "  getInfo = function(u) return LIVE[u]"
    , "    and { gridX = 0, gridY = 0, defName = 'acolyte', page = PAGE }"
    , "    or nil end,"
    , "  getStat = function() return 1.0 end,"
    , "  getSkill = function() return 25.0 end,"
    , "  getInventory = function() return {} end,"
    , "  moveTo = function() end, stop = function() end, addXP = function() end,"
    , "  setAnimOverride = function() end, clearAnimOverride = function() end }"
    , "world = { getActiveWorldId = function() return PAGE end,"
    , "  getMineDesignationAt = function(w, x, y)"
    , "    if desig(w, x, y) then return 0, 1, 1, 1, 1 end end,"
    , "  nearestMineDesignation = function() return nil end,"
    , "  getDigInfoAt = function() return nil, 1.0, 1.0, false end,"
    , "  getFluidAt = function() return nil end,"
    , "  getFloraAt = function() return nil end,"
    , "  getFloraGrowthAt = function() return nil end,"
    , "  harvestFloraInstance = function() end,"
    , "  getSurfaceAt = function() return 0 end,"
    , "  harvestFlora = function() end, setVegAt = function() end,"
    , "  plantCropAt = function() end, plantRowCropAt = function() end }"
    , "chop = { getDesignationAt = function(w, x, y)"
    , "    return desig(w, x, y) and"
    , "      { z = 0, x = x, y = y, instanceId = iidOf(x, y) } or nil end,"
    , "  getDesignationForInstance = function(w, i)"
    , "    local t = IIDTILE[i]"
    , "    return t and desig(w, t.x, t.y) and"
    , "      { z = 0, x = t.x, y = t.y, instanceId = i } or nil end,"
    , "  getDesignationsAt = function(w, x, y)"
    , "    if not desig(w, x, y) then return nil end"
    , "    local out = {}"
    , "    for _, i in ipairs(iidsAt(x, y)) do"
    , "      out[#out + 1] = { z = 0, x = x, y = y, instanceId = i } end"
    , "    return out end,"
    , "  nearestDesignation = function() return nil end,"
    , "  cancelDesignation = function() end }"
    , "till = { getDesignationAt = function(w, x, y)"
    , "    return desig(w, x, y) and { z = 0 } or nil end,"
    , "  nearestDesignation = function() return nil end,"
    , "  cancelDesignation = function() end }"
    , "plant = { getDesignationAt = function(w, x, y)"
    , "    return desig(w, x, y) and { z = 0, crop = 'oats', category = 'row' } or nil end,"
    , "  nearestDesignation = function() return nil end,"
    , "  cancelDesignation = function() end }"
    , "construction = { getDesignationAt = function(w, x, y)"
    , "    return desig(w, x, y) and { x = x, y = y, category = 'structure',"
    , "                                pack = 'p', kind = 'post', paid = true } or nil end,"
    , "  getPendingJobs = function() return JOBS or {} end,"
    , "  setJobStatus = function(w, x, y, st) STATUS[w][dk(x, y)] = st end,"
    , "  cancelDesignation = function() end, setMaterialsPaid = function() end,"
    , "  addJobProgress = function() end }"
    , "item = { spawnGround = function() end, listGround = function() return {} end,"
    , "  -- #1666: owning-page ground lookup; nothing on this page."
    , "  getGroundForUnit = function() return nil, true end,"
    , "  listDefs = function() return { { name = 'axe' }, { name = 'c' } } end }"
    , "building = { spawn = function() return nil end,"
    , "  listDefs = function() return { { name = 'hut' } } end }"
    , "structure = { floorZAt = function() return 0 end }"
    , "flora = { exists = function() return true end }"
    , "repair = { get = function() return { id = 'r' } end,"
    , "  repairAt = function() return true end }"
    , "-- movement_speed only supplies a number this fixture hands straight to"
    , "-- the unit.moveTo stub; its real body drags in the whole injury /"
    , "-- salt / exhaustion physiology chain, which no claim registry touches."
    , "package.loaded['scripts.movement_speed'] = {"
    , "  comfort = function() return 1.0 end, ordered = function() return 1.15 end,"
    , "  sprint = function() return 2.0 end, meander = function() return 0.5 end }"
    , "DIG = require('scripts.unit_ai_dig')"
    , "CHOP = require('scripts.unit_ai_chop')"
    , "CONSTRUCT = require('scripts.unit_ai_construct')"
    , "require('scripts.unit_ai_farm')"
    , "REPAIR = require('scripts.unit_ai_repair')"
    , "require('scripts.unit_ai_encounter')"
    , "AI = package.loaded['scripts.unit_ai']"
    , "CLAIMS = require('scripts.unit_ai_claims')"
    , "CORE = require('scripts.unit_ai_core')"
    , "SAVE = require('scripts.lib.save_modules')"
    , "require('scripts.unit_ai_save').register(CORE.aiState)"
    , "P = { dig_claim_timeout = 30, dig_scan_range = 20, dig_lock_utility = 6,"
    , "  dig_base_utility = 1, dig_arrival_tiles = 1.0, dig_xp_per_tile = 0,"
    , "  dig_tools = { shovel = { defs = {}, equip_anim = 'e', work_anim = 'w' },"
    , "                pick = { defs = {}, equip_anim = 'e', work_anim = 'w' } },"
    , "  chop_claim_timeout = 30, chop_scan_range = 20, chop_lock_utility = 6,"
    , "  chop_base_utility = 1, chop_bare_speed = 0.5, chop_tools = {},"
    , "  chop_equip_anim = 'e', chop_work_anim = 'w', chop_equip_seconds = 1,"
    , "  chop_rate = 1, chop_xp_per_fell = 0,"
    , "  till_claim_timeout = 30, till_scan_range = 20, till_lock_utility = 6,"
    , "  till_base_utility = 1, till_equip_seconds = 1, till_rate = 1,"
    , "  till_equip_anim = 'e', till_work_anim = 'w', till_xp_per_till = 0,"
    , "  plant_claim_timeout = 30, plant_scan_range = 20, plant_lock_utility = 6,"
    , "  plant_base_utility = 1, plant_equip_seconds = 1, plant_rate = 1,"
    , "  plant_equip_anim = 'e', plant_work_anim = 'w', plant_xp_per_plant = 0,"
    , "  construct_claim_timeout = 30, construct_scan_range = 20,"
    , "  construct_lock_utility = 6, construct_base_utility = 1,"
    , "  construct_scan_chunks = 1, repair_claim_timeout = 30 }"
    , "-- Claim every one of the five coordinate registries at (x,y) on the"
    , "-- ACTIVE page for `uid`, through each module's real execute path."
    , "function claimAll(uid, x, y)"
    , "  LIVE[uid] = true"
    , "  DESIG[PAGE][dk(x, y)] = true"
    , "  local s = CORE.ensureState(uid)"
    , "  s.digCandidate = { x = x, y = y, tool = 'shovel' }"
    , "  DIG.digExecute(uid, s, P)"
    , "  s.chopCandidate = { x = x, y = y, iid = iidOf(x, y) }"
    , "  CHOP.chopExecute(uid, s, P)"
    , "  s.tillCandidate = { x = x, y = y }"
    , "  AI.till.execute(uid, s, P)"
    , "  s.plantCandidate = { x = x, y = y }"
    , "  AI.plant.execute(uid, s, P)"
    , "  s.constructCandidate = { x = x, y = y, category = 'building', building = 'hut' }"
    , "  CONSTRUCT.constructExecute(uid, s, P)"
    , "  return s"
    , "end"
    , "-- Can `uid` take a job at (x,y) on the ACTIVE page? One fresh state per"
    , "-- registry so a claim taken by one doesn't mask another's refusal."
    , "function canClaim(uid, x, y)"
    , "  LIVE[uid] = true"
    , "  local out, s = {}, nil"
    , "  s = { digCandidate = { x = x, y = y, tool = 'shovel' } }"
    , "  DIG.digExecute(uid, s, P); out.dig = s.digJob ~= nil"
    , "  s = { chopCandidate = { x = x, y = y, iid = iidOf(x, y) } }"
    , "  CHOP.chopExecute(uid, s, P); out.chop = s.chopJob ~= nil"
    , "  s = { tillCandidate = { x = x, y = y } }"
    , "  AI.till.execute(uid, s, P); out.till = s.tillJob ~= nil"
    , "  s = { plantCandidate = { x = x, y = y } }"
    , "  AI.plant.execute(uid, s, P); out.plant = s.plantJob ~= nil"
    , "  s = { constructCandidate = { x = x, y = y, category = 'building', building = 'hut' } }"
    , "  CONSTRUCT.constructExecute(uid, s, P); out.construct = s.constructJob ~= nil"
    , "  return out"
    , "end"
    , "function allTrue(t) return t.dig and t.chop and t.till and t.plant and t.construct end"
    , "function allFalse(t)"
    , "  return not (t.dig or t.chop or t.till or t.plant or t.construct) end"
    , "function names(t)"
    , "  local o = {}"
    , "  for _, k in ipairs({ 'dig', 'chop', 'till', 'plant', 'construct' }) do"
    , "    o[#o + 1] = k .. '=' .. tostring(t[k]) end"
    , "  return table.concat(o, ' ')"
    , "end"
    , "-- Claim the repair of item instance `iid` for `uid`, through the real"
    , "-- repairExecute claim path (no scan: the candidate is what a scan would"
    , "-- have scored)."
    , "function claimRepair(uid, iid)"
    , "  LIVE[uid] = true"
    , "  local s = CORE.ensureState(uid)"
    , "  s.repairCandidate = { instanceId = iid, defName = 'axe', axis = 'condition',"
    , "                        recipeId = 'r', consumable = 'c', consumableCount = 1 }"
    , "  REPAIR.execute(uid, s, P)"
    , "  return s"
    , "end"
    , "-- One real load: snapshotAll -> prepareLoad -> applyAll."
    , "function runLoad()"
    , "  local snap = SAVE.snapshotAll()"
    , "  assert(snap.ok, 'snapshotAll failed')"
    , "  local prep = SAVE.prepareLoad(snap.components)"
    , "  assert(prep.ok, 'prepareLoad failed')"
    , "  SAVE.applyAll()"
    , "end"
    ]

spec ∷ Spec
spec = describe "unit AI load reset (#1329)" $ do
    it "clears every transient claim registry when a load replaces the\
           \ session, even with the loaded clock earlier than the session\
           \ that wrote them" $
        runsOk $ prelude <> "\n" <> lns
            [ "NOW = 1000"
            , "claimAll(7, 5, 5)"
            , "claimRepair(7, 42)"
            , "AI.setRepairPriority(42, true)"
            , "assert(AI.getRepairClaimant(42) == 7, 'session A should hold the repair claim')"
            , "assert(AI.isRepairPriority(42), 'session A should hold the priority flag')"
            , "assert(allFalse(canClaim(9, 5, 5)),"
            , "       'session A must block a second unit: ' .. names(canClaim(9, 5, 5)))"
            , "runLoad()"
            , "-- Session B reuses uid 7 and instance id 42 for unrelated entities, and"
            , "-- its clock runs EARLIER than A's -- which is exactly what makes the"
            , "-- `now - c.at > timeout` expiry unable to save us."
            , "NOW = 100"
            , "assert(AI.getRepairClaimant(42) == nil,"
            , "       'a repair claim from the replaced session survived the load')"
            , "assert(AI.isRepairPriority(42) == false,"
            , "       'a repair-priority flag from the replaced session survived the load')"
            , "local free = canClaim(9, 5, 5)"
            , "assert(allTrue(free),"
            , "       'a coordinate claim from the replaced session still blocks: '"
            , "       .. names(free))"
            ]
    it "clears them on a load carrying no data at all for this module\
           \ family" $
        runsOk $ prelude <> "\n" <> lns
            [ "NOW = 1000"
            , "claimAll(7, 5, 5)"
            , "claimRepair(7, 42)"
            , "AI.setRepairPriority(42, true)"
            , "-- registerResetHook's whole contract: the hook fires on every load, not"
            , "-- only one whose envelope carried a lua.unit_ai payload. The legacy"
            , "-- baseline path defaults every absent component, so nothing here is a"
            , "-- unit_ai component at all."
            , "local prep = SAVE.prepareLoad({}, nil, true)"
            , "assert(prep.ok, 'legacy-baseline prepareLoad failed: '"
            , "                .. tostring(prep.errors and prep.errors[1]))"
            , "SAVE.applyAll()"
            , "NOW = 100"
            , "assert(AI.getRepairClaimant(42) == nil, 'repair claim survived a data-less load')"
            , "assert(AI.isRepairPriority(42) == false, 'priority survived a data-less load')"
            , "assert(allTrue(canClaim(9, 5, 5)), 'coordinate claims survived a data-less load')"
            ]
    it "leaves the live registries untouched when a load is rejected\
           \ before publication" $
        runsOk $ prelude <> "\n" <> lns
            [ "NOW = 1000"
            , "claimAll(7, 5, 5)"
            , "claimRepair(7, 42)"
            , "AI.setRepairPriority(42, true)"
            , "-- A load rejected at prepare time never publishes, so the old session"
            , "-- stays live and keeps its coordination state (contract: a failed load"
            , "-- leaves the previous session unchanged). applyAll -- which is what runs"
            , "-- the reset hooks -- is never reached."
            , "local prep = SAVE.prepareLoad({ { id = 'unit_ai', version = 99, payload = 'x' } })"
            , "assert(not prep.ok, 'an unsupported component version should be rejected')"
            , "assert(AI.getRepairClaimant(42) == 7, 'a rejected load cleared the repair claim')"
            , "assert(AI.isRepairPriority(42), 'a rejected load cleared the priority flag')"
            , "assert(allFalse(canClaim(9, 5, 5)),"
            , "       'a rejected load cleared the coordinate claims: '"
            , "       .. names(canClaim(9, 5, 5)))"
            ]
    it "empties the claim tables in place and leaves content-data\
           \ caches warm" $
        runsOk $ prelude <> "\n" <> lns
            [ "NOW = 1000"
            , "local tillT, plantT = AI.till.claims, AI.plant.claims"
            , "claimAll(7, 5, 5)"
            , "-- Warm packBuildCache through the real scan: one paid structure job,"
            , "-- whose pack YAML is read exactly once and cached."
            , "JOBS = { { x = 9, y = 9, lx = 9, ly = 9, status = 'pending',"
            , "           category = 'structure', pack = 'p', kind = 'wall', paid = true } }"
            , "LIVE[8] = true"
            , "CONSTRUCT.constructUtility(8, CORE.ensureState(8), P)"
            , "assert(YAML_READS == 1, 'expected one pack-YAML read, got ' .. YAML_READS)"
            , "runLoad()"
            , "-- Requirement 3: closures (and these two public fields) hold the tables"
            , "-- directly, so the reset must EMPTY them, never rebind them."
            , "assert(AI.till.claims == tillT, 'unitAi.till.claims was reassigned by the reset')"
            , "assert(AI.plant.claims == plantT, 'unitAi.plant.claims was reassigned by the reset')"
            , "assert(next(AI.till.claims) == nil, 'unitAi.till.claims was not emptied')"
            , "assert(next(AI.plant.claims) == nil, 'unitAi.plant.claims was not emptied')"
            , "-- Requirement 4: packBuildCache holds YAML-derived facts identical"
            , "-- across sessions, so a load must not evict it."
            , "LIVE[11] = true"
            , "CONSTRUCT.constructUtility(11, CORE.ensureState(11), P)"
            , "assert(YAML_READS == 1,"
            , "       'the load evicted packBuildCache (' .. YAML_READS .. ' YAML reads)')"
            ]
    it "tracks every transient registry the unit-AI family owns" $
        runsOk $ prelude <> "\n" <> lns
            [ "-- Eleven: dig, chop, construct, till and plant coordinate claims,"
            , "-- repairClaims/repairPriority, and four encounter tick overlays."
            , "-- A new registry that forgets"
            , "-- claimsLib.track() would silently survive every load, so pin the count."
            , "assert(CLAIMS.trackedCount() == 11,"
            , "       'expected 11 tracked registries, got ' .. CLAIMS.trackedCount())"
            ]
    it "preserves same-page timeout and dead-claimant release" $
        runsOk $ prelude <> "\n" <> lns
            [ "NOW = 1000"
            , "claimAll(7, 5, 5)"
            , "assert(allFalse(canClaim(9, 5, 5)), 'a fresh claim should block')"
            , "NOW = 1000 + 31"
            , "assert(allTrue(canClaim(9, 5, 5)),"
            , "       'an expired claim should release: ' .. names(canClaim(9, 5, 5)))"
            , "NOW = 2000"
            , "claimAll(21, 6, 6)"
            , "assert(allFalse(canClaim(22, 6, 6)), 'a live claimant should block')"
            , "LIVE[21] = nil"
            , "assert(allTrue(canClaim(22, 6, 6)),"
            , "       'a dead claimant should release: ' .. names(canClaim(22, 6, 6)))"
            ]
    it "keeps the same coordinate on two pages as two distinct claims" $
        runsOk $ prelude <> "\n" <> lns
            [ "NOW = 1000"
            , "PAGE = 'A'"
            , "local sA = claimAll(7, 5, 5)"
            , "sA.constructJob.progress = 0.5"
            , "PAGE = 'B'"
            , "local sB = claimAll(8, 5, 5)"
            , "-- All five registries: page B's (5,5) is free even though page A holds it."
            , "assert(sB.digJob and sB.chopJob and sB.tillJob and sB.plantJob"
            , "       and sB.constructJob,"
            , "       'page B could not claim a coordinate page A holds')"
            , "assert(sA.constructJob ~= nil and sA.constructJob.progress == 0.5,"
            , "       'claiming on page B stole page A unit 7\\'s in-progress construct job')"
            , "-- Cancelling page B's designation must not reach page A's claim."
            , "CONSTRUCT.abandonClaim('B', 5, 5)"
            , "assert(sB.constructJob == nil, 'abandonClaim did not drop page B\\'s job')"
            , "assert(sA.constructJob ~= nil and sA.constructJob.progress == 0.5,"
            , "       'abandonClaim on page B dropped page A\\'s job')"
            , "PAGE = 'A'"
            , "assert(allFalse(canClaim(9, 5, 5)),"
            , "       'page A lost its claim to page B activity: ' .. names(canClaim(9, 5, 5)))"
            , "PAGE = 'B'"
            , "local b = canClaim(9, 5, 5)"
            , "assert(b.construct, 'page B construct should be free after abandonClaim')"
            , "assert(not (b.dig or b.chop or b.till or b.plant),"
            , "       'abandonClaim should touch construct only: ' .. names(b))"
            ]
    -- #1854: chopJob.iid is stripped at snapshot time, so a job that
    -- survives a load names only its TILE. A tile can carry several
    -- designated plants, and re-resolving every restored job to the same
    -- one would have two acolytes fell one tree together, orphan the
    -- other's designation, and silently overwrite the loser's claim.
    it "gives two acolytes whose restored chop jobs share a tile a \
       \DISTINCT designated plant each" $
        runsOk $ prelude <> "\n" <> lns
            [ "NOW = 1000"
            , "DESIG[PAGE][dk(5, 5)] = true"
            , "-- Two wood-tagged co-tenants on the one tile."
            , "DESIG_N[dk(5, 5)] = 2"
            , "LIVE[7], LIVE[8] = true, true"
            , "-- Exactly what a load leaves behind: the tile, no id."
            , "local sA = { chopJob = { x = 5, y = 5 }, chopPhase = 'walking' }"
            , "local sB = { chopJob = { x = 5, y = 5 }, chopPhase = 'walking' }"
            , "CHOP.chopExecute(7, sA, P)"
            , "CHOP.chopExecute(8, sB, P)"
            , "assert(sA.chopJob and sA.chopJob.iid,"
            , "       'unit 7 adopted no plant for its restored chop job')"
            , "assert(sB.chopJob and sB.chopJob.iid,"
            , "       'unit 8 adopted no plant for its restored chop job')"
            , "assert(sA.chopJob.iid ~= sB.chopJob.iid,"
            , "       'both units adopted the same plant: ' .. sA.chopJob.iid)"
            , "-- And each holds the claim on the plant it adopted, so"
            , "-- neither can be preempted by the other next tick."
            , "CHOP.chopExecute(7, sA, P)"
            , "CHOP.chopExecute(8, sB, P)"
            , "assert(sA.chopJob and sA.chopJob.iid,"
            , "       'unit 7 lost its restored chop job to unit 8')"
            , "assert(sB.chopJob and sB.chopJob.iid,"
            , "       'unit 8 lost its restored chop job to unit 7')"
            ]
    it "drops a restored chop job whose tile no longer designates \
       \anything this unit may hold" $
        runsOk $ prelude <> "\n" <> lns
            [ "NOW = 1000"
            , "DESIG[PAGE][dk(5, 5)] = true"
            , "LIVE[7], LIVE[8] = true, true"
            , "-- One designated plant, two restored jobs: the second has"
            , "-- nothing left to adopt and must release rather than steal."
            , "local sA = { chopJob = { x = 5, y = 5 }, chopPhase = 'walking' }"
            , "local sB = { chopJob = { x = 5, y = 5 }, chopPhase = 'walking' }"
            , "CHOP.chopExecute(7, sA, P)"
            , "CHOP.chopExecute(8, sB, P)"
            , "assert(sA.chopJob and sA.chopJob.iid,"
            , "       'unit 7 adopted no plant')"
            , "assert(sB.chopJob == nil,"
            , "       'unit 8 kept a job on a plant unit 7 holds')"
            ]
    it "re-adopts an engine-side claimed construction job after the\
           \ reset" $
        runsOk $ prelude <> "\n" <> lns
            [ "NOW = 1000"
            , "claimAll(7, 5, 5)"
            , "assert(STATUS.A['5,5'] == 'claimed', 'the claim should mark the job claimed')"
            , "runLoad()"
            , "NOW = 100"
            , "-- Requirement 5: the restored session still reports the job as"
            , "-- engine-side \"claimed\" while the reset left no registry entry. The"
            , "-- existing sweep must adopt it with an anonymous timer..."
            , "JOBS = { { x = 5, y = 5, lx = 5, ly = 5, status = 'claimed',"
            , "           category = 'building', building = 'hut' } }"
            , "STATUS.A['5,5'] = 'claimed'"
            , "LIVE[9] = true"
            , "local s = CORE.ensureState(9)"
            , "CONSTRUCT.constructUtility(9, s, P)"
            , "assert(STATUS.A['5,5'] == 'claimed',"
            , "       'the sweep released an orphan immediately instead of adopting it')"
            , "-- ...and, with nobody refreshing it, release it back to pending rather"
            , "-- than orphaning the job forever."
            , "NOW = 100 + 31"
            , "CONSTRUCT.constructUtility(9, s, P)"
            , "assert(STATUS.A['5,5'] == 'pending',"
            , "       'an adopted orphan was never released back to pending')"
            ]
