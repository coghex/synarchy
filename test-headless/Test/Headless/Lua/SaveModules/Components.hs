-- | The REAL @lua.unit_ai@ and @lua.building_spawn@ components, their
--   post-load reconciliation, the version bounds every component is
--   held to, and the tracked v1 fixtures on disk -- one of the four
--   owners 'Test.Headless.Lua.SaveModules' composes (issue #2047).
--
--   Five groups: the unit_ai component itself (issue #761
--   requirements 13/14), its transient runtime defaults (issue
--   #2055), post-load reference reconciliation (issue #1589),
--   component version bounds, and the tracked
--   @test-headless/data/save-compat/*.bin@ payloads read from disk
--   (issue #766, save-overhaul C4).
--
--   Its domain fixture lives here with it: 'unitAiReconcilePrelude'
--   and the settable-clock 'unitAiDefaultsPrelude' built on it.
module Test.Headless.Lua.SaveModules.Components (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS

import Test.Headless.Lua.SaveModules.Support
    (lns, runsOk, runsOkWithPayloads)

-- | Everything the REAL @lua.unit_ai@ component and
--   @scripts/unit_ai_reconcile.lua@ reach outside a live engine, plus
--   the registration and the reconciliation context both #1589 cases
--   share (issue #1589).
--
--   @CTX@ describes a restored two-page session: item instance 900
--   exists session-wide, unit 1 lives on page A and unit 2 on page B,
--   BOTH pages carry their own bill 5, and ground item 7 exists on page
--   A only. @MULE_MOVES@ counts the item hand-backs
--   @unit_ai_repair.lua@'s abort path performs, which is how the
--   repairJob drop proves it went through that path rather than a bare
--   field assignment.
unitAiReconcilePrelude ∷ [Text]
unitAiReconcilePrelude =
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] = {"
    , "  comfort = function() return 1.0 end, ordered = function() return 1.15 end,"
    , "  sprint = function() return 2.0 end, meander = function() return 0.5 end }"
    , "LOG, MULE_MOVES = {}, 0"
    , "engine = { gameTime = function() return 1000 end,"
    , "  logInfo = function(m) LOG[#LOG + 1] = m end,"
    , "  logWarn = function() end, logError = function() end,"
    , "  emitEventForUnit = function() end, loadYaml = function() return nil end }"
    , "unit = { exists = function() return true end,"
    -- #1673: the AI pairs every candidate with the ACTING unit's own
    -- page, so the stub world needs one; everyone shares it here.
    , "  getInfo = function(u)"
    , "    if u == 77 then"
    , "      return { gridX = 0, gridY = 0, defName = 'technomule',"
    , "               page = 'stub_page' } end"
    , "    return { gridX = 0, gridY = 0, defName = 'acolyte',"
    , "             page = 'stub_page' } end,"
    , "  getAllIds = function() return { 77 } end,"
    , "  getStat = function() return 1.0 end, getSkill = function() return 25.0 end,"
    , "  getInventory = function() return {} end,"
    , "  transferItemToUnit = function() MULE_MOVES = MULE_MOVES + 1"
    , "    return true end,"
    , "  moveTo = function() end, stop = function() end, addXP = function() end,"
    , "  setAnimOverride = function() end, clearAnimOverride = function() end }"
    , "world = { getActiveWorldId = function() return 'A' end,"
    , "  getLocationInstance = function() return nil end }"
    , "craft = { get = function(id)"
    , "  if id == 'known_recipe' then return { id = id } end end }"
    , "repair = { get = function(id)"
    , "  if id == 'known_repair' then return { id = id } end end,"
    , "  repairAt = function() return true end }"
    , "item = { listGround = function() return {} end,"
    , "  -- #1666: owning-page ground lookup; nothing on this page."
    , "  getGroundForUnit = function() return nil, true end,"
    , "  listDefs = function() return { { name = 'axe_steel' },"
    , "                                 { name = 'whetstone' } } end }"
    , "building = { findStation = function() return nil end,"
    , "  getInfo = function() return nil end,"
    , "  listDefs = function() return { { name = 'hut' } } end }"
    , "flora = { exists = function() return true end }"
    , "aiState = {}"
    , "require('scripts.unit_ai_save').register(aiState)"
    , "reconcile = require('scripts.unit_ai_reconcile')"
    , "saveModules = require('scripts.lib.save_modules')"
    , "codec = require('scripts.lib.data_codec')"
    , "CTX = { item_instance = { [900] = true },"
    , "        unitPage = { [1] = 'A', [2] = 'B' },"
    , "        byPage = { craft_bill = { A = { [5] = true }, B = { [5] = true } },"
    , "                   ground_item = { A = { [7] = true }, B = {} } } }"
    ]

-- | Issue #2055's shared stubs. The fill it covers happens at the
--   POST-PUBLISH reconcile, so these cases have to drive the real
--   @scripts/unit_ai_reconcile.lua@ too, not just prepareLoad/applyAll
--   — hence 'unitAiReconcilePrelude' as the base rather than a fresh
--   set of stubs.
--
--   What is added on top is a SETTABLE clock. The base prelude pins
--   @engine.gameTime@ to a constant; these cases need to move it,
--   because the whole reason the fill is at reconcile rather than at
--   decode is that staging and the restored session read different
--   values. @NOW@ starts at the base prelude's own 1000 so the cases
--   that do not care about the clock read the same number it always
--   did.
unitAiDefaultsPrelude ∷ [Text]
unitAiDefaultsPrelude = unitAiReconcilePrelude ⧺
    [ "NOW = 1000.0"
    , "engine.gameTime = function() return NOW end"
    -- unit_ai_core requires unit_ai_hold, which requires movement_speed
    -- at module scope; the base prelude already stubs that.
    , "unit.exists = function() return true end"
    , "item.listDefs = function() return {} end"
    , "building.listDefs = function() return { { name = 'hut' } } end"
    ]

spec ∷ Spec
spec = do
    describe "unit_ai save component (issue #761 requirements 13/14)" $ do
        it "strips every transient *Candidate scratch field from the \
           \persisted snapshot -- craftCandidate in particular embeds a \
           \full live RecipeDef (craft.get()'s return value), which must \
           \never be copied into a save payload" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = { [1] = {"
            , "  currentAction = 'idle',"
            , "  craftCandidate = { bill = { id = 5, station = 10 },"
            , "    recipe = { id = 'x', inputs = { a = 1 }, outputs = { b = 2 },"
            , "               station = 'forge' }, demands = {}, dist = 3 },"
            , "  repairCandidate = { instanceId = 42, defName = 'axe' },"
            , "  digCandidate = { x = 3, y = 4 } } }"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local snap = saveModules.registry.unit_ai.snapshot()"
            , "assert(snap[1] ~= nil, 'live unit state must still be present')"
            , "assert(snap[1].currentAction == 'idle', 'non-candidate fields survive')"
            , "assert(snap[1].craftCandidate == nil,"
            , "  'craftCandidate (which embeds a live RecipeDef) must be stripped')"
            , "assert(snap[1].repairCandidate == nil, 'repairCandidate must be stripped')"
            , "assert(snap[1].digCandidate == nil, 'digCandidate must be stripped')"
            , "-- The live singleton itself must be untouched (only the"
            , "-- SNAPSHOT copy is stripped) -- the AI loop still needs its"
            , "-- own in-memory candidate on this same tick."
            , "assert(fakeAiState[1].craftCandidate ~= nil,"
            , "  'stripping must not mutate the live aiState singleton')"
            , "-- The encoded payload itself must not contain the recipe id"
            , "-- as a smuggled string anywhere, proving no leftover copy"
            , "-- survives via some other path."
            , "local codec = require('scripts.lib.data_codec')"
            , "local payload = codec.encode(snap)"
            , "assert(payload:find('forge') == nil,"
            , "  'no trace of the live recipe content may reach the encoded payload')"
            ]

        it "rejects a load whose craftJob/repairJob reference a recipe or \
           \item def no longer registered (issue #761 round-4 review), \
           \during prepareLoad -- before any live state is touched -- and \
           \accepts one whose references all still resolve" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(id)"
            , "  if id == 'known_recipe' then return { id = 'known_recipe' } end"
            , "  return nil end }"
            , "repair = { get = function(id)"
            , "  if id == 'known_repair' then return { id = 'known_repair' } end"
            , "  return nil end }"
            , "item = { listDefs = function()"
            , "  return { { name = 'wood' }, { name = 'stone' } } end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "unitAiSave.register({})"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local function prepareWith(state)"
            , "  return saveModules.prepareLoad({"
            , "    { id = 'unit_ai', version = 1, payload = codec.encode(state) },"
            , "  })"
            , "end"
            , "local removedRecipe = prepareWith({ [1] = { craftJob = {"
            , "  billId = 5, bid = 9, recipeId = 'removed_recipe', need = { wood = 2 } } } })"
            , "assert(not removedRecipe.ok,"
            , "  'a craftJob referencing a removed recipe must reject the load')"
            , "local removedItem = prepareWith({ [1] = { craftJob = {"
            , "  billId = 5, bid = 9, recipeId = 'known_recipe',"
            , "  fromGround = { unobtainium = 3 } } } })"
            , "assert(not removedItem.ok,"
            , "  'a craftJob fetch map referencing a removed item must reject the load')"
            , "local removedRepairRefs = prepareWith({ [1] = { repairJob = {"
            , "  instanceId = 900, recipeId = 'removed_recipe', defName = 'ghost_axe',"
            , "  consumable = 'ghost_wood' } } })"
            , "assert(not removedRepairRefs.ok,"
            , "  'a repairJob referencing removed content defs must reject the load')"
            , "local allPresent = prepareWith({ [1] = {"
            , "  craftJob = { billId = 5, bid = 9, recipeId = 'known_recipe',"
            , "               need = { wood = 2 }, fromGround = { stone = 1 } },"
            , "} })"
            , "assert(allPresent.ok,"
            , "  'a craftJob whose recipe/items all still exist must not be rejected: '"
            , "  .. table.concat(allPresent.errors or {}, '; '))"
            , "local repairPresent = prepareWith({ [2] = {"
            , "  repairJob = { instanceId = 900, recipeId = 'known_repair',"
            , "                defName = 'wood', consumable = 'stone' },"
            , "} })"
            , "assert(repairPresent.ok,"
            , "  'a repairJob whose recipe/items all still exist must not be rejected: '"
            , "  .. table.concat(repairPresent.errors or {}, '; '))"
            ]

        it "rejects a craftJob missing its REQUIRED billId/bid, and a \
           \repairJob missing its REQUIRED instanceId (round-6 review, \
           \issue #764) -- craftJob.billId/bid and repairJob.instanceId \
           \are unconditionally set the instant their job is created \
           \(unit_ai_craft.lua/unit_ai_repair.lua), so a v2/v3 payload \
           \whose job table is present but missing one is structurally \
           \malformed, not a legitimate earlier job phase -- unlike a \
           \dangling id (a real id whose TARGET later vanished), which \
           \stays a tolerated, non-blocking diagnostic elsewhere" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return { id = _id } end }"
            , "repair = { get = function(_id) return { id = _id } end }"
            , "item = { listDefs = function() return { { name = 'wood' } } end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local function prepareWith(state)"
            , "  return saveModules.prepareLoad({"
            , "    { id = 'unit_ai', version = 2, payload = codec.encode(state) },"
            , "  })"
            , "end"
            , "local noBillId = prepareWith({ [1] = { craftJob = {"
            , "  bid = { __ref = 'building', id = 9 }, recipeId = 'x' } } })"
            , "assert(not noBillId.ok,"
            , "  'a craftJob with no billId at all must reject the load')"
            , "local noBid = prepareWith({ [1] = { craftJob = {"
            , "  billId = { __ref = 'craft_bill', id = 5 }, recipeId = 'x' } } })"
            , "assert(not noBid.ok,"
            , "  'a craftJob with no bid (station) at all must reject the load')"
            , "local noInstanceId = prepareWith({ [1] = { repairJob = {"
            , "  recipeId = 'x', defName = 'wood' } } })"
            , "assert(not noInstanceId.ok,"
            , "  'a repairJob with no instanceId at all must reject the load')"
            , "-- repairJob.bid is deliberately OPTIONAL. unit_ai_repair.lua"
            , "-- DOES set it -- but only once the job reaches its walking"
            , "-- phase and building.findStation resolves a station -- so a"
            , "-- job saved in an earlier phase legitimately carries none,"
            , "-- and requiring it would reject a real repair job."
            , "local repairNoBid = prepareWith({ [2] = { repairJob = {"
            , "  instanceId = { __ref = 'item_instance', id = 900 },"
            , "  recipeId = 'x', defName = 'wood' } } })"
            , "assert(repairNoBid.ok,"
            , "  'repairJob.bid must stay optional (it is only set once the '"
            , "  .. 'job reaches the walking phase in unit_ai_repair.lua): '"
            , "  .. table.concat(repairNoBid.errors or {}, '; '))"
            ]

        it "extends the same missing-content-reference rejection to \
           \constructJob/deliveryClaim/deliveryPendingTarget/plantJob \
           \(issue #761 round-5 review), and strips constructJob's live \
           \structure-pack build table from the snapshot without \
           \mutating the live job" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "item = { listDefs = function()"
            , "  return { { name = 'wood' }, { name = 'stone' } } end }"
            , "building = { listDefs = function()"
            , "  return { { name = 'workbench' } } end }"
            , "flora = { exists = function(name) return name == 'wheat' end }"
            , "engine.loadYaml = function(path)"
            , "  if path == 'data/structure_packs/known_pack.yaml' then"
            , "    return { build = { wall = { materials = { wood = 2 },"
            , "                                build_work = 3 } } }"
            , "  end"
            , "  return nil"
            , "end"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "-- Register BEFORE any prepareLoad call -- prepareLoad only"
            , "-- validates components already present in the registry, so"
            , "-- registering after would leave every prepareWith() call"
            , "-- below validating against an empty registry and passing"
            , "-- vacuously."
            , "local liveBuild = { materials = { wood = 2 }, build_work = 3 }"
            , "local fakeAiState = { [1] = { constructJob = {"
            , "  category = 'structure', pack = 'known_pack', kind = 'wall',"
            , "  build = liveBuild, need = { wood = 2 }, staking = 12.5 } } }"
            , "unitAiSave.register(fakeAiState)"
            , "local function prepareWith(state)"
            , "  return saveModules.prepareLoad({"
            , "    { id = 'unit_ai', version = 1, payload = codec.encode(state) },"
            , "  })"
            , "end"
            -- #1844 requirement 20: a structurally valid structure job
            -- whose pack or kind no longer resolves must REACH load
            -- staging, where the engine self-clears the matching
            -- designation and refunds its persisted receipt exactly
            -- once. Rejecting here would abort a whole load for a
            -- situation that is now resolved losslessly, which is why
            -- the two assertions below are the reverse of what they
            -- were. The narrow rejections either side of them are
            -- deliberately unchanged.
            , "local badPack = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'ghost_pack', kind = 'wall',"
            , "  need = {} } } })"
            , "assert(badPack.ok,"
            , "  'a removed structure pack must reach load reconciliation: '"
            , "  .. table.concat(badPack.errors or {}, '; '))"
            , "local badKind = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'known_pack', kind = 'ghost_kind',"
            , "  need = {} } } })"
            , "assert(badKind.ok,"
            , "  'a removed pack kind must reach load reconciliation: '"
            , "  .. table.concat(badKind.errors or {}, '; '))"
            , "local badConstructItem = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'known_pack', kind = 'wall',"
            , "  need = {}, fromGround = { unobtainium = 1 } } } })"
            , "assert(not badConstructItem.ok,"
            , "  'a constructJob fetch map referencing a removed item must reject the load')"
            , "local goodConstruct = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'known_pack', kind = 'wall',"
            , "  need = { wood = 2 }, fromGround = { stone = 1 } } } })"
            , "assert(goodConstruct.ok,"
            , "  'a constructJob whose pack/kind/items all still exist must not be rejected: '"
            , "  .. table.concat(goodConstruct.errors or {}, '; '))"
            , "local buildingConstruct = prepareWith({ [1] = { constructJob = {"
            , "  category = 'building', building = 'workbench', x = 1, y = 1 } } })"
            , "assert(buildingConstruct.ok,"
            , "  'a known building-category constructJob must not be rejected: '"
            , "  .. table.concat(buildingConstruct.errors or {}, '; '))"
            , "local badBuildingConstruct = prepareWith({ [1] = { constructJob = {"
            , "  category = 'building', building = 'ghost_building', x = 1, y = 1 } } })"
            , "assert(not badBuildingConstruct.ok,"
            , "  'a constructJob referencing a removed building def must reject the load')"
            , "local badDeliveryClaim = prepareWith({ [1] = { deliveryClaim = {"
            , "  bid = 1, materials = { unobtainium = 1 } } } })"
            , "assert(not badDeliveryClaim.ok,"
            , "  'a deliveryClaim referencing a removed material must reject the load')"
            , "local badDeliveryTarget = prepareWith({ [1] = { deliveryPendingTarget = {"
            , "  bid = 1, claim = { unobtainium = 1 } } } })"
            , "assert(not badDeliveryTarget.ok,"
            , "  'a deliveryPendingTarget referencing a removed material must reject the load')"
            , "local goodDelivery = prepareWith({ [1] = { deliveryClaim = {"
            , "  bid = 1, materials = { wood = 1 }, fromGround = { stone = 1 } } } })"
            , "assert(goodDelivery.ok,"
            , "  'a deliveryClaim whose materials all still exist must not be rejected: '"
            , "  .. table.concat(goodDelivery.errors or {}, '; '))"
            , "local badPlant = prepareWith({ [1] = { plantJob = {"
            , "  x = 1, y = 1, crop = 'ghost_crop' } } })"
            , "assert(not badPlant.ok,"
            , "  'a plantJob referencing a removed crop species must reject the load')"
            , "local goodPlant = prepareWith({ [1] = { plantJob = {"
            , "  x = 1, y = 1, crop = 'wheat' } } })"
            , "assert(goodPlant.ok,"
            , "  'a plantJob whose crop still exists must not be rejected: '"
            , "  .. table.concat(goodPlant.errors or {}, '; '))"
            , "-- The .build sub-field itself must never reach the encoded"
            , "-- payload (requirement 14), and stripping it must not mutate"
            , "-- the live aiState singleton's own job table."
            , "local snap = saveModules.registry.unit_ai.snapshot()"
            , "assert(snap[1].constructJob.build == nil,"
            , "  'constructJob.build must be stripped from the snapshot')"
            , "assert(snap[1].constructJob.pack == 'known_pack',"
            , "  'sibling constructJob fields must survive the strip')"
            , "assert(fakeAiState[1].constructJob.build == liveBuild,"
            , "  'stripping must not mutate the live constructJob table')"
            , "local payload = codec.encode(snap)"
            , "assert(payload:find('build_work') == nil,"
            , "  'no trace of the live build-cost content may reach the encoded payload')"
            -- #1845: the building-stake hand-off's clock is stripped by
            -- the SAME shallow copy, and for a reason the reference
            -- schema makes non-negotiable — a wait that outlives its
            -- session would be resumed against a building queue the
            -- load discarded. Its absence is what lets the resumed job
            -- re-derive the answer from the world instead of carrying
            -- an unreconcilable BuildingId.
            , "assert(snap[1].constructJob.staking == nil,"
            , "  'constructJob.staking must be stripped from the snapshot')"
            , "assert(fakeAiState[1].constructJob.staking == 12.5,"
            , "  'stripping staking must not mutate the live constructJob table')"
            , "assert(payload:find('staking') == nil,"
            , "  'no trace of the stake hand-off clock may reach the encoded payload')"
            -- …while the spawned id BESIDE it survives: it is a DECLARED
            -- building reference (unit_ai_ref_schema.lua), and it is the
            -- only thing that tells a resumed job whether its OWN stake
            -- landed rather than whether something that merely looks like
            -- it is standing at the tile.
            , "fakeAiState[1].constructJob.stakedBid = 42"
            , "local snapRef = saveModules.registry.unit_ai.snapshot()"
            , "local st = snapRef[1].constructJob.stakedBid"
            , "assert(type(st) == 'table' and st.__ref == 'building'"
            , "       and st.id == 42,"
            , "  'the staked id survives as a TYPED building reference')"
            , "fakeAiState[1].constructJob.stakedBid = nil"
            -- …and a job carrying ONLY the stake clock (no .build, which
            -- a building-category job never has) must still be stripped:
            -- the two are independent reasons on one shallow copy, and a
            -- guard that fired on .build alone would carry the clock
            -- straight through for exactly the jobs that own one.
            , "fakeAiState[1].constructJob = { category = 'building',"
            , "  building = 'workbench', x = 1, y = 1, staking = 4.0 }"
            , "local snap2 = saveModules.registry.unit_ai.snapshot()"
            , "assert(snap2[1].constructJob.staking == nil,"
            , "  'a building job with no .build must still lose its staking clock')"
            , "assert(snap2[1].constructJob.building == 'workbench',"
            , "  'sibling fields must survive that strip too')"
            , "assert(fakeAiState[1].constructJob.staking == 4.0,"
            , "  'that strip must not mutate the live job either')"
            ]

        it "includes the OUTER per-unit key itself as a unit reference \
           \(issue #761 round-6 review), mirroring building_spawn.lua's \
           \own references() including its per-building key -- not just \
           \the ids nested inside claim/job fields" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "unitAiSave.register({})"
            , "local refs = saveModules.registry.unit_ai.references("
            , "  { [42] = { currentAction = 'idle' } })"
            , "local found = false"
            , "for _, r in ipairs(refs) do"
            , "  if r.kind == 'unit' and r.id == 42 then found = true end"
            , "end"
            , "assert(found, 'the outer unit id itself must be a declared reference')"
            ]

        it "types every persisted reference field on the wire (issue #764, \
           \save-overhaul C3 requirement 13): a v1 payload with BARE-NUMBER \
           \reference fields migrates to the typed {__ref=,id=} shape, \
           \references() reads it correctly, and apply() unwraps it back \
           \to a bare number in the LIVE aiState (every other module \
           \still sees plain numbers)" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(id)"
            , "  if id == 'x' then return { id = 'x' } end return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "-- A v1 payload: every reference field is a BARE NUMBER,"
            , "-- exactly as #761 originally shipped it."
            , "local v1 = { [7] = {"
            , "  attackTargetUid = 8, buildTarget = 20,"
            , "  craftJob = { billId = 3, bid = 21, recipeId = 'x' },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 1, payload = codec.encode(v1) },"
            , "})"
            , "assert(prep.ok, 'v1 payload must migrate cleanly: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "local found = {}"
            , "for _, r in ipairs(prep.references) do"
            , "  found[r.kind .. ':' .. tostring(r.id)] = r.owner"
            , "end"
            , "assert(found['unit:8'] == 7,"
            , "  'attackTargetUid must resolve through the wrapped v1->v2 shape')"
            , "assert(found['building:20'] == 7,"
            , "  'buildTarget must resolve through the wrapped v1->v2 shape')"
            , "assert(found['craft_bill:3'] == 7,"
            , "  'craftJob.billId must resolve through the wrapped v1->v2 shape')"
            , "assert(found['building:21'] == 7,"
            , "  'craftJob.bid must resolve through the wrapped v1->v2 shape')"
            , "saveModules.applyAll()"
            , "assert(fakeAiState[7].attackTargetUid == 8,"
            , "  'apply() must unwrap attackTargetUid back to a bare number in LIVE aiState')"
            , "assert(type(fakeAiState[7].attackTargetUid) == 'number',"
            , "  'LIVE aiState must never hold a wrapped table -- every OTHER '"
            , "  .. 'module (unit_ai_combat.lua etc.) reads a bare number')"
            , "assert(fakeAiState[7].craftJob.billId == 3,"
            , "  'apply() must unwrap nested craftJob.billId too')"
            , "-- Round-trip through the engine's OWN encoder: snapshot() on"
            , "-- this now-live (unwrapped) state must re-wrap it as v2 --"
            , "-- the wire format is typed even for freshly-written saves,"
            , "-- not merely a migration-only artifact."
            , "local snap = saveModules.registry.unit_ai.snapshot()"
            , "assert(type(snap[7].attackTargetUid) == 'table'"
            , "  and snap[7].attackTargetUid.__ref == 'unit'"
            , "  and snap[7].attackTargetUid.id == 8,"
            , "  'snapshot() must write the TYPED structured-reference shape, '"
            , "  .. 'not a bare number, for a fresh v2 save')"
            , "-- Round-6 review: the OUTER per-unit key (7) is ALSO typed,"
            , "-- via a self-describing __owner field on the row."
            , "assert(type(snap[7].__owner) == 'table'"
            , "  and snap[7].__owner.__ref == 'unit' and snap[7].__owner.id == 7,"
            , "  'snapshot() must write a __owner field typing the outer '"
            , "  .. 'per-unit key too')"
            , "assert(fakeAiState[7].__owner == nil,"
            , "  '__owner must never leak into the LIVE aiState apply() writes back')"
            ]

        it "migrates a v2 unit_ai payload (every reference field wrapped, \
           \but no __owner yet) to v3 by adding ONLY __owner, without \
           \re-wrapping fields that are already wrapped (round-6 review, \
           \issue #764)" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local v2 = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 8 },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(v2) },"
            , "})"
            , "assert(prep.ok, 'a v2 payload must migrate to v3 cleanly: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "assert(fakeAiState[7].attackTargetUid == 8,"
            , "  'a v2-shaped attackTargetUid must still unwrap correctly after '"
            , "  .. 'the v2->v3 __owner-only migration')"
            ]

        it "rejects a v3 unit_ai payload with NO __owner at all, and one \
           \whose __owner id does not match its own outer key (round-6 \
           \review, issue #764) -- __owner is REQUIRED on every entry, \
           \unlike lastUid/attackTargetUid/etc., which are legitimately \
           \absent" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local noOwner = { [7] = {} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 3, payload = codec.encode(noOwner) },"
            , "})"
            , "assert(not prep.ok, 'a v3 entry missing __owner entirely must reject the load')"
            , "local mismatched = { [7] = { __owner = { __ref = 'unit', id = 8 } } }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 3, payload = codec.encode(mismatched) },"
            , "})"
            , "assert(not prep2.ok,"
            , "  \"a __owner id that doesn't match its own outer key must reject the load\")"
            , "local matched = { [7] = { __owner = { __ref = 'unit', id = 7 } } }"
            , "local prep3 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 3, payload = codec.encode(matched) },"
            , "})"
            , "assert(prep3.ok, 'a correctly-matched __owner must load cleanly: '"
            , "  .. table.concat(prep3.errors or {}, '; '))"
            ]

        it "types building_spawn's OUTER per-building key via __owner too \
           \(round-6 review, issue #764) -- migrates a v1 payload to v3 \
           \(synthesizing __owner even though NO lastUid was ever set), \
           \migrates a v2 payload by adding only __owner, and rejects a \
           \v3 payload with a missing or mismatched __owner" $
            runsOk $ lns
            [ "building = { getInfo = function(_bid) return { id = _bid } end }"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "buildingSpawn.init('test')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "-- v1: no lastUid at all (a building that hasn't spawned yet)."
            , "local v1 = { [12] = { lastSpawnedAt = 1.0 } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 1, payload = codec.encode(v1) },"
            , "})"
            , "assert(prep.ok, 'a v1 payload with no lastUid must still migrate '"
            , "  .. 'cleanly and gain __owner: ' .. table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "local snap = saveModules.registry.building_spawn.snapshot()"
            , "assert(type(snap[12].__owner) == 'table'"
            , "  and snap[12].__owner.__ref == 'building' and snap[12].__owner.id == 12,"
            , "  'a fresh snapshot() must carry __owner even for a building with no lastUid')"
            , "-- v2: lastUid already wrapped, no __owner yet."
            , "local v2 = { [12] = { lastUid = { __ref = 'unit', id = 4 } } }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 2, payload = codec.encode(v2) },"
            , "})"
            , "assert(prep2.ok, 'a v2 payload must migrate to v3 cleanly: '"
            , "  .. table.concat(prep2.errors or {}, '; '))"
            , "-- v3: missing __owner entirely must reject."
            , "local noOwner = { [12] = {} }"
            , "local prep3 = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 3, payload = codec.encode(noOwner) },"
            , "})"
            , "assert(not prep3.ok, 'a v3 entry missing __owner entirely must reject the load')"
            , "-- v3: mismatched __owner id must reject."
            , "local mismatched = { [12] = { __owner = { __ref = 'building', id = 13 } } }"
            , "local prep4 = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 3, payload = codec.encode(mismatched) },"
            , "})"
            , "assert(not prep4.ok,"
            , "  \"a __owner id that doesn't match its own outer key must reject the load\")"
            ]

        it "rejects a v2 payload whose wrapped reference carries the WRONG \
           \__ref kind for its field (round-2 review, issue #764) -- \
           \unwrapUnitState used to trust field position alone and would \
           \have silently applied a building id as if it were a unit id" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "-- attackTargetUid must be __ref='unit' -- this payload"
            , "-- tags it 'building' instead, same numeric id."
            , "local badKind = { [7] = {"
            , "  attackTargetUid = { __ref = 'building', id = 8 },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(badKind) },"
            , "})"
            , "assert(not prep.ok,"
            , "  'a wrong-kind wrapper on attackTargetUid must reject the load')"
            , "-- Untagged (no __ref at all) must also be rejected -- not"
            , "-- silently treated as a bare-number v1-shaped field, since"
            , "-- this component's declared version is 2."
            , "local untagged = { [7] = { attackTargetUid = { id = 8 } } }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(untagged) },"
            , "})"
            , "assert(not prep2.ok,"
            , "  'an untagged wrapper on attackTargetUid must reject the load')"
            , "-- A correctly-tagged payload must still succeed -- this is a"
            , "-- kind check, not a blanket rejection of every wrapped value."
            , "local goodKind = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 8 },"
            , "} }"
            , "local prep3 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(goodKind) },"
            , "})"
            , "assert(prep3.ok, 'a correctly-tagged wrapper must still load: '"
            , "  .. table.concat(prep3.errors or {}, '; '))"
            ]

        it "rejects a v2 payload whose wrapped reference has the RIGHT \
           \__ref kind but a non-numeric or invalid id (round-3 review, \
           \issue #764) -- a tag-only check would still accept \
           \{__ref='unit', id='bad'}, which would unwrap into live \
           \aiState and be silently dropped by every diagnostic that \
           \Lua.tointeger()s the id instead of being reported" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local badId = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 'bad' },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(badId) },"
            , "})"
            , "assert(not prep.ok,"
            , "  'a non-numeric id on a correctly-tagged wrapper must reject the load')"
            , "-- Zero / negative / fractional ids are equally invalid --"
            , "-- the same positive-integer contract every other id in"
            , "-- this codebase enforces."
            , "local zeroId = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 0 },"
            , "} }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(zeroId) },"
            , "})"
            , "assert(not prep2.ok, 'a zero id must reject the load')"
            , "local fracId = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 8.5 },"
            , "} }"
            , "local prep3 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(fracId) },"
            , "})"
            , "assert(not prep3.ok, 'a fractional id must reject the load')"
            ]

        it "accepts a ground_item reference id of 0 (round-4 review, issue \
           \#764) -- Item.Ground's ground-item allocator is ZERO-based \
           \(emptyGroundItems starts gisNextId at 0), unlike unit/building/ \
           \craft_bill/item_instance's allocators, which all start at 1; a \
           \blanket 'id >= 1' minimum incorrectly rejected the very first \
           \ground item a save could ever legitimately reference" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local zeroGid = { [7] = {"
            , "  pickupOrder = { gid = { __ref = 'ground_item', id = 0 } },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(zeroGid) },"
            , "})"
            , "assert(prep.ok, 'a ground_item id of 0 must be accepted: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "-- A negative ground_item id is still invalid -- the fix"
            , "-- widens the floor to 0, it doesn't remove it."
            , "local negGid = { [7] = {"
            , "  pickupOrder = { gid = { __ref = 'ground_item', id = -1 } },"
            , "} }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(negGid) },"
            , "})"
            , "assert(not prep2.ok, 'a negative ground_item id must still reject the load')"
            ]

        it "rejects a v2 building_spawn payload whose lastUid has the \
           \RIGHT __ref kind but a non-numeric id (round-3 review, \
           \issue #764) -- mirrors the unit_ai id-type check" $
            runsOk $ lns
            [ "building = { getInfo = function(_bid) return { id = _bid } end }"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "buildingSpawn.init('test')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local badId = { [12] = { lastUid = { __ref = 'unit', id = 'bad' } } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 2, payload = codec.encode(badId) },"
            , "})"
            , "assert(not prep.ok,"
            , "  'a non-numeric id on lastUid must reject the load')"
            ]

        it "rejects a v2 building_spawn payload whose lastUid carries the \
           \WRONG __ref kind (round-2 review, issue #764) -- mirrors the \
           \unit_ai wrapper-tag check for building_spawn's own sole \
           \reference field" $
            runsOk $ lns
            [ "building = { getInfo = function(_bid) return { id = _bid } end }"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "buildingSpawn.init('test')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local badKind = { [12] = { lastUid = { __ref = 'building', id = 8 } } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 2, payload = codec.encode(badKind) },"
            , "})"
            , "assert(not prep.ok,"
            , "  'a wrong-kind wrapper on lastUid must reject the load')"
            , "local goodKind = { [12] = { lastUid = { __ref = 'unit', id = 8 } } }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 2, payload = codec.encode(goodKind) },"
            , "})"
            , "assert(prep2.ok, 'a correctly-tagged lastUid must still load: '"
            , "  .. table.concat(prep2.errors or {}, '; '))"
            ]

        it "declares real Haskell-owned dependencies on the ACTUAL \
           \unit_ai and building_spawn registrations (issue #761 \
           \round-8 review) -- not just a synthetic component in the \
           \registry-mechanism tests above, since a mechanism nobody's \
           \real registration exercises doesn't satisfy requirement 2" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "unitAiSave.register({})"
            , "buildingSpawn.init('test')"
            , "local function hasDep(regId, dep)"
            , "  for _, d in ipairs(saveModules.registry[regId].deps) do"
            , "    if d == dep then return true end"
            , "  end"
            , "  return false"
            , "end"
            , "assert(hasDep('unit_ai', 'units'),"
            , "  'unit_ai must declare a real dependency on units')"
            , "assert(hasDep('unit_ai', 'buildings'),"
            , "  'unit_ai must declare a real dependency on buildings')"
            , "assert(hasDep('building_spawn', 'buildings'),"
            , "  'building_spawn must declare a real dependency on buildings')"
            , "assert(hasDep('building_spawn', 'units'),"
            , "  'building_spawn must declare a real dependency on units')"
            ]

        it "types building_spawn's lastUid reference field on the wire too \
           \(issue #764, save-overhaul C3 requirement 13): a v1 payload \
           \with a BARE-NUMBER lastUid migrates to the typed shape, \
           \references() reads it, apply() unwraps it back to a bare \
           \number, and a fresh snapshot() re-wraps it as v2" $ runsOk $ lns
            [ "building = { getInfo = function(_bid) return { id = _bid } end }"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "buildingSpawn.init('test')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local v1 = { [9] = { lastUid = 4, lastSpawnedAt = 1.0 } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 1, payload = codec.encode(v1) },"
            , "})"
            , "assert(prep.ok, 'v1 payload must migrate cleanly: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "local found = false"
            , "for _, r in ipairs(prep.references) do"
            , "  if r.kind == 'unit' and r.id == 4 then found = true end"
            , "end"
            , "assert(found, 'lastUid must resolve through the wrapped v1->v2 shape')"
            , "saveModules.applyAll()"
            , "local snap = saveModules.registry.building_spawn.snapshot()"
            , "assert(type(snap[9].lastUid) == 'table'"
            , "  and snap[9].lastUid.__ref == 'unit' and snap[9].lastUid.id == 4,"
            , "  'a fresh snapshot() must write the TYPED structured-reference '"
            , "  .. 'shape -- if apply() had left lastUid wrapped in LIVE state '"
            , "  .. 'this would double-wrap or crash instead')"
            , "local errs = saveModules.registryStaticErrors()"
            , "assert(#errs == 0, 'the real registrations must resolve their "
              <> "own deps cleanly: ' .. table.concat(errs, '; '))"
            ]


    -- Issue #2055. A row restored from an accepted schema version need
    -- not carry the transient runtime fields the thought tick reads
    -- before it has decided anything: this component's validator
    -- accepts a free-form state row on purpose, and applyEntityRows
    -- installs each decoded row verbatim. Such a row survived decode,
    -- canonical comparison, resave, restart and reload and then errored
    -- on its first live tick.
    --
    -- The fill happens at the POST-PUBLISH reconcile, and these cases
    -- pin all three reasons it has to be there rather than at decode()
    -- or apply(): the restored clock is live by then, a rolled-back
    -- load never reaches it, and nothing has ticked yet. It is also one
    -- stage rather than a back-fill per migration branch, which is what
    -- lets the version matrix below be a loop.
    describe "unit_ai transient runtime defaults (issue #2055)" $ do
        it "supplies every declared runtime default a restored row \
           \omits, for EVERY accepted inputVersion -- one stage that \
           \every version's decode branch has already converged on by \
           \reconcile time, so a payload from any accepted version \
           \comes out tickable" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "local defaults = require('scripts.unit_ai_defaults')"
            , "-- The accepted set comes off the registration itself, so a"
            , "-- version added there is covered by this loop the moment it"
            , "-- exists. The exact-set assertion below is the deliberate"
            , "-- tripwire beside that: a new version also needs its WIRE"
            , "-- shape taught to payloadFor, which no derived loop can"
            , "-- infer, so adding one must be a conscious act here."
            , "local accepted = saveModules.registry.unit_ai.inputVersions"
            , "assert(table.concat(accepted, ',') == '1,2,3,4,5,6,7,8,9',"
            , "  'expected inputVersions {1..9} (1-8 legacy, 9 current), got {'"
            , "  .. table.concat(accepted, ',') .. '}')"
            , "-- The tracked b3-lua-versioned-session-v1 fixture's own v1"
            , "-- row, verbatim: sparse, one reference field, none of the"
            , "-- runtime fields."
            , "local function sparseRow() return { buildTarget = 1 } end"
            , "-- Each version's WIRE shape, built with the component's own"
            , "-- helpers rather than hand-rolled: v1 is bare, v2 is wrapped"
            , "-- without __owner, v3+ carries __owner too. #1844's v8 is a"
            , "-- SEMANTIC bump on v7's layout (a constructJob gained the"
            , "-- attempt it claimed) and #1845's v9 another on v8's (that"
            , "-- job gained the building it staked, as a typed reference),"
            , "-- and a sparse row carries no constructJob at all, so all"
            , "-- three share one wire shape here."
            , "local function payloadFor(version)"
            , "  local rows = { [1] = sparseRow() }"
            , "  if version == 1 then return codec.encode(rows) end"
            , "  local wrapped = refs.wrapAiState(rows)"
            , "  if version == 2 then wrapped[1].__owner = nil end"
            , "  return codec.encode(wrapped)"
            , "end"
            , "for _, version in ipairs(accepted) do"
            , "  for k in pairs(aiState) do aiState[k] = nil end"
            , "  local prep = saveModules.prepareLoad({"
            , "    { id = 'unit_ai', version = version,"
            , "      payload = payloadFor(version) },"
            , "  }, 1, false, { unit = { [1] = true }, building = {} })"
            , "  assert(prep.ok, 'a sparse v' .. version .. ' payload must "
              <> "still be accepted: ' .. table.concat(prep.errors or {}, '; '))"
            , "  saveModules.applyAll()"
            , "  assert(aiState[1] ~= nil, 'v' .. version .. ': the row must apply')"
            , "  assert(aiState[1].buildTarget == 1, 'v' .. version .. ': the "
              <> "row\\'s own field must survive unwrapped')"
            , "  -- STAGING must not have filled anything: gameTimeRef is"
            , "  -- still the outgoing session's until publish."
            , "  for _, f in ipairs(defaults.FIELDS) do"
            , "    assert(aiState[1][f.name] == nil, 'v' .. version .. ': "
              <> "decode/apply run before publish and must fill nothing')"
            , "  end"
            , "  -- Building 1 survives, so the row\'s own buildTarget is"
            , "  -- not a dangling reference the scrub would clear -- this"
            , "  -- case is about the runtime defaults, not the scrub."
            , "  reconcile.reconcile(aiState, { 1 }, { 1 }, CTX)"
            , "  for _, f in ipairs(defaults.FIELDS) do"
            , "    assert(aiState[1][f.name] ~= nil, 'v' .. version .. ': the "
              <> "reconciled row must carry a ' .. f.name .. ' default')"
            , "  end"
            , "  assert(aiState[1].currentAction == 'idle', 'v' .. version"
            , "    .. ': the fresh-row currentAction')"
            , "  assert(aiState[1].nextActionAt == 0, 'v' .. version"
            , "    .. ': 0 means decide on first sight, not wait out an "
              <> "interval nobody scheduled')"
            , "  assert(aiState[1].actionStartedAt == NOW, 'v' .. version"
            , "    .. ': actionStartedAt is the RESTORED clock')"
            , "  assert(aiState[1].commandedTask == nil, 'v' .. version"
            , "    .. ': nil IS commandedTask\\'s value -- defaulting it "
              <> "would invent an order nobody issued')"
            , "end"
            ]

        it "stamps actionStartedAt from the RESTORED session's clock, \
           \not the outgoing one: decode and apply run during staging, \
           \before World.Load.Publish swaps gameTimeRef, so a partially \
           \sparse wander row filled there would have wanderUtility \
           \subtract a foreign timestamp and abandon a wander on time it \
           \never spent" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "-- The OUTGOING session's clock, live for the whole of"
            , "-- staging. In a fresh process this is 0; here it is a"
            , "-- deliberately WRONG-and-obvious value instead, so a stamp"
            , "-- taken from it is unmistakable."
            , "NOW = 5000.0"
            , "-- Only actionStartedAt is missing. currentAction says the"
            , "-- unit was wandering, which is exactly the row"
            , "-- unit_ai_needs.lua's wanderUtility does arithmetic for:"
            , "--   timeInSession = engine.gameTime() - s.actionStartedAt"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 7,"
            , "    payload = codec.encode(refs.wrapAiState("
            , "      { [1] = { currentAction = 'wander', nextActionAt = 0 } })) },"
            , "}, 1, false, { unit = { [1] = true }, building = {} })"
            , "assert(prep.ok, table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "assert(aiState[1].actionStartedAt == nil,"
            , "  'staging must not stamp a clock the restored session does "
              <> "not own yet')"
            , "-- Publish swaps gameTimeRef to the save's own game time."
            , "NOW = 42.0"
            , "reconcile.reconcile(aiState, { 1 }, {}, CTX)"
            , "assert(aiState[1].actionStartedAt == 42.0,"
            , "  'the stamp must be the RESTORED clock, got '"
            , "  .. tostring(aiState[1].actionStartedAt))"
            , "-- The consequence, stated as wanderUtility computes it: a"
            , "-- row with no recorded start has spent NO time in this"
            , "-- session's wander, which is the same answer a freshly"
            , "-- seen unit gets. A staging-time stamp would have made"
            , "-- this 42 - 5000 = -4958."
            , "local timeInSession = engine.gameTime() - aiState[1].actionStartedAt"
            , "assert(timeInSession == 0,"
            , "  'a restored wander must start from zero elapsed, got '"
            , "  .. tostring(timeInSession))"
            , "-- And the fields the row DID carry are still its own."
            , "assert(aiState[1].currentAction == 'wander')"
            , "assert(aiState[1].nextActionAt == 0)"
            ]

        it "fills ONLY what a restored row is missing: every value the \
           \payload actually carries survives, including a nextActionAt \
           \in the past and a currentAction the action list no longer \
           \knows -- a save's own scheduling is the save's to state" $
            runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "-- Unit 1 is complete, unit 2 has exactly one of the three."
            , "local rows = {"
            , "  [1] = { currentAction = 'retired_action',"
            , "          actionStartedAt = 1.5, nextActionAt = 2.5 },"
            , "  [2] = { nextActionAt = 7.5 },"
            , "}"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 7,"
            , "    payload = codec.encode(refs.wrapAiState(rows)) },"
            , "}, 1, false, { unit = { [1] = true, [2] = true }, building = {} })"
            , "assert(prep.ok, table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "reconcile.reconcile(aiState, { 1, 2 }, {}, CTX)"
            , "assert(aiState[1].currentAction == 'retired_action',"
            , "  'a restored currentAction must never be reset to idle')"
            , "assert(aiState[1].actionStartedAt == 1.5,"
            , "  'a restored actionStartedAt must never be re-clocked')"
            , "assert(aiState[1].nextActionAt == 2.5,"
            , "  'a restored nextActionAt must never be reset to 0')"
            , "assert(aiState[2].nextActionAt == 7.5,"
            , "  'a partially sparse row keeps the value it does carry')"
            , "assert(aiState[2].currentAction == 'idle' and"
            , "       aiState[2].actionStartedAt == NOW,"
            , "  'and gains only the ones it does not')"
            ]

        it "leaves applyEntityRows' generic semantics untouched: an \
           \absent-owner row is still dropped with its one diagnostic \
           \and is never normalized into existence, and the published \
           \aiState is the SAME table object consumers already hold \
           \(#900)" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "local warnings = {}"
            , "engine.logWarn = function(msg)"
            , "  warnings[#warnings + 1] = tostring(msg) end"
            , "-- What a CONSUMER holds: the reference every other unit-AI"
            , "-- module took when the singleton was created, captured"
            , "-- BEFORE the load. If the restore rebound aiState to a"
            , "-- fresh table instead of mutating it, this reference would"
            , "-- still point at the old one and would never see the"
            , "-- restored rows -- the orphaning #900 exists to prevent,"
            , "-- and what makes this more than comparing a local to"
            , "-- itself."
            , "local consumerRef = aiState"
            , "local rows = { [1] = { buildTarget = 1 }, [9] = { buildTarget = 1 } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 7,"
            , "    payload = codec.encode(refs.wrapAiState(rows)) },"
            , "}, 1, false, { unit = { [1] = true }, building = {} })"
            , "assert(prep.ok, 'an absent owner is tolerated-dangling: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "assert(aiState[9] == nil,"
            , "  'a row whose unit is absent must be dropped')"
            , "assert(#warnings == 1, 'exactly one drop diagnostic, got '"
            , "  .. #warnings)"
            , "assert(warnings[1]:find('9', 1, true) ~= nil,"
            , "  'the diagnostic must name the dropped unit: ' .. warnings[1])"
            , "-- Building 1 survives too, so buildTarget stays resolvable"
            , "-- and the reference below is testing table identity rather"
            , "-- than the dangling-reference scrub."
            , "reconcile.reconcile(aiState, { 1 }, { 1 }, CTX)"
            , "assert(aiState[1] ~= nil and aiState[1].nextActionAt == 0,"
            , "  'the retained row applies AND is normalized')"
            , "assert(aiState[9] == nil,"
            , "  'normalizing the retained rows must not resurrect the "
              <> "dropped one')"
            , "assert(consumerRef[1] ~= nil and consumerRef[1].buildTarget == 1"
            , "       and consumerRef[1].nextActionAt == 0,"
            , "  'the reference a consumer took BEFORE the load must see the "
              <> "restored, normalized rows -- aiState is mutated in place, "
              <> "never rebound')"
            ]

        it "leaves a SPARSE pre-load row untouched when an abandoned \
           \load unwinds through it: apply() is also applyAll's rollback \
           \entry point, and that unwind must restore the old session \
           \VERBATIM -- the fill is post-PUBLICATION, which a rolled-back \
           \load never reaches" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "local defaults = require('scripts.unit_ai_defaults')"
            , "-- The PRE-LOAD live session carries a sparse row. (A real"
            , "-- one cannot, now that both installers normalize -- but the"
            , "-- rollback contract is 'verbatim', not 'verbatim for rows"
            , "-- that happen to be complete', and it is the contract this"
            , "-- pins.)"
            , "aiState[1] = { buildTarget = 1 }"
            , "-- A reset hook that throws: it runs only AFTER every"
            , "-- component has committed, so unit_ai's forward apply has"
            , "-- definitely happened and is then unwound -- the exact"
            , "-- ordering an apply-failure in a later component produces,"
            , "-- without needing to force one."
            , "saveModules.registerResetHook('boom', function()"
            , "  error('reset hook failed') end)"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 7,"
            , "    payload = codec.encode(refs.wrapAiState("
            , "      { [9] = { buildTarget = 1 } })) },"
            , "}, 1, false, { unit = { [9] = true }, building = {} })"
            , "assert(prep.ok, table.concat(prep.errors or {}, '; '))"
            , "local ok = pcall(saveModules.applyAll)"
            , "assert(not ok, 'a throwing reset hook must fail the load')"
            , "-- The unwind restored the OLD session. Its sparse row must"
            , "-- come back exactly as it was, and reconcile -- which is"
            , "-- what would have filled it -- never ran."
            , "assert(aiState[1] ~= nil, 'the pre-load row must be restored')"
            , "assert(aiState[1].buildTarget == 1, 'restored verbatim')"
            , "assert(aiState[9] == nil,"
            , "  'the row from the abandoned load must be gone')"
            , "for _, f in ipairs(defaults.FIELDS) do"
            , "  assert(aiState[1][f.name] == nil,"
            , "    'a rollback must not add ' .. f.name .. ' to a pre-load "
              <> "row -- the unwind is VERBATIM, and the load it belongs to "
              <> "was abandoned')"
            , "end"
            ]

        it "fills a restored row from the SAME declaration ensureState \
           \builds a fresh one from, so the two installers cannot drift \
           \-- the enumeration is one list, not two agreeing by \
           \coincidence" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local defaults = require('scripts.unit_ai_defaults')"
            , "local core = require('scripts.unit_ai_core')"
            , "local fresh = core.ensureState(42)"
            , "local normalized = defaults.normalize({})"
            , "-- Same keys, same values: whatever ensureState produces for"
            , "-- a unit the AI has never seen is exactly what a sparse"
            , "-- restored row is brought up to."
            , "for k, v in pairs(fresh) do"
            , "  assert(normalized[k] == v,"
            , "    'ensureState set ' .. k .. ' but normalize did not match')"
            , "end"
            , "for k, v in pairs(normalized) do"
            , "  assert(fresh[k] == v,"
            , "    'normalize set ' .. k .. ' but ensureState did not match')"
            , "end"
            , "-- And the declaration names exactly the fields a tick reads"
            , "-- before it has decided anything (#2055 requirement 2)."
            , "local named = {}"
            , "for _, f in ipairs(defaults.FIELDS) do named[f.name] = true end"
            , "assert(named.currentAction and named.actionStartedAt"
            , "       and named.nextActionAt,"
            , "  'the three fields the pre-decision tick path reads must all "
              <> "be declared')"
            , "assert(#defaults.FIELDS == 3,"
            , "  'a field added to FIELDS needs its own justification here: '"
            , "  .. 'the list is the fields a tick reads BEFORE deciding, '"
            , "  .. 'not every field a row may carry')"
            , "assert(named.commandedTask == nil,"
            , "  'commandedTask must NOT be defaulted -- nil is its value')"
            ]

    describe "unit_ai post-load reconciliation (issue #1589)" $ do
        -- The WHOLE persisted path, not a hand-built table: a versioned
        -- payload goes through the REAL registered lua.unit_ai
        -- component's decode/migrate/validate/apply, and only then
        -- through the REAL reconcile -- so the wire wrap/unwrap, the
        -- per-entity apply and the stale-reference scrub are all
        -- exercised against the same rows in one pass. Requirement 9
        -- asks for exactly that here, including the per-page cases:
        -- bill 5 exists on BOTH pages (two different entities that
        -- share a number) and ground item 7 on page A only.
        it "resolves or clears EVERY reference family the schema \
           \declares on a row restored through the real component -- \
           \including the six the pre-#1589 scrub never reached -- and \
           \resolves the per-page kinds against the OWNING unit's page, \
           \so a same-numbered bill on another page is a different \
           \entity and a page A ground item is absent for a page B unit" $
            runsOk $ lns $ unitAiReconcilePrelude ⧺
            [ "-- v1 (bare ids) so the migration + wire wrap/unwrap run"
            , "-- before the reconcile ever sees a row."
            , "local prep = saveModules.prepareLoad({ { id = 'unit_ai',"
            , "  version = 1, payload = codec.encode({"
            , "  [1] = { attackTargetUid = 99,"
            , "          craftJob = { billId = 5, bid = 3,"
            , "                       recipeId = 'known_recipe' },"
            , "          repairJob = { instanceId = 901, itemFetched = true,"
            , "                        recipeId = 'known_repair',"
            , "                        defName = 'axe_steel',"
            , "                        consumable = 'whetstone' },"
            , "          pickupOrder = { gid = 7 },"
            , "          forageTarget = { kind = 'ground', gid = 71, x = 1, y = 1 },"
            , "          forageLoot = { 7, 71 }, foragePhase = 'collecting',"
            , "          harvestLoot = { 70 }, harvestPhase = 'collecting' },"
            , "  [2] = { craftJob = { billId = 5, bid = 42,"
            , "                       recipeId = 'known_recipe' },"
            , "          pickupOrder = { gid = 7 } } }) } },"
            , "  1, false, { unit = { [1] = true, [2] = true },"
            , "              building = { [42] = true },"
            , "              unitPage = { [1] = 'A', [2] = 'B' } })"
            , "assert(prep.ok, 'a DANGLING reference is tolerated at load, "
              <> "never a failure: ' .. table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "assert(aiState[1] ~= nil and aiState[2] ~= nil,"
            , "  'both rows apply -- their units are in the restored session')"
            , "assert(aiState[1].craftJob.billId == 5,"
            , "  'apply unwraps the wire reference back to a bare number, "
              <> "which is what the reconcile then resolves')"
            , ""
            , "reconcile.reconcile(aiState, { 1, 2 }, { 42 }, CTX)"
            , ""
            , "local one = aiState[1]"
            , "assert(one.attackTargetUid == nil,"
            , "  'a unit ref outside the survivor set clears (the #195 case)')"
            , "assert(one.craftJob == nil,"
            , "  'craftJob is dropped WHOLE: its bill resolved, but its "
              <> "station did not')"
            , "assert(one.repairJob == nil and one.repairPhase == nil,"
            , "  'repairJob goes out through unit_ai_repair.lua\\'s abort "
              <> "path, which clears the phase too')"
            , "assert(MULE_MOVES == 1,"
            , "  'and hands the already-fetched item back to the mule, "
              <> "rather than leaving it stranded: ' .. tostring(MULE_MOVES))"
            , "assert(one.pickupOrder ~= nil and one.pickupOrder.gid == 7,"
            , "  'ground item 7 exists on page A, where unit 1 lives, so its "
              <> "order survives untouched')"
            , "assert(one.forageTarget == nil,"
            , "  'a ground forageTarget naming no live ground item clears')"
            , "assert(#one.forageLoot == 1 and one.forageLoot[1] == 7,"
            , "  'forageLoot keeps its resolvable gid as a dense array')"
            , "assert(one.foragePhase == 'collecting',"
            , "  'a still-populated forage list keeps its phase')"
            , "assert(one.harvestLoot == nil and one.harvestPhase == nil,"
            , "  'an EMPTIED harvest list leaves the shape its own "
              <> "exhaustion path leaves')"
            , ""
            , "local two = aiState[2]"
            , "assert(two.craftJob ~= nil and two.craftJob.billId == 5,"
            , "  'unit 2 lives on page B, which has its OWN bill 5 -- a "
              <> "different entity that happens to share the number')"
            , "assert(two.pickupOrder == nil,"
            , "  'ground item 7 exists only on page A, so it is absent for "
              <> "a page B unit -- never resolved session-wide')"
            , ""
            , "-- Seven dangling declared edges: attackTargetUid,"
            , "-- craftJob.bid, repairJob.instanceId, forageTarget.gid,"
            , "-- forageLoot[2] and harvestLoot[1] on unit 1, plus unit 2's"
            , "-- pickupOrder.gid. craftJob.billId resolved, so the sibling"
            , "-- removed with the dropped job counts nothing."
            , "local reported = tonumber(LOG[#LOG]:match('(%d+) stale ref'))"
            , "assert(reported == 7,"
            , "  'the reconcile log must count every dangling edge removed, "
              <> "got ' .. tostring(reported))"
            ]

        it "refuses to reconcile at all when the engine supplies no \
           \reconciliation context, rather than silently resolving \
           \per-page ids against whichever page is active" $
            runsOk $ lns $ unitAiReconcilePrelude ⧺
            [ "aiState[1] = { pickupOrder = { gid = 7 } }"
            , "assert(not pcall(reconcile.reconcile, aiState, { 1 }, {}, nil),"
            , "  'an absent context must raise, not be treated as empty')"
            , "assert(aiState[1].pickupOrder ~= nil,"
            , "  'and must not have cleared anything on its way out')"
            ]

    describe "component version bounds (issue #761 round-4 review)" $ do
        it "rejects a version or inputVersions entry that is non-finite or \
           \outside Word32's representable range -- such a value passed \
           \Lua's own \"positive integer\" check (floor(math.huge) is \
           \math.huge) but HsLua's tointeger can't convert it, which used \
           \to make the whole component record silently vanish instead of \
           \failing the registration" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local function tryRegister(version, inputVersions)"
            , "  return pcall(saveModules.register, 'bad_version', {"
            , "    version = version, inputVersions = inputVersions,"
            , "    required = true, scope = 'global', deps = {},"
            , "    snapshot = function() return {} end,"
            , "    decode = function(_v, d) return d end,"
            , "    validate = function() return nil end,"
            , "    apply = function() end,"
            , "  })"
            , "end"
            , "local ok1 = tryRegister(math.huge, { math.huge })"
            , "assert(not ok1, 'math.huge must not be accepted as a version')"
            , "local ok2 = tryRegister(-math.huge, { -math.huge })"
            , "assert(not ok2, '-math.huge must not be accepted as a version')"
            , "local ok3 = tryRegister(4294967296, { 4294967296 })"
            , "assert(not ok3, 'a version above Word32 max must not be accepted')"
            , "local ok4 = tryRegister(0/0, { 0/0 })"
            , "assert(not ok4, 'NaN must not be accepted as a version')"
            , "local ok5 = tryRegister(1, { 1 })"
            , "assert(ok5, 'an ordinary positive integer version must still register')"
            ]

    -- Issue #766 (save-overhaul C4): docs/save_compat/manifest.json's
    -- "b3-lua-versioned" baseline tracks these two .bin fixtures --
    -- REAL v1 unit_ai/building_spawn payloads encoded through the
    -- genuine scripts/lib/data_codec.lua (via a real HsLua VM, see
    -- tools/save_compat_audit.py's "add tracked Lua payload/session
    -- fixtures with canonical expectations and exercise them through
    -- the real Lua preparation path" requirement) -- not re-synthesized
    -- inline via codec.encode the way every OTHER test above does. This
    -- proves the tracked BYTES ON DISK are what saveModules.prepareLoad
    -- accepts, matching test-headless/data/save-compat/
    -- lua-unit-ai-v1.expected.json / lua-building-spawn-v1.expected.json.
    describe "tracked v1 fixtures from disk (issue #766, save-overhaul C4)" $ do
        it "migrates the tracked lua-unit-ai-v1.bin fixture through \
           \saveModules.prepareLoad/applyAll to exactly the canonical \
           \unwrapped aiState and reference edges its .expected.json \
           \records" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/lua-unit-ai-v1.bin"
            runsOkWithPayloads [("FIXTURE", bytes)] $ lns
                [ "unit = { exists = function(_uid) return true end }"
                , "craft = { get = function(id)"
                , "  if id == 'x' then return { id = 'x' } end return nil end }"
                , "item = { listDefs = function() return {} end }"
                , "local unitAiSave = require('scripts.unit_ai_save')"
                , "local fakeAiState = {}"
                , "unitAiSave.register(fakeAiState)"
                , "local saveModules = require('scripts.lib.save_modules')"
                , "local prep = saveModules.prepareLoad({"
                , "  { id = 'unit_ai', version = 1, payload = FIXTURE },"
                , "})"
                , "assert(prep.ok, 'the tracked v1 fixture must migrate cleanly: '"
                , "  .. table.concat(prep.errors or {}, '; '))"
                , "local found = {}"
                , "for _, r in ipairs(prep.references) do"
                , "  found[r.kind .. ':' .. tostring(r.id)] = r.owner"
                , "end"
                , "assert(found['unit:7'] == 7,"
                , "  'the outer per-unit key itself must be a reference')"
                , "assert(found['unit:8'] == 7, 'attackTargetUid must resolve')"
                , "assert(found['building:20'] == 7, 'buildTarget must resolve')"
                , "assert(found['craft_bill:3'] == 7, 'craftJob.billId must resolve')"
                , "assert(found['building:21'] == 7, 'craftJob.bid must resolve')"
                , "saveModules.applyAll()"
                , "assert(fakeAiState[7].attackTargetUid == 8,"
                , "  'apply() must unwrap attackTargetUid to a bare number')"
                , "assert(fakeAiState[7].buildTarget == 20,"
                , "  'apply() must unwrap buildTarget to a bare number')"
                , "assert(fakeAiState[7].craftJob.billId == 3,"
                , "  'apply() must unwrap craftJob.billId to a bare number')"
                , "assert(fakeAiState[7].craftJob.bid == 21,"
                , "  'apply() must unwrap craftJob.bid to a bare number')"
                , "assert(fakeAiState[7].craftJob.recipeId == 'x',"
                , "  'non-reference fields must survive the migration untouched')"
                ]

        it "decodes the tracked lua-unit-ai-v4.bin fixture's per-unit \
           \location memories (#915) through saveModules.prepareLoad/ \
           \applyAll, keeping each entry's page, id and remembered anchor, \
           \and reporting one page-qualified location_instance edge each" $ do
            -- The complete-session baseline's canonical summary is
            -- Lua-OPAQUE (SessionSnapshot carries no Lua state), so
            -- save_compat_audit alone would pass even if this typed
            -- memory were dropped or mis-encoded. THIS is the assertion
            -- that would fail: real tracked bytes, produced by the real
            -- wrapAiState encoder, driven through the real preparation
            -- path — the same shape the e1 session fixture carries.
            bytes ← BS.readFile
                "test-headless/data/save-compat/lua-unit-ai-v4.bin"
            runsOkWithPayloads [("FIXTURE", bytes)] $ lns
                [ "unit = { exists = function(_uid) return true end }"
                , "item = { listDefs = function() return {} end }"
                , "local unitAiSave = require('scripts.unit_ai_save')"
                , "local fakeAiState = {}"
                , "unitAiSave.register(fakeAiState)"
                , "local saveModules = require('scripts.lib.save_modules')"
                , "local prep = saveModules.prepareLoad({"
                , "  { id = 'unit_ai', version = 4, payload = FIXTURE },"
                , "})"
                , "assert(prep.ok, 'the tracked v4 fixture must prepare cleanly: '"
                , "  .. table.concat(prep.errors or {}, '; '))"
                -- Each memory is reported as its OWN page-qualified edge:
                -- an id alone would be ambiguous, since the fixture
                -- deliberately carries the SAME instance id (1) on two
                -- different pages.
                , "local edges = {}"
                , "for _, r in ipairs(prep.references) do"
                , "  if r.kind == 'location_instance' then"
                , "    edges[r.path] = r end end"
                , "local a = edges['unit[7].knownLocations[1]']"
                , "local b = edges['unit[7].knownLocations[2]']"
                , "assert(a and a.id == 1 and a.page == 'generated_page'"
                , "       and a.owner == 7, 'first memory edge wrong')"
                , "assert(b and b.id == 1 and b.page == 'other_page'"
                , "       and b.owner == 7, 'second memory edge wrong')"
                , "saveModules.applyAll()"
                , "local ks = fakeAiState[7].knownLocations"
                , "assert(type(ks) == 'table' and #ks == 2,"
                , "  'apply() must restore both memories')"
                , "assert(ks[1].page == 'generated_page' and ks[1].id == 1"
                , "       and ks[1].x == 104 and ks[1].y == 40,"
                , "  'the first memory lost its page/id/anchor')"
                , "assert(ks[2].page == 'other_page' and ks[2].id == 1"
                , "       and ks[2].x == 3 and ks[2].y == 4,"
                , "  'the second memory lost its page/id/anchor')"
                -- aiState's LIVE shape never grows the wire tag.
                , "assert(ks[1].__ref == nil and ks[2].__ref == nil,"
                , "  'apply() must strip the __ref wire tag')"
                -- …and the sibling reference fields still migrate.
                , "assert(fakeAiState[7].attackTargetUid == 8,"
                , "  'a v4 payload must still unwrap its other references')"
                ]

        it "migrates the tracked lua-building-spawn-v1.bin fixture through \
           \saveModules.prepareLoad/applyAll to exactly the canonical \
           \unwrapped state and reference edges its .expected.json \
           \records" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/lua-building-spawn-v1.bin"
            runsOkWithPayloads [("FIXTURE", bytes)] $ lns
                [ "building = { getInfo = function(_bid) return { id = _bid } end }"
                , "local buildingSpawn = require('scripts.building_spawn')"
                , "buildingSpawn.init('test')"
                , "local saveModules = require('scripts.lib.save_modules')"
                , "local prep = saveModules.prepareLoad({"
                , "  { id = 'building_spawn', version = 1, payload = FIXTURE },"
                , "})"
                , "assert(prep.ok, 'the tracked v1 fixture must migrate cleanly: '"
                , "  .. table.concat(prep.errors or {}, '; '))"
                , "local found = {}"
                , "for _, r in ipairs(prep.references) do"
                , "  found[r.kind .. ':' .. tostring(r.id)] = true"
                , "end"
                , "assert(found['building:12'],"
                , "  'the outer per-building key itself must be a reference')"
                , "assert(found['unit:4'], 'lastUid must be a reference')"
                , "saveModules.applyAll()"
                , "assert(buildingSpawn.state[12].lastUid == 4,"
                , "  'apply() must unwrap lastUid to a bare number in LIVE state')"
                , "assert(buildingSpawn.state[12].lastSpawnedAt == 123.5,"
                , "  'non-reference fields must survive the migration untouched')"
                ]
