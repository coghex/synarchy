-- | Every AI target the four LAX item verbs can reach is paired with
--   the ACTING unit's own world page (#1673).
--
--   @unit.getAllIds@, @building.getActiveIds@ and @craft.getBills@ each
--   resolve the ACTIVE page independently, at the instant they are
--   called, and @world.show@ \/ @world.hide@ only ENQUEUE a selection
--   change the world thread applies later. So the active page can move
--   between any two of those calls inside one AI update, and a finder
--   that trusts "the actors and the buildings both came from the active
--   page" can hand an actor a candidate in another world. The finders'
--   own comments asserted that pairing; nothing enforced it.
--
--   The engine-side refusal
--   ('Test.Headless.Unit.CargoApi') is the caller-independent half and
--   closes the item-state consequence. This gate is the OTHER half: an
--   off-page candidate must never be cached, claimed, walked toward, or
--   passed to a lax verb in the first place — a walk toward another
--   page's coordinates is a consequence no commit-time refusal can
--   undo.
--
--   Every case is the same shape: one candidate, at the SAME
--   coordinates and in the same state, run twice — once on the actor's
--   page and once on another. Only the page differs, so a case cannot
--   pass because the candidate was unattractive for some other reason.
--
--   Same standalone-Lua-VM pattern as
--   'Test.Headless.Lua.UnitAiLogisticsTargets': one self-contained
--   chunk per 'it' in a fresh interpreter, asserting inside Lua, with
--   the REAL shipped modules and the REAL shipped acolyte tunables.
--   @scripts.movement_speed@ is stubbed (the real one reaches the whole
--   physiology chain to answer one pace) and @scripts.unit_ai@ is the
--   singleton table the submodules attach to; everything the page rule
--   actually runs through — the finders, the ladders, the caches — is
--   production code.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "AI page pairing"'@.
module Test.Headless.Lua.UnitAiPageTargets (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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

-- | A world model with an explicit page per entity, plus counters for
--   every observable consequence a mis-paired target would cause: a
--   move request, a stop, and each of the four lax item calls.
--
--   @HOME@ is the acting unit's page and @AWAY@ is the other live one.
--   A case places its ONE candidate on whichever it is testing; the
--   candidate is otherwise byte-identical between the two halves.
prelude ∷ Text
prelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.0 end,"
    , "    sprint  = function() return 1.0 end }"
    , "HOME, AWAY = 'page_home', 'page_away'"
    , "NOW = 0"
    , "UNITS = {}       -- [uid] = info row, page included"
    , "BUILDINGS = {}   -- array of building rows, page included"
    , "BILLS = {}"
    , "GROUND = {}"
    , "MOVES, STOPS = 0, 0"
    , "DEPOSITS, WITHDRAWS, TO_UNIT, TO_BUILDING = 0, 0, 0, 0"
    , "function unitRow(uid, defName, x, y, pg, inv)"
    , "  UNITS[uid] = { uid = uid, defName = defName, gridX = x, gridY = y,"
    , "                 page = pg, inv = inv or {}, contents = {} }"
    , "  return UNITS[uid]"
    , "end"
    , "-- A BUILT store, or an APPEARING build site, at (x, 0) on page pg."
    , "function buildingRow(kind, bid, x, pg, extra)"
    , "  local b = { bid = bid, x = x, y = 0, page = pg, storage = {},"
    , "              need = {}, delivered = {} }"
    , "  if kind == 'store' then"
    , "    b.activity, b.required, b.capacity, b.used = 'built', 0, 100, 0"
    , "  else"
    , "    b.activity, b.required, b.capacity, b.used = 'appearing', 240.0, 0, 0"
    , "  end"
    , "  for k, v in pairs(extra or {}) do b[k] = v end"
    , "  BUILDINGS[#BUILDINGS + 1] = b"
    , "  return b"
    , "end"
    , "local function brow(bid)"
    , "  for _, b in ipairs(BUILDINGS) do if b.bid == bid then return b end end"
    , "end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logError = function() end,"
    , "           logInfo = function() end, logDebug = function() end,"
    , "           emitEvent = function() end, emitEventAt = function() end,"
    , "           emitEventForUnit = function() end }"
    , "world = { getActiveWorldId = function() return 1 end }"
    , "debug = debug or {}"
    , "debug.recordOutcome = function() end"
    , "unit = {"
    , "  exists = function(uid) return UNITS[uid] ~= nil end,"
    , "  getAllIds = function()"
    , "    local ids = {}"
    , "    for uid in pairs(UNITS) do ids[#ids + 1] = uid end"
    , "    table.sort(ids)"
    , "    return ids end,"
    , "  getInfo = function(uid) return UNITS[uid] end,"
    , "  getInventory = function(uid)"
    , "    return UNITS[uid] and UNITS[uid].inv or {} end,"
    , "  getItemContents = function(uid, defName)"
    , "    local u = UNITS[uid]"
    , "    return u and u.contents[defName] or {} end,"
    -- Half-loaded: store_materials' utility is (carried/capacity)^3, so
    -- an empty unit would score 0 and the same-page control could not
    -- tell "cached and scored" from "found nothing".
    , "  getCarryingWeight = function() return 50 end,"
    , "  getStat = function(_, k)"
    , "    if k == 'carrying_capacity' then return 100 end"
    , "    return 1.0 end,"
    , "  getSkill = function() return 25.0 end,"
    , "  getKnowledge = function() return true end,"
    , "  getMentalEffectiveness = function() return 1.0 end,"
    , "  getPose = function() return 'standing' end,"
    , "  dropItemById = function() end,"
    , "  moveTo = function() MOVES = MOVES + 1 end,"
    , "  stop = function() STOPS = STOPS + 1 end,"
    , "  depositToCargo = function() DEPOSITS = DEPOSITS + 1; return false end,"
    , "  withdrawFromCargo = function() WITHDRAWS = WITHDRAWS + 1; return false end,"
    , "  transferItemToUnit = function() TO_UNIT = TO_UNIT + 1; return false end,"
    , "  transferItemToBuilding = function()"
    , "    TO_BUILDING = TO_BUILDING + 1; return false end }"
    , "building = {"
    , "  getActiveIds = function()"
    , "    local ids = {}"
    , "    for _, b in ipairs(BUILDINGS) do ids[#ids + 1] = b.bid end"
    , "    return ids end,"
    , "  getActivity = function(bid) local b = brow(bid); return b and b.activity end,"
    , "  getBuildRequired = function(bid) local b = brow(bid); return b and b.required end,"
    , "  getStorageCapacity = function(bid) local b = brow(bid); return b and b.capacity end,"
    , "  getStorageWeight = function(bid) local b = brow(bid); return b and b.used end,"
    , "  getStorage = function(bid) local b = brow(bid); return b and b.storage or {} end,"
    , "  getMaterialNeed = function(bid) local b = brow(bid); return b and b.need or {} end,"
    , "  getMaterialDelivered = function(bid)"
    , "    local b = brow(bid); return b and b.delivered or {} end,"
    , "  areMaterialsSatisfied = function(bid)"
    , "    local b = brow(bid); return b == nil or next(b.need) == nil end,"
    , "  getInfo = function(bid)"
    , "    local b = brow(bid)"
    , "    if not b then return nil end"
    , "    return { gridX = b.x, gridY = b.y, tileW = 1, tileH = 1,"
    , "             page = b.page } end }"
    , "PICKUPS = 0"
    , "item = {"
    , "  listGround = function() return GROUND end,"
    , "  pickupGround = function() PICKUPS = PICKUPS + 1; return true end,"
    , "  listDefs = function() return { { name = 'plate_steel', weight = 1 } } end }"
    , "craft = {"
    , "  getBills = function() return BILLS end,"
    , "  getBill = function(id)"
    , "    for _, b in ipairs(BILLS) do if b.id == id then return b end end end,"
    , "  get = function(rid) return { id = rid, work = 0, inputs = {} } end,"
    , "  claimBill = function() return true end,"
    , "  releaseBill = function() end,"
    , "  setBillWorking = function() end,"
    , "  addBillProgress = function() return 1.0 end,"
    , "  completeBillCycle = function() return 0 end,"
    , "  executeAt = function() return true, {} end }"
    , "power = { isStationPoweredForRecipe = function() return true end }"
    , "local PARAMS = require('scripts.unit_ai_tunables').acolyte"
    , "local page      = require('scripts.unit_ai_page')"
    , "local fetch     = require('scripts.unit_ai_fetch')"
    , "local logistics = require('scripts.unit_ai_logistics')"
    , "local deliver   = require('scripts.unit_ai_deliver')"
    , "local craftAi   = require('scripts.unit_ai_craft')"
    , "local medic     = require('scripts.unit_ai_medic')"
    , "-- The acting unit: uid 1, on HOME, at the origin."
    , "local ACTOR = unitRow(1, 'acolyte', 0, 0, HOME,"
    , "                      { { defName = 'plate_steel', category = 'Materials' } })"
    , "local function newState() return { currentAction = nil } end"
    ]

spec ∷ Spec
spec = describe "AI page pairing" $ do

    describe "store_materials" $ do
        it "ignores a cargo on another page, and takes the same one on its own" $
            runsOk $ lns
                [ prelude
                , "buildingRow('store', 9, 5, AWAY)"
                , "local s = newState()"
                , "s.storeTarget = 4242"
                , "local u = logistics.storeMaterialsUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page cargo must not score, got ' .. tostring(u))"
                , "assert(s.storeTarget == nil,"
                , "  'an off-page cargo must not be cached, left ' .. tostring(s.storeTarget))"
                , "logistics.storeMaterialsExecute(1, s, PARAMS)"
                , "assert(MOVES == 0 and DEPOSITS == 0,"
                , "  'an off-page cargo must draw neither a walk nor a deposit')"
                , "-- The SAME building, same coordinates, on the actor's page."
                , "BUILDINGS = {}"
                , "buildingRow('store', 9, 5, HOME)"
                , "local t = newState()"
                , "local v = logistics.storeMaterialsUtility(1, t, PARAMS)"
                , "assert(v > 0, 'a same-page cargo must still score, got ' .. tostring(v))"
                , "assert(t.storeTarget == 9,"
                , "  'a same-page cargo must still be cached, got ' .. tostring(t.storeTarget))"
                , "logistics.storeMaterialsExecute(1, t, PARAMS)"
                , "assert(MOVES == 1, 'a same-page cargo must still draw the walk')"
                ]

    describe "build_nearby" $ do
        it "ignores a site on another page, and takes the same one on its own" $
            runsOk $ lns
                [ prelude
                , "buildingRow('site', 77, 5, AWAY)"
                , "local s = newState()"
                , "s.buildTarget = 4242"
                , "local u = logistics.buildNearbyUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page site must not score, got ' .. tostring(u))"
                , "assert(s.buildTarget == nil,"
                , "  'an off-page site must not be cached, left ' .. tostring(s.buildTarget))"
                , "logistics.buildNearbyExecute(1, s, PARAMS)"
                , "assert(MOVES == 0 and STOPS == 0,"
                , "  'an off-page site must draw neither a walk nor a stop')"
                , "BUILDINGS = {}"
                , "buildingRow('site', 77, 5, HOME)"
                , "local t = newState()"
                , "local v = logistics.buildNearbyUtility(1, t, PARAMS)"
                , "assert(v > 0, 'a same-page site must still score, got ' .. tostring(v))"
                , "assert(t.buildTarget == 77, 'a same-page site must still be cached')"
                , "logistics.buildNearbyExecute(1, t, PARAMS)"
                , "assert(MOVES == 1, 'a same-page site must still draw the walk')"
                ]

    describe "deliver_to_build_site" $ do
        it "ignores a build site on another page" $
            runsOk $ lns
                [ prelude
                , "buildingRow('site', 55, 5, AWAY, { need = { plate_steel = 1 } })"
                , "local s = newState()"
                , "local u = deliver.deliverUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page site must not score, got ' .. tostring(u))"
                , "assert(s.deliveryPendingTarget == nil,"
                , "  'an off-page site must leave no pending target')"
                , "BUILDINGS = {}"
                , "buildingRow('site', 55, 5, HOME, { need = { plate_steel = 1 } })"
                , "local t = newState()"
                , "local v = deliver.deliverUtility(1, t, PARAMS)"
                , "assert(v > 0, 'a same-page site must still score, got ' .. tostring(v))"
                , "assert(t.deliveryPendingTarget"
                , "  and t.deliveryPendingTarget.bid == 55,"
                , "  'a same-page site must still be planned')"
                ]

        it "checks the claim's page BEFORE the sourcing phases run" $
            runsOk $ lns
                [ prelude
                -- The sourcing phases issue their own moveTo /
                -- pickupGround / transferItemToUnit, so a claim whose
                -- page is only checked at the arrival branch has
                -- already walked the unit and moved items by then. The
                -- claim carries BOTH ground and mule work, and both a
                -- reachable ground item and a reachable mule exist on
                -- the actor's page, so a check that ran too late would
                -- demonstrably fire them.
                , "buildingRow('site', 55, 5, AWAY, { need = { plate_steel = 1 } })"
                , "unitRow(2, 'technomule', 1, 0, HOME,"
                , "        { { defName = 'plate_steel', category = 'Materials' } })"
                , "GROUND = { { id = 1, defName = 'plate_steel', x = 0, y = 0,"
                , "             weight = 1 } }"
                , "local s = newState()"
                , "s.deliveryClaim = { bid = 55, materials = { plate_steel = 2 },"
                , "                    fromGround = { plate_steel = 1 },"
                , "                    fromMule = { plate_steel = 1 } }"
                , "deliver.deliverExecute(1, s, PARAMS)"
                , "assert(s.deliveryClaim == nil,"
                , "  'an off-page claim must be released before sourcing')"
                , "assert(PICKUPS == 0,"
                , "  'an off-page claim must never pick a ground item up')"
                , "assert(TO_UNIT == 0,"
                , "  'an off-page claim must never pull from the mule')"
                , "assert(MOVES == 0 and STOPS == 0,"
                , "  'an off-page claim must not steer a walk during sourcing')"
                , "-- Control: the SAME claim on the actor's page does"
                , "-- reach the ground rung, so the assertions above are"
                , "-- about the page and not about an inert fixture."
                , "BUILDINGS = {}"
                , "buildingRow('site', 55, 5, HOME, { need = { plate_steel = 1 } })"
                , "local t = newState()"
                , "t.deliveryClaim = { bid = 55, materials = { plate_steel = 2 },"
                , "                    fromGround = { plate_steel = 1 },"
                , "                    fromMule = { plate_steel = 1 } }"
                , "deliver.deliverExecute(1, t, PARAMS)"
                , "assert(PICKUPS == 1,"
                , "  'a same-page claim must still source from the ground')"
                ]

        it "releases a PERSISTED claim naming a building on another page" $
            runsOk $ lns
                [ prelude
                -- The shape a save written before this check restores,
                -- or a page switch between claim and arrival leaves.
                , "buildingRow('site', 55, 5, AWAY, { need = { plate_steel = 1 } })"
                , "local s = newState()"
                , "s.deliveryClaim = { bid = 55, materials = { plate_steel = 1 } }"
                , "deliver.deliverExecute(1, s, PARAMS)"
                , "assert(s.deliveryClaim == nil,"
                , "  'an off-page claim must be released, left '"
                , "  .. tostring(s.deliveryClaim and s.deliveryClaim.bid))"
                , "assert(MOVES == 0, 'an off-page claim must not steer a walk')"
                , "assert(TO_BUILDING == 0,"
                , "  'an off-page claim must never reach transferItemToBuilding')"
                ]

    describe "the cargo rung of the sourcing ladder" $ do
        it "counts stock on the actor's page only" $
            runsOk $ lns
                [ prelude
                , "local stock = { { defName = 'plate_steel' } }"
                , "buildingRow('store', 20, 5, AWAY, { storage = stock })"
                , "assert(fetch.cargoCountOf('plate_steel', HOME) == 0,"
                , "  'off-page cargo stock must not be counted')"
                , "BUILDINGS = {}"
                , "buildingRow('store', 20, 5, HOME, { storage = stock })"
                , "assert(fetch.cargoCountOf('plate_steel', HOME) == 1,"
                , "  'same-page cargo stock must still be counted')"
                , "assert(fetch.cargoCountOf('plate_steel', nil) == 0,"
                , "  'an unknown actor page must count nothing')"
                ]

        it "never walks to or withdraws from a store on another page" $
            runsOk $ lns
                [ prelude
                , "local stock = { { defName = 'plate_steel' } }"
                , "buildingRow('store', 20, 5, AWAY, { storage = stock })"
                , "local busy = fetch.fetchWantsFromCargo("
                , "  1, { plate_steel = 1 }, ACTOR, PARAMS)"
                , "assert(busy == false, 'no reachable store means the fetch is done')"
                , "assert(MOVES == 0 and WITHDRAWS == 0,"
                , "  'an off-page store must draw neither a walk nor a withdrawal')"
                , "BUILDINGS = {}"
                , "buildingRow('store', 20, 5, HOME, { storage = stock })"
                , "local busy2 = fetch.fetchWantsFromCargo("
                , "  1, { plate_steel = 1 }, ACTOR, PARAMS)"
                , "assert(busy2 == true, 'a distant same-page store must still be walked to')"
                , "assert(MOVES == 1, 'a same-page store must still draw the walk')"
                ]

    describe "craft_job" $ do
        it "ignores a station on another page" $
            runsOk $ lns
                [ prelude
                , "buildingRow('store', 42, 5, AWAY)"
                , "BILLS = { { id = 1, station = 42, recipe = 'r', mode = 'count' } }"
                , "local s = newState()"
                , "local u = craftAi.craftUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page station must not score, got ' .. tostring(u))"
                , "assert(s.craftCandidate == nil,"
                , "  'an off-page station must leave no candidate')"
                , "BUILDINGS = {}"
                , "buildingRow('store', 42, 5, HOME)"
                , "local t = newState()"
                , "local v = craftAi.craftUtility(1, t, PARAMS)"
                , "assert(v > 0, 'a same-page station must still score, got ' .. tostring(v))"
                , "assert(t.craftCandidate and t.craftCandidate.bill.id == 1,"
                , "  'a same-page station must still be nominated')"
                ]

        it "checks the station's page BEFORE the fetch phase runs" $
            runsOk $ lns
                [ prelude
                -- Same hazard as the delivery claim: craftExecute's
                -- fetch phase sources from ground, mule and cargo, all
                -- of which move the unit and items. A station checked
                -- only at the walking phase is checked too late.
                , "buildingRow('store', 42, 5, AWAY)"
                , "buildingRow('store', 43, 6, HOME,"
                , "  { storage = { { defName = 'plate_steel' } } })"
                , "unitRow(2, 'technomule', 1, 0, HOME,"
                , "        { { defName = 'plate_steel', category = 'Materials' } })"
                , "GROUND = { { id = 1, defName = 'plate_steel', x = 0, y = 0,"
                , "             weight = 1 } }"
                , "BILLS = { { id = 1, station = 42, recipe = 'r', mode = 'count' } }"
                , "local s = newState()"
                , "s.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 0,"
                , "               skill = 'smithing',"
                , "               need = { plate_steel = 3 },"
                , "               fromGround = { plate_steel = 1 },"
                , "               fromMule = { plate_steel = 1 },"
                , "               fromCargo = { plate_steel = 1 },"
                , "               phase = 'fetch' }"
                , "craftAi.craftExecute(1, s, PARAMS)"
                , "assert(s.craftJob == nil,"
                , "  'an off-page craft job must be released before sourcing')"
                , "assert(PICKUPS == 0 and TO_UNIT == 0 and WITHDRAWS == 0,"
                , "  'an off-page craft job must move no items while sourcing')"
                , "assert(MOVES == 0 and STOPS == 0,"
                , "  'an off-page craft job must not steer a walk while sourcing')"
                , "-- Control: the same job at a same-page station does"
                , "-- reach the ground rung."
                , "BUILDINGS = {}"
                , "buildingRow('store', 42, 5, HOME)"
                , "local t = newState()"
                , "t.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 0,"
                , "               skill = 'smithing',"
                , "               need = { plate_steel = 3 },"
                , "               fromGround = { plate_steel = 1 },"
                , "               fromMule = { plate_steel = 1 },"
                , "               fromCargo = { plate_steel = 1 },"
                , "               phase = 'fetch' }"
                , "craftAi.craftExecute(1, t, PARAMS)"
                , "assert(PICKUPS == 1,"
                , "  'a same-page craft job must still source from the ground')"
                ]

        it "releases a PERSISTED job naming a station on another page" $
            runsOk $ lns
                [ prelude
                , "buildingRow('store', 42, 5, AWAY)"
                , "BILLS = { { id = 1, station = 42, recipe = 'r', mode = 'count' } }"
                , "local s = newState()"
                , "s.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 0,"
                , "               skill = 'smithing', need = {}, fromGround = {},"
                , "               fromMule = {}, fromCargo = {}, phase = 'walking' }"
                , "craftAi.craftExecute(1, s, PARAMS)"
                , "assert(s.craftJob == nil,"
                , "  'an off-page craft job must be released')"
                , "assert(MOVES == 0, 'an off-page station must not steer a walk')"
                ]

    describe "unit-to-unit item targets" $ do
        it "findTechnomule refuses a mule on another page" $
            runsOk $ lns
                [ prelude
                , "unitRow(2, 'technomule', 5, 0, AWAY)"
                , "assert(fetch.findTechnomule(1, 0, 0) == nil,"
                , "  'an off-page mule must not be selected')"
                , "UNITS[2] = nil"
                , "unitRow(2, 'technomule', 5, 0, HOME)"
                , "local m = fetch.findTechnomule(1, 0, 0)"
                , "assert(m and m.uid == 2,"
                , "  'a same-page mule must still be selected')"
                , "-- An actor whose own page cannot be read selects nothing."
                , "assert(fetch.findTechnomule(99, 0, 0) == nil,"
                , "  'an unknown actor must select no mule')"
                ]

        it "findKitHolder refuses a holder on another page" $
            runsOk $ lns
                [ prelude
                , "local function kitHolder(uid, pg)"
                , "  local u = unitRow(uid, 'technomule', 5, 0, pg,"
                , "    { { defName = 'medkit', kind = 'container' } })"
                , "  u.contents['medkit'] = { { defName = 'bandage', count = 3 } }"
                , "end"
                , "kitHolder(2, AWAY)"
                , "assert(medic.findKitHolder(1, 0, 0) == nil,"
                , "  'an off-page kit holder must not be selected')"
                , "UNITS[2] = nil"
                , "kitHolder(2, HOME)"
                , "local h = medic.findKitHolder(1, 0, 0)"
                , "assert(h and h.uid == 2 and h.kit == 'medkit',"
                , "  'a same-page kit holder must still be selected')"
                ]
