-- | The "unit AI reconciliation boundary" gate (#1589): every typed
--   reference family @scripts/unit_ai_ref_schema.lua@'s @REF_SCHEMA@
--   declares is resolved or cleared on a surviving @aiState@ row when a
--   load publishes, and the PER-PAGE kinds are resolved against the
--   OWNING unit's page rather than whichever page happens to be active.
--
--   Both halves of that boundary are covered here, because a scrub that
--   is correct against a hand-built table proves nothing if the engine
--   never delivers that table:
--
--     * @"unit AI reconciliation boundary (#1589)"@ drives the REAL
--       @scripts/unit_ai_reconcile.lua@ -- including its production drop
--       hook, which routes a dropped @repairJob@ through
--       @unit_ai_repair.lua@'s own abort path -- in a bare Lua VM, the
--       same standalone pattern "Test.Headless.Lua.UnitAiLoadReset"
--       uses. The reconcile module, the reference schema and the repair
--       abort are the shipping code; only the engine API surface they
--       reach is stubbed.
--     * @"unit AI reconciliation boundary (engine broadcast, #1589)"@
--       drives the real Lua-thread dispatcher, proving the context
--       'World.Save.Payload.LoadReconcileContext' carries actually
--       arrives as @onSaveLoaded@'s third argument with its identity
--       scopes intact -- empty sets included, and with the same bill
--       number present on two different pages, which is precisely the
--       aliasing a page-blind context would collapse.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "unit AI reconciliation boundary"'@.
module Test.Headless.Lua.UnitAiReconcile (spec, envSpec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Concurrent.STM.TQueue (newTQueue)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..), LuaScript(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Integrity (KnownEntities(..), loadReconcileContextFrom)
import World.Save.Payload
    (LoadReconcileContext(..), emptyLoadReconcileContext)

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

-- | Stubbed engine globals + the real reconcile / schema / repair
--   modules.
--
--   @CTX@ is a realistic reconciliation context: item instance 900
--   exists session-wide, unit 1 lives on page A and unit 2 on page B,
--   and ground item 7 exists on page A ONLY -- while BOTH pages carry
--   their own bill 5, the same number naming two different entities.
--
--   @MULE_MOVES@ records every @unit.transferItemToUnit@ the repair
--   abort path performs, which is how the off-active-page case proves
--   it did not hand a fetched item to a stranger on another map.
prelude ∷ Text
prelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "LOG, MULE_MOVES, ACTIVE = {}, {}, 'A'"
    , "engine = { gameTime = function() return 1000 end,"
    , "  logInfo = function(m) LOG[#LOG + 1] = m end,"
    , "  logWarn = function() end, logError = function() end,"
    , "  emitEventForUnit = function() end, loadYaml = function() return nil end }"
    , "unit = { exists = function() return true end,"
    -- #1673: the AI pairs every candidate with the ACTING unit's own
    -- page, so the stub world needs one. Everyone shares it here;
    -- cross-page rejection is Test.Headless.Lua.UnitAiPageTargets'
    -- subject.
    , "  getInfo = function(u)"
    , "    if u == 77 then"
    , "      return { gridX = 0, gridY = 0, defName = 'technomule',"
    , "               page = 'stub_page' } end"
    , "    return { gridX = 0, gridY = 0, defName = 'acolyte',"
    , "             page = 'stub_page' } end,"
    , "  getAllIds = function() return { 77 } end,"
    , "  getStat = function() return 1.0 end, getSkill = function() return 25.0 end,"
    , "  getInventory = function() return {} end,"
    , "  transferItemToUnit = function(from, to, defName, iid)"
    , "    MULE_MOVES[#MULE_MOVES + 1] = from .. '->' .. to .. ':' .. tostring(iid)"
    , "    return true end,"
    , "  moveTo = function() end, stop = function() end, addXP = function() end,"
    , "  setAnimOverride = function() end, clearAnimOverride = function() end }"
    , "world = { getActiveWorldId = function() return ACTIVE end,"
    , "  getLocationInstance = function() return nil end }"
    , "item = { listGround = function() return {} end,"
    , "  -- #1666: owning-page ground lookup; nothing on this page."
    , "  getGroundForUnit = function() return nil, true end,"
    , "  listDefs = function() return { { name = 'axe' } } end }"
    , "building = { findStation = function() return nil end,"
    , "  getInfo = function() return nil end,"
    , "  listDefs = function() return { { name = 'hut' } } end }"
    , "repair = { get = function() return { id = 'r' } end,"
    , "  repairAt = function() return true end }"
    , "flora = { exists = function() return true end }"
    , "package.loaded['scripts.movement_speed'] = {"
    , "  comfort = function() return 1.0 end, ordered = function() return 1.15 end,"
    , "  sprint = function() return 2.0 end, meander = function() return 0.5 end }"
    , "R = require('scripts.unit_ai_reconcile')"
    , "SCHEMA = require('scripts.unit_ai_ref_schema')"
    , "REPAIR = require('scripts.unit_ai_repair')"
    , "CTX = { item_instance = { [900] = true },"
    , "        unitPage = { [1] = 'A', [2] = 'B' },"
    , "        byPage = { craft_bill = { A = { [5] = true }, B = { [5] = true } },"
    , "                   ground_item = { A = { [7] = true, [8] = true },"
    , "                                   B = {} } } }"
    , "-- How many stale edges did the last reconcile report?"
    , "function scrubbedCount()"
    , "  return tonumber(LOG[#LOG]:match('(%d+) stale ref'))"
    , "end"
    ]

spec ∷ Spec
spec = describe "unit AI reconciliation boundary (#1589)" $ do
    it "settles every restored constructJob against the PUBLISHED \
       \session's designations (#1844): a pre-v8 job adopts the attempt \
       \of the designation really standing at its tile, a v8 job has its \
       \own verified, and anything that does not match exactly is \
       \dropped" $ runsOk $ lns
        [ prelude
        -- One live designation per tile. (10,10) is what the fixture's
        -- jobs claim to be; (11,11) is a DIFFERENT job the player made
        -- at that tile while the save sat on disk; (12,12) carries
        -- nothing at all — a designation load staging self-cleared.
        , "construction = { getDesignationAt = function(_w, x, y)"
        , "  if x == 10 then return { x = 10, y = 10, attempt = 4,"
        , "    category = 'structure', pack = 'dungeon_1', kind = 'floor' } end"
        , "  if x == 11 then return { x = 11, y = 11, attempt = 9,"
        , "    category = 'structure', pack = 'wire', kind = 'wire' } end"
        , "  return nil end }"
        , "local function job(t) return { [1] = { constructJob = t } } end"
        -- A pre-v8 job: no attempt, and the designation is the same job.
        , "local legacy = job{ x = 10, y = 10, category = 'structure',"
        , "  pack = 'dungeon_1', kind = 'floor' }"
        , "R.reconcile(legacy, {1}, {}, CTX)"
        , "assert(legacy[1].constructJob ~= nil, 'a matching legacy job stays')"
        , "assert(legacy[1].constructJob.attempt == 4,"
        , "  'it adopts the live designation\\'s attempt')"
        -- A pre-v8 job whose tile now carries someone else's designation.
        , "local wrong = job{ x = 11, y = 11, category = 'structure',"
        , "  pack = 'dungeon_1', kind = 'floor' }"
        , "R.reconcile(wrong, {1}, {}, CTX)"
        , "assert(wrong[1].constructJob == nil,"
        , "  'a legacy job must not adopt a DIFFERENT job at its tile')"
        -- A v8 job whose designation load staging self-cleared.
        , "local gone = job{ x = 12, y = 12, attempt = 4,"
        , "  category = 'structure', pack = 'dungeon_1', kind = 'floor' }"
        , "R.reconcile(gone, {1}, {}, CTX)"
        , "assert(gone[1].constructJob == nil,"
        , "  'a v8 job over a self-cleared designation must be dropped')"
        -- A v8 job whose attempt was replaced by a successor.
        , "local stale = job{ x = 10, y = 10, attempt = 3,"
        , "  category = 'structure', pack = 'dungeon_1', kind = 'floor' }"
        , "R.reconcile(stale, {1}, {}, CTX)"
        , "assert(stale[1].constructJob == nil,"
        , "  'a v8 job naming a retired attempt must be dropped')"
        -- …and one that still names exactly what is there.
        , "local good = job{ x = 10, y = 10, attempt = 4,"
        , "  category = 'structure', pack = 'dungeon_1', kind = 'floor' }"
        , "R.reconcile(good, {1}, {}, CTX)"
        , "assert(good[1].constructJob ~= nil and"
        , "  good[1].constructJob.attempt == 4, 'an exact v8 job survives')"
        ]

    it "clears a stale reference from EVERY family the schema declares \
       \-- craftJob, repairJob, pickupOrder, a ground forageTarget, \
       \forageLoot and harvestLoot included, none of which the pre-#1589 \
       \scrub reached at all -- while leaving every still-resolvable \
       \reference untouched" $ runsOk $ lns
        [ prelude
        , "local ai = { [1] = {"
        , "  attackTargetUid = 99, buildTarget = 42,"
        , "  craftJob = { billId = 55, bid = 3, recipeId = 'x' },"
        , "  repairJob = { instanceId = 901, recipeId = 'r', defName = 'axe' },"
        , "  pickupOrder = { gid = 70 },"
        , "  forageTarget = { kind = 'ground', gid = 71, x = 1, y = 1 },"
        , "  forageLoot = { 7, 71, 8 }, foragePhase = 'collecting',"
        , "  harvestLoot = { 70 }, harvestPhase = 'collecting' } }"
        , "R.reconcile(ai, { 1 }, { 42 }, CTX)"
        , "local s = ai[1]"
        , "assert(s.attackTargetUid == nil, 'stale unit field must clear')"
        , "assert(s.buildTarget == 42, 'a SURVIVING building ref must be kept')"
        , "assert(s.craftJob == nil, 'stale craftJob must be dropped whole')"
        , "assert(s.repairJob == nil, 'stale repairJob must be dropped whole')"
        , "assert(s.pickupOrder == nil, 'stale pickupOrder must clear')"
        , "assert(s.forageTarget == nil, 'stale ground forageTarget must clear')"
        , "assert(#s.forageLoot == 2 and s.forageLoot[1] == 7"
        , "       and s.forageLoot[2] == 8,"
        , "  'forageLoot keeps its resolvable gids, in order, as a dense array')"
        , "assert(s.foragePhase == 'collecting',"
        , "  'a still-populated forage list must not disturb its phase')"
        , "assert(s.harvestLoot == nil and s.harvestPhase == nil,"
        , "  'an EMPTIED harvest list leaves the same shape its own"
          <> " exhaustion path leaves')"
        , "-- EIGHT dangling declared edges: attackTargetUid,"
        , "-- craftJob.billId, craftJob.bid, repairJob.instanceId,"
        , "-- pickupOrder.gid, forageTarget.gid, forageLoot[2],"
        , "-- harvestLoot[1]. buildTarget resolved, so it counts nothing --"
        , "-- and neither do craftJob's still-valid siblings, removed only"
        , "-- because their enclosing job was dropped."
        , "assert(scrubbedCount() == 8,"
        , "  'the log line must count every dangling edge removed, got '"
        , "  .. tostring(scrubbedCount()))"
        ]

    it "resolves a PER-PAGE id against the OWNING unit's page: the same \
       \bill number really existing on another page resolves for the unit \
       \that lives there and for nobody else, and a ground item present \
       \only on page A is absent for a page B unit" $ runsOk $ lns
        [ prelude
        , "local ai = { [1] = { craftJob = { billId = 5, bid = 42,"
        , "                                   recipeId = 'x' },"
        , "                     pickupOrder = { gid = 7 } },"
        , "             [2] = { craftJob = { billId = 5, bid = 42,"
        , "                                   recipeId = 'x' },"
        , "                     pickupOrder = { gid = 7 } } }"
        , "R.reconcile(ai, { 1, 2 }, { 42 }, CTX)"
        , "assert(ai[1].craftJob ~= nil,"
        , "  'the page A unit keeps its own page A bill 5')"
        , "assert(ai[1].pickupOrder ~= nil,"
        , "  'the page A unit keeps its own page A ground item 7')"
        , "assert(ai[2].craftJob ~= nil,"
        , "  'the page B unit keeps page B bill 5 -- a DIFFERENT entity that"
          <> " happens to share the number')"
        , "assert(ai[2].pickupOrder == nil,"
        , "  'the page B unit must not resolve ground item 7, which exists"
          <> " only on page A')"
        , "assert(scrubbedCount() == 1, 'exactly one edge dangled')"
        ]

    it "drops a stale repairJob through unit_ai_repair.lua's own abort \
       \path -- handing an already-fetched item back to a technomule \
       \rather than leaving the job half-dismantled by a bare field \
       \assignment" $ runsOk $ lns
        [ prelude
        , "local ai = { [1] = { repairPhase = 'walking', repairJob = {"
        , "  instanceId = 901, itemFetched = true, defName = 'axe',"
        , "  recipeId = 'r' } } }"
        , "R.reconcile(ai, { 1 }, {}, CTX)"
        , "assert(ai[1].repairJob == nil, 'the stale job is gone')"
        , "assert(ai[1].repairPhase == nil,"
        , "  'releaseRepairJob clears the phase too -- a phase with no job"
          <> " is exactly the malformed leftover requirement 3 forbids')"
        , "assert(#MULE_MOVES == 1 and MULE_MOVES[1] == '1->77:901',"
        , "  'the fetched instance goes back to the mule, targeted by its"
          <> " own instance id')"
        ]

    it "withholds the mule search when the repairing unit is NOT on the \
       \active page: unit.getAllIds only ever lists the active page, so \
       \reconciling an off-page unit must take the existing no-mule \
       \fallback instead of transferring its item to a stranger" $
        runsOk $ lns
        [ prelude
        , "-- Unit 2 lives on page B; page A is active."
        , "local ai = { [2] = { repairPhase = 'walking', repairJob = {"
        , "  instanceId = 901, itemFetched = true, defName = 'axe',"
        , "  recipeId = 'r' } } }"
        , "R.reconcile(ai, { 2 }, {}, CTX)"
        , "assert(ai[2].repairJob == nil, 'the stale job is still dropped')"
        , "assert(ai[2].repairPhase == nil, 'and its phase with it')"
        , "assert(#MULE_MOVES == 0,"
        , "  'no item may be handed to an active-page mule on behalf of an"
          <> " off-page unit')"
        ]

    it "treats a MISSING or malformed reconciliation context as an engine \
       \fault and fails the reconcile visibly, rather than silently \
       \clearing every per-page reference or falling back to an \
       \active-page query that cannot answer for another page" $
        runsOk $ lns
        [ prelude
        , "local function reconcileWith(raw)"
        , "  return pcall(R.reconcile, { [1] = { pickupOrder = { gid = 7 } } },"
        , "               { 1 }, {}, raw)"
        , "end"
        , "assert(not reconcileWith(nil), 'an absent context must raise')"
        , "assert(not reconcileWith('nonsense'),"
        , "  'a non-table context must raise')"
        , "assert(not reconcileWith({ unitPage = {}, byPage ="
        , "    { craft_bill = {}, ground_item = {} } }),"
        , "  'a context missing item_instance must raise')"
        , "assert(not reconcileWith({ item_instance = {}, byPage ="
        , "    { craft_bill = {}, ground_item = {} } }),"
        , "  'a context missing unitPage must raise')"
        , "assert(not reconcileWith({ item_instance = {}, unitPage = {} }),"
        , "  'a context missing byPage must raise')"
        , "assert(not reconcileWith({ item_instance = {}, unitPage = {},"
        , "    byPage = { craft_bill = {} } }),"
        , "  'a context missing one per-page kind must raise')"
        ]

    it "treats a present-but-EMPTY context as the real value it is -- a \
       \restored session with no item instances, bills or ground items -- \
       \so every per-page reference dangles rather than being waved \
       \through" $ runsOk $ lns
        [ prelude
        , "local ai = { [1] = { pickupOrder = { gid = 7 },"
        , "                     craftJob = { billId = 5, bid = 42,"
        , "                                  recipeId = 'x' },"
        , "                     repairJob = { instanceId = 900 } } }"
        , "R.reconcile(ai, { 1 }, { 42 }, { item_instance = {}, unitPage = {},"
        , "  byPage = { craft_bill = {}, ground_item = {} } })"
        , "assert(ai[1].pickupOrder == nil and ai[1].craftJob == nil"
        , "       and ai[1].repairJob == nil,"
        , "  'an empty restored session resolves nothing')"
        , "assert(scrubbedCount() == 3, 'all three edges dangled')"
        ]

    it "refuses to run at all when a schema row names a release path no \
       \hook supplies, instead of silently degrading that drop to a bare \
       \field assignment" $ runsOk $ lns
        [ prelude
        , "local hooked = false"
        , "for _, row in ipairs(SCHEMA.REF_SCHEMA) do"
        , "  if row.drop ~= nil then hooked = true end"
        , "end"
        , "assert(hooked, 'the schema must declare at least one drop path,'"
        , "  .. ' or this case proves nothing')"
        , "assert(not pcall(R.reconcile, { [1] = {} }, { 1 }, {}, CTX, {}),"
        , "  'an empty hook table must be refused')"
        ]

    it "drops a nested claim or order holder whose OWN id subfield is \
       \absent -- a present treatClaim/deliveryClaim naming nobody resolves \
       \to nothing and would otherwise run a lock-state action against a \
       \nil target -- while leaving the one subfield that is legitimately \
       \absent in a live row, repairJob.bid before its walking phase, \
       \untouched" $ runsOk $ lns
        [ prelude
        , "local ai = { [1] = { treatClaim = {}, treatPending = {},"
        , "                     deliveryClaim = {}, deliveryPendingTarget = {},"
        , "                     pickupOrder = {},"
        , "                     forageTarget = { kind = 'ground', x = 1, y = 1 } },"
        , "             -- A real pre-walking repair job: instanceId set at"
        , "             -- creation, bid not yet resolved to a station."
        , "             [2] = { repairJob = { instanceId = 900, defName = 'axe' },"
        , "                     repairPhase = 'fetch_item' } }"
        , "R.reconcile(ai, { 1, 2 }, {}, CTX)"
        , "local one = ai[1]"
        , "assert(one.treatClaim == nil and one.treatPending == nil,"
        , "  'a treatment claim with no patient names nobody')"
        , "assert(one.deliveryClaim == nil and one.deliveryPendingTarget == nil,"
        , "  'a delivery claim with no building names nothing')"
        , "assert(one.pickupOrder == nil, 'a pickup order with no gid too')"
        , "assert(one.forageTarget == nil,"
        , "  'and a ground forage target with no gid')"
        , "assert(scrubbedCount() == 6,"
        , "  'each absent-id holder counts once, got ' .. tostring(scrubbedCount()))"
        , "local two = ai[2]"
        , "assert(two.repairJob ~= nil and two.repairPhase == 'fetch_item',"
        , "  'repairJob.bid is legitimately absent before the walking phase, "
          <> "so the job must survive untouched')"
        ]

    it "still prunes an orphan row and preserves aiState's table identity, \
       \which every unit_ai submodule holds a reference to" $ runsOk $ lns
        [ prelude
        , "local ai = { [1] = { currentAction = 'idle' },"
        , "             [9] = { currentAction = 'attack' } }"
        , "local identity = ai"
        , "R.reconcile(ai, { 1 }, {}, CTX)"
        , "assert(ai == identity, 'the singleton table must be reused in place')"
        , "assert(ai[1] ~= nil, 'a survivor row is kept')"
        , "assert(ai[9] == nil, 'a row whose unit did not survive is pruned')"
        ]

-- | Register one real Lua module against @ls@, its module table produced
--   by evaluating @chunk@ -- the same fixture shape
--   "Test.Headless.Lua.DebugQueue" uses, and the only difference from a
--   script loaded off disk is where the source came from.
registerLuaModule ∷ LuaBackendState → Word32 → FilePath → Text → IO ()
registerLuaModule ls sid path chunk = do
    ref ← Lua.runWith (lbsLuaState ls) $ do
        status ← Lua.dostring (TE.encodeUtf8 chunk)
                    ∷ Lua.LuaE Lua.Exception Lua.Status
        case status of
            Lua.OK → Lua.ref Lua.registryindex
            _      → error ("fixture chunk failed to load: " <> path)
    atomically $ modifyTVar' (lbsScripts ls) $ Map.insert sid LuaScript
        { scriptId        = sid
        , scriptPath      = path
        , scriptTickRate  = 1000000
        , scriptNextTick  = 1000000
        , scriptModuleRef = ref
        , scriptPaused    = False
        }

-- | A backend carrying one module whose @onSaveLoaded@ flattens its
--   THIRD argument into a global report string, so the test can read
--   back exactly what crossed the boundary.
newContextReportingBackend ∷ EngineEnv → IO LuaBackendState
newContextReportingBackend env = do
    ls0 ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                 (assetPoolRef env) (nextObjectIdRef env)
                                 (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls0) env ls0 stateRef
    privateQueue ← atomically newTQueue
    let ls = ls0 { lbsDebugQueue = privateQueue }
    _ ← Lua.runWith (lbsLuaState ls)
            (Lua.dostring "__ctxReport = 'never ran'"
                ∷ Lua.LuaE Lua.Exception Lua.Status)
    registerLuaModule ls 1 "scripts/ctx_probe.lua" $ lns
        [ "local function size(t)"
        , "  if type(t) ~= 'table' then return 'NOTATABLE' end"
        , "  local n = 0"
        , "  for _ in pairs(t) do n = n + 1 end"
        , "  return tostring(n)"
        , "end"
        , "-- A page with no set at all must not be an indexing error: an"
        , "-- empty session legitimately names no pages."
        , "local function has(byPage, page, id)"
        , "  local set = byPage[page]"
        , "  return tostring(set ~= nil and set[id] == true)"
        , "end"
        , "return { onSaveLoaded = function(units, buildings, ctx)"
        , "  if type(ctx) ~= 'table' then"
        , "    __ctxReport = 'ctx=' .. type(ctx); return"
        , "  end"
        , "  __ctxReport = table.concat({"
        , "    'units=' .. #units, 'buildings=' .. #buildings,"
        , "    'iis=' .. size(ctx.item_instance),"
        , "    'ii900=' .. tostring(ctx.item_instance[900] == true),"
        , "    'ii901=' .. tostring(ctx.item_instance[901] == true),"
        , "    'page1=' .. tostring(ctx.unitPage[1]),"
        , "    'page2=' .. tostring(ctx.unitPage[2]),"
        , "    'billsA=' .. size(ctx.byPage.craft_bill.A),"
        , "    'billA5=' .. has(ctx.byPage.craft_bill, 'A', 5),"
        , "    'billB5=' .. has(ctx.byPage.craft_bill, 'B', 5),"
        , "    'groundA=' .. size(ctx.byPage.ground_item.A),"
        , "    'groundB=' .. size(ctx.byPage.ground_item.B),"
        , "  }, '|')"
        , "end }"
        ]
    pure ls

-- | Read the report string the fixture module wrote.
readReport ∷ LuaBackendState → IO Text
readReport ls = Lua.runWith (lbsLuaState ls) $ do
    _ ← Lua.getglobal "__ctxReport" ∷ Lua.LuaE Lua.Exception Lua.Type
    raw ← Lua.tostring (-1)
    Lua.pop 1
    pure (maybe "<nil>" TE.decodeUtf8Lenient raw)

-- | The known-entity sets a two-page restored session would produce.
--   Unit 1 lives on page A, unit 2 on page B; BOTH pages carry a bill
--   numbered 5, and page B has no ground items at all.
twoPageEntities ∷ KnownEntities
twoPageEntities = KnownEntities
    { keUnits             = HS.fromList [1, 2]
    , keBuildings         = HS.fromList [42]
    , keBillsByPage       = HM.fromList
        [ (WorldPageId "A", HS.fromList [5])
        , (WorldPageId "B", HS.fromList [5]) ]
    , keItemInstances     = HS.fromList [900]
    , keGroundItemsByPage = HM.fromList
        [ (WorldPageId "A", HS.fromList [7, 8])
        , (WorldPageId "B", HS.empty) ]
    , keLocationsByPage   = HM.empty
    , keUnitPage          = HM.fromList
        [ (1, WorldPageId "A"), (2, WorldPageId "B") ]
    , keNextUnitId        = 3
    , keNextBuildingId    = 43
    , keNextItemId        = 901
    }

envSpec ∷ SpecWith EngineEnv
envSpec = describe "unit AI reconciliation boundary (engine broadcast, #1589)" $ do
    it "projects exactly the three identity scopes the survivor arrays \
       \cannot answer out of the load's own KnownEntities, and no copy \
       \of the units and buildings onSaveLoaded already receives" $
        \_env → do
            let rc = loadReconcileContextFrom twoPageEntities
            L.sort (lrcItemInstances rc) `shouldBe` [900]
            L.sort (lrcUnitPages rc) `shouldBe` [(1, "A"), (2, "B")]
            L.sort (map (fmap L.sort) (lrcBillsByPage rc))
                `shouldBe` [("A", [5]), ("B", [5])]
            L.sort (map (fmap L.sort) (lrcGroundItemsByPage rc))
                `shouldBe` [("A", [7, 8]), ("B", [])]

    it "delivers that context to onSaveLoaded as an APPENDED third \
       \argument, with the session-global item instances flat, unit-page \
       \ownership intact, and each page's bills and ground items kept \
       \apart -- so two pages' equally numbered bill 5 stay two entities" $
        \env → do
            ls ← newContextReportingBackend env
            stateRef ← newIORef ThreadRunning
            processLuaMsg env ls stateRef
                (LuaSaveLoaded 4242 [1, 2] [42]
                    (loadReconcileContextFrom twoPageEntities))
            report ← readReport ls
            report `shouldBe` T.intercalate "|"
                [ "units=2", "buildings=1", "iis=1"
                , "ii900=true", "ii901=false"
                , "page1=A", "page2=B"
                , "billsA=1", "billA5=true", "billB5=true"
                , "groundA=2", "groundB=0" ]

    it "still delivers all four tables for a session that genuinely holds \
       \nothing, so a reconciling module can tell an EMPTY restored \
       \session apart from a context the engine failed to supply" $
        \env → do
            ls ← newContextReportingBackend env
            stateRef ← newIORef ThreadRunning
            processLuaMsg env ls stateRef
                (LuaSaveLoaded 4243 [] [] emptyLoadReconcileContext)
            report ← readReport ls
            report `shouldBe` T.intercalate "|"
                [ "units=0", "buildings=0", "iis=0"
                , "ii900=false", "ii901=false"
                , "page1=nil", "page2=nil"
                , "billsA=NOTATABLE", "billA5=false", "billB5=false"
                , "groundA=NOTATABLE", "groundB=NOTATABLE" ]
