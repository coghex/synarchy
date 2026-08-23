-- | The "session teardown clears Lua entity state" gate (#1610): Exit to
--   Menu ends a play session WITHOUT loading anything, so none of
--   @saveModules@' load-time machinery fires -- and every module holding
--   session-scoped Lua state had to be hand-listed in
--   @pauseMenu.onExitToMenu@ as its own @pcall@ clear. Three were
--   (#82 build-tool placement, #102 mine-tool anchor, #1014 transfer
--   session); @unit_ai@'s @aiState@ and @building_spawn@'s @state@ never
--   were, so their rows accumulated for the life of the process.
--
--   @scripts/lib/session_teardown.lua@ is that boundary declared once.
--   What this gate pins:
--
--     * all FIVE registrants clear on the real @onExitToMenu@ path, and
--       the two entity tables are emptied IN PLACE (same table object);
--     * the migrated three still reach a LIVE session -- the mine
--       anchor's engine-side clear lands on a world that still exists,
--       and the transfer session still stops both units it held -- which
--       is what the "before @world.destroyAll@" ordering buys
--       (@docs\/engine_contracts.md@, "Player transfers");
--     * the DRAIN WINDOW: @world.destroyAll@ only enqueues, so ordinary
--       updates keep seeing the destroyed session's entities for some
--       ticks afterwards and must not rebuild the rows just cleared;
--     * the next session activation releases that latch -- through the
--       real @uiManager.showMenu@ transition, and only for the two
--       gameplay views;
--     * one failing callback suppresses neither a later callback nor the
--       world teardown; and
--     * a save load fires NO session-teardown callback at all: it keeps
--       its own @applyAll@ reset hooks and @onSaveLoaded@ broadcast, and
--       this boundary adds no second clear to it.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.UnitAiLoadReset":
--   each 'it' runs one self-contained chunk via 'Lua.dostring' in a fresh
--   interpreter, asserting inside Lua via @assert()@.
--
--   The shipping code here is @pause_menu.lua@'s real @onExitToMenu@, the
--   real registry, all five real registrants (each registering through
--   its own real init\/module scope), @unitAi.update@ and
--   @buildingSpawn.update@, and the real
--   @snapshotAll -> prepareLoad -> applyAll@ load lifecycle. Only the
--   engine API surface those reach is stubbed.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "session teardown clears Lua entity state"'@.
module Test.Headless.Lua.SessionTeardown (spec) where

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

-- | Stubbed engine globals, the five real registrants wired up through
--   their own real init\/module-scope registrations, and @arm()@, which
--   puts every one of them into the state Exit to Menu has to clear.
--
--   @UNITS@ \/ @BUILDINGS@ are what the entity queries report and are
--   deliberately left populated across the teardown -- that is the drain
--   window, where @UnitClearAll@\/@BuildingClearAll@ are still queued.
--   @AWARE@ feeds @unit_ai_locations.ingestAwareness@, the row-creating
--   call @unitAi.update@ makes BEFORE its pause guard; @probe_unit@ is a
--   def no action registry knows, so @tickOne@ falls straight through
--   and @ingestAwareness@ is the only thing that can touch @aiState@.
prelude ∷ Text
prelude = lns
    [ "PAGE, NOW = 'main_world', 1000"
    , "DESTROYED, HIDDEN, PAUSED = false, {}, true"
    , "UNITS, BUILDINGS, AWARE = {}, {}, {}"
    , "MINE_CLEARS, STOPPED, ERRORS = 0, {}, {}"
    , "local function noop() end"
    , "engine = { logInfo = noop, logWarn = noop, logDebug = noop,"
    , "  logError = function(m) ERRORS[#ERRORS + 1] = m end,"
    , "  gameTime = function() return NOW end,"
    , "  isPaused = function() return PAUSED end,"
    , "  setPaused = function(b) PAUSED = b and true or false end,"
    , "  emitEvent = noop, emitEventForUnit = noop,"
    , "  getTextWidth = function() return 0 end,"
    , "  loadYaml = function() return {} end }"
    , "world = { destroyAll = function() DESTROYED = true end,"
    , "  hide = function(id) HIDDEN[#HIDDEN + 1] = id end,"
    , "  show = noop, destroy = noop,"
    , "  clearMineAnchor = function(w)"
    , "    assert(not DESTROYED, 'mine_tool.cancel reached the engine AFTER"
      <> " world.destroyAll -- the boundary must run while the session is live')"
    , "    MINE_CLEARS = MINE_CLEARS + 1 end,"
    , "  getLocationAwareness = function() return AWARE end,"
    , "  getActiveWorldId = function() return PAGE end,"
    , "  getToolMode = function() return 'mine' end }"
    , "unit = { getAllIds = function() return UNITS end,"
    , "  getInfo = function() return"
    , "    { defName = 'probe_unit', gridX = 0, gridY = 0 } end,"
    , "  exists = function() return true end,"
    , "  stop = function(u)"
    , "    assert(not DESTROYED, 'transfer_session released a unit AFTER"
      <> " world.destroyAll')"
    , "    STOPPED[#STOPPED + 1] = u end,"
    , "  moveTo = noop, getStat = function() return 1.0 end,"
    , "  transferContract = function() return {} end }"
    , "building = { getActiveIds = function() return BUILDINGS end,"
    , "  getActivity = function() return 'built' end,"
    , "  getInfo = function() return { defName = 'acolyte_portal',"
    , "    gridX = 0, gridY = 0, page = PAGE } end,"
    , "  getSpawnRemaining = function() return 0 end,"
    , "  setSpawnRemaining = noop,"
    , "  areMaterialsSatisfied = function() return false end,"
    , "  clearGhost = noop }"
    , "construction = { clearAnchor = noop, setLineMode = noop }"
    , "camera = setmetatable({},"
    , "  { __index = function() return function() return 0 end end })"
    , "UI = setmetatable({},"
    , "  { __index = function() return function() return 1 end end })"
    , "ST   = require('scripts.lib.session_teardown')"
    , "PM   = require('scripts.pause_menu')"
    , "AI   = require('scripts.unit_ai'); AI.init()"
    , "CORE = require('scripts.unit_ai_core')"
    , "BS   = require('scripts.building_spawn'); BS.init()"
    , "BT   = require('scripts.build_tool'); BT.init()"
    , "MT   = require('scripts.mine_tool')"
    , "TS   = require('scripts.transfer_session'); TS.init()"
    , "SAVE = require('scripts.lib.save_modules')"
    , "AISTATE, BSSTATE = CORE.aiState, BS.state"
    , "-- Put every registrant into the state Exit to Menu has to clear."
    , "-- building_spawn's row comes from its OWN real update, not a hand"
    , "-- -written table: ensureState is the only thing that makes one."
    , "function arm()"
    , "  PAUSED = false"
    , "  UNITS, BUILDINGS = { 7, 9 }, { 11, 12 }"
    , "  AWARE = { { uid = 7, page = PAGE, instance_id = 1, gx = 1, gy = 1 },"
    , "            { uid = 9, page = PAGE, instance_id = 1, gx = 1, gy = 1 } }"
    , "  AI.update(0.1)"
    , "  BS.update(0.1)"
    , "  BT.state.mode   = 'placement'"
    , "  BT.state.target = { kind = 'building', name = 'hut' }"
    , "  BT.state.anchor = { gx = 2, gy = 2 }"
    , "  MT.hud    = { worldId = PAGE }"
    , "  MT.anchor = { 5, 5 }"
    , "  TS.active = { id = 1, source = { id = 7, kind = 'unit' },"
    , "                destination = { id = 9, kind = 'unit' } }"
    , "  assert(AISTATE[7] ~= nil and AISTATE[9] ~= nil, 'arm: no AI rows')"
    , "  assert(BSSTATE[11] ~= nil and BSSTATE[12] ~= nil, 'arm: no spawn rows')"
    , "end"
    , "function count(t)"
    , "  local n = 0; for _ in pairs(t) do n = n + 1 end; return n"
    , "end"
    ]

spec ∷ Spec
spec = describe "session teardown clears Lua entity state (#1610)" $ do
    it "clears all five registrants on the real Exit to Menu path, in\
           \ place and while the session is still live" $
        runsOk $ prelude <> "\n" <> lns
            [ "assert(table.concat(ST.registeredIds(), ',') =="
            , "       'build_tool,building_spawn,mine_tool,transfer_session,unit_ai',"
            , "       'unexpected registrant set: '"
            , "       .. table.concat(ST.registeredIds(), ','))"
            , "arm()"
            , "PM.onExitToMenu()"
            , "-- The two entity tables (requirements 1 and 2)."
            , "assert(count(AISTATE) == 0, 'aiState kept '"
            , "       .. count(AISTATE) .. ' row(s) through Exit to Menu')"
            , "assert(count(BSSTATE) == 0, 'building_spawn state kept '"
            , "       .. count(BSSTATE) .. ' row(s) through Exit to Menu')"
            , "-- Requirement 5: emptied IN PLACE, never rebound, so every"
            , "-- module holding a direct reference still sees this table."
            , "assert(CORE.aiState == AISTATE, 'aiState was rebound')"
            , "assert(AI.aiState == AISTATE, 'unit_ai.aiState was rebound')"
            , "assert(BS.state == BSSTATE, 'building_spawn state was rebound')"
            , "-- The three migrated clears (#82 / #102 / #1014)."
            , "assert(BT.state.mode == 'off', 'build placement survived: '"
            , "       .. tostring(BT.state.mode))"
            , "assert(BT.state.target == nil and BT.state.anchor == nil,"
            , "       'build target/anchor survived')"
            , "assert(MT.anchor == nil, 'mine anchor survived')"
            , "assert(MINE_CLEARS == 1, 'the engine-side mine anchor was not"
              <> " cleared exactly once: ' .. MINE_CLEARS)"
            , "assert(TS.get() == nil, 'transfer session survived')"
            , "-- Coupled teardown, not a bare M.active = nil: BOTH held"
            , "-- units are stopped, which only works while they exist."
            , "assert(#STOPPED == 2, 'transfer session released '"
            , "       .. #STOPPED .. ' unit(s), expected 2')"
            , "assert(DESTROYED, 'world.destroyAll never ran')"
            , "-- Requirement 6: no save component unregistered, no reset"
            , "-- hook dropped -- this is not a script shutdown."
            , "local ids = {}"
            , "for _, c in ipairs(SAVE.describeAll()) do ids[c.id] = true end"
            , "assert(ids.unit_ai and ids.building_spawn,"
            , "       'a save component was unregistered by the teardown')"
            , "assert(SAVE.resetHooks.unit_ai_claims ~= nil"
            , "       and SAVE.resetHooks.transfer_session ~= nil,"
            , "       'a load reset hook was dropped by the teardown')"
            ]
    it "keeps both tables empty through the manager-clear drain window,\
           \ while the entity queries still report the destroyed session" $
        runsOk $ prelude <> "\n" <> lns
            [ "arm()"
            , "PM.onExitToMenu()"
            , "-- world.destroyAll only ENQUEUES WorldDestroyAll, whose handler"
            , "-- later queues UnitClearAll/BuildingClearAll onto two further"
            , "-- worker queues. Until those drain, these queries still answer"
            , "-- with the destroyed session -- exactly what is stubbed here."
            , "assert(#UNITS == 2 and #BUILDINGS == 2,"
            , "       'the fixture must still report the old entities')"
            , "assert(ST.isTornDown(), 'the teardown latch is not set')"
            , "for _ = 1, 5 do AI.update(0.1); BS.update(0.1) end"
            , "assert(count(AISTATE) == 0, 'an ordinary update rebuilt '"
            , "       .. count(AISTATE) .. ' AI row(s) after Exit to Menu')"
            , "assert(count(BSSTATE) == 0, 'an ordinary update rebuilt '"
            , "       .. count(BSSTATE) .. ' spawn row(s) after Exit to Menu')"
            ]
    it "lets the next session activation create fresh rows again" $
        runsOk $ prelude <> "\n" <> lns
            [ "arm()"
            , "PM.onExitToMenu()"
            , "AI.update(0.1); BS.update(0.1)"
            , "assert(count(AISTATE) == 0 and count(BSSTATE) == 0,"
            , "       'the latch did not hold')"
            , "-- The same updates, the same visible entities -- only the"
            , "-- activation differs, so the latch is what held them off."
            , "ST.beginSession()"
            , "assert(not ST.isTornDown(), 'beginSession did not release')"
            , "AI.update(0.1); BS.update(0.1)"
            , "assert(AISTATE[7] ~= nil and AISTATE[9] ~= nil,"
            , "       'a new session cannot create AI rows')"
            , "assert(BSSTATE[11] ~= nil and BSSTATE[12] ~= nil,"
            , "       'a new session cannot create spawn rows')"
            ]
    it "releases the latch from the real gameplay-view transition and\
           \ from a published load, and from no other menu" $
        runsOk $ prelude <> "\n" <> lns
            [ "local UIM = require('scripts.ui_manager')"
            , "-- The full ui_manager boot cannot run headless (it needs a GPU"
            , "-- font atlas), so each transition is expected to fail further"
            , "-- down. The latch release happens at the top of showMenu, which"
            , "-- is the wiring under test here."
            , "ST.runAll()"
            , "pcall(UIM.showMenu, 'main')"
            , "assert(ST.isTornDown(), 'a non-gameplay menu released the latch')"
            , "pcall(UIM.showMenu, 'settings')"
            , "assert(ST.isTornDown(), 'a non-gameplay menu released the latch')"
            , "pcall(UIM.showMenu, 'world_view')"
            , "assert(not ST.isTornDown(),"
            , "       'showMenu(\"world_view\") did not release the latch')"
            , "ST.runAll()"
            , "pcall(UIM.showMenu, 'test_arena_view')"
            , "assert(not ST.isTornDown(),"
            , "       'showMenu(\"test_arena_view\") did not release the latch')"
            , "-- A published load is a live session too, and a load fired"
            , "-- from the debug console while sitting in the menu after an"
            , "-- Exit to Menu never passes through showMenu at all."
            , "ST.runAll()"
            , "pcall(UIM.onSaveLoaded, {}, {})"
            , "assert(not ST.isTornDown(),"
            , "       'a published load left the teardown latch set')"
            ]
    it "runs every remaining clear and still destroys the world when one\
           \ callback fails" $
        runsOk $ prelude <> "\n" <> lns
            [ "RAN = {}"
            , "-- Ids run ascending, so this one fails BEFORE all five real"
            , "-- clears and before the probe that has to still run."
            , "ST.register('a_failing', function() error('boom') end)"
            , "ST.register('z_probe', function() RAN[#RAN + 1] = 'z' end)"
            , "arm()"
            , "PM.onExitToMenu()"
            , "assert(#RAN == 1, 'a later callback was suppressed by the"
              <> " failing one')"
            , "assert(#ERRORS == 1, 'the failure was not reported once: '"
            , "       .. #ERRORS)"
            , "assert(count(AISTATE) == 0 and count(BSSTATE) == 0,"
            , "       'an entity table survived alongside a failing callback')"
            , "assert(BT.state.mode == 'off' and MT.anchor == nil"
            , "       and TS.get() == nil,"
            , "       'a migrated clear was suppressed by the failing one')"
            , "assert(DESTROYED,"
            , "       'a failing callback stopped the world teardown')"
            ]
    it "fires no session-teardown callback on a save load" $
        runsOk $ prelude <> "\n" <> lns
            [ "LOADRAN = 0"
            , "ST.register('z_probe', function() LOADRAN = LOADRAN + 1 end)"
            , "arm()"
            , "-- The real load lifecycle: snapshotAll -> prepareLoad -> applyAll."
            , "local snap = SAVE.snapshotAll()"
            , "assert(snap.ok, 'snapshotAll failed: ' .. tostring(snap.error))"
            , "local prep = SAVE.prepareLoad(snap.components,"
            , "  { unit = { [7] = true, [9] = true },"
            , "    building = { [11] = true, [12] = true } })"
            , "assert(prep.ok, 'prepareLoad failed: '"
            , "       .. tostring(prep.errors and prep.errors[1]))"
            , "SAVE.applyAll()"
            , "-- Requirement 4: the boundary is not part of the load"
            , "-- transaction, so nothing is cleared a second time."
            , "assert(LOADRAN == 0, 'a load fired ' .. LOADRAN"
            , "       .. ' session-teardown callback(s)')"
            , "assert(not ST.isTornDown(), 'a load set the teardown latch')"
            , "-- The load boundary's own behaviour is unchanged: the restored"
            , "-- rows are live, and transfer_session's reset hook still fired."
            , "assert(AISTATE[7] ~= nil and BSSTATE[11] ~= nil,"
            , "       'the load did not restore the component rows')"
            , "assert(TS.get() == nil,"
            , "       \"transfer_session's load reset hook did not fire\")"
            , "assert(#STOPPED == 0, 'the load reset stopped a unit -- its uids"
              <> " are stale and it must not')"
            ]
