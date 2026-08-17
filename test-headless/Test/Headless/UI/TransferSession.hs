-- | Mode A's escort session (#1250, epic #1013 slice UIT-3B): the
--   session lifecycle, the hold, the two flanking panes, and the
--   immediate commits their rows perform.
--
--   Registered under a describe beginning "Transfer context menu" so
--   @--match "Transfer context menu"@ reaches this alongside
--   'Test.Headless.UI.TransferContextMenu' (the session's entry point)
--   and 'Test.Headless.UI.TransferGestures' (Mode B's own gestures),
--   which is the gate the issue names.
--
--   Like the Mode B fixture, and for the same reason, this builds a
--   REAL scene rather than stubbing the contract: Mode A's whole claim
--   is that an adjacent unit moves EXACT INSTANCES immediately, so a
--   spec that stubbed @unit.commitTransfer@ would prove only that a
--   callback ran. Real units, a real building, real item instances and
--   a real page; the production Lua drives itself; and every assertion
--   reads back through the engine's own state (@unit.getInventory@,
--   @unit.transferEndpointInfo@) or the window manager's dump oracle.
--
--   Distance is what separates the two modes, so it is controlled
--   explicitly here: a scenario that must WALK starts its carrier away
--   from the endpoint, and one that must COMMIT starts it adjacent.
--   Nothing in this fixture ticks a simulation, so a unit only ever
--   moves when the scenario moves it — which is what makes "it walked"
--   and "it arrived" two separately observable facts.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Transfer context menu (Mode A"'@.
module Test.Headless.UI.TransferSession (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.IORef (newIORef, writeIORef, atomicModifyIORef')
import Building.Types
    (BuildingId(..), BuildingInstance(..), BuildingManager(..)
    , emptyBuildingManager)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (vcUIScale)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemInstance(..), emptyItemManager)
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Unit.TransferApi
    (mkBuilding, mkItem, mkUnit, minimalDef, storageDef)
import UI.Types (emptyUIPageManager)
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState)

-- * Fixture ids

-- | uid 1 — the escort. Carries the inventory every "Store" scenario
--   moves out of.
carrierUid ∷ UnitId
carrierUid = UnitId 1

-- | uid 2 — a second acolyte, used as a UNIT destination (requirement
--   6) and as the unit a replacement session escorts instead.
matesUid ∷ UnitId
matesUid = UnitId 2

-- | The hold the carrier walks to. Deliberately 2x2 so every distance
--   in this gate is measured against a real FOOTPRINT: an
--   anchor-to-anchor rule would call the carrier adjacent one tile too
--   late on the far side.
holdBid ∷ BuildingId
holdBid = BuildingId 7

-- | A second, nearly-full hold — the partial-batch fixture.
tinyBid ∷ BuildingId
tinyBid = BuildingId 8

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "transfer_session_page"

-- * Fixture

-- | THREE identical rations, which the shared widget merges into one
--   row, plus one rope, which stays a single-instance row. That pair is
--   what separates "1 and all" from "1 alone" without a second scene.
carrierInventory ∷ [ItemInstance]
carrierInventory =
    [ mkItem "ration" 101 0.5
    , mkItem "ration" 102 0.5
    , mkItem "ration" 103 0.5
    , mkItem "rope"   110 2.0
    ]

-- | The hold's own storage, mirroring that shape from the other side.
holdStorage ∷ [ItemInstance]
holdStorage =
    [ mkItem "steel_bar" 201 2.0
    , mkItem "steel_bar" 202 2.0
    , mkItem "crowbar"   210 3.0
    ]

onPage ∷ UnitInstance → UnitInstance
onPage u = u { uiPage = fixturePage }

-- | Place the carrier at a chosen tile. Nothing here ticks a
--   simulation, so this is how a scenario "walks" a unit: the AI action
--   issues a real @unit.moveTo@ and the scenario decides whether the
--   step it asked for happened.
placeUnit ∷ EngineEnv → UnitId → (Float, Float) → IO ()
placeUnit env uid (gx, gy) =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.adjust
                (\u → u { uiGridX = gx, uiGridY = gy }) uid (umInstances um) }
        , ())

-- | Fill BOTH endpoints past a pane's 10-row cap, so each pane renders
--   its tallest possible panel. The minimum-viewport case needs that:
--   a short pair fits almost anywhere, and it is the TALL one that has
--   nowhere left to go once a toolbar reserves a band (#1250 review
--   round 4).
stockTall ∷ EngineEnv → IO ()
stockTall env = do
    let many prefix n =
            [ mkItem (prefix <> "_" <> T.pack (show i))
                     (fromIntegral (900 + i)) 0.1
            | i ← [1 .. n ∷ Int] ]
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.adjust
                (\u → u { uiInventory = many "carried" 14 })
                carrierUid (umInstances um) }, ())
    atomicModifyIORef' (buildingManagerRef env) $ \bm →
        (bm { bmInstances = HM.adjust
                (\b → b { biStorage = many "stored" 16 })
                holdBid (bmInstances bm) }, ())

-- | The carrier starts FAR from the hold (a walk is required), the mate
--   stands beside the hold, and both holds sit apart so no scenario can
--   be adjacent to one by accident.
resetWorld ∷ EngineEnv → IO ()
resetWorld env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) WorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (itemManagerRef env) emptyItemManager
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("acolyte", minimalDef "acolyte" "Acolyte")
            , ("wolf", minimalDef "wolf" "Wolf") ]
        , umInstances = HM.fromList
            [ (carrierUid, onPage
                  (mkUnit "acolyte" FactionPlayer (10, 10) 100
                          carrierInventory []))
            , (matesUid, onPage
                  (mkUnit "acolyte" FactionPlayer (60, 60) 100 [] [])) ]
        }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList
            [ ("cargo_hold", storageDef "cargo_hold" "Cargo Hold" (2, 2) 0 200)
            , ("tiny_hold",  storageDef "tiny_hold"  "Tiny Hold"  (1, 1) 0 1.2) ]
        , bmInstances = HM.fromList
            [ (holdBid, (mkBuilding "cargo_hold" (40, 40) (2, 2) holdStorage)
                            { biPage = fixturePage })
            , (tinyBid, (mkBuilding "tiny_hold" (80, 80) (1, 1) [])
                            { biPage = fixturePage }) ]
        }

withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
withSharedFixture action = withHeadlessEngine $ \env → do
    ls ← newBareLuaBackend env
    action (env, ls)

-- | Every case starts from a clean page manager, a clean Lua module
--   table and the world above. Wiping @package.loaded@ is what keeps
--   the container window's stack, the session singleton and the AI
--   action's own per-unit state from leaking between cases.
resetFixture ∷ EngineEnv → LuaBackendState → IO ()
resetFixture env ls = do
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (framebufferSizeRef env) (1280, 720)
    atomicModifyIORef' (videoConfigRef env) $ \c → (c { vcUIScale = 1.0 }, ())
    resetWorld env
    cleared ← evalOk ls
        "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end; return true"
    cleared `shouldBe` "true"
    _ ← evalOk ls sceneLua
    pure ()

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls src = do
    r ← executeDebugLua (lbsLuaState ls) src
    r `shouldNotSatisfy` isLuaError
    pure r

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

tshow ∷ Int → Text
tshow = T.pack . show

-- * The scene's Lua half

-- | Stands the container window up on a real page and installs the
--   observers every case reads back through.
--
--   Three engine verbs are WRAPPED rather than replaced, so each still
--   does its real work and the scene additionally learns it happened:
--   @camera.goToTile@ (the one snap, D-4),
--   @building.refreshContainerKnowledge@ (the one reveal, requirement
--   3) and @unit.moveTo@ (the approach). Replacing them would make
--   "exactly once" a claim about a stub.
sceneLua ∷ Text
sceneLua = luaLines
    [ "_G.__page = UI.newPage('escort_base', 'overlay');"
    , "UI.showPage(_G.__page);"
    , "local cip = require('scripts.cargo_inventory_panel');"
    , "cip.setup({page = _G.__page, fbW = 1280, fbH = 720,"
    , "           boxTexSet = 1, menuFont = 1});"
    -- The unit AI is not loaded here; unit_ai_transfer's module scope
    -- expects the singleton to exist (and unit_ai_escort reaches that
    -- module for the shared approach), exactly as the sibling AI specs
    -- arrange it.
    , "package.loaded['scripts.unit_ai'] = {};"
    -- Wrapped ONCE for the whole fixture, not once per case: `_G`
    -- survives the package.loaded wipe, so re-wrapping here would nest
    -- each case's wrapper inside the last one's and count a single call
    -- once per case that had run before it. That made every "exactly
    -- once" assertion depend on hspec's randomized ordering.
    , "_G.__events = {};"
    , "if not _G.__wrapped then"
    , "  _G.__wrapped = true;"
    , "  local realGoTo = camera.goToTile;"
    , "  camera.goToTile = function(x, y)"
    , "    _G.__snaps = _G.__snaps + 1; _G.__lastSnap = {x = x, y = y};"
    , "    return realGoTo(x, y) end;"
    , "  local realReveal = building.refreshContainerKnowledge;"
    , "  building.refreshContainerKnowledge = function(bid)"
    , "    _G.__reveals = _G.__reveals + 1; _G.__lastReveal = bid;"
    , "    return realReveal(bid) end;"
    , "  local realMoveTo = unit.moveTo;"
    , "  unit.moveTo = function(uid, x, y, sp)"
    , "    _G.__moves = _G.__moves + 1; _G.__lastMove = {uid = uid, x = x, y = y};"
    , "    return realMoveTo(uid, x, y, sp) end;"
    , "  local realStop = unit.stop;"
    , "  unit.stop = function(uid)"
    , "    _G.__stops = _G.__stops + 1; _G.__lastStop = uid;"
    , "    return realStop(uid) end;"
    , "  local realEmitFor = engine.emitEventForUnit;"
    , "  engine.emitEventForUnit = function(cat, text, uid, gx, gy)"
    , "    _G.__events[#_G.__events + 1] = cat .. '|' .. text;"
    , "    return realEmitFor(cat, text, uid, gx, gy) end;"
    , "end;"
    , "_G.__snaps = 0; _G.__reveals = 0; _G.__moves = 0; _G.__stops = 0;"
    -- Capture whatever menu a row gesture opens, without ever
    -- serializing a Lua function back to Haskell.
    , "local cm = require('scripts.ui.context_menu');"
    , "_G.__captured = nil;"
    , "cm.show = function(items) _G.__captured = items end;"
    -- Player-visible reporting: the escort's warnings and events are
    -- what requirement 4's partial-batch report reaches the player
    -- through, so the scene records them rather than trusting a count.
    , "_G.__eventText = function()"
    , "  return table.concat(_G.__events, '\\n') end;"
    -- One AI tick of the escort action for `uid`: score, then execute
    -- if it won anything at all. `_G.__ai[uid]` is that unit's aiState
    -- row, which the real action reads and writes.
    , "_G.__ai = {};"
    , "_G.__tick = function(uid)"
    , "  local tr = require('scripts.unit_ai_escort');"
    , "  _G.__ai[uid] = _G.__ai[uid] or {};"
    , "  local s = _G.__ai[uid];"
    , "  local u = tr.escortUtility(uid, s);"
    , "  if u > -math.huge then tr.escortExecute(uid, s) end;"
    , "  return u end;"
    -- Open a session and walk it to arrival, however many ticks that
    -- takes, teleporting is NOT done here: the scenario decides where
    -- the unit stands and this only drives the action.
    , "_G.__session = function() return require('scripts.transfer_session') end;"
    , "_G.__phase = function()"
    , "  local s = _G.__session().get(); return s and s.phase or 'none' end;"
    -- Right-click the row standing for `defName` in pane `paneKey`,
    -- through the widget's own routing on that pane's real list.
    , "_G.__rowMenu = function(paneKey, defName)"
    , "  local il = require('scripts.ui.item_list');"
    , "  local level = cip.getLevel(1); if not level then return nil end;"
    , "  local pane = cip.getPane(level, paneKey); if not pane then return nil end;"
    , "  local hit; for _, r in ipairs(il.getRows(pane.listId)) do"
    , "    if r.item.defName == defName then hit = r.hitId end end;"
    , "  if not hit then return nil end;"
    , "  _G.__captured = nil;"
    , "  il.handleCallback('onItemListRightClick', hit);"
    , "  return _G.__captured end;"
    , "_G.__labels = function(items)"
    , "  local out = {}; for i, e in ipairs(items or {}) do out[i] = e.label end;"
    , "  return table.concat(out, '|') end;"
    , "_G.__fire = function(items, label)"
    , "  for _, e in ipairs(items or {}) do"
    , "    if e.label == label and e.callback then e.callback(); return true end"
    , "  end; return false end;"
    -- The exact instance ids an endpoint holds, joined: an EMPTY Lua
    -- array serializes to a JSON OBJECT, not [], so "moved everything"
    -- and "moved nothing" would otherwise decode through different
    -- shapes.
    -- The header labels' OWN rasterised font sizes, which is where a
    -- pane's effective scale actually lands: label.new stores
    -- floor(baseFontSize * uiscale), so a header drawn at the
    -- configured scale inside a fitted pane is visible here as a size
    -- the fitted box never reserved room for.
    , "_G.__headerFonts = function(paneKey)"
    , "  local c = require('scripts.cargo_inventory_panel');"
    , "  local l = require('scripts.ui.label');"
    , "  local pane = c.getPane(c.getLevel(1), paneKey);"
    , "  if not pane then return nil end;"
    , "  return { title = l.getFontSize(pane.titleId),"
    , "           subtitle = l.getFontSize(pane.subtitleId) } end;"
    , "_G.__ids = function(kind, id)"
    , "  local info = unit.transferEndpointInfo({kind = kind, id = id});"
    , "  local out = {};"
    , "  for _, it in ipairs((info and info.contents) or {}) do"
    , "    out[#out + 1] = tostring(it.defName) .. '#' .. tostring(it.instanceId)"
    , "  end;"
    , "  table.sort(out);"
    , "  return table.concat(out, ',') end;"
    ]

-- | Create a session from `carrierUid` to one endpoint.
createLua ∷ Text → Int → Text
createLua kind rid = T.concat
    [ "return tostring(_G.__session().create(1, '", kind, "', "
    , tshow rid, ") ~= nil)" ]

-- * The gate

spec ∷ Spec
spec = aroundAll withSharedFixture $
  describe "Transfer context menu (Mode A escort session, #1250)" $ do

    describe "the escort lifecycle" $ do
        it "a fresh session is APPROACHING, holds its source unit, and has \
           \opened no window yet — the panels are an arrival event, not a \
           \creation one" $ \(env, ls) → do
            resetFixture env ls
            created ← evalOk ls (createLua "building" 7)
            created `shouldBe` "\"true\""
            phase ← evalOk ls "return _G.__phase()"
            phase `shouldBe` "\"approaching\""
            held ← evalOk ls
                "return tostring(_G.__session().holdsUnit(1))"
            held `shouldBe` "\"true\""
            other ← evalOk ls
                "return tostring(_G.__session().holdsUnit(2))"
            other `shouldBe` "\"false\""
            depth ← evalOk ls
                "return require('scripts.cargo_inventory_panel').depth()"
            depth `shouldBe` "0"
            snaps ← evalOk ls "return _G.__snaps"
            snaps `shouldBe` "0"

        it "the hold is a real in-progress LOCK: it outscores a fully rested \
           \acolyte's wander and idle, it does not vary with churn, and it \
           \is -inf for a unit no session names" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            -- The REAL wander/idle utilities, with the REAL acolyte
            -- tunables and a stat stub that puts wander at its own
            -- ceiling — the strongest form of "the wander tick cannot
            -- steal it" this fixture can state.
            scores ← evalOk ls (luaLines
                [ "unit.getStat = function() return 100 end;"
                , "package.loaded['scripts.unit_stats'] ="
                , "  { get = function() return 100 end };"
                , "local needs = require('scripts.unit_ai_needs');"
                , "local params = require('scripts.unit_ai_tunables').acolyte;"
                , "local s = { currentAction = 'wander',"
                , "            actionStartedAt = engine.gameTime() };"
                , "local escort = _G.__tick(1);"
                , "return { escort = escort,"
                , "         wander = needs.wanderUtility(1, s, params),"
                , "         idle   = needs.idleUtility(1, s, params) }" ])
            scores `shouldSatisfy` T.isInfixOf "\"escort\":7.5"
            -- Repeated scoring never moves it: an in-progress lock that
            -- decayed would be stealable by definition.
            steady ← evalOk ls
                "local a = _G.__tick(1); local b = _G.__tick(1); \
                \return tostring(a == b and a == 7.5)"
            steady `shouldBe` "\"true\""
            -- ...and it sits above every routine-work lock (6.0) and
            -- above follow_command (7.0), which is what puts it in the
            -- player-order band beside pickup and a queued order.
            band ← evalOk ls "return tostring(_G.__tick(1) > 7.0)"
            band `shouldBe` "\"true\""
            unheld ← evalOk ls "return tostring(_G.__tick(2))"
            unheld `shouldBe` "\"-inf\""

        it "a distant carrier WALKS toward the endpoint's footprint and does \
           \not open anything; standing adjacent it stops, reveals, opens \
           \both panes and snaps the camera — each exactly once" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            walked ← evalOk ls "return _G.__tick(1)"
            walked `shouldBe` "7.5"
            moves ← evalOk ls "return _G.__moves"
            moves `shouldBe` "1"
            stillApproaching ← evalOk ls "return _G.__phase()"
            stillApproaching `shouldBe` "\"approaching\""
            noWindow ← evalOk ls
                "return require('scripts.cargo_inventory_panel').depth()"
            noWindow `shouldBe` "0"

            -- Adjacent by the contract's own footprint rule: the hold
            -- occupies (40,40)..(41,41), so (42,41) is Chebyshev 1 from
            -- its far edge and a whole world away from its anchor.
            placeUnit env carrierUid (42, 41)
            arrived ← evalOk ls "_G.__tick(1); return _G.__phase()"
            arrived `shouldBe` "\"open\""
            reveals ← evalOk ls "return _G.__reveals"
            reveals `shouldBe` "1"
            snaps ← evalOk ls "return _G.__snaps"
            snaps `shouldBe` "1"
            -- ONE level, TWO panes: D-9's stated exception.
            shape ← evalOk ls
                "local d = require('scripts.cargo_inventory_panel').dump(); \
                \return { depth = d.depth, kind = d.levels[1].kind, \
                \         panes = d.levels[1].paneCount, \
                \         modal = d.levels[1].modal }"
            shape `shouldSatisfy` T.isInfixOf "\"depth\":1"
            shape `shouldSatisfy` T.isInfixOf "\"panes\":2"
            shape `shouldSatisfy` T.isInfixOf "\"kind\":\"escort\""
            shape `shouldSatisfy` T.isInfixOf "\"modal\":false"
            -- Every later tick holds: no second snap, no second reveal,
            -- no second window.
            again ← evalOk ls
                "_G.__tick(1); _G.__tick(1); \
                \return { snaps = _G.__snaps, reveals = _G.__reveals, \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            again `shouldSatisfy` T.isInfixOf "\"snaps\":1"
            again `shouldSatisfy` T.isInfixOf "\"reveals\":1"
            again `shouldSatisfy` T.isInfixOf "\"depth\":1"

        it "a UNIT destination opens a session with only the SOURCE held — \
           \the target is not commanded and not reveal-refreshed" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            created ← evalOk ls (createLua "unit" 2)
            created `shouldBe` "\"true\""
            opened ← evalOk ls "_G.__tick(1); return _G.__phase()"
            opened `shouldBe` "\"open\""
            -- A unit endpoint has no remembered snapshot, so the reveal
            -- verb is never called for one.
            reveals ← evalOk ls "return _G.__reveals"
            reveals `shouldBe` "0"
            holds ← evalOk ls
                "return { src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2) }"
            holds `shouldSatisfy` T.isInfixOf "\"src\":true"
            holds `shouldSatisfy` T.isInfixOf "\"dst\":false"
            -- The target scores nothing at all, so nothing holds it in
            -- place — UIT-4's two-sided hold is deliberately absent.
            targetScore ← evalOk ls "return tostring(_G.__tick(2))"
            targetScore `shouldBe` "\"-inf\""

        -- #1250 post-merge review: unit_ai's execute gate re-runs an
        -- action only on a SWITCH or when the unit is idle, and this
        -- action is deliberately not forceExecute — so a replacement
        -- session on the SAME unit kept walking to the OLD endpoint
        -- until that path ran out before it ever looked at the new one.
        it "replacing a session mid-APPROACH interrupts the walk it was on, \
           \so the new one routes to its own destination immediately" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (60, 60)
            _ ← evalOk ls (createLua "building" 7)
            first ← evalOk ls
                "_G.__tick(1); return { moves = _G.__moves, \
                \ stops = _G.__stops, x = _G.__lastMove.x, \
                \ y = _G.__lastMove.y }"
            -- Walking toward the hold at (40,40)..(41,41), and not yet
            -- stopped: this is the in-flight approach the replacement
            -- has to interrupt.
            first `shouldSatisfy` T.isInfixOf "\"moves\":1"
            first `shouldSatisfy` T.isInfixOf "\"stops\":0"
            first `shouldSatisfy` T.isInfixOf "\"x\":39.5"
            -- Replace it with a session on the MATE, far away in the
            -- other direction.
            replaced ← evalOk ls
                "_G.__session().create(1, 'unit', 2); \
                \return { stops = _G.__stops, stopped = _G.__lastStop, \
                \         phase = _G.__phase() }"
            replaced `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            -- The release STOPPED the escort rather than merely letting
            -- go of it, which is what makes the unit idle and the next
            -- tick re-decide.
            replaced `shouldSatisfy` T.isInfixOf "\"stops\":1"
            replaced `shouldSatisfy` T.isInfixOf "\"stopped\":1"
            -- ...and the very next tick walks toward the MATE at
            -- (60,60), not back to the hold.
            rerouted ← evalOk ls
                "_G.__tick(1); return { x = _G.__lastMove.x, \
                \ y = _G.__lastMove.y, moves = _G.__moves }"
            rerouted `shouldSatisfy` T.isInfixOf "\"moves\":2"
            rerouted `shouldSatisfy` T.isInfixOf "\"x\":59.5"
            rerouted `shouldSatisfy` T.isInfixOf "\"y\":59.5"

        it "closing the window closes BOTH panes, ends the session and \
           \releases the unit — and doing it again changes nothing" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            closed ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \cip.popLevel(); \
                \return { depth = cip.depth(), \
                \         session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1) }"
            closed `shouldSatisfy` T.isInfixOf "\"depth\":0"
            closed `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            closed `shouldSatisfy` T.isInfixOf "\"held\":false"
            -- Idempotent from both ends.
            again ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \cip.popLevel(); cip.closeIfOpen(); _G.__session().clear(); \
                \return { depth = cip.depth(), \
                \         session = tostring(_G.__session().get()) }"
            again `shouldSatisfy` T.isInfixOf "\"depth\":0"
            again `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            -- The released unit scores nothing, which IS the release.
            score ← evalOk ls "return tostring(_G.__tick(1))"
            score `shouldBe` "\"-inf\""

        it "the save-load reset hook ends the session and releases the unit, \
           \and the session still contributes NO save component (D-3)" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls
                "require('scripts.transfer_session').init('transfer_session'); \
                \return 'ok'"
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            kinds ← evalOk ls
                "local sm = require('scripts.lib.save_modules'); \
                \return type(sm.resetHooks['transfer_session']) .. '/' \
                \    .. type(sm.registry['transfer_session'])"
            kinds `shouldBe` "\"function/nil\""
            after ← evalOk ls
                "local sm = require('scripts.lib.save_modules'); \
                \sm.resetHooks['transfer_session'](); \
                \return { session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            after `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            after `shouldSatisfy` T.isInfixOf "\"held\":false"
            after `shouldSatisfy` T.isInfixOf "\"depth\":0"

        it "Exit to Menu ends the session the same way" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            _ ← evalOk ls "world.destroyAll = function() end; return 'ok'"
            exited ← evalOk ls
                "require('scripts.pause_menu').onExitToMenu(); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            exited `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            exited `shouldSatisfy` T.isInfixOf "\"depth\":0"

        it "a REFUSED replacement leaves the running session — its phase, its \
           \hold and its open panes — exactly as it found it" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- Building 999 does not exist, so create refuses at its
            -- receiver-missing branch, before it may disturb anything.
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'building', 999); \
                \return { made = tostring(s), reason = tostring(reason) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy` T.isInfixOf "\"reason\":\"receiver_missing\""
            intact ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \return { phase = _G.__phase(), held = _G.__session().holdsUnit(1), \
                \         depth = cip.depth(), \
                \         panes = cip.dump().levels[1].paneCount }"
            intact `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            intact `shouldSatisfy` T.isInfixOf "\"held\":true"
            intact `shouldSatisfy` T.isInfixOf "\"depth\":1"
            intact `shouldSatisfy` T.isInfixOf "\"panes\":2"

        it "a SUCCESSFUL replacement closes the old pair and re-arms the \
           \escort, and opening an ordinary container window instead ends \
           \the session outright" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            placeUnit env matesUid (43, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            firstId ← evalOk ls "return _G.__session().get().id"
            -- Replacing with a session on the MATE: one window, one
            -- session, and the new one starts over at approaching.
            replaced ← evalOk ls (luaLines
                [ "_G.__session().create(1, 'unit', 2);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                , "return { depth = cip.depth(), phase = _G.__phase(),"
                , "         id = _G.__session().get().id }" ])
            replaced `shouldSatisfy` T.isInfixOf "\"depth\":0"
            replaced `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            replaced `shouldNotSatisfy` T.isInfixOf ("\"id\":" <> firstId)
            _ ← evalOk ls "return _G.__tick(1)"
            -- ...and an ordinary container window at the same level
            -- replaces the pair, which ends the session (requirement 7).
            ended ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \cip.openFor('building', 7, 10, 10); \
                \return { depth = cip.depth(), kind = cip.dump().levels[1].kind, \
                \         session = tostring(_G.__session().get()) }"
            ended `shouldSatisfy` T.isInfixOf "\"depth\":1"
            ended `shouldSatisfy` T.isInfixOf "\"kind\":\"endpoint\""
            ended `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""

        it "a resize preserves the session, the hold, both panes and each \
           \pane's own tab and scroll — and repeats neither the snap nor \
           \the reveal (requirement 8)" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- Drive the REAL resize path: snapshot, the "resize"
            -- teardown sweep, restore — exactly what hud.createUI runs.
            restored ← evalOk ls (luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "local snap = cip.snapshotStack();"
                , "require('scripts.ui.view_teardown').run('resize');"
                , "local mid = { depth = cip.depth(),"
                , "              session = tostring(_G.__session().get()),"
                , "              held = _G.__session().holdsUnit(1) };"
                , "cip.restoreStack(snap);"
                , "local d = cip.dump();"
                , "return { midDepth = mid.depth, midSession = mid.session,"
                , "         midHeld = mid.held, depth = d.depth,"
                , "         kind = d.levels[1].kind,"
                , "         panes = d.levels[1].paneCount,"
                , "         phase = _G.__phase(),"
                , "         held = _G.__session().holdsUnit(1),"
                , "         snaps = _G.__snaps, reveals = _G.__reveals }" ])
            -- The teardown really did destroy the window...
            restored `shouldSatisfy` T.isInfixOf "\"midDepth\":0"
            -- ...and the session survived it, still holding its unit.
            restored `shouldSatisfy` T.isInfixOf "\"midSession\":\"table"
            restored `shouldSatisfy` T.isInfixOf "\"midHeld\":true"
            restored `shouldSatisfy` T.isInfixOf "\"depth\":1"
            restored `shouldSatisfy` T.isInfixOf "\"kind\":\"escort\""
            restored `shouldSatisfy` T.isInfixOf "\"panes\":2"
            restored `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            restored `shouldSatisfy` T.isInfixOf "\"held\":true"
            restored `shouldSatisfy` T.isInfixOf "\"snaps\":1"
            restored `shouldSatisfy` T.isInfixOf "\"reveals\":1"

        -- #1250 review round 1: a species the escort action was never
        -- registered for could be made a session's source, and the
        -- session then sat in `approaching` forever -- no walk, no
        -- panels, and a "hold" holding nothing.
        it "a source whose species cannot run the escort is skipped by the \
           \shared rule and refused by create, so no stuck session exists" $
           \(env, ls) → do
            resetFixture env ls
            -- A REAL registration, through the same public API every
            -- satellite AI script plugs itself in with: the bear is
            -- player-commandable here and registers no escort action.
            _ ← evalOk ls (luaLines
                [ "local a = require('scripts.unit_ai_actions');"
                , "a.byDef = {};"
                , "a.record('acolyte', { {name = 'wander'},"
                , "                      {name = 'escort_transfer'} });"
                , "a.record('bear', { {name = 'wander'} });"
                , "return 'ok'" ])
            -- uid 2 is the ONLY candidate and is a bear, so the rule has
            -- to answer nil rather than pick the only unit it has.
            _ ← evalOk ls (luaLines
                [ "local um = unit.getInfo;"
                , "unit.getInfo = function(uid)"
                , "  local i = um(uid); if not i then return nil end;"
                , "  if uid == 2 then i.defName = 'bear' end;"
                , "  return i end;"
                , "return 'ok'" ])
            resolved ← evalOk ls (luaLines
                [ "local s = require('scripts.transfer_session');"
                , "local ep = unit.transferEndpointInfo("
                , "  {kind = 'building', id = 7});"
                , "return { unfiltered = s.resolveSource({2}, nil, ep),"
                , "         filtered = tostring(s.resolveSource({2}, nil, ep,"
                , "                                s.ESCORT_ACTION)) }" ])
            resolved `shouldSatisfy` T.isInfixOf "\"unfiltered\":2"
            resolved `shouldSatisfy` T.isInfixOf "\"filtered\":\"nil\""
            -- ...and the ONE creation path refuses it too, so a surface
            -- that never ran the rule cannot mint the stuck session
            -- either.
            refused ← evalOk ls
                "local s, reason = require('scripts.transfer_session')\
                \.create(2, 'building', 7); \
                \return { made = tostring(s), reason = tostring(reason) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy`
                T.isInfixOf "\"reason\":\"source_not_escortable\""
            open' ← evalOk ls
                "return require('scripts.cargo_inventory_panel').depth()"
            open' `shouldBe` "0"
            -- The acolyte, which DID register it, still resolves and
            -- still creates — the filter must not refuse everything.
            ok ← evalOk ls (luaLines
                [ "local s = require('scripts.transfer_session');"
                , "local ep = unit.transferEndpointInfo("
                , "  {kind = 'building', id = 7});"
                , "return s.resolveSource({1}, nil, ep, s.ESCORT_ACTION)" ])
            ok `shouldBe` "1"

        -- #1250 review round 3: both panes named their widgets
        -- "cargo_inv", so their tab controls got identical element
        -- names — and control focus survives a rebuild by NAME,
        -- restoring the FIRST visible match. Focus parked on the
        -- destination pane came back on the SOURCE pane's
        -- corresponding tab: a silently wrong control, not a missing
        -- one.
        it "keyboard control focus on the DESTINATION pane comes back on \
           \the destination pane across a resize, not its source-pane twin" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- The two panes' controls must be distinguishable by name
            -- at all: that is the property by-name restore relies on.
            names ← evalOk ls (luaLines
                [ "local c = require('scripts.cargo_inventory_panel');"
                , "local il = require('scripts.ui.item_list');"
                , "local lvl = c.getLevel(1);"
                , "local function firstTab(key)"
                , "  local p = c.getPane(lvl, key);"
                , "  local t = il.getTabs(p.listId)[1];"
                , "  local i = t and UI.getElementInfo(t.boxId);"
                , "  return t and t.boxId, i and i.name end;"
                , "local sh, sn = firstTab('source');"
                , "local dh, dn = firstTab('destination');"
                , "_G.__srcTab, _G.__dstTab = sh, dh;"
                , "return { src = sn, dst = dn, distinct = sn ~= dn }" ])
            names `shouldSatisfy` T.isInfixOf "\"distinct\":true"
            names `shouldSatisfy` T.isInfixOf "cargo_inv_destination"
            -- Now the real round trip, through the SAME snapshot/restore
            -- pair hud.createUI wraps its rebuild in.
            restored ← evalOk ls (luaLines
                [ "local c = require('scripts.cargo_inventory_panel');"
                , "local r = require('scripts.ui.responsive');"
                , "local il = require('scripts.ui.item_list');"
                , "UI.setControlFocus(_G.__dstTab);"
                , "local want = r.snapshotControlFocusName();"
                , "local snap = c.snapshotStack();"
                , "require('scripts.ui.view_teardown').run('resize');"
                , "c.restoreStack(snap);"
                , "r.restoreControlFocusName(want);"
                , "local lvl = c.getLevel(1);"
                , "local focus = UI.getControlFocus();"
                , "local function ownsFocus(key)"
                , "  local p = c.getPane(lvl, key);"
                , "  for _, t in ipairs(il.getTabs(p.listId)) do"
                , "    if t.boxId == focus then return true end end;"
                , "  return false end;"
                , "return { want = want, focused = focus ~= nil,"
                , "         onDestination = ownsFocus('destination'),"
                , "         onSource = ownsFocus('source') }" ])
            restored `shouldSatisfy` T.isInfixOf "\"focused\":true"
            restored `shouldSatisfy` T.isInfixOf "\"onDestination\":true"
            restored `shouldSatisfy` T.isInfixOf "\"onSource\":false"

        it "both panes are framebuffer-clamped and do not overlap each other" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            geom ← evalOk ls (luaLines
                [ "local d = require('scripts.cargo_inventory_panel').dump();"
                , "local p = d.levels[1].panes;"
                , "local a, b = p[1], p[2];"
                , "local function inFrame(r)"
                , "  return r.x >= 0 and r.y >= 0"
                , "     and r.x + r.width <= 1280 and r.y + r.height <= 720 end;"
                , "local overlap = not (a.x + a.width <= b.x or b.x + b.width <= a.x"
                , "                     or a.y + a.height <= b.y"
                , "                     or b.y + b.height <= a.y);"
                , "return { inFrame = inFrame(a) and inFrame(b),"
                , "         overlap = overlap, leftFirst = a.x < b.x,"
                , "         keyA = a.paneKey, keyB = b.paneKey }" ])
            geom `shouldSatisfy` T.isInfixOf "\"inFrame\":true"
            geom `shouldSatisfy` T.isInfixOf "\"overlap\":false"
            geom `shouldSatisfy` T.isInfixOf "\"leftFirst\":true"
            geom `shouldSatisfy` T.isInfixOf "\"keyA\":\"source\""
            geom `shouldSatisfy` T.isInfixOf "\"keyB\":\"destination\""
            -- With room to spare nothing is fitted, so the headers
            -- rasterise at their configured sizes — the other half of
            -- the minimum-viewport case below, which is what stops a
            -- fix there from simply shrinking every header everywhere.
            fonts ← evalOk ls "return _G.__headerFonts('source')"
            fonts `shouldSatisfy` T.isInfixOf "\"title\":16"
            fonts `shouldSatisfy` T.isInfixOf "\"subtitle\":13"

        -- #1250 review round 1: at the envelope's FORMAL MINIMUM the
        -- pair's natural width (2x440 + gap = 904) exceeds the
        -- framebuffer, and clamping each panel on its own — all
        -- measurePane and UI.placePopup can do — lands them on top of
        -- each other. The PAIR is fitted first now.
        it "at the supported minimum 800x600 @ 1x the pair still flanks: \
           \fitted to width, in frame, source left, no overlap" $
           \(env, ls) → do
            resetFixture env ls
            writeIORef (framebufferSizeRef env) (800, 600)
            _ ← evalOk ls "require('scripts.cargo_inventory_panel')\
                          \.setup({page = _G.__page, fbW = 800, fbH = 600, \
                          \        boxTexSet = 1, menuFont = 1}); return 'ok'"
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            tight ← evalOk ls (luaLines
                [ "local d = require('scripts.cargo_inventory_panel').dump();"
                , "local p = d.levels[1].panes;"
                , "local a, b = p[1], p[2];"
                , "local function inFrame(r)"
                , "  return r.x >= 0 and r.y >= 0"
                , "     and r.x + r.width <= 800 and r.y + r.height <= 600 end;"
                , "local overlap = not (a.x + a.width <= b.x"
                , "                     or b.x + b.width <= a.x"
                , "                     or a.y + a.height <= b.y"
                , "                     or b.y + b.height <= a.y);"
                , "return { inFrame = inFrame(a) and inFrame(b),"
                , "         overlap = overlap, leftFirst = a.x < b.x,"
                , "         keyA = a.paneKey, keyB = b.paneKey,"
                -- `shrunk` proves this is a FIT and not merely a clamp:
                -- an unfitted pane would still measure its full 440.
                -- (A Lua comment cannot ride inside these chunks — the
                -- debug console is single-line, so `--` would swallow
                -- the rest of the statement.)
                , "         shrunk = a.width < 440 and b.width < 440,"
                , "         panes = d.levels[1].paneCount }" ])
            tight `shouldSatisfy` T.isInfixOf "\"inFrame\":true"
            tight `shouldSatisfy` T.isInfixOf "\"overlap\":false"
            tight `shouldSatisfy` T.isInfixOf "\"leftFirst\":true"
            tight `shouldSatisfy` T.isInfixOf "\"keyA\":\"source\""
            tight `shouldSatisfy` T.isInfixOf "\"keyB\":\"destination\""
            tight `shouldSatisfy` T.isInfixOf "\"shrunk\":true"
            tight `shouldSatisfy` T.isInfixOf "\"panes\":2"
            -- ...and the rows inside shrank WITH the box rather than
            -- overflowing it, which is what makes this a fit and not a
            -- crop.
            fitted ← evalOk ls
                "local c = require('scripts.cargo_inventory_panel'); \
                \local il = require('scripts.ui.item_list'); \
                \local lvl = c.getLevel(1); \
                \return { list = il.getScale(c.getPane(lvl, 'source').listId), \
                \         cfg = require('scripts.ui.scale').get() }"
            fitted `shouldSatisfy` T.isInfixOf "\"cfg\":1"
            fitted `shouldNotSatisfy` T.isInfixOf "\"list\":1"
            -- ...and so did the HEADERS (#1250 review round 2). Their
            -- three bands are reserved at the pane's fitted scale, so a
            -- title still rasterised at the configured 16 would reach
            -- down into rows the panel never sized for it.
            headers ← evalOk ls "return { src = _G.__headerFonts('source'), \
                                \         dst = _G.__headerFonts('destination') }"
            headers `shouldNotSatisfy` T.isInfixOf "\"title\":16"
            headers `shouldNotSatisfy` T.isInfixOf "\"subtitle\":13"
            smaller ← evalOk ls (luaLines
                [ "local f = _G.__headerFonts('source');"
                , "local g = _G.__headerFonts('destination');"
                , "return tostring(f.title > 0 and f.title < 16"
                , "                and f.subtitle > 0 and f.subtitle < 13"
                , "                and g.title == f.title"
                , "                and g.subtitle == f.subtitle)" ])
            smaller `shouldBe` "\"true\""

        -- #1250 review round 4: the pair was fitted to the FULL
        -- framebuffer and each panel then placed independently, so at
        -- the envelope's minimum with the real toolbar reserving a
        -- band, the first panel's own avoidance consumed the space the
        -- second needed and avoidReserved's best-effort fallback landed
        -- them on top of each other.
        it "at 800x600 @ 1x WITH the toolbar reserved and both lists at \
           \their row cap, the pair still fits beside it and still flanks" $
           \(env, ls) → do
            resetFixture env ls
            writeIORef (framebufferSizeRef env) (800, 600)
            stockTall env
            -- Supplied through the same lazy require the placement
            -- reads it with, so this is the production path and not a
            -- parallel one. A FULL-HEIGHT left rail deliberately: a
            -- short bottom-left cluster leaves the pair a vertical
            -- escape, and it is the reservation with no way around it
            -- that forces the pair to be narrow enough to sit BESIDE
            -- it — which is the half `availableWidth` buys and a
            -- fit-to-whole-framebuffer cannot.
            _ ← evalOk ls (luaLines
                [ "package.loaded['scripts.hud'] = {"
                , "  getToolbarRects = function()"
                , "    return { {name = 'tool_rail', x = 16, y = 0,"
                , "              w = 64, h = 600} } end };"
                , "require('scripts.cargo_inventory_panel').setup("
                , "  {page = _G.__page, fbW = 800, fbH = 600,"
                , "   boxTexSet = 1, menuFont = 1});"
                , "return 'ok'" ])
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            crowded ← evalOk ls (luaLines
                [ "local d = require('scripts.cargo_inventory_panel').dump();"
                , "local p = d.levels[1].panes;"
                , "local a, b = p[1], p[2];"
                , "local bar = {x = 16, y = 0, w = 64, h = 600};"
                , "local function hits(r, o)"
                , "  return not (r.x + r.width <= o.x or o.x + o.w <= r.x"
                , "              or r.y + r.height <= o.y"
                , "              or o.y + o.h <= r.y) end;"
                , "local function inFrame(r)"
                , "  return r.x >= 0 and r.y >= 0"
                , "     and r.x + r.width <= 800 and r.y + r.height <= 600 end;"
                , "local overlap = not (a.x + a.width <= b.x"
                , "                     or b.x + b.width <= a.x"
                , "                     or a.y + a.height <= b.y"
                , "                     or b.y + b.height <= a.y);"
                , "return { inFrame = inFrame(a) and inFrame(b),"
                , "         overlap = overlap, leftFirst = a.x < b.x,"
                , "         onToolbar = hits(a, bar) or hits(b, bar),"
                , "         gap = b.x - (a.x + a.width),"
                , "         rows = a.rowCount .. '/' .. b.rowCount,"
                , "         panes = d.levels[1].paneCount }" ])
            crowded `shouldSatisfy` T.isInfixOf "\"panes\":2"
            -- Both lists really are at the cap, so these are the
            -- TALLEST panels this window can produce.
            crowded `shouldSatisfy` T.isInfixOf "\"rows\":\"10/10\""
            crowded `shouldSatisfy` T.isInfixOf "\"inFrame\":true"
            crowded `shouldSatisfy` T.isInfixOf "\"overlap\":false"
            crowded `shouldSatisfy` T.isInfixOf "\"leftFirst\":true"
            crowded `shouldSatisfy` T.isInfixOf "\"onToolbar\":false"
            -- The pair is laid out as ONE rect and then split, so the
            -- two panes sit exactly one scaled gap apart WHEREVER
            -- arbitration puts them. Placing each panel on its own and
            -- nudging the second clear of the first leaves that spacing
            -- to whatever the nudge produced, so this is the assertion
            -- that pins the structure rather than the outcome. The base
            -- gap is 24 at uiscale 1 and the pair is fitted below that
            -- here, so a positive gap no larger than 24 is the fitted
            -- one and nothing else.
            gapText ← evalOk ls (luaLines
                [ "local d = require('scripts.cargo_inventory_panel').dump();"
                , "local p = d.levels[1].panes;"
                , "local g = p[2].x - (p[1].x + p[1].width);"
                , "return tostring(g > 0 and g <= 24)" ])
            gapText `shouldBe` "\"true\""


    describe "the session's row gestures" $ do
        let openOnHold env ls = do
                resetFixture env ls
                placeUnit env carrierUid (42, 41)
                _ ← evalOk ls (createLua "building" 7)
                _ ← evalOk ls "return _G.__tick(1)"
                pure ()

        it "the source pane offers Store and the destination pane Retrieve, \
           \each 1-and-all on a merged row and 1 alone on a single instance" $
           \(env, ls) → do
            openOnHold env ls
            store ← evalOk ls
                "return _G.__labels(_G.__rowMenu('source', 'ration'))"
            store `shouldBe` "\"Store 1|Store all\""
            lone ← evalOk ls
                "return _G.__labels(_G.__rowMenu('source', 'rope'))"
            lone `shouldBe` "\"Store 1\""
            retrieve ← evalOk ls
                "return _G.__labels(_G.__rowMenu('destination', 'steel_bar'))"
            retrieve `shouldBe` "\"Retrieve 1|Retrieve all\""
            loneOther ← evalOk ls
                "return _G.__labels(_G.__rowMenu('destination', 'crowbar'))"
            loneOther `shouldBe` "\"Retrieve 1\""

        it "Store all moves EVERY exact instance the merged row stands for, \
           \immediately, and Retrieve 1 moves the representative alone" $
           \(env, ls) → do
            openOnHold env ls
            before ← evalOk ls "return _G.__ids('building', 7)"
            before `shouldBe` "\"crowbar#210,steel_bar#201,steel_bar#202\""
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            afterStore ← evalOk ls "return _G.__ids('building', 7)"
            afterStore `shouldBe`
                "\"crowbar#210,ration#101,ration#102,ration#103,\
                \steel_bar#201,steel_bar#202\""
            carrierLeft ← evalOk ls "return _G.__ids('unit', 1)"
            carrierLeft `shouldBe` "\"rope#110\""
            -- ...and back the other way, one instance only.
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('destination', 'steel_bar'), \
                \                          'Retrieve 1'))"
            carrierNow ← evalOk ls "return _G.__ids('unit', 1)"
            carrierNow `shouldSatisfy` T.isInfixOf "steel_bar#201"
            carrierNow `shouldSatisfy` T.isInfixOf "rope#110"
            holdNow ← evalOk ls "return _G.__ids('building', 7)"
            holdNow `shouldNotSatisfy` T.isInfixOf "steel_bar#201"
            holdNow `shouldSatisfy` T.isInfixOf "steel_bar#202"
            -- The session is untouched by a commit: repeatable while
            -- adjacent is the whole point.
            still ← evalOk ls
                "return { phase = _G.__phase(), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            still `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            still `shouldSatisfy` T.isInfixOf "\"depth\":1"

        it "both panes refresh in the same gesture: the header weights move \
           \without waiting for a tick and without reopening anything" $
           \(env, ls) → do
            openOnHold env ls
            subs ← evalOk ls
                "local d = require('scripts.cargo_inventory_panel').dump(); \
                \return { a = d.levels[1].panes[1].subtitle, \
                \         b = d.levels[1].panes[2].subtitle }"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            after ← evalOk ls
                "local d = require('scripts.cargo_inventory_panel').dump(); \
                \return { a = d.levels[1].panes[1].subtitle, \
                \         b = d.levels[1].panes[2].subtitle, \
                \         depth = d.depth }"
            after `shouldNotBe` subs
            after `shouldSatisfy` T.isInfixOf "\"depth\":1"
            -- The carrier's own pane now reads the lighter load, and
            -- the hold's the heavier one — both LIVE, not the values
            -- either had when the window opened.
            after `shouldSatisfy` T.isInfixOf "Carrying: 2.00"
            after `shouldSatisfy` T.isInfixOf "Storage: 8.50"

        it "a partial batch commits what fits and reports the remainder by \
           \count and by the contract's own reason (D-1)" $ \(env, ls) → do
            resetFixture env ls
            -- The tiny hold takes 1.2 kg: exactly two of three rations.
            placeUnit env carrierUid (81, 80)
            _ ← evalOk ls (createLua "building" 8)
            _ ← evalOk ls "return _G.__tick(1)"
            phase ← evalOk ls "return _G.__phase()"
            phase `shouldBe` "\"open\""
            _ ← evalOk ls "_G.__events = {}; return 'ok'"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            stored ← evalOk ls "return _G.__ids('building', 8)"
            stored `shouldBe` "\"ration#101,ration#102\""
            kept ← evalOk ls "return _G.__ids('unit', 1)"
            kept `shouldSatisfy` T.isInfixOf "ration#103"
            reported ← evalOk ls "return _G.__eventText()"
            reported `shouldSatisfy` T.isInfixOf "unit_warning"
            reported `shouldSatisfy` T.isInfixOf "couldn't Store 1 x"
            reported `shouldSatisfy` T.isInfixOf "receiver_full"
            -- No item half-moved: every instance is on exactly one side.
            open' ← evalOk ls
                "return { phase = _G.__phase(), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            open' `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            open' `shouldSatisfy` T.isInfixOf "\"depth\":1"

        it "a target that drifts out of reach refuses by PROXIMITY, moves \
           \nothing, and leaves the session open (requirement 6)" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "return _G.__tick(1)"
            opened ← evalOk ls "return _G.__phase()"
            opened `shouldBe` "\"open\""
            -- The mate walks away. Nothing re-approaches: the hold is
            -- source-only until UIT-4.
            placeUnit env matesUid (60, 60)
            _ ← evalOk ls "_G.__events = {}; return 'ok'"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            mate ← evalOk ls "return _G.__ids('unit', 2)"
            mate `shouldBe` "\"\""
            carrier ← evalOk ls "return _G.__ids('unit', 1)"
            carrier `shouldSatisfy` T.isInfixOf "ration#101"
            reported ← evalOk ls "return _G.__eventText()"
            reported `shouldSatisfy` T.isInfixOf "unit_warning"
            reported `shouldSatisfy` T.isInfixOf "out_of_range"
            survives ← evalOk ls
                "return { phase = _G.__phase(), held = _G.__session().holdsUnit(1), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            survives `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            survives `shouldSatisfy` T.isInfixOf "\"held\":true"
            survives `shouldSatisfy` T.isInfixOf "\"depth\":1"

    describe "registration" $
        -- A source guard beside the behavioural cases above, following
        -- the "random stream ownership" precedent: the hold is only
        -- reachable if the action is in the real per-species lists, and
        -- nothing this fixture can drive would notice its absence.
        it "the escort action is registered for every player-commandable \
           \species the source rule can resolve" $ \_ → do
            src ← TIO.readFile "scripts/unit_ai.lua"
            let regs = length (T.breakOnAll "transfer.escortAction," src)
            regs `shouldBe` 2
