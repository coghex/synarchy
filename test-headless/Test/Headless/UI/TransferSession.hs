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

-- | uid 3 — a technomule, the OTHER unit-to-unit pairing #1251
--   requirement 3 names. A different species on purpose: nothing about
--   the hold or the commit policy may key on a unit definition (the
--   eligibility rule is @isPlayerCommandable@ of the live faction and
--   nothing else), so this pair is the same code path as
--   acolyte↔acolyte or the endpoint abstraction has leaked.
muleUid ∷ UnitId
muleUid = UnitId 3

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

-- | A SECOND page, for the one rule that needs two (#1415): a pair on
--   different pages. Never registered in @wmWorlds@ and never
--   populated — an endpoint's page is its own instance's @uiPage@ /
--   @biPage@ and the projection reads nothing else, so moving one unit
--   onto this id is the entire fixture.
otherPage ∷ WorldPageId
otherPage = WorldPageId "transfer_session_other_page"

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

-- | The mule's own stock, so a mule↔acolyte session has rows to
--   RETRIEVE as well as rows to store. Carried by the fixture rather
--   than by a case because uid 3 is new here and no pre-#1251 scenario
--   reads it, whereas the two acolytes' loads are asserted on exactly
--   as they stand and are therefore stocked per case ('stockUnit').
muleInventory ∷ [ItemInstance]
muleInventory =
    [ mkItem "steel_plate" 401 3.0
    , mkItem "steel_plate" 402 3.0
    ]

-- | The mate's own stock, for the acolyte↔acolyte direction.
mateInventory ∷ [ItemInstance]
mateInventory =
    [ mkItem "bandage" 301 0.2
    , mkItem "bandage" 302 0.2
    , mkItem "canteen" 310 1.0
    ]

-- | An ACCESSORY (#1251 requirement 4, D-6): it weighs into capacity
--   like everything else and is never transferable.
--   'Test.Headless.Unit.Transfer' proves that refusal against the pure
--   policy; what this fixture adds is that the session's own two
--   surfaces agree with it.
carrierWorn ∷ [ItemInstance]
carrierWorn = [ mkItem "acolyte_robe" 120 1.5 ]

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

-- | Give one unit a loose inventory and a set of worn accessories for
--   the duration of one case. Every pre-#1251 scenario asserts the two
--   acolytes' loads exactly as 'resetWorld' leaves them, so a case that
--   needs a different scene says so itself rather than moving the
--   shared one out from under them.
stockUnit ∷ EngineEnv → UnitId → [ItemInstance] → [ItemInstance] → IO ()
stockUnit env uid inv worn =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.adjust
                (\u → u { uiInventory = inv, uiAccessories = worn })
                uid (umInstances um) }
        , ())

-- | Force one unit's POSE for the duration of a case. Nothing here
--   ticks a simulation, so this is how a scenario kills or knocks out
--   a unit: 'uiPose' is exactly what @unit.getPose@ reports and what
--   scripts/unit_ai.lua's own short-circuit reads.
setPose ∷ EngineEnv → UnitId → Text → IO ()
setPose env uid pose =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.adjust
                (\u → u { uiPose = pose }) uid (umInstances um) }
        , ())

-- | Move one unit into another faction — the "faction loss" half of
--   requirement 2, and the one the CONTRACT already refuses on its own
--   (@Unit.Transfer.endpointEligible@ is @uevCommandable@).
setFaction ∷ EngineEnv → UnitId → Faction → IO ()
setFaction env uid f =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.adjust
                (\u → u { uiFactionId = f }) uid (umInstances um) }
        , ())

-- | Move one unit onto another world page, leaving its coordinates
--   alone — the #1415 cross-page fixture. The two sides stay at EQUAL
--   coordinates on purpose: a distance rule cannot tell two endpoints
--   sharing one tile apart, so a refusal there is page identity and
--   nothing else.
setPage ∷ EngineEnv → UnitId → WorldPageId → IO ()
setPage env uid pg =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.adjust
                (\u → u { uiPage = pg }) uid (umInstances um) }
        , ())

-- | Remove a unit outright: the instance is gone, so every engine
--   query about it stops resolving.
removeUnit ∷ EngineEnv → UnitId → IO ()
removeUnit env uid =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.delete uid (umInstances um) }, ())

-- | Demolish a building the same way.
demolish ∷ EngineEnv → BuildingId → IO ()
demolish env bid =
    atomicModifyIORef' (buildingManagerRef env) $ \bm →
        (bm { bmInstances = HM.delete bid (bmInstances bm) }, ())

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
            , ("technomule", minimalDef "technomule" "Technomule")
            , ("wolf", minimalDef "wolf" "Wolf") ]
        , umInstances = HM.fromList
            [ (carrierUid, onPage
                  (mkUnit "acolyte" FactionPlayer (10, 10) 100
                          carrierInventory []))
            , (matesUid, onPage
                  (mkUnit "acolyte" FactionPlayer (60, 60) 100 [] []))
            , (muleUid, onPage
                  (mkUnit "technomule" FactionPlayer (70, 70) 200
                          muleInventory [])) ]
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
    , "_G.__events = {}; _G.__eventUids = {};"
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
    -- The uid a warning was ATTRIBUTED to, recorded in lockstep with
    -- its text so the two arrays index together: #1415 requirement 4
    -- is about the attribution, not merely about a count.
    , "    _G.__eventUids[#_G.__eventUids + 1] = tostring(uid);"
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
    -- Clearing the log clears BOTH arrays, or the next warning's uid
    -- would line up against an older event's text.
    , "_G.__clearEvents = function()"
    , "  _G.__events = {}; _G.__eventUids = {} end;"
    -- How many 'unit_warning' events were aimed at THIS uid.
    , "_G.__warnCount = function(uid)"
    , "  local n = 0;"
    , "  for i, e in ipairs(_G.__events) do"
    , "    if e:sub(1, 13) == 'unit_warning|'"
    , "       and _G.__eventUids[i] == tostring(uid) then n = n + 1 end"
    , "  end; return n end;"
    -- One AI tick of Mode A's actions for `uid`: score BOTH sides the
    -- way the dispatch loop does, then execute whichever won. Since
    -- #1251 review round 1 the source-side escort and the target-side
    -- hold are two registered actions, so scoring only one of them
    -- would model a loop this engine does not run. `_G.__ai[uid]` is
    -- that unit's aiState row, which the real actions read and write,
    -- and `_G.__lastAction` records WHICH side won — the two are
    -- mutually exclusive by construction, and that is worth asserting
    -- rather than assuming.
    , "_G.__ai = {};"
    , "_G.__lastAction = nil;"
    , "_G.__tick = function(uid)"
    , "  local tr = require('scripts.unit_ai_escort');"
    , "  _G.__ai[uid] = _G.__ai[uid] or {};"
    , "  local s = _G.__ai[uid];"
    , "  local best, run, name = -math.huge, nil, nil;"
    , "  local sides = {"
    , "    {'escort_transfer', tr.escortUtility, tr.escortExecute},"
    , "    {'escort_hold',     tr.holdUtility,   tr.holdExecute} };"
    , "  for _, c in ipairs(sides) do"
    , "    local u = c[2](uid, s);"
    , "    if u > best then best, run, name = u, c[3], c[1] end"
    , "  end;"
    , "  _G.__lastAction = (best > -math.huge) and name or nil;"
    , "  if best > -math.huge then run(uid, s) end;"
    , "  return best end;"
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

        it "a UNIT destination is held from CREATION — before any tick, \
           \before the source has walked anywhere, and with no reveal" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            created ← evalOk ls (createLua "unit" 2)
            created `shouldBe` "\"true\""
            -- #1251 requirement 1: the target's hold begins HERE, while
            -- the session is still approaching, which is what gives the
            -- source's walk a fixed destination. Read before a single
            -- tick has run.
            atCreation ← evalOk ls
                "return { phase = _G.__phase(), \
                \         src = _G.__session().roleOf(1), \
                \         dst = _G.__session().roleOf(2), \
                \         held = _G.__session().holdsUnit(2) }"
            atCreation `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            atCreation `shouldSatisfy` T.isInfixOf "\"src\":\"source\""
            atCreation `shouldSatisfy` T.isInfixOf "\"dst\":\"target\""
            atCreation `shouldSatisfy` T.isInfixOf "\"held\":true"
            -- ...and the lock it scores is the SAME 7.5 the escort's is,
            -- so neither end can outscore the other and the target's
            -- routine work (<=6.0) and even a move order (7.0) lose to
            -- it, exactly like any player order.
            targetScore ← evalOk ls "return tostring(_G.__tick(2))"
            targetScore `shouldBe` "\"7.5\""
            -- The TARGET side is what won, not the escort's: the two are
            -- mutually exclusive, which is what lets them be registered
            -- separately without either end being scored twice.
            side ← evalOk ls "return _G.__lastAction"
            side `shouldBe` "\"escort_hold\""
            -- Holding the target is STANDING, never approaching: it
            -- issues no walk of its own in either phase.
            heldStill ← evalOk ls
                "return { moves = _G.__moves, stopped = _G.__lastStop }"
            heldStill `shouldSatisfy` T.isInfixOf "\"moves\":0"
            heldStill `shouldSatisfy` T.isInfixOf "\"stopped\":2"
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
            holds `shouldSatisfy` T.isInfixOf "\"dst\":true"
            -- Still standing once the window is open, and still no walk.
            openMoves ← evalOk ls "_G.__tick(2); return _G.__moves"
            openMoves `shouldBe` "0"

        -- #1251 review round 1. `roleOf` makes EVERY unit destination
        -- the held target, but being a session's SOURCE is a per-species
        -- capability — so scoping the hold to the species that register
        -- `escort_transfer` would leave a legal target (endpoint
        -- eligibility is player-commandability and nothing else) whose
        -- AI never evaluated it: it would keep walking while an escort
        -- approached where it used to be. The hold is therefore
        -- registered universally, and the two questions stay separate.
        it "a commandable species that never registered the ESCORT is still \
           \held as a TARGET, while the SOURCE gate still refuses it" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            -- A REAL registration through the public API, as the #1250
            -- source-gate case does: uid 2's species can run neither the
            -- escort nor anything else this fixture drives.
            _ ← evalOk ls (luaLines
                [ "local a = require('scripts.unit_ai_actions');"
                , "a.byDef = {};"
                , "a.record('acolyte', { {name = 'wander'},"
                , "                      {name = 'escort_transfer'} });"
                , "a.record('bear', { {name = 'wander'} });"
                , "local um = unit.getInfo;"
                , "unit.getInfo = function(uid)"
                , "  local i = um(uid); if not i then return nil end;"
                , "  if uid == 2 then i.defName = 'bear' end;"
                , "  return i end;"
                , "return 'ok'" ])
            -- It is still an eligible ENDPOINT — that rule reads the
            -- live faction, never the action registry — so the session
            -- is created and it becomes the target.
            created ← evalOk ls (createLua "unit" 2)
            created `shouldBe` "\"true\""
            held ← evalOk ls
                "return { role = _G.__session().roleOf(2), \
                \         score = tostring(_G.__tick(2)), \
                \         won = _G.__lastAction, \
                \         stopped = _G.__lastStop, moves = _G.__moves }"
            held `shouldSatisfy` T.isInfixOf "\"role\":\"target\""
            held `shouldSatisfy` T.isInfixOf "\"score\":\"7.5\""
            held `shouldSatisfy` T.isInfixOf "\"won\":\"escort_hold\""
            held `shouldSatisfy` T.isInfixOf "\"stopped\":2"
            held `shouldSatisfy` T.isInfixOf "\"moves\":0"
            -- ...and the SOURCE question is untouched: the same species
            -- still cannot be made an escort, by either gate.
            asSource ← evalOk ls (luaLines
                [ "local s = require('scripts.transfer_session');"
                , "local ep = unit.transferEndpointInfo("
                , "  {kind = 'building', id = 7});"
                , "local made, reason = s.create(2, 'building', 7);"
                , "return { resolved = tostring(s.resolveSource({2}, nil, ep,"
                , "                               s.ESCORT_ACTION)),"
                , "         made = tostring(made),"
                , "         reason = tostring(reason) }" ])
            asSource `shouldSatisfy` T.isInfixOf "\"resolved\":\"nil\""
            asSource `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            asSource `shouldSatisfy`
                T.isInfixOf "\"reason\":\"source_not_escortable\""
            -- The refused creation left the running session alone, so
            -- the bear is STILL held as its target.
            intact ← evalOk ls
                "return { role = _G.__session().roleOf(2), \
                \         src = _G.__session().roleOf(1) }"
            intact `shouldSatisfy` T.isInfixOf "\"role\":\"target\""
            intact `shouldSatisfy` T.isInfixOf "\"src\":\"source\""

        it "a BUILDING destination still holds its source alone — there is \
           \no second endpoint to hold" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            holds ← evalOk ls
                "return { src = _G.__session().roleOf(1), \
                \         mate = tostring(_G.__session().roleOf(2)), \
                \         phase = _G.__phase() }"
            holds `shouldSatisfy` T.isInfixOf "\"src\":\"source\""
            holds `shouldSatisfy` T.isInfixOf "\"mate\":\"nil\""
            holds `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            bystander ← evalOk ls "return tostring(_G.__tick(2))"
            bystander `shouldBe` "\"-inf\""

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

        -- #1251 requirement 2: every path that ends a unit-to-unit
        -- session releases BOTH ends. The four below are the whole set —
        -- the coupled panel close, a successful replacement, Exit to
        -- Menu, and the successful-load reset — and each asserts release
        -- as the AI sees it (a -inf score, which is what actually lets
        -- the unit be steered again) rather than as a cleared table.
        it "closing the window releases BOTH units of a unit-to-unit \
           \session, and both can be steered again" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            open' ← evalOk ls "return _G.__phase()"
            open' `shouldBe` "\"open\""
            closed ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \_G.__stops = 0; cip.popLevel(); \
                \return { depth = cip.depth(), \
                \         session = tostring(_G.__session().get()), \
                \         src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2), \
                \         stops = _G.__stops }"
            closed `shouldSatisfy` T.isInfixOf "\"depth\":0"
            closed `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            closed `shouldSatisfy` T.isInfixOf "\"src\":false"
            closed `shouldSatisfy` T.isInfixOf "\"dst\":false"
            -- The teardown STOPPED each of them rather than merely
            -- letting go, which is what makes both idle and so what
            -- makes the next tick re-decide instead of running out an
            -- approach nobody wants any more.
            closed `shouldSatisfy` T.isInfixOf "\"stops\":2"
            -- Neither is held by anything now, so ordinary AI is free to
            -- steer both: the escort action itself concedes.
            scores ← evalOk ls
                "return { a = tostring(_G.__tick(1)), \
                \         b = tostring(_G.__tick(2)) }"
            scores `shouldSatisfy` T.isInfixOf "\"a\":\"-inf\""
            scores `shouldSatisfy` T.isInfixOf "\"b\":\"-inf\""
            -- ...and neither release issued a walk of its own, so a unit
            -- carrying its own orders is handed back where it stands.
            noWalks ← evalOk ls "return _G.__moves"
            noWalks `shouldBe` "0"

        it "a SUCCESSFUL replacement releases the PRIOR unit target, not \
           \just the source it reuses" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            heldBefore ← evalOk ls
                "return tostring(_G.__session().holdsUnit(2))"
            heldBefore `shouldBe` "\"true\""
            -- The same source, a DIFFERENT destination: the mate stops
            -- being an endpoint of anything and must be let go, or a
            -- unit the player never mentioned again stands pinned
            -- forever with no window left to close.
            _ ← evalOk ls (createLua "building" 7)
            after ← evalOk ls
                "return { phase = _G.__phase(), \
                \         src = _G.__session().holdsUnit(1), \
                \         mate = _G.__session().holdsUnit(2), \
                \         mateScore = tostring(_G.__tick(2)) }"
            after `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            after `shouldSatisfy` T.isInfixOf "\"src\":true"
            after `shouldSatisfy` T.isInfixOf "\"mate\":false"
            after `shouldSatisfy` T.isInfixOf "\"mateScore\":\"-inf\""

        it "Exit to Menu releases BOTH ends of a unit-to-unit session" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            _ ← evalOk ls "world.destroyAll = function() end; return 'ok'"
            exited ← evalOk ls
                "require('scripts.pause_menu').onExitToMenu(); \
                \return { session = tostring(_G.__session().get()), \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            exited `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            exited `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            exited `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""
            exited `shouldSatisfy` T.isInfixOf "\"depth\":0"

        -- The reset hook is registered through `registerResetHook`, and
        -- saveModules.applyAll runs those only after every component has
        -- applied successfully — so "the load reset releases both" is a
        -- statement about a SUCCESSFUL, session-replacing load. A failed
        -- load never reaches this hook at all and therefore leaves the
        -- running session and both its holds exactly as they were; that
        -- half is 'Test.Headless.Lua.SaveModules''s rollback coverage,
        -- which this must not duplicate by asserting on a stub.
        it "the successful-load reset releases BOTH units and leaves a \
           \restored durable Mode B order on a reused uid alone" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls
                "require('scripts.transfer_session').init('transfer_session'); \
                \return 'ok'"
            -- A REAL durable order on the very unit the session holds,
            -- created through the engine's own verb against the live
            -- per-page store — the D-3 hazard is that the transient
            -- session's teardown reaches into durable state a load just
            -- restored onto the same uid.
            queued ← evalOk ls
                "return #unit.getTransferOrders(1) .. '/' .. tostring(\
                \unit.createTransferOrder(1, { source = { kind = 'unit', \
                \  id = 1 }, destination = { kind = 'building', id = 7 }, \
                \  items = { { instanceId = 110, defName = 'rope' } } }) \
                \  ~= nil)"
            queued `shouldBe` "\"0/true\""
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            after ← evalOk ls
                "local sm = require('scripts.lib.save_modules'); \
                \sm.resetHooks['transfer_session'](); \
                \return { session = tostring(_G.__session().get()), \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)), \
                \         orders = #unit.getTransferOrders(1), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            after `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            after `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            after `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""
            after `shouldSatisfy` T.isInfixOf "\"depth\":0"
            -- The order is still there and still the carrier's: the
            -- release stops a unit, it never cancels or prunes its work.
            after `shouldSatisfy` T.isInfixOf "\"orders\":1"

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

    -- #1415. 'M.create' is the ONE place a session is built and is
    -- deliberately reusable by a surface that never ran
    -- 'M.resolveSource', so every rule that resolution enforces has to
    -- hold HERE too. The two shipped menus screen all of them before
    -- they call, which is why nothing player-facing changes; these
    -- cases therefore call 'create' DIRECTLY, with no menu, no
    -- resolution and no click, and read every answer back out of the
    -- real engine rather than a stub.
    describe "the creation boundary's own source rules (#1415)" $ do

        it "an uncommandable source creates no session, reports \
           \source_ineligible, and warns that unit exactly once" $
           \(env, ls) → do
            resetFixture env ls
            -- Faction loss is exactly what the engine's endpoint rule
            -- means: Unit.Transfer.endpointEligible is `uevCommandable`
            -- and nothing else, so this is the contract's own refusal
            -- read through the projection rather than a rule invented
            -- for the session.
            setFaction env carrierUid FactionWildlife
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'building', 7); \
                \return { made = tostring(s), reason = tostring(reason), \
                \         warns = _G.__warnCount(1) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy`
                T.isInfixOf "\"reason\":\"source_ineligible\""
            refused `shouldSatisfy` T.isInfixOf "\"warns\":1"
            after ← evalOk ls
                "return tostring(_G.__session().get())"
            after `shouldBe` "\"nil\""

        it "a self-transfer creates no session and reports \
           \receiver_ineligible — the id the ENGINE produces for \
           \from == to" $ \(env, ls) → do
            resetFixture env ls
            -- Nothing downstream ever caught this: 'endpointFailure'
            -- validates each side independently, so a session on one
            -- unit twice passed the liveness tick forever while every
            -- commit was refused by planItemWith's `from == to`.
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'unit', 1); \
                \return { made = tostring(s), reason = tostring(reason), \
                \         warns = _G.__warnCount(1) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy`
                T.isInfixOf "\"reason\":\"receiver_ineligible\""
            refused `shouldSatisfy` T.isInfixOf "\"warns\":1"
            after ← evalOk ls "return tostring(_G.__session().get())"
            after `shouldBe` "\"nil\""

        it "a cross-page pair creates no session and reports \
           \out_of_range, at IDENTICAL coordinates so only the page \
           \differs" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (10, 10)
            placeUnit env matesUid  (10, 10)
            setPage env matesUid otherPage
            -- Distance 0 and still refused: 'reachable' fails on page
            -- identity before it ever measures, and this asserts the
            -- session agrees with it. Left unchecked the escort simply
            -- never arrived, so the session sat in `approaching` with
            -- nothing ever going wrong enough for the liveness tick to
            -- close it.
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'unit', 2); \
                \return { made = tostring(s), reason = tostring(reason), \
                \         warns = _G.__warnCount(1) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy` T.isInfixOf "\"reason\":\"out_of_range\""
            refused `shouldSatisfy` T.isInfixOf "\"warns\":1"
            after ← evalOk ls "return tostring(_G.__session().get())"
            after `shouldBe` "\"nil\""

        it "a DESTINATION failure still wins over a source one, so a call \
           \wrong on both sides reports the destination" $
           \(env, ls) → do
            resetFixture env ls
            setFaction env carrierUid FactionWildlife
            -- Building 999 does not exist AND the source is
            -- uncommandable. The placement rule the escort-capability
            -- check already documented says the destination answers.
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'building', 999); \
                \return tostring(reason)"
            refused `shouldBe` "\"receiver_missing\""

        it "each of the three new refusals leaves a RUNNING session — its \
           \identity, phase, hold, panes, both inventories and its stop \
           \count — exactly as it found them" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- One joined STRING rather than a table, so the comparison
            -- below is a real equality and not a bet on key order.
            let snapshot = "local cip = require('scripts.cargo_inventory_panel'); \
                           \return table.concat({ _G.__session().get().id, \
                           \  _G.__phase(), _G.__session().roleOf(1), \
                           \  cip.depth(), cip.dump().levels[1].paneCount, \
                           \  _G.__ids('unit', 1), _G.__ids('building', 7), \
                           \  _G.__stops }, '|')"
            before ← evalOk ls snapshot
            -- Three refusals in a row, one per NEW branch, and none of
            -- them may disturb what is open. Two of them are not even
            -- ABOUT this session: a refusal naming other units has to
            -- leave it alone just as surely as one naming its own.
            setPage env muleUid otherPage
            setFaction env matesUid FactionWildlife
            reasons ← evalOk ls
                "local s = _G.__session(); \
                \local _, a = s.create(1, 'unit', 1); \
                \local _, b = s.create(1, 'unit', 3); \
                \local _, c = s.create(2, 'building', 8); \
                \return { a = tostring(a), b = tostring(b), \
                \         c = tostring(c) }"
            reasons `shouldSatisfy` T.isInfixOf "\"a\":\"receiver_ineligible\""
            reasons `shouldSatisfy` T.isInfixOf "\"b\":\"out_of_range\""
            reasons `shouldSatisfy` T.isInfixOf "\"c\":\"source_ineligible\""
            after ← evalOk ls snapshot
            after `shouldBe` before



    -- #1254 (UIT-5B): every way a session can be interrupted.
    --
    -- Structured the way the issue's own review asked for, and NOT as a
    -- trigger-by-endpoint cross-product: each TRIGGER is proved to
    -- delegate to the one canonical teardown, and that teardown is
    -- proved separately to release both holds and both panes. The two
    -- together are what cover requirement 6 for every trigger, without
    -- twelve near-identical cases whose failures would all mean the
    -- same thing.
    describe "session failure handling (#1254)" $ do

        -- The per-tick liveness rule. It is the SESSION's own tick and
        -- not the window's on purpose: the approach has no window.
        it "an endpoint demolished while the panes are OPEN ends the \
           \session on its own tick — panes, hold and identity together" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            open' ← evalOk ls "return _G.__phase()"
            open' `shouldBe` "\"open\""
            demolish env holdBid
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         held = _G.__session().holdsUnit(1), \
                \         score = tostring(_G.__tick(1)) }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"
            gone `shouldSatisfy` T.isInfixOf "\"held\":false"
            gone `shouldSatisfy` T.isInfixOf "\"score\":\"-inf\""

        it "…and one demolished mid-APPROACH ends it too, which is the \
           \half of a session's life no window can notice" $
           \(env, ls) → do
            resetFixture env ls
            -- Far from the hold: this session never opens a window, so
            -- the container manager's own per-tick `stillThere` hook has
            -- nothing to run against and only this tick can see it.
            _ ← evalOk ls (createLua "building" 7)
            approaching ← evalOk ls "return _G.__phase()"
            approaching `shouldBe` "\"approaching\""
            demolish env holdBid
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1), \
                \         score = tostring(_G.__tick(1)) }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"held\":false"
            gone `shouldSatisfy` T.isInfixOf "\"score\":\"-inf\""

        -- Requirement 2. The contract cannot answer this one:
        -- `Unit.Transfer.endpointEligible` is `uevCommandable` alone, so
        -- a corpse is still a perfectly eligible endpoint by its lights
        -- and the panes would happily keep rendering its inventory.
        it "a DEAD held unit ends the session, even though the transfer \
           \contract still calls it an eligible endpoint" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- The contract's own verdict first, so this case cannot
            -- quietly become a test of a rule that moved elsewhere.
            setPose env carrierUid "dead"
            contract ← evalOk ls
                "local i = unit.transferEndpointInfo({kind='unit', id=1}); \
                \return tostring(i ~= nil and i.eligible)"
            contract `shouldBe` "\"true\""
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"

        it "an UNCONSCIOUS held target ends it, and a merely crawling one \
           \does not — the session sits through a recoverable pose" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            setPose env matesUid "crawling"
            survives ← evalOk ls
                "_G.__session().update(0); \
                \return { phase = _G.__phase(), \
                \         src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2) }"
            survives `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            survives `shouldSatisfy` T.isInfixOf "\"src\":true"
            survives `shouldSatisfy` T.isInfixOf "\"dst\":true"
            setPose env matesUid "collapsed"
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)) }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"
            gone `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            gone `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""

        it "a held unit that leaves the player's factions ends it" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            setFaction env matesUid FactionWildlife
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"

        -- The review's "a missing FIRST endpoint must not prevent
        -- cleanup of the other endpoint or either panel": the source is
        -- checked first and is the one that vanished, so if the teardown
        -- were not step-isolated the TARGET would stay pinned forever
        -- with no window left to release it.
        it "a source that stopped existing still releases the OTHER end \
           \and still closes both panes" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            removeUnit env carrierUid
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         dst = _G.__session().holdsUnit(2), \
                \         dstScore = tostring(_G.__tick(2)) }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"
            gone `shouldSatisfy` T.isInfixOf "\"dst\":false"
            gone `shouldSatisfy` T.isInfixOf "\"dstScore\":\"-inf\""

        -- The negative control the six above need: the tick must not be
        -- a session's own worst enemy.
        it "a healthy session survives its tick indefinitely, in either \
           \phase" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            walking ← evalOk ls
                "_G.__session().update(0); _G.__session().update(0); \
                \return { phase = _G.__phase(), \
                \         stale = tostring(_G.__session().staleReason()), \
                \         held = _G.__session().holdsUnit(1) }"
            walking `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            walking `shouldSatisfy` T.isInfixOf "\"stale\":\"nil\""
            walking `shouldSatisfy` T.isInfixOf "\"held\":true"
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls "return _G.__tick(1)"
            standing ← evalOk ls
                "_G.__session().update(0); _G.__session().update(0); \
                \return { phase = _G.__phase(), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         held = _G.__session().holdsUnit(1) }"
            standing `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            standing `shouldSatisfy` T.isInfixOf "\"depth\":1"
            standing `shouldSatisfy` T.isInfixOf "\"held\":true"

        -- Requirement 3, signed off 2026-08-11. The boundary itself;
        -- the real player INGRESS that calls it is pinned in
        -- 'Test.Headless.UI.TransferContextMenu'.
        it "a new player order to a held unit ends the session and the \
           \order proceeds — from EITHER side of a two-sided hold" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            -- Ordering the TARGET, not the escort: being an endpoint of
            -- somebody else's session must not make a unit unorderable.
            ordered ← evalOk ls
                "local core = require('scripts.unit_ai_core'); \
                \local ended = _G.__session().notePlayerOrder(2); \
                \package.loaded['scripts.unit_ai'].commandMove(2, 30, 30); \
                \local task = core.ensureState(2).commandedTask; \
                \return { ended = ended, \
                \         session = tostring(_G.__session().get()), \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         ordered = tostring(task ~= nil and task.x) }"
            ordered `shouldSatisfy` T.isInfixOf "\"ended\":true"
            ordered `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            ordered `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            ordered `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""
            ordered `shouldSatisfy` T.isInfixOf "\"depth\":0"
            -- The command really did land, and survived the teardown
            -- that ran before it.
            ordered `shouldSatisfy` T.isInfixOf "\"ordered\":\"30\""

        it "the boundary ignores a unit no session holds, so an ordinary \
           \order costs an unrelated session nothing" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            untouched ← evalOk ls
                "local ended = _G.__session().notePlayerOrder(3); \
                \return { ended = ended, phase = _G.__phase(), \
                \         src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            untouched `shouldSatisfy` T.isInfixOf "\"ended\":false"
            untouched `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            untouched `shouldSatisfy` T.isInfixOf "\"src\":true"
            untouched `shouldSatisfy` T.isInfixOf "\"dst\":true"
            untouched `shouldSatisfy` T.isInfixOf "\"depth\":1"

        -- The other half of the review's "not move-only" pair: the
        -- session's OWN movement is not a player order and must not
        -- cancel the thing that issued it. The escort's approach is a
        -- real `unit.moveTo` through the AI action, so this drives that
        -- action rather than asserting about a call that never happened.
        it "the escort's own approach does NOT self-cancel: an internal \
           \move is not a player order" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            walked ← evalOk ls
                "_G.__moves = 0; _G.__tick(1); _G.__tick(1); \
                \return { moves = _G.__moves, phase = _G.__phase(), \
                \         held = _G.__session().holdsUnit(1) }"
            walked `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            walked `shouldSatisfy` T.isInfixOf "\"held\":true"
            walked `shouldNotSatisfy` T.isInfixOf "\"moves\":0"

        -- Requirement 4's registry half (#156): the container-window
        -- entry beside it covers an OPEN session by closing its window,
        -- and this one is why a second entry exists at all.
        it "a HUD hide ends a session still APPROACHING, through the \
           \view-teardown registry rather than a one-off call" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            hidden ← evalOk ls
                "require('scripts.ui.view_teardown').run('hudHide'); \
                \return { session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1), \
                \         score = tostring(_G.__tick(1)) }"
            hidden `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            hidden `shouldSatisfy` T.isInfixOf "\"held\":false"
            hidden `shouldSatisfy` T.isInfixOf "\"score\":\"-inf\""

        it "…and a zoom-band change does, while a layout-only resize \
           \still leaves an approaching session alone" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            resized ← evalOk ls
                "require('scripts.ui.view_teardown').run('resize'); \
                \return { phase = _G.__phase(), \
                \         held = _G.__session().holdsUnit(1) }"
            resized `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            resized `shouldSatisfy` T.isInfixOf "\"held\":true"
            banded ← evalOk ls
                "require('scripts.ui.view_teardown').run('zoomBand', \
                \    { worldId = 'transfer_session_page', newView = 'zoomed_out' }); \
                \return { session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1) }"
            banded `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            banded `shouldSatisfy` T.isInfixOf "\"held\":false"

        -- The load reset is the ONE path whose recorded uids no longer
        -- name the units they did: saveModules.applyAll fires reset
        -- hooks only after every component has applied, and entity ids
        -- are reused across sessions. Panels and identity still go.
        it "the successful-load reset drops the session without stopping \
           \whatever the load restored onto its stale uids" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls
                "require('scripts.transfer_session').init('transfer_session'); \
                \return 'ok'"
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            after ← evalOk ls
                "local sm = require('scripts.lib.save_modules'); \
                \_G.__stops = 0; sm.resetHooks['transfer_session'](); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         stops = _G.__stops, \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)) }"
            after `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            after `shouldSatisfy` T.isInfixOf "\"depth\":0"
            after `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            after `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""
            -- Every OTHER teardown stops what it held; this one must
            -- not, because uid 1 and uid 2 are now somebody else.
            after `shouldSatisfy` T.isInfixOf "\"stops\":0"

        -- Requirement 5 (D-9). An abnormal close is not a special kind
        -- of close: the stack it leaves behind has to be the same empty
        -- stack an ordinary dismissal leaves.
        it "after an abnormal close the stack is empty and the next \
           \container window opens and renders normally" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            setPose env carrierUid "dead"
            _ ← evalOk ls "_G.__session().update(0); return 'ok'"
            reopened ← evalOk ls (luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "local before = cip.depth();"
                , "cip.openFor('building', 7, 10, 10);"
                , "local d = cip.dump();"
                , "local il = require('scripts.ui.item_list');"
                , "local pane = cip.getPane(cip.getLevel(1), 'main');"
                , "return { before = before, depth = d.depth,"
                , "         kind = d.levels[1].kind,"
                , "         rows = #il.getRows(pane.listId),"
                , "         session = tostring(_G.__session().get()) }" ])
            reopened `shouldSatisfy` T.isInfixOf "\"before\":0"
            reopened `shouldSatisfy` T.isInfixOf "\"depth\":1"
            reopened `shouldSatisfy` T.isInfixOf "\"kind\":\"endpoint\""
            reopened `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            reopened `shouldNotSatisfy` T.isInfixOf "\"rows\":0"

        -- Requirement 7, and exactly as far as it goes: per-REQUEST
        -- atomicity. A session owns no transaction, so ending one can
        -- neither half-move an item nor undo a move that completed.
        it "an abnormal close leaves an already-committed transfer \
           \committed — no rollback, no half-moved item" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            moved ← evalOk ls
                "local m = _G.__rowMenu('source', 'ration'); \
                \return tostring(_G.__fire(m, 'Store all'))"
            moved `shouldBe` "\"true\""
            before ← evalOk ls "return _G.__ids('building', 7)"
            before `shouldSatisfy` T.isInfixOf "ration#101"
            demolish env holdBid
            _ ← evalOk ls "_G.__session().update(0); return 'ok'"
            -- The building is gone, so the surviving record of the
            -- commit is the CARRIER's own inventory: the three rations
            -- left it and did not come back.
            after ← evalOk ls
                "return { session = tostring(_G.__session().get()), \
                \         carrier = _G.__ids('unit', 1) }"
            after `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            after `shouldNotSatisfy` T.isInfixOf "ration#101"
            after `shouldSatisfy` T.isInfixOf "rope#110"

    describe "the session's row gestures" $ do
        let openOnHold env ls = do
                resetFixture env ls
                placeUnit env carrierUid (42, 41)
                _ ← evalOk ls (createLua "building" 7)
                _ ← evalOk ls "return _G.__tick(1)"
                pure ()
            -- #1251: a unit-to-unit session, already arrived, with both
            -- endpoints carrying something. `destUid` is the ONLY thing
            -- that differs between the two species pairings below, which
            -- is the point — an endpoint is an endpoint.
            openPair env ls destUid destStock = do
                resetFixture env ls
                stockUnit env carrierUid carrierInventory []
                stockUnit env destUid destStock []
                placeUnit env carrierUid (10, 10)
                placeUnit env destUid (11, 10)
                let destId = fromIntegral (unUnitId destUid) ∷ Int
                created ← evalOk ls (createLua "unit" destId)
                created `shouldBe` "\"true\""
                _ ← evalOk ls ("_G.__tick(1); return _G.__tick("
                                <> tshow destId <> ")")
                phase ← evalOk ls "return _G.__phase()"
                phase `shouldBe` "\"open\""
                both ← evalOk ls ("return { src = _G.__session().roleOf(1), \
                                  \         dst = _G.__session().roleOf("
                                  <> tshow destId <> ") }")
                both `shouldSatisfy` T.isInfixOf "\"src\":\"source\""
                both `shouldSatisfy` T.isInfixOf "\"dst\":\"target\""
                pure destId

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
            _ ← evalOk ls "_G.__clearEvents(); return 'ok'"
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
            -- The mate is TELEPORTED away — a direct fixture write, not
            -- a walk, so #1251's hold on it is not what is under test
            -- here and could not have prevented this. What is under test
            -- is that the COMMIT is authoritative about reach however
            -- the drift happened. Nothing re-approaches, and UIT-5B
            -- (#1254) deliberately left that so: a live, commandable
            -- endpoint that merely drifted is not a session failure.
            placeUnit env matesUid (60, 60)
            _ ← evalOk ls "_G.__clearEvents(); return 'ok'"
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

        -- #1251 requirement 3 / D-10: acolyte↔acolyte and mule↔acolyte
        -- are the SAME path. Store and Retrieve are exercised in one
        -- session each, because "both directions" is a property of the
        -- open window rather than of two separate gestures — the pane
        -- the player clicked is what picks the direction.
        it "acolyte to acolyte: Store and Retrieve both commit exact \
           \instances between two held units, in one session" $
           \(env, ls) → do
            _ ← openPair env ls matesUid mateInventory
            labels ← evalOk ls
                "return { store = _G.__labels(_G.__rowMenu('source', 'ration')), \
                \         take  = _G.__labels(_G.__rowMenu('destination', 'bandage')) }"
            labels `shouldSatisfy` T.isInfixOf "Store 1|Store all"
            labels `shouldSatisfy` T.isInfixOf "Retrieve 1|Retrieve all"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('destination', 'bandage'), \
                \                          'Retrieve all'))"
            mate ← evalOk ls "return _G.__ids('unit', 2)"
            mate `shouldBe` "\"canteen#310,ration#101,ration#102,ration#103\""
            carrier ← evalOk ls "return _G.__ids('unit', 1)"
            carrier `shouldBe` "\"bandage#301,bandage#302,rope#110\""
            -- Repeatable while adjacent, exactly as against a building:
            -- neither commit ended the session or moved the window.
            still ← evalOk ls
                "return { phase = _G.__phase(), \
                \         src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            still `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            still `shouldSatisfy` T.isInfixOf "\"src\":true"
            still `shouldSatisfy` T.isInfixOf "\"dst\":true"
            still `shouldSatisfy` T.isInfixOf "\"depth\":1"

        it "acolyte to technomule commits identically — a DIFFERENT \
           \species on the far end changes nothing" $ \(env, ls) → do
            _ ← openPair env ls muleUid muleInventory
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'rope'), \
                \                          'Store 1'))"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('destination', 'steel_plate'), \
                \                          'Retrieve all'))"
            mule ← evalOk ls "return _G.__ids('unit', 3)"
            mule `shouldBe` "\"rope#110\""
            carrier ← evalOk ls "return _G.__ids('unit', 1)"
            carrier `shouldBe`
                "\"ration#101,ration#102,ration#103,\
                \steel_plate#401,steel_plate#402\""

        it "a technomule may be the ESCORT too: the source side is no more \
           \species-specific than the target side" $ \(env, ls) → do
            resetFixture env ls
            stockUnit env muleUid muleInventory []
            placeUnit env muleUid (10, 10)
            placeUnit env matesUid (11, 10)
            created ← evalOk ls
                "return tostring(_G.__session().create(3, 'unit', 2) ~= nil)"
            created `shouldBe` "\"true\""
            opened ← evalOk ls "_G.__tick(3); _G.__tick(2); return _G.__phase()"
            opened `shouldBe` "\"open\""
            roles ← evalOk ls
                "return { src = _G.__session().roleOf(3), \
                \         dst = _G.__session().roleOf(2), \
                \         carrier = tostring(_G.__session().roleOf(1)) }"
            roles `shouldSatisfy` T.isInfixOf "\"src\":\"source\""
            roles `shouldSatisfy` T.isInfixOf "\"dst\":\"target\""
            roles `shouldSatisfy` T.isInfixOf "\"carrier\":\"nil\""
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'steel_plate'), \
                \                          'Store all'))"
            mate ← evalOk ls "return _G.__ids('unit', 2)"
            mate `shouldBe` "\"steel_plate#401,steel_plate#402\""

        -- #1251 requirement 4 (D-6, contract-enforced — not
        -- reimplemented here). 'Test.Headless.Unit.Transfer' owns the
        -- policy proof; what this adds is that BOTH of the session's own
        -- surfaces agree with it, which is the only place a unit-to-unit
        -- session could have introduced a way around it.
        it "a worn accessory is not a session row at all, and the contract \
           \still refuses it by name if anything names it anyway" $
           \(env, ls) → do
            _ ← openPair env ls matesUid mateInventory
            -- Put the robe on the carrier with the window already open,
            -- then refresh, so the pane is re-derived from the endpoint
            -- as it stands rather than from an opening snapshot.
            stockUnit env carrierUid carrierInventory carrierWorn
            _ ← evalOk ls
                "local c = require('scripts.cargo_inventory_panel'); \
                \c.refreshLevel(c.getLevel(1)); return 'ok'"
            -- It weighs into the endpoint's load — 3 x 0.5 + 2.0 loose,
            -- plus the 1.5 robe — which is the whole reason the contract
            -- keeps worn gear visible to the capacity gate...
            weighed ← evalOk ls
                "local i = unit.transferEndpointInfo({kind='unit', id=1}); \
                \return string.format('%.2f', i.storedWeight)"
            weighed `shouldBe` "\"5.00\""
            -- ...but it is not among the rows, so no menu can offer it.
            rows ← evalOk ls "return _G.__ids('unit', 1)"
            rows `shouldBe` "\"ration#101,ration#102,ration#103,rope#110\""
            noMenu ← evalOk ls
                "return tostring(_G.__rowMenu('source', 'acolyte_robe'))"
            noMenu `shouldBe` "\"nil\""
            -- And naming it directly on the session's OWN endpoint pair
            -- — the identical request `commitNow` builds — is refused by
            -- the contract's own reason, with nothing moved.
            refused ← evalOk ls
                "local s = _G.__session().get(); \
                \local r = unit.commitTransfer({ source = s.source, \
                \  destination = s.destination, \
                \  items = { { instanceId = 120, defName = 'acolyte_robe' } } }); \
                \local o = r and (r.outcomes or {})[1]; \
                \return { accepted = r and r.accepted, \
                \         state = o and o.state, reason = o and o.reason }"
            refused `shouldSatisfy` T.isInfixOf "\"accepted\":true"
            refused `shouldSatisfy` T.isInfixOf "\"state\":\"failed\""
            refused `shouldSatisfy` T.isInfixOf "\"reason\":\"item_not_transferable\""
            intact ← evalOk ls
                "return { mate = _G.__ids('unit', 2), \
                \         carrier = _G.__ids('unit', 1) }"
            intact `shouldSatisfy`
                T.isInfixOf "\"mate\":\"bandage#301,bandage#302,canteen#310\""
            intact `shouldSatisfy`
                T.isInfixOf "\"carrier\":\"ration#101,ration#102,ration#103,rope#110\""

    describe "registration" $ do
        -- Source guards beside the behavioural cases above, following
        -- the "random stream ownership" precedent: neither side of the
        -- hold is reachable unless it is in the real action lists, and
        -- nothing this fixture can drive would notice its absence.
        it "the escort action is registered for every player-commandable \
           \species the source rule can resolve" $ \_ → do
            src ← TIO.readFile "scripts/unit_ai.lua"
            let regs = length (T.breakOnAll "transfer.escortAction," src)
            regs `shouldBe` 2

        -- #1251 review round 1: the TARGET side must reach every
        -- species, so it lives in the auto-prepended universal list and
        -- NOWHERE else. Two per-species mentions would be the same
        -- allowlist this exists to avoid, one species at a time.
        it "the target-side hold is registered UNIVERSALLY — once, in the \
           \list every species is given, never per species" $ \_ → do
            src ← TIO.readFile "scripts/unit_ai.lua"
            -- Named exactly once, and that once is inside the universal
            -- list: a second mention would be the per-species allowlist
            -- this exists to avoid, one species at a time.
            let named = length (T.breakOnAll "transfer.escortHoldAction" src)
            named `shouldBe` 1
            let universal = snd (T.breakOn "local UNIVERSAL_ACTIONS = {" src)
                listBody  = fst (T.breakOn "\n}" universal)
            listBody `shouldSatisfy` T.isInfixOf "transfer.escortHoldAction"
            -- ...and registerActions really prepends that list, so a
            -- species registering through the public API gets it
            -- without asking.
            src `shouldSatisfy`
                T.isInfixOf "for _, a in ipairs(UNIVERSAL_ACTIONS) do"
