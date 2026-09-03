-- | The one real scene every Mode A escort-session contract group
--   runs against (#1250, epic #1013 slice UIT-3B), and the single
--   source of truth for its ids, its world, its Lua and its helpers.
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
--   'withSharedFixture' stands up ONE headless engine and ONE Lua
--   state for the whole aggregate gate, and every behaviour group
--   composed under it calls 'resetFixture' per case; the wrapper
--   install-once guard lives in 'sceneLua' and nowhere else. Splitting
--   the groups across modules must not multiply any of those three,
--   which is why they are defined here rather than beside the cases
--   that use them.
module Test.Headless.UI.TransferSession.Fixture
    ( -- * Fixture ids
      carrierUid
    , matesUid
    , muleUid
    , holdBid
    , tinyBid
    , fixturePage
    , otherPage
      -- * Fixture inventories
    , carrierInventory
    , holdStorage
    , muleInventory
    , mateInventory
    , carrierWorn
      -- * Scene mutators
    , placeUnit
    , stockUnit
    , setPose
    , setFaction
    , setPage
    , removeUnit
    , demolish
    , stockTall
      -- * Lifecycle
    , withSharedFixture
    , resetFixture
      -- * Lua
    , evalOk
    , luaLines
    , sceneLua
    , createLua
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
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
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)

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
    writeIORef (worldManagerRef env) emptyWorldManager
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
