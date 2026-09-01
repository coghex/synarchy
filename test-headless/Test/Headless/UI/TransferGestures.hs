-- | Mode B's player gestures (#1249, epic #1013 phase 3): the "Store"
--   entries on a unit-info inventory row and the "Retrieve" entries on a
--   container-window row.
--
--   Registered under a describe beginning "Transfer context menu" so
--   @--match "Transfer context menu"@ reaches this alongside
--   'Test.Headless.UI.TransferContextMenu' (Mode A's own entry), which
--   is the gate the issue names.
--
--   What separates this fixture from that one is that a gesture here
--   must land in a REAL durable order store: the whole promotion is
--   "queue an order instead of moving an item now", so a spec that
--   stubbed @unit.createTransferOrder@ would prove only that a callback
--   ran. So this builds real units, a real building, real item
--   instances and a real page (reusing 'Test.Headless.Unit.TransferApi'
--   's constructors), drives the REAL production Lua — the shared
--   'scripts.transfer_gestures' builder through both of its hosts — and
--   reads the result back through @unit.getTransferOrders@.
--
--   Distance is the point of every scenario: the cargo hold sits thirty
--   tiles from everyone, so no gesture here could have been performed by
--   the adjacency-bound verbs this issue retired.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Transfer context menu (Mode B"'@.
module Test.Headless.UI.TransferGestures (spec) where

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

-- | uid 1, the acolyte whose unit-info panel supplies the "Store" rows.
carrierUid ∷ UnitId
carrierUid = UnitId 1

-- | uid 2, the acolyte a "Retrieve" resolves to. Deliberately NOT the
--   carrier, so a Store and a Retrieve in the same scene name different
--   executors and a swapped direction cannot pass.
retrieverUid ∷ UnitId
retrieverUid = UnitId 2

-- | uid 3, wildlife: selectable, never commandable, so it can prove the
--   source rule filters before it ranks.
wolfUid ∷ UnitId
wolfUid = UnitId 3

-- | uid 4, a @FactionDebug@ brown bear (#2030). Player-COMMANDABLE — the
--   debug overlay spawns any armed species with that faction and
--   'Unit.Faction.isPlayerCommandable' admits it — but @bear_brown@
--   registers no @transfer_order@ action, so every Mode B gate has to
--   refuse it for a reason the commandability filter cannot see.
--
--   Stands at (13,13): NEARER the hold at (40,40) than the acolyte at
--   (12,12), so a Retrieve that ranked before it filtered would pick it.
bearUid ∷ UnitId
bearUid = UnitId 4

-- | uid 5, a @FactionDebug@ red squirrel — the second species the issue
--   names, and nearer still at (14,14), so it is what an unfiltered
--   ranking actually resolves.
squirrelUid ∷ UnitId
squirrelUid = UnitId 5

-- | uid 6, a technomule: the OTHER species that registers the order, and
--   the farthest unit in the scene at (9,9). A Retrieve resolving it
--   over two nearer debug animals is what proves the filter picks the
--   nearest CAPABLE candidate rather than merely excluding a species.
technomuleUid ∷ UnitId
technomuleUid = UnitId 6

-- | 1x1, Built, capacity 200 — thirty tiles from every unit, which is
--   what makes every order here an at-a-distance one.
farHold ∷ BuildingId
farHold = BuildingId 7

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "transfer_gesture_page"

-- * Fixture

-- | The carrier's loose inventory: THREE identical rations, which the
--   shared widget merges into one row, plus one rope, which stays a
--   single-instance row. That pair is what separates "1 and all" from
--   "1 alone" without a second scene.
--
--   The kit is a real item CONTAINER holding a bandage, so the
--   item-container level a scenario descends into is a genuine one the
--   window resolves through @unit.getItemContents@ — not a synthetic
--   level kind invented to have something non-transferable to point at.
carrierInventory ∷ [ItemInstance]
carrierInventory =
    [ mkItem "ration" 101 0.5
    , mkItem "ration" 102 0.5
    , mkItem "ration" 103 0.5
    , mkItem "rope"   110 2.0
    , (mkItem "kit" 120 1.0) { iiContents = [mkItem "bandage" 121 0.05] }
    ]

-- | One loose row apiece for the two unsupported debug species, so the
--   Store gesture they must NOT be offered has a real row to be offered
--   on. Distinct instance ids, so an order that escaped either gate
--   would name the species that queued it.
bearInventory ∷ [ItemInstance]
bearInventory = [ mkItem "rope" 401 2.0 ]

squirrelInventory ∷ [ItemInstance]
squirrelInventory = [ mkItem "rope" 501 2.0 ]

-- | The hold's real storage, mirroring that shape from the other side:
--   two identical steel bars and one lone crowbar.
holdStorage ∷ [ItemInstance]
holdStorage =
    [ mkItem "steel_bar" 201 2.0
    , mkItem "steel_bar" 202 2.0
    , mkItem "crowbar"   210 3.0
    ]

onPage ∷ UnitInstance → UnitInstance
onPage u = u { uiPage = fixturePage }

-- | Reset every manager to the scene above, plus a live page carrying
--   its own @wsTransferOrdersRef@ — the durable owner an accepted
--   gesture must actually reach.
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
            , ("wolf", minimalDef "wolf" "Wolf")
            -- The REAL def names scripts/bear_ai.lua and
            -- scripts/red_squirrel_ai.lua register under, so the gate
            -- reads the shipped registry rather than a synthetic entry
            -- planted beside it.
            , ("bear_brown", minimalDef "bear_brown" "Brown Bear")
            , ("red_squirrel", minimalDef "red_squirrel" "Red Squirrel") ]
        , umInstances = HM.fromList
            [ (carrierUid, onPage
                  (mkUnit "acolyte" FactionPlayer (10, 10) 100
                          carrierInventory []))
            , (retrieverUid, onPage
                  (mkUnit "acolyte" FactionPlayer (12, 12) 100 [] []))
            , (wolfUid, onPage
                  (mkUnit "wolf" FactionWildlife (11, 11) 100 [] []))
            , (bearUid, onPage
                  (mkUnit "bear_brown" FactionDebug (13, 13) 100
                          bearInventory []))
            , (squirrelUid, onPage
                  (mkUnit "red_squirrel" FactionDebug (14, 14) 100
                          squirrelInventory []))
            , (technomuleUid, onPage
                  (mkUnit "technomule" FactionPlayer (9, 9) 100 [] [])) ]
        }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.singleton "cargo_hold"
                       (storageDef "cargo_hold" "Cargo Hold" (1, 1) 0 200)
        , bmInstances = HM.singleton farHold
              ((mkBuilding "cargo_hold" (40, 40) (1, 1) holdStorage)
                  { biPage = fixturePage })
        }

withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
withSharedFixture action = withHeadlessEngine $ \env → do
    ls ← newBareLuaBackend env
    action (env, ls)

-- | Every case starts from a clean page manager, a clean Lua module
--   table and the world above. Wiping @package.loaded@ is what keeps the
--   container window's stack, the gesture module and the unit-info
--   singleton from leaking between cases.
resetFixture ∷ EngineEnv → LuaBackendState → IO ()
resetFixture env ls = do
    writeIORef (uiManagerRef env) emptyUIPageManager
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

-- | Stands the container window up on a real page, preloads the
--   unit-info singleton the context-menu module attaches to, and
--   installs the two harness helpers every case drives the gesture
--   through.
--
--   @building.refreshContainerKnowledge@ is called for real rather than
--   stubbed: the window renders the player's REMEMBERED contents
--   (#1237), so without an observation on record it would show
--   "never inspected" and no rows at all. Taking a genuine snapshot of
--   the fixture's own live storage is what makes the rows here the same
--   rows a player would be looking at.
sceneLua ∷ Text
sceneLua = luaLines
    [ "_G.__page = UI.newPage('gesture_base', 'overlay');"
    , "UI.showPage(_G.__page);"
    -- The REAL per-species action registry, loaded HERE rather than
    -- left to the lazy require inside `transfer_gestures.queueOrder`
    -- (#2030). That require fires only after a menu callback has
    -- already run, so at the moment a Store or Retrieve menu is BUILT
    -- the registry would still be empty -- and an empty registry
    -- answers "yes" for every species by unit_ai_actions' own
    -- deliberate design, which would make every omission case below
    -- pass with the gate reverted. Loading `scripts.unit_ai` is what
    -- registers acolyte, technomule, bear_brown and red_squirrel from
    -- the shipped lists, exactly as gameplay boot does.
    , "require('scripts.unit_ai');"
    -- The unit-info singleton the context-menu module binds at load
    -- time. Only the fields that module reads are needed; its rendering
    -- is not what this gate is about.
    , "package.loaded['scripts.unit_info_v2'] ="
    , "  { activeUid = ", tshow 1, ", equipSlots = {}, accessoryRows = {} };"
    , "package.loaded['scripts.unit_info_v2_inventory'] ="
    , "  { invalidate = function() end };"
    , "local cip = require('scripts.cargo_inventory_panel');"
    , "cip.setup({page = _G.__page, fbW = 1920, fbH = 1080,"
    , "           boxTexSet = 1, menuFont = 1});"
    , "building.refreshContainerKnowledge(", tshow 7, ");"
    -- Capture whatever menu a gesture opens, without ever serializing a
    -- Lua function back to Haskell.
    , "local cm = require('scripts.ui.context_menu');"
    , "_G.__captured = nil;"
    , "cm.show = function(items) _G.__captured = items end;"
    -- Right-click the unit-info inventory row standing for `defName`,
    -- built from the SAME shared widget grouping the real panel uses, so
    -- a merged row here is merged for the same reason it is on screen.
    -- ...for ANY unit the panel could be showing, because the Store
    -- gesture's executor IS that panel's active unit (#2030): the
    -- singleton's `activeUid` is what the context-menu module forwards,
    -- so a case about an unsupported carrier has to repoint it rather
    -- than pass a uid alongside.
    , "_G.__storeMenuAs = function(uid, defName, mutate)"
    , "  local il = require('scripts.ui.item_list');"
    , "  local rows = il.groupItems(unit.getInventory(uid) or {});"
    , "  local row; for _, r in ipairs(rows) do"
    , "    if r.defName == defName then row = r end end;"
    , "  if not row then return nil end;"
    , "  if mutate then mutate(row) end;"
    , "  _G.__captured = nil;"
    , "  require('scripts.unit_info_v2_context_menu');"
    , "  local uiv2 = package.loaded['scripts.unit_info_v2'];"
    , "  local prev = uiv2.activeUid; uiv2.activeUid = uid;"
    , "  uiv2.handleInvItemRightClick(row);"
    , "  uiv2.activeUid = prev;"
    , "  return _G.__captured end;"
    , "_G.__storeMenu = function(defName, mutate)"
    , "  return _G.__storeMenuAs(", tshow 1, ", defName, mutate) end;"
    -- ...and the container-window row standing for `defName`, through
    -- the widget's own right-click routing on the level's real list.
    , "_G.__windowMenu = function(defName)"
    , "  local il = require('scripts.ui.item_list');"
    , "  local level = cip.getLevel(); if not level then return nil end;"
    , "  local rows = il.getRows(level.listId);"
    , "  local hit; for _, r in ipairs(rows) do"
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
    -- Every order a unit carries, flattened to the identities this gate
    -- asserts on: direction, endpoints, and the exact ordered instance
    -- ids the batch names.
    -- Joined into ONE string rather than returned as an array: an EMPTY
    -- Lua array serializes to a JSON OBJECT, not [], so "this unit
    -- carries no order" and "this unit carries one" would decode through
    -- different shapes. The same convention the sibling context-menu
    -- spec uses for captured labels.
    , "_G.__orders = function(uid)"
    , "  local out = {};"
    , "  for i, o in ipairs(unit.getTransferOrders(uid) or {}) do"
    , "    local ids = {};"
    , "    for j, e in ipairs(o.entries or {}) do ids[j] = e.instanceId end;"
    , "    out[i] = o.source.kind .. ':' .. o.source.id .. '>'"
    , "      .. o.destination.kind .. ':' .. o.destination.id"
    , "      .. '[' .. table.concat(ids, ',') .. ']';"
    , "  end;"
    , "  return table.concat(out, ';') end;"
    , "return true"
    ]

-- | Open the window's BASE level on one endpoint.
--
--   Concatenated rather than built with 'luaLines': that helper joins on
--   SPACES, which inside a quoted Lua string would make the endpoint
--   kind @' building '@ and silently refuse every open.
openEndpoint ∷ Text → Int → Text
openEndpoint kind eid =
    "return require('scripts.cargo_inventory_panel').openFor('" <> kind
        <> "', " <> tshow eid <> ", 300, 300)"

-- | Select exactly these uids.
selectStub ∷ [Int] → Text
selectStub uids = "unit.getSelected = function() return {"
    <> T.intercalate "," (map tshow uids) <> "} end; "

-- * Spec

spec ∷ Spec
spec = aroundAll withSharedFixture $
  describe "Transfer context menu (Mode B Store/Retrieve gestures, #1249)" $ do

    describe "Store — a unit-info row into the open window's endpoint" $ do

        it "offers nothing at all when no container window is open \
           \(requirement 1: the open window IS the target)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls "return _G.__labels(_G.__storeMenu('rope'))"
            r `shouldBe` "\"Equip\""

        it "a merged row offers Store 1 and Store all; a single-instance \
           \row offers Store 1 alone (requirement 3)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "building" 7)
            merged ← evalOk ls "return _G.__labels(_G.__storeMenu('ration'))"
            merged `shouldSatisfy` T.isInfixOf "Store 1"
            merged `shouldSatisfy` T.isInfixOf "Store all"
            single ← evalOk ls "return _G.__labels(_G.__storeMenu('rope'))"
            single `shouldSatisfy` T.isInfixOf "Store 1"
            single `shouldNotSatisfy` T.isInfixOf "Store all"

        it "Store 1 queues a durable order to the window's endpoint with \
           \NO adjacency — the whole promotion (requirements 1 and 5)" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "building" 7)
            fired ← evalOk ls
                "return _G.__fire(_G.__storeMenu('rope'), 'Store 1')"
            fired `shouldBe` "true"
            -- The carrier is at (10,10) and the hold at (40,40): thirty
            -- tiles, far outside the contract's Chebyshev-1 reach. The
            -- retired `unit.depositToCargo` path could not have run at
            -- all from here.
            o ← evalOk ls "return _G.__orders(1)"
            o `shouldBe` "\"unit:1>building:7[110]\""

        it "Store all queues EVERY instance id of the merged row, in \
           \order — never a count (requirement 3 / A2)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "building" 7)
            fired ← evalOk ls
                "return _G.__fire(_G.__storeMenu('ration'), 'Store all')"
            fired `shouldBe` "true"
            o ← evalOk ls "return _G.__orders(1)"
            o `shouldBe` "\"unit:1>building:7[101,102,103]\""

        it "targets a UNIT endpoint the same way it targets a building" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "unit" 2)
            fired ← evalOk ls
                "return _G.__fire(_G.__storeMenu('rope'), 'Store 1')"
            fired `shouldBe` "true"
            o ← evalOk ls "return _G.__orders(1)"
            o `shouldBe` "\"unit:1>unit:2[110]\""

        it "is omitted when the open endpoint IS the panel's own unit — \
           \the contract refuses a self-transfer, so the gesture must \
           \not queue a predictably invalid order" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "unit" 1)
            r ← evalOk ls "return _G.__labels(_G.__storeMenu('rope'))"
            r `shouldNotSatisfy` T.isInfixOf "Store"
            o ← evalOk ls "return _G.__orders(1)"
            o `shouldBe` "\"\""

        it "is omitted for an equipped item and for an accessory, exactly \
           \as the retired path excluded them (requirement 1)" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "building" 7)
            equipped ← evalOk ls
                "return _G.__labels(_G.__storeMenu('rope', function(r) \
                \r.equipped = true; r.equippedSlot = 'hand' end))"
            equipped `shouldNotSatisfy` T.isInfixOf "Store"
            accessory ← evalOk ls
                "return _G.__labels(_G.__storeMenu('rope', function(r) \
                \r.equipped = true; r.accessoryIndex = 1 end))"
            accessory `shouldNotSatisfy` T.isInfixOf "Store"

        it "is omitted when the ACTIVE level is an item container, and \
           \never falls back to its transfer-capable ancestor \
           \(requirement 4 / D-5)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "unit" 1)
            -- Descend into a container the carrier holds. The base level
            -- below it IS a transfer endpoint, which is exactly the
            -- ancestor a fallback would wrongly reach for.
            pushed ← evalOk ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "return cip.openLevel({ kind = 'unitItem', uid = 1,"
                , "  defName = 'kit', instanceId = 120, path = {},"
                , "  displayName = 'Kit' }, 300, 300, 1)" ]
            pushed `shouldBe` "true"
            depth ← evalOk ls
                "return require('scripts.cargo_inventory_panel').depth()"
            depth `shouldBe` "2"
            r ← evalOk ls "return _G.__labels(_G.__storeMenu('rope'))"
            r `shouldNotSatisfy` T.isInfixOf "Store"
            o ← evalOk ls "return _G.__orders(1)"
            o `shouldBe` "\"\""

        it "never offers the retired adjacent-cargo entry (requirement 5)" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "building" 7)
            r ← evalOk ls "return _G.__labels(_G.__storeMenu('ration'))"
            r `shouldNotSatisfy` T.isInfixOf "Store in "

    describe "Retrieve — a container-window row into the resolved unit" $ do

        it "queues a durable order from the window's endpoint to the \
           \resolved unit, with NO adjacency (requirement 2)" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (selectStub [2])
            _ ← evalOk ls (openEndpoint "building" 7)
            labels ← evalOk ls "return _G.__labels(_G.__windowMenu('crowbar'))"
            labels `shouldSatisfy` T.isInfixOf "Retrieve 1"
            fired ← evalOk ls
                "return _G.__fire(_G.__windowMenu('crowbar'), 'Retrieve 1')"
            fired `shouldBe` "true"
            -- The order belongs to the RETRIEVER (uid 2), which is both
            -- the executor and the destination — never the carrier whose
            -- panel supplied the Store rows in the sibling block.
            o ← evalOk ls "return _G.__orders(2)"
            o `shouldBe` "\"building:7>unit:2[210]\""
            carrier ← evalOk ls "return _G.__orders(1)"
            carrier `shouldBe` "\"\""

        it "Retrieve all queues every instance id of a merged window row" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (selectStub [2])
            _ ← evalOk ls (openEndpoint "building" 7)
            labels ← evalOk ls "return _G.__labels(_G.__windowMenu('steel_bar'))"
            labels `shouldSatisfy` T.isInfixOf "Retrieve 1"
            labels `shouldSatisfy` T.isInfixOf "Retrieve all"
            fired ← evalOk ls
                "return _G.__fire(_G.__windowMenu('steel_bar'), 'Retrieve all')"
            fired `shouldBe` "true"
            o ← evalOk ls "return _G.__orders(2)"
            o `shouldBe` "\"building:7>unit:2[201,202]\""

        it "resolves its source through the SHARED rule: the NEAREST \
           \eligible selected unit wins, and a non-commandable one is \
           \skipped however close it stands" $ \(env, ls) → do
            resetFixture env ls
            -- The wolf at (11,11) is nearer the hold than the acolyte at
            -- (12,12) but is wildlife, so filtering must precede ranking
            -- — the same property transfer_session.resolveSource's own
            -- gate has, reached here through the gesture.
            _ ← evalOk ls (selectStub [3, 2])
            _ ← evalOk ls (openEndpoint "building" 7)
            fired ← evalOk ls
                "return _G.__fire(_G.__windowMenu('crowbar'), 'Retrieve 1')"
            fired `shouldBe` "true"
            o ← evalOk ls "return _G.__orders(2)"
            o `shouldBe` "\"building:7>unit:2[210]\""

        it "is OMITTED when no eligible source resolves — never a \
           \disabled row, which is what the retired withdraw \
           \placeholder was (requirements 2 and 5)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (selectStub [])
            _ ← evalOk ls (openEndpoint "building" 7)
            -- No menu opens at all: a plain row with no gesture and no
            -- nested contents has nothing to show.
            r ← evalOk ls "return tostring(_G.__windowMenu('crowbar'))"
            r `shouldBe` "\"nil\""
            wildlifeOnly ← do
                _ ← evalOk ls (selectStub [3])
                evalOk ls "return tostring(_G.__windowMenu('crowbar'))"
            wildlifeOnly `shouldBe` "\"nil\""

        it "excludes the window's OWN unit endpoint from source \
           \resolution, so a unit window never retrieves from itself" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (selectStub [1])
            _ ← evalOk ls (openEndpoint "unit" 1)
            r ← evalOk ls "return tostring(_G.__windowMenu('rope'))"
            r `shouldBe` "\"nil\""
            o ← evalOk ls "return _G.__orders(1)"
            o `shouldBe` "\"\""

        it "never offers the retired withdraw entries (requirement 5)" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (selectStub [2])
            _ ← evalOk ls (openEndpoint "building" 7)
            r ← evalOk ls "return _G.__labels(_G.__windowMenu('crowbar'))"
            r `shouldNotSatisfy` T.isInfixOf "Withdraw"

    -- #2030: a Mode B order queued for a species that never registered
    -- `transfer_order` is never ticked at all, so it sits in the store
    -- for ever -- no walk, no commit, nothing to show the player. The
    -- debug overlay makes that reachable: FactionDebug is
    -- player-commandable, so a debug-spawned bear or squirrel passed
    -- every gate the gestures had.
    --
    -- Every case here depends on the REAL registry being loaded by
    -- 'sceneLua'; the first one asserts that precondition rather than
    -- assuming it, because an empty registry answers permissively by
    -- design and would make the whole block vacuous.
    describe "Executor capability — the order's own AI action (#2030)" $ do

        it "loads the shipped registry, so the omissions below are \
           \refusals rather than an absent inventory (requirement 5's \
           \permissive path is NOT what is being exercised)" $
           \(env, ls) → do
            resetFixture env ls
            reg ← evalOk ls $ luaLines
                [ "local a = require('scripts.unit_ai_actions');"
                , "local act = a.TRANSFER_ORDER_ACTION;"
                , "return { registered = a.registered(),"
                , "         acolyte = a.has('acolyte', act),"
                , "         mule = a.has('technomule', act),"
                , "         bear = a.has('bear_brown', act),"
                , "         squirrel = a.has('red_squirrel', act) }" ]
            reg `shouldSatisfy` T.isInfixOf "\"registered\":true"
            reg `shouldSatisfy` T.isInfixOf "\"acolyte\":true"
            reg `shouldSatisfy` T.isInfixOf "\"mule\":true"
            reg `shouldSatisfy` T.isInfixOf "\"bear\":false"
            reg `shouldSatisfy` T.isInfixOf "\"squirrel\":false"

        -- Review round 1: the name had been defined independently in two
        -- production modules, with this case merely asserting today's
        -- two literals matched -- which is a drift DETECTOR, not a
        -- single source. scripts/unit_ai_actions.lua now owns the one
        -- definition and every asker reads it, so what is checked here
        -- is that the registration really flows from that owner.
        it "takes the action's name from ONE owner: the registry defines \
           \it, the dispatch loop registers under it, and every gate \
           \asks by it (requirement 1)" $ \(env, ls) → do
            resetFixture env ls
            owned ← evalOk ls $ luaLines
                [ "local a = require('scripts.unit_ai_actions');"
                , "local t = require('scripts.unit_ai_transfer');"
                -- The registered action's own name IS the owner's value,
                -- so a rename of the owner renames the registration --
                -- which is what "one definition" has to mean.
                , "return { owner = a.TRANSFER_ORDER_ACTION,"
                , "         registeredUnder = t.action.name,"
                , "         sameString = t.action.name =="
                , "                      a.TRANSFER_ORDER_ACTION,"
                -- ...and the species inventory really is keyed by it.
                , "         inInventory = a.byDef['acolyte']"
                , "                         [a.TRANSFER_ORDER_ACTION] == true }" ]
            owned `shouldSatisfy` T.isInfixOf "\"owner\":\"transfer_order\""
            owned `shouldSatisfy`
                T.isInfixOf "\"registeredUnder\":\"transfer_order\""
            owned `shouldSatisfy` T.isInfixOf "\"sameString\":true"
            owned `shouldSatisfy` T.isInfixOf "\"inInventory\":true"
            -- Neither consumer re-declares it: the gesture module and
            -- the AI module are consumers, so the owner is the only
            -- place the string is written.
            noRedecl ← evalOk ls $ luaLines
                [ "local g = require('scripts.transfer_gestures');"
                , "local t = require('scripts.unit_ai_transfer');"
                , "return { gesture = tostring(g.TRANSFER_ORDER_ACTION),"
                , "         transfer = tostring(t.TRANSFER_ORDER_ACTION) }" ]
            noRedecl `shouldSatisfy` T.isInfixOf "\"gesture\":\"nil\""
            noRedecl `shouldSatisfy` T.isInfixOf "\"transfer\":\"nil\""

        it "omits Store for a commandable species that cannot run the \
           \order — a debug bear and a debug squirrel queue nothing, \
           \while the acolyte in the same scene still gets it \
           \(requirements 2 and 6)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "building" 7)
            bear ← evalOk ls "return _G.__labels(_G.__storeMenuAs(4, 'rope'))"
            bear `shouldNotSatisfy` T.isInfixOf "Store"
            squirrel ← evalOk ls
                "return _G.__labels(_G.__storeMenuAs(5, 'rope'))"
            squirrel `shouldNotSatisfy` T.isInfixOf "Store"
            -- The gate refuses a SPECIES, not the scene: the acolyte's
            -- own row is still offered, so an omission that swallowed
            -- everything would fail here.
            acolyte ← evalOk ls
                "return _G.__labels(_G.__storeMenuAs(1, 'rope'))"
            acolyte `shouldSatisfy` T.isInfixOf "Store 1"
            b ← evalOk ls "return _G.__orders(4)"
            b `shouldBe` "\"\""
            sq ← evalOk ls "return _G.__orders(5)"
            sq `shouldBe` "\"\""

        it "Retrieve filters BEFORE ranking: two nearer unsupported \
           \units are skipped for the capable acolyte, and again for a \
           \technomule that is farther than either (requirements 3 \
           \and 6)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (openEndpoint "building" 7)
            -- Unfiltered, the squirrel at (14,14) really is what the
            -- shared rule resolves — so the assertions below fail the
            -- moment the capability argument stops being passed.
            unfiltered ← evalOk ls $ luaLines
                [ "local s = require('scripts.transfer_session');"
                , "local ep = unit.transferEndpointInfo("
                , "  {kind = 'building', id = 7});"
                , "return { withMule = s.resolveSource({4, 5, 6}, nil, ep),"
                , "         withAcolyte = s.resolveSource({4, 5, 2}, nil,"
                , "                                       ep) }" ]
            unfiltered `shouldSatisfy` T.isInfixOf "\"withMule\":5"
            unfiltered `shouldSatisfy` T.isInfixOf "\"withAcolyte\":5"
            _ ← evalOk ls (selectStub [4, 5, 2])
            fired ← evalOk ls
                "return _G.__fire(_G.__windowMenu('crowbar'), 'Retrieve 1')"
            fired `shouldBe` "true"
            o ← evalOk ls "return _G.__orders(2)"
            o `shouldBe` "\"building:7>unit:2[210]\""
            skipped ← evalOk ls
                "return _G.__orders(4) .. '/' .. _G.__orders(5)"
            skipped `shouldBe` "\"/\""
            -- ...and a technomule wins the same way, so the capability
            -- is the registry's answer rather than "is it an acolyte".
            resetFixture env ls
            _ ← evalOk ls (selectStub [4, 5, 6])
            _ ← evalOk ls (openEndpoint "building" 7)
            firedMule ← evalOk ls
                "return _G.__fire(_G.__windowMenu('steel_bar'), 'Retrieve all')"
            firedMule `shouldBe` "true"
            mule ← evalOk ls "return _G.__orders(6)"
            mule `shouldBe` "\"building:7>unit:6[201,202]\""

        it "omits Retrieve entirely when every selected unit is an \
           \unsupported species — no entries, never a disabled row \
           \(requirements 3 and 6)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (selectStub [4, 5])
            _ ← evalOk ls (openEndpoint "building" 7)
            r ← evalOk ls "return tostring(_G.__windowMenu('crowbar'))"
            r `shouldBe` "\"nil\""
            o ← evalOk ls
                "return _G.__orders(4) .. '/' .. _G.__orders(5)"
            o `shouldBe` "\"/\""

        it "the reusable command boundary refuses an actionless executor \
           \and stores NOTHING — the check precedes \
           \unit.createTransferOrder, which is the verb that creates \
           \and stores (requirement 4)" $ \(env, ls) → do
            resetFixture env ls
            -- Submitted DIRECTLY, bypassing both gestures: `M.create`'s
            -- Mode A precedent, read for Mode B — the one boundary every
            -- order is made through is re-checked rather than trusted.
            refused ← evalOk ls $ luaLines
                [ "local ai = require('scripts.unit_ai');"
                , "return tostring(ai.commandTransferOrder(4, {"
                , "  source = {kind = 'building', id = 7},"
                , "  destination = {kind = 'unit', id = 4},"
                , "  items = {{instanceId = 210, defName = 'crowbar'}} }))" ]
            refused `shouldBe` "\"false\""
            stored ← evalOk ls "return _G.__orders(4)"
            stored `shouldBe` "\"\""
            -- The IDENTICAL request for a capable carrier is accepted,
            -- so the refusal names the species and not the request.
            accepted ← evalOk ls $ luaLines
                [ "local ai = require('scripts.unit_ai');"
                , "return tostring(ai.commandTransferOrder(2, {"
                , "  source = {kind = 'building', id = 7},"
                , "  destination = {kind = 'unit', id = 2},"
                , "  items = {{instanceId = 210, defName = 'crowbar'}} })"
                , "  ~= false)" ]
            accepted `shouldBe` "\"true\""
            ok ← evalOk ls "return _G.__orders(2)"
            ok `shouldBe` "\"building:7>unit:2[210]\""

        it "invents no refusal where no AI is loaded: an EMPTY registry \
           \still offers both gestures and still queues the order \
           \(requirement 5)" $ \(env, ls) → do
            resetFixture env ls
            -- What a UI-only fixture or a menu screen looks like: the
            -- module is present, nothing has registered. Answering
            -- "false" from an absent inventory would break every such
            -- process, which is why unit_ai_actions answers "true".
            _ ← evalOk ls
                "require('scripts.unit_ai_actions').byDef = {}; return true"
            _ ← evalOk ls (selectStub [4])
            _ ← evalOk ls (openEndpoint "building" 7)
            retrieve ← evalOk ls
                "return _G.__labels(_G.__windowMenu('crowbar'))"
            retrieve `shouldSatisfy` T.isInfixOf "Retrieve 1"
            fired ← evalOk ls
                "return _G.__fire(_G.__storeMenuAs(4, 'rope'), 'Store 1')"
            fired `shouldBe` "true"
            -- ...and the command boundary is permissive too, so the
            -- order really is stored rather than refused one layer down.
            o ← evalOk ls "return _G.__orders(4)"
            o `shouldBe` "\"unit:4>building:7[401]\""

    describe "Gesture-wide invariants" $ do

        it "no gesture moves the camera (D-4 / requirement 7)" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (selectStub [2])
            _ ← evalOk ls (openEndpoint "building" 7)
            before ← evalOk ls cameraProbe
            _ ← evalOk ls "return _G.__fire(_G.__storeMenu('ration'), 'Store all')"
            _ ← evalOk ls
                "return _G.__fire(_G.__windowMenu('steel_bar'), 'Retrieve all')"
            after ← evalOk ls cameraProbe
            after `shouldBe` before

        it "no gesture calls a lax AI verb: the player paths queue an \
           \order and the D-7 verbs stay the AI ladders' own" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (selectStub [2])
            _ ← evalOk ls (openEndpoint "building" 7)
            _ ← evalOk ls $ luaLines
                [ "_G.__lax = 0;"
                , "for _, v in ipairs({'depositToCargo', 'withdrawFromCargo',"
                , "                     'transferItemToUnit',"
                , "                     'transferItemToBuilding'}) do"
                , "  unit[v] = function() _G.__lax = _G.__lax + 1;"
                , "                       return false end end;" ]
            _ ← evalOk ls "return _G.__fire(_G.__storeMenu('ration'), 'Store all')"
            _ ← evalOk ls
                "return _G.__fire(_G.__windowMenu('steel_bar'), 'Retrieve all')"
            lax ← evalOk ls "return _G.__lax"
            lax `shouldBe` "0"
            -- ...and both gestures really did run, so the zero above is
            -- an absence of lax calls rather than an absence of work.
            store ← evalOk ls "return _G.__orders(1)"
            store `shouldBe` "\"unit:1>building:7[101,102,103]\""
            retrieve ← evalOk ls "return _G.__orders(2)"
            retrieve `shouldBe` "\"building:7>unit:2[201,202]\""

-- | Camera position, zoom and z-slice as one comparable string.
cameraProbe ∷ Text
cameraProbe = luaLines
    [ "local px, py = camera.getPosition();"
    , "return string.format('%.4f,%.4f,%.4f,%d', px or 0, py or 0,"
    , "  camera.getZoom(), camera.getZSlice())" ]
