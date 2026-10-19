{-# LANGUAGE OverloadedStrings #-}
-- | The player's @Open@ order on a ground container (#2528, epic #1231
--   PLC-18): a unit walks to a crate where it lies, the open resolves at
--   the result boundary, the contents become player knowledge, and the
--   crate's @portableItem@ window level opens. Nothing moves the crate.
--
--   Driven end to end against a LIVE headless engine with the real
--   @scripts/hud.lua@ booted (synthetic font and texture handles, as
--   "Test.Headless.Item.PortableWindow" boots it): real ground items, a
--   real placed location whose container slot binds a PENDING shell, a
--   real loot-profile registry, the real realization verb, the real
--   portable-knowledge map drained by a real world tick, the real
--   ground-item menu, the real shared assignment rule, the real window
--   manager and the PRODUCTION @scripts/unit_ai_open.lua@. Replaced:
--   @scripts.movement_speed@ (one walking pace, as every order test
--   replaces it), the @scripts.unit_ai@ singleton the order module
--   attaches to, @unit.getSelected@ (an input) and @item.hitTestAt@
--   (a pixel hit test needs a rendered frame).
--
--   What this layer cannot prove is a real disk save, quit and reload
--   with the order still pending: the headless harness cannot run
--   @engine.saveWorld@ end to end, so that half is
--   @tools/location_content_probe.py@'s Open scenario.
--
--   Filed under @Nested item contents@ (the window it opens) and named
--   "Open order" for its own gate:
--   @cabal test synarchy-test-headless --test-options='--match "Open order"'@.
module Test.Headless.Item.OpenOrder (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.PlayerEvent (CategoryCfg(..), clearEventStoreRows)
import Engine.PlayerEvent.Emit (PlayerEvent(..), StoredEvent(..), readEventLog)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Ground (GroundItem(..), GroundItems(..))
import Item.Types (ItemInstance(..), ItemManager(..))
import Location.Instance
    ( LocationContainerSlot(..), LocationInstance(..), LocationInstanceId(..)
    , LocationInstances(..), emptyLocationInstances, instancesToList
    , newLocationInstance )
import LootProfile.Types
    ( LootProfileDef(..), LootProfileEntry(..), LootProfileRegistry
    , emptyLootProfileRegistry, registerLootProfile )
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Item.PortableKnowledge.Fixture (loose, mkUnitOfFaction, testItems)
import Test.Headless.Location.ContainerShells
    ( containerContent, containerItemDefs, mkDef, paramsWith, spawnContainer )
import UI.Types (emptyUIPageManager)
import Unit.Command.Types (UnitCommand(..))
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..), unWorldPageId)
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )
import World.Thread (worldTickWith)

-- * Fixture identities

-- | @scripts/hud.lua@'s own page name; the ACTIVE page throughout,
--   except where a case moves it.
hudPage ∷ WorldPageId
hudPage = WorldPageId "main_world"

-- | A second live page, for the cases about page identity.
otherPage ∷ WorldPageId
otherPage = WorldPageId "open_other"

shellAt ∷ (Float, Float)
shellAt = (8, 8)

-- | One tile off the crate: inside the 1.2-tile floating arrival range
--   without standing on it.
arrivedAt ∷ (Float, Float)
arrivedAt = (8, 9)

startAt ∷ (Float, Float)
startAt = (0, 0)

-- | Where the plain (non-container) bar lies on the hud page.
barGid ∷ Int
barGid = 40

-- * Content

salvageProfile ∷ LootProfileDef
salvageProfile = LootProfileDef
    { lpdId = "fixture_salvage", lpdMultiplierMin = 2, lpdMultiplierMax = 2
    , lpdEntries = [ LootProfileEntry { lpeItem = "rations", lpeChance = 1.0
                                      , lpeQuantityFactor = 1 } ] }

profiles ∷ LootProfileRegistry
profiles = registerLootProfile salvageProfile emptyLootProfileRegistry

-- | One crate slot on location instance 1 naming @profile@ —
--   @"fixture_gone"@ is registered nowhere, the D-23 refusal.
crateTable ∷ Text → LocationInstances
crateTable profile =
    let liid = LocationInstanceId 1
        def  = mkDef "open2528_site" [containerContent "fixture_crate" profile 1]
        inst = either (error ∘ show) id
            (newLocationInstance Nothing liid (ChunkCoord 0 0) def)
    in emptyLocationInstances { lisNextId = 2, lisById = HM.singleton liid inst }

-- | Both registries the scene draws from: the portable-window fixture's
--   items and the container-shell fixture's crate, straw and rations.
allItems ∷ ItemManager
allItems = ItemManager (HM.union (imDefs containerItemDefs) (imDefs testItems))

data Carrier = Carrier
    { cUid ∷ Int, cPage ∷ WorldPageId, cAt ∷ (Float, Float)
    , cFaction ∷ Faction, cDef ∷ Text }

carrier ∷ Int → WorldPageId → (Float, Float) → Carrier
carrier u p at = Carrier u p at FactionPlayer "acolyte"

asUnit ∷ Carrier → UnitInstance
asUnit c = (mkUnitOfFaction (cFaction c) (cPage c) [] HM.empty [])
    { uiDefName = cDef c, uiName = "Unit" <> tshow (cUid c)
    , uiGridX = fst (cAt c), uiGridY = snd (cAt c)
    , uiStats = HM.singleton "carrying_capacity" 0 }

logOnlyCategory ∷ Text → CategoryCfg
logOnlyCategory cid = CategoryCfg
    { ccId = cid, ccDisplayName = cid, ccDescription = ""
    , ccTextColor = (1, 1, 1, 1)
    , ccLog = True, ccPopup = False, ccPause = False
    , ccPopupCoalesceWindow = 0, ccLogCoalesceWindow = 0 }

data Bindings = Bindings
    { bnEnv ∷ EngineEnv
    , bnLua ∷ LuaBackendState
    }

withBindings ∷ (Bindings → IO α) → IO α
withBindings act = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    act (Bindings env ls)

newPage ∷ Text → IO WorldState
newPage profile = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just (paramsWith (crateTable profile)))
    pure ws

-- | Install the pages (the FIRST active), spawn each page's ONE bound
--   pending shell through the real verb (so each page's shell is ground
--   id 0), lay the plain bar beside the hud page's crate, install the
--   carriers, and boot a fresh Lua module graph with the real HUD.
scene ∷ Bindings → [(WorldPageId, WorldState)] → [Carrier] → IO ()
scene b pages cs = do
    let env = bnEnv b
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (itemManagerRef env) allItems
    writeIORef (lootProfileRegistryRef env) profiles
    writeIORef (gameTimeRef env) 100
    writeIORef (notificationCfgRef env) $ HM.fromList
        [ (c, logOnlyCategory c) | c ← ["unit_event", "unit_warning"] ]
    atomically $ modifyTVar' (eventStoreRef env) clearEventStoreRows
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = pages, wmVisible = take 1 (map fst pages) }
    forM_ pages $ \(pid, ws) → do
        spawnContainer env pid 1 1 shellAt `shouldReturn` True
        when (pid ≡ hudPage) $ atomicModifyIORef' (wsGroundItemsRef ws) $
            \gis → ( gis { gisItems = HM.insert barGid (GroundItem loose 3 3)
                                          (gisItems gis)
                         , gisNextId = max (gisNextId gis) (barGid + 1) }, () )
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.fromList
            [ (UnitId (fromIntegral (cUid c)), asUnit c) | c ← cs ]
        , umNextId = 100 }
    _ ← Q.flushQueue (unitQueue env)
    _ ← luaOk b "for k, _ in pairs(package.loaded) do \
                 \package.loaded[k] = nil end; return true"
    _ ← luaOk b sceneLua
    pure ()

-- | The Lua half: the order module under the stub singleton, the real
--   HUD, the action registry with @open_ground@ for acolytes only, and
--   capture helpers for the menu and the window stack.
sceneLua ∷ Text
sceneLua = T.concat
    [ "package.loaded['scripts.unit_ai'] = {}; "
    , "package.loaded['scripts.movement_speed'] = "
    , "  { comfort = function() return 1.0 end, ordered = function() return 1.15 end, "
    , "    meander = function() return 0.5 end, sprint = function() return 2.0 end }; "
    , "_G.OPEN = require('scripts.unit_ai_open'); "
    , "_G.CORE = require('scripts.unit_ai_core'); "
    , "_G.UNITAI = package.loaded['scripts.unit_ai']; "
    , "_G.PARAMS = { pickup_utility = 7.5, open_timeout = 30, "
    , "              open_arrival_tiles = 1.2, pickup_arrival_tiles = 1.2 }; "
    , "require('scripts.unit_ai_actions').record('acolyte', "
    , "  { { name = 'pickup_ground' }, { name = 'open_ground' } }); "
    , "require('scripts.unit_ai_actions').record('bear', { { name = 'wander' } }); "
    , "_G.__hit = nil; _G.__sel = {}; _G.__calls = {}; "
    , "item.hitTestAt = function() if _G.__hit then return _G.__hit[1], _G.__hit[2] end end; "
    , "unit.getSelected = function() return _G.__sel end; "
    , "local hud = require('scripts.hud'); "
    , "hud.init(1, 2, 1920, 1080); hud.createUI(); "
    , "_G.__cip = require('scripts.cargo_inventory_panel'); "
    -- Record the ingress ORDER: the player-only session teardown, then
    -- the dispatch. Both wrappers forward to the real functions.
    , "local ts = require('scripts.transfer_session'); "
    , "local origNote = ts.notePlayerOrder; "
    , "ts.notePlayerOrder = function(uid) "
    , "  table.insert(_G.__calls, 'note:' .. uid); return origNote(uid) end; "
    , "local origOpen = UNITAI.commandOpen; "
    , "UNITAI.commandOpen = function(uid, gid, page) "
    , "  table.insert(_G.__calls, 'open:' .. uid); return origOpen(uid, gid, page) end; "
    , "_G.__menu = function(gid, page, sel) "
    , "  local cm = require('scripts.ui.context_menu'); "
    , "  _G.__hit = { gid, page }; _G.__sel = sel or {}; "
    , "  local captured, orig = nil, cm.show; "
    , "  cm.show = function(items) captured = items end; "
    , "  require('scripts.init_context_menu_item').tryItemMenu(100, 100); "
    , "  cm.show = orig; _G.__hit = nil; return captured or {} end; "
    , "_G.__labels = function(gid, page, sel) "
    , "  local out = {}; for i, e in ipairs(_G.__menu(gid, page, sel)) do "
    , "    out[i] = e.label end; return table.concat(out, '|') end; "
    , "_G.__fire = function(items, label) "
    , "  for _, e in ipairs(items) do if e.label == label then "
    , "    e.callback(); return true end end; return false end; "
    , "_G.__stack = function() "
    , "  local d = _G.__cip.dump(); local out = {}; "
    , "  for i, l in ipairs(d.levels) do "
    , "    out[i] = tostring(l.kind) .. '@' .. tostring(l.instanceId) end; "
    , "  return table.concat(out, '>') end; "
    , "_G.__rows = function() "
    , "  local il = require('scripts.ui.item_list'); "
    , "  local level = _G.__cip.getLevel(1); if not level then return 'nil' end; "
    , "  local parts = {}; "
    , "  for _, r in ipairs(il.getRows(level.listId)) do "
    , "    local g = r.item or {}; "
    , "    parts[#parts+1] = tostring(g.defName) .. ':' .. tostring(g.count or 0) end; "
    , "  table.sort(parts); return table.concat(parts, ',') end; "
    , "_G.__exec = function(uid) "
    , "  local s = CORE.ensureState(uid); s.currentAction = 'open_ground'; "
    , "  OPEN.action.execute(uid, s, PARAMS); return true end; "
    , "_G.__util = function(uid) "
    , "  local s = CORE.ensureState(uid); s.currentAction = 'open_ground'; "
    , "  return OPEN.action.utility(uid, s, PARAMS) end; "
    , "return true" ]

-- * Readers

shellOf ∷ WorldState → IO (Maybe (Int, Word64, Int, (Float, Float)))
shellOf ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    pure $ case [ (gid, iiInstanceId (giInst gi), length (iiContents (giInst gi))
                  , (giX gi, giY gi))
                | (gid, gi) ← HM.toList (gisItems gis)
                , iiDefName (giInst gi) ≡ "fixture_crate" ] of
        [one] → Just one
        _     → Nothing

realizedOn ∷ WorldState → IO Bool
realizedOn ws = do
    mp ← readIORef (wsGenParamsRef ws)
    pure $ or [ lcsRealized s | p ← maybeToList mp
                              , inst ← instancesToList (wgpLocationInstances p)
                              , s ← liContainers inst ]

eventsOf ∷ Bindings → Text → IO [Text]
eventsOf b cat = do
    evs ← map seEvent ⊚ readEventLog (bnEnv b)
    pure [ peText e | e ← evs, peCategory e ≡ cat ]

moves ∷ Bindings → IO [(Word32, Float, Float)]
moves b = do
    cmds ← Q.flushQueue (unitQueue (bnEnv b))
    pure [ (u, x, y) | UnitMoveTo (UnitId u) x y _ _ ← cmds ]

drainWorld ∷ Bindings → IO ()
drainWorld b = do
    lastRef ← newIORef 0
    _ ← worldTickWith (pure 0) (bnEnv b) lastRef
    pure ()

placeUnit ∷ Bindings → Int → (Float, Float) → IO ()
placeUnit b u (x, y) = atomicModifyIORef' (unitManagerRef (bnEnv b)) $ \um →
    ( um { umInstances = HM.adjust (\i → i { uiGridX = x, uiGridY = y })
                             (UnitId (fromIntegral u)) (umInstances um) }, () )

atTime ∷ Bindings → Double → IO ()
atTime b = writeIORef (gameTimeRef (bnEnv b))

-- | (state, revealedAt, weighedAt) of the player's record of a crate,
--   after a world tick.
knowledge ∷ Bindings → Word64 → IO Text
knowledge b iid = do
    drainWorld b
    luaStr b $ T.concat
        [ "(function() local k = item.getContainerKnowledge(", tshow iid, "); "
        , "return tostring(k.state) .. ',' .. tostring(k.revealedAt) .. ',' "
        , ".. tostring(k.weighedAt) end)()" ]

orderOf ∷ Bindings → Int → IO Text
orderOf b u = luaStr b
    ("(function() local s = CORE.aiState[" <> tshow u <> "]; "
     <> "return tostring(s and s.openOrder and s.openOrder.gid) end)()")

-- * Lua plumbing

luaOk ∷ Bindings → Text → IO Text
luaOk b src = do
    r ← executeDebugLua (lbsLuaState (bnLua b)) src
    r `shouldNotSatisfy` (\t → "error:" `T.isPrefixOf` t
                             ∨ "syntax error:" `T.isPrefixOf` t)
    pure (T.strip r)

luaStr ∷ Bindings → Text → IO Text
luaStr b expr = T.filter (≢ '"') <$> luaOk b ("return " <> expr)

pageT ∷ WorldPageId → Text
pageT p = "'" <> unWorldPageId p <> "'"

-- | Give the order through the REAL ground menu on the hud page's crate
--   (ground id 0), with @sel@ selected; answers whether "Open" fired.
openViaMenu ∷ Bindings → [Int] → IO Bool
openViaMenu b sel = (≡ "true") <$> luaStr b
    ("_G.__fire(_G.__menu(0, " <> pageT hudPage <> ", {"
     <> T.intercalate "," (map tshow sel) <> "}), 'Open')")

-- * Spec

spec ∷ Spec
spec = around withBindings $
  describe "Nested item contents" $ describe "the Open order (#2528)" $ do

    describe "the ground menu offers Open through the shared assignment rule" $ do

        it "offers Open on an item-container only, assigns the nearest \
           \CAPABLE unit, falls back to the page with nothing selected, and \
           \omits it for an entirely ineligible selection" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(hudPage, ws)]
                [ (carrier 1 hudPage shellAt) { cDef = "bear" }
                , (carrier 2 hudPage shellAt) { cFaction = FactionWildlife }
                , carrier 3 hudPage (12, 8) ]
            luaStr b ("_G.__labels(0, " <> pageT hudPage <> ", {})")
                >>= (`shouldSatisfy` T.isInfixOf "Open")
            -- The plain bar is no container: no Open, whatever the units.
            luaStr b ("_G.__labels(" <> tshow barGid <> ", " <> pageT hudPage <> ", {})")
                >>= (`shouldNotSatisfy` T.isInfixOf "Open")
            -- An ineligible selection is the candidate set: omitted.
            luaStr b ("_G.__labels(0, " <> pageT hudPage <> ", {1, 2})")
                >>= (`shouldNotSatisfy` T.isInfixOf "Open")
            -- Empty selection: the bear and the wildlife unit stand ON
            -- the crate, yet the order goes to unit 3.
            openViaMenu b [] `shouldReturn` True
            orderOf b 3 `shouldReturn` "0"
            orderOf b 1 `shouldReturn` "nil"

        it "ends the assigned unit's Mode A session BEFORE dispatching, \
           \while a direct commandOpen does not touch the session" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(hudPage, ws)] [carrier 3 hudPage (12, 8)]
            openViaMenu b [3] `shouldReturn` True
            luaStr b "table.concat(_G.__calls, ',')" `shouldReturn` "note:3,open:3"
            _ ← luaOk b "_G.__calls = {}; return UNITAI.commandOpen(3, 0)"
            luaStr b "table.concat(_G.__calls, ',')" `shouldReturn` "open:3"

        it "a STALE menu target (the crate taken before the click) stores no \
           \order" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(hudPage, ws)] [carrier 3 hudPage (12, 8)]
            _ ← luaOk b ("_G.__stale = _G.__menu(0, " <> pageT hudPage <> ", {3}); return true")
            atomicModifyIORef' (wsGroundItemsRef ws) $ \gis →
                (gis { gisItems = HM.delete 0 (gisItems gis) }, ())
            luaStr b "_G.__fire(_G.__stale, 'Open')" `shouldReturn` "true"
            orderOf b 3 `shouldReturn` "nil"

    describe "commandOpen's public boundary" $ do

        it "accepts a container with NO weight gate (the opener can carry \
           \nothing), clears a position hold, and resets the schedule" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(hudPage, ws)] [carrier 1 hudPage startAt]
            r ← luaStr b $ T.concat
                [ "(function() local s = CORE.ensureState(1); "
                , "s.holdAnchor = { x = 1, y = 1 }; s.nextActionAt = 99; "
                , "local ok = UNITAI.commandOpen(1, 0); "
                , "return tostring(ok) .. ',' .. tostring(s.holdAnchor) .. ',' "
                , ".. tostring(s.nextActionAt) .. ',' .. tostring(s.openOrder.gid) end)()" ]
            r `shouldBe` "true,nil,0,0"

        it "refuses a non-container, a missing id, a wrong page and an \
           \unresolvable owning page, changing nothing" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(hudPage, ws)] [carrier 1 hudPage startAt]
            let probe call = luaStr b $ T.concat
                    [ "(function() local s = CORE.ensureState(1); "
                    , "s.openOrder = { gid = 77, issuedAt = 5 }; "
                    , "s.holdAnchor = { x = 1, y = 1 }; s.nextActionAt = 99; "
                    , "local ok = ", call, "; "
                    , "return tostring(ok) .. ',' .. tostring(s.openOrder.gid) .. ',' "
                    , ".. tostring(s.holdAnchor ~= nil) .. ',' .. tostring(s.nextActionAt) end)()" ]
            probe ("UNITAI.commandOpen(1, " <> tshow barGid <> ")")
                `shouldReturn` "false,77,true,99"
            probe "UNITAI.commandOpen(1, 12345)" `shouldReturn` "false,77,true,99"
            probe ("UNITAI.commandOpen(1, 0, " <> pageT otherPage <> ")")
                `shouldReturn` "false,77,true,99"
            atomicModifyIORef' (worldManagerRef (bnEnv b)) $ \wm →
                (wm { wmWorlds = [] }, ())
            probe "UNITAI.commandOpen(1, 0)" `shouldReturn` "false,77,true,99"
            eventsOf b "unit_warning" `shouldReturn` []

    describe "an accepted order" $ do

        it "walks, arrives within the floating range, realizes the pending \
           \shell ONCE, records contents and weight at the observation time, \
           \emits the event, and opens the portable level -- which shows the \
           \committed knowledge once it lands; the crate never moves" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(hudPage, ws)] [carrier 1 hudPage startAt]
            Just (gid, iid, before, at) ← shellOf ws
            luaStr b ("tostring(UNITAI.commandOpen(1, " <> tshow gid <> "))")
                `shouldReturn` "true"
            _ ← luaOk b "return _G.__exec(1)"
            moves b `shouldReturn` [(1, fst shellAt, snd shellAt)]
            realizedOn ws `shouldReturn` False
            -- 1.5 tiles off: outside the 1.2 range, so still walking.
            placeUnit b 1 (8, 9.5)
            _ ← luaOk b "return _G.__exec(1)"
            length <$> moves b `shouldReturn` 1
            placeUnit b 1 arrivedAt
            atTime b 250
            _ ← luaOk b "return _G.__exec(1)"
            realizedOn ws `shouldReturn` True
            Just (gid', iid', after, at') ← shellOf ws
            (gid', iid', at') `shouldBe` (gid, iid, at)
            after `shouldBe` before + 2
            orderOf b 1 `shouldReturn` "nil"
            eventsOf b "unit_event" `shouldReturn` ["Unit1 opened fixture_crate"]
            luaStr b "_G.__stack()" `shouldReturn` ("portableItem@" <> tshow iid)
            knowledge b iid `shouldReturn` "known,250.0,250.0"
            _ ← luaOk b "_G.__cip.update(0); return true"
            luaStr b "_G.__rows()" >>= (`shouldSatisfy` T.isInfixOf "rations:2")

        it "a SECOND Open on the same crate rerolls nothing, refreshes both \
           \stamps, and reopens the level from knowledge" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(hudPage, ws)] [carrier 1 hudPage arrivedAt]
            atTime b 250
            _ ← luaOk b "UNITAI.commandOpen(1, 0); return _G.__exec(1)"
            Just (_, iid, first, _) ← shellOf ws
            gis1 ← readIORef (wsGroundItemsRef ws)
            _ ← luaOk b "require('scripts.item_contents_panel').closeIfOpen(); return true"
            atTime b 400
            _ ← luaOk b "UNITAI.commandOpen(1, 0); return _G.__exec(1)"
            Just (_, _, second, _) ← shellOf ws
            second `shouldBe` first
            gis2 ← readIORef (wsGroundItemsRef ws)
            (iiContents ∘ giInst ⊚ HM.lookup 0 (gisItems gis2))
                `shouldBe` (iiContents ∘ giInst ⊚ HM.lookup 0 (gisItems gis1))
            knowledge b iid `shouldReturn` "known,400.0,400.0"
            luaStr b "_G.__stack()" `shouldReturn` ("portableItem@" <> tshow iid)
            length <$> eventsOf b "unit_event" `shouldReturn` 2

        it "an unreachable order stalls out with the warning naming the unit \
           \and the container, and is not reassigned" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(hudPage, ws)] [carrier 1 hudPage startAt, carrier 2 hudPage startAt]
            _ ← luaOk b "return UNITAI.commandOpen(1, 0)"
            forM_ [101 .. 140 ∷ Int] $ \t → do
                atTime b (fromIntegral t)
                void $ luaOk b "_G.__util(1); return true"
            orderOf b 1 `shouldReturn` "nil"
            orderOf b 2 `shouldReturn` "nil"
            ws1 ← eventsOf b "unit_warning"
            ws1 `shouldSatisfy` (\w → length w ≡ 1)
            mconcat ws1 `shouldSatisfy` T.isInfixOf "Couldn't reach container to open"
            mconcat ws1 `shouldSatisfy` T.isInfixOf "Unit1"
            mconcat ws1 `shouldSatisfy` T.isInfixOf "fixture_crate"
            realizedOn ws `shouldReturn` False

        it "a D-23 realization refusal reports, observes nothing, opens no \
           \window and retires the order" $ \b → do
            ws ← newPage "fixture_gone"
            scene b [(hudPage, ws)] [carrier 1 hudPage arrivedAt]
            Just (gid, iid, before, at) ← shellOf ws
            _ ← luaOk b "UNITAI.commandOpen(1, 0); return _G.__exec(1)"
            shellOf ws `shouldReturn` Just (gid, iid, before, at)
            realizedOn ws `shouldReturn` False
            orderOf b 1 `shouldReturn` "nil"
            ws1 ← eventsOf b "unit_warning"
            ws1 `shouldSatisfy` (\w → length w ≡ 1)
            mconcat ws1 `shouldSatisfy` T.isInfixOf "Unit1"
            mconcat ws1 `shouldSatisfy` T.isInfixOf "fixture_crate"
            eventsOf b "unit_event" `shouldReturn` []
            knowledge b iid `shouldReturn` "unknown,nil,nil"
            luaStr b "_G.__stack()" `shouldReturn` ""

    describe "page identity" $ do

        it "opens only the CARRIER's crate when the active page holds a \
           \same-numbered pending shell, across an active-page switch" $ \b → do
            wsHud ← newPage "fixture_salvage"
            wsOther ← newPage "fixture_salvage"
            scene b [(otherPage, wsOther), (hudPage, wsHud)]
                [carrier 1 otherPage arrivedAt]
            Just (gidO, iidO, beforeO, _) ← shellOf wsOther
            Just (gidH, iidH, _, _) ← shellOf wsHud
            gidO `shouldBe` gidH
            luaStr b ("tostring(UNITAI.commandOpen(1, " <> tshow gidO <> ", "
                      <> pageT otherPage <> "))") `shouldReturn` "true"
            atomicModifyIORef' (worldManagerRef (bnEnv b)) $ \wm →
                (wm { wmVisible = [hudPage] }, ())
            _ ← luaOk b "return _G.__exec(1)"
            realizedOn wsOther `shouldReturn` True
            realizedOn wsHud `shouldReturn` False
            Just (_, _, afterO, _) ← shellOf wsOther
            afterO `shouldBe` beforeO + 2
            knowledge b iidO >>= (`shouldSatisfy` T.isPrefixOf "known,")
            knowledge b iidH `shouldReturn` "unknown,nil,nil"
            luaStr b "_G.__stack()" `shouldReturn` ("portableItem@" <> tshow iidO)

        it "leaves the order standing while the carrier's page cannot be \
           \resolved, without reading the active page's same id" $ \b → do
            wsHud ← newPage "fixture_salvage"
            wsOther ← newPage "fixture_salvage"
            scene b [(hudPage, wsHud), (otherPage, wsOther)]
                [carrier 1 otherPage arrivedAt]
            _ ← luaOk b ("return UNITAI.commandOpen(1, 0, " <> pageT otherPage <> ")")
            atomicModifyIORef' (worldManagerRef (bnEnv b)) $ \wm →
                (wm { wmWorlds = filter ((≢ otherPage) ∘ fst) (wmWorlds wm) }, ())
            u ← luaStr b "tostring(_G.__util(1) == -math.huge)"
            u `shouldBe` "true"
            _ ← luaOk b "return _G.__exec(1)"
            orderOf b 1 `shouldReturn` "0"
            realizedOn wsHud `shouldReturn` False
            luaStr b "_G.__stack()" `shouldReturn` ""
