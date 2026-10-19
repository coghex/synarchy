{-# LANGUAGE OverloadedStrings #-}
-- | "Pick up" under the shared assignment rule, and pending container
--   shells through the pickup order (#2522, epic #1231 PLC-16).
--
--   Two halves of one gesture:
--
--   * ASSIGNMENT. The ground-item menu's carrier is whoever
--     @transfer_session.resolveSource@ picks — the arc-wide rule (D-8,
--     D-28): nearest to the target in three dimensions, lowest uid on
--     an exact tie, player-commandable and able to run @pickup_ground@,
--     filtered before ranking — with the opt-in no-selection fallback
--     to the nearest eligible unit on the target's own page.
--   * PENDING SHELLS. @unitAi.commandPickup@ accepts a pending shell
--     without the command-time weight gate (it has no truthful weight
--     yet), and on arrival the executor realizes it, records a weight
--     observation, and runs the ordinary arrival gate against the
--     realized weight.
--
--   Driven through a PRIVATE 'EngineEnv' with the real registered Lua
--   API and the PRODUCTION @scripts/transfer_session.lua@,
--   @scripts/init_context_menu_item.lua@ and @scripts/unit_ai_pickup.lua@.
--   Pages are in-memory 'emptyWorldState's carrying a real location
--   table, loot-profile registry and terrain, so the pending
--   classification, the realization, the knowledge drain and the
--   terrain-derived target height are all the engine's own. Replaced:
--   @scripts.movement_speed@ (as in "Test.Headless.Lua.UnitAiPickupPage"),
--   the @scripts.unit_ai@ singleton the submodule attaches to,
--   @unit.getSelected@ (selection is an input here) and
--   @item.hitTestAt@ (it needs a camera); and @contextMenu.show@ only
--   records the rows it was handed.
--
--   Run with the rest of its gate: @cabal test synarchy-test-headless
--   --test-options='--match "pickup order page ownership"'@.
module Test.Headless.Lua.UnitAiPickupPending (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
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
import Item.Knowledge (lookupPortable)
import Item.Types (ItemInstance(..))
import Location.Instance
    ( LocationContainerSlot(..), LocationInstance(..), LocationInstanceId(..)
    , LocationInstances(..), emptyLocationInstances, instancesToList
    , newLocationInstance )
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Item.PortableKnowledge.Fixture (mkUnitOfFaction)
import Test.Headless.Location.ContainerShells
    ( containerContent, containerItemDefs, mkDef, paramsWith, spawnContainer )
import LootProfile.Types
    ( LootProfileDef(..), LootProfileEntry(..), LootProfileRegistry
    , emptyLootProfileRegistry, registerLootProfile )
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..), unWorldPageId)
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )
import World.Thread (worldTickWith)
import World.Tile.Types (WorldTileData(..))

-- * Fixture identities

-- | The ACTIVE page at the start of every example.
pageA ∷ WorldPageId
pageA = WorldPageId "pickup2522_a"

-- | Live, and the carrier's own page wherever the case is about page
--   identity.
pageB ∷ WorldPageId
pageB = WorldPageId "pickup2522_b"

-- | Where every shell is spawned on its page, and where an arriving
--   carrier stands.
shellAt ∷ (Float, Float)
shellAt = (8, 8)

-- | The terrain height under 'shellAt' on every fixture page, so a
--   ground row's @gridZ@ is a number this file chose rather than 0.
shellZ ∷ Int
shellZ = 10

-- * Content

-- | Two rations per realization: a known, nonzero cargo.
salvageProfile ∷ LootProfileDef
salvageProfile = LootProfileDef
    { lpdId = "fixture_salvage", lpdMultiplierMin = 2, lpdMultiplierMax = 2
    , lpdEntries = [ LootProfileEntry { lpeItem = "rations", lpeChance = 1.0
                                      , lpeQuantityFactor = 1 } ] }

profiles ∷ LootProfileRegistry
profiles = registerLootProfile salvageProfile emptyLootProfileRegistry

-- | One crate slot, on location instance 1, naming @profile@ — which is
--   @"fixture_gone"@ (registered nowhere) for the D-23 refusal.
crateTable ∷ Text → LocationInstances
crateTable profile =
    let liid = LocationInstanceId 1
        def  = mkDef "pickup2522_site" [containerContent "fixture_crate" profile 1]
        inst = either (error ∘ show) id
            (newLocationInstance Nothing liid (ChunkCoord 0 0) def)
    in emptyLocationInstances { lisNextId = 2, lisById = HM.singleton liid inst }

-- | Flat terrain at 'shellZ' over the one chunk every fixture uses.
flatTiles ∷ WorldTileData
flatTiles =
    let area = chunkSize * chunkSize
        z    = VU.replicate area shellZ
        lc = LoadedChunk
            { lcCoord = ChunkCoord 0 0, lcTiles = V.empty
            , lcSurfaceMap = z, lcTerrainSurfaceMap = z
            , lcFluidMap = V.replicate area Nothing, lcIceMap = emptyIceMap
            , lcFlora = emptyFloraChunkData, lcSideDeco = VU.empty
            , lcWaterTableMap = VU.empty, lcMagma = Nothing
            , lcStructures = emptyChunkStructures }
    in WorldTileData { wtdChunks = HM.singleton (ChunkCoord 0 0) lc
                     , wtdMaxChunks = 1, wtdBounds = Nothing }

-- | A fresh page whose one crate slot names @profile@, on flat terrain.
newPage ∷ Text → IO WorldState
newPage profile = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just (paramsWith (crateTable profile)))
    writeIORef (wsTilesRef ws) flatTiles
    pure ws

-- | A carrier: player-commandable unless told otherwise, named, at a
--   position and z, with a carrying capacity.
data Carrier = Carrier
    { cUid ∷ Int, cPage ∷ WorldPageId, cFaction ∷ Faction
    , cDef ∷ Text, cAt ∷ (Float, Float), cZ ∷ Int, cCap ∷ Float
    , cPose ∷ Text }

carrier ∷ Int → WorldPageId → (Float, Float) → Int → Float → Carrier
carrier u p at z cap = Carrier u p FactionPlayer "acolyte" at z cap "standing"

asUnit ∷ Carrier → UnitInstance
asUnit c = (mkUnitOfFaction (cFaction c) (cPage c) [] HM.empty [])
    { uiDefName = cDef c, uiName = "Unit" <> tshow (cUid c)
    , uiGridX = fst (cAt c), uiGridY = snd (cAt c)
    , uiGridZ = cZ c, uiRealZ = fromIntegral (cZ c)
    , uiStats = HM.singleton "carrying_capacity" (cCap c)
    , uiPose = cPose c }

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

-- | A private engine and Lua backend per example: these cases install a
--   page set, item and loot registries, and drive real world ticks.
withBindings ∷ (Bindings → IO α) → IO α
withBindings act = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    writeIORef (itemManagerRef env) containerItemDefs
    writeIORef (lootProfileRegistryRef env) profiles
    writeIORef (notificationCfgRef env) $ HM.fromList
        [ (c, logOnlyCategory c) | c ← ["unit_event", "unit_warning"] ]
    atomically $ modifyTVar' (eventStoreRef env) clearEventStoreRows
    act (Bindings env ls)

-- | Install the pages (the FIRST is active) and the carriers, and spawn
--   each page's one shell through the real binding verb.
scene ∷ Bindings → [(WorldPageId, WorldState)] → [Carrier] → IO ()
scene b pages cs = do
    let env = bnEnv b
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = pages, wmVisible = take 1 (map fst pages) }
    forM_ pages $ \(pid, _) →
        spawnContainer env pid 1 1 shellAt `shouldReturn` True
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.fromList
            [ (UnitId (fromIntegral (cUid c)), asUnit c) | c ← cs ]
        , umNextId = 100 }
    _ ← Q.flushQueue (unitQueue env)
    pure ()

-- | Make @pid@ the active page.
activate ∷ Bindings → WorldPageId → IO ()
activate b pid = atomicModifyIORef' (worldManagerRef (bnEnv b)) $ \wm →
    (wm { wmVisible = [pid] }, ())

-- * Readers

-- | The page's one shell: (gid, instance id, contents count).
shellOf ∷ WorldState → IO (Maybe (Int, Word64, Int))
shellOf ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    pure $ case [ (gid, iiInstanceId (giInst gi), length (iiContents (giInst gi)))
                | (gid, gi) ← HM.toList (gisItems gis)
                , iiDefName (giInst gi) ≡ "fixture_crate" ] of
        [one] → Just one
        _     → Nothing

-- | The page's one slot latch.
realizedOn ∷ WorldState → IO Bool
realizedOn ws = do
    mp ← readIORef (wsGenParamsRef ws)
    pure $ or [ lcsRealized s | p ← maybeToList mp
                              , inst ← instancesToList (wgpLocationInstances p)
                              , s ← liContainers inst ]

inventory ∷ Bindings → Int → IO [(Text, Int)]
inventory b u = do
    um ← readIORef (unitManagerRef (bnEnv b))
    pure [ (iiDefName i, length (iiContents i))
         | Just inst ← [HM.lookup (UnitId (fromIntegral u)) (umInstances um)]
         , i ← uiInventory inst ]

warnings ∷ Bindings → IO [Text]
warnings b = do
    evs ← map seEvent ⊚ readEventLog (bnEnv b)
    pure [ peText e | e ← evs, peCategory e ≡ "unit_warning" ]

-- | Run one world tick, so a queued knowledge observation lands (or,
--   for an absence, so "absent" means never enqueued).
drainWorld ∷ Bindings → IO ()
drainWorld b = do
    lastRef ← newIORef 0
    _ ← worldTickWith (pure 0) (bnEnv b) lastRef
    pure ()

-- | Whether the player's memory holds a weight observation for this
--   instance, and whether it holds a contents observation.
knowledgeOf ∷ Bindings → Word64 → IO (Bool, Bool)
knowledgeOf b iid = do
    drainWorld b
    wm ← readIORef (worldManagerRef (bnEnv b))
    lua ← luaRun b $ T.concat
        [ "local k = item.getContainerKnowledge(", tshow iid, ");"
        , "return tostring(k.storedWeight ~= nil) .. ',' .. tostring(k.items ~= nil)" ]
    let persisted = isJust (lookupPortable iid (wmPortableKnowledge wm))
    case T.splitOn "," (T.filter (≢ '"') lua) of
        [w, c] → pure (persisted ∧ w ≡ "true", c ≡ "true")
        _      → fail ("unexpected knowledge read: " <> T.unpack lua)

-- * Lua plumbing

luaRun ∷ Bindings → Text → IO Text
luaRun b src = do
    r ← executeDebugLua (lbsLuaState (bnLua b)) src
    r `shouldNotSatisfy` (\t → "error:" `T.isPrefixOf` t
                             ∨ "syntax error:" `T.isPrefixOf` t)
    pure r

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- | The production pickup module, the shared resolver and the ground
--   menu, with @pickup_ground@ registered for acolytes only (so the
--   capability filter has a species to refuse) and the menu's renderer
--   recording its rows.
loadModules ∷ Bindings → IO ()
loadModules b = do
    _ ← luaRun b $ luaLines
        [ "package.loaded['scripts.unit_ai'] = {};"
        , "package.loaded['scripts.movement_speed'] ="
        , "  { comfort = function() return 1.0 end,"
        , "    ordered = function() return 1.15 end,"
        , "    meander = function() return 0.5 end,"
        , "    sprint  = function() return 2.0 end };"
        , "_G.PICKUP = require('scripts.unit_ai_pickup');"
        , "_G.CORE = require('scripts.unit_ai_core');"
        , "_G.UNITAI = package.loaded['scripts.unit_ai'];"
        , "_G.SESSION = require('scripts.transfer_session');"
        , "require('scripts.unit_ai_actions').record('acolyte',"
        , "  { { name = 'pickup_ground' } });"
        , "require('scripts.unit_ai_actions').record('bear', { { name = 'wander' } });"
        , "_G.PARAMS = { pickup_timeout = 30, pickup_utility = 7.5,"
        , "              pickup_arrival_tiles = 1.5 };"
        , "local cm = require('scripts.ui.context_menu');"
        , "cm.show = function(items) _G.__rows = {};"
        , "  for _, it in ipairs(items) do _G.__rows[it.label] = it.callback end end;"
        , "return 'ok'" ]
    pure ()

-- | @resolveSource@ over a selection, against the shell row on @page@
--   — read through the REAL @item.listGround@ while @page@ is active, so
--   the target's @gridZ@ and @page@ are the engine's own projection.
resolveFor ∷ Bindings → WorldPageId → [Int] → IO Text
resolveFor b (WorldPageId page) sel = luaRun b $ luaLines
    [ "local row; for _, g in ipairs(item.listGround() or {}) do"
    , "  if g.defName == 'fixture_crate' then row = g end end;"
    , "assert(row and row.page == '" <> page <> "', 'shell row');"
    , "local t = { gridX = row.x, gridY = row.y, gridZ = row.gridZ, page = row.page };"
    , "return tostring(SESSION.resolveSource({" <> T.intercalate "," (map tshow sel)
        <> "}, nil, t, 'pickup_ground', { fallback = true }))" ]

-- | Open the real ground menu on the active page's shell, with
--   @selection@ selected, and answer which rows it offered.
openMenu ∷ Bindings → [Int] → IO Text
openMenu b sel = luaRun b $ luaLines
    [ "local row; for _, g in ipairs(item.listGround() or {}) do"
    , "  if g.defName == 'fixture_crate' then row = g end end;"
    , "item.hitTestAt = function() return row.id, row.page end;"
    , "unit.getSelected = function() return {" <> T.intercalate "," (map tshow sel) <> "} end;"
    , "_G.__rows = nil;"
    , "require('scripts.init_context_menu_item').tryItemMenu(1, 1);"
    , "return tostring(_G.__rows and _G.__rows['Pick up'] ~= nil)" ]

-- | The live order's gid, or "nil".
orderOf ∷ Bindings → Int → IO Text
orderOf b u = T.filter (≢ '"') <$> luaRun b
    ("local s = CORE.aiState[" <> tshow u <> "]; "
     <> "return tostring(s and s.pickupOrder and s.pickupOrder.gid)")

-- | One executor call for carrier @u@ (the order, if any, is its own).
execute ∷ Bindings → Int → IO ()
execute b u = void $ luaRun b
    ("local s = CORE.ensureState(" <> tshow u <> "); "
     <> "PICKUP.pickupExecute(" <> tshow u <> ", s, PARAMS); return 'ok'")

unq ∷ Text → Text
unq = T.filter (≢ '"')

-- * The spec

spec ∷ Spec
spec = describe "pickup order page ownership — the shared assignment rule \
                \and pending shells (#2522)" $ around withBindings $ do

    describe "assignment through transfer_session.resolveSource" $ do

        it "picks the unit NEAREST the target in three dimensions, not the \
           \lowest one, from a selection at equal x/y" $ \b → do
            -- The shell rests at z 10. Uid 1 stands lowest (z 0, ten
            -- levels below), uid 2 at z 7 (three below) and uid 3 at
            -- z 12 (two above): the nearest is uid 3, and a rule that
            -- preferred absolute elevation would answer uid 1.
            ws ← newPage "fixture_salvage"
            scene b [(pageA, ws)]
                [ carrier 1 pageA shellAt 0 100
                , carrier 2 pageA shellAt 7 100
                , carrier 3 pageA shellAt 12 100 ]
            loadModules b
            unq <$> resolveFor b pageA [2, 1, 3] `shouldReturn` "3"
            -- And the row's height IS the terrain's, which is what made
            -- that the right answer.
            unq <$> luaRun b "local r = item.listGround()[1]; return tostring(r.gridZ)"
                `shouldReturn` tshow shellZ

        it "breaks an exact tie on the LOWEST uid whatever the selection \
           \order" $ \b → do
            ws ← newPage "fixture_salvage"
            -- Mirror images about the shell: identical distance.
            scene b [(pageA, ws)]
                [ carrier 4 pageA (6, 8) shellZ 100
                , carrier 9 pageA (10, 8) shellZ 100 ]
            loadModules b
            unq <$> resolveFor b pageA [9, 4] `shouldReturn` "4"
            unq <$> resolveFor b pageA [4, 9] `shouldReturn` "4"

        it "with an empty selection assigns the nearest ELIGIBLE unit on the \
           \target's page: never a non-commandable one, an incapable one, \
           \or one on another page" $ \b → do
            wsA ← newPage "fixture_salvage"
            wsB ← newPage "fixture_salvage"
            scene b [(pageA, wsA), (pageB, wsB)]
                [ (carrier 1 pageA shellAt shellZ 100) { cFaction = FactionWildlife }
                , (carrier 2 pageA shellAt shellZ 100) { cDef = "bear" }
                , carrier 3 pageB shellAt shellZ 100
                , carrier 7 pageA (12, 8) shellZ 100
                , carrier 8 pageA (14, 8) shellZ 100 ]
            loadModules b
            -- 1 (wildlife), 2 (a species without pickup_ground) and 3
            -- (on page B) all stand ON the shell; 7 is the nearest
            -- eligible unit on page A.
            unq <$> resolveFor b pageA [] `shouldReturn` "7"

        it "never assigns a SELECTED unit standing on another page, \
           \however near its own coordinates are" $ \b → do
            wsA ← newPage "fixture_salvage"
            wsB ← newPage "fixture_salvage"
            scene b [(pageA, wsA), (pageB, wsB)]
                [ carrier 3 pageB shellAt shellZ 100
                , carrier 7 pageA (12, 8) shellZ 100 ]
            loadModules b
            -- Unit 3 stands exactly on page B's same-numbered shell.
            unq <$> resolveFor b pageA [3, 7] `shouldReturn` "7"
            unq <$> resolveFor b pageA [3] `shouldReturn` "nil"

        it "never assigns a DEAD unit: a nearer corpse loses to a living \
           \carrier, and a page of corpses assigns nobody" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(pageA, ws)]
                [ (carrier 1 pageA shellAt shellZ 100) { cPose = "dead" }
                , carrier 6 pageA (12, 8) shellZ 100 ]
            loadModules b
            unq <$> resolveFor b pageA [] `shouldReturn` "6"
            unq <$> resolveFor b pageA [1, 6] `shouldReturn` "6"
            unq <$> openMenu b [] `shouldReturn` "true"
            _ ← luaRun b "_G.__rows['Pick up'](); return 'ok'"
            orderOf b 6 `shouldReturn` "0"
            orderOf b 1 `shouldReturn` "nil"
            -- Only corpses on the page: nobody, and no Pick up row.
            ws2 ← newPage "fixture_salvage"
            scene b [(pageA, ws2)]
                [ (carrier 1 pageA shellAt shellZ 100) { cPose = "dead" } ]
            unq <$> resolveFor b pageA [] `shouldReturn` "nil"
            unq <$> openMenu b [] `shouldReturn` "false"

        it "ranks in the TARGET page's seam frame whichever page is \
           \visible" $ \b → do
            -- Page B wraps at world size 64; page A (visible) has no
            -- gen params, so ITS frame is the identity. Candidate 1 is
            -- physically adjacent to the target across B's seam;
            -- candidate 2 is ten tiles off in raw coordinates.
            wsA ← emptyWorldState
            wsB ← emptyWorldState
            writeIORef (wsGenParamsRef wsB)
                (Just (paramsWith emptyLocationInstances) { wgpWorldSize = 64 })
            writeIORef (worldManagerRef (bnEnv b)) emptyWorldManager
                { wmWorlds = [(pageA, wsA), (pageB, wsB)], wmVisible = [pageA] }
            writeIORef (unitManagerRef (bnEnv b)) emptyUnitManager
                { umInstances = HM.fromList
                    [ (UnitId 1, asUnit (carrier 1 pageB (-256, 256) 0 100))
                    , (UnitId 2, asUnit (carrier 2 pageB (245, -245) 0 100)) ]
                , umNextId = 3 }
            loadModules b
            let resolveSeam = unq <$> luaRun b (luaLines
                    [ "local t = { gridX = 255, gridY = -255, gridZ = 0,"
                    , "            page = '" <> unWorldPageId pageB <> "' };"
                    , "return tostring(SESSION.resolveSource({}, nil, t,"
                    , "  'pickup_ground', { fallback = true }))" ])
            resolveSeam `shouldReturn` "1"
            activate b pageB
            resolveSeam `shouldReturn` "1"

        it "answers nil for a nonempty selection with no eligible member, \
           \even with an eligible unselected unit standing by" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(pageA, ws)]
                [ (carrier 1 pageA shellAt shellZ 100) { cDef = "bear" }
                , (carrier 2 pageA shellAt shellZ 100) { cFaction = FactionWildlife }
                , carrier 5 pageA (9, 8) shellZ 100 ]
            loadModules b
            unq <$> resolveFor b pageA [1, 2] `shouldReturn` "nil"
            -- …and the menu omits Pick up for that selection, but
            -- offers it with nothing selected.
            unq <$> openMenu b [1, 2] `shouldReturn` "false"
            unq <$> openMenu b [] `shouldReturn` "true"

        it "answers nil, and the menu omits Pick up, on a page with no \
           \eligible unit at all" $ \b → do
            wsA ← newPage "fixture_salvage"
            wsB ← newPage "fixture_salvage"
            scene b [(pageA, wsA), (pageB, wsB)]
                [ carrier 3 pageB shellAt shellZ 100 ]
            loadModules b
            unq <$> resolveFor b pageA [] `shouldReturn` "nil"
            unq <$> openMenu b [] `shouldReturn` "false"

        it "the menu dispatches the resolved carrier with the target's \
           \page, and an off-page carrier is refused" $ \b → do
            wsA ← newPage "fixture_salvage"
            wsB ← newPage "fixture_salvage"
            scene b [(pageA, wsA), (pageB, wsB)]
                [ carrier 6 pageA (12, 8) shellZ 100
                , carrier 3 pageB shellAt shellZ 100 ]
            loadModules b
            unq <$> openMenu b [] `shouldReturn` "true"
            _ ← luaRun b "_G.__rows['Pick up'](); return 'ok'"
            orderOf b 6 `shouldReturn` "0"
            -- Unit 3 stands on page B, whose own shell is ALSO gid 0: a
            -- dispatch naming page A must refuse it, storing nothing.
            unq <$> luaRun b ("return tostring(UNITAI.commandPickup(3, 0, '"
                               <> unWorldPageId pageA <> "'))")
                `shouldReturn` "false"
            orderOf b 3 `shouldReturn` "nil"
            warnings b `shouldReturn` []

    describe "a pending shell through the pickup order" $ do

        it "is accepted at command time ABOVE the carrier's capacity, \
           \realized once on arrival with a weight observation, and left \
           \on the ground with the warning when it does not fit" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(pageA, ws)] [carrier 1 pageA shellAt shellZ 3]
            loadModules b
            Just (gid, iid, before) ← shellOf ws
            unq <$> luaRun b ("return tostring(item.listGround()[1].pending)")
                `shouldReturn` "true"
            unq <$> luaRun b ("return tostring(UNITAI.commandPickup(1, "
                               <> tshow gid <> "))")
                `shouldReturn` "true"
            -- Nothing realized at command time.
            realizedOn ws `shouldReturn` False
            execute b 1
            realizedOn ws `shouldReturn` True
            Just (gid', iid', after) ← shellOf ws
            (gid', iid') `shouldBe` (gid, iid)
            after `shouldBe` before + 2
            inventory b 1 `shouldReturn` []
            orderOf b 1 `shouldReturn` "nil"
            ws1 ← warnings b
            length ws1 `shouldBe` 1
            mconcat ws1 `shouldSatisfy` T.isInfixOf "can't carry"
            -- The player hefted it: weight known, contents not.
            knowledgeOf b iid `shouldReturn` (True, False)
            -- A second executor call neither rerolls nor re-warns.
            execute b 1
            Just (_, _, again) ← shellOf ws
            again `shouldBe` after
            warnings b `shouldReturn` ws1

        it "measures the arrival gate on the REALIZED weight: a shell whose \
           \empty weight fits but whose cargo does not stays on the ground" $
            \b → do
            ws ← newPage "fixture_salvage"
            scene b [(pageA, ws)] [carrier 1 pageA shellAt shellZ 6.8]
            loadModules b
            Just (gid, _, before) ← shellOf ws
            -- Self-check of the premise: unrealized, it WOULD fit.
            unq <$> luaRun b "return tostring(item.listGround()[1].weight <= 6.8)"
                `shouldReturn` "true"
            unq <$> luaRun b ("return tostring(UNITAI.commandPickup(1, "
                               <> tshow gid <> "))")
                `shouldReturn` "true"
            execute b 1
            unq <$> luaRun b "return tostring(item.listGround()[1].weight > 6.8)"
                `shouldReturn` "true"
            Just (_, _, after) ← shellOf ws
            after `shouldBe` before + 2
            inventory b 1 `shouldReturn` []
            ws1 ← warnings b
            length ws1 `shouldBe` 1

        it "moves the EXACT realized tree when it fits" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(pageA, ws)] [carrier 1 pageA shellAt shellZ 100]
            loadModules b
            Just (gid, iid, before) ← shellOf ws
            unq <$> luaRun b ("return tostring(UNITAI.commandPickup(1, "
                               <> tshow gid <> "))")
                `shouldReturn` "true"
            execute b 1
            shellOf ws `shouldReturn` Nothing
            inventory b 1 `shouldReturn` [("fixture_crate", before + 2)]
            realizedOn ws `shouldReturn` True
            warnings b `shouldReturn` []
            knowledgeOf b iid `shouldReturn` (True, False)

        it "on a D-23 realization refusal moves nothing, records nothing, \
           \warns naming the unit and the crate, and retires the order" $
            \b → do
            ws ← newPage "fixture_gone"
            scene b [(pageA, ws)] [carrier 1 pageA shellAt shellZ 100]
            loadModules b
            Just (gid, iid, before) ← shellOf ws
            unq <$> luaRun b ("return tostring(UNITAI.commandPickup(1, "
                               <> tshow gid <> "))")
                `shouldReturn` "true"
            execute b 1
            shellOf ws `shouldReturn` Just (gid, iid, before)
            realizedOn ws `shouldReturn` False
            inventory b 1 `shouldReturn` []
            orderOf b 1 `shouldReturn` "nil"
            ws1 ← warnings b
            length ws1 `shouldBe` 1
            mconcat ws1 `shouldSatisfy` T.isInfixOf "Unit1"
            mconcat ws1 `shouldSatisfy` T.isInfixOf "fixture_crate"
            knowledgeOf b iid `shouldReturn` (False, False)
            -- Repeating the call repeats nothing.
            execute b 1
            warnings b `shouldReturn` ws1

        it "a REALIZED crate keeps the command-time gate, refused exactly \
           \as before" $ \b → do
            ws ← newPage "fixture_salvage"
            scene b [(pageA, ws)] [carrier 1 pageA shellAt shellZ 3]
            loadModules b
            Just (gid, _, _) ← shellOf ws
            unq <$> luaRun b ("return item.realizeGround(" <> tshow gid <> ")")
                `shouldReturn` "realized"
            unq <$> luaRun b ("return tostring(item.listGround()[1].pending)")
                `shouldReturn` "false"
            unq <$> luaRun b ("return tostring(UNITAI.commandPickup(1, "
                               <> tshow gid <> "))")
                `shouldReturn` "false"
            orderOf b 1 `shouldReturn` "nil"
            ws1 ← warnings b
            length ws1 `shouldBe` 1
            mconcat ws1 `shouldSatisfy` T.isInfixOf "can't carry"

        it "realizes only the CARRIER's shell when the active page holds a \
           \same-numbered pending shell, across an active-page switch" $
            \b → do
            wsA ← newPage "fixture_salvage"
            wsB ← newPage "fixture_salvage"
            -- B active while the order is given, then A — whose own
            -- pending shell carries the same gid — before arrival.
            scene b [(pageB, wsB), (pageA, wsA)] [carrier 1 pageB shellAt shellZ 100]
            loadModules b
            Just (gidA, iidA, beforeA) ← shellOf wsA
            Just (gidB, iidB, beforeB) ← shellOf wsB
            gidA `shouldBe` gidB
            unq <$> luaRun b ("return tostring(UNITAI.commandPickup(1, "
                               <> tshow gidB <> ", '" <> unWorldPageId pageB <> "'))")
                `shouldReturn` "true"
            activate b pageA
            execute b 1
            inventory b 1 `shouldReturn` [("fixture_crate", beforeB + 2)]
            shellOf wsB `shouldReturn` Nothing
            shellOf wsA `shouldReturn` Just (gidA, iidA, beforeA)
            realizedOn wsA `shouldReturn` False
            knowledgeOf b iidB `shouldReturn` (True, False)
            knowledgeOf b iidA `shouldReturn` (False, False)
