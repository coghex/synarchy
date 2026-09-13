{-# LANGUAGE OverloadedStrings #-}
-- | The "food harvest target identity" gate (#2553): both food-seeking
--   AI paths harvest the EDIBLE plant their own search chose, never an
--   inedible co-tenant sharing its tile.
--
--   @world.findHarvestableFlora@'s bare (untagged) call is a FOOD
--   search: it admits only species whose phase-resolved yield contains
--   something edible, and it reports the winning wild plant's stable
--   @instanceId@ (#1854). Both callers used to throw that identity away
--   and re-enter by coordinate through @world.harvestFlora@, which
--   applies no edibility test at all and takes the FIRST admitting
--   instance in the tile's stored order. Several plants may legitimately
--   stand on one tile, so a wood-producing co-tenant took the yield and
--   the regrowth timer while the forager walked away hungry.
--
--   Driven through a REAL 'EngineEnv' with the REAL registered flora
--   query and harvest verbs and the PRODUCTION Lua modules — the
--   'Test.Headless.Lua.UnitAiPickupPage' technique, for its reason: the
--   defect lives in which PLANT a pair of engine calls agree on, which a
--   fixture answering both from one stubbed table structurally cannot
--   see. The page is an in-memory 'emptyWorldState', so nothing here
--   runs worldgen. Only @scripts.movement_speed@ and the
--   @scripts.unit_ai@ singleton are replaced, neither on the selection
--   or harvest path; the flora catalog, the item registry, the yields,
--   the per-instance regrowth timers and the ground rows are all the
--   engine's own.
--
--   The stored order of the two plants is a FIXTURE PARAMETER, and every
--   selection case runs both ways round. With the edible plant first the
--   old coordinate pick happened to agree, which is exactly why a
--   one-order fixture would have passed against the defect.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "food harvest target identity"'@.
module Test.Headless.Lua.FoodHarvestTarget (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Ground (GroundItem(..), GroundItems(..))
import Item.Types
    ( ItemDef(..), ItemFood(..), ItemInstance(..), ItemManager(..) )
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
import World.Chunk.Types
import World.Flora.CropPlot (CropPlotOf(..))
import World.Flora.Identity
import World.Flora.Types
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (globalToChunk)
import World.Page.Types (WorldPageId(..))
import World.State.Types
import World.Tile.Types (WorldTileData(..))
import Structure.Types (emptyChunkStructures)

-- * Fixture geography

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "food_harvest_target"

pageKey ∷ Text
pageKey = "food_harvest_target"

zSlice ∷ Int
zSlice = 4

homeChunk ∷ ChunkCoord
homeChunk = ChunkCoord 1 1

-- | The SHARED tile: where the edible plant and its inedible co-tenant
--   both stand.
sharedTile ∷ (Int, Int)
sharedTile = (chunkSize + 5, chunkSize + 7)

-- | A second tile in the same chunk, for the edible-only control and
--   for the crop plot.
loneTile ∷ (Int, Int)
loneTile = (chunkSize + 6, chunkSize + 7)

-- | Where the worker stands: orthogonally adjacent to 'sharedTile', so
--   every execute below is an in-reach pick rather than a walk.
workerAt ∷ (Float, Float)
workerAt = (fromIntegral (fst sharedTile) - 1, fromIntegral (snd sharedTile))

workerUid ∷ UnitId
workerUid = UnitId 1

-- * Species and items

oakId, berryId ∷ FloraId
oakId   = FloraId 1
berryId = FloraId 2

-- | Two harvestable species that differ ONLY in what they pay: the oak
--   drops an inedible log, the berry bush an edible fruit. Neither is
--   tag-restricted, so both are eligible to the harvest verbs and only
--   the FOOD filter separates them — which is what makes a wrong pick a
--   wrong pick rather than a refused one.
probeCatalog ∷ FloraCatalog
probeCatalog = emptyFloraCatalog
    { fcSpecies = HM.fromList
        [ (1, harvestable "probe_oak"   "probe_log")
        , (2, harvestable "probe_berry" "probe_fruit") ]
    , fcNextId = 3 }
  where
    harvestable name yield = (newFloraSpecies name (TextureHandle 0))
        { fsHarvest = Just FloraHarvest
            { fhTags = [], fhUngatedTags = []
            , fhYield = [(yield, 1, 1)], fhPhaseYields = HM.empty
            , fhRegrowth = 43200, fhHarvestedTexture = TextureHandle 0 } }

-- | A wood-tagged species, for requirement 4's compatibility control:
--   a tagged harvest must still reach it by coordinate.
timberId ∷ FloraId
timberId = FloraId 3

-- | The catalog plus the wood-tagged species, used only by the tagged
--   control so the food cases keep the smallest catalog that can show
--   the defect.
timberCatalog ∷ FloraCatalog
timberCatalog = probeCatalog
    { fcSpecies = HM.insert 3 timber (fcSpecies probeCatalog)
    , fcNextId = 4 }
  where
    timber = (newFloraSpecies "probe_timber" (TextureHandle 0))
        { fsHarvest = Just FloraHarvest
            { fhTags = ["wood"], fhUngatedTags = ["wood"]
            , fhYield = [("probe_log", 1, 1)], fhPhaseYields = HM.empty
            , fhRegrowth = 43200, fhHarvestedTexture = TextureHandle 0 } }

-- | Really registered, because 'spawnYields' SKIPS a name the item
--   registry does not resolve — an unregistered yield would make every
--   pick report zero items and every assertion below vacuous. And the
--   food block is what the bare search filters on, so an unregistered
--   @probe_fruit@ would leave NOTHING edible in range and the whole
--   fixture would fail closed rather than silently.
fixtureItems ∷ ItemManager
fixtureItems = ItemManager $ HM.fromList
    [ ("probe_log",   baseItem { idName = "probe_log" })
    , ("probe_fruit", baseItem { idName = "probe_fruit"
                               , idFood = Just (ItemFood 50 0) }) ]
  where
    baseItem = ItemDef
        { idName = "", idDisplayName = "Probe Item"
        , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
        , idWeight = 1, idWeightSpec = Nothing, idBulk = 1
        , idStorage = Nothing, idKind = "misc", idCategory = "Misc"
        , idMake = "", idMaterial = "", idQualitySpec = Nothing
        , idQualityTiers = [], idContainer = Nothing
        , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
        , idArmor = Nothing, idUnequippable = False, idBuffs = []
        , idInsulation = 0, idSourcePath = "test-fixture" }

-- * Plants

instanceAt ∷ (Int, Int) → FloraId → Int → FloraInstance
instanceAt tile fid ordinal =
    let (_, (lx, ly)) = globalToChunk (fst tile) (snd tile)
    in FloraInstance
        { fiSpecies = fid
        , fiTileX = fromIntegral lx, fiTileY = fromIntegral ly
        , fiOffU = 0, fiOffV = 0, fiZ = zSlice
        , fiAge = 1, fiHealth = 1, fiVariant = 0, fiBaseWidth = 8
        , fiInstanceId = idAt tile fid ordinal
        , fiChopDesignated = False
        }

-- | Through the PRODUCTION identity function, so the fixture cannot
--   drift from the engine's own answer. The catalog is 'timberCatalog',
--   which is a superset of 'probeCatalog' and therefore names every
--   fixture species — an unnamed one would derive from the empty string
--   and collide with any other.
idAt ∷ (Int, Int) → FloraId → Int → FloraInstanceId
idAt (gx, gy) fid ordinal =
    generatedFloraInstanceId pageKey gx gy
        (maybe "" fsName (lookupSpecies fid timberCatalog)) ordinal

oakIid, berryIid ∷ FloraInstanceId
oakIid   = idAt sharedTile oakId 0
berryIid = idAt sharedTile berryId 0

-- | The co-tenant pair in the two stored orders every selection case is
--   run in. INEDIBLE FIRST is the order the defect needed; the reverse
--   is the order that used to pass by luck.
inedibleFirst, edibleFirst ∷ [FloraInstance]
inedibleFirst = [ instanceAt sharedTile oakId 0
                , instanceAt sharedTile berryId 0 ]
edibleFirst   = [ instanceAt sharedTile berryId 0
                , instanceAt sharedTile oakId 0 ]

-- * Page fixtures

chunkWith ∷ [FloraInstance] → LoadedChunk
chunkWith insts =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
                 { ctStartZ = 0
                 , ctMats   = VU.replicate 20 1
                 , ctSlopes = VU.replicate 20 0
                 , ctVeg    = VU.replicate 20 0 }
    in LoadedChunk
        { lcCoord = homeChunk
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area zSlice
        , lcTerrainSurfaceMap = VU.replicate area zSlice
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap
        , lcFlora = FloraChunkData insts
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

-- | A fresh page holding exactly these plants, with the worker standing
--   beside 'sharedTile' and hungry enough for the forage rung to fire.
--   The page carries NO gen params, like every other bare-page Lua
--   fixture: the world worker would otherwise chase the visible page
--   and fail-stop trying to GENERATE it. That makes the page
--   non-wrapping, which is deliberate rather than a gap —
--   'canonicalTile' is the identity at world size 0, this fixture's
--   tiles are inland, and the seam behaviour of both harvest verbs
--   belongs to "flora instance identity", which owns it.
resetScene ∷ EngineEnv → FloraCatalog → [FloraInstance] → IO WorldState
resetScene env cat insts = do
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) WorldTileData
        { wtdChunks = HM.singleton homeChunk (chunkWith insts)
        , wtdMaxChunks = 200 }
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (floraCatalogRef env) cat
    writeIORef (itemManagerRef env) fixtureItems
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" (minimalDef "acolyte")
        , umInstances = HM.singleton workerUid (mkWorker workerAt) }
    pure ws

minimalDef ∷ Text → UnitDef
minimalDef name = UnitDef
    { udName = name, udNamePool = Nothing, udDisplayName = Just name
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- | Hungry, carrying nothing, ample capacity. The hunger numbers are
--   what put the forage rung above its own @forage_max_fraction@ gate;
--   the farming skill is what auto-harvest scales its work by.
mkWorker ∷ (Float, Float) → UnitInstance
mkWorker (gx, gy) = UnitInstance
    { uiDefName = "acolyte", uiName = "Nael", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = gx, uiGridY = gy, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.fromList
        [ ("carrying_capacity", 1000)
        , ("hunger", 5), ("max_hunger", 100)
        , ("calories", 100), ("max_calories", 2000) ]
    , uiModifiers = HM.empty
    , uiSkills = HM.singleton "farming" 50
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- * Live-state readers

-- | Every ground row the page holds, as @defName@s in ascending gid
--   order — the yields a pick actually spawned, read off the engine's
--   own store rather than out of the Lua return value.
groundNames ∷ WorldState → IO [Text]
groundNames ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    pure $ map snd $ L.sortOn fst
        [ (gid, iiDefName (giInst gi)) | (gid, gi) ← HM.toList (gisItems gis) ]

-- | Which plants carry a live regrowth timer. The whole defect was
--   visible here first: the wrong plant's timer started.
timerIds ∷ WorldState → IO [FloraInstanceId]
timerIds ws = L.sort . HM.keys <$> readIORef (wsFloraHarvestsRef ws)

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

runOk ∷ LuaBackendState → Text → IO Text
runOk ls src = do
    r ← evalDebug ls src
    r `shouldNotSatisfy` isLuaError
    pure r

-- | Load BOTH production food paths against this backend, with a fresh
--   per-unit AI state.
--
--   @scripts.movement_speed@ is replaced for the reason every other
--   production-module fixture replaces it: the pace a walk picks is not
--   what this gate is about, and the real module reaches four
--   physiology modules to answer one number. Nothing on the selection
--   or harvest path is replaced.
loadAi ∷ LuaBackendState → IO ()
loadAi ls = do
    _ ← runOk ls $ luaLines
        [ "package.loaded['scripts.unit_ai'] = {};"
        , "package.loaded['scripts.movement_speed'] ="
        , "  { comfort = function() return 1.0 end,"
        , "    ordered = function() return 1.15 end,"
        , "    meander = function() return 0.5 end,"
        , "    sprint  = function() return 2.0 end };"
        , "_G.NEEDS = require('scripts.unit_ai_needs');"
        , "require('scripts.unit_ai_farm');"
        , "_G.UNITAI = package.loaded['scripts.unit_ai'];"
        , "_G.HARVEST = UNITAI.harvest;"
        , "_G.S = {};"
        , "_G.PARAMS = { forage_search_radius = 24,"
        , "              forage_max_fraction = 0.9,"
        , "              forage_base_weight = 3.0,"
        , "              forage_urgency_scale = 5.0,"
        , "              harvest_scan_range = 24.0,"
        , "              harvest_base_utility = 2.0,"
        , "              harvest_rate = 0.5,"
        , "              harvest_xp_per_harvest = 1.0 };"
        , "return 'ok'" ]
    pure ()

-- | One full arbitration pass of the FORAGE rung: score (which is what
--   selects the plant and stores its identity), then execute (which is
--   what picks it).
forageTick ∷ LuaBackendState → IO Text
forageTick ls = runOk ls $ luaLines
    [ "local u = NEEDS.forageUtility(1, S, PARAMS);"
    , "if u <= -math.huge then return 'unselectable' end;"
    , "NEEDS.forageExecute(1, S, PARAMS);"
    , "return 'ok'" ]

-- | The same for AUTO-HARVEST, whose pick is a WORK action.
--
--   The engine's game clock does not advance in a headless fixture, so
--   the accumulator can bank nothing on its own; the banked work is
--   written directly AFTER the real 'unitAi.harvest.bindProgress' has
--   run over the real selection, and the second execute is the one that
--   completes. That keeps the binding under test — a bind that reset
--   spuriously would zero this again and the pick would never land —
--   while leaving how LONG a pick takes to the "skill-scaled
--   auto-harvest" gate, which owns it.
harvestTick ∷ LuaBackendState → IO Text
harvestTick ls = runOk ls $ luaLines
    [ "local u = HARVEST.utility(1, S, PARAMS);"
    , "if u <= -math.huge then return 'unselectable' end;"
    , "HARVEST.execute(1, S, PARAMS);"
    , "if S.harvestTarget then"
    , "  S.harvestProgress = HARVEST.WORK_TOTAL;"
    , "  HARVEST.execute(1, S, PARAMS);"
    , "end;"
    , "return 'ok'" ]

-- | The two production paths, named, so every selection case runs both
--   without being written twice.
foodPaths ∷ [(String, LuaBackendState → IO Text)]
foodPaths = [("the forage rung", forageTick), ("auto-harvest", harvestTick)]

-- | The two stored orders, named the same way.
orders ∷ [(String, [FloraInstance])]
orders = [("inedible first", inedibleFirst), ("edible first", edibleFirst)]

idNum ∷ FloraInstanceId → Int64
idNum = floraInstanceIdToLua

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "food harvest target identity" $ do

    describe "a shared tile pays the plant the food search chose" $
      forM_ orders $ \(orderName, insts) →
        forM_ foodPaths $ \(pathName, tick) →
          it (pathName <> " harvests the edible plant with the "
              <> orderName <> ", leaving its co-tenant's timer alone") $
            \env → do
              ws ← resetScene env probeCatalog insts
              ls ← newBareLuaBackend env
              loadAi ls
              tick ls `shouldReturn` "ok"
              -- The yield is FOOD. Under the defect this was probe_log
              -- whenever the oak was listed first.
              groundNames ws `shouldReturn` ["probe_fruit"]
              -- And only the berry bush is regrowing: the oak was never
              -- touched, so nothing of its state moved. Named both ways
              -- round, so a failure says WHICH plant took the pick.
              timers ← timerIds ws
              timers `shouldSatisfy` notElem oakIid
              timers `shouldBe` [berryIid]

    describe "an edible-only tile is unchanged" $
      forM_ foodPaths $ \(pathName, tick) →
        it (pathName <> " still picks a lone edible plant") $ \env → do
          ws ← resetScene env probeCatalog [instanceAt sharedTile berryId 0]
          ls ← newBareLuaBackend env
          loadAi ls
          tick ls `shouldReturn` "ok"
          groundNames ws `shouldReturn` ["probe_fruit"]
          timerIds ws `shouldReturn` [berryIid]

    describe "a plant that disappears between selection and execution" $
      -- Requirement 2, driven EXECUTE-ONLY. Both callers re-run their
      -- finder on every arbitration tick and overwrite the stored
      -- target, so a fixture that removed the plant and then ticked
      -- normally would simply re-find or clear — and would pass with
      -- the identity plumbing deleted. Selecting first and executing
      -- after the world has moved is the only shape that asks the
      -- question.
      forM_ orders $ \(orderName, insts) → do

        it ("the forage rung refuses rather than taking the co-tenant ("
            <> orderName <> ")") $ \env → do
          ws ← resetScene env probeCatalog insts
          ls ← newBareLuaBackend env
          loadAi ls
          -- Select while the berry bush is there.
          _ ← runOk ls "NEEDS.forageUtility(1, S, PARAMS); return 'ok'"
          runOk ls "return tostring(S.forageTarget.iid)"
            `shouldReturn` tshow (idNum berryIid)
          -- It is picked by someone else before this worker executes.
          writeIORef (wsTilesRef ws) WorldTileData
              { wtdChunks = HM.singleton homeChunk
                    (chunkWith [instanceAt sharedTile oakId 0])
              , wtdMaxChunks = 200 }
          _ ← runOk ls "NEEDS.forageExecute(1, S, PARAMS); return 'ok'"
          -- No wood yield, and the surviving oak carries no timer: the
          -- refusal touched nothing at all.
          groundNames ws `shouldReturn` []
          timerIds ws `shouldReturn` []
          runOk ls "return tostring(S.forageTarget)" `shouldReturn` "nil"

        it ("auto-harvest refuses rather than taking the co-tenant ("
            <> orderName <> ")") $ \env → do
          ws ← resetScene env probeCatalog insts
          ls ← newBareLuaBackend env
          loadAi ls
          -- Select, arrive, and bank the work — all while the chosen
          -- plant is still standing.
          _ ← runOk ls $ luaLines
              [ "HARVEST.utility(1, S, PARAMS);"
              , "HARVEST.execute(1, S, PARAMS);"
              , "S.harvestProgress = HARVEST.WORK_TOTAL;"
              , "return 'ok'" ]
          runOk ls "return tostring(S.harvestTarget.iid)"
            `shouldReturn` tshow (idNum berryIid)
          writeIORef (wsTilesRef ws) WorldTileData
              { wtdChunks = HM.singleton homeChunk
                    (chunkWith [instanceAt sharedTile oakId 0])
              , wtdMaxChunks = 200 }
          -- The completing tick, with no scan in front of it.
          _ ← runOk ls "HARVEST.execute(1, S, PARAMS); return 'ok'"
          groundNames ws `shouldReturn` []
          timerIds ws `shouldReturn` []
          runOk ls "return tostring(S.harvestTarget)" `shouldReturn` "nil"

    describe "compatibility controls" $ do

      -- Requirement 3. A crop plot carries no instanceId at all, so the
      -- callers must branch on that ABSENCE and keep the coordinate
      -- verb. Nothing else can name a plot.
      forM_ foodPaths $ \(pathName, tick) →
        it (pathName <> " still harvests a groundcover crop plot, which "
            <> "has no instance identity to name") $ \env → do
          ws ← resetScene env probeCatalog []
          writeIORef (wsCropPlotsRef ws)
              (HM.singleton sharedTile (CropPlot berryId 0 1.0 ∷ CropPlotOf FloraId))
          ls ← newBareLuaBackend env
          loadAi ls
          tick ls `shouldReturn` "ok"
          groundNames ws `shouldReturn` ["probe_fruit"]
          -- An annual plot is CLEARED by its harvest rather than given a
          -- regrowth timer, so the absence of both is the whole answer.
          timerIds ws `shouldReturn` []
          HM.null <$> readIORef (wsCropPlotsRef ws) `shouldReturn` True

      -- Requirement 4. The tagged coordinate verb chop uses is
      -- untouched, including on a tile the food search would have
      -- ranked differently.
      it "leaves tagged wood harvesting on its coordinate contract" $
        \env → do
          ws ← resetScene env timberCatalog
              [ instanceAt loneTile timberId 0 ]
          ls ← newBareLuaBackend env
          runOk ls (T.concat
              [ "local r = world.harvestFlora(", tshow (fst loneTile), ", "
              , tshow (snd loneTile), ", 'wood'); "
              , "return r and #r or -1" ])
            `shouldReturn` "1"
          groundNames ws `shouldReturn` ["probe_log"]

      -- And the exact-instance verb the food paths moved onto is the
      -- same one chop already used, so a wood-tagged fell by identity
      -- keeps working beside them.
      it "leaves the exact-instance verb's tagged contract alone" $
        \env → do
          let timberIid = idAt loneTile timberId 0
          ws ← resetScene env timberCatalog
              [ instanceAt loneTile timberId 0 ]
          ls ← newBareLuaBackend env
          runOk ls (T.concat
              [ "local r = world.harvestFloraInstance("
              , tshow (fst loneTile), ", ", tshow (snd loneTile), ", "
              , tshow (idNum timberIid), ", 'wood'); "
              , "return r and #r or -1" ])
            `shouldReturn` "1"
          timerIds ws `shouldReturn` [timberIid]
