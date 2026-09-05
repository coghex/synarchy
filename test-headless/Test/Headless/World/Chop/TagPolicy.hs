{-# LANGUAGE OverloadedStrings #-}
-- | The authored harvest-tag policy end to end (#2212), against a real
--   headless engine.
--
--   'Test.Headless.World.FloraGrowth' pins the pure predicate and
--   'Test.Headless.Asset.FloraHarvestPolicySchema' pins the authoring
--   boundary. What is under test HERE is that the four surfaces which
--   used to decide tagged eligibility independently now give ONE
--   answer, and that an accepted fell is distinguishable from a refused
--   one at the Lua boundary:
--
--     * the screen-space selection oracle ('World.Flora.HitTest') —
--       what the player's drag box offers;
--     * the world thread's designation commit
--       ('World.Thread.Command.Cursor.Chop') — what it accepts;
--     * @world.findHarvestableFlora(..., tag)@ — what an AI search
--       reports; and
--     * @world.harvestFloraInstance(..., tag)@ — what the fell does.
--
--   The parity case walks every plant through all four on its own page
--   and requires the four answers to be equal. Before #2212 the first
--   three admitted any wood-tagged plant unconditionally while the
--   fourth applied its own rule, so a species that authored no
--   exemption could be designated and then refused — requirement 3's
--   "designated that the harvest then refuses, or vice versa".
--
--   The fixture's two species differ ONLY in the authored policy, and
--   its three ages are a sprout, a matured tree and a standing-dead one
--   at absolute day 0, so nothing here needs the clock moved.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Chop tag policy"'@.
module Test.Headless.World.Chop.TagPolicy (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (sort)
import Text.Read (readMaybe)

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..), loggerRef, worldManagerRef)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Camera (CameraFacing(..), Camera2D(..), defaultCamera)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemDef(..), ItemManager(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.HitTest
import World.Flora.Identity
    (FloraInstanceId, floraInstanceIdToLua, generatedFloraInstanceId)
import World.Flora.Types
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.SpriteDepth (noFrontWallLift)
import World.Render.ViewBounds (computeViewBounds)
import World.State.Types
import World.Thread.Command.Cursor.Chop
    (handleWorldDesignateChopInstancesCommand)
import World.Tile.Types (WorldTileData(..))
import Structure.Types (emptyChunkStructures)

-- * Fixture

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "chop_tag_policy_probe"

worldSize, zSlice, effDepth, fbW, fbH, winW, winH ∷ Int
worldSize = 64
zSlice    = 12
effDepth  = 250
fbW = 800
fbH = 600
-- A window far larger than the framebuffer, exactly as
-- 'Test.Headless.World.Chop.Selection' sizes it: the pixel→world step
-- stays well under a tile, so nothing here turns on rounding.
winW = 8000
winH = 6000

zoom ∷ Float
zoom = 20.0

-- | One handle for every phase of both species. Geometry is not what
--   these examples are about, and a phase texture missing from
--   'fhvTexSizes' would give a degenerate quad — which would make the
--   selection half pass by drawing nothing.
plantTex ∷ TextureHandle
plantTex = TextureHandle 11

texSizes ∷ HM.HashMap TextureHandle (Int, Int)
texSizes = HM.fromList [(plantTex, (96, 128))]

oakId, elmId ∷ FloraId
oakId = FloraId 1
elmId = FloraId 2

-- | Two wood-tagged trees differing ONLY in the authored policy.
--
--   @probe_oak@ authors @wood@ as ungated with an empty sprout yield,
--   which is what the three shipped species author. @probe_elm@ authors
--   neither, which is the absent-schema default every other harvest
--   block gets.
--
--   Annual rather than perennial so the dead-window boundary is the
--   exact 360-day constant rather than a value rolled from the
--   placement mixer.
catalog ∷ FloraCatalog
catalog =
    insertSpecies elmId
        (tree "probe_elm") { fsHarvest = Just baseHarvest }
    $ insertSpecies oakId
        (tree "probe_oak")
            { fsHarvest = Just baseHarvest
                { fhUngatedTags = ["wood"]
                , fhPhaseYields = HM.fromList [(PhaseSprout, [])] } }
      emptyFloraCatalog
  where
    tree name = (newFloraSpecies name plantTex)
        { fsLifecycle = Annual
        , fsPhases = HM.fromList
            [ (PhaseSprout,  LifePhase PhaseSprout  0   plantTex)
            , (PhaseMatured, LifePhase PhaseMatured 60  plantTex)
            , (PhaseDead,    LifePhase PhaseDead    360 plantTex)
            ] }
    baseHarvest = FloraHarvest
        { fhTags = ["wood"], fhUngatedTags = []
        -- An exact count, so a spawned yield is an exact number rather
        -- than a range the assertions would have to loosen for.
        , fhYield = [("probe_log", 2, 2)], fhPhaseYields = HM.empty
        , fhRegrowth = 345600, fhHarvestedTexture = TextureHandle 0 }

-- | The yield item, really registered: 'spawnYields' SKIPS a name the
--   item registry does not resolve, so an unregistered one would make
--   every harvest report zero items and the sprout assertion vacuous.
fixtureItems ∷ ItemManager
fixtureItems = ItemManager $ HM.fromList
    [ ("probe_log", ItemDef
        { idName = "probe_log", idDisplayName = "Probe Log"
        , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
        , idWeight = 1, idWeightSpec = Nothing, idBulk = 1
        , idStorage = Nothing, idKind = "misc", idCategory = "Misc"
        , idMake = "", idMaterial = "", idQualitySpec = Nothing
        , idQualityTiers = [], idContainer = Nothing
        , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
        , idArmor = Nothing, idUnequippable = False, idBuffs = []
        , idInsulation = 0, idSourcePath = "test-fixture" }) ]

-- | One plant of a species in a named growth state, on its own tile.
--
--   Health is 1, so age advances one day per day and the placement age
--   IS the age at absolute day 0 — the day the fixture page sits on.
data Plant = Plant
    { plLabel    ∷ String
    , plInstance ∷ FloraInstance
    , plAdmitted ∷ Bool
      -- ^ What the authored policy says about a @wood@ harvest of this
      --   plant, right now. The expectation every surface is held to.
    }

plants ∷ [Plant]
plants =
    [ Plant "oak sprout"  (at 0 oakId 4 0.0)   True
    , Plant "oak matured" (at 1 oakId 5 100.0) True
    , Plant "oak dead"    (at 2 oakId 6 380.0) True
    , Plant "elm sprout"  (at 3 elmId 7 0.0)   False
    , Plant "elm matured" (at 4 elmId 8 100.0) True
    , Plant "elm dead"    (at 5 elmId 9 380.0) False
    ]
  where
    at ordinal species lx age = FloraInstance
        { fiSpecies = species
        , fiTileX = fromIntegral lx, fiTileY = 4
        , fiOffU = 0, fiOffV = 0, fiZ = zSlice
        , fiAge = age, fiHealth = 1, fiVariant = 0, fiBaseWidth = 16
        , fiInstanceId =
            generatedFloraInstanceId "chop_tag_policy" 0 0 "probe" ordinal
        , fiChopDesignated = False
        }

plantId ∷ Plant → FloraInstanceId
plantId = fiInstanceId . plInstance

-- | The plant's global tile. Chunk (0,0), so local and global coincide.
plantTile ∷ Plant → (Int, Int)
plantTile p = ( fromIntegral (fiTileX (plInstance p))
              , fromIntegral (fiTileY (plInstance p)) )

fixtureChunk ∷ ChunkCoord
fixtureChunk = ChunkCoord 0 0

tilesWith ∷ [FloraInstance] → WorldTileData
tilesWith insts =
    let area = chunkSize * chunkSize
        col = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.replicate (zSlice + 1) 1
            , ctSlopes = VU.replicate (zSlice + 1) 0
            , ctVeg    = VU.replicate (zSlice + 1) 0 }
        lc = LoadedChunk
            { lcCoord = fixtureChunk
            , lcTiles = V.replicate area col
            , lcSurfaceMap = VU.replicate area zSlice
            , lcTerrainSurfaceMap = VU.replicate area zSlice
            , lcFluidMap = V.replicate area Nothing
            , lcIceMap = emptyIceMap
            , lcFlora = FloraChunkData insts
            , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
            , lcMagma = Nothing, lcStructures = emptyChunkStructures }
    in WorldTileData { wtdChunks = HM.fromList [(fixtureChunk, lc)]
                     , wtdMaxChunks = 200 }

-- | A fresh page holding exactly these plants, made the active world.
--   The date is left at the epoch, so the growth clock reads
--   @(dayOfYear 0, absoluteDay 0)@ and each plant's placement age is
--   its live age.
resetPageWith ∷ EngineEnv → [FloraInstance] → IO WorldState
resetPageWith env insts = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws) (tilesWith insts)
    writeIORef (floraCatalogRef env) catalog
    writeIORef (itemManagerRef env) fixtureItems
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    pure ws

-- | The oracle's own view of a page, with the same epoch clock
--   'resetPageWith' leaves the page on — @fhvAbsDay = 0@ against a
--   360-day year, which is the day-of-year the growth clock reports.
viewOf ∷ WorldTileData → FloraHitView
viewOf tiles =
    let cam = defaultCamera { camPosition = (camX, camY), camZoom = zoom
                            , camFacing = FaceNorth, camZSlice = zSlice }
        (camX, camY) = (8.0, 8.0)
    in FloraHitView
        { fhvFacing = FaceNorth, fhvZSlice = zSlice
        , fhvPlaceCamX = camX, fhvPlaceCamY = camY
        , fhvZoom = zoom, fhvCamX = camX, fhvCamY = camY
        , fhvFbW = fbW, fhvFbH = fbH, fhvWinW = winW, fhvWinH = winH
        , fhvWorldSize = worldSize, fhvEffDepth = effDepth
        , fhvViewBounds = computeViewBounds cam fbW fbH effDepth
        , fhvTiles = tiles, fhvCatalog = catalog
        , fhvHarvests = HM.empty, fhvDesignated = HM.empty
        , fhvTexSizes = texSizes
        , fhvDaysPerYear = 360, fhvAbsDay = 0
        , fhvFrontWall = noFrontWallLift
        }

-- * The four surfaces, each answering "may `wood` take this plant?"

-- | Screen-space selection: does the drag box offer it?
selectsIt ∷ EngineEnv → Plant → IO Bool
selectsIt _ p = do
    let view = viewOf (tilesWith [plInstance p])
        picks = map (fpInstanceId . fst)
                    (floraSelectCandidates view (SelectChoppable "wood"))
    pure (plantId p `elem` picks)

-- | The world thread's commit: does it accept the designation?
commitsIt ∷ EngineEnv → Plant → IO Bool
commitsIt env p = do
    ws ← resetPageWith env [plInstance p]
    logger ← readIORef (loggerRef env)
    handleWorldDesignateChopInstancesCommand env logger fixturePage
        [plantId p] "wood"
    HM.member (plantId p) <$> readIORef (wsChopDesignationsRef ws)

-- | The tagged finder: does an AI search report it?
findsIt ∷ EngineEnv → LuaBackendState → Plant → IO Bool
findsIt env ls p = do
    _ ← resetPageWith env [plInstance p]
    let (gx, gy) = plantTile p
    reply ← evalDebug ls (T.concat
        [ "local f = world.findHarvestableFlora(", tshow gx, ", ", tshow gy
        , ", 8, 'wood'); return f and f.id or 'nil'" ])
    pure (reply ≢ "nil")

-- | The fell itself: does the harvest verb take it? @nil@ is the
--   refusal; an accepted fell answers with a table, EMPTY when the
--   authored phase yield is.
fellsIt ∷ EngineEnv → LuaBackendState → Plant → IO Bool
fellsIt env ls p = (≢ (-1)) <$> fellCount env ls p

-- | The fell's spawned-item count, or @-1@ for a refusal.
fellCount ∷ EngineEnv → LuaBackendState → Plant → IO Int
fellCount env ls p = do
    _ ← resetPageWith env [plInstance p]
    let (gx, gy) = plantTile p
    reply ← evalDebug ls (T.concat
        [ "local r = world.harvestFloraInstance(", tshow gx, ", ", tshow gy
        , ", ", tshow (idNum (plantId p)), ", 'wood'); "
        , "return r and #r or -1" ])
    -- The console replies with a bare number. An unreadable reply is
    -- reported as a distinct sentinel rather than defaulted, so a
    -- console-shape change cannot masquerade as a refusal.
    pure (fromMaybe (-99) (readMaybe (T.unpack reply)))

spec ∷ Spec
spec = describe "Chop tag policy" $ beforeAll setup $ do

  describe "the four surfaces agree (requirement 3)" $
    forM_ plants $ \p →
      it ("gives one answer for a " ⧺ plLabel p) $ \(env, ls) → do
        selects ← selectsIt env p
        commits ← commitsIt env p
        finds   ← findsIt env ls p
        fells   ← fellsIt env ls p
        -- Reported as one labelled tuple rather than four assertions:
        -- a disagreement then names WHICH surface drifted.
        ( "selects", "commits", "finds", "fells"
          , selects, commits, finds, fells )
          `shouldBe`
          ( "selects", "commits", "finds", "fells"
          , plAdmitted p, plAdmitted p, plAdmitted p, plAdmitted p )

  describe "what a tagged fell returns and leaves behind \
           \(requirements 4 and 5)" $ do

    it "an authored EMPTY sprout yield is an ACCEPTED fell that spawns \
       \nothing — an empty table, never the nil that means refused" $
        \(env, ls) → fellCount env ls (plantAt "oak sprout") `shouldReturn` 0

    it "matured and dead inherit the species roll, so a standing-dead \
       \tree still pays its logs" $ \(env, ls) → do
        fellCount env ls (plantAt "oak matured") `shouldReturn` 2
        fellCount env ls (plantAt "oak dead")    `shouldReturn` 2

    it "a fell whose yield is empty still starts the instance's regrowth \
       \timer, so the sprout cannot simply be felled again" $
        \(env, ls) → do
        let p = plantAt "oak sprout"
        ws ← resetPageWith env [plInstance p]
        let (gx, gy) = plantTile p
        _ ← evalDebug ls (T.concat
            [ "return tostring(world.harvestFloraInstance(", tshow gx, ", "
            , tshow gy, ", ", tshow (idNum (plantId p)), ", 'wood') ~= nil)" ])
        HM.lookup (plantId p) <$> readIORef (wsFloraHarvestsRef ws)
            `shouldReturn` Just 345600
        -- ... and the second fell is refused while that timer stands,
        -- which is what closes the unbounded wood source #2212 names.
        evalDebug ls (T.concat
            [ "return tostring(world.harvestFloraInstance(", tshow gx, ", "
            , tshow gy, ", ", tshow (idNum (plantId p)), ", 'wood') ~= nil)" ])
            `shouldReturn` "false"

    it "a REFUSED fell leaves no timer at all, so a non-declaring \
       \species' sprout is untouched rather than quietly consumed" $
        \(env, ls) → do
        let p = plantAt "elm sprout"
        ws ← resetPageWith env [plInstance p]
        let (gx, gy) = plantTile p
        _ ← evalDebug ls (T.concat
            [ "return tostring(world.harvestFloraInstance(", tshow gx, ", "
            , tshow gy, ", ", tshow (idNum (plantId p)), ", 'wood') ~= nil)" ])
        HM.null <$> readIORef (wsFloraHarvestsRef ws) `shouldReturn` True

    it "a BARE forage call is unaffected: it refuses every one of these \
       \wood plants, in every state, because none yields food" $
        \(env, ls) → forM_ plants $ \p → do
            _ ← resetPageWith env [plInstance p]
            let (gx, gy) = plantTile p
            found ← evalDebug ls (T.concat
                [ "local f = world.findHarvestableFlora(", tshow gx, ", "
                , tshow gy, ", 8); return f and f.id or 'nil'" ])
            (plLabel p, found) `shouldBe` (plLabel p, "nil")

  describe "the fixture is not vacuous" $
    it "the two species differ ONLY in the authored policy, and the \
       \three ages really are three different growth states" $
        \(env, _) → do
        -- If probe_elm's sprout were selectable the parity case would
        -- pass with the window gate removed entirely, so the whole
        -- suite turns on these two sets differing.
        let view = viewOf (tilesWith (map plInstance plants))
            picks = sort (map (fpInstanceId . fst)
                        (floraSelectCandidates view (SelectChoppable "wood")))
        picks `shouldBe` sort (map plantId (filter plAdmitted plants))
        length (filter plAdmitted plants) `shouldBe` 4
        _ ← resetPageWith env (map plInstance plants)
        pure ()

  where
    setup = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        pure (env, ls)

-- | The named fixture plant. A typo is a test-authoring error, not a
--   silently skipped case.
plantAt ∷ String → Plant
plantAt label = case [p | p ← plants, plLabel p ≡ label] of
    [p] → p
    _   → error ("no fixture plant labelled " ⧺ label)

idNum ∷ FloraInstanceId → Int64
idNum = floraInstanceIdToLua

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src
