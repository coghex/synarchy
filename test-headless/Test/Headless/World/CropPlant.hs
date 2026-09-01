{-# LANGUAGE TypeApplications #-}
-- | The crop Plant designation's two halves (issue #1858, DTV-3): the
--   FLAT top-surface marker #1857 established, and the continuous
--   tilled-soil validity behind it.
--
--   Before this slice the soil was checked once, at admission, and
--   never again: the record persisted, the marker kept drawing, and a
--   worker walked to ground it could no longer plant. D-14 makes tilled
--   soil a CONTINUOUS requirement and is explicit that hiding an
--   invalid designation while leaving an invisible job active is not
--   the fix — so validity is world-owned ('World.Plant.Validate'), the
--   world REMOVING the record is the whole cancellation protocol, and
--   the renderer only suppresses records it cannot resolve.
--
--   Three groups, and the split between them is the contract:
--
--     * @crop plant surface@ — the marker really reaches
--       'World.Render.TileQuads.worldFlatCursorToQuad', proved through
--       the production 'renderWorldCursorQuads' pass rather than by
--       calling the helper directly, with Mine in the SAME pass as the
--       control that the isometric map did not move.
--     * @crop plant invalidation@ — the tri-state resolution (pure),
--       the live edit that removes a record, the unloaded chunk that
--       does not, the shared predicate, admission left alone, and the
--       production Lua release path.
--     * The save/load half ('saveSpec'), which needs a real world, a
--       real save file and the real load transaction, so it gets its
--       own engine.
--
--   Run them: @cabal test synarchy-test-headless
--   --test-options='--match "crop plant surface"'@ and
--   @--match "crop plant invalidation"@.
module Test.Headless.World.CropPlant (spec, engineSpec, saveSpec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Picture as JP
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua
import System.Directory (doesFileExist, removePathForcibly)

import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import World.Construct.Types (ConstructTarget(..), StructurePiece(..))
import Engine.Graphics.Camera
    (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex
    (Vertex(..), faceMapId, noFaceMapVertexId)
import Engine.Scene.Types (SortableQuad(..))
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness
    (sendWorldCommand, waitForWorldInit)
import World.Flora.CropPlot (newCropPlot)
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import World.Grid (gridToWorld, tileHeight)
import World.Load.Publish (publishStagedSession)
import World.Load.Stage (stageSession, renderStageError)
import World.Mine.Types (designationFromSlope)
import World.Plant.Types (newPlantDesignation)
import World.Plant.Validate
    ( PlantSoilState(..), plantSoilState, prunePlantDesignations
    , revalidatePlantDesignations )
import World.Render.CursorQuads (renderWorldCursorQuads)
import World.Save.Serialize (loadWorld)
import World.Thread.Command.Cursor
    ( handleWorldAddConstructProgressCommand
    , handleWorldDesignateConstructCommand, handleWorldDesignateMineCommand
    , handleWorldDesignatePlantCommand )
import World.Thread.Command.Edit.Dig (handleWorldDigTileCommand)
import World.Thread.Command.Edit (handleWorldSetVegCommand)
import World.Types
import World.Vegetation (vegMediumGrass, vegTilledSoil)
import Test.Headless.Construct.Fixture (registerFixturePacks)

-- * Fixture geometry
--
--   worldSize 64 → wrap period 64 chunks, one u-alias step = 512 tiles.
--   The fixture chunk sits at u = 32, so it is STORED under a wrapped
--   key: every lookup here has to go through the canonical frame to
--   find it, which is what pins the #1175 half of the contract.

worldSize ∷ Int
worldSize = 64

zSlice ∷ Int
zSlice = 10

-- | The single fixture chunk, as STORED: it is generated at u = 17 and
--   the loader wraps u before inserting, so every lookup here has to go
--   through the canonical frame to find it.
storedChunk ∷ ChunkCoord
storedChunk = ChunkCoord (-15) 17

-- | Two tiles in that chunk, named canonically (the frame every
--   designation key is stored in) and, for 'plantTile', also by the raw
--   u-alias a pre-#1175 save could still be holding.
plantTile, otherTile ∷ (Int, Int)
plantTile = canonicalTile worldSize (17 * chunkSize + 4) ((-15) * chunkSize + 4)
otherTile = canonicalTile worldSize (17 * chunkSize + 6) ((-15) * chunkSize + 6)

plantTileRaw ∷ (Int, Int)
plantTileRaw = (17 * chunkSize + 4, (-15) * chunkSize + 4)

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "crop_plant_probe"

plantMarkerTexture, mineMarkerTexture, isoFaceMapTexture ∷ TextureHandle
plantMarkerTexture = TextureHandle 43
mineMarkerTexture  = TextureHandle 44
isoFaceMapTexture  = TextureHandle 29

probeCrop ∷ FloraId
probeCrop = FloraId 1

-- * Chunk fixtures

-- | A flat chunk whose whole surface carries @veg@, with named tiles
--   overridden. Only the SURFACE cell matters to every consumer here,
--   but the whole column is filled so an out-of-range read cannot pass
--   by accident.
fixtureChunk ∷ Word8 → [((Int, Int), Word8)] → LoadedChunk
fixtureChunk veg overrides =
    let area = chunkSize * chunkSize
        col v = ColumnTiles
                  { ctStartZ = 0
                  , ctMats   = VU.replicate 20 1
                  , ctSlopes = VU.replicate 20 0
                  , ctVeg    = VU.replicate 20 v
                  }
        localIdx (gx, gy) =
            let (_, (lx, ly), _) = canonicalTileFrame worldSize gx gy
            in columnIndex lx ly
        tiles = V.replicate area (col veg)
                V.// [ (localIdx tile, col v) | (tile, v) ← overrides ]
    in LoadedChunk
        { lcCoord = storedChunk
        , lcTiles = tiles
        , lcSurfaceMap = VU.replicate area zSlice
        , lcTerrainSurfaceMap = VU.replicate area zSlice
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

tilesWith ∷ Word8 → [((Int, Int), Word8)] → WorldTileData
tilesWith veg overrides = WorldTileData
    { wtdChunks = HM.singleton storedChunk (fixtureChunk veg overrides)
    , wtdMaxChunks = 200 }

-- | The same page with NO chunk resident at all — the unknown case.
noTiles ∷ WorldTileData
noTiles = WorldTileData { wtdChunks = HM.empty, wtdMaxChunks = 200 }

-- | A registered @row_crop@, so admission's own species check passes
--   and an example that is about the SOIL is really about the soil.
cropCatalog ∷ FloraCatalog
cropCatalog = emptyFloraCatalog
    { fcSpecies  = HM.fromList
        [(1, newFloraSpecies "probe_crop" (TextureHandle 0))]
    , fcWorldGen = HM.fromList [(1, cropWorldGen "row_crop")]
    , fcNextId   = 2 }

-- | The same species registered under a category
--   'isPlantableCropCategory' refuses — the admission control.
treeCatalog ∷ FloraCatalog
treeCatalog = cropCatalog
    { fcWorldGen = HM.fromList [(1, cropWorldGen "tree")] }

cropWorldGen ∷ Text → FloraWorldGen
cropWorldGen category = FloraWorldGen
    { fwCategory = category
    , fwMinTemp = -100, fwMaxTemp = 100, fwIdealTemp = 15
    , fwMinPrecip = 0, fwMaxPrecip = 10000, fwIdealPrecip = 500
    , fwMinAlt = -10000, fwMaxAlt = 10000, fwIdealAlt = 0
    , fwMinHumidity = 0, fwMaxHumidity = 1, fwIdealHumidity = 0.5
    , fwMaxSlope = 255, fwDensity = 0, fwSoils = [], fwFootprint = 1
    }

-- * Pure + Lua coverage

spec ∷ Spec
spec = do

  describe "crop plant surface" $ do

    it "ships a 96x64 RGBA marker whose alpha is exactly the flat \
       \top-surface diamond" $ do
      -- The alpha IS the shape: #1857's path applies no face mask, so
      -- anything the authored alpha covers is what the player sees. A
      -- marker inheriting the pre-#1858 three-face bitmap would hang
      -- colour off the tile, which is why this is checked rather than
      -- assumed.
      mask   ← readRGBA "assets/textures/facemap/vegface.png"
      marker ← readRGBA plantMarkerPath
      till   ← readRGBA "assets/textures/ui/hud/utility/till_designate.png"
      map imageSize [mask, marker, till] `shouldBe` replicate 3 (96, 64)
      alphaShapeMismatches mask marker `shouldBe` []
      -- Byte-for-byte Till's alpha, not merely the same silhouette:
      -- the two markers are the same surface, so they must sit at the
      -- same translucency as well as the same outline.
      alphaMismatches till marker `shouldBe` []

    it "is one flat light-green colour, distinct from Till's orange" $ do
      marker ← readRGBA plantMarkerPath
      till   ← readRGBA "assets/textures/ui/hud/utility/till_designate.png"
      visiblePixels marker `shouldBe` Set.singleton plantMarkerColour
      visiblePixels till `shouldBe` Set.singleton tillMarkerColour
      -- Stated structurally as well as exactly, so a later re-authoring
      -- has to keep the READING the requirement names and not just
      -- change a number this file also changed.
      let JP.PixelRGBA8 red green blue _ = plantMarkerColour
      green `shouldSatisfy` (\g → g > red ∧ g > blue)
      minimum [red, green, blue] `shouldSatisfy` (> 64)

    it "carries that colour under its transparent pixels too" $ do
      -- Till's own convention, and load-bearing: texture filtering is a
      -- runtime nearest/linear toggle, and a zeroed RGB plane under the
      -- transparent pixels dark-fringes the diamond edge on linear.
      marker ← readRGBA plantMarkerPath
      colourPlane marker `shouldBe` Set.singleton (rgbOf plantMarkerColour)

  describe "crop plant invalidation" $ do

    describe "the tri-state resolution" $ do

      it "reads resident tilled soil as valid" $
        plantSoilState worldSize (tilesWith vegTilledSoil []) plantTile
          `shouldBe` PlantSoilTilled

      it "reads resident non-tilled ground as LOST, not unknown" $
        plantSoilState worldSize (tilesWith vegMediumGrass []) plantTile
          `shouldBe` PlantSoilLost

      it "reads a non-resident chunk as UNKNOWN, not lost" $
        -- The whole reason validity is three-valued: an evicted chunk
        -- says nothing about its soil, and treating silence as proof
        -- would delete a player's field every time they panned away.
        plantSoilState worldSize noTiles plantTile
          `shouldBe` PlantSoilUnknown

      it "resolves a raw u-alias key to the chunk that STORES it" $ do
        -- #1175: the chunk is stored wrapped, so a designation key in
        -- the raw frame (a pre-#1175 save) must not read as unknown
        -- forever — which would make it permanently unremovable.
        plantSoilState worldSize (tilesWith vegTilledSoil []) plantTileRaw
          `shouldBe` PlantSoilTilled
        plantSoilState worldSize (tilesWith vegMediumGrass []) plantTileRaw
          `shouldBe` PlantSoilLost

      it "reads an out-of-column-range surface as LOST" $ do
        -- A malformed column must be refused, never inferred tilled.
        let truncated = tilesWith vegTilledSoil []
            shorten lc =
                let idx = localIndex plantTile
                    col = lcTiles lc V.! idx
                in lc { lcTiles = lcTiles lc V.// [(idx, col
                          { ctVeg = VU.take 2 (ctVeg col) })] }
        plantSoilState worldSize
            (truncated { wtdChunks =
                HM.adjust shorten storedChunk (wtdChunks truncated) })
            plantTile
          `shouldBe` PlantSoilLost

    describe "the sweep keeps exactly what it cannot disprove" $ do

      it "drops the lost record and keeps the tilled one" $ do
        let designations = HM.fromList
                [ (plantTile, newPlantDesignation zSlice probeCrop)
                , (otherTile, newPlantDesignation zSlice probeCrop) ]
            tiles = tilesWith vegTilledSoil [(plantTile, vegMediumGrass)]
            (kept, gone) = prunePlantDesignations worldSize tiles designations
        gone `shouldBe` [plantTile]
        sort (HM.keys kept) `shouldBe` [otherTile]

      it "keeps every record when nothing is resident" $ do
        let designations = HM.fromList
                [ (plantTile, newPlantDesignation zSlice probeCrop)
                , (otherTile, newPlantDesignation zSlice probeCrop) ]
            (kept, gone) = prunePlantDesignations worldSize noTiles designations
        gone `shouldBe` []
        sort (HM.keys kept) `shouldBe` sort [plantTile, otherTile]

      it "preserves the surviving records' contents, not just their keys" $ do
        -- Invalidation is a REMOVAL, never a rewrite: the stored z and
        -- the chosen crop of a record it keeps must be untouched.
        let record = newPlantDesignation 7 (FloraId 3)
            designations = HM.fromList [(otherTile, record)]
            (kept, _) = prunePlantDesignations worldSize
                            (tilesWith vegTilledSoil []) designations
        HM.lookup otherTile kept `shouldBe` Just record

    describe "one predicate serves admission and invalidation" $
      it "the vegetation id that admits is exactly the one that survives" $ do
        -- The engine example below drives the real admission handler
        -- over the same two ids; this pins that the INVALIDATION side
        -- reads them identically, so changing 'isTilledSoil' moves both
        -- and neither can drift to a raw comparison against id 77.
        let soilStates veg = plantSoilState worldSize (tilesWith veg []) plantTile
        soilStates vegTilledSoil `shouldBe` PlantSoilTilled
        soilStates vegMediumGrass `shouldBe` PlantSoilLost

    describe "the farm AI's release path" $ do

      it "clears the claim, job, phase and progress on the next tick \
         \once the designation is gone" $
        runsOk $ lns
            [ plantPrelude
            , "step()"
            , "assert(S.plantJob, 'the worker must have claimed the tile')"
            , "S.plantPhase = 'planting'; S.plantProgress = 0.4"
            , "assert(next(unitAi.plant.claims), 'the claim must be held')"
            -- The world removes the record: exactly what
            -- World.Plant.Validate does, seen from Lua.
            , "DESIGNATIONS = {}"
            , "step()"
            , "assert(S.plantJob == nil, 'plantJob must be released')"
            , "assert(S.plantPhase == nil, 'plantPhase must be cleared')"
            , "assert(S.plantProgress == nil, 'plantProgress must be cleared')"
            , "assert(next(unitAi.plant.claims) == nil,"
            , "  'the tile claim must be released')"
            , "assert(CALLS.clearAnim > 0, 'the work anim must be dropped')"
            ]

      it "scores itself out of contention rather than leaving an \
         \invisible job running" $
        runsOk $ lns
            [ plantPrelude
            , "step()"
            , "S.plantPhase = 'planting'; S.plantProgress = 0.4"
            , "DESIGNATIONS = {}"
            , "local u = unitAi.plant.utility(1, S, PARAMS)"
            , "assert(u == -math.huge,"
            , "  'a released plant action must not stay selectable')"
            -- ...and executing anyway must not resurrect the job from
            -- the stale candidate, which is what "no invisible job" means.
            , "unitAi.plant.execute(1, S, PARAMS)"
            , "assert(S.plantJob == nil, 'execute must not re-claim')"
            , "assert(CALLS.plantRow == 0, 'nothing may be planted')"
            ]

      it "keeps the job while the designation is still there" $
        -- The control: the release above is caused by the record
        -- disappearing, not by the tick itself.
        runsOk $ lns
            [ plantPrelude
            , "step()"
            , "S.plantPhase = 'planting'; S.plantProgress = 0.4"
            , "step()"
            , "assert(S.plantJob, 'an intact designation keeps the job')"
            , "assert(next(unitAi.plant.claims), 'and keeps the claim')"
            ]

-- | A structure piece, so 'applyConstructSlopeToChunk''s @isStructure@
--   guard passes and the progress write really reaches the tile.
wirePiece ∷ ConstructTarget
wirePiece = CtStructure (StructurePiece "wire" "wire" Nothing)

plantMarkerPath ∷ FilePath
plantMarkerPath = "assets/textures/ui/hud/utility/plant_designate.png"

-- | The authored marker colours: light green for crop planting, and
--   #1857's orange for Till beside it.
plantMarkerColour, tillMarkerColour ∷ JP.PixelRGBA8
plantMarkerColour = JP.PixelRGBA8 144 220 104 88
tillMarkerColour  = JP.PixelRGBA8 232 126 38 88

readRGBA ∷ FilePath → IO (JP.Image JP.PixelRGBA8)
readRGBA path = do
    result ← JP.readImage path
    case result of
        Left err → fail ("could not decode " ⧺ path ⧺ ": " ⧺ err)
        Right image → pure (JP.convertRGBA8 image)

imageSize ∷ JP.Image pixel → (Int, Int)
imageSize image = (JP.imageWidth image, JP.imageHeight image)

-- | Pixels where one image is opaque-ish and the other is not.
alphaShapeMismatches ∷ JP.Image JP.PixelRGBA8 → JP.Image JP.PixelRGBA8
                     → [(Int, Int)]
alphaShapeMismatches = comparePixels $ \a b →
    (alphaOf a > 0) ≢ (alphaOf b > 0)

-- | Pixels where two images' alpha bytes differ at all.
alphaMismatches ∷ JP.Image JP.PixelRGBA8 → JP.Image JP.PixelRGBA8
                → [(Int, Int)]
alphaMismatches = comparePixels $ \a b → alphaOf a ≢ alphaOf b

comparePixels ∷ (JP.PixelRGBA8 → JP.PixelRGBA8 → Bool)
              → JP.Image JP.PixelRGBA8 → JP.Image JP.PixelRGBA8
              → [(Int, Int)]
comparePixels differs left right =
    [ (x, y)
    | y ← [0 .. JP.imageHeight left - 1]
    , x ← [0 .. JP.imageWidth left - 1]
    , differs (JP.pixelAt left x y) (JP.pixelAt right x y) ]

alphaOf ∷ JP.PixelRGBA8 → Word8
alphaOf (JP.PixelRGBA8 _ _ _ alpha) = alpha

rgbOf ∷ JP.PixelRGBA8 → (Word8, Word8, Word8)
rgbOf (JP.PixelRGBA8 red green blue _) = (red, green, blue)

visiblePixels ∷ JP.Image JP.PixelRGBA8 → Set.Set JP.PixelRGBA8
visiblePixels image = Set.fromList
    [ pixel
    | y ← [0 .. JP.imageHeight image - 1]
    , x ← [0 .. JP.imageWidth image - 1]
    , let pixel = JP.pixelAt image x y
    , alphaOf pixel > 0 ]

-- | Every RGB triple in the image, transparent pixels included.
colourPlane ∷ JP.Image JP.PixelRGBA8 → Set.Set (Word8, Word8, Word8)
colourPlane image = Set.fromList
    [ rgbOf (JP.pixelAt image x y)
    | y ← [0 .. JP.imageHeight image - 1]
    , x ← [0 .. JP.imageWidth image - 1] ]

localIndex ∷ (Int, Int) → Int
localIndex (gx, gy) =
    let (_, (lx, ly), _) = canonicalTileFrame worldSize gx gy
    in columnIndex lx ly

-- * The production Lua module, driven in a bare interpreter
--
--   Same standalone-VM pattern as "Test.Headless.Lua.UnitAiHarvest":
--   one self-contained chunk per example, asserting inside Lua, with a
--   non-OK status surfaced as an hspec failure. The module under test
--   is the SHIPPED @scripts/unit_ai_farm.lua@ — the release path is the
--   one the game runs, not a restatement of it.

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

-- | One designated tile at (5, 5), one worker standing next to it, and
--   @step()@ — one arbitration pass, scoring @plant_designation@ and
--   executing it, exactly as @scripts/unit_ai.lua@ does for an idle
--   unit. @DESIGNATIONS@ is the world's designation map as Lua sees it
--   through the real @plant.*@ verbs; emptying it is precisely what
--   'World.Plant.Validate' does to an invalidated record.
plantPrelude ∷ Text
plantPrelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "NOW = 0"
    , "POS = { gridX = 5, gridY = 4 }"
    , "ACTIVITY = 'idle'"
    , "CALLS = { moveTo = 0, plantRow = 0, plantCrop = 0, cancel = 0,"
    , "          clearAnim = 0 }"
    , "DESIGNATIONS = { ['5,5'] = { z = 10, crop = 'probe_crop',"
    , "                             category = 'row_crop' } }"
    , "local function key(x, y) return string.format('%d,%d', x, y) end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logInfo = function() end }"
    , "unit = {"
    , "  getInfo = function() return POS end,"
    , "  exists = function() return true end,"
    , "  getStat = function() return 1.0 end,"
    , "  getSkill = function() return 50.0 end,"
    , "  setSkill = function() end,"
    , "  addXP = function() end,"
    , "  moveTo = function() CALLS.moveTo = CALLS.moveTo + 1 end,"
    , "  stop = function() ACTIVITY = 'idle' end,"
    , "  getActivity = function() return ACTIVITY end,"
    , "  setAnimOverride = function() end,"
    , "  clearAnimOverride = function()"
    , "    CALLS.clearAnim = CALLS.clearAnim + 1 end }"
    , "plant = {"
    , "  getDesignationAt = function(_, x, y) return DESIGNATIONS[key(x, y)] end,"
    , "  nearestDesignation = function(_, ux, uy)"
    , "    for k, _ in pairs(DESIGNATIONS) do"
    , "      local sx, sy = k:match('(-?%d+),(-?%d+)')"
    , "      local gx, gy = tonumber(sx), tonumber(sy)"
    , "      return gx, gy, math.sqrt((gx - ux) ^ 2 + (gy - uy) ^ 2)"
    , "    end"
    , "    return nil end,"
    , "  cancelDesignation = function(x, y)"
    , "    CALLS.cancel = CALLS.cancel + 1"
    , "    DESIGNATIONS[key(x, y)] = nil end }"
    , "world = {"
    , "  getActiveWorldId = function() return 1 end,"
    , "  plantCropAt = function() CALLS.plantCrop = CALLS.plantCrop + 1 end,"
    , "  plantRowCropAt = function() CALLS.plantRow = CALLS.plantRow + 1 end }"
    , "item = { listDefs = function() return {} end }"
    , "require('scripts.unit_ai_farm')"
    , "require('scripts.movement_speed').comfort = function() return 1.0 end"
    , "unitAi = package.loaded['scripts.unit_ai']"
    , "PARAMS = { plant_scan_range = 24.0, plant_base_utility = 2.0,"
    , "           plant_lock_utility = 6.0, plant_claim_timeout = 30.0,"
    , "           plant_rate = 0.5, plant_equip_seconds = 1.0,"
    , "           plant_equip_anim = 'equip', plant_work_anim = 'work',"
    , "           plant_xp_per_plant = 1.0 }"
    , "S = {}"
    -- scripts/unit_ai.lua's own re-execute rule: score, take the winner,
    -- and execute on a switch or when the unit is idle.
    , "function step()"
    , "  NOW = NOW + 0.5"
    , "  local u = unitAi.plant.utility(1, S, PARAMS)"
    , "  if u <= -math.huge then return u end"
    , "  local switching = S.currentAction ~= 'plant_designation'"
    , "  S.currentAction = 'plant_designation'"
    , "  if switching or ACTIVITY == 'idle' then"
    , "    unitAi.plant.execute(1, S, PARAMS)"
    , "  end"
    , "  return u"
    , "end"
    ]

-- * Engine coverage on a synthetic page
--
--   A bare headless engine with NO world worker (the fixture page's
--   hand-built chunk must not be handed to a real chunk loader), the
--   way "Test.Headless.World.DesignationSeam" does it.

engineSpec ∷ Spec
engineSpec = beforeAll setup $ do

  describe "crop plant surface" $ do

    it "renders a committed crop marker through the FLAT top-surface \
       \path" $ \env → do
      -- The neutral face-map sentinel is the observable difference
      -- between #1857's helper and the three-face one, and this reads
      -- it out of the production render pass — so routing the marker
      -- back through 'worldCursorToQuad' fails here even though the
      -- helper itself is unchanged.
      ws ← resetPage env vegTilledSoil
      writeIORef (wsPlantDesignationsRef ws) $
          HM.singleton plantTile (newPlantDesignation zSlice probeCrop)
      quads ← markerQuads env ws
      case V.toList quads of
        [quad] → do
          sqTexture quad `shouldBe` plantMarkerTexture
          quadFaceMaps quad `shouldBe` replicate 4 noFaceMapVertexId
        other → expectationFailure
            ("expected one committed crop marker, got " ⧺ show (length other))

    it "leaves Mine on the isometric face map in the same pass" $ \env → do
      -- The control that keeps the example above honest: both markers
      -- render together, from one call, and only one of them moved.
      ws ← resetPage env vegTilledSoil
      writeIORef (wsPlantDesignationsRef ws) $
          HM.singleton plantTile (newPlantDesignation zSlice probeCrop)
      writeIORef (wsMineDesignationsRef ws) $
          HM.singleton otherTile (designationFromSlope zSlice 0)
      quads ← markerQuads env ws
      let byTexture t = [ q | q ← V.toList quads, sqTexture q ≡ t ]
      map quadFaceMaps (byTexture plantMarkerTexture)
          `shouldBe` [replicate 4 noFaceMapVertexId]
      map quadFaceMaps (byTexture mineMarkerTexture)
          `shouldBe` [replicate 4 (fromIntegral (toInt isoFaceMapTexture))]

  describe "crop plant invalidation" $ do

    it "removes the designation when the soil stops being tilled" $
        \env → do
      -- Through the REAL setVeg handler the till AI itself calls, so
      -- the trigger under test is the production write path.
      ws ← resetPage env vegTilledSoil
      logger ← readIORef (loggerRef env)
      writeIORef (floraCatalogRef env) cropCatalog
      handleWorldDesignatePlantCommand env logger fixturePage
          (fst plantTile) (snd plantTile) "probe_crop"
      HM.keys <$> readIORef (wsPlantDesignationsRef ws)
          `shouldReturn` [plantTile]
      handleWorldSetVegCommand (toWorldSimCapability env) logger fixturePage
          (fst plantTile) (snd plantTile) zSlice vegMediumGrass
      HM.keys <$> readIORef (wsPlantDesignationsRef ws) `shouldReturn` []

    it "leaves an untouched neighbour's designation alone" $ \env → do
      -- The removal is per-tile, not a sweep that empties the map
      -- whenever anything changes.
      ws ← resetPage env vegTilledSoil
      logger ← readIORef (loggerRef env)
      writeIORef (floraCatalogRef env) cropCatalog
      forM_ [plantTile, otherTile] $ \(gx, gy) →
          handleWorldDesignatePlantCommand env logger fixturePage gx gy
              "probe_crop"
      handleWorldSetVegCommand (toWorldSimCapability env) logger fixturePage
          (fst plantTile) (snd plantTile) zSlice vegMediumGrass
      sort . HM.keys <$> readIORef (wsPlantDesignationsRef ws)
          `shouldReturn` [otherTile]

    it "removes it when a PARTIAL dig sheds the tile's vegetation" $
        \env → do
      -- Round 1 review: 'applyDigSlopeToChunk' clears the surface
      -- ctVeg the moment one corner drops, and mine admission does not
      -- exclude a tile carrying a plant designation — so the tile stops
      -- being tilled soil here, at a write that is neither a vegetation
      -- edit nor the eventual tile deletion.
      ws ← resetPage env vegTilledSoil
      logger ← readIORef (loggerRef env)
      writeIORef (floraCatalogRef env) cropCatalog
      handleWorldDesignatePlantCommand env logger fixturePage
          (fst plantTile) (snd plantTile) "probe_crop"
      handleWorldDesignateMineCommand env logger fixturePage
          (fst plantTile) (snd plantTile) (fst plantTile) (snd plantTile)
      handleWorldDigTileCommand env (statRNGRef env) (unitQueue env) logger
          fixturePage (fst plantTile) (snd plantTile)
          (fromIntegral (fst plantTile)) (fromIntegral (snd plantTile) - 1)
          0.25 1.0 1.0
      -- Really a PARTIAL dig: the mine designation is still there, so
      -- this is not the already-covered delete-tile path in disguise.
      HM.keys <$> readIORef (wsMineDesignationsRef ws)
          `shouldReturn` [plantTile]
      HM.keys <$> readIORef (wsPlantDesignationsRef ws) `shouldReturn` []

    it "removes it when construction progress sheds the tile's \
       \vegetation" $ \env → do
      -- Round 1 review, the same shedding through the other consumer of
      -- 'applyCornerSlopeToChunk'. ('resetConstructSlope' passes full
      -- corners and never touches ctVeg, so it is deliberately not a
      -- revalidation point.)
      ws ← resetPage env vegTilledSoil
      logger ← readIORef (loggerRef env)
      writeIORef (floraCatalogRef env) cropCatalog
      handleWorldDesignatePlantCommand env logger fixturePage
          (fst plantTile) (snd plantTile) "probe_crop"
      handleWorldDesignateConstructCommand env logger fixturePage
          (fst plantTile) (snd plantTile) (fst plantTile) (snd plantTile)
          wirePiece Nothing
      HM.keys <$> readIORef (wsConstructDesignationsRef ws)
          `shouldReturn` [plantTile]
      handleWorldAddConstructProgressCommand env logger fixturePage
          (fst plantTile) (snd plantTile) 0.5 Nothing
      HM.keys <$> readIORef (wsPlantDesignationsRef ws) `shouldReturn` []

    it "keeps — and does not draw — a designation whose chunk is gone" $
        \env → do
      -- Eviction is not evidence. The record survives the sweep and the
      -- marker simply stops drawing, which is requirement 6's whole
      -- point: no invisible job, and no silent deletion either.
      ws ← resetPage env vegTilledSoil
      logger ← readIORef (loggerRef env)
      writeIORef (wsPlantDesignationsRef ws) $
          HM.singleton plantTile (newPlantDesignation zSlice probeCrop)
      writeIORef (wsTilesRef ws) noTiles
      _ ← revalidatePlantDesignations logger ws
      HM.keys <$> readIORef (wsPlantDesignationsRef ws)
          `shouldReturn` [plantTile]
      V.length <$> markerQuads env ws `shouldReturn` 0

    it "resolves that retained designation when the terrain comes back" $
        \env → do
      ws ← resetPage env vegTilledSoil
      logger ← readIORef (loggerRef env)
      writeIORef (wsPlantDesignationsRef ws) $
          HM.singleton plantTile (newPlantDesignation zSlice probeCrop)
      writeIORef (wsTilesRef ws) noTiles
      _ ← revalidatePlantDesignations logger ws
      -- The chunk publishes again, now carrying different ground.
      writeIORef (wsTilesRef ws) (tilesWith vegMediumGrass [])
      _ ← revalidatePlantDesignations logger ws
      HM.keys <$> readIORef (wsPlantDesignationsRef ws) `shouldReturn` []

    it "keeps it when the terrain comes back still tilled" $ \env → do
      ws ← resetPage env vegTilledSoil
      logger ← readIORef (loggerRef env)
      writeIORef (wsPlantDesignationsRef ws) $
          HM.singleton plantTile (newPlantDesignation zSlice probeCrop)
      writeIORef (wsTilesRef ws) noTiles
      _ ← revalidatePlantDesignations logger ws
      writeIORef (wsTilesRef ws) (tilesWith vegTilledSoil [])
      _ ← revalidatePlantDesignations logger ws
      HM.keys <$> readIORef (wsPlantDesignationsRef ws)
          `shouldReturn` [plantTile]

    describe "admission is otherwise unchanged" $ do

      it "still refuses ground that was never tilled" $ \env → do
        ws ← resetPage env vegMediumGrass
        logger ← readIORef (loggerRef env)
        writeIORef (floraCatalogRef env) cropCatalog
        handleWorldDesignatePlantCommand env logger fixturePage
            (fst plantTile) (snd plantTile) "probe_crop"
        HM.keys <$> readIORef (wsPlantDesignationsRef ws) `shouldReturn` []

      it "still refuses a non-crop category" $ \env → do
        ws ← resetPage env vegTilledSoil
        logger ← readIORef (loggerRef env)
        writeIORef (floraCatalogRef env) treeCatalog
        handleWorldDesignatePlantCommand env logger fixturePage
            (fst plantTile) (snd plantTile) "probe_crop"
        HM.keys <$> readIORef (wsPlantDesignationsRef ws) `shouldReturn` []

      it "still refuses a tile carrying flora" $ \env → do
        ws ← resetPage env vegTilledSoil
        logger ← readIORef (loggerRef env)
        writeIORef (floraCatalogRef env) cropCatalog
        writeIORef (wsTilesRef ws) (tilesWithFloraAt plantTile)
        handleWorldDesignatePlantCommand env logger fixturePage
            (fst plantTile) (snd plantTile) "probe_crop"
        HM.keys <$> readIORef (wsPlantDesignationsRef ws) `shouldReturn` []

      it "still refuses a tile carrying a crop plot" $ \env → do
        ws ← resetPage env vegTilledSoil
        logger ← readIORef (loggerRef env)
        writeIORef (floraCatalogRef env) cropCatalog
        writeIORef (wsCropPlotsRef ws) $
            HM.singleton plantTile (newCropPlot probeCrop 0 1.0)
        handleWorldDesignatePlantCommand env logger fixturePage
            (fst plantTile) (snd plantTile) "probe_crop"
        HM.keys <$> readIORef (wsPlantDesignationsRef ws) `shouldReturn` []

      it "accepts exactly the vegetation id invalidation calls tilled" $
          \env → do
        -- Both halves of requirement 3 in one example: the id that
        -- passes admission is the id the sweep keeps, and the id that
        -- fails admission is the id the sweep drops.
        ws ← resetPage env vegTilledSoil
        logger ← readIORef (loggerRef env)
        writeIORef (floraCatalogRef env) cropCatalog
        handleWorldDesignatePlantCommand env logger fixturePage
            (fst plantTile) (snd plantTile) "probe_crop"
        HM.keys <$> readIORef (wsPlantDesignationsRef ws)
            `shouldReturn` [plantTile]
        plantSoilState worldSize (tilesWith vegTilledSoil []) plantTile
            `shouldBe` PlantSoilTilled

  where
    setup = do
        EngineInitResult env ← initializeEngineHeadless
        pure env

-- | Install a fresh synthetic page: one flat chunk with the given
--   ground cover, this world size, and empty designation maps.
resetPage ∷ EngineEnv → Word8 → IO WorldState
resetPage env veg = do
    -- #1844: a structure designation is admitted only against the
    -- registered art/build catalogue, so the one example here that
    -- commits one needs the packs registered the way boot registers
    -- them. Idempotent, and inert for every other example.
    registerFixturePacks env
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws) (tilesWith veg [])
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    pure ws

-- | The same page with one flora instance rooted on the named tile.
tilesWithFloraAt ∷ (Int, Int) → WorldTileData
tilesWithFloraAt tile =
    let (_, (lx, ly), _) = canonicalTileFrame worldSize (fst tile) (snd tile)
        occupied lc = lc { lcFlora = emptyFloraChunkData
            { fcdInstances =
                [ FloraInstance
                    { fiSpecies = probeCrop
                    , fiTileX = fromIntegral lx, fiTileY = fromIntegral ly
                    , fiOffU = 0, fiOffV = 0, fiZ = zSlice
                    , fiAge = 1, fiHealth = 1, fiVariant = 0, fiBaseWidth = 8
                    } ] } }
        base = tilesWith vegTilledSoil []
    in base { wtdChunks = HM.adjust occupied storedChunk (wtdChunks base) }

-- | The quads the live cursor pass draws for this page's committed
--   markers alone: the default tool contributes no preview, and no
--   cursor position is set, so every other quad source is empty by
--   construction.
markerQuads ∷ EngineEnv → WorldState → IO (V.Vector SortableQuad)
markerQuads env ws = do
    let (camX, camY) = gridToWorld FaceSouth (fst plantTile) (snd plantTile)
    writeIORef (cameraRef env) defaultCamera
        { camPosition = (camX, camY + tileHeight * 0.5)
        , camZoom = 40.0, camFacing = FaceSouth, camZSlice = zSlice }
    writeIORef (windowSizeRef env) (8000, 6000)
    writeIORef (framebufferSizeRef env) (800, 600)
    writeIORef (wsTexturesRef ws) defaultWorldTextures
        { wtIsoFaceMap = isoFaceMapTexture }
    writeIORef (wsToolModeRef ws) DefaultTool
    writeIORef (wsCursorRef ws) emptyCursorState
        { plantDesignTexture = Just plantMarkerTexture
        , mineDesignTexture  = Just mineMarkerTexture }
    renderWorldCursorQuads env ws 1.0

quadFaceMaps ∷ SortableQuad → [Float]
quadFaceMaps quad = map faceMapId [sqV0 quad, sqV1 quad, sqV2 quad, sqV3 quad]

-- * The save/load half
--
--   Its own engine: it performs a REAL publish, which replaces the whole
--   session. Reconciliation across a load is not something the sweep can
--   be asked about in isolation — the point is that a designation
--   restored VERBATIM (it keeps its serialized shape and its "Persist
--   exactly" classification) meets terrain the save did not carry, and
--   is reconciled against it when that terrain publishes.

saveSpec ∷ SpecWith EngineEnv
saveSpec = describe "crop plant invalidation" $

  it "reconciles restored designations against the terrain that \
     \publishes, both ways" $ \env →
    let slot = "crop_plant_reconcile"
        pid  = WorldPageId "crop_plant_save"
        cleanup = do
            removePathForcibly ("saves/" <> slot)
            writeIORef (enginePausedRef env) False
    in (`finally` cleanup) $ do
      removePathForcibly ("saves/" <> slot)

      sendWorldCommand env (WorldInit pid 4242 8 3 Nothing)
      ws ← waitForWorldInit env pid 300
      logger ← readIORef (loggerRef env)

      -- Two dry, flora-free tiles in one resident chunk, tilled by two
      -- DIFFERENT means. 'persistTile' goes through the real setVeg
      -- command, so its WeSetVeg edit rides into the save and replays
      -- onto the regenerated chunk. 'liveTile' is tilled by writing the
      -- chunk directly, which is live-only state a save never carries —
      -- so at load its ground is ordinary generated terrain again. That
      -- is exactly "a save whose ground is no longer tilled at load",
      -- built without hand-editing a save file.
      (persistTile, liveTile, surfZ) ← twoDryTiles ws

      sendWorldCommand env
          (WorldSetVeg pid (fst persistTile) (snd persistTile) surfZ
                       vegTilledSoil)
      waitForVeg ws persistTile surfZ vegTilledSoil
      pageSize ← pageWrapWorldSize ws
      atomicModifyIORef' (wsTilesRef ws) $ \td →
          (setSurfaceVeg pageSize liveTile surfZ vegTilledSoil td, ())

      -- Written straight in: admission has its own coverage above, and
      -- what this example needs is two records in the save.
      writeIORef (wsPlantDesignationsRef ws) $ HM.fromList
          [ (persistTile, newPlantDesignation surfZ probeCrop)
          , (liveTile,    newPlantDesignation surfZ probeCrop) ]

      sendWorldCommand env
          (WorldSave pid slot "2026-08-31T00:00:00.000000Z" [] [] Nothing)
      waitForFile ("saves/" <> slot <> "/world.synworld")

      (sd, _, _) ← loadWorld logger slot HS.empty HS.empty ⌦ either
          (\(_, e) → expectationFailure (T.unpack e) ≫ error "unreachable")
          pure
      matReg ← readIORef (materialRegistryRef env)
      staged ← stageSession env logger sd matReg ⌦ either
          (\e → expectationFailure (T.unpack (renderStageError e))
                  ≫ error "unreachable")
          pure

      publishStagedSession env logger 424242 staged
      writeIORef (enginePausedRef env) False
      loaded ← waitForWorldInit env pid 300

      -- Both sides in one assertion: the still-tilled record survives
      -- (this is the behaviour tools/plant_probe.py already relies on),
      -- and the one whose ground came back untilled is gone rather than
      -- restored as an invisible job.
      sort . HM.keys <$> readIORef (wsPlantDesignationsRef loaded)
          `shouldReturn` [persistTile]

-- | Pick two distinct tiles in one resident chunk whose surface is dry
--   and unseeded — @lcSurfaceMap@ equal to @lcTerrainSurfaceMap@ — so
--   the z this example tills at is the terrain surface a regenerated
--   chunk will present again.
twoDryTiles ∷ WorldState → IO ((Int, Int), (Int, Int), Int)
twoDryTiles ws = do
    td ← readIORef (wsTilesRef ws)
    let candidates =
            [ (tileOf coord idx, surf)
            | (coord, lc) ← HM.toList (wtdChunks td)
            , null (fcdInstances (lcFlora lc))
            , idx ← [0 .. chunkSize * chunkSize - 1]
            , let surf = lcSurfaceMap lc VU.! idx
            , surf ≡ lcTerrainSurfaceMap lc VU.! idx
            , let col = lcTiles lc V.! idx
            , let i = surf - ctStartZ col
            , i ≥ 0 ∧ i < VU.length (ctVeg col)
            ]
        tileOf (ChunkCoord cx cy) idx =
            ( cx * chunkSize + (idx `mod` chunkSize)
            , cy * chunkSize + (idx `div` chunkSize) )
    case [ (a, b, za) | ((a, za) : rest) ← [candidates]
                      , (b, zb) ← rest, zb ≡ za, b ≢ a ] of
        ((a, b, z) : _) → pure (a, b, z)
        [] → expectationFailure
                 "fixture world has no two dry same-z tiles"
             ≫ error "unreachable"

-- | Overwrite one tile's SURFACE vegetation cell in place, without
--   touching the edit log — live-only state, by construction.
setSurfaceVeg ∷ Int → (Int, Int) → Int → Word8 → WorldTileData
              → WorldTileData
setSurfaceVeg size (gx, gy) z veg td =
    let (coord, (lx, ly), _) = canonicalTileFrame size gx gy
        idx = columnIndex lx ly
        poke lc =
            let col = lcTiles lc V.! idx
                i   = z - ctStartZ col
            in if i < 0 ∨ i ≥ VU.length (ctVeg col) then lc
               else lc { lcTiles = lcTiles lc V.//
                           [(idx, col { ctVeg = ctVeg col VU.// [(i, veg)] })] }
    in td { wtdChunks = HM.adjust poke coord (wtdChunks td) }

-- | Poll until the world thread's setVeg edit has landed.
waitForVeg ∷ WorldState → (Int, Int) → Int → Word8 → IO ()
waitForVeg ws (gx, gy) z veg = go (300 ∷ Int)
  where
    go 0 = expectationFailure "setVeg never landed"
    go n = do
        td ← readIORef (wsTilesRef ws)
        size ← pageWrapWorldSize ws
        let (coord, (lx, ly), _) = canonicalTileFrame size gx gy
            idx = columnIndex lx ly
            here = do
                lc ← lookupChunk coord td
                let col = lcTiles lc V.! idx
                    i   = z - ctStartZ col
                if i ≥ 0 ∧ i < VU.length (ctVeg col)
                    then Just (ctVeg col VU.! i) else Nothing
        if here ≡ Just veg then pure () else threadDelay 100000 ≫ go (n - 1)

-- | Poll until the world thread has written the save file.
waitForFile ∷ FilePath → IO ()
waitForFile path = go (300 ∷ Int)
  where
    go 0 = expectationFailure ("save file never appeared: " ⧺ path)
    go n = do
        exists ← doesFileExist path
        if exists then pure () else threadDelay 100000 ≫ go (n - 1)
