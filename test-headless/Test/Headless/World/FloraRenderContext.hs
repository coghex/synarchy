{-# LANGUAGE OverloadedStrings #-}
-- | Flora render context (#2547, EFM-4 of epic #2526).
--
--   Every 'FloraInstance' carries an explicit 'FloraContext' — 'ContextWild'
--   or 'ContextCultivated' — assigned by the code that CREATES it and
--   persisted nowhere, because the creation record already determines it.
--   The three creators are generated placement (wild), the planting-edit
--   replay (cultivated) and a groundcover crop plot's synthetic instance
--   (cultivated).
--
--   What is pinned here, and how:
--
--   * a generated instance is wild — over a fixture that places MANY
--     instances (the control below fails the group on an empty
--     placement), regenerated independently, never compared with itself;
--   * a replayed @WePlaceFloraWithId@ is cultivated, whatever its id
--     namespace;
--   * the legacy @world-edits@ v1 rewrite is cultivated, driven through
--     the REAL v1 decoder, the migration and the load-time reference
--     resolution before replay;
--   * the CURRENT named-edit encoding (@WePlaceFloraRef@ / world-edits
--     v6) survives a codec round trip, reference resolution and replay as
--     the same planted identity, still cultivated. This is a focused
--     codec-and-reconstruction round trip, NOT an @engine.saveWorld@
--     test, which the headless harness cannot perform;
--   * a chunk's eviction and regeneration: the generated plants come back
--     wild and the replayed ones cultivated, in the same chunk;
--   * a crop plot's synthetic instance is cultivated, and keeps the
--     reserved non-identity id;
--   * the wild assertion discriminates: the placement mutation (every
--     generated instance becoming cultivated) makes it fail.
--
--   Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match "flora render context"'@
module Test.Headless.World.FloraRenderContext (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Asset.Handle (TextureHandle(..))
import Test.Headless.World.Save.Components.Fixture (minimalGlobals)
import World.Chunk.Types
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Flora.CropPlot (CropPlotOf(..), cropPlotInstance)
import World.Flora.Identity
import World.Flora.Placement (computeChunkFlora)
import World.Flora.Reference (FloraRef(..))
import World.Flora.Types
import World.Fluid.Types (FluidCell, emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page
import World.Save.Component.Types (ComponentCodec(..))
import World.Save.Snapshot (PageSnapshot(..), buildSessionSnapshot)
import World.Save.Types (resolveFloraReferences)
import Structure.Types (emptyChunkStructures)
import World.Weather.Types
    ( ClimateCoord(..), ClimateGrid(..), ClimateState(..)
    , RegionClimate(..), SeasonalClimate(..), defaultRegionClimate )

-- * Fixture

worldSize ∷ Int
worldSize = 64

pageKey ∷ Text
pageKey = "flora_render_context_probe"

fixturePage ∷ WorldPageId
fixturePage = WorldPageId pageKey

seed ∷ Word64
seed = 4242

zSlice ∷ Int
zSlice = 8

-- | The chunk every example plants into, and the tile inside it.
homeChunk ∷ ChunkCoord
homeChunk = ChunkCoord 0 0

plantTile ∷ (Int, Int)
plantTile = (2, 3)

-- | A second tile for the replayed state edit and the multi-plant cases.
otherTile ∷ (Int, Int)
otherTile = (5, 6)

-- | One species that grows anywhere the fixture's climate and soil allow,
--   so placement is NON-EMPTY (a control below asserts that), plus a
--   second, non-generating species a player can plant.
meadowId, wheatId ∷ FloraId
meadowId = FloraId 1
wheatId  = FloraId 2

meadowName, wheatName ∷ Text
meadowName = "probe_meadow"
wheatName  = "probe_wheat"

probeCatalog ∷ FloraCatalog
probeCatalog = insertWorldGen meadowId meadowGen emptyFloraCatalog
    { fcSpecies = HM.fromList
        [ (1, newFloraSpecies meadowName (TextureHandle 0))
        , (2, newFloraSpecies wheatName  (TextureHandle 0)) ]
    , fcNextId = 3 }
  where
    meadowGen = FloraWorldGen
        { fwCategory      = "groundcover"
        , fwMinTemp       = -100.0, fwMaxTemp = 100.0, fwIdealTemp = 15.0
        , fwMinPrecip     = 0.0,    fwMaxPrecip = 1.0, fwIdealPrecip = 0.5
        , fwMinAlt        = -1000,  fwMaxAlt = 1000,   fwIdealAlt = zSlice
        , fwMinHumidity   = 0.0,    fwMaxHumidity = 1.0, fwIdealHumidity = 0.5
        , fwMaxSlope      = 15
        , fwDensity       = 1.0
        , fwSoils         = [soilMat]
        , fwFootprint     = 0.0
        }

-- | Loam, a real soil (see "Test.Headless.World.FloraOrder"): flora
--   rejects every rock id outright.
soilMat ∷ Word8
soilMat = 56

flatSurface ∷ VU.Vector Int
flatSurface = VU.replicate (chunkSize * chunkSize) zSlice

flatMats, flatSlopes ∷ VU.Vector Word8
flatMats   = VU.replicate (chunkSize * chunkSize) soilMat
flatSlopes = VU.replicate (chunkSize * chunkSize) 0

noFluid ∷ V.Vector (Maybe FluidCell)
noFluid = V.replicate (chunkSize * chunkSize) Nothing

-- | A uniform, in-window climate. 'defaultWorldGenParams' carries an
--   EMPTY grid that interpolates to 0°C / 0 precipitation, which kills
--   placement outright, so the grid is built here.
climate ∷ ClimateState
climate = base
    { csClimate = ClimateGrid
        { cgSize = regions
        , cgRegions = HM.fromList
            [ (ClimateCoord ru rv, region)
            | ru ← [0 .. regions - 1], rv ← [0 .. regions - 1] ] } }
  where
    base = wgpClimateState defaultWorldGenParams
    regions = worldSize `div` 4
    region = defaultRegionClimate
        { rcAirTemp = SeasonalClimate 15.0 15.0
        , rcPrecipitation = SeasonalClimate 0.5 0.5
        , rcHumidity = 0.5 }

-- | The generated flora of one chunk, from freshly built inputs each
--   call, so two calls share no value: an eviction followed by a
--   regeneration, not a comparison of a value with itself.
generated ∷ ChunkCoord → [FloraInstance]
generated coord = fcdInstances $
    computeChunkFlora pageKey seed worldSize coord
        (VU.fromList (VU.toList flatSurface))
        (VU.fromList (VU.toList flatMats))
        (VU.fromList (VU.toList flatSlopes))
        (V.fromList (V.toList noFluid))
        climate probeCatalog

-- | A chunk holding the given flora, with the ONE surface z the planting
--   replay reads.
chunkHolding ∷ ChunkCoord → [FloraInstance] → LoadedChunk
chunkHolding coord insts =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
                 { ctStartZ = 0
                 , ctMats   = VU.replicate 20 1
                 , ctSlopes = VU.replicate 20 0
                 , ctVeg    = VU.replicate 20 0 }
    in LoadedChunk
        { lcCoord = coord
        , lcTiles = V.replicate area col
        , lcSurfaceMap = flatSurface
        , lcTerrainSurfaceMap = flatSurface
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap
        , lcFlora = FloraChunkData insts
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

-- | The instances a chunk holds, by id.
byId ∷ LoadedChunk → [(FloraInstanceId, FloraContext)]
byId lc = [ (fiInstanceId fi, fiContext fi) | fi ← fcdInstances (lcFlora lc) ]

contextOf ∷ FloraInstanceId → LoadedChunk → Maybe FloraContext
contextOf iid lc = lookup iid (byId lc)

-- * The planting edits

plantedIid ∷ FloraInstanceId
plantedIid = plantedFloraInstanceId 7

secondPlantedIid ∷ FloraInstanceId
secondPlantedIid = plantedFloraInstanceId 8

-- | The in-memory, ASSEMBLED planting entry (#2243): the species is
--   named, as a page carries it between load and save.
namedPlanting ∷ WorldEdit
namedPlanting = WePlaceFloraRef (fst plantTile) (snd plantTile)
    (FloraByName wheatName) 5 1.5 plantedIid

-- | A planting followed by a state edit on a SECOND plant, so the
--   record-update path ('WeSetFloraState') is exercised too.
plantingLog ∷ [WorldEdit]
plantingLog =
    [ namedPlanting
    , WePlaceFloraRef (fst otherTile) (snd otherTile)
          (FloraByName wheatName) 6 1.5 secondPlantedIid
    , WeSetFloraState (fst otherTile) (snd otherTile) secondPlantedIid
          zSlice 0.25 (-0.25) 2 0.5 (-3.0)
    ]

editsOf ∷ [WorldEdit] → WorldEdits
editsOf es = HM.singleton homeChunk es

-- | The log a LIVE page replays: the assembled named entries resolved
--   against the catalog, as the load stage does before any chunk sees an
--   edit. 'applyEdit' deliberately drops an unresolved @WePlaceFloraRef@.
liveLog ∷ WorldEdits
liveLog = resolvedEdits basePage { pgsEdits = editsOf plantingLog }

basePage ∷ PageSnapshot
basePage = blankPageSnapshot fixturePage
    defaultWorldGenParams { wgpWorldSize = worldSize }

-- | Resolve a decoded page's saved species references and return the
--   edit log a load would replay — the step 'World.Load.Stage' performs
--   before any chunk sees an edit.
resolvedEdits ∷ PageSnapshot → WorldEdits
resolvedEdits p =
    case resolveFloraReferences probeCatalog fixturePage
            (pgsEdits p) (pgsCropPlots p) (pgsPlantDesignations p)
            (pgsFloraConditions p) of
        Left missing → error ("unresolved flora references: " <> show missing)
        Right (edits, _, _, _) → edits

-- | Decode a payload through the real codec at @ver@ and apply it to the
--   fixture page, as the loader does.
decodeAndApply ∷ Word32 → BS.ByteString → PageSnapshot
decodeAndApply ver bytes =
    case ccDecode worldEditsCodec ver bytes of
        Left e → error ("decode: " <> show e)
        Right dto → case applyWorldEdits ver dto
                            (HM.singleton fixturePage basePage) of
            Left errs → error ("apply: " <> show errs)
            Right pages → case HM.lookup fixturePage pages of
                Nothing → error "page missing"
                Just p  → p

-- * The examples

spec ∷ Spec
spec = describe "flora render context" $ do

  describe "generated placement" $ do

    it "places a NON-EMPTY fixture, so the wild assertions below are not \
       \vacuous" $ do
      length (generated homeChunk) `shouldSatisfy` (> 5)
      length (generated (ChunkCoord 3 4)) `shouldSatisfy` (> 5)

    it "assigns every generated instance the wild context" $
      forM_ [homeChunk, ChunkCoord 3 4, ChunkCoord 7 1] $ \coord →
        map fiContext (generated coord)
          `shouldSatisfy` (\cs → not (null cs) ∧ all (≡ ContextWild) cs)

    it "is discriminating: the placement mutation (every generated \
       \instance cultivated) fails the wild assertion" $ do
      let mutated = [ fi { fiContext = ContextCultivated }
                    | fi ← generated homeChunk ]
      -- The very predicate the example above asserts, applied to the
      -- mutant's output. It must REJECT it, or the group could not
      -- catch the regression it exists for.
      all ((≡ ContextWild) . fiContext) mutated `shouldBe` False

    it "survives chunk eviction and regeneration, rebuilt from fresh \
       \inputs" $ do
      let before = generated homeChunk
          after  = generated homeChunk
      before `shouldSatisfy` (not . null)
      map (\fi → (fiInstanceId fi, fiContext fi)) after
        `shouldBe` map (\fi → (fiInstanceId fi, fiContext fi)) before
      after `shouldBe` before

  describe "planting-edit replay" $ do

    it "replays a WePlaceFloraWithId as a cultivated instance" $ do
      let lc = replayEdits
                 (editsOf [ WePlaceFloraWithId (fst plantTile) (snd plantTile)
                                wheatId 5 1.5 plantedIid ])
                 (chunkHolding homeChunk [])
      contextOf plantedIid lc `shouldBe` Just ContextCultivated

    it "assigns by what created the instance, never by id namespace: a \
       \GENERATED-namespace id replayed as a planting is still \
       \cultivated" $ do
      let gid = generatedFloraInstanceId pageKey 2 3 wheatName 0
      isGeneratedFloraInstanceId gid `shouldBe` True
      let lc = replayEdits
                 (editsOf [ WePlaceFloraWithId (fst plantTile) (snd plantTile)
                                wheatId 5 1.5 gid ])
                 (chunkHolding homeChunk [])
      contextOf gid lc `shouldBe` Just ContextCultivated

    it "leaves a wild neighbour wild and keeps the planted one \
       \cultivated, in the same chunk" $ do
      let wild = generated homeChunk
          lc = replayEdits liveLog
                 (chunkHolding homeChunk wild)
      wild `shouldSatisfy` (not . null)
      forM_ wild $ \fi →
        contextOf (fiInstanceId fi) lc `shouldBe` Just ContextWild
      contextOf plantedIid lc `shouldBe` Just ContextCultivated
      -- The state edit rewrites a cultivated plant's physical fields and
      -- must not touch its context.
      contextOf secondPlantedIid lc `shouldBe` Just ContextCultivated

    it "reaches a regenerated chunk as the same split: wild from the \
       \generator, cultivated from the replay" $ do
      let rebuild = replayEdits liveLog
                      (chunkHolding homeChunk (generated homeChunk))
          first  = byId rebuild
          second = byId (replayEdits liveLog
                      (chunkHolding homeChunk (generated homeChunk)))
      second `shouldBe` first
      length [ () | (_, c) ← first, c ≡ ContextCultivated ] `shouldBe` 2
      length [ () | (_, c) ← first, c ≡ ContextWild ]
        `shouldBe` length (generated homeChunk)

  describe "the legacy world-edits v1 rewrite" $ do

    it "replays a rewritten v1 WePlaceFlora as cultivated, under the id \
       \the migration assigned" $ do
      let dto = WorldEditsDTOv1
            [ PageEditsDTOv1 fixturePage $ HM.fromList
                [ (homeChunk, [ WePlaceFloraDv1 (fst plantTile) (snd plantTile)
                                    wheatId 5 1.5 ]) ] ]
          migrated = decodeAndApply 1 (S.encode dto)
          log' = HM.lookupDefault [] homeChunk (pgsEdits migrated)
          lc = replayEdits (resolvedEdits migrated)
                 (chunkHolding homeChunk (generated homeChunk))
      -- A real migration happened: the v1 payload carried no id at all.
      [ iid | WePlaceFloraRef _ _ _ _ _ iid ← log' ]
        `shouldBe` [plantedFloraInstanceId 1]
      contextOf (plantedFloraInstanceId 1) lc `shouldBe` Just ContextCultivated
      -- And the generated flora beside it is untouched.
      forM_ (generated homeChunk) $ \fi →
        contextOf (fiInstanceId fi) lc `shouldBe` Just ContextWild

    it "keeps the id-less engine form cultivated too: applyEdit of a \
       \bare WePlaceFlora is a planting" $ do
      let lc = replayEdits
                 (editsOf [ WePlaceFlora (fst plantTile) (snd plantTile)
                                wheatId 5 1.5 ])
                 (chunkHolding homeChunk [])
      map snd (byId lc) `shouldBe` [ContextCultivated]

  describe "the current named-edit encoding" $ do

    it "round-trips the codec, reference resolution and replay as the \
       \same planted identity, still cultivated" $ do
      let page = basePage
            { pgsEdits = editsOf plantingLog
            , pgsPlantedFloraCursor = 9 }
          snapshot = buildSessionSnapshot minimalGlobals [page]
          bytes = ccEncode worldEditsCodec snapshot
          restored = decodeAndApply (ccVersion worldEditsCodec) bytes
          lcBefore = replayEdits (resolvedEdits page)
                       (chunkHolding homeChunk (generated homeChunk))
          lcAfter  = replayEdits (resolvedEdits restored)
                       (chunkHolding homeChunk (generated homeChunk))
      -- The named references survived the codec as names, not ordinals.
      [ ref | WePlaceFloraRef _ _ ref _ _ _
                ← HM.lookupDefault [] homeChunk (pgsEdits restored) ]
        `shouldBe` [FloraByName wheatName, FloraByName wheatName]
      -- Same planted identities, same contexts, before and after.
      byId lcAfter `shouldBe` byId lcBefore
      contextOf plantedIid lcAfter `shouldBe` Just ContextCultivated
      contextOf secondPlantedIid lcAfter `shouldBe` Just ContextCultivated
      forM_ (generated homeChunk) $ \fi →
        contextOf (fiInstanceId fi) lcAfter `shouldBe` Just ContextWild
      -- Reconstruction is a full-instance equality, not only a context
      -- one: nothing about the plant was renamed or recontextualised.
      fcdInstances (lcFlora lcAfter) `shouldBe` fcdInstances (lcFlora lcBefore)

  describe "a groundcover crop plot's synthetic instance" $ do

    it "is cultivated and keeps the reserved non-identity id" $ do
      let inst = cropPlotInstance
                   (CropPlot { cpSpecies = wheatId, cpPlantedDay = 4
                             , cpHealth = 1.0 })
      fiContext inst `shouldBe` ContextCultivated
      fiInstanceId inst `shouldBe` floraInstanceIdNone

    it "does not borrow a wild instance's context for the same species" $ do
      let plot = CropPlot { cpSpecies = meadowId, cpPlantedDay = 0
                          , cpHealth = 1.0 }
      -- The meadow also GENERATES wild instances; the plot's synthetic
      -- instance of that same species is cultivated regardless.
      map fiContext (generated homeChunk)
        `shouldSatisfy` (\cs → not (null cs) ∧ all (≡ ContextWild) cs)
      fiContext (cropPlotInstance plot) `shouldBe` ContextCultivated
