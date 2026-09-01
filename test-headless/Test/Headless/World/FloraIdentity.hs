{-# LANGUAGE OverloadedStrings #-}
-- | Stable per-flora-instance identity and exact mutable state (#1854).
--
--   Two eligible wood-tagged trees can legitimately share one tile
--   ('World.Flora.Types.FloraInstance'\'s own co-tenancy note), and every
--   mutable authority that should address ONE plant used to be keyed by
--   the tile: Chop designations, regrowth timers, the Lua chop claims
--   built on them. Designating or felling one plant therefore moved its
--   co-tenant's state too. These examples pin the identity that fixed
--   it, the exactness it buys, and the two legacy migrations that carry
--   pre-identity saves across.
--
--   Selected by @--match "flora instance identity"@ and
--   @--match "flora instance persistence"@.
module Test.Headless.World.FloraIdentity (spec, engineSpec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Data.Int (Int64)
import Data.IORef (newIORef)

import World.Chop.Types
import World.Construct.Attempt (firstConstructAttemptId)
import World.Chunk.Types
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Flora.CropPlot (CropPlot(..), cropPlotInstance)
import World.Flora.Designation
import World.Flora.Harvest (tickFloraHarvests)
import World.Flora.Identity
import World.Flora.Placement (computeChunkFlora)
import World.Flora.Types
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates
    (canonicalTile, globalToChunk, tileAliasStep)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page
import World.Save.Component.Types (ComponentCodec(..))
import World.Save.Snapshot (PageSnapshot(..))
import World.State.Types
import World.Tile.Types (WorldTileData(..), lookupChunk)
import World.Weather.Types (ClimateState)
import Structure.Types (emptyChunkStructures)

-- * Fixture geography

-- | A wrapping page, so the seam-alias example has a seam to cross.
worldSize ∷ Int
worldSize = 64

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "flora_identity_probe"

pageKey ∷ Text
pageKey = "flora_identity_probe"

zSlice ∷ Int
zSlice = 4

-- | The chunk every fixture plants into, and the tile inside it that
--   carries the co-tenant pair.
homeChunk ∷ ChunkCoord
homeChunk = ChunkCoord 2 3

homeTile ∷ (Int, Int)
homeTile = (2 * chunkSize + 5, 3 * chunkSize + 7)

-- | A second tile in the same chunk, for the "one plant's state never
--   reaches another tile" controls.
otherTile ∷ (Int, Int)
otherTile = (2 * chunkSize + 6, 3 * chunkSize + 7)

-- * Species

oakId, berryId, fernId ∷ FloraId
oakId   = FloraId 1
berryId = FloraId 2
fernId  = FloraId 3

-- | Two harvestable species — one wood-tagged (the Chop target), one
--   not — plus a decorative one with no harvest at all, which is what
--   makes "decorative co-tenants receive no timer" a real distinction.
probeCatalog ∷ FloraCatalog
probeCatalog = emptyFloraCatalog
    { fcSpecies = HM.fromList
        [ (1, harvestable "probe_oak"   ["wood"])
        , (2, harvestable "probe_berry" [])
        , (3, newFloraSpecies "probe_fern" (TextureHandle 0)) ]
    , fcNextId = 4 }
  where
    harvestable name tags = (newFloraSpecies name (TextureHandle 0))
        { fsHarvest = Just FloraHarvest
            { fhTags = tags, fhYield = [("probe_log", 1, 1)]
            , fhRegrowth = 100, fhHarvestedTexture = TextureHandle 0 } }

speciesName ∷ FloraId → Text
speciesName fid = maybe "" fsName (lookupSpecies fid probeCatalog)

-- | The id a generated plant of this species, at this canonical tile
--   and ordinal, carries — derived through the PRODUCTION function so
--   the fixture cannot drift from the engine's own answer.
idAt ∷ Int → Int → FloraId → Int → FloraInstanceId
idAt gx gy fid ordinal =
    let (cgx, cgy) = canonicalTile worldSize gx gy
    in generatedFloraInstanceId pageKey cgx cgy (speciesName fid) ordinal

instanceAt ∷ (Int, Int) → FloraId → Int → FloraInstance
instanceAt tile fid ordinal =
    let (_, (lx, ly)) = globalToChunk (fst tile) (snd tile)
    in FloraInstance
        { fiSpecies = fid
        , fiTileX = fromIntegral lx, fiTileY = fromIntegral ly
        , fiOffU = 0, fiOffV = 0, fiZ = zSlice
        , fiAge = 1, fiHealth = 1, fiVariant = 0, fiBaseWidth = 8
        , fiInstanceId = idAt (fst tile) (snd tile) fid ordinal
        , fiChopDesignated = False
        }

-- | The co-tenancy the whole issue is about: an oak and a berry bush on
--   ONE tile, plus a decorative fern beside them.
coTenants ∷ [FloraInstance]
coTenants =
    [ instanceAt homeTile oakId 0
    , instanceAt homeTile berryId 0
    , instanceAt homeTile fernId 0
    ]

oakIid, berryIid, fernIid ∷ FloraInstanceId
oakIid   = idAt (fst homeTile) (snd homeTile) oakId 0
berryIid = idAt (fst homeTile) (snd homeTile) berryId 0
fernIid  = idAt (fst homeTile) (snd homeTile) fernId 0

-- * Chunk / page fixtures

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

tilesWith ∷ [FloraInstance] → WorldTileData
tilesWith insts = WorldTileData
    { wtdChunks = HM.singleton homeChunk (chunkWith insts)
    , wtdMaxChunks = 200 }

-- | A fresh page carrying the given flora and nothing else, registered
--   as the engine's active world.
resetPage ∷ EngineEnv → [FloraInstance] → IO WorldState
resetPage env insts = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws) (tilesWith insts)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (floraCatalogRef env) probeCatalog
    pure ws

-- | The same page with NO chunk resident, so an admission of some other
--   chunk cannot resolve a pending entry on 'homeTile'.
resetPageEmpty ∷ EngineEnv → IO WorldState
resetPageEmpty env = do
    ws ← resetPage env []
    writeIORef (wsTilesRef ws)
        WorldTileData { wtdChunks = HM.empty, wtdMaxChunks = 200 }
    pure ws

loadedInstances ∷ WorldState → IO [FloraInstance]
loadedInstances ws = do
    td ← readIORef (wsTilesRef ws)
    pure $ maybe [] (fcdInstances . lcFlora) (lookupChunk homeChunk td)

designatedFlags ∷ WorldState → IO [(FloraInstanceId, Bool)]
designatedFlags ws =
    map (\fi → (fiInstanceId fi, fiChopDesignated fi)) <$> loadedInstances ws

-- * Pure examples

spec ∷ Spec
spec = do

  describe "flora instance identity" $ do

    describe "the generated namespace" $ do

      it "is a pure function of page, canonical tile, species NAME and \
         \that species' own ordinal" $ do
        generatedFloraInstanceId "p" 10 20 "oak" 0
          `shouldBe` generatedFloraInstanceId "p" 10 20 "oak" 0
        -- Every component genuinely participates.
        generatedFloraInstanceId "q" 10 20 "oak" 0
          `shouldNotBe` generatedFloraInstanceId "p" 10 20 "oak" 0
        generatedFloraInstanceId "p" 11 20 "oak" 0
          `shouldNotBe` generatedFloraInstanceId "p" 10 20 "oak" 0
        generatedFloraInstanceId "p" 10 21 "oak" 0
          `shouldNotBe` generatedFloraInstanceId "p" 10 20 "oak" 0
        generatedFloraInstanceId "p" 10 20 "elm" 0
          `shouldNotBe` generatedFloraInstanceId "p" 10 20 "oak" 0
        generatedFloraInstanceId "p" 10 20 "oak" 1
          `shouldNotBe` generatedFloraInstanceId "p" 10 20 "oak" 0

      it "gives two co-tenants on ONE tile distinct ids" $ do
        -- The defect the issue exists to fix, stated as a fact about
        -- the identity function: same page, same tile, different plants.
        L.nub (map fiInstanceId coTenants) `shouldBe` map fiInstanceId coTenants
        length coTenants `shouldBe` 3

      it "answers the same for every cylindrical-seam alias of a tile" $ do
        -- One physical tile has several names near the seam (#1175); the
        -- id is derived from the CANONICAL one, so a plant reached
        -- through an alias is the same plant.
        -- One u-alias step moves a tile by (+s, -s) — the wrap
        -- topology at tile granularity (#1175).
        let s = tileAliasStep worldSize
            (gx, gy) = homeTile
        s `shouldSatisfy` (> 0)
        canonicalTile worldSize (gx + s) (gy - s) `shouldBe` homeTile
        idAt gx gy oakId 0 `shouldBe` idAt (gx + s) (gy - s) oakId 0
        idAt gx gy oakId 0 `shouldBe` idAt (gx - s) (gy + s) oakId 0

      it "survives chunk eviction and reload, because placement is a \
         \deterministic function of the same inputs" $ do
        let place = computeChunkFlora pageKey 4242 worldSize homeChunk
                        flatSurface flatMats flatSlopes noFluid
                        emptyClimate probeCatalog
        map fiInstanceId (fcdInstances place)
          `shouldBe` map fiInstanceId (fcdInstances place)

      it "does not rename a surviving plant when an unrelated species is \
         \added or reordered" $ do
        -- The hazard the issue names: generated placement salts its
        -- ROLLS off the species' position in worldGenSpecies, which
        -- comes from a HashMap. An id derived from that position — or
        -- from the registration-order FloraId — would rename plants
        -- nobody touched. These two catalogs give "probe_oak" DIFFERENT
        -- FloraIds and different list positions; the id must not move.
        let catA = emptyFloraCatalog
                { fcSpecies = HM.fromList [(1, bare "probe_oak")]
                , fcNextId = 2 }
            catB = emptyFloraCatalog
                { fcSpecies = HM.fromList
                    [ (7, bare "probe_oak"), (1, bare "unrelated_a")
                    , (2, bare "unrelated_b"), (3, bare "unrelated_c") ]
                , fcNextId = 8 }
            bare name = newFloraSpecies name (TextureHandle 0)
            idIn cat fid = generatedFloraInstanceId pageKey 40 60
                               (maybe "" fsName (lookupSpecies fid cat)) 0
        idIn catA (FloraId 1) `shouldBe` idIn catB (FloraId 7)

    describe "the two namespaces are disjoint" $ do

      it "classifies every id into exactly one namespace" $ do
        let generated = [ generatedFloraInstanceId "p" gx gy "s" o
                        | gx ← [-3 .. 3], gy ← [-3 .. 3], o ← [0 .. 3] ]
            planted   = map plantedFloraInstanceId [1 .. 200]
        all isGeneratedFloraInstanceId generated `shouldBe` True
        any isPlantedFloraInstanceId generated `shouldBe` False
        all isPlantedFloraInstanceId planted `shouldBe` True
        any isGeneratedFloraInstanceId planted `shouldBe` False
        L.intersect generated planted `shouldBe` []

      it "reserves zero for a value that is not a plant at all, and gives \
         \it to the synthetic crop-plot adapter" $ do
        -- cropPlotInstance manufactures a placement-shaped instance so
        -- the growth math can run over a TILE-keyed crop plot. It is not
        -- a plant, never enters chunk data, and must never key durable
        -- state — so it carries the reserved value, which belongs to
        -- neither namespace and so can collide with no real plant.
        isGeneratedFloraInstanceId floraInstanceIdNone `shouldBe` False
        isPlantedFloraInstanceId floraInstanceIdNone `shouldBe` False
        isFloraInstanceIdNone floraInstanceIdNone `shouldBe` True
        fiInstanceId (cropPlotInstance (CropPlot berryId 3 1.0))
          `shouldBe` floraInstanceIdNone

    describe "the planted allocator" $ do

      it "hands out ascending ids and keeps its cursor strictly above \
         \every one of them" $ do
        let (a, c1) = nextPlantedFloraCursor firstPlantedFloraCursor
            (b, c2) = nextPlantedFloraCursor c1
        a `shouldNotBe` b
        plantedFloraCursorAbove [a, b] `shouldBe` c2
        c2 `shouldSatisfy` (> c1)

      it "ignores generated ids when deriving a floor — they are not \
         \this allocator's to be above" $
        plantedFloraCursorAbove
            [ generatedFloraInstanceId "p" 1 2 "s" 0, floraInstanceIdNone ]
          `shouldBe` firstPlantedFloraCursor

  describe "flora instance persistence" $ do

    describe "world-edits v1 → v2" $ do

      it "assigns every legacy planted edit a distinct id, in a \
         \deterministic order, and repeated migration agrees" $ do
        let migrated = migrateLegacyEdits
        map plantedIdOf (chunkLog migrated (ChunkCoord 0 0))
          `shouldBe` [plantedFloraInstanceId 1, plantedFloraInstanceId 2]
        map plantedIdOf (chunkLog migrated (ChunkCoord 1 0))
          `shouldBe` [plantedFloraInstanceId 3]
        -- Repeatable: the same bytes migrate to the same ids, so a save
        -- read twice never renames a crop.
        migrated `shouldBe` migrateLegacyEdits

      it "orders a seam ALIAS chunk key against its canonical twin \
         \deterministically, never by hashmap traversal" $ do
        -- Two DISTINCT keys that canonicalize to the same chunk — the
        -- case World.Edit.Types.canonicalizeWorldEdits exists to merge.
        -- Sorting on the canonical coordinate alone leaves their order
        -- to HM.toList (sortOn is stable), so the ids handed out would
        -- vary with hashmap traversal. The alias sorts FIRST, matching
        -- canonicalizeWorldEdits' own merge order.
        wrapChunkCoordU worldSize aliasChunk `shouldBe` homeChunk
        aliasChunk `shouldNotBe` homeChunk
        let (edits, cursor) = migrateAliasedLegacyEdits
        map plantedIdOf (chunkLog edits aliasChunk)
          `shouldBe` [plantedFloraInstanceId 1]
        map plantedIdOf (chunkLog edits homeChunk)
          `shouldBe` [plantedFloraInstanceId 2]
        cursor `shouldBe` 3
        -- Repeatable, which is the property that actually matters.
        migrateAliasedLegacyEdits `shouldBe` migrateAliasedLegacyEdits

      it "initializes the cursor strictly above every id it assigned" $
        legacyCursor `shouldBe` plantedFloraCursorAbove
            [ plantedFloraInstanceId 1, plantedFloraInstanceId 2
            , plantedFloraInstanceId 3 ]

      it "leaves a v2 payload's own ids and cursor exactly as written" $ do
        let iid = plantedFloraInstanceId 9
            slice = PageEditsDTO fixturePage
                (HM.singleton (ChunkCoord 0 0)
                    [WePlaceFloraWithIdD 1 2 berryId 5 1.5 iid]) 10
        case applyWorldEdits 2 (WorldEditsDTO [slice])
                 (HM.singleton fixturePage basePage) of
            Left errs → expectationFailure (show errs)
            Right pages → case HM.lookup fixturePage pages of
                Nothing → expectationFailure "page missing"
                Just p → do
                    map plantedIdOf (chunkLog (pgsEdits p) (ChunkCoord 0 0))
                      `shouldBe` [iid]
                    pgsPlantedFloraCursor p `shouldBe` 10

    describe "world-activity v3 → v4" $ do

      it "defers a legacy tile-keyed designation and timer into the \
         \PENDING maps, leaving the live per-instance maps empty" $ do
        let legacy = WorldActivityDTOv3
                [ (emptyActivityV3 fixturePage)
                    { pad3Chop = HM.singleton (11, 12) (ChopDesignationDTOv1 6)
                    , pad3FloraHarvests = HM.singleton (13, 14) 42.5 } ]
        case ccDecode worldActivityCodec 3 (S.encode legacy) of
            Left e → expectationFailure (show e)
            Right (WorldActivityDTO slices) → case slices of
                [s] → do
                    padChop s `shouldBe` HM.empty
                    padFloraHarvests s `shouldBe` HM.empty
                    HM.keys (padPendingChop s) `shouldBe` [(11, 12)]
                    padPendingHarvests s `shouldBe` HM.singleton (13, 14) 42.5
                _ → expectationFailure "expected one page slice"

      it "accepts v1, v2, v3, v4 and v5, so no shipped payload lost its \
         \decoder" $
        ccInputVers worldActivityCodec `shouldBe` [1, 2, 3, 4, 5]

      it "does not follow the live harvest alias into the frozen v1/v2 \
         \layout: a tile-keyed timer still decodes as a tile-keyed timer" $ do
        -- The hazard the issue calls out by name. pad2FloraHarvests used
        -- to be spelled through the LIVE 'FloraHarvests' alias; re-keying
        -- that alias would silently have rewritten every shipped v1/v2
        -- payload's decoding. It names the frozen tile-keyed shape now,
        -- and this drives the real v2 decoder over v2 bytes to prove it.
        let legacy = WorldActivityDTOv2
                [ (emptyActivityV2 fixturePage)
                    { pad2FloraHarvests = HM.singleton (5, 6) 7.5 } ]
        case ccDecode worldActivityCodec 2 (S.encode legacy) of
            Left e → expectationFailure (show e)
            Right (WorldActivityDTO slices) → case slices of
                [s] → padPendingHarvests s `shouldBe` HM.singleton (5, 6) 7.5
                _ → expectationFailure "expected one page slice"

  where
    flatSurface = VU.replicate (chunkSize * chunkSize) zSlice
    flatMats    = VU.replicate (chunkSize * chunkSize) (1 ∷ Word8)
    flatSlopes  = VU.replicate (chunkSize * chunkSize) (0 ∷ Word8)
    noFluid     = V.replicate (chunkSize * chunkSize) Nothing

-- | The shipped default climate. These examples only ever compare
--   'computeChunkFlora'\'s output against ITSELF, never against a
--   placement count, so the climate only has to be well-formed.
emptyClimate ∷ ClimateState
emptyClimate = wgpClimateState defaultWorldGenParams

-- * world-edits migration fixture

-- | Two legacy planted edits in one chunk and a third in another, at the
--   frozen v1 shape that records no ids at all.
legacyEditsDTO ∷ WorldEditsDTOv1
legacyEditsDTO = WorldEditsDTOv1
    [ PageEditsDTOv1 fixturePage $ HM.fromList
        [ (ChunkCoord 0 0, [ WePlaceFloraDv1 1 2 berryId 5 1.5
                           , WeSetVegDv1 1 2 3 9
                           , WePlaceFloraDv1 3 4 berryId 6 1.5 ])
        , (ChunkCoord 1 0, [ WePlaceFloraDv1 20 2 berryId 7 1.5 ]) ]
    ]

-- | The u-alias of 'homeChunk': one wrap step along the seam axis, so
--   it is a different KEY naming the same physical chunk.
aliasChunk ∷ ChunkCoord
aliasChunk =
    let ChunkCoord cx cy = homeChunk
        step = worldSize `div` 2
    in ChunkCoord (cx + step) (cy - step)

-- | A v1 log holding one planted edit under a seam ALIAS key and
--   another under its canonical twin, driven through the real decoder
--   and apply step.
migrateAliasedLegacyEdits ∷ (WorldEdits, Word64)
migrateAliasedLegacyEdits =
    let dto = WorldEditsDTOv1
            [ PageEditsDTOv1 fixturePage $ HM.fromList
                [ (homeChunk,  [WePlaceFloraDv1 1 2 berryId 5 1.5])
                , (aliasChunk, [WePlaceFloraDv1 3 4 berryId 6 1.5]) ] ]
    in case ccDecode worldEditsCodec 1 (S.encode dto) of
        Left e → error (show e)
        Right d → case applyWorldEdits 1 d (HM.singleton fixturePage basePage) of
            Left errs → error (show errs)
            Right pages → case HM.lookup fixturePage pages of
                Nothing → error "page missing"
                Just p → (pgsEdits p, pgsPlantedFloraCursor p)

basePage ∷ PageSnapshot
basePage = blankPageSnapshot fixturePage
    defaultWorldGenParams { wgpWorldSize = worldSize }

-- | The legacy log driven through the REAL v1 decoder and apply step,
--   which is where #1854's id assignment lives.
migratedLegacy ∷ (WorldEdits, Word64)
migratedLegacy =
    case ccDecode worldEditsCodec 1 (S.encode legacyEditsDTO) of
        Left e → error (show e)
        Right dto → case applyWorldEdits 1 dto (HM.singleton fixturePage basePage) of
            Left errs → error (show errs)
            Right pages → case HM.lookup fixturePage pages of
                Nothing → error "page missing"
                Just p → (pgsEdits p, pgsPlantedFloraCursor p)

migrateLegacyEdits ∷ WorldEdits
migrateLegacyEdits = fst migratedLegacy

legacyCursor ∷ Word64
legacyCursor = snd migratedLegacy

-- | Only the planting entries of one chunk's log, in stored order.
chunkLog ∷ WorldEdits → ChunkCoord → [WorldEdit]
chunkLog edits coord =
    [ e | e@(WePlaceFloraWithId {}) ← HM.lookupDefault [] coord edits ]

-- | The id a planting entry carries. An id-LESS entry reaching this
--   would be a live session holding one, which 'applyWorldEdits'
--   rewrites away — 'floraInstanceIdNone' makes that visible as a
--   failing comparison rather than a pattern-match crash.
plantedIdOf ∷ WorldEdit → FloraInstanceId
plantedIdOf (WePlaceFloraWithId _ _ _ _ _ iid) = iid
plantedIdOf _ = floraInstanceIdNone

emptyActivityV3 ∷ WorldPageId → PageActivityDTOv3
emptyActivityV3 pid = PageActivityDTOv3
    { pad3PageId = pid, pad3Mine = HM.empty, pad3Construct = HM.empty
    , pad3Chop = HM.empty, pad3Till = HM.empty, pad3Plant = HM.empty
    , pad3FloraHarvests = HM.empty, pad3CropPlots = HM.empty
    , pad3GroundItems = GroundItemsDTO 0 HM.empty
    , pad3SpoilPiles = HM.empty }

emptyActivityV2 ∷ WorldPageId → PageActivityDTOv2
emptyActivityV2 pid = PageActivityDTOv2
    { pad2PageId = pid, pad2Mine = HM.empty, pad2Construct = HM.empty
    , pad2Chop = HM.empty, pad2Till = HM.empty, pad2Plant = HM.empty
    , pad2FloraHarvests = HM.empty, pad2CropPlots = HM.empty
    , pad2GroundItems = GroundItemsDTOv1 0 HM.empty
    , pad2SpoilPiles = HM.empty }

-- * Engine-backed examples

engineSpec ∷ Spec
engineSpec = beforeAll setup $ do

  describe "flora instance identity" $ do

    it "designating one co-tenant moves ONLY that plant's durable entry \
       \and loaded flag" $ \(env, _) → do
      ws ← resetPage env coTenants
      designateChopInstances ws [(oakIid, fst homeTile, snd homeTile, zSlice)]
      HM.keys <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` [oakIid]
      flags ← designatedFlags ws
      L.lookup oakIid flags `shouldBe` Just True
      L.lookup berryIid flags `shouldBe` Just False
      L.lookup fernIid flags `shouldBe` Just False

    it "records the designated plant's canonical tile, so a marker and a \
       \nearest-designation scan still have somewhere to read" $ \(env, _) → do
      ws ← resetPage env coTenants
      let s = tileAliasStep worldSize
      -- Designate through an ALIAS of the tile; the record must hold the
      -- canonical coords (#1175).
      designateChopInstances ws
          [(oakIid, fst homeTile + s, snd homeTile - s, zSlice)]
      m ← readIORef (wsChopDesignationsRef ws)
      fmap chopDesignationTile (HM.lookup oakIid m) `shouldBe` Just homeTile
      fmap chZ (HM.lookup oakIid m) `shouldBe` Just zSlice

    it "cancelling that plant leaves a co-tenant's designation standing" $
        \(env, _) → do
      ws ← resetPage env coTenants
      designateChopInstances ws
          [ (oakIid, fst homeTile, snd homeTile, zSlice)
          , (berryIid, fst homeTile, snd homeTile, zSlice) ]
      cancelChopForInstance ws oakIid
      HM.keys <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` [berryIid]
      flags ← designatedFlags ws
      L.lookup oakIid flags `shouldBe` Just False
      L.lookup berryIid flags `shouldBe` Just True

    it "the player's tile-granularity cancel still clears everything \
       \standing on the tile" $ \(env, _) → do
      ws ← resetPage env coTenants
      designateChopInstances ws
          [ (oakIid, fst homeTile, snd homeTile, zSlice)
          , (berryIid, fst homeTile, snd homeTile, zSlice) ]
      cancelChopAtTile ws (fst homeTile) (snd homeTile)
      HM.null <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` True

    it "an exact harvest starts a timer for the selected plant alone, and \
       \regrowing it leaves its co-tenant untouched" $ \(env, ls) → do
      ws ← resetPage env coTenants
      harvested ← evalDebug ls (T.concat
          [ "local r = world.harvestFloraInstance(", tshow (fst homeTile)
          , ", ", tshow (snd homeTile), ", ", tshow (idNum oakIid)
          , ", 'wood'); return r and #r or -1" ])
      harvested `shouldNotBe` "-1"
      HM.keys <$> readIORef (wsFloraHarvestsRef ws) `shouldReturn` [oakIid]
      -- Regrowth is per-plant too: ticking past the oak's timer clears
      -- the oak's entry and there was never a berry entry to clear.
      before ← readIORef (wsFloraHarvestsRef ws)
      let (after, regrew) = tickFloraHarvests 1000 before
      regrew `shouldBe` True
      HM.null after `shouldBe` True

    it "refuses an exact harvest of a plant that is not on the named tile" $
        \(env, ls) → do
      _ ← resetPage env coTenants
      evalDebug ls (T.concat
          [ "local r = world.harvestFloraInstance(", tshow (fst otherTile)
          , ", ", tshow (snd otherTile), ", ", tshow (idNum oakIid)
          , ", 'wood'); return r and 'some' or 'nil'" ])
        `shouldReturn` "nil"

    it "removing a plant clears its designation and its timer, and no \
       \orphan entry outlives it" $ \(env, _) → do
      ws ← resetPage env coTenants
      designateChopInstances ws
          [ (oakIid, fst homeTile, snd homeTile, zSlice)
          , (berryIid, fst homeTile, snd homeTile, zSlice) ]
      writeIORef (wsFloraHarvestsRef ws)
          (HM.fromList [(oakIid, 50), (berryIid, 60)])
      td ← readIORef (wsTilesRef ws)
      case lookupChunk homeChunk td of
          Nothing → expectationFailure "fixture chunk missing"
          Just old → do
              let survivors = filter ((≢ oakIid) . fiInstanceId) coTenants
              replaceChunkForgettingFlora ws old (chunkWith survivors)
              HM.keys <$> readIORef (wsChopDesignationsRef ws)
                `shouldReturn` [berryIid]
              HM.keys <$> readIORef (wsFloraHarvestsRef ws)
                `shouldReturn` [berryIid]

    it "keeps world.getFloraAt's existing species-facing contract and \
       \exposes identity ADDITIVELY" $ \(env, ls) → do
      ws ← resetPage env coTenants
      designateChopInstances ws [(oakIid, fst homeTile, snd homeTile, zSlice)]
      -- @id@ is still the SPECIES name, @harvestable@/@regrowthRemaining@
      -- /@tags@ still mean what they meant, and the plant's own identity
      -- and chop flag ride alongside.
      evalDebug ls (T.concat
          [ "local f = world.getFloraAt(", tshow (fst homeTile), ", "
          , tshow (snd homeTile), "); return f.id .. ',' .. "
          , "tostring(f.harvestable) .. ',' .. "
          , "string.format('%.0f', f.regrowthRemaining) .. ',' .. "
          , "tostring(f.instanceId) .. ',' .. tostring(f.chopDesignated)" ])
        `shouldReturn` T.concat
          [ "probe_oak,true,0,", tshow (idNum oakIid), ",true" ]

    it "reports each co-tenant's OWN timer through world.getFloraGrowthAt" $
        \(env, ls) → do
      ws ← resetPage env coTenants
      writeIORef (wsFloraHarvestsRef ws) (HM.singleton berryIid 77)
      -- Under the old tile key both harvestable plants read as depleted
      -- the moment either was picked; each now answers for itself, and
      -- the decorative fern (no harvest at all) is unaffected either way.
      evalDebug ls (T.concat
          [ "local g = world.getFloraGrowthAt(", tshow (fst homeTile), ", "
          , tshow (snd homeTile), "); local o = {}; "
          , "for _, e in ipairs(g) do o[#o+1] = e.id .. '=' .. "
          , "string.format('%.0f', e.regrowthRemaining) end; "
          , "return table.concat(o, ' ')" ])
        `shouldReturn` "probe_oak=0 probe_berry=77 probe_fern=0"

    it "answers chop.getDesignationForInstance for the designated plant \
       \and refuses its co-tenant" $ \(env, ls) → do
      ws ← resetPage env coTenants
      designateChopInstances ws [(oakIid, fst homeTile, snd homeTile, zSlice)]
      let ask iid = evalDebug ls (T.concat
              [ "local d = chop.getDesignationForInstance('", pageKey, "', "
              , tshow (idNum iid), "); return d and tostring(d.z) or 'nil'" ])
      ask oakIid `shouldReturn` tshow zSlice
      ask berryIid `shouldReturn` "nil"
      -- The nearest-designation scan reports that same plant's id.
      evalDebug ls (T.concat
          [ "local gx, gy, d, iid = chop.nearestDesignation('", pageKey
          , "', ", tshow (fst homeTile), ", ", tshow (snd homeTile)
          , "); return gx .. ',' .. gy .. ',' .. tostring(iid)" ])
        `shouldReturn` T.concat
          [ tshow (fst homeTile), ",", tshow (snd homeTile)
          , ",", tshow (idNum oakIid) ]

    it "lists EVERY designation on a tile, ascending by id — the order a \
       \restored chop job walks to find one nobody else holds" $
        \(env, ls) → do
      -- chopJob.iid is deliberately not persisted, so a job restored
      -- from a save knows only its TILE. scripts/unit_ai_chop.lua walks
      -- this list and adopts (and claims, in the same step) the first
      -- plant no other acolyte holds — which is what stops two units
      -- restoring jobs here from both taking the lower id, felling one
      -- tree together and orphaning the other's designation. The Lua
      -- half is pinned by "Test.Headless.Lua.UnitAiLoadReset".
      ws ← resetPage env coTenants
      designateChopInstances ws
          [ (oakIid, fst homeTile, snd homeTile, zSlice)
          , (berryIid, fst homeTile, snd homeTile, zSlice) ]
      evalDebug ls (T.concat
          [ "local ds = chop.getDesignationsAt('", pageKey, "', "
          , tshow (fst homeTile), ", ", tshow (snd homeTile), "); "
          , "if not ds then return 'nil' end; local o = {}; "
          , "for _, d in ipairs(ds) do o[#o+1] = tostring(d.instanceId) end; "
          , "return table.concat(o, ' ')" ])
        `shouldReturn` T.unwords
            (map (tshow . idNum) (L.sort [oakIid, berryIid]))

  describe "flora instance persistence" $ do

    it "resolves a deferred legacy chop designation to the plant the old \
       \wood-tagged harvest would have felled" $ \(env, _) → do
      ws ← resetPageEmpty env
      logger ← readIORef (loggerRef env)
      -- The berry bush is FIRST in stored order, so a tag-blind
      -- migration would pick it. The old selection filtered on the
      -- "wood" tag first, and this must reproduce that exactly.
      let ordered = [ instanceAt homeTile berryId 0
                    , instanceAt homeTile oakId 0
                    , instanceAt homeTile fernId 0 ]
      writeIORef (wsPendingChopMigrationRef ws)
          (HM.singleton homeTile (ChopDesignation zSlice
              (fst homeTile) (snd homeTile)))
      admitted ← admitChunkFlora ws probeCatalog logger (chunkWith ordered)
      HM.keys <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` [oakIid]
      HM.null <$> readIORef (wsPendingChopMigrationRef ws) `shouldReturn` True
      -- The loaded mirror is hydrated by the SAME admission, so the
      -- durable entry and the flag can never be observed out of step.
      [ (fiInstanceId fi, fiChopDesignated fi)
        | fi ← fcdInstances (lcFlora admitted) ]
        `shouldBe` [(berryIid, False), (oakIid, True), (fernIid, False)]

    it "expands a deferred legacy regrowth timer onto EVERY harvestable \
       \co-tenant with the same remaining time, and leaves a decorative \
       \one alone" $ \(env, _) → do
      ws ← resetPageEmpty env
      logger ← readIORef (loggerRef env)
      writeIORef (wsPendingFloraHarvestsRef ws) (HM.singleton homeTile 88.5)
      _ ← admitChunkFlora ws probeCatalog logger (chunkWith coTenants)
      timers ← readIORef (wsFloraHarvestsRef ws)
      L.sort (HM.toList timers)
        `shouldBe` L.sort [(oakIid, 88.5), (berryIid, 88.5)]
      HM.member fernIid timers `shouldBe` False
      HM.null <$> readIORef (wsPendingFloraHarvestsRef ws) `shouldReturn` True

    it "keeps an unresolvable entry PENDING, and answers no runtime query \
       \from it" $ \(env, ls) → do
      ws ← resetPageEmpty env
      logger ← readIORef (loggerRef env)
      writeIORef (wsPendingChopMigrationRef ws)
          (HM.singleton homeTile (ChopDesignation zSlice
              (fst homeTile) (snd homeTile)))
      writeIORef (wsPendingFloraHarvestsRef ws) (HM.singleton homeTile 88.5)
      -- Admitting some OTHER chunk cannot resolve this tile.
      let elsewhereChunk = (chunkWith []) { lcCoord = ChunkCoord 9 9 }
      _ ← admitChunkFlora ws probeCatalog logger elsewhereChunk
      HM.size <$> readIORef (wsPendingChopMigrationRef ws) `shouldReturn` 1
      HM.size <$> readIORef (wsPendingFloraHarvestsRef ws) `shouldReturn` 1
      -- Pending data is not an authority: nothing reads a designation,
      -- a marker count or a timer out of it.
      HM.null <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` True
      HM.null <$> readIORef (wsFloraHarvestsRef ws) `shouldReturn` True
      evalDebug ls (T.concat
          [ "local d = chop.getDesignationAt('", pageKey, "', "
          , tshow (fst homeTile), ", ", tshow (snd homeTile)
          , "); return d and 'some' or 'nil'" ]) `shouldReturn` "nil"
      evalDebug ls (T.concat
          [ "return chop.getDesignationCount('", pageKey, "')" ])
        `shouldReturn` "0"

    it "survives a further save/load while unresolved, then resolves when \
       \its chunk finally arrives" $ \(env, _) → do
      ws ← resetPageEmpty env
      logger ← readIORef (loggerRef env)
      let pendingChop = HM.singleton homeTile
              (ChopDesignation zSlice (fst homeTile) (snd homeTile))
          pendingHarv = HM.singleton homeTile 88.5
          page = basePage { pgsPendingChopMigration = pendingChop
                          , pgsPendingFloraHarvests = pendingHarv }
          slice = PageActivityDTO
              { padPageId = fixturePage
              , padMine = HM.empty, padConstruct = HM.empty
              , padChop = HM.empty, padTill = HM.empty, padPlant = HM.empty
              , padFloraHarvests = HM.empty, padCropPlots = HM.empty
              , padGroundItems = GroundItemsDTO 0 HM.empty
              , padSpoilPiles = HM.empty
              , padPendingChop = HM.map (ChopDesignationDTOv1 . chZ) pendingChop
              , padPendingHarvests = pendingHarv
              , padConstructNextAttempt = firstConstructAttemptId }
      -- Round-trip the v4 slice through the real codec, then restore it
      -- and prove the deferred entries are still there to resolve.
      case ccDecode worldActivityCodec 4 (S.encode (WorldActivityDTO [slice])) of
          Left e → expectationFailure (show e)
          Right dto →
            case applyWorldActivity 4 dto (HM.singleton fixturePage page) of
              Left errs → expectationFailure (show errs)
              Right pages → case HM.lookup fixturePage pages of
                Nothing → expectationFailure "page missing"
                Just p → do
                  pgsPendingChopMigration p `shouldBe` pendingChop
                  pgsPendingFloraHarvests p `shouldBe` pendingHarv
                  writeIORef (wsPendingChopMigrationRef ws)
                      (pgsPendingChopMigration p)
                  writeIORef (wsPendingFloraHarvestsRef ws)
                      (pgsPendingFloraHarvests p)
                  _ ← admitChunkFlora ws probeCatalog logger
                          (chunkWith coTenants)
                  HM.keys <$> readIORef (wsChopDesignationsRef ws)
                    `shouldReturn` [oakIid]
                  HM.size <$> readIORef (wsFloraHarvestsRef ws)
                    `shouldReturn` 2

    it "forgets a plant the post-admission passes shed, so a legacy entry \
       \admission just resolved cannot outlive it" $ \(env, _) → do
      -- Admission runs BEFORE a chunk is inserted (requirement 15), but
      -- the dig / build-progress corner-mask passes that follow the
      -- insert shed a progressed tile's rooted flora. Most of the time
      -- that plant's state was already cleared when the dig happened —
      -- but a PENDING legacy entry admission has only just resolved onto
      -- it would be left addressing a plant the same transaction removed.
      ws ← resetPageEmpty env
      logger ← readIORef (loggerRef env)
      writeIORef (wsPendingChopMigrationRef ws)
          (HM.singleton homeTile (ChopDesignation zSlice
              (fst homeTile) (snd homeTile)))
      writeIORef (wsPendingFloraHarvestsRef ws) (HM.singleton homeTile 88.5)
      admitted ← admitChunkFlora ws probeCatalog logger (chunkWith coTenants)
      HM.keys <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` [oakIid]
      -- What a corner-mask pass leaves behind: the oak is gone from the
      -- committed tile data, its co-tenants are not.
      let survivors = filter ((≢ oakIid) . fiInstanceId) coTenants
      writeIORef (wsTilesRef ws) (tilesWith survivors)
      readIORef (wsTilesRef ws) ⌦ forgetFloraDroppedSince ws [admitted]
      HM.null <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` True
      HM.keys <$> readIORef (wsFloraHarvestsRef ws) `shouldReturn` [berryIid]

    it "leaves an EVICTED chunk's plants alone — eviction is not removal" $
        \(env, _) → do
      -- The control the sweep above must not break: a chunk that leaves
      -- the tile map in the same transaction was evicted, and an evicted
      -- plant still exists. Its designation and timer are world-level
      -- precisely so they survive that.
      ws ← resetPage env coTenants
      logger ← readIORef (loggerRef env)
      designateChopInstances ws [(oakIid, fst homeTile, snd homeTile, zSlice)]
      writeIORef (wsFloraHarvestsRef ws) (HM.singleton berryIid 60)
      admitted ← admitChunkFlora ws probeCatalog logger (chunkWith coTenants)
      writeIORef (wsTilesRef ws)
          WorldTileData { wtdChunks = HM.empty, wtdMaxChunks = 200 }
      readIORef (wsTilesRef ws) ⌦ forgetFloraDroppedSince ws [admitted]
      HM.keys <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` [oakIid]
      HM.keys <$> readIORef (wsFloraHarvestsRef ws) `shouldReturn` [berryIid]

    it "discards a legacy entry whose resolved tile holds no eligible \
       \flora" $ \(env, _) → do
      ws ← resetPageEmpty env
      logger ← readIORef (loggerRef env)
      writeIORef (wsPendingChopMigrationRef ws)
          (HM.singleton homeTile (ChopDesignation zSlice
              (fst homeTile) (snd homeTile)))
      writeIORef (wsPendingFloraHarvestsRef ws) (HM.singleton homeTile 88.5)
      -- The tile resolves — its chunk arrives — carrying only the
      -- decorative fern, which is neither wood-tagged nor harvestable.
      _ ← admitChunkFlora ws probeCatalog logger
              (chunkWith [instanceAt homeTile fernId 0])
      HM.null <$> readIORef (wsPendingChopMigrationRef ws) `shouldReturn` True
      HM.null <$> readIORef (wsPendingFloraHarvestsRef ws) `shouldReturn` True
      HM.null <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` True
      HM.null <$> readIORef (wsFloraHarvestsRef ws) `shouldReturn` True

  where
    setup = do
        EngineInitResult env ← initializeEngineHeadless
        ls ← newBareLuaBackend env
        pure (env, ls)

-- | The id as Lua sees it. The whole space is a positive Int64 by
--   construction, which is what makes this a plain number in every
--   script, log line and console reply.
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
