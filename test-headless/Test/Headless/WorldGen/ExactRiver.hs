{-# LANGUAGE Strict #-}
module Test.Headless.WorldGen.ExactRiver (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import qualified Data.ByteString as BS
import World.Save.Envelope (decodeSessionEnvelope)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Fluid.Exact
import World.Fluid.Types (FluidCell(..), FluidType(River, Ocean), fluidVolumeOverTerrain)
import World.Fluid.OceanMask (WorldOceanMask(..))
import World.Fluid.River.Types (WorldRivers(..), RiverChunkEntry(..), emptyWorldRivers)
import World.Fluid.River.Identify.Common
import World.Fluid.River.Identify.Surface
import World.Fluid.River.Identify.SurfaceFlow (resolveSurfaceFlow)
import World.Generate.Chunk.Fluid (composeFluidMap, chunkWaterSurfMap, applyBasaltCaps, lakeSurfaceMap)
import World.Magma.Init (discoverChunkLava)
import World.Magma.Pool (identifyLavaPools)
import World.Fluid.Lake.Types (emptyWorldLakes, WorldLakes(..))
import World.Magma.Types
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Generate.Types
import World.Geology.Timeline.Types
    (GeoTimeline(..), VolcanicFeature(..), FissureParams(..), FeatureActivity(..), EventBBox(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Save.Component.PageCore
import World.Generate.Chunk.RiverBed (fitExactRiverBeds)
import World.Save.Component.WorldGen (toWorldGenParamsDTOv9, toWorldGenParamsDTOv1)
import World.Save.Snapshot (PageSnapshot(..), SessionSnapshot(..))
import World.Save.Component.Types (ComponentCodec(..))
import Test.Headless.World.Save.Components.Fixture
    (richSnapshot, minimalSaveDataV90, minimalSaveMetadataV90, minimalWorldPageSaveV90)
import World.Save.Compat.SessionV90
    (SaveDataV90(..), WorldPageSaveV90(..), migrateSessionV90)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Page.Types (WorldPageId(..))

-- Synthetic final footprints use the production final solver, not a
-- reimplementation of its allocation or relaxation order.
solve ∷ [(Int,Int)] → [(Int,Word8)] → [(Int,Int)] → [[Int]] → VU.Vector Int
solve heights directions sections paths = exactRiverSurfaces 8
    (VU.replicate 64 dirNone VU.// directions)
    (VU.generate 64 (\i → i `elem` map fst directions))
    (VU.generate 64 (\i → i `elem` map fst heights))
    (VU.replicate 64 minBound VU.// heights) sections paths 1

coord ∷ ChunkCoord
coord = ChunkCoord 0 0

rivers ∷ [Int] → WorldRivers
rivers surfaces = emptyWorldRivers
    { wrByChunk = HM.singleton coord $ V.singleton $ RiverChunkEntry
        0 (VU.generate 256 (< length surfaces))
        (VU.fromList (take 256 (surfaces <> repeat minBound)))
        (VU.replicate 256 2)
    , wrCarveDelta = HM.singleton coord (VU.replicate 256 3)
    }
params ∷ [Int] → WorldGenParams
params surfaces = defaultWorldGenParams
    { wgpGeoTimeline = (wgpGeoTimeline defaultWorldGenParams)
        { gtWorldRivers = rivers surfaces } }

legacy ∷ WorldPagesDTOv12
legacy = WorldPagesDTOv12 [PageCoreDTOv12
    (WorldPageId "river") (toWorldGenParamsDTOv9 (params [3,0,-2]))
    1 2 3 4 0.375 5 6 7 ZMDefault Nothing (Just (fixtureGeneratedWorldIdForPage (WorldPageId "river")))]

spec ∷ Spec
spec = describe "WorldGen.ExactRiver" $ do
    it "repairs only new-world river beds after smoothing, leaving banks and old saves alone" $ do
        let p = params [80]
            bed = VU.replicate 256 10
            repaired = fitExactRiverBeds p coord 0 bed
        repaired VU.! 0 `shouldBe` 9
        VU.tail repaired `shouldBe` VU.tail bed
        fitExactRiverBeds (p { wgpExactRiverBeds = False }) coord 0 bed `shouldBe` bed
    it "distributes a known one-z descent into several eighth steps" $ do
        let result = solve [(9,1),(10,1),(11,1),(12,0)]
                [(9,dirEast),(10,dirEast),(11,dirEast),(12,dirNone)] [] []
        map (result VU.!) [9,10,11,12] `shouldBe` [3,2,1,0]
        result VU.! 0 `shouldBe` minBound
    it "pins even a short breakthrough to the exact ocean plane" $ do
        let result = solve [(9,2),(10,1)] [(9,dirNone)] [] [[9,10]]
        result VU.! 10 `shouldBe` exactSurfaceOfZ 0
        result VU.! 9 `shouldSatisfy` (≤ exactSurfaceOfZ 1)
    it "routes an overwritten centre through a level wing to a lower junction" $ do
        let hs = [(9,2),(10,3),(17,2),(25,1)]
            mask = VU.generate 64 (\i → i `elem` map fst hs)
            planes = VU.replicate 64 minBound VU.// hs
            raw = [(9,10),(10,9)]
        resolveSurfaceFlow 8 mask planes raw `shouldBe` [(9,17),(10,9),(17,25)]
        resolveSurfaceFlow 8 mask planes (reverse raw)
            `shouldBe` resolveSurfaceFlow 8 mask planes raw
        let result = solve hs [(9,dirEast),(10,dirWest)] [] []
        forM_ [(9,17),(10,9),(17,25)] $ \(a,b) →
            result VU.! a `shouldSatisfy` (≥ result VU.! b)
        forM_ hs $ \(i,h) → result VU.! i `shouldSatisfy` (≥ h*8-7)
    it "routes a junction across wrapped adjacency and preserves ordinary branches" $ do
        let hs = [(15,2),(8,3),(23,2),(22,1)]
            mask = VU.generate 64 (\i → i `elem` map fst hs)
            planes = VU.replicate 64 minBound VU.// hs
        resolveSurfaceFlow 8 mask planes [(15,8),(8,15)]
            `shouldBe` [(8,15),(15,23),(23,22)]
        resolveSurfaceFlow 8 mask (planes VU.// [(15,3),(8,2)]) [(15,8),(15,23)]
            `shouldBe` [(15,8),(15,23)]
    it "retains a closed junction pool without excavating its higher neighbour" $ do
        let hs = [(9,1),(10,2),(11,2)]
            result = solve hs [(9,dirEast),(10,dirEast),(11,dirNone)] [] []
        map (result VU.!) [9,10,11] `shouldBe` [8,16,16]
    it "keeps compatible wings flat without flattening conflicting overlap claims" $ do
        let hs = [(9,3),(10,2),(17,3),(18,2),(11,1)]
            result = solve hs [(9,dirEast),(10,dirEast),(11,dirNone)]
                [(9,17),(10,17),(10,18)] []
        result VU.! 9 `shouldBe` result VU.! 17
        result VU.! 10 `shouldBe` result VU.! 18
        result VU.! 9 `shouldSatisfy` (> result VU.! 10)
        forM_ hs $ \(i,h) → result VU.! i `shouldSatisfy` (≥ h*8-7)
    it "keeps two converging reaches downhill without flattening different planes" $ do
        let result = solve [(9,3),(10,2),(18,2),(11,1)]
                [(9,dirEast),(18,dirNorth),(10,dirEast),(11,dirNone)]
                [(9,18),(10,18)] []
        forM_ [(9,10),(18,10),(10,11)] $ \(a,b) →
            result VU.! a `shouldSatisfy` (≥ result VU.! b)
        result VU.! 10 `shouldBe` result VU.! 18
    it "bounds nearby non-flow-linked reaches, including wrapped adjacency" $ do
        let result = solve [(8,1),(15,8),(16,1),(23,8)]
                [(8,dirNone),(15,dirNone)] [(15,23)] []
        abs (result VU.! 15 - result VU.! 8) `shouldSatisfy` (≤ fluidUnitsPerZ)
        result VU.! 15 `shouldBe` result VU.! 23
    it "does not depend on width-claim order" $ do
        let hs = [(9,3),(10,2),(17,1)]
            ds = [(9,dirEast),(10,dirNone)]
            es = [(9,17),(10,17)]
        solve hs ds es [] `shouldBe` solve hs ds (reverse es) []
    it "preserves neighbouring slope bounds after an overlapping section lowers" $ do
        let result = solve [(9,4),(10,3),(18,3)]
                [(9,dirEast),(10,dirNone)] [(10,18)] []
        abs (result VU.! 9 - result VU.! 10) `shouldSatisfy` (≤ 8)
        result VU.! 10 `shouldBe` result VU.! 18
    it "composes a fractional river directly and compares terrain in exact units" $ do
        let p = params [81,79]
            cells = composeFluidMap p coord (VU.replicate 256 10)
        cells V.! 0 `shouldBe` Just (FluidCell River 81)
        cells V.! 1 `shouldBe` Nothing
        chunkWaterSurfMap p coord VU.! 0 `shouldBe` 11
    it "migrates positive, zero and negative v12 surfaces, preserving absent slots and carve" $ do
        let pages = migrateWorldPagesV12 legacy
            page = wpBase pages HM.! WorldPageId "river"
            table = gtWorldRivers (wgpGeoTimeline (pgsGenParams page))
        table `shouldBe` rivers [24,0,-16]
        wgpExactRiverBeds (pgsGenParams page) `shouldBe` False
        pgsTimeRemainder page `shouldBe` 0.375
    it "keeps positive water where Ocean takes priority at the breakthrough sink" $ do
        let p = params [0]
            coastal = p { wgpGeoTimeline = (wgpGeoTimeline p)
                { gtWorldOcean = WorldOceanMask (HM.singleton coord (VU.replicate 256 True)) } }
            cell = composeFluidMap coastal coord (VU.replicate 256 (-1)) V.! 0
        cell `shouldBe` Just (FluidCell Ocean 0)
        fmap (fluidVolumeOverTerrain (-1)) cell `shouldBe` Just 8
    it "seals a magma breach below a fractional river without erasing its depth" $ do
        let source = MagmaSource (GeoFeatureId 1)
                (FissureVolcano (FissureParams (GeoCoord 0 0) (GeoCoord 1 0) 1 1 True))
                FActive (GeoCoord 0 0) [Cylindrical 0 0 0 30 1]
                (EventBBox (-1) (-1) 1 1) 0
            ctx = emptyVolcanoCtx { vcSources = V.singleton source
                , vcIndex = HM.singleton coord [0], vcWorldSize = 32 }
            p = params [81]
            raw = VU.replicate 256 5
            water = chunkWaterSurfMap p coord
        case discoverChunkLava ctx coord raw water of
            Nothing → expectationFailure "fixture did not breach under the river"
            Just overlay → do
                HM.lookup (0,0) (moBasaltCap overlay) `shouldBe` Just 10
                let bed = applyBasaltCaps coord (Just overlay) raw
                    cell = composeFluidMap p coord bed V.! 0
                cell `shouldBe` Just (FluidCell River 81)
                fmap (fluidVolumeOverTerrain (bed VU.! 0)) cell `shouldBe` Just 1
    it "keeps a lava pool out of a selected river whose pre-carve terrain is above water" $ do
        let source = MagmaSource (GeoFeatureId 1)
                (FissureVolcano (FissureParams (GeoCoord 0 0) (GeoCoord 1 0) 1 1 True))
                FActive (GeoCoord 0 0) [Cylindrical 0 0 0 30 2]
                (EventBBox (-2) (-2) 2 2) 0
            ctx = emptyVolcanoCtx { vcSources = V.singleton source
                , vcIndex = HM.singleton coord [0], vcWorldSize = 32 }
            pools = identifyLavaPools 32 6 8 ctx emptyWorldLakes (rivers [1])
                (VU.replicate (512*512) 5)
        V.null (wlLakes pools) `shouldBe` False
        lakeSurfaceMap pools coord VU.! 0 `shouldBe` minBound

    it "round-trips the frozen v12 shape through its real codec" $ do
        let decoded = S.decode (S.encode legacy) ∷ Either String WorldPagesDTOv12
        case decoded of
            Left err → expectationFailure err
            Right old → do
                let table = gtWorldRivers . wgpGeoTimeline . pgsGenParams $
                        wpBase (migrateWorldPagesV12 old) HM.! WorldPageId "river"
                table `shouldBe` rivers [24,0,-16]
    it "current fractional river-table bytes round-trip without another conversion" $ do
        let table = rivers [25,0,-15]
        (S.decode (S.encode table) ∷ Either String WorldRivers) `shouldBe` Right table

    it "resaves either bed policy and fractional water without rescaling" $ forM_ [False,True] $ \policy → do
        case ccDecode worldPagesCodec 12 (S.encode legacy) of
            Left err → expectationFailure (show err)
            Right old → do
                let fractionalPage = (wpBase old HM.! WorldPageId "river")
                        { pgsGenParams = (params [25,0,-15]) { wgpExactRiverBeds = policy } }
                    snapshot = richSnapshot { snapPages = HM.singleton
                        (WorldPageId "river") fractionalPage }
                case ccDecode worldPagesCodec (ccVersion worldPagesCodec)
                        (ccEncode worldPagesCodec snapshot) of
                    Left err → expectationFailure (show err)
                    Right restored → do
                        let table = gtWorldRivers . wgpGeoTimeline . pgsGenParams $
                                wpBase restored HM.! WorldPageId "river"
                        table `shouldBe` rivers [25,0,-15]
                        wgpExactRiverBeds (pgsGenParams (wpBase restored HM.! WorldPageId "river"))
                            `shouldBe` policy

    it "scales the legacy complete-session path exactly once as well" $ do
        let pid = WorldPageId "main_world"
            oldPage = (minimalWorldPageSaveV90 pid)
                { wp90GenParams = toWorldGenParamsDTOv1 (params [3,0,-2]) }
            oldSession = minimalSaveDataV90 { sd90Worlds = [oldPage] }
        case migrateSessionV90 minimalSaveMetadataV90 oldSession of
            Left err → expectationFailure (show err)
            Right restored → do
                let table = gtWorldRivers . wgpGeoTimeline . pgsGenParams $
                        snapPages restored HM.! pid
                table `shouldBe` rivers [24,0,-16]

    it "the tracked v13 session carries nonempty fractional generated rivers" $ do
        bytes ← BS.readFile "test-headless/data/save-compat/z5-exact-generated-rivers.bin"
        let luaNames = HS.fromList ["unit_ai", "building_spawn"]
        case decodeSessionEnvelope luaNames luaNames bytes of
            Left err → expectationFailure (show err)
            Right (_, snapshot, _, _) → do
                let tables = [gtWorldRivers (wgpGeoTimeline (pgsGenParams page))
                             | page ← HM.elems (snapPages snapshot)]
                    surfaces = [rcePerTileSurfZ entry VU.! i | table ← tables,
                        entries ← HM.elems (wrByChunk table), entry ← V.toList entries,
                        i ← [0..255], rceBitmask entry VU.! i]
                surfaces `shouldSatisfy` any (\s → s `mod` fluidUnitsPerZ ≢ 0)
                case ccDecode worldPagesCodec (ccVersion worldPagesCodec)
                        (ccEncode worldPagesCodec snapshot) of
                    Left err → expectationFailure (show err)
                    Right restored →
                        [gtWorldRivers (wgpGeoTimeline (pgsGenParams page))
                        | page ← HM.elems (wpBase restored)] `shouldBe` tables
