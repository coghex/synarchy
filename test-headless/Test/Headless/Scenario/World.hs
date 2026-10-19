{-# LANGUAGE Strict #-}
-- | The world-content adapters (#2711, SCN-11 of epic #2698): terrain,
--   fluid and flora construction, commit and capture, through the actual
--   reconstruction and persistence owners.
--
--   Two halves. The PURE half runs 'Scenario.World.Build' and
--   'Scenario.World.Capture' over the arena's real base and the real
--   edit replay ('World.Edit.Apply.replayEdits'), asserting exact
--   physical values. The LIVE half commits through
--   'Scenario.World.Install' onto a page and reads the page's own
--   owners back.
module Test.Headless.Scenario.World (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Scenario.World.Build
import Scenario.World.Capture
import Scenario.World.Flora (baselineForAge, derivedOccurrenceAge, derivedPlotAge)
import Scenario.World.Terrain
import Scenario.World.Types (WorldRefs(..))
import Scenario.Types
import World.Arena.Bounds (ArenaBounds)
import World.Chunk.Types
    ( ChunkCoord(..), LoadedChunk(..), columnIndex )
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Flora.CropPlot (CropPlotOf(..), cropPlotElapsedDays)
import World.Flora.Growth (FloraGrowth(..), floraGrowth, floraTotalAgeAt, growthRate)
import World.Flora.Identity
    ( firstPlantedFloraCursor, isPlantedFloraInstanceId
    , nextPlantedFloraCursor )
import World.Flora.Types
import World.Fluid.Exact (exactSurfaceOfZ)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Generate.Arena (generateArenaChunkIn, absentSurfaceZ)
import World.Generate.Coordinates (globalToChunk)
import World.Material (matGranite, matLoam, matSand)
import World.Material.Id (MaterialId(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Scenario.World.Fixtures
import qualified Test.Headless.Scenario.World.Live as Live

spec ∷ Spec
spec = do
    terrainSpec
    fluidSpec
    floraSpec
    captureSpec
    -- the live half: its own engine (it replaces the flora catalog and
    -- creates private arena pages)
    aroundAll withHeadlessEngine Live.spec

graniteW, loamW, sandW ∷ Word8
graniteW = unMaterialId matGranite
loamW = unMaterialId matLoam
sandW = unMaterialId matSand

-- | The (path, effect) pairs of a diagnostic list.
effects ∷ [ScenarioDiagnostic] → [(Text, DiagnosticEffect)]
effects = map (\d → (sdPath d, sdEffect d))

editTiles ∷ WorldEdits → [(Int, Int)]
editTiles es = Set.toAscList $ Set.fromList
    [ editTile e | chunkEs ← HM.elems es, e ← chunkEs ]

-- * Terrain

terrainSpec ∷ Spec
terrainSpec = describe "terrain" $ do

    it "raises a pad, digs a column, replaces a top cell and ramps one, over the real base" $ do
        let c = content Nothing 0
                [ terrainPatch "pad" (RegionRect 0 0 1 1) "granite" 3 Omitted []
                , terrainPatch "pit" (RegionTiles [(5, 5)]) "sand" (-2) Omitted []
                , terrainPatch "cap" (RegionTiles [(7, 7)]) "granite" 0 (Authored 5) [] ]
                [] []
            es = wcEdits c
        wcDiagnostics c `shouldBe` []
        forM_ [(0, 0), (1, 0), (0, 1), (1, 1)] $ \t →
            at Nothing es t `shouldBe` (3, Just graniteW, 0, 0)
        at Nothing es (5, 5) `shouldBe` (-2, Just sandW, 0, 0)
        at Nothing es (7, 7) `shouldBe` (0, Just graniteW, 5, 0)

    it "leaves an untouched column exactly as the base builds it, grass included" $ do
        let c = content Nothing 0 [terrainPatch "pad" (RegionTiles [(0, 0)]) "granite" 2 Omitted []] [] []
            (top, m, slope, veg) = at Nothing (wcEdits c) (9, 9)
            (_, _, _, baseVeg) = columnAt (generateArenaChunkIn Nothing seed (ChunkCoord 0 0)) (9, 9)
        (top, m, slope) `shouldBe` (0, Just loamW, 0)
        veg `shouldBe` baseVeg
        veg `shouldSatisfy` (> 0)

    it "produces no edit for a column that already is the stated one, and keeps its grass" $ do
        let c = content Nothing 0
                [terrainPatch "same" (RegionRect 0 0 3 3) "loam" 0 (Authored 0) []] [] []
        wcEdits c `shouldBe` HM.empty
        wcDiagnostics c `shouldBe` []

    it "reaches negative coordinates and distinct z cells in the right chunks" $ do
        let c = content Nothing 0
                [ terrainPatch "a" (RegionTiles [(-3, -2)]) "sand" 4 Omitted []
                , terrainPatch "b" (RegionTiles [(-17, 20)]) "granite" (-9) Omitted [] ]
                [] []
            es = wcEdits c
        HM.keys es `shouldMatchList` [ChunkCoord (-1) (-1), ChunkCoord (-2) 1]
        at Nothing es (-3, -2) `shouldBe` (4, Just sandW, 0, 0)
        at Nothing es (-17, 20) `shouldBe` (-9, Just graniteW, 0, 0)

    it "builds edits from each tile's FINAL state: a later patch replaces, never stacks" $ do
        let c = content Nothing 0
                [ terrainPatch "wide" (RegionRect 0 0 2 0) "granite" 4 Omitted []
                , terrainPatch "narrow" (RegionTiles [(1, 0)]) "sand" 1 Omitted [] ]
                [] []
            es = wcEdits c
        at Nothing es (0, 0) `shouldBe` (4, Just graniteW, 0, 0)
        at Nothing es (1, 0) `shouldBe` (1, Just sandW, 0, 0)
        at Nothing es (2, 0) `shouldBe` (4, Just graniteW, 0, 0)
        length [ () | chunkEs ← HM.elems es, e@(WeAddTile 1 0 _) ← chunkEs, e ≡ e ] `shouldBe` 1

    it "clips a patch tile by tile at a finite edge and a partial chunk" $ do
        let b = Just (bounds 20 10)
            c = content b 0 [terrainPatch "big" (RegionRect (-12) (-6) 11 7) "granite" 2 Omitted []] [] []
            es = wcEdits c
            (x0, x1, y0, y1) = (-10, 9, -4, 5)
        effects (wcDiagnostics c) `shouldBe` [("terrain[0]", TilesClipped)]
        map sdReason (wcDiagnostics c) `shouldBe` [ClippedTiles (24 * 14 - 200)]
        forM_ (editTiles es) $ \(x, y) →
            (x ≥ x0 ∧ x ≤ x1 ∧ y ≥ y0 ∧ y ≤ y1) `shouldBe` True
        length (editTiles es) `shouldBe` 200
        -- the partial chunk (0,0) holds raised in-rectangle columns, and
        -- the outside ones stay ABSENT
        let lc = chunkOf b es (ChunkCoord 0 0)
        columnAt lc (9, 5) `shouldBe` (2, Just graniteW, 0, 0)
        lcTerrainSurfaceMap lc VU.! columnIndex 10 0 `shouldBe` absentSurfaceZ
        lcTerrainSurfaceMap lc VU.! columnIndex 0 6 `shouldBe` absentSurfaceZ

    it "rejects a patch with no tile inside the map, leaving no edit and no identity" $ do
        let c = content (Just (bounds 4 4)) 0
                [terrainPatch "gone" (RegionRect 50 50 60 60) "granite" 2 Omitted [tag "t"]] [] []
        effects (wcDiagnostics c) `shouldBe` [("terrain[0]", EntryRejected)]
        map sdReason (wcDiagnostics c) `shouldBe` [OutsideBounds]
        wcEdits c `shouldBe` HM.empty
        wrTerrain (wcRefs c) `shouldBe` []
        wcTileTags c `shouldBe` []

    it "rejects an unknown or air material and a surface outside the column's window" $ do
        let c = content Nothing 0
                [ terrainPatch "unk" (RegionTiles [(0, 0)]) "unobtainium" 1 Omitted []
                , terrainPatch "air" (RegionTiles [(1, 0)]) "air" 1 Omitted []
                , terrainPatch "deep" (RegionTiles [(2, 0)]) "granite" (baseFloorZ - 1) Omitted []
                , terrainPatch "high" (RegionTiles [(3, 0)]) "granite" (maxSurfaceZ + 1) Omitted []
                , terrainPatch "ok" (RegionTiles [(4, 0)]) "granite" baseFloorZ Omitted [] ]
                [] []
        effects (wcDiagnostics c) `shouldBe`
            [ ("terrain[0]", EntryRejected), ("terrain[1]", EntryRejected)
            , ("terrain[2]", EntryRejected), ("terrain[3]", EntryRejected) ]
        map sdReason (take 1 (wcDiagnostics c)) `shouldBe` [UnknownDefinition "unobtainium"]
        editTiles (wcEdits c) `shouldBe` [(4, 0)]
        at Nothing (wcEdits c) (4, 0) `shouldBe` (baseFloorZ, Just graniteW, 0, 0)

    it "refuses a region beyond the per-family tile budget rather than attempting it" $ do
        let c = content Nothing 0
                [ terrainPatch "huge" (RegionRect 0 0 2000 2000) "granite" 1 Omitted []
                , terrainPatch "small" (RegionTiles [(0, 0)]) "granite" 1 Omitted [] ]
                [] []
        effects (wcDiagnostics c) `shouldBe` [("terrain[0]", EntryRejected)]
        editTiles (wcEdits c) `shouldBe` [(0, 0)]

    it "bounds the cells a patch may change, not just its tiles" $ do
        let c = content Nothing 0
                [ terrainPatch "tall" (RegionRect 0 0 999 999) "granite" 200 Omitted []   -- 1M tiles × 201
                , terrainPatch "ok" (RegionRect 0 0 99 99) "granite" 100 Omitted [] ]     -- 10k tiles × 101
                [] []
        effects (wcDiagnostics c) `shouldBe` [("terrain[0]", EntryRejected)]
        length (editTiles (wcEdits c)) `shouldBe` 10000
        maxMaterializedCells `shouldBe` 4194304

    it "labels the surviving top cell with the patch's tags, and only where it survives" $ do
        let c = content Nothing 0
                [ terrainPatch "tagged" (RegionRect 0 0 1 0) "granite" 3 Omitted [tag "mark"]
                , terrainPatch "lower" (RegionTiles [(1, 0)]) "sand" 1 Omitted [] ]
                [] []
        map fst (wcTileTags c) `shouldBe` [(0, 0, 3)]
        map snd (wcTileTags c) `shouldBe` [Set.fromList [tag "mark"]]
        effects (wcDiagnostics c) `shouldBe` [("terrain[0]", FieldRejected)]
        wrTerrain (wcRefs c) `shouldBe` [(ex "tagged", [(0, 0, 3)]), (ex "lower", [(1, 0, 1)])]

-- * Fluid

fluidSpec ∷ Spec
fluidSpec = describe "fluid" $ do

    let wetCell lc t = let (_, (lx, ly)) = globalToChunk (fst t) (snd t)
                       in lcFluidMap lc V.! columnIndex lx ly

    it "records the exact fractional plane, never a whole z or a full cell" $ do
        let c = content Nothing 0 [] [fluidPatch "pond" (RegionRect 0 0 1 1) FluidLake 35] []
            lc = chunkOf Nothing (wcEdits c) (ChunkCoord 0 0)
        wcDiagnostics c `shouldBe` []
        forM_ [(0, 0), (1, 1)] $ \t →
            wetCell lc t `shouldBe` Just (FluidCell Lake 35)
        wetCell lc (2, 2) `shouldBe` Nothing

    it "is wet only above the tile's FINAL terrain top: dry at or below it" $ do
        let c = content Nothing 0
                [terrainPatch "hill" (RegionTiles [(1, 0)]) "granite" 5 Omitted []]
                [fluidPatch "pond" (RegionRect 0 0 2 0) FluidRiver (exactSurfaceOfZ 3)] []
            lc = chunkOf Nothing (wcEdits c) (ChunkCoord 0 0)
        wetCell lc (0, 0) `shouldBe` Just (FluidCell River (exactSurfaceOfZ 3))
        wetCell lc (1, 0) `shouldBe` Nothing
        wetCell lc (2, 0) `shouldBe` Just (FluidCell River (exactSurfaceOfZ 3))
        -- the plane EXACTLY at the terrain top holds no volume either
        let flat = content Nothing 0 [] [fluidPatch "film" (RegionTiles [(0, 0)]) FluidLake 0] []
        wetCell (chunkOf Nothing (wcEdits flat) (ChunkCoord 0 0)) (0, 0) `shouldBe` Nothing

    it "lets a later fluid patch replace an earlier one, and a dry one clear it" $ do
        let c = content Nothing 0 []
                [ fluidPatch "a" (RegionRect 0 0 1 0) FluidLake 40
                , fluidPatch "b" (RegionTiles [(0, 0)]) FluidOcean 24
                , fluidPatch "dry" (RegionTiles [(1, 0)]) FluidLake (-8) ] []
            lc = chunkOf Nothing (wcEdits c) (ChunkCoord 0 0)
        wetCell lc (0, 0) `shouldBe` Just (FluidCell Ocean 24)
        wetCell lc (1, 0) `shouldBe` Nothing

    it "rejects a fluid deeper than the simulation's volume range, with overflow-safe arithmetic" $ do
        let c = content Nothing 0 []
                [ fluidPatch "abyss" (RegionTiles [(0, 0)]) FluidLake (fromIntegral maxFluidDepthUnits + 1)
                , fluidPatch "edge" (RegionTiles [(1, 0)]) FluidLake (fromIntegral maxFluidDepthUnits)
                , fluidPatch "huge" (RegionTiles [(2, 0)]) FluidLake maxBound ] []
        effects (wcDiagnostics c) `shouldBe` [("fluids[0]", EntryRejected), ("fluids[2]", EntryRejected)]
        [ (x, y) | es ← HM.elems (wcEdits c), WeSetFluidSnapshot x y _ _ ← es ] `shouldBe` [(1, 0)]

    it "clears a dug tile explicitly, so the replay cannot reveal groundwater the scenario does not state" $ do
        let c = content Nothing 0
                [ terrainPatch "pit" (RegionRect 0 0 1 0) "sand" (-4) Omitted [] ] [] []
            lc = chunkOf Nothing (wcEdits c) (ChunkCoord 0 0)
            unCleared = chunkOf Nothing
                (HM.map (filter (\e → case e of WeClearFluidSnapshot{} → False; _ → True))
                        (wcEdits c)) (ChunkCoord 0 0)
        wetCell lc (0, 0) `shouldBe` Nothing
        wetCell lc (1, 0) `shouldBe` Nothing
        -- without the explicit clear the dug column below the water table is wet
        wetCell unCleared (0, 0) `shouldSatisfy` isJust
        -- and a pond stated over the pit still wins
        let wet = content Nothing 0
                [ terrainPatch "pit" (RegionTiles [(0, 0)]) "sand" (-4) Omitted [] ]
                [ fluidPatch "pond" (RegionTiles [(0, 0)]) FluidLake (exactSurfaceOfZ (-1)) ] []
        wetCell (chunkOf Nothing (wcEdits wet) (ChunkCoord 0 0)) (0, 0)
            `shouldBe` Just (FluidCell Lake (exactSurfaceOfZ (-1)))

    it "rejects fluid tags (fluid is not a tag target) without dropping the patch" $ do
        let p = (fluidPatch "tagged" (RegionTiles [(0, 0)]) FluidLava 16) { fpTags = [tag "hot"] }
            c = content Nothing 0 [] [p] []
        effects (wcDiagnostics c) `shouldBe` [("fluids[0].tags", FieldRejected)]
        wetCell (chunkOf Nothing (wcEdits c) (ChunkCoord 0 0)) (0, 0)
            `shouldBe` Just (FluidCell Lava 16)

    it "clips fluid at a finite edge exactly as terrain, closing nothing it did not keep" $ do
        let b = Just (bounds 20 10)
            c = content b 0 [] [fluidPatch "sea" (RegionRect (-12) (-6) 11 7) FluidOcean 8] []
        map sdReason (wcDiagnostics c) `shouldBe` [ClippedTiles 136]
        length (editTiles (wcEdits c)) `shouldBe` 200

-- * Flora

authoredOak ∷ FloraEntry
authoredOak = (floraEntry "oak1" "oak" 3 4)
    { feTags = [tag "grove"], feZ = Authored 0, feAge = Authored 120, feHealth = Authored 0.5
    , feOffsetU = Authored (-0.25), feOffsetV = Authored 0.125, feVariant = Authored 2
    , feRegrowth = Authored 90 }

floraSpec ∷ Spec
floraSpec = describe "flora" $ do

    it "reconstructs an occurrence's exact physical state from the durable edits" $ do
        let c = content Nothing 400 [] [] [authoredOak]
            o = sole (occurrencesIn Nothing (wcEdits c))
            sp = speciesOf oakId
        wcDiagnostics c `shouldBe` []
        fiSpecies o `shouldBe` oakId
        (fiOffU o, fiOffV o, fiVariant o, fiHealth o, fiZ o) `shouldBe` (-0.25, 0.125, 2, 0.5, 0)
        fiBaseWidth o `shouldBe` 16
        isPlantedFloraInstanceId (fiInstanceId o) `shouldBe` True
        fiInstanceId o `shouldBe` fst (nextPlantedFloraCursor firstPlantedFloraCursor)
        -- the CURRENT age at the initialized clock is the authored one
        derivedOccurrenceAge 400 o `shouldBe` 120
        fgAge (floraGrowth sp 400 o) `shouldBe` 120
        -- ...and it grows from there at the health-scaled rate
        fgAge (floraGrowth sp 410 o) `shouldBe` 120 + 10 * growthRate 0.5
        wcHarvests c `shouldBe` [(fiInstanceId o, 90)]
        wcCursor c `shouldBe` firstPlantedFloraCursor + 1
        wcOccurrenceTags c `shouldBe` [(fiInstanceId o, (3, 4), Set.fromList [tag "grove"])]

    it "derives the authored age at any clock and health (exactly at day 0, to the float grid after), never the stored baseline" $
        forM_ [0, 0.1, 7.3, 360.5, 1234.567] $ \age →
          forM_ [1, 0.5, 0.1, 0] $ \health →
            forM_ [0, 1, 400, 20000] $ \day → do
                let e = authoredOak { feAge = Authored age, feHealth = Authored health }
                    o = sole (occurrencesIn Nothing (wcEdits (content Nothing day [] [] [e])))
                    rate = growthRate health
                    dr = fromIntegral day * rate
                    derived = derivedOccurrenceAge day o
                    slack = 2.5e-7 * (1 + abs (fiAge o) + abs dr)
                if day ≡ 0
                    then derived `shouldBe` age
                    else abs (derived - age) `shouldSatisfy` (≤ slack)
                -- the derived value is ON the grid: asking for it again is exact
                floraTotalAgeAt (baselineForAge health day derived) health day `shouldBe` derived

    it "keeps an explicit zero authoritative: no default fills it, no regrowth timer runs" $ do
        let e = (floraEntry "z" "oak" 0 0)
                { feAge = Authored 0, feHealth = Authored 0, feOffsetU = Authored 0
                , feOffsetV = Authored 0, feVariant = Authored 0, feRegrowth = Authored 0 }
            c = content Nothing 50 [] [] [e]
            o = sole (occurrencesIn Nothing (wcEdits c))
        (fiOffU o, fiOffV o, fiVariant o, fiHealth o) `shouldBe` (0, 0, 0, 0)
        derivedOccurrenceAge 50 o `shouldBe` 0
        wcHarvests c `shouldBe` []

    it "fills an omission from a private hash of the scenario id: stable per id, different between ids" $ do
        let stateOf i = let c = content Nothing 0 [] [] [floraEntry i "oak" 2 2]
                            o = sole (occurrencesIn Nothing (wcEdits c))
                        in (fiOffU o, fiOffV o, fiVariant o, fiAge o)
            ids = ["a", "b", "c", "d", "e", "f"]
        map stateOf ids `shouldBe` map stateOf ids
        length (Set.fromList (map (show ∘ stateOf) ids)) `shouldSatisfy` (> 1)
        -- never consumes gameplay randomness: a full-health default
        let c = content Nothing 0 [] [] [floraEntry "a" "oak" 2 2]
            o = sole (occurrencesIn Nothing (wcEdits c))
        fiHealth o `shouldBe` 1

    it "gives two same-tile occurrences distinct identities and keeps both" $ do
        let c = content Nothing 0 [] []
                [ floraEntry "p" "oak" 5 5, floraEntry "q" "fern" 5 5, floraEntry "r" "oak" 5 5 ]
            os = occurrencesIn Nothing (wcEdits c)
        length os `shouldBe` 3
        length (Set.fromList (map fiInstanceId os)) `shouldBe` 3
        map (\o → (fiTileX o, fiTileY o)) os `shouldBe` replicate 3 (5, 5)
        map snd (wrOccurrences (wcRefs c)) `shouldMatchList` map fiInstanceId os

    it "keeps a mortal plant's lifecycle through the age conversion: a dead plant stays dead, a reseeded one keeps its generation" $
        forM_ [ (365, True, 0), (430, False, 1), (780, True, 1), (10, False, 0) ] $ \(age, dead, gen) → do
            -- the Annual 'beet' lives 360 days and spends 60 dead before it reseeds
            let e = (floraEntry "m" "beet" 2 2) { feAge = Authored age, feHealth = Authored 1 }
                c = content Nothing 100 [] [] [e]
                o = sole (occurrencesIn Nothing (wcEdits c))
                g = floraGrowth (speciesOf (FloraId 3)) 100 o
                cw = captureWorldContent Set.empty (snapshotOf Nothing 100 c False)
                c' = buildWorldContent (envAt Nothing 100 firstPlantedFloraCursor) []  []
                        (withPaths "flora" (cwFlora cw))
                o' = sole (occurrencesIn Nothing (wcEdits c'))
            (fgDead g, fgGeneration g) `shouldBe` (dead, gen)
            -- capture exports the TOTAL growth, and the plant survives its own round trip
            map feAge (cwFlora cw) `shouldBe` [Authored age]
            floraGrowth (speciesOf (FloraId 3)) 100 o' `shouldBe` g

    it "rejects an authored z other than the column's top: a plant stands there whatever its z says" $ do
        let c = content Nothing 0
                [terrainPatch "hill" (RegionTiles [(1, 1)]) "granite" 3 Omitted []] []
                [ (floraEntry "off" "oak" 1 1) { feZ = Authored 5 }
                , (floraEntry "on" "oak" 1 1) { feZ = Authored 3 } ]
            os = occurrencesIn Nothing (wcEdits c)
        effects (wcDiagnostics c) `shouldBe` [("flora[0].z", FieldRejected)]
        map fiZ os `shouldBe` [3, 3]

    it "stands a plant on its tile's FINAL terrain top by default" $ do
        let c = content Nothing 0
                [terrainPatch "hill" (RegionTiles [(1, 1)]) "granite" 3 Omitted []] []
                [floraEntry "t" "oak" 1 1]
            o = sole (occurrencesIn Nothing (wcEdits c))
        fiZ o `shouldBe` 3

    it "builds a groundcover species as a crop plot with the plot owner's own growth" $ do
        let e = (floraEntry "w" "wheat" 6 6)
                { feTags = [tag "field"], feAge = Authored 10, feHealth = Authored 1 }
            c = content Nothing 100 [] [] [e]
        wcDiagnostics c `shouldBe` []
        occurrencesIn Nothing (wcEdits c) `shouldBe` []
        case wcCropPlots c of
            [((6, 6), plot)] → do
                cpSpecies plot `shouldBe` wheatId
                cpPlantedDay plot `shouldBe` 90
                cropPlotElapsedDays 100 plot `shouldBe` 10
                derivedPlotAge 100 plot `shouldBe` 10
                cpHealth plot `shouldBe` 1
            other → expectationFailure ("expected one plot, got " ⧺ show other)
        wcPlotTags c `shouldBe` [((6, 6), Set.fromList [tag "field"])]
        wcCursor c `shouldBe` firstPlantedFloraCursor      -- a plot consumes no id

    it "rejects a field that does not apply to a crop plot, keeping the plot" $ do
        let e = (floraEntry "w" "wheat" 6 6)
                { feOffsetU = Authored 0.1, feVariant = Authored 1, feRegrowth = Authored 5
                , feZ = Authored 9 }
            c = content Nothing 0 [] [] [e]
        effects (wcDiagnostics c) `shouldBe`
            [ ("flora[0].offset_u", FieldRejected), ("flora[0].variant", FieldRejected)
            , ("flora[0].regrowth", FieldRejected), ("flora[0].z", FieldRejected) ]
        length (wcCropPlots c) `shouldBe` 1

    it "rejects a second crop plot on one tile as an entry" $ do
        let c = content Nothing 0 [] []
                [floraEntry "w1" "wheat" 6 6, floraEntry "w2" "wheat" 6 6]
        effects (wcDiagnostics c) `shouldBe` [("flora[1]", EntryRejected)]
        map fst (wcCropPlots c) `shouldBe` [(6, 6)]

    it "accepts a regrowth timer only on a harvestable species" $ do
        let c = content Nothing 0 [] []
                [ (floraEntry "f" "fern" 1 1) { feRegrowth = Authored 5 }
                , (floraEntry "b" "beet" 2 2) { feRegrowth = Authored 40 } ]
            os = occurrencesIn Nothing (wcEdits c)
        effects (wcDiagnostics c) `shouldBe` [("flora[0].regrowth", FieldRejected)]
        length os `shouldBe` 2
        map snd (wcHarvests c) `shouldBe` [40]

    it "rejects an unknown species, an out-of-bounds plant and a plot with no relocation or id" $ do
        let b = Just (bounds 20 10)
            c = content b 0 [] []
                [ floraEntry "x" "mandrake" 0 0
                , floraEntry "o" "oak" 50 50
                , floraEntry "ok" "oak" 1 1 ]
            os = occurrencesIn b (wcEdits c)
        effects (wcDiagnostics c) `shouldBe` [("flora[0]", EntryRejected), ("flora[1]", EntryRejected)]
        map sdReason (wcDiagnostics c) `shouldBe` [UnknownDefinition "mandrake", OutsideBounds]
        -- the rejected entries consumed no identity: the survivor has the first
        map fiInstanceId os `shouldBe` [fst (nextPlantedFloraCursor firstPlantedFloraCursor)]
        map fst (wrOccurrences (wcRefs c)) `shouldBe` [ex "ok"]
        wcCursor c `shouldBe` firstPlantedFloraCursor + 1

    it "continues the page's allocator from its cursor, never reissuing an id" $ do
        let c = buildWorldContent (envAt Nothing 0 41) [] []
                (withPaths "flora" [floraEntry "a" "oak" 0 0, floraEntry "b" "oak" 1 0])
        wcCursor c `shouldBe` 43
        map (isPlantedFloraInstanceId ∘ snd) (wrOccurrences (wcRefs c)) `shouldBe` [True, True]
        map snd (wrOccurrences (wcRefs c))
            `shouldBe` [ fst (nextPlantedFloraCursor 41), fst (nextPlantedFloraCursor 42) ]

-- * Capture

-- | The snapshot a coherent capture would read after @c@ materialized on
--   a fresh page: the durable log, the resident chunks (or none, to read
--   the log alone), the plots, timers and tags.
snapshotOf ∷ Maybe ArenaBounds → Int → WorldContent → Bool → WorldSnapshot
snapshotOf b day c residentToo = WorldSnapshot
    { snapBounds = b, snapSeed = seed, snapEdits = wcEdits c
    , snapResident = if residentToo
        then [ chunkOf b (wcEdits c) k | k ← HM.keys (wcEdits c) ] else []
    , snapMaterials = registry, snapFlora = catalog, snapAbsDay = day
    , snapCropPlots = HM.fromList (wcCropPlots c)
    , snapHarvests = HM.fromList (wcHarvests c)
    , snapTileTags = M.fromList (wcTileTags c)
    , snapOccTags = M.fromList [ (i, ts) | (i, _, ts) ← wcOccurrenceTags c ]
    , snapPlotTags = M.fromList (wcPlotTags c) }

pathsOf ∷ CapturedWorld → ([(Text, TerrainPatch)], [(Text, FluidPatch)], [(Text, FloraEntry)])
pathsOf cw = (withPaths "terrain" (cwTerrain cw), withPaths "fluids" (cwFluids cw), withPaths "flora" (cwFlora cw))

captureSpec ∷ Spec
captureSpec = describe "capture" $ do

    let ts0 = [ terrainPatch "pad" (RegionRect (-2) (-2) 3 1) "granite" 3 (Authored 0) [tag "pad"]
              , terrainPatch "pit" (RegionTiles [(8, 8), (9, 8)]) "sand" (-2) Omitted []
              , terrainPatch "ramp" (RegionTiles [(0, 9)]) "granite" 0 (Authored 5) []
              , terrainPatch "mark" (RegionTiles [(12, 12)]) "loam" 0 (Authored 0) [tag "mark"] ]
        fs0 = [ fluidPatch "pond" (RegionRect 8 8 9 8) FluidLake 35
              , fluidPatch "bay" (RegionTiles [(20, 20)]) FluidOcean 17 ]
        ls0 = [ authoredOak { feX = 30, feY = 31 }
              , (floraEntry "beets" "beet" 30 31) { feAge = Authored 12, feRegrowth = Authored 0 }
              , (floraEntry "wheat" "wheat" 33 35)
                    { feAge = Authored 6, feTags = [tag "field"] } ]
        day = 777

    it "writes every supported value explicitly, retaining tags, and drops the baseline" $ do
        let c = content Nothing day ts0 fs0 ls0
            cw = captureWorldContent Set.empty (snapshotOf Nothing day c False)
        wcDiagnostics c `shouldBe` []
        -- terrain: the pad, the pit, the ramp and the TAGGED base-spec tile
        map (\p → (tpSurfaceZ p, tpMaterial p, tpSlope p, tpTags p)) (cwTerrain cw) `shouldMatchList`
            [ (3, "granite", Authored 0, [tag "pad"])
            , (-2, "sand", Authored 0, [])
            , (0, "granite", Authored 5, [])
            , (0, "loam", Authored 0, [tag "mark"]) ]
        -- fluid: exact planes
        map (\p → (fpKind p, fpSurfaceEighths p)) (cwFluids cw) `shouldMatchList`
            [(FluidLake, 35), (FluidOcean, 17)]
        -- flora: current age, never the stored baseline; explicit empties
        let byName n = [ e | e ← cwFlora cw, feSpecies e ≡ n ]
        map (\e → (feAge e, feHealth e, feOffsetU e, feVariant e, feRegrowth e, feZ e))
            (byName "oak")
            `shouldBe` [(Authored 120, Authored 0.5, Authored (-0.25), Authored 2, Authored 90, Authored 0)]
        map (\e → (feAge e, feRegrowth e)) (byName "beet") `shouldBe` [(Authored 12, Authored 0)]
        map (\e → (feAge e, feTags e, feZ e, feOffsetU e, feRegrowth e)) (byName "wheat")
            `shouldBe` [(Authored 6, [tag "field"], Omitted, Omitted, Omitted)]

    it "reads the same from the durable log alone as from the resident chunks" $ do
        let c = content Nothing day ts0 fs0 ls0
        captureWorldContent Set.empty (snapshotOf Nothing day c False)
            `shouldBe` captureWorldContent Set.empty (snapshotOf Nothing day c True)

    it "round-trips: capture → materialize → capture is the same, and a repeat accumulates nothing" $ do
        let c1 = content Nothing day ts0 fs0 ls0
            cw1 = captureWorldContent Set.empty (snapshotOf Nothing day c1 False)
            (t2, f2, l2) = pathsOf cw1
            c2 = buildWorldContent (envAt Nothing day firstPlantedFloraCursor) t2 f2 l2
            cw2 = captureWorldContent Set.empty (snapshotOf Nothing day c2 False)
            (t3, f3, l3) = pathsOf cw2
            c3 = buildWorldContent (envAt Nothing day firstPlantedFloraCursor) t3 f3 l3
            cw3 = captureWorldContent Set.empty (snapshotOf Nothing day c3 False)
        wcDiagnostics c2 `shouldBe` []
        cw2 `shouldBe` cw1
        cw3 `shouldBe` cw1
        length (cwFlora cw3) `shouldBe` length ls0
        length (concat (HM.elems (wcEdits c3))) `shouldBe` length (concat (HM.elems (wcEdits c2)))

    it "captures the clock-relative age at a DIFFERENT initialized clock as the same current age" $ do
        let c = content Nothing day [] [] [authoredOak]
            cw = captureWorldContent Set.empty (snapshotOf Nothing day c False)
            (_, _, ls) = pathsOf cw
            -- re-materialize at a later clock: the captured age is the age AT that clock
            c' = buildWorldContent (envAt Nothing 1000 firstPlantedFloraCursor) [] [] ls
            cw' = captureWorldContent Set.empty (snapshotOf Nothing 1000 c' False)
        map feAge (cwFlora cw') `shouldBe` map feAge (cwFlora cw)

    it "replaces a wet tile that has since dried: the recapture states no fluid there" $ do
        let c = content Nothing 0 [] [fluidPatch "pond" (RegionRect 0 0 1 0) FluidLake 35] []
            snap = snapshotOf Nothing 0 c True
            dried = [ lc { lcFluidMap = lcFluidMap lc V.// [(columnIndex 0 0, Nothing)] }
                    | lc ← snapResident snap ]
            cw = captureWorldContent Set.empty snap { snapResident = dried }
        map (\p → (fpKind p, fpRegion p, fpSurfaceEighths p)) (cwFluids cw)
            `shouldBe` [(FluidLake, RegionRect 1 0 1 0, 35)]

    it "ignores structure entries and keeps generated flora out of the scenario" $ do
        let c = content Nothing 0 [terrainPatch "pad" (RegionTiles [(0, 0)]) "granite" 2 Omitted []] [] []
            logged = HM.insertWith (⧺) (ChunkCoord 0 0)
                [WeSetStructure 1 1 0 7 7 2, WeClearStructure 2 2 0] (wcEdits c)
            snap = (snapshotOf Nothing 0 c False) { snapEdits = logged }
        let cw = captureWorldContent Set.empty snap
        map tpSurfaceZ (cwTerrain cw) `shouldBe` [2]
        cwDiagnostics cw `shouldBe` []

    it "names entries from their content, uniquely against the document's other ids" $ do
        let c = content Nothing 0 ts0 fs0 ls0
            cw = captureWorldContent (Set.fromList ["terrain.3.granite.s0.-2.-2", "flora.oak.30.31"])
                                     (snapshotOf Nothing 0 c False)
            ids = map (scenarioIdText ∘ tpId) (cwTerrain cw)
               ⧺ map (scenarioIdText ∘ fpId) (cwFluids cw)
               ⧺ map (scenarioIdText ∘ feId) (cwFlora cw)
        length (Set.fromList ids) `shouldBe` length ids
        ids `shouldSatisfy` (notElem "terrain.3.granite.s0.-2.-2")
        ids `shouldSatisfy` (notElem "flora.oak.30.31")
        ids `shouldSatisfy` (elem "terrain.3.granite.s0.-2.-2-2")

    it "reports a tag below the surface that the vocabulary cannot carry" $ do
        let c = content Nothing 0 [terrainPatch "pad" (RegionTiles [(0, 0)]) "granite" 2 Omitted []] [] []
            snap = (snapshotOf Nothing 0 c False)
                { snapTileTags = M.fromList [((0, 0, 1), Set.fromList [tag "deep"])] }
            cw = captureWorldContent Set.empty snap
        effects (cwDiagnostics cw) `shouldBe` [("terrain.tags", FieldRejected)]

    it "reports a column it cannot write — an unregistered top material, no solid cell — instead of writing garbage" $ do
        let c = content Nothing 0 [terrainPatch "pad" (RegionTiles [(0, 0)]) "granite" 2 Omitted []] [] []
            odd = HM.insertWith (⧺) (ChunkCoord 0 0)
                    [ WeAddTile 5 5 (MaterialId 77)                      -- a top the registry cannot name
                    , WeSetCell 7 7 0 (MaterialId 0) ]                   -- hollowed: the 7,7 top is now air
                    (wcEdits c)
            hollowed = foldr (\z es → HM.insertWith (⧺) (ChunkCoord 0 0) [WeSetCell 7 7 z (MaterialId 0)] es)
                             odd [-15 .. 0]
            cw = captureWorldContent Set.empty ((snapshotOf Nothing 0 c False) { snapEdits = hollowed })
        map tpSurfaceZ (cwTerrain cw) `shouldBe` [2]
        effects (cwDiagnostics cw) `shouldBe`
            [ ("terrain.columns", FieldRejected), ("terrain.material", FieldRejected) ]
