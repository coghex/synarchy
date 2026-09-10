{-# LANGUAGE Strict #-}
-- | Pure tests for DFL-1's flat-step fluid geometry (#2517): flat River
--   and Lake tops from 'World.Render.FluidTopQuads.fluidTopQuads', and
--   'World.Render.SideDecoQuads.waterSideFaceQuads' owning every positive
--   visible drop between a fluid top and a lower neighbour.
--
--   The two halves are one contract and are tested together, under the
--   registered group @World.Render.SideFace@, because neither is
--   sufficient on its own: a flat top with no side face leaves a one-z
--   drop invisible, and a side face under a ramped top double-draws it.
--
--   The side-face half also still guards its original regressions: side
--   faces used to be hard filtered to in-chunk neighbours, so a drop
--   landing right on a chunk seam produced nothing (#26), and the raw
--   cross-chunk key missed a loaded neighbour at the cylindrical U seam
--   (#1135).
--
--   No engine needed: both passes are pure. We hand-build a 16×16 home
--   chunk and feed neighbour chunks through the lookup callbacks. The
--   view bounds accept every tile, so the emitted-quad COUNT is exactly
--   the number of z-levels in the drop, and the face-map handles are all
--   DISTINCT (see 'distinctTextures') so an emitted quad names which face
--   map it selected.
module Test.Headless.World.Render.SideFace (spec) where

import UPrelude
import Test.Hspec
import Data.List (subsequences)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..))
import Engine.Scene.Types (SortableQuad(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..), IceCell(..)
                         , IceMode(..))
import World.Material (matOcean, matLava, unMaterialId)
import World.Render.ChunkLookup (canonicalChunkLookup)
import World.Render.FluidTopQuads (fluidTopQuads)
import World.Render.QuadContext (QuadContext(..), ZSlice(..)
                                , EffectiveDepth(..))
import World.Render.SideDecoQuads (waterSideFaceQuads)
import World.Render.Textures.Types (WorldTextures(..), defaultWorldTextures)
import World.Render.ViewBounds (ViewBounds(..))

-- | One chunk's fluid map: all-empty, with the listed cells set.
fluidMapWith ∷ [((Int, Int), FluidCell)] → V.Vector (Maybe FluidCell)
fluidMapWith cells =
    V.replicate (chunkSize * chunkSize) Nothing
      V.// [ (columnIndex x y, Just fc) | ((x, y), fc) ← cells ]

-- | One chunk's terrain-surface map: a flat base z with overrides.
terrMapWith ∷ Int → [((Int, Int), Int)] → VU.Vector Int
terrMapWith base overrides =
    VU.replicate (chunkSize * chunkSize) base
      VU.// [ (columnIndex x y, z) | ((x, y), z) ← overrides ]

-- | One chunk's ice map: all-empty, with the listed cells covered.
iceMapWith ∷ [((Int, Int), Int)] → V.Vector (Maybe IceCell)
iceMapWith cells =
    V.replicate (chunkSize * chunkSize) Nothing
      V.// [ (columnIndex x y, Just (IceCell z BasinIce)) | ((x, y), z) ← cells ]

-- | View bounds that accept every tile, so visibility never trims a quad.
allVisible ∷ ViewBounds
allVisible = ViewBounds (-1.0e9) 1.0e9 (-1.0e9) 1.0e9

-- | Drive the side-face generator over the home chunk (0,0) with
--   'testCtx'.
run ∷ V.Vector (Maybe FluidCell) → VU.Vector Int
    → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
    → (ChunkCoord → Maybe (VU.Vector Int))
    → [SortableQuad]
run fm tm fluidLookup terrLookup =
    waterSideFaceQuads testCtx (ChunkCoord 0 0) fm tm
        fluidLookup terrLookup allVisible

-- | Every face-map field of 'defaultWorldTextures' is @TextureHandle 0@,
--   so a WRONG face-map selection is indistinguishable from the right one
--   there. Give each one a distinct handle instead, and let the context's
--   'qcLookupFmSlot' return the handle id (below), so a quad's
--   'faceMapId' names exactly which face map the producer chose.
--
--   The side face maps get non-zero handles for the same reason they must
--   under 'defaultWorldTextures' stubbing: 'waterSideQuad' treats slot
--   @0.0@ as "face map not loaded" and emits nothing.
distinctTextures ∷ WorldTextures
distinctTextures = defaultWorldTextures
    { wtIsoFaceMap          = TextureHandle 10
    , wtSlopeFaceMapN       = TextureHandle 11
    , wtSlopeFaceMapE       = TextureHandle 12
    , wtSlopeFaceMapNE      = TextureHandle 13
    , wtSlopeFaceMapS       = TextureHandle 14
    , wtSlopeFaceMapNS      = TextureHandle 15
    , wtSlopeFaceMapES      = TextureHandle 16
    , wtSlopeFaceMapNES     = TextureHandle 17
    , wtSlopeFaceMapW       = TextureHandle 18
    , wtSlopeFaceMapNW      = TextureHandle 19
    , wtSlopeFaceMapEW      = TextureHandle 20
    , wtSlopeFaceMapNEW     = TextureHandle 21
    , wtSlopeFaceMapSW      = TextureHandle 22
    , wtSlopeFaceMapNSW     = TextureHandle 23
    , wtSlopeFaceMapESW     = TextureHandle 24
    , wtSlopeFaceMapNESW    = TextureHandle 25
    , wtNoFaceMap           = TextureHandle 26
    , wtSideFaceMapLeft     = TextureHandle 30
    , wtSideFaceMapRight    = TextureHandle 31
    , wtTileTextures        = HM.fromList
        [ (unMaterialId matOcean, TextureHandle 40)
        , (unMaterialId matLava,  TextureHandle 41)
        ]
    }

-- | The slot a correctly-selected FLAT fluid top must report.
flatFaceMapSlot ∷ Float
flatFaceMapSlot = fromIntegral (toInt (wtIsoFaceMap distinctTextures))

-- | Every slope face-map slot. A fluid top reporting one of these has
--   selected a ramp, which is exactly what #2517 removes.
slopeFaceMapSlots ∷ [Float]
slopeFaceMapSlots =
    map (fromIntegral ∘ toInt ∘ ($ distinctTextures))
        [ wtSlopeFaceMapN, wtSlopeFaceMapE, wtSlopeFaceMapNE
        , wtSlopeFaceMapS, wtSlopeFaceMapNS, wtSlopeFaceMapES
        , wtSlopeFaceMapNES, wtSlopeFaceMapW, wtSlopeFaceMapNW
        , wtSlopeFaceMapEW, wtSlopeFaceMapNEW, wtSlopeFaceMapSW
        , wtSlopeFaceMapNSW, wtSlopeFaceMapESW, wtSlopeFaceMapNESW
        ]

-- | The shared render context every case here drives a producer with
--   (#1138). Slot lookup returns 0 (any tile texture) and the face-map
--   slot the handle's own id, so the choice is observable; zSlice 10 /
--   effective depth 64 puts the whole drop inside the rendered z-window;
--   opaque, unwrapped.
testCtx ∷ QuadContext
testCtx = QuadContext
    { qcLookupSlot     = \_ → 0
    , qcLookupFmSlot   = \h → fromIntegral (toInt h)
    , qcTextures       = distinctTextures
    , qcFacing         = FaceSouth
    , qcZSlice         = ZSlice 10
    , qcEffectiveDepth = EffectiveDepth 64
    , qcTileAlpha      = 1.0
    , qcWrapOffset     = (0.0, 0.0)
    }

spec ∷ Spec
spec = do
  flatTopSpec
  oneZSpec
  preservedSideSpec
  inChunkSpec
  seamSpec

-- * Requirement 1 — flat fluid tops

-- | The tops half of the flat-step contract, driven through the real
--   production pass 'fluidTopQuads' (the one 'renderWorldQuads' calls)
--   rather than a stand-in fold.
flatTopSpec ∷ Spec
flatTopSpec = describe "fluidTopQuads selects a flat fluid top (#2517)" $ do
    let noIce = V.replicate (chunkSize * chunkSize) Nothing
        topsOf fm im = fluidTopQuads testCtx (ChunkCoord 0 0) fm im allVisible
        faceMaps = map (faceMapId ∘ sqV0)

        -- Centre tile at (8,8), surface 10, with the chosen cardinal
        -- neighbours one z LOWER. Before #2517 each such neighbour set a
        -- pair of slope bits, so 15 of these 16 topologies picked a ramp.
        topology ft dirs = fluidMapWith $
            ((8, 8), FluidCell ft 10)
              : [ (p, FluidCell ft 9) | p ← dirs ]
        cardinals = [(8, 7), (9, 8), (8, 9), (7, 8)]

    it "is flat for River under every lower-neighbour topology" $
        sequence_
            [ let (_, _, fresh) = topsOf (topology River dirs) noIce
              in do
                  fresh `shouldNotSatisfy` null
                  faceMaps fresh `shouldSatisfy` all (≡ flatFaceMapSlot)
            | dirs ← subsequences cardinals ]

    it "is flat for Lake under every lower-neighbour topology" $
        sequence_
            [ let (_, _, fresh) = topsOf (topology Lake dirs) noIce
              in do
                  fresh `shouldNotSatisfy` null
                  faceMaps fresh `shouldSatisfy` all (≡ flatFaceMapSlot)
            | dirs ← subsequences cardinals ]

    it "never selects any of the sixteen slope face maps" $ do
        -- The negative form of the two cases above: the flat handle is
        -- distinct from every ramp handle, so this fails for a producer
        -- that picks ANY ramp, including one that always picks the same
        -- one and would survive a single-topology fixture.
        let (_, _, fresh) = topsOf (topology River cardinals) noIce
            (_, _, fresh2) = topsOf (topology Lake [(9, 8)]) noIce
        (faceMaps fresh <> faceMaps fresh2)
            `shouldSatisfy` all (`notElem` slopeFaceMapSlots)

    it "is flat for a single lower neighbour, which used to ramp" $ do
        -- Grid N one z lower alone produced raw mask 3 → wtSlopeFaceMapNE
        -- (handle 13). Naming the specific handle the old code would
        -- have chosen keeps this case honest about what changed.
        let (_, _, fresh) = topsOf (topology River [(8, 7)]) noIce
        faceMaps fresh `shouldSatisfy` all (≡ flatFaceMapSlot)
        faceMaps fresh `shouldSatisfy` all (≢ 13.0)

    it "keeps Ocean and Lava tops flat and unchanged" $ do
        let fm = fluidMapWith [ ((4, 4), FluidCell Ocean 10)
                              , ((6, 6), FluidCell Lava 10)
                              , ((6, 7), FluidCell Lava 9) ]
            (ocean, lava, fresh) = topsOf fm noIce
        length ocean `shouldBe` 1
        length lava `shouldBe` 2
        length fresh `shouldBe` 0
        (faceMaps ocean <> faceMaps lava)
            `shouldSatisfy` all (≡ flatFaceMapSlot)

    it "suppresses Ocean and Lake tops under ice but keeps River tops" $ do
        -- Requirement 4 / the reviewer's clarification: ice ELIGIBILITY
        -- is unchanged. Ocean and Lake yield their top to the ice
        -- overlay; River has always kept drawing under ice.
        let fm = fluidMapWith [ ((1, 1), FluidCell Ocean 10)
                              , ((2, 2), FluidCell Lake 10)
                              , ((3, 3), FluidCell River 10) ]
            im = iceMapWith [((1, 1), 10), ((2, 2), 10), ((3, 3), 10)]
            (ocean, lava, fresh) = topsOf fm im
        length ocean `shouldBe` 0
        length lava `shouldBe` 0
        length fresh `shouldBe` 1
        faceMaps fresh `shouldBe` [flatFaceMapSlot]

    it "clips fluid tops to the z-slice window" $ do
        -- zSlice 10, effective depth 2 → only surfaces in [8, 10].
        let ctx = testCtx { qcEffectiveDepth = EffectiveDepth 2 }
            fm = fluidMapWith [ ((1, 1), FluidCell Lake 11)   -- above
                              , ((2, 2), FluidCell Lake 10)   -- in
                              , ((3, 3), FluidCell Lake 8)    -- in
                              , ((4, 4), FluidCell Lake 7) ]  -- below
            (_, _, fresh) = fluidTopQuads ctx (ChunkCoord 0 0) fm noIce allVisible
        length fresh `shouldBe` 2

    it "emits no top for a tile the view bounds reject" $ do
        let offscreen = ViewBounds 1.0e9 1.0e9 1.0e9 1.0e9
            fm = fluidMapWith [((8, 8), FluidCell Lake 10)]
            (_, _, fresh) = fluidTopQuads testCtx (ChunkCoord 0 0) fm noIce offscreen
        length fresh `shouldBe` 0

-- * Requirement 2 — the side-face generator owns every positive drop

oneZSpec ∷ Spec
oneZSpec = describe "waterSideFaceQuads owns a one-z drop (#2517)" $ do
    -- Home water at the interior tile (5,8), surface 10, flat terrain at
    -- 10. Under FaceSouth the two camera-visible neighbours are (5,9)
    -- and (6,8); only the one a case overrides ever drops.
    let waterAt ft = fluidMapWith [((5, 8), FluidCell ft 10)]
        flatTerr  = terrMapWith 10 []
        dryDropTo z = terrMapWith 10 [((6, 8), z)]
        wetDropTo z = fluidMapWith [ ((5, 8), FluidCell Lake 10)
                                   , ((6, 8), FluidCell Lake z) ]
        noLookup ∷ ChunkCoord → Maybe a
        noLookup = const Nothing

    it "emits exactly one quad for an in-chunk one-z DRY drop" $
        length (run (waterAt Lake) (dryDropTo 9) noLookup noLookup)
            `shouldBe` 1

    it "emits exactly one quad for an in-chunk one-z WET drop" $
        length (run (wetDropTo 9) flatTerr noLookup noLookup)
            `shouldBe` 1

    it "puts the one-z quad at the z the drop actually spans" $ do
        -- The single quad covers the level between the neighbour surface
        -- (9) and this tile's surface (10), i.e. z = 9. A producer that
        -- drew it at z = 10 would emit the right COUNT at the wrong
        -- height, so compare its sort key against the SAME level in a
        -- deeper drop over the same tile rather than only counting.
        let oneZ = run (waterAt Lake) (dryDropTo 9) noLookup noLookup
            twoZ = run (waterAt Lake) (dryDropTo 8) noLookup noLookup
        length oneZ `shouldBe` 1
        length twoZ `shouldBe` 2
        -- The stack is emitted bottom-up (z = bottomZ .. mySurf - 1), so
        -- the two-z stack's LAST quad is its z = 9 one.
        map sqSortKey oneZ `shouldBe` map sqSortKey (drop 1 twoZ)

    it "emits N quads for a drop of N z, for N = 1..5" $
        sequence_
            [ length (run (waterAt Lake) (dryDropTo (10 - n)) noLookup noLookup)
                `shouldBe` n
            | n ← [1 .. 5 ∷ Int] ]

    it "emits N quads for a WET drop of N z, for N = 1..5" $
        sequence_
            [ length (run (wetDropTo (10 - n)) flatTerr noLookup noLookup)
                `shouldBe` n
            | n ← [1 .. 5 ∷ Int] ]

    it "emits one quad per camera-visible edge for all four facings" $
        -- All four cardinal neighbours one z lower: each facing shows
        -- exactly its own two edges, never four and never zero.
        sequence_
            [ let ctx = testCtx { qcFacing = f }
                  fm = fluidMapWith
                          ( ((5, 8), FluidCell Lake 10)
                          : [ (p, FluidCell Lake 9)
                            | p ← [(5, 7), (6, 8), (5, 9), (4, 8)] ] )
              in length (waterSideFaceQuads ctx (ChunkCoord 0 0) fm flatTerr
                            noLookup noLookup allVisible)
                     `shouldBe` 2
            | f ← [FaceSouth, FaceEast, FaceNorth, FaceWest] ]

    it "clips the side stack to the z-slice window" $ do
        -- A five-z drop with effective depth 2 shows only z ∈ {8, 9}.
        let ctx = testCtx { qcEffectiveDepth = EffectiveDepth 2 }
        length (waterSideFaceQuads ctx (ChunkCoord 0 0)
                    (waterAt Lake) (dryDropTo 5) noLookup noLookup allVisible)
            `shouldBe` 2

    it "leaves Lava's one-z omission and multi-z geometry unchanged" $ do
        -- Requirement 4: DFL-1 is a freshwater change. Lava still starts
        -- at a two-z gap and still emits one quad per z above that.
        length (run (waterAt Lava) (dryDropTo 9) noLookup noLookup)
            `shouldBe` 0
        length (run (waterAt Lava) (dryDropTo 8) noLookup noLookup)
            `shouldBe` 2
        length (run (waterAt Lava) (dryDropTo 5) noLookup noLookup)
            `shouldBe` 5

    it "leaves Ocean excluded from fluid side faces entirely" $ do
        length (run (waterAt Ocean) (dryDropTo 9) noLookup noLookup)
            `shouldBe` 0
        length (run (waterAt Ocean) (dryDropTo 5) noLookup noLookup)
            `shouldBe` 0

    it "emits one quad each side of a one-z drop across a loaded seam" $ do
        -- Requirement 3, ordinary (non-U) chunk seam: the east edge tile
        -- steps into chunk (1,0). Dry and wet, each exactly one quad.
        let edgeWater = fluidMapWith [((chunkSize - 1, 8), FluidCell Lake 10)]
            fluidDry (ChunkCoord 1 0) = Just (fluidMapWith [])
            fluidDry _                = Nothing
            terrOne  (ChunkCoord 1 0) = Just (terrMapWith 9 [])
            terrOne  _                = Nothing
            fluidWet (ChunkCoord 1 0) =
                Just (fluidMapWith [((0, 8), FluidCell Lake 9)])
            fluidWet _                = Nothing
            terrFlat (ChunkCoord 1 0) = Just (terrMapWith 10 [])
            terrFlat _                = Nothing
        length (run edgeWater flatTerr fluidDry terrOne) `shouldBe` 1
        length (run edgeWater flatTerr fluidWet terrFlat) `shouldBe` 1

    it "emits one quad for a one-z drop across the cylindrical U seam" $ do
        -- Requirement 3 (#1135), through the real canonicalising lookup.
        let seamHome   = ChunkCoord 16 (-15)
            seamStored = ChunkCoord (-15) 17
            lookupVia m = canonicalChunkLookup 64
                              (HM.fromList [(seamStored, m)])
            edgeWater = fluidMapWith [((chunkSize - 1, 8), FluidCell Lake 10)]
            runAt fl tl = waterSideFaceQuads testCtx seamHome edgeWater
                              flatTerr fl tl allVisible
        length (runAt (lookupVia (fluidMapWith [])) (lookupVia (terrMapWith 9 [])))
            `shouldBe` 1
        length (runAt (lookupVia (fluidMapWith [((0, 8), FluidCell Lake 9)]))
                      (lookupVia (terrMapWith 10 [])))
            `shouldBe` 1

    it "draws nothing for a one-z drop into an UNLOADED seam neighbour" $ do
        -- The conservative default must survive the ownership transfer:
        -- an unknown neighbour is still not a drop.
        let edgeWater = fluidMapWith [((chunkSize - 1, 8), FluidCell Lake 10)]
        length (run edgeWater flatTerr (const Nothing) (const Nothing))
            `shouldBe` 0

-- * Requirement 3 — behaviour that must NOT change

preservedSideSpec ∷ Spec
preservedSideSpec = describe "waterSideFaceQuads leaves non-drops alone" $ do
    let flatTerr = terrMapWith 10 []
        noLookup ∷ ChunkCoord → Maybe a
        noLookup = const Nothing
        runIn fm tm = run fm tm noLookup noLookup

    it "draws nothing for an equal-height wet neighbour" $
        length (runIn (fluidMapWith [ ((5, 8), FluidCell Lake 10)
                                    , ((6, 8), FluidCell Lake 10) ]) flatTerr)
            `shouldBe` 0

    it "draws nothing for a HIGHER wet neighbour" $
        -- The higher neighbour's OWN visible neighbours ((6,9) and (7,8)
        -- under FaceSouth) are raised to its surface too, so the count
        -- below isolates the tile under test: without that, the pool at
        -- 12 draws its own two-z sides and the fixture is vacuous.
        length (runIn (fluidMapWith [ ((5, 8), FluidCell Lake 10)
                                    , ((6, 8), FluidCell Lake 12) ])
                      (terrMapWith 10 [((6, 9), 12), ((7, 8), 12)]))
            `shouldBe` 0

    it "draws nothing for equal or higher dry terrain" $ do
        length (runIn (fluidMapWith [((5, 8), FluidCell Lake 10)]) flatTerr)
            `shouldBe` 0
        length (runIn (fluidMapWith [((5, 8), FluidCell Lake 10)])
                      (terrMapWith 10 [((6, 8), 12)]))
            `shouldBe` 0

    it "draws nothing for enclosed equal-height water" $
        -- A 3×3 block of Lake at one surface on level terrain: no cell
        -- has a lower neighbour, so the whole block has no side at all.
        length (runIn (fluidMapWith [ ((x, y), FluidCell Lake 10)
                                    | x ← [4 .. 6], y ← [7 .. 9] ]) flatTerr)
            `shouldBe` 0

    it "keeps one quad per z, sorted a fixed step apart" $ do
        -- Sort ordering is unchanged: one quad per z-level of the drop,
        -- keys ascending in 0.001 steps as they always were.
        let quads = runIn (fluidMapWith [((5, 8), FluidCell Lake 10)])
                          (terrMapWith 10 [((6, 8), 6)])
            keys = map sqSortKey quads
            steps = zipWith (-) (drop 1 keys) keys
        length quads `shouldBe` 4
        steps `shouldSatisfy` all (\d → abs (d - 0.001) < 1.0e-5)

inChunkSpec ∷ Spec
inChunkSpec = describe "waterSideFaceQuads across chunk seams" $ do

    -- Home chunk (0,0): one Lake tile on the EAST edge (lx = 15) at z=10.
    -- Flat terrain at z=10 everywhere, so the in-chunk (left) neighbor is
    -- level with the water and never draws — every emitted quad therefore
    -- comes from the cross-chunk (right) neighbor.
    let homeFluid = fluidMapWith [((15, 8), FluidCell Lake 10)]
        homeTerr  = terrMapWith 10 []

    it "renders side faces over a DRY drop in the adjacent chunk (the bug)" $ do
        -- Neighbor chunk (1,0): dry, terrain at z=0 → a 10-tall waterfall
        -- face straddling the seam. Before the fix this produced nothing.
        let fluidLookup (ChunkCoord 1 0) = Just (fluidMapWith [])
            fluidLookup _                = Nothing
            terrLookup  (ChunkCoord 1 0) = Just (terrMapWith 0 [])
            terrLookup  _                = Nothing
        -- z = 0..9 → ten side-face quads.
        length (run homeFluid homeTerr fluidLookup terrLookup) `shouldBe` 10

    it "renders side faces over a LOWER-WATER drop in the adjacent chunk" $ do
        -- Neighbor (0,8) holds water at surface 5, so the stack bottoms
        -- out on that surface: faces from z=5..9 (five quads).
        let fluidLookup (ChunkCoord 1 0) =
                Just (fluidMapWith [((0, 8), FluidCell Lake 5)])
            fluidLookup _                = Nothing
            terrLookup  (ChunkCoord 1 0) = Just (terrMapWith 0 [])
            terrLookup  _                = Nothing
        length (run homeFluid homeTerr fluidLookup terrLookup) `shouldBe` 5

    it "draws nothing at the seam when the neighbor chunk is not loaded" $
        -- Both lookups miss → the drop is unknown, so no side face (the
        -- conservative default at an unloaded seam).
        length (run homeFluid homeTerr (const Nothing) (const Nothing))
            `shouldBe` 0

    it "still renders a waterfall face WITHIN a chunk (regression guard)" $ do
        -- Water at interior tile (5,8); the in-chunk right neighbor (6,8)
        -- is a dry 10-tile drop. No cross-chunk lookup is consulted.
        let inFluid = fluidMapWith [((5, 8), FluidCell Lake 10)]
            inTerr  = terrMapWith 10 [((6, 8), 0)]
        length (run inFluid inTerr (const Nothing) (const Nothing))
            `shouldBe` 10

-- | #1135: 'neighborCell' builds its cross-chunk coord in the HOME
--   chunk's raw frame, but chunks are STORED u-wrapped. Right at the
--   seam those disagree, so the raw @HM.lookup@ missed a LOADED
--   neighbour and the resulting Nothing read as "not loaded" — side
--   faces silently vanished along the whole seam.
--
--   These drive the real production lookup boundary
--   ('World.Render.ChunkLookup.canonicalChunkLookup' — the same helper
--   'renderWorldQuads' builds its two callbacks from) against a map
--   keyed ONLY by the canonical coord. A test-local 'wrapChunkCoordU'
--   would pass even if the production lookup regressed.
seamSpec ∷ Spec
seamSpec = describe "waterSideFaceQuads across the U seam (#1135)" $ do
    -- worldSize 64 → canonical chunk u ∈ [-32, 32). Home chunk (16,-15)
    -- has u = 31, so its raw EAST neighbour (17,-15) has u = 32 — one
    -- past the range, and is stored under ChunkCoord (-15) 17 instead.
    let seamHome   = ChunkCoord 16 (-15)
        seamStored = ChunkCoord (-15) 17
        seamWorld  = 64
        -- One Lake tile on the home chunk's EAST edge at z=10, flat
        -- terrain at 10, so every emitted quad comes from the seam step.
        homeFluid = fluidMapWith [((chunkSize - 1, 8), FluidCell Lake 10)]
        homeTerr  = terrMapWith 10 []
        lookupVia m = canonicalChunkLookup seamWorld
                          (HM.fromList [(seamStored, m)])
        runAt coord fluidLookup terrLookup =
            waterSideFaceQuads testCtx coord homeFluid homeTerr
                fluidLookup terrLookup allVisible

    it "renders side faces over a DRY drop across the seam" $
        -- Neighbour stored under the wrapped key is dry at z=0 → z=0..9.
        length (runAt seamHome (lookupVia (fluidMapWith []))
                               (lookupVia (terrMapWith 0 [])))
            `shouldBe` 10

    it "renders side faces over a LOWER-WATER drop across the seam" $
        length (runAt seamHome
                    (lookupVia (fluidMapWith [((0, 8), FluidCell Lake 5)]))
                    (lookupVia (terrMapWith 0 [])))
            `shouldBe` 5

    it "matches the equivalent interior fixture exactly" $ do
        -- Same relative geometry one chunk IN from the seam: home chunk
        -- (15,-15) has u = 29, so its raw east neighbour (16,-15) is
        -- itself canonical and the wrap is the identity (requirement 4).
        --
        -- The two fixtures sit at different world positions, so the
        -- absolute sort keys legitimately differ by the tile offset
        -- between them. Compare the per-quad structure instead —
        -- normalised against each fixture's own base key, which is
        -- exactly the z-stack the drop produces (one quad per z, keys
        -- 0.001 apart). Normalising subtracts Floats of different
        -- magnitudes, so compare within a tolerance two orders of
        -- magnitude below that step rather than bit-exactly.
        let interiorVia m = canonicalChunkLookup seamWorld
                                (HM.fromList [(ChunkCoord 16 (-15), m)])
            seamQuads = runAt seamHome (lookupVia (fluidMapWith []))
                                       (lookupVia (terrMapWith 0 []))
            interiorQuads = runAt (ChunkCoord 15 (-15))
                                (interiorVia (fluidMapWith []))
                                (interiorVia (terrMapWith 0 []))
            normalised qs = let ks = map sqSortKey qs
                            in map (subtract (minimum ks)) ks
        length seamQuads `shouldBe` 10
        length seamQuads `shouldBe` length interiorQuads
        zip (normalised seamQuads) (normalised interiorQuads)
            `shouldSatisfy` all (\(a, b) → abs (a - b) < 1.0e-5)

    it "draws nothing when the seam neighbour is genuinely unloaded" $
        -- The negative case the raw lookup could not tell apart from a
        -- loaded-but-aliased neighbour: an empty map means NOT LOADED.
        let emptyVia ∷ ChunkCoord → Maybe a
            emptyVia = canonicalChunkLookup seamWorld HM.empty
        in length (runAt seamHome emptyVia emptyVia) `shouldBe` 0
