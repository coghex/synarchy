{-# LANGUAGE Strict #-}
-- | Constructed-chunk coverage for the zoom cache's ocean boundary
--   fill (issue #2316).
--
--   The fill used to accept every fluid kind as an in-chunk seed and
--   to read the mutable vector it was writing, so a lake, river or
--   lava neighbour promoted a dry tile to ocean and the promotion
--   cascaded down-and-right through cells it had just written. Both
--   defects are pinned here on a synthetic 16×16 chunk; every case
--   below fails against that implementation.
module Test.Headless.WorldGen.ZoomOceanFill (spec) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Hspec
import World.Fluid.Internal (FluidMap)
import World.Types
import World.ZoomMap.Cache.OceanFill (extendOceanBoundary)
import World.ZoomMap.Cache.Pixels (generateChunkPixels)
import World.ZoomMap.ColorPalette
    (ZoomColorPalette(..), defaultOceanColor)

-- * Fixture helpers

chunkArea ∷ Int
chunkArea = chunkSize * chunkSize

atIdx ∷ Int → Int → Int
atIdx lx ly = ly * chunkSize + lx

-- | A chunk whose tiles all sit exactly at sea level unless listed.
--   Sea level is the interesting value: it both admits the fill and
--   paints as ocean once promoted.
elevWith ∷ [((Int, Int), Int)] → VU.Vector Int
elevWith overrides =
    let m = Map.fromList [ (atIdx lx ly, z) | ((lx, ly), z) ← overrides ]
    in VU.generate chunkArea $ \idx → Map.findWithDefault seaLevel idx m

-- | A composed fluid map carrying only the listed cells.
fluidWith ∷ [((Int, Int), FluidCell)] → FluidMap
fluidWith cells =
    let m = Map.fromList [ (atIdx lx ly, c) | ((lx, ly), c) ← cells ]
    in V.generate chunkArea $ \idx → Map.lookup idx m

ocean, lake, river, lava ∷ FluidCell
ocean = FluidCell Ocean seaLevel
lake  = FluidCell Lake  seaLevel
river = FluidCell River seaLevel
lava  = FluidCell Lava  seaLevel

-- | No adjacent chunk carries ocean.
noNeighborOcean ∷ Int → Int → Bool
noNeighborOcean _ _ = False

fluidAt ∷ FluidMap → (Int, Int) → Maybe FluidCell
fluidAt fm (lx, ly) = fm V.! atIdx lx ly

isOceanCell ∷ Maybe FluidCell → Bool
isOceanCell (Just fc) = fcType fc ≡ Ocean
isOceanCell Nothing   = False

-- | Point-reflect a chunk-shaped vector through its centre, so that a
--   row-major scan of the mirrored chunk visits tiles in exactly the
--   reverse of the original order.
mirrorIdx ∷ Int → Int
mirrorIdx idx =
    let lx = idx `mod` chunkSize
        ly = idx `div` chunkSize
    in atIdx (chunkSize - 1 - lx) (chunkSize - 1 - ly)

mirrorBoxed ∷ V.Vector α → V.Vector α
mirrorBoxed v = V.generate chunkArea $ \idx → v V.! mirrorIdx idx

mirrorUnboxed ∷ VU.Vector Int → VU.Vector Int
mirrorUnboxed v = VU.generate chunkArea $ \idx → v VU.! mirrorIdx idx

-- * Pixel-path helpers

-- | A palette whose one material colour is unmistakably not ocean.
testPalette ∷ ZoomColorPalette
testPalette = ZoomColorPalette
    { zcpMaterials  = Map.fromList [(testMat, (20, 200, 40, 255))]
    , zcpVegetation = Map.empty
    }

testMat ∷ Word8
testMat = 7

-- | Render a chunk through the real pixel path and count the pixels
--   painted with 'defaultOceanColor' — the full base-colour
--   replacement that only an Ocean cell at or below its surface gets.
oceanPixelCount ∷ VU.Vector Int → FluidMap → Int
oceanPixelCount elevs fluid =
    let tileVec = V.generate chunkArea $ \idx →
            ( elevs VU.! idx, testMat, 0 ∷ Word8
            , idx `mod` chunkSize, idx `div` chunkSize )
        iceMap = V.replicate chunkArea Nothing
        bytes = generateChunkPixels testPalette False 64 fluid iceMap tileVec
        quad i = ( BS.index bytes i, BS.index bytes (i + 1)
                 , BS.index bytes (i + 2), BS.index bytes (i + 3) )
    in length [ () | i ← [0, 4 .. BS.length bytes - 4]
              , quad i ≡ defaultOceanColor ]

-- * Spec

spec ∷ Spec
spec = describe "zoom ocean boundary fill" $ do

    describe "seeds only from ocean" $ do
        it "a lake neighbour promotes nothing" $
            rejectsSeed lake
        it "a river neighbour promotes nothing" $
            rejectsSeed river
        it "a lava neighbour promotes nothing" $
            rejectsSeed lava

        it "an ocean neighbour still closes the gap" $ do
            let fluid = fluidWith [((4, 4), ocean)]
                out = extendOceanBoundary noNeighborOcean
                          (elevWith []) fluid
            fluidAt out (5, 4) `shouldBe` Just ocean
            fluidAt out (3, 4) `shouldBe` Just ocean
            fluidAt out (4, 3) `shouldBe` Just ocean
            fluidAt out (4, 5) `shouldBe` Just ocean

        it "diagonal ocean is not adjacency" $ do
            let out = extendOceanBoundary noNeighborOcean (elevWith [])
                          (fluidWith [((4, 4), ocean)])
            fluidAt out (5, 5) `shouldBe` Nothing

    describe "closes gaps across a chunk boundary" $ do
        it "promotes an edge tile from the neighbour chunk's ocean" $ do
            -- Ocean sits west of the chunk, at local x = -1, y = 5.
            let probe x y = x ≡ (-1) ∧ y ≡ 5
                out = extendOceanBoundary probe (elevWith [])
                          (fluidWith [])
            fluidAt out (0, 5) `shouldBe` Just ocean
            -- Same edge column, a row the neighbour does not claim.
            fluidAt out (0, 7) `shouldBe` Nothing

        it "does not cascade inward from a cross-chunk seed" $ do
            let probe x y = x ≡ (-1) ∧ y ≡ 5
                out = extendOceanBoundary probe (elevWith [])
                          (fluidWith [])
            fluidAt out (1, 5) `shouldBe` Nothing

    describe "is one bounded dilation, not a flood" $ do
        it "a synthesized cell does not seed the next one" $ do
            let out = extendOceanBoundary noNeighborOcean (elevWith [])
                          (fluidWith [((2, 2), ocean)])
            fluidAt out (3, 2) `shouldBe` Just ocean
            fluidAt out (4, 2) `shouldBe` Nothing

        it "an invisible above-sea cell does not carry the promotion" $ do
            -- (3,6) is admitted (seaLevel+1 ≤ seaLevel+2) but paints
            -- nothing, because the fill writes surface = seaLevel and
            -- the pixel path needs elev ≤ surface. It must not hand the
            -- promotion on to the genuinely sub-sea tile behind it.
            let elevs = elevWith [((3, 6), seaLevel + 1)]
                out = extendOceanBoundary noNeighborOcean elevs
                          (fluidWith [((2, 6), ocean)])
            fluidAt out (3, 6) `shouldBe` Just ocean
            fluidAt out (4, 6) `shouldBe` Nothing

        it "leaves tiles above the admission bound alone" $ do
            let elevs = elevWith [((5, 9), seaLevel + 3)]
                out = extendOceanBoundary noNeighborOcean elevs
                          (fluidWith [((4, 9), ocean)])
            fluidAt out (5, 9) `shouldBe` Nothing

        it "leaves absent tiles alone" $ do
            let elevs = elevWith [((5, 11), minBound)]
                out = extendOceanBoundary noNeighborOcean elevs
                          (fluidWith [((4, 11), ocean)])
            fluidAt out (5, 11) `shouldBe` Nothing

    describe "does not depend on scan order" $
        it "a reversed scan of the same chunk yields identical output" $ do
            -- A chain that a row-major cascade would carry east and
            -- south, and a mirrored one would carry west and north.
            let cells = [ ((2, 2), ocean), ((9, 3), lake)
                        , ((11, 8), river), ((4, 12), lava) ]
                elevs = elevWith [((3, 2), seaLevel + 1)]
                fluid = fluidWith cells
                forward = extendOceanBoundary noNeighborOcean elevs fluid
                mirrored = extendOceanBoundary
                    (\x y → noNeighborOcean (chunkSize - 1 - x)
                                            (chunkSize - 1 - y))
                    (mirrorUnboxed elevs) (mirrorBoxed fluid)
            mirrorBoxed mirrored `shouldBe` forward

    describe "the rendered pixels" $ do
        it "paint no ocean beside a lake" $ do
            let elevs = elevWith []
                fluid = fluidWith [((6, 6), lake)]
                out = extendOceanBoundary noNeighborOcean elevs fluid
            oceanPixelCount elevs out `shouldBe` 0

        it "still paint the closed gap beside real ocean" $ do
            let elevs = elevWith []
                fluid = fluidWith [((6, 6), ocean)]
                out = extendOceanBoundary noNeighborOcean elevs fluid
                before = oceanPixelCount elevs fluid
                after = oceanPixelCount elevs out
            before `shouldSatisfy` (> 0)
            after `shouldSatisfy` (> before)

-- | A single non-ocean fluid cell must promote none of its four
--   cardinal neighbours, all of which are dry and at sea level.
rejectsSeed ∷ FluidCell → Expectation
rejectsSeed seed = do
    let out = extendOceanBoundary noNeighborOcean (elevWith [])
                  (fluidWith [((6, 6), seed)])
    fluidAt out (6, 6) `shouldBe` Just seed
    map (isOceanCell . fluidAt out)
        [(5, 6), (7, 6), (6, 5), (6, 7)]
        `shouldBe` [False, False, False, False]
