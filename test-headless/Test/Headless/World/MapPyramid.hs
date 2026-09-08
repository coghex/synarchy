{-# LANGUAGE Strict #-}
-- | #2298 (WML-5): the deterministic spatial map pyramid.
--
--   Everything here is GPU-free and engine-free except the two golden
--   examples, which need real 'WorldGenParams' and therefore a
--   generated world. The pure half injects its own cell source, so page
--   composition, gutters, wrapping, reduction and the level inventory
--   are all checked without generating a chunk.
--
--   The goldens are the load-bearing part. Their digests were captured
--   from the tree BEFORE this slice existed, by running the shipping
--   'buildZoomCacheWithPixels' and hashing the per-chunk blocks a page
--   covers. Comparing the new generator only against the legacy
--   function in the same change would not prove requirement 10 — both
--   could have moved together — so the pin is a literal, not a
--   recomputation.
module Test.Headless.World.MapPyramid (spec, worldSpec) where

import UPrelude
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import Data.IORef (readIORef)
import Numeric (showHex)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Test.Hspec
import Test.Headless.Harness (sharedWorld, getWorldGenParams)
import Engine.Core.State (EngineEnv, loggerRef, materialRegistryRef)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Generate.Types (WorldGenParams)
import World.Material (MaterialRegistry)
import World.Generate.Config.Normalize (minimumWorldSize)
import World.Map.ImagePlan
    ( MapImageCeiling(..), MapImageFormat(..), MapImagePlan(..)
    , MapImageSource(..), admitMapImage, mapImageRefusalText, planMapImage )
import World.Constants (seaLevel)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.ZoomMap.Cache.ChunkPass
    ( ZoomChunkPass(..), zoomChunkHaloNeighbours, zoomChunkInWorld
    , zoomChunkPass )
import World.ZoomMap.ColorPalette (ZoomColorPalette, buildColorPalette)
import World.ZoomMap.Pyramid
import World.ZoomMap.Types (zoomTileSize)

-- * Small helpers

accept ∷ HasCallStack ⇒ Either MapPyramidRefusal α → IO α
accept (Right a)  = pure a
accept (Left ref) =
    expectationFailure ("expected acceptance, got: "
                        ⧺ T.unpack (mapPyramidRefusalText ref))
        ≫ error "unreachable"

acceptAddress ∷ HasCallStack ⇒ Either MapAddressRefusal α → IO α
acceptAddress (Right a)  = pure a
acceptAddress (Left ref) =
    expectationFailure ("expected acceptance, got: "
                        ⧺ T.unpack (mapAddressRefusalText ref))
        ≫ error "unreachable"

acceptReduce ∷ HasCallStack ⇒ Either MapReduceRefusal α → IO α
acceptReduce (Right a)  = pure a
acceptReduce (Left ref) =
    expectationFailure ("expected acceptance, got: "
                        ⧺ T.unpack (mapReduceRefusalText ref))
        ≫ error "unreachable"

geometryFor ∷ HasCallStack ⇒ Int → IO MapGeometry
geometryFor = acceptAddress ∘ mapGeometry

-- | Re-read a generated page as a raster so windows of it can be
--   compared with windows of a whole level.
pageRaster ∷ HasCallStack ⇒ BS.ByteString → IO MapRaster
pageRaster = acceptReduce ∘ mapRasterFromBytes mapPageEdge mapPageEdge

inventoryFor ∷ HasCallStack ⇒ Int → IO MapPyramidInventory
inventoryFor = accept ∘ mapPyramidInventory

hex ∷ BS.ByteString → String
hex = concatMap byte ∘ BS.unpack
  where byte b = let s = showHex b "" in if length s ≡ 1 then '0' : s else s

-- | Every normalized world size this slice is designed through, and a
--   representative spread that deliberately includes non-powers of two.
allNormalizedSizes ∷ [Int]
allNormalizedSizes =
    [minimumWorldSize, 2 * minimumWorldSize .. mapDesignedMaxWorldSize]

representativeSizes ∷ [Int]
representativeSizes = [8, 24, 64, 72, 128, 344, 512, 1024, 2056, 8184, 8192]

-- * A synthetic cell source

-- | A cell tile whose every byte depends on the cell AND on the offset
--   within it, with genuinely transparent texels mixed in, so a wrong
--   cell, a wrong row, a wrong byte, or a lost alpha all show up as a
--   difference rather than cancelling out.
syntheticTile ∷ MapCell → BS.ByteString
syntheticTile (MapCell cu cv) = BS.pack $ concat
    [ [ channel 3, channel 5, channel 7, alpha ]
    | py ← [0 .. zoomTileSize - 1]
    , px ← [0 .. zoomTileSize - 1]
    , let mixed = (cu * 31 + cv) * 1009 + py * 37 + px * 11
          channel k = fromIntegral ((mixed * k) `mod` 251) ∷ Word8
          alpha = if (px + py + cu + cv) `mod` 9 ≡ 0
                  then 0
                  else fromIntegral (60 + mixed `mod` 196) ∷ Word8
    ]

syntheticSource ∷ MapCellSource
syntheticSource = MapCellSource (Right ∘ map syntheticTile)

-- * Reading a generated page

-- | One RGBA8 texel of a page's uploaded image.
pageTexel ∷ BS.ByteString → Int → Int → (Word8, Word8, Word8, Word8)
pageTexel bytes x y =
    let o = (y * mapPageEdge + x) * 4
    in ( BS.index bytes o, BS.index bytes (o + 1)
       , BS.index bytes (o + 2), BS.index bytes (o + 3) )

pageColumn ∷ BS.ByteString → Int → [(Word8, Word8, Word8, Word8)]
pageColumn bytes x = [ pageTexel bytes x y | y ← [0 .. mapPageEdge - 1] ]

pageRow ∷ BS.ByteString → Int → [(Word8, Word8, Word8, Word8)]
pageRow bytes y = [ pageTexel bytes x y | x ← [0 .. mapPageEdge - 1] ]

-- | The 'zoomTileSize'-square block a finest page's payload holds for
--   its @(col, row)@ cell position, in the same byte order
--   'World.ZoomMap.Cache.Pixels.generateChunkPixels' emits.
pageCellBlock ∷ BS.ByteString → Int → Int → BS.ByteString
pageCellBlock bytes col row = BS.pack $ concat
    [ [r, g, b, a]
    | py ← [0 .. zoomTileSize - 1]
    , px ← [0 .. zoomTileSize - 1]
    , let (r, g, b, a) = pageTexel bytes
              (mapPageGutter + col * zoomTileSize + px)
              (mapPageGutter + row * zoomTileSize + py)
    ]

-- | The smallest normalized world size whose root is level 2, so the
--   reduction tree is exercised carrying rows through one level and
--   into the next rather than stopping after a single adjacent step.
multiLevelWorldSize ∷ Int
multiLevelWorldSize = 136

-- | A page-payload-sized window of a whole level, read under the SAME
--   two rules a page samples its level by: longitude wraps by the
--   level's raster width, latitude outside the raster is transparent.
--
--   A plain 'mapRasterWindow' is not the right expectation for an edge
--   page. worldSize 136's finest level is 2176 texels wide and its five
--   page columns span 2560, so the last column legitimately shows the
--   start of the world again — while a plain window would simply run
--   out and report transparent. Wrapping here compares the overrun
--   against what is actually supposed to be there instead of excusing
--   it.
wholeLevelWindow ∷ MapRaster → Int → Int → MapRaster
wholeLevelWindow whole ox oy =
    MapRaster mapPagePayload mapPagePayload $
        VU.generate (mapPagePayload * mapPagePayload * 4) $ \i →
            let component = i `mod` 4
                pixel = i `div` 4
                x = ox + pixel `mod` mapPagePayload
                y = oy + pixel `div` mapPagePayload
                (rr, gg, bb, aa)
                  | y < 0 ∨ y ≥ mrHeight whole = (0, 0, 0, 0)
                  | otherwise =
                      mapRasterTexel whole (x `mod` mrWidth whole) y
            in case component of
                0 → rr
                1 → gg
                2 → bb
                _ → aa

-- | ONE 4x4 premultiplied box per output texel — the collapsed filter
--   D-16 forbids, written here only so a test can show that the
--   synthetic data tells it apart from two adjacent 2x2 steps. It is
--   deliberately NOT exported by the pyramid.
collapse4x4 ∷ MapRaster → MapRaster
collapse4x4 r = MapRaster w' h' $ VU.generate (w' * h' * 4) $ \i →
    let component = i `mod` 4
        pixel = i `div` 4
        x = pixel `mod` w'
        y = pixel `div` w'
        quad = [ mapRasterTexel r (4 * x + dx) (4 * y + dy)
               | dy ← [0 .. 3], dx ← [0 .. 3] ]
        alphaOf (_, _, _, a) = fromIntegral a ∷ Int
        chan f = sum [ fromIntegral (f q) * alphaOf q | q ← quad ] ∷ Int
        red (v, _, _, _) = v
        green (_, v, _, _) = v
        blue (_, _, v, _) = v
        alphaSum = sum (map alphaOf quad)
        narrow v = fromIntegral (max 0 (min 255 v)) ∷ Word8
        (rr, gg, bb, aa)
          | alphaSum ≡ 0 = (0, 0, 0, 0)
          | otherwise =
              ( narrow (divRoundHalfUp (chan red) alphaSum)
              , narrow (divRoundHalfUp (chan green) alphaSum)
              , narrow (divRoundHalfUp (chan blue) alphaSum)
              , narrow (divRoundHalfUp alphaSum 16) )
    in case component of
        0 → rr
        1 → gg
        2 → bb
        _ → aa
  where
    w' = mrWidth r `div` 4
    h' = mrHeight r `div` 4

cellsPerPageEdge ∷ Int
cellsPerPageEdge = mapPagePayload `div` zoomTileSize

-- * The pure spec

spec ∷ Spec
spec = describe "map pyramid (#2298)" $ do
    addressingSpec
    inventorySpec
    reductionSpec
    pageSpec
    ownershipSpec

-- ** Addressing

addressingSpec ∷ Spec
addressingSpec = describe "parity-compressed cylindrical addressing" $ do
    it "spends exactly one finest cell per physical chunk" $
      forM_ [64, 128, 512, 1024, 8192] $ \size → do
        geom ← geometryFor size
        (size, mgFinestCells geom) `shouldBe` (size, size * size `div` 2)

    it "matches the engine's own reported 2048 chunks at worldSize 64" $ do
        geom ← geometryFor 64
        -- The shipping engine logs "Zoom atlas: 1472×1440 (2048 chunks)"
        -- for a 64-chunk world; the pyramid must address the same
        -- population, not a different one.
        mgFinestCells geom `shouldBe` 2048
        mgCellsU geom `shouldBe` 32
        mgCellsV geom `shouldBe` 64

    it "round-trips every physical chunk through its finest cell" $
      forM_ [8, 24, 64] $ \size → do
        geom ← geometryFor size
        let h = mgLatticeSpan geom `div` 2
            chunks = [ ChunkCoord ((u + v) `div` 2) ((v - u) `div` 2)
                     | v ← [-h .. h - 1], u ← [-h .. h - 1], even (u + v) ]
        cells ← mapM (acceptAddress ∘ finestCellOfChunk geom) chunks
        back ← mapM (acceptAddress ∘ chunkOfFinestCell geom) cells
        back `shouldBe` chunks
        -- and it is a bijection, not just a total map
        Set.size (Set.fromList cells) `shouldBe` mgFinestCells geom

    it "round-trips every finest cell through its physical chunk" $
      forM_ [8, 24, 64] $ \size → do
        geom ← geometryFor size
        let cells = [ MapCell cu cv
                    | cv ← [0 .. mgCellsV geom - 1]
                    , cu ← [0 .. mgCellsU geom - 1] ]
        chunks ← mapM (acceptAddress ∘ chunkOfFinestCell geom) cells
        back ← mapM (acceptAddress ∘ finestCellOfChunk geom) chunks
        back `shouldBe` cells

    it "refuses odd-parity lattice coordinates instead of addressing them" $ do
        geom ← geometryFor 64
        finestCellOfUV geom 0 1 `shouldBe` Left (MapAddressOddParity 0 1)
        finestCellOfUV geom (-3) 0 `shouldBe` Left (MapAddressOddParity (-3) 0)
        -- and every odd coordinate in a whole small world
        small ← geometryFor 8
        let h = mgLatticeSpan small `div` 2
            odds = [ (u, v) | v ← [-h .. h - 1], u ← [-h .. h - 1]
                            , odd (u + v) ]
        odds `shouldSatisfy` (not ∘ null)
        forM_ odds $ \(u, v) →
            finestCellOfUV small u v `shouldBe` Left (MapAddressOddParity u v)

    it "pins the addressing contract at fixed spatial landmarks" $ do
        geom ← geometryFor 64
        -- Canonical origin: the lowest longitude of the lowest latitude.
        finestCellOfUV geom (-32) (-32) `shouldBe` Right (MapCell 0 0)
        chunkOfFinestCell geom (MapCell 0 0)
            `shouldBe` Right (ChunkCoord (-32) 0)
        -- The opposite corner of the same latitude row.
        finestCellOfUV geom 30 (-32) `shouldBe` Right (MapCell 31 0)
        -- One latitude up: the row's parity has flipped, so its cells
        -- sit on ODD lattice longitudes.
        finestCellOfUV geom (-31) (-31) `shouldBe` Right (MapCell 0 1)
        uvOfFinestCell geom (MapCell 0 1) `shouldBe` Right (-31, -31)
        -- The far corner.
        finestCellOfUV geom 31 31 `shouldBe` Right (MapCell 31 63)
        chunkOfFinestCell geom (MapCell 31 63) `shouldBe` Right (ChunkCoord 31 0)

    it "pins the parity transition between adjacent latitude rows" $ do
        geom ← geometryFor 64
        -- A cell's spatial diagonal neighbours are the lattice
        -- coordinates (u ± 1, v + 1). Under the compression they land
        -- on a DIFFERENT compressed column depending on the row's
        -- parity — the brick stagger. Getting this wrong is a
        -- self-consistent but spatially wrong permutation.
        let diagonal du cu cv = case uvOfFinestCell geom (MapCell cu cv) of
                Left ref → Left ref
                Right (u, v) → finestCellOfUV geom (u + du) (v + 1)
            downRight = diagonal 1
            downLeft  = diagonal (-1)
        -- even row
        downRight 5 10 `shouldBe` Right (MapCell 5 11)
        downLeft  5 10 `shouldBe` Right (MapCell 4 11)
        -- odd row
        downRight 5 11 `shouldBe` Right (MapCell 6 12)
        downLeft  5 11 `shouldBe` Right (MapCell 5 12)

    it "aliases longitude and refuses latitude" $ do
        geom ← geometryFor 64
        -- u + w is the same place.
        finestCellOfUV geom (-32) (-32) `shouldBe` finestCellOfUV geom 32 (-32)
        finestCellOfUV geom 30 (-32) `shouldBe` finestCellOfUV geom (-34) (-32)
        normalizeMapCell geom (MapCell (-1) 7) `shouldBe` Right (MapCell 31 7)
        normalizeMapCell geom (MapCell 32 7) `shouldBe` Right (MapCell 0 7)
        -- v has no alias.
        finestCellOfUV geom 0 32
            `shouldBe` Left (MapAddressLatitudeOutOfBounds 32 (-32) 31)
        normalizeMapCell geom (MapCell 0 64)
            `shouldBe` Left (MapAddressLatitudeOutOfBounds 64 0 63)

    it "forms world-scale products in Integer and refuses an overflow" $ do
        -- Well past any world the engine could generate, but exactly
        -- the arithmetic the addressing must not perform in a wrapping
        -- 'Int': the finest cell count of a 8e9-chunk world is 3.2e19.
        case mapGeometry 8000000000 of
            Right geom → expectationFailure
                ("expected a refusal, got " ⧺ show (mgFinestCells geom))
            Left ref → mapAddressRefusalText ref
                `shouldSatisfy` T.isInfixOf "does not fit a host Int"
        geom ← geometryFor 8192
        (mgFinestWidth geom, mgFinestHeight geom) `shouldBe` (131072, 262144)
        toInteger (mgFinestCells geom) `shouldBe` 8192 * 8192 `div` 2

    it "refuses a world size the normalizer would not produce" $ do
        mapGeometry 0 `shouldSatisfy` isLeftAddress
        mapGeometry 7 `shouldSatisfy` isLeftAddress
        mapGeometry (-64) `shouldSatisfy` isLeftAddress
        mapGeometry 12 `shouldSatisfy` isLeftAddress
        mapGeometry minimumWorldSize `shouldSatisfy` (not ∘ isLeftAddress)

    it "selects levels by the stated caps, derived rather than tabulated" $
      forM_ allNormalizedSizes $ \size → do
        geom ← geometryFor size
        let root = mapRootLevel geom
            cutoff = mapCoarseCutoffLevel geom
            longest l = max (mapLevelWidth geom l) (mapLevelHeight geom l)
        -- the root is the SMALLEST level under its cap
        (size, longest root ≤ mapRootAxisCap) `shouldBe` (size, True)
        when (root > 0) $
            (size, longest (root - 1) > mapRootAxisCap) `shouldBe` (size, True)
        -- likewise the mandatory-coarse cutoff, which is never above it
        (size, longest cutoff ≤ mapCoarseAxisCap) `shouldBe` (size, True)
        when (cutoff > 0) $
            (size, longest (cutoff - 1) > mapCoarseAxisCap)
                `shouldBe` (size, True)
        (size, cutoff ≤ root) `shouldBe` (size, True)

    it "keeps every reduced level even, so no reduction is ever ambiguous" $
      forM_ allNormalizedSizes $ \size → do
        geom ← geometryFor size
        let root = mapRootLevel geom
            oddLevels = [ l | l ← [0 .. root - 1]
                        , odd (mapLevelWidth geom l)
                          ∨ odd (mapLevelHeight geom l) ]
        (size, oddLevels) `shouldBe` (size, [])

    it "keeps page keys canonical and parents/children consistent" $ do
        geom ← geometryFor 512
        let root = mapRootLevel geom
        root `shouldBe` 3
        checkMapPageKey geom (MapPageKey 0 0 0) `shouldBe` Right ()
        checkMapPageKey geom (MapPageKey (root + 1) 0 0)
            `shouldBe` Left (MapAddressLevelOutOfRange (root + 1) root)
        checkMapPageKey geom (MapPageKey 0 (-1) 0) `shouldSatisfy` isLeftAddress
        checkMapPageKey geom (MapPageKey 0 (mapLevelPagesU geom 0) 0)
            `shouldSatisfy` isLeftAddress
        mapPageParent geom (MapPageKey 0 5 9) `shouldBe` Just (MapPageKey 1 2 4)
        mapPageParent geom (MapPageKey root 0 0) `shouldBe` Nothing
        mapPageChildren geom (MapPageKey 1 2 4) `shouldBe`
            [ MapPageKey 0 4 8, MapPageKey 0 5 8
            , MapPageKey 0 4 9, MapPageKey 0 5 9 ]
        -- every finest page's parent chain terminates at the root
        forM_ [ MapPageKey 0 pu pv
              | pu ← [0 .. mapLevelPagesU geom 0 - 1]
              , pv ← [0 .. mapLevelPagesV geom 0 - 1] ] $ \key →
            climb geom key `shouldBe` root

    it "names a chunk's four cardinal halo neighbours, wrapped and bounded" $ do
        -- The halo a page's pass two reads. Its SHAPE is pinned here
        -- because a halo that quietly became empty would leave every
        -- other assertion in this file green on a world where the
        -- cross-chunk extension happens to be inert.
        L.sort (zoomChunkHaloNeighbours 64 (ChunkCoord 0 0)) `shouldBe`
            L.sort [ ChunkCoord 1 0, ChunkCoord (-1) 0
                   , ChunkCoord 0 1, ChunkCoord 0 (-1) ]
        -- At the latitude edge (v = ccx + ccy = 31 is the last row) the
        -- two neighbours that would leave the world are dropped, and
        -- the one that leaves the canonical longitude range comes back
        -- wrapped rather than missing.
        L.sort (zoomChunkHaloNeighbours 64 (ChunkCoord 31 0)) `shouldBe`
            L.sort [ ChunkCoord 30 0, ChunkCoord (-1) 31 ]
        -- Latitude does not wrap, so the far edge loses neighbours too.
        L.sort (zoomChunkHaloNeighbours 64 (ChunkCoord (-32) 0)) `shouldBe`
            L.sort [ ChunkCoord (-31) 0, ChunkCoord 0 (-31) ]
        zoomChunkInWorld 64 (ChunkCoord 31 0) `shouldBe` True
        zoomChunkInWorld 64 (ChunkCoord 31 1) `shouldBe` False
        zoomChunkInWorld 64 (ChunkCoord (-32) (-1)) `shouldBe` False

    it "places every finest cell in the page that covers it" $ do
        geom ← geometryFor 128
        forM_ [ MapCell cu cv
              | cv ← [0 .. mgCellsV geom - 1]
              , cu ← [0 .. mgCellsU geom - 1] ] $ \cell → do
            key ← acceptAddress (mapPageOfFinestCell geom cell)
            covered ← acceptAddress (mapPageFinestCells geom key)
            (cell, cell `elem` [ c | (_, _, c) ← covered ])
                `shouldBe` (cell, True)
  where
    isLeftAddress (Left _) = True
    isLeftAddress _        = False
    climb geom key = case mapPageParent geom key of
        Nothing → mpkLevel key
        Just up → climb geom up

-- ** Inventory

inventorySpec ∷ Spec
inventorySpec = describe "streamable level inventory" $ do
    it "prices one page as 514×514 and 1,056,784 decoded bytes" $ do
        plan ← acceptPlan $
            planMapImage MapImageRGBA8 (TiledImageSource 1 mapPageEdge)
        (mipWidth plan, mipHeight plan) `shouldBe` (514, 514)
        mipByteCount plan `shouldBe` 1056784
        mapPageEdge `shouldBe` 514
        mapPagePayload `shouldBe` 512
        mapPagePayload `div` zoomTileSize `shouldBe` 16

    it "prices every level's pages through the planner, at every size" $
      forM_ representativeSizes $ \size → do
        inv ← inventoryFor size
        forM_ (mpiLevels inv) $ \row → do
            (size, mipWidth (mplPagePlan row)) `shouldBe` (size, 514)
            (size, mipHeight (mplPagePlan row)) `shouldBe` (size, 514)
            (size, mipByteCount (mplPagePlan row)) `shouldBe` (size, 1056784)
            (size, mplPageCount row)
                `shouldBe` (size, mplPagesU row * mplPagesV row)
            (size, toInteger (mplDecodedBytes row))
                `shouldBe` (size, toInteger (mplPageCount row) * 1056784)

    it "names exact page counts and byte totals for the known sizes" $ do
        w64 ← inventoryFor 64
        map (\r → (mplLevel r, mplWidth r, mplHeight r, mplPagesU r
                  , mplPagesV r, mplPageCount r, mplDecodedBytes r))
            (mpiLevels w64)
            `shouldBe` [(0, 1024, 2048, 2, 4, 8, 8 * 1056784)]
        (mpiRootLevel w64, mpiCoarseCutoffLevel w64) `shouldBe` (0, 0)

        w128 ← inventoryFor 128
        map (\r → (mplLevel r, mplWidth r, mplHeight r, mplPageCount r))
            (mpiLevels w128)
            `shouldBe` [(0, 2048, 4096, 32), (1, 1024, 2048, 8)]
        (mpiRootLevel w128, mpiCoarseCutoffLevel w128) `shouldBe` (1, 0)

        w1024 ← inventoryFor 1024
        map (\r → (mplLevel r, mplWidth r, mplHeight r, mplPageCount r))
            (mpiLevels w1024)
            `shouldBe` [ (0, 16384, 32768, 2048), (1, 8192, 16384, 512)
                       , (2, 4096, 8192, 128), (3, 2048, 4096, 32)
                       , (4, 1024, 2048, 8) ]
        (mpiRootLevel w1024, mpiCoarseCutoffLevel w1024) `shouldBe` (4, 3)
        mpiMandatoryBytes w1024 `shouldBe` (32 + 8) * 1056784

    it "keeps the root inside D-7's 2048-pixel cap at every size" $
      forM_ representativeSizes $ \size → do
        inv ← inventoryFor size
        let plan = mpiRootPlan inv
        (size, max (mipWidth plan) (mipHeight plan) ≤ mapRootAxisCap)
            `shouldBe` (size, True)
        (size, mipSource plan) `shouldBe`
            (size, WholeImageSource (mipWidth plan) (mipHeight plan))
        (size, mipByteCount plan)
            `shouldBe` (size, mipWidth plan * mipHeight plan * 4)

    it "stays valid and enumerable at the designed worldSize 8192" $ do
        inv ← inventoryFor mapDesignedMaxWorldSize
        mpiRootLevel inv `shouldBe` 7
        mpiCoarseCutoffLevel inv `shouldBe` 6
        map (\r → (mplLevel r, mplPageCount r)) (mpiLevels inv)
            `shouldBe` [ (0, 131072), (1, 32768), (2, 8192), (3, 2048)
                       , (4, 512), (5, 128), (6, 32), (7, 8) ]
        mpiMandatoryBytes inv `shouldBe` (32 + 8) * 1056784
        -- the finest level alone is 138 GB of decoded pages, which is
        -- exactly why nothing here materialises one.
        finest ← accept (mapPyramidLevel inv 0)
        toInteger (mplDecodedBytes finest) `shouldBe` 131072 * 1056784

    it "accepts every normalized size through the designed ceiling" $
      forM_ allNormalizedSizes $ \size →
        case mapPyramidInventory size of
            Right inv → (size, length (mpiLevels inv))
                `shouldBe` (size, mpiRootLevel inv + 1)
            Left ref → expectationFailure
                (show size ⧺ ": " ⧺ T.unpack (mapPyramidRefusalText ref))

    it "refuses an unsupported size before any level is priced" $ do
        mapPyramidInventory 7 `shouldSatisfy` isLeftPyramid
        mapPyramidInventory 0 `shouldSatisfy` isLeftPyramid
        mapPyramidInventory (-8) `shouldSatisfy` isLeftPyramid

    it "refuses an arithmetically invalid image before any allocation" $ do
        -- The planner is the admission gate; these are the refusals the
        -- inventory inherits rather than reimplements.
        planMapImage MapImageRGBA8 (TiledImageSource 0 mapPageEdge)
            `shouldSatisfy` isLeftImage
        planMapImage MapImageRGBA8 (TiledImageSource 1 0)
            `shouldSatisfy` isLeftImage
        planMapImage MapImageRGBA8 (WholeImageSource 0 514)
            `shouldSatisfy` isLeftImage
        refusal ← refusalTextOf $
            planMapImage MapImageRGBA8 (WholeImageSource maxBound maxBound)
        refusal `shouldSatisfy` T.isInfixOf "does not fit"
        -- and the device ceiling refuses an otherwise valid page before
        -- anything is allocated. The inventory itself plans WITHOUT a
        -- ceiling on purpose: whether one applies is a boot-mode
        -- question "Engine.Map.ImageAdmission" owns, not this module.
        overLimit ← refusalTextOf $ admitMapImage (CeilingKnown 256)
            MapImageRGBA8 (TiledImageSource 1 mapPageEdge)
        overLimit `shouldSatisfy` T.isInfixOf "maxImageDimension2D"
        admitMapImage CeilingNotApplicable MapImageRGBA8
            (TiledImageSource 1 mapPageEdge) `shouldSatisfy` (not ∘ isLeftImage)

    it "answers the mandatory range as cutoff through root inclusive" $ do
        inv ← inventoryFor 1024
        map mplLevel (mapPyramidMandatoryLevels inv) `shouldBe` [3, 4]
        mapPyramidLevel inv 9 `shouldSatisfy` isLeftPyramid
  where
    isLeftPyramid (Left _) = True
    isLeftPyramid _        = False
    isLeftImage (Left _) = True
    isLeftImage _        = False
    acceptPlan (Right p) = pure p
    acceptPlan (Left r)  =
        expectationFailure (T.unpack (mapImageRefusalText r))
            ≫ error "unreachable"
    refusalTextOf (Left r)  = pure (mapImageRefusalText r)
    refusalTextOf (Right _) =
        expectationFailure "expected a refusal" ≫ error "unreachable"

-- ** Reduction

reductionSpec ∷ Spec
reductionSpec = describe "premultiplied 2x2 reduction (D-16)" $ do
    it "names one rounding rule and rounds halves up" $ do
        mapReductionRuleName `shouldBe`
            "premultiplied 2x2 box reduction with half-up integer rounding"
        divRoundHalfUp 0 4 `shouldBe` 0
        divRoundHalfUp 1 2 `shouldBe` 1     -- 0.5 → up
        divRoundHalfUp 3 2 `shouldBe` 2     -- 1.5 → up
        divRoundHalfUp 1 3 `shouldBe` 0     -- 0.333 → down
        divRoundHalfUp 2 3 `shouldBe` 1     -- 0.667 → up
        divRoundHalfUp 255 4 `shouldBe` 64  -- 63.75 → up

    it "is the ordinary four-pixel mean where everything is opaque" $ do
        reduceQuad (10, 0, 0, 255) (20, 0, 0, 255)
                   (30, 0, 0, 255) (42, 0, 0, 255)
            `shouldBe` (26, 0, 0, 255)      -- 102 / 4 = 25.5 → 26
        reduceQuad (10, 20, 30, 255) (20, 40, 60, 255)
                   (30, 60, 90, 255) (40, 80, 120, 255)
            `shouldBe` (25, 50, 75, 255)

    it "differs from the naive mean when alpha is mixed" $ do
        -- Σ(c·a) = 100·200 + 200·100 + 0 + 60·50 = 43000, Σa = 350.
        -- Premultiplied: 43000/350 = 122.86 → 123.
        -- The naive mean would be (100+200+0+60)/4 = 90.
        reduceQuad (100, 100, 100, 200) (200, 200, 200, 100)
                   (0, 0, 0, 0)         (60, 60, 60, 50)
            `shouldBe` (123, 123, 123, 88)

    it "invents no colour for a fully transparent neighbourhood" $ do
        reduceQuad (0, 0, 0, 0) (0, 0, 0, 0) (0, 0, 0, 0) (0, 0, 0, 0)
            `shouldBe` (0, 0, 0, 0)
        -- transparent texels that still carry colour bytes contribute
        -- nothing at all
        reduceQuad (250, 250, 250, 0) (1, 2, 3, 0)
                   (9, 9, 9, 0)       (77, 88, 99, 0)
            `shouldBe` (0, 0, 0, 0)

    it "gives an edge neighbourhood no black fringe" $ do
        -- One opaque texel beside three transparent ones keeps its own
        -- colour exactly and only loses alpha. A straight mean would
        -- drag it to (50, 25, 12) and look like a dark halo.
        reduceQuad (200, 100, 50, 255) (0, 0, 0, 0)
                   (0, 0, 0, 0)        (0, 0, 0, 0)
            `shouldBe` (200, 100, 50, 64)

    it "refuses a raster it cannot pair, and pairs an even one" $ do
        odd' ← acceptReduce (mapRasterFromBytes 3 2 (BS.replicate (3 * 2 * 4) 7))
        reduceMapRaster odd' `shouldBe` Left (MapReduceOddRaster 3 2)
        even' ← acceptReduce (mapRasterFromBytes 2 2 (BS.replicate 16 200))
        fmap (\r → (mrWidth r, mrHeight r)) (reduceMapRaster even')
            `shouldBe` Right (1, 1)
        mapRasterFromBytes 2 2 (BS.replicate 15 0)
            `shouldBe` Left (MapReduceMalformedRaster 2 2 15)

    it "reduces a hand-built raster exactly, texel by texel" $ do
        -- Two 2×2 neighbourhoods side by side: an opaque one and an
        -- edge one, so both cases are visible in a single raster.
        let texels = concat
                [ [10,0,0,255, 20,0,0,255,  200,100,50,255, 0,0,0,0]
                , [30,0,0,255, 42,0,0,255,  0,0,0,0,        0,0,0,0] ]
        raster ← acceptReduce (mapRasterFromBytes 4 2 (BS.pack texels))
        reduced ← acceptReduce (reduceMapRaster raster)
        (mrWidth reduced, mrHeight reduced) `shouldBe` (2, 1)
        mapRasterTexel reduced 0 0 `shouldBe` (26, 0, 0, 255)
        mapRasterTexel reduced 1 0 `shouldBe` (200, 100, 50, 64)

-- ** Pages

pageSpec ∷ Spec
pageSpec = describe "page composition, gutters and seams" $ do
    it "produces exactly one planned page image" $ do
        inv ← inventoryFor 64
        bytes ← accept (mapPageImage inv syntheticSource (MapPageKey 0 0 0))
        BS.length bytes `shouldBe` 1056784
        BS.length bytes `shouldBe` mapPageEdge * mapPageEdge * 4

    it "composes each finest cell into its exact spatial position" $ do
        geom ← geometryFor 64
        inv ← inventoryFor 64
        let key = MapPageKey 0 1 2
        bytes ← accept (mapPageImage inv syntheticSource key)
        forM_ [ (col, row) | row ← [0 .. cellsPerPageEdge - 1]
                           , col ← [0 .. cellsPerPageEdge - 1] ] $
            \(col, row) → do
                let cell = MapCell (mpkPageU key * cellsPerPageEdge + col)
                                   (mpkPageV key * cellsPerPageEdge + row)
                (col, row, pageCellBlock bytes col row)
                    `shouldBe` (col, row, syntheticTile cell)
        -- and the page's own cell list agrees with that placement
        covered ← acceptAddress (mapPageFinestCells geom key)
        length covered `shouldBe` cellsPerPageEdge * cellsPerPageEdge
        covered `shouldBe`
            [ (col, row, MapCell (mpkPageU key * cellsPerPageEdge + col)
                                 (mpkPageV key * cellsPerPageEdge + row))
            | row ← [0 .. cellsPerPageEdge - 1]
            , col ← [0 .. cellsPerPageEdge - 1] ]
        mapPageFinestCells geom (MapPageKey 1 0 0)
            `shouldBe` Left (MapAddressNotFinestLevel 1)

    it "fills an interior page's gutter from its neighbour's payload edge" $ do
        inv ← inventoryFor 128
        let left  = MapPageKey 0 1 2
            right = MapPageKey 0 2 2
            above = MapPageKey 0 1 1
        leftBytes  ← accept (mapPageImage inv syntheticSource left)
        rightBytes ← accept (mapPageImage inv syntheticSource right)
        aboveBytes ← accept (mapPageImage inv syntheticSource above)
        -- The gutter equals the ADJACENT page's nearest PAYLOAD edge,
        -- not its opposite gutter; both directions are checked, so a
        -- one-texel slip in either page shows up.
        pageColumn leftBytes (mapPageEdge - 1)
            `shouldBe` pageColumn rightBytes mapPageGutter
        pageColumn rightBytes 0
            `shouldBe` pageColumn leftBytes (mapPageEdge - 2)
        pageRow leftBytes 0
            `shouldBe` pageRow aboveBytes (mapPageEdge - 2)
        pageRow aboveBytes (mapPageEdge - 1)
            `shouldBe` pageRow leftBytes mapPageGutter

    it "wraps the gutter across the longitude seam" $ do
        inv ← inventoryFor 64
        -- worldSize 64 has exactly two page columns, so the last page's
        -- right gutter is the first page's left payload column and vice
        -- versa: the cylinder closes.
        firstBytes ← accept (mapPageImage inv syntheticSource (MapPageKey 0 0 1))
        lastBytes  ← accept (mapPageImage inv syntheticSource (MapPageKey 0 1 1))
        pageColumn lastBytes (mapPageEdge - 1)
            `shouldBe` pageColumn firstBytes mapPageGutter
        pageColumn firstBytes 0
            `shouldBe` pageColumn lastBytes (mapPageEdge - 2)

    it "leaves the bounded latitude edge transparent" $ do
        inv ← inventoryFor 64
        geom ← geometryFor 64
        topBytes ← accept (mapPageImage inv syntheticSource (MapPageKey 0 0 0))
        let lastRow = mapLevelPagesV geom 0 - 1
        bottomBytes ← accept
            (mapPageImage inv syntheticSource (MapPageKey 0 0 lastRow))
        pageRow topBytes 0 `shouldSatisfy` all (≡ (0, 0, 0, 0))
        pageRow bottomBytes (mapPageEdge - 1)
            `shouldSatisfy` all (≡ (0, 0, 0, 0))
        -- the first payload row is NOT transparent, so the assertion
        -- above is about the gutter rather than about an empty world
        pageRow topBytes mapPageGutter `shouldSatisfy` any (≢ (0, 0, 0, 0))

    it "gives a partial edge page deterministic wrap and transparency" $ do
        -- worldSize 8 is 4 cells wide and 8 tall: one page whose 512×512
        -- payload runs past both, so every unused texel is decided by
        -- the same two rules rather than by whatever was in memory.
        inv ← inventoryFor 8
        geom ← geometryFor 8
        mapLevelPagesU geom 0 `shouldBe` 1
        mapLevelPagesV geom 0 `shouldBe` 1
        bytes ← accept (mapPageImage inv syntheticSource (MapPageKey 0 0 0))
        let wrapPeriod = mgCellsU geom * zoomTileSize   -- 128 texels
            worldRows = mgCellsV geom * zoomTileSize    -- 256 texels
        forM_ [0 .. mapPageEdge - 1 - wrapPeriod] $ \i →
            forM_ [1, 200] $ \j →
                (i, j, pageTexel bytes i j)
                    `shouldBe` (i, j, pageTexel bytes (i + wrapPeriod) j)
        forM_ [worldRows + mapPageGutter .. mapPageEdge - 1] $ \j →
            (j, pageRow bytes j) `shouldSatisfy`
                \(_, row) → all (≡ (0, 0, 0, 0)) row

    it "agrees between whole-level, pagewise and streamed execution" $ do
        -- worldSize 136 is the SMALLEST normalized size whose root is
        -- level 2, so the reduction tree has to carry rows through
        -- level 1 and into level 2 rather than stopping after a single
        -- adjacent step. The whole level is materialised and reduced by
        -- 'reduceMapRaster'; each page is produced by the independent
        -- streaming path. They are two executions of the same
        -- adjacent-level sequence, and at level 2 they are two
        -- executions of that sequence TWICE.
        inv ← inventoryFor multiLevelWorldSize
        geom ← geometryFor multiLevelWorldSize
        mpiRootLevel inv `shouldBe` 2
        forM_ [0, 1, 2] $ \level → do
            whole ← accept (mapLevelRaster inv syntheticSource level)
            let pagesU = mapLevelPagesU geom level
                pagesV = mapLevelPagesV geom level
                -- every page of the coarsest level, whose pages are
                -- the ones carried furthest through the tree; corners
                -- and an interior page of the larger finer levels
                keys | level ≡ 2 = [ MapPageKey level pu pv
                                   | pu ← [0 .. pagesU - 1]
                                   , pv ← [0 .. pagesV - 1] ]
                     | otherwise = [ MapPageKey level pu pv
                                   | (pu, pv) ← [ (0, 0), (pagesU - 1, 0)
                                                , (0, pagesV - 1)
                                                , (pagesU - 1, pagesV - 1)
                                                , (1, 2) ] ]
            forM_ keys $ \key → do
                bytes ← accept (mapPageImage inv syntheticSource key)
                page ← pageRaster bytes
                let (ox, oy) = mapPageTexelOrigin key
                    fromWhole = wholeLevelWindow whole ox oy
                    fromPage = mapRasterWindow page mapPageGutter mapPageGutter
                                    mapPagePayload mapPagePayload
                (key, fromPage ≡ fromWhole) `shouldBe` (key, True)

    it "reduces through every intermediate level, not in one wide box" $ do
        -- D-16 specifies REPEATED ADJACENT reduction, and RGBA8
        -- rounding is not associative across levels, so the pyramid
        -- must never collapse two levels into one wide box.
        --
        -- One adjacent step cannot tell those apart: a 4x4 box and two
        -- 2x2 boxes agree trivially at level 1. Level 2 is where they
        -- diverge, which is why this runs at the smallest world size
        -- that HAS a level 2.
        inv ← inventoryFor multiLevelWorldSize
        finest ← accept (mapLevelRaster inv syntheticSource 0)
        once ← acceptReduce (reduceMapRaster finest)
        twice ← acceptReduce (reduceMapRaster once)
        viaLevel1 ← accept (mapLevelRaster inv syntheticSource 1)
        viaLevel2 ← accept (mapLevelRaster inv syntheticSource 2)
        once `shouldBe` viaLevel1
        twice `shouldBe` viaLevel2

        -- and the data really does distinguish the two: collapsing the
        -- same finest raster in ONE 4x4 premultiplied box gives a
        -- different answer, so the equality above is evidence rather
        -- than a coincidence of uniform texels.
        let collapsed = collapse4x4 finest
        (mrWidth collapsed, mrHeight collapsed)
            `shouldBe` (mrWidth twice, mrHeight twice)
        collapsed `shouldNotBe` twice

    it "carries a coarse page through the tree in any order" $ do
        -- Evaluation-order independence where it can actually bite: a
        -- level-2 page's rows pass through one pending row per level,
        -- so a tree that leaked state between pages would show up here
        -- and not at level 0.
        inv ← inventoryFor multiLevelWorldSize
        geom ← geometryFor multiLevelWorldSize
        let keys = [ MapPageKey 2 pu pv
                   | pu ← [0 .. mapLevelPagesU geom 2 - 1]
                   , pv ← [0 .. mapLevelPagesV geom 2 - 1] ]
        forwards ← mapM (accept ∘ mapPageImage inv syntheticSource) keys
        backwards ← mapM (accept ∘ mapPageImage inv syntheticSource)
                         (reverse keys)
        again ← mapM (accept ∘ mapPageImage inv syntheticSource) keys
        forwards `shouldBe` again
        reverse backwards `shouldBe` forwards

    it "is deterministic across repeated and reordered generation" $ do
        inv ← inventoryFor 64
        let keys = [ MapPageKey 0 pu pv | pu ← [0, 1], pv ← [0 .. 3] ]
        forwards ← mapM (accept ∘ mapPageImage inv syntheticSource) keys
        backwards ← mapM (accept ∘ mapPageImage inv syntheticSource)
                         (reverse keys)
        again ← mapM (accept ∘ mapPageImage inv syntheticSource) keys
        forwards `shouldBe` again
        reverse backwards `shouldBe` forwards

    it "refuses a page key or a cell source that does not hold up" $ do
        inv ← inventoryFor 64
        mapPageImage inv syntheticSource (MapPageKey 0 99 0)
            `shouldSatisfy` isLeft'
        mapPageImage inv syntheticSource (MapPageKey 9 0 0)
            `shouldSatisfy` isLeft'
        let shortSource = MapCellSource $ \cells →
                Right (map (BS.take 10 ∘ syntheticTile) cells)
            emptySource = MapCellSource (const (Right []))
            angrySource = MapCellSource (const (Left "declined"))
        mapPageImage inv shortSource (MapPageKey 0 0 0)
            `shouldSatisfy` isLeft'
        mapPageImage inv emptySource (MapPageKey 0 0 0)
            `shouldSatisfy` isLeft'
        text ← refusalOf (mapPageImage inv angrySource (MapPageKey 0 0 0))
        text `shouldSatisfy` T.isInfixOf "declined"
  where
    isLeft' (Left _) = True
    isLeft' _        = False
    refusalOf (Left r)  = pure (mapPyramidRefusalText r)
    refusalOf (Right _) =
        expectationFailure "expected a refusal" ≫ error "unreachable"

-- ** Ownership

-- | Drop Haskell comments, so a doc comment that NAMES a forbidden
--   dependency is not mistaken for one. Both comment forms are
--   handled, nesting included; the pragma at the top of a module is a
--   block comment and goes with them.
--
--   The one shape this does not model is a @--@ inside a string
--   literal, which would hide the rest of that line. Nothing in the
--   tree has one, and the import rules below do not depend on this
--   function at all, so a body scan is the second of two independent
--   checks rather than the only one.
stripComments ∷ String → String
stripComments = go (0 ∷ Int)
  where
    go 0 ('-' : '-' : rest) = ' ' : go 0 (dropWhile (≢ '\n') rest)
    go depth ('{' : '-' : rest) = ' ' : go (depth + 1) rest
    go depth ('-' : '}' : rest)
      | depth ≤ 1 = ' ' : go 0 rest
      | otherwise = go (depth - 1) rest
    go 0 (c : rest) = c : go 0 rest
    go depth (_ : rest) = go depth rest
    go _ [] = []

-- | The module names a Haskell source actually imports.
importedModules ∷ String → [String]
importedModules body =
    [ moduleName rest
    | line ← lines (stripComments body)
    , Just rest ← [L.stripPrefix "import " (dropWhile (≡ ' ') line)] ]
  where
    moduleName rest =
        let afterQualified = fromMaybe rest (L.stripPrefix "qualified " rest)
        in takeWhile (\c → c ≢ ' ' ∧ c ≢ '(') (dropWhile (≡ ' ') afterQualified)

-- | Why a module under @src\/World\/ZoomMap@ would break the
--   producer-side direction the whole map arc rests on (D-1).
--
--   Two independent checks: what the module IMPORTS, which is the only
--   way a dependency can enter it at all, and what its code MENTIONS.
--   The umbrella "World.Types" is refused by name because it re-exports
--   both @World.Render.*@ and 'LoadedChunk', which would make the other
--   checks vacuous.
ownershipViolations ∷ String → [String]
ownershipViolations body =
    [ "imports " ⧺ m
    | m ← importedModules body
    , "World.Render" `L.isPrefixOf` m ∨ m ≡ "World.Types"
      -- The chunk residency/demand surface: importing any of it is how
      -- a map operation would come to enqueue detailed chunk work.
      ∨ "World.Thread" `L.isPrefixOf` m
      ∨ m `elem` [ "World.Chunk.Queue", "World.Chunk.Residency"
                 , "World.Chunk.Admit" ] ]
    ⧺ [ "mentions " ⧺ token
      | token ← ["World.Render.", "LoadedChunk"]
      , token `L.isInfixOf` stripComments body ]

ownershipSpec ∷ Spec
ownershipSpec = describe "producer-side ownership" $ do
    it "sees a real dependency and ignores one that is only named" $ do
        -- Without this, an ownership check that silently matched
        -- nothing would look exactly like a clean tree.
        ownershipViolations
            "import World.Render.Zoom.Types (X)\nf = 1\n"
            `shouldBe` ["imports World.Render.Zoom.Types", "mentions World.Render."]
        ownershipViolations "import qualified World.Types as T\n"
            `shouldSatisfy` elem "imports World.Types"
        ownershipViolations "f ∷ LoadedChunk → Int\n"
            `shouldBe` ["mentions LoadedChunk"]
        ownershipViolations "import World.Chunk.Queue (queueChunk)\n"
            `shouldBe` ["imports World.Chunk.Queue"]
        ownershipViolations "import World.Thread.ChunkLoading\n"
            `shouldBe` ["imports World.Thread.ChunkLoading"]
        -- the neighbouring module that is ALLOWED stays allowed
        ownershipViolations "import World.Chunk.Types (ChunkCoord(..))\n"
            `shouldBe` []
        -- but the same words inside comments are documentation
        ownershipViolations
            "-- never import World.Render. and never touch LoadedChunk\nf = 1\n"
            `shouldBe` []
        ownershipViolations
            "{- World.Render. LoadedChunk {- nested -} -}\nf = 1\n"
            `shouldBe` []
        importedModules "import Data.List (sort)\nimport qualified Data.Map as M\n"
            `shouldBe` ["Data.List", "Data.Map"]

    it "keeps every World.ZoomMap module clear of the renderer" $ do
        sources ← haskellSourcesUnder "src/World/ZoomMap"
        sources `shouldSatisfy` ((> 10) ∘ length)
        offenders ← forM sources $ \path → do
            body ← readFile path
            pure (path, ownershipViolations body)
        filter (not ∘ null ∘ snd) offenders `shouldBe` []

    it "leaves the pyramid with no production caller in this slice" $ do
        sources ← (⧺) ⊚ haskellSourcesUnder "src" ⊛ haskellSourcesUnder "app"
        callers ← forM sources $ \path → do
            body ← readFile path
            let pulls = [ m | m ← importedModules body
                        , "World.ZoomMap.Pyramid" `L.isPrefixOf` m ]
            pure (path, if "src/World/ZoomMap/Pyramid" `L.isPrefixOf` path
                        then [] else pulls)
        filter (not ∘ null ∘ snd) callers `shouldBe` []

haskellSourcesUnder ∷ FilePath → IO [FilePath]
haskellSourcesUnder root = do
    present ← doesDirectoryExist root
    if not present then pure [] else go root
  where
    go dir = do
        names ← listDirectory dir
        fmap concat $ forM (L.sort names) $ \name → do
            let path = dir </> name
            isDir ← doesDirectoryExist path
            if isDir then go path
            else pure [ path | takeExtension path ≡ ".hs" ]

-- * The golden spec

-- | Digests captured from the tree BEFORE this slice: SHA-256 over the
--   concatenated 'generateChunkPixels' blocks for the chunks a page
--   covers, in canonical cell order.
pageGoldens ∷ [(Int, Int, Int, String)]
pageGoldens =
    [ (64, 0, 0, "cfa61a0ee56366be5a679c6ee1ba80d4e27ecb0592a22b3f866475070069d13f")
    , (64, 1, 0, "efba7638a4766e5ce03d36569fc43a23733a33b603eb3b7bcda10a54891fdae0")
    , (64, 0, 1, "22d6e6c16f455089aea400dbb97fd9dfc245b378b7bacb5f4b1a0b49ae6ca269")
    , (64, 1, 3, "bcb5f2d8c3305c0af7da7ab11acf7c20e9ca55b7ae98818c34895d9e47ec441e")
    , (128, 0, 0, "6dbfebcc8bbcd8271a9615a6f1ceb074ae9e8e377ca1e13b67078cfe29bc951b")
    , (128, 3, 7, "15e36675144f9d8d146248e6520991038b7b352a053022a05a43adad95e3aba7")
    ]

-- | Digests of individual pre-change chunk blocks, pinned with the
--   cell they must appear in and the chunk that cell must hold. A
--   self-consistent but spatially wrong permutation moves these.
cellGoldens ∷ [(Int, Int, Int, Int, Int, String)]
cellGoldens =
    [ (64, 0, 0, -32, 0, "003bd8211d7b8a5b83b5aa611c751f9594443ac8f4f79419b667d851fe7fa6bd")
    , (64, 31, 15, 7, -24, "e14586bbc3cdacda013e6be162b9be06062d899305a4500829c18f3564eea02d")
    , (64, 16, 48, 8, 8, "fcc44113ca462d89297cb383ac27a02b9541a8e068fe4a43500e39dfc9a8ca99")
    , (128, 0, 0, -64, 0, "1ed145e2863b2bc0fcb5fc84eb36839e3f25e508442ca3cd0a30441c3218d0f5")
    , (128, 63, 127, 63, 0, "aecff0e733e498217fc695bd4b6dac3010603b91e8bc22b1aa63bfd34faafc89")
    ]

worldSpec ∷ SpecWith EngineEnv
worldSpec = describe "map pyramid finest-page goldens (#2298)" $ do
    it "matches the pre-change chunk bytes at worldSize 64" $ \env →
        goldenPages env 64
    it "matches the pre-change chunk bytes at worldSize 128" $ \env →
        goldenPages env 128
    it "carries the halo-aware bytes, and the halo can change them" $ \env →
        haloDependence env 64
    it "carries a neighbour's ocean through the composed cell source" $ \env →
        haloIntegration env 64
    it "generates a page alone exactly as within a larger region" $ \env →
        regionIndependence env 64

-- | Everything a golden example needs from one shared world, resolved
--   once: generating the world and reading the colour palette are the
--   only costs here that are not the pyramid's own.
data PyramidFixture = PyramidFixture
    { pfInventory ∷ MapPyramidInventory
    , pfGeometry  ∷ MapGeometry
    , pfSource    ∷ MapCellSource
    , pfParams    ∷ WorldGenParams
    , pfRegistry  ∷ MaterialRegistry
    , pfPalette   ∷ ZoomColorPalette
    }

pyramidFor ∷ EngineEnv → Int → IO PyramidFixture
pyramidFor env size = do
    ws ← sharedWorld env 42 size 3
    mParams ← getWorldGenParams ws
    params ← case mParams of
        Nothing → expectationFailure "shared world has no generation params"
                    ≫ error "unreachable"
        Just p  → pure p
    registry ← readIORef (materialRegistryRef env)
    logger ← readIORef (loggerRef env)
    palette ← buildColorPalette logger "data/materials" "data/vegetation"
    inv ← inventoryFor size
    geom ← geometryFor size
    src ← accept (worldGenCellSource geom params registry palette Nothing)
    pure PyramidFixture
        { pfInventory = inv, pfGeometry = geom, pfSource = src
        , pfParams = params, pfRegistry = registry, pfPalette = palette }

goldenPages ∷ EngineEnv → Int → IO ()
goldenPages env size = do
    PyramidFixture { pfInventory = inv, pfGeometry = geom, pfSource = src }
        ← pyramidFor env size
    forM_ [ (pu, pv, digest) | (s, pu, pv, digest) ← pageGoldens, s ≡ size ] $
      \(pu, pv, expected) → do
        let key = MapPageKey 0 pu pv
        bytes ← accept (mapPageImage inv src key)
        covered ← acceptAddress (mapPageFinestCells geom key)
        let blocks = [ pageCellBlock bytes col row | (col, row, _) ← covered ]
        (size, pu, pv, hex (SHA256.hash (BS.concat blocks)))
            `shouldBe` (size, pu, pv, expected)

    -- The individual cells pin BOTH the bytes and the chunk the cell
    -- must hold, so a permutation that preserves the aggregate digest
    -- but moves a chunk still fails.
    forM_ [ (cu, cv, cx, cy, d)
          | (s, cu, cv, cx, cy, d) ← cellGoldens, s ≡ size ] $
      \(cu, cv, cx, cy, expected) → do
        chunkOfFinestCell geom (MapCell cu cv)
            `shouldBe` Right (ChunkCoord cx cy)
        key ← acceptAddress (mapPageOfFinestCell geom (MapCell cu cv))
        bytes ← accept (mapPageImage inv src key)
        let col = cu `mod` cellsPerPageEdge
            row = cv `mod` cellsPerPageEdge
        (cu, cv, hex (SHA256.hash (pageCellBlock bytes col row)))
            `shouldBe` (cu, cv, expected)

-- | The page's bytes must be the halo-AWARE function of this world,
--   not a chunk-local one.
--
--   The obvious test does not hold: at seed 42 no chunk of the
--   worldSize 64 or 128 world changes when the halo is withheld,
--   because 'World.Generate.Chunk.Fluid.chunkOrNeighborOceanic' already
--   composes a chunk beside an oceanic one as oceanic, leaving the
--   cross-chunk half of
--   'World.ZoomMap.Cache.OceanFill.extendOceanBoundary' with nothing to
--   do. "The page differs from a halo-less page" would therefore be
--   vacuous on this world rather than evidence.
--
--   So the halo is pinned where it is assembled and consumed instead,
--   through the very functions the cell source uses:
--
--     1. 'renderCellsFromHaloTable' really does read a neighbour's
--        pass-one fluid map out of its table — flooding the
--        neighbours changes a real chunk's bytes, and the difference
--        is produced by the production renderer, so dropping its halo
--        lookup fails HERE whatever the world contains; and
--     2. every chunk a golden page covers carries exactly what that
--        renderer produces from the HONEST table, rebuilt here rather
--        than borrowed from the generator under test.
--
--   With 'mapCellHaloTable' holding the neighbours (checked below) and
--   'zoomChunkHaloNeighbours' naming the right ones (pinned in the pure
--   spec), the three mutations that could silently drop the halo each
--   fail one of these.
haloDependence ∷ EngineEnv → Int → IO ()
haloDependence env size = do
    PyramidFixture { pfInventory = inv, pfGeometry = geom, pfSource = src
                   , pfParams = params, pfRegistry = registry
                   , pfPalette = palette } ← pyramidFor env size
    let key = MapPageKey 0 0 0
    covered ← acceptAddress (mapPageFinestCells geom key)
    bytes ← accept (mapPageImage inv src key)
    placed ← forM covered $ \(col, row, cell) → do
        coord ← acceptAddress (chunkOfFinestCell geom cell)
        pure (col, row, coord)

    let honest = mapCellHaloTable params registry Nothing
                     [ coord | (_, _, coord) ← placed ]
        allOcean = V.replicate (chunkSize * chunkSize)
                       (Just (FluidCell Ocean seaLevel))
        -- One chunk's table, with and without its neighbours; and the
        -- neighbours it does have flooded, so a chunk with a shoreline
        -- gap on any edge shows the difference.
        loneTable coord = M.filterWithKey (\k _ → k ≡ coord) (tableFor coord)
        floodedTable coord = foldr
            (\n → M.adjust (\pass → pass { zcpRawFluid = allOcean }) n)
            (tableFor coord) (zoomChunkHaloNeighbours size coord)
        tableFor coord = mapCellHaloTable params registry Nothing [coord]
        renderVia table coord =
            renderCellsFromHaloTable palette size table [coord]

    -- The table really is the chunk plus its cardinal neighbours.
    case placed of
        [] → expectationFailure "the page covers no cells"
        ((_, _, coord) : _) →
            L.sort (M.keys (tableFor coord)) `shouldBe`
                L.sort (coord : zoomChunkHaloNeighbours size coord)

    -- (1) the production renderer consults it
    let sensitive = [ coord | (_, _, coord) ← placed
                    , renderVia (floodedTable coord) coord
                      ≢ renderVia (loneTable coord) coord ]
    (size, null (take 1 sensitive)) `shouldBe` (size, False)

    -- (2) and the page carries the honest-table bytes for every chunk
    forM_ placed $ \(col, row, coord) →
        (size, coord, Right [pageCellBlock bytes col row])
            `shouldBe` (size, coord, renderVia honest coord)

-- | The INTEGRATION the helper assertions cannot reach: that
--   'worldGenCellSource' composes the halo table and the halo-aware
--   renderer, rather than merely that each works when driven directly.
--
--   No generated world can show this. Every chunk of this one is
--   halo-inert, so a source that built a requested-chunks-only table
--   would return byte-identical output and every other assertion in
--   this file would stay green. The seam
--   'worldGenCellSourceWith' therefore drives the SAME 'generate' the
--   production source runs — same halo union, same lookup — with pass
--   one injected, so this world's own chunks can be given an ocean
--   neighbour.
--
--   Take the halo away anywhere along that path and this fails: the
--   union in 'mapCellHaloTableWith', a table filtered to the requested
--   chunks in 'worldGenCellSource' itself, or the neighbour lookup in
--   'renderCellsFromHaloTable'.
haloIntegration ∷ EngineEnv → Int → IO ()
haloIntegration env size = do
    PyramidFixture { pfGeometry = geom, pfSource = honestSource
                   , pfParams = params, pfRegistry = registry
                   , pfPalette = palette } ← pyramidFor env size
    covered ← acceptAddress (mapPageFinestCells geom (MapPageKey 0 0 0))

    let realPass coord = zoomChunkPass params registry Nothing coord
        allOcean = V.replicate (chunkSize * chunkSize)
                       (Just (FluidCell Ocean seaLevel))
        -- This world's own pass one, except that every chunk OTHER than
        -- the one being rendered composed as open ocean. Nothing else
        -- about the chunk changes, so the only thing that can move its
        -- bytes is the cross-boundary extension reading a neighbour.
        floodedExcept target coord
          | coord ≡ target = realPass coord
          | otherwise      = (realPass coord) { zcpRawFluid = allOcean }
        flooded target =
            worldGenCellSourceWith geom palette (floodedExcept target)
        tileFor src cell = case mcsCellTiles src [cell] of
            Right [tile] → Right tile
            Right tiles  → Left ("expected one tile, got "
                                 ⧺ show (length tiles))
            Left reason  → Left (T.unpack reason)

    -- Some chunk of this page must react to an ocean neighbour at all;
    -- otherwise the comparison below could not distinguish anything.
    let reacting =
            [ (cell, coord)
            | (_, _, cell) ← covered
            , Right coord ← [chunkOfFinestCell geom cell]
            , tileFor (flooded coord) cell ≢ tileFor honestSource cell ]
    (size, null (take 1 reacting)) `shouldBe` (size, False)

    -- and the composed source really is what carried it: the flooded
    -- neighbours reach the rendered chunk THROUGH
    -- 'worldGenCellSource''s own assembly.
    forM_ (take 1 reacting) $ \(cell, coord) → do
        withOcean ← either (\e → expectationFailure e ≫ error "x") pure
                        (tileFor (flooded coord) cell)
        honest ← either (\e → expectationFailure e ≫ error "x") pure
                     (tileFor honestSource cell)
        (size, coord, withOcean ≡ honest) `shouldBe` (size, coord, False)
        BS.length withOcean `shouldBe` mapCellTileBytes

-- | A page generated alone must equal the same page generated as part
--   of a larger region. The halo is what could break this: it is
--   derived per chunk rather than per request, so asking for four pages
--   at once must not change a single byte of any one of them.
regionIndependence ∷ EngineEnv → Int → IO ()
regionIndependence env size = do
    PyramidFixture { pfInventory = inv, pfGeometry = geom, pfSource = src }
        ← pyramidFor env size
    let key = MapPageKey 0 0 1
        block = [ MapPageKey 0 pu pv | pu ← [0, 1], pv ← [0, 1] ]
    covered ← acceptAddress (mapPageFinestCells geom key)
    blockCells ← concat ⊚ forM block (\k → do
        cs ← acceptAddress (mapPageFinestCells geom k)
        pure [ c | (_, _, c) ← cs ])
    alone ← acceptCells (mcsCellTiles src [ c | (_, _, c) ← covered ])
    together ← acceptCells (mcsCellTiles src blockCells)
    bytes ← accept (mapPageImage inv src key)
    let inRegion = M.fromList (zip blockCells together)
    length alone `shouldBe` length covered
    forM_ (zip covered alone) $ \((col, row, cell), tile) → do
        (cell, M.lookup cell inRegion) `shouldBe` (cell, Just tile)
        (cell, pageCellBlock bytes col row) `shouldBe` (cell, tile)

acceptCells ∷ HasCallStack ⇒ Either Text α → IO α
acceptCells (Right a) = pure a
acceptCells (Left t)  =
    expectationFailure ("cell source refused: " ⧺ T.unpack t)
        ≫ error "unreachable"
