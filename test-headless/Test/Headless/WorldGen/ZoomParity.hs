{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | STRICT parity between the zoom-map terrain path and the detail
--   chunk path.
--
--   'generateZoomTerrain' exists so the zoom atlas agrees with the
--   world view: it runs the same bordered pipeline (timeline +
--   coastal erosion + despike + carve + despike#2), the same basalt
--   caps, the same lava shell, and the same island-column smoother
--   as 'generateChunk'. Unlike the fast-path parity test (loose
--   tolerances — different algorithm), this one demands EXACT
--   per-tile equality of terrain elevation and fluid cells: any
--   mismatch is a real divergence the player can see as zoom-vs-
--   world disagreement (reported 2026-06-06: incorrect land around
--   coasts on the zoom map).
module Test.Headless.WorldGen.ZoomParity (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef)
import Test.Headless.Harness
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Material (MaterialRegistry)
import World.Generate.Chunk (generateZoomTerrain)

-- | One per-tile mismatch: (chunk, lx, ly, what, detail, zoom).
data Mismatch = Mismatch ChunkCoord Int Int String String String
    deriving Show

fluidStr ∷ Maybe FluidCell → String
fluidStr Nothing = "dry"
fluidStr (Just (FluidCell t s)) = show t ⧺ "@" ⧺ show s

compareChunk
    ∷ MaterialRegistry → WorldGenParams → ChunkCoord → LoadedChunk
    → [Mismatch]
compareChunk registry params coord lc =
    let (zoomSurf, zoomMat, zoomVeg, zoomFluid) =
            generateZoomTerrain registry params Nothing coord
        -- generateZoomTerrain returns the SURFACE map (mkSurfaceMap:
        -- river tiles at water level, others max(terrain, fluid)) —
        -- compare against the detail chunk's lcSurfaceMap, its exact
        -- counterpart.
        detailElev  = lcSurfaceMap lc
        detailFluid = lcFluidMap lc
        detailTerr  = lcTerrainSurfaceMap lc
        -- Detail surface veg / slope / mat live in the column tiles
        -- at the terrain-surface index.
        surfField f idx =
            let col = lcTiles lc V.! idx
                i = detailTerr VU.! idx - ctStartZ col
            in if i ≥ 0 ∧ i < VU.length (f col)
               then Just (f col VU.! i)
               else Nothing
    in [ m
       | ly ← [0 .. chunkSize - 1]
       , lx ← [0 .. chunkSize - 1]
       , let idx = columnIndex lx ly
             dE  = detailElev VU.! idx
             zE  = zoomSurf  VU.! idx
             dF  = detailFluid V.! idx
             zF  = zoomFluid  V.! idx
             -- Vegetation parity is checked only where the two paths
             -- have identical inputs: flat (zoom passes slopeId 0 —
             -- slope-gated moss/ivy variants legitimately differ),
             -- un-iced (the zoom adds its snow overlay later in the
             -- cache build), and same surface material.
             -- Slope-gated families (moss 17-24, ivy 25-32) are the
             -- documented zoom simplification — the zoom passes
             -- slopeId 0, so these tiles legitimately render as the
             -- flat-ground equivalent. Everything else must agree.
             slopeGated v = v ≥ 17 ∧ v ≤ 32
             vegComparable =
                 isNothing (lcIceMap lc V.! idx)
               ∧ surfField ctMats idx ≡ Just (zoomMat VU.! idx)
               ∧ maybe True (not . slopeGated) dV
             dV = surfField ctVeg idx
             zV = zoomVeg VU.! idx
       , m ← (if dE ≠ zE
              then [ Mismatch coord lx ly "elev" (show dE) (show zE) ]
              else [])
           ⧺ (if dF ≠ zF
              then [ Mismatch coord lx ly "fluid"
                              (fluidStr dF) (fluidStr zF) ]
              else [])
           ⧺ (if vegComparable ∧ dV ≠ Just zV
              then [ Mismatch coord lx ly "veg" (show dV) (show zV) ]
              else [])
       ]

spec ∷ SpecWith EngineEnv
spec = do

    describe "Zoom terrain vs detail chunk parity" $

        it "terrain + fluid are bit-identical per tile (seed 1840733254 w64 plates 10)" $ \env → do
            -- The user-reported coastal-discrepancy world (own params
            -- — this specific repro is the point of the test).
            ws ← sharedWorld env 1840733254 64 10
            -- Load a wide ring so the comparison includes coastline,
            -- not just the init region around the origin.
            queueChunks ws [ ChunkCoord cx cy
                           | cx ← [-6 .. 6], cy ← [-6 .. 6] ]
            ok ← waitForChunksAt ws (ChunkCoord 6 6) 240
            ok `shouldBe` True
            tiles ← getWorldTileData ws
            mParams ← getWorldGenParams ws
            registry ← readIORef (materialRegistryRef env)
            case mParams of
                Nothing → expectationFailure "gen params should exist"
                Just params → do
                    let mismatches =
                            [ m
                            | (coord, lc) ← HM.toList (wtdChunks tiles)
                            , m ← compareChunk registry params coord lc
                            ]
                    case mismatches of
                        [] → pure ()
                        ms → expectationFailure $
                            show (length ms)
                            ⧺ " zoom/detail tile mismatches; first 12: "
                            ⧺ show (take 12 ms)
