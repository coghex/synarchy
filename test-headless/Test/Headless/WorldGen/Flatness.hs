-- | Biome-flatness invariants.
--
--   Pins the guarantees established by the worldgen flatness work
--   (plateau snap in 'World.Plate', wetland post-pass in
--   'World.Generate.Chunk'):
--
--   1. Wetland gating — a surface wetland soil (peat 62 / mucky peat
--      63 / muck 64) only survives 'wetlandKeep': in-chunk 4-neighbour
--      max |Δterrain| ≤ 2 AND water table at the surface (wt ≥
--      terrain−1). The post-pass guarantees this by construction, so
--      any violation is a real regression — strict hspec material per
--      the testing philosophy.
--
--   2. Plains existence — plateau snap quantizes lowland elevation
--      into flat bands, so a meaningful fraction of land tiles must be
--      fully flat. The floor here (5%) is far below measured reality
--      (~70% at w64 seed 42, 2026-06-05); a failure means plateau
--      snapping broke, not that variance drifted.
--
--   Quality-spectrum metrics (roughness percentiles, sand/salt-flat
--   slope rates) live in tools/world_audit.py, not here.
module Test.Headless.WorldGen.Flatness (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Headless.Harness
import World.Types
import World.Constants (seaLevel)

-- | Top surface material of a column (mirrors the --dump extraction:
--   highest stored tile; fluids live in a separate map so this is the
--   terrain surface for ordinary columns).
surfaceMatOf ∷ ColumnTiles → Word8
surfaceMatOf col
    | VU.null (ctMats col) = 0
    | otherwise            = VU.last (ctMats col)

isWetlandMat ∷ Word8 → Bool
isWetlandMat m = m ≡ 62 ∨ m ≡ 63 ∨ m ≡ 64

-- | In-chunk max 4-neighbour |Δterrain| — the exact neighbour
--   convention of 'World.Generate.Chunk.wetlandKeep' (out-of-chunk
--   neighbours read as 0, i.e. lenient at chunk borders).
maxNbrDelta ∷ VU.Vector Int → Int → Int
maxNbrDelta terrain idx =
    let tz = terrain VU.! idx
        lx = idx `mod` chunkSize
        ly = idx `div` chunkSize
        nbrD dlx dly =
            let lx' = lx + dlx
                ly' = ly + dly
            in if lx' < 0 ∨ lx' ≥ chunkSize ∨ ly' < 0 ∨ ly' ≥ chunkSize
               then 0
               else abs (tz - terrain VU.! (ly' * chunkSize + lx'))
    in max (max (nbrD 1 0) (nbrD (-1) 0)) (max (nbrD 0 1) (nbrD 0 (-1)))

-- | Wetland tiles violating the keep-gate: (lx, ly, mat, maxΔ, tz, wt).
wetlandViolations ∷ LoadedChunk → [(Int, Int, Word8, Int, Int, Int)]
wetlandViolations lc =
    [ (lx, ly, mat, d, tz, wt)
    | ly ← [0 .. chunkSize - 1]
    , lx ← [0 .. chunkSize - 1]
    , let idx = columnIndex lx ly
          mat = surfaceMatOf (lcTiles lc V.! idx)
    , isWetlandMat mat
    , let terrain = lcTerrainSurfaceMap lc
          tz = terrain VU.! idx
          wt = lcWaterTableMap lc VU.! idx
          d  = maxNbrDelta terrain idx
    -- Sub-sea tiles are seabed, not land: the ocean-floor pass places
    -- muck on the deep floor by design (it's steep and wet), so the
    -- flat∧wet land gate doesn't apply (mirrors wetlandKeep's
    -- @tz ≤ seaLevel@ exemption).
    , tz > seaLevel
    , d > 2 ∨ wt < tz - 1
    ]

-- | (fully-flat land tiles, land tiles) over interior positions (all
--   four neighbours in-chunk). Fully flat = max 4-neighbour Δ ≤ 1.
flatLandCounts ∷ LoadedChunk → (Int, Int)
flatLandCounts lc =
    let terrain = lcTerrainSurfaceMap lc
        landFlags =
            [ maxNbrDelta terrain idx ≤ 1
            | ly ← [1 .. chunkSize - 2]
            , lx ← [1 .. chunkSize - 2]
            , let idx = columnIndex lx ly
            , terrain VU.! idx > seaLevel
            ]
    in (length (filter id landFlags), length landFlags)

spec ∷ Spec
spec = aroundAll withHeadlessEngine $ do

    describe "Biome flatness invariants" $
        forM_ [(42 ∷ Int, "flat42"), (7, "flat7")] $ \(seed, pid) →
            it ("wetland gating + plains existence hold (w64 seed "
                ⧺ show seed ⧺ ")") $ \env → do
                sendWorldCommand env
                    (WorldInit (WorldPageId pid) (fromIntegral seed) 64 3)
                ws ← waitForWorldInit env (WorldPageId pid) 120
                tiles ← getWorldTileData ws
                let chunks = HM.toList (wtdChunks tiles)

                -- 1. Wetland gating: zero violations, by construction.
                let bad = [ (coord, v)
                          | (coord, lc) ← chunks
                          , v ← wetlandViolations lc ]
                case bad of
                    [] → pure ()
                    _  → expectationFailure $
                        show (length bad) ⧺ " wetland tiles violate "
                        ⧺ "flat∧wet gate; first: "
                        ⧺ show (take 5 bad)

                -- 2. Plains existence: plateau snap means a healthy
                -- share of land is fully flat. 5% floor is ~14× below
                -- measured (70.8% at seed 42); only a structural break
                -- can dip under it. Skip when the init region happens
                -- to be nearly all ocean.
                let (flat, land) =
                        foldl' (\(f, l) (_, lc) →
                                  let (f', l') = flatLandCounts lc
                                  in (f + f', l + l'))
                               (0, 0) chunks
                when (land ≥ 2000) $
                    let frac = fromIntegral flat / fromIntegral land ∷ Double
                    in frac `shouldSatisfy` (≥ 0.05)
