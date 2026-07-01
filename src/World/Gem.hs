{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Seeded gem region field (dig yields, G3).
--
--   The world is divided into 'gemRegionSize'-square tile regions.
--   Each region deterministically answers (from a hash of the world
--   seed + region coords — nothing is stored or saved):
--
--     * bearing?  ~25% of regions host a gem at all
--     * which?    ONE gem type per region, weighted by rarity tier —
--                 that's what makes a streak read as "an emerald
--                 pocket" instead of a lottery
--     * richness? skewed low (1 + 11·u³): most bearing regions are
--                 thin traces, the occasional one is a hot pocket
--                 that yields several finds in a row
--
--   Per completed dig tile the handler rolls ONCE:
--   @tierBase × richness × perception@, capped at 'gemChanceCap'.
--   Perception (the finishing digger's stat, ~0.8–1.2) is the
--   noticing-the-glint factor — deliberately NOT mining skill.
module World.Gem
    ( gemRegionSize
    , gemChanceCap
    , gemAtRegion
    , gemChanceAt
    ) where

import UPrelude

-- | Region side in tiles. A typical mining operation stays inside
--   one region, so a bearing region produces same-gem streaks.
gemRegionSize ∷ Int
gemRegionSize = 12

-- | Hard cap on the per-tile find chance, after richness and
--   perception. Even the hottest pocket misses sometimes.
gemChanceCap ∷ Float
gemChanceCap = 0.6

-- | (item def name, per-tile base chance) by rarity tier. Weights
--   below pick WHICH gem a bearing region hosts; the base chance is
--   how often a tile dug inside the region yields one at richness 1.
gemTable ∷ [(Text, Float, Int)]   -- ^ (defName, tierBase, pickWeight)
gemTable =
    -- common: 10% base, weight 6 each
    [ ("raw_garnet",       0.10, 6)
    , ("raw_amazonite",    0.10, 6)
    , ("raw_smoky_quartz", 0.10, 6)
    -- uncommon: 7%, weight 3
    , ("raw_zircon",       0.07, 3)
    , ("raw_moonstone",    0.07, 3)
    , ("raw_citrine",      0.07, 3)
    , ("raw_fluorite",     0.07, 3)
    -- rare: 4%, weight 2
    , ("raw_aquamarine",   0.04, 2)
    , ("raw_topaz",        0.04, 2)
    , ("raw_golden_beryl", 0.04, 2)
    , ("raw_tourmaline",   0.04, 2)
    , ("raw_amethyst",     0.04, 2)
    -- very rare: 2%, weight 1
    , ("raw_emerald",      0.02, 1)
    , ("raw_morganite",    0.02, 1)
    ]

-- | SplitMix64-style avalanche. Good enough mixing that adjacent
--   regions decorrelate; pure function of (seed, rx, ry, salt).
mix64 ∷ Word64 → Word64
mix64 z0 =
    let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
    in z2 `xor` (z2 `shiftR` 31)

regionHash ∷ Int → (Int, Int) → Word64 → Word64
regionHash seed (rx, ry) salt =
    mix64 ( fromIntegral seed * 0x9e3779b97f4a7c15
          + fromIntegral rx   * 0xc2b2ae3d27d4eb4f
          + fromIntegral ry   * 0x165667b19e3779f9
          + salt )

-- | Uniform [0,1) from a hash draw.
unitFloat ∷ Word64 → Float
unitFloat h = fromIntegral (h `shiftR` 40) / 16777216.0

-- | What (if anything) a region hosts: (gem def name, richness).
gemAtRegion ∷ Int → (Int, Int) → Maybe (Text, Float)
gemAtRegion seed region =
    let bearing = unitFloat (regionHash seed region 1) < 0.25
    in if not bearing
       then Nothing
       else
        let totalW = sum [ w | (_, _, w) ← gemTable ]
            pick   = unitFloat (regionHash seed region 2)
                       * fromIntegral totalW
            chosen = go pick gemTable
            u      = unitFloat (regionHash seed region 3)
            richness = 1 + 11 * u * u * u
        in case chosen of
            Just (name, _, _) → Just (name, richness)
            Nothing           → Nothing
  where
    go _ [] = Nothing
    go p (entry@(_, _, w) : rest)
        | p < fromIntegral w = Just entry
        | otherwise          = go (p - fromIntegral w) rest

-- | Per-tile find chance at a tile: Nothing when the region is
--   barren; otherwise (gem def name, chance) with perception folded
--   in and the cap applied.
gemChanceAt ∷ Int → (Int, Int) → Float → Maybe (Text, Float)
gemChanceAt seed (gx, gy) perception = do
    let region = ( floor (fromIntegral gx / fromIntegral gemRegionSize
                            ∷ Double)
                 , floor (fromIntegral gy / fromIntegral gemRegionSize
                            ∷ Double) )
    (name, richness) ← gemAtRegion seed region
    let tierBase = case [ b | (n, b, _) ← gemTable, n ≡ name ] of
            (b:_) → b
            []    → 0
        chance = min gemChanceCap
                     (tierBase * richness * max 0 perception)
    pure (name, chance)
