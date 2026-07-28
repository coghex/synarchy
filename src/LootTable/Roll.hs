{-# LANGUAGE Strict #-}
module LootTable.Roll
    ( rollLootTable
    , rollLootTableFor
    , pickByWeight
    , LootRollContext(..)
    , lootRollUnit
    ) where

import UPrelude
import Data.IORef (IORef, atomicModifyIORef')
import System.Random (StdGen, randomR)
import LootTable.Types (LootTableDef(..), LootTableEntry(..))

-- | The weighted walk both roll paths share, given a draw @u@ in
--   [0, 1). Mirrors 'Unit.Injury.weightedPick': @u@ is scaled by the
--   total weight and the entries are walked by running sum, the last
--   entry catching any floating-point overshoot. Weights stay RELATIVE
--   — nothing here normalises them to probabilities. 'Nothing' only for
--   an empty table (an unknown table id is handled by the caller, which
--   never looks one up in the first place).
pickByWeight ∷ Float → LootTableDef → Maybe Text
pickByWeight u def = case ltdEntries def of
    []      → Nothing
    entries →
        let total  = sum (map lteWeight entries)
            target = u * total
            go acc (e : rest)
                | acc + lteWeight e ≥ target ∨ null rest = lteId e
                | otherwise = go (acc + lteWeight e) rest
            go _ [] = error "pickByWeight: unreachable (empty guarded above)"
        in Just (go 0 entries)

-- | One weighted draw from a loot table's entries using the engine's
--   shared, entropy-seeded stat RNG. This is the UNCONTEXTUAL path
--   behind the one-argument @loot.roll(tableId)@ (#90): its result
--   depends on process entropy and on every other consumer of that
--   generator, so it is deliberately NOT reproducible across runs.
--   Placed-location content spawning uses 'rollLootTableFor' instead
--   (#948). An empty table returns 'Nothing' without consuming the
--   generator.
rollLootTable ∷ LootTableDef → IORef StdGen → IO (Maybe Text)
rollLootTable def rngRef = case ltdEntries def of
    [] → pure Nothing
    _  → atomicModifyIORef' rngRef $ \g →
        let (u, g') = randomR (0 ∷ Float, 1) g
        in (g', pickByWeight u def)

-- | The stable context identifying ONE loot draw for a placed location
--   (#948). Every component is durable state, so the same draw resolves
--   identically in any process, in any chunk/location load order, and
--   before or after a save/load:
--
--     * 'lrcWorldSeed' — the world page's persisted generation seed
--       (@wgpSeed@, exposed as @world.getSeed(pageId)@);
--     * 'lrcInstanceId' — the placed location's stable instance id
--       (#911), allocated at placement in deterministic overlay order
--       and persisted;
--     * 'lrcEntryIndex' — the POSITIONAL index of the @loot_table@
--       entry within its location definition's @contents@ list. It is
--       deliberately positional rather than the table id: one
--       definition may carry several entries naming the same table, and
--       those must not share a roll sequence;
--     * 'lrcRollIndex' — which roll within that entry (1-based).
data LootRollContext = LootRollContext
    { lrcWorldSeed  ∷ !Int
    , lrcInstanceId ∷ !Int
    , lrcEntryIndex ∷ !Int
    , lrcRollIndex  ∷ !Int
    } deriving (Show, Eq)

-- | SplitMix's 64-bit finalizer, written out here rather than reached
--   through the @random@ package. The mapping from context to item is a
--   pinned contract (#948) with fixed vectors in the
--   \"Location loot determinism\" spec, so it must not be able to shift
--   under a dependency bump.
mix64 ∷ Word64 → Word64
mix64 z0 =
    let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
    in z2 `xor` (z2 `shiftR` 31)

-- | Fold the context's four components into one well-mixed word. Each
--   component is absorbed in turn (golden-ratio increment, mix, xor
--   into the accumulator), so the result is order-sensitive: changing
--   any single component — including swapping two of them — moves the
--   draw. Negative components (a negative world seed, or the
--   anchor-derived fallback id @scripts\/locations.lua@ uses for a
--   hand-stamped location that owns no placed instance) wrap into
--   'Word64' deterministically.
lootRollHash ∷ LootRollContext → Word64
lootRollHash ctx = foldl' step 0x9e3779b97f4a7c15
    [ fromIntegral (lrcWorldSeed ctx)
    , fromIntegral (lrcInstanceId ctx)
    , fromIntegral (lrcEntryIndex ctx)
    , fromIntegral (lrcRollIndex ctx)
    ]
  where
    step acc w = acc `xor` mix64 (acc + 0x9e3779b97f4a7c15 + w)

-- | The context's draw in [0, 1). Takes the hash's top 24 bits, which
--   is exactly a 'Float' mantissa — the conversion is lossless, so the
--   fixed vectors can't drift on a different FPU rounding mode.
lootRollUnit ∷ LootRollContext → Float
lootRollUnit ctx =
    fromIntegral (lootRollHash ctx `shiftR` 40) / 16777216

-- | One weighted draw for a placed location's loot-table content entry
--   (#948) — a PURE function of the loot table and the stable context,
--   touching neither the shared stat RNG nor process entropy. Same
--   contract as 'rollLootTable' otherwise: relative weights, at most one
--   selected item definition, 'Nothing' for an empty table.
rollLootTableFor ∷ LootTableDef → LootRollContext → Maybe Text
rollLootTableFor def ctx = pickByWeight (lootRollUnit ctx) def
