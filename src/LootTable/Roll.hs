{-# LANGUAGE Strict, UnicodeSyntax #-}
module LootTable.Roll
    ( rollLootTable
    ) where

import UPrelude
import Data.IORef (IORef, atomicModifyIORef')
import System.Random (StdGen, randomR)
import LootTable.Types (LootTableDef(..), LootTableEntry(..))

-- | One weighted draw from a loot table's entries. Mirrors
--   'Unit.Injury.weightedPick': a roll in [0, 1) is scaled by the
--   total weight and the entries are walked by running sum, the last
--   entry catching any floating-point overshoot. 'Nothing' only for an
--   empty table (an unknown table id is handled by the caller, which
--   never looks one up in the first place).
rollLootTable ∷ LootTableDef → IORef StdGen → IO (Maybe Text)
rollLootTable def rngRef = case ltdEntries def of
    [] → pure Nothing
    entries → atomicModifyIORef' rngRef $ \g →
        let (u, g') = randomR (0 ∷ Float, 1) g
            total    = sum (map lteWeight entries)
            target   = u * total
            go acc (e : rest)
                | acc + lteWeight e ≥ target ∨ null rest = lteId e
                | otherwise = go (acc + lteWeight e) rest
            go _ [] = error "rollLootTable: unreachable (empty guarded above)"
        in (g', Just (go 0 entries))
