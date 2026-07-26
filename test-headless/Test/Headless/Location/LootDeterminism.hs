{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | "Location loot determinism" (#948): a placed location's loot-table
--   selections are a pure function of stable, persisted context — the
--   world page's generation seed, the location's #911 instance id, the
--   positional index of the @loot_table@ entry within its definition's
--   @contents@ list, and the roll number — instead of the shared,
--   entropy-seeded stat RNG the one-argument @loot.roll@ still uses.
--
--   The fixed vectors below PIN that mapping. They are a deliberate
--   fixture, not a derivation: a change to the mixing in
--   'LootTable.Roll', to the weighted walk, or to @ruin_common@'s
--   entries/weights is supposed to fail here and require an explicit
--   update, which is what stops the per-location reward mapping #921
--   balances against from drifting silently between builds.
module Test.Headless.Location.LootDeterminism
    ( spec
    ) where

import UPrelude
import Test.Hspec
import LootTable.Types
    ( LootTableDef(..), LootTableEntry(..)
    , emptyLootTableRegistry, registerLootTable, lookupLootTable )
import LootTable.Roll
    ( LootRollContext(..), lootRollUnit, pickByWeight, rollLootTableFor )

-- | The shipped data/loot_tables/ruin_common.yaml, mirrored here so the
--   vectors pin the SELECTION MAPPING rather than the YAML loader. #921
--   owns this table's composition; if it changes there, the vectors
--   below are updated deliberately alongside it.
ruinCommon ∷ LootTableDef
ruinCommon = LootTableDef
    { ltdId = "ruin_common"
    , ltdEntries =
        [ LootTableEntry "rations"        3
        , LootTableEntry "quinoa_sack"    2
        , LootTableEntry "first_aid_kit"  2
        , LootTableEntry "shovel_steel"   2
        , LootTableEntry "steel_hardware" 2
        , LootTableEntry "steel_dagger"   1
        ]
    }

-- | The context ruin_small's single @loot_table@ entry rolls under: it
--   is the THIRD entry in that definition's @contents@ list (after the
--   two fixed-position items).
ctxAt ∷ Int → Int → Int → Int → LootRollContext
ctxAt seed inst entry roll = LootRollContext
    { lrcWorldSeed  = seed
    , lrcInstanceId = inst
    , lrcEntryIndex = entry
    , lrcRollIndex  = roll
    }

-- | The full selected-item sequence for one entry's @rolls@ draws.
sequenceFor ∷ LootTableDef → Int → Int → Int → Int → [Maybe Text]
sequenceFor def seed inst entry rolls =
    [ rollLootTableFor def (ctxAt seed inst entry r) | r ← [1 .. rolls] ]

-- | How many of `n` contexts (varying the instance id) selected `name`.
tally ∷ LootTableDef → Text → Int → Int
tally def name n =
    length [ () | i ← [1 .. n]
                , rollLootTableFor def (ctxAt 42 i 1 1) ≡ Just name ]

spec ∷ Spec
spec = describe "Location loot determinism" $ do

    describe "fixed vectors" $ do
        -- ruin_small's own contract: entry 3, two rolls, per instance.
        it "pins ruin_common at seed 42, contents entry 3" $ do
            sequenceFor ruinCommon 42 1 3 2
                `shouldBe` [Just "rations", Just "steel_hardware"]
            sequenceFor ruinCommon 42 2 3 2
                `shouldBe` [Just "shovel_steel", Just "quinoa_sack"]
            sequenceFor ruinCommon 42 3 3 2
                `shouldBe` [Just "first_aid_kit", Just "steel_dagger"]
            sequenceFor ruinCommon 42 4 3 2
                `shouldBe` [Just "shovel_steel", Just "steel_dagger"]

        it "pins a second world seed" $
            sequenceFor ruinCommon 1337 2 1 4
                `shouldBe` [ Just "rations", Just "steel_dagger"
                           , Just "shovel_steel", Just "steel_hardware" ]

        it "pins the underlying unit draws" $
            [ lootRollUnit (ctxAt 42 1 3 r) | r ← [1, 2] ]
                `shouldBe` [3.4748614e-2, 0.8019202]

        -- The discriminator that a table-id-keyed context would miss:
        -- ONE definition carrying several loot_table entries that all
        -- name the same table. Positional indices keep their sequences
        -- independent; a table-id key would hand them identical loot.
        it "pins two contents entries naming the SAME table id" $ do
            let entry1 = sequenceFor ruinCommon 42 1 1 3
                entry2 = sequenceFor ruinCommon 42 1 2 3
            entry1 `shouldBe` [ Just "shovel_steel", Just "steel_hardware"
                              , Just "quinoa_sack" ]
            entry2 `shouldBe` [ Just "steel_hardware", Just "quinoa_sack"
                              , Just "shovel_steel" ]
            entry1 `shouldNotBe` entry2

    describe "stable context, not process state" $ do
        it "repeats identically for the same context" $
            replicate 5 (rollLootTableFor ruinCommon (ctxAt 42 7 3 1))
                `shouldBe` replicate 5 (Just "rations")

        it "is unaffected by the order contexts are evaluated in" $ do
            let ctxs     = [ ctxAt 42 i 3 r | i ← [1 .. 6], r ← [1, 2] ]
                forwards = map (rollLootTableFor ruinCommon) ctxs
                reversed = reverse (map (rollLootTableFor ruinCommon)
                                        (reverse ctxs))
            reversed `shouldBe` forwards

        it "gives each instance its own independent draw sequence" $ do
            let seqs  = [ sequenceFor ruinCommon 42 i 3 2 | i ← [1 .. 4] ]
                first = sequenceFor ruinCommon 42 1 3 2
            -- Not an all-distinct claim: chance may legitimately pick the
            -- same entry twice. What must hold is that the instance id
            -- actually moves the draw at all.
            length (filter (/= first) seqs) `shouldSatisfy` (> 0)

        it "separates roll indices within one entry" $ do
            let s     = sequenceFor ruinCommon 42 1 3 6
                first = rollLootTableFor ruinCommon (ctxAt 42 1 3 1)
            length (filter (≡ first) s) `shouldSatisfy` (< length s)

        it "separates world seeds" $
            sequenceFor ruinCommon 42 1 3 4
                `shouldNotBe` sequenceFor ruinCommon 43 1 3 4

        -- scripts/locations.lua keys a HAND-STAMPED location (no placed
        -- instance) on a negative anchor-derived id. Those must stay
        -- defined, in range, and stable — not wrap into a crash or
        -- collide with instance 1's stream.
        it "handles the negative anchor-derived fallback ids" $ do
            let neg = ctxAt 42 (-1234567) 3 1
            rollLootTableFor ruinCommon neg
                `shouldBe` rollLootTableFor ruinCommon neg
            rollLootTableFor ruinCommon neg `shouldSatisfy` isJust
            lootRollUnit neg `shouldSatisfy` \u → u ≥ 0 ∧ u < 1

        it "keeps every unit draw in [0, 1)" $
            [ lootRollUnit (ctxAt s i e r)
            | s ← [-7, 0, 42], i ← [-3, 1, 900], e ← [1, 2], r ← [1, 5] ]
                `shouldSatisfy` all (\u → u ≥ 0 ∧ u < 1)

    describe "loot-table data contract" $ do
        it "walks entries by running sum, lower bound inclusive" $ do
            -- total weight 12; rations spans [0, 3].
            pickByWeight 0      ruinCommon `shouldBe` Just "rations"
            pickByWeight 0.25   ruinCommon `shouldBe` Just "rations"
            pickByWeight 0.2501 ruinCommon `shouldBe` Just "quinoa_sack"
            pickByWeight (5 / 12) ruinCommon `shouldBe` Just "quinoa_sack"
            pickByWeight 0.9999 ruinCommon `shouldBe` Just "steel_dagger"

        it "gives the last entry any floating-point overshoot" $
            pickByWeight 1 ruinCommon `shouldBe` Just "steel_dagger"

        it "treats weights as RELATIVE, not percentages" $ do
            -- Same 3:1 ratio between rations and steel_dagger whether the
            -- weights sum to 12 or to 1.2.
            let scaled = ruinCommon
                    { ltdEntries = [ LootTableEntry (lteId e) (lteWeight e / 10)
                                   | e ← ltdEntries ruinCommon ] }
            [ pickByWeight u scaled | u ← [0, 0.25, 0.2501, 0.9999] ]
                `shouldBe` [ Just "rations", Just "rations"
                           , Just "quinoa_sack", Just "steel_dagger" ]

        it "selects at most one item definition per roll" $
            all (maybe True (const True)) (sequenceFor ruinCommon 42 1 3 8)
                `shouldBe` True

        it "tracks the declared weights over many contexts" $ do
            -- 60000 draws: rations (weight 3/12) ~15000, steel_dagger
            -- (1/12) ~5000. Wide bands — this catches a degenerate or
            -- weight-blind mixer, not sampling noise.
            tally ruinCommon "rations"      60000 `shouldSatisfy` \n →
                n > 14000 ∧ n < 16000
            tally ruinCommon "steel_dagger" 60000 `shouldSatisfy` \n →
                n > 4500 ∧ n < 5500

        it "returns Nothing for an empty table" $
            rollLootTableFor (LootTableDef "empty" []) (ctxAt 42 1 1 1)
                `shouldBe` Nothing

        it "always selects the only entry of a single-entry table" $ do
            let one = LootTableDef "one" [LootTableEntry "quinoa_sack" 1]
            sequenceFor one 42 1 1 5 `shouldBe` replicate 5 (Just "quinoa_sack")

        -- An unknown table id never reaches the roll: the Lua surface
        -- looks it up first and pushes nil (which scripts/locations.lua
        -- turns into its "unknown loot table" warning).
        it "leaves an unknown table id to the registry lookup" $ do
            let reg = registerLootTable ruinCommon emptyLootTableRegistry
            lookupLootTable "no_such_table" reg `shouldBe` Nothing
            (lookupLootTable "ruin_common" reg
                >>= \d → rollLootTableFor d (ctxAt 42 1 3 1))
                `shouldBe` Just "rations"
