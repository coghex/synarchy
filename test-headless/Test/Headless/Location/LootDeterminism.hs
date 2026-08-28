-- | "Location loot determinism" (#948): a placed location's loot-table
--   selections are a pure function of stable, persisted context — the
--   world page's generation seed, the location's #911 instance id, the
--   positional index of the @loot_table@ entry within its definition's
--   @contents@ list, and the roll number — instead of the shared,
--   entropy-seeded stat RNG the one-argument @loot.roll@ still uses.
--
--   The fixed vectors below PIN that mapping, and they are computed
--   from the SHIPPED data/loot_tables/ruin_common.yaml rather than from
--   a copy of it. Requirement 8 covers loot-table DATA as well as the
--   selection mapping: if the vectors ran against an in-test replica,
--   re-weighting the real table would change what real ruins hand out
--   while every example here still passed. So the live file is decoded
--   once, checked against an explicit pin ('pinnedRuinCommon'), and then
--   used for every draw — a change to the mixing, to the weighted walk,
--   or to the table's entries/weights all fail here and all require a
--   deliberate fixture update, which is what stops the per-location
--   reward mapping from drifting silently. That matters more since
--   #921: a ruin's contents are now ONLY these draws, so this table and
--   this mapping are the whole of what a ruin is worth.
module Test.Headless.Location.LootDeterminism
    ( spec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.Yaml as Yaml
import Engine.Asset.YamlLootTables
    ( LootTableYamlDef(..), LootTableYamlEntry(..) )
import LootTable.Types
    ( LootTableDef(..), LootTableEntry(..)
    , emptyLootTableRegistry, registerLootTable, lookupLootTable )
import LootTable.Roll
    ( LootRollContext(..), lootRollUnit, pickByWeight, rollLootTableFor )

-- | The engine's own YAML → registry conversion
--   ('Engine.Scripting.Lua.API.LootTables.loadLootTableYamlFn'), so the
--   vectors below are taken against the table the engine would actually
--   roll, not a re-typed approximation of the file.
toLootTableDef ∷ LootTableYamlDef → LootTableDef
toLootTableDef d = LootTableDef
    { ltdId      = ltydId d
    , ltdEntries = [ LootTableEntry (ltyeId e) (ltyeWeight e)
                   | e ← ltydEntries d ]
    }

-- | The pinned composition of data/loot_tables/ruin_common.yaml. When
--   the table changes there, this pin AND the vectors below are updated
--   together, deliberately. #921 deliberately left it alone: `radio`
--   and `canteen_steel_2l` stay OUT of it (they are spawn-only starting
--   equipment), so dropping them from ruin_small removed them from ruin
--   content entirely rather than moving them into the draw.
pinnedRuinCommon ∷ LootTableDef
pinnedRuinCommon = LootTableDef
    { ltdId = "ruin_common"
    , ltdEntries =
        [ LootTableEntry "rations"        3
        , LootTableEntry "quinoa_sack"    2
        , LootTableEntry "first_aid_kit"  2
        , LootTableEntry "shovel_steel"   2
        , LootTableEntry "steel_hardware" 2
        , LootTableEntry "steel_dagger"   1
        , LootTableEntry "field_toolbox"  1
        ]
    }

-- | One roll context, spelled out positionally.
--
--   ruin_small's single @loot_table@ entry rolls under entry index 1:
--   #921 removed the two fixed-position items that used to precede it,
--   so it is now the FIRST (and only) entry in that definition's
--   @contents@ list, where it used to be the third. That shift is
--   intentional and changes which items fresh worlds' ruins select;
--   worlds saved before the change keep the loot they already spawned,
--   which rides the persisted one-time content flag (#90) rather than
--   being re-derived. The ruin_small vectors below pin the new index;
--   the property blocks further down pass an arbitrary index, since
--   what they assert holds for any entry.
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

    -- Decoded once, at spec-construction time — the same way
    -- Test.Headless.Location.Bounds/MapIcons read the shipped
    -- data/locations/*.yaml. Every draw below runs against THIS def.
    shipped ← runIO
        (Yaml.decodeFileEither "data/loot_tables/ruin_common.yaml")
    let ruinCommon = either (const pinnedRuinCommon) toLootTableDef shipped

    describe "shipped loot-table data" $ do
        it "decodes data/loot_tables/ruin_common.yaml" $
            either (\e → expectationFailure (show e)) (const (pure ())) shipped

        -- The guard that makes the vectors bind the real data: re-weight
        -- or re-order the YAML and this fails by name, alongside the
        -- vectors themselves.
        it "still holds exactly the entries and weights the vectors pin" $
            fmap toLootTableDef shipped
                `shouldSatisfy` either (const False) (≡ pinnedRuinCommon)

    describe "fixed vectors" $ do
        -- ruin_small's own contract: entry 1 (#921 — see 'ctxAt'), two
        -- rolls, per instance. Nothing else in the definition spawns
        -- content now, so these ARE a ruin's entire yield.
        it "pins ruin_common at seed 42, contents entry 1" $ do
            sequenceFor ruinCommon 42 1 1 2
                `shouldBe` [Just "shovel_steel", Just "steel_hardware"]
            sequenceFor ruinCommon 42 2 1 2
                `shouldBe` [Just "steel_hardware", Just "steel_hardware"]
            sequenceFor ruinCommon 42 3 1 2
                `shouldBe` [Just "first_aid_kit", Just "rations"]
            sequenceFor ruinCommon 42 4 1 2
                `shouldBe` [Just "steel_hardware", Just "field_toolbox"]

        it "pins a second world seed" $
            sequenceFor ruinCommon 1337 2 1 4
                `shouldBe` [ Just "rations", Just "field_toolbox"
                           , Just "shovel_steel", Just "steel_dagger" ]

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
            entry2 `shouldBe` [ Just "steel_hardware", Just "first_aid_kit"
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
        -- instance) on a negative anchor-derived id, which
        -- 'lootRollHash' absorbs by wrapping two's-complement into
        -- 'Word64'. These two goldens pin the whole of that path — the
        -- mixer's draw and the entry it then selects — so a change to
        -- the signed-to-'Word64' conversion fails here. They SUBSUME
        -- the definedness, range and stability assertions this example
        -- used to make: an exact expected value is by construction
        -- defined, in [0, 1), and identical on every evaluation. The
        -- non-collision half is evidence too — instance 1's draw at
        -- this same entry and roll is pinned above as 3.4748614e-2,
        -- which this value is not.
        it "pins the negative anchor-derived fallback id" $ do
            let neg = ctxAt 42 (-1234567) 3 1
            lootRollUnit neg `shouldBe` 0.60222095
            rollLootTableFor ruinCommon neg `shouldBe` Just "shovel_steel"

        it "keeps every unit draw in [0, 1)" $
            [ lootRollUnit (ctxAt s i e r)
            | s ← [-7, 0, 42], i ← [-3, 1, 900], e ← [1, 2], r ← [1, 5] ]
                `shouldSatisfy` all (\u → u ≥ 0 ∧ u < 1)

    describe "loot-table data contract" $ do
        it "walks entries by running sum, lower bound inclusive" $ do
            -- Total weight 13; rations spans the scaled interval [0, 3].
            pickByWeight 0        ruinCommon `shouldBe` Just "rations"
            pickByWeight (3 / 13) ruinCommon `shouldBe` Just "rations"
            pickByWeight 0.231    ruinCommon `shouldBe` Just "quinoa_sack"
            pickByWeight (5 / 13) ruinCommon `shouldBe` Just "quinoa_sack"
            pickByWeight 0.9999   ruinCommon `shouldBe` Just "field_toolbox"

        it "gives the last entry any floating-point overshoot" $
            pickByWeight 1 ruinCommon `shouldBe` Just "field_toolbox"

        it "treats weights as RELATIVE, not percentages" $ do
            -- Same 3:1 ratio between rations and field_toolbox whether the
            -- weights sum to 13 or to 1.3.
            let scaled = ruinCommon
                    { ltdEntries = [ LootTableEntry (lteId e) (lteWeight e / 10)
                                   | e ← ltdEntries ruinCommon ] }
            [ pickByWeight u scaled | u ← [0, 3 / 13, 0.231, 0.9999] ]
                `shouldBe` [ Just "rations", Just "rations"
                           , Just "quinoa_sack", Just "field_toolbox" ]

        it "tracks the declared weights over many contexts" $ do
            -- 60000 draws: rations (weight 3/13) ~13846, while both
            -- one-weight entries are ~4615. Wide bands catch a degenerate
            -- or weight-blind mixer rather than sampling noise.
            tally ruinCommon "rations"      60000 `shouldSatisfy` \n →
                n > 13000 ∧ n < 14800
            tally ruinCommon "steel_dagger" 60000 `shouldSatisfy` \n →
                n > 4100 ∧ n < 5200
            tally ruinCommon "field_toolbox" 60000 `shouldSatisfy` \n →
                n > 4100 ∧ n < 5200

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
