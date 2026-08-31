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
    , luaSpec
    ) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import System.Directory
    (getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Engine.Asset.YamlLootTables
    ( LootTableYamlDef(..), LootTableYamlEntry(..), loadLootTableYaml )
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Core (CoreCapability, toCoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Scripting.Lua.API.LootTables (loadLootTableYamlFn)
import LootTable.Types
    ( LootTableDef(..), LootTableEntry(..), LootTableRegistry
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
                `shouldBe` [Just "shovel_steel", Just "shovel_steel"]
            sequenceFor ruinCommon 42 3 1 2
                `shouldBe` [Just "first_aid_kit", Just "rations"]
            sequenceFor ruinCommon 42 4 1 2
                `shouldBe` [Just "steel_hardware", Just "steel_dagger"]

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

    -- #1946: the authoring boundary for `weight` itself. Every case
    -- below goes through the REAL 'loadLootTableYaml' on a real file,
    -- because two halves of what is under test are facts about the
    -- source text: `.nan`/`.inf` resolving to STRINGS rather than
    -- numbers, and an ordinary Scientific literal narrowing to
    -- Infinity or to 0.0 in the engine's stored 32-bit Float. A
    -- fixture built from Haskell values could express neither.
    describe "weight domain (#1946)" $ do
        describe "rejected weights" $ do
            it "rejects zero, which pickByWeight would still SELECT at \
               \draw 0 — the one outcome `weight: 0` means to rule out" $
                rejectsNaming ["positive", "0.0"] (probeTable "0")

            it "rejects an explicitly floating zero the same way" $
                rejectsNaming ["positive", "0.0"] (probeTable "0.0")

            it "rejects a negative weight, which shrinks the total and \
               \makes later entries unreachable" $
                rejectsNaming ["positive", "-1.0"] (probeTable "-1")

            it "rejects a finite YAML literal that OVERFLOWS the \
               \engine's 32-bit Float to infinity — an infinite weight \
               \poisons the sum and hands every draw to the last entry" $
                rejectsNaming ["finite"] (probeTable "1.0e+100")

            it "rejects a positive weight that UNDERFLOWS to zero in \
               \that same Float, reporting the effective 0.0 rather \
               \than the authored literal" $
                -- The mirror image of the overflow above: positivity is
                -- evaluated AFTER narrowing, so a Scientific the YAML
                -- parser is perfectly happy with cannot author a weight
                -- the runtime would only ever sum as zero.
                rejectsNaming ["positive", "0.0"] (probeTable "1.0e-60")

            it "rejects .nan, which YAML's scalar resolver hands over \
               \as a STRING rather than a number" $
                rejectsNaming ["number", ".nan"] (probeTable ".nan")

            it "rejects .inf the same way" $
                rejectsNaming ["number", ".inf"] (probeTable ".inf")

            it "rejects an absent weight — it is required and has no \
               \default, exactly as before" $
                rejectsNaming ["required"]
                    (tableSource "probe_ruins"
                        ["  - id: rations", "  - id: quinoa_sack\n    weight: 1"])

            it "rejects an authored null, which aeson reads as absent" $
                rejectsNaming ["required"] (probeTable "null")

            it "rejects a non-numeric weight by entry rather than by \
               \list index" $
                rejectsNaming ["number", "often"] (probeTable "\"often\"")

        it "names the TABLE and the offending ENTRY, not a bare JSON \
           \path — an index nobody can map back without counting" $
            -- The rejected entry is the SECOND one, so an assertion
            -- naming the first would pass by accident.
            rejectsNaming ["probe_ruins", "2", "quinoa_sack", "weight"]
                (probeTable "0")

        it "fails the WHOLE document, so the valid sibling entry is \
           \not salvaged either" $
            withTempLootYaml (probeTable "0") $ \path → do
                (logger, _) ← callbackLogger
                loadLootTableYaml logger path `shouldReturn` Nothing

        describe "accepted weights" $ do
            it "accepts ordinary positive weights" $
                acceptsAs ("probe_ruins", [("rations", 3), ("quinoa_sack", 1)])
                    (probeTable "1")

            it "accepts an arbitrarily small positive weight — the \
               \boundary is zero, and it is exclusive" $
                acceptsAs
                    ("probe_ruins", [("rations", 3), ("quinoa_sack", 1.0e-3)])
                    (probeTable "0.001")

            -- Requirement 5, gated at the YAML boundary rather than
            -- only over a directly constructed def: an empty table is a
            -- defined outcome, so the decoder must not reject it while
            -- closing the weight domain.
            it "accepts entries: [] and still rolls Nothing" $
                withTempLootYaml (tableSource "probe_empty" []) $ \path → do
                    (logger, _) ← callbackLogger
                    mDef ← loadLootTableYaml logger path
                    fmap ltydEntries mDef `shouldBe` Just []
                    fmap (\d → rollLootTableFor (toLootTableDef d) (ctxAt 42 1 1 1))
                        mDef `shouldBe` Just Nothing

            it "still accepts the shipped data/loot_tables/ruin_common.yaml \
               \through the real loader" $ do
                (logger, _) ← callbackLogger
                mDef ← loadLootTableYaml logger
                    "data/loot_tables/ruin_common.yaml"
                fmap toLootTableDef mDef `shouldBe` Just pinnedRuinCommon

-- * #1946 fixtures and assertions

-- | A loot table document named @tid@ whose @entries:@ list is
--   @entryLines@ verbatim (empty for @entries: []@).
tableSource ∷ Text → [Text] → Text
tableSource tid entryLines
    | null entryLines = T.unlines ["id: " <> tid, "entries: []"]
    | otherwise       = T.unlines (["id: " <> tid, "entries:"] ⧺ entryLines)

-- | Two entries under the name every rejection assertion looks for.
--   The first is always valid; the SECOND carries @w@ authored exactly
--   as a content author writes it — including the spellings (@null@, a
--   quoted string, @.nan@) that are not numbers at all.
probeTable ∷ Text → Text
probeTable w = tableSource "probe_ruins"
    [ "  - id: rations\n    weight: 3"
    , "  - id: quinoa_sack\n    weight: " <> w
    ]

-- | Load @src@ through the REAL loader and require whole-document
--   rejection: 'Nothing' plus exactly one 'CatAsset' 'LevelWarn' whose
--   message names the file and every token in @tokens@.
--
--   Tokens are matched as whole WORDS of a punctuation-scrubbed
--   message, not substrings, so @finite@ cannot be satisfied by a
--   message that only ever says @infinite@. The scrub deliberately
--   leaves @.@ and @-@ alone: they are inside the values (@-1.0@,
--   @.nan@) the tokens have to match.
rejectsNaming ∷ [String] → Text → Expectation
rejectsNaming tokens src =
    withTempLootYaml src $ \path → do
        (logger, entriesRef) ← callbackLogger
        mDef ← loadLootTableYaml logger path
        mDef `shouldBe` Nothing
        entries ← readIORef entriesRef
        case entries of
            [entry] → do
                leLevel entry `shouldBe` LevelWarn
                leCategory entry `shouldBe` CatAsset
                let msg     = T.unpack (leMessage entry)
                    ws      = words (map scrub msg)
                    wanted  = path : tokens
                    missing = [t | t ← wanted, t `notElem` ws]
                if null missing
                  then pure ()
                  else expectationFailure $
                      "rejected, but the warning does not name "
                      ⧺ show missing ⧺ ": " ⧺ msg
            other → expectationFailure $
                "expected exactly one captured log entry, got "
                ⧺ show (length other)
  where
    scrub c = if c `elem` ("'\"(),:;=\\\8212" ∷ String) then ' ' else c

-- | Load @src@ and require exactly that table id and those entries.
acceptsAs ∷ (Text, [(Text, Float)]) → Text → Expectation
acceptsAs expected src =
    withTempLootYaml src $ \path → do
        (logger, _) ← callbackLogger
        mDef ← loadLootTableYaml logger path
        fmap (\d → (ltydId d, [ (ltyeId e, ltyeWeight e)
                              | e ← ltydEntries d ])) mDef
            `shouldBe` Just expected

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef'. 'CatAsset' debug logging stays OFF (the default) so a
--   rejection's warning is the only entry captured, which is what lets
--   'rejectsNaming' require exactly one.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)

withTempLootYaml ∷ Text → (FilePath → IO a) → IO a
withTempLootYaml contents action = do
    tmp ← getTemporaryDirectory
    let dir  = tmp </> "synarchy-loot-weight-spec"
        path = dir </> "probe_loot_table.yaml"
    createDirectoryIfMissing True dir
    writeFile path (T.unpack contents)
    action path `finally` removeDirectoryRecursive dir

-- | The load-and-register boundary itself (#1946), driven through the
--   real @engine.loadLootTableYaml@ verb and the live engine's own
--   @content-registries@ projection.
--
--   The decoder cases above prove the FILE is rejected; this proves
--   what that rejection means for the registry, which is a separate
--   contract: 'Engine.Scripting.Lua.API.LootTables.loadLootTableYamlFn'
--   mutates 'crLootTableRegistryRef' only once 'loadLootTableYaml' has
--   answered 'Just', and 'registerLootTable' inserts BY TABLE ID — so
--   without the decoder guard a semantically invalid file would silently
--   replace a good table of the same id.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Location loot determinism"'@.
luaSpec ∷ SpecWith EngineEnv
luaSpec = describe "Location loot determinism (load and register)" $ do
    it "leaves an already-registered table of the same id EXACTLY as it \
       \was when the replacement file is rejected" $ \env →
        withPrepopulatedRegistry env $ \core regs → do
            loadVia core regs (probeTable "0") `shouldReturn` Just 0
            reg ← readIORef (crLootTableRegistryRef regs)
            lookupLootTable "probe_ruins" reg `shouldBe` Just priorProbeTable

    -- The control that keeps the assertion above honest: the same
    -- boundary DOES replace the table when the file is valid, so the
    -- untouched registry is the rejection's doing and not a wiring
    -- failure that never registers anything.
    it "still replaces that table when the file is valid" $ \env →
        withPrepopulatedRegistry env $ \core regs → do
            loadVia core regs (probeTable "1") `shouldReturn` Just 1
            reg ← readIORef (crLootTableRegistryRef regs)
            lookupLootTable "probe_ruins" reg `shouldBe` Just LootTableDef
                { ltdId      = "probe_ruins"
                , ltdEntries = [ LootTableEntry "rations"     3
                               , LootTableEntry "quinoa_sack" 1 ]
                }

-- | The table already in the registry when a replacement file arrives.
priorProbeTable ∷ LootTableDef
priorProbeTable = LootTableDef
    { ltdId      = "probe_ruins"
    , ltdEntries = [LootTableEntry "steel_dagger" 7]
    }

-- | Run @action@ with the live engine's loot-table registry holding
--   'priorProbeTable' alone, restoring whatever it held before. The ref
--   is shared with every other spec riding this engine, so it is
--   borrowed rather than reassigned.
withPrepopulatedRegistry
    ∷ EngineEnv
    → (CoreCapability → ContentRegistriesCapability → IO a)
    → IO a
withPrepopulatedRegistry env action = do
    let core = toCoreCapability env
        regs = toContentRegistriesCapability env
        ref  = crLootTableRegistryRef regs ∷ IORef LootTableRegistry
    before ← readIORef ref
    writeIORef ref (registerLootTable priorProbeTable emptyLootTableRegistry)
    action core regs `finally` writeIORef ref before

-- | One @engine.loadLootTableYaml(path)@ call over a temporary file
--   holding @src@, returning the verb's own 1/0 result.
loadVia ∷ CoreCapability → ContentRegistriesCapability → Text
        → IO (Maybe Lua.Integer)
loadVia core regs src = withTempLootYaml src $ \path → Lua.run $ do
    Lua.openlibs
    Lua.pushstring (TE.encodeUtf8 (T.pack path))
    _ ← loadLootTableYamlFn core regs
    Lua.tointeger (-1)
