-- | "Loot profiles" (#2499, epic #1231 PLC-12): the
--   @data/loot_profiles/*.yaml@ authoring boundary, its registry, and
--   the two read-only @loot@ queries over it.
--
--   Three things are pinned here, and they are separate contracts:
--
--   1. __The shipped file.__ @ruin_industrial_salvage.yaml@ is decoded
--      through the REAL loader and compared against an explicit pin,
--      the same technique 'Test.Headless.Location.LootDeterminism' uses
--      for @ruin_common@. Nothing consumes a profile yet, so a fixture
--      copy would let the shipped calibration drift silently until
--      PLC-13 arrives and starts rolling it.
--
--   2. __Every authored-field rule, at its own boundary.__ Each rule is
--      driven through the production caller (a real file, the real
--      'loadLootProfileYaml') at the tightest value the guard admits and
--      the tightest one it must refuse — for @chance@ that is the
--      literal successor of @1.0@ in the engine's stored 32-bit
--      'Float', not a comfortable @1.5@. A guard that is deleted,
--      loosened to @≤@, or widened to any nearby constant fails an
--      example here.
--
--   3. __What a rejection means for the REGISTRY.__ Decoding is not
--      registering: the loader publishes only after both the decode and
--      the unknown-item check have passed, so a rejected replacement
--      leaves the previously registered profile exactly as it was.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Loot profiles"'@.
module Test.Headless.Loot.Profiles
    ( spec
    , luaSpec
    ) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.FilePath ((</>))
import Engine.Asset.YamlLootProfiles
    ( LootProfileYamlDef(..), LootProfileYamlEntry(..)
    , loadLootProfileYaml, lootProfileItemErrors )
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )
import Engine.Core.State (EngineEnv, itemManagerRef)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability, toContentRegistriesViewCapability)
import Engine.Scripting.Lua.API.LootProfiles
    (loadLootProfileYamlFn, lootProfileFn, lootListProfilesFn)
import Engine.Asset.Handle (TextureHandle(..))
import Item.Types (ItemDef(..), ItemManager(..))
import LootProfile.Types
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

-----------------------------------------------------------------------
-- The shipped profile
-----------------------------------------------------------------------

-- | The pinned composition of
--   @data/loot_profiles/ruin_industrial_salvage.yaml@ (design D-20).
--   When the profile is retuned — which is PLC-10's job, not this
--   slice's — this pin is updated deliberately alongside it.
--
--   Chances are written as the 32-bit 'Float' the engine stores, so the
--   comparison is against the value PLC-13 would actually roll against
--   rather than against the decimal literal in the file.
pinnedIndustrialSalvage ∷ LootProfileDef
pinnedIndustrialSalvage = LootProfileDef
    { lpdId            = "ruin_industrial_salvage"
    , lpdMultiplierMin = 1
    , lpdMultiplierMax = 4
    , lpdEntries =
        [ LootProfileEntry "steel_bar"            0.30  5
        , LootProfileEntry "electric_motor"       0.05  1
        , LootProfileEntry "steel_hardware"       0.30 10
        , LootProfileEntry "high_voltage_battery" 0.01  1
        , LootProfileEntry "steel_plate"          0.30  5
        , LootProfileEntry "processing_unit"      0.05  1
        , LootProfileEntry "wiring"               0.30  5
        , LootProfileEntry "rations"              0.10  5
        ]
    }

shippedProfilePath ∷ FilePath
shippedProfilePath = "data/loot_profiles/ruin_industrial_salvage.yaml"

-- | The engine's own YAML → registry conversion, so the pin above is
--   taken against the def the engine would actually register rather
--   than against a re-typed approximation of the file. It is spelled
--   out here rather than exported from the loader because the loader's
--   copy runs inside a Lua binding, and this half of the gate runs
--   without a VM at all.
toProfileDef ∷ LootProfileYamlDef → LootProfileDef
toProfileDef d = LootProfileDef
    { lpdId            = lpydId d
    , lpdMultiplierMin = lpydMultiplierMin d
    , lpdMultiplierMax = lpydMultiplierMax d
    , lpdEntries       = [ LootProfileEntry (lpyeItem e) (lpyeChance e)
                                            (lpyeQuantityFactor e)
                         | e ← lpydEntries d ]
    }

-----------------------------------------------------------------------
-- Document fixtures
-----------------------------------------------------------------------

-- | A profile document: an @id@ line, a @quantity_multiplier@ block
--   spelled verbatim, and an @entries@ list spelled verbatim (empty for
--   @entries: []@). Everything under test is authored as TEXT, because
--   half the rules here are about spellings — @null@, a quoted string,
--   @.nan@, a literal that overflows the stored 'Float' — that a
--   fixture built from Haskell values could not express.
docSource ∷ Text → Text → [Text] → Text
docSource pid mult entryLines
    | null entryLines = T.unlines ["id: " <> pid, mult, "entries: []"]
    | otherwise       = T.unlines (["id: " <> pid, mult, "entries:"]
                                   ⧺ entryLines)

okMult ∷ Text
okMult = multOf "1" "4"

multOf ∷ Text → Text → Text
multOf lo hi = "quantity_multiplier:\n  min: " <> lo <> "\n  max: " <> hi

entryOf ∷ Text → Text → Text → Text
entryOf item chance factor = T.intercalate "\n"
    [ "  - item: " <> item
    , "    chance: " <> chance
    , "    quantity_factor: " <> factor ]

-- | Two entries under the names every assertion below looks for. The
--   FIRST is always valid and the SECOND carries the value under test,
--   so a rejection assertion naming entry 2 cannot be satisfied by a
--   diagnostic that only ever reaches the first entry.
probeProfile ∷ Text → Text → Text
probeProfile chance factor = docSource "probe_salvage" okMult
    [ entryOf "rations" "0.5" "2"
    , entryOf "steel_bar" chance factor ]

-- | The healthy document every acceptance case starts from.
healthyProfile ∷ Text
healthyProfile = probeProfile "0.25" "3"

healthyDecoded ∷ (Text, (Int, Int), [(Text, Float, Int)])
healthyDecoded = ("probe_salvage", (1, 4),
    [ ("rations", 0.5, 2), ("steel_bar", 0.25, 3) ])

withTempProfileYaml ∷ Text → (FilePath → IO α) → IO α
withTempProfileYaml contents action =
    withExclusiveTempDirectory "synarchy-loot-profile-spec" $ \dir → do
        let path = dir </> "probe_loot_profile.yaml"
        writeFile path (T.unpack contents)
        action path

-----------------------------------------------------------------------
-- Assertions
-----------------------------------------------------------------------

-- | Load @src@ through the REAL loader and require whole-document
--   rejection: 'Nothing' plus exactly one 'CatAsset' 'LevelWarn' whose
--   message names the file and every token in @wanted@ — and none of
--   the tokens in @unwanted@.
--
--   Tokens are matched as whole WORDS of a punctuation-scrubbed
--   message, not substrings, so @finite@ cannot be satisfied by a
--   message that only ever says @infinite@, and the @unwanted@ list can
--   assert that a PROFILE-level rejection prints no entry coordinates
--   it does not have. The scrub deliberately leaves @.@ and @-@ alone:
--   they are inside the values (@-1.0e-30@, @.nan@) the tokens match.
rejectsNamingBut ∷ [String] → [String] → Text → Expectation
rejectsNamingBut wanted unwanted src =
    withTempProfileYaml src $ \path → do
        (logger, entriesRef) ← callbackLogger
        mDef ← loadLootProfileYaml logger path
        mDef `shouldBe` Nothing
        entries ← readIORef entriesRef
        case entries of
            [entry] → do
                leLevel entry `shouldBe` LevelWarn
                leCategory entry `shouldBe` CatAsset
                let msg     = T.unpack (leMessage entry)
                    ws      = words (map scrub msg)
                    missing = [t | t ← path : wanted, t `notElem` ws]
                    present = [t | t ← unwanted, t `elem` ws]
                if null missing ∧ null present
                  then pure ()
                  else expectationFailure $
                      "rejected, but the warning does not name "
                      ⧺ show missing ⧺ " and wrongly names "
                      ⧺ show present ⧺ ": " ⧺ msg
            other → expectationFailure $
                "expected exactly one captured log entry, got "
                ⧺ show (length other)
  where
    scrub c = if c `elem` ("'\"(),:;=\\\8212" ∷ String) then ' ' else c

rejectsNaming ∷ [String] → Text → Expectation
rejectsNaming wanted = rejectsNamingBut wanted []

-- | Load @src@ and require exactly that profile id, multiplier range,
--   and entry list.
acceptsAs ∷ (Text, (Int, Int), [(Text, Float, Int)]) → Text → Expectation
acceptsAs expected src =
    withTempProfileYaml src $ \path → do
        (logger, _) ← callbackLogger
        mDef ← loadLootProfileYaml logger path
        fmap decoded mDef `shouldBe` Just expected
  where
    decoded d = ( lpydId d
                , (lpydMultiplierMin d, lpydMultiplierMax d)
                , [ (lpyeItem e, lpyeChance e, lpyeQuantityFactor e)
                  | e ← lpydEntries d ] )

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef'. 'CatAsset' debug logging stays OFF (the default) so a
--   rejection's warning is the only entry captured, which is what lets
--   'rejectsNamingBut' require exactly one.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)

-----------------------------------------------------------------------
-- The decode half
-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "Loot profiles" $ do

    describe "the shipped ruin_industrial_salvage profile" $ do
        it "decodes through the real loader with the pinned entries, in \
           \authored order" $ do
            (logger, _) ← callbackLogger
            mDef ← loadLootProfileYaml logger shippedProfilePath
            fmap toProfileDef mDef `shouldBe` Just pinnedIndustrialSalvage

        -- D-20's own precondition is that every id this file names is a
        -- real definition. It is NOT re-derived here from a hand-rolled
        -- scan of data/items/: `tools/content_registry_probe.py` boots
        -- the real engine, loads the real item tree through the real
        -- verb, and then requires this profile to register — which is
        -- the same claim proved against production's own loaders rather
        -- than against a second parser this spec would own.

    describe "profile-level rejections" $ do
        it "rejects a missing id, with no entry coordinates it does not \
           \have" $
            rejectsNamingBut ["id", "required"] ["entry"] $
                T.unlines [okMult, "entries:", entryOf "rations" "0.5" "2"]

        it "rejects an empty id" $
            rejectsNaming ["id", "empty"] (docSource "\"\"" okMult
                [entryOf "rations" "0.5" "2"])

        it "rejects a non-string id" $
            rejectsNaming ["id", "name", "string"] (docSource "5" okMult
                [entryOf "rations" "0.5" "2"])

        -- The libyaml resolver keeps the LAST binding of a repeated
        -- mapping key, so without this the document below would decode
        -- cleanly as `second` and register a profile whose name nobody
        -- reading the file would predict. It is also the ONE case with
        -- no profile to name: printing either id would print exactly
        -- the value this rejection exists to distrust.
        it "rejects a REPEATED top-level id key, naming the raw path and \
           \neither of the two ids" $
            rejectsNamingBut [".id", "duplicate"] ["first", "second"] $
                T.unlines [ "id: first", "id: second", okMult
                          , "entries:", entryOf "rations" "0.5" "2" ]

        -- Everything a duplicate did NOT touch stays nameable, so a
        -- duplicate INSIDE an entry carries the same profile / 1-based
        -- index / item coordinates a bad `chance` in that entry would.
        it "rejects a repeated key inside an entry, by profile, 1-based \
           \entry index and item — not by a raw 0-based path" $
            rejectsNamingBut
                ["probe_salvage", "2", "steel_bar", "chance", "duplicate"]
                [".entries[1].chance"] $
                T.unlines
                [ "id: probe_salvage", okMult, "entries:"
                , entryOf "rations" "0.5" "2"
                , "  - item: steel_bar\n    chance: 0.5\n    chance: 0.9\
                  \\n    quantity_factor: 3" ]

        it "rejects a repeated key inside quantity_multiplier, naming \
           \the profile and the block" $
            rejectsNamingBut
                ["probe_salvage", "quantity_multiplier", "min", "duplicate"]
                ["entry"] $
                docSource "probe_salvage"
                    "quantity_multiplier:\n  min: 1\n  min: 2\n  max: 4"
                    [entryOf "rations" "0.5" "2"]

        it "rejects a repeated top-level key other than id, naming the \
           \profile — that name is still trustworthy" $
            rejectsNamingBut ["probe_salvage", "entries", "duplicate"]
                             [".entries"] $
                T.unlines
                [ "id: probe_salvage", okMult
                , "entries:", entryOf "rations" "0.5" "2"
                , "entries:", entryOf "steel_bar" "0.5" "3" ]

        -- A duplicated `entries` key makes the INDEX untrustworthy for
        -- the same reason a duplicated `id` makes the name untrustworthy
        -- — libyaml reports the inner duplicate against whichever list
        -- it walked, while only the last list decoded. The profile is
        -- still named; the entry is not guessed at.
        it "keeps the profile but drops entry coordinates when entries \
           \itself repeated" $
            rejectsNamingBut ["probe_salvage", "duplicate", ".entries[0].chance"]
                             ["rations", "steel_bar"] $
                T.unlines
                [ "id: probe_salvage", okMult
                , "entries:"
                , "  - item: rations\n    chance: 0.5\n    chance: 0.9\
                  \\n    quantity_factor: 2"
                , "entries:", entryOf "steel_bar" "0.5" "3" ]

        it "rejects entries: [] — a profile that can only ever realize \
           \nothing" $
            rejectsNamingBut ["probe_salvage", "entries", "empty"] ["entry"] $
                docSource "probe_salvage" okMult []

        it "rejects an absent entries list" $
            rejectsNaming ["probe_salvage", "entries", "required"] $
                T.unlines ["id: probe_salvage", okMult]

        it "rejects an authored null entries list" $
            rejectsNaming ["probe_salvage", "entries", "required"] $
                T.unlines ["id: probe_salvage", okMult, "entries: null"]

        it "rejects a scalar where the entries list belongs" $
            rejectsNaming ["probe_salvage", "entries", "list"] $
                T.unlines ["id: probe_salvage", okMult, "entries: 3"]

    describe "quantity_multiplier" $ do
        it "rejects an absent block — it is required and has no default" $
            rejectsNamingBut
                ["probe_salvage", "quantity_multiplier", "required"] ["entry"] $
                T.unlines [ "id: probe_salvage", "entries:"
                          , entryOf "rations" "0.5" "2" ]

        it "rejects an authored null block" $
            rejectsNaming ["probe_salvage", "quantity_multiplier", "required"] $
                docSource "probe_salvage" "quantity_multiplier: null"
                    [entryOf "rations" "0.5" "2"]

        it "rejects a scalar where the block belongs" $
            rejectsNaming ["probe_salvage", "quantity_multiplier", "block"] $
                docSource "probe_salvage" "quantity_multiplier: 4"
                    [entryOf "rations" "0.5" "2"]

        it "rejects an absent min" $
            rejectsNaming ["quantity_multiplier", "min", "required"] $
                docSource "probe_salvage" "quantity_multiplier:\n  max: 4"
                    [entryOf "rations" "0.5" "2"]

        it "rejects an absent max" $
            rejectsNaming ["quantity_multiplier", "max", "required"] $
                docSource "probe_salvage" "quantity_multiplier:\n  min: 1"
                    [entryOf "rations" "0.5" "2"]

        -- ONE below the inclusive floor. Paired with the min=max=1
        -- acceptance below, this fails for a guard deleted, loosened to
        -- `n ≥ 0`, or tightened to `n ≥ 2`.
        it "rejects min: 0, one below the inclusive floor of 1" $
            rejectsNaming ["quantity_multiplier", "min", "positive", "0"] $
                docSource "probe_salvage" (multOf "0" "4")
                    [entryOf "rations" "0.5" "2"]

        it "rejects a negative min" $
            rejectsNaming ["quantity_multiplier", "min", "positive", "-2"] $
                docSource "probe_salvage" (multOf "-2" "4")
                    [entryOf "rations" "0.5" "2"]

        it "rejects a FRACTIONAL min — the multiplier is a whole-number \
           \range" $
            rejectsNaming ["quantity_multiplier", "min", "whole"] $
                docSource "probe_salvage" (multOf "1.5" "4")
                    [entryOf "rations" "0.5" "2"]

        it "rejects a fractional max the same way" $
            rejectsNaming ["quantity_multiplier", "max", "whole"] $
                docSource "probe_salvage" (multOf "1" "4.5")
                    [entryOf "rations" "0.5" "2"]

        -- ONE below min, against a min deliberately larger than 1 so an
        -- implementation that compared max against the constant 1
        -- rather than against min would pass the `max: 1` case and fail
        -- here.
        it "rejects max exactly one below min" $
            rejectsNaming ["quantity_multiplier", "max", "min", "3", "2"] $
                docSource "probe_salvage" (multOf "3" "2")
                    [entryOf "rations" "0.5" "2"]

        it "accepts min == max, the degenerate range" $
            acceptsAs ("probe_salvage", (3, 3), [("rations", 0.5, 2)]) $
                docSource "probe_salvage" (multOf "3" "3")
                    [entryOf "rations" "0.5" "2"]

        it "accepts the smallest legal range, min = max = 1" $
            acceptsAs ("probe_salvage", (1, 1), [("rations", 0.5, 2)]) $
                docSource "probe_salvage" (multOf "1" "1")
                    [entryOf "rations" "0.5" "2"]

    describe "entry chance" $ do
        it "rejects an absent chance" $
            rejectsNaming ["probe_salvage", "2", "steel_bar", "chance"
                          , "required"] $
                docSource "probe_salvage" okMult
                    [ entryOf "rations" "0.5" "2"
                    , "  - item: steel_bar\n    quantity_factor: 3" ]

        it "rejects an authored null" $
            rejectsNaming ["chance", "required"] (probeProfile "null" "3")

        -- The tightest possible refusal above the inclusive ceiling:
        -- 1.0000001 is the literal SUCCESSOR of 1.0 in the engine's
        -- stored 32-bit Float. A guard weakened to `> 1.5`, to `≥ 1.1`,
        -- or deleted outright accepts this; only `> 1` refuses it.
        it "rejects the very next Float above 1.0" $
            rejectsNaming ["chance", "between", "0", "1", "inclusive"]
                (probeProfile "1.0000001" "3")

        it "rejects a chance comfortably above 1" $
            rejectsNaming ["chance", "between", "0", "1", "inclusive"]
                (probeProfile "1.5" "3")

        -- The mirror image below the inclusive floor.
        it "rejects a tiny negative chance" $
            rejectsNaming ["chance", "between", "0", "1", "inclusive"]
                (probeProfile "-1.0e-30" "3")

        it "rejects a chance that OVERFLOWS the stored 32-bit Float to \
           \infinity, before the range check can see it" $
            rejectsNaming ["chance", "finite"] (probeProfile "1.0e+100" "3")

        it "rejects .nan, which YAML's scalar resolver hands over as a \
           \STRING rather than a number" $
            rejectsNaming ["chance", "number", ".nan"] (probeProfile ".nan" "3")

        it "rejects .inf the same way" $
            rejectsNaming ["chance", "number", ".inf"] (probeProfile ".inf" "3")

        it "rejects a quoted numeric string — a chance is a number, and \
           \a string that looks like one is an authoring slip" $
            rejectsNaming ["chance", "number"] (probeProfile "\"0.5\"" "3")

        it "accepts exactly 0 — an authored entry that cannot appear" $
            acceptsAs ("probe_salvage", (1, 4),
                       [("rations", 0.5, 2), ("steel_bar", 0, 3)])
                (probeProfile "0" "3")

        it "accepts exactly 1 — an entry that always appears" $
            acceptsAs ("probe_salvage", (1, 4),
                       [("rations", 0.5, 2), ("steel_bar", 1, 3)])
                (probeProfile "1" "3")

        it "accepts an explicitly floating 1.0 as the same ceiling" $
            acceptsAs ("probe_salvage", (1, 4),
                       [("rations", 0.5, 2), ("steel_bar", 1, 3)])
                (probeProfile "1.0" "3")

    describe "entry quantity_factor" $ do
        it "rejects an absent factor" $
            rejectsNaming ["probe_salvage", "2", "steel_bar"
                          , "quantity_factor", "required"] $
                docSource "probe_salvage" okMult
                    [ entryOf "rations" "0.5" "2"
                    , "  - item: steel_bar\n    chance: 0.5" ]

        it "rejects an authored null" $
            rejectsNaming ["quantity_factor", "required"]
                (probeProfile "0.5" "null")

        -- ONE below the inclusive floor. Zero is not a disable toggle:
        -- PLC-13 would admit a lot and then have nothing to put in it.
        it "rejects 0, one below the inclusive floor of 1" $
            rejectsNaming ["quantity_factor", "positive", "0"]
                (probeProfile "0.5" "0")

        it "rejects a negative factor, which would size a NEGATIVE lot" $
            rejectsNaming ["quantity_factor", "positive", "-1"]
                (probeProfile "0.5" "-1")

        -- The tightest fractional refusal: 1.0000001 is a whole unit
        -- away from nothing, and a `floor`/`round` shortcut would
        -- silently accept it as 1.
        it "rejects the very next value above the whole number 1" $
            rejectsNaming ["quantity_factor", "whole"]
                (probeProfile "0.5" "1.0000001")

        it "rejects an ordinary fractional factor" $
            rejectsNaming ["quantity_factor", "whole"]
                (probeProfile "0.5" "2.5")

        it "rejects a quoted numeric string" $
            rejectsNaming ["quantity_factor", "whole"]
                (probeProfile "0.5" "\"3\"")

        -- Not a fractional value but an unstorable one: a magnitude
        -- beyond Int must be REFUSED, never silently wrapped into a
        -- small (or negative) lot size.
        it "rejects a magnitude the engine's Int cannot hold" $
            rejectsNaming ["quantity_factor", "whole"]
                (probeProfile "0.5" "99999999999999999999")

        it "accepts exactly 1, the inclusive floor" $
            acceptsAs ("probe_salvage", (1, 4),
                       [("rations", 0.5, 2), ("steel_bar", 0.5, 1)])
                (probeProfile "0.5" "1")

        it "accepts an explicitly floating 1.0 as the same whole number" $
            acceptsAs ("probe_salvage", (1, 4),
                       [("rations", 0.5, 2), ("steel_bar", 0.5, 1)])
                (probeProfile "0.5" "1.0")

    describe "entry item" $ do
        it "rejects an absent item, stopping the coordinates at the \
           \index because there is no name to print" $
            rejectsNamingBut ["probe_salvage", "2", "item", "required"]
                             ["steel_bar"] $
                docSource "probe_salvage" okMult
                    [ entryOf "rations" "0.5" "2"
                    , "  - chance: 0.5\n    quantity_factor: 3" ]

        it "rejects a non-string item" $
            rejectsNaming ["probe_salvage", "2", "item", "name", "string"]
                (probeProfile "0.5" "3" `withSecondItem` "5")

    it "fails the WHOLE document, so the valid FIRST entry is not \
       \salvaged either" $
        withTempProfileYaml (probeProfile "1.5" "3") $ \path → do
            (logger, _) ← callbackLogger
            loadLootProfileYaml logger path `shouldReturn` Nothing

    it "accepts the healthy document every rejection above is a single \
       \edit away from" $
        acceptsAs healthyDecoded healthyProfile

    describe "unknown item ids (D-20)" $ do
        it "names every unresolved entry, by profile and 1-based index" $ do
            let d = LootProfileYamlDef "probe_salvage" 1 4
                        [ LootProfileYamlEntry "rations" 0.5 2
                        , LootProfileYamlEntry "no_such_item" 0.5 3
                        , LootProfileYamlEntry "also_absent" 0.5 1 ]
                errs = lootProfileItemErrors (HS.fromList ["rations"]) d
            length errs `shouldBe` 2
            errs `shouldSatisfy` any (\e → "entry 2" `T.isInfixOf` e
                                         ∧ "no_such_item" `T.isInfixOf` e)
            errs `shouldSatisfy` any (\e → "entry 3" `T.isInfixOf` e
                                         ∧ "also_absent" `T.isInfixOf` e)

        it "answers nothing when every entry resolves" $ do
            let d = LootProfileYamlDef "probe_salvage" 1 4
                        [LootProfileYamlEntry "rations" 0.5 2]
            lootProfileItemErrors (HS.fromList ["rations", "steel_bar"]) d
                `shouldBe` []

-- | Replace the SECOND entry's @item@ line, so the item rule can be
--   driven with a spelling 'entryOf' cannot author.
withSecondItem ∷ Text → Text → Text
withSecondItem src item =
    T.replace "  - item: steel_bar" ("  - item: " <> item) src

-----------------------------------------------------------------------
-- The load-and-register half
-----------------------------------------------------------------------

-- | The two item definitions the fixtures below resolve against.
--   @steel_bar@ is the decoy: a profile naming it must register, so a
--   rejection is provably about the UNKNOWN id and not about the check
--   refusing everything.
fixtureItems ∷ ItemManager
fixtureItems = ItemManager $ HM.fromList
    [ ("rations",   fixtureDef "rations")
    , ("steel_bar", fixtureDef "steel_bar") ]

fixtureDef ∷ Text → ItemDef
fixtureDef name = ItemDef
    { idName = name, idDisplayName = name
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.4, idWeightSpec = Nothing
    , idBulk = 0.4, idStorage = Nothing, idKind = "misc"
    , idCategory = "Materials", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing
    , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
    , idArmor = Nothing, idUnequippable = False, idBuffs = []
    , idInsulation = 0, idSourcePath = "test-fixture"
    }

-- | The profile already in the registry when a replacement file
--   arrives.
priorProbeProfile ∷ LootProfileDef
priorProbeProfile = LootProfileDef
    { lpdId            = "probe_salvage"
    , lpdMultiplierMin = 7
    , lpdMultiplierMax = 9
    , lpdEntries       = [LootProfileEntry "rations" 0.75 4]
    }

-- | Run @action@ against the live engine's own registries, with the
--   loot-profile registry holding @seeded@ and the item registry
--   holding 'fixtureItems'. Both refs are SHARED with every other spec
--   riding this engine, so they are borrowed and restored rather than
--   reassigned.
--
--   The 'CoreCapability' handed over is the live one with its logger
--   swapped for a capturing backend: the diagnostics this half asserts
--   on (a replacement, an unknown item id) are the loader's own, and
--   they are only observable through the logger it was given.
withProfileFixture
    ∷ EngineEnv
    → LootProfileRegistry
    → (CoreCapability → ContentRegistriesCapability
       → ContentRegistriesViewCapability → IORef [LogEntry] → IO α)
    → IO α
withProfileFixture env seeded action = do
    (core, entriesRef) ← capturingCore env
    let regs     = toContentRegistriesCapability env
        regsView = toContentRegistriesViewCapability env
        profRef  = crLootProfileRegistryRef regs
        itemRef  = itemManagerRef env
    beforeProfiles ← readIORef profRef
    beforeItems    ← readIORef itemRef
    writeIORef profRef seeded
    writeIORef itemRef fixtureItems
    action core regs regsView entriesRef
        `finally` (writeIORef profRef beforeProfiles
                     ≫ writeIORef itemRef beforeItems)

-- | The live 'CoreCapability' with a capturing logger in place of the
--   engine's. Everything else aliases the engine's own handles.
capturingCore ∷ EngineEnv → IO (CoreCapability, IORef [LogEntry])
capturingCore env = do
    (logger, entriesRef) ← callbackLogger
    ref ← newIORef logger
    pure ((toCoreCapability env) { ccLoggerRef = ref }, entriesRef)

-- | One @engine.loadLootProfileYaml(path, true)@ call over a temporary
--   file holding @src@, answering the verb's own
--   @(resultCount, count, parsed)@.
--
--   The truthy SECOND argument is exactly what
--   @scripts/startup_loader.lua@ passes, so this drives the loader's
--   real startup arity rather than a shape only this spec uses.
loadWithOutcome
    ∷ CoreCapability → ContentRegistriesCapability
    → ContentRegistriesViewCapability → Text
    → IO (Lua.NumResults, Maybe Lua.Integer, Bool)
loadWithOutcome core regs regsView src =
    withTempProfileYaml src $ \path → Lua.run $ do
        Lua.openlibs
        Lua.pushstring (TE.encodeUtf8 (T.pack path))
        Lua.pushboolean True
        nres  ← loadLootProfileYamlFn core regs regsView
        count ← Lua.tointeger (-2)
        ok    ← Lua.toboolean (-1)
        pure (nres, count, ok)

-- | The same call with the path ALONE, the way every ad-hoc console and
--   probe caller makes it.
loadBare
    ∷ CoreCapability → ContentRegistriesCapability
    → ContentRegistriesViewCapability → Text
    → IO (Lua.NumResults, Maybe Lua.Integer)
loadBare core regs regsView src =
    withTempProfileYaml src $ \path → Lua.run $ do
        Lua.openlibs
        Lua.pushstring (TE.encodeUtf8 (T.pack path))
        nres  ← loadLootProfileYamlFn core regs regsView
        count ← Lua.tointeger (-1)
        pure (nres, count)

-- | Run @chunk@ in a bare Lua VM with the two read-only queries
--   installed as globals, and answer what it returned as text. A Lua
--   error is answered as its own message rather than swallowed, so a
--   broken chunk fails the example it belongs to instead of looking
--   like a nil result.
runProfileLua ∷ ContentRegistriesCapability → Text → IO (Maybe Text)
runProfileLua regs chunk = Lua.run $ do
    Lua.openlibs
    install "lootProfile"      (lootProfileFn regs)
    install "lootListProfiles" (lootListProfilesFn regs)
    st  ← Lua.dostring (TE.encodeUtf8 chunk)
    val ← fmap TE.decodeUtf8Lenient <$> Lua.tostring (-1)
    pure $ if st ≡ Lua.OK
             then val
             else Just ("lua error: " <> fromMaybe "?" val)
  where
    install name fn = do
        Lua.pushHaskellFunction fn
        Lua.setglobal (Lua.Name name)

luaSpec ∷ SpecWith EngineEnv
luaSpec = describe "Loot profiles (load and register)" $ do

    describe "the loader's outcome contract (#2203)" $ do
        it "answers (1, true) and registers the profile when the file is \
           \good" $ \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView _ → do
                loadWithOutcome core regs regsView healthyProfile
                    `shouldReturn` (2, Just 1, True)
                reg ← readIORef (crLootProfileRegistryRef regs)
                fmap lpdId (lookupLootProfile "probe_salvage" reg)
                    `shouldBe` Just "probe_salvage"

        it "answers (0, false) for a DECODE failure — the startup \
           \loader's terminal-failure signal" $ \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView _ →
                loadWithOutcome core regs regsView (probeProfile "1.5" "3")
                    `shouldReturn` (2, Just 0, False)

        -- The distinction this whole contract exists for: a file whose
        -- entry names an item that is not registered DECODED perfectly
        -- well. Reporting it as a parse failure would make an ordinary
        -- content mistake indistinguishable from a corrupt data tree.
        it "answers (0, true) for an unknown item id — the file parsed, \
           \and nothing was registered" $ \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView _ → do
                loadWithOutcome core regs regsView unknownItemProfile
                    `shouldReturn` (2, Just 0, True)
                reg ← readIORef (crLootProfileRegistryRef regs)
                lookupLootProfile "probe_unknown" reg `shouldBe` Nothing

        it "warns once per unresolved entry, naming the file, the \
           \profile and the entry" $ \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView entriesRef → do
                _ ← loadWithOutcome core regs regsView unknownItemProfile
                entries ← readIORef entriesRef
                let warns = [ leMessage e | e ← entries
                                          , leLevel e ≡ LevelWarn ]
                length warns `shouldBe` 1
                warns `shouldSatisfy` any (\m →
                    "probe_unknown" `T.isInfixOf` m
                    ∧ "entry 2" `T.isInfixOf` m
                    ∧ "no_such_item" `T.isInfixOf` m)

        it "answers ONE bare number when the caller does not opt in, so \
           \an ad-hoc `return engine.loadLootProfileYaml(p)` is \
           \unchanged" $ \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView _ →
                loadBare core regs regsView healthyProfile
                    `shouldReturn` (1, Just 1)

    describe "insert/replace by profile id" $ do
        it "leaves an already-registered profile EXACTLY as it was when \
           \the replacement file is rejected at decode" $ \env →
            withProfileFixture env seededRegistry $
              \core regs regsView _ → do
                _ ← loadWithOutcome core regs regsView
                        (replacementProfile "1.5" "3")
                reg ← readIORef (crLootProfileRegistryRef regs)
                lookupLootProfile "probe_salvage" reg
                    `shouldBe` Just priorProbeProfile

        -- The same guarantee one stage later: this file DECODES and is
        -- then refused for an unknown item, which is a different code
        -- path from the decode rejection above.
        it "leaves it as it was when the replacement is rejected for an \
           \unknown item id" $ \env →
            withProfileFixture env seededRegistry $
              \core regs regsView _ → do
                _ ← loadWithOutcome core regs regsView
                        (replacementNamingUnknownItem)
                reg ← readIORef (crLootProfileRegistryRef regs)
                lookupLootProfile "probe_salvage" reg
                    `shouldBe` Just priorProbeProfile

        -- The control that keeps both assertions above honest.
        it "DOES replace it, in place, when the replacement is valid" $
          \env →
            withProfileFixture env seededRegistry $
              \core regs regsView _ → do
                loadWithOutcome core regs regsView (replacementProfile "0.5" "6")
                    `shouldReturn` (2, Just 1, True)
                reg ← readIORef (crLootProfileRegistryRef regs)
                lookupLootProfile "probe_salvage" reg `shouldBe` Just
                    LootProfileDef
                        { lpdId            = "probe_salvage"
                        , lpdMultiplierMin = 2
                        , lpdMultiplierMax = 5
                        , lpdEntries =
                            [ LootProfileEntry "rations" 0.5 6 ] }

        it "lists the replaced id exactly ONCE — a replacement is not a \
           \second registration" $ \env →
            withProfileFixture env seededRegistry $
              \core regs regsView _ → do
                _ ← loadWithOutcome core regs regsView
                        (replacementProfile "0.5" "6")
                runProfileLua regs
                    "local t = lootListProfiles()\n\
                    \return #t .. ':' .. table.concat(t, ',')"
                    `shouldReturn` Just "1:probe_salvage"

        it "says so when it replaces, naming the file and the profile" $
          \env →
            withProfileFixture env seededRegistry $
              \core regs regsView entriesRef → do
                _ ← loadWithOutcome core regs regsView
                        (replacementProfile "0.5" "6")
                entries ← readIORef entriesRef
                let warns = [ leMessage e | e ← entries
                                          , leLevel e ≡ LevelWarn ]
                warns `shouldSatisfy` any (\m →
                    "replaces" `T.isInfixOf` m
                    ∧ "probe_salvage" `T.isInfixOf` m)

        it "stays silent about replacement for a FIRST registration" $
          \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView entriesRef → do
                _ ← loadWithOutcome core regs regsView healthyProfile
                entries ← readIORef entriesRef
                [ leMessage e | e ← entries, leLevel e ≡ LevelWarn ]
                    `shouldBe` []

    describe "loot.profile / loot.listProfiles" $ do
        it "answers the whole profile in the pinned shape, with dense \
           \1-based entries in AUTHORED order" $ \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView _ → do
                _ ← loadWithOutcome core regs regsView healthyProfile
                runProfileLua regs
                    "local p = lootProfile('probe_salvage')\n\
                    \return table.concat({ p.id,\n\
                    \  p.quantity_multiplier.min, p.quantity_multiplier.max,\n\
                    \  #p.entries,\n\
                    \  p.entries[1].item, p.entries[1].quantity_factor,\n\
                    \  p.entries[2].item, p.entries[2].quantity_factor },\n\
                    \  '|')"
                    `shouldReturn`
                        Just "probe_salvage|1|4|2|rations|2|steel_bar|3"

        it "answers each entry's chance as the authored probability" $
          \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView _ → do
                _ ← loadWithOutcome core regs regsView healthyProfile
                runProfileLua regs
                    "local p = lootProfile('probe_salvage')\n\
                    \return string.format('%.4f/%.4f',\n\
                    \  p.entries[1].chance, p.entries[2].chance)"
                    `shouldReturn` Just "0.5000/0.2500"

        it "answers nil for an unknown profile id" $ \env →
            withProfileFixture env emptyLootProfileRegistry $
              \_ regs _ _ →
                runProfileLua regs
                    "return tostring(lootProfile('no_such_profile'))"
                    `shouldReturn` Just "nil"

        it "answers nil rather than erroring when called with no id" $
          \env →
            withProfileFixture env emptyLootProfileRegistry $
              \_ regs _ _ →
                runProfileLua regs "return tostring(lootProfile())"
                    `shouldReturn` Just "nil"

        -- Read-only in the sense that actually matters to a script: the
        -- table handed back is a COPY, so a caller that mutates it has
        -- mutated nothing the next call will see.
        it "hands back a fresh table each call, so editing one cannot \
           \change the next" $ \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView _ → do
                _ ← loadWithOutcome core regs regsView healthyProfile
                runProfileLua regs
                    "local p = lootProfile('probe_salvage')\n\
                    \p.id = 'MUTATED'\n\
                    \p.quantity_multiplier.max = 99\n\
                    \p.entries[1].item = 'MUTATED'\n\
                    \p.entries[2] = nil\n\
                    \local q = lootProfile('probe_salvage')\n\
                    \return table.concat({ q.id, q.quantity_multiplier.max,\n\
                    \  #q.entries, q.entries[1].item }, '|')"
                    `shouldReturn` Just "probe_salvage|4|2|rations"

        it "lists every registered id, ascending and without repeats" $
          \env →
            withProfileFixture env emptyLootProfileRegistry $
              \core regs regsView _ → do
                -- Registered in DESCENDING order, so a listing that
                -- merely echoed insertion order would fail.
                forM_ ["zulu", "mike", "alpha"] $ \pid →
                    loadWithOutcome core regs regsView
                        (docSource pid okMult [entryOf "rations" "0.5" "2"])
                runProfileLua regs "return table.concat(lootListProfiles(), ',')"
                    `shouldReturn` Just "alpha,mike,zulu"

        it "answers an empty table when nothing is registered" $ \env →
            withProfileFixture env emptyLootProfileRegistry $
              \_ regs _ _ →
                runProfileLua regs "return tostring(#lootListProfiles())"
                    `shouldReturn` Just "0"

seededRegistry ∷ LootProfileRegistry
seededRegistry = registerLootProfile priorProbeProfile emptyLootProfileRegistry

-- | A well-formed replacement for 'priorProbeProfile', parameterised so
--   the same document can be authored valid or invalid.
replacementProfile ∷ Text → Text → Text
replacementProfile chance factor =
    docSource "probe_salvage" (multOf "2" "5")
        [entryOf "rations" chance factor]

-- | A replacement that DECODES and is then refused: @tin_can@ is not in
--   'fixtureItems'.
replacementNamingUnknownItem ∷ Text
replacementNamingUnknownItem =
    docSource "probe_salvage" (multOf "2" "5")
        [entryOf "tin_can" "0.5" "6"]

-- | A file whose SECOND entry names an item no registry holds; the
--   first entry resolves, so a rejection is about the unknown id alone.
unknownItemProfile ∷ Text
unknownItemProfile = docSource "probe_unknown" okMult
    [ entryOf "rations" "0.5" "2"
    , entryOf "no_such_item" "0.5" "3" ]
