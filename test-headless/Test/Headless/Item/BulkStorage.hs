-- | Focused coverage for physical bulk and portable-storage capacity
--   (#1233, epic #1231, @docs/portable_loot_containers.md@ D-4/D-12):
--
--   * every item definition must author a finite, strictly positive
--     top-level @bulk@ in litres — no default, never inferred;
--   * the optional @storage:@ block must author BOTH a weight and a bulk
--     capacity, on the same terms;
--   * every rejection names the offending FILE and DEFINITION, not a
--     bare JSON index;
--   * @container:@ (fluid/pill fill) and @storage:@ (nested items) are
--     independent — neither implies, defaults, or validates the other;
--   * the shipped corpus is COMPLETELY migrated: every file parses and
--     every definition authors a bulk, with the eight-row industrial
--     calibration reproduced exactly; and
--   * an instance's physical values are materialized ONCE and persist
--     independently of the definition registry (requirement 6), while a
--     pre-#1233 payload decodes with them honestly absent.
module Test.Headless.Item.BulkStorage (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import System.FilePath ((</>))
import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )
import Engine.Asset.YamlItems
    ( ItemYamlDef(..), ItemYamlFile(..), ItemYamlStorage(..)
    , ItemYamlContainer(..), loadItemYaml )
import Item.Types (ItemInstance(..), ItemStorage(..))
import World.Save.Component.Page
    ( ItemInstanceDTOv1, toItemInstanceDTO, fromItemInstanceDTO
    , toItemInstanceDTOv1, migrateItemInstanceDTOv1
    , itdBulk, itdStorage, itdContents )
import World.Save.Envelope (decodeSessionEnvelope)
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))
import World.Save.Types
    (ItemWalkOrder(..), pageItemContainers, flattenItemInstances)
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

-- | The shipped item-definition directory, exactly as boot loads it.
shippedItemDir ∷ FilePath
shippedItemDir = "data/items"

-- | The CURRENT shipped inventory, pinned deliberately (#1233
--   requirement 5 + its review's completeness clause): proving that
--   "some" definitions loaded is not proving the full 66-definition
--   inventory was loaded, so the numbers are asserted rather than derived from
--   whatever happens to be on disk.
--
--   __Adding or removing an item definition is expected to fail this
--   spec.__ That is the ratchet working: update these two numbers in the
--   same change, having confirmed the new definition authors a bulk.
shippedFileCount, shippedDefCount ∷ Int
shippedFileCount = 38
shippedDefCount  = 66

-- | @docs/portable_loot_containers.md@'s "First industrial-profile bulk
--   calibration" table, verbatim. These eight values are the calibration
--   the design authority accepted, so they are pinned exactly rather
--   than range-checked — including the pair that makes the point: a
--   compact 15 kg battery (6.00 L) authors LESS bulk per kilogram than a
--   1.2 kg steel plate (1.50 L).
calibration ∷ [(Text, Float)]
calibration =
    [ ("steel_bar",            0.75)
    , ("steel_plate",          1.50)
    , ("electric_motor",       1.25)
    , ("high_voltage_battery", 6.00)
    , ("steel_hardware",       0.15)
    , ("processing_unit",      0.40)
    , ("wiring",               0.75)
    , ("rations",              0.20)
    ]

-- | One definition's YAML with @bulk:@ replaced by whatever @b@ says
--   (a raw YAML fragment, so @.nan@ / a quoted string / a missing key
--   are all expressible). Everything else is a minimally valid def.
defWithBulk ∷ String → String
defWithBulk b = unlines
    [ "items:"
    , "  - name: \"probe_crate\""
    , "    sprite: \"assets/textures/items/probe.png\""
    , "    weight: 4.0"
    ] ⧺ b

-- | The same, with a fully valid bulk and @storage:@ set to @s@.
defWithStorage ∷ String → String
defWithStorage s = defWithBulk "    bulk: 30.0\n" ⧺ s

decodeDef ∷ String → Either String ItemYamlDef
decodeDef src = case Yaml.decodeEither' (BSC.pack src) of
    Left err               → Left (show err)
    Right (ItemYamlFile [d]) → Right d
    Right (ItemYamlFile ds)  →
        Left ("expected exactly one definition, got " ⧺ show (length ds))

-- | Did the decode fail, and does the message name the definition?
--   Both halves matter: a rejection that does not name @probe_crate@ is
--   the unusable @$.items[0].bulk@ diagnostic requirement 2 rules out.
rejectsNaming ∷ String → Expectation
rejectsNaming src = case decodeDef src of
    Right d → expectationFailure $
        "expected a rejection, but the definition parsed: " ⧺ show d
    Left err
        | "probe_crate" `elem` words (map scrub err) → pure ()
        | otherwise → expectationFailure $
            "rejected, but the message does not name the definition: " ⧺ err
  where
    -- The name appears inside quotes in the message; reduce punctuation
    -- to spaces so a plain word match is exact rather than a substring.
    scrub c = if c `elem` ("'\"(),:;" ∷ String) then ' ' else c

spec ∷ Spec
spec = do
    describe "top-level bulk (#1233 requirements 1+2)" $ do
        it "accepts a finite, strictly positive bulk" $
            (iydBulk <$> decodeDef (defWithBulk "    bulk: 30.0\n"))
                `shouldBe` Right 30.0

        it "accepts an integral spelling" $
            (iydBulk <$> decodeDef (defWithBulk "    bulk: 30\n"))
                `shouldBe` Right 30.0

        it "accepts an arbitrarily small positive bulk (the boundary is \
           \zero, and it is exclusive)" $
            (iydBulk <$> decodeDef (defWithBulk "    bulk: 0.001\n"))
                `shouldBe` Right 0.001

        it "rejects a MISSING bulk — there is no default" $
            rejectsNaming (defWithBulk "")

        it "rejects an explicitly null bulk" $
            rejectsNaming (defWithBulk "    bulk: null\n")

        it "rejects zero" $ rejectsNaming (defWithBulk "    bulk: 0\n")

        it "rejects zero written as a decimal" $
            rejectsNaming (defWithBulk "    bulk: 0.0\n")

        it "rejects a negative bulk" $
            rejectsNaming (defWithBulk "    bulk: -1.5\n")

        it "rejects NaN" $ rejectsNaming (defWithBulk "    bulk: .nan\n")

        it "rejects positive infinity" $
            rejectsNaming (defWithBulk "    bulk: .inf\n")

        it "rejects negative infinity" $
            rejectsNaming (defWithBulk "    bulk: -.inf\n")

        it "rejects a value that is finite in YAML but INFINITE once \
           \narrowed to the engine's 32-bit Float" $
            -- The trap a bare positivity test misses: 1.0e+100 is an
            -- unremarkable Scientific, and only becomes Infinity in the
            -- Float field it lands in.
            rejectsNaming (defWithBulk "    bulk: 1.0e+100\n")

        it "rejects a non-numeric bulk rather than coercing it" $
            rejectsNaming (defWithBulk "    bulk: \"30\"\n")

        it "rejects a boolean bulk" $
            rejectsNaming (defWithBulk "    bulk: true\n")

    describe "storage: capacities (#1233 requirement 3)" $ do
        let good = "    storage:\n\
                   \      weight_capacity: 30.0\n\
                   \      bulk_capacity: 23.0\n"

        it "accepts both capacities, independent of each other and of \
           \the item's own external bulk" $
            case decodeDef (defWithStorage good) of
                Left err → expectationFailure err
                Right d  → do
                    iydBulk d `shouldBe` 30.0
                    iydStorage d `shouldBe`
                        Just (ItemYamlStorage 30.0 23.0)

        it "a definition with no storage: block has none — the block is \
           \optional, not defaulted" $
            (iydStorage <$> decodeDef (defWithStorage ""))
                `shouldBe` Right Nothing

        -- An AUTHORED-but-null block is the one case `.:?` gets wrong:
        -- it reports it as absent, which would accept a definition that
        -- visibly declared storage as though it never mentioned it. A key
        -- the author wrote is present, so a null value is a half-authored
        -- block and fails like any other invalid one. All three YAML
        -- spellings of null are covered, since the rejection is on the
        -- decoded Null rather than on the source text.
        it "rejects an explicitly null storage: block rather than reading \
           \it as absent" $
            rejectsNaming (defWithStorage "    storage: null\n")

        it "rejects a storage: key written with no value at all" $
            rejectsNaming (defWithStorage "    storage:\n")

        it "rejects a tilde-null storage: block" $
            rejectsNaming (defWithStorage "    storage: ~\n")

        it "rejects a non-object storage: value" $
            rejectsNaming (defWithStorage "    storage: 23.0\n")

        it "rejects a storage: block missing weight_capacity" $
            rejectsNaming (defWithStorage
                "    storage:\n      bulk_capacity: 23.0\n")

        it "rejects a storage: block missing bulk_capacity" $
            rejectsNaming (defWithStorage
                "    storage:\n      weight_capacity: 30.0\n")

        it "rejects an empty storage: block" $
            rejectsNaming (defWithStorage "    storage: {}\n")

        it "rejects a zero weight capacity" $
            rejectsNaming (defWithStorage
                "    storage:\n      weight_capacity: 0\n\
                \      bulk_capacity: 23.0\n")

        it "rejects a zero bulk capacity" $
            rejectsNaming (defWithStorage
                "    storage:\n      weight_capacity: 30.0\n\
                \      bulk_capacity: 0\n")

        it "rejects a negative weight capacity" $
            rejectsNaming (defWithStorage
                "    storage:\n      weight_capacity: -30.0\n\
                \      bulk_capacity: 23.0\n")

        it "rejects a negative bulk capacity" $
            rejectsNaming (defWithStorage
                "    storage:\n      weight_capacity: 30.0\n\
                \      bulk_capacity: -23.0\n")

        it "rejects a NaN weight capacity" $
            rejectsNaming (defWithStorage
                "    storage:\n      weight_capacity: .nan\n\
                \      bulk_capacity: 23.0\n")

        it "rejects an infinite bulk capacity" $
            rejectsNaming (defWithStorage
                "    storage:\n      weight_capacity: 30.0\n\
                \      bulk_capacity: .inf\n")

        it "rejects a bulk capacity that narrows to infinity" $
            rejectsNaming (defWithStorage
                "    storage:\n      weight_capacity: 30.0\n\
                \      bulk_capacity: 1.0e+100\n")

    describe "container: and storage: stay separate (D-12, requirement 4)" $ do
        let container = "    container:\n\
                        \      capacity: 2.0\n\
                        \      holds: \"water\"\n\
                        \      fill_weight: 1.0\n"
            storage = "    storage:\n\
                      \      weight_capacity: 30.0\n\
                      \      bulk_capacity: 23.0\n"

        it "a fillable container gains NO storage capacity from its fill \
           \capacity" $
            case decodeDef (defWithStorage container) of
                Left err → expectationFailure err
                Right d  → do
                    iycCapacity <$> iydContainer d `shouldBe` Just 2.0
                    iydStorage d `shouldBe` Nothing

        it "an item-storage container gains NO fill capacity from its \
           \storage capacities" $
            case decodeDef (defWithStorage storage) of
                Left err → expectationFailure err
                Right d  → do
                    iydContainer d `shouldBe` Nothing
                    iydStorage d `shouldBe`
                        Just (ItemYamlStorage 30.0 23.0)

        it "a definition may carry BOTH, each keeping its own values" $
            case decodeDef (defWithStorage (container ⧺ storage)) of
                Left err → expectationFailure err
                Right d  → do
                    iycCapacity <$> iydContainer d `shouldBe` Just 2.0
                    iydStorage d `shouldBe`
                        Just (ItemYamlStorage 30.0 23.0)

        it "a present container: does NOT satisfy storage:'s own \
           \validation — a half-authored storage block still fails" $
            rejectsNaming (defWithStorage
                (container ⧺ "    storage:\n      bulk_capacity: 23.0\n"))

    describe "rejection diagnostics name the file AND the definition" $ do
        -- The two halves come from different places: loadYamlList
        -- supplies the path, the parser supplies the name. Only the
        -- composed warning proves an author can actually find the
        -- offending line, so it is asserted through the real loader.
        it "an invalid bulk warns with both the YAML path and the \
           \definition name" $
            withTempItemYaml (defWithBulk "    bulk: 0\n") $ \path → do
                (logger, entriesRef) ← callbackLogger
                defs ← loadItemYaml logger path
                defs `shouldBe` []
                msg ← soleWarning entriesRef
                msg `shouldSatisfy` T.isInfixOf (T.pack path)
                msg `shouldSatisfy` T.isInfixOf "probe_crate"
                msg `shouldSatisfy` T.isInfixOf "bulk"

        it "an invalid storage capacity warns with both, naming the \
           \capacity that was wrong" $
            withTempItemYaml (defWithStorage
                "    storage:\n      weight_capacity: -1\n\
                \      bulk_capacity: 23.0\n") $ \path → do
                (logger, entriesRef) ← callbackLogger
                defs ← loadItemYaml logger path
                defs `shouldBe` []
                msg ← soleWarning entriesRef
                msg `shouldSatisfy` T.isInfixOf (T.pack path)
                msg `shouldSatisfy` T.isInfixOf "probe_crate"
                msg `shouldSatisfy` T.isInfixOf "storage weight capacity"

        it "a valid definition loads with no warning at all" $
            withTempItemYaml (defWithStorage
                "    storage:\n      weight_capacity: 30.0\n\
                \      bulk_capacity: 23.0\n") $ \path → do
                (logger, entriesRef) ← callbackLogger
                defs ← loadItemYaml logger path
                map iydName defs `shouldBe` ["probe_crate"]
                entries ← readIORef entriesRef
                map leMessage (filter ((≡ LevelWarn) ∘ leLevel) entries)
                    `shouldBe` []

    describe "the shipped corpus is completely migrated (requirement 5)" $ do
        it "every shipped item YAML parses, and the inventory is exactly \
           \the expected file and definition count" $ do
            files ← shippedItemFiles
            length files `shouldBe` shippedFileCount
            results ← mapM (\f → (,) f <$> Yaml.decodeFileEither f) files
            let failures = [ (f, show e) | (f, Left e) ← results ]
            failures `shouldBe` []
            let defs = concat [ iyfItems d | (_, Right d) ← results ]
                names = map iydName defs
            length defs `shouldBe` shippedDefCount
            sort names `shouldBe` sort (dedup names)

        it "every shipped definition authors a finite, strictly positive \
           \bulk" $ do
            defs ← shippedItemDefs
            [ (iydName d, iydBulk d)
              | d ← defs
              , isNaN (iydBulk d) ∨ isInfinite (iydBulk d) ∨ iydBulk d ≤ 0 ]
                `shouldBe` []

        it "no shipped definition contributes a file that parses to zero \
           \definitions" $ do
            files ← shippedItemFiles
            empties ← concat <$> mapM (\f → do
                r ← Yaml.decodeFileEither f
                pure [ f | Right (ItemYamlFile []) ← [r] ]) files
            empties `shouldBe` []

        it "reproduces the design authority's eight-row industrial bulk \
           \calibration exactly" $ do
            defs ← shippedItemDefs
            let authored n = iydBulk <$> lookupDef n defs
            [ (n, authored n) | (n, _) ← calibration ]
                `shouldBe` [ (n, Just b) | (n, b) ← calibration ]

    describe "instance physical values are materialized once (requirement 6)" $ do
        it "an instance carries its OWN bulk and capacities, so a later \
           \definition edit cannot re-value it" $ do
            -- The instance never references a def for these: the values
            -- ARE its own fields. Encode/decode through the real
            -- component codec to prove the registry is not consulted
            -- anywhere on the wire either.
            let inst = mintedCrate
            roundTrip inst `shouldBe` Right inst
            iiBulk inst `shouldBe` Just 30.0
            iiStorage inst `shouldBe` Just (ItemStorage 30.0 23.0)

        it "round-trips a recursive tree whose three levels have three \
           \DIFFERENT physical shapes" $ do
            let inst = mintedCrate
                    { iiContents =
                        [ (mintedCrate { iiInstanceId = 2 })
                            { iiBulk = Just 0.75, iiStorage = Nothing
                            , iiContents =
                                [ (mintedCrate { iiInstanceId = 3 })
                                    { iiBulk = Nothing, iiStorage = Nothing
                                    , iiContents = [] } ] } ] }
            roundTrip inst `shouldBe` Right inst

        it "a pre-#1233 payload decodes with both values ABSENT, all the \
           \way down its contents tree" $ do
            -- Never a fabricated zero (an invalid bulk the loader would
            -- refuse) and never re-derived from today's definition
            -- (which is the retroactive re-valuation requirement 6
            -- forbids). See migrateItemInstanceDTOv1.
            let nested = mintedCrate
                    { iiContents = [ mintedCrate { iiInstanceId = 2 } ] }
            case S.decode (S.encode (toItemInstanceDTOv1 nested)) of
                Left err → expectationFailure err
                Right (v1 ∷ ItemInstanceDTOv1) → do
                    let cur = migrateItemInstanceDTOv1 v1
                    itdBulk cur `shouldBe` Nothing
                    itdStorage cur `shouldBe` Nothing
                    map itdBulk (itdContents cur) `shouldBe` [Nothing]
                    map itdStorage (itdContents cur) `shouldBe` [Nothing]

        it "everything else in a pre-#1233 payload survives the \
           \migration untouched" $ do
            case S.decode (S.encode (toItemInstanceDTOv1 mintedCrate)) of
                Left err → expectationFailure err
                Right (v1 ∷ ItemInstanceDTOv1) → do
                    let back = fromItemInstanceDTO
                                   (migrateItemInstanceDTOv1 v1)
                    back `shouldBe`
                        mintedCrate { iiBulk = Nothing, iiStorage = Nothing }

        it "a REAL save written by a REAL engine carries a materialized \
           \bulk on every one of its item instances" $ do
            -- The end-to-end half of requirement 6, and the one thing a
            -- pure test cannot reach: the eight production mint paths
            -- really do snapshot the definition's bulk. This fixture's
            -- items came from a unit's starting kit, `unit.addItem`,
            -- `unit.depositToCargo` into a building, and
            -- `item.spawnGround` — four different mint sites, three
            -- different item containers.
            bytes ← BS.readFile m1FixturePath
            let luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames bytes of
                Left err → expectationFailure (T.unpack err)
                Right (_, snap, _, _) → do
                    let items =
                            [ i
                            | page       ← HM.elems (snapPages snap)
                            , (_, insts) ← pageItemContainers ItemsGroundFirst
                                               pgsGroundItems pgsUnits
                                               pgsBuildings page
                            , inst ← insts
                            , i    ← flattenItemInstances inst ]
                    -- Non-vacuous: a fixture that happened to carry no
                    -- items would satisfy an all-quantified check.
                    length items `shouldSatisfy` (≥ 3)
                    [ (iiDefName i, iiBulk i)
                      | i ← items, not (isPositiveFinite (iiBulk i)) ]
                        `shouldBe` []
                    -- And the values are the DEFINITIONS' authored ones,
                    -- not some uniform placeholder.
                    lookup "rations" [ (iiDefName i, iiBulk i) | i ← items ]
                        `shouldBe` Just (Just 0.20)

-- | The #1233 baseline fixture: a real save written by a real engine
--   through the real production path, whose items were all minted by
--   real production mint paths (a unit's starting kit and `unit.addItem`,
--   a `depositToCargo` into a building, a `item.spawnGround` drop).
--
--   Reading it back is the only assertion here that covers what a pure
--   test structurally cannot: that those mint paths actually SNAPSHOT the
--   definition's bulk. A test that constructs its own instance can only
--   restate its own assignment.
m1FixturePath ∷ FilePath
m1FixturePath = "test-headless/data/save-compat/m1-item-bulk-storage.bin"

-- | An instance as the eight production mint paths build one: the
--   physical values snapshotted from a crate-shaped definition
--   (@bulk: 30@, @storage: {weight_capacity: 30, bulk_capacity: 23}@ —
--   the design authority's wooden-crate calibration).
mintedCrate ∷ ItemInstance
mintedCrate = ItemInstance
    { iiDefName = "probe_crate", iiCurrentFill = 0, iiQuality = 82
    , iiCondition = 74.5, iiWeight = 4.0, iiSharpness = 0
    , iiContents = [], iiInstanceId = 1, iiTemp = Just 21.5
    , iiBulk = Just 30.0, iiStorage = Just (ItemStorage 30.0 23.0)
    }

-- | Through the REAL component codec: live record → frozen DTO → cereal
--   bytes → frozen DTO → live record.
roundTrip ∷ ItemInstance → Either String ItemInstance
roundTrip i =
    fromItemInstanceDTO <$> S.decode (S.encode (toItemInstanceDTO i))

-- | Is this an authored bulk at all: present, finite, strictly positive
--   — the same three conditions the loader enforces.
isPositiveFinite ∷ Maybe Float → Bool
isPositiveFinite Nothing  = False
isPositiveFinite (Just v) = not (isNaN v) ∧ not (isInfinite v) ∧ v > 0

dedup ∷ [Text] → [Text]
dedup = go []
  where
    go acc []       = reverse acc
    go acc (x : xs) = if x `elem` acc then go acc xs else go (x : acc) xs

lookupDef ∷ Text → [ItemYamlDef] → Maybe ItemYamlDef
lookupDef n ds = case filter ((≡ n) ∘ iydName) ds of
    (d : _) → Just d
    []      → Nothing

-- | Every @*.yaml@ under @data/items@ AT ANY DEPTH, discovered through
--   the same walk startup discovery uses (#1232) rather than listed, so
--   a file added — including one added in a subdirectory — and never
--   registered still shows up here.
shippedItemFiles ∷ IO [FilePath]
shippedItemFiles = do
    rels ← walkFilesWithExtension shippedItemDir ".yaml"
    pure (sort [ shippedItemDir </> rel | rel ← rels ])

shippedItemDefs ∷ IO [ItemYamlDef]
shippedItemDefs = do
    files ← shippedItemFiles
    concat <$> mapM (\f → do
        r ← Yaml.decodeFileEither f
        case r of
            Left err → fail (f ⧺ ": " ⧺ show err)
            Right d  → pure (iyfItems d)) files

-- | The single captured warning, or a failure naming what was captured
--   instead — a loader that emitted two warnings, or none, is not the
--   behaviour under test.
soleWarning ∷ IORef [LogEntry] → IO Text
soleWarning entriesRef = do
    entries ← readIORef entriesRef
    case filter ((≡ LevelWarn) ∘ leLevel) entries of
        [e] → do
            leCategory e `shouldBe` CatAsset
            pure (leMessage e)
        other → do
            expectationFailure $
                "expected exactly one captured warning, got "
                ⧺ show (map leMessage other)
            pure T.empty

-- | A logger whose backend records every emitted entry (same shape as
--   "Test.Headless.Asset.YamlList"'s).
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :))
        , lcDebugCategories = [CatAsset]
        }
    pure (logger, entriesRef)

withTempItemYaml ∷ String → (FilePath → IO α) → IO α
withTempItemYaml contents action =
    withExclusiveTempDirectory "synarchy-1233-item-bulk" $ \dir → do
        let path = dir </> "probe_items.yaml"
        writeFile path contents
        action path
