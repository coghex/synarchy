-- | "Location spatial bounds" (#777): the authoritative footprint every
--   location definition declares — YAML parsing/rejection, the pure
--   translate/contain/intersect/distance operations later
--   location work (#778/#779/#780/#1230) shares, cylindrical-seam behavior,
--   and the shipped ruin_small's exact 5x5 contract.
module Test.Headless.Location.Bounds
    ( spec
    , decodeDef
    , rejectedNaming
    , rejectedNamingFields
    , isRight'
    ) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.Yaml as Yaml
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Data.List (sort)
import Data.Foldable (toList)
import qualified Data.Aeson.KeyMap as KeyMap
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )
import Engine.Asset.YamlLocations
    ( LocationYamlBounds(..), LocationYamlContent(..), LocationYamlCountRange(..)
    , LocationYamlDef(..), LocationYamlFile(..), authoredLocationCoordinateLimit
    , loadLocationYaml, significantItemErrors )
import Location.Bounds
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

decodeBounds ∷ BS.ByteString → Either String LocationYamlBounds
decodeBounds = either (Left . show) Right . Yaml.decodeEither'

decodeDef ∷ BS.ByteString → Either String LocationYamlDef
decodeDef = either (Left . show) Right . Yaml.decodeEither'

isRight' ∷ Either a b → Bool
isRight' = either (const False) (const True)

-- | True iff decoding failed AND the error names the given location id
--   (the #777 "identifies the location definition" requirement) — a
--   bare failure check can't catch a regression back to aeson's own
--   id-less key-not-found/type-mismatch error text.
rejectedNaming ∷ Text → Either String a → Bool
rejectedNaming lid = either (T.isInfixOf ("location '" <> lid <> "'") . T.pack)
                            (const False)

-- | 'rejectedNaming' plus #777's OTHER half: the message must also name
--   the offending FIELD. The id alone cannot distinguish "min_x > max_x"
--   from any other rejection this def could earn, so an axis case that
--   checked only the id would still pass if the two axes' messages were
--   swapped or collapsed into one generic "bad bounds" string (#1151).
rejectedNamingFields ∷ Text → [Text] → Either String a → Bool
rejectedNamingFields lid fields =
    either (\err → let msg = T.pack err
                   in T.isInfixOf ("location '" <> lid <> "'") msg
                      ∧ all (`T.isInfixOf` msg) fields)
           (const False)

decodeFile ∷ BS.ByteString → Either String LocationYamlFile
decodeFile = either (Left . show) Right . Yaml.decodeEither'

-- | One decoded definition as a single-element list, for the checks
--   that take the whole file's defs. Fails the example loudly rather
--   than silently reporting no errors over an empty list — which is
--   what a fixture that stopped parsing would otherwise look like.
decodedDefs ∷ BS.ByteString → [LocationYamlDef]
decodedDefs raw = case decodeDef raw of
    Right d  → [d]
    Left err → error ("Bounds fixture failed to decode: " <> err)

-- | Every item def name the shipped @data/items@ tree registers, read
--   the way the engine reads it: one file at a time, off disk. Used to
--   prove the shipped ruin's guaranteed item is a real item rather
--   than a name that merely looks plausible.
shippedItemNames ∷ IO (HS.HashSet Text)
shippedItemNames = do
    files ← listDirectory "data/items"
    fmap (HS.fromList . concat) $ forM (sort files) $ \f → do
        raw ← Yaml.decodeFileEither ("data/items" </> f)
        case raw ∷ Either Yaml.ParseException Yaml.Value of
            Left err → error ("data/items/" <> f <> ": " <> show err)
            Right v  → pure (itemNamesOf v)
  where
    itemNamesOf v = case v of
        Yaml.Object o → case KeyMap.lookup "items" o of
            Just (Yaml.Array xs) →
                [ n | Yaml.Object e ← toList xs
                    , Just (Yaml.String n) ← [KeyMap.lookup "name" e] ]
            _ → []
        _ → []

spec ∷ Spec
spec = describe "Location spatial bounds" $ do

    describe "YAML bounds parsing (#777)" $ do
        it "parses a valid bounds block" $
            decodeBounds "{ min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
                `shouldBe` Right (LocationYamlBounds (-2) (-2) 2 2)

        it "rejects a definition missing bounds entirely, naming the location" $
            decodeDef
                "{ id: t, builder: b,\
                \  naming: { heads: [KEEP], modifiers: [ASH] } }"
                `shouldSatisfy` rejectedNaming "t"

        it "rejects a definition missing the naming block entirely (#1101), \
           \naming the location" $
            decodeDef
                "{ id: t, builder: b,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } }"
                `shouldSatisfy` rejectedNaming "t"

        it "rejects a naming block missing one of its two pools (#1101)" $
            decodeDef
                "{ id: t, builder: b,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  naming: { heads: [KEEP] } }"
                `shouldSatisfy` rejectedNaming "t"

        it "rejects an EMPTY naming pool (#1101) -- present but empty is \
           \authored data silently meaning 'fall back to the label', which \
           \is what an absent language means and must not be forgeable" $ do
            decodeDef
                "{ id: t, builder: b,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  naming: { heads: [], modifiers: [ASH] } }"
                `shouldSatisfy` rejectedNaming "t"
            decodeDef
                "{ id: t, builder: b,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  naming: { heads: [KEEP], modifiers: [] } }"
                `shouldSatisfy` rejectedNaming "t"

        it "no longer requires discovery_margin (#1230) — a definition \
           \declaring only bounds and naming is complete" $
            -- The inversion of the removed "rejects a definition missing
            -- discovery_margin" case: what used to be a required field
            -- is now absent from the contract entirely, and an authored
            -- file that omits it must load rather than fail.
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } }"
                `shouldSatisfy` isRight'

        it "IGNORES a leftover discovery_margin (#1230) rather than \
           \rejecting the file — an unmigrated authored def still loads" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  discovery_margin: 6,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } }"
                `shouldSatisfy` isRight'

        it "rejects a malformed bounds block, naming the location" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: nope, min_y: -2, max_x: 2, max_y: 2 } }"
                `shouldSatisfy` rejectedNaming "t"

        it "rejects inverted bounds (min_x > max_x), naming the location \
           \AND the x-axis fields" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: 5, min_y: -2, max_x: 2, max_y: 2 } }"
                `shouldSatisfy`
                    rejectedNamingFields "t" ["bounds.min_x", "bounds.max_x"]

        it "rejects inverted bounds (min_y > max_y), naming the location \
           \AND the y-axis fields" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: 5, max_x: 2, max_y: 2 } }"
                `shouldSatisfy`
                    rejectedNamingFields "t" ["bounds.min_y", "bounds.max_y"]

        it "ACCEPTS a degenerate single-tile box (min_x == max_x, \
           \min_y == max_y) -- the rule is min <= max, and this is the \
           \case that fails the moment it is tightened to strict <" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: 0, min_y: 0, max_x: 0, max_y: 0 } }"
                `shouldSatisfy` isRight'

        it "ACCEPTS a box degenerate on ONE axis only" $ do
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: 3, max_x: 2, max_y: 3 } }"
                `shouldSatisfy` isRight'
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: 3, min_y: -2, max_x: 3, max_y: 2 } }"
                `shouldSatisfy` isRight'

        it "one inverted definition fails the WHOLE file's load (#777) -- \
           \the surviving defs are not returned without it" $ do
            let goodOnly = "{ locations: [\
                    \ { id: ok, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } } ] }"
                withBad = "{ locations: [\
                    \ { id: ok, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } },\
                    \ { id: bad, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: 5, min_y: -2, max_x: 2, max_y: 2 } } ] }"
            -- The control: the same file WITHOUT the inverted def decodes,
            -- so the failure below is the inverted bounds and not the
            -- fixture's own shape.
            fmap (map lydId . lyfLocations) (decodeFile goodOnly)
                `shouldBe` Right ["ok"]
            decodeFile withBad
                `shouldSatisfy`
                    rejectedNamingFields "bad" ["bounds.min_x", "bounds.max_x"]

        it "rejects a fixed content position outside the declared bounds, naming the location" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: item, id: x, position: {x: 5, y: 0} } ] }"
                `shouldSatisfy` rejectedNaming "t"

        it "accepts a fixed content position on the bounds edge (inclusive)" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: item, id: x, position: {x: 2, y: -2} } ] }"
                `shouldSatisfy` isRight'

        -- #1708: the content-kind vocabulary is CLOSED at this same
        -- entry point. Both rejections assert the id AND the offending
        -- token, because a message carrying only one of them cannot
        -- tell an author which entry of which definition to fix.
        it "rejects the removed nested 'structure' content kind (#1708), \
           \naming the location and the offending kind" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: structure, id: room_small, position: {x: 2, y: 0} } ] }"
                `shouldSatisfy` rejectedNamingFields "t" ["'structure'"]

        it "rejects an unrecognized content kind (#1708), naming the \
           \location and the offending kind" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: not_a_real_kind, id: x } ] }"
                `shouldSatisfy` rejectedNamingFields "t" ["'not_a_real_kind'"]

        it "accepts every kind in the closed content vocabulary (#1708)" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: unit, id: u, count: 2, faction: hostile },\
                \              { kind: item, id: i, position: {x: 1, y: 1} },\
                \              { kind: loot_table, id: l, rolls: 3 },\
                \              { kind: building, id: g } ] }"
                `shouldSatisfy` isRight'

        it "one bad content kind fails the WHOLE file's load (#1708) -- \
           \the surviving defs are not returned without it" $ do
            let goodOnly = "{ locations: [\
                    \ { id: ok, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \   contents: [ { kind: loot_table, id: l } ] } ] }"
                withBad = "{ locations: [\
                    \ { id: ok, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \   contents: [ { kind: loot_table, id: l } ] },\
                    \ { id: bad, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \   contents: [ { kind: structure, id: room_small } ] } ] }"
            fmap (map lydId . lyfLocations) (decodeFile goodOnly)
                `shouldBe` Right ["ok"]
            decodeFile withBad
                `shouldSatisfy` rejectedNamingFields "bad" ["'structure'"]

        -- #1721: `count` and `rolls` are per-entry MULTIPLICITIES with a
        -- positive domain, closed at this same entry point. A
        -- non-positive value used to load cleanly, spawn nothing (the
        -- Lua spawn loops run zero iterations), log nothing, and then be
        -- recorded as the location's permanent exactly-once content
        -- lifecycle -- invisible at every layer.
        it "rejects a zero `count`, naming the location, the entry and \
           \the offending value" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: unit, id: raider, count: 0 } ] }"
                `shouldSatisfy`
                    rejectedNamingFields "t"
                        ["content entry 1", "'raider'", "'count'", "0"]

        it "rejects a negative `count`" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: unit, id: raider, count: -1 } ] }"
                `shouldSatisfy`
                    rejectedNamingFields "t"
                        ["content entry 1", "'raider'", "'count'", "-1"]

        it "rejects a zero `rolls`" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: loot_table, id: ruin_common, rolls: 0 } ] }"
                `shouldSatisfy`
                    rejectedNamingFields "t"
                        ["content entry 1", "'ruin_common'", "'rolls'", "0"]

        it "rejects a negative `rolls`" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: loot_table, id: ruin_common, rolls: -4 } ] }"
                `shouldSatisfy`
                    rejectedNamingFields "t"
                        ["content entry 1", "'ruin_common'", "'rolls'", "-4"]

        -- The index must be the 1-based POSITION in `contents`, the same
        -- index `ipairs`/`rollCtx.index` and the Lua-facing `zip [1..]`
        -- already use. A fixture whose only bad entry is the first
        -- cannot tell a 1-based index from a 0-based one, or from a
        -- hard-coded constant.
        it "names the 1-based position of a bad entry that is NOT the \
           \first in the list" $ do
            let bad = decodeDef
                    "{ id: t, builder: b,\
                    \  naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  contents: [ { kind: unit, id: raider, count: 2 },\
                    \              { kind: item, id: canteen },\
                    \              { kind: loot_table, id: ruin_common,\
                    \                rolls: -7 } ] }"
            bad `shouldSatisfy`
                rejectedNamingFields "t"
                    ["content entry 3", "'ruin_common'", "'rolls'", "-7"]
            -- ...and NOT the position it would carry under a 0-based or
            -- constant index.
            bad `shouldNotSatisfy` rejectedNamingFields "t" ["content entry 2"]

        -- #917: `significant` marks a GUARANTEED item the owning
        -- location's clearance predicate waits on. It is legal on a
        -- fixed `kind: item` entry and on NOTHING else — a loot-table
        -- draw carrying it would make what a location owes depend on
        -- what it rolled, and a unit or building could never be picked
        -- up to discharge the obligation. Rejected HERE rather than at
        -- spawn time, where warning and skipping would still burn the
        -- location's exactly-once content lifecycle and leave it
        -- permanently unclearable.
        it "rejects `significant` on a loot_table entry" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: loot_table, id: ruin_common,\
                \                significant: true } ] }"
                `shouldSatisfy`
                    rejectedNamingFields "t"
                        [ "content entry 1", "'ruin_common'", "'significant'"
                        , "only for item content", "'loot_table'" ]

        it "rejects `significant` on a unit entry, naming its position" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: item, id: canteen },\
                \              { kind: unit, id: raider, significant: true } ] }"
                `shouldSatisfy`
                    rejectedNamingFields "t"
                        [ "content entry 2", "'raider'", "'significant'"
                        , "'unit'" ]

        it "rejects `significant` on a building entry" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: building, id: shed,\
                \                significant: true } ] }"
                `shouldSatisfy`
                    rejectedNamingFields "t"
                        ["content entry 1", "'significant'", "'building'"]

        it "accepts `significant` on an item entry and defaults it to \
           \false everywhere else -- an entry is incidental unless it \
           \says otherwise" $
            fmap (map (\c → (lycId c, lycSignificant c)) . lydContents)
                (decodeDef
                    "{ id: t, builder: b,\
                    \  naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  contents: [ { kind: loot_table, id: ruin_common },\
                    \              { kind: item, id: canteen },\
                    \              { kind: item, id: processing_unit,\
                    \                significant: true },\
                    \              { kind: item, id: radio,\
                    \                significant: false } ] }")
                `shouldBe` Right [ ("ruin_common", False)
                                 , ("canteen", False)
                                 , ("processing_unit", True)
                                 , ("radio", False) ]

        it "accepts positive multiplicities and retains the authored \
           \values exactly" $
            fmap (map (\c → (lycId c, lycCount c, lycRolls c)) . lydContents)
                (decodeDef
                    "{ id: t, builder: b,\
                    \  naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  contents: [ { kind: unit, id: raider, count: 3 },\
                    \              { kind: loot_table, id: ruin_common,\
                    \                rolls: 2 },\
                    \              { kind: item, id: canteen, count: 1,\
                    \                rolls: 1 } ] }")
                `shouldBe` Right [ ("raider", 3, 1)
                                 , ("ruin_common", 1, 2)
                                 , ("canteen", 1, 1) ]

        -- An explicit YAML `null` reads as absence through aeson's `.:?`
        -- and must keep doing so: this rejection constrains the numeric
        -- domain, it does not change what counts as omitted.
        it "defaults an omitted -- or explicitly null -- `count` and \
           \`rolls` to 1" $
            fmap (map (\c → (lycId c, lycCount c, lycRolls c)) . lydContents)
                (decodeDef
                    "{ id: t, builder: b,\
                    \  naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  contents: [ { kind: item, id: canteen },\
                    \              { kind: unit, id: raider, count: null,\
                    \                rolls: null } ] }")
                `shouldBe` Right [ ("canteen", 1, 1), ("raider", 1, 1) ]

        it "parses a unit count_range whose zero lower bound is a real \
           \encounter outcome (#916)" $
            fmap (map (\c → (lycId c, lycCountRange c, lycClearance c))
                    . lydContents)
                (decodeDef
                    "{ id: t, builder: b,\
                    \  naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  contents: [ { kind: unit, id: nomad_primitive,\
                    \                count_range: { min: 0, max: 3 },\
                    \                clearance: death_only } ] }")
                `shouldBe` Right
                    [("nomad_primitive", Just (LocationYamlCountRange 0 3),
                      Just "death_only")]

        it "rejects count_range on non-unit content, a negative minimum, \
           \an inverted or over-capacity range, and a second encounter" $ do
            let wrap entries = BS.pack
                    ("{ id: t, builder: b,"
                    <> " naming: { heads: [KEEP], modifiers: [ASH] },"
                    <> " bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },"
                    <> " contents: [" <> entries <> "] }")
            decodeDef (wrap
                "{ kind: item, id: x, count_range: { min: 0, max: 3 } }")
                `shouldSatisfy` rejectedNamingFields "t"
                    ["content entry 1", "count_range", "only for unit"]
            decodeDef (wrap
                "{ kind: unit, id: x, count_range: { min: -1, max: 3 } }")
                `shouldSatisfy` rejectedNamingFields "t"
                    ["count_range.min", "non-negative", "-1"]
            decodeDef (wrap
                "{ kind: unit, id: x, count_range: { min: 3, max: 2 } }")
                `shouldSatisfy` rejectedNamingFields "t"
                    ["count_range.max", "below min"]
            decodeDef (wrap
                "{ kind: unit, id: x, count_range: { min: 0, max: 26 } }")
                `shouldSatisfy` rejectedNamingFields "t"
                    ["count_range.max", "26", "25 distinct tiles"]
            decodeDef (wrap
                "{ kind: unit, id: x, count_range: { min: 0, max: 3 } }")
                `shouldSatisfy` rejectedNamingFields "t"
                    ["count_range", "explicit", "clearance"]
            decodeDef (wrap
                ("{ kind: unit, id: x, count_range: { min: 0, max: 3 },"
                <> " clearance: flee_only }"))
                `shouldSatisfy` rejectedNamingFields "t"
                    ["unsupported", "flee_only", "death_only"]
            decodeDef (wrap
                "{ kind: unit, id: x, clearance: death_only }")
                `shouldSatisfy` rejectedNamingFields "t"
                    ["clearance", "only with", "count_range"]
            decodeDef (wrap
                ("{ kind: unit, id: x, count_range: { min: 0, max: 1 },"
                <> " clearance: death_only },"
                <> "{ kind: unit, id: 'y', count_range: { min: 0, max: 1 },"
                <> " clearance: death_only }"))
                `shouldSatisfy` rejectedNamingFields "t"
                    ["at most one", "count_range"]

        it "one non-positive multiplicity fails the WHOLE file's load \
           \(#1721) -- the surviving defs are not returned without it" $ do
            let goodOnly = "{ locations: [\
                    \ { id: ok, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \   contents: [ { kind: loot_table, id: l, rolls: 2 } ] } ] }"
                withBad = "{ locations: [\
                    \ { id: ok, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \   contents: [ { kind: loot_table, id: l, rolls: 2 } ] },\
                    \ { id: bad, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \   contents: [ { kind: unit, id: raider, count: 0 } ] } ] }"
            fmap (map lydId . lyfLocations) (decodeFile goodOnly)
                `shouldBe` Right ["ok"]
            decodeFile withBad
                `shouldSatisfy`
                    rejectedNamingFields "bad"
                        ["content entry 1", "'raider'", "'count'", "0"]

        it "the shipped ruin_small.yaml declares the exact 5x5 contract" $ do
            result ← Yaml.decodeFileEither "data/locations/ruin_small.yaml"
            case result of
                Left err → expectationFailure (show (err ∷ Yaml.ParseException))
                Right lf → case lyfLocations lf of
                    [def] → do
                        lydBounds def `shouldBe` LocationYamlBounds (-2) (-2) 2 2
                        [ (lycId c, lycCountRange c, lycClearance c)
                          | c ← lydContents def, isJust (lycCountRange c) ]
                            `shouldBe`
                                [("nomad_primitive",
                                  Just (LocationYamlCountRange 0 3),
                                  Just "death_only")]
                        -- #917 requirement 6: the shipped ruin authors
                        -- EXACTLY ONE guaranteed significant item, so a
                        -- zero-nomad ruin stays uncleared until it is
                        -- taken. Pinned by def name, because the reward
                        -- must stay distinct from `radio` (D-6).
                        [ lycId c | c ← lydContents def, lycSignificant c ]
                            `shouldBe` ["processing_unit"]
                        -- …and the two incidental `ruin_common` rolls
                        -- keep authored index 1 (#948 keys each draw on
                        -- the entry's POSITION, so the significant
                        -- entry is appended, never inserted).
                        take 1 [ (lycKind c, lycId c, lycRolls c)
                               | c ← lydContents def ]
                            `shouldBe` [("loot_table", "ruin_common", 2)]
                    defs → expectationFailure
                        ("expected exactly one location def, got "
                            <> show (length defs))

    -- #1796: the authored-coordinate DOMAIN, the first of the two
    -- complementary boundaries. Every case here decodes a COMPLETE
    -- LocationYamlDef, because that is the only scope carrying the
    -- location id the rejection is attributed with -- LocationYamlBounds
    -- 's own FromJSON instance still accepts any four Ints on purpose,
    -- so 'decodeBounds' would not exercise this rule at all.
    describe "authored coordinate domain (#1796)" $ do
        let limit  = authoredLocationCoordinateLimit
            defAt ∷ Int → Int → Int → Int → BS.ByteString
            defAt mnx mny mxx mxy = BS.pack
                ("{ id: t, builder: b,\
                 \  naming: { heads: [KEEP], modifiers: [ASH] },\
                 \  bounds: { min_x: " <> show mnx
                     <> ", min_y: " <> show mny
                     <> ", max_x: " <> show mxx
                     <> ", max_y: " <> show mxy <> " } }")

        it "the limit is exactly 2^31 - 1" $
            limit `shouldBe` 2147483647

        it "accepts a box sitting exactly on both authored limits" $
            decodeDef (defAt (negate limit) (negate limit) limit limit)
                `shouldSatisfy` isRight'

        it "accepts the shipped ruin_small box, well inside the domain" $
            decodeDef (defAt (-2) (-2) 2 2) `shouldSatisfy` isRight'

        -- Each of the four fields is driven out of the domain on its
        -- OWN, so a rejection that named the wrong field -- or collapsed
        -- all four into one generic message -- fails here.
        it "rejects bounds.min_x one step below the negative limit, \
           \naming the location and that field" $
            decodeDef (defAt (negate limit - 1) (negate limit) limit limit)
                `shouldSatisfy` rejectedNamingFields "t" ["bounds.min_x"]

        it "rejects bounds.min_y one step below the negative limit, \
           \naming the location and that field" $
            decodeDef (defAt (negate limit) (negate limit - 1) limit limit)
                `shouldSatisfy` rejectedNamingFields "t" ["bounds.min_y"]

        it "rejects bounds.max_x one step above the positive limit, \
           \naming the location and that field" $
            decodeDef (defAt (negate limit) (negate limit) (limit + 1) limit)
                `shouldSatisfy` rejectedNamingFields "t" ["bounds.max_x"]

        it "rejects bounds.max_y one step above the positive limit, \
           \naming the location and that field" $
            decodeDef (defAt (negate limit) (negate limit) limit (limit + 1))
                `shouldSatisfy` rejectedNamingFields "t" ["bounds.max_y"]

        -- The check must be a direct two-sided comparison against the
        -- domain. A magnitude test written with @abs ∷ Int → Int@ would
        -- accept minBound, because 'abs' 'minBound' IS 'minBound' --
        -- the single most extreme value the rule exists to exclude.
        it "rejects minBound, which an 'abs'-based magnitude check would \
           \wrongly accept" $ do
            decodeDef (defAt minBound (-2) 2 2)
                `shouldSatisfy` rejectedNamingFields "t" ["bounds.min_x"]
            decodeDef (defAt (-2) minBound 2 2)
                `shouldSatisfy` rejectedNamingFields "t" ["bounds.min_y"]

        it "rejects maxBound too" $
            decodeDef (defAt (-2) (-2) maxBound 2)
                `shouldSatisfy` rejectedNamingFields "t" ["bounds.max_x"]

        it "reports the offending VALUE and the accepted domain, so an \
           \author is told what to change it to" $
            decodeDef (defAt (-3000000000) (-2) 2 2)
                `shouldSatisfy` rejectedNamingFields "t"
                    [ "bounds.min_x", "-3000000000"
                    , "-2147483647", "2147483647" ]

        -- The range rule sits BESIDE the #777/#1151 ordering rule, not
        -- instead of it: an in-domain but inverted box still earns the
        -- ordering rejection it always did.
        it "still rejects an in-domain but inverted box on each axis" $ do
            decodeDef (defAt 5 (-2) 2 2)
                `shouldSatisfy` rejectedNamingFields "t"
                    ["bounds.min_x", "bounds.max_x"]
            decodeDef (defAt (-2) 5 2 2)
                `shouldSatisfy` rejectedNamingFields "t"
                    ["bounds.min_y", "bounds.max_y"]

        it "still accepts a degenerate single-tile box (min == max)" $
            decodeDef (defAt 0 0 0 0) `shouldSatisfy` isRight'

        it "one out-of-domain box fails the WHOLE file's decode -- the \
           \valid def beside it is not returned without it" $ do
            let goodOnly = "{ locations: [\
                    \ { id: ok, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } } ] }"
                withBad = "{ locations: [\
                    \ { id: ok, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } },\
                    \ { id: bad, builder: b,\
                    \   naming: { heads: [KEEP], modifiers: [ASH] },\
                    \   bounds: { min_x: -2, min_y: -2,\
                    \             max_x: 2147483648, max_y: 2 } } ] }"
            fmap (map lydId . lyfLocations) (decodeFile goodOnly)
                `shouldBe` Right ["ok"]
            decodeFile withBad
                `shouldSatisfy` rejectedNamingFields "bad" ["bounds.max_x"]

        -- Requirement 3: that file-level failure must actually REACH
        -- 'loadLocationYaml' 's boundary, where 'loadYamlList' turns it
        -- into one CatAsset warning and an empty result -- which is what
        -- makes engine.loadLocationYaml register nothing and return 0.
        it "loadLocationYaml returns [] and logs ONE CatAsset warning \
           \naming the invalid definition and field" $ do
            let contents = unlines
                    [ "locations:"
                    , "  - id: ok"
                    , "    builder: b"
                    , "    naming: { heads: [KEEP], modifiers: [ASH] }"
                    , "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
                    , "  - id: bad"
                    , "    builder: b"
                    , "    naming: { heads: [KEEP], modifiers: [ASH] }"
                    , "    bounds: { min_x: -2, min_y: -2,"
                    , "              max_x: 2147483648, max_y: 2 }"
                    ]
            withTempLocationYaml "range.yaml" contents $ \path → do
                (logger, entriesRef) ← callbackLogger
                defs ← loadLocationYaml logger path
                map lydId defs `shouldBe` []
                entries ← readIORef entriesRef
                case entries of
                    [entry] → do
                        leLevel entry `shouldBe` LevelWarn
                        leCategory entry `shouldBe` CatAsset
                        leMessage entry `shouldSatisfy`
                            T.isInfixOf "location \'bad\'"
                        leMessage entry `shouldSatisfy`
                            T.isInfixOf "bounds.max_x"
                    other → expectationFailure
                        ("expected exactly one captured log entry, got "
                            <> show (length other))

        -- #917: a guaranteed significant item that resolves against no
        -- registered def is a HARDER failure than an ordinary content
        -- id, which may warn and be skipped at spawn time (#90). The
        -- obligation is created at PLACEMENT, so an item that can never
        -- spawn leaves the location permanently unclearable — which is
        -- why 'Engine.Scripting.Lua.API.Locations' rejects the whole
        -- file on any result here, exactly as it does for a bad naming
        -- scheme.
        it "significantItemErrors names every unresolved guaranteed item \
           \and ignores every incidental id, resolved or not" $ do
            let registered = HS.fromList ["processing_unit", "rations"]
                defs = decodedDefs
                    "{ id: a, builder: b,\
                    \  naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  contents: [ { kind: item, id: processing_unit,\
                    \                significant: true },\
                    \              { kind: item, id: no_such_item },\
                    \              { kind: loot_table, id: no_such_table },\
                    \              { kind: item, id: ghost_core,\
                    \                significant: true } ] }"
            significantItemErrors registered defs
                `shouldBe` [ "location 'a': guaranteed significant content \
                             \'ghost_core' names no registered item \
                             \definition" ]

        it "significantItemErrors accepts a file whose every guaranteed \
           \item resolves, and an empty registry rejects one" $ do
            let defs = decodedDefs
                    "{ id: a, builder: b,\
                    \  naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  contents: [ { kind: item, id: processing_unit,\
                    \                significant: true } ] }"
            significantItemErrors (HS.singleton "processing_unit") defs
                `shouldBe` []
            length (significantItemErrors HS.empty defs) `shouldBe` 1

        it "the shipped ruin_small.yaml's guaranteed item resolves \
           \against the shipped item definitions" $ do
            result ← Yaml.decodeFileEither "data/locations/ruin_small.yaml"
            names ← shippedItemNames
            case result of
                Left err → expectationFailure (show (err ∷ Yaml.ParseException))
                Right lf → significantItemErrors names (lyfLocations lf)
                    `shouldBe` []

        it "a file whose defs are all in-domain still loads normally" $ do
            let contents = unlines
                    [ "locations:"
                    , "  - id: ok"
                    , "    builder: b"
                    , "    naming: { heads: [KEEP], modifiers: [ASH] }"
                    , "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
                    ]
            withTempLocationYaml "ok.yaml" contents $ \path → do
                (logger, _) ← callbackLogger
                defs ← loadLocationYaml logger path
                map lydId defs `shouldBe` ["ok"]

    describe "translateBounds" $
        it "anchors a relative box at an absolute tile" $
            translateBounds (10, 20) (RelBounds (-2) (-2) 2 2)
                `shouldBe` AbsBounds 8 18 12 22

    -- #1796: the checked ARITHMETIC these two primitives provide. The
    -- attributed, definition-aware construction that builds on them is
    -- 'Location.Instance.locationInstanceGeometry', covered under
    -- "Location instance identity".
    describe "checked coordinate arithmetic (#1796)" $ do
        it "narrows any value inside Int's range unchanged" $ do
            narrowTileCoordinate "c" 0 `shouldBe` Right 0
            narrowTileCoordinate "c" (toInteger (maxBound ∷ Int))
                `shouldBe` Right maxBound
            narrowTileCoordinate "c" (toInteger (minBound ∷ Int))
                `shouldBe` Right minBound

        it "refuses a value one step outside Int's range on either side, \
           \reporting the component and the exact value" $ do
            narrowTileCoordinate "c" (toInteger (maxBound ∷ Int) + 1)
                `shouldBe` Left (LocationGeometryFailure "c"
                                    (toInteger (maxBound ∷ Int) + 1))
            narrowTileCoordinate "c" (toInteger (minBound ∷ Int) - 1)
                `shouldBe` Left (LocationGeometryFailure "c"
                                    (toInteger (minBound ∷ Int) - 1))

        it "agrees exactly with translateBounds wherever the translation \
           \is representable" $ do
            let cases = [ ((10, 20), RelBounds (-2) (-2) 2 2)
                        , ((0, 0),   RelBounds 0 0 0 0)
                        , ((-40, 72), RelBounds (-9) (-3) 1 9)
                        , ((5, -3),  RelBounds (-2147483647) (-2147483647)
                                               2147483647 2147483647) ]
            forM_ cases $ \(anchor@(gx, gy), rel) →
                translateBoundsChecked (toInteger gx, toInteger gy) rel
                    `shouldBe` Right (translateBounds anchor rel)

        it "refuses the first offending component in min_x, min_y, max_x, \
           \max_y order rather than wrapping" $ do
            let top = toInteger (maxBound ∷ Int)
            translateBoundsChecked (top, 0) (RelBounds 0 0 1 0)
                `shouldBe` Left (LocationGeometryFailure "bounds.max_x"
                                    (top + 1))
            translateBoundsChecked (0, top) (RelBounds 0 0 0 1)
                `shouldBe` Left (LocationGeometryFailure "bounds.max_y"
                                    (top + 1))
            translateBoundsChecked (toInteger (minBound ∷ Int), 0)
                                   (RelBounds (-1) 0 0 0)
                `shouldBe` Left (LocationGeometryFailure "bounds.min_x"
                                    (toInteger (minBound ∷ Int) - 1))

    describe "boundsContainsPoint (non-wrapping)" $ do
        let box = AbsBounds 0 0 4 4
        it "contains an interior point" $
            boundsContainsPoint 0 box (2, 2) `shouldBe` True
        it "contains points on every edge and corner (inclusive)" $ do
            boundsContainsPoint 0 box (0, 0) `shouldBe` True
            boundsContainsPoint 0 box (4, 4) `shouldBe` True
            boundsContainsPoint 0 box (0, 4) `shouldBe` True
            boundsContainsPoint 0 box (4, 0) `shouldBe` True
        it "excludes a point just outside the box" $ do
            boundsContainsPoint 0 box (5, 2) `shouldBe` False
            boundsContainsPoint 0 box (2, -1) `shouldBe` False

    describe "boundsIntersect (non-wrapping)" $ do
        it "true for overlapping interiors" $
            boundsIntersect 0 (AbsBounds 0 0 4 4) (AbsBounds 2 2 6 6)
                `shouldBe` True
        it "true for boxes touching along a shared edge" $
            boundsIntersect 0 (AbsBounds 0 0 4 4) (AbsBounds 4 0 8 4)
                `shouldBe` True
        it "true for boxes touching at a single shared corner" $
            boundsIntersect 0 (AbsBounds 0 0 4 4) (AbsBounds 4 4 8 8)
                `shouldBe` True
        it "false for boxes separated by a gap" $
            boundsIntersect 0 (AbsBounds 0 0 4 4) (AbsBounds 6 0 10 4)
                `shouldBe` False

    describe "distancePointToBounds (non-wrapping)" $ do
        let box = AbsBounds 0 0 4 4
        it "is 0 for a point inside" $
            distancePointToBounds 0 box (2, 2) `shouldBe` 0
        it "is 0 for a point exactly on the edge" $
            distancePointToBounds 0 box (4, 2) `shouldBe` 0
        it "is the Chebyshev distance for a point outside on one axis" $
            distancePointToBounds 0 box (7, 2) `shouldBe` 3
        it "is the Chebyshev distance for a point outside on both axes" $
            distancePointToBounds 0 box (7, 7) `shouldBe` 3

    describe "distanceBoundsToBounds (non-wrapping)" $ do
        it "is 0 for boxes touching along an edge" $
            distanceBoundsToBounds 0 (AbsBounds 0 0 4 4) (AbsBounds 4 0 8 4)
                `shouldBe` 0
        it "is 0 for overlapping boxes" $
            distanceBoundsToBounds 0 (AbsBounds 0 0 4 4) (AbsBounds 2 2 6 6)
                `shouldBe` 0
        it "is the gap between two separated boxes" $
            distanceBoundsToBounds 0 (AbsBounds 0 0 4 4) (AbsBounds 7 0 10 4)
                `shouldBe` 3

    describe "cylindrical U-seam behavior (#422-style, tile granularity)" $ do
        -- worldSize 8 chunks -> worldWidthTiles 128, halfW 64. A box at
        -- (70,6) and a point at (7,70) are physical neighbours under the
        -- u-wrap (shift the box by (-64,+64) and it lands one tile from
        -- the point) even though their raw coordinates are far apart —
        -- mirrors the chunkSeamChebyshev spec in Test.Headless.WorldGen
        -- ("raw Chebyshev says 4, actual is 1") at tile instead of chunk
        -- scale.
        let ws = 8 ∷ Int
            box = AbsBounds 68 4 72 8
            farPoint = (6, 70)
        it "a seam-adjacent point is NOT contained by the raw box" $
            boundsContainsPoint 0 box farPoint `shouldBe` False
        it "the same point IS contained once the seam wrap is considered" $
            boundsContainsPoint ws box farPoint `shouldBe` True
        it "seam-aware distance is 0 (contained); the raw distance is not" $ do
            distancePointToBounds ws box farPoint `shouldBe` 0
            distancePointToBounds 0  box farPoint `shouldBe` 62
        it "a non-wrapping (arena / zero-size) world never wraps" $
            boundsContainsPoint 0 (AbsBounds 60 0 63 4) (-64, 0)
                `shouldBe` False

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef', with 'CatAsset' debug logging enabled -- mirrors
--   'Test.Headless.Asset.YamlList' 's own fixture, which does not
--   export it.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :))
        , lcDebugCategories = [CatAsset]
        }
    pure (logger, entriesRef)

withTempLocationYaml ∷ FilePath → String → (FilePath → IO a) → IO a
withTempLocationYaml name contents action =
    withExclusiveTempDirectory "synarchy-location-bounds-spec" $ \dir → do
        let path = dir </> name
        writeFile path contents
        action path
