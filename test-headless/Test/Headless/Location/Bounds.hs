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
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Engine.Asset.YamlLocations
    ( LocationYamlBounds(..), LocationYamlContent(..), LocationYamlDef(..)
    , LocationYamlFile(..) )
import Location.Bounds

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
                    defs → expectationFailure
                        ("expected exactly one location def, got "
                            <> show (length defs))

    describe "translateBounds" $
        it "anchors a relative box at an absolute tile" $
            translateBounds (10, 20) (RelBounds (-2) (-2) 2 2)
                `shouldBe` AbsBounds 8 18 12 22

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
