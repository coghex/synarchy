{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | "Location spatial bounds" (#777): the authoritative footprint every
--   location definition declares — YAML parsing/rejection, the pure
--   translate/contain/intersect/expand/distance operations later
--   location work (#778/#779/#780) shares, cylindrical-seam behavior,
--   and the shipped ruin_small's exact 5x5 contract.
module Test.Headless.Location.Bounds (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import Engine.Asset.YamlLocations
    ( LocationYamlBounds(..), LocationYamlDef(..), LocationYamlFile(..) )
import Location.Bounds

decodeBounds ∷ BS.ByteString → Either String LocationYamlBounds
decodeBounds = either (Left . show) Right . Yaml.decodeEither'

decodeDef ∷ BS.ByteString → Either String LocationYamlDef
decodeDef = either (Left . show) Right . Yaml.decodeEither'

isLeft' ∷ Either a b → Bool
isLeft' = either (const True) (const False)

isRight' ∷ Either a b → Bool
isRight' = either (const False) (const True)

spec ∷ Spec
spec = describe "Location spatial bounds" $ do

    describe "YAML bounds parsing (#777)" $ do
        it "parses a valid bounds block" $
            decodeBounds "{ min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
                `shouldBe` Right (LocationYamlBounds (-2) (-2) 2 2)

        it "rejects a definition missing bounds entirely" $
            decodeDef "{ id: t, builder: b, discovery_margin: 6 }"
                `shouldSatisfy` isLeft'

        it "rejects a malformed bounds block (wrong field type)" $
            decodeDef
                "{ id: t, builder: b, discovery_margin: 6,\
                \  bounds: { min_x: nope, min_y: -2, max_x: 2, max_y: 2 } }"
                `shouldSatisfy` isLeft'

        it "rejects inverted bounds (min_x > max_x)" $
            decodeDef
                "{ id: t, builder: b, discovery_margin: 6,\
                \  bounds: { min_x: 5, min_y: -2, max_x: 2, max_y: 2 } }"
                `shouldSatisfy` isLeft'

        it "rejects inverted bounds (min_y > max_y)" $
            decodeDef
                "{ id: t, builder: b, discovery_margin: 6,\
                \  bounds: { min_x: -2, min_y: 5, max_x: 2, max_y: 2 } }"
                `shouldSatisfy` isLeft'

        it "rejects a negative discovery margin" $
            decodeDef
                "{ id: t, builder: b, discovery_margin: -1,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } }"
                `shouldSatisfy` isLeft'

        it "rejects a fixed content position outside the declared bounds" $
            decodeDef
                "{ id: t, builder: b, discovery_margin: 6,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: item, id: x, position: {x: 5, y: 0} } ] }"
                `shouldSatisfy` isLeft'

        it "accepts a fixed content position on the bounds edge (inclusive)" $
            decodeDef
                "{ id: t, builder: b, discovery_margin: 6,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  contents: [ { kind: item, id: x, position: {x: 2, y: -2} } ] }"
                `shouldSatisfy` isRight'

        it "the shipped ruin_small.yaml declares the exact 5x5 contract" $ do
            result ← Yaml.decodeFileEither "data/locations/ruin_small.yaml"
            case result of
                Left err → expectationFailure (show (err ∷ Yaml.ParseException))
                Right lf → case lyfLocations lf of
                    [def] → do
                        lydBounds def `shouldBe` LocationYamlBounds (-2) (-2) 2 2
                        lydDiscoveryMargin def `shouldBe` 6
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

    describe "expandBounds" $ do
        it "grows the box outward on all four sides by the margin" $
            expandBounds 3 (AbsBounds 0 0 4 4) `shouldBe` AbsBounds (-3) (-3) 7 7
        it "a zero margin is a no-op" $
            expandBounds 0 (AbsBounds 1 1 3 3) `shouldBe` AbsBounds 1 1 3 3

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
