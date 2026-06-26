{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for 'World.River.Graph.classifyMouth' — the river-mouth
--   sink classifier. We don't need the engine: we hand-build a lake
--   lookup map and exercise the ocean / lake / inland branches directly,
--   including the wrap-aware proximity test and nearest-lake selection.
module Test.Headless.River.Graph (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Hydrology.Types (LakeParams(..), LakeSource(..))
import World.River.Graph (SinkType(..), classifyMouth)

-- | World size in chunks used for the wrap arithmetic (matches the
--   canonical small test world; chunkSize is 16 → 1024-tile u-period).
worldSize ∷ Int
worldSize = 64

-- | A lake of the given id, centre and radius. Surface/depth/source are
--   irrelevant to mouth classification, so we pin them to fixed values.
lake ∷ Int → GeoCoord → Int → (GeoFeatureId, LakeParams)
lake fid centre radius =
    ( GeoFeatureId fid
    , LakeParams
        { lkCenter  = centre
        , lkRadius  = radius
        , lkSurface = 10
        , lkDepth   = 5
        , lkSource  = TectonicBasin
        } )

lakes ∷ [(GeoFeatureId, LakeParams)] → HM.HashMap GeoFeatureId LakeParams
lakes = HM.fromList

noLakes ∷ HM.HashMap GeoFeatureId LakeParams
noLakes = HM.empty

spec ∷ Spec
spec = do
    describe "classifyMouth" $ do

        it "classifies an at/below-sea-level mouth as OceanSink" $ do
            -- Sea level is 0; a lake sitting on the mouth must not win.
            classifyMouth worldSize 0 (GeoCoord 100 100)
                (lakes [lake 1 (GeoCoord 100 100) 10])
                `shouldBe` OceanSink
            classifyMouth worldSize (-3) (GeoCoord 100 100) noLakes
                `shouldBe` OceanSink

        it "classifies an above-sea mouth with no nearby lake as InlandSink" $
            classifyMouth worldSize 5 (GeoCoord 100 100) noLakes
                `shouldBe` InlandSink

        it "classifies a mouth at a lake centre as LakeSink for that lake" $
            classifyMouth worldSize 5 (GeoCoord 100 100)
                (lakes [lake 7 (GeoCoord 100 100) 10])
                `shouldBe` LakeSink (GeoFeatureId 7)

        it "treats a mouth at the lake shoreline (≈ one radius out) as a LakeSink" $
            -- 8 tiles from a radius-10 centre is within the 1.25× slack.
            classifyMouth worldSize 5 (GeoCoord 108 100)
                (lakes [lake 7 (GeoCoord 100 100) 10])
                `shouldBe` LakeSink (GeoFeatureId 7)

        it "treats a mouth well beyond the slack radius as an InlandSink" $
            -- 30 tiles from a radius-10 centre (slack radius ≈ 12) misses.
            classifyMouth worldSize 5 (GeoCoord 130 100)
                (lakes [lake 7 (GeoCoord 100 100) 10])
                `shouldBe` InlandSink

        it "drains into the NEAREST in-range lake when several overlap" $
            -- Mouth at x=118: 18 tiles from lake 1 (centre 100), 22 from
            -- lake 2 (centre 140). Both r=20 (slack 25) contain it; the
            -- nearer lake 1 wins.
            classifyMouth worldSize 5 (GeoCoord 118 100)
                (lakes [ lake 1 (GeoCoord 100 100) 20
                       , lake 2 (GeoCoord 140 100) 20 ])
                `shouldBe` LakeSink (GeoFeatureId 1)

        it "uses wrap-aware distance along the u-axis" $
            -- Lake at the origin; mouth at (510,-510) is ~1020 away in raw
            -- u but only ~3 tiles away once the 1024-tile u-period wraps,
            -- so it still drains into the lake.
            classifyMouth worldSize 5 (GeoCoord 510 (-510))
                (lakes [lake 3 (GeoCoord 0 0) 10])
                `shouldBe` LakeSink (GeoFeatureId 3)
