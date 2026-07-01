{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for the sprite-batch builders 'mergeQuadsToBatch' and
--   'createBatch' — the vertex-expansion contract behind #445's
--   boxed→storable 'rbVertices' switch. Guards that quads land in
--   ascending sort-key order, each quad expands to the two-triangle
--   pattern (v0,v1,v2, v0,v2,v3), and counts/metadata survive the
--   representation change. No engine needed: both builders are pure.
module Test.Headless.Scene.BatchMerge (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Base (LayerId(..), ObjectId(..))
import Engine.Scene.Types.Batch (SortableQuad(..), DrawableObject(..)
                                , RenderBatch(..), mergeQuadsToBatch)
import Engine.Scene.Batch.Update (createBatch)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..), mkVertex)

-- | A vertex tagged by its position's x component, so expansion order
--   is observable in the output vector.
vAt ∷ Float → Vertex
vAt k = mkVertex (Vec2 k 0) (Vec2 0 0) (Vec4 1 1 1 1) 0 0

-- | Tag of a vertex (inverse of 'vAt').
tagOf ∷ Vertex → Float
tagOf v = x (pos v)

-- | A quad whose four corners carry tags key·10+0..3.
quadAt ∷ Float → SortableQuad
quadAt key = SortableQuad
    { sqSortKey  = key
    , sqV0       = vAt (key * 10 + 0)
    , sqV1       = vAt (key * 10 + 1)
    , sqV2       = vAt (key * 10 + 2)
    , sqV3       = vAt (key * 10 + 3)
    , sqTexture  = TextureHandle 7
    , sqLayer    = LayerId 1
    }

-- | The expected two-triangle expansion of one quad: v0,v1,v2, v0,v2,v3.
expandedTags ∷ Float → [Float]
expandedTags key = map (\i → key * 10 + i) [0, 1, 2, 0, 2, 3]

spec ∷ Spec
spec = do
    describe "mergeQuadsToBatch" $ do
        it "sorts quads by sort key and expands each as v0,v1,v2,v0,v2,v3" $ do
            let batch = mergeQuadsToBatch (LayerId 1)
                            (V.fromList [quadAt 3, quadAt 1, quadAt 2])
            map tagOf (VS.toList (rbVertices batch))
                `shouldBe` concatMap expandedTags [1, 2, 3]

        it "emits 6 vertices per quad" $ do
            let batch = mergeQuadsToBatch (LayerId 1)
                            (V.fromList (map quadAt [1 .. 20]))
            VS.length (rbVertices batch) `shouldBe` 120

        it "produces an empty batch from no quads" $ do
            let batch = mergeQuadsToBatch (LayerId 1) V.empty
            VS.null (rbVertices batch) `shouldBe` True

        it "averages the depth range into rbAvgZ" $ do
            let batch = mergeQuadsToBatch (LayerId 1)
                            (V.fromList [quadAt 4, quadAt 2])
            rbAvgZ batch `shouldBe` 3

    describe "createBatch" $ do
        let dobj ∷ Word32 → Float → DrawableObject
            dobj oid z = DrawableObject
                { doId      = ObjectId oid
                , doTexture = TextureHandle 9
                , doV0      = vAt (z * 10 + 0)
                , doV1      = vAt (z * 10 + 1)
                , doV2      = vAt (z * 10 + 2)
                , doV3      = vAt (z * 10 + 3)
                , doZIndex  = z
                , doLayer   = LayerId 2
                }

        it "expands objects in input order as v0,v1,v2,v0,v2,v3" $ do
            let (_, batch) = createBatch ((TextureHandle 9, LayerId 2)
                                         , V.fromList [dobj 1 1, dobj 2 2])
            map tagOf (VS.toList (rbVertices batch))
                `shouldBe` concatMap expandedTags [1, 2]

        it "carries object ids and averages z" $ do
            let (_, batch) = createBatch ((TextureHandle 9, LayerId 2)
                                         , V.fromList [dobj 1 1, dobj 2 3])
            V.toList (rbObjects batch) `shouldBe` [ObjectId 1, ObjectId 2]
            rbAvgZ batch `shouldBe` 2
