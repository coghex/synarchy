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
                                , RenderBatch(..), mergeQuadsToBatch
                                , batchFromSortedQuads, mergeSortedQuads
                                , sortQuadsByLayer)
import qualified Data.Map as Map
import Data.List (sortOn)
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

-- | 'quadAt' with an explicit layer.
quadOnLayer ∷ Word32 → Float → SortableQuad
quadOnLayer l key = (quadAt key) { sqLayer = LayerId l }

-- | Sort a run by key the same way the builders do (for references).
sortRun ∷ [SortableQuad] → V.Vector SortableQuad
sortRun = V.fromList ∘ sortOn sqSortKey

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

    -- #446: static quads are pre-sorted per layer at cache rebuild and
    -- dynamic quads linear-merged in per frame. These pin the merge path
    -- to the same result the old full re-sort produced.
    describe "sortQuadsByLayer" $ do
        it "groups by layer and depth-sorts each run" $ do
            let quads = V.fromList [ quadOnLayer 2 5, quadOnLayer 1 3
                                   , quadOnLayer 2 4, quadOnLayer 1 8 ]
                m = sortQuadsByLayer quads
            Map.keys m `shouldBe` [LayerId 1, LayerId 2]
            map sqSortKey (V.toList (m Map.! LayerId 1)) `shouldBe` [3, 8]
            map sqSortKey (V.toList (m Map.! LayerId 2)) `shouldBe` [4, 5]

        it "produces an empty map from no quads" $
            Map.null (sortQuadsByLayer V.empty) `shouldBe` True

    describe "mergeSortedQuads" $ do
        it "merges two sorted runs into one sorted run" $ do
            let a = sortRun (map quadAt [1, 4, 6])
                b = sortRun (map quadAt [2, 3, 5, 7])
                merged = mergeSortedQuads a b
            map sqSortKey (V.toList merged) `shouldBe` [1 .. 7]

        it "is identity against an empty side" $ do
            let a = sortRun (map quadAt [1, 2])
            map sqSortKey (V.toList (mergeSortedQuads a V.empty)) `shouldBe` [1, 2]
            map sqSortKey (V.toList (mergeSortedQuads V.empty a)) `shouldBe` [1, 2]

        it "prefers the left (static) run on equal keys" $ do
            -- Same sort key, distinguishable by vertex tag: the left
            -- quad's vertices must come first so the right (dynamic)
            -- quad draws after — i.e. on top.
            let staticQ = (quadAt 5) { sqV0 = vAt 100 }
                dynQ    = (quadAt 5) { sqV0 = vAt 200 }
                merged  = mergeSortedQuads (V.singleton staticQ) (V.singleton dynQ)
            map (tagOf ∘ sqV0) (V.toList merged) `shouldBe` [100, 200]

    describe "batchFromSortedQuads" $ do
        it "matches mergeQuadsToBatch on the same (distinct-key) input" $ do
            let statics = map quadAt [1, 4, 6]
                dyns    = map quadAt [2, 3, 5]
                viaMerge = batchFromSortedQuads (LayerId 1)
                    (mergeSortedQuads (sortRun statics) (sortRun dyns))
                viaSort = mergeQuadsToBatch (LayerId 1)
                    (V.fromList (statics ⧺ dyns))
            VS.toList (rbVertices viaMerge) `shouldBe` VS.toList (rbVertices viaSort)
            rbAvgZ viaMerge `shouldBe` rbAvgZ viaSort
            rbTexture viaMerge `shouldBe` rbTexture viaSort
