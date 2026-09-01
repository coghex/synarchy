-- | The one shared definition every world sprite quad's four vertices
--   come out of (#1152): winding order, UV↔corner pairing, and the
--   five payload values each corner repeats.
--
--   Eight producers — buildings and their ghosts, structure pieces,
--   sliced front walls, corner posts, units, ground items and blood
--   decals — used to restate this by hand. Swapping a UV pair there
--   mirrors or shears a sprite rather than failing to compile, so the
--   correspondence is pinned here against literal expected 'Vertex'
--   values rather than re-derived from the helper's own fields.
--
--   Every case uses distinguishable numbers: no two coordinates, UVs
--   or payload slots share a value, so a transposition cannot pass by
--   coincidence.
module Test.Headless.Render.QuadVertices (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Vulkan.Types.Vertex
    ( Vertex(..), Vec2(..), Vec4(..)
    , QuadCorners(..), QuadUV(..), QuadPayload(..)
    , quadVertices, rectCorners, fullQuadUV, tileWorldUV )

-- A payload whose five values are pairwise distinct and none of them 0
-- or 1, so any of them being dropped, defaulted or swapped shows up.
payload ∷ QuadPayload
payload = QuadPayload
    { qpTint      = Vec4 0.25 0.5 0.75 0.125
    , qpAtlasSlot = 17
    , qpFaceMap   = 23
    , qpFlags     = 1
    , qpWorldUV   = tileWorldUV 7 (-3)
    }

-- | The expected vertex at one corner: the position and UV under test,
--   plus the payload spelled out independently of 'payload'.
at ∷ Float → Float → Float → Float → Vertex
--   The solar page slot is not part of 'QuadPayload': attribution is
--   stamped onto finished quads by 'World.Render' once the owning page
--   is known (#1869), so every producer's own output is page-less here.
at px py u v = Vertex (Vec2 px py) (Vec2 u v)
                      (Vec4 0.25 0.5 0.75 0.125)
                      17 23 1 (tileWorldUV 7 (-3)) 0

spec ∷ Spec
spec = do
    describe "rectCorners" $
        it "expands a top-left corner and a size into the canonical winding" $
            rectCorners (Vec2 100 200) (Vec2 30 40)
                `shouldBe` QuadCorners
                    { qcTopLeft     = Vec2 100 200
                    , qcTopRight    = Vec2 130 200
                    , qcBottomRight = Vec2 130 240
                    , qcBottomLeft  = Vec2 100 240
                    }

    describe "quadVertices" $ do
        it "pairs the full UV rect with the four corners (the five plain sites)" $
            quadVertices (rectCorners (Vec2 100 200) (Vec2 30 40))
                         fullQuadUV payload
                `shouldBe` ( at 100 200 0 0
                           , at 130 200 1 0
                           , at 130 240 1 1
                           , at 100 240 0 1
                           )

        it "mirrors horizontally when the U range runs backwards (Unit.Render's flip)" $
            -- The unflipped sample's sub-rect is u ∈ [0.25, 0.5];
            -- flipping hands leftU/rightU over the other way round,
            -- and ONLY the U values move.
            quadVertices (rectCorners (Vec2 100 200) (Vec2 30 40))
                         QuadUV { quLeftU   = 0.5
                                , quTopV    = 0.125
                                , quRightU  = 0.25
                                , quBottomV = 0.375
                                }
                         payload
                `shouldBe` ( at 100 200 0.5  0.125
                           , at 130 200 0.25 0.125
                           , at 130 240 0.25 0.375
                           , at 100 240 0.5  0.375
                           )

        it "keeps a sliced U range on the matching edges (Structure.Render's wall strip)" $
            -- Strip 5 of 16: positions [xa,xb] sample exactly [ua,ub].
            quadVertices QuadCorners { qcTopLeft     = Vec2 105 200
                                     , qcTopRight    = Vec2 111 200
                                     , qcBottomRight = Vec2 111 240
                                     , qcBottomLeft  = Vec2 105 240
                                     }
                         QuadUV { quLeftU   = 0.3125
                                , quTopV    = 0
                                , quRightU  = 0.375
                                , quBottomV = 1
                                }
                         payload
                `shouldBe` ( at 105 200 0.3125 0
                           , at 111 200 0.375  0
                           , at 111 240 0.375  1
                           , at 105 240 0.3125 1
                           )

        it "accepts four independently positioned corners (BloodQuads' rotation)" $
            -- No two corners share an x or a y, and the quad is neither
            -- axis-aligned nor derivable from an origin plus a size.
            quadVertices QuadCorners { qcTopLeft     = Vec2 11 19
                                     , qcTopRight    = Vec2 37 23
                                     , qcBottomRight = Vec2 41 53
                                     , qcBottomLeft  = Vec2 13 47
                                     }
                         fullQuadUV payload
                `shouldBe` ( at 11 19 0 0
                           , at 37 23 1 0
                           , at 41 53 1 1
                           , at 13 47 0 1
                           )

        it "repeats tint, atlas slot, face-map slot, flags and world UV on every corner" $ do
            let (v0, v1, v2, v3) =
                    quadVertices (rectCorners (Vec2 1 2) (Vec2 3 4))
                                 fullQuadUV payload
                trailing v = ( color v, atlasId v, faceMapId v
                             , renderFlags v, worldUV v )
                expected = ( Vec4 0.25 0.5 0.75 0.125
                           , 17, 23, 1, tileWorldUV 7 (-3) )
            map trailing [v0, v1, v2, v3] `shouldBe` replicate 4 expected
