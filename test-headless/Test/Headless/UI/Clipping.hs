{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #747 gate: opt-in rectangular descendant clipping (Phase C child
--   C1 of #741). Most of this suite is pure "UI.Clipping"/
--   "UI.Manager.Query" coverage — the acceptance explicitly wants the
--   clip decision testable with no Vulkan, window, or Lua engine,
--   mirroring @Test.Headless.UI.ElementInputPolicy@. A final block
--   proves the new opt-in is configurable/queryable through the real
--   Lua @UI@ API.
module Test.Headless.UI.Clipping (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Data.IORef (newIORef)
import Engine.Graphics.Font.Data (GlyphInstance(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types.Batch (RenderBatch(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Clipping
import UI.InputOwnership (routePointer, routeScroll, PointerKind(..), InputRoute(..))
import UI.Manager
import UI.Render (clipGlyphInstance, makeBoxBatches, renderSpriteBatch)
import UI.Types

-- * Pure fixtures (mirrors Test.Headless.UI.ElementInputPolicy)

page ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
page name layer mgr =
    let (h, m1) = createPage name layer mgr
    in (h, showPage h m1)

-- | A plain container at absolute (x,y)/(w,h), optionally opting into
--   clipping its descendants.
containerAt ∷ Text → (Float, Float) → (Float, Float) → Bool → PageHandle
           → UIPageManager → (ElementHandle, UIPageManager)
containerAt name (x, y) (w, h) clips pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClipChildren eh clips m2
    in (eh, m3)

-- | A passive child at a RELATIVE offset from its parent.
childAt ∷ ElementHandle → Text → (Float, Float) → (Float, Float) → PageHandle
       → UIPageManager → (ElementHandle, UIPageManager)
childAt parentH name (relX, relY) (w, h) pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addChildElement parentH eh relX relY m1
    in (eh, m2)

-- | A clickable hit surface parented under an existing element
--   (mirrors the real widget shape: a passive box + a clickable
--   hit-sprite child).
hitChildAt ∷ ElementHandle → Text → (Float, Float) → (Float, Float) → Text → PageHandle
          → UIPageManager → (ElementHandle, UIPageManager)
hitChildAt parentH name (relX, relY) (w, h) cb pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addChildElement parentH eh relX relY m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
    in (eh, m4)

-- | A clickable hit surface mounted directly on the page (no parent) —
--   the "floating root-mounted content" shape (dropdown option lists,
--   context menus).
rootHitAt ∷ Text → (Float, Float) → (Float, Float) → Text → PageHandle
         → UIPageManager → (ElementHandle, UIPageManager)
rootHitAt name (x, y) (w, h) cb pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
    in (eh, m4)

-- | The (min, max) position of every vertex in a render batch — an
--   axis-aligned bounding box, order-independent, so it doesn't assume
--   anything about how makeQuadVertices orders its 6 vertices.
vertexBounds ∷ VS.Vector Vertex → ((Float, Float), (Float, Float))
vertexBounds vs =
    let ps = map pos (VS.toList vs)
        xs = map x ps
        ys = map y ps
    in ((minimum xs, minimum ys), (maximum xs, maximum ys))

-- | Same, but for the UV (texture-coordinate) attribute.
uvBounds ∷ VS.Vector Vertex → ((Float, Float), (Float, Float))
uvBounds vs =
    let ts = map tex (VS.toList vs)
        us = map x ts
        vvs = map y ts
    in ((minimum us, minimum vvs), (maximum us, maximum vvs))

spec ∷ Spec
spec = do
    describe "effectiveClip — opt-in compatibility (#747)" $ do
        it "an element with no clipping ancestor is unclipped" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) False hudH m1
                (childH, m3) = childAt containerH "child" (0, 0) (50, 50) hudH m2
            in effectiveClip childH m3 `shouldBe` Nothing

        it "a container with ueClipChildren left False (the default) never restricts its children" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (10, 10) (20, 20) False hudH m1
                (_childH, m3) = hitChildAt containerH "row" (100, 100) (50, 20) "rowClick" hudH m2
            in routePointer PointerLeftClick (125, 110) m3 `shouldSatisfy` (≢ RouteMiss)

    describe "effectiveClip — full / partial / nested / empty intersections" $ do
        it "a child fully inside a clipping ancestor hits normally" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (childH, m3) = hitChildAt containerH "row" (10, 10) (50, 20) "rowClick" hudH m2
            in routePointer PointerLeftClick (30, 20) m3 `shouldBe` RouteElement childH "rowClick"

        it "a child straddling the clip boundary hits only within the visible slice" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                -- row spans absolute x in [60,120), clip only allows [0,100)
                (_childH, m3) = hitChildAt containerH "row" (60, 10) (60, 20) "rowClick" hudH m2
            in do
                routePointer PointerLeftClick (80, 20) m3 `shouldSatisfy` (≢ RouteMiss)
                routePointer PointerLeftClick (110, 20) m3 `shouldBe` RouteMiss

        it "nested clips intersect — a leaf is bounded by the tighter of two clipping ancestors" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (outerH, m2) = containerAt "outer" (0, 0) (200, 200) True hudH m1
                (innerH, m3) = childAt outerH "inner" (50, 50) (60, 60) hudH m2  -- abs [50,110)x[50,110)
                m4 = setElementClipChildren innerH True m3
                -- leaf abs spans [50,150)x[50,150) but inner only clips to [50,110)
                (_leafH, m5) = hitChildAt innerH "leaf" (0, 0) (100, 100) "leafClick" hudH m4
            in do
                routePointer PointerLeftClick (70, 70) m5 `shouldSatisfy` (≢ RouteMiss)
                routePointer PointerLeftClick (130, 70) m5 `shouldBe` RouteMiss
                routePointer PointerLeftClick (70, 130) m5 `shouldBe` RouteMiss

        it "an empty (non-overlapping) intersection hits nothing" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (outerH, m2) = containerAt "outer" (0, 0) (100, 100) True hudH m1
                -- inner sits entirely outside outer's own bounds
                (innerH, m3) = childAt outerH "inner" (200, 200) (50, 50) hudH m2
                m4 = setElementClipChildren innerH True m3
                (_leafH, m5) = hitChildAt innerH "leaf" (0, 0) (50, 50) "leafClick" hudH m4
            in routePointer PointerLeftClick (220, 220) m5 `shouldBe` RouteMiss

    -- Review round 5 (#857): a child merely TOUCHING a clip edge overlaps
    -- it in a zero-width/zero-height sliver — real area zero, so
    -- clipQuadUV draws nothing there. Hit-testing must agree: a point on
    -- that sliver must miss on every input path (click/hover/scroll),
    -- for every edge, while a child with genuine (however thin) positive
    -- overlap must still hit within its visible slice.
    describe "effectiveClip — a child flush against a clip edge (zero-area overlap) is not hittable" $ do
        it "flush against the RIGHT edge misses a click exactly on the seam" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                -- child abs rect (100,10,20,20); clip ends at x=100
                (_childH, m3) = hitChildAt containerH "row" (100, 10) (20, 20) "rowClick" hudH m2
            in routePointer PointerLeftClick (100, 20) m3 `shouldBe` RouteMiss

        it "flush against the LEFT edge misses a click exactly on the seam" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                -- child abs rect (-20,10,20,20); its right edge sits at x=0
                (_childH, m3) = hitChildAt containerH "row" (-20, 10) (20, 20) "rowClick" hudH m2
            in routePointer PointerLeftClick (0, 20) m3 `shouldBe` RouteMiss

        it "flush against the TOP edge misses a click exactly on the seam" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                -- child abs rect (10,-20,20,20); its bottom edge sits at y=0
                (_childH, m3) = hitChildAt containerH "row" (10, -20) (20, 20) "rowClick" hudH m2
            in routePointer PointerLeftClick (20, 0) m3 `shouldBe` RouteMiss

        it "flush against the BOTTOM edge misses a click exactly on the seam" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                -- child abs rect (10,100,20,20); clip ends at y=100
                (_childH, m3) = hitChildAt containerH "row" (10, 100) (20, 20) "rowClick" hudH m2
            in routePointer PointerLeftClick (20, 100) m3 `shouldBe` RouteMiss

        it "a genuine (positive-area) overlap at the same edge still hits within its visible slice" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                -- child abs rect (90,10,20,20): overlaps clip in a real
                -- [90,100)x[10,30) sliver, not just a seam
                (childH, m3) = hitChildAt containerH "row" (90, 10) (20, 20) "rowClick" hudH m2
            in do
                routePointer PointerLeftClick (95, 20) m3 `shouldBe` RouteElement childH "rowClick"
                routePointer PointerLeftClick (100, 20) m3 `shouldBe` RouteElement childH "rowClick"
                routePointer PointerLeftClick (105, 20) m3 `shouldBe` RouteMiss

        it "flush against a clip edge is not hover-hittable (findElementAt)" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (_rowH, m3) = childAt containerH "row" (100, 10) (20, 20) hudH m2
            -- The point sits on the clip's own edge, which coincides with
            -- the (unclipped-to-itself) container's own boundary, so the
            -- container itself is a legitimate hover target here; what
            -- the bug would have wrongly surfaced is the fully-clipped
            -- ROW, which must not be the result.
            in findElementAt (100, 20) m3 `shouldBe` Just containerH

        it "a scroll-capturing child flush against a clip edge does not capture wheel input" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (eh, m3) = childAt containerH "panelBg" (100, 10) (20, 20) hudH m2
                m4 = setElementCapturesScroll eh True m3
            in routeScroll (100, 20) m4 `shouldBe` Nothing

    describe "effectiveClip — movement/resize take effect immediately (no caching)" $ do
        it "moving the clipping ancestor immediately changes what's clipped" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (_childH, m3) = hitChildAt containerH "row" (0, 0) (100, 20) "rowClick" hudH m2
                m4 = setElementPosition containerH 500 500 m3
            in do
                routePointer PointerLeftClick (10, 10) m4 `shouldBe` RouteMiss
                routePointer PointerLeftClick (510, 510) m4 `shouldSatisfy` (≢ RouteMiss)

        it "resizing the clipping ancestor immediately changes the clip bounds" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (_childH, m3) = hitChildAt containerH "row" (0, 0) (100, 20) "rowClick" hudH m2
                m4 = setElementSize containerH 40 40 m3
            in do
                routePointer PointerLeftClick (30, 10) m4 `shouldSatisfy` (≢ RouteMiss)
                routePointer PointerLeftClick (80, 10) m4 `shouldBe` RouteMiss

    describe "clipQuadUV — render-path geometry shared by boxes/sprites/text" $ do
        it "passes a quad through unchanged with no clip in effect" $
            clipQuadUV Nothing (10, 10, 50, 50) (0, 0, 1, 1) `shouldBe` Just ((10, 10, 50, 50), (0, 0, 1, 1))

        it "returns Nothing for a quad fully outside the clip (nothing to draw)" $
            clipQuadUV (Just (0, 0, 100, 100)) (200, 200, 50, 50) (0, 0, 1, 1) `shouldBe` Nothing

        it "clips a partially-overlapping quad and adjusts its UV rect proportionally" $
            -- clip = [0,100)x[0,100); quad = [50,150)x[0,50) -> visible [50,100)x[0,50)
            case clipQuadUV (Just (0, 0, 100, 100)) (50, 0, 100, 50) (0, 0, 1, 1) of
                Just ((cx, cy, cw, ch), (u0, v0, u1, v1)) → do
                    (cx, cy, cw, ch) `shouldBe` (50, 0, 50, 50)
                    (u0, v0, u1, v1) `shouldBe` (0, 0, 0.5, 1)
                Nothing → expectationFailure "expected a clipped quad, got Nothing"

        it "an ancestor clip fully containing the quad leaves it unchanged" $
            clipQuadUV (Just (0, 0, 200, 200)) (10, 10, 50, 50) (0, 0, 1, 1)
                `shouldBe` Just ((10, 10, 50, 50), (0, 0, 1, 1))

    -- #747 review round 1 (concern 3): the underlying clipQuadUV
    -- helper being correct doesn't prove UI.Render's per-element WIRING
    -- (box tile math, glyph-instance field extraction) is correct — so
    -- these drive the actual pure functions UI.Render.makeBoxBatches /
    -- the text-clipping path call, not just clipQuadUV in isolation.
    -- Sprite rendering has no comparable wiring risk: renderElementData's
    -- RenderSprite branch is a single direct clipQuadUV call with no
    -- intermediate geometry, already fully covered above.
    describe "boxTileRects — the real per-tile box render geometry (UI.Render.makeBoxBatches)" $ do
        it "unclipped: all nine 3x3 tiles are present with the expected rects" $
            let tiles = boxTileRects 0 0 40 40 10 Nothing
                lookupTile t = [r | (t', r, _) ← tiles, t' ≡ t]
            in do
                length tiles `shouldBe` 9
                lookupTile TileNW     `shouldBe` [(0, 0, 10, 10)]
                lookupTile TileN      `shouldBe` [(10, 0, 20, 10)]
                lookupTile TileNE     `shouldBe` [(30, 0, 10, 10)]
                lookupTile TileW      `shouldBe` [(0, 10, 10, 20)]
                lookupTile TileCenter `shouldBe` [(10, 10, 20, 20)]
                lookupTile TileE      `shouldBe` [(30, 10, 10, 20)]
                lookupTile TileSW     `shouldBe` [(0, 30, 10, 10)]
                lookupTile TileS      `shouldBe` [(10, 30, 20, 10)]
                lookupTile TileSE     `shouldBe` [(30, 30, 10, 10)]

        it "a clip covering only the left part of the box drops the right-hand tiles and clips the straddling ones" $
            let tiles = boxTileRects 0 0 40 40 10 (Just (0, 0, 25, 40))
                tileNames = [t | (t, _, _) ← tiles]
                lookupTile t = [r | (t', r, _) ← tiles, t' ≡ t]
                lookupUV t = [uv | (t', _, uv) ← tiles, t' ≡ t]
            in do
                -- NE/E/SE sit entirely at x∈[30,40) — fully outside x<25.
                tileNames `shouldNotSatisfy` elem TileNE
                tileNames `shouldNotSatisfy` elem TileE
                tileNames `shouldNotSatisfy` elem TileSE
                length tiles `shouldBe` 6
                -- Untouched tiles (fully inside x<25) keep their full rect.
                lookupTile TileNW `shouldBe` [(0, 0, 10, 10)]
                lookupTile TileW  `shouldBe` [(0, 10, 10, 20)]
                lookupTile TileSW `shouldBe` [(0, 30, 10, 10)]
                -- Straddling tiles (N/Center/S, originally x∈[10,30)) are
                -- clipped to x∈[10,25) — width 15/20, and the UV right
                -- edge shrinks proportionally to 0.75.
                lookupTile TileN      `shouldBe` [(10, 0, 15, 10)]
                lookupTile TileCenter `shouldBe` [(10, 10, 15, 20)]
                lookupTile TileS      `shouldBe` [(10, 30, 15, 10)]
                lookupUV TileN `shouldBe` [(0, 0, 0.75, 1)]

        it "a clip disjoint from the box entirely drops all nine tiles" $
            boxTileRects 0 0 40 40 10 (Just (1000, 1000, 10, 10)) `shouldBe` []

    describe "clipGlyphInstance — the real per-glyph text render geometry (UI.Render)" $ do
        let glyph = GlyphInstance
                { instancePosition = (0, 0)
                , instanceSize     = (20, 10)
                , instanceUVRect   = (0, 0, 1, 1)
                , instanceColor    = (1, 1, 1, 1)
                }

        it "passes a glyph through unchanged with no clip in effect" $
            clipGlyphInstance Nothing glyph `shouldBe` Just glyph

        it "leaves a glyph unchanged when the clip fully contains it" $
            clipGlyphInstance (Just (0, 0, 100, 100)) glyph `shouldBe` Just glyph

        it "clips a partially-overlapping glyph and shrinks its UV rect to match" $
            case clipGlyphInstance (Just (0, 0, 10, 10)) glyph of
                Just gi → do
                    instancePosition gi `shouldBe` (0, 0)
                    instanceSize gi `shouldBe` (10, 10)
                    instanceUVRect gi `shouldBe` (0, 0, 0.5, 1)
                    -- Color is untouched by clipping.
                    instanceColor gi `shouldBe` (1, 1, 1, 1)
                Nothing → expectationFailure "expected a clipped glyph, got Nothing"

        it "drops a glyph that falls entirely outside the clip" $
            clipGlyphInstance (Just (1000, 1000, 10, 10)) glyph `shouldBe` Nothing

    -- #747 review round 2 (concern 3): boxTileRects/clipGlyphInstance
    -- above prove the underlying geometry, but a reviewer correctly
    -- noted that removing the clip call from the ACTUAL RenderBox/
    -- RenderSprite branches in UI.Render would still leave those green
    -- (they never call through UI.Render's own render-batch
    -- functions). makeBoxBatches/renderSpriteBatch ARE those actual
    -- functions (BindlessTextureSystem was dead weight — #747 dropped
    -- it, see UI.Render — so they're now pure and callable directly
    -- with no Vulkan/GPU), so these tests gate real call-site wiring:
    -- deleting the clip there breaks batch count/vertex/UV output here.
    describe "makeBoxBatches — the real RenderBox call site (UI.Render)" $ do
        let texSet = BoxTextureSet
                { btsCenter = TextureHandle 5, btsN = TextureHandle 2
                , btsS = TextureHandle 8, btsE = TextureHandle 6
                , btsW = TextureHandle 4, btsNE = TextureHandle 3
                , btsNW = TextureHandle 1, btsSE = TextureHandle 9
                , btsSW = TextureHandle 7
                }
            findByTex t bs = [b | b ← V.toList bs, rbTexture b ≡ TextureHandle t]

        it "unclipped: nine batches, textures matching the nine tiles" $
            let batches = makeBoxBatches texSet 0 0 40 40 10 (1, 1, 1, 1) (LayerId 0) Nothing
            in V.length batches `shouldBe` 9

        it "a partial clip drops the excluded tiles' batches and clips a straddling tile's vertices/UVs" $
            let batches = makeBoxBatches texSet 0 0 40 40 10 (1, 1, 1, 1) (LayerId 0) (Just (0, 0, 25, 40))
            in do
                V.length batches `shouldBe` 6
                -- NE (texture 3) sits entirely at x∈[30,40) — dropped.
                length (findByTex 3 batches) `shouldBe` 0
                -- Center (texture 5) is clipped from (10,10,20,20) to
                -- (10,10,15,20) — vertex bounds and UV bounds must
                -- reflect exactly that, not the unclipped tile.
                case findByTex 5 batches of
                    [b] → do
                        vertexBounds (rbVertices b) `shouldBe` ((10, 10), (25, 30))
                        uvBounds (rbVertices b) `shouldBe` ((0, 0), (0.75, 1))
                    other → expectationFailure ("expected exactly one center batch, got " ⧺ show (length other))

        it "a clip disjoint from the box produces zero batches" $
            makeBoxBatches texSet 0 0 40 40 10 (1, 1, 1, 1) (LayerId 0) (Just (1000, 1000, 10, 10))
                `shouldSatisfy` V.null

    describe "renderSpriteBatch — the real RenderSprite call site (UI.Render)" $ do
        it "unclipped: one batch spanning the sprite's full rect and UV" $
            let (batches, items) = renderSpriteBatch (TextureHandle 42) (1, 1, 1, 1) 10 10 50 50 (LayerId 0) Nothing
            in do
                V.length batches `shouldBe` 1
                V.length items `shouldBe` 1
                vertexBounds (rbVertices (V.head batches)) `shouldBe` ((10, 10), (60, 60))
                uvBounds (rbVertices (V.head batches)) `shouldBe` ((0, 0), (1, 1))

        it "a partial clip produces one batch with clipped vertex bounds and a proportionally shrunk UV rect" $
            let (batches, _) = renderSpriteBatch (TextureHandle 42) (1, 1, 1, 1) 50 0 100 50 (LayerId 0) (Just (0, 0, 100, 100))
            in case V.toList batches of
                [b] → do
                    vertexBounds (rbVertices b) `shouldBe` ((50, 0), (100, 50))
                    uvBounds (rbVertices b) `shouldBe` ((0, 0), (0.5, 1))
                other → expectationFailure ("expected exactly one batch, got " ⧺ show (length other))

        it "a clip fully excluding the sprite produces no batch at all" $
            let (batches, items) = renderSpriteBatch (TextureHandle 42) (1, 1, 1, 1) 200 200 50 50 (LayerId 0) (Just (0, 0, 100, 100))
            in do
                batches `shouldSatisfy` V.null
                items `shouldSatisfy` V.null

    describe "hover clipping (findElementAt — backs tooltip hover detection)" $ do
        it "does not return a row clipped out of view" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (_rowH, m3) = childAt containerH "row" (150, 10) (50, 20) hudH m2
            in findElementAt (170, 20) m3 `shouldBe` Nothing

        it "still returns a row that IS within the clip" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (rowH, m3) = childAt containerH "row" (10, 10) (50, 20) hudH m2
            in findElementAt (30, 20) m3 `shouldBe` Just rowH

    describe "wheel/scroll clipping (routeScroll)" $
        it "a scroll-capturing element outside the clip does not capture wheel input" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (eh, m3) = childAt containerH "panelBg" (150, 10) (50, 20) hudH m2
                m4 = setElementCapturesScroll eh True m3
            in routeScroll (170, 20) m4 `shouldBe` Nothing

    describe "floating root-mounted content escapes trigger ancestry" $
        it "a root-mounted popup positioned outside its trigger's tiny clip still hits normally" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_triggerH, m2) = containerAt "trigger" (0, 0) (50, 20) True hudH m1
                (popupH, m3) = rootHitAt "popup" (0, 20) (50, 80) "popupClick" hudH m2
            in routePointer PointerLeftClick (25, 60) m3 `shouldBe` RouteElement popupH "popupClick"

    describe "clip does not disturb z-order resolution" $
        it "among two overlapping clipped-in elements, the higher zIndex still wins" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (_loH, m3) = hitChildAt containerH "lo" (0, 0) (100, 100) "loClick" hudH m2
                (hiH, m4) = hitChildAt containerH "hi" (0, 0) (100, 100) "hiClick" hudH m3
                m5 = setElementZIndex hiH 10 m4
            in routePointer PointerLeftClick (50, 50) m5 `shouldBe` RouteElement hiH "hiClick"

    -- #747 Lua-facing coverage: the new opt-in and its effective-clip
    -- query can be configured/queried through the real UI API without
    -- any graphical engine (mirrors ElementInputPolicy.hs's pattern).
    around withHeadlessEngine $
        describe "Lua-facing UI API for clipping (#747)" $ do
            it "UI.setClipChildren / UI.isClipChildren round-trip through the real Lua UI API" $ \env → do
                ls ← newBareLuaBackend env
                before' ← evalDebug ls
                    "local pg = UI.newPage('clip_t1', 'hud'); \
                    \local el = UI.newElement('clip_e1', 10, 10, pg); \
                    \UI.addToPage(pg, el, 0, 0); \
                    \_G.__clipEl1 = el; \
                    \return UI.isClipChildren(el)"
                before' `shouldBe` "false"
                after' ← evalDebug ls
                    "UI.setClipChildren(_G.__clipEl1, true); return UI.isClipChildren(_G.__clipEl1)"
                after' `shouldBe` "true"

            it "UI.getEffectiveClip is nil when unclipped and reports the intersecting rect once a parent opts in" $ \env → do
                ls ← newBareLuaBackend env
                nilBefore ← evalDebug ls
                    "local pg = UI.newPage('clip_t2', 'hud'); \
                    \local parent = UI.newElement('clip_p2', 100, 100, pg); \
                    \UI.addToPage(pg, parent, 10, 10); \
                    \local child = UI.newElement('clip_c2', 20, 20, pg); \
                    \UI.addChild(parent, child, 5, 5); \
                    \_G.__clipParent2 = parent; _G.__clipChild2 = child; \
                    \return UI.getEffectiveClip(child) == nil"
                nilBefore `shouldBe` "true"
                clipAfter ← evalDebug ls
                    "UI.setClipChildren(_G.__clipParent2, true); \
                    \local c = UI.getEffectiveClip(_G.__clipChild2); \
                    \return c.x, c.y, c.w, c.h"
                clipAfter `shouldBe` "10.0\t10.0\t100.0\t100.0"

            it "a clipped-out element cannot be hit through UI.findElementAt" $ \env → do
                ls ← newBareLuaBackend env
                _ ← evalDebug ls
                    "local pg = UI.newPage('clip_t3', 'hud'); \
                    \local parent = UI.newElement('clip_p3', 100, 100, pg); \
                    \UI.addToPage(pg, parent, 0, 0); \
                    \UI.setClipChildren(parent, true); \
                    \local row = UI.newElement('clip_r3', 50, 20, pg); \
                    \UI.addChild(parent, row, 150, 10)"
                out ← evalDebug ls "return UI.findElementAt(170, 20)"
                out `shouldBe` "null"

    -- #747 adoption: the REAL scripts/ui/list.lua module ("Reusable
    -- list" — also reused by save_browser.lua/plant_panel.lua) now
    -- parents every visible slot under an opt-in clipping viewport
    -- (see list.lua's list.new). Drives the production module through
    -- the real Lua UI API (mirrors Test.Headless.UI.Slider's technique
    -- for scripts/ui/slider.lua) and proves the migrated region both
    -- exposes the correct effective clip AND that a row clipped out of
    -- view (simulated here by shrinking the viewport below its full
    -- slot stack, the exact "resize edge case" #747 is a safety net
    -- for) cannot be hit — while a still-visible row keeps working.
    around withHeadlessEngine $
        describe "scripts/ui/list.lua clip adoption (#747)" $
            it "exposes the list's effective clip and a row clipped out of view cannot be hit" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls listSetupLua
                setup `shouldNotSatisfy` isLuaError

                -- The viewport itself is a real opt-in clip container.
                evalDebug ls "return UI.isClipChildren(_G.__listViewportId)" ≫= (`shouldBe` "true")

                -- Each slot's effective clip is exactly the list's own
                -- bounds (x=100,y=100,width=200,height=3*20=60).
                clipBefore ← evalDebug ls
                    "local c = UI.getEffectiveClip(_G.__listHit1Id); return c.x, c.y, c.w, c.h"
                clipBefore `shouldBe` "100.0\t100.0\t200.0\t60.0"

                -- All three rows are hittable at their own centres.
                evalDebug ls "return UI.findElementAt(150, 110) == _G.__listHit1Id" ≫= (`shouldBe` "true")
                evalDebug ls "return UI.findElementAt(150, 130) == _G.__listHit2Id" ≫= (`shouldBe` "true")
                evalDebug ls "return UI.findElementAt(150, 150) == _G.__listHit3Id" ≫= (`shouldBe` "true")

                -- Shrink the viewport below its full slot stack (e.g. a
                -- caller resizing the list smaller without re-issuing
                -- list.setItems) — rows 2 and 3 fall outside the clip.
                _ ← evalDebug ls "UI.setSize(_G.__listViewportId, 200, 20)"

                clipAfter ← evalDebug ls
                    "local c = UI.getEffectiveClip(_G.__listHit2Id); return c.x, c.y, c.w, c.h"
                clipAfter `shouldBe` "100.0\t100.0\t200.0\t20.0"

                -- Row 1 (still within the shrunk clip) keeps hitting;
                -- rows 2 and 3 (now clipped out) cannot be hit at all.
                evalDebug ls "return UI.findElementAt(150, 110) == _G.__listHit1Id" ≫= (`shouldBe` "true")
                evalDebug ls "return UI.findElementAt(150, 130)" ≫= (`shouldBe` "null")
                evalDebug ls "return UI.findElementAt(150, 150)" ≫= (`shouldBe` "null")

    -- #857 review round 6: list.setVisible(id, false) hid every slot
    -- and the scrollbar but left the clipping viewport itself visible —
    -- a real, sized element, so it kept winning findElementAt/hover
    -- over whatever sat behind a "hidden" list. Proves the viewport now
    -- follows the list's own visibility, so a hidden list falls through
    -- to an underlying control, and showing it again restores its rows.
    around withHeadlessEngine $
        describe "scripts/ui/list.lua setVisible also hides its clipping viewport (#857)" $
            it "a hidden list stops intercepting hits; showing it again restores its rows" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls hiddenListSetupLua
                setup `shouldNotSatisfy` isLuaError

                -- Visible: the list's own row wins over the control
                -- positioned directly behind it.
                evalDebug ls "return UI.findElementAt(150, 110) == _G.__hlHit1Id" ≫= (`shouldBe` "true")

                -- Hidden: the viewport must no longer intercept the hit,
                -- so it falls through to the underlying control.
                _ ← evalDebug ls "require('scripts.ui.list').setVisible(_G.__hlListId, false)"
                evalDebug ls "return UI.findElementAt(150, 110) == _G.__hlUnderId" ≫= (`shouldBe` "true")

                -- Shown again: the row is back on top.
                _ ← evalDebug ls "require('scripts.ui.list').setVisible(_G.__hlListId, true)"
                evalDebug ls "return UI.findElementAt(150, 110) == _G.__hlHit1Id" ≫= (`shouldBe` "true")

    -- #747 review round 1 follow-up: the shared widget library gained
    -- (or, for checkbox/label/textbox/button, already had) an opt-in
    -- `parent` (panel.lua: `parentElement`, distinct from its existing
    -- panel-of-panel `parent`) so a caller CAN reparent a widget under
    -- a clipping viewport instead of the page root. Proves the real
    -- widgets actually attach as children (effective clip matches the
    -- container, not just "no crash"), and specifically regression-
    -- tests the two latent absolute-vs-relative-position bugs this
    -- uncovered: slider's drag-to-value mapping and randbox's
    -- click-outside bounds both used to assume their own stored
    -- x/y were framebuffer-absolute, which stops being true once
    -- parented — both now query the live (parent-aware) element
    -- position instead.
    around withHeadlessEngine $
        describe "widget library parent support (#747)" $ do
            it "dropdown/slider/randbox/sprite/panel all become real children of a clipping container" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls widgetParentSetupLua
                setup `shouldNotSatisfy` isLuaError

                let expectClip ∷ Text → IO ()
                    expectClip name = evalDebug ls
                        (T.concat ["local c = UI.getEffectiveClip(_G.", name, "); return c.x, c.y, c.w, c.h"])
                            ≫= (`shouldBe` "50.0\t50.0\t300.0\t200.0")
                expectClip "__wpDdHandle"
                expectClip "__wpSliderHandle"
                expectClip "__wpRbHandle"
                expectClip "__wpSpriteHandle"
                expectClip "__wpPanelHandle"

            it "slider drag-to-value mapping stays correct once parented (uses the live absolute track position)" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls widgetParentSetupLua
                setup `shouldNotSatisfy` isLuaError

                -- Container at (50,50); slider at relative (10,60) ⇒
                -- absolute (60,110); capWidth defaults to 8 ⇒ the
                -- track's absolute left edge is 50+10+8=68, trackWidth
                -- = 200 - 8*2 = 184. A pre-#747-style bug would use
                -- sl.trackX = 10+8=18 (the RELATIVE offset, missing the
                -- container's own 50,50) and badly mis-map every click.
                startDrag ← evalDebug ls
                    "local sl = require('scripts.ui.slider'); \
                    \sl.onTrackClick(_G.__wpSliderHandle); return 'ok'"
                startDrag `shouldBe` "\"ok\""

                atTrackStart ← evalDebug ls
                    "require('scripts.ui.slider').onDragMove(68, 110); \
                    \return require('scripts.ui.slider').getValue(_G.__wpSliderId)"
                atTrackStart `shouldBe` "0"

                atTrackEnd ← evalDebug ls
                    "require('scripts.ui.slider').onDragMove(68 + 184, 110); \
                    \return require('scripts.ui.slider').getValue(_G.__wpSliderId)"
                atTrackEnd `shouldBe` "100"

            it "randbox click-outside still uses the live absolute position once parented" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls widgetParentSetupLua
                setup `shouldNotSatisfy` isLuaError

                _ ← evalDebug ls "require('scripts.ui.randbox').focus(_G.__wpRbId)"
                isFocused ← evalDebug ls "return require('scripts.ui.randbox').isFocused(_G.__wpRbId)"
                isFocused `shouldBe` "true"

                -- Container at (50,50); randbox at relative (10,100) ⇒
                -- absolute (60,150). A click well inside that absolute
                -- box must NOT unfocus it; a pre-#747-style bug (using
                -- the raw relative rb.x/rb.y as if absolute) would
                -- wrongly treat this genuinely-inside click as outside.
                _ ← evalDebug ls "require('scripts.ui.randbox').onClickOutside(70, 160)"
                stillFocused ← evalDebug ls "return require('scripts.ui.randbox').isFocused(_G.__wpRbId)"
                stillFocused `shouldBe` "true"

                -- A click genuinely far away still unfocuses it.
                _ ← evalDebug ls "require('scripts.ui.randbox').onClickOutside(900, 900)"
                nowUnfocused ← evalDebug ls "return require('scripts.ui.randbox').isFocused(_G.__wpRbId)"
                nowUnfocused `shouldBe` "false"

-- * Wire-integration helpers (mirrors Test.Headless.UI.ElementInputPolicy)

-- | A real Lua backend with the FULL Lua API registered and nothing
--   else loaded — callers drive it with 'evalDebug' snippets calling
--   the real @UI.*@ functions, exactly as any script would.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console itself uses ('executeDebugLua').
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Creates a shown page, requires the real scripts/ui/list.lua
--   module, and builds a 5-item / 3-visible list at (100,100), then
--   resolves the viewport + first three slot hit-boxes by name into
--   globals the assertions above read (mirrors
--   Test.Headless.UI.Slider's sliderSetupLua — 'UI.getVisibleElements'
--   doesn't assume anything about handle numbering). font=0 is the
--   "unset" FontHandle sentinel; text still creates fine headless with
--   no font loaded, it just wouldn't render (no GPU here anyway).
listSetupLua ∷ Text
listSetupLua = T.concat
    [ "local page = UI.newPage('test_list_page', 'hud'); "
    , "UI.showPage(page); "
    , "local listMod = require('scripts.ui.list'); "
    , "listMod.init(); "
    , "local items = {}; "
    , "for i = 1, 5 do items[i] = { text = 'item' .. i, value = i } end; "
    , "_G.__listId = listMod.new({ name = 'test_list', x = 100, y = 100, "
    , "width = 200, itemHeight = 20, maxVisible = 3, items = items, "
    , "page = page, font = 0 }); "
    , "for _, e in ipairs(UI.getVisibleElements()) do "
    , "  if e.name == 'test_list_viewport' then _G.__listViewportId = e.handle end; "
    , "  if e.name == 'test_list_hit_1' then _G.__listHit1Id = e.handle end; "
    , "  if e.name == 'test_list_hit_2' then _G.__listHit2Id = e.handle end; "
    , "  if e.name == 'test_list_hit_3' then _G.__listHit3Id = e.handle end; "
    , "end"
    ]

-- | Like 'listSetupLua', but also places a plain clickable control
--   ("hlUnder") on the same page, directly behind the list's first
--   row, so hiding the list can be proven to actually fall through to
--   it (#857 review round 6) rather than merely asserting the viewport
--   itself returns something-or-other.
hiddenListSetupLua ∷ Text
hiddenListSetupLua = T.concat
    [ "local page = UI.newPage('test_hidden_list_page', 'hud'); "
    , "UI.showPage(page); "
    , "local under = UI.newElement('hl_under', 200, 60, page); "
    , "UI.addToPage(page, under, 100, 100); "
    , "UI.setClickable(under, true); "
    , "UI.setOnClick(under, 'underClick'); "
    , "_G.__hlUnderId = under; "
    , "local listMod = require('scripts.ui.list'); "
    , "listMod.init(); "
    , "local items = {}; "
    , "for i = 1, 5 do items[i] = { text = 'item' .. i, value = i } end; "
    , "_G.__hlListId = listMod.new({ name = 'hl_list', x = 100, y = 100, "
    , "width = 200, itemHeight = 20, maxVisible = 3, items = items, "
    , "page = page, font = 0 }); "
    , "for _, e in ipairs(UI.getVisibleElements()) do "
    , "  if e.name == 'hl_list_hit_1' then _G.__hlHit1Id = e.handle end; "
    , "end"
    ]

-- | Builds a 300x200 clipping container at (50,50) — deliberately
--   NOT at the origin, so a widget that wrongly treats its own stored
--   (relative) x/y as framebuffer-absolute would be caught rather than
--   coincidentally passing — then creates one instance of each widget
--   that gained #747 parent support, parented to that container.
widgetParentSetupLua ∷ Text
widgetParentSetupLua = T.concat
    [ "local page = UI.newPage('test_wp_page', 'hud'); "
    , "UI.showPage(page); "
    , "local container = UI.newElement('wp_container', 300, 200, page); "
    , "UI.addToPage(page, container, 50, 50); "
    , "UI.setClipChildren(container, true); "
    , "_G.__wpContainer = container; "

    , "local dd = require('scripts.ui.dropdown'); dd.init(); "
    , "_G.__wpDdId = dd.new({ name = 'wp_dd', x = 10, y = 10, page = page, "
    , "font = 0, parent = container, options = { { text = 'A', value = 'a' } } }); "
    , "_G.__wpDdHandle = dd.getElementHandle(_G.__wpDdId); "

    , "local sl = require('scripts.ui.slider'); sl.init(); "
    , "_G.__wpSliderId = sl.new({ name = 'wp_slider', x = 10, y = 60, page = page, "
    , "parent = container, min = 0, max = 100, default = 50 }); "
    , "_G.__wpSliderHandle = sl.getElementHandle(_G.__wpSliderId); "

    , "local rb = require('scripts.ui.randbox'); rb.init(); "
    , "_G.__wpRbId = rb.new({ name = 'wp_rb', x = 10, y = 100, page = page, "
    , "font = 0, parent = container, default = 'seed' }); "
    , "_G.__wpRbHandle = rb.getElementHandle(_G.__wpRbId); "

    , "local sp = require('scripts.ui.sprite'); "
    , "_G.__wpSpriteId = sp.new({ name = 'wp_sprite', x = 10, y = 140, page = page, "
    , "parent = container, texture = 0, width = 20, height = 20 }); "
    , "_G.__wpSpriteHandle = sp.getElementHandle(_G.__wpSpriteId); "

    , "local pn = require('scripts.ui.panel'); "
    , "_G.__wpPanelId = pn.new({ name = 'wp_panel', x = 10, y = 160, page = page, "
    , "parentElement = container, width = 50, height = 30, textureSet = 0 }); "
    , "_G.__wpPanelHandle = pn.getBoxHandle(_G.__wpPanelId)"
    ]
