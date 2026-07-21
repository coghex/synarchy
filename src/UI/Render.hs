{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Render
  ( renderUIPages
  , uiLayerToLayerId
  , clipGlyphInstance
  , makeBoxBatches
  , renderSpriteBatch
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Text as T
import Data.List (sortOn)
import Data.IORef (readIORef)
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Core.Monad
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logWarnM)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Font.Data (FontCache(..), fcFonts, GlyphInstance(..))
import Engine.Graphics.Font.Draw (layoutTextUI)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..), mkVertex)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types.Batch (RenderBatch(..), RenderItem(..), TextRenderBatch(..))
import UI.Types
import UI.Clipping (ClipRect, effectiveClip, clipQuadUV, boxTileRects, BoxTile(..))
import UI.InteractiveBounds (elementOverflow)
import UI.Manager (getVisiblePages, getElementAbsolutePosition, getBoxTextureSet)
import World.Grid (uiLayerThreshold)

-- | Base layer offset for UI (world uses the layers below it). Derived
--   from 'World.Grid.uiLayerThreshold' — the same value that drives
--   'Engine.Scene.Batch.Visibility.isUILayer' and the scene render
--   split — so the two pipelines can't disagree about where UI starts.
uiLayerBase ∷ Int
uiLayerBase = let LayerId t = uiLayerThreshold in fromIntegral t

unLayerId ∷ LayerId → Word32
unLayerId (LayerId i) = fromIntegral i

-- | Convert UI layer to render LayerId
--
--   The element renderer adds @ueZIndex@ on top of the page's band,
--   so bands must be spaced far enough apart that one layer's highest
--   element can't outrank the next layer's base. The band values live
--   in 'UI.Types.uiLayerBand' — shared with hit-testing in
--   "UI.Manager" so clicks/hover resolve to exactly what's painted
--   on top.
uiLayerToLayerId ∷ UILayer → Int → LayerId
uiLayerToLayerId layer zIndex = LayerId $ fromIntegral $
    uiLayerBase + uiLayerBand layer zIndex

-- | Bake the STABLE texture-handle id (#286). The bindless UI fragment
--   shader resolves it to a live slot at draw time via the handle→slot
--   table, exactly like the world path — so UI quads can't encode a stale
--   slot. (Was: resolve to the bindless slot here, which now mismatches
--   the shader and sampled the wrong texture.) Never actually depended
--   on the 'BindlessTextureSystem' itself (only ever called 'toInt' on
--   the handle) — #747 review round 2 drops that dead parameter so
--   'makeBoxBatches'/'renderSpriteBatch' are genuinely pure and
--   directly Hspec-testable, not just the clip geometry they call.
lookupTextureSlot ∷ TextureHandle → Float
lookupTextureSlot texHandle = fromIntegral (toInt texHandle)

mergeLayeredTextItems ∷ Map.Map LayerId (V.Vector RenderItem)
                      → Map.Map LayerId (V.Vector RenderItem)
mergeLayeredTextItems = Map.map mergeInLayer
  where
    mergeInLayer ∷ V.Vector RenderItem → V.Vector RenderItem
    mergeInLayer items =
        let sprites = V.filter isSprite items
            texts   = [t | TextItem t ← V.toList items]
            merged  = mergeTextBatches texts
        in sprites <> V.fromList (map TextItem merged)

    isSprite (SpriteItem _) = True
    isSprite _              = False

    mergeTextBatches ∷ [TextRenderBatch] → [TextRenderBatch]
    mergeTextBatches [] = []
    mergeTextBatches batches@(first:_) =
        let grouped = Map.toList $ foldl' (\acc b →
                let key = trbFont b
                    existing = Map.findWithDefault V.empty key acc
                in Map.insert key (existing <> trbInstances b) acc
              ) Map.empty batches
            layer = trbLayer first
        in [ TextRenderBatch
               { trbFont      = font
               , trbLayer     = layer
               , trbInstances = insts
               , trbObjects   = V.empty
               }
           | (font, insts) ← grouped
           ]

-- | Render all visible UI pages
renderUIPages ∷ EngineM ε σ (V.Vector RenderBatch, Map.Map LayerId (V.Vector RenderItem))
renderUIPages = do
    env ← ask
    mgr ← liftIO $ readIORef (uiManagerRef env)
    
    maybeBindless ← liftIO $ readIORef (textureSystemRef env)

    case maybeBindless of
        Nothing → do
            logWarnM CatUI "No bindless texture system available for UI rendering"
            pure (V.empty, Map.empty)
        Just bindless → do
            logDebugM CatUI $ "UI rendering with bindless texture system containing " <> T.pack (show $ Map.size (btsHandleMap bindless)) <> " textures"
            fontCache ← liftIO $ readIORef (fontCacheRef env)

            let visiblePages = getVisiblePages mgr

            results ← forM visiblePages $ \page →
                renderPage mgr fontCache page

            let allBatches = V.concat $ map fst results
                allLayered = foldr (Map.unionWith (<>)) Map.empty (map snd results)
                -- Merge text batches that share a font within each layer
                mergedLayered = mergeLayeredTextItems allLayered
            
            pure (allBatches, mergedLayered)

-- | Render a single page
renderPage ∷ UIPageManager → FontCache → UIPage
           → EngineM ε σ (V.Vector RenderBatch, Map.Map LayerId (V.Vector RenderItem))
renderPage mgr fontCache page = do
    let layerId = uiLayerToLayerId (upLayer page) (upZIndex page)
        rootElems = upRootElements page

    results ← forM rootElems $ \elemHandle →
        renderElement mgr fontCache layerId elemHandle

    let allBatches = V.concat $ map fst results
        allLayered = foldr (Map.unionWith (<>)) Map.empty (map snd results)

    pure (allBatches, allLayered)

renderElement ∷ UIPageManager → FontCache
              → LayerId → ElementHandle
              → EngineM ε σ (V.Vector RenderBatch, Map.Map LayerId (V.Vector RenderItem))
renderElement mgr fontCache baseLayerId handle = do
    case Map.lookup handle (upmElements mgr) of
        Nothing → pure (V.empty, Map.empty)
        Just elem
            | not (ueVisible elem) → pure (V.empty, Map.empty)
            | otherwise → do
                let (absX, absY) = case getElementAbsolutePosition handle mgr of
                        Just pos → pos
                        Nothing  → (0, 0)
                    -- #747: the clip THIS element is subject to, from its
                    -- own ancestors' 'ueClipChildren' opt-ins — the same
                    -- shared helper 'isPointInElement' consults for
                    -- hit-testing, so paint and hit-test can't drift apart.
                    clip = effectiveClip handle mgr

                let elemLayerId = LayerId $ unLayerId baseLayerId + fromIntegral (ueZIndex elem)

                (selfBatches, selfItems) ← renderElementData mgr fontCache
                                              elemLayerId elem absX absY clip

                let sortedChildren = sortOn (getChildZIndex mgr) (ueChildren elem)
                childResults ← forM sortedChildren $ \childHandle →
                    renderElement mgr fontCache elemLayerId childHandle

                let childBatches = V.concat $ map fst childResults
                    childLayered = foldr (Map.unionWith (<>)) Map.empty (map snd childResults)
                
                let allBatches = selfBatches <> childBatches
                    allLayered = if V.null selfItems
                                 then childLayered
                                 else Map.unionWith (<>) 
                                        (Map.singleton elemLayerId selfItems) 
                                        childLayered
                
                pure (allBatches, allLayered)

getChildZIndex ∷ UIPageManager → ElementHandle → Int
getChildZIndex mgr handle = 
    case Map.lookup handle (upmElements mgr) of
        Nothing → 0
        Just elem → ueZIndex elem

renderElementData ∷ UIPageManager → FontCache
                  → LayerId → UIElement → Float → Float → Maybe ClipRect
                  → EngineM ε σ (V.Vector RenderBatch, V.Vector RenderItem)
renderElementData mgr fontCache layerId elem absX absY clip =
    case ueRenderData elem of
        RenderNone → pure (V.empty, V.empty)

        RenderBox style → do
            case getBoxTextureSet (ubsTextures style) mgr of
                Nothing → do
                    logWarnM CatUI "UI box texture set not found"
                    pure (V.empty, V.empty)
                Just texSet → do
                    let (w, h) = ueSize elem
                        tileSize = ubsTileSize style
                        color = ubsColor style
                        -- #749: the clamped, size-aware overflow (the
                        -- ONE shared expansion both this render path and
                        -- 'UI.Manager.Query.isPointInElement' consult, so
                        -- visual and interactive bounds can't drift). The
                        -- element's LOGICAL position/size ('uePosition'/
                        -- 'ueSize') is unchanged — this only expands what
                        -- draws. Hit-testing uses this SAME expanded rect
                        -- only when the box opts its border in via
                        -- 'ueInteractiveOverflow'; a decorative box still
                        -- hit-tests content-only (overflow never creates
                        -- a target on its own).
                        overflow = elementOverflow elem
                        vx = absX - overflow
                        vy = absY - overflow
                        vw = w + overflow * 2
                        vh = h + overflow * 2
                        batches = makeBoxBatches texSet vx vy vw vh tileSize color layerId clip
                        items = V.map SpriteItem batches
                    pure (batches, items)

        RenderText style → do
            let fontHandle = utsFont style
            case Map.lookup fontHandle (fcFonts fontCache) of
                Nothing → do
                    logWarnM CatUI $ "Font cache miss: UI text font not found: " <> (T.pack (show fontHandle))
                    pure (V.empty, V.empty)
                Just atlas → do
                    logDebugM CatFont $ "Font cache hit: Found UI font " <> T.pack (show fontHandle)
                    let text = utsText style
                        (cr, cg, cb, ca) = utsColor style
                        color = (cr, cg, cb, ca)
                        size = utsSize style
                        rawInstances = layoutTextUI atlas size absX absY text color
                        -- #747: clip each glyph quad independently — a
                        -- glyph straddling the clip boundary shows only
                        -- its visible slice, and a glyph fully outside
                        -- the clip is dropped entirely.
                        instances = V.mapMaybe (clipGlyphInstance clip) rawInstances

                    logDebugM CatFont $ "UI text layout generated " <> T.pack (show $ V.length instances) <> " vertices"

                    if V.null instances
                        then pure (V.empty, V.empty)
                        else do
                            let textBatch = TextRenderBatch
                                    { trbFont      = fontHandle
                                    , trbLayer     = layerId
                                    , trbInstances = instances
                                    , trbObjects   = V.empty
                                    }
                            pure (V.empty, V.singleton (TextItem textBatch))

        RenderSprite style → do
            let (w, h) = ueSize elem
                (batches, items) = renderSpriteBatch (ussTexture style) (ussColor style)
                                       absX absY w h layerId clip
            pure (batches, items)

-- | Pure: a sprite element's render batch, clipped. This is the ACTUAL
--   function 'renderElementData''s 'RenderSprite' branch calls (#747
--   review round 2) — not just the underlying 'clipQuadUV' helper in
--   isolation — so a test exercising it gates real call-site wiring:
--   removing the clip here would show up as a batch/vertex mismatch,
--   not just a passing geometry-only test. 'Nothing'-equivalent (empty
--   vectors) when the clip excludes the sprite entirely.
renderSpriteBatch ∷ TextureHandle → (Float, Float, Float, Float)
                  → Float → Float → Float → Float → LayerId → Maybe ClipRect
                  → (V.Vector RenderBatch, V.Vector RenderItem)
renderSpriteBatch tex color absX absY w h layerId clip =
    case clipQuadUV clip (absX, absY, w, h) (0, 0, 1, 1) of
        Nothing → (V.empty, V.empty)
        Just ((cx, cy, cw, ch), uv) →
            let atlasId = lookupTextureSlot tex
                vertices = makeQuadVertices cx cy cw ch color atlasId uv
                batch = RenderBatch
                    { rbTexture  = tex
                    , rbLayer    = layerId
                    , rbVertices = vertices
                    , rbObjects  = V.empty
                    , rbDirty    = True
                    , rbAvgZ     = 0.0
                    }
            in (V.singleton batch, V.singleton (SpriteItem batch))

-- | Clip one glyph instance's quad + UV rect against the effective
--   clip; 'Nothing' drops a glyph that falls entirely outside it.
clipGlyphInstance ∷ Maybe ClipRect → GlyphInstance → Maybe GlyphInstance
clipGlyphInstance clip gi =
    let (px, py) = instancePosition gi
        (w, h)   = instanceSize gi
    in case clipQuadUV clip (px, py, w, h) (instanceUVRect gi) of
        Nothing → Nothing
        Just ((cx, cy, cw, ch), uv) →
            Just gi { instancePosition = (cx, cy)
                    , instanceSize     = (cw, ch)
                    , instanceUVRect   = uv
                    }

-- | The texture for one of the nine 3x3 box tiles.
tileTexture ∷ BoxTextureSet → BoxTile → TextureHandle
tileTexture texSet tile = case tile of
    TileNW     → btsNW texSet
    TileN      → btsN texSet
    TileNE     → btsNE texSet
    TileW      → btsW texSet
    TileCenter → btsCenter texSet
    TileE      → btsE texSet
    TileSW     → btsSW texSet
    TileS      → btsS texSet
    TileSE     → btsSE texSet

-- | Pure: the nine box-tile render batches, clipped. This is the
--   ACTUAL function 'renderElementData''s 'RenderBox' branch calls
--   (#747 review round 2) — not just 'UI.Clipping.boxTileRects' in
--   isolation — so a test exercising it gates real call-site wiring:
--   removing the clip here would show up as a batch/vertex mismatch
--   (wrong count, wrong rect, wrong UV), not just a passing
--   geometry-only test. 'UI.Clipping.boxTileRects' supplies the
--   already-clipped per-tile geometry (a tile fully outside the
--   effective clip is omitted, one straddling the boundary keeps only
--   its visible rect + UV slice); this is pure glue turning each
--   surviving tile into a RenderBatch.
makeBoxBatches ∷ BoxTextureSet
               → Float → Float → Float → Float → Float
               → (Float, Float, Float, Float) → LayerId → Maybe ClipRect
               → V.Vector RenderBatch
makeBoxBatches texSet x y w h tileSize color layerId clip =
    V.fromList $ map toBatch (boxTileRects x y w h tileSize clip)
  where
    toBatch (tile, (cx, cy, cw, ch), uv) =
        let tex = tileTexture texSet tile
            atlasId = lookupTextureSlot tex
            vertices = makeQuadVertices cx cy cw ch color atlasId uv
        in RenderBatch
            { rbTexture  = tex
            , rbLayer    = layerId
            , rbVertices = vertices
            , rbObjects  = V.empty
            , rbDirty    = True
            , rbAvgZ     = 0.0
            }

-- | Generate quad vertices for a UI element. The UV rect is a
--   parameter (rather than always the full 0..1 texture) so a
--   #747-clipped partial quad can sample only its visible texture
--   slice instead of stretching the whole texture across a smaller
--   screen rect — every caller passes @(0, 0, 1, 1)@ through
--   'UI.Clipping.clipQuadUV', which returns that unchanged when there
--   is no clip in effect.
--   faceMapId is always 0 because the UI pipeline ignores face-map lighting.
makeQuadVertices ∷ Float → Float → Float → Float
                 → (Float, Float, Float, Float)
                 → Float
                 → (Float, Float, Float, Float)
                 → VS.Vector Vertex
makeQuadVertices x y w h (cr, cg, cb, ca) atlasId (u0, v0, u1, v1) =
    let x0 = x
        y0 = y
        x1 = x + w
        y1 = y + h

        col = Vec4 cr cg cb ca
        fmId = 0

        vtx1 = mkVertex (Vec2 x0 y0) (Vec2 u0 v0) col atlasId fmId
        vtx2 = mkVertex (Vec2 x1 y0) (Vec2 u1 v0) col atlasId fmId
        vtx3 = mkVertex (Vec2 x0 y1) (Vec2 u0 v1) col atlasId fmId
        vtx4 = mkVertex (Vec2 x1 y0) (Vec2 u1 v0) col atlasId fmId
        vtx5 = mkVertex (Vec2 x1 y1) (Vec2 u1 v1) col atlasId fmId
        vtx6 = mkVertex (Vec2 x0 y1) (Vec2 u0 v1) col atlasId fmId
    in VS.fromList [vtx1, vtx2, vtx3, vtx4, vtx5, vtx6]
