{-# LANGUAGE Strict #-}
-- | The tree-anchored designation marker (#1856, D-3 / D-12).
--
-- A committed Chop designation annotates the TREE, not its tile: one
-- alpha-bearing icon horizontally centred on the sprite's rendered
-- ground-contact anchor, sitting immediately above that point near the
-- sprite's bottom centre. It replaces the flat full-tile ground overlay
-- the two-click rectangle drew, which could not distinguish two
-- wood-tagged co-tenants on one tile and drifted from the sprite
-- wherever elevation, sub-tile offset or sprite geometry moved a tree
-- off its tile's centre.
--
-- The anchor comes from the SAME
-- 'World.Render.FloraProjection.floraAnchor' the selection oracle
-- tests, so what the player boxed is exactly what ends up marked.
--
-- Like the flora sprite itself the icon carries 'noFaceMapVertexId':
-- it is a screen-facing annotation, not a terrain surface, so the
-- three-face isometric mask must not touch it and its authored alpha
-- owns the whole shape.
module World.Render.FloraMarker
    ( floraMarkerQuad
    ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Vulkan.Types.Vertex
    (Vec2(..), Vec4(..), mkVertexWorld, tileWorldUV, noFaceMapVertexId)
import World.Grid (tileWidth, tileHeight, worldLayer, baseTileW, baseTileH)
import World.Render.FloraProjection (FloraGeom(..))

-- | One designated tree's marker quad, placed off that tree's own
--   'fgAnchorX' \/ 'fgAnchorY'.
--
--   Sized from the ICON's pixel dimensions on the same
--   pixels→world scale flora sprites use, so an icon authored at any
--   size lands at its intended footprint. It sits with its BOTTOM edge
--   on the anchor — \"immediately above\" the ground contact — and its
--   painter depth is a hair beyond the tree's own, so the annotation
--   draws over the trunk it belongs to and never behind it.
floraMarkerQuad
    ∷ (TextureHandle → Int)  -- ^ handle → shader slot
    → FloraGeom              -- ^ the tree's own projected geometry
    → (Float, Float)         -- ^ icon pixel size
    → Float                  -- ^ tile alpha (the whole-layer zoom fade)
    → Int → Int              -- ^ the tree's global tile x, y (world UV)
    → TextureHandle          -- ^ icon texture
    → SortableQuad
floraMarkerQuad lookupSlot geom (iconW, iconH) tileAlpha gx gy tex =
    let quadW = tileWidth  * (iconW / baseTileW)
        quadH = tileHeight * (iconH / baseTileH)
        drawX = fgAnchorX geom - quadW * 0.5
        drawY = fgAnchorY geom - quadH
        slot  = fromIntegral (lookupSlot tex)
        tint  = Vec4 1.0 1.0 1.0 tileAlpha
        wuv   = tileWorldUV gx gy
        -- The tree's own depth plus one tie-break step: the marker is
        -- part of that sprite's annotation, so it must sort with it
        -- rather than at its tile's generic depth.
        sortKey = fgSortKey geom + 0.00001
        v uvx uvy px py = mkVertexWorld wuv (Vec2 px py) (Vec2 uvx uvy)
                              tint slot noFaceMapVertexId
    in SortableQuad
        { sqSortKey = sortKey
        , sqV0      = v 0 0 drawX drawY
        , sqV1      = v 1 0 (drawX + quadW) drawY
        , sqV2      = v 1 1 (drawX + quadW) (drawY + quadH)
        , sqV3      = v 0 1 drawX (drawY + quadH)
        , sqTexture = tex
        , sqLayer   = worldLayer
        }
