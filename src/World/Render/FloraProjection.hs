{-# LANGUAGE Strict #-}
-- | The ONE shared flora projection boundary (#1856).
--
-- Every consumer that needs to know where a flora sprite is on screen —
-- the renderer that draws it, the selection oracle that picks it, and
-- the designation marker that annotates it — derives that geometry
-- HERE, from the same inputs, rather than recomputing it. Chop's
-- drag-box selection is screen-space (D-9): what the player boxes is
-- what gets designated, so a picker that recomputed the projection
-- would drift from the painter at exactly the places the rule exists
-- for — cliffs, sub-tile offsets and sprite geometry all move a tree's
-- rendered position away from its tile's.
--
-- The same discipline 'Unit.HitTest.unitHitRect' holds for units: size
-- and place the hit box from the frame the renderer is DRAWING, never
-- from an independently derived value.
--
-- == What is texture-dependent and what is not
--
-- The ground-contact anchor is deliberately texture-INDEPENDENT. Solve
-- 'floraQuadRect'\'s @drawY@ for the anchor and @quadH@ cancels:
--
-- > anchorY = drawY + quadH - baseRadius
-- >         = rawY + wrapY - heightOffset + subY + tileHalfDiamondHeight
--
-- and likewise @anchorX = drawX + quadW * 0.5@ loses @quadW@. That is
-- what lets the marker and a box selection agree without either of them
-- resolving a growth-stage texture — while CLICK selection, which tests
-- the rendered quad's bounds, still needs the real cell size and gets it
-- from the same 'floraQuadRect' the renderer uses.
module World.Render.FloraProjection
    ( FloraGeom(..)
    , floraTexSize
    , floraQuadRect
    , floraAnchor
    , floraSortKey
    , floraGeom
    , floraVisibleInSlice
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (CameraFacing(..))
import World.Grid (gridToScreen, tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , applyFacing, baseTileW, baseTileH)
import World.Flora.Types (FloraInstance(..))

-- | One flora instance's fully derived screen geometry: the quad the
--   renderer emits, the ground-contact anchor selection and markers
--   share, and the painter depth that decides which of two overlapping
--   sprites is on top.
data FloraGeom = FloraGeom
    { fgDrawX   ∷ !Float  -- ^ quad left edge (world units)
    , fgDrawY   ∷ !Float  -- ^ quad top edge (world units)
    , fgQuadW   ∷ !Float  -- ^ quad width
    , fgQuadH   ∷ !Float  -- ^ quad height
    , fgAnchorX ∷ !Float  -- ^ ground-contact x (quad's horizontal centre)
    , fgAnchorY ∷ !Float  -- ^ ground-contact y (trunk base centre)
    , fgSortKey ∷ !Float  -- ^ painter depth, identical to 'SortableQuad'
    , fgTexture ∷ !TextureHandle
      -- ^ The frame the sprite is DRAWING — the growth stage or the
      --   depleted swap 'World.Render.FloraDraws' resolved, never the
      --   species' base art. Carried here because it is part of the
      --   scene's painter order
      --   ('Engine.Scene.Types.Batch.quadPainterOrder'), which a picker
      --   has to reconstruct in full.
    } deriving (Show, Eq)

-- | The instance's texture pixel dimensions, defaulting to one tile
--   when the size is not known yet — the SAME lookup and the SAME
--   default 'World.Render.FloraQuads.floraToQuad' applies, so a picker
--   sizing a quad before its texture upload resolves agrees with the
--   renderer drawing that same frame.
floraTexSize
    ∷ HM.HashMap TextureHandle (Int, Int) → TextureHandle → (Float, Float)
floraTexSize texSizes texHandle = case HM.lookup texHandle texSizes of
    Just (w, h) → (fromIntegral w, fromIntegral h)
    Nothing     → (baseTileW, baseTileH)

-- | The sprite quad as @(drawX, drawY, quadW, quadH)@.
--
--   'fiZ' is the LIVE surface z the renderer places the sprite at
--   ('World.Render.Quads' overwrites it with @findTopSolid@ before
--   drawing), not the z stored in any designation record — a picker
--   reading a stale z would sit a tile-side-height away from the sprite
--   on any column that has since changed, which is the elevation half
--   of the drift D-9 rejects.
floraQuadRect
    ∷ CameraFacing
    → Int → Int          -- ^ global tile x, y
    → FloraInstance
    → (Float, Float)     -- ^ texture pixel size
    → Int                -- ^ camera z-slice
    → (Float, Float)     -- ^ wrap offset (#1176)
    → (Float, Float, Float, Float)
floraQuadRect facing gx gy inst (texW, texH) zSlice (wrapX, wrapY) =
    let scaleX = texW / baseTileW
        scaleY = texH / baseTileH
        quadW  = tileWidth  * scaleX
        quadH  = tileHeight * scaleY
        (ax, ay) = floraAnchor facing gx gy inst zSlice (wrapX, wrapY)
        baseRadius = floraBaseRadius inst
        drawX = ax - quadW * 0.5
        drawY = ay - quadH + baseRadius
    in (drawX, drawY, quadW, quadH)

-- | The rendered ground-contact point: horizontally the sprite's
--   centre, vertically the trunk-base circle's centre. This is the
--   point D-12 anchors the committed marker to and the point a
--   drag-box tests for containment.
floraAnchor
    ∷ CameraFacing
    → Int → Int
    → FloraInstance
    → Int
    → (Float, Float)
    → (Float, Float)
floraAnchor facing gx gy inst zSlice (wrapX, wrapY) =
    let (rawX, rawY) = gridToScreen facing gx gy
        relativeZ    = fiZ inst - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        subX = (fiOffU inst - fiOffV inst) * tileHalfWidth
        subY = (fiOffU inst + fiOffV inst) * tileHalfDiamondHeight
    in ( rawX + wrapX + subX + tileWidth * 0.5
       , rawY + wrapY - heightOffset + subY + tileHalfDiamondHeight )

-- | Trunk-base radius in world units — the offset between the quad's
--   bottom edge and its ground contact.
floraBaseRadius ∷ FloraInstance → Float
floraBaseRadius inst = fiBaseWidth inst * 0.5 / baseTileH * tileHeight

-- | Painter depth. Bit-identical to the @sqSortKey@
--   'World.Render.FloraQuads.floraToQuad' stamps on the quad, so
--   "topmost" for a picker means the sprite the renderer actually drew
--   last. Ties are broken by the caller on the stable instance id, so
--   two co-tenants at one depth still resolve deterministically.
floraSortKey ∷ CameraFacing → Int → Int → FloraInstance → Int → Float
floraSortKey facing gx gy inst zSlice =
    let (fa, fb) = applyFacing facing gx gy
        relativeZ = fiZ inst - zSlice
    in fromIntegral (fa + fb)
     + fromIntegral relativeZ * 0.001
     + 0.0003
     + fiOffV inst * 0.00005

-- | Everything at once, for the consumers that need more than one part.
floraGeom
    ∷ CameraFacing
    → Int → Int
    → FloraInstance
    → TextureHandle
    → HM.HashMap TextureHandle (Int, Int)
    → Int
    → (Float, Float)
    → FloraGeom
floraGeom facing gx gy inst tex texSizes zSlice wrapOff =
    let texSize = floraTexSize texSizes tex
        (drawX, drawY, quadW, quadH) =
            floraQuadRect facing gx gy inst texSize zSlice wrapOff
        (ax, ay) = floraAnchor facing gx gy inst zSlice wrapOff
    in FloraGeom
        { fgDrawX   = drawX
        , fgDrawY   = drawY
        , fgQuadW   = quadW
        , fgQuadH   = quadH
        , fgAnchorX = ax
        , fgAnchorY = ay
        , fgSortKey = floraSortKey facing gx gy inst zSlice
        , fgTexture = tex
        }

-- | The renderer's own z-slice cull ('floraToQuad' returns Nothing
--   outside it). A picker must apply the SAME band or it selects trees
--   the player cannot see.
floraVisibleInSlice ∷ Int → Int → FloraInstance → Bool
floraVisibleInSlice zSlice effDepth inst =
    let floraZ = fiZ inst
    in floraZ ≤ zSlice ∧ floraZ ≥ (zSlice - effDepth)
