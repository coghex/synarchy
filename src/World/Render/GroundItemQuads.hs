{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-frame quads + hit-testing for items lying in the world
--   (Item.Ground).
--
--   Ground items store only float (x, y); their resting height is
--   derived HERE from the current terrain surface — so an item over a
--   freshly dug tile drops with the terrain on the very next frame,
--   an item on a sloped tile sits on the incline via bilinear
--   interpolation of the slope mask's corner drops, and an item in
--   water sinks to the bed (terrain surface, not fluid surface) with
--   the same underwater tint terrain gets.
--
--   The selected item (cursor state's selectedGroundItem) renders
--   with 'renderFlagSelected' — the same shader outline units use.
module World.Render.GroundItemQuads
    ( renderGroundItemQuads
    , hitTestGroundItemAt
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv, itemManagerRef)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Asset.YamlTextures (lookupTextureName)
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing)
import Engine.Graphics.Viewport (windowDegenerate)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..)
                                           , renderFlagSelected, packWorldUV)
import Engine.Scene.Types (SortableQuad(..))
import Item.Ground (GroundItem(..), GroundItems(..))
import Item.Types (ItemManager(..), ItemDef(..), ItemInstance(..))
import World.Generate (viewDepth)
import World.Generate.Coordinates (globalToChunk)
import World.Grid (gridToScreen, tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , worldLayer, applyFacing, GridConfig(..)
                  , defaultGridConfig)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.ViewBounds (computeViewBounds)
import World.Types

baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)

-- | Ground-render scale for item sprites. Item textures are 32×32
--   inventory icons drawn edge-to-edge; at raw pixel ratio they'd
--   render a third of a tile wide — two thirds of an acolyte (48 px
--   with padding). Halving reads as hand-tool-sized next to a unit.
--   Applied in itemGeometry so rendering and click hit-testing stay
--   in agreement.
groundItemScale ∷ Float
groundItemScale = 0.5

-- | How far (0 or 1 z) each corner of a tile is lowered, from the
--   slope-id edge mask (N=1, E=2, S=4, W=8 = slopes down that way).
--   A corner is lowered when either adjacent edge slopes — the same
--   rule as World.Mine.Types.designationFromSlope.
cornerDrops ∷ Word8 → (Float, Float, Float, Float)
cornerDrops mask =
    let eN = testBit mask 0
        eE = testBit mask 1
        eS = testBit mask 2
        eW = testBit mask 3
        d b = if b then 1.0 else 0.0
    in ( d (eN ∨ eW)   -- NW
       , d (eN ∨ eE)   -- NE
       , d (eS ∨ eE)   -- SE
       , d (eS ∨ eW)   -- SW
       )

-- | Bilinear corner-drop at a fractional in-tile position (0..1).
dropAt ∷ Word8 → Float → Float → Float
dropAt mask fx fy =
    let (dNW, dNE, dSE, dSW) = cornerDrops mask
    in (1 - fx) * (1 - fy) * dNW
     + fx       * (1 - fy) * dNE
     + fx       * fy       * dSE
     + (1 - fx) * fy       * dSW

-- | Shared per-item geometry: terrain z, screen quad rect, and how
--   deep underwater the resting point is. Nothing when the chunk
--   isn't loaded or the item def is unknown.
itemGeometry ∷ WorldTileData → ItemManager
             → HM.HashMap TextureHandle (Int, Int)
             → CameraFacing → Int
             → GroundItem
             → Maybe (Int, TextureHandle, Float, Float, Float, Float, Int)
               -- ^ (tz, tex, drawX, drawY, quadW, quadH, underwaterDepth)
itemGeometry tileData im texSizes facing zSlice gi = do
    let tx = floor (giX gi) ∷ Int
        ty = floor (giY gi) ∷ Int
        (chunkCoord, (lx, ly)) = globalToChunk tx ty
    lc ← HM.lookup chunkCoord (wtdChunks tileData)
    itemDef ← HM.lookup (iiDefName (giInst gi)) (imDefs im)
    let idx = columnIndex lx ly
        tz  = lcTerrainSurfaceMap lc VU.! idx
        col = lcTiles lc V.! idx
        si  = tz - ctStartZ col
        slopeMask = if si ≥ 0 ∧ si < VU.length (ctSlopes col)
                    then ctSlopes col VU.! si
                    else 0
        fx = giX gi - fromIntegral tx
        fy = giY gi - fromIntegral ty
        drop' = dropAt slopeMask fx fy

        underwaterDepth = case lcFluidMap lc V.! idx of
            Just fc | tz < fcSurface fc → fcSurface fc - tz
            _ → 0

        texHandle = idTexture itemDef
        (texW, texH) = case HM.lookup texHandle texSizes of
            Just (w, h) → (fromIntegral w, fromIntegral h)
            Nothing     → (32.0, 32.0)
        quadW = tileWidth  * (texW / baseTileW) * groundItemScale
        quadH = tileHeight * (texH / baseTileH) * groundItemScale

        relativeZ = tz - zSlice
        (rawX, rawY) = gridToScreen facing tx ty
        heightOffset = fromIntegral relativeZ * tileSideHeight
                     - drop' * tileSideHeight

        offU = fx - 0.5
        offV = fy - 0.5
        subX = (offU - offV) * tileHalfWidth
        subY = (offU + offV) * tileHalfDiamondHeight

        drawX = rawX + subX + (tileWidth - quadW) * 0.5
        -- Bottom-anchor at the tile diamond center, lifted 2 sprite
        -- PIXELS so flat items don't z-fight the tile top. The nudge
        -- must be converted to world units like flora's baseRadius —
        -- a bare "+ 2.0" here is two full screen-heights and
        -- teleports every item far below its tile (the GUI-only
        -- invisible-items bug, 2026-06-12).
        nudge = 2.0 / baseTileH * tileHeight
        drawY = rawY - heightOffset + subY
              + tileHalfDiamondHeight - quadH + nudge
    pure (tz, texHandle, drawX, drawY, quadW, quadH, underwaterDepth)

renderGroundItemQuads ∷ EngineEnv → WorldState → Float
                      → IO (V.Vector SortableQuad)
renderGroundItemQuads env worldState tileAlpha = do
    gis ← readIORef (wsGroundItemsRef worldState)
    if HM.null (gisItems gis)
      then return V.empty
      else do
        let rv = toRenderViewCapability env
        camera   ← readIORef (rvCameraRef rv)
        tileData ← readIORef (wsTilesRef worldState)
        im       ← readIORef (itemManagerRef env)
        texSizes ← readIORef (rvTextureSizeRef rv)
        paramsM  ← readIORef (wsGenParamsRef worldState)
        cs       ← readIORef (wsCursorRef worldState)
        (fbW, fbH) ← readIORef (rvFramebufferSizeRef rv)
        -- The broken-weapon overlay, registered by name during item
        -- loading (Lua/API/Items). Absent until items load.
        nameReg ← readIORef (rvTextureNameRegistryRef rv)
        let mBrokenTex = lookupTextureName "broken_equipment" nameReg

        -- Bake the STABLE texture-handle id; the bindless shader resolves
        -- it to a live slot at draw time (#286). -1 = default face map:
        -- the world-layer shader masks every quad by its face-map sample,
        -- so this routes to the neutral default (the value units / flora
        -- pass) instead of whatever lives at bindless index 0.
        let lookupSlot texHandle = fromIntegral (toInt texHandle)
            defFmSlot = -1 ∷ Float
            facing  = camFacing camera
            zoom    = camZoom camera
            zSlice  = camZSlice camera
            (camX, _camY) = camPosition camera
            worldSize = maybe 128 wgpWorldSize paramsM
            effectiveDepth =
                min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
            vb = computeViewBounds camera fbW fbH effectiveDepth
            selectedGid = selectedGroundItem cs

            quadFor (gid, gi) = case quadForM (gid, gi) of
                Nothing → []
                Just q  →
                    -- Broken items (condition 0) get the broken overlay
                    -- drawn over the same rect, one hair in front.
                    if iiCondition (giInst gi) ≤ 0
                      then case mBrokenTex of
                          Just bt → [q, overlayQuad q bt]
                          Nothing → [q]
                      else [q]

            overlayQuad q bt =
                let slotF = fromIntegral (lookupSlot bt ∷ Int)
                    setTex v = v { atlasId = slotF }
                in q { sqSortKey = sqSortKey q + 0.0001
                     , sqTexture = bt
                     , sqV0 = setTex (sqV0 q), sqV1 = setTex (sqV1 q)
                     , sqV2 = setTex (sqV2 q), sqV3 = setTex (sqV3 q)
                     }

            quadForM (gid, gi) = do
                (tz, texHandle, drawX0, drawY, quadW, quadH, uwDepth)
                    ← itemGeometry tileData im texSizes facing zSlice gi
                let tx = floor (giX gi) ∷ Int
                    ty = floor (giY gi) ∷ Int
                    (chunkCoord, _) = globalToChunk tx ty
                xOff ← isChunkVisibleWrapped facing worldSize vb camX
                                             chunkCoord
                if tz > zSlice ∨ tz < zSlice - effectiveDepth
                  then Nothing
                  else do
                    let drawX = drawX0 + xOff
                        relativeZ = tz - zSlice
                        fy = giY gi - fromIntegral ty
                        (fa, fb) = applyFacing facing tx ty
                        sortKey = fromIntegral (fa + fb)
                                + fromIntegral relativeZ * 0.001
                                + 0.0006
                                + (fy - 0.5) * 0.00005

                        actualSlot = lookupSlot texHandle
                        depth = zSlice - tz
                        fadeRange = max 1 effectiveDepth
                        fadeT = clamp01 (fromIntegral depth
                                         / fromIntegral fadeRange)
                        hazeT = fadeT * fadeT * 0.6
                        -- Underwater: same blue ramp terrain uses.
                        (r, g, b) =
                            if uwDepth > 0
                            then let t = clamp01
                                         (fromIntegral uwDepth / 30.0)
                                 in (0.6 - t * 0.4, 0.7 - t * 0.4
                                    , 0.9 - t * 0.3)
                            else ( 1.0 * (1.0 - hazeT) + 0.72 * hazeT
                                 , 1.0 * (1.0 - hazeT) + 0.85 * hazeT
                                 , 1.0 * (1.0 - hazeT) + 0.95 * hazeT )
                        tint = Vec4 r g b tileAlpha

                        flags = if selectedGid ≡ Just gid
                                then renderFlagSelected else 0
                        slotF = fromIntegral (actualSlot ∷ Int)
                        wuv = packWorldUV tx ty
                        v0 = Vertex (Vec2 drawX drawY)
                                 (Vec2 0 0) tint slotF defFmSlot flags wuv
                        v1 = Vertex (Vec2 (drawX + quadW) drawY)
                                 (Vec2 1 0) tint slotF defFmSlot flags wuv
                        v2 = Vertex (Vec2 (drawX + quadW)
                                          (drawY + quadH))
                                 (Vec2 1 1) tint slotF defFmSlot flags wuv
                        v3 = Vertex (Vec2 drawX (drawY + quadH))
                                 (Vec2 0 1) tint slotF defFmSlot flags wuv
                    Just SortableQuad
                        { sqSortKey = sortKey
                        , sqV0 = v0, sqV1 = v1, sqV2 = v2, sqV3 = v3
                        , sqTexture = texHandle
                        , sqLayer = worldLayer
                        }

        return $ V.fromList
            (concatMap quadFor (HM.toList (gisItems gis)))

-- | Hit test at window-pixel coordinates (the input layer's mouse
--   coords). Returns the topmost ground item whose sprite quad
--   contains the point — same screen math as the render pass, same
--   pixel→world transform as the unit hit test.
hitTestGroundItemAt ∷ EngineEnv → WorldState → Double → Double
                    → IO (Maybe Int)
hitTestGroundItemAt env worldState pixX pixY = do
    gis ← readIORef (wsGroundItemsRef worldState)
    (winW, winH) ← readIORef (rvWindowSizeRef (toRenderViewCapability env))
    -- Zero-size window (minimize): the pixel→world divisions below would
    -- yield a non-finite click coord. Report "no item".
    if HM.null (gisItems gis) ∨ windowDegenerate winW winH
      then return Nothing
      else do
        camera   ← readIORef (rvCameraRef (toRenderViewCapability env))
        tileData ← readIORef (wsTilesRef worldState)
        im       ← readIORef (itemManagerRef env)
        texSizes ← readIORef (rvTextureSizeRef (toRenderViewCapability env))

        let facing = camFacing camera
            zoom   = camZoom camera
            zSlice = camZSlice camera
            -- Match the render band (renderGroundItemQuads) so a visible
            -- item below the camera is also clickable, not just within 25.
            effectiveDepth =
                min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
            (camX, camY) = camPosition camera

            vw     = zoom * (fromIntegral winW / fromIntegral winH)
            vh     = zoom
            normX  = realToFrac pixX / fromIntegral winW ∷ Float
            normY  = realToFrac pixY / fromIntegral winH ∷ Float
            worldX = (normX * 2.0 - 1.0) * vw + camX
            worldY = (normY * 2.0 - 1.0) * vh + camY

            candidates =
                [ (tz, dist, gid)
                | (gid, gi) ← HM.toList (gisItems gis)
                , Just (tz, _tex, drawX, drawY, quadW, quadH, _uw)
                    ← [itemGeometry tileData im texSizes facing zSlice gi]
                , tz ≤ zSlice
                , tz ≥ zSlice - effectiveDepth
                , let cx = drawX + quadW * 0.5
                      cy = drawY + quadH * 0.5
                      dx = worldX - cx
                      dy = worldY - cy
                , abs dx ≤ quadW * 0.5
                , abs dy ≤ quadH * 0.5
                , let dist = dx * dx + dy * dy
                ]
        return $ case L.sortOn (\(tz, dist, _) → (negate tz, dist))
                               candidates of
            ((_, _, gid):_) → Just gid
            []              → Nothing
