{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-frame quads for spoil piles (World.Spoil) — the gravel
--   mounds digging deposits at tile vertices.
--
--   Each tile with at least one raised corner gets an overlay quad
--   per raised level: the spoil material's tile texture drawn at
--   surface+level with the slope face map whose LOWERED corners are
--   the ones the piles haven't raised. Same trick the dig system
--   uses in reverse — the face map is orthogonal to the material
--   texture. The overlay only ever covers the partial fringe; full
--   cells are promoted to real terrain (WeAddTile) and render
--   through the normal cached tile pass.
--
--   Per-frame like the ground-item pass: piles change every dig tick
--   and the set is small, so skipping the chunk-quad cache keeps the
--   invalidation story simple.
module World.Render.SpoilQuads
    ( renderSpoilQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef)
import Data.Maybe (mapMaybe)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Asset.Handle (toInt)
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..), mkVertexWorld
                                           , packWorldUV)
import Engine.Scene.Types (SortableQuad(..))
import World.Generate (viewDepth)
import World.Generate.Coordinates (globalToChunk)
import World.Grid (gridToScreen, tileWidth, tileHeight, tileSideHeight
                  , worldLayer, applyFacing)
import World.Material.Id (MaterialId(..))
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.Textures (getTileTexture, getTileFaceMapTexture)
import World.Render.ViewBounds (computeViewBounds)
import World.Spoil.Types (SpoilPile(..), spoilSlotTiles, spoilLevelAt)
import World.Types

-- | Per-tile raised-corner state at one level: (NW, NE, SE, SW).
type Corners = (Bool, Bool, Bool, Bool)

-- | Slope-mask of LOWERED corners, edge-encoded (N=1,E=2,S=4,W=8 —
--   an edge slopes once both its corners are lowered; same rule as
--   World.Mine.Types.digSlopeMask). Diagonal-only patterns aren't
--   expressible in edge bits and degrade to fewer lowered corners —
--   a slight over-render, acceptable for a transient overlay.
loweredMask ∷ Corners → Word8
loweredMask (rNW, rNE, rSE, rSW) =
    let lNW = not rNW
        lNE = not rNE
        lSE = not rSE
        lSW = not rSW
    in (if lNW ∧ lNE then 1 else 0)
     ⌄ (if lNE ∧ lSE then 2 else 0)
     ⌄ (if lSE ∧ lSW then 4 else 0)
     ⌄ (if lSW ∧ lNW then 8 else 0)

renderSpoilQuads ∷ EngineEnv → WorldState → Float
                 → IO (V.Vector SortableQuad)
renderSpoilQuads env worldState tileAlpha = do
    piles ← readIORef (wsSpoilRef worldState)
    if HM.null piles
      then return V.empty
      else do
        camera   ← readIORef (cameraRef env)
        tileData ← readIORef (wsTilesRef worldState)
        textures ← readIORef (wsTexturesRef worldState)
        paramsM  ← readIORef (wsGenParamsRef worldState)
        (fbW, fbH) ← readIORef (framebufferSizeRef env)

        -- Bake STABLE handle ids (tile + its face map); the bindless
        -- shader resolves them to live slots at draw time, and applies
        -- the default-face-map fallback when a face-map handle is
        -- unregistered (slot 0) (#286).
        let lookupSlot texHandle = fromIntegral (toInt texHandle) ∷ Float
            lookupFmSlot texHandle = fromIntegral (toInt texHandle) ∷ Float
            facing  = camFacing camera
            zoom    = camZoom camera
            zSlice  = camZSlice camera
            (camX, _camY) = camPosition camera
            worldSize = maybe 128 wgpWorldSize paramsM
            effectiveDepth =
                min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
            vb = computeViewBounds camera fbW fbH effectiveDepth

            -- Tile → per-level raised corners, folded from every
            -- vertex pile touching it. Corner order NW,NE,SE,SW;
            -- tile corner ↔ vertex slot via the fixed slot layout
            -- (see World.Spoil.Types.spoilSlotTiles).
            tilesAtLevel lvl = HM.foldlWithKey' step HM.empty piles
              where
                step acc v p =
                    foldl' (stepSlot v p) acc (zip [0 ∷ Int ..]
                                                   (spoilSlotTiles v))
                stepSlot _v p acc (slotIdx, (tile, _)) =
                    let fill = case slotIdx of
                            0 → f0; 1 → f1; 2 → f2; _ → f3
                        (f0, f1, f2, f3) = spFill p
                    in if spoilLevelAt fill ≥ lvl
                       then HM.insertWith merge tile
                                (spMat p, cornerOf slotIdx) acc
                       else acc
                merge (m, c1) (_, c2) = (m, orC c1 c2)
                orC (a1,b1,c1,d1) (a2,b2,c2,d2) =
                    (a1∨a2, b1∨b2, c1∨c2, d1∨d2)
                -- A vertex's slot N corresponds to a specific corner
                -- of the slot tile: slot 0 tile's SE, slot 1 SW,
                -- slot 2 NE, slot 3 NW (see spoilSlotTiles).
                cornerOf 0 = (False, False, True,  False)  -- SE
                cornerOf 1 = (False, False, False, True )  -- SW
                cornerOf 2 = (False, True,  False, False)  -- NE
                cornerOf _ = (True,  False, False, False)  -- NW

            quadFor lvl (_tile@(tx, ty), (mat, corners)) = do
                let (chunkCoord, (lx, ly)) = globalToChunk tx ty
                lc ← HM.lookup chunkCoord (wtdChunks tileData)
                xOff ← isChunkVisibleWrapped facing worldSize vb camX
                                             chunkCoord
                let surfZ = lcTerrainSurfaceMap lc
                                VU.! columnIndex lx ly
                    topZ  = surfZ + lvl
                if topZ > zSlice ∨ topZ < zSlice - effectiveDepth
                  then Nothing
                  else do
                    let (rawX, rawY) = gridToScreen facing tx ty
                        (fa, fb) = applyFacing facing tx ty
                        relativeZ = topZ - zSlice
                        heightOffset =
                            fromIntegral relativeZ * tileSideHeight
                        drawX = rawX + xOff
                        drawY = rawY - heightOffset
                        -- Just above the terrain tile at this cell
                        -- (terrain 0.0), below fluid (0.0005).
                        sortKey = fromIntegral (fa + fb)
                                + fromIntegral relativeZ * 0.001
                                + 0.0003
                        tex    = getTileTexture textures
                                     (unMaterialId mat)
                        fmTex  = getTileFaceMapTexture textures
                                     (unMaterialId mat)
                                     (loweredMask corners)
                        slotF  = lookupSlot tex
                        fmF    = lookupFmSlot fmTex
                        tint   = Vec4 1.0 1.0 1.0 tileAlpha
                        wuv    = packWorldUV tx ty
                        v0 = mkVertexWorld wuv (Vec2 drawX drawY)
                                 (Vec2 0 0) tint slotF fmF
                        v1 = mkVertexWorld wuv (Vec2 (drawX + tileWidth) drawY)
                                 (Vec2 1 0) tint slotF fmF
                        v2 = mkVertexWorld wuv (Vec2 (drawX + tileWidth)
                                            (drawY + tileHeight))
                                 (Vec2 1 1) tint slotF fmF
                        v3 = mkVertexWorld wuv (Vec2 drawX (drawY + tileHeight))
                                 (Vec2 0 1) tint slotF fmF
                    Just SortableQuad
                        { sqSortKey = sortKey
                        , sqV0 = v0, sqV1 = v1, sqV2 = v2, sqV3 = v3
                        , sqTexture = tex
                        , sqLayer = worldLayer
                        }

        return $ V.fromList $ concat
            [ mapMaybe (quadFor lvl) (HM.toList (tilesAtLevel lvl))
            | lvl ← [1, 2] ]
