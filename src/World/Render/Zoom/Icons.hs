{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Zoom-map location discovery-state icon annotations (#781): a
--   dedicated dynamic overlay, entirely separate from
--   'World.Render.Zoom.Quads.makeMapQuads' so switching 'ZoomMapMode'
--   never tints, dims, or hides it. One square, screen-upright quad per
--   placed location that declares a @map_icons@ pair, texture-selected
--   live from the page's 'WorldGenParams' discovery state every frame
--   (no atlas rebake, no cached per-instance flag).
module World.Render.Zoom.Icons
    ( locationIconTargetPixels
    , iconWorldSize
    , buildLocationIconMap
    , makeLocationIconQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlTextures (TextureNameRegistry, lookupTextureName)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..), mkVertexWorld, packWorldUV)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..))
import Location.Types (LocationDef(..), LocationRegistry, allLocations, locationIconTextureName)
import Location.Overlay.Types (overlayToList)
import World.Chunk.Types (ChunkCoord(..))
import World.Types (WorldGenParams(..), chunkSize)
import World.Grid (gridToWorld)
import World.Render.Zoom.ViewBounds (ZoomViewBounds, isChunkInView, bestZoomWrapOffset)

-- * Constant Logical Screen Size

-- | Target on-screen icon size in LOGICAL (window, not framebuffer)
--   pixels. See 'iconWorldSize' for why framebuffer/DPI scale never
--   enters the size calculation at all.
locationIconTargetPixels ∷ Float
locationIconTargetPixels = 32.0

-- | World-space side length of a square icon quad that projects to
--   exactly 'targetPx' LOGICAL screen pixels at the given camera zoom
--   and LOGICAL window height.
--
--   'Engine.Graphics.Camera.createProjectionMatrix' maps @2*zoom@ world
--   units onto the FULL FRAMEBUFFER height (@top = zoom@, @bottom =
--   -zoom@ over 'fbH' physical pixels), so world units → framebuffer
--   pixels is @fbH / (2*zoom)@. Framebuffer pixels → LOGICAL pixels
--   divides by the DPI ratio @fbH / winH@. The 'fbH' factor cancels
--   algebraically, leaving:
--
--   > worldSize = targetPx * 2 * zoom / winH
--
--   — independent of framebuffer size/DPI scale (never a parameter
--   here) and of world chunk dimensions ('chunkSize' never appears
--   either). A non-positive 'winH' (degenerate/minimized window) yields
--   0 (no visible icon) rather than dividing by zero.
iconWorldSize ∷ Float → Float → Float → Float
iconWorldSize targetPx zoom winH
    | winH ≤ 0  = 0
    | otherwise = targetPx * 2.0 * zoom / winH

-- * Icon Texture Resolution

-- | Resolve every location def's declared icon pair to loaded texture
--   handles, keyed by def id. A def with no 'ldMapIcons' contributes NO
--   entry at all — the render side treats a missing id as "no
--   annotation", exactly matching the "definitions may deliberately
--   omit map_icons" requirement. A def that DOES declare icons but
--   whose registered texture name isn't loaded yet this session (or
--   never finished loading) falls back to 'fallback' — the caller's own
--   world.wtNoTexture — for that state, so a location marker is never
--   silently dropped for a texture-load timing reason.
buildLocationIconMap
    ∷ LocationRegistry → TextureNameRegistry → TextureHandle
    → HM.HashMap Text (TextureHandle, TextureHandle)
buildLocationIconMap registry nameReg fallback =
    HM.fromList
        [ (lid, (resolveState False, resolveState True))
        | def ← allLocations registry
        , Just _ ← [ldMapIcons def]
        , let lid = ldId def
              resolveState isDiscovered = fromMaybe fallback
                  (lookupTextureName (locationIconTextureName lid isDiscovered) nameReg)
        ]

-- * Icon Quad Generation

-- | Sort keys start well above every terrain 'World.Render.Zoom.Bake'
--   entry (@bzeSortKey = chunkY@, a small signed int for any world this
--   engine generates) and above the zoom-map cursor's select/hover
--   quads (99/100, 'World.Render.Zoom.Cursor') — painter's algorithm
--   within 'zoomMapLayer' draws icons above both, matching "renders
--   above all zoom-map terrain and climate-map modes".
iconSortKeyBase ∷ Float
iconSortKeyBase = 1000.0

-- | Pure per-frame icon-quad generation, mirroring 'World.Render.Zoom.
--   Quads.makeMapQuads': the SAME cylindrical wrap
--   ('bestZoomWrapOffset') and view-frustum culling ('isChunkInView')
--   terrain quads use, applied to each placed location's anchor tile
--   instead of a chunk box. Iterates 'overlayToList' order (sorted by
--   @(cx,cy)@, never hashmap-iteration order) for deterministic quad
--   ordering call over call — no two frames with unchanged inputs can
--   reorder or flicker. Always axis-aligned / screen-upright: the
--   facing rotation is already baked into 'gridToWorld's world position
--   for the anchor, and the quad itself carries no additional rotation,
--   so it stays upright regardless of camera facing. 'alpha' is passed
--   through as-is (the caller supplies the same zoomAlpha terrain
--   fades with) and is the ONLY thing 'ZoomMapMode' or day/night could
--   otherwise dim — icon color is always plain white × alpha, never
--   routed through a mode's color function.
makeLocationIconQuads
    ∷ WorldGenParams
    → HM.HashMap Text (TextureHandle, TextureHandle)
    → CameraFacing → ZoomViewBounds
    → Float → Float             -- ^ camX, camY
    → Float                     -- ^ alpha (zoomAlpha)
    → Float                     -- ^ icon world size, from 'iconWorldSize'
    → LayerId
    → (TextureHandle → Int) → Float   -- ^ lookupSlot, defFmSlot
    → V.Vector SortableQuad
makeLocationIconQuads params iconMap facing vb camX camY alpha iconSize layer lookupSlot defFmSlot
    | iconSize ≤ 0 = V.empty
    | otherwise =
        let ws = wgpWorldSize params
            overlay = wgpLocationOverlay params
            discovered = wgpLocationDiscovered params
            half = iconSize / 2.0
            entries = zip [iconSortKeyBase ..] (overlayToList overlay)
        in V.mapMaybe (\(sortKey, (coord@(ChunkCoord cx cy), lid)) → do
               (undiscTex, discTex) ← HM.lookup lid iconMap
               let hlf = chunkSize `div` 2
                   gx = cx * chunkSize + hlf
                   gy = cy * chunkSize + hlf
                   (baseX, baseY) = gridToWorld facing gx gy
                   (offX, offY) = bestZoomWrapOffset facing ws camX camY baseX baseY
                   wrappedX = baseX + offX
                   wrappedY = baseY + offY
                   drawX = wrappedX - half
                   drawY = wrappedY - half
                   tex = if HS.member coord discovered then discTex else undiscTex
               if isChunkInView vb drawX drawY iconSize iconSize
                   then Just (emitIconQuad tex drawX drawY iconSize layer alpha
                                            sortKey gx gy lookupSlot defFmSlot)
                   else Nothing
              ) (V.fromList entries)

-- | Emit one screen-upright square quad at (drawX, drawY)..(+size,+size)
--   in world space. 'wuv' is packed from the anchor tile so the shared
--   bindless pipeline has a representative (not centered-on-nothing)
--   value, mirroring 'World.Render.Zoom.Cursor.emitCursorQuad'.
emitIconQuad
    ∷ TextureHandle → Float → Float → Float → LayerId → Float → Float
    → Int → Int → (TextureHandle → Int) → Float → SortableQuad
emitIconQuad tex drawX drawY size layer alpha sortKey gx gy lookupSlot defFmSlot =
    let slot  = fromIntegral (lookupSlot tex)
        color = Vec4 1.0 1.0 1.0 alpha
        wuv   = packWorldUV gx gy
    in SortableQuad
        { sqSortKey = sortKey
        , sqV0 = mkVertexWorld wuv (Vec2 drawX drawY)                 (Vec2 0 0) color slot defFmSlot
        , sqV1 = mkVertexWorld wuv (Vec2 (drawX + size) drawY)        (Vec2 1 0) color slot defFmSlot
        , sqV2 = mkVertexWorld wuv (Vec2 (drawX + size) (drawY + size)) (Vec2 1 1) color slot defFmSlot
        , sqV3 = mkVertexWorld wuv (Vec2 drawX (drawY + size))        (Vec2 0 1) color slot defFmSlot
        , sqTexture = tex
        , sqLayer   = layer
        }
