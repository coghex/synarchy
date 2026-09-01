{-# LANGUAGE Strict #-}
-- | Zoom-map location lifecycle-state icon annotations (#781): a
--   dedicated dynamic overlay, entirely separate from
--   @World.Render.Zoom.Quads.makeMapQuads@ so switching 'ZoomMapMode'
--   never tints, dims, or hides it. One square, screen-upright quad per
--   placed location whose definition declares a @map_icon@,
--   texture-selected live from the page's 'WorldGenParams' lifecycle
--   state every frame (no atlas rebake, no cached per-instance flag).
--
--   #1230 replaced #781's per-definition (undiscovered, discovered)
--   PAIR with three explicitly enumerated appearances over the six
--   'LocationLifecycle' constructors — see 'LocationIconAppearance'.
module World.Render.Zoom.Icons
    ( locationIconTargetPixels
    , iconWorldSize
    , LocationIconSet(..)
    , LocationIconAppearance(..)
    , locationIconAppearance
    , clearedIconTint
    , buildLocationIconMap
    , makeLocationIconQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.TextureNameRegistry (TextureNameRegistry, lookupTextureName)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..), mkVertexWorld, tileWorldUV)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..))
import Location.Types
    ( LocationDef(..), LocationRegistry, allLocations
    , locationIconTextureName, locationUnknownIconTextureName )
import Location.Instance
    (LocationInstance(..), LocationLifecycle(..), instancesToList)
import World.Types (WorldGenParams(..))
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

-- | How one placed location draws right now (#1230 requirement 3) —
--   the ONE mapping from lifecycle to appearance, enumerated over every
--   constructor rather than derived from a discovered\/undiscovered
--   boolean, so adding a lifecycle state is a compile error here rather
--   than a silent reuse of whichever half of a pair the boolean picked.
data LocationIconAppearance
    = IconUnknownMarker
      -- ^ @unknown@ \/ @hinted@: the ONE shared unknown icon, identical
      --   for every definition. The zoom map must not leak WHAT is
      --   there before a unit has seen it.
    | IconTypeNormal
      -- ^ @discovered@ \/ @active@: the definition's own 'ldMapIcon',
      --   drawn at full brightness.
    | IconTypeDark
      -- ^ @cleared@ \/ @depleted@: the SAME type icon, darkened through
      --   the quad's own colour. No second bitmap is authored for it.
    deriving (Show, Eq)

locationIconAppearance ∷ LocationLifecycle → LocationIconAppearance
locationIconAppearance LifecycleUnknown    = IconUnknownMarker
locationIconAppearance LifecycleHinted     = IconUnknownMarker
locationIconAppearance LifecycleDiscovered = IconTypeNormal
locationIconAppearance LifecycleActive     = IconTypeNormal
locationIconAppearance LifecycleCleared    = IconTypeDark
locationIconAppearance LifecycleDepleted   = IconTypeDark

-- | RGB multiplier for a spent ('IconTypeDark') location's icon quad.
--   Strictly below the normal 1.0 on every channel, so a cleared ruin
--   reads as spent without a second authored bitmap.
--
--   This is a deliberate, ENUMERATED exception to the project's
--   no-tinting rule, authorized by @docs\/expedition_gameplay_loop.md@
--   D-16 alongside the existing underwater, fluid-surface and lava
--   tints. It is confined to this one icon quad's own 'Vec4': no
--   texture is re-authored and nothing else on the zoom map is tinted.
clearedIconTint ∷ Float
clearedIconTint = 0.45

-- | Every texture the icon overlay can draw: the ONE shared
--   unknown-location marker, plus each definition's own type icon keyed
--   by def id.
--
--   A def with no 'ldMapIcon' contributes NO entry at all — the render
--   side treats a missing id as "no annotation", exactly matching the
--   "definitions may deliberately omit a map icon" requirement.
data LocationIconSet = LocationIconSet
    { lisUnknown ∷ !TextureHandle
      -- ^ the shared unknown marker, registered independently of every
      --   definition under 'locationUnknownIconTextureName' (#1230)
    , lisTypeIcons ∷ !(HM.HashMap Text TextureHandle)
      -- ^ per-definition type icons, keyed by 'ldId'
    } deriving (Show, Eq)

-- | Resolve the shared unknown marker and every location def's declared
--   type icon to loaded texture handles. A def that DOES declare an icon
--   but whose registered texture name isn't loaded yet this session (or
--   never finished loading) falls back to 'fallback' — the caller's own
--   world.wtNoTexture — so a location marker is never silently dropped
--   for a texture-load timing reason. The shared unknown marker falls
--   back the same way, and is resolved unconditionally: it belongs to no
--   definition, so an empty registry still yields a drawable set.
buildLocationIconMap
    ∷ LocationRegistry → TextureNameRegistry → TextureHandle
    → LocationIconSet
buildLocationIconMap registry nameReg fallback = LocationIconSet
    { lisUnknown   = resolve locationUnknownIconTextureName
    , lisTypeIcons = HM.fromList
        [ (lid, resolve (locationIconTextureName lid))
        | def ← allLocations registry
        , Just _ ← [ldMapIcon def]
        , let lid = ldId def
        ]
    }
  where
    resolve name = fromMaybe fallback (lookupTextureName name nameReg)

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
--   terrain quads use, applied to each placed location's stored anchor
--   tile instead of a chunk box. Iterates 'instancesToList' order
--   (sorted by instance id, never hashmap-iteration order) for
--   deterministic quad ordering call over call — no two frames with
--   unchanged inputs can reorder or flicker.
--
--   Texture and colour selection is 'locationIconAppearance' — the one
--   explicit per-lifecycle mapping (#1230). An instance whose def
--   declares no 'ldMapIcon' draws NOTHING, in every lifecycle state
--   including the unknown ones: every branch below is gated on that
--   def's own type icon being present. "Definitions with no map-icon
--   declaration produce no zoom-map annotation" is the contract, and
--   drawing the shared marker for one would both annotate a location
--   the author opted out of annotating and promise a reveal that has
--   no icon to reveal into. Gate: the "renders nothing, in EVERY
--   lifecycle state" case in the "Location map icons" spec.
--
--   Always axis-aligned / screen-upright: the facing rotation is already
--   baked into 'gridToWorld's world position for the anchor, and the
--   quad itself carries no additional rotation, so it stays upright
--   regardless of camera facing. 'alpha' is passed
--   through as-is (the caller supplies the same zoomAlpha terrain
--   fades with) in ALL six lifecycle states — 'clearedIconTint' scales
--   only RGB — and is the ONLY thing 'ZoomMapMode' or day/night could
--   otherwise dim: icon color is never routed through a mode's color
--   function.
makeLocationIconQuads
    ∷ WorldGenParams
    → LocationIconSet
    → CameraFacing → ZoomViewBounds
    → Float → Float             -- ^ camX, camY
    → Float                     -- ^ alpha (zoomAlpha)
    → Float                     -- ^ icon world size, from 'iconWorldSize'
    → LayerId
    → (TextureHandle → Int) → Float   -- ^ lookupSlot, defFmSlot
    → V.Vector SortableQuad
makeLocationIconQuads params iconSet facing vb camX camY alpha iconSize layer lookupSlot defFmSlot
    | iconSize ≤ 0 = V.empty
    | otherwise =
        let ws = wgpWorldSize params
            half = iconSize / 2.0
            entries = zip [iconSortKeyBase ..]
                          (instancesToList (wgpLocationInstances params))
        in V.mapMaybe (\(sortKey, inst) → do
               let mTypeTex = HM.lookup (liDefId inst) (lisTypeIcons iconSet)
               (tex, rgb) ← case locationIconAppearance (liLifecycle inst) of
                   IconUnknownMarker →
                       (\_ → (lisUnknown iconSet, 1.0)) ⊚ mTypeTex
                   IconTypeNormal    → (\t → (t, 1.0)) ⊚ mTypeTex
                   IconTypeDark      → (\t → (t, clearedIconTint)) ⊚ mTypeTex
               let (gx, gy) = liAnchor inst
                   (baseX, baseY) = gridToWorld facing gx gy
                   (offX, offY) = bestZoomWrapOffset facing ws camX camY baseX baseY
                   wrappedX = baseX + offX
                   wrappedY = baseY + offY
                   drawX = wrappedX - half
                   drawY = wrappedY - half
               if isChunkInView vb drawX drawY iconSize iconSize
                   then Just (emitIconQuad tex rgb drawX drawY iconSize layer alpha
                                            sortKey gx gy lookupSlot defFmSlot)
                   else Nothing
              ) (V.fromList entries)

-- | Emit one screen-upright square quad at (drawX, drawY)..(+size,+size)
--   in world space, with @rgb@ on all three colour channels and the
--   caller's zoom fade untouched in alpha. 'wuv' is packed from the anchor tile so the shared
--   bindless pipeline has a representative (not centered-on-nothing)
--   value, mirroring 'World.Render.Zoom.Cursor.emitCursorQuad'.
emitIconQuad
    ∷ TextureHandle → Float → Float → Float → Float → LayerId → Float → Float
    → Int → Int → (TextureHandle → Int) → Float → SortableQuad
emitIconQuad tex rgb drawX drawY size layer alpha sortKey gx gy lookupSlot defFmSlot =
    let slot  = fromIntegral (lookupSlot tex)
        color = Vec4 rgb rgb rgb alpha
        wuv   = tileWorldUV gx gy
    in SortableQuad
        { sqSortKey = sortKey
        , sqV0 = mkVertexWorld wuv (Vec2 drawX drawY)                 (Vec2 0 0) color slot defFmSlot
        , sqV1 = mkVertexWorld wuv (Vec2 (drawX + size) drawY)        (Vec2 1 0) color slot defFmSlot
        , sqV2 = mkVertexWorld wuv (Vec2 (drawX + size) (drawY + size)) (Vec2 1 1) color slot defFmSlot
        , sqV3 = mkVertexWorld wuv (Vec2 drawX (drawY + size))        (Vec2 0 1) color slot defFmSlot
        , sqTexture = tex
        , sqLayer   = layer
        }
