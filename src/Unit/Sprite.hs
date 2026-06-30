{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Camera-rotation-aware directional sprite resolution, shared by the
--   renderer ('Unit.Render') and the click/box hit-tester
--   ('Unit.HitTest'). Both must resolve the SAME texture for a given
--   unit facing + camera rotation: the renderer to draw it, the
--   hit-tester to size the selection quad. Keeping the resolution in one
--   place stops the two from diverging — historically 'Unit.HitTest'
--   carried a stripped copy that omitted the 'mirrorDir' fallback, so
--   the hit-box of W/SW/NW-facing units (drawn as the mirrored eastern
--   sprite) was sized from the default texture instead (#389).
--
--   This module is deliberately light — Direction math + 'CameraFacing'
--   + 'TextureHandle' only — so 'Unit.HitTest' can share it without
--   pulling in the whole Vulkan renderer. 'Unit.Render' re-exports
--   'screenDirOf' and 'resolveTexture' for existing callers.
module Unit.Sprite
    ( cameraRotSteps
    , screenDirOf
    , resolveTexture
    ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Unit.Direction (Direction(..), dirIndex, indexToDir, mirrorDir)

-- | How many direction steps (in our 8-dir clockwise ring) the
--   camera rotation shifts.  Each 90 deg CW rotation = 2 steps.
cameraRotSteps ∷ CameraFacing → Int
cameraRotSteps FaceSouth = 0
cameraRotSteps FaceWest  = 2
cameraRotSteps FaceNorth = 4
cameraRotSteps FaceEast  = 6

-- | Apply camera rotation to a world-space facing to get the screen-space
--   direction we should render. Used by both the T-pose picker and the
--   animation frame picker.
screenDirOf ∷ CameraFacing → Direction → Direction
screenDirOf camFacing unitFacing =
    indexToDir ((dirIndex unitFacing - cameraRotSteps camFacing) `mod` 8)

-- | Pick the correct directional sprite for a unit given its world-space
--   facing and the current camera rotation.
--
--   Lookup order: requested screen direction → its `mirrorDir` (returned
--   with `flipX = True` so the renderer flips UVs) → fallback default
--   (no flip). The mirror step lets animations ship 5 directional
--   sprites (S/SE/E/NE/N) instead of 8 — SW/W/NW are produced by
--   horizontal mirror at draw time.
resolveTexture
    ∷ CameraFacing
    → Direction                          -- ^ unit world facing
    → Map.Map Direction TextureHandle    -- ^ directional sprites
    → TextureHandle                      -- ^ fallback default
    → (TextureHandle, Bool)              -- ^ (handle, flipX)
resolveTexture camFacing unitFacing dirSprites fallback
    | Map.null dirSprites = (fallback, False)
    | otherwise =
        let dir = screenDirOf camFacing unitFacing
        in case Map.lookup dir dirSprites of
            Just h  → (h, False)
            Nothing → case mirrorDir dir >>= (`Map.lookup` dirSprites) of
                Just h  → (h, True)
                Nothing → (fallback, False)
