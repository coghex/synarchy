{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Render
    ( renderUnitQuads
    , pickFrame
    , screenDirOf
    , resolveTexture
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (readIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..)
                                          , renderFlagSelected)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , worldLayer, GridConfig(..), defaultGridConfig)
import Unit.Types
import Unit.Direction (Direction(..), dirIndex, indexToDir)

baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)

unitSortNudge ∷ Float
unitSortNudge = 0.0003

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
--   facing and the current camera rotation. Falls back to the default texture
--   if no directional map or no entry for the computed screen direction.
resolveTexture
    ∷ CameraFacing
    → Direction                          -- ^ unit world facing
    → Map.Map Direction TextureHandle    -- ^ directional sprites
    → TextureHandle                      -- ^ fallback default
    → TextureHandle
resolveTexture camFacing unitFacing dirSprites fallback
    | Map.null dirSprites = fallback
    | otherwise =
        case Map.lookup (screenDirOf camFacing unitFacing) dirSprites of
            Just h  → h
            Nothing → fallback

-- | Choose a texture for a unit. If the unit has an active animation and
--   the requested frames exist, pick by elapsed time; otherwise fall back
--   to the T-pose. Used by the render path.
pickFrame
    ∷ Double            -- ^ now (POSIX seconds)
    → CameraFacing
    → UnitInstance
    → UnitDef           -- ^ animation library + T-pose fallback
    → TextureHandle
pickFrame now cam inst def
    | T.null (uiCurrentAnim inst) = tpose
    | otherwise =
        case HM.lookup (uiCurrentAnim inst) (udAnimations def) of
            Nothing  → tpose
            Just an  →
                let dir = screenDirOf cam (uiFacing inst)
                in case Map.lookup dir (aFrames an) of
                    Nothing → tpose
                    Just fs
                        | V.null fs → tpose
                        | otherwise →
                            let elapsed = max 0 (now - uiAnimStart inst)
                                raw     = floor (elapsed * realToFrac (aFps an)) ∷ Int
                                n       = V.length fs
                                fwdIdx  = if aLoop an
                                          then raw `mod` n
                                          else min raw (n - 1)
                                -- Reverse path: walk the frames from
                                -- the last index toward 0. Mirrors the
                                -- forward clamp so we hold frame 0
                                -- when "elapsed" runs out — the unit
                                -- stays in the standing pose after
                                -- the revive anim completes (sim will
                                -- transition Reviving→Idle next tick).
                                idx     = if uiAnimReverse inst
                                          then (n - 1) - fwdIdx
                                          else fwdIdx
                            in fs V.! idx
  where
    tpose = resolveTexture cam (uiFacing inst)
                           (uiDirSprites inst) (uiTexture inst)

renderUnitQuads ∷ EngineEnv → CameraFacing → Int → Float → IO (V.Vector SortableQuad)
renderUnitQuads env facing zSlice tileAlpha = do
    um ← readIORef (unitManagerRef env)
    let instances = umInstances um
        defs      = umDefs um
        selected  = umSelected um
    if HM.null instances
        then return V.empty
        else do
            now ← realToFrac <$> getPOSIXTime
            texSizes ← readIORef (textureSizeRef env)
            mBts ← readIORef (textureSystemRef env)
            defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
            case mBts of
                Nothing → return V.empty
                Just bts → do
                    let lookupSlot h = getTextureSlotIndex h bts
                        defFmSlot = fromIntegral defFmSlotWord
                        quads = V.fromList
                            $ HM.foldlWithKey' (\acc uid inst →
                                let isSel = HS.member uid selected
                                    mDef  = HM.lookup (uiDefName inst) defs
                                in case unitToQuad lookupSlot defFmSlot facing
                                                zSlice tileAlpha isSel inst
                                                mDef now texSizes of
                                    Just sq → sq : acc
                                    Nothing → acc
                              ) [] instances
                    return quads

unitToQuad
    ∷ (TextureHandle → Word32)
    → Float
    → CameraFacing
    → Int
    → Float
    → Bool                                 -- ^ selected (sets outline bit)
    → UnitInstance
    → Maybe UnitDef                        -- ^ animation library (Nothing → T-pose only)
    → Double                               -- ^ now (POSIX seconds)
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
unitToQuad lookupSlot defFmSlot facing zSlice tileAlpha isSel inst mDef now texSizes =
    let gridZ = uiGridZ inst
        relativeZ = gridZ - zSlice
    in if gridZ > zSlice ∨ gridZ < (zSlice - 25)
       then Nothing
       else
        let texHandle = case mDef of
                Just def → pickFrame now facing inst def
                Nothing  → resolveTexture facing (uiFacing inst)
                                          (uiDirSprites inst) (uiTexture inst)

            (texW, texH) = case HM.lookup texHandle texSizes of
                Just (w, h) → (fromIntegral w, fromIntegral h)
                Nothing     → (baseTileW, baseTileH)

            scaleX = texW / baseTileW
            scaleY = texH / baseTileH
            quadW = tileWidth  * scaleX
            quadH = tileHeight * scaleY

            gxF = uiGridX inst
            gyF = uiGridY inst
            (faF, fbF) = applyFacingF facing gxF gyF

            rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
            rawY = (faF + fbF) * tileHalfDiamondHeight

            heightOffset = fromIntegral relativeZ * tileSideHeight
            baseRadius = uiBaseWidth inst * 0.5 / baseTileH * tileHeight

            drawX = rawX + (tileWidth - quadW) * 0.5
            drawY = rawY - heightOffset
                  + tileHalfDiamondHeight - quadH + baseRadius

            spriteRowSpan = quadH / tileHalfDiamondHeight * 0.5
            sortKey = (faF + fbF)
                    + spriteRowSpan
                    + fromIntegral relativeZ * 0.001
                    + 0.0006

            actualSlot = lookupSlot texHandle
            tint = Vec4 1.0 1.0 1.0 tileAlpha
            flags = if isSel then renderFlagSelected else 0

            v0 = Vertex (Vec2 drawX drawY)
                         (Vec2 0 0) tint (fromIntegral actualSlot) defFmSlot flags
            v1 = Vertex (Vec2 (drawX + quadW) drawY)
                         (Vec2 1 0) tint (fromIntegral actualSlot) defFmSlot flags
            v2 = Vertex (Vec2 (drawX + quadW) (drawY + quadH))
                         (Vec2 1 1) tint (fromIntegral actualSlot) defFmSlot flags
            v3 = Vertex (Vec2 drawX (drawY + quadH))
                         (Vec2 0 1) tint (fromIntegral actualSlot) defFmSlot flags

        in Just SortableQuad
            { sqSortKey = sortKey
            , sqV0      = v0
            , sqV1      = v1
            , sqV2      = v2
            , sqV3      = v3
            , sqTexture = texHandle
            , sqLayer   = worldLayer
            }

applyFacingF ∷ CameraFacing → Float → Float → (Float, Float)
applyFacingF FaceSouth gx gy = ( gx,  gy)
applyFacingF FaceWest  gx gy = ( gy, -gx)
applyFacingF FaceNorth gx gy = (-gx, -gy)
applyFacingF FaceEast  gx gy = (-gy,  gx)
