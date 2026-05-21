{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Building.Render
    ( renderBuildingQuads
    , renderGhostQuad
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
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
import Unit.Direction (Direction(..))
import Unit.Types (Animation(..))
import Building.Types

baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)

-- | Pick a frame for a building at the given POSIX time. Mirrors
--   Unit.Render.pickFrame but simpler — only one direction key
--   ("default") and no reverse-playback flag.
pickBuildingFrame ∷ Double → BuildingInstance → BuildingDef → TextureHandle
pickBuildingFrame now inst def =
    let activity   = currentActivity now inst def
        stateKey   = case activity of
                       Appearing → "appearing" ∷ Text
                       Built     → "built"
        -- Find the animation for the current state. If we're Built
        -- and no "built" animation is defined, fall back to the LAST
        -- frame of "appearing" so the visible sprite doesn't snap
        -- back to bdTexture (which may differ from the final
        -- construction frame). pinLastFrame flags that mode.
        (mAnim, pinLastFrame) =
            case HM.lookup stateKey (bdStateAnims def) of
                Just animName
                    | Just a ← HM.lookup animName (bdAnimations def)
                    → (Just a, False)
                _ → case activity of
                    Built → case HM.lookup "appearing" (bdStateAnims def) of
                        Just animName →
                            (HM.lookup animName (bdAnimations def), True)
                        Nothing → (Nothing, False)
                    _ → (Nothing, False)
    in case mAnim of
        Nothing → bdTexture def
        Just a  → case Map.lookup DirS (aFrames a) of
            Nothing → bdTexture def
            Just fs
                | V.null fs → bdTexture def
                | otherwise →
                    let n = V.length fs
                        -- Worker-driven construction: while Appearing
                        -- and bdBuildWork > 0, the visible frame tracks
                        -- progress directly. No workers → frac stays
                        -- put → animation freezes mid-build.
                        progressIdx =
                            let frac = realToFrac (biBuildProgress inst)
                                     / realToFrac (bdBuildWork def) ∷ Double
                                raw  = floor (frac * fromIntegral n) ∷ Int
                            in max 0 (min (n - 1) raw)
                        timeIdx =
                            let elapsed = max 0 (now - biSpawnedAt inst)
                                raw     = floor (elapsed * realToFrac (aFps a)) ∷ Int
                            in if aLoop a
                               then raw `mod` n
                               else min raw (n - 1)
                        idx
                          | pinLastFrame                              = n - 1
                          | activity ≡ Appearing ∧ bdBuildWork def > 0 = progressIdx
                          | otherwise                                  = timeIdx
                    in fs V.! idx

renderBuildingQuads ∷ EngineEnv → CameraFacing → Int → Float → IO (V.Vector SortableQuad)
renderBuildingQuads env facing zSlice tileAlpha = do
    bm ← readIORef (buildingManagerRef env)
    let instances = bmInstances bm
        defs      = bmDefs bm
        selected  = bmSelected bm
    if HM.null instances
        then return V.empty
        else do
            -- Game-clock matches biSpawnedAt's clock, so the
            -- Appearing→Built transition derived from elapsed time
            -- doesn't run while paused.
            now ← readIORef (gameTimeRef env)
            texSizes ← readIORef (textureSizeRef env)
            mBts ← readIORef (textureSystemRef env)
            defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
            case mBts of
                Nothing → return V.empty
                Just bts → do
                    let lookupSlot h = getTextureSlotIndex h bts
                        defFmSlot = fromIntegral defFmSlotWord
                        quads = V.fromList
                            $ HM.foldlWithKey' (\acc bid inst →
                                let mDef  = HM.lookup (biDefName inst) defs
                                    isSel = selected ≡ Just bid
                                in case buildingToQuad lookupSlot defFmSlot facing
                                                zSlice tileAlpha isSel inst mDef
                                                now texSizes of
                                    Just sq → sq : acc
                                    Nothing → acc
                              ) [] instances
                    return quads

buildingToQuad
    ∷ (TextureHandle → Word32)
    → Float
    → CameraFacing
    → Int
    → Float
    → Bool                                  -- ^ selected (sets outline bit)
    → BuildingInstance
    → Maybe BuildingDef
    → Double
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
buildingToQuad lookupSlot defFmSlot facing zSlice tileAlpha isSel inst mDef now texSizes =
    let gridZ = biGridZ inst
        relativeZ = gridZ - zSlice
    in if gridZ > zSlice ∨ gridZ < (zSlice - 25)
       then Nothing
       else
        let -- Pre-delivery ghost: building was placed but its materials
            -- gate hasn't been satisfied yet. Render the final form
            -- with 0.6 alpha (matches the placement-time ghost) so
            -- the player sees a translucent silhouette of what'll
            -- land here once delivery completes.
            isGhost = case mDef of
                Just d  → bdBuildWork d > 0
                       ∧ not (HM.null (bdMaterials d))
                       ∧ not (materialsSatisfied inst d)
                Nothing → False
            texHandle = case mDef of
                Just def
                    | isGhost   → bdTexture def
                    | otherwise → pickBuildingFrame now inst def
                Nothing → biTexture inst

            (texW, texH) = case HM.lookup texHandle texSizes of
                Just (w, h) → (fromIntegral w, fromIntegral h)
                Nothing     → (baseTileW, baseTileH)

            scaleX = texW / baseTileW
            scaleY = texH / baseTileH
            quadW = tileWidth  * scaleX
            quadH = tileHeight * scaleY

            -- Anchor at the bottom-left tile of the footprint. We
            -- offset to the center of that tile for the iso math, the
            -- same way units use their float (gx, gy) center.
            gxF = fromIntegral (biAnchorX inst) + 0.5
            gyF = fromIntegral (biAnchorY inst) + 0.5
            (faF, fbF) = applyFacingF facing gxF gyF

            rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
            rawY = (faF + fbF) * tileHalfDiamondHeight

            heightOffset = fromIntegral relativeZ * tileSideHeight

            -- bdSpriteAnchor = "tile_bottom" lets the texture include
            -- the cube's side face (16 px on the standard 96×64 tile).
            -- We then push the quad DOWN by tileSideHeight so the
            -- texture's bottom edge lines up with the world tile's
            -- side-face bottom instead of dangling past it.
            anchorOffset = case mDef of
                Just d  | bdSpriteAnchor d ≡ "tile_bottom" → tileSideHeight
                _                                          → 0

            drawX = rawX + (tileWidth - quadW) * 0.5
            drawY = rawY - heightOffset
                  + tileHalfDiamondHeight - quadH + anchorOffset

            -- Sort by the iso depth of the GROUND TILE, not the sprite
            -- top. Adding spriteRowSpan (the sprite's vertical extent)
            -- to the sort key as units do made tall buildings — e.g.
            -- a 96×96 cargo hold has spriteRowSpan ≈ 2.0 — outrank
            -- units at the same tile, drawing the building on top of
            -- a unit standing in front of it. Keeping just the iso
            -- bottom plus the +0.0005 tiebreaker means a unit at the
            -- same row sorts in front (their key has +0.0006), and
            -- units north of the building still get obscured because
            -- their key is lower (north = smaller faF + fbF).
            sortKey = (faF + fbF)
                    + fromIntegral relativeZ * 0.001
                    + 0.0005

            actualSlot = lookupSlot texHandle
            ghostFactor = if isGhost then 0.6 else 1.0
            tint = Vec4 1.0 1.0 1.0 (tileAlpha * ghostFactor)
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

-- | Render the ghost preview if one is set. Returns at most one quad,
--   in a vector for caller convenience. Tinted: valid → soft-white
--   semi-transparent, invalid → red semi-transparent. This is the one
--   place RGB tinting is allowed by design (see the no-tinting rule).
renderGhostQuad ∷ EngineEnv → CameraFacing → Int → IO (V.Vector SortableQuad)
renderGhostQuad env facing zSlice = do
    mGhost ← readIORef (buildingGhostRef env)
    case mGhost of
        Nothing → return V.empty
        Just ghost → do
            bm ← readIORef (buildingManagerRef env)
            case HM.lookup (bgDefName ghost) (bmDefs bm) of
                Nothing → return V.empty
                Just def → do
                    texSizes ← readIORef (textureSizeRef env)
                    mBts ← readIORef (textureSystemRef env)
                    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
                    case mBts of
                        Nothing → return V.empty
                        Just bts →
                            let lookupSlot h = getTextureSlotIndex h bts
                                defFmSlot = fromIntegral defFmSlotWord
                                texHandle = bdTexture def
                                (texW, texH) = case HM.lookup texHandle texSizes of
                                    Just (w, h) → (fromIntegral w, fromIntegral h)
                                    Nothing     → (baseTileW, baseTileH)
                                scaleX = texW / baseTileW
                                scaleY = texH / baseTileH
                                quadW = tileWidth  * scaleX
                                quadH = tileHeight * scaleY
                                gxF = fromIntegral (bgGridX ghost) + 0.5
                                gyF = fromIntegral (bgGridY ghost) + 0.5
                                (faF, fbF) = applyFacingF facing gxF gyF
                                rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
                                rawY = (faF + fbF) * tileHalfDiamondHeight
                                -- Ghost sits at zSlice so relativeZ = 0,
                                -- meaning we draw it at the current
                                -- slice level. Heightless preview is
                                -- intentional — we want the player to
                                -- see the building flat on the active
                                -- camera slice.
                                -- Same anchor logic as the placed-building
                                -- path: tile_bottom textures get pushed
                                -- down by tileSideHeight so their drawn
                                -- side face matches the world tile's.
                                anchorOffset =
                                    if bdSpriteAnchor def ≡ "tile_bottom"
                                    then tileSideHeight else 0
                                drawX = rawX + (tileWidth - quadW) * 0.5
                                drawY = rawY
                                      + tileHalfDiamondHeight - quadH
                                      + anchorOffset
                                tint = if bgValid ghost
                                       then Vec4 1.0 1.0 1.0 0.6
                                       else Vec4 1.0 0.4 0.4 0.6
                                actualSlot = lookupSlot texHandle
                                sortKey = (faF + fbF) + quadH / tileHalfDiamondHeight * 0.5 + 0.01
                                v0 = Vertex (Vec2 drawX drawY)
                                             (Vec2 0 0) tint (fromIntegral actualSlot) defFmSlot 0
                                v1 = Vertex (Vec2 (drawX + quadW) drawY)
                                             (Vec2 1 0) tint (fromIntegral actualSlot) defFmSlot 0
                                v2 = Vertex (Vec2 (drawX + quadW) (drawY + quadH))
                                             (Vec2 1 1) tint (fromIntegral actualSlot) defFmSlot 0
                                v3 = Vertex (Vec2 drawX (drawY + quadH))
                                             (Vec2 0 1) tint (fromIntegral actualSlot) defFmSlot 0
                            in return $ V.singleton SortableQuad
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
