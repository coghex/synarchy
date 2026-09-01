{-# LANGUAGE Strict #-}
module Building.Render
    ( renderBuildingQuads
    , renderBuildingQuadsScanned
    , renderGhostQuad
    , renderGhostQuadScanned
    , ghostTint
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv, buildingGhostRef, buildingManagerRef
   )
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Scene.Types (SortableQuad(..), setQuadSolarPage)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..)
                                          , QuadPayload(..), quadVertices
                                          , rectCorners, fullQuadUV
                                          , renderFlagSelected, tileWorldUV
                                          , noFaceMapVertexId)
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , worldLayer, applyFacingF, baseTileW, baseTileH)
import Unit.Direction (Direction(..))
import World.State.Types (wmVisible)
import World.Page.Types (WorldPageId(..))
import Building.Types

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
        -- Buildings are never compiled to atlases (D-8): they carry
        -- their own per-frame `BuildingAnimation`, which #1261 split
        -- off the unit record when unit animations retired theirs.
        Just a  → case Map.lookup DirS (banFrames a) of
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
                                raw     = floor (elapsed * realToFrac (banFps a)) ∷ Int
                            in if banLoop a
                               then raw `mod` n
                               else min raw (n - 1)
                        idx
                          | pinLastFrame                              = n - 1
                          | activity ≡ Appearing ∧ bdBuildWork def > 0 = progressIdx
                          | otherwise                                  = timeIdx
                    in fs V.! idx

-- | Like 'Unit.Render.renderUnitQuads', one sweep over every visible
--   page's buildings, so each instance's quad takes ITS OWN page's
--   solar slot (#1869).
renderBuildingQuads ∷ EngineEnv → (WorldPageId → Word32) → CameraFacing → Int
                    → Int → Float → IO (V.Vector SortableQuad)
renderBuildingQuads env solarSlotOf facing zSlice effDepth tileAlpha =
    snd ⊚ renderBuildingQuadsScanned env solarSlotOf facing zSlice
                                     effDepth tileAlpha

-- | 'renderBuildingQuads' with the scene-assembly telemetry (#1921)
--   this pass contributes: the entries examined in the GLOBAL
--   building-manager map, paired with the quads it produced.
--
--   Counted before visible-page, texture-system and Z filtering, for
--   the same reason as 'Unit.Render.renderUnitQuadsScanned': the global
--   map is what the pass walks. It stays non-zero under GPU-free
--   headless execution, where emitted is legitimately zero.
renderBuildingQuadsScanned
    ∷ EngineEnv → (WorldPageId → Word32) → CameraFacing → Int
    → Int → Float → IO (Int, V.Vector SortableQuad)
renderBuildingQuadsScanned env solarSlotOf facing zSlice effDepth tileAlpha = do
    bm ← readIORef (buildingManagerRef env)
    -- Render only the visible worlds' buildings — buildings are
    -- world-scoped so a hidden world's must not draw here (#76).
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    let visiblePages = HS.fromList (wmVisible mgr)
        instances = buildingsOnPages visiblePages (bmInstances bm)
        defs      = bmDefs bm
        selected  = bmSelected bm
        scanned   = HM.size (bmInstances bm)
    if HM.null instances
        then return (scanned, V.empty)
        else do
            -- Game-clock matches biSpawnedAt's clock, so the
            -- Appearing→Built transition derived from elapsed time
            -- doesn't run while paused.
            now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
            texSizes ← readIORef (rvTextureSizeRef (toRenderViewCapability env))
            mBts ← readIORef (rvTextureSystemRef (toRenderViewCapability env))
            case mBts of
                Nothing → return (scanned, V.empty)
                Just _bts → do
                    -- Stable handle id resolved in the shader (#286);
                    -- buildings carry no directional face map (#1696).
                    let lookupSlot h = fromIntegral (toInt h) ∷ Word32
                        defFmSlot = noFaceMapVertexId
                        quads = V.fromList
                            $ HM.foldlWithKey' (\acc bid inst →
                                let mDef  = HM.lookup (biDefName inst) defs
                                    isSel = selected ≡ Just bid
                                in case buildingToQuad lookupSlot defFmSlot facing
                                                zSlice effDepth tileAlpha isSel inst mDef
                                                now texSizes of
                                    Just sq →
                                        setQuadSolarPage
                                            (solarSlotOf (biPage inst)) sq : acc
                                    Nothing → acc
                              ) [] instances
                    return (scanned, quads)

buildingToQuad
    ∷ (TextureHandle → Word32)
    → Float
    → CameraFacing
    → Int
    → Int                                   -- ^ effDepth (terrain view depth)
    → Float
    → Bool                                  -- ^ selected (sets outline bit)
    → BuildingInstance
    → Maybe BuildingDef
    → Double
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
buildingToQuad lookupSlot defFmSlot facing zSlice effDepth tileAlpha isSel inst mDef now texSizes =
    let gridZ = biGridZ inst
        relativeZ = gridZ - zSlice
        -- Match the terrain band (see Unit.Render): cull only above the
        -- slice or past the view depth, not for being below the camera.
    in if gridZ > zSlice ∨ gridZ < (zSlice - effDepth)
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
            wuv = tileWorldUV (biAnchorX inst) (biAnchorY inst)

            (v0, v1, v2, v3) =
                quadVertices (rectCorners (Vec2 drawX drawY) (Vec2 quadW quadH))
                             fullQuadUV
                             QuadPayload
                                 { qpTint      = tint
                                 , qpAtlasSlot = fromIntegral actualSlot
                                 , qpFaceMap   = defFmSlot
                                 , qpFlags     = flags
                                 , qpWorldUV   = wuv
                                 }

        in Just SortableQuad
            { sqSortKey = sortKey
            , sqV0      = v0
            , sqV1      = v1
            , sqV2      = v2
            , sqV3      = v3
            , sqTexture = texHandle
            , sqLayer   = worldLayer
            }

-- | The ghost preview's validity → RGBA tint (#778): neutral white
--   translucent when valid, red-dominant translucent when invalid —
--   the one place RGB tinting is allowed by design (see the
--   no-tinting rule). A standalone pure function (not inlined into
--   'renderGhostQuad') so the decision is Hspec-testable without a
--   texture system / GPU.
ghostTint ∷ Bool → Vec4
ghostTint valid
    | valid     = Vec4 1.0 1.0 1.0 0.6
    | otherwise = Vec4 1.0 0.4 0.4 0.6

-- | Render the ghost preview if one is set. Returns at most one quad,
--   in a vector for caller convenience. Tinted via 'ghostTint'.
--
--   The ghost has no page of its own — it previews a placement on
--   whichever page is active — so it takes the ACTIVE page's solar slot
--   (#1869), which is the page the placement will land on.
renderGhostQuad ∷ EngineEnv → Word32 → CameraFacing → Int
                → IO (V.Vector SortableQuad)
renderGhostQuad env solarSlot facing zSlice =
    snd ⊚ renderGhostQuadScanned env solarSlot facing zSlice

-- | 'renderGhostQuad' with the scene-assembly telemetry (#1921) this
--   pass contributes: the optional ghost CANDIDATE — zero or one —
--   paired with the quad it produced.
--
--   Counted from the presence of the ghost alone, before the
--   definition lookup and the texture-system check reject it, since
--   those rejections are exactly what an emitted count of zero beside
--   a scanned count of one records.
renderGhostQuadScanned ∷ EngineEnv → Word32 → CameraFacing → Int
                       → IO (Int, V.Vector SortableQuad)
renderGhostQuadScanned env solarSlot facing zSlice = do
    mGhost ← readIORef (buildingGhostRef env)
    case mGhost of
        Nothing → return (0, V.empty)
        Just ghost → do
            bm ← readIORef (buildingManagerRef env)
            case HM.lookup (bgDefName ghost) (bmDefs bm) of
                Nothing → return (1, V.empty)
                Just def → do
                    texSizes ← readIORef (rvTextureSizeRef (toRenderViewCapability env))
                    mBts ← readIORef (rvTextureSystemRef (toRenderViewCapability env))
                    case mBts of
                        Nothing → return (1, V.empty)
                        Just _bts →
                            -- Stable handle id resolved in the shader (#286);
                            -- buildings carry no directional face map (#1696).
                            let lookupSlot h = fromIntegral (toInt h) ∷ Word32
                                defFmSlot = noFaceMapVertexId
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
                                -- Lift the ghost to the terrain Z that
                                -- the placed building will land at,
                                -- mirroring `buildingToQuad`. Without
                                -- this the ghost stays glued to the
                                -- camera slice while the cursor + the
                                -- about-to-be-placed building both sit
                                -- at terrainZ, producing a visible
                                -- offset on non-flat terrain.
                                relativeZ = bgGridZ ghost - zSlice
                                heightOffset =
                                    fromIntegral relativeZ * tileSideHeight
                                -- Same anchor logic as the placed-building
                                -- path: tile_bottom textures get pushed
                                -- down by tileSideHeight so their drawn
                                -- side face matches the world tile's.
                                anchorOffset =
                                    if bdSpriteAnchor def ≡ "tile_bottom"
                                    then tileSideHeight else 0
                                drawX = rawX + (tileWidth - quadW) * 0.5
                                drawY = rawY - heightOffset
                                      + tileHalfDiamondHeight - quadH
                                      + anchorOffset
                                tint = ghostTint (bgValid ghost)
                                actualSlot = lookupSlot texHandle
                                sortKey = (faF + fbF) + quadH / tileHalfDiamondHeight * 0.5 + 0.01
                                wuv = tileWorldUV (bgGridX ghost) (bgGridY ghost)
                                (v0, v1, v2, v3) =
                                    quadVertices
                                        (rectCorners (Vec2 drawX drawY)
                                                     (Vec2 quadW quadH))
                                        fullQuadUV
                                        QuadPayload
                                            { qpTint      = tint
                                            , qpAtlasSlot = fromIntegral actualSlot
                                            , qpFaceMap   = defFmSlot
                                            , qpFlags     = 0
                                            , qpWorldUV   = wuv
                                            }
                            in return $ (,) 1 $ V.singleton
                                $ setQuadSolarPage solarSlot SortableQuad
                                { sqSortKey = sortKey
                                , sqV0      = v0
                                , sqV1      = v1
                                , sqV2      = v2
                                , sqV3      = v3
                                , sqTexture = texHandle
                                , sqLayer   = worldLayer
                                }
