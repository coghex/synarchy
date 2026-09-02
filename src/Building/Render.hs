{-# LANGUAGE Strict #-}
-- | Building quads for the scene: placed instances (solid, or the
--   translucent pre-delivery ghost), the transient destruction
--   presentations of demolished ones (#2091), and the build tool's
--   placement preview.
--
--   WHICH asset is drawn, and WHERE, both come from 'Building.Visual'
--   (#2088): the facing's own declared view — south, west, north, east,
--   never a stored orientation — at the lifecycle frame the progress /
--   clock selects, sized from that texture and anchored on the
--   footprint. 'Building.HitTest' reads the same functions, so the
--   click target is the visible quad by construction rather than a
--   second copy of the arithmetic. Camera rotation changes only the
--   selected canvas and the projection; footprint, grid position, z,
--   sort ownership and lifecycle progress are untouched by it.
--
--   A destruction effect goes through the SAME geometry boundary
--   ('Building.Visual.buildingQuadRect') from the anchor, z and
--   sprite-anchor drop it captured at demolition, with its frame from
--   'Building.Destruction'. It is drawn and nothing more: no selection
--   outline, no ghost opacity, and 'Building.HitTest' never sees it.
module Building.Render
    ( renderBuildingQuads
    , renderBuildingQuadsScanned
    , buildingToQuad
    , destructionToQuad
    , placedBuildingSortKey
    , renderGhostQuad
    , renderGhostQuadScanned
    , ghostToQuad
    , ghostTint
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
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
import World.Grid (tileHalfDiamondHeight, worldLayer)
import World.State.Types (wmVisible)
import World.Page.Types (WorldPageId(..))
import Building.Types
import Building.Visual
import Building.Destruction (destructionFrame, destructionsOnPages)

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
--   building-manager state — every live instance in 'bmInstances' PLUS
--   every stored destruction effect in 'bmDestructions' (#2091) —
--   paired with the quads it produced.
--
--   Counted before visible-page, texture-system and Z filtering, for
--   the same reason as 'Unit.Render.renderUnitQuadsScanned': the global
--   maps are what the pass walks. It stays non-zero under GPU-free
--   headless execution, where emitted is legitimately zero, and a frame
--   holding only effects — the moments after the last building on a
--   page is demolished — is still measured rather than reported as an
--   empty pass.
renderBuildingQuadsScanned
    ∷ EngineEnv → (WorldPageId → Word32) → CameraFacing → Int
    → Int → Float → IO (Int, V.Vector SortableQuad)
renderBuildingQuadsScanned env solarSlotOf facing zSlice effDepth tileAlpha = do
    bm ← readIORef (buildingManagerRef env)
    -- Render only the visible worlds' buildings — buildings are
    -- world-scoped so a hidden world's must not draw here (#76). The
    -- same scoping holds for a destruction effect: a hidden page's
    -- effect emits no quad, and if the page is shown again before the
    -- clip runs out it resumes at the phase the clock dictates.
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    let visiblePages = HS.fromList (wmVisible mgr)
        instances = buildingsOnPages visiblePages (bmInstances bm)
        effects   = destructionsOnPages visiblePages (bmDestructions bm)
        defs      = bmDefs bm
        selected  = bmSelected bm
        scanned   = HM.size (bmInstances bm) + HM.size (bmDestructions bm)
    if HM.null instances ∧ HM.null effects
        then return (scanned, V.empty)
        else do
            -- Game-clock matches biSpawnedAt's clock, so the
            -- Appearing→Built transition a zero-work def derives
            -- from elapsed time
            -- doesn't run while paused. A destruction effect's
            -- start is the same clock, so pause freezes its phase too.
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
                        withEffects = HM.foldl' (\acc eff →
                                case destructionToQuad lookupSlot defFmSlot facing
                                            zSlice effDepth tileAlpha eff now
                                            texSizes of
                                    Just sq →
                                        setQuadSolarPage
                                            (solarSlotOf (dePage eff)) sq : acc
                                    Nothing → acc
                              ) [] effects
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
                              ) withEffects instances
                    return (scanned, quads)

-- | One placed instance's quad, or 'Nothing' when the camera band
--   culls it. Pure — exported so the render/hit-test agreement is
--   assertable without a texture system (#2088), the way
--   'Unit.Render.unitToQuad' is for units.
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
        let -- The ONE visual decision, shared with the hit test: which
            -- handle this facing shows (the lifecycle frame, or the
            -- static view at ghost opacity while materials are
            -- outstanding, or the stamped handle when the def is
            -- gone), sized from that handle and anchored on the
            -- footprint.
            (visual, rect) = placedBuildingQuad facing now zSlice texSizes
                                                inst mDef
            texHandle = bvTexture visual
            isGhost   = bvGhost visual
            BuildingQuadRect
                { bqX = drawX, bqY = drawY, bqW = quadW, bqH = quadH
                , bqIsoDepth = isoDepth } = rect

            sortKey = placedBuildingSortKey isoDepth relativeZ

            actualSlot = lookupSlot texHandle
            -- Pre-delivery ghost: 0.6 alpha, matching the placement-
            -- time ghost, so the player sees a translucent silhouette
            -- of what'll land here once delivery completes.
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

-- | The sort key a PLACED building's quad carries, and — so a
--   demolition changes nothing about where its footprint sorts — the
--   one its destruction effect carries too (#2091).
--
--   Sort by the iso depth of the GROUND TILE, not the sprite top.
--   Adding spriteRowSpan (the sprite's vertical extent) to the sort
--   key as units do made tall buildings — e.g. a 96×96 cargo hold has
--   spriteRowSpan ≈ 2.0 — outrank units at the same tile, drawing the
--   building on top of a unit standing in front of it. Keeping just
--   the iso bottom plus the +0.0005 tiebreaker means a unit at the
--   same row sorts in front (their key has +0.0006), and units north
--   of the building still get obscured because their key is lower
--   (north = smaller faF + fbF). Texture-independent, so a facing
--   whose canvas differs cannot move a placed building in the sort.
placedBuildingSortKey ∷ Float → Int → Float
placedBuildingSortKey isoDepth relativeZ =
    isoDepth + fromIntegral relativeZ * 0.001 + 0.0005

-- | One destruction effect's quad at game time @now@, or 'Nothing'
--   when the camera band culls it, the clip has expired, or the facing
--   declares no frame (#2091). Pure — exported so the timing, facing
--   and geometry contracts are assertable without a texture system,
--   exactly as 'buildingToQuad' is.
--
--   Geometry is the placed building's: 'buildingQuadRect' from the
--   captured anchor, grid z and sprite-anchor drop, sized from the
--   selected frame's own canvas, the same ground contact, world UV,
--   layer and 'placedBuildingSortKey'. What is deliberately NOT the
--   placed building's: no selection outline (the effect is never
--   selected and never hit-tested) and no pre-delivery ghost opacity —
--   the tint is exactly the scene's @tileAlpha@ whatever the building
--   looked like when it was demolished.
destructionToQuad
    ∷ (TextureHandle → Word32)
    → Float
    → CameraFacing
    → Int
    → Int                                   -- ^ effDepth (terrain view depth)
    → Float                                 -- ^ scene tileAlpha
    → DestructionEffect
    → Double                                -- ^ game time now
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
destructionToQuad lookupSlot defFmSlot facing zSlice effDepth tileAlpha eff now texSizes =
    let gridZ = deGridZ eff
        relativeZ = gridZ - zSlice
    in if gridZ > zSlice ∨ gridZ < (zSlice - effDepth)
       then Nothing
       else case destructionFrame facing now eff of
        Nothing → Nothing
        Just texHandle →
            let BuildingQuadRect
                    { bqX = drawX, bqY = drawY, bqW = quadW, bqH = quadH
                    , bqIsoDepth = isoDepth } =
                    buildingQuadRect facing zSlice texSizes
                                     (deAnchorOffset eff)
                                     (deAnchorX eff) (deAnchorY eff) gridZ
                                     texHandle
                sortKey = placedBuildingSortKey isoDepth relativeZ
                actualSlot = lookupSlot texHandle
                tint = Vec4 1.0 1.0 1.0 tileAlpha
                wuv = tileWorldUV (deAnchorX eff) (deAnchorY eff)
                (v0, v1, v2, v3) =
                    quadVertices (rectCorners (Vec2 drawX drawY) (Vec2 quadW quadH))
                                 fullQuadUV
                                 QuadPayload
                                     { qpTint      = tint
                                     , qpAtlasSlot = fromIntegral actualSlot
                                     , qpFaceMap   = defFmSlot
                                     , qpFlags     = 0
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
                            in return $ (,) 1 $ V.singleton
                                $ setQuadSolarPage solarSlot
                                $ ghostToQuad lookupSlot noFaceMapVertexId
                                              facing zSlice texSizes ghost def

-- | The placement preview's quad: the facing's STATIC view of the
--   definition (#2088), placed exactly as 'buildingToQuad' would place
--   the building once committed. Pure — exported so the facing
--   selection and placement are assertable without a texture system.
ghostToQuad
    ∷ (TextureHandle → Word32)
    → Float
    → CameraFacing
    → Int
    → HM.HashMap TextureHandle (Int, Int)
    → BuildingGhost
    → BuildingDef
    → SortableQuad
ghostToQuad lookupSlot defFmSlot facing zSlice texSizes ghost def =
    let texHandle = previewBuildingTexture facing def
        -- Lifted to the terrain Z the placed building will land at,
        -- with the same sprite-anchor drop, by the same function the
        -- placed path uses. Without the lift the ghost stays glued to
        -- the camera slice while the cursor + the about-to-be-placed
        -- building both sit at terrainZ, producing a visible offset
        -- on non-flat terrain.
        BuildingQuadRect
            { bqX = drawX, bqY = drawY, bqW = quadW, bqH = quadH
            , bqIsoDepth = isoDepth } =
            buildingQuadRect facing zSlice texSizes
                             (spriteAnchorOffset (Just def))
                             (bgGridX ghost) (bgGridY ghost) (bgGridZ ghost)
                             texHandle
        tint = ghostTint (bgValid ghost)
        actualSlot = lookupSlot texHandle
        -- Unlike the placed key, this one carries the canvas height, so
        -- a facing whose authored canvas is taller legitimately sorts
        -- differently. Retained as it was (#2088 keeps the ghost sort
        -- formula out of scope).
        sortKey = isoDepth + quadH / tileHalfDiamondHeight * 0.5 + 0.01
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
    in SortableQuad
        { sqSortKey = sortKey
        , sqV0      = v0
        , sqV1      = v1
        , sqV2      = v2
        , sqV3      = v3
        , sqTexture = texHandle
        , sqLayer   = worldLayer
        }
