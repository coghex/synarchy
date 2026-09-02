{-# LANGUAGE Strict #-}
-- | The ONE facing-aware building visual boundary (BDA-2, #2088):
--   which asset a building shows from the active camera facing, and
--   where that asset's quad sits in world space.
--
--   'Building.Render' draws from it and 'Building.HitTest' sizes its
--   click target from it, so the two cannot decide different assets or
--   different geometry — the drift this slice removed was exactly a
--   hit-test that sized from the instance's south handle and never
--   applied the sprite anchor the renderer did. The committed
--   building designation (#1845) consumes the same functions when it
--   replaces its generic marker.
--
--   Everything here is PURE and takes the camera facing, the instance,
--   the definition, the clock and the texture-size lookup as explicit
--   arguments. That is not a style choice: the scanned render entry
--   points emit nothing without a texture system (the headless state),
--   so render/hit-test agreement is only assertable through functions
--   that need no GPU.
--
--   Camera facing maps DIRECTLY onto the declared view — south, west,
--   north, east — and is never composed with a stored building
--   orientation: none exists ('BuildingInstance' carries no facing,
--   and no save field moved). Rotation changes presentation only.
module Building.Visual
    ( -- * Which asset is on screen
      pickBuildingFrame
    , BuildingVisual(..)
    , placedBuildingVisual
    , previewBuildingTexture
    , isPreDeliveryGhost
      -- * Where it is drawn
    , BuildingQuadRect(..)
    , buildingQuadRect
    , spriteAnchorOffset
    , placedBuildingQuad
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (CameraFacing)
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , applyFacingF, baseTileW, baseTileH)
import Building.Schema
import Building.Types

-- | Pick the frame a building shows from @facing@ at the given game
--   time. Mirrors Unit.Render.pickFrame but simpler — no reverse
--   playback and no mirror flag, because a building declares each of
--   its four views independently (#2080) and this reads the declared
--   view for the active camera (#2088).
--
--   Lifecycle phase is FACING-INDEPENDENT: 'currentActivity' derives it
--   from progress / the clock alone, so a camera turn never flips
--   Constructing / Appearing / Built. The frame INDEX is derived from
--   the selected direction's own real frame count with the same rule
--   at every facing — progress fraction while constructing, elapsed
--   time × fps (looped or clamped) for the timed roles, last-frame
--   pinning once Built — so rotating selects the same semantic frame
--   from the new direction without restarting, advancing or rewinding
--   anything.
pickBuildingFrame ∷ CameraFacing → Double → BuildingInstance → BuildingDef
                  → TextureHandle
pickBuildingFrame facing now inst def =
    let activity  = currentActivity now inst def
        stateRole = case activity of
                      Constructing → RoleConstruction
                      Appearing    → RoleAppearance
                      Built        → RoleBuilt
        -- The role a Built building pins its last frame from when it
        -- declares no `built` animation: whichever role its own
        -- build_work put it through on the way up (#2080). Reading
        -- the definition's own discriminator, rather than one fixed
        -- role, is what keeps Cargo Hold / Furnace / Workbench /
        -- Machine Shop pinning their final construction frame instead
        -- of snapping back to the static sprite.
        pinRole = legacyRoleFor (bdBuildWork def)
        -- Find the animation for the current role. If we're Built and
        -- no "built" animation is defined, fall back to the LAST frame
        -- of `pinRole` so the visible sprite doesn't snap back to the
        -- static sprite (which may differ from the final construction
        -- frame). pinLastFrame flags that mode.
        (mAnim, pinLastFrame) =
            case Map.lookup stateRole (bdRoleAnims def) of
                Just animName
                    | Just a ← HM.lookup animName (bdAnimations def)
                    → (Just a, False)
                _ → case activity of
                    Built → case Map.lookup pinRole (bdRoleAnims def) of
                        Just animName →
                            (HM.lookup animName (bdAnimations def), True)
                        Nothing → (Nothing, False)
                    _ → (Nothing, False)
        -- The static view for this facing: what a building with no
        -- applicable animation shows, and what an animation with no
        -- frames in this direction falls back to.
        static = previewBuildingTexture facing def
    in case mAnim of
        Nothing → static
        -- Buildings are never compiled to atlases (D-8): they carry
        -- their own per-frame `BuildingAnimation`, which #1261 split
        -- off the unit record when unit animations retired theirs.
        Just a  →
            let fs = facingAsset facing (banFrames a)
            in if V.null fs then static else
                    let n = V.length fs
                        -- Worker-driven construction: while
                        -- Constructing the visible frame tracks
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
                          | pinLastFrame          = n - 1
                          | activity ≡ Constructing = progressIdx
                          | otherwise               = timeIdx
                    in fs V.! idx

-- | The one observable visual sample of a placed building: the handle
--   on screen, and whether it is drawn as the translucent pre-delivery
--   ghost rather than the solid building.
data BuildingVisual = BuildingVisual
    { bvTexture ∷ !TextureHandle
    , bvGhost   ∷ !Bool
      -- ^ Placed, but its materials gate is not satisfied yet: the
      --   facing's STATIC view at ghost opacity, until delivery
      --   completes and construction takes over.
    } deriving (Show, Eq)

-- | A placed building whose materials gate has not been satisfied:
--   rendered as a translucent silhouette of its final form so the
--   player sees what will land here once delivery completes.
isPreDeliveryGhost ∷ BuildingInstance → BuildingDef → Bool
isPreDeliveryGhost inst def =
    bdBuildWork def > 0
    ∧ not (HM.null (bdMaterials def))
    ∧ not (materialsSatisfied inst def)

-- | The static view a placement preview or a pre-delivery ghost shows
--   from @facing@ — the facing's own declared sprite, never a mirrored
--   or substituted direction. A legacy declaration exposes its one
--   path through all four views, so it renders identically at every
--   facing by construction.
previewBuildingTexture ∷ CameraFacing → BuildingDef → TextureHandle
previewBuildingTexture facing = facingAsset facing ∘ bdTextures

-- | Resolve which handle a placed instance shows from @facing@ at game
--   time @now@. Three cases, and rendering and hit-testing take all
--   three from HERE:
--
--   * the definition is present and the materials gate is open (or
--     absent): the lifecycle frame from 'pickBuildingFrame';
--   * the definition is present but materials are outstanding: the
--     facing's static view, flagged as a ghost;
--   * the definition is MISSING from the manager (a save naming a def
--     the player removed): the single handle stamped on the instance,
--     facing-blind, because there is no declaration to select a view
--     from. Render and hit-test agree in that state too.
placedBuildingVisual ∷ CameraFacing → Double → BuildingInstance
                     → Maybe BuildingDef → BuildingVisual
placedBuildingVisual facing now inst mDef = case mDef of
    Nothing → BuildingVisual (biTexture inst) False
    Just def
        | isPreDeliveryGhost inst def →
            BuildingVisual (previewBuildingTexture facing def) True
        | otherwise →
            BuildingVisual (pickBuildingFrame facing now inst def) False

-- | Where a building's quad sits in world units: its top-left corner
--   and size, plus the iso depth of its anchor tile under the facing
--   (the term both sort keys are built on).
data BuildingQuadRect = BuildingQuadRect
    { bqX        ∷ !Float
    , bqY        ∷ !Float
    , bqW        ∷ !Float
    , bqH        ∷ !Float
    , bqIsoDepth ∷ !Float
      -- ^ @faF + fbF@ of the anchor tile's centre after the facing
      --   rotation: the ground tile's iso depth, texture-independent.
    } deriving (Show, Eq)

-- | How far the quad is pushed DOWN so the texture's bottom edge lands
--   where the definition says it should. @bdSpriteAnchor =
--   "tile_bottom"@ lets the texture include the cube's side face (16 px
--   on the standard 96×64 tile), so the quad drops by 'tileSideHeight'
--   and the drawn side face lines up with the world tile's instead of
--   dangling past it; @"diamond_bottom"@ (the default) and a missing
--   definition anchor at the south point of the top face.
spriteAnchorOffset ∷ Maybe BuildingDef → Float
spriteAnchorOffset mDef = case mDef of
    Just d | bdSpriteAnchor d ≡ "tile_bottom" → tileSideHeight
    _                                          → 0

-- | The quad for texture @tex@ anchored at tile @(gx, gy)@ on grid
--   level @gz@, seen from @facing@ with the camera at @zSlice@.
--
--   Width and height come from the SELECTED texture's own pixel size
--   (a handle the size table does not know draws at the base tile
--   size), so a facing whose authored canvas differs changes the quad
--   — but never what it is anchored to: the quad is centred on the
--   footprint's anchor tile and its bottom edge sits on that tile's
--   iso bottom (plus the sprite-anchor drop) at every facing, the same
--   way units use their float @(gx, gy)@ centre.
buildingQuadRect
    ∷ CameraFacing
    → Int                                   -- ^ camera z slice
    → HM.HashMap TextureHandle (Int, Int)   -- ^ texture pixel sizes
    → Float                                 -- ^ 'spriteAnchorOffset'
    → Int → Int → Int                       -- ^ anchor gx, gy, grid z
    → TextureHandle
    → BuildingQuadRect
buildingQuadRect facing zSlice texSizes anchorOffset gx gy gz tex =
    let (texW, texH) = case HM.lookup tex texSizes of
            Just (w, h) → (fromIntegral w, fromIntegral h)
            Nothing     → (baseTileW, baseTileH)
        quadW = tileWidth  * (texW / baseTileW)
        quadH = tileHeight * (texH / baseTileH)
        -- Anchor at the bottom-left tile of the footprint, offset to
        -- the centre of that tile for the iso math.
        gxF = fromIntegral gx + 0.5
        gyF = fromIntegral gy + 0.5
        (faF, fbF) = applyFacingF facing gxF gyF
        rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
        rawY = (faF + fbF) * tileHalfDiamondHeight
        heightOffset = fromIntegral (gz - zSlice) * tileSideHeight
        drawX = rawX + (tileWidth - quadW) * 0.5
        drawY = rawY - heightOffset
              + tileHalfDiamondHeight - quadH + anchorOffset
    in BuildingQuadRect
        { bqX = drawX, bqY = drawY, bqW = quadW, bqH = quadH
        , bqIsoDepth = faF + fbF }

-- | The visual sample AND the quad of a placed instance, from one
--   call: what 'Building.Render.buildingToQuad' draws and what
--   'Building.HitTest.hitTestBuildingAt' tests the click against.
placedBuildingQuad
    ∷ CameraFacing → Double → Int → HM.HashMap TextureHandle (Int, Int)
    → BuildingInstance → Maybe BuildingDef
    → (BuildingVisual, BuildingQuadRect)
placedBuildingQuad facing now zSlice texSizes inst mDef =
    let visual = placedBuildingVisual facing now inst mDef
        rect   = buildingQuadRect facing zSlice texSizes
                     (spriteAnchorOffset mDef)
                     (biAnchorX inst) (biAnchorY inst) (biGridZ inst)
                     (bvTexture visual)
    in (visual, rect)
