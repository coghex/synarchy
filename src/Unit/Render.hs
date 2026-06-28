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
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..)
                                          , renderFlagSelected)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , worldLayer, applyFacing, GridConfig(..), defaultGridConfig)
import World.State.Types (wmVisible)
import Unit.Types
import Unit.Direction (Direction(..), dirIndex, indexToDir, mirrorDir)

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

-- | Choose a texture for a unit. If the unit has an active animation and
--   the requested frames exist, pick by elapsed time; otherwise fall back
--   to the T-pose. Used by the render path.
--
--   Returns @(handle, flipX)@. `flipX` is `True` when the texture was
--   resolved via `mirrorDir` (the western directions falling back to
--   their eastern counterparts); the renderer swaps U coordinates to
--   produce the horizontal flip.
pickFrame
    ∷ Double            -- ^ now (POSIX seconds)
    → CameraFacing
    → UnitInstance
    → UnitDef           -- ^ animation library + T-pose fallback
    → (TextureHandle, Bool)
pickFrame now cam inst def
    | T.null (uiCurrentAnim inst) = tpose
    | otherwise =
        case HM.lookup (uiCurrentAnim inst) (udAnimations def) of
            Nothing  → tpose
            Just an  →
                let dir = screenDirOf cam (uiFacing inst)
                in case lookupFlip (aFlip an) dir (aFrames an) of
                    Nothing            → tpose
                    Just (fs, flipX)
                        | V.null fs → tpose
                        | otherwise →
                            let elapsed = max 0 (now - uiAnimStart inst)
                                raw     = floor (elapsed * realToFrac (aFps an)) ∷ Int
                                n       = V.length fs
                                -- Stride > 1 skips frames: stride 2
                                -- shows frames 0, 2, 4, … so a 9-frame
                                -- transition completes in half the time.
                                stride  = max 1 (uiAnimStride inst)
                                strided = raw * stride
                                -- `uiForceLoop` is the debug
                                -- anim-panel hook: when set, treat
                                -- the anim as if `aLoop` were True
                                -- so one-shots (attacks, transitions,
                                -- death) cycle continuously during
                                -- preview instead of holding their
                                -- last frame.
                                doLoop  = aLoop an ∨ uiForceLoop inst
                                fwdIdx  = if doLoop
                                          then strided `mod` n
                                          else min strided (n - 1)
                                -- Reverse path: walk the frames from
                                -- the last index toward 0. Mirrors the
                                -- forward clamp so we hold frame 0
                                -- when "elapsed" runs out — for a
                                -- reverse-direction pose transition,
                                -- frame 0 is the "destination" pose.
                                idx     = if uiAnimReverse inst
                                          then (n - 1) - fwdIdx
                                          else fwdIdx
                            in (fs V.! idx, flipX)
  where
    tpose = resolveTexture cam (uiFacing inst)
                           (uiDirSprites inst) (uiTexture inst)
    -- `flipOK` from the animation's `aFlip` flag gates the mirror
    -- fallback. When False we deliberately do NOT mirror — an anim
    -- with an asymmetric held prop (weapon in right hand) would
    -- otherwise have the prop visually swap sides on western
    -- directions. Author sets `flip: false` (or omits) to opt out.
    lookupFlip flipOK d m = case Map.lookup d m of
        Just v  → Just (v, False)
        Nothing
          | not flipOK → Nothing
          | otherwise  → case mirrorDir d of
              Just md → (\v → (v, True)) <$> Map.lookup md m
              Nothing → Nothing

renderUnitQuads ∷ EngineEnv → CameraFacing → Int → Int → Float → IO (V.Vector SortableQuad)
renderUnitQuads env facing zSlice effDepth tileAlpha = do
    um ← readIORef (unitManagerRef env)
    -- Render only units of the VISIBLE worlds — units are world-scoped, so
    -- a hidden world's units must not draw over the active one (#78).
    mgr ← readIORef (worldManagerRef env)
    let visiblePages = HS.fromList (wmVisible mgr)
        instances = unitsOnPages visiblePages (umInstances um)
        defs      = umDefs um
        selected  = umSelected um
    if HM.null instances
        then return V.empty
        else do
            -- Read the game-clock (advances only when not paused) so
            -- the rendered frame index matches the uiAnimStart values
            -- published in game-time.
            now ← readIORef (gameTimeRef env)
            texSizes ← readIORef (textureSizeRef env)
            mBts ← readIORef (textureSystemRef env)
            case mBts of
                Nothing → return V.empty
                Just _bts → do
                    -- Bake a STABLE texture-handle id; the bindless shader
                    -- resolves it to a live slot at draw time (#286). -1 =
                    -- "use the default face map" (units have no directional
                    -- face map) — the shader maps it to the default slot.
                    let lookupSlot h = fromIntegral (toInt h) ∷ Word32
                        defFmSlot = -1 ∷ Float
                        quads = V.fromList
                            $ HM.foldlWithKey' (\acc uid inst →
                                let isSel = HS.member uid selected
                                    mDef  = HM.lookup (uiDefName inst) defs
                                in case unitToQuad lookupSlot defFmSlot facing
                                                zSlice effDepth tileAlpha isSel inst
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
    → Int                                  -- ^ effDepth (terrain view depth)
    → Float
    → Bool                                 -- ^ selected (sets outline bit)
    → UnitInstance
    → Maybe UnitDef                        -- ^ animation library (Nothing → T-pose only)
    → Double                               -- ^ now (POSIX seconds)
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
unitToQuad lookupSlot defFmSlot facing zSlice effDepth tileAlpha isSel inst mDef now texSizes =
    let gridZ = uiGridZ inst
        -- Use the continuous uiRealZ for the visual vertical offset
        -- so climbs interpolate smoothly. Cull / slice math still
        -- consults the integer uiGridZ — visibility is per-tile.
        relativeZf = uiRealZ inst - fromIntegral zSlice
        -- Visible band matches the terrain (Quads.hs): culled only when
        -- ABOVE the slice (camera below the unit) or beyond the view
        -- depth. The old fixed `zSlice - 25` lower bound (= camera's own
        -- terrain level, since z-tracking sets zSlice = camTerrain + 25)
        -- wrongly hid every unit standing below the camera — e.g. units
        -- at a cliff base seen from the top.
    in if gridZ > zSlice ∨ gridZ < (zSlice - effDepth)
       then Nothing
       else
        let (texHandle, flipX) = case mDef of
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

            heightOffset = relativeZf * tileSideHeight
            baseRadius = uiBaseWidth inst * 0.5 / baseTileH * tileHeight

            drawX = rawX + (tileWidth - quadW) * 0.5
            -- Foot anchor: the unit feeds its CONTINUOUS position
            -- (uiGridX/Y = usRealX/Y) straight through applyFacingF, so
            -- rawY is already the ground-point projection — the diamond
            -- CENTRE of the tile it stands on. (Flora and ground items
            -- feed INTEGER tile coords, so their rawY is the diamond
            -- APEX and they add tileHalfDiamondHeight to reach the
            -- centre; the unit must NOT — doing so dropped the feet a
            -- half-diamond down onto the tile's vertical side face.)
            drawY = rawY - heightOffset - quadH + baseRadius

            -- Painter sort anchored at the unit's FOOT row (faF+fbF),
            -- with z as a sub-row tiebreak and a small constant nudge so
            -- the unit draws just above the terrain/fluid at its own tile.
            -- This matches the flora/ground-item convention.
            --
            -- It deliberately does NOT add a "sprite row span" forward
            -- push. A tall sprite spans more than one screen row, so a
            -- push sized to its height (~1.33 rows for a 1:1 sprite, more
            -- for taller units) exceeded a full row and let an elevated /
            -- climbing unit out-sort — and draw OVER — a cliff a full row
            -- in FRONT of it. The screen row (faF+fbF) already orders the
            -- unit correctly against tiles ahead of and behind it; the
            -- sprite extends upward from the foot, over the rows behind,
            -- which the row term already handles.
            normalSort = (faF + fbF)
                       + relativeZf * 0.001
                       + 0.0006
            -- Far-side climb occlusion: while climbing onto a cliff
            -- column whose face is between the unit and the camera (its
            -- screen-row is in FRONT of the unit's frozen base), sort the
            -- unit just BEHIND that column so the cliff hides it. The
            -- spriteRowSpan forward-push would otherwise draw the climber
            -- OVER the column it's climbing. Only applies while the unit
            -- is still on the base side (its tile ≠ the dest column); once
            -- the pullup carries its xy onto the top tile it falls back to
            -- normalSort and emerges in front.
            baseTile = (floor (uiGridX inst) ∷ Int, floor (uiGridY inst) ∷ Int)
            sortKey = case uiClimbDest inst of
                Just dest@(dx, dy) | baseTile ≢ dest →
                    let (bfa, bfb) = applyFacing facing (fst baseTile) (snd baseTile)
                        (dfa, dfb) = applyFacing facing dx dy
                    in if (dfa + dfb) > (bfa + bfb)
                       then fromIntegral (dfa + dfb) - 0.5  -- behind the column
                       else normalSort
                _ → normalSort

            actualSlot = lookupSlot texHandle
            tint = Vec4 1.0 1.0 1.0 tileAlpha
            flags = if isSel then renderFlagSelected else 0

            -- Horizontal flip: swap U coordinates between left and right
            -- vertices. The geometry stays the same; only the texture
            -- sampling reads right-to-left. Lets western directions
            -- (SW/W/NW) be drawn from their eastern (SE/E/NE) sprites.
            (u0, u1, u2, u3) = if flipX
                               then (1, 0, 0, 1)
                               else (0, 1, 1, 0)

            v0 = Vertex (Vec2 drawX drawY)
                         (Vec2 u0 0) tint (fromIntegral actualSlot) defFmSlot flags
            v1 = Vertex (Vec2 (drawX + quadW) drawY)
                         (Vec2 u1 0) tint (fromIntegral actualSlot) defFmSlot flags
            v2 = Vertex (Vec2 (drawX + quadW) (drawY + quadH))
                         (Vec2 u2 1) tint (fromIntegral actualSlot) defFmSlot flags
            v3 = Vertex (Vec2 drawX (drawY + quadH))
                         (Vec2 u3 1) tint (fromIntegral actualSlot) defFmSlot flags

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
