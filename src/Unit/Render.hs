{-# LANGUAGE Strict #-}
module Unit.Render
    ( renderUnitQuads
    , renderUnitQuadsScanned
    , unitToQuad
    , pickFrame
    , screenDirOf
    , resolveTexture
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (readIORef)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv, unitManagerRef)
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Scene.Types (SortableQuad(..), setQuadSolarPage)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..)
                                          , QuadUV(..), QuadPayload(..)
                                          , quadVertices, rectCorners
                                          , renderFlagSelected, packWorldUV
                                          , noFaceMapVertexId)
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , worldLayer, applyFacing, applyFacingF
                  , baseTileW, baseTileH)
import World.State.Types (wmVisible)
import World.Page.Types (WorldPageId(..))
import Unit.Types
import Unit.Direction (mirrorDir)
import Unit.Sprite (screenDirOf, resolveTexture)

unitSortNudge ∷ Float
unitSortNudge = 0.0003

-- `cameraRotSteps`, `screenDirOf`, and `resolveTexture` now live in
-- `Unit.Sprite` (shared with `Unit.HitTest`); re-exported above.

-- | Choose a frame for a unit. If the unit has an active animation and
--   the requested frames exist, pick by elapsed time; otherwise fall back
--   to the T-pose. Used by the render path and the hit-tester.
--
--   Returns a storage-neutral 'FrameSample': the stable bindless handle
--   (#286), the frame's own UV endpoints within that handle's image,
--   the frame's pixel dimensions when the storage knows them, and the
--   flip flag. `fsFlipX` is `True` when the frame was resolved via
--   `mirrorDir` (the western directions falling back to their eastern
--   counterparts); the renderer mirrors the sampled sub-rect to produce
--   the horizontal flip.
--
--   THE FRAME-INDEX ARITHMETIC BELOW IS FROZEN (#1259, D-3). Atlas
--   storage changed where a frame's pixels live and how they are
--   addressed; it changed nothing about WHICH logical frame plays. The
--   only storage-dependent step is the per-direction frame COUNT —
--   'storageFrameCount', which for an atlas is the index's real count,
--   never the padded column count (D-5), so padding is unreachable by
--   construction.
pickFrame
    ∷ Double            -- ^ now (POSIX seconds)
    → CameraFacing
    → UnitInstance
    → UnitDef           -- ^ animation library + T-pose fallback
    → FrameSample
pickFrame now cam inst def
    | T.null (uiCurrentAnim inst) = tpose
    | otherwise =
        case HM.lookup (uiCurrentAnim inst) (udAnimations def) of
            Nothing  → tpose
            Just an  →
                let dir = screenDirOf cam (uiFacing inst)
                    st  = aStorage an
                in case lookupFlip (aFlip an) dir st of
                    Nothing            → tpose
                    Just (srcDir, n, flipX)
                        | n ≤ 0 → tpose
                        | otherwise →
                            let elapsed = max 0 (now - uiAnimStart inst)
                                raw     = floor (elapsed * realToFrac (aFps an)) ∷ Int
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
                            -- The index is already inside the REAL
                            -- count, so a Nothing here would be a
                            -- storage bug rather than a padding cell;
                            -- the T-pose is the safe visible answer.
                            in fromMaybe tpose (storageSampleAt st srcDir idx flipX)
  where
    tpose = uncurry wholeImageSample $
        resolveTexture cam (uiFacing inst) (uiDirSprites inst) (uiTexture inst)
    -- `flipOK` from the animation's `aFlip` flag gates the mirror
    -- fallback. When False we deliberately do NOT mirror — an anim
    -- with an asymmetric held prop (weapon in right hand) would
    -- otherwise have the prop visually swap sides on western
    -- directions. Author sets `flip: false` (or omits) to opt out.
    --
    -- Returns the direction the frames were actually found under, so
    -- the sample is read from the SOURCE row (a mirrored SW reads
    -- SE's cells and flips them) — the mirror is a sampling effect,
    -- never a second copy of the art.
    lookupFlip flipOK d st = case storageFrameCount st d of
        Just n  → Just (d, n, False)
        Nothing
          | not flipOK → Nothing
          | otherwise  → case mirrorDir d of
              Just md → (\n → (md, n, True)) <$> storageFrameCount st md
              Nothing → Nothing

-- | Units are page-scoped, so each one's quad is attributed to ITS OWN
--   page's sun rather than the visible head's (#1869) — the pass draws
--   every visible page's units in one sweep, so the slot has to be
--   resolved per instance here rather than stamped over the result.
renderUnitQuads ∷ EngineEnv → (WorldPageId → Word32) → CameraFacing → Int → Int
                → Float → IO (V.Vector SortableQuad)
renderUnitQuads env solarSlotOf facing zSlice effDepth tileAlpha =
    snd ⊚ renderUnitQuadsScanned env solarSlotOf facing zSlice effDepth tileAlpha

-- | 'renderUnitQuads' with the scene-assembly telemetry (#1921) this
--   pass contributes: the entries examined in the GLOBAL unit-manager
--   map, paired with the quads it produced.
--
--   The count is taken before ANY filtering — 'unitsOnPages' narrows to
--   the visible pages, the texture-system check below rejects the whole
--   pass without one, and 'unitToQuad' applies the Z band — because the
--   global map is what this pass actually walks, and that is the cost
--   the measurement exists to expose. It therefore stays non-zero under
--   GPU-free headless execution, where emitted is legitimately zero.
renderUnitQuadsScanned
    ∷ EngineEnv → (WorldPageId → Word32) → CameraFacing → Int → Int
    → Float → IO (Int, V.Vector SortableQuad)
renderUnitQuadsScanned env solarSlotOf facing zSlice effDepth tileAlpha = do
    um ← readIORef (unitManagerRef env)
    -- Render only units of the VISIBLE worlds — units are world-scoped, so
    -- a hidden world's units must not draw over the active one (#78).
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    let visiblePages = HS.fromList (wmVisible mgr)
        instances = unitsOnPages visiblePages (umInstances um)
        defs      = umDefs um
        selected  = umSelected um
        scanned   = HM.size (umInstances um)
    if HM.null instances
        then return (scanned, V.empty)
        else do
            -- Read the game-clock (advances only when not paused) so
            -- the rendered frame index matches the uiAnimStart values
            -- published in game-time.
            now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
            texSizes ← readIORef (rvTextureSizeRef (toRenderViewCapability env))
            mBts ← readIORef (rvTextureSystemRef (toRenderViewCapability env))
            case mBts of
                Nothing → return (scanned, V.empty)
                Just _bts → do
                    -- Bake a STABLE texture-handle id; the bindless shader
                    -- resolves it to a live slot at draw time (#286).
                    -- Units have no directional face map of their own, so
                    -- the shader maps this to the default slot (#1696).
                    let lookupSlot h = fromIntegral (toInt h) ∷ Word32
                        defFmSlot = noFaceMapVertexId
                        quads = V.fromList
                            $ HM.foldlWithKey' (\acc uid inst →
                                let isSel = HS.member uid selected
                                    mDef  = HM.lookup (uiDefName inst) defs
                                in case unitToQuad lookupSlot defFmSlot facing
                                                zSlice effDepth tileAlpha isSel inst
                                                mDef now texSizes of
                                    Just sq →
                                        setQuadSolarPage
                                            (solarSlotOf (uiPage inst)) sq : acc
                                    Nothing → acc
                              ) [] instances
                    return (scanned, quads)

-- | The pure per-unit quad the renderer emits — exported so a test can
--   gate the ACTUAL consumer geometry (vertex positions and UVs)
--   rather than only the 'FrameSample' metadata that feeds it.
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
        let sample = case mDef of
                Just def → pickFrame now facing inst def
                Nothing  → uncurry wholeImageSample $
                    resolveTexture facing (uiFacing inst)
                                   (uiDirSprites inst) (uiTexture inst)
            texHandle = fsTexture sample
            flipX     = fsFlipX sample

            -- An atlas sample reports its CELL's dimensions; only a
            -- whole-image sample (the T-pose's direct sprite) falls
            -- through to the texture-size map. Measuring an atlas
            -- handle there would size every unit to the whole sheet.
            (texW, texH) = frameDimensions texSizes (baseTileW, baseTileH) sample

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
                       -- 2× the base nudge so the unit sorts just above
                       -- the terrain/fluid at its own tile (one nudge for
                       -- the surface, one to clear it).
                       + 2 * unitSortNudge
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
            wuv = uncurry packWorldUV baseTile

            -- The frame's own UV sub-rect. A T-pose sample is the
            -- whole image, so (su0,sv0,su1,sv1) = (0,0,1,1) and the
            -- vertices below are byte-for-byte the coordinates this
            -- path has always emitted; an animation frame narrows it to
            -- one atlas cell.
            (su0, sv0, su1, sv1) = fsUV sample

            -- Horizontal flip: swap U between left and right vertices.
            -- The geometry stays the same; only the texture sampling
            -- reads right-to-left. Lets western directions (SW/W/NW) be
            -- drawn from their eastern (SE/E/NE) sprites. Note the swap
            -- is WITHIN the frame's own sub-rect — mirroring across the
            -- whole image would, for an atlas, land in a different cell
            -- entirely (the #887 flip-the-clipped-slice ordering, which
            -- with atlases governs every sample rather than just the
            -- preview's).
            (uL, uR) = if flipX then (su1, su0) else (su0, su1)

            (v0, v1, v2, v3) =
                quadVertices (rectCorners (Vec2 drawX drawY) (Vec2 quadW quadH))
                             QuadUV { quLeftU   = uL
                                    , quTopV    = sv0
                                    , quRightU  = uR
                                    , quBottomV = sv1
                                    }
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
