{-# LANGUAGE Strict #-}
-- | The structure TEARDOWN pass (#2491): draw the transient
--   presentations 'Structure.Destruction' captured, through the same
--   geometry a placed piece and a construction ghost use.
--
--   It is a separate pass from 'Structure.Render.renderStructureQuads'
--   and contributes to the SAME 'Engine.Scene.Stats.ScStructures'
--   category, which is the shape requirement 10 asks for: no new scene
--   category, and a frame holding only effects measured rather than
--   reported as an empty pass. The piece pass short-circuits on "no
--   chunk holds a piece" and again on "there is no texture system", and
--   both of those are states an effect can legitimately outlive — a
--   chunk evicted after the capture, a headless session — so the
--   effects cannot be counted from inside it.
--
--   Two rules the geometry is not allowed to bend:
--
--     * __the identity is the STATIC appearance's.__ A wall's camera
--       rotation is resolved from the captured static sprite and cap
--       facemap through 'World.Render.StructureGhost.drawnWallEdge' —
--       the same question 'Structure.Render' asks for a placed piece —
--       and only THEN is the rotated appearance's own clip consulted.
--       Feeding an animation frame path into the wall catalogue could
--       not identify a family at all: a frame is never registered art.
--     * __no substitution, ever.__ An effect whose drawn appearance
--       resolves no clip emits nothing. It does not fall back to the
--       static sprite, the other facings' frames, the construction
--       sequence reversed, or another appearance's art.
module World.Render.StructureDestruction
    ( renderStructureDestructionQuadsScanned
    , countStructureDestructions
    , structureDestructionQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.IORef (readIORef)
import Engine.Asset.Handle (TextureHandle, toInt)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.RenderHandoff
  (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.WorldSim
  (WorldSimCapability(..), toWorldSimCapability)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing)
import Engine.Scene.Types (SortableQuad(..))
import Structure.ArtCatalog
    ( AppearanceKey(..), AppearanceSlot(..), ArtAsset(..)
    , DestructionSequence(..), PieceArt(..), StructureArtCatalog
    , resolveDestructionSequence )
import Structure.Destruction
import Structure.Facing (wallEdgeOfSlot)
import Structure.Render
    ( ResolvedPieceArt(..), opaqueTint, structurePieceQuadsResolved
    , translateQuad )
import Structure.Types (StructureSlot)
import Structure.WallCatalog (StructureWallCatalog)
import World.Generate.Coordinates (globalToChunk)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.StructureGhost (drawnWallEdge)
import World.Render.ViewBounds (ViewBounds, computeViewBounds)
import World.Types
    (WorldState, wsGenParamsRef, wsStructureDestructionsRef
    , WorldGenParams(..))

-- | How many teardown effects this page is RETAINING, without drawing
--   any of them.
--
--   Requirement 7's "an effect on a hidden page is counted but not
--   drawn", and the reason 'World.Render' walks every page rather than
--   only the visible ones for this category. Deliberately not filtered
--   by visibility, residency or the z band: the count answers "what is
--   this page still holding", which is exactly the number the world
--   tick is responsible for retiring.
countStructureDestructions ∷ WorldState → IO Int
countStructureDestructions ws =
    HM.size <$> readIORef (wsStructureDestructionsRef ws)

-- | Draw one VISIBLE page's teardown effects, paired with the same
--   retained count 'countStructureDestructions' reports.
--
--   The count is the page's whole collection, not the subset that
--   survived culling — an effect the camera is not looking at is still
--   retained, still expiring, and still this pass's responsibility. The
--   quads are of course only the ones that resolved a frame, a pair of
--   handles and a visible chunk alias.
--
--   Emits nothing at all without a texture system (the headless state),
--   exactly as the piece pass does, while still reporting the count.
renderStructureDestructionQuadsScanned
    ∷ EngineEnv → WorldState → CameraFacing → Int → Int → Float
    → IO (Int, V.Vector SortableQuad)
renderStructureDestructionQuadsScanned env ws facing zSlice effDepth tileAlpha = do
    effects ← readIORef (wsStructureDestructionsRef ws)
    let scanned = HM.size effects
    if HM.null effects then return (0, V.empty) else do
        let handoff = toRenderHandoffCapability env
            rv      = toRenderViewCapability env
        mBts ← readIORef (rvTextureSystemRef rv)
        case mBts of
            Nothing → return (scanned, V.empty)
            Just _bts → do
                art     ← readIORef (rhStructureArtCatalogRef handoff)
                walls   ← readIORef (rhStructureWallCatalogRef handoff)
                handles ← readIORef (rhTexPaletteHandlesRef handoff)
                texSizes ← readIORef (rvTextureSizeRef rv)
                camera   ← readIORef (rvCameraRef rv)
                (fbW, fbH) ← readIORef (rvFramebufferSizeRef rv)
                paramsM  ← readIORef (wsGenParamsRef ws)
                now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
                let lookupSlot h = fromIntegral (toInt h) ∷ Word32
                    worldSize = maybe 128 wgpWorldSize paramsM
                    vb = computeViewBounds camera fbW fbH effDepth
                    (camX, camY) = camPosition camera
                    quads = structureDestructionQuads art walls handles
                                lookupSlot texSizes facing zSlice effDepth
                                tileAlpha worldSize vb camX camY now
                                (HM.elems effects)
                return (scanned, V.fromList quads)

-- | The pure per-effect pipeline: resolve the drawn appearance, its
--   frame, and the piece geometry that frame is drawn through.
--
--   Pure so a headless spec can drive the real geometry — the IO entry
--   above deliberately emits nothing until a texture system exists, and
--   headless never has one.
structureDestructionQuads
    ∷ StructureArtCatalog
    → StructureWallCatalog
    → HM.HashMap Int TextureHandle             -- ^ palette id → runtime handle
    → (TextureHandle → Word32)                 -- ^ handle → bindless slot id
    → HM.HashMap TextureHandle (Int, Int)      -- ^ texture pixel sizes
    → CameraFacing → Int → Int → Float
    → Int                                      -- ^ world size in chunks
    → ViewBounds
    → Float → Float                            -- ^ camera screen position
    → Double                                   -- ^ game-clock seconds
    → [StructureDestructionEffect]
    → [SortableQuad]
structureDestructionQuads art walls handles lookupSlot texSizes facing
                          zSlice effDepth tileAlpha worldSize vb camX camY
                          now effects =
    concatMap effectQuads effects
  where
    effectQuads eff = fromMaybe [] $ do
        idx   ← destructionEffectFrameIndex now eff
        texH  ← resolveHandle (sdeTexId eff) (sdeTexHandle eff)
        faceH ← resolveHandle (sdeFaceId eff) (sdeFaceHandle eff)
        let slot = toEnum (fromIntegral (sdeSlotTag eff)) ∷ StructureSlot
            drawnAk = drawnAppearance eff slot texH faceH
        clip  ← resolveDestructionSequence art (sdePack eff) drawnAk
        let frames = dsFrames clip
        guard (not (V.null frames))
        let frame = frames V.! min idx (V.length frames - 1)
        off ← isChunkVisibleWrapped facing worldSize vb camX camY
                  (fst (globalToChunk (sdeGX eff) (sdeGY eff)))
        pure $ map (translateQuad off) $
            structurePieceQuadsResolved walls lookupSlot texSizes facing
                zSlice effDepth (opaqueTint tileAlpha)
                (sdeGX eff) (sdeGY eff) slot
                ResolvedPieceArt
                    { rpaTexture     = texH
                    , rpaFacemap     = faceH
                    , rpaTexturePath = Just (sdeTexPath eff)
                    , rpaFacemapPath = sdeFacePath eff
                    , rpaLifecycle   = Just (aaHandle frame)
                    }
                (sdeGridZ eff)

    -- The LIVE handle map first, the capture's own reading second. The
    -- live map is the authority (a load re-resolves every palette id
    -- into fresh handles), and the captured pair is what keeps an
    -- effect drawable when that map has not caught up — neither of
    -- which is allowed to decide whether the effect EXISTS.
    resolveHandle pid captured = case HM.lookup pid handles of
        Just h  → Just h
        Nothing → captured

    -- Which authored appearance this effect is DRAWN as at @facing@.
    --
    -- For every kind but a wall it is the captured one, and the camera
    -- cannot change it. A WALL's authored edge does not move but the
    -- screen edge it occupies does, and the sprite drawn is the
    -- family's art for THAT edge — so the clip has to be that edge's
    -- too, or a turning camera would show one direction's collapse on
    -- another direction's wall. The frame INDEX is unaffected, which is
    -- what registration's equal-length, equal-fps rule guarantees.
    --
    -- The question is asked of 'drawnWallEdge', the same helper the
    -- construction pass asks, so the two lifecycles can never disagree
    -- about which edge is on screen. Where it declines to rotate — art
    -- no registered family carries, or a path two families contest —
    -- the piece draws exactly as authored and so does its teardown.
    drawnAppearance eff slot texH faceH = case
            (apSlot (sdeAppearance eff), wallEdgeOfSlot slot, sdeFacePath eff) of
        (ApWall _, Just edge, Just facePath) →
            (sdeAppearance eff)
                { apSlot = ApWall (drawnWallEdge walls facing edge
                                      (pieceArtOf (sdeTexPath eff) texH
                                                  facePath faceH)) }
        _ → sdeAppearance eff

    pieceArtOf texPath texH facePath faceH =
        PieceArt (ArtAsset texPath texH) (ArtAsset facePath faceH)
