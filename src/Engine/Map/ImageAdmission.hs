{-# LANGUAGE Strict #-}
-- | The engine-side half of #2020's map-image admission: it decides
--   whether a device ceiling applies to THIS boot mode, reads the
--   published limit, and hands both to the pure planner in
--   "World.Map.ImagePlan".
--
--   The split is deliberate. The planner is pure and knows nothing about
--   boot modes or Vulkan; this module knows nothing about geometry or
--   arithmetic. What lives here is exactly the mode question — which is
--   a question, because three of the five boot modes have no GPU at all
--   and refusing a world there because no device answered would be
--   wrong, while accepting an unbounded image in a mode that DOES have
--   one would be the bug this whole slice exists to close.
module Engine.Map.ImageAdmission
    ( bootModeNeedsDeviceCeiling
    , resolveMapImageCeiling
    , readMapImageCeiling
    , admitWorldZoomAtlas
    , validateZoomAtlasUpload
    , withValidatedZoomAtlasUpload
    ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv)
import Engine.Core.Types (BootMode(..), EngineConfig(..), bootModeName)
import World.Map.ImagePlan
    ( MapImageCeiling(..), MapImageFormat(..), MapImagePlan
    , MapImageRefusal, MapImageSource(..), admitMapImage
    , checkUploadPayload )

-- | Does a map image built in this boot mode have to fit the physical
--   device's @maxImageDimension2D@?
--
--   Total by construction — every one of 'BootMode''s five constructors
--   is answered here, with no catch-all, so adding a sixth is a compile
--   error rather than an unexamined default.
--
--   * @--dump@ and @--headless@ have no GPU by design. They still get
--     every geometry, representability and byte-count check; they get no
--     ceiling, and the absence of a device is not an error there.
--   * @--offscreen@ and the graphical mode both create real Vulkan
--     images, so both need the real limit.
--   * @--preview@ is GPU-capable and is classified as such. In practice
--     it never reaches map-image admission at all — @app\/App\/Preview.hs@
--     starts no world, unit, sim or combat thread — but classifying it
--     by what it CAN do keeps the answer honest if that ever changes.
bootModeNeedsDeviceCeiling ∷ BootMode → Bool
bootModeNeedsDeviceCeiling ModeDump      = False
bootModeNeedsDeviceCeiling ModeHeadless  = False
bootModeNeedsDeviceCeiling ModeOffscreen = True
bootModeNeedsDeviceCeiling ModeGraphical = True
bootModeNeedsDeviceCeiling ModePreview   = True

-- | Turn a boot mode and whatever limit has been published into the
--   ceiling the planner should apply.
--
--   A GPU-capable mode with no published limit yields
--   'CeilingUnavailable', never 'CeilingNotApplicable': continuing there
--   would be exactly the silent unchecked allocation this slice removes.
resolveMapImageCeiling ∷ BootMode → Maybe Int → MapImageCeiling
resolveMapImageCeiling mode mLimit
    | not (bootModeNeedsDeviceCeiling mode) = CeilingNotApplicable
    | otherwise = case mLimit of
        Just limit → CeilingKnown limit
        Nothing    → CeilingUnavailable $
            "the physical device's maxImageDimension2D has not been "
            <> "published for this " <> bootModeName mode
            <> " session (Engine.Graphics.Vulkan.Init publishes it as "
            <> "soon as a device exists)"

-- | The live ceiling for this session: the boot mode from
--   'EngineConfig' (reachable from any thread) and the limit from the
--   worker-safe render view. Neither reaches @GraphicsState@.
readMapImageCeiling ∷ EngineEnv → IO MapImageCeiling
readMapImageCeiling env = do
    mLimit ← readIORef (rvMaxImageDimensionRef (toRenderViewCapability env))
    let mode = ecBootMode (ccEngineConfig (toCoreCapability env))
    pure $ resolveMapImageCeiling mode mLimit

-- | Admit the whole-world zoom atlas for a world of @worldSize@ chunks
--   per side. The one entry point every producer of that atlas — fresh
--   init, load staging, and @world.init@'s synchronous pre-enqueue
--   check — goes through, so they cannot disagree about what is
--   admissible or about how a refusal reads.
admitWorldZoomAtlas ∷ EngineEnv → Int
                    → IO (Either MapImageRefusal MapImagePlan)
admitWorldZoomAtlas env worldSize = do
    mapCeiling ← readMapImageCeiling env
    pure $ admitMapImage mapCeiling MapImageRGBA8 (ZoomAtlasSource worldSize)

-- | The zoom-atlas UPLOAD boundary's own validation (#2020,
--   requirement 5), against the device limit queried at upload time.
--
--   Deliberately independent of construction: construction proved the
--   image could be planned, upload proves that THESE dimensions and
--   THIS payload can be handed to THIS device. The expected byte count
--   is re-derived from the dimensions about to reach Vulkan, through the
--   same checked planner, rather than recomputed by hand at the call
--   site.
validateZoomAtlasUpload ∷ Int → Int → Int → Int
                        → Either MapImageRefusal MapImagePlan
validateZoomAtlasUpload deviceLimit w h payloadLength = do
    plan ← admitMapImage (CeilingKnown deviceLimit) MapImageRGBA8
               (WholeImageSource w h)
    checkUploadPayload plan payloadLength
    pure plan

-- | Run the uploading continuation ONLY on an accepted upload.
--
--   The seam exists so the ordering requirement — nothing is allocated
--   before the checks pass — is a property of the code the engine runs
--   rather than of a comment: a refusal never enters @onAccepted@, and a
--   headless test can pass allocation-recording continuations to the
--   very same function 'Engine.Scripting.Lua.Message.WorldTexture' calls.
withValidatedZoomAtlasUpload
    ∷ Monad m ⇒ Int → Int → Int → Int
    → (MapImageRefusal → m α) → (MapImagePlan → m α) → m α
withValidatedZoomAtlasUpload deviceLimit w h payloadLength
                             onRefused onAccepted =
    either onRefused onAccepted
        (validateZoomAtlasUpload deviceLimit w h payloadLength)
