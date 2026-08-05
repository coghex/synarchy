module Engine.Graphics.Font.Load where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef', IORef)
import Engine.Asset.Handle (FontHandle)
import Engine.Graphics.Font.Atlas (generateFontAtlas)
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.Repertoire
  (bitmapFontKey, repertoireForFont, repertoireSize, sdfFontKey)
import Engine.Graphics.Font.SDF
  (generateSDFFontAtlas, sdfAtlasErrorMessage, sdfBaseSize)
import Engine.Graphics.Font.Upload (uploadFontAtlasToGPU)
import Vulkan.Core10
  ( PhysicalDeviceProperties(..), PhysicalDeviceLimits(..)
  , getPhysicalDeviceProperties )
import Engine.Core.Log.Monad (logDebugSM, logWarnM, logAndThrowM)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..), GraphicsState(..), loggerRef)
import Engine.Core.Capability.Render
  (RenderCapability(..), toRenderCapability)
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Core.Log (LogCategory(..))

-- | Load a TTF font at specified size
loadFont ∷ FontHandle → FilePath → Int → EngineM σ FontHandle
loadFont requestedHandle fontPath fontSize = do
    logDebugSM CatFont "Font atlas generation started"
        [("path", T.pack fontPath)
        ,("size", T.pack $ show fontSize)
        ,("char_range", "' ' to '~'")]

    cacheRef ← asks (rcFontCacheRef . toRenderCapability)
    cache ← liftIO $ readIORef cacheRef
    gs ← gets graphicsState
    case Map.lookup (bitmapFontKey fontPath fontSize) (fcPathCache cache) of
        Just handle → do
            logWarnM CatFont $ "Font already loaded: " <> T.pack (show fontPath)
            return handle
        Nothing → do
            fontDescLayout ← case fontDescriptorLayout gs of
                Nothing → logAndThrowM CatFont (ExGraphics DescriptorError)
                              "Font descriptor layout not initialized"
                Just layout → return layout
            -- Generate atlas
            loggerRef ← asks loggerRef
            logger ← liftIO $ readIORef loggerRef
            atlas ← liftIO $ generateFontAtlas logger fontPath fontSize

            logDebugSM CatFont "Atlas texture dimensions"
                [("width", T.pack $ show $ faAtlasWidth atlas)
                ,("height", T.pack $ show $ faAtlasHeight atlas)
                ,("glyph_count", T.pack $ show $ Map.size $ faGlyphData atlas)]

            (texHandle, descriptorSet, imgView, samp) ← uploadFontAtlasToGPU atlas fontDescLayout

            logDebugSM CatFont "Font GPU upload completion"
                [("atlas_size", T.pack (show (faAtlasWidth atlas)) <> "x" <> T.pack (show (faAtlasHeight atlas)))]

            let newAtlas = atlas { faTexture = texHandle
                                 , faDescriptorSet = Just descriptorSet
                                 , faImageView = Just imgView
                                 , faSampler = Just samp }
                handle = requestedHandle

            liftIO $ atomicModifyIORef' cacheRef $ \c → ((c
                { fcFonts = Map.insert handle newAtlas (fcFonts c)
                , fcPathCache = Map.insert (bitmapFontKey fontPath fontSize)
                                           handle (fcPathCache c) }
                ), ())

            return handle

-- | Load an SDF font (generates atlas once, scalable to any size)
loadSDFFont ∷ FontHandle → FilePath → EngineM σ FontHandle
loadSDFFont requestedHandle fontPath = do
    logDebugSM CatFont "SDF Font atlas generation started"
        [("path", T.pack fontPath)
        ,("base_size", T.pack $ show sdfBaseSize)
        ,("requested_chars", T.pack $ show $ repertoireSize
                                           $ repertoireForFont fontPath)]

    cacheRef ← asks (rcFontCacheRef . toRenderCapability)
    cache ← liftIO $ readIORef cacheRef
    gs ← gets graphicsState

    case Map.lookup (sdfFontKey fontPath) (fcPathCache cache) of
        Just existingHandle → do
            logDebugSM CatFont "SDF Font already loaded, reusing atlas"
                [("path", T.pack fontPath)
                ,("existing_handle", T.pack $ show existingHandle)
                ,("requested_handle", T.pack $ show requestedHandle)]

            case Map.lookup existingHandle (fcFonts cache) of
                Just existingAtlas → do
                    liftIO $ atomicModifyIORef' cacheRef $ \c → ((c
                        { fcFonts = Map.insert requestedHandle existingAtlas (fcFonts c)
                        }), ())
                    return requestedHandle
                Nothing → do
                    logWarnM CatFont "Cached font handle has no atlas, reloading"
                    loadNewSDFFont requestedHandle fontPath cacheRef gs
        Nothing → loadNewSDFFont requestedHandle fontPath cacheRef gs

-- | Load a new SDF font when not found in cache
loadNewSDFFont ∷ FontHandle → FilePath → IORef FontCache → GraphicsState → EngineM σ FontHandle
loadNewSDFFont requestedHandle fontPath cacheRef gs = do
    fontDescLayout ← case fontDescriptorLayout gs of
        Nothing → logAndThrowM CatFont (ExGraphics DescriptorError)
                      "Font descriptor layout not initialized"
        Just layout → return layout

    maxDimension ← maxAtlasDimension gs

    loggerRef' ← asks loggerRef
    logger ← liftIO $ readIORef loggerRef'
    result ← liftIO $ generateSDFFontAtlas logger fontPath
                                           (repertoireForFont fontPath)
                                           maxDimension
    atlas ← case result of
        Left err → logAndThrowM CatFont (ExGraphics FontError)
                       (sdfAtlasErrorMessage err)
        Right generated → return generated

    logDebugSM CatFont "SDF Atlas texture dimensions"
        [("width", T.pack $ show $ faAtlasWidth atlas)
        ,("height", T.pack $ show $ faAtlasHeight atlas)
        ,("glyph_count", T.pack $ show $ Map.size $ faGlyphData atlas)]

    (texHandle, descriptorSet, imgView, samp) ← uploadFontAtlasToGPU atlas fontDescLayout

    let newAtlas = atlas { faTexture = texHandle
                         , faDescriptorSet = Just descriptorSet
                         , faImageView = Just imgView
                         , faSampler = Just samp }

    liftIO $ atomicModifyIORef' cacheRef $ \c → ((c
        { fcFonts = Map.insert requestedHandle newAtlas (fcFonts c)
        , fcPathCache = Map.insert (sdfFontKey fontPath)
                                   requestedHandle (fcPathCache c) }
        ), ())

    return requestedHandle

-- | The device's @maxImageDimension2D@, which bounds what the packing
--   planner may choose. Queried rather than assumed: a plan that
--   overruns it would fail at image creation, after the CPU bitmap has
--   already been allocated.
maxAtlasDimension ∷ GraphicsState → EngineM σ Int
maxAtlasDimension gs = case vulkanPDevice gs of
    Nothing → logAndThrowM CatFont (ExGraphics VulkanDeviceLost)
                  "No physical device to query maxImageDimension2D"
    Just pDevice → do
        PhysicalDeviceProperties { limits = deviceLimits } ←
            liftIO $ getPhysicalDeviceProperties pDevice
        return $ fromIntegral $ maxImageDimension2D deviceLimits
