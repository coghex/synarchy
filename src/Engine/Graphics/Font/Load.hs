module Engine.Graphics.Font.Load where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef', IORef)
import Engine.Asset.Handle (FontHandle)
import Engine.Graphics.Font.Atlas (generateFontAtlas)
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.SDF (generateSDFFontAtlas, sdfBaseSize)
import Engine.Graphics.Font.Upload (uploadFontAtlasToGPU)
import Engine.Core.Log.Monad (logDebugSM, logWarnM, logAndThrowM)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..), GraphicsState(..), loggerRef)
import Engine.Core.Capability.Render
  (RenderCapability(..), toRenderCapability)
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Core.Log (LogCategory(..))

-- | Load a TTF font at specified size
loadFont ∷ FontHandle → FilePath → Int → EngineM ε σ FontHandle
loadFont requestedHandle fontPath fontSize = do
    logDebugSM CatFont "Font atlas generation started"
        [("path", T.pack fontPath)
        ,("size", T.pack $ show fontSize)
        ,("char_range", "' ' to '~'")]

    cacheRef ← asks (rcFontCacheRef . toRenderCapability)
    cache ← liftIO $ readIORef cacheRef
    gs ← gets graphicsState
    case Map.lookup (fontPath, fontSize) (fcPathCache cache) of
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
                , fcPathCache = Map.insert (fontPath, fontSize) handle (fcPathCache c) }
                ), ())

            return handle

-- | Load an SDF font (generates atlas once, scalable to any size)
loadSDFFont ∷ FontHandle → FilePath → EngineM ε σ FontHandle
loadSDFFont requestedHandle fontPath = do
    logDebugSM CatFont "SDF Font atlas generation started"
        [("path", T.pack fontPath)
        ,("base_size", T.pack $ show sdfBaseSize)
        ,("char_range", "' ' to '~'")]

    cacheRef ← asks (rcFontCacheRef . toRenderCapability)
    cache ← liftIO $ readIORef cacheRef
    gs ← gets graphicsState

    -- SDF fonts use size = -1 as cache key sentinel
    case Map.lookup (fontPath, -1) (fcPathCache cache) of
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
loadNewSDFFont ∷ FontHandle → FilePath → IORef FontCache → GraphicsState → EngineM ε σ FontHandle
loadNewSDFFont requestedHandle fontPath cacheRef gs = do
    fontDescLayout ← case fontDescriptorLayout gs of
        Nothing → logAndThrowM CatFont (ExGraphics DescriptorError)
                      "Font descriptor layout not initialized"
        Just layout → return layout

    loggerRef' ← asks loggerRef
    logger ← liftIO $ readIORef loggerRef'
    atlas ← liftIO $ generateSDFFontAtlas logger fontPath

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
        , fcPathCache = Map.insert (fontPath, -1) requestedHandle (fcPathCache c) }
        ), ())

    return requestedHandle
