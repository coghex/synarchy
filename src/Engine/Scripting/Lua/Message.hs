-- | Lua → engine message dispatch (#558). This module is the per-frame
--   entry point ('processLuaMessages') and the dispatch table
--   ('handleLuaMessage'); the handler bodies themselves live in
--   focused submodules split by concern:
--
--   * "Engine.Scripting.Lua.Message.Video" — window/VSync/MSAA/
--     brightness/pixel-snap/texture-filter settings.
--   * "Engine.Scripting.Lua.Message.Texture" — batched GPU texture
--     file loads and SDF font loads.
--   * "Engine.Scripting.Lua.Message.Scene" — scene-graph object
--     spawn/update/destroy (text, sprites).
--   * "Engine.Scripting.Lua.Message.WorldTexture" — world-preview and
--     zoom-atlas GPU uploads from raw pixel bytes.
module Engine.Scripting.Lua.Message
  ( processLuaMessages
  ) where

import UPrelude
import qualified Data.Text as T
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logDebugSM, logWarnM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Types (ecHeadless)
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Message.Scene ( handleSpawnText, handleSetText
                                           , handleSpawnSprite, handleSetPos
                                           , handleSetColor, handleSetSize
                                           , handleSetVisible, handleDestroy)
import Engine.Scripting.Lua.Message.Texture ( handleLoadTextureBatch
                                             , handleLoadTexture
                                             , handleLoadFont)
import Engine.Scripting.Lua.Message.Video ( handleSetResolution
                                           , handleSetWindowMode
                                           , handleSetVSync, handleSetMSAA
                                           , handleSetBrightness
                                           , handleSetPixelSnap
                                           , handleSetTextureFilter)
import Engine.Scripting.Lua.Message.WorldTexture ( handleWorldPreview
                                                   , handleZoomAtlasUpload)
import Engine.Scripting.Lua.Types
import World.Render.BloodQuads (uploadBloodTextures)

processLuaMessages ∷ EngineM ε σ ()
processLuaMessages = do
    env ← ask
    messages ← liftIO $ Q.flushQueue (luaToEngineQueue env)

    when (not $ null messages) $
        logDebugSM CatLua "Processing Lua messages"
            [("count", T.pack $ show $ length messages)]

    process messages
    whenGraphical handleWorldPreview
    whenGraphical handleZoomAtlasUpload
    whenGraphical uploadBloodTextures
  where
    process [] = pure ()
    process (LuaLoadTextureRequest handle path : rest) = do
        let (burst, rest') = span isTextureLoad rest
            requests = (handle, path) : unwrapTextureLoads burst
        whenGraphical $ handleLoadTextureBatch requests
        process rest'
    process (msg : rest) = do
        handleLuaMessage msg
        process rest

    isTextureLoad (LuaLoadTextureRequest _ _) = True
    isTextureLoad _                           = False

    unwrapTextureLoads msgs =
        [ (handle, path) | LuaLoadTextureRequest handle path ← msgs ]

-- | Run a GPU-touching action only in graphical mode; skip it when
--   headless (no device). Lets the single 'handleLuaMessage' serve both
--   the graphical loop and the headless/dump loop — the scene-graph and
--   pure-IORef cases run in both; only GPU operations are gated. (Before
--   this, a separate 'handleLuaMessageHeadless' duplicated every
--   scene-graph case and had already drifted from this one.)
whenGraphical ∷ EngineM ε σ () → EngineM ε σ ()
whenGraphical act = do
    env ← ask
    if ecHeadless (engineConfig env) then pure () else act

handleLuaMessage ∷ LuaToEngineMsg → EngineM ε σ ()
handleLuaMessage msg = do
    case msg of
        LuaSetWindowMode mode → whenGraphical $ do
            logDebugM CatLua $ "Setting window mode: " <> T.pack (show mode)
            handleSetWindowMode mode

        LuaSetResolution w h → whenGraphical $ do
            logDebugSM CatLua "Setting resolution"
                [("width", T.pack $ show w)
                ,("height", T.pack $ show h)]
            handleSetResolution w h

        LuaSetVSync enabled → whenGraphical $ do
            logDebugSM CatLua "Setting VSync"
                [("enabled", if enabled then "true" else "false")]
            handleSetVSync enabled

        LuaSetMSAA msaa → whenGraphical $ do
            logDebugSM CatLua "Setting MSAA"
                [("samples", T.pack $ show msaa)]
            handleSetMSAA msaa

        LuaSetBrightness brightness → do
            logDebugSM CatLua "Setting brightness"
                [("brightness", T.pack $ show brightness)]
            handleSetBrightness brightness

        LuaSetPixelSnap enabled → do
            logDebugSM CatLua "Setting pixel snap"
                [("enabled", if enabled then "true" else "false")]
            handleSetPixelSnap enabled

        LuaSetTextureFilter tf → whenGraphical $ handleSetTextureFilter tf

        LuaLoadFontRequest handle path size → whenGraphical $ do
            logDebugSM CatLua "Loading font"
                [("path", T.pack path)
                ,("size", T.pack $ show size)
                ,("handle", T.pack (show handle))]
            handleLoadFont handle path size

        LuaLoadTextureRequest handle path → whenGraphical $ do
            logDebugSM CatLua "Loading texture"
                [("path", T.pack path)
                ,("handle", T.pack (show handle))]
            handleLoadTexture handle path

        LuaSpawnTextRequest objId x y font text color layer size → do
            logDebugSM CatLua "Spawning text"
                [("objId", T.pack (show objId))
                ,("pos", T.pack (show x) <> "," <> T.pack (show y))
                ,("text", T.take 20 text)
                ,("layer", T.pack (show layer))
                ,("size", T.pack (show size))]
            handleSpawnText objId x y font text color layer size

        LuaSetTextRequest objId text → do
            logDebugSM CatLua "Setting text"
                [("objId", T.pack (show objId))
                ,("text", T.take 20 text)]
            handleSetText objId text

        LuaSpawnSpriteRequest objId x y w h tex layer → do
            logDebugSM CatLua "Spawning sprite"
                [("objId", T.pack (show objId))
                ,("pos", T.pack (show x) <> "," <> T.pack (show y))
                ,("size", T.pack (show w) <> "x" <> T.pack (show h))
                ,("layer", T.pack (show layer))]
            handleSpawnSprite objId x y w h tex layer

        LuaSetPosRequest objId x y → do
            logDebugSM CatLua "Moving object"
                [("objId", T.pack $ show objId)
                ,("pos", T.pack (show x) <> "," <> T.pack (show y))]
            handleSetPos objId x y

        LuaSetColorRequest objId color → do
            logDebugM CatLua $ "Setting color for object " <> T.pack (show objId)
            handleSetColor objId color

        LuaSetSizeRequest objId w h → do
            logDebugSM CatLua "Setting size"
                [("objId", T.pack $ show objId)
                ,("size", T.pack (show w) <> "x" <> T.pack (show h))]
            handleSetSize objId w h

        LuaSetVisibleRequest objId visible → do
            logDebugSM CatLua "Setting visibility"
                [("objId", T.pack $ show objId)
                ,("visible", if visible then "true" else "false")]
            handleSetVisible objId visible

        LuaDestroyRequest objId → do
            logDebugM CatLua $ "Destroying object " <> T.pack (show objId)
            handleDestroy objId

        -- The remaining 'LuaToEngineMsg' constructors (logging, focus,
        -- sprite-scale, etc.) are consumed by the Lua thread's own
        -- 'processLuaMsg' on a different queue, never this engine queue.
        -- If one ever arrives here it's a routing bug — log it rather
        -- than crash (this case used to be a partial match).
        other →
            logWarnM CatLua $
                "handleLuaMessage: unexpected message on engine queue: "
                <> T.pack (show other)
