module Engine.Loop.Headless
  ( headlessLoop
  , processLuaMessagesHeadless
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logWarnM)
import Engine.Scene.Base
import Engine.Scene.Graph (modifySceneNode, deleteSceneNode)
import Engine.Scene.Manager (addObjectToScene)
import Engine.Scene.Types
import Engine.Scripting.Lua.Types
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))

-- | Headless main loop: processes messages without rendering
headlessLoop ∷ EngineM ε σ ()
headlessLoop = do
    env ← ask
    lifecycle ← liftIO $ readIORef (lifecycleRef env)
    case lifecycle of
        EngineStarting → do
            logDebugM CatSystem "Headless engine starting..."
            liftIO $ threadDelay 100000
            _ ← liftIO $ Q.flushQueue (inputQueue env)
            liftIO $ writeIORef (lifecycleRef env) EngineRunning
            headlessLoop
        EngineRunning → do
            processLuaMessagesHeadless
            lifecycle' ← liftIO $ readIORef (lifecycleRef env)
            if lifecycle' ≡ EngineRunning
                then do
                    liftIO $ threadDelay 16666
                    headlessLoop
                else do
                    logInfoM CatSystem "Headless engine shutting down..."
                    liftIO $ writeIORef (lifecycleRef env) CleaningUp
        CleaningUp → logDebugM CatSystem "Headless engine cleaning up"
        EngineStopped → logDebugM CatSystem "Headless engine stopped"

-- | Process Lua messages in headless mode.
--   Handles scene graph operations (pure data), discards GPU operations.
processLuaMessagesHeadless ∷ EngineM ε σ ()
processLuaMessagesHeadless = do
    env ← ask
    messages ← liftIO $ Q.flushQueue (luaToEngineQueue env)
    forM_ messages handleLuaMessageHeadless

handleLuaMessageHeadless ∷ LuaToEngineMsg → EngineM ε σ ()
handleLuaMessageHeadless msg = case msg of
    -- GPU operations — discard silently
    LuaSetWindowMode _       → pure ()
    LuaSetVSync _            → pure ()
    LuaSetMSAA _             → pure ()
    LuaSetResolution _ _     → pure ()
    LuaSetTextureFilter _    → pure ()
    LuaLoadTextureRequest _ _ → pure ()
    LuaLoadFontRequest _ _ _ → pure ()

    -- Pure IORef writes — still process these
    LuaSetBrightness pct → do
        env ← ask
        liftIO $ writeIORef (brightnessRef env) (max 50 (min 300 pct))
    LuaSetPixelSnap enabled → do
        env ← ask
        liftIO $ writeIORef (pixelSnapRef env) enabled

    -- Scene graph operations — pure data, no GPU needed
    LuaSpawnTextRequest oid x y fontHandle text color layer size → do
        sceneMgr ← gets sceneManager
        case smActiveScene sceneMgr of
            Just sceneId → do
                let node = (createSceneNode TextObject)
                      { nodeId = oid
                      , nodeTransform = defaultTransform { position = (x, y) }
                      , nodeFont = Just fontHandle
                      , nodeFontSize = Just size
                      , nodeText = Just text
                      , nodeColor = color
                      , nodeVisible = True
                      , nodeLayer = layer
                      }
                case addObjectToScene sceneId node sceneMgr of
                    Just (_, newMgr) → modify $ \s → s { sceneManager = newMgr }
                    Nothing → pure ()
            Nothing → pure ()

    LuaSpawnSpriteRequest{..} → do
        sceneMgr ← gets sceneManager
        case smActiveScene sceneMgr of
            Just sceneId → do
                let node = (createSceneNode SpriteObject)
                      { nodeId = lssObjectId
                      , nodeTransform = defaultTransform { position = (lssX, lssY) }
                      , nodeTexture = Just lssTextureHandle
                      , nodeSize = (lssWidth, lssHeight)
                      , nodeColor = Vec4 1 1 1 1
                      , nodeVisible = True
                      , nodeLayer = lssLayer
                      }
                case addObjectToScene sceneId node sceneMgr of
                    Just (_, newMgr) → modify $ \s → s { sceneManager = newMgr }
                    Nothing → pure ()
            Nothing → pure ()

    LuaSetTextRequest objId text →
        void $ modifySceneNode objId $ \node → node { nodeText = Just text }
    LuaSetPosRequest objId x y →
        void $ modifySceneNode objId $ \node →
            node { nodeTransform = (nodeTransform node) { position = (x, y) } }
    LuaSetColorRequest objId color →
        void $ modifySceneNode objId $ \node → node { nodeColor = color }
    LuaSetSizeRequest objId w h →
        void $ modifySceneNode objId $ \node → node { nodeSize = (w, h) }
    LuaSetVisibleRequest objId visible →
        void $ modifySceneNode objId $ \node → node { nodeVisible = visible }
    LuaDestroyRequest objId →
        void $ deleteSceneNode objId
    LuaSetSpriteScaleRequest objId w h →
        void $ modifySceneNode objId $ \node → node { nodeSize = (w, h) }

    -- Focus operations — just IORef writes, no GPU
    LuaRequestFocus _       → pure ()
    LuaReleaseFocus         → pure ()
    LuaRegisterFocusable _ _ → pure ()
    LuaUnregisterFocusable _ → pure ()

    -- Log messages
    LuaLog _ logMsg → logDebugM CatLua (T.pack logMsg)
