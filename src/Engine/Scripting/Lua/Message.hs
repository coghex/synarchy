module Engine.Scripting.Lua.Message
  ( processLuaMessages
  ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef')
import Data.Time.Clock (getCurrentTime)
import System.FilePath (takeBaseName)
import Engine.Asset.Handle
import Engine.Asset.Manager
import Engine.Asset.Types
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logWarnM, logDebugSM, logInfoSM, logWarnSM)
import Engine.Core.Monad
import Engine.Core.State
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Font.Load (loadFont)
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import Engine.Scene.Base
import Engine.Scene.Graph (modifySceneNode, deleteSceneNode)
import Engine.Scene.Manager (addObjectToScene)
import Engine.Scene.Types
import Engine.Scripting.Lua.Types

processLuaMessages ∷ EngineM ε σ ()
processLuaMessages = do
    env ← ask
    messages ← liftIO $ Q.flushQueue (luaToEngineQueue env)
    
    when (not $ null messages) $
        logDebugSM CatLua "Processing Lua messages"
            [("count", T.pack $ show $ length messages)]
    
    forM_ messages handleLuaMessage

handleLuaMessage ∷ LuaToEngineMsg → EngineM ε σ ()
handleLuaMessage msg = do
    case msg of
        LuaLoadFontRequest handle path size → do
            logDebugSM CatLua "Loading font"
                [("path", T.pack path)
                ,("size", T.pack $ show size)
                ,("handle", T.pack (show handle))]
            handleLoadFont handle path size
        
        LuaLoadTextureRequest handle path → do
            logDebugSM CatLua "Loading texture"
                [("path", T.pack path)
                ,("handle", T.pack (show handle))]
            handleLoadTexture handle path
        
        LuaSpawnTextRequest objId x y font text color layer → do
            logDebugSM CatLua "Spawning text"
                [("objId", T.pack (show objId))
                ,("pos", T.pack (show x) <> "," <> T.pack (show y))
                ,("text", T.take 20 text)
                ,("layer", T.pack (show layer))]
            handleSpawnText objId x y font text color layer
        
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

-- | Handle texture load request
handleLoadTexture ∷ TextureHandle → FilePath → EngineM ε σ ()
handleLoadTexture handle path = do
    logInfoM CatLua $ "Loading texture from Lua: " <> T.pack path
    assetId ← loadTextureAtlasWithHandle handle (T.pack $ takeBaseName path) path "default"
    
    apRef ← asks assetPoolRef
    liftIO $ do
      pool ← readIORef apRef
      updateTextureState handle (AssetReady assetId []) pool
    
    pool ← liftIO $ readIORef apRef
    modify $ \s → s { assetPool = pool }
    logInfoM CatLua $ "Texture loaded successfully: " <> T.pack path

-- | Handle font load request
handleLoadFont ∷ FontHandle → FilePath → Int → EngineM ε σ ()
handleLoadFont handle path size = do
    logInfoM CatLua $ "Loading font from Lua: " <> T.pack path
    actualHandle ← loadFont handle path size
    env ← ask
    let etlq = luaQueue env
    liftIO $ Q.writeQueue etlq (LuaFontLoaded actualHandle)
    logInfoM CatLua $ "Font loaded successfully: " <> T.pack path

-- | Handle spawn text request
handleSpawnText ∷ ObjectId → Float → Float → FontHandle → Text
                → Vec4 → LayerId → EngineM ε σ ()
handleSpawnText oid x y fontHandle text color layer = do
    sceneMgr ← gets sceneManager
    case smActiveScene sceneMgr of
      Just sceneId → do
        let node = (createSceneNode TextObject)
              { nodeId = oid
              , nodeTransform = defaultTransform { position = (x, y) }
              , nodeFont = Just fontHandle
              , nodeText = Just text
              , nodeColor = color
              , nodeVisible = True
              , nodeLayer = layer
              }
        case addObjectToScene sceneId node sceneMgr of
          Just (addedObjId, newSceneMgr) → do
            modify $ \s → s { sceneManager = newSceneMgr }
            env ← ask
            liftIO $ atomicModifyIORef' (textBuffersRef env) $ \m →
              (Map.insert oid text m, ())
          Nothing → logDebugM CatLua $ "Failed to add text object " <> T.pack (show oid)
      Nothing → logDebugM CatLua "Cannot spawn text: no active scene"

-- | Handle set text request
handleSetText ∷ ObjectId → Text → EngineM ε σ ()
handleSetText objId text = do
    env ← ask
    liftIO $ atomicModifyIORef' (textBuffersRef env) $ \m →
      (Map.insert objId text m, ())
    modifySceneNode objId $ \node → node { nodeText = Just text }
    return ()

-- | Handle spawn sprite request
handleSpawnSprite ∷ ObjectId → Float → Float → Float → Float 
                  → TextureHandle → LayerId → EngineM ε σ ()
handleSpawnSprite objId x y width height texHandle layer = do
    sceneMgr ← gets sceneManager
    case smActiveScene sceneMgr of
      Just sceneId → do
        let node = (createSceneNode SpriteObject)
              { nodeId = objId
              , nodeTransform = defaultTransform { position = (x, y) }
              , nodeTexture = Just texHandle
              , nodeSize = (width, height)
              , nodeColor = Vec4 1 1 1 1
              , nodeVisible = True
              , nodeLayer = layer
              }
        case addObjectToScene sceneId node sceneMgr of
          Just (addedObjId, newSceneMgr) → do
            modify $ \s → s { sceneManager = newSceneMgr }
          Nothing → logDebugM CatLua $ "Failed to add sprite " <> T.pack (show objId)
      Nothing → logDebugM CatLua "Cannot spawn sprite: no active scene"

handleSetPos ∷ ObjectId → Float → Float → EngineM ε σ ()
handleSetPos objId x y =
    void $ modifySceneNode objId $ \node →
      node { nodeTransform = (nodeTransform node) { position = (x, y) } }

handleSetColor ∷ ObjectId → Vec4 → EngineM ε σ ()
handleSetColor objId color =
    void $ modifySceneNode objId $ \node → node { nodeColor = color }

handleSetSize ∷ ObjectId → Float → Float → EngineM ε σ ()
handleSetSize objId width height =
    void $ modifySceneNode objId $ \node → node { nodeSize = (width, height) }

handleSetVisible ∷ ObjectId → Bool → EngineM ε σ ()
handleSetVisible objId visible =
    void $ modifySceneNode objId $ \node → node { nodeVisible = visible }

handleDestroy ∷ ObjectId → EngineM ε σ ()
handleDestroy objId = void $ deleteSceneNode objId
