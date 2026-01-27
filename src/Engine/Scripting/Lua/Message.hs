module Engine.Scripting.Lua.Message
  ( processLuaMessages
  ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.IORef (readIORef)
import Data.Time.Clock (getCurrentTime)
import System.FilePath (takeBaseName)
import Engine.Asset.Handle
import Engine.Asset.Manager
import Engine.Asset.Types
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Font.Load (loadFont)
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import Engine.Scene.Base
import Engine.Scene.Graph (createSceneNode)
import Engine.Scene.Manager (addObjectToScene)
import Engine.Scene.Types
import Engine.Scripting.Lua.Types

-- | Process all pending messages from Lua thread
processLuaMessages ∷ EngineM ε σ ()
processLuaMessages = do
    env ← ask
    let lteq = luaToEngineQueue env
    
    maybeMsg ← liftIO $ Q.tryReadQueue lteq
    case maybeMsg of
      Nothing → return ()
      Just msg → do
        currentTime ← liftIO getCurrentTime
        logDebug $ "[MAIN RECV " <> show currentTime <> "] processing: " ⧺ show msg
        handleLuaMessage msg
        processLuaMessages

-- | Handle individual Lua messages
handleLuaMessage ∷ LuaToEngineMsg → EngineM ε σ ()
handleLuaMessage msg = case msg of
  LuaLoadTextureRequest handle path → handleLoadTexture handle path
  LuaLoadFontRequest handle path size → handleLoadFont handle path size
  LuaSpawnTextRequest oid x y fontHandle text layer → 
    handleSpawnText oid x y fontHandle text layer
  LuaSpawnSpriteRequest objId x y width height texHandle layer →
    handleSpawnSprite objId x y width height texHandle layer
  LuaMoveSpriteRequest objId x y → handleMoveSprite objId x y
  LuaSetSpriteColorRequest objId color → handleSetSpriteColor objId color
  LuaSetVisibleRequest objId visible → handleSetVisible objId visible
  LuaDestroySpriteRequest objId → handleDestroySprite objId
  _ → return ()

-- | Handle texture load request
handleLoadTexture ∷ TextureHandle → FilePath → EngineM ε σ ()
handleLoadTexture handle path = do
    logDebug $ "Loading texture: " ⧺ show path ⧺ " with handle " ⧺ show handle
    assetId ← loadTextureAtlasWithHandle handle (T.pack $ takeBaseName path) path "default"
    
    apRef ← asks assetPoolRef
    liftIO $ do
      pool ← readIORef apRef
      updateAssetState @TextureHandle handle (AssetReady assetId []) pool
    
    pool ← liftIO $ readIORef apRef
    modify $ \s → s { assetPool = pool }
    logDebug $ "Texture loaded: handle=" ⧺ show handle ⧺ ", assetId=" ⧺ show assetId

-- | Handle font load request
handleLoadFont ∷ FontHandle → FilePath → Int → EngineM ε σ ()
handleLoadFont handle path size = do
    logDebug $ "Loading font: " ⧺ show path ⧺ " size=" ⧺ show size
    actualHandle ← loadFont handle path size
    logDebug $ "Font loaded successfully: handle=" ⧺ show actualHandle
    
    env ← ask
    let etlq = luaQueue env
    liftIO $ Q.writeQueue etlq (LuaFontLoaded actualHandle)

-- | Handle spawn text request
handleSpawnText ∷ ObjectId → Float → Float → FontHandle → Text → LayerId 
                → EngineM ε σ ()
handleSpawnText oid x y fontHandle text layer = do
    sceneMgr ← gets sceneManager
    case smActiveScene sceneMgr of
      Just sceneId → do
        let node = (createSceneNode TextObject)
              { nodeId = oid
              , nodeTransform = defaultTransform { position = (x, y) }
              , nodeFont = Just fontHandle
              , nodeText = Just text
              , nodeColor = Vec4 1 1 1 1
              , nodeVisible = True
              , nodeLayer = layer
              }
        case addObjectToScene sceneId node sceneMgr of
          Just (addedObjId, newSceneMgr) → do
            modify $ \s → s { sceneManager = newSceneMgr }
            logDebug $ "Text object spawned with id: " ⧺ show addedObjId
          Nothing → logDebug $ "Failed to add text object " ⧺ show oid
      Nothing → logDebug "Cannot spawn text: no active scene"

-- | Handle spawn sprite request
handleSpawnSprite ∷ ObjectId → Float → Float → Float → Float 
                  → TextureHandle → LayerId → EngineM ε σ ()
handleSpawnSprite objId x y width height texHandle layer = do
    logDebug $ "Spawning sprite id=" ⧺ show objId ⧺
               " pos=(" ⧺ show x ⧺ ", " ⧺ show y ⧺ ")" ⧺
               " size=(" ⧺ show width ⧺ ", " ⧺ show height ⧺ ")" ⧺
               " tex=" ⧺ show texHandle
    
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
            logDebug $ "Sprite spawned with id: " ⧺ show addedObjId
          Nothing → logDebug $ "Failed to add sprite " ⧺ show objId
      Nothing → logDebug "Cannot spawn sprite: no active scene"

-- | Handle move sprite request
handleMoveSprite ∷ ObjectId → Float → Float → EngineM ε σ ()
handleMoveSprite objId x y = do
    modifySceneNode objId $ \node →
      node { nodeTransform = (nodeTransform node) { position = (x, y) } }

-- | Handle set sprite color request
handleSetSpriteColor ∷ ObjectId → Vec4 → EngineM ε σ ()
handleSetSpriteColor objId color = do
    modifySceneNode objId $ \node → node { nodeColor = color }

-- | Handle set sprite visible request
handleSetVisible ∷ ObjectId → Bool → EngineM ε σ ()
handleSetVisible objId visible = do
    modifySceneNode objId $ \node → node { nodeVisible = visible }

-- | Handle destroy sprite request
handleDestroySprite ∷ ObjectId → EngineM ε σ ()
handleDestroySprite objId = do
    sceneMgr ← gets sceneManager
    case smActiveScene sceneMgr of
      Just sceneId → case Map.lookup sceneId (smSceneGraphs sceneMgr) of
        Just graph → do
          let updatedGraph = graph 
                { sgNodes      = Map.delete objId (sgNodes graph)
                , sgWorldTrans = Map.delete objId (sgWorldTrans graph)
                , sgDirtyNodes = Set.delete objId (sgDirtyNodes graph) 
                }
              updatedGraphs = Map.insert sceneId updatedGraph (smSceneGraphs sceneMgr)
          modify $ \s → s { sceneManager = sceneMgr { smSceneGraphs = updatedGraphs } }
          logDebug $ "Destroyed sprite: " ⧺ show objId
        Nothing → logInfo "No scene graph"
      Nothing → logInfo "No active scene"

-- | Modify a scene node by ID
modifySceneNode ∷ ObjectId → (SceneNode → SceneNode) → EngineM ε σ ()
modifySceneNode objId f = do
  sceneMgr ← gets sceneManager
  case smActiveScene sceneMgr of
    Just sceneId → case Map.lookup sceneId (smSceneGraphs sceneMgr) of
      Just graph → case Map.lookup objId (sgNodes graph) of
        Just node → do
          let updatedNode = f node
              updatedGraph = graph 
                { sgNodes = Map.insert objId updatedNode (sgNodes graph)
                , sgDirtyNodes = Set.insert objId (sgDirtyNodes graph) 
                }
              updatedGraphs = Map.insert sceneId updatedGraph (smSceneGraphs sceneMgr)
          modify $ \s → s { sceneManager = sceneMgr { smSceneGraphs = updatedGraphs } }
        Nothing → logInfo $ "Sprite not found: " ⧺ show objId
      Nothing → logInfo "No scene graph"
    Nothing → logInfo "No active scene"
