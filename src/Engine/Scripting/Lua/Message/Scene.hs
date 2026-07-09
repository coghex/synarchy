-- | Scene-graph Lua message handlers (split out of
--   'Engine.Scripting.Lua.Message', #558): spawn/update/destroy for
--   text and sprite scene objects. Pure scene-graph + IORef bookkeeping
--   only — no GPU calls, so these run in both graphical and headless
--   mode (unlike 'Engine.Scripting.Lua.Message.Texture' /
--   '.WorldTexture').
module Engine.Scripting.Lua.Message.Scene
    ( handleSpawnText
    , handleSetText
    , handleSpawnSprite
    , handleSetPos
    , handleSetColor
    , handleSetSize
    , handleSetVisible
    , handleDestroy
    ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle, FontHandle)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import Engine.Scene.Base
import Engine.Scene.Graph (modifySceneNode, deleteSceneNode)
import Engine.Scene.Manager (addObjectToScene)
import Engine.Scene.Types

handleSpawnText ∷ ObjectId → Float → Float → FontHandle → Text
                → Vec4 → LayerId → Float → EngineM ε σ ()
handleSpawnText oid x y fontHandle text color layer size = do
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
          Just (_addedObjId, newSceneMgr) → do
            modify $ \s → s { sceneManager = newSceneMgr }
            env ← ask
            liftIO $ atomicModifyIORef' (textBuffersRef env) $ \m →
              (Map.insert oid text m, ())
          Nothing → logDebugM CatLua $ "Failed to add text object " <> T.pack (show oid)
      Nothing → logDebugM CatLua "Cannot spawn text: no active scene"

handleSetText ∷ ObjectId → Text → EngineM ε σ ()
handleSetText objId text = do
    env ← ask
    liftIO $ atomicModifyIORef' (textBuffersRef env) $ \m →
      (Map.insert objId text m, ())
    -- Bool result ignored: setText on a missing node is a no-op by design.
    _ ← modifySceneNode objId $ \node → node { nodeText = Just text }
    return ()

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
          Just (_addedObjId, newSceneMgr) → do
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
