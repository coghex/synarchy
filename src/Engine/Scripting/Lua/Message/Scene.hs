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
import Data.IORef (atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle, FontHandle)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..))
import Engine.Core.Capability.Ui (UiCapability(..), toUiCapability)
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import Engine.Scene.Base
import Engine.Scene.Graph (modifySceneNode, deleteSceneNode)
import Engine.Scene.Manager (addObjectToScene)
import Engine.Scene.Types

handleSpawnText ∷ ObjectId → Float → Float → FontHandle → Text
                → Vec4 → LayerId → Float → EngineM σ ()
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
            liftIO $ atomicModifyIORef' (uicTextBuffersRef (toUiCapability env)) $ \m →
              (Map.insert oid text m, ())
          Nothing → logDebugM CatLua $ "Failed to add text object " <> tshow oid
      Nothing → logDebugM CatLua "Cannot spawn text: no active scene"

-- | Update a live text node's string, and the scene-text cache with it.
--
--   The node write comes FIRST and its 'Bool' decides the cache write
--   (#1961): 'uicTextBuffersRef' entries follow their scene nodes' own
--   lifetimes, so a @setText@ naming an id with no node must change
--   nothing at all — not the scene graph, and not the cache that
--   @engine.getText@ answers from. Writing the cache unconditionally
--   (as this did) left @engine.getText@ reporting text for objects that
--   never existed, in a map with no other way to shrink.
handleSetText ∷ ObjectId → Text → EngineM σ ()
handleSetText objId text = do
    nodeUpdated ← modifySceneNode objId $ \node → node { nodeText = Just text }
    when nodeUpdated $ do
      env ← ask
      liftIO $ atomicModifyIORef' (uicTextBuffersRef (toUiCapability env)) $ \m →
        (Map.insert objId text m, ())

handleSpawnSprite ∷ ObjectId → Float → Float → Float → Float
                  → TextureHandle → LayerId → EngineM σ ()
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
          Nothing → logDebugM CatLua $ "Failed to add sprite " <> tshow objId
      Nothing → logDebugM CatLua "Cannot spawn sprite: no active scene"

handleSetPos ∷ ObjectId → Float → Float → EngineM σ ()
handleSetPos objId x y =
    void $ modifySceneNode objId $ \node →
      node { nodeTransform = (nodeTransform node) { position = (x, y) } }

handleSetColor ∷ ObjectId → Vec4 → EngineM σ ()
handleSetColor objId color =
    void $ modifySceneNode objId $ \node → node { nodeColor = color }

handleSetSize ∷ ObjectId → Float → Float → EngineM σ ()
handleSetSize objId width height =
    void $ modifySceneNode objId $ \node → node { nodeSize = (width, height) }

handleSetVisible ∷ ObjectId → Bool → EngineM σ ()
handleSetVisible objId visible =
    void $ modifySceneNode objId $ \node → node { nodeVisible = visible }

-- | Destroy a scene object, retiring its scene-text cache entry with it.
--
--   The cache delete is the other half of #1961's lifetime coupling and
--   is deliberately UNCONDITIONAL: @Map.delete@ on an absent key is a
--   no-op, so a sprite (or an already-destroyed id) costs nothing,
--   while every destroyed text object is guaranteed to stop answering
--   through @engine.getText@ even if the node itself had already left
--   the active graph. This is the only removal path the map has, which
--   is what keeps its @boot-process@ classification honest — see
--   "Engine.Core.Capability.Ui".
handleDestroy ∷ ObjectId → EngineM σ ()
handleDestroy objId = do
    _ ← deleteSceneNode objId
    env ← ask
    liftIO $ atomicModifyIORef' (uicTextBuffersRef (toUiCapability env)) $ \m →
      (Map.delete objId m, ())
