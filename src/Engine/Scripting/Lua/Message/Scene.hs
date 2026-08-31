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

-- The scene-text cache invariant
--
--   The invariant every handler below maintains (#1961):
--   'uicTextBuffersRef' holds an entry for an 'ObjectId' EXACTLY when
--   the active scene graph holds a TEXT node at that id, and the entry
--   is that node's text. That is what makes the field's
--   @boot-process@\/reset-@None@ classification honest — entries are
--   retired with their nodes, so a session boundary has nothing to
--   clear (see "Engine.Core.Capability.Ui").
--
--   Every write goes through 'cacheSceneText' or 'forgetSceneText', so
--   the invariant is maintained in one place rather than at four call
--   sites. There are exactly four transitions that can break it, and
--   each is handled: a text node is added ('handleSpawnText'), a live
--   text node's string changes ('handleSetText'), a node is destroyed
--   ('handleDestroy'), and — the one that is easy to miss — a node is
--   REPLACED, because 'Engine.Scene.Graph.addNode' is a @Map.insert@:
--   spawning a sprite over an id that already names a text node
--   ('handleSpawnSprite') leaves an object with no text, so its entry
--   must go too.

-- | Record @text@ as the scene-text for @oid@, whose text node the
--   caller has just confirmed exists.
cacheSceneText ∷ ObjectId → Text → EngineM σ ()
cacheSceneText oid text = do
    env ← ask
    liftIO $ atomicModifyIORef' (uicTextBuffersRef (toUiCapability env)) $ \m →
      (Map.insert oid text m, ())

-- | Retire @oid@'s scene-text entry, because it no longer names a text
--   node. Deliberately unconditional: @Map.delete@ on an absent key is
--   a no-op, so an id that never had text costs nothing, and no id can
--   be left answering through @engine.getText@ for an object that is
--   gone or is no longer text.
forgetSceneText ∷ ObjectId → EngineM σ ()
forgetSceneText oid = do
    env ← ask
    liftIO $ atomicModifyIORef' (uicTextBuffersRef (toUiCapability env)) $ \m →
      (Map.delete oid m, ())

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
            cacheSceneText oid text
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
--   never existed, in a map with no removal path at all.
handleSetText ∷ ObjectId → Text → EngineM σ ()
handleSetText objId text = do
    nodeUpdated ← modifySceneNode objId $ \node → node { nodeText = Just text }
    when nodeUpdated $ cacheSceneText objId text

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
            -- 'addNode' is a @Map.insert@, so this REPLACES whatever
            -- node held @objId@ — possibly a live text node, once
            -- 'nextObjectIdRef''s 'Word32' wraps or an id is otherwise
            -- reused. A sprite has no text, so the replaced node's
            -- cache entry must go with it or 'engine.getText' would
            -- keep answering for an object whose @nodeText@ is now
            -- 'Nothing' (#1961).
            forgetSceneText objId
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

-- | Destroy a scene object, retiring its scene-text cache entry with it
--   (#1961) — the destruction half of the invariant described above.
handleDestroy ∷ ObjectId → EngineM σ ()
handleDestroy objId = do
    _ ← deleteSceneNode objId
    forgetSceneText objId
