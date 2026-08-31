-- | Issue #1961: the scene-object text cache behind 'uicTextBuffersRef'
--   must follow its scene nodes' own lifetimes.
--
--   "Engine.Core.Capability.Ui" classifies that map @boot-process@ with
--   reset @None@ — @World.Load.Publish.resetTransientState@ deliberately
--   never touches it — and that is only honest if entries are created
--   and removed with the nodes they describe. They were not: the
--   handler inserted before checking whether the node existed, and
--   destroying an object removed nothing, so @engine.getText@ answered
--   for ids that named no object and for objects that had been
--   destroyed, in a map with no removal path at all.
--
--   These cases drive the REAL production route — a 'LuaToEngineMsg' on
--   'luaToEngineQueue', drained by 'processLuaMessages', dispatched to
--   "Engine.Scripting.Lua.Message.Scene" — rather than calling a
--   handler directly, so the queue-to-handler routing is covered too.
--   The scene handlers are pure scene-graph + IORef bookkeeping with no
--   GPU calls, and every GPU step in 'processLuaMessages' sits behind
--   @whenGraphical@, so the whole path runs headless.
--
--   The one thing headless does NOT give us is an active scene:
--   'initializeEngineHeadless' inherits @defaultEngineState@'s empty
--   'sceneManager', and the default scene is created by
--   "Engine.Graphics.Vulkan.Init". 'withActiveScene' installs the same
--   @\"default\"@ scene that boot does and restores BOTH the previous
--   'sceneManager' and the previous text-buffer map afterward, so
--   nothing leaks into the @aroundAll@ environment these specs share.
module Test.Headless.Lua.SceneText (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket)
import qualified Data.Map.Strict as Map
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (FontHandle(..))
import Engine.Core.Capability.Ui (UiCapability(..), toUiCapability)
import Engine.Core.Monad (EngineM', runEngineM)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), EngineState(..))
import Engine.Graphics.Camera (defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import Engine.Scene.Base (LayerId(..), ObjectId(..))
import Engine.Scene.Manager (createScene, setActiveScene)
import Engine.Scene.Types
    (SceneGraph(..), SceneManager(..), SceneNode(..))
import Engine.Scripting.Lua.Message (processLuaMessages)
import Engine.Scripting.Lua.Types (LuaToEngineMsg(..))

-- | An id no case spawns, standing in for the issue's
--   @engine.setText(424242, \"orphan\")@ repro.
orphanId ∷ ObjectId
orphanId = ObjectId 424242

-- | The id the live-node cases spawn. Distinct from 'orphanId' so a
--   single case can assert on both at once.
liveId ∷ ObjectId
liveId = ObjectId 90001

-- | Queue @msgs@ and drain them through the real per-frame dispatcher.
pump ∷ EngineEnv → [LuaToEngineMsg] → IO ()
pump env msgs = do
    mapM_ (Q.writeQueue (luaToEngineQueue env)) msgs
    let action ∷ EngineM' ()
        action = processLuaMessages
    _ ← runEngineM action env pure
    pure ()

-- | Spawn @liveId@ as a real text node carrying @text@.
spawnLive ∷ Text → LuaToEngineMsg
spawnLive text =
    LuaSpawnTextRequest liveId 0 0 (FontHandle 0) text
                        (Vec4 1 1 1 1) (LayerId 0) 12

-- | The scene-text cache's current entry for @oid@, which is exactly
--   what @engine.getText@ reads ("Engine.Scripting.Lua.API.Text").
cachedText ∷ EngineEnv → ObjectId → IO (Maybe Text)
cachedText env oid =
    Map.lookup oid <$> readIORef (uicTextBuffersRef (toUiCapability env))

-- | The active scene graph's current 'nodeText' for @oid@ — the other
--   half of the coupling, read straight off the node.
nodeTextOf ∷ EngineEnv → ObjectId → IO (Maybe Text)
nodeTextOf env oid = do
    st ← readIORef (engineStateRef env)
    let mgr = sceneManager st
    pure $ do
        sid   ← smActiveScene mgr
        graph ← Map.lookup sid (smSceneGraphs mgr)
        node  ← Map.lookup oid (sgNodes graph)
        nodeText node

-- | Run @body@ with the boot-shaped @\"default\"@ scene active,
--   restoring the shared env's scene manager and text-buffer map
--   afterward however the body ends.
withActiveScene ∷ EngineEnv → IO α → IO α
withActiveScene env body = bracket install restore (const body)
  where
    install = do
        st ← readIORef (engineStateRef env)
        buffers ← readIORef (uicTextBuffersRef (toUiCapability env))
        let mgr = setActiveScene "default"
                    (createScene "default" defaultCamera (sceneManager st))
        writeIORef (engineStateRef env) st { sceneManager = mgr }
        pure (sceneManager st, buffers)
    restore (mgr, buffers) = do
        atomicModifyIORef' (engineStateRef env) $ \s →
            (s { sceneManager = mgr }, ())
        writeIORef (uicTextBuffersRef (toUiCapability env)) buffers

spec ∷ SpecWith EngineEnv
spec = describe "scene-text cache lifetime (issue #1961)" $ do
    it "leaves no cache entry when setText names an id with no scene \
       \node, so engine.getText still reports nothing for it" $ \env →
        withActiveScene env $ do
            pump env [LuaSetTextRequest orphanId "orphan"]

            cachedText env orphanId `shouldReturn` Nothing
            -- The scene-graph half is unchanged too: a missing node is
            -- not conjured into existence by naming it.
            nodeTextOf env orphanId `shouldReturn` Nothing

    it "still updates both the node and the cache when setText names a \
       \live text node" $ \env →
        withActiveScene env $ do
            pump env [spawnLive "spawned"]
            cachedText env liveId `shouldReturn` Just "spawned"

            pump env [LuaSetTextRequest liveId "updated"]

            nodeTextOf env liveId `shouldReturn` Just "updated"
            cachedText env liveId `shouldReturn` Just "updated"

    it "drops the cache entry when the object is destroyed, so \
       \engine.getText reports nothing for a destroyed id" $ \env →
        withActiveScene env $ do
            pump env [spawnLive "doomed"]
            cachedText env liveId `shouldReturn` Just "doomed"

            pump env [LuaDestroyRequest liveId]

            cachedText env liveId `shouldReturn` Nothing
            nodeTextOf env liveId `shouldReturn` Nothing

    it "does not resurrect a destroyed id: setText after destroy is the \
       \unknown-id no-op again" $ \env →
        withActiveScene env $ do
            pump env [spawnLive "doomed", LuaDestroyRequest liveId]

            pump env [LuaSetTextRequest liveId "ghost"]

            cachedText env liveId `shouldReturn` Nothing

    it "leaves the map empty across a spawn/set/destroy round trip, \
       \which is what makes its boot-process reset None honest" $ \env →
        withActiveScene env $ do
            before ← readIORef (uicTextBuffersRef (toUiCapability env))
            pump env [ spawnLive "one"
                     , LuaSetTextRequest liveId "two"
                     , LuaSetTextRequest orphanId "never"
                     , LuaDestroyRequest liveId ]

            after ← readIORef (uicTextBuffersRef (toUiCapability env))
            Map.keys after `shouldBe` Map.keys before
