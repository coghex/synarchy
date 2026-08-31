-- | @debug.getSceneStats()@ — the read-only scene-assembly telemetry
--   query (#1921), registered on the @debug@ table beside the other
--   diagnostic verbs.
--
--   It reports whatever the world thread published at the end of its
--   last completed 'World.Render.updateWorldTiles' pass. The whole
--   snapshot is read with one 'readIORef' of an immutable value, so the
--   rows a caller sees always come from ONE pass — never a mixture of
--   two — and reading can no more disturb the pass than it can be
--   disturbed by it.
--
--   The query needs no GPU: the world thread runs headless, so it
--   answers in @--headless@ and @--offscreen@ alike. What differs there
--   is the EMITTED counts of the three texture-gated categories, not
--   the availability of the answer.
--
--   The returned table is freshly built on every call, so a Lua caller
--   mutating it changes nothing the engine holds.
module Engine.Scripting.Lua.API.SceneStats
  ( getSceneStatsFn
  ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Scene.Stats
    ( SceneCategoryStat(..), SceneStats(..)
    , sceneCategoryId, sceneCategoryOrder, zeroCategoryStat )
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua

-- | @debug.getSceneStats() →@
--   @{available, sequence, categories = {{id, scanned, emitted, durationNs}, ...}}@
--
--   @categories@ is a dense array of exactly ten rows, in
--   'sceneCategoryOrder', whatever the state: before the first
--   completed pass and after a world teardown the query answers
--   @available = false@, @sequence = 0@ and ten zero-valued rows rather
--   than an empty or absent table, so a caller never has to distinguish
--   "no telemetry" from "a category is missing".
--
--   @durationNs@ is a whole-nanosecond elapsed monotonic-clock
--   duration. Counts and durations are non-negative integers.
getSceneStatsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getSceneStatsFn env = do
    mStats ← Lua.liftIO $
        readIORef (rhSceneStatsRef (toRenderHandoffCapability env))
    let (available, sequenceNo, rows) = case mStats of
            Nothing → (False, 0, map zeroCategoryStat sceneCategoryOrder)
            Just stats → (True, ssSequence stats, ssCategories stats)
    Lua.newtable
    Lua.pushboolean available
    Lua.setfield (-2) "available"
    Lua.pushinteger (fromIntegral sequenceNo)
    Lua.setfield (-2) "sequence"
    Lua.newtable
    forM_ (zip [1 ..] rows) $ \(i, row) → do
        pushCategoryRow row
        Lua.rawseti (-2) i
    Lua.setfield (-2) "categories"
    return 1

pushCategoryRow ∷ SceneCategoryStat → Lua.LuaE Lua.Exception ()
pushCategoryRow row = do
    Lua.newtable
    Lua.pushstring (TE.encodeUtf8 (sceneCategoryId (scsCategory row)))
    Lua.setfield (-2) "id"
    Lua.pushinteger (fromIntegral (scsScanned row))
    Lua.setfield (-2) "scanned"
    Lua.pushinteger (fromIntegral (scsEmitted row))
    Lua.setfield (-2) "emitted"
    Lua.pushinteger (fromIntegral (scsDurationNs row))
    Lua.setfield (-2) "durationNs"
