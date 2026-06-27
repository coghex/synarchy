{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | HUD cursor-info message contract (#128).
--
--   The shared HUD info panel carries four tabs. Basic/Advanced are
--   owned by whichever selection is active; the dynamic Weather and
--   Resources tabs belong ONLY to chunk (zoom-map) selection. Both
--   chunk and tile selection route their Basic/Advanced text through
--   the same 'sendHudInfo' path, and on the Lua side
--   @infoPanel.useSchema("tile")@ is a no-op when already on the tile
--   schema — so a tile readout cannot clear the Weather/Resources tabs
--   on its own. The bug (#128): after a chunk selection populated those
--   tabs, switching to a zoomed-in tile left them visible with stale
--   chunk data.
--
--   The fix lives in 'pollCursorInfo': a tile-selection change now also
--   pushes empty Weather/Resources so those tabs disappear. On tile
--   DEselect it only clears them when no chunk is still selected, so a
--   simultaneous tile-clear + chunk-select transition can't clobber the
--   chunk's freshly-pushed tabs.
--
--   These specs drive the real 'pollCursorInfo' against a hand-built
--   visible world (no world thread, so the polling is deterministic)
--   and assert the exact 'LuaMsg's queued for the Lua HUD.
module Test.Headless.World.CursorInfo (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (writeIORef, modifyIORef')
import qualified Engine.Core.Queue as Q
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Thread.Cursor (pollCursorInfo)
import World.State.Types ( WorldState(..), emptyWorldState
                         , WorldManager(..), CursorSnapshot(..) )
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Page.Types (WorldPageId(..))

pid ∷ WorldPageId
pid = WorldPageId "cursor_info_test"

-- | Drain every queued Lua message (non-blocking).
drainLua ∷ EngineEnv → IO [LuaMsg]
drainLua env = go []
  where
    go acc = do
        m ← Q.tryReadQueue (luaQueue env)
        case m of
            Nothing → pure (reverse acc)
            Just x  → go (x : acc)

-- | A fresh empty world, registered as the sole visible page, with the
--   Lua queue drained so the next 'pollCursorInfo' starts clean.
freshVisibleWorld ∷ EngineEnv → IO WorldState
freshVisibleWorld env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) (WorldManager [(pid, ws)] [pid])
    _ ← drainLua env
    pure ws

spec ∷ Spec
spec = beforeAll initEnv $ do

    it "tile selection clears the chunk Weather and Resources tabs" $ \env → do
        ws ← freshVisibleWorld env
        -- Chunk selection first: this is what populates Weather/Resources.
        modifyIORef' (wsCursorRef ws) (\c → c { zoomSelectedPos = Just (0, 0) })
        pollCursorInfo env
        _ ← drainLua env  -- discard the chunk-selection messages
        -- Now select a zoomed-in tile.
        modifyIORef' (wsCursorRef ws) (\c → c { worldSelectedTile = Just (5, 5, 1) })
        pollCursorInfo env
        msgs ← drainLua env
        -- The tile readout must wipe the dynamic chunk tabs.
        msgs `shouldSatisfy` elem (LuaHudLogWeatherInfo "")
        msgs `shouldSatisfy` elem (LuaHudLogResourcesInfo "")

    it "tile deselect does NOT clear chunk tabs while a chunk stays selected" $ \env → do
        ws ← freshVisibleWorld env
        -- Start with both a chunk and a tile selected; snapshot agrees so
        -- only the tile-deselect transition fires this poll.
        writeIORef (wsCursorRef ws)
            (emptyCursorState { zoomSelectedPos   = Just (0, 0)
                              , worldSelectedTile = Just (5, 5, 1) })
        writeIORef (wsCursorSnapshotRef ws)
            (CursorSnapshot (Just (0, 0)) (Just (5, 5, 1)))
        -- Deselect the tile only.
        modifyIORef' (wsCursorRef ws) (\c → c { worldSelectedTile = Nothing })
        pollCursorInfo env
        msgs ← drainLua env
        -- Only the Basic/Advanced clear; the chunk's tabs are left alone.
        msgs `shouldBe` [LuaHudLogInfo "" ""]

    it "tile deselect clears chunk tabs when no chunk is selected" $ \env → do
        ws ← freshVisibleWorld env
        writeIORef (wsCursorRef ws)
            (emptyCursorState { worldSelectedTile = Just (5, 5, 1) })
        writeIORef (wsCursorSnapshotRef ws)
            (CursorSnapshot Nothing (Just (5, 5, 1)))
        modifyIORef' (wsCursorRef ws) (\c → c { worldSelectedTile = Nothing })
        pollCursorInfo env
        msgs ← drainLua env
        msgs `shouldSatisfy` elem (LuaHudLogWeatherInfo "")
        msgs `shouldSatisfy` elem (LuaHudLogResourcesInfo "")

  where
    initEnv ∷ IO EngineEnv
    initEnv = do
        EngineInitResult env ← initializeEngineHeadless
        pure env
