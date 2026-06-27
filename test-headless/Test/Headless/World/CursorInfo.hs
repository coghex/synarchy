{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | HUD cursor-info message contract (#128).
--
--   The shared HUD info panel carries four tabs. Basic/Advanced are
--   owned by whichever selection is active; the dynamic Weather and
--   Resources tabs belong ONLY to chunk (zoom-map) selection. Both
--   chunk and tile selection push their Basic/Advanced text through the
--   same 'sendHudInfo' path, and on the Lua side
--   @infoPanel.useSchema("tile")@ is a no-op when already on the tile
--   schema — so a tile readout cannot clear the Weather/Resources tabs
--   on its own. The bug (#128): after a chunk selection populated those
--   tabs, switching to a zoomed-in tile left them visible with stale
--   chunk data.
--
--   The fix lives in 'pollCursorInfo', which now renders ONE coherent
--   HUD state per change from the combined cursor (a selected tile owns
--   the panel and clears the chunk-only tabs; otherwise a selected
--   chunk shows its tabs; otherwise the panel is empty) rather than two
--   independent updates that could fight inside a single tick.
--
--   These specs drive the real 'pollCursorInfo' against a hand-built
--   visible world (no world thread, so the polling is deterministic)
--   and assert the exact 'LuaMsg's queued for the Lua HUD. The world is
--   given gen params with an empty climate grid so a chunk selection
--   produces a NON-EMPTY Weather tab ("No climate data") — that is what
--   makes the dynamic tab "active", i.e. the realistic stale-tab
--   precondition the tile selection then has to clear. (Seed 0 marks
--   the params as an arena world, which keeps the Resources survey off
--   the transient chunk-generation path so the specs stay cheap.)
module Test.Headless.World.CursorInfo (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Data.IORef (writeIORef, modifyIORef')
import qualified Engine.Core.Queue as Q
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Thread.Cursor (pollCursorInfo)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.State.Types ( WorldState(..), emptyWorldState
                         , WorldManager(..), CursorSnapshot(..) )
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Page.Types (WorldPageId(..))

pid ∷ WorldPageId
pid = WorldPageId "cursor_info_test"

-- | Gen params whose climate grid is empty, so 'chunkWeatherInfo'
--   yields a non-empty "No climate data" Weather tab. Seed 0 flags it
--   as an arena world so the Resources survey skips transient chunk
--   generation.
testParams ∷ WorldGenParams
testParams = defaultWorldGenParams { wgpSeed = 0 }

-- | Drain every queued Lua message (non-blocking).
drainLua ∷ EngineEnv → IO [LuaMsg]
drainLua env = go []
  where
    go acc = do
        m ← Q.tryReadQueue (luaQueue env)
        case m of
            Nothing → pure (reverse acc)
            Just x  → go (x : acc)

weatherMsgs ∷ [LuaMsg] → [Text]
weatherMsgs msgs = [t | LuaHudLogWeatherInfo t ← msgs]

resourceMsgs ∷ [LuaMsg] → [Text]
resourceMsgs msgs = [t | LuaHudLogResourcesInfo t ← msgs]

-- | Basic-tab text of every Basic/Advanced push, in order.
infoBasics ∷ [LuaMsg] → [Text]
infoBasics msgs = [b | LuaHudLogInfo b _ ← msgs]

-- | A fresh empty world (with climate-less gen params), registered as
--   the sole visible page, Lua queue drained so the next
--   'pollCursorInfo' starts clean.
freshVisibleWorld ∷ EngineEnv → IO WorldState
freshVisibleWorld env = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just testParams)
    writeIORef (worldManagerRef env) (WorldManager [(pid, ws)] [pid])
    _ ← drainLua env
    pure ws

-- | Select chunk (0,0) and poll, returning the queued messages.
selectChunk ∷ EngineEnv → WorldState → IO [LuaMsg]
selectChunk env ws = do
    modifyIORef' (wsCursorRef ws) (\c → c { zoomSelectedPos = Just (0, 0) })
    pollCursorInfo env
    drainLua env

spec ∷ Spec
spec = beforeAll initEnv $ do

    it "a chunk selection activates the Weather tab" $ \env → do
        ws ← freshVisibleWorld env
        msgs ← selectChunk env ws
        -- Precondition for the #128 scenario: the dynamic Weather tab is
        -- non-empty (here "No climate data"), i.e. genuinely active.
        weatherMsgs msgs `shouldSatisfy` any (≢ "")

    it "tile selection clears the active Weather and Resources tabs" $ \env → do
        ws ← freshVisibleWorld env
        _ ← selectChunk env ws  -- Weather tab now active
        -- Switch to a zoomed-in tile.
        modifyIORef' (wsCursorRef ws) (\c → c { worldSelectedTile = Just (8, 8, 1) })
        pollCursorInfo env
        msgs ← drainLua env
        -- The tile readout must wipe the chunk-only tabs...
        weatherMsgs msgs  `shouldSatisfy` elem ""
        resourceMsgs msgs `shouldSatisfy` elem ""
        -- ...and show the tile in Basic/Advanced.
        infoBasics msgs `shouldSatisfy` any (T.isInfixOf "Tile (")

    it "clicking a chunk shows the chunk, not a stale still-selected tile" $ \env → do
        ws ← freshVisibleWorld env
        -- A tile is selected and already shown (snapshot agrees). Zooming
        -- out does not clear worldSelectedTile (hud.onScroll only clears
        -- the panel — issue 135), so the tile lingers in the cursor.
        writeIORef (wsCursorRef ws)
            (emptyCursorState { worldSelectedTile = Just (8, 8, 1) })
        writeIORef (wsCursorSnapshotRef ws)
            (CursorSnapshot Nothing (Just (8, 8, 1)))
        -- Now click a chunk in the zoomed-out view: only zoomSelectedPos
        -- changes; the stale tile is unchanged.
        modifyIORef' (wsCursorRef ws) (\c → c { zoomSelectedPos = Just (0, 0) })
        pollCursorInfo env
        msgs ← drainLua env
        -- The chunk the user just clicked must win, not the stale tile.
        infoBasics msgs `shouldSatisfy` any (T.isInfixOf "Chunk (")
        infoBasics msgs `shouldSatisfy` (not . any (T.isInfixOf "Tile ("))

    it "deselecting a tile empties the panel even when a chunk stays selected" $ \env → do
        ws ← freshVisibleWorld env
        -- Established state: chunk + tile both selected, snapshot agrees
        -- so only the tile-deselect transition fires this poll. (A chunk
        -- selection can persist into the zoomed-in view — issue 135.)
        writeIORef (wsCursorRef ws)
            (emptyCursorState { zoomSelectedPos   = Just (0, 0)
                              , worldSelectedTile = Just (8, 8, 1) })
        writeIORef (wsCursorSnapshotRef ws)
            (CursorSnapshot (Just (0, 0)) (Just (8, 8, 1)))
        -- Deselect the tile only.
        modifyIORef' (wsCursorRef ws) (\c → c { worldSelectedTile = Nothing })
        pollCursorInfo env
        msgs ← drainLua env
        -- An empty Basic payload must be sent (the arena tile-editor popup
        -- couples its teardown to it — scripts/tile_editor.lua), and the
        -- chunk readout must NOT come back and strand that popup.
        infoBasics msgs  `shouldSatisfy` elem ""
        infoBasics msgs  `shouldSatisfy` (not . any (T.isInfixOf "Chunk ("))
        weatherMsgs msgs `shouldSatisfy` elem ""
        resourceMsgs msgs `shouldSatisfy` elem ""

    it "a chunk-select + tile-deselect in one tick empties the panel coherently" $ \env → do
        ws ← freshVisibleWorld env
        -- Old state (snapshot): no chunk, a tile selected.
        writeIORef (wsCursorSnapshotRef ws)
            (CursorSnapshot Nothing (Just (8, 8, 1)))
        -- New state (cursor): chunk selected, tile gone — BOTH changed.
        writeIORef (wsCursorRef ws)
            (emptyCursorState { zoomSelectedPos = Just (0, 0) })
        pollCursorInfo env
        msgs ← drainLua env
        -- A single coherent blank — NOT a chunk readout rendered and then
        -- blanked by a competing tile-deselect write (the point-1 race).
        infoBasics msgs `shouldBe` [""]
        weatherMsgs msgs `shouldSatisfy` elem ""
        resourceMsgs msgs `shouldSatisfy` elem ""

    it "deselecting everything empties the panel" $ \env → do
        ws ← freshVisibleWorld env
        writeIORef (wsCursorSnapshotRef ws)
            (CursorSnapshot Nothing (Just (8, 8, 1)))
        writeIORef (wsCursorRef ws) emptyCursorState
        pollCursorInfo env
        msgs ← drainLua env
        infoBasics msgs  `shouldSatisfy` elem ""
        weatherMsgs msgs `shouldSatisfy` elem ""
        resourceMsgs msgs `shouldSatisfy` elem ""

  where
    initEnv ∷ IO EngineEnv
    initEnv = do
        EngineInitResult env ← initializeEngineHeadless
        pure env
