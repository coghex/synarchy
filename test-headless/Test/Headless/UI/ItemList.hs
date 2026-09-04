-- | The shared item-list widget and the parameterized tabbar layouts
--   it renders its tab strip through (#1088, epic #1013 phase C0).
--
--   A composition facade (#2147): it owns the top-level
--   @Item list widget@ describe and the per-case queue drain, and
--   composes the four behavior owners in the order their describes
--   have always run.
--
--   * 'Test.Headless.UI.ItemList.Model' -- canonical grouping, the
--     tracked-temperature summary, category tabs.
--   * 'Test.Headless.UI.ItemList.Rows' -- UTF-8-safe truncation, the
--     instance/row/right-click lifecycle, scroll offsets.
--   * 'Test.Headless.UI.ItemList.Invalidation' -- rebuild
--     invalidation.
--   * 'Test.Headless.UI.ItemList.Tabbar' -- framed, row and wrapped
--     tabbar layouts.
--
--   Their shared bare-Lua-backend plumbing lives in
--   'Test.Headless.UI.ItemList.Support': a real Lua backend with the
--   full Lua API registered, so @scripts.ui.item_list@ and
--   @scripts.ui.tabbar@ are the real production modules any caller's
--   @require@ would get, driven against real UI elements on a real
--   page -- with no world, units or buildings. This exercises the
--   WIDGET's own contract directly; the three migrated hosts' wiring
--   is covered by 'Test.Headless.UI.ResponsiveGameplay'.
--
--   NB: @engine.getTextWidth@ returns 0 in this synthetic boot, so
--   every case that depends on real measurement (truncation, tab
--   shrink-to-fit, wrapping) stubs it with a deterministic
--   width-per-character model first.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Item list widget"'@.
module Test.Headless.UI.ItemList (spec) where

import UPrelude
import Test.Hspec
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import qualified Test.Headless.UI.ItemList.Model as Model
import qualified Test.Headless.UI.ItemList.Rows as Rows
import qualified Test.Headless.UI.ItemList.Invalidation as Invalidation
import qualified Test.Headless.UI.ItemList.Tabbar as Tabbar

spec ∷ SpecWith EngineEnv
spec = after drainLuaToEngineQueue $ describe "Item list widget" $ do
    Model.spec         -- grouping, tracked temperature, category tabs
    Rows.spec          -- truncation, row lifecycle, scroll offset
    Invalidation.spec  -- rebuild invalidation
    Tabbar.spec        -- parameterized tabbar layouts

-- | Empty the engine's Lua-to-engine queue after every case.
--
-- Each case boots its OWN Lua backend, so each re-runs the widget
-- module init that loads the 9-patch tab/box textures through
-- engine.loadTexture -- and every one of those posts a
-- LuaLoadTextureRequest to the queue this spec SHARES with the rest of
-- the aroundAll block. Nothing drains it here (the harness runs no
-- render thread), so the requests would pile up and later specs that
-- assert on the queue's exact contents would see this spec's leftovers
-- instead of their own single message
-- (Test.Headless.Lua.RenderQueue, and Test.Headless.Lua.PauseGate
-- through the load transaction RenderQueue leaves open when it fails).
-- Same "leave the shared engine as you found it" discipline as
-- Test.Headless.UI.TransferContextMenu's unitManagerRef bracket.
drainLuaToEngineQueue ∷ EngineEnv → IO ()
drainLuaToEngineQueue env = do
    _ ← Q.flushQueue (luaToEngineQueue env)
    pure ()
