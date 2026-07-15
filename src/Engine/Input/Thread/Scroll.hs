{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Wheel/scroll dispatch (#787, unified #744/A3): an ordinary wheel
--   event and a Shift-held one now share ONE routing pipeline —
--   framebuffer-coordinate conversion, the degenerate-viewport guard,
--   and 'UI.InputOwnership' modal-boundary/scroll-capture policy
--   (#742/#743) — before either can become a gameplay action (camera
--   zoom or Shift z-slice paging), plus the F4 (#730) scroll-outcome
--   record. Split out of 'Engine.Input.Thread' to keep that facade a
--   thin thread-loop entrypoint; reached only through
--   'Engine.Input.Thread.Dispatch.dispatchInput'.
module Engine.Input.Thread.Scroll
  ( dispatchScrollEvent
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef (readIORef)
import Engine.Core.Log (logDebug, LogCategory(..))
import Engine.Core.State
import Engine.Input.Types
import Engine.Scripting.Lua.Types
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import Engine.Graphics.Viewport (viewportDegenerate)
import qualified Engine.Core.Queue as Q
import UI.Types (ElementHandle(..))
import UI.InputOwnership (routeScroll, isGameplayBlocked)

-- | The real per-event dispatch for 'InputScrollEvent' — split out of
--   'dispatchInput' the same way 'Engine.Input.Thread.Keyboard.dispatchKeyEvent'
--   is.
dispatchScrollEvent ∷ EngineEnv → InputState → Double → Double → IO InputState
dispatchScrollEvent env inpSt x y = do
    logger ← readIORef (loggerRef env)
    logDebug logger CatInput $ "Scroll event: dx=" <> T.pack (show x) <> ", dy=" <> T.pack (show y)

    -- Check both shifts independently: released keys keep a map
    -- entry with keyPressed=False, so a nested left-then-right
    -- lookup would stop consulting RightShift after the first
    -- LeftShift press of the session.
    let shiftDown k = maybe False keyPressed (Map.lookup k (inpKeyStates inpSt))
        shiftHeld = shiftDown GLFW.Key'LeftShift ∨ shiftDown GLFW.Key'RightShift
        (rawX, rawY) = inpMousePos inpSt

        -- F4 (#730) Layer A: exactly one record per H1 `scroll`
        -- action — InputScrollEvent carries no bracketing internal
        -- events (unlike key taps/types), so no batching is
        -- needed. Coordinates are the CURRENT cursor position
        -- (#730 review — a scroll event carries no position of
        -- its own), in the same window-space 'recordRouteOutcome'
        -- above uses for clicks.
        recordScrollOutcome ∷ Text → Text → Maybe Word32 → IO ()
        recordScrollOutcome outcome domain target = do
            gt ← readIORef (gameTimeRef env)
            pushActionOutcome (actionOutcomeRef env) ActionOutcome
                { aoTs = gt, aoKind = "input.scroll", aoOutcome = outcome
                , aoWhereX = Just rawX, aoWhereY = Just rawY, aoTarget = target
                , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
                , aoReason = Nothing, aoHandler = Just domain
                }

    (winW, winH) ← readIORef (windowSizeRef env)
    (fbW, fbH) ← readIORef (framebufferSizeRef env)
    let scaleX = fromIntegral fbW / fromIntegral winW
        scaleY = fromIntegral fbH / fromIntegral winH
        mouseX = realToFrac rawX * scaleX
        mouseY = realToFrac rawY * scaleY

    uiMgr ← readIORef (uiManagerRef env)

    -- #744: the same zero-size window/framebuffer guard the click
    -- path uses, now shared by BOTH plain and Shift-held wheel — the
    -- pre-#744 Shift branch bypassed this (and every check below)
    -- entirely.
    if viewportDegenerate winW winH fbW fbH
      then recordScrollOutcome "noop" "degenerate_viewport" Nothing
      -- #742/#743/#744: routeScroll selects the topmost in-scope
      -- elementCapturesScroll surface (the same modal-scoped search
      -- click routing uses) regardless of Shift state — a
      -- scroll-capturing panel under the cursor always wins the
      -- event, Shift held or not. Scroll capture carries no callback
      -- name of its own — onUIScroll dispatches purely on the element
      -- handle, now with the Shift flag carried alongside so Lua can
      -- still tell modified from unmodified UI scroll input.
      else case routeScroll (mouseX, mouseY) uiMgr of
        Just elemHandle@(ElementHandle eh) → do
            logDebug logger CatInput $ "Scroll on UI element: " <> T.pack (show elemHandle)
            Q.writeQueue (luaQueue env) (LuaUIScrollEvent elemHandle x y shiftHeld)
            recordScrollOutcome "accepted" "ui_scroll" (Just eh)
        -- No capturing surface under the cursor. #744: a visible
        -- modal boundary with nothing of its own here must not leak
        -- through to gameplay either — the one thing the pre-#744
        -- Shift branch got wrong (it never even looked), and the one
        -- thing the non-Shift branch used to leave to a Lua-side
        -- UI.isInputBlocked() re-check (scripts/ui_manager_scroll.lua's
        -- onScroll/onZSliceScroll) rather than deciding here. Deciding
        -- it once, in this one place, is what lets that Lua-side
        -- compensating gate go away instead of staying duplicated.
        Nothing
          | isGameplayBlocked uiMgr → do
              logDebug logger CatInput "Scroll: blocked by modal boundary"
              recordScrollOutcome "noop" "ui_modal_block" Nothing
          | shiftHeld → do
              logDebug logger CatInput "Shift+scroll: z-slice adjustment"
              Q.writeQueue (luaQueue env) (LuaZSliceScroll x y)
              recordScrollOutcome "accepted" "z_slice" Nothing
          | otherwise → do
              logDebug logger CatInput "Scroll: game scroll (camera zoom)"
              Q.writeQueue (luaQueue env) (LuaScrollEvent x y)
              recordScrollOutcome "accepted" "game_scroll" Nothing

    return inpSt
