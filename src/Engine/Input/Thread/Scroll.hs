{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Wheel/scroll dispatch (#787): Shift+scroll z-slice paging vs. UI
--   scroll-capture (#743's 'UI.InputOwnership.routeScroll') vs. plain
--   game camera-zoom scroll, plus the F4 (#730) scroll-outcome
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
import UI.InputOwnership (routeScroll)

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

    if shiftHeld
    then do
        logDebug logger CatInput "Shift+scroll: z-slice adjustment"
        Q.writeQueue (luaQueue env) (LuaZSliceScroll x y)
        recordScrollOutcome "accepted" "z_slice" Nothing
    else do
        (winW, winH) ← readIORef (windowSizeRef env)
        (fbW, fbH) ← readIORef (framebufferSizeRef env)
        let scaleX = fromIntegral fbW / fromIntegral winW
            scaleY = fromIntegral fbH / fromIntegral winH
            mouseX = realToFrac rawX * scaleX
            mouseY = realToFrac rawY * scaleY

        uiMgr ← readIORef (uiManagerRef env)
        -- Same zero-size window/framebuffer guard as the click
        -- path — now also recording the drop (#730 review: parity
        -- with the click path's own degenerate_viewport noop). The
        -- shift+scroll z-slice branch above deliberately bypasses
        -- this guard, same as before this change (passivity —
        -- recording must not alter that routing).
        if viewportDegenerate winW winH fbW fbH
          then recordScrollOutcome "noop" "degenerate_viewport" Nothing
          -- #743: routeScroll selects the topmost elementCapturesScroll
          -- surface in the same modal-scoped search click routing uses
          -- (#742) — a wheel miss stopped at a modal boundary is
          -- forwarded as a game scroll exactly like a genuine miss
          -- (Lua's isGameplayBlocked gate is what actually stops camera
          -- zoom behind a modal; see scripts/ui_manager_scroll.lua's
          -- onScroll). Unlike click routing, scroll capture carries no
          -- callback name of its own — onUIScroll dispatches purely on
          -- the element handle.
          else case routeScroll (mouseX, mouseY) uiMgr of
            Just elemHandle@(ElementHandle eh) → do
                logDebug logger CatInput $ "Scroll on UI element: " <> T.pack (show elemHandle)
                Q.writeQueue (luaQueue env) (LuaUIScrollEvent elemHandle x y)
                recordScrollOutcome "accepted" "ui_scroll" (Just eh)
            Nothing → do
                logDebug logger CatInput "Scroll: game scroll (camera zoom)"
                Q.writeQueue (luaQueue env) (LuaScrollEvent x y)
                recordScrollOutcome "accepted" "game_scroll" Nothing

    return inpSt
