{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | #745: release-time resolution of a discrete control's deferred
--   pointer activation — split out of 'Engine.Input.Thread.Mouse' to
--   keep that module under its line budget (mirrors the existing
--   Keyboard.hs/Scroll.hs/Char.hs split of 'Engine.Input.Thread').
--   'UI.ControlActivation.resolveActivation' is the pure decision;
--   this is the thin IO shell that reads the current 'UIPageManager',
--   converts the release position to the same framebuffer-scaled
--   space the press used, and fires the deferred
--   'Engine.Scripting.Lua.Types.LuaUIClickEvent'/'LuaUIRightClickEvent'
--   only on 'UI.ControlActivation.Activate'.
module Engine.Input.Thread.Mouse.Activation
  ( resolvePendingActivation
  ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.State
import Engine.Scripting.Lua.Types
import Engine.Graphics.Viewport (viewportDegenerate)
import qualified Engine.Core.Queue as Q
import UI.InputOwnership (PointerKind(..))
import UI.ControlActivation (PendingActivation(..), ActivationOutcome(..), resolveActivation)

-- | Resolve a button's pending activation (if any) against the
--   release position, queuing the deferred click event on
--   'UI.ControlActivation.Activate'. 'Nothing' in ⇒ 'Nothing' out
--   (a drag-activation control, the middle-button camera-drag route,
--   or no UI route at all never populates a pending activation, so
--   this is a no-op for them — the caller's existing "accepted"
--   recording stays untouched, 100% unchanged from before #745).
resolvePendingActivation
    ∷ EngineEnv → Double → Double → Int → Int → Int → Int
    → Maybe PendingActivation → IO (Maybe ActivationOutcome)
resolvePendingActivation _ _ _ _ _ _ _ Nothing = return Nothing
resolvePendingActivation env x y winW winH fbW fbH (Just pending) = do
    outcome ←
        if viewportDegenerate winW winH fbW fbH
            then return (Cancel "degenerate viewport")
            else do
                let scaleX = fromIntegral fbW / fromIntegral winW
                    scaleY = fromIntegral fbH / fromIntegral winH
                    releasePos = (realToFrac x * scaleX, realToFrac y * scaleY)
                mgr' ← readIORef (uiManagerRef env)
                return (resolveActivation releasePos mgr' pending)
    case outcome of
        Activate h cb → Q.writeQueue (luaQueue env) $ case paKind pending of
            PointerLeftClick  → LuaUIClickEvent h cb x y
            PointerRightClick → LuaUIRightClickEvent h cb x y
        Cancel _ → return ()
    return (Just outcome)
