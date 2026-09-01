{-# LANGUAGE Strict #-}
-- | Lua bindings for #2056's presentation boundary — the two verbs a
--   Lua surface needs to prove its own content reached a completed
--   renderer snapshot before acting on the assumption that the player
--   saw it. The mechanism, and why it is a proof rather than a
--   heuristic, are documented once in "UI.Manager.Presentation".
--
--   Deliberately just these two. A Lua surface never needs the raw
--   witness value, a frame number, or a render callback, so none is
--   exposed: the token is opaque, and the only question askable of it
--   is whether it has been presented yet.
module Engine.Scripting.Lua.API.UI.Presentation
  ( uiArmPresentationFn
  , uiIsPresentedFn
  ) where

import UPrelude
import qualified HsLua as Lua
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Ui (UiCapability(..), toUiCapability)
import UI.Manager (armPresentation, isPresented)

-- | UI.armPresentation() -> token
--
--   Mint a token standing for everything this surface has already
--   written to the UI manager. Call it only once the elements are built
--   AND the page carrying them is showing — a token armed before
--   @UI.showPage@ stands for a hidden page, and would be honestly
--   reported as presented while nobody could see it.
--
--   Re-arm on EVERY change to what is on screen. The tokens are
--   monotonic, so a fresh one automatically invalidates evidence
--   gathered for content that has since been hidden, collapsed,
--   scrolled or rebuilt.
uiArmPresentationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiArmPresentationFn env = do
    token ← Lua.liftIO $
        atomicModifyIORef' (uicUiManagerRef (toUiCapability env))
                           armPresentation
    Lua.pushinteger (fromIntegral token)
    return 1

-- | UI.isPresented(token) -> boolean
--
--   True once a renderer snapshot taken at or after @token@ was armed
--   has been rendered to completion. A missing, non-numeric or zero
--   token is @false@: nothing has been armed, so nothing is presented.
--
--   Always false under GPU-less @--headless@, which draws no frame —
--   that is the honest answer there, not a limitation to work around.
uiIsPresentedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiIsPresentedFn env = do
    -- Lua.tointeger coerces a numeric STRING, so check the type first
    -- (the gotcha #1497 closed elsewhere in this API).
    ty ← Lua.ltype (Lua.nthBottom 1)
    tokenArg ← if ty ≡ Lua.TypeNumber then Lua.tointeger 1 else pure Nothing
    case tokenArg of
        Just n | n > 0 → do
            mgr ← Lua.liftIO $ readIORef (uicUiManagerRef (toUiCapability env))
            Lua.pushboolean (isPresented (fromIntegral n) mgr)
        _ → Lua.pushboolean False
    return 1
