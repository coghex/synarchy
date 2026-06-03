{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.Event where

import UPrelude
import Engine.Core.Monad (EngineM)

-- | Vestigial no-op. Input state is consumed directly from
--   'inputStateRef' (EngineEnv) by the camera loop and the Lua input
--   API; 'EngineState' no longer caches a copy (audit 2026-06 Tier-1:
--   no cross-thread field is duplicated into EngineState). Kept as a
--   no-op so the main-loop call site doesn't need to change.
handleInputEvents ∷ EngineM ε σ ()
handleInputEvents = pure ()
