-- | The @input-lua-transport@ capability record (epic #537, issue #892
--   — E4): the eight fields
--   'docs/engineenv_capability_inventory.md' SS5's @input-lua-transport@
--   table groups — the raw input queue, the synthetic-injection barrier
--   pair, the published input state, the keybind registry, the
--   @onKeyDown@ current-key handoff, and the two Lua transport queues.
--
--   == This is the @LuaThread@-visible record
--
--   Two of the eight fields are @LuaThread@-PRIVATE per SS5:
--   @inputBarrierNextRef@ (the barrier-token allocator — read and
--   written by @LuaThread@ only) and @currentKeyDownRef@ (the
--   transient-handoff current key, "meaningful only for the duration
--   of one @onKeyDown@ broadcast" — again @LuaThread@ only). The
--   capability-record convention
--   ('docs/engineenv_capability_inventory.md' SS2.1) makes that a
--   structural obligation rather than a comment: because a capability
--   record is exported as @XCapability(..)@ — constructor AND
--   accessors — every module that can import THIS record can construct
--   and inspect both private fields. So this record is the
--   @LuaThread@-only one, and
--   "Engine.Core.Capability.InputView" is its strictly narrower
--   worker-safe companion, carrying neither private field. That is the
--   same main-only/worker-view shape SS3.1 defines for
--   "Engine.Core.Capability.Render" \/
--   "Engine.Core.Capability.RenderView", and
--   @tools/engine_env_capability_audit.py@ enforces it the same way:
--   only a @LuaThread@ module may import this module, and only the
--   fields' genuine owners may name either private field at all.
--
--   An input-thread or world-thread consumer therefore takes
--   'Engine.Core.Capability.InputView.InputViewCapability', never this
--   record — even though several of the six shared fields are ones it
--   legitimately uses.
--
--   Like the other capability modules, this one imports only the
--   narrow slice of @Engine.Core.State@ it needs rather than
--   @EngineEnv(..)@ or a bare import, so it is not itself a
--   full-@EngineEnv@-access consumer under the SS6 ratchet.
module Engine.Core.Capability.Input
  ( InputCapability(..)
  , toInputCapability
  ) where

import UPrelude
import Control.Concurrent.STM.TVar (TVar)
import Data.IORef (IORef)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Queue as Q
import Engine.Input.Bindings (KeyBindings)
import Engine.Input.Types (InputEvent, InputState)
import Engine.Scripting.Lua.Types (LuaMsg, LuaToEngineMsg)
import Engine.Core.State
  ( EngineEnv
  , inputQueue, inputBarrierNextRef, inputBarrierRef, inputStateRef
  , keyBindingsRef, currentKeyDownRef, luaToEngineQueue, luaQueue
  )

-- | The complete @input-lua-transport@ capability, for @LuaThread@
--   consumers only. See 'docs/engineenv_capability_inventory.md' SS5
--   @input-lua-transport@ and SS7.3.
--
--   'icInputBarrierNextRef' and 'icCurrentKeyDownRef' are the two
--   @LuaThread@-private fields the worker-safe view deliberately omits.
data InputCapability = InputCapability
  { icInputQueue          ∷ Q.Queue InputEvent
  , icInputBarrierNextRef ∷ TVar Int
  , icInputBarrierRef     ∷ TVar Int
  , icInputStateRef       ∷ IORef InputState
  , icKeyBindingsRef      ∷ IORef KeyBindings
  , icCurrentKeyDownRef   ∷ IORef (Maybe GLFW.Key)
  , icLuaToEngineQueue    ∷ Q.Queue LuaToEngineMsg
  , icLuaQueue            ∷ Q.Queue LuaMsg
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
--
--   Note the two same-typed @TVar Int@ barrier fields: the allocator
--   ('icInputBarrierNextRef') and the processed watermark
--   ('icInputBarrierRef') are DIFFERENT live containers, and a
--   transposed binding here would still typecheck while making every
--   'Engine.Input.Inject.waitForBarrier' return instantly.
--   @Test.Headless.Capability.Input@ pins each to its own named
--   counterpart for exactly that reason.
toInputCapability ∷ EngineEnv → InputCapability
toInputCapability env = InputCapability
  { icInputQueue          = inputQueue env
  , icInputBarrierNextRef = inputBarrierNextRef env
  , icInputBarrierRef     = inputBarrierRef env
  , icInputStateRef       = inputStateRef env
  , icKeyBindingsRef      = keyBindingsRef env
  , icCurrentKeyDownRef   = currentKeyDownRef env
  , icLuaToEngineQueue    = luaToEngineQueue env
  , icLuaQueue            = luaQueue env
  }
