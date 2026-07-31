-- | The __worker-visible__ input view of the @input-lua-transport@
--   capability (epic #537, issue #892 — E4): the strictly narrower
--   companion to "Engine.Core.Capability.Input", for production code
--   that runs on a thread other than @LuaThread@ — the input thread's
--   own dispatch chain and the world thread's Lua-message producers.
--
--   == Why a second record exists
--
--   'docs/engineenv_capability_inventory.md' SS5 marks two of the
--   capability's eight fields @LuaThread@-private:
--   @inputBarrierNextRef@ (the synthetic-injection barrier-token
--   allocator) and @currentKeyDownRef@ (the transient @onKeyDown@
--   current-key handoff). E1's convention exports every capability
--   record as @Capability(..)@ — constructor and accessors alike — so
--   a single eight-field record visible to the input thread would hand
--   @InputThread@\/@WorldThread@ code a way to allocate barrier tokens
--   and to inspect (or overwrite) the Lua thread's in-flight key. This
--   record is the resolution, exactly as
--   "Engine.Core.Capability.RenderView" resolves the same problem for
--   @engineStateRef@ (SS3.1): it __contains neither field at all__, so
--   there is no path from here to either.
--
--   It is a projection of 'EngineEnv' in its own right — never derived
--   from, and never widened back into,
--   'Engine.Core.Capability.Input.InputCapability'.
--
--   == What is deliberately absent
--
--   Beyond the two private fields, @luaToEngineQueue@ is omitted on
--   E1's "no unused capability records ahead of need" grounds, applied
--   field-by-field: its only production consumers are the permanently
--   full-access orchestration modules SS6.1 lists
--   ('Engine.Scripting.Lua.Thread', 'Engine.Scripting.Lua.Message')
--   plus two API modules that already import the accessor narrowly
--   ('Engine.Scripting.Lua.API.Text', 'Engine.Scripting.Lua.API.Config'),
--   so no module this migration narrows needs it. A later migration
--   that has a real consumer adds it then.
--
--   Like the other capability modules, this one imports only the
--   narrow slice of @Engine.Core.State@ it needs rather than
--   @EngineEnv(..)@ or a bare import, so it is not itself a
--   full-@EngineEnv@-access consumer under the SS6 ratchet.
module Engine.Core.Capability.InputView
  ( InputViewCapability(..)
  , toInputViewCapability
  ) where

import UPrelude
import Control.Concurrent.STM.TVar (TVar)
import Data.IORef (IORef)
import Engine.Core.Queue as Q
import Engine.Input.Bindings (KeyBindings)
import Engine.Input.Types (InputEvent, InputState)
import Engine.Scripting.Lua.Types (LuaMsg)
import Engine.Core.State
  ( EngineEnv
  , inputQueue, inputBarrierRef, inputStateRef, keyBindingsRef, luaQueue
  )

-- | The worker-safe slice of @input-lua-transport@: the raw input
--   queue the input thread drains (and a load publish flushes), the
--   barrier watermark the input thread PUBLISHES after processing a
--   token, the published input state, the keybind registry the
--   keyboard dispatch consults, and the engine→Lua message queue.
--
--   Every field here is a container SS5 records an @InputThread@,
--   @WorldThread@ or @MainRender@ reader or writer for. Writes stay
--   exactly as SS5 classifies them — this record grants no new write
--   authority, it only removes the ability to reach the two fields a
--   non-Lua thread has no business touching.
data InputViewCapability = InputViewCapability
  { ivInputQueue      ∷ Q.Queue InputEvent
  , ivInputBarrierRef ∷ TVar Int
  , ivInputStateRef   ∷ IORef InputState
  , ivKeyBindingsRef  ∷ IORef KeyBindings
  , ivLuaQueue        ∷ Q.Queue LuaMsg
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied, and
--   nothing is routed through
--   'Engine.Core.Capability.Input.InputCapability'.
toInputViewCapability ∷ EngineEnv → InputViewCapability
toInputViewCapability env = InputViewCapability
  { ivInputQueue      = inputQueue env
  , ivInputBarrierRef = inputBarrierRef env
  , ivInputStateRef   = inputStateRef env
  , ivKeyBindingsRef  = keyBindingsRef env
  , ivLuaQueue        = luaQueue env
  }
