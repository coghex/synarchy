{-# LANGUAGE UnicodeSyntax #-}
-- | The first capability record of the @EngineEnv@ capability split
--   (epic #537, issue #889): @core-init@ — the four fields
--   'docs/engineenv_capability_inventory.md' SS5's @core-init@ table
--   groups (boot configuration, the shared logger, the engine
--   lifecycle flag, the input-thread-started flag).
--
--   __The convention every later capability record (E2+, #890-#899)
--   follows:__
--
--   * __Naming and placement.__ One module per capability under
--     @Engine.Core.Capability.\<Name\>@, exporting one record named
--     @\<Name\>Capability@ with fields prefixed by the record's own
--     initials (here @cc@), plus a single @to\<Name\>Capability@
--     projection.
--   * __One-way projection only.__ The projection function goes
--     @EngineEnv → XCapability@ and nothing travels the other
--     direction — a capability record is never assembled back into
--     (or used to reconstruct) an @EngineEnv@.
--   * __Shared live containers, never copied state.__ Every field is
--     the exact same 'IORef'\/'Data.IORef.IORef'-like handle (or
--     immutable value) 'EngineEnv' already carries — the projection
--     aliases live state, it does not snapshot or duplicate it. A
--     capability record is safe to construct as often as needed
--     precisely because it's just a narrower view over the same
--     handles.
--   * __No capability module imports its own consumers.__ This module
--     (and every future @Engine.Core.Capability.*@ module) may be
--     imported freely by the modules it narrows access for, but must
--     never import back into them.
--   * __No unused capability records ahead of need.__ A capability
--     record is introduced only in the migration issue that actually
--     narrows a real consumer to it — this module exists because
--     'Engine.Core.Log.Monad' (this issue) needs exactly it, not in
--     anticipation of E2-E8.
--
--   This module deliberately imports only the narrow slice of
--   @Engine.Core.State@ it needs (the bare 'EngineEnv' type, the four
--   field accessors below, and 'EngineLifecycle') rather than
--   @EngineEnv(..)@ or a bare module import — the same narrow-import
--   shape 'Engine.Core.Resource' already uses for 'loggerRef' alone —
--   so this module itself is not a full-@EngineEnv@-access consumer
--   under @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.Core
  ( CoreCapability(..)
  , toCoreCapability
  ) where

import UPrelude
import Data.IORef (IORef)
import Engine.Core.Log (LoggerState)
import Engine.Core.Types (EngineConfig)
import Engine.Core.State
  ( EngineEnv, EngineLifecycle
  , engineConfig, loggerRef, lifecycleRef, inputThreadActiveRef
  )

-- | The @core-init@ capability: boot configuration, the shared
--   logger, the engine lifecycle flag, and whether the input thread
--   has started. See 'docs/engineenv_capability_inventory.md' SS5
--   @core-init@ and SS7.1.
data CoreCapability = CoreCapability
  { ccEngineConfig         ∷ EngineConfig
  , ccLoggerRef            ∷ IORef LoggerState
  , ccLifecycleRef         ∷ IORef EngineLifecycle
  , ccInputThreadActiveRef ∷ IORef Bool
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toCoreCapability ∷ EngineEnv → CoreCapability
toCoreCapability env = CoreCapability
  { ccEngineConfig         = engineConfig env
  , ccLoggerRef            = loggerRef env
  , ccLifecycleRef         = lifecycleRef env
  , ccInputThreadActiveRef = inputThreadActiveRef env
  }
