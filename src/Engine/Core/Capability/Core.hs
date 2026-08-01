-- | The first capability record of the @EngineEnv@ capability split
--   (epic #537, issue #889): @core-init@ — the four fields
--   'docs/engineenv_capability_inventory.md' SS5's @core-init@ table
--   groups (boot configuration, the shared logger, the engine
--   lifecycle flag, the input-thread-started flag).
--
--   This is the first capability record, and the smallest at four
--   fields — introduced because 'Engine.Core.Log.Monad' (issue #889)
--   needed exactly it, not in anticipation of E2-E8. It established
--   the naming/placement, one-way-projection, shared-live-container,
--   no-back-import, no-record-ahead-of-need, and thread-private-split
--   rules every later capability record (E2-E8, #890-#899) follows.
--   'docs/engineenv_capability_inventory.md' SS2.1's canonical
--   convention block is their one authoritative statement; it is not
--   restated here.
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
