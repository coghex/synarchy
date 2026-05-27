{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric #-}

-- | Combat command + event types.
--
-- Commands flow Lua → Haskell (the combat thread drains the command
-- queue and runs resolution); events flow Haskell → Lua (Lua's
-- combat_log module drains the event buffer at its own cadence and
-- renders the log). Both sides intentionally live in this module so
-- adding a new command/event variant only touches one file.
--
-- Skeleton phase: only one command and one event are defined.
-- Resolution itself is a no-op until the next slice — the thread
-- exists so we can validate the wiring end-to-end before there's
-- anything interesting to wire.
module Combat.Types
    ( CombatCommand(..)
    , CombatEvent(..)
    , emptyEventQueue
    ) where

import UPrelude
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import Data.Word (Word32)

-- | A command issued from Lua / AI / scripts asking the combat sim
--   to do something. Drained by the combat thread.
data CombatCommand
    = CombatAttack !Word32 !Word32
      -- ^ attacker uid → target uid. The combat thread will resolve
      --   hit/miss, apply damage, and emit events.
    deriving (Show)

-- | One log-worthy thing the combat sim did. Drained by Lua to feed
--   the combat log UI.
--
--   `cePayload` is a flat key/value map so we don't have to bump a
--   typed schema every time a new field shows up (weapon, body part,
--   damage, etc.). Lua reads keys it cares about; future structured
--   typing can be added once the event vocabulary stabilises.
data CombatEvent = CombatEvent
    { ceTs       ∷ !Double
      -- ^ game-clock seconds when the event occurred
    , ceKind     ∷ !Text
      -- ^ "attack" | "hit" | "miss" | "wound" | "death" | ...
    , ceAttacker ∷ !(Maybe Word32)
    , ceTarget   ∷ !(Maybe Word32)
    , cePayload  ∷ !(HM.HashMap Text Text)
    } deriving (Show)

-- | Initial event-buffer state for `Engine.Core.Init`. Kept here so
--   the init module doesn't need to know the buffer's concrete type.
emptyEventQueue ∷ Seq.Seq CombatEvent
emptyEventQueue = Seq.empty
