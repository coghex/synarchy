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
    , AttackMode(..)
    , attackModeText
    , emptyEventQueue
    , pushInjuryEvent
    ) where

import UPrelude
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef')
import Data.Word (Word32)

-- | Which swing the attacker is throwing.
--
--   Quick uses sqrt(strength) for damage and the full dexterity weight
--   for aim — a controlled, finesse-leaning motion.
--
--   Heavy uses linear strength (puts full body weight in) and half
--   dexterity weight (committed motion, can't course-correct mid-swing).
--
--   No flat damage/hit multipliers — the heavy/quick differential is
--   entirely a function of how strength and dexterity get applied, so
--   a high-strength unit naturally benefits more from heavy than a
--   high-dexterity unit does.
data AttackMode = Quick | Heavy
    deriving (Show, Eq)

attackModeText ∷ AttackMode → Text
attackModeText Quick = "quick"
attackModeText Heavy = "heavy"

-- | A command issued from Lua / AI / scripts asking the combat sim
--   to do something. Drained by the combat thread.
data CombatCommand
    = CombatAttack !Word32 !Word32 !AttackMode !Float !Float
      -- ^ attacker uid → target uid + swing type + REACH BONUS + IMPACT
      --   SPEED. reachBonus (metres) is added to the attacker's strike-
      --   height reach (0 for a normal swing); a lunge passes its leap
      --   strike-reach so it can hit higher parts. impactSpeed (m/s) is
      --   the lunge's body travel speed (0 for a normal swing); the body's
      --   full-mass momentum (½·m·v² / m·v) folds into the strike. The
      --   combat thread resolves hit/miss, applies damage + stamina
      --   drain, and emits events.
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

-- | Append a NON-combat injury event to a stream buffer (the victim
--   rides in `ceTarget`, no attacker). Shared by the engine-side injury
--   producers — Unit.Fall and unit.injure — so they don't each
--   re-spell the CombatEvent construction. The Lua-side producer uses
--   `injury.emit`; both feed the same `injuryEventsRef` the injury-log
--   UI drains.
pushInjuryEvent
    ∷ IORef (Seq.Seq CombatEvent)  -- ^ injuryEventsRef
    → Double                       -- ^ game-time
    → Word32                       -- ^ victim uid
    → Text                         -- ^ kind: "fall" | "injure" | "death"
    → [(Text, Text)]               -- ^ payload (part / woundKind / severity / cause / detail)
    → IO ()
pushInjuryEvent ref ts victim kind payload =
    atomicModifyIORef' ref $ \buf →
        ( buf Seq.|> CombatEvent
            { ceTs       = ts
            , ceKind     = kind
            , ceAttacker = Nothing
            , ceTarget   = Just victim
            , cePayload  = HM.fromList payload
            }
        , () )
