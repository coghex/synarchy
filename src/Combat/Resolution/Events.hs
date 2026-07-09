{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Combat event construction + the engine's event ring / unit-kill
--   plumbing. Split (issue #550) out of "Combat.Resolution"; see that
--   module's haddock for the overall resolution flow that pushes these.
module Combat.Resolution.Events
    ( missEvent
    , hitEvent
    , deathEvent
    , pushEvent
    , setDead
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import Data.IORef (atomicModifyIORef')
import Combat.Types (CombatEvent(..), AttackMode(..), attackModeText)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Unit.Types (UnitId(..))
import Unit.Command.Types (UnitCommand(..))

missEvent ∷ Double → Word32 → Word32 → AttackMode → Bool → Bool → CombatEvent
missEvent gt atk tgt mode isLunge isDodge = CombatEvent
    { ceTs       = gt
    , ceKind     = "miss"
    , ceAttacker = Just atk
    , ceTarget   = Just tgt
    , cePayload  = HM.fromList $
        [ ("mode", attackModeText mode) ]
        <> [ ("lunge", "1") | isLunge ]   -- a missed lunge → "lunges but…"
        <> [ ("dodge", "1") | isDodge ]   -- defender evaded → "X dodges…"
    }

hitEvent
    ∷ Double → Word32 → Word32 → Text → Text
    → Float → Float → Float → AttackMode
    → Text → Text → Text → Bool → CombatEvent
hitEvent gt atk tgt part kind sev rawDmg effDmg mode limb weapon detail isLunge = CombatEvent
    { ceTs       = gt
    , ceKind     = "hit"
    , ceAttacker = Just atk
    , ceTarget   = Just tgt
    , cePayload  = HM.fromList $
        [ ("part",     part)         -- macro-part id
        , ("limb",     limb)         -- macro-part display name ("left arm")
        , ("kind",     kind)         -- mechanism (slash/stab/blunt)
        , ("weapon",   weapon)       -- weapon display name / natural facet
        , ("severity", T.pack (show sev))
        , ("detail",   detail)       -- per-layer "subpart:layer:material:sevPct|…"
        , ("raw",      T.pack (show rawDmg))
        , ("eff",      T.pack (show effDmg))
        , ("mode",     attackModeText mode)
        ]
        <> [ ("lunge", "1") | isLunge ]  -- the combat log opens with a lunge line
    }

deathEvent
    ∷ Double → Word32 → Word32 → Text → Text → Text → CombatEvent
deathEvent gt atk tgt cause part kind = CombatEvent
    { ceTs       = gt
    , ceKind     = "death"
    , ceAttacker = Just atk
    , ceTarget   = Just tgt
    , cePayload  = HM.fromList
        [ ("cause", cause)
        , ("part",  part)
        , ("kind",  kind)
        ]
    }

pushEvent ∷ EngineEnv → CombatEvent → IO ()
pushEvent env ev =
    atomicModifyIORef' (combatEventsRef env) $ \buf →
        (buf Seq.|> ev, ())

-- | Set a unit to Dead via the engine's UnitKill command path
--   (uiPose is a mirror of sim usPose — writing it directly is
--   overwritten by publishToRender each frame). UnitKill snaps the
--   sim-side pose and clears in-flight state.
setDead ∷ EngineEnv → Word32 → IO ()
setDead env tgtRaw =
    Q.writeQueue (unitQueue env) (UnitKill (UnitId tgtRaw))
