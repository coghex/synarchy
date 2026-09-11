-- | Bounded worker-local health and diagnostic suppression. Time is supplied
-- explicitly so rolling-window and recovery behavior can be tested without sleeps.
module Engine.Audio.Health
  ( AudioHealth(..), Health, newHealth, observeHealth, healthWarnings
  , DiagnosticLimiter, newDiagnosticLimiter, limitDiagnostic, diagnosticKeyCount
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Engine.Audio.Config.Runtime
import Engine.Audio.Native
import Engine.Audio.Transport

data AudioHealth = AudioHealth
  { healthDegraded ∷ Bool, healthRecentUnderruns ∷ Word64
  , healthServiceFrames ∷ Word64, healthBudgetViolations ∷ Word64
  , healthBudgetExceeded ∷ Bool, healthControlBacklog ∷ Bool
  } deriving (Eq, Show)

data Health = Health
  { hUnderruns ∷ Word64, hFrames ∷ Word64
  , hBuckets ∷ Map.Map Word64 Word64, hSnapshot ∷ AudioHealth }

newHealth ∷ NativeStatus → Health
newHealth native = Health (nsUnderruns native) (nsRenderedFrames native) Map.empty
  (AudioHealth False 0 0 0 False False)

-- The trailing minute uses 61 one-second buckets. A bucket expires when its
-- newest possible observation is a minute old: at most one second conservative,
-- and constant memory even if every callback underruns for hours.
observeHealth ∷ RuntimeConfig → Word64 → TransportStats → NativeStatus → Health
  → (Health, AudioHealth)
observeHealth config now stats native previous =
  let second = now `div` 1000000000
      delta = nsUnderruns native - min (nsUnderruns native) (hUnderruns previous)
      retained = Map.filterWithKey (\key _ → second - min second key ≤ 60) (hBuckets previous)
      buckets = if delta ≡ 0 then retained else Map.insertWith (+) second delta retained
      recent = sum (Map.elems buckets)
      frames = nsRenderedFrames native - min (nsRenderedFrames native) (hFrames previous)
      budget = 1e9 * fromIntegral frames / fromIntegral (ncSampleRate $ rcNative config)
        * realToFrac (rcServiceBudgetFractionWarn config) ∷ Double
      violation = frames > 0 ∧ fromIntegral (nsServiceNs native) > budget
      old = hSnapshot previous
      busy = if frames ≡ 0 then healthBudgetExceeded old else violation
      backlog = transportControlDepth stats > rcControlBacklogWarn config
      degraded = recent ≥ fromIntegral (rcUnderrunsPerMinuteWarn config) ∨ busy ∨ backlog
      snapshot = AudioHealth degraded recent frames
        (healthBudgetViolations old + if violation then 1 else 0) busy backlog
  in (Health (nsUnderruns native) (nsRenderedFrames native) buckets snapshot, snapshot)

healthWarnings ∷ RuntimeConfig → AudioHealth → [Text]
healthWarnings config status = ["control backlog" | healthControlBacklog status]
  <> ["audio service budget exceeded" | healthBudgetExceeded status]
  <> ["output underruns" | healthRecentUnderruns status ≥ fromIntegral (rcUnderrunsPerMinuteWarn config)]

data Diagnostic = Diagnostic
  { diagnosticEmittedAt ∷ Word64, diagnosticSeenAt ∷ Word64, diagnosticSuppressed ∷ Word64 }
newtype DiagnosticLimiter = DiagnosticLimiter (Map.Map (Text, Text) Diagnostic)

newDiagnosticLimiter ∷ DiagnosticLimiter
newDiagnosticLimiter = DiagnosticLimiter Map.empty

diagnosticKeyCount ∷ DiagnosticLimiter → Int
diagnosticKeyCount (DiagnosticLimiter entries) = Map.size entries

-- A fixed 256-key least-recently-seen cache prevents arbitrary Lua IDs growing
-- memory. Keys/text are bounded, and each emitted recurrence carries its count.
limitDiagnostic ∷ Word64 → Word64 → (Text, Text) → Text → DiagnosticLimiter
  → (DiagnosticLimiter, Maybe Text)
limitDiagnostic now interval (reason, name) message (DiagnosticLimiter entries) =
  let key = (Text.take 64 reason, Text.take 128 name)
      prior = Map.lookup key entries
      emit = maybe True (\old → now - min now (diagnosticEmittedAt old) ≥ interval) prior
      suppressed = maybe 0 diagnosticSuppressed prior
      next = Diagnostic (if emit then now else maybe now diagnosticEmittedAt prior) now
        (if emit then 0 else suppressed + 1)
      room = if Map.size entries < 256 ∨ Map.member key entries then entries
        else case Map.toList entries of
          [] → entries
          first:rest → let oldest = foldl' (\a b → if diagnosticSeenAt (snd b) < diagnosticSeenAt (snd a)
                            then b else a) first rest
                        in Map.delete (fst oldest) entries
      suffix = if suppressed ≡ 0 then "" else " (" <> tshow suppressed <> " suppressed)"
  in (DiagnosticLimiter $ Map.insert key next room,
      if emit then Just (Text.take 440 message <> suffix) else Nothing)
