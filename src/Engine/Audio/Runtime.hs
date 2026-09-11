-- | Worker-local semantic state and native-result reconciliation. A step makes
-- one bounded C call. Loop lifetimes and listener changes form step boundaries
-- so later commands observe the actual result, including rejection or eviction.
module Engine.Audio.Runtime
  ( AudioRuntime, newAudioRuntime, serviceAudioRuntime, runtimeStatus, runtimeDiagnostics ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Audio.Catalog.Types
import Engine.Audio.Catalog.Upload
import Engine.Audio.Config.Player
import Engine.Audio.Config.Runtime
import Engine.Audio.Native
import Engine.Audio.Spatial
import Engine.Audio.Status
import Engine.Audio.Transport
import Engine.Audio.Types

-- A binding never enters EngineEnv, telemetry, or a save.
data LoopBinding = LoopBinding
  { bindingKey ∷ Word64, bindingSound ∷ Text, bindingBus ∷ Bus }

data AudioRuntime = AudioRuntime
  { rtCatalog ∷ UploadedCatalog, rtListener ∷ Maybe ListenerSnapshot
  , rtLoops ∷ Map.Map Text LoopBinding, rtNextLoop ∷ Word64
  , rtEpoch ∷ Word64, rtDrops ∷ Map.Map AudioDropReason Word64
  , rtPerSoundSteals ∷ Word64, rtGlobalSteals ∷ Word64
  , rtResets ∷ Word64, rtError ∷ Text, rtDiagnostics ∷ [(AudioDropReason, Text)]
  }

data Effect = NoEffect Text | BindLoop Text LoopBinding | ReleaseLoop Text

newAudioRuntime ∷ UploadedCatalog → Word64 → AudioRuntime
newAudioRuntime catalog epoch = AudioRuntime catalog Nothing Map.empty 1 epoch Map.empty 0 0 0
  (boundedAudioError $ fromMaybe "" $ listToMaybe $ reverse $ uploadWarnings catalog) []

runtimeDiagnostics ∷ AudioRuntime → [(AudioDropReason, Text)]
runtimeDiagnostics = reverse ∘ rtDiagnostics

recordDrop ∷ AudioDropReason → Text → Text → AudioRuntime → AudioRuntime
recordDrop reason key message runtime = runtime
  { rtDrops = Map.insertWith (+) reason 1 (rtDrops runtime), rtError = boundedAudioError message
  , rtDiagnostics = (reason, key) : rtDiagnostics runtime }

-- | At most one structural control per step. This avoids speculating that a
-- failed start succeeded, or reusing a key that an earlier trigger just evicted.
-- Runs of one-shots/updates/volumes/pause still share a single native batch.
stepRequests ∷ [Stamped] → ([Stamped], [Stamped])
stepRequests [] = ([], [])
stepRequests (first:rest)
  | structural first = ([first], rest)
  | otherwise = let (prefix, remaining) = span (not ∘ structural) rest in (first:prefix, remaining)
  where
    structural item = case stampedRequest item of
      AudioStartLoop {} → True
      AudioStopLoop {} → True
      AudioListener {} → True
      AudioResetSession → True
      _ → False

serviceAudioRuntime ∷ Native → RuntimeConfig → Word64 → [Stamped] → AudioRuntime
  → IO (Either Text (AudioRuntime, [Stamped], NativeStatus))
serviceAudioRuntime native config epoch pending runtime = do
  let current item = case stampedRequest item of
        AudioVolumes _ → True
        AudioReloadPreview → True
        _ → stampedEpoch item ≡ epoch
      fresh = filter current pending
      stale = length pending - length fresh
      cleared = runtime { rtDiagnostics = [] }
      counted = if stale ≡ 0 then cleared else cleared
        { rtDrops = Map.insertWith (+) StaleSession (fromIntegral stale) (rtDrops runtime) }
      reset = epoch ≢ rtEpoch counted
      (input, remaining) = if reset then ([], fresh) else stepRequests fresh
      seed = if reset then counted { rtEpoch = epoch, rtLoops = Map.empty,
        rtListener = Nothing, rtResets = rtResets counted + 1 } else counted
      (prepared, commands, effects) = if reset
        then (seed, [ResetSessionNative, WorldMixNative 1 0], [NoEffect "", NoEffect ""])
        else foldl' (prepare config) (seed, [], []) input
  result ← serviceCommandsNative native commands
  pure $ case result of
    Left err → Left err
    Right (results, status) → Right (foldl' reconcile prepared (zip effects results), remaining, status)

prepare ∷ RuntimeConfig → (AudioRuntime, [NativeCommand], [Effect]) → Stamped
  → (AudioRuntime, [NativeCommand], [Effect])
prepare config (runtime, commands, effects) item =
  let append state command effect = (state, commands <> [command], effects <> [effect])
      key = case stampedRequest item of
        AudioPlay name _ → name; AudioStartLoop loop _ _ → loop
        AudioUpdateLoop loop _ → loop; AudioStopLoop loop → loop
        _ → ""
      reject reason message = (recordDrop reason key message runtime, commands, effects)
      lookupSound name = Map.lookup name (uploadedSounds $ rtCatalog runtime)
      positionFor sound position
        | not (policySpatial $ soundPolicy sound) = Right (0, 0, 0)
        | Nothing ← position = Left InvalidRequest
        | Nothing ← rtListener runtime = Left NoListener
        | Just listener ← rtListener runtime, Just source ← position =
            maybe (Left WrongPage) Right (relativePosition listener source)
      start loop name options = case lookupSound name of
        Nothing → reject UnknownSound ("unknown sound " <> name)
        Just (handle, sound) → case positionFor sound (triggerPosition options) of
          Left reason → reject reason ("cannot position sound " <> name)
          Right xyz → case loop of
            Nothing → append runtime (PlayNative handle xyz (triggerGainDb options) (triggerPitch options)) (NoEffect key)
            Just logical
              | not (policyAllowLoop $ soundPolicy sound) → reject InvalidRequest ("sound does not permit loops: " <> name)
              | otherwise →
                  let existing = Map.lookup logical (rtLoops runtime)
                      key = maybe (rtNextLoop runtime) bindingKey existing
                      next = if isJust existing then runtime else runtime { rtNextLoop = key + 1 }
                      binding = LoopBinding key name (policyBus $ soundPolicy sound)
                  in append next (StartLoopNative key handle xyz (triggerGainDb options) (triggerPitch options))
                       (BindLoop logical binding)
  in case stampedRequest item of
    AudioPlay name options → start Nothing name options
    AudioStartLoop loop name options → start (Just loop) name options
    AudioUpdateLoop loop options → case Map.lookup loop (rtLoops runtime) of
      Nothing → reject MissingLoop ("cannot update missing loop " <> loop)
      Just binding → case lookupSound (bindingSound binding) of
        Nothing → reject UnknownSound ("missing loop sound " <> bindingSound binding)
        Just (_, sound) →
          let located = case loopPosition options of
                Nothing → Right Nothing
                Just position → Just <$> positionFor sound (Just position)
          in case located of
            Left reason → reject reason ("cannot position loop " <> loop)
            Right xyz → append runtime (UpdateLoopNative (bindingKey binding) xyz (loopGainDb options)) (NoEffect key)
    AudioStopLoop loop → case Map.lookup loop (rtLoops runtime) of
      Nothing → reject MissingLoop ("cannot stop missing loop " <> loop)
      Just binding → append runtime (StopLoopNative $ bindingKey binding) (ReleaseLoop loop)
    AudioSetPlayerPaused paused → append runtime (PlayerPauseNative paused) (NoEffect "")
    AudioVolumes volumes →
      let gain = volumeGain (rcVolumeExponent config)
      in append runtime (VolumesNative (gain $ volumeMaster volumes) (gain $ volumeWorld volumes)
          (gain $ volumeUI volumes)) (NoEffect key)
    AudioListener snapshot →
      let valid = snapshot ⌦ \listener → if validListener listener then Just listener else Nothing
          rebase = do old ← rtListener runtime; new ← valid; listenerRebase old new
          clear = runtime { rtLoops = Map.filter ((≢ WorldBus) ∘ bindingBus) (rtLoops runtime) }
          next = (if isNothing rebase then clear else runtime) { rtListener = valid }
          (range, gain) = maybe (1, 0) (zoomTargets config) valid
          transform = maybe ClearWorldNative RebaseNative rebase
          wrapping = case (rebase, valid) of
            (Just _, Just listener) → let (u, v) = listenerWrapFrame listener in [WrapFrameNative u v]
            _ → []
          changes = [transform] <> wrapping <> [WorldMixNative range gain]
      in if valid ≡ rtListener runtime then (runtime, commands, effects)
         else (next, commands <> changes, effects <> replicate (length changes) (NoEffect ""))
    AudioResetSession → (runtime, commands, effects) -- Epoch was handled before every step.
    AudioShutdownWake → (runtime, commands, effects)
    AudioReloadPreview → (runtime, commands, effects) -- Preview worker owns the rebuild.

reconcile ∷ AudioRuntime → (Effect, CommandResult) → AudioRuntime
reconcile runtime (effect, result) =
  let evicted = if crEvictedLoop result ≡ 0 then runtime else runtime
        { rtLoops = Map.filter ((≢ crEvictedLoop result) ∘ bindingKey) (rtLoops runtime) }
      stolen = evicted
        { rtPerSoundSteals = rtPerSoundSteals evicted + if crStealReason result ≡ 1 then 1 else 0
        , rtGlobalSteals = rtGlobalSteals evicted + if crStealReason result ≡ 2 then 1 else 0 }
      key = case effect of NoEffect name → name; BindLoop name _ → name; ReleaseLoop name → name
      reason = case crResult result of
        7 → MissingLoop
        9 → VoiceCooldown
        10 → VoiceCapacity
        11 → PausedWorld
        12 → LoopConflict
        13 → VoicePriority
        _ → InvalidRequest
      changed = if crResult result ≡ 0 then stolen
        else recordDrop reason key ("native command rejected: " <> tshow reason) stolen
  in case effect of
    BindLoop name binding | crResult result ≡ 0 → changed { rtLoops = Map.insert name binding (rtLoops changed) }
    ReleaseLoop name → changed { rtLoops = Map.delete name (rtLoops changed) }
    _ → changed

runtimeStatus ∷ Volumes → TransportStats → NativeStatus → AudioRuntime → AudioStatus
runtimeStatus volumes transport native runtime = (initialAudioStatus volumes)
  { audioLifecycle = nativeLifecycle (nsLifecycle native), audioNative = Just native
  , audioTransportStats = transport, audioDrops = rtDrops runtime, audioVolumes = volumes
  , audioEpoch = rtEpoch runtime, audioCatalogSounds = Map.size $ uploadedSounds $ rtCatalog runtime
  , audioCatalogWarnings = length $ uploadWarnings $ rtCatalog runtime
  , audioSessionResets = rtResets runtime, audioLastError = rtError runtime
  , audioPerSoundSteals = rtPerSoundSteals runtime, audioGlobalSteals = rtGlobalSteals runtime
  , audioCatalogEntries = uploadedCounts $ rtCatalog runtime
  , audioPreviewEntries = uploadedPreview $ rtCatalog runtime
  , audioPreviewRevision = uploadedRevision $ rtCatalog runtime }
