module Engine.Scripting.Lua.API.Audio
  ( playFn, startLoopFn, updateLoopFn, stopLoopFn, statusFn
  , savedVolumesFn, defaultVolumesFn, setVolumesFn, saveVolumesFn
  ) where

import UPrelude
import Data.Bifunctor (first)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Parser, parseEither)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified HsLua as Lua
import qualified Engine.Audio.API as Audio
import Engine.Audio.Config.Player
import Engine.Audio.Native (NativeStatus(..))
import Engine.Audio.Health
import Engine.Audio.Preview.Types
import Engine.Audio.Status
import Engine.Audio.Transport
import Engine.Audio.Types (validSoundId, validLoopId)
import Engine.Core.Capability.Audio
import Engine.Core.Capability.Core
import Engine.Core.Log (logWarn, LogCategory(..))
import Engine.Scripting.Lua.API.Audio.Args
import Engine.Scripting.Lua.API.Yaml (pushAeson)

readParsed ∷ Lua.StackIndex → (Value → Parser α) → Lua.LuaE Lua.Exception (Either Text α)
readParsed index parser = do
  value ← readValue 2 index
  pure $ value ⌦ first Text.pack ∘ parseEither parser

readIdentifier ∷ (Text → Bool) → Lua.StackIndex → Lua.LuaE Lua.Exception (Either Text Text)
readIdentifier valid index = do
  value ← readText index
  pure $ value ⌦ \name → if valid name then Right name else Left "invalid audio identifier"

reply ∷ CoreCapability → Int → Int → Either Text (IO Bool) → Lua.LuaE Lua.Exception Lua.NumResults
reply core low high requested = do
  count ← Lua.gettop
  let checked = if count < fromIntegral low ∨ count > fromIntegral high
        then Left "wrong number of audio arguments" else requested
  result ← case checked of
    Left err → do
      Lua.liftIO $ readIORef (ccLoggerRef core) ⌦ \logger → logWarn logger CatAudio (boundedAudioError err)
      pure False
    Right action → Lua.liftIO action
  Lua.pushboolean result
  pure 1

playFn ∷ CoreCapability → AudioCapability → Lua.LuaE Lua.Exception Lua.NumResults
playFn core capability = do
  sound ← readIdentifier validSoundId 1
  trigger ← readParsed 2 parseTrigger
  reply core 1 2 $ (\name args → (≡ Audio.Enqueued) <$> Audio.playSound capability name args) <$> sound <*> trigger

startLoopFn ∷ CoreCapability → AudioCapability → Lua.LuaE Lua.Exception Lua.NumResults
startLoopFn core capability = do
  loop ← readIdentifier validLoopId 1
  sound ← readIdentifier validSoundId 2
  trigger ← readParsed 3 parseTrigger
  reply core 2 3 $ (\key name args → (≡ Audio.Enqueued) <$> Audio.startSoundLoop capability key name args)
    <$> loop <*> sound <*> trigger

updateLoopFn ∷ CoreCapability → AudioCapability → Lua.LuaE Lua.Exception Lua.NumResults
updateLoopFn core capability = do
  loop ← readIdentifier validLoopId 1
  update ← readParsed 2 parseLoopUpdate
  reply core 2 2 $ (\key args → (≡ Audio.Enqueued) <$> Audio.updateSoundLoop capability key args) <$> loop <*> update

stopLoopFn ∷ CoreCapability → AudioCapability → Lua.LuaE Lua.Exception Lua.NumResults
stopLoopFn core capability = do
  loop ← readIdentifier validLoopId 1
  reply core 1 1 $ (\key → (≡ Audio.Enqueued) <$> Audio.stopSoundLoop capability key) <$> loop

volumesValue ∷ Volumes → Value
volumesValue volumes = object ["master" .= volumeMaster volumes, "world" .= volumeWorld volumes, "ui" .= volumeUI volumes]

savedVolumesFn, defaultVolumesFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
savedVolumesFn = Lua.liftIO loadSavedVolumes ⌦ pushAeson ∘ volumesValue >> pure 1
defaultVolumesFn = Lua.liftIO loadDefaultVolumes ⌦ pushAeson ∘ volumesValue >> pure 1

setVolumesFn, saveVolumesFn ∷ CoreCapability → AudioCapability → Lua.LuaE Lua.Exception Lua.NumResults
setVolumesFn core capability = do
  parsed ← readParsed 1 parseVolumes
  reply core 1 1 $ (\volumes → True <$ Audio.setVolumes capability volumes) <$> parsed
saveVolumesFn core capability = do
  parsed ← readParsed 1 parseVolumes
  reply core 1 1 $ (\volumes → do
    result ← Audio.saveVolumes capability volumes
    case result of
      Right _ → pure True
      Left err → readIORef (ccLoggerRef core) ⌦ \logger → logWarn logger CatAudio err >> pure False) <$> parsed

statusFn ∷ AudioCapability → Lua.LuaE Lua.Exception Lua.NumResults
statusFn capability = do
  status ← Lua.liftIO $ Audio.readAudioStatus capability
  volumes ← Lua.liftIO $ Audio.currentVolumes capability
  let stats = audioTransportStats status
      health = audioHealth status
      lifecycle = case audioLifecycle status of
        AudioStarting → "starting"; AudioRunningNull → "running_null"; AudioRunningReal → "running_real"
        AudioDegradedNull → "degraded_null"; AudioStopped → "stopped"; AudioDisabled → "disabled"
  pushAeson $ object ["lifecycle" .= (lifecycle ∷ Text), "volumes" .= volumesValue volumes,
    "previewRevision" .= audioPreviewRevision status,
    "previewEntries" .= [object ["id" .= paeId entry, "label" .= paeLabel entry,
      "category" .= paeCategory entry, "path" .= paePath entry, "playable" .= paePlayable entry]
      | entry ← audioPreviewEntries status],
    "epoch" .= audioEpoch status, "sessionResets" .= audioSessionResets status,
    "catalog" .= Map.map (\(valid, disabled) → object ["valid" .= valid, "disabled" .= disabled]) (audioCatalogEntries status),
    "perSoundSteals" .= audioPerSoundSteals status, "globalSteals" .= audioGlobalSteals status,
    "catalogSounds" .= audioCatalogSounds status, "catalogWarnings" .= audioCatalogWarnings status,
    "lastError" .= audioLastError status,
    "snapshotSequence" .= audioSnapshotSequence status, "publishedNs" .= audioPublishedNs status,
    "diagnosticWarnings" .= audioDiagnosticWarnings status,
    "health" .= object ["degraded" .= healthDegraded health, "recentUnderruns" .= healthRecentUnderruns health,
      "serviceFrames" .= healthServiceFrames health, "budgetViolations" .= healthBudgetViolations health,
      "budgetExceeded" .= healthBudgetExceeded health, "controlBacklog" .= healthControlBacklog health],
    "drops" .= Map.fromList [(tshow reason, count) | (reason, count) ← Map.toList $ audioDrops status],
    "transport" .= object ["queued" .= transportQueued stats,
      "eventPeak" .= transportEventPeak stats, "controlPeak" .= transportControlPeak stats, "queueDrops" .= transportQueueDrops stats,
      "disabledDrops" .= transportDisabledDrops stats, "invalidDrops" .= transportInvalidDrops stats,
      "staleDrops" .= transportStaleDrops stats, "eventDepth" .= transportEventDepth stats,
      "controlDepth" .= transportControlDepth stats, "loopCoalesced" .= transportLoopCoalesced stats,
      "listenerCoalesced" .= transportListenerCoalesced stats, "volumesCoalesced" .= transportVolumesCoalesced stats],
    "native" .= fmap nativeValue (audioNative status)]
  pure 1

nativeValue ∷ NativeStatus → Value
nativeValue native = object
  ["sink" .= (if nsSink native ≡ 0 then "null" else "real" ∷ Text), "backend" .= nsBackend native,
   "deviceName" .= nsDeviceName native, "sampleRate" .= nsSampleRate native, "periodFrames" .= nsPeriodFrames native,
   "ringFill" .= nsRingFill native, "ringMin" .= nsRingMin native, "ringMax" .= nsRingMax native,
   "activeVoices" .= nsActiveVoices native, "peakVoices" .= nsPeakVoices native, "activeLoops" .= nsActiveLoops native, "peakLoops" .= nsPeakLoops native,
   "samples" .= nsSampleCount native, "instruments" .= nsInstrumentCount native, "sounds" .= nsSoundCount native,
   "renderedFrames" .= nsRenderedFrames native, "callbackFrames" .= nsCallbackFrames native,
   "callbacks" .= nsCallbacks native, "underruns" .= nsUnderruns native, "accepted" .= nsAccepted native,
   "dropped" .= nsDropped native, "steals" .= nsSteals native, "missingLoops" .= nsMissingLoops native,
   "decodedFrames" .= nsDecodedFrames native, "decodedBytes" .= nsDecodedBytes native,
   "limitedSamples" .= nsLimitedSamples native, "nonfiniteSamples" .= nsNonfiniteSamples native,
   "transitions" .= nsTransitions native, "serviceNs" .= nsServiceNs native, "mixPeak" .= nsMixPeak native]
