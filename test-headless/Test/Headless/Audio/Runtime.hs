module Test.Headless.Audio.Runtime (spec) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Engine.Audio.Catalog.Types
import Engine.Audio.Catalog.Upload
import Engine.Audio.Config.Player
import Engine.Audio.Config.Runtime
import Engine.Audio.Native
import Engine.Audio.Runtime
import Engine.Audio.Status
import Engine.Audio.Transport
import Engine.Audio.Types
import Engine.Graphics.Camera (CameraFacing(..))
import World.Page.Types (WorldPageId(..))
import Test.Hspec

withRuntime ∷ (Native → AudioRuntime → IO ()) → IO ()
withRuntime action = withNative defaultNativeConfig ForcedNull (\native → do
  let timbre = Timbre Sine 440 False 1 0 0 0.2 10 Bypass 2000 0.7 0 10000
      policy = Policy UIBus False 1 16 1 0 50 1 DropNew 0 True 0 False
      sounds = [Sound "a" policy (SynthSource timbre), Sound "b" policy (SynthSource timbre),
        Sound "world" (policy { policyBus = WorldBus, policySpatial = True, policyFreeze = True })
          (SynthSource timbre)]
      catalog = Catalog Map.empty Map.empty (Map.fromList [(soundId sound, sound) | sound ← sounds]) [] Map.empty
  uploaded ← uploadCatalog native defaultRuntimeConfig catalog
  uploadWarnings uploaded `shouldBe` []
  action native (newAudioRuntime uploaded 0))
  ⌦ either (expectationFailure ∘ Text.unpack) pure

run ∷ Native → Word64 → [AudioRequest] → AudioRuntime → IO (AudioRuntime, AudioStatus)
run native epoch requests runtime = go (zipWith (\n → Stamped n epoch) [1..] requests) runtime
  where
    go pending current = do
      result ← serviceAudioRuntime native defaultRuntimeConfig epoch pending current
      case result of
        Left err → expectationFailure (Text.unpack err) >> fail "native service failed"
        Right (next, remaining, status)
          | null remaining → pure (next, runtimeStatus defaultVolumes
              emptyTransportStats status next)
          | otherwise → go remaining next

count ∷ AudioDropReason → AudioStatus → Word64
count reason = Map.findWithDefault 0 reason ∘ audioDrops

listener ∷ ListenerSnapshot
listener = ListenerSnapshot (WorldPageId "world") 0 0 0 FaceSouth 0.25 256 0 0.25 1.2 1.6

spec ∷ Spec
spec = describe "Audio.Runtime" $ do
  it "keeps playing sources on the nearest wrapped side when the camera crosses their antipode" $ withRuntime $ \native runtime → do
    let oldListener = listener { listenerWrapU = 16 }
        newListener = oldListener { listenerX = -0.4, listenerY = 0.4 }
        positioned = defaultTriggerOptions
          { triggerPosition = Just $ AudioPosition (WorldPageId "world") 3.9 (-3.9) 0 }
        energy channel pcm = sum [sample * sample | (index, sample) ← zip [0 ∷ Int ..] pcm, index `mod` 2 ≡ channel]
        render = renderOffline native 2400 ⌦ either (fail ∘ Text.unpack) pure
    (playing, _) ← run native 0 [AudioListener $ Just oldListener,
      AudioStartLoop "wrapped" "world" positioned] runtime
    before ← render
    energy 0 before `shouldBe` 0
    energy 1 before `shouldSatisfy` (> 1)
    void $ run native 0 [AudioListener $ Just newListener] playing
    after ← render
    energy 0 after `shouldSatisfy` (> 1)
    energy 1 after `shouldBe` 0

  it "does not publish a loop binding after native capacity rejection" $ withRuntime $ \native runtime → do
    (_, status) ← run native 0 [AudioPlay "a" defaultTriggerOptions, AudioStartLoop "rejected" "a" defaultTriggerOptions,
      AudioUpdateLoop "rejected" (LoopOptions Nothing $ Just (-3))] runtime
    count VoiceCapacity status `shouldBe` 1
    count MissingLoop status `shouldBe` 1

  it "preserves the original binding on conflicting starts and allows reuse after stop" $ withRuntime $ \native runtime → do
    (_, status) ← run native 0 [AudioStartLoop "same" "a" defaultTriggerOptions,
      AudioStartLoop "same" "b" defaultTriggerOptions, AudioStopLoop "same",
      AudioStartLoop "same" "b" defaultTriggerOptions, AudioUpdateLoop "same" (LoopOptions Nothing $ Just (-6))] runtime
    count LoopConflict status `shouldBe` 1
    count MissingLoop status `shouldBe` 0
    nsActiveVoices <$> audioNative status `shouldBe` Just 1

  it "clears world bindings on page changes while UI loops survive" $ withRuntime $ \native runtime → do
    let positioned = defaultTriggerOptions { triggerPosition = Just $ AudioPosition (WorldPageId "world") 0 0 0 }
    (_, status) ← run native 0 [AudioListener $ Just listener,
      AudioStartLoop "world_loop" "world" positioned, AudioStartLoop "ui_loop" "a" defaultTriggerOptions,
      AudioListener $ Just $ listener { listenerPage = WorldPageId "elsewhere" },
      AudioStopLoop "world_loop", AudioStopLoop "ui_loop"] runtime
    count MissingLoop status `shouldBe` 1
    nsActiveVoices <$> audioNative status `shouldBe` Just 0

  it "rejects unknown sounds, missing listeners, and positions on other pages" $ withRuntime $ \native runtime → do
    let wrong = defaultTriggerOptions { triggerPosition = Just $ AudioPosition (WorldPageId "other") 0 0 0 }
    (_, status) ← run native 0 [AudioPlay "missing" defaultTriggerOptions, AudioPlay "world" wrong,
      AudioListener $ Just listener, AudioPlay "world" wrong] runtime
    map (`count` status) [UnknownSound, NoListener, WrongPage] `shouldBe` [1, 1, 1]

  it "resets native and logical state before applying commands from a replacement session" $ withRuntime $ \native runtime → do
    (old, _) ← run native 0 [AudioStartLoop "old" "a" defaultTriggerOptions] runtime
    reset ← serviceAudioRuntime native defaultRuntimeConfig 1
      [Stamped 2 0 $ AudioPlay "a" defaultTriggerOptions, Stamped 3 1 $ AudioStopLoop "old"] old
    case reset of
      Left err → expectationFailure (Text.unpack err)
      Right (next, pending, nativeStatus) → do
        nsActiveVoices nativeStatus `shouldBe` 0
        length pending `shouldBe` 1
        (_, status) ← run native 1 (map stampedRequest pending) next
        audioSessionResets status `shouldBe` 1
        map (`count` status) [StaleSession, MissingLoop] `shouldBe` [1, 1]
