module Test.Headless.Audio.Native (spec) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.Either (isLeft)
import Engine.Audio.Catalog.Resolve (loadCatalog)
import Engine.Audio.Catalog.Upload
import Engine.Audio.Config.Runtime (defaultRuntimeConfig)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Engine.Audio.Native
import System.Timeout (timeout)
import Test.Hspec

withCore ∷ (Native → IO ()) → IO ()
withCore action = withNative defaultNativeConfig ForcedNull action
  ⌦ either (expectationFailure ∘ Text.unpack) pure

spec ∷ Spec
spec = describe "Audio.Native — C-owned forced-null boundary" $ do
  it "pins the cross-language POD sizes and round-trips every configuration field" $ do
    nativeAbiSizes `shouldBe` (68, 568)
    alignment defaultNativeConfig `shouldBe` 4
    let config = NativeConfig 44100 128 512 2048 64 32 128 3 1200 9000 15 12 3 8 0.8
    alloca $ \ptr → do
      poke ptr config
      peek ptr `shouldReturn` config

  it "refuses an invalid ABI configuration without constructing a core" $ do
    result ← withNative (defaultNativeConfig { ncSampleRate = 0 }) ForcedNull (const $ pure ())
    result `shouldSatisfy` isLeft

  it "does not attach an earlier file error to an unrelated validation failure" $ withCore $ \core → do
    loadSampleNative core "test-headless/data/audio/absent.wav" (DecodeLimits 1024 1024 8192)
      ≫= (`shouldSatisfy` isLeft)
    let invalid = NativeInstrument 0 1 0 0 0 0 0 0 1 0 1000 0.7 0 100
    addInstrumentNative core invalid `shouldReturn` Left "audio error 1: invalid argument or ABI layout"

  it "round-trips catalog layouts and every closed command without losing payload fields" $ do
    catalogAbiSizes `shouldBe` [(32, 8), (64, 4), (80, 4)]
    commandAbiSizes `shouldBe` [(128, 8), (24, 8)]
    let roundTrip value = alloca $ \ptr → poke ptr value >> (peek ptr `shouldReturn` value)
    roundTrip $ DecodeLimits 16777216 720000 67108864
    roundTrip $ NativeInstrument 4 1234567 3 1 442 0.75 2 10 0.4 200 620 2.8 (-6) 55
    roundTrip $ NativeSound 1 23 0 1 87 12 1 0.25 1 1 4294967295 2 80 1.5 (-3) 70 25
    roundTrip $ CommandResult 13 4294967305 2
    mapM_ roundTrip
      [ PlayNative 37 (1, -2, 3) (-12) 7
      , StartLoopNative 4294967305 29 (-4, 5, 6) 3 (-9)
      , UpdateLoopNative 11 (Just (4, 3, 2)) (Just (-1))
      , UpdateLoopNative 12 Nothing Nothing, UpdateLoopNative 13 (Just (1,2,3)) Nothing
      , UpdateLoopNative 14 Nothing (Just 2), StopLoopNative 4294967305
      , RebaseNative ((0,-1,0,12), (1,0,0,3), (0,0,1,-7))
      , WrapFrameNative (128,0,0) (0,256,0)
      , VolumesNative 0.25 0.5 0.75, WorldMixNative 1.5 0.9
      , PlayerPauseNative True, PlayerPauseNative False, ResetSessionNative, ClearWorldNative ]

  it "renders the shipped menu synth catalog through the Haskell-to-C command boundary" $ withCore $ \core → do
    catalog ← loadCatalog defaultRuntimeConfig
    uploaded ← uploadCatalog core defaultRuntimeConfig catalog
    uploadWarnings uploaded `shouldBe` []
    Map.keys (uploadedSounds uploaded) `shouldBe` ["menu_back", "menu_selected"]
    forM_ (Map.elems $ uploadedSounds uploaded) $ \(handle, _) → do
      submitOffline core [PlayNative handle (0, 0, 0) 0 0] `shouldReturn` Right [CommandResult 0 0 0]
      rendered ← renderOffline core 6000
      case rendered of
        Left err → expectationFailure (Text.unpack err)
        Right pcm → do
          maximum (map abs pcm) `shouldSatisfy` (> 0.01)
          all (\x → not (isNaN x ∨ isInfinite x) ∧ abs x ≤ 1) pcm `shouldBe` True
          drop 11000 pcm `shouldBe` replicate 1000 0
      nsActiveVoices <$> readNativeStatus core `shouldReturn` 0

  it "constructs a stopped null device and reports actual native fields" $ withCore $ \core → do
    status ← readNativeStatus core
    nsSink status `shouldBe` 0
    nsSampleRate status `shouldBe` 48000
    nsCallbacks status `shouldBe` 0
    nsRenderedFrames status `shouldBe` 0

  it "renders exact stereo silence through the real offline C entry point" $ withCore $ \core → do
    renderOffline core 257 `shouldReturn` Right (replicate 514 0)
    status ← readNativeStatus core
    nsRenderedFrames status `shouldBe` 257
    nsCallbackFrames status `shouldBe` 0
    renderOffline core (-1) ≫= (`shouldSatisfy` isLeft)

  it "bounds a refill to the target rather than filling the ring" $ withCore $ \core → do
    result ← serviceNative core
    case result of
      Left err → expectationFailure (Text.unpack err)
      Right status → do
        nsRingFill status `shouldBe` 1024
        nsRingMax status `shouldBe` 1024
        nsRenderedFrames status `shouldBe` 1024

  it "runs the real null callback and joins it on repeated stop" $ withCore $ \core → do
    startNative core `shouldReturn` Right ()
    startNative core `shouldReturn` Right ()
    let wait = do
          status ← readNativeStatus core
          if nsCallbacks status > 1 then pure ()
          else serviceNative core >> threadDelay 2000 >> wait
    timeout 1000000 wait `shouldReturn` Just ()
    renderOffline core 1 ≫= (`shouldSatisfy` isLeft)
    stopNative core
    stopNative core
    before ← readNativeStatus core
    threadDelay 20000
    after ← readNativeStatus core
    nsCallbacks after `shouldBe` nsCallbacks before
    nsLifecycle after `shouldBe` 4
