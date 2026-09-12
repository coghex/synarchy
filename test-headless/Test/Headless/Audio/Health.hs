module Test.Headless.Audio.Health (spec) where

import UPrelude
import qualified Data.Text as Text
import Engine.Audio.Config.Runtime
import Engine.Audio.Health
import Engine.Audio.Native
import Engine.Audio.Transport
import Test.Hspec

withStatus ∷ (NativeStatus → IO ()) → IO ()
withStatus action = withNative defaultNativeConfig ForcedNull (\native → readNativeStatus native ⌦ action)
  ⌦ either (expectationFailure ∘ Text.unpack) pure

spec ∷ Spec
spec = describe "Audio.Health" $ do
  it "keeps a trailing minute across minute boundaries and clears recovered health" $ withStatus $ \base → do
    let config = defaultRuntimeConfig
        step sec count = observeHealth config (sec * 1000000000) emptyTransportStats
          (base { nsUnderruns = count })
        (first, _) = step 59 2 (newHealth base)
        (second, degraded) = step 61 3 first
        (third, recovered) = step 120 3 second
        (_, expired) = step 122 3 third
    healthRecentUnderruns degraded `shouldBe` 3
    healthDegraded degraded `shouldBe` True
    healthRecentUnderruns recovered `shouldBe` 1
    healthDegraded recovered `shouldBe` False
    healthRecentUnderruns expired `shouldBe` 0

  it "measures the audio actually rendered and retains lifetime budget violations" $ withStatus $ \base → do
    let config = defaultRuntimeConfig
        step frames duration = observeHealth config 0 emptyTransportStats
          (base { nsRenderedFrames = frames, nsServiceNs = duration })
        (idle, a) = step 0 999999999 (newHealth base)
        (busy, b) = step 480 6000000 idle -- 6 ms exceeds half of 10 ms audio.
        (quiet, c) = step 480 0 busy
        (_, d) = step 960 4000000 quiet
    healthBudgetViolations a `shouldBe` 0
    healthBudgetExceeded b `shouldBe` True
    healthBudgetExceeded c `shouldBe` True
    healthDegraded d `shouldBe` False
    healthBudgetViolations d `shouldBe` 1
    healthServiceFrames d `shouldBe` 480

  it "detects control backlog independently and recovers without disabling output" $ withStatus $ \base → do
    let config = defaultRuntimeConfig
        (busy, status) = observeHealth config 0
          (emptyTransportStats { transportControlDepth = 65 }) base (newHealth base)
        (_, cleared) = observeHealth config 1 emptyTransportStats base busy
    healthWarnings config status `shouldBe` ["control backlog"]
    healthDegraded cleared `shouldBe` False

  it "warns immediately per reason and ID, then reports the suppressed count" $ do
    let warn now key = limitDiagnostic now 10 key "missing sound"
        (a, first) = warn 0 ("unknown", "bear") newDiagnosticLimiter
        (b, second) = warn 1 ("unknown", "bear") a
        (c, other) = warn 2 ("unknown", "wolf") b
        (d, different) = warn 3 ("capacity", "bear") c
        (_, resumed) = warn 10 ("unknown", "bear") d
    first `shouldBe` Just "missing sound"
    second `shouldBe` Nothing
    other `shouldBe` Just "missing sound"
    different `shouldBe` Just "missing sound"
    resumed `shouldBe` Just "missing sound (1 suppressed)"

  it "bounds diagnostic key retention and keeps recently used IDs" $ do
    let insert cache n = fst $ limitDiagnostic n 1000 ("unknown", tshow n) "missing" cache
        full = foldl' insert newDiagnosticLimiter [0..255]
        touched = fst $ limitDiagnostic 256 1000 ("unknown", "0") "missing" full
        grown = foldl' insert touched [257..400]
        (_, retained) = limitDiagnostic 401 1000 ("unknown", "0") "missing" grown
        (_, evicted) = limitDiagnostic 401 1000 ("unknown", "1") "missing" grown
    diagnosticKeyCount grown `shouldBe` 256
    retained `shouldBe` Nothing
    evicted `shouldBe` Just "missing"
