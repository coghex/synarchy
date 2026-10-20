{-# LANGUAGE Strict #-}
-- | A captured arena survives every route back (#2714): an ordinary save of
--   a loaded scenario session restores WITHOUT the source file and captures
--   to the same document the live arena does, and the captured file loads
--   again as the same session (D-52: ordinary-save integration, no second
--   session-save system).
--
--   The fresh-process leg — a new engine process loading the captured file
--   and recapturing it — is evidence of a different kind (a real process
--   boundary) and is driven by @tools/scenario_capture_probe.py@, whose
--   exact invocation and retained comparison output are committed under
--   @docs/pr-proofs/@.
module Test.Headless.Scenario.Capture.Cycle (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import Scenario.Capture.Inputs (SessionParts(..), readSessionInputs)
import Scenario.Capture.Service (buildDocument)
import Scenario.Load.Catalog (liveScenarioCatalog)
import Test.Headless.Scenario.Capture.Support
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Load.Persist (integratedDoc)
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Test.Headless.Scenario.Placement.Glue (CaptureSources(..), saveAndStage)
import Scenario.Load.Session (arenaPageId)
import Scenario.Capture.Types (FailureKind)

spec ∷ SpecWith H
spec = describe "every route back" $ do

    it "an ordinary save restores the arena without the source file, and captures to the same document as the live arena" $ \h → do
        path ← writeScenario "capture-cycle-source.yaml" integratedDoc
        reloadFrom h path
        (_, live, _) ← captureLive h "cycle-live"
        ws ← liveArena h
        -- an ordinary save of the session, restored through the real codecs and
        -- the real load stage: no scenario document is involved at any point
        src ← saveAndStage (hFx h) arenaPageId ws
        inputs ← readSessionInputs (fxEnv (hFx h)) SessionParts
            { spPage = csPage src, spWorld = csWorld src, spUnits = csUnits src
            , spBuildings = csBuildings src, spItemRefs = csItemRefs src
            , spItemTags = csItemTags src, spPalette = csPalette src
            , spLiveCamera = Nothing }
        catalog ← liveScenarioCatalog (fxEnv (hFx h))
        case buildDocument catalog inputs of
            Left (kind, why, _) → expectationFailure ("the restored arena could not be captured: " ⧺ show (kind ∷ FailureKind) ⧺ " " ⧺ show why)
            Right (restored, _) → restored `shouldBe` live

    it "loads a captured file back as the same session, over and over" $ \h → do
        path ← writeScenario "capture-cycle-again.yaml" integratedDoc
        reloadFrom h path
        (dest1, bytes1, _) ← captureLive h "cycle-1"
        reloadFrom h dest1
        (dest2, bytes2, _) ← captureLive h "cycle-2"
        reloadFrom h dest2
        (_, bytes3, _) ← captureLive h "cycle-3"
        bytes2 `shouldBe` bytes1
        bytes3 `shouldBe` bytes1
        BS.length bytes1 `shouldSatisfy` (> 0)
