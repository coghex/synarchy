{-# LANGUAGE Strict #-}
-- | "Scenario.Capture" (#2714, SCN-14 of epic #2698): the scenario-capture
--   service, driven through its production entry point, the production Lua
--   message pump, the real save barrier, the real world thread, the real
--   projection and the real file writer, against the shipped definitions.
--   Only the owner threads a headless harness never starts are stand-ins
--   ("Test.Headless.Scenario.Load.Harness"), and the examples act as those
--   owners the way @Test.Headless.Save.Barrier@ does.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Scenario.Capture"'@.
module Test.Headless.Scenario.Capture (spec) where

import UPrelude
import Test.Hspec
import qualified Test.Headless.Scenario.Capture.Content as Content
import qualified Test.Headless.Scenario.Capture.Edits as Edits
import qualified Test.Headless.Scenario.Capture.Exclusion as Exclusion
import qualified Test.Headless.Scenario.Capture.Storage as Storage
import qualified Test.Headless.Scenario.Capture.Coherence as Coherence
import qualified Test.Headless.Scenario.Capture.Cycle as Cycle
import Control.Exception (finally)
import Test.Headless.Scenario.Capture.Support (clearLine)
import Test.Headless.Scenario.Load.Harness (withH)

spec ∷ Spec
spec = aroundAll withH $ aroundWith (\example h → (clearLine h ≫ example h) `finally` clearLine h) $ do
    Content.spec
    Edits.spec
    Exclusion.spec
    Storage.spec
    Coherence.spec
    Cycle.spec
