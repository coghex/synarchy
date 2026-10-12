-- TEMPORARY (#2747): a deliberate hang for the cancellation demonstration.
-- Never merged; removed before final review.
module Test.Headless.Core.DiagHang (spec) where

import UPrelude
import Control.Concurrent (threadDelay)
import Test.Hspec

spec ∷ Spec
spec = describe "Diag.Hang.Example" $ do
    it "completes before the hang" (pure () ∷ IO ())
    it "hangs here (temporary #2747 demonstration)"
        (forever (threadDelay 1000000) ∷ IO ())
