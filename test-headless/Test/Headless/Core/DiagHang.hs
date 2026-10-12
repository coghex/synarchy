-- TEMPORARY (#2747): a deliberate hang in a shared fixture's teardown for
-- the job-timeout demonstration. Never merged; removed before final review.
module Test.Headless.Core.DiagHang (spec) where

import UPrelude
import Control.Concurrent (threadDelay)
import Test.Hspec

spec ∷ Spec
spec = aroundAll (\use → use () ≫ forever (threadDelay 1000000)) $
    describe "Diag.Hang.Fixture" $ do
        it "completes before the teardown hang" (pure () ∷ IO ())
        it "also completes before the teardown hang" (pure () ∷ IO ())
