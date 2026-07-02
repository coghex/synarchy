{-# LANGUAGE UnicodeSyntax #-}
-- | Corner-progress derivation for the construction display (#96).
--   The contract under test: 'constructCorners' maps a designation's
--   scalar build progress onto the same 4-corner state mining's
--   'digSlopeMask' consumes — zero progress must derive mask 0 (a
--   fresh blueprint renders nothing), corners drain one quarter of
--   the job at a time in fixed NW→NE→SE→SW order, and mid-job states
--   produce a non-zero mask (the tile visibly under work).
module Test.Headless.Construct.Corners (spec) where

import UPrelude
import Test.Hspec
import World.Construct.Types (constructCorners)
import World.Mine.Types (digSlopeMask)

spec ∷ Spec
spec = describe "constructCorners" $ do
    it "derives full corners (mask 0, nothing rendered) at progress 0" $ do
        constructCorners 0.0 `shouldBe` (1.0, 1.0, 1.0, 1.0)
        digSlopeMask (constructCorners 0.0) `shouldBe` 0

    it "drains all corners at progress 1" $
        constructCorners 1.0 `shouldBe` (0.0, 0.0, 0.0, 0.0)

    it "drains NW first, one quarter of the job per corner" $ do
        constructCorners 0.25 `shouldBe` (0.0, 1.0, 1.0, 1.0)
        constructCorners 0.5  `shouldBe` (0.0, 0.0, 1.0, 1.0)
        constructCorners 0.75 `shouldBe` (0.0, 0.0, 0.0, 1.0)

    it "splits a mid-quarter progress across the active corner" $
        constructCorners 0.125 `shouldBe` (0.5, 1.0, 1.0, 1.0)

    it "renders a non-zero mask once two corners have drained" $ do
        -- An edge slopes only when BOTH its corners are below the dig
        -- threshold (digSlopeMask), so one drained corner (progress
        -- 0.3) still reads flat; from half way the site visibly cuts
        -- in — including at completion (the all-dug holdback keeps
        -- mask 15 from flashing flat).
        digSlopeMask (constructCorners 0.3) `shouldBe` 0
        digSlopeMask (constructCorners 0.5) `shouldNotBe` 0
        digSlopeMask (constructCorners 0.7) `shouldNotBe` 0
        digSlopeMask (constructCorners 1.0) `shouldNotBe` 0

    it "clamps out-of-range progress" $ do
        constructCorners (-0.5) `shouldBe` (1.0, 1.0, 1.0, 1.0)
        constructCorners 1.5    `shouldBe` (0.0, 0.0, 0.0, 0.0)
