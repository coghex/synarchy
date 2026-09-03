{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Shared rejection vocabulary for the pure unit-atlas specs (#1259,
--   TEX-3).
--
--   Every owner under "Test.Headless.Unit.Atlas" asserts on rejections,
--   and all three assert them the same way: match a SUBSTRING of the
--   rendered diagnostic so the test pins WHAT was wrong without
--   freezing the exact wording. This leaf owns that vocabulary so the
--   three owners cannot drift into three spellings of it.
--
--   A support leaf: it imports production modules only, never a spec
--   owner.
module Test.Headless.Unit.Atlas.Rejection
    ( rejection
    , shouldReject
    , isRejected
    ) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Unit.Atlas.Index (AtlasLoadError, renderAtlasLoadError)

-- | The reason text of a rejection, or a marker when it unexpectedly
--   succeeded. Assertions match on a substring so they pin WHAT was
--   wrong without freezing the exact wording.
rejection ∷ Either AtlasLoadError a → Text
rejection (Left e)  = renderAtlasLoadError e
rejection (Right _) = "<<accepted>>"

shouldReject ∷ HasCallStack ⇒ Either AtlasLoadError a → Text → Expectation
shouldReject r needle =
    let msg = rejection r
    in if needle `T.isInfixOf` msg
        then pure ()
        else expectationFailure
            ("expected a rejection mentioning " ⧺ show needle
             ⧺ ", got: " ⧺ T.unpack msg)

isRejected ∷ Either AtlasLoadError a → Bool
isRejected (Left _) = True
isRejected _        = False
