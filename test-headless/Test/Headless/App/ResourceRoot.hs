-- | @App.ResourceRoot@'s precedence decision (#636, #1949).
--
--   The root the executable chdirs into is picked before any boot mode
--   is selected, from @--resource-root@ and @SYNARCHY_ROOT@ alone. The
--   defect #1949 closed lived entirely in that pick: an empty flag
--   operand was carried through as a present, highest-precedence
--   explicit selection, and @makeAbsolute \"\"@ then resolved it to
--   whichever directory the process was launched from — so
--   @--resource-root \"$UNSET_VARIABLE\"@ loaded the caller's own
--   checkout and reported it as the explicit selection, overriding a
--   @SYNARCHY_ROOT@ naming a different one.
--
--   'decideResourceRoot' is that pick, made pure so these cases need
--   no process and no filesystem: an empty operand is one of its
--   outcomes rather than something a later @doesDirectoryExist@
--   happens to catch when the cwd is unsuitable. That the rejection
--   reaches stderr with exit 1 before any engine, window or server
--   starts is @tools\/resource_root_probe.py@'s half.
module Test.Headless.App.ResourceRoot (spec) where

import UPrelude
import Test.Hspec
import App.ResourceRoot (RootDecision(..), decideResourceRoot)

-- | The repo-root default: no flag, no environment variable.
noEnv ∷ Maybe String
noEnv = Nothing

spec ∷ Spec
spec = describe "App.ResourceRoot precedence (#1949)" $ do

  describe "an explicitly empty --resource-root operand" $ do

    it "is its own outcome, not an explicit root" $
      decideResourceRoot ["--resource-root", ""] noEnv
        `shouldBe` RootFlagEmptyValue

    it "does not fall through to a valid SYNARCHY_ROOT" $
      decideResourceRoot ["--resource-root", ""] (Just "/some/checkout")
        `shouldBe` RootFlagEmptyValue

    it "does not fall through to the current directory" $
      decideResourceRoot ["--resource-root", ""] (Just "")
        `shouldBe` RootFlagEmptyValue

    it "is rejected wherever it sits in argv" $
      decideResourceRoot ["--dump", "--resource-root", "", "--seed", "7"] noEnv
        `shouldBe` RootFlagEmptyValue

    it "is distinct from a bare trailing flag" $
      decideResourceRoot ["--resource-root", ""] noEnv
        `shouldNotBe` decideResourceRoot ["--resource-root"] noEnv

  describe "the outcomes it must not disturb" $ do

    it "keeps a bare trailing --resource-root a missing-operand error" $
      decideResourceRoot ["--dump", "--resource-root"] noEnv
        `shouldBe` RootFlagMissingValue

    it "keeps a bare trailing flag an error even with SYNARCHY_ROOT set" $
      decideResourceRoot ["--resource-root"] (Just "/some/checkout")
        `shouldBe` RootFlagMissingValue

    it "takes a non-empty operand as the explicit root" $
      decideResourceRoot ["--resource-root", "/some/checkout"] noEnv
        `shouldBe` RootExplicit "/some/checkout" "--resource-root"

    it "keeps a non-empty operand ahead of a non-empty SYNARCHY_ROOT" $
      decideResourceRoot ["--resource-root", "/from/flag"] (Just "/from/env")
        `shouldBe` RootExplicit "/from/flag" "--resource-root"

    it "accepts a relative operand verbatim" $
      decideResourceRoot ["--resource-root", "../synarchy"] noEnv
        `shouldBe` RootExplicit "../synarchy" "--resource-root"

    it "accepts an operand containing spaces" $
      decideResourceRoot ["--resource-root", "/two words/synarchy"] noEnv
        `shouldBe` RootExplicit "/two words/synarchy" "--resource-root"

    it "accepts an operand with a trailing separator" $
      decideResourceRoot ["--resource-root", "/some/checkout/"] noEnv
        `shouldBe` RootExplicit "/some/checkout/" "--resource-root"

    it "takes a non-empty SYNARCHY_ROOT when the flag is absent" $
      decideResourceRoot ["--dump"] (Just "/from/env")
        `shouldBe` RootExplicit "/from/env" "SYNARCHY_ROOT"

    it "treats an EMPTY SYNARCHY_ROOT as absence, as it always has" $
      decideResourceRoot ["--dump"] (Just "")
        `shouldBe` RootCurrentDirectory

    it "falls back to the current directory with neither supplied" $
      decideResourceRoot ["--dump", "--seed", "7"] noEnv
        `shouldBe` RootCurrentDirectory
