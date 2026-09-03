-- | Mode A's escort session (#1250, epic #1013 slice UIT-3B): the
--   session lifecycle, the hold, the two flanking panes, and the
--   immediate commits their rows perform.
--
--   Registered under a describe beginning "Transfer context menu" so
--   @--match "Transfer context menu"@ reaches this alongside
--   'Test.Headless.UI.TransferContextMenu' (the session's entry point)
--   and 'Test.Headless.UI.TransferGestures' (Mode B's own gestures),
--   which is the gate the issue names.
--
--   This module is the aggregate façade and owns nothing but the
--   composition (#2090): ONE 'aroundAll' over the shared engine and
--   Lua state of "Test.Headless.UI.TransferSession.Fixture", under
--   which each independently-changing contract group supplies its own
--   describe —
--
--   * "Test.Headless.UI.TransferSession.Lifecycle";
--   * "Test.Headless.UI.TransferSession.CreationBoundary";
--   * "Test.Headless.UI.TransferSession.Failure";
--   * "Test.Headless.UI.TransferSession.Gestures";
--   * "Test.Headless.UI.TransferSession.Registration".
--
--   A group owner never reaches another group owner: everything they
--   share is the fixture module's, and everything local to one group
--   stays inside it.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Transfer context menu (Mode A"'@.
module Test.Headless.UI.TransferSession (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.UI.TransferSession.Fixture (withSharedFixture)
import qualified Test.Headless.UI.TransferSession.CreationBoundary as CreationBoundary
import qualified Test.Headless.UI.TransferSession.Failure as Failure
import qualified Test.Headless.UI.TransferSession.Gestures as Gestures
import qualified Test.Headless.UI.TransferSession.Lifecycle as Lifecycle
import qualified Test.Headless.UI.TransferSession.Registration as Registration

spec ∷ Spec
spec = aroundAll withSharedFixture $
  describe "Transfer context menu (Mode A escort session, #1250)" $ do
    Lifecycle.spec
    CreationBoundary.spec
    Failure.spec
    Gestures.spec
    Registration.spec
