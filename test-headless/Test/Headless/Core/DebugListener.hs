-- | The mode-aware debug-listener policy (#1190).
--
--   @--headless@ and @--offscreen@ have no window: the debug TCP
--   console is their ONLY interactive control surface. Before this,
--   both booted happily without one — a failed bind became a warning
--   and an inert 'TQueue' nothing ever fed, and port 0 (issue #46's
--   \"no TCP listener at all\" sentinel for @--dump@) was honoured for
--   any caller, because 'startDebugServer' sees a number and no boot
--   mode. The result was a live process with five worker threads, no
--   @READY@ line for the documented wait-for-boot pattern to match, and
--   no reachable @engine.quit()@.
--
--   This is the pure half of the fix: the per-mode policy, the port-0
--   dispatch it drives, and the diagnostic a refused boot dies with.
--   The process-level half — exit code, cleanup, and the absence of a
--   @READY@ marker on a real boot — is @tools\/debug_console_boot_probe.py@,
--   which cannot be expressed here (nothing in-process can assert that
--   another process exited).
module Test.Headless.Core.DebugListener (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Engine.Core.Types (BootMode(..), bootModeName)
import Engine.Scripting.Lua.DebugServer
  ( DebugConsolePolicy(..), debugConsolePolicy
  , ListenerAction(..), listenerAction
  , DebugListenerFailure(..), debugListenerFailureMessage )

-- | Every boot mode, from the type itself rather than a hand-written
--   list: a mode added without a policy decision fails these cases
--   instead of quietly inheriting one.
allModes ∷ [BootMode]
allModes = [minBound .. maxBound]

-- | The two modes whose only control surface is the console.
consoleRequiredModes ∷ [BootMode]
consoleRequiredModes = [ModeHeadless, ModeOffscreen]

consoleOptionalModes ∷ [BootMode]
consoleOptionalModes = filter (`notElem` consoleRequiredModes) allModes

spec ∷ Spec
spec = describe "debug-console listener policy (#1190)" $ do

  describe "debugConsolePolicy" $ do
    it "requires a console in exactly the two windowless, console-only \
       \modes" $
      filter ((≡ ConsoleRequired) ∘ debugConsolePolicy) allModes
        `shouldBe` consoleRequiredModes

    it "leaves dump, graphical and preview tolerant — each has a real \
       \alternative (JSON on stdout, or a window and a keyboard)" $
      filter ((≡ ConsoleOptional) ∘ debugConsolePolicy) allModes
        `shouldBe` consoleOptionalModes

    it "classifies all five modes, distinguishing dump from headless and \
       \including offscreen (neither ecHeadless nor BootProfile can)" $ do
      map bootModeName allModes
        `shouldBe` ["dump", "headless", "offscreen", "graphical", "preview"]
      -- ecHeadless is True for both of these and so cannot tell them
      -- apart, yet their policies differ — that is the whole reason
      -- BootMode exists.
      debugConsolePolicy ModeDump `shouldNotBe` debugConsolePolicy ModeHeadless
      -- ecHeadless is False for offscreen, which nonetheless needs the
      -- strict policy.
      debugConsolePolicy ModeOffscreen `shouldBe` ConsoleRequired

  describe "listenerAction" $ do
    it "refuses port 0 in a console-required mode: the #46 sentinel means \
       \'no listener at all', which those modes cannot survive" $
      map (`listenerAction` 0) consoleRequiredModes
        `shouldBe` [RejectPortZero, RejectPortZero]

    it "keeps port 0 meaning 'no listener' for dump — the #46 contract is \
       \scoped, not removed — and for the two windowed modes" $
      map (`listenerAction` 0) consoleOptionalModes
        `shouldBe` map (const TolerateListener) consoleOptionalModes

    it "attempts, and requires, a real bind on any non-zero port in a \
       \console-required mode" $
      [ listenerAction mode port
      | mode ← consoleRequiredModes, port ← [8008, 9008, -1, 65535] ]
        `shouldSatisfy` all (≡ RequireListener)

    it "tolerates every port in a console-optional mode, including the \
       \unbindable ones a required mode rejects" $
      [ listenerAction mode port
      | mode ← consoleOptionalModes, port ← [0, 8008, -1, 65535] ]
        `shouldSatisfy` all (≡ TolerateListener)

  describe "debugListenerFailureMessage" $ do
    let portZeroMsg = debugListenerFailureMessage ModeHeadless 0 ListenerPortZero
        bindErr = "Network.Socket.bind: resource busy (Address already in use)"
        bindMsg = debugListenerFailureMessage ModeOffscreen 9451
                    (ListenerBindFailed bindErr)

    it "names the selected mode" $ do
      portZeroMsg `shouldSatisfy` T.isInfixOf (bootModeName ModeHeadless)
      bindMsg `shouldSatisfy` T.isInfixOf (bootModeName ModeOffscreen)

    it "names the effective port" $ do
      portZeroMsg `shouldSatisfy` T.isInfixOf "port is 0"
      bindMsg `shouldSatisfy` T.isInfixOf "port 9451"

    it "gives port 0 its own explicit reason rather than a bind error it \
       \never attempted" $ do
      portZeroMsg `shouldSatisfy` T.isInfixOf "no TCP listener at all"
      portZeroMsg `shouldSatisfy` T.isInfixOf "--dump"
      portZeroMsg `shouldSatisfy` (not ∘ T.isInfixOf "failed to start")

    it "carries the listener's own error text through verbatim, so an \
       \occupied port reads differently from a malformed one" $ do
      bindMsg `shouldSatisfy` T.isInfixOf bindErr
      bindMsg `shouldSatisfy` T.isInfixOf "failed to start"

    it "is a single line — it goes to stderr beside the READY markers, \
       \where an agent's boot wait is watching" $ do
      T.lines portZeroMsg `shouldSatisfy` ((≡ 1) ∘ length)
      T.lines bindMsg `shouldSatisfy` ((≡ 1) ∘ length)
