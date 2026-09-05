-- | The "Tutorial HUD" gate (#960, phase 3 of the tutorial epic #956):
--   @scripts/tutorial_hud.lua@, the one surface that renders #958's
--   tutorial view model.
--
--   Same headless technique (and the same constraint) as
--   'Test.Headless.UI.ResponsiveGameplay': the full ui_manager boot
--   never reaches gameplay UI headless (it gates on fontsReady, which
--   needs a GPU font atlas), so this suite boots @scripts/hud.lua@
--   directly with synthetic texture/font handles and then boots the
--   tutorial HUD on top of it. @engine.getTextWidth@ measures 0 in that
--   fixture, so every assertion here is geometry- or count-based and
--   the module under test derives row height and scroll range from the
--   UI scale rather than from measured text. The one exception is
--   #1419's toggle-caption fit, whose own group STUBS
--   @engine.getTextWidth@ with a deterministic non-zero metric (the
--   'Test.Headless.UI.ResponsiveGameplay' idiom) and restores it
--   immediately, precisely so the rest of this suite stays
--   measurement-free — an unmeasurable caption leaves the toggle at
--   exactly its historical constant width.
--
--   The tutorial TREE arrives two ways on purpose: injected through
--   @tutorialProgress.setTree@ (the same injection point #958's own
--   gate uses, for the shapes a hand-authored tree can produce), and
--   loaded for real from @data/tutorials@ through
--   @engine.loadTutorialDir@ so the shipped YAML's labels and tooltips
--   are proven to reach the screen. The shared headless engine does not
--   populate the tutorial registry on its own, and this suite puts it
--   back empty afterwards, exactly as 'Test.Headless.Tutorial.Definitions'
--   does.
--
--   The suite is SPLIT by owner. This module is the facade and the
--   fixture's only entry point: it holds the one
--   @aroundAll withSharedFixture@, so the complete aggregate creates
--   exactly one 'Engine.Core.State.EngineEnv' and one
--   'Engine.Scripting.Lua.Types.LuaBackendState' however many owners
--   run. Each child exports a fixture-consuming
--   @SpecWith Fixture@ fragment, boots no engine and no Lua state of
--   its own, and keeps its own probe decoders and single-owner Lua
--   helpers beside the cases that read them:
--
--   * "Test.Headless.UI.TutorialHud.Lifecycle" — the presentation
--     lifecycle (#960 requirements 2/5);
--   * "Test.Headless.UI.TutorialHud.ModelPresentation" — #958's view
--     model and #2056's renderer-presentation handshake;
--   * "Test.Headless.UI.TutorialHud.Scrolling" — scoped wheel capture
--     and scrolling;
--   * "Test.Headless.UI.TutorialHud.Responsive" — the gameplay-surface
--     resize lifecycle;
--   * "Test.Headless.UI.TutorialHud.CaptionFit" — #1419's measured
--     toggle-caption fit.
--
--   "Test.Headless.UI.TutorialHud.Support" owns the fixture type,
--   'withSharedFixture', 'resetFixture', the tutorial-tree
--   constructors and the Lua eval/decode plumbing they all share.
--   Group order here is the aggregate's declared order and is
--   load-bearing alongside each example's own 'resetFixture' call.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Tutorial HUD"'@.
module Test.Headless.UI.TutorialHud (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.UI.TutorialHud.Support (withSharedFixture)
import qualified Test.Headless.UI.TutorialHud.CaptionFit as CaptionFit
import qualified Test.Headless.UI.TutorialHud.Lifecycle as Lifecycle
import qualified Test.Headless.UI.TutorialHud.ModelPresentation as ModelPresentation
import qualified Test.Headless.UI.TutorialHud.Responsive as Responsive
import qualified Test.Headless.UI.TutorialHud.Scrolling as Scrolling

spec ∷ Spec
spec = aroundAll withSharedFixture $ do
    Lifecycle.spec
    ModelPresentation.spec
    Scrolling.spec
    Responsive.spec
    CaptionFit.spec
