-- | #750 gate (Phase C, child C4 of #741): migrating gameplay HUD/
--   overlay/modal surfaces onto the #748 responsive lifecycle, plus
--   the new deterministic reserved-region/priority contract
--   (scripts/ui/reserved_regions.lua) and the resize-safe teardown of
--   world_page-mounted popups/tools (scripts/ui/view_teardown.lua's
--   new "resize" transition, driven from scripts/hud.lua's
--   createUI()).
--
--   Same headless constraint as 'Test.Headless.UI.ResponsiveMenus':
--   the full ui_manager boot sequence never reaches gameplay UI
--   headless (it gates on fontsReady, which needs a GPU font atlas).
--   This suite boots scripts/hud.lua directly (hud.init + hud.createUI)
--   with synthetic texture/font handles — the same technique
--   ResponsiveMenus uses for menu screens and
--   'Test.Headless.UI.InputOwnership' uses for scripts/debug.lua —
--   rather than going through uiManager.init(). engine.getTextWidth
--   always measures 0 headless this way, so assertions here are
--   geometry-only, the same caveat ResponsiveMenus documents.
--
--   'uiManager.onFramebufferResize' itself is deliberately NOT driven
--   here: its meaningful body only runs once the boot-only local
--   `initialized` flips true, which — like fontsReady — only happens
--   through the real (GPU-gated) boot sequence. This suite instead
--   drives every gameplay surface's own onFramebufferResize directly,
--   and 'uiManager.notifyGameplayRescale' (the #750 scale-only path),
--   which has no such gate.
--
--   #2126: the suite is split into owner-scoped fragments behind THIS
--   facade, which keeps the one and only @aroundAll withSharedFixture@
--   — the complete aggregate still boots exactly one 'EngineEnv' and one
--   'LuaBackendState' (CH-116's cost guardrail). The owners
--   ('Test.Headless.UI.ResponsiveGameplay.Lifecycle', @.Surfaces@,
--   @.Container@, @.Etymology@) export @SpecWith SharedFixture@
--   fragments and never boot anything; the shared plumbing lives in
--   'Test.Headless.UI.ResponsiveGameplay.Fixture'. The sequence below
--   reproduces the monolith's aggregate group order exactly, which is
--   why the Surfaces owner contributes two fragments around the
--   container owner rather than one.
module Test.Headless.UI.ResponsiveGameplay (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.UI.ResponsiveGameplay.Fixture (withSharedFixture)
import qualified Test.Headless.UI.ResponsiveGameplay.Lifecycle as Lifecycle
import qualified Test.Headless.UI.ResponsiveGameplay.Surfaces as Surfaces
import qualified Test.Headless.UI.ResponsiveGameplay.Container as Container
import qualified Test.Headless.UI.ResponsiveGameplay.Etymology as Etymology

spec ∷ Spec
spec = aroundAll withSharedFixture $ do
    Lifecycle.spec
    Surfaces.preContainerSpec
    Container.spec
    Surfaces.pickerSpec
    Etymology.spec
