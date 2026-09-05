-- | The Tutorial HUD's gameplay-surface resize lifecycle (requirements
--   5/6/8): one rebuild per real framebuffer resize, one per scale-only
--   notification, reachable geometry across every supported responsive
--   band, safe degradation outside that envelope, and the engine's
--   minimum UI scale (#2198).
--
--   A fixture-consuming fragment of 'Test.Headless.UI.TutorialHud':
--   the engine, the Lua backend and the per-case reset all belong to
--   'Test.Headless.UI.TutorialHud.Support', and this module boots
--   neither.
module Test.Headless.UI.TutorialHud.Responsive (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Config (vcUIScale)
import Test.Headless.UI.TutorialHud.Support
    ( Fixture, bootAt, decodeOr, evalOk, luaLines, resetFixture, treeHelpers )

-- * Decoded probe shapes

data BandProbe = BandProbe
    { bpW ∷ Int, bpH ∷ Int, bpScale ∷ Double
    , bpToggleInFrame ∷ Bool, bpToggleClearOfToolbars ∷ Bool
    , bpPanelInFrame ∷ Bool, bpListInFrame ∷ Bool
    , bpCapacity ∷ Int, bpRangeOk ∷ Bool
    , bpToggleStable ∷ Bool } deriving (Show, Eq)
instance FromJSON BandProbe where
    parseJSON = withObject "BandProbe" $ \o →
        BandProbe <$> o .: "w" <*> o .: "h" <*> o .: "scale"
                   <*> o .: "toggleInFrame" <*> o .: "toggleClear"
                   <*> o .: "panelInFrame" <*> o .: "listInFrame"
                   <*> o .: "capacity" <*> o .: "rangeOk"
                   <*> o .: "toggleStable"

data ResizeProbe = ResizeProbe
    { rzBeforeRebuilds ∷ Int, rzBroadcastRebuilds ∷ Int, rzAfterRebuilds ∷ Int
    , rzBeforeOpen ∷ Bool, rzAfterOpen ∷ Bool
    , rzBeforeOffset ∷ Int, rzAfterOffset ∷ Int, rzMovedRight ∷ Bool
    , rzClearOfToolbars ∷ Bool }
instance FromJSON ResizeProbe where
    parseJSON = withObject "ResizeProbe" $ \o →
        ResizeProbe <$> o .: "beforeRebuilds" <*> o .: "broadcastRebuilds"
                     <*> o .: "afterRebuilds"
                     <*> o .: "beforeOpen" <*> o .: "afterOpen"
                     <*> o .: "beforeOffset" <*> o .: "afterOffset"
                     <*> o .: "movedRight" <*> o .: "clearOfToolbars"

data RescaleProbe = RescaleProbe
    { rsclBeforeRebuilds ∷ Int, rsclAfterRebuilds ∷ Int
    , rsclBeforeOpen ∷ Bool, rsclAfterOpen ∷ Bool
    , rsclBeforeOffset ∷ Int, rsclAfterOffset ∷ Int, rsclGrewRows ∷ Bool }
instance FromJSON RescaleProbe where
    parseJSON = withObject "RescaleProbe" $ \o →
        RescaleProbe <$> o .: "beforeRebuilds" <*> o .: "afterRebuilds"
                      <*> o .: "beforeOpen" <*> o .: "afterOpen"
                      <*> o .: "beforeOffset" <*> o .: "afterOffset"
                      <*> o .: "grewRows"

data DegradeProbe = DegradeProbe { dgOk ∷ Text, dgMinimizeIgnored ∷ Bool }
instance FromJSON DegradeProbe where
    parseJSON = withObject "DegradeProbe" $ \o →
        DegradeProbe <$> o .: "ok" <*> o .: "minimizeIgnored"

spec ∷ SpecWith Fixture
spec = do

    describe "the gameplay-surface resize lifecycle (requirements 5/6/8)" $ do
        it "a real framebuffer resize rebuilds exactly once and preserves open state and offset" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "wideTree(60)"
                , "th.setOpen(true); th.setScrollOffset(4);"
                , "local before = th.dump();"
                -- The REAL order: broadcastToModules fans the resize out
                -- in script-map order, so this module can be reached
                -- while hud still holds the PRE-resize toolbar;
                -- ui_manager rebuilds hud and only then reflows this
                -- surface.
                , "th.onFramebufferResize(1920, 1080);"
                , "local broadcast = th.dump();"
                , "hud.init(1, 2, 1920, 1080); hud.createUI();"
                , "th.reflow();"
                , "local after = th.dump();"
                -- Clear of the toolbar clusters at their NEW positions,
                -- which only holds if the rebuild ran after hud's.
                , "local reserved = require('scripts.ui.reserved_regions');"
                , "local clear = true;"
                , "for _, rc in ipairs(hud.getToolbarRects()) do"
                , "  if reserved.rectsOverlap({x=after.toggle.x, y=after.toggle.y,"
                , "      w=after.toggle.w, h=after.toggle.h}, rc) then clear = false end end;"
                , "return { beforeRebuilds = before.rebuildCount,"
                , "         broadcastRebuilds = broadcast.rebuildCount,"
                , "         afterRebuilds = after.rebuildCount,"
                , "         beforeOpen = before.open, afterOpen = after.open,"
                , "         beforeOffset = before.scrollOffset,"
                , "         afterOffset = after.scrollOffset,"
                , "         clearOfToolbars = clear,"
                , "         movedRight = (after.toggle.x > before.toggle.x) }"
                ]
            probe ← decodeOr r ∷ IO ResizeProbe
            -- The broadcast half must NOT rebuild: hud.getToolbarRects()
            -- still describes the pre-resize toolbar at that point, so a
            -- rebuild there would anchor the toggle against stale bounds.
            rzBroadcastRebuilds probe `shouldBe` rzBeforeRebuilds probe
            -- Requirement 8: ONE rebuild per real resize across the whole
            -- path — the module stays out of ui_manager_boot's manual
            -- onFramebufferResize forward set (which would double-fire
            -- it) and takes its single rebuild from the post-hud reflow,
            -- exactly like popup/unit_info_v2.
            rzAfterRebuilds probe - rzBeforeRebuilds probe `shouldBe` 1
            (rzBeforeOpen probe, rzAfterOpen probe) `shouldBe` (True, True)
            (rzBeforeOffset probe, rzAfterOffset probe) `shouldBe` (4, 4)
            rzMovedRight probe `shouldBe` True
            rzClearOfToolbars probe `shouldBe` True

        it "a scale-only change reaches it exactly once through uiManager.notifyGameplayRescale" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                -- Everything except the module under test is stubbed,
                -- the same technique UI.ResponsiveGameplay uses for
                -- this fan-out; the hud stub keeps the fields the
                -- tutorial HUD reads off it.
                , "local stub = { onFramebufferResize = function() end,"
                , "               reflow = function() end };"
                , "local hudStub = { onFramebufferResize = function() end,"
                , "                  visible = true, menuFont = 2, boxTexSet = 1,"
                , "                  getToolbarRects = function() return {} end };"
                , "local tp = require('scripts.tutorial_progress');"
                , "tp.reset(); tp.setTree(wideTree(60));"
                , "local th = require('scripts.tutorial_hud');"
                , "th.init(); th.reflow(1920, 1080);"
                , "th.setOpen(true); th.setScrollOffset(6);"
                , "package.loaded['scripts.world_view'] = stub;"
                , "package.loaded['scripts.hud'] = hudStub;"
                , "package.loaded['scripts.ui.context_menu'] = stub;"
                , "package.loaded['scripts.build_tool_remote_warning'] = stub;"
                , "package.loaded['scripts.popup'] = stub;"
                , "package.loaded['scripts.event_log'] = stub;"
                , "package.loaded['scripts.combat_log'] = stub;"
                , "package.loaded['scripts.injury_log_panel'] = stub;"
                , "package.loaded['scripts.unit_log'] = stub;"
                , "package.loaded['scripts.unit_info_v2'] = stub;"
                , "package.loaded['scripts.debug'] = stub;"
                , "package.loaded['scripts.test_arena'] = stub;"
                , "local uiManager = require('scripts.ui_manager');"
                , "uiManager.moduleReady.worldView = true;"
                , "uiManager.moduleReady.hud = true;"
                , "uiManager.moduleReady.buildToolRemoteWarning = true;"
                , "uiManager.moduleReady.popupsAndLogs = true;"
                , "uiManager.moduleReady.testArena = true;"
                , "local before = th.dump();"
                , "engine.setUIScale(2.0);"
                , "uiManager.notifyGameplayRescale(1920, 1080);"
                , "local after = th.dump();"
                , "return { beforeRebuilds = before.rebuildCount,"
                , "         afterRebuilds = after.rebuildCount,"
                , "         beforeOpen = before.open, afterOpen = after.open,"
                , "         beforeOffset = before.scrollOffset,"
                , "         afterOffset = after.scrollOffset,"
                , "         grewRows = (after.rowH > before.rowH) }"
                ]
            probe ← decodeOr r ∷ IO RescaleProbe
            rsclAfterRebuilds probe - rsclBeforeRebuilds probe `shouldBe` 1
            (rsclBeforeOpen probe, rsclAfterOpen probe) `shouldBe` (True, True)
            (rsclBeforeOffset probe, rsclAfterOffset probe) `shouldBe` (6, 6)
            -- The new scale really did reach the layout.
            rsclGrewRows probe `shouldBe` True

        it "keeps reachable right-anchored geometry and a scrollable list across every supported band" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                , "local responsive = require('scripts.ui.responsive');"
                , "local reserved = require('scripts.ui.reserved_regions');"
                , "local hud = require('scripts.hud');"
                , "local tp = require('scripts.tutorial_progress');"
                , "tp.reset(); tp.setTree(wideTree(60));"
                , "local th = require('scripts.tutorial_hud');"
                , "local out = {};"
                -- Derived from responsive.lua's own bands table (never
                -- hand-copied), at both scale bounds of each band.
                , "for _, band in ipairs(responsive.bands) do"
                , "  for _, h in ipairs({ band.minH, band.maxH }) do"
                , "    for _, sc in ipairs({ band.minScale, band.maxScale }) do"
                , "      local w = math.max(responsive.MIN_WIDTH, math.floor(h * 16 / 9));"
                , "      engine.setUIScale(sc);"
                , "      hud.init(1, 2, w, h); hud.createUI(); hud.visible = true;"
                , "      th.init(); th.reflow(w, h);"
                , "      local closed = th.dump();"
                , "      th.setOpen(true);"
                , "      local d = th.dump();"
                , "      local t = d.toggle;"
                , "      local clear = true;"
                , "      for _, rc in ipairs(hud.getToolbarRects()) do"
                , "        if reserved.rectsOverlap({x=t.x,y=t.y,w=t.w,h=t.h}, rc) then"
                , "          clear = false end end;"
                , "      out[#out + 1] = {"
                , "        w = w, h = h, scale = sc,"
                , "        toggleInFrame = (t.x >= 0 and t.y >= 0"
                , "            and (t.x + t.w) <= w and (t.y + t.h) <= h),"
                , "        toggleClear = clear,"
                , "        panelInFrame = (d.panelX >= 0 and (d.panelX + d.panelW) <= w),"
                , "        listInFrame = (d.listTop >= 0 and d.listBottom <= h"
                , "            and d.listTop <= d.listBottom),"
                , "        capacity = d.capacity,"
                , "        rangeOk = (d.scrollRange == math.max(0, d.activeCount - d.capacity)),"
                , "        toggleStable = (closed.toggle.x == t.x and closed.toggle.y == t.y"
                , "            and closed.toggle.w == t.w and closed.toggle.h == t.h) };"
                , "      th.setOpen(false);"
                , "    end end end;"
                , "return out"
                ]
            rows ← decodeOr r ∷ IO [BandProbe]
            -- 4 bands x 2 heights x 2 scales.
            length rows `shouldBe` 16
            forM_ rows $ \row → do
                let ctx = " at " ⧺ show (bpW row) ⧺ "x" ⧺ show (bpH row)
                            ⧺ " @" ⧺ show (bpScale row)
                (show (bpToggleInFrame row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpToggleClearOfToolbars row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpPanelInFrame row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpListInFrame row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpRangeOk row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpToggleStable row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpCapacity row > 0) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)

        it "degrades safely (no crash, no invalid geometry) outside the supported envelope" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                , "local hud = require('scripts.hud');"
                , "local tp = require('scripts.tutorial_progress');"
                , "tp.reset(); tp.setTree(wideTree(60));"
                , "local th = require('scripts.tutorial_hud');"
                , "th.init();"
                -- The last two are DEGENERATE-but-positive sizes: a
                -- readable-width floor that ignored the framebuffer
                -- would push the toggle straight off the right edge
                -- there (review round 1). The 4K combo's scale is
                -- OUT OF BAND for its height (band 1601+ starts at
                -- 1.5) but IN the engine's 0.5–4.0 domain: a scale
                -- below 0.5 is refused by engine.setUIScale (#2198)
                -- and would silently leave the previous scale in
                -- place, making the case vacuous.
                , "local combos = { {320, 240, 4.0}, {800, 600, 4.0},"
                , "                 {3840, 2160, 0.5}, {640, 480, 0.5},"
                , "                 {12, 9, 1.0}, {1, 1, 4.0} };"
                , "local ok = true;"
                , "for _, c in ipairs(combos) do"
                , "  engine.setUIScale(c[3]);"
                , "  hud.init(1, 2, c[1], c[2]); hud.createUI();"
                , "  local good, err = pcall(function()"
                , "      th.reflow(c[1], c[2]); th.setOpen(true);"
                , "      local d = th.dump();"
                , "      assert(d.toggle.w > 0 and d.toggle.h > 0, 'toggle collapsed');"
                , "      assert(d.toggle.x >= 0 and d.toggle.y >= 0, 'toggle off-screen');"
                , "      assert(d.toggle.x + d.toggle.w <= c[1], 'toggle overflows width');"
                , "      assert(d.toggle.y + d.toggle.h <= c[2], 'toggle overflows height');"
                , "      assert(d.panelW > 0 and d.panelX >= 0, 'panel invalid');"
                , "      assert(d.capacity >= 0, 'negative capacity');"
                , "      assert(d.listTop >= 0 and d.listBottom >= d.listTop,"
                , "             'invalid list viewport');"
                , "      assert(d.scrollOffset >= 0 and d.scrollOffset <= d.scrollRange,"
                , "             'offset outside range');"
                , "      assert(#d.rows <= d.capacity, 'more rows than capacity');"
                , "      th.setOpen(false); end);"
                , "  if not good then ok = tostring(err) end end;"
                -- A 0x0 minimize must never be adopted as geometry,
                -- on either half of the resize path.
                , "local prior = th.dump();"
                , "th.onFramebufferResize(0, 0);"
                , "th.reflow(0, 0);"
                , "local after = th.dump();"
                , "return { ok = tostring(ok), minimizeIgnored = (after.fbW == prior.fbW"
                , "         and after.fbH == prior.fbH) }"
                ]
            probe ← decodeOr r ∷ IO DegradeProbe
            dgOk probe `shouldBe` "true"
            dgMinimizeIgnored probe `shouldBe` True

        it "cannot be driven below the engine's minimum UI scale: engine.setUIScale(0.25) is refused and the stored scale is unchanged (#2198)" $ \(env, ls) → do
            resetFixture env ls
            before ← vcUIScale ⊚ readIORef (videoConfigRef env)
            r ← evalOk ls "return engine.setUIScale(0.25)"
            r `shouldBe` "false"
            after ← vcUIScale ⊚ readIORef (videoConfigRef env)
            after `shouldBe` before
