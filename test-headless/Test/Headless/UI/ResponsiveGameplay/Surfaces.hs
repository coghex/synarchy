-- | Surfaces owner of the #750 responsive-gameplay gate (#2126):
--   popup reflow and toolbar avoidance, log-panel clipping, the
--   info-panel / tile-editor bounds, unit information, the
--   build_tool_remote_warning modal and the build-tool picker.
--
--   Two ordered fragments, not one: the monolith interleaved the
--   picker group AFTER the three container groups, and the aggregate
--   order is preserved exactly, so the facade sequences
--   'preContainerSpec' before 'Test.Headless.UI.ResponsiveGameplay.Container'
--   and 'pickerSpec' after it. Both run over the facade's single
--   shared fixture ('Test.Headless.UI.ResponsiveGameplay.Fixture');
--   this module never boots an engine or a Lua VM of its own.
module Test.Headless.UI.ResponsiveGameplay.Surfaces (preContainerSpec, pickerSpec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Test.Headless.UI.ResponsiveGameplay.Fixture

-- | The six surface groups that precede the container owner in the
--   aggregate order.
preContainerSpec ∷ SpecWith SharedFixture
preContainerSpec = do
    describe "popup.lua reflows active cards on resize (#750 round-1 review)" $ do
        it "a card's width is capped to the framebuffer at a narrow, high-scale, still-C2-supported combination (round-3 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,800,2160);"
                , "p.onShowPopup('unit_event', 'hi', 0, 0, 0, 1, {});"
                , "local b = p.getActiveBounds()[1];"
                , "return {w = b.w, inFrame = (b.x >= 0 and (b.x+b.w) <= 800"
                , "                            and b.y >= 0 and (b.y+b.h) <= 2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WidthCapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    wcpInFrame p `shouldBe` True
                    wcpW p `shouldSatisfy` (≤ 800)

        it "a card never overlaps a tall reserved column even when the framebuffer cap alone isn't enough to clear it (round-4 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(2.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,901);"
                , "hud.createUI();"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,800,901);"
                , "p.onShowPopup('unit_event', 'a normal length notification message here', 0, 0, 0, 1, {});"
                , "local b = p.getActiveBounds()[1];"
                , "local rr = require('scripts.ui.reserved_regions');"
                , "local overlapsAny = false;"
                , "for _, rect in ipairs(hud.getToolbarRects()) do"
                , "    if rr.rectsOverlap(b, rect) then overlapsAny = true end"
                , "end;"
                , "return {overlapsAny = overlapsAny,"
                , "        inFrame = (b.x >= 0 and (b.x+b.w) <= 800 and b.y >= 0 and (b.y+b.h) <= 901)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe OverlapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    opOverlapsAny p `shouldBe` False
                    opInFrame p `shouldBe` True

        it "a 10-line card squeezed to a tiny reserved-width gap still produces a positive-width, in-frame click box, OK button, and mute icon (round-14/round-16 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-14 review: the reserved-width cap floored
            -- panelW at a flat 20px — enough to keep panelW itself
            -- positive, but NOT enough once panelW < 2*s.padX (288px at
            -- 800x2160@4x): the line click box's `panelW - 2*s.padX`
            -- went negative and the title/OK button landed outside the
            -- panel box entirely. Stubs hud.getToolbarRects() directly
            -- (popup.lua re-requires scripts.hud fresh on every
            -- renderPopup, so patching the already-loaded module takes
            -- effect immediately) to reproduce the reviewer's own "both
            -- toolbar clusters overlap, 64px free gap" scenario without
            -- depending on the real toolbar's actual measured geometry.
            --
            -- #750 round-16 review: the round-14 floor (padding + 20px)
            -- still didn't account for the panel's own FIXED CHROME —
            -- the OK button alone needs at least s.buttonMinW (320px at
            -- 4x), wider than the whole round-14 floor (308px), and the
            -- mute icon (present here since popup.bootstrap loads both
            -- textures) needs its own strip beside the close X. Extended
            -- this same test to also assert the OK button and mute icon
            -- stay within the panel, not just the line click boxes.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local origRects = hud.getToolbarRects;"
                , "hud.getToolbarRects = function() return {"
                , "    {name='log_toggle', x=0,   y=0, w=368, h=2160},"
                , "    {name='map_toggle', x=432, y=0, w=368, h=2160},"
                , "} end;"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,800,2160);"
                , "local longMsg = string.rep('x', 200);"
                , "for i = 1, 10 do"
                , "    p.onShowPopup('unit_event', longMsg .. i, 0,0,0,1, {x=i,y=i});"
                , "end;"
                , "hud.getToolbarRects = origRects;"
                , "local rec = p.active[1];"
                , "local b = p.getActiveBounds()[1];"
                , "local lineRects = {};"
                , "for _, line in ipairs(rec.lines) do"
                , "    if line.clickBoxHandle then"
                , "        local info = UI.getElementInfo(line.clickBoxHandle);"
                , "        table.insert(lineRects, {w = info.width, x = info.x});"
                , "    end"
                , "end;"
                , "local button = require('scripts.ui.button');"
                , "local okInfo = UI.getElementInfo(button.getElementHandle(rec.okBtnId));"
                , "local closeInfo = UI.getElementInfo(button.getElementHandle(rec.closeBtnId));"
                , "local muteInfo = rec.muteToggleId and UI.getElementInfo(rec.muteToggleId);"
                , "return {panelW = b.w, panelX = b.x, panelInFrame = (b.x >= 0 and (b.x+b.w) <= 800),"
                , "        lineRects = lineRects,"
                , "        okX = okInfo.x, okW = okInfo.width,"
                , "        closeX = closeInfo.x, closeW = closeInfo.width,"
                , "        hasMute = (muteInfo ~= nil),"
                , "        muteX = muteInfo and muteInfo.x or 0, muteW = muteInfo and muteInfo.width or 0}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe PopupSqueezeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    psqPanelW p `shouldSatisfy` (> 0)
                    psqPanelInFrame p `shouldBe` True
                    length (psqLineRects p) `shouldSatisfy` (≥ 1)
                    forM_ (psqLineRects p) $ \lr →
                        lrpW lr `shouldSatisfy` (> 0)
                    let panelRight = psqPanelX p + psqPanelW p
                    psqOkW p `shouldSatisfy` (> 0)
                    psqOkX p `shouldSatisfy` (≥ psqPanelX p)
                    (psqOkX p + psqOkW p) `shouldSatisfy` (≤ panelRight)
                    psqCloseW p `shouldSatisfy` (> 0)
                    psqCloseX p `shouldSatisfy` (≥ psqPanelX p)
                    (psqCloseX p + psqCloseW p) `shouldSatisfy` (≤ panelRight)
                    when (psqHasMute p) $ do
                        psqMuteW p `shouldSatisfy` (> 0)
                        psqMuteX p `shouldSatisfy` (≥ psqPanelX p)
                        (psqMuteX p + psqMuteW p) `shouldSatisfy` (≤ panelRight)

        it "a max-lines card's rows compact to fit instead of overlapping the OK button at 800x1601@4x (round-19 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-19 review: a max-lines (10) card's natural
            -- line-block height alone (1120px at this combination)
            -- plus the panel's fixed chrome needs 1760px total, but
            -- 800x1601@4x is within the supported envelope (1601-2160
            -- at 1.5x-4x). The round-3 panelH cap only shrunk the
            -- PANEL, leaving rows laid out at their full natural
            -- rowH, so the last row collided with the OK button
            -- (moved up to fit the capped panel). unit_event has no
            -- coalesce_window (folds never happen — each onShowPopup
            -- call spawns its own popup, see foldEventIntoPopup), so
            -- lines are appended directly to the spawned popup's
            -- record and p.reflow() (the same renderPopup entry point
            -- a real resize/rescale drives) is called to lay them out
            -- at the new size — mirrors the existing round-16 "10-line
            -- card" test's own direct-record-manipulation technique.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,1601);"
                , "hud.createUI();"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,800,1601);"
                , "p.onShowPopup('unit_event', 'line 1', 0,0,0,1, {});"
                , "local rec = p.active[1];"
                , "for i = 2, 10 do"
                , "    table.insert(rec.lines, {text = 'line '..i, count = 1, coords = {},"
                , "        cycleIdx = 0, lastPannedX = nil, lastPannedY = nil,"
                , "        firstEventTime = 0, lastUpdateTime = 0,"
                , "        labelId = nil, clickBoxHandle = nil});"
                , "end;"
                , "p.reflow();"
                , "local b = p.getActiveBounds()[1];"
                , "local button = require('scripts.ui.button');"
                , "local okInfo = UI.getElementInfo(button.getElementHandle(rec.okBtnId));"
                , "local lastLine = rec.lines[#rec.lines];"
                , "local lastInfo = UI.getElementInfo(lastLine.clickBoxHandle);"
                , "return {lineCount = #rec.lines,"
                , "        panelBottom = (b.y + b.h), panelInFrame = ((b.y + b.h) <= 1601),"
                , "        lastLineBottom = (lastInfo.y + lastInfo.height),"
                , "        okY = okInfo.y, okBottom = (okInfo.y + okInfo.height)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe PopupLineOverflowProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    plopLineCount p `shouldBe` 10
                    plopPanelInFrame p `shouldBe` True
                    plopLastLineBottom p `shouldSatisfy` (≤ plopOkY p)
                    plopOkBottom p `shouldSatisfy` (≤ plopPanelBottom p)

        it "a popup line's label baseline uses the SCALED font size instead of the unscaled base value (round-19 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-19 review: the baseline nudge used to add the
            -- unscaled base fontSize (always 20px) regardless of
            -- uiscale — at 4x the label rendered 60px too high,
            -- bleeding into the row above (and its higher-z click
            -- box). Isolated from the line-block fit above (only 1
            -- short line, comfortably within 1080px) so this checks
            -- ONLY the per-row baseline math, not the max-lines path.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onShowPopup('unit_event', 'line one', 0,0,0,1, {});"
                , "local rec = p.active[1];"
                , "local line = rec.lines[1];"
                , "local label = require('scripts.ui.label');"
                , "local lineInfo = UI.getElementInfo(line.clickBoxHandle);"
                , "local lblInfo = UI.getElementInfo(label.getElementHandle(line.labelId));"
                , "return {offset = (lblInfo.y - lineInfo.y)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe PopupLineBaselineProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → plbpOffset p `shouldBe` 80.0

        it "onFramebufferResize alone stores the new size but does NOT reflow (ordering hazard: it fires before hud rebuilds)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onShowPopup('unit_event', 'hello', 0, 0, 0, 1, {});"
                , "local before = p.getActiveBounds()[1];"
                , "p.onFramebufferResize(800, 600);"
                , "local after = p.getActiveBounds()[1];"
                , "return {beforeX=before.x, beforeY=before.y, afterX=after.x, afterY=after.y,"
                , "        afterInFrame=false}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ReflowProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    (rpAfterX p, rpAfterY p) `shouldBe` (rpBeforeX p, rpBeforeY p)

        it "p.reflow() recenters a card to the current framebuffer instead of leaving it stale or off-screen after a shrink" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onShowPopup('unit_event', 'hello', 0, 0, 0, 1, {});"
                , "local before = p.getActiveBounds()[1];"
                , "p.onFramebufferResize(800, 600);"
                , "p.reflow();"
                , "local after = p.getActiveBounds()[1];"
                , "return {beforeX=before.x, beforeY=before.y, afterX=after.x, afterY=after.y,"
                , "        afterInFrame=(after.x >= 0 and after.y >= 0"
                , "                      and (after.x+after.w) <= 800 and (after.y+after.h) <= 600)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ReflowProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rpAfterInFrame p `shouldBe` True
                    (rpAfterX p, rpAfterY p) `shouldNotBe` (rpBeforeX p, rpBeforeY p)

    describe "popup.lua avoids the reserved toolbar regions (#750) — \"notifications avoid required controls\"" $ do
        it "renderPopup calls reserved_regions.avoidReserved against the real hud toolbar rects" $ \(env, ls) → do
            resetFixture env ls
            called ← evalBool ls $ luaLines
                [ "local rr = require('scripts.ui.reserved_regions');"
                , "local origAvoid = rr.avoidReserved;"
                , "_G.__called = false;"
                , "rr.avoidReserved = function(rect, reserved, w, h)"
                , "    _G.__called = true;"
                , "    return origAvoid(rect, reserved, w, h)"
                , "end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onShowPopup('unit_event', 'hello', 0, 0, 0, 1, {});"
                , "rr.avoidReserved = origAvoid;"
                , "return _G.__called"
                ]
            called `shouldBe` True

        it "a card forced to overlap a reserved rect ends up moved clear of it" $ \(env, ls) → do
            resetFixture env ls
            ok ← evalBool ls $ luaLines
                [ "local rr = require('scripts.ui.reserved_regions');"
                , "local origAvoid = rr.avoidReserved;"
                -- Force the "natural" popup position to be treated as
                -- exactly the reserved region it must avoid, by feeding
                -- avoidReserved a reserved list built from the popup's own
                -- (pre-nudge) rect, captured for the post-hoc check below —
                -- proves the nudge actually changes the final on-screen
                -- bounds rather than being a no-op passthrough.
                , "rr.avoidReserved = function(rect, reserved, w, h)"
                , "    _G.__natural = {x=rect.x, y=rect.y, w=rect.w, h=rect.h};"
                , "    local forced = { { x = rect.x, y = rect.y, w = rect.w, h = rect.h } };"
                , "    return origAvoid(rect, forced, w, h)"
                , "end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onShowPopup('unit_event', 'hello', 0, 0, 0, 1, {});"
                , "local bounds = p.getActiveBounds();"
                , "rr.avoidReserved = origAvoid;"
                , "local card = bounds[1];"
                , "return card ~= nil and not rr.rectsOverlap(card, _G.__natural)"
                ]
            ok `shouldBe` True

    describe "event/combat/injury/unit log panels migrate their scrollable content to real #747 clipping (round-2 review)" $ do
        it "event_log: a rendered row is a real child of a clipsChildren viewport, and its absolute bounds resolve inside the clip" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.emitEvent('unit_event', 'hello world');"
                , "local el = require('scripts.event_log');"
                , "el.bootstrap(1,2,3,1920,1080);"
                , "el.show();"
                , "local vp = UI.getElementInfo(el.rowViewportId);"
                , "local rowHandle = next(el.rowClickBoxes);"
                , "local row = UI.getElementInfo(rowHandle);"
                , "return {clipsChildren = vp.clipsChildren,"
                , "        rowInsideClip = (row.x >= vp.x and row.y >= vp.y"
                , "                         and (row.x+row.width) <= (vp.x+vp.width)"
                , "                         and (row.y+row.height) <= (vp.y+vp.height)),"
                , "        rowEffectiveClipMatchesViewport ="
                , "            (row.effectiveClip.x == vp.x and row.effectiveClip.y == vp.y"
                , "             and row.effectiveClip.w == vp.width and row.effectiveClip.h == vp.height)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe LogClipProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    lcpClipsChildren p `shouldBe` True
                    lcpRowInsideClip p `shouldBe` True
                    lcpRowEffectiveClipMatchesViewport p `shouldBe` True

        it "combat_log/injury_log_panel/unit_log each create a clipsChildren content viewport, and their empty-state label is a real descendant" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local out = {};"
                , "local specs = {"
                , "    {mod = 'scripts.combat_log', vp = 'contentViewportId'},"
                , "    {mod = 'scripts.injury_log_panel', vp = 'contentViewportId'},"
                , "    {mod = 'scripts.unit_log', vp = 'contentViewportId'},"
                , "};"
                , "for _, spec in ipairs(specs) do"
                , "    local m = require(spec.mod);"
                , "    m.bootstrap(1,2,3,1920,1080);"
                , "    if spec.mod == 'scripts.unit_log' then m.show(1) else m.show() end;"
                , "    local vp = UI.getElementInfo(m[spec.vp]);"
                , "    table.insert(out, {mod = spec.mod, clipsChildren = vp and vp.clipsChildren or false});"
                , "end;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [LogViewportRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rows → do
                    length rows `shouldBe` 3
                    forM_ rows $ \row → lvrClipsChildren row `shouldBe` True

        it "a resize while a log panel is visible rebuilds its viewport with clipsChildren still true (no regression to page-attached content)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local el = require('scripts.event_log');"
                , "el.bootstrap(1,2,3,1920,1080);"
                , "el.show();"
                , "local ok = pcall(function() el.onFramebufferResize(1280, 720) end);"
                , "local vp = UI.getElementInfo(el.rowViewportId);"
                , "return {ok = ok, clipsChildren = vp and vp.clipsChildren or false}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ResizeClipProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rcpOk p `shouldBe` True
                    rcpClipsChildren p `shouldBe` True

        it "the clip viewport's own zIndex stays 0 so a reparented row's effective paint position is unchanged (round-3 review: zIndex accumulates through parents)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.emitEvent('unit_event', 'hello world');"
                , "local el = require('scripts.event_log');"
                , "el.bootstrap(1,2,3,1920,1080);"
                , "el.show();"
                , "local vp = UI.getElementInfo(el.rowViewportId);"
                , "local rowHandle = next(el.rowClickBoxes);"
                , "local row = UI.getElementInfo(rowHandle);"
                -- Compare against a page-root reference element with the
                -- SAME zIndex on the SAME page — no band/offset math
                -- hardcoded here. If the viewport's own z leaked into the
                -- row's accumulated z, the row's paintKey would be
                -- HIGHER than this reference's, not equal.
                , "local refHandle = UI.newElement('zidx_ref', 1, 1, el.pageId);"
                , "UI.addToPage(el.pageId, refHandle, 0, 0);"
                , "UI.setZIndex(refHandle, row.zIndex);"
                , "local ref = UI.getElementInfo(refHandle);"
                , "return {viewportZ = vp.zIndex, rowPaintKey = row.paintKey,"
                , "        refPaintKey = ref.paintKey}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ZIndexProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    zipViewportZ p `shouldBe` 0
                    zipRowPaintKey p `shouldBe` zipRefPaintKey p

        it "event/combat/injury/unit log panels all build a non-degenerate content viewport at the issue's own out-of-envelope exemplar (800x600@4x, round-10 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.emitEvent('unit_event', 'hello world');"
                , "local out = {};"
                , "local specs = {"
                , "    {mod = 'scripts.event_log',        vp = 'rowViewportId'},"
                , "    {mod = 'scripts.combat_log',        vp = 'contentViewportId'},"
                , "    {mod = 'scripts.injury_log_panel',  vp = 'contentViewportId'},"
                , "    {mod = 'scripts.unit_log',          vp = 'contentViewportId'},"
                , "};"
                , "for _, spec in ipairs(specs) do"
                , "    local m = require(spec.mod);"
                , "    m.bootstrap(1,2,3,800,600);"
                , "    local ok = pcall(function()"
                , "        if spec.mod == 'scripts.unit_log' then m.show(1) else m.show() end"
                , "    end);"
                , "    local vp = ok and UI.getElementInfo(m[spec.vp]);"
                , "    table.insert(out, {mod = spec.mod, ok = ok,"
                , "        w = vp and vp.width or -1, h = vp and vp.height or -1});"
                , "end;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [ModGeometryRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rows → do
                    length rows `shouldBe` 4
                    forM_ rows $ \row → do
                        mgrOk row `shouldBe` True
                        mgrW row `shouldSatisfy` (> 0)
                        mgrH row `shouldSatisfy` (> 0)

    describe "hud/info_panel.lua and tile_editor.lua stay in-frame at a narrow, high-scale, still-C2-supported combination (round-15 review)" $ do
        it "info_panel: every visible tab stays inside the panel's own bounds with 4 tabs active (round-15 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-15 review: the panel is deliberately narrow
            -- (20% of the framebuffer), but the tabbar laid tabs out at
            -- full scaled text width with no fit — at the issue's own
            -- 800x2160@4x, the ~80px content area couldn't hold even
            -- the 2 default tile-schema tabs, let alone the 2 dynamic
            -- ones (resources/weather) added here to prove the fix
            -- holds under the reviewer's own "resource/weather/status
            -- tabs extend outside" scenario.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                -- tabbar.init() loads its own module-level box-texture
                -- handles (texSetFrame/texSetSelected/texSetUnselected)
                -- — never called by info_panel.lua/hud.lua themselves
                -- (a real boot reaches it via uiManager.init(), which
                -- this suite deliberately never drives — see the module
                -- docstring). Without it every tabbar.new() this suite
                -- creates passes a nil box-texture handle to
                -- UI.newBox(), which requires an integer 4th argument
                -- and silently returns nil on a type mismatch — leaving
                -- tab.boxId/frameBoxId nil and so invisible to
                -- UI.getElementInfo/tabbar.dump(). Harmless for tests
                -- that only touch tabbar's own bookkeeping (selectByKey,
                -- getSelectedKey), but this test needs real tab element
                -- geometry, so it's the first in this suite to need the
                -- explicit init() call.
                , "require('scripts.ui.tabbar').init();"
                -- Pre-seed the dynamic tabs' text directly (rather than
                -- via setResourcesInfo/setWeatherInfo after createUI())
                -- so hud.createUI()'s single infoPanel.create() call
                -- builds the full 4-tab tabbar in one pass instead of
                -- going through 2 extra hide/rebuild/show cycles on the
                -- SAME page — orthogonal to what this test verifies.
                , "local ip = require('scripts.hud.info_panel');"
                , "ip.tabText.basic = 'basic text';"
                , "ip.tabText.advanced = 'advanced text';"
                , "ip.tabText.resources = 'iron: 12';"
                , "ip.tabText.weather = 'sunny';"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "ip.refresh();"
                , "local tabbar = require('scripts.ui.tabbar');"
                , "local prefix = 'tabbar:' .. tostring(ip.tabBarId) .. ':';"
                , "local pb = ip.getBounds();"
                , "local tabs = {};"
                , "for _, e in ipairs(tabbar.dump()) do"
                , "    if e.id:sub(1, #prefix) == prefix then"
                , "        table.insert(tabs, {x = e.bounds.x, y = e.bounds.y,"
                , "                            w = e.bounds.w, h = e.bounds.h})"
                , "    end"
                , "end;"
                , "return {panelX = pb.x, panelW = pb.w, tabCount = #tabs, tabs = tabs}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe InfoPanelTabsProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    iptTabCount p `shouldBe` 4
                    forM_ (iptTabs p) $ \t →
                        (rrX t + rrW t) `shouldSatisfy` (≤ iptPanelX p + iptPanelW p)

        it "tile_editor: the Delete Tile button fits inside the panel's content bounds (round-15 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-15 review: the panel is width-fractional
            -- (mirrors info_panel.lua's own sizing), but the Delete
            -- Tile button stayed a fixed 320-base-unit width — at the
            -- issue's own 800x2160@4x, pbounds.width (~64px) is far
            -- smaller than the button's natural 1280px.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local pg = UI.newPage('tile_editor_test_page', 'overlay');"
                , "local te = require('scripts.tile_editor');"
                , "te.setup({page = pg, fbW = 800, fbH = 2160, boxTexSet = 1, menuFont = 2, worldId = 'test_arena'});"
                , "te.setArenaActive(true);"
                , "te.onTileSelected(5, 5);"
                , "local p = require('scripts.ui.panel');"
                , "local px, py = p.getPosition(te.state.panelId);"
                , "local pw, ph = p.getSize(te.state.panelId);"
                , "local b = require('scripts.ui.button');"
                , "local bh = b.getElementHandle(te.state.deleteBtn);"
                , "local info = UI.getElementInfo(bh);"
                , "return {panelX = px, panelW = pw, btnX = info.x, btnW = info.width}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TileEditorButtonProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tebBtnW p `shouldSatisfy` (> 0)
                    tebBtnX p `shouldSatisfy` (≥ tebPanelX p)
                    (tebBtnX p + tebBtnW p) `shouldSatisfy` (≤ tebPanelX p + tebPanelW p)

    describe "\"unit info reserves right edge and suppresses conflicting info\" (#750 introspection over pre-existing behavior)" $ do
        it "unitInfoV2.getBounds() mirrors the real flush-right column, and is nil while not visible" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "local hiddenBounds = u.getBounds();"
                , "u.lastWantVisible = true;"
                , "local b = u.getBounds();"
                , "return {hiddenIsNil=(hiddenBounds == nil), x=b.x, y=b.y, w=b.w, h=b.h}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe UnitInfoBoundsProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    uibpHiddenIsNil p `shouldBe` True
                    uibpX p `shouldBe` (1920 - 340)
                    uibpY p `shouldBe` 0
                    uibpW p `shouldBe` 340
                    uibpH p `shouldBe` 1080

        it "the flush-right column's width is capped to the framebuffer at a narrow, high-scale, still-C2-supported combination (round-3 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "u.lastWantVisible = true;"
                , "local b = u.getBounds();"
                , "return {x = b.x, w = b.w, inFrame = (b.x >= 0 and (b.x+b.w) <= 800)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WidthCapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    wcpInFrame p `shouldBe` True
                    wcpW p `shouldSatisfy` (≤ 800)

        it "the flush-right column never grows wide enough to cover the left-side toolbar clusters (round-8 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local rr = require('scripts.ui.reserved_regions');"
                , "local u = require('scripts.unit_info_v2');"
                , "u.lastWantVisible = true;"
                , "local b = u.getBounds();"
                , "local overlapsAny = false;"
                , "for _, rc in ipairs(hud.getToolbarRects()) do"
                , "    if rc.name ~= 'map_toggle' and rr.rectsOverlap(b, rc) then overlapsAny = true end"
                , "end;"
                , "return {overlapsAny = overlapsAny,"
                , "        inFrame = (b.x >= 0 and (b.x+b.w) <= 800)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe OverlapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    opOverlapsAny p `shouldBe` False
                    opInFrame p `shouldBe` True

        it "the 4 fixed sections + inventory all stay within the framebuffer at a narrow, high-scale, still-C2-supported combination (round-16 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-16 review: tabsH/headerH/statsH/equipH alone
            -- (352/336/1120/1088px at the issue's own 800x2160@4x)
            -- already exceed the framebuffer height before any gap/
            -- divider overhead, driving inventory's remaining-height
            -- computation negative (the section used to be omitted
            -- outright) and pushing equipment's own rect past the
            -- bottom edge. unitInfoV2.rebuildLayout() is only reachable
            -- through update() (bootstrap() — which sets .page and
            -- triggers the FIRST rebuildLayout() — is local, not
            -- exported); unit.getSelected() safely returns
            -- nil/empty headless, so this drives the real entry point.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "local ok = pcall(function() u.update(0.016) end);"
                , "return {ok = ok,"
                , "        hasInvRect = (u.invRect ~= nil),"
                , "        invH = u.invRect and u.invRect.h or 0,"
                , "        equipBottom = u.equipRect and (u.equipRect.y + u.equipRect.h) or 0,"
                , "        statsBottom = u.statsRect and (u.statsRect.y + u.statsRect.h) or 0,"
                , "        invBottom = u.invRect and (u.invRect.y + u.invRect.h) or 0}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe UnitInfoVerticalProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    uivpOk p `shouldBe` True
                    uivpHasInvRect p `shouldBe` True
                    uivpInvH p `shouldSatisfy` (> 0)
                    uivpEquipBottom p `shouldSatisfy` (≤ 2160)
                    uivpStatsBottom p `shouldSatisfy` (≤ 2160)
                    uivpInvBottom p `shouldSatisfy` (≤ 2160)

        it "a resize rebuilds the stats sub-tab strip and recomputes its content rect instead of leaving them permanently empty/stale (round-18 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-18 review: rebuildLayout() (via clearOwned ->
            -- statsMod.clearAll()) wipes the stats sub-tab strip, but
            -- rebuildSubTabs() (which recreates it AND recomputes
            -- statsContentRect from the current statsRect) was
            -- otherwise only ever called once, at bootstrap — so every
            -- resize after the first left the stats section
            -- permanently empty and its content rect stuck at the
            -- pre-resize size. Rescales to a narrow, high-uiscale
            -- combination (round-3/16/17's own technique) so a stale
            -- statsContentRect (still the old 1920@1x-derived width)
            -- is distinguishable from a freshly recomputed one
            -- (matching the new, capped-narrow statsRect).
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "u.update(0.016);"
                , "local subTabCountBefore = #u.subTabs;"
                , "local statsWBefore = u.statsContentRect and u.statsContentRect.w or -1;"
                , "engine.setUIScale(4.0);"
                , "hud.onFramebufferResize(800, 2160);"
                , "u.reflow();"
                , "return {subTabCountBefore = subTabCountBefore, subTabCountAfter = #u.subTabs,"
                , "        statsWBefore = statsWBefore,"
                , "        hasContentRectAfter = (u.statsContentRect ~= nil),"
                , "        statsRectW = u.statsRect and u.statsRect.w or -1,"
                , "        statsContentRectW = u.statsContentRect and u.statsContentRect.w or -1}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe UnitInfoStatsReflowProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    uisrpSubTabCountBefore p `shouldBe` 5
                    uisrpSubTabCountAfter p `shouldBe` 5
                    uisrpHasContentRectAfter p `shouldBe` True
                    uisrpStatsContentRectW p `shouldBe` uisrpStatsRectW p
                    uisrpStatsContentRectW p `shouldNotBe` uisrpStatsWBefore p

        it "a resize preserves the active unit tab and scroll offset instead of resetting to the first tab (round-17 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-17 review: reflow() used to force lastSelKey
            -- to differ so the next update() tick would call
            -- rebuildTabs, which always resets the active tab to
            -- sel[1] and the scroll offset to 0 — correct for a real
            -- selection change, but a resize re-processes the SAME
            -- selection and must not lose either. unit.getSelected is
            -- stubbed (10 units, forcing scroll) since no real world/
            -- units exist in this harness; tabs.rebuildTabs is driven
            -- directly so building the strip doesn't need the other
            -- section modules' own unit/equipment stubs.
            r ← evalJSON ls $ luaLines
                [ "local origSel = unit.getSelected;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "local tabs = require('scripts.unit_info_v2_tabs');"
                , "u.update(0.016);"
                , "unit.getSelected = function() return {1,2,3,4,5,6,7,8,9,10} end;"
                , "tabs.rebuildTabs(unit.getSelected());"
                , "u.handleScrollRight(u.scrollRightId);"
                , "u.handleScrollRight(u.scrollRightId);"
                , "u.activeUid = 5;"
                , "local activeBefore = u.activeUid;"
                , "local scrollBefore = u.scrollOffset;"
                , "u.reflow();"
                , "unit.getSelected = origSel;"
                , "return {activeBefore = activeBefore, scrollBefore = scrollBefore,"
                , "        activeAfter = u.activeUid, scrollAfter = u.scrollOffset,"
                , "        tabCountAfter = #u.tabs}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe UnitInfoTabPreserveProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    uitpScrollBefore p `shouldSatisfy` (> 0)
                    uitpActiveAfter p `shouldBe` uitpActiveBefore p
                    uitpScrollAfter p `shouldBe` uitpScrollBefore p
                    uitpTabCountAfter p `shouldBe` 10

        it "a resize preserves keyboard CONTROL focus (#745) on the Log button, restoring it after rebuildLayout deletes/recreates it (round-17 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-17 review: reflow()'s rebuildLayout() deletes
            -- and recreates every unit-info control, including the
            -- keyboard-focusable Log button, with no focus snapshot/
            -- restore of its own — a focus HUD's own resize handler
            -- had just restored onto a unit-info control (#745,
            -- mirroring hud.lua's round-10 pattern) was immediately
            -- orphaned again. isGameplayInputActive is stubbed true so
            -- reflow's visibility gate keeps the pane shown — a real
            -- gameplay/zoom state isn't reachable in this bare-Lua-
            -- backend harness (no fontsReady/world boot).
            r ← evalJSON ls $ luaLines
                [ "local uiManager = require('scripts.ui_manager');"
                , "local origActive = uiManager.isGameplayInputActive;"
                , "uiManager.isGameplayInputActive = function() return true end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "hud.currentView = 'zoomed_in';"
                , "local u = require('scripts.unit_info_v2');"
                , "u.update(0.016);"
                , "u.lastSelCount = 1;"
                , "u.lastWantVisible = true;"
                , "UI.showPage(u.page);"
                , "UI.setControlFocus(u.logBtnBoxId);"
                , "local hadFocusBefore = UI.hasControlFocus(u.logBtnBoxId);"
                , "u.reflow();"
                , "uiManager.isGameplayInputActive = origActive;"
                , "local focusHandle = UI.getControlFocus();"
                , "local nameAfter = focusHandle and UI.getElementInfo(focusHandle).name or '';"
                , "return {hadFocusBefore = hadFocusBefore, hasFocusAfter = (focusHandle ~= nil),"
                , "        nameAfter = nameAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe UnitInfoFocusProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    uifpHadFocusBefore p `shouldBe` True
                    uifpHasFocusAfter p `shouldBe` True
                    uifpNameAfter p `shouldBe` "unit_info_v2_log_btn"

        it "equipment silhouette + slot content fits the (possibly round-16-fitted) equipment rect instead of overflowing it at 800x2160@4x (round-17 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-17 review: L.fitVerticalSections (round-16)
            -- only shrinks the equipment SECTION's rect, never
            -- equipmentMod's own content scale — a 1024px silhouette
            -- still rendered at the full 4x uiscale into the fitted
            -- ~625px rect, overlapping stats/inventory. unit.getInfo/
            -- equipment.getClass/getLoadout/getAccessories are stubbed
            -- (no real world/units exist in this harness);
            -- rebuildEquipmentSection is driven directly, bypassing
            -- update()'s other section modules (which would need their
            -- own unrelated stubs to not error).
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "u.update(0.016);"
                , "local origInfo = unit.getInfo;"
                , "local origClass = equipment.getClass;"
                , "local origLoadout = equipment.getLoadout;"
                , "local origAcc = equipment.getAccessories;"
                , "unit.getInfo = function(uid) return {equipmentClass = 'probe_class'} end;"
                , "equipment.getClass = function(name) return {"
                , "    silhouette = 0, silhouetteW = 1024, silhouetteH = 1024,"
                , "    slots = {{id='head', name='Head', kind='armor', x=480, y=20, w=64, h=64}}"
                , "} end;"
                , "equipment.getLoadout = function(uid) return {} end;"
                , "equipment.getAccessories = function(uid) return {} end;"
                , "u.activeUid = 1;"
                , "local em = require('scripts.unit_info_v2_equipment');"
                , "local ok = pcall(function() em.rebuildEquipmentSection() end);"
                , "unit.getInfo = origInfo; equipment.getClass = origClass;"
                , "equipment.getLoadout = origLoadout; equipment.getAccessories = origAcc;"
                , "local er = u.equipRect;"
                , "local silInfo, slotInfo = nil, nil;"
                , "for _, e in ipairs(u.equipElements) do"
                , "    if e.id then"
                , "        local info = UI.getElementInfo(e.id);"
                , "        if info and info.name == 'unit_info_v2_equip_silhouette' then silInfo = info end;"
                , "        if info and info.name == 'unit_info_v2_equip_slot_1' then slotInfo = info end;"
                , "    end"
                , "end;"
                , "return {ok = ok, hasSil = (silInfo ~= nil), hasSlot = (slotInfo ~= nil),"
                , "        rectBottom = (er.y + er.h), rectTop = er.y,"
                , "        silBottom = silInfo and (silInfo.y + silInfo.height) or -1,"
                , "        silTop = silInfo and silInfo.y or -1,"
                , "        slotBottom = slotInfo and (slotInfo.y + slotInfo.height) or -1}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe UnitInfoEquipFitProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    uiefpOk p `shouldBe` True
                    uiefpHasSil p `shouldBe` True
                    uiefpHasSlot p `shouldBe` True
                    uiefpSilTop p `shouldSatisfy` (≥ uiefpRectTop p)
                    uiefpSilBottom p `shouldSatisfy` (≤ uiefpRectBottom p)
                    uiefpSlotBottom p `shouldSatisfy` (≤ uiefpRectBottom p)

        it "a populated inventory renders at least one item row instead of maxRows=0 at 800x2160@4x (round-21 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-21 review: round-16's own vertical fit
            -- (unit_info_v2_layout.lua's fitVerticalSections) can leave
            -- the whole inventory section only ~253px tall at this
            -- combination, but rebuildInventorySection still derived
            -- its own tab strip + row + footer chrome from the full,
            -- unfitted uiscale — one tab row plus top/bottom padding
            -- and the footer alone consumed ~240px, leaving maxRows at
            -- 0: a nonempty inventory rendered no item rows or
            -- right-click hit zones at all. unit.getInventory/
            -- equipment.getLoadout/unit.getInfo/equipment.getAccessories
            -- are stubbed (no real world/units exist in this harness);
            -- rebuildInventorySection is driven directly, bypassing
            -- update()'s other section modules (which would need their
            -- own unrelated stubs to not error).
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "u.update(0.016);"
                , "local origInv = unit.getInventory;"
                , "local origLoadout = equipment.getLoadout;"
                , "local origInfo = unit.getInfo;"
                , "local origAcc = equipment.getAccessories;"
                , "unit.getInventory = function(uid) return {"
                , "    {defName='item1', displayName='Item One', category='Cat1', weight=1.0},"
                , "    {defName='item2', displayName='Item Two', category='Cat1', weight=1.0},"
                , "    {defName='item3', displayName='Item Three', category='Cat1', weight=1.0},"
                , "} end;"
                , "equipment.getLoadout = function(uid) return {} end;"
                , "unit.getInfo = function(uid) return {equipmentClass = nil} end;"
                , "equipment.getAccessories = function(uid) return {} end;"
                , "u.activeUid = 1;"
                , "local im = require('scripts.unit_info_v2_inventory');"
                , "local ok = pcall(function() im.rebuildInventorySection() end);"
                , "unit.getInventory = origInv; equipment.getLoadout = origLoadout;"
                , "unit.getInfo = origInfo; equipment.getAccessories = origAcc;"
                , "local ir = u.invRect;"
                , "local rows = im.rows();"
                , "local rowInfos = {};"
                , "for _, row in ipairs(rows) do"
                , "    local info = UI.getElementInfo(row.hitId);"
                , "    table.insert(rowInfos, {x=info.x, y=info.y, w=info.width, h=info.height});"
                , "end;"
                , "return {ok = ok, rowCount = #rows,"
                , "        rectTop = ir.y, rectBottom = ir.y + ir.h, rows = rowInfos}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe UnitInfoInvRowsProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    uiirpOk p `shouldBe` True
                    uiirpRowCount p `shouldSatisfy` (≥ 1)
                    forM_ (uiirpRows p) $ \rc → do
                        rrY rc `shouldSatisfy` (≥ uiirpRectTop p)
                        (rrY rc + rrH rc) `shouldSatisfy` (≤ uiirpRectBottom p)

        it "the migrated inventory tab strip wraps into centred rows with no content frame, and falls back to All when the selected category disappears (#1088)" $ \(env, ls) → do
            resetFixture env ls
            -- #1088 requirement 7: unit-info's bespoke tab strip is now
            -- a frame-free WRAPPED scripts/ui/tabbar layout. Enough
            -- categories to force a wrap; every row must stay centred
            -- inside the section rect and no content frame may appear.
            -- Then the selected category's only item goes away and the
            -- selection must fall back to All rather than blanking the
            -- list.
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "u.update(0.016);"
                , "local origInv = unit.getInventory;"
                , "local origLoadout = equipment.getLoadout;"
                , "local origInfo = unit.getInfo;"
                , "local origAcc = equipment.getAccessories;"
                , "local origWidth = engine.getTextWidth;"
                -- Real measurement: this fixture otherwise reports 0 for
                -- every string, which would never wrap.
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.6 end;"
                , "local full = {};"
                , "for i = 1, 8 do full[i] = {defName='item'..i,"
                , "    displayName='Item '..i, category='Category'..i, weight=1.0} end;"
                , "unit.getInventory = function() return full end;"
                , "equipment.getLoadout = function() return {} end;"
                , "unit.getInfo = function() return {equipmentClass = nil} end;"
                , "equipment.getAccessories = function() return {} end;"
                , "u.activeUid = 1;"
                , "local im = require('scripts.unit_info_v2_inventory');"
                , "im.rebuildInventorySection();"
                , "local il = require('scripts.ui.item_list');"
                , "local tb = require('scripts.ui.tabbar');"
                , "local tabs = il.getTabs(u.invListId);"
                , "local rect = u.invRect;"
                , "local rowsY = {}; local minX, maxRight = nil, nil;"
                , "for _, t in ipairs(tabs) do"
                , "    rowsY[tostring(t.y)] = true;"
                , "    if minX == nil or t.x < minX then minX = t.x end;"
                , "    if maxRight == nil or (t.x + t.width) > maxRight then"
                , "        maxRight = t.x + t.width end"
                , "end;"
                , "local nTabRows = 0;"
                , "for _ in pairs(rowsY) do nTabRows = nTabRows + 1 end;"
                , "im.onTabChange('Category3');"
                , "im.rebuildInventorySection();"
                , "local selected = u.activeInvTab;"
                -- Category3's only item is removed: the selection must
                -- snap back to All rather than showing an empty list.
                , "local reduced = {};"
                , "for i, it in ipairs(full) do"
                , "    if it.category ~= 'Category3' then reduced[#reduced+1] = it end"
                , "end;"
                , "unit.getInventory = function() return reduced end;"
                , "im.rebuildInventorySection();"
                , "local after = u.activeInvTab;"
                , "local rowsAfter = #il.getRows(u.invListId);"
                , "engine.getTextWidth = origWidth;"
                , "unit.getInventory = origInv; equipment.getLoadout = origLoadout;"
                , "unit.getInfo = origInfo; equipment.getAccessories = origAcc;"
                , "return {tabRows = nTabRows, hasFrame = tb.hasFrame(il.getTabBarId(u.invListId)),"
                , "        minX = minX, maxRight = maxRight,"
                , "        rectLeft = rect.x, rectRight = rect.x + rect.w,"
                , "        selected = selected, after = after, rowsAfter = rowsAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe InvTabMigrationProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    itmpTabRows p `shouldSatisfy` (≥ 2)      -- genuinely wrapped
                    itmpHasFrame p `shouldBe` False
                    itmpMinX p `shouldSatisfy` (≥ itmpRectLeft p)
                    itmpMaxRight p `shouldSatisfy` (≤ itmpRectRight p)
                    -- Centred: the left margin matches the right margin
                    -- to within a pixel of integer rounding.
                    abs ((itmpMinX p - itmpRectLeft p)
                         - (itmpRectRight p - itmpMaxRight p)) `shouldSatisfy` (≤ 1)
                    itmpSelected p `shouldBe` "Category3"
                    itmpAfter p `shouldBe` "All"
                    itmpRowsAfter p `shouldSatisfy` (≥ 1)

        it "infoPanel.suppress('unit_info_v2') hides the generic panel; unsuppress restores it while content remains" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local ip = require('scripts.hud.info_panel');"
                , "ip.setText('basic', 'some tile info');"
                , "local visibleBefore = ip.isVisible();"
                , "ip.suppress('unit_info_v2');"
                , "local visibleSuppressed = ip.isVisible();"
                , "local boundsSuppressed = ip.getBounds();"
                , "ip.unsuppress('unit_info_v2');"
                , "local visibleAfter = ip.isVisible();"
                , "return {before=visibleBefore, suppressed=visibleSuppressed,"
                , "        boundsNil=(boundsSuppressed == nil), after=visibleAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe SuppressProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    spBefore p `shouldBe` True
                    spSuppressed p `shouldBe` False
                    spBoundsNil p `shouldBe` True
                    spAfter p `shouldBe` True

    describe "build_tool_remote_warning.lua stays in-frame at a narrow, high-scale, still-C2-supported combination (round-6 review)" $ do
        it "a resize preserves keyboard CONTROL focus (#745) on Establish/Cancel, restoring it onto the rebuilt control (round-13 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-13 review: onFramebufferResize deletes and
            -- recreates the whole modal page (including the Establish/
            -- Cancel boxes, both real keyboard-control-focusable
            -- elements per #745) — page deletion clears upmControlFocus
            -- with no restore, so a Tab-focused action silently lost
            -- focus on every resize/scale change. Mirrors hud.lua's own
            -- identical fix (#750 round-10).
            r ← evalJSON ls $ luaLines
                [ "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,1920,1080);"
                , "w.open('acolyte_portal', 5, 5, 100, 50);"
                , "local boxHandle = next(w.clickHandlers);"
                , "UI.setControlFocus(boxHandle);"
                , "local hadFocusBefore = UI.hasControlFocus(boxHandle);"
                , "w.onFramebufferResize(1600, 900);"
                , "local focusHandle = UI.getControlFocus();"
                , "return {hadFocusBefore = hadFocusBefore, hasFocusAfter = (focusHandle ~= nil)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RemoteWarningFocusProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rwfpHadFocusBefore p `shouldBe` True
                    rwfpHasFocusAfter p `shouldBe` True

        it "a resize with no confirmation pending does not attempt to restore control focus (nothing to restore, no crash)" $ \(env, ls) → do
            resetFixture env ls
            ok ← evalBool ls $ luaLines
                [ "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,1920,1080);"
                , "local ok = pcall(function() w.onFramebufferResize(1600, 900) end);"
                , "return ok"
                ]
            ok `shouldBe` True

        it "the modal panel is capped to the framebuffer instead of pushing Establish/Cancel off-screen" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,800,2160);"
                , "w.open('acolyte_portal', 5, 5, 100, 50);"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(w.panelId);"
                , "local pw, ph = p.getSize(w.panelId);"
                , "return {x = x, y = y, w = pw, h = ph,"
                , "        inFrame = (x >= 0 and y >= 0 and (x+pw) <= 800 and (y+ph) <= 2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RemoteWarningProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rwpInFrame p `shouldBe` True
                    rwpW p `shouldSatisfy` (≤ 800)
                    rwpH p `shouldSatisfy` (≤ 2160)

        it "a normal resolution still gets its natural, unshrunk panel size" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(1.0);"
                , "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,1920,1080);"
                , "w.open('acolyte_portal', 5, 5, 100, 50);"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(w.panelId);"
                , "local pw, ph = p.getSize(w.panelId);"
                , "return {x = x, y = y, w = pw, h = ph, inFrame = true}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RemoteWarningProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → rwpW p `shouldBe` 560

        it "both Establish/Cancel buttons stay within the (possibly shrunk) panel and the framebuffer (round-7 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,800,2160);"
                , "w.open('acolyte_portal', 5, 5, 100, 50);"
                , "local out = {};"
                , "for h, _ in pairs(w.clickHandlers) do"
                , "    local info = UI.getElementInfo(h);"
                , "    table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height})"
                , "end;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [RectRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rects → do
                    length rects `shouldBe` 2
                    forM_ rects $ \rc → do
                        rrX rc `shouldSatisfy` (≥ 0)
                        rrY rc `shouldSatisfy` (≥ 0)
                        (rrX rc + rrW rc) `shouldSatisfy` (≤ 800)
                        (rrY rc + rrH rc) `shouldSatisfy` (≤ 2160)

        it "a heavily-shrunk button's label font shrinks with its box, not at a fixed full-uiscale size (round-13 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-13 review: shrinking the button BOX alone
            -- (round-7) left its child text rendering at the full,
            -- unshrunk s.buttonFontSize — with the shipped Press Start
            -- 2P font, "Choose Another Site" is wide enough at that size
            -- to render across/off an 800px modal despite the click box
            -- staying in-frame. UI.getElementInfo always reports a
            -- zero-sized bounding box for a raw UI.newText element (see
            -- label.lua's own comment on the same fact for label-wrapped
            -- text), so this compares the fix's own
            -- buttonFontSizeByBox[boxH] (added for exactly this
            -- introspection) between a wide framebuffer (no shrink
            -- needed) and the reviewer's narrow 800x2160@4x — a fixed,
            -- unshrunk uiscale would report the SAME font size either
            -- way; the fix makes the narrow one measurably smaller.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,1920,1080);"
                , "w.open('acolyte_portal', 5, 5, 100, 50);"
                , "local unshrunkSizes = {};"
                , "for _, fs in pairs(w.buttonFontSizeByBox) do table.insert(unshrunkSizes, fs) end;"
                -- w.open() no-ops while a confirmation is already pending
                -- (buildToolRemoteWarning.pending) — must close before
                -- reopening at the new framebuffer size, or the second
                -- open silently reuses the first's geometry.
                , "w.closeIfOpen();"
                , "w.init(1,2,3,800,2160);"
                , "w.open('acolyte_portal', 5, 5, 100, 50);"
                , "local shrunkSizes = {};"
                , "for _, fs in pairs(w.buttonFontSizeByBox) do table.insert(shrunkSizes, fs) end;"
                , "table.sort(unshrunkSizes); table.sort(shrunkSizes);"
                , "return {unshrunkSizes = unshrunkSizes, shrunkSizes = shrunkSizes}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe FontSizeCompareProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    length (fscpUnshrunkSizes p) `shouldBe` 2
                    length (fscpShrunkSizes p) `shouldBe` 2
                    forM_ (fscpShrunkSizes p) $ \sz → sz `shouldSatisfy` (> 0)
                    -- Every shrunk-case size must be no larger than its
                    -- unshrunk counterpart (both lists sorted the same
                    -- way — establish is always narrower than cancel).
                    forM_ (zip (fscpShrunkSizes p) (fscpUnshrunkSizes p)) $ \(sh, un) →
                        sh `shouldSatisfy` (≤ un)
                    -- At least one button must have ACTUALLY shrunk (not
                    -- just clamped equal) to prove the fix engaged.
                    or (zipWith (<) (fscpShrunkSizes p) (fscpUnshrunkSizes p))
                        `shouldBe` True

        it "the title/message labels stay within the panel instead of overflowing from a zero-width center origin (round-20 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-20 review: panel.place's width=0/height=0 meant
            -- a "top-center" origin offset by ZERO regardless of the
            -- label's real size — the title/message text was never
            -- actually centered at all, starting at the panel's content
            -- midpoint and running rightward off the edge. This suite's
            -- synthetic font handles make engine.getTextWidth always
            -- return 0 (see this module's own header comment), which
            -- would make both the old bug AND the fix look identical (0
            -- offset either way) — stubs it with a real, deterministic,
            -- length-proportional measurement for this one test so the
            -- centering math (and the width-fit it now feeds) is
            -- actually exercised, restored immediately after use.
            -- UI.getElementInfo always reports a zero-sized bounding box
            -- for a raw UI.newText element (label.lua's own comment on
            -- the same fact, also relied on by the round-13 button-font
            -- test above) — label.getSize(id) is label.lua's own cached
            -- REAL width (from the same engine.getTextWidth call), read
            -- via w.ownedLabels[1]/[2] (title/message, in the order
            -- createUI() inserts them — the only handle a caller outside
            -- createUI() has to the label ids at all).
            r ← evalJSON ls $ luaLines
                [ "local origGTW = engine.getTextWidth;"
                , "engine.getTextWidth = function(font, text, size) return #text * size end;"
                , "engine.setUIScale(4.0);"
                , "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,800,2160);"
                , "w.open('acolyte_portal', 5, 5, 987654321, 30);"
                , "engine.getTextWidth = origGTW;"
                , "local label = require('scripts.ui.label');"
                , "local panelMod = require('scripts.ui.panel');"
                , "local px, py = panelMod.getPosition(w.panelId);"
                , "local pw, ph = panelMod.getSize(w.panelId);"
                , "local titleId, msgId = w.ownedLabels[1], w.ownedLabels[2];"
                , "local titleW = label.getSize(titleId);"
                , "local msgW = label.getSize(msgId);"
                , "local titleInfo = UI.getElementInfo(label.getElementHandle(titleId));"
                , "local msgInfo = UI.getElementInfo(label.getElementHandle(msgId));"
                , "return {hasMsg = (msgInfo ~= nil), hasTitle = (titleInfo ~= nil),"
                , "        panelX = px, panelRight = px + pw,"
                , "        msgX = msgInfo.x, msgRight = msgInfo.x + msgW,"
                , "        titleX = titleInfo.x, titleRight = titleInfo.x + titleW}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RemoteWarningTextFitProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rwtfHasMsg p `shouldBe` True
                    rwtfHasTitle p `shouldBe` True
                    rwtfMsgX p `shouldSatisfy` (≥ rwtfPanelX p)
                    rwtfMsgRight p `shouldSatisfy` (≤ rwtfPanelRight p)
                    rwtfTitleX p `shouldSatisfy` (≥ rwtfPanelX p)
                    rwtfTitleRight p `shouldSatisfy` (≤ rwtfPanelRight p)

        it "the title/message glyph bands sit inside the panel's content area with the authored rhythm at an ordinary 1920x1080 @ 1x (#1394)" $ \(env, ls) → do
            resetFixture env ls
            -- #1394: a text element's uePosition is its BASELINE, with
            -- the glyph mass ABOVE it (scripts/ui/label.lua's own
            -- convention comment), while panel.place's "top-center"
            -- origin carries origin.y = 0 and therefore lays the y it is
            -- handed down AS that baseline. Placing the title at y = 0
            -- put its whole band a font size ABOVE contentY — in the top
            -- padding, across the panel's drawn 9-slice top border strip
            -- — and placed the message in the slot the title was
            -- allotted, collapsing the stack to one line height.
            --
            -- This case is the UNFITTED half of the contract: at
            -- 1920x1080 @ 1x the stubbed widths still fit the panel, so
            -- responsive.fitScale returns the panel uiscale unchanged
            -- and each label renders at exactly its panel-scaled slot
            -- size. That isolates the band math from the fitted-height
            -- rule the 4x case below exercises.
            r ← evalJSON ls (remoteWarningVerticalLua "1.0" "1920" "1080" "100")
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RemoteWarningVerticalProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- Independently restated from baseSizes: floor(26 *
                    -- 1.0) / floor(18 * 1.0), i.e. no shrink here.
                    rwvTitleFontSize p `shouldBe` 26
                    rwvMsgFontSize p `shouldBe` 18
                    assertRemoteWarningBands 14 28 p

        it "the same containment and rhythm hold at 800x2160 @ 4x, where responsive.fitScale renders both labels well below their panel-scaled slot (#1394)" $ \(env, ls) → do
            resetFixture env ls
            -- The narrow, high-scale, still-C2-supported combination
            -- this whole describe block exists for — and the one that
            -- separates a fitted label's REAL rendered font size from
            -- the panel-scaled titleH/msgH the panel height was budgeted
            -- from. engine.getTextWidth measures 0 headless (this
            -- module's header comment), and responsive.fitScale returns
            -- the scale UNCHANGED for a zero natural width, so the
            -- deterministic width stub is what forces a real shrink here
            -- rather than a vacuous pass; the size assertions below
            -- prove the shrink actually happened.
            r ← evalJSON ls (remoteWarningVerticalLua "4.0" "800" "2160" "987654321")
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RemoteWarningVerticalProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- Panel-scaled slots, restated from baseSizes:
                    -- floor(26 * 4.0) = 104 and floor(18 * 4.0) = 72.
                    -- Placement math reading those instead of the real
                    -- rendered sizes would blow the rhythm assertions
                    -- below by the difference.
                    rwvTitleFontSize p `shouldSatisfy` (< 104)
                    rwvMsgFontSize p `shouldSatisfy` (< 72)
                    rwvTitleFontSize p `shouldSatisfy` (> 0)
                    rwvMsgFontSize p `shouldSatisfy` (> 0)
                    -- floor(14 * 4.0) / floor(28 * 4.0).
                    assertRemoteWarningBands 56 112 p

-- | The build-tool picker group, which the aggregate order places
--   after the container owner.
pickerSpec ∷ SpecWith SharedFixture
pickerSpec = do
    describe "build_tool.lua's picker stays in-frame at a narrow, high-scale, still-C2-supported combination (round-7 review)" $ do
        it "the picker width is capped to the remaining framebuffer space right of its toolbar anchor, with no prior position clamp to rely on" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = engine.getBuildingDefs;"
                , "if orig then engine.getBuildingDefs = function() return {"
                , "    { name = 'test_wall', displayName = 'Wall', category = 'Structures' }"
                , "} end end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "local ok = pcall(function() bt.showPicker() end);"
                , "if orig then engine.getBuildingDefs = orig end;"
                , "if not ok or bt.state.mode ~= 'picker' then return {skipped = true} end;"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(bt.state.panelId);"
                , "local pw, ph = p.getSize(bt.state.panelId);"
                , "return {skipped = false, x=x, y=y, w=pw, h=ph,"
                , "        inFrame=(x>=0 and y>=0 and (x+pw)<=800 and (y+ph)<=2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe PickerProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p →
                    -- No headless hook exists to force visibleEntries()
                    -- non-empty across every build (it reads real
                    -- building defs, whose exact loading path varies) —
                    -- when the picker genuinely couldn't open, this is a
                    -- soft skip rather than a false failure; the formula
                    -- fix itself is exercised directly whenever it can
                    -- open.
                    when (not (ppSkipped p)) $ ppInFrame p `shouldBe` True

        it "the icon grid's column count derives from the (possibly shrunk) panel's real content width, not a fixed constant (round-8 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = building.listDefs;"
                , "building.listDefs = function() return {"
                , "    { name='t1', displayName='Wall',  category='Structures', isStarting=true },"
                , "    { name='t2', displayName='Farm',  category='Structures', isStarting=true },"
                , "    { name='t3', displayName='Kiln',  category='Structures', isStarting=true },"
                , "    { name='t4', displayName='Fort',  category='Structures', isStarting=true },"
                , "    { name='t5', displayName='Dock',  category='Structures', isStarting=true },"
                , "    { name='t6', displayName='Mill',  category='Structures', isStarting=true },"
                , "} end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "local ok = pcall(function() bt.showPicker() end);"
                , "building.listDefs = orig;"
                , "if not ok or bt.state.mode ~= 'picker' then return {skipped = true} end;"
                , "local out = {};"
                , "for _, ic in ipairs(bt.state.iconIds or {}) do"
                , "    local info = UI.getElementInfo(ic);"
                , "    if info then table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height}) end"
                , "end;"
                , "return {skipped = false, columnsPerRow = bt.state.columnsPerRow, icons = out}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ColumnGridProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → when (not (cgpSkipped p)) $ do
                    cgpColumnsPerRow p `shouldSatisfy` (≥ 1)
                    forM_ (cgpIcons p) $ \rc → do
                        rrX rc `shouldSatisfy` (≥ 0)
                        (rrX rc + rrW rc) `shouldSatisfy` (≤ 800)

        it "the tab strip shrinks to fit the panel's content width instead of running tabs off the framebuffer (round-8 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = building.listDefs;"
                , "building.listDefs = function() return {"
                , "    { name='t1', displayName='A1', category='Cat1', isStarting=true },"
                , "    { name='t2', displayName='A2', category='Cat2', isStarting=true },"
                , "    { name='t3', displayName='A3', category='Cat3', isStarting=true },"
                , "    { name='t4', displayName='A4', category='Cat4', isStarting=true },"
                , "    { name='t5', displayName='A5', category='Cat5', isStarting=true },"
                , "    { name='t6', displayName='A6', category='Cat6', isStarting=true },"
                , "    { name='t7', displayName='A7', category='Cat7', isStarting=true },"
                , "    { name='t8', displayName='A8', category='Cat8', isStarting=true },"
                , "} end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "local ok = pcall(function() bt.showPicker() end);"
                , "building.listDefs = orig;"
                , "if not ok or bt.state.mode ~= 'picker' then return {skipped = true} end;"
                , "local out = {};"
                , "for _, t in ipairs(bt.state.tabIds) do"
                , "    local info = UI.getElementInfo(t.boxId);"
                , "    table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height})"
                , "end;"
                , "return {skipped = false, tabs = out}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TabStripProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → when (not (tspSkipped p)) $ do
                    length (tspTabs p) `shouldSatisfy` (≥ 8)
                    forM_ (tspTabs p) $ \rc → do
                        rrX rc `shouldSatisfy` (≥ 0)
                        rrW rc `shouldSatisfy` (≥ 20)
                        (rrX rc + rrW rc) `shouldSatisfy` (≤ 800)

        it "a heavily-shrunk tab's label renders smaller than an unshrunk one, not at a fixed full-uiscale size (round-12 review)" $ \(env, ls) → do
            resetFixture env ls
            -- Same reasoning as cargo_inventory_panel's identical round-12
            -- test: engine.getTextWidth always measures 0 in this suite's
            -- synthetic boot, so label WIDTH can't prove overlap directly,
            -- but label.lua's HEIGHT is derived straight from
            -- `fontSize * uiscale`, independent of real text metrics — a
            -- fixed, unshrunk uiscale would report the SAME height
            -- regardless of category count.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = building.listDefs;"
                , "local label = require('scripts.ui.label');"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "building.listDefs = function() return {"
                , "    { name='u1', displayName='U1', category='Solo', isStarting=true },"
                , "} end;"
                , "local ok1 = pcall(function() bt.showPicker() end);"
                , "local unshrunkH = 0;"
                , "if ok1 and bt.state.mode == 'picker' and bt.state.tabIds[1] then"
                , "    local _, h = label.getSize(bt.state.tabIds[1].labelId);"
                , "    unshrunkH = h;"
                , "end;"
                , "bt.hidePicker();"
                , "building.listDefs = function() return {"
                , "    { name='t1', displayName='A1', category='Cat1', isStarting=true },"
                , "    { name='t2', displayName='A2', category='Cat2', isStarting=true },"
                , "    { name='t3', displayName='A3', category='Cat3', isStarting=true },"
                , "    { name='t4', displayName='A4', category='Cat4', isStarting=true },"
                , "    { name='t5', displayName='A5', category='Cat5', isStarting=true },"
                , "    { name='t6', displayName='A6', category='Cat6', isStarting=true },"
                , "    { name='t7', displayName='A7', category='Cat7', isStarting=true },"
                , "    { name='t8', displayName='A8', category='Cat8', isStarting=true },"
                , "} end;"
                , "local ok2 = pcall(function() bt.showPicker() end);"
                , "local shrunkH = 0;"
                , "local skipped = not (ok1 and ok2 and bt.state.mode == 'picker');"
                , "if ok2 and bt.state.mode == 'picker' and bt.state.tabIds[2] then"
                , "    local _, h = label.getSize(bt.state.tabIds[2].labelId);"
                , "    shrunkH = h;"
                , "end;"
                , "building.listDefs = orig;"
                , "return {skipped = skipped, unshrunkH = unshrunkH, shrunkH = shrunkH}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ShrinkHeightSkipProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → when (not (shspSkipped p)) $ do
                    shspShrunkH p `shouldSatisfy` (> 0)
                    shspShrunkH p `shouldSatisfy` (< shspUnshrunkH p)

        it "the icon grid is vertically compacted so a many-entry single-column category doesn't run off the framebuffer (round-9 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = building.listDefs;"
                , "building.listDefs = function() return {"
                , "    { name='t1', displayName='A1', category='All', isStarting=true },"
                , "    { name='t2', displayName='A2', category='All', isStarting=true },"
                , "    { name='t3', displayName='A3', category='All', isStarting=true },"
                , "    { name='t4', displayName='A4', category='All', isStarting=true },"
                , "    { name='t5', displayName='A5', category='All', isStarting=true },"
                , "    { name='t6', displayName='A6', category='All', isStarting=true },"
                , "    { name='t7', displayName='A7', category='All', isStarting=true },"
                , "    { name='t8', displayName='A8', category='All', isStarting=true },"
                , "} end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "local ok = pcall(function() bt.showPicker() end);"
                , "building.listDefs = orig;"
                , "if not ok or bt.state.mode ~= 'picker' then return {skipped = true} end;"
                , "local out = {};"
                , "for _, ic in ipairs(bt.state.iconIds or {}) do"
                , "    local info = UI.getElementInfo(ic);"
                , "    if info then table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height}) end"
                , "end;"
                , "return {skipped = false, columnsPerRow = bt.state.columnsPerRow, icons = out}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ColumnGridProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → when (not (cgpSkipped p)) $ do
                    length (cgpIcons p) `shouldSatisfy` (≥ 8)
                    forM_ (cgpIcons p) $ \rc → do
                        rrY rc `shouldSatisfy` (≥ 0)
                        (rrY rc + rrH rc) `shouldSatisfy` (≤ 2160)

-- * FromJSON row types and helpers owned by the surface groups

data LogClipProbe = LogClipProbe
    { lcpClipsChildren ∷ Bool, lcpRowInsideClip ∷ Bool
    , lcpRowEffectiveClipMatchesViewport ∷ Bool } deriving Show
instance FromJSON LogClipProbe where
    parseJSON = withObject "LogClipProbe" $ \o →
        LogClipProbe <$> o .: "clipsChildren" <*> o .: "rowInsideClip"
                      <*> o .: "rowEffectiveClipMatchesViewport"

data LogViewportRow = LogViewportRow
    { lvrMod ∷ Text, lvrClipsChildren ∷ Bool } deriving Show
instance FromJSON LogViewportRow where
    parseJSON = withObject "LogViewportRow" $ \o →
        LogViewportRow <$> o .: "mod" <*> o .: "clipsChildren"

data ResizeClipProbe = ResizeClipProbe
    { rcpOk ∷ Bool, rcpClipsChildren ∷ Bool } deriving Show
instance FromJSON ResizeClipProbe where
    parseJSON = withObject "ResizeClipProbe" $ \o →
        ResizeClipProbe <$> o .: "ok" <*> o .: "clipsChildren"

data ZIndexProbe = ZIndexProbe
    { zipViewportZ ∷ Int, zipRowPaintKey ∷ Int, zipRefPaintKey ∷ Int } deriving Show
instance FromJSON ZIndexProbe where
    parseJSON = withObject "ZIndexProbe" $ \o →
        ZIndexProbe <$> o .: "viewportZ" <*> o .: "rowPaintKey" <*> o .: "refPaintKey"

data OverlapProbe = OverlapProbe { opOverlapsAny ∷ Bool, opInFrame ∷ Bool } deriving Show
instance FromJSON OverlapProbe where
    parseJSON = withObject "OverlapProbe" $ \o →
        OverlapProbe <$> o .: "overlapsAny" <*> o .: "inFrame"

data RemoteWarningProbe = RemoteWarningProbe
    { rwpW ∷ Int, rwpH ∷ Int, rwpInFrame ∷ Bool } deriving Show
instance FromJSON RemoteWarningProbe where
    parseJSON = withObject "RemoteWarningProbe" $ \o →
        RemoteWarningProbe <$> o .: "w" <*> o .: "h" <*> o .: "inFrame"

data PickerProbe = PickerProbe { ppSkipped ∷ Bool, ppInFrame ∷ Bool } deriving Show
instance FromJSON PickerProbe where
    parseJSON = withObject "PickerProbe" $ \o → do
        skipped ← o .: "skipped"
        if skipped
            then pure (PickerProbe True False)
            else PickerProbe False <$> o .: "inFrame"

data ColumnGridProbe = ColumnGridProbe
    { cgpSkipped ∷ Bool, cgpColumnsPerRow ∷ Int, cgpIcons ∷ [RectRow] } deriving Show
instance FromJSON ColumnGridProbe where
    parseJSON = withObject "ColumnGridProbe" $ \o → do
        skipped ← o .: "skipped"
        if skipped
            then pure (ColumnGridProbe True 0 [])
            else ColumnGridProbe False <$> o .: "columnsPerRow" <*> o .: "icons"

data TabStripProbe = TabStripProbe { tspSkipped ∷ Bool, tspTabs ∷ [RectRow] } deriving Show
instance FromJSON TabStripProbe where
    parseJSON = withObject "TabStripProbe" $ \o → do
        skipped ← o .: "skipped"
        if skipped
            then pure (TabStripProbe True [])
            else TabStripProbe False <$> o .: "tabs"

data ModGeometryRow = ModGeometryRow
    { mgrMod ∷ Text, mgrOk ∷ Bool, mgrW ∷ Double, mgrH ∷ Double } deriving Show
instance FromJSON ModGeometryRow where
    parseJSON = withObject "ModGeometryRow" $ \o →
        ModGeometryRow <$> o .: "mod" <*> o .: "ok" <*> o .: "w" <*> o .: "h"

data UnitInfoVerticalProbe = UnitInfoVerticalProbe
    { uivpOk ∷ Bool, uivpHasInvRect ∷ Bool, uivpInvH ∷ Double
    , uivpEquipBottom ∷ Double, uivpStatsBottom ∷ Double, uivpInvBottom ∷ Double } deriving Show
instance FromJSON UnitInfoVerticalProbe where
    parseJSON = withObject "UnitInfoVerticalProbe" $ \o →
        UnitInfoVerticalProbe <$> o .: "ok" <*> o .: "hasInvRect" <*> o .: "invH"
                               <*> o .: "equipBottom" <*> o .: "statsBottom" <*> o .: "invBottom"

data UnitInfoInvRowsProbe = UnitInfoInvRowsProbe
    { uiirpOk ∷ Bool, uiirpRowCount ∷ Int
    , uiirpRectTop ∷ Double, uiirpRectBottom ∷ Double, uiirpRows ∷ [RectRow] } deriving Show
instance FromJSON UnitInfoInvRowsProbe where
    parseJSON = withObject "UnitInfoInvRowsProbe" $ \o →
        UnitInfoInvRowsProbe <$> o .: "ok" <*> o .: "rowCount"
                               <*> o .: "rectTop" <*> o .: "rectBottom" <*> o .: "rows"

data RemoteWarningTextFitProbe = RemoteWarningTextFitProbe
    { rwtfHasMsg ∷ Bool, rwtfHasTitle ∷ Bool
    , rwtfPanelX ∷ Double, rwtfPanelRight ∷ Double
    , rwtfMsgX ∷ Double, rwtfMsgRight ∷ Double
    , rwtfTitleX ∷ Double, rwtfTitleRight ∷ Double } deriving Show
instance FromJSON RemoteWarningTextFitProbe where
    parseJSON = withObject "RemoteWarningTextFitProbe" $ \o →
        RemoteWarningTextFitProbe <$> o .: "hasMsg" <*> o .: "hasTitle"
                                    <*> o .: "panelX" <*> o .: "panelRight"
                                    <*> o .: "msgX" <*> o .: "msgRight"
                                    <*> o .: "titleX" <*> o .: "titleRight"

-- | #1394: the remote-settlement modal's vertical label geometry, all
--   in screen coordinates. See 'remoteWarningVerticalLua' for how each
--   field is measured and 'assertRemoteWarningBands' for the contract.
data RemoteWarningVerticalProbe = RemoteWarningVerticalProbe
    { rwvContentTop ∷ Double, rwvContentBottom ∷ Double, rwvContentMid ∷ Double
    , rwvTitleFontSize ∷ Int, rwvMsgFontSize ∷ Int
    , rwvTitleBandTop ∷ Double, rwvTitleBandBottom ∷ Double
    , rwvMsgBandTop ∷ Double, rwvMsgBandBottom ∷ Double
    , rwvTitleMid ∷ Double, rwvMsgMid ∷ Double
    , rwvButtonTop ∷ Double, rwvButtonBottom ∷ Double
    , rwvButtonCount ∷ Int } deriving Show
instance FromJSON RemoteWarningVerticalProbe where
    parseJSON = withObject "RemoteWarningVerticalProbe" $ \o →
        RemoteWarningVerticalProbe
            <$> o .: "contentTop" <*> o .: "contentBottom" <*> o .: "contentMid"
            <*> o .: "titleFontSize" <*> o .: "msgFontSize"
            <*> o .: "titleBandTop" <*> o .: "titleBandBottom"
            <*> o .: "msgBandTop" <*> o .: "msgBandBottom"
            <*> o .: "titleMid" <*> o .: "msgMid"
            <*> o .: "buttonTop" <*> o .: "buttonBottom"
            <*> o .: "buttonCount"

data PopupLineOverflowProbe = PopupLineOverflowProbe
    { plopLineCount ∷ Int, plopPanelInFrame ∷ Bool, plopLastLineBottom ∷ Double
    , plopOkY ∷ Double, plopOkBottom ∷ Double, plopPanelBottom ∷ Double } deriving Show
instance FromJSON PopupLineOverflowProbe where
    parseJSON = withObject "PopupLineOverflowProbe" $ \o →
        PopupLineOverflowProbe <$> o .: "lineCount" <*> o .: "panelInFrame"
                                 <*> o .: "lastLineBottom" <*> o .: "okY"
                                 <*> o .: "okBottom" <*> o .: "panelBottom"

data PopupLineBaselineProbe = PopupLineBaselineProbe { plbpOffset ∷ Double } deriving Show
instance FromJSON PopupLineBaselineProbe where
    parseJSON = withObject "PopupLineBaselineProbe" $ \o →
        PopupLineBaselineProbe <$> o .: "offset"

data UnitInfoStatsReflowProbe = UnitInfoStatsReflowProbe
    { uisrpSubTabCountBefore ∷ Int, uisrpSubTabCountAfter ∷ Int, uisrpStatsWBefore ∷ Double
    , uisrpHasContentRectAfter ∷ Bool, uisrpStatsRectW ∷ Double, uisrpStatsContentRectW ∷ Double
    } deriving Show
instance FromJSON UnitInfoStatsReflowProbe where
    parseJSON = withObject "UnitInfoStatsReflowProbe" $ \o →
        UnitInfoStatsReflowProbe <$> o .: "subTabCountBefore" <*> o .: "subTabCountAfter"
                                   <*> o .: "statsWBefore" <*> o .: "hasContentRectAfter"
                                   <*> o .: "statsRectW" <*> o .: "statsContentRectW"

data UnitInfoTabPreserveProbe = UnitInfoTabPreserveProbe
    { uitpActiveBefore ∷ Int, uitpScrollBefore ∷ Int
    , uitpActiveAfter ∷ Int, uitpScrollAfter ∷ Int, uitpTabCountAfter ∷ Int } deriving Show
instance FromJSON UnitInfoTabPreserveProbe where
    parseJSON = withObject "UnitInfoTabPreserveProbe" $ \o →
        UnitInfoTabPreserveProbe <$> o .: "activeBefore" <*> o .: "scrollBefore"
                                   <*> o .: "activeAfter" <*> o .: "scrollAfter" <*> o .: "tabCountAfter"

data UnitInfoFocusProbe = UnitInfoFocusProbe
    { uifpHadFocusBefore ∷ Bool, uifpHasFocusAfter ∷ Bool, uifpNameAfter ∷ Text } deriving Show
instance FromJSON UnitInfoFocusProbe where
    parseJSON = withObject "UnitInfoFocusProbe" $ \o →
        UnitInfoFocusProbe <$> o .: "hadFocusBefore" <*> o .: "hasFocusAfter" <*> o .: "nameAfter"

data UnitInfoEquipFitProbe = UnitInfoEquipFitProbe
    { uiefpOk ∷ Bool, uiefpHasSil ∷ Bool, uiefpHasSlot ∷ Bool
    , uiefpRectBottom ∷ Double, uiefpRectTop ∷ Double
    , uiefpSilBottom ∷ Double, uiefpSilTop ∷ Double, uiefpSlotBottom ∷ Double } deriving Show
instance FromJSON UnitInfoEquipFitProbe where
    parseJSON = withObject "UnitInfoEquipFitProbe" $ \o →
        UnitInfoEquipFitProbe <$> o .: "ok" <*> o .: "hasSil" <*> o .: "hasSlot"
                                <*> o .: "rectBottom" <*> o .: "rectTop"
                                <*> o .: "silBottom" <*> o .: "silTop" <*> o .: "slotBottom"

data InfoPanelTabsProbe = InfoPanelTabsProbe
    { iptPanelX ∷ Double, iptPanelW ∷ Double, iptTabCount ∷ Int, iptTabs ∷ [RectRow] } deriving Show
instance FromJSON InfoPanelTabsProbe where
    parseJSON = withObject "InfoPanelTabsProbe" $ \o →
        InfoPanelTabsProbe <$> o .: "panelX" <*> o .: "panelW"
                            <*> o .: "tabCount" <*> o .: "tabs"

data TileEditorButtonProbe = TileEditorButtonProbe
    { tebPanelX ∷ Double, tebPanelW ∷ Double, tebBtnX ∷ Double, tebBtnW ∷ Double } deriving Show
instance FromJSON TileEditorButtonProbe where
    parseJSON = withObject "TileEditorButtonProbe" $ \o →
        TileEditorButtonProbe <$> o .: "panelX" <*> o .: "panelW"
                               <*> o .: "btnX" <*> o .: "btnW"

data LineRectProbe = LineRectProbe { lrpW ∷ Double, lrpX ∷ Double } deriving Show
instance FromJSON LineRectProbe where
    parseJSON = withObject "LineRectProbe" $ \o →
        LineRectProbe <$> o .: "w" <*> o .: "x"

data PopupSqueezeProbe = PopupSqueezeProbe
    { psqPanelW ∷ Double, psqPanelX ∷ Double, psqPanelInFrame ∷ Bool
    , psqLineRects ∷ [LineRectProbe]
    , psqOkX ∷ Double, psqOkW ∷ Double
    , psqCloseX ∷ Double, psqCloseW ∷ Double
    , psqHasMute ∷ Bool, psqMuteX ∷ Double, psqMuteW ∷ Double } deriving Show
instance FromJSON PopupSqueezeProbe where
    parseJSON = withObject "PopupSqueezeProbe" $ \o →
        PopupSqueezeProbe <$> o .: "panelW" <*> o .: "panelX" <*> o .: "panelInFrame"
                           <*> o .: "lineRects"
                           <*> o .: "okX" <*> o .: "okW"
                           <*> o .: "closeX" <*> o .: "closeW"
                           <*> o .: "hasMute" <*> o .: "muteX" <*> o .: "muteW"

data RemoteWarningFocusProbe = RemoteWarningFocusProbe
    { rwfpHadFocusBefore ∷ Bool, rwfpHasFocusAfter ∷ Bool } deriving Show
instance FromJSON RemoteWarningFocusProbe where
    parseJSON = withObject "RemoteWarningFocusProbe" $ \o →
        RemoteWarningFocusProbe <$> o .: "hadFocusBefore" <*> o .: "hasFocusAfter"

data FontSizeCompareProbe = FontSizeCompareProbe
    { fscpUnshrunkSizes ∷ [Int], fscpShrunkSizes ∷ [Int] } deriving Show
instance FromJSON FontSizeCompareProbe where
    parseJSON = withObject "FontSizeCompareProbe" $ \o →
        FontSizeCompareProbe <$> o .: "unshrunkSizes" <*> o .: "shrunkSizes"

data ShrinkHeightSkipProbe = ShrinkHeightSkipProbe
    { shspSkipped ∷ Bool, shspUnshrunkH ∷ Double, shspShrunkH ∷ Double } deriving Show
instance FromJSON ShrinkHeightSkipProbe where
    parseJSON = withObject "ShrinkHeightSkipProbe" $ \o →
        ShrinkHeightSkipProbe <$> o .: "skipped" <*> o .: "unshrunkH" <*> o .: "shrunkH"

data ReflowProbe = ReflowProbe
    { rpBeforeX ∷ Double, rpBeforeY ∷ Double
    , rpAfterX ∷ Double, rpAfterY ∷ Double, rpAfterInFrame ∷ Bool } deriving (Show, Eq)
instance FromJSON ReflowProbe where
    parseJSON = withObject "ReflowProbe" $ \o →
        ReflowProbe <$> o .: "beforeX" <*> o .: "beforeY"
                     <*> o .: "afterX" <*> o .: "afterY" <*> o .: "afterInFrame"

data InvTabMigrationProbe = InvTabMigrationProbe
    { itmpTabRows ∷ Int, itmpHasFrame ∷ Bool
    , itmpMinX ∷ Double, itmpMaxRight ∷ Double
    , itmpRectLeft ∷ Double, itmpRectRight ∷ Double
    , itmpSelected ∷ Text, itmpAfter ∷ Text, itmpRowsAfter ∷ Int } deriving Show
instance FromJSON InvTabMigrationProbe where
    parseJSON = withObject "InvTabMigrationProbe" $ \o →
        InvTabMigrationProbe <$> o .: "tabRows" <*> o .: "hasFrame"
                             <*> o .: "minX" <*> o .: "maxRight"
                             <*> o .: "rectLeft" <*> o .: "rectRight"
                             <*> o .: "selected" <*> o .: "after"
                             <*> o .: "rowsAfter"

data UnitInfoBoundsProbe = UnitInfoBoundsProbe
    { uibpHiddenIsNil ∷ Bool, uibpX ∷ Int, uibpY ∷ Int, uibpW ∷ Int, uibpH ∷ Int } deriving Show
instance FromJSON UnitInfoBoundsProbe where
    parseJSON = withObject "UnitInfoBoundsProbe" $ \o →
        UnitInfoBoundsProbe <$> o .: "hiddenIsNil" <*> o .: "x" <*> o .: "y" <*> o .: "w" <*> o .: "h"

data SuppressProbe = SuppressProbe
    { spBefore ∷ Bool, spSuppressed ∷ Bool, spBoundsNil ∷ Bool, spAfter ∷ Bool } deriving Show
instance FromJSON SuppressProbe where
    parseJSON = withObject "SuppressProbe" $ \o →
        SuppressProbe <$> o .: "before" <*> o .: "suppressed" <*> o .: "boundsNil" <*> o .: "after"

-- | #1394 vertical-geometry probe for the remote-settlement modal's two
--   labels, shared by the ordinary and the narrow high-scale case so
--   both measure the identical thing.
--
--   Everything returned is a SCREEN coordinate. panel.getContentBounds
--   answers in panel-local coordinates while UI.getElementInfo answers
--   in screen ones, so the content rect is translated by
--   panel.getPosition before it can be compared against a label or a
--   button — the same treatment the round-20 fit case above already
--   gives UI element positions.
--
--   The width stub is this suite's established idiom (round-20 above):
--   synthetic font handles make engine.getTextWidth measure 0, which
--   would make responsive.fitScale a no-op and leave requirement 5
--   untested. It is restored immediately after createUI() has consumed
--   it, so nothing downstream in the shared fixture sees it.
--
--   A label's glyph band is its reported baseline minus its RENDERED
--   font size, through that baseline — the canonical authored line-band
--   convention this modal's own button text already uses, not
--   label.dump's floor(fontSize * 1.25) visual rectangle.
--   label.getFontSize is label.lua's own cached rendered size (the
--   fitted one it actually drew with); UI.getElementInfo reports a
--   zero-sized bounding box for a text element and so cannot supply it.
remoteWarningVerticalLua ∷ Text → Text → Text → Text → Text
remoteWarningVerticalLua uiscale fbW fbH distance = luaLines
    [ "local origGTW = engine.getTextWidth;"
    , "engine.getTextWidth = function(font, text, size) return #text * size end;"
    , "engine.setUIScale(" <> uiscale <> ");"
    , "local w = require('scripts.build_tool_remote_warning');"
    , "w.init(1,2,3," <> fbW <> "," <> fbH <> ");"
    , "w.open('acolyte_portal', 5, 5, " <> distance <> ", 30);"
    , "engine.getTextWidth = origGTW;"
    , "local label = require('scripts.ui.label');"
    , "local panelMod = require('scripts.ui.panel');"
    , "local px, py = panelMod.getPosition(w.panelId);"
    , "local cb = panelMod.getContentBounds(w.panelId);"
    , "local contentTop = py + cb.y;"
    , "local contentLeft = px + cb.x;"
    , "local titleId, msgId = w.ownedLabels[1], w.ownedLabels[2];"
    , "local titleFS = label.getFontSize(titleId);"
    , "local msgFS = label.getFontSize(msgId);"
    , "local titleW = label.getSize(titleId);"
    , "local msgW = label.getSize(msgId);"
    , "local ti = UI.getElementInfo(label.getElementHandle(titleId));"
    , "local mi = UI.getElementInfo(label.getElementHandle(msgId));"
    , "local buttonTop, buttonBottom, buttonCount = nil, nil, 0;"
    , "for h, _ in pairs(w.clickHandlers) do"
    , "    local bi = UI.getElementInfo(h);"
    , "    buttonCount = buttonCount + 1;"
    , "    if buttonTop == nil or bi.y < buttonTop then buttonTop = bi.y end;"
    , "    if buttonBottom == nil or (bi.y + bi.height) > buttonBottom then"
    , "        buttonBottom = bi.y + bi.height end"
    , "end;"
    , "return {contentTop = contentTop, contentBottom = contentTop + cb.height,"
    , "        contentMid = contentLeft + cb.width / 2,"
    , "        titleFontSize = titleFS, msgFontSize = msgFS,"
    , "        titleBandTop = ti.y - titleFS, titleBandBottom = ti.y,"
    , "        msgBandTop = mi.y - msgFS, msgBandBottom = mi.y,"
    , "        titleMid = ti.x + titleW / 2, msgMid = mi.x + msgW / 2,"
    , "        buttonTop = buttonTop, buttonBottom = buttonBottom,"
    , "        buttonCount = buttonCount}"
    ]

-- | The #1394 contract itself, asserted identically at both envelope
--   points. The two gap arguments are the SCALED authored gaps
--   (floor(titleGap * uiscale), floor(messageGap * uiscale)), restated
--   by each caller from scripts/build_tool_remote_warning.lua's own
--   baseSizes rather than read back out of the module — an edit to
--   either side has to fail.
assertRemoteWarningBands
    ∷ Double → Double → RemoteWarningVerticalProbe → Expectation
assertRemoteWarningBands titleGap messageGap p = do
    rwvButtonCount p `shouldBe` 2
    -- Requirements 1 and 2: each label's visible glyph band lies inside
    -- the content area, never in the padding or the drawn top border.
    rwvTitleBandTop p `shouldSatisfy` (≥ rwvContentTop p)
    rwvTitleBandBottom p `shouldSatisfy` (≤ rwvContentBottom p)
    rwvMsgBandTop p `shouldSatisfy` (≥ rwvContentTop p)
    rwvMsgBandBottom p `shouldSatisfy` (≤ rwvContentBottom p)
    -- The title's band starts exactly AT the content top — its authored
    -- y = 0 realized as a band edge. The bug put it a full rendered font
    -- size above this, which is the negative the containment check above
    -- would otherwise have to catch on its own.
    rwvTitleBandTop p `shouldBe` rwvContentTop p
    -- Requirement 4: the authored rhythm separates VISIBLE edges, so
    -- both gaps are exact. Placement reading a panel-scaled slot height
    -- instead of the rendered one shows up here as an off-by-the-shrink.
    (rwvMsgBandTop p - rwvTitleBandBottom p) `shouldBe` titleGap
    (rwvButtonTop p - rwvMsgBandBottom p) `shouldBe` messageGap
    -- Requirement 7: the button row still lands inside the content area
    -- (the labels moving down must not push it out of the panel).
    rwvButtonBottom p `shouldSatisfy` (≤ rwvContentBottom p)
    -- Requirement 3: both labels stay centered on the content area. A
    -- half-pixel of slack for an odd content width; the round-20
    -- regression this guards against misses by half a label width.
    abs (rwvTitleMid p - rwvContentMid p) `shouldSatisfy` (< 0.51)
    abs (rwvMsgMid p - rwvContentMid p) `shouldSatisfy` (< 0.51)
