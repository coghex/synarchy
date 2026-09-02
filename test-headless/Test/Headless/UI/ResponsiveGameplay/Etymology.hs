-- | Etymology owner of the #750 responsive-gameplay gate (#2126): the
--   complete Etymology panel (#1104) contract.
--
--   A 'SpecWith' fragment over the facade's single shared fixture —
--   see 'Test.Headless.UI.ResponsiveGameplay.Fixture'. This module
--   never boots an engine or a Lua VM of its own.
module Test.Headless.UI.ResponsiveGameplay.Etymology (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:), (.:?), (.!=))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Test.Headless.UI.ResponsiveGameplay.Fixture

spec ∷ SpecWith SharedFixture
spec = do
    -- #1104: the shared etymology panel and the name plate that hosts
    -- its three entry points. The engine query is STUBBED here for the
    -- same reason every other data-driven case in this module stubs its
    -- source: the bare Lua backend has no world page, so world.* would
    -- answer nil and the panel would only ever be exercised in its
    -- unavailable state. What is under test is the panel's own
    -- lifecycle, which is Lua.
    describe "Etymology panel (#1104)" $ do
        let -- One stub reply per entity kind, all sharing the ASH
            -- morpheme so recurrence rows really appear. Injected as
            -- world.getEtymology; the real engine function is restored
            -- by resetFixture's package.loaded wipe + the next case's
            -- own setup, and nothing here writes engine state.
            stubQuery = luaLines
                [ "world.getEtymology = function(kind, id)"
                , "  if kind == 'world' then"
                , "    return { available = true, name = 'Karadun',"
                , "      gloss = 'Ashen Land', form = 'modifier',"
                , "      language = { seed = '42', version = 5 },"
                , "      morphemes = {"
                , "        { id='42:5:ASH', concept='ASH', role='modifier',"
                , "          surface='kar', free='kara', bound=true,"
                , "          lemma='ashen' },"
                , "        { id='42:5:LAND', concept='LAND', role='head',"
                , "          surface='adun', free='adun', bound=false,"
                , "          lemma='land' } },"
                , "      tokens = { {kind='morpheme', text='Kar'},"
                , "                 {kind='morpheme', text='adun'} },"
                , "      recurrence = { { concept='ASH', morpheme='42:5:ASH',"
                , "        entries = { {kind='location', name='Karkeep'} } } } }"
                , "  elseif kind == 'location' then"
                , "    return { available = true, name = 'Karkeep',"
                , "      gloss = 'Ashen Keep', form = 'modifier',"
                , "      language = { seed = '42', version = 5 },"
                , "      morphemes = {"
                , "        { id='42:5:ASH', concept='ASH', role='modifier',"
                , "          surface='kar', free='kara', bound=true,"
                , "          lemma='ashen' } },"
                , "      tokens = { {kind='morpheme', text='Karkeep'} },"
                , "      recurrence = {} }"
                , "  elseif kind == 'river' then"
                , "    return { available = true, name = 'Karvash',"
                , "      gloss = 'Ashen River', form = 'modifier',"
                , "      language = { seed = '42', version = 5 },"
                , "      morphemes = {"
                , "        { id='42:5:ASH', concept='ASH', role='modifier',"
                , "          surface='kar', free='kara', bound=true,"
                , "          lemma='ashen' } },"
                , "      tokens = { {kind='morpheme', text='Karvash'} },"
                , "      recurrence = {} }"
                , "  end;"
                , "  return { available = false, reason = 'no_entity',"
                , "           reasonText = 'there is no such name' }"
                , "end;"
                ]
            -- Enough morphemes to overflow the panel's visible window,
            -- so a nonzero scroll offset is a legal position rather than
            -- one the panel is correct to clamp back to 0.
            stubScrollable = luaLines
                [ "world.getEtymology = function(kind, id)"
                , "  local ms = {};"
                , "  for i = 1, 20 do"
                , "    ms[i] = { id='42:5:C'..i, concept='C'..i,"
                , "              role='modifier', surface='s'..i,"
                , "              free='s'..i, bound=false, lemma='l'..i }"
                , "  end;"
                , "  return { available = true, name = 'Longname',"
                , "    gloss = 'A Long Name', form = 'modifier',"
                , "    language = { seed = '42', version = 5 },"
                , "    morphemes = ms,"
                , "    tokens = { {kind='morpheme', text='Longname'} },"
                , "    recurrence = {} }"
                , "end;"
                ]
            -- A long unavailable reason, for the clipping case.
            stubUnavailable = luaLines
                [ "world.getEtymology = function()"
                , "  return { available = false, name = 'Player Name',"
                , "    reason = 'custom',"
                , "    reasonText = 'this name was chosen rather than "
                    <> "generated, so it has no roots to show and nothing "
                    <> "beneath it to explain' }"
                , "end;"
                ]
            bootHud = luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                ]

        it "opens from the world entry point, and the panel it opens is \
           \populated from the query rather than empty" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubQuery, bootHud
                , "local ep = require('scripts.etymology_panel');"
                , "ep.openFor('world');"
                , "local d = ep.dump();"
                , "return {open = d.open, kind = d.kind,"
                , "        available = d.available, name = d.name,"
                , "        rowCount = d.rowCount,"
                , "        morphemeCount = #(d.morphemes or {})}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyOpenProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    eopOpen p `shouldBe` True
                    eopKind p `shouldBe` Just "world"
                    eopAvailable p `shouldBe` Just True
                    eopName p `shouldBe` Just "Karadun"
                    eopRowCount p `shouldSatisfy` (> 0)
                    eopMorphemeCount p `shouldBe` 2

        it "all three entry points reach the SAME panel implementation -- \
           \one module, one page, one set of elements, retargeted rather \
           \than duplicated" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubQuery, bootHud
                , "local ep = require('scripts.etymology_panel');"
                , "local out = {};"
                , "for _, t in ipairs({{'world'}, {'location', 1}, {'river', 7}}) do"
                , "  ep.openFor(t[1], t[2]);"
                , "  local d = ep.dump();"
                , "  table.insert(out, {kind = d.kind, name = d.name,"
                , "                     viewport = d.viewport,"
                , "                     rowCount = d.rowCount});"
                , "end;"
                , "return {rows = out, sameModule = ep == "
                    <> "require('scripts.etymology_panel')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyRoutesProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    erpSameModule p `shouldBe` True
                    map erKind (erpRows p)
                        `shouldBe` [Just "world", Just "location", Just "river"]
                    map erName (erpRows p)
                        `shouldBe` [Just "Karadun", Just "Karkeep", Just "Karvash"]
                    -- Every route produced a real, populated view.
                    forM_ (erpRows p) $ \row →
                        erRowCount row `shouldSatisfy` (> 0)

        it "close and Escape both dismiss it, and both are idempotent" $
            \(env, ls) → do
                resetFixture env ls
                r ← evalJSON ls $ luaLines
                    [ stubQuery, bootHud
                    , "local ep = require('scripts.etymology_panel');"
                    , "ep.openFor('world');"
                    , "local opened = ep.isOpen();"
                    , "local consumed = ep.handleKeyDown('Escape');"
                    , "local afterEsc = ep.isOpen();"
                    , "local again = ep.handleKeyDown('Escape');"
                    , "ep.openFor('river', 7);"
                    , "ep.closeIfOpen(); ep.closeIfOpen();"
                    , "return {opened = opened, consumed = consumed,"
                    , "        afterEsc = afterEsc, again = again,"
                    , "        afterClose = ep.isOpen()}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyCloseProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        ecpOpened p `shouldBe` True
                        ecpConsumed p `shouldBe` True
                        ecpAfterEsc p `shouldBe` False
                        -- Escape with nothing open is not consumed, so it
                        -- falls through to the rest of the cascade.
                        ecpAgain p `shouldBe` False
                        ecpAfterClose p `shouldBe` False

        it "renders the honest unavailable state -- the stored name stays \
           \visible beside the reason, and no morpheme row is invented" $
            \(env, ls) → do
                resetFixture env ls
                r ← evalJSON ls $ luaLines
                    [ stubUnavailable, bootHud
                    , "local ep = require('scripts.etymology_panel');"
                    , "ep.openFor('world');"
                    , "local d = ep.dump();"
                    , "return {open = d.open, available = d.available,"
                    , "        name = d.name, reason = d.reason,"
                    , "        rowCount = d.rowCount,"
                    , "        morphemeCount = #(d.morphemes or {})}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyOpenProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        eopOpen p `shouldBe` True
                        eopAvailable p `shouldBe` Just False
                        eopName p `shouldBe` Just "Player Name"
                        -- Something IS shown (the reason), and it is not
                        -- a fabricated decomposition.
                        eopRowCount p `shouldSatisfy` (> 0)
                        eopMorphemeCount p `shouldBe` 0

        it "content that exceeds the panel lives inside a real #747 \
           \clipping viewport, and every rendered row is a descendant of \
           \it rather than a page-attached element" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubQuery, bootHud
                , "local ep = require('scripts.etymology_panel');"
                , "ep.openFor('world');"
                , "local d = ep.dump();"
                , "local vp = d.viewport;"
                , "local vpInfo = vp and UI.getElementInfo(vp);"
                , "local allInside = true;"
                , "for _, row in ipairs(d.rows or {}) do"
                , "  if not row.handle then allInside = false end"
                , "end;"
                , "return {hasViewport = vp ~= nil,"
                , "        clips = (vpInfo and vpInfo.clipsChildren) == true,"
                , "        vpW = vpInfo and vpInfo.width or 0,"
                , "        vpH = vpInfo and vpInfo.height or 0,"
                , "        zIndex = vpInfo and vpInfo.zIndex or 0,"
                , "        allInside = allInside}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyClipProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    ecClipHasViewport p `shouldBe` True
                    ecClipClips p `shouldBe` True
                    ecClipW p `shouldSatisfy` (> 0)
                    ecClipH p `shouldSatisfy` (> 0)
                    -- zIndex ACCUMULATES through the parent chain, so a
                    -- wrapper/viewport must stay at 0.
                    ecClipZ p `shouldBe` 0
                    ecClipAllInside p `shouldBe` True

        it "a resize preserves the INSPECTED TARGET and the scroll offset \
           \-- a geometry rebuild is a layout change, not a semantic \
           \re-entry" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubScrollable, bootHud
                , "local ep = require('scripts.etymology_panel');"
                , "ep.openFor('river', 7);"
                , "ep.state.scrollOffset = 3;"
                , "local beforeKind, beforeId = ep.currentTarget();"
                , "local before = ep.state.scrollOffset;"
                , "ep.onFramebufferResize(1600, 900);"
                , "local afterKind, afterId = ep.currentTarget();"
                , "return {beforeKind = beforeKind, beforeId = beforeId,"
                , "        before = before, afterKind = afterKind,"
                , "        afterId = afterId, after = ep.state.scrollOffset,"
                , "        stillOpen = ep.isOpen()}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyResizeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    erzStillOpen p `shouldBe` True
                    erzAfterKind p `shouldBe` erzBeforeKind p
                    erzAfterId p `shouldBe` erzBeforeId p
                    -- A REAL offset, not a vacuous zero.
                    erzBefore p `shouldSatisfy` (> 0)
                    erzAfter p `shouldBe` erzBefore p

        it "hud.createUI() carries the panel across a full rebuild -- the \
           \real resize path, which destroys and recreates world_page" $
            \(env, ls) → do
                resetFixture env ls
                r ← evalJSON ls $ luaLines
                    [ stubQuery, bootHud
                    , "local ep = require('scripts.etymology_panel');"
                    , "ep.openFor('location', 1);"
                    , "local hud = require('scripts.hud');"
                    , "hud.init(1,2,1600,900);"
                    , "hud.createUI();"
                    , "local k, i = ep.currentTarget();"
                    , "local d = ep.dump();"
                    , "return {stillOpen = ep.isOpen(), kind = k, id = i,"
                    , "        rowCount = d.rowCount}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyRebuildProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        erbStillOpen p `shouldBe` True
                        erbKind p `shouldBe` Just "location"
                        erbId p `shouldBe` Just 1
                        erbRowCount p `shouldSatisfy` (> 0)

        it "a scale-only change reaches it through \
           \uiManager.notifyGameplayRescale, and it is NOT on the \
           \responsive menu registry (which would double-fire it)" $
            \(env, ls) → do
                resetFixture env ls
                r ← evalJSON ls $ luaLines
                    [ stubQuery, bootHud
                    , "local ep = require('scripts.etymology_panel');"
                    , "ep.openFor('world');"
                    , "local calls = 0;"
                    , "local realReflow = ep.reflow;"
                    , "ep.reflow = function(...) calls = calls + 1;"
                    , "                          return realReflow(...) end;"
                    , "engine.setUIScale(2.0);"
                    , "local uiManager = require('scripts.ui_manager');"
                    , "uiManager.notifyGameplayRescale(1920, 1080);"
                    , "ep.reflow = realReflow;"
                    , "local responsive = require('scripts.ui.responsive');"
                    , "local registered = false;"
                    , "for name, _ in pairs(responsive.registry or {}) do"
                    , "  if name == 'etymology_panel' or name == 'name_plate'"
                    , "  then registered = true end"
                    , "end;"
                    , "return {calls = calls, registered = registered,"
                    , "        stillOpen = ep.isOpen()}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyRescaleProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        -- Exactly once: reached, and not double-fired.
                        ersCalls p `shouldBe` 1
                        ersRegistered p `shouldBe` False
                        ersStillOpen p `shouldBe` True

        it "content is bounded by the FRAMEBUFFER, not only by the row \
           \count: a short screen shows fewer rows and scrolls the rest, \
           \rather than painting rows the panel box cannot hold" $
            \(env, ls) → do
                resetFixture env ls
                r ← evalJSON ls $ luaLines
                    [ stubScrollable
                    , "local hud = require('scripts.hud');"
                    -- Deliberately short, and inside the supported
                    -- envelope's formal 800x600 minimum.
                    , "hud.init(1,2,800,600);"
                    , "hud.createUI();"
                    , "local ep = require('scripts.etymology_panel');"
                    , "ep.openFor('world');"
                    , "local panel = require('scripts.ui.panel');"
                    , "local d = ep.dump();"
                    , "local box = panel.getBoxHandle(ep.state.panelId);"
                    , "local bi = box and UI.getElementInfo(box);"
                    , "local vi = d.viewport and UI.getElementInfo(d.viewport);"
                    , "return {rowCount = d.rowCount,"
                    , "        visibleRows = d.visibleRows,"
                    , "        panelBottom = bi and (bi.y + bi.height) or 0,"
                    , "        viewportBottom = vi and (vi.y + vi.height) or 0,"
                    , "        hasScrollbar = d.scrollbar ~= nil}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyFitProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        -- More content than fits, so the cap is doing work.
                        efRowCount p `shouldSatisfy` (> efVisibleRows p)
                        efVisibleRows p `shouldSatisfy` (> 0)
                        -- The panel stays on screen...
                        efPanelBottom p `shouldSatisfy` (≤ 600)
                        -- ...and its scrolling viewport stays inside it,
                        -- which is what a row count derived only from the
                        -- content would break.
                        efViewportBottom p `shouldSatisfy` (≤ efPanelBottom p)
                        efHasScrollbar p `shouldBe` True

        it "no-ops safely at a minimized 0x0 framebuffer, and a later \
           \real resize recovers valid geometry" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubQuery, bootHud
                , "local ep = require('scripts.etymology_panel');"
                , "local np = require('scripts.name_plate');"
                , "ep.openFor('world');"
                , "local ok1 = pcall(function()"
                , "  ep.onFramebufferResize(0, 0); np.onFramebufferResize(0, 0)"
                , "end);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,0,0); local ok2 = pcall(hud.createUI);"
                , "hud.init(1,2,1280,720); local ok3 = pcall(hud.createUI);"
                , "local d = ep.dump();"
                , "local vp = d.viewport;"
                , "local info = vp and UI.getElementInfo(vp);"
                , "return {ok1 = ok1, ok2 = ok2, ok3 = ok3,"
                , "        w = info and info.width or 0,"
                , "        h = info and info.height or 0}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyMinimizeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    emzOk1 p `shouldBe` True
                    emzOk2 p `shouldBe` True
                    emzOk3 p `shouldBe` True
                    emzW p `shouldSatisfy` (> 0)
                    emzH p `shouldSatisfy` (> 0)

        it "the view_teardown sweep removes it on BOTH the hud-hide and \
           \zoom-band transitions, leaving no stale open state and no \
           \stale element handles" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubQuery, bootHud
                , "local ep = require('scripts.etymology_panel');"
                , "local np = require('scripts.name_plate');"
                , "ep.openFor('world');"
                , "np.state.rows = {{kind='world', id=nil, name='Karadun'}};"
                -- The registry itself is module-private, so this
                -- asserts what registration MEANS: running the sweep
                -- reaches both widgets. A missing entry shows up as a
                -- surviving panel, which is the regression that matters.
                , "local vt = require('scripts.ui.view_teardown');"
                , "vt.run('hudHide', {});"
                , "local d = ep.dump();"
                , "local afterHide = ep.isOpen();"
                , "ep.openFor('world');"
                , "vt.run('zoomBand', {});"
                , "local afterBand = ep.isOpen();"
                , "return {registeredPanel = afterHide == false,"
                , "        registeredPlate = afterBand == false,"
                , "        open = ep.isOpen(), rowCount = d.rowCount,"
                , "        viewportNil = d.viewport == nil,"
                , "        plateRows = #np.state.rows}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyTeardownProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- Both transitions really closed it.
                    etdRegisteredPanel p `shouldBe` True
                    etdRegisteredPlate p `shouldBe` True
                    etdOpen p `shouldBe` False
                    etdRowCount p `shouldBe` 0
                    etdViewportNil p `shouldBe` True
                    etdPlateRows p `shouldBe` 0

        it "the mouse WHEEL scrolls it through the real routing -- \
           \uiManager.onUIScroll over the panel moves the content and \
           \consumes the event, so it cannot fall through to the world \
           \zoom underneath" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubScrollable, bootHud
                , "local ep = require('scripts.etymology_panel');"
                , "local uiManager = require('scripts.ui_manager');"
                , "ep.openFor('world');"
                , "local panel = require('scripts.ui.panel');"
                , "local box = panel.getBoxHandle(ep.state.panelId);"
                , "local info = box and UI.getElementInfo(box);"
                , "local before = ep.state.scrollOffset;"
                -- Wheel DOWN over the panel box: the same element
                -- routeScroll would have picked, since it is the
                -- capturing surface.
                , "uiManager.onUIScroll(box, 0, -1, false);"
                , "local afterDown = ep.state.scrollOffset;"
                , "uiManager.onUIScroll(box, 0, 1, false);"
                , "local afterUp = ep.state.scrollOffset;"
                -- An element that is NOT ours must not be consumed.
                , "local foreign = ep.state.viewportId;"
                , "local consumedForeign = ep.onScroll(-12345, 0, -1);"
                , "return {captures = (info and info.scrollCapturing) == true,"
                , "        before = before, afterDown = afterDown,"
                , "        afterUp = afterUp,"
                , "        consumed = ep.onScroll(box, 0, -1) == true,"
                , "        consumedForeign = consumedForeign == true,"
                , "        rowsMoved = (ep.dump().rows[1] or {}).text}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyWheelProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    ewCaptures p `shouldBe` True
                    ewBefore p `shouldBe` 0
                    -- The wheel really moved the view, both ways.
                    ewAfterDown p `shouldSatisfy` (> 0)
                    ewAfterUp p `shouldBe` 0
                    ewConsumed p `shouldBe` True
                    -- and an unrelated element is left alone.
                    ewConsumedForeign p `shouldBe` False

        -- The scrollbar's ARROW route. Its buttons are UI sprites, and
        -- the bare Lua backend has no textures, so a real scrollbar here
        -- owns no element handles at all (dump().scrollHandles is empty)
        -- and the arrows cannot be clicked the way a GPU boot can. What
        -- IS testable headlessly is the wiring the review found missing:
        -- that uiManager's arrow routes consult this panel, and that its
        -- handler moves the view for a handle belonging to its own
        -- scrollbar. tools/etymology_probe.py drives the real arrows.
        it "uiManager.onScrollUp / onScrollDown consult the panel, and \
           \its handler moves the view for its OWN scrollbar's handle \
           \while ignoring another widget's" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubScrollable, bootHud
                , "local ep = require('scripts.etymology_panel');"
                , "local uiManager = require('scripts.ui_manager');"
                , "local scrollbar = require('scripts.ui.scrollbar');"
                , "ep.openFor('world');"
                -- (a) The router really reaches this panel on both
                -- arrow callbacks.
                , "local seen = {};"
                , "local real = ep.handleScrollCallback;"
                , "ep.handleScrollCallback = function(name, h)"
                , "  seen[#seen + 1] = name; return false end;"
                , "uiManager.onScrollUp(-999);"
                , "uiManager.onScrollDown(-999);"
                , "ep.handleScrollCallback = real;"
                -- (b) The handler itself: a handle that resolves to OUR
                -- scrollbar scrolls; one that resolves elsewhere does not.
                , "local realFind = scrollbar.findByElementHandle;"
                , "scrollbar.findByElementHandle = function(h)"
                , "  if h == 4242 then return ep.state.scrollbarId end"
                , "  return realFind(h) end;"
                , "local before = ep.state.scrollOffset;"
                , "local downOk = ep.handleScrollCallback('onScrollDown', 4242);"
                , "local afterDown = ep.state.scrollOffset;"
                , "local upOk = ep.handleScrollCallback('onScrollUp', 4242);"
                , "local afterUp = ep.state.scrollOffset;"
                , "local foreign = ep.handleScrollCallback('onScrollDown', 7);"
                , "scrollbar.findByElementHandle = realFind;"
                , "return {seen = seen, before = before,"
                , "        afterDown = afterDown, afterUp = afterUp,"
                , "        downOk = downOk == true, upOk = upOk == true,"
                , "        foreign = foreign == true}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyArrowProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- Both arrow routes reach the panel.
                    eaSeen p `shouldBe` ["onScrollUp", "onScrollDown"]
                    eaBefore p `shouldBe` 0
                    eaDownOk p `shouldBe` True
                    eaAfterDown p `shouldSatisfy` (> 0)
                    eaUpOk p `shouldBe` True
                    eaAfterUp p `shouldBe` 0
                    -- Another widget's scrollbar is left alone.
                    eaForeign p `shouldBe` False

        it "the name plate offers an entry point for a DISCOVERED \
           \location and for a river on the selected tile, and none for \
           \an undiscovered location -- so an unfound name is never even \
           \reachable" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ bootHud
                , "local np = require('scripts.name_plate');"
                , "world.getIdentity = function() return {name='Karadun'} end;"
                , "world.getSelectedTile = function() return {gx=5, gy=5} end;"
                , "world.getRiverAt = function() return {id=7, name='Karvash'} end;"
                , "world.listPlacedLocations = function() return {"
                , "  {instance_id=1, name='Karkeep', lifecycle='discovered',"
                , "   bounds={min_x=0,min_y=0,max_x=10,max_y=10}},"
                , "  {instance_id=2, name='Hidden', lifecycle='unknown',"
                , "   bounds={min_x=0,min_y=0,max_x=10,max_y=10}} } end;"
                , "np.refresh();"
                , "local d = np.dump();"
                , "local kinds, names = {}, {};"
                , "for _, r in ipairs(d.rows) do"
                , "  table.insert(kinds, r.kind); table.insert(names, r.name)"
                , "end;"
                , "return {kinds = kinds, names = names}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyPlateProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    eppKinds p `shouldBe` ["world", "location", "river"]
                    -- The undiscovered ruin's name appears nowhere.
                    eppNames p `shouldBe` ["Karadun", "Karkeep", "Karvash"]
                    eppNames p `shouldNotSatisfy` elem "Hidden"

        it "a name plate row is a real, in-frame, clickable control, and \
           \clicking it opens the panel on THAT entity" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubQuery, bootHud
                , "local np = require('scripts.name_plate');"
                , "local ep = require('scripts.etymology_panel');"
                , "world.getIdentity = function() return {name='Karadun'} end;"
                , "world.getSelectedTile = function() return {gx=5, gy=5} end;"
                , "world.getRiverAt = function() return {id=7, name='Karvash'} end;"
                , "world.listPlacedLocations = function() return {} end;"
                , "np.refresh();"
                , "local d = np.dump();"
                , "local riverRow;"
                , "for _, r in ipairs(d.rows) do"
                , "  if r.kind == 'river' then riverRow = r end"
                , "end;"
                , "if not riverRow then return {found = false} end;"
                , "local button = require('scripts.ui.button');"
                , "button.handleClickByElement(riverRow.handle);"
                , "local k, i = ep.currentTarget();"
                , "return {found = true, x = riverRow.x, y = riverRow.y,"
                , "        w = riverRow.width, h = riverRow.height,"
                , "        openKind = k, openId = i}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtyClickProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    eclFound p `shouldBe` True
                    eclX p `shouldSatisfy` (≥ 0)
                    eclY p `shouldSatisfy` (≥ 0)
                    eclW p `shouldSatisfy` (> 0)
                    eclH p `shouldSatisfy` (> 0)
                    (eclX p + eclW p) `shouldSatisfy` (≤ 1920)
                    eclOpenKind p `shouldBe` Just "river"
                    eclOpenId p `shouldBe` Just 7

        -- #1264. Location bounds are CYLINDRICAL (Location.Bounds
        -- .boundsContainsPoint tests the box and both u-wrap aliases)
        -- while #1175 hands the plate a CANONICAL selected tile, so a
        -- location straddling the seam is named by a tile that a raw
        -- Cartesian comparison rejects. The numbers here are the
        -- authoritative divergence Test.Headless.Location.Bounds pins:
        -- world size 8 (wrap period 8 * 16 = 128 tiles), bounds
        -- (68,4)-(72,8), canonical tile (6,70) — raw distance 62, seam
        -- distance 0. The wrap width is stubbed only for the hud's OWN
        -- page, so a plate that asked about the active page or assumed
        -- a nominal size gets 0 back and fails to find the row.
        it "the name plate finds a DISCOVERED location whose bounds \
           \straddle the U seam from the canonical selected tile, \
           \resolving the wrap period of its own page -- while an \
           \undiscovered seam location and a genuinely distant one \
           \still produce no row" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ stubQuery, bootHud
                , "local np = require('scripts.name_plate');"
                , "local ep = require('scripts.etymology_panel');"
                , "world.getIdentity = function() return {name='Karadun'} end;"
                -- Canonical frame: this tile lies in the (-1) alias of
                -- the ruin's stored box, never in the box itself.
                , "world.getSelectedTile = function() return {gx=6, gy=70} end;"
                , "world.getRiverAt = function() return nil end;"
                , "local pages = {};"
                , "world.getWrapWidth = function(pid)"
                , "  pages[#pages + 1] = tostring(pid);"
                , "  if pid == 'main_world' then return 128 end;"
                , "  return 0 end;"
                -- Ordered so a broken lifecycle gate or an always-true
                -- containment would be caught by the WRONG row winning.
                , "world.listPlacedLocations = function() return {"
                , "  {instance_id=9, name='Faraway', lifecycle='discovered',"
                , "   bounds={min_x=200,min_y=200,max_x=210,max_y=210}},"
                , "  {instance_id=12, name='Hidden', lifecycle='unknown',"
                , "   bounds={min_x=68,min_y=4,max_x=72,max_y=8}},"
                , "  {instance_id=13, name='Rumoured', lifecycle='hinted',"
                , "   bounds={min_x=68,min_y=4,max_x=72,max_y=8}},"
                , "  {instance_id=11, name='Karkeep', lifecycle='discovered',"
                , "   bounds={min_x=68,min_y=4,max_x=72,max_y=8}} } end;"
                , "np.refresh();"
                , "local d = np.dump();"
                , "local kinds, names, locRow = {}, {}, nil;"
                , "for _, row in ipairs(d.rows) do"
                , "  table.insert(kinds, row.kind);"
                , "  table.insert(names, row.name);"
                , "  if row.kind == 'location' then locRow = row end"
                , "end;"
                , "local openKind, openId;"
                , "if locRow then"
                , "  local button = require('scripts.ui.button');"
                , "  button.handleClickByElement(locRow.handle);"
                , "  openKind, openId = ep.currentTarget()"
                , "end;"
                -- Never an empty array, so "the plate never asked" is a
                -- readable assertion failure rather than a decode error.
                , "if #pages == 0 then pages[1] = '(never asked)' end;"
                , "return {kinds = kinds, names = names, pages = pages,"
                , "        found = locRow ~= nil,"
                , "        openKind = openKind, openId = openId}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EtySeamProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- The seam-straddling ruin produces its row.
                    espFound p `shouldBe` True
                    espKinds p `shouldBe` ["world", "location"]
                    espNames p `shouldBe` ["Karadun", "Karkeep"]
                    -- Neither undiscovered lifecycle leaks a name, and
                    -- containment did not simply become true for all.
                    espNames p `shouldNotSatisfy` elem "Hidden"
                    espNames p `shouldNotSatisfy` elem "Rumoured"
                    espNames p `shouldNotSatisfy` elem "Faraway"
                    -- The wrap period came from the plate's OWN page.
                    espPages p `shouldSatisfy` all (≡ "main_world")
                    espPages p `shouldNotSatisfy` null
                    -- and the row still opens the etymology panel on
                    -- that instance, so the action stays reachable.
                    espOpenKind p `shouldBe` Just "location"
                    espOpenId p `shouldBe` Just 11

-- * FromJSON row types owned by the Etymology panel group

-- #1104 etymology panel probes.
data EtyOpenProbe = EtyOpenProbe
    { eopOpen ∷ Bool, eopKind ∷ Maybe Text, eopAvailable ∷ Maybe Bool
    , eopName ∷ Maybe Text, eopRowCount ∷ Int
    , eopMorphemeCount ∷ Int } deriving Show
instance FromJSON EtyOpenProbe where
    parseJSON = withObject "EtyOpenProbe" $ \o →
        EtyOpenProbe <$> o .: "open" <*> o .:? "kind" <*> o .:? "available"
                     <*> o .:? "name" <*> o .: "rowCount"
                     <*> o .: "morphemeCount"

data EtyRouteRow = EtyRouteRow
    { erKind ∷ Maybe Text, erName ∷ Maybe Text
    , erRowCount ∷ Int } deriving Show
instance FromJSON EtyRouteRow where
    parseJSON = withObject "EtyRouteRow" $ \o →
        EtyRouteRow <$> o .:? "kind" <*> o .:? "name" <*> o .: "rowCount"

data EtyRoutesProbe = EtyRoutesProbe
    { erpRows ∷ [EtyRouteRow], erpSameModule ∷ Bool } deriving Show
instance FromJSON EtyRoutesProbe where
    parseJSON = withObject "EtyRoutesProbe" $ \o →
        EtyRoutesProbe <$> o .: "rows" <*> o .: "sameModule"

data EtyCloseProbe = EtyCloseProbe
    { ecpOpened ∷ Bool, ecpConsumed ∷ Bool, ecpAfterEsc ∷ Bool
    , ecpAgain ∷ Bool, ecpAfterClose ∷ Bool } deriving Show
instance FromJSON EtyCloseProbe where
    parseJSON = withObject "EtyCloseProbe" $ \o →
        EtyCloseProbe <$> o .: "opened" <*> o .: "consumed"
                      <*> o .: "afterEsc" <*> o .: "again"
                      <*> o .: "afterClose"

data EtyClipProbe = EtyClipProbe
    { ecClipHasViewport ∷ Bool, ecClipClips ∷ Bool, ecClipW ∷ Int
    , ecClipH ∷ Int, ecClipZ ∷ Int, ecClipAllInside ∷ Bool } deriving Show
instance FromJSON EtyClipProbe where
    parseJSON = withObject "EtyClipProbe" $ \o →
        EtyClipProbe <$> o .: "hasViewport" <*> o .: "clips" <*> o .: "vpW"
                     <*> o .: "vpH" <*> o .: "zIndex" <*> o .: "allInside"

data EtyResizeProbe = EtyResizeProbe
    { erzBeforeKind ∷ Maybe Text, erzBeforeId ∷ Maybe Int, erzBefore ∷ Int
    , erzAfterKind ∷ Maybe Text, erzAfterId ∷ Maybe Int, erzAfter ∷ Int
    , erzStillOpen ∷ Bool } deriving Show
instance FromJSON EtyResizeProbe where
    parseJSON = withObject "EtyResizeProbe" $ \o →
        EtyResizeProbe <$> o .:? "beforeKind" <*> o .:? "beforeId"
                       <*> o .: "before" <*> o .:? "afterKind"
                       <*> o .:? "afterId" <*> o .: "after"
                       <*> o .: "stillOpen"

data EtyRebuildProbe = EtyRebuildProbe
    { erbStillOpen ∷ Bool, erbKind ∷ Maybe Text, erbId ∷ Maybe Int
    , erbRowCount ∷ Int } deriving Show
instance FromJSON EtyRebuildProbe where
    parseJSON = withObject "EtyRebuildProbe" $ \o →
        EtyRebuildProbe <$> o .: "stillOpen" <*> o .:? "kind" <*> o .:? "id"
                        <*> o .: "rowCount"

data EtyRescaleProbe = EtyRescaleProbe
    { ersCalls ∷ Int, ersRegistered ∷ Bool
    , ersStillOpen ∷ Bool } deriving Show
instance FromJSON EtyRescaleProbe where
    parseJSON = withObject "EtyRescaleProbe" $ \o →
        EtyRescaleProbe <$> o .: "calls" <*> o .: "registered"
                        <*> o .: "stillOpen"

data EtyMinimizeProbe = EtyMinimizeProbe
    { emzOk1 ∷ Bool, emzOk2 ∷ Bool, emzOk3 ∷ Bool
    , emzW ∷ Int, emzH ∷ Int } deriving Show
instance FromJSON EtyMinimizeProbe where
    parseJSON = withObject "EtyMinimizeProbe" $ \o →
        EtyMinimizeProbe <$> o .: "ok1" <*> o .: "ok2" <*> o .: "ok3"
                         <*> o .: "w" <*> o .: "h"

data EtyTeardownProbe = EtyTeardownProbe
    { etdRegisteredPanel ∷ Bool, etdRegisteredPlate ∷ Bool, etdOpen ∷ Bool
    , etdRowCount ∷ Int, etdViewportNil ∷ Bool
    , etdPlateRows ∷ Int } deriving Show
instance FromJSON EtyTeardownProbe where
    parseJSON = withObject "EtyTeardownProbe" $ \o →
        EtyTeardownProbe <$> o .: "registeredPanel" <*> o .: "registeredPlate"
                         <*> o .: "open" <*> o .: "rowCount"
                         <*> o .: "viewportNil" <*> o .: "plateRows"

data EtyPlateProbe = EtyPlateProbe
    { eppKinds ∷ [Text], eppNames ∷ [Text] } deriving Show
instance FromJSON EtyPlateProbe where
    parseJSON = withObject "EtyPlateProbe" $ \o →
        EtyPlateProbe <$> o .: "kinds" <*> o .: "names"

data EtyClickProbe = EtyClickProbe
    { eclFound ∷ Bool, eclX ∷ Int, eclY ∷ Int, eclW ∷ Int, eclH ∷ Int
    , eclOpenKind ∷ Maybe Text, eclOpenId ∷ Maybe Int } deriving Show
instance FromJSON EtyClickProbe where
    parseJSON = withObject "EtyClickProbe" $ \o →
        EtyClickProbe <$> o .: "found" <*> o .:? "x" .!= 0 <*> o .:? "y" .!= 0
                      <*> o .:? "w" .!= 0 <*> o .:? "h" .!= 0
                      <*> o .:? "openKind" <*> o .:? "openId"

data EtySeamProbe = EtySeamProbe
    { espKinds ∷ [Text], espNames ∷ [Text], espPages ∷ [Text]
    , espFound ∷ Bool, espOpenKind ∷ Maybe Text, espOpenId ∷ Maybe Int
    } deriving Show
instance FromJSON EtySeamProbe where
    parseJSON = withObject "EtySeamProbe" $ \o →
        EtySeamProbe <$> o .: "kinds" <*> o .: "names" <*> o .: "pages"
                     <*> o .: "found" <*> o .:? "openKind" <*> o .:? "openId"

data EtyWheelProbe = EtyWheelProbe
    { ewCaptures ∷ Bool, ewBefore ∷ Int, ewAfterDown ∷ Int, ewAfterUp ∷ Int
    , ewConsumed ∷ Bool, ewConsumedForeign ∷ Bool } deriving Show
instance FromJSON EtyWheelProbe where
    parseJSON = withObject "EtyWheelProbe" $ \o →
        EtyWheelProbe <$> o .: "captures" <*> o .: "before"
                      <*> o .: "afterDown" <*> o .: "afterUp"
                      <*> o .: "consumed" <*> o .: "consumedForeign"

data EtyArrowProbe = EtyArrowProbe
    { eaSeen ∷ [Text], eaBefore ∷ Int, eaAfterDown ∷ Int, eaAfterUp ∷ Int
    , eaDownOk ∷ Bool, eaUpOk ∷ Bool, eaForeign ∷ Bool } deriving Show
instance FromJSON EtyArrowProbe where
    parseJSON = withObject "EtyArrowProbe" $ \o →
        EtyArrowProbe <$> o .: "seen" <*> o .: "before"
                      <*> o .: "afterDown" <*> o .: "afterUp"
                      <*> o .: "downOk" <*> o .: "upOk"
                      <*> o .: "foreign"

data EtyFitProbe = EtyFitProbe
    { efRowCount ∷ Int, efVisibleRows ∷ Int, efPanelBottom ∷ Int
    , efViewportBottom ∷ Int, efHasScrollbar ∷ Bool } deriving Show
instance FromJSON EtyFitProbe where
    parseJSON = withObject "EtyFitProbe" $ \o →
        EtyFitProbe <$> o .: "rowCount" <*> o .: "visibleRows"
                    <*> o .: "panelBottom" <*> o .: "viewportBottom"
                    <*> o .: "hasScrollbar"
