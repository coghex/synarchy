-- | #1676: @scripts/unit_drag_select.lua@'s half of the deferred-gesture
--   coordinate contract. Its F4 (#774) action-outcome records must land in
--   the framebuffer-pixel oracle space, and for a DEFERRED gesture — a
--   pending click, or a drag torn down by a view transition — the position
--   they report is the one the press had, captured with the
--   window→framebuffer ratio live at press. Converting the retained window
--   coordinate at resolution time instead reinterprets the press under
--   whatever ratio a mid-hold DPI change or resize left behind.
--
--   Every case here runs the REAL module in a stdlib-only Lua VM (the
--   pattern "Test.Headless.Lua.SharedHelpers" uses), stubbing only what
--   the module reaches out to: the two geometry queries and the cursor
--   position (so the ratio can be moved mid-gesture), @debug.recordOutcome@
--   (so the records are readable), and the sibling globals its untaken
--   branches index. No engine boots — nothing under test here needs one,
--   and @Test/Headless/UI/InputOwnership.hs@ stubs this module out
--   entirely, so before #1676 no headless spec exercised it at all.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "drag-select deferred capture"'@.
module Test.Headless.Lua.DragSelectDeferred (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua

-- | Stubs plus the real module, shared by every case. @__win@/@__fb@ are
--   the geometry the module sees and are rewritten mid-gesture by the
--   cases; @__mouse@ is what @dragSelect.update@ polls; @__records@
--   collects what would have reached the F4 ring.
--
--   @edgeIds@ is set to an empty table because the rect sprites are the
--   one thing a headless VM cannot have: @setEdgesVisible@ walks it with
--   @pairs@, so @{}@ makes the visual a no-op without stubbing @UI@.
prelude ∷ [Text]
prelude =
    [ "__win, __fb, __mouse, __records = {1280,720}, {2560,1440}, {0,0}, {}"
    , "engine = {"
    , "  getWindowSize = function() return __win[1], __win[2] end,"
    , "  getFramebufferSize = function() return __fb[1], __fb[2] end,"
    , "  getMousePosition = function() return __mouse[1], __mouse[2] end,"
    , "  logInfo = function() end,"
    , "  isKeyDown = function() return false end }"
    , "debug.recordOutcome = function(rec) __records[#__records+1] = rec end"
    , "world, unit, item, building, UI = {}, {}, {}, {}, {}"
    , "package.loaded['scripts.hud'] = {}"
    , "ds = require('scripts.unit_drag_select')"
    , "ds.edgeIds = {}"
    -- Every case expects exactly one record; asserting the count here
    -- keeps a case from passing on a record some other path emitted.
    , "function only()"
    , "  assert(#__records == 1, 'expected one record, got ' .. #__records)"
    , "  return __records[1]"
    , "end"
    , "function at(rec, x, y)"
    , "  assert(rec.where.x == x and rec.where.y == y,"
    , "    'expected (' .. x .. ',' .. y .. '), got ('"
    , "      .. tostring(rec.where.x) .. ',' .. tostring(rec.where.y) .. ')')"
    , "end"
    ]

-- | Run one case's chunk on top of 'prelude' in a fresh interpreter. The
--   chunk asserts in Lua; a raised error becomes an hspec failure carrying
--   the Lua message.
runCase ∷ [Text] → Expectation
runCase body = do
    result ← Lua.run $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 (T.unlines (prelude ⧺ body)))
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

-- | The mid-gesture ratio change every defect-exposing case uses: the
--   display drops out of HiDPI (2x → 1:1) while the button is still down.
dropToOneToOne ∷ Text
dropToOneToOne = "__fb = {1280, 720}"

spec ∷ Spec
spec = describe "drag-select deferred capture (#1676)" $ do

    describe "a deferred click" $ do
        it "reports the press's press-time framebuffer point after the ratio changes mid-gesture" $
            runCase
                [ "ds.handleMouseDown(1, 75, 32)"
                , "ds.deferClick(1, 'select_unit', 'accepted', 75, 32, nil)"
                , dropToOneToOne
                -- 2.24 window pixels: below DRAG_THRESHOLD, so this
                -- resolves as the deferred click.
                , "ds.onMouseUp(1, 77, 33, 'game')"
                , "local rec = only()"
                , "assert(rec.kind == 'input.click', rec.kind)"
                , "assert(rec.handler == 'select_unit', tostring(rec.handler))"
                -- Converting the retained window press (75, 32) at the
                -- now-1:1 release ratio would report (75, 32).
                , "at(rec, 150, 64)"
                ]

        it "does the same for the right button's independent pending click" $
            runCase
                [ "ds.handleMouseDown(2, 75, 32)"
                , "ds.deferClick(2, 'context_menu', 'accepted', 75, 32, nil)"
                , dropToOneToOne
                , "ds.onMouseUp(2, 77, 33, 'game')"
                , "local rec = only()"
                , "assert(rec.kind == 'input.click', rec.kind)"
                , "assert(rec.handler == 'context_menu', tostring(rec.handler))"
                , "at(rec, 150, 64)"
                ]

        it "is flushed by a view-transition cancel at the same press-time point" $
            -- dragSelect.cancel's other branch: the gesture never
            -- reached "dragging", so the pending click is what resolves.
            runCase
                [ "ds.handleMouseDown(1, 75, 32)"
                , "ds.deferClick(1, 'select_unit', 'accepted', 75, 32, nil)"
                , dropToOneToOne
                , "ds.cancel()"
                , "local rec = only()"
                , "assert(rec.kind == 'input.click', rec.kind)"
                , "at(rec, 150, 64)"
                ]

    -- The reviewer's amendment: left and right maintain separate state
    -- AND separate start coordinates, so one cancellation case cannot
    -- protect both branches.
    describe "a view-transition cancel of a real drag" $ do
        it "reports where the LEFT drag began, not where the ratio later put it" $
            runCase
                [ "ds.handleMouseDown(1, 75, 32)"
                , "ds.armBoxSelect()"
                -- The real tick is what promotes "pressed" to
                -- "dragging"; driving it keeps this off a hand-set flag.
                , "__mouse = {200, 150}"
                , "ds.update(0.03)"
                , "assert(ds.state == 'dragging', ds.state)"
                , dropToOneToOne
                , "ds.cancel()"
                , "local rec = only()"
                , "assert(rec.kind == 'input.drag', rec.kind)"
                , "assert(rec.reason == 'cancelled (view transition)', tostring(rec.reason))"
                , "at(rec, 150, 64)"
                , "assert(ds.state == 'idle', ds.state)"
                ]

        it "reports where the RIGHT drag began, from its own start coordinates" $
            runCase
                -- Deliberately a different origin from the left case, so
                -- a right-button record built from the left button's
                -- capture would fail rather than coincide.
                [ "ds.handleMouseDown(2, 300, 200)"
                , "__mouse = {400, 250}"
                , "ds.update(0.03)"
                , "assert(ds.rightState == 'dragging', ds.rightState)"
                , dropToOneToOne
                , "ds.cancel()"
                , "local rec = only()"
                , "assert(rec.kind == 'input.drag', rec.kind)"
                , "assert(rec.reason == 'cancelled (view transition)', tostring(rec.reason))"
                , "at(rec, 600, 400)"
                , "assert(ds.rightState == 'idle', ds.rightState)"
                ]

    describe "the degenerate-viewport fallback" $ do
        it "keeps a press captured under a zero framebuffer raw, however the ratio later recovers" $
            -- Requirement 6: the fallback is decided at capture. Unlike
            -- the engine, whose press routing swallows a degenerate
            -- press before anything is deferred, this module has no such
            -- guard — a press taken while the window is minimized really
            -- does reach deferClick.
            runCase
                [ "__fb = {0, 0}"
                , "ds.handleMouseDown(1, 75, 32)"
                , "ds.deferClick(1, 'select_unit', 'accepted', 75, 32, nil)"
                , "__fb = {2560, 1440}"
                , "ds.onMouseUp(1, 77, 33, 'game')"
                , "local rec = only()"
                , "at(rec, 75, 32)"
                ]

        it "keeps a drag origin captured under a zero WINDOW size raw too" $
            -- The all-four-dimensions guard, not just the framebuffer
            -- half: a zero window size divides by zero the other way.
            runCase
                [ "__win = {0, 0}"
                , "ds.handleMouseDown(1, 75, 32)"
                , "ds.armBoxSelect()"
                , "__mouse = {200, 150}"
                , "ds.update(0.03)"
                , "__win, __fb = {1280, 720}, {2560, 1440}"
                , "ds.cancel()"
                , "local rec = only()"
                , "at(rec, 75, 32)"
                ]

    describe "a resolution at its own event still converts live" $ do
        it "records a committed drag's RELEASE point under release-time geometry" $
            -- Preservation. The release location is contemporaneous with
            -- the conversion, so #1676 must leave it alone; nothing else
            -- in this spec would notice it being changed to the capture.
            runCase
                [ "unit.hitTestInRect = function() return {} end"
                , "unit.setSelection = function() end"
                , "unit.getSelected = function() return {} end"
                , "ds.handleMouseDown(1, 75, 32)"
                , "ds.armBoxSelect()"
                , "__mouse = {200, 150}"
                , "ds.update(0.03)"
                , dropToOneToOne
                , "ds.onMouseUp(1, 200, 150, 'game')"
                , "local rec = only()"
                , "assert(rec.kind == 'input.drag', rec.kind)"
                -- The 1:1 release ratio: neither the capture (150, 64)
                -- nor the release under the press-time 2x ratio.
                , "at(rec, 200, 150)"
                ]
