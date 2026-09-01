{-# LANGUAGE Strict #-}
-- | Chop's press-drag gesture (#1856), driven through the REAL
--   @scripts/chop_tool.lua@ and @scripts/unit_drag_select.lua@ in a
--   stdlib-only Lua VM.
--
--   The engine-side half — which trees a click or a box selects — is
--   'Test.Headless.World.Chop.Selection'. What is under test HERE is
--   the gesture: that exactly one outcome is recorded per press, that
--   the four-pixel boundary falls where #730's shared threshold puts
--   it, that a gesture completing between two update ticks still
--   classifies correctly, that left adds while right erases through the
--   same machinery, that both buttons claim their press so nothing
--   falls through to unit selection, and that focus loss, a tool switch
--   and a view transition each leave nothing half-committed.
--
--   No engine boots: the four @chop.*@ verbs are stubbed so each case
--   can read exactly which one the gesture called and with what.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Chop gesture"'@.
module Test.Headless.Lua.ChopGesture (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua

-- | Stubs plus the two real modules.
--
--   @__calls@ records every @chop.*@ verb the gesture reached, so a case
--   can assert both that the right verb fired and that nothing else
--   did. @__records@ collects the F4 action outcomes. @edgeIds = {}@
--   makes the rect visual a no-op without stubbing @UI@ (the same
--   trick 'Test.Headless.Lua.DragSelectDeferred' uses).
prelude ∷ [Text]
prelude =
    [ "__win, __fb, __mouse = {1280,720}, {1280,720}, {0,0}"
    , "__records, __calls, __selected = {}, {}, 1"
    , "engine = {"
    , "  getWindowSize = function() return __win[1], __win[2] end,"
    , "  getFramebufferSize = function() return __fb[1], __fb[2] end,"
    , "  getMousePosition = function() return __mouse[1], __mouse[2] end,"
    , "  logInfo = function() end, logDebug = function() end,"
    , "  isKeyDown = function() return false end }"
    , "debug.recordOutcome = function(rec) __records[#__records+1] = rec end"
    , "local function note(name)"
    , "  return function(...)"
    , "    __calls[#__calls+1] = { name = name, args = {...} }"
    , "    return __selected"
    , "  end"
    , "end"
    , "chop = { designateAt = note('designateAt'),"
    , "         designateInRect = note('designateInRect'),"
    , "         eraseAt = note('eraseAt'),"
    , "         eraseInRect = note('eraseInRect') }"
    , "world = { getToolMode = function() return 'chop' end,"
    , "          clearWorldCursorSelect = function() end }"
    , "unit, item, building, UI = {}, {}, {}, {}"
    , "package.loaded['scripts.hud'] = { worldId = 'w', currentView = 'zoomed_in' }"
    , "ds = require('scripts.unit_drag_select')"
    , "ds.edgeIds = {}"
    , "ct = require('scripts.chop_tool')"
    , "ct.setup({ hud = package.loaded['scripts.hud'] })"
    -- The two helpers every case shares.
    , "function only()"
    , "  assert(#__records == 1, 'expected one record, got ' .. #__records)"
    , "  return __records[1]"
    , "end"
    , "function onlyCall()"
    , "  assert(#__calls == 1, 'expected one chop call, got ' .. #__calls)"
    , "  return __calls[1]"
    , "end"
    -- One whole press → release, with the cursor moved to (x2, y2)
    -- before the release so update() sees the motion.  `ticks` says
    -- whether dragSelect.update runs at all: a gesture that completes
    -- between two ticks must still classify from its coordinates.
    , "function gesture(button, x1, y1, x2, y2, ticks, route)"
    , "  ds.handleMouseDown(button, x1, y1)"
    , "  assert(ct.handleMouseDown(button, x1, y1), 'chop_tool did not claim')"
    , "  ds.deferClick(button, 'chop_tool', nil, x1, y1, nil)"
    , "  if ticks then __mouse = {x2, y2}; ds.update(0.03) end"
    , "  ds.onMouseUp(button, x2, y2, route or 'game')"
    , "end"
    ]

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

spec ∷ Spec
spec = describe "Chop gesture" $ do

    describe "click versus drag resolves to exactly one outcome" $ do

        it "a press and release at the same point is a click" $
            runCase
                [ "gesture(1, 100, 100, 100, 100, true)"
                , "local c = onlyCall()"
                , "assert(c.name == 'designateAt', c.name)"
                , "assert(c.args[1] == 'w' and c.args[2] == 100"
                , "       and c.args[3] == 100, 'bad click args')"
                , "local rec = only()"
                , "assert(rec.kind == 'input.click', rec.kind)"
                ]

        it "motion below four pixels is a click" $
            -- 3 window pixels along one axis: strictly below the
            -- threshold, so the shared classifier calls it a click.
            runCase
                [ "gesture(1, 100, 100, 103, 100, true)"
                , "assert(onlyCall().name == 'designateAt', 'expected a click')"
                , "assert(only().kind == 'input.click')"
                ]

        it "motion of exactly four pixels is a drag" $
            -- The boundary itself: #730's comparison is >=, so four
            -- pixels is already a box.
            runCase
                [ "gesture(1, 100, 100, 104, 100, true)"
                , "local c = onlyCall()"
                , "assert(c.name == 'designateInRect', c.name)"
                , "assert(c.args[2] == 100 and c.args[3] == 100"
                , "       and c.args[4] == 104 and c.args[5] == 100,"
                , "       'the box must be the press-to-release rect')"
                , "assert(only().kind == 'input.drag')"
                ]

        it "classifies a fast drag completed between two update ticks" $
            -- No dragSelect.update runs at all, so the state machine
            -- never leaves "pressed". The classification is computed
            -- from the press/release coordinates regardless.
            runCase
                [ "gesture(1, 100, 100, 300, 260, false)"
                , "assert(onlyCall().name == 'designateInRect', 'expected a box')"
                , "assert(only().kind == 'input.drag')"
                ]

        it "records the diagonal threshold at the same four-pixel radius" $
            -- (3, 2) is 3.6 px — a click. (3, 3) is 4.24 — a drag.
            runCase
                [ "gesture(1, 100, 100, 103, 102, true)"
                , "assert(onlyCall().name == 'designateAt', 'expected a click')"
                , "__calls, __records = {}, {}"
                , "gesture(1, 200, 200, 203, 203, true)"
                , "assert(onlyCall().name == 'designateInRect', 'expected a box')"
                ]

    describe "left adds and right erases, symmetrically" $ do

        it "a right click erases at the pointer" $
            runCase
                [ "gesture(2, 100, 100, 100, 100, true)"
                , "local c = onlyCall()"
                , "assert(c.name == 'eraseAt', c.name)"
                , "assert(only().kind == 'input.click')"
                ]

        it "a right drag erases over the identical box a left drag would add" $
            runCase
                [ "gesture(1, 40, 50, 200, 180, true)"
                , "local add = onlyCall()"
                , "__calls, __records = {}, {}"
                , "gesture(2, 40, 50, 200, 180, true)"
                , "local erase = onlyCall()"
                , "assert(add.name == 'designateInRect' and"
                , "       erase.name == 'eraseInRect', 'wrong verbs')"
                , "for i = 1, 5 do"
                , "  assert(add.args[i] == erase.args[i],"
                , "    'box argument ' .. i .. ' differs')"
                , "end"
                ]

        it "passes the wood tag on every one of the four verbs" $
            runCase
                [ "local seen = {}"
                , "gesture(1, 10, 10, 10, 10, true); seen[1] = onlyCall()"
                , "__calls, __records = {}, {}"
                , "gesture(1, 10, 10, 90, 90, true); seen[2] = onlyCall()"
                , "__calls, __records = {}, {}"
                , "gesture(2, 10, 10, 10, 10, true); seen[3] = onlyCall()"
                , "__calls, __records = {}, {}"
                , "gesture(2, 10, 10, 90, 90, true); seen[4] = onlyCall()"
                , "assert(seen[1].args[4] == 'wood', 'designateAt tag')"
                , "assert(seen[2].args[6] == 'wood', 'designateInRect tag')"
                , "assert(seen[3].args[4] == 'wood', 'eraseAt tag')"
                , "assert(seen[4].args[6] == 'wood', 'eraseInRect tag')"
                ]

    describe "input claiming" $ do

        it "claims both buttons while the tool is active" $
            runCase
                [ "assert(ct.handleMouseDown(1, 5, 5), 'left not claimed')"
                , "assert(ct.handleMouseDown(2, 5, 5), 'right not claimed')"
                ]

        it "claims neither when another tool mode is selected" $
            runCase
                [ "world.getToolMode = function() return 'mine' end"
                , "assert(not ct.handleMouseDown(1, 5, 5), 'left claimed')"
                , "assert(not ct.handleMouseDown(2, 5, 5), 'right claimed')"
                ]

        it "claims neither outside the zoomed-in view" $
            runCase
                [ "package.loaded['scripts.hud'].currentView = 'zoomed_out'"
                , "assert(not ct.handleMouseDown(1, 5, 5), 'left claimed')"
                , "assert(not ct.handleMouseDown(2, 5, 5), 'right claimed')"
                ]

        it "never arms a unit box-selection" $
            -- #730 review round 6: a tool claim must not reach the
            -- unit/item/building selection commit path.
            runCase
                [ "gesture(1, 10, 10, 200, 200, true)"
                , "assert(ds.boxSelectArmed == false, 'box select armed')"
                ]

    describe "the outcome is reported honestly" $ do

        it "a gesture that selected nothing records a noop" $
            runCase
                [ "__selected = 0"
                , "gesture(1, 10, 10, 200, 200, true)"
                , "local rec = only()"
                , "assert(rec.kind == 'input.drag', rec.kind)"
                , "assert(rec.outcome == 'noop', tostring(rec.outcome))"
                , "assert(rec.applied == 0, tostring(rec.applied))"
                ]

        it "a productive gesture records what it applied" $
            runCase
                [ "__selected = 7"
                , "gesture(1, 10, 10, 200, 200, true)"
                , "local rec = only()"
                , "assert(rec.outcome == 'accepted', tostring(rec.outcome))"
                , "assert(rec.applied == 7 and rec.requested == 7,"
                , "       'applied/requested must report the selection')"
                ]

        it "a click that found no tree records a noop, not an accept" $
            runCase
                [ "__selected = 0"
                , "gesture(1, 10, 10, 10, 10, true)"
                , "local rec = only()"
                , "assert(rec.kind == 'input.click', rec.kind)"
                , "assert(rec.outcome == 'noop', tostring(rec.outcome))"
                ]

    describe "cancellation leaves nothing half-committed" $ do

        it "a focus-loss release commits nothing" $
            -- A synthetic release routed "swallowed" (Engine.Input.Thread
            -- on focus loss / minimize) must not designate at whatever
            -- stale coordinate the cursor was left at.
            runCase
                [ "gesture(1, 10, 10, 200, 200, true, 'swallowed')"
                , "assert(#__calls == 0, 'a swallowed release designated')"
                , "local rec = only()"
                , "assert(rec.outcome == 'noop', tostring(rec.outcome))"
                ]

        it "the same holds for the right button's erase" $
            runCase
                [ "gesture(2, 10, 10, 200, 200, true, 'swallowed')"
                , "assert(#__calls == 0, 'a swallowed release erased')"
                ]

        it "a view transition abandons an in-flight box" $
            runCase
                [ "ds.handleMouseDown(1, 10, 10)"
                , "ct.handleMouseDown(1, 10, 10)"
                , "ds.deferClick(1, 'chop_tool', nil, 10, 10, nil)"
                , "__mouse = {200, 200}; ds.update(0.03)"
                , "ds.cancel()"
                , "assert(#__calls == 0, 'a cancelled box designated')"
                , "assert(ds.toolBox[1] == nil, 'the effect survived cancel')"
                -- A release arriving after the teardown must not
                -- resurrect it.
                , "__records = {}"
                , "ds.onMouseUp(1, 200, 200, 'game')"
                , "assert(#__calls == 0, 'a post-cancel release designated')"
                ]

        it "a tool switch disarms both buttons" $
            runCase
                [ "ds.handleMouseDown(1, 10, 10); ct.handleMouseDown(1, 10, 10)"
                , "ds.handleMouseDown(2, 10, 10); ct.handleMouseDown(2, 10, 10)"
                , "ct.onToolMode('tool_mine')"
                , "assert(ds.toolBox[1] == nil and ds.toolBox[2] == nil,"
                , "       'a tool switch left an armed box')"
                , "ds.onMouseUp(1, 200, 200, 'game')"
                , "ds.onMouseUp(2, 200, 200, 'game')"
                , "assert(#__calls == 0, 'a disarmed gesture still committed')"
                ]

        it "Escape disarms without claiming the key" $
            runCase
                [ "ds.handleMouseDown(1, 10, 10); ct.handleMouseDown(1, 10, 10)"
                , "assert(ct.handleKeyDown('Escape') == false,"
                , "       'Escape must fall through — there is no anchor')"
                , "assert(ds.toolBox[1] == nil, 'Escape left an armed box')"
                ]

        it "resolves each gesture exactly once across a whole sequence" $
            runCase
                [ "gesture(1, 10, 10, 10, 10, true)"
                , "gesture(2, 20, 20, 200, 200, true)"
                , "gesture(1, 30, 30, 300, 300, false)"
                , "assert(#__records == 3, 'expected 3 records, got '"
                , "       .. #__records)"
                , "assert(#__calls == 3, 'expected 3 calls, got ' .. #__calls)"
                , "assert(__calls[1].name == 'designateAt', __calls[1].name)"
                , "assert(__calls[2].name == 'eraseInRect', __calls[2].name)"
                , "assert(__calls[3].name == 'designateInRect', __calls[3].name)"
                ]
