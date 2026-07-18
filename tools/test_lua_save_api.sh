#!/usr/bin/env bash
# Smoke-test the engine.saveWorld / engine.loadSave Lua API for correct
# error reporting. Boots a headless engine on a non-default port, runs
# assertions over the TCP debug console, tears down. Exits 0 on all-pass.
#
# Usage:  tools/test_lua_save_api.sh
#
# Why this exists: hspec covers pure code, but the Lua API surface
# spans the Lua thread, world thread, and disk. End-to-end behaviour
# (does saveWorld actually return false when validation fails?) is
# only testable by driving the running engine, which is exactly what
# the headless debug console is for.

set -u
PORT=9008
LOG=/tmp/test_lua_save_engine.log
TEST_SAVE_NAME=smoke_test_save_$$  # PID-suffixed so concurrent runs don't clash
SECOND_SAVE_NAME=${TEST_SAVE_NAME}_two

# ── Setup ────────────────────────────────────────────────────────────
HPID=""
cleanup() {
    if [ -n "$HPID" ]; then
        echo 'engine.quit()' | nc -w 2 localhost $PORT >/dev/null 2>&1 || true
        wait $HPID 2>/dev/null || true
    fi
    rm -rf "saves/${TEST_SAVE_NAME}" "saves/${SECOND_SAVE_NAME}" \
        "saves/${TEST_SAVE_NAME}_broken" "saves/${TEST_SAVE_NAME}_recovered" 2>/dev/null
}
trap cleanup EXIT

cabal run -v0 exe:synarchy -- --headless --port $PORT > $LOG 2>&1 &
HPID=$!

# Wait for READY (printed by Engine.Core.Loop when TCP listener is up)
until grep -q "READY" $LOG 2>/dev/null; do
    sleep 0.2
    if ! kill -0 $HPID 2>/dev/null; then
        echo "FAIL: engine died during startup. Log:"
        cat $LOG
        exit 1
    fi
done

# ── Assertion helper ─────────────────────────────────────────────────
FAIL=0
PASS=0
lua() {
    # Send one Lua line, return its single-line stdout response with the
    # debug-console prompt prefix and trailing next-prompt suffix stripped.
    #   raw: "synarchy debug console> false> "
    #   cleaned: "false"
    # Optional second arg = nc timeout in seconds (default 5).
    local cmd="$1" timeout="${2:-5}"
    echo "$cmd" | nc -w "$timeout" localhost $PORT \
        | tr -d '\r\n' \
        | sed -E 's/synarchy debug console>[[:space:]]*//g; s/[[:space:]]*>[[:space:]]*$//; s/^[[:space:]]*//; s/[[:space:]]*$//'
}

assert_eq() {
    local desc="$1" expected="$2" cmd="$3"
    local got
    got=$(lua "return $cmd")
    if [ "$got" = "$expected" ]; then
        echo "  PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $desc"
        echo "        cmd:      $cmd"
        echo "        expected: [$expected]"
        echo "        got:      [$got]"
        FAIL=$((FAIL + 1))
    fi
}

# ── Tests ────────────────────────────────────────────────────────────
echo "[1] saveWorld validation rejects bad inputs (no world initialized)"
assert_eq "missing page"          "false" "engine.saveWorld('nonexistent_page', 'test')"
assert_eq "empty save name"       "false" "engine.saveWorld('any', '')"
assert_eq "traversal in name"     "false" "engine.saveWorld('any', '../injected')"
assert_eq "absolute path in name" "false" "engine.saveWorld('any', '/tmp/owned')"
assert_eq "backslash in name"     "false" "engine.saveWorld('any', 'bad\\\\name')"
assert_eq "leading dot"           "false" "engine.saveWorld('any', '.hidden')"
assert_eq "65 char name"          "false" \
    "engine.saveWorld('any', string.rep('a', 65))"

echo "[2] Initialize a small world for the success path"
lua "world.init('test', 42, 64, 1)" > /dev/null
INIT_RESULT=$(lua "return world.waitForInit(120)" 120)
echo "  world.waitForInit returned: [$INIT_RESULT]"
lua "world.show('test')" > /dev/null
# Ensure unpaused starting state for the pause-on-save check below.
lua "engine.setPaused(false)" > /dev/null
assert_eq "engine starts unpaused"  "false" "engine.isPaused()"

echo "[3] saveWorld accepts a valid name on an initialized world"
assert_eq "good save"             "true"  "engine.saveWorld('test', '${TEST_SAVE_NAME}')"
assert_eq "engine paused after save" "true" "engine.isPaused()"
# Reset before the next save assertion so we can re-verify the pause-flip.
lua "engine.setPaused(false)" > /dev/null
assert_eq "good save can repeat"  "true"  "engine.saveWorld('test', '${TEST_SAVE_NAME}')"
assert_eq "engine paused after repeat" "true" "engine.isPaused()"

echo "[4] saveWorld still rejects bad inputs on an initialized world"
# Validation failures should NOT pause the engine (no side effects on reject).
# Drain any pending WorldSave from [3] — the world thread's defense-in-depth
# pause-write at Save.hs:60 lands async and would otherwise race with our
# setPaused(false). 0.5s is plenty for a 64-tile-world's save handler.
sleep 0.5
lua "engine.setPaused(false)" > /dev/null
assert_eq "bad name post-init"    "false" "engine.saveWorld('test', '../still_bad')"
assert_eq "no pause on bad name"  "false" "engine.isPaused()"
assert_eq "wrong page post-init"  "false" "engine.saveWorld('nonexistent', '${TEST_SAVE_NAME}_x')"
assert_eq "no pause on wrong page" "false" "engine.isPaused()"

echo "[5] saved file has ISO 8601 timestamp populated"
# Iterate engine.listSaves(), find our test save, verify its timestamp
# matches the YYYY-MM-DDTHH:MM:SSZ pattern produced at save time.
assert_eq "timestamp is ISO 8601" "true" \
    "(function() for _, s in ipairs(engine.listSaves()) do if s.name == '${TEST_SAVE_NAME}' then return s.timestamp:match('^%d%d%d%d%-%d%d%-%d%dT%d%d:%d%d:%d%dZ\$') ~= nil end end return false end)()"

echo "[6] pause.toggle works on first press after a direct setPaused"
# Simulate the auto-pause-on-save desync condition: engine flag flipped
# directly (without going through pause.set), Lua mirror untouched.
# Pre-fix: the mirror is false while engine is true, so pause.toggle's
# first call is a no-op visually. Post-fix: pause.set/toggle check the
# engine flag, so first toggle correctly unpauses.
lua "engine.setPaused(true)" > /dev/null
sleep 0.3
assert_eq "engine is paused"          "true"  "engine.isPaused()"
lua "require('scripts.pause').toggle()" > /dev/null
assert_eq "first toggle unpauses"     "false" "engine.isPaused()"
lua "require('scripts.pause').toggle()" > /dev/null
assert_eq "second toggle re-pauses"   "true"  "engine.isPaused()"

echo "[7] listSaves returns saves newest-first"
# Create a second save 1.1s after the first so its ISO 8601 second-
# precision timestamp is strictly greater. saveWorldFn now captures
# the timestamp at API request time (not world-thread processing
# time) so the on-disk order is independent of world-thread queue
# latency.
lua "engine.setPaused(false)" > /dev/null
sleep 1.1
assert_eq "second save created"   "true" "engine.saveWorld('test', '${SECOND_SAVE_NAME}')"
# engine.saveWorld returns true once VALIDATION passes and the
# WorldSave command is queued — the actual disk write happens
# asynchronously on the world thread. Poll listSaves from bash
# until both names appear before checking the sort order,
# otherwise we race the writer.
LIST_QUERY="local list = engine.listSaves(); local n, o = false, false; for _, s in ipairs(list) do if s.name == '${SECOND_SAVE_NAME}' then n = true end; if s.name == '${TEST_SAVE_NAME}' then o = true end; end; return n and o"
for _i in $(seq 1 40); do
    if [ "$(lua "return ${LIST_QUERY}")" = "true" ]; then
        break
    fi
    sleep 0.25
done
assert_eq "newer save sorts first" "true" \
    "(function() local list = engine.listSaves(); local n_idx, o_idx = -1, -1; for i, s in ipairs(list) do if s.name == '${SECOND_SAVE_NAME}' then n_idx = i end; if s.name == '${TEST_SAVE_NAME}' then o_idx = i end; end; return n_idx > 0 and o_idx > 0 and n_idx < o_idx end)()"

echo "[8] camera transients reset on load"
# Set a non-zero zoom velocity, then loadSave the test save. The fix
# uses Camera2D record-construction (vs record-update) so every
# transient field is explicitly initialized. Pre-fix, the loaded
# camera would inherit the prior camera's zoom velocity / drag state.
# Only camZoomVelocity is exposed via Lua, but it's a representative
# witness for the whole class of transients.
lua "camera.setZoomVelocity(5.0)" > /dev/null
assert_eq "zoom velocity pre-load"   "5"     "math.floor(camera.getZoomVelocity())"
assert_eq "loadSave returns true"    "true"  "engine.loadSave('${TEST_SAVE_NAME}')"
# Load handler is async — block on the load progress phaseRef instead of
# sleeping (it's shared with init's phaseRef; phase=3 means "done").
LOAD_RESULT=$(lua "return world.waitForInit(60)" 60)
echo "  waitForInit (post-load) returned: [$LOAD_RESULT]"
assert_eq "zoom velocity post-load"  "0"     "math.floor(camera.getZoomVelocity())"

echo "[9] engine.emitEvent appends to the log ring buffer"
# Snapshot count, fire one event, verify count incremented and the
# newest entry has the correct category + text. getEventLog returns
# oldest-first, so the new entry is at the end.
PRE_LOG_COUNT=$(lua "return #engine.getEventLog()")
lua "engine.emitEvent('save_load', 'smoke event A')" > /dev/null
sleep 0.2
assert_eq "log grew by one" "$((PRE_LOG_COUNT + 1))" "#engine.getEventLog()"
assert_eq "newest entry category" "true" \
    "engine.getEventLog()[#engine.getEventLog()].category == 'save_load'"
assert_eq "newest entry text" "true" \
    "engine.getEventLog()[#engine.getEventLog()].text == 'smoke event A'"

echo "[10] survival_critical pauses the engine"
# Pause is a per-category default in data/notification_categories.yaml;
# survival_critical has pause: true.
lua "engine.setPaused(false)" > /dev/null
sleep 0.2
assert_eq "engine starts unpaused"   "false" "engine.isPaused()"
lua "engine.emitEvent('survival_critical', 'simulated death')" > /dev/null
sleep 0.2
assert_eq "engine paused after emit" "true"  "engine.isPaused()"
lua "engine.setPaused(false)" > /dev/null
sleep 0.2

echo "[11] unknown category drops without growing the log"
# Loud-fail path: dev warning to the engine log, event dropped.
PRE_LOG_COUNT=$(lua "return #engine.getEventLog()")
lua "engine.emitEvent('does_not_exist', 'should be dropped')" > /dev/null
sleep 0.2
assert_eq "log unchanged on unknown cat" "$PRE_LOG_COUNT" "#engine.getEventLog()"

echo "[12] popup module queues onShowPopup broadcasts"
# Headless mode never bootstraps the popup (no fonts/textures), so
# emitted events stay queued — exactly the right shape to verify that
# the engine→Lua broadcast hit the popup module's onShowPopup handler.
PRE_QUEUE=$(lua "return require('scripts.popup').queueLength()")
lua "engine.emitEvent('save_load', 'popup queue probe')" > /dev/null
sleep 0.2
assert_eq "popup queue grew" "$((PRE_QUEUE + 1))" \
    "require('scripts.popup').queueLength()"

echo "[13] engine.getNotificationCfg returns 10 categories in registry order"
assert_eq "category count" "10" "#engine.getNotificationCfg()"
assert_eq "first id is save_load"        "true" \
    "engine.getNotificationCfg()[1].id == 'save_load'"
assert_eq "second id is survival_critical" "true" \
    "engine.getNotificationCfg()[2].id == 'survival_critical'"
assert_eq "save_load default popup=true" "true" \
    "engine.getNotificationCfg()[1].popup"
assert_eq "survival_critical default pause=true" "true" \
    "engine.getNotificationCfg()[2].pause"
assert_eq "save_load text_color.r is 1.0" "true" \
    "math.abs(engine.getNotificationCfg()[1].textColor.r - 1.0) < 0.001"

echo "[14] setNotificationOverrides changes emit routing"
# Disable save_load.popup, fire an event, verify popup queue did NOT grow.
lua "engine.setNotificationOverrides({save_load = {popup = false}})" > /dev/null
sleep 0.2
assert_eq "save_load.popup is false" "false" \
    "engine.getNotificationCfg()[1].popup"
PRE_QUEUE=$(lua "return require('scripts.popup').queueLength()")
lua "engine.emitEvent('save_load', 'popup-disabled probe')" > /dev/null
sleep 0.2
assert_eq "popup queue unchanged when popup=false" "$PRE_QUEUE" \
    "require('scripts.popup').queueLength()"
# But the log still grew (save_load.log is still true).
assert_eq "log still grew" "true" \
    "engine.getEventLog()[#engine.getEventLog()].text == 'popup-disabled probe'"

echo "[15] setNotificationOverrides ignores unknown category"
# Should return true (call shape valid) but log a warning. No crash.
assert_eq "unknown category returns true" "true" \
    "engine.setNotificationOverrides({nope_not_real = {log = true}})"

echo "[16] Restore save_load.popup=true for cross-run cleanliness"
# Leaves the YAML overrides file in the same shape the test started in.
lua "engine.setNotificationOverrides({save_load = {popup = true}})" > /dev/null
sleep 0.2
assert_eq "save_load.popup restored" "true" \
    "engine.getNotificationCfg()[1].popup"

echo "[17] emitEventAt routes coords through the popup queue"
# Engine pushes onShowPopup with coords; popup module stashes them
# in the queued entry for the spawn step.
lua "engine.setPaused(false)" > /dev/null
PRE_QUEUE=$(lua "return require('scripts.popup').queueLength()")
lua "engine.emitEventAt('survival_critical', 'death at (10,20)', 10, 20)" > /dev/null
sleep 0.2
assert_eq "popup queue grew with coords event" "$((PRE_QUEUE + 1))" \
    "require('scripts.popup').queueLength()"
# Inspect the most-recently-queued entry: it should carry coords.
assert_eq "queued entry has coords.x=10" "true" \
    "(function() local q = require('scripts.popup').queue; local e = q[#q]; return e.coords ~= nil and e.coords.x == 10 end)()"
assert_eq "queued entry has coords.y=20" "true" \
    "(function() local q = require('scripts.popup').queue; local e = q[#q]; return e.coords ~= nil and e.coords.y == 20 end)()"

echo "[18] emitEvent (no coords) leaves coords nil"
PRE_QUEUE=$(lua "return require('scripts.popup').queueLength()")
lua "engine.emitEvent('save_load', 'plain save event')" > /dev/null
sleep 0.2
assert_eq "queue grew without coords" "$((PRE_QUEUE + 1))" \
    "require('scripts.popup').queueLength()"
assert_eq "queued entry has no coords" "true" \
    "(function() local q = require('scripts.popup').queue; return q[#q].coords == nil end)()"

echo "[19] engine.realTime returns a POSIX-shaped double"
# We just check it's a number and >= a sanity floor; the actual
# value is wall-clock so it shifts every run.
assert_eq "realTime is a number" "true" \
    "type(engine.realTime()) == 'number'"
assert_eq "realTime is a recent timestamp" "true" \
    "engine.realTime() > 1700000000"

echo "[20] coalesce_window exposed via getNotificationCfg"
# save_load has no coalesce_window in YAML → default 0
assert_eq "save_load coalesceWindow is 0"  "true" \
    "engine.getNotificationCfg()[1].coalesceWindow == 0"
# survival_critical = 1.0 in YAML
assert_eq "survival_critical coalesceWindow is 1.0" "true" \
    "math.abs(engine.getNotificationCfg()[2].coalesceWindow - 1.0) < 0.001"
# combat = 2.0 in YAML
assert_eq "combat coalesceWindow is 2.0" "true" \
    "math.abs(engine.getNotificationCfg()[4].coalesceWindow - 2.0) < 0.001"

echo "[21] popup module exposes coalesce-test helpers"
assert_eq "onLineClick is a function" "true" \
    "type(require('scripts.popup').onLineClick) == 'function'"
assert_eq "activeLineCount is a function" "true" \
    "type(require('scripts.popup').activeLineCount) == 'function'"
assert_eq "activeLastLineCount is a function" "true" \
    "type(require('scripts.popup').activeLastLineCount) == 'function'"

echo "[22] a REQUIRED Lua save component's snapshot failure aborts the \
whole save (issue #761 requirement 6)"
# Temporarily break unit_ai's registered snapshot function so
# saveModules.snapshotAll() reports {ok=false}. engine.saveWorld must
# then return false and never queue a WorldSave command -- no partial
# save, no stale barrier. Stash the original in a Lua global so it
# survives across separate debug-console connections.
lua "engine.setPaused(false)" > /dev/null
lua "local sm = require('scripts.lib.save_modules'); \
     _G.__smoke_orig_snapshot = sm.registry.unit_ai.snapshot; \
     sm.registry.unit_ai.snapshot = function() error('smoke-injected failure') end" > /dev/null
assert_eq "save fails when a required component's snapshot throws" \
    "false" "engine.saveWorld('test', '${TEST_SAVE_NAME}_broken')"
assert_eq "no save directory was created for the aborted save" \
    "false" \
    "(function() for _, s in ipairs(engine.listSaves()) do if s.name == '${TEST_SAVE_NAME}_broken' then return true end end return false end)()"
lua "local sm = require('scripts.lib.save_modules'); \
     sm.registry.unit_ai.snapshot = _G.__smoke_orig_snapshot; \
     _G.__smoke_orig_snapshot = nil" > /dev/null
sleep 0.3
assert_eq "a normal save still succeeds once restored" "true" \
    "engine.saveWorld('test', '${TEST_SAVE_NAME}_recovered')"

# ── Report ───────────────────────────────────────────────────────────
echo ""
echo "=========================================="
echo "Passed: $PASS   Failed: $FAIL"
echo "=========================================="
exit $FAIL
