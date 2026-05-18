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
    rm -rf "saves/${TEST_SAVE_NAME}" "saves/${SECOND_SAVE_NAME}" 2>/dev/null
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
# precision timestamp is strictly greater. Pre-fix, listSaves returned
# filesystem order; post-fix, sortBy (Down . timestamp) <> name puts
# the newer one first regardless of inode layout.
lua "engine.setPaused(false)" > /dev/null
sleep 1.1
assert_eq "second save created"   "true" "engine.saveWorld('test', '${SECOND_SAVE_NAME}')"
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

# ── Report ───────────────────────────────────────────────────────────
echo ""
echo "=========================================="
echo "Passed: $PASS   Failed: $FAIL"
echo "=========================================="
exit $FAIL
