#!/usr/bin/env python3
"""Headless probe for issues #618 and #665: `TE.decodeUtf8` call sites
across `Engine.Scripting.Lua` must not throw on a byte string that isn't
valid UTF-8.

`"caf\\195"` is Lua's decimal escape for the raw byte 0xC3 — a truncated
UTF-8 lead byte with no continuation byte, exactly the shape produced by
the byte-vs-codepoint bug in the five `truncateToWidth` ellipsis helpers
#618 fixes. Those five private copies are gone: #1088's shared item-list
widget merged three of them, #1107 moved the body into
`scripts/ui/text_wrap.lua`, and #1157 retired the popup's and the event
log's last two, so the one surviving implementation is
`textWrap.truncateToWidth`.

Note the Text-API case is a NARROWER regression than #622's, whose
invariant — Strict/StrictData on `Engine.Scripting.Lua.Types` forces every
`LuaToEngineMsg`/`LuaMsg` field at construction, inside
`registerLuaFunction`'s catch guard, so an exception hiding in a field can
never escape to the consuming thread and kill the engine — is gated by the
engine-free headless spec `Test.Headless.Lua.MessageStrictness`
(`--match "Lua message field strictness"`, #2161). No real-engine probe
can exercise that guard any more, because #618/#665 made this very path
lenient; the probe that used to send malformed UTF-8 through
`engine.setText` for #622 was retired for exactly that reason. That
strictness alone would still leave `setTextFn`'s `TE.decodeUtf8` throwing
a *caught* Lua error every single call — which is what #618's fix (switch
to `TE.decodeUtf8Lenient`, the codebase's established convention per
#437/PR #492) actually eliminates. The Text-API case below asserts the
stronger, issue-#618-specific property: no error at all, from
`setTextFn`'s own `TE.decodeUtf8Lenient` at the Lua argument boundary,
which runs before `Q.writeQueue` and is where the pre-fix throw happened.
The closing `engine_alive` + `console_responsive` checks stay here as the
real-engine survival evidence for that same malformed message.

That decode is ALL this probe can observe of the Text API's `setText`,
because #1961 made a `setText` naming an id with no scene node a genuine
no-op: it writes no scene-text cache entry, so `engine.getText` answers
nil for it. This probe deliberately does not spawn a text node -- it
cannot, since a headless engine has no active scene ("Cannot spawn text:
no active scene"; the default scene is created by
`Engine.Graphics.Vulkan.Init`) -- so ids 1 and 2 below name nothing, and
the checks assert exactly that NOTHING was cached rather than the
round-trip they used to require. Requiring a round-trip here would be
asserting the lifetime defect #1961 fixed. The engine-side coupling
between a cache entry and its scene node is covered by the headless
`Test.Headless.Lua.SceneText` spec, which can install a real scene.

#665 completed the same sweep across every remaining strict
`TE.decodeUtf8` call site under `src/Engine/Scripting/Lua/`, covering both
Lua-argument boundaries and other byte sources in that tree. The
`world.show` case below is the representative non-Text-API boundary for
that broader sweep (`Engine.Scripting.Lua.API.World.Lifecycle.worldShowFn`,
registered as Lua's `world.show`): it asserts malformed input no longer
raises a `Haskell exception in show: ...` guard error and instead proceeds
to `World.Thread.Command.UI.handleWorldShowCommand`'s normal semantic
handling (an unrecognized page id just logs a warning and is a no-op —
see `World/Thread/Command/UI.hs`), with the debug console still
responsive afterward.

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps human-readable
per-check output.

Usage: python3 tools/text_encoding_probe.py [--port 9618]
       python3 tools/text_encoding_probe.py --describe
Exit 0 = pass.
"""
from __future__ import annotations
import argparse
import sys

import probe_protocol
from probelib import boot, quit_engine, send

LOG = "/tmp/text_encoding_probe_engine.log"
LOG_NAME = "text_encoding_probe_engine.log"
PROBE_KEY = "text_encoding"

CHECKS = [
    ("well_formed_text_call", "well-formed engine.setText raises no error"),
    ("well_formed_text_uncached",
     "well-formed engine.setText on an id with no scene node caches nothing"),
    ("malformed_text_call", "malformed UTF-8 engine.setText raises no error"),
    ("malformed_text_uncached",
     "malformed engine.setText on an id with no scene node caches nothing"),
    ("well_formed_world_show", "well-formed world.show raises no error"),
    ("malformed_world_show", "malformed UTF-8 world.show raises no error"),
    ("engine_alive", "the engine remains alive after malformed UTF-8"),
    ("console_responsive",
     "the debug console remains responsive after malformed UTF-8"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9618)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 check declaration and "
                         "exit without booting an engine")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args, args.port, rep)
    finally:
        rep.close()


def _run(args, port, rep) -> int:
    port = args.port

    proc = boot(port, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    ok = True
    try:
        # setText with a well-formed string is the control case: must be
        # completely unaffected (requirement 4).
        result = send(port, 'engine.setText(1, "hello"); return "no_error"')
        call_ok = result == "no_error"
        ok &= rep.check(
            "well_formed_text_call", call_ok,
            ("well-formed setText raised no error" if call_ok else
             f"well-formed setText returned {result!r}"),
            {"result": result})
        # Id 1 names no scene node here, so post-#1961 the handler is a
        # genuine no-op and getText must answer nil (the console renders
        # that as the JSON literal `null`). This is the lifetime
        # contract, asserted on the same control case: the call is
        # accepted at the Lua boundary and changes nothing downstream.
        got = send(port, "return engine.getText(1)")
        uncached_ok = got == "null"
        ok &= rep.check(
            "well_formed_text_uncached", uncached_ok,
            ("no cache entry was created for an id with no scene node"
             if uncached_ok else
             f"unspawned id 1 read back as {got!r}, expected 'null'"),
            {"observed": got})

        # The malformed repro: a truncated multi-byte UTF-8 lead byte.
        # Pre-fix (TE.decodeUtf8), this throws -- caught (post-#622) as a
        # Lua error instead of crashing the process, but still an error on
        # every call. Post-fix (TE.decodeUtf8Lenient), it must succeed.
        result = send(port, 'engine.setText(2, "caf\\195"); return "no_error"')
        malformed_call_ok = result == "no_error"
        ok &= rep.check(
            "malformed_text_call", malformed_call_ok,
            ("malformed setText raised no error" if malformed_call_ok else
             f"malformed setText returned {result!r}"),
            {"result": result})

        got = send(port, "return engine.getText(2)")
        # Id 2 names no scene node either, so the same #1961 no-op
        # applies and nothing may be cached. The malformed input's
        # survival past `setTextFn`'s decode is what the check above
        # measures -- a strict `TE.decodeUtf8` there would have returned
        # a caught Lua error instead of "no_error", regardless of what
        # the handler downstream then decided to store.
        malformed_uncached_ok = got == "null"
        ok &= rep.check(
            "malformed_text_uncached", malformed_uncached_ok,
            ("no cache entry was created for the malformed call's "
             "unspawned id" if malformed_uncached_ok else
             f"unspawned id 2 read back as {got!r}, expected 'null'"),
            {"observed": got})

        # world.show is the representative non-Text-API boundary (#665):
        # a valid-but-nonexistent page id is the control case (requirement
        # 4) -- it must reach handleWorldShowCommand's normal semantic
        # no-op rather than error on decode.
        result = send(port, 'world.show("no_such_page"); return "no_error"')
        show_ok = result == "no_error"
        ok &= rep.check(
            "well_formed_world_show", show_ok,
            ("well-formed world.show raised no error" if show_ok else
             f"well-formed world.show returned {result!r}"),
            {"result": result})

        # The malformed repro, same truncated-byte shape as above, against
        # a non-Text API. Pre-fix this raises "Haskell exception in show:
        # ...bad UTF-8..." (Engine.Scripting.Lua.API.Internal's guard);
        # post-fix it must decode leniently and proceed to
        # handleWorldShowCommand's ordinary "nonexistent world" no-op.
        result = send(port, 'world.show("caf\\195"); return "no_error"')
        malformed_show_ok = result == "no_error"
        ok &= rep.check(
            "malformed_world_show", malformed_show_ok,
            ("malformed world.show raised no error" if malformed_show_ok else
             f"malformed world.show returned {result!r}"),
            {"result": result})

        # The engine must still be alive and answering afterward, matching
        # #622's guarantee (defense in depth -- this probe is the one that
        # actually exercises the decode-site fix, not just the crash guard).
        alive = proc.poll() is None
        ok &= rep.check(
            "engine_alive", alive,
            ("engine process remained alive" if alive else
             f"engine process exited; see {rep.engine_log_path(LOG_NAME, LOG)}"),
            {"alive": alive})
        if not alive:
            rep.skip("debug-console responsiveness could not be checked because "
                     "the engine exited")
        else:
            try:
                echo = send(port, "return 1+1").strip()
            except OSError as e:
                echo = None
                responsive = False
                detail = {"error": str(e)}
            else:
                responsive = echo == "2"
                detail = {"echo": echo}
            ok &= rep.check(
                "console_responsive", responsive,
                ("follow-up debug-console echo returned 2" if responsive else
                 f"follow-up debug-console echo failed: {detail}"),
                detail)

        rep.note("PASS" if ok else "FAIL")
        return 0 if ok else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
