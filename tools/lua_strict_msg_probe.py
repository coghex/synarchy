#!/usr/bin/env python3
"""Headless probe for issue #622: a Haskell exception embedded, unevaluated,
in a LuaToEngineMsg/LuaMsg constructor field must not escape to the
consuming thread and crash the whole engine process.

`registerLuaFunction` (Engine.Scripting.Lua.API) wraps every registered Lua
function in a catch guard, so a Haskell exception raised *while still inside
that guarded call* becomes a caught, logged Lua error. But
Engine.Scripting.Lua.Types (LuaToEngineMsg/LuaMsg) used to be ordinary lazy
Haskell: a lazy field embedding a thunk that can throw (e.g.
`TE.decodeUtf8` on invalid UTF-8) isn't forced until some OTHER thread later
consumes the queued message — outside the guard, so it hits a fail-stop
top-level handler and kills the whole process instead of degrading to a
caught Lua error.

`engine.setText(objId, text)` (Engine.Scripting.Lua.API.Text.setTextFn)
decodes `text` with `TE.decodeUtf8` and embeds the (lazy, pre-fix) result in
a `LuaSetTextRequest` queued for the engine thread. `"caf\195"` is Lua's
decimal escape for the raw byte 0xC3, an invalid/truncated UTF-8 lead byte —
decoding it throws. Pre-fix this kills the whole engine; post-fix
(`{-# LANGUAGE Strict, StrictData #-}` on Types.hs forces the field at
construction, still inside the guard) it degrades to a caught Lua error and
the engine stays alive to answer a follow-up command.

No world/AI stack is needed — `engine.setText` is registered at Lua-API
boot time, before any script loads.

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps human-readable
per-check output.

Usage: python3 tools/lua_strict_msg_probe.py [--port 9622]
       python3 tools/lua_strict_msg_probe.py --describe
Exit 0 = pass (engine survived + kept responding).
"""
from __future__ import annotations
import argparse
import sys
import time

import probe_protocol
from probelib import boot, quit_engine, send

LOG = "/tmp/lua_strict_msg_probe_engine.log"
LOG_NAME = "lua_strict_msg_probe_engine.log"
PROBE_KEY = "lua_strict_msg"

CHECKS = [
    ("engine_alive", "the engine remains alive after the malformed message"),
    ("console_responsive",
     "the debug console remains responsive after the malformed message"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9622)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 check declaration and "
                         "exit without booting an engine")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args.port, rep)
    finally:
        rep.close()


def _run(port, rep) -> int:
    engine_log = rep.engine_log_path(LOG_NAME, LOG)

    proc = boot(port, log=engine_log, args=rep.engine_args())
    try:
        # Malformed-UTF-8 payload: Lua's \195 decimal escape is the raw
        # byte 0xC3, an invalid/truncated UTF-8 lead byte on its own.
        send(port, 'engine.setText(1, "caf\\195"); return "sent"',
             expect_result=False)

        # setText is fire-and-forget: the decode (and, pre-fix, the crash)
        # happens on the engine thread AFTER this call already returned, so
        # give it a moment to land before checking the process is still up.
        time.sleep(1.0)
        alive = proc.poll() is None
        ok = rep.check(
            "engine_alive", alive,
            ("engine process remained alive" if alive else
             f"engine process exited; see {engine_log}"),
            {"alive": alive})
        if not alive:
            rep.skip("debug-console responsiveness could not be checked because "
                     "the engine exited")
            rep.note("FAIL")
            return 1

        # Confirm the debug console (and thus the engine loop) is still
        # genuinely responsive, not just an un-reaped zombie process. A
        # crash landing between the poll() above and this call would drop
        # the connection — that's a FAIL too, not a probe error.
        try:
            echo = send(port, "return 1+1").strip()
        except OSError as e:
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
