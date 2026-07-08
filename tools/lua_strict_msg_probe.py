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

Usage: python3 tools/lua_strict_msg_probe.py [--port 9622]
Exit 0 = pass (engine survived + kept responding).
"""
from __future__ import annotations
import argparse
import sys
import time
from probelib import boot, quit_engine, send

LOG = "/tmp/lua_strict_msg_probe_engine.log"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9622)
    args = ap.parse_args()
    port = args.port

    proc = boot(port, log=LOG)
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
        print(f"engine process alive after setText: {alive}")
        if not alive:
            print("FAIL: engine process exited (see", LOG, ")")
            return 1

        # Confirm the debug console (and thus the engine loop) is still
        # genuinely responsive, not just an un-reaped zombie process. A
        # crash landing between the poll() above and this call would drop
        # the connection — that's a FAIL too, not a probe error.
        try:
            echo = send(port, "return 1+1").strip()
        except OSError as e:
            print(f"FAIL: debug console unreachable after setText ({e})")
            return 1
        responsive = echo == "2"
        print(f"follow-up debug-console echo: {echo!r} (responsive={responsive})")

        ok = alive and responsive
        print("PASS" if ok else "FAIL")
        return 0 if ok else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
