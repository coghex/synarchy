#!/usr/bin/env python3
"""Headless probe for issues #618 and #665: `TE.decodeUtf8` call sites
across `Engine.Scripting.Lua` must not throw on a byte string that isn't
valid UTF-8.

`"caf\\195"` is Lua's decimal escape for the raw byte 0xC3 — a truncated
UTF-8 lead byte with no continuation byte, exactly the shape produced by
the byte-vs-codepoint bug in the five `truncateToWidth` ellipsis helpers
#618 fixes (`scripts/popup.lua`, `scripts/event_log.lua`,
`scripts/unit_info_v2_inventory.lua`, `scripts/item_contents_panel.lua`,
`scripts/cargo_inventory_panel.lua`).

Note the Text-API case is a NARROWER regression than #622's
`lua_strict_msg_probe.py`, which already established that `engine.setText`
with malformed UTF-8 no longer crashes the whole engine process (Strict/
StrictData on `LuaToEngineMsg` forces the field inside
`registerLuaFunction`'s catch guard). That fix alone still leaves
`setTextFn`'s `TE.decodeUtf8` throwing a *caught* Lua error every single
call — which is what #618's fix (switch to `TE.decodeUtf8Lenient`, the
codebase's established convention per #437/PR #492) actually eliminates.
The Text-API case below asserts the stronger, issue-#618-specific
property: no error at all, and the malformed text is actually stored
(round-trips through `engine.getText`) rather than silently dropped when
the call errors out before `Q.writeQueue` runs.

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

Usage: python3 tools/text_encoding_probe.py [--port 9618]
Exit 0 = pass.
"""
from __future__ import annotations
import argparse
import sys
from probelib import boot, quit_engine, send

LOG = "/tmp/text_encoding_probe_engine.log"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9618)
    args = ap.parse_args()
    port = args.port

    proc = boot(port, log=LOG)
    ok = True
    try:
        # setText with a well-formed string is the control case: must be
        # completely unaffected (requirement 4).
        result = send(port, 'engine.setText(1, "hello"); return "no_error"')
        print(f"setText(well-formed) result: {result!r}")
        if result != "no_error":
            print("FAIL: well-formed setText raised an error")
            ok = False
        got = send(port, "return engine.getText(1)")
        print(f"getText(1) after well-formed setText: {got!r}")
        if got != "hello":
            print("FAIL: well-formed text did not round-trip")
            ok = False

        # The malformed repro: a truncated multi-byte UTF-8 lead byte.
        # Pre-fix (TE.decodeUtf8), this throws -- caught (post-#622) as a
        # Lua error instead of crashing the process, but still an error on
        # every call. Post-fix (TE.decodeUtf8Lenient), it must succeed.
        result = send(port, 'engine.setText(2, "caf\\195"); return "no_error"')
        print(f"setText(malformed) result: {result!r}")
        if result != "no_error":
            print(f"FAIL: malformed setText raised an error: {result!r}")
            ok = False

        got = send(port, "return engine.getText(2)")
        print(f"getText(2) after malformed setText: {got!r}")
        # decodeUtf8Lenient replaces the invalid byte with U+FFFD; the
        # exact replacement text isn't the contract, but it must have been
        # stored at all (proves the message wasn't dropped by a caught
        # exception aborting setTextFn before Q.writeQueue ran).
        if not got or got in ("null", ""):
            print("FAIL: malformed text was not stored (message dropped)")
            ok = False
        elif not got.startswith("caf"):
            print(f"FAIL: stored text lost its well-formed prefix: {got!r}")
            ok = False

        # world.show is the representative non-Text-API boundary (#665):
        # a valid-but-nonexistent page id is the control case (requirement
        # 4) -- it must reach handleWorldShowCommand's normal semantic
        # no-op rather than error on decode.
        result = send(port, 'world.show("no_such_page"); return "no_error"')
        print(f"world.show(well-formed) result: {result!r}")
        if result != "no_error":
            print("FAIL: well-formed world.show raised an error")
            ok = False

        # The malformed repro, same truncated-byte shape as above, against
        # a non-Text API. Pre-fix this raises "Haskell exception in show:
        # ...bad UTF-8..." (Engine.Scripting.Lua.API.Internal's guard);
        # post-fix it must decode leniently and proceed to
        # handleWorldShowCommand's ordinary "nonexistent world" no-op.
        result = send(port, 'world.show("caf\\195"); return "no_error"')
        print(f"world.show(malformed) result: {result!r}")
        if result != "no_error":
            print(f"FAIL: malformed world.show raised an error: {result!r}")
            ok = False

        # The engine must still be alive and answering afterward, matching
        # #622's guarantee (defense in depth -- this probe is the one that
        # actually exercises the decode-site fix, not just the crash guard).
        alive = proc.poll() is None
        print(f"engine process alive: {alive}")
        if not alive:
            print("FAIL: engine process exited (see", LOG, ")")
            ok = False
        else:
            try:
                echo = send(port, "return 1+1").strip()
            except OSError as e:
                print(f"FAIL: debug console unreachable ({e})")
                ok = False
                echo = None
            if echo is not None:
                responsive = echo == "2"
                print(f"follow-up debug-console echo: {echo!r} (responsive={responsive})")
                ok = ok and responsive

        print("PASS" if ok else "FAIL")
        return 0 if ok else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
