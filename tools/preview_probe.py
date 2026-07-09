#!/usr/bin/env python3
"""--preview boot skeleton probe (#632, Phase 1 of the --preview epic #427).

Phase 1 has no pixel-verifiable texture browsing yet — this probe checks
what IS headlessly verifiable:

  1. A grouped category with no item (e.g. "units") prints the "select a
     specific ..." guidance and exits 0 WITHOUT ever printing READY or
     booting any engine thread.
  2. An unrecognized category prints an error and exits 1, also without
     ever booting.
  3. A bare --preview (no category at all) prints an error and exits 1,
     also without ever booting — it must NOT silently fall through to
     the normal headless/graphical boot path.
  4. A real preview boot (simple category, or grouped+item) reaches the
     debug console (same READY handshake as --headless), reports its
     boot profile as "preview", and echoes back the exact (category,
     item) target it was launched with via engine.getPreviewTarget().

Usage:
  python3 tools/preview_probe.py [--port 9150]

Exit 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import subprocess
import sys
from probelib import boot, quit_engine, send, send_json

LOG = "/tmp/preview_probe_engine.log"


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f"  ({detail})" if detail else ""))
    return ok


def run_cli(*extra_args: str, timeout: float = 30.0) -> subprocess.CompletedProcess:
    cmd = ["cabal", "run", "-v0", "exe:synarchy", "--", *extra_args]
    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)


def check_grouped_no_item() -> bool:
    print("1. grouped category, no item: exit 0, no READY, prints guidance")
    r = run_cli("--preview", "units")
    ok = (r.returncode == 0
          and "READY" not in r.stdout
          and "select a specific units" in r.stdout
          and "units/acolyte" in r.stdout)
    return check("grouped-no-item", ok,
                 f"rc={r.returncode} stdout={r.stdout.strip()!r}")


def check_unrecognized_category() -> bool:
    print("2. unrecognized category: exit 1, no READY")
    r = run_cli("--preview", "not_a_real_category")
    ok = (r.returncode == 1
          and "READY" not in r.stdout
          and "not_a_real_category" in (r.stdout + r.stderr))
    return check("unrecognized-category", ok,
                 f"rc={r.returncode} stderr={r.stderr.strip()!r}")


def check_missing_target() -> bool:
    print("3. bare --preview (no target at all): exit 1, no READY, no silent fallthrough")
    # A regression here falls through to a real graphical boot, which
    # blocks indefinitely (a window waiting on user input) rather than
    # exiting — bound the wait so a regression FAILs fast instead of
    # hanging the probe, and never sits out the full default timeout.
    try:
        r = run_cli("--preview", timeout=15.0)
    except subprocess.TimeoutExpired:
        return check("missing-target", False,
                     "process did not exit within 15s — likely fell through "
                     "to a real graphical boot instead of erroring")
    ok = (r.returncode == 1
          and "READY" not in r.stdout
          and "--preview requires a target" in r.stderr)
    return check("missing-target", ok,
                 f"rc={r.returncode} stderr={r.stderr.strip()!r}")


def check_real_preview_boot(port: int) -> bool:
    print("4. real preview boot: boot profile + preview target over the debug console")
    proc = boot(port, log=LOG, mode=("--preview", "units/acolyte"),
                label="preview engine")
    try:
        profile = send(port, "return engine.getBootProfile()")
        ok1 = check("boot profile == preview", profile == "preview", profile)

        target = send_json(port, "return engine.getPreviewTarget()")
        ok2 = check(
            "preview target == units/acolyte",
            isinstance(target, dict)
            and target.get("category") == "units"
            and target.get("item") == "acolyte",
            target)
        return ok1 and ok2
    finally:
        quit_engine(port, proc)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9150)
    args = ap.parse_args()

    results = [
        check_grouped_no_item(),
        check_unrecognized_category(),
        check_missing_target(),
        check_real_preview_boot(args.port),
    ]

    passed = all(results)
    print(f"\n  {'PASS' if passed else 'FAIL'}: --preview boot skeleton"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
