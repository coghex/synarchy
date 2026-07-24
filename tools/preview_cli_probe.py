#!/usr/bin/env python3
"""--preview CLI-contract probe (#886, Phase 2 of the --preview epic #427).

Every check here is a PRE-BOOT rejection or exit — no GPU, no window, no
engine thread ever starts (App.Main/App.Cli/Engine.Preview.Discovery
reject before 'App.Preview.runPreview' is ever called). This is what makes
the probe CI-eligible: a classifier or path-containment regression fails
PRs directly instead of waiting for a manual, needs-gpu dev-machine run
(tools/preview_probe.py, which keeps the real-boot browser checks).

Checks:
  1. A bare --preview (no target at all) errors and exits 1, no silent
     fallthrough to the normal boot path (regression risk: hangs on a
     real graphical boot instead).
  2. Every explicitly unexposed category name (equipment, hud, facemap,
     utility, vegetation) is an ordinary unknown-category error listing
     exactly the canonical set — no compatibility aliases.
  3. Every grouped category (units, flora, buildings, structures,
     including structures — the #428-reorganization addition) with no
     item prints the "select a specific ..." guidance and exits 0.
  4. A nonexistent simple-category item path rejects before boot.
  5. Path-containment: an absolute item path, a leading ".." traversal,
     and a ".." component in the middle of the path all reject before
     boot (never touching a file outside the category root).
  6. A directory given as a simple-category item rejects before boot.

Usage:
  python3 tools/preview_cli_probe.py

Exit 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import subprocess
import sys

LOG = "/tmp/preview_cli_probe_engine.log"

UNEXPOSED_CATEGORIES = ["equipment", "hud", "facemap", "utility", "vegetation"]
GROUPED_CATEGORIES = ["units", "flora", "buildings", "structures"]
CANONICAL_LIST_TEXT = "icons, items, ui, world, units, flora, buildings, structures"


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f"  ({detail})" if detail else ""))
    return ok


def run_cli(*extra_args: str, timeout: float = 30.0) -> subprocess.CompletedProcess:
    cmd = ["cabal", "run", "-v0", "exe:synarchy", "--", *extra_args]
    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)


def check_missing_target() -> bool:
    print("1. bare --preview (no target at all): exit 1, no READY, no silent fallthrough")
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


def check_unexposed_category(cat: str) -> bool:
    r = run_cli("--preview", cat)
    stderr = r.stderr.strip()
    ok = (r.returncode == 1
          and "READY" not in r.stdout
          and cat in stderr
          and CANONICAL_LIST_TEXT in stderr)
    return check(f"unexposed category '{cat}'", ok, f"rc={r.returncode} stderr={stderr!r}")


def check_grouped_no_item(cat: str) -> bool:
    r = run_cli("--preview", cat)
    ok = (r.returncode == 0
          and "READY" not in r.stdout
          and f"select a specific {cat}" in r.stdout)
    return check(f"grouped category '{cat}', no item", ok,
                 f"rc={r.returncode} stdout={r.stdout.strip()!r}")


def check_nonexistent_simple_item() -> bool:
    print("4. nonexistent simple-category item: exit 1, no READY, pre-boot")
    r = run_cli("--preview", "icons/this/does/not/exist.png")
    ok = (r.returncode == 1
          and "READY" not in r.stdout
          and "no such texture" in r.stderr)
    return check("nonexistent-simple-item", ok,
                 f"rc={r.returncode} stderr={r.stderr.strip()!r}")


def check_path_containment() -> bool:
    print("5. path containment: absolute / .. traversal reject before boot")
    results = []
    for label, target in [
        ("absolute path", "icons//etc/passwd"),
        ("leading .. traversal", "icons/../../../etc/passwd"),
        ("mid-path .. traversal", "icons/skill/../../ui/box"),
    ]:
        r = run_cli("--preview", target)
        ok = (r.returncode == 1
              and "READY" not in r.stdout
              and "must stay within the category" in r.stderr)
        results.append(check(f"containment: {label}", ok,
                             f"rc={r.returncode} stderr={r.stderr.strip()!r}"))
    return all(results)


def check_directory_as_item() -> bool:
    print("6. a directory given as the item: exit 1, no READY, pre-boot")
    r = run_cli("--preview", "icons/skill")
    ok = (r.returncode == 1
          and "READY" not in r.stdout
          and "directory" in r.stderr)
    return check("directory-as-item", ok,
                 f"rc={r.returncode} stderr={r.stderr.strip()!r}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    # Every registered probe accepts --port (#723) so tools/run_probes.py
    # can force a non-default port on any of them uniformly, even one
    # like this that boots no engine at all and has no use for it.
    ap.add_argument("--port", type=int, default=None)
    ap.parse_args()

    results = [check_missing_target()]

    print("2. unexposed categories (no compatibility aliases): exit 1, canonical list")
    results += [check_unexposed_category(c) for c in UNEXPOSED_CATEGORIES]

    print("3. grouped categories, no item: exit 0, guidance printed")
    results += [check_grouped_no_item(c) for c in GROUPED_CATEGORIES]

    results.append(check_nonexistent_simple_item())
    results.append(check_path_containment())
    results.append(check_directory_as_item())

    passed = all(results)
    print(f"\n  {'PASS' if passed else 'FAIL'}: --preview CLI contract"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
