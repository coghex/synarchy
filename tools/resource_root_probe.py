#!/usr/bin/env python3
"""Resource-root launch-contract probe (#636).

The executable loads every runtime resource family (scripts/, assets/,
data/, config/) by cwd-relative paths; App.ResourceRoot resolves ONE
resource root at startup (--resource-root flag > SYNARCHY_ROOT env >
current directory) and chdirs into it. This probe proves the built
executable works from a working directory OUTSIDE the repo:

  1. locate (or build) exe:synarchy via cabal, then run everything
     below as the raw binary from a fresh temp directory;
  2. no resource root given from the temp dir -> actionable exit-1
     error naming the root in use and the missing resource paths;
  3. --resource-root pointing at a nonexistent directory -> same, and
     a bare --resource-root with no path errors instead of silently
     falling back to the cwd;
  4. a small --dump run with --resource-root pointing at the repo ->
     stdout is a nonempty JSON tile array (flag mechanism);
  5. a --headless boot with SYNARCHY_ROOT pointing at the repo ->
     READY, debug console answers, clean engine.quit shutdown (env
     mechanism).

Usage:
  python3 tools/resource_root_probe.py [--port 9636]

Exit 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import tempfile
import time
from pathlib import Path

from probelib import GUI_PORT, send, quit_engine

REPO = Path(__file__).resolve().parent.parent
LOG = "/tmp/resource_root_probe_engine.log"


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f"  ({detail})" if detail else ""))
    return ok


def locate_binary() -> str:
    """`cabal list-bin exe:synarchy` from the repo root, building first
    if the binary isn't there yet."""
    def list_bin() -> str | None:
        r = subprocess.run(["cabal", "list-bin", "exe:synarchy"],
                           cwd=REPO, capture_output=True, text=True)
        if r.returncode == 0:
            p = r.stdout.strip()
            if p and os.path.isfile(p):
                return p
        return None

    binary = list_bin()
    if binary is None:
        print("  building exe:synarchy (cabal build)...")
        subprocess.run(["cabal", "build", "exe:synarchy"], cwd=REPO,
                       check=True)
        binary = list_bin()
    if binary is None:
        sys.exit("could not locate exe:synarchy via cabal list-bin")
    return binary


def base_env() -> dict[str, str]:
    """Process env with SYNARCHY_ROOT stripped, so each check controls
    the mechanism under test explicitly."""
    env = dict(os.environ)
    env.pop("SYNARCHY_ROOT", None)
    return env


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9636)
    args = ap.parse_args()
    if args.port == GUI_PORT:
        sys.exit(f"refusing port {GUI_PORT} (the GUI port); pass a 9xxx port")

    binary = locate_binary()
    print(f"  exe: {binary}")
    failures = 0
    tmp = tempfile.mkdtemp(prefix="synarchy_resource_root_")
    print(f"  temp cwd (outside repo): {tmp}")

    # --- 2. no root given from a non-repo cwd: actionable error -----------
    r = subprocess.run([binary, "--dump", "--worldSize", "64"],
                       cwd=tmp, env=base_env(),
                       capture_output=True, text=True, timeout=60)
    ok = (r.returncode == 1
          and "invalid resource root" in r.stderr
          and "current directory" in r.stderr
          and os.path.join(tmp, "scripts") in r.stderr
          and "--resource-root" in r.stderr)
    failures += not check("no root from non-repo cwd -> exit 1 naming root + missing paths",
                          ok, f"rc={r.returncode}")

    # --- 3. nonexistent explicit root: actionable error -------------------
    bogus = os.path.join(tmp, "nonexistent")
    r = subprocess.run([binary, "--dump", "--resource-root", bogus],
                       cwd=tmp, env=base_env(),
                       capture_output=True, text=True, timeout=60)
    ok = (r.returncode == 1
          and "invalid resource root" in r.stderr
          and bogus in r.stderr
          and "--resource-root" in r.stderr)
    failures += not check("nonexistent --resource-root -> exit 1 naming it",
                          ok, f"rc={r.returncode}")

    # --- 3b. bare --resource-root (no path): error, not cwd fallback ------
    r = subprocess.run([binary, "--dump", "--resource-root"],
                       cwd=tmp, env=base_env(),
                       capture_output=True, text=True, timeout=60)
    ok = (r.returncode == 1
          and "--resource-root requires a path" in r.stderr)
    failures += not check("bare --resource-root -> exit 1, no cwd fallback",
                          ok, f"rc={r.returncode}")

    # --- 4. --dump from the temp dir with --resource-root <repo> ----------
    r = subprocess.run([binary, "--dump", "--seed", "42",
                        "--worldSize", "64", "--region", "0,0,0,0",
                        "--resource-root", str(REPO)],
                       cwd=tmp, env=base_env(),
                       capture_output=True, text=True, timeout=600)
    tiles = None
    if r.returncode == 0:
        try:
            tiles = json.loads(r.stdout)
        except ValueError:
            tiles = None
    ok = (isinstance(tiles, list) and len(tiles) > 0
          and all(k in tiles[0] for k in ("x", "y", "terrainZ")))
    failures += not check("--dump via --resource-root -> nonempty JSON tile array",
                          ok, f"rc={r.returncode}, tiles={len(tiles) if isinstance(tiles, list) else 'n/a'}")

    # --- 5. --headless from the temp dir with SYNARCHY_ROOT=<repo> --------
    env = base_env()
    env["SYNARCHY_ROOT"] = str(REPO)
    logf = open(LOG, "w")
    proc = subprocess.Popen([binary, "--headless", "--port", str(args.port)],
                            cwd=tmp, env=env,
                            stdout=logf, stderr=subprocess.STDOUT)
    try:
        ready = False
        deadline = time.time() + 180
        while time.time() < deadline:
            if "READY" in open(LOG).read():
                ready = True
                break
            if proc.poll() is not None:
                break
            time.sleep(0.4)
        failures += not check("headless via SYNARCHY_ROOT -> READY", ready,
                              f"see {LOG}")
        if ready:
            failures += not check("debug console answers",
                                  send(args.port, "return 1+1") == "2")
            quit_engine(args.port, proc)
            failures += not check("clean shutdown (engine.quit -> exit 0)",
                                  proc.returncode == 0,
                                  f"rc={proc.returncode}")
    finally:
        if proc.poll() is None:
            proc.kill()
            proc.wait(timeout=10)
        logf.close()

    passed = failures == 0
    print(f"\n  {'PASS' if passed else 'FAIL'}: resource-root launch contract"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
