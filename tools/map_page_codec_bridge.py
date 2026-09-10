"""Manual-only compiler/process boundary for the map-page experiment (#2303)."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parent.parent
EXE = ROOT / "dist-newstyle/map-codec"


def build():
    import probe_resource_lock as lock
    start = time.monotonic()
    while True:
        try:
            hold = lock.acquire(exclusive={"cabal-build"},
                                namespace=lock.repository_namespace(ROOT),
                                purpose="manual map-page codec measurement #2303")
            break
        except lock.ResourceBusy as busy:
            print(busy, flush=True)
            if time.monotonic() - start >= 1800:
                raise RuntimeError("30-minute build-lock timeout") from busy
            time.sleep(60)
    with hold:
        subprocess.run(["cabal", "build", "lib:synarchy"], cwd=ROOT, check=True)
        args = ["cabal", "exec", "--", "ghc", "-O2", "-threaded", "-rtsopts",
                "-fno-full-laziness", "-fno-cse", "-Wall", "-Werror",
                "-Wno-name-shadowing", "-Wno-type-defaults", "-XGHC2024",
                "-XNoImplicitPrelude", "-XUnicodeSyntax", "-XOverloadedStrings",
                "-XDuplicateRecordFields", "-XRecordWildCards",
                "-XNoMonomorphismRestriction", "-package", "synarchy",
                "-itest-headless", "-itools/map_codec",
                "-outputdir", "dist-newstyle/map-codec-build",
                "tools/map_codec/Main.hs", "-o", str(EXE)]
        subprocess.run(args, cwd=ROOT, check=True)
    return EXE


def run(exe, args, *, timeout=1800, parse=True, capabilities=1):
    """wait4 yields THIS child's peak RSS, not RUSAGE_CHILDREN's old maximum.

    macOS returns bytes; Linux returns KiB. RSS is whole-process memory,
    including native codec allocations. Timing is separately inside Haskell.
    """
    with tempfile.TemporaryFile() as stdout, tempfile.TemporaryFile() as stderr:
        proc = subprocess.Popen([str(exe), *map(str, args), "+RTS", f"-N{capabilities}", "-RTS"],
                                cwd=ROOT, stdout=stdout, stderr=stderr)
        start = time.monotonic()
        try:
            while True:
                pid, status, usage = os.wait4(proc.pid, os.WNOHANG)
                if pid:
                    proc.returncode = os.waitstatus_to_exitcode(status)
                    break
                if time.monotonic() - start > timeout:
                    proc.kill()
                    proc.wait()
                    raise TimeoutError(f"map-codec timeout: {args}")
                time.sleep(0.02)
        except BaseException:
            if proc.returncode is None:
                proc.kill()
                proc.wait()
            raise
        stdout.seek(0)
        stderr.seek(0)
        out, err = stdout.read().decode(), stderr.read().decode()
        if proc.returncode:
            raise RuntimeError(f"map-codec failed ({proc.returncode}): {args}\n{out}\n{err}")
        if not parse:
            return {"stderr": err}
        result = json.loads(out)
        if isinstance(result, dict):
            result["peak_process_rss_bytes"] = int(usage.ru_maxrss) * (1 if sys.platform == "darwin" else 1024)
        return result
