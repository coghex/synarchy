#!/usr/bin/env python3
"""Staging-failure cleanup for the four isolated-root probes (issue #1791).

`tools/foraging_probe.py` (#1618), `tools/flora_growth_probe.py` and
`tools/farm_ai_probe.py` (#1616) and `tools/item_temp_probe.py` (#1613)
each give one invocation its own throwaway resource root and promise to
remove it on every exit path. The promise used to have a hole: both
`tempfile.mkdtemp` and `make_isolated_root(base)` ran BEFORE the `try`
whose `finally` owns `remove_run_root(base)`, so an exception raised
while STAGING the tree — the root, three symlinks into the checkout, a
copied `config/`, `saves/`, created in that order — bypassed cleanup
entirely and left the invocation-owned directory on disk. The comment
beside those lines already reasoned correctly about why `boot` belongs
inside the guard; the two lines above it were never moved in.

This file pins the boundary that closes it, for all four probes, by
driving each probe's REAL `main()` — not `make_isolated_root` in
isolation, which would pass while the defect above it stood. Each case
runs the probe in a subprocess with a stand-in checkout, a private
TMPDIR and injected faults, so the exit status and the operator-visible
cause are the process's own rather than this file's interpretation of
them.

Four scenarios per probe:

  * **Staging fails.** `shutil.copytree` raises once the root and its
    three symlinks exist, so there is real invocation-owned state to
    leak. The run must terminate non-zero with the cause visible, the
    invocation's base must be gone, the private TMPDIR must be empty,
    and neither `boot` nor `quit_engine` may have run — a staging
    failure happens before any engine exists, and an `engine.quit()`
    sent anyway would be aimed at whoever else holds the port.
  * **Deletion stays inside the run.** The stand-in checkout's
    `scripts/`, `assets/` and `data/` are reachable through the
    symlinks the partial tree already holds, and an unrelated directory
    sits outside it. Both must come through every scenario byte-for-byte
    unchanged, which pins requirement 3 without assuming anything about
    how `shutil.rmtree` treats a symlink.
  * **Removal that fails is still visibly non-zero.** Cleanup cannot
    promise absence when the filesystem refuses: a silently no-op
    removal and a removal that raises must each still leave a non-zero
    run whose output names the residue or the cause.
  * **The path that already worked is unchanged.** A `boot` that aborts
    the way `probelib.boot` does still prints the staged root and slot,
    still leaves nothing behind, and still sends no `engine.quit()`.

No engine, no world, no GPU, no network: sixteen short subprocesses over
temporary directories, in a couple of seconds.

Usage:
  python3 tools/test_probe_root_cleanup.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import tempfile

TOOLS = os.path.dirname(os.path.abspath(__file__))

PROBES = (
    "foraging_probe",
    "flora_growth_probe",
    "farm_ai_probe",
    "item_temp_probe",
)

# Carried through the injected failure into the probe's own output, so a
# test can tell the cause it planted from any other diagnostic the run
# might print.
TOKEN = "synthetic-staging-fault-1791"

FAILURES: list[str] = []

# The driver runs INSIDE the probe's own process, one statement away
# from the shipped `if __name__ == "__main__": sys.exit(main())`, so the
# exit status and the traceback are produced by the probe exactly as an
# operator would see them. Everything it replaces is replaced on the
# module the probe itself reads: `REPO` (the stand-in checkout the
# symlinks point into), `boot`/`quit_engine` (which must be observable
# without an engine), `tempfile.mkdtemp` (to capture the base the run
# chose) and, per scenario, `shutil.copytree`/`shutil.rmtree`.
DRIVER = r'''
import json, os, shutil, sys, tempfile

config = json.loads(open(sys.argv[1], encoding="utf-8").read())
sys.path.insert(0, config["tools"])
events = []


def note(**fields):
    events.append(fields)
    with open(config["record"], "w", encoding="utf-8") as handle:
        json.dump(events, handle)


probe = __import__(config["module"])
probe.REPO = config["repo"]

real_mkdtemp = tempfile.mkdtemp


def capture_mkdtemp(*a, **k):
    base = real_mkdtemp(*a, **k)
    note(kind="base", path=base)
    return base


tempfile.mkdtemp = capture_mkdtemp


def stub_boot(*a, **k):
    note(kind="boot")
    if config["fault"] == "boot":
        # How `probelib.boot` ends a run whose engine died before READY.
        raise SystemExit("engine exited before READY: " + config["token"])
    raise AssertionError("boot ran after staging had already failed")


def stub_quit(*a, **k):
    note(kind="quit")
    raise AssertionError("quit_engine ran with no engine of this run's own")


probe.boot = stub_boot
probe.quit_engine = stub_quit

if config["fault"] != "boot":
    def failing_copytree(src, dst, *a, **k):
        # `dst` is `<base>/root/config`, so the partial tree recorded
        # here is what staging had already committed to disk when the
        # failure struck.
        note(kind="staged", entries=sorted(os.listdir(os.path.dirname(dst))))
        raise OSError(28, config["token"])

    shutil.copytree = failing_copytree

if config["fault"] == "rmtree-noop":
    shutil.rmtree = lambda *a, **k: None
elif config["fault"] == "rmtree-raises":
    def refusing_rmtree(*a, **k):
        raise OSError("permission denied")

    shutil.rmtree = refusing_rmtree

sys.argv = [config["module"] + ".py"]
sys.exit(probe.main())
'''


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


class Fixture:
    """One scenario's disposable world: a stand-in checkout, a private
    TMPDIR the run's own base is the only thing that may appear in, and
    an unrelated outside directory that nothing may touch."""

    def __init__(self) -> None:
        self.home = tempfile.mkdtemp(prefix="test_probe_root_cleanup_")
        self.repo = os.path.join(self.home, "checkout")
        self.tmp = os.path.join(self.home, "tmp")
        self.outside = os.path.join(self.home, "outside")
        os.makedirs(self.tmp)
        os.makedirs(self.outside)
        for family in ("scripts", "assets", "data"):
            os.makedirs(os.path.join(self.repo, family))
            self._write(os.path.join(self.repo, family, "sentinel.txt"),
                        f"the real {family}/, reachable only through a symlink")
        os.makedirs(os.path.join(self.repo, "config"))
        self._write(os.path.join(self.repo, "config", "video_default.yaml"),
                    "tracked: default\n")
        self._write(os.path.join(self.outside, "unrelated.txt"),
                    "a path this run never created")
        self.sentinels = {p: self._read(p) for p in self._sentinel_paths()}

    def _sentinel_paths(self) -> list[str]:
        paths = [os.path.join(self.repo, f, "sentinel.txt")
                 for f in ("scripts", "assets", "data")]
        paths.append(os.path.join(self.repo, "config", "video_default.yaml"))
        paths.append(os.path.join(self.outside, "unrelated.txt"))
        return paths

    @staticmethod
    def _write(path: str, text: str) -> None:
        with open(path, "w", encoding="utf-8") as handle:
            handle.write(text)

    @staticmethod
    def _read(path: str) -> bytes | None:
        try:
            with open(path, "rb") as handle:
                return handle.read()
        except OSError:
            return None

    def outside_state_unchanged(self) -> bool:
        return all(self._read(p) == v for p, v in self.sentinels.items())

    def close(self) -> None:
        shutil.rmtree(self.home, ignore_errors=True)


class Run:
    """What one driven `main()` left behind."""

    def __init__(self, code: int, output: str, events: list[dict],
                 fixture: Fixture) -> None:
        self.code = code
        self.output = output
        self.events = events
        self.fixture = fixture

    def kinds(self) -> list[str]:
        return [e["kind"] for e in self.events]

    def base(self) -> str | None:
        for event in self.events:
            if event["kind"] == "base":
                return event["path"]
        return None

    def staged(self) -> list[str] | None:
        for event in self.events:
            if event["kind"] == "staged":
                return event["entries"]
        return None

    def stray_tmp_entries(self) -> list[str]:
        return sorted(os.listdir(self.fixture.tmp))


def drive(module: str, fault: str, fixture: Fixture) -> Run:
    """Run `module.main()` in its own process under `fault`, and report
    what it did. Both streams are merged: what matters is what the
    operator sees, and the probes print diagnostics on stdout while an
    uncaught staging failure lands on stderr."""
    driver = os.path.join(fixture.home, "driver.py")
    record = os.path.join(fixture.home, "record.json")
    config = os.path.join(fixture.home, "config.json")
    with open(driver, "w", encoding="utf-8") as handle:
        handle.write(DRIVER)
    with open(config, "w", encoding="utf-8") as handle:
        json.dump({"tools": TOOLS, "module": module, "repo": fixture.repo,
                   "record": record, "fault": fault, "token": TOKEN}, handle)
    environment = dict(os.environ)
    environment["TMPDIR"] = fixture.tmp
    result = subprocess.run([sys.executable, driver, config],
                            capture_output=True, text=True,
                            env=environment, cwd=fixture.home)
    events: list[dict] = []
    if os.path.exists(record):
        with open(record, encoding="utf-8") as handle:
            events = json.load(handle)
    return Run(result.returncode, result.stdout + result.stderr, events,
               fixture)


# ---------------------------------------------------------------------
# The boundary the issue is about
# ---------------------------------------------------------------------
def test_a_staging_failure_removes_the_invocation_base(module: str) -> None:
    print(f"\ntest_a_staging_failure_removes_the_invocation_base[{module}]")
    fixture = Fixture()
    try:
        run = drive(module, "staging", fixture)
        base = run.base()
        expect(base is not None, "the run created an invocation base to own")
        expect(run.staged() is not None
               and set(run.staged() or []) >= {"scripts", "assets", "data"},
               f"staging really had committed partial state before it "
               f"failed (got {run.staged()})")
        expect(run.code != 0,
               f"a staging failure does not report success (exit {run.code})")
        expect(TOKEN in run.output,
               "the cause is visible to the operator on stdout or stderr")
        expect(base is not None and not os.path.exists(base),
               f"the invocation base is gone ({base})")
        expect(run.stray_tmp_entries() == [],
               f"nothing else survives in the run's temp directory "
               f"(found {run.stray_tmp_entries()})")
        expect("boot" not in run.kinds(),
               "no engine is booted once staging has failed")
        expect("quit" not in run.kinds(),
               "no engine.quit() is sent, so a busy port belonging to "
               "another instance is never touched")
        expect(fixture.outside_state_unchanged(),
               "removal followed neither the symlinks into the checkout nor "
               "anything else outside the run")
    finally:
        fixture.close()


def test_removal_that_cannot_finish_is_still_non_zero(module: str) -> None:
    print(f"\ntest_removal_that_cannot_finish_is_still_non_zero[{module}]")
    for fault, phrase in (("rmtree-noop", "survived removal"),
                          ("rmtree-raises", "could not remove")):
        fixture = Fixture()
        try:
            run = drive(module, fault, fixture)
            expect(run.code != 0,
                   f"[{fault}] a run that cannot remove its own tree does "
                   f"not report success (exit {run.code})")
            expect(phrase in run.output,
                   f"[{fault}] the residue is named on the operator's "
                   f"streams ('{phrase}')")
            base = run.base()
            expect(base is not None and os.path.exists(base),
                   f"[{fault}] the refused removal really did leave the "
                   f"tree, so the check above is not vacuous")
            expect(fixture.outside_state_unchanged(),
                   f"[{fault}] a failed removal still touches nothing "
                   f"outside the run")
        finally:
            fixture.close()


def test_a_boot_abort_still_leaves_nothing(module: str) -> None:
    print(f"\ntest_a_boot_abort_still_leaves_nothing[{module}]")
    fixture = Fixture()
    try:
        run = drive(module, "boot", fixture)
        expect("boot" in run.kinds(),
               "staging succeeded and the run reached its engine boot")
        expect("isolated resource root:" in run.output
               and "save slot:" in run.output,
               "the staged root and this run's save slot are still "
               "announced before the boot")
        expect(run.code != 0,
               f"a boot abort does not report success (exit {run.code})")
        expect(TOKEN in run.output, "the abort's own cause is visible")
        base = run.base()
        expect(base is not None and not os.path.exists(base),
               f"the invocation base is gone ({base})")
        expect(run.stray_tmp_entries() == [],
               f"nothing survives in the run's temp directory "
               f"(found {run.stray_tmp_entries()})")
        expect("quit" not in run.kinds(),
               "a dead engine is not sent engine.quit()")
        expect(fixture.outside_state_unchanged(),
               "the checkout behind the symlinks is untouched")
    finally:
        fixture.close()


def main() -> int:
    for module in PROBES:
        test_a_staging_failure_removes_the_invocation_base(module)
        test_removal_that_cannot_finish_is_still_non_zero(module)
        test_a_boot_abort_still_leaves_nothing(module)
    if FAILURES:
        print(f"\n{len(FAILURES)} check(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print(f"\nAll isolated-root cleanup tests passed for "
          f"{len(PROBES)} probes")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
