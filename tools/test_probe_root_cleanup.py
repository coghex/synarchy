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

Issue #1912 adds two more, and `tools/item_instance_probe.py` (#67) to
the probes driven:

  * **A read-only source `config/` still yields a removable tree.**
    `shutil.copytree` reproduces the SOURCE's mode bits, so a checkout,
    CI cache, read-only mount or unpacked archive whose `config/` lacks
    owner write hands the run a private copy whose entries cannot be
    unlinked — unlinking a child needs write on its parent directory.
    Every probe that copies `config/` must relax its OWN copy, leave
    the source's modes and contents exactly as it found them, keep
    `*.local.yaml` out, and still come through teardown having touched
    neither the content families nor anything outside the run.
  * **Cleanup decides `item_instance_probe`'s exit status.** Its
    removal used to be a `shutil.rmtree(..., ignore_errors=True)` in a
    `finally` beneath an already-computed `return`, so a refused
    deletion reported every check as passing. A survivor must now force
    a non-zero run and be named, whatever the scenario found, while a
    clean removal leaves the scenario's own 0/1/2 intact.

No engine, no world, no GPU, no network: short subprocesses over
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

import selftestlib
from selftestlib import FAILURES, expect

TOOLS = os.path.dirname(os.path.abspath(__file__))

PROBES = (
    "foraging_probe",
    "flora_growth_probe",
    "farm_ai_probe",
    "item_temp_probe",
)

# Every probe that COPIES `config/` into its invocation-owned root, and
# therefore inherits the source's mode bits (#1912). `item_instance_probe`
# joins the four above here only: it announces neither a staged root nor a
# save slot, so #1791's scenarios above do not describe it.
CONFIG_COPY_PROBES = PROBES + ("item_instance_probe",)

# Carried through the injected failure into the probe's own output, so a
# test can tell the cause it planted from any other diagnostic the run
# might print.
TOKEN = "synthetic-staging-fault-1791"


# The driver runs INSIDE the probe's own process, one statement away
# from the shipped `if __name__ == "__main__": sys.exit(main())`, so the
# exit status and the traceback are produced by the probe exactly as an
# operator would see them. Everything it replaces is replaced on the
# module the probe itself reads: `REPO` (the stand-in checkout the
# symlinks point into), `boot`/`quit_engine` (which must be observable
# without an engine), `tempfile.mkdtemp` (to capture the base the run
# chose) and, per scenario, `shutil.copytree`/`shutil.rmtree`.
DRIVER = r'''
import json, os, shutil, stat, sys, tempfile

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


def describe(base):
    """Every path under `base`, by its own lstat: a symlink is never
    followed, so the content families are described as the links they
    are rather than as whatever they name."""
    found = {}
    for path, dirs, files in os.walk(base):
        for name in [None, *dirs, *files]:
            target = path if name is None else os.path.join(path, name)
            mode = os.lstat(target).st_mode
            found[os.path.relpath(target, base)] = {
                "link": stat.S_ISLNK(mode),
                "dir": bool(stat.S_ISDIR(mode)) and not stat.S_ISLNK(mode),
                "w": bool(mode & stat.S_IWUSR),
                "x": bool(mode & stat.S_IXUSR),
            }
    return found


def stub_boot(*a, **k):
    # Staging has finished and nothing has been released yet, so this is
    # the one moment the invocation-owned tree can be described as the
    # run itself would have used it (#1912).
    base = next((e["path"] for e in events if e["kind"] == "base"), None)
    note(kind="boot", tree=describe(base) if base else None)
    if config["fault"] == "boot":
        # How `probelib.boot` ends a run whose engine died before READY.
        raise SystemExit("engine exited before READY: " + config["token"])
    raise AssertionError("boot ran after staging had already failed")


def stub_quit(*a, **k):
    note(kind="quit")
    raise AssertionError("quit_engine ran with no engine of this run's own")


probe.boot = stub_boot
probe.quit_engine = stub_quit

scenario = config.get("scenario")

if scenario is not None:
    # The cleanup contract, with the scenario's own answer as an input:
    # `main` still stages, still shuts down, still releases and still
    # composes the exit status -- only the checks themselves are stood
    # in for, which is what lets a PASSING run meet a refused removal.
    def scenario_run_probe(args, tmpdir, slot, adopt):
        root = probe.make_isolated_root(tmpdir)
        base = next((e["path"] for e in events if e["kind"] == "base"), None)
        note(kind="scenario", root=root,
             tree=describe(base) if base else None)
        return scenario

    probe.run_probe = scenario_run_probe

if scenario is None and config["fault"] != "boot":
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


# The source `config/` a read-only fixture stages, and the modes it
# stages them with: a nested directory and a file inside it as well as a
# top-level one, because `_make_owner_writable` has to walk (#1912). The
# `.local.yaml` is the developer override every probe's `copytree`
# already excludes; it is here so teardown can prove the exclusion still
# holds once the modes are in play.
READ_ONLY_CONFIG = (
    ("video_default.yaml", 0o444),
    ("save.local.yaml", 0o444),
    (os.path.join("nested", "extra_default.yaml"), 0o444),
)
READ_ONLY_DIRS = ("nested",)


class Fixture:
    """One scenario's disposable world: a stand-in checkout, a private
    TMPDIR the run's own base is the only thing that may appear in, and
    an unrelated outside directory that nothing may touch.

    `read_only` stages the checkout's `config/` the way a CI cache
    restored read-only, a read-only mount or an unpacked archive hands
    it over: `0555` directories holding `0444` files, which
    `shutil.copytree` reproduces onto the invocation's private copy
    (#1912)."""

    def __init__(self, read_only: bool = False) -> None:
        self.home = tempfile.mkdtemp(prefix="test_probe_root_cleanup_")
        self.repo = os.path.join(self.home, "checkout")
        self.tmp = os.path.join(self.home, "tmp")
        self.outside = os.path.join(self.home, "outside")
        self.read_only = read_only
        os.makedirs(self.tmp)
        os.makedirs(self.outside)
        for family in ("scripts", "assets", "data"):
            os.makedirs(os.path.join(self.repo, family))
            self._write(os.path.join(self.repo, family, "sentinel.txt"),
                        f"the real {family}/, reachable only through a symlink")
        config = os.path.join(self.repo, "config")
        os.makedirs(config)
        self._write(os.path.join(config, "video_default.yaml"),
                    "tracked: default\n")
        if read_only:
            for relative in READ_ONLY_DIRS:
                os.makedirs(os.path.join(config, relative))
            for relative, mode in READ_ONLY_CONFIG:
                self._write(os.path.join(config, relative),
                            f"tracked: {relative}\n")
                os.chmod(os.path.join(config, relative), mode)
            for relative in READ_ONLY_DIRS:
                os.chmod(os.path.join(config, relative), 0o555)
            os.chmod(config, 0o555)
        self._write(os.path.join(self.outside, "unrelated.txt"),
                    "a path this run never created")
        self.sentinels = {p: self._read(p) for p in self._sentinel_paths()}
        self.source_modes = self._config_modes()
        self.families = self._family_manifest()

    def _sentinel_paths(self) -> list[str]:
        paths = [os.path.join(self.repo, f, "sentinel.txt")
                 for f in ("scripts", "assets", "data")]
        paths.append(os.path.join(self.repo, "config", "video_default.yaml"))
        if self.read_only:
            paths.extend(os.path.join(self.repo, "config", relative)
                         for relative, _ in READ_ONLY_CONFIG)
        paths.append(os.path.join(self.outside, "unrelated.txt"))
        return paths

    def _config_modes(self) -> dict[str, int]:
        """Every mode bit in the source `config/` tree, by relative path.
        The run relaxes its own COPY; anything here that moves means it
        reached back into the checkout instead."""
        config = os.path.join(self.repo, "config")
        modes = {}
        for path, dirs, files in os.walk(config):
            for name in [None, *dirs, *files]:
                target = path if name is None else os.path.join(path, name)
                modes[os.path.relpath(target, config)] = \
                    os.lstat(target).st_mode
        return modes

    def _family_manifest(self) -> dict[str, list[str]]:
        """What each content family holds. The run symlinks these and
        `rmtree` unlinks the symlink, so a teardown that followed one
        instead would show up here as a family emptied out."""
        return {family: sorted(os.listdir(os.path.join(self.repo, family)))
                for family in ("scripts", "assets", "data")}

    def source_modes_unchanged(self) -> bool:
        return self._config_modes() == self.source_modes

    def families_unchanged(self) -> bool:
        return self._family_manifest() == self.families

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
        # A run that FAILED to relax its copy leaves an unremovable tree
        # behind on purpose; this fixture still has to be able to take
        # its own scratch directory away, so it relaxes everything first.
        for path, dirs, _files in os.walk(self.home):
            for target in [path, *(os.path.join(path, d) for d in dirs)]:
                try:
                    os.chmod(target, 0o755)
                except OSError:
                    pass
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

    def tree(self) -> dict | None:
        """The invocation-owned tree as it stood after staging and
        before any release, described by `lstat` in the run's own
        process."""
        for event in self.events:
            if event["kind"] in ("boot", "scenario") and event.get("tree"):
                return event["tree"]
        return None

    def entry(self, relative: str) -> dict | None:
        return (self.tree() or {}).get(relative)

    def stray_tmp_entries(self) -> list[str]:
        return sorted(os.listdir(self.fixture.tmp))


def drive(module: str, fault: str, fixture: Fixture,
          scenario: int | None = None) -> Run:
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
                   "record": record, "fault": fault, "token": TOKEN,
                   "scenario": scenario}, handle)
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


# ---------------------------------------------------------------------
# Issue #1912: the source's mode bits, and item-instance's exit status
# ---------------------------------------------------------------------
def test_a_read_only_source_config_still_yields_a_removable_tree(
        module: str) -> None:
    print(f"\ntest_a_read_only_source_config_still_yields_a_removable_tree"
          f"[{module}]")
    fixture = Fixture(read_only=True)
    try:
        # A boot abort is the cheapest way to reach cleanup with the
        # tree fully staged: staging is real, the copy is real, and the
        # release below is the probe's own.
        run = drive(module, "boot", fixture)
        base = run.base()
        expect(run.tree() is not None,
               "the staged tree was described before anything was released")
        for relative in ("root/config", "root/config/nested"):
            entry = run.entry(relative)
            expect(entry is not None and entry["w"] and entry["x"],
                   f"the private {relative} is writable and searchable by "
                   f"this run even though the source was not (got {entry})")
        entry = run.entry("root/config/video_default.yaml")
        expect(entry is not None and entry["w"],
               f"...and so is a copied config file, which the engine "
               f"rewrites when it saves settings (got {entry})")
        entry = run.entry("root/config/nested/extra_default.yaml")
        expect(entry is not None and entry["w"],
               f"...and one a whole directory deeper, so the relaxation "
               f"really walks (got {entry})")
        expect(not any(name.endswith(".local.yaml")
                       for name in (run.tree() or {})),
               f"the developer's *.local.yaml overrides stayed out of the "
               f"copy (tree held {sorted(run.tree() or {})})")
        for family in ("scripts", "assets", "data"):
            entry = run.entry(f"root/{family}")
            expect(entry is not None and entry["link"],
                   f"root/{family} is the symlink the run made, not a copy "
                   f"of the checkout (got {entry})")
        expect(base is not None and not os.path.exists(base),
               f"the run removes its own tree instead of reporting residue "
               f"({base})")
        expect(run.stray_tmp_entries() == [],
               f"nothing else survives in the run's temp directory "
               f"(found {run.stray_tmp_entries()})")
        expect(fixture.source_modes_unchanged(),
               "the checkout's own config/ modes are untouched -- only "
               "this run's copy is relaxed")
        expect(fixture.outside_state_unchanged(),
               "the checkout's own contents, and everything outside the "
               "run, come through byte-for-byte")
        expect(fixture.families_unchanged(),
               "teardown unlinked the content-family symlinks rather than "
               "following them into the checkout")
    finally:
        fixture.close()


def test_cleanup_decides_the_item_instance_exit_status() -> None:
    module = "item_instance_probe"
    print(f"\ntest_cleanup_decides_the_item_instance_exit_status[{module}]")
    # A clean removal leaves the scenario's own answer alone: the setup
    # failure, the failed summary and the passing summary all survive.
    for scenario in (0, 1, 2):
        fixture = Fixture()
        try:
            run = drive(module, "none", fixture, scenario=scenario)
            expect(run.code == scenario,
                   f"[clean/{scenario}] a removed tree leaves the scenario's "
                   f"own exit status intact (exit {run.code})")
            base = run.base()
            expect(base is not None and not os.path.exists(base),
                   f"[clean/{scenario}] the invocation base is gone ({base})")
        finally:
            fixture.close()
    # ...and a survivor overrides a PASSING run, in both shapes the
    # sibling probes already cover: a removal that raises, and one that
    # returns having deleted nothing.
    for fault, phrase in (("rmtree-noop", "survived removal"),
                          ("rmtree-raises", "could not remove")):
        fixture = Fixture()
        try:
            run = drive(module, fault, fixture, scenario=0)
            expect(run.code != 0,
                   f"[{fault}] a run whose own tree survives is not a pass, "
                   f"though every check passed (exit {run.code})")
            expect(phrase in run.output,
                   f"[{fault}] the residue is named on the operator's "
                   f"streams ('{phrase}')")
            base = run.base()
            expect(base is not None and os.path.exists(base),
                   f"[{fault}] the refused removal really did leave the "
                   f"tree, so the check above is not vacuous")
            expect(fixture.outside_state_unchanged()
                   and fixture.families_unchanged(),
                   f"[{fault}] a failed removal still touches nothing "
                   f"outside the run")
        finally:
            fixture.close()


def main() -> int:
    selftestlib.parse_verbose()
    for module in PROBES:
        test_a_staging_failure_removes_the_invocation_base(module)
        test_removal_that_cannot_finish_is_still_non_zero(module)
        test_a_boot_abort_still_leaves_nothing(module)
    for module in CONFIG_COPY_PROBES:
        test_a_read_only_source_config_still_yields_a_removable_tree(module)
    test_cleanup_decides_the_item_instance_exit_status()
    if FAILURES:
        print(f"\n{len(FAILURES)} check(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, f"\nAll isolated-root cleanup tests passed for "
        f"{len(CONFIG_COPY_PROBES)} probes")


if __name__ == "__main__":
    raise SystemExit(main())
