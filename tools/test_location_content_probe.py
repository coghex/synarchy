#!/usr/bin/env python3
"""Artifact ownership for the location-content probe (issue #1884).

`tools/location_content_probe.py` is manual-only, boots seven engines
and generates several worlds, so its own acceptance can only be observed
by a run nothing in CI can make. The contract this file pins is the half
that is pure Python and would otherwise regress silently: every file one
invocation creates lives under ONE directory that invocation owns, and
the whole tree goes away again on every handled exit — unless
`--keep-artifacts` says otherwise.

Before #1884 the probe's five fixture YAMLs and its engine log were the
fixed, process-global names `/tmp/loc_content_probe_bogus.yaml`,
`/tmp/loc_content_probe_bogus_loot.yaml`,
`/tmp/loc_content_probe_quinoa.yaml`,
`/tmp/loc_content_probe_quinoa_loot.yaml`,
`/tmp/loc_content_probe_dense.yaml` and
`/tmp/location_content_engine.log`. Each was written with a truncating
`open(..., "w")` (`probelib.boot` opens the log the same way), none
carried a PID, port or any other invocation identity, and nothing
removed any of them. #1620 had already moved the SAVE slots into an
invocation-owned resource root and explicitly left these six behind.

Two concurrent runs — a supported mode: `run_probes.py --jobs N`, and
`probe_flake.py`'s machine-wide port lease — collided on all six. The
log collision is the sharp one: the probe ASSERTS against that log
twice (the integrity diagnostic in phase 2, the two unknown-content
warnings in phase 3), so a foreign truncation could turn a passing phase
into a failure or a failure into a pass.

The properties asserted directly, because each is a way the probe would
leak, collide, or stop proving what it claims:

  * Two invocations share no path — none of the five fixtures, not the
    log, not the root — so the fixed logical names inside each tree are
    safe. Every one of those paths is absolute and inside the run's own
    directory, because the engine is chdir'd into the isolated resource
    root (`App.ResourceRoot`) and resolves a relative path against it.
  * No artifact keeps a legacy fixed `/tmp` name, and a real run leaves
    all six of them exactly as it found them — absent if they were
    absent, byte-identical if a developer has one. Nor is any same-named
    file elsewhere opened, truncated or removed.
  * The tree is released after a pass, an early return, an exception, a
    `probelib.boot` abort, a `_PhaseAborted` and a handled Ctrl-C.
  * Every engine the run LAUNCHED is dead before any of that is removed,
    and the disposal is a direct kill rather than an `engine.quit()`
    aimed at whoever holds the port.
  * Every boot goes through the one funnel that hands it this
    invocation's log and registers the process as it is launched, and
    both log-reading ASSERTIONS read that same log.
  * Retention is opt-in, keeps the run's own result, names where the
    artifacts are, and describes what the run ACTUALLY produced rather
    than what a finished run usually would.
  * A default failing run says its log went with the tree and points at
    the flag, instead of leaving the operator chasing a deleted path.
  * Cleanup that cannot finish makes an otherwise passing run non-zero,
    through #1620's own `remove_isolated_root` reporting.
  * Fixture registration ORDER and CONTENTS are unchanged — bogus
    location, bogus loot, quinoa location, quinoa loot for phase 3, then
    `dense` alone for phase 4 — because placement and loot draws are
    order- and content-sensitive; and every one still goes through
    `load_fixture_yaml`, so a fixture that registers nothing still stops
    the probe at setup (#1342).
  * The public helpers other probes import — `make_isolated_root`,
    `remove_isolated_root`, `save_and_wait` — still exist with the
    shapes `tools/portal_ghost_probe.py` and
    `tools/test_location_probe_config_isolation.py` depend on.

No engine, no world, no worldgen, no GPU: every test here runs against
temporary directories in about a second.

Usage:
  python3 tools/test_location_content_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import ast
import contextlib
import hashlib
import inspect
import io
import os
import shutil
import sys
import tempfile
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import probelib  # type: ignore  # noqa: E402
import location_content_probe as probe  # type: ignore  # noqa: E402
import portal_ghost_probe as portal  # type: ignore  # noqa: E402

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402

#: The six process-global names the probe used before #1884. Nothing it
#: writes may resolve to one of them again, and a real run must leave
#: each exactly as it found it.
LEGACY_PATHS = (
    "/tmp/loc_content_probe_bogus.yaml",
    "/tmp/loc_content_probe_bogus_loot.yaml",
    "/tmp/loc_content_probe_quinoa.yaml",
    "/tmp/loc_content_probe_quinoa_loot.yaml",
    "/tmp/loc_content_probe_dense.yaml",
    "/tmp/location_content_engine.log",
)

#: The five fixtures, by the logical name `RunArtifacts.fixture` is
#: asked for, in the order the probe REGISTERS them: phase 3's four
#: (bogus location, bogus loot, quinoa location, quinoa loot) and then
#: phase 4's `dense` alone.
FIXTURE_NAMES = ("bogus", "bogus_loot", "quinoa", "quinoa_loot", "dense")

#: The loader each of those is registered through, in the same order.
FIXTURE_LOADERS = (
    "engine.loadLocationYaml",
    "engine.loadLootTableYaml",
    "engine.loadLocationYaml",
    "engine.loadLootTableYaml",
    "engine.loadLocationYaml",
)

#: The fixture bodies are load-bearing content, not scaffolding: which
#: content ids they name is what phase 3's unknown-id checks read back,
#: the single-entry loot tables are what make a specific item spawn
#: whatever the draw selects, and the fixed `position` is what phase 3
#: asserts to the exact tile. Pinning the bytes makes an edit a
#: deliberate, visible act rather than a silent change to what the probe
#: proves. These digests are of the bodies as they stood before #1884
#: moved WHERE they are written.
FIXTURE_DIGESTS = {
    "BOGUS_LOCATION_YAML":
        "407ebb91d16a57c874dee8d9395d620ccc6f640f4b067a2be7570c1a1357d8f9",
    "BOGUS_LOOT_YAML":
        "9589056a6a07c51f0096047050be628f1cec417c547f4a0138bfb983e7d908d9",
    "QUINOA_LOCATION_YAML":
        "1ca6a930511393416f545ac954cefe8eff3df3e044fa6ce0ffbaceb02c5b5dc7",
    "QUINOA_LOOT_YAML":
        "09bc563d2e3daf2c7fbfadca995ef164e0af9ff7d91d145120659d0f76a7bf5a",
    "DENSE_LOCATION_YAML":
        "3e0fc0dbd0b9abf46ba05f85c00b0446b39799393aec0179c520d811226104d0",
}


@contextlib.contextmanager
def fresh_run():
    """A built `RunArtifacts` on a temporary base, always cleaned up
    afterwards however the test leaves it — including when the test is
    about a removal that deliberately did not happen."""
    art = probe.RunArtifacts(tempfile.mkdtemp(prefix="test_loc_content_"))
    try:
        art.build()
        yield art
    finally:
        shutil.rmtree(art.base, ignore_errors=True)


def run_main(argv: list[str], body) -> tuple[int | None, BaseException | None,
                                             str, str | None]:
    """Drive `probe.main()` with `run` replaced by `body`, and report
    `(exit code, what propagated, merged output, the base the run
    owned)`.

    Substituting `run` is what lets these tests exercise the guard's
    real paths — an early return, an exception, `probelib.boot`'s
    `SystemExit` abort, a `_PhaseAborted`, a Ctrl-C — without booting an
    engine. Both streams are merged deliberately: what matters is what
    the operator sees, and the probe prints its checks on stdout while
    its failures and a propagating exception land on stderr.

    `code` is None when nothing was returned, which is itself the
    assertion for the cases that propagate: `raise SystemExit(main())`
    is never reached, so the interpreter exits non-zero on the
    exception.
    """
    seen: dict[str, object] = {}

    def wrapper(args, art, token):
        seen["base"] = art.base
        seen["art"] = art
        seen["token"] = token
        return body(art)

    original_run, original_argv = probe.run, sys.argv
    probe.run = wrapper
    sys.argv = ["location_content_probe.py", *argv]
    out = io.StringIO()
    code: int | None = None
    raised: BaseException | None = None
    try:
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(out):
            code = probe.main()
    except BaseException as exc:  # noqa: BLE001 - several cases are about this
        raised = exc
    finally:
        probe.run, sys.argv = original_run, original_argv
    return code, raised, out.getvalue(), seen.get("base")  # type: ignore[return-value]


def run_body(fn):
    """Source of `probe.run` as an AST function node, for the properties
    that are about the code the interpreter actually runs rather than
    about a value it produces."""
    return ast.parse(inspect.getsource(fn)).body[0]


# ---------------------------------------------------------------------
# Invocation-unique paths
# ---------------------------------------------------------------------
def test_two_invocations_share_no_path() -> None:
    print("\ntest_two_invocations_share_no_path")
    with fresh_run() as first, fresh_run() as second:
        expect(first.base != second.base,
               "two invocations own two directories")
        for name in FIXTURE_NAMES:
            expect(first.fixture(name) != second.fixture(name),
                   f"the {name} fixture resolves to disjoint paths, so one "
                   f"run cannot overwrite the other's between its write and "
                   f"the engine-side read")
        expect(first.engine_log != second.engine_log,
               "two concurrent runs cannot truncate one another's engine "
               "log — which two checks ASSERT against")
        expect(first.root != second.root,
               "and each keeps its own resource root, so its save slots too")


def test_every_fixture_path_is_absolute_and_owned() -> None:
    print("\ntest_every_fixture_path_is_absolute_and_owned")
    # The paths are handed to the ENGINE, which has chdir'd into the
    # isolated resource root, so a relative one would resolve somewhere
    # else entirely (#1884 requirement 8).
    with fresh_run() as art:
        for name in FIXTURE_NAMES:
            path = art.fixture(name)
            expect(os.path.isabs(path), f"the {name} fixture path is absolute")
            expect(path.startswith(art.fixtures + os.sep),
                   f"...and lands in the run's own fixtures directory")
        expect(os.path.isabs(art.engine_log)
               and art.engine_log.startswith(art.logs + os.sep),
               "the engine log does too")
        expect(art.root.startswith(art.base + os.sep)
               and art.logs.startswith(art.base + os.sep)
               and art.fixtures.startswith(art.base + os.sep),
               "and all three live under the one directory the invocation "
               "owns, so removing it removes them")

    # …and the five names really are the five the probe asks for: a
    # sixth fixture that skipped `RunArtifacts` would not be covered by
    # any of the above.
    fixtures = [node.args[0].value
                for node in ast.walk(run_body(probe.run))
                if isinstance(node, ast.Call)
                and isinstance(node.func, ast.Attribute)
                and node.func.attr == "fixture"
                and node.args and isinstance(node.args[0], ast.Constant)]
    expect(tuple(fixtures) == FIXTURE_NAMES,
           f"the probe asks for exactly these five fixtures, in this order "
           f"(got {fixtures})")


def test_no_artifact_keeps_a_legacy_fixed_tmp_name() -> None:
    print("\ntest_no_artifact_keeps_a_legacy_fixed_tmp_name")
    with fresh_run() as art:
        chosen = {art.fixture(name) for name in FIXTURE_NAMES}
        chosen.add(art.engine_log)
        for legacy in LEGACY_PATHS:
            expect(legacy not in chosen,
                   f"nothing this run writes resolves to {legacy}")
        expect(os.path.realpath(art.base) != os.path.realpath("/tmp"),
               "the run's own directory is not /tmp itself")
    source = Path(probe.__file__).read_text(encoding="utf-8")
    tree = ast.parse(source)
    literals = [node.value for node in ast.walk(tree)
                if isinstance(node, ast.Constant)
                and isinstance(node.value, str)
                and node.value.startswith("/tmp/")]
    expect(not literals,
           f"no /tmp path literal is left in the module at all (got "
           f"{literals})")


def test_a_real_run_leaves_every_legacy_path_as_it_found_it() -> None:
    print("\ntest_a_real_run_leaves_every_legacy_path_as_it_found_it")
    # Non-destructive by construction: whatever the six legacy paths
    # hold on this machine — nothing, or a developer's own files — is
    # recorded and compared, never created or removed here. Opening one
    # `"w"` is the quiet failure #1884 names, so the bytes are what is
    # compared, not merely existence.
    def snapshot() -> dict[str, bytes | None]:
        state: dict[str, bytes | None] = {}
        for path in LEGACY_PATHS:
            try:
                with open(path, "rb") as handle:
                    state[path] = handle.read()
            except OSError:
                state[path] = None
        return state

    before = snapshot()
    code, raised, _text, base = run_main([], lambda art: 0)
    after = snapshot()
    expect(code == 0 and raised is None,
           f"the run itself finished cleanly (code {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "and released its own tree")
    for path in LEGACY_PATHS:
        expect(before[path] == after[path],
               f"{path} is exactly as the run found it "
               f"({'absent' if before[path] is None else 'byte-identical'})")


def test_release_never_touches_what_the_run_did_not_create() -> None:
    print("\ntest_release_never_touches_what_the_run_did_not_create")
    outside = tempfile.mkdtemp(prefix="test_loc_content_decoy_")
    try:
        decoys = {}
        for name in (*(f"{n}.yaml" for n in FIXTURE_NAMES), "engine.log"):
            path = os.path.join(outside, name)
            payload = f"a developer's {name}, not the probe's".encode()
            with open(path, "wb") as handle:
                handle.write(payload)
            decoys[path] = payload
        code, raised, _text, base = run_main([], lambda art: 0)
        expect(code == 0 and raised is None,
               f"the run itself finished cleanly (code {code}, {raised!r})")
        expect(base is not None and not os.path.exists(base),
               "and released its own tree")
        for path, payload in decoys.items():
            expect(os.path.isfile(path),
                   f"the same-named {os.path.basename(path)} outside the run "
                   f"is still there")
            with open(path, "rb") as handle:
                expect(handle.read() == payload, "...and byte-identical")
    finally:
        shutil.rmtree(outside, ignore_errors=True)


def test_release_does_not_follow_the_content_symlinks() -> None:
    print("\ntest_release_does_not_follow_the_content_symlinks")
    with fresh_run() as art:
        before = sorted(os.listdir(os.path.join(probe.REPO, "scripts")))
        expect(probe.release_artifacts(art, keep=False) is None,
               "a clean removal reports no leftover")
        expect(not os.path.exists(art.base), "the run's own tree is gone")
        expect(sorted(os.listdir(os.path.join(probe.REPO, "scripts"))) == before,
               "the real scripts/ is untouched — rmtree unlinked the symlink")


# ---------------------------------------------------------------------
# The guard around the whole run
# ---------------------------------------------------------------------
def test_a_passing_run_leaves_nothing() -> None:
    print("\ntest_a_passing_run_leaves_nothing")
    code, raised, text, base = run_main([], lambda art: 0)
    expect(code == 0 and raised is None,
           f"a run whose checks all passed exits 0 (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a passing run leaves no artifact directory")
    expect(base is not None and base not in text,
           "and its summary does not point at a directory that was deleted")
    expect("retained this run's artifacts" not in text,
           "...and does not claim to have retained anything")
    expect("--keep-artifacts" not in text,
           "a passing run does not offer the diagnostic flag it did not need")


def test_a_failing_run_still_releases_and_points_at_the_flag() -> None:
    print("\ntest_a_failing_run_still_releases_and_points_at_the_flag")
    code, raised, text, base = run_main([], lambda art: 1)
    expect(code == 1 and raised is None,
           f"a run with failures exits non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a failing run leaves no artifact directory either")
    expect("re-run with --keep-artifacts" in text,
           "and tells the operator how to keep the log it just deleted")


def test_an_early_return_still_releases() -> None:
    print("\ntest_an_early_return_still_releases")

    def early_return(art):
        # What the probe does when a phase gives up: some fixtures
        # written, most phases never run.
        with open(art.fixture("bogus"), "w") as handle:
            handle.write(probe.BOGUS_LOCATION_YAML)
        return 1

    code, raised, _text, base = run_main([], early_return)
    expect(code == 1 and raised is None,
           f"an early return exits non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "an early return leaves no fixture behind")


def test_an_exception_mid_run_still_releases() -> None:
    print("\ntest_an_exception_mid_run_still_releases")

    def blow_up(art):
        raise RuntimeError("kaboom")

    code, raised, _text, base = run_main([], blow_up)
    expect(isinstance(raised, RuntimeError) and code is None,
           f"an unexpected exception propagates with its traceback, so "
           f"`raise SystemExit(main())` never runs and the interpreter "
           f"exits non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "an unexpected exception leaves no artifact directory")


def test_a_phase_abort_still_releases() -> None:
    print("\ntest_a_phase_abort_still_releases")

    def abort_phase(art):
        # `_PhaseAborted` is caught by the two phases that raise it, but
        # the guard must hold for one that escapes anyway.
        raise probe._PhaseAborted()

    code, raised, _text, base = run_main([], abort_phase)
    expect(isinstance(raised, probe._PhaseAborted) and code is None,
           f"a _PhaseAborted still ends the run (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a _PhaseAborted leaves no artifact directory")


def test_a_boot_abort_still_releases_and_points_at_the_flag() -> None:
    print("\ntest_a_boot_abort_still_releases_and_points_at_the_flag")

    def abort(art):
        # How `probelib.boot` ends a run whose engine died before READY.
        # Its message names the log path verbatim — the path the release
        # below is about to delete.
        raise SystemExit(f"engine exited before READY; see {art.engine_log}")

    code, raised, text, base = run_main([], abort)
    expect(code == 1 and raised is None,
           f"a boot abort exits non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a boot abort leaves no artifact directory")
    expect("aborted before finishing" in text,
           "the summary reports the abort rather than swallowing it")
    expect("re-run with --keep-artifacts" in text,
           "and tells the operator how to keep the log it just named")


def test_a_keyboard_interrupt_still_releases() -> None:
    print("\ntest_a_keyboard_interrupt_still_releases")

    def interrupt(art):
        raise KeyboardInterrupt()

    code, raised, _text, base = run_main([], interrupt)
    expect(isinstance(raised, KeyboardInterrupt) and code is None,
           f"a handled Ctrl-C still ends the run as an interrupt "
           f"(got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a handled Ctrl-C leaves no artifact directory")


def test_cleanup_failure_fails_an_otherwise_clean_run() -> None:
    print("\ntest_cleanup_failure_fails_an_otherwise_clean_run")
    # #1620 requirement 6's reporting, now covering the fixtures and the
    # log as well as the save slots: a green result sitting beside
    # leftover artifacts is precisely what this isolation exists to
    # prevent, so it must not be reported as a pass.
    for broken, phrase in (("noop", "still exists after removal"),
                           ("raises", "could not remove")):
        real_rmtree = shutil.rmtree
        if broken == "noop":
            shutil.rmtree = lambda *a, **k: None
        else:
            def refuse(*_a, **_k):
                raise OSError("permission denied")

            shutil.rmtree = refuse
        try:
            code, raised, text, base = run_main([], lambda art: 0)
        finally:
            shutil.rmtree = real_rmtree
        try:
            expect(code == 1 and raised is None,
                   f"[{broken}] a run that cannot remove its own tree does "
                   f"not report a pass (got {code}, {raised!r})")
            expect(phrase in text and base is not None and base in text,
                   f"[{broken}] the summary identifies the residue "
                   f"('{phrase}')")
            expect(base is not None and os.path.exists(base),
                   f"[{broken}] the refused removal really did leave the "
                   f"tree, so the check above is not vacuous")
        finally:
            if base:
                real_rmtree(base, ignore_errors=True)


# ---------------------------------------------------------------------
# Engines this run launched
# ---------------------------------------------------------------------
class _StandInEngine:
    """Stands in for the `subprocess.Popen` `probelib.boot` launches, so
    a teardown assertion can name the exact handle that was disposed of
    and say HOW."""

    pid = 4242

    def __init__(self, alive: bool = True) -> None:
        self.alive = alive
        self.killed = False

    def poll(self):
        return None if self.alive else 0

    def kill(self) -> None:
        self.killed = True
        self.alive = False

    def wait(self, timeout=None) -> int:
        return 0

    def __repr__(self) -> str:  # pragma: no cover - diagnostics only
        return f"<stand-in engine alive={self.alive} killed={self.killed}>"


def test_every_boot_is_handed_this_runs_log_and_root() -> None:
    print("\ntest_every_boot_is_handed_this_runs_log_and_root")
    seen: dict = {}
    launched = _StandInEngine()

    def stub_boot(port, log=None, args=None, on_launch=None, **kwargs):
        seen.update(port=port, log=log, args=args)
        if on_launch is not None:
            on_launch(launched)
        return launched

    with fresh_run() as art:
        original = probe.boot
        probe.boot = stub_boot
        try:
            probe.boot_isolated(9190, art)
        finally:
            probe.boot = original
        expect(seen.get("log") == art.engine_log,
               f"the boot writes into this invocation's own log "
               f"(got {seen.get('log')!r})")
        expect(seen.get("args") == ["--resource-root", art.root],
               f"...and into this invocation's own resource root "
               f"(got {seen.get('args')!r})")
        expect(art.launched == [launched],
               "and the process is registered as it is LAUNCHED, so the "
               "span boot spends waiting for READY is covered too")

    # Every boot in `run` really does go through that one funnel: a bare
    # `boot(...)` would pick its own log and register nothing.
    body = run_body(probe.run)
    bare = [node for node in ast.walk(body)
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
            and node.func.id == "boot"]
    funnelled = [node for node in ast.walk(body)
                 if isinstance(node, ast.Call)
                 and isinstance(node.func, ast.Name)
                 and node.func.id == "boot_isolated"]
    expect(not bare, f"no phase boots outside boot_isolated (got {len(bare)})")
    expect(len(funnelled) == 7,
           f"all seven phase boots go through it (got {len(funnelled)})")


def test_launched_engines_are_dead_before_anything_is_removed() -> None:
    print("\ntest_launched_engines_are_dead_before_anything_is_removed")
    # `probelib.boot` hands the handle over the statement after its
    # `Popen` and only decides about READY up to three minutes later, and
    # `quit_engine` is itself interruptible at every step — so a run can
    # reach teardown with a live engine still writing into the tree that
    # is about to be deleted.
    order: list[str] = []
    stranded = _StandInEngine()
    real_rmtree = shutil.rmtree

    def watched_rmtree(*a, **k):
        order.append("rmtree")
        return real_rmtree(*a, **k)

    def strand(art):
        art.launched.append(stranded)
        raise KeyboardInterrupt()

    original_kill = _StandInEngine.kill

    def watched_kill(self):
        order.append("kill")
        original_kill(self)

    _StandInEngine.kill = watched_kill  # type: ignore[method-assign]
    shutil.rmtree = watched_rmtree
    try:
        code, raised, _text, base = run_main([], strand)
    finally:
        shutil.rmtree = real_rmtree
        _StandInEngine.kill = original_kill  # type: ignore[method-assign]
    expect(stranded.killed,
           f"an engine stranded inside boot is disposed of (got {stranded})")
    expect(order[:2] == ["kill", "rmtree"],
           f"...and it is dead BEFORE the tree it was writing into is "
           f"removed (got {order})")
    expect(isinstance(raised, KeyboardInterrupt) and code is None,
           f"and the interrupt still ends the run (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "with nothing left behind")

    # A handle that has already exited — the ordinary case, every phase
    # having quit its own engine — is not killed a second time.
    finished = _StandInEngine(alive=False)
    run_main([], lambda art: (art.launched.append(finished), 0)[1])
    expect(not finished.killed,
           "a handle that has already exited is left alone")


# ---------------------------------------------------------------------
# Opt-in retention
# ---------------------------------------------------------------------
def test_retention_keeps_a_passing_run_passing() -> None:
    print("\ntest_retention_keeps_a_passing_run_passing")
    code, raised, text, base = run_main(["--keep-artifacts"], lambda art: 0)
    try:
        expect(code == 0 and raised is None,
               f"the passing run still exits 0 (got {code}, {raised!r})")
        expect(base is not None and os.path.isdir(base),
               "--keep-artifacts retains a passing run's artifacts too")
        expect("retained this run's artifacts (--keep-artifacts)" in text
               and base is not None and f": {base}\n" in text,
               "...and the summary names the retained directory itself, not "
               "merely paths inside it")
    finally:
        if base:
            shutil.rmtree(base, ignore_errors=True)


def test_retention_keeps_a_failing_run_failing() -> None:
    print("\ntest_retention_keeps_a_failing_run_failing")
    code, raised, text, base = run_main(["--keep-artifacts"], lambda art: 1)
    try:
        expect(code == 1 and raised is None,
               f"the failing run still exits non-zero (got {code}, {raised!r})")
        expect(base is not None and os.path.isdir(base),
               "--keep-artifacts retains a failing run's artifacts — the "
               "engine log is its primary evidence")
        expect("retained this run's artifacts (--keep-artifacts)" in text
               and base is not None and f": {base}\n" in text,
               "...and the summary names the retained directory itself")
        expect("re-run with --keep-artifacts" not in text,
               "...and does not tell the operator to pass the flag it "
               "already has")
    finally:
        if base:
            shutil.rmtree(base, ignore_errors=True)


def test_retention_after_a_boot_abort_keeps_the_log() -> None:
    print("\ntest_retention_after_a_boot_abort_keeps_the_log")
    # The case the flag exists for: the abort message names the log, and
    # with the flag that path is still there to read.
    def abort(art):
        with open(art.engine_log, "w") as handle:
            handle.write("engine said something useful\n")
        raise SystemExit(f"engine exited before READY; see {art.engine_log}")

    code, raised, text, base = run_main(["--keep-artifacts"], abort)
    try:
        expect(code == 1 and raised is None,
               f"the abort still exits non-zero (got {code}, {raised!r})")
        expect(base is not None and os.path.isdir(base),
               "the tree is retained")
        expect(base is not None
               and os.path.isfile(os.path.join(base, "logs", "engine.log")),
               "and the log the abort message named is still readable")
        expect("engine.log" in text,
               f"...and the summary names it (got {text!r})")
    finally:
        if base:
            shutil.rmtree(base, ignore_errors=True)


def test_retention_reports_what_the_run_actually_produced() -> None:
    print("\ntest_retention_reports_what_the_run_actually_produced")
    with fresh_run() as art:
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            expect(probe.release_artifacts(art, keep=True) is None,
                   "retention is not a cleanup failure")
        text = out.getvalue()
        expect(os.path.isdir(art.base), "--keep-artifacts retains the tree")
        expect(text.count("(empty)") == 3,
               f"a pre-READY failure's retained tree is reported as empty "
               f"rather than as holding fixtures and save slots "
               f"(got {text.count('(empty)')} empty lines)")
        for label, path in (("the artifact directory", art.base),
                            ("the engine log directory", art.logs),
                            ("the fixtures", art.fixtures),
                            ("the saves", os.path.join(art.root, "saves")),
                            ("the isolated root", art.root)):
            expect(path in text, f"the summary names {label}")

    with fresh_run() as art:
        open(art.engine_log, "w").close()
        for name in FIXTURE_NAMES:
            open(art.fixture(name), "w").close()
        os.makedirs(os.path.join(art.root, "saves", "loc_content_probe_abc"))
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            probe.release_artifacts(art, keep=True)
        text = out.getvalue()
        for name in ("engine.log", *(f"{n}.yaml" for n in FIXTURE_NAMES),
                     "loc_content_probe_abc"):
            expect(name in text, f"a finished run's summary names {name}")
        expect("(empty)" not in text, "...and reports nothing as empty")


def test_retention_after_a_staging_failure_names_nothing_absent() -> None:
    print("\ntest_retention_after_a_staging_failure_names_nothing_absent")
    # `build()` stages incrementally, so a permission, source or
    # disk-space failure part-way through leaves a tree whose log,
    # fixture and saves directories were never created. Reporting those
    # as "(empty)" would name artifacts that do not exist at all.
    base = tempfile.mkdtemp(prefix="test_loc_content_partial_")
    art = probe.RunArtifacts(base)
    try:
        os.makedirs(art.root)  # staging died right after the root itself
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            expect(probe.release_artifacts(art, keep=True) is None,
                   "retention after a staging failure is not a cleanup "
                   "failure")
        text = out.getvalue()
        expect("(empty)" not in text,
               f"a directory that was never created is not reported as "
               f"empty (got {text!r})")
        expect(text.count("never created") == 3,
               f"each of the three absent artifact directories says so "
               f"(got {text.count('never created')})")
        expect(art.root in text and "resource root" in text,
               "the root that DOES exist is still named")
    finally:
        shutil.rmtree(base, ignore_errors=True)


# ---------------------------------------------------------------------
# What the probe still proves
# ---------------------------------------------------------------------
def test_the_fixture_bodies_are_byte_for_byte_unchanged() -> None:
    print("\ntest_the_fixture_bodies_are_byte_for_byte_unchanged")
    for name, digest in FIXTURE_DIGESTS.items():
        body = getattr(probe, name).encode("utf-8")
        expect(hashlib.sha256(body).hexdigest() == digest,
               f"{name} is byte-for-byte what it was — moving where a "
               f"fixture is written must not change what it says")


def test_registration_order_and_loaders_are_unchanged() -> None:
    print("\ntest_registration_order_and_loaders_are_unchanged")
    # Placement and loot draws are order-sensitive: phase 3 registers
    # bogus location, bogus loot, quinoa location, quinoa loot, and
    # phase 4 registers dense alone (#1884 requirement 6).
    body = run_body(probe.run)
    loads = [node for node in ast.walk(body)
             if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
             and node.func.id == "load_fixture_yaml"]
    loads.sort(key=lambda node: (node.lineno, node.col_offset))
    loaders = [node.args[1].value for node in loads
               if isinstance(node.args[1], ast.Constant)]
    targets = [node.args[2].id for node in loads
               if isinstance(node.args[2], ast.Name)]
    expect(targets == [f"{n}_yaml" for n in FIXTURE_NAMES],
           f"the five fixtures register in the unchanged order "
           f"(got {targets})")
    expect(loaders == list(FIXTURE_LOADERS),
           f"...each through its own loader (got {loaders})")


def test_every_fixture_still_goes_through_load_fixture_yaml() -> None:
    print("\ntest_every_fixture_still_goes_through_load_fixture_yaml")
    # #1342 / #1884 requirement 7: `load_fixture_yaml` raises when a
    # fixture registered nothing, so a file that is invalid for the
    # current schema stops the probe at SETUP instead of surfacing as
    # downstream behavioural failures.
    body = run_body(probe.run)
    loads = [node for node in ast.walk(body)
             if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
             and node.func.id == "load_fixture_yaml"]
    expect(len(loads) == len(FIXTURE_NAMES),
           f"every one of the five fixtures is loaded through the checking "
           f"helper, and nothing else is (got {len(loads)})")
    raw = [node for node in ast.walk(body)
           if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
           and node.func.id == "send"
           and any("loadLocationYaml" in c.value or "loadLootTableYaml" in c.value
                   for c in ast.walk(node)
                   if isinstance(c, ast.Constant) and isinstance(c.value, str))]
    expect(not raw,
           f"no fixture is registered through a bare send() that would skip "
           f"the registration check (got {len(raw)})")
    expect(probelib.FixtureNotRegistered is probe.FixtureNotRegistered,
           "and the probe still imports that helper's own failure type, so "
           "a rejected fixture ends the run rather than a traceback")


def test_both_log_assertions_read_this_invocations_log() -> None:
    print("\ntest_both_log_assertions_read_this_invocations_log")
    # #1884 requirement 9. Two checks ASSERT against the engine log —
    # the integrity diagnostic in phase 2 and the two unknown-content
    # warnings in phase 3 — so a read of anything but this invocation's
    # own log could report another run's evidence as this one's.
    body = run_body(probe.run)
    opens = [node for node in ast.walk(body)
             if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
             and node.func.id == "open"]
    reads = [node for node in opens
             if not any(isinstance(a, ast.Constant) and a.value == "w"
                        for a in node.args[1:])]
    writes = [node for node in opens if node not in reads]
    expect(len(reads) == 2,
           f"the probe reads the log in exactly the two places that assert "
           f"against it (got {len(reads)})")
    expect(all(isinstance(node.args[0], ast.Attribute)
               and node.args[0].attr == "engine_log"
               and isinstance(node.args[0].value, ast.Name)
               and node.args[0].value.id == "art"
               for node in reads),
           "and both read this invocation's own log")
    expect(len(writes) == len(FIXTURE_NAMES)
           and all(isinstance(node.args[0], ast.Name)
                   and node.args[0].id.endswith("_yaml") for node in writes),
           f"and every truncating write in the run is one of the five "
           f"fixtures (got {len(writes)})")


def test_the_public_helpers_other_probes_import_are_intact() -> None:
    print("\ntest_the_public_helpers_other_probes_import_are_intact")
    # `tools/portal_ghost_probe.py` imports the first two and
    # `tools/test_location_probe_config_isolation.py` pins that sharing;
    # `save_and_wait` is called from there with its own `log`.
    expect(portal.make_isolated_root is probe.make_isolated_root
           and portal.remove_isolated_root is probe.remove_isolated_root
           and portal.save_and_wait is probe.save_and_wait,
           "portal_ghost_probe still shares all three helpers")
    save = inspect.signature(probe.save_and_wait).parameters
    expect(list(save) == ["port", "page", "slot", "failures", "log"],
           f"save_and_wait keeps its parameter names and order "
           f"(got {list(save)})")
    expect(save["log"].default is None,
           "with `log` now caller-supplied rather than a module global, "
           "because the log belongs to the invocation")
    with fresh_run() as art:
        expect(probe.make_isolated_root(art.base) == art.root,
               "make_isolated_root still answers with <base>/root, which is "
               "what RunArtifacts.build hands the engine")


def main() -> int:
    selftest.parse_verbose()
    test_two_invocations_share_no_path()
    test_every_fixture_path_is_absolute_and_owned()
    test_no_artifact_keeps_a_legacy_fixed_tmp_name()
    test_a_real_run_leaves_every_legacy_path_as_it_found_it()
    test_release_never_touches_what_the_run_did_not_create()
    test_release_does_not_follow_the_content_symlinks()
    test_a_passing_run_leaves_nothing()
    test_a_failing_run_still_releases_and_points_at_the_flag()
    test_an_early_return_still_releases()
    test_an_exception_mid_run_still_releases()
    test_a_phase_abort_still_releases()
    test_a_boot_abort_still_releases_and_points_at_the_flag()
    test_a_keyboard_interrupt_still_releases()
    test_cleanup_failure_fails_an_otherwise_clean_run()
    test_every_boot_is_handed_this_runs_log_and_root()
    test_launched_engines_are_dead_before_anything_is_removed()
    test_retention_keeps_a_passing_run_passing()
    test_retention_keeps_a_failing_run_failing()
    test_retention_after_a_boot_abort_keeps_the_log()
    test_retention_reports_what_the_run_actually_produced()
    test_retention_after_a_staging_failure_names_nothing_absent()
    test_the_fixture_bodies_are_byte_for_byte_unchanged()
    test_registration_order_and_loaders_are_unchanged()
    test_every_fixture_still_goes_through_load_fixture_yaml()
    test_both_log_assertions_read_this_invocations_log()
    test_the_public_helpers_other_probes_import_are_intact()
    if FAILURES:
        print(f"\n{len(FAILURES)} check(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftest.concluded(1)
    return selftest.concluded(
        0, "\nAll location_content_probe artifact-ownership tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
