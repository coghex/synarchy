#!/usr/bin/env python3
"""Artifact ownership for the flora-growth probe (issue #1682).

`tools/flora_growth_probe.py` is manual-only and worldgen-heavy, so its
own acceptance can only be observed by a run that generates a world. The
contract this file pins is the half that is pure Python and would
otherwise regress silently: every file one invocation creates lives under
ONE directory that invocation owns, and the whole tree goes away again on
every handled exit — unless `--keep-artifacts` says otherwise.

Before #1682 the probe's two fixture YAMLs and its engine log were the
fixed, process-global names `/tmp/probe_berry.yaml`,
`/tmp/probe_clover.yaml` and `/tmp/flora_growth_probe_engine.log`. Each
was written with a truncating `open(..., "w")` (`probelib.boot` opens the
log the same way), none carried a PID, port or any other invocation
identity, and nothing removed any of them. Two concurrent runs — a
supported mode: `run_probes.py --jobs N`, and `probe_flake.py`'s
machine-wide port lease — collided on all three, one overwriting a
fixture between another's write and the engine-side read of it while both
interleaved into one truncated log. #1616 had already moved the SAVE slot
into an invocation-owned root and explicitly left these behind.

Ten properties are asserted directly rather than inferred, because each
is a way the probe would leak, collide, or stop proving what it claims:

  * Two invocations share no path — not the fixtures, not the log, not
    the root — so the fixed logical names inside each tree are safe.
  * No artifact keeps a legacy fixed `/tmp` name, so a pre-existing file
    at one of them is never opened for writing, truncated, modified or
    deleted. Nor is any same-named file the run did not create.
  * The tree is released after a pass, an early return, an exception, a
    `probelib.boot` abort, and a handled Ctrl-C.
  * An engine this run LAUNCHED is always disposed of, including when
    the interrupt lands inside `probelib.boot` — which hands the handle
    over the statement after its `Popen` and only decides about READY up
    to three minutes later. Only a boot that RETURNED may be shut down
    through the PORT, because a boot fails on a busy port precisely
    because somebody else's instance holds it.
  * Retention is opt-in, keeps the run's own success or failure result,
    names where the artifacts are, and describes what the run ACTUALLY
    produced rather than what a finished run usually would.
  * A default failing run says its log went with the tree and points at
    the flag, instead of leaving the operator chasing a deleted path.
  * Cleanup that cannot finish makes an otherwise passing run non-zero.
  * A read-only checkout still yields a REMOVABLE tree. `copytree`
    reproduces the source's mode bits, so a read-only `config/` would
    otherwise produce a private copy whose entries cannot be unlinked —
    residue, and a failing run, from a source the probe only read — and
    the source's own modes stay exactly as they were.
  * A staging failure's retained tree names no artifact that does not
    exist: a directory that was never created is reported as such, not
    as empty.
  * Registration ORDER is unchanged — the sorted real flora, then
    `probe_berry`, then `probe_clover` — because placement hashes are
    indexed by it; both fixture bodies are byte-for-byte what they were;
    and `load_fixture_yaml`'s positive-registration check still guards
    both, so a fixture that registers nothing still stops the probe at
    setup (#1342).

No engine, no world, no worldgen, no GPU: every test here runs against
temporary directories in about a second.

Usage:
  python3 tools/test_flora_growth_probe.py
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
import flora_growth_probe as probe  # type: ignore  # noqa: E402

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402

# The three process-global names the probe used before #1682. Nothing it
# writes may resolve to one of them again.
LEGACY_PATHS = (
    "/tmp/probe_berry.yaml",
    "/tmp/probe_clover.yaml",
    "/tmp/flora_growth_probe_engine.log",
)

# The fixture bodies are load-bearing content, not scaffolding: their
# worldGen tolerances are what make both species place on any seed, and
# their phases/annualCycle are what the season-window and regrowth checks
# read back. Pinning the bytes makes an edit to either one a deliberate,
# visible act rather than a silent change to what the probe proves.
FIXTURE_DIGESTS = {
    "PROBE_BERRY_YAML":
        "dc7dfcd43dcbc9502193dad385c786b45888eccf5cc74655c81f185aae0cc40c",
    "PROBE_CLOVER_YAML":
        "ded30e02c64e921f3fb198c70741f2b2b555802fcc206f32f4a86d9465e5a308",
}


@contextlib.contextmanager
def fresh_run():
    """A built `RunArtifacts` on a temporary base, always cleaned up
    afterwards however the test leaves it — including when the test is
    about a removal that deliberately did not happen."""
    art = probe.RunArtifacts(tempfile.mkdtemp(prefix="test_flora_growth_"))
    try:
        art.build()
        yield art
    finally:
        shutil.rmtree(art.base, ignore_errors=True)


def run_main(argv: list[str], body) -> tuple[int | None, BaseException | None,
                                             str, str | None]:
    """Drive `probe.main()` with `run_probe` replaced by `body`, and
    report `(exit code, what propagated, merged output, the base the run
    owned)`.

    Substituting `run_probe` is what lets these tests exercise the
    guard's real paths — an early return, an exception, `probelib.boot`'s
    `SystemExit` abort, a Ctrl-C — without booting an engine. Both
    streams are merged deliberately: what matters is what the operator
    sees, and the probe prints its checks on stdout while a propagating
    exception lands on stderr.

    `code` is None when nothing was returned, which is itself the
    assertion for the two cases that propagate: `sys.exit(main())` is
    never reached, so the interpreter exits non-zero on the exception.
    """
    seen: dict[str, str] = {}

    def wrapper(args, art):
        seen["base"] = art.base
        return body(art)

    original_run, original_argv = probe.run_probe, sys.argv
    probe.run_probe = wrapper
    sys.argv = ["flora_growth_probe.py", *argv]
    out = io.StringIO()
    code: int | None = None
    raised: BaseException | None = None
    try:
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(out):
            code = probe.main()
    except BaseException as exc:  # noqa: BLE001 - several cases are about this
        raised = exc
    finally:
        probe.run_probe, sys.argv = original_run, original_argv
    return code, raised, out.getvalue(), seen.get("base")


# ---------------------------------------------------------------------
# Invocation-unique paths
# ---------------------------------------------------------------------
def test_two_invocations_share_no_path() -> None:
    print("\ntest_two_invocations_share_no_path")
    with fresh_run() as first, fresh_run() as second:
        expect(first.base != second.base,
               "two invocations own two directories")
        for name in ("probe_berry", "probe_clover"):
            expect(first.fixture(name) != second.fixture(name),
                   f"the {name} fixture resolves to disjoint paths, so one "
                   f"run cannot overwrite the other's between its write and "
                   f"the engine-side read")
        expect(first.engine_log != second.engine_log,
               "two concurrent runs cannot truncate one another's engine log")
        expect(first.root != second.root,
               "and each keeps its own resource root, so its saves/ too")
        for art in (first, second):
            for path in (art.fixture("probe_berry"), art.fixture("probe_clover"),
                         art.engine_log, art.root):
                expect(path.startswith(art.base + os.sep),
                       f"{os.path.basename(path)} lands inside the run's own "
                       f"directory")


def test_no_artifact_keeps_a_legacy_fixed_tmp_name() -> None:
    print("\ntest_no_artifact_keeps_a_legacy_fixed_tmp_name")
    with fresh_run() as art:
        chosen = {art.fixture("probe_berry"), art.fixture("probe_clover"),
                  art.engine_log}
        for legacy in LEGACY_PATHS:
            expect(legacy not in chosen,
                   f"nothing this run writes resolves to {legacy}")
        expect(os.path.realpath(art.base) != os.path.realpath("/tmp"),
               "the run's own directory is not /tmp itself")


def test_release_never_touches_what_the_run_did_not_create() -> None:
    print("\ntest_release_never_touches_what_the_run_did_not_create")
    outside = tempfile.mkdtemp(prefix="test_flora_growth_decoy_")
    try:
        # A developer's own copies, sharing the probe's logical names, in
        # a directory this run never owned. Deleting one is the obvious
        # failure; opening one `"w"` is the quieter one #1682 names,
        # so the bytes are what is compared.
        decoys = {}
        for name in ("probe_berry.yaml", "probe_clover.yaml", "engine.log"):
            path = os.path.join(outside, name)
            payload = f"a developer's {name}, not the probe's".encode()
            with open(path, "wb") as handle:
                handle.write(payload)
            decoys[path] = payload
        code, raised, _text, base = run_main([], lambda art: True)
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
        released = probe.release_artifacts(art, keep=False)
        expect(released, "a clean removal reports success")
        expect(not os.path.exists(art.base), "the run's own tree is gone")
        expect(sorted(os.listdir(os.path.join(probe.REPO, "scripts"))) == before,
               "the real scripts/ is untouched — rmtree unlinked the symlink")


@contextlib.contextmanager
def read_only_checkout():
    """A stand-in checkout whose `config/` (and a subdirectory of it) is
    mode 0555, with `probe.REPO` pointed at it.

    `shutil.copytree` reproduces the source's mode bits, so this is what
    a read-only checkout — a CI cache restored read-only, a read-only
    mount, an archive unpacked without write bits — hands the run: a
    private `config/` whose entries cannot be unlinked. Always restored
    to writable before teardown, so the fixture can remove itself.
    """
    repo = tempfile.mkdtemp(prefix="test_flora_growth_ro_repo_")
    for family in ("scripts", "assets", "data"):
        os.makedirs(os.path.join(repo, family))
    config = os.path.join(repo, "config")
    os.makedirs(os.path.join(config, "nested"))
    for path in (os.path.join(config, "video_default.yaml"),
                 os.path.join(config, "nested", "extra_default.yaml")):
        with open(path, "w") as handle:
            handle.write("tracked: default\n")
        os.chmod(path, 0o444)
    os.chmod(os.path.join(config, "nested"), 0o555)
    os.chmod(config, 0o555)
    original = probe.REPO
    probe.REPO = repo
    try:
        yield repo
    finally:
        probe.REPO = original
        for path, dirs, _files in os.walk(repo):
            os.chmod(path, 0o755)
            for name in dirs:
                os.chmod(os.path.join(path, name), 0o755)
        shutil.rmtree(repo, ignore_errors=True)


def test_a_read_only_checkout_still_yields_a_removable_tree() -> None:
    print("\ntest_a_read_only_checkout_still_yields_a_removable_tree")
    with read_only_checkout():
        art = probe.RunArtifacts(
            tempfile.mkdtemp(prefix="test_flora_growth_ro_"))
        try:
            art.build()
            config = os.path.join(art.root, "config")
            expect(os.access(config, os.W_OK | os.X_OK),
                   "the private config/ is writable by this run even though "
                   "the source was not")
            expect(os.access(os.path.join(config, "nested"), os.W_OK | os.X_OK),
                   "...and so is a nested config directory")
            expect(os.access(os.path.join(config, "video_default.yaml"),
                             os.W_OK),
                   "...and a copied config file, which the engine rewrites "
                   "when it saves settings")
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                released = probe.release_artifacts(art, keep=False)
            expect(released and not os.path.exists(art.base),
                   f"the run removes its own tree instead of reporting "
                   f"residue (said {out.getvalue().strip()!r})")
        finally:
            for path, dirs, _files in os.walk(art.base):
                os.chmod(path, 0o755)
                for name in dirs:
                    os.chmod(os.path.join(path, name), 0o755)
            shutil.rmtree(art.base, ignore_errors=True)


def test_the_read_only_source_itself_is_never_modified() -> None:
    print("\ntest_the_read_only_source_itself_is_never_modified")
    with read_only_checkout() as repo:
        config = os.path.join(repo, "config")
        watched = (config, os.path.join(config, "nested"),
                   os.path.join(config, "video_default.yaml"))
        before = {path: os.stat(path).st_mode for path in watched}
        art = probe.RunArtifacts(
            tempfile.mkdtemp(prefix="test_flora_growth_ro2_"))
        try:
            art.build()
            with contextlib.redirect_stdout(io.StringIO()):
                probe.release_artifacts(art, keep=False)
            after = {path: os.stat(path).st_mode for path in watched}
            expect(before == after,
                   "the checkout's own modes are untouched — only this "
                   "run's copy is relaxed")
        finally:
            shutil.rmtree(art.base, ignore_errors=True)


# ---------------------------------------------------------------------
# The guard around the whole run
# ---------------------------------------------------------------------
def test_a_passing_run_leaves_nothing() -> None:
    print("\ntest_a_passing_run_leaves_nothing")
    code, raised, text, base = run_main([], lambda art: True)
    expect(code == 0 and raised is None,
           f"a run whose checks all passed exits 0 (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a passing run leaves no artifact directory")
    expect(base is not None and base not in text,
           "and its summary does not point at a directory that was deleted")
    expect("--keep-artifacts" not in text,
           "a passing run does not offer the diagnostic flag it did not need")


def test_an_early_return_still_releases() -> None:
    print("\ntest_an_early_return_still_releases")

    def early_return(art):
        # What the probe does when its own fixture never places: the
        # tree is staged, most phases never run.
        art.build()
        print("  [FAIL] probe_berry fixture not found in scan region")
        return False

    code, raised, text, base = run_main([], early_return)
    expect(code == 1 and raised is None,
           f"an early return exits non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "an early return leaves no artifact directory")
    expect("SOME FAILED" in text, "and the summary still reports the failure")


def test_an_exception_mid_run_still_releases() -> None:
    print("\ntest_an_exception_mid_run_still_releases")

    def blow_up(art):
        art.build()
        raise RuntimeError("kaboom")

    code, raised, _text, base = run_main([], blow_up)
    expect(isinstance(raised, RuntimeError) and code is None,
           f"an unexpected exception propagates with its traceback, so "
           f"`sys.exit(main())` never runs and the interpreter exits "
           f"non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "an unexpected exception leaves no artifact directory")


def test_a_boot_abort_still_releases_and_points_at_the_flag() -> None:
    print("\ntest_a_boot_abort_still_releases_and_points_at_the_flag")

    def abort(art):
        # How `probelib.boot` ends a run whose engine died before READY.
        # Its message names the log path verbatim — the path the release
        # below is about to delete.
        art.build()
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
        art.build()
        raise KeyboardInterrupt()

    code, raised, _text, base = run_main([], interrupt)
    expect(isinstance(raised, KeyboardInterrupt) and code is None,
           f"a handled Ctrl-C still ends the run as an interrupt "
           f"(got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a handled Ctrl-C leaves no artifact directory")


def test_cleanup_failure_fails_an_otherwise_clean_run() -> None:
    print("\ntest_cleanup_failure_fails_an_otherwise_clean_run")
    for broken, phrase in (("noop", "survived removal"),
                           ("raises", "could not remove")):
        real_rmtree = shutil.rmtree
        if broken == "noop":
            shutil.rmtree = lambda *a, **k: None
        else:
            def refuse(*_a, **_k):
                raise OSError("permission denied")

            shutil.rmtree = refuse
        try:
            code, raised, text, base = run_main([], lambda art: True)
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


class _StandInEngine:
    """Stands in for the `subprocess.Popen` `probelib.boot` launches, so a
    teardown assertion can name the exact handle that was disposed of and
    say HOW — killed directly, or asked to quit through the port."""

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


def drive_run_probe(art, boot_result, after_boot, launched_handle=None,
                    quit_raises=None) -> tuple[list, BaseException | None]:
    """Call the REAL `run_probe` with `boot`, `quit_engine` and
    `bootstrap` replaced, and report `(handles shut down, what
    propagated)`.

    `boot_result` is either the handle the stand-in boot returns or an
    exception it raises; `launched_handle` is what the stand-in registers
    through `on_launch` before either, standing for the process that
    already exists while `boot` is still deciding; `after_boot` runs in
    `bootstrap`'s place, one statement past the boot; `quit_raises`
    makes the orderly shutdown itself fail, which is what a Ctrl-C
    delivered inside `quit_engine` looks like.
    """
    quits: list = []

    def stub_boot(_port, _log, args=None, on_launch=None, **_kw):
        # What `probelib.boot` does: hand the handle over the statement
        # after its own `Popen`, long before it decides about READY.
        if on_launch is not None and launched_handle is not None:
            on_launch(launched_handle)
        if isinstance(boot_result, BaseException):
            raise boot_result
        return boot_result

    def stub_quit(_port, proc=None, **_kw):
        quits.append(proc)
        if quit_raises is not None:
            raise quit_raises()
        # What the real one leaves behind: it waits out the exit and
        # hard-kills if it has to, so a shutdown that COMPLETED always
        # ends with a dead process.
        if proc is not None:
            proc.alive = False

    def stub_bootstrap(_port, _art):
        if after_boot is not None:
            after_boot()

    original = probe.boot, probe.quit_engine, probe.bootstrap
    probe.boot, probe.quit_engine, probe.bootstrap = (
        stub_boot, stub_quit, stub_bootstrap)
    raised: BaseException | None = None
    try:
        with contextlib.redirect_stdout(io.StringIO()):
            probe.run_probe(_Args(), art)
    except BaseException as exc:  # noqa: BLE001 - the point of these cases
        raised = exc
    finally:
        probe.boot, probe.quit_engine, probe.bootstrap = original
    return quits, raised


class _Args:
    port = 9186
    seed = 42
    size = 64
    plates = 3
    keep_artifacts = False


def test_a_booted_engine_is_always_shut_down() -> None:
    print("\ntest_a_booted_engine_is_always_shut_down")
    # The teardown guard has to be armed BEFORE the boot: `boot` hands
    # back a LIVE engine, so anything raised at the very next statement
    # would otherwise leave the process running with nothing holding its
    # handle — the port and this run's files with it.
    for label, blow_up in (
            ("a handled Ctrl-C", KeyboardInterrupt),
            ("an unexpected exception", lambda: RuntimeError("kaboom")),
            ("a setup abort", lambda: SystemExit("setup gave up"))):
        engine = _StandInEngine()

        def raise_it():
            raise blow_up()

        with fresh_run() as art:
            quits, raised = drive_run_probe(art, engine, raise_it,
                                            launched_handle=engine)
        expect(quits == [engine],
               f"{label} one statement after the boot still shuts this "
               f"run's engine down through the port (got {quits})")
        expect(not engine.killed,
               f"...through engine.quit(), not a kill — the boot returned, "
               f"so the port is this run's ({label})")
        expect(raised is not None,
               f"...and {label} still ends the run (got {raised!r})")


def test_a_boot_that_aborted_is_never_sent_quit() -> None:
    print("\ntest_a_boot_that_aborted_is_never_sent_quit")
    # `probelib.boot` disposes of the process it started on both of its
    # own failure paths, so the handle it registered is already dead —
    # and a busy port is exactly why a boot fails, which makes an
    # `engine.quit()` sent anyway an attack on somebody else's instance.
    def unreachable():
        raise AssertionError("bootstrap ran after the boot had failed")

    dead = _StandInEngine(alive=False)
    with fresh_run() as art:
        quits, raised = drive_run_probe(
            art, SystemExit("engine exited before READY"), unreachable,
            launched_handle=dead)
    expect(quits == [],
           f"a boot abort sends no engine.quit() at whoever holds the "
           f"port (got {quits})")
    expect(not dead.killed,
           "and nothing is killed twice — boot already disposed of it")
    expect(isinstance(raised, SystemExit),
           f"...and the abort still ends the run (got {raised!r})")


def test_an_engine_stranded_inside_boot_is_still_disposed_of() -> None:
    print("\ntest_an_engine_stranded_inside_boot_is_still_disposed_of")
    # THE window this split exists for. `probelib.boot` registers the
    # process the statement after its `Popen` and then waits up to
    # `ready_timeout` — three minutes — for READY. An interrupt anywhere
    # in that span leaves a LIVE engine that `boot` never got to decide
    # about, and that no `proc = boot(...)` assignment will ever name.
    for label, blow_up in (("a handled Ctrl-C", KeyboardInterrupt),
                           ("an unexpected exception",
                            lambda: RuntimeError("kaboom"))):
        stranded = _StandInEngine()
        with fresh_run() as art:
            quits, raised = drive_run_probe(art, blow_up(), None,
                                            launched_handle=stranded)
        expect(stranded.killed,
               f"{label} inside boot still disposes of the engine it had "
               f"already launched (got {stranded})")
        expect(quits == [],
               f"...directly, never through the port — boot never returned, "
               f"so the listener there may be somebody else's ({label})")
        expect(raised is not None,
               f"...and {label} still ends the run (got {raised!r})")

    # An already-dead stranded handle is left alone rather than killed
    # again, so the disposal is not merely unconditional.
    already_gone = _StandInEngine(alive=False)
    with fresh_run() as art:
        drive_run_probe(art, KeyboardInterrupt(), None,
                        launched_handle=already_gone)
    expect(not already_gone.killed,
           "a handle that has already exited is not killed again")


def test_an_interrupt_during_the_shutdown_still_kills_the_engine() -> None:
    print("\ntest_an_interrupt_during_the_shutdown_still_kills_the_engine")
    # `quit_engine` sends `engine.quit()`, waits out the exit, then
    # hard-kills — three interruptible steps. A Ctrl-C in any of them
    # used to unwind straight out of the teardown with the engine still
    # running, holding the port and the log `main` was about to delete.
    for label, blow_up in (
            ("a handled Ctrl-C", KeyboardInterrupt),
            ("a socket failure", lambda: OSError("the console went away"))):
        engine = _StandInEngine()

        def finish():
            raise SystemExit("checks finished")

        with fresh_run() as art:
            quits, raised = drive_run_probe(art, engine, finish,
                                            launched_handle=engine,
                                            quit_raises=blow_up)
        expect(quits == [engine],
               f"[{label}] the orderly shutdown really was attempted first "
               f"(got {quits})")
        expect(engine.killed,
               f"[{label}] and when it did not finish, the engine was killed "
               f"outright rather than left holding the port (got {engine})")
        expect(raised is not None and not isinstance(raised, SystemExit),
               f"[{label}] the interrupt still ends the run (got {raised!r})")


def test_the_boot_itself_is_inside_the_teardown_guard() -> None:
    print("\ntest_the_boot_itself_is_inside_the_teardown_guard")
    # The two cases above enter through `bootstrap`, one line past the
    # boot, and both shapes of this code protect that line. The window
    # they CANNOT reach is the store of the handle `boot` just returned:
    # CPython checks for a pending signal between bytecodes, so an
    # interrupt taken there leaves a live engine bound to a local with
    # no handler if the `try` starts only afterwards. Nothing at line
    # granularity can observe that, so the property is asserted on the
    # source the interpreter actually runs — the boot call must sit
    # INSIDE the guarded block, with the handle pre-set to None so a
    # boot that aborted still sends nothing.
    fn = ast.parse(inspect.getsource(probe.run_probe)).body[0]
    guards = [node for node in fn.body if isinstance(node, ast.Try)]
    expect(len(guards) == 1,
           f"run_probe has exactly one outermost teardown guard "
           f"(found {len(guards)})")
    if len(guards) != 1:
        return
    guard = guards[0]

    def calls(nodes, name):
        return [n for node in nodes for n in ast.walk(node)
                if isinstance(n, ast.Call) and isinstance(n.func, ast.Name)
                and n.func.id == name]

    before = [stmt for stmt in fn.body if stmt is not guard]
    expect(not calls(before, "boot") and len(calls(guard.body, "boot")) == 1,
           "the boot happens INSIDE the guard, so an interrupt at the "
           "store of its handle still reaches the shutdown")
    expect(len(calls(guard.finalbody, "quit_engine")) == 1,
           "the guard's finally is what shuts the engine down")
    presets = [stmt for stmt in before
               if isinstance(stmt, ast.Assign)
               and any(isinstance(t, ast.Name) and t.id == "proc"
                       for t in stmt.targets)
               and isinstance(stmt.value, ast.Constant)
               and stmt.value.value is None]
    expect(len(presets) == 1,
           "the handle is None until the boot returns, so a boot that "
           "aborted sends no engine.quit() at somebody else's instance")
    expect(any(isinstance(node, ast.If) and "proc" in ast.dump(node.test)
               and any(c.func.id == "quit_engine"  # type: ignore[union-attr]
                       for c in calls([node], "quit_engine"))
               for node in guard.finalbody),
           "...and the shutdown is conditional on having one")
    booted = calls(guard.body, "boot")
    expect(bool(booted) and any(kw.arg == "on_launch" for kw in booted[0].keywords),
           "the boot registers the process as it is LAUNCHED, so the span "
           "boot spends waiting for READY is covered too")
    expect(len(calls(guard.finalbody, "abandon_engine")) == 2,
           "a handle that only ever got registered is disposed of directly, "
           "and the orderly shutdown has a direct-kill fallback of its own")
    inner = [node for node in ast.walk(ast.Module(body=guard.finalbody,
                                                  type_ignores=[]))
             if isinstance(node, ast.Try)]
    expect(any(calls(g.body, "quit_engine") and calls(g.finalbody,
                                                      "abandon_engine")
               for g in inner),
           "...and that fallback is a finally around quit_engine, which is "
           "itself interruptible at every step")


# ---------------------------------------------------------------------
# Opt-in retention
# ---------------------------------------------------------------------
def test_retention_keeps_a_passing_run_passing() -> None:
    print("\ntest_retention_keeps_a_passing_run_passing")
    code, raised, text, base = run_main(["--keep-artifacts"],
                                        lambda art: True)
    try:
        expect(code == 0 and raised is None,
               f"the passing run still exits 0 (got {code}, {raised!r})")
        expect(base is not None and os.path.isdir(base),
               "--keep-artifacts retains a passing run's artifacts too")
        expect(base is not None and base in text,
               "...and the summary names where they are")
    finally:
        if base:
            shutil.rmtree(base, ignore_errors=True)


def test_retention_keeps_a_failing_run_failing() -> None:
    print("\ntest_retention_keeps_a_failing_run_failing")
    code, raised, text, base = run_main(["--keep-artifacts"],
                                        lambda art: (art.build(), False)[1])
    try:
        expect(code == 1 and raised is None,
               f"the failing run still exits non-zero (got {code}, {raised!r})")
        expect(base is not None and os.path.isdir(base),
               "--keep-artifacts retains a failing run's artifacts")
        expect(base is not None and base in text,
               "...and the summary names where they are")
        expect("re-run with --keep-artifacts" not in text,
               "...and does not tell the operator to pass the flag it "
               "already has")
    finally:
        if base:
            shutil.rmtree(base, ignore_errors=True)


def test_retention_reports_what_the_run_actually_produced() -> None:
    print("\ntest_retention_reports_what_the_run_actually_produced")
    # A run that died before READY holds a staged tree and nothing in it;
    # a finished run holds two fixtures, a log and a save slot. The
    # summary must describe the one it was handed.
    with fresh_run() as art:
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            expect(probe.release_artifacts(art, keep=True),
                   "retention is not a cleanup failure")
        text = out.getvalue()
        expect(os.path.isdir(art.base), "--keep-artifacts retains the tree")
        expect(text.count("(empty)") == 3,
               f"a pre-READY failure's retained tree is reported as empty "
               f"rather than as holding fixtures and a save slot "
               f"(got {text.count('(empty)')} empty lines)")
        for label, path in (("the artifact directory", art.base),
                            ("the engine log directory", art.logs),
                            ("the fixtures", art.fixtures),
                            ("the saves", os.path.join(art.root, "saves")),
                            ("the isolated root", art.root)):
            expect(path in text, f"the summary names {label}")

    with fresh_run() as art:
        open(art.engine_log, "w").close()
        open(art.fixture("probe_berry"), "w").close()
        open(art.fixture("probe_clover"), "w").close()
        os.makedirs(os.path.join(art.root, "saves", "flora_growth_check_abc"))
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            probe.release_artifacts(art, keep=True)
        text = out.getvalue()
        for name in ("engine.log", "probe_berry.yaml", "probe_clover.yaml",
                     "flora_growth_check_abc"):
            expect(name in text, f"a finished run's summary names {name}")
        expect("(empty)" not in text, "...and reports nothing as empty")


def test_retention_after_a_staging_failure_names_nothing_absent() -> None:
    print("\ntest_retention_after_a_staging_failure_names_nothing_absent")
    # `build()` stages incrementally, so a permission, source or
    # disk-space failure part-way through leaves a tree whose log,
    # fixture and saves directories were never created. Reporting those
    # as "(empty)" would name artifacts that do not exist at all.
    base = tempfile.mkdtemp(prefix="test_flora_growth_partial_")
    art = probe.RunArtifacts(base)
    try:
        os.makedirs(art.root)  # staging died right after the root itself
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            expect(probe.release_artifacts(art, keep=True),
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

    # And the mirror image: a tree staged all the way through, with
    # nothing written into it yet, really is empty rather than absent.
    with fresh_run() as art:
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            probe.release_artifacts(art, keep=True)
        text = out.getvalue()
        expect("never created" not in text,
               "a fully staged tree reports no directory as missing")
        expect(text.count("(empty)") == 3,
               f"...and its three empty directories as empty "
               f"(got {text.count('(empty)')})")


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


@contextlib.contextmanager
def stand_in_data_tree(flora_names: list[str]):
    """A cwd whose `data/` holds the four families `bootstrap` globs,
    with the flora files deliberately out of alphabetical order on disk
    so a lost `sorted()` is visible."""
    home = tempfile.mkdtemp(prefix="test_flora_growth_data_")
    for family in ("substances", "items", "materials", "flora"):
        os.makedirs(os.path.join(home, "data", family))
    for family, names in (("substances", ["a.yaml"]), ("items", ["b.yaml"]),
                          ("materials", ["c.yaml"]), ("flora", flora_names)):
        for name in names:
            with open(os.path.join(home, "data", family, name), "w") as handle:
                handle.write("# stand-in\n")
    original = os.getcwd()
    os.chdir(home)
    try:
        yield home
    finally:
        os.chdir(original)
        shutil.rmtree(home, ignore_errors=True)


@contextlib.contextmanager
def recorded_console(fixture_reply: str = "3"):
    """`probe.send` and `probelib.send` both replaced by one recorder, so
    the calls `bootstrap` makes directly and the ones the REAL
    `load_fixture_yaml` makes on its behalf land in one ordered list."""
    calls: list[str] = []

    def recorder(_port, lua, timeout=None, expect_result=True):
        calls.append(lua)
        if "probe_berry" in lua or "probe_clover" in lua:
            return fixture_reply
        return "7"

    original = probe.send, probelib.send
    probe.send, probelib.send = recorder, recorder
    try:
        yield calls
    finally:
        probe.send, probelib.send = original


def test_bootstrap_loads_real_flora_then_berry_then_clover() -> None:
    print("\ntest_bootstrap_loads_real_flora_then_berry_then_clover")
    # Placement hashes are indexed by registration order, so the real
    # species' rolls AND probe_berry's own index depend on this sequence.
    with stand_in_data_tree(["zz_late.yaml", "aa_early.yaml"]), \
            fresh_run() as art:
        with recorded_console() as calls:
            probe.bootstrap(9186, art)
        flora = [c for c in calls if "loadFloraYaml" in c]
        expect(flora[:2] == ["return engine.loadFloraYaml('data/flora/aa_early.yaml')",
                             "return engine.loadFloraYaml('data/flora/zz_late.yaml')"],
               f"the shipped flora is loaded first, in sorted order "
               f"(got {flora[:2]})")
        expect(len(flora) == 4,
               f"and then exactly the two fixtures (got {len(flora)} flora "
               f"loads in all)")
        expect(len(flora) > 2 and art.fixture("probe_berry") in flora[2],
               f"probe_berry is registered third (got {flora[2:3]!r})")
        expect(len(flora) > 3 and art.fixture("probe_clover") in flora[3],
               f"probe_clover is registered fourth, after it "
               f"(got {flora[3:4]!r})")
        for name, constant in (("probe_berry", probe.PROBE_BERRY_YAML),
                               ("probe_clover", probe.PROBE_CLOVER_YAML)):
            path = art.fixture(name)
            expect(os.path.isfile(path),
                   f"{name} was written inside the run's own directory")
            if os.path.isfile(path):
                with open(path, encoding="utf-8") as handle:
                    expect(handle.read() == constant,
                           f"...with {name}'s constant, exactly")


def test_a_rejected_fixture_still_stops_the_probe_at_setup() -> None:
    print("\ntest_a_rejected_fixture_still_stops_the_probe_at_setup")
    # #1342: both fixtures go through `load_fixture_yaml`, which raises
    # rather than let a fixture that registered nothing be reported as
    # downstream behavioural failures.
    with stand_in_data_tree(["only.yaml"]), fresh_run() as art:
        raised = None
        with recorded_console(fixture_reply="0"):
            try:
                probe.bootstrap(9186, art)
            except probelib.FixtureNotRegistered as exc:
                raised = exc
        expect(raised is not None,
               "a fixture the loader rejects stops the probe at setup")
        expect(raised is not None
               and art.fixture("probe_berry") in str(raised),
               f"and the failure names the fixture (got {raised!r})")


def main() -> int:
    selftest.parse_verbose()
    test_two_invocations_share_no_path()
    test_no_artifact_keeps_a_legacy_fixed_tmp_name()
    test_release_never_touches_what_the_run_did_not_create()
    test_release_does_not_follow_the_content_symlinks()
    test_a_read_only_checkout_still_yields_a_removable_tree()
    test_the_read_only_source_itself_is_never_modified()
    test_a_passing_run_leaves_nothing()
    test_an_early_return_still_releases()
    test_an_exception_mid_run_still_releases()
    test_a_boot_abort_still_releases_and_points_at_the_flag()
    test_a_keyboard_interrupt_still_releases()
    test_cleanup_failure_fails_an_otherwise_clean_run()
    test_a_booted_engine_is_always_shut_down()
    test_a_boot_that_aborted_is_never_sent_quit()
    test_an_engine_stranded_inside_boot_is_still_disposed_of()
    test_an_interrupt_during_the_shutdown_still_kills_the_engine()
    test_the_boot_itself_is_inside_the_teardown_guard()
    test_retention_keeps_a_passing_run_passing()
    test_retention_keeps_a_failing_run_failing()
    test_retention_reports_what_the_run_actually_produced()
    test_retention_after_a_staging_failure_names_nothing_absent()
    test_the_fixture_bodies_are_byte_for_byte_unchanged()
    test_bootstrap_loads_real_flora_then_berry_then_clover()
    test_a_rejected_fixture_still_stops_the_probe_at_setup()
    if FAILURES:
        print(f"\n{len(FAILURES)} check(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftest.concluded(1)
    return selftest.concluded(
        0, "\nAll flora_growth_probe artifact-ownership tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
