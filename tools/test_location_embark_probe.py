#!/usr/bin/env python3
"""Unit tests for the embark probe's artifact ownership (issue #1569).

`tools/location_embark_probe.py` is manual-only and `needs-gpu`, so its
own acceptance can only be observed by a long GPU run. The contract this
file pins is the half that is pure Python and would otherwise regress
silently: the probe creates every artifact inside ONE directory it owns,
boots every engine against that directory's resource root, and removes
the whole tree again on success, on failure, and on an abort — unless
`--keep-artifacts` says otherwise.

Five properties are asserted directly rather than inferred, because each
is a way the probe would leak, or has leaked:

  * The isolated root is real isolation, not a name: the content
    families are symlinks to the checkout, `config/` is a copy WITHOUT
    the developer's `*.local.yaml`, and `saves/` starts empty and
    belongs to this run.
  * Removal never follows those symlinks, so deleting a run's tree can
    never reach the real `scripts/`, `assets/` or `data/`.
  * A pre-existing directory the probe did not create is not touched,
    even when it carries one of the probe's own save-slot names.
  * Residue is a FAILING check. A run that cannot remove its own tree
    must not report a pass, and the failure must name what is left.
  * A read-only checkout still yields a removable tree. `copytree`
    reproduces the source's mode bits, so a read-only `config/` would
    otherwise produce a private copy whose entries cannot be unlinked —
    residue, and a failing run, from a source the probe only read.

The second contract (issue #1746) is the probe's two saves. Both slots
are read by a LATER session — one of them by a fresh process — and
`engine.saveWorld` only ACCEPTS synchronously, so neither the API's own
Boolean nor the request-specific `SaveCaptureComplete` may be skipped.
These tests drive `save_and_wait` and `run_probe` against stubbed
console responses (no engine, no port) and pin:

  * A refused save fails the probe, names the engine log, and never
    waits on a request that does not exist.
  * A missing request id fails the probe without calling
    `wait_save_complete` — `probelib.capture_request_id` is documented
    to return None on timeout.
  * A `SaveFailed` terminal phase, and a wait that times out with no
    status at all, both fail with the request id and what was observed.
  * The success path reports the request id and the terminal phase.
  * A failed save SUPPRESSES every session that would read the slot,
    rather than letting the missing file resurface as a load timeout in
    a later session.

Since #2164 the probe is a facade over `tools/location_embark/`, so a
name this file stubs may be resolved in one of those owner modules
rather than in the facade. Every stub therefore goes through `patched`
below, which refuses to replace a name the target module does not
already define, and every fixture additionally proves its stub was
really reached — a patch that lands where nothing reads it leaves these
tests passing while asserting nothing.

No engine, no world, no GPU: every test here runs against temporary
directories in well under a second.

Usage:
  python3 tools/test_location_embark_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import os
import shutil
import sys
import tempfile
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import location_embark_probe as probe  # type: ignore  # noqa: E402
from location_embark import invocation  # type: ignore  # noqa: E402

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402


@contextlib.contextmanager
def patched(module, **replacements):
    """Replace named attributes on the module that DEFINES them, failing
    loudly when one is not there to replace.

    A Python function resolves its globals in its OWN module, so once
    #2164 moved `RunArtifacts.build` and `save_and_wait` out of the
    facade, patching `location_embark_probe.REPO` or
    `location_embark_probe.send` would create an attribute nothing
    reads: the stub would stop intercepting, and every test built on it
    would keep passing while asserting nothing at all. The read-only
    checkout is the dangerous one —
    `test_a_read_only_checkout_still_yields_a_removable_tree` would then
    build against the REAL checkout and still report a pass.

    The `hasattr` gate turns a later move into a failure at the moment
    the name leaves the module. It is only half the proof, so every
    fixture below also asserts that its stub was actually CALLED, or
    that the run really observed the stand-in it was handed.
    """
    missing = sorted(name for name in replacements
                     if not hasattr(module, name))
    if missing:
        raise AssertionError(
            f"{module.__name__} defines no {', '.join(missing)} to patch: "
            f"the stub would land somewhere nothing reads and silently "
            f"stop intercepting")
    originals = {name: getattr(module, name) for name in replacements}
    for name, value in replacements.items():
        setattr(module, name, value)
    try:
        yield
    finally:
        for name, value in originals.items():
            setattr(module, name, value)


@contextlib.contextmanager
def fresh_run():
    """A built `RunArtifacts` on a temporary base, always cleaned up
    afterwards however the test leaves it — including when the test is
    about a removal that deliberately did not happen."""
    probe.failures.clear()
    art = probe.RunArtifacts(tempfile.mkdtemp(prefix="test_embark_"))
    try:
        art.build()
        yield art
    finally:
        shutil.rmtree(art.base, ignore_errors=True)
        probe.failures.clear()


@contextlib.contextmanager
def captured():
    """Run the probe's own reporting with both streams captured into one
    buffer, so a test can assert on what the summary does and does not
    say. The two streams are merged deliberately: `report` prints the
    `FAIL:` lines on stderr and the surrounding summary on stdout, and
    what these tests care about is what the operator sees."""
    out = io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(out):
        yield out


def run_main(argv: list[str], body) -> tuple[int, str, str]:
    """Drive `probe.main()` with `run_probe` replaced by `body`, and
    report `(exit code, stdout, the base directory the run owned)`.

    Substituting `run_probe` is what lets these tests exercise the
    guard's real paths — an early return, an exception, `probelib.boot`'s
    `SystemExit` abort — without booting an engine.
    """
    seen: dict[str, str] = {}

    def wrapper(args, w, h, art):
        seen["base"] = art.base
        body(art)

    probe.failures.clear()
    original_argv = sys.argv
    sys.argv = ["location_embark_probe.py", *argv]
    try:
        # `main` calls `run_probe` through this module's own globals, so
        # the facade is the module that defines it and the one to patch.
        with patched(probe, run_probe=wrapper), captured() as out:
            code = probe.main()
    finally:
        sys.argv = original_argv
        probe.failures.clear()
    # Without this the substitution could stop taking effect and every
    # caller below would assert about a directory no run ever owned.
    expect("base" in seen,
           "probe.main() really reached the substituted run_probe")
    return code, out.getvalue(), seen.get("base", "")


# ---------------------------------------------------------------------
# The isolated root
# ---------------------------------------------------------------------
def test_root_symlinks_content_and_copies_config() -> None:
    print("\ntest_root_symlinks_content_and_copies_config")
    with fresh_run() as art:
        for family in ("scripts", "assets", "data"):
            path = os.path.join(art.root, family)
            expect(os.path.islink(path)
                   and os.path.realpath(path)
                   == os.path.realpath(
                       os.path.join(invocation.REPO, family)),
                   f"{family}/ is a symlink to the checkout's own")
        config = os.path.join(art.root, "config")
        expect(os.path.isdir(config) and not os.path.islink(config),
               "config/ is a real copy, so the engine's writes stay in this run")
        expect(not [f for f in os.listdir(config) if f.endswith(".local.yaml")],
               "config/ copy excludes the developer's *.local.yaml overrides")
        tracked = sorted(
            f for f in os.listdir(os.path.join(invocation.REPO, "config"))
            if not f.endswith(".local.yaml"))
        expect(sorted(os.listdir(config)) == tracked,
               "config/ copy keeps every tracked default")
        saves = os.path.join(art.root, "saves")
        expect(os.path.isdir(saves) and not os.listdir(saves),
               "saves/ starts empty and belongs to this run")


@contextlib.contextmanager
def read_only_checkout():
    """A stand-in checkout whose `config/` (and a subdirectory of it) is
    mode 0555, with `location_embark.invocation.REPO` — the name
    `RunArtifacts.build` actually resolves — pointed at it.

    `shutil.copytree` reproduces the source's mode bits, so this is what
    a read-only checkout hands the run: a private `config/` whose entries
    cannot be unlinked. Always restored to writable before teardown, so
    the fixture can remove itself.
    """
    repo = tempfile.mkdtemp(prefix="test_embark_ro_repo_")
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
    # `RunArtifacts.build` reads `REPO` from the module that DEFINES it
    # (#2164 moved both into `location_embark.invocation`), so that is
    # the module this stand-in has to replace. Patching the facade's own
    # name instead would leave the builder reading the REAL checkout,
    # and this fixture would prove nothing while still passing.
    try:
        with patched(invocation, REPO=repo):
            yield repo
    finally:
        for path, dirs, _files in os.walk(repo):
            os.chmod(path, 0o755)
            for name in dirs:
                os.chmod(os.path.join(path, name), 0o755)
        shutil.rmtree(repo, ignore_errors=True)


def test_a_read_only_checkout_still_yields_a_removable_tree() -> None:
    print("\ntest_a_read_only_checkout_still_yields_a_removable_tree")
    with read_only_checkout() as repo:
        probe.failures.clear()
        art = probe.RunArtifacts(tempfile.mkdtemp(prefix="test_embark_ro_"))
        try:
            art.build()
            expect(os.path.realpath(os.path.join(art.root, "scripts"))
                   == os.path.realpath(os.path.join(repo, "scripts")),
                   "the builder really read the read-only stand-in, not the "
                   "checkout — the REPO stub intercepted")
            config = os.path.join(art.root, "config")
            expect(os.access(config, os.W_OK | os.X_OK),
                   "the private config/ is writable by this run even though "
                   "the source was not")
            expect(os.access(os.path.join(config, "nested"), os.W_OK | os.X_OK),
                   "...and so is a nested config directory")
            expect(os.access(os.path.join(config, "video_default.yaml"), os.W_OK),
                   "...and a copied config file, which the engine rewrites "
                   "when it saves settings")
            probe.release_artifacts(art, keep=False)
            expect(not os.path.exists(art.base),
                   "the run removes its own tree instead of reporting residue")
            expect(not probe.failures,
                   f"a read-only source is not a cleanup failure "
                   f"(got {probe.failures})")
        finally:
            for path, dirs, _files in os.walk(art.base):
                os.chmod(path, 0o755)
                for name in dirs:
                    os.chmod(os.path.join(path, name), 0o755)
            shutil.rmtree(art.base, ignore_errors=True)
            probe.failures.clear()


def test_the_read_only_source_itself_is_never_modified() -> None:
    print("\ntest_the_read_only_source_itself_is_never_modified")
    with read_only_checkout() as repo:
        config = os.path.join(repo, "config")
        before = {p: os.stat(p).st_mode
                  for p in (config,
                            os.path.join(config, "nested"),
                            os.path.join(config, "video_default.yaml"))}
        probe.failures.clear()
        art = probe.RunArtifacts(tempfile.mkdtemp(prefix="test_embark_ro2_"))
        try:
            art.build()
            probe.release_artifacts(art, keep=False)
            after = {p: os.stat(p).st_mode for p in before}
            expect(before == after,
                   "the checkout's own modes are untouched — only the copy "
                   "is relaxed")
        finally:
            shutil.rmtree(art.base, ignore_errors=True)
            probe.failures.clear()


def test_every_boot_is_pinned_to_the_run_root() -> None:
    print("\ntest_every_boot_is_pinned_to_the_run_root")
    with fresh_run() as art:
        expect(art.boot_args() == ["--resource-root", art.root],
               "a bare boot names this run's root")
        expect(art.boot_args(["--size", "1280x720"])
               == ["--size", "1280x720", "--resource-root", art.root],
               "an offscreen boot keeps --size and still names this run's root")
        expect(art.log("engine_prep").startswith(art.logs + os.sep),
               "engine logs land inside this run's directory, not /tmp")
        expect(art.shots.startswith(art.base + os.sep),
               "screenshots land inside this run's directory")


def test_two_invocations_share_no_path() -> None:
    print("\ntest_two_invocations_share_no_path")
    with fresh_run() as first, fresh_run() as second:
        expect(first.base != second.base,
               "two invocations own two directories")
        expect(os.path.join(first.root, "saves", probe.SAVE_BASE)
               != os.path.join(second.root, "saves", probe.SAVE_BASE),
               "the fixture slot resolves to disjoint paths, so the fixed "
               "key is safe")
        expect(first.log("engine_session_a") != second.log("engine_session_a"),
               "two concurrent runs cannot write one another's engine logs")


# ---------------------------------------------------------------------
# Release
# ---------------------------------------------------------------------
def test_release_removes_the_tree_without_following_symlinks() -> None:
    print("\ntest_release_removes_the_tree_without_following_symlinks")
    with fresh_run() as art:
        before = sorted(os.listdir(os.path.join(invocation.REPO, "scripts")))
        probe.release_artifacts(art, keep=False)
        expect(not os.path.exists(art.base), "the run's own tree is gone")
        expect(not probe.failures,
               f"a clean removal records no failure (got {probe.failures})")
        expect(sorted(os.listdir(os.path.join(invocation.REPO, "scripts")))
               == before,
               "the real scripts/ is untouched — rmtree unlinked the symlink")


def test_release_never_touches_what_the_run_did_not_create() -> None:
    print("\ntest_release_never_touches_what_the_run_did_not_create")
    outside = tempfile.mkdtemp(prefix="test_embark_decoy_")
    try:
        # A developer save slot sharing one of the probe's names, in a
        # resource root the probe never booted against.
        decoy = os.path.join(outside, "saves", probe.SAVE_BASE)
        os.makedirs(decoy)
        payload = os.path.join(decoy, "world.synworld")
        with open(payload, "wb") as handle:
            handle.write(b"developer save, not the probe's")
        with fresh_run() as art:
            probe.release_artifacts(art, keep=False)
        expect(os.path.isfile(payload),
               "a same-named save slot outside the run is still there")
        with open(payload, "rb") as handle:
            expect(handle.read() == b"developer save, not the probe's",
                   "...and byte-identical")
    finally:
        shutil.rmtree(outside, ignore_errors=True)


def test_residue_is_a_failing_check() -> None:
    print("\ntest_residue_is_a_failing_check")
    with fresh_run() as art:
        real_rmtree = shutil.rmtree
        shutil.rmtree = lambda *a, **k: None  # a removal that silently no-ops
        try:
            probe.release_artifacts(art, keep=False)
        finally:
            shutil.rmtree = real_rmtree
        expect(len(probe.failures) == 1,
               f"a surviving tree records exactly one failure "
               f"(got {probe.failures})")
        expect(probe.failures and art.base in probe.failures[0],
               "...and the failure names the residue")

    with fresh_run() as art:
        real_rmtree = shutil.rmtree

        def boom(*_a, **_k):
            raise OSError("permission denied")

        shutil.rmtree = boom
        try:
            probe.release_artifacts(art, keep=False)
        finally:
            shutil.rmtree = real_rmtree
        expect(len(probe.failures) == 1
               and art.base in probe.failures[0]
               and "permission denied" in probe.failures[0],
               f"a removal that raises is reported with its cause "
               f"(got {probe.failures})")


def test_keep_retains_and_names_the_directory() -> None:
    print("\ntest_keep_retains_and_names_the_directory")
    with fresh_run() as art:
        with captured() as out:
            probe.release_artifacts(art, keep=True)
        text = out.getvalue()
        expect(os.path.isdir(art.base), "--keep-artifacts retains the tree")
        expect(not probe.failures, "retention is not a failure")
        for label, path in (("the artifact directory", art.base),
                            ("the engine logs", art.logs),
                            ("the screenshots", art.shots),
                            ("the saves", os.path.join(art.root, "saves")),
                            ("the isolated root", art.root)):
            expect(path in text, f"the summary names {label}")


def test_keep_reports_what_the_run_actually_produced() -> None:
    print("\ntest_keep_reports_what_the_run_actually_produced")
    # A phase-0 failure retains a tree with nothing in it; a finished run
    # retains the real slots and shots. The summary must describe the one
    # it was handed, not the one the probe usually produces.
    with fresh_run() as art:
        with captured() as out:
            probe.release_artifacts(art, keep=True)
        text = out.getvalue()
        expect(text.count("(empty)") == 3,
               "a phase-0 failure's retained tree is reported as empty, "
               "not as holding the two save slots")
        expect(probe.SAVE_BASE not in text,
               "...and no save slot is named that does not exist")

    with fresh_run() as art:
        os.makedirs(os.path.join(art.root, "saves", probe.SAVE_BASE))
        os.makedirs(os.path.join(art.root, "saves", probe.SAVE_LOCAL))
        open(os.path.join(art.logs, "engine_prep.log"), "w").close()
        open(os.path.join(art.shots, "ghost_valid.png"), "w").close()
        with captured() as out:
            probe.release_artifacts(art, keep=True)
        text = out.getvalue()
        for name in (probe.SAVE_BASE, probe.SAVE_LOCAL,
                     "engine_prep.log", "ghost_valid.png"):
            expect(name in text, f"a finished run's summary names {name}")
        expect("(empty)" not in text,
               "...and reports nothing as empty")


# ---------------------------------------------------------------------
# The guard around the whole run
# ---------------------------------------------------------------------
def test_a_passing_run_leaves_nothing_and_its_summary_names_nothing() -> None:
    print("\ntest_a_passing_run_leaves_nothing_and_its_summary_names_nothing")
    code, text, base = run_main([], lambda art: art.build())
    expect(code == 0, f"a run with no failing check exits 0 (got {code})")
    expect(not os.path.exists(base), "a passing run leaves no artifact directory")
    expect(base not in text,
           "the summary does not point at a directory that was deleted")


def test_an_early_phase_zero_return_still_releases() -> None:
    print("\ntest_an_early_phase_zero_return_still_releases")

    def early_return(art):
        # What phase 0 does when no candidate seed places two ruins:
        # the isolated root exists, no offscreen session ever boots.
        art.build()
        probe.check("a candidate seed placed at least two ruin_small locations",
                    False, "tried seeds [1], best count 0")

    code, _text, base = run_main([], early_return)
    expect(code == 1, f"a phase-0 failure exits non-zero (got {code})")
    expect(not os.path.exists(base),
           "a phase-0 failure leaves no artifact directory")


def test_an_exception_mid_run_still_releases() -> None:
    print("\ntest_an_exception_mid_run_still_releases")

    def blow_up(art):
        art.build()
        raise RuntimeError("kaboom")

    code, _text, base = run_main([], blow_up)
    expect(code == 1, f"an unexpected exception exits non-zero (got {code})")
    expect(not os.path.exists(base),
           "an unexpected exception leaves no artifact directory")


def test_a_boot_abort_still_releases() -> None:
    print("\ntest_a_boot_abort_still_releases")

    def abort(art):
        # `probelib.boot` ends the run this way when an engine dies
        # before READY.
        art.build()
        raise SystemExit(f"prep engine exited before READY; "
                         f"see {art.log('engine_prep')}")

    code, text, base = run_main([], abort)
    expect(code == 1, f"a boot abort exits non-zero (got {code})")
    expect(not os.path.exists(base), "a boot abort leaves no artifact directory")
    expect("aborted before finishing" in text,
           "the summary reports the abort as a failing check")


def test_a_failing_run_can_retain_on_request() -> None:
    print("\ntest_a_failing_run_can_retain_on_request")

    def fail_after_build(art):
        art.build()
        probe.check("a synthetic check", False)

    code, text, base = run_main(["--keep-artifacts"], fail_after_build)
    try:
        expect(code == 1, f"the failing check still exits non-zero (got {code})")
        expect(os.path.isdir(base),
               "--keep-artifacts retains a failing run's artifacts")
        expect(base in text, "...and the summary names where they are")
    finally:
        shutil.rmtree(base, ignore_errors=True)


def test_a_passing_run_can_retain_on_request() -> None:
    print("\ntest_a_passing_run_can_retain_on_request")
    code, text, base = run_main(["--keep-artifacts"], lambda art: art.build())
    try:
        expect(code == 0, f"the passing run still exits 0 (got {code})")
        expect(os.path.isdir(base),
               "--keep-artifacts retains a passing run's artifacts too")
        expect(base in text, "...and the summary names where they are")
    finally:
        shutil.rmtree(base, ignore_errors=True)


def test_release_failure_fails_an_otherwise_clean_run() -> None:
    print("\ntest_release_failure_fails_an_otherwise_clean_run")
    real_rmtree = shutil.rmtree
    shutil.rmtree = lambda *a, **k: None
    try:
        code, text, base = run_main([], lambda art: art.build())
    finally:
        shutil.rmtree = real_rmtree
    try:
        expect(code == 1,
               f"a run that cannot remove its tree does not report a pass "
               f"(got {code})")
        expect("survived removal" in text and base in text,
               "the summary identifies the unintended residue")
    finally:
        real_rmtree(base, ignore_errors=True)


# ---------------------------------------------------------------------
# Durable saves (#1746)
# ---------------------------------------------------------------------
@contextlib.contextmanager
def stub_save(accepted: str, request_id, completion):
    """Replace the three console-facing names `save_and_wait` composes
    with fixed answers, and record what it actually called.

    `accepted` is what `engine.saveWorld` returns over the console,
    `request_id` what `capture_request_id` yields (None models its
    documented timeout), and `completion` the `(ok, status)` pair
    `wait_save_complete` returns. The recorder is what lets a test
    assert that a step was SKIPPED — waiting on a request id that was
    never captured is the specific mistake being ruled out.
    """
    calls: dict[str, list] = {"send": [], "capture": [], "wait": []}

    def fake_send(port, lua):
        calls["send"].append(lua)
        return accepted

    def fake_capture(port, status_lua, *a, **k):
        calls["capture"].append(status_lua)
        return request_id

    def fake_wait(port, rid, *a, **k):
        calls["wait"].append(rid)
        return completion

    probe.failures.clear()
    probe.set_log("/nonexistent/engine_stub.log")
    try:
        # `save_and_wait` composes these three through
        # `location_embark.invocation`'s own globals (#2164), so that is
        # where the stubs go. `calls["send"]` below is what PROVES they
        # landed: an unintercepted `save_and_wait` would reach a real
        # console and record nothing here.
        with patched(invocation, send=fake_send,
                     capture_request_id=fake_capture,
                     wait_save_complete=fake_wait):
            yield calls
    finally:
        expect(calls["send"],
               "the console stubs intercepted save_and_wait — it issued its "
               "engine.saveWorld through the patched module")
        probe.failures.clear()
        probe.set_log(None)


def test_a_refused_save_fails_and_never_waits() -> None:
    print("\ntest_a_refused_save_fails_and_never_waits")
    with stub_save("false", 7, (True, {"id": 7, "phase": "SaveCaptureComplete"})) as calls:
        with captured() as out:
            ok = invocation.save_and_wait(9420, "ew", probe.SAVE_BASE, "phase 0")
        text = out.getvalue()
        expect(ok is False, "a refused engine.saveWorld returns False")
        expect(len(probe.failures) == 1,
               f"...and records exactly one failure (got {probe.failures})")
        expect(probe.failures and "'false'" in probe.failures[0],
               f"...naming the value the API actually returned "
               f"(got {probe.failures})")
        expect(probe.failures and "engine_stub.log" in probe.failures[0],
               "...and the engine log holding the validation reason")
        expect(probe.SAVE_BASE in text, "the failing check names the slot")
        expect(not calls["capture"] and not calls["wait"],
               "a request that was never accepted is never waited on")


def test_a_missing_request_id_fails_without_waiting() -> None:
    print("\ntest_a_missing_request_id_fails_without_waiting")
    with stub_save("true", None, (True, {"phase": "SaveCaptureComplete"})) as calls:
        with captured():
            ok = invocation.save_and_wait(9420, "ew", probe.SAVE_LOCAL, "session b")
        expect(ok is False, "no request id is a failure, not a pass")
        expect(len(probe.failures) == 1
               and "request id" in probe.failures[0]
               and probe.SAVE_LOCAL in probe.failures[0],
               f"...reported with the slot it belongs to "
               f"(got {probe.failures})")
        expect(probe.failures and "engine_stub.log" in probe.failures[0],
               "...and the engine log to read it in")
        expect(calls["capture"] == ["return engine.getSaveStatus()"],
               f"the id is captured from getSaveStatus (got {calls['capture']})")
        expect(not calls["wait"],
               "wait_save_complete is not called without an id to wait on")


def test_a_failed_or_timed_out_save_fails_with_what_was_observed() -> None:
    print("\ntest_a_failed_or_timed_out_save_fails_with_what_was_observed")
    failed = {"id": 11, "phase": "SaveFailed", "message": "disk full"}
    with stub_save("true", 11, (False, failed)) as calls:
        with captured():
            ok = invocation.save_and_wait(9420, "ew", probe.SAVE_LOCAL, "session b")
        expect(ok is False, "a SaveFailed terminal phase is a failure")
        expect(calls["wait"] == [11],
               f"the wait is tied to THIS request id (got {calls['wait']})")
        expect(len(probe.failures) == 1
               and "11" in probe.failures[0]
               and "SaveFailed" in probe.failures[0]
               and probe.SAVE_LOCAL in probe.failures[0],
               f"the failure names the save, its request id and the observed "
               f"status (got {probe.failures})")

    # probelib.wait_save_complete returns (False, None) when the console
    # never reported a status for the request at all before its deadline.
    with stub_save("true", 12, (False, None)):
        with captured():
            ok = invocation.save_and_wait(9420, "ew", probe.SAVE_BASE, "phase 0")
        expect(ok is False, "a wait timeout is a failure, not a pass")
        expect(len(probe.failures) == 1
               and "12" in probe.failures[0]
               and probe.SAVE_BASE in probe.failures[0],
               f"...still naming which save and which request "
               f"(got {probe.failures})")


def test_a_completed_save_reports_its_request_and_phase() -> None:
    print("\ntest_a_completed_save_reports_its_request_and_phase")
    done = {"id": 4, "phase": "SaveCaptureComplete"}
    with stub_save("true", 4, (True, done)) as calls:
        with captured() as out:
            ok = invocation.save_and_wait(9420, "ew", probe.SAVE_BASE, "phase 0")
        text = out.getvalue()
        expect(ok is True, "a completed save returns True")
        expect(not probe.failures,
               f"...and records no failure (got {probe.failures})")
        expect(calls["send"] == ["return engine.saveWorld('ew', "
                                f"'{probe.SAVE_BASE}')"],
               f"the API's own result is returned, not a literal "
               f"(got {calls['send']})")
        expect(calls["wait"] == [4],
               f"completion is waited on for this request (got {calls['wait']})")
        expect("request 4" in text and "SaveCaptureComplete" in text,
               f"the output names the request id and terminal phase "
               f"(got {text!r})")


class _Args:
    """The subset of the parsed command line `run_probe` reads."""

    def __init__(self, keep_artifacts: bool = False) -> None:
        self.seed = 42
        self.alt_seeds = "7"
        self.size = 64
        self.port = 9420
        self.win_size = "1280x720"
        self.keep_artifacts = keep_artifacts


@contextlib.contextmanager
def stub_sessions(prepare):
    """Run `run_probe` with its engine boots and its three sessions
    replaced, recording which sessions ran and how many engines the run
    booted for them.

    `prepare` stands in for `prepare_fixture` and returns its
    `(seed, ruins)` pair. Everything the sessions themselves do needs a
    GPU; what these tests are about is which of them the run REACHES.
    """
    ran: list[str] = []
    engines: list[object] = []
    state: dict[str, object] = {"local_result": True}

    def fake_boot(*_a, **_k):
        engine = object()
        engines.append(engine)
        return engine

    def fake_quit(_port, engine, *_a, **_k):
        expect(engine in engines,
               "the run quits an engine this fixture handed it, so the boot "
               "stub really intercepted")

    def ghost(*_a, **_k):
        ran.append("a")

    def local(*_a, **_k):
        ran.append("b")
        return state["local_result"]

    def reload_check(*_a, **_k):
        ran.append("c")

    probe.failures.clear()
    try:
        # `run_probe` stayed in the facade, so these six ARE resolved in
        # `probe`'s globals and this is the module to patch. The
        # engine-count assertion in `_drive_run_probe` is what proves it:
        # an unintercepted `boot` would try to launch a real engine.
        with patched(probe,
                     prepare_fixture=lambda *a, **k: prepare(),
                     boot=fake_boot, quit_engine=fake_quit,
                     session_ghost_and_remote=ghost,
                     session_local_and_discovery=local,
                     session_reload_check=reload_check):
            yield ran, state, engines
    finally:
        probe.failures.clear()


def _drive_run_probe(prepare, local_result=True):
    """`run_probe` on a throwaway artifact tree; returns
    `(sessions that ran, captured output, failures recorded)`.

    The failure list is SNAPSHOT here rather than read from
    `probe.failures` afterwards: `stub_sessions` clears that module
    global on its way out, so an assertion made after the fixture closes
    would be asserting about an empty list whatever the run recorded.
    """
    art = probe.RunArtifacts(tempfile.mkdtemp(prefix="test_embark_gate_"))
    try:
        with stub_sessions(prepare) as (ran, state, engines):
            state["local_result"] = local_result
            with captured() as out:
                probe.run_probe(_Args(), 1280, 720, art)
            # One booted engine per session the run reached, and no
            # engine booted for a session it suppressed: the facade owns
            # the process lifecycle, so a session that "ran" without a
            # boot of its own would mean an owner opened its own.
            expect(len(engines) == len(ran),
                   f"each session the run reached got exactly one engine of "
                   f"its own (booted {len(engines)} for {ran})")
            return list(ran), out.getvalue(), list(probe.failures)
    finally:
        shutil.rmtree(art.base, ignore_errors=True)


def test_a_failed_fixture_save_suppresses_every_dependent_session() -> None:
    print("\ntest_a_failed_fixture_save_suppresses_every_dependent_session")
    ruins = [{"id": "ruin_small", "cx": 0, "cy": 0, "gx": 0, "gy": 0,
              "bounds": {}},
             {"id": "ruin_small", "cx": 1, "cy": 1, "gx": 8, "gy": 8,
              "bounds": {}}]
    # A qualifying seed whose SAVE failed: prepare_fixture reports the
    # ruins it found but no seed, because the fixture is not on disk.
    ran, text, recorded = _drive_run_probe(lambda: (None, ruins))
    expect(ran == [],
           f"sessions (a), (b) and (c) all read a slot that was never "
           f"published, so none of them runs (got {ran})")
    expect("skipped" in text and probe.SAVE_BASE in text,
           f"...and the run says so, naming the slot (got {text!r})")
    expect(not [f for f in recorded if "ruin_small" in f],
           f"the save failure is not misreported as a worldgen shortfall "
           f"(got {recorded})")
    expect("[PASS] a candidate seed placed at least two ruin_small" in text,
           f"...the worldgen check having actually passed (got {text!r})")

    # The pre-existing "no seed qualified" path still reads that way.
    ran, text, recorded = _drive_run_probe(lambda: (None, []))
    expect(ran == [], f"no qualifying seed still runs no session (got {ran})")
    expect(any("ruin_small" in f for f in recorded),
           f"...and is still recorded as a worldgen shortfall (got {recorded})")
    expect("[FAIL] a candidate seed placed at least two ruin_small" in text,
           f"...and reported as one (got {text!r})")


def test_a_failed_session_b_save_suppresses_only_session_c() -> None:
    print("\ntest_a_failed_session_b_save_suppresses_only_session_c")
    ruins = [{"id": "ruin_small", "cx": 0, "cy": 0, "gx": 0, "gy": 0,
              "bounds": {}},
             {"id": "ruin_small", "cx": 1, "cy": 1, "gx": 8, "gy": 8,
              "bounds": {}}]
    ran, text, _recorded = _drive_run_probe(lambda: (42, ruins),
                                            local_result=False)
    expect(ran == ["a", "b"],
           f"session (c) loads SAVE_LOCAL in a fresh process, so a session "
           f"(b) that published nothing stops it (got {ran})")
    expect("session (c) skipped" in text and probe.SAVE_LOCAL in text,
           f"...and the reason names the save, not the load (got {text!r})")

    ran, _text, _recorded = _drive_run_probe(lambda: (42, ruins),
                                             local_result=True)
    expect(ran == ["a", "b", "c"],
           f"a durable session (b) save still runs the reload check "
           f"(got {ran})")


def main() -> int:
    selftestlib.parse_verbose()
    test_root_symlinks_content_and_copies_config()
    test_a_read_only_checkout_still_yields_a_removable_tree()
    test_the_read_only_source_itself_is_never_modified()
    test_every_boot_is_pinned_to_the_run_root()
    test_two_invocations_share_no_path()
    test_release_removes_the_tree_without_following_symlinks()
    test_release_never_touches_what_the_run_did_not_create()
    test_residue_is_a_failing_check()
    test_keep_retains_and_names_the_directory()
    test_keep_reports_what_the_run_actually_produced()
    test_a_passing_run_leaves_nothing_and_its_summary_names_nothing()
    test_an_early_phase_zero_return_still_releases()
    test_an_exception_mid_run_still_releases()
    test_a_boot_abort_still_releases()
    test_a_failing_run_can_retain_on_request()
    test_a_passing_run_can_retain_on_request()
    test_release_failure_fails_an_otherwise_clean_run()
    test_a_refused_save_fails_and_never_waits()
    test_a_missing_request_id_fails_without_waiting()
    test_a_failed_or_timed_out_save_fails_with_what_was_observed()
    test_a_completed_save_reports_its_request_and_phase()
    test_a_failed_fixture_save_suppresses_every_dependent_session()
    test_a_failed_session_b_save_suppresses_only_session_c()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nAll location_embark_probe artifact-ownership and durable-save "
        "tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
