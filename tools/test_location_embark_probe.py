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

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


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
    original_run, original_argv = probe.run_probe, sys.argv
    probe.run_probe = wrapper
    sys.argv = ["location_embark_probe.py", *argv]
    try:
        with captured() as out:
            code = probe.main()
    finally:
        probe.run_probe, sys.argv = original_run, original_argv
        probe.failures.clear()
    return code, out.getvalue(), seen["base"]


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
                   == os.path.realpath(os.path.join(probe.REPO, family)),
                   f"{family}/ is a symlink to the checkout's own")
        config = os.path.join(art.root, "config")
        expect(os.path.isdir(config) and not os.path.islink(config),
               "config/ is a real copy, so the engine's writes stay in this run")
        expect(not [f for f in os.listdir(config) if f.endswith(".local.yaml")],
               "config/ copy excludes the developer's *.local.yaml overrides")
        tracked = sorted(f for f in os.listdir(os.path.join(probe.REPO, "config"))
                         if not f.endswith(".local.yaml"))
        expect(sorted(os.listdir(config)) == tracked,
               "config/ copy keeps every tracked default")
        saves = os.path.join(art.root, "saves")
        expect(os.path.isdir(saves) and not os.listdir(saves),
               "saves/ starts empty and belongs to this run")


@contextlib.contextmanager
def read_only_checkout():
    """A stand-in checkout whose `config/` (and a subdirectory of it) is
    mode 0555, with `probe.REPO` pointed at it.

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
        probe.failures.clear()
        art = probe.RunArtifacts(tempfile.mkdtemp(prefix="test_embark_ro_"))
        try:
            art.build()
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
        before = sorted(os.listdir(os.path.join(probe.REPO, "scripts")))
        probe.release_artifacts(art, keep=False)
        expect(not os.path.exists(art.base), "the run's own tree is gone")
        expect(not probe.failures,
               f"a clean removal records no failure (got {probe.failures})")
        expect(sorted(os.listdir(os.path.join(probe.REPO, "scripts"))) == before,
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


def main() -> int:
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
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll location_embark_probe artifact-ownership tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
