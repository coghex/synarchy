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

Seven properties are asserted directly rather than inferred, because each
is a way the probe would leak, collide, or stop proving what it claims:

  * Two invocations share no path — not the fixtures, not the log, not
    the root — so the fixed logical names inside each tree are safe.
  * No artifact keeps a legacy fixed `/tmp` name, so a pre-existing file
    at one of them is never opened for writing, truncated, modified or
    deleted. Nor is any same-named file the run did not create.
  * The tree is released after a pass, an early return, an exception, a
    `probelib.boot` abort, and a handled Ctrl-C.
  * Retention is opt-in, keeps the run's own success or failure result,
    names where the artifacts are, and describes what the run ACTUALLY
    produced rather than what a finished run usually would.
  * A default failing run says its log went with the tree and points at
    the flag, instead of leaving the operator chasing a deleted path.
  * Cleanup that cannot finish makes an otherwise passing run non-zero.
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

import contextlib
import hashlib
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
    test_two_invocations_share_no_path()
    test_no_artifact_keeps_a_legacy_fixed_tmp_name()
    test_release_never_touches_what_the_run_did_not_create()
    test_release_does_not_follow_the_content_symlinks()
    test_a_passing_run_leaves_nothing()
    test_an_early_return_still_releases()
    test_an_exception_mid_run_still_releases()
    test_a_boot_abort_still_releases_and_points_at_the_flag()
    test_a_keyboard_interrupt_still_releases()
    test_cleanup_failure_fails_an_otherwise_clean_run()
    test_retention_keeps_a_passing_run_passing()
    test_retention_keeps_a_failing_run_failing()
    test_retention_reports_what_the_run_actually_produced()
    test_the_fixture_bodies_are_byte_for_byte_unchanged()
    test_bootstrap_loads_real_flora_then_berry_then_clover()
    test_a_rejected_fixture_still_stops_the_probe_at_setup()
    if FAILURES:
        print(f"\n{len(FAILURES)} check(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll flora_growth_probe artifact-ownership tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
