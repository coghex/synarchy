#!/usr/bin/env python3
"""Unit tests for persistence_contract_sweep.py's registry-drift guard
(issue #1321) and its durable phase records (issue #1768).

`SELECTABLE_CROSS_REFERENCED_PROBE_KEYS` is a hand-maintained copy of a
subset of `probe_runner_registry.PROBES`. Nothing previously
checked the two agree: a key renamed or removed in `PROBES` would leave
`persistence_contract_sweep.py --cross-probe-keys ...` handing a dead key
to `run_probes.py --exact`, which silently drops it, after which the
sweep reports "cross-referenced probes (...) all passed" while having run
fewer than it named -- the exact false-green requirement 11/13 coverage
exists to prevent.

`unregistered_selectable_probe_keys` is the pure, parameterized check
`persistence_contract_sweep.main` runs against the real lists before
booting anything. These tests exercise it directly -- no engine, no
probe, no subprocess -- proving both that today's real pairing is clean
AND (the round-2 review's point) that a deliberately introduced
disagreement is actually caught rather than merely asserting the
current state happens to be fine.

The #1768 half is the same shape: `SWEEP_PHASE_IDENTITIES` and
`announce_phase` are the sweep's declared phase contract, and every
record they emit has to be recognizable to `probe_runner_diagnostics`'s
timeout attribution -- the consumer at the other end of the pipe. These
tests hold both halves against each other without booting anything.

The #2060 half is the failing counterpart. `Checks.ok` used to retain a
COUNT and nothing else, so a failed check that enough sweep output
followed was truncated out of `run_probes.py`'s bounded `--tail 25`
presentation and its identity was simply gone -- which is what the
2026-08-31 artifact lost. `Checks` now emits one durable
`#probe-failure#` record per failed check, and these tests drive the
REAL `Checks` through the REAL `FailureEmitter` print path, bury the
records under far more than a tail's worth of ordinary output, and
require `probe_runner_diagnostics.failure_attribution` to name every one
of them back -- the same reader the runner applies to the complete
capture in both its sequential and its `--jobs N` mode, so neither mode
needs a real 900 s sweep run to be covered.

The #2805 cases drive the real CLI, comparison adapter and capture/cleanup
boundary with engine, codec and child execution stubbed. They pin opt-in
retry forwarding and evidence retention without changing the default run.

Usage:
  python3 tools/test_persistence_contract_sweep.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import json
import shutil
import tempfile
from unittest.mock import Mock, patch
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from persistence_contract_sweep import (  # type: ignore
    SELECTABLE_CROSS_REFERENCED_PROBE_KEYS,
    SWEEP_CYCLE_LETTERS,
    SWEEP_FAILURE_PRODUCER,
    SWEEP_PHASE_COMPARISON,
    SWEEP_PHASE_CROSS_PROBES,
    SWEEP_PHASE_ENGINE_A,
    SWEEP_PHASE_IDENTITIES,
    Checks,
    announce_phase,
    engine_cycle_phase,
    unregistered_selectable_probe_keys,
)
import persistence_contract_sweep as sweep
import persistence_snapshot as snapshot
import save_compat_audit_codec as codec
import probe_runner_diagnostics  # type: ignore
from probe_runner_registry import PROBES  # type: ignore

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402


def test_todays_selectable_keys_are_all_registered() -> None:
    print("\n-- every SELECTABLE_CROSS_REFERENCED_PROBE_KEYS entry names a "
          "real run_probes.py probe today")
    registered = {p[0] for p in PROBES}
    stale = unregistered_selectable_probe_keys(
        SELECTABLE_CROSS_REFERENCED_PROBE_KEYS, registered)
    expect(stale == [],
           f"the real registry and SELECTABLE_CROSS_REFERENCED_PROBE_KEYS "
           f"disagree on {stale!r}")


def test_a_key_the_registry_drops_is_caught() -> None:
    print("\n-- a selectable key that vanishes from the registry is reported, "
          "not silently accepted (round-2 review's negative regression)")
    registered = {p[0] for p in PROBES} - {"chop"}
    stale = unregistered_selectable_probe_keys(
        SELECTABLE_CROSS_REFERENCED_PROBE_KEYS, registered)
    expect(stale == ["chop"],
           f"removing 'chop' from the registry should surface it alone, "
           f"got {stale!r}")


def test_every_stale_key_is_named_at_once() -> None:
    print("\n-- every stale key is identified, not just the first")
    stale = unregistered_selectable_probe_keys(
        ["chop", "not_a_probe", "till", "also_not_a_probe"],
        {"chop", "till"})
    expect(stale == ["not_a_probe", "also_not_a_probe"],
           f"expected both unknown keys in request order, got {stale!r}")


def test_empty_selectable_list_has_nothing_stale() -> None:
    print("\n-- an empty selectable list trivially has no stale entries")
    stale = unregistered_selectable_probe_keys([], {"chop", "till"})
    expect(stale == [], f"expected no stale keys, got {stale!r}")


# --------------------------------------------------------------------------
# Durable phase records (#1768)
# --------------------------------------------------------------------------
class CapturedEmitter(probe_runner_diagnostics.ProgressEmitter):
    """The real emitter, with its flushed line kept instead of printed.

    Subclassing rather than redirecting stdout keeps the formatting under
    test the shipped one: only the delivery is replaced.
    """

    def __init__(self) -> None:
        super().__init__()
        self.lines: list[str] = []

    def emit(self, kind: str, identity: str, detail: str) -> str:
        line = probe_runner_diagnostics.format_progress(kind, identity, detail,
                                          elapsed=0.0, now=0.0)
        self.lines.append(line)
        return line


def test_every_required_sweep_phase_is_declared() -> None:
    print("\n-- the four phases #1768 requires are all declared identities")
    expect(SWEEP_PHASE_ENGINE_A in SWEEP_PHASE_IDENTITIES,
           f"engine A's phase is declared (identities: "
           f"{SWEEP_PHASE_IDENTITIES!r})")
    cycles = [engine_cycle_phase(letter) for letter in SWEEP_CYCLE_LETTERS]
    expect(len(cycles) == 3,
           f"requirement 9's three fresh-process cycles are all named "
           f"(got {cycles!r})")
    expect(all(cycle in SWEEP_PHASE_IDENTITIES for cycle in cycles),
           f"and each is a declared phase identity (got {cycles!r})")
    expect(SWEEP_PHASE_COMPARISON in SWEEP_PHASE_IDENTITIES,
           "the structural comparison's phase is declared")
    expect(SWEEP_PHASE_CROSS_PROBES in SWEEP_PHASE_IDENTITIES,
           "the cross-referenced probe phase is declared")
    expect(len(set(SWEEP_PHASE_IDENTITIES)) == len(SWEEP_PHASE_IDENTITIES),
           f"no identity is declared twice, so begin/end pairing and "
           f"'latest phase' stay unambiguous ({SWEEP_PHASE_IDENTITIES!r})")


def test_an_undeclared_phase_is_refused_rather_than_emitted() -> None:
    print("\n-- a phase missing from the declared list is refused, so the "
          "list cannot silently go stale")
    emitter = CapturedEmitter()
    try:
        announce_phase(emitter, "engine Z", "a phase nobody declared")
    except ValueError:
        expect(True, "an undeclared phase identity raises")
    else:
        expect(False, "an undeclared phase identity should have raised")
    expect(emitter.lines == [],
           f"and emits nothing at all (got {emitter.lines!r})")


def test_every_declared_phase_emits_a_record_the_runner_recognizes() -> None:
    print("\n-- each phase record parses back through the diagnostics "
          "reader, with the right kind and identity")
    for identity in SWEEP_PHASE_IDENTITIES:
        emitter = CapturedEmitter()
        line = announce_phase(emitter, identity, "some detail")
        expect(emitter.lines == [line],
               f"{identity!r} emitted exactly one record (got "
               f"{emitter.lines!r})")
        record = probe_runner_diagnostics.parse_progress(line)
        expect(record is not None,
               f"{identity!r}'s record parses as a progress record "
               f"({line!r})")
        expect(record is not None and record.kind == "phase",
               f"{identity!r}'s record is a phase record (got {record!r})")
        expect(record is not None and record.identity == identity,
               f"{identity!r}'s record carries its identity (got {record!r})")
        expect(record is not None and record.detail == "some detail",
               f"{identity!r}'s record carries its detail (got {record!r})")


def test_the_latest_sweep_phase_survives_a_long_tail() -> None:
    print("\n-- the runner's attribution names the sweep's LATEST phase even "
          "when far more than --tail 25 lines followed it")
    emitter = CapturedEmitter()
    for identity in SWEEP_PHASE_IDENTITIES:
        announce_phase(emitter, identity, f"detail for {identity}")
    capture = "\n".join(
        emitter.lines + [f"ordinary sweep output {i}" for i in range(60)])
    got = probe_runner_diagnostics.progress_attribution(capture)
    text = "\n".join(got)
    latest = SWEEP_PHASE_IDENTITIES[-1]
    expect(latest in text,
           f"the last phase entered ({latest!r}) is named (got {got!r})")
    expect(SWEEP_PHASE_ENGINE_A not in text,
           f"and the phases it superseded are not (got {got!r})")
    expect("ordinary sweep output 0" not in text,
           f"the attribution does not dump the capture (got {got!r})")


# --------------------------------------------------------------------------
# Durable failed-check records (#2060)
#
# The runner's default presentation prints a bounded tail of the capture,
# so what is retained about a failure is whatever `failure_attribution`
# recovers from the COMPLETE capture plus the last `--tail` ORDINARY
# lines. Every case below therefore drives the real `Checks` through the
# real `FailureEmitter` print path and then buries its records under more
# ordinary output than that tail can hold: a `Checks` that only counted
# would leave nothing for the reader to find.
# --------------------------------------------------------------------------
def runner_default_tail() -> int:
    """`tools/run_probes.py`'s OWN default `--tail`, read from its parser.

    The burial below has to be measured against the number the runner
    really uses, not a copy of it: a default raised past the burial size
    would leave every case here passing while proving nothing, because
    the printed `[FAIL]` lines would still be sitting in the tail. The
    runner builds its parser inside `main` and exposes it nowhere, so the
    default is taken at the moment it would be used and the parse is
    abandoned -- nothing is selected, launched, or run.
    """
    import argparse
    import run_probes  # type: ignore

    captured: list[int] = []

    def intercept(self, *args, **kwargs):
        captured.append(self.get_default("tail"))
        raise SystemExit(0)

    real = argparse.ArgumentParser.parse_args
    argparse.ArgumentParser.parse_args = intercept   # type: ignore[method-assign]
    try:
        argv, sys.argv = sys.argv, ["run_probes.py", "--list"]
        try:
            run_probes.main()
        except SystemExit:
            pass
        finally:
            sys.argv = argv
    finally:
        argparse.ArgumentParser.parse_args = real   # type: ignore[method-assign]
    if len(captured) != 1 or not isinstance(captured[0], int):
        raise AssertionError(
            f"could not read run_probes.py's default --tail (got "
            f"{captured!r}); these cases cannot claim a record is outside "
            f"the retained tail without it")
    return captured[0]


#: `tools/run_probes.py`'s default `--tail`, so a case can say "outside the
#: retained tail" in the units the runner actually uses.
DEFAULT_TAIL = runner_default_tail()

#: One realistic failing sweep assertion per long phase, deliberately more
#: than one so "the SECOND failure was unrecoverable" -- the loss #2060 is
#: about -- is what these cases reproduce.
SAMPLE_FAILURES = [
    "gen1 saved page 'contract_sweep_page' back with its own visibility",
    "the craft bill survived the second load->save cycle intact",
    "cross-referenced probes (chop, till, crop) all passed",
]
SAMPLE_PASSES = [
    "the mine designation round-tripped",
    "the camera position is not the default",
]


def run_checks_capturing(failures, passes=()) -> tuple[Checks, str]:
    """Drive the REAL `Checks` and keep everything it wrote.

    `Checks()` builds its own real `FailureEmitter`, which prints, so the
    capture here is byte-for-byte what the sweep would push into the
    runner's pipe -- the emitter's flushing and formatting are under test
    rather than stood in for.
    """
    checks = Checks()
    buffer = io.StringIO()
    with contextlib.redirect_stdout(buffer):
        for label in passes:
            checks.ok(True, label)
        for label in failures:
            checks.ok(False, label)
    return checks, buffer.getvalue()


def bury(capture: str, lines: int | None = None) -> str:
    """`capture` followed by more ordinary output than the tail holds.

    Sized from the runner's real default rather than a fixed number, so
    the burial stays a burial if that default ever moves.
    """
    lines = DEFAULT_TAIL * 2 + 10 if lines is None else lines
    return capture + "".join(
        f"  [PASS] later sweep assertion {i}\n" for i in range(lines))


def retained_tail(capture: str) -> list[str]:
    """The ordinary lines the runner would actually print beside the block."""
    ordinary = probe_runner_diagnostics.without_failure_records(capture)
    return ordinary.splitlines()[-DEFAULT_TAIL:]


def test_the_producer_identity_names_this_script() -> None:
    print("\n-- the producer a sweep record carries is this script's own "
          "name, not some other spelling of it")

    # Asserted against the filename rather than a copy of the constant:
    # comparing the records below to the constant alone would hold only
    # that the sweep agrees with itself, and an operator reading
    # "N recorded failure(s) from <producer>" needs the name to be the
    # tool they would rerun. It is also the name that keeps a sweep-own
    # assertion apart from a cross-referenced probe's own records.
    import persistence_contract_sweep  # type: ignore
    expect(SWEEP_FAILURE_PRODUCER == "persistence_contract_sweep",
           f"the sweep records itself under the name #2060 names (got "
           f"{SWEEP_FAILURE_PRODUCER!r})")
    expect(SWEEP_FAILURE_PRODUCER
           == Path(persistence_contract_sweep.__file__).stem,
           f"which is the script's own filename (got "
           f"{SWEEP_FAILURE_PRODUCER!r})")
    expect(SWEEP_FAILURE_PRODUCER
           not in SELECTABLE_CROSS_REFERENCED_PROBE_KEYS,
           f"and is not one of the cross-referenced probe keys it would "
           f"be confused with (got {SWEEP_FAILURE_PRODUCER!r})")


def test_a_failed_sweep_check_records_its_own_label() -> None:
    print("\n-- one failed check produces exactly one durable record, "
          "carrying the whole label and naming the sweep as its producer")
    checks, capture = run_checks_capturing(SAMPLE_FAILURES[:1])
    records = probe_runner_diagnostics.failure_records(capture)
    expect(len(records) == 1,
           f"exactly one record, so the runner names it once (got "
           f"{records!r})")
    expect(records[0].kind == "check",
           f"in the 'check' vocabulary, not 'setup' -- a sweep assertion "
           f"is a product failure, not a fixture one (got {records[0]!r})")
    expect(records[0].identity == SWEEP_FAILURE_PRODUCER,
           f"naming {SWEEP_FAILURE_PRODUCER!r} as its producer, so a "
           f"sweep-own assertion stays distinguishable from a nested "
           f"probe's (got {records[0].identity!r})")
    expect(records[0].detail == SAMPLE_FAILURES[0],
           f"and carrying the COMPLETE label, which is the only thing "
           f"identifying which check this was (got {records[0].detail!r})")
    expect(checks.failed == 1,
           f"the numeric counter the terminal summary reports is unchanged "
           f"(got {checks.failed})")


def test_a_passing_sweep_check_emits_no_failure_marker() -> None:
    print("\n-- a passing check adds no record and no marker, so a green "
          "run is exactly as quiet as it was")
    checks, capture = run_checks_capturing([], SAMPLE_PASSES)
    expect(probe_runner_diagnostics.FAILURE_MARKER not in capture,
           f"no failure marker appears anywhere in a passing run (got "
           f"{capture!r})")
    expect(probe_runner_diagnostics.failure_records(capture) == [],
           "and therefore no records at all")
    expect(probe_runner_diagnostics.failure_attribution(capture) == [],
           "so the runner's presentation gains nothing for a passing run")
    expect(capture.splitlines()
           == [f"  [PASS] {label}" for label in SAMPLE_PASSES],
           f"the printed verdict lines keep their exact shape, one per "
           f"check and nothing else (got {capture.splitlines()!r})")
    expect(checks.failed == 0, f"and nothing is counted (got {checks.failed})")


def test_a_passing_check_beside_failing_ones_stays_unrecorded() -> None:
    print("\n-- among failures, only the FAILED checks are recorded")
    checks, capture = run_checks_capturing(SAMPLE_FAILURES, SAMPLE_PASSES)
    details = [record.detail
               for record in probe_runner_diagnostics.failure_records(capture)]
    expect(details == SAMPLE_FAILURES,
           f"every failed label, in order, and no passing one (got "
           f"{details!r})")
    for label in SAMPLE_PASSES:
        expect(label not in "\n".join(
                   probe_runner_diagnostics.failure_attribution(capture)),
               f"the passing check {label!r} is named nowhere in the "
               f"failure presentation")
    expect(checks.failed == len(SAMPLE_FAILURES),
           f"the counter still counts only failures (got {checks.failed})")


def test_every_failed_check_survives_outside_the_retained_tail() -> None:
    print("\n-- every failed check is named even when the runner's default "
          "--tail 25 has long since scrolled past all of them")
    _, capture = run_checks_capturing(SAMPLE_FAILURES, SAMPLE_PASSES)
    buried = bury(capture)

    # The precondition: without it this case would pass on a `Checks` that
    # records nothing, purely because the tail still happened to hold the
    # printed [FAIL] lines.
    tail = "\n".join(retained_tail(buried))
    for label in SAMPLE_FAILURES:
        expect(label not in tail,
               f"{label!r} really is outside the retained tail, so only a "
               f"durable record can bring it back")

    got = probe_runner_diagnostics.failure_attribution(buried)
    text = "\n".join(got)
    for label in SAMPLE_FAILURES:
        expect(text.count(label) == 1,
               f"{label!r} is named exactly once by the attribution (got "
               f"{got!r})")
    expect(f"{len(SAMPLE_FAILURES)} recorded failure(s)" in text,
           f"the block counts every one of them (got {got!r})")
    expect(SWEEP_FAILURE_PRODUCER in text,
           f"and attributes them to the sweep (got {got!r})")
    expect("later sweep assertion 0" not in text,
           f"without dumping the capture it read (got {got!r})")


def test_sweep_failure_records_are_not_consumed_by_phase_attribution() -> None:
    print("\n-- the failed-check records and #1768's phase records stay on "
          "separate channels, and neither reader eats the other's")
    emitter = CapturedEmitter()
    for identity in SWEEP_PHASE_IDENTITIES:
        announce_phase(emitter, identity, f"detail for {identity}")
    _, failures = run_checks_capturing(SAMPLE_FAILURES)
    combined = bury("\n".join(emitter.lines) + "\n" + failures)

    progress = probe_runner_diagnostics.progress_attribution(combined)
    progress_text = "\n".join(progress)
    for label in SAMPLE_FAILURES:
        expect(label not in progress_text,
               f"phase attribution does not report {label!r}: 'where was "
               f"it' is not 'what failed' (got {progress!r})")
    expect(SWEEP_PHASE_IDENTITIES[-1] in progress_text,
           f"while still naming the latest phase (got {progress!r})")

    failure_text = "\n".join(
        probe_runner_diagnostics.failure_attribution(combined))
    for identity in SWEEP_PHASE_IDENTITIES:
        expect(f"detail for {identity}" not in failure_text,
               f"and the failure block does not report the {identity!r} "
               f"phase record (got {failure_text!r})")

    # Records-only captures, so neither result can be carried by the other
    # convention's lines happening to be present.
    expect(probe_runner_diagnostics.failure_attribution(
               "\n".join(emitter.lines) + "\n") == [],
           "phase records alone yield no failure attribution")
    expect(probe_runner_diagnostics.progress_attribution(failures) == [],
           "and failed-check records alone yield no phase attribution")


def test_the_failure_records_are_removed_from_the_ordinary_tail() -> None:
    print("\n-- the records are presented in the failure block and taken "
          "out of the tail beside it, never printed twice")
    _, capture = run_checks_capturing(SAMPLE_FAILURES)
    ordinary = probe_runner_diagnostics.without_failure_records(capture)
    expect(probe_runner_diagnostics.FAILURE_MARKER not in ordinary,
           f"no raw record reaches the ordinary tail (got {ordinary!r})")
    expect(ordinary.splitlines()
           == [f"  [FAIL] {label}" for label in SAMPLE_FAILURES],
           f"which keeps exactly the printed verdict lines the sweep "
           f"always had (got {ordinary.splitlines()!r})")


# --------------------------------------------------------------------------
# Opt-in capture and retry plumbing through the REAL main lifecycle (#2805)
# --------------------------------------------------------------------------
def _drive_sweep(argv, *, outcome=codec.COMPARE_OK, fail_slot=None,
                 copy_error=False, result_error=False, marker_error=False,
                 child_exit=0, destination=None, destination_error=False,
                 preparation_error=False, comparison_error=False, save_exit=False,
                 no_report=False, symlink_recovery=False):
    """Fake only engine/codec/child boundaries; preserve main and cleanup.

    The cleanup spy snapshots the export BEFORE deleting the real temporary
    root. Codec comparison is mocked below the real compare_session_files,
    so flag plumbing also proves exactly one report/verdict-producing call.
    """
    with tempfile.TemporaryDirectory(prefix="sweep_test_") as td:
        base = Path(td)
        run = base / "run"
        run.mkdir()
        destination = destination or base / "evidence"
        args = [str(destination) if item == "CAPTURE" else item for item in argv]
        events = []
        generated = {}
        loaded = False
        before_cleanup = {}
        comparison_report = {}
        diagnostic = "" if outcome == codec.COMPARE_OK else (
            "mismatch first line\n" + "many details é\n" * 200 + "last mismatch line\n")

        def boot(root, port, log):
            events.append(("boot", Path(log).name))
            return object()

        def quit(port, proc):
            events.append(("quit",))

        def save(chk, port, page, slot):
            path = run / "root" / "saves" / slot
            path.mkdir()
            for name in ("world.synworld", "world.synworld.prev"):
                payload = b"\x00\xff" + f"{slot}/{name}".encode() + b"\n"
                (path / name).write_bytes(payload)
                generated[f"{slot}/{name}"] = payload
            if symlink_recovery and slot == "gen2":
                recovery = path / "world.synworld.prev"
                recovery.unlink()
                recovery.symlink_to(path / "world.synworld")
                del generated[f"{slot}/world.synworld.prev"]
            (path / "world-synworld-tmp-poison").write_bytes(b"not evidence")
            (path / "world-synworld-stale-poison").write_bytes(b"not evidence")
            (path / "unrelated").symlink_to(Path(sweep.REPO) / "assets")
            events.append(("save", slot))
            if slot == fail_slot:
                if save_exit:
                    raise SystemExit(9)
                raise RuntimeError("stubbed save interruption")

        def send(port, command, **kwargs):
            nonlocal loaded
            if "engine.loadSave" in command:
                loaded = True
                return "true"
            if "world.getActiveWorldId" in command:
                return sweep.PAGE
            if "unit.exists" in command:
                return "true"
            if "world.getTimeScale" in command:
                return "1"
            return "ok"

        def compare(paths):
            events.append(("compare",))
            if comparison_error:
                raise RuntimeError("stubbed comparison interruption")
            comparison_report.update(reference=str(paths[0]),
                                     snapshotDiffers=[] if outcome == codec.COMPARE_OK
                                     else [str(paths[2])], luaComponentDiffers=[],
                                     decodeErrors=[], fullNested={"detail": "full data"})
            return outcome, None if no_report else comparison_report, diagnostic

        real_copy = shutil.copy2
        copied = []

        def copy(source, target):
            events.append(("copy", Path(source).parent.name, Path(source).name))
            if copy_error and copied:
                raise OSError("stubbed second copy failure")
            copied.append(str(source))
            return real_copy(source, target)

        real_write = Path.write_text

        def write(path, text, **kwargs):
            if ((destination_error and path.name == "capture.json") or
                    (result_error and path.name == "comparison.json") or
                    (marker_error and path.name == ".capture.json.tmp")):
                raise OSError("stubbed result/marker write failure")
            return real_write(path, text, **kwargs)

        real_remove = shutil.rmtree

        def remove(path, **kwargs):
            if Path(path) == run:
                events.append(("cleanup",))
                expect(run.exists(), "real run root still exists at cleanup boundary")
                if destination.exists():
                    before_cleanup.update({str(p.relative_to(destination)): p.read_bytes()
                                           for p in destination.rglob("*") if p.is_file()})
                real_remove(path, **kwargs)
                expect(not run.exists(), "normal cleanup removes the isolated run root")
            else:
                real_remove(path, **kwargs)

        child = Mock(return_value=type("Result", (), {"returncode": child_exit})())
        preparation = Mock(side_effect=RuntimeError("stubbed preparation failure")
                           if preparation_error else None)
        boot_mock = Mock(side_effect=boot)
        compare_mock = Mock(side_effect=compare)
        replacements = dict(
            prepare_decoder=preparation, boot_probe=boot_mock,
            build_rich_scenario=lambda *a: (1, 2, 3, 4), save_and_wait=save,
            assert_nondefault_map_mode=lambda *a: None, quit_engine=quit,
            bootstrap_defs=lambda *a: None, load_ai_stack=lambda *a: None,
            send=send, capture_request_id=lambda *a: 123,
            wait_load_published=lambda *a, **kw: (True, "published"),
            page_exists=lambda port, page: page != sweep.GHOST_PAGE or not loaded,
            send_json=lambda *a: {"name": "Sweep Beta World"},
            assert_reset_policy=lambda *a: None, get_attack_target=lambda *a: 3,
            sample_live_state=lambda *a: {"paused": True})
        buffer = io.StringIO()
        error = None
        code = None
        with contextlib.ExitStack() as stack:
            for name, value in replacements.items():
                stack.enter_context(patch.object(sweep, name, value))
            stack.enter_context(patch.object(sweep.tempfile, "mkdtemp", return_value=str(run)))
            stack.enter_context(patch.object(sweep.time, "sleep", return_value=None))
            stack.enter_context(patch.object(snapshot, "compare_session_snapshots", compare_mock))
            stack.enter_context(patch.object(snapshot, "_summary_diff", return_value="summary diff"))
            stack.enter_context(patch.object(sweep.subprocess, "run", child))
            stack.enter_context(patch.object(sweep.shutil, "copy2", side_effect=copy))
            stack.enter_context(patch.object(sweep.shutil, "rmtree", side_effect=remove))
            stack.enter_context(patch.object(Path, "write_text", new=write))
            stack.enter_context(patch.object(sys, "argv", ["persistence_contract_sweep.py", *args]))
            stack.enter_context(contextlib.redirect_stdout(buffer))
            stack.enter_context(contextlib.redirect_stderr(buffer))
            try:
                code = sweep.main()
            except (SystemExit, RuntimeError) as caught:
                error = caught
        after_cleanup = {str(p.relative_to(destination)): p.read_bytes()
                         for p in destination.rglob("*") if p.is_file()}
        return dict(code=code, error=error, text=buffer.getvalue(), events=events,
                    generated=generated, before=before_cleanup, after=after_cleanup,
                    child=child, preparation=preparation, boot=boot_mock,
                    compare=compare_mock, report=comparison_report, diagnostic=diagnostic,
                    destination=destination)


def test_retry_cli_and_default_capture_behavior() -> None:
    print("\n-- default and opt-in retries reach the real child unchanged")
    for flags, retries in (([], 1), (["--cross-probe-retries", "0"], 0),
                           (["--cross-probe-retries", "3"], 3)):
        result = _drive_sweep(flags)
        expect(result["code"] == 0 and result["error"] is None,
               "stubbed main retains successful check/child outcome")
        expect(result["preparation"].call_count == 1 and result["boot"].call_count == 4
               and result["compare"].call_count == 1 and result["child"].call_count == 1,
               "real main reaches every stubbed boundary in the original lifecycle")
        argv = result["child"].call_args.args[0]
        expect(argv == [sys.executable, str(sweep.REPO / "tools" / "run_probes.py"),
                        "--only", ",".join(sweep.DEFAULT_CROSS_REFERENCED_PROBE_KEYS),
                        "--exact", "--jobs", "2", "--retries", str(retries)]
               and result["child"].call_args.kwargs == {"cwd": sweep.REPO},
               "only the requested retry count varies; jobs/keys/exact/cwd are intact")
        expect(f"--retries {retries} (this is slow)" in result["text"],
               "phase diagnostic states the actual retry count")
        expect(result["before"] == result["after"] == {}
               and "generation evidence" not in result["text"]
               and not [e for e in result["events"] if e[0] == "copy"],
               "no capture flag means normal cleanup, no export and no extra output")
        expect(result["events"][-1] == ("cleanup",), "default cleanup remains active")
    for value in ("-1", "nope", "1.5"):
        result = _drive_sweep(["--cross-probe-retries", value])
        expect(isinstance(result["error"], SystemExit) and result["error"].code == 2,
               "invalid retries are CLI errors")
        expect(not result["boot"].called and not result["preparation"].called
               and not result["child"].called, "invalid retries fail before all launches")
    result = _drive_sweep(["--cross-probe-keys", "chop,till", "--cross-probe-jobs", "4",
                           "--cross-probe-retries", "0"], child_exit=7)
    argv = result["child"].call_args.args[0]
    expect(argv[-7:] == ["--only", "chop,till", "--exact", "--jobs", "4", "--retries", "0"]
           and result["code"] == 1 and "exit code 7" in result["text"],
           "selected keys/jobs and nonzero child outcome accounting are unchanged")
    result = _drive_sweep(["--skip-cross-probes", "--cross-probe-retries", "0"])
    expect(result["code"] == 1 and not result["child"].called
           and "coverage is NOT exercised" in result["text"],
           "skipping remains reduced coverage and launches no child")


def test_full_capture_and_same_comparison_verdict() -> None:
    print("\n-- success/mismatch/error capture preserves all bytes and the full single-call result")
    for outcome in (codec.COMPARE_OK, codec.COMPARE_MISMATCH,
                    codec.COMPARE_DECODE_FAILED, codec.COMPARE_ERROR):
        result = _drive_sweep(["--keep-generations", "CAPTURE"], outcome=outcome)
        expect(result["code"] == (0 if outcome == codec.COMPARE_OK else 1),
               "capture never changes the comparison's check outcome")
        expect(result["compare"].call_count == 1, "export never repeats comparison")
        expect(result["before"] == result["after"],
               "all exported evidence already exists before cleanup and survives it")
        for name, payload in result["generated"].items():
            expect(result["after"].get(name) == payload,
                   f"{name} primary/recovery bytes survive exactly")
        expect(set(result["after"]) == set(result["generated"]) | {
            "capture.json", "comparison.json"},
               "only session files and result markers exported; no links/temp/resource trees")
        marker = json.loads(result["after"]["capture.json"])
        expect(marker["complete"] and marker["compared"]
               and marker["missing_generations"] == [], "completed captures are honestly marked")
        exported = json.loads(result["after"]["comparison.json"])
        evidence = exported["result"]
        expect(evidence["outcome"] == outcome and evidence["report"] == result["report"]
               and evidence["diagnostic"] == result["diagnostic"]
               and evidence["ok"] == (outcome == codec.COMPARE_OK),
               "actual outcome, full structured report and raw diagnostic are retained")
        final_detail = result["diagnostic"]
        if outcome == codec.COMPARE_MISMATCH:
            final_detail += "\nfirst structural difference (via canonical summary): summary diff"
        expect(evidence["detail"] == final_detail, "the entire returned mismatch detail is retained")
        for source, target in exported["files"].items():
            relative = str(Path(target).relative_to(result["destination"]))
            expect(result["after"][relative] == result["generated"][relative],
                   "original generation identities map to surviving exported files")
        expect(exported["files"][evidence["report"]["reference"]].endswith("gen1/world.synworld"),
               "reference generation maps to the retained gen1")
        if outcome != codec.COMPARE_OK:
            expect(exported["files"][evidence["report"]["snapshotDiffers"][0]].endswith(
                "gen3/world.synworld"), "divergent generation maps to the retained gen3")
        events = result["events"]
        expect(max(i for i, e in enumerate(events) if e[0] == "quit")
               < min(i for i, e in enumerate(events) if e[0] == "copy")
               < next(i for i, e in enumerate(events) if e[0] == "cleanup"),
               "export follows engine teardown and precedes temporary-root deletion")
        expect(str(result["destination"]) in result["text"], "export location is reported")


def test_partial_interrupted_and_failed_capture_cleanup() -> None:
    print("\n-- partial/interrupted exports never claim complete or compared, and cleanup still runs")
    result = _drive_sweep(["--keep-generations", "CAPTURE"], fail_slot="gen3")
    expect(isinstance(result["error"], RuntimeError) and not result["compare"].called,
           "exception before comparison keeps the original exception and never compares")
    expect(result["before"] == result["after"], "partial export also precedes cleanup")
    marker = json.loads(result["after"]["capture.json"])
    expect(not marker["complete"] and not marker["compared"]
           and marker["missing_generations"] == ["gen4"], "partial capture names missing generation")
    expect(json.loads(result["after"]["comparison.json"])["result"] is None,
           "an unperformed comparison gets no manufactured outcome")
    events = result["events"]
    expect(events[-1] == ("cleanup",)
           and events[events.index(("save", "gen3")) + 1] == ("quit",),
           "live engine is asked to quit before partial capture/cleanup")
    result = _drive_sweep(["--keep-generations", "CAPTURE"], fail_slot="gen2", save_exit=True)
    marker = json.loads(result["after"]["capture.json"])
    expect(isinstance(result["error"], SystemExit) and result["error"].code == 9
           and not marker["complete"] and not marker["compared"]
           and marker["missing_generations"] == ["gen3", "gen4"]
           and result["events"][-1] == ("cleanup",),
           "SystemExit retains only produced generations, preserves exit status and cleans up")
    result = _drive_sweep(["--keep-generations", "CAPTURE"], comparison_error=True)
    marker = json.loads(result["after"]["capture.json"])
    expect(isinstance(result["error"], RuntimeError) and result["compare"].call_count == 1
           and not marker["complete"] and not marker["compared"]
           and marker["missing_generations"] == []
           and json.loads(result["after"]["comparison.json"])["result"] is None,
           "all files without a completed comparison remain incomplete/not compared")
    result = _drive_sweep(["--keep-generations", "CAPTURE"], preparation_error=True)
    marker = json.loads(result["after"]["capture.json"])
    expect(isinstance(result["error"], RuntimeError) and not result["boot"].called
           and result["preparation"].called and result["events"][-1] == ("cleanup",),
           "preparation exception still captures available evidence and cleans up before any boot")
    expect(not marker["complete"] and not marker["compared"]
           and marker["missing_generations"] == ["gen1", "gen2", "gen3", "gen4"],
           "preparation failure honestly labels an empty export")
    result = _drive_sweep(["--keep-generations", "CAPTURE"], fail_slot="gen3", copy_error=True)
    expect(isinstance(result["error"], RuntimeError) and "capture failed" in result["text"]
           and result["events"][result["events"].index(("save", "gen3")) + 1] == ("quit",)
           and result["events"][-1] == ("cleanup",),
           "capture failure on an interrupted live-engine path preserves teardown and cleanup")
    for failure in ("copy_error", "result_error", "marker_error"):
        result = _drive_sweep(["--keep-generations", "CAPTURE"], **{failure: True})
        marker = json.loads(result["after"]["capture.json"])
        expect(result["code"] == 1 and "capture failed" in result["text"]
               and "exported to" not in result["text"], "capture failure is visible, never a retention claim")
        expect(not marker["complete"] and not marker["compared"],
               "an interrupted copy/result/final-marker write retains the initial incomplete marker")
        expect(result["events"][-1] == ("cleanup",) and result["before"] == result["after"],
               "capture failure cannot prevent teardown or cleanup")

    result = _drive_sweep(["--keep-generations", "CAPTURE"],
                          outcome=codec.COMPARE_ERROR, no_report=True)
    evidence = json.loads(result["after"]["comparison.json"])["result"]
    expect(result["code"] == 1 and evidence["report"] is None
           and evidence["outcome"] == codec.COMPARE_ERROR
           and evidence["diagnostic"] == result["diagnostic"],
           "comparison error without a report preserves the real error and absent report")
    result = _drive_sweep(["--keep-generations", "CAPTURE"], symlink_recovery=True)
    expect(result["code"] == 0 and "gen2/world.synworld.prev" not in result["after"]
           and json.loads(result["after"]["capture.json"])["complete"],
           "a symlinked recovery file is never followed or copied")


def test_destination_hygiene_and_help() -> None:
    print("\n-- invalid/reused destinations fail before boot; help describes both defaults")
    with tempfile.TemporaryDirectory() as td:
        base = Path(td)
        file = base / "file"
        file.write_bytes(b"occupied")
        occupied = base / "occupied"
        occupied.mkdir()
        (occupied / "gen4").write_bytes(b"old run")
        link = base / "link"
        link.symlink_to(occupied, target_is_directory=True)
        for path in (file, occupied, link):
            result = _drive_sweep(["--keep-generations", "CAPTURE"], destination=path)
            expect(isinstance(result["error"], SystemExit) and result["error"].code == 2,
                   "invalid/reused destination is a CLI error")
            expect(not result["boot"].called and not result["preparation"].called
                   and not result["child"].called, "unusable destination refused before any launches")
        expect(file.read_bytes() == b"occupied" and (occupied / "gen4").read_bytes() == b"old run",
               "rejection preserves existing destination contents")
        empty = base / "empty"
        empty.mkdir()
        result = _drive_sweep(["--keep-generations", "CAPTURE"], destination=empty)
        expect(result["code"] == 0, "an existing empty directory is usable")
    result = _drive_sweep(["--keep-generations", "CAPTURE"], destination_error=True)
    expect(isinstance(result["error"], SystemExit) and result["error"].code == 2
           and not result["boot"].called and not result["preparation"].called
           and not result["child"].called, "destination write failure is rejected before launches")
    result = _drive_sweep(["--help"])
    expect(isinstance(result["error"], SystemExit) and result["error"].code == 0
           and "--cross-probe-retries" in result["text"] and "default: 1" in result["text"]
           and "--keep-generations" in result["text"] and "default: no export" in result["text"],
           "CLI help states both opt-in controls and defaults")


def main() -> int:
    selftestlib.parse_verbose()
    test_todays_selectable_keys_are_all_registered()
    test_a_key_the_registry_drops_is_caught()
    test_every_stale_key_is_named_at_once()
    test_empty_selectable_list_has_nothing_stale()
    test_every_required_sweep_phase_is_declared()
    test_an_undeclared_phase_is_refused_rather_than_emitted()
    test_every_declared_phase_emits_a_record_the_runner_recognizes()
    test_the_latest_sweep_phase_survives_a_long_tail()
    test_the_producer_identity_names_this_script()
    test_a_failed_sweep_check_records_its_own_label()
    test_a_passing_sweep_check_emits_no_failure_marker()
    test_a_passing_check_beside_failing_ones_stays_unrecorded()
    test_every_failed_check_survives_outside_the_retained_tail()
    test_sweep_failure_records_are_not_consumed_by_phase_attribution()
    test_the_failure_records_are_removed_from_the_ordinary_tail()
    test_retry_cli_and_default_capture_behavior()
    test_full_capture_and_same_comparison_verdict()
    test_partial_interrupted_and_failed_capture_cleanup()
    test_destination_hygiene_and_help()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nAll persistence_contract_sweep registry-drift, phase-record, "
        "failed-check-record and opt-in capture/retry tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
