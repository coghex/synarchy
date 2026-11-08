#!/usr/bin/env python3
"""Self-test for `tools/probe_cost_table.py` and the tracked `docs/probe_costs.json` (#2947).

Engine-free. A small synthetic ledger (sqlite, the lab's `runs`, `trials` and
`results` tables) drives the generator: latest-trial selection and its
tie-break, the count/minimum/median/maximum, nulls for an unrun probe, key
order, failed and timed-out trials, a failed check under a clean exit,
exclusions (pending, blocked, harness-error, crashed, bad durations, unknown
suites), shakedown trials that name their own suite, byte-identical
regeneration, and the failure paths that must leave the tracked table alone.

The registry-consistency test fails when a registered probe has no row in the
tracked table, or the table names an unregistered one, so adding or retiring
a probe forces `docs/probe_costs.json` to follow (refresh command:
`tools/README.md` "Probe cost table").

Usage:
  python3 tools/test_probe_cost_table.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import hashlib
import io
import json
import os
import sqlite3
import sys
import tempfile
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))

import probe_cost_table as pct  # noqa: E402
import probe_runner_registry  # noqa: E402
import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

LEDGER_SCHEMA = """
CREATE TABLE runs (
    id TEXT PRIMARY KEY, lane TEXT NOT NULL, suite TEXT, revision TEXT NOT NULL,
    source_ref TEXT NOT NULL, upstream INTEGER NOT NULL, started TEXT NOT NULL,
    heartbeat TEXT NOT NULL, state TEXT NOT NULL, finished_epoch REAL,
    document TEXT NOT NULL, summary TEXT);
CREATE TABLE trials (
    run_id TEXT NOT NULL REFERENCES runs(id), number INTEGER NOT NULL,
    state TEXT NOT NULL, document TEXT NOT NULL, PRIMARY KEY(run_id, number));
CREATE TABLE results (
    run_id TEXT NOT NULL REFERENCES runs(id), number INTEGER NOT NULL,
    test_id TEXT NOT NULL, outcome TEXT NOT NULL, PRIMARY KEY(run_id, number, test_id));
"""


def make_ledger(path: Path, runs, trials, results=()) -> Path:
    """`runs`: (id, suite, revision); `trials`: (run, number, state, document dict);
    `results`: (run, number, test_id, outcome)."""
    db = sqlite3.connect(path)
    db.executescript(LEDGER_SCHEMA)
    for run_id, suite, revision in runs:
        db.execute("INSERT INTO runs VALUES(?,?,?,?,?,?,?,?,?,?,?,?)",
                   (run_id, "flake", suite, revision, "ref", 0, "2026-01-01T00:00:00+00:00",
                    "2026-01-01T00:00:00+00:00", "complete", 1.0, "{}", None))
    for run_id, number, state, document in trials:
        db.execute("INSERT INTO trials VALUES(?,?,?,?)", (run_id, number, state, json.dumps(document)))
    for row in results:
        db.execute("INSERT INTO results VALUES(?,?,?,?)", row)
    db.commit()
    db.close()
    return path


def trial(seconds, finished, started=None, **extra):
    return dict(duration_seconds=seconds, finished=finished, started=started or finished, **extra)


def rows_for(ledger: Path, keys):
    return pct.build_table(pct.read_trials(ledger), list(keys))["probes"]


def run_main(*argv):
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        status = pct.main(list(argv))
    return status, out.getvalue(), err.getvalue()


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def test_selection_and_statistics(tmp: Path) -> None:
    ledger = make_ledger(
        tmp / "stats.sqlite3",
        [("run-a", "probe:blood-impact", "aaaaaaaaaaaaaaaaaaaa"), ("run-b", "probe:blood-impact", "bbbbbbbbbbbbbbbbbbbb"),
         ("run-c", "probe:blood-impact", "cccccccccccccccccccc")],
        [("run-a", 1, "passed", trial(10.0, "2026-03-01T10:00:00+00:00")),
         ("run-b", 1, "passed", trial(40.0, "2026-03-03T10:00:00+00:00")),
         ("run-b", 2, "passed", trial(20.0, "2026-03-03T09:00:00+00:00")),
         ("run-c", 1, "passed", trial(30.0, "2026-03-02T10:00:00+00:00"))])
    row = rows_for(ledger, ["blood_impact", "never_run_probe"])["blood_impact"]
    expect(row["wall_seconds"] == 40.0 and row["run_id"] == "run-b",
           f"latest = most recently finished trial, not the largest id or number: {row}")
    expect(row["revision"] == "bbbbbbbbbbbb", f"revision is the run's, shortened to 12: {row['revision']}")
    expect(row["date"] == "2026-03-03", f"date is the trial's finish date: {row['date']}")
    expect((row["count"], row["min_seconds"], row["median_seconds"], row["max_seconds"]) == (4, 10.0, 25.0, 40.0),
           f"count/min/even-count median/max: {row}")
    expect(row["state"] == "passed", "a clean trial is passed")


def test_tiebreak_is_deterministic(tmp: Path) -> None:
    same = "2026-03-01T10:00:00+00:00"
    ledger = make_ledger(
        tmp / "tie.sqlite3",
        [("run-1", "probe:wire", "1" * 40), ("run-2", "probe:wire", "2" * 40)],
        [("run-1", 1, "passed", trial(1.0, same, "2026-03-01T09:59:00+00:00")),
         ("run-1", 2, "passed", trial(2.0, same, "2026-03-01T09:59:30+00:00")),
         ("run-2", 1, "passed", trial(3.0, same, "2026-03-01T09:59:30+00:00"))])
    row = rows_for(ledger, ["wire"])["wire"]
    expect(row["wall_seconds"] == 3.0 and row["run_id"] == "run-2",
           f"finished, then started, then run id, then number: {row}")
    # The same instant written with another UTC offset is the same instant.
    ledger = make_ledger(
        tmp / "offset.sqlite3", [("r", "probe:wire", "9" * 40)],
        [("r", 1, "passed", trial(1.0, "2026-03-01T12:00:00+02:00")),
         ("r", 2, "passed", trial(2.0, "2026-03-01T10:30:00+00:00"))])
    expect(rows_for(ledger, ["wire"])["wire"]["wall_seconds"] == 2.0, "offsets compare as instants")


def test_unrun_probe_has_explicit_nulls(tmp: Path) -> None:
    ledger = make_ledger(tmp / "unrun.sqlite3", [("r", "probe:wire", "a" * 40)],
                         [("r", 1, "passed", trial(1.0, "2026-03-01T10:00:00+00:00"))])
    rows = rows_for(ledger, ["wire", "audio_manual"])
    expect(rows["audio_manual"] == dict(
        wall_seconds=None, state=None, revision=None, run_id=None, date=None,
        count=0, min_seconds=None, median_seconds=None, max_seconds=None),
        f"an unrun probe is all nulls with count 0: {rows['audio_manual']}")
    expect(list(rows) == ["wire", "audio_manual"], "every requested key has a row")


def test_underscored_key_maps_to_hyphenated_suite(tmp: Path) -> None:
    expect(pct.suite_id("follow_command_priority") == "probe:follow-command-priority", "suite id convention")
    ledger = make_ledger(tmp / "under.sqlite3", [("r", "probe:follow-command-priority", "a" * 40)],
                         [("r", 1, "passed", trial(7.0, "2026-03-01T10:00:00+00:00"))])
    rows = rows_for(ledger, ["follow_command_priority", "follow"])
    expect(rows["follow_command_priority"]["count"] == 1, "an underscored key finds its hyphenated suite")
    expect(rows["follow"]["count"] == 0, "a key does not match a longer suite by prefix")


def test_states(tmp: Path) -> None:
    ledger = make_ledger(
        tmp / "states.sqlite3",
        [("fail", "probe:a-probe", "f" * 40), ("slow", "probe:b-probe", "5" * 40),
         ("quiet", "probe:c-probe", "c" * 40), ("fast-timeout", "probe:d-probe", "d" * 40)],
        [("fail", 1, "failed", trial(5.0, "2026-03-01T10:00:00+00:00", returncode=1)),
         ("slow", 1, "timeout", trial(1.5, "2026-03-01T10:00:00+00:00")),
         ("quiet", 1, "passed", trial(6.0, "2026-03-01T10:00:00+00:00", returncode=0)),
         ("fast-timeout", 1, "passed", trial(9999.0, "2026-03-01T10:00:00+00:00"))],
        [("quiet", 1, "probe:c-probe::check", "failed"), ("quiet", 1, "probe:c-probe::other", "passed")])
    rows = rows_for(ledger, ["a_probe", "b_probe", "c_probe", "d_probe"])
    expect(rows["a_probe"]["state"] == "failed", "a failed trial is failed")
    expect(rows["b_probe"]["state"] == "timeout" and rows["b_probe"]["wall_seconds"] == 1.5,
           "a recorded timeout is reported as one, whatever its elapsed time")
    expect(rows["c_probe"]["state"] == "failed", "a failed check under a clean exit is failed")
    expect(rows["d_probe"]["state"] == "passed", "a long passing trial is never inferred to be a timeout")
    ledger = make_ledger(tmp / "missing.sqlite3", [("r", "probe:a-probe", "a" * 40)],
                         [("r", 1, "passed", trial(1.0, "2026-03-01T10:00:00+00:00"))],
                         [("r", 1, "probe:a-probe::x", "missing")])
    expect(rows_for(ledger, ["a_probe"])["a_probe"]["state"] == "failed", "a missing check is failed")
    ledger = make_ledger(tmp / "incomplete.sqlite3", [("r", "probe:a-probe", "a" * 40)],
                         [("r", 1, "passed", trial(1.0, "2026-03-01T10:00:00+00:00"))],
                         [("r", 1, "probe:a-probe::x", "incomplete")])
    expect(rows_for(ledger, ["a_probe"])["a_probe"]["state"] == "passed",
           "an unproven check does not turn a clean trial into a fabricated failure")


def test_exclusions(tmp: Path) -> None:
    good = trial(10.0, "2026-03-01T10:00:00+00:00")
    later = "2026-04-01T10:00:00+00:00"
    ledger = make_ledger(
        tmp / "excluded.sqlite3",
        [("r", "probe:wire", "a" * 40), ("other", "probe:not-registered", "b" * 40),
         ("shake", None, "c" * 40)],
        [("r", 1, "passed", good),
         ("r", 2, "running", trial(99.0, later)),
         ("r", 3, "harness-error", trial(98.0, later, error="x")),
         ("r", 4, "crashed", trial(97.0, later)),
         ("r", 5, "interrupted", trial(96.0, later)),
         ("r", 6, "pending", trial(95.0, later)),
         ("r", 7, "passed", trial(-1.0, later)),
         ("r", 8, "passed", trial(float("nan"), later)),
         ("r", 9, "passed", trial(float("inf"), later)),
         ("r", 10, "passed", trial("12", later)),
         ("r", 11, "passed", trial(True, later)),
         ("r", 12, "passed", dict(finished=later)),
         ("r", 13, "passed", dict(duration_seconds=3.0)),
         ("r", 14, "passed", dict(duration_seconds=3.0, finished="not a time")),
         ("r", 15, "passed", dict(duration_seconds=3.0, finished="2026-04-01T10:00:00")),
         ("other", 1, "passed", trial(1000.0, later)),
         ("shake", 1, "passed", trial(20.0, "2026-02-01T10:00:00+00:00", suite="probe:wire")),
         ("shake", 2, "passed", trial(21.0, "2026-02-02T10:00:00+00:00", suite="probe:also-unknown")),
         ("shake", 3, "passed", trial(22.0, "2026-02-03T10:00:00+00:00", suite="synarchy-test-headless"))])
    row = rows_for(ledger, ["wire"])["wire"]
    expect(row["count"] == 2, f"only passed/failed/timeout trials with finite durations count: {row}")
    expect((row["min_seconds"], row["max_seconds"]) == (10.0, 20.0), f"excluded trials leave no trace: {row}")
    expect(row["wall_seconds"] == 10.0 and row["run_id"] == "r",
           f"latest is among samples only, not a later blocked or invalid attempt: {row}")
    expect(set(pct.build_table(pct.read_trials(ledger), ["wire"])["probes"]) == {"wire"},
           "suites the registry does not know are ignored")
    ledger = make_ledger(tmp / "onlybad.sqlite3", [("r", "probe:wire", "a" * 40)],
                         [("r", 1, "harness-error", trial(1.0, later))])
    expect(rows_for(ledger, ["wire"])["wire"]["count"] == 0, "an empty population has count 0 and nulls")


def test_output_shape_and_order(tmp: Path) -> None:
    ledger = make_ledger(tmp / "shape.sqlite3", [("r", "probe:wire", "a" * 40)],
                         [("r", 1, "passed", trial(1.2345678, "2026-03-01T10:00:00+00:00"))])
    table = pct.build_table(pct.read_trials(ledger), ["zebra", "wire", "alpha"])
    text = pct.render(table)
    expect(table["schema"] == "probe-costs/v1", "schema name")
    expect(text.endswith("}\n") and json.loads(text) == table, "valid JSON with a trailing newline")
    keys = list(json.loads(text)["probes"])
    expect(keys == sorted(keys), f"probe keys are sorted in the file: {keys}")
    expect(table["probes"]["wire"]["wall_seconds"] == 1.235, "seconds are rounded to milliseconds")
    for phrase in ("wall-clock", "engine boot", "not a CI timing", "serially"):
        expect(phrase in table["description"], f"the header states {phrase!r}")


def test_regeneration_is_byte_identical(tmp: Path) -> None:
    ledger = make_ledger(tmp / "twice.sqlite3", [("r", "probe:wire", "a" * 40)],
                         [("r", 1, "passed", trial(1.0, "2026-03-01T10:00:00+00:00"))])
    first, second = tmp / "first.json", tmp / "second.json"
    expect(run_main("--ledger", str(ledger), "--output", str(first))[0] == 0, "first run succeeds")
    expect(run_main("--ledger", str(ledger), "--output", str(second))[0] == 0, "second run succeeds")
    expect(first.read_bytes() == second.read_bytes(), "two runs against one ledger are byte-identical")
    text = first.read_text()
    expect(str(tmp) not in text and "generated" not in json.loads(text), "no path or generation timestamp is embedded")
    status, out, _ = run_main("--ledger", str(ledger), "--stdout")
    expect(status == 0 and out.encode() == first.read_bytes(), "--stdout prints the file's exact bytes")
    environment = os.environ.get(pct.LEDGER_ENV)
    os.environ[pct.LEDGER_ENV] = str(ledger)
    try:
        expect(pct.default_ledger() == ledger, "the environment variable overrides the git-derived path")
    finally:
        if environment is None:
            del os.environ[pct.LEDGER_ENV]
        else:
            os.environ[pct.LEDGER_ENV] = environment


def test_failure_paths_leave_the_table_alone(tmp: Path) -> None:
    tracked = tmp / "tracked.json"
    tracked.write_text('{"keep": "me"}\n')
    before = digest(tracked)

    def refused(label: str, ledger: Path) -> None:
        status, _out, err = run_main("--ledger", str(ledger), "--output", str(tracked))
        expect(status == 2 and "probe_cost_table:" in err, f"{label}: clear failure (status {status}): {err!r}")
        expect(digest(tracked) == before, f"{label}: the tracked table is untouched")
        expect(not (tmp / "tracked.json.tmp").exists(), f"{label}: no temporary file is left")

    absent = tmp / "absent.sqlite3"
    refused("a missing ledger", absent)
    expect(not absent.exists(), "a missing ledger is not created")
    garbage = tmp / "garbage.sqlite3"
    garbage.write_bytes(b"this is not a database" * 100)
    refused("an unreadable ledger", garbage)
    bare = tmp / "bare.sqlite3"
    sqlite3.connect(bare).close()
    refused("an empty ledger", bare)
    narrow = tmp / "narrow.sqlite3"
    db = sqlite3.connect(narrow)
    db.executescript("CREATE TABLE runs(id TEXT, suite TEXT); CREATE TABLE trials(run_id TEXT, number INTEGER);"
                     "CREATE TABLE results(run_id TEXT, number INTEGER, outcome TEXT);")
    db.commit()
    db.close()
    refused("an incompatible ledger", narrow)
    broken = make_ledger(tmp / "broken.sqlite3", [("r", "probe:wire", "a" * 40)], [])
    db = sqlite3.connect(broken)
    db.execute("INSERT INTO trials VALUES('r', 1, 'passed', 'not json')")
    db.commit()
    db.close()
    refused("a trial with an unreadable document", broken)


def test_ledger_is_only_read(tmp: Path) -> None:
    ledger = make_ledger(tmp / "readonly.sqlite3", [("r", "probe:wire", "a" * 40)],
                         [("r", 1, "passed", trial(1.0, "2026-03-01T10:00:00+00:00"))])
    folder = ledger.parent
    names, before = sorted(p.name for p in folder.iterdir()), digest(ledger)
    expect(run_main("--ledger", str(ledger), "--output", str(tmp / "ro-out.json"))[0] == 0, "read succeeds")
    after = sorted(p.name for p in folder.iterdir())
    expect(digest(ledger) == before, "the ledger's bytes are unchanged")
    expect(after == sorted(names + ["ro-out.json"]), f"no file is created beside the ledger: {after}")


def test_registry_consistency() -> None:
    path = pct.DEFAULT_OUTPUT
    if not expect(path.is_file(), f"{path} is tracked"):
        return
    document = json.loads(path.read_text(encoding="utf-8"))
    expect(document.get("schema") == pct.SCHEMA, "the tracked table has the probe-costs/v1 schema")
    registered, named = set(pct.registry_keys()), set(document.get("probes", {}))
    expect(not registered - named,
           f"registered probes without a row (run tools/probe_cost_table.py): {sorted(registered - named)}")
    expect(not named - registered,
           f"rows for probes the registry no longer has (run tools/probe_cost_table.py): {sorted(named - registered)}")
    fields = set(pct.build_table([], ["x"])["probes"]["x"])
    for key, row in document.get("probes", {}).items():
        expect(set(row) == fields, f"{key}: row fields are exactly {sorted(fields)}")
        expect(row["count"] >= 0 and (row["count"] == 0) == (row["wall_seconds"] is None),
               f"{key}: a null latest sample and a zero count go together")
    expect(list(document.get("probes", {})) == sorted(document.get("probes", {})), "rows are in key order")
    expect(probe_runner_registry.PROBES, "the registry is not empty")


def main() -> int:
    selftestlib.parse_verbose()
    with tempfile.TemporaryDirectory(prefix="probe-cost-table-") as root:
        for index, case in enumerate((
                test_selection_and_statistics, test_tiebreak_is_deterministic, test_unrun_probe_has_explicit_nulls,
                test_underscored_key_maps_to_hyphenated_suite, test_states, test_exclusions,
                test_output_shape_and_order, test_regeneration_is_byte_identical,
                test_failure_paths_leave_the_table_alone, test_ledger_is_only_read)):
            folder = Path(root) / f"case-{index}"
            folder.mkdir()
            case(folder)
    test_registry_consistency()
    if FAILURES:
        print(f"\n{len(FAILURES)} failure(s):")
        for failure in FAILURES:
            print(f"  FAIL: {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "All probe cost table tests passed")


if __name__ == "__main__":
    sys.exit(main())
