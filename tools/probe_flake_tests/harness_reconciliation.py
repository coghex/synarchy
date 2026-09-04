#!/usr/bin/env python3
"""Run reconciliation and the harness's own error shapes (#2087).

What the harness makes of a completed run: a clean pass, a failed check, a
nonzero exit with partial checks, an abort, a timeout and a malformed
stream.
"""
from __future__ import annotations

import os

from .support import probe_flake
from .support import SyntheticTree, run_synthetic, expect
from .support import synthetic_descriptor as _descriptor
from .support import event_line as _line

def test_reconciliation() -> None:
    print("\n-- run reconciliation --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "pass", runs=2)
        expect(m.valid and [r.outcome for r in m.runs] == ["PASS", "PASS"],
               "a complete passing run reconciles to PASS")
        expect(all(v == "PASS" for r in m.runs for v in r.checks.values()),
               "every declared check passes in a complete passing run")
        expect(m.failure_count == 0 and m.failure_rate == 0.0,
               "a clean measurement reports a 0% failure rate")

        m = run_synthetic(tree, "fail", runs=1)
        expect([r.outcome for r in m.runs] == ["FAIL"],
               "a parsed check failure makes the run FAIL")
        expect(m.runs[0].checks == {"alpha": "PASS", "beta": "FAIL",
                                    "gamma": "PASS"},
               "the failing check is the only one recorded FAIL")

        m = run_synthetic(tree, "nonzero_partial", runs=1)
        expect([r.outcome for r in m.runs] == ["FAIL"],
               "a nonzero exit with only passing checks still FAILs the run")
        expect(m.runs[0].checks["gamma"] == "MISSING",
               "the check a nonzero exit prevented is MISSING")

        m = run_synthetic(tree, "abort", runs=1)
        expect([r.outcome for r in m.runs] == ["FAIL"],
               "an early setup abort makes the run FAIL")
        expect(m.runs[0].checks == {"alpha": "PASS", "beta": "MISSING",
                                    "gamma": "MISSING"},
               "an early abort leaves its remaining checks MISSING")

        m = run_synthetic(tree, "diagnostics", runs=1)
        expect([r.outcome for r in m.runs] == ["PASS"],
               "INFO/WARN/SKIP diagnostics never make a run fail")

        m = run_synthetic(tree, "brackets_ok", runs=1)
        expect(m.valid and [r.outcome for r in m.runs] == ["PASS"],
               "bracketed DATA on stdout is not a marker and passes")

        m = run_synthetic(tree, "hang", runs=1, timeout=3.0)
        expect([r.outcome for r in m.runs] == ["TIMEOUT"],
           "a hung probe reconciles to TIMEOUT")
        expect(m.runs[0].checks == {"alpha": "PASS", "beta": "PASS",
                                    "gamma": "MISSING"},
               "partial checks emitted before a timeout are kept, the rest MISSING")
        expect(m.timeout_count == 1 and m.failure_count == 1,
               "a timeout counts toward the failure rate and stays separately visible")

        # TIMEOUT wins over a FAIL check in the same run.
        raw = tree.root / "timeout_fail.jsonl"
        raw.write_text(_line(event="check", id="alpha", outcome="FAIL"),
                       encoding="utf-8")
        outcome, outcomes = probe_flake.reconcile(
            _descriptor(), ok=False, timed_out=True,
            events_text=raw.read_text(), stdout_text="")
        expect(outcome == "TIMEOUT" and outcomes["alpha"] == "FAIL",
               "TIMEOUT takes precedence over a failed check in the same run")


def test_harness_errors() -> None:
    print("\n-- harness errors --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "rawbytes", runs=1)
        expect(not m.valid and m.status == "harness-error",
               "an event stream of invalid UTF-8 is a harness error")
        expect(m.failure_rate is None and m.error_run is not None
               and m.error_run.artifact_dir is not None
               and m.error_run.artifact_dir.exists(),
               "and its run is retained and reported, with no rate")
        expect("UnicodeDecodeError" in (m.error or ""),
               f"the harness error names the decoding failure ({m.error})")

        m = run_synthetic(tree, "marker", runs=1)
        expect(not m.valid and m.status == "harness-error",
               "a forbidden bracketed stdout marker is a harness error")
        expect(m.failure_rate is None,
               "a harness error reports no failure rate at all")
        expect("bracketed stdout markers" in (m.error or ""),
               "the harness error names the second result channel")
        expect(m.error_run is not None
               and m.error_run.checks.get("alpha") == "PASS"
               and m.error_run.checks.get("beta") == "MISSING",
               "a marker error still reports the valid partial check data "
               "its stream did parse")
        text = probe_flake.render(m)
        expect("every run succeeded" not in text
               and str(m.error_run.artifact_dir) in text,
               "the table names the retained artifacts instead of claiming "
               "success")

        raws = {
            "truncated": '{"event": "check", "id": "alpha"',
            "malformed": "this is not json\n",
            "prefix_then_malformed": (
                _line(event="check", id="alpha", outcome="PASS") +
                "this is not json\n"),
            "prefix_then_truncated": (
                _line(event="check", id="alpha", outcome="PASS") +
                '{"event": "check", "id": "be'),
            "duplicate": (_line(event="check", id="alpha", outcome="PASS") +
                          _line(event="check", id="alpha", outcome="PASS")),
            "unexpected": _line(event="check", id="delta", outcome="PASS"),
            "outoforder": _line(event="check", id="gamma", outcome="PASS"),
            "unclassifiable": _line(event="mystery", id="alpha"),
        }
        for name, body in raws.items():
            path = tree.root / f"raw-{name}.jsonl"
            path.write_text(body, encoding="utf-8")
            os.environ["SYNTHETIC_RAW_PATH"] = str(path)
            try:
                m = run_synthetic(tree, "raw", runs=2)
            finally:
                os.environ.pop("SYNTHETIC_RAW_PATH", None)
            expect(not m.valid and m.status == "harness-error",
                   f"a {name} protocol stream is a harness error, not a probe PASS")
            expect(m.failure_rate is None,
                   f"a {name} stream yields no flake rate")
            expect(len(m.runs) < 2,
                   f"a {name} stream stops the measurement rather than continuing")
            expect(m.error_run is not None,
                   f"a {name} stream still REPORTS the run it discarded")
            expect(m.error_run is not None and m.retained_artifacts()
                   and m.error_run.artifact_dir is not None
                   and m.error_run.artifact_dir.exists(),
                   f"a {name} run's retained artifacts are named in the result")
            expect(m.error_run is not None
                   and m.error_run.outcome == "HARNESS_ERROR"
                   and m.error_run not in m.runs,
                   f"a {name} run is not counted as a probe outcome")
            expect("every run succeeded" not in probe_flake.render(m),
                   f"the {name} table never claims every run succeeded")
            if name.startswith("prefix_then_"):
                expect(m.error_run is not None
                       and m.error_run.checks.get("alpha") == "PASS"
                       and m.error_run.checks.get("beta") == "MISSING",
                       f"a {name} stream still reports the trusted prefix it "
                       f"parsed before the fault")


TESTS = (
    test_reconciliation,
    test_harness_errors,
)
