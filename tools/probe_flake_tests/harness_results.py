#!/usr/bin/env python3
"""Result-document serialization, exit codes and rendering (#2087).

The third mutation-covered parser, and the human table the harness prints
above it.
"""
from __future__ import annotations

import json
import os

from .support import probe_flake
from .support import SyntheticTree, run_synthetic, expect

def test_result_document() -> None:
    print("\n-- probe-flake-result/v1 --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "fail", runs=3)
        out = tree.root / "result.json"
        probe_flake.write_result(m, str(out))
        doc = json.loads(out.read_text(encoding="utf-8"))

        required = {"schema", "probe", "status", "error", "requested_runs",
                    "completed_runs", "runs", "checks", "check_counts",
                    "failure_count", "failure_rate", "timeout_count",
                    "worst_elapsed_seconds", "timestamp_utc", "commit_sha",
                    "rts_capabilities", "peak_concurrency",
                    "retained_artifacts", "artifact_root", "invocation_dir"}
        missing = required - set(doc)
        expect(not missing, f"the result document carries every required field "
                            f"(missing: {sorted(missing)})")
        expect(doc["schema"] == "probe-flake-result/v1",
               "the result document is versioned")
        expect(doc["requested_runs"] == 3 and doc["completed_runs"] == 3,
               "requested and completed run counts are both reported")
        expect(len(doc["runs"]) == 3,
               "the complete valid per-run outcome list has one entry per run")
        expect(all("elapsed_seconds" in r and "checks" in r
                   for r in doc["runs"]),
               "each run reports its elapsed duration and check outcomes")
        expect(doc["rts_capabilities"] == 4,
               "the effective RTS capability count is recorded")
        expect(doc["failure_count"] == 3 and doc["failure_rate"] == 1.0,
               "the aggregate failure rate counts every failing run")

        for cid, counts in doc["check_counts"].items():
            total = sum(counts.values())
            expect(total == doc["requested_runs"],
                   f"check {cid}: PASS+FAIL+MISSING == requested runs ({total})")

        # Timeouts ride in the failure numerator while staying visible.
        m = run_synthetic(tree, "hang", runs=1, timeout=3.0)
        doc = m.to_document()
        expect(doc["timeout_count"] == 1 and doc["failure_count"] == 1
               and doc["failure_rate"] == 1.0,
               "a timeout is in the failure numerator and separately visible")
        expect(doc["worst_elapsed_seconds"] >= 0.0,
               "the worst elapsed duration is reported")

        # A harness error keeps the valid partial data but no rate.
        os.environ["SYNTHETIC_RAW_PATH"] = str(tree.root / "bad2.jsonl")
        (tree.root / "bad2.jsonl").write_text("nope\n", encoding="utf-8")
        try:
            m = run_synthetic(tree, "raw", runs=4)
        finally:
            os.environ.pop("SYNTHETIC_RAW_PATH", None)
        doc = m.to_document()
        expect(doc["status"] == "harness-error" and doc["failure_rate"] is None,
               "an invalid measurement declares its status and reports no rate")
        expect(isinstance(doc["error"], str) and doc["error"],
               "an invalid measurement carries error detail")


def test_exit_codes() -> None:
    print("\n-- harness exit codes --")
    with SyntheticTree() as tree:
        # A valid measurement exits 0 whatever it observed. Driving
        # main() needs the module state the SyntheticTree installed, so
        # call it in-process rather than as a subprocess.
        for mode, label in (("pass", "0% observed"), ("fail", "100% observed")):
            previous = os.environ.get("SYNTHETIC_MODE")
            os.environ["SYNTHETIC_MODE"] = mode
            try:
                rc = probe_flake.main(["--probe", "synthetic", "--runs", "1",
                                       "--artifact-root", str(tree.artifacts())])
            finally:
                if previous is None:
                    os.environ.pop("SYNTHETIC_MODE", None)
                else:
                    os.environ["SYNTHETIC_MODE"] = previous
            expect(rc == 0, f"a valid measurement exits 0 ({label})")

        rc = probe_flake.main(["--probe", "nosuchprobe", "--runs", "1",
                               "--artifact-root", str(tree.artifacts())])
        expect(rc == probe_flake.EXIT_REJECTED,
               "a pre-execution rejection exits nonzero")

        os.environ["SYNTHETIC_MODE"] = "marker"
        try:
            rc = probe_flake.main(["--probe", "synthetic", "--runs", "1",
                                   "--artifact-root", str(tree.artifacts())])
        finally:
            os.environ.pop("SYNTHETIC_MODE", None)
        expect(rc == probe_flake.EXIT_HARNESS_ERROR,
               "a harness error exits nonzero")

        # Undecodable event bytes are malformed protocol input, so they
        # must reach the harness-error exit rather than raising
        # UnicodeDecodeError out of the measurement.
        os.environ["SYNTHETIC_MODE"] = "rawbytes"
        try:
            rc = probe_flake.main(["--probe", "synthetic", "--runs", "1",
                                   "--artifact-root", str(tree.artifacts())])
        finally:
            os.environ.pop("SYNTHETIC_MODE", None)
        expect(rc == probe_flake.EXIT_HARNESS_ERROR,
               f"an event stream of invalid UTF-8 exits "
               f"{probe_flake.EXIT_HARNESS_ERROR}, not a traceback (got {rc})")

        # The whole point of the class: malformed protocol input reaches
        # the documented harness-error exit rather than a traceback.
        for name, body in (("unhashable id", '{"event": "check", "id": [], '
                                             '"outcome": "PASS"}\n'),
                           ("object id", '{"event": "check", "id": {}, '
                                         '"outcome": "PASS"}\n')):
            raw = tree.root / f"hostile-{name.replace(' ', '-')}.jsonl"
            raw.write_text(body, encoding="utf-8")
            os.environ["SYNTHETIC_RAW_PATH"] = str(raw)
            os.environ["SYNTHETIC_MODE"] = "raw"
            try:
                rc = probe_flake.main(
                    ["--probe", "synthetic", "--runs", "1",
                     "--artifact-root", str(tree.artifacts())])
            finally:
                os.environ.pop("SYNTHETIC_MODE", None)
                os.environ.pop("SYNTHETIC_RAW_PATH", None)
            expect(rc == probe_flake.EXIT_HARNESS_ERROR,
                   f"a stream with an {name} exits {probe_flake.EXIT_HARNESS_ERROR}, "
                   f"not a traceback (got {rc})")

        saved = (probe_flake.PORT_MIN, probe_flake.PORT_MAX)
        held = None
        try:
            probe_flake.PORT_MIN = probe_flake.PORT_MAX = 8009
            held = probe_flake.PortLease.try_acquire(8009)
            os.environ["SYNTHETIC_MODE"] = "pass"
            rc = probe_flake.main(["--probe", "synthetic", "--runs", "1",
                                   "--artifact-root", str(tree.artifacts())])
        finally:
            os.environ.pop("SYNTHETIC_MODE", None)
            if held:
                held.release()
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = saved
        expect(rc == probe_flake.EXIT_NO_PORT,
               "port-range exhaustion exits nonzero without starting a probe")


def test_render() -> None:
    print("\n-- human-readable table --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "fail", runs=2)
        text = probe_flake.render(m)
        for needle in ("alpha", "beta", "gamma", "MISS", "failures",
                       "timeouts", "RTS capabilities", "peak concurrency",
                       "retained artifacts"):
            expect(needle in text, f"the table reports {needle!r}")


TESTS = (
    test_result_document,
    test_exit_codes,
    test_render,
)
