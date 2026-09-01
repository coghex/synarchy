#!/usr/bin/env python3
"""Focused self-test for the read-only Codex `$test` evidence reader (#1432).

Deterministic, engine-free, GPU-free and offline. Every case runs
against a synthetic `codex-test` tree in a throwaway temporary
directory: a synthetic `registry.json` shaped like the real
`codex-test-coordinator/v1` document plus synthetic
`*.test-result.md` reports. Nothing here boots an engine, runs a
registered probe, or touches the developer's real machine-local `$test`
state. The real `tools/probe_external_evidence.py` is imported and
driven, so this exercises the shipped code paths rather than a copy.

The central contract under test is NON-INTERACTION, and it is proved
mechanically rather than inferred from the reader's output:

* every file under the synthetic tree is digested before and after each
  read, and the digests (and the path set) must be identical — registry,
  reports and lock files alike;
* the confinement cases record every file the reader actually opens, so
  an out-of-scope read fails even though the reader would never echo a
  byte of it back;
* `subprocess.run` / `subprocess.Popen` are replaced with tripwires, so
  a coordinator invocation of ANY subcommand — permitted or mutating —
  fails the test rather than passing quietly;
* `fcntl.flock` is replaced with a tripwire, so taking any `$test` lock
  fails the test.

The one case that legitimately shells out (`git rev-parse
--git-common-dir` for state resolution) builds its own scratch
repository with a real linked worktree and runs outside the tripwires.

Usage:
  python3 tools/test_probe_external_evidence.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import fcntl
import hashlib
import json
import os
import shutil
import stat
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_external_evidence as evidence  # noqa: E402
import probe_runner_registry  # noqa: E402

FAILURES: list[str] = []
PASSED = 0

SENTINEL = "SENTINEL-OUT-OF-SCOPE-CONTENT-MUST-NEVER-BE-READ"


# --------------------------------------------------------------------------
# Harness
# --------------------------------------------------------------------------

def check(condition: bool, label: str, detail: str = "") -> None:
    global PASSED
    if condition:
        PASSED += 1
        return
    FAILURES.append(f"{label}{': ' + detail if detail else ''}")


def check_equal(actual: object, expected: object, label: str) -> None:
    check(actual == expected, label, f"expected {expected!r}, got {actual!r}")


class Tripwire(AssertionError):
    """Raised when the reader reaches a forbidden interaction."""


class RecordReads:
    """Record every file path the reader actually opens for reading.

    The confinement contract cannot be proved from the reader's output
    alone: it extracts only an interpretation status and a section
    count, so an out-of-scope file could be read without a byte of it
    surfacing. This records the reads themselves.
    """

    def __init__(self) -> None:
        self.paths: list[Path] = []
        self._saved: dict[str, object] = {}

    def __enter__(self) -> "RecordReads":
        recorder = self

        def record(name: str, original):
            def wrapper(self_path, *args, **kwargs):
                recorder.paths.append(Path(self_path))
                return original(self_path, *args, **kwargs)
            return wrapper

        self._saved = {
            "read_text": Path.read_text,
            "read_bytes": Path.read_bytes,
            "open": Path.open,
        }
        Path.read_text = record("read_text", self._saved["read_text"])   # type: ignore[assignment]
        Path.read_bytes = record("read_bytes", self._saved["read_bytes"])  # type: ignore[assignment]
        Path.open = record("open", self._saved["open"])                  # type: ignore[assignment]
        return self

    def __exit__(self, *exc_info: object) -> bool:
        Path.read_text = self._saved["read_text"]      # type: ignore[assignment]
        Path.read_bytes = self._saved["read_bytes"]    # type: ignore[assignment]
        Path.open = self._saved["open"]                # type: ignore[assignment]
        return False


class NonInteraction:
    """Forbid subprocesses and locks, and pin every byte under `root`."""

    def __init__(self, root: Path) -> None:
        self.root = root
        self.before: dict[str, str] = {}
        self._saved: dict[str, object] = {}

    def _digest_tree(self) -> dict[str, str]:
        digests: dict[str, str] = {}
        for path in sorted(self.root.rglob("*")):
            relative = str(path.relative_to(self.root))
            if path.is_symlink():
                digests[relative] = "symlink:" + os.readlink(path)
            elif path.is_dir():
                digests[relative] = "dir"
            else:
                try:
                    digests[relative] = hashlib.sha256(path.read_bytes()).hexdigest()
                except OSError as exc:
                    digests[relative] = f"unreadable:{exc.errno}"
        return digests

    def __enter__(self) -> "NonInteraction":
        self.before = self._digest_tree()

        def forbidden_subprocess(*args: object, **kwargs: object) -> None:
            raise Tripwire(f"the reader invoked a subprocess: {args!r}")

        def forbidden_flock(*args: object, **kwargs: object) -> None:
            raise Tripwire("the reader took a lock")

        self._saved = {
            "run": subprocess.run,
            "Popen": subprocess.Popen,
            "call": subprocess.call,
            "check_output": subprocess.check_output,
            "flock": fcntl.flock,
            "lockf": fcntl.lockf,
        }
        subprocess.run = forbidden_subprocess            # type: ignore[assignment]
        subprocess.Popen = forbidden_subprocess          # type: ignore[assignment]
        subprocess.call = forbidden_subprocess           # type: ignore[assignment]
        subprocess.check_output = forbidden_subprocess   # type: ignore[assignment]
        fcntl.flock = forbidden_flock                    # type: ignore[assignment]
        fcntl.lockf = forbidden_flock                    # type: ignore[assignment]
        return self

    def __exit__(self, *exc_info: object) -> bool:
        subprocess.run = self._saved["run"]               # type: ignore[assignment]
        subprocess.Popen = self._saved["Popen"]           # type: ignore[assignment]
        subprocess.call = self._saved["call"]             # type: ignore[assignment]
        subprocess.check_output = self._saved["check_output"]  # type: ignore[assignment]
        fcntl.flock = self._saved["flock"]                # type: ignore[assignment]
        fcntl.lockf = self._saved["lockf"]                # type: ignore[assignment]
        return False

    def assert_untouched(self, label: str) -> None:
        after = self._digest_tree()
        check_equal(sorted(after), sorted(self.before), f"{label}: path set unchanged")
        changed = [p for p in after if p in self.before and after[p] != self.before[p]]
        check(not changed, f"{label}: bytes unchanged", f"changed: {changed}")


# --------------------------------------------------------------------------
# Synthetic state
# --------------------------------------------------------------------------

def make_run(test_id: str, run_id: str, **overrides: object) -> dict:
    """A synthetic registry record shaped like a real completed run."""
    record = {
        "area": "synthetic",
        "claimed_at": "2026-08-12T17:41:35Z",
        "completed_at": "2026-08-12T17:47:26Z",
        "elapsed_seconds": 288.783,
        "execution_status": "passed",
        "interpretation_outcome": "clean",
        "revision": "8f995f395dd1748f67ffcaeedc5cf8d7c2e9e430",
        "revision_committed_at": "2026-08-12T10:33:25-07:00",
        "revision_subject": "Document audio system design",
        "run_id": run_id,
        "status": "completed",
        "test_exit_code": 0,
        "test_id": test_id,
    }
    record.update(overrides)
    return {k: v for k, v in record.items() if v is not None or k in overrides}


MISSING = object()


def run_with_identity(run_id: str, identity: object, **overrides: object) -> dict:
    """A record whose `test_id` is set to an arbitrary value, or removed.

    `make_run` takes `test_id` positionally and drops None values, so a
    deliberately damaged identity has to be written onto the record
    afterwards — including the case where the field is absent entirely.
    """
    record = make_run("probe:placeholder", run_id, **overrides)
    if identity is MISSING:
        record.pop("test_id", None)
    else:
        record["test_id"] = identity
    return record


def report_text(run_id: str, test_id: str, interpretation: str,
                observations: int) -> str:
    lines = [
        "---",
        'schema: "codex-test-result/v1"',
        f'run_id: "{run_id}"',
        f'test_id: "{test_id}"',
        f'execution_status: "passed"',
        f'interpretation_status: "{interpretation}"',
        "---",
        "",
        f"# Test result: {test_id}",
        "",
        "## Observations",
        "",
    ]
    if observations == 0:
        lines.append("No reportable observations.")
    for index in range(1, observations + 1):
        lines.append(f"### OBS-{index:03d} — synthetic observation {index}")
        lines.append("")
        lines.append("- **Category:** gameplay")
        lines.append("")
    return "\n".join(lines) + "\n"


def build_state(root: Path, runs: list[dict], reports: dict[str, tuple[str, int]],
                *, schema: str = evidence.COORDINATOR_SCHEMA) -> Path:
    """Write a synthetic `codex-test` tree and return its root."""
    state = root / evidence.STATE_DIRNAME
    (state / evidence.REPORTS_DIRNAME).mkdir(parents=True, exist_ok=True)
    (state / "logs").mkdir(parents=True, exist_ok=True)
    (state / "registry.lock").write_text("", encoding="utf-8")
    (state / "base.lock").write_text("", encoding="utf-8")
    for run_id, (interpretation, count) in reports.items():
        test_id = next((r["test_id"] for r in runs if r.get("run_id") == run_id), "probe:x")
        path = state / evidence.REPORTS_DIRNAME / (run_id + evidence.REPORT_SUFFIX)
        path.write_text(report_text(run_id, test_id, interpretation, count),
                        encoding="utf-8")
    document = {
        "schema": schema,
        "updated_at": "2026-08-12T17:47:26Z",
        "snapshots": [],
        "proposals": [],
        "runs": runs,
    }
    (state / evidence.REGISTRY_FILENAME).write_text(
        json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return state


def report_path(state: Path, run_id: str) -> str:
    return str(state / evidence.REPORTS_DIRNAME / (run_id + evidence.REPORT_SUFFIX))


def read(state: Path | str, probe: str) -> dict:
    return evidence.read_probe_evidence(probe, state_root=state)


# --------------------------------------------------------------------------
# Cases
# --------------------------------------------------------------------------

def test_identity_mapping() -> None:
    """Keys map to `$test` ids by key, not by stripping a script suffix."""
    check_equal(evidence.test_id_for_probe("transfer_order"), "probe:transfer-order",
                "transfer_order maps to probe:transfer-order")
    check_equal(evidence.test_id_for_probe("persistence_contract_sweep"),
                "probe:persistence-contract-sweep",
                "persistence_contract_sweep maps by key")
    check_equal(evidence.probe_script("persistence_contract_sweep"),
                "persistence_contract_sweep.py",
                "persistence_contract_sweep has no _probe suffix to strip")
    check_equal(evidence.probe_script("transfer_order"), "transfer_order_probe.py",
                "transfer_order's registered script")

    keys = evidence.probe_keys()
    check_equal(keys, [k for k, _s, _p in probe_runner_registry.PROBES],
                "probe_keys mirrors probe_runner_registry.PROBES order")
    ids = [evidence.test_id_for_probe(k) for k in keys]
    check_equal(len(set(ids)), len(ids), "every registered key maps to a distinct id")
    check(all(i.startswith("probe:") and "_" not in i for i in ids),
          "every mapped id is a hyphenated probe: id")


def test_both_identities_map_to_one_probe() -> None:
    """`probe:` and `probe-flake:` are two identities of the same probe."""
    both = evidence.test_ids_for_probe("transfer_order")
    check_equal(both, {"run": "probe:transfer-order",
                       "flake": "probe-flake:transfer-order"},
                "transfer_order's two stable identities")
    check_equal(evidence.flake_test_id_for_probe("persistence_contract_sweep"),
                "probe-flake:persistence-contract-sweep",
                "the measurement identity is derived from the key too")

    keys = evidence.probe_keys()
    every = [i for k in keys for i in evidence.test_ids_for_probe(k).values()]
    check_equal(len(set(every)), len(every),
                "all identities across both namespaces stay distinct")
    check(all("_" not in i for i in every), "every identity is hyphenated")

    # The reverse mapping is by exact generated identity, never by
    # undoing the hyphenation — which is what keeps `probe:transfer_order`
    # from resolving onto the registered `transfer_order`.
    check_equal(evidence.probe_for_test_id("probe:transfer-order"),
                ("transfer_order", evidence.TEST_KIND_RUN),
                "an ordinary id resolves to its probe and kind")
    check_equal(evidence.probe_for_test_id("probe-flake:transfer-order"),
                ("transfer_order", evidence.TEST_KIND_FLAKE),
                "a measurement id resolves to the SAME probe, a different kind")
    for rejected in ("probe:transfer_order", "probe-flake:transfer_order",
                     "probe:transfer-order-extra", "gameplay:transfer-order",
                     "PROBE:TRANSFER-ORDER", "probe:", "probe-flake:", "",
                     "flake:transfer-order"):
        check_equal(evidence.probe_for_test_id(rejected), None,
                    f"{rejected!r} resolves to no probe")
    for rejected in (None, 17, ["probe:transfer-order"]):
        check_equal(evidence.probe_for_test_id(rejected), None,
                    f"a non-string test id ({type(rejected).__name__}) resolves to none")

    # Prefix families stay distinct across BOTH namespaces.
    for shorter, longer in (("repair", "repair_ai"), ("repair", "repair_item"),
                            ("power", "power_workshop"),
                            ("persistence_contract", "persistence_contract_sweep")):
        short_ids = set(evidence.test_ids_for_probe(shorter).values())
        long_ids = set(evidence.test_ids_for_probe(longer).values())
        check(not (short_ids & long_ids),
              f"{shorter} and {longer} share no identity")


def test_a_measurement_run_is_the_same_probes_work() -> None:
    """A `probe-flake:` run matches, and stays labelled as a measurement."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:transfer-order", "ordinary",
                     claimed_at="2026-08-12T10:00:00Z"),
            make_run("probe-flake:transfer-order", "measurement",
                     claimed_at="2026-08-13T10:00:00Z"),
            make_run("probe-flake:transfer-order-extra", "prefixed"),
            make_run("probe-flake:transfer_order", "underscored"),
            make_run("probe-flake:role", "another-probe"),
        ]
        state = build_state(Path(tmp), runs, {})
        with NonInteraction(state) as guard:
            result = read(state, "transfer_order")
            guard.assert_untouched("both-identity matching")

        check_equal([r["run_id"] for r in result["runs"]],
                    ["measurement", "ordinary"],
                    "both identities match, newest first")
        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(by_id["ordinary"]["test_id"], "probe:transfer-order",
                    "the ordinary run reports its own id")
        check_equal(by_id["ordinary"]["test_kind"], evidence.TEST_KIND_RUN,
                    "and is labelled an ordinary run")
        check_equal(by_id["measurement"]["test_id"], "probe-flake:transfer-order",
                    "the measurement reports its own id")
        check_equal(by_id["measurement"]["test_kind"], evidence.TEST_KIND_FLAKE,
                    "and is labelled a measurement")
        check_equal(result["test_id"], "probe:transfer-order",
                    "the document's test_id stays the ordinary identity")
        check_equal(result["test_ids"],
                    {"run": "probe:transfer-order",
                     "flake": "probe-flake:transfer-order"},
                    "and test_ids carries both")
        check_equal(result["diagnostics"], [], "the read is diagnostic-free")
        rendered = evidence.render(result)
        check("probe-flake:transfer-order" in rendered,
              "the measurement identity renders", rendered)
        check("(flake)" in rendered and "(run)" in rendered,
              "each run renders its kind", rendered)


def test_the_heartbeat_is_reported_raw() -> None:
    """`heartbeat_at` is surfaced un-interpreted, as text or as None."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "beating", status="running",
                     claimed_at="2026-08-13T10:00:00Z",
                     heartbeat_at="2026-08-13T10:04:00Z"),
            make_run("probe:role", "silent", status="running",
                     claimed_at="2026-08-12T10:00:00Z"),
            make_run("probe:role", "malformed", status="running",
                     claimed_at="2026-08-11T10:00:00Z", heartbeat_at=1234),
        ]
        state = build_state(Path(tmp), runs, {})
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("heartbeat reporting")
        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(by_id["beating"]["heartbeat_at"], "2026-08-13T10:04:00Z",
                    "a recorded heartbeat is reported verbatim")
        check_equal(by_id["silent"]["heartbeat_at"], None,
                    "an absent heartbeat is unavailable, never fabricated")
        check_equal(by_id["malformed"]["heartbeat_at"], None,
                    "a non-string heartbeat is unavailable too")
        check_equal(by_id["beating"]["run_state"], "running",
                    "the raw run state is reported; activity is not decided here")

        # `recorded_fields` separates "not recorded" from "recorded but
        # unusable", which the normalized `null` alone cannot.
        check("heartbeat_at" in by_id["malformed"]["recorded_fields"],
              "a malformed heartbeat is still listed as recorded")
        check("heartbeat_at" not in by_id["silent"]["recorded_fields"],
              "an absent heartbeat is not listed as recorded")
        check_equal(by_id["silent"]["recorded_fields"],
                    sorted(by_id["silent"]["recorded_fields"]),
                    "recorded_fields is sorted")
        check(all("claimed_at" in r["recorded_fields"] for r in result["runs"]),
              "every synthetic run records claimed_at")


def test_a_damaged_state_root_is_not_an_absent_one() -> None:
    """A state root that is THERE but unusable is damage, never absence.

    `Path.is_dir()` swallows `OSError` and answers False, so a regular
    file, a dangling symlink and an unstattable path at the state root
    all used to read as the normal "Codex is not installed here" result
    — an empty run list with NO diagnostic, which a consumer failing
    closed on unreadable active-run state would accept as a clean read.
    """
    for label, build in (
            ("a regular file", lambda p: p.write_text("not a state tree")),
            ("a dangling symlink", lambda p: p.symlink_to(p.parent / "gone"))):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / evidence.STATE_DIRNAME
            build(root)
            with NonInteraction(Path(tmp)) as guard:
                result = read(root, "role")
                guard.assert_untouched(f"{label} at the state root")
            check_equal(result["state"], evidence.STATE_PRESENT,
                        f"{label} at the state root is present, not absent")
            check_equal(result["runs"], [], f"{label} yields no runs")
            check_equal([d["scope"] for d in result["diagnostics_detail"]],
                        [evidence.SCOPE_REGISTRY],
                        f"{label} is diagnosed as active-run state, so a "
                        f"fail-closed consumer sees it")
            check(any("is not a directory" in d for d in result["diagnostics"]),
                  f"{label} says what is wrong", str(result["diagnostics"]))

    # A path that cannot be examined at all is the reader's rejection.
    with tempfile.TemporaryDirectory() as tmp:
        blocker = Path(tmp) / evidence.STATE_DIRNAME
        blocker.write_text("a file where a directory belongs", encoding="utf-8")
        try:
            read(blocker / "nested" / evidence.STATE_DIRNAME, "role")
            check(False, "an unstattable state root is rejected")
        except evidence.EvidenceRejected as exc:
            check("cannot stat the $test state root" in str(exc),
                  "and says so", str(exc))

    # A genuinely absent root is still the normal, diagnostic-free result.
    with tempfile.TemporaryDirectory() as tmp:
        result = read(Path(tmp) / "nothing-here", "role")
        check_equal(result["state"], evidence.STATE_ABSENT, "absence is absence")
        check_equal(result["diagnostics"], [], "and carries no diagnostic")


def test_entry_state_separates_absent_from_unexaminable() -> None:
    """The shared primitive keeps three answers apart, not two."""
    with tempfile.TemporaryDirectory() as tmp:
        base = Path(tmp)
        regular = base / "file"
        regular.write_text("x", encoding="utf-8")
        directory = base / "dir"
        directory.mkdir()
        dangling = base / "dangling"
        dangling.symlink_to(base / "gone")
        good_link = base / "link"
        good_link.symlink_to(regular)

        present, mode, failure = evidence.entry_state(regular)
        check((present, failure), (True, None), "a regular file is present")
        check(mode is not None and stat.S_ISREG(mode), "and reports S_ISREG")

        present, mode, failure = evidence.entry_state(directory)
        check(mode is not None and stat.S_ISDIR(mode), "a directory reports S_ISDIR")

        present, mode, failure = evidence.entry_state(good_link)
        check(mode is not None and stat.S_ISREG(mode),
              "a symlink is judged by its target's kind")

        present, mode, failure = evidence.entry_state(dangling)
        check_equal((present, mode, failure), (True, None, None),
                    "a dangling symlink is PRESENT with no usable kind")

        present, mode, failure = evidence.entry_state(base / "missing")
        check_equal((present, mode, failure), (False, None, None),
                    "a missing path is absent, with no failure")

        present, mode, failure = evidence.entry_state(regular / "child")
        check(present and failure is not None,
              "a path under a non-directory is a stat FAILURE, not an absence",
              f"{present!r} {failure!r}")
        check(not os.path.lexists(regular / "child"),
              "which is exactly what lexists cannot tell you")


def test_an_unreadable_run_identity_is_record_damage() -> None:
    """A record whose `test_id` cannot be read is diagnosed, not dropped.

    `test_id` is arbitrary external JSON. An UNHASHABLE value crashed the
    whole read outright (`TypeError` from the set membership test),
    taking every valid run with it; a missing or empty one was silently
    skipped, which let an otherwise-active run of indeterminate ownership
    pass as no evidence at all.
    """
    shapes = [
        ("an unhashable list", []),
        ("an unhashable dict", {}),
        ("a number", 17),
        ("a boolean", True),
        ("an absent field", MISSING),
        ("null", None),
        ("an empty string", ""),
        ("whitespace", "   "),
    ]
    for label, value in shapes:
        with tempfile.TemporaryDirectory() as tmp:
            runs = [
                run_with_identity("damaged", value, status="running"),
                make_run("probe:role", "healthy"),
            ]
            state = build_state(Path(tmp), runs, {})
            with NonInteraction(state) as guard:
                result = read(state, "role")
                guard.assert_untouched(f"{label} test_id")
            check_equal([r["run_id"] for r in result["runs"]], ["healthy"],
                        f"{label} does not crash the read, and the valid run "
                        f"is still reported")
            check_equal([d["scope"] for d in result["diagnostics_detail"]],
                        [evidence.SCOPE_RECORD],
                        f"{label} is diagnosed as record damage")
            detail = result["diagnostics"][0]
            check("no usable test_id" in detail, f"{label} says what is wrong",
                  detail)
            check("damaged" in detail, f"{label} names the run", detail)
            json.dumps(result)
            check(True, f"{label} leaves the document serializable")

    # A well-formed identity for ANOTHER probe is not damage; it is just
    # a non-match, and must stay diagnostic-free.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [
            make_run("gameplay:role", "other-namespace"),
            make_run("probe:transfer-order", "other-probe")], {})
        result = read(state, "role")
        check_equal(result["runs"], [], "neither matches")
        check_equal(result["diagnostics"], [],
                    "and a non-match is never diagnosed as damage")


def test_diagnostics_carry_the_state_they_concern() -> None:
    """Every diagnostic is scoped, so a consumer can fail closed precisely."""
    # A damaged REGISTRY is active-run state.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        (state / evidence.REGISTRY_FILENAME).write_text("{not json",
                                                        encoding="utf-8")
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("unparseable registry")
        check_equal([d["scope"] for d in result["diagnostics_detail"]],
                    [evidence.SCOPE_REGISTRY],
                    "an unparseable registry is scoped `registry`")
        check_equal(result["diagnostics"],
                    [d["message"] for d in result["diagnostics_detail"]],
                    "the flat list mirrors the detailed one, in order")

    # A damaged RECORD is active-run state too.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), ["not-an-object",
                                        make_run("probe:role", "run")], {})
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("malformed record")
        check_equal([d["scope"] for d in result["diagnostics_detail"]],
                    [evidence.SCOPE_RECORD],
                    "a non-object run record is scoped `record`")
        check_equal([r["run_id"] for r in result["runs"]], ["run"],
                    "the valid record is still read")

    # A damaged REPORT is NOT active-run state.
    with tempfile.TemporaryDirectory() as tmp:
        runs = [make_run("probe:role", "run")]
        state = build_state(Path(tmp), runs, {})
        runs[0]["report_path"] = report_path(state, "run")
        build_state(Path(tmp), runs, {})
        # A DIRECTORY where the report belongs: it exists, so it is
        # damage rather than a report that was simply never written.
        (state / evidence.REPORTS_DIRNAME / ("run" + evidence.REPORT_SUFFIX)).mkdir()
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("damaged report")
        check_equal([d["scope"] for d in result["diagnostics_detail"]],
                    [evidence.SCOPE_REPORT],
                    "a damaged report is scoped `report`, never `registry`")

    # A clean read carries neither list.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        result = read(state, "role")
        check_equal(result["diagnostics_detail"], [],
                    "a clean read has no scoped diagnostics")

    # The absent-state result still carries both keys.
    with tempfile.TemporaryDirectory() as tmp:
        result = read(Path(tmp) / "nothing-here", "role")
        check_equal(result["state"], evidence.STATE_ABSENT, "absent state")
        check_equal(result["diagnostics_detail"], [],
                    "an absent state tree is not damage")
        check_equal(result["test_ids"]["flake"], "probe-flake:role",
                    "both identities are reported even with no state")


def test_unknown_key_is_rejected() -> None:
    """An unregistered key is a rejection, not a no-evidence answer."""
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [], {})
        try:
            read(state, "definitely_not_a_probe")
            check(False, "unknown key raises EvidenceRejected")
        except evidence.EvidenceRejected as exc:
            check("definitely_not_a_probe" in str(exc),
                  "the rejection names the offending key", str(exc))
            check("probe_runner_registry.PROBES" in str(exc),
                  "the rejection names the authoritative registry", str(exc))

        code = evidence.main(["--probe", "definitely_not_a_probe"])
        check_equal(code, evidence.EXIT_REJECTED, "CLI exits 2 on an unknown key")
        check_equal(evidence.main(["--probe", "role", "--state-root", str(state)]),
                    evidence.EXIT_OK, "CLI exits 0 on a registered key")


def test_exact_matching() -> None:
    """Only the exactly-mapped `$test` id matches."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:transfer-order", "exact"),
            make_run("probe:transfer-order-extra", "prefixed"),
            make_run("probe:transfer_order", "underscored"),
            make_run("gameplay:transfer-order", "other-namespace"),
            make_run("probe:transfer", "shorter"),
            make_run("PROBE:TRANSFER-ORDER", "uppercased"),
        ]
        state = build_state(Path(tmp), runs, {})
        with NonInteraction(state) as guard:
            result = read(state, "transfer_order")
            guard.assert_untouched("exact matching")
        check_equal([r["run_id"] for r in result["runs"]], ["exact"],
                    "only the exact test id matches")
        check_equal(result["state"], evidence.STATE_PRESENT, "state reported present")
        check_equal(result["diagnostics"], [], "an exact-match read is diagnostic-free")


def test_clean_and_observed_reports() -> None:
    """Observation status distinguishes clean, observed and not-yet-known."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "clean-run", claimed_at="2026-08-12T10:00:00Z",
                     interpretation_outcome="clean",
                     report_path=None),
            make_run("probe:role", "observed-run", claimed_at="2026-08-13T10:00:00Z",
                     interpretation_outcome="observations"),
        ]
        state = build_state(Path(tmp), runs,
                            {"clean-run": ("clean", 0), "observed-run": ("observations", 2)})
        for record in runs:
            record["report_path"] = report_path(state, record["run_id"])
        build_state(Path(tmp), runs,
                    {"clean-run": ("clean", 0), "observed-run": ("observations", 2)})

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("clean and observed reports")

        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal([r["run_id"] for r in result["runs"]], ["observed-run", "clean-run"],
                    "runs are ordered newest-claimed first")
        check_equal(by_id["clean-run"]["observations"], evidence.OBSERVATIONS_NONE,
                    "a clean run records no observations")
        check_equal(by_id["clean-run"]["report"]["observation_count"], 0,
                    "a clean report has zero OBS sections")
        check_equal(by_id["clean-run"]["report"]["status"], evidence.REPORT_AVAILABLE,
                    "a clean report is readable")
        check_equal(by_id["observed-run"]["observations"], evidence.OBSERVATIONS_RECORDED,
                    "an observed run records observations")
        check_equal(by_id["observed-run"]["report"]["observation_count"], 2,
                    "both OBS sections are counted")
        check_equal(by_id["observed-run"]["report"]["interpretation_status"], "observations",
                    "the report's own interpretation status is surfaced")
        check_equal(result["diagnostics"], [], "readable reports produce no diagnostic")


def test_incomplete_run_reports_unavailable_not_false() -> None:
    """An active or partially recorded run is surfaced, with nulls."""
    with tempfile.TemporaryDirectory() as tmp:
        active = make_run(
            "probe:role", "active-run", status="running",
            execution_status="not-run", interpretation_outcome="pending",
            completed_at=None, elapsed_seconds=None, test_exit_code=None,
        )
        del active["completed_at"], active["elapsed_seconds"], active["test_exit_code"]
        legacy = make_run("probe:role", "legacy-run", claimed_at="2026-08-01T00:00:00Z")
        for field in ("execution_status", "interpretation_outcome", "elapsed_seconds",
                      "revision_subject", "test_exit_code"):
            del legacy[field]
        state = build_state(Path(tmp), [active, legacy], {})
        active["report_path"] = report_path(state, "active-run")   # not written yet
        build_state(Path(tmp), [active, legacy], {})

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("incomplete runs")

        by_id = {r["run_id"]: r for r in result["runs"]}
        run = by_id["active-run"]
        check_equal(run["run_state"], "running", "an active run keeps its state")
        check_equal(run["execution_status"], "not-run", "its mechanical status is surfaced")
        check_equal(run["duration_seconds"], None, "an unrecorded duration is None")
        check_equal(run["exit_code"], None, "an unrecorded exit code is None")
        check_equal(run["observations"], evidence.OBSERVATIONS_UNAVAILABLE,
                    "a pending interpretation is unavailable, not 'none'")
        check_equal(run["report"]["status"], evidence.REPORT_ABSENT,
                    "a report that does not exist yet is absent")

        old = by_id["legacy-run"]
        check_equal(old["execution_status"], None, "a missing mechanical status is None")
        check_equal(old["exit_code"], None, "a missing exit code is None")
        check_equal(old["duration_seconds"], None, "a missing duration is None")
        check_equal(old["tested_commit"], active["revision"],
                    "a legacy record still reports its provenance")
        check_equal(old["observations"], evidence.OBSERVATIONS_UNAVAILABLE,
                    "a missing interpretation is unavailable")
        check_equal(old["report"]["status"], evidence.REPORT_NOT_RECORDED,
                    "a record with no report_path records no report")
        check_equal(result["diagnostics"], [],
                    "an incomplete record is data, not damage")


def test_mechanical_outcome_is_not_inferred_from_interpretation() -> None:
    """Execution status comes from the registry, never from the report."""
    with tempfile.TemporaryDirectory() as tmp:
        failed = make_run("probe:role", "failed-but-clean-report",
                          execution_status="failed", test_exit_code=1,
                          interpretation_outcome="observations")
        state = build_state(Path(tmp), [failed],
                            {"failed-but-clean-report": ("clean", 0)})
        failed["report_path"] = report_path(state, "failed-but-clean-report")
        build_state(Path(tmp), [failed], {"failed-but-clean-report": ("clean", 0)})

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("mechanical vs interpreted")

        run = result["runs"][0]
        check_equal(run["execution_status"], "failed",
                    "the mechanical outcome is the registry's, not the report's")
        check_equal(run["exit_code"], 1, "the recorded exit code is surfaced")
        check_equal(run["report"]["interpretation_status"], "clean",
                    "the report's disagreeing interpretation is reported beside it")
        check_equal(run["observations"], evidence.OBSERVATIONS_RECORDED,
                    "the registry's interpretation drives observation status")


def test_missing_and_malformed_reports_are_non_fatal() -> None:
    """Damaged report state diagnoses; it never fails or drops the run."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "unreadable-run", claimed_at="2026-08-14T00:00:00Z"),
            make_run("probe:role", "headless-run", claimed_at="2026-08-13T00:00:00Z"),
        ]
        state = build_state(Path(tmp), runs, {"headless-run": ("clean", 0)})
        for record in runs:
            record["report_path"] = report_path(state, record["run_id"])
        build_state(Path(tmp), runs, {"headless-run": ("clean", 0)})

        # A report with no frontmatter at all, and one that is not UTF-8.
        Path(report_path(state, "headless-run")).write_text(
            "# Test result\n\n### OBS-001 — one\n", encoding="utf-8")
        Path(report_path(state, "unreadable-run")).write_bytes(b"\xff\xfe\x00 not utf-8")

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("damaged reports")

        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(len(result["runs"]), 2, "both runs are still reported")
        check_equal(by_id["unreadable-run"]["report"]["status"], evidence.REPORT_UNREADABLE,
                    "a non-decodable report is unreadable")
        check_equal(by_id["unreadable-run"]["execution_status"], "passed",
                    "a damaged report does not disturb the mechanical fields")
        check_equal(by_id["headless-run"]["report"]["status"], evidence.REPORT_AVAILABLE,
                    "a frontmatter-less report is still read")
        check_equal(by_id["headless-run"]["report"]["observation_count"], 1,
                    "its OBS section is still counted")
        check_equal(by_id["headless-run"]["report"]["interpretation_status"], None,
                    "it carries no interpretation status")
        check(any("unreadable-run" in d for d in result["diagnostics"]),
              "the unreadable report is diagnosed", str(result["diagnostics"]))
        check(any("frontmatter" in d for d in result["diagnostics"]),
              "the frontmatter-less report is diagnosed", str(result["diagnostics"]))


def test_an_existing_non_regular_report_is_damage_not_absence() -> None:
    """A path that EXISTS but is not a regular file is diagnosed.

    A genuinely missing report is data — the run has not written it yet,
    or it was cleaned up. A directory (or socket, or device) sitting
    where a `*.test-result.md` file belongs is damaged external state,
    and damage is non-fatal but never silent.
    """
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "directory-run", claimed_at="2026-08-15T00:00:00Z"),
            make_run("probe:role", "dangling-run", claimed_at="2026-08-14T00:00:00Z"),
        ]
        state = build_state(Path(tmp), runs, {})
        reports = state / evidence.REPORTS_DIRNAME

        # A DIRECTORY named exactly like a report.
        (reports / ("directory-run" + evidence.REPORT_SUFFIX)).mkdir()
        # A symlink inside reports/ whose target, also inside reports/,
        # does not exist: in scope, but genuinely not there.
        os.symlink(reports / ("missing" + evidence.REPORT_SUFFIX),
                   reports / ("dangling-run" + evidence.REPORT_SUFFIX))

        for record in runs:
            record["report_path"] = report_path(state, record["run_id"])
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = runs
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("non-regular report")

        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(by_id["directory-run"]["report"]["status"], evidence.REPORT_UNREADABLE,
                    "a directory where a report belongs is unreadable, not absent")
        check_equal(by_id["directory-run"]["report"]["observation_count"], None,
                    "it contributes no observation count")
        check_equal(by_id["directory-run"]["execution_status"], "passed",
                    "the run's mechanical fields survive the damage")
        check(any("directory-run" in d and "not a regular file" in d
                  for d in result["diagnostics"]),
              "the non-regular report is diagnosed", str(result["diagnostics"]))

        check_equal(by_id["dangling-run"]["report"]["status"], evidence.REPORT_ABSENT,
                    "an in-scope path that is simply not there is absent")
        check(not any("dangling-run" in d for d in result["diagnostics"]),
              "absence is not diagnosed", str(result["diagnostics"]))
        check_equal(len(result["diagnostics"]), 1,
                    "exactly one diagnostic, for the damage alone")
        check(not any(p.name.endswith(evidence.REPORT_SUFFIX) for p in reads.paths),
              "neither non-file path is opened for reading",
              str([str(p) for p in reads.paths]))


def test_report_reads_are_confined_to_the_reports_directory() -> None:
    """A recorded path never widens read scope."""
    with tempfile.TemporaryDirectory() as tmp:
        outside = Path(tmp) / "outside.test-result.md"
        outside.write_text(SENTINEL + "\n### OBS-001 — leaked\n", encoding="utf-8")

        runs = [
            make_run("probe:role", "absolute-escape", claimed_at="2026-08-15T00:00:00Z"),
            make_run("probe:role", "traversal-escape", claimed_at="2026-08-14T00:00:00Z"),
            make_run("probe:role", "symlink-escape", claimed_at="2026-08-13T00:00:00Z"),
            make_run("probe:role", "wrong-suffix", claimed_at="2026-08-12T00:00:00Z"),
            make_run("probe:role", "nested", claimed_at="2026-08-11T00:00:00Z"),
        ]
        state = build_state(Path(tmp), runs, {})
        reports = state / evidence.REPORTS_DIRNAME
        os.symlink(outside, reports / ("symlink-escape" + evidence.REPORT_SUFFIX))
        (reports / "wrong-suffix.md").write_text(SENTINEL, encoding="utf-8")
        (reports / "nested").mkdir()
        (reports / "nested" / ("nested" + evidence.REPORT_SUFFIX)).write_text(
            SENTINEL, encoding="utf-8")

        paths = {
            "absolute-escape": str(outside),
            "traversal-escape": str(reports / ".." / ".." / "outside.test-result.md"),
            "symlink-escape": report_path(state, "symlink-escape"),
            "wrong-suffix": str(reports / "wrong-suffix.md"),
            "nested": str(reports / "nested" / ("nested" + evidence.REPORT_SUFFIX)),
        }
        for record in runs:
            record["report_path"] = paths[record["run_id"]]
        build_state(Path(tmp), runs, {})

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("confined report reads")

        scope = reports.resolve()
        escaped = [p for p in reads.paths
                   if p.resolve() != (state / evidence.REGISTRY_FILENAME).resolve()
                   and p.resolve().parent != scope]
        check(not escaped, "no file outside reports/ is opened at all",
              str([str(p) for p in escaped]))
        check(outside.resolve() not in [p.resolve() for p in reads.paths],
              "the out-of-scope target is never opened")
        rendered = evidence.render(result) + json.dumps(result)
        check(SENTINEL not in rendered,
              "no out-of-scope file content reaches the output")
        for run in result["runs"]:
            check_equal(run["report"]["status"], evidence.REPORT_OUT_OF_SCOPE,
                        f"{run['run_id']} is refused as out of scope")
            check_equal(run["report"]["observation_count"], None,
                        f"{run['run_id']} contributes no observation count")
        check_equal(len(result["diagnostics"]), len(runs),
                    "each refusal is diagnosed exactly once")


def test_a_symlinked_reports_directory_refuses_every_read() -> None:
    """The scope check is on the DIRECTORY too, not only each path.

    A symlinked `reports/` relocates the whole read scope out of the
    state tree while every individual recorded path still resolves to a
    `*.test-result.md` file directly under its own parent — so the
    per-path check alone would happily read them.
    """
    with tempfile.TemporaryDirectory() as tmp:
        runs = [make_run("probe:role", "relocated-run")]
        state = build_state(Path(tmp), runs, {})

        # Move reports/ out of the state tree and symlink it back in,
        # with a perfectly well-formed report waiting inside it.
        external = Path(tmp) / "elsewhere"
        external.mkdir()
        leaked = external / ("relocated-run" + evidence.REPORT_SUFFIX)
        leaked.write_text(
            report_text("relocated-run", "probe:role", "observations", 3)
            + SENTINEL + "\n", encoding="utf-8")
        shutil.rmtree(state / evidence.REPORTS_DIRNAME)
        os.symlink(external, state / evidence.REPORTS_DIRNAME)

        runs[0]["report_path"] = str(state / evidence.REPORTS_DIRNAME
                                     / ("relocated-run" + evidence.REPORT_SUFFIX))
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = runs
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("symlinked reports directory")

        opened = [path.resolve() for path in reads.paths]
        check(leaked.resolve() not in opened,
              "the relocated report is never opened", str([str(p) for p in opened]))
        check(external.resolve() not in [p.resolve().parent for p in reads.paths],
              "nothing in the relocated directory is opened")
        run = result["runs"][0]
        check_equal(run["report"]["status"], evidence.REPORT_OUT_OF_SCOPE,
                    "the relocated report is refused as out of scope")
        check_equal(run["report"]["observation_count"], None,
                    "it contributes no observation count")
        check_equal(run["execution_status"], "passed",
                    "the run's mechanical fields are still reported")
        check(any("immediate child of the state root" in d
                  for d in result["diagnostics"]),
              "the relocated directory is diagnosed once at directory level",
              str(result["diagnostics"]))
        check_equal(len(result["diagnostics"]), 1,
                    "one directory-level diagnostic, not one per run")
        check(SENTINEL not in evidence.render(result) + json.dumps(result),
              "no relocated content reaches the output")

        # The scope helper says so directly, and creates nothing.
        diagnostics = evidence.DiagnosticLog()
        check_equal(evidence.resolve_reports_scope(state, diagnostics), None,
                    "resolve_reports_scope refuses the relocated directory")
        check_equal(len(diagnostics.entries), 1, "and diagnoses it exactly once")
        check_equal(diagnostics.scopes(), {evidence.SCOPE_REPORT},
                    "and scopes it to the report, not to active-run state")

        # A real directory in the same place is trusted again.
        (state / evidence.REPORTS_DIRNAME).unlink()
        (state / evidence.REPORTS_DIRNAME).mkdir()
        trusted = evidence.DiagnosticLog()
        check_equal(evidence.resolve_reports_scope(state, trusted),
                    (state / evidence.REPORTS_DIRNAME).resolve(),
                    "a real reports directory is trusted")
        check_equal(trusted.entries, [], "and produces no diagnostic")


def test_a_misplaced_reports_directory_refuses_every_read() -> None:
    """`reports/` must be a DIRECTORY, not merely correctly named.

    A regular file sitting at `reports` passes the resolve-and-confine
    check, and every recorded report then resolves to a path under it
    that does not exist — so without a kind check each one would read as
    a silent `absent` rather than as the damaged state it is.
    """
    with tempfile.TemporaryDirectory() as tmp:
        runs = [make_run("probe:role", "run")]
        state = build_state(Path(tmp), runs, {})
        runs[0]["report_path"] = report_path(state, "run")
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = runs
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))
        shutil.rmtree(state / evidence.REPORTS_DIRNAME)
        (state / evidence.REPORTS_DIRNAME).write_text(SENTINEL, encoding="utf-8")

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("misplaced reports directory")

        run = result["runs"][0]
        check_equal(run["report"]["status"], evidence.REPORT_OUT_OF_SCOPE,
                    "the report is refused, not reported absent")
        check_equal(run["execution_status"], "passed",
                    "the run's mechanical fields are still reported")
        check(any("not a directory" in d for d in result["diagnostics"]),
              "the misplaced reports path is diagnosed", str(result["diagnostics"]))
        check_equal(len(result["diagnostics"]), 1,
                    "one directory-level diagnostic, not one per run")
        check((state / evidence.REPORTS_DIRNAME).resolve()
              not in [p.resolve() for p in reads.paths],
              "the file standing in for the directory is never opened")
        check(SENTINEL not in evidence.render(result) + json.dumps(result),
              "none of its content reaches the output")

        # An ABSENT reports directory is not damage: the reports are
        # simply not there, which each run already says for itself.
        (state / evidence.REPORTS_DIRNAME).unlink()
        clean = read(state, "role")
        check_equal(clean["runs"][0]["report"]["status"], evidence.REPORT_ABSENT,
                    "an absent reports directory makes each report absent")
        check_equal(clean["diagnostics"], [],
                    "and is not diagnosed as damage")


def test_the_registry_is_confined_to_the_state_root() -> None:
    """A symlinked or non-regular `registry.json` is refused, not followed."""
    with tempfile.TemporaryDirectory() as tmp:
        external = Path(tmp) / "planted.json"
        external.write_text(json.dumps({
            "schema": evidence.COORDINATOR_SCHEMA,
            "runs": [make_run("probe:role", "planted-run",
                              revision_subject=SENTINEL)],
        }), encoding="utf-8")

        state = build_state(Path(tmp), [make_run("probe:role", "real-run")], {})
        (state / evidence.REGISTRY_FILENAME).unlink()
        os.symlink(external, state / evidence.REGISTRY_FILENAME)

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("symlinked registry")

        check_equal(result["runs"], [], "a relocated registry contributes no runs")
        check(external.resolve() not in [p.resolve() for p in reads.paths],
              "the planted registry is never opened",
              str([str(p) for p in reads.paths]))
        check(SENTINEL not in evidence.render(result) + json.dumps(result),
              "none of its content reaches the output")
        check(any("refused to read the registry" in d for d in result["diagnostics"]),
              "the relocated registry is diagnosed", str(result["diagnostics"]))
        check_equal(result["state"], evidence.STATE_PRESENT,
                    "the state is still present, just unusable")

        diagnostics = evidence.DiagnosticLog()
        check_equal(evidence.resolve_registry_path(state, diagnostics), None,
                    "resolve_registry_path refuses it directly")
        check_equal(len(diagnostics.entries), 1, "and diagnoses it exactly once")
        check_equal(diagnostics.scopes(), {evidence.SCOPE_REGISTRY},
                    "and scopes it to the registry")

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        (state / evidence.REGISTRY_FILENAME).unlink()
        (state / evidence.REGISTRY_FILENAME).mkdir()
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("non-regular registry")
        check_equal(result["runs"], [], "a directory registry contributes no runs")
        check(any("not a regular file" in d for d in result["diagnostics"]),
              "a non-regular registry is diagnosed", str(result["diagnostics"]))

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        resolved = evidence.resolve_registry_path(state, evidence.DiagnosticLog())
        check_equal(resolved, (state / evidence.REGISTRY_FILENAME).resolve(),
                    "a real registry resolves to itself")


def test_a_malformed_report_path_never_aborts_the_read() -> None:
    """An unusable path STRING is one run's diagnostic, not a traceback.

    A registry field is arbitrary external text. A path built from a
    string containing an embedded NUL raises `ValueError` — not
    `OSError` — from `resolve` and `stat`, so catching only `OSError`
    would let one malformed record abort the whole read and take every
    later valid run with it.
    """
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "nul-run", claimed_at="2026-08-15T00:00:00Z"),
            make_run("probe:role", "long-run", claimed_at="2026-08-14T00:00:00Z"),
            make_run("probe:role", "good-run", claimed_at="2026-08-13T00:00:00Z"),
        ]
        state = build_state(Path(tmp), runs, {"good-run": ("observations", 2)})
        paths = {
            "nul-run": report_path(state, "nul-run").replace("nul-run", "nul\x00run"),
            "long-run": report_path(state, "n" * 4096),
            "good-run": report_path(state, "good-run"),
        }
        for record in runs:
            record["report_path"] = paths[record["run_id"]]
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = runs
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("malformed report path")

        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(len(result["runs"]), 3, "no run is lost to the malformed one")
        check_equal(by_id["nul-run"]["report"]["status"], evidence.REPORT_UNREADABLE,
                    "an unusable path string is unreadable evidence")
        check_equal(by_id["nul-run"]["execution_status"], "passed",
                    "the malformed run's mechanical fields survive")
        check(any("nul-run" in d for d in result["diagnostics"]),
              "the malformed path is diagnosed", str(result["diagnostics"]))

        # The LATER, valid run is still read in full — this is the half
        # a bare traceback would have destroyed.
        check_equal(by_id["good-run"]["report"]["status"], evidence.REPORT_AVAILABLE,
                    "a later valid report is still read")
        check_equal(by_id["good-run"]["report"]["observation_count"], 2,
                    "and its observations are still counted")
        check(by_id["long-run"]["report"]["status"] in (
                  evidence.REPORT_UNREADABLE, evidence.REPORT_ABSENT),
              "an over-long path is handled without raising",
              str(by_id["long-run"]["report"]["status"]))

        check_equal(evidence.main(["--probe", "role", "--json",
                                   "--state-root", str(state)]),
                    evidence.EXIT_OK, "the CLI still exits 0")

    # An unusable STATE ROOT is controlled too — a rejection naming it,
    # or the ordinary absent-state answer, but never a traceback.
    # (`Path.is_dir` swallows the NUL itself on CPython and answers
    # False; the reader's own guard covers platforms where it does not.)
    try:
        result = read("/tmp/nul\x00root", "role")
        check_equal(result["state"], evidence.STATE_ABSENT,
                    "an unstattable state root reads as absent")
        check_equal(result["runs"], [], "and contributes no runs")
    except evidence.EvidenceRejected as exc:
        check("state root" in str(exc),
              "the rejection names the state root", str(exc))
        check(True, "and is a controlled rejection")
    except ValueError as exc:                              # pragma: no cover
        check(False, "an unusable state root never raises ValueError", repr(exc))
        check(False, "an unusable state root never raises ValueError", repr(exc))

    # And so is one that git cannot resolve for the same reason.
    try:
        evidence.resolve_state_root("/tmp/nul\x00repo")
        check(False, "an unusable repo path raises EvidenceRejected")
    except evidence.EvidenceRejected as exc:
        check(True, "resolve_state_root rejects it")
    except ValueError as exc:                              # pragma: no cover
        check(False, "resolve_state_root raises EvidenceRejected", repr(exc))


def test_absent_state_is_success_not_error() -> None:
    """An absent `codex-test` tree is the normal no-evidence result."""
    with tempfile.TemporaryDirectory() as tmp:
        missing = Path(tmp) / "no-such-state"
        result = read(missing, "role")
        check_equal(result["state"], evidence.STATE_ABSENT, "the state reads as absent")
        check_equal(result["runs"], [], "no runs are reported")
        check_equal(result["diagnostics"], [], "absence is not a diagnostic")
        check_equal(result["test_id"], "probe:role", "the mapped id is still reported")
        check(not missing.exists(), "resolving absent state creates nothing")
        check_equal(evidence.main(["--probe", "role", "--state-root", str(missing)]),
                    evidence.EXIT_OK, "the CLI exits 0 on absent state")
        check("no external evidence" in evidence.render(result),
              "the rendering says so plainly")


def test_damaged_registry_is_non_fatal() -> None:
    """A malformed or missing registry diagnoses; it never raises."""
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        (state / evidence.REGISTRY_FILENAME).write_text("{ not json", encoding="utf-8")
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("malformed registry")
        check_equal(result["state"], evidence.STATE_PRESENT, "the state is still present")
        check_equal(result["runs"], [], "an unparseable registry yields no runs")
        check(any("cannot parse" in d for d in result["diagnostics"]),
              "the parse failure is diagnosed", str(result["diagnostics"]))

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [], {})
        (state / evidence.REGISTRY_FILENAME).unlink()
        result = read(state, "role")
        check(any("does not" in d for d in result["diagnostics"]),
              "a missing registry beside present state is diagnosed",
              str(result["diagnostics"]))

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {},
                            schema="codex-test-coordinator/v99")
        result = read(state, "role")
        check(any("v99" in d for d in result["diagnostics"]),
              "an unexpected schema is diagnosed", str(result["diagnostics"]))
        check_equal([r["run_id"] for r in result["runs"]], ["run"],
                    "an unexpected schema is still read best-effort")

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [], {})
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = ["not-an-object", make_run("probe:role", "good")]
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))
        result = read(state, "role")
        check_equal([r["run_id"] for r in result["runs"]], ["good"],
                    "a malformed record is skipped, the good one kept")
        check(any("not an object" in d for d in result["diagnostics"]),
              "the malformed record is diagnosed", str(result["diagnostics"]))


def test_non_finite_numbers_never_reach_the_output() -> None:
    """`NaN`/`Infinity` in the registry are malformed state, and diagnosed.

    Python's `json` reads those non-standard constants happily and
    writes them straight back, so without this the reader would present
    a damaged registry with no diagnostic AND emit invalid JSON from
    `--json`.
    """
    for token in ("NaN", "Infinity", "-Infinity"):
        with tempfile.TemporaryDirectory() as tmp:
            state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
            document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
            (state / evidence.REGISTRY_FILENAME).write_text(
                json.dumps(document).replace('"elapsed_seconds": 288.783',
                                             f'"elapsed_seconds": {token}'),
                encoding="utf-8")
            with NonInteraction(state) as guard:
                result = read(state, "role")
                guard.assert_untouched(f"{token} registry")
            check_equal(result["runs"], [],
                        f"a registry carrying {token} contributes no runs")
            check(any("cannot parse" in d and token.lstrip("-") in d
                      for d in result["diagnostics"]),
                  f"{token} is diagnosed as a parse failure",
                  str(result["diagnostics"]))
            check_equal(evidence.main(["--probe", "role", "--json",
                                       "--state-root", str(state)]),
                        evidence.EXIT_OK,
                        f"the CLI still exits 0 with {token} in the registry")

    # Second layer: nothing non-finite survives the field readers, so no
    # hand-built or computed value can reintroduce one.
    for bad in (float("nan"), float("inf"), float("-inf")):
        check_equal(evidence._number_or_none(bad), None,
                    f"{bad!r} reads as unavailable")
    check_equal(evidence._number_or_none(288.783), 288.783,
                "a finite duration is kept")
    check_equal(evidence._number_or_none(True), None, "a bool is not a duration")

    # And the emitted document is STRICT JSON: no NaN, no Infinity.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        result = read(state, "role")
        strict = json.dumps(result, allow_nan=False)
        json.loads(strict, parse_constant=evidence._reject_json_constant)
        check(True, "the evidence document is strict, constant-free JSON")


def test_full_history_is_never_truncated() -> None:
    """No default limit silently drops known history."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", f"run-{index:03d}",
                     claimed_at=f"2026-08-{(index % 28) + 1:02d}T{index % 24:02d}:00:00Z")
            for index in range(60)
        ]
        runs.append(make_run("probe:chop", "unrelated"))
        state = build_state(Path(tmp), runs, {})
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("full history")
        check_equal(len(result["runs"]), 60, "every matching run is reported")
        claimed = [r["claimed_at"] for r in result["runs"]]
        check_equal(claimed, sorted(claimed, reverse=True),
                    "the whole history stays newest-first")


def test_presentation_only() -> None:
    """The reader is not wired into the lab's statistics."""
    source = Path(evidence.__file__).read_text(encoding="utf-8")
    check("import probe_census" not in source,
          "the reader does not import the census")
    check("import probe_flake" not in source,
          "the reader does not import the flake harness")
    check(not hasattr(evidence, "probe_census"),
          "the reader exposes no census handle")
    for forbidden in ("write_text", "write_bytes", "mkdir", "os.replace", "flock"):
        check(f"{forbidden}(" not in source,
              f"the reader contains no {forbidden} call")


def test_state_root_resolves_through_the_common_git_dir() -> None:
    """A linked worktree resolves to the MAIN checkout's git directory."""
    if shutil.which("git") is None:                       # pragma: no cover
        check(False, "git is available for the resolution case")
        return
    with tempfile.TemporaryDirectory() as tmp:
        main = Path(tmp) / "main"
        main.mkdir()
        env = dict(os.environ, GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_SYSTEM=os.devnull)

        def git(*args: str, cwd: Path = main) -> None:
            subprocess.run(["git", "-C", str(cwd), *args], check=True,
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, env=env)

        git("init", "-q", "-b", "main")
        git("config", "user.email", "test@example.com")
        git("config", "user.name", "Test")
        (main / "file.txt").write_text("x\n", encoding="utf-8")
        git("add", "file.txt")
        git("commit", "-qm", "seed")
        linked = Path(tmp) / "linked"
        git("worktree", "add", "-q", "--detach", str(linked))

        expected = (main / ".git" / evidence.STATE_DIRNAME).resolve()
        check_equal(evidence.resolve_state_root(main).resolve(), expected,
                    "the main checkout resolves to its own git dir")
        check_equal(evidence.resolve_state_root(linked).resolve(), expected,
                    "a linked worktree resolves to the MAIN git dir")
        check(not (linked / ".git").is_dir(),
              "the linked worktree's .git is a pointer file, not a directory")
        check(not expected.exists(), "resolution creates no state tree")

        outside = Path(tmp) / "not-a-repo"
        outside.mkdir()
        try:
            evidence.resolve_state_root(outside)
            check(False, "a non-repository is rejected")
        except evidence.EvidenceRejected as exc:
            check("common directory" in str(exc),
                  "the rejection explains what could not be resolved", str(exc))


def test_render_is_total() -> None:
    """Every field renders, including when all of them are unavailable."""
    with tempfile.TemporaryDirectory() as tmp:
        bare = {"test_id": "probe:role", "run_id": "bare", "status": "claimed"}
        state = build_state(Path(tmp), [bare], {})
        result = read(state, "role")
        text = evidence.render(result)
        check("bare" in text, "the run id renders")
        check(text.count("unavailable") >= 4,
              "every unavailable field renders as unavailable", text)
        check("makes no scheduling decision" in text,
              "the read-only note renders")
        json.dumps(result)  # raises if a value is not JSON-serializable
        check(True, "the evidence document is JSON-serializable")


def main() -> int:
    cases = [
        test_identity_mapping,
        test_both_identities_map_to_one_probe,
        test_a_measurement_run_is_the_same_probes_work,
        test_the_heartbeat_is_reported_raw,
        test_diagnostics_carry_the_state_they_concern,
        test_a_damaged_state_root_is_not_an_absent_one,
        test_an_unreadable_run_identity_is_record_damage,
        test_entry_state_separates_absent_from_unexaminable,
        test_unknown_key_is_rejected,
        test_exact_matching,
        test_clean_and_observed_reports,
        test_incomplete_run_reports_unavailable_not_false,
        test_mechanical_outcome_is_not_inferred_from_interpretation,
        test_missing_and_malformed_reports_are_non_fatal,
        test_an_existing_non_regular_report_is_damage_not_absence,
        test_report_reads_are_confined_to_the_reports_directory,
        test_a_symlinked_reports_directory_refuses_every_read,
        test_a_misplaced_reports_directory_refuses_every_read,
        test_the_registry_is_confined_to_the_state_root,
        test_a_malformed_report_path_never_aborts_the_read,
        test_absent_state_is_success_not_error,
        test_damaged_registry_is_non_fatal,
        test_non_finite_numbers_never_reach_the_output,
        test_full_history_is_never_truncated,
        test_presentation_only,
        test_state_root_resolves_through_the_common_git_dir,
        test_render_is_total,
    ]
    for case in cases:
        try:
            case()
        except Exception as exc:                          # noqa: BLE001
            FAILURES.append(f"{case.__name__} raised {type(exc).__name__}: {exc}")
    print(f"probe_external_evidence self-test: {PASSED} checks passed, "
          f"{len(FAILURES)} failed")
    for failure in FAILURES:
        print(f"  FAIL {failure}")
    return 1 if FAILURES else 0


if __name__ == "__main__":
    sys.exit(main())
