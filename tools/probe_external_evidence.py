#!/usr/bin/env python3
"""Read the Codex `$test` record for one registered probe, read-only (#1432).

The Codex `$test` skill has been independently recording coordinated,
non-CI runs against this same probe set, with exact commit provenance
and interpreted observations. The de-flake lab (`tools/probe_flake.py`,
`tools/probe_census.py`) knows nothing about them. This is the reader
that lets it SHOW them.

The two systems run side by side and MUST NOT INTERACT. This module is
strictly a reader:

* It never writes, anywhere. Not the registry, not a report, not a lock,
  not a directory — resolving the state root never creates it.
* It never takes a `$test` lock, and it never invokes the `$test`
  coordinator at all. That is not squeamishness: every one of the
  issue's four permitted read subcommands (`list`, `show`,
  `proposal-list`, `value-status`) goes through the coordinator's own
  `read_registry` -> `locked_registry`, which takes the EXCLUSIVE
  `registry.lock` flock, `mkdir`s the state tree, and rewrites
  `registry.json` with a fresh `updated_at` on the way out. Invoking one
  would take a lock and change authoritative bytes, both of which the
  issue forbids. Reading the JSON directly is the only way to honour the
  permission boundary, and it is also the only way that works when the
  machine-local coordinator is not installed.
* Every entry it opens is confined to the state tree. `registry.json`
  and `reports/` are each resolved and then required to still be an
  immediate child of the RESOLVED state root, of the right kind — a
  symlink at either fixed name would otherwise relocate what gets read
  while the name still looked right. A report is then read only when its
  own recorded path resolves to a regular `*.test-result.md` file
  directly beneath that validated `reports/`. A recorded path is data,
  never an authority to widen read scope.

**External evidence is presentation-only.** A `$test` run appearing,
disappearing, passing, failing or recording observations changes no
census sample, no statistic, no schedule and no skip decision. This
module imports `run_probes` only to validate a probe key, and imports
`probe_census` not at all. One interpreted `$test` run is CONTEXT, not
a measurement in the lab's statistics.

State location
--------------
`<git-common-dir>/codex-test`, resolved with `git rev-parse
--git-common-dir` — never a literal `<checkout>/.git/codex-test`, since
in a linked worktree `.git` is a pointer FILE and the shared registry
lives under the main checkout's git directory. That matches the `$test`
skill's own contract.

That tree is untracked and machine-local. It is absent on a fresh clone,
on another machine, and wherever Codex is not installed. **Absence is a
normal "no external evidence" result** — exit 0, an empty run list, no
diagnostic. Damage is different: an existing but unreadable or malformed
registry or report produces a non-fatal DIAGNOSTIC. Neither condition
fails the read, suppresses the probe, or reclassifies anything.

Identity
--------
`run_probes.PROBES` keys are the canonical input. A key maps to the
`$test` run identifier by underscores-to-hyphens under the `probe:`
namespace — `transfer_order` -> `probe:transfer-order`. It is derived
from the KEY, never from the script filename: `persistence_contract_sweep`
is registered as `persistence_contract_sweep.py`, with no `_probe`
suffix to strip. Matching is EXACT; `probe:transfer-order-extra`,
`probe:transfer_order` and `gameplay:transfer-order` are all non-matches,
and a probe key that is not in `run_probes.PROBES` is a controlled
unknown-key rejection, NOT a "no external evidence" answer.

Reporting
---------
For every exact match this reports the run id and state, the tested
commit from the run's recorded provenance, the MECHANICAL execution
outcome, the recorded duration, and the observation status. The
mechanical outcome comes from the registry's own `execution_status` /
`test_exit_code` and is never inferred from the report's interpretation
— the `$test` contract separates command execution from interpretation
on purpose. Active, legacy and partially recorded runs are surfaced
rather than dropped: a value the record does not carry is reported as
UNAVAILABLE (`null` in JSON), never as a fabricated `false` or `0`. The
full known history is reported, always; there is no limit option and no
default truncation.

Usage:
  python3 tools/probe_external_evidence.py --probe role
  python3 tools/probe_external_evidence.py --probe transfer_order --json

Exit codes:
  0  the read completed — including the absent-state "no external
     evidence" result, and including a read that emitted non-fatal
     diagnostics about damaged external state
  2  rejected input or an unusable environment: an unknown probe key,
     a state root that cannot be resolved, and argparse's own usage
     errors

Gate: `python3 tools/test_probe_external_evidence.py` (deterministic,
synthetic, offline). Like the rest of the de-flake lab's self-tests
(`test_probe_census.py`, `test_probe_flake.py`) it is run on demand and
is deliberately not a CI step: everything it covers is optional,
machine-local state that CI does not have. A live parse against a real
registry is manual PR evidence, not a portable gate.
"""
from __future__ import annotations

import argparse
import json
import math
import os
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import run_probes  # noqa: E402

EVIDENCE_SCHEMA = "probe-external-evidence/v1"

# The `$test` coordinator's own names for its state, mirrored here
# because this reader deliberately does not import or execute it.
STATE_DIRNAME = "codex-test"
REGISTRY_FILENAME = "registry.json"
REPORTS_DIRNAME = "reports"
REPORT_SUFFIX = ".test-result.md"
COORDINATOR_SCHEMA = "codex-test-coordinator/v1"

# `probe:<key with hyphens>`. The `$test` skill also records
# `gameplay:*` and `playtest:*` runs; those namespaces are never a
# probe's evidence and never match.
TEST_ID_NAMESPACE = "probe:"

# Observation status, kept a tri-state on purpose: a run whose
# interpretation has not happened yet must not read as "no observations".
OBSERVATIONS_RECORDED = "recorded"
OBSERVATIONS_NONE = "none-recorded"
OBSERVATIONS_UNAVAILABLE = "unavailable"

# Why a run's report was not read. Only the last two are damage.
REPORT_AVAILABLE = "available"
REPORT_NOT_RECORDED = "not-recorded"
REPORT_ABSENT = "absent"
REPORT_UNREADABLE = "unreadable"
REPORT_OUT_OF_SCOPE = "out-of-scope"

STATE_PRESENT = "present"
STATE_ABSENT = "absent"

EXIT_OK = 0
EXIT_REJECTED = 2


class EvidenceRejected(Exception):
    """A controlled rejection: bad input or an unusable environment."""


# Every filesystem call below catches `ValueError` beside `OSError`. A
# registry field is arbitrary external text, and a path built from a
# string containing an embedded NUL raises `ValueError` — not `OSError`
# — from `resolve`, `stat` and `subprocess`. Catching only `OSError`
# would turn one malformed record into a traceback that aborts the whole
# read and takes every later valid run with it.
PATH_ERRORS = (OSError, ValueError)


# --------------------------------------------------------------------------
# Identity
# --------------------------------------------------------------------------

def probe_keys() -> list[str]:
    """Every registered probe key, in registry order."""
    return [key for key, _script, _purpose in run_probes.PROBES]


def probe_script(probe_key: str) -> str | None:
    """The registered script filename for `probe_key`, or None."""
    for key, script, _purpose in run_probes.PROBES:
        if key == probe_key:
            return script
    return None


def test_id_for_probe(probe_key: str) -> str:
    """The `$test` run identifier a registered probe key maps to.

    Derived from the KEY, never the script filename — `_probe.py` is not
    a reliable suffix (`persistence_contract_sweep.py`).
    """
    return TEST_ID_NAMESPACE + probe_key.replace("_", "-")


def require_known_probe(probe_key: str) -> None:
    """Reject a key `run_probes.PROBES` does not register.

    Deliberately distinct from the absent-state result: an unknown key
    is a caller mistake, not evidence that no external runs exist.
    """
    if probe_key in set(probe_keys()):
        return
    raise EvidenceRejected(
        f"unknown probe key {probe_key!r}: it is not registered in "
        f"run_probes.PROBES. Run `python3 tools/run_probes.py --list` "
        f"for the registered keys."
    )


# --------------------------------------------------------------------------
# State location
# --------------------------------------------------------------------------

def resolve_state_root(repo: str | os.PathLike[str] | None = None) -> Path:
    """`<git-common-dir>/codex-test` for `repo` (default: this checkout).

    Uses `git rev-parse --git-common-dir` rather than a literal
    `.git/codex-test`, because in a linked worktree `.git` is a pointer
    file and the shared `$test` state lives under the main checkout's
    git directory. Never creates anything.
    """
    start = Path(repo) if repo is not None else Path(run_probes.REPO_ROOT)
    try:
        completed = subprocess.run(
            ["git", "-C", str(start), "rev-parse", "--git-common-dir"],
            text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            check=False,
        )
    except PATH_ERRORS as exc:                              # no git binary, bad path
        raise EvidenceRejected(f"cannot run git to locate $test state: {exc}") from exc
    if completed.returncode != 0:
        detail = completed.stderr.strip() or f"git exited {completed.returncode}"
        raise EvidenceRejected(
            f"cannot resolve the git common directory from {start}: {detail}"
        )
    common = Path(completed.stdout.strip())
    if not common.is_absolute():
        common = Path(start) / common
    return common.resolve() / STATE_DIRNAME


# --------------------------------------------------------------------------
# Reading
# --------------------------------------------------------------------------

def _text_or_none(value: object) -> str | None:
    """`value` as a non-empty string, or None when it is unavailable."""
    if isinstance(value, str) and value.strip():
        return value
    return None


def _number_or_none(value: object) -> float | int | None:
    """`value` as a FINITE real number, or None when it is unavailable.

    `bool` is excluded deliberately: a duration is never True. So are
    NaN and the infinities — Python's `json` reads the non-standard
    `NaN`/`Infinity`/`-Infinity` constants happily and writes them back
    out, which would make this reader's own `--json` invalid JSON. The
    registry is rejected outright when it carries one (see
    `_reject_json_constant`); this is the second layer, so no arithmetic
    or hand-built value can reintroduce one either.
    """
    if isinstance(value, bool):
        return None
    if isinstance(value, (int, float)) and math.isfinite(value):
        return value
    return None


def _integer_or_none(value: object) -> int | None:
    if isinstance(value, bool) or not isinstance(value, int):
        return None
    return value


def _observation_status(record: dict) -> str:
    """Whether this run RECORDED observations, from the registry alone.

    `clean` and `observations` are the coordinator's two interpreted
    outcomes; everything else (`pending`, `blocked`, `inconclusive`, a
    missing field, a legacy value) is unavailable rather than "none".
    """
    outcome = _text_or_none(record.get("interpretation_outcome"))
    if outcome == "observations":
        return OBSERVATIONS_RECORDED
    if outcome == "clean":
        return OBSERVATIONS_NONE
    return OBSERVATIONS_UNAVAILABLE


def _parse_report(text: str) -> dict:
    """The few facts this reader takes from a `*.test-result.md` body.

    Treated strictly as data. The frontmatter is scanned line-wise for
    `interpretation_status` instead of being handed to a YAML parser,
    and observations are counted by their `### OBS-` section headings.
    """
    interpretation = None
    observation_count = 0
    in_frontmatter = False
    frontmatter_done = False
    for index, raw in enumerate(text.splitlines()):
        line = raw.rstrip()
        if index == 0 and line == "---":
            in_frontmatter = True
            continue
        if in_frontmatter:
            if line == "---":
                in_frontmatter = False
                frontmatter_done = True
                continue
            key, separator, value = line.partition(":")
            if separator and key.strip() == "interpretation_status":
                interpretation = value.strip().strip('"').strip("'") or None
            continue
        if line.startswith("### OBS-"):
            observation_count += 1
    return {
        "interpretation_status": interpretation,
        "observation_count": observation_count,
        "frontmatter_parsed": frontmatter_done,
    }


def _confined_child(root: Path, name: str) -> tuple[Path | None, str | None]:
    """Resolve `<root>/<name>`, confined to the RESOLVED state root.

    Every entry this reader opens is a fixed, named child of the state
    tree, so each one is resolved and then required to still BE that
    child. Without this, a symlink at the fixed name silently relocates
    what gets read while the name itself still looks right. Returns
    `(path, None)` when the entry may be used, or `(None, refusal)`.
    Non-existence is not a refusal — the callers below mean different
    things by an absent registry and an absent reports directory.
    """
    try:
        root_resolved = root.resolve()
        resolved = (root / name).resolve()
    except PATH_ERRORS as exc:
        return None, f"cannot resolve {root / name}: {exc}"
    if resolved.parent != root_resolved or resolved.name != name:
        return None, (f"{root / name} resolves to {resolved}, which is not an "
                      f"immediate child of the state root {root_resolved}")
    return resolved, None


def resolve_registry_path(root: Path, diagnostics: list[str]) -> Path | None:
    """The one registry file this reader may open, or None when untrusted."""
    resolved, refusal = _confined_child(root, REGISTRY_FILENAME)
    if refusal is not None:
        diagnostics.append(f"refused to read the registry: {refusal}")
        return None
    return resolved


def resolve_reports_scope(root: Path, diagnostics: list[str]) -> Path | None:
    """The ONE directory report reads may touch, or None when untrusted.

    Checking each recorded path against `<root>/reports` is not enough
    on its own: if `reports` is ITSELF a symlink to somewhere else, that
    check keeps passing while the whole read scope has quietly moved out
    of the state tree. So the scope is trusted only when it resolves to
    an immediate child of the RESOLVED state root, under its own name,
    and is a directory when it exists at all. Anything else refuses
    every report read with one diagnostic. A reports directory that is
    simply ABSENT is not a refusal: each recorded report then reports
    itself absent, which is what a missing report means.
    """
    scope, refusal = _confined_child(root, REPORTS_DIRNAME)
    if refusal is not None:
        diagnostics.append(f"refused to read any report: {refusal}")
        return None
    try:
        misplaced = scope.exists() and not scope.is_dir()
    except PATH_ERRORS as exc:
        diagnostics.append(
            f"refused to read any report: cannot stat {scope}: {exc}")
        return None
    if misplaced:
        diagnostics.append(
            f"refused to read any report: {root / REPORTS_DIRNAME} exists but is "
            f"not a directory"
        )
        return None
    return scope


def _read_report(record: dict, scope: Path | None, diagnostics: list[str]) -> dict:
    """Read this run's report, if it is recorded and in scope.

    Read scope is confined to resolved regular files named
    `*.test-result.md` directly beneath `scope`, the validated reports
    directory from `resolve_reports_scope`. A recorded path that escapes
    it — traversal, an absolute path elsewhere, a symlink out of the
    tree — is refused and diagnosed, never followed. A `scope` of None
    means the reports directory itself was not trustworthy, which is
    already diagnosed once at the directory level.
    """
    recorded = _text_or_none(record.get("report_path"))
    report: dict = {
        "path": recorded,
        "status": REPORT_NOT_RECORDED,
        "interpretation_status": None,
        "observation_count": None,
    }
    if recorded is None:
        return report
    if scope is None:
        report["status"] = REPORT_OUT_OF_SCOPE
        return report

    run_id = _text_or_none(record.get("run_id")) or "<unidentified run>"
    try:
        candidate = Path(recorded)
        if not candidate.is_absolute():
            candidate = scope / candidate
        resolved = candidate.resolve()
    except PATH_ERRORS as exc:
        report["status"] = REPORT_UNREADABLE
        diagnostics.append(f"{run_id}: cannot resolve report path {recorded!r}: {exc}")
        return report

    in_scope = resolved.parent == scope and resolved.name.endswith(REPORT_SUFFIX)
    if not in_scope:
        report["status"] = REPORT_OUT_OF_SCOPE
        diagnostics.append(
            f"{run_id}: refused to read report {recorded!r}: it does not resolve to a "
            f"{REPORT_SUFFIX} file directly under {scope}"
        )
        return report

    # A path that is simply not there is DATA: a run whose report has not
    # been written yet, or was cleaned up. A path that EXISTS but is not
    # a regular file — a directory named `*.test-result.md`, a socket, a
    # device — is damaged external state, and damage is diagnosed.
    try:
        exists = resolved.exists()
        is_file = resolved.is_file()
    except PATH_ERRORS as exc:
        report["status"] = REPORT_UNREADABLE
        diagnostics.append(f"{run_id}: cannot stat report {resolved}: {exc}")
        return report
    if not is_file:
        if not exists:
            report["status"] = REPORT_ABSENT
            return report
        report["status"] = REPORT_UNREADABLE
        diagnostics.append(
            f"{run_id}: cannot read report {resolved}: it exists but is not a "
            f"regular file"
        )
        return report

    try:
        text = resolved.read_text(encoding="utf-8")
    except (*PATH_ERRORS, UnicodeDecodeError) as exc:
        report["status"] = REPORT_UNREADABLE
        diagnostics.append(f"{run_id}: cannot read report {resolved}: {exc}")
        return report

    parsed = _parse_report(text)
    if not parsed["frontmatter_parsed"]:
        diagnostics.append(
            f"{run_id}: report {resolved} has no closed `---` frontmatter block; "
            f"reporting what could be read from it"
        )
    report["status"] = REPORT_AVAILABLE
    report["interpretation_status"] = parsed["interpretation_status"]
    report["observation_count"] = parsed["observation_count"]
    return report


def _summarize_run(record: dict, scope: Path | None, diagnostics: list[str]) -> dict:
    """One matching `$test` run, with every unavailable value as None."""
    return {
        "run_id": _text_or_none(record.get("run_id")),
        "run_state": _text_or_none(record.get("status")),
        "area": _text_or_none(record.get("area")),
        "tested_commit": _text_or_none(record.get("revision")),
        "tested_commit_subject": _text_or_none(record.get("revision_subject")),
        "tested_commit_time": _text_or_none(record.get("revision_committed_at")),
        # Mechanical only. Never derived from the report's interpretation.
        "execution_status": _text_or_none(record.get("execution_status")),
        "exit_code": _integer_or_none(record.get("test_exit_code")),
        "duration_seconds": _number_or_none(record.get("elapsed_seconds")),
        "claimed_at": _text_or_none(record.get("claimed_at")),
        "completed_at": _text_or_none(record.get("completed_at")),
        "observations": _observation_status(record),
        "interpretation_outcome": _text_or_none(record.get("interpretation_outcome")),
        "report": _read_report(record, scope, diagnostics),
    }


def _reject_json_constant(token: str) -> None:
    """Refuse JSON's non-standard `NaN`/`Infinity`/`-Infinity` constants.

    `json.loads` accepts them by default and `json.dumps` writes them
    back, so a registry carrying one would flow through this reader and
    make its own `--json` output invalid JSON — presented as evidence,
    with no diagnostic. A document containing one is malformed external
    state, and malformed state is diagnosed.
    """
    raise ValueError(f"non-standard JSON constant {token}")


def _load_runs(registry_path: Path, diagnostics: list[str]) -> list[dict]:
    """The registry's run list, or [] with a diagnostic when unusable."""
    try:
        exists = registry_path.exists()
        is_file = registry_path.is_file()
    except PATH_ERRORS as exc:
        diagnostics.append(f"cannot stat {registry_path}: {exc}")
        return []
    if not is_file:
        if not exists:
            diagnostics.append(
                f"$test state exists but {registry_path} does not; reporting no runs"
            )
        else:
            diagnostics.append(
                f"cannot read {registry_path}: it exists but is not a regular file"
            )
        return []
    try:
        text = registry_path.read_text(encoding="utf-8")
    except (*PATH_ERRORS, UnicodeDecodeError) as exc:
        diagnostics.append(f"cannot read {registry_path}: {exc}")
        return []
    try:
        document = json.loads(text, parse_constant=_reject_json_constant)
    except ValueError as exc:                  # JSONDecodeError is a ValueError
        diagnostics.append(f"cannot parse {registry_path}: {exc}")
        return []
    if not isinstance(document, dict):
        diagnostics.append(f"{registry_path} is not a JSON object; reporting no runs")
        return []
    schema = document.get("schema")
    if schema != COORDINATOR_SCHEMA:
        diagnostics.append(
            f"{registry_path} declares schema {schema!r}, not {COORDINATOR_SCHEMA!r}; "
            f"reading it anyway, on a best-effort basis"
        )
    runs = document.get("runs")
    if not isinstance(runs, list):
        diagnostics.append(f"{registry_path} has no `runs` list; reporting no runs")
        return []
    return runs


def read_probe_evidence(probe_key: str,
                        *,
                        state_root: str | os.PathLike[str] | None = None,
                        repo: str | os.PathLike[str] | None = None) -> dict:
    """What the Codex `$test` record knows about one registered probe.

    Read-only and non-fatal: an absent state tree is a successful "no
    external evidence" result, and damaged state is reported as a
    diagnostic beside whatever could still be read. Raises
    `EvidenceRejected` only for an unknown probe key or a state root
    that cannot be resolved at all.
    """
    require_known_probe(probe_key)
    root = Path(state_root) if state_root is not None else resolve_state_root(repo)
    test_id = test_id_for_probe(probe_key)

    evidence: dict = {
        "schema": EVIDENCE_SCHEMA,
        "probe": probe_key,
        "script": probe_script(probe_key),
        "test_id": test_id,
        "state_root": str(root),
        "state": STATE_ABSENT,
        "runs": [],
        "diagnostics": [],
    }
    try:
        present = root.is_dir()
    except PATH_ERRORS as exc:
        raise EvidenceRejected(f"cannot stat the $test state root {root}: {exc}") from exc
    if not present:
        return evidence

    evidence["state"] = STATE_PRESENT
    diagnostics: list[str] = evidence["diagnostics"]
    registry_path = resolve_registry_path(root, diagnostics)
    records = _load_runs(registry_path, diagnostics) if registry_path else []
    scope = resolve_reports_scope(root, diagnostics)

    matches = []
    for index, record in enumerate(records):
        if not isinstance(record, dict):
            diagnostics.append(f"registry run #{index} is not an object; skipped")
            continue
        if record.get("test_id") != test_id:
            continue
        matches.append(record)

    # Newest first, by claim time — the coordinator's own ordering. The
    # WHOLE known history is reported; there is deliberately no limit.
    matches.sort(key=lambda record: _text_or_none(record.get("claimed_at")) or "",
                 reverse=True)
    evidence["runs"] = [_summarize_run(r, scope, diagnostics) for r in matches]
    return evidence


# --------------------------------------------------------------------------
# Presentation
# --------------------------------------------------------------------------

def _format_duration(seconds: float | int | None) -> str:
    if seconds is None:
        return "unavailable"
    return f"{float(seconds):.1f}s"


def _format_report(report: dict) -> str:
    status = report["status"]
    if status != REPORT_AVAILABLE:
        return status
    count = report["observation_count"]
    return f"available ({count} OBS)"


def render(evidence: dict) -> str:
    """The human-readable rendering of one `read_probe_evidence` result."""
    lines = [
        f"$test evidence for probe {evidence['probe']} "
        f"({evidence['script']}) -> {evidence['test_id']}",
        f"  state root: {evidence['state_root']} [{evidence['state']}]",
    ]
    if evidence["state"] == STATE_ABSENT:
        lines.append("  no external evidence: the $test state tree is not present "
                     "on this machine.")
    elif not evidence["runs"]:
        lines.append("  no external evidence: no $test run matches this probe.")
    else:
        lines.append(f"  {len(evidence['runs'])} run(s), newest first:")
        for run in evidence["runs"]:
            lines.append(f"    {run['run_id'] or 'unavailable'}  [{run['run_state'] or 'unavailable'}]")
            lines.append(f"      tested commit:  {run['tested_commit'] or 'unavailable'}"
                         + (f"  {run['tested_commit_subject']}"
                            if run["tested_commit_subject"] else ""))
            exit_code = run["exit_code"]
            lines.append(f"      execution:      {run['execution_status'] or 'unavailable'}"
                         f" (exit {exit_code if exit_code is not None else 'unavailable'})")
            lines.append(f"      duration:       {_format_duration(run['duration_seconds'])}")
            lines.append(f"      observations:   {run['observations']}"
                         f"  report: {_format_report(run['report'])}")
    for diagnostic in evidence["diagnostics"]:
        lines.append(f"  diagnostic: {diagnostic}")
    lines.append("  (presentation only: $test evidence is never a census sample)")
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Read the Codex $test record for one registered probe "
                    "(read-only; never writes, locks, or invokes the coordinator).")
    parser.add_argument("--probe", required=True,
                        help="a registered run_probes.PROBES key, e.g. role")
    parser.add_argument("--json", action="store_true",
                        help="emit the evidence document instead of a table")
    parser.add_argument("--state-root", default=None,
                        help="override the resolved <git-common-dir>/codex-test path")
    args = parser.parse_args(argv)

    try:
        evidence = read_probe_evidence(args.probe, state_root=args.state_root)
    except EvidenceRejected as exc:
        print(f"probe_external_evidence: {exc}", file=sys.stderr)
        return EXIT_REJECTED

    if args.json:
        # allow_nan=False so this can never emit invalid JSON: by here
        # nothing non-finite can have survived, and the exit path below
        # would rather fail loudly than print a `NaN` as data.
        print(json.dumps(evidence, indent=2, sort_keys=True, allow_nan=False))
    else:
        print(render(evidence))
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
