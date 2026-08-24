#!/usr/bin/env python3
"""`/deflake` diagnosis: the mechanics of turning a measurement into a fix (#1437).

`tools/deflake.py` (#1436) answers "how often does this probe fail".
That is a NUMBER, and a number is not a fix. This module owns the step
after it: given one complete measurement handoff, decide mechanically
whether the evidence supports a probe-side repair — and, when it does
not, which declared non-repair route the invocation must take instead.

    python3 tools/deflake_diagnosis.py --diagnosis <document.json>
    python3 tools/deflake_diagnosis.py --manifest <checkout-root>
    python3 tools/test_deflake_diagnosis.py       # the deterministic gate

Nothing here boots an engine, runs a probe, opens a port, edits a file
in a worktree, or talks to GitHub. It reads documents and answers
questions about them. The expensive half — twenty real probe runs across
two worktrees — is performed by `tools/probe_flake.py` exactly as
`/deflake` already performs ten, and its retained
`probe-flake-result/v1` documents are this module's input.

Why a tracked module rather than skill prose
--------------------------------------------
The `/deflake` workflow surface is not tracked in this repository
(`.gitignore` ignores `.claude/`), so a repository test cannot cover
prose. The rules below are the MECHANICAL half — the ones a test can
hold to — and they live here so `tools/test_deflake_diagnosis.py` can
assert them:

* the entry gate over a #1436 handoff, including one-probe enforcement;
* X-out-of-10 arithmetic, taken from `probe_census` rather than restated;
* the configuration manifest, including CONFIRMED ABSENCE;
* MISSING evaluation and stable check identity;
* same-environment verification acceptance and the no-retry rule;
* the required diagnosis evidence and preservation attestations; and
* the one-PR limit.

The JUDGEMENT half stays with the agent running the workflow: whether a
diagnosis is convincing, whether a repair is minimal, whether an
expectation is genuinely obsolete. Those are reviewer questions and this
module does not pretend to answer them. What it does instead is refuse a
route whose machine-checkable evidence is absent — a repair with no
stated cause, no evidence, or no preservation attestation cannot be
declared at all.

The entry gate
--------------
One invocation consumes exactly ONE handoff naming exactly ONE probe.
`deflake-handoff/v1` is deliberately thin: it carries the probe key, X,
the pointers to the retained artifacts, and the invocation and
configuration records — and it EMBEDS or REFERENCES the original
`probe-flake-result/v1` document rather than paraphrasing it. That is
not tidiness. `probe_census.ingest_result` deliberately drops ports,
per-run check maps, descriptor labels, the artifact root, the invocation
directory and the exact command, so a handoff reconstructed from the
durable census row alone cannot identify the baseline invocation, and
this gate refuses it.

Descriptor equality, not identifier normalization
-------------------------------------------------
`probe-result/v1` already requires STATIC descriptor identifiers and
puts every runtime value in an event's `detail`
(`tools/probe_protocol.py`). So a per-run undeclared or changing
identifier is a MALFORMED protocol result — a rejected handoff — and
never a diagnosis outcome to be scored. This module compares
descriptors for exact equality (identifiers and order) and refuses
anything else, rather than inventing the normalization pass that is
already this issue's declared non-goal.

The scoped MISSING rule
-----------------------
Probes abort. A run that FAILs check three and stops cannot, by
construction, emit checks four onward, and `probe_protocol` records
those as MISSING. Demanding "every expected check in every run" would
therefore be unsatisfiable for any probe with an X above zero, so the
rule is scoped:

* every TARGET check identifier has zero MISSING across all ten runs;
* every PASSING run emits every expected check, MISSING zero;
* an accepted failing or timed-out run may omit only checks after its
  own abort point — and because `probe_protocol` enforces declared
  ORDER, "after the abort point" is exactly "a contiguous suffix of the
  declared order", which is checked rather than assumed; and
* no expected identifier disappears from the batch as a whole.

For X=0 no failing run is accepted, so this collapses to the strict
formulation and the default case is unchanged.

Same environment means the same MEASUREMENT, not the same characters
--------------------------------------------------------------------
The baseline and the verification batch necessarily differ in the
worktree they run from and in where they write, and `probe_flake` leases
ports dynamically. So "the same invocation" is compared on
BEHAVIOR-AFFECTING settings with effective defaults filled in — probe,
run count, RTS capabilities, timeout, retry policy — while `--result`,
`--artifact-root` and the observed ports are recorded and permitted to
differ. Both destinations must still sit outside every worktree: the
artifact root is guarded by `probe_flake.check_artifact_root`, but
`--result` is written wherever it is pointed, and a result document
inside the repair worktree would either enter the commit or wedge the
drainer's cleanup.

The repair is frozen before it is verified
------------------------------------------
`probe_flake` records `git rev-parse HEAD` and cannot see uncommitted
source changes, so a verification batch run against a dirty worktree
measures something no commit contains. A declared repair therefore
requires a source-clean repair worktree and a verification result whose
`commit_sha` equals the repair commit being proposed.

Weakening an assertion is never a fix
-------------------------------------
The shapes a machine can see are refused here: a descriptor that lost or
renamed an identifier, a target check that became MISSING, a run count
below the policy, a retry policy that lets any passing attempt count,
and a repair declared without the three preservation attestations.
Whether a surviving assertion was quietly BROADENED is a reviewer
judgement this module cannot make, and it says so rather than implying
coverage it does not have.

Routes
------
Exactly one per invocation, and only the first opens a pull request:

* `repair-pr` — a confidently diagnosed, successfully verified
  probe-side repair. One PR, one probe, one root cause.
* `handoff-rejected` — the entry gate refused. No code change, no PR.
* `cannot-reproduce` — the controlled baseline did not reproduce an
  over-X result. Handed off to #1439.
* `production-defect` — the assertion is right and the product is
  wrong. The probe is not touched; handed off to #1438.
* `no-confident-fix` — several failures with no one established
  probe-side cause. Handed off to #1439.
* `partial-improvement` — the repaired batch improved but stayed above
  X, or the batch became invalid. Handed off to #1439.

Emitting a handoff here means emitting `deflake-diagnosis-outcome/v1`
naming the route, the owning issue and the retained evidence. What #1438
and #1439 then DO with it is theirs to define; this module does not
invent their contracts, and filing an issue is not its job.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402

HANDOFF_SCHEMA = "deflake-handoff/v1"
DIAGNOSIS_SCHEMA = "deflake-diagnosis/v1"
MANIFEST_SCHEMA = "deflake-config-manifest/v1"
OUTCOME_SCHEMA = "deflake-diagnosis-outcome/v1"

# The measurement contract, taken from the modules that own it rather
# than restated. A private copy that drifted would let this module
# accept a batch `/deflake` and the census would both reject.
RUN_COUNT = probe_census.POLICY_RUN_COUNT
RTS_CAPABILITIES = probe_flake.DEFAULT_RTS_CAPS

# The gitignored per-worktree configuration family (`.gitignore`). Probes
# symlink the invoking worktree's `config/` into their isolated resource
# roots, so two comparison worktrees that disagree here are measuring two
# different conditions. An EMPTY manifest is the expected default, not an
# edge case, and it is recorded explicitly so absence compares as
# rigorously as contents do.
CONFIG_GLOB = "config/*.local.yaml"

# `/deflake`'s own N and capability count, spelled as the thing every
# supplied invocation is measured against.
SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
COMMIT_RE = re.compile(r"^[0-9a-f]{40}$")

# Command-line options whose VALUE is a destination rather than a
# condition. Baseline and verification must differ here — writing both
# batches to one path would destroy the comparison — so they are dropped
# before two invocations are compared, and validated separately.
DESTINATION_OPTIONS = ("--result", "--artifact-root")

# Every behavior-affecting setting an invocation is compared on, with
# the effective default `probe_flake` would apply when it is absent.
# `--runs` and `--probe` have no default: `probe_flake.main` requires
# both, so an invocation missing one never ran.
INVOCATION_DEFAULTS = {
    "--rts-caps": RTS_CAPABILITIES,
    "--timeout": probe_flake.DEFAULT_TIMEOUT,
}

# The probe-side defect categories of the issue's diagnosis boundary.
# A declared repair names exactly one: "several independent causes" is
# the `no-confident-fix` route, not a list here.
CAUSE_CATEGORIES = (
    "fixture-construction",
    "setup-precondition",
    "competing-actor",
    "observation",
    "oracle-wiring",
    "check-identity",
    "fixture-discovery",
    "obsolete-expectation",
)

# The three preservation claims a repair must make explicitly. They are
# ATTESTATIONS, not proofs: the machine checks the shapes it can see
# (identifiers, MISSING, run counts, retries) and requires the agent to
# state the rest on the record, so a reviewer knows what was claimed.
ATTESTATIONS = (
    "product_behavior_preserved",
    "check_identities_preserved",
    "no_assertion_weakened",
)

ROUTE_REPAIR = "repair-pr"
ROUTE_HANDOFF_REJECTED = "handoff-rejected"
ROUTE_CANNOT_REPRODUCE = "cannot-reproduce"
ROUTE_PRODUCTION_DEFECT = "production-defect"
ROUTE_NO_CONFIDENT_FIX = "no-confident-fix"
ROUTE_PARTIAL_IMPROVEMENT = "partial-improvement"

ROUTES = (ROUTE_REPAIR, ROUTE_HANDOFF_REJECTED, ROUTE_CANNOT_REPRODUCE,
          ROUTE_PRODUCTION_DEFECT, ROUTE_NO_CONFIDENT_FIX,
          ROUTE_PARTIAL_IMPROVEMENT)

# Which issue owns each non-repair route's handoff. `repair-pr` owns its
# own standalone pull request and `handoff-rejected` hands off to nobody
# — the gate refused before any work began, and there is nothing to
# report onward.
ROUTE_OWNER = {
    ROUTE_REPAIR: None,
    ROUTE_HANDOFF_REJECTED: None,
    ROUTE_CANNOT_REPRODUCE: 1439,
    ROUTE_PRODUCTION_DEFECT: 1438,
    ROUTE_NO_CONFIDENT_FIX: 1439,
    ROUTE_PARTIAL_IMPROVEMENT: 1439,
}

# The one route that may touch the probe's source. Every other route
# stops without a repair PR and emits its declared handoff.
ROUTES_THAT_CHANGE_CODE = frozenset({ROUTE_REPAIR})

EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_REFUSED = 3


class HandoffError(Exception):
    """A handoff or diagnosis document the entry gate refuses.

    Distinct from a diagnosis ROUTE on purpose: a malformed input never
    reached a diagnosis, so it is not a `cannot-reproduce` or a
    `no-confident-fix` and must not be recorded as one.
    """


class RouteRefused(Exception):
    """A well-formed document whose declared route the evidence denies."""


# ==========================================================================
# Configuration manifest
# ==========================================================================
def _digest(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(65536), b""):
            digest.update(block)
    return digest.hexdigest()


def config_manifest(root) -> dict:
    """The `config/*.local.yaml` state of one checkout, as a document.

    Sorted by relative path so two manifests compare as data rather than
    as directory order, and an empty `entries` list is a POSITIVE
    statement that the family is absent — which is the expected default
    in this repository, since none of the four paths is tracked.
    """
    base = Path(root).expanduser().resolve()
    entries = []
    for path in sorted(base.glob(CONFIG_GLOB)):
        if not path.is_file():
            continue
        entries.append({
            "path": path.relative_to(base).as_posix(),
            "sha256": _digest(path),
        })
    return {"schema": MANIFEST_SCHEMA, "root": str(base), "entries": entries}


def require_manifest(document, what: str) -> dict:
    """`document` as a valid manifest, or the refusal that names it."""
    if not isinstance(document, dict):
        raise HandoffError(
            f"{what} must be a JSON object, got {type(document).__name__}")
    schema = document.get("schema")
    if schema != MANIFEST_SCHEMA:
        raise HandoffError(
            f"{what} is {schema!r}, expected {MANIFEST_SCHEMA!r}")
    entries = document.get("entries")
    if not isinstance(entries, list):
        raise HandoffError(
            f"{what} has no `entries` list; record an empty list to state "
            f"that no {CONFIG_GLOB} file exists, rather than omitting it — "
            f"absence has to be asserted, not inferred from a missing key")
    seen = set()
    for position, entry in enumerate(entries):
        where = f"{what} entry {position}"
        if not isinstance(entry, dict):
            raise HandoffError(f"{where} must be a JSON object")
        relative = entry.get("path")
        if not isinstance(relative, str) or not relative:
            raise HandoffError(f"{where} has no `path`")
        if relative in seen:
            raise HandoffError(f"{where} repeats the path {relative!r}")
        seen.add(relative)
        digest = entry.get("sha256")
        if not isinstance(digest, str) or not SHA256_RE.match(digest):
            raise HandoffError(
                f"{where} has no SHA-256 digest for {relative!r}, got "
                f"{digest!r}")
    return document


def manifest_entries(document) -> dict:
    return {entry["path"]: entry["sha256"] for entry in document["entries"]}


def manifest_differences(left, right, *, left_name: str,
                         right_name: str) -> list:
    """Every way two configuration manifests disagree, absence included."""
    a = manifest_entries(left)
    b = manifest_entries(right)
    problems = []
    for path in sorted(set(a) - set(b)):
        problems.append(
            f"{path} exists in {left_name} and is absent from {right_name}")
    for path in sorted(set(b) - set(a)):
        problems.append(
            f"{path} exists in {right_name} and is absent from {left_name}")
    for path in sorted(set(a) & set(b)):
        if a[path] != b[path]:
            problems.append(
                f"{path} differs: {left_name} {a[path][:12]}…, "
                f"{right_name} {b[path][:12]}…")
    return problems


# ==========================================================================
# Invocation records
# ==========================================================================
def parse_command(command, what: str) -> dict:
    """A `probe_flake.py` argument vector as `{option: value}`.

    `--flag value` and `--flag=value` are the same thing; anything else
    in the vector is positional and kept under `_positional`, because a
    command this cannot read is a command whose conditions cannot be
    compared, and that is a refusal rather than a silent pass.
    """
    if not isinstance(command, list) or not command:
        raise HandoffError(f"{what} must be a non-empty list of arguments")
    if not all(isinstance(token, str) for token in command):
        raise HandoffError(f"{what} must contain only strings")
    options: dict = {}
    positional: list = []
    index = 0
    while index < len(command):
        token = command[index]
        if token.startswith("--"):
            if "=" in token:
                name, value = token.split("=", 1)
            else:
                name = token
                if index + 1 >= len(command):
                    raise HandoffError(f"{what}: {token} has no value")
                index += 1
                value = command[index]
            if name in options:
                raise HandoffError(f"{what} repeats {name}")
            options[name] = value
        else:
            positional.append(token)
        index += 1
    options["_positional"] = positional
    return options


def _number(value, what: str):
    """A supplied option value as a number, or a refusal naming it."""
    if isinstance(value, bool):
        raise HandoffError(f"{what} must be a number, got {value!r}")
    if isinstance(value, (int, float)):
        return value
    try:
        return int(value)
    except (TypeError, ValueError):
        pass
    try:
        return float(value)
    except (TypeError, ValueError):
        raise HandoffError(f"{what} must be a number, got {value!r}") from None


def require_invocation(document, what: str) -> dict:
    """One recorded `probe_flake.py` invocation, validated.

    The record is the COMMAND plus the things the command does not say:
    the directory it ran in, the ports the harness actually leased, and
    the retry policy in force. Ports are evidence, never a condition —
    they are leased dynamically and differ on every run.
    """
    if not isinstance(document, dict):
        raise HandoffError(
            f"{what} must be a JSON object, got {type(document).__name__}")
    options = parse_command(document.get("command"), f"{what}.command")
    directory = document.get("directory")
    if not isinstance(directory, str) or not directory:
        raise HandoffError(f"{what} has no `directory`")
    retries = document.get("retries")
    if retries != 0:
        raise HandoffError(
            f"{what} records a retry policy of {retries!r}; a measurement "
            f"where any passing attempt counts as a pass measures a "
            f"different quantity, so this lab's retry policy is 0 and "
            f"nothing else (`tools/run_probes.py --retries` is exactly the "
            f"behavior being excluded)")
    ports = document.get("ports")
    if ports is not None:
        if (not isinstance(ports, list)
                or not all(isinstance(port, int) and not isinstance(port, bool)
                           for port in ports)):
            raise HandoffError(f"{what}.ports must be a list of integers")
    return document


def effective_settings(invocation, what: str) -> dict:
    """The behavior-affecting conditions one invocation actually ran under.

    Destinations and positional tokens are dropped: the script path
    differs by worktree and `--result`/`--artifact-root` MUST differ, so
    comparing them would refuse every legitimate pair. Absent options
    are filled in with the effective default `probe_flake` would apply,
    because "the caller declined to override a default" is not a
    difference in conditions.
    """
    options = parse_command(invocation["command"], f"{what}.command")
    settings = {}
    probe = options.get("--probe")
    if not probe:
        raise HandoffError(f"{what}.command names no --probe")
    settings["probe"] = probe
    if "--runs" not in options:
        raise HandoffError(f"{what}.command names no --runs")
    settings["runs"] = _number(options["--runs"], f"{what}.command --runs")
    for option, default in INVOCATION_DEFAULTS.items():
        key = option.lstrip("-").replace("-", "_")
        if option in options:
            settings[key] = _number(options[option],
                                    f"{what}.command {option}")
        else:
            settings[key] = default
    unknown = sorted(name for name in options
                     if name != "_positional"
                     and name not in INVOCATION_DEFAULTS
                     and name not in DESTINATION_OPTIONS
                     and name not in ("--probe", "--runs"))
    if unknown:
        raise HandoffError(
            f"{what}.command carries options this comparison does not know "
            f"how to classify as a condition or a destination: "
            f"{', '.join(unknown)}. Classify them in "
            f"`deflake_diagnosis.INVOCATION_DEFAULTS` or "
            f"`DESTINATION_OPTIONS` rather than letting an unrecognised "
            f"option silently compare equal")
    settings["retries"] = invocation["retries"]
    return settings


def invocation_differences(baseline, verification) -> list:
    """Every behavior-affecting difference between two invocations."""
    left = effective_settings(baseline, "baseline.invocation")
    right = effective_settings(verification, "verification.invocation")
    return [f"{key}: baseline {left[key]!r}, verification {right[key]!r}"
            for key in sorted(set(left) | set(right))
            if left.get(key) != right.get(key)]


def destinations(invocation) -> list:
    """The paths one invocation wrote to, as recorded in its command."""
    options = parse_command(invocation["command"], "invocation.command")
    return [options[name] for name in DESTINATION_OPTIONS if name in options]


def worktree_paths() -> list:
    """Every registered worktree, including the primary checkout.

    Delegated to `probe_flake`, which already computes exactly this for
    `check_artifact_root`. Answering it a second way is how the artifact
    root and the result document would come to disagree about what
    "inside a worktree" means.
    """
    return probe_flake._worktree_paths()


def inside_any_worktree(path, worktrees) -> str | None:
    """The worktree containing `path`, or None.

    Supplied rather than discovered, so the rule is testable without a
    git checkout; the CLI passes `probe_flake._worktree_paths()`.
    """
    resolved = Path(path).expanduser()
    for tree in worktrees:
        tree = Path(tree).expanduser()
        if resolved == tree or tree in resolved.parents:
            return str(tree)
    return None


# ==========================================================================
# Result documents
# ==========================================================================
def require_result(document, what: str) -> dict:
    """A `probe-flake-result/v1` document, structurally validated."""
    if not isinstance(document, dict):
        raise HandoffError(
            f"{what} must be a JSON object, got {type(document).__name__}")
    schema = document.get("schema")
    if schema != probe_flake.RESULT_SCHEMA:
        raise HandoffError(
            f"{what} is {schema!r}, expected {probe_flake.RESULT_SCHEMA!r}")
    probe = document.get("probe")
    if not isinstance(probe, str) or not probe:
        raise HandoffError(f"{what} names no probe")
    commit = document.get("commit_sha")
    if not isinstance(commit, str) or not COMMIT_RE.match(commit):
        raise HandoffError(
            f"{what} does not name a resolved commit ({commit!r}); "
            f"`probe_flake` writes the literal 'unknown' when git could not "
            f"be consulted, and a measurement nobody can attribute to a "
            f"commit is not evidence")
    checks = document.get("checks")
    if not isinstance(checks, list) or not checks:
        raise HandoffError(f"{what} declares no checks")
    ids = []
    for position, entry in enumerate(checks):
        if not isinstance(entry, dict):
            raise HandoffError(f"{what}.checks[{position}] must be an object")
        cid = entry.get("id")
        if not isinstance(cid, str) or not probe_protocol.CHECK_ID_RE.match(cid):
            raise HandoffError(
                f"{what}.checks[{position}] has no stable identifier "
                f"({cid!r}); `probe-result/v1` identifiers are static and "
                f"carry no runtime value, so this is a malformed protocol "
                f"result rather than something to diagnose")
        if cid in ids:
            raise HandoffError(f"{what}.checks repeats {cid!r}")
        ids.append(cid)
    runs = document.get("runs")
    if not isinstance(runs, list):
        raise HandoffError(f"{what} has no `runs` list")
    for position, run in enumerate(runs):
        where = f"{what}.runs[{position}]"
        if not isinstance(run, dict):
            raise HandoffError(f"{where} must be an object")
        if run.get("outcome") not in (probe_flake.RUN_PASS, probe_flake.RUN_FAIL,
                                      probe_flake.RUN_TIMEOUT):
            raise HandoffError(
                f"{where} has outcome {run.get('outcome')!r}, which is not "
                f"one of the three valid per-run outcomes")
        checks_map = run.get("checks")
        if not isinstance(checks_map, dict):
            raise HandoffError(f"{where} has no `checks` map")
        undeclared = sorted(set(checks_map) - set(ids))
        if undeclared:
            raise HandoffError(
                f"{where} reports identifiers the descriptor never declared "
                f"({', '.join(undeclared)}); a per-run identifier that is "
                f"not in the descriptor is a malformed protocol result, not "
                f"a diagnosis input")
        absent = [cid for cid in ids if cid not in checks_map]
        if absent:
            raise HandoffError(
                f"{where} omits declared identifiers entirely "
                f"({', '.join(absent)}); `probe_protocol.parse_event_stream` "
                f"reports every declared check as PASS, FAIL or MISSING, so "
                f"a key that is simply absent means the document was not "
                f"written by the harness")
        for cid, result in checks_map.items():
            if result not in probe_protocol.CHECK_RESULTS:
                raise HandoffError(
                    f"{where} reports {cid!r} as {result!r}, which is not "
                    f"PASS, FAIL or MISSING")
    return document


def descriptor_ids(document) -> list:
    return [entry["id"] for entry in document["checks"]]


def require_controlled(document, *, what: str, expected_ids=None) -> dict:
    """A result document usable as one side of a controlled comparison.

    The four conditions beyond structural validity: the measurement did
    not abort, it is the policy's own size, it ran at the fixed
    capability count, and nothing else was running beside it. The last
    one matters because an overlapping flake-harness invocation shares
    the machine, and a comparison made against a contended baseline is
    not controlled.
    """
    require_result(document, what)
    if document.get("status") != "ok":
        raise HandoffError(
            f"{what} has status {document.get('status')!r}; only a valid "
            f"measurement is a controlled comparison side (a harness error "
            f"has no trustworthy failure rate at all)")
    requested = document.get("requested_runs")
    completed = document.get("completed_runs")
    if requested != RUN_COUNT:
        raise HandoffError(
            f"{what} requested {requested!r} runs; this lab's measurement "
            f"contract is exactly {RUN_COUNT}, which is the basis X is "
            f"stated on")
    if completed != requested:
        raise HandoffError(
            f"{what} completed {completed!r} of {requested!r} runs; an "
            f"incomplete batch is not a measurement")
    if len(document["runs"]) != completed:
        raise HandoffError(
            f"{what} reports {completed!r} completed runs but carries "
            f"{len(document['runs'])} run record(s)")
    caps = document.get("rts_capabilities")
    if caps != RTS_CAPABILITIES:
        raise HandoffError(
            f"{what} ran at {caps!r} RTS capabilities; the fixed condition "
            f"is {RTS_CAPABILITIES}, and an unpinned run measures a "
            f"different condition on every machine")
    peak = document.get("peak_concurrency")
    if peak != 1:
        raise HandoffError(
            f"{what} observed peak concurrency {peak!r}; another flake "
            f"harness invocation overlapped this measurement, so it is not "
            f"a controlled comparison side")
    ids = descriptor_ids(document)
    if expected_ids is not None and list(expected_ids) != ids:
        raise HandoffError(
            f"{what} declares the checks {ids} where the handoff's expected "
            f"descriptor is {list(expected_ids)}; identifiers and their "
            f"order are the stable contract, and a rename or removal is a "
            f"separately approved protocol change this issue does not "
            f"invent a mapping for")
    return document


def non_pass_ids(document) -> list:
    """Every declared identifier that was ever FAIL or MISSING."""
    ids = descriptor_ids(document)
    seen = set()
    for run in document["runs"]:
        for cid, result in run["checks"].items():
            if result != probe_protocol.PASS:
                seen.add(cid)
    return [cid for cid in ids if cid in seen]


def failure_count(document) -> int:
    """Runs that failed or timed out — the quantity X is stated against."""
    return sum(1 for run in document["runs"]
               if run["outcome"] in (probe_flake.RUN_FAIL,
                                     probe_flake.RUN_TIMEOUT))


def missing_problems(document, *, targets, what: str) -> list:
    """Every violation of the scoped MISSING rule in one batch.

    See the module docstring: targets are absolute, passing runs are
    absolute, an accepted failing run may only lose a contiguous suffix,
    and no identifier may vanish from the batch entirely.

    X is deliberately NOT an argument. A batch above X has already
    failed on its count, and the caller is what decides whether a
    violation here means "refuse the repair" or "this is why the route
    is `partial-improvement`" — scoring it twice would let one fact
    produce two verdicts.
    """
    ids = descriptor_ids(document)
    order = {cid: position for position, cid in enumerate(ids)}
    problems = []
    emitted = set()
    for run in document["runs"]:
        index = run.get("index")
        results = run["checks"]
        emitted |= {cid for cid, result in results.items()
                    if result != probe_protocol.MISSING}
        missing = sorted((cid for cid, result in results.items()
                          if result == probe_protocol.MISSING),
                         key=order.__getitem__)
        for cid in missing:
            if cid in targets:
                problems.append(
                    f"{what} run {index} reports the target check {cid!r} as "
                    f"MISSING; a target that stops being emitted has not "
                    f"been fixed, it has stopped being measured")
        if not missing:
            continue
        if run["outcome"] == probe_flake.RUN_PASS:
            problems.append(
                f"{what} run {index} passed while omitting "
                f"{', '.join(missing)}; a run that reached the end must "
                f"emit every expected check")
            continue
        positions = [order[cid] for cid in missing]
        if positions != list(range(min(positions), len(ids))):
            problems.append(
                f"{what} run {index} omits {', '.join(missing)}, which is "
                f"not a contiguous suffix of the declared order; an aborted "
                f"run loses everything after its abort point and nothing "
                f"before it, so this is a malformed result rather than an "
                f"abort")
    vanished = [cid for cid in ids if cid not in emitted]
    if vanished:
        problems.append(
            f"{what} never emitted {', '.join(vanished)} in any of its "
            f"{len(document['runs'])} runs; an identifier that disappears "
            f"from the batch as a whole has been removed from the "
            f"measurement, whatever any single run did")
    return problems


# ==========================================================================
# The handoff
# ==========================================================================
class Handoff:
    """One complete #1436 measurement handoff, validated.

    The probe key, X, the ordered expected descriptor, the observed
    non-PASS identifiers, the exact invocation, the configuration
    manifest and the retained artifacts. Exactly one probe: the
    workflow's whole bound is one invocation, one probe, one root cause.
    """

    def __init__(self, document, *, acceptable_failures: int):
        self.document = document
        self.probe = document["probe"]
        self.acceptable_failures = acceptable_failures
        self.result = document["result"]
        self.invocation = document["invocation"]
        self.configuration = document["configuration"]
        self.expected_checks = descriptor_ids(self.result)
        self.targets = tuple(document["targets"])
        self.artifacts = list(document.get("artifacts") or [])

    @property
    def commit_sha(self) -> str:
        return self.result["commit_sha"]


def require_handoff(document) -> Handoff:
    """`document` as a usable handoff, or the entry gate's refusal.

    Every refusal here is `handoff-rejected`: the invocation stops
    without changing code and without opening a PR, and it is NOT
    recorded as a diagnosis outcome, because no diagnosis happened.
    """
    what = "handoff"
    if not isinstance(document, dict):
        raise HandoffError(
            f"{what} must be a JSON object, got {type(document).__name__}")
    schema = document.get("schema")
    if schema != HANDOFF_SCHEMA:
        raise HandoffError(f"{what} is {schema!r}, expected {HANDOFF_SCHEMA!r}")

    probe = document.get("probe")
    if isinstance(probe, list):
        raise HandoffError(
            f"{what} names {len(probe)} probes ({probe!r}); one invocation "
            f"diagnoses exactly one probe, so several candidates are "
            f"several invocations")
    if not isinstance(probe, str) or not probe:
        raise HandoffError(f"{what} names no probe")
    registered = probe_flake.registered_scripts()
    if probe not in registered:
        raise HandoffError(
            f"{what} names {probe!r}, which is not a registered probe key "
            f"in `tools/run_probes.py`")
    if probe_flake.protocol_status(probe) != probe_protocol.PROTOCOL_VERSION:
        raise HandoffError(
            f"{what} names {probe!r}, which does not implement "
            f"{probe_protocol.PROTOCOL_VERSION}; a probe with no descriptor "
            f"has no per-check evidence to diagnose")

    try:
        acceptable = probe_census.require_acceptable_failures(
            document.get("acceptable_failures"),
            f"{what}'s `acceptable_failures`")
    except probe_census.CensusError as error:
        raise HandoffError(
            f"{str(error)}; X is #1430's validated integer out of "
            f"{RUN_COUNT} runs and this workflow neither invents nor "
            f"changes it") from None

    result = document.get("result")
    if not isinstance(result, dict):
        raise HandoffError(
            f"{what} carries no embedded {probe_flake.RESULT_SCHEMA!r} "
            f"document. The durable census row is not a substitute: "
            f"`probe_census.ingest_result` drops the ports, the per-run "
            f"check maps, the descriptor labels, the artifact root, the "
            f"invocation directory and the exact command, so a handoff "
            f"rebuilt from it cannot identify the baseline invocation")
    require_controlled(result, what=f"{what}.result")
    if result["probe"] != probe:
        raise HandoffError(
            f"{what} names {probe!r} but its result document measured "
            f"{result['probe']!r}")

    expected = document.get("expected_checks")
    if expected is not None and list(expected) != descriptor_ids(result):
        raise HandoffError(
            f"{what}.expected_checks is {list(expected)} where its own "
            f"result document declares {descriptor_ids(result)}; the "
            f"descriptor is the ordered contract and the two cannot "
            f"disagree")

    observed = non_pass_ids(result)
    targets = document.get("targets")
    if not isinstance(targets, list) or not targets:
        raise HandoffError(
            f"{what} names no target check identifiers; every non-PASS "
            f"identifier for this probe is a diagnosis input, and this "
            f"measurement observed {observed or 'none'}")
    unknown = [cid for cid in targets if cid not in descriptor_ids(result)]
    if unknown:
        raise HandoffError(
            f"{what} targets identifiers the descriptor never declared: "
            f"{', '.join(unknown)}")
    unobserved = [cid for cid in targets if cid not in observed]
    if unobserved:
        raise HandoffError(
            f"{what} targets {', '.join(unobserved)}, which never failed or "
            f"went missing in the measurement it is derived from; the "
            f"diagnosis inputs are exactly this probe's non-PASS "
            f"identifiers ({', '.join(observed) or 'none'})")

    require_invocation(document.get("invocation"), f"{what}.invocation")
    settings = effective_settings(document["invocation"], f"{what}.invocation")
    if settings["probe"] != probe:
        raise HandoffError(
            f"{what}.invocation measured {settings['probe']!r} where the "
            f"handoff names {probe!r}")
    if settings["runs"] != RUN_COUNT:
        raise HandoffError(
            f"{what}.invocation requested {settings['runs']!r} runs; the "
            f"measurement contract is exactly {RUN_COUNT}")
    if settings["rts_caps"] != RTS_CAPABILITIES:
        raise HandoffError(
            f"{what}.invocation ran at {settings['rts_caps']!r} RTS "
            f"capabilities; the fixed condition is {RTS_CAPABILITIES}")

    require_manifest(document.get("configuration"), f"{what}.configuration")

    artifacts = document.get("artifacts")
    if artifacts is not None and (not isinstance(artifacts, list)
                                  or not all(isinstance(path, str)
                                             for path in artifacts)):
        raise HandoffError(f"{what}.artifacts must be a list of paths")
    retained = result.get("retained_artifacts") or []
    if failure_count(result) and not (artifacts or retained):
        raise HandoffError(
            f"{what} observed {failure_count(result)} unsuccessful run(s) "
            f"and records no retained artifact; the failing logs are the "
            f"diagnosis evidence and `probe_flake` keeps them by default")
    return Handoff(document, acceptable_failures=acceptable)


# ==========================================================================
# The diagnosis
# ==========================================================================
class Outcome:
    """One diagnosis invocation's route and everything it must report."""

    def __init__(self, route: str, *, probe=None, detail: str = "",
                 owner_issue=None, opens_pull_request: bool = False,
                 targets=(), baseline_failures=None,
                 verification_failures=None, acceptable_failures=None,
                 artifacts=(), notes=(), handoff=None):
        self.handoff = handoff
        self.route = route
        self.probe = probe
        self.detail = detail
        self.owner_issue = owner_issue
        self.opens_pull_request = opens_pull_request
        self.targets = list(targets)
        self.baseline_failures = baseline_failures
        self.verification_failures = verification_failures
        self.acceptable_failures = acceptable_failures
        self.artifacts = list(artifacts)
        self.notes = list(notes)

    def to_document(self) -> dict:
        return {
            "schema": OUTCOME_SCHEMA,
            "route": self.route,
            "probe": self.probe,
            "detail": self.detail or None,
            "owner_issue": self.owner_issue,
            "opens_pull_request": self.opens_pull_request,
            "targets": self.targets,
            "acceptable_failures": self.acceptable_failures,
            "baseline_failures": self.baseline_failures,
            "verification_failures": self.verification_failures,
            "retained_artifacts": self.artifacts,
            "notes": self.notes or None,
        }


def _require_batch(section, *, what: str, handoff: Handoff,
                   worktrees) -> dict:
    """One controlled measurement side: its result, command and manifest."""
    if not isinstance(section, dict):
        raise HandoffError(f"{what} must be a JSON object")
    result = section.get("result")
    if not isinstance(result, dict):
        raise HandoffError(
            f"{what} carries no {probe_flake.RESULT_SCHEMA!r} document")
    require_controlled(result, what=f"{what}.result",
                       expected_ids=handoff.expected_checks)
    if result["probe"] != handoff.probe:
        raise HandoffError(
            f"{what}.result measured {result['probe']!r}, not "
            f"{handoff.probe!r}")
    invocation = require_invocation(section.get("invocation"),
                                   f"{what}.invocation")
    require_manifest(section.get("configuration"), f"{what}.configuration")
    worktree = section.get("worktree")
    if not isinstance(worktree, str) or not worktree:
        raise HandoffError(f"{what} names no worktree")
    for path in destinations(invocation):
        tree = inside_any_worktree(path, worktrees)
        if tree is not None:
            raise HandoffError(
                f"{what} wrote {path} inside the working tree {tree}; the "
                f"result document and the raw artifacts must both stay "
                f"outside every worktree, or they enter a commit or wedge "
                f"the drainer's cleanup")
    return section


class Diagnosis:
    """One invocation: one handoff, one probe, at most one pull request.

    The one-PR limit is a property of this object rather than a rule
    somebody has to remember: `open_pull_request` may be reached once,
    and only on the repair route.
    """

    def __init__(self, handoff: Handoff):
        self.handoff = handoff
        self.pull_requests = 0

    def open_pull_request(self, outcome: Outcome) -> int:
        if outcome.route not in ROUTES_THAT_CHANGE_CODE:
            raise RouteRefused(
                f"the {outcome.route!r} route opens no pull request; it "
                f"stops without a repair and emits its declared handoff")
        if self.pull_requests:
            raise RouteRefused(
                f"this invocation already opened a pull request for "
                f"{self.handoff.probe!r}; one invocation opens at most one "
                f"PR for one probe-side root cause, and a second cause is a "
                f"second measurement")
        self.pull_requests += 1
        return self.pull_requests


def evaluate(document, *, worktrees=()) -> Outcome:
    """The route one diagnosis document's own evidence supports.

    Raises `HandoffError` when the entry gate refuses the input and
    `RouteRefused` when a well-formed document declares a route its
    evidence denies — a repair whose verification stayed above X, for
    instance, which is `partial-improvement` and not a repair the agent
    may relabel.
    """
    if not isinstance(document, dict):
        raise HandoffError(
            f"diagnosis must be a JSON object, got {type(document).__name__}")
    schema = document.get("schema")
    if schema != DIAGNOSIS_SCHEMA:
        raise HandoffError(
            f"diagnosis is {schema!r}, expected {DIAGNOSIS_SCHEMA!r}")
    handoff = require_handoff(document.get("handoff"))
    route = document.get("route")
    if route not in ROUTES:
        raise HandoffError(
            f"diagnosis declares the route {route!r}; the declared routes "
            f"are {', '.join(ROUTES)}")
    if route == ROUTE_HANDOFF_REJECTED:
        raise HandoffError(
            f"diagnosis declares {ROUTE_HANDOFF_REJECTED!r} while carrying a "
            f"handoff this gate accepted; that route is what a REFUSED "
            f"handoff produces, and it is never a conclusion drawn after one "
            f"was admitted")

    owner = ROUTE_OWNER[route]
    common = {
        "handoff": handoff,
        "probe": handoff.probe,
        "targets": handoff.targets,
        "acceptable_failures": handoff.acceptable_failures,
        "owner_issue": owner,
        "artifacts": handoff.artifacts or (
            handoff.result.get("retained_artifacts") or []),
    }

    baseline_section = document.get("baseline")
    if baseline_section is None:
        raise HandoffError(
            "diagnosis carries no controlled pre-fix baseline; the #1436 "
            "census result triggers diagnosis but is not itself the "
            "controlled base-versus-branch comparison")
    _require_batch(baseline_section, what="baseline", handoff=handoff,
                   worktrees=worktrees)
    baseline = baseline_section["result"]
    baseline_failures = failure_count(baseline)
    common["baseline_failures"] = baseline_failures

    config_problems = manifest_differences(
        handoff.configuration, baseline_section["configuration"],
        left_name="the handoff", right_name="the clean comparison worktree")
    if config_problems:
        raise RouteRefused(
            "the controlled baseline did not reproduce the handoff's "
            "configuration state, so it is not the same condition: "
            + "; ".join(config_problems))

    reproduced = probe_census.tolerance_state(
        handoff.acceptable_failures, baseline["requested_runs"],
        baseline["completed_runs"], baseline_failures)
    observed = set(non_pass_ids(baseline))
    hit = [cid for cid in handoff.targets if cid in observed]

    if route == ROUTE_CANNOT_REPRODUCE:
        _require_evidence(document, route)
        if reproduced == probe_census.TOLERANCE_OVER and hit:
            raise RouteRefused(
                f"the controlled baseline DID reproduce an over-tolerance "
                f"result ({baseline_failures}/{RUN_COUNT} against an X of "
                f"{handoff.acceptable_failures}) with the target check(s) "
                f"{', '.join(hit)} non-PASS, so {ROUTE_CANNOT_REPRODUCE!r} "
                f"is not this measurement's outcome")
        return Outcome(route, detail=(
            f"the controlled baseline observed {baseline_failures}/"
            f"{RUN_COUNT} failures against an X of "
            f"{handoff.acceptable_failures}"), **common)

    if reproduced != probe_census.TOLERANCE_OVER:
        raise RouteRefused(
            f"the controlled baseline observed {baseline_failures}/"
            f"{RUN_COUNT} failures against an X of "
            f"{handoff.acceptable_failures} ({reproduced}); a repair may "
            f"only proceed from a baseline that exceeds X, so this is the "
            f"{ROUTE_CANNOT_REPRODUCE!r} outcome for #1439")
    if not hit:
        raise RouteRefused(
            f"the controlled baseline never observed the target check(s) "
            f"{', '.join(handoff.targets)} as FAIL or MISSING, so it did "
            f"not reproduce the pattern the target was identified from; "
            f"this is the {ROUTE_CANNOT_REPRODUCE!r} outcome for #1439")

    if route in (ROUTE_PRODUCTION_DEFECT, ROUTE_NO_CONFIDENT_FIX):
        _require_evidence(document, route)
        if document.get("verification") is not None:
            raise RouteRefused(
                f"the {route!r} route runs no verification batch: it opens "
                f"no pull request and changes no probe, so a verification "
                f"result here means a repair was attempted and the route is "
                f"mislabelled")
        return Outcome(route, detail=_evidence_detail(document, route),
                       **common)

    verification_section = document.get("verification")
    if verification_section is None:
        raise HandoffError(
            f"the {route!r} route requires a verification batch; a repair "
            f"is only ever accepted against a fresh {RUN_COUNT}-run "
            f"measurement in the repair worktree")
    _require_batch(verification_section, what="verification", handoff=handoff,
                   worktrees=worktrees)
    verification = verification_section["result"]
    verification_failures = failure_count(verification)
    common["verification_failures"] = verification_failures

    differences = invocation_differences(baseline_section["invocation"],
                                         verification_section["invocation"])
    if differences:
        raise RouteRefused(
            "the verification batch did not run under the baseline's "
            "conditions, so the two are not comparable: "
            + "; ".join(differences))
    config_problems = manifest_differences(
        baseline_section["configuration"], verification_section["configuration"],
        left_name="the clean comparison worktree",
        right_name="the repair worktree")
    if config_problems:
        raise RouteRefused(
            "the two comparison worktrees do not hold the same "
            "configuration state: " + "; ".join(config_problems))
    if (baseline_section["worktree"] == verification_section["worktree"]):
        raise RouteRefused(
            f"the baseline and the verification ran in the same worktree "
            f"({baseline_section['worktree']}); the clean comparison "
            f"worktree stays at the baseline commit and the repair lives in "
            f"its own")
    for path in destinations(baseline_section["invocation"]):
        if path in destinations(verification_section["invocation"]):
            raise RouteRefused(
                f"both batches wrote to {path}; the baseline evidence must "
                f"survive the verification that follows it")

    # A verification batch is ACCEPTED only when both halves hold: the
    # count is at or below X, and the scoped MISSING rule is intact. The
    # issue names them together for a reason — "verification remains
    # above X, contains any MISSING result, becomes invalid, or only
    # partially improves the rate" is ONE list, and every entry on it
    # goes to #1439 rather than to a pull request.
    problems = missing_problems(verification, targets=set(handoff.targets),
                                what="the verification batch")
    state = probe_census.tolerance_state(
        handoff.acceptable_failures, verification["requested_runs"],
        verification["completed_runs"], verification_failures)
    accepted = state == probe_census.TOLERANCE_ACCEPTABLE and not problems

    if route == ROUTE_PARTIAL_IMPROVEMENT:
        if accepted:
            raise RouteRefused(
                f"the verification batch reached {verification_failures}/"
                f"{RUN_COUNT} against an X of "
                f"{handoff.acceptable_failures} with the MISSING rule "
                f"intact; that is an accepted verification, so "
                f"{ROUTE_PARTIAL_IMPROVEMENT!r} is not its outcome")
        _require_evidence(document, route)
        reason = ("; ".join(problems) if problems else
                  f"stayed above the X of {handoff.acceptable_failures}")
        return Outcome(route, detail=(
            f"the verification batch went from {baseline_failures} to "
            f"{verification_failures} failures out of {RUN_COUNT} but "
            f"{reason}"), **common)

    # The repair route: everything above held, so the remaining questions
    # are the ones about the repair itself.
    if problems:
        raise RouteRefused(
            f"the verification batch violates the MISSING rule, so it is "
            f"not an accepted verification whatever its failure count; "
            f"this is the {ROUTE_PARTIAL_IMPROVEMENT!r} outcome for #1439: "
            + "; ".join(problems))
    if state != probe_census.TOLERANCE_ACCEPTABLE:
        raise RouteRefused(
            f"the verification batch observed {verification_failures}/"
            f"{RUN_COUNT} failures against an X of "
            f"{handoff.acceptable_failures} ({state}); a repair is accepted "
            f"only at or below X, so this is the "
            f"{ROUTE_PARTIAL_IMPROVEMENT!r} outcome for #1439")
    _require_evidence(document, route)
    _require_repair(document, verification_section)
    return Outcome(route, opens_pull_request=True, detail=(
        f"{baseline_failures}/{RUN_COUNT} before, {verification_failures}/"
        f"{RUN_COUNT} after, against an X of "
        f"{handoff.acceptable_failures}"), **common)


def _require_evidence(document, route: str) -> None:
    """The diagnosis every route must state, and the repair's attestations.

    Deliberately a check on PRESENCE and SHAPE. Whether the evidence is
    convincing is the reviewer's call, but a route declared with no
    stated cause and no evidence at all cannot be reviewed, so it cannot
    be declared.
    """
    diagnosis = document.get("diagnosis")
    if not isinstance(diagnosis, dict):
        raise HandoffError(
            f"the {route!r} route states no diagnosis; every route records "
            f"why the measurement ended where it did")
    evidence = diagnosis.get("evidence")
    if (not isinstance(evidence, list) or not evidence
            or not all(isinstance(item, str) and item.strip()
                       for item in evidence)):
        raise HandoffError(
            f"the {route!r} route records no diagnosis evidence; the "
            f"evidence is what makes the route reviewable rather than "
            f"asserted")
    category = diagnosis.get("category")
    if route == ROUTE_REPAIR:
        if category not in CAUSE_CATEGORIES:
            raise HandoffError(
                f"a repair names the one probe-side cause it fixes, from "
                f"{', '.join(CAUSE_CATEGORIES)}; got {category!r}. Several "
                f"independent causes are the {ROUTE_NO_CONFIDENT_FIX!r} "
                f"route, not a list here")
        attestations = document.get("attestations")
        if not isinstance(attestations, dict):
            raise HandoffError(
                "a repair records its preservation attestations explicitly")
        for name in ATTESTATIONS:
            if attestations.get(name) is not True:
                raise HandoffError(
                    f"a repair must attest {name} (got "
                    f"{attestations.get(name)!r}); weakening an assertion is "
                    f"never a fix, and the shapes a machine cannot see — a "
                    f"broadened expected value, an assertion quietly dropped "
                    f"from a still-passing check — have to be claimed on the "
                    f"record for a reviewer to check")
    elif category is not None and category not in CAUSE_CATEGORIES:
        raise HandoffError(
            f"the {route!r} route names the cause category {category!r}, "
            f"which is not one of {', '.join(CAUSE_CATEGORIES)}")


def _evidence_detail(document, route: str) -> str:
    diagnosis = document.get("diagnosis") or {}
    return diagnosis.get("summary") or f"{route}: see the recorded evidence"


def _require_repair(document, verification_section) -> None:
    """The repair commit is frozen, clean, and the thing that was measured."""
    repair = document.get("repair")
    if not isinstance(repair, dict):
        raise HandoffError("a repair route records its `repair` block")
    commit = repair.get("commit_sha")
    if not isinstance(commit, str) or not COMMIT_RE.match(commit):
        raise HandoffError(
            f"the repair names no resolved commit ({commit!r}); the repair "
            f"is committed and frozen BEFORE it is verified")
    if verification_section["result"]["commit_sha"] != commit:
        raise HandoffError(
            f"the verification batch measured commit "
            f"{verification_section['result']['commit_sha']} while the "
            f"proposed repair is {commit}; `probe_flake` records only "
            f"`git rev-parse HEAD` and cannot see uncommitted source, so a "
            f"verification against another commit measures something this "
            f"pull request does not contain")
    if verification_section.get("source_clean") is not True:
        raise HandoffError(
            "the repair worktree was not recorded as source-clean at "
            "verification time; an uncommitted change there is invisible to "
            "the recorded commit and invalidates the measurement")
    changed = repair.get("changed_paths")
    if (not isinstance(changed, list) or not changed
            or not all(isinstance(path, str) and path for path in changed)):
        raise HandoffError("the repair records no changed paths")
    offenders = [path for path in changed
                 if not (path.startswith("tools/")
                         or path.startswith("test-headless/"))]
    if offenders:
        raise HandoffError(
            f"the repair changes {', '.join(offenders)}; production Haskell "
            f"and Lua behavior changes are outside this workflow's repair "
            f"scope — an assertion that is right about a product that is "
            f"wrong is the {ROUTE_PRODUCTION_DEFECT!r} route for #1438")


# ==========================================================================
# CLI
# ==========================================================================
def _load(path: str, what: str):
    try:
        return json.loads(Path(path).read_text(encoding="utf-8"))
    except OSError as error:
        raise HandoffError(f"{what} at {path} is unreadable ({error})") from None
    except json.JSONDecodeError as error:
        raise HandoffError(f"{what} at {path} is not JSON ({error})") from None


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    group = ap.add_mutually_exclusive_group(required=True)
    group.add_argument("--diagnosis", metavar="PATH",
                       help=f"evaluate a {DIAGNOSIS_SCHEMA} document")
    group.add_argument("--handoff", metavar="PATH",
                       help=f"check a {HANDOFF_SCHEMA} document against the "
                            f"entry gate and stop")
    group.add_argument("--manifest", metavar="ROOT",
                       help=f"print the {MANIFEST_SCHEMA} for a checkout")
    ap.add_argument("--json", action="store_true",
                    help="print the outcome document instead of prose")
    args = ap.parse_args(argv)

    if args.manifest:
        print(json.dumps(config_manifest(args.manifest), indent=2,
                         sort_keys=True))
        return EXIT_OK

    try:
        if args.handoff:
            handoff = require_handoff(_load(args.handoff, "handoff"))
            if args.json:
                print(json.dumps({"accepted": True, "probe": handoff.probe,
                                  "targets": list(handoff.targets),
                                  "acceptable_failures":
                                      handoff.acceptable_failures},
                                 indent=2, sort_keys=True))
            else:
                print(f"handoff accepted: {handoff.probe}, targets "
                      f"{', '.join(handoff.targets)}, X="
                      f"{handoff.acceptable_failures}/{RUN_COUNT}")
            return EXIT_OK
        document = _load(args.diagnosis, "diagnosis")
        outcome = evaluate(document, worktrees=worktree_paths())
        # The one-PR limit, exercised rather than described: a repair
        # route reaches it exactly once per invocation, and every other
        # route is refused by the session itself rather than by prose.
        if outcome.opens_pull_request:
            Diagnosis(outcome.handoff).open_pull_request(outcome)
    except HandoffError as error:
        print(f"deflake_diagnosis: {ROUTE_HANDOFF_REJECTED}: {error}",
              file=sys.stderr)
        return EXIT_REJECTED
    except RouteRefused as error:
        print(f"deflake_diagnosis: route refused: {error}", file=sys.stderr)
        return EXIT_REFUSED

    if args.json:
        print(json.dumps(outcome.to_document(), indent=2, sort_keys=True))
    else:
        owner = (f" — handed off to #{outcome.owner_issue}"
                 if outcome.owner_issue else "")
        print(f"{outcome.route}: {outcome.detail}{owner}")
        if outcome.artifacts:
            print("retained artifacts:")
            for path in outcome.artifacts:
                print(f"  {path}")
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
