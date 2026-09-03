#!/usr/bin/env python3
"""Is other work already in flight for this probe? (#1433)

Measuring one probe's flakiness costs ten or more runs and can take an
hour (`tools/probe_flake.py` requires an explicit `--probe` alongside
`--runs`, and its own examples use `--runs 10`). That work must not
begin when the same probe already has an active `$test` run, an open
tracker item, an open findings entry, or another fix under way.

This is the per-probe ELIGIBILITY component that answers the question,
and nothing else. For one canonical probe key it returns exactly one of:

  * `clear`        — every required source was read COMPLETELY and no
                     matching work is in flight;
  * `in-flight`    — one or more matches, each with inspectable
                     evidence;
  * `source-error` — a required source could not be read or interpreted
                     completely, with an actionable diagnostic.

Ownership boundaries, stated because they are easy to drift across:
candidate iteration and ranking belong to #1435, which consumes this
component immediately before selection and skips every `in-flight`
candidate before ranking; claim acquisition belongs to #1434/#1436; the
launch, the census recording and the claim release belong to #1436.
Nothing here iterates candidates, claims a probe, launches a
measurement, updates the census, or publishes a finding.

It is a POINT-IN-TIME SNAPSHOT. The consumer takes it immediately before
selecting, claiming or starting the expensive measurement, and a match
present at that instant excludes the candidate. This component never
polls and never cancels work that has already begun.

Read-only, absolutely
---------------------
Nothing here invokes the `$test` coordinator, takes its lock, creates a
state directory, rewrites `registry.json`, or changes a report — not one
byte, not a checkbox, not a checklist marker, not a heading. That is
#1432's boundary and this component inherits it whole.

Fail closed
-----------
A partial source scan NEVER returns `clear`. When any required source
cannot be read or parsed without ambiguity, the result is
`source-error`, and it stays `source-error` even when other sources
already produced matches — the matches are still reported so their
evidence is not lost, but an incomplete scan is not allowed to present
itself as a determinate verdict.

Two different things are called "ambiguous" and they are NOT the same
outcome:

  * SUBJECT-match ambiguity — a subject where canonical normalization
    cannot say which registered probe is meant — yields `in-flight`,
    with the competing identities retained in that match's evidence.
    Preferring exclusion over an unsafe clear is the whole point.
  * SOURCE-STRUCTURE ambiguity — a report whose checklist/heading state
    is partial, inconsistent, duplicated or unlexable — yields
    `source-error`. The source is broken and must be fixed, not guessed
    past.

Canonical identity
------------------
`probe_runner_registry.PROBES`'s key is the canonical identity, and an
unknown key is a controlled input error rather than a `clear`.

Matching accepts case- and separator-normalized human forms derived from
the registered key AND the registered script — `injury_log`,
`injury-log probe`, `Injury-log probe`, `injury_log_probe.py` all name
the same probe. It is NOT substring matching: a subject is tokenized and
scanned left to right, taking the LONGEST registered identity at each
position and then advancing past it, so registered prefix families stay
distinct (`repair` / `repair_ai` / `repair_item`, `power` /
`power_workshop`, `persistence_contract` / `persistence_contract_sweep`).

That longest-match rule is per OCCURRENCE, not per subject. One mention
of `repair_ai` never also credits `repair`; a subject mentioning
`repair_ai` in one place and a standalone `repair` probe in another
credits both. The distinction is only visible on the two-mention case,
which is why the gate covers both shapes.

**Common-word keys over-exclude, deliberately.** Several registered keys
are single ordinary English words — `power`, `role`, `chop`, `plant`,
`craft`, `preview`, `construction`, `movement` — so a title using one of
them incidentally ("Move the power node role and rating into the
building YAML schema") matches both `power` and `role`. That is the
direction this component is required to err in: it prefers a false
exclusion, which costs #1435 one skipped candidate, over a false clear,
which costs an hour of duplicated measurement colliding with work
already under way. Nothing here guesses which mentions were incidental —
a wordlist would be exactly the unprincipled judgement the canonical
registry exists to replace. Instead every match carries `matched_text`,
the exact normalized tokens it matched on, so a reader can see in one
line why a candidate was excluded and whether they disagree.

The four sources
----------------
1. Active `$test` runs, through `tools/probe_external_evidence.py` under
   BOTH of a probe's stable identities: `probe:<hyphenated-key>` for an
   ordinary execution and `probe-flake:<hyphenated-key>` for a
   flakiness measurement. Either excludes the other while active; they
   stay distinct in `$test` history.

   The coordinator's authoritative active statuses are `claimed`,
   `creating-worktree`, `worktree-ready`, `running` and
   `awaiting-report`; `completed`, `blocked` and `abandoned` are
   terminal and inactive. An active-status record counts as active only
   when its `heartbeat_at` — falling back to `claimed_at` when no
   heartbeat is recorded at all — is valid and no older than the
   six-hour stale horizon at the supplied evaluation time. A record past
   that horizon is stale and inactive in this read-only snapshot, and is
   NOT modified. An unknown state, or an active state whose required
   timestamp is absent, unusable or not timezone-qualified, is a
   `source-error` rather than an unsafe clear.

   An absent `<git-common-dir>/codex-test` tree stays #1432's normal
   no-evidence state. An existing registry whose complete active-run
   state cannot be read or parsed is a `source-error` — which is exactly
   why #1432 tags each diagnostic with the state it concerns: one
   finished run's missing REPORT says nothing about whether any run is
   active, and must not fail the scan.

2. Every page of the target repository's OPEN ISSUES.
3. Every page of its OPEN PULL REQUESTS. Draft PRs count as open; closed
   issues and closed or merged PRs do not.

   Issue and PR matching uses TITLES only — never bodies, comments,
   branch names or incidental narrative text. Because the title is the
   whole subject, a record whose title cannot be read is a record whose
   subject cannot be interpreted, and that is a `source-error` rather
   than a non-match: "no tokens" and "no match" are otherwise the same
   answer. The number and the state are validated for the same reason
   from the other two ends — one makes the evidence inspectable, the
   other decides eligibility, and neither may be guessed.

   **Target repository resolution.** `tools/` contained no
   GitHub-querying code before this module, so this establishes the
   convention: the target is resolved deterministically from the
   checked-out repository's `origin` remote, and an absent, unparseable
   or non-GitHub `origin` is a `source-error` naming the remote — never
   a hard-coded fallback, and never a silent skip of those two sources.
   Access goes through an injectable transport seam (the `github`
   argument) whose default shells to `gh api` one explicit page at a
   time; the self-test injects a fake and performs no network I/O at
   all, failing rather than skipping if a case would reach the network.

4. The four required findings reports, read in BOTH the checked-out
   repository and a branch-resolved `docs-wip` worktree, excluding
   conservatively on an open match in either. The long-lived docs
   worktree can hold unpublished processing decisions, while the
   checked-out branch can hold newer default-branch findings, so
   neither alone is sufficient.

   The `docs-wip` worktree is resolved BY BRANCH with the idiom already
   shipped in `tools/probe_census_storage.py` (`_worktree_records` over
   `git worktree list --porcelain`, then `resolve_docs_worktree`, both
   still reachable as `probe_census.<name>`), never a hard-coded path
   and never the primary checkout. This DIVERGES from that helper in
   exactly one way: the census treats a missing docs worktree as an
   actionable stop, whereas here an absent or unresolvable `docs-wip`
   worktree is a NORMAL no-evidence state — the checked-out reports are
   the required source and are always read. A resolved `docs-wip`
   worktree that simply does not contain one of the four reports is
   likewise no-evidence for that path. Only a `docs-wip` report that is
   truly ABSENT is no-evidence: one that exists on the filesystem but is
   not a readable, parseable regular file — a directory, a broken
   symlink, an unstattable path — is a `source-error`, because a broken
   source must never be read as an absent one.

   Each report is parsed with its OWN native finding-key family — `NCT-`
   in `non_ci_test_audit_findings.md`, `CIT-` in
   `ci_test_audit_findings.md`, `PYT-` in `python_testing_findings.md`,
   `CH-` in `code_health_findings.md`. The four families are disjoint,
   so a foreign key encountered in a report is a structural surprise and
   is classified as `source-error`, never quietly ignored.

   Matching is against the FULL finding heading, never narrative or
   evidence text. The heading marker is authoritative: `[deferred]`,
   `[no-issue]`, `[#N]` and `[#N, note]` are dispositioned; a bare
   heading paired with its unchecked, unmarked checklist entry is
   unprocessed and therefore open. The marker is lexed with
   `tools/findings_report_audit.py`'s established grammar — the token
   that OPENS the segment following the entry's final em-dash separator,
   with trailing prose ignored — and a marker token anywhere else is a
   lex failure. `tools/ci-local.sh` invokes that audit bare, so it only
   ever checks `code_health_findings.md`; the other three carry no CI
   agreement guarantee, so genuine heading/checklist disagreement in
   them is handled deterministically here (as `source-error`) rather
   than crashing a parser.

Usage:
  python3 tools/probe_inflight.py --probe injury_log
  python3 tools/probe_inflight.py --probe injury_log --json

Exit codes:
  0  a determinate verdict: `clear` or `in-flight` (read `result`)
  1  `source-error`: a required source could not be read completely
  2  rejected input — an unknown probe key, argparse's own usage errors

Gate: `python3 tools/test_probe_inflight.py`, standalone and complete
from a bare invocation with no arguments, no environment and no network.

It is deliberately NOT a `tools/ci-local.sh` / CI step, which is the
convention this component's nearest neighbour already sets:
`test_probe_external_evidence.py` (#1432) is on-demand because what it
covers is optional machine-local state CI does not have — and this
component adds two more things CI has no business reaching, a GitHub API
and a `docs-wip` worktree. `test_probe_flake.py` and, since #1429,
`test_probe_census.py` are the counter-examples that ARE CI steps
(`tools/ci-local.sh` and `.github/workflows/ci.yml` both run them),
covering synthetic documents in throwaway trees rather than any local
state, so they are the precedent for a later promotion; #1433 lists
"changing `make ci` or GitHub CI" as an explicit non-goal, so that
promotion is not this PR's to make.
"""
from __future__ import annotations

import argparse
import json
import os
import re
import stat
import subprocess
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_external_evidence as evidence  # noqa: E402
import probe_engine  # noqa: E402
import probe_runner_registry  # noqa: E402

INFLIGHT_SCHEMA = "probe-inflight/v1"

RESULT_CLEAR = "clear"
RESULT_IN_FLIGHT = "in-flight"
RESULT_SOURCE_ERROR = "source-error"

SOURCE_TEST_RUN = "test-run"
SOURCE_ISSUE = "issue"
SOURCE_PULL_REQUEST = "pull-request"
SOURCE_REPORT = "report"
SOURCES = (SOURCE_TEST_RUN, SOURCE_ISSUE, SOURCE_PULL_REQUEST, SOURCE_REPORT)

# The `$test` coordinator's own run states. Mirrored here because this
# component, like #1432's reader, never invokes the coordinator.
ACTIVE_STATUSES = ("claimed", "creating-worktree", "worktree-ready",
                   "running", "awaiting-report")
TERMINAL_STATUSES = ("completed", "blocked", "abandoned")
STALE_HORIZON = timedelta(hours=6)

# Which findings reports are required, and the finding-key family each
# one owns. The four families are disjoint; a foreign key in a report is
# a structural surprise, not a finding to skip over.
REPORTS = (
    ("docs/non_ci_test_audit_findings.md", "NCT"),
    ("docs/ci_test_audit_findings.md", "CIT"),
    ("docs/python_testing_findings.md", "PYT"),
    ("docs/code_health_findings.md", "CH"),
)

WORKTREE_CHECKOUT = "checkout"
WORKTREE_DOCS = "docs-wip"

# `findings_report_audit.py`'s grammar, deliberately reused rather than
# re-invented: the same marker token set, the same em-dash separator and
# the same "marker opens the segment after the FINAL separator" rule.
MARKER_SEPARATOR = " — "
_MARKER = r"\[(?:#\d+(?:,[^\]]*)?|no-issue|deferred)\]"
MARKER_RE = re.compile(_MARKER)
LEADING_MARKER_RE = re.compile(rf"^{_MARKER}")

CHECKLIST_SHAPE_RE = re.compile(r"^- \[")
HEADING_SHAPE_RE = re.compile(r"^### ")
# Parsed with the family left OPEN, so a foreign key is caught and named
# rather than falling through as an unlexable line.
CHECKLIST_RE = re.compile(r"^- \[([ x])\] ([A-Z]+)-(\d+)\.[ \t]*(.*)$")
HEADING_RE = re.compile(rf"^### (?:({_MARKER})[ \t]+)?([A-Z]+)-(\d+)\.[ \t]*(.*)$")

GITHUB_HOST = "github.com"
PER_PAGE = 100
# A backstop against a transport that never shortens a page. 100 pages
# of 100 is far past any plausible open-issue count for this repository.
MAX_PAGES = 100
GH_TIMEOUT_SECONDS = 60

EXIT_OK = 0
EXIT_SOURCE_ERROR = 1
EXIT_REJECTED = 2

_UNSET = object()


class InflightRejected(Exception):
    """Controlled input rejection: an unknown probe key."""


class SourceError(Exception):
    """A required source could not be read or interpreted completely."""


# ==========================================================================
# Canonical probe identity
# ==========================================================================

def normalize_tokens(text: object) -> list[str]:
    """A subject as lowercase alphanumeric tokens.

    Every run of non-alphanumeric characters is one separator, so
    `injury_log`, `injury-log probe`, `Injury-Log`, `injury_log_probe.py`
    and a heading's backticked prose all reduce to the same token stream.
    """
    if not isinstance(text, str):
        return []
    return [token for token in re.split(r"[^0-9a-z]+", text.lower()) if token]


def identity_forms(key: str, script: str) -> set[tuple[str, ...]]:
    """The token sequences that name one probe.

    Derived from the registry alone: the key, the script's stem, and the
    script with its extension (so a literal `injury_log_probe.py` in a
    title matches). Nothing is invented and nothing is hand-listed.
    """
    forms = {
        tuple(normalize_tokens(key)),
        tuple(normalize_tokens(script[:-3] if script.endswith(".py") else script)),
        tuple(normalize_tokens(script)),
    }
    return {form for form in forms if form}


def build_identity_index(probes=None) -> dict[tuple[str, ...], frozenset[str]]:
    """Every registered identity form, mapped to the probe(s) that own it.

    A form is normally owned by exactly one probe. It is a mapping to a
    SET anyway because a collision is a real (if unlikely) registry
    shape, and the honest response to one is to credit every owner and
    mark the occurrence ambiguous — not to pick a winner silently.
    """
    index: dict[tuple[str, ...], set[str]] = {}
    for key, script, _purpose in (probe_runner_registry.PROBES if probes is None else probes):
        for form in identity_forms(key, script):
            index.setdefault(form, set()).add(key)
    return {form: frozenset(keys) for form, keys in index.items()}


def find_occurrences(subject: object, index) -> list[dict]:
    """Every registered identity named in `subject`, left to right.

    At each token position the LONGEST registered form wins and the scan
    advances past it, which is what keeps a registered prefix family
    distinct: one mention of `repair_ai` never also credits `repair`,
    while a subject naming `repair_ai` in one place and `repair` in
    another credits both. When two or more DISTINCT probes tie at the
    same position for the same maximal length, every one of them is
    credited and the occurrence is marked ambiguous — exclusion is
    preferred over an unsafe clear, and the competing identities are
    retained as the evidence that caused it.
    """
    tokens = normalize_tokens(subject)
    longest = max((len(form) for form in index), default=0)
    occurrences: list[dict] = []
    position = 0
    while position < len(tokens):
        matched: frozenset[str] | None = None
        span = 0
        for length in range(min(longest, len(tokens) - position), 0, -1):
            form = tuple(tokens[position:position + length])
            owners = index.get(form)
            if owners is None:
                continue
            matched, span = owners, length
            break
        if matched is None:
            position += 1
            continue
        occurrences.append({
            "probes": sorted(matched),
            "text": " ".join(tokens[position:position + span]),
            "position": position,
            "ambiguous": len(matched) > 1,
        })
        position += span
    return occurrences


def subject_matches(subject: object, probe_key: str, index) -> list[dict]:
    """The occurrences in `subject` that name `probe_key`."""
    return [occurrence for occurrence in find_occurrences(subject, index)
            if probe_key in occurrence["probes"]]


def _match(probe_key: str, source: str, reason: str, evidence_body: dict,
           occurrences: list[dict]) -> dict:
    competing = sorted({key for occurrence in occurrences
                        for key in occurrence["probes"] if key != probe_key})
    return {
        "probe": probe_key,
        "source": source,
        "reason": reason,
        # SUBJECT-match ambiguity: reported, and still a match. Distinct
        # from the source-structure ambiguity that produces source-error.
        "ambiguous": any(o["ambiguous"] for o in occurrences),
        "competing_probes": competing,
        "matched_text": [o["text"] for o in occurrences],
        "evidence": evidence_body,
    }


# ==========================================================================
# Source 1: active `$test` runs
# ==========================================================================

def parse_timestamp(value: object) -> datetime:
    """A timezone-qualified ISO-8601 instant, or `ValueError`.

    A NAIVE timestamp is refused rather than assumed to be UTC: it
    cannot be compared against the evaluation time without guessing a
    zone, and guessing is exactly what fail-closed forbids. The
    coordinator writes `...Z`, which `fromisoformat` accepts only from
    Python 3.11, so it is normalized here.
    """
    if not isinstance(value, str) or not value.strip():
        raise ValueError("not a timestamp")
    text = value.strip()
    if text.endswith(("Z", "z")):
        text = text[:-1] + "+00:00"
    parsed = datetime.fromisoformat(text)
    if parsed.tzinfo is None or parsed.tzinfo.utcoffset(parsed) is None:
        raise ValueError(f"{value!r} carries no timezone")
    return parsed


def _active_stamp(run: dict) -> tuple[str, datetime]:
    """The instant an active run is measured from, or `SourceError`.

    `heartbeat_at` when one is recorded, otherwise `claimed_at`. The
    fallback is for ABSENCE only: a field that IS recorded but carries an
    unusable value is malformed active-run state, and malformed
    active-run state fails closed. `recorded_fields` is what separates
    the two — the normalized `null` alone cannot.
    """
    recorded = set(run.get("recorded_fields") or ())
    run_id = run.get("run_id") or "<unidentified run>"
    for field in ("heartbeat_at", "claimed_at"):
        value = run.get(field)
        if value is not None:
            try:
                return field, parse_timestamp(value)
            except ValueError as exc:
                raise SourceError(
                    f"$test run {run_id} is in active state "
                    f"{run.get('run_state')!r} but its {field} "
                    f"({value!r}) is not a timezone-qualified ISO-8601 "
                    f"instant: {exc}") from None
        if field in recorded:
            raise SourceError(
                f"$test run {run_id} is in active state "
                f"{run.get('run_state')!r} and records a {field}, but its "
                f"value is unusable. Repair the record, or let the "
                f"coordinator complete the run.")
    raise SourceError(
        f"$test run {run_id} is in active state {run.get('run_state')!r} but "
        f"records neither heartbeat_at nor claimed_at, so whether it is still "
        f"active cannot be decided.")


def evaluate_test_runs(probe_key: str, now: datetime, *,
                       state_root=None, repo_root=None) -> list[dict]:
    """Matches from active `$test` runs. Raises `SourceError` to fail closed."""
    try:
        record = evidence.read_probe_evidence(
            probe_key, state_root=state_root, repo=repo_root)
    except evidence.EvidenceRejected as exc:
        raise SourceError(f"the $test record could not be read: {exc}") from None

    # Only ACTIVE-RUN state fails the scan. A `report` diagnostic is one
    # finished run's interpretation artifact and says nothing about
    # whether any run is active.
    blocking = [entry["message"] for entry in record.get("diagnostics_detail", [])
                if entry.get("scope") in (evidence.SCOPE_REGISTRY,
                                          evidence.SCOPE_RECORD)]
    if blocking:
        raise SourceError(
            "the $test registry's active-run state could not be read "
            "completely: " + "; ".join(blocking))

    matches: list[dict] = []
    for run in record["runs"]:
        state = run.get("run_state")
        if state in TERMINAL_STATUSES:
            continue
        if state not in ACTIVE_STATUSES:
            raise SourceError(
                f"$test run {run.get('run_id') or '<unidentified run>'} is in "
                f"state {state!r}, which is neither an active state "
                f"({', '.join(ACTIVE_STATUSES)}) nor a terminal one "
                f"({', '.join(TERMINAL_STATUSES)}). Whether it is in flight "
                f"cannot be decided.")
        field, stamp = _active_stamp(run)
        age = now - stamp
        if age > STALE_HORIZON:
            # Stale, therefore inactive — and left exactly as it is. This
            # snapshot never rewrites a record it disbelieves.
            continue
        matches.append(_match(
            probe_key, SOURCE_TEST_RUN,
            f"an active $test run ({state}) is already working this probe",
            {
                "run_id": run.get("run_id"),
                "test_id": run.get("test_id"),
                "test_kind": run.get("test_kind"),
                "run_state": state,
                "timestamp_field": field,
                "timestamp": run.get(field),
                "age_seconds": age.total_seconds(),
                "state_root": record["state_root"],
            },
            []))
    return matches


# ==========================================================================
# Sources 2 and 3: open issues and open pull requests
# ==========================================================================

def resolve_target_repository(repo_root=None) -> str:
    """`owner/name` for the checked-out repository's `origin` remote.

    Deterministic, and the only definition of "the target repository"
    this component has. An absent, unparseable or non-GitHub `origin` is
    a source error naming the remote — never a hard-coded fallback and
    never a silent skip of the issue and pull-request sources.
    """
    root = str(repo_root or probe_engine.REPO_ROOT)
    try:
        done = subprocess.run(["git", "-C", root, "remote", "get-url", "origin"],
                              text=True, capture_output=True, timeout=30)
    except (OSError, subprocess.SubprocessError) as exc:
        raise SourceError(
            f"could not read the `origin` remote of {root}: {exc}") from None
    if done.returncode != 0:
        detail = done.stderr.strip() or f"git exited {done.returncode}"
        raise SourceError(
            f"{root} has no readable `origin` remote, so the target "
            f"repository for the open issue and pull-request scans is "
            f"undefined: {detail}")
    url = done.stdout.strip()
    parsed = parse_github_remote(url)
    if parsed is None:
        raise SourceError(
            f"the `origin` remote of {root} is {url!r}, which does not name a "
            f"{GITHUB_HOST} repository, so the target repository for the open "
            f"issue and pull-request scans is undefined.")
    return parsed


def parse_github_remote(url: object) -> str | None:
    """`owner/name` from a GitHub remote URL, or None.

    Covers the three shapes git actually hands back: `scp`-style ssh
    (`git@github.com:owner/name.git`), a URL with a scheme
    (`https://`, `ssh://`, with or without userinfo or a port), and the
    same with the `.git` suffix omitted.
    """
    if not isinstance(url, str) or not url.strip():
        return None
    text = url.strip()
    host_and_path: str | None = None
    if "://" in text:
        host_and_path = text.split("://", 1)[1]
    elif ":" in text and "/" in text.split(":", 1)[1]:
        host_and_path = text.replace(":", "/", 1)
    if host_and_path is None:
        return None
    if "@" in host_and_path.split("/", 1)[0]:
        host_and_path = host_and_path.split("@", 1)[1]
    host, _, path = host_and_path.partition("/")
    if host.split(":", 1)[0].lower() != GITHUB_HOST:
        return None
    parts = [part for part in path.split("/") if part]
    if len(parts) != 2:
        return None
    owner, name = parts[0], parts[1]
    if name.endswith(".git"):
        name = name[:-4]
    if not owner or not name:
        return None
    return f"{owner}/{name}"


class GitHubCli:
    """The default transport: `gh api`, one EXPLICIT page at a time.

    Deliberately not `gh api --paginate`: pagination is this module's own
    loop so that "every page was retrieved" is a property the self-test
    can exercise against a fake transport, rather than a promise made by
    a binary it must not invoke.
    """

    def __init__(self, timeout: float = GH_TIMEOUT_SECONDS) -> None:
        self.timeout = timeout

    def __call__(self, path: str, params: dict[str, str]) -> list:
        query = "&".join(f"{key}={value}" for key, value in sorted(params.items()))
        endpoint = f"{path}?{query}" if query else path
        try:
            done = subprocess.run(
                ["gh", "api", "-H", "Accept: application/vnd.github+json", endpoint],
                text=True, capture_output=True, timeout=self.timeout)
        except FileNotFoundError:
            raise SourceError(
                "the GitHub CLI (`gh`) is not installed, so the open issue and "
                "pull-request lists cannot be retrieved. Install it, or pass a "
                "transport to evaluate_probe_inflight().") from None
        except (OSError, subprocess.SubprocessError) as exc:
            raise SourceError(f"`gh api {endpoint}` failed: {exc}") from None
        if done.returncode != 0:
            detail = done.stderr.strip() or f"gh exited {done.returncode}"
            raise SourceError(f"`gh api {endpoint}` failed: {detail}")
        try:
            return json.loads(done.stdout)
        except ValueError as exc:
            raise SourceError(
                f"`gh api {endpoint}` did not return JSON: {exc}") from None


def _paginate(transport, path: str, params: dict[str, str]) -> list[dict]:
    """Every page of a list endpoint, or `SourceError`.

    A short page ends the walk. A page that is not a list of objects, or
    a walk that never shortens, is a source error: an incomplete list
    must never be presented as a complete one.
    """
    items: list[dict] = []
    for page in range(1, MAX_PAGES + 1):
        batch = transport(path, {**params, "per_page": str(PER_PAGE),
                                 "page": str(page)})
        if not isinstance(batch, list):
            raise SourceError(
                f"page {page} of {path} is not a JSON list, so the complete "
                f"list could not be retrieved.")
        for entry in batch:
            if not isinstance(entry, dict):
                raise SourceError(
                    f"page {page} of {path} contains a non-object entry, so the "
                    f"complete list could not be interpreted.")
        items.extend(batch)
        if len(batch) < PER_PAGE:
            return items
    raise SourceError(
        f"{path} did not end after {MAX_PAGES} pages of {PER_PAGE}; refusing to "
        f"present a truncated list as complete.")


OPEN_STATE = "open"


def _tracker_subject(entry: dict, repository: str, kind: str) -> tuple[int, str]:
    """`(number, title)` for one tracker record, or `SourceError`.

    The title is the ONLY subject this component matches a tracker item
    on, so a record whose title cannot be read is a record whose subject
    cannot be interpreted — and a scan containing one has not been read
    completely, whatever the rest of the page said. `normalize_tokens`
    would answer "no tokens" for a missing or non-string title, which is
    indistinguishable from a genuine non-match and would let the scan
    report `clear`.

    The number is validated for the same reason from the other end: the
    evidence contract promises number, title and URL, and a number that
    is not one makes the match uninspectable. `bool` is excluded
    explicitly — it is an `int` in Python, and issue #True is not a
    thing. The URL is deliberately NOT required: it can be reconstructed
    from the repository and number, so its absence costs no evidence.
    """
    number, title = entry.get("number"), entry.get("title")
    if isinstance(number, bool) or not isinstance(number, int):
        raise SourceError(
            f"an open {kind} in {repository} records no usable number "
            f"({number!r}), so the complete list could not be interpreted.")
    if not isinstance(title, str) or not title.strip():
        raise SourceError(
            f"open {kind} #{number} in {repository} records no usable title "
            f"({title!r}), and the title is the only subject a {kind} is "
            f"matched on, so whether it names this probe cannot be decided.")
    return number, title


def _is_open(entry: dict, number: int, repository: str, kind: str) -> bool:
    """Whether a record is open, refusing to guess when it does not say.

    The endpoints are already asked for `state=open`, so this is a
    belt-and-braces check on the answer rather than the primary filter —
    but a record that does not say whether it is open is one whose
    eligibility cannot be decided, and closed items must not be able to
    exclude a probe.
    """
    state = entry.get("state")
    if not isinstance(state, str) or not state.strip():
        raise SourceError(
            f"open {kind} #{number} in {repository} records no usable state "
            f"({state!r}), so whether it is still open cannot be decided.")
    return state == OPEN_STATE


def _tracker_evidence(entry: dict, repository: str, path_segment: str, *,
                      include_draft: bool = False) -> dict:
    """Number, title and URL — the inspectable evidence for a tracker item.

    `html_url` is used when the record carries one and is reconstructed
    otherwise, so evidence is never missing a URL just because a
    transport returned a lean record.
    """
    number = entry.get("number")
    url = entry.get("html_url")
    if not isinstance(url, str) or not url:
        url = f"https://{GITHUB_HOST}/{repository}/{path_segment}/{number}"
    body = {
        "number": number,
        "title": entry.get("title"),
        "url": url,
        "repository": repository,
    }
    if include_draft:
        # Drafts count as open, so whether an item IS one is part of the
        # evidence a reader needs to judge the exclusion.
        body["draft"] = bool(entry.get("draft"))
    return body


def evaluate_issues(probe_key: str, repository: str, transport, index) -> list[dict]:
    """Matches among every page of the repository's OPEN issues.

    GitHub's issues endpoint returns pull requests too; those carry a
    `pull_request` key and are dropped here so each is counted once, by
    the pull-request scan.
    """
    matches: list[dict] = []
    for entry in _paginate(transport, f"repos/{repository}/issues",
                           {"state": OPEN_STATE}):
        if "pull_request" in entry:
            continue
        number, title = _tracker_subject(entry, repository, "issue")
        if not _is_open(entry, number, repository, "issue"):
            continue
        occurrences = subject_matches(title, probe_key, index)
        if not occurrences:
            continue
        matches.append(_match(
            probe_key, SOURCE_ISSUE,
            "an open issue names this probe in its title",
            _tracker_evidence(entry, repository, "issues"), occurrences))
    return matches


def evaluate_pull_requests(probe_key: str, repository: str, transport,
                           index) -> list[dict]:
    """Matches among every page of the repository's OPEN pull requests.

    `state=open` includes drafts, which count; closed and merged pull
    requests are not returned and do not count.
    """
    matches: list[dict] = []
    for entry in _paginate(transport, f"repos/{repository}/pulls",
                           {"state": OPEN_STATE}):
        number, title = _tracker_subject(entry, repository, "pull request")
        if not _is_open(entry, number, repository, "pull request"):
            continue
        occurrences = subject_matches(title, probe_key, index)
        if not occurrences:
            continue
        matches.append(_match(
            probe_key, SOURCE_PULL_REQUEST,
            "an open pull request names this probe in its title",
            _tracker_evidence(entry, repository, "pull", include_draft=True),
            occurrences))
    return matches


# ==========================================================================
# Source 4: the findings reports
# ==========================================================================

class ReportError(SourceError):
    """A report's checklist/heading state could not be lexed unambiguously."""


def checklist_marker(rest: str, line_no: int, key: str, where: str) -> str:
    """The marker a checklist entry declares, or `""` when bare.

    `findings_report_audit.py`'s rule, unchanged: the marker OPENS the
    segment after the entry's FINAL em-dash separator, so trailing prose
    (a `[deferred]` precondition, a note on what an issue covers) is
    ignored. A marker token anywhere else is a lex failure.
    """
    _head, separator, tail = rest.rpartition(MARKER_SEPARATOR)
    if separator:
        found = LEADING_MARKER_RE.match(tail)
        if found:
            return found.group(0)
    if MARKER_RE.search(rest):
        raise ReportError(
            f"{where}:{line_no}: {key}'s checklist entry carries a status marker "
            f"somewhere other than immediately after its final "
            f"'{MARKER_SEPARATOR.strip()}' separator, so which token is the "
            f"marker is ambiguous.")
    return ""


def parse_report(text: str, family: str, where: str) -> list[dict]:
    """Every finding in one report, with its status resolved.

    Raises `ReportError` — hence `SourceError` — on any partial,
    inconsistent, duplicate, foreign-family or unlexable state. Guessing
    such a report clear is precisely the unsafe answer this component
    exists to avoid.
    """
    checklist: dict[str, dict] = {}
    headings: dict[str, dict] = {}
    for line_no, raw in enumerate(text.split("\n"), 1):
        line = raw.rstrip()
        if CHECKLIST_SHAPE_RE.match(line):
            found = CHECKLIST_RE.match(line)
            if not found:
                raise ReportError(
                    f"{where}:{line_no}: checklist line is not a "
                    f"`- [ ] {family}-<n>. <title>` entry: {line}")
            if found.group(2) != family:
                raise ReportError(
                    f"{where}:{line_no}: checklist entry declares finding key "
                    f"family {found.group(2)!r}, but this report owns "
                    f"{family!r}: {line}")
            key = f"{family}-{found.group(3)}"
            if key in checklist:
                raise ReportError(
                    f"{where}:{line_no}: {key} has a second checklist entry "
                    f"(the first is at line {checklist[key]['line']}).")
            checklist[key] = {
                "line": line_no,
                "checked": found.group(1) == "x",
                "marker": checklist_marker(found.group(4).rstrip(), line_no,
                                           key, where),
            }
            continue
        if HEADING_SHAPE_RE.match(line):
            found = HEADING_RE.match(line)
            if not found:
                raise ReportError(
                    f"{where}:{line_no}: finding heading is not a "
                    f"`### [marker] {family}-<n>. <title>` heading: {line}")
            if found.group(2) != family:
                raise ReportError(
                    f"{where}:{line_no}: finding heading declares finding key "
                    f"family {found.group(2)!r}, but this report owns "
                    f"{family!r}: {line}")
            key = f"{family}-{found.group(3)}"
            if key in headings:
                raise ReportError(
                    f"{where}:{line_no}: {key} has a second finding heading "
                    f"(the first is at line {headings[key]['line']}).")
            headings[key] = {
                "line": line_no,
                "marker": found.group(1) or "",
                "heading": line,
            }

    findings: list[dict] = []
    for key in sorted(set(checklist) | set(headings),
                      key=lambda k: int(k.split("-")[1])):
        entry, heading = checklist.get(key), headings.get(key)
        if heading is None:
            raise ReportError(
                f"{where}:{entry['line']}: {key}'s checklist entry has no "
                f"`### ` finding heading, so its status is indeterminate.")
        if entry is None:
            raise ReportError(
                f"{where}:{heading['line']}: {key}'s finding heading has no "
                f"`## Status` checklist entry, so its status is indeterminate.")
        if entry["marker"] != heading["marker"]:
            raise ReportError(
                f"{where}: {key}'s heading (line {heading['line']}) says "
                f"{heading['marker'] or '(no marker)'} while its checklist "
                f"entry (line {entry['line']}) says "
                f"{entry['marker'] or '(no marker)'}; the two disagree, so "
                f"whether it is open cannot be decided.")
        if not heading["marker"] and entry["checked"]:
            raise ReportError(
                f"{where}: {key} carries no disposition marker yet its "
                f"checklist entry (line {entry['line']}) is checked; "
                f"whether it is open cannot be decided.")
        findings.append({
            "key": key,
            "heading": heading["heading"],
            "line": heading["line"],
            # The heading marker is authoritative. A bare heading paired
            # with its unchecked, unmarked checklist entry is unprocessed.
            "open": not heading["marker"],
        })
    return findings


def _report_file_state(path: Path, where: str) -> tuple[bool, bool]:
    """`(present, usable)` for a report path, failing closed on anything else.

    `evidence.entry_state` is the shared primitive, deliberately not a
    second copy: `Path.exists` / `.is_dir` / `.is_file` all SWALLOW
    `OSError` and answer False, so an unstattable path — a denied
    directory, a parent that is not a directory, an I/O error — reads
    exactly like a file that is simply not there, and the optional
    docs-wip scope would then skip it and go on to answer `clear`. Two
    hand-rolled copies of that rule would be two chances to drift back.

    Only true absence is absence here; every stat failure is a source
    error naming the path, and a symlink whose target is gone is PRESENT
    and unusable rather than missing.
    """
    present, mode, failure = evidence.entry_state(path)
    if failure is not None:
        raise SourceError(f"{where} could not be examined at {path}: {failure}")
    return present, mode is not None and stat.S_ISREG(mode)


def _read_report(path: Path, family: str, where: str) -> list[dict]:
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, ValueError, UnicodeDecodeError) as exc:
        raise SourceError(f"{where} could not be read: {exc}") from None
    return parse_report(text, family, where)


def resolve_docs_worktree(repo_root=None) -> Path | None:
    """The `docs-wip` worktree resolved BY BRANCH, or None when there is none.

    `probe_census.resolve_docs_worktree` is the shipped idiom and is
    reused whole. The ONE divergence: that helper treats a missing or
    unusable docs worktree as an actionable stop, because it is about to
    write the census there. Here it is a normal no-evidence state — the
    checked-out reports are the required source and are always read.

    "Unresolvable" deliberately covers every way that helper declines,
    not only "no worktree is on that branch": a `prunable` registration,
    a registered path that is not a usable checkout, and a failing `git
    worktree list` all land here as no-evidence. That is the approved
    contract for this component rather than an oversight — the docs
    worktree is CONSERVATIVE EXTRA evidence, so its unavailability may
    not block an evaluation the required checked-out reports can answer.
    Note the boundary: this is about resolving the WORKTREE. Once one
    resolves, a report inside it that is present but unexaminable is a
    source error, not no-evidence (see `evaluate_reports`).
    """
    try:
        return probe_census.resolve_docs_worktree(repo_root)
    except probe_census.DocsWorktreeMissing:
        return None


def evaluate_reports(probe_key: str, index, *, repo_root=None,
                     docs_root=_UNSET) -> list[dict]:
    """Matches among open findings in BOTH report worktrees.

    The checked-out reports are REQUIRED: an absent, unreadable or
    unparseable one is a source error. The `docs-wip` copies are
    conservative extra evidence: an absent worktree, and a present
    worktree that simply does not carry one of the four reports, are both
    no-evidence. A `docs-wip` report that IS present but unreadable or
    unparseable is a source error, because a broken source cannot be read
    as an absent one — and "present" here means present on the
    filesystem, not merely present as a readable regular file.
    """
    checkout = Path(repo_root or probe_engine.REPO_ROOT)
    docs = resolve_docs_worktree(repo_root) if docs_root is _UNSET else docs_root
    scopes = [(WORKTREE_CHECKOUT, checkout, True)]
    if docs is not None:
        scopes.append((WORKTREE_DOCS, Path(docs), False))

    matches: list[dict] = []
    for role, root, required in scopes:
        for relpath, family in REPORTS:
            path = root / relpath
            where = f"{role}:{relpath}"
            # ABSENT and PRESENT-BUT-UNUSABLE are different answers, and
            # the convenience predicates conflate them: a directory, a
            # socket, a broken symlink and an unstattable path all read
            # as "not a file". Only true absence may be no-evidence —
            # anything that is THERE but not a readable regular file is
            # damage, and damage fails closed even in the optional
            # docs-wip scope.
            present, usable = _report_file_state(path, where)
            if not usable:
                if present:
                    raise SourceError(
                        f"{where} exists at {path} but is not a readable "
                        f"regular file, so it could not be read completely.")
                if required:
                    raise SourceError(
                        f"{where} is a required findings report but is not "
                        f"present at {path}.")
                continue
            for finding in _read_report(path, family, where):
                if not finding["open"]:
                    continue
                occurrences = subject_matches(finding["heading"], probe_key, index)
                if not occurrences:
                    continue
                matches.append(_match(
                    probe_key, SOURCE_REPORT,
                    "an open findings entry names this probe in its heading",
                    {
                        "worktree": role,
                        "worktree_path": str(root),
                        "report_path": relpath,
                        "finding_key": finding["key"],
                        "heading": finding["heading"],
                        "line": finding["line"],
                    },
                    occurrences))
    return matches


# ==========================================================================
# The evaluation
# ==========================================================================

def default_github_transport():
    """The transport used when a caller supplies none."""
    return GitHubCli()


def evaluate_probe_inflight(probe_key: str, *,
                            now: datetime | None = None,
                            repo_root=None,
                            state_root=None,
                            docs_root=_UNSET,
                            target_repository: str | None = None,
                            github=None,
                            identity_index=None) -> dict:
    """Whether one registered probe already has related work in flight.

    Returns an eligibility SNAPSHOT document whose `result` is exactly
    one of `clear`, `in-flight` or `source-error`. Raises
    `InflightRejected` only for a probe key `probe_runner_registry.PROBES` does not
    register — a caller mistake, never evidence that nothing is in
    flight.

    Every source is attempted, so one broken source does not hide the
    others' evidence; but a source error is decisive, and the verdict is
    `source-error` even when other sources already matched, because a
    partial scan must never present itself as determinate.
    """
    try:
        evidence.require_known_probe(probe_key)
    except evidence.EvidenceRejected as exc:
        raise InflightRejected(str(exc)) from None

    moment = now or datetime.now(timezone.utc)
    if moment.tzinfo is None or moment.tzinfo.utcoffset(moment) is None:
        raise InflightRejected(
            "the evaluation time must be timezone-aware; a naive instant "
            "cannot be compared against the coordinator's UTC timestamps.")
    index = identity_index if identity_index is not None else build_identity_index()
    if not any(probe_key in owners for owners in index.values()):
        # An index that does not own the key would answer "no
        # occurrences" for every subject in every source, which reads as
        # a completely clean scan. That is the one way a caller can make
        # this component answer `clear` without looking at anything.
        raise InflightRejected(
            f"the supplied identity index registers no forms for probe "
            f"{probe_key!r}, so every subject would read as a non-match.")
    transport = github if github is not None else default_github_transport()

    document: dict = {
        "schema": INFLIGHT_SCHEMA,
        "probe": probe_key,
        "script": evidence.probe_script(probe_key),
        "test_ids": evidence.test_ids_for_probe(probe_key),
        "target_repository": None,
        "evaluated_at": moment.isoformat(),
        "result": RESULT_CLEAR,
        "matches": [],
        "source_errors": [],
        "sources": {source: "read" for source in SOURCES},
    }

    def attempt(source: str, work) -> None:
        try:
            document["matches"].extend(work())
        except SourceError as exc:
            document["sources"][source] = "error"
            document["source_errors"].append({"source": source,
                                              "detail": str(exc)})

    attempt(SOURCE_TEST_RUN, lambda: evaluate_test_runs(
        probe_key, moment, state_root=state_root, repo_root=repo_root))

    repository = target_repository
    if repository is None:
        try:
            repository = resolve_target_repository(repo_root)
        except SourceError as exc:
            # One unresolvable target breaks BOTH tracker scans; each is
            # reported so neither reads as skipped.
            for source in (SOURCE_ISSUE, SOURCE_PULL_REQUEST):
                document["sources"][source] = "error"
                document["source_errors"].append({"source": source,
                                                  "detail": str(exc)})
    if repository is not None:
        document["target_repository"] = repository
        attempt(SOURCE_ISSUE, lambda: evaluate_issues(
            probe_key, repository, transport, index))
        attempt(SOURCE_PULL_REQUEST, lambda: evaluate_pull_requests(
            probe_key, repository, transport, index))

    attempt(SOURCE_REPORT, lambda: evaluate_reports(
        probe_key, index, repo_root=repo_root, docs_root=docs_root))

    if document["source_errors"]:
        document["result"] = RESULT_SOURCE_ERROR
    elif document["matches"]:
        document["result"] = RESULT_IN_FLIGHT
    return document


# ==========================================================================
# Presentation
# ==========================================================================

def render(document: dict) -> str:
    lines = [
        f"in-flight check for probe {document['probe']} "
        f"({document['script']}) -> {document['result']}",
        f"  evaluated at: {document['evaluated_at']}",
        f"  target repository: {document['target_repository'] or 'unresolved'}",
    ]
    for source in SOURCES:
        found = [m for m in document["matches"] if m["source"] == source]
        lines.append(f"  {source}: {document['sources'][source]}, "
                     f"{len(found)} match(es)")
    for match in document["matches"]:
        lines.append(f"    [{match['source']}] {match['reason']}")
        for name in sorted(match["evidence"]):
            lines.append(f"      {name}: {match['evidence'][name]}")
        if match["ambiguous"]:
            lines.append(f"      ambiguous with: "
                         f"{', '.join(match['competing_probes'])}")
    for problem in document["source_errors"]:
        lines.append(f"  source error [{problem['source']}]: {problem['detail']}")
    if document["result"] == RESULT_CLEAR:
        lines.append("  clear: every required source was read completely and "
                     "nothing matched.")
    lines.append("  (read-only snapshot: nothing was claimed, launched, locked "
                 "or written)")
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Whether one registered probe already has related work in "
                    "flight (read-only; claims nothing and launches nothing).")
    parser.add_argument("--probe", required=True,
                        help="a registered probe_runner_registry.PROBES key, e.g. injury_log")
    parser.add_argument("--json", action="store_true",
                        help="emit the eligibility document instead of a table")
    args = parser.parse_args(argv)

    try:
        document = evaluate_probe_inflight(args.probe)
    except InflightRejected as exc:
        print(f"probe_inflight: {exc}", file=sys.stderr)
        return EXIT_REJECTED

    if args.json:
        print(json.dumps(document, indent=2, sort_keys=True, allow_nan=False))
    else:
        print(render(document))
    return (EXIT_SOURCE_ERROR if document["result"] == RESULT_SOURCE_ERROR
            else EXIT_OK)


if __name__ == "__main__":
    sys.exit(main())
