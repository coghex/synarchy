#!/usr/bin/env python3
"""File an issue instead of a PR when the bug is in the engine (#1438).

`tools/deflake_diagnosis.py` (#1437) decides mechanically which route
one measurement handoff supports and emits a
`deflake-diagnosis-outcome/v1` record naming it. `production-defect` is
the route it takes when the diagnosis is that PRODUCTION CODE OR SHIPPED
SCRIPTS are wrong — a real race, not a racy test. This module owns that
ending, and it owns nothing else.

    python3 tools/deflake_issue.py --handoff <document.json> --origin claude
    python3 tools/deflake_issue.py --handoff <document.json> --dry-run
    python3 tools/test_deflake_diagnosis.py       # the deterministic gate
    python3 tools/test_deflake_diagnosis.py --only issue      # this module's

An engine race that reaches a pull request as a probe adjustment is a
bug converted into a permanent green light. So this route publishes ONE
review-ready tracker issue carrying the measured evidence, records the
issue's identity in the probe's census row, and STOPS. It does not touch
the probe, it does not touch production code, and it opens no pull
request — the two boundaries below exist so that silence is a branch a
gate can exercise rather than a call nobody happened to write.

What "engine" means here
------------------------
The approved review fixes the shorthand: production Haskell under `src/`
and `app/`, and shipped Lua under `scripts/`. Probe implementation under
`tools/*_probe.py` is explicitly NOT that — a wrong probe is #1437's
repair route, and the harness modules are its measurement apparatus.
This module never inspects a diff to decide; the CALLER's explicit
production-defect diagnosis is the branch input, exactly as the issue
body assigns that judgement to the calling agent, and a second heuristic
classifier here would be a second opinion nobody asked for.

One entry gate, two consumers
-----------------------------
The envelope is `deflake-outcome-handoff/v1` — the SAME document
`tools/deflake_outcome.py` (#1439) reads — and it is validated by
`tools/deflake_handoff.py`, the contract BOTH consumers depend on,
through its `require_handoff` parameterized by `OWNED` below. Every rule
the two siblings share is therefore checked once: the producer record,
the measurement binding, the whole ordered descriptor, the worktree
boundary, the rebuilt artifact list, the exit contract, the aggregate
reconciliation. Only the owned-route table differs, and it is data
rather than a fork of the gate.

The contract is a third module rather than one sibling reading the
other. Neither of epic #1426's consumers is the other's prerequisite, so
a shared rule owned by #1439 would be a rule #1438 could not rely on;
this module imports the contract and never `deflake_outcome`.

`require_reproduced` is called for the same reason. #1437 asks it of
every route past the `cannot-reproduce` fork, `production-defect` is one
of those routes, and a private re-reading here could drift from the
producer that already refused what it refuses: a batch that failed
somewhere else, or stayed at or below X with every target present,
reproduced nothing to attribute to the engine.

The issue itself, and the evidence in it, are owned below
---------------------------------------------------------
Two of this route's load-bearing arguments are stated once, in the
module that implements each, rather than restated here where they would
be free to drift from the code:

* `deflake_issue_document.py` owns why quoted content cannot forge the
  review-routing marker — one neutralization funnel over the whole
  assembled body, two real markers appended after it, and a finished
  text that is CHECKED rather than trusted — and why every part of the
  body is required, so that only the second and later runs' evidence may
  be dropped to fit and an unfittable body is refused rather than sliced.
* `deflake_issue_evidence.py` owns why the artifact tree is TRAVERSED
  rather than resolved, and why an attempt whose retained artifacts have
  all been pruned is refused rather than filed on machine-local
  pathnames nobody else can open.

What belongs here is what neither can decide alone: this module reads
those retained artifacts through the evidence owner and renders them
through the document owner, and the numbers beside them come from the
measurement documents the diagnosis judged — the failure numerator,
denominator and rate, the timeout count, every declared check's
PASS/FAIL/MISSING tally, the measured commit, the run count and the RTS
capability setting.

Publication is idempotent, including across a crash
---------------------------------------------------
A recorded outcome is the completion marker: resuming an attempt the
census already holds reuses its stored issue, touches the tracker not at
all, and appends nothing.

That alone is not enough, because the window that matters is the one
where issue creation TOOK EFFECT and its identity was never durably
recorded — a timeout, a crash, or a census write that refused between
the two.

So every diagnosis carries a stable PUBLICATION KEY, derived from the
handoff rather than supplied by a caller, and written into the issue
body as a marker line. Before anything is created, the key is reconciled
against the tracker: an issue already carrying it IS this attempt's
issue, and it is recorded rather than duplicated. The marker is verified
in the returned body rather than trusted from a search index, because a
search matches text anywhere and an issue that merely QUOTES a key is
not the one that was filed under it — and since a filed issue quotes
engine logs, a marker inside a code fence is evidence about some other
issue, so only a standalone line outside every fence counts.

A reconciled issue also supplies its OWN `issue-origin` brand, read off
that same body. The brand decides which agent reviews it, and the issue
was filed by whoever filed it: a Claude-origin creation resumed by a
Codex invocation still routes to Claude's opposite brand, and recording
the retry's brand would put a second, false answer in the durable
history. An issue carrying this attempt's key but no readable origin
marker is not one this workflow filed, and is a publication failure.

BOTH recoveries run before anything is RENDERED. Rendering re-reads the
retained artifacts off disk and those are transient, while an issue and
a census record are durable — so an attempt that already reached the
tracker resumes on what is durable, however long ago the artifact tree
was swept. Only an attempt with no issue at all renders, which is the
one case where there is genuinely something to file. What runs earlier
still is the route's own evidence check, which needs no artifact: a
handoff this route does not support is refused without so much as a
search.

What that does not cover, and deliberately: two invocations of one
brand-new attempt running at the same instant can both miss the
reconcile and both create. The census still refuses the loser, so the
DURABLE history holds one outcome — and serializing the attempt itself
is `tools/probe_claim.py`'s (#1434) per-probe claim, taken by the
workflow before either of these steps. Holding the census's blocking
`flock` across a remote call instead would let one hung request stall
every unrelated census writer, which is a worse trade for a narrower
window.

Failure leaves the attempt resumable
------------------------------------
A publication that fails stops with an actionable error, records
nothing, and never falls through to the probe-adjustment or fix-PR path.
A census write that refuses does the same, leaving the file
byte-identical: the issue may exist remotely, and the next invocation
reconciles the key, finds it, and records it once.

Where each rule now lives
-------------------------
This module is the route's public import façade, its admission gate and
its ordering authority; the implementation has four owners beside it,
none of which imports this module or any other consumer.

* `deflake_issue_evidence.py` — the retained artifacts: failing-run
  discovery in role order, the descriptor-relative traversal below each
  declared artifact root, the bounded tail reads, the run and
  engine-log excerpts, every evidence bound, and the refusal when
  nothing reviewable remains. A leaf: it renders nothing and publishes
  nothing.
* `deflake_issue_document.py` — the issue itself: the publication-key
  schema and derivation, the two markers and their recognition, the
  fence-aware prose scan, the neutralization funnel, the title, the
  measurement, diagnosis and evidence sections, the body and diagnosis
  bounds, and the evidence trimming. Also a leaf; the evidence it
  renders arrives as data.
* `deflake_issue_tracker.py` — the boundary: the `Publication`
  interface, the `gh`-backed implementation, the publication-key
  search, issue creation, the JSON and URL parsing, and the two issue
  identity validators. It CALLS the document owner's `carries_key` and
  `body_origin` rather than restating them, which is the one permitted
  edge between two extracted owners; it touches no artifact and no
  census.
* `deflake_issue_record.py` — the durable ending: the production-defect
  census document, the stored-record lookup, the retry reconciler, the
  `Published` result, and the route qualification a record cannot be
  written without. It performs no tracker I/O and renders no issue.

What stays here is what none of them can own alone: the
`production-defect` ownership and role policy, the two boundary tables,
the shared-handoff and diagnosis validation, the combined accepted
`Defect`, `render`'s composition across three owners, `publish`'s exact
statement order, and the command line. `render` in particular is the
façade's because placing it on the document owner would force
document → evidence and document → route-admission edges and make the
family cyclic.
"""
from __future__ import annotations

import argparse
import copy
import importlib
import json
import os
import sys
from pathlib import Path

# `tools/` carries no `__init__.py`, so it is an implicit namespace
# package, and every module in it has TWO import spellings: the
# `tools.<name>` one used from the repository root, and the bare one a
# caller who put `tools/` on `sys.path` uses. Python treats those as
# DIFFERENT modules, so resolving an owner by bare name from a facade
# that was itself loaded as `tools.deflake_issue` loads a SECOND copy of
# it. Every guarantee below would then be false under that spelling:
# `tools.deflake_issue.issue_body is not
# tools.deflake_issue_document.issue_body`, `except
# tools.deflake_issue.PublicationFailed` stops catching what
# `tools.deflake_issue_tracker` raises, and a substituted
# `MAX_BODY_CHARS` lands on a module nothing renders through.
#
# So every dependency is resolved under the spelling that loaded THIS
# module, and the path insertion below remains for the bare spelling and
# for running this file directly as a script.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))


def _sibling(name: str):
    """One `tools/` module, under the spelling that loaded this one."""
    return importlib.import_module(
        f"{__package__}.{name}" if __package__ else name)


deflake_contract = _sibling("deflake_contract")
deflake_handoff = _sibling("deflake_handoff")
probe_census = _sibling("probe_census")

_document = _sibling("deflake_issue_document")
_evidence = _sibling("deflake_issue_evidence")
_record = _sibling("deflake_issue_record")
_tracker = _sibling("deflake_issue_tracker")

# The compatibility surface, and the facade's own composition inputs in
# one block: every name below is the CANONICAL object its owner defines,
# bound here and not copied. So `deflake_issue.PublicationFailed is
# deflake_issue_tracker.PublicationFailed`, an `except` against either
# catches what the other raises, and a `FakePublication` subclassing
# `deflake_issue.Publication` subclasses the real interface.
#
# `MAX_BODY_CHARS` is deliberately NOT among them, and it is the only
# public name of the four owners that is not. It is the one constant of
# this family a caller SUBSTITUTES — the deterministic gate lowers it to
# exercise the unfittable-body refusal — and `issue_body` reads it out
# of `deflake_issue_document`'s own globals. A second binding here would
# accept that assignment and change nothing at all, leaving the refusal
# unexercised while the test appeared to drive it.
COMMENT_OPENER = _document.COMMENT_OPENER
KEY_SCHEMA = _document.KEY_SCHEMA
MAX_DIAGNOSIS_EVIDENCE = _document.MAX_DIAGNOSIS_EVIDENCE
MAX_DIAGNOSIS_EVIDENCE_ITEM = _document.MAX_DIAGNOSIS_EVIDENCE_ITEM
MAX_DIAGNOSIS_SUMMARY = _document.MAX_DIAGNOSIS_SUMMARY
MAX_TITLE_CHARS = _document.MAX_TITLE_CHARS
NEUTRAL_OPENER = _document.NEUTRAL_OPENER
ORIGIN_ANYWHERE = _document.ORIGIN_ANYWHERE
ORIGIN_LINE = _document.ORIGIN_LINE
ORIGIN_MARKER = _document.ORIGIN_MARKER
ORIGINS = _document.ORIGINS
PUBLICATION_MARKER = _document.PUBLICATION_MARKER
body_origin = _document.body_origin
carries_key = _document.carries_key
issue_body = _document.issue_body
issue_title = _document.issue_title
key_marker = _document.key_marker
neutralize = _document.neutralize
origin_marker = _document.origin_marker
probe_script = _document.probe_script
prose_lines = _document.prose_lines
publication_key = _document.publication_key
require_one_marker_each = _document.require_one_marker_each

ENGINE_LOG_DIR = _evidence.ENGINE_LOG_DIR
MAX_EVIDENCE_FILES_PER_RUN = _evidence.MAX_EVIDENCE_FILES_PER_RUN
MAX_EVIDENCE_RUNS = _evidence.MAX_EVIDENCE_RUNS
MAX_EXCERPT_CHARS = _evidence.MAX_EXCERPT_CHARS
MAX_EXCERPT_LINES = _evidence.MAX_EXCERPT_LINES
MAX_READ_BYTES = _evidence.MAX_READ_BYTES
RUN_EVIDENCE_FILES = _evidence.RUN_EVIDENCE_FILES
collect_evidence = _evidence.collect_evidence
excerpt = _evidence.excerpt
failing_runs = _evidence.failing_runs
open_run_directory = _evidence.open_run_directory
run_excerpts = _evidence.run_excerpts

Published = _record.Published
outcome_record = _record.outcome_record
require_outcome_timestamp = _record.require_outcome_timestamp
require_supported = _record.require_supported
reuse_stored_publication = _record.reuse_stored_publication
stored_record = _record.stored_record
utc_now = _record.utc_now

ISSUE_URL = _tracker.ISSUE_URL
GitHubPublication = _tracker.GitHubPublication
Publication = _tracker.Publication
PublicationFailed = _tracker.PublicationFailed
require_issue_identity = _tracker.require_issue_identity
require_reconciled_issue = _tracker.require_reconciled_issue

# The envelope, the route and the outcome are all named from their
# owners rather than restated. A second spelling of one identifier is
# how a census row stops being greppable against the diagnosis that
# produced it.
HANDOFF_SCHEMA = deflake_handoff.HANDOFF_SCHEMA
ROUTE = deflake_contract.ROUTE_PRODUCTION_DEFECT
OUTCOME_PRODUCTION_DEFECT = ROUTE
OWNER_ISSUE = deflake_contract.ROUTE_OWNER[ROUTE]

ROLE_HANDOFF = deflake_handoff.ROLE_HANDOFF
ROLE_BASELINE = deflake_handoff.ROLE_BASELINE
ROLE_VERIFICATION = deflake_handoff.ROLE_VERIFICATION
ROLES = deflake_handoff.ROLES

# Which of #1437's routes this workflow owns, and the roles the route's
# evidence rests on. `production-defect` is reached PAST the
# `cannot-reproduce` fork, so it always carries a controlled baseline;
# and #1437 refuses it a verification section outright, because that
# route opens no pull request and changes no probe, so a verification
# would mean a repair was attempted and the route is mislabelled.
OWNED = deflake_handoff.RouteOwnership(
    issue=OWNER_ISSUE,
    outcomes=(OUTCOME_PRODUCTION_DEFECT,),
    roles={ROUTE: {"designated": ROLE_BASELINE,
                   "required": (ROLE_BASELINE,),
                   "forbidden": (ROLE_VERIFICATION,)}})

# The two boundaries this route may not cross, spelled as decisions
# rather than omissions. `publish` consults both tables and calls a
# publisher only on a True, so "never called" is a branch a gate can
# exercise: flipping an entry makes the injected spy fire, which is what
# stops the absence resting on nobody having written the call.
CHANGES_THE_PROBE = {OUTCOME_PRODUCTION_DEFECT: False}
OPENS_PULL_REQUEST = {OUTCOME_PRODUCTION_DEFECT: False}



EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_NON_SUCCESS = 3

# The gate's own refusals, re-exported rather than re-declared: the
# module that validates the handoff is the module that names why it was
# refused, and a caller catching one type should catch it whichever
# sibling it called.
HandoffError = deflake_handoff.HandoffError
NonSuccess = deflake_handoff.NonSuccess

# The complete public surface of this route, in one list: what the
# façade defines itself, and what it binds from its four owners.
# `MAX_BODY_CHARS` is absent for the reason the import block states.
__all__ = [
    "CHANGES_THE_PROBE",
    "COMMENT_OPENER",
    "Defect",
    "ENGINE_LOG_DIR",
    "EXIT_NON_SUCCESS",
    "EXIT_OK",
    "EXIT_REJECTED",
    "GitHubPublication",
    "HANDOFF_SCHEMA",
    "HandoffError",
    "ISSUE_URL",
    "KEY_SCHEMA",
    "MAX_DIAGNOSIS_EVIDENCE",
    "MAX_DIAGNOSIS_EVIDENCE_ITEM",
    "MAX_DIAGNOSIS_SUMMARY",
    "MAX_EVIDENCE_FILES_PER_RUN",
    "MAX_EVIDENCE_RUNS",
    "MAX_EXCERPT_CHARS",
    "MAX_EXCERPT_LINES",
    "MAX_READ_BYTES",
    "MAX_TITLE_CHARS",
    "NEUTRAL_OPENER",
    "NonSuccess",
    "OPENS_PULL_REQUEST",
    "ORIGINS",
    "ORIGIN_ANYWHERE",
    "ORIGIN_LINE",
    "ORIGIN_MARKER",
    "OUTCOME_PRODUCTION_DEFECT",
    "OWNED",
    "OWNER_ISSUE",
    "PUBLICATION_MARKER",
    "Publication",
    "PublicationFailed",
    "Published",
    "ROLES",
    "ROLE_BASELINE",
    "ROLE_HANDOFF",
    "ROLE_VERIFICATION",
    "ROUTE",
    "RUN_EVIDENCE_FILES",
    "accept",
    "body_origin",
    "carries_key",
    "collect_evidence",
    "excerpt",
    "failing_runs",
    "forbidden_probe_change",
    "forbidden_pull_request",
    "issue_body",
    "issue_title",
    "key_marker",
    "main",
    "neutralize",
    "open_run_directory",
    "origin_marker",
    "outcome_record",
    "probe_script",
    "prose_lines",
    "publication_key",
    "publish",
    "render",
    "require_defect_diagnosis",
    "require_handoff",
    "require_issue_identity",
    "require_one_marker_each",
    "require_origin",
    "require_outcome_timestamp",
    "require_reconciled_issue",
    "require_supported",
    "reuse_stored_publication",
    "run_excerpts",
    "stored_record",
    "utc_now",
]


def forbidden_probe_change(record) -> None:
    """The probe-adjustment publisher, which exists to refuse being called.

    A production defect is diagnosed in `src/`, `app/` or `scripts/`.
    Editing the probe on the strength of it is exactly the conversion
    this route exists to prevent, so the call raises rather than being a
    silently unused parameter.
    """
    raise NonSuccess(
        f"the {record.get('outcome', ROUTE)!r} outcome changes no probe: the "
        f"diagnosis is that production code or shipped scripts are wrong, "
        f"and adjusting the probe would convert that bug into a permanent "
        f"green light")


def forbidden_pull_request(record) -> None:
    """The fix-PR publisher, which exists to refuse being called."""
    raise NonSuccess(
        f"the {record.get('outcome', ROUTE)!r} outcome opens no pull "
        f"request: an engine fix belongs in the human-reviewed lane, which "
        f"is what filing the issue puts it in")


# ==========================================================================
# The diagnosis this route publishes
# ==========================================================================
def require_defect_diagnosis(document) -> dict:
    """#1437's `diagnosis` block, held to what the issue body renders.

    `deflake_handoff.require_diagnosis_outcome` deliberately validates
    only the fields its own classification rests on, and the diagnosis
    prose is not one of them — #1439 records a summary the CALLER
    supplies. This route is different: the filed issue has to identify
    the diagnosed production behaviour, and that is what this block
    says. #1437 requires it of every route it evaluates
    (`_require_evidence`), so a `production-defect` record without one
    is a producer record that could not have been written.
    """
    outcome = document.get("diagnosis_outcome")
    section = outcome.get("diagnosis") if isinstance(outcome, dict) else None
    if not isinstance(section, dict):
        raise HandoffError(
            "the diagnosis outcome states no `diagnosis` block; #1437 "
            "records one on every route it evaluates, and it is what names "
            "the production behaviour this issue is about")
    evidence = section.get("evidence")
    if (not isinstance(evidence, list) or not evidence
            or not all(isinstance(item, str) and item.strip()
                       for item in evidence)):
        raise HandoffError(
            "the diagnosis records no evidence; the evidence is what makes "
            "the filed issue reviewable rather than asserted")
    if len(evidence) > MAX_DIAGNOSIS_EVIDENCE:
        raise HandoffError(
            f"the diagnosis states {len(evidence)} evidence lines, over the "
            f"{MAX_DIAGNOSIS_EVIDENCE} an issue body carries; the evidence "
            f"is published whole rather than trimmed, so a longer list is "
            f"refused instead of being cut down to one that no longer says "
            f"what the diagnosis said")
    for position, item in enumerate(evidence):
        if len(item) > MAX_DIAGNOSIS_EVIDENCE_ITEM:
            raise HandoffError(
                f"the diagnosis's evidence line {position + 1} is "
                f"{len(item)} characters, over the "
                f"{MAX_DIAGNOSIS_EVIDENCE_ITEM} an issue body carries; a log "
                f"belongs in the retained artifacts this workflow quotes "
                f"from, not in the evidence list")
    summary = section.get("summary")
    if not isinstance(summary, str) or not summary.strip():
        raise HandoffError(
            "the diagnosis states no `summary`; the issue title and its "
            "opening line are that sentence, and neither is this workflow's "
            "to invent")
    if len(summary) > MAX_DIAGNOSIS_SUMMARY:
        raise HandoffError(
            f"the diagnosis `summary` is {len(summary)} characters, over the "
            f"{MAX_DIAGNOSIS_SUMMARY} an issue body carries; the summary is "
            f"the issue's own claim and is published whole")
    category = section.get("category")
    if category is not None and not isinstance(category, str):
        raise HandoffError(
            f"the diagnosis names the cause category {category!r}, which is "
            f"not a string")
    return {
        "summary": summary.strip(),
        "evidence": [item.strip() for item in evidence],
        "category": category,
    }


def require_origin(value) -> str:
    """The review-routing brand, which is the invoking agent's own."""
    if value not in ORIGINS:
        raise HandoffError(
            f"the issue origin must be one of {', '.join(ORIGINS)} (got "
            f"{value!r}); it is the `{ORIGIN_MARKER}` marker "
            f"`approve_issues.py` reads to route the filed issue to the "
            f"opposite agent, and an issue carrying none never enters the "
            f"review gate")
    return value


def require_handoff(document, *, worktrees=(), primary=None):
    """One `deflake-outcome-handoff/v1` on this workflow's own route."""
    return deflake_handoff.require_handoff(
        document, worktrees=worktrees, primary=primary, owned=OWNED)


class Defect:
    """One accepted production-defect handoff.

    The shared gate's `Handoff` plus the one thing only this route
    reads: #1437's `diagnosis` block, which is what the filed issue is
    ABOUT. A wrapper rather than an attribute bolted onto the shared
    object, so nothing downstream can reach the renderer with a
    diagnosis that never went through `require_defect_diagnosis` — every
    other field is delegated, so the two read as one accepted input.
    """

    def __init__(self, handoff, diagnosis: dict):
        self.handoff = handoff
        self.diagnosis = diagnosis

    def __getattr__(self, name):
        return getattr(self.handoff, name)



def render(defect: Defect, *, origin: str, key=None) -> tuple:
    """The issue this attempt would file: `(title, body, evidence, trimmed)`.

    Everything up to the tracker, and nothing beyond it. `--dry-run` is
    this and `publish` opens with it — one renderer, so what an operator
    reviews is exactly what gets filed, and a handoff the route does not
    support is refused by both.
    """
    origin = require_origin(origin)
    require_supported(defect)
    key = publication_key(defect) if key is None else key
    evidence = collect_evidence(defect)
    body, trimmed = issue_body(defect, diagnosis=defect.diagnosis,
                               evidence=evidence, key=key, origin=origin)
    return issue_title(defect, defect.diagnosis), body, evidence, trimmed


def publish(handoff: Defect, *, census_path: Path, now: str,
            publication: Publication, origin: str,
            probe_publisher=forbidden_probe_change,
            pull_request_publisher=forbidden_pull_request) -> Published:
    """File one issue for one diagnosed production defect, and record it.

    The order is deliberate. The route's evidence is judged first, so a
    handoff this route does not support is refused without touching the
    tracker. The census is consulted next and the publication key
    reconciled after it — both BEFORE anything is rendered, because
    rendering re-reads the retained artifacts and those are transient
    while an issue and a census record are durable. An attempt that
    already reached the tracker therefore resumes on what is durable,
    however long ago its artifact tree was swept. Only an attempt with
    no issue at all renders, and that is the one case where there is
    genuinely something to file.

    Nothing follows the record. The two publisher parameters are
    consulted through their tables and, under this route's policy, never
    called.
    """
    origin = require_origin(origin)
    path = Path(census_path)
    key = publication_key(handoff)
    require_outcome_timestamp(now)

    # The route's own evidence is judged here, before the tracker is
    # touched at all: a handoff this route does not support is refused
    # without so much as a search. It needs no artifact on disk, which
    # is what lets both recovery paths below run after the tree is gone.
    require_supported(handoff)

    # THEN completion, and THEN the reconcile — both before anything is
    # rendered. Rendering reads the retained artifacts off disk and
    # those are transient, while an issue and a census record are
    # durable. So an attempt that already reached the tracker, whether
    # its outcome was recorded or the census refused after the issue was
    # created, resumes on what is durable rather than on evidence that
    # may have been swept in between. Only an attempt with no issue at
    # all renders, and that is the one case where there is genuinely
    # something to file.
    complete = stored_record(path, handoff)
    created = False
    reconciled = False
    trimmed = False
    if complete is not None:
        issue = copy.deepcopy(complete["issue"])
    else:
        found = publication.find(key)
        if found is not None:
            issue = require_reconciled_issue(found, key)
            reconciled = True
        else:
            title, body, _evidence, trimmed = render(handoff, origin=origin,
                                                     key=key)
            issue = require_issue_identity(
                publication.create(title=title, body=body), key, origin)
            created = True

    document = outcome_record(handoff, now=now, issue=issue)
    durable = True
    resumed = False
    try:
        _probe, resumed, document = probe_census.record_outcome_installed(
            path, handoff.probe, document,
            reconcile=reuse_stored_publication)
    except probe_census.CensusDurabilityUnconfirmed as error:
        durable = False
        print(f"deflake_issue: warning: the census append is installed but "
              f"its durability is unconfirmed ({error}); do not re-record "
              f"this attempt", file=sys.stderr)
    except probe_census.CensusError as error:
        raise NonSuccess(
            f"the census refused the {OUTCOME_PRODUCTION_DEFECT!r} outcome "
            f"for attempt {handoff.attempt!r} ({error}); issue "
            f"#{issue['number']} exists and nothing else was attempted, so "
            f"re-running this workflow reconciles it rather than filing a "
            f"second one") from None

    outcome = document["outcome"]
    changed_probe = False
    if CHANGES_THE_PROBE[outcome]:
        probe_publisher(document)
        changed_probe = True
    opened = False
    if OPENS_PULL_REQUEST[outcome]:
        pull_request_publisher(document)
        opened = True
    return Published(document, resumed=resumed, durable=durable,
                     created=created, reconciled=reconciled, trimmed=trimmed,
                     changed_probe=changed_probe, opened_pull_request=opened)


# ==========================================================================
# Command line
# ==========================================================================
def _load(path: str, what: str):
    try:
        return json.loads(Path(path).read_text(encoding="utf-8"))
    except OSError as error:
        raise HandoffError(
            f"{what} at {path} is unreadable ({error})") from None
    except json.JSONDecodeError as error:
        raise HandoffError(f"{what} at {path} is not JSON ({error})") from None


def accept(document, *, worktrees=(), primary=None) -> Defect:
    """The whole entry gate: the shared one, plus this route's diagnosis."""
    return Defect(
        require_handoff(document, worktrees=worktrees, primary=primary),
        require_defect_diagnosis(document))


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--handoff", metavar="PATH", required=True,
                    help=f"one {HANDOFF_SCHEMA} on the {ROUTE!r} route")
    ap.add_argument("--origin", choices=ORIGINS, default=None,
                    help="the agent brand filing the issue; it becomes the "
                         "issue-origin marker the review gate routes on, and "
                         "a dry run needs it too because the marker is part "
                         "of the body")
    ap.add_argument("--census", metavar="PATH", default=None,
                    help="the census to append to (default: the docs-wip "
                         "worktree's docs/probe_census.json)")
    ap.add_argument("--repo", metavar="OWNER/NAME", default=None,
                    help="the tracker to file in (default: whatever `gh` "
                         "resolves from the working directory)")
    ap.add_argument("--dry-run", action="store_true",
                    help="render the issue and stop: nothing is filed and "
                         "nothing is recorded")
    ap.add_argument("--json", action="store_true",
                    help="print the published outcome instead of prose")
    args = ap.parse_args(argv)

    try:
        handoff = accept(_load(args.handoff, "outcome handoff"),
                         worktrees=deflake_contract.worktree_paths(),
                         primary=deflake_contract.primary_checkout())
        if args.origin is None:
            # Required by a dry run too: the marker is part of the body,
            # so a preview rendered under an assumed brand would not be
            # the issue the next invocation files.
            raise HandoffError(
                f"--origin is required; it becomes the `{ORIGIN_MARKER}` "
                f"marker the canonical review gate routes on, and it is the "
                f"INVOKING agent's brand, which no document can derive")
        if args.dry_run:
            title, body, _evidence, trimmed = render(handoff,
                                                     origin=args.origin)
            if args.json:
                print(json.dumps({"title": title, "body": body,
                                  "publication_key": publication_key(handoff),
                                  "evidence_trimmed": trimmed,
                                  "published": False},
                                 indent=2, sort_keys=True))
            else:
                print(title)
                print()
                print(body)
            return EXIT_OK
        census_path = (Path(args.census) if args.census
                       else probe_census.manifest_path())
        published = publish(handoff, census_path=census_path, now=utc_now(),
                            publication=GitHubPublication(args.repo),
                            origin=args.origin)
    except HandoffError as error:
        print(f"deflake_issue: handoff rejected: {error}", file=sys.stderr)
        return EXIT_REJECTED
    except NonSuccess as error:
        print(f"deflake_issue: nothing filed: {error}", file=sys.stderr)
        return EXIT_NON_SUCCESS
    except probe_census.DocsWorktreeMissing as error:
        print(f"deflake_issue: nothing filed: {error}", file=sys.stderr)
        return EXIT_NON_SUCCESS

    if args.json:
        print(json.dumps(published.to_document(), indent=2, sort_keys=True))
    else:
        if published.resumed:
            verb = "already recorded"
        elif published.reconciled:
            verb = "reconciled"
        else:
            verb = "filed"
        print(f"{OUTCOME_PRODUCTION_DEFECT}: {verb} issue "
              f"#{published.record['issue']['number']} for "
              f"{published.record['probe']} attempt "
              f"{published.record['attempt']}")
        print(published.record["issue"]["url"])
        print("terminal: the probe is unchanged and no pull request was "
              "opened")
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
