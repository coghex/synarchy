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

Quoted content cannot forge the routing marker
----------------------------------------------
Everything this body quotes is arbitrary text — an engine log above all
— and one of the things arbitrary text contains is an `issue-origin`
marker. `approve_issues.issue_origin` scans the WHOLE raw body, fenced
blocks included and case-insensitively, and RAISES on two markers naming
different brands, so a quoted log carrying one would stop the filed
issue entering the review gate at all: the one thing this route exists
to do.

So the assembled body passes through ONE funnel that breaks every
HTML-comment opener, and only then are the two real markers appended.
One funnel rather than an escape at each interpolation site, because a
checklist covering the diagnosis prose, the evidence lines, the artifact
paths, the command tokens and every quoted log is a checklist that
eventually misses one. The finished text is then CHECKED — exactly one
origin, exactly one publication key, exactly two comments — rather than
trusted, because the cost of being wrong is silent.

Every part of the body is required, so nothing is silently cut
------------------------------------------------------------
The measurement facts, the provenance markers and at least one quoted
excerpt are what make the issue reviewable, and a tracker body has a
size limit. Only the SECOND and later runs' evidence may be dropped to
fit; when even that is not enough the publication is REFUSED, because a
defect report published with its measurements or its log evidence
truncated away is the thing this workflow exists to prevent. #1437
bounds neither the diagnosis summary nor its evidence list, so those are
bounded at this gate instead — refused rather than trimmed, since the
summary is the issue's own claim.

Evidence, not pathnames
-----------------------
The artifact tree is walked component by component from the DECLARED
artifact root, `O_NOFOLLOW` at every step below it, and the engine
directory is listed by descriptor. Only the root is opened by path: it
is the anchor the producer record declares and
`require_artifact_reference` has already refused one inside a worktree.
Everything below is traversed rather than resolved because what is found
there is QUOTED INTO A PUBLISHED ISSUE — a symlinked `engine`
directory, or a run directory substituted after #1437's own
canonical-path check passed, would otherwise have every listing and open
land somewhere else and publish whatever regular files live there as
this probe's failure evidence. Each file is then opened `O_NONBLOCK` and
required to be regular, so a FIFO planted at one of those names cannot
block the workflow on an open.

`probe_flake` retains a directory per FAIL and TIMEOUT run — the
protocol event stream, the probe's stdout, and the engine logs — and
`probe_census.summarize_sample` stores only their PATHS. Those paths are
machine-local: a reader of the filed issue cannot open them, and an
issue whose only evidence is a path nobody else has is not reviewable.
So this module READS those retained artifacts and quotes bounded
excerpts into the body, and an attempt whose artifacts have all been
pruned is refused rather than filed on paths alone. The numbers beside
them come from the measurement documents the diagnosis judged: the
failure numerator, denominator and rate, the timeout count, every
declared check's PASS/FAIL/MISSING tally, the measured commit, the run
count and the RTS capability setting.

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
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
import os
import re
import stat
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake_diagnosis  # noqa: E402
import deflake_handoff  # noqa: E402
import probe_census  # noqa: E402
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402
import probe_runner_registry  # noqa: E402

# The envelope, the route and the outcome are all named from their
# owners rather than restated. A second spelling of one identifier is
# how a census row stops being greppable against the diagnosis that
# produced it.
HANDOFF_SCHEMA = deflake_handoff.HANDOFF_SCHEMA
ROUTE = deflake_diagnosis.ROUTE_PRODUCTION_DEFECT
OUTCOME_PRODUCTION_DEFECT = ROUTE
OWNER_ISSUE = deflake_diagnosis.ROUTE_OWNER[ROUTE]

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

# The canonical issue-review routing metadata. `approve_issues.py` reads
# this marker out of the issue BODY to decide which agent brand reviews
# it, so an issue filed without one does not enter the review gate at
# all. The brand is the INVOKING agent's, which no document can derive,
# so it is a required input rather than a default.
ORIGIN_MARKER = "issue-origin"
ORIGINS = ("claude", "codex")

# Everything this body quotes — engine logs above all — is arbitrary
# text, and one of the things arbitrary text can contain is an
# `issue-origin` marker. `approve_issues.py` scans the WHOLE raw body,
# fenced blocks included, and RAISES on two markers naming different
# brands: a quoted log carrying one would stop the filed issue entering
# the review gate at all, which is the one thing this route exists to
# do. So every untrusted character rendered into the body has its
# HTML-comment OPENER broken before the two real markers are appended.
# ASCII, visible, and reversible by eye — a reader sees what the log
# said and the preamble says it was done.
COMMENT_OPENER = "<!--"
NEUTRAL_OPENER = "<! --"

# The publication key's marker, and the string the key is derived over.
# Versioned so a later change to what identifies a diagnosis cannot
# silently collide with keys already filed.
PUBLICATION_MARKER = "deflake-publication-key"
KEY_SCHEMA = "deflake-production-defect-publication/v1"

# How much retained log evidence one issue carries. Bounded on every
# axis, because the body is a review surface and a whole engine log is
# not one: at most this many non-PASS runs, this many files from each,
# and this many trailing lines and characters from each file. The TAIL
# is what is quoted — a probe that fails aborts, so the end of its
# stream is where the failure is.
MAX_EVIDENCE_RUNS = 3
MAX_EVIDENCE_FILES_PER_RUN = 4
MAX_EXCERPT_LINES = 24
MAX_EXCERPT_CHARS = 2400
# Read only the tail off disk. An engine log can be large and only its
# end is quoted, so the whole file is never held in memory.
MAX_READ_BYTES = 262144
# GitHub refuses a body over 65536 characters. The evidence section is
# what gets trimmed if the rendered body would exceed this, never the
# measurement table — and the trim says so on the record.
MAX_BODY_CHARS = 60000
MAX_TITLE_CHARS = 240
# #1437 requires a diagnosis summary and at least one evidence line and
# bounds NEITHER, so an accepted producer record can carry prose longer
# than a whole issue body. Bounded HERE rather than truncated later: the
# summary is what the issue is about and the evidence is what makes it
# reviewable, so silently cutting either would publish a defect report
# whose own claim had been trimmed away.
MAX_DIAGNOSIS_SUMMARY = 4000
MAX_DIAGNOSIS_EVIDENCE = 20
MAX_DIAGNOSIS_EVIDENCE_ITEM = 1000

# The per-run artifact layout `probe_flake.measure` creates, in the
# order a reader wants it: what the probe declared, what it printed, and
# then whatever the engine logged.
RUN_EVIDENCE_FILES = ("events.jsonl", "stdout.txt")
ENGINE_LOG_DIR = "engine"

EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_NON_SUCCESS = 3

# The gate's own refusals, re-exported rather than re-declared: the
# module that validates the handoff is the module that names why it was
# refused, and a caller catching one type should catch it whichever
# sibling it called.
HandoffError = deflake_handoff.HandoffError
NonSuccess = deflake_handoff.NonSuccess


class PublicationFailed(NonSuccess):
    """The tracker could not be reconciled or written.

    A NonSuccess because the ending is the same one: nothing is
    recorded, nothing else is attempted, and the attempt stays
    resumable. Its own type so a caller — and the gate — can tell a
    boundary failure from evidence that did not support the route.
    """


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


# ==========================================================================
# The publication key
# ==========================================================================
def publication_key(handoff) -> str:
    """The stable identity one diagnosis publishes under.

    DERIVED, never supplied: a caller-chosen key could be retyped
    differently on the resume that needed it to match. The material is
    the four facts that identify the attempt — the attempt id, the
    probe, the route and the baseline commit — under a versioned schema
    string, so a later change to what identifies a diagnosis produces
    different keys rather than colliding with issues already filed.
    """
    material = "\n".join([KEY_SCHEMA, handoff.attempt, handoff.probe,
                          handoff.route, handoff.baseline_sha])
    return hashlib.sha256(material.encode("utf-8")).hexdigest()


def key_marker(key: str) -> str:
    return f"<!-- {PUBLICATION_MARKER}: {key} -->"


def origin_marker(origin: str) -> str:
    return f"<!-- {ORIGIN_MARKER}:{origin} -->"


ORIGIN_LINE = re.compile(rf"^<!--\s*{ORIGIN_MARKER}:(\w+)\s*-->$")
# Deliberately `approve_issues.ORIGIN_RE`'s own shape: the gate scans the
# WHOLE raw body, fences and all, case-insensitively, so this is what
# the finished body is checked against before it is published.
ORIGIN_ANYWHERE = re.compile(
    rf"<!--\s*{ORIGIN_MARKER}:(claude|codex)\s*-->", re.IGNORECASE)


def prose_lines(body) -> list:
    """The body's own lines, with every fenced code block removed.

    An issue this workflow files QUOTES engine logs, and a quoted log is
    arbitrary text: it can contain any line at all, this module's own
    marker line included. A marker found inside a fence is therefore
    evidence about some other issue, not a statement about this one —
    which is exactly how a tracker search turns a duplicate report into
    a reconciled publication.

    Fences are matched the way `_fence` writes them: three or more
    backticks open a block, and a run at least as long closes it. An
    unterminated fence swallows the rest of the body, which is the safe
    direction — an unclosed quote is not a place to read a marker from.
    """
    if not isinstance(body, str):
        return []
    kept: list = []
    fence = 0
    for line in body.splitlines():
        stripped = line.strip()
        run = len(stripped) - len(stripped.lstrip("`"))
        if fence:
            if run >= fence and not stripped.lstrip("`").strip():
                fence = 0
            continue
        if run >= 3:
            fence = run
            continue
        kept.append(stripped)
    return kept


def neutralize(text: str) -> str:
    """Break every HTML-comment opener in untrusted rendered content.

    Applied to the WHOLE assembled body except the trailer this module
    writes itself, rather than at each interpolation site: one funnel
    cannot miss a field, and it makes "the only comments in this body
    are the two we wrote" an invariant instead of a review checklist
    covering the diagnosis prose, the evidence lines, the artifact
    paths, the command tokens and every quoted log at once.
    """
    return text.replace(COMMENT_OPENER, NEUTRAL_OPENER)


def require_one_marker_each(body: str, *, key: str, origin: str) -> None:
    """The body carries this module's two markers and no other comment.

    Asserted on the finished text rather than trusted from
    `neutralize`, because the cost of being wrong is silent: a second
    origin marker makes `approve_issues.issue_origin` raise, and the
    filed issue never enters the review gate the whole route exists to
    reach.
    """
    # Most specific first, so the message names the actual problem: the
    # total-comment clause below is the strongest of the three and would
    # otherwise answer for all of them.
    origins = {found.lower() for found in ORIGIN_ANYWHERE.findall(body)}
    if origins != {origin}:
        raise NonSuccess(
            f"the rendered issue body names the origin(s) "
            f"{sorted(origins) or ['none']} where it must name exactly "
            f"{origin!r}; `approve_issues.py` refuses a body with "
            f"conflicting origin markers, and the filed issue would never "
            f"enter the review gate")
    keys = body.count(key_marker(key))
    if keys != 1:
        raise NonSuccess(
            f"the rendered issue body carries {keys} "
            f"`{PUBLICATION_MARKER}` markers where it must carry exactly "
            f"one; a resume reconciles on that line")
    comments = body.count(COMMENT_OPENER)
    if comments != 2:
        raise NonSuccess(
            f"the rendered issue body carries {comments} HTML comment(s) "
            f"where it must carry exactly two — this attempt's "
            f"`{PUBLICATION_MARKER}` and its `{ORIGIN_MARKER}` marker; a "
            f"third would be read as review-routing metadata this workflow "
            f"did not write")


def carries_key(body, key: str) -> bool:
    """Whether a fetched issue body really was filed under `key`.

    A tracker search matches text anywhere, so an issue that merely
    QUOTES a key — a duplicate report, a log excerpt, a comment pasted
    into a body — comes back from it. What the publisher writes is one
    STANDALONE marker line outside every code fence, and matching that
    rather than a substring is what separates the two.
    """
    return key_marker(key) in prose_lines(body)


def body_origin(body):
    """The `issue-origin` brand a fetched issue body actually carries.

    Read from the ISSUE rather than from the invoking caller, because a
    reconciled issue was filed by whoever filed it: a Claude-origin
    creation whose census write failed, resumed by a Codex invocation,
    still routes to Claude's opposite brand, and recording the retry's
    own brand would put a second, false answer in the durable history.
    """
    for line in prose_lines(body):
        found = ORIGIN_LINE.match(line)
        if found is not None and found.group(1) in ORIGINS:
            return found.group(1)
    return None


# ==========================================================================
# Retained log evidence
# ==========================================================================
def _open_directory(name, *, dir_fd=None):
    """One directory component, opened WITHOUT following a symlink."""
    try:
        return os.open(name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                       dir_fd=dir_fd)
    except (OSError, ValueError):
        return None


def open_run_directory(artifact_root: str, run_dir: str):
    """A descriptor for one retained run directory, or None.

    Walked component by component from the DECLARED artifact root, with
    `O_NOFOLLOW` on every step below it. Only the root itself is opened
    by path: it is the anchor the producer record declares, and
    `require_artifact_reference` has already refused one inside any
    worktree.

    Everything under it is traversed by descriptor because this module
    QUOTES what it finds into a published issue. `O_NOFOLLOW` on the
    final file alone is not enough — a symlinked `engine` directory, or
    a symlinked run directory, would have `os.listdir` and every open
    below it land somewhere else entirely, and whatever regular files
    live there would be read and published as this probe's failure
    evidence. Refusing at every component closes that, and closes the
    component race with it: nothing is re-resolved by path after the
    first open.
    """
    try:
        relative = Path(run_dir).relative_to(Path(artifact_root))
    except ValueError:
        # #1437's artifact topology puts every run directory under the
        # root its own result declares, so this is unreachable for an
        # accepted handoff — and a path that is not under the anchor is
        # one this has no safe way to walk.
        return None
    fd = _open_directory(artifact_root)
    for part in relative.parts:
        if fd is None:
            return None
        nxt = _open_directory(part, dir_fd=fd)
        os.close(fd)
        fd = nxt
    return fd


def _tail_text(name: str, *, dir_fd) -> str | None:
    """The tail of one retained artifact, or None if it cannot be read.

    Opened by NAME within its own directory's descriptor and with
    `O_NOFOLLOW`, so neither the file nor any directory above it can be
    a symlink to somewhere this has no business quoting.
    `O_NONBLOCK` and the regular-file check are the other half: a FIFO
    planted at one of these names would otherwise block the open until
    someone wrote to it, and a directory would read as an error late
    rather than a skip early. Decoding replaces undecodable bytes
    instead of raising — a macOS engine log carries GLFW's junk, and
    evidence that exists must not be dropped because one byte is not
    UTF-8.
    """
    try:
        fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK,
                     dir_fd=dir_fd)
    except (OSError, ValueError):
        return None
    try:
        if not stat.S_ISREG(os.fstat(fd).st_mode):
            return None
        size = os.lseek(fd, 0, os.SEEK_END)
        start = max(0, size - MAX_READ_BYTES)
        os.lseek(fd, start, os.SEEK_SET)
        raw = os.read(fd, MAX_READ_BYTES)
    except OSError:
        return None
    finally:
        os.close(fd)
    text = raw.decode("utf-8", errors="replace")
    if start:
        # The first line of a mid-file read is a fragment; dropping it
        # is what keeps every quoted line a whole one.
        text = text.split("\n", 1)[1] if "\n" in text else ""
    return text


def excerpt(path: Path, name: str, *, dir_fd) -> dict | None:
    """One bounded, quotable excerpt of one retained artifact.

    `path` is the label the issue prints so a reader can find the whole
    artifact; `name` and `dir_fd` are what is actually opened.
    """
    text = _tail_text(name, dir_fd=dir_fd)
    if text is None:
        return None
    lines = [line for line in text.splitlines() if line.strip()]
    if not lines:
        return None
    kept = lines[-MAX_EXCERPT_LINES:]
    body = "\n".join(kept)
    clipped = len(kept) < len(lines)
    if len(body) > MAX_EXCERPT_CHARS:
        body = body[-MAX_EXCERPT_CHARS:]
        clipped = True
    return {"path": str(path), "lines": len(kept), "clipped": clipped,
            "text": body}


def run_excerpts(artifact_root: str, run_dir: str) -> list:
    """The bounded excerpts one retained run directory yields, in order.

    The protocol stream first — it is what the checks were scored from —
    then the probe's own stdout, then whatever the engine logged. The
    engine directory is listed rather than guessed at, because its file
    names come from the probe being measured; it is listed BY
    DESCRIPTOR, so a symlink standing in for it reaches nothing.
    """
    excerpts: list = []
    run_fd = open_run_directory(artifact_root, run_dir)
    if run_fd is None:
        return excerpts
    display = Path(run_dir)
    try:
        for name in RUN_EVIDENCE_FILES:
            if len(excerpts) >= MAX_EVIDENCE_FILES_PER_RUN:
                return excerpts
            found = excerpt(display / name, name, dir_fd=run_fd)
            if found is not None:
                excerpts.append(found)
        engine_fd = _open_directory(ENGINE_LOG_DIR, dir_fd=run_fd)
        if engine_fd is None:
            return excerpts
        try:
            names = sorted(os.listdir(engine_fd))
        except OSError:
            names = []
        try:
            for name in names:
                if len(excerpts) >= MAX_EVIDENCE_FILES_PER_RUN:
                    break
                found = excerpt(display / ENGINE_LOG_DIR / name, name,
                                dir_fd=engine_fd)
                if found is not None:
                    excerpts.append(found)
        finally:
            os.close(engine_fd)
    finally:
        os.close(run_fd)
    return excerpts


def failing_runs(handoff) -> list:
    """Every non-PASS run this attempt retained, in role order.

    `probe_flake.measure` deletes a run's directory the moment it passes
    and keeps every unsuccessful one, so these are exactly the runs with
    something to read. A harness-error run is deliberately not among
    them: it is kept out of `runs`, and a measurement carrying one is
    refused by `require_supported` before any evidence is collected.
    """
    found = []
    for role in ROLES:
        measurement = handoff.measurement(role)
        if measurement is None:
            continue
        for run in measurement.result["runs"]:
            if run["outcome"] == probe_flake.RUN_PASS:
                continue
            directory = run.get("artifact_dir")
            if not directory:
                continue
            found.append({"role": role, "index": run["index"],
                          "outcome": run["outcome"],
                          "artifact_dir": directory,
                          # The anchor its own measurement declared, so
                          # the walk below starts from a path the
                          # producer record vouches for rather than from
                          # whatever the run directory's parents are
                          # today.
                          "artifact_root": measurement.result[
                              "artifact_root"]})
    return found


def require_supported(handoff) -> None:
    """Everything that must hold before this attempt reaches the tracker.

    Trustworthiness over EVERY declared measurement, not only the one
    the route is judged on: a filed defect is stated over all of them,
    so one untrustworthy batch makes the whole attempt an operational
    error. Then #1437's own reproduction qualification, CALLED rather
    than paraphrased — a batch that stayed at or below X with every
    target present, or that failed only checks nobody targeted,
    reproduced nothing to attribute to the engine and is #1439's
    `cannot-reproduce`.
    """
    problems = [problem
                for role in ROLES if role in handoff.measurements
                for problem in
                handoff.measurements[role].trustworthiness_problems()]
    if problems:
        raise NonSuccess(
            "a filed production defect rests on complete, trustworthy "
            "measurements, and this attempt's are not that: "
            + "; ".join(problems))
    deflake_handoff.require_reproduced(
        handoff, handoff.measurement(ROLE_BASELINE))


def collect_evidence(handoff) -> list:
    """The bounded excerpts this issue will carry, or the refusal.

    An issue whose only log evidence is a machine-local pathname is not
    reviewable — the approved correction says so explicitly — so an
    attempt whose retained artifacts have all been pruned is refused
    here, BEFORE anything is published, rather than filed on paths
    alone.
    """
    blocks = []
    for run in failing_runs(handoff)[:MAX_EVIDENCE_RUNS]:
        excerpts = run_excerpts(run["artifact_root"], run["artifact_dir"])
        if excerpts:
            blocks.append({**run, "excerpts": excerpts})
    if not blocks:
        raise NonSuccess(
            f"no retained artifact of this attempt could be read, so the "
            f"issue would carry machine-local pathnames and nothing a "
            f"reviewer can open; the evidence is what makes the filed issue "
            f"reviewable, so re-measure the probe with "
            f"`python3 tools/probe_flake.py --probe {handoff.probe}` and "
            f"hand off the fresh artifacts rather than filing on paths")
    return blocks


# ==========================================================================
# The issue
# ==========================================================================
def probe_script(probe: str) -> str:
    """The registered script name, from the registry rather than a pattern.

    Most probes are `tools/<key>_probe.py` and some are not, and the
    filed issue names the one that was actually measured.
    """
    for key, script, _purpose in probe_runner_registry.PROBES:
        if key == probe:
            return script
    return f"{probe}_probe.py"


def issue_title(handoff, diagnosis: dict) -> str:
    baseline = handoff.measurement(ROLE_BASELINE)
    head = (f"{handoff.probe}: {baseline.failure_count}/"
            f"{baseline.requested_runs} runs fail on a production defect — "
            f"{diagnosis['summary']}")
    head = neutralize(" ".join(head.split()))
    if len(head) > MAX_TITLE_CHARS:
        head = head[:MAX_TITLE_CHARS - 1].rstrip() + "…"
    return head


def _measurement_section(measurement) -> list:
    result = measurement.result
    rate = result["failure_rate"]
    lines = [
        f"#### The {measurement.role} measurement",
        "",
        f"- Probe: `{result['probe']}`",
        f"- Measured commit: `{result['commit_sha']}`",
        f"- Measured at: {result['timestamp_utc']}",
        f"- Runs: {result['completed_runs']} completed of "
        f"{result['requested_runs']} requested",
        f"- Failures: {result['failure_count']}/{result['requested_runs']} "
        f"(rate {rate if rate is not None else 'unavailable'})",
        f"- Timeouts: {result['timeout_count']}",
        f"- RTS capabilities: `+RTS -N{result['rts_capabilities']}`",
        "",
        "| check | PASS | FAIL | MISSING |",
        "|---|---:|---:|---:|",
    ]
    for cid, tally in sorted(measurement.check_counts.items()):
        lines.append(
            f"| `{cid}` | {tally.get(probe_protocol.PASS, 0)} "
            f"| {tally.get(probe_protocol.FAIL, 0)} "
            f"| {tally.get(probe_protocol.MISSING, 0)} |")
    lines.append("")
    lines.append("Per-run outcomes: "
                 + ", ".join(f"{run['index']}={run['outcome']}"
                             for run in result["runs"]))
    lines.append("")
    return lines


def _fence(text: str) -> str:
    """A code fence longer than any backtick run inside the quoted text.

    An engine log is arbitrary bytes and may contain a fence of its own.
    Quoting it inside a three-backtick block would end the block early
    and let the rest of the log render as markdown, which is how a
    reviewable excerpt turns into a mangled issue body.
    """
    longest = 0
    run = 0
    for character in text:
        run = run + 1 if character == "`" else 0
        longest = max(longest, run)
    return "`" * max(3, longest + 1)


def _evidence_section(blocks: list) -> list:
    lines = ["### Retained log evidence", "",
             "Bounded excerpts read from the artifacts the harness retained "
             "for each non-PASS run. The paths are named so the full "
             "artifact can be found on the machine that measured it; the "
             "quoted text is what a reviewer can read here. An HTML-comment "
             f"opener anywhere in quoted content is rendered as "
             f"`{NEUTRAL_OPENER}`, so this issue carries exactly one "
             f"review-routing marker: its own.", ""]
    for block in blocks:
        lines.append(f"#### {block['role']} run {block['index']} "
                     f"({block['outcome']}) — `{block['artifact_dir']}`")
        lines.append("")
        for item in block["excerpts"]:
            suffix = " (tail)" if item["clipped"] else ""
            fence = _fence(item["text"])
            lines.append(f"`{item['path']}`{suffix}")
            lines.append("")
            lines.append(fence)
            lines.append(item["text"])
            lines.append(fence)
            lines.append("")
    return lines


def issue_body(handoff, *, diagnosis: dict, evidence: list, key: str,
               origin: str) -> tuple:
    """The rendered body, and whether the evidence had to be trimmed."""
    baseline = handoff.measurement(ROLE_BASELINE)
    head = [
        f"`tools/{probe_script(handoff.probe)}` was measured by the "
        f"`/deflake` lab (#1426) and the diagnosis is that **production "
        f"code or "
        f"shipped scripts are wrong** — a real race, not a racy test. The "
        f"probe was NOT changed and no fix pull request was opened; this "
        f"issue is the hand-off into the human-reviewed lane.",
        "",
        "### Diagnosis",
        "",
        diagnosis["summary"],
        "",
    ]
    if diagnosis["category"]:
        head.append(f"Cause category: `{diagnosis['category']}`")
        head.append("")
    head.append("Evidence stated by the diagnosis:")
    head.append("")
    head += [f"- {item}" for item in diagnosis["evidence"]]
    head += [
        "",
        "### The attempt",
        "",
        f"- Probe: `{handoff.probe}`",
        f"- Baseline commit: `{handoff.baseline_sha}`",
        f"- Target check(s): "
        + (", ".join(f"`{cid}`" for cid in handoff.targets) or "none"),
        f"- Acceptable failures (X): {handoff.acceptable_failures} out of "
        f"{baseline.requested_runs}",
        f"- Attempt identity: `{handoff.attempt}`",
        f"- `/deflake` invocation: "
        f"`{' '.join(handoff.invocation['command'])}` in "
        f"`{handoff.invocation['directory']}`",
        "",
        "### Measurements",
        "",
    ]
    for role in ROLES:
        measurement = handoff.measurement(role)
        if measurement is not None:
            head += _measurement_section(measurement)
    head += ["### Configuration read by the measurement", ""]
    entries = handoff.configuration
    if entries:
        head += [f"- `{entry['path']}` sha256 `{entry['sha256']}`"
                 for entry in entries]
    else:
        head.append("No `config/*.local.yaml` file was present — the "
                    "repository's expected default.")
    head.append("")
    head.append("### Attempt summary")
    head.append("")
    head.append(handoff.summary)
    head.append("")

    tail = ["", "---", "",
            "Filed by `tools/deflake_issue.py` (#1438). This outcome is "
            "terminal: the probe is unchanged and no pull request was "
            "opened.",
            "", key_marker(key), origin_marker(origin), ""]

    fixed = "\n".join(head) + "\n"
    closing = "\n".join(tail)
    note = ("\n_Evidence from further runs was omitted to keep this body "
            "within the tracker's size limit; the retained artifact paths "
            "above name every one._\n")
    # What the body may not be published WITHOUT: the measurement facts,
    # the provenance markers, and at least one quoted excerpt. So the
    # only thing that may be dropped to fit is the SECOND and later
    # runs' evidence — and when even that is not enough, this refuses
    # rather than slicing. A published body missing its measurements, or
    # missing the log evidence entirely, is the defect report this
    # workflow exists to prevent, and truncating one silently is worse
    # than not filing it.
    trimmed = False
    while True:
        section = "\n".join(_evidence_section(evidence))
        if trimmed:
            section += note
        # ONE funnel: everything above this line is rendered from the
        # handoff and is untrusted, and the trailer below is the only
        # part this module writes as metadata.
        body = neutralize(fixed + section) + closing
        if len(body) <= MAX_BODY_CHARS:
            require_one_marker_each(body, key=key, origin=origin)
            return body, trimmed
        if len(evidence) <= 1:
            raise NonSuccess(
                f"the issue body is {len(body)} characters against the "
                f"{MAX_BODY_CHARS} a tracker accepts, and every part of it "
                f"that is left is required: the measurement evidence, the "
                f"provenance markers, and one quoted excerpt. Nothing here "
                f"will publish a defect report with its measurements or its "
                f"log evidence cut away, so shorten the diagnosis prose or "
                f"the `/deflake` invocation this handoff records and file "
                f"again")
        evidence = evidence[:-1]
        trimmed = True


# ==========================================================================
# The publication boundary
# ==========================================================================
class Publication:
    """What a publisher must answer. Injected so a gate can fake it.

    Two operations and no more: RECONCILE a publication key against the
    tracker, and CREATE one issue. Nothing here edits an issue, labels
    one, or closes one — the review lane owns an issue once it exists.

    `find` answers with the issue's `number`, `url` AND `body`, or None.
    The body is not a convenience: it is what proves the match is the
    marker line rather than a quotation, and it is where the issue's own
    `issue-origin` brand is read from. `create` answers with `number`
    and `url`, since this workflow wrote that body itself.
    """

    def find(self, key: str):
        raise NotImplementedError

    def create(self, *, title: str, body: str):
        raise NotImplementedError


ISSUE_URL = re.compile(r"^https://[^\s]+/issues/(\d+)$")


class GitHubPublication(Publication):
    """The real boundary: `gh`, and nothing else.

    Constructed but never used by the document paths, so the module
    imports and every test runs on a machine with no `gh` and no
    network.
    """

    def __init__(self, repo: str | None = None, *, timeout: float = 120.0):
        self.repo = repo
        self.timeout = timeout

    def _gh(self, *args) -> str:
        argv = ["gh", *args]
        if self.repo:
            argv += ["--repo", self.repo]
        try:
            done = subprocess.run(argv, capture_output=True, text=True,
                                  timeout=self.timeout)
        except (OSError, subprocess.SubprocessError) as error:
            raise PublicationFailed(
                f"`{' '.join(argv)}` could not be run ({error}); nothing was "
                f"recorded, so the attempt stays resumable") from None
        if done.returncode != 0:
            raise PublicationFailed(
                f"`{' '.join(argv)}` exited {done.returncode} "
                f"({done.stderr.strip()[:400]}); nothing was recorded, so "
                f"the attempt stays resumable")
        return done.stdout

    def find(self, key: str):
        stdout = self._gh("issue", "list", "--state", "all", "--limit", "50",
                          "--search", key, "--json", "number,url,body")
        try:
            rows = json.loads(stdout or "[]")
        except json.JSONDecodeError as error:
            raise PublicationFailed(
                f"`gh issue list` did not answer with JSON ({error})"
            ) from None
        if not isinstance(rows, list):
            raise PublicationFailed(
                "`gh issue list` did not answer with a list of issues")
        # The oldest match wins, so a resume that races a duplicate
        # still converges on one issue rather than alternating.
        matches = sorted(
            (row for row in rows
             if isinstance(row, dict) and carries_key(row.get("body"), key)),
            key=lambda row: row.get("number") or 0)
        if not matches:
            return None
        return {"number": matches[0].get("number"),
                "url": matches[0].get("url"),
                "body": matches[0].get("body")}

    def create(self, *, title: str, body: str):
        handle, name = tempfile.mkstemp(prefix="deflake_issue_",
                                        suffix=".md")
        try:
            with os.fdopen(handle, "w", encoding="utf-8") as stream:
                stream.write(body)
            stdout = self._gh("issue", "create", "--title", title,
                              "--body-file", name)
        finally:
            try:
                os.unlink(name)
            except OSError:
                pass
        # `gh issue create` prints the new issue's URL on its last line.
        lines = [line.strip() for line in (stdout or "").splitlines()
                 if line.strip()]
        url = lines[-1] if lines else ""
        match = ISSUE_URL.match(url)
        if not match:
            raise PublicationFailed(
                f"`gh issue create` did not print an issue URL (got "
                f"{url[:200]!r}); the issue may exist, so re-run this "
                f"workflow — the publication key reconciles it rather than "
                f"filing a second one")
        return {"number": int(match.group(1)), "url": url}


def require_issue_identity(value, key: str, origin: str) -> dict:
    """A CREATED issue, held to a shape the census can store.

    `origin` is the caller's here, and correctly so: this workflow wrote
    that body and put the marker in it. A RECONCILED issue is the other
    case, and it reads its brand off the issue instead.
    """
    number, url = _require_number_and_url(value)
    return {"number": number, "url": url, "publication_key": key,
            "origin": origin}


def _require_number_and_url(value) -> tuple:
    if not isinstance(value, dict):
        raise PublicationFailed(
            f"the publication boundary answered with "
            f"{type(value).__name__}, not an issue identity")
    number = value.get("number")
    if not isinstance(number, int) or isinstance(number, bool) or number < 1:
        raise PublicationFailed(
            f"the publication boundary answered with the issue number "
            f"{number!r}, which is not a positive integer")
    url = value.get("url")
    if not isinstance(url, str) or not url.startswith("https://"):
        raise PublicationFailed(
            f"the publication boundary answered with the issue URL {url!r}, "
            f"which is not an absolute https URL a reviewer can open")
    return number, url


def require_reconciled_issue(value, key: str) -> dict:
    """An EXISTING issue this attempt was already filed as.

    Held to more than a created one, because nothing here wrote it: the
    marker line has to be in the fetched body — a search index match is
    not evidence — and the recorded brand is the one the ISSUE carries,
    not the one the resuming invocation happens to run under. An issue
    with no readable origin marker is not one this workflow filed, so it
    is a publication failure rather than something to record under a
    guess.
    """
    number, url = _require_number_and_url(value)
    body = value.get("body")
    if not carries_key(body, key):
        raise PublicationFailed(
            f"the publication boundary answered with issue #{number}, whose "
            f"body carries no `{PUBLICATION_MARKER}` line for {key}; a "
            f"tracker search matches text anywhere, so an issue that only "
            f"quotes the key is not the one this attempt was filed as")
    origin = body_origin(body)
    if origin is None:
        raise PublicationFailed(
            f"issue #{number} carries this attempt's publication key but no "
            f"readable `{ORIGIN_MARKER}` marker, so it is not one this "
            f"workflow filed and the brand its review routes on cannot be "
            f"recorded")
    return {"number": number, "url": url, "publication_key": key,
            "origin": origin}


# ==========================================================================
# The durable record
# ==========================================================================
def utc_now() -> str:
    return deflake_handoff.utc_now()


def outcome_record(handoff, *, now: str, issue: dict) -> dict:
    """The census record one published production defect produces.

    Deliberately the SAME shape `deflake_outcome.outcome_record` writes,
    plus the issue identity: one `outcomes` collection holds every
    ending of a de-flake attempt, and a second shape would make the row
    unreadable without knowing which workflow wrote each entry.
    `recommendation` and `comparison` are the two route-specific fields
    this ending has nothing to say in, and they are stored as an
    explicit null rather than dropped.

    Raises `NonSuccess` when the evidence does not support the route.
    Nothing is written here; `publish` is what installs it.
    """
    _delegate_timestamp(now)
    require_supported(handoff)
    return {
        "attempt": handoff.attempt,
        "outcome": OUTCOME_PRODUCTION_DEFECT,
        "reason": handoff.reason,
        "probe": handoff.probe,
        "timestamp_utc": now,
        "baseline_sha": handoff.baseline_sha,
        "acceptable_failures": handoff.acceptable_failures,
        "targets": handoff.targets,
        "configuration": handoff.configuration,
        "invocation": handoff.invocation,
        "measurements": [handoff.measurements[role].summary()
                         for role in ROLES if role in handoff.measurements],
        "retained_artifacts": handoff.artifacts(),
        "summary": handoff.summary,
        "recommendation": None,
        "comparison": None,
        "issue": copy.deepcopy(issue),
    }


def _delegate_timestamp(now: str) -> None:
    try:
        probe_census.parse_timestamp(now, "the outcome timestamp")
    except probe_census.CensusError as error:
        raise HandoffError(f"the outcome timestamp: {error}") from None


def stored_record(census_path: Path, handoff):
    """This attempt's already-recorded outcome, or None.

    Read OUTSIDE the census lock and used for exactly one decision: not
    to touch the tracker at all when the attempt is already complete.
    Correctness does not rest on it — `probe_census.ingest_outcome`
    re-reads under the lock and refuses a conflicting record — so a
    stale read costs a redundant reconcile, never a duplicate.
    """
    try:
        document = json.loads(Path(census_path).read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    if not isinstance(document, dict):
        # Not a census at all. The locked transaction is what refuses
        # that, with a message naming the file; this read answers only
        # "is the attempt already complete", and it is not.
        return None
    row = probe_census.find_entry(document, handoff.probe)
    found = probe_census.find_outcome((row or {}).get("census"),
                                      handoff.attempt)
    if found is None:
        return None
    if found.get("outcome") != OUTCOME_PRODUCTION_DEFECT:
        raise NonSuccess(
            f"attempt {handoff.attempt!r} is already recorded for "
            f"{handoff.probe!r} as the {found.get('outcome')!r} outcome, so "
            f"this is not a resume of that attempt; an attempt identity "
            f"must identify one attempt")
    issue = found.get("issue")
    if not isinstance(issue, dict):
        raise NonSuccess(
            f"attempt {handoff.attempt!r} is already recorded as a "
            f"{OUTCOME_PRODUCTION_DEFECT!r} outcome carrying no issue "
            f"identity; that record could not have been written by this "
            f"workflow and is not one to resume")
    return found


def reuse_stored_publication(candidate: dict, stored: dict) -> dict:
    """The fields of a rebuilt record that cannot be derived again.

    Idempotency is the WHOLE record, and this ending has two fields a
    retry cannot reproduce from the handoff: `timestamp_utc`, which
    comes from a clock, and `issue`, which came from the tracker and
    carries the review-routing brand the resuming invocation may have
    spelled differently. Both are copied from what the census actually
    holds, INSIDE its transaction, so a replay is a replay rather than a
    conflict — and two genuinely different outcomes still cannot be made
    to agree, because nothing else is copied across.
    """
    candidate = deflake_handoff.reuse_stored_timestamp(candidate, stored)
    issue = stored.get("issue")
    if isinstance(issue, dict):
        candidate = dict(candidate, issue=copy.deepcopy(issue))
    return candidate


class Published:
    """What one accepted attempt did, once the tracker and census answered."""

    def __init__(self, record: dict, *, resumed: bool, durable: bool,
                 created: bool, reconciled: bool, trimmed: bool,
                 changed_probe: bool, opened_pull_request: bool):
        self.record = record
        self.resumed = resumed
        self.durable = durable
        # Whether THIS invocation created the issue, whether it found
        # one already filed under the publication key, or neither
        # because the census already held the completed outcome. Three
        # distinct histories that all end with one issue.
        self.created = created
        self.reconciled = reconciled
        self.trimmed = trimmed
        self.changed_probe = changed_probe
        self.opened_pull_request = opened_pull_request

    def to_document(self) -> dict:
        return {"outcome": self.record["outcome"],
                "attempt": self.record["attempt"],
                "probe": self.record["probe"],
                "issue": copy.deepcopy(self.record["issue"]),
                "resumed": self.resumed,
                "durable": self.durable,
                "created_issue": self.created,
                "reconciled_issue": self.reconciled,
                "evidence_trimmed": self.trimmed,
                "changed_the_probe": self.changed_probe,
                "opened_pull_request": self.opened_pull_request,
                "terminal": True,
                "record": self.record}


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
    _delegate_timestamp(now)

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
                         worktrees=deflake_diagnosis.worktree_paths(),
                         primary=deflake_diagnosis.primary_checkout())
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
