#!/usr/bin/env python3
"""The issue one production-defect diagnosis publishes, and its identity (#1438).

`tools/deflake_issue.py` files ONE tracker issue for a diagnosed
production defect. This module owns what that issue IS — the stable key
it is published under, the two review-routing markers, and the rendered
title and body — and nothing about how it reaches a tracker or a census.

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate

Its ownership runs from an accepted handoff to a finished `(body,
trimmed)` pair: the publication-key schema and its derivation, marker
construction and recognition, the fence-aware prose scan, the
neutralization funnel, the title, the measurement, diagnosis and
evidence sections, the tracker-body and diagnosis bounds, and the
evidence trimming. It reads no artifact, calls no tracker and writes no
record, so it imports none of the other owners and not the façade; the
evidence it renders arrives as data the evidence owner produced.

Quoted content cannot forge the routing marker
----------------------------------------------
Everything this body quotes — an engine log above all — is arbitrary
text, and one of the things arbitrary text contains is an `issue-origin`
marker. `approve_issues.issue_origin` scans the WHOLE raw body, fenced
blocks included and case-insensitively, and RAISES on two markers naming
different brands, so a quoted log carrying one would stop the filed
issue entering the review gate at all: the one thing the route exists to
do.

So the assembled body passes through ONE funnel that breaks every
HTML-comment opener, and only then are the two real markers appended.
One funnel rather than an escape at each interpolation site, because a
checklist covering the diagnosis prose, the evidence lines, the artifact
paths, the command tokens and every quoted log is a checklist that
eventually misses one. The finished text is then CHECKED — exactly one
origin, exactly one publication key, exactly two comment openers in the
whole body — rather than trusted, because the cost of being wrong is
silent.

Every part of the body is required, so nothing is silently cut
------------------------------------------------------------
The measurement facts, the provenance markers and at least one quoted
excerpt are what make the issue reviewable, and a tracker body has a
size limit. Only the SECOND and later runs' evidence may be dropped to
fit; when even that is not enough the publication is REFUSED, because a
defect report published with its measurements or its log evidence
truncated away is the thing this workflow exists to prevent. #1437
bounds neither the diagnosis summary nor its evidence list, so those are
bounded here too — the three limits below are declared once, in the
module that renders what they bound, and the façade's
`require_defect_diagnosis` imports them rather than restating them.

`MAX_BODY_CHARS` lives here and only here for the same reason. It is the
one constant of this family a caller substitutes to exercise the
unfittable-body refusal, and `issue_body` reads it out of THIS module's
globals; a second binding on the façade would take an assignment and
change nothing.
"""
from __future__ import annotations

import hashlib
import os
import re
import sys

# `tools/` carries no `__init__.py`, so it is an implicit namespace
# package: under the repository-root spelling `import
# tools.deflake_issue_document` this directory is NOT on `sys.path`, and
# the sibling imports below resolve only because the pre-split module
# put its own directory there first. Every owner in this family carries
# the same bootstrap ahead of its own sibling imports.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake_handoff  # noqa: E402
import probe_protocol  # noqa: E402
import probe_runner_registry  # noqa: E402

ROLES = deflake_handoff.ROLES
ROLE_BASELINE = deflake_handoff.ROLE_BASELINE
NonSuccess = deflake_handoff.NonSuccess


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


# GitHub refuses a body over 65536 characters. The evidence section is
# what gets trimmed if the rendered body would exceed this, never the
# measurement table — and the trim says so on the record.
MAX_BODY_CHARS = 60000
MAX_TITLE_CHARS = 240
# #1437 requires a diagnosis summary and at least one evidence line and
# bounds NEITHER, so an accepted producer record can carry prose longer
# than a whole issue body. Bounded rather than truncated later: the
# summary is what the issue is about and the evidence is what makes it
# reviewable, so silently cutting either would publish a defect report
# whose own claim had been trimmed away. Declared here, where the body
# that carries them is rendered, and IMPORTED by the facade's
# `require_defect_diagnosis`, which refuses on them at the entry gate; a
# second declaration beside that check is how a bound and the render it
# bounds come to disagree.
MAX_DIAGNOSIS_SUMMARY = 4000
MAX_DIAGNOSIS_EVIDENCE = 20
MAX_DIAGNOSIS_EVIDENCE_ITEM = 1000


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
