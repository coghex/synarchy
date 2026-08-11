#!/usr/bin/env python3
"""Findings-report status agreement audit (issue #1196).

`docs/code_health_findings.md` records each finding's status TWICE: once
as a `## Status` checklist line near the top, and once as a marker on
the finding's own `### ` heading. The checklist is the at-a-glance
index; the heading is the authority the report processor reads when it
picks the next unprocessed finding.

Two independent lanes write that file. The report-processing lane owns
the status fields; an implementation PR resolving a finding also edits
the file, because the issue body's Related section points at the entry.
Nothing stopped the implementation lane from writing processor-owned
state, and it already has: commit `82607204` changed `### CH-126.` to
`### [#1159] CH-126.` while landing the fix, leaving the checklist line
unchecked and bare. Commit `89b015d3` drifted CH-73 the other way --
checklist marked, heading bare.

Either direction re-files merged work. A bare heading loses under the
processor's "headings win, correct the checklist" tie-break and unchecks
a finding that was already resolved; a bare checklist line makes a
marked finding look unprocessed. The cost is not only a duplicate issue:
`.github/workflows/review-gate.yml` strips `reviewed:approve` when a
push touches a file an open PR also owns, so every master-side report
edit costs an open PR that touches the report its approval.

What this checks, per CH item:

  1. The checklist and the headings declare the SAME set of CH numbers,
     each exactly once. A number present on one side only, or twice on
     either side, is a failure -- otherwise a parser that silently
     skipped an item could report success without auditing it.
  2. The two markers are byte-identical. A bare entry has no marker, and
     bare-on-both-sides agrees.

What it deliberately does NOT check: whether a marker is the RIGHT one,
whether the checkbox matches the marker's terminality, or anything about
a finding's narrative body. Those are the processing lane's judgement,
not a mechanical invariant.

`> **Partial:**` entries need no special case. An entry whose partial
coverage is still open carries no heading marker and a bare checklist
line (bare/bare agrees); one whose remainder was later dispositioned
carries that terminal marker in both places (CH-43, CH-54). Both pass on
the same rule.

Marker grammar (the report's own status legend, plus the annotated form
CH-5 uses): `[#N]`, `[#N, <note>]`, `[no-issue]`, `[deferred]`. The
comparison is on the complete token, literally.

A checklist line may carry explanatory text after its marker -- a
`[deferred]` line appends its precondition, and CH-54's names what its
issue covers. The marker is therefore read as the token that OPENS the
segment following the entry's final em-dash separator, and trailing
prose is ignored. A line that carries a marker token somewhere other
than that position is a hard parse error, not a silent pass: a report
whose shape this script cannot lex must fail loudly rather than audit
nothing.

Usage:
  python3 tools/findings_report_audit.py [report.md]
Self-tests: python3 tools/test_findings_report_audit.py
Exit codes: 0 = every CH item's two markers agree, 1 = one or more disagree
            (or the report could not be lexed).
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_REPORT = REPO_ROOT / "docs" / "code_health_findings.md"

# The em-dash separator between a checklist entry's title and its marker.
MARKER_SEPARATOR = " — "

# A status marker token. `[#936, closed obsolete]` is the annotated issue
# form; the note may not itself contain `]`, which keeps the token's end
# unambiguous.
_MARKER = r"\[(?:#\d+(?:,[^\]]*)?|no-issue|deferred)\]"
MARKER_RE = re.compile(_MARKER)
LEADING_MARKER_RE = re.compile(rf"^{_MARKER}")

# `- [x] CH-12. Title -- [#947]` / `- [ ] CH-95. Title`
CHECKLIST_RE = re.compile(r"^- \[([ x])\] CH-(\d+)\.[ \t]*(.*)$")
# Any checklist-shaped line, so a malformed one is caught rather than skipped.
CHECKLIST_SHAPE_RE = re.compile(r"^- \[")

# `### [#947] CH-12. Title` / `### CH-95. Title`
HEADING_RE = re.compile(rf"^### (?:({_MARKER})[ \t]+)?CH-(\d+)\.[ \t]*(.*)$")
# Any finding-heading-shaped line, for the same reason.
HEADING_SHAPE_RE = re.compile(r"^### ")


class ReportError(Exception):
    """The report's shape could not be lexed. Never a silent pass."""


class Entry:
    """One side's declaration of a CH item's status."""

    def __init__(self, number: int, line: int, marker: str) -> None:
        self.number = number
        self.line = line
        self.marker = marker

    def shown(self) -> str:
        return self.marker if self.marker else "(no marker)"


def checklist_marker(number: int, line_no: int, rest: str) -> str:
    """The marker a checklist entry declares, or `""` for a bare entry.

    The marker opens the segment after the entry's FINAL separator, so
    trailing prose (a `[deferred]` precondition, a note on what an issue
    covers) is ignored. A title may itself contain the separator; that is
    only unambiguous when the entry carries no marker at all, so a marker
    token found outside marker position is an error.
    """
    head, sep, tail = rest.rpartition(MARKER_SEPARATOR)
    if sep:
        found = LEADING_MARKER_RE.match(tail)
        if found:
            return found.group(0)
    if MARKER_RE.search(rest):
        raise ReportError(
            f"{line_no}: CH-{number}'s checklist entry carries a status "
            f"marker somewhere other than immediately after its final "
            f"'{MARKER_SEPARATOR.strip()}' separator, so which token is "
            f"the marker is ambiguous:\n    - [ ] CH-{number}. {rest}")
    return ""


def parse_checklist(lines: list[str]) -> list[Entry]:
    entries: list[Entry] = []
    for line_no, line in enumerate(lines, 1):
        if not CHECKLIST_SHAPE_RE.match(line):
            continue
        found = CHECKLIST_RE.match(line)
        if not found:
            raise ReportError(
                f"{line_no}: checklist line is not a `- [ ] CH-<n>. "
                f"<title>` entry:\n    {line}")
        number = int(found.group(2))
        entries.append(
            Entry(number, line_no,
                  checklist_marker(number, line_no, found.group(3).rstrip())))
    return entries


def parse_headings(lines: list[str]) -> list[Entry]:
    entries: list[Entry] = []
    for line_no, line in enumerate(lines, 1):
        if not HEADING_SHAPE_RE.match(line):
            continue
        found = HEADING_RE.match(line)
        if not found:
            raise ReportError(
                f"{line_no}: finding heading is not a `### [marker] "
                f"CH-<n>. <title>` heading:\n    {line}")
        entries.append(
            Entry(int(found.group(2)), line_no, found.group(1) or ""))
    return entries


def index(entries: list[Entry], side: str) -> dict[int, Entry]:
    """Key entries by CH number, rejecting duplicates."""
    by_number: dict[int, Entry] = {}
    for entry in entries:
        earlier = by_number.get(entry.number)
        if earlier is not None:
            raise ReportError(
                f"{entry.line}: CH-{entry.number} has a second {side} "
                f"(the first is at line {earlier.line}); each CH item must "
                f"appear exactly once on each side.")
        by_number[entry.number] = entry
    return by_number


def audit(text: str) -> tuple[int, list[str]]:
    """The CH item count, and every disagreement between the two sides."""
    lines = text.split("\n")
    checklist = index(parse_checklist(lines), "checklist entry")
    headings = index(parse_headings(lines), "finding heading")

    problems: list[str] = []
    for number in sorted(set(checklist) | set(headings)):
        entry = checklist.get(number)
        heading = headings.get(number)
        if heading is None:
            problems.append(
                f"CH-{number}: checklist entry at line {entry.line} has no "
                f"`### ` finding heading.")
            continue
        if entry is None:
            problems.append(
                f"CH-{number}: finding heading at line {heading.line} has no "
                f"`## Status` checklist entry.")
            continue
        if entry.marker != heading.marker:
            problems.append(
                f"CH-{number}: heading (line {heading.line}) says "
                f"{heading.shown()}, checklist (line {entry.line}) says "
                f"{entry.shown()}.")
    return len(set(checklist) | set(headings)), problems


def run(path: Path) -> int:
    try:
        text = path.read_text(encoding="utf-8")
    except OSError as error:
        print(f"Could not read {path}: {error}")
        return 1
    try:
        total, problems = audit(text)
    except ReportError as error:
        print(f"{path}: the report could not be read\n  {error}")
        return 1

    print(f"  {path.name}: {total} CH item(s)")
    if problems:
        print(f"\n{len(problems)} status disagreement(s):")
        for problem in problems:
            print(f"  {problem}")
        print("\nThe heading marker is authoritative -- correct the "
              "checklist line to match it.\nOnly the report-processing "
              "lane may change either one; an implementation PR\nedits a "
              "finding's narrative body and nothing else.")
        return 1
    print("\nEvery CH item's heading and checklist markers agree")
    return 0


def main(argv: list[str]) -> int:
    target = Path(argv[1]) if len(argv) > 1 else DEFAULT_REPORT
    return run(target)


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
