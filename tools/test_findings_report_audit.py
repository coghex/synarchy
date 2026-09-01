#!/usr/bin/env python3
"""Unit tests for findings_report_audit.py (issue #1196).

Drives the audit against constructed in-memory reports, never by
editing the real `docs/code_health_findings.md` -- a checked-in failing
fixture would make the repository-wide audit fail by construction. This
mirrors the convention stated in tools/test_cabal_module_audit.py and
tools/test_engine_env_capability_audit.py.

The real report is exercised too, so a future reformat that silently
breaks lexing fails here rather than turning the audit into a no-op
that passes everything.

Usage:
  python3 tools/test_findings_report_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import findings_report_audit as fra  # type: ignore

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


def report(checklist: list[str], findings: list[str]) -> str:
    """A minimal report with the real file's two-section shape."""
    return "\n".join([
        "# Fixture findings",
        "",
        "## Status",
        "",
        *checklist,
        "",
        "## Batch 1 — fixture",
        "",
        *findings,
        "",
    ])


def problems(checklist: list[str], findings: list[str]) -> list[str]:
    return fra.audit(report(checklist, findings))[1]


def test_agreeing_markers_pass() -> None:
    print("\n[pass] agreeing markers, in every supported form")
    cases = [
        ("bare on both sides (an unprocessed finding)", "", ""),
        ("a plain issue marker", "[#947] ", "[#947]"),
        ("an annotated issue marker", "[#936, closed obsolete] ",
         "[#936, closed obsolete]"),
        ("a no-issue marker", "[no-issue] ", "[no-issue]"),
        ("a deferred marker", "[deferred] ", "[deferred]"),
    ]
    for label, heading_marker, checklist_marker in cases:
        tail = f" — {checklist_marker}" if checklist_marker else ""
        found = problems(
            [f"- [x] CH-1. A title{tail}"],
            [f"### {heading_marker}CH-1. A title"])
        expect(found == [], f"{label} agrees ({found})")


def test_marker_present_on_one_side_only_fails() -> None:
    print("\n[fail] a marker on one side only -- the two verified drifts")
    # Commit 82607204's shape: heading marked while landing the fix,
    # checklist left unchecked and bare.
    found = problems(["- [ ] CH-126. Wrapping is byte-wise"],
                     ["### [#1159] CH-126. Wrapping is byte-wise"])
    expect(len(found) == 1 and "CH-126" in found[0],
           f"a marked heading against a bare checklist entry fails ({found})")
    expect(found and "[#1159]" in found[0] and "(no marker)" in found[0],
           f"...and names both sides' markers ({found})")

    # Commit 89b015d3's shape: checklist marked, heading left bare.
    found = problems(["- [x] CH-73. `serializeCodec` — [#1093]"],
                     ["### CH-73. `serializeCodec`"])
    expect(len(found) == 1 and "CH-73" in found[0],
           f"a marked checklist entry against a bare heading fails ({found})")


def test_differing_nonempty_markers_fail() -> None:
    print("\n[fail] two different markers")
    for heading, entry in (("[#947]", "[#948]"),
                           ("[no-issue]", "[deferred]"),
                           ("[#985]", "[no-issue]"),
                           ("[#936]", "[#936, closed obsolete]")):
        found = problems([f"- [x] CH-4. A title — {entry}"],
                         [f"### {heading} CH-4. A title"])
        expect(len(found) == 1,
               f"heading {heading} against checklist {entry} fails ({found})")


def test_explanatory_text_after_the_marker_is_ignored() -> None:
    print("\n[pass] trailing prose after a checklist marker")
    # CH-54's shape: a terminal marker with a note on what it covers.
    found = problems(
        ["- [x] CH-54. 97 exported names — [#1083] covers the surviving 64"],
        ["### [#1083] CH-54. 97 exported names"])
    expect(found == [], f"a note after an issue marker is ignored ({found})")

    # CH-67's shape: an UNCHECKED box whose marker carries a precondition.
    found = problems(
        ["- [ ] CH-67. `parseRegion` — [deferred]: #1081 retypes the region"],
        ["### [deferred] CH-67. `parseRegion`"])
    expect(found == [],
           f"an unchecked entry's marker is still extracted ({found})")

    # The box state is the processing lane's judgement, not this audit's:
    # markers agree, so an unchecked terminal marker is not our failure.
    found = problems(["- [ ] CH-8. A title — [#944]"],
                     ["### [#944] CH-8. A title"])
    expect(found == [],
           f"the checkbox state is not audited when markers agree ({found})")


def test_partial_findings_pass_in_both_representations() -> None:
    print("\n[pass] `> **Partial:**` entries")
    # An OPEN partial: no heading marker (putting [#N] there would strand
    # the remainder), unchecked box, bare checklist entry.
    found = problems(
        ["- [ ] CH-43. Five Lua API modules are 400-520 lines"],
        ["### CH-43. Five Lua API modules are 400-520 lines",
         "> **Partial:** #985 extracted only the `EngineEnv`-free "
         "definitions, leaving the facade at 859 lines.",
         "",
         "Body prose."])
    expect(found == [],
           f"an open partial passes bare on both sides ({found})")

    # A partial whose remainder was later dispositioned: the note is
    # historical and the terminal marker sits on both sides.
    found = problems(
        ["- [x] CH-43. Five Lua API modules — [no-issue]: #985 split "
         "`Save.hs`, remainder closed"],
        ["### [no-issue] CH-43. Five Lua API modules",
         "> **Partial:** ... **Historical (superseded 2026-08-06):** the "
         "remainder was dispositioned `[no-issue]` below.",
         "",
         "> **Disposition:** No issue — the remainder is a size complaint."])
    expect(found == [],
           f"a historical partial with a terminal marker passes ({found})")


def test_missing_counterpart_fails() -> None:
    print("\n[fail] a CH item present on one side only")
    found = problems(["- [x] CH-1. A title — [#947]",
                      "- [x] CH-2. Another — [#948]"],
                     ["### [#947] CH-1. A title"])
    expect(len(found) == 1 and "CH-2" in found[0] and "checklist" in found[0],
           f"a checklist entry with no heading fails ({found})")

    found = problems(["- [x] CH-1. A title — [#947]"],
                     ["### [#947] CH-1. A title",
                      "### [#948] CH-2. Another"])
    expect(len(found) == 1 and "CH-2" in found[0] and "heading" in found[0],
           f"a heading with no checklist entry fails ({found})")


def test_duplicate_counterpart_fails() -> None:
    print("\n[fail] a CH number declared twice on one side")
    for label, checklist, findings in (
        ("checklist",
         ["- [x] CH-1. A title — [#947]", "- [x] CH-1. A dupe — [#947]"],
         ["### [#947] CH-1. A title"]),
        ("headings",
         ["- [x] CH-1. A title — [#947]"],
         ["### [#947] CH-1. A title", "### [#947] CH-1. A dupe"]),
    ):
        try:
            problems(checklist, findings)
        except fra.ReportError as error:
            expect("CH-1" in str(error),
                   f"a duplicate {label} entry is rejected ({error})")
        else:
            expect(False, f"a duplicate {label} entry was not rejected")


def test_unlexable_lines_fail_loudly() -> None:
    print("\n[fail] shapes the lexer cannot read")
    cases = [
        ("a checklist line that is not a CH entry",
         ["- [x] Not a CH item at all"], ["### [#947] CH-1. A title"]),
        ("a checklist box that is neither ` ` nor `x`",
         ["- [X] CH-1. A title — [#947]"], ["### [#947] CH-1. A title"]),
        ("a `### ` heading that is not a CH finding",
         ["- [x] CH-1. A title — [#947]"], ["### Some other subsection"]),
        ("a marker outside marker position",
         ["- [x] CH-1. A title [#947] — trailing prose"],
         ["### [#947] CH-1. A title"]),
    ]
    for label, checklist, findings in cases:
        try:
            problems(checklist, findings)
        except fra.ReportError:
            expect(True, f"{label} raises rather than passing silently")
        else:
            expect(False, f"{label} was silently accepted")


def test_ambiguous_bare_title_is_not_a_false_failure() -> None:
    print("\n[pass] a bare entry whose title contains the separator")
    found = problems(["- [ ] CH-9. Two zoom namespaces — a real split"],
                     ["### CH-9. Two zoom namespaces — a real split"])
    expect(found == [],
           f"an em-dash in a bare title is not read as a marker ({found})")


def test_end_to_end_exit_codes() -> None:
    print("\n[cli] exit codes against files on disk")
    audit = Path(__file__).resolve().parent / "findings_report_audit.py"
    passing = report(["- [x] CH-1. A title — [#947]"],
                     ["### [#947] CH-1. A title"])
    failing = report(["- [ ] CH-1. A title"], ["### [#947] CH-1. A title"])
    with tempfile.TemporaryDirectory() as tmp:
        for label, text, expected in (("an agreeing report", passing, 0),
                                      ("a drifted report", failing, 1)):
            path = Path(tmp) / f"{expected}.md"
            path.write_text(text, encoding="utf-8")
            result = subprocess.run(
                [sys.executable, str(audit), str(path)],
                cwd=tmp, capture_output=True, text=True)
            expect(result.returncode == expected,
                   f"{label} exits {expected} (got {result.returncode}: "
                   f"{result.stdout.strip()[-160:]})")


def test_real_report() -> None:
    print("\n[real] the checked-in docs/code_health_findings.md")
    text = fra.DEFAULT_REPORT.read_text(encoding="utf-8")
    lines = text.split("\n")
    checklist = fra.parse_checklist(lines)
    headings = fra.parse_headings(lines)
    expect(len(checklist) > 100,
           f"the real checklist lexes ({len(checklist)} entries) -- a "
           "reformat that broke parsing would show up as a small count")
    expect(len(checklist) == len(headings),
           f"the real report has one heading per checklist entry "
           f"({len(checklist)} vs {len(headings)})")
    markers = {entry.marker for entry in headings}
    expect("[no-issue]" in markers,
           "the real report's `[no-issue]` headings are recognised")
    expect(any(m.startswith("[#") for m in markers),
           "the real report's issue-number headings are recognised")
    # There is deliberately NO "at least one bare heading" case here, and
    # reinstating one re-breaks this file. A bare heading is an
    # UNDISPOSITIONED finding, so asserting the real report still has one
    # measured how much work the processing lane had left rather than
    # anything about the lexer -- and it failed the moment that lane
    # caught up. Master 8f451433 dispositioned the last bare finding of
    # 138, which turned this self-test red on master and on every branch
    # built against it, none of which had touched the report.
    #
    # No coverage is lost. The bare-marker path is exercised three times
    # over by the synthetic fixtures above -- bare on both sides, either
    # side bare against a marked counterpart, and an open
    # `> **Partial:**` -- and none of those can expire, because this file
    # owns them.
    #
    # Nor is a substitute assertion available: `_MARKER` is a closed
    # alternation, so a marker in an unrecognised shape does not lex into
    # an odd marker to be caught, it raises at `parse_headings`. A check
    # for one would be vacuous by construction, which is worse than an
    # absent check because it reads as coverage.
    #
    # There is deliberately NO "at least one `[deferred]` heading" case
    # either, and it was removed for the SAME reason, one step later.
    # `[deferred]` reads like a disposition but is transitional: it means
    # "blocked on a stated precondition", so when that precondition lands
    # the processing lane files the entry and the marker becomes `[#N]`.
    # A fully caught-up report therefore has ZERO of them, and asserting
    # one exists measured the deferral backlog rather than the lexer.
    # Master 559e946f filed the last two (CH-67 as #1481, CH-138 as
    # #1482), taking `[deferred]` headings from 2 to 0 and turning this
    # self-test red on master and on every branch built against it, none
    # of which had touched the report -- the identical failure the bare
    # case above had already produced at 8f451433.
    #
    # Only `[no-issue]` and `[#N]` are terminal, and only they accumulate
    # and never leave, so only they can back a real-report assertion. Do
    # not add a third form here without first checking it is terminal.
    #
    # No coverage is lost here either: `[deferred]` is exercised three
    # times over by the synthetic fixtures -- as an agreeing form, as the
    # losing side of a differing-marker pair, and in CH-67's shape of an
    # UNCHECKED box whose marker carries a precondition -- and this file
    # owns all three, so none of them can expire.
    #
    # What the real report is here to catch is the lexer that silently
    # stops recognising markers: every heading would read as bare, both
    # sides would then agree, and the audit would pass everything. The
    # two form assertions above ARE that guard -- each needs a real
    # marker to survive lexing -- and they hold on a fully dispositioned
    # report, because terminal dispositions accumulate and never leave.

    with tempfile.TemporaryDirectory() as tmp:
        result = subprocess.run(
            [sys.executable,
             str(Path(__file__).resolve().parent / "findings_report_audit.py")],
            cwd=tmp, capture_output=True, text=True)
        expect(result.returncode == 0,
               "the audit exits 0 on this repository when run from an "
               f"unrelated cwd (got {result.returncode}: "
               f"{result.stdout.strip()[-200:]})")


def main() -> int:
    selftest.parse_verbose()
    test_agreeing_markers_pass()
    test_marker_present_on_one_side_only_fails()
    test_differing_nonempty_markers_fail()
    test_explanatory_text_after_the_marker_is_ignored()
    test_partial_findings_pass_in_both_representations()
    test_missing_counterpart_fails()
    test_duplicate_counterpart_fails()
    test_unlexable_lines_fail_loudly()
    test_ambiguous_bare_title_is_not_a_false_failure()
    test_end_to_end_exit_codes()
    test_real_report()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftest.concluded(1)
    return selftest.concluded(0, "\nAll findings_report_audit tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
