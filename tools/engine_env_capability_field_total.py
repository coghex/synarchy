#!/usr/bin/env python3
"""SS1's audited field total and field span (issue #1669; extracted from
tools/engine_env_capability_audit.py by issue #2064).

SS1 used to state a hand-maintained field total ("exactly 83 fields")
and three hand-written `src/Engine/Core/State.hs` line anchors. All
four had drifted from the live record while every gate stayed green:
the SS5 row check (tools/engine_env_capability_inventory.py) compares
the live field SET with SS5's row set, so it never looked at the prose
total at all, and SS6.2's assignment-method procedure repeated the same
stale number a second time.

`audit_field_total` re-derives the count and the two span field names
from the live declaration and rejects the marked SS1 block when either
disagrees; SS6.2's assignment-method sentence, which used to repeat the
same total, is marked as prose that must carry no number at all. Each
marker pair is bound to the section it governs, so it cannot be parked
somewhere inert while the real sentence regains a hand-maintained
number. The marker contract itself is documented in the section comment
below.

This owner reads the doc and the live field LIST and nothing else: it
takes no source tree, and it shares SS6.2's heading with the
access-governance owner through tools/engine_env_capability_common.py
rather than either module holding a second copy of the literal.

Not independently a gate: `python3 tools/engine_env_capability_audit.py`
remains the one command CI and tools/ci-local.sh run.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import NamedTuple

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore
    BACKTICK_RE, ENGINE_ENV_FILE, INVENTORY_PATH, SECTION_6_2_HEADING,
)


# ===========================================================================
# The marker contract (issue #1669)
# ===========================================================================
#
# The fix follows the rule this repository settled on in issue #1584:
# a total a document displays is obtained mechanically or is not
# displayed. Here it is displayed and mechanically checked, which needs
# the governed prose to be delimited rather than pattern-matched out of
# free text -- a prose validator that has to GUESS which sentence is
# the total is the shape that burned review rounds in #704/#1128/#1309.
# Two explicit HTML-comment markers do that instead:
#
#   `<!-- engineenv-field-total -->` ... `<!-- /engineenv-field-total -->`
#       SS1's scope paragraph. Exactly one block, and inside it:
#       exactly one number, equal to the live field count; the live
#       record's FIRST and LAST field named in backticks and no other
#       live field name; and the one-row-per-field contract sentence.
#       "Exactly one number" is what keeps a hand-written
#       `State.hs:NNN` anchor out, so the anchors #1669 removed cannot
#       come back either.
#
# SS6.2's assignment-method sentence -- the second copy's old home --
# is governed WITHOUT markers, by structural position plus a content
# anchor: SS6.2's first numbered item must still be that sentence
# (`PROCEDURE_ITEM_ANCHOR`) and must state no number at all. Markers
# there would have to open the list item's content, which starts a
# Markdown HTML block and splits the item's rendering.
#
# Three things stop the governed prose from being hollowed out rather
# than edited: the marker pair is REQUIRED (deleting it is a
# violation), it must be SS1's first content and SS1's only number
# (so an unaudited paragraph cannot stand in front of the audited one,
# and no second count can hide behind it), and the whole document is
# swept for the one unambiguous reintroduction shape,
# `<n> EngineEnv field(s)`.
#
# Section references (`SS5`, `SS7.3` -- written with the section sign in
# the document) are excluded from both number scans. They are
# navigation, not counts, and SS1's contract sentence necessarily names
# SS5.
# The two sections whose prose these markers govern. A block found
# outside its own section is not governing that section's prose, which
# is the whole point of the check.
# SS1's heading has exactly one reader -- this owner -- so it stays
# here. SS6.2's is shared with the access-governance owner (which
# parses SS6.2's table) and therefore lives in
# tools/engine_env_capability_common.py; `section_bounds` takes the
# heading as an argument, so both uses read the one definition.
SECTION_1_HEADING = "## 1. Scope"

FIELD_TOTAL_OPEN = "<!-- engineenv-field-total -->"
FIELD_TOTAL_CLOSE = "<!-- /engineenv-field-total -->"
# SS6.2's assignment-method item 1 -- the sentence that used to carry
# the second copy of the total -- is bound by CONTENT, not by markers:
# it is identified by the phrase below and then required to state no
# number. Markers were tried first and rejected: an HTML comment cannot
# open a Markdown list item's content without starting an HTML block
# and splitting the item's rendering, and marking only part of the
# sentence would leave the rest free to state a total.
PROCEDURE_ITEM_ANCHOR = "`EngineEnv` field names from §5"

# Requirement 2 of issue #1669: the one-row-per-field contract is the
# useful half of SS1's sentence and is independent of the number, so it
# survives explicitly rather than by accident.
ONE_ROW_PHRASE = "exactly one row in §5"

_SECTION_REF_RE = re.compile(r"§\d+(?:\.\d+)*")
_INTEGER_RE = re.compile(r"\d+")


class MarkedSpan(NamedTuple):
    """One marker pair: its inner prose and where the whole block sits.

    `start`/`end` bracket the block INCLUDING both markers, which is
    what the section-containment check needs -- a pair whose opening
    marker is inside its section but whose closing marker is not has
    not kept its prose there.
    """
    body: str
    start: int
    end: int


def extract_marked_spans(text: str, open_marker: str, close_marker: str
                         ) -> tuple[list[MarkedSpan], list[str]]:
    """Return `(spans, violations)` for one marker pair.

    Deliberately literal string scanning, not a regex: the markers are
    fixed literals, and an unbalanced or nested pair must be reported
    as the malformed markup it is rather than silently matching some
    other pair's text.

    The inventory document is the only text this is applied to, so the
    diagnostics name it directly.
    """
    spans: list[MarkedSpan] = []
    violations: list[str] = []
    cursor = 0
    while True:
        start = text.find(open_marker, cursor)
        if start < 0:
            break
        body_start = start + len(open_marker)
        end = text.find(close_marker, body_start)
        if end < 0:
            violations.append(
                f"`{open_marker}` at offset {start} in "
                f"docs/{INVENTORY_PATH.name} is never closed by "
                f"`{close_marker}`")
            break
        nested = text.find(open_marker, body_start, end)
        if nested >= 0:
            violations.append(
                f"`{open_marker}` blocks are nested in "
                f"docs/{INVENTORY_PATH.name} (a second one opens at "
                f"offset {nested} before the first closes) -- the "
                f"governed prose must be one flat block")
        spans.append(MarkedSpan(text[body_start:end], start,
                                end + len(close_marker)))
        cursor = end + len(close_marker)
    return spans, violations


def _countable_numbers(body: str) -> list[str]:
    """Every decimal integer in `body` that is not a section reference."""
    return _INTEGER_RE.findall(_SECTION_REF_RE.sub("", body))


_FENCE_RE = re.compile(r"^(`{3,}|~{3,})")


def _fence_states(text: str) -> "list[bool]":
    """Per line, whether that line is INSIDE a fenced code block.

    A fence opens on a line of three or more backticks or tildes and
    closes on a later line of at least as many of the SAME character.
    The opening and closing fence lines themselves count as inside, so
    a heading can never be read out of fenced content.

    This exists because `section_bounds` decides where a section ends,
    and a fenced block containing a line like `## example` would
    otherwise end the section early -- while Markdown still renders
    everything after it inside the real section. That gap was a live
    bypass of the scope rules below: prose past the fake heading was
    outside the audit and inside the document.
    """
    inside: list[bool] = []
    open_char = ""
    open_len = 0
    for line in text.splitlines():
        stripped = line.strip()
        match = _FENCE_RE.match(stripped)
        if not open_char:
            if match:
                open_char = match.group(1)[0]
                open_len = len(match.group(1))
                inside.append(True)
                continue
            inside.append(False)
            continue
        inside.append(True)
        if (match and match.group(1)[0] == open_char
                and len(match.group(1)) >= open_len
                and not stripped[len(match.group(1)):].strip()):
            open_char = ""
            open_len = 0
    return inside


def section_bounds(text: str, heading: str,
                   stop_prefixes: tuple[str, ...]) -> tuple[int, int] | None:
    """Character bounds of one Markdown section's body, or `None` when
    the heading is absent.

    The body runs from just after the heading line to just before the
    next line whose stripped form starts with one of `stop_prefixes`
    (or to end of document). `"## "` does NOT match `"### "` -- the
    third character is a `#`, not the required space -- so a top-level
    section legitimately contains its own subsections.

    Lines inside a fenced code block are not headings, in either role:
    a fenced `## 1. Scope` does not start the section, and a fenced
    `## anything` does not end it.
    """
    fenced = _fence_states(text)
    start: int | None = None
    offset = 0
    for index, line in enumerate(text.splitlines(keepends=True)):
        stripped = line.strip()
        in_fence = fenced[index] if index < len(fenced) else False
        if start is None:
            if stripped == heading and not in_fence:
                start = offset + len(line)
        elif not in_fence and any(stripped.startswith(prefix)
                                  for prefix in stop_prefixes):
            return start, offset
        offset += len(line)
    if start is None:
        return None
    return start, offset


def _audit_scope_block_placement(inventory_text: str,
                                 spans: list[MarkedSpan]) -> list[str]:
    """SS1's marked block must BE the scope statement, not merely live
    in the same section as one.

    Section containment alone is not enough: the pair could be moved to
    a later paragraph of SS1 -- or into a fenced block -- while SS1's
    opening sentence went back to a hand-maintained total that nothing
    reads. Two rules pin it, and they are deliberately positional
    rather than semantic:

    (a) The pair is SS1's first content. Whatever a reader sees first
        under `## 1. Scope` is the audited paragraph.
    (b) The rest of SS1 states no number at all. Digits are permitted
        only in a SOURCE-LOCATION code span (`` `src/Engine/Core/
        State.hs:446` `` in the out-of-scope list), in a section
        reference (`SS5`), and in an issue reference (`#1669`). A bare
        `` `83` `` is none of those -- code font does not make a field
        total a citation -- so a second copy has nowhere in SS1 to
        live.
    """
    violations: list[str] = []
    doc = f"docs/{INVENTORY_PATH.name}"
    bounds = section_bounds(inventory_text, SECTION_1_HEADING, ("## ",))
    if bounds is None:
        return [f"{doc} has no `{SECTION_1_HEADING}` heading -- "
                f"`{FIELD_TOTAL_OPEN}` is anchored to that section, so it "
                f"cannot be renamed or removed without moving the marker "
                f"contract with it"]
    start, end = bounds
    for span in spans:
        if span.start < start or span.end > end:
            violations.append(
                f"{doc}'s `{FIELD_TOTAL_OPEN}` block (offsets "
                f"{span.start}-{span.end}) is not inside "
                f"`{SECTION_1_HEADING}` (offsets {start}-{end}) -- a "
                f"marker pair moved out of its section governs nothing, "
                f"leaving the scope statement free to drift again")
    if len(spans) == 1 and start <= spans[0].start <= end:
        leading = inventory_text[start:spans[0].start]
        if leading.strip():
            violations.append(
                f"{doc}'s `{FIELD_TOTAL_OPEN}` block is not the first "
                f"content of `{SECTION_1_HEADING}` -- {leading.strip()[:60]!r} "
                f"precedes it. The audited paragraph must be the scope "
                f"statement a reader meets first, or an unaudited one can "
                f"stand in front of it")

    remainder = inventory_text[start:end]
    for span in spans:
        if start <= span.start and span.end <= end:
            remainder = remainder.replace(
                inventory_text[span.start:span.end], "", 1)
    stray = _stray_numbers_outside_code(remainder)
    if stray:
        violations.append(
            f"{doc}'s `{SECTION_1_HEADING}` states number(s) "
            f"{', '.join(stray)} outside its `{FIELD_TOTAL_OPEN}` block. "
            f"SS1 states exactly one number, the field total, inside that "
            f"block; anything else there is a second hand-maintained count "
            f"waiting to drift. Section (SS5) and issue (#1669) references "
            f"are fine, and so is a source-location code span such as "
            f"`src/Engine/Core/State.hs:446` -- but a bare `83` is a field "
            f"total in code font, not a citation")
    return violations


# A backtick span whose digits are a SOURCE LOCATION -- a repository
# path, optionally with a line or line-range anchor. This is the only
# code span whose numbers are exempt from the no-stray-count rule.
#
# Exempting code spans wholesale was the third rereview's finding: it
# let `` `83` `` stand in the governed prose, which reads to a human as
# exactly the stale total this audit exists to remove. A span has to
# LOOK like a source reference to be excused, and a bare number does
# not.
_SOURCE_SPAN_RE = re.compile(
    r"^[A-Za-z0-9_./+\-]+\.(?:hs|lua|py|md|json|yaml|yml|cabal|sh)"
    r"(?::\d+(?:-\d+)?)?$")


def _stray_numbers_outside_code(text: str) -> list[str]:
    """Decimal integers in `text` that are neither a section reference,
    an issue reference, nor part of a source-location code span.

    A code span that is NOT a source location keeps its digits in the
    scan: `` `83` `` is a field total wearing a code font, not a
    citation.
    """
    def _strip_span(match: re.Match[str]) -> str:
        inner = match.group(1).strip()
        return "" if _SOURCE_SPAN_RE.match(inner) else match.group(1)

    without_code = re.sub(r"`([^`]*)`", _strip_span, text)
    without_refs = re.sub(r"#\d+", "", _SECTION_REF_RE.sub("", without_code))
    return _INTEGER_RE.findall(without_refs)


_NUMBERED_ITEM_RE = re.compile(r"^(\d+)\.[ \t]", re.MULTILINE)


def _first_numbered_item(body: str) -> tuple[int, int] | None:
    """`(content start, content end)` of the first `1. ` list item in
    `body`, the item running to the next top-level numbered marker or
    to the end of the text."""
    first = None
    for match in _NUMBERED_ITEM_RE.finditer(body):
        if first is None:
            if match.group(1) != "1":
                continue
            first = match
            continue
        return first.end(), match.start()
    if first is None:
        return None
    return first.end(), len(body)


def _audit_procedure_item_binding(inventory_text: str) -> list[str]:
    """SS6.2's assignment-method item 1 must still be that sentence,
    and must state no number.

    That item is where the second, drifted copy of the total lived.
    Section containment would not be enough even with markers: SS6.2
    legitimately carries many numbers (module tallies, epic ids, its
    own table), so the rule is pinned to the sentence itself -- found
    by structural position AND by its own wording, so that inserting a
    new item ahead of it does not quietly move the governed sentence
    out from under the check.
    """
    doc = f"docs/{INVENTORY_PATH.name}"
    bounds = section_bounds(inventory_text, SECTION_6_2_HEADING,
                            ("## ", "### "))
    if bounds is None:
        return [f"{doc} has no `{SECTION_6_2_HEADING}` heading -- its "
                f"assignment-method item 1 is audited for the field total "
                f"it used to repeat, so the section cannot be renamed or "
                f"removed without moving that rule with it"]
    start, end = bounds
    body = inventory_text[start:end]
    item = _first_numbered_item(body)
    if item is None:
        return [f"{doc}'s `{SECTION_6_2_HEADING}` has no numbered "
                f"assignment-method item -- item 1 is the audited "
                f"sentence, so it must still exist"]
    content = body[item[0]:item[1]]
    violations: list[str] = []
    if PROCEDURE_ITEM_ANCHOR not in content:
        violations.append(
            f"{doc}'s `{SECTION_6_2_HEADING}` item 1 no longer contains "
            f"{PROCEDURE_ITEM_ANCHOR!r} -- that sentence is the one "
            f"audited for the field total it used to repeat, so it may "
            f"not be reworded past recognition or displaced by a new "
            f"item 1. Item 1 currently reads {content.strip()[:80]!r}...")
    stray = _stray_numbers_outside_code(content)
    if stray:
        violations.append(
            f"{doc}'s `{SECTION_6_2_HEADING}` item 1 states number(s) "
            f"{', '.join(stray)} -- that sentence must state no field "
            f"total at all. SS1's marked block is the document's only "
            f"field count; a second copy here is the drift issue #1669 "
            f"removed")
    return violations


# The one reintroduction shape that is unambiguous enough to police
# across the WHOLE document rather than inside a marked block: a number
# directly qualifying `EngineEnv` fields, which is how both drifted
# copies were written ("exactly 83 fields" was SS1's, "one of the 83
# `EngineEnv` field names" was SS6.2's). Deliberately NOT the bare
# "<n> fields" shape -- SS5's capability groups and SS7's roadmap
# legitimately state their own record sizes ("21 fields", "a 14-field
# record"), and a rule that flagged those would be a rule maintainers
# route around.
_ENGINEENV_TOTAL_RE = re.compile(
    r"[`*]{0,2}\d+[`*]{0,2}\s*[`*]{0,2}EngineEnv[`*]{0,2}\s+fields?\b",
    re.IGNORECASE)


def _audit_no_stray_engineenv_total(inventory_text: str,
                                    spans: list[MarkedSpan]) -> list[str]:
    """No `<n> EngineEnv field(s)` phrase anywhere outside the marked
    block, whichever section it appears in."""
    outside = inventory_text
    for span in spans:
        outside = outside.replace(
            inventory_text[span.start:span.end], "", 1)
    found = [m.group(0) for m in _ENGINEENV_TOTAL_RE.finditer(outside)]
    if not found:
        return []
    return [f"docs/{INVENTORY_PATH.name} states {found} outside its "
            f"`{FIELD_TOTAL_OPEN}` block -- the field total is stated "
            f"once, in that block, and nowhere else in this document"]


def audit_field_total(live_fields: list[str], inventory_text: str
                      ) -> list[str]:
    """SS1's stated field total and field span must be the live ones,
    and SS6.2's procedure sentence must state no total at all.

    `live_fields` is the ORDERED field list `extract_record_fields`
    returns, so `[0]`/`[-1]` are the record's real first and last
    field -- which is what SS1's "`engineConfig` through
    `popupQueueRef`" span claim asserts.
    """
    violations: list[str] = []
    doc = f"docs/{INVENTORY_PATH.name}"

    bodies, marker_violations = extract_marked_spans(
        inventory_text, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    violations.extend(marker_violations)
    violations.extend(_audit_scope_block_placement(inventory_text, bodies))
    violations.extend(_audit_no_stray_engineenv_total(
        inventory_text, bodies))
    if not bodies:
        violations.append(
            f"{doc} SS1 has no `{FIELD_TOTAL_OPEN}` block -- the audited "
            f"field-total paragraph is missing, so nothing states the "
            f"live count and nothing can be checked against it")
    elif len(bodies) > 1:
        violations.append(
            f"{doc} has {len(bodies)} `{FIELD_TOTAL_OPEN}` blocks -- "
            f"exactly one paragraph may state the field total, or the "
            f"copies can disagree with each other again")
    else:
        body = bodies[0].body
        numbers = _countable_numbers(body)
        expected = str(len(live_fields))
        if not numbers:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block states no field "
                f"total -- it must state exactly one number, the live "
                f"count ({expected})")
        elif len(numbers) > 1:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block contains "
                f"{len(numbers)} numbers ({', '.join(numbers)}) -- it "
                f"may contain exactly one, the field total. A source "
                f"line number belongs nowhere in it: hand-written "
                f"anchors are what drifted before issue #1669")
        elif numbers[0] != expected:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block states "
                f"{numbers[0]} fields, but {ENGINE_ENV_FILE} declares "
                f"{expected} -- update the block (the total is stated "
                f"once, there, and nowhere else in the document)")

        live_set = set(live_fields)
        named = [t for t in BACKTICK_RE.findall(body) if t in live_set]
        span = [live_fields[0], live_fields[-1]]
        if named != span:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block names live "
                f"EngineEnv field(s) {named} -- it must name exactly the "
                f"record's first and last field, in order "
                f"(`{span[0]}` through `{span[1]}`), and no others")

        if ONE_ROW_PHRASE not in body:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block no longer states "
                f"the one-row-per-field contract ({ONE_ROW_PHRASE!r}) -- "
                f"that contract is the useful half of the sentence and is "
                f"independent of the number")

    violations.extend(_audit_procedure_item_binding(inventory_text))
    return violations
