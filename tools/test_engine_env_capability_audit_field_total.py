#!/usr/bin/env python3
"""The SS1 audited field-total prose contract of
engine_env_capability_field_total.py (issue #1669; extracted from
tools/test_engine_env_capability_audit.py by issue #2062).

Every rule `audit_field_total` adds is mutation-tested here in BOTH
directions: each rejects a crafted violating document built by
`_field_total_doc`, and the real inventory is accepted. Issue #1669
requirement 5 exists because hand-rolled prose validators have shipped
here that rejected nothing (#704, #1128, #1309), so "the real file
passes" on its own is not evidence that a rule is enforced. The cases
cover the marked block's existence and uniqueness, the live count and
first/last field span, the section and procedure anchors, duplicate or
stray totals, Markdown fence handling, and `section_bounds`. Two cases
read the real `docs/engineenv_capability_inventory.md` and say so:
`test_real_inventory_fenced_heading_escape_rejected` and
`test_field_total_against_the_real_repo`, the latter mutating a copy of
the real document six ways to prove the check reads THIS document's
block.

`test_field_total_synchronized_rows_alone_do_not_save_a_stale_block`
is the one case here that also drives the row audit, over the shared
synthetic record grown by a field -- which is why the record and the
inventory builders live in `test_engine_env_capability_audit_support`
rather than with the inventory owner.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore  # noqa: E402
    ENGINE_ENV_PATTERN, SECTION_6_2_HEADING,
)
from engine_env_capability_field_total import (  # type: ignore  # noqa: E402
    FIELD_TOTAL_CLOSE, FIELD_TOTAL_OPEN, ONE_ROW_PHRASE,
    PROCEDURE_ITEM_ANCHOR, SECTION_1_HEADING, audit_field_total,
    extract_marked_spans, section_bounds,
)
from engine_env_capability_inventory import audit_source  # type: ignore  # noqa: E402
from test_engine_env_capability_audit_support import (  # noqa: E402
    FIELD_THREE_ROW, FIELD_TWO_ROW, SYNTHETIC_ENGINE_ENV, expect,
    extract_record_fields, inventory_doc, real_engine_env_source,
    real_inventory_text,
)


# ----- SS1's audited field total and field span (issue #1669) ----------
#
# Every rule the new prose check adds is mutation-tested here in BOTH
# directions: each rejects a crafted violating document, and the REAL
# inventory is accepted. Issue #1669 requirement 5 exists because
# hand-rolled prose validators have shipped here that rejected nothing
# (#704, #1128, #1309), so "the real file passes" on its own is not
# evidence that a rule is enforced.

# Three fields whose FIRST and LAST are what SS1's span claim names.
_FT_LIVE = ["fieldOne", "fieldTwo", "fieldThree"]


def _field_total_doc(total_body: str | None = None,
                     procedure_item: str | None = None,
                     *, total_blocks: int = 1,
                     scope_prefix: str = "",
                     scope_suffix: str = "",
                     trailing_section: str = "") -> str:
    """A minimal document shaped like the real one: SS1 opening with the
    marked block, and SS6.2 whose first numbered item is the audited
    assignment-method sentence."""
    if total_body is None:
        total_body = ("\n\n`Fake.hs` declares it with exactly **3** fields, "
                      "`fieldOne` through `fieldThree`, and every one of them "
                      f"has {ONE_ROW_PHRASE} below.\n\n")
    if procedure_item is None:
        procedure_item = (f"For each module, scan its source for every "
                          f"occurrence of one of the "
                          f"{PROCEDURE_ITEM_ANCHOR}.")
    parts = [f"# Fake inventory\n\n{SECTION_1_HEADING}\n\n"]
    if scope_prefix:
        parts.append(f"{scope_prefix}\n\n")
    for _ in range(total_blocks):
        parts.append(f"{FIELD_TOTAL_OPEN}{total_body}{FIELD_TOTAL_CLOSE}\n\n")
    if scope_suffix:
        parts.append(f"{scope_suffix}\n\n")
    parts.append("## 6. Boundary\n\nprose\n\n"
                 f"{SECTION_6_2_HEADING}\n\nintro\n\n"
                 f"1. {procedure_item}\n"
                 "2. A later step that legitimately counts 4 modules.\n\n")
    if trailing_section:
        parts.append(f"{trailing_section}\n\n")
    return "".join(parts)


def test_field_total_clean_fixture_accepted():
    violations = audit_field_total(_FT_LIVE, _field_total_doc())
    expect(violations == [],
           f"a document whose marked block states the live count and the "
           f"real first/last field should have zero violations, got: "
           f"{violations}")


def test_field_total_stale_count_rejected_while_rows_stay_synchronized():
    """Issue #1669 requirement 4, in the shape the issue review pinned:
    the recurrence that ESCAPES today is a live field change whose SS5
    row was added correctly while the SS1 prose stayed stale. A wholly
    unamended document already fails `audit`'s missing-row check, so
    that case proves nothing about this one."""
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldOne` through `fieldFour`, and every one of them "
                    f"has {ONE_ROW_PHRASE} below.\n\n"))
    grown = _FT_LIVE + ["fieldFour"]
    violations = audit_field_total(grown, doc)
    expect(any("states 3 fields" in v and "declares 4" in v
               for v in violations),
           f"a fourth live field whose SS1 total was not amended must be "
           f"rejected, got: {violations}")


def test_field_total_synchronized_rows_alone_do_not_save_a_stale_block():
    """The same recurrence proven end-to-end against the ROW audit:
    `audit` (rows vs live field set) passes on a document whose SS5 rows
    were updated for a new field, and only `audit_field_total` catches
    the stale SS1 total. Without this pairing, the new check could be
    passing for a reason the old one already covered."""
    grown_env = SYNTHETIC_ENGINE_ENV.replace(
        "  } deriving (Eq)",
        "  , fieldFour  ∷ IORef Bool\n  } deriving (Eq)")
    field_four_row = (
        "| `fieldFour` | boot-process | `Boot` (`src/Fake/Init.hs:8`) "
        "| `Boot` (`src/Fake/Init.hs:8`) | `IORef Bool` "
        "| `src/Fake/Init.hs:8` | None | — |\n")
    rows_doc = inventory_doc(render_rows=FIELD_TWO_ROW + FIELD_THREE_ROW
                    + field_four_row)
    expect(audit_source(grown_env, rows_doc) == [],
           "the row audit must accept a new field whose SS5 row was added "
           "-- that is the case whose SS1 prose then goes stale unnoticed")
    live = extract_record_fields(grown_env, ENGINE_ENV_PATTERN)
    expect(len(live) == 4, f"fixture should now declare 4 fields, got {live}")
    stale = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldOne` through `fieldThree`, and every one of them "
                    f"has {ONE_ROW_PHRASE} below.\n\n"))
    expect(audit_field_total(live, stale) != [],
           "the field-total check must reject the SS1 block the row audit "
           "just accepted the document for")


def test_field_total_missing_block_rejected():
    doc = _field_total_doc(total_blocks=0)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("has no" in v and FIELD_TOTAL_OPEN in v for v in violations),
           f"deleting the marked block must be a violation, not a way to "
           f"turn the check off, got: {violations}")


def test_field_total_duplicate_block_rejected():
    doc = _field_total_doc(total_blocks=2)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("2 " in v and FIELD_TOTAL_OPEN in v for v in violations),
           f"two total blocks can disagree with each other, so a second "
           f"one must be rejected, got: {violations}")


def test_field_total_unclosed_block_rejected():
    doc = _field_total_doc().replace(FIELD_TOTAL_CLOSE, "", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("never closed" in v for v in violations),
           f"an unbalanced marker pair must be reported as malformed "
           f"markup, got: {violations}")


def test_field_total_reintroduced_line_anchor_rejected():
    """Issue #1669 requirement 3: the stale `State.hs:NNN` anchors are
    gone, and the one-number rule is what keeps them gone."""
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs:12` declares it with exactly **3** "
                    "fields, `fieldOne` through `fieldThree`, and every one "
                    f"of them has {ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("contains 2 numbers" in v for v in violations),
           f"a hand-written source line anchor inside the block is a "
           f"second number and must be rejected, got: {violations}")


def test_field_total_absent_number_rejected():
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares some fields, `fieldOne` through "
                    f"`fieldThree`, and every one of them has "
                    f"{ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("states no field total" in v for v in violations),
           f"a block that states no total at all is not a passing block, "
           f"got: {violations}")


def test_field_total_wrong_span_field_rejected():
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldOne` through `fieldTwo`, and every one of them "
                    f"has {ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("first and last field" in v for v in violations),
           f"a span claim naming a field that is not the record's last "
           f"must be rejected, got: {violations}")


def test_field_total_reversed_span_rejected():
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldThree` through `fieldOne`, and every one of them "
                    f"has {ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("first and last field" in v for v in violations),
           f"the span claim is ordered -- first THROUGH last -- so a "
           f"reversed pair must be rejected, got: {violations}")


def test_field_total_missing_one_row_contract_rejected():
    """Requirement 2: the one-row-per-field contract is the useful half
    of the sentence and must survive independently of the number."""
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldOne` through `fieldThree`.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("one-row-per-field contract" in v for v in violations),
           f"dropping the one-row-per-field contract must be rejected, "
           f"got: {violations}")


def test_field_total_section_references_are_not_counts():
    """The section sign is navigation, not a number: a block citing
    §5 and §7.3 alongside the one real total is still a
    one-number block."""
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` (see §7.3) declares it with exactly "
                    "**3** fields, `fieldOne` through `fieldThree`, and every "
                    f"one of them has {ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"section references must not be read as a second field total, "
           f"got: {violations}")


def test_procedure_item_reintroduced_total_rejected():
    """Requirement 1: SS1 and the SS6.2 procedure sentence must not be
    able to disagree. They cannot, because only SS1 may state a total --
    and this is the rule that keeps the second copy from coming back."""
    doc = _field_total_doc(
        procedure_item=(f"For each module, scan its source for every "
                        f"occurrence of one of the 83 "
                        f"{PROCEDURE_ITEM_ANCHOR}."))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("must state no field total" in v for v in violations),
           f"a field count reintroduced into SS6.2's procedure sentence "
           f"must be rejected, got: {violations}")


def test_procedure_item_agreeing_total_still_rejected():
    """Even a CORRECT second copy is rejected: two hand-maintained
    numbers is the defect, not one wrong one."""
    doc = _field_total_doc(
        procedure_item=(f"For each module, scan its source for every "
                        f"occurrence of one of the 3 "
                        f"{PROCEDURE_ITEM_ANCHOR}."))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("must state no field total" in v for v in violations),
           f"a second copy of the total is rejected even when it agrees "
           f"today, got: {violations}")


def test_procedure_item_total_in_its_tail_rejected():
    """The whole item is audited, not its opening clause: a total added
    after the recognizable phrase is still a second copy."""
    doc = _field_total_doc(
        procedure_item=(f"For each module, scan its source for every "
                        f"occurrence of one of the "
                        f"{PROCEDURE_ITEM_ANCHOR}, all 83 of them."))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("must state no field total" in v for v in violations),
           f"a total in the item's tail must be rejected, got: "
           f"{violations}")


def test_procedure_item_reworded_away_rejected():
    """The sentence is bound by its own wording as well as by position,
    so it cannot be rewritten past recognition (or displaced by a new
    item 1) while a stale total returns under a new phrasing."""
    doc = _field_total_doc(
        procedure_item="Tally each module's hits against the 83 names.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("no longer contains" in v for v in violations),
           f"an item 1 that is no longer the audited sentence must be "
           f"rejected, got: {violations}")


def test_procedure_item_displaced_by_a_new_first_item_rejected():
    doc = _field_total_doc()
    displaced = doc.replace(
        "1. For each module",
        "1. A newly inserted first step.\n2. For each module", 1)
    violations = audit_field_total(_FT_LIVE, displaced)
    expect(any("no longer contains" in v for v in violations),
           f"inserting a new item 1 must not move the audited sentence "
           f"out from under the check, got: {violations}")


def test_procedure_item_missing_section_rejected():
    doc = _field_total_doc().replace(SECTION_6_2_HEADING, "### 6.2 Gone", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("has no" in v and SECTION_6_2_HEADING in v
               for v in violations),
           f"renaming the procedure's section must be rejected, got: "
           f"{violations}")


def test_procedure_item_later_items_may_count_legitimately():
    """SS6.2's other steps legitimately state their own tallies; only
    the one audited sentence is held to no-number."""
    violations = audit_field_total(_FT_LIVE, _field_total_doc())
    expect(violations == [],
           f"item 2's legitimate '4 modules' count must not be flagged, "
           f"got: {violations}")


def test_scope_block_must_be_section_ones_first_content():
    """Same-section relocation: the pair still sits in SS1, but an
    unaudited paragraph -- carrying a stale total -- now stands in
    front of it."""
    doc = _field_total_doc(
        scope_prefix="The record has exactly 83 fields.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("not the first content" in v for v in violations),
           f"an unaudited paragraph placed ahead of the block must be "
           f"rejected, got: {violations}")


def test_scope_section_may_state_no_other_number():
    """The other half of same-section relocation: the pair stays first,
    and the stale copy is appended after it instead."""
    doc = _field_total_doc(
        scope_suffix="Historically the record had 83 of them.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "83" in v for v in violations),
           f"a second count later in SS1 must be rejected, got: "
           f"{violations}")


def test_scope_section_allows_code_spans_and_references():
    """The rule above must not flag what SS1 legitimately carries: a
    source reference inside a code span, a section reference, and an
    issue reference."""
    doc = _field_total_doc(
        scope_suffix="Out of scope: `EngineState` "
                     "(`src/Engine/Core/State.hs:446`), see §7.3 and "
                     "issue #1669.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"code spans and section/issue references must not read as "
           f"field counts, got: {violations}")


def test_scope_section_code_span_total_rejected():
    """Code font does not make a field total a citation: a bare
    `` `83` `` in SS1 is the stale count a reader sees, so it is
    rejected even though it sits inside backticks."""
    doc = _field_total_doc(scope_suffix="It has exactly `83` fields.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "83" in v for v in violations),
           f"a code-span field total in SS1 must be rejected, got: "
           f"{violations}")


def test_procedure_item_code_span_total_rejected():
    doc = _field_total_doc(
        procedure_item=(f"For each module, scan its source for every "
                        f"occurrence of one of the `83` "
                        f"{PROCEDURE_ITEM_ANCHOR}."))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("must state no field total" in v for v in violations),
           f"a code-span field total in the procedure sentence must be "
           f"rejected, got: {violations}")


def test_source_location_spans_stay_exempt():
    """The narrow exemption still has to cover what the document really
    carries: a path with a line anchor, and a path with a line range."""
    doc = _field_total_doc(
        scope_suffix="See `src/Engine/Core/State.hs:446` and "
                     "`docs/persistence_state_inventory.md:12-20`.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"source-location code spans must stay exempt, got: "
           f"{violations}")


def test_stray_engineenv_total_anywhere_rejected():
    """The document-wide backstop: the one unambiguous reintroduction
    shape is rejected wherever it appears, not only in the two governed
    places."""
    doc = _field_total_doc(
        trailing_section="## 9. Appendix\n\nA reminder that there are `83` "
                         "`EngineEnv` fields in total.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "EngineEnv" in v
               for v in violations),
           f"an EngineEnv field total stated in an unrelated section must "
           f"be rejected, got: {violations}")


def test_bare_field_counts_elsewhere_are_not_flagged():
    """The backstop is deliberately narrow: SS5's capability groups and
    SS7's roadmap state their own record sizes, and a rule that flagged
    those is a rule maintainers route around."""
    doc = _field_total_doc(
        trailing_section="## 9. Appendix\n\nThe render capability covers "
                         "21 fields; content-registries is a 7-field "
                         "record.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"a capability record's own field count must not be mistaken "
           f"for the EngineEnv total, got: {violations}")


def test_field_total_block_outside_section_one_rejected():
    """The escape the section binding closes: both marker pairs still
    exist, still well-formed and self-consistent, but they were lifted
    out of the prose they govern -- which is then free to carry a stale
    hand-maintained total again."""
    doc = _field_total_doc(trailing_section="## 9. Appendix")
    spans, _ = extract_marked_spans(doc, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    block = doc[spans[0].start:spans[0].end]
    moved = doc.replace(block, "It has exactly 83 fields.", 1) + block + "\n"
    violations = audit_field_total(_FT_LIVE, moved)
    expect(any("is not inside" in v and SECTION_1_HEADING in v
               for v in violations),
           f"a total block relocated out of the scope section must be "
           f"rejected, got: {violations}")


def test_field_total_renamed_section_heading_rejected():
    """Renaming the section is the other half of relocating the block:
    the pair stays put and the heading moves away from it."""
    doc = _field_total_doc().replace(SECTION_1_HEADING, "## 1. Purpose", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("has no" in v and SECTION_1_HEADING in v
               for v in violations),
           f"renaming the governed section must be rejected, got: "
           f"{violations}")


def test_fenced_heading_does_not_end_the_scope_section():
    """A fenced code block containing a heading-shaped line must not
    end SS1: Markdown renders everything after it inside SS1 still, so
    a stale total placed there would be inside the document and outside
    the audit."""
    doc = _field_total_doc(
        scope_suffix="```\n## example\n```\n\nIt has exactly 83 fields.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "83" in v for v in violations),
           f"prose hidden behind a fenced pseudo-heading must still be "
           f"audited, got: {violations}")


def test_fenced_heading_does_not_end_the_procedure_section():
    doc = _field_total_doc().replace(
        "1. For each module",
        "```\n### 6.9 not a heading\n```\n\n1. For each module", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"a fenced pseudo-heading before the procedure item must not "
           f"hide the item from the check, got: {violations}")


def test_fenced_scope_heading_does_not_start_the_section():
    """The same rule in the other role: a fenced `## 1. Scope` must not
    be mistaken for the section's start, which would put the real
    section's prose outside the audited range."""
    doc = _field_total_doc().replace(
        "# Fake inventory\n",
        f"# Fake inventory\n\n```\n{SECTION_1_HEADING}\n```\n", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"a fenced copy of the scope heading must be ignored, got: "
           f"{violations}")


def test_tilde_fences_and_longer_closers_are_handled():
    """Tilde fences count too, and a closing fence must be at least as
    long as its opener -- a shorter run inside the block does not end
    it."""
    doc = _field_total_doc(
        scope_suffix="~~~~\n## example\n~~~\nstill fenced\n~~~~\n\n"
                     "It has exactly 83 fields.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "83" in v for v in violations),
           f"tilde fences must be tracked with the same length rule, "
           f"got: {violations}")


def test_real_inventory_fenced_heading_escape_rejected():
    """The same escape, on the real document."""
    real = real_inventory_text()
    live = extract_record_fields(
        real_engine_env_source(),
        ENGINE_ENV_PATTERN)
    spans, _ = extract_marked_spans(real, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    escaped = (real[:spans[0].end]
               + "\n\n```\n## example\n```\n\nIt has exactly 83 fields.\n"
               + real[spans[0].end:])
    expect(any("outside its" in v for v in audit_field_total(live, escaped)),
           "a fenced pseudo-heading must not carve a stale total out of "
           "the real SS1")


def test_section_bounds_stops_at_the_next_peer_heading():
    doc = _field_total_doc()
    bounds = section_bounds(doc, SECTION_1_HEADING, ("## ",))
    expect(bounds is not None, "SS1's bounds must be found")
    start, end = bounds
    expect("## 6. Boundary" not in doc[start:end],
           "SS1's body must stop at the next top-level heading")
    expect(FIELD_TOTAL_OPEN in doc[start:end],
           "SS1's body must contain the total block it governs")


def test_section_bounds_keeps_subsections_inside_a_top_level_section():
    """`"## "` must not match `"### "` -- otherwise every top-level
    section would end at its own first subsection."""
    doc = "## 6. Boundary\n\nintro\n\n### 6.2 Sub\n\ntail\n\n## 7. Next\n"
    bounds = section_bounds(doc, "## 6. Boundary", ("## ",))
    expect(bounds is not None, "the section must be found")
    start, end = bounds
    expect("### 6.2 Sub" in doc[start:end] and "tail" in doc[start:end],
           f"a subsection must stay inside its parent section, got: "
           f"{doc[start:end]!r}")
    expect("## 7. Next" not in doc[start:end],
           "the next peer heading must end the section")


def test_field_total_against_the_real_repo():
    real_source = real_engine_env_source()
    real_inventory = real_inventory_text()
    live_fields = extract_record_fields(real_source, ENGINE_ENV_PATTERN)
    violations = audit_field_total(live_fields, real_inventory)
    expect(violations == [],
           f"the real inventory's SS1 block must state the real live "
           f"count and span, got: {violations}")
    spans, marker_violations = extract_marked_spans(
        real_inventory, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    expect(marker_violations == [] and len(spans) == 1,
           f"the real document must carry exactly one well-formed total "
           f"block, got {len(spans)} and {marker_violations}")
    body = spans[0].body
    stale_body = body.replace(str(len(live_fields)),
                              str(len(live_fields) - 1), 1)
    expect(stale_body != body,
           "the real block must actually contain the live count for this "
           "mutation to mean anything")
    stale = real_inventory.replace(body, stale_body, 1)
    expect(audit_field_total(live_fields, stale) != [],
           "the real inventory with its own block's total decremented by "
           "one must be rejected -- proving the check reads THIS "
           "document's block, not only synthetic fixtures")
    anchored_body = body.replace(
        "`src/Engine/Core/State.hs`", "`src/Engine/Core/State.hs:70`", 1)
    expect(anchored_body != body,
           "the real block must name the source file for this mutation to "
           "mean anything")
    anchored = real_inventory.replace(body, anchored_body, 1)
    expect(audit_field_total(live_fields, anchored) != [],
           "a hand-written source line anchor put back into the real "
           "block must be rejected (issue #1669 requirement 3)")

    # Relocation: lift the real pair out of SS1 and re-append it,
    # unchanged and self-consistent, at the very end of the document,
    # leaving a stale hand-maintained total behind in SS1's prose. Every
    # between-the-markers rule still passes on the moved block; only the
    # section binding catches it.
    whole_block = real_inventory[spans[0].start:spans[0].end]
    relocated = (real_inventory.replace(
        whole_block,
        "`src/Engine/Core/State.hs` declares exactly 83 fields.", 1)
        + "\n\n## 9. Appendix\n\n" + whole_block + "\n")
    expect(any("is not inside" in v for v in
               audit_field_total(live_fields, relocated)),
           "moving the real total block out of SS1 must be rejected, or "
           "SS1's prose could carry a stale total again with the markers "
           "parked somewhere inert")

    # Same-section relocation on the REAL document: the block stays in
    # SS1 but an unaudited paragraph carrying a stale total is placed
    # ahead of it.
    shadowed = real_inventory.replace(
        f"{SECTION_1_HEADING}\n",
        f"{SECTION_1_HEADING}\n\nThe record has exactly 83 fields.\n", 1)
    expect(any("not the first content" in v for v in
               audit_field_total(live_fields, shadowed)),
           "an unaudited scope paragraph placed ahead of the real block "
           "must be rejected")

    # The real SS6.2 procedure sentence, given its old total back --
    # plain, and again wearing code font.
    for restored in ("83", "`83`"):
        procedure = real_inventory.replace(
            f"one of the\n   {PROCEDURE_ITEM_ANCHOR}",
            f"one of the\n   {restored} {PROCEDURE_ITEM_ANCHOR}", 1)
        expect(procedure != real_inventory,
               f"the real procedure sentence must be found for the "
               f"{restored} mutation to mean anything")
        expect(any("must state no field total" in v or "outside its" in v
                   for v in audit_field_total(live_fields, procedure)),
               f"restoring the second copy of the total ({restored}) in "
               f"the real SS6.2 procedure sentence must be rejected")

    # And the real SS1, given a code-font copy after its block.
    spans2, _ = extract_marked_spans(
        real_inventory, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    tail = real_inventory[:spans2[0].end] + \
        "\n\nThe record has `83` fields.\n" + \
        real_inventory[spans2[0].end:]
    expect(any("outside its" in v for v in
               audit_field_total(live_fields, tail)),
           "a code-font second count after the real block must be "
           "rejected")


#: This owner's inventory, in the relative order these groups hold
#: within the aggregate's run sequence. `tools/test_engine_env_capability_audit.py`
#: composes that sequence from every owner's inventory; nothing here
#: decides when, or whether, it runs.
TESTS = (
    test_field_total_clean_fixture_accepted,
    test_field_total_stale_count_rejected_while_rows_stay_synchronized,
    test_field_total_synchronized_rows_alone_do_not_save_a_stale_block,
    test_field_total_missing_block_rejected,
    test_field_total_duplicate_block_rejected,
    test_field_total_unclosed_block_rejected,
    test_field_total_reintroduced_line_anchor_rejected,
    test_field_total_absent_number_rejected,
    test_field_total_wrong_span_field_rejected,
    test_field_total_reversed_span_rejected,
    test_field_total_missing_one_row_contract_rejected,
    test_field_total_section_references_are_not_counts,
    test_procedure_item_reintroduced_total_rejected,
    test_procedure_item_agreeing_total_still_rejected,
    test_procedure_item_total_in_its_tail_rejected,
    test_procedure_item_reworded_away_rejected,
    test_procedure_item_displaced_by_a_new_first_item_rejected,
    test_procedure_item_missing_section_rejected,
    test_procedure_item_later_items_may_count_legitimately,
    test_scope_block_must_be_section_ones_first_content,
    test_scope_section_may_state_no_other_number,
    test_scope_section_allows_code_spans_and_references,
    test_scope_section_code_span_total_rejected,
    test_procedure_item_code_span_total_rejected,
    test_source_location_spans_stay_exempt,
    test_stray_engineenv_total_anywhere_rejected,
    test_bare_field_counts_elsewhere_are_not_flagged,
    test_field_total_block_outside_section_one_rejected,
    test_field_total_renamed_section_heading_rejected,
    test_fenced_heading_does_not_end_the_scope_section,
    test_fenced_heading_does_not_end_the_procedure_section,
    test_fenced_scope_heading_does_not_start_the_section,
    test_tilde_fences_and_longer_closers_are_handled,
    test_real_inventory_fenced_heading_escape_rejected,
    test_section_bounds_stops_at_the_next_peer_heading,
    test_section_bounds_keeps_subsections_inside_a_top_level_section,
    test_field_total_against_the_real_repo,
)
