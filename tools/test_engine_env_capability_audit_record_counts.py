#!/usr/bin/env python3
"""The §2.1 audited capability-record sizes of
engine_env_capability_record_counts.py (issue #2269).

Every rule `audit_record_counts` adds is mutation-tested here in BOTH
directions: each rejects a crafted violating document built by
`_counts_doc` over the synthetic capability tree `_SOURCES`, and the
real `docs/engineenv_capability_inventory.md` is accepted. Issue #2269
requirement 5 asks specifically that a STALE count and a MISSING count
each be proven to fail, and this repository's reason for asking is on
the record: hand-rolled prose validators have shipped here that rejected
nothing (#704, #1128, #1309), so "the real file passes" is not on its
own evidence that a rule is enforced.

The synthetic tree is three `Engine.Core.Capability.*` modules of two,
three and one field. Nothing here reads or writes the real inventory
except the two real-repository cases, which say so in their names and
mutate a COPY of the document text.

`test_record_counts_row_and_total_audits_do_not_catch_a_stale_size` is
the case that makes the new check load-bearing rather than merely
green: it drives the §5 row audit and the §1 field-total audit over the
same drift and shows both accept it.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore  # noqa: E402
    ENGINE_ENV_PATTERN, scan_production_sources, REPO_ROOT,
)
from engine_env_capability_field_total import (  # type: ignore  # noqa: E402
    FIELD_TOTAL_CLOSE, FIELD_TOTAL_OPEN, audit_field_total,
)
from engine_env_capability_inventory import audit_source  # type: ignore  # noqa: E402
from engine_env_capability_record_counts import (  # type: ignore  # noqa: E402
    RECORD_COLUMN_HEADER, RECORD_COUNTS_CLOSE, RECORD_COUNTS_OPEN,
    SECTION_2_1_HEADING, audit_record_counts, live_record_sizes,
    parse_record_column,
)
from test_engine_env_capability_audit_support import (  # noqa: E402
    FIELD_THREE_ROW, FIELD_TWO_ROW, SYNTHETIC_ENGINE_ENV, expect,
    extract_record_fields, inventory_doc, real_engine_env_source,
    real_inventory_text,
)


# ----- §2.1's audited capability-record sizes (issue #2269) -------------
#
# Three synthetic capability modules, so the fixtures exercise a
# multi-record row (the shape every real split identifier has), a
# single-record row, and a record the table can be made to forget.

def _module(record: str, fields: list[str]) -> str:
    body = "\n  , ".join(f"{name} ∷ IORef Int" for name in fields)
    return (f"module Engine.Core.Capability.{record[:-len('Capability')]} "
            f"where\n\n"
            f"data {record} = {record}\n"
            f"  {{ {body}\n"
            f"  }}\n")


_SOURCES = {
    "src/Engine/Core/Capability/Alpha.hs": _module(
        "AlphaCapability", ["acOne", "acTwo"]),
    "src/Engine/Core/Capability/Beta.hs": _module(
        "BetaCapability", ["bcOne", "bcTwo", "bcThree"]),
    "src/Engine/Core/Capability/Gamma.hs": _module(
        "GammaCapability", ["gcOne"]),
}

#: The clean record column: Alpha and Beta share a row, Gamma has its own.
_CLEAN_ROWS = (
    "| `alpha-beta` | `Engine.Core.Capability.Alpha` — `AlphaCapability` "
    "(2 fields); `Engine.Core.Capability.Beta` — `BetaCapability` "
    "(3 fields, the worker-safe view) | #1 (E1) / #2 (E2) |\n"
    "| `gamma` | `Engine.Core.Capability.Gamma` — `GammaCapability` "
    "(1 field) | #3 (E3) |\n"
)


def _counts_doc(rows: str = _CLEAN_ROWS, *,
                blocks: int = 1,
                header: str = f"| Identifier | {RECORD_COLUMN_HEADER} "
                              f"| Landed by |",
                section_heading: str = SECTION_2_1_HEADING,
                prefix: str = "",
                trailing: str = "") -> str:
    """A minimal document shaped like the real one: §2.1 carrying the
    marked record table, and a later section that does not."""
    table = f"{header}\n|---|---|---|\n{rows}"
    parts = [f"# Fake inventory\n\n## 2. Vocabulary\n\n"
             f"{section_heading}\n\nintro prose\n\n"]
    if prefix:
        parts.append(f"{prefix}\n\n")
    for _ in range(blocks):
        parts.append(f"{RECORD_COUNTS_OPEN}\n{table}{RECORD_COUNTS_CLOSE}\n\n")
    parts.append("## 3. Something else\n\nprose that mentions "
                 "`AlphaCapability` without sizing it.\n\n")
    if trailing:
        parts.append(f"{trailing}\n\n")
    return "".join(parts)


def test_record_counts_clean_fixture_accepted():
    violations = audit_record_counts(_SOURCES, _counts_doc())
    expect(violations == [],
           f"a table whose three sizes match the three synthetic records "
           f"should have zero violations, got: {violations}")


def test_record_counts_stale_size_rejected():
    """Requirement 5's first half, in the shape §2.1 actually drifted:
    a record gains a field and the table keeps its old number."""
    grown = dict(_SOURCES)
    grown["src/Engine/Core/Capability/Alpha.hs"] = _module(
        "AlphaCapability", ["acOne", "acTwo", "acThree"])
    violations = audit_record_counts(grown, _counts_doc())
    expect(any("`AlphaCapability` carries 2 field(s)" in v
               and "carries 3" in v for v in violations),
           f"a third live field whose §2.1 size was not amended must be "
           f"rejected, got: {violations}")


def test_record_counts_missing_size_rejected():
    """Requirement 5's second half: a record the column NAMES but no
    longer sizes. Deleting the number is the obvious way to make a
    stale one stop failing, so it must fail on its own."""
    rows = _CLEAN_ROWS.replace("`AlphaCapability` (2 fields)",
                               "`AlphaCapability` (the world/sim half)")
    violations = audit_record_counts(_SOURCES, _counts_doc(rows))
    expect(any("names `AlphaCapability` in the record column without "
               "stating its size" in v for v in violations),
           f"removing a record's count must be a violation, not a way to "
           f"turn the check off, got: {violations}")


def test_record_counts_missing_row_rejected():
    """A live record with no row at all -- the shape a NEW capability
    record lands in. The name-without-a-size rule cannot see this one,
    so the live-set comparison is what has to."""
    rows = _CLEAN_ROWS.split("| `gamma` |")[0]
    violations = audit_record_counts(_SOURCES, _counts_doc(rows))
    expect(any("states no size for the live record `GammaCapability`" in v
               for v in violations),
           f"a live record the table never lists must be rejected, got: "
           f"{violations}")


def test_record_counts_dead_record_row_rejected():
    rows = _CLEAN_ROWS + (
        "| `delta` | `Engine.Core.Capability.Delta` — `DeltaCapability` "
        "(7 fields) | #4 (E4) |\n")
    violations = audit_record_counts(_SOURCES, _counts_doc(rows))
    expect(any("states a size for `DeltaCapability`, which no" in v
               for v in violations),
           f"a row whose record no longer exists must be rejected, got: "
           f"{violations}")


def test_record_counts_duplicate_size_rejected():
    rows = _CLEAN_ROWS + (
        "| `alpha-again` | `Engine.Core.Capability.Alpha` — "
        "`AlphaCapability` (2 fields) | #1 (E1) |\n")
    violations = audit_record_counts(_SOURCES, _counts_doc(rows))
    expect(any("states a size for `AlphaCapability` more than once" in v
               for v in violations),
           f"two rows sizing one record can disagree, so the second must "
           f"be rejected even while it agrees, got: {violations}")


def test_record_counts_missing_block_rejected():
    violations = audit_record_counts(_SOURCES, _counts_doc(blocks=0))
    expect(any("has no" in v and RECORD_COUNTS_OPEN in v
               for v in violations),
           f"deleting the marked block must be a violation, not a way to "
           f"turn the check off, got: {violations}")


def test_record_counts_duplicate_block_rejected():
    violations = audit_record_counts(_SOURCES, _counts_doc(blocks=2))
    expect(any("2 " in v and RECORD_COUNTS_OPEN in v for v in violations),
           f"two record tables can disagree with each other, so a second "
           f"one must be rejected, got: {violations}")


def test_record_counts_unclosed_block_rejected():
    doc = _counts_doc().replace(RECORD_COUNTS_CLOSE, "", 1)
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("never closed" in v for v in violations),
           f"an unbalanced marker pair must be reported as malformed "
           f"markup, got: {violations}")


def test_record_counts_block_outside_section_two_one_rejected():
    doc = _counts_doc()
    block_start = doc.index(RECORD_COUNTS_OPEN)
    block_end = doc.index(RECORD_COUNTS_CLOSE) + len(RECORD_COUNTS_CLOSE)
    block = doc[block_start:block_end]
    moved = doc[:block_start] + doc[block_end:] + f"\n\n{block}\n"
    violations = audit_record_counts(_SOURCES, moved)
    expect(any("is not inside" in v and SECTION_2_1_HEADING in v
               for v in violations),
           f"a marker pair parked outside §2.1 governs nothing, so it "
           f"must be rejected, got: {violations}")


def test_record_counts_renamed_section_heading_rejected():
    doc = _counts_doc(section_heading="### 2.1 Capabilities")
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("has no" in v and SECTION_2_1_HEADING in v
               for v in violations),
           f"§2.1 anchors the marker contract, so renaming its heading "
           f"without moving the contract must be rejected, got: "
           f"{violations}")


def test_record_counts_renamed_column_rejected():
    doc = _counts_doc(header="| Identifier | Records | Landed by |")
    violations = audit_record_counts(_SOURCES, doc)
    expect(any(RECORD_COLUMN_HEADER in v and "no" in v for v in violations),
           f"the audited column is found by its header, so renaming it "
           f"must be rejected rather than silently skipping the check, "
           f"got: {violations}")


def test_record_counts_empty_table_rejected():
    violations = audit_record_counts(_SOURCES, _counts_doc(rows=""))
    expect(any("states no record rows at all" in v for v in violations),
           f"emptying the table must be rejected, got: {violations}")


def test_record_counts_block_without_a_table_rejected():
    doc = (f"# Fake inventory\n\n## 2. Vocabulary\n\n{SECTION_2_1_HEADING}"
           f"\n\n{RECORD_COUNTS_OPEN}\njust prose, no table\n"
           f"{RECORD_COUNTS_CLOSE}\n\n## 3. Else\n\nprose\n")
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("contains\nno Markdown table" in v.replace("  ", " ")
               or "no Markdown table" in v for v in violations),
           f"a marked block with no table must be rejected, got: "
           f"{violations}")


def test_record_counts_second_table_in_the_block_rejected():
    """The round-1 review's finding: a second table inside the marked
    block. Its rows used to be appended to the first table's and read
    at the FIRST table's column index, so a stale size sitting in a
    differently-ordered second table was displayed in the governed
    block and checked by nothing."""
    smuggled = (
        "\nHistorical, for reference:\n\n"
        f"| Identifier | Landed by | {RECORD_COLUMN_HEADER} |\n"
        "|---|---|---|\n"
        "| `alpha-beta` | epic E-one | `Engine.Core.Capability.Alpha` — "
        "`AlphaCapability` (9 fields) |\n")
    doc = _counts_doc().replace(RECORD_COUNTS_CLOSE,
                                smuggled + RECORD_COUNTS_CLOSE, 1)
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("Markdown tables -- exactly one" in v
               or "are not column-zero table rows" in v for v in violations),
           f"a second table inside the marked block must be rejected, "
           f"got: {violations}")


def test_record_counts_second_table_smuggling_a_stale_size_rejected():
    """The same hole against the REAL document and the REAL records: a
    second table displaying `WorldSimCapability` (9 fields) -- the very
    figure issue #2269 removed -- inside the marked block. Nothing else
    in this owner catches it: the audited column of the first table
    still reads 11, and the outside-the-block sweep does not reach
    inside the block."""
    sources = scan_production_sources(REPO_ROOT)
    document = real_inventory_text()
    smuggled = document.replace(
        RECORD_COUNTS_CLOSE,
        "\nHistorical, for reference:\n\n"
        f"| Identifier | Landed by | {RECORD_COLUMN_HEADER} |\n"
        "|---|---|---|\n"
        "| `world-sim-render-handoff` | epic E-five-a | "
        "`Engine.Core.Capability.WorldSim` — `WorldSimCapability` "
        "(9 fields) |\n\n" + RECORD_COUNTS_CLOSE, 1)
    expect(smuggled != document, "the fixture must actually change the "
                                 "real document")
    expect(audit_record_counts(sources, smuggled) != [],
           "a stale record size displayed in a second table inside the "
           "real marked block must be rejected")


def test_record_counts_size_in_the_blocks_prose_rejected():
    """The round-1 finding one step over: a size in prose INSIDE the
    marked block. It is exempt from the outside-the-block sweep because
    it is inside the governed region, and it never reaches the column
    check because it is not a table cell."""
    doc = _counts_doc().replace(
        RECORD_COUNTS_CLOSE,
        "\nHistorically `AlphaCapability` (9 fields).\n\n"
        + RECORD_COUNTS_CLOSE, 1)
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("are not column-zero table rows" in v for v in violations),
           f"a record size in the block's own prose must be rejected, "
           f"got: {violations}")


def test_record_counts_emphasised_size_in_the_block_rejected():
    """Round 2's first variant: the same smuggled size wrapped in
    Markdown emphasis. `**`XCapability`**` renders as the same name, so
    a rule that only recognized the bare code span was one formatting
    choice away from being bypassed."""
    doc = _counts_doc().replace(
        RECORD_COUNTS_CLOSE,
        "\nHistorical **`AlphaCapability`** (9 fields).\n\n"
        + RECORD_COUNTS_CLOSE, 1)
    violations = audit_record_counts(_SOURCES, doc)
    expect(violations != [],
           "an emphasised record size inside the block must be rejected")


def test_record_counts_pipeless_table_in_the_block_rejected():
    """Round 2's second variant: a valid GFM table written WITHOUT
    leading pipes, which `_pipe_runs` does not see as a table at all.
    The structural rule reaches it because its lines are not table rows,
    which is why that rule is structural rather than another scan."""
    doc = _counts_doc().replace(
        RECORD_COUNTS_CLOSE,
        "\nIdentifier | Landed by | Record / view type(s)\n"
        "---|---|---\n"
        "`alpha-beta` | epic E-one | **`AlphaCapability`** (9 fields)\n\n"
        + RECORD_COUNTS_CLOSE, 1)
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("are not column-zero table rows" in v for v in violations),
           f"a pipe-less GFM table inside the block must be rejected, "
           f"got: {violations}")


def test_record_counts_decorated_size_outside_the_block_rejected():
    """The document-wide sweep tolerates the same decoration, in both
    orders a size is written: parenthesised after the name, and the
    number plus the word `field` reached across punctuation."""
    for trailing in ("Historical **`AlphaCapability`** (9 fields).",
                     "Historical _AlphaCapability_ — 9 fields.",
                     "Record | Size\n---|---\n"
                     "**`AlphaCapability`** (9 fields) | x"):
        violations = audit_record_counts(
            _SOURCES, _counts_doc(trailing=trailing))
        expect(any("outside its" in v and RECORD_COUNTS_OPEN in v
                   for v in violations),
               f"a decorated second copy of a size must be rejected: "
               f"{trailing!r} gave {violations}")


def test_record_counts_prose_about_record_contents_is_not_a_size():
    """The other direction of the widened sweep. §3.1's "a strict subset
    -- the 16 fields of ..." and §7.6's "`XCapability` over exactly the
    7 fields" are legitimate descriptions of what a record contains; a
    rule that flagged them is a rule maintainers route around."""
    for trailing in (
            "`AlphaCapability`'s worker-safe view is a strict subset — "
            "the 2 fields it keeps are read-only.",
            "`Engine.Core.Capability.Alpha` exports `AlphaCapability` "
            "over exactly the 2 fields E1 found."):
        violations = audit_record_counts(
            _SOURCES, _counts_doc(trailing=trailing))
        expect(violations == [],
               f"prose describing a record's contents must be accepted: "
               f"{trailing!r} gave {violations}")


def test_record_counts_fenced_block_rejected():
    """Round 3's finding: wrapping the marker pair and its table in a
    ```` ```markdown ```` fence. The markers still parse, but the fenced
    content renders as an EXAMPLE -- §2.1 would carry no capability
    table at all while every rule that reads the returned span still
    passed."""
    doc = _counts_doc()
    doc = doc.replace(RECORD_COUNTS_OPEN, "```markdown\n" + RECORD_COUNTS_OPEN, 1)
    doc = doc.replace(RECORD_COUNTS_CLOSE, RECORD_COUNTS_CLOSE + "\n```", 1)
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("inside a fenced code block" in v for v in violations),
           f"a marker pair moved into a fence governs nothing and must "
           f"be rejected, got: {violations}")
    expect(any("has no" in v and RECORD_COUNTS_OPEN in v
               for v in violations),
           f"a fenced pair must also leave §2.1 reported as having no "
           f"audited table, got: {violations}")


def test_record_counts_missing_separator_row_rejected():
    """A run of pipe-leading lines is only a RENDERED table when a
    separator row follows the header; GFM renders it as a paragraph of
    literal pipes otherwise."""
    doc = _counts_doc().replace("|---|---|---|\n", "", 1)
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("separator row" in v for v in violations),
           f"a pipe run with no separator row is not a table and must "
           f"be rejected, got: {violations}")


def test_record_counts_fenced_real_block_rejected():
    """The same fence escape against the real document."""
    sources = scan_production_sources(REPO_ROOT)
    document = real_inventory_text()
    fenced = document.replace(
        RECORD_COUNTS_OPEN, "```markdown\n" + RECORD_COUNTS_OPEN, 1)
    fenced = fenced.replace(
        RECORD_COUNTS_CLOSE, RECORD_COUNTS_CLOSE + "\n```", 1)
    expect(fenced != document, "the fixture must change the real document")
    expect(any("inside a fenced code block" in v
               for v in audit_record_counts(sources, fenced)),
           "fencing the real §2.1 block must be rejected")


def test_field_total_block_in_a_fence_is_also_rejected():
    """`extract_marked_spans` is shared, so the same escape was open on
    §1's field-total block (#1669). Fixing it at the primitive fixes
    both; this pins the other owner's half so the shared change is not
    an untested behaviour change."""
    live = extract_record_fields(real_engine_env_source(),
                                 ENGINE_ENV_PATTERN)
    document = real_inventory_text()
    fenced = document.replace(
        FIELD_TOTAL_OPEN, "```markdown\n" + FIELD_TOTAL_OPEN, 1)
    fenced = fenced.replace(
        FIELD_TOTAL_CLOSE, FIELD_TOTAL_CLOSE + "\n```", 1)
    expect(fenced != document, "the fixture must change the real document")
    expect(audit_field_total(live, document) == [],
           "the unfenced real §1 block must still pass")
    expect(any("inside a fenced code block" in v
               for v in audit_field_total(live, fenced)),
           "fencing the real §1 field-total block must be rejected too")


def test_record_counts_indented_table_rejected():
    """Round 4's finding: indenting every table row by four spaces.
    GFM renders that as an indented code block -- literal pipes, no
    table -- yet the stripped rows used to parse and match the live
    counts, so §2.1 carried no rendered table and the audit passed."""
    doc = _counts_doc()
    indented = "\n".join(
        "    " + line if line.startswith("|") else line
        for line in doc.splitlines()) + "\n"
    expect(indented != doc, "the fixture must actually indent the table")
    violations = audit_record_counts(_SOURCES, indented)
    expect(any("column-zero table rows" in v for v in violations),
           f"an indented table is an indented code block, not a table, "
           f"and must be rejected, got: {violations}")


def test_record_counts_blockquoted_table_rejected():
    """The same construct family: a `>` prefix moves the table inside a
    blockquote rather than leaving it as §2.1's own content. The
    column-zero rule refuses the family, not just the one member."""
    doc = _counts_doc()
    quoted = "\n".join(
        "> " + line if line.startswith("|") else line
        for line in doc.splitlines()) + "\n"
    violations = audit_record_counts(_SOURCES, quoted)
    expect(any("column-zero table rows" in v for v in violations),
           f"a blockquoted table must be rejected, got: {violations}")


def test_record_counts_indented_real_table_rejected():
    """The indentation escape against the real document."""
    sources = scan_production_sources(REPO_ROOT)
    document = real_inventory_text()
    start = document.index(RECORD_COUNTS_OPEN)
    end = document.index(RECORD_COUNTS_CLOSE)
    indented = document[:start] + "\n".join(
        "    " + line if line.startswith("|") else line
        for line in document[start:end].splitlines()) + document[end:]
    expect(indented != document, "the fixture must change the real document")
    expect(any("column-zero table rows" in v
               for v in audit_record_counts(sources, indented)),
           "indenting the real §2.1 table must be rejected")


def test_record_counts_html_verbatim_block_rejected():
    """The other construct that carries following lines verbatim: an
    open `<pre>` HTML block. CommonMark carries it across blank lines
    until its closing tag, so it swallows the markers and their table
    exactly as a fence does."""
    doc = _counts_doc().replace(
        RECORD_COUNTS_OPEN, "<pre>\n" + RECORD_COUNTS_OPEN, 1).replace(
        RECORD_COUNTS_CLOSE, RECORD_COUNTS_CLOSE + "\n</pre>", 1)
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("fenced code block" in v for v in violations),
           f"markers inside an open <pre> block must be rejected, got: "
           f"{violations}")


def test_parse_record_column_refuses_indented_rows():
    """`parse_record_column`'s own half of the column-zero rule, pinned
    directly. The block-content rule rejects an indented block first in
    an end-to-end run, which would leave this function free to go back
    to stripping -- and it is public, so its contract is its own."""
    indented = ("    | Identifier | " + RECORD_COLUMN_HEADER + " | Landed by |\n"
                "    |---|---|---|\n"
                "    | `gamma` | `Engine.Core.Capability.Gamma` — "
                "`GammaCapability` (1 field) | #3 (E3) |\n")
    column, violations = parse_record_column(indented)
    expect(column == [], f"an indented run must yield no audited cells, "
                         f"got: {column}")
    expect(any("no Markdown table" in v for v in violations),
           f"an indented run is an indented code block, so this parser "
           f"must report NO TABLE rather than a malformed one -- "
           f"anything else means it read the rows and objected to their "
           f"shape, got: {violations}")


def test_record_counts_raw_html_block_rejected():
    """Round 5's finding: a CommonMark type-6 raw HTML block. `<div>`
    with no blank line before the marker encloses the markers and the
    table; Markdown inside a raw HTML block is not parsed, so §2.1
    carries no rendered table while the markers and pipe rows are still
    found as plain text. Type 1 (`<pre>`) was already refused; this is
    the family that ends at a blank line rather than at a closing tag.
    """
    for tag in ("div", "section", "table"):
        # No blank line between the tag and the marker: a type-6 block
        # ends at the first blank line, so an adjacent tag is what
        # actually encloses the markers and the table.
        doc = _counts_doc().replace(
            RECORD_COUNTS_OPEN, f"<{tag}>\n" + RECORD_COUNTS_OPEN, 1).replace(
            RECORD_COUNTS_CLOSE, RECORD_COUNTS_CLOSE + f"\n</{tag}>", 1)
        violations = audit_record_counts(_SOURCES, doc)
        expect(any("fenced code block" in v for v in violations),
               f"markers inside a <{tag}> raw HTML block must be "
               f"rejected, got: {violations}")


def test_record_counts_raw_html_real_block_rejected():
    """The same escape against the real document."""
    sources = scan_production_sources(REPO_ROOT)
    document = real_inventory_text()
    wrapped = document.replace(
        RECORD_COUNTS_OPEN, "<div>\n" + RECORD_COUNTS_OPEN, 1)
    wrapped = wrapped.replace(
        RECORD_COUNTS_CLOSE, RECORD_COUNTS_CLOSE + "\n</div>", 1)
    expect(wrapped != document, "the fixture must change the real document")
    expect(any("fenced code block" in v
               for v in audit_record_counts(sources, wrapped)),
           "wrapping the real §2.1 block in a <div> must be rejected")


def test_record_counts_marker_comments_do_not_open_an_html_block():
    """The other direction, and the reason the rule excludes `<!`: the
    markers ARE raw HTML. Read as type-6 openers they would swallow
    their own table and fail the real document, so the type-2 comment
    form is excluded and the clean fixture must still pass."""
    expect(audit_record_counts(_SOURCES, _counts_doc()) == [],
           "the marker comments must not be read as opening a raw HTML "
           "block over their own table")


def test_record_counts_ragged_row_rejected():
    rows = _CLEAN_ROWS + (
        "| `delta` | `Engine.Core.Capability.Delta` — `DeltaCapability` "
        "(7 fields) |\n")
    violations = audit_record_counts(_SOURCES, _counts_doc(rows))
    expect(any("cell(s) against the header's" in v for v in violations),
           f"the audited column is read by position, so a ragged row must "
           f"be rejected rather than read at the wrong index, got: "
           f"{violations}")


def test_record_counts_stray_number_in_the_column_rejected():
    """The `EventsCapability` cell's real shape before this change: one
    stated count plus a loose "a 4th" that reads as a second one."""
    rows = _CLEAN_ROWS.replace(
        "`AlphaCapability` (2 fields)",
        "`AlphaCapability` (2 fields; a 3rd was removed by #2285)")
    violations = audit_record_counts(_SOURCES, _counts_doc(rows))
    expect(any("that are not a record size" in v for v in violations),
           f"a second hand-maintained figure in the record column must be "
           f"rejected, got: {violations}")


def test_record_counts_column_references_are_not_counts():
    """Section, issue and source-location references stay legal in the
    audited column: they are navigation and provenance, not counts, and
    a rule that flagged them is a rule maintainers route around."""
    rows = _CLEAN_ROWS.replace(
        "`AlphaCapability` (2 fields)",
        "`AlphaCapability` (2 fields, see §7.4 and #893, "
        "`src/Engine/Core/Capability/Alpha.hs:75-147`)")
    violations = audit_record_counts(_SOURCES, _counts_doc(rows))
    expect(violations == [],
           f"§7.4, #893 and a source-location span are citations, not "
           f"counts, and must be accepted, got: {violations}")


def test_record_counts_projection_names_are_not_records():
    """`toAlphaCapability` is a projection function, not a record, so
    naming one in the column must not demand a size for it."""
    rows = _CLEAN_ROWS.replace(
        "`AlphaCapability` (2 fields)",
        "`AlphaCapability` (2 fields, built by `toAlphaCapability`)")
    violations = audit_record_counts(_SOURCES, _counts_doc(rows))
    expect(violations == [],
           f"a projection function name must not be read as a record "
           f"needing a count, got: {violations}")


def test_record_counts_stray_size_elsewhere_rejected():
    doc = _counts_doc(trailing="Elsewhere: `BetaCapability` (3 fields).")
    violations = audit_record_counts(_SOURCES, doc)
    expect(any("outside its" in v and RECORD_COUNTS_OPEN in v
               for v in violations),
           f"a second copy of a record's size anywhere in the document "
           f"must be rejected, got: {violations}")


def test_record_counts_historical_prose_counts_are_not_flagged():
    """§6.2 and §7.4 legitimately describe what #893 and #894 landed
    ("the nine world/sim fields"). The document-wide sweep matches only
    a size ATTACHED to a record in this table's shape, so that prose
    stays legal -- a rule that flagged it is one maintainers route
    around."""
    doc = _counts_doc(
        trailing="Since #893 the nine world/sim fields reach "
                 "`AlphaCapability`; #894 moved the other 7 to "
                 "`BetaCapability`.")
    violations = audit_record_counts(_SOURCES, doc)
    expect(violations == [],
           f"historical prose that states a count without attaching it to "
           f"a record in the table's shape must be accepted, got: "
           f"{violations}")


def test_record_counts_unreadable_record_is_not_size_zero():
    """A declaration whose record block cannot be read is a violation,
    never a record of size 0 -- otherwise the table could state any
    number for it and pass."""
    broken = dict(_SOURCES)
    broken["src/Engine/Core/Capability/Gamma.hs"] = (
        "module Engine.Core.Capability.Gamma where\n\n"
        "data GammaCapability = GammaCapability Int\n")
    sizes, violations = live_record_sizes(broken)
    expect("GammaCapability" not in sizes,
           f"an unreadable record must not enter the size map, got: {sizes}")
    expect(any("field list cannot be read" in v for v in violations),
           f"an unreadable record declaration must be reported, got: "
           f"{violations}")
    expect(audit_record_counts(broken, _counts_doc()) != [],
           "the audit must fail on a record it cannot count, whatever "
           "the table says about it")


def test_record_counts_duplicate_declaration_rejected():
    doubled = dict(_SOURCES)
    doubled["src/Engine/Core/Capability/AlphaAgain.hs"] = _module(
        "AlphaCapability", ["acOne", "acTwo"]).replace(
        "Engine.Core.Capability.Alpha ", "Engine.Core.Capability.AlphaAgain ")
    sizes, violations = live_record_sizes(doubled)
    expect(any("declared by more than one module" in v for v in violations),
           f"one size per record name is ambiguous when two modules "
           f"declare the name, got: {violations}")
    expect("AlphaCapability" in sizes,
           "the first declaration still supplies a size, so the rest of "
           "the report stays useful")


def test_parse_record_column_reads_the_named_column():
    """The column is located by header, not by index: moving it must
    move what the audit reads."""
    block = (f"| {RECORD_COLUMN_HEADER} | Identifier | Landed by |\n"
             f"|---|---|---|\n"
             f"| `Engine.Core.Capability.Gamma` — `GammaCapability` "
             f"(1 field) | `gamma` | #3 (E3) |\n")
    column, violations = parse_record_column(block)
    expect(violations == [], f"a well-formed table must parse, got: "
                             f"{violations}")
    expect(len(column) == 1 and "`GammaCapability` (1 field)" in column[0],
           f"the reordered table's record column must still be the one "
           f"read, got: {column}")


def test_record_counts_row_and_total_audits_do_not_catch_a_stale_size():
    """The drift proven end-to-end against the checks that were already
    there: the §5 row audit and the §1 field-total audit both accept a
    repository whose §2.1 size went stale, so this owner is not merely
    passing for a reason another group already covered."""
    grown_env = SYNTHETIC_ENGINE_ENV.replace(
        "  } deriving (Eq)",
        "  , fieldFour  ∷ IORef Bool\n  } deriving (Eq)")
    field_four_row = (
        "| `fieldFour` | boot-process | `Boot` (`src/Fake/Init.hs:8`) "
        "| `Boot` (`src/Fake/Init.hs:8`) | `IORef Bool` "
        "| `src/Fake/Init.hs:8` | None | — |\n")
    rows_doc = inventory_doc(
        render_rows=FIELD_TWO_ROW + FIELD_THREE_ROW + field_four_row)
    expect(audit_source(grown_env, rows_doc) == [],
           "the §5 row audit must accept a document whose rows track the "
           "live fields -- that is the state a stale §2.1 size hides in")
    live = extract_record_fields(grown_env, ENGINE_ENV_PATTERN)
    expect(audit_field_total(live, rows_doc) != [],
           "the §1 field-total check reads §1, not §2.1's record table")
    grown = dict(_SOURCES)
    grown["src/Engine/Core/Capability/Alpha.hs"] = _module(
        "AlphaCapability", ["acOne", "acTwo", "acThree"])
    expect(audit_record_counts(grown, _counts_doc()) != [],
           "only this owner rejects the §2.1 size the other two never "
           "look at")


def test_record_counts_against_the_real_repo():
    """The live `docs/engineenv_capability_inventory.md` and the live
    `Engine.Core.Capability.*` tree, mutated four ways on a COPY to
    prove the check reads THIS document's block and THESE records."""
    sources = scan_production_sources(REPO_ROOT)
    document = real_inventory_text()
    expect(audit_record_counts(sources, document) == [],
           f"the real inventory's §2.1 record table must state every live "
           f"record's real size, got: "
           f"{audit_record_counts(sources, document)}")

    sizes, size_violations = live_record_sizes(sources)
    expect(size_violations == [],
           f"every live capability record's field list must be readable, "
           f"got: {size_violations}")
    expect(len(sizes) == 14 and sum(sizes.values()) == 115,
           f"the live tree should be the 14 records and 115 projected "
           f"fields the aggregate reports, got {len(sizes)} record(s) "
           f"totalling {sum(sizes.values())}")

    stale = document.replace("`WorldSimCapability` (11 fields",
                             "`WorldSimCapability` (9 fields", 1)
    expect(stale != document and audit_record_counts(sources, stale) != [],
           "restoring the stale WorldSim size issue #2269 removed must "
           "fail against the real records")
    dropped = document.replace("`RenderHandoffCapability` (10 fields, ",
                               "`RenderHandoffCapability` (", 1)
    expect(dropped != document and audit_record_counts(sources, dropped) != [],
           "removing a real record's count must fail")
    unmarked = document.replace(RECORD_COUNTS_OPEN, "", 1)
    expect(unmarked != document
           and audit_record_counts(sources, unmarked) != [],
           "deleting the real opening marker must fail")
    moved = document.replace(f"| Identifier | {RECORD_COLUMN_HEADER} |",
                             "| Identifier | Records |", 1)
    expect(moved != document and audit_record_counts(sources, moved) != [],
           "renaming the real audited column must fail")


#: This owner's ordered inventory. The aggregate composes it; nothing
#: here decides when, or whether, it runs.
TESTS = (
    test_record_counts_clean_fixture_accepted,
    test_record_counts_stale_size_rejected,
    test_record_counts_missing_size_rejected,
    test_record_counts_missing_row_rejected,
    test_record_counts_dead_record_row_rejected,
    test_record_counts_duplicate_size_rejected,
    test_record_counts_missing_block_rejected,
    test_record_counts_duplicate_block_rejected,
    test_record_counts_unclosed_block_rejected,
    test_record_counts_block_outside_section_two_one_rejected,
    test_record_counts_renamed_section_heading_rejected,
    test_record_counts_renamed_column_rejected,
    test_record_counts_empty_table_rejected,
    test_record_counts_block_without_a_table_rejected,
    test_record_counts_second_table_in_the_block_rejected,
    test_record_counts_second_table_smuggling_a_stale_size_rejected,
    test_record_counts_size_in_the_blocks_prose_rejected,
    test_record_counts_emphasised_size_in_the_block_rejected,
    test_record_counts_pipeless_table_in_the_block_rejected,
    test_record_counts_decorated_size_outside_the_block_rejected,
    test_record_counts_prose_about_record_contents_is_not_a_size,
    test_record_counts_fenced_block_rejected,
    test_record_counts_missing_separator_row_rejected,
    test_record_counts_fenced_real_block_rejected,
    test_field_total_block_in_a_fence_is_also_rejected,
    test_record_counts_indented_table_rejected,
    test_record_counts_blockquoted_table_rejected,
    test_record_counts_indented_real_table_rejected,
    test_record_counts_html_verbatim_block_rejected,
    test_record_counts_raw_html_block_rejected,
    test_record_counts_raw_html_real_block_rejected,
    test_record_counts_marker_comments_do_not_open_an_html_block,
    test_record_counts_ragged_row_rejected,
    test_record_counts_stray_number_in_the_column_rejected,
    test_record_counts_column_references_are_not_counts,
    test_record_counts_projection_names_are_not_records,
    test_record_counts_stray_size_elsewhere_rejected,
    test_record_counts_historical_prose_counts_are_not_flagged,
    test_record_counts_unreadable_record_is_not_size_zero,
    test_record_counts_duplicate_declaration_rejected,
    test_parse_record_column_reads_the_named_column,
    test_parse_record_column_refuses_indented_rows,
    test_record_counts_row_and_total_audits_do_not_catch_a_stale_size,
    test_record_counts_against_the_real_repo,
)
