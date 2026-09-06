#!/usr/bin/env python3
"""§2.1's audited capability-record sizes (issue #2269).

§2.1's "Eight identifiers, fourteen record/view types" table names every
`Engine.Core.Capability.*` record and, in each cell, how many fields
that record carries. Those numbers were hand-maintained, and two of
them had drifted: the table said `WorldSimCapability` (9) and
`RenderHandoffCapability` (7) while the live declarations carried 11 and
10. Five fields had joined after #893/#894 landed -- `wsPlayerIntentGenRef`
(#913), `wsEnginePauseGenRef` (#1730), `rhStructureWallCatalogRef`
(#1712), `rhStructureArtCatalogRef` (#1842) and `rhSceneStatsRef`
(#1921) -- each legitimately, each classified in §5, and none of them
reflected here. By the time the issue was worked a third had drifted the
same way: `UnitCombatCapability` had gained `ucPathingConfigRef` and read
10 against a live 11.

Nothing in the field inventory was wrong. `audit`'s §5 row check compares
the live field SET with §5's row set, and `audit_capability_projection_
completeness` proves every projected field reaches a live accessor, so
both stayed green throughout: neither has ever looked at a number §2.1
displays in prose. That is the same gap issue #1669 closed for §1's field
total, and this owner closes it for §2.1's per-record sizes, the same
way: the numbers stay displayed, and are checked against the records they
name.

The rule follows #1584's -- a total a document displays is obtained
mechanically or is not displayed -- and #1669's marker discipline, for
the reason #1669 recorded: a prose validator that has to GUESS which
number in a table is a field count is the shape that burned review rounds
in #704/#1128/#1309. So the table is delimited, its record column is
LEXED as a Markdown column rather than pattern-matched out of running
text, and every count in it must be spelled one way.

The contract
------------
  `<!-- capability-record-counts -->` ... `<!-- /capability-record-counts -->`
      Exactly one block, inside §2.1, containing exactly one Markdown
      table with a `Record / view type(s)` column. In that column:

      * exactly one column is headed `Record / view type(s)`. Only one
        is read, so a second would be a column of the governed table
        displaying sizes nothing checks;
      * every capability record named in a backtick span states its
        size exactly once, as `` `XCapability` (<n> fields) ``;
      * `<n>` equals `capability_record_fields`' count for that record;
      * the records named are exactly the live ones -- every live record
        appears, and nothing that is not a live record does;
      * no other digit appears in the column. Section references (§5),
        issue references (#1669) and source locations
        (`src/Engine/Core/State.hs:446`) are excluded, since they are
        citations rather than counts; a bare number in prose is a second
        hand-maintained figure waiting to drift, which is how
        `EventsCapability`'s cell came to read "3 ... a 4th ... removed
        by #2285" and `ContentRegistriesViewCapability`'s to read
        "4 registries ... + `crvInfectionManagerRef` raw" -- two cells
        whose count a reader could not state unambiguously.

      The block holds that table and NOTHING else, and the table must
      RENDER as one. There is no Markdown parser available to this
      audit, so the contract is pinned to the narrowest arrangement
      that is unambiguously a rendered GFM table, and each rule names
      the construct it refuses:

      * every non-blank line between the markers begins with `|` in
        COLUMN ZERO. Four or more leading spaces make an indented code
        block, a `>` prefix a blockquote, a list marker a nested
        container -- none of which is §2.1 carrying a table;
      * exactly one contiguous run of such lines, so no second table
        can sit in the block at a column index nothing reads;
      * a `|---|` separator row under the header, with exactly as many
        cells as the header -- GFM recognizes a table only then, and
        renders the run as a paragraph of literal pipes otherwise;
      * every row drawn from ONE permitted character vocabulary
        (`_ROW_VOCABULARY`). This is a whitelist rather than a list of
        refused constructs, so raw HTML, links, images, footnotes,
        entities, backslash escapes, tabs, emphasis and non-ASCII
        homoglyphs are all excluded because the characters they need
        are absent -- including the ones nobody has thought of. The
        block-level rules keep the TABLE rendering; this keeps each
        CELL rendering as the text it is written as;
      * neither marker inside a fenced code block or an open
        `<pre>`/`<script>`/`<style>`/`<textarea>` HTML block -- the two
        constructs that carry following lines verbatim.
        `extract_marked_spans` refuses those, and the owner then also
        reports the block as missing.

      Rounds 1 through 4 of this change's review each found one
      instance of the same class -- a second piped table, a size in the
      block's prose, an emphasised size and a pipe-less table, an
      indented table -- which is why the contract is now stated as what
      is ACCEPTED rather than as a list of what is rejected.
      Only the one table's record column is compared with the live
      records, so anything else inside the markers is governed markup
      nothing verifies. Both review rounds of this change found an
      instance of that class (a second pipe-delimited table read at the
      first table's column index; a size written in the block's prose),
      which is why the rule is structural rather than another scan for
      the next arrangement of characters a reader would read as a
      count. Prose ABOUT the table belongs outside the markers, where
      the document-wide sweep below reaches it.

The other two columns are unconstrained: the identifier column is §2.1's
own vocabulary and the "Landed by" column is epic-step provenance
(`#893 (E5a)`), whose digits are neither counts nor citations this
scan could usefully separate.

Outside the block, the whole document is swept for a second copy of a
record's size written in a form that reads as this table's: the record
name -- backticked, emphasised, or bare -- followed by a parenthesised
number, or by a number and the word `field` reached across punctuation.
Prose that names a count WITHOUT attaching it to a record that way is
deliberately not matched: §3.1's "a strict subset -- the 16 fields of
..." and §7.4's "the nine world/sim fields" are legitimate descriptions
of what a record contains, and a rule that flagged those is a rule
maintainers route around.

Ordering
--------
This owner requires the live record set to be COMPLETE, so
`engine_env_capability_audit.py` runs it after
`audit_capability_projection_completeness`, whose
`undiscovered_capability_declarations` half is what makes discovery
fail-closed. A record whose declaration the parser cannot read would
otherwise be absent from the live set and so not required in the table
-- a silent hole exactly like the one #2059 closed. A field list this
owner cannot read is still reported here rather than counted as zero.

Not independently a gate: `python3 tools/engine_env_capability_audit.py`
remains the one command CI and tools/ci-local.sh run.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore
    BACKTICK_RE, INVENTORY_PATH, SEPARATOR_ROW_RE, extract_marked_spans,
    section_bounds, stray_numbers_outside_code,
)
from engine_env_capability_writers import (  # type: ignore
    capability_record_fields, discover_capability_records,
)

__all__ = [
    "RECORD_COLUMN_HEADER",
    "RECORD_COUNTS_CLOSE",
    "RECORD_COUNTS_OPEN",
    "SECTION_2_1_HEADING",
    "audit_record_counts",
    "live_record_sizes",
    "parse_record_column",
]

SECTION_2_1_HEADING = "### 2.1 Capability identifiers"
RECORD_COUNTS_OPEN = "<!-- capability-record-counts -->"
RECORD_COUNTS_CLOSE = "<!-- /capability-record-counts -->"
#: The header cell that identifies the audited column. Matched exactly,
#: so renaming the column is a violation rather than a way to move the
#: counts out from under this check.
RECORD_COLUMN_HEADER = "Record / view type(s)"

#: A capability record NAME as the document spells it. Anchored on an
#: uppercase initial so the projection functions the same cells name
#: (`toWorldSimCapability`) are not mistaken for records.
_RECORD_NAME_RE = re.compile(r"[A-Z][A-Za-z0-9_']*Capability")
#: The one permitted spelling of a size: the record's backtick span,
#: then its count in parentheses, then the word `field`/`fields`.
_COUNT_RE = re.compile(
    r"`(?P<record>[A-Z][A-Za-z0-9_']*Capability)`\s*"
    r"\(\s*(?P<count>\d+)\s+fields?\b")
#: A record name as a reader meets it, tolerant of the decoration
#: Markdown lets a writer wrap it in. Backticks are the convention, but
#: `**`XCapability`**` and `_XCapability_` render as the same name, and
#: a rule that only recognized the bare code span was a formatting
#: variant away from being bypassed (round-2 review). Same tolerance
#: the SS1 field-total sweep already applies to its own phrase.
_DECORATED_RECORD = r"[`*_]{0,3}[A-Z][A-Za-z0-9_']*Capability[`*_]{0,3}"
#: The COMPLETE character vocabulary a governed table row may use.
#:
#: A whitelist, deliberately, and it replaced a growing blocklist. Ten
#: rounds of review of this change each named one more construct whose
#: source differs from what a reader sees -- an `<!-- ... -->` comment
#: around a cell, a `<span>` hidden by CSS, `[](# "...")` putting the
#: count in a link title -- and each was rejected by adding a pattern
#: for it. That argument is only ever as good as the enumeration
#: behind it, which is the shape #1309 and #1463 burned review rounds
#: on here before.
#:
#: This inverts it. Only these characters may appear in a row, so
#: every construct that could hide a record or its count is excluded
#: because the characters it needs are absent, whether or not anyone
#: thought of it:
#:
#:   `<` `>`   raw HTML, autolinks
#:   `[` `]`   links, images, footnotes, reference links
#:   `&`       HTML entities (`&#87;orldSimCapability`)
#:   `\`       backslash escapes (an escaped `|` or backtick)
#:   tab       GFM expands it to four columns: an indented code block
#:   `*` `_`   emphasis, which decorates a name past the count pattern
#:   `~`       strikethrough, and fences
#:   `"`       link titles
#:   anything non-ASCII but the two below -- zero-width spaces and
#:             homoglyphs, which render as a name this audit never sees
#:
#: The real table uses exactly this set and nothing else. Widening it
#: is a deliberate edit here, and the cost of each addition is the
#: constructs it re-admits -- which is the point: the vocabulary is
#: the argument, so it is stated in one place and reviewed as one.
_ROW_VOCABULARY = " #(),-./:;`|'§—"
_FORBIDDEN_ROW_CHAR_RE = re.compile(
    r"[^0-9A-Za-z" + re.escape(_ROW_VOCABULARY) + r"]")
#: What a rejected character would most likely have been, so the
#: diagnostic names the construct rather than only the codepoint. A
#: character absent from this map is still rejected -- the vocabulary
#: decides, this only explains.
_EXCLUDED_CHAR_REASONS = {
    "<": "raw HTML or an autolink", ">": "raw HTML or an autolink",
    "[": "a link, image or footnote", "]": "a link, image or footnote",
    "&": "an HTML entity", "\\": "a backslash escape",
    "\t": "a tab, which GFM expands into an indented code block",
    "*": "emphasis", "_": "emphasis",
    "~": "strikethrough or a code fence", '"': "a link title",
}
#: The reintroduction shape swept for outside the block, in the two
#: orders a size is written in: parenthesised after the name
#: (`` `XCapability` (9 fields) ``, this table's own shape, and the
#: older `` (22, `MainRender`-only) `` shape it replaced), or the number
#: and the word `field` reached across punctuation (`` `XCapability` —
#: 9 fields ``).
#:
#: Deliberately NOT "a number anywhere near a record name": SS3.1's
#: "a strict subset -- the 16 fields of ..." and SS7.6's
#: "`ContentRegistriesCapability` over exactly the 7 fields" are
#: legitimate prose about what a record contains, and a rule that
#: flagged those is a rule maintainers route around. Both stay outside
#: these two shapes because the words between the name and the number
#: are letters.
_STRAY_COUNT_RE = re.compile(
    _DECORATED_RECORD + r"(?:"
    r"\s*\(\s*[`*_]{0,2}\d+"
    r"|[^A-Za-z0-9\n]{0,6}[`*_]{0,2}\d+[`*_]{0,2}\s*[`*_]{0,2}fields?\b"
    r")")

_DOC = f"docs/{INVENTORY_PATH.name}"


def live_record_sizes(sources: dict[str, str]
                      ) -> tuple[dict[str, int], list[str]]:
    """`({record: field count}, violations)` for every live capability
    record.

    A record whose field list cannot be read is a violation, never a
    record of size zero: an unreadable declaration that silently
    counted as empty would let the table state any number for it and
    pass. `discover_capability_records`' own completeness backstop runs
    earlier in the aggregate (see this module's docstring), so a record
    missing from `sources` entirely has already been reported by then.
    """
    sizes: dict[str, int] = {}
    violations: list[str] = []
    for entry in discover_capability_records(sources):
        if entry.record in sizes:
            violations.append(
                f"`{entry.record}` is declared by more than one module "
                f"(most recently `{entry.module}`, {entry.relpath}) -- "
                f"{_DOC} §2.1 states one size per record name, which two "
                f"declarations of the same name make ambiguous")
            continue
        try:
            sizes[entry.record] = len(
                capability_record_fields(sources[entry.relpath], entry.record))
        except ValueError as error:
            violations.append(
                f"`{entry.record}`'s field list cannot be read from "
                f"{entry.relpath} ({error}) -- §2.1's stated size cannot "
                f"be checked against a record this audit cannot count, and "
                f"an unreadable record is not a record of size 0")
    return sizes, violations


def _pipe_runs(block: str) -> list[list[str]]:
    """The block's maximal runs of consecutive `|`-leading lines.

    A Markdown table is exactly such a run: a blank line or a line of
    prose ends it. The `|` must be at COLUMN ZERO -- four or more
    leading spaces make the line an indented code block in GFM, which
    renders the pipes literally and leaves §2.1 with no table at all,
    and a `>` or list marker in front of it moves the table inside
    another container. Accepting a stripped line was the round-4
    escape. Grouping rather than filtering is what makes a
    SECOND table in the block visible. Collecting every pipe-leading
    line into one list instead -- the first shape of this parser --
    silently appended the second table's header and rows to the first
    table's, where they were read at the FIRST table's column index. A
    second three-column table whose columns are ordered differently
    then displayed a stale record size in the governed block, in the
    cell this owner never looked at, and the audit passed.
    """
    runs: list[list[str]] = []
    current: list[str] = []
    for line in block.splitlines():
        if line.startswith("|"):
            current.append(line.rstrip())
            continue
        if current:
            runs.append(current)
            current = []
    if current:
        runs.append(current)
    return runs


def parse_record_column(block: str) -> tuple[list[str], list[str]]:
    """`(cells, violations)` -- the record column of the block's one
    Markdown table.

    Lexes the table rather than scanning the block as running text: the
    audited column is found by its header, and every data row is split
    on `|` and indexed. A row with a different cell count, a missing
    header, or a second table in the block is reported instead of being
    read past, so the numbers this owner checks are the numbers a reader
    sees in that column and not whatever a regex happened to reach.
    """
    violations: list[str] = []
    runs = _pipe_runs(block)
    if not runs:
        return [], [f"{_DOC} §2.1's `{RECORD_COUNTS_OPEN}` block contains "
                    f"no Markdown table -- the audited record/view table "
                    f"must be inside it"]
    if len(runs) > 1:
        return [], [f"{_DOC} §2.1's `{RECORD_COUNTS_OPEN}` block contains "
                    f"{len(runs)} Markdown tables -- exactly one may be "
                    f"inside it. Only the audited column of ONE table is "
                    f"read, so a second table is a place inside the "
                    f"governed block for a record size that nothing "
                    f"checks"]
    rows = runs[0]

    def cells(row: str) -> list[str]:
        return [cell.strip() for cell in row.strip("|").split("|")]

    def is_separator(row: str) -> bool:
        parsed = cells(row)
        return bool(parsed) and all(SEPARATOR_ROW_RE.fullmatch(cell)
                                    for cell in parsed)

    # A run of pipe-leading lines is only a RENDERED table when a
    # separator row follows the header; without one GFM renders the
    # whole run as a paragraph of literal pipes. Requiring it is what
    # stops the audited column being read out of markup no reader sees
    # as a table.
    if len(rows) < 2 or not is_separator(rows[1]):
        return [], [f"{_DOC} §2.1's `{RECORD_COUNTS_OPEN}` block has no "
                    f"`|---|` separator row under its header -- without "
                    f"one the lines are not a rendered Markdown table, "
                    f"and the audited column would be read out of markup "
                    f"no reader sees as a table"]

    header = cells(rows[0])
    # GFM recognizes a table only when the delimiter row has EXACTLY as
    # many cells as the header; a mismatch renders the whole run as a
    # paragraph. Checking the delimiter's shape without its width was
    # the round-8 escape: `|---|---|` under a three-column header
    # satisfied every count rule over a table no reader sees.
    delimiter = cells(rows[1])
    if len(delimiter) != len(header):
        return [], [f"{_DOC} §2.1's `{RECORD_COUNTS_OPEN}` block has a "
                    f"separator row of {len(delimiter)} cell(s) under a "
                    f"header of {len(header)} -- GFM recognizes a table "
                    f"only when the two match, so this run renders as a "
                    f"paragraph of literal pipes rather than as §2.1's "
                    f"table"]
    matching = [position for position, cell in enumerate(header)
                if cell == RECORD_COLUMN_HEADER]
    if not matching:
        return [], [f"{_DOC} §2.1's `{RECORD_COUNTS_OPEN}` block has no "
                    f"`{RECORD_COLUMN_HEADER}` column (header row: "
                    f"{header}) -- that column is what carries the audited "
                    f"sizes, so it may not be renamed or removed"]
    if len(matching) > 1:
        return [], [f"{_DOC} §2.1's `{RECORD_COUNTS_OPEN}` block has "
                    f"{len(matching)} `{RECORD_COLUMN_HEADER}` columns "
                    f"(at positions {matching}) -- exactly one column "
                    f"carries the audited sizes. With two, only the "
                    f"first is read, so the second is a column of the "
                    f"governed table displaying record sizes that "
                    f"nothing checks"]
    index = matching[0]
    width = len(header)

    column: list[str] = []
    for number, row in enumerate(rows[2:], start=3):
        row_cells = cells(row)
        if is_separator(row):
            continue
        if len(row_cells) != width:
            violations.append(
                f"{_DOC} §2.1's record table row {number} has "
                f"{len(row_cells)} cell(s) against the header's {width} -- "
                f"the audited column is read by position, so a ragged row "
                f"would move it")
            continue
        forbidden = _FORBIDDEN_ROW_CHAR_RE.search(row)
        if forbidden is not None:
            character = forbidden.group(0)
            reason = _EXCLUDED_CHAR_REASONS.get(character)
            because = (f", which is {reason}" if reason else
                       ", which is outside the permitted vocabulary")
            violations.append(
                f"{_DOC} §2.1's record table row {number} uses "
                f"{character!r} (U+{ord(character):04X}){because}. A "
                f"governed row is plain text: only the characters in "
                f"`_ROW_VOCABULARY` may appear, so no construct whose "
                f"source differs from what a reader sees can carry a "
                f"record or its count past this audit while the "
                f"rendered cell shows something else. Row: "
                f"{row[:70]!r}")
            continue
        column.append(row_cells[index])
    if not column:
        violations.append(
            f"{_DOC} §2.1's `{RECORD_COUNTS_OPEN}` block states no record "
            f"rows at all -- emptying the table is not a way to satisfy "
            f"this check")
    return column, violations


def _audit_block_placement(inventory_text: str, spans: list) -> list[str]:
    """The marked pair must sit inside §2.1.

    A pair parked in another section governs whatever text moved with
    it, while §2.1's real table goes back to hand-maintained numbers --
    the placement failure #1669's own §1 rule exists to refuse.
    """
    bounds = section_bounds(inventory_text, SECTION_2_1_HEADING,
                            ("## ", "### "))
    if bounds is None:
        return [f"{_DOC} has no `{SECTION_2_1_HEADING}` heading -- "
                f"`{RECORD_COUNTS_OPEN}` is anchored to that section, so "
                f"it cannot be renamed or removed without moving the "
                f"marker contract with it"]
    start, end = bounds
    return [f"{_DOC}'s `{RECORD_COUNTS_OPEN}` block (offsets "
            f"{span.start}-{span.end}) is not inside "
            f"`{SECTION_2_1_HEADING}` (offsets {start}-{end}) -- a marker "
            f"pair moved out of its section governs nothing, leaving the "
            f"record table free to drift again"
            for span in spans if span.start < start or span.end > end]


def _audit_stray_counts(inventory_text: str, spans: list) -> list[str]:
    """No `` `XCapability` (<n> `` phrase anywhere but the block's table.

    Two directions, because "stated once" has two ways to fail and the
    audited column only reads one of them:

    * outside the marked block, anywhere in the document -- a second
      copy in prose that reads exactly like the table's;
    * inside the block but OUTSIDE its table -- prose between the
      marker and the table, which is inside the governed region and so
      exempt from the first sweep, but is not a table cell and so never
      reaches `_audit_column_counts` either. That gap is the same one
      the round-1 review found for a second TABLE, one step over.
    """
    violations: list[str] = []
    outside = inventory_text
    for span in spans:
        outside = outside.replace(
            inventory_text[span.start:span.end], "", 1)
    found = [match.group(0) for match in _STRAY_COUNT_RE.finditer(outside)]
    if found:
        violations.append(
            f"{_DOC} states {found} outside its `{RECORD_COUNTS_OPEN}` "
            f"block -- a record's size is stated once, in that block's "
            f"table, and nowhere else in this document")
    violations.extend(_audit_block_holds_only_its_table(spans))
    return violations


def _audit_block_holds_only_its_table(spans: list) -> list[str]:
    """Every non-blank line inside a marked block is a table row.

    The structural rule, rather than a widening scan for the shapes a
    smuggled size can take. Only the table's record column is compared
    with the live records, so ANY other content in the block is a
    region of the governed markup that nothing verifies -- prose
    (`Historically `XCapability` (9 fields).`), a GFM table written
    without leading pipes, raw HTML. Both review rounds found an
    instance of that class, one table-shaped and one prose-shaped; this
    closes the class instead of the next instance, and it is decidable
    by looking at each line rather than by guessing which arrangement
    of characters a reader would read as a count.
    """
    violations: list[str] = []
    for span in spans:
        stray = [line for line in span.body.splitlines()
                 if line.strip() and not line.startswith("|")]
        if stray:
            violations.append(
                f"{_DOC}'s `{RECORD_COUNTS_OPEN}` block holds "
                f"{len(stray)} line(s) that are not column-zero table "
                f"rows, the first being {stray[0][:70]!r} -- the block "
                f"holds its one table and nothing else, every row "
                f"starting with `|` in column zero. Only that table's "
                f"record column is checked against the live records, so "
                f"any other content inside the markers is a displayed "
                f"figure nothing verifies, and an indented row is an "
                f"indented code block rather than a table. Prose about "
                f"the table goes outside the markers")
    return violations


def _audit_column_counts(column: list[str], sizes: dict[str, int]
                         ) -> list[str]:
    """Every count in the record column, against the live records."""
    violations: list[str] = []
    stated: dict[str, int] = {}
    for cell in column:
        counted: set[str] = set()
        for match in _COUNT_RE.finditer(cell):
            record = match.group("record")
            counted.add(record)
            if record in stated:
                violations.append(
                    f"{_DOC} §2.1 states a size for `{record}` more than "
                    f"once -- two copies can disagree, so each record is "
                    f"counted exactly once")
                continue
            stated[record] = int(match.group("count"))
        for name in BACKTICK_RE.findall(cell):
            if _RECORD_NAME_RE.fullmatch(name) and name not in counted:
                violations.append(
                    f"{_DOC} §2.1 names `{name}` in the record column "
                    f"without stating its size -- every record the column "
                    f"names states one, as `` `{name}` (<n> fields) ``, or "
                    f"a record can lose its audited count by being "
                    f"reworded rather than corrected")

        remainder = _COUNT_RE.sub(lambda m: f"`{m.group('record')}`", cell)
        stray = stray_numbers_outside_code(remainder)
        if stray:
            violations.append(
                f"{_DOC} §2.1's record column states number(s) "
                f"{', '.join(stray)} that are not a record size: "
                f"{cell.strip()[:90]!r}. The column's only numbers are the "
                f"per-record counts; anything else there is a second "
                f"hand-maintained figure waiting to drift. Section (§5), "
                f"issue (#1669) and source-location references are fine")

    for record in sorted(set(sizes) - set(stated)):
        violations.append(
            f"{_DOC} §2.1's record table states no size for the live "
            f"record `{record}` ({sizes[record]} fields) -- every live "
            f"capability record appears in that table with its count, so "
            f"a new record cannot land unlisted")
    for record in sorted(set(stated) - set(sizes)):
        violations.append(
            f"{_DOC} §2.1's record table states a size for `{record}`, "
            f"which no `Engine.Core.Capability.*` module declares -- the "
            f"row outlived its record")
    for record in sorted(set(stated) & set(sizes)):
        if stated[record] != sizes[record]:
            violations.append(
                f"{_DOC} §2.1 states `{record}` carries {stated[record]} "
                f"field(s), but its declaration carries {sizes[record]} -- "
                f"update the table (§5 remains the field-by-field "
                f"authority; this table states each record's size)")
    return violations


def audit_record_counts(sources: dict[str, str], inventory_text: str
                        ) -> list[str]:
    """§2.1's stated capability-record sizes must be the live ones.

    `sources` is the one production-tree map the aggregate reads;
    `inventory_text` the one inventory document it reads. Nothing here
    walks the tree or opens a file.
    """
    violations: list[str] = []
    spans, marker_violations = extract_marked_spans(
        inventory_text, RECORD_COUNTS_OPEN, RECORD_COUNTS_CLOSE)
    violations.extend(marker_violations)
    violations.extend(_audit_block_placement(inventory_text, spans))
    violations.extend(_audit_stray_counts(inventory_text, spans))

    if not spans:
        violations.append(
            f"{_DOC} §2.1 has no `{RECORD_COUNTS_OPEN}` block -- the "
            f"audited record/view table is missing, so nothing states the "
            f"live record sizes and nothing can be checked against them")
        return violations
    if len(spans) > 1:
        violations.append(
            f"{_DOC} has {len(spans)} `{RECORD_COUNTS_OPEN}` blocks -- "
            f"exactly one table may state the record sizes, or the copies "
            f"can disagree with each other again")
        return violations

    sizes, size_violations = live_record_sizes(sources)
    violations.extend(size_violations)
    column, column_violations = parse_record_column(spans[0].body)
    violations.extend(column_violations)
    if column:
        violations.extend(_audit_column_counts(column, sizes))
    return violations
