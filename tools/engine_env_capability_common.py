#!/usr/bin/env python3
"""The substrate every owner of the EngineEnv capability audit reads
(issue #2036; widened by issue #2064).

`tools/engine_env_capability_audit.py` is the aggregate CI gate. It
holds no contract implementation of its own: since #2064 each contract
has its own owner, and the gate loads the repository inputs once,
composes the owners in a fixed order and reports.

  `engine_env_capability_inventory.py`   the SS5 inventory-row contract
      (issue #876);
  `engine_env_capability_field_total.py` the SS1 audited field-total
      and field-span prose (issue #1669);
  `engine_env_capability_access.py`      the SS6 full-access ratchet
      and the SS6.1 permanent-boundary comparison (issues #889, #899);
  `engine_env_capability_boundaries.py`  the SS3 main-render and SS7.3
      LuaThread structural boundaries (issues #891, #892);
  `engine_env_capability_saveload.py`    the E8 save-load projection
      correspondence (issue #899);
  `engine_env_capability_writers.py`     the SS5 writing-module scanner
      (issue #1892, extracted by #2036).

Those owners share a small set of inputs and helpers, and #2036 moved
every one of them HERE -- #2064 adding the ones a second owner started
needing when the aggregate was split -- so each exists exactly once and
no owner needs to import another. (An owner may still have an interior:
`engine_env_capability_writers.py` is a facade over four
implementation owners of its own since issue #2230, which import each
other one way and read this module directly, exactly as the facade
did. Nothing here is duplicated into them.) The shared substrate:

  * the repository anchors -- `REPO_ROOT`, the inventory doc, the
    `EngineEnv` declaration file and pattern -- and the live-field
    derivation itself (`extract_record_fields`, imported from its one
    owner, tools/persistence_inventory_audit_haskell.py, so this audit and the
    persistence-inventory audit can never drift onto two notions of
    "the live EngineEnv field set");
  * `scan_production_sources`, the ONE production-tree walk both
    halves consume (the aggregate scans the tree once and hands the
    same `{relpath: text}` map to every check);
  * SS6.1's checked-in permanent full-access set (`PERMANENT_DEFINER`
    + `PERMANENT_IMPORTERS`), which the ratchet and boundary checks
    compare against the live importers and the writer scan exempts
    (design decision D-4);
  * the Haskell source helpers -- comment stripping, top-level import
    chunking, import-head resolution (`imports_module`), path-to-module
    naming -- that every source-level check is built on;
  * the policy-free inventory-document primitives -- `_is_placeholder`,
    `BACKTICK_RE`, `SEPARATOR_ROW_RE` and SS6.2's heading -- that more
    than one document-reading owner would otherwise duplicate;
  * the marked-block and section mechanics (`MarkedSpan`,
    `extract_marked_spans`, `fenced_line_flags`, `section_bounds` and
    `stray_numbers_outside_code`), which the SS1 field-total owner
    (#1669) and the SS2.1 record-count owner (#2269) both read. A
    second fence-aware section reader is exactly the hand-rolled
    format validator this repository has twice paid review rounds for;
  * the projection canonicalizer (`canonical_projection_accessor`,
    `parse_projection_binding_expressions`, `parse_projection_bindings`
    and `ALIAS_PRESERVING_WRAPPERS`), which the E8 save-load check and
    the writer scan's capability-accessor map both read.

The import direction is one way only: the aggregate imports this
module and every owner; every owner imports this module; nothing here
imports any of them, and no owner imports another. Adding a symbol
here is justified only when two or more owners need it -- a helper one
owner owns belongs in that owner. Nothing here may carry inventory,
boundary, projection or writer POLICY (#2064 requirement 12): these are
mechanics, and the decisions they are applied to live with the owner
that makes them.

This module is not a gate and has no `main`; the two commands CI and
`tools/ci-local.sh` run are unchanged (`python3
tools/test_engine_env_capability_audit.py` then `python3
tools/engine_env_capability_audit.py`).
"""
from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import NamedTuple

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from persistence_inventory_audit_haskell import extract_record_fields  # type: ignore

INVENTORY_PATH = REPO_ROOT / "docs" / "engineenv_capability_inventory.md"
ENGINE_ENV_FILE = "src/Engine/Core/State.hs"
ENGINE_ENV_PATTERN = r"^data EngineEnv = EngineEnv\b"

# ===========================================================================
# Inventory-document primitives (issue #876; widened by issue #2064)
# ===========================================================================
#
# Policy-free MARKDOWN mechanics: how a cell is read, not what any cell
# is allowed to say. Every rule about which capabilities, roles,
# lifecycles, modules or fields a cell may name belongs to the owner
# that enforces it -- none of that lives here. What is here is what
# more than one owner would otherwise write out a second time, which
# #2064 requirement 21 forbids.

# A free-text cell (Sync/Init/Shutdown/Notes) that is present but
# carries no real content -- name-presence without an actual decision.
# Read by the inventory owner (SS5's Sync/Init/Shutdown cells), the
# access owner (SS6.1's Category/Reason cells) and the writer scanner's
# shadow-exemption check.
_PLACEHOLDER_CELLS = {"", "-", "--", "—", "?", "tbd", "n/a", "na"}


def _is_placeholder(cell: str) -> bool:
    return cell.strip().lower() in _PLACEHOLDER_CELLS


# The inventory doc names every field, module, role and capability in a
# backtick code span, so pulling the names out of a table cell is the
# one operation all three document-reading owners share: the inventory
# owner (SS5's field names and role tokens), the field-total owner (SS1's
# marked span) and the access owner (SS6.1/SS6.2's module and capability
# cells).
BACKTICK_RE = re.compile(r"`([^`]+)`")
# A Markdown table's `|---|:--:|` separator row cell. Skipped by every
# table parser here -- SS5's rows, SS6.1's and SS6.2's -- so it is written
# once rather than once per parser.
SEPARATOR_ROW_RE = re.compile(r":?-{2,}:?")
# SS6.2's heading. It anchors the access owner's temporary-boundary
# table AND the field-total owner's procedure-item rule (SS6.2's item 1
# is the sentence that used to repeat SS1's total), so the two cannot
# hold separate copies of the literal and drift apart. SS1's and SS6.1's
# headings have one owner each and stay with it.
SECTION_6_2_HEADING = "### 6.2 Temporary compatibility boundary (production)"


# ---------------------------------------------------------------------------
# Marked-block and section mechanics (issue #1669; shared by issue #2269)
# ---------------------------------------------------------------------------
#
# An HTML-comment marker pair delimiting governed prose, the fence-aware
# section reader that decides whether a pair sits in the section it
# governs, and the number scan that says which digits in a governed span
# are a count rather than a citation. All three arrived with the SS1
# field total (#1669) and all three are now read by a second owner --
# SS2.1's audited capability-record counts (#2269) -- so they live here
# rather than in either owner. They remain policy-free: what a marker is
# CALLED, which section it must sit in, and what its numbers must equal
# are each the calling owner's rule.
#
# `§5` and `#1669` are excluded from every number scan below. They are
# navigation and provenance, not counts, and the governed prose
# necessarily carries both.

SECTION_REF_RE = re.compile(r"§\d+(?:\.\d+)*")
INTEGER_RE = re.compile(r"\d+")


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

    A pair whose markers OR BODY lie in a verbatim region is REPORTED
    and not returned. Checking only the opening marker was not enough:
    a fence opened immediately after it and closed immediately before
    the closing marker turns the whole governed paragraph into a
    rendered example while both markers sit in ordinary prose.
    Fenced content renders as an example rather than as the document's
    own prose, so a governed block moved into a fence would leave the
    real text ungoverned while every rule that reads the returned span
    still passed -- the same class of escape `section_bounds` already
    refuses for headings. Reporting rather than silently skipping it
    keeps the owner's "the block is missing" and "the block is here"
    diagnostics from both being technically true at once.

    The inventory document is the only text this is applied to, so the
    diagnostics name it directly.
    """
    spans: list[MarkedSpan] = []
    violations: list[str] = []
    fenced = fenced_line_flags(text)
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
        cursor = end + len(close_marker)
        first = text.count("\n", 0, start)
        last = text.count("\n", 0, cursor)
        verbatim = [index for index in range(first, min(last + 1, len(fenced)))
                    if fenced[index]]
        if verbatim:
            where = ("is itself inside" if verbatim[0] == first
                     else "encloses")
            violations.append(
                f"`{open_marker}` at offset {start} in "
                f"docs/{INVENTORY_PATH.name} {where} a fenced code block "
                f"or raw-HTML block (line {verbatim[0] + 1}) -- such "
                f"content is a rendered EXAMPLE, not the document's own "
                f"prose, so the governed text is displayed to nobody "
                f"while every rule that reads this span still passes")
            continue
        spans.append(MarkedSpan(text[body_start:end], start, cursor))
    return spans, violations


_FENCE_RE = re.compile(r"^(`{3,}|~{3,})")


#: The four HTML block tags CommonMark treats as VERBATIM (block type
#: 1): an unclosed one of these swallows every following line, blank
#: lines included, until its closing tag.
_VERBATIM_HTML_OPEN_RE = re.compile(
    r"<(?:pre|script|style|textarea)\b", re.IGNORECASE)
_VERBATIM_HTML_CLOSE_RE = re.compile(
    r"</(?:pre|script|style|textarea)\s*>", re.IGNORECASE)
#: A raw-HTML block of CommonMark type 6 or 7 -- `<div>`, `<table>`,
#: `<section>`, or any other complete tag on a line of its own. It
#: opens on a line whose first non-space character is `<` followed by
#: a tag name or `/`, at an indent of at most three spaces, and it runs
#: to the next BLANK line. Everything in it is raw HTML: Markdown
#: inside is not parsed, so a table there does not render as a table.
#:
#: The `<!-- ... -->` comment the markers themselves are is block type
#: 2, which ends at its own `-->` and therefore never reaches the line
#: after it. Excluding `<!` here is what keeps the markers from being
#: read as openers of a block that swallows their own table.
_HTML_BLOCK_OPEN_RE = re.compile(r"^ {0,3}<[A-Za-z/]")
#: The raw-HTML blocks that end at an explicit TERMINATOR rather than
#: at a blank line, as `(opener, terminator)` pairs -- CommonMark block
#: types 3 (`<?` ... `?>`), 5 (`<![CDATA[` ... `]]>`) and 4 (`<!` plus a
#: letter ... `>`). Like type 1 they carry every following line,
#: including blank ones, until their terminator, so each can enclose
#: the markers and a whole table.
#:
#: CDATA is listed before the type-4 declaration deliberately, though
#: the two cannot collide: `<![CDATA[` has `[` where type 4 requires a
#: letter. The `<!--` comment the markers are is type 2 and matches
#: neither -- its third character is `-` -- which is what keeps a
#: marker line from opening a block over its own table.
_TERMINATED_HTML_BLOCKS = (
    (re.compile(r"<\?"), re.compile(r"\?>")),
    (re.compile(r"<!\[CDATA\["), re.compile(r"\]\]>")),
    (re.compile(r"<![A-Za-z]"), re.compile(r">")),
)


def fenced_line_flags(text: str) -> "list[bool]":
    """Per line, whether Markdown renders that line VERBATIM rather
    than as document content.

    Two constructs, because they are the two that swallow following
    lines whole:

    * a fenced code block -- three or more backticks or tildes, closed
      by a later line of at least as many of the SAME character. The
      opening and closing fence lines themselves count as inside;
    * an open `<pre>`/`<script>`/`<style>`/`<textarea>` HTML block
      (CommonMark type 1), which carries across blank lines until its
      closing tag;
    * a raw-HTML block that ends at an explicit terminator rather than
      a blank line -- CommonMark types 3 (`<?` ... `?>`), 4 (`<!` plus
      a letter ... `>`) and 5 (`<![CDATA[` ... `]]>`);
    * any other raw-HTML block (`<div>`, `<table>`, `<section>` --
      CommonMark types 6 and 7), which runs to the next blank line.
      Markdown inside one is not parsed, so a table there renders as
      literal text. The marker comments are type 2 and end at their own
      `-->`, so they never open one.

    Types 1 and 3 through 7 are therefore all covered; type 2 is the
    markers themselves and is excluded by construction.

    This exists because `section_bounds` decides where a section ends,
    and a block containing a line like `## example` would otherwise end
    the section early -- while Markdown still renders everything after
    it inside the real section. That gap was a live bypass of the scope
    rules below: prose past the fake heading was outside the audit and
    inside the document. `extract_marked_spans` reads the same flags to
    refuse a governed block that has been moved somewhere it renders as
    an example instead of as the document's own text.
    """
    inside: list[bool] = []
    open_char = ""
    open_len = 0
    html_open = False
    html_until_blank = False
    html_terminator = None
    for line in text.splitlines():
        stripped = line.strip()
        match = _FENCE_RE.match(stripped)
        if open_char:
            inside.append(True)
            if (match and match.group(1)[0] == open_char
                    and len(match.group(1)) >= open_len
                    and not stripped[len(match.group(1)):].strip()):
                open_char = ""
                open_len = 0
            continue
        if html_open:
            inside.append(True)
            if _VERBATIM_HTML_CLOSE_RE.search(line):
                html_open = False
            continue
        if html_terminator is not None:
            inside.append(True)
            if html_terminator.search(line):
                html_terminator = None
            continue
        if html_until_blank:
            if not stripped:
                html_until_blank = False
                inside.append(False)
                continue
            inside.append(True)
            continue
        if match:
            open_char = match.group(1)[0]
            open_len = len(match.group(1))
            inside.append(True)
            continue
        if (_VERBATIM_HTML_OPEN_RE.search(line)
                and not _VERBATIM_HTML_CLOSE_RE.search(line)):
            html_open = True
            inside.append(True)
            continue
        opened = next(
            (close for open_re, close in _TERMINATED_HTML_BLOCKS
             if open_re.search(line)), None)
        if opened is not None:
            inside.append(True)
            if not opened.search(line, line.index("<")):
                html_terminator = opened
            continue
        if _HTML_BLOCK_OPEN_RE.match(line):
            html_until_blank = True
            inside.append(True)
            continue
        inside.append(False)
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
    fenced = fenced_line_flags(text)
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


# A backtick span whose digits are a SOURCE LOCATION -- a repository
# path, optionally with a line or line-range anchor. This is the only
# code span whose numbers are exempt from the no-stray-count rule.
#
# Exempting code spans wholesale was the third rereview's finding: it
# let `` `83` `` stand in the governed prose, which reads to a human as
# exactly the stale total this audit exists to remove. A span has to
# LOOK like a source reference to be excused, and a bare number does
# not.
SOURCE_SPAN_RE = re.compile(
    r"^[A-Za-z0-9_./+\-]+\.(?:hs|lua|py|md|json|yaml|yml|cabal|sh)"
    r"(?::\d+(?:-\d+)?)?$")


def stray_numbers_outside_code(text: str) -> list[str]:
    """Decimal integers in `text` that are neither a section reference,
    an issue reference, nor part of a source-location code span.

    A code span that is NOT a source location keeps its digits in the
    scan: `` `83` `` is a field total wearing a code font, not a
    citation.
    """
    def _strip_span(match: re.Match[str]) -> str:
        inner = match.group(1).strip()
        return "" if SOURCE_SPAN_RE.match(inner) else match.group(1)

    without_code = re.sub(r"`([^`]*)`", _strip_span, text)
    without_refs = re.sub(r"#\d+", "", SECTION_REF_RE.sub("", without_code))
    return INTEGER_RE.findall(without_refs)


# ===========================================================================
# SS6 full-access ratchet (issue #889, EngineEnv capability split E1)
# ===========================================================================
#
# docs/engineenv_capability_inventory.md SS6.1's permanent modules -- a
# hard, checked-in allowlist. `Engine.Core.State` itself (the definer,
# which imports nothing and so can never appear in a live importer
# scan) is the 24th permanent module; PERMANENT_IMPORTERS below holds
# only the 23 modules that actually IMPORT it.
PERMANENT_DEFINER = "Engine.Core.State"

PERMANENT_IMPORTERS = frozenset({
    "Engine.Core.Monad",
    "Engine.Core.Init",
    "Engine.Core.Defaults",
    # `Engine.Loop.Headless` left this list in issue #1022: its whole
    # body is now one `Engine.Loop.Mode.LoopMode` value, and the shared
    # driver that reads `lifecycleRef`/`inputQueue`/`saveBarrierRef`
    # (`Engine.Loop.Mode`) names those three fields in a narrow import.
    "Engine.Loop", "Engine.Loop.Frame",
    "Engine.Loop.Shutdown", "Engine.Loop.Camera", "Engine.Loop.Timing",
    "Engine.Loop.Resource",
    "app/App/Graphical.hs", "app/App/Offscreen.hs", "app/App/Preview.hs",
    "app/App/Headless.hs", "app/App/Dump.hs",
    "Engine.Scripting.Lua.Thread", "Engine.Scripting.Lua.Thread.Dispatch",
    "Engine.Scripting.Lua.Thread.Console",
    "Engine.Scripting.Lua.Message",
    "World.Thread.Command.Save", "World.Thread.Command.Save.WriteWorld",
    "World.Load.Stage", "World.Load.Publish", "Engine.Scripting.Lua.API.Save",
})

PRODUCTION_DIRS = ("src", "app")
STATE_MODULE = "Engine.Core.State"
# The record whose selectors `EngineEnv(..)` brings into scope --
# `WindowState(..)` in the same import list brings its own, and not
# these.
ENGINE_ENV_TYPE = "EngineEnv"

_IMPORT_LINE_RE = re.compile(r"^import\b")
# A character literal, including an escape. Used both to step over one
# while stripping comments and to step over one while tokenizing.
_CHAR_LITERAL_RE = re.compile(r"'(?:\\.|[^\\'])'")
# The symbol characters a dash run may continue into. Per the Haskell
# report `--` opens a comment only when the run of dashes is NOT
# followed by one of these -- otherwise it is an operator such as
# `-->`, and the code after it is code.
_SYMBOL_CHARS = frozenset("!#$%&*+./<=>?@\\^|~:-")


def _strip_haskell_comments(text: str) -> str:
    """Blank `{- -}` and `--` comments, preserving every character
    position: comment characters become spaces and newlines are kept,
    so line numbers and the column-0 tests downstream are unaffected.

    __Literal-aware, because a comment marker inside a string is
    text.__ `let marker = "--" in writeIORef (fieldOne env) 1` is a
    real write, and a scanner that stopped at that `--` would drop it
    silently -- the exact failure mode this audit exists to prevent.
    String and character literals are therefore stepped over, block
    comments nest the way Haskell's do, and a dash run continuing into
    a symbol character is an operator rather than a comment."""
    out = list(text)
    i, n = 0, len(text)
    while i < n:
        ch = text[i]
        if ch == '"':
            i += 1
            while i < n and text[i] != '"':
                i += 2 if text[i] == "\\" else 1
            i += 1
            continue
        if ch == "'":
            # A prime continues an identifier; only a `'` that does not
            # follow one can open a character literal.
            previous = text[i - 1] if i else ""
            literal = (None if (previous.isalnum() or previous in "_'")
                       else _CHAR_LITERAL_RE.match(text, i))
            i = literal.end() if literal else i + 1
            continue
        if text.startswith("{-", i):
            depth, j = 0, i
            while j < n:
                if text.startswith("{-", j):
                    depth += 1
                    j += 2
                    continue
                if text.startswith("-}", j):
                    depth -= 1
                    j += 2
                    if depth == 0:
                        break
                    continue
                j += 1
            for k in range(i, min(j, n)):
                if out[k] != "\n":
                    out[k] = " "
            i = j
            continue
        if text.startswith("--", i):
            run = i
            while run < n and text[run] == "-":
                run += 1
            if run < n and text[run] in _SYMBOL_CHARS:
                i = run
                continue
            end = text.find("\n", i)
            end = n if end == -1 else end
            for k in range(i, end):
                out[k] = " "
            i = end
            continue
        i += 1
    return "".join(out)


def _import_chunks(text: str) -> list[str]:
    """Every top-level `import` declaration's FULL text (covering
    multiline module names/import lists), bounded by Haskell's layout
    rule: a continuation line is blank or indented; the declaration
    ends at the first non-blank, column-0 line (the next import, or
    the first non-import top-level declaration -- e.g. a bare import
    that is the file's LAST import is bounded correctly either way)."""
    lines = text.split("\n")
    starts = [i for i, line in enumerate(lines) if _IMPORT_LINE_RE.match(line)]
    chunks = []
    for start in starts:
        end = len(lines)
        for j in range(start + 1, len(lines)):
            line = lines[j]
            if line.strip() == "":
                continue
            if line[0] not in (" ", "\t"):
                end = j
                break
        chunks.append("\n".join(lines[start:end]))
    return chunks


_IMPORT_HEAD_RE = re.compile(r"^import\s+(?:qualified\s+)?([A-Za-z][A-Za-z0-9_.']*)")


def imports_module(source_text: str, module: str) -> bool:
    """True iff `source_text` imports `module` (comments stripped, so a
    Haddock reference to a module name never counts as an import).

    `_IMPORT_HEAD_RE` -- the module name at the head of one chunk -- is
    the completion of `_import_chunks` above, and issue #2064 moved
    both here because the access owner's `Engine.Core.State`
    classification and the structural-boundary owner's capability-import
    check read the same head. The predicate travels with its regex
    rather than leaving a bare pattern exported on its own.
    """
    for chunk in _import_chunks(_strip_haskell_comments(source_text)):
        head = _IMPORT_HEAD_RE.match(chunk)
        if head and head.group(1) == module:
            return True
    return False


def module_identifier(relpath: str) -> str:
    """`src/Engine/Core/Log/Monad.hs` -> `Engine.Core.Log.Monad`
    (matching SS6.2's dotted-name citations); an `app/*.hs` boot module
    keeps its literal relative path (matching SS6.1's own citations --
    every one of them is `module Main where`, so a dotted name would
    collide)."""
    parts = Path(relpath).parts
    if parts[0] == "src":
        return ".".join(parts[1:])[:-len(".hs")]
    return relpath


def scan_production_sources(repo_root: Path) -> dict[str, str]:
    """IO wrapper: `{relative_path: source_text}` for every production
    Haskell file under `repo_root`."""
    sources: dict[str, str] = {}
    for base in PRODUCTION_DIRS:
        for path in sorted((repo_root / base).rglob("*.hs")):
            relpath = str(path.relative_to(repo_root))
            sources[relpath] = path.read_text(encoding="utf-8", errors="replace")
    return sources


# ===========================================================================
# Projection canonicalization (issues #899, #1896, #2059)
# ===========================================================================
#
# How a capability record's `to<Name>Capability env = ...` construction
# is read back into `{capability field: EngineEnv accessor}` pairs.
# Two consumers, in two modules: the aggregate's E8 save-load check
# (`audit_save_load_projection`) pins one record's five bindings, and
# the writer scanner's `capability_accessor_map` derives the whole
# accessor-ownership map behind SS5 enforcement and the SS6.5 residue
# from every record. One canonicalizer serves both so they cannot
# disagree about what a binding means.
# docs/engineenv_capability_inventory.md SS2.1's abstract-wrapper
# extension (issue #1896): a view field may be
# `field = wrapper (accessor env)` instead of `field = accessor env`.
# The wrapper set is a CLOSED, named list, not "any function", because
# what earns the alias treatment is the guarantee the wrapper carries:
# `Engine.Core.ReadOnlyRef.toReadOnlyRef` is documented to wrap the
# caller's live handle and never to copy it, so the projected field is
# the same container the accessor named. A projection that applied any
# other function would be transforming the state, and inventing an
# alias for it is exactly what this canonicalizer must not do -- so an
# unrecognized wrapper does not canonicalize, and the field's binding
# is reported as underivable rather than guessed at.
#
# Reading this shape is not cosmetic. `capability_accessor_map` is what
# turns a record selector into an `EngineEnv` field for BOTH the write
# scan and the pass-on residue, so a dropped view accessor would make
# every use of it invisible -- including the context-record pass-on in
# `Building.Knowledge.Live` that D-7 exists to demonstrate, which would
# have silently left the residue CMA-3 weighs.
ALIAS_PRESERVING_WRAPPERS = frozenset({"toReadOnlyRef"})

# The name every projection binds its `EngineEnv` argument to. SS2.1's
# convention gives each capability exactly one
# `to<Name>Capability env = <Name>Capability { ... }` equation, and
# `parse_projection_binding_expressions` finds that equation by this
# same name -- so a projection that renamed the parameter would not be
# found at all, which `audit_capability_projection_completeness` then
# reports rather than silently deriving nothing.
PROJECTION_PARAMETER = "env"

# A projection binding's right-hand side is canonicalized
# STRUCTURALLY, not matched as a surface string (issue #2059). Two
# shapes denote the live container: the bare application
# `accessor env` -- with the accessor optionally QUALIFIED, since a
# capability module may import `Engine.Core.State` under an alias and
# project `fkFieldOne = State.fieldOne env` -- and SS2.1's wrapped
# `wrapper (accessor env)`.
#
# Haskell lets EITHER be written with semantically inert grouping:
# `(accessor env)`, `(accessor) env`, `wrapper ((accessor env))`. The
# regexes this replaced matched only the ungrouped spellings, so a
# parenthesized-but-equivalent projection silently produced NO
# binding: `capability_accessor_map` omitted the selector, every
# direct write through it resolved to no field and was recorded as
# `other`, and the write vanished from `CAPABILITY_WRITER_MODULES`
# enforcement, the residue and the closed-form safety check while the
# gate still exited 0. Grouping is stripped here so no legal
# respelling can do that, and anything this canonicalizer cannot read
# fails LOUDLY through the completeness audit instead of disappearing.
_PROJECTION_ATOM_RE = re.compile(
    r"(?P<paren>[()])"
    r"|(?P<name>(?:[A-Z][A-Za-z0-9_']*\.)*[A-Za-z_][A-Za-z0-9_']*)"
    r"|(?P<space>\s+)"
    r"|(?P<other>\S)")


def _projection_expression_tree(expression: str) -> list | None:
    """`expression` as a nested list -- an identifier per string, one
    level of nesting per parenthesis group -- or `None` when it holds
    anything else or its parentheses do not balance.

    Refusing everything else is the fail-closed half. An operator, a
    literal, a lambda, a visible type application or a record update is
    an expression this canonicalizer does not model, and guessing at
    one would invent an alias; returning `None` instead leaves the
    field underivable, which
    `audit_capability_projection_completeness` reports by name."""
    stack: list[list] = [[]]
    for match in _PROJECTION_ATOM_RE.finditer(expression):
        kind = match.lastgroup
        if kind == "space":
            continue
        if kind == "other":
            return None
        if kind == "name":
            stack[-1].append(match.group())
            continue
        if match.group() == "(":
            stack.append([])
        else:
            if len(stack) == 1:
                return None
            group = stack.pop()
            stack[-1].append(group)
    return stack[0] if len(stack) == 1 else None


def _ungroup(node: "str | list"):
    """`(x)` is `x`, however deeply nested: a group holding exactly one
    item is inert grouping and unwraps to that item. A group holding
    two or more is an APPLICATION and must survive, which is what keeps
    `(accessor env)` distinguishable from `(accessor) env` only in
    spelling and not in meaning."""
    while isinstance(node, list) and len(node) == 1:
        node = node[0]
    return node


def _canonical_application(node: "str | list",
                           wrappers: frozenset[str]) -> str | None:
    """The bare accessor `node` applies to `PROJECTION_PARAMETER`, or
    `None`. Recursive only through a recognized alias-preserving
    wrapper, so `toReadOnlyRef (fieldOne env)` reaches `fieldOne` while
    `snapshotOf (fieldOne env)` reaches nothing."""
    node = _ungroup(node)
    if not isinstance(node, list) or len(node) != 2:
        return None
    head, argument = _ungroup(node[0]), _ungroup(node[1])
    if isinstance(head, list):
        return None
    base = head.rpartition(".")[2]
    if argument == PROJECTION_PARAMETER:
        return base
    if base not in wrappers:
        return None
    return _canonical_application(argument, wrappers)


def canonical_projection_accessor(
    expression: str, *,
    wrappers: frozenset[str] = ALIAS_PRESERVING_WRAPPERS,
) -> str | None:
    """The BARE `EngineEnv` accessor a projection binding's right-hand
    side names, or `None` when this canonicalizer cannot read it.

    Semantically inert grouping is stripped, so every spelling of the
    same projection canonicalizes identically -- `accessor env`,
    `(accessor env)`, `(accessor) env`, `((accessor env))`, and the
    wrapped `wrapper (accessor env)` with the same freedom. The
    accessor is reported bare whether it was written qualified or not:
    the qualifier says which module it came from, which is already
    settled by the time we get here, and the FIELD name is what every
    consumer wants."""
    tree = _projection_expression_tree(expression)
    if tree is None:
        return None
    return _canonical_application(tree, wrappers)


_BINDING_HEAD_RE = re.compile(
    r"^\s*([a-z_][A-Za-z0-9_']*)\s*=(?![=<>:!#$%&*+./\\?@^|~-])")


def _split_top_level(text: str) -> list[str]:
    """`text` split on the commas that are not inside a nested brace,
    parenthesis or bracket -- i.e. one segment per record binding."""
    segments: list[str] = []
    depth = 0
    start = 0
    for index, character in enumerate(text):
        if character in "([{":
            depth += 1
        elif character in ")]}":
            depth -= 1
        elif character == "," and depth == 0:
            segments.append(text[start:index])
            start = index + 1
    segments.append(text[start:])
    return segments


def parse_projection_binding_expressions(source_text: str, projection: str
                                         ) -> dict[str, str]:
    """`{capability field: right-hand-side text}` for every top-level
    binding in `projection`'s record construction, WHETHER OR NOT the
    right-hand side canonicalizes.

    Comments are stripped first, so a Haddock example never counts as a
    binding. Returns `{}` if the projection has no
    `<projection> env = <Record> { ... }` equation in `source_text` at
    all -- a state `audit_capability_projection_completeness` reports,
    because a projection the parser cannot find is every one of its
    fields lost at once."""
    code = _strip_haskell_comments(source_text)
    lines = code.split("\n")
    start = None
    equation = re.compile(
        rf"^[ \t]*{re.escape(projection)}\s+{PROJECTION_PARAMETER}\s*=")
    for i, line in enumerate(lines):
        if equation.match(line):
            start = i
            break
    if start is None:
        return {}

    depth = 0
    seen_open = False
    body: list[str] = []
    for line in lines[start:]:
        body.append(line)
        depth += line.count("{") - line.count("}")
        if "{" in line:
            seen_open = True
        if seen_open and depth <= 0:
            break
    text = "\n".join(body)
    opening = text.find("{")
    if opening == -1:
        return {}
    closing = text.rfind("}")
    if closing < opening:
        return {}

    bindings: dict[str, str] = {}
    for segment in _split_top_level(text[opening + 1:closing]):
        head = _BINDING_HEAD_RE.match(segment)
        if head is None:
            continue
        bindings[head.group(1)] = segment[head.end():].strip()
    return bindings


def parse_projection_bindings(source_text: str, projection: str
                              ) -> dict[str, str]:
    """`{capability field: EngineEnv accessor}` for every binding
    inside `projection`'s record construction that canonicalizes
    (`canonical_projection_accessor`), the accessor reported BARE
    whether it was written qualified or not.

    A binding whose right-hand side does not canonicalize is ABSENT
    from this map rather than guessed at -- inventing an alias for
    `snapshotOf (fieldOne env)` would claim a guarantee nothing gives.
    Absence is not the end of the story: every declared field of a live
    capability record must appear here, and
    `audit_capability_projection_completeness` is what turns a missing
    one into a loud failure instead of a silent hole in the write
    map."""
    return {
        field: accessor
        for field, accessor in (
            (field, canonical_projection_accessor(expression))
            for field, expression
            in parse_projection_binding_expressions(
                source_text, projection).items())
        if accessor is not None}

