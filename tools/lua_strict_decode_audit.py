#!/usr/bin/env python3
"""Guard (#1605): no direct strict `Data.Text.Encoding.decodeUtf8` in
Haskell source under `src/Engine/Scripting/Lua/`.

Issue #665 / PR #825 established the convention this restores. A Lua
string is an arbitrary byte array, so a value crossing the Lua boundary
may hold bytes that are not valid UTF-8. `decodeUtf8` is PARTIAL: it
throws `UnicodeException` on such input, out of the middle of a Lua
call, instead of letting the surrounding code refuse the value with the
warning it was written to emit (closed #618 is the original crash).
The sweep replaced 236 expressions across 55 files with a total
sibling; nothing checked, and 37 days later commit `893cbd8f` (#1217)
reintroduced two strict calls in `API/Units/Spawn.hs` that passed
review and the headless suite. This audit is what would have caught
them.

The compliant siblings are deliberately NOT flagged, because each is
total:

  decodeUtf8'       -> `Either UnicodeException Text`
  decodeUtf8With    -> caller-supplied `OnDecodeError` handler
  decodeUtf8Lenient -> substitutes U+FFFD

WHAT IS DETECTED, exactly: a USE of the function
`Data.Text.Encoding.decodeUtf8` -- that ONE module's export -- in code
under the scoped tree, however it is spelled. Nothing else. An import
is not a use, and a same-named function from anywhere else is a
different function: `Data.Text.Lazy.Encoding.decodeUtf8`, an unrelated
`Other.decodeUtf8`, or a local binding of that name are all clean, and
the fixtures below pin each of them so.

Reaching that precision takes three parts.

1. IMPORTS ARE RESOLVED PER FILE, BY TOKEN. Each file's own import
   declarations are parsed for exactly `Data.Text.Encoding`, and what
   they establish drives the scan. A declaration begins at an `import`
   TOKEN that opens one -- at the start of a line at any indent, or
   after `{` or `;` -- so neither an indented top-level layout
   (`module M where` with the body at column 1) nor an explicit-brace
   layout (`module M where { import ...; f = ... }`) hides one. It ends
   at the `;`/`}` that closes it, or by layout at the first line indented
   no further than its own opening column, whichever comes first.

   Both `qualified` spellings are understood -- the prepositive one and
   GHC2024's postpositive `import Data.Text.Encoding qualified as TE`
   (`ImportQualifiedPost`, on by default in this language edition).

   The parser is FAIL-LOUD: a declaration naming `Data.Text.Encoding` in
   a shape it does not model raises rather than being scanned as though
   the module were absent. A silent "no import, so nothing to flag" is
   exactly how an import-resolving guard gets walked past.

2. A QUALIFIED USE IS RESOLVED EXACTLY. `TE.decodeUtf8` under the
   repository's standard alias, `Enc.decodeUtf8` under any other, and
   the unaliased `Data.Text.Encoding.decodeUtf8` all resolve through the
   file's own import, so renaming the alias cannot walk past this. A
   module qualifier can never be captured by a local term binder, so a
   qualified hit is unambiguous -- there is no shadowing question to ask.

3. A BARE USE IS RESOLVED PER OCCURRENCE, AGAINST THE BINDERS THAT CAN
   ACTUALLY REACH IT. `import Data.Text.Encoding` puts `decodeUtf8` in
   unqualified scope, but a nested binder -- a function parameter, a
   `let`/`where` binding, a lambda or case pattern -- may legally shadow
   it. (A TOP-LEVEL binding cannot: clashing with an unqualified import
   of the same name is an ambiguous-occurrence error, so such a file
   does not compile.) Every nested binder is confined to ONE top-level
   declaration, so the file is split into those first and each
   occurrence consults only its own declaration's binders. Within a
   declaration the reach of a binder follows the construct that made it:

     * a parameter or definition head (a binder left of the
       declaration's own `=`) and a `where` binding scope over the WHOLE
       declaration. `where` is why "a binder shadows what follows it" is
       not enough on its own: a `where` clause is written AFTER the body
       it scopes over.

     * every other binder -- `let`, lambda, case alternative -- is
       written BEFORE the body it scopes over, so it shadows only
       occurrences after it. An occurrence EARLIER in the same
       declaration is still the import's, and is reported.

   What counts as a possible binder (`_binder_positions`) is
   deliberately over-approximated, because erring that way can only ever
   silence an occurrence, never blame one -- the direction a guard may
   err in when the alternative is guessing at scope.

Two more properties hold throughout: candidates are maximal identifier
LEXEMES, so the total siblings are safe by construction
(`decodeUtf8Lenient` is a DIFFERENT identifier, never a `decodeUtf8`
followed by something, and `myDecodeUtf8` is different again); and
comment and string-literal awareness comes from
`tools/unicode_operator_audit.py`'s lexer via its public
`haskell_code_spans` / `haskell_code_only`, rather than a second copy
free to drift from it.

Usage:
  python3 tools/lua_strict_decode_audit.py              # audit the tree
  python3 tools/lua_strict_decode_audit.py --self-test  # fixtures only
Exit codes: 0 = clean, 1 = a strict decode appeared under the scoped
tree (or, under --self-test, a fixture behaved wrongly).
"""
from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from unicode_operator_audit import (  # type: ignore  # noqa: E402
    haskell_code_only, haskell_code_spans)

# The tree #665 scoped the convention to. Widening it beyond the Lua
# API is a separate decision (#1605 "Out of scope"): the rest of `src/`
# and `app/` happens to be clean today, but nothing there is handed raw
# Lua bytes.
SCOPED_TREE = "src/Engine/Scripting/Lua"

# The module that exports the banned function, and the function itself.
# Both are exact: `Data.Text.Lazy.Encoding` is a different module and
# `decodeUtf8Lenient` is a different name.
STRICT_MODULE = "Data.Text.Encoding"
BANNED_IDENTIFIER = "decodeUtf8"
TOTAL_SIBLINGS = ("decodeUtf8'", "decodeUtf8With", "decodeUtf8Lenient")

# Exemptions, if one is ever needed: repo-relative path -> the reason it
# is exempt, stated inline the way tools/unicode_operator_audit.py
# states its own. Whole-file, because there is no construct-scoped case
# to carve out here. A clean tree needs none.
EXEMPTIONS: dict[str, str] = {}

# Haskell 2010 report SS2.4: the characters a variable or constructor
# identifier is made of. A MAXIMAL run of these is one lexeme.
_IDENT_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'"

# `STRICT_MODULE` named as a whole module path -- not as the prefix of a
# longer one, so `Data.Text.Encoding.Error` is correctly NOT a mention.
_MENTIONS_STRICT_MODULE = re.compile(
    r"(?<![\w'.])" + re.escape(STRICT_MODULE) + r"(?![\w'.])")

# The import shapes this module understands, per the Haskell 2010 report
# SS5.3: `import [qualified] Data.Text.Encoding [as Alias]
# [hiding] [( ... )]`. Anything else that names the module is a parse
# failure, not a pass (see `_ImportParseError`).
_STRICT_IMPORT = re.compile(
    r"\Aimport\s+(?P<prequalified>qualified\s+)?"
    + re.escape(STRICT_MODULE) + r"(?![\w'.])"
    # GHC2024 enables ImportQualifiedPost, so `qualified` may follow the
    # module name instead of preceding it. Both spellings mean the same
    # import; rejecting the postpositive one would raise on a perfectly
    # compliant file.
    r"(?P<postqualified>\s+qualified(?![\w']))?"
    r"(?:\s+as\s+(?P<alias>[A-Z][\w']*(?:\.[A-Z][\w']*)*))?"
    r"(?P<rest>[\s\S]*)\Z")

# Reserved words read as whole tokens, wherever they sit.
_IMPORT_TOKEN = re.compile(r"(?<![\w'])import(?![\w'])")
_MODULE_TOKEN = re.compile(r"(?<![\w'])module(?![\w'])")
_WHERE_TOKEN = re.compile(r"(?<![\w'])where(?![\w'])")

# Haskell 2010 report SS2.4's symbolic-operator characters. A maximal run
# of these is one operator lexeme, which is how `=` is told from `==`,
# `=>` or `>=`.
_SYMBOL_RUN = re.compile(r"[!#$%&*+./<=>?@\\^|~:-]+")

# Operator lexemes that separate a binding's LEFT-HAND side from its
# right: `=` for a definition, `->` for a lambda or case alternative,
# `::` for the signature that declares one. The project's Unicode
# spellings count too.
_BINDING_SEPARATORS = frozenset({"=", "->", "::"})
_UNICODE_BINDING_SEPARATORS = ("→", "∷")


class _ImportParseError(Exception):
    """A `Data.Text.Encoding` import this module cannot classify.

    Raised rather than swallowed: treating an unmodelled import shape as
    "the module is not imported here" would make every use in that file
    invisible, which is the one failure mode a guard must not have."""


@dataclass(frozen=True)
class Violation:
    path: str
    line: int
    spelling: str

    def __str__(self) -> str:
        return (f"{self.path}:{self.line}: strict decode "
                f"`{self.spelling}` ({STRICT_MODULE}.{BANNED_IDENTIFIER}) "
                f"-- Lua strings are arbitrary bytes; use "
                f"decodeUtf8Lenient (or another total sibling)")


@dataclass(frozen=True)
class StrictImports:
    """How `STRICT_MODULE`'s `decodeUtf8` can be named in one file:
    the module qualifiers a call site may write it behind, and whether
    any import also puts it in unqualified scope."""
    qualifiers: frozenset[str]
    unqualified: bool


# ---------------------------------------------------------------------
# Import resolution
# ---------------------------------------------------------------------

def _opens_declaration(code_text: str, start: int) -> bool:
    """True if the `import` token at `start` opens a declaration rather
    than sitting inside one.

    It does when only spaces separate it from the start of its line, or
    from an explicit-layout `{` or `;`. Those are the only positions the
    Haskell grammar allows a declaration to begin at, in either layout
    style."""
    i = start - 1
    while i >= 0 and code_text[i] in " \t":
        i -= 1
    return i < 0 or code_text[i] in "\n{;"


def _declaration_end(code_text: str, pos: int, column: int) -> int:
    """Where the declaration opened at `column` and continuing from
    `pos` ends: at the `;` or `}` that closes it in an explicit layout,
    or at the first line indented no further than `column` in an
    implicit one -- whichever comes first.

    Bracket depth is tracked so a `;` inside an import list (or any
    parenthesised group) cannot end the declaration early."""
    depth = 0
    i = pos
    n = len(code_text)
    while i < n:
        char = code_text[i]
        if char in "([":
            depth += 1
        elif char in ")]":
            depth = max(0, depth - 1)
        elif depth == 0 and char in ";}":
            return i
        elif char == "\n":
            line_start = i + 1
            j = line_start
            while j < n and code_text[j] in " \t":
                j += 1
            if j < n and code_text[j] != "\n":
                # A continuation line can never itself be the reserved
                # word `import`, so one that is starts a fresh
                # declaration however deeply it is indented. Without
                # this a malformed file folds two imports into one and
                # the guard reports the wrong shape instead of the
                # import it can read.
                if (j - line_start) <= column or _IMPORT_TOKEN.match(code_text, j):
                    return i
        i += 1
    return n


def _import_declarations(code_text: str) -> list[tuple[int, int, str]]:
    """Every import declaration in `code_text` as `(start, end, text)`.

    Found by TOKEN position, never by column: see `_opens_declaration`
    and `_declaration_end`. `code_text` must already be comment- and
    string-masked, so a comment quoting an import is never mistaken for
    one."""
    decls: list[tuple[int, int, str]] = []
    for match in _IMPORT_TOKEN.finditer(code_text):
        start = match.start()
        if not _opens_declaration(code_text, start):
            continue
        column = start - (code_text.rfind("\n", 0, start) + 1)
        end = _declaration_end(code_text, match.end(), column)
        decls.append((start, end, code_text[start:end]))
    return decls


def _listed_names(chunk: str) -> set[str]:
    """The identifiers named inside an import list. Membership is all
    this module asks of it, and `decodeUtf8Lenient` lexes as its own
    identifier, so a coarse identifier scan cannot answer wrongly."""
    return set(re.findall(r"[A-Za-z_][A-Za-z0-9_']*", chunk))


def _classify_strict_import(decl: str, rel_path: str,
                            line: int) -> tuple[str, bool] | None:
    """`(qualifier, unqualified)` for an import of `STRICT_MODULE` that
    brings `decodeUtf8` into scope, or None if it does not (a `hiding`
    list naming it, or an explicit list omitting it).

    Raises `_ImportParseError` when the declaration names the module in
    a shape this parser does not model."""
    match = _STRICT_IMPORT.match(decl.strip())
    if match is None:
        raise _ImportParseError(
            f"{rel_path}:{line}: this import names {STRICT_MODULE} in a shape "
            f"tools/lua_strict_decode_audit.py does not model, so it cannot "
            f"say which spellings of {BANNED_IDENTIFIER} it brings into "
            f"scope:\n\n{decl.strip()}\n\n"
            f"Teach _STRICT_IMPORT the shape rather than letting the file go "
            f"unscanned.")

    rest = match.group("rest").strip()
    hiding = False
    if rest.startswith("hiding"):
        hiding = True
        rest = rest[len("hiding"):].strip()
    if rest.startswith("("):
        if not rest.endswith(")"):
            raise _ImportParseError(
                f"{rel_path}:{line}: unterminated import list on this "
                f"{STRICT_MODULE} import:\n\n{decl.strip()}")
        listed = _listed_names(rest)
        in_scope = (BANNED_IDENTIFIER not in listed) if hiding \
            else (BANNED_IDENTIFIER in listed)
    elif rest or hiding:
        raise _ImportParseError(
            f"{rel_path}:{line}: unexpected trailing text on this "
            f"{STRICT_MODULE} import:\n\n{decl.strip()}")
    else:
        in_scope = True

    if not in_scope:
        return None
    alias = match.group("alias")
    qualified = (match.group("prequalified") is not None
                 or match.group("postqualified") is not None)
    return (alias if alias else STRICT_MODULE, not qualified)


def resolve_strict_imports(
        code_text: str,
        rel_path: str) -> tuple[StrictImports, list[tuple[int, int]]]:
    """`(how this file can name the banned function, import spans)`.

    The spans are returned so the call-site scan can skip the import
    declarations themselves: importing a name decodes nothing, and the
    USE it enables is what this audit reports."""
    qualifiers: set[str] = set()
    unqualified = False
    spans: list[tuple[int, int]] = []
    for start, end, decl in _import_declarations(code_text):
        spans.append((start, end))
        if not _MENTIONS_STRICT_MODULE.search(decl):
            continue
        classified = _classify_strict_import(
            decl, rel_path, code_text.count("\n", 0, start) + 1)
        if classified is None:
            continue
        qualifier, is_unqualified = classified
        qualifiers.add(qualifier)
        # The unaliased module path always names this module
        # unambiguously wherever it is in scope at all, so accept it
        # alongside whatever alias was declared. On code that compiles
        # this can only ever be the banned function.
        qualifiers.add(STRICT_MODULE)
        unqualified = unqualified or is_unqualified
    return StrictImports(frozenset(qualifiers), unqualified), spans


# ---------------------------------------------------------------------
# Call-site scan
# ---------------------------------------------------------------------

def _identifier_runs(text: str, start: int, end: int):
    """Every maximal identifier lexeme in `text[start:end)`, as
    `(position, lexeme)`."""
    i = start
    while i < end:
        if text[i] not in _IDENT_CHARS:
            i += 1
            continue
        run_start = i
        while i < end and text[i] in _IDENT_CHARS:
            i += 1
        yield run_start, text[run_start:i]


def _qualifier_before(text: str, pos: int) -> str:
    """The module qualifier written immediately before `pos` (`"TE"`,
    `"Data.Text.Encoding"`), or `""` for an unqualified occurrence.

    A qualifier is written with no space before the `.`, so it is the
    `[\\w'.]` run ending in `.` directly ahead of the name -- and only
    when every one of its dot-separated segments is uppercase-led, which
    is what separates `TE.decodeUtf8` from the composition
    `f.decodeUtf8`."""
    start = pos
    while start > 0 and (text[start - 1] in _IDENT_CHARS
                         or text[start - 1] == "."):
        start -= 1
    candidate = text[start:pos]
    if not candidate.endswith("."):
        return ""
    segments = candidate[:-1].split(".")
    if segments and all(seg and seg[0].isupper() for seg in segments):
        return candidate[:-1]
    return ""


def _module_header_span(code_text: str) -> list[tuple[int, int]]:
    """The `module ... where` header's span, if any.

    Excluded from the call-site scan for the same reason import
    declarations are: an export list names entities, it does not use
    them, so `module M (decodeUtf8) where` decodes nothing."""
    match = _MODULE_TOKEN.search(code_text)
    if match is None or not _opens_declaration(code_text, match.start()):
        return []
    depth = 0
    i = match.end()
    n = len(code_text)
    while i < n:
        char = code_text[i]
        if char in "([":
            depth += 1
        elif char in ")]":
            depth = max(0, depth - 1)
        elif depth == 0:
            where = _WHERE_TOKEN.match(code_text, i)
            if where:
                return [(match.start(), where.end())]
        i += 1
    return [(match.start(), n)]


def _body_column(code_text: str, skip_spans: list[tuple[int, int]]) -> int:
    """The column the module body's top-level declarations sit at.

    Every declaration's continuation lines are indented further than its
    head, so the SMALLEST indent among the body's code lines is the
    column the heads share -- which is 0 for an ordinary module and 1
    for one laid out at column 1."""
    columns = []
    for offset, line in _lines_with_offsets(code_text):
        body = line.lstrip(" \t")
        if not body or _within(offset, skip_spans):
            continue
        columns.append(len(line) - len(body))
    return min(columns) if columns else 0


def _top_level_declarations(
        code_text: str,
        skip_spans: list[tuple[int, int]]) -> list[tuple[int, int]]:
    """The `[start, end)` span of each top-level declaration.

    A declaration head sits at `_body_column`; anything indented further
    belongs to it. An explicit-layout `;` at bracket depth 0 splits one
    too. This is the unit a `where` binding or a parameter can shadow
    across, and nothing outside it can be shadowed by them -- which is
    what makes per-occurrence resolution possible without a parser."""
    column = _body_column(code_text, skip_spans)
    boundaries = [0]
    depth = 0
    for i, char in enumerate(code_text):
        if char in "([":
            depth += 1
        elif char in ")]":
            depth = max(0, depth - 1)
        elif depth == 0 and char in ";{}":
            boundaries.append(i + 1)
        elif char == "\n":
            line_start = i + 1
            j = line_start
            while j < len(code_text) and code_text[j] in " \t":
                j += 1
            if (j < len(code_text) and code_text[j] != "\n"
                    and (j - line_start) <= column):
                boundaries.append(line_start)
    boundaries.append(len(code_text))
    ordered = sorted(set(boundaries))
    return [(a, b) for a, b in zip(ordered, ordered[1:]) if b > a]


def _binder_positions(code_text: str, start: int, end: int,
                      skip_spans: list[tuple[int, int]]) -> list[int]:
    """Positions in `code_text[start:end)` where `decodeUtf8` might be
    BOUND rather than used.

    Deliberately an OVER-approximation: erring towards "this is a
    binder" can only ever silence a bare occurrence, never blame one,
    which is the direction a guard may err in when the alternative is
    guessing at scope. An occurrence counts when it is the first token
    of its line (a declaration or case-alternative head), when a
    binding separator (`=`, `->`, `::`, or the project's `→` / `∷`)
    follows it on its line -- covering a definition head, a parameter, a
    `let`/`where` binding, a case pattern and the signature that
    declares one -- or when a `\\` opens a lambda earlier on its line."""
    binders: list[int] = []
    for line_start, line in _lines_with_offsets(code_text):
        if line_start >= end:
            break
        if line_start + len(line) < start:
            continue
        for pos, lexeme in _identifier_runs(code_text, line_start,
                                            line_start + len(line)):
            if lexeme != BANNED_IDENTIFIER or not (start <= pos < end):
                continue
            if _within(pos, skip_spans):
                continue
            column = pos - line_start
            if (not line[:column].strip()
                    or "\\" in line[:column]
                    or _separator_after(line, column)):
                binders.append(pos)
    return binders


def _shadowed_region(code_text: str, decl: tuple[int, int],
                     skip_spans: list[tuple[int, int]]) -> int | None:
    """From which position in `decl` a bare `decodeUtf8` is shadowed by
    one of the declaration's own binders -- `decl[0]` when the whole
    declaration is shadowed, or None when nothing in it is.

    The split is by which construct binds. A PARAMETER or definition
    head (a binder left of the declaration's first `=`) and a `where`
    binding both scope over the entire declaration, and the `where` case
    is why this cannot simply be "binders shadow what follows them": a
    `where` clause is written AFTER the body it scopes over. Every other
    binder -- `let`, lambda, case alternative -- is written BEFORE the
    body it scopes over, so it can only shadow occurrences that come
    after it, and an occurrence earlier in the same declaration is still
    the import's."""
    start, end = decl
    binders = _binder_positions(code_text, start, end, skip_spans)
    if not binders:
        return None
    first_equals = _first_binding_equals(code_text, start, end)
    where_at = _keyword_position(code_text, start, end, _WHERE_TOKEN)
    earliest_local = None
    for binder in binders:
        if first_equals is not None and binder < first_equals:
            return start
        if where_at is not None and binder > where_at:
            return start
        earliest_local = binder if earliest_local is None \
            else min(earliest_local, binder)
    return earliest_local


def _first_binding_equals(code_text: str, start: int, end: int) -> int | None:
    """The position of the declaration's own `=` -- the one separating
    its head from its body. Read as a maximal symbol run, so `==`, `=>`
    and `>=` are not it."""
    for match in _SYMBOL_RUN.finditer(code_text, start, end):
        if match.group(0) != "=":
            continue
        # The declaration's own `=` sits outside any bracket; one inside
        # a list or a record update belongs to a sub-expression.
        opened = (code_text.count("(", start, match.start())
                  + code_text.count("[", start, match.start()))
        closed = (code_text.count(")", start, match.start())
                  + code_text.count("]", start, match.start()))
        if opened <= closed:
            return match.start()
    return None


def _keyword_position(code_text: str, start: int, end: int,
                      pattern: re.Pattern[str]) -> int | None:
    match = pattern.search(code_text, start, end)
    return match.start() if match else None


def _lines_with_offsets(text: str):
    """`(offset, line)` for every line of `text`."""
    offset = 0
    for line in text.split("\n"):
        yield offset, line
        offset += len(line) + 1


def _separator_after(line: str, column: int) -> bool:
    """True if a binding separator (`=`, `->`, `::`, or the project's
    `→` / `∷`) separates the name at `column` from a body on this line.

    Operators are read as MAXIMAL symbol runs, so `==`, `=>`, `>=` and
    `<-` are correctly not `=` or `->`. Only a separator at bracket
    depth 0 or less counts: one inside a group OPENED after the name --
    the `->` of a nested lambda in `(decodeUtf8 x, (\\y -> y) z)` --
    belongs to that sub-expression, not to a binding of this name, and
    counting it would misread an ordinary use as a binder and silence
    it."""
    depth = 0
    for index, char in enumerate(line[column:]):
        if char in "([":
            depth += 1
        elif char in ")]":
            depth -= 1
        elif depth <= 0 and char in _UNICODE_BINDING_SEPARATORS:
            return True
    for match in _SYMBOL_RUN.finditer(line, column):
        if match.group(0) not in _BINDING_SEPARATORS:
            continue
        segment = line[column:match.start()]
        if (segment.count("(") + segment.count("[")
                <= segment.count(")") + segment.count("]")):
            return True
    return False


def _line_of(text: str, pos: int) -> int:
    return text.count("\n", 0, pos) + 1


def _within(pos: int, spans: list[tuple[int, int]]) -> bool:
    return any(start <= pos < end for start, end in spans)


def find_violations(text: str, rel_path: str) -> list[Violation]:
    """Every use of `Data.Text.Encoding.decodeUtf8` in `text` (the
    source of the file at repo-relative `rel_path`), outside comments,
    string literals, the module header, import declarations and
    `EXEMPTIONS`."""
    if rel_path in EXEMPTIONS:
        return []
    code_text = haskell_code_only(text)
    imports, import_spans = resolve_strict_imports(code_text, rel_path)
    skip_spans = import_spans + _module_header_span(code_text)

    # Where a bare occurrence is shadowed by one of its own
    # declaration's binders, resolved per declaration rather than per
    # file: a binder in one top-level declaration cannot reach into
    # another.
    shadow_from: dict[tuple[int, int], int | None] = {}
    if imports.unqualified:
        for decl in _top_level_declarations(code_text, skip_spans):
            shadow_from[decl] = _shadowed_region(code_text, decl, skip_spans)

    def bare_is_the_import(pos: int) -> bool:
        if not imports.unqualified:
            return False
        for (start, end), from_pos in shadow_from.items():
            if start <= pos < end:
                return from_pos is None or pos < from_pos
        return True

    violations: list[Violation] = []
    for start, end in haskell_code_spans(text):
        for pos, lexeme in _identifier_runs(text, start, end):
            if lexeme != BANNED_IDENTIFIER or _within(pos, skip_spans):
                continue
            qualifier = _qualifier_before(text, pos)
            if qualifier:
                if qualifier not in imports.qualifiers:
                    continue
                spelling = f"{qualifier}.{lexeme}"
            else:
                if not bare_is_the_import(pos):
                    continue
                spelling = lexeme
            violations.append(Violation(rel_path, _line_of(text, pos),
                                        spelling))
    return violations


def scan_tree(repo_root: Path) -> list[Violation]:
    violations: list[Violation] = []
    for path in sorted((repo_root / SCOPED_TREE).glob("**/*.hs")):
        rel = path.relative_to(repo_root).as_posix()
        violations.extend(find_violations(path.read_text(encoding="utf-8"), rel))
    return violations


# ---------------------------------------------------------------------
# Self-test
# ---------------------------------------------------------------------

# Fixtures, never the shipped tree: each is a synthetic module source
# put through the same `find_violations` the audit runs, so the
# self-test proves the DETECTOR rather than restating today's tree.
_TE = "import qualified Data.Text.Encoding as TE\n"

# `(label, source, lines that must be reported in order)`
DETECTED_FIXTURES: list[tuple[str, str, list[int]]] = [
    (
        "the repository-standard `TE.` alias -- the exact shape commit "
        "893cbd8f reintroduced",
        "module M where\n" + _TE
        + "f raw = TE.decodeUtf8 raw\n",
        [3],
    ),
    (
        "a different alias -- renaming the alias must not walk past the "
        "guard",
        "module M where\n"
        "import qualified Data.Text.Encoding as Enc\n"
        "f raw = Enc.decodeUtf8 raw\n",
        [3],
    ),
    (
        "no alias at all, so the qualifier is the module path itself",
        "module M where\n"
        "import qualified Data.Text.Encoding\n"
        "f raw = Data.Text.Encoding.decodeUtf8 raw\n",
        [3],
    ),
    (
        "an unqualified import with no list, and no binding of the name "
        "anywhere -- the bare occurrence can only be the import's",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "f raw = decodeUtf8 raw\n",
        [3],
    ),
    (
        "an unqualified import whose explicit list NAMES the function",
        "module M where\n"
        "import Data.Text.Encoding (decodeUtf8, decodeUtf8Lenient)\n"
        "f raw = decodeUtf8 raw\n",
        [3],
    ),
    (
        "a `hiding` list that hides something ELSE, so the function is "
        "still in unqualified scope",
        "module M where\n"
        "import Data.Text.Encoding hiding (decodeUtf8Lenient)\n"
        "f raw = decodeUtf8 raw\n",
        [3],
    ),
    (
        "an unqualified import with an alias -- both spellings name the "
        "banned function",
        "module M where\n"
        "import Data.Text.Encoding as TE\n"
        "f raw = (TE.decodeUtf8 raw, decodeUtf8 raw)\n",
        [3, 3],
    ),
    (
        "an import list spread over several lines",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "    ( decodeUtf8\n"
        "    , decodeUtf8Lenient )\n"
        "f raw = decodeUtf8 raw\n",
        [5],
    ),
    (
        "an INDENTED top-level layout -- GHC accepts a module body at "
        "any column, and a column-0 import rule would leave the whole "
        "file unparsed and this call unreported",
        "module M where\n"
        " import qualified Data.Text.Encoding as TE\n"
        " f raw = TE.decodeUtf8 raw\n",
        [3],
    ),
    (
        "an indented layout whose import list also continues, so the "
        "declaration ends by column rather than by column 0",
        "module M where\n"
        "  import qualified Data.Text.Encoding as TE\n"
        "      ( decodeUtf8 )\n"
        "  f raw = TE.decodeUtf8 raw\n",
        [4],
    ),
    (
        "an EXPLICIT-BRACE layout with the import inline -- the import "
        "never starts a line at all",
        "module M where { import qualified Data.Text.Encoding as Enc; "
        "f raw = Enc.decodeUtf8 raw }\n",
        [1],
    ),
    (
        "explicit braces over several lines, the import sharing a line "
        "with the declaration before it",
        "module M where {\n"
        "  g = 1; import qualified Data.Text.Encoding as Enc;\n"
        "  f raw = Enc.decodeUtf8 raw }\n",
        [3],
    ),
    (
        "an explicit-brace import list, whose `;` separators must not "
        "end the declaration early",
        "module M where { import qualified Data.Text.Encoding as Enc "
        "(decodeUtf8, decodeUtf8Lenient); f raw = Enc.decodeUtf8 raw }\n",
        [1],
    ),
    (
        "an import indented past a preceding multi-line import: it is a "
        "declaration of its own, and the call it enables is reported",
        "module M where\n"
        "import Other\n"
        "    ( thing )\n"
        "  import qualified Data.Text.Encoding as TE\n"
        "f raw = TE.decodeUtf8 raw\n",
        [5],
    ),
    (
        "MIXED SCOPE: one declaration shadows the name, another does "
        "not -- the unshadowed call is still the import's, and a "
        "whole-file binder check would have missed it",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "bad raw = decodeUtf8 raw\n"
        "local raw = let decodeUtf8 = id in decodeUtf8 raw\n",
        [3],
    ),
    (
        "MIXED SCOPE: within ONE declaration, an occurrence BEFORE a "
        "lambda binder is not in that binder's scope",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "f raw = (decodeUtf8 raw, (\\decodeUtf8 -> decodeUtf8) id)\n",
        [3],
    ),
    (
        "postpositive `qualified` (GHC2024's ImportQualifiedPost) is a "
        "qualified import like any other, and its uses are reported",
        "module M where\n"
        "import Data.Text.Encoding qualified as TE\n"
        "f raw = TE.decodeUtf8 raw\n",
        [3],
    ),
    (
        "postpositive `qualified` with no alias",
        "module M where\n"
        "import Data.Text.Encoding qualified\n"
        "f raw = Data.Text.Encoding.decodeUtf8 raw\n",
        [3],
    ),
    (
        "two strict calls on one line are both reported",
        "module M where\n" + _TE
        + "f a b = (TE.decodeUtf8 a, TE.decodeUtf8 b)\n",
        [3, 3],
    ),
    (
        "a real strict call on a line that also carries a comment "
        "mentioning it -- the comment is skipped, the code is not",
        "module M where\n" + _TE
        + "f raw = TE.decodeUtf8 raw  -- TE.decodeUtf8 again, in prose\n",
        [3],
    ),
    (
        "a char literal `'\"'` before a strict call -- the lexer must "
        "not read it as opening a string and swallow the call",
        "module M where\n" + _TE
        + "f raw = (q, TE.decodeUtf8 raw) where q = '\"'\n",
        [3],
    ),
]

CLEAN_FIXTURES: list[tuple[str, str]] = [
    (
        "the total sibling `decodeUtf8Lenient` -- the compliant fix",
        "module M where\n" + _TE
        + "f raw = TE.decodeUtf8Lenient raw\n",
    ),
    (
        "the total sibling `decodeUtf8'`",
        "module M where\n" + _TE
        + "f raw = either (const \"\") id (TE.decodeUtf8' raw)\n",
    ),
    (
        "the total sibling `decodeUtf8With`",
        "module M where\n" + _TE
        + "import qualified Data.Text.Encoding.Error as TEE\n"
        + "f raw = TE.decodeUtf8With TEE.lenientDecode raw\n",
    ),
    (
        "a `--` line comment naming the banned call",
        "module M where\n" + _TE
        + "-- Was TE.decodeUtf8 raw before #1605.\n"
        + "f raw = TE.decodeUtf8Lenient raw\n",
    ),
    (
        "a `{- -}` block comment naming it, nested",
        "module M where\n" + _TE
        + "{- never TE.decodeUtf8 {- not even here -} -}\n"
        + "f raw = TE.decodeUtf8Lenient raw\n",
    ),
    (
        "a string literal containing the banned call",
        "module M where\n" + _TE
        + "warning = \"replaced TE.decodeUtf8 with the lenient sibling\"\n"
        + "f raw = TE.decodeUtf8Lenient raw\n",
    ),
    (
        "a longer identifier merely ENDING in the banned one",
        "module M where\n" + _TE
        + "f raw = TE.myDecodeUtf8 raw\n",
    ),
    (
        "an import with no use -- an import decodes nothing; the USE it "
        "enables is what fails",
        "module M where\n"
        "import Data.Text.Encoding (decodeUtf8)\n"
        "f raw = raw\n",
    ),
    # --- provenance: a same-named function from somewhere else is a
    #     DIFFERENT function, and widening the ban to it is a separate
    #     decision from #1605's.
    (
        "PROVENANCE: Data.Text.Lazy.Encoding's own decodeUtf8 -- a "
        "different module, outside this convention's scope",
        "module M where\n"
        "import qualified Data.Text.Lazy.Encoding as TLE\n"
        "f raw = TLE.decodeUtf8 raw\n",
    ),
    (
        "PROVENANCE: an unrelated module aliased to the very alias this "
        "tree uses for the banned one",
        "module M where\n"
        "import qualified Data.Text.Lazy.Encoding as TE\n"
        "f raw = TE.decodeUtf8 raw\n",
    ),
    (
        "PROVENANCE: an unrelated module's total decodeUtf8",
        "module M where\n"
        "import qualified Other.Codec as Other\n"
        "f raw = Other.decodeUtf8 raw\n",
    ),
    (
        "PROVENANCE: a local binding of that name in a file that never "
        "imports the module",
        "module M where\n"
        "decodeUtf8 :: ByteString -> Text\n"
        "decodeUtf8 = myOwnTotalThing\n"
        "f raw = decodeUtf8 raw\n",
    ),
    (
        "PROVENANCE: a bare use where the module is imported QUALIFIED "
        "only, so the bare name cannot be its export",
        "module M where\n" + _TE
        + "f raw = decodeUtf8 raw\n",
    ),
    (
        "PROVENANCE: the module imported unqualified but `hiding` the "
        "function, so the bare name is not its export",
        "module M where\n"
        "import Data.Text.Encoding hiding (decodeUtf8)\n"
        "f raw = decodeUtf8 raw\n",
    ),
    (
        "PROVENANCE: an explicit import list that omits the function",
        "module M where\n"
        "import Data.Text.Encoding (decodeUtf8Lenient)\n"
        "f raw = decodeUtf8 raw\n",
    ),
    (
        "PROVENANCE: Data.Text.Encoding.Error is not Data.Text.Encoding",
        "module M where\n"
        "import qualified Data.Text.Encoding.Error as TEE\n"
        "f raw = TEE.decodeUtf8 raw\n",
    ),
    # --- shadowing: a nested binder legally captures the bare name even
    #     under an UNQUALIFIED import, so those occurrences are not the
    #     import's and must not be blamed.
    (
        "SHADOWING: an unqualified import plus a `where` binding of the "
        "same name -- the call is the local one",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "f raw = decodeUtf8 raw\n"
        "  where decodeUtf8 = myOwnTotalThing\n",
    ),
    (
        "SHADOWING: an unqualified import plus a `let` binding",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "f raw = let decodeUtf8 = myOwnTotalThing in decodeUtf8 raw\n",
    ),
    (
        "SHADOWING: an unqualified import plus a parameter of that name",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "f decodeUtf8 raw = decodeUtf8 raw\n",
    ),
    (
        "SHADOWING: an unqualified import plus a lambda binder",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "f raw = (\\decodeUtf8 -> decodeUtf8 raw) myOwnTotalThing\n",
    ),
    (
        "SHADOWING: an unqualified import plus a case-alternative "
        "pattern on its own line",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "f raw = case g of\n"
        "    decodeUtf8 -> decodeUtf8 raw\n",
    ),
    (
        "postpositive `qualified` with only a COMPLIANT sibling in use "
        "-- the form must parse, not raise",
        "module M where\n"
        "import Data.Text.Encoding qualified as TE\n"
        "f raw = TE.decodeUtf8Lenient raw\n",
    ),
    (
        "postpositive `qualified` means the bare name is NOT in scope",
        "module M where\n"
        "import Data.Text.Encoding qualified as TE\n"
        "f raw = decodeUtf8 raw\n",
    ),
    (
        "a module header whose export list names the function -- an "
        "export names an entity, it does not use one",
        "module M (decodeUtf8) where\n"
        "import Data.Text.Encoding\n"
        "f raw = raw\n",
    ),
    (
        "the composition `f.decodeUtf8` is a bare use, not a qualified "
        "one -- and stays clean under a qualified import",
        "module M where\n" + _TE
        + "f = g.decodeUtf8\n",
    ),
]

# A `Data.Text.Encoding` import this parser cannot classify must RAISE,
# not be scanned as though the module were absent.
UNMODELLED_FIXTURES: list[tuple[str, str]] = [
    (
        "a package-qualified import shape the parser does not model",
        "module M where\n"
        "import \"text\" qualified Data.Text.Encoding as TE\n"
        "f raw = TE.decodeUtf8 raw\n",
    ),
    (
        "an unterminated import list",
        "module M where\n"
        "import Data.Text.Encoding (decodeUtf8\n"
        "f raw = decodeUtf8 raw\n",
    ),
]


def _exemptions_missing_reasons() -> list[str]:
    """Requirement 6: every exemption states its reason inline."""
    return [path for path, reason in EXEMPTIONS.items()
            if not isinstance(reason, str) or not reason.strip()]


def self_test() -> int:
    failures: list[str] = []

    for label, source, expected in DETECTED_FIXTURES:
        got = [v.line for v in find_violations(source, "fixture.hs")]
        if got != expected:
            failures.append(f"  DETECTED: {label}\n"
                            f"    expected violations on lines {expected}, "
                            f"got {got}")

    for label, source in CLEAN_FIXTURES:
        got = [str(v) for v in find_violations(source, "fixture.hs")]
        if got:
            failures.append(f"  CLEAN: {label}\n"
                            f"    expected no violations, got {got}")

    for label, source in UNMODELLED_FIXTURES:
        try:
            find_violations(source, "fixture.hs")
        except _ImportParseError:
            continue
        failures.append(f"  UNMODELLED: {label}\n"
                        f"    expected _ImportParseError, got a clean scan")

    # An exemption must actually suppress, and suppress only its path,
    # or the table is decorative.
    exempt_probe = "module M where\n" + _TE + "f raw = TE.decodeUtf8 raw\n"
    EXEMPTIONS["exempt.hs"] = "self-test probe"
    try:
        if find_violations(exempt_probe, "exempt.hs"):
            failures.append("  EXEMPTIONS: an exempt path was still flagged")
        if not find_violations(exempt_probe, "fixture.hs"):
            failures.append("  EXEMPTIONS: exempting one path silenced another")
    finally:
        del EXEMPTIONS["exempt.hs"]

    for path in _exemptions_missing_reasons():
        failures.append(f"  EXEMPTIONS['{path}'] carries no inline reason")

    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for failure in failures:
            print(failure)
        return 1
    total = (len(DETECTED_FIXTURES) + len(CLEAN_FIXTURES)
             + len(UNMODELLED_FIXTURES))
    print(f"lua_strict_decode_audit self-test: {total} fixtures OK "
          f"({len(DETECTED_FIXTURES)} detected across every import, "
          f"qualification and layout form; {len(CLEAN_FIXTURES)} clean, "
          f"including the total siblings, comments, literals, "
          f"same-named functions from other modules and shadowed bare "
          f"names; {len(UNMODELLED_FIXTURES)} unmodelled imports "
          f"that raise rather than pass).")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--self-test", action="store_true",
                        help="run the fixture self-test instead of the tree "
                             "audit; touches no shipped source")
    args = parser.parse_args()
    if args.self_test:
        return self_test()

    try:
        violations = scan_tree(REPO_ROOT)
    except _ImportParseError as error:
        print(f"lua_strict_decode_audit: {error}")
        return 1
    if violations:
        print(f"{len(violations)} strict "
              f"`{STRICT_MODULE}.{BANNED_IDENTIFIER}` use(s) under "
              f"{SCOPED_TREE}/:")
        for violation in violations:
            print(f"  {violation}")
        print(f"\nLua strings are arbitrary byte arrays (#665, #618): a "
              f"strict decode throws UnicodeException on malformed input "
              f"instead of letting the call site refuse the value. Use a "
              f"total sibling -- {', '.join(TOTAL_SIBLINGS)}.")
        if EXEMPTIONS:
            print("\nExempt by design:")
            for path, reason in EXEMPTIONS.items():
                print(f"  {path}: {reason}")
        return 1
    print(f"No strict `{STRICT_MODULE}.{BANNED_IDENTIFIER}` use found under "
          f"{SCOPED_TREE}/.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
