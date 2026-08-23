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

WHAT IS DETECTED: a QUALIFIED reference to that one module's export, in
code under the scoped tree. A reference is qualified when it is written
`<qualifier>.decodeUtf8` and the qualifier resolves, through the file's
OWN import declarations, to exactly `Data.Text.Encoding`. Every alias
is covered because the alias comes from the file itself --
`TE.decodeUtf8` under the repository's standard one, `Enc.decodeUtf8`
under any other, the unaliased `Data.Text.Encoding.decodeUtf8` -- with
`qualified` written before the module name or after it (GHC2024 enables
`ImportQualifiedPost`). Renaming the alias cannot walk past this.

A same-named function from ANYWHERE ELSE is never caught, because it is
not the banned one: `Data.Text.Lazy.Encoding.decodeUtf8`, an unrelated
`Other.decodeUtf8`, or a local binding of that name.

QUALIFIED REFERENCES ARE THE WHOLE DECIDABLE DOMAIN, AND THAT IS
DELIBERATE (#1605 requirement 3). A module qualifier can never be
captured by a term binder, so a qualified hit is unambiguous by
construction. A BARE `decodeUtf8` is not analysed at all: deciding
whether one is an unqualified import's export or a nested binder that
legally shadows it -- a parameter, a `let`/`where` binding, a lambda or
case pattern -- needs real Haskell scope analysis (layout in three
flavours, binding groups, guards, view patterns, record syntax), and a
partial attempt is worse than none because it SILENCES real uses. PR
#1623 spent ten review rounds proving that one construct at a time.

So instead of guessing, this guard REFUSES what it cannot decide
(requirement 4). Three causes, each exiting non-zero with the file, the
cause and the remedy:

  * an import naming `Data.Text.Encoding` in a shape it does not model;

  * an UNQUALIFIED import of that module -- with or without an alias, an
    explicit list, or a `hiding` clause -- since that is exactly the
    case whose call sites cannot be resolved;

  * an identifier-rewriting CPP directive (`#define`, `#undef`,
    `#include`), since this reads UNPREPROCESSED source and a macro can
    rename the very alias it resolves by. Conditional directives
    (`#if`, `#ifdef`) only SELECT text, so a scan that reads every
    branch over-reports at worst and they are not a cause.

A refusal is not a claim that the file decodes strictly; it says the
file cannot be certified as written. The remedy is a qualified import --
which every one of the 90 `Data.Text.Encoding` imports in `src/` and
`app/` already is, 88 of them under the scoped tree and the other two in
`src/UPrelude.hs` and `src/Unit/Atlas/Digest.hs` -- or an `EXEMPTIONS`
entry carrying its reason.

Two properties hold throughout. Candidates are maximal identifier
LEXEMES, so the total siblings are safe by construction
(`decodeUtf8Lenient` is a DIFFERENT identifier, never a `decodeUtf8`
followed by something, and `myDecodeUtf8` is different again). And
comment and string-literal awareness comes from
`tools/unicode_operator_audit.py`'s lexer via its public
`haskell_code_spans` / `haskell_code_only`, rather than a second copy
free to drift from it.

Usage:
  python3 tools/lua_strict_decode_audit.py              # audit the tree
  python3 tools/lua_strict_decode_audit.py --self-test  # fixtures only
Exit codes: 0 = clean, 1 = a strict use was found or a file could not be
certified (or, under --self-test, a fixture behaved wrongly).
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

# Exemptions: repo-relative path -> the reason it is exempt, stated
# inline the way tools/unicode_operator_audit.py states its own
# (requirement 7). Whole-file, because there is no construct-scoped case
# to carve out here. This is also the recorded escape for a file the
# guard refuses. A clean tree needs none, and the checked-in table is
# empty.
EXEMPTIONS: dict[str, str] = {}

def _is_ident_char(char: str) -> bool:
    """True for a character a Haskell variable, constructor or module
    identifier is made of (report SS2.4, as GHC extends it): a letter, a
    digit, `_`, or `'`.

    Unicode-aware on purpose. GHC accepts non-ASCII letters in names, so
    `import qualified Data.Text.Encoding as TE\u00c9` is a valid alias --
    and an ASCII-only lexer would stop mid-qualifier at the `\u00c9`,
    read `TE\u00c9.decodeUtf8` as unqualified, and miss the call. The
    same reasoning is why `tools/unicode_operator_audit.py` resolves ITS
    qualifiers with Unicode-aware `\\w` and `str.isupper()` rather than
    an ASCII class."""
    return char.isalnum() or char in "_'"

# `STRICT_MODULE` named as a whole module path -- not as the prefix of a
# longer one, so `Data.Text.Encoding.Error` is correctly NOT a mention.
_MENTIONS_STRICT_MODULE = re.compile(
    r"(?<![\w'.])" + re.escape(STRICT_MODULE) + r"(?![\w'.])")

# The import shapes this module understands, per the Haskell 2010 report
# SS5.3 plus GHC2024's ImportQualifiedPost: `import [qualified]
# Data.Text.Encoding [qualified] [as Alias] [hiding] [( ... )]`.
# Anything else that names the module is a parse failure, not a pass.
_STRICT_IMPORT = re.compile(
    r"\Aimport\s+(?P<prequalified>qualified\s+)?"
    + re.escape(STRICT_MODULE) + r"(?![\w'.])"
    r"(?P<postqualified>\s+qualified(?![\w']))?"
    r"(?:\s+as\s+(?P<alias>\w[\w']*(?:\.\w[\w']*)*))?"
    r"(?P<rest>[\s\S]*)\Z")

# `import` as a whole token, wherever it sits.
_IMPORT_TOKEN = re.compile(r"(?<![\w'])import(?![\w'])")

# CPP directives that can rewrite identifiers before GHC ever sees them,
# or hide text from this scan. Matched on the RAW file text, not the
# comment-masked copy: CPP runs before Haskell is parsed, so a `#define`
# inside what looks like a Haskell comment is still a real directive.
_REWRITING_CPP_DIRECTIVE = re.compile(
    r"^[ \t]*#[ \t]*(define|undef|include)\b", re.MULTILINE)


class _UnscannableSource(Exception):
    """Source this guard cannot certify, for any reason.

    Raised rather than swallowed, always. Every alternative -- scanning
    a file whose imports were not understood, one whose bare names it
    cannot resolve, or one whose text the compiler will rewrite before
    it sees it -- makes the uses in that file invisible, which is the
    one failure mode a guard must not have."""


class _ImportParseError(_UnscannableSource):
    """A `Data.Text.Encoding` import this module cannot classify."""


class _UnqualifiedImportError(_UnscannableSource):
    """An UNQUALIFIED import of `Data.Text.Encoding`.

    Its call sites are bare names, which cannot be told from a local
    binding that legally shadows them without full scope analysis -- see
    the module docstring. Refused as a whole rather than guessed at, and
    refused whatever else the declaration carries: an alias, an explicit
    import list, or a `hiding` clause all still put the module's names
    in unqualified scope."""


class _PreprocessorError(_UnscannableSource):
    """A file whose text CPP will rewrite before GHC parses it."""


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


# ---------------------------------------------------------------------
# Import resolution
# ---------------------------------------------------------------------

def _opens_declaration(code_text: str, start: int) -> bool:
    """True if the `import` token at `start` opens a declaration rather
    than sitting inside one.

    It does when only spaces separate it from the start of its line, or
    from an explicit-layout `{` or `;`. Those are the only positions the
    Haskell grammar allows a declaration to begin at, in either layout
    style -- which is why declarations are found by TOKEN here and never
    by column: GHC accepts a top-level layout indented to any column,
    and an explicit-brace module body puts imports inline. Where the
    declaration then ENDS depends on which layout it is in; see
    `_declaration_end`."""
    i = start - 1
    while i >= 0 and code_text[i] in " \t":
        i -= 1
    return i < 0 or code_text[i] in "\n{;"


def _in_explicit_layout(code_text: str, pos: int) -> bool:
    """True if `pos` sits inside an explicit-layout `{ ... }` block.

    Any brace ENCLOSING an import is a layout brace: record syntax is
    one expression and cannot contain a declaration, so a record's
    braces are always balanced before one. Counting all braces is
    therefore both simple and exact for this question."""
    return code_text.count("{", 0, pos) > code_text.count("}", 0, pos)


def _declaration_end(code_text: str, pos: int, column: int,
                     explicit: bool) -> int:
    """Where the declaration continuing from `pos` ends.

    In an EXPLICIT layout it ends only at the `;` or `}` that closes it;
    indentation carries no meaning there, so a declaration may wrap
    across lines at any column and `module M where { import qualified\n
    Data.Text.Encoding as TE; ... }` is one import. In an IMPLICIT
    layout it ends at the first line indented no further than its own
    opening `column`, or at a `;`/`}`, whichever comes first.

    Bracket depth is tracked either way, so a `;` inside an import list
    cannot end the declaration early. In the implicit case a
    continuation line can never itself be the reserved word `import`, so
    one that is starts a fresh declaration however deeply it is
    indented."""
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
        elif char == "\n" and not explicit:
            line_start = i + 1
            j = line_start
            while j < n and code_text[j] in " \t":
                j += 1
            if j < n and code_text[j] != "\n":
                if (j - line_start) <= column or _IMPORT_TOKEN.match(code_text, j):
                    return i
        i += 1
    return n


def _import_declarations(code_text: str) -> list[tuple[int, int, str]]:
    """Every import declaration in `code_text` as `(start, end, text)`.

    `code_text` must already be comment- and string-masked, so a comment
    quoting an import is never mistaken for one."""
    decls: list[tuple[int, int, str]] = []
    for match in _IMPORT_TOKEN.finditer(code_text):
        start = match.start()
        if not _opens_declaration(code_text, start):
            continue
        column = start - (code_text.rfind("\n", 0, start) + 1)
        end = _declaration_end(code_text, match.end(), column,
                               _in_explicit_layout(code_text, start))
        decls.append((start, end, code_text[start:end]))
    return decls


def _classify_strict_import(decl: str, rel_path: str, line: int) -> str:
    """The module qualifier a `STRICT_MODULE` import establishes.

    Raises `_UnqualifiedImportError` when the import is not qualified,
    and `_ImportParseError` when it names the module in a shape this
    parser does not model."""
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
    if rest.startswith("hiding"):
        rest = rest[len("hiding"):].strip()
    if rest.startswith("("):
        if not rest.endswith(")"):
            raise _ImportParseError(
                f"{rel_path}:{line}: unterminated import list on this "
                f"{STRICT_MODULE} import:\n\n{decl.strip()}")
    elif rest:
        raise _ImportParseError(
            f"{rel_path}:{line}: unexpected trailing text on this "
            f"{STRICT_MODULE} import:\n\n{decl.strip()}")

    qualified = (match.group("prequalified") is not None
                 or match.group("postqualified") is not None)
    if not qualified:
        raise _UnqualifiedImportError(
            f"{rel_path}:{line}: this import puts {STRICT_MODULE}'s names in "
            f"UNQUALIFIED scope:\n\n{decl.strip()}\n\n"
            f"tools/lua_strict_decode_audit.py resolves {BANNED_IDENTIFIER} "
            f"by module qualifier (#1605 requirement 3). A BARE occurrence "
            f"cannot be told from a local binding that legally shadows it "
            f"without full Haskell scope analysis, so this file cannot be "
            f"certified as written -- which is not a claim that it decodes "
            f"strictly.\n\n"
            f"Import the module qualified, as every {STRICT_MODULE} import "
            f"in src/ and app/ already is, or record this file in "
            f"EXEMPTIONS with the reason it is safe.")

    alias = match.group("alias")
    if alias is None:
        return STRICT_MODULE
    # A module alias is a modid: every dot-separated segment is
    # uppercase-led. The pattern above is Unicode-aware rather than
    # `[A-Z]`, so the check is here, where `str.isupper()` covers any
    # alphabet -- and anything that is not a modid did not come from
    # source GHC accepts.
    if not all(seg and seg[0].isupper() for seg in alias.split(".")):
        raise _ImportParseError(
            f"{rel_path}:{line}: the `as` clause of this {STRICT_MODULE} "
            f"import does not name a module (every segment of a module "
            f"alias is uppercase-led):\n\n{decl.strip()}")
    return alias


def resolve_strict_imports(
        code_text: str,
        rel_path: str) -> tuple[frozenset[str], list[tuple[int, int]]]:
    """`(qualifiers naming the banned function, import spans)`.

    The spans are returned so the call-site scan can skip the import
    declarations themselves: an import names nothing on its own, and the
    USE it enables is what this audit reports."""
    qualifiers: set[str] = set()
    spans: list[tuple[int, int]] = []
    for start, end, decl in _import_declarations(code_text):
        spans.append((start, end))
        if not _MENTIONS_STRICT_MODULE.search(decl):
            continue
        line = code_text.count("\n", 0, start) + 1
        qualifiers.add(_classify_strict_import(decl, rel_path, line))
        # The unaliased module path always names this module
        # unambiguously wherever it is in scope at all, so accept it
        # alongside whatever alias was declared. On code that compiles
        # this can only ever be the banned function.
        qualifiers.add(STRICT_MODULE)
    return frozenset(qualifiers), spans


# ---------------------------------------------------------------------
# Call-site scan
# ---------------------------------------------------------------------

def _identifier_runs(text: str, start: int, end: int):
    """Every maximal identifier lexeme in `text[start:end)`, as
    `(position, lexeme)`."""
    i = start
    while i < end:
        if not _is_ident_char(text[i]):
            i += 1
            continue
        run_start = i
        while i < end and _is_ident_char(text[i]):
            i += 1
        yield run_start, text[run_start:i]


def _qualifier_before(text: str, pos: int) -> str:
    """The module qualifier written immediately before `pos` (`"TE"`,
    `"Data.Text.Encoding"`), or `""` for an unqualified occurrence.

    A qualifier is written with no space before the `.`, so it is the
    `[\\w'.]` run ending in `.` directly ahead of the name -- and only
    when every one of its dot-separated segments is uppercase-led, which
    is what separates `TE.decodeUtf8` from the composition
    `f.decodeUtf8`.

    A leading Template Haskell name quote is stripped first. `'` is an
    identifier CHARACTER in Haskell (`map'`), so it lands inside that
    run, and `$(varE 'TE.decodeUtf8)` would otherwise present the
    segment `'TE` -- not uppercase-led, hence read as no qualifier at
    all. The splice names and invokes the very function this guards, so
    the quote must not hide it. `''Name` (the type-level quote) is
    stripped the same way."""
    start = pos
    while start > 0 and (_is_ident_char(text[start - 1])
                         or text[start - 1] == "."):
        start -= 1
    candidate = text[start:pos]
    if not candidate.endswith("."):
        return ""
    candidate = candidate.lstrip("'")
    if not candidate:
        return ""
    segments = candidate[:-1].split(".")
    if segments and all(seg and seg[0].isupper() for seg in segments):
        return candidate[:-1]
    return ""


def _line_of(text: str, pos: int) -> int:
    return text.count("\n", 0, pos) + 1


def _within(pos: int, spans: list[tuple[int, int]]) -> bool:
    return any(start <= pos < end for start, end in spans)


def find_violations(text: str, rel_path: str) -> list[Violation]:
    """Every qualified use of `Data.Text.Encoding.decodeUtf8` in `text`
    (the source of the file at repo-relative `rel_path`), outside
    comments, string literals and import declarations.

    Nothing at all for a path in `EXEMPTIONS`. Raises
    `_UnscannableSource` for a file this guard cannot certify."""
    if rel_path in EXEMPTIONS:
        return []
    directive = _REWRITING_CPP_DIRECTIVE.search(text)
    if directive:
        line = text.count("\n", 0, directive.start()) + 1
        raise _PreprocessorError(
            f"{rel_path}:{line}: this file carries a CPP "
            f"`{directive.group(1)}` directive, and "
            f"tools/lua_strict_decode_audit.py reads UNPREPROCESSED source. "
            f"A macro can rename the very module alias this scan resolves "
            f"{BANNED_IDENTIFIER} by, so the file cannot be certified as "
            f"written.\n\n"
            f"Either drop the directive, or record the file in EXEMPTIONS "
            f"with the reason it is safe. Conditional directives "
            f"(#if / #ifdef) only select text and are not reported.")

    code_text = haskell_code_only(text)
    qualifiers, import_spans = resolve_strict_imports(code_text, rel_path)

    violations: list[Violation] = []
    for start, end in haskell_code_spans(text):
        for pos, lexeme in _identifier_runs(text, start, end):
            if lexeme != BANNED_IDENTIFIER or _within(pos, import_spans):
                continue
            qualifier = _qualifier_before(text, pos)
            if qualifier and qualifier in qualifiers:
                violations.append(
                    Violation(rel_path, _line_of(text, pos),
                              f"{qualifier}.{lexeme}"))
    return violations


def scan_tree(repo_root: Path) -> list[Violation]:
    violations: list[Violation] = []
    for path in sorted((repo_root / SCOPED_TREE).glob("**/*.hs")):
        rel = path.relative_to(repo_root).as_posix()
        violations.extend(find_violations(path.read_text(encoding="utf-8"), rel))
    return violations


# ---------------------------------------------------------------------
# Self-test (requirement 5)
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
        "postpositive `qualified` (GHC2024's ImportQualifiedPost)",
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
        "a qualified import with an explicit list",
        "module M where\n"
        "import qualified Data.Text.Encoding as TE (decodeUtf8)\n"
        "f raw = TE.decodeUtf8 raw\n",
        [3],
    ),
    (
        "an import list spread over several lines",
        "module M where\n"
        "import qualified Data.Text.Encoding as TE\n"
        "    ( decodeUtf8\n"
        "    , decodeUtf8Lenient )\n"
        "f raw = TE.decodeUtf8 raw\n",
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
        "EXPLICIT LAYOUT: an import wrapped across lines at column 0 -- "
        "indentation carries no meaning inside braces, so the "
        "declaration runs to its `;` and not to the next line",
        "module M where { import qualified\n"
        "Data.Text.Encoding as TE;\n"
        "f raw = TE.decodeUtf8 raw }\n",
        [3],
    ),
    (
        "EXPLICIT LAYOUT: the same wrap with the alias and an import "
        "list on further lines",
        "module M where { import qualified\n"
        "Data.Text.Encoding\n"
        "as TE\n"
        "( decodeUtf8 );\n"
        "f raw = TE.decodeUtf8 raw }\n",
        [5],
    ),
    (
        "EXPLICIT LAYOUT: a nested `do` block's braces do not make the "
        "module body explicit for a LATER import",
        "module M where\n"
        "g = do { pure () }\n"
        "import qualified Data.Text.Encoding as TE\n"
        "f raw = TE.decodeUtf8 raw\n",
        [4],
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
    (
        "UNICODE ALIAS: GHC accepts non-ASCII letters in names, so an "
        "ASCII-only qualifier lexer would stop mid-alias and read this "
        "as unqualified",
        "module M where\n"
        "import qualified Data.Text.Encoding as T\u00c9\n"
        "f raw = T\u00c9.decodeUtf8 raw\n",
        [3],
    ),
    (
        "UNICODE ALIAS: one whose FIRST letter is non-ASCII uppercase",
        "module M where\n"
        "import qualified Data.Text.Encoding as \u00c9nc\n"
        "f raw = \u00c9nc.decodeUtf8 raw\n",
        [3],
    ),
    (
        "UNICODE ALIAS: a multi-segment one",
        "module M where\n"
        "import qualified Data.Text.Encoding as T\u00c9.Inner\n"
        "f raw = T\u00c9.Inner.decodeUtf8 raw\n",
        [3],
    ),
    (
        "TEMPLATE HASKELL: a quoted name `'TE.decodeUtf8` -- `'` is an "
        "identifier character, so the quote lands inside the qualifier "
        "run and must be stripped, or the splice that invokes the "
        "decoder goes unreported",
        "module M where\n" + _TE
        + "f raw = $(varE 'TE.decodeUtf8) raw\n",
        [3],
    ),
    (
        "TEMPLATE HASKELL: a quoted fully qualified name",
        "module M where\n"
        "import qualified Data.Text.Encoding\n"
        "f raw = $(varE 'Data.Text.Encoding.decodeUtf8) raw\n",
        [3],
    ),
    (
        "TEMPLATE HASKELL: the type-level `''` quote is stripped too",
        "module M where\n" + _TE
        + "f = $(reify ''TE.decodeUtf8)\n",
        [3],
    ),
    (
        "a qualified call inside a guard, a record and a nested layout "
        "block -- the scan needs no notion of any of them",
        "module M where\n" + _TE
        + "f raw | p (TE.decodeUtf8 raw) = do { g R { v = TE.decodeUtf8 raw } }\n",
        [3, 3],
    ),
    (
        "a qualified call in a view pattern, likewise",
        "module M where\n" + _TE
        + "f (TE.decodeUtf8 -> _) raw = raw\n",
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
        "postpositive `qualified` with only a compliant sibling in use "
        "-- the form must parse, not raise",
        "module M where\n"
        "import Data.Text.Encoding qualified as TE\n"
        "f raw = TE.decodeUtf8Lenient raw\n",
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
        "an import with no use -- an import names nothing on its own; "
        "the USE it enables is what fails",
        "module M where\n"
        "import qualified Data.Text.Encoding as TE (decodeUtf8)\n"
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
        "PROVENANCE: Data.Text.Encoding.Error is not Data.Text.Encoding",
        "module M where\n"
        "import qualified Data.Text.Encoding.Error as TEE\n"
        "f raw = TEE.decodeUtf8 raw\n",
    ),
    # --- bare names are outside the decidable domain (requirement 3);
    #     with no unqualified import they are simply somebody else's.
    (
        "BARE: a local binding of that name in a file that never "
        "imports the module",
        "module M where\n"
        "decodeUtf8 :: ByteString -> Text\n"
        "decodeUtf8 = myOwnTotalThing\n"
        "f raw = decodeUtf8 raw\n",
    ),
    (
        "BARE: a bare name where the module is imported QUALIFIED only, "
        "so it cannot be that module's export",
        "module M where\n" + _TE
        + "f raw = decodeUtf8 raw\n",
    ),
    (
        "UNICODE ALIAS: a compliant sibling behind one stays clean",
        "module M where\n"
        "import qualified Data.Text.Encoding as T\u00c9\n"
        "f raw = T\u00c9.decodeUtf8Lenient raw\n",
    ),
    (
        "UNICODE ALIAS: another module behind a non-ASCII alias is "
        "still another module",
        "module M where\n"
        "import qualified Data.Text.Lazy.Encoding as T\u00c9\n"
        "f raw = T\u00c9.decodeUtf8 raw\n",
    ),
    (
        "UNICODE: a non-ASCII identifier merely CONTAINING the banned "
        "name is a different lexeme",
        "module M where\n" + _TE
        + "f raw = TE.decodeUtf8\u00c9 raw\n",
    ),
    (
        "TEMPLATE HASKELL: a quoted COMPLIANT sibling stays clean",
        "module M where\n" + _TE
        + "f raw = $(varE 'TE.decodeUtf8Lenient) raw\n",
    ),
    (
        "TEMPLATE HASKELL: a quoted name from another module stays "
        "clean -- stripping the quote does not widen the target",
        "module M where\n"
        "import qualified Data.Text.Lazy.Encoding as TLE\n"
        "f raw = $(varE 'TLE.decodeUtf8) raw\n",
    ),
    (
        "BARE: the composition `f.decodeUtf8` is a bare name, not a "
        "qualified one",
        "module M where\n" + _TE
        + "f = g.decodeUtf8\n",
    ),
    (
        "CPP: a conditional directive only SELECTS text, so it is not a "
        "refusal and the file still scans",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n" + _TE
        + "#ifdef DARWIN\n"
        + "f raw = TE.decodeUtf8Lenient raw\n"
        + "#endif\n",
    ),
]

# Source this guard cannot certify must RAISE (requirement 4).
UNSCANNABLE_FIXTURES: list[tuple[str, str, type]] = [
    (
        "an UNQUALIFIED import: its call sites are bare names, which is "
        "the case requirement 3 leaves undecided",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "f raw = decodeUtf8 raw\n",
        _UnqualifiedImportError,
    ),
    (
        "an unqualified import WITH AN ALIAS -- still unqualified scope",
        "module M where\n"
        "import Data.Text.Encoding as TE\n"
        "f raw = TE.decodeUtf8Lenient raw\n",
        _UnqualifiedImportError,
    ),
    (
        "an unqualified import with an EXPLICIT LIST",
        "module M where\n"
        "import Data.Text.Encoding (decodeUtf8Lenient)\n"
        "f raw = decodeUtf8Lenient raw\n",
        _UnqualifiedImportError,
    ),
    (
        "an unqualified import with a `hiding` clause -- hiding one "
        "name still puts the rest in unqualified scope",
        "module M where\n"
        "import Data.Text.Encoding hiding (decodeUtf8)\n"
        "f raw = decodeUtf8Lenient raw\n",
        _UnqualifiedImportError,
    ),
    (
        "an `as` clause that does not name a module -- a modid is "
        "uppercase-led, so this did not come from source GHC accepts",
        "module M where\n"
        "import qualified Data.Text.Encoding as te\n"
        "f raw = te.decodeUtf8 raw\n",
        _ImportParseError,
    ),
    (
        "a package-qualified import shape the parser does not model",
        "module M where\n"
        "import \"text\" qualified Data.Text.Encoding as TE\n"
        "f raw = TE.decodeUtf8 raw\n",
        _ImportParseError,
    ),
    (
        "an unterminated import list",
        "module M where\n"
        "import qualified Data.Text.Encoding as TE (decodeUtf8\n"
        "f raw = TE.decodeUtf8 raw\n",
        _ImportParseError,
    ),
    (
        "CPP: a `#define` that renames the very alias the scan resolves "
        "by -- the compiler sees `TE`, an unpreprocessed scan sees "
        "`TextEnc`, and the call resolves against neither",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n"
        "#define TextEnc TE\n"
        "import qualified Data.Text.Encoding as TextEnc\n"
        "f raw = TE.decodeUtf8 raw\n",
        _PreprocessorError,
    ),
    (
        "CPP: a `#define` inside what looks like a Haskell comment is "
        "still a real directive, because CPP runs first",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n"
        "{- harmless?\n"
        "#define TextEnc TE\n"
        "-}\n"
        "import qualified Data.Text.Encoding as TextEnc\n"
        "f raw = TE.decodeUtf8 raw\n",
        _PreprocessorError,
    ),
    (
        "CPP: `#undef`, which can un-rename just as destructively",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n"
        "#undef TextEnc\n" + _TE
        + "f raw = TE.decodeUtf8Lenient raw\n",
        _PreprocessorError,
    ),
    (
        "CPP: `#include`, whose text this scan never sees",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n"
        "#include \"aliases.h\"\n" + _TE
        + "f raw = TE.decodeUtf8Lenient raw\n",
        _PreprocessorError,
    ),
]


def _exemptions_missing_reasons() -> list[str]:
    """Requirement 7: every exemption states its reason inline."""
    return [path for path, reason in EXEMPTIONS.items()
            if not isinstance(reason, str) or not reason.strip()]


def self_test() -> int:
    failures: list[str] = []

    for label, source, expected in DETECTED_FIXTURES:
        try:
            got = [v.line for v in find_violations(source, "fixture.hs")]
        except _UnscannableSource as error:
            failures.append(f"  DETECTED: {label}\n"
                            f"    expected violations on lines {expected}, "
                            f"but the scan refused: {error}")
            continue
        if got != expected:
            failures.append(f"  DETECTED: {label}\n"
                            f"    expected violations on lines {expected}, "
                            f"got {got}")

    for label, source in CLEAN_FIXTURES:
        try:
            got = [str(v) for v in find_violations(source, "fixture.hs")]
        except _UnscannableSource as error:
            failures.append(f"  CLEAN: {label}\n"
                            f"    expected no violations, but the scan "
                            f"refused: {error}")
            continue
        if got:
            failures.append(f"  CLEAN: {label}\n"
                            f"    expected no violations, got {got}")

    for label, source, expected_error in UNSCANNABLE_FIXTURES:
        try:
            find_violations(source, "fixture.hs")
        except expected_error:
            continue
        except _UnscannableSource as error:
            failures.append(f"  UNSCANNABLE: {label}\n"
                            f"    expected {expected_error.__name__}, got "
                            f"{type(error).__name__}")
            continue
        failures.append(f"  UNSCANNABLE: {label}\n"
                        f"    expected {expected_error.__name__}, got a scan")

    # An exemption must actually suppress, and suppress only its path,
    # or the table is decorative. It must also cover a REFUSAL, since
    # that is the recorded escape requirement 4 points at.
    strict_probe = "module M where\n" + _TE + "f raw = TE.decodeUtf8 raw\n"
    refused_probe = ("module M where\nimport Data.Text.Encoding\n"
                     "f raw = decodeUtf8 raw\n")
    EXEMPTIONS["exempt.hs"] = "self-test probe"
    try:
        if find_violations(strict_probe, "exempt.hs"):
            failures.append("  EXEMPTIONS: an exempt path was still flagged")
        if not find_violations(strict_probe, "fixture.hs"):
            failures.append("  EXEMPTIONS: exempting one path silenced another")
        try:
            find_violations(refused_probe, "exempt.hs")
        except _UnscannableSource:
            failures.append("  EXEMPTIONS: an exempt path was still refused")
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
             + len(UNSCANNABLE_FIXTURES))
    print(f"lua_strict_decode_audit self-test: {total} fixtures OK "
          f"({len(DETECTED_FIXTURES)} qualified uses detected across every "
          f"import, qualification and layout form; {len(CLEAN_FIXTURES)} "
          f"clean, including the total siblings, comments, literals, bare "
          f"names and same-named functions from other modules; "
          f"{len(UNSCANNABLE_FIXTURES)} sources refused rather than "
          f"certified).")
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
    except _UnscannableSource as error:
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
