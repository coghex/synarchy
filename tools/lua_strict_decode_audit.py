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

WHAT IS DETECTED, exactly: the function
`Data.Text.Encoding.decodeUtf8` -- that ONE module's export -- reaching
code under the scoped tree, however it is spelled. Each file's own
import declarations are parsed for exactly that module, and the two
ways its export can be named are handled DIFFERENTLY, because only one
of them can be answered without full scope analysis:

  * A QUALIFIED use is reported at the call site. Every alias is
    covered, because the alias comes from the file's own import:
    `TE.decodeUtf8` under the repository's standard one,
    `Enc.decodeUtf8` under any other, and the unaliased
    `Data.Text.Encoding.decodeUtf8`. Renaming the alias cannot walk past
    this, and a qualifier can never be captured by a local term binder,
    so a qualified hit is unambiguous.

  * A BARE use cannot be. `import Data.Text.Encoding` puts `decodeUtf8`
    in unqualified scope, but a nested binder -- a function parameter, a
    `let`/`where` binding, a lambda -- may legally shadow it, and
    deciding which one a given occurrence means needs real scope
    analysis. Guessing either way is wrong: guessing "shadowed" hides a
    real strict decode, guessing "the import's" blames innocent code.
    So bare occurrences are never reported, and what IS reported is the
    UNQUALIFIED IMPORT that puts the strict decoder in bare scope --
    a syntactic fact about the file, not a guess about a call, and the
    single edit that removes the hazard (import it qualified, `hiding`
    it, or take the lenient sibling instead).

    A qualified import is NOT reported: it names nothing on its own, and
    every use it enables is caught precisely by the first rule. Under
    the scoped tree today all 89 `Data.Text.Encoding` imports are
    qualified, so this second rule reports nothing.

A same-named function from ANYWHERE ELSE is never caught, because it is
not the banned one: `Data.Text.Lazy.Encoding.decodeUtf8`, an unrelated
`Other.decodeUtf8`, or a local binding of that name. Widening the ban
to those is a separate decision from #1605's, and the fixture set below
pins each of them passing.

Two supporting rules keep that precision from becoming a bypass:

  * the parser is FAIL-LOUD. A file whose import declaration names
    `Data.Text.Encoding` in a shape this module does not model raises
    rather than being scanned as though the module were absent -- a
    silent "no import, so nothing to flag" is exactly how an
    import-resolving guard would be walked past. Import declarations are
    found by LAYOUT, not by column 0, so an indented top-level layout
    (`module M where` followed by a body indented one space) is parsed
    like any other.

  * candidates are maximal identifier LEXEMES, so the three total
    siblings are safe by construction: `decodeUtf8Lenient` is a
    DIFFERENT identifier, never a `decodeUtf8` followed by something,
    and a longer name that merely ends in it (`myDecodeUtf8`) is
    different again.

Comment and string-literal awareness comes from
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
    r"\Aimport[ \t]+(?P<qualified>qualified[ \t]+)?"
    + re.escape(STRICT_MODULE) + r"(?![\w'.])"
    r"(?:\s+as\s+(?P<alias>[A-Z][\w']*(?:\.[A-Z][\w']*)*))?"
    r"(?P<rest>[\s\S]*)\Z")

_IMPORT_KEYWORD = re.compile(r"\Aimport(?![\w'])")


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
    #: "use" -- a qualified call site naming the banned function.
    #: "unqualified-import" -- an import putting it in bare scope, where
    #: call sites cannot be resolved without full scope analysis.
    kind: str = "use"

    def __str__(self) -> str:
        if self.kind == "unqualified-import":
            return (f"{self.path}:{self.line}: `{self.spelling}` puts "
                    f"{STRICT_MODULE}.{BANNED_IDENTIFIER} in UNQUALIFIED "
                    f"scope -- a bare occurrence then cannot be told from a "
                    f"local binding that shadows it, so the import is what "
                    f"is reported. Import the module qualified, `hiding` "
                    f"{BANNED_IDENTIFIER}, or take a total sibling")
        return (f"{self.path}:{self.line}: strict decode "
                f"`{self.spelling}` ({STRICT_MODULE}.{BANNED_IDENTIFIER}) "
                f"-- Lua strings are arbitrary bytes; use "
                f"decodeUtf8Lenient (or another total sibling)")


@dataclass(frozen=True)
class StrictImports:
    """How `STRICT_MODULE`'s `decodeUtf8` can be named in one file.

    `unqualified_sites` are the import declarations that put it in bare
    scope, each `(line, declaration head)`; `qualifiers` are the module
    qualifiers a call site may write it behind."""
    qualifiers: frozenset[str]
    unqualified_sites: tuple[tuple[int, str], ...]

    def names_banned(self, qualifier: str) -> bool:
        """True if `qualifier` spells this file's
        `Data.Text.Encoding.decodeUtf8`. Only qualified uses are asked:
        a bare occurrence is deliberately never resolved here."""
        return bool(qualifier) and qualifier in self.qualifiers


# ---------------------------------------------------------------------
# Import resolution
# ---------------------------------------------------------------------

def _import_declarations(code_text: str) -> list[tuple[int, int, str]]:
    """Every import declaration in `code_text` as `(start, end, text)`.

    Found by LAYOUT, not by column 0. A declaration begins at any line
    whose first token is the reserved word `import` -- GHC accepts a
    top-level layout indented to any column, so `module M where`
    followed by a body at column 1 is ordinary valid source, and a
    column-0 rule would leave every import in such a file unparsed and
    every use in it unreported. It continues through the following lines
    indented STRICTLY MORE than its own opening column, which is exactly
    the layout rule that makes a multi-line import list one declaration.

    `code_text` must already be comment- and string-masked, so a comment
    quoting an import is never mistaken for one. `import` is a reserved
    word, so a line whose first token is `import` can be nothing else."""
    lines = code_text.split("\n")
    offsets: list[int] = []
    pos = 0
    for line in lines:
        offsets.append(pos)
        pos += len(line) + 1

    decls: list[tuple[int, int, str]] = []
    i = 0
    while i < len(lines):
        stripped = lines[i].lstrip(" \t")
        if not _IMPORT_KEYWORD.match(stripped):
            i += 1
            continue
        column = len(lines[i]) - len(stripped)
        last = i
        j = i + 1
        while j < len(lines):
            body = lines[j].lstrip(" \t")
            if body and ((len(lines[j]) - len(body)) <= column
                         # A continuation line can never itself start
                         # with the reserved word `import`; treating one
                         # as a fresh declaration keeps a malformed file
                         # from folding two imports into one and then
                         # reporting the wrong shape.
                         or _IMPORT_KEYWORD.match(body)):
                break
            if body:
                last = j
            j += 1
        decls.append((offsets[i], offsets[last] + len(lines[last]),
                      "\n".join(line.strip() for line in lines[i:last + 1])))
        i = last + 1
    return decls


def _listed_names(chunk: str) -> set[str]:
    """The identifiers named inside an import list. Membership is all
    this module asks of it, and `decodeUtf8Lenient` lexes as its own
    identifier, so a coarse identifier scan cannot answer wrongly."""
    return set(re.findall(r"[A-Za-z_][A-Za-z0-9_']*", chunk))


def _classify_strict_import(decl: str, rel_path: str, line: int) -> tuple[str, bool] | None:
    """`(qualifier, unqualified)` for an import of `STRICT_MODULE` that
    brings `decodeUtf8` into scope, or None if it does not (a `hiding`
    list naming it, or an explicit list omitting it).

    Raises `_ImportParseError` when the declaration names the module in
    a shape this parser does not model."""
    match = _STRICT_IMPORT.match(decl)
    if match is None:
        raise _ImportParseError(
            f"{rel_path}:{line}: this import names {STRICT_MODULE} in a shape "
            f"tools/lua_strict_decode_audit.py does not model, so it cannot "
            f"say which spellings of {BANNED_IDENTIFIER} it brings into "
            f"scope:\n\n{decl}\n\n"
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
                f"{STRICT_MODULE} import:\n\n{decl}")
        listed = _listed_names(rest)
        in_scope = (BANNED_IDENTIFIER not in listed) if hiding \
            else (BANNED_IDENTIFIER in listed)
    elif rest or hiding:
        raise _ImportParseError(
            f"{rel_path}:{line}: unexpected trailing text on this "
            f"{STRICT_MODULE} import:\n\n{decl}")
    else:
        in_scope = True

    if not in_scope:
        return None
    alias = match.group("alias")
    return (alias if alias else STRICT_MODULE, match.group("qualified") is None)


def resolve_strict_imports(code_text: str, rel_path: str) -> tuple[StrictImports, list[tuple[int, int]]]:
    """`(how this file can name the banned function, import spans)`.

    The spans are returned so the call-site scan can skip the import
    declarations themselves -- a qualified import names nothing on its
    own, and the unqualified case is reported here instead."""
    qualifiers: set[str] = set()
    unqualified_sites: list[tuple[int, str]] = []
    spans: list[tuple[int, int]] = []
    for start, end, decl in _import_declarations(code_text):
        spans.append((start, end))
        if not _MENTIONS_STRICT_MODULE.search(decl):
            continue
        line = code_text.count("\n", 0, start) + 1
        classified = _classify_strict_import(decl, rel_path, line)
        if classified is None:
            continue
        qualifier, is_unqualified = classified
        qualifiers.add(qualifier)
        # The unaliased module path always names this module
        # unambiguously wherever it is in scope at all, so accept it
        # alongside whatever alias was declared. On code that compiles
        # this can only ever be the banned function.
        qualifiers.add(STRICT_MODULE)
        if is_unqualified:
            unqualified_sites.append((line, decl.split("\n")[0].strip()))
    return (StrictImports(frozenset(qualifiers), tuple(unqualified_sites)),
            spans)


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
    while start > 0 and (text[start - 1] in _IDENT_CHARS or text[start - 1] == "."):
        start -= 1
    candidate = text[start:pos]
    if not candidate.endswith("."):
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
    """Every way `Data.Text.Encoding.decodeUtf8` reaches code in `text`
    (the source of the file at repo-relative `rel_path`): each qualified
    use outside comments, string literals and import declarations, plus
    each import that puts the function in unqualified scope. Nothing at
    all for a path in `EXEMPTIONS`."""
    if rel_path in EXEMPTIONS:
        return []
    code_text = haskell_code_only(text)
    imports, import_spans = resolve_strict_imports(code_text, rel_path)

    violations: list[Violation] = [
        Violation(rel_path, line, decl, kind="unqualified-import")
        for line, decl in imports.unqualified_sites
    ]
    for start, end in haskell_code_spans(text):
        for pos, lexeme in _identifier_runs(text, start, end):
            if lexeme != BANNED_IDENTIFIER or _within(pos, import_spans):
                continue
            qualifier = _qualifier_before(text, pos)
            if not imports.names_banned(qualifier):
                continue
            violations.append(
                Violation(rel_path, _line_of(text, pos),
                          f"{qualifier}.{lexeme}"))
    return sorted(violations, key=lambda v: (v.line, v.kind))


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
# `expected` is the lines that must be reported, in order.
_TE = "import qualified Data.Text.Encoding as TE\n"

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
        "an unqualified import with no list: the IMPORT is reported, "
        "because a bare occurrence cannot be told from a local binding "
        "that shadows it",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "f raw = decodeUtf8 raw\n",
        [2],
    ),
    (
        "an unqualified import whose explicit list NAMES the function",
        "module M where\n"
        "import Data.Text.Encoding (decodeUtf8, decodeUtf8Lenient)\n"
        "f raw = decodeUtf8 raw\n",
        [2],
    ),
    (
        "an unqualified import with an alias -- the import for its bare "
        "exposure, and the qualified use at its own call site",
        "module M where\n"
        "import Data.Text.Encoding as TE\n"
        "f raw = (TE.decodeUtf8 raw, decodeUtf8 raw)\n",
        [2, 3],
    ),
    (
        "a `hiding` list that hides something ELSE, so the function is "
        "still in unqualified scope",
        "module M where\n"
        "import Data.Text.Encoding hiding (decodeUtf8Lenient)\n"
        "f raw = decodeUtf8 raw\n",
        [2],
    ),
    (
        "an import list spread over several lines",
        "module M where\n"
        "import Data.Text.Encoding\n"
        "    ( decodeUtf8\n"
        "    , decodeUtf8Lenient )\n"
        "f raw = decodeUtf8 raw\n",
        [2],
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
        "declaration ends by column and not by column 0",
        "module M where\n"
        "  import qualified Data.Text.Encoding as TE\n"
        "      ( decodeUtf8 )\n"
        "  f raw = TE.decodeUtf8 raw\n",
        [4],
    ),
    (
        "an import indented past a preceding multi-line import: a "
        "continuation line can never start with `import`, so this is a "
        "fresh declaration and the call it enables is still reported",
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
        "a QUALIFIED import with no use -- it names nothing on its own, "
        "and every use it enables is caught at its own call site",
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
        "SHADOWING: a `where`-bound decodeUtf8 legally shadows the "
        "unqualified import at this call site, so the CALL is never "
        "blamed (the import above it is what the audit reports)",
        "module M where\n" + _TE
        + "f raw = decodeUtf8 raw\n"
        + "  where decodeUtf8 = myOwnTotalThing\n",
    ),
    (
        "SHADOWING: a parameter named decodeUtf8 under a qualified "
        "import",
        "module M where\n" + _TE
        + "f decodeUtf8 raw = decodeUtf8 raw\n",
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
    (
        "the composition `f.decodeUtf8` is a bare use, not a qualified "
        "one -- and stays clean when the module is imported qualified",
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

    # An exemption must actually suppress, or the table is decorative.
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
    total = len(DETECTED_FIXTURES) + len(CLEAN_FIXTURES) + len(UNMODELLED_FIXTURES)
    print(f"lua_strict_decode_audit self-test: {total} fixtures OK "
          f"({len(DETECTED_FIXTURES)} detected through every import, "
          f"qualification and layout form; {len(CLEAN_FIXTURES)} clean, "
          f"including the total siblings, comments, literals, shadowed "
          f"bare names and same-named functions from other modules; "
          f"{len(UNMODELLED_FIXTURES)} unmodelled imports that raise "
          f"rather than pass).")
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
        print(f"{len(violations)} way(s) "
              f"`{STRICT_MODULE}.{BANNED_IDENTIFIER}` reaches code under "
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
    print(f"No strict `{STRICT_MODULE}.{BANNED_IDENTIFIER}` found under "
          f"{SCOPED_TREE}/.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
