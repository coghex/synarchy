#!/usr/bin/env python3
"""Guard: `src/` + `app/` render a `Show` value to strict `Text` with
`tshow`, never with a hand-written `pack`-a-shown-value wrapper (#2177).

`UPrelude` defines that wrapper once (`src/UPrelude.hs`,
`tshow = TXT.pack ∘ show`), and issue #1099 / PR #1568 replaced the 709
direct copies it found and deleted the 24 local `tshow` definitions.
Nothing enforced the result, and the spelling regressed twice within
nine days of that merge -- `Engine.Asset.YamlRecipes` (PR #1984) and
`Engine.Scripting.Lua.API.Graphics` (PR #2104) -- while the point-free
compositions #1099's acceptance grep never scoped survived untouched in
eleven more files, two of them as renamed copies (`showT`, `tShow`).

Every occurrence is definitionally `tshow`, so this is a pure
respelling: `tshow = TXT.pack ∘ show` is the same function, and no
rendered byte moves. That is what makes it a spelling rule of the same
class as #1005 / #1494's operator guard, which this module sits beside
and borrows its comment/string lexer from.

The rule
--------
A `pack` that resolves to `Data.Text`'s -- the STRICT one -- applied
directly to `show`, in any of the four spellings, is a violation:

    pack (show x)     pack $ show x     pack . show     pack ∘ show

Directly means adjacency modulo whitespace and comments, so a multiline
wrap is the same hit; `pack . f . show` is not one, being a different
function. `show` may itself be qualified (`P.show`).

Resolution is by BINDING, never by the `T.` qualifier's spelling. This
tree binds `T` to `Data.Text` in most modules -- but in `src/UPrelude.hs`
`T` is `Data.Text.Encoding` and `Data.Text` is `TXT`, and
`Engine.Scripting.Lua.Thread.Console` and `.API.Shell` each bind `T` to
BOTH `Data.Text` and `Data.Text.Read`. A qualifier-keyed rule would
report `UPrelude`'s encoder and miss a `Data.Text` under any other
alias. Imports are
read with `tools/engine_env_capability_writer_syntax.py`'s
`parse_imports` / `imports_name` -- the same resolver the capability
writer scanner uses -- so all of these are decided correctly:

  * `import qualified Data.Text as T`     -> `T.pack (show x)` is a hit.
  * `import qualified Data.Text as Txt`   -> `Txt.pack ...` is a hit too.
  * `import qualified Data.Text`          -> `Data.Text.pack ...` is a hit.
  * `import Data.Text (pack)`             -> a BARE `pack ...` is a hit.
  * `import qualified Data.ByteString.Char8 as BC`
                                          -> `BC.pack (show w)` is NOT
    a hit: `Unit.Atlas.Digest` builds ByteString digest material, a
    different conversion that this rule leaves alone (#2177 out of
    scope).
  * `import qualified Data.Text.Lazy as TL` -> not a hit either; `tshow`
    produces strict `Text` and is not that function.

Non-code text is not scanned. `tools/unicode_operator_audit.py`'s
`haskell_code_only` excludes `--` line comments, nestable `{- -}` block
comments and `"..."` string literals, and this module masks
QUASIQUOTES on top of that. The sibling guard's quasiquote handling is
scoped to `ShaderCode.hs`'s GLSL splices by name; the rule here applies
to every production path, so the masking has to be general.
`QuasiQuotes` is a global `default-extensions` entry in
`synarchy.cabal`, which is also what makes the `[varid|` form
unambiguous: with the extension on, GHC itself reads `[e|` as a
quasiquote opener and a list comprehension must be written `[ e | ... ]`.

Refusals
--------
A file this module cannot certify is REFUSED, loudly, rather than
scanned as if it were clean -- the one failure mode a guard must not
have. Three cases, each with its own fixture below:

  * a CPP `#define` / `#undef` / `#include`, which can rewrite the very
    alias this scan resolves by before GHC ever sees the module;
  * a `Data.Text` import whose shape the resolver does not model (an
    alias it cannot read, trailing text it cannot classify);
  * a `Data.Text` mention outside any recognized import declaration --
    an indented or explicit-brace top-level layout, which
    `_import_chunks` does not collect and whose import would otherwise
    be invisible.

`src/UPrelude.hs`'s own export list is checked separately, once per
run: were it ever to re-export `Data.Text` wholesale, every module in
the tree would gain an unqualified `pack` that no per-file import
declares, and this module's resolution would silently under-report.

Exemption
---------
One, construct-scoped: `tshow`'s canonical definition in
`src/UPrelude.hs`. It is the wrapper, so it must spell itself out. The
exemption covers that ONE `pack` occurrence and not the file -- a
second hand-written wrapper elsewhere in `UPrelude` still fails.

Usage:
  python3 tools/tshow_spelling_audit.py              # audit the tree
  python3 tools/tshow_spelling_audit.py --self-test  # fixtures only
Exit codes: 0 = clean, 1 = a hand-written wrapper was found or a file
could not be certified (or, under --self-test, a fixture behaved
wrongly).
"""
from __future__ import annotations

import argparse
import re
import sys
import unicodedata
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from unicode_operator_audit import (  # type: ignore  # noqa: E402
    _CHAR_LITERAL, _IDENT_CONTINUE, haskell_code_only)
from engine_env_capability_common import (  # type: ignore  # noqa: E402
    _SYMBOL_CHARS, _import_chunks)
from engine_env_capability_writer_syntax import (  # type: ignore  # noqa: E402
    _IMPORT_DECL_RE, imports_name, parse_imports)

# The scanned trees. `test/` and `test-headless/` are out of scope:
# #1099 scoped its rewrite to production, and the sibling spelling guard
# scans src/+app/ only.
SCANNED_TREES = ("src", "app")

# The module whose `pack` this rule is about, exactly. `Data.Text.Lazy`
# is a different type and `Data.ByteString.Char8` a different
# conversion; neither is `tshow`.
TEXT_MODULE = "Data.Text"
PACK = "pack"
CANONICAL = "tshow"

UPRELUDE_FILE = "src/UPrelude.hs"

# Exemptions: repo-relative path -> the reason, stated inline the way
# tools/unicode_operator_audit.py states its own. Whole-file, and
# deliberately EMPTY: the single real exemption is construct-scoped
# (`_TSHOW_DEFINITION` below) so that `UPrelude` is covered for the one
# definition and audited everywhere else.
EXEMPTIONS: dict[str, str] = {}

# `tshow`'s canonical definition, matched in code only and anchored at
# column 0 so a `where`-bound local of the same name is NOT exempt.
# Group 1 is the `pack` token itself, which is what makes the exemption
# that one occurrence rather than the line or the file.
_TSHOW_DEFINITION = re.compile(
    r"^" + CANONICAL + r"[ \t]*=[ \t]*(?:[A-Za-z][\w'.]*\.)?(" + PACK + r")"
    r"[ \t]*[.∘][ \t]*show[ \t]*$",
    re.MULTILINE)

# `Data.Text` as a WHOLE module path: never the prefix of
# `Data.Text.Encoding`, and never the qualifier of a use such as
# `Data.Text.pack` (whose trailing `.` fails the lookahead), so the
# refusal check below counts import mentions and nothing else.
_TEXT_MODULE_MENTION = re.compile(
    r"(?<![\w'.])" + re.escape(TEXT_MODULE) + r"(?![\w'.])")

# CPP directives that rewrite identifiers before GHC parses them, or
# hide text from this scan. Matched on RAW text: CPP runs first, so a
# `#define` inside what looks like a Haskell comment is still real.
_REWRITING_CPP_DIRECTIVE = re.compile(
    r"^[ \t]*#[ \t]*(define|undef|include)\b", re.MULTILINE)

# A quasiquote OPENER: `[` immediately followed by a quasiquoter name (a
# varid, optionally module-qualified) and `|`. The payload runs to the
# first `|]` and is not Haskell at all. See the module docstring for why
# the no-space form is unambiguous here.
_QUASIQUOTE_OPEN = re.compile(r"\[(?:[A-Z][\w']*\.)*[a-z_][\w']*\|")
_QUASIQUOTE_CLOSE = "|]"

# `pack` as a maximal identifier lexeme. `'` is an identifier character
# in Haskell, so it joins `\w` in both lookarounds: `pack'` is a
# different name and `unpack` is not this one.
_PACK_LEXEME = re.compile(r"(?<![\w'])" + PACK + r"(?![\w'])")

# What must follow a `pack` for the occurrence to be a show-to-Text
# wrapper. `\x00` joins whitespace in every gap because `haskell_code_only`
# blanks each non-code byte to it in place, so `pack {- why -} (show x)`
# is the same expression with a null run in the middle. An optional
# uppercase-led qualifier on `show` is admitted and then validated, so
# `P.show` counts and a `t.show` record selector does not.
_WRAPPER_TAIL = re.compile(
    r"[\s\x00]*(?:\((?P<paren>)|\$(?P<dollar>)|(?P<compose>[.∘]))[\s\x00]*"
    r"(?P<qualifier>(?:[A-Za-z][\w']*\.)+)?show(?![\w'])")

_FORM_NAMES = {
    "paren": "pack (show …)",
    "dollar": "pack $ show …",
    "compose": "pack ∘ show",
}


class _UnscannableSource(Exception):
    """Source this guard cannot certify, for any reason.

    Raised rather than swallowed. Every alternative -- scanning a file
    whose `Data.Text` import was not understood, or one whose text the
    preprocessor rewrites before the compiler sees it -- makes the
    wrappers in that file invisible, and a guard that can be silenced
    by an unusual import is not a guard."""


class _PreprocessorError(_UnscannableSource):
    """A file whose text CPP will rewrite before GHC parses it."""


class _ImportParseError(_UnscannableSource):
    """A `Data.Text` import this module cannot classify."""


class _PremiseError(_UnscannableSource):
    """`UPrelude` no longer matches the premise this scan rests on."""


@dataclass(frozen=True)
class Violation:
    path: str
    line: int
    spelling: str
    form: str

    def __str__(self) -> str:
        return (f"{self.path}:{self.line}: hand-written show-to-Text "
                f"wrapper `{self.spelling}` ({_FORM_NAMES[self.form]}) "
                f"-- use `{CANONICAL}`")


def _mask_spans(text: str, spans: list[tuple[int, int]]) -> str:
    """Blank `[start, end)` spans to `\\x00`, one byte for one byte, so
    every other position -- and every line number -- stays valid."""
    out = list(text)
    for start, end in spans:
        for i in range(start, end):
            if out[i] != "\n":
                out[i] = "\x00"
    return "".join(out)


def _line_of(text: str, pos: int) -> int:
    return text.count("\n", 0, pos) + 1


def _is_module_path(candidate: str) -> bool:
    """True if `candidate` (`P`, `Data.Text`) is a Haskell qualified
    module path: dot-separated segments, each uppercase-led.

    Unicode-aware via `str.isupper()` rather than an ASCII `[A-Z]`, for
    the same reason `tools/unicode_operator_audit.py` resolves ITS
    qualifiers that way: GHC accepts non-ASCII letters in module names,
    and this codebase's own identifiers are not ASCII-limited."""
    if not candidate:
        return False
    return all(seg and seg[0].isupper()
               and all(ch.isalnum() or ch in "_'" for ch in seg[1:])
               for seg in candidate.split("."))


def _qualifier_before(text: str, pos: int) -> str:
    """The module qualifier written immediately before `pos` (`"T"`,
    `"Data.Text"`), or `""` for an unqualified occurrence.

    A qualifier is written with no space before its `.`, so it is the
    `[\\w'.]` run ending in `.` directly ahead of the name -- and only
    when it is a real module path, which is what separates the
    qualified `T.pack` from the composition `f.pack`. A leading Template
    Haskell name quote is stripped first: `'` is an identifier character
    (`map'`), so `$(varE 'T.pack)` would otherwise present the segment
    `'T`, read as no qualifier at all, and the splice names the very
    function this resolves."""
    start = pos
    while start > 0 and (text[start - 1].isalnum()
                         or text[start - 1] in "_'."):
        start -= 1
    candidate = text[start:pos]
    if not candidate.endswith("."):
        return ""
    candidate = candidate[:-1].lstrip("'")
    return candidate if _is_module_path(candidate) else ""


# Haskell 2010 SS2.2's `special` characters, which `uniSymbol` excludes
# along with `_`, `"` and `'`. All ASCII, which is what lets
# `_is_symbol_char` decide every ASCII character from `_SYMBOL_CHARS`
# alone and reach the Unicode tables only for the rest.
_HASKELL_SPECIAL = frozenset("()[]{},;`_\"'")


def _is_symbol_char(char: str) -> bool:
    """True for a character a Haskell symbolic operator is made of.

    Report SS2.2: `symbol -> ascSymbol | uniSymbol<special | _ | " | '>`,
    where `uniSymbol` is ANY Unicode symbol or punctuation. This
    codebase is `UnicodeSyntax` throughout and defines its own operators
    from that set (`⊚`, `⌦`, `∘`, `⚟`), so an ASCII-only test splits a
    lexeme like `⊚--` and hands the trailing `--` on as a comment
    opener -- masking the wrapper after a valid operator (PR #2404
    review round 3).

    `_SYMBOL_CHARS` is `ascSymbol`, and every `special` character is
    ASCII, so an ASCII character is decided by that set alone and the
    category lookup runs only for the rest."""
    if char.isascii():
        return char in _SYMBOL_CHARS
    return unicodedata.category(char)[0] in "SP"


def _premasked_spans(text: str) -> list[tuple[int, int]]:
    """The `[start, end)` spans `_prepared_code` blanks before handing
    the source to `haskell_code_only`, in source order.

    Exactly the two constructs that lexer gets WRONG, and nothing else.
    It models `--` line comments, nestable `{- -}` block comments,
    string literals and character literals, and it models them
    correctly; masking those a second time here would be a second
    opinion free to drift from it. What it does not model is a
    QUASIQUOTE, and what it gets wrong is a multi-dash OPERATOR:

      * a quasiquote's payload is not Haskell at all, so a lone `"` in
        one opens a string that runs past the closing `|]` and swallows
        the code after it, and a `--` in one opens a comment that eats
        the `|]` itself (PR #2404 review round 1);
      * `-->` is an operator, not a comment. Haskell report SS2.3 opens
        a comment only on a dash run the rest of its symbol lexeme does
        not continue, so `f x = x --> T.pack (show x)` is code and a
        lexer that stops at the `--` masks the wrapper (round 2).

    Masking the operator is what leaves the shared lexer no `--` to
    misread, which is what makes the dash rule here the only one in
    play. It removes nothing the scan could have reported: the four
    spellings this guard matches are `(`, `$`, `.` and `∘`, none of
    them a dash run. A run carrying no `--` at all (`->`, `-`, `.`) is
    left alone, having nothing to be misread.

    Both sides of the dash run decide it -- `-->` is an operator
    because the run continues into `>`, `<--` because it began at `<`.
    The leading side needs no predicate: a symbol run is consumed WHOLE
    from its first character, and every other arm leaves `i` on a
    character no symbol precedes, so a dash inside a longer lexeme is
    never tested on its own. What counts as a symbol character is
    `_is_symbol_char`, which is report SS2.2's full set and not just the
    ASCII half -- this tree writes its own operators in Unicode, and
    `⊚--` is one lexeme.

    Comments, strings and character literals are SKIPPED here, never
    recorded: skipping is what stops an opener quoted inside one from
    being read as a real quasiquote boundary, and that is all this walk
    needs from them. `_CHAR_LITERAL` and `_IDENT_CONTINUE` come from
    the shared module too -- a `'x'` must be skipped atomically so the
    real `'"'` in `Engine.Scripting.Lua.API.Shell` cannot open a
    phantom string, and a `'` that is an identifier's trailing prime
    (`map'`) must not be read as a literal at all.

    An UNCLOSED quasiquote ran to end of file and is masked that way.
    Such a source does not compile either way, and leaving its tail
    readable would let one phantom opener hide the rest of the module.
    That branch is also what makes the walk terminate on every input:
    a recorded span ends at `opener.end()` or later, and every other
    arm advances `i` by at least one, so `i` strictly increases."""
    spans: list[tuple[int, int]] = []
    i, n = 0, len(text)
    while i < n:
        char = text[i]
        if text.startswith("{-", i):
            depth, i = 1, i + 2
            while i < n and depth:
                if text.startswith("{-", i):
                    depth, i = depth + 1, i + 2
                elif text.startswith("-}", i):
                    depth, i = depth - 1, i + 2
                else:
                    i += 1
            continue
        if char == '"':
            i += 1
            while i < n and text[i] != '"':
                i += 2 if text[i] == "\\" else 1
            i += 1
            continue
        if char == "'" and (i == 0 or not _IDENT_CONTINUE.match(text[i - 1])):
            literal = _CHAR_LITERAL.match(text, i)
            if literal:
                i = literal.end()
                continue
        if _is_symbol_char(char):
            run = i
            while run < n and _is_symbol_char(text[run]):
                run += 1
            lexeme = text[i:run]
            if len(lexeme) >= 2 and lexeme == "-" * len(lexeme):
                newline = text.find("\n", run)
                i = n if newline == -1 else newline
            else:
                if "--" in lexeme:
                    spans.append((i, run))
                i = run
            continue
        opener = _QUASIQUOTE_OPEN.match(text, i)
        if opener:
            close = text.find(_QUASIQUOTE_CLOSE, opener.end())
            end = n if close == -1 else close + len(_QUASIQUOTE_CLOSE)
            spans.append((i, end))
            i = end
            continue
        i += 1
    return spans


def _prepared_code(text: str) -> str:
    """`text` reduced to the Haskell code this module scans: comments
    and quasiquotes masked by the walk above, then `haskell_code_only`
    over the result for strings and character literals. Positions and
    line numbers are preserved throughout, so every offset still maps
    back to the original source."""
    return haskell_code_only(
        _mask_spans(text, _premasked_spans(text)))


_MODULE_KEYWORD = re.compile(r"^module(?![\w'])", re.MULTILINE)
_WHERE_KEYWORD = re.compile(r"(?<![\w'])where(?![\w'])")


def _split_module_header(code_text: str) -> tuple[str, int]:
    """`(export-list header, offset where the module body starts)`.

    The header is `module M ( ... ) where`, which is the one place a
    module NAME appears without being an import: `UPrelude`'s export
    list carries `module Data.Text`. Both callers need the split and
    need it drawn the same way -- the premise check reads the header,
    the import accounting reads only the body -- so it is one helper.

    A file with no module header (an implicit `Main`) has an empty
    header and a body starting at 0, which is what makes the accounting
    below conservative for it rather than blind."""
    keyword = _MODULE_KEYWORD.search(code_text)
    if keyword is None:
        return "", 0
    where = _WHERE_KEYWORD.search(code_text, keyword.end())
    if where is None:
        return code_text, len(code_text)
    return code_text[:where.end()], where.end()


def _classify_text_imports(prepared: str, rel_path: str) -> None:
    """Raise `_ImportParseError` unless every `Data.Text` import in
    `text` is one the resolver reads completely.

    Two questions, because they fail differently. Is each mention of the
    module inside an import declaration `_import_chunks` collected at
    all -- an indented or explicit-brace top-level layout is valid
    Haskell that it does not collect, and an uncollected import is an
    invisible one. And does the collected declaration's text end after
    a shape the resolver models: nothing, a `hiding` clause, an import
    list? Anything left over means a piece of the declaration -- an
    alias it could not read, most of all -- was dropped, and the
    qualifier this scan resolves by would be wrong."""
    # The module header is excluded: an export list is the one place a
    # module name appears without being an import, and `module
    # Data.Text` there is a re-export, whose hazard is
    # `check_uprelude_premise`'s subject and not this one's.
    _, body_start = _split_module_header(prepared)
    code_text = prepared[body_start:]
    accounted = 0
    for chunk in _import_chunks(code_text):
        head = _IMPORT_DECL_RE.match(chunk)
        if head is None or head.group("module") != TEXT_MODULE:
            continue
        accounted += len(_TEXT_MODULE_MENTION.findall(chunk))
        rest = chunk[head.end():].strip()
        if rest.startswith("hiding"):
            rest = rest[len("hiding"):].strip()
        if rest.startswith("("):
            if not rest.endswith(")"):
                raise _ImportParseError(
                    f"{rel_path}: unterminated import list on this "
                    f"{TEXT_MODULE} import:\n\n{chunk.strip()}")
        elif rest:
            raise _ImportParseError(
                f"{rel_path}: unexpected trailing text on this "
                f"{TEXT_MODULE} import:\n\n{chunk.strip()}\n\n"
                f"tools/tshow_spelling_audit.py resolves `{PACK}` by the "
                f"qualifier this declaration establishes, and it could not "
                f"read this one -- so the file cannot be certified as "
                f"written. Teach the resolver the shape, or record the file "
                f"in EXEMPTIONS with the reason it is safe.")

    total = len(_TEXT_MODULE_MENTION.findall(code_text))
    if total > accounted:
        raise _ImportParseError(
            f"{rel_path}: {total - accounted} mention(s) of {TEXT_MODULE} "
            f"sit outside any import declaration this module recognizes "
            f"(a top-level layout that is indented, or in explicit "
            f"braces). An import it cannot see is one whose `{PACK}` uses "
            f"it cannot resolve, so the file cannot be certified as "
            f"written.")


def check_uprelude_premise(uprelude_source: str) -> None:
    """Raise `_PremiseError` if `UPrelude` re-exports `Data.Text`'s
    `pack` under its UNQUALIFIED name.

    Every module in this tree imports `UPrelude` (`NoImplicitPrelude` is
    global), so such a re-export would put a bare `pack` in scope
    everywhere WITHOUT any per-file import declaring it -- and
    `find_violations` resolves a bare `pack` from the importing file's
    own import declarations, so it would silently under-report.

    `UPrelude`'s export list does carry `module Data.Text`, and that is
    NOT the same thing. Report SS5.2: `module M` exports exactly the
    entities in scope under BOTH an unqualified name and the qualified
    name `M.e`. `UPrelude` imports the module twice -- qualified as
    `TXT`, which supplies no unqualified name, and `Data.Text (Text)`,
    which supplies the type alone -- so the re-export carries `Text` and
    not `pack`. Both halves of the report's rule are tested here, with
    the same resolver the scan uses, rather than the entry's presence
    being read as the hazard."""
    header, _ = _split_module_header(_prepared_code(uprelude_source))
    re_exported = re.search(
        r"(?<![\w'])module\s+" + re.escape(TEXT_MODULE) + r"(?![\w'.])",
        header) is not None
    if not re_exported:
        return

    declarations = parse_imports(uprelude_source)
    unqualified = imports_name(declarations, TEXT_MODULE, PACK, "")
    under_module_name = imports_name(declarations, TEXT_MODULE, PACK,
                                     TEXT_MODULE)
    if unqualified and under_module_name:
        raise _PremiseError(
            f"{UPRELUDE_FILE} re-exports `module {TEXT_MODULE}` while "
            f"`{PACK}` is in scope there both unqualified and as "
            f"`{TEXT_MODULE}.{PACK}`, so the re-export carries it (report "
            f"SS5.2) and every module in the tree gains a bare `{PACK}`. "
            f"tools/tshow_spelling_audit.py resolves a bare `{PACK}` from "
            f"the importing file's OWN import declarations, so it would "
            f"now under-report. Teach it the re-export, or keep "
            f"{TEXT_MODULE} imported qualified there.")


def find_violations(text: str, rel_path: str) -> list[Violation]:
    """Every hand-written show-to-Text wrapper in `text` (the source of
    the file at repo-relative `rel_path`), outside comments, string
    literals, quasiquotes and this module's exemptions.

    Nothing at all for a path in `EXEMPTIONS`. Raises
    `_UnscannableSource` for a file this guard cannot certify."""
    if rel_path in EXEMPTIONS:
        return []

    directive = _REWRITING_CPP_DIRECTIVE.search(text)
    if directive:
        raise _PreprocessorError(
            f"{rel_path}:{_line_of(text, directive.start())}: this file "
            f"carries a CPP `{directive.group(1)}` directive, and "
            f"tools/tshow_spelling_audit.py reads UNPREPROCESSED source. A "
            f"macro can rename the very module alias this scan resolves "
            f"`{PACK}` by, so the file cannot be certified as written.\n\n"
            f"Either drop the directive, or record the file in EXEMPTIONS "
            f"with the reason it is safe. Conditional directives "
            f"(#if / #ifdef) only select text and are not reported.")

    # One preparation, shared by the import accounting and the scan, so
    # a construct one of them reads as comment text can never be code
    # to the other.
    scan_text = _prepared_code(text)
    _classify_text_imports(scan_text, rel_path)
    declarations = parse_imports(text)

    exempt: set[int] = set()
    if rel_path == UPRELUDE_FILE:
        exempt = {m.start(1) for m in _TSHOW_DEFINITION.finditer(scan_text)}

    violations: list[Violation] = []
    for match in _PACK_LEXEME.finditer(scan_text):
        if match.start() in exempt:
            continue
        qualifier = _qualifier_before(scan_text, match.start())
        if not imports_name(declarations, TEXT_MODULE, PACK, qualifier):
            continue
        tail = _WRAPPER_TAIL.match(scan_text, match.end())
        if tail is None:
            continue
        show_qualifier = tail.group("qualifier")
        if show_qualifier and not _is_module_path(show_qualifier[:-1]):
            continue
        form = next(name for name in _FORM_NAMES
                    if tail.group(name) is not None)
        spelling = f"{qualifier}.{PACK}" if qualifier else PACK
        violations.append(Violation(rel_path, _line_of(text, match.start()),
                                    spelling, form))
    return violations


def scan_tree(repo_root: Path) -> list[Violation]:
    check_uprelude_premise(
        (repo_root / UPRELUDE_FILE).read_text(encoding="utf-8"))
    violations: list[Violation] = []
    for tree in SCANNED_TREES:
        for path in sorted((repo_root / tree).glob("**/*.hs")):
            rel = path.relative_to(repo_root).as_posix()
            violations.extend(
                find_violations(path.read_text(encoding="utf-8"), rel))
    return violations


# ---------------------------------------------------------------------
# Self-test (requirement 5)
# ---------------------------------------------------------------------
#
# Fixtures, never the shipped tree: each is a synthetic module source
# put through the same `find_violations` the audit runs, so a rule that
# stops matching is caught here rather than by a clean run on a dirty
# tree.

_T = "import qualified Data.Text as T\n"

# `(label, source, lines that must be reported in order)`
DETECTED_FIXTURES: list[tuple[str, str, list[int]]] = [
    (
        "`T.pack (show …)` -- the exact form PR #1984 reintroduced",
        "module M where\n" + _T + "f x = T.pack (show x)\n",
        [3],
    ),
    (
        "`T.pack $ show …`, #1099's other acceptance form",
        "module M where\n" + _T + "f x = T.pack $ show x\n",
        [3],
    ),
    (
        "`T.pack . show`, the ASCII composition #1099 never scoped",
        "module M where\n" + _T + "f = T.pack . show\n",
        [3],
    ),
    (
        "`T.pack ∘ show`, the Unicode composition -- the spelling this "
        "codebase actually writes",
        "module M where\n" + _T + "f = T.pack ∘ show\n",
        [3],
    ),
    (
        "a renamed copy is still the wrapper: `showT`, as "
        "World.Save.Storage defined it",
        "module M where\n" + _T
        + "showT :: Show a => a -> Text\nshowT = T.pack . show\n",
        [4],
    ),
    (
        "an alias other than `T` -- a qualifier-keyed rule would miss it",
        "module M where\n"
        "import qualified Data.Text as Txt\n"
        "f x = Txt.pack (show x)\n",
        [3],
    ),
    (
        "no alias, so the qualifier is the module path itself",
        "module M where\n"
        "import qualified Data.Text\n"
        "f x = Data.Text.pack (show x)\n",
        [3],
    ),
    (
        "postpositive `qualified` (GHC2024's ImportQualifiedPost)",
        "module M where\n"
        "import Data.Text qualified as T\n"
        "f x = T.pack (show x)\n",
        [3],
    ),
    (
        "`pack` imported UNQUALIFIED from Data.Text, so the bare name is "
        "the strict packer",
        "module M where\n"
        "import Data.Text (Text, pack)\n"
        "f x = pack (show x)\n",
        [3],
    ),
    (
        "an unqualified import with no list at all still brings `pack` in",
        "module M where\n"
        "import Data.Text\n"
        "f x = pack (show x)\n",
        [3],
    ),
    (
        "an unqualified import `hiding` something ELSE still brings "
        "`pack` in",
        "module M where\n"
        "import Data.Text hiding (unpack)\n"
        "f x = pack (show x)\n",
        [3],
    ),
    (
        "a MULTILINE wrapper: the argument wraps onto the next line",
        "module M where\n" + _T
        + "f x = T.pack (show\n"
        "                x)\n",
        [3],
    ),
    (
        "a multiline COMPOSITION, the `.` and `show` on later lines",
        "module M where\n" + _T
        + "f = T.pack\n"
        "      .\n"
        "      show\n",
        [3],
    ),
    (
        "a comment sitting INSIDE the expression does not break the "
        "adjacency the rule is about",
        "module M where\n" + _T
        + "f x = T.pack {- why -} (show x)\n",
        [3],
    ),
    (
        "a QUALIFIED `show`: `P.show` is the same function",
        "module M where\n" + _T
        + "import qualified Prelude as P\n"
        "f x = T.pack (P.show x)\n",
        [4],
    ),
    (
        "two wrappers on ONE line are two violations",
        "module M where\n" + _T
        + "f x y = T.pack (show x) <> T.pack (show y)\n",
        [3, 3],
    ),
    (
        "TWO modules sharing one alias, only one of them Data.Text -- "
        "real in Engine.Scripting.Lua.Thread.Console, and GHC resolves "
        "`T.pack` unambiguously because only one of them has it",
        "module M where\n" + _T
        + "import qualified Data.Text.Read as T\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "an import list that names `pack` explicitly, qualified",
        "module M where\n"
        "import qualified Data.Text as T (pack)\n"
        "f x = T.pack (show x)\n",
        [3],
    ),
    (
        "an import list spread over several lines",
        "module M where\n"
        "import qualified Data.Text as T\n"
        "    ( pack\n"
        "    , unpack )\n"
        "f x = T.pack (show x)\n",
        [5],
    ),
    (
        "a lone `\"` in a quasiquote PAYLOAD must not open a Haskell "
        "string that runs past the closing `|]` and swallows the "
        "wrapper below it (PR #2404 review)",
        "module M where\n" + _T
        + "s = [text| \" |]\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a `--` in a payload must not open a line comment that eats "
        "the closing `|]` itself",
        "module M where\n" + _T
        + "s = [text| -- |]\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a `{-` in a payload must not open a block comment either",
        "module M where\n" + _T
        + "s = [text| {- |]\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "an apostrophe in a payload is prose, not a character literal",
        "module M where\n" + _T
        + "s = [text| it's |]\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a MULTILINE payload carrying a quote, closed properly",
        "module M where\n" + _T
        + "s = [glsl|\n"
        "  \" x\n"
        "  |]\n"
        "f x = T.pack (show x)\n",
        [6],
    ),
    (
        "`-->` is an OPERATOR, not a comment: Haskell report SS2.3 opens "
        "a comment only on a dash run the rest of its symbol lexeme "
        "does not continue (PR #2404 review round 2)",
        "module M where\n" + _T
        + "f x = x --> T.pack (show x)\n",
        [3],
    ),
    (
        "`<--` is an operator too -- the dash run BEGAN at a symbol "
        "character, which is the half of the rule a trailing-side-only "
        "check misses",
        "module M where\n" + _T
        + "f x = x <-- T.pack (show x)\n",
        [3],
    ),
    (
        "`--|` continues into a symbol character on the trailing side",
        "module M where\n" + _T
        + "f x = x --| T.pack (show x)\n",
        [3],
    ),
    (
        "a longer operator carrying `--` inside it",
        "module M where\n" + _T
        + "f x = x --<> T.pack (show x)\n",
        [3],
    ),
    (
        "`⊚--` is ONE operator: report SS2.2's symbol set is Unicode, and "
        "this tree writes its own operators from it, so an ASCII-only "
        "test splits the lexeme and hands the `--` on as a comment "
        "opener (PR #2404 review round 3)",
        "module M where\n" + _T
        + "(⊚--) :: Int -> T.Text -> T.Text\n"
        "_ ⊚-- y = y\n"
        "f x = 0 ⊚-- T.pack (show x)\n",
        [5],
    ),
    (
        "report SS2.2's `uniSymbol` is symbols AND punctuation, so a "
        "dash run continuing into an em dash (category Pd) is an "
        "operator too -- and this tree's comment prose is full of em "
        "dashes, so a symbols-only test is not a safe approximation",
        "module M where\n" + _T
        + "(--—) :: Int -> T.Text -> T.Text\n"
        "_ --— y = y\n"
        "f x = 0 --— T.pack (show x)\n",
        [5],
    ),
    (
        "a dash run continuing into a UNICODE symbol is an operator for "
        "the same reason",
        "module M where\n" + _T
        + "f x = x --⊚ T.pack (show x)\n",
        [3],
    ),
    (
        "a Unicode operator followed by a REAL comment: the operator "
        "run ends at the space, so the `--` after it still opens one",
        "module M where\n" + _T
        + "f x = x ⊚ y -- T.pack (show x)\n"
        "g x = T.pack (show x)\n",
        [4],
    ),
    (
        "a Unicode type signature is skipped without hiding the wrapper "
        "under it",
        "module M where\n" + _T
        + "f ∷ Int → Text\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a single-dash `->` is left alone and the wrapper below it is "
        "still read",
        "module M where\n" + _T
        + "f :: Int -> Text\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "an ESCAPED quote inside a string does not end it, so the "
        "opener-shaped text after it is still string content and the "
        "wrapper below stays visible",
        "module M where\n" + _T
        + 'note = "\\" [text|"\n'
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a STRING containing quasiquote-opener text opens nothing, so "
        "the wrapper after it is still reported",
        "module M where\n" + _T
        + "note = \"[glsl| x\"\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a line comment containing opener text opens nothing either",
        "module M where\n" + _T
        + "-- [glsl| x\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a block comment containing opener text opens nothing either",
        "module M where\n" + _T
        + "{- [glsl| x -}\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "the character literal `'\"'` -- real in "
        "Engine.Scripting.Lua.API.Shell -- must be skipped atomically "
        "rather than opening a phantom string",
        "module M where\n" + _T
        + "q = '\"'\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a wrapper OUTSIDE a quasiquote in a file that also contains "
        "one -- masking a splice must not mask the module",
        "module M where\n" + _T
        + "s = [glsl| float x == 1.0; |]\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
]

# `(label, source)` -- each must produce no violation and no refusal.
CLEAN_FIXTURES: list[tuple[str, str]] = [
    (
        "the canonical spelling",
        "module M where\nf x = tshow x\n",
    ),
    (
        "`BC.pack (show …)`: Unit.Atlas.Digest's ByteString digest "
        "material, a different conversion (#2177 out of scope)",
        "module M where\n"
        "import qualified Data.ByteString.Char8 as BC\n"
        "f w = BC.pack (show w)\n",
    ),
    (
        "a ByteString packer beside a real Data.Text import: the "
        "qualifier, not the file, decides",
        "module M where\n" + _T
        + "import qualified Data.ByteString.Char8 as BC\n"
        "f w = BC.pack (show w)\n",
    ),
    (
        "`Data.Text.Lazy`'s packer builds lazy Text, which `tshow` is "
        "not",
        "module M where\n"
        "import qualified Data.Text.Lazy as TL\n"
        "f x = TL.pack (show x)\n",
    ),
    (
        "the alias `T` bound to Data.Text.Encoding -- real in this tree "
        "(Engine.Graphics.Vulkan.Texture.Bindless), and a "
        "qualifier-keyed rule would report it",
        "module M where\n"
        "import qualified Data.Text.Encoding as T\n"
        "f x = T.pack (show x)\n",
    ),
    (
        "a module-local `pack` with no Data.Text import at all",
        "module M where\n"
        "pack :: String -> Text\n"
        "pack = undefined\n"
        "f x = pack (show x)\n",
    ),
    (
        "a QUALIFIED-only import leaves the bare name unbound, so a "
        "local `pack` beside it is not Data.Text's",
        "module M where\n" + _T
        + "pack = undefined\n"
        "f x = pack (show x)\n",
    ),
    (
        "an unqualified import that `hiding`s `pack` does not bring it in",
        "module M where\n"
        "import Data.Text hiding (pack)\n"
        "f x = pack (show x)\n",
    ),
    (
        "an unqualified import list WITHOUT `pack` does not bring it in",
        "module M where\n"
        "import Data.Text (Text)\n"
        "f x = pack (show x)\n",
    ),
    (
        "the same text inside a `--` line comment",
        "module M where\n" + _T
        + "-- once written T.pack (show x)\n"
        "f x = tshow x\n",
    ),
    (
        "the same text inside a nestable block comment",
        "module M where\n" + _T
        + "{- was {- really -} T.pack (show x) -}\n"
        "f x = tshow x\n",
    ),
    (
        "the same text inside a string literal",
        "module M where\n" + _T
        + "note = \"T.pack (show x)\"\n",
    ),
    (
        "the same text inside a QUASIQUOTE in an ordinary production "
        "path -- not ShaderCode.hs, whose name the sibling guard's "
        "masking is keyed to",
        "module M where\n" + _T
        + "s = [text| T.pack (show x) |]\n",
    ),
    (
        "a wrapper inside a payload that ALSO carries a quote stays "
        "inside the payload",
        "module M where\n" + _T
        + "s = [text| \" T.pack (show x) |]\n",
    ),
    (
        "a LONE `-` is subtraction, not a comment: reading it as one "
        "would skip the rest of the line and leave the quasiquote after "
        "it unmasked, reporting its payload as real code",
        "module M where\n" + _T
        + "banner n = tshow (n - 1) <> [text| T.pack (show n) |]\n",
    ),
    (
        "a plain `--` line comment is still a comment",
        "module M where\n" + _T
        + "-- T.pack (show x)\n"
        "f x = tshow x\n",
    ),
    (
        "three or more dashes is still a comment -- the run is nothing "
        "but dashes",
        "module M where\n" + _T
        + "--- T.pack (show x)\n"
        "f x = tshow x\n",
    ),
    (
        "a Haddock `-- |` comment: the `|` follows a SPACE, so the dash "
        "run ends at two and opens a comment",
        "module M where\n" + _T
        + "-- | T.pack (show x)\n"
        "f x = tshow x\n",
    ),
    (
        "an em dash inside comment prose is comment text, not an "
        "operator -- this tree's comments are full of them",
        "module M where\n" + _T
        + "-- note — T.pack (show x)\n"
        "f x = tshow x\n",
    ),
    (
        "a comment closing the file with no trailing newline",
        "module M where\n" + _T
        + "f x = tshow x\n"
        "-- T.pack (show x)",
    ),
    (
        "a `'\"'` character literal BEFORE a quasiquote must be skipped "
        "atomically -- read as an ordinary quote it opens a phantom "
        "string that hides the opener, and the payload's wrapper is "
        "then reported as real code",
        "module M where\n" + _T
        + "q = '\"'\n"
        "s = [text| T.pack (show x) |]\n",
    ),
    (
        "an identifier's trailing prime is not a literal opener: "
        "`f' '\"'` misread that way consumes the real literal's opening "
        "quote and the same phantom string follows",
        "module M where\n" + _T
        + "g = f' '\"'\n"
        "s = [text| T.pack (show x) |]\n",
    ),
    (
        "an UNCLOSED quasiquote ran to end of file, so its tail is "
        "payload -- the source does not compile either way, and a "
        "readable tail would let one phantom opener hide the module",
        "module M where\n" + _T
        + "s = [text| unclosed\n"
        "f x = T.pack (show x)\n",
    ),
    (
        "a quasiquote whose payload spans lines",
        "module M where\n" + _T
        + "s = [glsl|\n"
        "  T.pack (show x)\n"
        "  |]\n",
    ),
    (
        "a LIST COMPREHENSION, which QuasiQuotes makes the spaced form "
        "-- masking it would hide the wrapper beside it",
        "module M where\n" + _T
        + "xs = [ y | y <- ys ]\n"
        "f x = tshow x\n",
    ),
    (
        "`pack` composed with something else before `show` is a "
        "different function",
        "module M where\n" + _T
        + "f = T.pack . map toLower . show\n",
    ),
    (
        "`T.pack` applied to something that merely STARTS with `show`",
        "module M where\n" + _T
        + "f x = T.pack (showFFloat x)\n",
    ),
    (
        "`T.pack` applied to `shows`, a different function",
        "module M where\n" + _T
        + "f x = T.pack (shows x \"\")\n",
    ),
    (
        "`T.unpack`, whose lexeme merely ends in `pack`",
        "module M where\n" + _T
        + "f t = T.unpack t\n",
    ),
    (
        "a longer name merely ENDING in `pack`, with the unqualified "
        "`pack` genuinely in scope beside it -- the left lexeme "
        "boundary is what separates the two",
        "module M where\n"
        "import Data.Text (Text, pack)\n"
        "repack :: String -> Text\n"
        "repack s = pack s\n"
        "f x = repack (show x)\n",
    ),
    (
        "a name that merely starts with `pack`",
        "module M where\n" + _T
        + "f x = T.packed (show x)\n",
    ),
    (
        "`T.pack` with no `show` after it at all",
        "module M where\n" + _T
        + "f p = T.pack p\n",
    ),
    (
        "a record selector spelled `.show` is not the Prelude's",
        "module M where\n" + _T
        + "f r = T.pack (r.show)\n",
    ),
    (
        "conditional CPP only SELECTS text and is not a refusal",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n"
        "#ifdef DARWIN\n" + _T
        + "#endif\n"
        "f x = tshow x\n",
    ),
    (
        "a `module Data.Text` RE-EXPORT in the export list is not an "
        "import, so it is not an unaccounted mention -- UPrelude's own "
        "header carries one",
        "module M\n"
        "  ( module Data.Text\n"
        "  , Text ) where\n"
        "import qualified Data.Text as TXT\n"
        "import Data.Text (Text)\n"
        "f x = tshow x\n",
    ),
    (
        "a Haddock reference to Data.Text does not count as an import, "
        "so it is not an unaccounted mention",
        "module M where\n"
        "-- | Wraps \"Data.Text\" and Data.Text.\n"
        "f x = tshow x\n",
    ),
]

# `(label, source, expected refusal)`
UNSCANNABLE_FIXTURES: list[tuple[str, str, type[_UnscannableSource]]] = [
    (
        "CPP `#define` renaming the very alias the scan resolves by: "
        "the compiler sees `T`, an unpreprocessed scan sees `Txt`, and "
        "the call resolves against neither",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n"
        "#define Txt T\n"
        "import qualified Data.Text as Txt\n"
        "f x = T.pack (show x)\n",
        _PreprocessorError,
    ),
    (
        "CPP `#define` inside what looks like a Haskell comment is "
        "still a real directive, because CPP runs first",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n"
        "{- harmless?\n"
        "#define Txt T\n"
        "-}\n" + _T
        + "f x = tshow x\n",
        _PreprocessorError,
    ),
    (
        "CPP `#undef`, which can un-rename just as destructively",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n"
        "#undef Txt\n" + _T
        + "f x = tshow x\n",
        _PreprocessorError,
    ),
    (
        "CPP `#include`, whose text this scan never sees",
        "{-# LANGUAGE CPP #-}\n"
        "module M where\n"
        "#include \"aliases.h\"\n" + _T
        + "f x = tshow x\n",
        _PreprocessorError,
    ),
    (
        "an alias the resolver cannot read -- GHC accepts a non-ASCII "
        "module alias, and reading only its ASCII head would resolve "
        "every use against the wrong qualifier",
        "module M where\n"
        "import qualified Data.Text as TÉ\n"
        "f x = TÉ.pack (show x)\n",
        _ImportParseError,
    ),
    (
        "an unterminated import list",
        "module M where\n"
        "import qualified Data.Text as T (pack\n"
        "f x = T.pack (show x)\n",
        _ImportParseError,
    ),
    (
        "an INDENTED top-level layout: GHC accepts a module body at any "
        "column, and this import is not collected, so its uses could "
        "not be resolved",
        "module M where\n"
        " import qualified Data.Text as T\n"
        " f x = T.pack (show x)\n",
        _ImportParseError,
    ),
    (
        "an EXPLICIT-BRACE layout with the import inline, which never "
        "starts a line at all",
        "module M where { import qualified Data.Text as T; "
        "f x = T.pack (show x) }\n",
        _ImportParseError,
    ),
]

# `(label, source, lines that must be reported)` -- each run against
# `UPRELUDE_FILE`, where the one construct-scoped exemption applies.
# Both halves of that scoping are load-bearing: the exemption is the
# canonical definition (not the file), and it is that file (not the
# definition anywhere).
UPRELUDE_FIXTURES: list[tuple[str, str, list[int]]] = [
    (
        "UPrelude's own definition, the one exemption",
        "module UPrelude where\n"
        "import qualified Data.Text as TXT\n"
        "tshow :: Show a => a -> Text\n"
        "tshow = TXT.pack ∘ show\n",
        [],
    ),
    (
        "the same definition written with an ASCII `.`",
        "module UPrelude where\n"
        "import qualified Data.Text as TXT\n"
        "tshow = TXT.pack . show\n",
        [],
    ),
    (
        "a second hand-written wrapper elsewhere in UPrelude still "
        "fails: the exemption is the definition, not the file",
        "module UPrelude where\n" + _T
        + "tshow = T.pack . show\n"
        "other x = T.pack (show x)\n",
        [4],
    ),
    (
        "a `where`-bound local named `tshow` is NOT the canonical "
        "definition, and is reported even here",
        "module UPrelude where\n" + _T
        + "f x = g x\n"
        "  where tshow = T.pack . show\n",
        [4],
    ),
    (
        "a definition whose body is not the wrapper at all leaves the "
        "wrapper beside it reported",
        "module UPrelude where\n" + _T
        + "tshow = T.pack . map toUpper . show\n"
        "other x = T.pack (show x)\n",
        [4],
    ),
]


# `(label, UPrelude source, whether the premise check must refuse)`
PREMISE_FIXTURES: list[tuple[str, str, bool]] = [
    (
        "today's UPrelude: `module Data.Text` IS re-exported, but the "
        "module is imported qualified as TXT plus `(Text)`, so the "
        "re-export carries the type and not `pack`",
        "module UPrelude\n"
        "  ( module Data.Text\n"
        "  , module UPrelude ) where\n"
        "import qualified Data.Text as TXT\n"
        "import Data.Text (Text)\n",
        False,
    ),
    (
        "an UNQUALIFIED, unaliased import beside the re-export puts "
        "`pack` in scope both ways, so the re-export carries it",
        "module UPrelude\n"
        "  ( module Data.Text ) where\n"
        "import Data.Text\n",
        True,
    ),
    (
        "the same import listing `pack` explicitly",
        "module UPrelude\n"
        "  ( module Data.Text ) where\n"
        "import Data.Text (Text, pack)\n",
        True,
    ),
    (
        "an unqualified import that `hiding`s `pack`: nothing to carry",
        "module UPrelude\n"
        "  ( module Data.Text ) where\n"
        "import Data.Text hiding (pack)\n",
        False,
    ),
    (
        "an unqualified import ALIASED away: `pack` is in scope bare "
        "and as `TXT.pack`, never as `Data.Text.pack`, so `module "
        "Data.Text` carries nothing",
        "module UPrelude\n"
        "  ( module Data.Text ) where\n"
        "import Data.Text as TXT\n",
        False,
    ),
    (
        "no re-export at all, however the module is imported",
        "module UPrelude ( Text ) where\n"
        "import Data.Text\n",
        False,
    ),
    (
        "a COMMENT mentioning the re-export is not the re-export",
        "module UPrelude\n"
        "  -- never: module Data.Text\n"
        "  ( Text ) where\n"
        "import Data.Text\n",
        False,
    ),
]


def _exemptions_missing_reasons() -> list[str]:
    """Every exemption states its reason inline, the way the sibling
    operator guard's do."""
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

    for label, source, must_refuse in PREMISE_FIXTURES:
        try:
            check_uprelude_premise(source)
        except _PremiseError:
            if not must_refuse:
                failures.append(f"  PREMISE: {label}\n"
                                f"    expected acceptance, got a refusal")
            continue
        if must_refuse:
            failures.append(f"  PREMISE: {label}\n"
                            f"    expected a refusal, got acceptance")

    for label, source, expected in UPRELUDE_FIXTURES:
        try:
            got = [v.line for v in find_violations(source, UPRELUDE_FILE)]
        except _UnscannableSource as error:
            failures.append(f"  UPRELUDE: {label}\n"
                            f"    expected violations on lines {expected}, "
                            f"but the scan refused: {error}")
            continue
        if got != expected:
            failures.append(f"  UPRELUDE: {label}\n"
                            f"    expected violations on lines {expected}, "
                            f"got {got}")

    # The exemption is that FILE's definition. The identical definition
    # in any other module is an ordinary local copy and must fail.
    uprelude_definition = ("module UPrelude where\n" + _T
                           + "tshow = T.pack . show\n")
    if not find_violations(uprelude_definition, "src/Other.hs"):
        failures.append("  EXEMPTION: the canonical definition was exempted "
                        "in a module that is not " + UPRELUDE_FILE)

    # An exemption must actually suppress, and suppress only its path,
    # or the table is decorative.
    probe = "module M where\n" + _T + "f x = T.pack (show x)\n"
    EXEMPTIONS["exempt.hs"] = "self-test probe"
    try:
        if find_violations(probe, "exempt.hs"):
            failures.append("  EXEMPTIONS: an exempt path was still flagged")
        if not find_violations(probe, "fixture.hs"):
            failures.append("  EXEMPTIONS: exempting one path silenced "
                            "another")
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
             + len(UNSCANNABLE_FIXTURES) + len(UPRELUDE_FIXTURES)
             + len(PREMISE_FIXTURES))
    print(f"tshow_spelling_audit self-test: {total} fixtures OK "
          f"({len(DETECTED_FIXTURES)} wrappers detected across every "
          f"spelling, import form and multiline wrap; "
          f"{len(CLEAN_FIXTURES)} clean, including ByteString and lazy "
          f"packers, comments, strings, quasiquotes and near-miss names; "
          f"{len(UNSCANNABLE_FIXTURES)} sources refused rather than "
          f"certified; {len(UPRELUDE_FIXTURES)} exercising the "
          f"construct-scoped UPrelude exemption; {len(PREMISE_FIXTURES)} "
          f"UPrelude export premises).")
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
        print(f"tshow_spelling_audit: {error}")
        return 1
    if violations:
        print(f"{len(violations)} hand-written show-to-Text wrapper(s) in "
              f"{'/, '.join(SCANNED_TREES)}/:")
        for violation in violations:
            print(f"  {violation}")
        print(f"\n`{CANONICAL}` (src/UPrelude.hs) is this project's one "
              f"spelling of the pack-a-shown-value wrapper (#1099, #2177). "
              f"It is definitionally the same function, so the rewrite "
              f"changes no rendered byte.")
        if EXEMPTIONS:
            print("\nExempt by design:")
            for path, reason in EXEMPTIONS.items():
                print(f"  {path}: {reason}")
        return 1
    print(f"No hand-written show-to-Text wrapper found in "
          f"{'/, '.join(SCANNED_TREES)}/ -- every one reads `{CANONICAL}`.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
