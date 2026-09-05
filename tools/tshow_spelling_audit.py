#!/usr/bin/env python3
"""Guard: `src/` + `app/` render a `Show` value to strict `Text` with
`tshow`, never with a hand-written `pack`-a-shown-value wrapper (#2177).

`UPrelude` defines that wrapper once (`src/UPrelude.hs`,
`tshow = TXT.pack ∘ show`), and issue #1099 / PR #1568 replaced the 709
direct copies it found and deleted the 24 local `tshow` definitions.
Nothing enforced the result, and the spelling regressed twice within
nine days of that merge -- `Engine.Asset.YamlRecipes` (PR #1984) and
`Engine.Scripting.Lua.API.Graphics` (PR #2104) -- while the point-free
compositions #1099's acceptance grep never scoped survived in eleven
more files, two of them as renamed copies (`showT`, `tShow`).

THE RULE IS A CLOSED LIST
=========================
This is a SPELLING guard, not an expression analyser. It reports
`pack` applied to `show` written as one of EXACTLY:

    pack (show …)     pack $ show …     pack . show     pack ∘ show

and nothing else about the surrounding expression is read. `$!` counts
as `$`: strict application applies the same function to the same
argument.

That list is exhaustive by design (#2177 §Scope note). An earlier
attempt, PR #2404, read the requirement as an obligation to recognise
every valid Haskell SPELLING of the conversion; twenty review rounds
did not exhaust it, because that is a parsing problem. Operator
sections (`(pack .) show`), prefix operators (`(.) pack show`),
qualified connectors (`pack P.$ P.show`), type ascriptions
(`(pack :: String -> Text) . show`), redundant parentheses
(`pack $ (show x)`), application position (`format pack . show`) and
argument extent (`pack (show x <> " s")`) are all OUT OF SCOPE. A
wrapper written one of those ways is not caught, and that is the
contract; a NON-wrapper reported because of one is answered with an
`EXEMPTIONS` entry, not with more analysis.

WHAT IS RESOLVED
================
Two names, by BINDING rather than by spelling, and a third thing
refused outright.

`pack` must resolve to `Data.Text`'s. This tree binds `T` to
`Data.Text` in most modules -- but in `src/UPrelude.hs` `T` is
`Data.Text.Encoding` and `Data.Text` is `TXT`, and
`Engine.Scripting.Lua.Thread.Console` and `.API.Shell` each bind `T` to
BOTH `Data.Text` and `Data.Text.Read`. A qualifier-keyed rule would
report `UPrelude`'s encoder and miss a `Data.Text` under any other
alias. Imports are read with
`tools/engine_env_capability_writer_syntax.py`'s `parse_imports` /
`imports_name`, the resolver the capability writer scanner already
uses, so an alias, a bare `import qualified Data.Text`, `hiding` and an
`ImportQualifiedPost` declaration all resolve; `Data.ByteString.Char8`'s
packer (`Unit.Atlas.Digest`'s digest material) and `Data.Text.Lazy`'s
are different conversions and are never reported.

`show` must be the `Show` METHOD -- bare, or qualified by one of
`SHOW_MODULES`. Any other qualifier is REFUSED: it may be an unrelated
formatter, whose wrapper is not `tshow`, or another re-export of the
method, whose wrapper is, and deciding needs that module's exports.

The connector must be written UNQUALIFIED. A qualified one is REFUSED,
on the same reasoning: `C.$` may be somebody's own operator.

REFUSAL IS A FIRST-CLASS RESULT
===============================
A file this module cannot certify exits non-zero naming the file and
the reason, rather than being scanned as if it were clean -- the one
failure mode a guard must not have. The cases, each with its own
fixture below:

  * a CPP `#define` / `#undef` / `#include`, which can rename the very
    alias this scan resolves by before GHC sees the module;
  * a `Data.Text` import whose shape the resolver does not model, or a
    `Data.Text` mention outside any import declaration it recognises;
  * an import putting `pack` in UNQUALIFIED scope, whose uses are bare
    names a local binder may legally shadow;
  * a file that BINDS `show` locally, for the same reason;
  * a `show` or a connector qualified by a module this cannot resolve.

`src/UPrelude.hs`'s own export list is checked once per run: were it to
re-export `Data.Text`'s `pack` unqualified, every module would gain a
bare `pack` that no per-file import declares.

EXEMPTION
=========
One, construct-scoped: `tshow`'s canonical definition in
`src/UPrelude.hs`. It is the wrapper, so it must spell itself out. The
exemption covers that ONE `pack` occurrence and not the file -- a
second hand-written wrapper elsewhere in `UPrelude` still fails.
`EXEMPTIONS` is the per-file escape, and suppresses a refusal as well
as a report.

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
    _CHAR_LITERAL, haskell_code_only, is_haskell_ident_char)
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
SHOW = "show"
CANONICAL = "tshow"

UPRELUDE_FILE = "src/UPrelude.hs"

# The modules whose `show` is the `Show` METHOD. `GHC.Show` is where
# `base` defines the class, `Text.Show` and `Prelude` re-export it, and
# `UPrelude` re-exports `module Prelude` -- which is where every bare
# `show` in this tree comes from.
SHOW_MODULES = frozenset({"Prelude", "UPrelude", "GHC.Show", "Text.Show"})

# Exemptions: repo-relative path -> the reason, stated inline the way
# tools/unicode_operator_audit.py states its own. Whole-file, and the
# recorded escape for BOTH a report and a refusal. Deliberately empty:
# the single real exemption is construct-scoped (`_TSHOW_DEFINITION`
# below), so `UPrelude` is covered for its one definition and audited
# everywhere else.
EXEMPTIONS: dict[str, str] = {}

# `tshow`'s canonical definition, matched in code only and anchored at
# column 0 so a `where`-bound local of the same name is NOT exempt.
# Group 1 is the `pack` token itself, which is what makes the exemption
# that one occurrence rather than the line or the file.
_TSHOW_DEFINITION = re.compile(
    r"^" + CANONICAL + r"[ \t]*=[ \t]*(?:[A-Za-z][\w'.]*\.)?(" + PACK + r")"
    r"[ \t]*[.∘][ \t]*" + SHOW + r"[ \t]*$",
    re.MULTILINE)

# Everything `_prepared_code` can leave where source text used to be.
# `\x00` is how a masked comment, string or quasiquote survives in
# place, and `str.strip()` does not remove it -- so an ordinary
# `import qualified Data.Text as T -- rationale` would look like an
# import with trailing text this module could not read.
_BLANK = " \t\r\n\x00"

_GAP = re.compile(r"[\s\x00]*")

# `Data.Text` as a WHOLE module path: never the prefix of
# `Data.Text.Encoding`, and never the qualifier of a use such as
# `Data.Text.pack` (whose trailing `.` fails the lookahead), so the
# accounting below counts import mentions and nothing else.
_TEXT_MODULE_MENTION = re.compile(
    r"(?<![\w'.])" + re.escape(TEXT_MODULE) + r"(?![\w'.])")

# CPP directives that rewrite identifiers before GHC parses them, or
# hide text from this scan. Matched on RAW text: CPP runs first, so a
# `#define` inside what looks like a Haskell comment is still real.
# Conditional directives only SELECT text and are scanned in every
# branch, which over-reports at worst.
_REWRITING_CPP_DIRECTIVE = re.compile(
    r"^[ \t]*#[ \t]*(define|undef|include)\b", re.MULTILINE)


# ---------------------------------------------------------------------
# Haskell lexical predicates
# ---------------------------------------------------------------------

# Report SS2.4 as GHC maps it (`GHC.Parser.Lexer`'s `adjustChar`): a
# conid begins with an UPPERCASE or TITLECASE letter, a varid with a
# LOWERCASE or OTHER letter (or `_`). `str.isupper()` is false for a
# titlecase letter such as `ǅ`, and `str.islower()` is false for an
# `Lo` letter such as `א`, so neither predicate states the rule alone.
_CONID_HEAD_CATEGORIES = frozenset({"Lu", "Lt"})
_VARID_HEAD_CATEGORIES = frozenset({"Ll", "Lo"})

# Haskell 2010 SS2.2's `special` characters, which `uniSymbol` excludes
# along with `_`, `"` and `'`. All ASCII, which is what lets
# `_is_symbol_char` decide every ASCII character from `_SYMBOL_CHARS`
# alone and reach the Unicode tables only for the rest.
_HASKELL_SPECIAL = frozenset("()[]{},;`_\"'")


def _is_conid_head(char: str) -> bool:
    """True if `char` may begin a Haskell CONID -- a module name or a
    constructor."""
    return unicodedata.category(char) in _CONID_HEAD_CATEGORIES


def _is_varid_head(char: str) -> bool:
    """True if `char` may begin a Haskell VARID -- a function or
    variable name, a quasiquoter among them."""
    return char == "_" or unicodedata.category(char) in _VARID_HEAD_CATEGORIES


def _is_symbol_char(char: str) -> bool:
    """True for a character a Haskell symbolic operator is made of.

    Report SS2.2: `symbol -> ascSymbol | uniSymbol<special | _ | " | '>`,
    where `uniSymbol` is ANY Unicode symbol or punctuation. This
    codebase is `UnicodeSyntax` throughout and defines its own operators
    from that set (`⊚`, `⌦`, `∘`, `⚟`), so an ASCII-only test splits a
    lexeme like `⊚--` and hands the trailing `--` on as a comment
    opener. `_SYMBOL_CHARS` is `ascSymbol`, and every `special`
    character is ASCII, so an ASCII character is decided by that set
    alone and the category lookup runs only for the rest."""
    if char.isascii():
        return char in _SYMBOL_CHARS
    return unicodedata.category(char)[0] in "SP"


def _read_ident(text: str, pos: int) -> tuple[str, int] | None:
    """`(lexeme, offset just past it)` for the identifier at `pos`, or
    `None`. Read with `is_haskell_ident_char`, so a combining mark
    stays inside the name."""
    n = len(text)
    if pos >= n or not is_haskell_ident_char(text[pos]):
        return None
    i = pos
    while i < n and is_haskell_ident_char(text[i]):
        i += 1
    return text[pos:i], i


def _is_module_path(candidate: str) -> bool:
    """True if `candidate` (`P`, `Data.Text`, `ǅ`) is a Haskell
    qualified module path: dot-separated segments, each conid-led."""
    if not candidate:
        return False
    return all(seg and _is_conid_head(seg[0])
               and all(is_haskell_ident_char(ch) for ch in seg[1:])
               for seg in candidate.split("."))


def _read_module_path(text: str, pos: int) -> tuple[str, int] | None:
    """`(module path, offset just past it)` at `pos`, or `None`."""
    segments: list[str] = []
    i, n = pos, len(text)
    while i < n and _is_conid_head(text[i]):
        read = _read_ident(text, i)
        if read is None:
            break
        segments.append(read[0])
        k = read[1]
        if k < n and text[k] == "." and k + 1 < n and _is_conid_head(text[k + 1]):
            i = k + 1
            continue
        i = k
        break
    return (".".join(segments), i) if segments else None


def _qualifier_before(text: str, pos: int) -> str:
    """The module qualifier written immediately before `pos` (`"T"`,
    `"Data.Text"`), or `""` for an unqualified occurrence.

    A qualifier is written with no space before its `.`, so it is the
    identifier-and-dot run ending in `.` directly ahead of the name --
    and only when it is a real module path, which separates the
    qualified `T.pack` from the composition `f.pack`. A leading
    Template Haskell name quote is stripped first: `'` is an identifier
    character (`map'`), so `$(varE 'T.pack)` would otherwise present the
    segment `'T`, read as no qualifier at all, and the splice names the
    very function this resolves."""
    start = pos
    while start > 0 and (is_haskell_ident_char(text[start - 1])
                         or text[start - 1] == "."):
        start -= 1
    candidate = text[start:pos]
    if not candidate.endswith("."):
        return ""
    stripped = candidate[:-1].lstrip("'")
    return stripped if _is_module_path(stripped) else ""


# ---------------------------------------------------------------------
# Source preparation
# ---------------------------------------------------------------------

# A quasiquote's payload is not Haskell. Template Haskell's QUOTATION
# brackets are not quasiquotes, though: `[e| |]`, `[t| |]`, `[d| |]`,
# `[p| |]` and the bare `[| |]` hold Haskell, and masking one would
# hide a wrapper written inside it. This tree writes `[t|` and the bare
# form (World.Material.Id, Engine.Graphics.Vulkan.Uniform.Layout), so
# the distinction is live. Only the UNQUALIFIED spellings are brackets
# -- `[Mod.e| … |]` names a quasiquoter called `e`.
_QUASIQUOTE_CLOSE = "|]"
_TH_QUOTATION_NAMES = frozenset({"e", "t", "d", "p"})


def _is_quasiquoter_name(name: str) -> bool:
    """True if `name` (the text between `[` and `|`) names a
    quasiquoter: a varid, optionally module-qualified, that is not one
    of Template Haskell's quotation brackets."""
    segments = name.split(".")
    head, qualifier = segments[-1], segments[:-1]
    if not head or not _is_varid_head(head[0]):
        return False
    if not all(seg and _is_conid_head(seg[0]) for seg in qualifier):
        return False
    return bool(qualifier) or head not in _TH_QUOTATION_NAMES


def _quasiquote_name_end(text: str, pos: int) -> int | None:
    """The offset just past the `|` of a quasiquote opener starting at
    `pos`, or `None` if no opener starts there.

    The name is read with `is_haskell_ident_char` rather than a regex
    character class, because a Haskell identifier may carry a combining
    mark that no `\\w` class matches: an unrecognised opener leaves its
    payload readable as Haskell, and a lone `"` in there then masks the
    rest of the file."""
    if text[pos] != "[":
        return None
    i, n = pos + 1, len(text)
    while i < n and (is_haskell_ident_char(text[i]) or text[i] == "."):
        i += 1
    if i == pos + 1 or i >= n or text[i] != "|":
        return None
    return i + 1


def _mask_spans(text: str, spans: list[tuple[int, int, str]]) -> str:
    """Blank each `[start, end)` span to its own fill character, one for
    one, so every other position -- and every line number -- stays
    valid.

    The fill is part of what the span MEANS. A quasiquote is
    transparent, so it becomes `\\x00`, which every gap here treats as
    whitespace. A masked multi-dash OPERATOR is not: it separates two
    operands, so it becomes `+`, an ASCII symbol character that is not
    one of the connectors."""
    out = list(text)
    for start, end, fill in spans:
        for i in range(start, end):
            if out[i] != "\n":
                out[i] = fill
    return "".join(out)


def _premasked_spans(text: str) -> list[tuple[int, int, str]]:
    """The spans `_prepared_code` blanks before handing the source to
    `haskell_code_only`, in source order.

    Exactly the two constructs that lexer gets WRONG, and nothing else.
    It models `--` line comments, nestable `{- -}` block comments,
    string literals and character literals correctly; masking those a
    second time here would be a second opinion free to drift from it.
    What it does not model is a QUASIQUOTE, and what it gets wrong is a
    multi-dash OPERATOR:

      * a quasiquote's payload is not Haskell at all, so a lone `"` in
        one opens a string that runs past the closing `|]` and swallows
        the code after it, and a `--` in one opens a comment that eats
        the `|]` itself;
      * `-->` is an operator, not a comment. Haskell report SS2.3 opens
        a comment only on a dash run the rest of its symbol lexeme does
        not continue, so `f x = x --> T.pack (show x)` is code and a
        lexer that stops at the `--` masks the wrapper.

    Masking the operator is what leaves the shared lexer no `--` to
    misread, which makes the dash rule here the only one in play. It
    removes nothing reportable: the four spellings this guard matches
    are `(`, `$`, `.` and `∘`, none of them a dash run. A run carrying
    no `--` (`->`, `-`, `.`) is left alone.

    Both sides of the dash run decide it -- `-->` is an operator
    because the run continues into `>`, `<--` because it began at `<`.
    The leading side needs no predicate: a symbol run is consumed WHOLE
    from its first character, and every other arm leaves `i` on a
    character no symbol precedes.

    Comments, strings and character literals are SKIPPED here, never
    recorded: skipping is what stops an opener quoted inside one from
    being read as a real quasiquote boundary. A string GAP (report
    SS2.6's `\\ whitechar {whitechar} \\`, which this tree writes 258
    times) ends at its own backslash and not at the next character, or
    the gap's closer pairs with the string's closing quote and the
    string never ends.

    An UNCLOSED quasiquote ran to end of file and is masked that way.
    That branch is also what makes the walk terminate on every input: a
    recorded span ends at the opener's end or later, and every other arm
    advances `i` by at least one."""
    spans: list[tuple[int, int, str]] = []
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
                if text[i] != "\\":
                    i += 1
                    continue
                j = i + 1
                if j < n and text[j].isspace():
                    while j < n and text[j].isspace():
                        j += 1
                    i = j + 1 if j < n and text[j] == "\\" else j
                else:
                    i = j + 1
            i += 1
            continue
        if char == "'" and (i == 0
                            or not is_haskell_ident_char(text[i - 1])):
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
                    spans.append((i, run, "+"))
                i = run
            continue
        name_end = _quasiquote_name_end(text, i)
        if name_end is not None and _is_quasiquoter_name(text[i + 1:name_end - 1]):
            close = text.find(_QUASIQUOTE_CLOSE, name_end)
            end = n if close == -1 else close + len(_QUASIQUOTE_CLOSE)
            spans.append((i, end, "\x00"))
            i = end
            continue
        i += 1
    return spans


def _prepared_code(text: str) -> str:
    """`text` reduced to the Haskell code this module scans: quasiquotes
    and multi-dash operators masked by the walk above, then
    `haskell_code_only` over the result for comments, strings and
    character literals. Positions and line numbers are preserved, so
    every offset still maps back to the original source."""
    return haskell_code_only(_mask_spans(text, _premasked_spans(text)))


# ---------------------------------------------------------------------
# The wrapper itself
# ---------------------------------------------------------------------

# `pack` is found as a bare occurrence and then bounded by the
# predicate: `unpack` and `pack'` are different names, and so is a
# `pack` carrying a combining mark.
_PACK_LEXEME = re.compile(PACK)

# The CLOSED form list, and the whole of it. `\x00` joins whitespace in
# every gap because `haskell_code_only` blanks each non-code byte to it
# in place, so `pack {- why -} (show x)` is the same expression with a
# null run in the middle. `$!` rides with `$`: strict application
# applies the same function to the same argument.
_WRAPPER_TAIL = re.compile(
    r"[\s\x00]*(?:\((?P<paren>)|\$!?(?P<dollar>)|(?P<compose>[.∘]))"
    r"[\s\x00]*")

_FORM_NAMES = {
    "paren": "pack (show …)",
    "dollar": "pack $ show …",
    "compose": "pack ∘ show",
}

# The connector operators, as maximal symbol lexemes, for the refusal
# below. `.&.` is not `.`, so the run must be exactly one of these.
_CONNECTOR_OPERATORS = frozenset({".", "∘", "$", "$!"})


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


# ---------------------------------------------------------------------
# Refusals
# ---------------------------------------------------------------------

class _UnscannableSource(Exception):
    """Source this guard cannot certify, for any reason.

    Raised rather than swallowed. Every alternative -- scanning a file
    whose `Data.Text` import was not understood, whose `pack` or `show`
    may be locally bound, or whose text the preprocessor rewrites before
    the compiler sees it -- makes the wrappers in that file invisible,
    and a guard that can be silenced by an unusual import is not a
    guard. A refusal is not a claim that the file contains a wrapper; it
    is a refusal to certify that it does not."""


class _PreprocessorError(_UnscannableSource):
    """A file whose text CPP will rewrite before GHC parses it."""


class _ImportParseError(_UnscannableSource):
    """A `Data.Text` import this module cannot classify."""


class _UnqualifiedImportError(_UnscannableSource):
    """An import that puts `Data.Text`'s `pack` in UNQUALIFIED scope.

    Its uses are bare names, and a bare name cannot be told from a local
    binder that legally shadows it without full Haskell scope analysis:
    `f x = let pack = id in pack (show x)` is valid and is not this
    wrapper. A qualified use has no such ambiguity -- a `let` cannot
    bind `T.pack` -- so only the bare case is refused.
    `tools/lua_strict_decode_audit.py` refuses an unqualified
    `Data.Text.Encoding` import for exactly this reason."""


class _ShadowedShowError(_UnscannableSource):
    """A file that BINDS `show` locally.

    `f x = let show _ = "custom" in T.pack (show x)` is valid, is not
    this wrapper, and rewriting it to `tshow` would change both its
    behaviour and its constraints. Deciding which `show` a bare
    occurrence denotes needs full scope analysis, so a file that binds
    the name at all is refused rather than guessed at.

    The shapes recognised are `_SHOW_BINDERS`, a closed set: an equation
    head or parameter at any column, a `let`/`where`-introduced binding,
    a lambda parameter and a `do` binder, with every identifier in them
    read by `is_haskell_ident_char`. An `instance Show … where` method
    is none of those -- it IS the method. A `show` bound by a TUPLE or
    constructor PATTERN is not recognised and cannot be without a
    parser: `(show, y) = …` and `f (show x)` differ only in what
    encloses them. That residual is a false POSITIVE on code shadowing
    `show` that way, with `EXEMPTIONS` as the escape, never a silent
    pass on a wrapper."""


class _UnresolvedShowError(_UnscannableSource):
    """A wrapper whose `show` is qualified by a module this cannot
    resolve.

    `T.pack (C.show x)` is `tshow` when `C` names a module re-exporting
    the `Show` method and is something else entirely when `C` names a
    formatter of its own. Deciding needs that module's exports, which
    this audit does not read, so it refuses -- guessing either way is a
    defect."""


class _QualifiedConnectorError(_UnscannableSource):
    """A wrapper joined by a QUALIFIED connector.

    The closed form list is written unqualified. `T.pack C.$ show x`
    may be `tshow x` and may be built from somebody's own `$` with its
    own semantics, so it is refused rather than read either way."""


class _PremiseError(_UnscannableSource):
    """`UPrelude` no longer matches the premise this scan rests on."""


# ---------------------------------------------------------------------
# Import accounting
# ---------------------------------------------------------------------

_MODULE_KEYWORD = re.compile(r"^module(?![\w'])", re.MULTILINE)
_WHERE_KEYWORD = re.compile(r"(?<![\w'])where(?![\w'])")
_IMPORT_HEAD = re.compile(r"import(?![\w'])")
_QUALIFIED_KEYWORD = re.compile(r"qualified(?![\w'])")
_AS_KEYWORD = re.compile(r"as(?![\w'])")


def _split_module_header(code_text: str) -> tuple[str, int]:
    """`(export-list header, offset where the module body starts)`.

    The header is `module M ( ... ) where`, which is the one place a
    module NAME appears without being an import: `UPrelude`'s export
    list carries `module Data.Text`. Both callers need the split and
    need it drawn the same way -- the premise check reads the header,
    the import accounting reads only the body."""
    keyword = _MODULE_KEYWORD.search(code_text)
    if keyword is None:
        return "", 0
    where = _WHERE_KEYWORD.search(code_text, keyword.end())
    if where is None:
        return code_text, len(code_text)
    return code_text[:where.end()], where.end()


def _established_qualifiers(prepared: str) -> dict[str, frozenset[str]]:
    """Every module qualifier the file's imports establish, mapped to
    the module names it can denote -- the `as` alias where there is
    one, and the module's own name either way.

    This reads the import heads itself rather than taking
    `parse_imports`' aliases, because that resolver's alias grammar is
    ASCII-only: a valid `import qualified Prelude as Ü` establishes no
    qualifier there, and a `Ü.show` would go unresolved. Only the
    QUALIFIER is taken from here; `pack` itself is still resolved by the
    shared resolver, which refuses a `Data.Text` import it cannot
    read."""
    qualifiers: dict[str, set[str]] = {}
    for chunk in _import_chunks(prepared):
        head = _IMPORT_HEAD.match(chunk)
        if head is None:
            continue
        j = _GAP.match(chunk, head.end()).end()
        pre = _QUALIFIED_KEYWORD.match(chunk, j)
        if pre is not None:
            j = _GAP.match(chunk, pre.end()).end()
        module = _read_module_path(chunk, j)
        if module is None:
            continue
        name, j = module
        qualifiers.setdefault(name, set()).add(name)
        j = _GAP.match(chunk, j).end()
        post = _QUALIFIED_KEYWORD.match(chunk, j)
        if post is not None:
            j = _GAP.match(chunk, post.end()).end()
        alias_at = _AS_KEYWORD.match(chunk, j)
        if alias_at is None:
            continue
        alias = _read_module_path(chunk,
                                  _GAP.match(chunk, alias_at.end()).end())
        if alias is not None:
            qualifiers.setdefault(alias[0], set()).add(name)
    return {q: frozenset(m) for q, m in qualifiers.items()}


def _classify_text_imports(prepared: str, rel_path: str) -> None:
    """Raise unless every `Data.Text` import in `prepared` is one the
    resolver reads completely, and unless `pack` stays qualified.

    Two questions, because they fail differently. Is each mention of the
    module inside an import declaration `_import_chunks` collected at
    all -- an indented or explicit-brace top-level layout is valid
    Haskell that it does not collect, and an uncollected import is an
    invisible one. And does the collected declaration's text end after a
    shape the resolver models: nothing, a `hiding` clause, an import
    list? Anything left over means a piece of the declaration -- an
    alias it could not read, most of all -- was dropped."""
    # The module header is excluded: an export list is the one place a
    # module name appears without being an import.
    _, body_start = _split_module_header(prepared)
    code_text = prepared[body_start:]
    accounted = 0
    for chunk in _import_chunks(code_text):
        head = _IMPORT_DECL_RE.match(chunk)
        if head is None or head.group("module") != TEXT_MODULE:
            continue
        accounted += len(_TEXT_MODULE_MENTION.findall(chunk))
        rest = chunk[head.end():].strip(_BLANK)
        if rest.startswith("hiding"):
            rest = rest[len("hiding"):].strip(_BLANK)
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
            f"(a top-level layout that is indented, or in explicit braces). "
            f"An import it cannot see is one whose `{PACK}` uses it cannot "
            f"resolve.")

    if imports_name(parse_imports(prepared), TEXT_MODULE, PACK, ""):
        raise _UnqualifiedImportError(
            f"{rel_path}: this file imports {TEXT_MODULE}'s `{PACK}` "
            f"UNQUALIFIED, so its uses are bare names.\n\n"
            f"tools/tshow_spelling_audit.py resolves `{PACK}` by the binding "
            f"the import establishes, and a BARE occurrence cannot be told "
            f"from a local binder that legally shadows it -- "
            f"`let {PACK} = id in {PACK} (show x)` is valid and is not this "
            f"wrapper -- without full Haskell scope analysis.\n\n"
            f"Import {TEXT_MODULE} qualified, as every import of it in src/ "
            f"and app/ already is, or record this file in EXEMPTIONS with "
            f"the reason it is safe.")


def check_uprelude_premise(uprelude_source: str) -> None:
    """Raise `_PremiseError` if `UPrelude` re-exports `Data.Text`'s
    `pack` under its UNQUALIFIED name.

    Every module in this tree imports `UPrelude` (`NoImplicitPrelude` is
    global), so such a re-export would put a bare `pack` in scope
    everywhere WITHOUT any per-file import declaring it -- and this
    module resolves a bare `pack` from the importing file's own import
    declarations.

    `UPrelude`'s export list does carry `module Data.Text`, and that is
    NOT the same thing. Report SS5.2: `module M` exports exactly the
    entities in scope under BOTH an unqualified name and the qualified
    name `M.e`. `UPrelude` imports the module twice -- qualified as
    `TXT`, which supplies no unqualified name, and `Data.Text (Text)`,
    which supplies the type alone -- so the re-export carries `Text` and
    not `pack`. Both halves of the report's rule are tested here."""
    header, _ = _split_module_header(_prepared_code(uprelude_source))
    if re.search(r"(?<![\w'])module\s+" + re.escape(TEXT_MODULE)
                 + r"(?![\w'.])", header) is None:
        return
    declarations = parse_imports(uprelude_source)
    if (imports_name(declarations, TEXT_MODULE, PACK, "")
            and imports_name(declarations, TEXT_MODULE, PACK, TEXT_MODULE)):
        raise _PremiseError(
            f"{UPRELUDE_FILE} re-exports `module {TEXT_MODULE}` while "
            f"`{PACK}` is in scope there both unqualified and as "
            f"`{TEXT_MODULE}.{PACK}`, so the re-export carries it (report "
            f"SS5.2) and every module in the tree gains a bare `{PACK}`. "
            f"tools/tshow_spelling_audit.py resolves a bare `{PACK}` from "
            f"the importing file's OWN import declarations, so it would now "
            f"under-report.")


# ---------------------------------------------------------------------
# `show`, and the shadows of it
# ---------------------------------------------------------------------

_LINE_INDENT = re.compile(r"^[ \t\x00]*", re.MULTILINE)
_HORIZONTAL_GAP = re.compile(r"[ \t\x00]*")
_METHOD_BLOCK_HEAD = re.compile(r"(?:instance|class)(?![\w'])")


def _opens_method_block(text: str, pos: int) -> bool:
    """True if the declaration containing `pos` belongs to an `instance`
    or `class` block, whose `show` is the `Show` METHOD rather than a
    local binding shadowing it.

    Decided by the nearest preceding COLUMN-ZERO line, which is where a
    Haskell top-level declaration begins; an `instance … where` head may
    wrap across lines, and every continuation line is indented."""
    line_start = text.rfind("\n", 0, pos) + 1
    while line_start > 0:
        if line_start < len(text) and text[line_start] not in " \t\n":
            break
        line_start = text.rfind("\n", 0, line_start - 1) + 1
    return _METHOD_BLOCK_HEAD.match(text, line_start) is not None


def _reads_maximal(text: str, pos: int, lexeme: str) -> bool:
    """True if the maximal symbol run at `pos` is exactly `lexeme`, so
    `==`, `=>` and `=<<` are not `=`."""
    run = pos
    while run < len(text) and _is_symbol_char(text[run]):
        run += 1
    return text[pos:run] == lexeme


def _read_parameters(text: str, pos: int) -> tuple[list[str], int]:
    """`(identifier parameters, offset past them)` from `pos`, separated
    by horizontal whitespace.

    The names are returned because a PARAMETER called `show` binds it
    just as a head of that name does: `f show x = …` shadows the method
    inside `f`."""
    names: list[str] = []
    i = pos
    while True:
        gap = _HORIZONTAL_GAP.match(text, i).end()
        parameter = _read_ident(text, gap)
        if parameter is None:
            return names, gap
        names.append(parameter[0])
        i = parameter[1]


def _show_equation_binding(prepared: str) -> int | None:
    """Where an equation binds `show` -- as its HEAD or as one of its
    PARAMETERS, at ANY column, so a `let`/`where` block's second binding
    counts as much as a top-level one -- or `None`.

    An `instance`/`class` method definition is excluded: it IS the
    method.

    No fixture claims an independent failure mode for `_reads_maximal`
    here, and deliberately so: layout puts every continuation line at a
    deeper column, so a column-zero declaration beginning `show
    <params>` can only continue with `=` or `::`. The rule is here
    because it is what an equation head means."""
    for indent in _LINE_INDENT.finditer(prepared):
        start = indent.end()
        name = _read_ident(prepared, start)
        if name is None:
            continue
        parameters, after = _read_parameters(prepared, name[1])
        if name[0] != SHOW and SHOW not in parameters:
            continue
        if not _reads_maximal(prepared, after, "="):
            continue
        if (name[0] == SHOW and start > indent.start()
                and _opens_method_block(prepared, start)):
            continue
        return start
    return None


def _show_keyword_binding(prepared: str) -> int | None:
    """Where a `let` or `where` on the SAME LINE introduces `show` --
    `f x = let show _ = "c" in …`, which starts no line of its own."""
    for found in re.finditer(r"(?<![\w'])(?P<keyword>let|where)(?![\w'])",
                             prepared):
        cursor = found.end()
        while cursor < len(prepared) and prepared[cursor] in " \t\x00{;":
            cursor += 1
        name = _read_ident(prepared, cursor)
        if name is None or name[0] != SHOW:
            continue
        if (found.group("keyword") == "where"
                and _opens_method_block(prepared, found.start())):
            continue
        return found.start()
    return None


def _show_lambda_binding(prepared: str) -> int | None:
    """Where a lambda binds `show` as a parameter, or `None`.

    Every repetition of the parameter run consumes at least one
    character, so the walk cannot backtrack exponentially."""
    for found in re.finditer(r"\\", prepared):
        i, binds = found.end(), False
        while True:
            gap = _GAP.match(prepared, i).end()
            parameter = _read_ident(prepared, gap)
            if parameter is None:
                i = gap
                break
            binds = binds or parameter[0] == SHOW
            i = parameter[1]
        if binds and (_reads_maximal(prepared, i, "->")
                      or _reads_maximal(prepared, i, "→")):
            return found.start()
    return None


def _show_do_binding(prepared: str) -> int | None:
    """Where a `do` statement binds `show`, or `None`."""
    for found in re.finditer(SHOW, prepared):
        start, end = found.start(), found.end()
        if start > 0 and is_haskell_ident_char(prepared[start - 1]):
            continue
        if end < len(prepared) and is_haskell_ident_char(prepared[end]):
            continue
        if _reads_maximal(prepared, _GAP.match(prepared, end).end(), "<-"):
            return start
    return None


_SHOW_BINDERS = (("an equation head or parameter", _show_equation_binding),
                 ("a `let`/`where` binding", _show_keyword_binding),
                 ("a lambda parameter", _show_lambda_binding),
                 ("a `do` binder", _show_do_binding))


def _refuse_shadowed_show(prepared: str, rel_path: str) -> None:
    """Raise `_ShadowedShowError` if `prepared` binds `show`."""
    for shape, detect in _SHOW_BINDERS:
        found = detect(prepared)
        if found is None:
            continue
        raise _ShadowedShowError(
            f"{rel_path}:{_line_of(prepared, found)}: this file binds "
            f"`{SHOW}` as {shape}, shadowing the `Show` method.\n\n"
            f"tools/tshow_spelling_audit.py reads a bare `{SHOW}` as that "
            f"method, and `{CANONICAL}` is defined in terms of it, so a "
            f"wrapper written in the shadow's scope is NOT the same "
            f"function -- rewriting it would change behaviour and "
            f"constraints.\n\nRename the local binding, or record this file "
            f"in EXEMPTIONS with the reason it is safe.")


def _names_show(text: str, pos: int, qualifiers: dict[str, frozenset[str]],
                rel_path: str) -> bool:
    """True if the lexeme at `pos` is the `Show` method `show` -- bare,
    or qualified by a module that exports it.

    Read character by character rather than by a regex class: a module
    alias may carry a combining mark (`Ṕ́.show`), which no `\\w` class
    matches. Splitting on `.` after the fact is what separates the
    qualified `P.show` from the record selector `t.show`, whose prefix
    is not a module path. A qualifier that IS a module path but names
    none of `SHOW_MODULES` is refused rather than guessed."""
    i, n = pos, len(text)
    while i < n and (is_haskell_ident_char(text[i]) or text[i] == "."):
        i += 1
    qualifier, _, head = text[pos:i].rpartition(".")
    if head != SHOW:
        return False
    if not qualifier:
        return True
    if not _is_module_path(qualifier):
        # A lowercase prefix is a record selector, not a qualifier.
        return False
    named = qualifiers.get(qualifier, frozenset())
    if named & SHOW_MODULES:
        return True
    raise _UnresolvedShowError(
        f"{rel_path}:{_line_of(text, pos)}: a wrapper here renders through "
        f"`{qualifier}.{SHOW}`, and `{qualifier}` names "
        + (", ".join(sorted(named)) if named
           else "no module this file imports")
        + f".\n\ntools/tshow_spelling_audit.py resolves `{SHOW}` to the "
        f"`Show` method, which {', '.join(sorted(SHOW_MODULES))} export. A "
        f"`{SHOW}` from anywhere else may be an unrelated formatter -- whose "
        f"wrapper is NOT `{CANONICAL}` -- or another re-export of the method "
        f"-- whose wrapper is.\n\nQualify it by one of those modules, or "
        f"record this file in EXEMPTIONS with the reason it is safe.")


def _refuse_qualified_connector(text: str, pos: int, rel_path: str) -> None:
    """Raise `_QualifiedConnectorError` if a QUALIFIED connector follows
    `pos`.

    The closed form list is written unqualified, so `T.pack C.$ show x`
    is not read -- but it may be this wrapper, and silently passing it
    would leave the guard bypassable by qualifying an operator. It is
    refused instead, and the exemption table is the escape."""
    cursor = _GAP.match(text, pos).end()
    module = _read_module_path(text, cursor)
    if module is None:
        return
    qualifier, after = module
    if after >= len(text) or text[after] != ".":
        return
    run = after + 1
    while run < len(text) and _is_symbol_char(text[run]):
        run += 1
    operator = text[after + 1:run]
    if operator not in _CONNECTOR_OPERATORS:
        return
    raise _QualifiedConnectorError(
        f"{rel_path}:{_line_of(text, pos)}: a wrapper here is joined by "
        f"`{qualifier}.{operator}`, a QUALIFIED connector.\n\n"
        f"tools/tshow_spelling_audit.py reads the closed form list of "
        f"#2177, which is written unqualified, and `{qualifier}.{operator}` "
        f"may be the standard operator or somebody's own with its own "
        f"semantics.\n\nWrite the connector unqualified, or record this file "
        f"in EXEMPTIONS with the reason it is safe.")


# ---------------------------------------------------------------------
# The scan
# ---------------------------------------------------------------------

def _line_of(text: str, pos: int) -> int:
    return text.count("\n", 0, pos) + 1


def find_violations(text: str, rel_path: str) -> list[Violation]:
    """Every hand-written show-to-Text wrapper in `text` (the source of
    the file at repo-relative `rel_path`), in the closed form list and
    outside comments, string literals, quasiquotes and this module's
    exemptions.

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

    scan_text = _prepared_code(text)
    _classify_text_imports(scan_text, rel_path)
    _refuse_shadowed_show(scan_text, rel_path)
    # From the SAME masked text, never the raw source: a column-zero
    # `import qualified Data.Text as T` inside a quasiquote payload is
    # not an import, and resolving against it would bind `T` here.
    declarations = parse_imports(scan_text)
    qualifiers = _established_qualifiers(scan_text)

    exempt: set[int] = set()
    if rel_path == UPRELUDE_FILE:
        exempt = {m.start(1) for m in _TSHOW_DEFINITION.finditer(scan_text)}

    violations: list[Violation] = []
    for match in _PACK_LEXEME.finditer(scan_text):
        # A maximal identifier LEXEME, decided by the predicate rather
        # than a `\w` class. No fixture claims an independent failure
        # mode for the LEADING half: a bare `pack` is refused rather
        # than resolved, so a `pack` found inside `unpack` would carry
        # the qualifier `T.un`, which is not a module path, and be
        # dropped a step later anyway. It is here because it is what
        # "the identifier `pack`" means.
        if ((match.start() > 0
             and is_haskell_ident_char(scan_text[match.start() - 1]))
                or (match.end() < len(scan_text)
                    and is_haskell_ident_char(scan_text[match.end()]))):
            continue
        if match.start() in exempt:
            continue
        qualifier = _qualifier_before(scan_text, match.start())
        if not imports_name(declarations, TEXT_MODULE, PACK, qualifier):
            continue
        tail = _WRAPPER_TAIL.match(scan_text, match.end())
        if tail is None:
            _refuse_qualified_connector(scan_text, match.end(), rel_path)
            continue
        if not _names_show(scan_text, tail.end(), qualifiers, rel_path):
            continue
        form = next(name for name in _FORM_NAMES
                    if tail.group(name) is not None)
        violations.append(Violation(
            rel_path, _line_of(text, match.start()),
            f"{qualifier}.{PACK}" if qualifier else PACK, form))
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
        "`T.pack ∘ show`, the Unicode composition this codebase writes",
        "module M where\n" + _T + "f = T.pack ∘ show\n",
        [3],
    ),
    (
        "`$!` is the same application of the same function",
        "module M where\n" + _T + "f x = T.pack $! show x\n",
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
        "TWO modules sharing one alias, only one of them Data.Text -- "
        "real in Engine.Scripting.Lua.Thread.Console",
        "module M where\n" + _T
        + "import qualified Data.Text.Read as T\n"
        "f x = T.pack (show x)\n",
        [4],
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
        "two wrappers on ONE line are two violations",
        "module M where\n" + _T
        + "f x y = T.pack (show x) <> T.pack (show y)\n",
        [3, 3],
    ),
    (
        "a QUALIFIED `show`: `P.show` is the same method",
        "module M where\n" + _T
        + "import qualified Prelude as P\n"
        "f x = T.pack (P.show x)\n",
        [4],
    ),
    (
        "`GHC.Show` is where `base` DEFINES the class",
        "module M where\n" + _T
        + "import qualified GHC.Show as S\n"
        "f x = T.pack (S.show x)\n",
        [4],
    ),
    (
        "and `Text.Show` re-exports it",
        "module M where\n" + _T
        + "import qualified Text.Show as S\n"
        "f x = T.pack (S.show x)\n",
        [4],
    ),
    (
        "`UPrelude` re-exports `module Prelude`, which is where every "
        "bare `show` in this tree comes from",
        "module M where\n" + _T
        + "import qualified UPrelude as U\n"
        "f x = T.pack (U.show x)\n",
        [4],
    ),
    (
        "a UNICODE module alias on `show`: GHC accepts one, and the "
        "shared resolver's alias grammar is ASCII-only",
        "module M where\n" + _T
        + "import qualified Prelude as Ü\n"
        "f x = T.pack (Ü.show x)\n",
        [4],
    ),
    (
        "a TITLECASE module alias: GHC's conid head is uppercase OR "
        "titlecase (`Lt`), and `str.isupper()` is false for `ǅ`",
        "module M where\n" + _T
        + "import qualified Prelude as ǅ\n"
        "f x = T.pack (ǅ.show x)\n",
        [4],
    ),
    (
        "an `instance Show … where` method is the `Show` method, not a "
        "shadow of it, so the file is scanned normally",
        "module M where\n" + _T
        + "data F = F\n"
        "instance Show F where\n"
        "  show _ = \"F\"\n"
        "g x = T.pack (show x)\n",
        [6],
    ),
    (
        "and neither is a `class … where` method declaration",
        "module M where\n" + _T
        + "class C a where\n"
        "  show :: a -> String\n"
        "g x = T.pack (show x)\n",
        [5],
    ),
    (
        "`-->` is an OPERATOR, not a comment: Haskell report SS2.3 opens "
        "a comment only on a dash run the rest of its symbol lexeme does "
        "not continue",
        "module M where\n" + _T
        + "f x = x --> T.pack (show x)\n",
        [3],
    ),
    (
        "`<--` is an operator too -- the dash run BEGAN at a symbol "
        "character, which is the half a trailing-side-only check misses",
        "module M where\n" + _T
        + "f x = x <-- T.pack (show x)\n",
        [3],
    ),
    (
        "`⊚--` is ONE operator: report SS2.2's symbol set is Unicode, "
        "and this tree writes its own operators from it",
        "module M where\n" + _T
        + "(⊚--) :: Int -> T.Text -> T.Text\n"
        "_ ⊚-- y = y\n"
        "f x = 0 ⊚-- T.pack (show x)\n",
        [5],
    ),
    (
        "report SS2.2's `uniSymbol` is symbols AND punctuation, so a "
        "dash run continuing into an em dash (category Pd) is an "
        "operator too -- and this tree's comment prose is full of them",
        "module M where\n" + _T
        + "(--—) :: Int -> T.Text -> T.Text\n"
        "_ --— y = y\n"
        "f x = 0 --— T.pack (show x)\n",
        [5],
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
        "Template Haskell's `[t| … |]` is a QUOTATION BRACKET, not a "
        "quasiquote: its body is Haskell, so masking it would hide the "
        "wrapper inside. This tree writes them (World.Material.Id)",
        "module M where\n" + _T
        + "x = [t| T.pack (show y) |]\n",
        [3],
    ),
    (
        "the same for `[e|` and `[d|`",
        "module M where\n" + _T
        + "x = [e| T.pack (show y) |]\n"
        "z = [d| g = T.pack (show y) |]\n",
        [3, 4],
    ),
    (
        "the BARE `[| … |]` bracket, which this tree also writes",
        "module M where\n" + _T
        + "x = [| T.pack (show y) |]\n",
        [3],
    ),
    (
        "a tight list comprehension over a CONSTRUCTOR: `[Nothing|` is "
        "not a quasiquoter -- a quasiquoter name is a varid",
        "module M where\n" + _T
        + "ys = [Nothing|x<-xs]\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a lone `\"` in a quasiquote PAYLOAD must not open a Haskell "
        "string that runs past the closing `|]` and swallows the "
        "wrapper below it",
        "module M where\n" + _T
        + "s = [text| \" |]\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a `--` in a payload must not open a line comment that eats the "
        "closing `|]` itself",
        "module M where\n" + _T
        + "s = [text| -- |]\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a BLOCK comment containing a quasiquote opener opens nothing: "
        "read as a real opener it masks to end of file and hides the "
        "wrapper below",
        "module M where\n" + _T
        + "{- [glsl| x -}\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "and a LINE comment containing one opens nothing either",
        "module M where\n" + _T
        + "-- [glsl| x\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a STRING containing quasiquote-opener text opens nothing",
        "module M where\n" + _T
        + "note = \"[glsl| x\"\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a string GAP ends at its own backslash: read as a two-character "
        "escape, the gap's closer pairs with the string's closing quote "
        "and everything below is masked",
        "module M where\n" + _T
        + 's = "a\\\n\\"\n'
        "f x = T.pack (show x)\n",
        [5],
    ),
    (
        "the shape this tree writes 258 times -- a message split across "
        "lines by a gap",
        "module M where\n" + _T
        + 's = "first \\\n'
        '      \\ second"\n'
        "f x = T.pack (show x)\n",
        [5],
    ),
    (
        "an ESCAPED quote is not a gap: the string ends at its real "
        "closing quote and the wrapper below it is reported",
        "module M where\n" + _T
        + 's = "she said \\"hi\\""\n'
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
        "a UNICODE identifier's trailing prime is part of the name: read "
        "as a char-literal opener it eats the quote after it",
        "module M where\n" + _T
        + "g = let π́' x = x in π́'\"'\"\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "an import declaration inside a QUASIQUOTE payload is not an "
        "import: resolving against it would bind `T` and report a "
        "ByteString packer",
        "module M where\n"
        "import qualified Data.ByteString.Char8 as BC\n" + _T
        + "s = [text|\n"
        "import qualified Data.Text as BC\n"
        "|]\n"
        "f x = T.pack (show x)\n"
        "g w = BC.pack (show w)\n",
        [7],
    ),
    (
        "a wrapper on an import carrying a trailing comment",
        "module M where\n"
        "import qualified Data.Text as T -- rationale\n"
        "f x = T.pack (show x)\n",
        [3],
    ),
    (
        "a comment between `hiding` and its list is masked in place, so "
        "the text after the keyword needs the same blanks stripped",
        "module M where\n"
        "import qualified Data.Text as T hiding {- why -} (unpack)\n"
        "f x = T.pack (show x)\n",
        [3],
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
        "material, a different conversion",
        "module M where\n"
        "import qualified Data.ByteString.Char8 as BC\n"
        "f w = BC.pack (show w)\n",
    ),
    (
        "a ByteString packer beside a real Data.Text import: the "
        "binding, not the file, decides",
        "module M where\n" + _T
        + "import qualified Data.ByteString.Char8 as BC\n"
        "f w = BC.pack (show w)\n",
    ),
    (
        "`Data.Text.Lazy`'s packer builds lazy Text, which `tshow` is not",
        "module M where\n"
        "import qualified Data.Text.Lazy as TL\n"
        "f x = TL.pack (show x)\n",
    ),
    (
        "the alias `T` bound to Data.Text.Encoding -- real in "
        "src/UPrelude.hs, and a qualifier-keyed rule would report it",
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
        "an unqualified import that `hiding`s `pack` does not bring it "
        "in, so the file is scanned rather than refused",
        "module M where\n"
        "import Data.Text hiding (pack)\n"
        "f x = pack (show x)\n",
    ),
    (
        "an unqualified import list WITHOUT `pack` does not bring it in "
        "either -- UPrelude's own `import Data.Text (Text)` is this "
        "shape, and refusing it would refuse the tree",
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
        "a gapped string BEFORE a quasiquote: if the pre-pass runs "
        "past the string's real closing quote it never sees the "
        "opener, leaves the payload unmasked, and reports it as code",
        "module M where\n" + _T
        + 's = "a\\\n\\"\n'
        "q = [text| T.pack (show x) |]\n",
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
        "a varid may begin with an `Lo` letter -- GHC maps OtherLetter "
        "to `small`, and `str.islower()` is false for one",
        "module M where\n" + _T
        + 's = [אq| T.pack (show x) |]\n',
    ),
    (
        "a QUALIFIED operator that is not one of the four connectors "
        "is not refused: only a connector spelling is ambiguous",
        "module M where\n" + _T
        + "import qualified Data.Semigroup as S\n"
        "f x = T.pack S.<> show x\n",
    ),
    (
        "a quasiquote whose payload spans lines",
        "module M where\n" + _T
        + "s = [glsl|\n"
        "  T.pack (show x)\n"
        "  |]\n",
    ),
    (
        "a quasiquoter with a UNICODE name is still a quasiquote",
        "module M where\n" + _T
        + "s = [tëxt| T.pack (show x) |]\n",
    ),
    (
        "`[Mod.e| … |]` names a quasiquoter called `e`, which is a "
        "quasiquote and not Template Haskell's bracket",
        "module M where\n" + _T
        + "s = [Mod.e| T.pack (show x) |]\n",
    ),
    (
        "a GAPPED string whose body contains the wrapper text is still "
        "a literal, in both of its halves",
        "module M where\n" + _T
        + 's = "T.pack (show x) \\\n'
        '      \\ more"\n'
        "f x = tshow x\n",
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
        "an em dash inside comment prose is comment text",
        "module M where\n" + _T
        + "-- note — T.pack (show x)\n"
        "f x = tshow x\n",
    ),
    (
        "a LONE `-` is subtraction, not a comment: reading it as one "
        "would skip the rest of the line and leave the quasiquote after "
        "it unmasked",
        "module M where\n" + _T
        + "banner n = tshow (n - 1) <> [text| T.pack (show n) |]\n",
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
        "a Haddock reference to Data.Text does not count as an import",
        "module M where\n"
        "-- | Wraps \"Data.Text\" and Data.Text.\n"
        "f x = tshow x\n",
    ),
    (
        "a `module Data.Text` RE-EXPORT in the export list is not an "
        "import -- UPrelude's own header carries one",
        "module M\n"
        "  ( module Data.Text\n"
        "  , Text ) where\n"
        "import qualified Data.Text as TXT\n"
        "import Data.Text (Text)\n"
        "f x = tshow x\n",
    ),
    (
        "OUT OF SCOPE by the closed list: an operator SECTION is not "
        "one of the four forms (#2177 §Out of scope)",
        "module M where\n" + _T
        + "f = (T.pack .) show\n",
    ),
    (
        "OUT OF SCOPE: a PREFIX-spelled connector",
        "module M where\n" + _T
        + "f = (.) T.pack show\n",
    ),
    (
        "OUT OF SCOPE: a TYPE ASCRIPTION between the packer and the "
        "connector",
        "module M where\n" + _T
        + "f = (T.pack :: String -> T.Text) . show\n",
    ),
    (
        "OUT OF SCOPE: redundant parentheses around the shown value",
        "module M where\n" + _T
        + "f x = T.pack $ (show x)\n",
    ),
]

# `(label, source, expected refusal)`
UNSCANNABLE_FIXTURES: list[tuple[str, str, type]] = [
    (
        "CPP `#define` renaming the very alias the scan resolves by",
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
        "column, and this import is not collected",
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
    (
        "an import putting `pack` in UNQUALIFIED scope: its uses are "
        "bare names, and `let pack = id in pack (show x)` is valid and "
        "is not this wrapper",
        "module M where\n"
        "import Data.Text (Text, pack)\n"
        "f x = let pack = id in pack (show x)\n",
        _UnqualifiedImportError,
    ),
    (
        "the same refusal without a shadowing binder in sight -- the "
        "file is refused for what its imports ALLOW",
        "module M where\n"
        "import Data.Text (Text, pack)\n"
        "f x = pack (show x)\n",
        _UnqualifiedImportError,
    ),
    (
        "an unqualified import with no list at all brings `pack` in too",
        "module M where\n"
        "import Data.Text\n"
        "f x = pack (show x)\n",
        _UnqualifiedImportError,
    ),
    (
        "an unqualified import `hiding` something ELSE still brings "
        "`pack` in",
        "module M where\n"
        "import Data.Text hiding (unpack)\n"
        "f x = pack (show x)\n",
        _UnqualifiedImportError,
    ),
    (
        "a `let`-bound `show`: valid, not this wrapper, and rewriting "
        "it would change behaviour and constraints",
        "module M where\n" + _T
        + 'f x = let show _ = "c" in T.pack (show x)\n',
        _ShadowedShowError,
    ),
    (
        "a `where`-bound one",
        "module M where\n" + _T
        + "f x = T.pack (show x)\n"
        '  where show _ = "c"\n',
        _ShadowedShowError,
    ),
    (
        "a top-level equation head",
        "module M where\n" + _T
        + 'show _ = "c"\n'
        "g x = T.pack (show x)\n",
        _ShadowedShowError,
    ),
    (
        "an equation PARAMETER named `show` binds it inside that "
        "equation just as a head of that name would",
        "module M where\n" + _T
        + "f show x = T.pack (show x)\n",
        _ShadowedShowError,
    ),
    (
        "a `let` block whose `show` binding is not the FIRST one: an "
        "equation head is recognised at any column",
        "module M where\n" + _T
        + "f x = let y = 1\n"
        '          show _ = "c"\n'
        "      in T.pack (show x)\n",
        _ShadowedShowError,
    ),
    (
        "a lambda parameter",
        "module M where\n" + _T
        + "f = \\show -> T.pack (show 1)\n",
        _ShadowedShowError,
    ),
    (
        "a lambda parameter carrying a COMBINING MARK beside it: a "
        "binder identifier read with a narrower class ends early",
        "module M where\n" + _T
        + "f = \\π́ show -> T.pack (show 1)\n",
        _ShadowedShowError,
    ),
    (
        "an equation head whose PARAMETER carries one",
        "module M where\n" + _T
        + 'show π́ = "c"\n'
        "g x = T.pack (show x)\n",
        _ShadowedShowError,
    ),
    (
        "a `do` binder",
        "module M where\n" + _T
        + "f = do\n"
        "  show <- act\n"
        "  pure (T.pack (show 1))\n",
        _ShadowedShowError,
    ),
    (
        "a `show` qualified by a module this cannot read: `Custom.show` "
        "may be an unrelated formatter -- whose wrapper is NOT `tshow` "
        "-- or another re-export of the method -- whose wrapper is",
        "module M where\n" + _T
        + "import qualified Custom as C\n"
        "f x = T.pack (C.show x)\n",
        _UnresolvedShowError,
    ),
    (
        "and one qualified by a module the file does not import at all",
        "module M where\n" + _T
        + "f x = T.pack (C.show x)\n",
        _UnresolvedShowError,
    ),
    (
        "a QUALIFIED connector: the closed form list is written "
        "unqualified, and `C.$` may be the standard operator or "
        "somebody's own",
        "module M where\n" + _T
        + "import qualified Custom as C\n"
        "f x = T.pack C.$ show x\n",
        _QualifiedConnectorError,
    ),
    (
        "the qualified composition, whose qualifier's dot and operator "
        "are both `.`",
        "module M where\n" + _T
        + "import qualified Prelude as P\n"
        "f = T.pack P.. P.show\n",
        _QualifiedConnectorError,
    ),
]

# `(label, source, lines that must be reported)` -- each run against
# `UPRELUDE_FILE`, where the one construct-scoped exemption applies.
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
        "an unqualified import that `hiding`s `pack`: nothing to carry",
        "module UPrelude\n"
        "  ( module Data.Text ) where\n"
        "import Data.Text hiding (pack)\n",
        False,
    ),
    (
        "an unqualified import ALIASED away: `pack` is never in scope "
        "as `Data.Text.pack`, so the re-export carries nothing",
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


def _fixture_shape_failures() -> list[str]:
    """Every fixture whose SOURCE lost its line breaks.

    Such a fixture is VACUOUS, not failing: its import stops being an
    import declaration and the file is refused for a reason that has
    nothing to do with the rule under test."""
    out: list[str] = []
    for group, table, single_line_ok in (
            ("DETECTED", DETECTED_FIXTURES, ()),
            ("CLEAN", CLEAN_FIXTURES, ()),
            ("UNSCANNABLE", UNSCANNABLE_FIXTURES, ("an EXPLICIT-BRACE",)),
            ("UPRELUDE", UPRELUDE_FIXTURES, ())):
        for entry in table:
            label, source = entry[0], entry[1]
            if "\\n" in source:
                out.append(f"  {group}: {label}\n"
                           f"    source carries a LITERAL backslash-n; its "
                           f"line breaks were escaped away")
            elif ("\n" not in source.rstrip("\n")
                    and not label.startswith(single_line_ok)):
                out.append(f"  {group}: {label}\n"
                           f"    source is a single line; a fixture module "
                           f"needs its line breaks")
    return out


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

    # The exemption is that FILE's definition. The identical definition
    # in any other module is an ordinary local copy and must fail.
    uprelude_definition = ("module UPrelude where\n" + _T
                           + "tshow = T.pack . show\n")
    if not find_violations(uprelude_definition, "src/Other.hs"):
        failures.append("  EXEMPTION: the canonical definition was exempted "
                        "in a module that is not " + UPRELUDE_FILE)

    # An EXEMPTIONS entry must actually suppress, suppress only its path,
    # and suppress a REFUSAL as well as a report -- or requirement 4's
    # recorded escape is not mechanically enforced.
    report_probe = "module M where\n" + _T + "f x = T.pack (show x)\n"
    refusal_probe = ("module M where\n"
                     "import Data.Text (Text, pack)\n"
                     "f x = pack (show x)\n")
    EXEMPTIONS["exempt.hs"] = "self-test probe"
    try:
        if find_violations(report_probe, "exempt.hs"):
            failures.append("  EXEMPTIONS: an exempt path was still flagged")
        if not find_violations(report_probe, "fixture.hs"):
            failures.append("  EXEMPTIONS: exempting one path silenced "
                            "another")
        try:
            find_violations(refusal_probe, "exempt.hs")
        except _UnscannableSource:
            failures.append("  EXEMPTIONS: an exempt path was still refused")
    finally:
        del EXEMPTIONS["exempt.hs"]

    for path in _exemptions_missing_reasons():
        failures.append(f"  EXEMPTIONS['{path}'] carries no inline reason")

    failures.extend(_fixture_shape_failures())

    # And the fixture-shape check is probed too: a checker nothing can
    # fail is a comment.
    for probe, complaint in ((("probe", "module M where\\nf = 1\\n", []),
                              "LITERAL backslash-n"),
                             (("probe", "module M where f = 1", []),
                              "single line")):
        DETECTED_FIXTURES.append(probe)
        try:
            caught = [line for line in _fixture_shape_failures()
                      if complaint in line]
        finally:
            DETECTED_FIXTURES.pop()
        if not caught:
            failures.append(f"  FIXTURE SHAPE: a fixture source that is "
                            f"{complaint} was not reported")

    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for failure in failures:
            print(failure)
        return 1
    total = (len(DETECTED_FIXTURES) + len(CLEAN_FIXTURES)
             + len(UNSCANNABLE_FIXTURES) + len(UPRELUDE_FIXTURES)
             + len(PREMISE_FIXTURES))
    print(f"tshow_spelling_audit self-test: {total} fixtures OK "
          f"({len(DETECTED_FIXTURES)} wrappers detected across the closed "
          f"form list, every import and `show` form, and the lexical traps; "
          f"{len(CLEAN_FIXTURES)} clean, including ByteString and lazy "
          f"packers, comments, strings, quasiquotes, near-miss names and "
          f"the out-of-scope spellings; {len(UNSCANNABLE_FIXTURES)} sources "
          f"refused rather than certified; {len(UPRELUDE_FIXTURES)} "
          f"exercising the construct-scoped UPrelude exemption; "
          f"{len(PREMISE_FIXTURES)} UPrelude export premises).")
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
