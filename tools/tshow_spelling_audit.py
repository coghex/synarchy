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
  * `import Data.Text (pack)`             -> REFUSED, not resolved: a
    bare name cannot be told from a local binder that shadows it.
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
  * an import putting `pack` in UNQUALIFIED scope, whose uses are bare
    names a local binder may legally shadow;
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
#
# The qualifier class is ASCII here, unlike `_WRAPPER_TAIL`'s, and the
# difference is not an oversight: this pattern only ever sees a
# qualifier bound to `Data.Text`, and a Unicode ALIAS for it is refused
# by `_classify_text_imports` before the scan runs at all. There is no
# reachable Unicode case to widen for.
_TSHOW_DEFINITION = re.compile(
    r"^" + CANONICAL + r"[ \t]*=[ \t]*(?:[A-Za-z][\w'.]*\.)?(" + PACK + r")"
    r"[ \t]*[.∘][ \t]*show[ \t]*$",
    re.MULTILINE)

# `Data.Text` as a WHOLE module path: never the prefix of
# `Data.Text.Encoding`, and never the qualifier of a use such as
# `Data.Text.pack` (whose trailing `.` fails the lookahead), so the
# refusal check below counts import mentions and nothing else.
# Everything `_prepared_code` can leave where source text used to be.
# `\x00` is how a masked comment, string or quasiquote survives in
# place, and `str.strip()` does not remove it -- so an ordinary
# `import qualified Data.Text as T -- rationale` looked like an import
# with trailing text this module could not read, and was refused
# (PR #2404 review round 6).
_BLANK = " \t\r\n\x00"

_TEXT_MODULE_MENTION = re.compile(
    r"(?<![\w'.])" + re.escape(TEXT_MODULE) + r"(?![\w'.])")

# CPP directives that rewrite identifiers before GHC parses them, or
# hide text from this scan. Matched on RAW text: CPP runs first, so a
# `#define` inside what looks like a Haskell comment is still real.
_REWRITING_CPP_DIRECTIVE = re.compile(
    r"^[ \t]*#[ \t]*(define|undef|include)\b", re.MULTILINE)

# A quasiquote OPENER: `[` immediately followed by a quasiquoter name
# and `|`. The name is captured loosely and validated by
# `_is_quasiquoter_name`, because it is a Haskell identifier and those
# are not ASCII. The payload runs to the first `|]` and is not Haskell.
# See the module docstring for why the no-space form is unambiguous.
_QUASIQUOTE_CLOSE = "|]"


def _quasiquote_name_end(text: str, pos: int) -> int | None:
    """The offset just past the `|` of a quasiquote opener starting at
    `pos`, or `None` if no opener starts there.

    The name is read with `is_haskell_ident_char` rather than a regex
    character class, because a Haskell identifier may carry a combining
    mark that no `\\w` class matches (PR #2404 review round 9): an
    unrecognised opener leaves its payload readable as Haskell, and a
    lone `"` in there then masks the rest of the file."""
    if text[pos] != "[":
        return None
    i, n = pos + 1, len(text)
    while i < n and (is_haskell_ident_char(text[i]) or text[i] == "."):
        i += 1
    if i == pos + 1 or i >= n or text[i] != "|":
        return None
    return i + 1

# Template Haskell's quotation brackets. `[e| |]`, `[t| |]`, `[d| |]`,
# `[p| |]` and the bare `[| |]` are NOT quasiquotes: their bodies are
# Haskell, and masking one would hide a wrapper written inside it. This
# tree uses `[t|` and the bare form (World.Material.Id,
# Engine.Graphics.Vulkan.Uniform.Layout), so the distinction is live and
# not hypothetical. Only the UNQUALIFIED spellings are brackets --
# `[Mod.e| … |]` names a quasiquoter called `e` and is a quasiquote.
_TH_QUOTATION_NAMES = frozenset({"e", "t", "d", "p"})


def _is_quasiquoter_name(name: str) -> bool:
    """True if `name` (the text between `[` and `|`) names a
    quasiquoter: a varid, optionally module-qualified, that is not one
    of Template Haskell's quotation brackets.

    A varid begins with a lowercase or other letter (or `_`) and a
    module segment with an uppercase or titlecase one, decided by
    `_is_varid_head` / `_is_conid_head` rather than by an ASCII class or
    by `str.islower()` / `str.isupper()` -- GHC accepts non-ASCII
    identifiers, and a name this did not recognize would leave a real
    quasiquote unmasked, whose payload could then open a string that
    swallows the code after it."""
    segments = name.split(".")
    head, qualifier = segments[-1], segments[:-1]
    if not head or not _is_varid_head(head[0]):
        return False
    if not all(seg and _is_conid_head(seg[0]) for seg in qualifier):
        return False
    return bool(qualifier) or head not in _TH_QUOTATION_NAMES

# `pack` as a maximal identifier lexeme. `'` is an identifier character
# in Haskell, so it joins `\w` in both lookarounds: `pack'` is a
# different name and `unpack` is not this one.
#
# No fixture claims an independent failure mode for the lexeme-boundary
# test applied to each match below, and that is deliberate rather than
# an omission: since a bare `pack` is refused rather than resolved
# (`_UnqualifiedImportError`), a `pack` found inside `unpack` would
# carry the qualifier `T.un`, which is not a module path, and be
# dropped a step later anyway. The test is there because it is what
# "the identifier `pack`" means, not because a reachable input depends
# on it.
_PACK_LEXEME = re.compile(PACK)

# What must follow a `pack` for the occurrence to be a show-to-Text
# wrapper. `\x00` joins whitespace in every gap because `haskell_code_only`
# blanks each non-code byte to it in place, so `pack {- why -} (show x)`
# is the same expression with a null run in the middle.
#
# REDUNDANT PARENTHESES around `show` are transparent and admitted:
# `pack $ (show x)`, `pack . (show)` and `pack ((show x))` are the same
# function as the unparenthesised spellings, byte for byte (PR #2404
# review round 5). Only parentheses that open with NOTHING between them
# count -- `pack (f (show x))` puts `f` in the way and is a different
# function, so the run stops at it.
#
# `$!` joins `$` for the same reason: strict application applies `pack`
# to `show x` and produces the same Text.
#
# An optional qualifier on `show` is admitted by `\w` -- Python's own,
# hence Unicode-aware -- and then validated by `_is_module_path`, so
# `P.show` and `Ü.show` count while a `t.show` record selector does not.
# An ASCII-only class here would miss a Unicode module alias, which GHC
# accepts and this UnicodeSyntax tree may well write.
_GAP = re.compile(r"[\s\x00]*")

# The connector OPERATORS, as maximal symbol lexemes. `.&.` is not `.`,
# so the run must be exactly one of these.
_CONNECTOR_OPERATORS = {".": "compose", "∘": "compose",
                        "$": "dollar", "$!": "dollar"}


_IMPORT_HEAD = re.compile(r"import(?![\w'])")
_QUALIFIED_KEYWORD = re.compile(r"qualified(?![\w'])")
_AS_KEYWORD = re.compile(r"as(?![\w'])")


def _read_module_path(text: str, pos: int) -> tuple[str, int] | None:
    """`(module path, offset just past it)` at `pos`, or `None`.

    Read with `_is_conid_head` / `is_haskell_ident_char` rather than an
    ASCII class, which is the whole point of it existing separately from
    the shared resolver's."""
    segments: list[str] = []
    i, n = pos, len(text)
    while i < n and _is_conid_head(text[i]):
        k = i
        while k < n and is_haskell_ident_char(text[k]):
            k += 1
        segments.append(text[i:k])
        if k < n and text[k] == "." and k + 1 < n and _is_conid_head(text[k + 1]):
            i = k + 1
            continue
        i = k
        break
    return (".".join(segments), i) if segments else None


def _established_qualifiers(prepared: str) -> dict[str, frozenset[str]]:
    """Every module qualifier the file's imports establish, mapped to
    the module names it can denote -- the `as` alias where there is
    one, and the module's own name either way.

    This reads the import heads itself rather than taking
    `parse_imports`' aliases, because that resolver's alias grammar is
    ASCII-only: a valid `import qualified Prelude as Ü` establishes no
    qualifier there, so `T.pack Ü.$ Ü.show x` -- a direct wrapper --
    went unread (PR #2404 review round 14). Only the QUALIFIER is taken
    from here; `pack` itself is still resolved by the shared resolver,
    which refuses a `Data.Text` import it cannot read."""
    qualifiers: dict[str, set[str]] = {}
    for chunk in _import_chunks(prepared):
        i = _IMPORT_HEAD.match(chunk)
        if i is None:
            continue
        j = _GAP.match(chunk, i.end()).end()
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
        alias = _read_module_path(chunk, _GAP.match(chunk, alias_at.end()).end())
        if alias is not None:
            qualifiers.setdefault(alias[0], set()).add(name)
    return {q: frozenset(m) for q, m in qualifiers.items()}


def _read_connector(text: str, pos: int,
                    qualifiers: frozenset[str]) -> tuple[str, int] | None:
    """`(form, offset just past it)` for a connector OPERATOR at `pos`,
    or `None`.

    The operator may carry a module qualifier: `T.pack P.$ P.show x`
    and `T.pack P.. P.show` are valid, compile to the same function, and
    were missed while only the bare spellings were read (PR #2404 review
    round 13). `P..` is the awkward one -- the qualifier's separating
    dot and the operator are both `.` -- so the qualifier is read
    segment by segment rather than by a greedy class.

    A qualifier is honoured only when the file ESTABLISHES it, which is
    what keeps this from reading an arbitrary `Foo.$` as Prelude's. The
    operator itself is a maximal symbol run, so `.&.` is not `.`."""
    i, n = pos, len(text)
    segments: list[str] = []
    j = i
    while j < n and is_haskell_ident_char(text[j]) and _is_conid_head(text[j]):
        k = j
        while k < n and is_haskell_ident_char(text[k]):
            k += 1
        if k >= n or text[k] != ".":
            break
        segments.append(text[j:k])
        j = k + 1
    if segments:
        qualifier = ".".join(segments)
        if qualifier not in qualifiers:
            return None
    else:
        j = i
    run = j
    while run < n and _is_symbol_char(text[run]):
        run += 1
    form = _CONNECTOR_OPERATORS.get(text[j:run])
    return None if form is None else (form, run)

# The redundant openers admitted between the connector and `show`, and
# the gap after them.
_TRANSPARENT_OPEN_RUN = re.compile(r"(?:[\s\x00]*\()*[\s\x00]*")


# The modules whose `show` is the `Show` METHOD. `GHC.Show` is where
# `base` defines the class, `Text.Show` and `Prelude` re-export it, and
# `UPrelude` re-exports `module Prelude` -- which is where every bare
# `show` in this tree comes from.
#
# A qualifier naming anything else is not resolved either way: it may be
# somebody's own formatter, whose wrapper is NOT `tshow` and must not be
# reported (PR #2404 review round 16), or another re-exporter of the
# method, whose wrapper IS and must not be missed (round 17). Telling
# those apart means reading the other module, so the file is REFUSED --
# which closes the question rather than guessing it one module at a
# time.
SHOW_MODULES = frozenset({"Prelude", "UPrelude", "GHC.Show", "Text.Show"})


def _names_show(text: str, pos: int,
                qualifiers: dict[str, frozenset[str]]) -> bool:
    """True if the lexeme at `pos` is the `Show` method `show` -- bare,
    or qualified by a module that exports it.

    Read character by character with `is_haskell_ident_char` rather than
    by a regex class: a module alias may carry a combining mark
    (`Ṕ́.show`), which no `\\w` class matches, and the wrapper would then
    go unreported (PR #2404 review round 9). Splitting on `.` after the
    fact is what separates the qualified `P.show` from the record
    selector `t.show`, whose prefix is not a module path."""
    i, n = pos, len(text)
    while i < n and (is_haskell_ident_char(text[i]) or text[i] == "."):
        i += 1
    lexeme = text[pos:i]
    qualifier, _, head = lexeme.rpartition(".")
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
        f"a wrapper here renders through `{qualifier}.{SHOW}`, and "
        f"`{qualifier}` names "
        + (f"{', '.join(sorted(named))}" if named else "no module this "
           "file imports")
        + f".\n\ntools/tshow_spelling_audit.py resolves `{SHOW}` to the "
        f"`Show` method, which {', '.join(sorted(SHOW_MODULES))} export. "
        f"A `{SHOW}` from anywhere else may be an unrelated formatter -- "
        f"whose wrapper is NOT `{CANONICAL}` -- or another re-export of "
        f"the method -- whose wrapper is. Deciding needs that module's "
        f"exports, which this audit does not read.\n\nQualify it by one "
        f"of those modules, or record this file in EXEMPTIONS with the "
        f"reason it is safe.")

# A closing parenthesis, and whatever whitespace precedes it, between
# `pack` and the connector after it. Consumed only as many times as
# `_transparent_open_parens` found matching openers before the
# expression, so `(T.pack) . show` reads as the wrapper it is while
# `g (h (T.pack)) . show` -- where the parentheses belong to `g` and
# `h`, not to `pack` -- does not.
_TRANSPARENT_CLOSE = re.compile(r"[\s\x00]*\)")

# A TYPE ASCRIPTION on the packer. `(pack :: String -> Text) . show` is
# the same function -- the ascription names the type `pack` already has
# -- and the connector sits after the group's closing parenthesis
# rather than after the name (PR #2404 review round 19). `∷` is the
# spelling this tree writes.
_ASCRIPTION = re.compile(r"[\s\x00]*(?:::|∷)")


def _past_ascription(text: str, pos: int) -> int | None:
    """The offset just past the `)` closing the group whose ascription
    begins at `pos`, or `None` if it never closes.

    Parenthesis depth is tracked, so a type spelling its own
    (`(pack :: (String) -> Text)`) does not end the group early. The
    type's text is not read: nothing about it changes what `pack` is."""
    depth, i, n = 0, pos, len(text)
    while i < n:
        if text[i] == "(":
            depth += 1
        elif text[i] == ")":
            if depth == 0:
                return i + 1
            depth -= 1
        i += 1
    return None

# An OPERATOR SECTION puts the connector at the edge of a parenthesised
# expression instead of between its operands, and both halves of that
# are still this wrapper (PR #2404 review round 11):
#
#   (pack .) show      (pack $) (show x)     -- LEFT sections, applied
#   (. show) pack      ($ show x) pack       -- RIGHT sections, applied
#
# GHC infers `Show a => a -> Text` for every one, so each is `tshow`
# spelled out. The left forms are read by letting a section's closing
# `)` sit between the connector and `show`, drawing on the same opener
# budget `_transparent_open_parens` computes. The right forms put
# `show` BEFORE `pack`, so they need their own backward look.
#
# The backward look reads only a section it can see whole: it walks
# back over identifier characters, connectors and gaps to the opening
# `(`, and gives up at anything else -- a nested bracket, a literal.
# That is a conservative MISS on an exotic section rather than a
# balance count that a `')'` character literal could throw off.
_SECTION_SCANNABLE = frozenset(".∘$!")


# Haskell 2010 SS2.4's reserved words. One of these before an
# expression is a keyword introducing it, not a function applying to
# it: `let … in T.pack (show x)` and `if p then T.pack (show x)` are
# both standalone wrappers, and reading `in`/`then` as an applicand
# hid them.
RESERVED_WORDS = frozenset({
    "case", "class", "data", "default", "deriving", "do", "else",
    "foreign", "if", "import", "in", "infix", "infixl", "infixr",
    "instance", "let", "module", "newtype", "of", "then", "type",
    "where", "_"})


def _read_ident(text: str, pos: int) -> tuple[str, int] | None:
    """`(lexeme, offset just past it)` for the identifier at `pos`, or
    `None`.

    Read with `is_haskell_ident_char`, so a combining mark stays inside
    the name -- which is the whole reason this exists rather than a
    `\\w` class (PR #2404 review rounds 9 and 18)."""
    n = len(text)
    if pos >= n or not is_haskell_ident_char(text[pos]):
        return None
    i = pos
    while i < n and is_haskell_ident_char(text[i]):
        i += 1
    return text[pos:i], i


def _ident_ending_at(text: str, pos: int) -> str:
    """The identifier lexeme ending just past `pos`, or `""`."""
    if pos < 0 or not is_haskell_ident_char(text[pos]):
        return ""
    start = pos
    while start > 0 and is_haskell_ident_char(text[start - 1]):
        start -= 1
    return text[start:pos + 1]


def _in_function_position(text: str, pos: int) -> bool:
    """True if nothing is being APPLIED to the expression starting at
    `pos` -- that is, the character before it is not something an
    application could have as its function.

    Application is left-associative juxtaposition, so `f (. show) T.pack`
    hands `f` two independent arguments and is not a section applied to
    the packer at all (PR #2404 review round 12). The same test decides
    whether a prefix operator is being applied or is itself an
    argument.

    A RESERVED WORD before the expression introduces it rather than
    applying to it, so `let … in T.pack (show x)` and
    `if p then T.pack (show x)` are standalone."""
    i = pos - 1
    while i >= 0 and (text[i].isspace() or text[i] == "\x00"):
        i -= 1
    if i < 0:
        return True
    if is_haskell_ident_char(text[i]):
        return _ident_ending_at(text, i) in RESERVED_WORDS
    return text[i] not in _APPLICAND_TAIL


# A connector written in PREFIX form: `(.) pack show` is `pack . show`
# and `($) pack (show x)` is `pack (show x)`, both with the operator
# named rather than placed between its operands (PR #2404 review round
# 12). The operator itself must be in function position, or
# `g (.) pack show` -- three arguments to `g` -- would read as one.
def _prefix_operator_before(text: str, pos: int,
                            qualifiers: dict[str, frozenset[str]]
                            ) -> tuple[str, int] | None:
    """`(form, offset of the prefix operator's own `(`)` for the
    prefix-spelled connector applied immediately before `pos`, or
    `None`.

    `(.) pack show` names the operator ahead of both operands, and
    `(P.$) pack (show x)` does the same with it qualified. The operator
    must itself be in function position, or `g (.) pack show` -- three
    arguments to `g` -- would read as one."""
    # Skip the operand's OWN opening parentheses on the way back:
    # `(.) (pack) show` puts them between the operator and the operand,
    # and they are redundant there (PR #2404 review round 15).
    i = pos - 1
    while i >= 0 and (text[i].isspace() or text[i] == "\x00"
                      or text[i] == "("):
        i -= 1
    if i < 0 or text[i] != ")":
        return None
    # No fixture claims an independent failure mode for that `)` test,
    # and deliberately so: `_read_connector` below must consume the
    # whole of `inner`, so reaching this code with anything else before
    # the operand needs an unbalanced parenthesis, which no source GHC
    # accepts has. It is here because it is what closes a prefix
    # operator -- the same reasoning as `_PACK_LEXEME`'s boundary test.
    start = text.rfind("(", 0, i)
    if start < 0 or not _in_function_position(text, start):
        return None
    inner = text[start + 1:i]
    read = _read_connector(inner, 0, frozenset(qualifiers))
    if read is None or read[1] != len(inner):
        return None
    return read[0], start


def _right_section_before(text: str, pos: int,
                          qualifiers: dict[str, frozenset[str]]) -> bool:
    """True if a right section applying `show` sits immediately before
    `pos` IN FUNCTION POSITION -- `(. show)` or `($ show …)`, whose
    operand is therefore the expression at `pos`.

    Function position is what makes it an application rather than a
    sibling argument: `f (. show) T.pack` hands `f` two arguments and is
    not this wrapper (PR #2404 review round 12)."""
    i = pos - 1
    while i >= 0 and (text[i].isspace() or text[i] == "\x00"):
        i -= 1
    if i < 0 or text[i] != ")":
        return False
    j = i - 1
    while j >= 0 and (is_haskell_ident_char(text[j])
                      or text[j] in _SECTION_SCANNABLE
                      or text[j].isspace() or text[j] == "\x00"):
        j -= 1
    if j < 0 or text[j] != "(" or not _in_function_position(text, j):
        return False
    content = text[j + 1:i]
    head = _read_connector(content, _GAP.match(content, 0).end(),
                           frozenset(qualifiers))
    if head is None:
        return False
    return _names_show(content, _GAP.match(content, head[1]).end(),
                       qualifiers)


_FORM_NAMES = {
    "paren": "pack (show …)",
    "dollar": "pack $ show …",
    "compose": "pack ∘ show",
    "section": "(∘ show) pack",
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


class _UnqualifiedImportError(_UnscannableSource):
    """An import that puts `Data.Text`'s `pack` in UNQUALIFIED scope.

    Its uses are bare names, and a bare name cannot be told from a
    local binder that legally shadows it without full Haskell scope
    analysis: `f x = let pack = id in pack (show x)` is valid, returns
    `String`, and is not this wrapper at all (PR #2404 review round 6).
    A qualified use has no such ambiguity -- a `let` cannot bind
    `T.pack` -- so only the bare case is refused.

    `tools/lua_strict_decode_audit.py` refuses an unqualified
    `Data.Text.Encoding` import for exactly this reason, and this is
    the same rule for the same reason. A refusal is not a claim that
    the file contains a wrapper; it is a refusal to certify that it
    does not, which is a non-zero exit naming the file rather than a
    silent pass. Importing `Data.Text` qualified -- as every import of
    it in `src/` and `app/` already is -- resolves it."""


class _ShadowedShowError(_UnscannableSource):
    """A file that BINDS `show` locally.

    `f x = let show _ = "custom" in T.pack (show x)` is valid, is not
    this wrapper, and rewriting it to `tshow` would change both its
    behaviour and its constraints (PR #2404 review round 13). Deciding
    which `show` a bare occurrence denotes needs full scope analysis, so
    a file that binds the name at all is refused rather than guessed at
    -- the same treatment `_UnqualifiedImportError` gives a bare `pack`.

    The binding shapes recognized are an equation head (`show … =`) at
    any column, a `let`/`where`-introduced binding, a lambda parameter
    and a `do` binder, with every identifier in them read by
    `is_haskell_ident_char` so a combining mark cannot end one early. An `instance Show … where`
    method definition is none of those -- it IS the method -- and is
    excluded by both the column-zero anchor and `_opens_method_block`. A `show` bound by a TUPLE or constructor PATTERN is not
    recognized, and cannot be without a parser: `(show, y) = …` and
    `f (show x)` differ only in what encloses them. That residual is a
    false POSITIVE on code that shadows `show` that way -- loud, and
    `EXEMPTIONS` is the recorded escape -- never a silent pass on a
    wrapper."""


class _UnresolvedShowError(_UnscannableSource):
    """A wrapper whose `show` is qualified by a module this cannot
    resolve.

    `T.pack (C.show x)` is `tshow` when `C` names a module re-exporting
    the `Show` method and is something else entirely when `C` names a
    formatter of its own. Deciding needs that module's exports, which
    this audit does not read, so it refuses rather than guessing --
    guessing either way is a defect (PR #2404 review rounds 16 and
    17)."""


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


def _mask_spans(text: str, spans: list[tuple[int, int, str]]) -> str:
    """Blank each `[start, end)` span to its own fill character, one
    for one, so every other position -- and every line number -- stays
    valid.

    The fill is part of what the span MEANS. A comment or a quasiquote
    is transparent, so it becomes `\\x00`, which every gap here treats as
    whitespace. A masked multi-dash OPERATOR is not transparent -- it
    separates two operands, and reading through it would make the right
    one look like an argument of the left (PR #2404 review round 16) --
    so it becomes `+`, an ASCII symbol character that is not one of the
    connectors."""
    out = list(text)
    for start, end, fill in spans:
        for i in range(start, end):
            if out[i] != "\n":
                out[i] = fill
    return "".join(out)


def _line_of(text: str, pos: int) -> int:
    return text.count("\n", 0, pos) + 1


# Report SS2.4 as GHC maps it (`GHC.Parser.Lexer`'s `adjustChar`): a
# conid begins with an UPPERCASE or TITLECASE letter, a varid with a
# LOWERCASE or OTHER letter (or `_`). `str.isupper()` is false for a
# titlecase letter such as `ǅ`, and `str.islower()` is false for an
# `Lo` letter such as `א`, so neither predicate states the rule on its
# own (PR #2404 review round 10).
_CONID_HEAD_CATEGORIES = frozenset({"Lu", "Lt"})
_VARID_HEAD_CATEGORIES = frozenset({"Ll", "Lo"})


def _is_conid_head(char: str) -> bool:
    """True if `char` may begin a Haskell CONID -- a module name or a
    constructor."""
    return unicodedata.category(char) in _CONID_HEAD_CATEGORIES


def _is_varid_head(char: str) -> bool:
    """True if `char` may begin a Haskell VARID -- a function or
    variable name, a quasiquoter among them."""
    return char == "_" or unicodedata.category(char) in _VARID_HEAD_CATEGORIES


def _is_module_path(candidate: str) -> bool:
    """True if `candidate` (`P`, `Data.Text`, `ǅ`) is a Haskell
    qualified module path: dot-separated segments, each conid-led."""
    if not candidate:
        return False
    return all(seg and _is_conid_head(seg[0])
               and all(is_haskell_ident_char(ch) for ch in seg[1:])
               for seg in candidate.split("."))


def _qualifier_before(text: str, pos: int) -> tuple[str, int]:
    """`(module qualifier written immediately before `pos`, where the
    qualified expression starts)` -- `("T", …)`, `("Data.Text", …)`, or
    `("", pos)` for an unqualified occurrence.

    A qualifier is written with no space before its `.`, so it is the
    `[\\w'.]` run ending in `.` directly ahead of the name -- and only
    when it is a real module path, which is what separates the
    qualified `T.pack` from the composition `f.pack`. A leading Template
    Haskell name quote is stripped first: `'` is an identifier character
    (`map'`), so `$(varE 'T.pack)` would otherwise present the segment
    `'T`, read as no qualifier at all, and the splice names the very
    function this resolves."""
    start = pos
    while start > 0 and (is_haskell_ident_char(text[start - 1])
                         or text[start - 1] == "."):
        start -= 1
    candidate = text[start:pos]
    if not candidate.endswith("."):
        return "", pos
    stripped = candidate[:-1].lstrip("'")
    if not _is_module_path(stripped):
        return "", pos
    # The expression begins at the qualifier, not at `pack`, so a
    # parenthesis before `T.pack` is one this occurrence can close.
    return stripped, pos - len(candidate) + (len(candidate) - 1 - len(stripped))


# What an opening parenthesis is an ARGUMENT to, if anything sits
# directly before it. Juxtaposition is application in Haskell, so
# `format (T.pack)` applies `format` -- the parentheses group the
# argument, they do not wrap the whole expression, and
# `format (T.pack) . show` is `(format T.pack) . show` rather than
# `T.pack . show` (PR #2404 review round 10). A backtick closes an
# infix application and makes the next parentheses its right operand
# the same way.
_APPLICAND_TAIL = frozenset(')]`"')


def _open_parens_before(text: str, pos: int) -> int:
    """How many `(` sit immediately before `pos`, with only whitespace
    between them -- whether they group or supply an argument.

    `_transparent_open_parens` adds the question of whether they GROUP,
    which is what an infix connector needs (`format (pack) . show` is a
    different function). A prefix connector does not: its operand's
    position is fixed by the form, so `(.) (pack) show` is `(.) pack
    show` and the operand's own parentheses are redundant either way
    (PR #2404 review round 15)."""
    count = 0
    i = pos - 1
    while i >= 0:
        if text[i].isspace() or text[i] == "\x00":
            i -= 1
        elif text[i] == "(":
            count += 1
            i -= 1
        else:
            break
    return count


def _transparent_open_parens(text: str, pos: int) -> int:
    """How many `(` sit immediately before `pos`, with only whitespace
    between them, and GROUP rather than supply an argument.

    Each such parenthesis is one the occurrence at `pos` may close again
    without changing what the expression means, which is what makes
    `(T.pack) . show` the same wrapper as `T.pack . show`. Counting
    them -- rather than skipping any `)` that happens to follow -- is
    what keeps `g (h (T.pack)) . show` out: those parentheses were
    opened by `g` and `h`, so only one of the two closers after `pack`
    is transparent and the second is not the connector.

    Whatever sits before the outermost `(` decides whether they group at
    all. An identifier, a closing bracket or a backtick means the
    parentheses are somebody's ARGUMENT and the expression is not
    standalone, so none of them is transparent and the count is zero;
    an operator, a `=`, a `[`, or the start of the file leaves them
    grouping."""
    count = _open_parens_before(text, pos)
    if count == 0:
        return 0
    i = pos - 1
    while i >= 0 and (text[i].isspace() or text[i] == "\x00"
                      or text[i] == "("):
        i -= 1
    # The loop above already skipped whitespace, so it broke on the
    # first character that is neither space nor `(` -- the applicand
    # candidate itself.
    if i >= 0 and (is_haskell_ident_char(text[i])
                   or text[i] in _APPLICAND_TAIL):
        return 0
    return count


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


def _premasked_spans(text: str) -> list[tuple[int, int, str]]:
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
                if text[i] != "\\":
                    i += 1
                    continue
                # Report SS2.6: a backslash followed by WHITESPACE is a
                # string GAP (`\ whitechar {whitechar} \`), not an
                # escape. Consumed as a two-character escape, the gap's
                # closing backslash pairs with the string's own closing
                # quote and the string never ends -- masking every
                # wrapper to end of file (PR #2404 review round 7).
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
            # The closing `]` is left visible: a quasiquote is a VALUE,
            # so something written directly after it is applying to it,
            # and `_APPLICAND_TAIL` recognises `]`. Masking it away
            # would make that application invisible.
            spans.append((i, max(i, end - 1), "\x00"))
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

    if imports_name(parse_imports(prepared), TEXT_MODULE, PACK, ""):
        raise _UnqualifiedImportError(
            f"{rel_path}: this file imports {TEXT_MODULE}'s `{PACK}` "
            f"UNQUALIFIED, so its uses are bare names.\n\n"
            f"tools/tshow_spelling_audit.py resolves `{PACK}` by the binding "
            f"the import establishes, and a BARE occurrence cannot be told "
            f"from a local binder that legally shadows it -- "
            f"`let {PACK} = id in {PACK} (show x)` is valid and is not this "
            f"wrapper -- without full Haskell scope analysis. So this file "
            f"cannot be certified as written, which is not a claim that it "
            f"contains one.\n\n"
            f"Import {TEXT_MODULE} qualified, as every import of it in "
            f"src/ and app/ already is, or record this file in EXEMPTIONS "
            f"with the reason it is safe.")

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


# The identifier this wrapper renders through. A local binding of it
# shadows `Prelude`'s, and then `pack (show x)` is not this wrapper.
SHOW = "show"

_LINE_INDENT = re.compile(r"^[ \t\x00]*", re.MULTILINE)
_HORIZONTAL_GAP = re.compile(r"[ \t\x00]*")
_METHOD_BLOCK_HEAD = re.compile(r"(?:instance|class)(?![\w'])")


def _opens_method_block(text: str, pos: int) -> bool:
    """True if the declaration containing `pos` belongs to an
    `instance` or `class` block, whose `show` is the `Show` METHOD
    rather than a local binding shadowing it.

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

    Identifiers are read with `_read_ident`, so a parameter carrying a
    combining mark (`show π́ = …`) does not end the run early and leave
    the binding unrecognised (PR #2404 review round 18). The names are
    returned because a PARAMETER called `show` binds it just as a head
    of that name does: `f show x = … ` shadows the method inside `f`."""
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
    method. That exclusion is what the column matters for, and it is
    made by `_opens_method_block` rather than by requiring column
    zero, which missed every indented local binding."""
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
    """Where a `let` or `where` on the SAME LINE introduces `show`, or
    `None` -- `f x = let show _ = "c" in …`, which starts no line of
    its own."""
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

    No fixture claims an independent failure mode for the closing
    `->`/`→`, and deliberately so: the parameter run only reports
    `binds` when it read the lexeme `show` as a parameter, and a `\\`
    whose run reaches that lexeme in some LATER declaration is reading
    an equation head or parameter that `_show_equation_binding` already
    refuses. The arrow is here because it is what makes the construct a
    lambda -- the same reasoning as `_PACK_LEXEME`'s boundary test."""
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


_SHOW_BINDERS = (("an equation head", _show_equation_binding),
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
            f"{rel_path}:{_line_of(prepared, found)}: this file "
            f"binds `{SHOW}` as {shape}, shadowing the `Show` method.\n\n"
            f"tools/tshow_spelling_audit.py reads a bare `{SHOW}` as that "
            f"method, and `{CANONICAL}` is defined in terms of it, so a "
            f"wrapper written in the shadow's scope is NOT the same "
            f"function -- rewriting it would change behaviour and "
            f"constraints. Telling the two apart needs full scope "
            f"analysis, so the file cannot be certified as written.\n\n"
            f"Rename the local binding, or record this file in EXEMPTIONS "
            f"with the reason it is safe.")


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
    _refuse_shadowed_show(scan_text, rel_path)
    # From the SAME masked text, never the raw source: a column-zero
    # `import qualified Data.Text as T` inside a quasiquote payload is
    # not an import, and resolving against it would bind `T` here and
    # report a `T.pack (show x)` that is really a ByteString packer
    # (PR #2404 review round 6).
    declarations = parse_imports(scan_text)
    # Every qualifier the file establishes. A qualified connector
    # (`P.$`, `P..`) is honoured only under one of these, so an
    # arbitrary `Foo.$` is not read as Prelude's.
    qualifiers = _established_qualifiers(scan_text)

    exempt: set[int] = set()
    if rel_path == UPRELUDE_FILE:
        exempt = {m.start(1) for m in _TSHOW_DEFINITION.finditer(scan_text)}

    violations: list[Violation] = []
    for match in _PACK_LEXEME.finditer(scan_text):
        # A `show` this cannot resolve is reported against the line the
        # wrapper sits on, not against the whole file.
        # A maximal identifier LEXEME, decided by the predicate rather
        # than a `\w` class: `unpack` and `pack'` are different names,
        # and so is a `pack` carrying a combining mark.
        if ((match.start() > 0
             and is_haskell_ident_char(scan_text[match.start() - 1]))
                or (match.end() < len(scan_text)
                    and is_haskell_ident_char(scan_text[match.end()]))):
            continue
        if match.start() in exempt:
            continue
        qualifier, expression_start = _qualifier_before(scan_text,
                                                        match.start())
        if not imports_name(declarations, TEXT_MODULE, PACK, qualifier):
            continue
        spelling = f"{qualifier}.{PACK}" if qualifier else PACK
        # `(. show) T.pack` and `($ show x) T.pack`: the section holds
        # `show` and this expression is its operand, so the wrapper
        # reads right to left.
        if _right_section_before(scan_text, expression_start, qualifiers):
            violations.append(Violation(rel_path,
                                        _line_of(text, match.start()),
                                        spelling, "section"))
            continue
        # `(.) T.pack show` and `($) T.pack (show x)`: the connector is
        # named in prefix form ahead of both operands.
        prefix = _prefix_operator_before(scan_text, expression_start,
                                         qualifiers)
        if prefix is not None:
            form, operator_start = prefix
            # Two kinds of redundant parenthesis close after the
            # operand: those GROUPING the whole application
            # (`((.) pack) show`) and the operand's own
            # (`(.) (pack) show`). The operand's need no grouping test
            # -- the prefix form fixes where it sits.
            budget = (_transparent_open_parens(scan_text, operator_start)
                      + _open_parens_before(scan_text, expression_start))
            cursor = match.end()
            while budget:
                closer = _TRANSPARENT_CLOSE.match(scan_text, cursor)
                if closer is None:
                    break
                cursor, budget = closer.end(), budget - 1
            cursor = _TRANSPARENT_OPEN_RUN.match(scan_text, cursor).end()
            if _names_show(scan_text, cursor, qualifiers):
                violations.append(Violation(rel_path,
                                            _line_of(text, match.start()),
                                            spelling, form))
                continue
        # Otherwise the connector follows -- and `pack` must be the
        # connector's own left operand, not somebody's ARGUMENT.
        # Application binds tighter than any of them, so
        # `format T.pack . show` is `(format T.pack) . show` and
        # `g T.pack (show x)` is two arguments to `g` (PR #2404 review
        # round 16).
        if not _in_function_position(scan_text, expression_start):
            continue
        # Close only as many
        # parentheses as this expression opened, so `(T.pack) . show` is
        # read while `g (h (T.pack)) . show` -- a different function --
        # is not; a section's own closer may sit on either side of the
        # connector, and draws on the same budget.
        budget = _transparent_open_parens(scan_text, expression_start)
        cursor = match.end()
        # An ascription lives inside the parentheses this expression
        # opened, so it is read only when there is one to close -- and
        # reading past it spends that one. Without the budget test,
        # `g (T.pack :: …) . show` would read as a wrapper, when it is
        # `(g (T.pack :: …)) . show`. An ascription that never closes is
        # left alone rather than followed; the ordinary path then finds
        # no connector after `pack` and reports nothing.
        #
        # No fixture claims an independent failure mode for the budget
        # DECREMENT: the closer loop below stops at the first character
        # that is not `)`, so an over-count can only be spent where
        # more consecutive closers stand than this expression opened,
        # which no balanced source has. It is here to keep the
        # accounting honest.
        ascription = _ASCRIPTION.match(scan_text, cursor)
        if ascription is not None and budget:
            closed = _past_ascription(scan_text, ascription.end())
            if closed is not None:
                cursor, budget = closed, budget - 1
        while budget:
            closer = _TRANSPARENT_CLOSE.match(scan_text, cursor)
            if closer is None:
                break
            cursor, budget = closer.end(), budget - 1
        cursor = _GAP.match(scan_text, cursor).end()
        if scan_text[cursor:cursor + 1] == "(":
            form, cursor = "paren", cursor + 1
        else:
            read = _read_connector(scan_text, cursor,
                                   frozenset(qualifiers))
            if read is None:
                continue
            form, cursor = read
        if budget:
            closer = _TRANSPARENT_CLOSE.match(scan_text, cursor)
            if closer is not None:
                cursor = closer.end()
        cursor = _TRANSPARENT_OPEN_RUN.match(scan_text, cursor).end()
        if not _names_show(scan_text, cursor, qualifiers):
            continue
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
        "redundant parentheses around `show` after `$` are transparent "
        "-- `pack $ (show x)` is the same function byte for byte "
        "(PR #2404 review round 5)",
        "module M where\n" + _T
        + "f x = T.pack $ (show x)\n",
        [3],
    ),
    (
        "the same after a composition: `pack . (show)`",
        "module M where\n" + _T
        + "f = T.pack . (show)\n",
        [3],
    ),
    (
        "the same nested inside the application's own parentheses: "
        "`pack ((show x))`",
        "module M where\n" + _T
        + "f x = T.pack ((show x))\n",
        [3],
    ),
    (
        "with whitespace inside the redundant parentheses too",
        "module M where\n" + _T
        + "f x = T.pack (( show x ))\n",
        [3],
    ),
    (
        "`$!` is strict application of the same function to the same "
        "argument, so it renders the same Text",
        "module M where\n" + _T
        + "f x = T.pack $! show x\n",
        [3],
    ),
    (
        "the parenthesised WRAPPER: `(T.pack) . show` closes a "
        "parenthesis this expression itself opened",
        "module M where\n" + _T
        + "f = (T.pack) . show\n",
        [3],
    ),
    (
        "the parentheses may be spaced off the expression they wrap: "
        "`( T.pack ) . show` opens one just the same",
        "module M where\n" + _T
        + "f = ( T.pack ) . show\n",
        [3],
    ),
    (
        "a LEFT operator section applied to `show`: `(pack .) show` is\n        the same function, and GHC infers `Show a => a -> Text` for it "
        "(PR #2404 review round 11)",
        "module M where\n" + _T
        + "f = (T.pack .) show\n",
        [3],
    ),
    (
        "a left section of `$`, applied to the shown value",
        "module M where\n" + _T
        + "g x = (T.pack $) (show x)\n",
        [3],
    ),
    (
        "a QUALIFIED connector: `T.pack P.$ P.show x` compiles to the "
        "same function, and `P..` is the awkward one -- the "
        "qualifier's dot and the operator are both `.` (PR #2404 "
        "review round 13)",
        "module M where\n" + _T
        + "import qualified Prelude as P\n"
        "f x = T.pack P.$ P.show x\n",
        [4],
    ),
    (
        "a UNICODE module alias on the connector: the shared "
        "resolver's alias grammar is ASCII-only, so it establishes no "
        "qualifier for `Ü` and the wrapper went unread (PR #2404 "
        "review round 14)",
        "module M where\n" + _T
        + "import qualified Prelude as Ü\n"
        "f x = T.pack Ü.$ Ü.show x\n",
        [4],
    ),
    (
        "the same alias on the qualified composition",
        "module M where\n" + _T
        + "import qualified Prelude as Ü\n"
        "f = T.pack Ü.. Ü.show\n",
        [4],
    ),
    (
        "and in a right section",
        "module M where\n" + _T
        + "import qualified Prelude as Ü\n"
        "f = (Ü.. Ü.show) T.pack\n",
        [4],
    ),
    (
        "a postpositive `qualified` (ImportQualifiedPost) still "
        "establishes its alias",
        "module M where\n" + _T
        + "import Prelude qualified as Ü\n"
        "f x = T.pack Ü.$ Ü.show x\n",
        [4],
    ),
    (
        "a MULTI-SEGMENT module name qualifies as itself: reading only "
        "its first segment would establish `Data` and leave "
        "`Data.Function.$` unresolved",
        "module M where\n" + _T
        + "import qualified Data.Function\n"
        "f x = T.pack Data.Function.$ show x\n",
        [4],
    ),
    (
        "an alias established WITHOUT `as` -- the module's own name "
        "qualifies it",
        "module M where\n" + _T
        + "import qualified Prelude\n"
        "f x = T.pack Prelude.$ Prelude.show x\n",
        [4],
    ),
    (
        "the qualified composition, `T.pack P.. P.show`",
        "module M where\n" + _T
        + "import qualified Prelude as P\n"
        "f = T.pack P.. P.show\n",
        [4],
    ),
    (
        "qualified in PREFIX form",
        "module M where\n" + _T
        + "import qualified Prelude as P\n"
        "f x = (P.$) T.pack (P.show x)\n",
        [4],
    ),
    (
        "qualified in a RIGHT section",
        "module M where\n" + _T
        + "import qualified Prelude as P\n"
        "f = (P.. P.show) T.pack\n",
        [4],
    ),
    (
        "qualified in a LEFT section",
        "module M where\n" + _T
        + "import qualified Prelude as P\n"
        "f = (T.pack P..) P.show\n",
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
        "a connector named in PREFIX form: `(.) pack show` is "
        "`pack . show` with the operator ahead of both operands, and "
        "GHC infers `Show a => a -> Text` for it (PR #2404 review "
        "round 12)",
        "module M where\n" + _T
        + "f = (.) T.pack show\n",
        [3],
    ),
    (
        "a prefix application GROUPED as a whole: `((.) pack) show` "
        "closes a redundant parenthesis after the operand (PR #2404 "
        "review round 15)",
        "module M where\n" + _T
        + "f = ((.) T.pack) show\n",
        [3],
    ),
    (
        "the OPERAND parenthesised instead: a prefix form fixes where "
        "its operand sits, so those parentheses are redundant with no "
        "grouping test needed",
        "module M where\n" + _T
        + "f = (.) (T.pack) show\n",
        [3],
    ),
    (
        "both at once",
        "module M where\n" + _T
        + "f = ((.) (T.pack)) show\n",
        [3],
    ),
    (
        "a grouped prefix `($)`, applied to the shown value",
        "module M where\n" + _T
        + "g x = (($) T.pack) (show x)\n",
        [3],
    ),
    (
        "the Unicode composition operator in prefix form too",
        "module M where\n" + _T
        + "f = (∘) T.pack show\n",
        [3],
    ),
    (
        "and prefix `($)`, applied to the shown value",
        "module M where\n" + _T
        + "g x = ($) T.pack (show x)\n",
        [3],
    ),
    (
        "a RIGHT section puts `show` BEFORE the packer: `(. show) pack` "
        "reads right to left and needs its own backward look",
        "module M where\n" + _T
        + "f = (. show) T.pack\n",
        [3],
    ),
    (
        "and the `$` right section, whose operand is the packer too",
        "module M where\n" + _T
        + "g x = ($ show x) T.pack\n",
        [3],
    ),
    (
        "a TYPE ASCRIPTION on the packer: the ascription names the "
        "type it already has, and the connector sits after the "
        "group's closing parenthesis rather than after the name "
        "(PR #2404 review round 19)",
        "module M where\n" + _T
        + "f = (T.pack :: String -> T.Text) . show\n",
        [3],
    ),
    (
        "the same written with this tree's own `∷`",
        "module M where\n" + _T
        + "f = (T.pack ∷ String → T.Text) ∘ show\n",
        [3],
    ),
    (
        "an ascribed packer APPLIED to the shown value",
        "module M where\n" + _T
        + "f x = (T.pack :: String -> T.Text) (show x)\n",
        [3],
    ),
    (
        "a type spelling its own parentheses does not close the group "
        "early",
        "module M where\n" + _T
        + "f = (T.pack :: (String) -> T.Text) . show\n",
        [3],
    ),
    (
        "grouping parentheses after `$` are still transparent",
        "module M where\n" + _T
        + "f = id $ (T.pack) . show\n",
        [3],
    ),
    (
        "and inside a list, where nothing is being applied",
        "module M where\n" + _T
        + "fs = [(T.pack) . show]\n",
        [3],
    ),
    (
        "and in a lambda body",
        "module M where\n" + _T
        + "f = \\_ -> (T.pack) . show\n",
        [3],
    ),
    (
        "two of them, closed twice",
        "module M where\n" + _T
        + "f = ((T.pack)) . show\n",
        [3],
    ),
    (
        "a parenthesised `pack` applied directly",
        "module M where\n" + _T
        + "f x = (T.pack) (show x)\n",
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
        "`show x == y` is an expression, not an equation head: the "
        "`=` must be a MAXIMAL symbol lexeme, or the line reads as a "
        "binding and the file is refused",
        "module M where\n" + _T
        + "f x y =\n"
        "  show x == y\n"
        "g x = T.pack (show x)\n",
        [5],
    ),
    (
        "a RESERVED WORD before the wrapper introduces it rather than "
        "applying to it: `let … in T.pack (show x)` is standalone "
        "(PR #2404 review round 18)",
        "module M where\n" + _T
        + "f x = let y = 1 in T.pack (show x)\n",
        [3],
    ),
    (
        "and so do `then` and `of`",
        "module M where\n" + _T
        + "f p x = if p then T.pack (show x) else t\n"
        "g x = case x of _ -> T.pack (show x)\n",
        [3, 4],
    ),
    (
        "a wrapper that is the RIGHT OPERAND of a masked multi-dash "
        "operator is still standalone -- the operator separates two "
        "operands, so reading through it would make this one look "
        "like an argument of the left",
        "module M where\n" + _T
        + "f x = x --> T.pack (show x)\n",
        [3],
    ),
    (
        "`GHC.Show` is where `base` DEFINES the class, so a `show` "
        "qualified by it is the same method (PR #2404 review round 17)",
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
        "`UPrelude` re-exports `module Prelude`, so a `show` qualified "
        "by it is the same method -- which is where every bare `show` "
        "in this tree comes from",
        "module M where\n" + _T
        + "import qualified UPrelude as U\n"
        "f x = T.pack (U.show x)\n",
        [4],
    ),
    (
        "a QUALIFIED `show`: `P.show` is the same function",
        "module M where\n" + _T
        + "import qualified Prelude as P\n"
        "f x = T.pack (P.show x)\n",
        [4],
    ),
    (
        "a UNICODE module alias on `show`: GHC accepts one, and an "
        "ASCII-only qualifier class walks straight past the wrapper "
        "(PR #2404 review round 4)",
        "module M where\n" + _T
        + "import qualified Prelude as Ü\n"
        "f x = T.pack (Ü.show x)\n",
        [4],
    ),
    (
        "Template Haskell's `[t| … |]` is a QUOTATION BRACKET, not a "
        "quasiquote: its body is Haskell, so masking it would hide the "
        "wrapper inside. This tree writes them (World.Material.Id, "
        "Engine.Graphics.Vulkan.Uniform.Layout)",
        "module M where\n" + _T
        + "x = [t| T.pack (show y) |]\n",
        [3],
    ),
    (
        "the same for `[e|`, and for `[d|` / `[p|` alongside it",
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
        "not a quasiquoter -- a quasiquoter name is a varid -- so its "
        "text stays code and the wrapper below it is still reported",
        "module M where\n" + _T
        + "ys = [Nothing|x<-xs]\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a lowercase-qualified `[mod.e|` names no module, so it is "
        "neither a bracket nor a quasiquote and its text stays code",
        "module M where\n" + _T
        + "s = [mod.e| T.pack (show x) |]\n",
        [3],
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
        "an import declaration inside a QUASIQUOTE payload is not an "
        "import: resolving against it would bind `T` to Data.Text and "
        "report a ByteString packer (PR #2404 review round 6)",
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
        "the text after the keyword needs the same blank set stripped "
        "off it as the text before",
        "module M where\n"
        "import qualified Data.Text as T hiding {- why -} (unpack)\n"
        "f x = T.pack (show x)\n",
        [3],
    ),
    (
        "and on one carrying a trailing block comment",
        "module M where\n"
        "import qualified Data.Text as T {- why -}\n"
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
        "a string GAP ends at its own backslash: read as a two-character "
        "escape, the gap's closer pairs with the string's closing quote, "
        "the string never ends and every wrapper below is masked "
        "(PR #2404 review round 7)",
        "module M where\n" + _T
        + 's = "a\\\n\\"\n'
        "f x = T.pack (show x)\n",
        [5],
    ),
    (
        "an ESCAPED quote is not a gap: a backslash before a NON-space "
        "is still an escape, so the string ends at its real closing "
        "quote and the wrapper below it is reported",
        "module M where\n" + _T
        + 's = "she said \\"hi\\""\n'
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "an escaped BACKSLASH ending a string is not a gap either",
        "module M where\n" + _T
        + 's = "path\\\\"\n'
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "the shape this tree actually writes 258 times -- a message "
        "split across lines by a gap",
        "module M where\n" + _T
        + 's = "first \\\n'
        '      \\ second"\n'
        "f x = T.pack (show x)\n",
        [5],
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
        "a UNICODE identifier's trailing prime is part of the name: "
        "read as a char-literal opener it eats the opening quote of the "
        "`'\"'` after it, and the real closing quote then opens a "
        "phantom string masking everything below (PR #2404 review "
        "round 8)",
        "module M where\n" + _T
        + "g = let π' _ = () in π'\"'\"\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a COMBINING MARK is an identifier character GHC accepts (issue "
        "#7650) and Python's `\\w` does not match: read without it, the "
        "prime of `π́'` opens a char literal and the string after it "
        "masks the wrapper (PR #2404 review round 9)",
        "module M where\n" + _T
        + "g = let π́' x = x in π́'\"'\"\n"
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a TITLECASE module alias: GHC's conid head is uppercase OR "
        "titlecase (`Lt`), and `str.isupper()` is false for `ǅ` "
        "(PR #2404 review round 10)",
        "module M where\n" + _T
        + "import qualified Prelude as ǅ\n"
        "f x = T.pack (ǅ.show x)\n",
        [4],
    ),
    (
        "a module alias carrying a mark still qualifies `show`",
        "module M where\n" + _T
        + "import qualified Prelude as Ṕ́\n"
        "f x = T.pack (Ṕ́.show x)\n",
        [4],
    ),
    (
        "a quasiquoter name carrying a mark is still a quasiquoter -- "
        "an opener read as ordinary code leaves its payload readable, "
        "and a lone quote in there masks everything below",
        "module M where\n" + _T
        + 's = [téxt| " |]\n'
        "f x = T.pack (show x)\n",
        [4],
    ),
    (
        "a prime may itself follow a prime: `x''` is one name, and "
        "dropping `'` from the continuation set makes its second prime "
        "open a literal that eats the string after it",
        "module M where\n" + _T
        + "g = let x'' _ = () in x''\"'\"\n"
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
        "a gapped string BEFORE a quasiquote: if the pre-pass runs past "
        "the string's real closing quote it never sees the opener, "
        "leaves the payload unmasked, and reports it as code",
        "module M where\n" + _T
        + 's = "a\\\n\\"\n'
        "q = [text| T.pack (show x) |]\n",
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
        "and its payload is still a payload: a wrapper written inside a "
        "marked quasiquoter's body is not code",
        "module M where\n" + _T
        + 's = [téxt| T.pack (show x) |]\n',
    ),
    (
        "a quasiquoter with a UNICODE name is still a quasiquote -- one "
        "this did not recognize would leave a payload readable as "
        "Haskell",
        "module M where\n" + _T
        + "s = [tëxt| T.pack (show x) |]\n",
    ),
    (
        "a TITLECASE quasiquoter qualifier names a module too, so the "
        "payload is still a payload",
        "module M where\n" + _T
        + 's = [ǅ.e| T.pack (show x) |]\n',
    ),
    (
        "a varid may begin with an `Lo` letter -- GHC maps OtherLetter "
        "to `small`, and `str.islower()` is false for one",
        "module M where\n" + _T
        + 's = [אq| T.pack (show x) |]\n',
    ),
    (
        "`format (T.pack) . show` is `(format T.pack) . show`: "
        "juxtaposition is application, so those parentheses supply an "
        "ARGUMENT and do not wrap the expression -- `format` may "
        "transform the converter, so this is not the same function "
        "(PR #2404 review round 10)",
        "module M where\n" + _T
        + "f = format (T.pack) . show\n",
    ),
    (
        "a left section that is somebody's ARGUMENT is not standalone: "
        "`format (T.pack .) show` applies `format` to both",
        "module M where\n" + _T
        + "f = format (T.pack .) show\n",
    ),
    (
        "an ascribed packer that is somebody's ARGUMENT: "
        "`g (T.pack :: …) . show` is `(g (T.pack :: …)) . show`, so "
        "the ascription is read only when there is a parenthesis of "
        "this expression's own to close",
        "module M where\n" + _T
        + "f = g (T.pack :: String -> T.Text) . show\n",
    ),
    (
        "an ascribed packer composed with something that is not "
        "`show` is a different function",
        "module M where\n" + _T
        + "f = (T.pack :: String -> T.Text) . g\n",
    ),
    (
        "`format T.pack . show` is `(format T.pack) . show`: "
        "application binds tighter than any connector, so the packer "
        "is `format`'s ARGUMENT and not the composition's left "
        "operand (PR #2404 review round 16)",
        "module M where\n" + _T
        + "f = format T.pack . show\n",
    ),
    (
        "the same unparenthesised application before the paren form: "
        "`g T.pack (show x)` is two arguments to `g`",
        "module M where\n" + _T
        + "f x = g T.pack (show x)\n",
    ),
    (
        "a QUASIQUOTE is a value, so something written after it is "
        "applying to it: its closing bracket stays visible, or "
        "`[fn| g |] (T.pack) . show` would read as a standalone "
        "wrapper",
        "module M where\n" + _T
        + "s = [fn| g |] (T.pack) . show\n",
    ),
    (
        "a section in ARGUMENT position is not applied to what follows "
        "it: application is left-associative, so `f (. show) T.pack` "
        "hands `f` two independent arguments (PR #2404 review round 12)",
        "module M where\n" + _T
        + "x = f (. show) T.pack\n",
    ),
    (
        "the same with a BARE `show` after it, so the connector's own "
        "qualifier is what decides",
        "module M where\n" + _T
        + "f x = T.pack Q.$ show x\n",
    ),
    (
        "a qualifier the file does NOT establish is not read as a "
        "connector: `Q.$` under no such import is somebody else's "
        "operator",
        "module M where\n" + _T
        + "f x = T.pack Q.$ Q.show x\n",
    ),
    (
        "a prefix connector applied to something that is not `show` "
        "is a different function: `(.) T.pack g` is `T.pack . g`",
        "module M where\n" + _T
        + "f = (.) T.pack g\n",
    ),
    (
        "an applicand ending in a closing bracket puts the section in "
        "argument position too: `(g y) (. show) T.pack` applies `g y`",
        "module M where\n" + _T
        + "x = (g y) (. show) T.pack\n",
    ),
    (
        "a GROUPED prefix application in argument position is not "
        "applied to what follows it either: `g ((.) T.pack) show` "
        "hands `g` two arguments",
        "module M where\n" + _T
        + "f = g ((.) T.pack) show\n",
    ),
    (
        "and a prefix operator in argument position is not applying "
        "anything either -- `g (.) T.pack show` is three arguments to "
        "`g`",
        "module M where\n" + _T
        + "f = g (.) T.pack show\n",
    ),
    (
        "a right section naming a DIFFERENT function",
        "module M where\n" + _T
        + "f = (. showFFloat) T.pack\n",
    ),
    (
        "`(g . show)` is a composition, not a section -- its content "
        "does not begin with the connector, so applying it to `pack` "
        "is `g (show pack)` and not this wrapper",
        "module M where\n" + _T
        + "f = (g . show) T.pack\n",
    ),
    (
        "the backward look reads only a section it can see WHOLE: it "
        "gives up at the `<>` rather than scanning on to some earlier "
        "`(`, so `(. show <> q)` -- whose right operand is not `show` "
        "-- is not read as one",
        "module M where\n" + _T
        + "f = (. show <> q) T.pack\n",
    ),
    (
        "a section must be CLOSED before its operand: in "
        "`(. show . T.pack)` the packer is inside the parentheses, so "
        "nothing is applying the section to it",
        "module M where\n" + _T
        + "f = (. show . T.pack)\n",
    ),
    (
        "a right section whose operand is a ByteString packer",
        "module M where\n"
        "import qualified Data.ByteString.Char8 as BC\n"
        + "f = (. show) BC.pack\n",
    ),
    (
        "a BACKTICK application binds the parentheses after it the "
        "same way",
        "module M where\n" + _T
        + "f = x `fmt` (T.pack) . show\n",
    ),
    (
        "and so does an applicand that itself ends in a closing "
        "parenthesis",
        "module M where\n" + _T
        + "f = (g x) (T.pack) . show\n",
    ),
    (
        "`[Mod.e| … |]` names a quasiquoter called `e`, which is a "
        "quasiquote and not Template Haskell's bracket -- the exclusion "
        "is for the UNQUALIFIED spellings only",
        "module M where\n" + _T
        + "s = [Mod.e| T.pack (show x) |]\n",
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
        "parentheses that belong to somebody ELSE are not transparent: "
        "in `g (h (T.pack)) . show` they were opened by `g` and `h`, so "
        "only one of the two closers after `pack` is this expression's "
        "and the second is not the connector",
        "module M where\n" + _T
        + "f = g (h (T.pack)) . show\n",
    ),
    (
        "a parenthesis with something INSIDE it before `show` is not "
        "redundant -- `pack (f (show x))` is a different function",
        "module M where\n" + _T
        + "f x = T.pack (f (show x))\n",
    ),
    (
        "`pack` fmapped over something is not applied to it",
        "module M where\n" + _T
        + "f = T.pack <$> Just show\n",
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
        "a trailing COMMENT on the import is not trailing text the "
        "resolver failed to read -- masked bytes are whitespace here, "
        "and `str.strip()` does not remove a NUL (PR #2404 review "
        "round 6)",
        "module M where\n"
        "import qualified Data.Text as T -- rationale\n"
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
        "a `show` qualified by a module this cannot read: `Custom.show` "
        "may be an unrelated formatter -- whose wrapper is NOT `tshow` "
        "-- or another re-export of the method -- whose wrapper is. "
        "Guessing either way is a defect, so it refuses (PR #2404 "
        "review rounds 16 and 17)",
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
        "a `let`-bound `show`: valid, not this wrapper, and rewriting "
        "it would change behaviour and constraints (PR #2404 review "
        "round 13)",
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
        "a TOP-LEVEL equation head, which an indented instance method "
        "is not",
        "module M where\n" + _T
        + 'show _ = "c"\n'
        "g x = T.pack (show x)\n",
        _ShadowedShowError,
    ),
    (
        "a lambda parameter carrying a COMBINING MARK beside it: a "
        "binder identifier read with a narrower class ends early and "
        "leaves the binding unrecognised (PR #2404 review round 18)",
        "module M where\n" + _T
        + "f = \\\\π́ show -> T.pack (show 1)\n",
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
        "an equation head whose PARAMETER carries one",
        "module M where\n" + _T
        + 'show π́ = "c"\n'
        "g x = T.pack (show x)\n",
        _ShadowedShowError,
    ),
    (
        "a `let` block whose `show` binding is not the FIRST one: an "
        "equation head is recognised at any column, so an indented "
        "local binding counts like a top-level one",
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
        "a `do` binder",
        "module M where\n" + _T
        + "f = do\n"
        "  show <- act\n"
        "  pure (T.pack (show 1))\n",
        _ShadowedShowError,
    ),
    (
        "an import putting `pack` in UNQUALIFIED scope: its uses are "
        "bare names, and `let pack = id in pack (show x)` is valid, "
        "returns String, and is not this wrapper (PR #2404 review "
        "round 6)",
        "module M where\n"
        "import Data.Text (Text, pack)\n"
        "f x = let pack = id in pack (show x)\n",
        _UnqualifiedImportError,
    ),
    (
        "the same refusal without a shadowing binder in sight -- the "
        "file is refused for what its imports ALLOW, since finding "
        "every binder is the scope analysis this does not do",
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


def _fixture_shape_failures() -> list[str]:
    """Every fixture whose SOURCE lost its line breaks.

    Such a fixture is VACUOUS, not failing: its import stops being an
    import declaration and the file is refused (or scanned wrongly) for
    a reason that has nothing to do with the rule under test. This suite
    made that mistake, so it is checked rather than watched for."""
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

    failures.extend(_fixture_shape_failures())

    # And the invariant itself is probed, the way EXEMPTIONS is: a
    # checker nothing can fail is a comment.
    for probe, complaint in ((("probe", "module M where\\nf = 1\\n", []),
                              "LITERAL backslash-n"),
                             (("probe", "module M where f = 1", []),
                              "single line")):
        DETECTED_FIXTURES.append(probe)
        try:
            broken = [line for line in _fixture_shape_failures()
                      if complaint in line]
        finally:
            DETECTED_FIXTURES.pop()
        if not broken:
            failures.append(f"  FIXTURE SHAPE: a fixture source that is "
                            f"{complaint} was not reported")

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
