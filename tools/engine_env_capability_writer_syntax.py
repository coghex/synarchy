#!/usr/bin/env python3
"""Haskell lexical, import and mutation-expression analysis for the SS5
writing-module scanner (issue #1892, capability mutation-authority epic
#1890 -- CMA-1; extracted from
tools/engine_env_capability_writers.py by issue #2230).

Everything here reads Haskell SOURCE and nothing here knows what an
`EngineEnv` field is: the tokenizer (`tokenize_haskell`), the import
resolver (`parse_imports`, `imports_name`, `resolve_primitive`), and
the mutation-expression reader that decides whether a primitive
occurrence is an APPLIED mutation, a bare or local value, the
primitive passed as a value, or an expression the scan cannot read at
all (`classify_mutation_site` and the head, argument and operand
helpers under it). That four-way split is what makes the recognized
form list a CLOSED set rather than an aspiration, so it is a contract,
not an implementation detail.

It reads the recognized primitives' defining modules from
`engine_env_capability_writer_authority` and source-preparation
helpers from `engine_env_capability_common`; it calls no projection or
scan function, and imports nothing from the facade. Dependencies run
one way: authority, then syntax and projections over it, then scan
over all three.
"""
from __future__ import annotations

import re
from typing import NamedTuple

import engine_env_capability_writer_authority as authority  # type: ignore
from engine_env_capability_common import (  # type: ignore
    _CHAR_LITERAL_RE, _import_chunks, _strip_haskell_comments,
)


_HS_IDENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_']*")
_IMPORT_WILDCARD_RE = re.compile(
    r"([A-Z][A-Za-z0-9_']*)\s*\(\s*\.\.\s*\)")
# An equation left-hand side (top-level, `where` or `let` alike),
# anchored at the start of its line and forbidding `{` in the parameter
# text, so a record construction or update -- `env { fooRef = r }`, or a
# continuation line starting with `,` or `{` -- is never mistaken for
# one. Group 1 is the bound name, group 2 its parameter text.
# An import's `qualified` keyword, module name, and optional `as`
# alias, in either the classic or the `ImportQualifiedPost` order. All
# three matter, and each is a language rule rather than a preference:
# `qualified` removes the UNQUALIFIED spelling from scope entirely, and
# an `as` alias REPLACES the module name as the qualifier instead of
# joining it.
# The four binding forms, each anchored at the start of ONE line and
# each paired below with the layout region it actually scopes over.
# `_BINDING_LHS_RE` forbids `{` in the parameter text so a record
# construction or update (`env { fooRef = r }`) is never read as an
# equation; a continuation line opening with `,` or `{` matches
# nothing at all.
_IMPORT_HIDING_RE = re.compile(r"(?<![A-Za-z0-9_'])hiding(?![A-Za-z0-9_'])")
_IMPORT_DECL_RE = re.compile(
    r"^import\s+(?P<pre>qualified\s+)?(?P<module>[A-Z][A-Za-z0-9_.']*)"
    r"(?P<post>\s+qualified\b)?"
    r"(?:\s+as\s+(?P<alias>[A-Z][A-Za-z0-9_.']*))?")


class Token(NamedTuple):
    """One identifier or single-character punctuation token.

    `offset` is the character position, which is what lets ADJACENCY be
    tested: `env.fieldOne` and `env . fieldOne` tokenize identically and
    mean entirely different things, and only the gap between them says
    which."""
    kind: str   # "id" | "punc"
    text: str
    line: int   # 1-based
    offset: int


class ImportDecl(NamedTuple):
    """One `import` declaration, reduced to what name resolution needs.

    `qualifier` is the prefix a QUALIFIED use must carry -- the `as`
    alias when there is one, otherwise the module's own name.
    `qualified` says whether the UNQUALIFIED spelling is in scope at
    all: `import qualified M as N` puts only `N.f` in scope, never `f`,
    which is why the two flags cannot be collapsed into one map.
    `names` is `None` only for an import that enumerates nothing at all
    -- a bare import, or one carrying a `hiding` clause, whose excluded
    names are in `hidden`. Otherwise `names` holds the plainly listed
    symbols and `wildcards` the TYPES imported as `T(..)`: that form
    brings in `T`'s own selectors and nobody else's, so
    `Engine.Core.State (WindowState(..))` does not put an `EngineEnv`
    field in scope."""
    module: str
    qualified: bool
    qualifier: str
    names: frozenset[str] | None
    hidden: frozenset[str]
    wildcards: frozenset[str]


def tokenize_haskell(text: str) -> list[Token]:
    """Identifier / single-character-punctuation tokens over
    ALREADY-comment-stripped Haskell source, with string and character
    literals consumed whole and dropped.

    Whitespace -- newlines included -- is skipped, which is what makes
    every consumer below scan complete EXPRESSIONS rather than
    individual lines: a mutation whose accessor argument sits on the
    next line (`Engine.Input.Thread.Dispatch`'s `atomicModifyIORef'` of
    `rvFramebufferMinimizeGenRef`, `Engine.Scripting.Lua.API.StructureArt`'s
    `rhStructureArtCatalogRef` write) is one token sequence here.
    Numeric literals degrade into punctuation tokens, which is harmless:
    nothing downstream matches on them."""
    tokens: list[Token] = []
    i = 0
    line = 1
    n = len(text)
    while i < n:
        ch = text[i]
        if ch == "\n":
            line += 1
            i += 1
            continue
        if ch.isspace():
            i += 1
            continue
        ident = _HS_IDENT_RE.match(text, i)
        if ident:
            # Haskell lexes `Mod.name` with NO intervening space as one
            # QUALIFIED name whenever the prefix is a conid, so
            # `State.fieldOne` must not degrade into `State`, `.`,
            # `fieldOne` -- that is how a qualified write would slip the
            # scan. Composition (`f . g`, and `f.g` on a lowercase head)
            # keeps its own tokens.
            end = ident.end()
            component = ident.group(0)
            while component[:1].isupper() and end < n and text[end] == ".":
                nxt = _HS_IDENT_RE.match(text, end + 1)
                if nxt is None:
                    break
                component = nxt.group(0)
                end = nxt.end()
            tokens.append(Token("id", text[i:end], line, i))
            i = end
            continue
        if ch == '"':
            j = i + 1
            while j < n and text[j] != '"':
                if text[j] == "\\":
                    # The ESCAPED character may itself be a newline: a
                    # Haskell string gap is a backslash, whitespace
                    # (newlines included) and another backslash. Missing
                    # it reports every later token a line too early,
                    # and a residue entry or a blocking site names the
                    # wrong source line.
                    j += 1
                    if j < n and text[j] == "\n":
                        line += 1
                elif text[j] == "\n":
                    line += 1
                j += 1
            i = min(j + 1, n)
            continue
        if ch == "'":
            # An identifier's trailing prime is already consumed above,
            # so a `'` reaching here opens a character literal (or is
            # stray punctuation).
            literal = _CHAR_LITERAL_RE.match(text, i)
            if literal:
                i = literal.end()
                continue
        tokens.append(Token("punc", ch, line, i))
        i += 1
    return tokens


def strip_import_declarations(text: str) -> str:
    """`text` with every top-level `import` declaration blanked (line
    count preserved). An import list names accessors -- often the very
    accessor a module writes -- and naming one is not using one."""
    for chunk in _import_chunks(text):
        text = text.replace(chunk, "\n" * chunk.count("\n"), 1)
    return text


def prepared_source(text: str) -> str:
    """Comment-stripped, import-blanked source: what the scan reads.
    Haddock and `--` commentary can name any accessor without counting
    as a use."""
    return strip_import_declarations(_strip_haskell_comments(text))


def parse_imports(source_text: str) -> list[ImportDecl]:
    """Every `import` declaration in `source_text`, as `ImportDecl`s.

    A LIST, not a map keyed by module: one module is legitimately
    imported twice with different terms (`import Data.Map (Map)` beside
    `import qualified Data.Map as M`), and each declaration carries its
    own answer about which spellings it admits.

    This is what decides whether an identifier at a write site can even
    BE the accessor -- in both directions. `src/Unit/Thread/Movement.hs`
    writes a local `utsRef` parameter while importing
    `Engine.Core.State` for the `EngineEnv` TYPE alone, so the identical
    name there is not the field; and under
    `import qualified Engine.Core.State as State` only `State.fieldOne`
    is the field, while a bare `fieldOne` is necessarily something the
    module defined itself. A `hiding` clause is recorded rather than
    waved through, for the same reason: a module that hides `fieldOne`
    and defines its own is not writing the field."""
    declarations: list[ImportDecl] = []
    for chunk in _import_chunks(_strip_haskell_comments(source_text)):
        head = _IMPORT_DECL_RE.match(chunk)
        if not head:
            continue
        module = head.group("module")
        alias = head.group("alias")
        qualified = bool(head.group("pre") or head.group("post"))
        body = chunk[head.end():]
        hiding = _IMPORT_HIDING_RE.search(body)
        hidden: frozenset[str] = frozenset()
        wildcards = frozenset(match.group(1)
                              for match in _IMPORT_WILDCARD_RE.finditer(body))
        if hiding is not None:
            # Everything EXCEPT the listed names. A `hiding (T(..))`
            # names the type, not its fields, so a field hidden that way
            # is not recorded -- which can only leave a write attributed
            # (a loud violation), never hide one.
            hidden = frozenset(_HS_IDENT_RE.findall(body[hiding.end():]))
            names: frozenset[str] | None = None
        elif "(" not in body:
            names = None
        else:
            # A `T(..)` group is recorded as a wildcard on `T`, never as
            # a plain name, so it can only grant `T`'s own selectors.
            names = frozenset(_HS_IDENT_RE.findall(
                _IMPORT_WILDCARD_RE.sub(" ", body)))
        declarations.append(ImportDecl(module, qualified, alias or module,
                                       names, hidden, wildcards))
    return declarations


def imports_name(declarations: list[ImportDecl], module: str, name: str,
                 qualifier: str, owner_type: str | None = None) -> bool:
    """True iff `declarations` put `module`'s `name` in scope under the
    spelling used -- `qualifier` empty for a bare use, otherwise the
    prefix that was written. A qualified use must match a declaration's
    own qualifier; an unqualified one must find a declaration that is
    not `qualified` at all.

    `owner_type` is the record `name` is a selector of. A `T(..)` group
    grants only `T`'s selectors, so an import list carrying some OTHER
    type's wildcard does not put this one in scope. `None` means the
    owner is unknown, in which case any wildcard is accepted -- the
    direction that keeps a write visible."""
    for declaration in declarations:
        if declaration.module != module:
            continue
        if qualifier:
            if declaration.qualifier != qualifier:
                continue
        elif declaration.qualified:
            continue
        if name in declaration.hidden:
            continue
        if declaration.names is None or name in declaration.names:
            return True
        if declaration.wildcards and (owner_type is None
                                      or owner_type in declaration.wildcards):
            return True
    return False


def resolve_primitive(declarations: list[ImportDecl], name: str) -> str | None:
    """The handle-consuming primitive `name` denotes here, or `None`.

    Bare or qualified, the base name must be one of the recognized
    primitives AND must reach this module from that primitive's OWN
    defining module (`ACCESS_PRIMITIVE_MODULES`) under that exact
    spelling. A module-local `writeIORef`, or `Other.writeIORef` from
    an unrelated module, is a different function; attributing its
    argument would invent a write out of code that mutates no `IORef`
    at all. `Engine.Core.ReadOnlyRef`'s read goes through the identical
    rule, not a looser second path.

    __A TOP-LEVEL homonym is covered by the same rule, because Haskell
    makes it so.__ Defining `writeIORef` beside an unqualified
    `import Data.IORef` is an ambiguous occurrence at every use site --
    that module does not compile -- so the only spellings that reach
    here are the ones this test already decides: the import names the
    primitive, or it does not (`hiding (writeIORef)`, an explicit list
    without it, `qualified`), and a local definition then stands alone.

    A LOCAL binding -- a `let`, a `where`, a lambda parameter -- can
    legally shadow the imported primitive, and that is the mirror of an
    accessor shadowed the same way. Both are `SHADOW_EXEMPTIONS`'
    business, by requirement 7's deliberate choice: the exemption
    suppresses the module/field pair whatever name was shadowed to
    produce it, and no scope analysis is performed for either."""
    qualifier, _, base = name.rpartition(".")
    owner = authority.ACCESS_PRIMITIVE_MODULES.get(base)
    if owner is None:
        return None
    if not imports_name(declarations, owner, base, qualifier):
        return None
    return base


def _applied_head(tokens: list[Token], head: int) -> int | None:
    """`head` if the accessor at that index is APPLIED to something,
    else `None`.

    Parentheses around the accessor ITSELF change nothing --
    `writeIORef ((fieldOne) env) 1` applies exactly what
    `writeIORef (fieldOne env) 1` does -- so the closers balancing the
    openers written directly before it are stepped over before the next
    token is judged. Exactly that many are consumed and no more, so a
    genuinely unapplied `(fieldOne)` still ends at its own closer
    instead of reading whatever follows the group it sits in."""
    peeled = 0
    k = head - 1
    while k >= 0 and tokens[k].kind == "punc" and tokens[k].text == "(":
        peeled += 1
        k -= 1
    j = head + 1
    while (peeled > 0 and j < len(tokens) and tokens[j].kind == "punc"
           and tokens[j].text == ")"):
        peeled -= 1
        j += 1
    if j >= len(tokens):
        return None
    following = tokens[j]
    applied = (following.kind == "id"
               or (following.kind == "punc"
                   and following.text in ("(", "[", "$")))
    return head if applied else None


def _skip_type_atom(tokens: list[Token], index: int) -> int:
    """Index just past the type atom at `index` -- one identifier, or
    one balanced `(`/`[` group. Anything else is left where it is, so a
    shape this does not understand stops the walk instead of consuming
    the value argument."""
    if index >= len(tokens):
        return index
    token = tokens[index]
    if token.kind == "id":
        return index + 1
    if token.kind == "punc" and token.text in ("(", "["):
        depth = 0
        while index < len(tokens):
            current = tokens[index]
            if current.kind == "punc" and current.text in ("(", "["):
                depth += 1
            elif current.kind == "punc" and current.text in (")", "]"):
                depth -= 1
                if depth == 0:
                    return index + 1
            index += 1
    return index


# Keywords lex as identifiers but apply to nothing: `else
# atomicModifyIORef' (...) ...` is a head-position use, and
# `src/Unit/Thread/Movement/Climb.hs:86` is exactly that.
_HASKELL_KEYWORDS = frozenset({
    "case", "do", "else", "if", "in", "let", "of", "then", "where",
})


def after_operator_section(tokens: list[Token], index: int) -> bool:
    """True if `tokens[index]` is directly preceded by an OPERATOR
    SECTION -- a parenthesized group holding nothing but punctuation,
    as in `($) writeIORef (fieldOne env) value` or `(.) f g`.

    Applying an operator prefix that way is ordinary Haskell, and what
    the section does with its arguments is exactly what a textual scan
    cannot know: `($)` applies them, `(.)` composes them, and the two
    have opposite consequences for whether a write happens here. So the
    site is neither attributed nor waved through -- it is
    unclassifiable, and requirement 6 reports it. Recognizing each
    operator individually is the open-ended path this arc rejects."""
    if index == 0:
        return False
    closing = tokens[index - 1]
    if closing.kind != "punc" or closing.text != ")":
        return False
    depth, j = 0, index - 1
    while j >= 0:
        token = tokens[j]
        if token.kind == "punc" and token.text == ")":
            depth += 1
        elif token.kind == "punc" and token.text == "(":
            depth -= 1
            if depth == 0:
                break
        j -= 1
    if j < 0:
        return False
    # An empty group vacuously qualifies, which is harmless: `()` can
    # never be applying a primitive in code that compiles.
    return all(tokens[k].kind == "punc" for k in range(j + 1, index - 1))


def line_indents(code: str) -> list[int | None]:
    """Indent column per 1-BASED line (index 0 unused), `None` for a
    blank line. `in_head_position` reads it to tell a continuation from
    a new statement."""
    return [None] + [None if not line.strip()
                     else len(line) - len(line.lstrip())
                     for line in code.split("\n")]


def in_head_position(tokens: list[Token], index: int,
                     indents: list[int | None] | None = None) -> bool:
    """True unless something is plainly APPLYING to `tokens[index]`.

    `withLogging writeIORef (fieldOne env) 1` hands the primitive to
    `withLogging`; reading the tokens after it as its own arguments
    invents a write, and hides the accessor's pass-on residue entry
    behind a phantom inline use. What can apply to it is an identifier
    or a closing bracket -- but a newline does not end an application,
    and layout does not end a statement with any token, so the token
    alone cannot decide it:

    * a KEYWORD applies to nothing, wherever it sits, which is what
      makes `else writeIORef (...) ...` and the `do` opening a block
      both head position;
    * on the SAME line, an identifier or closing bracket is applying;
    * across lines, LAYOUT decides. A continuation is indented past the
      line that opened the expression (`withLogging` on one line, the
      primitive indented under it), while a sibling statement starts at
      the same column or further left.

    Without `indents` the across-lines case answers True, which keeps a
    write visible rather than dropping it silently."""
    if index == 0:
        return True
    previous = tokens[index - 1]
    if previous.kind == "id":
        if previous.text in _HASKELL_KEYWORDS:
            return True
    elif not (previous.kind == "punc" and previous.text in (")", "]")):
        return True
    if previous.line == tokens[index].line:
        return False
    if indents is None:
        return True
    # A token's own line is never blank, so `or 0` is a totality guard
    # rather than a branch worth its own case.
    here = indents[tokens[index].line] or 0
    there = indents[previous.line] or 0
    return here <= there


def _past_primitive_parentheses(tokens: list[Token], index: int) -> int:
    """Index of the first token after `tokens[index]` that is not a
    `)` closing a `(` written immediately before the primitive.

    `(writeIORef) (accessor handle) v` is the same application as the
    unparenthesized form -- parentheses around a function name change
    nothing -- so the closers have to be stepped over before the value
    argument can be found.

    Two conditions keep that from inventing an application. Only closers
    balanced by openers DIRECTLY preceding the primitive are consumed,
    so `foo (writeIORef ref v)` is untouched; and the outermost of those
    openers must itself sit in head position -- nothing applying to it
    on its left -- because in `withLogging (writeIORef) (accessor
    handle) v` the primitive is an ARGUMENT being passed on, not the
    function being applied, and what that callee does with it is exactly
    the indirection D-5 reports rather than attributes."""
    openers = 0
    k = index - 1
    while k >= 0 and tokens[k].kind == "punc" and tokens[k].text == "(":
        openers += 1
        k -= 1
    if openers and k >= 0 and (tokens[k].kind == "id"
                               or tokens[k].text in (")", "]")):
        openers = 0
    j = index + 1
    while j < len(tokens) and tokens[j].kind == "punc":
        if tokens[j].text == "@":
            # `(writeIORef @Int) (accessor handle) v` -- the type
            # application sits INSIDE the parentheses, so it has to be
            # stepped over before the closer can be.
            j = _skip_type_atom(tokens, j + 1)
            continue
        if openers > 0 and tokens[j].text == ")":
            openers -= 1
            j += 1
            continue
        break
    return j


def _infix_left_operand_head(tokens: list[Token], index: int) -> int | None:
    """Token index of the head identifier of a BACKTICKED primitive's
    left operand -- ``(accessor handle) `writeIORef` value`` -- or
    `None`.

    Haskell lets any two-argument function be written infix, so this is
    the same direct write as the prefix form with the arguments swapped,
    and a scan that only looked to the RIGHT of the primitive would miss
    it silently. `tokens[index]` is the primitive itself, so its
    backticks sit at `index - 1` and `index + 1`. The operand must be a
    application, for exactly the reason `_first_argument_head` requires
    one. It need not be PARENTHESIZED, since a backtick operator binds
    looser than application: ``fieldOne env `writeIORef` 1`` is the
    same write. `_operand_head` finds the head either way, so a
    trailing `)` that closes an ARGUMENT
    (``fkFieldOne (toFakeCapability env) `writeIORef` v``) is not
    mistaken for one closing the whole operand."""
    if (index == 0 or tokens[index - 1].kind != "punc"
            or tokens[index - 1].text != "`"):
        return None
    if (index + 1 >= len(tokens)
            or tokens[index + 1].kind != "punc"
            or tokens[index + 1].text != "`"):
        return None
    head = _operand_head(tokens, index - 2)
    if head is None or head >= index - 1 or tokens[head].kind != "id":
        return None
    return _applied_head(tokens, head)


def _operand_head(tokens: list[Token], last: int) -> int | None:
    """Index of the head identifier of the application ENDING at
    `tokens[last]`, or `None`.

    Walks left over ATOMS only -- an identifier, or a balanced `(`/`[`
    group -- so an operator, a `$`, a comma or an equals ends the
    operand where it stands. `(fieldOne env)`, `fieldOne env` and
    `fkFieldOne (toFakeCapability env)` therefore all resolve to their
    own head, which is the point: whether the operand is parenthesized
    says nothing about where its head is, and a trailing `)` may be
    closing an ARGUMENT rather than the whole operand.

    When the walk ends having consumed nothing but one group, that group
    IS the operand, so its head lies inside it and the search descends
    (peeling any redundant nesting on the way)."""
    head: int | None = None
    group_open: int | None = None
    j = last
    while j >= 0:
        token = tokens[j]
        if token.kind == "id":
            head, group_open = j, None
            j -= 1
            continue
        if token.kind == "punc" and token.text in (")", "]"):
            depth = 0
            k = j
            while k >= 0:
                current = tokens[k]
                if current.kind == "punc" and current.text in (")", "]"):
                    depth += 1
                elif current.kind == "punc" and current.text in ("(", "["):
                    depth -= 1
                    if depth == 0:
                        break
                k -= 1
            if k < 0:
                break
            head, group_open = None, k
            j = k - 1
            continue
        break
    if head is not None:
        return head
    if group_open is None:
        return None
    inner = group_open + 1
    while (inner < len(tokens) and tokens[inner].kind == "punc"
           and tokens[inner].text == "("):
        inner += 1
    return inner if inner < len(tokens) else None


def _opens_record_dot(tokens: list[Token], index: int) -> bool:
    """True if `tokens[index]` is an identifier IMMEDIATELY followed by
    `.` and another identifier.

    Only a lowercase head can reach this: `tokenize_haskell` already
    merges `Mod.name` into one qualified token, so an uppercase head is
    never left with a separate `.` beside it.

    Written without spaces that is `OverloadedRecordDot` field access
    (`env.fieldOne`); written with them it is composition. The scan can
    read neither as an accessor application, so rather than take the
    left operand as the argument head -- which quietly makes
    `modifyIORef' (env.fieldOne) id` a non-write -- the site is left
    unclassifiable and requirement 6 reports it. No such site exists in
    this tree: the extension is not enabled anywhere in it."""
    if tokens[index].kind != "id":
        return False
    dot, name = index + 1, index + 2
    if name >= len(tokens):
        return False
    return (tokens[dot].kind == "punc" and tokens[dot].text == "."
            and tokens[name].kind == "id"
            and tokens[dot].offset == tokens[index].offset
            + len(tokens[index].text)
            and tokens[name].offset == tokens[dot].offset + 1)


def first_argument_token(tokens: list[Token], index: int
                         ) -> tuple[int | None, bool]:
    """`(index of the first argument's head identifier, was a grouping
    token consumed)` for the mutation primitive at `tokens[index]`.

    The head is returned whether or not it is APPLIED, because naming it
    is what `classify_mutation_site` needs and being applied is what
    `_first_argument_head` needs. `grouped` says whether anything that
    OPENS an argument -- a `(`, a `$`, a `$!`, a visible type
    application -- was stepped over on the way, which is what separates
    "an argument is being formed here and I cannot read it" from "this
    primitive is not applied to anything here"."""
    j = _past_primitive_parentheses(tokens, index)
    grouped = j != index + 1
    while j < len(tokens) and tokens[j].kind == "punc":
        token = tokens[j]
        if token.text == "@":
            # A visible type application (`writeIORef @Int (ref) v`,
            # legal under GHC2024's default `TypeApplications`) is not
            # the value argument. Skip its type atom -- an identifier,
            # or one balanced group -- and keep looking.
            j = _skip_type_atom(tokens, j + 1)
            grouped = True
            continue
        if token.text in ("$", "("):
            grouped = True
            j += 1
            # `$!` is the strict sibling of `$` and groups identically;
            # the tokenizer splits it, so its `!` is stepped over here.
            if (token.text == "$" and j < len(tokens)
                    and tokens[j].kind == "punc" and tokens[j].text == "!"):
                j += 1
            continue
        break
    if j < len(tokens) and tokens[j].kind == "id":
        if _opens_record_dot(tokens, j):
            return None, True
        return j, grouped
    return None, grouped


def _first_argument_head(tokens: list[Token], index: int) -> int | None:
    """Token index of the head identifier of `tokens[index]`'s first
    argument, when that argument is an APPLICATION -- `prim (accessor
    handle) ...` or `prim $ accessor handle`. Otherwise `None`.

    __Requiring the application is a type argument, not a heuristic.__
    Every accessor here projects out of a handle -- `EngineEnv -> IORef
    a`, or `XCapability -> IORef a` -- so it cannot itself BE the
    `IORef` a mutation primitive takes. A BARE identifier in that
    position therefore never denotes the accessor; it denotes some
    local binding that happens to share its name, exactly like
    `src/Unit/Thread/Movement.hs`'s `utsRef` parameter. That is decided
    by SHAPE, without modelling Haskell's binding forms at all -- see
    `SHADOW_EXEMPTIONS` for the one residual case and why it is a
    checked-in list rather than a scope analysis."""
    head, grouped = first_argument_token(tokens, index)
    if head is None or not grouped:
        return None
    return _applied_head(tokens, head)


class MutationSite(NamedTuple):
    """One mutation-primitive occurrence and what the scan made of it.

    `kind` is exactly one of:

    * `"write"` -- an APPLIED, in-scope, non-exempt accessor: attributed
      to `field`.
    * `"other"` -- a nameable head that is not this boundary's business:
      a local `IORef`, an unapplied accessor, an accessor the module
      cannot reach, an exempted shadow, or the primitive used as a
      VALUE rather than applied to anything here.
    * `"unclassifiable"` -- an argument is plainly being formed and the
      scan cannot name its head. This BLOCKS (requirement 6): it is how
      a spelling outside the recognized set fails loudly instead of
      silently dropping a write.
    """
    relpath: str
    line: int
    module: str
    kind: str
    field: str | None


def classify_mutation_site(tokens: list[Token], index: int
                           ) -> tuple[str, int | None]:
    """`(kind, head token index)` for the mutation primitive at
    `tokens[index]`, before scope is consulted -- `"applied"`,
    `"bare"`, `"value"` or `"unclassifiable"`.

    Every occurrence lands in exactly one of the four, which is what
    makes the recognized-form list a closed set rather than an
    aspiration."""
    head, grouped = first_argument_token(tokens, index)
    if head is not None:
        applied = grouped and _applied_head(tokens, head) is not None
        return ("applied" if applied else "bare"), head
    if (index > 0 and tokens[index - 1].kind == "punc"
            and tokens[index - 1].text == "`"):
        operand = _operand_head(tokens, index - 2)
        if operand is None:
            return "unclassifiable", None
        applied = _infix_left_operand_head(tokens, index) is not None
        return ("applied" if applied else "bare"), operand
    return ("unclassifiable" if grouped else "value"), None
