#!/usr/bin/env python3
r"""Registered-verb guard for the Haskell/Lua boundary (#1996).

The Haskell/Lua boundary is a runtime ABI that nothing used to check.
`src/Engine/Scripting/Lua/API/Register/*.hs` installs a fixed set of
global namespace tables and attaches a fixed set of verbs to each; a
Lua script naming a verb the engine does not register fails at the
moment a player reaches that line, not at build time.

That is not hypothetical. `scripts/ui/bar.lua` called
`UI.setSpriteColor`, which no registration provides -- the registered
verb is `UI.setColor`. It survived from `9af2585c` until #1914 fixed it
in `7f46468e`, purely because `bar.setFillColor` had no callers yet.

This audit extracts the registered namespace->verb map from the
registrar modules, extracts namespaced member references from every
`scripts/**/*.lua`, and reports the one condition that is a defect: a
reference names a verb its namespace does not provide.

WHAT IT DOES NOT DO
-------------------
It does not flag registered-but-uncalled verbs (reverse coverage cannot
be trusted without enumerating the hspec suites, the ~85 probes, and the
debug console, all of which call verbs `scripts/` never names), and it
checks neither argument counts nor return shapes. Those belong to a
later slice with a descriptor contract behind it.

THE TWO OUTCOMES ARE DISTINCT
-----------------------------
A *finding* is the semantic defect above. A *certification failure* is
this analyzer declining to certify an input: an unreadable file, a lex
error, an empty corpus on either side, a registration it cannot
associate with a namespace, a registrar construct outside the accepted
grammar, or a Lua construct its scope resolver cannot classify. Both
exit nonzero, and neither is ever reported as the other. Nothing is ever
skipped in silence -- every failure names the file and the line.

THE CANDIDATE GRAMMAR
---------------------
A checked candidate is a *direct dot-member reference rooted at an
extracted engine namespace, while that root is not lexically shadowed*:

    <namespace> . <member>

with the `<namespace>` token not itself preceded by `.` or `:`. That
definition, not "a call", is what is checked -- so it covers first-class
function references passed as values (`engine.loadMaterialYaml` in
`scripts/startup_loader.lua`), and every Lua call argument form
including the table-constructor calls `debug.recordOutcome{...}` and
`structure.registerPackArt{...}` and string-literal calls, none of which
a parenthesis-only recognizer would see.

Only the FIRST hop is checked. In `item.foo.bar`, `item.foo` must be a
registered member for the expression to work at all, so it is checked;
`.bar` is a member of whatever that returned and is outside this slice.
This is also what makes the naive scanner's worst false-positive class
disappear: in `unitAi.till.execute` the root is `unitAi`, which no
registrar installs, and the `till.execute` tail is never a candidate
because its `till` is preceded by `.`.

Outside the grammar entirely, and therefore silently uninteresting:
longer paths and computed indexing rooted at unrelated Lua modules, and
calls through aliases. But a construct that *begins* as a direct,
unshadowed known-namespace reference and then leaves the grammar --
`engine:method`, `engine[expr]` -- is an attributed failure, not a skip.
A dynamic access at the engine boundary names a verb this analyzer
cannot see, which is exactly the thing it must not certify.

WHY IT IS A LEXER AND NOT A REGEX
---------------------------------
This repository has paid for the naive version of this tool three times:
PR #704 took 12 review rounds on fixed-width regex windows bridging
adjacent calls, PR #1128 took 4 and ended only by replacing per-line
matching with a real lexer plus a fail-loud catch-all, and PR #1309 took
14 on a hand-rolled format parser inside a CI gate. A naive
`\b<namespace>\.<name>` scan over `scripts/` finds 4,905 occurrences
across 560 distinct names, 85 of which do not resolve -- and every one
of those 85 is a false positive, in five classes all present in the tree
today. Comments and string literals (classes 1 and 3), dotted module
references in prose (class 2), and tail matches on `unitAi` sub-tables
(class 5) all fall to lexing; locals shadowing a registered global
(class 4) falls to scope resolution. `tools/lua_duplicate_function_audit.py`
is the shape that worked, and #1324's two lessons on it are requirements
here: an unrecognized in-scope file is an attributed failure, and an
empty corpus is a failure.

Usage:
  python3 tools/lua_registration_audit.py [--json]
Exit codes: 0 = every registrar and script certified with no findings,
1 = a finding, 2 = a certification failure.
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

REGISTRAR_GLOB = "src/Engine/Scripting/Lua/API/Register/*.hs"
SCRIPTS_GLOB = "scripts/**/*.lua"

EXIT_OK = 0
EXIT_FINDINGS = 1
EXIT_CERTIFICATION = 2


class CertificationError(Exception):
    """An input this analyzer declines to certify, attributed to a line.

    Never a finding: a finding is a defect in the code under audit, this
    is the analyzer saying it cannot vouch for what it read. Both are
    fatal, and the two are reported separately so that a gate failure is
    never ambiguous about which one occurred.
    """

    def __init__(self, path: str, line: int, message: str) -> None:
        super().__init__(f"{path}:{line}: {message}")
        self.path = path
        self.line = line
        self.message = message


# ---------------------------------------------------------------------------
# Shared token type
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Token:
    kind: str      # 'name' | 'number' | 'string' | 'op'
    text: str      # the spelling for names/ops; the decoded value for strings
    line: int      # 1-based
    col: int       # 0-based, used only to find Haskell top-level declarations


# ---------------------------------------------------------------------------
# Haskell lexer -- enough of one to read the registrar modules
# ---------------------------------------------------------------------------

# Haskell's `'` is both the char-literal delimiter and a legal identifier
# character (`x'`, `env'`). A run of apostrophes only opens a literal when
# it spells exactly one character or one escape, which is what
# tools/check_texture_paths.py settled on for the same corpus.
HS_CHAR_LIT = re.compile(r"'(?:\\.[0-9a-zA-Z]*|[^'\\])'")
HS_SYMBOL = frozenset("!#$%&*+./<=>?@\\^|-~:")
HS_NAME = re.compile(r"[A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*")
HS_NUMBER = re.compile(r"[0-9][0-9_]*(?:\.[0-9][0-9_]*)?")


def _hs_opens_line_comment(line: str, i: int) -> bool:
    """True when the `--` at `i` opens a comment rather than an operator.

    `-->` and `<--` are operators: a dash run is a comment opener only
    when it is maximal and not adjacent to another symbol character.
    """
    if not line.startswith("--", i):
        return False
    if i > 0 and line[i - 1] in HS_SYMBOL:
        return False
    j = i
    while j < len(line) and line[j] == "-":
        j += 1
    return j >= len(line) or line[j] not in HS_SYMBOL


def lex_haskell(path: str, text: str) -> list[Token]:
    """Tokenize Haskell, dropping comments and keeping string values.

    Deliberately partial: it resolves comments, string literals, char
    literals, names and the punctuation the registrar grammar reads, and
    emits everything else as single-character `op` tokens. That is all
    the grammar below consults, and an unreadable literal is a
    certification failure rather than a guess.
    """
    lines = text.split("\n")
    tokens: list[Token] = []
    state = "code"
    nest = 0            # {- -} nesting depth
    opened = 0          # 1-based line where the open construct started

    for ln, line in enumerate(lines, start=1):
        i = 0
        length = len(line)
        while i < length:
            if state == "block":
                if line.startswith("{-", i):
                    nest += 1
                    i += 2
                elif line.startswith("-}", i):
                    nest -= 1
                    i += 2
                    if nest == 0:
                        state = "code"
                else:
                    i += 1
                continue

            char = line[i]
            if char in " \t\r":
                i += 1
            elif line.startswith("{-", i):
                state, nest, opened = "block", 1, ln
                i += 2
            elif _hs_opens_line_comment(line, i):
                i = length
            elif char == '"':
                value, end = _hs_string(path, line, i, ln)
                tokens.append(Token("string", value, ln, i))
                i = end
            elif char == "'" and HS_CHAR_LIT.match(line, i):
                i = HS_CHAR_LIT.match(line, i).end()
            elif char.isalpha() or char == "_":
                m = HS_NAME.match(line, i)
                tokens.append(Token("name", m.group(0), ln, i))
                i = m.end()
            elif char.isdigit():
                m = HS_NUMBER.match(line, i)
                tokens.append(Token("number", m.group(0), ln, i))
                i = m.end()
            else:
                tokens.append(Token("op", char, ln, i))
                i += 1

    if state == "block":
        raise CertificationError(path, opened, "unterminated {- block comment")
    return _drop_header_declarations(tokens)


# `import Engine.Scripting.Lua.API.Internal (registerLuaFunction)` names the
# same identifier the grammar below reads as a registration, and the module
# export list names the registrar entry points. Neither is executable, so a
# top-level declaration opened by `module` or `import` is dropped whole --
# from its column-zero keyword to the next column-zero token, which is where
# the next top-level declaration begins.
HS_HEADER_KEYWORDS = frozenset({"module", "import"})


def _drop_header_declarations(tokens: list[Token]) -> list[Token]:
    kept: list[Token] = []
    skipping = False
    for token in tokens:
        if token.col == 0:
            skipping = token.kind == "name" and token.text in HS_HEADER_KEYWORDS
        if not skipping:
            kept.append(token)
    return kept


def _hs_string(path: str, line: str, start: int, ln: int) -> tuple[str, int]:
    """Read one Haskell string literal, returning (value, end index).

    Registrar verb names are plain ASCII identifiers, so the only escape
    handling needed is enough to find the closing quote correctly.
    Multi-line string gaps do not occur in these modules; one would be an
    unterminated literal here, which is a certification failure and not a
    silent truncation.
    """
    out: list[str] = []
    i = start + 1
    while i < len(line):
        char = line[i]
        if char == "\\":
            if i + 1 >= len(line):
                break
            out.append(line[i + 1])
            i += 2
        elif char == '"':
            return "".join(out), i + 1
        else:
            out.append(char)
            i += 1
    raise CertificationError(path, ln, "unterminated string literal")


# ---------------------------------------------------------------------------
# Registrar extraction
# ---------------------------------------------------------------------------

# Lua 5.4's stock `debug` table, which `Lua.openlibs` installs before any
# registrar runs. Register/Debug.hs deliberately ADDS to that table
# rather than replacing it, so these members are genuinely provisioned
# even though no registerLuaFunction names them.
#
# Keyed by namespace because the augmenting form is what selects it: a
# namespace opened from an existing global is augmenting a stdlib table,
# and one whose stdlib members this map does not know cannot be
# certified. The `lua` package this project builds against bundles Lua
# 5.4, whose debug library is the list below.
STOCK_MEMBERS: dict[str, frozenset[str]] = {
    "debug": frozenset({
        "debug", "gethook", "getinfo", "getlocal", "getmetatable",
        "getregistry", "getupvalue", "getuservalue", "sethook", "setcstacklimit",
        "setlocal", "setmetatable", "setupvalue", "setuservalue", "traceback",
        "upvalueid", "upvaluejoin",
    }),
}

# Every HsLua construct that can attach a member to the table under
# construction or install a global. `registerLuaFunction` and the two
# `Lua.Name` globals are the accepted grammar; the rest are recognized
# only so that introducing one fails loudly instead of yielding a
# quietly smaller map.
UNSUPPORTED_INSTALL = frozenset({
    "Lua.setfield", "Lua.rawset", "Lua.rawseti", "Lua.settable",
    "Lua.register", "Lua.pushcfunction", "Lua.pushHaskellFunction",
    "Lua.setglobal'", "Lua.rawsetp",
})

VERB_NAME_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")


@dataclass
class RegistrarResult:
    """The registered surface, plus what it took to certify it."""

    namespaces: dict[str, set[str]] = field(default_factory=dict)
    augmenting: set[str] = field(default_factory=set)
    registrations: int = 0
    files: int = 0

    def provided(self, namespace: str) -> frozenset[str]:
        """Every member a call site may legitimately name on `namespace`."""
        members = set(self.namespaces[namespace])
        if namespace in self.augmenting:
            members |= STOCK_MEMBERS[namespace]
        return frozenset(members)


def _expect_lua_name(path: str, tokens: list[Token], i: int, construct: str) -> tuple[str, int]:
    """Read the `(Lua.Name "x")` argument at `i`, or fail loudly.

    The accepted spelling is exact. A namespace installed under any other
    shape -- a variable, a computed name, a different constructor -- is an
    unsupported registration block rather than something to guess at.
    """
    line = tokens[i - 1].line
    shape = [("op", "("), ("name", "Lua.Name"), ("string", None), ("op", ")")]
    if i + len(shape) > len(tokens):
        raise CertificationError(path, line, f"{construct} is truncated")
    for offset, (kind, text) in enumerate(shape):
        token = tokens[i + offset]
        if token.kind != kind or (text is not None and token.text != text):
            raise CertificationError(
                path, token.line,
                f'{construct} does not spell its argument as (Lua.Name "<name>"); '
                f"found {token.text!r}")
    name = tokens[i + 2].text
    if not VERB_NAME_RE.match(name):
        raise CertificationError(path, line, f"{construct} names an unusable namespace {name!r}")
    return name, i + len(shape)


def extract_registrations(path: str, text: str) -> tuple[dict[str, set[str]], set[str], int]:
    """Read one registrar module's namespace->verb map.

    The accepted grammar is a sequence of blocks. A block OPENS with
    either `Lua.newtable` (a fresh table) or
    `Lua.getglobal (Lua.Name "ns")` (augmenting a stdlib table already
    installed by openlibs), attaches one or more
    `registerLuaFunction "<verb>"` members, and CLOSES with
    `Lua.setglobal (Lua.Name "ns")`, which is what names it.

    Register/Debug.hs takes the augmenting form and guards both halves
    behind the same `isTbl` test, so the newtable and the setglobal sit
    on mutually exclusive runtime branches. Statically both branches
    publish the same verb set on the same namespace, which is the only
    property this map records.

    Everything outside that grammar fails with a file and a line: a
    registration attached to no open block, a block that reaches end of
    file without being installed, a namespace named by anything but a
    string literal, and any other HsLua construct that could attach a
    member.
    """
    tokens = lex_haskell(path, text)
    namespaces: dict[str, set[str]] = {}
    augmenting: set[str] = set()
    registrations = 0

    open_kind: str | None = None      # None | 'new' | 'global'
    pending: list[tuple[str, int]] = []
    # The namespace a `Lua.getglobal` opened since the last install, if any.
    # A later `Lua.newtable` does not clear it: Register/Debug.hs guards the
    # two on mutually exclusive branches of the same `isTbl` test, so the
    # block augments the stdlib table on one path and recreates it on the
    # other while publishing one verb set on one namespace either way.
    augment_of: str | None = None
    augment_line = 0

    i = 0
    while i < len(tokens):
        token = tokens[i]
        if token.kind != "name":
            i += 1
            continue

        if token.text in UNSUPPORTED_INSTALL:
            raise CertificationError(
                path, token.line,
                f"unsupported registration construct {token.text}: this analyzer "
                "cannot certify which namespace it attaches to")

        if token.text == "Lua.newtable":
            open_kind = "new"
            i += 1
        elif token.text == "Lua.getglobal":
            line = token.line
            name, i = _expect_lua_name(path, tokens, i + 1, "Lua.getglobal")
            open_kind, augment_of, augment_line = "global", name, line
        elif token.text == "registerLuaFunction":
            if i + 1 >= len(tokens) or tokens[i + 1].kind != "string":
                found = tokens[i + 1].text if i + 1 < len(tokens) else "end of file"
                raise CertificationError(
                    path, token.line,
                    f"registerLuaFunction is not followed by a literal verb name; found {found!r}")
            verb = tokens[i + 1].text
            if not VERB_NAME_RE.match(verb):
                raise CertificationError(
                    path, token.line, f"registerLuaFunction names an unusable verb {verb!r}")
            if open_kind is None:
                raise CertificationError(
                    path, token.line,
                    f"registerLuaFunction {verb!r} is attached to no open table block")
            pending.append((verb, token.line))
            registrations += 1
            i += 2
        elif token.text == "Lua.setglobal":
            name, i = _expect_lua_name(path, tokens, i + 1, "Lua.setglobal")
            if open_kind is None:
                raise CertificationError(
                    path, token.line, f"namespace {name!r} is installed with no table block open")
            if not pending:
                raise CertificationError(
                    path, token.line, f"namespace {name!r} is installed with no registrations")
            if augment_of is not None:
                if augment_of != name:
                    raise CertificationError(
                        path, token.line,
                        f"namespace {name!r} is installed from a block opened on "
                        f"{augment_of!r}")
                if name not in STOCK_MEMBERS:
                    raise CertificationError(
                        path, augment_line,
                        f"namespace {name!r} augments an existing global whose stock "
                        "members this analyzer does not know")
                augmenting.add(name)
            namespaces.setdefault(name, set()).update(verb for verb, _ in pending)
            pending = []
            open_kind, augment_of = None, None
        else:
            i += 1

    if pending:
        verb, line = pending[0]
        raise CertificationError(
            path, line,
            f"registerLuaFunction {verb!r} reaches end of file with no "
            "Lua.setglobal naming its namespace")
    return namespaces, augmenting, registrations


def collect_registrations(root: Path) -> RegistrarResult:
    """Certify every registrar module and merge their maps."""
    paths = sorted(root.glob(REGISTRAR_GLOB))
    if not paths:
        raise CertificationError(
            REGISTRAR_GLOB, 0,
            "no registrar modules matched: the registered surface cannot be "
            "extracted, so this gate would certify nothing")

    result = RegistrarResult()
    for path in paths:
        rel = str(path.relative_to(root))
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError) as error:
            raise CertificationError(rel, 0, f"unreadable registrar module: {error}") from error
        namespaces, augmenting, count = extract_registrations(rel, text)
        if not namespaces:
            raise CertificationError(
                rel, 0,
                "registrar module installs no namespace: it is either an "
                "unrecognized shape or it no longer belongs to this glob")
        for name, verbs in namespaces.items():
            result.namespaces.setdefault(name, set()).update(verbs)
        result.augmenting |= augmenting
        result.registrations += count
        result.files += 1
    return result


# ---------------------------------------------------------------------------
# Lua lexer
# ---------------------------------------------------------------------------

LUA_KEYWORDS = frozenset({
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function",
    "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return",
    "then", "true", "until", "while",
})

LUA_LONG_OPEN = re.compile(r"\[(=*)\[")
LUA_NAME = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
LUA_NUMBER = re.compile(r"0[xX][0-9a-fA-F.]+(?:[pP][+-]?[0-9]+)?"
                        r"|[0-9.]+(?:[eE][+-]?[0-9]+)?")
LUA_LONG_OPS = ("...", "..", "==", "~=", "<=", ">=", "//", "<<", ">>", "::")


def lex_lua(path: str, text: str) -> list[Token]:
    r"""Tokenize Lua, dropping comments and string CONTENT.

    String tokens are emitted (an expression needs them to stay
    well-formed for the statement-boundary rule, and `f"literal"` is a
    call) but carry no text, because a `scripts/` path or a doc comment
    that happens to spell `engine.foo` is not a call site -- classes 1
    and 3 of the five false-positive classes fall out here.

    Every unterminated literal is a certification failure attributed to
    the line that opened it. Nothing is guessed and nothing is skipped.
    """
    lines = text.split("\n")
    tokens: list[Token] = []
    state = "code"
    level = 0
    quote = ""
    opened = 0

    for ln, line in enumerate(lines, start=1):
        i = 0
        length = len(line)
        continued = False
        while i < length:
            char = line[i]
            if state == "code":
                if line.startswith("--", i):
                    match = LUA_LONG_OPEN.match(line, i + 2)
                    if match:
                        state, level, opened = "longcomment", len(match.group(1)), ln
                        i = match.end()
                    else:
                        i = length
                elif char == "[" and LUA_LONG_OPEN.match(line, i):
                    match = LUA_LONG_OPEN.match(line, i)
                    state, level, opened = "longstring", len(match.group(1)), ln
                    tokens.append(Token("string", "", ln, i))
                    i = match.end()
                elif char in "\"'":
                    state, quote, opened = "string", char, ln
                    tokens.append(Token("string", "", ln, i))
                    i += 1
                elif char.isalpha() or char == "_":
                    match = LUA_NAME.match(line, i)
                    word = match.group(0)
                    kind = "kw" if word in LUA_KEYWORDS else "name"
                    tokens.append(Token(kind, word, ln, i))
                    i = match.end()
                elif char.isdigit() or (char == "." and i + 1 < length and line[i + 1].isdigit()):
                    match = LUA_NUMBER.match(line, i)
                    tokens.append(Token("number", match.group(0), ln, i))
                    i = match.end()
                elif char in " \t\r":
                    i += 1
                else:
                    for op in LUA_LONG_OPS:
                        if line.startswith(op, i):
                            tokens.append(Token("op", op, ln, i))
                            i += len(op)
                            break
                    else:
                        tokens.append(Token("op", char, ln, i))
                        i += 1
            elif state == "string":
                if char == "\\":
                    if i + 1 >= length:
                        continued = True   # an escaped newline continues the literal
                        i = length
                    else:
                        i += 2
                elif char == quote:
                    state = "code"
                    i += 1
                else:
                    i += 1
            else:  # longstring / longcomment
                closer = "]" + "=" * level + "]"
                at = line.find(closer, i)
                if at < 0:
                    i = length
                else:
                    state = "code"
                    i = at + len(closer)

        if state == "string" and not continued:
            raise CertificationError(path, opened, "unterminated string literal")

    if state == "string":
        raise CertificationError(path, opened, "unterminated string literal")
    if state == "longstring":
        raise CertificationError(path, opened, "unterminated long string")
    if state == "longcomment":
        raise CertificationError(path, opened, "unterminated long comment")
    return tokens


# ---------------------------------------------------------------------------
# Lua scope resolution and candidate extraction
# ---------------------------------------------------------------------------

# Tokens after which an expression is COMPLETE, so a following name or
# `function` begins a new statement rather than continuing this one.
# This is the one place Lua's grammar is genuinely ambiguous (`a = b`
# followed by `(f)(x)` is a call continuation, which is why the manual
# recommends a semicolon), and `(` is deliberately absent below so that
# this analyzer resolves it the way Lua itself does.
COMPLETING_OPS = frozenset({")", "]", "}", "..."})
COMPLETING_KEYWORDS = frozenset({"end", "true", "false", "nil"})

# Keywords that always begin a new statement or close the current block.
BOUNDARY_KEYWORDS = frozenset({
    "local", "if", "while", "for", "repeat", "until", "do", "end",
    "return", "break", "elseif", "else", "then", "goto",
})

BLOCK_CLOSED_BY_END = frozenset({"function", "do", "while", "for", "if"})

BRACKET_PAIRS = {"(": ")", "[": "]", "{": "}"}


@dataclass
class Block:
    kind: str
    names: set[str] = field(default_factory=set)


@dataclass
class PendingLocal:
    """`local a, b = <rhs>` names, which are not visible until the rhs ends.

    Lua binds a `local` statement's names only after its expression list,
    which is what makes `local blood = unit.getBlood(uid)` a reference to
    the outer `blood` on the right of the `=` and the new local
    everywhere after it. Binding them at the keyword instead would put a
    shadow over the very expression that reads through it.

    These NEST: an expression list can contain a function body or a table
    constructor holding `local` statements of its own, as in
    `local engine = { f = function() local seen = true; return seen end }`.
    They are therefore held on a stack, innermost first. A single slot
    would let the inner declaration displace the outer one, which would
    then never bind at all -- and every later reference through that name
    would be resolved against the global it was shadowing.
    """

    names: list[str]
    block: Block
    block_depth: int
    bracket: int


@dataclass
class LoopHeader:
    """A `for`/`while` header waiting for the `do` that opens its body.

    These NEST, and the `do` that belongs to one is the one at the SAME
    block and bracket depth the header started at. A `do` opened deeper
    -- inside a function or a parenthesised expression in the header
    itself -- is an ordinary block, and consuming the header there would
    scan the real loop body with its control variables unbound.
    """

    kind: str
    names: list[str]
    block_depth: int
    bracket: int


@dataclass
class Candidate:
    namespace: str
    member: str
    line: int


def _completes_expression(token: Token | None) -> bool:
    if token is None:
        return False
    if token.kind in ("name", "number", "string"):
        return True
    if token.kind == "op":
        return token.text in COMPLETING_OPS
    return token.text in COMPLETING_KEYWORDS


def _starts_statement(token: Token, previous: Token | None) -> bool:
    if token.kind == "kw":
        if token.text in BOUNDARY_KEYWORDS:
            return True
        # `local f = function() end` continues an expression; `x = 1`
        # followed by `function g() end` does not.
        return token.text == "function" and _completes_expression(previous)
    if token.kind == "op":
        return token.text in (";", "::")
    if token.kind == "name":
        return _completes_expression(previous)
    return False


class LuaScopeReader:
    """Walks one Lua chunk, tracking which names are lexically bound.

    Only one question is asked of the result: at this token, is this
    engine-namespace name shadowed by a local, a parameter, or a loop
    variable? Everything the class models -- ordinary and multiple
    `local` declarations, `local function`, function parameters and the
    implicit `self` of a method definition, numeric and generic `for`
    variables, nested `do`/`if`/`while`/`for`/`repeat` blocks,
    declaration order, and restoration of the global on scope exit --
    exists to answer it.

    Every construct it cannot classify is a certification failure naming
    the file and the line, because the alternative is a scan that
    silently stops resolving shadows and reports the same clean run.
    """

    def __init__(self, path: str, tokens: list[Token], namespaces: frozenset[str]) -> None:
        self.path = path
        self.tokens = tokens
        self.namespaces = namespaces
        self.blocks: list[Block] = [Block("chunk")]
        self.brackets: list[Token] = []
        self.pending: list[PendingLocal] = []
        self.loop_headers: list[LoopHeader] = []
        self.repeat_closing: list[tuple[Block, int]] = []
        self.candidates: list[Candidate] = []
        self.i = 0

    # -- helpers ---------------------------------------------------------

    def fail(self, token: Token, message: str) -> None:
        raise CertificationError(self.path, token.line, message)

    def peek(self, offset: int = 0) -> Token | None:
        index = self.i + offset
        return self.tokens[index] if 0 <= index < len(self.tokens) else None

    @property
    def bracket(self) -> int:
        return len(self.brackets)

    def bound(self, name: str) -> bool:
        return any(name in block.names for block in self.blocks)

    def read_name_list(self, token: Token, what: str,
                       *, attributes: bool = False) -> list[str]:
        """Read `a, b, c` at the cursor, leaving it on the following token.

        With `attributes`, each name may carry its own Lua 5.4 attribute:
        the grammar is `Name attrib {',' Name attrib}`, so an attribute
        binds to the name BEFORE it and the list continues past it.
        Reading the whole list first and then draining attributes stops
        at the first `<` and loses every name after it -- and a lost name
        is a lost shadow, which surfaces as a false missing-verb finding
        on a perfectly ordinary local.
        """
        names: list[str] = []
        while True:
            current = self.peek()
            if current is None:
                self.fail(token, f"{what} is truncated at end of file")
            if current.kind != "name":
                self.fail(token, f"{what} does not name a variable; "
                                 f"found {current.text!r}")
            names.append(current.text)
            self.i += 1
            marker = self.peek()
            if attributes and marker is not None and marker.kind == "op" and marker.text == "<":
                attribute, closer = self.peek(1), self.peek(2)
                if (attribute is None or attribute.kind != "name"
                        or closer is None or closer.kind != "op" or closer.text != ">"):
                    self.fail(token, f"{what} carries an unreadable attribute")
                self.i += 3
            following = self.peek()
            if following is not None and following.kind == "op" and following.text == ",":
                self.i += 1
                continue
            return names

    # -- statement boundaries -------------------------------------------

    def commit_top_pending(self) -> None:
        """Bind the innermost deferred `local` into the frame that owns it.

        The membership test is defensive rather than load-bearing: every
        construct that pops a frame (`end`, `until`) is also a boundary
        keyword, so the drain below always reaches a pending declaration
        before its own frame can be popped out from under it. It is kept
        so that adding a block closer that is NOT a boundary keyword
        degrades to a dropped binding rather than a write into a
        detached frame.
        """
        pending = self.pending.pop()
        if pending.block in self.blocks:
            pending.block.names.update(pending.names)

    def commit_ready_pending(self, token: Token, previous: Token | None) -> None:
        """Bind every deferred `local` whose expression list has ended.

        Innermost first, and more than one can become ready at the same
        token, so this drains rather than committing a single entry.
        """
        while (self.pending
               and len(self.blocks) == self.pending[-1].block_depth
               and self.bracket == self.pending[-1].bracket
               and self.at_boundary(token, previous)):
            self.commit_top_pending()

    def commit_all_pending(self) -> None:
        while self.pending:
            self.commit_top_pending()

    def at_boundary(self, token: Token, previous: Token | None) -> bool:
        return _starts_statement(token, previous)

    def close_ready_repeats(self, token: Token, previous: Token | None) -> None:
        """Retire every `repeat` frame whose `until` expression is over.

        Gated on block depth: an anonymous function inside the `until`
        expression closes with an `end`, which is a boundary keyword, and
        acting on that one would drop the loop's own locals while the
        expression that reads them is still being scanned.

        These NEST too -- an `until` expression may contain a whole
        `repeat` loop of its own -- so pending closures are a stack,
        innermost first. A single slot let an inner loop displace the
        outer one, which was then never retired at all and surfaced at
        end of file as a spurious unclosed block.
        """
        while (self.repeat_closing
               and len(self.blocks) == self.repeat_closing[-1][1]
               and self.at_boundary(token, previous)):
            self.close_top_repeat()

    def close_top_repeat(self) -> None:
        block, _ = self.repeat_closing.pop()
        if block in self.blocks:
            self.blocks.remove(block)

    # -- the walk --------------------------------------------------------

    def run(self) -> list[Candidate]:
        previous: Token | None = None
        while self.i < len(self.tokens):
            token = self.tokens[self.i]

            self.commit_ready_pending(token, previous)
            self.close_ready_repeats(token, previous)

            start = self.i
            self.step(token, previous)
            if self.i == start:                     # pragma: no cover - defensive
                raise AssertionError(f"{self.path}:{token.line}: scope reader stalled")
            previous = token

        self.commit_all_pending()
        while self.repeat_closing:
            self.close_top_repeat()
        if self.loop_headers:
            header = self.loop_headers[-1]
            raise CertificationError(
                self.path, self.tokens[-1].line if self.tokens else 0,
                f"a `{header.kind}` header is never followed by the `do` that "
                "opens its body")
        if self.brackets:
            opener = self.brackets[0]
            raise CertificationError(
                self.path, opener.line,
                f"file ends with unclosed {opener.text!r}: the expression it opens "
                "was never classified")
        if len(self.blocks) != 1:
            open_kinds = ", ".join(block.kind for block in self.blocks[1:])
            raise CertificationError(
                self.path, self.tokens[-1].line if self.tokens else 0,
                f"file ends with unclosed block(s): {open_kinds}")
        return self.candidates

    def step(self, token: Token, previous: Token | None) -> None:
        if token.kind == "op":
            self.step_op(token)
        elif token.kind == "kw":
            self.step_keyword(token)
        elif token.kind == "name":
            self.step_name(token, previous)
        else:
            self.i += 1

    def step_op(self, token: Token) -> None:
        if token.text in "([{":
            self.brackets.append(token)
        elif token.text in ")]}":
            if not self.brackets:
                self.fail(token, f"unbalanced {token.text!r}: it closes nothing")
            opener = self.brackets.pop()
            if BRACKET_PAIRS[opener.text] != token.text:
                self.fail(token, f"{token.text!r} closes a {opener.text!r} opened on "
                                 f"line {opener.line}")
        self.i += 1

    def step_keyword(self, token: Token) -> None:
        word = token.text
        if word == "local":
            self.i += 1
            self.step_local(token)
        elif word == "function":
            self.i += 1
            self.step_function_header(token, named=True)
        elif word == "for":
            self.i += 1
            self.step_for(token)
        elif word == "while":
            self.loop_headers.append(
                LoopHeader("while", [], len(self.blocks), self.bracket))
            self.i += 1
        elif word == "do":
            header = self.loop_headers[-1] if self.loop_headers else None
            if (header is not None and header.block_depth == len(self.blocks)
                    and header.bracket == self.bracket):
                self.loop_headers.pop()
                self.blocks.append(Block(header.kind, set(header.names)))
            else:
                self.blocks.append(Block("do"))
            self.i += 1
        elif word == "then":
            self.blocks.append(Block("if"))
            self.i += 1
        elif word in ("elseif", "else"):
            self.pop_block(token, expected={"if"}, closer=word)
            if word == "else":
                self.blocks.append(Block("if"))
            self.i += 1
        elif word == "repeat":
            self.blocks.append(Block("repeat"))
            self.i += 1
        elif word == "until":
            if not self.blocks or self.blocks[-1].kind != "repeat":
                self.fail(token, "`until` does not close a `repeat` block")
            # The `until` expression can still read the body's locals, so
            # the frame stays live until the statement after it.
            self.repeat_closing.append((self.blocks[-1], len(self.blocks)))
            self.i += 1
        elif word == "end":
            self.pop_block(token, expected=BLOCK_CLOSED_BY_END, closer="end")
            self.i += 1
        else:
            self.i += 1

    def pop_block(self, token: Token, expected: frozenset[str] | set[str], closer: str) -> None:
        if len(self.blocks) <= 1:
            self.fail(token, f"`{closer}` closes a block that was never opened")
        block = self.blocks[-1]
        if block.kind not in expected:
            self.fail(token, f"`{closer}` closes a `{block.kind}` block")
        self.blocks.pop()

    def step_local(self, token: Token) -> None:
        following = self.peek()
        if following is not None and following.kind == "kw" and following.text == "function":
            self.i += 1
            name = self.peek()
            if name is None or name.kind != "name":
                self.fail(token, "`local function` does not name a function")
            # A `local function` IS visible inside its own body, unlike
            # `local f = function() ... end`, so it binds before the header.
            self.blocks[-1].names.add(name.text)
            self.i += 1
            self.step_function_header(token, named=False)
            return

        # Lua 5.4 attributes are per-name: `local n <const>, h <close> = ...`.
        names = self.read_name_list(token, "`local`", attributes=True)
        assign = self.peek()
        if assign is not None and assign.kind == "op" and assign.text == "=":
            self.pending.append(
                PendingLocal(names, self.blocks[-1], len(self.blocks), self.bracket))
            self.i += 1
        else:
            self.blocks[-1].names.update(names)

    def step_function_header(self, token: Token, named: bool) -> None:
        """Read `[<name path>] ( params )` and push the body's scope."""
        params: list[str] = []
        if named:
            first = self.peek()
            if first is not None and first.kind == "name":
                self.i += 1
                while True:
                    sep = self.peek()
                    if sep is None or sep.kind != "op" or sep.text not in (".", ":"):
                        break
                    part = self.peek(1)
                    if part is None or part.kind != "name":
                        self.fail(token, "function name path is truncated")
                    if sep.text == ":":
                        params.append("self")   # implicit receiver
                        self.i += 2
                        break
                    self.i += 2

        opener = self.peek()
        if opener is None or opener.kind != "op" or opener.text != "(":
            self.fail(token, "function header has no parameter list")
        self.i += 1
        while True:
            current = self.peek()
            if current is None:
                self.fail(token, "function parameter list is truncated")
            if current.kind == "op" and current.text == ")":
                self.i += 1
                break
            if current.kind == "op" and current.text == "...":
                params.append("...")
            elif current.kind == "name":
                params.append(current.text)
            else:
                self.fail(current, f"unreadable function parameter {current.text!r}")
            self.i += 1
            separator = self.peek()
            if separator is not None and separator.kind == "op" and separator.text == ",":
                self.i += 1
        self.blocks.append(Block("function", {name for name in params if name != "..."}))

    def step_for(self, token: Token) -> None:
        names = self.read_name_list(token, "`for`")
        head = self.peek()
        if head is None:
            self.fail(token, "`for` header is truncated")
        numeric = head.kind == "op" and head.text == "="
        generic = head.kind == "kw" and head.text == "in"
        if not (numeric or generic):
            self.fail(token, "`for` header is neither numeric (`=`) nor generic (`in`)")
        self.i += 1
        self.loop_headers.append(
            LoopHeader("for", names, len(self.blocks), self.bracket))

    def step_name(self, token: Token, previous: Token | None) -> None:
        if (token.text not in self.namespaces
                or (previous is not None and previous.kind == "op"
                    and previous.text in (".", ":"))
                or self.bound(token.text)):
            self.i += 1
            return

        following = self.peek(1)
        if following is None or following.kind != "op":
            self.i += 1
            return
        if following.text == ".":
            member = self.peek(2)
            if member is None or member.kind != "name":
                self.fail(token, f"`{token.text}.` is not followed by a member name")
            self.candidates.append(Candidate(token.text, member.text, token.line))
            self.i += 3
            return
        if following.text == ":":
            self.fail(token, f"`{token.text}:` is a method call on an engine namespace, "
                             "which this analyzer does not model")
        if following.text == "[":
            self.fail(token, f"`{token.text}[` indexes an engine namespace with a "
                             "computed key, so the verb it names cannot be checked")
        self.i += 1


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

@dataclass
class Finding:
    """A call site naming a verb its namespace does not provide."""

    path: str
    line: int
    namespace: str
    member: str

    def render(self) -> str:
        return (f"{self.path}:{self.line}: {self.namespace}.{self.member} is not "
                f"registered on the `{self.namespace}` namespace")


def scan_scripts(root: Path, registered: RegistrarResult) -> tuple[list[Finding], int, int]:
    """Certify every script and report the verbs that do not resolve."""
    paths = sorted(root.glob(SCRIPTS_GLOB))
    if not paths:
        raise CertificationError(
            SCRIPTS_GLOB, 0,
            "no Lua scripts matched: this gate would certify nothing")

    namespaces = frozenset(registered.namespaces)
    findings: list[Finding] = []
    references = 0
    for path in paths:
        rel = str(path.relative_to(root))
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError) as error:
            raise CertificationError(rel, 0, f"unreadable script: {error}") from error
        candidates = LuaScopeReader(rel, lex_lua(rel, text), namespaces).run()
        references += len(candidates)
        for candidate in candidates:
            if candidate.member not in registered.provided(candidate.namespace):
                findings.append(
                    Finding(rel, candidate.line, candidate.namespace, candidate.member))
    return findings, references, len(paths)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Gate every Lua call site against the engine's registration set.")
    parser.add_argument("--json", action="store_true",
                        help="emit the report as JSON on stdout")
    parser.add_argument("--root", type=Path, default=REPO_ROOT,
                        help="repository root to audit (defaults to this tool's own)")
    args = parser.parse_args(argv)

    try:
        registered = collect_registrations(args.root)
        findings, references, scripts = scan_scripts(args.root, registered)
    except CertificationError as error:
        if args.json:
            print(json.dumps({"certified": False, "error": str(error),
                              "path": error.path, "line": error.line}, indent=2))
        else:
            print(f"CERTIFICATION FAILURE: {error}", file=sys.stderr)
            print("This audit could not vouch for what it read, so it reports no "
                  "result at all rather than a clean one.", file=sys.stderr)
        return EXIT_CERTIFICATION

    # The counts are derived from source on every run, never pinned: adding
    # a registration in an already-recognized block is an ordinary change
    # and must not fail merely because a number moved.
    summary = {
        "certified": True,
        "registrar_files": registered.files,
        "namespaces": len(registered.namespaces),
        "registrations": registered.registrations,
        "scripts": scripts,
        "references": references,
        "findings": [
            {"path": f.path, "line": f.line, "namespace": f.namespace, "member": f.member}
            for f in findings
        ],
    }
    if args.json:
        print(json.dumps(summary, indent=2))
    else:
        for finding in findings:
            print(finding.render(), file=sys.stderr)
        print(f"lua registration audit: {registered.registrations} registrations across "
              f"{len(registered.namespaces)} namespaces from {registered.files} registrar "
              f"modules; {references} namespaced references across {scripts} scripts; "
              f"{len(findings)} finding(s)")
    return EXIT_FINDINGS if findings else EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
