#!/usr/bin/env python3
"""Guard (#2648): no `test-headless/` module imports the production
`Engine.Core.Init.initializeEngineHeadless`.

Engine contracts §Headless fixture logging (#1925) requires every
`test-headless` engine to boot through `Test.Headless.Harness.Log`. The
production initializer hard-wires `LogToHandle stdout`, and the backend
can only be steered BEFORE initialization, so a fixture booting it
directly ignores `SYNARCHY_TEST_LOG=quiet`, cannot be redirected with
`SYNARCHY_TEST_LOG=stderr`, and writes every enabled initialization-time
category into the test runner's stdout. The contract said "never" and
nothing enforced it: #2121 landed the Chop authority fixture on the
production initializer after #2143 had migrated the whole suite. This
audit is what would have caught it.

WHAT IS DETECTED: an import declaration of exactly `Engine.Core.Init`
that brings `initializeEngineHeadless` into scope, qualified or not:

  * an import list naming it -- `import Engine.Core.Init
    (initializeEngineHeadless, EngineInitResult(..))`, over one line or
    several;
  * an UNRESTRICTED import -- `import Engine.Core.Init`,
    `import qualified Engine.Core.Init as I`, or the ImportQualifiedPost
    spelling -- because it exports every name, this one included;
  * a `hiding` import whose list does not hide it.

Never flagged: an import naming only other exports -- `EngineInitResult`,
`resolveConfigPath`, `migrateLegacyConfig` and the rest, which specs
across the suite legitimately use -- or the harness's own
`initializeEngineHeadlessWith`, and any mention inside a comment or a
string literal. Comment and string awareness is
`unicode_operator_audit.py`'s lexer (`haskell_code_only`), and the
layout-aware declaration splitter is `lua_strict_decode_audit.py`'s
(`haskell_import_declarations`), rather than second copies of either.

An `Engine.Core.Init` import in a shape this module does not model is a
failure, not a pass: a file whose import was not understood cannot be
certified as clean. So is EVERY file the C preprocessor runs on;
`test-headless/` has none. Rejecting directives alone is not enough: a
backslash-newline splice, a `/**/` comment pasting two halves of a
module name, or a macro defined outside the file through `cpp-options`
each rewrites an import with no directive in the file, and `ghc -E`
confirms every one (#2648 review). Certifying preprocessed source would
take a preprocessor, so this audit refuses such a file instead.

Whether CPP runs follows GHC 9.12, checked against the compiler
(#2648 review):

  * Only HEADER pragmas count: the comment spans that open with `{-#`
    before the file's first code token. GHC ignores a pragma after
    `module`, or anywhere past the header, with `-Wmisplaced-pragmas`.
    GHC also ends the header at a known non-options pragma such as
    `INLINE`. This audit does not keep that keyword list, so it reads a
    CPP pragma after one as live and refuses the file.
    Pragma text in a string literal, a line comment or an enclosing
    block comment is not a pragma at all. A line-1 `#!` shebang is
    skipped, as GHC skips it, and is blanked before lexing so a quote
    or `{-` in it cannot swallow the imports.
  * The keyword must directly follow `{-#` (case-insensitively); after
    `{-# {- note -}` GHC no longer reads the pragma as LANGUAGE.
  * A LANGUAGE pragma is its comma-separated names with comments
    removed, so `CPP{- note -}` and `CPP -- note` enable CPP, while
    `{- CPP -}` and `-- CPP` do not.
  * An OPTIONS/OPTIONS_GHC pragma is split the way GHC's `toArgs` splits
    it: a `[..]` list of string literals, or whitespace-separated words
    where a `"..."` literal is one argument and `-Wall"-XCPP"` is a
    single, different one. Its comments are NOT removed; GHC hands them
    on as flags and fails. A string literal holding a backslash is not
    decoded -- `"-X\67PP"` is `-XCPP` to GHC -- and counts as enabling
    CPP. That is the one fail-closed reading here.
  * `CPP`/`-XCPP`/`-cpp` switch it on and `NoCPP`/`-XNoCPP` off, in
    order, starting from the package's setting: a later `NoCPP` wins.

The package setting is on when any `*.cabal` file at the repository root
names the CPP extension or flag outside a comment. That check is coarse
(a component other than the headless suite counts too), and it can only
refuse more. Where CPP does not run, `#define` text can only sit in a
comment or string -- GHC would reject it anywhere else -- and is ignored
with them.

Out of scope, and absent from `test-headless/`: source formats that are
not plain Haskell (`.lhs`, `.hsig`, and Cabal-preprocessed `.hsc`, `.x`,
`.y` and the like) and custom preprocessors (`-F -pgmF`, `-pgmP`).

Whitespace is GHC's: `lua_strict_decode_audit`'s splitter skips every
character `str.isspace` accepts except the newline, so a non-breaking
space, form feed or vertical tab between `where`, `;` or `{` and an
import hides nothing, and it measures layout columns with GHC's
eight-column tab stops. Comments are blanked before splitting, so
`{- note -} import ...` is a declaration too. A tab inside a comment
stays a tab, so every column after it stays GHC's.

The single exemption is `Test.Headless.Harness.Log`, the boundary that
owns the backend choice (requirement 4). It is matched by exact path, so
a sibling module or a same-named file elsewhere is scanned as usual.

Usage:
  python3 tools/headless_init_import_audit.py              # the gate
  python3 tools/headless_init_import_audit.py --self-test  # fixture suite
"""
from __future__ import annotations

import argparse
import re
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from unicode_operator_audit import (  # type: ignore  # noqa: E402
    haskell_code_only, haskell_comment_spans)
from lua_strict_decode_audit import (  # type: ignore  # noqa: E402
    haskell_import_declarations)

SCOPED_TREE = "test-headless"
INIT_MODULE = "Engine.Core.Init"
BANNED_IDENTIFIER = "initializeEngineHeadless"
HARNESS_MODULE = "Test.Headless.Harness.Log"

# Repo-relative path -> the reason it is exempt. Whole-file and exact.
EXEMPTIONS: dict[str, str] = {
    "test-headless/Test/Headless/Harness/Log.hs":
        "the harness is the one place a fixture's log backend is chosen, "
        "and wraps Engine.Core.Init's initializers (#1925)",
}

# `INIT_MODULE` as a whole module path, not the prefix of a longer one.
_MENTIONS_INIT_MODULE = re.compile(
    r"(?<![\w'.])" + re.escape(INIT_MODULE) + r"(?![\w'.])")

# Haskell 2010 report SS5.3 plus `safe` and GHC2024's
# ImportQualifiedPost: `import [safe] [qualified] Engine.Core.Init
# [qualified] [as Alias] [hiding] [( ... )]`. A `{-# SOURCE #-}` pragma
# or a package-import string is already blanked by the lexer.
_INIT_IMPORT = re.compile(
    r"\Aimport\s+(?:safe\s+)?(?:qualified\s+)?"
    + re.escape(INIT_MODULE) + r"(?![\w'.])"
    r"(?:\s+qualified(?![\w']))?"
    r"(?:\s+as\s+\w[\w']*(?:\.\w[\w']*)*)?"
    r"(?P<rest>[\s\S]*)\Z")

_HIDING = re.compile(r"\Ahiding(?![\w'])\s*(?P<list>[\s\S]*)\Z")

_BANNED_TOKEN = re.compile(
    r"(?<![\w'.])" + re.escape(BANNED_IDENTIFIER) + r"(?![\w'])")

# A real pragma's keyword; GHC reads pragma keywords case-insensitively.
# Extension names and flags are case-sensitive.
_PRAGMA_KEYWORD = re.compile(r"\A\{-#\s*(?P<keyword>\w+)")
_LANGUAGE_CPP_SWITCHES = {"CPP": True, "NoCPP": False}
_OPTIONS_CPP_SWITCHES = {"-XCPP": True, "-cpp": True, "-XNoCPP": False}

# CPP enabled at the package level: the extension in an extensions field
# or the flag in `ghc-options`. `cpp-options` names no extension and
# matches neither spelling.
_CABAL_ENABLES_CPP = re.compile(r"(?<![\w-])(?:CPP|-XCPP|-cpp)(?![\w-])")


@dataclass(frozen=True)
class Violation:
    path: str
    line: int
    reason: str

    def __str__(self) -> str:
        return f"{self.path}:{self.line}: {self.reason}"


def _line_of(text: str, pos: int) -> int:
    return text.count("\n", 0, pos) + 1


def _import_list(text: str) -> str | None:
    """The contents of a parenthesised import list that is the whole of
    `text`, or None when `text` is not exactly one balanced list."""
    if not text.startswith("("):
        return None
    depth = 0
    for i, char in enumerate(text):
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return text[1:i] if not text[i + 1:].strip() else None
    return None


def _classify(decl: str) -> str | None:
    """Why this `INIT_MODULE` import exposes the banned name, or None if
    it does not. Raises ValueError for a shape it does not model."""
    match = _INIT_IMPORT.match(decl)
    if match is None:
        raise ValueError("unrecognised import shape")
    rest = match.group("rest").strip()
    if not rest:
        return (f"unrestricted import of {INIT_MODULE} exposes "
                f"{BANNED_IDENTIFIER}")
    hiding = _HIDING.match(rest)
    names = _import_list(hiding.group("list") if hiding else rest)
    if names is None:
        raise ValueError("unterminated or trailing text after import list")
    named = _BANNED_TOKEN.search(names) is not None
    if hiding:
        return None if named else (
            f"hiding import of {INIT_MODULE} still exposes "
            f"{BANNED_IDENTIFIER}")
    return (f"imports {INIT_MODULE}.{BANNED_IDENTIFIER}"
            if named else None)


def _blank_shebang(text: str) -> str:
    """`text` with a line-1 `#!` line blanked, as GHC skips it. Left in,
    a quote or `{-` there would open a string or comment for the lexer
    and swallow the imports below it."""
    if not text.startswith("#!"):
        return text
    end = text.find("\n")
    end = len(text) if end < 0 else end
    return " " * end + text[end:]


def _header_pragmas(source: str) -> list[tuple[int, str]]:
    """The pragmas GHC reads for LANGUAGE and OPTIONS, as `(start,
    text)`: every comment span opening with `{-#` before the first code
    token. The lexer reports a nested block comment as its outermost
    span and never reports string contents, so a pragma quoted in a
    string, a line comment or another block comment is not one of these.
    Past the first code token GHC ignores even a real pragma."""
    masked = haskell_code_only(source)
    header_end = next((i for i, char in enumerate(masked)
                       if char != "\0" and not char.isspace()), len(masked))
    return [(start, source[start:end])
            for start, end in haskell_comment_spans(source)
            if start < header_end and source.startswith("{-#", start)]


def _read_string(text: str, start: int) -> tuple[str, int] | None:
    """The Haskell string literal opening at `text[start]` and the index
    after it, or None when it is unterminated or holds a backslash. An
    escape could spell any flag (`"-X\\67PP"`), and decoding them is left
    undone deliberately: the caller treats None as enabling CPP."""
    end = text.find('"', start + 1)
    if end < 0 or "\\" in text[start + 1:end] or "\n" in text[start + 1:end]:
        return None
    return text[start + 1:end], end + 1


def _ghc_args(body: str) -> list[str] | None:
    """An OPTIONS pragma body split as GHC's `toArgs` splits it, or None
    when an argument cannot be read without decoding an escape."""
    text = body.strip()
    args: list[str] = []
    if text.startswith("["):
        i = 1
        while True:
            while i < len(text) and text[i].isspace():
                i += 1
            if i < len(text) and text[i] == "]" and not args:
                return args if not text[i + 1:].strip() else None
            if i >= len(text) or text[i] != '"':
                return None
            read = _read_string(text, i)
            if read is None:
                return None
            args.append(read[0])
            i = read[1]
            while i < len(text) and text[i].isspace():
                i += 1
            if i < len(text) and text[i] == ",":
                i += 1
                continue
            if i < len(text) and text[i] == "]":
                return args if not text[i + 1:].strip() else None
            return None
    i = 0
    while True:
        while i < len(text) and text[i].isspace():
            i += 1
        if i >= len(text):
            return args
        part = i
        while i < len(text) and not text[i].isspace() and text[i] != '"':
            i += 1
        if i < len(text) and text[i] == '"':
            read = _read_string(text, i)
            if read is None:
                return None
            quoted, i = read
            # A literal alone is its contents; glued to a word it keeps
            # its quotes, as `toArgs` shows it back.
            args.append(quoted if part == i - len(quoted) - 2
                        else f'{text[part:i - len(quoted) - 2]}"{quoted}"')
        else:
            args.append(text[part:i])


def _pragma_cpp_switches(pragma: str) -> list[bool]:
    """The CPP switches one header pragma makes, in order."""
    keyword = _PRAGMA_KEYWORD.match(pragma)
    if keyword is None:
        return []
    body = pragma[keyword.end():]
    body = body[:-len("#-}")] if body.endswith("#-}") else body
    name = keyword.group("keyword").upper()
    if name == "LANGUAGE":
        names = re.split(r"[\s,\0]+", haskell_code_only(body))
        return [_LANGUAGE_CPP_SWITCHES[n] for n in names
                if n in _LANGUAGE_CPP_SWITCHES]
    if name in ("OPTIONS", "OPTIONS_GHC"):
        args = _ghc_args(body)
        if args is None:
            return [True]
        return [_OPTIONS_CPP_SWITCHES[a] for a in args
                if a in _OPTIONS_CPP_SWITCHES]
    return []


def _cpp_runs(source: str, package_cpp: bool) -> tuple[bool, int | None]:
    """Whether CPP runs on the file, and where the header pragma that
    last switched it on starts (None when that is the package)."""
    runs, where = package_cpp, None
    for start, pragma in _header_pragmas(source):
        for switch in _pragma_cpp_switches(pragma):
            runs, where = switch, (start if switch else None)
    return runs, where


def cabal_enables_cpp(repo_root: Path) -> bool:
    """True if any root `*.cabal` file enables CPP anywhere in it."""
    for cabal in sorted(repo_root.glob("*.cabal")):
        code = "\n".join(
            line for line in cabal.read_text(encoding="utf-8").splitlines()
            if not line.lstrip().startswith("--"))
        if _CABAL_ENABLES_CPP.search(code):
            return True
    return False


def find_violations(text: str, rel_path: str,
                    cpp_everywhere: bool = False) -> list[Violation]:
    """Every banned import in one module's source. `cpp_everywhere` says
    the package enables CPP for every module (`cabal_enables_cpp`)."""
    source = _blank_shebang(text)
    runs, pragma = _cpp_runs(source, cpp_everywhere)
    if runs:
        cause = ("this pragma enables CPP" if pragma is not None
                 else "the package enables CPP for every module")
        return [Violation(
            rel_path, 1 if pragma is None else _line_of(text, pragma),
            f"{cause}, and a directive, a line splice, a /**/ comment or a "
            f"macro defined outside the file can each rewrite an import "
            f"before GHC sees it, so this file cannot be certified; "
            f"{SCOPED_TREE}/ uses no CPP")]
    # Blank what the lexer masked, keeping a tab a tab: GHC advances a
    # comment's tab to the next tab stop, and a space would shift every
    # later column on the line out of layout.
    code_text = "".join(
        ("\t" if original == "\t" else " ") if masked == "\0" else masked
        for masked, original in zip(haskell_code_only(source), source))
    violations: list[Violation] = []
    for start, _end, decl in haskell_import_declarations(code_text):
        if not _MENTIONS_INIT_MODULE.search(decl):
            continue
        line = _line_of(text, start)
        try:
            reason = _classify(decl.strip())
        except ValueError as error:
            reason = (f"cannot classify this {INIT_MODULE} import ({error}); "
                      f"teach tools/headless_init_import_audit.py the shape "
                      f"rather than letting the file go unchecked:\n    "
                      + " ".join(decl.split()))
        if reason is not None:
            violations.append(Violation(rel_path, line, reason))
    return violations


def scan_tree(repo_root: Path) -> list[Violation]:
    violations: list[Violation] = []
    cpp_everywhere = cabal_enables_cpp(repo_root)
    tree = repo_root / SCOPED_TREE
    paths = sorted({*tree.glob("**/*.hs"), *tree.glob("**/*.hs-boot")})
    for path in paths:
        rel = path.relative_to(repo_root).as_posix()
        if rel in EXEMPTIONS:
            continue
        violations.extend(find_violations(
            path.read_text(encoding="utf-8"), rel, cpp_everywhere))
    return violations


# ---------------------------------------------------------------------
# Self-test
# ---------------------------------------------------------------------

_HEAD = "module M where\n"

# `(label, source, lines that must be reported in order)`
DETECTED_FIXTURES: list[tuple[str, str, list[int]]] = [
    ("the #2121 shape: an explicit list naming it",
     _HEAD + "import Engine.Core.Init (initializeEngineHeadless, "
     "EngineInitResult(..))\n", [2]),
    ("the name alone",
     _HEAD + "import Engine.Core.Init (initializeEngineHeadless)\n", [2]),
    ("a multiline list naming it on a continuation line",
     _HEAD + "import Engine.Core.Init\n"
     "  ( EngineInitResult(..)\n"
     "  , initializeEngineHeadless\n"
     "  )\n", [2]),
    ("a qualified import whose list names it",
     _HEAD + "import qualified Engine.Core.Init as I "
     "(initializeEngineHeadless)\n", [2]),
    ("an unrestricted import",
     _HEAD + "import Engine.Core.Init\n", [2]),
    ("an unrestricted qualified import",
     _HEAD + "import qualified Engine.Core.Init as I\n", [2]),
    ("an unrestricted qualified import with no alias",
     _HEAD + "import qualified Engine.Core.Init\n", [2]),
    ("an unrestricted ImportQualifiedPost import",
     _HEAD + "import Engine.Core.Init qualified as I\n", [2]),
    ("a multiline unrestricted qualified import",
     _HEAD + "import qualified\n  Engine.Core.Init\n    as I\n", [2]),
    ("a hiding import that hides something else",
     _HEAD + "import Engine.Core.Init hiding (initializeEngine)\n", [2]),
    ("a SOURCE pragma and a package import are blanked, not shields",
     _HEAD + "import {-# SOURCE #-} \"synarchy\" Engine.Core.Init\n", [2]),
    ("an indented top-level layout",
     _HEAD + "  import Engine.Core.Init (initializeEngineHeadless)\n", [2]),
    ("a real import after a commented-out one is still found",
     _HEAD + "-- import Engine.Core.Init (EngineInitResult(..))\n"
     "import Engine.Core.Init (initializeEngineHeadless)\n", [3]),
    ("every offending import is reported",
     _HEAD + "import Engine.Core.Init (initializeEngineHeadless)\n"
     "import Data.IORef (newIORef)\n"
     "import qualified Engine.Core.Init as I\n", [2, 4]),
    ("an unmodelled shape fails rather than passing",
     _HEAD + "import Engine.Core.Init (EngineInitResult(..)) junk\n", [2]),
    ("an unterminated list fails rather than passing",
     _HEAD + "import Engine.Core.Init (EngineInitResult(..)\n", [2]),
    ("the first import on the `module ... where` line itself (#2648 "
     "review): no newline, `{` or `;` precedes it",
     "module M where import Engine.Core.Init (initializeEngineHeadless); "
     "fixture = initializeEngineHeadless\n", [1]),
    ("an unrestricted import on the `where` line, layout continuing below",
     "module M (spec) where import Engine.Core.Init\n"
     "                      import Data.IORef (newIORef)\n", [1]),
    ("a CPP file with a rewriting directive is refused at its pragma",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "#define BOOT initializeEngineHeadless\n"
     "import Engine.Core.Init (EngineInitResult(..))\n", [1]),
    ("CPP in a LANGUAGE list",
     "{-# LANGUAGE OverloadedStrings, CPP #-}\n" + _HEAD
     + "{-\n#include \"boot.h\"\n-}\n", [1]),
    ("CPP in a LANGUAGE list spread over lines",
     "{-# LANGUAGE OverloadedStrings,\n             CPP #-}\n" + _HEAD, [1]),
    ("a lower-case pragma keyword still enables CPP",
     "{-# language CPP #-}\n" + _HEAD + "#undef X\n", [1]),
    ("CPP through OPTIONS_GHC -XCPP",
     "{-# OPTIONS_GHC -Wall -XCPP #-}\n" + _HEAD + "#define X Y\n", [1]),
    ("CPP through OPTIONS_GHC -cpp",
     "{-# OPTIONS_GHC -cpp #-}\n" + _HEAD + "  #  define X Y\n", [1]),
    ("CPP through the deprecated OPTIONS pragma",
     "{-# OPTIONS -cpp #-}\n" + _HEAD, [1]),
    ("`CPP{- note -}`: a nested comment directly after the name (#2648 "
     "review; ghc -E expands the macro)",
     "{-# LANGUAGE CPP{- note -} #-}\n" + _HEAD
     + "#define BOOT Engine.Core.Init\n"
     "import BOOT (initializeEngineHeadless)\n"
     "fixture = initializeEngineHeadless\n", [1]),
    ("a quoted OPTIONS_GHC argument, as GHC's toArgs reads it (#2648 "
     "review)",
     "{-# OPTIONS_GHC \"-XCPP\" #-}\n" + _HEAD
     + "#define BOOT Engine.Core.Init\n"
     "import BOOT (initializeEngineHeadless)\n", [1]),
    ("OPTIONS_GHC's bracketed list form",
     "{-# OPTIONS_GHC [\"-Wall\", \"-XCPP\"] #-}\n" + _HEAD, [1]),
    ("a string escape could spell the flag, so any backslash in OPTIONS "
     "counts",
     "{-# OPTIONS_GHC \"-X\\67PP\" #-}\n" + _HEAD, [1]),
    ("CPP -- note: a line comment after the name inside the pragma",
     "{-# LANGUAGE CPP -- note\n #-}\n" + _HEAD, [1]),
    ("a comment before the name inside the pragma",
     "{-# LANGUAGE {- a -} CPP #-}\n" + _HEAD, [1]),
    ("the name alone on its own line", "{-# LANGUAGE\nCPP\n#-}\n" + _HEAD, [1]),
    ("no spaces inside the braces", "{-#LANGUAGE CPP#-}\n" + _HEAD, [1]),
    ("after a line-1 shebang, which GHC skips",
     "#!/usr/bin/env runghc\n{-# LANGUAGE CPP #-}\n" + _HEAD, [2]),
    ("after an unrecognised pragma, which does not end the header",
     "{-# FOO bar #-}\n{-# LANGUAGE CPP #-}\n" + _HEAD, [2]),
    ("after comments and blank lines",
     "-- c\n\n{- b -}\n{-# LANGUAGE CPP #-}\n" + _HEAD, [4]),
    ("a module with no header: the pragma precedes the first declaration",
     "{-# LANGUAGE CPP #-}\nmain :: IO ()\nmain = pure ()\n", [1]),
    ("a quoted OPTIONS_GHC argument followed by another",
     "{-# OPTIONS_GHC \"-XCPP\" -Wall #-}\n" + _HEAD, [1]),
    ("a `\\&` escape in a quoted argument is not decoded",
     "{-# OPTIONS_GHC \"-XC\\&PP\" #-}\n" + _HEAD, [1]),
    ("fail-closed limit: GHC's header also ends at a known non-options "
     "pragma such as INLINE, which this audit does not model, so it "
     "refuses rather than certifies",
     "{-# INLINE f #-}\n{-# LANGUAGE CPP #-}\n" + _HEAD, [2]),
    ("a later pragma switches CPP back on after NoCPP",
     "{-# LANGUAGE NoCPP #-}\n{-# OPTIONS_GHC -cpp #-}\n" + _HEAD, [2]),
    ("a shebang holding a quote cannot swallow the imports",
     "#!/usr/bin/env runghc \"x\n" + _HEAD
     + "import Engine.Core.Init (initializeEngineHeadless)\n", [3]),
    ("a shebang holding `{-` cannot swallow the imports",
     "#!/bin/sh {-\n" + _HEAD
     + "import Engine.Core.Init (initializeEngineHeadless)\n", [3]),
    ("a line splice rebuilds the banned import with no directive at all "
     "(#2648 review; ghc -E joins the lines)",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "import Engine.Core.\\\n"
     "Init (initializeEngineHeadless)\n"
     "fixture = initializeEngineHeadless\n", [1]),
    ("a /**/ comment pastes the module name together under ghc -E",
     "{-# LANGUAGE CPP #-}\n" + _HEAD
     + "import Engine.Core./**/Init (initializeEngineHeadless)\n", [1]),
    ("a macro from cpp-options needs nothing in the file at all",
     "{-# LANGUAGE CPP #-}\n" + _HEAD
     + "import BOOT (initializeEngineHeadless)\n", [1]),
    ("a CPP file importing only allowed names is refused too",
     "{-# LANGUAGE CPP #-}\n" + _HEAD
     + "import Engine.Core.Init (EngineInitResult(..))\n", [1]),
    ("an import at the start of a line indented with a non-breaking space",
     _HEAD + "\u00a0import Engine.Core.Init (initializeEngineHeadless)\n", [2]),
    ("a non-breaking space after an explicit-layout `;`",
     "module M where { import Data.IORef (newIORef);\u00a0"
     "import Engine.Core.Init (initializeEngineHeadless) }\n", [1]),
    ("a block comment before the import on its own line",
     _HEAD + "{- note -} import Engine.Core.Init (initializeEngineHeadless)\n",
     [2]),
    ("a list continued on lines indented with non-breaking spaces",
     _HEAD + "import Engine.Core.Init\n"
     "\u00a0\u00a0( EngineInitResult(..)\n"
     "\u00a0\u00a0, initializeEngineHeadless )\n", [2]),
    ("a real CPP pragma after a header comment",
     "-- header\n{-# LANGUAGE CPP #-}\n" + _HEAD
     + "{-\n#define X Y\n-}\n", [2]),
    ("a real CPP pragma carrying a spaced nested comment",
     "{-# LANGUAGE CPP {- explanatory note -} #-}\n" + _HEAD
     + "#define MOD Engine.Core.Init\n", [1]),
    ("a real CPP pragma spaced with non-breaking spaces",
     "{-#\u00a0LANGUAGE\u00a0CPP\u00a0#-}\n" + _HEAD + "#define X Y\n", [1]),
    ("a tab-containing comment before a banned import",
     _HEAD + "{-\t-} import Engine.Core.Init (initializeEngineHeadless)\n"
     "           fixture = initializeEngineHeadless\n", [2]),
] + [
    (f"the `where`-line import after U+{ord(space):04X} -- GHC skips every "
     f"Unicode space separator, form feed and vertical tab (#2648 review)",
     f"module M where{space}import Engine.Core.Init "
     f"(initializeEngineHeadless); fixture = initializeEngineHeadless\n", [1])
    for space in ("\u00a0", "\f", "\v", "\u2003", "\u3000")
]

CLEAN_FIXTURES: list[tuple[str, str]] = [
    ("EngineInitResult-only, the shape across the suite",
     _HEAD + "import Engine.Core.Init (EngineInitResult(..))\n"),
    ("other exports over several lines",
     _HEAD + "import Engine.Core.Init\n"
     "  ( resolveConfigPath, migrateLegacyConfig\n"
     "  , LegacyNeutralityCheck(..) )\n"),
    ("the harness's own seam, a longer name",
     _HEAD + "import Engine.Core.Init (initializeEngineHeadlessWith, "
     "EngineInitResult(..))\n"),
    ("a qualified import with a clean list",
     _HEAD + "import qualified Engine.Core.Init as I (EngineInitResult(..))\n"),
    ("a hiding import that hides it",
     _HEAD + "import Engine.Core.Init hiding (initializeEngineHeadless)\n"),
    ("the harness's quiet entry point from its own module",
     _HEAD + "import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)\n"
     "f = initializeEngineHeadlessQuiet\n"),
    ("a line comment quoting the banned import",
     _HEAD + "-- import Engine.Core.Init (initializeEngineHeadless)\n"),
    ("a block comment quoting it over several lines",
     _HEAD + "{- import Engine.Core.Init\n     (initializeEngineHeadless) -}\n"),
    ("a haddock naming the qualified function",
     _HEAD + "-- | Never 'Engine.Core.Init.initializeEngineHeadless'.\n"
     "import Engine.Core.Init (EngineInitResult(..))\n"),
    ("a string literal quoting it",
     _HEAD + "msg = \"import Engine.Core.Init (initializeEngineHeadless)\"\n"),
    ("a longer module path is a different module",
     _HEAD + "import Engine.Core.Init.Extra (initializeEngineHeadless)\n"),
    ("a different module exporting the same name",
     _HEAD + "import Other.Init (initializeEngineHeadless)\n"),
    ("quoted CPP text in a block comment of a non-CPP module (#2648 "
     "review)",
     _HEAD + "{-\n#define BOOT initializeEngineHeadless\n-}\n"),
    ("the same comment beside an allowed import",
     _HEAD + "import Engine.Core.Init (EngineInitResult(..))\n"
     "{-\n#include \"boot.h\"\n#undef BOOT\n-}\n"),
    ("a name merely containing the letters CPP is not the CPP token "
     "(lexical only: GHC rejects the unknown extension itself)",
     "{-# LANGUAGE NoCPPish #-}\n" + _HEAD + "{-\n#define X Y\n-}\n"),
    ("an allowed list continued on non-breaking-space-indented lines is "
     "one declaration, not an unrestricted import",
     _HEAD + "import Engine.Core.Init\n"
     "\u00a0\u00a0( EngineInitResult(..) )\n"),
    ("a tab-indented continuation sits at GHC's column 8, past an "
     "indented import's column 2",
     "module M where\n  import Engine.Core.Init\n\t(EngineInitResult(..))\n"
     "  x = 1\n"),
    ("a hiding list on a non-breaking-space-indented line",
     _HEAD + "import Engine.Core.Init hiding\n"
     "\u00a0(initializeEngineHeadless)\n"),
    ("a tab inside a comment before the import keeps GHC's column 11, "
     "so the next declaration aligned there is not swallowed (#2648 "
     "review)",
     _HEAD + "{-\t-} import Engine.Core.Init (EngineInitResult(..))\n"
     "           fixture = EngineInitResult\n"),
    ("tabs in a multi-line comment before the import",
     _HEAD + "{- a\n\t\tb -}\t import Engine.Core.Init (EngineInitResult(..))\n"
     + " " * 25 + "fixture = EngineInitResult\n"),
    ("OPTIONS_GHC flags that merely contain `cpp` or `CPP`",
     "{-# OPTIONS_GHC -Wall -optP-DCPP -optP-cpp #-}\n" + _HEAD
     + "{-\n#define X Y\n-}\n"),
    ("a non-LANGUAGE, non-OPTIONS pragma mentioning CPP",
     _HEAD + "{-# WARNING fixture \"needs -XCPP and CPP\" #-}\n"
     "fixture = ()\n{-\n#define X Y\n-}\n"),
    ("a nested comment before the keyword: GHC no longer reads LANGUAGE",
     "{-# {- note -} LANGUAGE CPP #-}\n" + _HEAD + "{-\n#define X Y\n-}\n"),
    ("a CPP pragma after `module` is misplaced, and GHC ignores it",
     _HEAD + "{-# LANGUAGE CPP #-}\n{-\n#define X Y\n-}\n"),
    ("CPP only inside a comment within LANGUAGE",
     "{-# LANGUAGE GADTs {- CPP -} #-}\n" + _HEAD + "{-\n#define X Y\n-}\n"),
    ("CPP only inside a line comment within LANGUAGE",
     "{-# LANGUAGE GADTs -- CPP\n #-}\n" + _HEAD + "{-\n#define X Y\n-}\n"),
    ("a later NoCPP in the same pragma wins",
     "{-# LANGUAGE CPP, NoCPP #-}\n" + _HEAD + "{-\n#define X Y\n-}\n"),
    ("-XNoCPP after -XCPP in OPTIONS_GHC",
     "{-# OPTIONS_GHC -XCPP -XNoCPP #-}\n" + _HEAD + "{-\n#define X Y\n-}\n"),
    ("a quoted literal glued to a word is one different argument",
     "{-# OPTIONS_GHC -Wall\"-XCPP\" #-}\n" + _HEAD + "{-\n#define X Y\n-}\n"),
    ("a shebang is not code: the pragma below it is still a header pragma, "
     "and a quote in the shebang hides nothing from a clean module",
     "#!/usr/bin/env runghc \"x\n" + _HEAD
     + "import Engine.Core.Init (EngineInitResult(..))\n"),
    ("a CPP pragma quoted in a string literal (#2648 review)",
     _HEAD + "message = \"{-# LANGUAGE CPP #-}\"\n"
     "{-\n#define BOOT initializeEngineHeadless\n-}\n"),
    ("a CPP pragma nested inside a block comment (#2648 review)",
     _HEAD + "{- {-# LANGUAGE CPP #-} -}\n"
     "{-\n#define BOOT initializeEngineHeadless\n-}\n"),
    ("a CPP pragma quoted in a haddock line comment",
     "-- | Enable with {-# LANGUAGE CPP #-}\n" + _HEAD
     + "{-\n#include \"boot.h\"\n-}\n"),
    ("OPTIONS_GHC -XCPP quoted in a string literal",
     _HEAD + "flags = \"{-# OPTIONS_GHC -XCPP #-}\"\n"
     "{-\n#undef BOOT\n-}\n"),
    ("the `where` keyword is matched whole: an identifier ending in it "
     "is not the layout keyword",
     _HEAD + "import Engine.Core.Init (EngineInitResult(..))\n"
     "nowhere = 0\n"),
]

_BANNED_SOURCE = _HEAD + "import Engine.Core.Init (initializeEngineHeadless)\n"

# `(relative path, expected to be reported)` for the exemption's exactness.
EXEMPTION_FIXTURES: list[tuple[str, bool]] = [
    ("test-headless/Test/Headless/Harness/Log.hs", False),
    ("test-headless/Test/Headless/Harness/LogExtra.hs", True),
    ("test-headless/Test/Headless/Harness.hs", True),
    ("test-headless/Test/Headless/Other/Harness/Log.hs", True),
    ("test-headless/Test/Headless/World/Chop/Authority.hs", True),
    ("test-headless/Test/Headless/Harness/Log.hs-boot", True),
]


_QUOTED_DIRECTIVE = _HEAD + "{-\n#define BOOT initializeEngineHeadless\n-}\n"

# `(label, root *.cabal text or None, module source, expected reported)`,
# each scanned as the only module of its own temporary tree.
TREE_FIXTURES: list[tuple[str, str | None, str, bool]] = [
    ("scan_tree reports the `where`-line import (#2648 review)", None,
     "module M where import Engine.Core.Init (initializeEngineHeadless); "
     "fixture = initializeEngineHeadless\n", True),
    ("scan_tree reports it after a non-breaking space", None,
     "module M where\u00a0import Engine.Core.Init (initializeEngineHeadless); "
     "fixture = initializeEngineHeadless\n", True),
    ("scan_tree reports it after a form feed", None,
     "module M where\fimport Engine.Core.Init (initializeEngineHeadless)\n",
     True),
    ("scan_tree certifies an allowed list on non-breaking-space lines", None,
     _HEAD + "import Engine.Core.Init\n\u00a0\u00a0( EngineInitResult(..) )\n",
     False),
    ("scan_tree ignores a string-quoted CPP pragma", None,
     _HEAD + "message = \"{-# LANGUAGE CPP #-}\"\n"
     "{-\n#define BOOT initializeEngineHeadless\n-}\n", False),
    ("scan_tree ignores a CPP pragma nested in a block comment", None,
     _HEAD + "{- {-# LANGUAGE CPP #-} -}\n"
     "{-\n#define BOOT initializeEngineHeadless\n-}\n", False),
    ("scan_tree still refuses a real CPP pragma's quoted directive", None,
     "{-# LANGUAGE CPP #-}\n" + _QUOTED_DIRECTIVE, True),
    ("scan_tree refuses `LANGUAGE CPP{- note -}` (#2648 review)", None,
     "{-# LANGUAGE CPP{- note -} #-}\n" + _HEAD
     + "#define BOOT Engine.Core.Init\nimport BOOT (initializeEngineHeadless)\n"
     "fixture = initializeEngineHeadless\n", True),
    ("scan_tree refuses a quoted `\"-XCPP\"` option (#2648 review)", None,
     "{-# OPTIONS_GHC \"-XCPP\" #-}\n" + _HEAD
     + "#define BOOT Engine.Core.Init\nimport BOOT (initializeEngineHeadless)\n"
     "fixture = initializeEngineHeadless\n", True),
    ("scan_tree refuses a line-spliced import in a CPP file (#2648 review)",
     None,
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "import Engine.Core.\\\n"
     "Init (initializeEngineHeadless)\nfixture = initializeEngineHeadless\n",
     True),
    ("scan_tree certifies an import after a tab-containing comment (#2648 "
     "review)", None,
     _HEAD + "{-\t-} import Engine.Core.Init (EngineInitResult(..))\n"
     "           fixture = EngineInitResult\n", False),
    ("package-level CPP refuses even a module with no directive",
     "test-suite t\n    default-extensions: CPP\n",
     _HEAD + "import Engine.Core.Init (EngineInitResult(..))\n", True),
    ("a file's NoCPP turns package-level CPP off",
     "test-suite t\n    default-extensions: CPP\n",
     "{-# LANGUAGE NoCPP #-}\n" + _QUOTED_DIRECTIVE, False),
    ("scan_tree ignores a misplaced CPP pragma", None,
     _HEAD + "{-# LANGUAGE CPP #-}\n{-\n#define X Y\n-}\n", False),
    ("scan_tree reports an import behind a quoting shebang", None,
     "#!/usr/bin/env runghc \"x\n" + _HEAD
     + "import Engine.Core.Init (initializeEngineHeadless)\n", True),
    ("no cabal file: quoted CPP text is only a comment", None,
     _QUOTED_DIRECTIVE, False),
    ("cpp-options alone does not enable CPP",
     "test-suite t\n    cpp-options: -DDARWIN\n", _QUOTED_DIRECTIVE, False),
    ("a commented-out cabal line does not enable CPP",
     "test-suite t\n    -- default-extensions: CPP\n",
     _QUOTED_DIRECTIVE, False),
    ("CPP in cabal default-extensions preprocesses every module",
     "test-suite t\n    default-extensions: GHC2024, CPP\n",
     _QUOTED_DIRECTIVE, True),
    ("-cpp in cabal ghc-options preprocesses every module",
     "test-suite t\n    ghc-options: -Wall -cpp\n", _QUOTED_DIRECTIVE, True),
]


def self_test() -> int:
    failures: list[str] = []
    for label, cabal, source, expected in TREE_FIXTURES:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            module = root / SCOPED_TREE / "Test" / "Fixture.hs"
            module.parent.mkdir(parents=True)
            module.write_text(source, encoding="utf-8")
            if cabal is not None:
                (root / "fixture.cabal").write_text(cabal, encoding="utf-8")
            got = bool(scan_tree(root))
            if got != expected:
                failures.append(
                    f"TREE {label}: expected reported={expected}, got {got}")
    for label, source, lines in DETECTED_FIXTURES:
        got = [v.line for v in find_violations(source, "Fixture.hs")]
        if got != lines:
            failures.append(f"DETECT {label}: expected lines {lines}, got {got}")
    for label, source in CLEAN_FIXTURES:
        got = find_violations(source, "Fixture.hs")
        if got:
            failures.append(f"CLEAN {label}: unexpected {[str(v) for v in got]}")
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        for rel, _ in EXEMPTION_FIXTURES:
            path = root / rel
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(_BANNED_SOURCE, encoding="utf-8")
        reported = {v.path for v in scan_tree(root)}
        for rel, expected in EXEMPTION_FIXTURES:
            if (rel in reported) != expected:
                failures.append(
                    f"EXEMPTION {rel}: expected reported={expected}, "
                    f"got {rel in reported}")
    for rel, reason in EXEMPTIONS.items():
        if not reason.strip():
            failures.append(f"EXEMPTION {rel} carries no reason")
    total = (len(DETECTED_FIXTURES) + len(CLEAN_FIXTURES)
             + len(EXEMPTION_FIXTURES) + len(TREE_FIXTURES))
    if failures:
        print(f"headless_init_import_audit self-test: {len(failures)} of "
              f"{total} case(s) FAILED:")
        for failure in failures:
            print(f"  {failure}")
        return 1
    print(f"headless_init_import_audit self-test: all {total} cases passed.")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--self-test", action="store_true",
                        help="run the fixture suite instead of the gate")
    args = parser.parse_args(argv)
    if args.self_test:
        return self_test()
    missing = [rel for rel in EXEMPTIONS if not (REPO_ROOT / rel).is_file()]
    if missing:
        print(f"Stale exemption(s), no such file: {', '.join(missing)}")
        return 1
    violations = scan_tree(REPO_ROOT)
    if violations:
        print(f"{len(violations)} {SCOPED_TREE}/ import(s) of the production "
              f"{INIT_MODULE}.{BANNED_IDENTIFIER}:")
        for violation in violations:
            print(f"  {violation}")
        print(f"\nIt hard-wires the engine log to stdout, so SYNARCHY_TEST_LOG "
              f"cannot steer it. Boot through {HARNESS_MODULE} "
              f"(initializeEngineHeadlessQuiet, or "
              f"initializeEngineHeadlessLogging for a spec that wants the "
              f"entries); see engine contracts §Headless fixture logging.")
        return 1
    print(f"No {SCOPED_TREE}/ module imports {INIT_MODULE}."
          f"{BANNED_IDENTIFIER} outside {HARNESS_MODULE}.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
