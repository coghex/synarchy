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

CPP runs on a file when one of its real pragmas enables it, or when any
`*.cabal` file at the repository root enables CPP. A real pragma is a
top-level comment span, as the shared lexer reports it, that itself
opens with `{-#`. Pragma text inside a string literal, a line comment or
an enclosing block comment is quoted, and GHC does not act on it. A real
pragma is read fail-closed (`_pragma_enables_cpp`): a `CPP` token
anywhere in a LANGUAGE pragma, nested comments included; `-XCPP` or
`-cpp` anywhere in an OPTIONS/OPTIONS_GHC pragma, quoted or not, or any
backslash there, since a string escape could spell either; and either
spelling in a pragma whose keyword cannot be read. Every real pragma
counts, not only those in the file header GHC reads. The cabal check is
coarse in the same direction: a CPP extension or flag anywhere in the
package counts. Where CPP does not run, `#define` text can only sit in a
comment or string -- GHC would reject it anywhere else -- and is ignored
with them.

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
# `CPP` as a whole token: `CPP{- note -}` is one, `NoCPPish` is not.
_LANGUAGE_CPP = re.compile(r"(?<![\w'])CPP(?![\w'])")
# `-XCPP`/`-cpp` as a whole flag, quoted (`"-XCPP"`, `["-XCPP"]`) or not.
_OPTIONS_CPP = re.compile(r"(?<![\w-])-(?:XCPP|cpp)(?![\w-])")

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


def _real_pragmas(text: str) -> list[tuple[int, str]]:
    """Every pragma GHC could act on, as `(start, text)`: each top-level
    comment span that itself opens with `{-#`. The lexer reports a
    nested block comment as its outermost span and never reports string
    contents, so a pragma quoted in a string, a line comment or another
    block comment is not one of these."""
    return [(start, text[start:end])
            for start, end in haskell_comment_spans(text)
            if text.startswith("{-#", start)]


def _pragma_enables_cpp(pragma: str) -> bool:
    """True if one real pragma may switch CPP on, read fail-closed.

    Rather than parse a pragma body the way GHC does -- nested comments
    in a LANGUAGE list, Haskell string literals and escapes in an
    OPTIONS_GHC argument list -- this errs toward CPP: a false positive
    only refuses a file, a false negative certifies preprocessed source.
    """
    keyword = _PRAGMA_KEYWORD.match(pragma)
    if keyword is None:
        return bool(_LANGUAGE_CPP.search(pragma) or _OPTIONS_CPP.search(pragma))
    name = keyword.group("keyword").upper()
    body = pragma[keyword.end():]
    if name == "LANGUAGE":
        return _LANGUAGE_CPP.search(body) is not None
    if name in ("OPTIONS", "OPTIONS_GHC"):
        return _OPTIONS_CPP.search(body) is not None or "\\" in body
    return False


def _cpp_pragma(text: str) -> int | None:
    """Where the file's first CPP-enabling real pragma starts, or None."""
    for start, pragma in _real_pragmas(text):
        if _pragma_enables_cpp(pragma):
            return start
    return None


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
    pragma = _cpp_pragma(text)
    if pragma is not None or cpp_everywhere:
        source = ("this pragma enables CPP" if pragma is not None
                  else "the package enables CPP for every module")
        return [Violation(
            rel_path, 1 if pragma is None else _line_of(text, pragma),
            f"{source}, and a directive, a line splice, a /**/ comment or a "
            f"macro defined outside the file can each rewrite an import "
            f"before GHC sees it, so this file cannot be certified; "
            f"{SCOPED_TREE}/ uses no CPP")]
    # Blank what the lexer masked, keeping a tab a tab: GHC advances a
    # comment's tab to the next tab stop, and a space would shift every
    # later column on the line out of layout.
    code_text = "".join(
        ("\t" if original == "\t" else " ") if masked == "\0" else masked
        for masked, original in zip(haskell_code_only(text), text))
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
    ("a pragma whose keyword cannot be read still counts",
     "{-# {- note -} LANGUAGE CPP #-}\n" + _HEAD, [1]),
    ("a CPP pragma below the header still counts",
     _HEAD + "{-# LANGUAGE CPP #-}\n", [2]),
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
