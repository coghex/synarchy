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
certified as clean. So is a CPP directive that can rewrite or hide an
import (`#define`, `#undef`, `#include`); `test-headless/` uses none.

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
from unicode_operator_audit import haskell_code_only  # type: ignore  # noqa: E402
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

_REWRITING_CPP_DIRECTIVE = re.compile(
    r"^[ \t]*#[ \t]*(define|undef|include)\b", re.MULTILINE)


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


def find_violations(text: str, rel_path: str) -> list[Violation]:
    """Every banned import in one module's source."""
    directive = _REWRITING_CPP_DIRECTIVE.search(text)
    if directive:
        return [Violation(
            rel_path, _line_of(text, directive.start()),
            f"CPP #{directive.group(1)} can rewrite or hide an import, so "
            f"this file cannot be certified; {SCOPED_TREE}/ uses none")]
    code_text = haskell_code_only(text).replace("\0", " ")
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
    tree = repo_root / SCOPED_TREE
    paths = sorted({*tree.glob("**/*.hs"), *tree.glob("**/*.hs-boot")})
    for path in paths:
        rel = path.relative_to(repo_root).as_posix()
        if rel in EXEMPTIONS:
            continue
        violations.extend(find_violations(path.read_text(encoding="utf-8"), rel))
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
    ("a rewriting CPP directive refuses the file",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "#define BOOT initializeEngineHeadless\n"
     "import Engine.Core.Init (EngineInitResult(..))\n", [3]),
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


def self_test() -> int:
    failures: list[str] = []
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
             + len(EXEMPTION_FIXTURES))
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
