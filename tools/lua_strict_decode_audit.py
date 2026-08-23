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

WHAT IS DETECTED, exactly: the identifier lexeme `decodeUtf8` occurring
in genuine Haskell code under the scoped tree -- whatever qualification
or import alias it is written behind (`TE.decodeUtf8`,
`Data.Text.Encoding.decodeUtf8`, a bare `decodeUtf8` from an
unqualified import, or any other alias). The rule is the LEXEME rather
than a resolved import, and that is the point: a guard that modelled
import forms could be walked past by an import form it failed to model,
whereas every spelling of the call site necessarily contains this one
maximal identifier. Maximal-lexeme matching is also what makes the
three compliant siblings safe by construction -- `decodeUtf8Lenient` is
a DIFFERENT identifier, never a `decodeUtf8` followed by something, so
they cannot be flagged even by accident.

The cost of the lexeme rule is that a hypothetical unrelated binding
named `decodeUtf8` -- a local shadow, or the equally strict
`Data.Text.Lazy.Encoding.decodeUtf8` -- is reported too. Both are
things this convention wants reported rather than silently allowed, and
`EXEMPTIONS` below is the escape hatch if a genuine one ever appears.
A clean tree needs none, and the checked-in table is empty.

Comment and string-literal awareness comes from
`tools/unicode_operator_audit.py`'s lexer via its public
`haskell_code_spans`, rather than a second copy free to drift from it.

Usage:
  python3 tools/lua_strict_decode_audit.py              # audit the tree
  python3 tools/lua_strict_decode_audit.py --self-test  # fixtures only
Exit codes: 0 = clean, 1 = a strict decode appeared under the scoped
tree (or, under --self-test, a fixture behaved wrongly).
"""
from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from unicode_operator_audit import haskell_code_spans  # type: ignore  # noqa: E402

# The tree #665 scoped the convention to. Widening it beyond the Lua
# API is a separate decision (#1605 "Out of scope"): the rest of `src/`
# and `app/` happens to be clean today, but nothing there is handed raw
# Lua bytes.
SCOPED_TREE = "src/Engine/Scripting/Lua"

# The one banned identifier. Its total siblings are listed for the
# failure report only -- they are never candidates, because a maximal
# identifier lexeme is compared whole.
BANNED_IDENTIFIER = "decodeUtf8"
TOTAL_SIBLINGS = ("decodeUtf8'", "decodeUtf8With", "decodeUtf8Lenient")

# Exemptions, if one is ever needed: repo-relative path -> the reason it
# is exempt, stated inline the way tools/unicode_operator_audit.py
# states its own. Whole-file, because there is no construct-scoped case
# to carve out here. A clean tree needs none.
EXEMPTIONS: dict[str, str] = {}

# Haskell 2010 report SS2.4: the characters a variable or constructor
# identifier is made of. A MAXIMAL run of these is one lexeme, which is
# what keeps `decodeUtf8Lenient` and `decodeUtf8'` from ever matching
# `decodeUtf8` -- and what keeps a longer unrelated identifier that
# merely ENDS in it (`myDecodeUtf8`) from matching either.
_IDENT_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'"


@dataclass(frozen=True)
class Violation:
    path: str
    line: int
    spelling: str

    def __str__(self) -> str:
        return (f"{self.path}:{self.line}: strict decode "
                f"`{self.spelling}` -- Lua strings are arbitrary bytes; "
                f"use decodeUtf8Lenient (or another total sibling)")


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
    """The `Mod.` / `Data.Text.Encoding.` qualifier written immediately
    before `pos`, or `""`. Reported in the failure message only, so the
    developer sees the spelling they actually wrote; detection never
    depends on it."""
    start = pos
    while start > 0 and (text[start - 1] in _IDENT_CHARS or text[start - 1] == "."):
        start -= 1
    candidate = text[start:pos]
    if not candidate.endswith("."):
        return ""
    segments = candidate[:-1].split(".")
    if all(seg and seg[0].isupper() for seg in segments):
        return candidate
    return ""


def _line_of(text: str, pos: int) -> int:
    return text.count("\n", 0, pos) + 1


def find_violations(text: str, rel_path: str) -> list[Violation]:
    """Every strict-decode occurrence in `text` (the source of the file
    at repo-relative `rel_path`), outside comments and string literals
    and outside `EXEMPTIONS`."""
    if rel_path in EXEMPTIONS:
        return []
    violations: list[Violation] = []
    for start, end in haskell_code_spans(text):
        for pos, lexeme in _identifier_runs(text, start, end):
            if lexeme != BANNED_IDENTIFIER:
                continue
            spelling = _qualifier_before(text, pos) + lexeme
            violations.append(Violation(rel_path, _line_of(text, pos), spelling))
    return violations


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
# checked through the same `find_violations` the audit runs, so the
# self-test proves the DETECTOR rather than restating today's tree.
# `expected` is the list of lines that must be reported, in order.
FIXTURES: list[tuple[str, str, list[int]]] = [
    (
        "the repository-standard `TE.` spelling -- the exact shape "
        "commit 893cbd8f reintroduced",
        "module M where\n"
        "import qualified Data.Text.Encoding as TE\n"
        "f raw = TE.decodeUtf8 raw\n",
        [3],
    ),
    (
        "an alternate qualified alias -- an alias rename must not "
        "walk past the guard",
        "module M where\n"
        "import qualified Data.Text.Encoding as Enc\n"
        "f raw = Enc.decodeUtf8 raw\n",
        [3],
    ),
    (
        "the fully qualified spelling with no alias at all",
        "module M where\n"
        "import qualified Data.Text.Encoding\n"
        "f raw = Data.Text.Encoding.decodeUtf8 raw\n",
        [3],
    ),
    (
        "an unqualified import, so the call site is a bare identifier",
        "module M where\n"
        "import Data.Text.Encoding (decodeUtf8)\n"
        "f raw = decodeUtf8 raw\n",
        [2, 3],
    ),
    (
        "the total sibling `decodeUtf8Lenient` -- the compliant fix",
        "module M where\n"
        "import qualified Data.Text.Encoding as TE\n"
        "f raw = TE.decodeUtf8Lenient raw\n",
        [],
    ),
    (
        "the total sibling `decodeUtf8'`",
        "module M where\n"
        "import qualified Data.Text.Encoding as TE\n"
        "f raw = either (const \"\") id (TE.decodeUtf8' raw)\n",
        [],
    ),
    (
        "the total sibling `decodeUtf8With`",
        "module M where\n"
        "import qualified Data.Text.Encoding as TE\n"
        "import qualified Data.Text.Encoding.Error as TEE\n"
        "f raw = TE.decodeUtf8With TEE.lenientDecode raw\n",
        [],
    ),
    (
        "a `--` line comment naming the banned call",
        "module M where\n"
        "-- Was TE.decodeUtf8 raw before #1605.\n"
        "f raw = TE.decodeUtf8Lenient raw\n",
        [],
    ),
    (
        "a `{- -}` block comment naming the banned call, nested",
        "module M where\n"
        "{- do not use TE.decodeUtf8 {- not even here -} ever -}\n"
        "f raw = TE.decodeUtf8Lenient raw\n",
        [],
    ),
    (
        "a string literal containing the banned call",
        "module M where\n"
        "warning = \"replaced TE.decodeUtf8 with the lenient sibling\"\n"
        "f raw = TE.decodeUtf8Lenient raw\n",
        [],
    ),
    (
        "a longer identifier merely ENDING in the banned one",
        "module M where\n"
        "f raw = myDecodeUtf8 raw\n",
        [],
    ),
    (
        "a real strict call on a line that also carries a comment "
        "mentioning it -- the comment is skipped, the code is not",
        "module M where\n"
        "f raw = TE.decodeUtf8 raw  -- TE.decodeUtf8 again, in prose\n",
        [2],
    ),
    (
        "a char literal `'\"'` before a strict call -- the lexer must "
        "not read it as opening a string and swallow the call",
        "module M where\n"
        "f raw = (q, TE.decodeUtf8 raw) where q = '\"'\n",
        [2],
    ),
]


def _exemption_reasons_present() -> list[str]:
    """Requirement 6: every exemption states its reason inline."""
    return [path for path, reason in EXEMPTIONS.items()
            if not isinstance(reason, str) or not reason.strip()]


def self_test() -> int:
    failures: list[str] = []
    for label, source, expected in FIXTURES:
        got = [v.line for v in find_violations(source, "fixture.hs")]
        if got != expected:
            failures.append(
                f"  {label}\n"
                f"    expected violations on lines {expected}, got {got}")
    for path in _exemption_reasons_present():
        failures.append(f"  EXEMPTIONS['{path}'] carries no inline reason")
    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for f in failures:
            print(f)
        return 1
    print(f"lua_strict_decode_audit self-test: {len(FIXTURES)} fixtures OK "
          f"(detection through every qualification form; "
          f"{', '.join(TOTAL_SIBLINGS)}, comments and string literals "
          f"all clean).")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--self-test", action="store_true",
                        help="run the fixture self-test instead of the tree "
                             "audit; touches no shipped source")
    args = parser.parse_args()
    if args.self_test:
        return self_test()

    violations = scan_tree(REPO_ROOT)
    if violations:
        print(f"{len(violations)} strict `{BANNED_IDENTIFIER}` call(s) under "
              f"{SCOPED_TREE}/:")
        for v in violations:
            print(f"  {v}")
        print(f"\nLua strings are arbitrary byte arrays (#665, #618): a "
              f"strict decode throws UnicodeException on malformed input "
              f"instead of letting the call site refuse the value. Use a "
              f"total sibling -- {', '.join(TOTAL_SIBLINGS)}.")
        if EXEMPTIONS:
            print("\nExempt by design:")
            for path, reason in EXEMPTIONS.items():
                print(f"  {path}: {reason}")
        return 1
    print(f"No strict `{BANNED_IDENTIFIER}` found under {SCOPED_TREE}/.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
