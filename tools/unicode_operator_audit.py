#!/usr/bin/env python3
"""Guard: the five ASCII operators normalised to Unicode by issue #1005
must not reappear in `src/` + `app/` Haskell source, outside a short,
explicit exemption list. Every fixity matches its ASCII counterpart
exactly (src/UPrelude.hs:65-68, base-unicode-symbols Data/Eq/Unicode.hs),
so the substitution below can never silently reassociate an expression:

  .&. -> ⌃   bitwise AND,  infixl 7
  .|. -> ⌄   bitwise OR,   infixl 5
  >>= -> ⌦   monadic bind, infixl 1
  ==  -> ≡   equality,     infix 4
  /=  -> ≢   inequality,   infix 4

Mirrors tools/haskell_module_budget.py's label+check()+main() shape and
tools/engine_env_capability_audit.py's comment/string-aware line scan
(`_strip_haskell_comments`), extended with string-literal and GLSL
quasiquote awareness since a false hit here would rewrite content this
guard must never touch.

Usage:
  python3 tools/unicode_operator_audit.py
Exit codes: 0 = clean tree, 1 = a forbidden operator reappeared outside
the exemption list below.
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

TOKEN_REPLACEMENTS = {
    ".&.": "⌃",
    ".|.": "⌄",
    ">>=": "⌦",
    "==": "≡",
    "/=": "≢",
}
FORBIDDEN_TOKENS = frozenset(TOKEN_REPLACEMENTS)

# Haskell 2010 report §2.4: the ASCII characters usable in a symbolic
# operator. A maximal run of these is one lexeme -- so `.&.` only
# matches a run that is EXACTLY `.&.`, never a substring of some other,
# unrelated compound operator.
_SYMBOL_RUN = re.compile(r"[!#$%&*+./<=>?@\\^|~:-]+")

# A qualified operator (`B..&.`, `P.>>=`, `Data.Bits..&.`) is written
# with NO space between the module path and the operator, so the
# qualifier's separating `.` lands INSIDE the maximal symbol run,
# making a qualified use exactly one extra leading `.` versus the bare
# spelling -- true for all five forbidden operators regardless of how
# many dotted segments the qualifier itself has (each earlier internal
# separator sits between two identifier segments and never touches the
# run). `_QUALIFIER_CHARS`/`_VALID_QUALIFIER` confirm a genuine
# uppercase-led module path sits immediately before the run, so this
# can't misfire on an unrelated same-shaped run with no real qualifier.
_QUALIFIER_CHARS = re.compile(r"[A-Za-z0-9_'.]")
_VALID_QUALIFIER = re.compile(r"^(?:[A-Z][A-Za-z0-9_']*\.)*[A-Z][A-Za-z0-9_']*$")


def _qualifier_before(text: str, pos: int) -> str:
    """The maximal `[A-Za-z0-9_'.]` run immediately before `pos`."""
    start = pos
    while start > 0 and _QUALIFIER_CHARS.match(text[start - 1]):
        start -= 1
    return text[start:pos]


def _matched_token(run: str, text: str, run_start: int) -> str | None:
    """`run` (one maximal symbol-char lexeme) as a forbidden token,
    bare or qualified -- or None if it's neither."""
    if run in FORBIDDEN_TOKENS:
        return run
    if run.startswith(".") and run[1:] in FORBIDDEN_TOKENS:
        if _VALID_QUALIFIER.match(_qualifier_before(text, run_start)):
            return run[1:]
    return None


# Whole-file exemption: the one place the ASCII forms are the
# definitions themselves, not usages.
WHOLE_FILE_EXEMPT = {
    "src/UPrelude.hs":
        "sole definition site for ⌃/⌄/⌦ (and the re-exported >>= that "
        "backs Prelude's own bind) -- contains the ASCII operators by "
        "necessity, not by drift.",
}

# Construct-scoped exemptions: a forbidden operator ELSEWHERE in these
# same files still fails -- only the named construct is exempt.
GLSL_QUASIQUOTE_FILE = "src/Engine/Graphics/Vulkan/ShaderCode.hs"
EQ_INSTANCE_FILE = "src/Engine/Core/Error/Exception.hs"
MONAD_INSTANCE_FILE = "src/Engine/Core/Monad.hs"

CONSTRUCT_EXEMPTIONS = {
    GLSL_QUASIQUOTE_FILE:
        "quasiquoted GLSL source literals (`[vert|...|]` / `[frag|...|]`) "
        "-- GLSL's `==` etc. is not a Haskell operator.",
    EQ_INSTANCE_FILE:
        "`instance Eq EngineException`'s `(==) a b = ...` method "
        "definition -- the method name must stay ASCII `==`.",
    MONAD_INSTANCE_FILE:
        "`instance Monad (EngineM σ)`'s `mx >>= k = ...` method "
        "definition -- the method name must stay ASCII `>>=`.",
}

_GLSL_QUASIQUOTE_SPAN = re.compile(r"\[(?:vert|frag)\|.*?\|\]", re.DOTALL)
# Capture group 1 is the exempt token itself, so the exemption is the
# ONE method-name occurrence, not the rest of its line -- a second
# forbidden operator riding the same line (e.g. inside the method body)
# still fails. Anchored to the EXACT documented instance header, with
# only indented (i.e. still-inside-that-instance-block) lines allowed
# between it and the method line, so a differently-named lookalike
# instance (`instance Eq SomethingElse where`) is never exempt.
_EQ_METHOD_TOKEN = re.compile(
    r"^instance\s+Eq\s+EngineException\s+where[ \t]*\n"
    r"(?:[ \t]+.*\n)*?"
    r"[ \t]*\((==)\)\s",
    re.MULTILINE)
_MONAD_BIND_METHOD_TOKEN = re.compile(
    r"^instance\s+Monad\s+\(EngineM\s+σ\)\s+where[ \t]*\n"
    r"(?:[ \t]+.*\n)*?"
    r"[ \t]*mx\s*(>>=)\s*k\s*=",
    re.MULTILINE)


@dataclass(frozen=True)
class Violation:
    path: str
    line: int
    token: str

    def __str__(self) -> str:
        return (f"{self.path}:{self.line}: forbidden operator "
                f"'{self.token}' (use '{TOKEN_REPLACEMENTS[self.token]}' "
                f"instead)")


def _mask_spans(text: str, spans: list[tuple[int, int]]) -> str:
    """Blank [start,end) spans to a placeholder byte, one-for-one, so
    every other position -- and every line number -- stays valid."""
    out = list(text)
    for start, end in spans:
        for i in range(start, end):
            if out[i] != "\n":
                out[i] = "\x00"
    return "".join(out)


_IDENT_CONTINUE = re.compile(r"[A-Za-z0-9_']")
# A Haskell char literal, escaped or not (`'x'`, `'\n'`, `'\''`, `'\NUL'`,
# `'\65'`, `'\x41'`, ...). Matched WHOLE and skipped atomically so a
# literal double quote inside one -- `'"'`, a real occurrence in this
# tree (Engine/Scripting/Lua/API/Shell.hs) -- can never be mistaken for
# the start of a string literal and swallow everything up to some
# unrelated LATER `"`, along with any real operator in between.
_CHAR_LITERAL = re.compile(r"'(?:\\(?:[A-Za-z0-9^]+|.)|[^'\\\n])'")


def _code_runs(text: str) -> list[tuple[int, int]]:
    """Spans of `text` that are plain Haskell code: outside `--` line
    comments, nestable `{- -}` block comments, `'x'` char literals, and
    `"..."` string literals (backslash-escaped; under-skipping an
    escape's payload never risks the closing quote, since the only
    escape that could fool detection -- `\\"` -- is still consumed
    whole)."""
    i, n = 0, len(text)
    runs: list[tuple[int, int]] = []
    run_start: int | None = None
    state = "CODE"
    depth = 0
    while i < n:
        c = text[i]
        if state == "CODE":
            if text.startswith("{-", i):
                if run_start is not None:
                    runs.append((run_start, i))
                    run_start = None
                state, depth, i = "BLOCK", 1, i + 2
                continue
            if text.startswith("--", i):
                if run_start is not None:
                    runs.append((run_start, i))
                    run_start = None
                state, i = "LINE", i + 2
                continue
            # A `'` is only a CANDIDATE char-literal start when it can't
            # be the trailing prime of an identifier (`x'`, `map''`) --
            # Haskell identifiers may contain `'` anywhere after the
            # first character.
            if c == "'" and (i == 0 or not _IDENT_CONTINUE.match(text[i - 1])):
                m = _CHAR_LITERAL.match(text, i)
                if m:
                    if run_start is None:
                        run_start = i
                    i = m.end()
                    continue
            if c == '"':
                if run_start is not None:
                    runs.append((run_start, i))
                    run_start = None
                state, i = "STRING", i + 1
                continue
            if run_start is None:
                run_start = i
            i += 1
        elif state == "LINE":
            i += 1
            if c == "\n":
                state = "CODE"
        elif state == "BLOCK":
            if text.startswith("{-", i):
                depth += 1
                i += 2
            elif text.startswith("-}", i):
                depth -= 1
                i += 2
                if depth == 0:
                    state = "CODE"
            else:
                i += 1
        else:  # STRING
            if c == "\\":
                i += 2
            elif c == '"':
                state, i = "CODE", i + 1
            else:
                i += 1
    if run_start is not None:
        runs.append((run_start, n))
    return runs


def _line_of(text: str, pos: int) -> int:
    return text.count("\n", 0, pos) + 1


def _code_only(text: str) -> str:
    """`text` with everything OUTSIDE a `_code_runs` span blanked out --
    used to hunt for a construct (like a GLSL quasiquote's opening/
    closing marker) without a comment that merely CONTAINS that
    construct's text being mistaken for the real thing."""
    code_spans = _code_runs(text)
    non_code_spans = []
    prev_end = 0
    for start, end in code_spans:
        if start > prev_end:
            non_code_spans.append((prev_end, start))
        prev_end = end
    if prev_end < len(text):
        non_code_spans.append((prev_end, len(text)))
    return _mask_spans(text, non_code_spans)


def find_violations(text: str, rel_path: str) -> list[Violation]:
    """Every forbidden-operator occurrence in `text` (the source of the
    file at repo-relative `rel_path`) outside a comment, a string
    literal, and this module's explicit exemptions above."""
    if rel_path in WHOLE_FILE_EXEMPT:
        return []

    scan_text = text
    if rel_path == GLSL_QUASIQUOTE_FILE:
        # Locate the quasiquote markers only in genuine code -- a `--`
        # comment that happens to CONTAIN `[frag|`/`|]`-shaped text must
        # never be mistaken for a real quasiquote boundary and mask
        # real Haskell code in between.
        spans = [m.span() for m in _GLSL_QUASIQUOTE_SPAN.finditer(_code_only(text))]
        scan_text = _mask_spans(text, spans)

    # Same rationale as the GLSL case: hunt for the instance header/
    # method-head text only in genuine code, so a comment that merely
    # CONTAINS matching text can never manufacture an exemption.
    exempt_spans: set[tuple[int, int]] = set()
    if rel_path == EQ_INSTANCE_FILE:
        exempt_spans.update(m.span(1) for m in _EQ_METHOD_TOKEN.finditer(_code_only(text)))
    if rel_path == MONAD_INSTANCE_FILE:
        exempt_spans.update(m.span(1) for m in _MONAD_BIND_METHOD_TOKEN.finditer(_code_only(text)))

    violations: list[Violation] = []
    for start, end in _code_runs(scan_text):
        for m in _SYMBOL_RUN.finditer(scan_text, start, end):
            tok = _matched_token(m.group(0), scan_text, m.start())
            if tok is None:
                continue
            if (m.start(), m.end()) in exempt_spans:
                continue
            violations.append(Violation(rel_path, _line_of(text, m.start()), tok))
    return violations


def scan_tree(repo_root: Path) -> list[Violation]:
    files = sorted(repo_root.glob("src/**/*.hs")) + sorted(repo_root.glob("app/**/*.hs"))
    violations: list[Violation] = []
    for path in files:
        rel = path.relative_to(repo_root).as_posix()
        text = path.read_text(encoding="utf-8")
        violations.extend(find_violations(text, rel))
    return violations


def main() -> int:
    violations = scan_tree(REPO_ROOT)
    if violations:
        print(f"{len(violations)} forbidden ASCII operator occurrence(s):")
        for v in violations:
            print(f"  {v}")
        print("\nExempt by design:")
        for path, reason in {**WHOLE_FILE_EXEMPT, **CONSTRUCT_EXEMPTIONS}.items():
            print(f"  {path}: {reason}")
        return 1
    print("No forbidden ASCII operators found outside the exemption list.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
