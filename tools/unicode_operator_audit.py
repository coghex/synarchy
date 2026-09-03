#!/usr/bin/env python3
"""Guard: two spelling rules on `src/` + `app/` Haskell source, both
outside a short, explicit exemption list.

1. (#1005) The five ASCII operators normalised to Unicode must not
   reappear. Every fixity matches its ASCII counterpart exactly
   (src/UPrelude.hs:65-68, base-unicode-symbols Data/Eq/Unicode.hs), so
   the substitution below can never silently reassociate an expression:

     .&. -> ⌃   bitwise AND,  infixl 7
     .|. -> ⌄   bitwise OR,   infixl 5
     >>= -> ⌦   monadic bind, infixl 1
     ==  -> ≡   equality,     infix 4
     /=  -> ≢   inequality,   infix 4

2. (#1494) Inequality is spelled `≢`, never the noncanonical `≠`.
   Both come from the same `Data.Eq.Unicode` re-export and are the same
   `/=` at the same `infix 4`, so this too is a pure respelling. `≠`
   remains legal in comment prose (pseudocode, a maths formula) --
   which the scanner below already excludes -- and is forbidden only as
   an operator.

Rule 2 needs its own detection path: `_SYMBOL_RUN`'s character class is
ASCII by construction, so `≠` is never lexed as a candidate token and a
tree spelled entirely `≠` would audit clean.

Mirrors tools/haskell_module_budget.py's label+check()+main() shape and
tools/engine_env_capability_common.py's comment/string-aware line scan
(`_strip_haskell_comments` there; `_code_runs`/`_code_only` here),
extended with string-literal and GLSL quasiquote awareness since a
false hit here would rewrite content this guard must never touch.

Usage:
  python3 tools/unicode_operator_audit.py
Exit codes: 0 = clean tree, 1 = a forbidden ASCII operator reappeared,
or inequality was spelled `≠`, outside the exemption list below.
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

# #1494: already-Unicode spellings that are nonetheless not this
# project's canonical one. Kept in their OWN table rather than folded
# into TOKEN_REPLACEMENTS because the two are detected differently (see
# `_NONCANONICAL_RUN`) and exempted differently (WHOLE_FILE_EXEMPT
# covers the ASCII definition sites only).
NONCANONICAL_REPLACEMENTS = {
    "≠": "≢",
}
NONCANONICAL_TOKENS = frozenset(NONCANONICAL_REPLACEMENTS)

ALL_REPLACEMENTS = {**TOKEN_REPLACEMENTS, **NONCANONICAL_REPLACEMENTS}

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
# run). `_qualifier_before`/`_is_valid_qualifier` confirm a genuine
# uppercase-led module path sits immediately before the run, so this
# can't misfire on an unrelated same-shaped run with no real qualifier.
# `\w` (Python `re`'s default, Unicode-aware) and `str.isupper()` --
# not an ASCII-only `[A-Z]` -- since Haskell module names may start
# with any Unicode uppercase letter, same as this codebase's own
# Unicode operators are not ASCII-limited.
_QUALIFIER_CHARS = re.compile(r"[\w'.]")

# Every noncanonical token is a SINGLE code point, so it is found by a
# literal search rather than by lexing a maximal symbol run: `≠` cannot
# be a substring of an identifier (Haskell identifiers hold no symbol
# characters), and a qualified use (`E.≠`) or a hypothetical compound
# (`≠?`) contains the noncanonical spelling either way and should be
# reported either way. That makes this path unable to MISS a use, which
# is the failure mode that matters for a guard. What it CAN over-match
# is a char literal `'≠'` -- whose span `_scan_code` reports so
# `find_violations` can exclude it.
_NONCANONICAL_RUN = re.compile("[" + re.escape("".join(sorted(NONCANONICAL_TOKENS))) + "]")


def _qualifier_before(text: str, pos: int) -> str:
    """The maximal `[\\w'.]` run immediately before `pos`."""
    start = pos
    while start > 0 and _QUALIFIER_CHARS.match(text[start - 1]):
        start -= 1
    return text[start:pos]


def _is_valid_qualifier(candidate: str) -> bool:
    """True if `candidate` (e.g. `B`, `Data.Bits`, `Δ`) is a valid
    Haskell qualified-module path: one or more dot-separated segments,
    each a Unicode-uppercase letter followed by letters, digits, `_`,
    or `'`."""
    if not candidate:
        return False
    segments = candidate.split(".")
    return all(
        seg and seg[0].isupper()
        and all(ch.isalnum() or ch in "_'" for ch in seg[1:])
        for seg in segments
    )


def _matched_token(run: str, text: str, run_start: int) -> str | None:
    """`run` (one maximal symbol-char lexeme) as a forbidden token,
    bare or qualified -- or None if it's neither."""
    if run in FORBIDDEN_TOKENS:
        return run
    if run.startswith(".") and run[1:] in FORBIDDEN_TOKENS:
        if _is_valid_qualifier(_qualifier_before(text, run_start)):
            return run[1:]
    return None


# Whole-file exemption, scoped to the ASCII tokens ONLY: the one place
# the ASCII forms are the definitions themselves, not usages. It is
# deliberately NOT a blanket amnesty -- a noncanonical `≠` written as an
# operator here is ordinary drift with no definition-site excuse, and is
# still flagged (#1494).
WHOLE_FILE_EXEMPT = {
    "src/UPrelude.hs":
        "sole definition site for ⌃/⌄/⌦ (and the re-exported >>= that "
        "backs Prelude's own bind) -- contains the ASCII operators by "
        "necessity, not by drift. Exempts the ASCII tokens only; a "
        "noncanonical `≠` operator here still fails.",
}

# Construct-scoped exemptions: a forbidden operator ELSEWHERE in these
# same files still fails -- only the named construct is exempt.
GLSL_QUASIQUOTE_FILE = "src/Engine/Graphics/Vulkan/ShaderCode.hs"
EQ_INSTANCE_FILE = "src/Engine/Core/Error/Exception.hs"
MONAD_INSTANCE_FILE = "src/Engine/Core/Monad.hs"

CONSTRUCT_EXEMPTIONS = {
    GLSL_QUASIQUOTE_FILE:
        "quasiquoted GLSL source literals (`[vert|...|]` / `[frag|...|]` / "
        "the interpolating `[glsl|...|]` spliced by `compileShaderQ`) "
        "-- GLSL's `==` etc. is not a Haskell operator.",
    EQ_INSTANCE_FILE:
        "`instance Eq EngineException`'s `(==) a b = ...` method "
        "definition -- the method name must stay ASCII `==`.",
    MONAD_INSTANCE_FILE:
        "`instance Monad (EngineM σ)`'s `mx >>= k = ...` method "
        "definition -- the method name must stay ASCII `>>=`.",
}

_GLSL_QUASIQUOTE_SPAN = re.compile(r"\[(?:vert|frag|glsl)\|.*?\|\]", re.DOTALL)
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
                f"'{self.token}' (use '{ALL_REPLACEMENTS[self.token]}' "
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


def _scan_code(
    text: str,
) -> tuple[list[tuple[int, int]], list[tuple[int, int]], list[tuple[int, int]]]:
    """`(code spans, char-literal spans, comment spans)`.

    Code spans are the parts of `text` that are plain Haskell code:
    outside `--` line comments, nestable `{- -}` block comments, and
    `"..."` string literals (backslash-escaped; under-skipping an
    escape's payload never risks the closing quote, since the only
    escape that could fool detection -- `\\"` -- is still consumed
    whole).

    A `'x'` char literal is skipped ATOMICALLY but stays INSIDE its code
    span, which is what the ASCII pass wants (a one-character literal
    can never hold a multi-character token like `>>=`, and keeping the
    span intact avoids splitting a run mid-expression). A single
    code-point search cannot rely on that, so the literals' spans are
    reported separately for `find_violations` to exclude -- otherwise
    `'≠'`, a legitimate character value, would read as an operator.

    Comment spans are the `--` line comments and the nestable `{- -}`
    block comments, reported as a THIRD list rather than inferred by
    subtracting the code spans from the file: the complement of the code
    spans also holds string literals, which are not comments (#2292).
    Each span covers the comment's delimiters and body -- for a line
    comment, up to but not including its terminating newline."""
    i, n = 0, len(text)
    runs: list[tuple[int, int]] = []
    char_literals: list[tuple[int, int]] = []
    comments: list[tuple[int, int]] = []
    run_start: int | None = None
    comment_start: int | None = None
    state = "CODE"
    depth = 0
    while i < n:
        c = text[i]
        if state == "CODE":
            if text.startswith("{-", i):
                if run_start is not None:
                    runs.append((run_start, i))
                    run_start = None
                comment_start = i
                state, depth, i = "BLOCK", 1, i + 2
                continue
            if text.startswith("--", i):
                if run_start is not None:
                    runs.append((run_start, i))
                    run_start = None
                comment_start = i
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
                    char_literals.append((i, m.end()))
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
            if c == "\n":
                # The newline terminates the comment but is not part of
                # it, so a caller slicing the span never picks up the
                # line break between one comment and the next.
                comments.append((comment_start, i))
                comment_start = None
                state = "CODE"
            i += 1
        elif state == "BLOCK":
            if text.startswith("{-", i):
                depth += 1
                i += 2
            elif text.startswith("-}", i):
                depth -= 1
                i += 2
                if depth == 0:
                    comments.append((comment_start, i))
                    comment_start = None
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
    # An unterminated comment (a `--` on the last line with no trailing
    # newline, or an unclosed `{-`) still ran to end of file, so it is
    # still comment text.
    if comment_start is not None:
        comments.append((comment_start, n))
    return runs, char_literals, comments


def _code_runs(text: str) -> list[tuple[int, int]]:
    """`_scan_code`'s code spans alone, for callers with no interest in
    where the char literals sit."""
    return _scan_code(text)[0]


def haskell_code_spans(text: str) -> list[tuple[int, int]]:
    """The `[start, end)` spans of `text` that are genuine Haskell code
    -- outside `--` line comments, nestable `{- -}` block comments and
    `"..."` string literals.

    The public name for `_code_runs`, so a sibling guard that needs the
    same comment/string awareness (tools/lua_strict_decode_audit.py)
    reuses this one lexer instead of keeping a second copy free to
    drift from it. `_scan_code`'s subtleties -- nested block comments,
    atomically skipped char literals so a `'"'` cannot open a phantom
    string -- are exactly the ones a copy would get wrong."""
    return _code_runs(text)


def haskell_comment_spans(text: str) -> list[tuple[int, int]]:
    """The `[start, end)` spans of `text` that are Haskell COMMENTS --
    `--` line comments and nestable `{- -}` block comments, delimiters
    included.

    The counterpart of `haskell_code_spans` for a guard that reads
    comment prose rather than code (tools/haddock_link_audit.py). It is
    a separate report rather than the complement of the code spans
    because string literals and quasiquotes are non-code too, and a
    guard that treated them as comments would read a link out of a
    string it is required to ignore (#2292)."""
    return _scan_code(text)[2]


def haskell_code_only(text: str) -> str:
    """`text` with every non-code position blanked, positions and line
    numbers preserved.

    The public name for `_code_only`, for the same reuse reason as
    `haskell_code_spans`: a sibling guard that hunts for a DECLARATION
    (an import, say) must not find one inside a comment that merely
    quotes it."""
    return _code_only(text)


def _within(pos: int, spans: list[tuple[int, int]]) -> bool:
    return any(start <= pos < end for start, end in spans)


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

    code_spans, char_literal_spans, _ = _scan_code(scan_text)
    # The whole-file exemption covers the ASCII definition sites only,
    # so it suppresses that pass rather than the whole scan.
    ascii_exempt = rel_path in WHOLE_FILE_EXEMPT

    hits: list[tuple[int, str]] = []
    for start, end in code_spans:
        if not ascii_exempt:
            for m in _SYMBOL_RUN.finditer(scan_text, start, end):
                tok = _matched_token(m.group(0), scan_text, m.start())
                if tok is None:
                    continue
                if (m.start(), m.end()) in exempt_spans:
                    continue
                hits.append((m.start(), tok))
        for m in _NONCANONICAL_RUN.finditer(scan_text, start, end):
            if _within(m.start(), char_literal_spans):
                continue
            hits.append((m.start(), m.group(0)))
    # Sorted by position so the two passes interleave into one
    # source-order report rather than one pass's hits trailing the
    # other's.
    return [Violation(rel_path, _line_of(text, pos), tok)
            for pos, tok in sorted(hits)]


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
        print(f"{len(violations)} forbidden operator occurrence(s) "
              f"(ASCII spellings and/or noncanonical `≠`):")
        for v in violations:
            print(f"  {v}")
        print("\nExempt by design:")
        for path, reason in {**WHOLE_FILE_EXEMPT, **CONSTRUCT_EXEMPTIONS}.items():
            print(f"  {path}: {reason}")
        return 1
    print("No forbidden ASCII operators and no noncanonical `≠` found "
          "outside the exemption list.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
