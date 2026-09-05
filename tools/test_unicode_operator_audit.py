#!/usr/bin/env python3
"""Unit tests for unicode_operator_audit.py (issue #1005 acceptance: the
guard detects an intentionally planted forbidden-operator regression
using synthetic fixtures, never by editing real tracked sources;
extended by issue #1494 to cover the noncanonical `≠` spelling of
inequality, which is detected by a separate single-code-point path).

Mirrors tools/test_engine_env_capability_audit.py's approach: feed the
audit's pure `find_violations` synthetic Haskell text, so these tests
stay stable regardless of how the real tree grows.

Usage:
  python3 tools/test_unicode_operator_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from unicode_operator_audit import (  # type: ignore
    find_violations, FORBIDDEN_TOKENS, TOKEN_REPLACEMENTS,
    NONCANONICAL_TOKENS, ALL_REPLACEMENTS,
    GLSL_QUASIQUOTE_FILE, EQ_INSTANCE_FILE, MONAD_INSTANCE_FILE,
    WHOLE_FILE_EXEMPT,
)

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

# The ASCII tokens and the noncanonical Unicode ones are found by two
# different code paths, so the shared comment/string/detection fixtures
# below run over BOTH -- a `≠` that only the dedicated tests exercised
# would leave the generic exclusions unproven for it.
ALL_TOKENS = FORBIDDEN_TOKENS | NONCANONICAL_TOKENS


ORDINARY_FILE = "src/Some/Ordinary/Module.hs"


def _tokens(violations) -> set[str]:
    return {v.token for v in violations}


# ----- Each forbidden operator is caught as real code -------------------

def test_each_forbidden_operator_detected_as_real_code():
    for tok in ALL_TOKENS:
        text = f"go x y = x {tok} y\n"
        v = find_violations(text, ORDINARY_FILE)
        expect(len(v) == 1 and v[0].token == tok,
               f"'{tok}' used as real code is flagged exactly once "
               f"(got {v})")
        expect(v == [] or v[0].line == 1,
               f"'{tok}' violation reports the correct line number")


def test_replacement_table_is_self_consistent():
    for tok in ALL_TOKENS:
        expect(tok in ALL_REPLACEMENTS,
               f"'{tok}' has a canonical replacement recorded")
    expect(FORBIDDEN_TOKENS.isdisjoint(NONCANONICAL_TOKENS),
           "the ASCII and noncanonical token sets do not overlap")
    expect(all(tok in TOKEN_REPLACEMENTS for tok in FORBIDDEN_TOKENS),
           "every ASCII token keeps its entry in TOKEN_REPLACEMENTS")


# ----- Comments and strings never trigger ------------------------------

def test_line_comment_does_not_trigger():
    for tok in ALL_TOKENS:
        text = f"-- mentions {tok} in prose, not code\ngo x y = x ∧ y\n"
        v = find_violations(text, ORDINARY_FILE)
        expect(v == [], f"'{tok}' inside a line comment is not flagged")


def test_block_comment_does_not_trigger():
    for tok in ALL_TOKENS:
        text = f"{{- a block comment mentioning {tok} -}}\ngo x y = x ∧ y\n"
        v = find_violations(text, ORDINARY_FILE)
        expect(v == [], f"'{tok}' inside a block comment is not flagged")


def test_nested_block_comment_does_not_trigger_but_real_code_after_does():
    text = (
        "{- outer {- inner >>= == -} still a comment /= .&. .|. -}\n"
        "go x y = x == y\n"
    )
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {"=="},
           f"only the real `==` after a nested block comment is flagged "
           f"(got {[str(x) for x in v]})")
    expect(v[0].line == 2, "the flagged occurrence reports line 2, not line 1")


def test_string_literal_does_not_trigger():
    for tok in ALL_TOKENS:
        text = f'msg = "please use {tok} carefully"\ngo x y = x ∧ y\n'
        v = find_violations(text, ORDINARY_FILE)
        expect(v == [], f"'{tok}' inside a string literal is not flagged")


def test_escaped_quote_inside_string_does_not_end_it_early():
    # If the escaped quote were mistaken for the closing quote, the
    # real `>>=` below would appear to be back in a string and be
    # missed -- so a hit here proves the escape is handled, not just
    # that nothing spurious fired.
    text = 'msg = "she said \\"hello\\" then == this"\ngo x y = x >>= y\n'
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {">>="}, f"escaped quotes stay inside the string "
           f"literal, so only the real code `>>=` is flagged (got {v})")


def test_dash_run_continuing_into_a_symbol_is_an_operator():
    # Report SS2.3: a dash run opens a comment only when the maximal
    # symbol lexeme it belongs to is nothing but dashes. `-->` and
    # `--|` continue into another symbol character, so the code after
    # them is code -- read as comments they mask every operator to end
    # of line (#2177).
    for source in ('go x y = x --> x >>= y\n',
                   'go x y = x --| x >>= y\n'):
        v = find_violations(source, ORDINARY_FILE)
        expect(_tokens(v) == {">>="}, f"a dash run continuing into a symbol "
               f"is an operator, so {source.strip()!r} still has real code "
               f"after it (got {v})")


def test_dash_run_beginning_at_a_symbol_is_an_operator():
    # The leading half of the same rule: `<--` began at `<`, so the run
    # is `<--` and not a comment. A trailing-side-only check misses it.
    text = 'go x y = x <-- x >>= y\n'
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {">>="}, f"`<--` is one operator lexeme, so the "
           f"code after it is scanned (got {v})")


def test_dash_run_and_a_unicode_symbol_are_one_lexeme():
    # Report SS2.2's symbol set is Unicode: `⊚--` is one operator, and
    # `--\u2014` continues into an em dash (category Pd). This tree writes
    # its own operators from that set, so an ASCII-only test splits them
    # and hands the `--` on as a comment opener.
    for source in ('go x y = x \u229a-- x >>= y\n',
                   'go x y = x --\u2014 x >>= y\n'):
        v = find_violations(source, ORDINARY_FILE)
        expect(_tokens(v) == {">>="}, f"a Unicode symbol continues the "
               f"lexeme, so {source.strip()!r} still has real code after it "
               f"(got {v})")


def test_a_run_of_only_dashes_is_still_a_comment():
    # The other direction, so the rule is not simply switched off: `--`
    # and `---` are nothing but dashes and open comments, and `-- |`
    # ends its run at the space.
    for source in ('-- x >>= y\n', '--- x >>= y\n', '-- | x >>= y\n'):
        v = find_violations(source + 'go a b = a == b\n', ORDINARY_FILE)
        expect(_tokens(v) == {"=="}, f"{source.strip()!r} is a comment, so "
               f"only the real code below it is flagged (got {v})")


def test_a_lone_dash_is_subtraction_not_a_comment():
    # Two dashes minimum: a single `-` is an operator, and reading it as
    # a comment would mask the rest of its line.
    text = 'go a b = (a - b) >>= y\n'
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {">>="}, f"a lone `-` is subtraction, so the code "
           f"after it is scanned (got {v})")


def test_string_gap_ends_at_its_own_backslash_not_the_closing_quote():
    # Report SS2.6: `\ whitechar {whitechar} \` is a string GAP, not an
    # escape. Read as a two-character escape, the gap's closing
    # backslash pairs with the string's closing quote, the string never
    # ends, and every operator to end of file is masked. `src/`+`app/`
    # carries 258 gaps today.
    text = 'msg = "a\\\n\\"\ngo x y = x >>= y\n'
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {">>="}, f"a string gap ends the string at its "
           f"own closing quote, so the real code after it is still "
           f"scanned (got {v})")


def test_multi_line_string_gap_keeps_its_body_out_of_the_scan():
    # The shape this tree actually writes: a message split across lines
    # with a gap, whose text contains an operator that is NOT code.
    text = ('msg = "first part == not code \\\n'
            '        \\ second part"\n'
            'go x y = x >>= y\n')
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {">>="}, f"the gapped string's body stays a "
           f"literal and the code after it is scanned (got {v})")


# ----- Char literals -----------------------------------------------------

def test_char_literal_containing_a_double_quote_does_not_mask_later_code():
    # A real occurrence (Engine/Scripting/Lua/API/Shell.hs): `'"'` is a
    # valid char literal for the double-quote character. If its `"`
    # were mistaken for a string-literal opener, the scanner would
    # stay "in a string" past it, and the real `==` below would go
    # undetected until some LATER unrelated `"` happened to close it.
    text = "quote = '\"'\nequal x y = x == y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {"=="} and len(v) == 1,
           f"a real '==' after a `'\"'` char literal is still flagged "
           f"(got {[str(x) for x in v]})")
    expect(v[0].line == 2, "the flagged occurrence reports line 2")


def test_escaped_single_quote_char_literal_does_not_confuse_the_scanner():
    text = "tick = '\\''\ngo x y = x == y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {"=="} and len(v) == 1,
           f"a real '==' after an escaped-quote char literal is still "
           f"flagged (got {[str(x) for x in v]})")


def test_combining_mark_is_an_identifier_character():
    # GHC accepts a combining mark inside an identifier (issue #7650),
    # and Python's `\\w` -- `str.isalnum()` plus `_` -- does not match
    # one. Excluded, the prime of `π́'` opens a char literal, eats the
    # quote after it, and the real closing quote opens a phantom string
    # masking every operator below.
    text = ('g = let π́\' x = x in π́\'"\'"\\n'
            'go x y = x >>= y\\n')
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {">>="}, f"a combining mark stays inside the "
           f"identifier, so the code after it is scanned (got {v})")


def test_double_prime_identifier_is_one_name():
    # `'` is itself an identifier-continuation character, so the second
    # prime of `x\'\'` is part of the name. Dropped from the set, it opens
    # a char literal that swallows the string after it and every
    # operator below.
    text = ('g = let x\'\' _ = () in x\'\'"\'"\n'
            'go x y = x >>= y\n')
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {">>="}, f"a doubled prime stays inside the "
           f"identifier, so the code after it is scanned (got {v})")


def test_unicode_identifier_trailing_prime_is_not_a_char_literal():
    # GHC accepts non-ASCII identifiers and this tree is UnicodeSyntax
    # throughout. Read with an ASCII-only identifier class, the prime of
    # `\u03c0'` opens a char literal, consumes the opening quote of the
    # `'"'` after it, and the real closing quote then opens a phantom
    # string that masks every operator to end of file.
    text = ('g = let \u03c0\' _ = () in \u03c0\'"\'"\n'
            'go x y = x >>= y\n')
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {">>="}, f"a Unicode identifier's trailing prime "
           f"is part of the name, so the code after it is still scanned "
           f"(got {v})")


def test_identifier_trailing_prime_is_not_mistaken_for_a_char_literal():
    # Haskell identifiers may end (or contain) `'` -- `x'` is not the
    # start of a char literal, so this must not throw off detection.
    text = "go x' y = x' == y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {"=="} and len(v) == 1,
           f"a trailing prime on an identifier does not mask the real "
           f"'==' that follows (got {[str(x) for x in v]})")


# ----- Exact-token / maximal-munch boundary -----------------------------

def test_longer_symbol_run_is_not_a_false_positive():
    # `..&..` is one lexeme, not `.&.` padded with dots -- must not fire.
    text = "go x y = x ..&.. y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(v == [], f"a longer symbol run containing '.&.' as a substring "
           f"is not flagged (got {v})")


def test_adjacent_operators_without_whitespace_still_detected():
    text = "go x y = (x==y)\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {"=="},
           f"'==' with no surrounding whitespace is still flagged (got {v})")


# ----- Qualified operator forms -------------------------------------------
# Haskell writes a qualified operator with NO space (`B..&.`, `P.>>=`),
# so its lexeme is the bare operator with one extra leading `.` from
# the qualifier separator -- already used legitimately in UPrelude.hs
# itself (`(B..&.)`, `(P.>>=)`). A production file using a qualified
# import must be caught exactly like the bare spelling.

_QUALIFIED_PREFIX = {
    ".&.": "B.", ".|.": "B.", ">>=": "P.", "==": "E.", "/=": "E.",
}


def test_qualified_operator_forms_are_detected():
    for tok, prefix in _QUALIFIED_PREFIX.items():
        text = f"go x y = x {prefix}{tok} y\n"
        v = find_violations(text, ORDINARY_FILE)
        expect(_tokens(v) == {tok} and len(v) == 1,
               f"qualified '{prefix}{tok}' is flagged as '{tok}' "
               f"(got {[str(x) for x in v]})")


def test_multi_segment_qualified_operator_is_detected():
    text = "go x y = x Data.Bits..&. y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {".&."} and len(v) == 1,
           f"a multi-segment qualifier ('Data.Bits..&.') is still "
           f"flagged (got {[str(x) for x in v]})")


def test_dot_prefixed_run_with_no_real_qualifier_is_not_flagged():
    # Same shape as a qualified '.&.' (one extra leading dot) but with
    # nothing that could be a real Haskell module path before it --
    # must not be misread as a qualified use.
    text = "go x y = x ..&. y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(v == [], f"a dot-prefixed run with no real module qualifier "
           f"before it is not flagged (got {v})")


def test_lowercase_prefix_is_not_a_valid_qualifier():
    # Haskell module names always start uppercase -- a lowercase-led
    # identifier immediately before the same dot-prefixed shape is not
    # a real qualifier and must not be flagged.
    text = "go x y = x foo..&. y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(v == [], f"a lowercase-led prefix is not treated as a "
           f"qualifier (got {v})")


def test_unicode_uppercase_qualifier_is_detected():
    # Haskell module names may start with any Unicode uppercase letter,
    # not just ASCII A-Z -- same as this codebase's own Unicode
    # operators are not ASCII-limited.
    text = "f x y = x Δ..&. y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {".&."} and len(v) == 1,
           f"a Unicode-uppercase-led qualifier ('Δ..&.') is flagged "
           f"(got {[str(x) for x in v]})")


def test_unicode_lowercase_prefix_is_not_a_valid_qualifier():
    text = "f x y = x δ..&. y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(v == [], f"a Unicode-lowercase-led prefix is not treated as "
           f"a qualifier (got {v})")


# ----- Whole-file exemption (UPrelude.hs) -------------------------------

def test_uprelude_whole_file_is_exempt():
    # Scoped to the ASCII tokens: the noncanonical half of the same
    # exemption is pinned by
    # `test_noncanonical_inequality_is_not_exempt_in_uprelude` below.
    upl = next(iter(WHOLE_FILE_EXEMPT))
    text = "\n".join(f"go x y = x {tok} y" for tok in FORBIDDEN_TOKENS) + "\n"
    v = find_violations(text, upl)
    expect(v == [], f"every forbidden operator used as real code in "
           f"{upl} is exempt (got {v})")


# ----- Construct-scoped exemptions ---------------------------------------

def test_glsl_quasiquote_is_exempt_but_surrounding_haskell_is_not():
    text = (
        "shaderCode = [frag|\n"
        "    if (a == b) { x = 1; }\n"
        "|]\n"
        "\n"
        "otherCode x y = x == y\n"
    )
    v = find_violations(text, GLSL_QUASIQUOTE_FILE)
    expect(_tokens(v) == {"=="} and len(v) == 1,
           f"GLSL '==' inside [frag|...|] is exempt, but the real "
           f"Haskell '==' elsewhere in the same file still fails "
           f"(got {[str(x) for x in v]})")
    expect(v[0].line == 5,
           "the surviving violation is reported on the Haskell line, "
           "not the GLSL block")


def test_interpolating_glsl_quasiquote_is_exempt():
    # #975: the two bindless fragment shaders are spliced as
    # `$(compileShaderQ ... [glsl|...|])` so they can interpolate the
    # shared Haskell limits -- that form is GLSL source just the same.
    text = (
        'shaderCode = $(compileShaderQ Nothing "frag" Nothing [glsl|\n'
        "    if (a == b) { x = 1; }\n"
        "|])\n"
        "\n"
        "otherCode x y = x == y\n"
    )
    v = find_violations(text, GLSL_QUASIQUOTE_FILE)
    expect(_tokens(v) == {"=="} and len(v) == 1,
           f"GLSL '==' inside [glsl|...|] is exempt, but the real "
           f"Haskell '==' elsewhere in the same file still fails "
           f"(got {[str(x) for x in v]})")
    expect(v[0].line == 5,
           "the surviving violation is reported on the Haskell line, "
           "not the interpolating GLSL block")


def test_eq_instance_method_is_exempt_but_other_eq_uses_are_not():
    text = (
        "instance Eq EngineException where\n"
        "  (==) a b = fieldA a == fieldA b\n"
        "\n"
        "unrelated x y = x == y\n"
    )
    v = find_violations(text, EQ_INSTANCE_FILE)
    # The method HEAD `(==) a b =` is exempt; the `fieldA a == fieldA b`
    # on the SAME line is ordinary code and must still be flagged, same
    # as `unrelated`'s -- proving the exemption is the line only insofar
    # as the audit's own construct-scope rule intends it, not a whole-line
    # amnesty for any other operator use riding along.
    expect(_tokens(v) == {"=="},
           f"the method head is exempt but other '==' uses still fail "
           f"(got {[str(x) for x in v]})")
    expect({x.line for x in v} == {2, 4},
           f"both the same-line non-exempt use and the unrelated use are "
           f"caught (got lines {[x.line for x in v]})")


def test_eq_lookalike_instance_is_not_exempt():
    # Anchored to `instance Eq EngineException` specifically -- a
    # differently-named instance with the identical method-head shape
    # must NOT be exempt, or the guard could be defeated by adding an
    # unrelated Eq instance with a hand-written `(==)`.
    text = "instance Eq SomethingElse where\n  (==) a b = a == b\n"
    v = find_violations(text, EQ_INSTANCE_FILE)
    expect(len(v) == 2 and _tokens(v) == {"=="},
           f"a lookalike 'instance Eq' is fully flagged, not exempt "
           f"(got {[str(x) for x in v]})")


def test_monad_bind_method_is_exempt_but_other_binds_are_not():
    text = (
        "instance Monad (EngineM σ) where\n"
        "  return = pure\n"
        "  mx >>= k = runFoo mx >>= k\n"
        "\n"
        "unrelated x f = x >>= f\n"
    )
    v = find_violations(text, MONAD_INSTANCE_FILE)
    # `return = pure` sits between the header and the method line, as
    # it does in the real Engine.Core.Monad instance -- the exemption
    # must still find the method line past it.
    expect(_tokens(v) == {">>="},
           f"the method head is exempt but other '>>=' uses still fail "
           f"(got {[str(x) for x in v]})")
    expect({x.line for x in v} == {3, 5},
           f"both the same-line non-exempt use and the unrelated use are "
           f"caught (got lines {[x.line for x in v]})")


def test_monad_lookalike_instance_is_not_exempt():
    text = "instance Monad Other where\n  mx >>= k = runOther mx >>= k\n"
    v = find_violations(text, MONAD_INSTANCE_FILE)
    expect(len(v) == 2 and _tokens(v) == {">>="},
           f"a lookalike 'instance Monad' is fully flagged, not exempt "
           f"(got {[str(x) for x in v]})")


def test_glsl_marker_text_inside_comments_does_not_manufacture_a_span():
    # A regression case: `--` comments that merely CONTAIN
    # `[frag|`/`|]`-shaped text must never be mistaken for a real
    # quasiquote's boundaries -- that would mask genuine Haskell code
    # (here, a real `==`) sitting between the two comments.
    text = (
        "-- mentions [frag| in prose, not a real quasiquote\n"
        "otherCode x y = x == y\n"
        "-- also mentions |] in prose\n"
    )
    v = find_violations(text, GLSL_QUASIQUOTE_FILE)
    expect(_tokens(v) == {"=="} and len(v) == 1,
           f"a '==' between two unrelated comments that merely contain "
           f"quasiquote-shaped text is still flagged, not masked "
           f"(got {[str(x) for x in v]})")
    expect(v[0].line == 2, "the flagged occurrence reports the code line")


def test_eq_instance_text_inside_a_comment_does_not_manufacture_an_exemption():
    text = (
        "-- instance Eq EngineException where\n"
        "--   (==) a b = a == b\n"
        "unrelated x y = x == y\n"
    )
    v = find_violations(text, EQ_INSTANCE_FILE)
    expect(_tokens(v) == {"=="} and len(v) == 1,
           f"instance/method text that only appears inside comments "
           f"grants no exemption (got {[str(x) for x in v]})")


def test_construct_exemptions_do_not_leak_to_other_files():
    # The GLSL/Eq/Monad exemptions are keyed to specific files -- the
    # exact same text in an ordinary module must be fully flagged.
    text = "shaderCode = [frag|\n    if (a == b) { x = 1; }\n|]\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {"=="},
           f"a '[frag|...|]'-shaped block in a non-ShaderCode file is "
           f"ordinary code and is flagged (got {v})")


# ----- Noncanonical inequality `≠` (#1494) -------------------------------
# `≠` and `≢` are the same `Data.Eq.Unicode` operator; only `≢` is this
# project's spelling. The ASCII lexer cannot see `≠` at all, so these
# pin the separate detection path rather than re-covering the loops above.

def test_noncanonical_inequality_as_an_operator_is_flagged():
    text = "go x y = x ≠ y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {"≠"} and len(v) == 1,
           f"'≠' used as an operator is flagged exactly once "
           f"(got {[str(x) for x in v]})")
    expect(v != [] and v[0].line == 1,
           "the '≠' violation reports the correct line number")
    expect(v != [] and "≢" in str(v[0]),
           f"the '≠' violation names '≢' as the replacement (got "
           f"{[str(x) for x in v]})")


def test_noncanonical_inequality_in_comment_prose_passes():
    # The whole point of the rule: `≠` stays legal in prose (pseudocode,
    # a maths formula), which is the form the eight retained production
    # occurrences take.
    text = (
        "-- a run is stable while wind ≠ 0, per the formula\n"
        "{- and here too: v ≠ w -}\n"
        "go x y = x ≢ y\n"
    )
    v = find_violations(text, ORDINARY_FILE)
    expect(v == [], f"'≠' in line and block comment prose is not flagged "
           f"(got {[str(x) for x in v]})")


def test_canonical_inequality_is_never_flagged():
    text = "go x y = x ≢ y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(v == [], f"the canonical '≢' is not flagged (got {v})")


def test_noncanonical_char_literal_is_not_an_operator():
    # `_scan_code` keeps a char literal inside its code span, so a raw
    # single-code-point search would read `'≠'` -- a legitimate
    # character value -- as an operator unless its span is excluded.
    # The operator on the next line proves the exclusion is the literal
    # only, not an amnesty for the file.
    text = "notEqualChar = '≠'\ngo x y = x ≠ y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect(_tokens(v) == {"≠"} and len(v) == 1,
           f"a '≠' char literal passes while a real '≠' operator is "
           f"flagged (got {[str(x) for x in v]})")
    expect(v != [] and v[0].line == 2,
           f"the flagged occurrence is the operator on line 2, not the "
           f"char literal on line 1 (got lines "
           f"{[x.line for x in v]})")


def test_noncanonical_inequality_is_not_exempt_in_uprelude():
    # The whole-file exemption exists for the ASCII DEFINITION sites; a
    # `≠` operator there is ordinary drift with no such excuse, and the
    # exemption must not have widened into a blanket amnesty.
    upl = next(iter(WHOLE_FILE_EXEMPT))
    text = "go x y = x ≠ y\n"
    v = find_violations(text, upl)
    expect(_tokens(v) == {"≠"} and len(v) == 1,
           f"'≠' used as an operator in {upl} is still flagged despite "
           f"its whole-file ASCII exemption (got {[str(x) for x in v]})")


def test_noncanonical_inequality_in_glsl_quasiquote_is_exempt():
    # GLSL source is not Haskell -- and the surrounding Haskell in the
    # same file is still held to the rule.
    text = (
        "shaderCode = [frag|\n"
        "    // a ≠ b in a GLSL comment\n"
        "|]\n"
        "\n"
        "otherCode x y = x ≠ y\n"
    )
    v = find_violations(text, GLSL_QUASIQUOTE_FILE)
    expect(_tokens(v) == {"≠"} and len(v) == 1,
           f"'≠' inside [frag|...|] is exempt but the real Haskell '≠' "
           f"elsewhere in the same file still fails "
           f"(got {[str(x) for x in v]})")
    expect(v != [] and v[0].line == 5,
           "the surviving violation is reported on the Haskell line, "
           "not the GLSL block")


def test_ascii_and_noncanonical_violations_report_in_source_order():
    # The two passes run per code span; their hits must interleave into
    # one source-order report rather than one pass's trailing the other's.
    text = "a x y = x ≠ y\nb x y = x == y\nc x y = x ≠ y\n"
    v = find_violations(text, ORDINARY_FILE)
    expect([(x.line, x.token) for x in v] == [(1, "≠"), (2, "=="), (3, "≠")],
           f"violations are reported in source order across both "
           f"detection paths (got {[(x.line, x.token) for x in v]})")


def main() -> int:
    selftestlib.parse_verbose()
    for fn in [
        test_each_forbidden_operator_detected_as_real_code,
        test_replacement_table_is_self_consistent,
        test_line_comment_does_not_trigger,
        test_block_comment_does_not_trigger,
        test_nested_block_comment_does_not_trigger_but_real_code_after_does,
        test_string_literal_does_not_trigger,
        test_escaped_quote_inside_string_does_not_end_it_early,
        test_dash_run_continuing_into_a_symbol_is_an_operator,
        test_dash_run_beginning_at_a_symbol_is_an_operator,
        test_dash_run_and_a_unicode_symbol_are_one_lexeme,
        test_a_run_of_only_dashes_is_still_a_comment,
        test_a_lone_dash_is_subtraction_not_a_comment,
        test_string_gap_ends_at_its_own_backslash_not_the_closing_quote,
        test_multi_line_string_gap_keeps_its_body_out_of_the_scan,
        test_char_literal_containing_a_double_quote_does_not_mask_later_code,
        test_escaped_single_quote_char_literal_does_not_confuse_the_scanner,
        test_identifier_trailing_prime_is_not_mistaken_for_a_char_literal,
        test_unicode_identifier_trailing_prime_is_not_a_char_literal,
        test_double_prime_identifier_is_one_name,
        test_combining_mark_is_an_identifier_character,
        test_longer_symbol_run_is_not_a_false_positive,
        test_adjacent_operators_without_whitespace_still_detected,
        test_qualified_operator_forms_are_detected,
        test_multi_segment_qualified_operator_is_detected,
        test_dot_prefixed_run_with_no_real_qualifier_is_not_flagged,
        test_lowercase_prefix_is_not_a_valid_qualifier,
        test_unicode_uppercase_qualifier_is_detected,
        test_unicode_lowercase_prefix_is_not_a_valid_qualifier,
        test_uprelude_whole_file_is_exempt,
        test_glsl_quasiquote_is_exempt_but_surrounding_haskell_is_not,
        test_interpolating_glsl_quasiquote_is_exempt,
        test_eq_instance_method_is_exempt_but_other_eq_uses_are_not,
        test_eq_lookalike_instance_is_not_exempt,
        test_monad_bind_method_is_exempt_but_other_binds_are_not,
        test_monad_lookalike_instance_is_not_exempt,
        test_glsl_marker_text_inside_comments_does_not_manufacture_a_span,
        test_eq_instance_text_inside_a_comment_does_not_manufacture_an_exemption,
        test_construct_exemptions_do_not_leak_to_other_files,
        test_noncanonical_inequality_as_an_operator_is_flagged,
        test_noncanonical_inequality_in_comment_prose_passes,
        test_canonical_inequality_is_never_flagged,
        test_noncanonical_char_literal_is_not_an_operator,
        test_noncanonical_inequality_is_not_exempt_in_uprelude,
        test_noncanonical_inequality_in_glsl_quasiquote_is_exempt,
        test_ascii_and_noncanonical_violations_report_in_source_order,
    ]:
        fn()
    if FAILURES:
        print(f"\n{len(FAILURES)} failure(s)")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "\nall tests passed")


if __name__ == "__main__":
    sys.exit(main())
