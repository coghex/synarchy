#!/usr/bin/env python3
"""Unit tests for unicode_operator_audit.py (issue #1005 acceptance: the
guard detects an intentionally planted forbidden-operator regression
using synthetic fixtures, never by editing real tracked sources).

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
    GLSL_QUASIQUOTE_FILE, EQ_INSTANCE_FILE, MONAD_INSTANCE_FILE,
    WHOLE_FILE_EXEMPT,
)

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


ORDINARY_FILE = "src/Some/Ordinary/Module.hs"


def _tokens(violations) -> set[str]:
    return {v.token for v in violations}


# ----- Each forbidden operator is caught as real code -------------------

def test_each_forbidden_operator_detected_as_real_code():
    for tok in FORBIDDEN_TOKENS:
        text = f"go x y = x {tok} y\n"
        v = find_violations(text, ORDINARY_FILE)
        expect(len(v) == 1 and v[0].token == tok,
               f"'{tok}' used as real code is flagged exactly once "
               f"(got {v})")
        expect(v == [] or v[0].line == 1,
               f"'{tok}' violation reports the correct line number")


def test_replacement_table_is_self_consistent():
    for tok in FORBIDDEN_TOKENS:
        expect(tok in TOKEN_REPLACEMENTS,
               f"'{tok}' has a Unicode replacement recorded")


# ----- Comments and strings never trigger ------------------------------

def test_line_comment_does_not_trigger():
    for tok in FORBIDDEN_TOKENS:
        text = f"-- mentions {tok} in prose, not code\ngo x y = x ∧ y\n"
        v = find_violations(text, ORDINARY_FILE)
        expect(v == [], f"'{tok}' inside a line comment is not flagged")


def test_block_comment_does_not_trigger():
    for tok in FORBIDDEN_TOKENS:
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
    for tok in FORBIDDEN_TOKENS:
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


# ----- Whole-file exemption (UPrelude.hs) -------------------------------

def test_uprelude_whole_file_is_exempt():
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


def main() -> int:
    for fn in [
        test_each_forbidden_operator_detected_as_real_code,
        test_replacement_table_is_self_consistent,
        test_line_comment_does_not_trigger,
        test_block_comment_does_not_trigger,
        test_nested_block_comment_does_not_trigger_but_real_code_after_does,
        test_string_literal_does_not_trigger,
        test_escaped_quote_inside_string_does_not_end_it_early,
        test_longer_symbol_run_is_not_a_false_positive,
        test_adjacent_operators_without_whitespace_still_detected,
        test_uprelude_whole_file_is_exempt,
        test_glsl_quasiquote_is_exempt_but_surrounding_haskell_is_not,
        test_eq_instance_method_is_exempt_but_other_eq_uses_are_not,
        test_eq_lookalike_instance_is_not_exempt,
        test_monad_bind_method_is_exempt_but_other_binds_are_not,
        test_monad_lookalike_instance_is_not_exempt,
        test_glsl_marker_text_inside_comments_does_not_manufacture_a_span,
        test_eq_instance_text_inside_a_comment_does_not_manufacture_an_exemption,
        test_construct_exemptions_do_not_leak_to_other_files,
    ]:
        fn()
    if FAILURES:
        print(f"\n{len(FAILURES)} failure(s)")
        return 1
    print("\nall tests passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
