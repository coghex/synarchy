#!/usr/bin/env python3
"""Unit tests for check_texture_paths.py (issue #1705).

Every case is built in an isolated temporary tree with its own
`assets/textures/`, so nothing here reads, writes or depends on the
repository's real assets — a checked-in failing fixture would make the
repository-wide check fail by construction. This mirrors the convention
stated in tools/test_findings_report_audit.py and
tools/test_cabal_module_audit.py.

Coverage is one fixture per rule, each accepted by the other rules, so
removing or inverting any single rule fails here:

  requirement 2  a reference inside a comment is not reported missing,
                 for whole-line, trailing and multi-line comments in
                 .hs, .lua and .yaml;
  requirement 3  skipping is per REFERENCE — executable content on a
                 line that also carries a trailing comment is checked;
  requirement 4  a comment introducer inside a string literal starts no
                 comment and hides no later reference on that line;
  requirement 5  a genuinely missing reference still fails non-zero and
                 names its file:line, for all three reference shapes
                 (file literal, directory base, concatenation prefix);
  requirement 6  an input the lexer cannot resolve is a located error
                 (exit 2), never a quietly smaller scan.

The malformed-input cases assert the diagnostic AND its location, and
assert that nothing was reported as missing, so they cannot pass merely
because a missing-path check happened to fail too.

The real repository is exercised at the end, so a lexer regression that
turns the blocking gate red fails here rather than after a push.

Usage:
  python3 tools/test_check_texture_paths.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import check_texture_paths as ctp  # type: ignore

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402

REPO_ROOT = Path(__file__).resolve().parent.parent
TEMP_TREES: list[Path] = []

#: Present in every fixture tree, so a case can carry a reference that
#: resolves without depending on the repository's own assets.
BASE_ASSETS = ("assets/textures/real/ok.png", "assets/textures/dir/")


def build_tree(files: dict[str, str | bytes],
               assets: tuple[str, ...] = BASE_ASSETS) -> Path:
    root = Path(tempfile.mkdtemp(prefix="check_texture_paths_"))
    TEMP_TREES.append(root)
    for rel in assets:
        target = root / rel
        if rel.endswith("/"):
            target.mkdir(parents=True, exist_ok=True)
        else:
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(b"")
    for rel, content in files.items():
        target = root / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        if isinstance(content, bytes):
            target.write_bytes(content)
        else:
            target.write_text(content, encoding="utf-8")
    return root


def run(root: Path) -> tuple[int, str, str]:
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        code = ctp.main(["--root", str(root)])
    return code, out.getvalue(), err.getvalue()


def check(files: dict[str, str | bytes],
          assets: tuple[str, ...] = BASE_ASSETS) -> tuple[int, str, str]:
    return run(build_tree(files, assets))


def skipped_count(stdout: str) -> int:
    marker = "skipped "
    start = stdout.index(marker) + len(marker)
    return int(stdout[start:stdout.index(" ", start)])


def scanned_count(stdout: str) -> int:
    marker = "scanned "
    start = stdout.index(marker) + len(marker)
    return int(stdout[start:stdout.index(" ", start)])


# ---------------------------------------------------------------------------
# Requirement 2 — a reference occurring only inside a comment is not missing
# ---------------------------------------------------------------------------

def test_comment_references_are_skipped() -> None:
    print("\n[req 2] references confined to comments are not reported missing")
    cases = {
        "a Haskell line comment": {
            "src/A.hs": '-- see assets/textures/nope/hs_line.png\n'
                        'ok = "assets/textures/real/ok.png"\n',
        },
        "a Haskell block comment": {
            "src/B.hs": '{- see assets/textures/nope/hs_block.png -}\n'
                        'ok = "assets/textures/real/ok.png"\n',
        },
        "a nested Haskell block comment": {
            "src/C.hs": '{- outer {- assets/textures/nope/hs_nest.png -} -}\n'
                        'ok = "assets/textures/real/ok.png"\n',
        },
        "a Lua line comment": {
            "scripts/a.lua": '-- see assets/textures/nope/lua_line.png\n'
                             'local ok = "assets/textures/real/ok.png"\n',
        },
        "a Lua long comment": {
            "scripts/b.lua": '--[[ see assets/textures/nope/lua_long.png ]]\n'
                             'local ok = "assets/textures/real/ok.png"\n',
        },
        "a levelled Lua long comment": {
            "scripts/c.lua": '--[==[ assets/textures/nope/lua_lvl.png ]==]\n'
                             'local ok = "assets/textures/real/ok.png"\n',
        },
        "a YAML comment": {
            "data/a.yaml": '# see assets/textures/nope/yaml_line.png\n'
                           'icon: assets/textures/real/ok.png\n',
        },
        "a trailing YAML comment": {
            "data/b.yaml": 'icon: assets/textures/real/ok.png  '
                           '# was assets/textures/nope/yaml_trail.png\n',
        },
    }
    for label, files in cases.items():
        code, out, err = check(files)
        expect(code == ctp.EXIT_OK,
               f"{label}: exits 0 (got {code}: {out.strip()} {err.strip()})")
        expect("MISSING" not in out, f"{label}: reports nothing missing")
        expect(skipped_count(out) == 1,
               f"{label}: reports exactly one comment skip "
               f"(got {out.splitlines()[0]})")
        expect(scanned_count(out) == 1,
               f"{label}: the executable reference is still scanned")


def test_multiline_comment_state_and_code_resumption() -> None:
    print("\n[req 2] multi-line comment state, and code resuming after it")
    cases = {
        "Haskell {- -}": ("src/M.hs", [
            'before = "assets/textures/nope/before.png"',
            '{-',
            '  assets/textures/nope/inside.png',
            '-}',
            'after = "assets/textures/nope/after.png"',
        ]),
        "Lua --[[ ]]": ("scripts/m.lua", [
            'local before = "assets/textures/nope/before.png"',
            '--[[',
            '  assets/textures/nope/inside.png',
            ']]',
            'local after = "assets/textures/nope/after.png"',
        ]),
    }
    for label, (path, lines) in cases.items():
        code, out, err = check({path: "\n".join(lines) + "\n"})
        expect(code == ctp.EXIT_MISSING,
               f"{label}: exits 1 (got {code}: {err.strip()})")
        expect("assets/textures/nope/before.png" in out,
               f"{label}: the reference before the opener is still checked")
        expect("assets/textures/nope/after.png" in out,
               f"{label}: the reference after the closer is still checked")
        expect("assets/textures/nope/inside.png" not in out,
               f"{label}: the reference inside the block is skipped")
        expect(skipped_count(out) == 1,
               f"{label}: exactly one skip is reported")


# ---------------------------------------------------------------------------
# Requirement 3 — skipping is per reference, not per line
# ---------------------------------------------------------------------------

def test_skipping_is_per_reference() -> None:
    print("\n[req 3] executable content on a line with a trailing comment "
          "is still checked")
    cases = {
        "Haskell": ("src/T.hs",
                    'icon = "assets/textures/nope/code.png"'
                    '  -- was assets/textures/nope/prose.png\n'),
        "Lua": ("scripts/t.lua",
                'local icon = "assets/textures/nope/code.png"'
                '  -- was assets/textures/nope/prose.png\n'),
        "YAML": ("data/t.yaml",
                 'icon: assets/textures/nope/code.png'
                 '  # was assets/textures/nope/prose.png\n'),
    }
    for label, (path, text) in cases.items():
        code, out, err = check({path: text})
        expect(code == ctp.EXIT_MISSING,
               f"{label}: exits 1 (got {code}: {err.strip()})")
        expect("assets/textures/nope/code.png" in out,
               f"{label}: the executable reference is reported missing")
        expect(f"{path}:1" in out,
               f"{label}: the report names {path}:1")
        expect("assets/textures/nope/prose.png" not in out,
               f"{label}: the trailing-comment reference is not reported")
        expect(skipped_count(out) == 1 and scanned_count(out) == 1,
               f"{label}: one reference scanned, one skipped "
               f"(got {out.splitlines()[0]})")


# ---------------------------------------------------------------------------
# Requirement 4 — a comment introducer inside a string starts no comment
# ---------------------------------------------------------------------------

def test_comment_introducer_inside_a_string_literal() -> None:
    print("\n[req 4] a comment introducer inside a string hides nothing")
    cases = {
        "Haskell --": ("src/S.hs",
                       'p = "a -- b" <> "assets/textures/nope/hs_str.png"\n'),
        "Haskell {-": ("src/S2.hs",
                       'p = "a {- b" <> "assets/textures/nope/hs_blk.png"\n'),
        "Lua --": ("scripts/s.lua",
                   'local a = "-- b"; load("assets/textures/nope/lua_str.png")\n'),
        "Lua --[[": ("scripts/s2.lua",
                     'local a = "--[[ b"; load("assets/textures/nope/lua_lng.png")\n'),
        "YAML #": ("data/s.yaml",
                   'list: ["a # b", "assets/textures/nope/yaml_str.png"]\n'),
    }
    for label, (path, text) in cases.items():
        code, out, err = check({path: text})
        expect(code == ctp.EXIT_MISSING,
               f"{label}: exits 1 (got {code}: {out.strip()} {err.strip()})")
        expect(f"{path}:1" in out,
               f"{label}: the reference after the introducer is reported "
               f"at {path}:1")
        expect(skipped_count(out) == 0,
               f"{label}: nothing was skipped as a comment "
               f"(got {out.splitlines()[0]})")


def test_haskell_operator_and_identifier_tick_are_not_comments() -> None:
    print("\n[req 4] `-->`/`<--` are operators and `foo'` is an identifier")
    code, out, err = check({
        "src/O.hs": 'f = a --> "assets/textures/nope/operator.png"\n'
                    'g = a <-- "assets/textures/nope/leftop.png"\n'
                    'foo\' = "assets/textures/nope/tick.png"\n',
    })
    expect(code == ctp.EXIT_MISSING,
           f"exits 1 (got {code}: {out.strip()} {err.strip()})")
    expect("assets/textures/nope/operator.png" in out,
           "a dash run continuing into a symbol is an operator, not a comment")
    expect("assets/textures/nope/leftop.png" in out,
           "a dash run preceded by a symbol is an operator, not a comment")
    expect("assets/textures/nope/tick.png" in out,
           "a prime-suffixed identifier does not open a character literal")
    expect(skipped_count(out) == 0, "nothing was skipped as a comment")


def test_yaml_hash_inside_a_plain_scalar_is_not_a_comment() -> None:
    print("\n[req 4] a `#` not preceded by whitespace is a plain-scalar "
          "character")
    code, out, err = check({
        "data/h.yaml": 'list: [red#tint, assets/textures/nope/hash.png]\n',
    })
    expect(code == ctp.EXIT_MISSING,
           f"exits 1 (got {code}: {out.strip()} {err.strip()})")
    expect("data/h.yaml:1" in out,
           "the reference after the plain-scalar `#` is still checked")
    expect(skipped_count(out) == 0, "nothing was skipped as a comment")


def test_yaml_apostrophe_in_a_plain_scalar() -> None:
    print("\n[req 4] an apostrophe inside a YAML plain scalar is a character")
    code, out, err = check({
        "data/p.yaml": "note: it isn't a quoted scalar  # assets/textures/nope/p.png\n"
                       "icon: assets/textures/nope/plain.png\n",
    })
    expect(code == ctp.EXIT_MISSING,
           f"exits 1 (got {code}: {out.strip()} {err.strip()})")
    expect("data/p.yaml:2" in out,
           "the following line is still lexed as code")
    expect(skipped_count(out) == 1,
           f"the trailing comment is still recognised "
           f"(got {out.splitlines()[0]})")


def test_legal_multiline_constructs_are_not_errors() -> None:
    """Requirement 6 forbids a quietly smaller scan, not a legal input: the
    multi-line spellings this tree actually uses must lex, not raise."""
    print("\n[req 6] legal multi-line constructs lex instead of erroring")
    cases = {
        "a YAML literal block scalar, whose body is content not comment": (
            "data/blk.yaml",
            'note: |\n'
            '  # assets/textures/nope/inblock.png\n'
            'icon: assets/textures/nope/afterblock.png\n',
            ("assets/textures/nope/inblock.png",
             "assets/textures/nope/afterblock.png"),
            0),
        "a YAML quoted scalar spanning lines": (
            "data/multi.yaml",
            'note: "line one\n'
            '  line two"\n'
            'icon: assets/textures/nope/aftermulti.png\n',
            ("assets/textures/nope/aftermulti.png",),
            0),
        "a Lua short string continued with an escaped newline": (
            "scripts/cont.lua",
            'local s = "abc \\\n'
            'def"\n'
            'local icon = "assets/textures/nope/aftercont.png"\n',
            ("assets/textures/nope/aftercont.png",),
            0),
        "a Haskell string gap": (
            "src/Gap.hs",
            'msg = "abc \\\n'
            '      \\def"\n'
            'icon = "assets/textures/nope/aftergap.png"\n',
            ("assets/textures/nope/aftergap.png",),
            0),
    }
    for label, (path, text, expected, skips) in cases.items():
        code, out, err = check({path: text})
        expect(code == ctp.EXIT_MISSING,
               f"{label}: exits 1, not 2 (got {code}: {err.strip()})")
        for ref in expected:
            expect(ref in out, f"{label}: {ref} is checked")
        expect(skipped_count(out) == skips,
               f"{label}: {skips} comment skip(s) "
               f"(got {out.splitlines()[0]})")


def test_yaml_tag_and_anchor_properties_precede_a_quoted_scalar() -> None:
    """A node's tag/anchor properties sit between the indicator and the scalar,
    so a quote after one still opens a quoted scalar. Missing that, the `#`
    inside `!!str "literal # character"` reads as a comment and masks every
    reference after it on the line."""
    print("\n[req 4] a quote after a YAML tag or anchor still opens a scalar")
    cases = {
        "a tagged scalar": (
            "data/tag.yaml",
            '{note: !!str "literal # character", '
            'icon: "assets/textures/nope/tagged.png"}\n',
            "assets/textures/nope/tagged.png"),
        "an anchored scalar": (
            "data/anc.yaml",
            '{note: &id "literal # character", '
            'icon: "assets/textures/nope/anchored.png"}\n',
            "assets/textures/nope/anchored.png"),
        "a tagged and anchored scalar": (
            "data/both.yaml",
            '{note: !!str &id "literal # character", '
            'icon: "assets/textures/nope/both.png"}\n',
            "assets/textures/nope/both.png"),
    }
    for label, (path, text, ref) in cases.items():
        code, out, err = check({path: text})
        expect(code == ctp.EXIT_MISSING,
               f"{label}: exits 1 (got {code}: {out.strip()} {err.strip()})")
        expect(ref in out,
               f"{label}: the reference after the scalar is still checked")
        expect(skipped_count(out) == 0,
               f"{label}: the `#` inside the scalar started no comment "
               f"(got {out.splitlines()[0]})")


def test_yaml_property_lookbehind_stops_at_a_plain_scalar() -> None:
    """The counterpart to the rule above: only a whole whitespace-delimited
    `!`/`&` token is a property. `hello !world "x"` is one plain scalar, so
    its trailing comment is still a comment."""
    print("\n[req 4] a `!` word inside a plain scalar is not a tag property")
    code, out, err = check({
        "data/plainprop.yaml":
            'note: hello !world "still plain"'
            '  # assets/textures/nope/prose.png\n'
            'icon: assets/textures/nope/code.png\n',
    })
    expect(code == ctp.EXIT_MISSING,
           f"exits 1 (got {code}: {out.strip()} {err.strip()})")
    expect("assets/textures/nope/code.png" in out,
           "the following line is still lexed as code")
    expect("assets/textures/nope/prose.png" not in out,
           "the trailing comment is still a comment")
    expect(skipped_count(out) == 1,
           f"exactly one comment skip (got {out.splitlines()[0]})")


# ---------------------------------------------------------------------------
# Requirement 5 — a genuinely missing reference still fails, all three shapes
# ---------------------------------------------------------------------------

def test_missing_references_still_fail() -> None:
    print("\n[req 5] each supported reference shape still fails when missing")
    cases = {
        "an image-extension file literal": (
            "scripts/f.lua",
            'local icon = "assets/textures/nope/gone.png"\n',
            "assets/textures/nope/gone.png"),
        "a directory base": (
            "scripts/d.lua",
            'boxTextures.load("assets/textures/nopedir")\n',
            "assets/textures/nopedir"),
        "a concatenation prefix": (
            "scripts/p.lua",
            'local base = "assets/textures/nopepfx/" .. name\n',
            "assets/textures/nopepfx"),
    }
    for label, (path, text, ref) in cases.items():
        code, out, err = check({path: text})
        expect(code == ctp.EXIT_MISSING,
               f"{label}: exits 1 (got {code}: {out.strip()} {err.strip()})")
        expect(ref in out, f"{label}: names the missing reference {ref}")
        expect(f"{path}:1" in out, f"{label}: names {path}:1")


def test_lua_long_strings_are_executable_content() -> None:
    print("\n[req 5] a Lua long string is content, not a long comment")
    code, out, err = check({
        "scripts/l.lua": 'local a = [[assets/textures/nope/long.png]]\n'
                         'local b = [==[assets/textures/nope/lvl.png]==]\n',
    })
    expect(code == ctp.EXIT_MISSING,
           f"exits 1 (got {code}: {out.strip()} {err.strip()})")
    expect("assets/textures/nope/long.png" in out,
           "a reference in a [[ ]] long string is checked")
    expect("assets/textures/nope/lvl.png" in out,
           "a reference in a levelled long string is checked")
    expect(skipped_count(out) == 0, "nothing was skipped as a comment")


def test_resolving_references_pass() -> None:
    print("\n[req 5] resolving references of every shape pass")
    code, out, err = check({
        "scripts/ok.lua": 'local a = "assets/textures/real/ok.png"\n'
                          'local b = "assets/textures/dir"\n'
                          'local c = "assets/textures/dir/" .. name\n',
    })
    expect(code == ctp.EXIT_OK,
           f"exits 0 (got {code}: {out.strip()} {err.strip()})")
    expect("OK — all referenced texture paths exist" in out,
           "prints the OK line")
    expect(scanned_count(out) == 2 and skipped_count(out) == 0,
           f"counts the two unique references and no skips "
           f"(got {out.splitlines()[0]})")


def test_json_is_scanned_without_comment_syntax() -> None:
    print("\n[req 5] .json stays in scope and has no comment syntax")
    code, out, err = check({
        "data/j.json": '{"icon": "assets/textures/nope/json.png",\n'
                       ' "hash": "# not a comment -- nor this"}\n',
    })
    expect(code == ctp.EXIT_MISSING,
           f"exits 1 (got {code}: {out.strip()} {err.strip()})")
    expect("data/j.json:1" in out, "names data/j.json:1")
    expect(skipped_count(out) == 0, "skips nothing in a comment-free language")


# ---------------------------------------------------------------------------
# Requirement 6 — an unresolvable input shape is a located error, not a
# quietly smaller scan
# ---------------------------------------------------------------------------

def test_malformed_input_fails_loudly() -> None:
    print("\n[req 6] an unresolvable input shape is a located error")
    cases = {
        "an unterminated Haskell string": (
            "src/E.hs", 'msg = "oops\n', "src/E.hs:1",
            "unterminated string literal"),
        "an unterminated Haskell block comment": (
            "src/E2.hs", 'x = 1\n{- opened here\nstill open\n', "src/E2.hs:2",
            "unterminated block comment"),
        "an unterminated Haskell string gap": (
            "src/E3.hs", 'msg = "opened \\\n', "src/E3.hs:1",
            "unterminated string literal"),
        "an unterminated Lua string": (
            "scripts/e.lua", 'local s = "oops\n', "scripts/e.lua:1",
            "unterminated string literal"),
        "an unterminated Lua long string": (
            "scripts/e2.lua", 'local s = [==[ oops\n', "scripts/e2.lua:1",
            "unterminated long string"),
        "an unterminated Lua long comment": (
            "scripts/e3.lua", 'x = 1\n--[[ oops\n', "scripts/e3.lua:2",
            "unterminated long comment"),
        "an unterminated YAML quoted scalar": (
            "data/e.yaml", 'a: 1\nb: "oops\n', "data/e.yaml:2",
            "unterminated quoted scalar"),
        "an unterminated JSON string": (
            "data/e.json", '{"a": "oops\n', "data/e.json:1",
            "unterminated string literal"),
    }
    for label, (path, text, location, diagnostic) in cases.items():
        code, out, err = check({path: text})
        expect(code == ctp.EXIT_INPUT,
               f"{label}: exits 2 (got {code}: {out.strip()} {err.strip()})")
        expect(location in err,
               f"{label}: the diagnostic names {location} (got {err.strip()})")
        expect(diagnostic in err,
               f"{label}: the diagnostic says '{diagnostic}' "
               f"(got {err.strip()})")
        expect("MISSING" not in out and "OK —" not in out,
               f"{label}: no reference report is produced from a partial scan")


def test_undecodable_input_fails_loudly() -> None:
    print("\n[req 6] an input that cannot be decoded is an error naming it")
    code, out, err = check({
        "scripts/bad.lua": b'local s = "assets/textures/real/\xff.png"\n',
    })
    expect(code == ctp.EXIT_INPUT,
           f"exits 2 (got {code}: {out.strip()} {err.strip()})")
    expect("scripts/bad.lua" in err and "unreadable" in err,
           f"the diagnostic names the file (got {err.strip()})")
    expect("MISSING" not in out and "OK —" not in out,
           "no reference report is produced from a partial scan")


# ---------------------------------------------------------------------------
# Requirement 7 — the skip count is occurrences, not unique paths
# ---------------------------------------------------------------------------

def test_skip_count_is_occurrences() -> None:
    print("\n[req 7] the skip count counts occurrences, not unique paths")
    code, out, err = check({
        "scripts/n.lua": '-- assets/textures/real/ok.png\n'
                         '-- assets/textures/real/ok.png\n'
                         'local a = "assets/textures/real/ok.png"\n',
    })
    expect(code == ctp.EXIT_OK,
           f"exits 0 (got {code}: {out.strip()} {err.strip()})")
    expect(skipped_count(out) == 2,
           f"two skipped occurrences of one path are counted twice "
           f"(got {out.splitlines()[0]})")
    expect(scanned_count(out) == 1,
           "the executable occurrence keeps the path in the unique count")


# ---------------------------------------------------------------------------
# The real repository and the real command line
# ---------------------------------------------------------------------------

def test_real_repository() -> None:
    print("\n[repo] the real tree lexes cleanly and every reference resolves")
    result = subprocess.run(
        [sys.executable, str(REPO_ROOT / "tools" / "check_texture_paths.py")],
        cwd=tempfile.gettempdir(), capture_output=True, text=True)
    expect(result.returncode == ctp.EXIT_OK,
           f"exits 0 from an unrelated cwd (got {result.returncode}: "
           f"{result.stdout.strip()[-300:]} {result.stderr.strip()[-300:]})")
    expect("skipped " in result.stdout,
           "reports its comment-skip count alongside the scanned counts")


def main() -> int:
    selftest.parse_verbose()
    try:
        test_comment_references_are_skipped()
        test_multiline_comment_state_and_code_resumption()
        test_skipping_is_per_reference()
        test_comment_introducer_inside_a_string_literal()
        test_haskell_operator_and_identifier_tick_are_not_comments()
        test_yaml_apostrophe_in_a_plain_scalar()
        test_yaml_hash_inside_a_plain_scalar_is_not_a_comment()
        test_yaml_tag_and_anchor_properties_precede_a_quoted_scalar()
        test_yaml_property_lookbehind_stops_at_a_plain_scalar()
        test_missing_references_still_fail()
        test_lua_long_strings_are_executable_content()
        test_resolving_references_pass()
        test_json_is_scanned_without_comment_syntax()
        test_legal_multiline_constructs_are_not_errors()
        test_malformed_input_fails_loudly()
        test_undecodable_input_fails_loudly()
        test_skip_count_is_occurrences()
        test_real_repository()
    finally:
        for tree in TEMP_TREES:
            shutil.rmtree(tree, ignore_errors=True)
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftest.concluded(1)
    return selftest.concluded(0, "\nAll check_texture_paths tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
