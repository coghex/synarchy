#!/usr/bin/env python3
"""Unit tests for haddock_link_audit.py (issue #2292).

Every rule the audit enforces is pinned in BOTH directions: a fixture
that must be reported, and a fixture that must not be. A checker that
reported everything, or nothing, cannot pass this suite.

The false-greens this suite exists to make unreachable, because the
audit's success path prints the same clean line either way:

* **An ASCII-only definition rule.** This tree is UnicodeSyntax
  throughout (CLAUDE.md §Language & conventions) and has ZERO top-level
  `name ::` signatures, so a detector that recognised only `::` would
  find no definitions at all, report no dead links, generate an EMPTY
  baseline, and pass.
* **A same-line-only definition rule.** `publishValidated` --- the one
  same-module case the initial baseline is required to contain --- puts
  its `∷` on a CONTINUATION line, so a `^name\\s*∷` rule silently drops
  it. The same-module fixture below uses that shape deliberately.
* **A comment scanner that lost a comment form.** A rule that stopped
  recognising `--` line comments, or stopped nesting `{- {- -} -}`,
  would report nothing from the files that use them.
* **Treating every non-code span as a comment.** Strings, character
  literals and quasiquotes are non-code too, and each of them here
  carries a link plus comment-like delimiters.

Every fixture is built in its own temporary root and passed to `main()`
through its explicit `--repo-root`/`--baseline` arguments, so nothing
here reads or writes the shipped `src/`, `app/` or baseline. The audit's
real `main()` is driven, not a private copy of its logic, so a rule that
regressed in the shipped tool fails here.

Usage:
  python3 tools/test_haddock_link_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from haddock_link_audit import (  # type: ignore
    LINK_RE, build_index, comment_text_spans, main)

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

CLEAN_SUMMARY = "No new dead qualified haddock links"

# The module every fixture links INTO: it exports `visible` and hides
# `hidden`, and both are declared with UnicodeSyntax `∷`.
ALPHA = """\
module Alpha
    ( visible
    , Widget(..)
    ) where

data Widget = Widget
    { widgetLabel ∷ Int
    }

visible ∷ Int
visible = 1

hidden ∷ Int
hidden = 2
"""


def _write(root: Path, rel: str, body: str) -> None:
    path = root / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _root(stack: contextlib.ExitStack, **sources: str) -> Path:
    """A synthetic repo root holding Alpha plus the named sources.

    Keys are written under `src/`; a key starting with `app_` lands in
    `app/` instead, so the app tree is exercised too."""
    root = Path(stack.enter_context(tempfile.TemporaryDirectory()))
    _write(root, "src/Alpha.hs", ALPHA)
    for name, body in sources.items():
        if name.startswith("app_"):
            _write(root, f"app/{name[len('app_'):]}.hs", body)
        else:
            _write(root, f"src/{name}.hs", body)
    return root


def _run(root: Path, *extra: str) -> tuple[int, str]:
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        code = main(["--repo-root", str(root),
                     "--baseline", str(root / "baseline.json"), *extra])
    return code, buf.getvalue()


def _expect_reported(body: str, what: str, *, needle: str = "'Alpha.hidden'",
                     **extra: str) -> None:
    with contextlib.ExitStack() as stack:
        code, out = _run(_root(stack, Beta=body, **extra))
    expect(code == 1, f"{what} must exit 1, got {code} with output: {out!r}")
    expect(needle in out,
           f"{what} must name {needle} in its output, got: {out!r}")
    expect("src/Beta.hs:" in out or "app/Main.hs:" in out,
           f"{what} must name the source path and line, got: {out!r}")


def _expect_clean(body: str, what: str, **extra: str) -> None:
    with contextlib.ExitStack() as stack:
        code, out = _run(_root(stack, Beta=body, **extra))
    expect(code == 0, f"{what} must exit 0, got {code} with output: {out!r}")
    expect(CLEAN_SUMMARY in out,
           f"{what} must report the clean summary, got: {out!r}")


# --------------------------------------------------------------------
# Detection: these must be reported.
# --------------------------------------------------------------------

def test_line_comment_cross_module_target_is_reported() -> None:
    _expect_reported("""\
module Beta ( run ) where
-- | Delegates to 'Alpha.hidden', which Alpha does not export.
run ∷ Int
run = 0
""", "an unexported cross-module target in a line comment")


def test_block_comment_cross_module_target_is_reported() -> None:
    _expect_reported("""\
module Beta ( run ) where
{- | Delegates to 'Alpha.hidden', which Alpha does not export. -}
run ∷ Int
run = 0
""", "an unexported cross-module target in a block comment")


def test_nested_block_comment_target_is_reported() -> None:
    _expect_reported("""\
module Beta ( run ) where
{- outer {- inner -} still outer: 'Alpha.hidden' -}
run ∷ Int
run = 0
""", "an unexported target inside a nested block comment")


def test_same_module_continuation_signature_target_is_reported() -> None:
    """The `publishValidated` shape: the module links its OWN unexported
    name, and that name's `∷` sits on a CONTINUATION line."""
    _expect_reported("""\
module Beta ( run ) where
-- | The real work is in 'Beta.helper', which this module hides.
run ∷ Int
run = helper

helper
    ∷ Int
helper = 7
""", "a same-module unexported target with a continuation-line signature",
        needle="'Beta.helper'")


def test_char_literal_does_not_swallow_a_later_comment() -> None:
    """A `'"'` literal must not open a phantom string that hides every
    real comment after it."""
    _expect_reported("""\
module Beta ( quote, run ) where
quote ∷ Char
quote = '"'
-- | Still scanned: 'Alpha.hidden' is dead.
run ∷ Int
run = 0
""", "a comment after a `'\"'` char literal")


def test_unexported_record_field_is_reported() -> None:
    """A record field IS a linkable definition: `'M.field'` resolves
    when M exports its type as `T(..)`, and is dead when M exports the
    bare `T`. Without fields in the definition index the link would be
    silently dismissed as a non-Haskell name, the same way a Lua verb
    is."""
    _expect_reported("""\
module Beta ( run ) where
-- | 'Gamma.hiddenSize' — Gamma exports Hidden, but not its fields.
run ∷ Int
run = 0
""", "a record field whose type is exported without `(..)`",
        needle="'Gamma.hiddenSize'", Gamma="""\
module Gamma ( Hidden ) where
data Hidden = Hidden
    { hiddenSize ∷ Int
    }
""")


def test_app_tree_is_scanned() -> None:
    with contextlib.ExitStack() as stack:
        root = _root(stack, app_Main="""\
module Main ( main ) where
-- | 'Alpha.hidden' is not exported.
main ∷ Int
main = 0
""")
        code, out = _run(root)
    expect(code == 1, f"an app/ source must be scanned, got {code}: {out!r}")
    expect("app/Main.hs:" in out,
           f"the app/ path must be named, got: {out!r}")


def test_new_finding_absent_from_the_baseline_fails() -> None:
    with contextlib.ExitStack() as stack:
        root = _root(stack, Beta="""\
module Beta ( run ) where
-- | 'Alpha.hidden' is dead.
run ∷ Int
run = 0
""")
        code, out = _run(root, "--update-baseline")
        expect(code == 0, f"--update-baseline must exit 0, got {code}")
        code, out = _run(root)
        expect(code == 0,
               f"the freshly baselined tree must be green, got {code}: {out!r}")
        _write(root, "src/Gamma.hs", """\
module Gamma ( go ) where
-- | A NEW dead link: 'Alpha.hidden'.
go ∷ Int
go = 0
""")
        code, out = _run(root)
    expect(code == 1, f"a new unbaselined dead link must exit 1, got {code}")
    expect("not in the baseline" in out,
           f"the diagnostic must say the finding is new, got: {out!r}")
    expect("src/Gamma.hs:2" in out,
           f"the new finding must name its file and line, got: {out!r}")


def test_stale_baseline_entry_fails() -> None:
    with contextlib.ExitStack() as stack:
        root = _root(stack, Beta="""\
module Beta ( run ) where
-- | 'Alpha.hidden' is dead.
run ∷ Int
run = 0
""")
        _run(root, "--update-baseline")
        # The link is fixed the way D-2 prescribes -- demoted to a code
        # span -- but the baseline entry is left behind.
        _write(root, "src/Beta.hs", """\
module Beta ( run ) where
-- | @Alpha.hidden@ is named, not linked.
run ∷ Int
run = 0
""")
        code, out = _run(root)
    expect(code == 1, f"a stale baseline entry must exit 1, got {code}")
    expect("no longer found" in out,
           f"the diagnostic must say the entry is stale, got: {out!r}")
    expect("'Alpha.hidden'" in out,
           f"the stale entry must be named, got: {out!r}")


def test_duplicate_occurrences_are_counted_separately() -> None:
    """Two dead links to the same name in the same file are two baseline
    entries, so fixing only one still fails as stale-plus-new."""
    with contextlib.ExitStack() as stack:
        root = _root(stack, Beta="""\
module Beta ( run ) where
-- | 'Alpha.hidden' once.
-- | 'Alpha.hidden' twice.
run ∷ Int
run = 0
""")
        _run(root, "--update-baseline")
        entries = (root / "baseline.json").read_text(encoding="utf-8")
        expect(entries.count("'Alpha.hidden'") == 2,
               f"both occurrences must be baselined, got: {entries!r}")
        _write(root, "src/Beta.hs", """\
module Beta ( run ) where
-- | @Alpha.hidden@ once.
-- | 'Alpha.hidden' twice.
run ∷ Int
run = 0
""")
        code, out = _run(root)
    expect(code == 1, f"fixing one of two occurrences must exit 1, got {code}")
    expect("no longer found" in out,
           f"the surviving-count drop must be reported stale, got: {out!r}")


# --------------------------------------------------------------------
# Permitted: these must not be reported.
# --------------------------------------------------------------------

def test_exported_target_is_permitted() -> None:
    _expect_clean("""\
module Beta ( run ) where
-- | Delegates to 'Alpha.visible', which Alpha exports.
run ∷ Int
run = 0
""", "an exported target")


def test_module_reexport_is_permitted() -> None:
    """`module Alpha` supplies what Alpha itself exports, so a link
    naming Gamma resolves even though Gamma's list never spells the
    name."""
    _expect_clean("""\
module Beta ( run ) where
-- | Reached through 'Gamma.visible', which Gamma re-exports from Alpha.
run ∷ Int
run = 0
""", "a valid `module X` re-export", Gamma="""\
module Gamma
    ( module Alpha
    , spin
    ) where
import Alpha
spin ∷ Int
spin = 0
""")


def test_comment_inside_an_export_list_is_permitted() -> None:
    """The real defect this pins: `haskell_code_only` blanks a comment
    to NULs so offsets survive, and an export entry followed by a
    haddock heading is `name` + NULs. A parser that did not turn those
    back into whitespace read the entry as a name ending in NULs, so
    `World.Load.Publish`'s exported `publishStagedSession` -- the tree's
    most-linked target, with a `-- *` heading right after it -- was
    reported dead at all ten of its sites."""
    _expect_clean("""\
module Beta ( run ) where
-- | 'Gamma.spin' is exported, despite the headings around it.
run ∷ Int
run = 0
""", "an export list interleaved with haddock headings", Gamma="""\
module Gamma
    ( -- * The entry point
      spin
      -- * Exported for its gate only
    , wind
    ) where
spin ∷ Int
spin = 0
wind ∷ Int
wind = 0
""")


def test_self_reexport_exports_everything() -> None:
    """`module M` inside M's own export list re-exports every name in
    scope there -- `Engine.Core.State` is the tree's canonical case
    (`module Engine.Core.State, module Engine.Core.Lifecycle`), and
    every link into it resolves, record fields included."""
    _expect_clean("""\
module Beta ( run ) where
-- | 'Gamma.tucked' and 'Gamma.gizmoWidth' both resolve.
run ∷ Int
run = 0
""", "a module re-exporting itself", Gamma="""\
module Gamma
    ( module Gamma
    ) where
data Gizmo = Gizmo
    { gizmoWidth ∷ Int
    }
tucked ∷ Int
tucked = 0
""")


def test_module_reexport_does_not_supply_a_hidden_name() -> None:
    """A `module Alpha` re-export carries Alpha's EXPORTS, not its
    private definitions -- so it must not launder a dead link clean."""
    _expect_reported("""\
module Beta ( run ) where
-- | 'Gamma.hidden' — Alpha hides it, so Gamma cannot re-export it.
run ∷ Int
run = 0
""", "a name absent from the re-exported module's own export list",
        needle="'Gamma.hidden'", Gamma="""\
module Gamma
    ( module Alpha
    , spin
    ) where
import Alpha
spin ∷ Int
spin = 0
""")


def test_module_without_an_export_list_is_permitted() -> None:
    _expect_clean("""\
module Beta ( run ) where
-- | 'Gamma.buried' — Gamma exports everything it defines.
run ∷ Int
run = 0
""", "a module with no explicit export list", Gamma="""\
module Gamma where
buried ∷ Int
buried = 0
""")


def test_record_field_through_open_type_export_is_permitted() -> None:
    _expect_clean("""\
module Beta ( run ) where
-- | 'Alpha.widgetLabel' resolves through Alpha's Widget(..).
run ∷ Int
run = 0
""", "a record field exported through `Type(..)`")


def test_record_field_of_a_type_declared_elsewhere_is_permitted() -> None:
    """`Type(..)` must resolve against the type's OWN declaration, which
    routinely lives in another module (`Unit.Types` exports
    `UnitManager(..)`, declared in `Unit.Types.Manager`)."""
    _expect_clean("""\
module Beta ( run ) where
-- | 'Gamma.gadgetSize' resolves through Gamma's Gadget(..).
run ∷ Int
run = 0
""", "a record field of a type declared in another module",
        Gamma="""\
module Gamma ( Gadget(..) ) where
import Delta
""",
        Delta="""\
module Delta ( Gadget(..) ) where
data Gadget = Gadget
    { gadgetSize ∷ Int
    }
""")


def test_code_span_is_permitted() -> None:
    """The spelling D-2 makes every sweep write. It is excluded by its
    DELIMITER, so this fixture also pins that the audit never widens
    into `@…@` as a second reportable class."""
    _expect_clean("""\
module Beta ( run ) where
-- | @Alpha.hidden@ is named as a code span, not linked.
run ∷ Int
run = 0
""", "an `@Module.function@` code span")


def test_quoted_link_inside_a_code_span_is_still_reported() -> None:
    """Haddock resolves a quoted identifier inside `@…@` too, so a
    `'M.f'` written there is a real link. A guard that masked code spans
    before scanning would lose it -- and would lose every link sitting
    between two unrelated `@` characters."""
    _expect_reported("""\
module Beta ( run ) where
-- | @let x = 'Alpha.hidden' in x@
run ∷ Int
run = 0
""", "a quoted link inside a code span")


def test_backtick_span_is_permitted() -> None:
    """`src/Unit/Pathing/Cost.hs` writes the very name the audit hunts
    (`Unit.Pathing.Config.finiteOr`) in backticks; D-1 scopes this arc
    to the single-quoted spelling only."""
    _expect_clean("""\
module Beta ( run ) where
-- | `Alpha.hidden` is prose, not a haddock link.
run ∷ Int
run = 0
""", "a backtick-quoted qualified reference")


def test_unqualified_link_is_permitted() -> None:
    _expect_clean("""\
module Beta ( run ) where
-- | 'hidden' is unqualified, so D-1 leaves it alone.
run ∷ Int
run = 0
""", "an unqualified link")


def test_module_link_is_permitted() -> None:
    _expect_clean("""\
module Beta ( run ) where
-- | "Alpha" is a module link, not a qualified function link.
run ∷ Int
run = 0
""", "a module link")


def test_module_outside_the_repository_is_permitted() -> None:
    _expect_clean("""\
module Beta ( run ) where
-- | 'Data.Text.someUnexportedThing' names a module we do not own.
run ∷ Int
run = 0
""", "a repository-external module")


def test_lua_binding_name_is_permitted() -> None:
    """`'UI.setVisible'` resembles a Haskell reference only because a
    module named `UI` exists; `setVisible` is a Lua verb with no Haskell
    definition anywhere, so it is not a candidate."""
    _expect_clean("""\
module Beta ( run ) where
-- | The script calls 'UI.setVisible' on the element.
run ∷ Int
run = 0
""", "a Lua binding name", UI="""\
module UI ( render ) where
render ∷ Int
render = 0
""")


def test_string_literal_is_permitted() -> None:
    _expect_clean("""\
module Beta ( note, run ) where
note ∷ String
note = "-- 'Alpha.hidden' {- still inside the string -}"
run ∷ Int
run = 0
""", "a string containing a link and comment-like delimiters")


def test_char_literal_is_permitted() -> None:
    _expect_clean("""\
module Beta ( tick, quote, run ) where
tick ∷ Char
tick = '\\''
quote ∷ Char
quote = '"'
run ∷ Int
run = 0
""", "a character literal")


def test_quasiquote_is_permitted() -> None:
    _expect_clean("""\
module Beta ( shader, run ) where
shader ∷ String
shader = [glsl|
  // 'Alpha.hidden' -- {- comment-like delimiters inside a quasiquote -}
  |]
run ∷ Int
run = 0
""", "a quasiquote containing a link and comment-like delimiters")


def test_constructor_link_is_permitted() -> None:
    """D-1 scopes the arc to FUNCTIONS. The rule lives in the production
    `LINK_RE` itself, which is asserted directly as well as end to end:
    no reachable fixture can distinguish a widened symbol class from the
    correct one, because a constructor is never in the definition index
    either, so the end-to-end run alone would pass a widened regex."""
    _expect_clean("""\
module Beta ( run ) where
-- | 'Alpha.Widget' names a constructor, which D-1 leaves alone.
run ∷ Int
run = 0
""", "an upper-case (constructor) target")
    expect(LINK_RE.search("'Alpha.Widget'") is None,
           "an upper-case symbol must not be a candidate at all")
    expect(LINK_RE.search("'Alpha.hidden'") is not None,
           "a lower-case symbol must still be a candidate")
    match = LINK_RE.search("see 'World.Save.Storage.publishValidated' here")
    expect(match is not None
           and match.group(1) == "World.Save.Storage"
           and match.group(2) == "publishValidated",
           "a multi-component module must split at its LAST dot, got "
           f"{match.groups() if match else None}")


# --------------------------------------------------------------------
# The scanner and the baseline themselves.
# --------------------------------------------------------------------

def test_unicode_syntax_signatures_are_recognized() -> None:
    """The false-green this pins: an ASCII-`::`-only definition rule
    finds nothing in this tree and reports a clean run."""
    with contextlib.ExitStack() as stack:
        root = _root(stack)
        index = build_index(root)
    expect("hidden" in index.definitions,
           "a `∷` signature must register as a definition; an ASCII-only "
           f"rule would leave definitions empty, got {sorted(index.definitions)}")
    expect(index.definitions.get("hidden") == {"Alpha"},
           "the defining module must be recorded so the diagnostic can "
           f"name it, got {index.definitions.get('hidden')}")
    expect("widgetLabel" in index.record_fields.get("Widget", set()),
           "a record field must be indexed against its own type, got "
           f"{index.record_fields}")


def test_comment_spans_exclude_strings_and_quasiquotes() -> None:
    """Comment spans are reported by the shared scanner, never inferred
    by subtracting code spans -- strings and quasiquotes are non-code
    too."""
    source = ('module M where\n'
              '-- a comment\n'
              's = "not -- a comment"\n'
              'q = [glsl| also -- not a comment |]\n')
    spans = comment_text_spans(source)
    texts = [source[a:b] for a, b in spans]
    expect(texts == ["-- a comment"],
           f"only the real comment must be a comment span, got {texts!r}")


def test_baseline_generation_is_deterministic() -> None:
    with contextlib.ExitStack() as stack:
        root = _root(stack, Beta="""\
module Beta ( run ) where
-- | 'Alpha.hidden' and 'Beta.helper' are both dead.
run ∷ Int
run = helper
helper
    ∷ Int
helper = 0
""", Gamma="""\
module Gamma ( go ) where
-- | 'Alpha.hidden' again.
go ∷ Int
go = 0
""")
        _run(root, "--update-baseline")
        first = (root / "baseline.json").read_text(encoding="utf-8")
        _run(root, "--update-baseline")
        second = (root / "baseline.json").read_text(encoding="utf-8")
        code, out = _run(root)
    expect(first == second,
           "two --update-baseline runs must produce identical bytes")
    expect(first.count('"link"') == 3,
           f"all three dead links must be baselined, got: {first!r}")
    expect(code == 0 and "3 still baselined" in out,
           f"the baselined tree must be green and count 3, got {code}: {out!r}")


def test_command_line_entry_point_runs() -> None:
    """The shipped script must work as a SUBPROCESS, not only through an
    in-process `main()` a refactor could orphan."""
    with contextlib.ExitStack() as stack:
        root = _root(stack, Beta="""\
module Beta ( run ) where
-- | 'Alpha.hidden' is dead.
run ∷ Int
run = 0
""")
        result = subprocess.run(
            [sys.executable, str(Path(__file__).resolve().parent
                                 / "haddock_link_audit.py"),
             "--repo-root", str(root),
             "--baseline", str(root / "baseline.json")],
            capture_output=True, text=True)
    expect(result.returncode == 1,
           f"the CLI must exit 1 on a dead link, got {result.returncode}")
    expect("'Alpha.hidden'" in result.stdout,
           f"the CLI must print the finding, got: {result.stdout!r}")


def test_shipped_baseline_matches_the_shipped_tree() -> None:
    """The checked-in baseline must describe THIS tree: a landed sweep
    that forgot to shrink it, or a new dead link, fails here as well as
    in the gate."""
    code, out = 0, ""
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        code = main([])
    out = buf.getvalue()
    expect(code == 0,
           f"the shipped tree must match its baseline, got {code}: {out!r}")
    expect(CLEAN_SUMMARY in out,
           f"the shipped run must report the clean summary, got: {out!r}")


TESTS = [
    test_line_comment_cross_module_target_is_reported,
    test_block_comment_cross_module_target_is_reported,
    test_nested_block_comment_target_is_reported,
    test_same_module_continuation_signature_target_is_reported,
    test_char_literal_does_not_swallow_a_later_comment,
    test_unexported_record_field_is_reported,
    test_app_tree_is_scanned,
    test_new_finding_absent_from_the_baseline_fails,
    test_stale_baseline_entry_fails,
    test_duplicate_occurrences_are_counted_separately,
    test_exported_target_is_permitted,
    test_module_reexport_is_permitted,
    test_comment_inside_an_export_list_is_permitted,
    test_self_reexport_exports_everything,
    test_module_reexport_does_not_supply_a_hidden_name,
    test_module_without_an_export_list_is_permitted,
    test_record_field_through_open_type_export_is_permitted,
    test_record_field_of_a_type_declared_elsewhere_is_permitted,
    test_code_span_is_permitted,
    test_quoted_link_inside_a_code_span_is_still_reported,
    test_backtick_span_is_permitted,
    test_unqualified_link_is_permitted,
    test_module_link_is_permitted,
    test_module_outside_the_repository_is_permitted,
    test_lua_binding_name_is_permitted,
    test_string_literal_is_permitted,
    test_char_literal_is_permitted,
    test_quasiquote_is_permitted,
    test_constructor_link_is_permitted,
    test_unicode_syntax_signatures_are_recognized,
    test_comment_spans_exclude_strings_and_quasiquotes,
    test_baseline_generation_is_deterministic,
    test_command_line_entry_point_runs,
    test_shipped_baseline_matches_the_shipped_tree,
]


def main_() -> int:
    selftestlib.parse_verbose()
    for test in TESTS:
        print(f"{test.__name__}:")
        test()
    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s)")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, f"\nAll {len(TESTS)} tests passed")


if __name__ == "__main__":
    raise SystemExit(main_())
