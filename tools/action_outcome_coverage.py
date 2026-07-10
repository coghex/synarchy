#!/usr/bin/env python3
"""F4 (#646) action-outcome coverage self-audit.

The action-outcome oracle (`debug.recordOutcome` / `debug.drainActionOutcomes`)
only has value where somebody actually wired it up at a commit boundary.
Rather than trust a hand-maintained "is X done" list that silently drifts
as new commit-boundary verbs are added, this greps each registered verb's
own source for its instrumentation call site and reports yes/no per verb —
mirrors `tools/ci_probes.py --status`'s "make the gap visible" self-audit,
in the no-engine-needed style of `tools/lua_module_budget.py`.

A file-wide substring search is not precise enough on its own: several
verbs share a file (e.g. unitAi.commandMove/commandAttack both live in
scripts/unit_ai_core.lua; craft.execute/executeAt share
Craft/Execute.hs), so a naive per-file pattern would mark BOTH complete
the moment either one is instrumented. Each verb below is checked within
its OWN function body — the span from its definition to the next
top-level definition in the same file — using a pattern that only
matches a real call site, not the instrumentation helper's own
definition line. `--self-test` proves this discriminates (see below).

Growing coverage is a two-step: add the real `debug.recordOutcome` /
`pushActionOutcome` call at the commit boundary, then register the verb
here (or extend its function-scope patterns) so the audit stops
flagging it as a gap.

Usage:
  python3 tools/action_outcome_coverage.py
  python3 tools/action_outcome_coverage.py --self-test
Exit code is always 0 for the coverage report — this is a visibility
report, not a blocking gate. Tier 2/3 gaps are deliberate fast-follows
(see issue #646), not regressions; only Tier 1 (this PR's scope) is
expected to read 100%. --self-test exits 1 on a self-test failure.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

LUA_FUNCTION_BOUNDARY = r"^function "
HASKELL_TOPLEVEL_BOUNDARY = "^\\w+\\s*∷"  # '∷' marks a top-level type signature


def _function_scope(text: str, start_pattern: str, boundary_pattern: str) -> str | None:
    """The slice of `text` from the first match of `start_pattern` up to
    (not including) the next match of `boundary_pattern` after it — i.e.
    one definition's own body, not the whole file. `None` if
    `start_pattern` doesn't match at all."""
    m = re.search(start_pattern, text, re.MULTILINE)
    if m is None:
        return None
    rest = text[m.end():]
    b = re.search(boundary_pattern, rest, re.MULTILINE)
    return rest[:b.start()] if b else rest


def _scoped_check(relpath: str, start_pattern: str, boundary_pattern: str,
                   instrument_pattern: str) -> bool:
    path = REPO_ROOT / relpath
    if not path.exists():
        return False
    scope = _function_scope(path.read_text(encoding="utf-8"),
                             start_pattern, boundary_pattern)
    return scope is not None and re.search(instrument_pattern, scope) is not None


def _filewide_check(relpath: str, pattern: str) -> bool:
    path = REPO_ROOT / relpath
    return path.exists() and re.search(
        pattern, path.read_text(encoding="utf-8")) is not None


def _all_present(text: str, patterns: list[str]) -> bool:
    """Every pattern must independently match `text` — for a multi-route
    area (Layer A's several distinct swallow/no-handler routes, or a
    single verb's several distinct reject reasons) where a single
    "does ANY instrumentation exist in this file" pattern would read
    DONE the moment just one route/reason is wired up, hiding the rest
    (review round 2)."""
    return all(re.search(p, text) is not None for p in patterns)


def _all_present_check(relpath: str, patterns: list[str]) -> bool:
    path = REPO_ROOT / relpath
    if not path.exists():
        return False
    return _all_present(path.read_text(encoding="utf-8"), patterns)


def _count_at_least(text: str, pattern: str, n: int) -> bool:
    return len(re.findall(pattern, text)) >= n


# Anchored to an ACTUAL oracle call, not just a bare literal appearing
# somewhere in the file — a handler/route STRING with no producer call
# anywhere near it (the call renamed, replaced, or deleted while a
# literal was left behind, e.g. in a stale comment or an unrelated
# table) must NOT read as instrumented. Review round 3's counter-example
# was the bare-substring version of this for build-tool; round 4 found
# the `reason =`/`outcome = `-only version of the same hole there; round
# 5 found the identical hole here — Layer A's two areas checked only
# handler literals, never the `recordRouteOutcome`/`recordClick` call
# itself. Every entry below opens with the real call name and a bounded
# lazy window before the literal it's checking, mirroring _ROC's fix.
_ROC_ROUTE = r"recordRouteOutcome[\s\S]{0,80}?"  # Engine.Input.Thread's helper
_ROC_CLICK = r"recordClick\([\s\S]{0,60}?"       # init_mouse.lua's helper

# Shared source of truth between the real checks below and the self-test:
# every distinct route/reason literal a multi-route area is expected to
# carry. Defined once so the self-test proving "remove one -> gap" can't
# silently drift from what main() actually checks (review round 2).
LAYER_A_SWALLOWED_ROUTES = [
    _ROC_ROUTE + r'"degenerate_viewport"', _ROC_ROUTE + r'"tooltip_lock_toggle"',
    _ROC_ROUTE + r'"ui_surface_block"', _ROC_ROUTE + r'"camera_drag"',
    _ROC_ROUTE + r'"tooltip_lock_dismiss"',
    # The no-right-click-handler route records the control's OWN
    # left-click callback (its real identity, review round 8), not a
    # fixed literal — so this checks the SOURCE STRUCTURE that wires it
    # through, not a runtime string.
    _ROC_ROUTE + r'"accepted"\s*\(Just leftClickCallback\)',
    _ROC_ROUTE + r'"unmapped_button"',  # GLFW buttons 4-8, mapped to Lua button 0
]
LAYER_A_GAME_CHAIN_HANDLERS = [
    _ROC_CLICK + r'"debug_overlay"', _ROC_CLICK + r'"debug_anim_panel"',
    _ROC_CLICK + r'"build_tool"', _ROC_CLICK + r'"mine_tool"',
    _ROC_CLICK + r'"chop_tool"', _ROC_CLICK + r'"till_tool"',
    _ROC_CLICK + r'"plant_tool"', _ROC_CLICK + r'"unit_select"',
    _ROC_CLICK + r'"item_select"', _ROC_CLICK + r'"building_select"',
    _ROC_CLICK + r'"deselect"', _ROC_CLICK + r'"context_menu_building"',
    _ROC_CLICK + r'"context_menu_unit"', _ROC_CLICK + r'"context_menu_item"',
    _ROC_CLICK + r'"move_order"', _ROC_CLICK + r'"context_menu_tile"',
]

# Anchored to an ACTUAL `debug.recordOutcome{...}` call, not just a bare
# `reason =`/`outcome = ` field appearing somewhere in the function — a
# table literal with those exact fields but no `debug.recordOutcome`
# prefix (e.g. the call renamed, replaced, or deleted while its `{...}`
# argument was left behind) must NOT read as instrumented (review round
# 3's counter-example was the bare-substring version of this; round 4
# found the `reason =`/`outcome = `-only version of the same hole — a
# `debug.recordOutcome` mention with an entirely different field set
# elsewhere in the function must not satisfy a DIFFERENT exit's
# requirement, so each pattern demands the call and its own field
# within one bounded, lazy window rather than treating "some call
# exists somewhere" and "this field exists somewhere" as independent).
_ROC = r"debug\.recordOutcome\{[\s\S]{0,220}?"  # call open + bounded body
COMMIT_PLACEMENT_REQUIRED = [
    _ROC + r'reason\s*=[^\n]*"not a placeable power item',
    _ROC + r'reason\s*=[^\n]*"no selected unit carries',
    _ROC + r"reason\s*=\s*tostring\(buildingIdOrErr\)",
    _ROC + r'outcome\s*=\s*"accepted"',
]
HANDLE_MOUSE_DOWN_REQUIRED = [
    _ROC + r'reason\s*=[^\n]*"off-world click during placement"',
    _ROC + r'reason\s*=[^\n]*"invalid placement tile"',
    _ROC + r'reason\s*=[^\n]*"building\.spawn failed"',
]


def _game_chain_check(text: str) -> bool:
    # Three independent deadclick call sites today: the MOUSE_LEFT
    # inactive-gameplay gate, the MOUSE_RIGHT inactive-gameplay gate, and
    # the off-world no-selection tile-menu miss (review round 6 — the
    # count was stuck at 2, so the two original gates alone satisfied it
    # even with the third, newer site's hook removed/renamed). Bump this
    # whenever a genuinely new deadclick site is added. The third site
    # ALSO gets its own route-specific pattern (its "no tile under
    # cursor" reason is unique among the three) so it stays covered even
    # if a future fourth deadclick site changes what the plain count of 3
    # would mean.
    return bool(text) and _all_present(text, LAYER_A_GAME_CHAIN_HANDLERS) \
        and _count_at_least(text, _ROC_CLICK + r'"deadclick"', 3) \
        and _all_present(text, [
            _ROC_CLICK + r'"deadclick"[\s\S]{0,40}?"no tile under cursor"'])


#   The portal-accepted hook has no distinguishing reason text (success
#   carries no reason at all), so it's anchored to `building.spawn(...)`
#   — unique to the portal branch — rather than a reason literal
#   (review round 8: neither the plain outcome="accepted" text nor the
#   "building.spawn failed" reject reason alone proved this specific
#   hook, since other accepted/rejected calls in the same function
#   already satisfy those).
#   Bounded by a negative lookahead on `else` rather than a fixed
#   character window: a window wide enough to survive real source's
#   comment placement also bridges past the `if id then ... else` block
#   boundary in more tightly-packed text (a prior attempt at this
#   pattern's window did exactly that, matching the NEXT unrelated
#   accepted call instead) — `(?:(?!else)[\s\S])*?` can consume any text
#   EXCEPT a run starting with the literal "else", so the search is
#   structurally confined to the `if id then` branch itself, regardless
#   of how much (or how little) sits in between.
_PORTAL_ACCEPTED = (r"building\.spawn\(target\.def, igx, igy\)"
                    r"(?:(?!else)[\s\S])*?" + _ROC + r'outcome\s*=\s*"accepted"')


def _build_tool_check(text: str) -> bool:
    if not text:
        return False
    commit_scope = _function_scope(
        text, r"^function buildTool\.commitPlacement", LUA_FUNCTION_BOUNDARY)
    handle_scope = _function_scope(
        text, r"^function buildTool\.handleMouseDown", LUA_FUNCTION_BOUNDARY)
    if commit_scope is None or handle_scope is None:
        return False
    return (_all_present(commit_scope, COMMIT_PLACEMENT_REQUIRED)
            and _all_present(handle_scope, HANDLE_MOUSE_DOWN_REQUIRED)
            and _all_present(handle_scope, [_PORTAL_ACCEPTED])
            and _count_at_least(
                handle_scope, _ROC + r'reason\s*=[^\n]*"routed to construction\.designate"', 2)
            and _count_at_least(
                handle_scope, _ROC + r'reason\s*=[^\n]*"no active world id"', 2))


# Each entry: (tier, verb, check) where check() -> bool. Built below so
# each verb's own check can pick file-wide vs function-scoped as needed.
def _build_verbs() -> list[tuple[str, str, callable]]:
    return [
        # --- Layer A: input routing, "complete" per the issue's scope note ---
        ("A", "input click -> UI widget consumption (real event queued)",
         # Both the left- and right-click producer calls are required —
         # a single "does recordWidgetClickOutcome appear anywhere"
         # check reads DONE with either one deleted (review round 8).
         lambda: _all_present_check(
             "src/Engine/Scripting/Lua/Thread/Dispatch.hs",
             [r'recordWidgetClickOutcome env "input\.click"',
              r'recordWidgetClickOutcome env "input\.rightClick"'])),
        ("A", "input click -> swallowed/no-handler routes (no event ever queued)",
         # Every distinct ClickSwallowed/no-handler-ClickUI route this
         # module knows about — ALL must be present, not just one, or a
         # route silently loses its record (review round 2 found the
         # degenerate-viewport and middle-click-miss routes missing
         # while the others made the whole area read DONE).
         lambda: _all_present_check(
             "src/Engine/Input/Thread.hs", LAYER_A_SWALLOWED_ROUTES)),
        ("A", "input click -> game-world tool/select/deadclick chain",
         lambda: _game_chain_check(
             (REPO_ROOT / "scripts/init_mouse.lua").read_text(encoding="utf-8")
             if (REPO_ROOT / "scripts/init_mouse.lua").exists() else "")),

        # --- Layer B Tier 1: onboarding + highest naive-frequency (this PR) ---
        ("B1", "createWorld.generate (proceed commit)",
         lambda: _filewide_check(
             "scripts/create_world/generation.lua", r"debug\.recordOutcome")),
        ("B1", "buildTool.commitPlacement",
         # Every distinct reject/accept site handleMouseDown's placement
         # branch and commitPlacement itself cover — a single file-wide
         # "does debug.recordOutcome appear anywhere" pattern reads DONE
         # even if every hook but one were deleted (review round 2).
         lambda: _build_tool_check(
             (REPO_ROOT / "scripts/build_tool.lua").read_text(encoding="utf-8")
             if (REPO_ROOT / "scripts/build_tool.lua").exists() else "")),
        ("B1", "wire.place",
         lambda: _filewide_check(
             "scripts/wire.lua", r"debug\.recordOutcome")),
        # Each designation verb needs BOTH its partial/reject-within-a-
        # loaded-page hook AND its separate missing-world-page hook
        # (review round 7 found all four silently dropped the latter —
        # `pure ()` with no F4 record at all when the queued page no
        # longer exists, a different failure than "page exists, sweep
        # found nothing").
        ("B1", "till.designate (partial-drop counts + missing-world)",
         lambda: _all_present_check(
             "src/World/Thread/Command/Cursor/Till.hs",
             [r'recordDesignationOutcome env "till\.designate"',
              r'recordMissingWorldOutcome env "till\.designate"'])),
        ("B1", "chop.designate (partial-drop counts + missing-world)",
         lambda: _all_present_check(
             "src/World/Thread/Command/Cursor/Chop.hs",
             [r'recordDesignationOutcome env "chop\.designate"',
              r'recordMissingWorldOutcome env "chop\.designate"'])),
        ("B1", "world.designateMine (partial-drop counts + missing-world)",
         lambda: _all_present_check(
             "src/World/Thread/Command/Cursor/Mine.hs",
             [r'recordDesignationOutcome env "world\.designateMine"',
              r'recordMissingWorldOutcome env "world\.designateMine"'])),
        ("B1", "plant.designate (accept/reject + missing-world)",
         # Both branches share the same aoKind literal (only aoOutcome
         # differs), so a single aoKind presence check reads DONE with
         # either branch's push deleted (review round 8) — require both
         # aoOutcome values specifically, not just the shared aoKind.
         lambda: _all_present_check(
             "src/World/Thread/Command/Cursor/Plant.hs",
             [r'aoKind\s*=\s*"plant\.designate"',
              r'aoOutcome\s*=\s*"accepted"',
              r'aoOutcome\s*=\s*"rejected"',
              r'recordMissingWorldOutcome env "plant\.designate"'])),

        # --- Layer B Tier 2: common mid-game — fast-follow, not this PR.
        # Function-scoped: commandMove/commandAttack share a file, as do
        # execute/executeAt, so a file-wide pattern would false-positive
        # the moment either sibling is instrumented. ---
        ("B2", "unitAi.commandMove",
         lambda: _scoped_check(
             "scripts/unit_ai_core.lua",
             r"^function unitAi\.commandMove", LUA_FUNCTION_BOUNDARY,
             r"debug\.recordOutcome")),
        ("B2", "unitAi.commandAttack",
         lambda: _scoped_check(
             "scripts/unit_ai_core.lua",
             r"^function unitAi\.commandAttack", LUA_FUNCTION_BOUNDARY,
             r"debug\.recordOutcome")),
        ("B2", "craft.execute",
         lambda: _scoped_check(
             "src/Engine/Scripting/Lua/API/Craft/Execute.hs",
             r"^craftExecuteFn\s*∷", HASKELL_TOPLEVEL_BOUNDARY,
             r"pushActionOutcome")),
        ("B2", "craft.executeAt",
         lambda: _scoped_check(
             "src/Engine/Scripting/Lua/API/Craft/Execute.hs",
             r"^craftExecuteAtFn\s*∷", HASKELL_TOPLEVEL_BOUNDARY,
             r"pushActionOutcome")),
        ("B2", "craft.addBill",
         lambda: _scoped_check(
             "src/Engine/Scripting/Lua/API/Craft/Bill.hs",
             r"^craftAddBillFn\s*∷", HASKELL_TOPLEVEL_BOUNDARY,
             r"pushActionOutcome")),

        # --- Layer B Tier 3: everything else, added as those paths get
        # touched. construction.designate is structurally identical to
        # till/chop/mine but wasn't named in the issue's Tier 1 list. ---
        ("B3", "construction.designate (building/structure)",
         lambda: _filewide_check(
             "src/World/Thread/Command/Cursor/Construct.hs",
             r'recordDesignationOutcome env "construction\.designate"')),
    ]


def check() -> list[tuple[str, str, bool]]:
    return [(tier, verb, fn()) for tier, verb, fn in _build_verbs()]


def main() -> int:
    results = check()
    done = sum(1 for *_r, ok in results if ok)
    print(f"F4 action-outcome coverage: {done}/{len(results)} registered "
          f"commit-boundary verbs instrumented\n")
    for tier, verb, ok in results:
        mark = "DONE" if ok else "gap "
        print(f"  [{mark}] tier {tier:<2}  {verb:<58}")

    gaps = [(tier, verb) for tier, verb, ok in results if not ok]
    if gaps:
        print(f"\n{len(gaps)} gap(s) — expected for Tier 2/3 fast-follows, "
              f"not for Tier 1:")
        for tier, verb in gaps:
            print(f"  tier {tier}: {verb}")
    return 0


# ---------------------------------------------------------------------------
# Self-test: proves the function-scoping actually discriminates between two
# sibling definitions in the same file, rather than false-positiving the
# moment EITHER one is instrumented (the exact bug review round 1 found).
# ---------------------------------------------------------------------------

_LUA_SIBLINGS = """\
function unitAi.commandMove(uid, tx, ty, speed)
    if not tx then return end
    s.commandedTask = {tx, ty}
end

function unitAi.commandAttack(uid, targetUid, committed)
    if not targetUid then return end
end
"""

_LUA_SIBLINGS_ONE_INSTRUMENTED = """\
function unitAi.commandMove(uid, tx, ty, speed)
    if not tx then return end
    debug.recordOutcome{kind = "unitAi.commandMove", outcome = "accepted"}
    s.commandedTask = {tx, ty}
end

function unitAi.commandAttack(uid, targetUid, committed)
    if not targetUid then return end
end
"""

_HS_SIBLINGS_ONE_INSTRUMENTED = """\
craftExecuteFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftExecuteFn env = do
    pushActionOutcome (actionOutcomeRef env) ev
    pure ()

craftExecuteAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftExecuteAtFn env = do
    pure ()
"""


def _self_test() -> list[str]:
    failures = []

    def expect(label: str, got: bool, want: bool) -> None:
        if got != want:
            failures.append(f"{label}: expected {want}, got {got}")

    # 1. Neither sibling instrumented -> both scopes report False.
    move_scope = _function_scope(_LUA_SIBLINGS, r"^function unitAi\.commandMove",
                                  LUA_FUNCTION_BOUNDARY)
    attack_scope = _function_scope(_LUA_SIBLINGS, r"^function unitAi\.commandAttack",
                                    LUA_FUNCTION_BOUNDARY)
    expect("neither instrumented: commandMove",
           bool(move_scope and re.search(r"debug\.recordOutcome", move_scope)), False)
    expect("neither instrumented: commandAttack",
           bool(attack_scope and re.search(r"debug\.recordOutcome", attack_scope)), False)

    # 2. Only commandMove instrumented -> commandMove True, commandAttack
    #    STILL False. A naive file-wide `debug\.recordOutcome` search
    #    would incorrectly mark commandAttack True too — this is exactly
    #    what review round 1 flagged.
    move_scope2 = _function_scope(_LUA_SIBLINGS_ONE_INSTRUMENTED,
                                   r"^function unitAi\.commandMove", LUA_FUNCTION_BOUNDARY)
    attack_scope2 = _function_scope(_LUA_SIBLINGS_ONE_INSTRUMENTED,
                                     r"^function unitAi\.commandAttack", LUA_FUNCTION_BOUNDARY)
    expect("one instrumented: commandMove detected",
           bool(move_scope2 and re.search(r"debug\.recordOutcome", move_scope2)), True)
    expect("one instrumented: commandAttack NOT falsely detected",
           bool(attack_scope2 and re.search(r"debug\.recordOutcome", attack_scope2)), False)
    filewide_would_be_wrong = bool(
        re.search(r"debug\.recordOutcome", _LUA_SIBLINGS_ONE_INSTRUMENTED))
    expect("file-wide search WOULD have false-positived (sanity check on the fixture)",
           filewide_would_be_wrong, True)

    # 3. Same proof for the Haskell function-scope boundary
    #    (craftExecuteFn vs craftExecuteAtFn sharing a file).
    exec_scope = _function_scope(_HS_SIBLINGS_ONE_INSTRUMENTED,
                                  r"^craftExecuteFn\s*∷", HASKELL_TOPLEVEL_BOUNDARY)
    exec_at_scope = _function_scope(_HS_SIBLINGS_ONE_INSTRUMENTED,
                                     r"^craftExecuteAtFn\s*∷", HASKELL_TOPLEVEL_BOUNDARY)
    expect("one instrumented: craftExecuteFn detected",
           bool(exec_scope and re.search(r"pushActionOutcome", exec_scope)), True)
    expect("one instrumented: craftExecuteAtFn NOT falsely detected",
           bool(exec_at_scope and re.search(r"pushActionOutcome", exec_at_scope)), False)

    # 4. The definition-vs-call distinction for recordClick/
    #    recordWidgetClickOutcome: a lone definition (no calls) must not
    #    read as instrumented.
    def_only = 'local function recordClick(handler, outcome, x, y, reason)\n    debug.recordOutcome{kind="input.click"}\nend\n'
    expect("recordClick definition alone is not a call site",
           len(re.findall(r"recordClick\((?:\"|nil)", def_only)) > 0, False)
    with_call = def_only + '\nrecordClick("build_tool", nil, x, y)\n'
    expect("recordClick with a real call site is detected",
           len(re.findall(r"recordClick\((?:\"|nil)", with_call)) > 0, True)

    # 5. Same def-vs-call distinction for the Haskell/Lua helper-wrapper
    #    patterns used by the two Layer A "A" entries above.
    hs_def_only = ('recordWidgetClickOutcome ∷ EngineEnv → Text → Text → IO ()\n'
                   'recordWidgetClickOutcome env kind callbackName = do\n    pure ()\n')
    expect("recordWidgetClickOutcome definition alone is not a call site",
           bool(re.search(r'recordWidgetClickOutcome env "', hs_def_only)), False)
    hs_with_call = hs_def_only + '\nrecordWidgetClickOutcome env "input.click" callback\n'
    expect("recordWidgetClickOutcome with a real call site is detected",
           bool(re.search(r'recordWidgetClickOutcome env "', hs_with_call)), True)

    route_def_only = ('recordRouteOutcome ∷ Text → Maybe Text → IO ()\n'
                       'recordRouteOutcome outcome handler = do\n    pure ()\n')
    expect("recordRouteOutcome definition alone is not a call site",
           bool(re.search(r'recordRouteOutcome "', route_def_only)), False)
    route_with_call = route_def_only + '\nrecordRouteOutcome "accepted" (Just "x")\n'
    expect("recordRouteOutcome with a real call site is detected",
           bool(re.search(r'recordRouteOutcome "', route_with_call)), True)

    # 6. The multi-route areas review round 2 found: prove that dropping
    #    ANY ONE required route/reason literal flips the check to False
    #    — not just that having all of them reads True. Built from
    #    realistic CALL-SHAPED fixtures (not bare literals) so a
    #    call-renaming mutation (review round 5) is provably caught too.
    _ROUTE_NAMES = [
        "degenerate_viewport", "tooltip_lock_toggle", "ui_surface_block",
        "camera_drag", "tooltip_lock_dismiss", "unmapped_button",
    ]
    # The no-right-click-handler route (review round 8): NOT a quoted
    # literal, since it records the control's own callback variable.
    _LEFT_CLICK_CALLBACK_ROUTE = "leftClickCallback"
    _HANDLER_NAMES = [
        "debug_overlay", "debug_anim_panel", "build_tool", "mine_tool",
        "chop_tool", "till_tool", "plant_tool", "unit_select", "item_select",
        "building_select", "deselect", "context_menu_building",
        "context_menu_unit", "context_menu_item", "move_order",
        "context_menu_tile",
    ]

    def swallowed_routes_fixture(include: set[str], with_left_click_route: bool,
                                  call_name: str = "recordRouteOutcome") -> str:
        lines = [f"{call_name} ∷ Text → Maybe Text → IO ()",
                  f"{call_name} outcome handler = do pure ()"]
        for name in _ROUTE_NAMES:
            if name in include:
                lines.append(f'{call_name} "accepted" (Just "{name}")')
        if with_left_click_route:
            lines.append(f'{call_name} "accepted" (Just {_LEFT_CLICK_CALLBACK_ROUTE})')
        return "\n".join(lines)

    swallowed_full = swallowed_routes_fixture(set(_ROUTE_NAMES), True)
    expect("Layer A swallowed routes: all present reads DONE",
           _all_present(swallowed_full, LAYER_A_SWALLOWED_ROUTES), True)
    for name in _ROUTE_NAMES:
        missing_one = swallowed_routes_fixture(set(_ROUTE_NAMES) - {name}, True)
        expect(f"Layer A swallowed routes: missing {name!r} reads gap",
               _all_present(missing_one, LAYER_A_SWALLOWED_ROUTES), False)
    missing_left_click_route = swallowed_routes_fixture(set(_ROUTE_NAMES), False)
    expect("Layer A swallowed routes: missing the no-right-click-handler "
           "route (review round 8) reads gap",
           _all_present(missing_left_click_route, LAYER_A_SWALLOWED_ROUTES), False)
    renamed_call = swallowed_routes_fixture(set(_ROUTE_NAMES), True, call_name="someOtherHelper")
    expect("Layer A swallowed routes: recordRouteOutcome renamed away "
           "(literals kept) reads as a gap (review round 5)",
           _all_present(renamed_call, LAYER_A_SWALLOWED_ROUTES), False)

    def game_chain_fixture(include: set[str], deadclick_reasons: list[str],
                            call_name: str = "recordClick") -> str:
        lines = [f'local function {call_name}(handler, outcome, x, y, reason)',
                  '    debug.recordOutcome{kind = "input.click"}', 'end']
        for name in _HANDLER_NAMES:
            if name in include:
                lines.append(f'{call_name}("{name}", nil, x, y)')
        for reason in deadclick_reasons:
            lines.append(f'{call_name}(nil, "deadclick", x, y, "{reason}")')
        return "\n".join(lines)

    # The three real deadclick sites: both inactive-gameplay gates share
    # identical text (they really are the same call duplicated in
    # scripts/init_mouse.lua), the off-world tile-menu miss (review round
    # 6) has its own distinguishing reason.
    _INACTIVE_GAMEPLAY = "gameplay input inactive (menu/paused/hidden world)"
    _REALISTIC_DEADCLICKS = [_INACTIVE_GAMEPLAY, _INACTIVE_GAMEPLAY,
                              "no tile under cursor"]

    chain_full = game_chain_fixture(set(_HANDLER_NAMES), _REALISTIC_DEADCLICKS)
    expect("game chain: all handlers + all 3 real deadclick sites reads DONE",
           _game_chain_check(chain_full), True)
    for name in _HANDLER_NAMES:
        missing_one = game_chain_fixture(set(_HANDLER_NAMES) - {name}, _REALISTIC_DEADCLICKS)
        expect(f"game chain: missing handler {name!r} reads gap",
               _game_chain_check(missing_one), False)
    for n in (0, 1, 2):
        expect(f"game chain: only {n} of 3 real deadclick sites reads gap",
               _game_chain_check(
                   game_chain_fixture(set(_HANDLER_NAMES), _REALISTIC_DEADCLICKS[:n])),
               False)
    # The review-round-6 exact blocker: enough generic deadclicks to pass
    # a plain count check, but NONE for the off-world tile-menu miss
    # specifically — must still read as a gap.
    generic_only = game_chain_fixture(set(_HANDLER_NAMES), [_INACTIVE_GAMEPLAY] * 4)
    expect("game chain: 4 generic deadclicks but none for the off-world "
           "tile-menu miss reads gap (review round 6)",
           _game_chain_check(generic_only), False)
    chain_renamed_call = game_chain_fixture(
        set(_HANDLER_NAMES), _REALISTIC_DEADCLICKS, call_name="someOtherFn")
    expect("game chain: recordClick renamed away (literals kept) reads "
           "as a gap (review round 5)",
           _game_chain_check(chain_renamed_call), False)

    # buildTool.commitPlacement: realistic fixture text (function-scoped,
    # hook-anchored `reason =`/`outcome = ` fields) rather than bare
    # literals, so removing a HOOK (not just a word) is what's tested —
    # review round 3's exact ask.
    def commit_placement_fn(include: set[str]) -> str:
        lines = ["function buildTool.commitPlacement(defName, gx, gy)"]
        if "power" in include:
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = "not a placeable power item: " .. tostring(defName)}')
        if "carrier" in include:
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = "no selected unit carries " .. tostring(defName)}')
        if "node" in include:
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = tostring(buildingIdOrErr)}')
        if "accepted" in include:
            lines.append('    debug.recordOutcome{outcome = "accepted"}')
        lines.append("end")
        return "\n".join(lines)

    def handle_mouse_down_fn(include: set[str], designate_sites: int,
                             no_world_sites: int) -> str:
        lines = ["function buildTool.handleMouseDown(button, x, y)"]
        if "offworld" in include:
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = "off-world click during placement"}')
        if "invalid" in include:
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = "invalid placement tile"}')
        if "spawn" in include or "portal_accepted" in include:
            # Real if/else/end shape (not a flat sequence) — the
            # portal-accepted anchor pattern is bounded by a negative
            # lookahead on "else", so the fixture needs the same
            # structure the real code has for that bound to mean
            # anything (review round 8's window-based predecessor
            # bridged past "building.spawn failed" into the NEXT
            # unrelated accepted call in a flatter fixture).
            lines.append('    local id = building.spawn(target.def, igx, igy)')
            lines.append('    if id then')
            if "portal_accepted" in include:
                lines.append('        debug.recordOutcome{outcome = "accepted"}')
            lines.append('    else')
            if "spawn" in include:
                lines.append('        debug.recordOutcome{outcome = "rejected", '
                              'reason = "building.spawn failed"}')
            lines.append('    end')
        for _ in range(designate_sites):
            lines.append('    debug.recordOutcome{outcome = "accepted", '
                          'reason = "routed to construction.designate"}')
        for _ in range(no_world_sites):
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = "no active world id"}')
        lines.append("end")
        return "\n".join(lines)

    all_commit_parts = {"power", "carrier", "node", "accepted"}
    all_handle_parts = {"offworld", "invalid", "spawn", "portal_accepted"}
    full_fixture = (commit_placement_fn(all_commit_parts) + "\n"
                    + handle_mouse_down_fn(all_handle_parts, 2, 2))
    expect("buildTool.commitPlacement: all hooks present reads DONE",
           _build_tool_check(full_fixture), True)
    missing_portal_accepted = (
        commit_placement_fn(all_commit_parts) + "\n"
        + handle_mouse_down_fn(all_handle_parts - {"portal_accepted"}, 2, 2))
    expect("buildTool.commitPlacement: missing the portal-accepted hook "
           "reads gap (review round 8)",
           _build_tool_check(missing_portal_accepted), False)

    # The exact review-round-3 counter-example: the function bodies keep
    # their ordinary `return nil, "..."` values but EVERY
    # debug.recordOutcome hook is deleted. Must read as a gap, not DONE.
    no_hooks_fixture = (
        'function buildTool.commitPlacement(defName, gx, gy)\n'
        '    if not power.isPlaceable(defName) then\n'
        '        return nil, "not a placeable power item"\n'
        '    end\n'
        '    return nil, "no selected unit carries " .. defName\n'
        'end\n'
        'function buildTool.handleMouseDown(button, x, y)\n'
        '    if not gx or not gy then return true end\n'
        'end\n')
    expect("buildTool.commitPlacement: return-value strings with NO "
           "recordOutcome hooks read as a gap (review round 3)",
           _build_tool_check(no_hooks_fixture), False)

    # The exact review-round-4 counter-example: every `debug.recordOutcome`
    # CALL renamed away (to some unrelated table constructor) while its
    # `{outcome = ..., reason = ...}` fields are left completely intact.
    # A field-only check (no anchor to the call itself) still reads DONE
    # here; requiring the call is what review round 4 asked for.
    call_renamed_fixture = (
        full_fixture.replace("debug.recordOutcome", "someOtherTableCtor"))
    expect("buildTool.commitPlacement: recordOutcome call renamed away "
           "(fields kept) reads as a gap (review round 4)",
           _build_tool_check(call_renamed_fixture), False)

    for missing in all_commit_parts:
        variant = (commit_placement_fn(all_commit_parts - {missing}) + "\n"
                   + handle_mouse_down_fn(all_handle_parts, 2, 2))
        expect(f"buildTool.commitPlacement: missing the {missing!r} hook reads gap",
               _build_tool_check(variant), False)
    for missing in all_handle_parts:
        variant = (commit_placement_fn(all_commit_parts) + "\n"
                   + handle_mouse_down_fn(all_handle_parts - {missing}, 2, 2))
        expect(f"buildTool.commitPlacement: missing the {missing!r} hook reads gap",
               _build_tool_check(variant), False)
    only_one_designate = (commit_placement_fn(all_commit_parts) + "\n"
                           + handle_mouse_down_fn(all_handle_parts, 1, 2))
    expect("buildTool.commitPlacement: only ONE construction.designate "
           "hook (of two call sites) reads gap",
           _build_tool_check(only_one_designate), False)
    only_one_no_world = (commit_placement_fn(all_commit_parts) + "\n"
                          + handle_mouse_down_fn(all_handle_parts, 2, 1))
    expect("buildTool.commitPlacement: only ONE 'no active world id' hook "
           "(of two call sites) reads gap",
           _build_tool_check(only_one_no_world), False)

    return failures


def main_self_test() -> int:
    failures = _self_test()
    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for f in failures:
            print(f"  FAIL: {f}")
        return 1
    print("action_outcome_coverage.py self-test: all checks passed")
    return 0


if __name__ == "__main__":
    if "--self-test" in sys.argv:
        raise SystemExit(main_self_test())
    raise SystemExit(main())
