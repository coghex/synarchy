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

# #730: the non-click Layer A families' producer calls, anchored to the
# EXACT positional argument sequence real source uses (not a lazy window)
# — each call's outcome/domain string literals sit immediately after the
# call name with no other field in between, so an exact sequence is both
# more precise than a window and immune to a window wide enough to bridge
# past a renamed call into an unrelated sibling's leftover literal.
_KEY_OUTCOME    = r"recordKeyOutcome\s+env\s+"          # Engine.Input.Thread
_CHAR_ACC_TRUE  = r'accumulateCharOutcome\s+inpSt\s+True\s+'   # ditto
_CHAR_ACC_FALSE = r'accumulateCharOutcome\s+inpSt\s+False\s+' # ditto
_SCROLL_OUTCOME = r"recordScrollOutcome\s+"             # ditto
_DRAG_OUTCOME   = r"recordDragOutcome\("                # unit_drag_select.lua

LAYER_A_KEY_DOMAINS = [
    _KEY_OUTCOME + r'"shell_text"', _KEY_OUTCOME + r'"ui_text"',
    _KEY_OUTCOME + r'"gameplay_key"',
]
LAYER_A_CHAR_DOMAINS = [
    _CHAR_ACC_TRUE + r'"shell_text"', _CHAR_ACC_TRUE + r'"ui_text"',
    _CHAR_ACC_FALSE + r'"dropped_backtick"',
    _CHAR_ACC_FALSE + r'"dropped_unfocused"',
]
LAYER_A_SCROLL_DOMAINS = [
    _SCROLL_OUTCOME + r'"accepted"\s*"z_slice"',
    _SCROLL_OUTCOME + r'"accepted"\s*"ui_scroll"',
    _SCROLL_OUTCOME + r'"accepted"\s*"game_scroll"',
    _SCROLL_OUTCOME + r'"noop"\s*"degenerate_viewport"',
]
LAYER_A_DRAG_OUTCOMES = [
    _DRAG_OUTCOME + r'#final > 0 and "accepted" or "noop"',
    _DRAG_OUTCOME + r'"noop"[\s\S]{0,80}?"release swallowed',
    # #730 review round 4: right-button game-world drags (context
    # menus / move-order / deadclick chain) have no box-selection
    # effect, so a real drag is an honest noop rather than a fake
    # "accepted".
    _DRAG_OUTCOME + r'"noop"[\s\S]{0,80}?"no drag gesture is defined for right-button',
]

# #730 review rounds 2 & 3: a ClickUI-routed press (real UI widget
# click OR an H1 drag that starts on one) OR a middle-button press
# (camera-drag) is deferred to its matching release — 'Engine.Input.
# Thread' stashes (kind, callback, press-x, press-y) via writeIORef
# pendingUIClickRef at EACH of the four producing sites (left-click
# hit, right-click hit, right-click-consumed-by-a-left-only-control,
# middle-button camera_drag), then resolves exactly one outcome at
# release by comparing movement against uiDragThresholdPx — OR, if
# focus loss/minimize swallows the release entirely (round 3),
# 'Engine.Input.State.releaseHeldButtons' resolves it as an
# interrupted noop instead of losing it silently. Anchored to the
# actual call/comparison, not bare literals, same convention as
# _ROC_ROUTE.
_PENDING_UI_CLICK = r'writeIORef pendingUIClickRef[\s\S]{0,60}?'


def _ui_click_deferred_check(thread_text: str, state_text: str) -> bool:
    if not thread_text or not state_text:
        return False
    return (
        # THREE sites push a leading "input.click" (the left-click hit,
        # the right-click-consumed-by-left-only-control fallback, AND
        # the middle-button camera_drag site below) — a count, not a
        # bare _all_present, so losing any one of them still reads as
        # a gap (mirrors the game-chain deadclick count above).
        _count_at_least(thread_text, _PENDING_UI_CLICK + r'"input\.click"', 3)
        and bool(re.search(_PENDING_UI_CLICK + r'"input\.rightClick"', thread_text))
        and bool(re.search(
            _PENDING_UI_CLICK + r'"input\.click",\s*"camera_drag"', thread_text))
        and bool(re.search(
            r'movedPx\s*≥\s*uiDragThresholdPx[\s\S]{0,80}?"input\.drag"'
            r'[\s\S]{0,80}?else\s*\(clickKind', thread_text))
        and bool(re.search(
            r'pushActionOutcome[\s\S]{0,220}?aoOutcome\s*=\s*"noop"'
            r'[\s\S]{0,260}?"release swallowed \(focus loss / minimize\)"',
            state_text)))

# Shared source of truth between the real checks below and the self-test:
# every distinct route/reason literal a multi-route area is expected to
# carry. Defined once so the self-test proving "remove one -> gap" can't
# silently drift from what main() actually checks (review round 2).
LAYER_A_SWALLOWED_ROUTES = [
    _ROC_ROUTE + r'"degenerate_viewport"', _ROC_ROUTE + r'"tooltip_lock_toggle"',
    _ROC_ROUTE + r'"ui_surface_block"',
    _ROC_ROUTE + r'"tooltip_lock_dismiss"',
    _ROC_ROUTE + r'"unmapped_button"',  # GLFW buttons 4-8, mapped to Lua button 0
    # NB: "camera_drag" (middle-button) and the no-right-click-handler
    # route (review round 8) used to live here as immediate
    # recordRouteOutcome calls — #730 review rounds 2-3 moved BOTH to
    # the SAME deferred-to-release pendingUIClickRef mechanism as the
    # other ClickUI routes (a right-click OR middle-button press can
    # start an H1 drag too), so their coverage now lives in
    # _ui_click_deferred_check's "click_site2"/"camera_drag_site" parts
    # instead.
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


#   Haskell producers' raw pushActionOutcome call, bounded to its OWN
#   record literal (the run up to the closing `}`) rather than a fixed
#   character window: plant.designate's accepted/rejected calls sit
#   close together, and a wide-enough-for-real-source window bridged
#   from one call's `pushActionOutcome` token straight through a
#   RENAMED sibling call's leftover `aoOutcome = "..."` field, reading
#   DONE even with that sibling's own call renamed away (review round
#   10 — same window-bridging bug class as the portal-accepted check
#   below, here in the generic Haskell-producer anchor instead of a
#   single hand-written one).
_PAO = r"pushActionOutcome(?:(?!\})[\s\S])*?"


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
_PORTAL_ACCEPTED_ANCHOR = r"building\.spawn\(target\.def, igx, igy\)"


def _portal_accepted_body(text: str) -> str | None:
    """Everything from the portal's building.spawn call up to (not
    including) the branch's own "else" — the whole `if id then ... else`
    body, not just the span up to outcome="accepted". Bounded the same
    structural way as _game_chain_check's routes: `(?:(?!else)[\s\S])*`
    can consume any text EXCEPT a run starting with the literal "else",
    so this is confined to the `if id then` branch regardless of field
    order or spacing inside it."""
    m = re.search(_PORTAL_ACCEPTED_ANCHOR + r"((?:(?!else)[\s\S])*)", text)
    return m.group(1) if m else None


def _portal_accepted_present(text: str) -> bool:
    """The portal-accepted hook has no distinguishing reason text
    (success carries no reason at all), so it's anchored to
    `building.spawn(...)` — unique to the portal branch — rather than a
    reason literal (review round 8: neither the plain outcome="accepted"
    text nor the "building.spawn failed" reject reason alone proved this
    specific hook, since other accepted/rejected calls in the same
    function already satisfy those). Requires outcome="accepted" to
    appear inside an actual `debug.recordOutcome{...}` call (_ROC), not
    just anywhere in the branch body — review round 11: renaming ONLY
    the portal-success call to some other table constructor left the
    literal `outcome = "accepted"` text sitting right there in the
    body, which a check with no call anchor still happily matched."""
    body = _portal_accepted_body(text)
    return body is not None and bool(
        re.search(_ROC + r'outcome\s*=\s*"accepted"', body))


def _portal_accepted_omits_reason(text: str) -> bool:
    """The portal-accepted debug.recordOutcome{...} block itself must
    NOT also set a `reason` field anywhere in it — guards the exact
    review-round-7 bug class (`ok and nil or "constant"` always
    attaching a failure reason even on success) from resurfacing in
    this specific call. Checks the WHOLE branch body (see
    _portal_accepted_body), not just the text after the "accepted"
    match: a `reason` field reintroduced BEFORE `outcome = "accepted"`
    in the same record literal — a plausible field-reordering
    regression — is invisible to a check that only looks forward from
    the match (review round 10's exact counter-example)."""
    body = _portal_accepted_body(text)
    return body is not None and "reason" not in body


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
            and _portal_accepted_present(handle_scope)
            and _portal_accepted_omits_reason(handle_scope)
            and _count_at_least(
                handle_scope, _ROC + r'reason\s*=[^\n]*"routed to construction\.designate"', 2)
            and _count_at_least(
                handle_scope, _ROC + r'reason\s*=[^\n]*"no active world id"', 2))


# Each entry: (tier, verb, check) where check() -> bool. Built below so
# each verb's own check can pick file-wide vs function-scoped as needed.
def _build_verbs() -> list[tuple[str, str, callable]]:
    return [
        # --- Layer A: input routing, "complete" per the issue's scope note ---
        ("A", "input click -> UI/camera-drag consumption (deferred to release, #730 rounds 2-3)",
         # #730 review round 2 moved this from an immediate Dispatch.hs
         # record to a deferred Engine.Input.Thread one; round 3 added
         # the middle-button camera_drag site and the focus-loss/
         # minimize interrupted-release resolution in Engine.Input.State
         # (see _ui_click_deferred_check) — a UI-origin OR middle-button
         # H1 drag reads as exactly one "input.drag" instead of also
         # carrying a stale press-time "input.click", and a swallowed
         # release no longer loses the record outright.
         lambda: _ui_click_deferred_check(
             (REPO_ROOT / "src/Engine/Input/Thread.hs").read_text(encoding="utf-8")
             if (REPO_ROOT / "src/Engine/Input/Thread.hs").exists() else "",
             (REPO_ROOT / "src/Engine/Input/State.hs").read_text(encoding="utf-8")
             if (REPO_ROOT / "src/Engine/Input/State.hs").exists() else "")),
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
        # --- Layer A (#730): non-click H1 input families — keyboard, text,
        # scroll/z-slice, drag. Each area's `_all_present_check` requires
        # EVERY registered domain literal, not just one, mirroring the
        # click area's multi-route pattern above. ---
        ("A", "input key -> shell/UI-text/gameplay routing domains",
         lambda: _all_present_check(
             "src/Engine/Input/Thread.hs", LAYER_A_KEY_DOMAINS)),
        ("A", "input type/char -> text-delivery + drop domains (aggregated)",
         lambda: _all_present_check(
             "src/Engine/Input/Thread.hs", LAYER_A_CHAR_DOMAINS)),
        ("A", "input scroll -> z-slice/UI-scroll/game-scroll/degenerate domains",
         lambda: _all_present_check(
             "src/Engine/Input/Thread.hs", LAYER_A_SCROLL_DOMAINS)),
        ("A", "input drag -> unit_drag_select box-selection outcome",
         lambda: _all_present_check(
             "scripts/unit_drag_select.lua", LAYER_A_DRAG_OUTCOMES)),

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
         # differs), so a bare field-presence check reads DONE even with
         # the accepted branch's pushActionOutcome call itself renamed
         # away (review round 9 — same class of bug as build-tool's
         # portal-accepted hook) — anchor both outcomes to an actual
         # pushActionOutcome call, not just the record fields.
         lambda: _all_present_check(
             "src/World/Thread/Command/Cursor/Plant.hs",
             [r'aoKind\s*=\s*"plant\.designate"',
              _PAO + r'aoOutcome\s*=\s*"accepted"',
              _PAO + r'aoOutcome\s*=\s*"rejected"',
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
        "tooltip_lock_dismiss", "unmapped_button",
    ]
    _HANDLER_NAMES = [
        "debug_overlay", "debug_anim_panel", "build_tool", "mine_tool",
        "chop_tool", "till_tool", "plant_tool", "unit_select", "item_select",
        "building_select", "deselect", "context_menu_building",
        "context_menu_unit", "context_menu_item", "move_order",
        "context_menu_tile",
    ]

    def swallowed_routes_fixture(include: set[str],
                                  call_name: str = "recordRouteOutcome") -> str:
        lines = [f"{call_name} ∷ Text → Maybe Text → IO ()",
                  f"{call_name} outcome handler = do pure ()"]
        for name in _ROUTE_NAMES:
            if name in include:
                lines.append(f'{call_name} "accepted" (Just "{name}")')
        return "\n".join(lines)

    swallowed_full = swallowed_routes_fixture(set(_ROUTE_NAMES))
    expect("Layer A swallowed routes: all present reads DONE",
           _all_present(swallowed_full, LAYER_A_SWALLOWED_ROUTES), True)
    for name in _ROUTE_NAMES:
        missing_one = swallowed_routes_fixture(set(_ROUTE_NAMES) - {name})
        expect(f"Layer A swallowed routes: missing {name!r} reads gap",
               _all_present(missing_one, LAYER_A_SWALLOWED_ROUTES), False)
    renamed_call = swallowed_routes_fixture(set(_ROUTE_NAMES), call_name="someOtherHelper")
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
                             no_world_sites: int,
                             portal_accepted_reason: str | None = None,
                             portal_accepted_reason_first: bool = False,
                             portal_accepted_call: str = "debug.recordOutcome") -> str:
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
                if portal_accepted_reason is None:
                    lines.append(f'        {portal_accepted_call}{{outcome = "accepted"}}')
                elif portal_accepted_reason_first:
                    # Same reason bug, but with the fields in the OTHER
                    # order — reason precedes outcome in the same record
                    # literal (review round 10's counter-example: a check
                    # that only looks forward from the outcome match
                    # never sees a reason placed before it).
                    lines.append(f'        {portal_accepted_call}{{reason = "{portal_accepted_reason}", '
                                  'outcome = "accepted"}')
                else:
                    # Review-round-7 bug reintroduced: a reason attached
                    # to the SUCCESS record.
                    lines.append(f'        {portal_accepted_call}{{outcome = "accepted", '
                                  f'reason = "{portal_accepted_reason}"}}')
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
    portal_accepted_with_reason = (
        commit_placement_fn(all_commit_parts) + "\n"
        + handle_mouse_down_fn(all_handle_parts, 2, 2,
                               portal_accepted_reason="building.spawn failed"))
    expect("buildTool.commitPlacement: a reason reintroduced on the "
           "portal-accepted record reads gap (review round 9 — the "
           "exact `ok and nil or ...` bug class)",
           _build_tool_check(portal_accepted_with_reason), False)
    portal_accepted_reason_before = (
        commit_placement_fn(all_commit_parts) + "\n"
        + handle_mouse_down_fn(all_handle_parts, 2, 2,
                               portal_accepted_reason="building.spawn failed",
                               portal_accepted_reason_first=True))
    expect("buildTool.commitPlacement: a reason reintroduced BEFORE "
           "outcome=\"accepted\" in the same record reads gap (review "
           "round 10 — field order previously evaded a check that only "
           "looked forward from the outcome match)",
           _build_tool_check(portal_accepted_reason_before), False)
    portal_accepted_call_renamed = (
        commit_placement_fn(all_commit_parts) + "\n"
        + handle_mouse_down_fn(all_handle_parts, 2, 2,
                                portal_accepted_call="someOtherTableCtor"))
    expect("buildTool.commitPlacement: ONLY the portal-success call "
           "renamed away (fields kept) reads gap (review round 11 — "
           "outcome=\"accepted\" alone, with no debug.recordOutcome "
           "anchor, previously still read DONE)",
           _build_tool_check(portal_accepted_call_renamed), False)

    # plant.designate: both branches share the same aoKind literal, so
    # anchor each aoOutcome to its own pushActionOutcome call (review
    # round 9 — renaming just the accepted branch's call previously
    # still read DONE).
    def plant_fixture(include: set[str], accepted_call: str = "pushActionOutcome",
                       rejected_call: str = "pushActionOutcome") -> str:
        lines = []
        if "accepted" in include:
            lines.append(f'{accepted_call} (actionOutcomeRef env) ActionOutcome\n'
                          '    {{ aoTs = gt, aoKind = "plant.designate"\n'
                          '    , aoOutcome = "accepted"\n    }}')
        if "rejected" in include:
            lines.append(f'{rejected_call} (actionOutcomeRef env) ActionOutcome\n'
                          '    {{ aoTs = gt, aoKind = "plant.designate"\n'
                          '    , aoOutcome = "rejected"\n    }}')
        if "missing_world" in include:
            lines.append('recordMissingWorldOutcome env "plant.designate" pageId gx gy')
        return "\n".join(lines)

    _PLANT_PATTERNS = [r'aoKind\s*=\s*"plant\.designate"',
                       _PAO + r'aoOutcome\s*=\s*"accepted"',
                       _PAO + r'aoOutcome\s*=\s*"rejected"',
                       r'recordMissingWorldOutcome env "plant\.designate"']
    _PLANT_PARTS = {"accepted", "rejected", "missing_world"}
    plant_full = plant_fixture(_PLANT_PARTS)
    expect("plant.designate: all hooks present reads DONE",
           _all_present(plant_full, _PLANT_PATTERNS), True)
    for missing in _PLANT_PARTS:
        expect(f"plant.designate: missing the {missing!r} hook reads gap",
               _all_present(plant_fixture(_PLANT_PARTS - {missing}), _PLANT_PATTERNS),
               False)
    plant_renamed = plant_fixture(_PLANT_PARTS, accepted_call="someOtherPushFn",
                                   rejected_call="someOtherPushFn")
    expect("plant.designate: BOTH producers renamed away (fields kept) "
           "reads as a gap (review round 9)",
           _all_present(plant_renamed, _PLANT_PATTERNS), False)
    # Isolate each hook individually (review round 10 — the round 9
    # self-test only ever renamed both calls together, so it never
    # proved the checker doesn't bridge from the ONE INTACT call's
    # pushActionOutcome token across to the OTHER, renamed call's
    # leftover aoOutcome field — the same window-bridging bug class the
    # portal-accepted check above was just hardened against).
    plant_only_accepted_renamed = plant_fixture(
        _PLANT_PARTS, accepted_call="someOtherPushFn")
    expect("plant.designate: only the accepted producer renamed (rejected "
           "intact) reads gap (review round 10)",
           _all_present(plant_only_accepted_renamed, _PLANT_PATTERNS), False)
    plant_only_rejected_renamed = plant_fixture(
        _PLANT_PARTS, rejected_call="someOtherPushFn")
    expect("plant.designate: only the rejected producer renamed (accepted "
           "intact) reads gap (review round 10)",
           _all_present(plant_only_rejected_renamed, _PLANT_PATTERNS), False)

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

    # #730: the non-click Layer A families (keyboard, char/type
    # aggregation, scroll/z-slice, drag) — same removal-mutation shape
    # as the click swallowed-routes block above: all domains present
    # reads DONE, missing any ONE domain reads gap, and the producer
    # call renamed away (domain literals left behind) reads gap.
    def key_outcome_fixture(include: set[str], call_name: str = "recordKeyOutcome") -> str:
        lines = [f"{call_name} ∷ EngineEnv → Text → Maybe Text → Maybe Word32 → IO ()",
                 f"{call_name} env domain matched target = pure ()"]
        for domain in ("shell_text", "ui_text", "gameplay_key"):
            if domain in include:
                lines.append(f'{call_name} env "{domain}" matched (Just fid)')
        return "\n".join(lines)

    _KEY_DOMAINS_SET = {"shell_text", "ui_text", "gameplay_key"}
    key_full = key_outcome_fixture(_KEY_DOMAINS_SET)
    expect("input key: all three routing domains present reads DONE",
           _all_present(key_full, LAYER_A_KEY_DOMAINS), True)
    for domain in _KEY_DOMAINS_SET:
        expect(f"input key: missing the {domain!r} domain reads gap",
               _all_present(key_outcome_fixture(_KEY_DOMAINS_SET - {domain}),
                             LAYER_A_KEY_DOMAINS), False)
    key_renamed = key_outcome_fixture(_KEY_DOMAINS_SET, call_name="someOtherKeyFn")
    expect("input key: recordKeyOutcome renamed away (literals kept) reads gap",
           _all_present(key_renamed, LAYER_A_KEY_DOMAINS), False)

    def char_outcome_fixture(include: set[str],
                              call_name: str = "accumulateCharOutcome") -> str:
        lines = [f"{call_name} ∷ InputState → Bool → Text → Maybe Word32 → InputState",
                 f"{call_name} inpSt applied domain target = inpSt"]
        if "shell_text" in include:
            lines.append(f'{call_name} inpSt True "shell_text" (Just fid)')
        if "ui_text" in include:
            lines.append(f'{call_name} inpSt True "ui_text" (Just eh)')
        if "dropped_backtick" in include:
            lines.append(f'{call_name} inpSt False "dropped_backtick" Nothing')
        if "dropped_unfocused" in include:
            lines.append(f'{call_name} inpSt False "dropped_unfocused" Nothing')
        return "\n".join(lines)

    _CHAR_DOMAINS_SET = {"shell_text", "ui_text", "dropped_backtick", "dropped_unfocused"}
    char_full = char_outcome_fixture(_CHAR_DOMAINS_SET)
    expect("input type/char: all four domains present reads DONE",
           _all_present(char_full, LAYER_A_CHAR_DOMAINS), True)
    for domain in _CHAR_DOMAINS_SET:
        expect(f"input type/char: missing the {domain!r} domain reads gap",
               _all_present(char_outcome_fixture(_CHAR_DOMAINS_SET - {domain}),
                             LAYER_A_CHAR_DOMAINS), False)
    char_renamed = char_outcome_fixture(_CHAR_DOMAINS_SET, call_name="someOtherCharFn")
    expect("input type/char: accumulateCharOutcome renamed away (literals "
           "kept) reads gap",
           _all_present(char_renamed, LAYER_A_CHAR_DOMAINS), False)

    def scroll_outcome_fixture(include: set[str],
                                call_name: str = "recordScrollOutcome") -> str:
        lines = [f"{call_name} ∷ Text → Text → Maybe Word32 → IO ()",
                 f"{call_name} outcome domain target = pure ()"]
        combos = {
            "z_slice": '"accepted" "z_slice" Nothing',
            "ui_scroll": '"accepted" "ui_scroll" (Just eh)',
            "game_scroll": '"accepted" "game_scroll" Nothing',
            "degenerate_viewport": '"noop" "degenerate_viewport" Nothing',
        }
        for name, argtext in combos.items():
            if name in include:
                lines.append(f'{call_name} {argtext}')
        return "\n".join(lines)

    _SCROLL_DOMAINS_SET = {"z_slice", "ui_scroll", "game_scroll", "degenerate_viewport"}
    scroll_full = scroll_outcome_fixture(_SCROLL_DOMAINS_SET)
    expect("input scroll: all four domains present reads DONE",
           _all_present(scroll_full, LAYER_A_SCROLL_DOMAINS), True)
    for domain in _SCROLL_DOMAINS_SET:
        expect(f"input scroll: missing the {domain!r} domain reads gap",
               _all_present(scroll_outcome_fixture(_SCROLL_DOMAINS_SET - {domain}),
                             LAYER_A_SCROLL_DOMAINS), False)
    scroll_renamed = scroll_outcome_fixture(_SCROLL_DOMAINS_SET, call_name="someOtherScrollFn")
    expect("input scroll: recordScrollOutcome renamed away (literals kept) "
           "reads gap",
           _all_present(scroll_renamed, LAYER_A_SCROLL_DOMAINS), False)

    def drag_outcome_fixture(include: set[str],
                              call_name: str = "recordDragOutcome") -> str:
        lines = [f"local function {call_name}(outcome, x, y, requested, applied, reason)",
                 '    debug.recordOutcome{kind = "input.drag"}', "end"]
        if "completed" in include:
            lines.append(
                f'{call_name}(#final > 0 and "accepted" or "noop", x, y, #ids, #final)')
        if "swallowed" in include:
            lines.append(
                f'{call_name}("noop", x, y, 0, 0, '
                '"release swallowed (focus loss / minimize)")')
        if "right_undefined" in include:
            lines.append(
                f'{call_name}("noop", x, y, 0, 0, '
                '"no drag gesture is defined for right-button game-world input")')
        return "\n".join(lines)

    _DRAG_PARTS = {"completed", "swallowed", "right_undefined"}
    drag_full = drag_outcome_fixture(_DRAG_PARTS)
    expect("input drag: all three outcome call sites present reads DONE",
           _all_present(drag_full, LAYER_A_DRAG_OUTCOMES), True)
    for part in _DRAG_PARTS:
        expect(f"input drag: missing the {part!r} call site reads gap",
               _all_present(drag_outcome_fixture(_DRAG_PARTS - {part}),
                             LAYER_A_DRAG_OUTCOMES), False)
    drag_renamed = drag_outcome_fixture(_DRAG_PARTS, call_name="someOtherDragFn")
    expect("input drag: recordDragOutcome renamed away (literals kept) reads gap",
           _all_present(drag_renamed, LAYER_A_DRAG_OUTCOMES), False)

    # #730 review rounds 2 & 3: the deferred-to-release UI/camera-drag
    # click classification — the "input.click" push sites (left-click
    # hit, right-click-consumed-by-left-only-control), the
    # "input.rightClick" site, the middle-button "camera_drag" site,
    # the release-side threshold comparison that picks between them
    # and "input.drag" (all in Engine.Input.Thread, `thread_include`),
    # and the interrupted-release resolution in Engine.Input.State
    # (`state_include`) for a focus-loss/minimize swallow.
    def ui_click_deferred_fixture(thread_include: set[str],
                                   pending_call: str = "writeIORef pendingUIClickRef") -> str:
        lines = []
        if "click_site1" in thread_include:
            lines.append(f'{pending_call} (Just ("input.click", callback, x, y))')
        if "rightclick_site" in thread_include:
            lines.append(f'{pending_call} (Just ("input.rightClick", callback, x, y))')
        if "click_site2" in thread_include:
            lines.append(f'{pending_call} (Just ("input.click", leftClickCallback, x, y))')
        if "camera_drag_site" in thread_include:
            lines.append(f'{pending_call} (Just ("input.click", "camera_drag", x, y))')
        if "release_classify" in thread_include:
            lines.append(
                'if movedPx ≥ uiDragThresholdPx then ("input.drag", x, y) '
                'else (clickKind, px, py)')
        return "\n".join(lines)

    def ui_click_interrupted_fixture(include: set[str],
                                      push_call: str = "pushActionOutcome") -> str:
        if "interrupted" not in include:
            return ""
        return (f'{push_call} (actionOutcomeRef env) ActionOutcome\n'
                '    { aoOutcome = "noop"\n'
                '    , aoReason = Just "release swallowed (focus loss / minimize)"\n'
                '    }')

    _UI_CLICK_THREAD_PARTS = {
        "click_site1", "rightclick_site", "click_site2",
        "camera_drag_site", "release_classify",
    }
    _UI_CLICK_STATE_PARTS = {"interrupted"}
    ui_click_full = ui_click_deferred_fixture(_UI_CLICK_THREAD_PARTS)
    ui_interrupted_full = ui_click_interrupted_fixture(_UI_CLICK_STATE_PARTS)
    expect("input click UI deferral: all parts present reads DONE",
           _ui_click_deferred_check(ui_click_full, ui_interrupted_full), True)
    for part in _UI_CLICK_THREAD_PARTS:
        expect(f"input click UI deferral: missing {part!r} reads gap",
               _ui_click_deferred_check(
                   ui_click_deferred_fixture(_UI_CLICK_THREAD_PARTS - {part}),
                   ui_interrupted_full),
               False)
    expect("input click UI deferral: missing the interrupted-release "
           "resolution (review round 3) reads gap",
           _ui_click_deferred_check(ui_click_full, ui_click_interrupted_fixture(set())),
           False)
    # Only ONE of the two "input.click" push sites present must still
    # read as a gap (a count, not a bare presence check).
    expect("input click UI deferral: only one of two 'input.click' sites reads gap",
           _ui_click_deferred_check(
               ui_click_deferred_fixture(
                   {"click_site1", "rightclick_site", "camera_drag_site", "release_classify"}),
               ui_interrupted_full),
           False)
    ui_click_renamed = ui_click_deferred_fixture(
        _UI_CLICK_THREAD_PARTS, pending_call="someOtherPendingWrite")
    expect("input click UI deferral: pendingUIClickRef write renamed away "
           "(literals kept) reads gap",
           _ui_click_deferred_check(ui_click_renamed, ui_interrupted_full), False)
    ui_interrupted_renamed = ui_click_interrupted_fixture(
        _UI_CLICK_STATE_PARTS, push_call="someOtherPushFn")
    expect("input click UI deferral: interrupted-release pushActionOutcome "
           "renamed away (literals kept) reads gap",
           _ui_click_deferred_check(ui_click_full, ui_interrupted_renamed), False)

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
