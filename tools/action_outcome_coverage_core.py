"""F4 (#646) action-outcome coverage: the production audit owner.

Everything `tools/action_outcome_coverage.py` needs to read the tree
lives here — repository-root resolution, the Lua/Haskell function-scope
extraction, the file-wide / all-present / count predicates, the Layer A
producer paths and call-anchored patterns, the deferred-click,
swallowed-route, game-chain, portal and build-tool checks, the complete
tier/verb/source/check registry, coverage-result evaluation, and the
Tier 1 mapping/instrumentation verification (#1704). The comments beside
each pattern carry the review round or issue that made it load-bearing.

Three interfaces are this module's contract with the command facade
(#2149):

  * `build_verbs()` — the ordered inventory of coverage entries;
  * `evaluate_coverage(entries)` — the plain report's per-entry verdicts;
  * `verify_tier1(entries)` — the blocking Tier 1 policy over a supplied
    inventory, returning the area total beside the problem list so the
    facade formats numbers it did not derive.

This module prints nothing and exits nowhere. It imports neither the
facade nor `tools/action_outcome_coverage_selftest.py`; importing it
runs no check and reads no file, because the registry stays inside
`build_verbs()` and every predicate is only evaluated on demand. The
synthetic mutation corpus that proves these predicates discriminate is
the self-test module, which reaches them by import — so every producer
path, regex contract and registry entry has exactly one authoritative
definition, here.

Usage: `tools/action_outcome_coverage.py`'s docstring and `tools/README.md`.
"""
from __future__ import annotations

import re
from collections.abc import Callable
from pathlib import Path
from typing import NamedTuple

# (tier, verb, paths, check): `check()` reads exactly the repo-relative
# files `paths` names and answers whether every required producer shape
# is present. See `build_verbs` for why `paths` is load-bearing.
CoverageEntry = tuple[str, str, list[str], Callable[[], bool]]

REPO_ROOT = Path(__file__).resolve().parent.parent

LUA_FUNCTION_BOUNDARY = r"^function "
HASKELL_TOPLEVEL_BOUNDARY = "^\\w+\\s*∷"  # '∷' marks a top-level type signature


def function_scope(text: str, start_pattern: str, boundary_pattern: str) -> str | None:
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
    scope = function_scope(path.read_text(encoding="utf-8"),
                             start_pattern, boundary_pattern)
    return scope is not None and re.search(instrument_pattern, scope) is not None


def _filewide_check(relpath: str, pattern: str) -> bool:
    path = REPO_ROOT / relpath
    return path.exists() and re.search(
        pattern, path.read_text(encoding="utf-8")) is not None


def all_present(text: str, patterns: list[str]) -> bool:
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
    return all_present(path.read_text(encoding="utf-8"), patterns)


def count_at_least(text: str, pattern: str, n: int) -> bool:
    return len(re.findall(pattern, text)) >= n


def _read(relpath: str) -> str:
    """The file's text, or "" when it does not exist. Every Layer A
    check treats "" as a gap, and `--verify-tier1` reports the absence
    separately and more precisely (#1704) — a moved producer is a
    stranded MAPPING, not missing instrumentation, and the two need
    different repairs."""
    path = REPO_ROOT / relpath
    return path.read_text(encoding="utf-8") if path.exists() else ""


# #1704: the modules that really own each Layer A family's producers
# today. #787 moved every one of them out of 'Engine.Input.Thread',
# which is now a 98-line thread-lifecycle facade (see its own header
# comment) that emits no outcome at all; #1676 then split the deferred
# gesture's press-capture/release-resolution pair out of Mouse.hs into
# 'Engine.Input.Thread.Mouse.Deferred'. Named as constants so the
# mapping is one declaration per family, reachable by --verify-tier1
# rather than buried inside a lambda.
INPUT_MOUSE = "src/Engine/Input/Thread/Mouse.hs"
INPUT_MOUSE_DEFERRED = "src/Engine/Input/Thread/Mouse/Deferred.hs"
INPUT_STATE = "src/Engine/Input/State.hs"
INPUT_KEYBOARD = "src/Engine/Input/Thread/Keyboard.hs"
INPUT_CHAR = "src/Engine/Input/Thread/Char.hs"
INPUT_SCROLL = "src/Engine/Input/Thread/Scroll.hs"


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
# lazy window before the literal it's checking, mirroring ROC's fix.
ROC_ROUTE = r"recordRouteOutcome[\s\S]{0,80}?"  # Thread.Mouse's helper
ROC_CLICK = r"recordClick\([\s\S]{0,60}?"       # init_mouse.lua's helper

# #730: the non-click Layer A families' producer calls, anchored to the
# EXACT positional argument sequence real source uses (not a lazy window)
# — each call's outcome/domain string literals sit immediately after the
# call name with no other field in between, so an exact sequence is both
# more precise than a window and immune to a window wide enough to bridge
# past a renamed call into an unrelated sibling's leftover literal.
_KEY_OUTCOME    = r"recordKeyOutcome\s+env\s+"          # Thread.Keyboard
_CHAR_ACC_TRUE  = r'accumulateCharOutcome\s+inpSt\s+True\s+'   # Thread.Char
_CHAR_ACC_FALSE = r'accumulateCharOutcome\s+inpSt\s+False\s+' # ditto
_SCROLL_OUTCOME = r"recordScrollOutcome\s+"             # Thread.Scroll
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
    # #1704: the fifth domain Scroll.hs really emits — a wheel event
    # consumed by a modal page's empty space (#742's modal boundary
    # swallows it rather than letting it fall through to the z-slice or
    # the camera). It was never in this list, so deleting its record
    # would not have registered as a Layer A gap.
    _SCROLL_OUTCOME + r'"noop"\s*"ui_modal_block"',
]
LAYER_A_DRAG_OUTCOMES = [
    _DRAG_OUTCOME + r'#final > 0 and "accepted" or "noop"',
    _DRAG_OUTCOME + r'"noop"[\s\S]{0,80}?"release swallowed',
    # #730 review round 4: right-button game-world drags (context
    # menus / move-order / deadclick chain) have no box-selection
    # effect, so a real drag is an honest noop rather than a fake
    # "accepted".
    _DRAG_OUTCOME + r'"noop"[\s\S]{0,80}?"no drag gesture is defined for right-button',
    # #730 review round 6: a left-button press that reached a debug/
    # build/mine/chop/till/plant tool claim (or the gameplay-inactive
    # deadclick) is armed for classification but never box-select-armed
    # — a real drag there is ALSO an honest noop, not a fake box-select
    # "accepted".
    _DRAG_OUTCOME + r'"noop"[\s\S]{0,80}?"no drag gesture is defined for this input"',
]

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
PAO = r"pushActionOutcome(?:(?!\})[\s\S])*?"


# #730 round 3's interrupted release, in 'Engine.Input.State': a press
# whose release never arrives (focus loss / minimize) still resolves to
# exactly one record instead of being silently dropped. Both the "noop"
# outcome and its reason must sit inside the SAME record literal —
# PAO's brace bound, not a character window, so the pattern is immune
# to however much commentary the record grows between the two fields
# (#1676 added eight lines of it, which is what a fixed window would
# have broken on).
_INTERRUPTED_RELEASE = (
    PAO + r'aoOutcome\s*=\s*"noop"' + r"(?:(?!\})[\s\S])*?"
    + r'"release swallowed \(focus loss / minimize\)"')

# #730 review rounds 2 & 3: a ClickUI-routed press (real UI widget
# click OR an H1 drag that starts on one) OR a middle-button press
# (camera-drag) is deferred to its matching release — 'Engine.Input.
# Thread.Mouse' stashes the press's kind/callback/position at each
# producing site, then exactly one outcome is resolved at release by
# comparing movement against uiDragThresholdPx — OR, if focus loss/
# minimize swallows the release entirely (round 3),
# 'Engine.Input.State.releaseHeldButtons' resolves it as an
# interrupted noop instead of losing it silently. Anchored to the
# actual call/comparison, not bare literals, same convention as
# ROC_ROUTE.
#
# #1676 replaced the four bare `writeIORef pendingUIClickRef` sites
# with a `deferPress kind callback` helper (the press's framebuffer
# position now has to be captured from PRESS-time geometry, which the
# helper closes over) and moved the release-side resolution into
# 'Engine.Input.Thread.Mouse.Deferred'. #743 had already turned the
# fourth site — a right-click consumed by a left-click-only control —
# into an IMMEDIATE `recordRouteOutcome "noop" (Just leftClickCallback)`,
# which is why the deferred sites now number three, not four, and why
# that route is checked in the swallowed-routes area below instead
# (#1704: this check still demanded three deferred "input.click" writes
# when only two have existed since #743).
_DEFER_PRESS = r"deferPress\s+"


def ui_click_deferred_check(mouse_text: str, deferred_text: str,
                              state_text: str) -> bool:
    if not mouse_text or not deferred_text or not state_text:
        return False
    return (
        # TWO sites defer a leading "input.click" — the left-click UI
        # hit and the middle-button camera_drag site below — a count,
        # not a bare all_present, so losing either one still reads as
        # a gap (mirrors the game-chain deadclick count above).
        count_at_least(mouse_text, _DEFER_PRESS + r'"input\.click"', 2)
        and bool(re.search(_DEFER_PRESS + r'"input\.rightClick"', mouse_text))
        and bool(re.search(
            _DEFER_PRESS + r'"input\.click"\s+"camera_drag"', mouse_text))
        # The release-side classification, in the module that owns it
        # since #1676. Bounded by a negative lookahead on the guard bar
        # rather than a character window: `(?:(?!\|)[\s\S])*?` cannot
        # cross out of the `| movedPx ≥ uiDragThresholdPx =` guard's own
        # alternative, so a window wide enough for real source can never
        # bridge into the NEXT guard's leftover literal (the exact
        # window-bridging bug class review rounds 10-11 found elsewhere
        # in this file).
        and bool(re.search(
            r'movedPx\s*≥\s*uiDragThresholdPx(?:(?!\|)[\s\S])*?"input\.drag"',
            deferred_text))
        # …and its below-threshold companion, which must keep replaying
        # the PRESS's own kind rather than inventing one.
        and bool(re.search(r'otherwise\s*=\s*\(pucKind\b', deferred_text))
        # A classification that reaches no recorder is not coverage:
        # require the resolved kind to actually be pushed, within ONE
        # record literal (PAO's brace-bounded anchor above).
        and bool(re.search(PAO + r'aoKind\s*=\s*kind\b', deferred_text))
        and bool(re.search(_INTERRUPTED_RELEASE, state_text)))

# Shared source of truth between the real checks below and the self-test:
# every distinct route/reason literal a multi-route area is expected to
# carry. Defined once so the self-test proving "remove one -> gap" can't
# silently drift from what the real checks actually use (review round 2).
LAYER_A_SWALLOWED_ROUTES = [
    ROC_ROUTE + r'"degenerate_viewport"', ROC_ROUTE + r'"tooltip_lock_toggle"',
    ROC_ROUTE + r'"ui_surface_block"',
    ROC_ROUTE + r'"tooltip_lock_dismiss"',
    ROC_ROUTE + r'"unmapped_button"',  # GLFW buttons 4-8, mapped to Lua button 0
    # #1704: the no-right-click-handler consumption — a right-click
    # eaten by an ordinary left-click-only control. It briefly lived on
    # the deferred path (#730 round 8), but #743 made it an IMMEDIATE
    # noop again, recorded under the consuming control's OWN left-click
    # callback name for diagnostic identity. It is therefore a
    # swallowed route once more, and was in NEITHER area's inventory in
    # between — deleting it registered nowhere. Its handler is a
    # BINDING, not a literal, so it is matched by name.
    ROC_ROUTE + r'\bleftClickCallback\b',
    # NB: "camera_drag" (middle-button) stays out of this list — #730
    # review round 3 moved it onto the SAME deferred-to-release
    # mechanism as the other ClickUI routes (a middle-button press can
    # start an H1 drag too), so its coverage lives in
    # ui_click_deferred_check's "camera_drag_site" part instead.
]

# #1704: "ui_pointer_block" is emitted at TWO independently
# load-bearing sites — #743's left-click RouteBlocked branch and its
# right-click twin — and either can be deleted alone. A bare presence
# check over the shared literal is satisfied by whichever one survives,
# so this area counts them, exactly as the deferred "input.click" sites
# and the game chain's deadclick sites are counted. Bump this when a
# genuinely new pointer-block branch is added.
LAYER_A_POINTER_BLOCK_ROUTE = ROC_ROUTE + r'"ui_pointer_block"'
LAYER_A_POINTER_BLOCK_SITES = 2


def swallowed_routes_check(text: str) -> bool:
    return (bool(text) and all_present(text, LAYER_A_SWALLOWED_ROUTES)
            and count_at_least(text, LAYER_A_POINTER_BLOCK_ROUTE,
                                LAYER_A_POINTER_BLOCK_SITES))


LAYER_A_GAME_CHAIN_HANDLERS = [
    ROC_CLICK + r'"debug_overlay"', ROC_CLICK + r'"debug_anim_panel"',
    ROC_CLICK + r'"build_tool"', ROC_CLICK + r'"mine_tool"',
    ROC_CLICK + r'"chop_tool"', ROC_CLICK + r'"till_tool"',
    ROC_CLICK + r'"plant_tool"', ROC_CLICK + r'"unit_select"',
    ROC_CLICK + r'"item_select"', ROC_CLICK + r'"building_select"',
    ROC_CLICK + r'"deselect"', ROC_CLICK + r'"context_menu_building"',
    ROC_CLICK + r'"context_menu_unit"', ROC_CLICK + r'"context_menu_item"',
    ROC_CLICK + r'"move_order"', ROC_CLICK + r'"context_menu_tile"',
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
ROC = r"debug\.recordOutcome\{[\s\S]{0,220}?"  # call open + bounded body
COMMIT_PLACEMENT_REQUIRED = [
    ROC + r'reason\s*=[^\n]*"not a placeable power item',
    ROC + r'reason\s*=[^\n]*"no selected unit carries',
    ROC + r"reason\s*=\s*tostring\(buildingIdOrErr\)",
    ROC + r'outcome\s*=\s*"accepted"',
]
HANDLE_MOUSE_DOWN_REQUIRED = [
    ROC + r'reason\s*=[^\n]*"off-world click during placement"',
    ROC + r'reason\s*=[^\n]*"invalid placement tile"',
]
# #1602 moved the portal's spawn — and therefore its accepted/rejected
# hooks — out of handleMouseDown's own scope into
# buildTool.commitStartingPlacement, which #779 had already extracted;
# these two patterns had been reading against handleMouseDown, where
# neither has existed since #779, so the verb read `gap` on master.
# They are re-anchored to the function that really owns them.
COMMIT_STARTING_REQUIRED = [
    ROC + r'reason\s*=[^\n]*"building\.spawn failed"',
]
# plant.designate: both branches share the same aoKind literal (only
# aoOutcome differs), so a bare field-presence check reads DONE even
# with the accepted branch's pushActionOutcome call itself renamed
# away (review round 9 — same class of bug as build-tool's
# portal-accepted hook) — anchor both outcomes to an actual
# pushActionOutcome call, not just the record fields. One definition,
# read by the registry entry and by the self-test's plant cases.
PLANT_DESIGNATE_REQUIRED = [
    r'aoKind\s*=\s*"plant\.designate"',
    PAO + r'aoOutcome\s*=\s*"accepted"',
    PAO + r'aoOutcome\s*=\s*"rejected"',
    r'recordMissingWorldOutcome env "plant\.designate"',
]


def game_chain_check(*texts: str) -> bool:
    # The chain spans TWO files since #1875 split the world-entity
    # fallback out of scripts/init_mouse.lua: the ordered tool/overlay
    # claim guards and the two inactive-gameplay deadclick gates stay in
    # init_mouse.lua, while the unit/item/building selection chain and
    # the context-menu/move-order chain (with their own handler names and
    # the off-world tile-menu deadclick) moved to init_mouse_entity.lua.
    # Both are read here because the AREA is the whole chain — checking
    # only one file would silently pass with half the routes deleted.
    # Concatenated rather than checked per-file so no pattern has to
    # encode which side of the split its route currently lives on; the
    # window-bounded anchors are all local to one call site.
    #
    # Three independent deadclick call sites today: the MOUSE_LEFT
    # inactive-gameplay gate, the MOUSE_RIGHT inactive-gameplay gate, and
    # the off-world no-selection tile-menu miss (review round 6 — the
    # count was stuck at 2, so the two original gates alone satisfied it
    # even with the third, newer site's hook removed/renamed). Bump this
    # whenever a genuinely new deadclick site is added. #1875's two
    # off-band gates deliberately do NOT bump it: they pass the outcome
    # through offBandOutcome(band), so neither spells a "deadclick"
    # literal at the call. The third site ALSO gets its own
    # route-specific pattern (its "no tile under cursor" reason is unique
    # among the three) so it stays covered even if a future fourth
    # deadclick site changes what the plain count of 3 would mean.
    if not all(texts):
        return False
    text = "\n".join(texts)
    return all_present(text, LAYER_A_GAME_CHAIN_HANDLERS) \
        and count_at_least(text, ROC_CLICK + r'"deadclick"', 3) \
        and all_present(text, [
            ROC_CLICK + r'"deadclick"[\s\S]{0,40}?"no tile under cursor"'])



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
#   #1602: anchored to commitStartingPlacement's REAL call site — the
#   binding-carrying five-argument spawn — not the three-argument
#   `building.spawn(target.def, igx, igy)` shape that has not existed
#   since #779 moved the call out of handleMouseDown.
_PORTAL_ACCEPTED_ANCHOR = \
    r"building\.spawn\(defName, gx, gy, bindPage, bindGen\)"


def _portal_accepted_body(text: str) -> str | None:
    r"""Everything from the portal's building.spawn call up to (not
    including) the branch's own "else" — the whole `if id then ... else`
    body, not just the span up to outcome="accepted". Bounded the same
    structural way as game_chain_check's routes: `(?:(?!else)[\s\S])*`
    can consume any text EXCEPT a run starting with the literal "else",
    so this is confined to the `if id then` branch regardless of field
    order or spacing inside it."""
    m = re.search(_PORTAL_ACCEPTED_ANCHOR + r"((?:(?!else)[\s\S])*)", text)
    return m.group(1) if m else None


def portal_accepted_present(text: str) -> bool:
    """The portal-accepted hook has no distinguishing reason text
    (success carries no reason at all), so it's anchored to
    `building.spawn(...)` — unique to the portal branch — rather than a
    reason literal (review round 8: neither the plain outcome="accepted"
    text nor the "building.spawn failed" reject reason alone proved this
    specific hook, since other accepted/rejected calls in the same
    function already satisfy those). Requires outcome="accepted" to
    appear inside an actual `debug.recordOutcome{...}` call (ROC), not
    just anywhere in the branch body — review round 11: renaming ONLY
    the portal-success call to some other table constructor left the
    literal `outcome = "accepted"` text sitting right there in the
    body, which a check with no call anchor still happily matched."""
    body = _portal_accepted_body(text)
    return body is not None and bool(
        re.search(ROC + r'outcome\s*=\s*"accepted"', body))


def portal_accepted_omits_reason(text: str) -> bool:
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


def build_tool_check(text: str) -> bool:
    if not text:
        return False
    commit_scope = function_scope(
        text, r"^function buildTool\.commitPlacement", LUA_FUNCTION_BOUNDARY)
    handle_scope = function_scope(
        text, r"^function buildTool\.handleMouseDown", LUA_FUNCTION_BOUNDARY)
    starting_scope = function_scope(
        text, r"^function buildTool\.commitStartingPlacement",
        LUA_FUNCTION_BOUNDARY)
    if commit_scope is None or handle_scope is None or starting_scope is None:
        return False
    return (all_present(commit_scope, COMMIT_PLACEMENT_REQUIRED)
            and all_present(handle_scope, HANDLE_MOUSE_DOWN_REQUIRED)
            and all_present(starting_scope, COMMIT_STARTING_REQUIRED)
            and portal_accepted_present(starting_scope)
            and portal_accepted_omits_reason(starting_scope)
            and count_at_least(
                handle_scope, ROC + r'reason\s*=[^\n]*"routed to construction\.designate"', 2)
            and count_at_least(
                handle_scope, ROC + r'reason\s*=[^\n]*"no active world id"', 2)
            # #1602's page-binding rejection: three distinct exits in
            # handleMouseDown (a pick-to-validation switch caught by
            # canPlaceAt, a validation-to-commit switch caught by the
            # starting-building spawn, and the same for the
            # construction.designate branch). Counting all three keeps
            # any one of them from being deleted while the other two
            # still satisfy a bare presence check.
            and count_at_least(
                handle_scope, ROC + r'reason\s*=[^\n]*"page binding changed"', 3))


# Each entry: (tier, verb, paths, check) where check() -> bool and
# `paths` names every repo-relative source file that check() reads.
# Built below so each verb's own check can pick file-wide vs
# function-scoped as needed.
#
# #1704: `paths` is not decoration. A check reads its files through
# _read/_all_present_check, both of which turn an absent file into a
# plain False — indistinguishable, in the report, from instrumentation
# that was really deleted. Declaring the mapping alongside the check
# lets --verify-tier1 tell the two apart and fail on either.
def build_verbs() -> list[CoverageEntry]:
    return [
        # --- Layer A: input routing, "complete" per the issue's scope note ---
        ("A", "input click -> UI/camera-drag consumption (deferred to release, #730 rounds 2-3)",
         # #730 review round 2 moved this from an immediate Dispatch.hs
         # record to a deferred one; round 3 added the middle-button
         # camera_drag site and the focus-loss/minimize
         # interrupted-release resolution in Engine.Input.State (see
         # ui_click_deferred_check) — a UI-origin OR middle-button H1
         # drag reads as exactly one "input.drag" instead of also
         # carrying a stale press-time "input.click", and a swallowed
         # release no longer loses the record outright. #787 moved the
         # press sites into Thread.Mouse and #1676 moved the release
         # resolution into Thread.Mouse.Deferred.
         [INPUT_MOUSE, INPUT_MOUSE_DEFERRED, INPUT_STATE],
         lambda: ui_click_deferred_check(
             _read(INPUT_MOUSE), _read(INPUT_MOUSE_DEFERRED),
             _read(INPUT_STATE))),
        ("A", "input click -> swallowed/no-handler routes (no event ever queued)",
         # Every distinct ClickSwallowed/no-handler-ClickUI route this
         # module knows about — ALL must be present, not just one, or a
         # route silently loses its record (review round 2 found the
         # degenerate-viewport and middle-click-miss routes missing
         # while the others made the whole area read DONE).
         [INPUT_MOUSE],
         lambda: swallowed_routes_check(_read(INPUT_MOUSE))),
        ("A", "input click -> game-world tool/select/deadclick chain",
         # #1875 split the world-entity fallback (selection chain,
         # context menus, move orders) into init_mouse_entity.lua; the
         # claim guards and the two gameplay-inactive gates stayed put.
         ["scripts/init_mouse.lua", "scripts/init_mouse_entity.lua"],
         lambda: game_chain_check(_read("scripts/init_mouse.lua"),
                                   _read("scripts/init_mouse_entity.lua"))),
        # --- Layer A (#730): non-click H1 input families — keyboard, text,
        # scroll/z-slice, drag. Each area's `_all_present_check` requires
        # EVERY registered domain literal, not just one, mirroring the
        # click area's multi-route pattern above. ---
        ("A", "input key -> shell/UI-text/gameplay routing domains",
         [INPUT_KEYBOARD],
         lambda: _all_present_check(INPUT_KEYBOARD, LAYER_A_KEY_DOMAINS)),
        ("A", "input type/char -> text-delivery + drop domains (aggregated)",
         [INPUT_CHAR],
         lambda: _all_present_check(INPUT_CHAR, LAYER_A_CHAR_DOMAINS)),
        ("A", "input scroll -> z-slice/UI-scroll/game-scroll/modal/degenerate domains",
         [INPUT_SCROLL],
         lambda: _all_present_check(INPUT_SCROLL, LAYER_A_SCROLL_DOMAINS)),
        ("A", "input drag -> unit_drag_select box-selection outcome",
         ["scripts/unit_drag_select.lua"],
         lambda: _all_present_check(
             "scripts/unit_drag_select.lua", LAYER_A_DRAG_OUTCOMES)),

        # --- Layer B Tier 1: onboarding + highest naive-frequency (this PR) ---
        ("B1", "createWorld.generate (proceed commit)",
         ["scripts/create_world/generation.lua"],
         lambda: _filewide_check(
             "scripts/create_world/generation.lua", r"debug\.recordOutcome")),
        ("B1", "buildTool.commitPlacement",
         ["scripts/build_tool.lua"],
         # Every distinct reject/accept site handleMouseDown's placement
         # branch and commitPlacement itself cover — a single file-wide
         # "does debug.recordOutcome appear anywhere" pattern reads DONE
         # even if every hook but one were deleted (review round 2).
         lambda: build_tool_check(_read("scripts/build_tool.lua"))),
        ("B1", "wire.place",
         ["scripts/wire.lua"],
         lambda: _filewide_check(
             "scripts/wire.lua", r"debug\.recordOutcome")),
        # Each designation verb needs BOTH its partial/reject-within-a-
        # loaded-page hook AND its separate missing-world-page hook
        # (review round 7 found all four silently dropped the latter —
        # `pure ()` with no F4 record at all when the queued page no
        # longer exists, a different failure than "page exists, sweep
        # found nothing").
        ("B1", "till.designate (partial-drop counts + missing-world)",
         ["src/World/Thread/Command/Cursor/Till.hs"],
         lambda: _all_present_check(
             "src/World/Thread/Command/Cursor/Till.hs",
             [r'recordDesignationOutcome env "till\.designate"',
              r'recordMissingWorldOutcome env "till\.designate"'])),
        ("B1", "chop.designate (partial-drop counts + missing-world)",
         ["src/World/Thread/Command/Cursor/Chop.hs"],
         lambda: _all_present_check(
             "src/World/Thread/Command/Cursor/Chop.hs",
             [r'recordDesignationOutcome env "chop\.designate"',
              r'recordMissingWorldOutcome env "chop\.designate"'])),
        ("B1", "world.designateMine (partial-drop counts + missing-world)",
         ["src/World/Thread/Command/Cursor/Mine.hs"],
         lambda: _all_present_check(
             "src/World/Thread/Command/Cursor/Mine.hs",
             [r'recordDesignationOutcome env "world\.designateMine"',
              r'recordMissingWorldOutcome env "world\.designateMine"'])),
        ("B1", "plant.designate (accept/reject + missing-world)",
         ["src/World/Thread/Command/Cursor/Plant.hs"],
         # Call-anchored per outcome — see PLANT_DESIGNATE_REQUIRED.
         lambda: _all_present_check(
             "src/World/Thread/Command/Cursor/Plant.hs",
             PLANT_DESIGNATE_REQUIRED)),

        # --- Layer B Tier 2: common mid-game — fast-follow, not this PR.
        # Function-scoped: commandMove/commandAttack share a file, as do
        # execute/executeAt, so a file-wide pattern would false-positive
        # the moment either sibling is instrumented. ---
        ("B2", "unitAi.commandMove",
         ["scripts/unit_ai_core.lua"],
         lambda: _scoped_check(
             "scripts/unit_ai_core.lua",
             r"^function unitAi\.commandMove", LUA_FUNCTION_BOUNDARY,
             r"debug\.recordOutcome")),
        ("B2", "unitAi.commandAttack",
         ["scripts/unit_ai_core.lua"],
         lambda: _scoped_check(
             "scripts/unit_ai_core.lua",
             r"^function unitAi\.commandAttack", LUA_FUNCTION_BOUNDARY,
             r"debug\.recordOutcome")),
        ("B2", "craft.execute",
         ["src/Engine/Scripting/Lua/API/Craft/Execute.hs"],
         lambda: _scoped_check(
             "src/Engine/Scripting/Lua/API/Craft/Execute.hs",
             r"^craftExecuteFn\s*∷", HASKELL_TOPLEVEL_BOUNDARY,
             r"pushActionOutcome")),
        ("B2", "craft.executeAt",
         ["src/Engine/Scripting/Lua/API/Craft/Execute.hs"],
         lambda: _scoped_check(
             "src/Engine/Scripting/Lua/API/Craft/Execute.hs",
             r"^craftExecuteAtFn\s*∷", HASKELL_TOPLEVEL_BOUNDARY,
             r"pushActionOutcome")),
        ("B2", "craft.addBill",
         ["src/Engine/Scripting/Lua/API/Craft/Bill.hs"],
         lambda: _scoped_check(
             "src/Engine/Scripting/Lua/API/Craft/Bill.hs",
             r"^craftAddBillFn\s*∷", HASKELL_TOPLEVEL_BOUNDARY,
             r"pushActionOutcome")),

        # --- Layer B Tier 3: everything else, added as those paths get
        # touched. construction.designate is structurally identical to
        # till/chop/mine but wasn't named in the issue's Tier 1 list. ---
        ("B3", "construction.designate (building/structure)",
         ["src/World/Thread/Command/Cursor/Construct.hs"],
         lambda: _filewide_check(
             "src/World/Thread/Command/Cursor/Construct.hs",
             r'recordDesignationOutcome env "construction\.designate"')),
    ]


def evaluate_coverage(entries: list[CoverageEntry]) -> list[tuple[str, str, bool]]:
    """The plain report's verdicts: each entry's tier, verb and whether
    its own check found every required producer shape, in inventory
    order. Evaluation only — the report's formatting and its always-0
    exit belong to the facade."""
    return [(tier, verb, fn()) for tier, verb, _paths, fn in entries]


TIER1 = "A"


def tier1_entries(entries: list[CoverageEntry]) -> list[CoverageEntry]:
    """The Tier 1 selector, in inventory order — the ONE place tier policy
    decides which areas the blocking gate covers. Both the verification
    below and the facade's "N of M" diagnostic read it, so the M the
    facade prints can never disagree with the entries that were gated."""
    return [entry for entry in entries if entry[0] == TIER1]


def verify_tier1_entries(entries) -> list[str]:
    """#1704 requirement 4: every Tier 1 (Layer A) area must be both
    MAPPED and INSTRUMENTED, evaluated against the real checked-in tree.

    Two distinct failures, reported apart because they have different
    repairs:

      * a mapped source file is absent — a producer was renamed or
        moved and the mapping is stranded. This is what #787 did to all
        five Layer A areas: the checker went on reading
        `Engine.Input.Thread` after that module became a thread-lifecycle
        facade, reporting instrumented behaviour as five gaps while
        exiting 0.
      * every mapped file exists but a required producer pattern is
        missing — instrumentation really was deleted.

    Takes the entry list so the self-test can prove both branches
    against synthetic entries without mutating the working tree.
    """
    problems = []
    for _tier, verb, paths, fn in tier1_entries(entries):
        if not paths:
            problems.append(f"{verb}: declares no source-file mapping")
            continue
        missing = [rel for rel in paths if not (REPO_ROOT / rel).exists()]
        if missing:
            problems.append(
                f"{verb}: mapped source file(s) absent — {', '.join(missing)}. "
                f"A producer moved; re-point this checker's mapping.")
            continue
        if not fn():
            problems.append(
                f"{verb}: a required producer pattern is missing from "
                f"{', '.join(paths)}. Instrumentation was deleted, or its "
                f"call shape changed.")
    return problems


class Tier1Verification(NamedTuple):
    """`verify_tier1`'s result: how many Tier 1 areas the policy covered
    and every problem it found. Empty `problems` is the passing gate."""
    total: int
    problems: list[str]


def verify_tier1(entries: list[CoverageEntry]) -> Tier1Verification:
    """The blocking Tier 1 policy over a supplied inventory. Returns the
    area total beside the problem list so the facade only interpolates
    two numbers it did not derive — the tier selection that produces
    both lives in `tier1_entries`, not in the command."""
    return Tier1Verification(len(tier1_entries(entries)),
                             verify_tier1_entries(entries))
