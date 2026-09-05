"""F4 (#646) action-outcome coverage: the synthetic mutation corpus.

Loaded only by `tools/action_outcome_coverage.py --self-test`. Nothing
in the production path imports it, and it reaches the command through
one entry point, `run_self_test()`, which returns the failure list the
facade formats.

Every fixture here is constructed source — deliberately damaged Lua and
Haskell — driven through the production predicates imported from
`tools/action_outcome_coverage_core.py`. No regex, mapping, helper or
registry entry is reproduced in this module: a case that needs one
imports it, so the corpus cannot drift from what the real audit checks
(review round 2's reason for sharing `LAYER_A_SWALLOWED_ROUTES`, now the
rule for the whole module). Importing this module runs no case, prints
nothing and reads no file; the corpus lives inside `run_self_test()`.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from action_outcome_coverage_core import (  # noqa: E402
    HASKELL_TOPLEVEL_BOUNDARY,
    LAYER_A_CHAR_DOMAINS,
    LAYER_A_DRAG_OUTCOMES,
    LAYER_A_KEY_DOMAINS,
    LAYER_A_SCROLL_DOMAINS,
    LAYER_A_SWALLOWED_ROUTES,
    LUA_FUNCTION_BOUNDARY,
    PLANT_DESIGNATE_REQUIRED,
    TIER1,
    all_present,
    build_tool_check,
    build_verbs,
    function_scope,
    game_chain_check,
    swallowed_routes_check,
    ui_click_deferred_check,
    verify_tier1_entries,
)


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


def run_self_test() -> list[str]:
    """Every case in the corpus, returning the failure messages the
    facade prints; an empty list is "all checks passed"."""
    failures = []

    # Most cases compare booleans; #1704's verify-tier1 cases compare a
    # problem-list length or the list itself, so this stays value-generic.
    def expect(label: str, got: object, want: object) -> None:
        if got != want:
            failures.append(f"{label}: expected {want!r}, got {got!r}")

    # 1. Neither sibling instrumented -> both scopes report False.
    move_scope = function_scope(_LUA_SIBLINGS, r"^function unitAi\.commandMove",
                                  LUA_FUNCTION_BOUNDARY)
    attack_scope = function_scope(_LUA_SIBLINGS, r"^function unitAi\.commandAttack",
                                    LUA_FUNCTION_BOUNDARY)
    expect("neither instrumented: commandMove",
           bool(move_scope and re.search(r"debug\.recordOutcome", move_scope)), False)
    expect("neither instrumented: commandAttack",
           bool(attack_scope and re.search(r"debug\.recordOutcome", attack_scope)), False)

    # 2. Only commandMove instrumented -> commandMove True, commandAttack
    #    STILL False. A naive file-wide `debug\.recordOutcome` search
    #    would incorrectly mark commandAttack True too — this is exactly
    #    what review round 1 flagged.
    move_scope2 = function_scope(_LUA_SIBLINGS_ONE_INSTRUMENTED,
                                   r"^function unitAi\.commandMove", LUA_FUNCTION_BOUNDARY)
    attack_scope2 = function_scope(_LUA_SIBLINGS_ONE_INSTRUMENTED,
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
    exec_scope = function_scope(_HS_SIBLINGS_ONE_INSTRUMENTED,
                                  r"^craftExecuteFn\s*∷", HASKELL_TOPLEVEL_BOUNDARY)
    exec_at_scope = function_scope(_HS_SIBLINGS_ONE_INSTRUMENTED,
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

    # #1704: the two `ui_pointer_block` branches are counted, not merely
    # present, and the no-right-click-handler route records under a
    # BINDING rather than a literal — so the fixture names those three
    # sites individually.
    _POINTER_BLOCK_SITES = {"pointer_block_left", "pointer_block_right"}
    _SWALLOWED_SITES = set(_ROUTE_NAMES) | _POINTER_BLOCK_SITES | {"no_handler"}

    def swallowed_routes_fixture(include: set[str],
                                  call_name: str = "recordRouteOutcome") -> str:
        lines = [f"{call_name} ∷ Text → Maybe Text → IO ()",
                  f"{call_name} outcome handler = do pure ()"]
        for name in _ROUTE_NAMES:
            if name in include:
                lines.append(f'{call_name} "accepted" (Just "{name}")')
        for site in sorted(_POINTER_BLOCK_SITES):
            if site in include:
                lines.append(f'-- {site}')
                lines.append(f'{call_name} "noop" (Just "ui_pointer_block")')
        if "no_handler" in include:
            lines.append(f'{call_name} "noop" (Just leftClickCallback)')
        return "\n".join(lines)

    swallowed_full = swallowed_routes_fixture(_SWALLOWED_SITES)
    expect("Layer A swallowed routes: all present reads DONE",
           swallowed_routes_check(swallowed_full), True)
    for name in sorted(_SWALLOWED_SITES):
        missing_one = swallowed_routes_fixture(_SWALLOWED_SITES - {name})
        expect(f"Layer A swallowed routes: missing {name!r} reads gap",
               swallowed_routes_check(missing_one), False)
    # The count is what makes EITHER pointer-block branch load-bearing:
    # a bare presence check over the shared literal passes with one of
    # the two deleted, which is exactly the hole #1704 closed.
    one_pointer_block = swallowed_routes_fixture(
        _SWALLOWED_SITES - {"pointer_block_right"})
    expect("Layer A swallowed routes: a bare presence check WOULD have "
           "missed one deleted ui_pointer_block branch (sanity check on "
           "the fixture)",
           all_present(one_pointer_block, LAYER_A_SWALLOWED_ROUTES), True)
    renamed_call = swallowed_routes_fixture(_SWALLOWED_SITES,
                                            call_name="someOtherHelper")
    expect("Layer A swallowed routes: recordRouteOutcome renamed away "
           "(literals kept) reads as a gap (review round 5)",
           swallowed_routes_check(renamed_call), False)

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
           game_chain_check(chain_full), True)
    for name in _HANDLER_NAMES:
        missing_one = game_chain_fixture(set(_HANDLER_NAMES) - {name}, _REALISTIC_DEADCLICKS)
        expect(f"game chain: missing handler {name!r} reads gap",
               game_chain_check(missing_one), False)
    for n in (0, 1, 2):
        expect(f"game chain: only {n} of 3 real deadclick sites reads gap",
               game_chain_check(
                   game_chain_fixture(set(_HANDLER_NAMES), _REALISTIC_DEADCLICKS[:n])),
               False)
    # The review-round-6 exact blocker: enough generic deadclicks to pass
    # a plain count check, but NONE for the off-world tile-menu miss
    # specifically — must still read as a gap.
    generic_only = game_chain_fixture(set(_HANDLER_NAMES), [_INACTIVE_GAMEPLAY] * 4)
    expect("game chain: 4 generic deadclicks but none for the off-world "
           "tile-menu miss reads gap (review round 6)",
           game_chain_check(generic_only), False)
    chain_renamed_call = game_chain_fixture(
        set(_HANDLER_NAMES), _REALISTIC_DEADCLICKS, call_name="someOtherFn")
    expect("game chain: recordClick renamed away (literals kept) reads "
           "as a gap (review round 5)",
           game_chain_check(chain_renamed_call), False)

    # #1875: the chain spans TWO files now, so the check has to be
    # satisfied by their UNION and by neither half alone. Without these,
    # a version that read only one of the two would still pass every
    # single-text case above while half the routes could be deleted.
    _ROUTER_HANDLERS = {"debug_overlay", "debug_anim_panel", "build_tool",
                        "mine_tool", "chop_tool", "till_tool", "plant_tool"}
    _ENTITY_HANDLERS = set(_HANDLER_NAMES) - _ROUTER_HANDLERS
    router_half = game_chain_fixture(_ROUTER_HANDLERS,
                                     [_INACTIVE_GAMEPLAY, _INACTIVE_GAMEPLAY])
    entity_half = game_chain_fixture(_ENTITY_HANDLERS,
                                     ["no tile under cursor"])
    expect("game chain: router half + entity half together read DONE "
           "(#1875 split)",
           game_chain_check(router_half, entity_half), True)
    expect("game chain: the router half alone reads gap (#1875 split)",
           game_chain_check(router_half), False)
    expect("game chain: the entity half alone reads gap (#1875 split)",
           game_chain_check(entity_half), False)
    expect("game chain: an unreadable/empty second file reads gap rather "
           "than being ignored (#1875 split)",
           game_chain_check(chain_full, ""), False)

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

    # #1602: the portal's spawn hooks live in commitStartingPlacement,
    # a separate top-level function, so they get their own fixture — the
    # same realistic if/else/end shape the anchor's negative-lookahead
    # bound needs to mean anything.
    def commit_starting_fn(include: set[str],
                           portal_accepted_reason: str | None = None,
                           portal_accepted_reason_first: bool = False,
                           portal_accepted_call: str = "debug.recordOutcome",
                           spawn_call: str =
                               "building.spawn(defName, gx, gy, bindPage, bindGen)"
                           ) -> str:
        lines = ["function buildTool.commitStartingPlacement(defName, gx, gy, "
                 "bindPage, bindGen)"]
        if "spawn" in include or "portal_accepted" in include:
            # Real if/else/end shape (not a flat sequence) — the
            # portal-accepted anchor pattern is bounded by a negative
            # lookahead on "else", so the fixture needs the same
            # structure the real code has for that bound to mean
            # anything (review round 8's window-based predecessor
            # bridged past "building.spawn failed" into the NEXT
            # unrelated accepted call in a flatter fixture).
            lines.append(f'    local id, spawnErr = {spawn_call}')
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
        lines.append("end")
        return "\n".join(lines)

    def handle_mouse_down_fn(include: set[str], designate_sites: int,
                             no_world_sites: int,
                             binding_sites: int = 3) -> str:
        lines = ["function buildTool.handleMouseDown(button, x, y)"]
        if "offworld" in include:
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = "off-world click during placement"}')
        if "invalid" in include:
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = "invalid placement tile"}')
        for _ in range(binding_sites):
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = "page binding changed"}')
        for _ in range(designate_sites):
            lines.append('    debug.recordOutcome{outcome = "accepted", '
                          'reason = "routed to construction.designate"}')
        for _ in range(no_world_sites):
            lines.append('    debug.recordOutcome{outcome = "rejected", '
                          'reason = "no active world id"}')
        lines.append("end")
        return "\n".join(lines)

    all_commit_parts = {"power", "carrier", "node", "accepted"}
    all_handle_parts = {"offworld", "invalid"}
    all_starting_parts = {"spawn", "portal_accepted"}

    def build_tool_fixture(commit_parts=None, handle_parts=None,
                           starting_parts=None, designate_sites: int = 2,
                           no_world_sites: int = 2, binding_sites: int = 3,
                           **starting_kwargs) -> str:
        return (commit_placement_fn(
                    all_commit_parts if commit_parts is None else commit_parts)
                + "\n" + commit_starting_fn(
                    all_starting_parts if starting_parts is None
                    else starting_parts, **starting_kwargs)
                + "\n" + handle_mouse_down_fn(
                    all_handle_parts if handle_parts is None else handle_parts,
                    designate_sites, no_world_sites, binding_sites))

    full_fixture = build_tool_fixture()
    expect("buildTool.commitPlacement: all hooks present reads DONE",
           build_tool_check(full_fixture), True)
    missing_portal_accepted = build_tool_fixture(
        starting_parts=all_starting_parts - {"portal_accepted"})
    expect("buildTool.commitPlacement: missing the portal-accepted hook "
           "reads gap (review round 8)",
           build_tool_check(missing_portal_accepted), False)
    portal_accepted_with_reason = build_tool_fixture(
        portal_accepted_reason="building.spawn failed")
    expect("buildTool.commitPlacement: a reason reintroduced on the "
           "portal-accepted record reads gap (review round 9 — the "
           "exact `ok and nil or ...` bug class)",
           build_tool_check(portal_accepted_with_reason), False)
    portal_accepted_reason_before = build_tool_fixture(
        portal_accepted_reason="building.spawn failed",
        portal_accepted_reason_first=True)
    expect("buildTool.commitPlacement: a reason reintroduced BEFORE "
           "outcome=\"accepted\" in the same record reads gap (review "
           "round 10 — field order previously evaded a check that only "
           "looked forward from the outcome match)",
           build_tool_check(portal_accepted_reason_before), False)
    portal_accepted_call_renamed = build_tool_fixture(
        portal_accepted_call="someOtherTableCtor")
    expect("buildTool.commitPlacement: ONLY the portal-success call "
           "renamed away (fields kept) reads gap (review round 11 — "
           "outcome=\"accepted\" alone, with no debug.recordOutcome "
           "anchor, previously still read DONE)",
           build_tool_check(portal_accepted_call_renamed), False)
    # #1602: the anchor must name the binding-carrying spawn. Reverting
    # commitStartingPlacement to a spawn that drops the page binding
    # leaves every outcome hook intact yet removes the very call the
    # portal-accepted hook is anchored to — the binding regression this
    # re-anchoring exists to catch.
    spawn_binding_dropped = build_tool_fixture(
        spawn_call="building.spawn(defName, gx, gy)")
    expect("buildTool.commitPlacement: commitStartingPlacement spawning "
           "WITHOUT the page binding reads gap (#1602)",
           build_tool_check(spawn_binding_dropped), False)
    # #1602: each of the three page-binding rejection exits must be
    # individually load-bearing — two surviving siblings must not cover
    # for a deleted third.
    for present in (0, 1, 2):
        expect(f"buildTool.commitPlacement: only {present} of three "
               "'page binding changed' hooks reads gap (#1602)",
               build_tool_check(build_tool_fixture(binding_sites=present)),
               False)
    # The portal hooks must be found in commitStartingPlacement's OWN
    # scope, not wherever they happen to sit: dropping that function
    # entirely (its hooks pasted into handleMouseDown, as they were
    # before #779) reads as a gap rather than silently passing.
    starting_absent = (commit_placement_fn(all_commit_parts) + "\n"
                       + commit_starting_fn(all_starting_parts).replace(
                           "function buildTool.commitStartingPlacement",
                           "function buildTool.handleMouseDown")
                       + "\n" + handle_mouse_down_fn(all_handle_parts, 2, 2))
    expect("buildTool.commitPlacement: portal hooks outside "
           "commitStartingPlacement's own scope reads gap (#1602)",
           build_tool_check(starting_absent), False)

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

    _PLANT_PATTERNS = PLANT_DESIGNATE_REQUIRED
    _PLANT_PARTS = {"accepted", "rejected", "missing_world"}
    plant_full = plant_fixture(_PLANT_PARTS)
    expect("plant.designate: all hooks present reads DONE",
           all_present(plant_full, _PLANT_PATTERNS), True)
    for missing in _PLANT_PARTS:
        expect(f"plant.designate: missing the {missing!r} hook reads gap",
               all_present(plant_fixture(_PLANT_PARTS - {missing}), _PLANT_PATTERNS),
               False)
    plant_renamed = plant_fixture(_PLANT_PARTS, accepted_call="someOtherPushFn",
                                   rejected_call="someOtherPushFn")
    expect("plant.designate: BOTH producers renamed away (fields kept) "
           "reads as a gap (review round 9)",
           all_present(plant_renamed, _PLANT_PATTERNS), False)
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
           all_present(plant_only_accepted_renamed, _PLANT_PATTERNS), False)
    plant_only_rejected_renamed = plant_fixture(
        _PLANT_PARTS, rejected_call="someOtherPushFn")
    expect("plant.designate: only the rejected producer renamed (accepted "
           "intact) reads gap (review round 10)",
           all_present(plant_only_rejected_renamed, _PLANT_PATTERNS), False)

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
           build_tool_check(no_hooks_fixture), False)

    # The exact review-round-4 counter-example: every `debug.recordOutcome`
    # CALL renamed away (to some unrelated table constructor) while its
    # `{outcome = ..., reason = ...}` fields are left completely intact.
    # A field-only check (no anchor to the call itself) still reads DONE
    # here; requiring the call is what review round 4 asked for.
    call_renamed_fixture = (
        full_fixture.replace("debug.recordOutcome", "someOtherTableCtor"))
    expect("buildTool.commitPlacement: recordOutcome call renamed away "
           "(fields kept) reads as a gap (review round 4)",
           build_tool_check(call_renamed_fixture), False)

    for missing in all_commit_parts:
        expect(f"buildTool.commitPlacement: missing the {missing!r} hook reads gap",
               build_tool_check(build_tool_fixture(
                   commit_parts=all_commit_parts - {missing})), False)
    for missing in all_handle_parts:
        expect(f"buildTool.commitPlacement: missing the {missing!r} hook reads gap",
               build_tool_check(build_tool_fixture(
                   handle_parts=all_handle_parts - {missing})), False)
    for missing in all_starting_parts:
        expect(f"buildTool.commitPlacement: missing the {missing!r} hook reads gap",
               build_tool_check(build_tool_fixture(
                   starting_parts=all_starting_parts - {missing})), False)
    expect("buildTool.commitPlacement: only ONE construction.designate "
           "hook (of two call sites) reads gap",
           build_tool_check(build_tool_fixture(designate_sites=1)), False)
    expect("buildTool.commitPlacement: only ONE 'no active world id' hook "
           "(of two call sites) reads gap",
           build_tool_check(build_tool_fixture(no_world_sites=1)), False)

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
           all_present(key_full, LAYER_A_KEY_DOMAINS), True)
    for domain in _KEY_DOMAINS_SET:
        expect(f"input key: missing the {domain!r} domain reads gap",
               all_present(key_outcome_fixture(_KEY_DOMAINS_SET - {domain}),
                             LAYER_A_KEY_DOMAINS), False)
    key_renamed = key_outcome_fixture(_KEY_DOMAINS_SET, call_name="someOtherKeyFn")
    expect("input key: recordKeyOutcome renamed away (literals kept) reads gap",
           all_present(key_renamed, LAYER_A_KEY_DOMAINS), False)

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
           all_present(char_full, LAYER_A_CHAR_DOMAINS), True)
    for domain in _CHAR_DOMAINS_SET:
        expect(f"input type/char: missing the {domain!r} domain reads gap",
               all_present(char_outcome_fixture(_CHAR_DOMAINS_SET - {domain}),
                             LAYER_A_CHAR_DOMAINS), False)
    char_renamed = char_outcome_fixture(_CHAR_DOMAINS_SET, call_name="someOtherCharFn")
    expect("input type/char: accumulateCharOutcome renamed away (literals "
           "kept) reads gap",
           all_present(char_renamed, LAYER_A_CHAR_DOMAINS), False)

    def scroll_outcome_fixture(include: set[str],
                                call_name: str = "recordScrollOutcome") -> str:
        lines = [f"{call_name} ∷ Text → Text → Maybe Word32 → IO ()",
                 f"{call_name} outcome domain target = pure ()"]
        combos = {
            "z_slice": '"accepted" "z_slice" Nothing',
            "ui_scroll": '"accepted" "ui_scroll" (Just eh)',
            "game_scroll": '"accepted" "game_scroll" Nothing',
            "degenerate_viewport": '"noop" "degenerate_viewport" Nothing',
            # #1704: the fifth domain, absent from this inventory until
            # now — a modal page's empty space swallowing the wheel.
            "ui_modal_block": '"noop" "ui_modal_block" Nothing',
        }
        for name, argtext in combos.items():
            if name in include:
                lines.append(f'{call_name} {argtext}')
        return "\n".join(lines)

    _SCROLL_DOMAINS_SET = {"z_slice", "ui_scroll", "game_scroll",
                           "degenerate_viewport", "ui_modal_block"}
    scroll_full = scroll_outcome_fixture(_SCROLL_DOMAINS_SET)
    expect("input scroll: all five domains present reads DONE",
           all_present(scroll_full, LAYER_A_SCROLL_DOMAINS), True)
    for domain in _SCROLL_DOMAINS_SET:
        expect(f"input scroll: missing the {domain!r} domain reads gap",
               all_present(scroll_outcome_fixture(_SCROLL_DOMAINS_SET - {domain}),
                             LAYER_A_SCROLL_DOMAINS), False)
    scroll_renamed = scroll_outcome_fixture(_SCROLL_DOMAINS_SET, call_name="someOtherScrollFn")
    expect("input scroll: recordScrollOutcome renamed away (literals kept) "
           "reads gap",
           all_present(scroll_renamed, LAYER_A_SCROLL_DOMAINS), False)

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
        if "left_undefined" in include:
            lines.append(
                f'{call_name}("noop", x, y, 0, 0, '
                '"no drag gesture is defined for this input")')
        return "\n".join(lines)

    _DRAG_PARTS = {"completed", "swallowed", "right_undefined", "left_undefined"}
    drag_full = drag_outcome_fixture(_DRAG_PARTS)
    expect("input drag: all four outcome call sites present reads DONE",
           all_present(drag_full, LAYER_A_DRAG_OUTCOMES), True)
    for part in _DRAG_PARTS:
        expect(f"input drag: missing the {part!r} call site reads gap",
               all_present(drag_outcome_fixture(_DRAG_PARTS - {part}),
                             LAYER_A_DRAG_OUTCOMES), False)
    drag_renamed = drag_outcome_fixture(_DRAG_PARTS, call_name="someOtherDragFn")
    expect("input drag: recordDragOutcome renamed away (literals kept) reads gap",
           all_present(drag_renamed, LAYER_A_DRAG_OUTCOMES), False)

    # #730 review rounds 2 & 3, re-pointed by #1704: the
    # deferred-to-release UI/camera-drag click classification. Three
    # producing sites live in Engine.Input.Thread.Mouse
    # (`mouse_include`) — the left-click UI hit, the right-click hit,
    # and the middle-button camera_drag press, all through #1676's
    # `deferPress` helper; the release-side threshold comparison that
    # picks between the press's own kind and "input.drag" lives in
    # Engine.Input.Thread.Mouse.Deferred (`deferred_include`); and the
    # interrupted-release resolution for a focus-loss/minimize swallow
    # lives in Engine.Input.State (`state_include`).
    #
    # There is deliberately NO fourth "input.click" site: #743 turned
    # the right-click-consumed-by-a-left-only-control route back into
    # an immediate noop, which the swallowed-routes area above now
    # requires instead.
    def ui_click_mouse_fixture(mouse_include: set[str],
                                defer_call: str = "deferPress") -> str:
        lines = []
        if "click_site" in mouse_include:
            lines.append(f'{defer_call} "input.click" callback')
        if "rightclick_site" in mouse_include:
            lines.append(f'{defer_call} "input.rightClick" callback')
        if "camera_drag_site" in mouse_include:
            lines.append(f'{defer_call} "input.click" "camera_drag"')
        return "\n".join(lines)

    def ui_click_deferred_fixture(deferred_include: set[str],
                                   push_call: str = "pushActionOutcome") -> str:
        lines = ["    let (kind, whereX, whereY)"]
        if "release_classify" in deferred_include:
            lines.append('            | movedPx ≥ uiDragThresholdPx =')
            lines.append('                let (rx, ry) = windowToFbOrRaw win fb (x, y)')
            lines.append('                in ("input.drag", rx, ry)')
        if "release_click_branch" in deferred_include:
            lines.append('            | otherwise = (pucKind pc, pucPressFbX pc, '
                         'pucPressFbY pc)')
        if "release_push" in deferred_include:
            lines.append(f'    {push_call} (actionOutcomeRef env) ActionOutcome')
            lines.append('        { aoTs = gt, aoKind = kind, aoOutcome = outcomeName')
            lines.append('        }')
        return "\n".join(lines)

    def ui_click_interrupted_fixture(include: set[str],
                                      push_call: str = "pushActionOutcome") -> str:
        if "interrupted" not in include:
            return ""
        return (f'{push_call} (actionOutcomeRef env) ActionOutcome\n'
                '    { aoOutcome = "noop"\n'
                '    -- #1676: the press\'s own PRESS-TIME framebuffer\n'
                '    -- position, captured before the ratio could move.\n'
                '    , aoWhereX = Just (pucPressFbX pc)\n'
                '    , aoReason = Just "release swallowed (focus loss / minimize)"\n'
                '    }')

    _UI_CLICK_MOUSE_PARTS = {"click_site", "rightclick_site", "camera_drag_site"}
    _UI_CLICK_DEFERRED_PARTS = {
        "release_classify", "release_click_branch", "release_push",
    }
    _UI_CLICK_STATE_PARTS = {"interrupted"}
    ui_mouse_full = ui_click_mouse_fixture(_UI_CLICK_MOUSE_PARTS)
    ui_deferred_full = ui_click_deferred_fixture(_UI_CLICK_DEFERRED_PARTS)
    ui_interrupted_full = ui_click_interrupted_fixture(_UI_CLICK_STATE_PARTS)
    expect("input click UI deferral: all parts present reads DONE",
           ui_click_deferred_check(ui_mouse_full, ui_deferred_full,
                                     ui_interrupted_full), True)
    for part in sorted(_UI_CLICK_MOUSE_PARTS):
        expect(f"input click UI deferral: missing the {part!r} press site reads gap",
               ui_click_deferred_check(
                   ui_click_mouse_fixture(_UI_CLICK_MOUSE_PARTS - {part}),
                   ui_deferred_full, ui_interrupted_full),
               False)
    for part in sorted(_UI_CLICK_DEFERRED_PARTS):
        expect(f"input click UI deferral: missing the {part!r} release part reads gap",
               ui_click_deferred_check(
                   ui_mouse_full,
                   ui_click_deferred_fixture(_UI_CLICK_DEFERRED_PARTS - {part}),
                   ui_interrupted_full),
               False)
    expect("input click UI deferral: missing the interrupted-release "
           "resolution (review round 3) reads gap",
           ui_click_deferred_check(ui_mouse_full, ui_deferred_full,
                                     ui_click_interrupted_fixture(set())),
           False)
    # #1704: a stale THRESHOLD is its own regression. The check used to
    # demand three deferred "input.click" writes when #743 had left
    # only two, which is one of the five false Tier 1 gaps this issue
    # closed — so the fixture pins that exactly two satisfy it and one
    # does not.
    expect("input click UI deferral: only one of two 'input.click' sites reads gap",
           ui_click_deferred_check(
               ui_click_mouse_fixture({"rightclick_site", "camera_drag_site"}),
               ui_deferred_full, ui_interrupted_full),
           False)
    ui_mouse_renamed = ui_click_mouse_fixture(
        _UI_CLICK_MOUSE_PARTS, defer_call="someOtherDeferFn")
    expect("input click UI deferral: deferPress renamed away "
           "(literals kept) reads gap",
           ui_click_deferred_check(ui_mouse_renamed, ui_deferred_full,
                                     ui_interrupted_full), False)
    ui_deferred_renamed = ui_click_deferred_fixture(
        _UI_CLICK_DEFERRED_PARTS, push_call="someOtherPushFn")
    expect("input click UI deferral: release-side pushActionOutcome renamed "
           "away (classification kept) reads gap",
           ui_click_deferred_check(ui_mouse_full, ui_deferred_renamed,
                                     ui_interrupted_full), False)
    ui_interrupted_renamed = ui_click_interrupted_fixture(
        _UI_CLICK_STATE_PARTS, push_call="someOtherPushFn")
    expect("input click UI deferral: interrupted-release pushActionOutcome "
           "renamed away (literals kept) reads gap",
           ui_click_deferred_check(ui_mouse_full, ui_deferred_full,
                                     ui_interrupted_renamed), False)
    # Each of the three files is independently load-bearing: an EMPTY
    # one (which is what _read returns for a path that does not exist —
    # the #787 stranding, seen from inside a check) reads as a gap
    # rather than being skipped.
    for label, args in (
            ("Mouse.hs", ("", ui_deferred_full, ui_interrupted_full)),
            ("Mouse/Deferred.hs", (ui_mouse_full, "", ui_interrupted_full)),
            ("State.hs", (ui_mouse_full, ui_deferred_full, "")),
    ):
        expect(f"input click UI deferral: an absent {label} reads gap",
               ui_click_deferred_check(*args), False)

    # ---------------------------------------------------------------
    # #1704 requirement 4: the Tier 1 mapping/instrumentation gate.
    # Proven against SYNTHETIC entries so both failure branches are
    # exercised without mutating the working tree — the real-tree half
    # is `--verify-tier1` itself, which CI runs on every push.
    # ---------------------------------------------------------------
    # The command facade: a repo-relative path that really exists, so
    # the "mapped-and-instrumented" case below exercises the passing
    # branch rather than silently falling into the stranded-mapping one.
    _REAL_PATH = "tools/action_outcome_coverage.py"
    _GONE_PATH = "src/Engine/Input/ThisModuleWasMovedAwayByARefactor.hs"

    expect("verify-tier1: a mapped-and-instrumented Tier 1 area passes",
           verify_tier1_entries([(TIER1, "ok", [_REAL_PATH], lambda: True)]), [])
    stranded = verify_tier1_entries(
        [(TIER1, "stranded", [_GONE_PATH], lambda: False)])
    expect("verify-tier1: a Tier 1 area whose mapped file is absent fails",
           len(stranded), 1)
    expect("verify-tier1: …and says the mapping is stranded, not that the "
           "instrumentation is missing",
           "mapped source file(s) absent" in stranded[0], True)
    # The stranded case must be reported even when only ONE of several
    # mapped files went missing — the #787 shape, where a check reads a
    # facade that still exists beside modules that moved.
    partly_stranded = verify_tier1_entries(
        [(TIER1, "partly", [_REAL_PATH, _GONE_PATH], lambda: True)])
    expect("verify-tier1: one absent file among several still fails",
           len(partly_stranded), 1)
    deleted = verify_tier1_entries(
        [(TIER1, "deleted", [_REAL_PATH], lambda: False)])
    expect("verify-tier1: a present-but-uninstrumented Tier 1 area fails",
           len(deleted), 1)
    expect("verify-tier1: …and says the producer pattern is missing",
           "required producer pattern is missing" in deleted[0], True)
    expect("verify-tier1: a Tier 1 area declaring no mapping at all fails",
           len(verify_tier1_entries([(TIER1, "unmapped", [], lambda: True)])), 1)
    # Tier 2/3 gaps are deliberate fast-follows (#646) — the gate must
    # ignore them entirely, or it would block on work nobody has done
    # yet. This is the other half of requirement 5.
    expect("verify-tier1: a failing Tier 2 entry is NOT a gate failure",
           verify_tier1_entries([("B2", "fast-follow", [_REAL_PATH],
                                   lambda: False)]), [])
    expect("verify-tier1: every Tier 1 entry in the real table declares "
           "at least one mapped path",
           all(bool(paths)
               for tier, _verb, paths, _fn in build_verbs() if tier == TIER1),
           True)

    return failures
