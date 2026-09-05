#!/usr/bin/env python3
"""The aggregate runner's probe REGISTRY, and every per-key declaration.

The authoritative list of registered probes (`PROBES`), the selection
rules `--only`/`--exact` are resolved through, and the two per-key
declaration tables that are pure DATA about a probe rather than about a
run: how many ports it may bind (`PROBE_PORT_SPANS`, #1571) and how long
its complete scenario is allowed to take (`PROBE_TIMEOUT_OVERRIDES`).

This is a LEAF owner (#2074): it imports nothing of the runner's, so
every other owner — and every tool that only wants the inventory
(`tools/ci_probes.py`, the census/claim/evidence/inflight readers, the
de-flake lab) — can read the registry without pulling in resource
scheduling, process lifecycle, or the runner command itself.

`tools/run_probes.py` remains the only command; nothing here is a
program.
"""
from __future__ import annotations
import math

# The user's own graphical instance. No probe may ever be pointed at it,
# and since #1571 that refusal covers every port a probe RESERVES, not
# just the base it is handed (see `PROBE_PORT_SPANS` below).
GUI_PORT = 8008

# Default origin for the per-probe port SPANS handed out in parallel mode
# (--jobs) when `--port` does not supply one. Clear of the GUI port. Each
# probe in a concurrent batch is launched with an explicit --port
# override naming the base of its own span, so this range never needs to
# be clear of any probe's own default port — the override always wins
# over it.
PARALLEL_PORT_BASE = 9400

# (key, script filename, one-line purpose for --list). Every registered
# probe accepts --port (#723); each script's own default is its historical
# fixed port, so a bare invocation behaves exactly as before.
PROBES = [
    ("action_outcome", "action_outcome_probe.py",
     "F4 action-outcome oracle: recordOutcome/drainActionOutcomes public "
     "contract, destructive drain, till/chop mixed-sweep partial paths (#646)"),
    ("blood_decal", "blood_decal_probe.py",
     "blood decal model: texture reuse/eviction, render quads, wetness-tint aging (#604, #606)"),
    ("blood_impact", "blood_impact_probe.py",
     "wound-kind/severity -> impact-blood style/volume mapping (#607)"),
    ("blood_gpu_lifecycle", "blood_gpu_lifecycle_probe.py",
     "blood GPU textures disposed on world destroy/replace teardown paths (#788; offscreen, needs a GPU)"),
    ("bleeding_trail", "bleeding_trail_probe.py",
     "ongoing bleeding-trail emission: distance/cadence-gated marks, timescale invariance, "
     "clot/internal-wound/death cutoffs (#882)"),
    ("canteen_instance", "canteen_instance_probe.py",
     "the water AI drains/fills the exact canteen instance it selected, not the "
     "first same-def match (#1220)"),
    ("cargo_capacity", "cargo_capacity_probe.py",
     "depositToCargo weighs the actual ItemInstance, not the def base weight (#189)"),
    ("chop", "chop_probe.py",
     "chop-designation layer + chop AI + wood_log yield (#97)"),
    ("circadian", "circadian_probe.py",
     "circadian urge signal + sleep_pressure drain-only resource wiring (#611)"),
    ("circadian_species", "circadian_species_probe.py",
     "species-specific circadian phase (bear dawn vs acolyte dusk) through the real go_to_sleep AI + pose chain (#613)"),
    ("collapse_crawl", "collapse_crawl_probe.py",
     "collapse<->crawl pose hysteresis in tickInjuries (#304)"),
    ("combat_anim", "combat_anim_probe.py",
     "real fight headless; verifies swing/death animations play"),
    ("concussion_revive", "concussion_revive_probe.py",
     "checkRevive concussion-band hysteresis (#304)"),
    ("config_migration", "config_migration_probe.py",
     "pre-#661 legacy config (video/keybinds/notifications) migrates to "
     "*.local.yaml on boot: idempotent, local-wins, malformed-fails-safe (#786)"),
    ("config_state", "config_state_probe.py",
     "local runtime config (video/keybinds/notifications) vs versioned "
     "_default.yaml templates never dirties git (#638)"),
    ("construction", "construction_probe.py",
     "construct_job AI end-to-end: claim/source/progress/place/stake/release (#96)"),
    ("construction_blueprint_footprint", "construction_blueprint_footprint_probe.py",
     "offscreen: committed CtBuilding blueprint renders ONE ghost of the "
     "building's own sprite, in the staked building's own box, while staying "
     "one designation job (#807/#1845)"),
    ("content_registry", "content_registry_probe.py",
     "content-registries capability (#890): all seven registries written via "
     "their public load*Yaml verb and read back via their public query, plus "
     "post-boot reload (insert/replace) and the placed-location def join"),
    ("consumable_effects", "consumable_effects_probe.py",
     "drink effects scaled by item quality/temperature: hydration/caffeine/mood/warmth (#347)"),
    ("cooking", "cooking_probe.py",
     "kitchen workshop + cooking skill/basic_cuisine + basic_food/coffee content (#346)"),
    ("craft", "craft_probe.py",
     "craft.* API: catalogue, execute, stations, quality, smelting (#325/#326/#343/#327)"),
    ("craft_bill", "craft_bill_probe.py",
     "craft-bill backend + craft_job AI: queue/claim/progress, source (ground+cargo) -> work -> produce loop (#329)"),
    ("crop", "crop_probe.py",
     "row-crop natural placement + groundcover world.plantCropAt into a CropPlot, growth/harvest/save-load (#334)"),
    ("debug_console_boot", "debug_console_boot_probe.py",
     "required-debug-console boot contract: --headless/--offscreen refuse port 0 and a "
     "failed bind, exit non-zero with a mode/port/cause diagnostic and a cleanup trace, "
     "no READY, no offscreen Vulkan; --dump's #46 port-0 sentinel and a successful "
     "bind unchanged (#1190); plus normal-boot widget-module loading — the 28 "
     "scripts.ui.* modules a non-preview headless boot loads, with two negative "
     "regressions against really-broken boots (#1365)"),
    ("disarm", "disarm_probe.py",
     "disabled-hand auto-drop must re-fire (#193)"),
    ("expedition_loop", "expedition_loop_probe.py",
     "the first expedition end to end — the arc's final integrated gate: "
     "colony from a real portal roster, water secured by a real FOV scan, "
     "provisioning off the technomule, a two-traveller journey whose "
     "unprepared control is measurably worse off, discovery by sight, "
     "extraction of the ruin's own loot roll, return and deposit, and a "
     "fresh-process reload verifying every durable identity (#923)"),
    ("expedition_retrieval", "expedition_retrieval_probe.py",
     "player-driven remote retrieval end to end: command-time capacity refusal, "
     "travel/pickup/carrier identity, survival interruption, save-restart-load "
     "mid-journey, deposit into colony storage, reuse by another colonist (#920)"),
    ("farm_ai", "farm_ai_probe.py",
     "farm AI: world.plantRowCropAt + CropPlot auto-harvest scan + rot/occupancy gating (#336)"),
    ("flora_growth", "flora_growth_probe.py",
     "derived flora growth/age/phase under the advancing calendar (#332)"),
    ("follow_command_priority", "follow_command_priority_probe.py",
     "follow-command priority against other AI goals (#306)"),
    ("foraging", "foraging_probe.py",
     "foraging AI + harvestable-flora gating (#94)"),
    ("infection", "infection_probe.py",
     "infection growth/prevention/cure/sepsis loop end-to-end"),
    ("injury_log", "injury_log_probe.py",
     "injury-log stream roundtrip: emit/drain, unit.injure, "
     "emitEventForUnit, a real fall's event"),
    ("item_instance", "item_instance_probe.py",
     "per-instance item identity (#67)"),
    ("item_temp", "item_temp_probe.py",
     "item temperature model (#344)"),
    ("location_content", "location_content_probe.py",
     "location content spawning + ruin probe (#90, #91)"),
    ("location_embark", "location_embark_probe.py",
     "offscreen embark-to-discovery integration: zoom-map icons, portal ghost/"
     "remote-modal flow, real-order-driven discovery, save/reload (#782; needs a GPU)"),
    ("location_overlay", "location_overlay_probe.py",
     "world-gen location-overlay placement (#89)"),
    ("location_stamp_idempotent", "location_stamp_idempotent_probe.py",
     "geometry-stamp idempotency survives a cleared anchor floor + save/reload (#424), "
     "and the stamped 5x5 footprint really materialized level (#1575)"),
    ("lua_orphan_prune", "lua_orphan_prune_probe.py",
     "Lua per-id AI state pruned (not inherited) after a save load (#195)"),
    ("lunge", "lunge_probe.py",
     "combat lunge leap -> land -> strike on a short-reach species (#1713): "
     "the landing strike fires once with the launched target and the "
     "lunge's own stored reach/impact speed rather than timing out, and a "
     "replaced target, an aged-out timeout and a launch the engine never "
     "lifts each cancel it with all seven lunge fields cleared"),
    ("machine_shop", "machine_shop_probe.py",
     "electric furnace smelt recipe + machine_shop wiring/motor recipes, real content (#591)"),
    ("meal_waste", "meal_waste_probe.py",
     "stop-before-waste meal policy: a mostly-wasted discrete ration is "
     "withheld, bulk food still finishes the meal, the first item is "
     "exempt, entry gates and the 10-feed bound unchanged (#1219)"),
    ("medic_coord", "medic_coord_probe.py",
     "bestMedicFor/medicAvailable distance-discounted selection fix"),
    ("mental_efficiency", "mental_efficiency_probe.py",
     "mental-effectiveness combat/craft tie-ins: getMentalEffectiveness, "
     "craft-bill progress rate, craft-quality delta, damage-energy invariance (#353)"),
    ("mental_state", "mental_state_probe.py",
     "mental-state ladder: stressed hysteresis, break episodes + wander/flee AI, euphoria (#352)"),
    ("movement", "movement_probe.py",
     "obstacle-course movement: pathing/climbs/falls/ramps"),
    ("multiworld_save", "multiworld_save_probe.py",
     "multi-world save -> quit -> restart -> load; cross-page survival (#214, #219)"),
    ("offscreen", "offscreen_probe.py",
     "--offscreen render mode: windowless Vulkan boot, UI flow, screenshots, input injection, parallel instances (#650; needs a GPU)"),
    ("persistence_contract", "persistence_contract_probe.py",
     "compact fresh-process persistence contract smoke: three real "
     "save->load->save cycles compared structurally (SessionSnapshot Eq + "
     "lua.* payload byte-equality) through the real production codec, "
     "plus reset-policy and paused-stability checks (#767)"),
    ("persistence_contract_sweep", "persistence_contract_sweep.py",
     "broader persistence contract sweep: a real generated-world "
     "representative scenario (craft bill, mine designation, identity) "
     "through the same three-cycle fresh-process structural comparison "
     "(#767)"),
    ("persistence_integrity", "persistence_integrity_probe.py",
     "shared save/load integrity graph: a genuinely dangling Lua AI reference "
     "survives a save/restart/load round trip as a non-blocking diagnostic, "
     "and a corrupted save is rejected without touching the already-loaded "
     "live session (#764)"),
    ("physiology", "physiology_probe.py",
     "thermoregulation/circulation sanity across controlled environments"),
    ("plant", "plant_probe.py",
     "plant-designation layer + world.getPlantSuitability soil/factor gating (#335)"),
    ("portal_ghost", "portal_ghost_probe.py",
     "GPU offscreen visual check: build-tool ghost tint reflects location-exclusion validity (#778)"),
    ("portal_location", "portal_location_probe.py",
     "starting-building placement excluded from placed-location bounds; spawn/preview agreement (#778)"),
    ("power", "power_probe.py",
     "build-tool-routed power-node placement + wire connectivity + energy balance, save/load reconnect (#358, #360)"),
    ("position_hold", "position_hold_probe.py",
     "position hold after a completed player move order: sustained unpaused "
     "containment, work suppression vs a same-unit post-release resume, a "
     "displacing refill interrupt that returns to the anchor, and the "
     "supersede/release/internal-move boundaries (#1216)"),
    ("power_workshop", "power_workshop_probe.py",
     "job-dependent recipe power_draw: a station carrying no power_drain draws "
     "only while a bill is claimed AND actively worked (cbWorking) — idle at "
     "full generation is zero demand, a second recipe sums with the "
     "already-active one, and a paused continuing bill loses claim and demand "
     "once its permitted cycle completes; plus the unpowered craft.executeAt "
     "refusal, the wired-but-uncharged gate, the craft_job AI stalling at 0 "
     "progress while browned out then resuming, and battery storedWh "
     "rising/falling over a simulated day/night. bdPowerDrain / "
     "power.isBuildingPowered stay the hypothetical always-on non-crafting "
     "device path, not what this exercises (#361, #590, #796)"),
    ("preview", "preview_probe.py",
     "--preview real-boot browser, five independently runnable families behind "
     "one aggregate gate (--only simple|units|buildings|dispatch|zoom, #2089): "
     "simple-category list/focused-item discovery+selection+scroll+trimmed-loading "
     "via previewManager.dump() (#886), units animation viewer + promoted "
     "declaration + the shipped roster (#887/#1261), buildings viewer + "
     "flora/structures dispatch into the shared simple browser + the "
     "every-category no-placeholder sweep (#888), and centered bounded zoom "
     "over every display kind (#1907)"),
    ("preview_cli", "preview_cli_probe.py",
     "argv-to-dispatch CLI contract, no boot: canonical --preview category classification "
     "(unexposed names, grouped-no-item guidance), pre-boot item-path rejection "
     "(nonexistent/containment/directory) for simple items (#886), units (#887), and "
     "flora/buildings/structures items (#888); mode-incompatible flags (#1012/CH-58); and "
     "present-but-malformed values for every numeric flag, --dump= layer selection and --size, "
     "with omission still defaulting (#1191); and every malformed --region shape "
     "(non-numeric/too few/too many/partially numeric/no operand) rejected pre-boot with the "
     "omitted-flag default still reachable (#1481, CH-67)"),
    ("remote_warning_page_guard", "remote_warning_page_guard_probe.py",
     "remote-warning establishHere() revalidation guard against an active-world switch (#844)"),
    ("repair_item", "repair_item_probe.py",
     "unit.repairItem primitive (#300)"),
    ("repair", "repair_probe.py",
     "repair policy layer, station-gated (#301)"),
    ("etymology", "etymology_probe.py",
     "name etymology through the real offscreen UI: the world/location/river "
     "entry points all opening ONE panel, morpheme rows whose surface tokens "
     "reproduce the stored name, bound/free and recurrence presentation, the "
     "honest unavailable state, and resize/close/teardown (#1104)"),
    ("repair_ai", "repair_ai_probe.py",
     "repair AI: claim/fetch/walk/repair/return + role weighting (#302)"),
    ("resource_root", "resource_root_probe.py",
     "resource-root launch contract: built exe runs --dump/--headless from outside the repo (#636)"),
    ("river_naming", "river_naming_probe.py",
     "river identity + naming through Lua: stable GeoFeatureId ids attached to "
     "the same geometry call over call, generated name/gloss with a recurring "
     "head, the no-language fallback, and both surviving save/fresh-load and "
     "regeneration (#1102)"),
    ("retaliation_swap", "retaliation_swap_probe.py",
     "mid-fight retaliation target swap (#1483): the attack_target branch "
     "completes its tick on a live non-excluded recent attacker, the "
     "next-ordered unit still gets its update, and a hit past the window "
     "swaps nothing and raises nothing"),
    ("role", "role_probe.py",
     "derived unit-role hysteresis/demotion/work-XP growth (#265)"),
    ("save_compat_migration", "save_compat_migration_probe.py",
     "fresh-process migration probe: EVERY complete-session fixture the "
     "save-compat manifest declares loads, publishes, dwells paused, "
     "re-saves current-format, and reloads cleanly across a real process "
     "restart (#766, #1485)"),
    ("autosave", "autosave_probe.py",
     "interval autosave (#913): default-off dwell, a real one-minute interval "
     "firing, pause/time-scale restoration and its player-intent suppression, "
     "the failed-autosave pause ratchet, silent skips, manual-slot collision "
     "refusal, ordered rotation, and depth-reduction/disable retention"),
    ("save_pause", "save_pause_probe.py",
     "save/load pause-semantics regression (#42)"),
    ("scene_primitives", "scene_primitives_probe.py",
     "offscreen: engine.spawnText and a UI-layer engine.spawnSprite render at "
     "their declared layers and every mutation verb is reflected next frame, "
     "pixel-attributed per primitive (#2192; needs a GPU)"),
    ("scene_stats", "scene_stats_probe.py",
     "World.Render scene-assembly telemetry: debug.getSceneStats()'s ten-row "
     "shape and sequence, per-category scanned counts moving by exactly the "
     "population created, and the non-zero unit/ground-item/building emitted "
     "counts only a live offscreen texture system can produce (#1921)"),
    ("pause_speed", "pause_speed_probe.py",
     "the player's chosen world speed survives a pause imposed by a "
     "pause: true notification, a whole manual engine.saveWorld driven to a "
     "terminal outcome, and a save taken from an already-paused session; "
     "load policy still resumes at the default speed (#1599)"),
    ("save_barrier", "save_barrier_probe.py",
     "coordinated save owner acknowledgement and paused reload smoke test (#757)"),
    ("save_storage", "save_storage_probe.py",
     "atomic save-storage transaction: publish/retain generations, restart-and-select "
     "fallback across corrupted/missing/incompatible authoritative files, real "
     "disk-write-failure phase reporting (#762)"),
    ("sleep", "sleep_probe.py",
     "Sleeping pose + go_to_sleep AI goal: lie-down/wake chain, sleep_pressure regen, wake conditions (#612)"),
    ("startup_asset_logging", "startup_asset_logging_probe.py",
     "startup YAML logging ownership: one Info aggregate per registry family "
     "from scripts/startup_loader.lua (12 normal / 11 arena), per-file path + "
     "authoritative count only at CatAsset Debug (#1930; offscreen, needs a GPU)"),
    ("state_of_mind", "state_of_mind_probe.py",
     "unified consciousness/mood model: pain/awareness drift, no-hunger-config fallback, locomotor-collapse regression guard (#350)"),
    ("structure_rotation", "structure_rotation_probe.py",
     "GPU offscreen visual check: one stamped room captured at all four "
     "camera facings — rotated wall art, terrain interleave, billboard lift (#1712)"),
    ("text_encoding", "text_encoding_probe.py",
     "TE.decodeUtf8Lenient sweep across Engine.Scripting.Lua: malformed UTF-8 through engine.setText (#618) and the representative non-Text-API world.show boundary (#665) no longer errors, and a setText naming no scene node caches nothing for engine.getText to read back (#1961)"),
    ("thermo_altitude", "thermo_altitude_probe.py",
     "altitude-lapse thermal effect (#308)"),
    ("thought", "thought_probe.py",
     "thought event stream, state-of-mind-biased selection, thought-log data path (#351)"),
    ("till", "till_probe.py",
     "till-designation layer + till AI end to end, plantable contract (#333)"),
    ("tutorial", "tutorial_probe.py",
     "first-session tutorial integration gate: the shipped first_session "
     "branch driven end to end from real gameplay state (portal, discovered "
     "and radio-shared water, stepwise supplies), monotonic full-objective "
     "latching, and a fresh-process save/load round trip (#922)"),
    ("tutorial_hud", "tutorial_hud_probe.py",
     "GPU offscreen visual check: tutorial checklist HUD open/closed panel, "
     "transparent overlay over terrain, long-list wheel scrolling, and real "
     "clicks passing through its rows to gameplay (#960)"),
    ("item_list_widget", "item_list_widget_probe.py",
     "offscreen: the shared item-list widget across all three migrated hosts "
     "(cargo Contents, item-container Contents, unit-info inventory) — stack "
     "grouping/counts, parameterized tab layouts, tab selection, resize-"
     "restored selection, representative right-click routing, and the "
     "item-contents panel's deliberate absence of one (#1088)"),
    ("transactional_load", "transactional_load_probe.py",
     "whole-session load transaction: mutual exclusion, complete replacement "
     "(not merge), missing-def rejection leaves the old session intact, "
     "paused dwell + default-speed unpause, no ghost pages on repeat (#763)"),
    ("transfer_order", "transfer_order_probe.py",
     "transfer-order unit job (#1247): a queued order walks its carrier to "
     "the counterpart footprint and commits exactly once; the command-time "
     "and arrival capacity refusals; a partial batch; a stale instance; a "
     "vanished counterpart retiring quietly; a blocked approach stalling "
     "out; a progressing trip outliving the stall budget; and a mid-walk "
     "save/load round trip resuming to completion"),
    ("unified_transfer", "unified_transfer_probe.py",
     "offscreen: the unified transfer system end to end (#1255) - one "
     "fixed-seed session proving an exact instance moves in BOTH directions "
     "between all three endpoint classes (acolyte, technomule, built "
     "storage) through BOTH modes, a twelve-into-eight partial batch, the "
     "container-knowledge reveal rule, one widget rendering every container "
     "view, and a Mode B order surviving a fresh-process reload while a Mode "
     "A session does not"),
    ("transfer_context_menu", "transfer_context_menu_probe.py",
     "offscreen: right-click 'Transfer' on a built storage building and on "
     "the technomule, located via ui.dumpWidgets(), creates a session with "
     "the exact source/receiver identities and never falls through to a "
     "move order (#1014)"),
    ("wander_hazard", "wander_hazard_probe.py",
     "ambient wander never routes over a damaging drop: a sustained AI-live "
     "wander window on a 3-high ledge, setMoveSpeed retaining an in-flight "
     "request's policy, a protected command over the same edge, and an "
     "ordinary command that still falls (#1217)"),
    ("wire", "wire_probe.py",
     "wire autotile shape derivation + path-builder UX + construct_job wire AI (#359)"),
]


# ---------------------------------------------------------------------------
# Reserved port spans: how many ports a probe may bind (#1571)
#
# A probe is handed ONE `--port`, but two registered probes derive a
# second listener from it and keep both alive at once:
# `debug_console_boot_probe.py` boots its successful-bind and
# widget-module checks on `--port + 1`, and `offscreen_probe.py` starts a
# second offscreen engine on `--port + 1` while the first is still up.
# Stride-1 parallel allocation therefore handed a two-port probe its
# NEIGHBOUR's base: selecting `debug_console_boot` immediately before
# `transactional_load` under `--jobs 2` put both on 9401, and the
# resulting `Address already in use` read as a regression in probes that
# each pass alone.
#
# The fix is to make the port count DATA rather than something the
# allocator infers. A declared count N reserves the CONTIGUOUS span
# `base .. base + N - 1`; an undeclared probe reserves its base alone.
# Adding a future multi-port probe is one row here — nothing in the
# allocator, the GUI-port refusal, or `tools/probe_flake.py`'s lease
# scanner knows any probe by name. `tools/test_run_probes.py` validates
# every row against the live registry, the way it already does for
# `probe_runner_resources.EXCLUSIVE_RESOURCES`, so a renamed or retired
# probe cannot leave a
# stale declaration behind.
DEFAULT_PORT_SPAN = 1

PROBE_PORT_SPANS: dict[str, int] = {
    "debug_console_boot": 2,
    "offscreen": 2,
}


def port_span(key: str) -> int:
    """How many contiguous ports probe `key` may bind from its base."""
    return PROBE_PORT_SPANS.get(key, DEFAULT_PORT_SPAN)


def reserved_ports(key: str, base: int) -> range:
    """Every port probe `key` may bind when launched with `--port base`."""
    return range(base, base + port_span(key))


def allocate_parallel_ports(probes, base: int = PARALLEL_PORT_BASE) -> list[int]:
    """One base port per probe, spans laid end to end so none overlap.

    `probes` is a slice of `PROBES` (or anything whose items start with
    the probe key). The result is positional: index i is the `--port`
    probe i is launched with, and probe i's whole reserved span sits
    strictly below probe i+1's base. Pure arithmetic over the declared
    counts — it starts nothing, so a caller can validate the entire plan
    before the first subprocess exists.
    """
    ports: list[int] = []
    nxt = base
    for probe in probes:
        ports.append(nxt)
        nxt += port_span(probe[0])
    return ports


def gui_port_conflicts(allocations) -> list[tuple[str, int]]:
    """The `(key, base)` allocations whose reserved span covers the GUI port.

    `allocations` is an iterable of `(key, base)`. Checking the SPAN
    rather than the base is the point: `--port 8007` never equals 8008,
    but it puts `debug_console_boot`'s second engine straight onto the
    user's running game.

    A repeated `(key, base)` is reported once. A parallel plan lists a
    probe twice — its own allocation and the solo-retry origin — and the
    same pair twice over is one mistake, not two.
    """
    seen: set[tuple[str, int]] = set()
    conflicts: list[tuple[str, int]] = []
    for key, base in allocations:
        if (key, base) in seen or GUI_PORT not in reserved_ports(key, base):
            continue
        seen.add((key, base))
        conflicts.append((key, base))
    return conflicts


def describe_gui_conflicts(conflicts) -> str:
    """A refusal naming each probe and the span that made it one."""
    parts = []
    for key, base in conflicts:
        span = reserved_ports(key, base)
        where = (f"port {base}" if len(span) == 1
                 else f"ports {span.start}-{span.stop - 1}")
        parts.append(f"{key} would reserve {where}")
    return (f"refusing to use port {GUI_PORT} (the user's GUI port, see "
            f"CLAUDE.md): " + "; ".join(parts))



# farm_ai_probe.py's O(n^2) TCP tile scan over natural terrain (#336) is the
# slowest probe in the ordinary runtime class at ~11.5 min solo on a warm dev
# machine (#721), so the shared default needs real margin above that. A probe
# whose declared scenario structurally exceeds this class belongs in the
# validated override table below rather than making every hung probe wait for
# the exceptional case's budget.
DEFAULT_TIMEOUT = 900.0

# Per-key defaults for registered scenarios whose complete, expected workload
# exceeds DEFAULT_TIMEOUT. An explicit CLI --timeout remains authoritative for
# every selected key. save_compat_migration runs every manifest-declared
# complete-session fixture through two real engine processes and two real-codec
# GHCi dumps; its 20-fixture clean reference run took 2311 s, so 3600 s gives
# useful loaded-machine and manifest-growth margin without weakening the normal
# 900 s hang bound for the rest of the registry.
PROBE_TIMEOUT_OVERRIDES: dict[str, float] = {
    "save_compat_migration": 3600.0,
}


def timeout_override_problems(
        probes=None, overrides: dict[str, float] | None = None) -> list[str]:
    """Validate per-key default timeout declarations against the registry."""
    registry = PROBES if probes is None else probes
    declared = PROBE_TIMEOUT_OVERRIDES if overrides is None else overrides
    known = {key for key, _, _ in registry}
    problems = []
    for key, timeout in sorted(declared.items()):
        if key not in known:
            problems.append(f"timeout override names unknown probe key {key!r}")
        if (isinstance(timeout, bool) or not isinstance(timeout, (int, float))
                or not math.isfinite(timeout) or timeout <= 0):
            problems.append(
                f"timeout override for {key!r} must be finite and positive "
                f"(got {timeout!r})")
    return problems


def effective_timeout(key: str, explicit: float | None = None) -> float:
    """The CLI override, key-specific default, or ordinary default."""
    if explicit is not None:
        return explicit
    return PROBE_TIMEOUT_OVERRIDES.get(key, DEFAULT_TIMEOUT)


def format_timeout(timeout: float) -> str:
    return f"{timeout:g}s"


def timeout_plan(chosen, explicit: float | None = None) -> str:
    """Compact human-readable budget summary for one selected run."""
    budgets = sorted({effective_timeout(key, explicit) for key, _, _ in chosen})
    if len(budgets) == 1:
        return f"timeout {format_timeout(budgets[0])} each"
    rendered = ", ".join(format_timeout(timeout) for timeout in budgets)
    return f"per-probe timeouts {rendered}"


def _only_tokens(only: str) -> list[str]:
    """The trimmed, non-empty comma-separated tokens of a `--only` value."""
    return [n.strip() for n in only.split(",") if n.strip()]


def select(only: str | None, exact: bool = False) -> list[tuple[str, str, str]]:
    if not only:
        return list(PROBES)
    needles = _only_tokens(only)
    if exact:
        # Match probe KEYS exactly. The CI gate (#530) needs this: a
        # substring "craft" would otherwise also pull in "craft_bill".
        wanted = set(needles)
        return [p for p in PROBES if p[0] in wanted]
    selected = [p for p in PROBES if any(n in p[0] or n in p[1] for n in needles)]
    return selected


def unknown_exact_keys(only: str | None) -> list[str]:
    """`--exact` request tokens naming no registered probe key (#1321).

    Empty `only` selects every probe (nothing to reject); otherwise
    every requested token that `select(..., exact=True)` would silently
    drop, in request order. Used to tell a MIXED valid/invalid request
    apart from an all-invalid one, which keeps its own pre-existing
    empty-selection diagnostic.
    """
    if not only:
        return []
    known = {p[0] for p in PROBES}
    return [n for n in _only_tokens(only) if n not in known]

