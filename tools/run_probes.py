#!/usr/bin/env python3
"""Opt-in aggregate runner for the headless behavior probes (#370).

Each `tools/*_probe.py` script is a self-contained regression harness: it
boots its own headless engine (`--headless --port NNNN`), drives a scenario
over the debug-console TCP protocol, asserts on the result, and exits 0/1.
They're normally run one at a time, by hand, whichever is relevant to a
change. This script runs a selection of them back-to-back and prints a
single PASS/FAIL summary.

Probes each own their engine (boot + teardown). By default they run one at
a time; `--jobs N` runs up to N CONCURRENTLY, each an independent engine on
a unique port (#531). Probes canNOT share a single engine — 8 neutralise
the global `unit_ai.update`, 37 load defs engine-wide, many reuse the same
world/page names, and 16 restart the engine, so there is no clean per-
scenario isolation on one long-lived engine; running independent engines in
parallel gets the speed without the isolation problem. A full sequential
run is low tens of minutes; `--jobs` cuts wall-time to ~total/N (bounded by
the slowest single probe). This is NOT part of any default test tier (see
CLAUDE.md Testing Tiers) — run it deliberately, and prefer `--only`.

Usage:
  python3 tools/run_probes.py                  # run everything, sequentially
  python3 tools/run_probes.py --jobs 4         # up to 4 probes at once
  python3 tools/run_probes.py --only combat,movement
  python3 tools/run_probes.py --list
  python3 tools/run_probes.py --port 9500       # override every probe's --port
  python3 tools/run_probes.py --timeout 300

Probes are launched into their own session, and this runner reaps that
process group after EVERY completion path — not just the timeout (#1323).
A probe that dies of an unexpected exception after booting its engine
never reaches its own teardown, and the engine then outlives it holding
the probe's port; see `reap_group` below.

Exit 0 = all selected probes passed. 1 = at least one failed. 2 = bad
invocation (e.g. --only matched nothing). 130 = interrupted with Ctrl-C,
after terminating every probe still running and the engine it booted.
"""
from __future__ import annotations
import argparse
import concurrent.futures
import os
import signal
import subprocess
import sys
import threading
import time

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Base for the unique per-probe ports handed out in parallel mode (--jobs).
# Clear of the GUI port 8008. Each probe in a concurrent batch is launched
# with an explicit --port = PARALLEL_PORT_BASE + index override, so this
# range never needs to be clear of any probe's own default port — the
# override always wins over it.
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
     "offscreen: committed CtBuilding blueprint renders its full def footprint "
     "while staying one designation job (#807)"),
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
     "bind unchanged (#1190)"),
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
     "injury-log stream roundtrip: emit/drain, unit.injure, emitEventForUnit"),
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
     "geometry-stamp idempotency survives a cleared anchor floor + save/reload (#424)"),
    ("lua_orphan_prune", "lua_orphan_prune_probe.py",
     "Lua per-id AI state pruned (not inherited) after a save load (#195)"),
    ("lua_strict_msg", "lua_strict_msg_probe.py",
     "a Haskell exception embedded in a LuaToEngineMsg/LuaMsg field must not escape and crash the engine (#622)"),
    ("machine_shop", "machine_shop_probe.py",
     "electric furnace smelt recipe + machine_shop wiring/motor recipes, real content (#591)"),
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
    ("power_workshop", "power_workshop_probe.py",
     "requires_power workshop consumer: unpowered refusal, wired-uncharged gate, AI stall/resume, day/night balance (#361)"),
    ("preview", "preview_probe.py",
     "--preview real-boot browser: simple-category list/focused-item "
     "discovery+selection+scroll+trimmed-loading via previewManager.dump() (#886), "
     "units animation viewer (#887), buildings viewer + flora/structures dispatch "
     "into the shared simple browser + the every-category no-placeholder sweep (#888)"),
    ("preview_cli", "preview_cli_probe.py",
     "argv-to-dispatch CLI contract, no boot: canonical --preview category classification "
     "(unexposed names, grouped-no-item guidance), pre-boot item-path rejection "
     "(nonexistent/containment/directory) for simple items (#886), units (#887), and "
     "flora/buildings/structures items (#888); mode-incompatible flags (#1012/CH-58); and "
     "present-but-malformed values for every numeric flag, --dump= layer selection and --size, "
     "with omission still defaulting (#1191)"),
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
    ("role", "role_probe.py",
     "derived unit-role hysteresis/demotion/work-XP growth (#265)"),
    ("save_compat_migration", "save_compat_migration_probe.py",
     "fresh-process migration probe: a tracked pre-#760 B1 fixture loads, "
     "publishes, dwells paused, re-saves current-format, and reloads "
     "cleanly across a real process restart (#766)"),
    ("autosave", "autosave_probe.py",
     "interval autosave (#913): default-off dwell, a real one-minute interval "
     "firing, pause/time-scale restoration and its player-intent suppression, "
     "the failed-autosave pause ratchet, silent skips, manual-slot collision "
     "refusal, ordered rotation, and depth-reduction/disable retention"),
    ("save_pause", "save_pause_probe.py",
     "save/load pause-semantics regression (#42)"),
    ("save_barrier", "save_barrier_probe.py",
     "coordinated save owner acknowledgement and paused reload smoke test (#757)"),
    ("save_storage", "save_storage_probe.py",
     "atomic save-storage transaction: publish/retain generations, restart-and-select "
     "fallback across corrupted/missing/incompatible authoritative files, real "
     "disk-write-failure phase reporting (#762)"),
    ("sleep", "sleep_probe.py",
     "Sleeping pose + go_to_sleep AI goal: lie-down/wake chain, sleep_pressure regen, wake conditions (#612)"),
    ("state_of_mind", "state_of_mind_probe.py",
     "unified consciousness/mood model: pain/awareness drift, no-hunger-config fallback, locomotor-collapse regression guard (#350)"),
    ("text_encoding", "text_encoding_probe.py",
     "TE.decodeUtf8Lenient sweep across Engine.Scripting.Lua: malformed UTF-8 through engine.setText (#618) and the representative non-Text-API world.show boundary (#665) no longer errors, and the Text-API case round-trips through engine.getText"),
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
    ("transfer_context_menu", "transfer_context_menu_probe.py",
     "offscreen: right-click 'Transfer' on a built storage building and on "
     "the technomule, located via ui.dumpWidgets(), creates a session with "
     "the exact source/receiver identities and never falls through to a "
     "move order (#1014)"),
    ("wire", "wire_probe.py",
     "wire autotile shape derivation + path-builder UX + construct_job wire AI (#359)"),
]

# farm_ai_probe.py's O(n^2) TCP tile scan over natural terrain (#336) is the
# slowest registered probe at ~11.5 min solo on a warm dev machine (#721) —
# the default needs real margin above that for CI's slower runner.
DEFAULT_TIMEOUT = 900.0

# How long a signalled probe process group gets to leave on its own before
# the escalation to SIGKILL. One grace period shared by the timeout path
# (which has always spent it waiting on the probe leader) and the
# after-every-completion reap below (which spends it polling the GROUP,
# because by then the leader has already been reaped).
GROUP_GRACE = 10.0

# How long to wait for a SIGKILLed group to actually empty. Short by
# design: SIGKILL cannot be blocked, so this covers the asynchronous
# delivery and teardown — during which the engine still holds its port —
# rather than giving anything a second chance to shut down cleanly.
KILL_SETTLE = 5.0


# --------------------------------------------------------------------------
# Process-group teardown (#1323)
# --------------------------------------------------------------------------
def _signal_group(pgid: int, sig: int) -> bool:
    """Send ``sig`` to a whole process group; True if the group was there.

    ``ProcessLookupError`` (ESRCH) means the group is already empty, which
    is the ORDINARY outcome for a probe that tore its own engine down —
    success, not an error, and it must leave the probe's recorded result
    alone. ``PermissionError`` means a member exists that we may not
    signal, so the group is still alive.
    """
    try:
        os.killpg(pgid, sig)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _group_alive(pgid: int) -> bool:
    """True while any process — running OR zombie — remains in the group."""
    return _signal_group(pgid, 0)


def _group_states(pgid: int) -> list[str] | None:
    """Process state of every member of ``pgid``, or None if unreadable."""
    try:
        done = subprocess.run(["ps", "-eo", "pgid=,state="],
                              capture_output=True, text=True, timeout=15)
    except (OSError, subprocess.SubprocessError):
        return None
    if done.returncode != 0:
        return None
    states = []
    for line in done.stdout.splitlines():
        parts = line.split()
        if len(parts) >= 2 and parts[0].isdigit() and int(parts[0]) == pgid:
            states.append(parts[1])
    return states


def _group_running(pgid: int) -> bool:
    """True while the group holds a member that has NOT yet exited.

    A zombie does not count. It has already exited — releasing its port
    and every other resource — and is only waiting to be reaped, which
    nothing may ever do for a process orphaned under an init that does not
    reap (a container's PID 1, typically). Signals cannot tell the two
    apart: a zombie-only group answers EPERM on macOS and succeeds on
    Linux, so `killpg` alone reports a long-dead engine as live and would
    spend the entire grace, and then the post-SIGKILL settle, waiting for
    something that has already released the port.

    Falls back to the signal answer when states cannot be read, erring
    towards waiting rather than towards calling a live engine gone.
    """
    states = _group_states(pgid)
    if states is None:
        return _group_alive(pgid)
    return any(not state.startswith("Z") for state in states)


def reap_group(pgid: int, grace: float | None = None) -> None:
    """Terminate anything still running in a probe's process group.

    Called on EVERY completion path (#1323) — success, ordinary nonzero
    exit, timeout, and an exception in the runner itself — not just on the
    timeout, because a probe that dies of an unexpected exception after
    booting its engine never reaches its own ``quit_engine``. The engine
    then outlives it holding the probe's port, and `--retries` (and the
    parallel solo-retry, which reuses PARALLEL_PORT_BASE) re-runs onto that
    held port, where #1190 aborts the boot and reports the leak as an
    unrelated "exited before READY".

    ``communicate()`` cannot detect that: ``probelib.boot`` redirects the
    engine's output to a log file rather than the runner's inherited pipe,
    so the pipe reaches EOF the moment the probe itself exits.

    Reaping an empty group is a no-op. Once ``communicate()`` has reaped
    the probe leader there is nothing left to wait on, so the SIGTERM grace
    is spent polling the GROUP for liveness before escalating to SIGKILL.

    It does not return until the group is OBSERVED empty (or the budget
    runs out). Sending SIGKILL is not the same as the group being gone —
    delivery and teardown are asynchronous — and until its last member
    exits, the engine still owns the listening port. Returning early would
    hand that port straight to the next `--retries` attempt and recreate
    the #1190 boot abort this reap exists to prevent.
    """
    if grace is None:
        grace = GROUP_GRACE
    # Signal-only fast path: an empty group is the ordinary case, and
    # answering it without spawning `ps` keeps this free for every probe
    # that tore its own engine down. It also means a recycled pid is never
    # signalled.
    if not _group_alive(pgid):
        return
    if not _group_running(pgid):
        return
    if not _signal_group(pgid, signal.SIGTERM):
        return
    if _wait_group_stopped(pgid, grace):
        return
    _signal_group(pgid, signal.SIGKILL)
    # SIGKILL cannot be blocked, so this is a short settle rather than
    # another full grace — the members are already condemned.
    if _wait_group_stopped(pgid, KILL_SETTLE):
        return
    # Say so rather than let the retry fail as an unexplained "exited
    # before READY", which is the confusion #1323 is about.
    print(f"warning: process group {pgid} still had a running member after "
          f"SIGKILL; a retry reusing its port may fail to bind",
          file=sys.stderr)


def _wait_group_stopped(pgid: int, budget: float) -> bool:
    """Poll until nothing in the group is still running, or time runs out."""
    deadline = time.monotonic() + budget
    while True:
        if not _group_running(pgid):
            return True
        if time.monotonic() >= deadline:
            return False
        time.sleep(0.2)


class _DeferSigint:
    """Hold a Ctrl-C until a freshly spawned probe is trackable.

    The interpreter checks for signals BETWEEN bytecodes, so a
    KeyboardInterrupt can land after `Popen` has already forked and even
    between that call and the assignment naming its result. A probe
    spawned in that window is one nothing can reach: `groups` never
    learned its pgid, and the `finally` that reaps has not been entered —
    so Ctrl-C leaves it, and the engine it goes on to boot, running.

    Deferring by HANDLER rather than by `pthread_sigmask` is deliberate: a
    blocked mask survives fork/exec (measured), so masking here would hand
    every probe a SIGINT it could no longer receive. Swapping the handler
    leaves the child alone, since exec resets handlers anyway.

    Only the main thread may hold a signal handler, and only the main
    thread is ever sent KeyboardInterrupt — so off it this is a no-op,
    which is exactly the `--jobs` worker case.
    """

    def __init__(self) -> None:
        self._caught = False
        self._previous = None
        self._armed = False

    def _record(self, signum, frame) -> None:
        self._caught = True

    def __enter__(self) -> "_DeferSigint":
        if threading.current_thread() is threading.main_thread():
            try:
                self._previous = signal.signal(signal.SIGINT, self._record)
                self._armed = True
            except (ValueError, OSError):
                self._armed = False
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        if self._armed:
            signal.signal(signal.SIGINT, self._previous)
            self._armed = False
        # Only synthesize the interrupt when nothing else is already
        # propagating: that exception reaches the same teardown, and
        # replacing it would hide why the launch failed.
        if self._caught and exc_type is None:
            raise KeyboardInterrupt
        return False


def _terminate_probe(proc: subprocess.Popen, pgid: int,
                     grace: float | None = None) -> str:
    """End a probe that is still running, and collect its output.

    SIGTERM the group, give it ``grace`` to leave, SIGKILL what is left.
    The LEADER is reaped here (via ``communicate``) as well as signalled,
    which is what lets `reap_group` afterwards find an empty group at once
    instead of waiting out its own grace on a zombie.

    Shared by the two paths that have to stop a live probe: the wall-clock
    timeout, and a probe launched into an already-starting shutdown.
    """
    if grace is None:
        grace = GROUP_GRACE
    _signal_group(pgid, signal.SIGTERM)
    try:
        out, _ = proc.communicate(timeout=grace)
    except subprocess.TimeoutExpired:
        _signal_group(pgid, signal.SIGKILL)
        out, _ = proc.communicate()
    return out


class ProbeGroups:
    """The probe process groups running right now, plus the stop flag.

    Ctrl-C reaches the RUNNER only: every probe is launched into its own
    session (``start_new_session=True``), so the terminal's SIGINT never
    touches a probe, let alone the engine it booted. The runner therefore
    has to signal them itself — in the sequential path and the ``--jobs``
    path alike — and has to stop worker threads from picking up the next
    queued probe while it does.
    """

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._pgids: set[int] = set()
        self.stopping = threading.Event()

    def add(self, pgid: int) -> None:
        with self._lock:
            self._pgids.add(pgid)

    def discard(self, pgid: int) -> None:
        with self._lock:
            self._pgids.discard(pgid)

    def reap_all(self) -> None:
        with self._lock:
            pgids = sorted(self._pgids)
        for pgid in pgids:
            reap_group(pgid)


def select(only: str | None, exact: bool = False) -> list[tuple[str, str, str]]:
    if not only:
        return list(PROBES)
    needles = [n.strip() for n in only.split(",") if n.strip()]
    if exact:
        # Match probe KEYS exactly. The CI gate (#530) needs this: a
        # substring "craft" would otherwise also pull in "craft_bill".
        wanted = set(needles)
        return [p for p in PROBES if p[0] in wanted]
    selected = [p for p in PROBES if any(n in p[0] or n in p[1] for n in needles)]
    return selected


def run_one(script: str, port: int | None, timeout: float,
            groups: ProbeGroups | None = None):
    cmd = ["python3", os.path.join("tools", script)]
    if port is not None:
        cmd += ["--port", str(port)]
    if groups is not None and groups.stopping.is_set():
        # The runner is tearing down; a queued worker must not boot one
        # more engine on its way out.
        return False, False, 0.0, "(not started: the runner is shutting down)\n"
    start = time.time()
    proc = None
    pgid = None
    launched_into_shutdown = False
    timed_out = False
    try:
        # The launch window is held against Ctrl-C: a KeyboardInterrupt
        # between the spawn and `pgid` being recorded would leave a probe
        # nothing knows about — outside `groups`, and outside this
        # `finally` — surviving the interrupt with its engine.
        with _DeferSigint():
            proc = subprocess.Popen(
                cmd, cwd=REPO_ROOT,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                text=True, start_new_session=True,
            )
            # `start_new_session=True` makes the child a session AND
            # process-group leader, so its pgid IS its pid. Capture it here
            # rather than calling os.getpgid(proc.pid) later, for two
            # reasons: once communicate() reaps the leader that call
            # raises, leaving the descendants we still have to reap
            # unaddressable; and calling it right now could race the
            # child's own setsid() and hand back the RUNNER's group.
            pgid = proc.pid
            # Did the runner begin shutting down between the check above
            # and this Popen? `reap_all` may then already have taken its
            # snapshot without this group in it, so this call has to own
            # the escalation itself.
            if groups is not None:
                groups.add(pgid)
                launched_into_shutdown = groups.stopping.is_set()
        # Leaving that block re-raises a deferred Ctrl-C — here, inside the
        # try, so the reap below owns it.
        if launched_into_shutdown:
            # A bare SIGTERM is not enough: a probe that ignores it would
            # otherwise sit in communicate() for the whole `--timeout`
            # (900 s by default) before the finally below could escalate,
            # while the interrupted runner waits on this worker.
            out = _terminate_probe(proc, pgid)
            rc = -1
        else:
            try:
                out, _ = proc.communicate(timeout=timeout)
                rc = proc.returncode
            except subprocess.TimeoutExpired:
                timed_out = True
                out = _terminate_probe(proc, pgid)
                rc = -1
        # Measured BEFORE the reap below, so a probe's recorded elapsed
        # time stays the probe's own and never absorbs teardown's grace.
        elapsed = time.time() - start
    finally:
        # Every exit from here on, including a KeyboardInterrupt raised in
        # the runner while communicate() was blocked, and one deferred
        # across the launch window above. `pgid` is None only when the
        # spawn itself failed, in which case there is nothing to reap.
        if pgid is not None:
            reap_group(pgid)
            if groups is not None:
                groups.discard(pgid)
    ok = (rc == 0) and not timed_out
    return ok, timed_out, elapsed, out or ""


def run_with_retry(script, port, timeout, retries, announce=None, groups=None):
    """Run a probe, re-running SOLO up to `retries` times on failure.

    Returns (status, elapsed, out, attempts). `announce(kind, attempt,
    retries)` is an optional callback for live progress before each retry.
    A retry never starts before the previous attempt's group is reaped —
    `run_one` does that before it returns — so it can never be handed a
    port a leaked engine still holds.
    """
    attempt = 0
    while True:
        ok, timed_out, elapsed, out = run_one(script, port, timeout, groups)
        attempt += 1
        if ok or attempt > retries:
            break
        if announce:
            announce("TIMEOUT" if timed_out else "FAIL", attempt, retries)
    status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
    return status, elapsed, out, attempt


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--only", default=None,
                     help="comma-separated substrings matched against probe key/filename")
    ap.add_argument("--exact", action="store_true",
                     help="treat --only as exact probe KEYS, not substrings (the CI gate "
                          "uses this so e.g. 'craft' can't also select 'craft_bill')")
    ap.add_argument("--list", action="store_true", help="list known probes and exit")
    ap.add_argument("--port", type=int, default=None,
                     help="override every probe's --port (default: avoids 8008; "
                          "each probe keeps its own default if unset)")
    ap.add_argument("--timeout", type=float, default=DEFAULT_TIMEOUT,
                     help=f"per-probe wall-clock timeout in seconds (default {DEFAULT_TIMEOUT:.0f})")
    ap.add_argument("--tail", type=int, default=25,
                     help="lines of captured output to print for a failing probe")
    ap.add_argument("--retries", type=int, default=0,
                     help="on failure, re-run a probe SOLO up to N more times before "
                          "marking it failed — absorbs the sequential-engine contention "
                          "flakes seen in a back-to-back run (#530); a probe that passes "
                          "on any attempt counts as PASS")
    ap.add_argument("--jobs", type=int, default=1, metavar="N",
                     help="run up to N probes CONCURRENTLY, each its own engine on a "
                          "unique port (#531). Cuts wall-time to ~total/N. Default 1 = the "
                          "sequential behavior CI relies on. Since concurrency raises "
                          "contention, --retries re-runs failures SOLO after the parallel "
                          "batch. Cap N to (cores - 1) or so — each probe is a full engine.")
    args = ap.parse_args()

    if args.port == 8008:
        sys.exit("refusing --port 8008: that's the user's GUI port, see CLAUDE.md")

    chosen = select(args.only, exact=args.exact)
    if not chosen:
        print(f"--only {args.only!r} matched no probes; see --list", file=sys.stderr)
        return 2

    if args.list:
        for key, script, purpose in chosen:
            print(f"{key:28s} {script:32s} {purpose}")
        return 0

    n = len(chosen)
    results: dict[str, tuple[str, str, float, str]] = {}  # key -> (script, status, elapsed, out)
    wall_start = time.time()
    groups = ProbeGroups()

    try:
        if args.jobs <= 1:
            # Sequential — the mode CI relies on: live, ordered, inline retry.
            print(f"Running {n} probe(s) sequentially (timeout {args.timeout:.0f}s each)...\n")

            def announce(kind, attempt, retries):
                print(f"{kind}, retrying solo ({attempt}/{retries}) ... ", end="", flush=True)

            for i, (key, script, purpose) in enumerate(chosen, 1):
                print(f"[{i}/{n}] {script} ... ", end="", flush=True)
                status, elapsed, out, attempts = run_with_retry(
                    script, args.port, args.timeout, args.retries, announce, groups)
                note = f"  [passed on retry {attempts}]" if status == "PASS" and attempts > 1 else ""
                print(f"{status} ({elapsed:.1f}s){note}")
                if status != "PASS" and args.tail > 0:
                    for ln in out.splitlines()[-args.tail:]:
                        print(f"    {ln}")
                results[key] = (script, status, elapsed, out)
        else:
            # Parallel (#531) — one independent engine per probe, up to --jobs at
            # once, each on a unique port. No isolation issue: separate processes,
            # separate ports, unique save names. Retries run SOLO afterward,
            # since parallel contention is exactly what a retry needs to escape.
            jobs = args.jobs
            print(f"Running {n} probe(s), up to {jobs} concurrently "
                  f"(timeout {args.timeout:.0f}s each)...\n")

            def work(idx, probe):
                key, script, _ = probe
                ok, timed_out, elapsed, out = run_one(
                    script, PARALLEL_PORT_BASE + idx, args.timeout, groups)
                status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
                return key, script, status, elapsed, out

            with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as ex:
                # SUBMISSION is inside the try as well as completion: a
                # Ctrl-C partway through it would otherwise leave the `with`
                # directly, and shutdown(wait=True) then runs every future
                # submitted so far — launching probes, and engines, after
                # the interrupt. `futs` is grown as we go so the handler
                # sees exactly what has been submitted.
                futs: list[concurrent.futures.Future] = []
                try:
                    for i, probe in enumerate(chosen):
                        futs.append(ex.submit(work, i, probe))
                    for done, fut in enumerate(concurrent.futures.as_completed(futs), 1):
                        key, script, status, elapsed, out = fut.result()
                        print(f"[{done}/{n}] {script} ... {status} ({elapsed:.1f}s)")
                        results[key] = (script, status, elapsed, out)
                except BaseException:
                    # Ctrl-C, or any orchestration failure. cancel() alone
                    # cannot stop a work item a free worker already picked
                    # up, so `stopping` short-circuits those before they
                    # Popen anything; reap_all then takes down the engines
                    # already running. Leaving the `with` waits for the
                    # in-flight workers to finish their own cleanup.
                    groups.stopping.set()
                    for pending in futs:
                        pending.cancel()
                    groups.reap_all()
                    raise

            failed = [p for p in chosen if results[p[0]][1] != "PASS"]
            if failed and args.retries > 0:
                # The parallel batch was already the FIRST attempt, so a probe
                # gets exactly `--retries` more solo attempts here — total
                # attempts (1 + retries) match the sequential path, no bonus try.
                print(f"\nRe-running {len(failed)} failed probe(s) SOLO "
                      f"(up to {args.retries} more attempt(s) each; the parallel "
                      f"batch was the first)...")
                for key, script, _ in failed:
                    for r in range(1, args.retries + 1):
                        ok, timed_out, elapsed, out = run_one(
                            script, PARALLEL_PORT_BASE, args.timeout, groups)
                        status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
                        print(f"  {script} solo retry {r}/{args.retries} ... "
                              f"{status} ({elapsed:.1f}s)")
                        results[key] = (script, status, elapsed, out)
                        if ok:
                            break

            if args.tail > 0:
                for key, script, _ in chosen:
                    r = results[key]
                    if r[1] != "PASS":
                        print(f"\n--- {r[0]} ({r[1]}) ---")
                        for ln in r[3].splitlines()[-args.tail:]:
                            print(f"    {ln}")
    except KeyboardInterrupt:
        # Each run_one already reaped its own group on the way out; this is
        # the backstop for a group whose worker never got that far.
        groups.stopping.set()
        groups.reap_all()
        print("\ninterrupted — every probe still running, and the engine it "
              "booted, has been terminated", file=sys.stderr)
        return 130

    wall = time.time() - wall_start
    ordered = [(key, results[key][0], results[key][1], results[key][2]) for key, *_ in chosen]
    passed = sum(1 for _, _, status, _ in ordered if status == "PASS")
    probe_time = sum(elapsed for _, _, _, elapsed in ordered)
    extra = f" (wall {wall:.1f}s, {probe_time / wall:.1f}x)" if args.jobs > 1 and wall > 0 else ""
    print(f"\n{passed}/{n} passed, total probe-time {probe_time:.1f}s{extra}")
    if passed != n:
        print("FAILED:")
        for key, script, status, _ in ordered:
            if status != "PASS":
                print(f"  {status:8s} {script}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
