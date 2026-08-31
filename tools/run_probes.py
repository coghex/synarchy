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
its own RESERVED PORT SPAN (#531, #1571) — a probe that binds more than one
port declares how many in `PROBE_PORT_SPANS` below, and the allocator lays
the spans end to end so no two concurrent probes overlap. Probes canNOT
share a single engine — 8 neutralise the global `unit_ai.update`, 37 load
defs engine-wide, many reuse the same world/page names, and 16 restart the
engine, so there is no clean per-scenario isolation on one long-lived
engine; running independent engines in parallel gets the speed without that
problem. Separate processes and
separate ports are ALL that isolates — every probe still drives the same
checkout — so shared repository-relative resources are scheduled
reader/writer (#1322, #1444): every probe holds the resources named in
`IMPLICIT_SHARED_RESOURCES` below in a SHARED interest, and the few probes
that need one to themselves declare it in `EXCLUSIVE_RESOURCES`, which
holds such a probe back until nothing else is running and holds every other
probe back until it has finished.

The build directory is one of those resources (#1570). Probes used to
launch their engine as `cabal run exe:synarchy`, so a `--jobs N` sweep put
N concurrent Cabal processes on one `dist-newstyle` and an otherwise
healthy probe died on the inplace package database before its engine
started. This runner therefore resolves the executable ONCE — one
freshness build plus one `cabal list-bin`, in `engine_preflight` below,
after selection is validated and before any probe is spawned — and hands
every probe that absolute path through the environment, so no probe
process invokes Cabal while another is running. That preflight is itself
a Cabal writer, so it runs inside an EXCLUSIVE `cabal-build` hold: two
aggregate runs cannot build at once, and neither can a build and another
runner's `cabal repl` probe. The few probes that
legitimately still drive Cabal (GHCi consumers: `cabal repl` behind
`persistence_snapshot`/`save_compat_audit`) declare `cabal-build`
EXCLUSIVELY instead, which is the same scheduling mechanism keeping them
off everyone else's toes. Since #1436 the SAME two tables are also
enforced ACROSS processes, through `tools/probe_resource_lock.py`, so a
`/deflake` measurement or a second runner cannot overlap what this one is
holding either. A full sequential run is low tens of
minutes; `--jobs` cuts wall-time to ~total/N (bounded by the slowest single
probe). This is NOT part of any default test tier (see CLAUDE.md Testing
Tiers) — run it deliberately, and prefer `--only`.

Usage:
  python3 tools/run_probes.py                  # run everything, sequentially
  python3 tools/run_probes.py --jobs 4         # up to 4 probes at once
  python3 tools/run_probes.py --only combat,movement
  python3 tools/run_probes.py --list
  python3 tools/run_probes.py --port 9500       # override every probe's --port
  python3 tools/run_probes.py --jobs 4 --port 9500   # ... and base the spans there
  python3 tools/run_probes.py --timeout 300

Probes are launched into their own session, and this runner reaps that
process group after EVERY completion path — not just the timeout (#1323).
A probe that dies of an unexpected exception after booting its engine
never reaches its own teardown, and the engine then outlives it holding
the probe's port; see `reap_group` below.

Exit 0 = all selected probes passed. 1 = at least one failed. 2 = the run
never started — a bad invocation (e.g. --only matched nothing), an
unusable cross-process resource namespace, or an engine executable the
preflight could not resolve. 130 = interrupted with Ctrl-C, after
terminating every probe still running and the engine it booted.
"""
from __future__ import annotations
import argparse
import concurrent.futures
import contextlib
import math
import os
import signal
import subprocess
import sys
import threading
import time
from typing import NamedTuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_engine  # noqa: E402
import probe_protocol  # noqa: E402
import probe_resource_lock  # noqa: E402

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

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
    ("lua_strict_msg", "lua_strict_msg_probe.py",
     "a Haskell exception embedded in a LuaToEngineMsg/LuaMsg field must not escape and crash the engine (#622)"),
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
    ("state_of_mind", "state_of_mind_probe.py",
     "unified consciousness/mood model: pain/awareness drift, no-hunger-config fallback, locomotor-collapse regression guard (#350)"),
    ("structure_rotation", "structure_rotation_probe.py",
     "GPU offscreen visual check: one stamped room captured at all four "
     "camera facings — rotated wall art, terrain interleave, billboard lift (#1712)"),
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
# Shared repository-relative resources: the reader/writer conflict model
# (#1322, #1444)
#
# Two tables, one per interest. `IMPLICIT_SHARED_RESOURCES` is what EVERY
# registered probe holds SHARED; `EXCLUSIVE_RESOURCES` is what a named probe
# holds to itself. Shared holders coexist freely; an exclusive holder
# coexists with nothing else that names the same resource. The scheduler
# below reads only these two tables and knows nothing about any particular
# probe, so a future probe that must run alone is one row here. Since #1436
# `tools/probe_resource_lock.py` reads the same two tables through the same
# two accessors, so one conflict model covers both inside this process and
# between processes.
#
# `repo-config` is the tracked `config/` directory of THIS checkout.
#
# EXCLUSIVE side. `config_migration_probe.py` and `config_state_probe.py`
# each move the same three tracked legacy files
# (`config/{video,keybinds,notifications}.yaml`) aside into their OWN fixed
# /tmp backup directory and each delete the same three `config/*.local.yaml`
# paths before restoring — so run together, one probe's cleanup deletes or
# overwrites state the other owns and can leave a tracked file missing or
# holding fixture content in the primary checkout. (`config_state_probe.py`
# additionally owns `config/save.local.yaml`, which
# `config_migration_probe.py` does not touch; the three legacy paths and the
# other three local paths are shared.) Isolating them behind
# `--resource-root` is deliberately NOT the fix: `config_state_probe.py`
# asserts against the real tracked tree because proving the engine never
# dirties it is that probe's whole purpose (#638), and under an isolated
# root those assertions go vacuous.
#
# SHARED side (#1444). Those two are not the whole conflict set, because
# ENGINE INIT ITSELF writes `config/` when a local file is absent, and
# absent-local is precisely the fixture state both config probes install:
# `Engine.Asset.YamlNotifications.loadOverrides` materializes
# `config/notifications.local.yaml` from registry defaults, and
# `Engine.Core.Init.migrateLegacyConfig` copies a present legacy file over
# an absent local one for video/keybinds/notifications — or, for a
# video/keybinds placeholder it judges neutral (#1937), writes a
# `config/*.legacy-neutral.local.yaml` record instead. A foreign engine
# booting while a config probe has removed the local files but has not yet
# removed or installed every legacy file can therefore copy stale legacy
# content, or materialize registry defaults, that the config probe then
# reads back as its own result — a spurious verdict, not a corrupted
# checkout (both probes clear-then-restore from backups).
#
# So every probe declares the shared interest, rather than an enumerated
# engine-booting subset: the subset would be a second list to keep in sync
# with ~85 probes, and a probe that GAINS an engine boot must not silently
# lose the guard. Shared-against-shared never blocks, so the conservative
# reading costs the undeclared probes nothing.
#
# `cabal-build` is this checkout's shared Cabal build state — the one
# `dist-newstyle` every probe's engine comes out of (#1570). Since the
# preflight below resolves the executable once and every probe execs that
# binary, an ordinary probe only READS it, which is the shared interest.
# Three registered probes still drive Cabal themselves, and they are the
# reason the resource exists: `persistence_contract` and
# `persistence_contract_sweep` run `cabal repl test:synarchy-test-headless`
# through `persistence_snapshot.compare_session_files`, and
# `save_compat_migration` (and the other two) through
# `save_compat_audit.dump_canonical_summary`. A `cabal repl` recompiles into
# the same inplace package database whose concurrent mutation is the defect,
# so each of them takes `cabal-build` EXCLUSIVELY: two of them cannot
# overlap each other, and neither overlaps a probe reading the binary they
# may be relinking. They are deliberately RETAINED rather than converted —
# a GHCi consumer is not an engine boot and has no prebuilt equivalent.
#: The shared Cabal build state, named once so the preflight below, the
#: two declaration tables, and the direct path's own preparation (#1913)
#: cannot drift apart. The name is `probe_engine`'s, because that module
#: owns every Cabal contact a probe makes.
BUILD_RESOURCE = probe_engine.BUILD_RESOURCE

IMPLICIT_SHARED_RESOURCES: tuple[str, ...] = ("repo-config", BUILD_RESOURCE)

EXCLUSIVE_RESOURCES: dict[str, tuple[str, ...]] = {
    "config_migration": ("repo-config",),
    "config_state": ("repo-config",),
    "persistence_contract": (BUILD_RESOURCE,),
    "persistence_contract_sweep": (BUILD_RESOURCE,),
    "save_compat_migration": (BUILD_RESOURCE,),
}


def exclusive_resources(key: str) -> set[str]:
    """The resources probe ``key`` needs exclusively; empty when it declares none."""
    return set(EXCLUSIVE_RESOURCES.get(key, ()))


def shared_resources(key: str) -> set[str]:
    """The resources probe ``key`` touches but does not need exclusively.

    A resource the probe declared EXCLUSIVELY is subtracted rather than
    held twice: an interest is one or the other, never both, so releasing
    one cannot leave the other behind.
    """
    return set(IMPLICIT_SHARED_RESOURCES) - exclusive_resources(key)


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
# `EXCLUSIVE_RESOURCES`, so a renamed or retired probe cannot leave a
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


class ResourceLedger:
    """Which resources the running probes hold, and in which interest.

    One reader/writer lock per resource name: any number of probes may hold
    a resource SHARED at once, while an EXCLUSIVE holder runs only when no
    one else holds it at all — in either interest, in either direction. The
    shared side is a COUNT per resource, not a set: three concurrent readers
    must all release before a writer may start.

    Not thread-safe by design — the scheduler owns it from its own thread
    and the workers never touch it, which is what keeps a blocked probe out
    of a worker slot instead of parked inside one.

    In-process ONLY, and deliberately so: it is a plain object in one
    runner's memory, so it says nothing about a second runner or a
    `/deflake` measurement. `tools/probe_resource_lock.py` is the
    cross-process half (#1436), taken at the same dispatch point from the
    same two declaration tables; see the note below it.
    """

    def __init__(self) -> None:
        self._exclusive: set[str] = set()
        self._shared: dict[str, int] = {}

    def blocked(self, need_exclusive: set[str], need_shared: set[str]) -> bool:
        """True when these interests cannot be granted right now."""
        if need_exclusive & (self._exclusive | set(self._shared)):
            return True
        return bool(need_shared & self._exclusive)

    def acquire(self, need_exclusive: set[str], need_shared: set[str]) -> None:
        self._exclusive |= need_exclusive
        for name in need_shared:
            self._shared[name] = self._shared.get(name, 0) + 1

    def release(self, need_exclusive: set[str], need_shared: set[str]) -> None:
        self._exclusive -= need_exclusive
        for name in need_shared:
            remaining = self._shared.get(name, 0) - 1
            if remaining > 0:
                self._shared[name] = remaining
            else:
                # Dropped rather than kept at zero, so `blocked` can read
                # the keys as "held shared by someone" without counting.
                self._shared.pop(name, None)

    def idle(self) -> bool:
        """True when nothing is held, and therefore nothing can be blocked."""
        return not self._exclusive and not self._shared


# ---------------------------------------------------------------------------
# The same reader/writer model BETWEEN processes (#1436)
#
# The ledger above coordinates the probes inside ONE runner. It cannot see a
# second `run_probes.py`, and it cannot see a `/deflake` measurement, yet
# every one of them drives the same checkout's tracked `config/` tree — so
# `config_state_probe.py` holding `repo-config` exclusively stopped nothing
# outside its own sweep. `tools/probe_resource_lock.py` is the cross-process
# half, keyed by the same two declaration tables above so there is one
# conflict model rather than two.
#
# Both layers apply, and they compose in one direction: the ledger decides
# what may overlap WITHIN this sweep, and the lock decides whether it may
# overlap something outside it. A probe the ledger holds back never attempts
# the cross-process acquisition at all, which is what keeps this process from
# conflicting with itself — two of its own probes asking for the same
# resource in incompatible interests would otherwise meet each other's flock.
RESOURCE_WAIT_POLL = 5.0

# The namespace every worktree of this repository shares, resolved from git
# rather than from a path. `None` means "resolve it from REPO_ROOT"; it is
# overridable only so `tools/test_run_probes.py` can isolate its synthetic
# sweep from the real repository's live locks. Production never sets it.
RESOURCE_NAMESPACE: str | None = None


def resource_namespace() -> str:
    """The cross-process resource namespace for this checkout's repository."""
    if RESOURCE_NAMESPACE is not None:
        return RESOURCE_NAMESPACE
    return probe_resource_lock.repository_namespace(REPO_ROOT)


@contextlib.contextmanager
def resource_hold(key: str, namespace, *, announce=None):
    """Hold `key`'s cross-process interests around ONE probe execution.

    Entered immediately before the probe process is launched and left only
    once `run_one` has returned, which is after it has reaped the probe's
    whole process group — so a foreign holder never starts while this
    probe's engine is still up, exactly as the in-process ledger already
    guarantees within a sweep.

    Waiting happens HERE, in front of the launch, so it is never charged to
    the probe: `run_one` starts its own clock after this returns, so a
    probe's reported `elapsed` and its `--timeout` cover execution alone and
    a queued probe can never be reported as a TIMEOUT.

    `namespace` of None disables the cross-process layer entirely for
    callers that have no repository to name (the module's own helpers used
    as a library); the in-process ledger is unaffected either way.
    """
    if namespace is None:
        yield None
        return
    need_exclusive, need_shared = cross_process_interests(key, namespace)
    hold = probe_resource_lock.wait_acquire(
        exclusive=need_exclusive, shared=need_shared,
        namespace=namespace, purpose=f"run_probes {key}",
        poll=RESOURCE_WAIT_POLL, announce=announce)
    try:
        yield hold
    finally:
        hold.release()


# ---------------------------------------------------------------------------
# Resources an ANCESTOR already holds on this process's behalf (#1570)
#
# `persistence_contract_sweep` is a registered probe that itself invokes
# `run_probes.py` for the probes it cross-references. Once the sweep holds
# `cabal-build` EXCLUSIVELY, the flock its own nested runner takes for a
# child probe — in EITHER interest — conflicts with its ancestor's, and the
# nested runner would then wait forever for a holder that is itself blocked
# waiting on the nested runner.
#
# So a runner exports what it holds EXCLUSIVELY to every probe it launches,
# and a nested runner drops those names from its CROSS-PROCESS requests: it
# is inside its ancestor's exclusion, not competing with it. Its own
# in-process ledger is untouched, so a nested sweep still serialises its own
# probes against each other exactly as before.
#
# Only EXCLUSIVE holds are exported, and that is the whole rule. An
# ancestor's exclusive hold already excludes every foreign process, so a
# descendant inside it needs nothing further. An ancestor's SHARED hold
# cannot stand in for a descendant's exclusive request and must not be
# skipped — while a descendant's SHARED request against it is granted by the
# kernel anyway (LOCK_SH beside LOCK_SH), so there is nothing to export.
#
# The namespace rides along and is compared before anything is inherited: a
# name means nothing outside the repository it was taken in.
ENV_HELD_EXCLUSIVE = "SYNARCHY_PROBE_HELD_EXCLUSIVE"
ENV_HELD_NAMESPACE = "SYNARCHY_PROBE_HELD_NAMESPACE"

#: Every variable THIS runner owns in a probe's environment, so
#: `run_one` can strip the lot from what it inherited before
#: supplying its own — the same rule `probe_protocol` states for its
#: own four. A nested runner re-derives each of them, so nothing is
#: lost by dropping a value the parent set.
RUNNER_ENV_VARS: tuple[str, ...] = (probe_engine.ENV_ENGINE_EXE,
                                    ENV_HELD_EXCLUSIVE,
                                    ENV_HELD_NAMESPACE)


def descendant_hold_env(key: str, namespace: str | None) -> dict[str, str]:
    """What probe `key`'s own descendants may treat as already excluded.

    The probe's own exclusive declarations PLUS whatever this runner
    itself inherited, so the rule survives a second level of nesting.
    Empty when there is nothing to inherit, which is the ordinary case
    and passes no environment override at all.
    """
    if namespace is None:
        return {}
    held = exclusive_resources(key) | inherited_exclusive_resources(namespace)
    if not held:
        return {}
    return {ENV_HELD_EXCLUSIVE: ",".join(sorted(held)),
            ENV_HELD_NAMESPACE: namespace}


def inherited_exclusive_resources(namespace: str | None,
                                  environ=None) -> set[str]:
    """Resources an ancestor process already holds exclusively for us."""
    env = os.environ if environ is None else environ
    if namespace is None or env.get(ENV_HELD_NAMESPACE) != namespace:
        return set()
    raw = env.get(ENV_HELD_EXCLUSIVE) or ""
    return {name.strip() for name in raw.split(",") if name.strip()}


def cross_process_interests(key: str, namespace: str | None,
                            environ=None) -> tuple[set[str], set[str]]:
    """`key`'s (exclusive, shared) interests for the CROSS-PROCESS layer.

    The in-process ledger keeps using `exclusive_resources` /
    `shared_resources` unchanged; only the flocks drop what an ancestor
    is already holding exclusively on this process's behalf.
    """
    inherited = inherited_exclusive_resources(namespace, environ)
    return (exclusive_resources(key) - inherited,
            shared_resources(key) - inherited)


# ---------------------------------------------------------------------------
# The engine executable: resolved ONCE per run (#1570)
#
# Overridable only so `tools/test_run_probes.py` can drive the preflight
# with a deterministic subprocess double instead of a real toolchain.
# Production leaves it None and `main` fills it in.
ENGINE_EXECUTABLE: str | None = None

#: The subprocess entry point `engine_preflight` resolves through. Tests
#: substitute a recording double; production leaves it None, which is
#: `subprocess.run`.
ENGINE_PREFLIGHT_RUNNER = None


@contextlib.contextmanager
def preflight_hold(namespace, *, announce=None, environ=None):
    """Hold the shared Cabal build state EXCLUSIVELY across the preflight.

    The preflight is itself a Cabal WRITER — one `cabal build` into the
    same `dist-newstyle` every probe's engine comes out of — so resolving
    the executable outside the exclusion would leave exactly the race
    this issue is about, one level up: two aggregate runs preflighting at
    once, or one runner's build landing inside another runner's
    `persistence_contract` / `save_compat_migration` `cabal repl`. Nothing
    inside a single run can see that; only the cross-process lock can.

    Held for the build alone and released before any probe is dispatched,
    so the sweep's own probes are never queued behind it. `namespace` of
    None disables the cross-process layer, exactly as `resource_hold`
    does, and an ancestor already holding this resource exclusively is
    inherited rather than waited on — `persistence_contract_sweep`'s
    nested runner is inside its ancestor's exclusion, not competing with
    it.
    """
    if (namespace is None
            or BUILD_RESOURCE in inherited_exclusive_resources(namespace,
                                                                environ)):
        yield None
        return
    hold = probe_resource_lock.wait_acquire(
        exclusive={BUILD_RESOURCE}, namespace=namespace,
        purpose="run_probes engine preflight",
        poll=RESOURCE_WAIT_POLL, announce=announce)
    try:
        yield hold
    finally:
        hold.release()


def engine_preflight(namespace=None, environ=None, *, announce=None) -> str:
    """The one Cabal contact an aggregate run makes, before any probe.

    Adopts an executable an ANCESTOR already resolved when there is one —
    that is how `persistence_contract_sweep`'s nested runner reaches its
    own probes without a second build, and it takes no lock because it
    builds nothing. Otherwise it runs one freshness build plus one `cabal
    list-bin`, INSIDE `preflight_hold` so no other runner or GHCi consumer
    is in the build directory at the same time. Raises
    `EngineExecutableError`, which `main` reports as a nonzero exit before
    a probe is spawned, a retry allocated, or any probe assertion
    attributed to it.
    """
    inherited = probe_engine.runner_executable(environ)
    if inherited is not None:
        return inherited
    with preflight_hold(namespace, announce=announce, environ=environ):
        return probe_engine.resolve_executable(
            REPO_ROOT, run=ENGINE_PREFLIGHT_RUNNER)


# ── Durable progress records (#1768) ──────────────────────────────────
#
# A probe's stdout is a PIPE this runner drains only when the child ends
# (`run_one` below), and the child is launched as a plain `python3` with
# no `-u`, so an ordinary `print` sits in the child's own block buffer
# until it fills. When a slow probe is SIGKILLed at `--timeout`, that
# buffer dies with it and the artifact the operator reads names no phase
# at all.
#
# The fix is ONE convention, used by every producer that wants a record
# to survive termination, and recognized by the failure presentation at
# the other end. A progress record is a single flushed line:
#
#   #probe-progress# HH:MM:SS +12.3s | phase | engine A | build ... 'gen1'
#   #probe-progress# HH:MM:SS +0.1s  | begin | chop (chop_probe.py) attempt 1/2 | dispatched
#   #probe-progress# HH:MM:SS +45.2s | end   | chop (chop_probe.py) attempt 1/2 | PASS (45.1s)
#
# The four fields are the stamped marker, the KIND, the IDENTITY, and
# free-text detail. `begin` and `end` carry the SAME identity, which is
# what makes the in-flight set at termination derivable: every `begin`
# with no matching `end` is a nested attempt that never finished.
#
# The marker deliberately does not start with `[`: the runner's own
# verdict announcements do (`[3/12] chop_probe.py ... PASS`), and
# `tools/test_run_probes.py`'s `progress_lines` helper counts those by
# that shape. Keeping the two apart means a progress record can never be
# miscounted as a verdict.
PROGRESS_MARKER = "#probe-progress#"
PROGRESS_SEP = " | "
PROGRESS_KINDS = ("phase", "begin", "end")


class ProgressRecord(NamedTuple):
    """One parsed progress record: `stamp` is `HH:MM:SS +<elapsed>s`."""
    stamp: str
    kind: str
    identity: str
    detail: str


def format_progress(kind: str, identity: str, detail: str, *,
                    elapsed: float, now: float) -> str:
    """Render one progress record in the ONE shared convention.

    Both halves of the timing evidence are carried: a wall-clock time, so
    records from two processes sharing this pipe (a sweep and the runner
    it nests) can be ordered against each other, and an offset from the
    emitting producer's own start, so how long the last named phase
    occupied before a timeout is readable without arithmetic.
    """
    if kind not in PROGRESS_KINDS:
        raise ValueError(f"unknown progress kind {kind!r}; "
                         f"expected one of {PROGRESS_KINDS}")
    stamp = f"{time.strftime('%H:%M:%S', time.localtime(now))} +{elapsed:.1f}s"
    return PROGRESS_SEP.join(
        [f"{PROGRESS_MARKER} {stamp}", kind, identity, detail])


def parse_progress(line: str) -> ProgressRecord | None:
    """One progress record, or None for any other line of child output."""
    text = line.strip()
    if not text.startswith(PROGRESS_MARKER + " "):
        return None
    fields = text.split(PROGRESS_SEP)
    if len(fields) < 4:
        return None
    stamp = fields[0][len(PROGRESS_MARKER):].strip()
    kind = fields[1].strip()
    if kind not in PROGRESS_KINDS:
        return None
    # Detail is rejoined rather than taken as fields[3]: free text may
    # itself contain the separator, and only the first three fields are
    # structural.
    return ProgressRecord(stamp, kind, fields[2].strip(),
                          PROGRESS_SEP.join(fields[3:]).strip())


def attempt_identity(key: str, script: str, attempt: int, total: int) -> str:
    """The identity a nested attempt's `begin` and `end` records share.

    Names the registered probe KEY, its script, and which attempt of how
    many this is — so the record is self-describing, and so the pairing
    is exact even when one probe is retried while another is dispatched.
    """
    return f"{key} ({script}) attempt {attempt}/{total}"


class ProgressEmitter:
    """Emits progress records against one producer's own start time.

    Every record is flushed as it is written. That is the whole point:
    the emitting process is a probe (or a runner nested inside one) whose
    stdout is a pipe nobody reads until it exits, so an unflushed record
    would die in its buffer at `--timeout` — exactly the loss #1768 is
    about.
    """

    def __init__(self, start: float | None = None) -> None:
        self.start = time.time() if start is None else start

    def emit(self, kind: str, identity: str, detail: str) -> str:
        now = time.time()
        line = format_progress(kind, identity, detail,
                               elapsed=now - self.start, now=now)
        # `file` is left at its default so a caller redirecting
        # `sys.stdout` (this suite's own drivers do) still captures it.
        print(line, flush=True)
        return line

    def phase(self, identity: str, detail: str) -> str:
        return self.emit("phase", identity, detail)

    def begin(self, identity: str, detail: str = "dispatched") -> str:
        return self.emit("begin", identity, detail)

    def end(self, identity: str, detail: str) -> str:
        return self.emit("end", identity, detail)


def progress_attribution(out: str) -> list[str]:
    """Attribution lines for a failing probe's DEFAULT presentation.

    Reads the complete captured output — which `run_one` holds in full —
    and returns only a short summary: the latest phase the child entered,
    and every nested attempt it started without finishing. The ordinary
    `--tail` context is printed beside this, unchanged; the complete
    capture is deliberately NOT dumped.

    A capture holding no progress records yields nothing at all, so every
    probe that emits none has exactly the failure presentation it always
    had.
    """
    records = [record for record in
               (parse_progress(line) for line in out.splitlines())
               if record is not None]
    if not records:
        return []
    lines: list[str] = []
    phases = [record for record in records if record.kind == "phase"]
    if phases:
        last = phases[-1]
        lines.append(f"progress: latest phase entered at {last.stamp}: "
                     f"{last.identity} -- {last.detail}")
    # Insertion order is dispatch order, which is the order an operator
    # wants the still-running set named in.
    started: dict[str, ProgressRecord] = {}
    for record in records:
        if record.kind == "begin":
            started[record.identity] = record
        elif record.kind == "end":
            started.pop(record.identity, None)
    if started:
        lines.append(f"progress: {len(started)} nested probe attempt(s) "
                     f"still in flight when this run ended:")
        for identity, record in started.items():
            lines.append(f"    {identity}, dispatched at {record.stamp}")
    return lines


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
    parallel solo-retry, which reuses the allocation origin) re-runs onto that
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


def probe_protocol_env(event_path: str | None = None,
                       artifact_dir: str | None = None,
                       engine_log_dir: str | None = None,
                       rts_caps: int | None = None) -> dict[str, str]:
    """The `probe-result/v1` environment for one harnessed run (#1425).

    A migrated probe reads these to decide where its event stream,
    artifacts and engine logs go, and how many RTS capabilities every
    engine it boots gets. All four are optional and an empty result
    means "no protocol wiring", which is what an ordinary
    `run_probes.py` run passes.
    """
    env: dict[str, str] = {}
    if event_path is not None:
        env[probe_protocol.ENV_EVENTS] = str(event_path)
    if artifact_dir is not None:
        env[probe_protocol.ENV_ARTIFACT_DIR] = str(artifact_dir)
    if engine_log_dir is not None:
        env[probe_protocol.ENV_ENGINE_LOG_DIR] = str(engine_log_dir)
    if rts_caps is not None:
        env[probe_protocol.ENV_RTS_CAPS] = str(rts_caps)
    return env


def run_one(script: str, port: int | None, timeout: float,
            groups: ProbeGroups | None = None, *,
            event_path: str | None = None,
            artifact_dir: str | None = None,
            engine_log_dir: str | None = None,
            rts_caps: int | None = None,
            hold_env: dict[str, str] | None = None):
    """Launch one probe, capture it, and reap its whole process group.

    The four `probe-result/v1` keyword parameters are that protocol's
    wiring (#1425), handed to the child through the environment so a
    migrated probe needs no new command-line flags. `hold_env` is the
    same idea for the resources an ancestor holds exclusively on the
    child's behalf (#1570), which only matters to a probe that nests
    another runner. Every one defaults to None, which passes no
    environment override at all — so every pre-existing positional
    caller behaves exactly as it did.
    """
    cmd = ["python3", os.path.join("tools", script)]
    if port is not None:
        cmd += ["--port", str(port)]
    # `run_one` is the ONE authority on a child's protocol wiring: the
    # inherited environment's own SYNARCHY_PROBE_* variables are dropped
    # first, so a stale export in the operator's shell cannot silently
    # push an ordinary `run_probes.py` run into protocol mode (where a
    # probe would stop printing the human output the runner's failure
    # tail exists to show).
    protocol_env = probe_protocol_env(event_path, artifact_dir,
                                      engine_log_dir, rts_caps)
    child_env = {k: v for k, v in os.environ.items()
                 if k not in probe_protocol.PROTOCOL_ENV_VARS
                 and k not in RUNNER_ENV_VARS}
    child_env.update(protocol_env)
    # The engine every probe launches (#1570), resolved once by `main`'s
    # preflight. Stripped-then-set for the same reason the protocol
    # variables are: an operator's stale export must not decide which
    # binary a sweep runs. A caller that resolved nothing leaves the child
    # to prepare its own executable (#1913) — still one probe at a time
    # and still no concurrent Cabal, because that preparation takes
    # `cabal-build` exclusively before it builds. `tools/deflake.py`
    # fills this in for the de-flake lab's own runs, ahead of the hold
    # its measurement takes, so no child of a measurement prepares
    # anything.
    if ENGINE_EXECUTABLE is not None:
        child_env[probe_engine.ENV_ENGINE_EXE] = ENGINE_EXECUTABLE
    if hold_env:
        child_env.update(hold_env)
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
                text=True, start_new_session=True, env=child_env,
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


def run_with_retry(script, port, timeout, retries, announce=None, groups=None,
                   key=None, namespace=None, waiting=None):
    """Run a probe, re-running SOLO up to `retries` times on failure.

    Returns (status, elapsed, out, attempts). `announce(kind, attempt,
    retries)` is an optional callback for live progress before each retry.
    A retry never starts before the previous attempt's group is reaped —
    `run_one` does that before it returns — so it can never be handed a
    port a leaked engine still holds.

    `key` and `namespace` bring EVERY attempt, the first and each retry
    alike, inside the cross-process resource hold (#1436): a retry is
    another full probe execution, so a foreign exclusive holder must be
    able to stop it exactly as it stops the first attempt. `waiting` is an
    optional callback receiving the `ResourceBusy` that is holding this
    attempt up. Passing no `key` leaves the behaviour as it was, which is
    what `tools/test_run_probes.py`'s direct calls rely on.
    """
    attempt = 0
    hold_env = descendant_hold_env(key, namespace) if key else {}
    while True:
        with resource_hold(key, namespace if key else None, announce=waiting):
            ok, timed_out, elapsed, out = run_one(script, port, timeout, groups,
                                                  hold_env=hold_env)
        attempt += 1
        if ok or attempt > retries:
            break
        if announce:
            announce("TIMEOUT" if timed_out else "FAIL", attempt, retries)
    status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
    return status, elapsed, out, attempt


def main() -> int:
    global ENGINE_EXECUTABLE
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--only", default=None,
                     help="comma-separated substrings matched against probe key/filename")
    ap.add_argument("--exact", action="store_true",
                     help="treat --only as exact probe KEYS, not substrings (the CI gate "
                          "uses this so e.g. 'craft' can't also select 'craft_bill')")
    ap.add_argument("--list", action="store_true", help="list known probes and exit")
    ap.add_argument("--port", type=int, default=None,
                     help="base port. Sequentially it overrides every probe's "
                          "--port; with --jobs it is the ORIGIN the "
                          "non-overlapping per-probe spans are laid out from, "
                          "instead of the default 9400 (#1571). Unset, each "
                          "probe keeps its own default sequentially and the "
                          "parallel allocation starts at 9400. Never 8008, and "
                          "never a base whose span reaches it.")
    ap.add_argument(
        "--timeout", type=float, default=None,
        help=("explicit wall-clock timeout in seconds for every selected "
              f"probe (ordinary default {DEFAULT_TIMEOUT:g}; registered "
              "per-key defaults apply when omitted)"))
    ap.add_argument("--tail", type=int, default=25,
                     help="lines of captured output to print for a failing probe")
    ap.add_argument("--retries", type=int, default=0,
                     help="on failure, re-run a probe SOLO up to N more times before "
                          "marking it failed — absorbs the sequential-engine contention "
                          "flakes seen in a back-to-back run (#530); a probe that passes "
                          "on any attempt counts as PASS")
    ap.add_argument("--jobs", type=int, default=1, metavar="N",
                     help="run up to N probes CONCURRENTLY, each its own engine on its "
                          "own reserved port span (#531, #1571). Cuts wall-time to "
                          "~total/N. Default 1 = the "
                          "sequential behavior CI relies on. Since concurrency raises "
                          "contention, --retries re-runs failures SOLO after the parallel "
                          "batch. Cap N to (cores - 1) or so — each probe is a full engine.")
    args = ap.parse_args()

    if (args.timeout is not None
            and (not math.isfinite(args.timeout) or args.timeout <= 0)):
        print(f"--timeout must be finite and positive (got {args.timeout!r})",
              file=sys.stderr)
        return 2

    timeout_problems = timeout_override_problems()
    if timeout_problems:
        print("registered probe timeout declarations are unusable:",
              file=sys.stderr)
        for problem in timeout_problems:
            print(f"  - {problem}", file=sys.stderr)
        return 2

    if args.port == GUI_PORT:
        # The base itself. Refused here, before `--list` and before any
        # selection, so the obvious mistake never depends on which probes
        # were chosen. The SPAN-aware refusal below needs the selection
        # and therefore runs after it.
        sys.exit(f"refusing --port {GUI_PORT}: that's the user's GUI port, "
                 f"see CLAUDE.md")

    chosen = select(args.only, exact=args.exact)
    if args.exact:
        # A MIXED valid/invalid request is reported by naming the unknown
        # keys, before any listing or running. An ALL-invalid request
        # falls through to the empty-selection branch below unchanged
        # (`chosen` is empty there too, so this never fires for it).
        unknown = unknown_exact_keys(args.only)
        if unknown and chosen:
            print(f"--only {args.only!r} names unknown probe key(s) with "
                  f"--exact: {', '.join(unknown)}; see --list", file=sys.stderr)
            return 2
    if not chosen:
        print(f"--only {args.only!r} matched no probes; see --list", file=sys.stderr)
        return 2

    if args.list:
        for key, script, purpose in chosen:
            print(f"{key:28s} {script:32s} {purpose}")
        return 0

    # The WHOLE port plan is computed and validated here, before a single
    # subprocess exists (#1571): every port any selected probe may bind,
    # in the mode it is about to run in. A probe that would reach the GUI
    # port is a refusal, not a boot against the user's running game.
    parallel_base = PARALLEL_PORT_BASE if args.port is None else args.port
    parallel_ports = allocate_parallel_ports(chosen, parallel_base)
    if args.jobs <= 1:
        # Sequential hands every probe the same base, one at a time, so
        # there is nothing to lay out — but `--port 8007` still reaches
        # 8008 through a two-port probe's span.
        planned = ([] if args.port is None
                   else [(key, args.port) for key, _, _ in chosen])
    else:
        planned = [(key, port)
                   for (key, _, _), port in zip(chosen, parallel_ports)]
        if args.retries > 0:
            # A parallel failure is re-run SOLO from the allocation
            # origin, so that span is part of the plan too.
            planned += [(key, parallel_base) for key, _, _ in chosen]
    conflicts = gui_port_conflicts(planned)
    if conflicts:
        print(describe_gui_conflicts(conflicts), file=sys.stderr)
        return 2

    # Resolved ONCE, here: after --list has had its chance to return without
    # needing a repository, and before any probe runs, so an unusable
    # namespace is a loud refusal in the first second rather than a sweep
    # that silently coordinates with nobody. It precedes the executable
    # preflight below for exactly that reason — the preflight may spend a
    # build, and a namespace this run can never use should not wait behind
    # one.
    try:
        namespace = resource_namespace()
    except probe_resource_lock.ResourceLockError as error:
        print(f"cannot coordinate probe resources across processes: {error}",
              file=sys.stderr)
        return 2

    def waiting(busy) -> None:
        print(f"\n    waiting for {busy.resource!r} ({busy.interest}) held "
              f"outside this runner ... ", end="", flush=True)

    # The whole Cabal contact this run makes (#1570): one freshness build
    # plus one `cabal list-bin`, HERE — after `--list` and after every
    # selection and port refusal above, so a rejected or empty selection
    # stays build-free, and before a single probe process exists, so the
    # concurrent-`cabal run` race cannot happen at all. The build itself
    # runs inside an EXCLUSIVE `cabal-build` hold, because a preflight is
    # a Cabal writer like any other and a second runner's preflight (or
    # another runner's `cabal repl` probe) must not be in the build
    # directory beside it. A failure is this runner's own nonzero exit,
    # never a retry and never a probe's assertion failure.
    try:
        ENGINE_EXECUTABLE = engine_preflight(namespace, announce=waiting)
    except probe_engine.EngineExecutableError as error:
        print(f"cannot resolve the engine the probes launch: {error}",
              file=sys.stderr)
        return 2
    except probe_resource_lock.ResourceLockError as error:
        print(f"cannot coordinate the engine build across processes: {error}",
              file=sys.stderr)
        return 2
    print(f"engine: {ENGINE_EXECUTABLE}")

    n = len(chosen)
    results: dict[str, tuple[str, str, float, str]] = {}  # key -> (script, status, elapsed, out)
    wall_start = time.time()
    # #1768: the runner's own half of the shared progress convention. Its
    # records go to this process's stdout, which — when this runner is
    # NESTED inside a probe (the persistence sweep runs one) — is that
    # probe's captured pipe, so they survive the outer runner's timeout
    # kill and name which attempts were in flight.
    progress = ProgressEmitter(wall_start)
    total_attempts = args.retries + 1
    groups = ProbeGroups()

    try:
        if args.jobs <= 1:
            # Sequential — the mode CI relies on: live, ordered, inline retry.
            print(f"Running {n} probe(s) sequentially "
                  f"({timeout_plan(chosen, args.timeout)})...\n")

            def announce(kind, attempt, retries):
                print(f"{kind}, retrying solo ({attempt}/{retries}) ... ", end="", flush=True)

            for i, (key, script, purpose) in enumerate(chosen, 1):
                timeout = effective_timeout(key, args.timeout)
                print(f"[{i}/{n}] {script} ... "
                      f"[timeout {format_timeout(timeout)}] ",
                      end="", flush=True)
                status, elapsed, out, attempts = run_with_retry(
                    script, args.port, timeout, args.retries, announce,
                    groups, key=key, namespace=namespace, waiting=waiting)
                note = f"  [passed on retry {attempts}]" if status == "PASS" and attempts > 1 else ""
                print(f"{status} ({elapsed:.1f}s){note}")
                if status != "PASS" and args.tail > 0:
                    # #1768: the attribution first, then the ordinary tail
                    # as context. The attribution is drawn from the
                    # COMPLETE capture, so a phase record that more than
                    # `--tail` lines followed is still named; nothing else
                    # of the capture is printed.
                    for ln in progress_attribution(out):
                        print(f"    {ln}")
                    for ln in out.splitlines()[-args.tail:]:
                        print(f"    {ln}")
                results[key] = (script, status, elapsed, out)
        else:
            # Parallel (#531) — one independent engine per probe, up to --jobs at
            # once, each on a unique port. That isolates exactly two dimensions
            # and claims nothing further: separate processes cannot corrupt each
            # other's memory, and unique ports cannot collide on a bind. Every
            # probe still drives the SAME checkout, the same build directory and
            # the same repository-relative files, so the reader/writer ledger
            # above decides what may overlap (#1322, #1444) — the scheduling
            # below holds a conflicting probe back rather than letting a worker
            # discover the conflict, and takes the cross-process interest at
            # the same point (#1436) so a foreign runner or measurement cannot
            # overlap it either. Since #1570 the build directory is one of the
            # scheduled resources rather than an unguarded one: every probe
            # here execs the executable the preflight already resolved, and the
            # three GHCi consumers that still run Cabal themselves hold
            # `cabal-build` EXCLUSIVELY. Anything not named by either resource
            # table is still unguarded. Retries run SOLO afterward, since
            # parallel contention is exactly what a retry needs to escape.
            jobs = args.jobs
            print(f"Running {n} probe(s), up to {jobs} concurrently "
                  f"({timeout_plan(chosen, args.timeout)})...\n")

            def work(idx, probe):
                key, script, _ = probe
                timeout = effective_timeout(key, args.timeout)
                # `parallel_ports[idx]` is the BASE of this probe's own
                # reserved span, which the allocation above laid clear of
                # every other selected probe's (#1571). Never `base + idx`:
                # a two-port probe binds its neighbour's base under that.
                ok, timed_out, elapsed, out = run_one(
                    script, parallel_ports[idx], timeout, groups,
                    hold_env=descendant_hold_env(key, namespace))
                status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
                return key, script, status, elapsed, out, timeout

            with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as ex:
                # SUBMISSION is inside the try as well as completion: a
                # Ctrl-C partway through it would otherwise leave the `with`
                # directly, and shutdown(wait=True) then runs every future
                # submitted so far — launching probes, and engines, after
                # the interrupt. `futs` is grown as we go so the handler
                # sees exactly what has been submitted.
                futs: list[concurrent.futures.Future] = []
                # Submission is INTERLEAVED with completion rather than done
                # up front, because a probe waiting on a resource someone
                # else holds must not occupy a worker slot that an unrelated
                # ready probe could use. Holding it back here — rather than
                # taking a lock inside the worker — is what keeps `--jobs`
                # worth of real work in flight while a conflict is pending.
                pending = list(enumerate(chosen))
                running: dict[concurrent.futures.Future,
                              tuple[set[str], set[str],
                                    probe_resource_lock.ResourceHold]] = {}
                ledger = ResourceLedger()
                done = 0
                foreign = None
                try:
                    while pending or running:
                        # Dispatch in registry order every probe that fits and
                        # whose resources are free; a BLOCKED probe is skipped,
                        # never waited on, so later disjoint probes still start.
                        foreign = None
                        for item in list(pending):
                            if len(running) >= jobs:
                                break
                            i, probe = item
                            need_exclusive = exclusive_resources(probe[0])
                            need_shared = shared_resources(probe[0])
                            if ledger.blocked(need_exclusive, need_shared):
                                continue
                            # The flock request drops whatever an ancestor
                            # already holds exclusively for us (#1570); the
                            # ledger above keeps the full declarations, so
                            # this runner still serialises its own probes.
                            lock_exclusive, lock_shared = cross_process_interests(
                                probe[0], namespace)
                            # The cross-process half, taken at the SAME point
                            # and in the same non-blocking way (#1436): a probe
                            # a foreign holder conflicts with stays pending
                            # rather than occupying a worker or blocking the
                            # dispatch of a disjoint probe behind it. Taken
                            # before `submit`, so no worker is ever spent on a
                            # probe that cannot start.
                            try:
                                hold = probe_resource_lock.acquire(
                                    exclusive=lock_exclusive,
                                    shared=lock_shared, namespace=namespace,
                                    purpose=f"run_probes {probe[0]}")
                            except probe_resource_lock.ResourceBusy as busy:
                                foreign = busy
                                continue
                            ledger.acquire(need_exclusive, need_shared)
                            # Emitted BEFORE the work item is queued (and
                            # so before any engine boots), on the
                            # dispatching thread: the parallel path
                            # otherwise says nothing about a probe until
                            # it completes, which is precisely what makes
                            # a timeout here unattributable.
                            progress.begin(attempt_identity(
                                probe[0], probe[1], 1, total_attempts))
                            fut = ex.submit(work, i, probe)
                            running[fut] = (need_exclusive, need_shared, hold)
                            futs.append(fut)
                            pending.remove(item)
                        if not running:
                            if foreign is not None:
                                # Reachable only because of a holder OUTSIDE
                                # this runner — another sweep, or a /deflake
                                # measurement. The in-process ledger is idle
                                # here, so this is not the stall below: there
                                # is real work pending and it will become
                                # dispatchable when the foreign holder
                                # finishes. Wait for it rather than crashing
                                # the sweep. The wait cannot wedge: an flock
                                # dies with the process holding it, so only a
                                # live holder still doing its work keeps us
                                # here.
                                print(f"waiting for {foreign.resource!r} "
                                      f"({foreign.interest}) held outside this "
                                      f"runner: {foreign.describe()}")
                                time.sleep(RESOURCE_WAIT_POLL)
                                continue
                            # Unreachable: with nothing running the ledger is
                            # idle and no foreign holder was met, so the first
                            # pending probe always dispatches. Say so rather
                            # than spin forever.
                            raise RuntimeError(
                                "probe scheduler stalled with work pending: "
                                f"{[p[0] for _, p in pending]}")
                        done_futs, _ = concurrent.futures.wait(
                            running, return_when=concurrent.futures.FIRST_COMPLETED)
                        for fut in done_futs:
                            # Released on EVERY outcome — PASS, FAIL and
                            # TIMEOUT alike — and only once `run_one` has
                            # returned, which is after it reaped the probe's
                            # whole process group. A probe waiting on these
                            # resources therefore never starts while the
                            # previous holder's engine is still up. Both
                            # layers are released together, in-process ledger
                            # first, so no window pairs one with the other.
                            probe_exclusive, probe_shared, hold = running.pop(fut)
                            ledger.release(probe_exclusive, probe_shared)
                            hold.release()
                            done += 1
                            key, script, status, elapsed, out, timeout = fut.result()
                            progress.end(
                                attempt_identity(key, script, 1, total_attempts),
                                f"{status} ({elapsed:.1f}s)")
                            print(f"[{done}/{n}] {script} ... "
                                  f"[timeout {format_timeout(timeout)}] "
                                  f"{status} ({elapsed:.1f}s)")
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
                    # Only AFTER the engines are down: a cross-process holder
                    # waiting on these resources must not be let in while this
                    # runner's engines are still being torn down.
                    for _exclusive, _shared, hold in running.values():
                        hold.release()
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
                    timeout = effective_timeout(key, args.timeout)
                    for r in range(1, args.retries + 1):
                        # A solo retry is another full probe execution, so it
                        # takes the cross-process hold like any other (#1436).
                        # The parallel batch was attempt 1, so this is
                        # attempt r+1 — and it gets the same before/after
                        # pair, so a timeout DURING a retry is attributed
                        # exactly as a timeout during the batch is.
                        retry_identity = attempt_identity(
                            key, script, r + 1, total_attempts)
                        progress.begin(retry_identity, "solo retry")
                        with resource_hold(key, namespace, announce=waiting):
                            ok, timed_out, elapsed, out = run_one(
                                script, parallel_base, timeout, groups,
                                hold_env=descendant_hold_env(key, namespace))
                        status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
                        progress.end(retry_identity, f"{status} ({elapsed:.1f}s)")
                        print(f"  {script} solo retry {r}/{args.retries} ... "
                              f"[timeout {format_timeout(timeout)}] "
                              f"{status} ({elapsed:.1f}s)")
                        results[key] = (script, status, elapsed, out)
                        if ok:
                            break

            if args.tail > 0:
                for key, script, _ in chosen:
                    r = results[key]
                    if r[1] != "PASS":
                        print(f"\n--- {r[0]} ({r[1]}) ---")
                        # The same #1768 attribution the sequential path
                        # prints: `--jobs` is the OTHER default failure
                        # presentation, and the guarantee has to hold on
                        # both or it silently lapses whenever a probe is
                        # selected inside a parallel run.
                        for ln in progress_attribution(r[3]):
                            print(f"    {ln}")
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
