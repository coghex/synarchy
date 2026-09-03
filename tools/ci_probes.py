#!/usr/bin/env python3
"""Path-selective behavior-probe selection for CI (#530).

Given the files a change touched, decide which behavior probes CI should
run — only the ones relevant to the change, with a full-set catch-all for
core/unclassified changes and zero probes for docs/assets, the two test
trees (#1359), or subsystems whose behavior probes are intentionally
manual-only.
This is what makes a green CI mean "the features still work" without
paying a ~60s engine boot per probe on every PR.

The selection is deliberately FAIL-SAFE: anything this mapping can't
classify falls through to the full CI-eligible set (over-test rather than
silently skip). Feature areas may explicitly map to an empty set when their
available behavior probes are too narrow or too expensive for the blocking
gate.

Curated set + coverage/non-coverage is documented in CLAUDE.md. Only broad,
cheap, deterministic smoke probes are CI-eligible; flaky, scenario-heavy,
targeted, slow/worldgen-heavy, or base-failing probes stay manual-only
(run_probes.py). A retry-on-flake in run_probes.py absorbs residual
sequential-engine contention so the blocking gate doesn't redden good PRs.

Usage:
  # print the selected probe keys (one per line) + a reason on stderr
  python3 tools/ci_probes.py --changed src/Power/Network.hs data/items/x.yaml
  git diff --name-only origin/master...HEAD | python3 tools/ci_probes.py --stdin
  python3 tools/ci_probes.py --self-test    # validate the mapping, no engine
  python3 tools/ci_probes.py --status       # list every probe's CI eligibility (#540)
                                            # (manual-only probes list EVERY reason)
"""
from __future__ import annotations

import argparse
import fnmatch
import sys
import os
from dataclasses import dataclass
from typing import Sequence

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_runner_registry import PROBES  # noqa: E402

ALL_KEYS = {p[0] for p in PROBES}

# --------------------------------------------------------------------------
# Curated CI-eligible set — intentionally SMALL smoke coverage only. A probe
# can be deterministic and still be too narrow or too expensive for every
# matching PR; keep those manual-only and run them deliberately when touching
# their subsystem.
# --------------------------------------------------------------------------
CI_ELIGIBLE = {
    # #1220: the only automated proof that the two water AI actions mutate
    # the canteen instance they selected. Deterministic by construction —
    # the unit_ai tick is neutralised and the sim is PAUSED, so the two
    # execute functions are the only things that move a fill — one arena
    # boot, no worldgen, no GPU. Same shape as cargo_capacity: a narrow
    # item-instance regression whose call sites (scripts/unit_ai_*.lua)
    # already route to the full set through CORE_GLOBS.
    "canteen_instance",
    "cargo_capacity",
    "consumable_effects",
    # #890 (E2): the only gate covering all seven content registries —
    # deterministic (no AI, no timing), one boot, and the sole automated
    # coverage of the infection / location-def / loot-table writer+reader
    # pairs.
    "content_registry",
    # #1577: re-measured against #722's standard instead of inheriting the
    # bare `targeted` label direct commit b09c1518 left behind. Deterministic
    # by construction — `scripts.unit_ai`'s update is neutralised at
    # bootstrap and every assertion is a direct `craft.*` / `unit.*` /
    # `building.*` read, so nothing here grades AI arbitration or reaction
    # timing. One flat-arena boot, no worldgen, no GPU: 6/6 measured
    # attempts passed — 42.7/40.0/39.6 s solo and 39.2/39.3/39.2 s inside a
    # --jobs 2 batch (macOS/aarch64, 6e12c9c3), a 3.5 s total spread across
    # a machine whose load average moved between 14 and 46. Broad rather than
    # narrow: it is the only automated gate over kitchen.yaml built through
    # the real materials + build-progress machinery and the operations it
    # then advertises, the technomule depot as a REAL supply path,
    # all-or-nothing input consumption, #343 crafter-derived quality, and
    # the #344/#346 `output_temp` hook. `craft` stays the generic craft
    # smoke; this is the content integration beside it.
    "cooking",
    "craft",
    # #1190: the only automated proof that --headless/--offscreen refuse
    # to boot without their one control surface. Deterministic (no AI, no
    # world, no GPU — every failing boot dies before the engine action),
    # broad (both required modes, both failure kinds, plus the --dump #46
    # sentinel and the successful-bind regressions).
    #
    # Since #1365 it ALSO owns the widget-kit gate below: its check 8 boots
    # normally three more times to prove the 28 `scripts.ui.*` modules a
    # non-preview headless boot loads really loaded, and that the gate fails
    # when one of them doesn't. Those boots load no world and no GPU either,
    # and cost ~2 s together — the probe stays in the seconds range that
    # makes it CI-eligible.
    "debug_console_boot",
    # #1577: re-promoted. #600 promoted this probe on the strength of #593
    # making it self-contained, and direct commit b09c1518 removed it an
    # hour later with no recorded evidence. The #593 property still holds:
    # it sets SYNARCHY_INFECTION_TEST_MODE=1 on its OWN engine subprocess,
    # so growth and sepsis are observable over fixed seconds instead of the
    # production rate's real minutes, `unit_ai.update` is neutralised, and
    # every assertion is a direct wound/stat read over a monotone engine
    # tick with a wide margin — no AI arbitration, no skip path. One
    # flat-arena boot, no worldgen, no GPU: 6/6 measured attempts passed —
    # 57.8/58.0/58.0 s solo and 57.5/57.5/56.8 s inside a --jobs 2 batch
    # (macOS/aarch64, 6e12c9c3), a 1.2 s total spread across a machine whose
    # load average moved between 14 and 46. Broad: all four halves of the loop
    # (growth, antiseptic prevention, antibiotic cure, sepsis meter) have no
    # other automated end-to-end coverage.
    "infection",
    "medic_coord",
    "persistence_contract",
    "preview_cli",
    "repair",
    "repair_item",
}

# Manual-only reason categories (#540). Kept short + greppable; `--status`
# and `_self_test` below both read this dict, so it is the single source of
# truth for "why isn't X in CI_ELIGIBLE" — no more digging through comments.
FLAKY = "flaky"
BASE_FAILING = "base-failing"
SLOW_WORLDGEN = "slow/worldgen-heavy"
SCENARIO_HEAVY = "scenario-heavy"
TARGETED = "targeted"
NEEDS_GPU = "needs-gpu"
UNCLASSIFIED = "unclassified"


@dataclass(frozen=True)
class Reason:
    """One independent ground for keeping a probe out of the blocking gate.

    A probe is often excluded on more than one count that would each hold
    on its own -- `expedition_retrieval` needs a real generated world AND
    walks its legs in real time -- and #1440 exists because a single
    `(category, explanation)` tuple could only ever record one of them.
    """

    category: str
    explanation: str


# Every registered probe (ALL_KEYS) NOT in CI_ELIGIBLE must have an entry
# here: a non-empty tuple of `Reason` records, one per INDEPENDENT ground,
# each with a known category and a non-blank explanation, and no category
# repeated within one probe (fold two facets of the same category into one
# explanation instead). `_self_test` enforces full coverage so a
# newly-registered probe can't silently land in neither bucket, and
# `validate_manual_only_reasons` enforces the record shape.
#
# Reasons render in DECLARED order: that is the stable per-probe order
# `--status` and any later census reporting read (#1440). The `--- category
# ---` banners below group entries by their FIRST declared reason; a probe
# excluded on more than one count is filed under that one and carries the
# rest inline.
#
# OWNERSHIP (#1426): a command that merely NOTICES possible flakiness
# appends its evidence to the de-flake census, not here. Only the de-flake
# workflow promotes verified evidence into this registry, so an unrelated
# branch must not add a `flaky` reason opportunistically -- and never from
# an old mention or one unrelated failing run.
MANUAL_ONLY_REASONS: dict[str, tuple[Reason, ...]] = {
    # --- flaky: AI-reaction/arbitration timing the slower, variable-speed
    # Linux CI runner destabilizes run-to-run;
    # within-run retry can't fix run-to-run flakiness. ---
    "craft_bill": (Reason(FLAKY, "craft_job AI claim/work timing flakes run-to-run on CI"),),
    "role": (Reason(FLAKY, "role-hysteresis timing flakes run-to-run on CI"),),
    "chop": (Reason(FLAKY, "chop AI claim/work timing flakes run-to-run on CI"),),
    "foraging": (Reason(FLAKY, "foraging AI timing flakes run-to-run on CI"),),
    "sleep": (Reason(FLAKY, "go_to_sleep AI goal-arbitration + multi-hop pose-chain "
                            "timing flakes run-to-run on CI (#722; the disarm lesson — "
                            "local greenness isn't sufficient evidence for an "
                            "AI-reaction/arbitration-timing probe)"),),
    "combat_anim": (Reason(FLAKY, "#1396 retired the recorded failure mode: the fixture is "
                                  "the flat arena, every tile the combatants can traverse "
                                  "is verified flat/dry/loaded, engagement is positively "
                                  "observed before sampling, and an unestablished fixture "
                                  "now exits 2 instead of masquerading as the missing "
                                  "swing it used to (the attacker fell to its death "
                                  "approaching the target in 1/3 solo runs, #724). What is "
                                  "still GRADED rides on AI arbitration and swing-cooldown "
                                  "timing across a sampling window, so the disarm lesson "
                                  "still applies — green solo runs locally are not "
                                  "evidence for the slower, variable-speed CI runner"),),
    "follow_command_priority": (Reason(FLAKY, "a struck goal-bound unit occasionally treats "
                                              "an ally instead of engaging combat, failing "
                                              "'combat reached over a pending move' "
                                              "(1/3 solo runs, #724). #1736 retired the "
                                              "OTHER recorded failure mode: the pickup check "
                                              "discarded commandPickup's Boolean, so a weak "
                                              "carrying_capacity roll refusing a 10 kg chunk "
                                              "at the command-time gate (#920) read as a "
                                              "ladder regression; both pickup checks now "
                                              "stage the capacity answer and stop at exit 2 "
                                              "on an unestablished fixture. What is still "
                                              "GRADED rides on combat/treatment arbitration "
                                              "timing, which is the #724 flake above"),),
    "repair_ai": (Reason(FLAKY, "repair-AI claim/fetch/work timing flakes with a DIFFERENT "
                                "failing-check set each run (3/3 solo runs failed, no two "
                                "alike, #724) — not #489 (whetstone), which is long fixed "
                                "and never implicated"),),
    "physiology": (Reason(FLAKY, "'temperate (22C/0.5): circ min' sits right at its 0.75 "
                                 "pass threshold and failed both failing runs (0.74, 0.75); "
                                 "the combat-vs-idle calorie-drain-ratio check also flaked "
                                 "once, alongside it (2/3 solo runs failed, #724)"),),
    # --- scenario-heavy: deterministic enough to run manually, but either
    # long-running or broad end-to-end scenarios that make the blocking PR
    # gate too expensive. ---
    "construction": (Reason(SCENARIO_HEAVY, "long construct_job AI end-to-end scenario: "
                                            "inventory + ground sourcing, dead-claimant "
                                            "release, blueprint staking (4 phases, "
                                            "~123-168s solo); 14/14 checks passed all 3 "
                                            "solo runs (#724)"),),
    "location_content": (Reason(SCENARIO_HEAVY, "eight real-engine-process scenario: ruin "
                                                "content spawn + geometry, seed-stable "
                                                "per-instance loot in same and reversed visit "
                                                "order, save/quit/restart/load round-trip, "
                                                "bogus/valid content-registry validation with "
                                                "fixed-position item dispatch, hidden-page "
                                                "multiworld building/unit spawn, sight-based "
                                                "discovery and per-unit location knowledge, "
                                                "and generated-language location naming with "
                                                "its own fresh-process reload (#800 replaced "
                                                "the stale loot_names allowlist with the live "
                                                "item registry, resolving the prior "
                                                "quinoa_sack flake; #915 added the per-unit "
                                                "location-knowledge layer alongside the "
                                                "player-wide discovery checks; #1101 the "
                                                "naming phases; #2095 split the scenario "
                                                "owners out behind the probe's facade)"),),
    "item_instance": (Reason(SCENARIO_HEAVY, "real worldgen plus save/load identity regression"),),
    "river_naming": (Reason(SCENARIO_HEAVY, "#1102: three real engine boots -- generation, a "
                                            "fresh-process save/load round trip, and a "
                                            "fresh-process regeneration -- around two real "
                                            "16-chunk worldgens"),),
    "item_temp": (Reason(SCENARIO_HEAVY, "real worldgen, cooling waits, and save/load round-trip"),),
    "position_hold": (Reason(SCENARIO_HEAVY, "#1216: three acolytes sharing one arena through a "
                                             "32 s unpaused containment window, a real "
                                             "refill excursion out to a lake and back, and a "
                                             "post-release work resume -- every stage is a "
                                             "real-time AI-arbitration leg, ~4 minutes solo"),),
    "power_workshop": (Reason(SCENARIO_HEAVY,
        "#1577 measured it against #722's standard rather than inheriting "
        "b09c1518's bare label, and it did not qualify. Its check 5 is a "
        "REAL craft_job end-to-end leg: the AI has to claim the bill, fetch "
        "a steel_bar, walk to the built station and mark itself working "
        "inside 20 s + 20 s polling windows, then complete inside a 60 s "
        "one. That is AI arbitration and reaction timing, which is what the "
        "probe GRADES rather than merely what it waits on, and 3 of 6 "
        "measured attempts (1/3 solo, 2/3 in a --jobs 2 batch; "
        "macOS/aarch64, 6e12c9c3) failed on exactly the same two checks "
        "there -- 'AI reaches the working phase' and the drainW read that "
        "depends on it -- while every non-AI check passed in all 6. It is "
        "also the most expensive of the three probes #1577 reviewed by "
        "~2.5x (88.9-107.9 s solo vs cooking's 39.6-42.7 s), and three of "
        "its stages are real-time by construction: a 5 s browned-out stall "
        "observation and two 5 s day/night fast-forwards that no runner "
        "speed shortens. Per #722's disarm lesson, a probe whose graded "
        "assertions ride AI timing does not become gate-worthy by passing "
        "locally"),),
    "power": (Reason(SCENARIO_HEAVY, "long build-tool power-node placement + wire network + "
                                     "day/night balance + save/restart/load round-trip scenario"),),
    "save_compat_migration": (Reason(SCENARIO_HEAVY, "two real engine boots plus a real "
                                                      "process restart exercising a tracked "
                                                      "legacy-fixture load/publish/resave/"
                                                      "reload round trip (#766)"),),
    "autosave": (Reason(SCENARIO_HEAVY, "#913: the validated 1-minute minimum for "
                        "interval_minutes forces multiple >60s wall-clock waits (the "
                        "default-off dwell and the one real-interval fire alone are "
                        "~3 minutes), and the player-intent-suppression check depends "
                        "on racing the post-capture storage-write window -- neither is "
                        "cheap or stable enough for a blocking per-PR gate"),),
    "save_pause": (Reason(SCENARIO_HEAVY, "real worldgen plus save/load pause race checks"),),
    "pause_speed": (Reason(SCENARIO_HEAVY, "#1599: real worldgen plus four pause sources driven end to end -- a notification, a whole manual engine.saveWorld waited to its terminal outcome, a second save taken from an already-paused session, and a published load -- each with its own poll-until-settled window"),),
    "save_barrier": (Reason(SCENARIO_HEAVY, "two real engine boots plus worldgen/save/load boundary smoke"),),
    "save_storage": (Reason(SCENARIO_HEAVY, "worldgen plus ~10 real engine boots exercising the "
                                            "atomic storage transaction's restart-and-select fallback"),),
    "transactional_load": (Reason(SCENARIO_HEAVY, "three real engine boots, three real world pages, "
                                                   "a deterministically-held in-flight mutual-exclusion "
                                                   "window (#1181), and a repeated-load loop "
                                                   "(#763)"),),
    # --- targeted: useful regression probes, but too narrow for the default
    # PR gate. Run them when touching the named feature. ---
    "collapse_crawl": (Reason(TARGETED, "narrow #304 collapse/crawl hysteresis regression"),),
    "concussion_revive": (Reason(TARGETED, "narrow #304 concussion revive hysteresis regression"),),
    "config_migration": (Reason(TARGETED, "narrow #786 pre-#661 legacy config upgrade regression"),),
    "config_state": (Reason(TARGETED, "narrow #638 config load/save vs git-tracking regression"),),
    "disarm": (Reason(TARGETED, "narrow #193 disabled-hand auto-drop regression"),),
    "injury_log": (Reason(TARGETED, "injury-log backend plumbing, narrower than the combat subsystem"),),
    "lua_strict_msg": (Reason(TARGETED, "narrow #622 LuaToEngineMsg/LuaMsg strictness crash regression"),),
    "lunge": (Reason(TARGETED, "narrow #1713 combat-lunge reachability regression: one "
                               "leap -> land -> strike state machine and the four ways "
                               "it can end. combat_anim remains the general "
                               "unit-animation/combat gate and mental_state the general "
                               "#352/#717 lash-out one; neither species reaches the "
                               "lunge path at all"),
              Reason(SCENARIO_HEAVY, "four staged scenarios, each re-staging a "
                                     "probability-rolled launch until it takes and then "
                                     "waiting out a real leap flight and a multi-second "
                                     "settle window. What it GRADES rides on AI "
                                     "arbitration and leap timing, which is the class "
                                     "#722's disarm lesson keeps off the blocking gate")),
    "machine_shop": (Reason(TARGETED, "electric furnace + machine_shop content regression, narrower "
                                      "than the generic #590 power-draw mechanism probe"),),
    "meal_waste": (Reason(TARGETED, "narrow #1219 stop-before-waste meal-policy regression; "
                                    "foraging remains the generic #94 food-ladder gate"),),
    "mental_efficiency": (Reason(TARGETED, "narrow #353 mental-effectiveness combat/craft tie-in "
                                           "regression: getMentalEffectiveness, craft-bill progress "
                                           "rate, craft-quality delta, damage-energy invariance"),),
    "mental_state": (Reason(TARGETED, "narrow #352 mental-state ladder/hysteresis/break-AI regression"),),
    "retaliation_swap": (Reason(TARGETED, "narrow #1483 mid-fight retaliation-swap "
                                          "regression: one attack_target branch and the "
                                          "tick-abort it caused. mental_state remains the "
                                          "general #352/#717 gate over the same branch's "
                                          "collapsed/technomule exclusions, which "
                                          "short-circuit before the comparison this one "
                                          "reaches"),),
    "movement": (Reason(TARGETED, "registered CI invocation takes no arguments, so it runs only "
                                  "movement_probe.py's default corner_trap diagonal-routing "
                                  "regression, not the cliff/fall/ramp/stamina courses the "
                                  "obstacle-course description implies (#722 defined promotion "
                                  "broadness from the actual registered invocation; #754 made "
                                  "the unsupported promotion; #772 demoted it)"),),
    "wander_hazard": (Reason(TARGETED, "narrow #1217 ambient-wander fall-free routing "
                                       "regression; movement_probe.py remains the general "
                                       "obstacle-course gate"),
                      Reason(FLAKY, "stage A is a sustained window of REAL unit_ai wander, so it "
                                    "depends on AI thought-tick timing the slower, variable-speed "
                                    "Linux CI runner destabilizes run-to-run")),
    "remote_warning_page_guard": (Reason(TARGETED, "narrow #844 remote-warning establishHere() "
                                                   "cross-page revalidation regression"),),
    "resource_root": (Reason(TARGETED, "narrow #636 resource-root launch-contract regression "
                                       "(also runs its own small worldgen dump)"),),
    "text_encoding": (Reason(TARGETED, "narrow #618 Lua text API decodeUtf8Lenient regression, "
                                       "extended by #665 with a representative non-Text API "
                                       "(world.show) boundary from the same decodeUtf8Lenient sweep"),),
    "blood_decal": (Reason(TARGETED, "narrow #604/#606 blood decal texture reuse/eviction/render regression"),),
    "blood_impact": (Reason(TARGETED, "narrow #607 wound-to-impact-blood mapping regression"),),
    "bleeding_trail": (Reason(TARGETED, "narrow #882/#883 ongoing-bleeding emission regression "
                                        "(moving trail + stationary/collapsed pooling)"),),
    "circadian": (Reason(TARGETED, "narrow #611 circadian urge + sleep_pressure drain regression"),),
    "circadian_species": (Reason(TARGETED, "narrow #613 species-specific circadian phase regression"),),
    "thought": (Reason(TARGETED, "thought event/log backend plumbing, narrower than the full "
                                 "psychology arc (mirrors injury_log_probe.py)"),),
    "wire": (Reason(TARGETED, "narrow #359 wire connection/path-builder/build-AI regression"),),
    "lua_orphan_prune": (Reason(TARGETED, "narrow #195 Lua per-id AI-state-pruning save/load "
                                          "regression; ~40s solo, passed all 3 solo runs (#724)"),),
    "state_of_mind": (Reason(TARGETED, "narrow #350 state-of-mind/awareness-term regression; "
                                       "periodic thoughts (#351) are neutralised for the probe's "
                                       "own engine so a random 0-30s thought can't land inside its "
                                       "mood-drift sampling windows (#793)"),),
    # (no probes currently classified base-failing — the category stays
    # defined below for any future genuinely-broken-on-master case.)
    # --- needs-gpu: requires a real Vulkan device, which the CI runner
    # does not have. First candidate for a future GPU-equipped CI lane. ---
    "offscreen": (Reason(NEEDS_GPU, "boots the full Vulkan render pipeline (windowless) — no GPU on the CI runner"),),
    "blood_gpu_lifecycle": (Reason(NEEDS_GPU, "offscreen boot: uploadBloodTextures needs a real Vulkan "
                                              "device to upload/dispose blood textures (#788) — no GPU on the CI runner"),),
    "preview": (Reason(NEEDS_GPU, "real preview boot creates a GLFW window and calls "
                                  "initializeVulkan (app/App/Preview.hs), same as the "
                                  "graphical boot path — no GPU on the CI runner (#722)"),),
    "construction_blueprint_footprint": (Reason(NEEDS_GPU, "offscreen boot: proves the "
                                  "committed building blueprint's ghost is the building's "
                                  "own sprite, matching the staked one, via real "
                                  "screenshot diffing (#807/#1845) — no GPU on the CI runner"),),
    "portal_ghost": (Reason(NEEDS_GPU, "offscreen boot: verifies the build-tool ghost's "
                                       "rendered tint via real screenshots — no GPU on the CI runner (#778)"),),
    "structure_rotation": (Reason(NEEDS_GPU, "offscreen boot: captures one stamped "
                                  "room at all four camera facings so the rotated "
                                  "wall art, the terrain interleave and the billboard "
                                  "lift can be eyeballed (#1712) — no GPU on the CI "
                                  "runner. The rotation arithmetic itself is proven "
                                  "exhaustively by the pure hspec groups "
                                  "World.Render.StructureRotation and "
                                  "World.Render.FrontWallLift, which DO gate on CI"),),
    "scene_primitives": (Reason(NEEDS_GPU, "offscreen boot: attributes engine.spawnText "
                                            "and a UI-layer engine.spawnSprite to bounded "
                                            "screenshot regions across every mutation verb "
                                            "(#2192) — no GPU on the CI runner"),),
    "scene_stats": (Reason(NEEDS_GPU, "offscreen boot: the #1921 telemetry's "
                           "non-zero unit/ground-item/building EMITTED counts "
                           "exist only with a live texture system, which the "
                           "GPU-free headless path never has -- no GPU on the "
                           "CI runner. Publication, ordering, sequence and "
                           "every scanned meaning ARE gated on CI, by the pure "
                           "hspec group \"scene assembly telemetry\""),),
    "startup_asset_logging": (Reason(NEEDS_GPU, "offscreen boot: a --headless "
                             "engine never runs scripts/loading_screen.lua or "
                             "scripts/ui_manager_boot.lua, so the startup "
                             "asset queue this probe reads does not execute "
                             "there at all and every check would pass "
                             "vacuously -- no GPU on the CI runner (#1930). "
                             "Both halves of the ownership contract ARE gated "
                             "on CI, by the pure hspec group \"Startup asset "
                             "logging\""),),
    "tutorial_hud": (Reason(NEEDS_GPU, "offscreen boot: verifies the tutorial checklist "
                                       "HUD's rendered overlay, real toggle clicks, wheel "
                                       "scrolling and click-through via screenshots — no "
                                       "GPU on the CI runner (#960)"),),
    "etymology": (Reason(NEEDS_GPU, "offscreen boot: drives the in-game etymology panel "
                         "through the real UI (name-plate rows located via the widget "
                         "dump, clicked with input.click) against a real generated "
                         "world -- needs a Vulkan device the CI runner does not have"),),
    "location_embark": (Reason(NEEDS_GPU, "offscreen boot: real embark-to-discovery session "
                                          "through zoom-map icons, portal ghost/remote-modal "
                                          "flow, and real-input-driven unit movement — no GPU "
                                          "on the CI runner (#782)"),),
    "item_list_widget": (Reason(NEEDS_GPU, "offscreen boot: the shared item-list "
                                "widget's real rendered rows/tabs across its migrated "
                                "hosts, the container window's nesting stack (#1238), "
                                "the real rendered Store/Retrieve row menus whose "
                                "gestures queue durable transfer orders (#1249), and "
                                "the Mode A escort session's two flanking panels and "
                                "their immediate commits (#1250), "
                                "located via ui.dumpWidgets() against a real "
                                "Vulkan-rendered HUD — no GPU on the CI runner "
                                "(#1088)"),),
    # #1440: two independent counts. Even on a GPU-equipped runner the
    # real generated world and fresh-process save/load would still make
    # the unified transfer gate too expensive for every matching PR.
    "unified_transfer": (Reason(NEEDS_GPU, "offscreen boot: the unified transfer "
                                "system's end-to-end gate (#1255) - two real "
                                "Vulkan-rendered engine boots driving Mode A's "
                                "flanking panes and Mode B's row menus through "
                                "ui.dumpWidgets() - no GPU on the CI runner"),
                         Reason(SLOW_WORLDGEN, "the same gate also generates a real "
                                "world and drives a fresh-process save/load across "
                                "its two boots (#1255)"),),
    "transfer_context_menu": (Reason(NEEDS_GPU, "offscreen boot: real right-click -> "
                                     "\"Transfer\" context-menu row located via "
                                     "ui.dumpWidgets() against a real Vulkan-rendered "
                                     "menu — no GPU on the CI runner (#1014/#1085)"),),
    # --- slow/worldgen-heavy: needs a real generated world, not the flat
    # arena — too slow for a blocking per-PR gate. ---
    "action_outcome": (Reason(SLOW_WORLDGEN, "needs a real generated world to scan for a "
                             "mixed tillable/fluid box and a real tree for the chop "
                             "partial path (#646)"),),
    "flora_growth": (Reason(SLOW_WORLDGEN, "needs a real generated world for natural ground cover"),),
    "multiworld_save": (Reason(SLOW_WORLDGEN, "generates two real world pages"),),
    "persistence_integrity": (Reason(SLOW_WORLDGEN, "generates a real world page and boots "
                                     "three engines (build+save, dangling-reference load, "
                                     "corrupted-load-vs-live-session) (#764)"),),
    "persistence_contract_sweep": (Reason(SLOW_WORLDGEN, "real worldSize-64 generation plus "
                                     "four real engine boots (three fresh-process save->load-"
                                     "save cycles) for the broader representative-scenario "
                                     "comparison, plus every cross-referenced probe it runs "
                                     "(#767); the compact persistence_contract probe is "
                                     "CI-eligible instead"),),
    # #1440: the "two independent counts" #1247 already recorded in prose,
    # now one reason record each. Neither is a `flaky` claim: no recorded
    # run-to-run failure evidence exists for this probe, and real-time /
    # AI-arbitration dependence alone is scenario weight, not flakiness.
    "transfer_order": (Reason(SLOW_WORLDGEN, "its save/load phase generates a real "
                              "world page and boots two more engines, because a "
                              "save containing an arena page hangs the world thread "
                              "on load (#365/#1247)"),
                       Reason(SCENARIO_HEAVY, "the stall/long-haul phases are "
                              "REAL-TIME by construction — a stall budget can only "
                              "be shown to expire by letting ~60 s of it elapse, and "
                              "a progressing trip can only be shown to outlive that "
                              "budget by walking for longer than it; both also lean "
                              "on AI arbitration timing (the transfer_order lock "
                              "holding against the wander tick), the same grounds "
                              "that keep every other AI-driven probe out of the "
                              "blocking gate. Total ~8 min (#1247)"),),
    "location_overlay": (Reason(SLOW_WORLDGEN, "needs real worldgen for overlay placement"),),
    "location_stamp_idempotent": (Reason(SLOW_WORLDGEN, "needs real worldgen plus a save/restart/reload round-trip"),),
    "portal_location": (Reason(SLOW_WORLDGEN, "needs real worldgen for a placed ruin_small "
                              "to test starting-building exclusion against (#778)"),),
    "thermo_altitude": (Reason(SLOW_WORLDGEN, "needs a real generated world (worldSize 128) "
                              "for elevation data, ~1 min runtime"),),
    "crop": (Reason(SLOW_WORLDGEN, "needs a real generated world for natural row-crop placement + "
                                   "groundcover planting, plus a save/load round-trip"),),
    # #1440: the "manual-only on both counts" #923 already recorded in
    # prose, now one reason record each.
    "expedition_loop": (Reason(SLOW_WORLDGEN, "the arc's whole first expedition in "
                               "one session: a real worldSize-64 generation, a scan "
                               "for a ruin with a walkable colony site and nearby "
                               "water, the portal's own six-unit spawn roster, and a "
                               "real save/restart/load across two engine boots "
                               "(~15 min) (#923)"),
                        Reason(SCENARIO_HEAVY, "two travellers walk ~30 tiles each "
                               "way in real time, and that leg leans on AI "
                               "arbitration timing (drink_from_canteen and "
                               "eat_from_inventory outranking a move order, "
                               "pickup_ground, follow_command) — the same grounds as "
                               "expedition_retrieval below; #923's own scenario "
                               "contract also classifies this as recorded manual "
                               "verification rather than a CI gate"),),
    # #1440: the "manual-only on both counts" #920 already recorded in
    # prose, now one reason record each.
    "expedition_retrieval": (Reason(SLOW_WORLDGEN, "needs a real generated world for "
                                    "a placed ruin_small and a walkable colony site "
                                    "tens of tiles from it, across two engine boots "
                                    "with a real save/restart/load in the middle "
                                    "(#920)"),
                             Reason(SCENARIO_HEAVY, "it walks both legs in real time "
                                    "and leans on AI arbitration timing "
                                    "(pickup_ground, drink_from_canteen, "
                                    "notify_allies) (#920)"),),
    "plant": (Reason(SLOW_WORLDGEN, "needs a real generated world for natural ground cover + "
                                    "real climate/slope suitability data"),),
    "till": (Reason(SLOW_WORLDGEN, "needs a real generated world for natural ground cover to "
                                   "exercise the tillable-tile filter; slow AI loop (~7 min observed)"),),
    "farm_ai": (Reason(SLOW_WORLDGEN, "needs a real generated world for the till->plant->harvest AI "
                                      "loop across 5 distinct tillable sites; slowest registered "
                                      "probe (~11 min observed, O(n^2) TCP tile scan over natural terrain)"),),
    # #1440: #922's description already recorded two independent counts.
    "tutorial": (Reason(SLOW_WORLDGEN, "needs a real generated world for a natural "
                        "lake or river to discover and a water-free camp far from "
                        "it, plus a real save/restart/load across two engine boots "
                        "(#922)"),
                 Reason(SCENARIO_HEAVY, "the radio-share leg runs on AI arbitration "
                        "timing, and #922 requirement 6 classifies this integration "
                        "check as recorded manual verification rather than a CI "
                        "gate"),),
}

# Sentinels (distinct objects so `is` comparisons are unambiguous).
ALL = object()   # this file can affect anything -> full CI-eligible set
NONE = object()  # this file affects no probe -> contributes nothing
# Same zero contribution as NONE, kept distinct only so `select()` can say
# WHY nothing was selected: CI echoes that string, and "docs/assets only"
# would misdescribe a test-tree change (#1359).
NONE_TEST = object()


def contributes_nothing(contribution: object) -> bool:
    """True for every sentinel that selects no probes.

    Kept as one predicate so a third such sentinel cannot be added and
    then missed by one of `select()`'s two branches. Compares by IDENTITY:
    a feature rule's contribution is a real `set`, which must never be
    mistaken for a sentinel.
    """
    return contribution is NONE or contribution is NONE_TEST


# Docs / assets: zero probes. Checked FIRST.
SKIP_GLOBS = [
    "*.md", "docs/*", "assets/*", "*.txt", "LICENSE*", ".gitignore",
    "*.png", "*.jpg",
    # The playtest harness (#647) is human-run against a windowed
    # instance and cannot alter engine behavior — no probe covers it
    # and none should run for it.
    "tools/playtest/*",
]

# Test sources: zero probes (#1359). Checked after SKIP_GLOBS, before
# everything else, so a change confined to either test tree contributes
# nothing instead of falling through to the fail-safe full set.
#
# The behavior probes boot and drive the already-built `exe:synarchy`, and
# neither test tree is an input to it: the executable is `hs-source-dirs:
# app` (synarchy.cabal), `synarchy-test-graphical` is `hs-source-dirs:
# test`, and `synarchy-test-headless` is `hs-source-dirs: test-headless,
# app`. The dependency runs one way — the suites consume the app sources,
# never the reverse — so no edit under `test/` or `test-headless/` can
# change what a probe observes.
#
# One real coupling survives. The CI-eligible `persistence_contract` probe calls
# `persistence_snapshot.compare_session_files`, which launches
# `cabal repl test:synarchy-test-headless`, so it does need `test-headless/`
# to COMPILE. The separate `behavior-probes` job therefore builds both
# `exe:synarchy` and `synarchy-test-headless` before launching any selected
# probe. A test-only change still selects no probes and is compiled + tested by
# `test-and-audits`; a mixed core/test change selects probes and the
# prerequisite build fails cleanly before their timeout begins. `test/` has no such
# coupling: it belongs to
# `synarchy-test-graphical`, which this selector never builds and which is
# compiled only when `tools/ci_expensive_gates.py` selects the graphical
# build through its own independent `GRAPHICAL_GLOBS` entry `test/*` —
# a separate selector this mapping does not touch.
TEST_GLOBS = [
    "test/*", "test-headless/*",
]

# Core / shared code: a change here can affect ANY probe -> full set.
# The AI script stack + unit/world threads + core data are loaded by
# essentially every probe.
CORE_GLOBS = [
    "src/Engine/Core/*", "src/Engine/Monad*",
    "src/Unit/*", "src/World/Thread/*", "src/World/Save/*",
    "src/World/Load/*", "src/Engine/Load/*", "src/Engine/Save/*",
    "scripts/unit_ai.lua", "scripts/unit_ai_*.lua",
    "scripts/unit_resources.lua", "scripts/unit_stats.lua",
    "scripts/movement_arena.lua",
    "data/units/*", "data/materials/*", "data/substances/*",
    "tools/probelib.py", "tools/probe_engine.py", "tools/run_probes.py",
    "tools/ci_probes.py",
    ".github/workflows/ci.yml",
]

# Feature-area rules: path glob(s) -> the CI-eligible probes covering it.
# First matching rule wins per file. Keys here must be in CI_ELIGIBLE.
# Empty sets are intentional for subsystems whose behavior probes are now
# manual-only because they are scenario-heavy or too narrowly targeted.
FEATURE_RULES: list[tuple[list[str], set[str]]] = [
    (["src/Combat/*", "scripts/acolyte_combat.lua", "scripts/combat_log.lua",
      "scripts/injury_log*.lua"],
     # medic_coord gates the bestMedicFor/medicAvailable distance-discounted
     # selection. #1577 added infection here because the mechanism that probe
     # grades lives under THIS glob, not under src/Infection/*: the wound
     # infection/necrosis tuning is src/Combat/Wounds/Infection.hs, the tick
     # that accrues it is src/Combat/Wounds/Tick.hs, and src/Infection/ holds
     # only the catalogue types.
     {"medic_coord", "infection"}),
    (["src/Infection/*", "data/infections/*"],
     # content_registry is the #890 smoke over this catalogue's load+query
     # path. #1577 restored the scenario probe direct commit b09c1518
     # removed: it loads data/infections/*.yaml and then drives a real wound
     # through growth -> antiseptic prevention -> antibiotic cure -> sepsis,
     # which is the only automated proof the catalogue a change here edits is
     # actually consumed by the wound tick.
     {"content_registry", "infection"}),
    (["src/Craft/*", "data/recipes/*", "scripts/crafting_panel.lua",
      "scripts/craft*.lua", "scripts/cooking*.lua"],
     # data/recipes/* also covers repair.yaml (repair-tagged recipes) and
     # brew_coffee (consumable_effects' brew step) — both promoted probes
     # (#722) load the full data/recipes/*.yaml glob at bootstrap.
     #
     # cooking is the selection PR #535's own review required here (7f7cd03b)
     # and direct commit b09c1518 removed; #1577 restored it on re-measured
     # evidence. It is the only probe that asserts basic_food.yaml's
     # brew_coffee DECLARATION — station/inputs/outputs/skill/knowledge/
     # output_temp — against what the engine actually crafts from it, so an
     # edit to this glob that the generic craft smoke would still pass no
     # longer lands unwatched.
     {"craft", "consumable_effects", "repair", "content_registry", "cooking"}),
    (["src/Power/*", "scripts/wire.lua", "scripts/power*.lua",
      "data/structure_packs/*"],
     # The one rule that deliberately selects NOTHING, and #1577 re-measured
     # that rather than restating the value direct commit b09c1518 left here.
     # All three power probes stay manual-only for grounds recorded in
     # MANUAL_ONLY_REASONS above: `power` and `power_workshop` are
     # scenario-heavy, `machine_shop` is targeted. `power_workshop` is the
     # closest candidate and the one #1577 actually measured: its craft_job
     # leg GRADES AI arbitration and reaction timing inside real-time
     # polling windows, and 3 of 6 measured attempts failed on exactly those
     # two checks while every non-AI check passed in all 6.
     #
     # The accepted gap is therefore specific, not general: no BLOCKING
     # check exercises Power.Network.activeCraftConsumersOn — a live claimed
     # AND working craft bill's draw folded into a real network's balance
     # inside a running engine. Test.Headless.Power.Network's own module
     # header names that function as the one thing it cannot cover and
     # points at tools/power_workshop_probe.py for it.
     #
     # What DOES gate a change here is the always-blocking headless hspec
     # suite. Those are unit/integration specs, not behavior probes — they
     # assert on functions and on the engine's own command queues, never on
     # a scripted play session:
     #   * Test.Headless.Power.Network — wire connectivity, solar intensity,
     #     charge/discharge/brownout, #361 consumer drain, and #590's
     #     combineConsumers always-on + active-job union, over synthetic
     #     nodes and consumers.
     #   * Test.Headless.Power.Types — the pure node-registry transitions.
     #   * Test.Headless.Power.Placement (#1205) and .Demolition (#1206) —
     #     a real headless engine driven through the production
     #     power.placeNode and BuildingDestroy paths.
     #   * Test.Headless.Craft.Bills' "working (#590)" group — the cbWorking
     #     lifecycle every active-job draw keys off.
     set()),
    (["src/Item/*", "data/items/*", "src/Equipment/*", "data/equipment/*"],
     # consumable_effects exercises coffee_pot/coffee_grounds/water; repair
     # exercises axe_steel/whetstone/lignite_chunk (#722).
     {"cargo_capacity", "repair_item", "consumable_effects", "repair",
      "content_registry"}),
    (["src/Building/*", "data/buildings/*"],
     # consumable_effects builds a kitchen; repair builds a furnace +
     # workbench (#722). cooking (#1577) builds kitchen.yaml through the real
     # materials-delivery + build-progress machinery and then asserts the
     # operation set the built instance advertises and that
     # building.findStation resolves to it — the def's own
     # declaration-to-behaviour join, which neither of the other two checks.
     {"craft", "consumable_effects", "repair", "cooking"}),
    # The Lua widget kit (#1365). Before this rule a widget-module change
    # matched nothing and fell through to the fail-safe full set, paying
    # every CI-eligible probe for a change none of them exercise.
    #
    # `debug_console_boot` is selected because its check 8 VERIFIES normal-
    # boot widget-module loading: it boots headless, reads back which
    # `scripts.ui.*` modules `package.loaded` actually holds, and fails both
    # when a covered module is missing and when the boot logged a Lua
    # load/init failure — each half proven load-bearing by its own negative
    # regression against a real broken boot. It keeps its separate #1190
    # listener-policy and #1283 console-lifecycle checks alongside that.
    #
    # So this is NOT a "nothing covers it" empty set, and must not be
    # reduced to one: emptying it would silently drop the only CI coverage
    # a widget-kit change has. Narrowing it further needs equivalent
    # failure-sensitive coverage supplied first.
    #
    # The trailing slash is load-bearing. `fnmatch` is unanchored, so the
    # glob `scripts/ui*` would also swallow every `scripts/ui_manager*.lua`
    # module — top-level gameplay/UI wiring that is deliberately out of
    # scope (#1365) and still selects the full set. `_self_test`'s
    # `scripts/ui_manager.lua` case is the guard that proves it.
    (["scripts/ui/*"],
     {"debug_console_boot"}),
]


def classify_file(path: str):
    """A single file's probe contribution: NONE, ALL, or a set of keys."""
    for g in SKIP_GLOBS:
        if fnmatch.fnmatch(path, g):
            return NONE
    for g in TEST_GLOBS:
        if fnmatch.fnmatch(path, g):
            return NONE_TEST
    for g in CORE_GLOBS:
        if fnmatch.fnmatch(path, g):
            return ALL
    for globs, keys in FEATURE_RULES:
        if any(fnmatch.fnmatch(path, g) for g in globs):
            return keys
    return ALL  # unclassified -> fail-safe full set


def _no_probe_reason(contributions: list[tuple[str, object]]) -> str:
    """Name WHY a wholly non-contributing change selected nothing (#1359).

    CI echoes this string, so a test-only change must not be reported as
    "docs/assets only". Takes the contributions rather than re-classifying
    so the reason can never disagree with the selection above it.
    """
    tests = [f for f, c in contributions if c is NONE_TEST]
    if not tests:
        return "docs/assets only -> no probes"
    if len(tests) == len(contributions):
        return f"test sources only ({tests[0]}) -> no probes"
    return f"docs/assets and test sources only ({tests[0]}) -> no probes"


def select(changed_files: list[str]) -> tuple[list[str], str]:
    """Return (sorted probe keys to run, human reason)."""
    if not changed_files:
        return [], "no changed files"
    contributions = [(f, classify_file(f)) for f in changed_files]
    if any(c is ALL for _, c in contributions):
        trigger = next(f for f, c in contributions if c is ALL)
        return sorted(CI_ELIGIBLE), f"core/unclassified change ({trigger}) -> full CI-eligible set"
    if all(contributes_nothing(c) for _, c in contributions):
        return [], _no_probe_reason(contributions)
    keys: set[str] = set()
    for _, c in contributions:
        if not contributes_nothing(c):
            keys |= c
    keys &= CI_ELIGIBLE
    if not keys:
        return [], "no CI-eligible probes for changed paths"
    return sorted(keys), "feature-scoped selection"


KNOWN_REASON_CATEGORIES = {
    FLAKY,
    BASE_FAILING,
    SLOW_WORLDGEN,
    SCENARIO_HEAVY,
    NEEDS_GPU,
    TARGETED,
    UNCLASSIFIED,
}

MANUAL_ONLY_LABEL = "manual-only"


def validate_manual_only_reasons(mapping: object) -> list[str]:
    """Return every structural problem in a probe -> reason-records mapping.

    Deliberately takes an arbitrary object and RETURNS problems instead of
    raising: `_self_test` drives it over SYNTHETIC malformed mappings to
    prove each rejection, and a traceback there would be a crash rather
    than a reported failure. Problems come out in mapping-insertion order,
    so repeated runs print identically.

    Rejects: a non-mapping; a value that is not an ordered collection; an
    empty collection; a record that is not a `Reason`; a non-string or
    unknown category; a category repeated within one probe; a non-string
    or blank explanation.
    """
    if not isinstance(mapping, dict):
        return [f"manual-only reasons must be a dict, got {type(mapping).__name__}"]
    problems: list[str] = []
    for key, reasons in mapping.items():
        where = f"MANUAL_ONLY_REASONS[{key!r}]"
        # `str`/`bytes` are sequences but not collections OF records, and a
        # `set` has no stable order, so neither can carry a render order.
        if isinstance(reasons, (str, bytes)) or not isinstance(reasons, (list, tuple)):
            problems.append(f"{where} must be an ordered collection of Reason "
                            f"records, got {type(reasons).__name__}")
            continue
        if not reasons:
            problems.append(f"{where} declares no reasons")
            continue
        seen: set[str] = set()
        for index, record in enumerate(reasons):
            at = f"{where}[{index}]"
            if not isinstance(record, Reason):
                problems.append(f"{at} is not a Reason record "
                                f"(got {type(record).__name__})")
                continue
            category = record.category
            if not isinstance(category, str):
                problems.append(f"{at} has a non-string category "
                                f"({type(category).__name__})")
            elif category not in KNOWN_REASON_CATEGORIES:
                problems.append(f"{at} has unknown category {category!r}")
            elif category in seen:
                problems.append(f"{at} repeats category {category!r}; fold both "
                                f"facets into one explanation instead")
            else:
                seen.add(category)
            explanation = record.explanation
            if not isinstance(explanation, str):
                problems.append(f"{at} has a non-string explanation "
                                f"({type(explanation).__name__})")
            elif not explanation.strip():
                problems.append(f"{at} has a blank explanation")
    return problems


def format_manual_only_lines(key: str, reasons: Sequence[Reason],
                             width: int) -> list[str]:
    """Render one manual-only probe as `--status` lines, one per reason.

    The probe key and the `manual-only` label occupy the FIRST line only;
    every further reason is indented to the same category column. That is
    what lets a probe excluded on several counts show all of them while
    still appearing exactly once in the listing (#1440).
    """
    head = f"  {key:<{width}}  {MANUAL_ONLY_LABEL}  "
    continuation = " " * len(head)
    return [f"{head if i == 0 else continuation}[{r.category}]  {r.explanation}"
            for i, r in enumerate(reasons)]


def status_lines() -> list[str]:
    """The `--status` report as lines (#540).

    Reads the same CI_ELIGIBLE / MANUAL_ONLY_REASONS dicts `select()` and
    `_self_test()` use -- nothing here can drift from actual CI selection
    without `--self-test` catching it first. Returned rather than printed
    so `_self_test` can assert on the rendering itself.
    """
    width = max(len(k) for k in ALL_KEYS)
    lines = [f"{len(CI_ELIGIBLE)} CI-eligible, {len(MANUAL_ONLY_REASONS)} manual-only, "
             f"{len(ALL_KEYS)} total registered probes", ""]
    lines.append("-- CI-eligible --")
    for k in sorted(CI_ELIGIBLE):
        lines.append(f"  {k:<{width}}  CI-eligible")
    lines.append("")
    lines.append(f"-- {MANUAL_ONLY_LABEL} --")
    for k in sorted(MANUAL_ONLY_REASONS):
        lines.extend(format_manual_only_lines(k, MANUAL_ONLY_REASONS[k], width))
    return lines


def _reason_shape_cases() -> list[str]:
    """Prove `validate_manual_only_reasons` on SYNTHETIC mappings (#1440).

    Each case names a shape the registry must never take. The validator is
    driven through the same entry point the real registry goes through, so
    a rejection that regressed to a raise shows up here as a reported
    failure rather than a traceback out of `--self-test`.
    """
    ok = Reason(SLOW_WORLDGEN, "needs a real generated world")
    other = Reason(SCENARIO_HEAVY, "walks its legs in real time")
    accept: list[tuple[str, object]] = [
        ("one reason", {"a": (ok,)}),
        ("multiple reasons", {"a": (ok, other)}),
        ("multiple reasons in a list", {"a": [ok, other]}),
        ("several probes", {"a": (ok,), "b": (ok, other)}),
    ]
    reject: list[tuple[str, object]] = [
        ("not a mapping", [(ok,)]),
        ("empty reason set", {"a": ()}),
        ("empty reason list", {"a": []}),
        ("duplicate categories", {"a": (ok, Reason(SLOW_WORLDGEN, "again"))}),
        ("unknown category", {"a": (Reason("made-up", "why"),)}),
        ("bare tuple record", {"a": ((SLOW_WORLDGEN, "why"),)}),
        ("dict record", {"a": ({"category": SLOW_WORLDGEN, "explanation": "why"},)}),
        ("unwrapped Reason", {"a": ok}),
        ("string instead of records", {"a": "slow"}),
        ("unordered set of records", {"a": {ok}}),
        ("non-string category", {"a": (Reason(0, "why"),)}),
        ("non-string explanation", {"a": (Reason(SLOW_WORLDGEN, None),)}),
        ("blank explanation", {"a": (Reason(SLOW_WORLDGEN, "   "),)}),
    ]
    problems: list[str] = []
    for must_reject, cases in ((False, accept), (True, reject)):
        for name, mapping in cases:
            try:
                found = validate_manual_only_reasons(mapping)
            except Exception as error:  # broad on purpose: a raise IS the failure
                problems.append(f"reason-shape case {name!r} raised "
                                f"{type(error).__name__}: {error}")
                continue
            if must_reject and not found:
                problems.append(f"reason-shape case {name!r} was accepted "
                                f"but must be rejected")
            if not must_reject and found:
                problems.append(f"reason-shape case {name!r} must be accepted, "
                                f"got {found}")
    return problems


def _status_rendering_cases() -> list[str]:
    """Prove the `--status` rendering is deterministic and key-unique (#1440)."""
    problems: list[str] = []
    width = 6
    # Column layout restated independently of format_manual_only_lines:
    # two spaces, the key column, two spaces, "manual-only", two spaces.
    indent = " " * (2 + width + 2 + len(MANUAL_ONLY_LABEL) + 2)
    single = format_manual_only_lines(
        "demo", (Reason(NEEDS_GPU, "no GPU on the CI runner"),), width)
    want_single = [f"  demo    {MANUAL_ONLY_LABEL}  [{NEEDS_GPU}]  no GPU on the CI runner"]
    if single != want_single:
        problems.append(f"single-reason rendering: expected {want_single} got {single}")
    multi = format_manual_only_lines(
        "demo",
        (Reason(SLOW_WORLDGEN, "generates a real world"),
         Reason(SCENARIO_HEAVY, "walks its legs in real time")),
        width)
    want_multi = [
        f"  demo    {MANUAL_ONLY_LABEL}  [{SLOW_WORLDGEN}]  generates a real world",
        f"{indent}[{SCENARIO_HEAVY}]  walks its legs in real time",
    ]
    if multi != want_multi:
        problems.append(f"multi-reason rendering: expected {want_multi} got {multi}")
    # the probe key appears exactly once however many reasons it carries
    occurrences = sum(line.count("demo") for line in multi)
    if occurrences != 1:
        problems.append(f"multi-reason rendering repeats the probe key "
                        f"{occurrences} times, expected 1")
    # the real report is stable across renders, lists every probe key once,
    # and emits one line per declared reason
    first, second = status_lines(), status_lines()
    if first != second:
        problems.append("status_lines() is not deterministic across calls")
    heads = [ln for ln in first if f"  {MANUAL_ONLY_LABEL}  [" in ln]
    if len(heads) != len(MANUAL_ONLY_REASONS):
        problems.append(f"--status listed {len(heads)} manual-only probe keys, "
                        f"expected {len(MANUAL_ONLY_REASONS)}")
    reason_lines = sum(len(v) for v in MANUAL_ONLY_REASONS.values())
    rendered = sum(len(format_manual_only_lines(k, v, max(len(x) for x in ALL_KEYS)))
                   for k, v in MANUAL_ONLY_REASONS.items())
    if rendered != reason_lines:
        problems.append(f"--status rendered {rendered} reason lines, "
                        f"expected {reason_lines}")
    # both shapes are actually exercised by the live registry
    if not any(len(v) == 1 for v in MANUAL_ONLY_REASONS.values()):
        problems.append("no manual-only probe declares exactly one reason")
    if not any(len(v) > 1 for v in MANUAL_ONLY_REASONS.values()):
        problems.append("no manual-only probe declares multiple reasons")
    # #1440's named minimum: these two recorded independent causes in prose
    # and must now record them as separate reasons.
    for key in ("transfer_order", "expedition_retrieval"):
        if len(MANUAL_ONLY_REASONS.get(key, ())) < 2:
            problems.append(f"{key} must declare more than one manual-only reason (#1440)")
    return problems


def _reason_cases() -> list[str]:
    """Pin the human reason `select()` returns, not just its keys (#1359).

    CI echoes this string as the whole explanation for a skipped probe
    step, so a test-only change reported as "docs/assets only" would be a
    silently wrong audit trail. The key-level cases in `_self_test` cannot
    catch that: every one of these selects the empty list either way.
    """
    problems: list[str] = []
    docs_only = "docs/assets only"
    for files, name in [
        (["test/Spec.hs"], "test tree"),
        (["test-headless/Spec.hs"], "test-headless tree"),
        (["test-headless/Test/Headless/UI/ItemList.hs"], "nested test-headless source"),
        (["README.md", "test-headless/Spec.hs"], "docs plus a test source"),
    ]:
        keys, reason = select(files)
        if keys:
            problems.append(f"reason case {name!r}: expected no probes, got {keys}")
        if "test" not in reason:
            problems.append(f"reason case {name!r}: reason does not name test paths: {reason!r}")
        if docs_only in reason:
            problems.append(f"reason case {name!r}: reason misreports a test change "
                            f"as {docs_only!r}: {reason!r}")
        if not any(f in reason for f in files if classify_file(f) is NONE_TEST):
            problems.append(f"reason case {name!r}: reason names no changed test "
                            f"file: {reason!r}")
    # the docs-only reason is unchanged by the test-path split
    keys, reason = select(["README.md", "assets/x.png"])
    if keys or docs_only not in reason:
        problems.append(f"docs-only reason regressed: keys={keys} reason={reason!r}")
    return problems


def _self_test() -> int:
    """Validate the mapping wiring — no engine needed."""
    problems = []
    # every CI-eligible key is a real probe
    for k in CI_ELIGIBLE:
        if k not in ALL_KEYS:
            problems.append(f"CI_ELIGIBLE key not in PROBES registry: {k}")
    # feature rules only reference CI-eligible probes
    for globs, keys in FEATURE_RULES:
        for k in keys:
            if k not in CI_ELIGIBLE:
                problems.append(f"FEATURE_RULES references non-eligible probe: {k} ({globs[0]})")
    # #1577 requirement 7: a feature rule exists to BOUND what a PR touching
    # that area pays. One that selects the whole eligible set is the fail-safe
    # fallthrough written out by hand, and costs the gate its bound — so a
    # promotion may only ever ADD a probe to a rule, never widen one to
    # everything. CORE_GLOBS and the unclassified fallthrough are the two
    # routes that are SUPPOSED to select the full set; their cases below pin
    # that, and this check deliberately does not touch them.
    for globs, keys in FEATURE_RULES:
        if keys == CI_ELIGIBLE:
            problems.append(f"FEATURE_RULES entry selects the whole CI-eligible "
                            f"set, defeating its own bound: {globs[0]}")
    # every registered probe is classified exactly once (#540): CI-eligible
    # XOR manual-only-with-a-reason. This is what keeps --status from ever
    # silently drifting behind a newly-registered probe.
    overlap = CI_ELIGIBLE & MANUAL_ONLY_REASONS.keys()
    if overlap:
        problems.append(f"keys in both CI_ELIGIBLE and MANUAL_ONLY_REASONS: {sorted(overlap)}")
    stale = MANUAL_ONLY_REASONS.keys() - ALL_KEYS
    if stale:
        problems.append(f"MANUAL_ONLY_REASONS references keys not in PROBES registry: {sorted(stale)}")
    uncovered = ALL_KEYS - CI_ELIGIBLE - MANUAL_ONLY_REASONS.keys()
    if uncovered:
        problems.append(f"probes with no CI status at all (add to CI_ELIGIBLE or "
                         f"MANUAL_ONLY_REASONS): {sorted(uncovered)}")
    problems += validate_manual_only_reasons(MANUAL_ONLY_REASONS)
    problems += _reason_shape_cases()
    problems += _status_rendering_cases()
    problems += _reason_cases()
    # behavioural expectations
    cases = [
        (["README.md"], [], "docs only"),
        (["docs/foo.md", "assets/x.png"], [], "docs+assets"),
        # #1577: the EXACT set, not merely "cooking is in there" — restoring
        # the selection #535's review required must not cost this glob any
        # of the coverage #722 put on it.
        (["data/recipes/smelting.yaml"],
         sorted({"craft", "consumable_effects", "repair", "content_registry",
                 "cooking"}),
         "recipes -> craft + consumable_effects + repair + content_registry "
         "+ cooking (#1577 restored #535's cooking selection)"),
        (["data/buildings/furnace.yaml"],
         sorted({"craft", "consumable_effects", "repair", "cooking"}),
         "buildings -> craft + consumable_effects + repair + cooking (#1577)"),
        (["data/items/coffee_pot.yaml"],
         sorted({"cargo_capacity", "repair_item", "consumable_effects", "repair",
                 "content_registry"}),
         "items -> cargo_capacity + repair_item + consumable_effects + repair "
         "+ content_registry"),
        # #1577 requirement 5: these three cases assert the decision reached
        # in #1577 — power_workshop was MEASURED against #722's standard and
        # kept manual-only for the recorded reason above — rather than
        # restating the value direct commit b09c1518 left here. The empty
        # expectation is unchanged; what changed is that it is now evidenced.
        (["src/Power/Network.hs"], [],
         "power selects nothing: all three power probes stay manual-only on "
         "#1577's measured grounds"),
        # #1577: bacteria.yaml is the tracked infection catalogue file.
        (["data/infections/bacteria.yaml"],
         sorted({"content_registry", "infection"}),
         "infections -> the #890 registry smoke PLUS the scenario probe "
         "b09c1518 removed and #1577 re-promoted"),
        # #1577: the infection tick itself lives under src/Combat/Wounds/, so
        # the Combat rule selects the probe that grades it.
        (["src/Combat/Wounds/Tick.hs"],
         sorted({"medic_coord", "infection"}),
         "combat -> medic_coord + infection (the wound-infection tick lives "
         "here, #1577)"),
        (["src/Infection/Types.hs"],
         sorted({"content_registry", "infection"}),
         "the infection catalogue types -> the same pair as its data glob"),
        (["scripts/unit_ai.lua"], sorted(CI_ELIGIBLE), "core -> full"),
        (["scripts/unit_ai_combat.lua"], sorted(CI_ELIGIBLE),
         "unit_ai_*.lua submodule (#538) -> full"),
        (["src/SomethingNew/X.hs"], sorted(CI_ELIGIBLE), "unclassified -> full"),
        (["README.md", "src/Power/Network.hs"], [],
         "docs ignored, and power still selects nothing on #1577's measured "
         "grounds"),
        # #1359 requirement 1: either test tree alone selects nothing.
        (["test-headless/Test/Headless/UI/ItemList.hs"], [],
         "test-headless source -> no probes"),
        (["test-headless/Spec.hs"], [], "test-headless entry point -> no probes"),
        (["test/Spec.hs"], [], "graphical test entry point -> no probes"),
        (["test/Test/Engine/Graphics/Vulkan/Device.hs"], [],
         "nested graphical test source -> no probes"),
        # #1359 requirement 2: a test path neither adds nor removes probes
        # from what the production path alone selects.
        (["test-headless/Spec.hs", "data/items/coffee_pot.yaml"],
         sorted({"cargo_capacity", "repair_item", "consumable_effects", "repair",
                 "content_registry"}),
         "test + items -> exactly the items set"),
        (["test/Spec.hs", "src/Power/Network.hs"], [],
         "test + power (whose probes are all manual-only on #1577's measured "
         "grounds) -> still nothing"),
        # #1359 requirement 3: the fail-safe still fires through a test path.
        (["test-headless/Spec.hs", "src/SomethingNew/X.hs"], sorted(CI_ELIGIBLE),
         "test + unclassified production -> full"),
        # #1359 requirement 4: probe infrastructure and unrecognized tools/
        # paths keep selecting the full set.
        (["tools/probelib.py"], sorted(CI_ELIGIBLE), "probelib -> full"),
        # The launcher every probe's engine comes out of (#1570).
        (["tools/probe_engine.py"], sorted(CI_ELIGIBLE),
         "probe_engine -> full"),
        (["tools/run_probes.py"], sorted(CI_ELIGIBLE), "run_probes -> full"),
        (["tools/ci_probes.py"], sorted(CI_ELIGIBLE), "this mapping -> full"),
        (["tools/world_check.py"], sorted(CI_ELIGIBLE),
         "an unrecognized tools/ path still falls through to full"),
        # #1365 requirement 1: a widget-kit change selects exactly the one
        # probe that verifies normal-boot widget-module loading — neither
        # the full set (the pre-#1365 fallthrough) nor nothing at all.
        (["scripts/ui/slider.lua"], ["debug_console_boot"],
         "a widget module -> the boot smoke alone"),
        (["scripts/ui/item_list.lua", "scripts/ui/registry.lua"],
         ["debug_console_boot"],
         "several widget modules -> still the boot smoke alone"),
        # #1365 requirement 5: a mixed change is the UNION of what each path
        # contributes, so the UI rule can only ever ADD debug_console_boot to
        # what the production path already selected.
        (["scripts/ui/slider.lua", "data/items/coffee_pot.yaml"],
         sorted({"debug_console_boot", "cargo_capacity", "repair_item",
                 "consumable_effects", "repair", "content_registry"}),
         "widget + items -> the items set plus the boot smoke"),
        (["scripts/ui/slider.lua", "src/SomethingNew/X.hs"], sorted(CI_ELIGIBLE),
         "#1365 req 6: widget + unclassified production -> still full"),
        # #1365 requirement 7: top-level scripts are OUT of scope, and the
        # rule's glob is anchored with a trailing slash so `fnmatch`'s
        # unanchored `*` cannot capture the `scripts/ui_manager*` family.
        (["scripts/ui_manager.lua"], sorted(CI_ELIGIBLE),
         "ui_manager is not a widget module -> full"),
        (["scripts/ui_manager_boot.lua"], sorted(CI_ELIGIBLE),
         "no ui_manager submodule is captured either -> full"),
        (["scripts/hud.lua"], sorted(CI_ELIGIBLE),
         "hud is not a widget module -> full"),
    ]
    for files, expect, name in cases:
        got, reason = select(files)
        if got != expect:
            problems.append(f"case {name!r}: files={files} expected {expect} got {got} ({reason})")
    if problems:
        for p in problems:
            print(f"SELF-TEST FAIL: {p}", file=sys.stderr)
        return 1
    print("ci_probes self-test: all cases pass")
    return 0


def _status() -> int:
    """Print every registered probe's CI eligibility (#540)."""
    for line in status_lines():
        print(line)
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--changed", nargs="*", default=None,
                    help="changed file paths (repo-relative)")
    ap.add_argument("--stdin", action="store_true",
                    help="read changed file paths from stdin, one per line")
    ap.add_argument("--self-test", action="store_true",
                    help="validate the mapping and exit (no engine)")
    ap.add_argument("--status", action="store_true",
                    help="list every registered probe's CI eligibility "
                         "(CI-eligible, or manual-only with every reason category "
                         "excluding it) and exit")
    args = ap.parse_args()

    if args.self_test:
        return _self_test()
    if args.status:
        return _status()

    files = list(args.changed or [])
    if args.stdin:
        files += [ln.strip() for ln in sys.stdin if ln.strip()]

    keys, reason = select(files)
    print(f"probe selection: {reason}", file=sys.stderr)
    if keys:
        print(f"  -> {', '.join(keys)}", file=sys.stderr)
    else:
        print("  -> (no probes)", file=sys.stderr)
    # stdout: the --only value for run_probes.py (empty if none)
    print(",".join(keys))
    return 0


if __name__ == "__main__":
    sys.exit(main())
