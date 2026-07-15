#!/usr/bin/env python3
"""Path-selective behavior-probe selection for CI (#530).

Given the files a change touched, decide which behavior probes CI should
run — only the ones relevant to the change, with a full-set catch-all for
core/unclassified changes and zero probes for docs/assets or subsystems
whose behavior probes are intentionally manual-only.
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
"""
from __future__ import annotations

import argparse
import fnmatch
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from run_probes import PROBES  # noqa: E402

ALL_KEYS = {p[0] for p in PROBES}

# --------------------------------------------------------------------------
# Curated CI-eligible set — intentionally SMALL smoke coverage only. A probe
# can be deterministic and still be too narrow or too expensive for every
# matching PR; keep those manual-only and run them deliberately when touching
# their subsystem.
# --------------------------------------------------------------------------
CI_ELIGIBLE = {
    "cargo_capacity",
    "consumable_effects",
    "craft",
    "medic_coord",
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

# Every registered probe (ALL_KEYS) NOT in CI_ELIGIBLE must have an entry
# here: (category, one-line reason). `_self_test` enforces full coverage so
# a newly-registered probe can't silently land in neither bucket.
MANUAL_ONLY_REASONS: dict[str, tuple[str, str]] = {
    # --- flaky: AI-reaction/arbitration timing the slower, variable-speed
    # Linux CI runner destabilizes run-to-run;
    # within-run retry can't fix run-to-run flakiness. ---
    "craft_bill": (FLAKY, "craft_job AI claim/work timing flakes run-to-run on CI"),
    "role": (FLAKY, "role-hysteresis timing flakes run-to-run on CI"),
    "chop": (FLAKY, "chop AI claim/work timing flakes run-to-run on CI"),
    "foraging": (FLAKY, "foraging AI timing flakes run-to-run on CI"),
    "sleep": (FLAKY, "go_to_sleep AI goal-arbitration + multi-hop pose-chain "
                     "timing flakes run-to-run on CI (#722; the disarm lesson — "
                     "local greenness isn't sufficient evidence for an "
                     "AI-reaction/arbitration-timing probe)"),
    "combat_anim": (FLAKY, "the attacker occasionally falls to its death approaching "
                           "the target and never swings, failing 'a swing animation "
                           "appeared' (1/3 solo runs, #724)"),
    "follow_command_priority": (FLAKY, "a struck goal-bound unit occasionally treats "
                                       "an ally instead of engaging combat, failing "
                                       "'combat reached over a pending move' "
                                       "(1/3 solo runs, #724)"),
    "repair_ai": (FLAKY, "repair-AI claim/fetch/work timing flakes with a DIFFERENT "
                         "failing-check set each run (3/3 solo runs failed, no two "
                         "alike, #724) — not #489 (whetstone), which is long fixed "
                         "and never implicated"),
    "physiology": (FLAKY, "'temperate (22C/0.5): circ min' sits right at its 0.75 "
                          "pass threshold and failed both failing runs (0.74, 0.75); "
                          "the combat-vs-idle calorie-drain-ratio check also flaked "
                          "once, alongside it (2/3 solo runs failed, #724)"),
    # --- scenario-heavy: deterministic enough to run manually, but either
    # long-running or broad end-to-end scenarios that make the blocking PR
    # gate too expensive. ---
    "construction": (SCENARIO_HEAVY, "long construct_job AI end-to-end scenario: "
                                     "inventory + ground sourcing, dead-claimant "
                                     "release, blueprint staking (4 phases, "
                                     "~123-168s solo); 14/14 checks passed all 3 "
                                     "solo runs (#724)"),
    "infection": (SCENARIO_HEAVY, "timed infection/sepsis scenario with deliberate sleeps"),
    "location_content": (SCENARIO_HEAVY, "four real-engine-boot scenario: ruin content "
                                         "spawn + geometry, save/quit/restart/load "
                                         "round-trip, bogus/valid content-registry "
                                         "validation, and hidden-page multiworld "
                                         "building/unit spawn (#800 replaced the stale "
                                         "loot_names allowlist with the live item "
                                         "registry, resolving the prior quinoa_sack flake)"),
    "item_instance": (SCENARIO_HEAVY, "real worldgen plus save/load identity regression"),
    "item_temp": (SCENARIO_HEAVY, "real worldgen, cooling waits, and save/load round-trip"),
    "power_workshop": (SCENARIO_HEAVY, "long powered-workshop AI plus day/night balance scenario"),
    "power": (SCENARIO_HEAVY, "long build-tool power-node placement + wire network + "
                              "day/night balance + save/restart/load round-trip scenario"),
    "save_pause": (SCENARIO_HEAVY, "real worldgen plus save/load pause race checks"),
    "save_barrier": (SCENARIO_HEAVY, "two real engine boots plus worldgen/save/load boundary smoke"),
    # --- targeted: useful regression probes, but too narrow for the default
    # PR gate. Run them when touching the named feature. ---
    "collapse_crawl": (TARGETED, "narrow #304 collapse/crawl hysteresis regression"),
    "concussion_revive": (TARGETED, "narrow #304 concussion revive hysteresis regression"),
    "config_migration": (TARGETED, "narrow #786 pre-#661 legacy config upgrade regression"),
    "config_state": (TARGETED, "narrow #638 config load/save vs git-tracking regression"),
    "cooking": (TARGETED, "cooking content integration; craft remains the generic craft smoke gate"),
    "disarm": (TARGETED, "narrow #193 disabled-hand auto-drop regression"),
    "injury_log": (TARGETED, "injury-log backend plumbing, narrower than the combat subsystem"),
    "lua_strict_msg": (TARGETED, "narrow #622 LuaToEngineMsg/LuaMsg strictness crash regression"),
    "machine_shop": (TARGETED, "electric furnace + machine_shop content regression, narrower "
                               "than the generic #590 power-draw mechanism probe"),
    "mental_state": (TARGETED, "narrow #352 mental-state ladder/hysteresis/break-AI regression"),
    "movement": (TARGETED, "registered CI invocation takes no arguments, so it runs only "
                           "movement_probe.py's default corner_trap diagonal-routing "
                           "regression, not the cliff/fall/ramp/stamina courses the "
                           "obstacle-course description implies (#722 defined promotion "
                           "broadness from the actual registered invocation; #754 made "
                           "the unsupported promotion; #772 demoted it)"),
    "resource_root": (TARGETED, "narrow #636 resource-root launch-contract regression "
                                "(also runs its own small worldgen dump)"),
    "text_encoding": (TARGETED, "narrow #618 Lua text API decodeUtf8Lenient regression, "
                                "extended by #665 with a representative non-Text API "
                                "(world.show) boundary from the same decodeUtf8Lenient sweep"),
    "blood_decal": (TARGETED, "narrow #604/#606 blood decal texture reuse/eviction/render regression"),
    "blood_impact": (TARGETED, "narrow #607 wound-to-impact-blood mapping regression"),
    "circadian": (TARGETED, "narrow #611 circadian urge + sleep_pressure drain regression"),
    "circadian_species": (TARGETED, "narrow #613 species-specific circadian phase regression"),
    "thought": (TARGETED, "thought event/log backend plumbing, narrower than the full "
                          "psychology arc (mirrors injury_log_probe.py)"),
    "wire": (TARGETED, "narrow #359 wire connection/path-builder/build-AI regression"),
    "lua_orphan_prune": (TARGETED, "narrow #195 Lua per-id AI-state-pruning save/load "
                                   "regression; ~40s solo, passed all 3 solo runs (#724)"),
    "state_of_mind": (TARGETED, "narrow #350 state-of-mind/awareness-term regression; "
                                "periodic thoughts (#351) are neutralised for the probe's "
                                "own engine so a random 0-30s thought can't land inside its "
                                "mood-drift sampling windows (#793)"),
    # (no probes currently classified base-failing — the category stays
    # defined below for any future genuinely-broken-on-master case.)
    # --- needs-gpu: requires a real Vulkan device, which the CI runner
    # does not have. First candidate for a future GPU-equipped CI lane. ---
    "offscreen": (NEEDS_GPU, "boots the full Vulkan render pipeline (windowless) — no GPU on the CI runner"),
    "blood_gpu_lifecycle": (NEEDS_GPU, "offscreen boot: uploadBloodTextures needs a real Vulkan "
                                       "device to upload/dispose blood textures (#788) — no GPU on the CI runner"),
    "preview": (NEEDS_GPU, "real preview boot creates a GLFW window and calls "
                           "initializeVulkan (app/App/Preview.hs), same as the "
                           "graphical boot path — no GPU on the CI runner (#722)"),
    # --- slow/worldgen-heavy: needs a real generated world, not the flat
    # arena — too slow for a blocking per-PR gate. ---
    "action_outcome": (SLOW_WORLDGEN, "needs a real generated world to scan for a mixed tillable/fluid box and a real tree for the chop partial path (#646)"),
    "flora_growth": (SLOW_WORLDGEN, "needs a real generated world for natural ground cover"),
    "multiworld_save": (SLOW_WORLDGEN, "generates two real world pages"),
    "location_overlay": (SLOW_WORLDGEN, "needs real worldgen for overlay placement"),
    "location_stamp_idempotent": (SLOW_WORLDGEN, "needs real worldgen plus a save/restart/reload round-trip"),
    "thermo_altitude": (SLOW_WORLDGEN, "needs a real generated world (worldSize 128) for elevation data, ~1 min runtime"),
    "crop": (SLOW_WORLDGEN, "needs a real generated world for natural row-crop placement + "
                            "groundcover planting, plus a save/load round-trip"),
    "plant": (SLOW_WORLDGEN, "needs a real generated world for natural ground cover + "
                             "real climate/slope suitability data"),
    "till": (SLOW_WORLDGEN, "needs a real generated world for natural ground cover to "
                            "exercise the tillable-tile filter; slow AI loop (~7 min observed)"),
    "farm_ai": (SLOW_WORLDGEN, "needs a real generated world for the till->plant->harvest AI "
                               "loop across 5 distinct tillable sites; slowest registered "
                               "probe (~11 min observed, O(n^2) TCP tile scan over natural terrain)"),
}

# Sentinels (distinct objects so `is` comparisons are unambiguous).
ALL = object()   # this file can affect anything -> full CI-eligible set
NONE = object()  # this file affects no probe -> contributes nothing

# Docs / assets: zero probes. Checked FIRST.
SKIP_GLOBS = [
    "*.md", "docs/*", "assets/*", "*.txt", "LICENSE*", ".gitignore",
    "*.png", "*.jpg",
    # The playtest harness (#647) is human-run against a windowed
    # instance and cannot alter engine behavior — no probe covers it
    # and none should run for it.
    "tools/playtest/*",
]

# Core / shared code: a change here can affect ANY probe -> full set.
# The AI script stack + unit/world threads + core data are loaded by
# essentially every probe.
CORE_GLOBS = [
    "src/Engine/Core/*", "src/Engine/Monad*",
    "src/Unit/*", "src/World/Thread/*", "src/World/Save/*",
    "scripts/unit_ai.lua", "scripts/unit_ai_*.lua",
    "scripts/unit_resources.lua", "scripts/unit_stats.lua",
    "scripts/movement_arena.lua",
    "data/units/*", "data/materials/*", "data/substances/*",
    "tools/probelib.py", "tools/run_probes.py", "tools/ci_probes.py",
    ".github/workflows/ci.yml",
]

# Feature-area rules: path glob(s) -> the CI-eligible probes covering it.
# First matching rule wins per file. Keys here must be in CI_ELIGIBLE.
# Empty sets are intentional for subsystems whose behavior probes are now
# manual-only because they are scenario-heavy or too narrowly targeted.
FEATURE_RULES: list[tuple[list[str], set[str]]] = [
    (["src/Combat/*", "scripts/acolyte_combat.lua", "scripts/combat_log.lua",
      "scripts/injury_log*.lua"],
     {"medic_coord"}),
    (["src/Infection/*", "data/infections/*"],
     set()),
    (["src/Craft/*", "data/recipes/*", "scripts/crafting_panel.lua",
      "scripts/craft*.lua", "scripts/cooking*.lua"],
     # data/recipes/* also covers repair.yaml (repair-tagged recipes) and
     # brew_coffee (consumable_effects' brew step) — both promoted probes
     # (#722) load the full data/recipes/*.yaml glob at bootstrap.
     {"craft", "consumable_effects", "repair"}),
    (["src/Power/*", "scripts/wire.lua", "scripts/power*.lua",
      "data/structure_packs/*"],
     set()),
    (["src/Item/*", "data/items/*", "src/Equipment/*", "data/equipment/*"],
     # consumable_effects exercises coffee_pot/coffee_grounds/water; repair
     # exercises axe_steel/whetstone/lignite_chunk (#722).
     {"cargo_capacity", "repair_item", "consumable_effects", "repair"}),
    (["src/Building/*", "data/buildings/*"],
     # consumable_effects builds a kitchen; repair builds a furnace +
     # workbench (#722).
     {"craft", "consumable_effects", "repair"}),
]


def classify_file(path: str):
    """A single file's probe contribution: NONE, ALL, or a set of keys."""
    for g in SKIP_GLOBS:
        if fnmatch.fnmatch(path, g):
            return NONE
    for g in CORE_GLOBS:
        if fnmatch.fnmatch(path, g):
            return ALL
    for globs, keys in FEATURE_RULES:
        if any(fnmatch.fnmatch(path, g) for g in globs):
            return keys
    return ALL  # unclassified -> fail-safe full set


def select(changed_files: list[str]) -> tuple[list[str], str]:
    """Return (sorted probe keys to run, human reason)."""
    if not changed_files:
        return [], "no changed files"
    contributions = [(f, classify_file(f)) for f in changed_files]
    if any(c is ALL for _, c in contributions):
        trigger = next(f for f, c in contributions if c is ALL)
        return sorted(CI_ELIGIBLE), f"core/unclassified change ({trigger}) -> full CI-eligible set"
    if all(c is NONE for _, c in contributions):
        return [], "docs/assets only -> no probes"
    keys: set[str] = set()
    for _, c in contributions:
        if c is not NONE:
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
    for k, (category, reason) in MANUAL_ONLY_REASONS.items():
        if category not in KNOWN_REASON_CATEGORIES:
            problems.append(f"MANUAL_ONLY_REASONS[{k!r}] has unknown category {category!r}")
        if not reason.strip():
            problems.append(f"MANUAL_ONLY_REASONS[{k!r}] has an empty reason")
    # behavioural expectations
    cases = [
        (["README.md"], [], "docs only"),
        (["docs/foo.md", "assets/x.png"], [], "docs+assets"),
        (["data/recipes/smelting.yaml"],
         sorted({"craft", "consumable_effects", "repair"}),
         "recipes -> craft + consumable_effects + repair"),
        (["data/buildings/furnace.yaml"],
         sorted({"craft", "consumable_effects", "repair"}),
         "buildings -> craft + consumable_effects + repair"),
        (["data/items/coffee_pot.yaml"],
         sorted({"cargo_capacity", "repair_item", "consumable_effects", "repair"}),
         "items -> cargo_capacity + repair_item + consumable_effects + repair"),
        (["src/Power/Network.hs"], [], "power probes are manual-only"),
        (["data/infections/staph.yaml"],
         [],
         "infection probes are manual-only (#593)"),
        (["scripts/unit_ai.lua"], sorted(CI_ELIGIBLE), "core -> full"),
        (["scripts/unit_ai_combat.lua"], sorted(CI_ELIGIBLE),
         "unit_ai_*.lua submodule (#538) -> full"),
        (["src/SomethingNew/X.hs"], sorted(CI_ELIGIBLE), "unclassified -> full"),
        (["README.md", "src/Power/Network.hs"], [], "docs ignored, power manual-only"),
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
    """Print every registered probe's CI eligibility (#540).

    Reads the same CI_ELIGIBLE / MANUAL_ONLY_REASONS dicts `select()` and
    `_self_test()` use — nothing here can drift from actual CI selection
    without `--self-test` catching it first.
    """
    width = max(len(k) for k in ALL_KEYS)
    print(f"{len(CI_ELIGIBLE)} CI-eligible, {len(MANUAL_ONLY_REASONS)} manual-only, "
          f"{len(ALL_KEYS)} total registered probes\n")
    print("-- CI-eligible --")
    for k in sorted(CI_ELIGIBLE):
        print(f"  {k:<{width}}  CI-eligible")
    print("\n-- manual-only --")
    for k in sorted(MANUAL_ONLY_REASONS):
        category, reason = MANUAL_ONLY_REASONS[k]
        print(f"  {k:<{width}}  manual-only  [{category}]  {reason}")
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
                         "(CI-eligible, or manual-only with a reason category) and exit")
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
