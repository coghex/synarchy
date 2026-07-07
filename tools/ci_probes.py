#!/usr/bin/env python3
"""Path-selective behavior-probe selection for CI (#530).

Given the files a change touched, decide which behavior probes CI should
run — only the ones relevant to the change, with a full-set catch-all for
core/unclassified changes and a zero-probe fast path for docs/assets.
This is what makes a green CI mean "the features still work" without
paying a ~60s engine boot per probe on every PR.

The selection is deliberately FAIL-SAFE: anything this mapping can't
classify falls through to the full CI-eligible set (over-test rather than
silently skip). Docs/assets are the only things that run zero probes.

Curated set + coverage/non-coverage is documented in CLAUDE.md. Only
fast + DETERMINISTIC probes are CI-eligible; flaky/slow/worldgen-heavy or
base-failing probes stay manual-only (run_probes.py). A retry-on-flake in
run_probes.py absorbs residual sequential-engine contention so the
blocking gate doesn't redden good PRs.

Usage:
  # print the selected probe keys (one per line) + a reason on stderr
  python3 tools/ci_probes.py --changed src/Power/Network.hs data/items/x.yaml
  git diff --name-only origin/master...HEAD | python3 tools/ci_probes.py --stdin
  python3 tools/ci_probes.py --self-test    # validate the mapping, no engine
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
# Curated CI-eligible set — fast + DETERMINISTIC probes only (verified by
# repeated runs, #529/#530). Everything else stays manual-only:
#   - flaky:        craft_bill, role, chop, foraging (AI-timing sensitive)
#   - base-failing: construction, combat_anim, follow_command_priority,
#                   location_content, repair_ai (#489), lua_orphan_prune,
#                   physiology
#   - slow/worldgen: flora_growth, till, multiworld_save, location_overlay,
#                    location_stamp_idempotent, farm_ai
# Growing this set is a follow-up: prove a probe deterministic, then add it.
# --------------------------------------------------------------------------
CI_ELIGIBLE = {
    "cargo_capacity",
    "collapse_crawl",
    "concussion_revive",
    "craft",
    "disarm",
    "injury_log",
    "item_instance",
    "item_temp",
    "medic_coord",
    "power_workshop",
    "repair_item",
    "save_pause",
}

# Sentinels (distinct objects so `is` comparisons are unambiguous).
ALL = object()   # this file can affect anything -> full CI-eligible set
NONE = object()  # this file affects no probe -> contributes nothing

# Docs / assets: zero probes. Checked FIRST.
SKIP_GLOBS = [
    "*.md", "docs/*", "assets/*", "*.txt", "LICENSE*", ".gitignore",
    "*.png", "*.jpg",
]

# Core / shared code: a change here can affect ANY probe -> full set.
# The AI script stack + unit/world threads + core data are loaded by
# essentially every probe.
CORE_GLOBS = [
    "src/Engine/Core/*", "src/Engine/Monad*",
    "src/Unit/*", "src/World/Thread/*", "src/World/Save/*",
    "scripts/unit_ai.lua", "scripts/unit_resources.lua", "scripts/unit_stats.lua",
    "scripts/movement_arena.lua",
    "data/units/*", "data/materials/*", "data/substances/*",
    "tools/probelib.py", "tools/run_probes.py", "tools/ci_probes.py",
    ".github/workflows/ci.yml",
]

# Feature-area rules: path glob(s) -> the CI-eligible probes covering it.
# First matching rule wins per file. Keys here must be in CI_ELIGIBLE.
FEATURE_RULES: list[tuple[list[str], set[str]]] = [
    (["src/Combat/*", "scripts/acolyte_combat.lua", "scripts/combat_log.lua",
      "scripts/injury_log*.lua", "src/Infection/*", "data/infections/*"],
     {"collapse_crawl", "concussion_revive", "disarm", "injury_log", "medic_coord"}),
    (["src/Craft/*", "data/recipes/*", "scripts/crafting_panel.lua",
      "scripts/craft*.lua"],
     {"craft"}),
    (["src/Power/*", "scripts/wire.lua", "scripts/power*.lua",
      "data/structure_packs/*"],
     {"power_workshop"}),
    (["src/Item/*", "data/items/*", "src/Equipment/*", "data/equipment/*"],
     {"item_instance", "item_temp", "cargo_capacity", "repair_item"}),
    (["src/Building/*", "data/buildings/*"],
     {"power_workshop", "craft"}),
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
    keys: set[str] = set()
    for _, c in contributions:
        if c is not NONE:
            keys |= c
    keys &= CI_ELIGIBLE
    if not keys:
        return [], "docs/assets only -> no probes"
    return sorted(keys), "feature-scoped selection"


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
    # behavioural expectations
    cases = [
        (["README.md"], [], "docs only"),
        (["docs/foo.md", "assets/x.png"], [], "docs+assets"),
        (["data/recipes/smelting.yaml"], sorted({"craft"}), "recipes -> craft"),
        (["src/Power/Network.hs"], sorted({"power_workshop"}), "power"),
        (["scripts/unit_ai.lua"], sorted(CI_ELIGIBLE), "core -> full"),
        (["src/SomethingNew/X.hs"], sorted(CI_ELIGIBLE), "unclassified -> full"),
        (["README.md", "src/Power/Network.hs"], sorted({"power_workshop"}), "docs ignored, power kept"),
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--changed", nargs="*", default=None,
                    help="changed file paths (repo-relative)")
    ap.add_argument("--stdin", action="store_true",
                    help="read changed file paths from stdin, one per line")
    ap.add_argument("--self-test", action="store_true",
                    help="validate the mapping and exit (no engine)")
    args = ap.parse_args()

    if args.self_test:
        return _self_test()

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
