#!/usr/bin/env python3
"""Select path-relevant expensive CI gates.

On pull requests, this retains the full blocking Hspec suite while skipping
the graphical test-suite compilation, the quick worldgen-output check, and
the unit-asset inventory validation when their inputs were untouched.
Pushes to master run all of them as a post-merge backstop.

Patterns are matched with ``fnmatch``, where ``*`` crosses ``/`` and
``**`` carries no special meaning — write ``dir/*`` for a whole subtree.
"""
from __future__ import annotations

import argparse
import fnmatch
import sys


WORLDGEN_GLOBS = [
    "app/App/Dump.hs", "app/Main.hs",
    "src/Engine/Core/Init.hs", "src/Engine/Scripting/Lua/API/World/GenConfig*",
    # Generation-family subtrees use a `Name*` prefix (not `Name/*`) so each
    # family's facade module (e.g. src/World/Generate.hs, src/World/Fluids.hs)
    # matches alongside its directory. Deliberately NOT src/World/* wholesale:
    # gameplay/plumbing subtrees there (Save, Command, Thread, designations,
    # render-side Tile texturing, ...) cannot shift a bare --dump's
    # terrain/material/fluid/ice/ore layers, and must not trigger the gate.
    "src/World/Generate*", "src/World/Geology*", "src/World/Hydrology*",
    "src/World/Fluid*", "src/World/Flora*", "src/World/Weather*",
    "src/World/Ocean*", "src/World/Magma*",
    "src/World/Material*", "src/World/Plate*", "src/World/Chunk*",
    "src/World/Region*", "src/World/Tile/*", "src/World/Vegetation*",
    "src/World/Grid.hs", "src/World/Scale.hs", "src/World/Constants.hs",
    "src/World/Base.hs",
    "src/World/ZoomMap*", "src/World/Types*",
    "config/world_gen_default.yaml", "data/materials/*", "data/flora/*",
    "data/vegetation/*", "tools/world_*.py", "tools/baselines/*",
]

GRAPHICAL_GLOBS = [
    "app/*", "cbits/*", "test/*", "assets/*", "config/video*.yaml",
    "src/Engine/Graphics/*", "src/Engine/Scene/*", "src/Engine/Asset/*",
    "src/Engine/Loop/*", "src/UI/*", "src/World/Render/*",
    "synarchy.cabal", "cabal.project", "cabal.project.*",
]

# The unit-asset inventory gate (#1257): tools/test_pack_atlas.py plus
# tools/pack_atlas.py --validate-only --strict.
#
# These are fnmatch patterns, NOT globs — `*` crosses `/` and `**` means
# nothing special — so `assets/textures/units/*` already covers the whole
# subtree, and `data/units/*.yaml` would also match a nested path under
# that directory. Anything that can move which PNGs exist, which frames
# are declared, how a declaration decodes, or how the gate itself runs
# belongs here.
UNIT_ASSET_GLOBS = [
    # The assets and their declarations.
    "assets/textures/units/*", "data/units/*.yaml",
    # The checker, its self-test, this selector, and the CI wiring that
    # invokes them.
    "tools/pack_atlas.py", "tools/test_pack_atlas.py",
    "tools/ci_expensive_gates.py", "tools/ci-local.sh", "Makefile",
    ".github/workflows/ci.yml", ".github/ci/Dockerfile",
    # The unit-YAML schema/loader, the preview metadata loader, and the
    # gameplay registration loader — the three decoders that have to
    # agree with the checker about the declaration shape.
    "src/Engine/Asset/YamlUnits.hs", "src/Engine/Asset/YamlList.hs",
    "src/Engine/Preview/Unit.hs",
    "src/Engine/Scripting/Lua/API/Units/Yaml.hs",
    # The boot path that feeds every data/units/*.yaml to loadUnitYaml:
    # it is what makes an asset-only file runtime-visible at all, so a
    # change here can break the exclusion the gate exists to protect.
    "scripts/startup_loader.lua",
    # Registration of the headless group and the module list that makes
    # it compile. Either one silently un-registers Asset.UnitInventory,
    # which turns its --match gate vacuous rather than failing.
    "test-headless/Spec.hs", "synarchy.cabal",
    # Their tests and the preview probe.
    "test-headless/Test/Headless/Asset/UnitInventory.hs",
    "test-headless/Test/Headless/Asset/TextureFallback.hs",
    "test-headless/Test/Headless/Preview/UnitAnimation.hs",
    "tools/preview_probe.py",
]

# Every selectable gate. A dict rather than a chain of conditionals on
# purpose: the previous two-way `A if gate == "worldgen" else B` made an
# unrecognised gate name silently inherit GRAPHICAL_GLOBS, so a new gate
# could look wired up while actually running on the graphical patterns.
GATE_GLOBS: dict[str, list[str]] = {
    "worldgen": WORLDGEN_GLOBS,
    "graphical": GRAPHICAL_GLOBS,
    "unit-assets": UNIT_ASSET_GLOBS,
}


# The names the CLI accepts. Kept beside GATE_GLOBS and cross-checked in
# the self-test so the two can never drift apart.
GATE_CHOICES = ("worldgen", "graphical", "unit-assets")


def selected(gate: str, changed_files: list[str]) -> bool:
    """Whether a PR touching ``changed_files`` needs ``gate``."""
    try:
        patterns = GATE_GLOBS[gate]
    except KeyError:
        raise ValueError(f"unknown gate: {gate!r}") from None
    return any(any(fnmatch.fnmatch(path, pattern) for pattern in patterns)
               for path in changed_files)


def self_test() -> int:
    cases = [
        ("worldgen", ["src/World/Geology/Timeline.hs"], True),
        # Facade modules sitting NEXT to their directory must match too —
        # the original `Name/*` globs silently missed these.
        ("worldgen", ["src/World/Generate.hs"], True),
        ("worldgen", ["src/World/Fluids.hs"], True),
        ("worldgen", ["src/World/Plate.hs"], True),
        ("worldgen", ["src/World/Magma/Pool.hs"], True),
        ("worldgen", ["src/World/Material/Id.hs"], True),
        ("worldgen", ["src/World/Weather.hs"], True),
        # Non-generation src/World subtrees must NOT trigger the gate — a
        # save/thread/designation change never shifts bare --dump output.
        ("worldgen", ["src/World/Save/Storage.hs"], False),
        ("worldgen", ["src/World/Thread/Command/Save.hs"], False),
        ("worldgen", ["src/World/Mine/Types.hs"], False),
        ("worldgen", ["scripts/unit_ai.lua"], False),
        ("worldgen", ["data/materials/stone.yaml"], True),
        ("graphical", ["src/Engine/Graphics/Vulkan/Device.hs"], True),
        ("graphical", ["test/Test/Engine/Core/Queue.hs"], True),
        ("graphical", ["scripts/crafting_panel.lua"], False),
        ("graphical", ["synarchy.cabal"], True),
        # unit-assets (#1257). The positives below deliberately lead with
        # paths GRAPHICAL_GLOBS does NOT match: `assets/*` is already a
        # graphical pattern, so a suite whose positives all sat under
        # assets/textures/units/ would still pass if this gate silently
        # fell back to the graphical patterns.
        ("unit-assets", ["data/units/acolyte.yaml"], True),
        ("unit-assets", ["tools/pack_atlas.py"], True),
        ("unit-assets", ["tools/test_pack_atlas.py"], True),
        ("unit-assets", ["tools/ci_expensive_gates.py"], True),
        ("unit-assets", ["tools/ci-local.sh"], True),
        ("unit-assets", ["Makefile"], True),
        ("unit-assets", [".github/workflows/ci.yml"], True),
        ("unit-assets", [".github/ci/Dockerfile"], True),
        ("unit-assets", ["src/Engine/Asset/YamlUnits.hs"], True),
        ("unit-assets", ["src/Engine/Preview/Unit.hs"], True),
        ("unit-assets", ["src/Engine/Scripting/Lua/API/Units/Yaml.hs"], True),
        ("unit-assets",
         ["test-headless/Test/Headless/Asset/UnitInventory.hs"], True),
        ("unit-assets", ["tools/preview_probe.py"], True),
        ("unit-assets", ["scripts/startup_loader.lua"], True),
        ("unit-assets", ["test-headless/Spec.hs"], True),
        ("unit-assets", ["synarchy.cabal"], True),
        ("unit-assets",
         ["assets/textures/units/tiller/animations/idle/south/frame_000.png"],
         True),
        ("unit-assets", ["data/units/white_tailed_deer.yaml"], True),
        # ...and negatives, so the gate cannot be trivially always-true.
        ("unit-assets", ["scripts/crafting_panel.lua"], False),
        ("unit-assets", ["assets/textures/icons/skill/climbing.png"], False),
        ("unit-assets", ["data/materials/stone.yaml"], False),
        ("unit-assets", ["src/World/Geology/Timeline.hs"], False),
        ("unit-assets", ["docs/texture_infrastructure.md"], False),
        # A path selecting one gate must not drag in the others.
        ("worldgen", ["tools/pack_atlas.py"], False),
        ("worldgen", ["data/units/acolyte.yaml"], False),
        ("graphical", ["data/units/acolyte.yaml"], False),
        ("graphical", ["tools/pack_atlas.py"], False),
    ]
    failures = []
    for gate, files, expected in cases:
        actual = selected(gate, files)
        if actual != expected:
            failures.append(f"{gate} {files}: expected {expected}, got {actual}")

    # Every gate the CLI accepts must have its own pattern list, and
    # every pattern list must be reachable from the CLI. The dispatch
    # bug this replaced was exactly a gate name the CLI accepted while
    # `selected` had no patterns of its own for it.
    cli_gates = set(GATE_CHOICES)
    if cli_gates != set(GATE_GLOBS):
        failures.append(
            f"--gate choices {sorted(cli_gates)} do not match the gates with "
            f"patterns {sorted(GATE_GLOBS)}")
    for gate in sorted(cli_gates & set(GATE_GLOBS)):
        if not any(case_gate == gate for case_gate, _, _ in cases):
            failures.append(f"gate '{gate}' has no self-test case")
        if not any(case_gate == gate and expected
                   for case_gate, _, expected in cases):
            failures.append(f"gate '{gate}' has no POSITIVE self-test case")
        if not any(case_gate == gate and not expected
                   for case_gate, _, expected in cases):
            failures.append(f"gate '{gate}' has no NEGATIVE self-test case")
    try:
        selected("no-such-gate", ["anything"])
    except ValueError:
        pass
    else:
        failures.append(
            "an unknown gate name did not raise — it silently inherited "
            "another gate's patterns")

    if failures:
        for failure in failures:
            print(f"SELF-TEST FAIL: {failure}", file=sys.stderr)
        return 1
    print("ci_expensive_gates self-test: all cases pass")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--gate", choices=GATE_CHOICES)
    parser.add_argument("--changed", nargs="*", default=[])
    parser.add_argument("--stdin", action="store_true")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    if args.self_test:
        return self_test()
    if not args.gate:
        parser.error("--gate is required unless --self-test is used")
    files = list(args.changed)
    if args.stdin:
        files.extend(line.strip() for line in sys.stdin if line.strip())
    print(str(selected(args.gate, files)).lower())
    return 0


if __name__ == "__main__":
    sys.exit(main())
