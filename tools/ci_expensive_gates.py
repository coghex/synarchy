#!/usr/bin/env python3
"""Select path-relevant expensive CI gates.

On pull requests, this retains the full blocking Hspec suite while skipping
the graphical test-suite compilation and quick worldgen-output check when
their inputs were untouched. Pushes to master run both as a post-merge
backstop.
"""
from __future__ import annotations

import argparse
import fnmatch
import sys


WORLDGEN_GLOBS = [
    "app/App/Dump.hs", "app/Main.hs",
    "src/Engine/Core/Init.hs", "src/Engine/Scripting/Lua/API/World/GenConfig*",
    "src/World/Generate/*", "src/World/Geology/*", "src/World/Hydrology/*",
    "src/World/Fluid/*", "src/World/Flora/*", "src/World/Climate/*",
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


def selected(gate: str, changed_files: list[str]) -> bool:
    """Whether a PR touching ``changed_files`` needs ``gate``."""
    patterns = WORLDGEN_GLOBS if gate == "worldgen" else GRAPHICAL_GLOBS
    return any(any(fnmatch.fnmatch(path, pattern) for pattern in patterns)
               for path in changed_files)


def self_test() -> int:
    cases = [
        ("worldgen", ["src/World/Geology/Timeline.hs"], True),
        ("worldgen", ["scripts/unit_ai.lua"], False),
        ("worldgen", ["data/materials/stone.yaml"], True),
        ("graphical", ["src/Engine/Graphics/Vulkan/Device.hs"], True),
        ("graphical", ["test/Test/Engine/Core/Queue.hs"], True),
        ("graphical", ["scripts/crafting_panel.lua"], False),
        ("graphical", ["synarchy.cabal"], True),
    ]
    failures = []
    for gate, files, expected in cases:
        actual = selected(gate, files)
        if actual != expected:
            failures.append(f"{gate} {files}: expected {expected}, got {actual}")
    if failures:
        for failure in failures:
            print(f"SELF-TEST FAIL: {failure}", file=sys.stderr)
        return 1
    print("ci_expensive_gates self-test: all cases pass")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--gate", choices=("worldgen", "graphical"))
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
