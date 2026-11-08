#!/usr/bin/env python3
"""Engine-free self-test for `gameplay_tag_persistence_probe.scenario_files`
(#2967).

The probe asserts its isolated resource root holds no scenario DEFINITION,
so restored gameplay tags can only come from the save. The scan used to
match any file under the root whose name contained "scenario", which
always hit the tracked code module `scripts/scenario_startup.lua` (#2715)
that the root symlinks in. The engine looks scenarios up as `.yaml` files
under `<root>/scenarios/` (`src/Scenario/Path.hs`), so the scan now covers
exactly that folder. Pinned here:

  * `scripts/scenario_startup.lua`, and any code module whose name merely
    contains "scenario", are ignored, including through a symlinked
    `scripts/` family as `make_isolated_root` builds it;
  * `scenarios/ambush.yaml` and `scenarios/sub/ambush.yaml` are reported,
    whatever the file name contains;
  * a missing or empty `scenarios/` folder reports nothing;
  * a non-`.yaml` file in the folder is not a definition.

Usage:
  python3 tools/test_gameplay_tag_persistence_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import gameplay_tag_persistence_probe as probe  # type: ignore  # noqa: E402

FAILURES: list[str] = []


def check(name: str, condition: bool, detail: str = "") -> None:
    if condition:
        print(f"  [PASS] {name}")
    else:
        FAILURES.append(name)
        print(f"  [FAIL] {name}{(': ' + detail) if detail else ''}")


def touch(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("")


def main() -> int:
    with tempfile.TemporaryDirectory(prefix="gtp_scan_test_") as tmp:
        base = Path(tmp)

        # A symlinked scripts/ family holding scenario-named CODE, built
        # the way make_isolated_root builds the real root.
        source = base / "source"
        touch(source / "scripts" / "scenario_startup.lua")
        touch(source / "scripts" / "scenario_helpers" / "scenario_core.lua")
        touch(source / "data" / "scenario_notes.yaml")
        root = base / "root"
        root.mkdir()
        os.symlink(source / "scripts", root / "scripts")
        os.symlink(source / "data", root / "data")
        (root / "saves").mkdir()
        touch(root / "saves" / "scenario_slot.yaml")

        check("ignores scripts/scenario_startup.lua and scenario-named code",
              probe.scenario_files(str(root)) == [],
              str(probe.scenario_files(str(root))))

        (root / "scenarios").mkdir()
        check("an empty scenarios/ folder reports nothing",
              probe.scenario_files(str(root)) == [])

        touch(root / "scenarios" / "ambush.yaml")
        touch(root / "scenarios" / "sub" / "ambush.yaml")
        got = probe.scenario_files(str(root))
        want = sorted([str(root / "scenarios" / "ambush.yaml"),
                       str(root / "scenarios" / "sub" / "ambush.yaml")])
        check("reports scenarios/ambush.yaml and scenarios/sub/ambush.yaml",
              got == want, str(got))

        touch(root / "scenarios" / "notes.txt")
        check("only .yaml files count as definitions",
              probe.scenario_files(str(root)) == want)

    with tempfile.TemporaryDirectory(prefix="gtp_scan_test_") as tmp:
        root = Path(tmp) / "root"
        touch(root / "scripts" / "scenario_startup.lua")
        check("a root with no scenarios/ folder reports nothing",
              probe.scenario_files(str(root)) == [])

    with tempfile.TemporaryDirectory(prefix="gtp_scan_test_") as tmp:
        root = Path(tmp) / "root"
        touch(root / "scenarios" / "plain.yaml")
        check("a definition is reported whatever its name contains",
              probe.scenario_files(str(root))
              == [str(root / "scenarios" / "plain.yaml")])

    if FAILURES:
        print(f"{len(FAILURES)} failed: {FAILURES}")
        return 1
    print("all passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
