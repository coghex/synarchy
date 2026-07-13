#!/usr/bin/env python3
"""Headless config-migration probe (#786).

`Engine.Core.Init.migrateLegacyConfig` is the upgrade path from the
pre-#786 tracked config layout (`config/video.yaml`,
`config/keybinds.yaml`, `config/notifications.yaml`) to the current
gitignored `*.local.yaml` runtime paths. Those three legacy paths stay
tracked with content equal to the versioned default/registry (never a
real player's values) purely so a readable legacy file always exists —
see CLAUDE.md for why. This probe asserts the engine-side upgrade
contract end to end:

  0. The REAL committed tree (not a hand-placed fixture) retains a
     readable legacy file, resolves it to the versioned default, and
     migrates it into the matching `*.local.yaml` on a plain boot —
     the direct-updater path itself, not just the mechanism in
     isolation.
  1. A legacy file with distinct non-default values, present with no
     local file yet, gets migrated on boot: the resolved config reflects
     the legacy values, and the matching `*.local.yaml` is written.
  2. A second boot is idempotent: editing the legacy file afterward has
     no further effect — the already-migrated local file keeps winning.
  3. When both a legacy file and a genuine local file exist from the
     start, the local file wins outright and migration never touches
     either file.
  4. A malformed legacy file falls back safely to the versioned default
     (video) without ever creating a local file — and, separately, a
     malformed legacy file next to an already-valid local file leaves
     that local file completely untouched.

Any local config files present before the probe runs are backed up and
restored afterward. Phases 1-4 additionally overwrite (then restore)
the tracked legacy files with their own fixtures to drive the mechanism
directly with known, distinct values.

Usage:
  python3 tools/config_migration_probe.py [--port 9166]

Exit 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import yaml
from probelib import quit_engine, boot, send

LOG = "/tmp/config_migration_probe_engine.log"

LOCAL_FILES = [
    "config/video.local.yaml",
    "config/keybinds.local.yaml",
    "config/notifications.local.yaml",
]
LEGACY_FILES = [
    "config/video.yaml",
    "config/keybinds.yaml",
    "config/notifications.yaml",
]

BACKUP_DIR = "/tmp/config_migration_probe_backup"

LEGACY_VIDEO = """video:
  resolution:
    width: 1920
    height: 1080
  window_mode: windowed
  ui_scale: 1.75
  vsync: false
  frame_limit: 45
  msaa: 1
  brightness: 100
  pixel_snap: false
  texture_filter: nearest
  tooltip_dwell_ms: 400
  tooltip_hint_delay_ms: 400
"""

LEGACY_KEYBINDS = """keybinds:
  moveUp: [I, K]
  moveDown: [Down, S]
  moveLeft: [Left, A]
  moveRight: [Right, D]
  rotateCCW: [Q]
  rotateCW: [E]
  resetZTracking: [Home]
  escape: [Escape]
  openShell: [Grave]
  toggleEventLog: [L]
"""

LEGACY_NOTIFICATIONS = """categories:
  debug:
    log: true
    popup: false
    pause: false
"""

MALFORMED_YAML = "video: [this, is: not, valid: {yaml"


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f"  ({detail})" if detail else ""))
    return ok


def git_status(paths: list[str]) -> str:
    r = subprocess.run(["git", "status", "--short", "--"] + paths,
                        capture_output=True, text=True, check=True)
    return r.stdout


def clear_all() -> None:
    for p in LOCAL_FILES + LEGACY_FILES:
        if os.path.exists(p):
            os.remove(p)


def backup_local_only() -> dict[str, str]:
    """Back up (move aside) only the *.local.yaml paths, leaving the
    tracked legacy files exactly as committed — used for phase 0, which
    tests the real repo tree rather than a hand-placed fixture."""
    backups = {}
    os.makedirs(BACKUP_DIR, exist_ok=True)
    for p in LOCAL_FILES:
        if os.path.exists(p):
            bak = os.path.join(BACKUP_DIR, os.path.basename(p) + ".orig")
            shutil.move(p, bak)
            backups[p] = bak
    return backups


def restore_local_only(backups: dict[str, str]) -> None:
    for p in LOCAL_FILES:
        if os.path.exists(p):
            os.remove(p)
    for p, bak in backups.items():
        shutil.move(bak, p)


def backup_all() -> dict[str, str]:
    backups = {}
    os.makedirs(BACKUP_DIR, exist_ok=True)
    for p in LOCAL_FILES + LEGACY_FILES:
        if os.path.exists(p):
            bak = os.path.join(BACKUP_DIR, os.path.basename(p) + ".orig")
            shutil.move(p, bak)
            backups[p] = bak
    return backups


def restore_all(backups: dict[str, str]) -> None:
    clear_all()
    for p, bak in backups.items():
        shutil.move(bak, p)


def load_yaml(path: str):
    with open(path) as f:
        return yaml.safe_load(f)


def get_video_ui_scale(port: int) -> float:
    r = send(port,
              "local w,h,wm,uis,vs = engine.getVideoConfig(); "
              "return uis..'|'..tostring(vs)")
    scale, vsync = r.split("|")
    return float(scale), vsync == "true"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9166)
    args = ap.parse_args()

    passed = True
    proc = None

    # ----------------------------------------------------------------
    # Phase 0: the REAL committed tree (not a hand-placed fixture)
    # retains a readable legacy file and migrates it cleanly. This is
    # the direct-updater path itself, not just the mechanism exercised
    # against synthetic fixtures below.
    # ----------------------------------------------------------------
    print("0. the real committed legacy files resolve to versioned defaults and migrate")
    for p in LEGACY_FILES:
        passed &= check(f"tracked {p} is present on disk", os.path.exists(p))
    status = git_status(LEGACY_FILES)
    passed &= check("tracked legacy files are unmodified (clean git status)",
                     status == "", status.strip())

    local_backups = backup_local_only()
    try:
        for p in LOCAL_FILES:
            passed &= check(f"{p} absent pre-boot", not os.path.exists(p))

        proc = boot(args.port, log=LOG)

        want_default = load_yaml("config/video_default.yaml")["video"]
        scale, vsync = get_video_ui_scale(args.port)
        passed &= check("real legacy video.yaml resolves to the versioned default ui_scale",
                         abs(scale - float(want_default["ui_scale"])) < 1e-6, str(scale))
        passed &= check("real legacy video.yaml resolves to the versioned default vsync",
                         vsync == bool(want_default["vsync"]), str(vsync))

        want_kb = load_yaml("config/keybinds_default.yaml")["keybinds"]
        r = send(args.port, "local b = engine.getKeybinds(); return table.concat(b.moveUp, ',')")
        passed &= check("real legacy keybinds.yaml resolves to the versioned default moveUp",
                         r == ",".join(want_kb["moveUp"]), r)

        r = send(args.port,
                  "local cfg = engine.getNotificationCfg(); "
                  "for _,c in ipairs(cfg) do if c.id == 'debug' then "
                  "return tostring(c.log) end end; return 'NOTFOUND'")
        passed &= check("real legacy notifications.yaml resolves to the registry default",
                         r == "false", r)

        for p in LOCAL_FILES:
            passed &= check(f"{p} created by migrating the real committed legacy file",
                             os.path.exists(p))

        quit_engine(args.port, proc)
        proc = None
    finally:
        if proc is not None:
            quit_engine(args.port, proc)
            proc = None
        restore_local_only(local_backups)

    backups = backup_all()
    try:
        # ------------------------------------------------------------
        # Phase 1: upgrade — legacy present, local absent, all three
        # ------------------------------------------------------------
        print("1. upgrade: legacy files with distinct non-default values, no local yet")
        clear_all()
        with open("config/video.yaml", "w") as f:
            f.write(LEGACY_VIDEO)
        with open("config/keybinds.yaml", "w") as f:
            f.write(LEGACY_KEYBINDS)
        with open("config/notifications.yaml", "w") as f:
            f.write(LEGACY_NOTIFICATIONS)

        proc = boot(args.port, log=LOG)

        scale, vsync = get_video_ui_scale(args.port)
        passed &= check("video: legacy ui_scale=1.75 is effective",
                         abs(scale - 1.75) < 1e-6, str(scale))
        passed &= check("video: legacy vsync=false is effective", vsync is False, str(vsync))

        r = send(args.port, "local b = engine.getKeybinds(); return table.concat(b.moveUp, ',')")
        passed &= check("keybinds: legacy moveUp=[I,K] is effective", r == "I,K", r)

        r = send(args.port,
                  "local cfg = engine.getNotificationCfg(); "
                  "for _,c in ipairs(cfg) do if c.id == 'debug' then "
                  "return tostring(c.log) end end; return 'NOTFOUND'")
        passed &= check("notifications: legacy debug.log=true is effective", r == "true", r)

        for p in LOCAL_FILES:
            passed &= check(f"{p} created by migration", os.path.exists(p))
        if os.path.exists("config/video.local.yaml"):
            migrated = load_yaml("config/video.local.yaml")["video"]
            passed &= check("migrated video.local.yaml carries the legacy ui_scale",
                             abs(float(migrated["ui_scale"]) - 1.75) < 1e-6)

        quit_engine(args.port, proc)
        proc = None

        # ------------------------------------------------------------
        # Phase 2: idempotent second boot — legacy edited afterward,
        # must not be re-migrated over the already-local value.
        # ------------------------------------------------------------
        print("2. idempotent second boot: post-migration legacy edits are ignored")
        with open("config/video.yaml", "w") as f:
            f.write(LEGACY_VIDEO.replace("ui_scale: 1.75", "ui_scale: 3.3"))

        proc = boot(args.port, log=LOG)
        scale, _ = get_video_ui_scale(args.port)
        passed &= check("second boot keeps the phase-1 migrated value (1.75), "
                         "ignoring the now-edited legacy file",
                         abs(scale - 1.75) < 1e-6, str(scale))
        quit_engine(args.port, proc)
        proc = None
        migrated = load_yaml("config/video.local.yaml")["video"]
        passed &= check("video.local.yaml on disk is unchanged by the second boot",
                         abs(float(migrated["ui_scale"]) - 1.75) < 1e-6)

        # ------------------------------------------------------------
        # Phase 3: a genuine newer local file wins over a legacy file
        # present from the very start (not just a stale post-migration
        # edit as in phase 2).
        # ------------------------------------------------------------
        print("3. existing newer local state wins over legacy state")
        clear_all()
        with open("config/video.yaml", "w") as f:
            f.write(LEGACY_VIDEO)  # ui_scale 1.75
        with open("config/video.local.yaml", "w") as f:
            f.write(LEGACY_VIDEO.replace("ui_scale: 1.75", "ui_scale: 9.9"))

        proc = boot(args.port, log=LOG)
        scale, _ = get_video_ui_scale(args.port)
        passed &= check("resolved config uses the local value (9.9), not the legacy one (1.75)",
                         abs(scale - 9.9) < 1e-6, str(scale))
        quit_engine(args.port, proc)
        proc = None
        untouched = load_yaml("config/video.yaml")["video"]
        passed &= check("legacy config/video.yaml itself is left untouched",
                         abs(float(untouched["ui_scale"]) - 1.75) < 1e-6)

        # ------------------------------------------------------------
        # Phase 4: malformed legacy state fails safely.
        # ------------------------------------------------------------
        print("4. malformed legacy state falls back safely")
        clear_all()
        with open("config/video.yaml", "w") as f:
            f.write(MALFORMED_YAML)

        proc = boot(args.port, log=LOG)
        want_default = load_yaml("config/video_default.yaml")["video"]
        scale, _ = get_video_ui_scale(args.port)
        passed &= check("malformed legacy -> falls back to the versioned default ui_scale",
                         abs(scale - float(want_default["ui_scale"])) < 1e-6, str(scale))
        passed &= check("no video.local.yaml is created from a malformed legacy file",
                         not os.path.exists("config/video.local.yaml"))
        quit_engine(args.port, proc)
        proc = None

        print("4b. malformed legacy state does not destroy a valid newer local file")
        with open("config/video.local.yaml", "w") as f:
            f.write(LEGACY_VIDEO.replace("ui_scale: 1.75", "ui_scale: 4.2"))
        # config/video.yaml is still the malformed fixture from 4a.

        proc = boot(args.port, log=LOG)
        scale, _ = get_video_ui_scale(args.port)
        passed &= check("valid local value (4.2) survives next to a malformed legacy file",
                         abs(scale - 4.2) < 1e-6, str(scale))
        quit_engine(args.port, proc)
        proc = None
        kept = load_yaml("config/video.local.yaml")["video"]
        passed &= check("video.local.yaml content itself is unchanged",
                         abs(float(kept["ui_scale"]) - 4.2) < 1e-6)

        print(f"\n  {'PASS' if passed else 'FAIL'}: config-migration upgrade path"
              + ("" if passed else " — see failures above"))
        return 0 if passed else 1
    finally:
        if proc is not None:
            quit_engine(args.port, proc)
        restore_all(backups)


if __name__ == "__main__":
    sys.exit(main())
