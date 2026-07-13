#!/usr/bin/env python3
"""Headless config-state probe (#638, paths updated for #786).

Verifies the boundary between versioned config *templates*
(`config/*_default.yaml`, tracked) and local runtime config *state*
(`config/video.local.yaml`, `config/keybinds.local.yaml`,
`config/notifications.local.yaml`, gitignored) that the settings UI's
Save actions write:

  1. `git status` for `config/` + `.gitignore` starts clean.
  2. With no local config files AND no legacy pre-#661 config files
     present (simulating a fresh clone), the engine boots and falls
     back to the versioned `_default.yaml` templates (video/keybinds)
     or materializes from `data/notification_categories.yaml`
     (notifications) — same effective defaults either way.
  3. The public save paths (`engine.saveVideoConfig`, `engine.saveKeybinds`,
     `engine.setNotificationOverrides`) write the expected `*.local.yaml`
     files, not the legacy paths.
  4. None of that dirties git: `git status` for `config/` + `.gitignore`
     is still clean afterward.

Legacy-file migration itself (the #786 upgrade path) is
`tools/config_migration_probe.py`'s job, not this one's.

Any local/legacy config files present before the probe runs are backed
up and restored afterward, so a developer's own settings survive a run.

Usage:
  python3 tools/config_state_probe.py [--port 9165]

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

LOG = "/tmp/config_state_probe_engine.log"

LOCAL_FILES = [
    "config/video.local.yaml",
    "config/keybinds.local.yaml",
    "config/notifications.local.yaml",
]

# Pre-#661 paths. Nothing writes to these any more, but a probe run (or
# a developer's own machine) could still have one lying around from an
# old checkout — #786's migrateLegacyConfig would treat that as legacy
# state to import, so a "simulated fresh clone" must hide these too.
LEGACY_FILES = [
    "config/video.yaml",
    "config/keybinds.yaml",
    "config/notifications.yaml",
]


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f"  ({detail})" if detail else ""))
    return ok


def git_status(paths: list[str]) -> str:
    r = subprocess.run(["git", "status", "--short", "--"] + paths,
                        capture_output=True, text=True, check=True)
    return r.stdout


BACKUP_DIR = "/tmp/config_state_probe_backup"


def backup_local_files() -> dict[str, str]:
    """Move aside any existing local/legacy config files (simulate a
    fresh clone) and return a map of original path -> backup path to
    restore.

    Backups live under /tmp, not alongside the originals — a sibling
    `config/video.yaml.bak` would itself show up as untracked in the
    `git status -- config` checks this probe runs, contaminating them."""
    backups = {}
    os.makedirs(BACKUP_DIR, exist_ok=True)
    for p in LOCAL_FILES + LEGACY_FILES:
        if os.path.exists(p):
            bak = os.path.join(BACKUP_DIR, os.path.basename(p))
            shutil.move(p, bak)
            backups[p] = bak
    return backups


def restore_local_files(backups: dict[str, str]) -> None:
    # LOCAL_FILES are always cleared: gitignored, so any left over from
    # this run (e.g. a save action's output) is just cruft. LEGACY_FILES
    # are only touched here if still pending in `backups` (i.e.
    # `restore_legacy_files` never ran, e.g. an early exception) —
    # otherwise they're already correctly back in their tracked state
    # and must NOT be removed again.
    for p in LOCAL_FILES:
        if os.path.exists(p):
            os.remove(p)
    for p in LEGACY_FILES:
        if p in backups and os.path.exists(p):
            os.remove(p)
    for p, bak in backups.items():
        shutil.move(bak, p)


def restore_legacy_files(backups: dict[str, str]) -> None:
    """Restore just the tracked legacy paths from `backups`, popping them
    so the final `restore_local_files` doesn't try to move them again.

    Legacy files are TRACKED (#786) — unlike the gitignored local paths,
    leaving them hidden until the very end would make the mid-run
    git-status post-check see them as deleted. They must be back on
    disk well before that check runs, even though local-file cleanup
    can still wait until the very end (git never sees those either way)."""
    for p in LEGACY_FILES:
        bak = backups.pop(p, None)
        if bak is not None:
            shutil.move(bak, p)


def load_yaml(path: str):
    with open(path) as f:
        return yaml.safe_load(f)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9165)
    args = ap.parse_args()

    print("0. pre-check: clean git status for config/ + .gitignore")
    pre = git_status(["config", ".gitignore"])
    passed = check("clean before the probe runs", pre == "", pre.strip())
    if not passed:
        print("  Aborting: fix the tracked config state before running this probe.")
        return 1

    backups = backup_local_files()
    proc = None
    try:
        for p in LOCAL_FILES + LEGACY_FILES:
            passed &= check(f"{p} absent pre-boot (simulated fresh clone)",
                             not os.path.exists(p))

        proc = boot(args.port, log=LOG)

        print("1. fresh-clone boot falls back to versioned templates")
        r = send(args.port,
                  "local w,h,wm,uis,vs = engine.getVideoConfig(); "
                  "return w..'|'..h..'|'..wm..'|'..uis..'|'..tostring(vs)")
        want = load_yaml("config/video_default.yaml")["video"]
        parts = r.split("|")
        ok = (len(parts) == 5 and int(parts[0]) == want["resolution"]["width"]
              and int(parts[1]) == want["resolution"]["height"]
              and parts[2] == want["window_mode"]
              and abs(float(parts[3]) - float(want["ui_scale"])) < 1e-6
              and parts[4] == str(bool(want["vsync"])).lower())
        passed &= check("video config == config/video_default.yaml", ok, r)

        r = send(args.port,
                  "local b = engine.getKeybinds(); return table.concat(b.moveUp, ',')")
        want_kb = load_yaml("config/keybinds_default.yaml")["keybinds"]
        passed &= check("keybinds == config/keybinds_default.yaml",
                         r == ",".join(want_kb["moveUp"]), r)

        passed &= check("config/notifications.local.yaml materialized on boot",
                         os.path.exists("config/notifications.local.yaml"))
        r = send(args.port,
                  "local cfg = engine.getNotificationCfg(); "
                  "for _,c in ipairs(cfg) do if c.id == 'debug' then "
                  "return tostring(c.log) end end; return 'NOTFOUND'")
        passed &= check("notification defaults come from the registry",
                         r == "false", r)
        # Pinned deliberately (#638 PR review): the pre-#638 tracked
        # config/notifications.yaml had building.popup=true and
        # unit_warning.pause=true, which do NOT match
        # data/notification_categories.yaml's registry defaults below.
        # That mismatch was itself accidental local-preference drift —
        # the exact problem #638 removes — not an intentional default,
        # so a fresh clone deliberately gets the clean registry values,
        # not the old drifted ones.
        r = send(args.port,
                  "local cfg = engine.getNotificationCfg(); "
                  "for _,c in ipairs(cfg) do if c.id == 'building' then "
                  "return tostring(c.popup) end end; return 'NOTFOUND'")
        passed &= check("building.popup comes from the registry (not the old drifted file)",
                         r == "false", r)
        r = send(args.port,
                  "local cfg = engine.getNotificationCfg(); "
                  "for _,c in ipairs(cfg) do if c.id == 'unit_warning' then "
                  "return tostring(c.pause) end end; return 'NOTFOUND'")
        passed &= check("unit_warning.pause comes from the registry (not the old drifted file)",
                         r == "false", r)

        for p in ("config/video.local.yaml", "config/keybinds.local.yaml"):
            passed &= check(f"{p} not written just by loading", not os.path.exists(p))
        for p in LEGACY_FILES:
            passed &= check(f"legacy {p} not resurrected by a fresh-clone boot",
                             not os.path.exists(p))

        # The legacy paths are TRACKED (#786) — restore them on disk now,
        # well before the mid-run git-status post-check, rather than
        # leaving them hidden until the final `finally`. This doesn't
        # disturb the already-running engine (config was already loaded
        # into memory at boot); the save paths below write from that
        # in-memory state, not by re-reading these files. Content is
        # captured so phase 2 can prove the save paths never touch them,
        # without relying on "does not exist" (no longer true once
        # they're back on disk).
        restore_legacy_files(backups)
        legacy_before = {p: open(p).read() for p in LEGACY_FILES}

        print("2. exercise the public save paths")
        send(args.port, "engine.setUIScale(1.23); return 'ok'")
        send(args.port, "engine.saveVideoConfig(); return 'ok'")
        passed &= check("config/video.local.yaml written",
                         os.path.exists("config/video.local.yaml"))
        passed &= check("legacy config/video.yaml NOT modified by save",
                         open("config/video.yaml").read() == legacy_before["config/video.yaml"])
        if os.path.exists("config/video.local.yaml"):
            saved = load_yaml("config/video.local.yaml")["video"]
            passed &= check("saved video config has the new ui_scale",
                             abs(float(saved["ui_scale"]) - 1.23) < 1e-6,
                             str(saved.get("ui_scale")))

        send(args.port, "engine.setActionKeys('moveUp', {'I','K'}); return 'ok'")
        send(args.port, "engine.saveKeybinds(); return 'ok'")
        passed &= check("config/keybinds.local.yaml written",
                         os.path.exists("config/keybinds.local.yaml"))
        passed &= check("legacy config/keybinds.yaml NOT modified by save",
                         open("config/keybinds.yaml").read() == legacy_before["config/keybinds.yaml"])
        if os.path.exists("config/keybinds.local.yaml"):
            saved_kb = load_yaml("config/keybinds.local.yaml")["keybinds"]
            passed &= check("saved keybinds have the new moveUp",
                             saved_kb.get("moveUp") == ["I", "K"],
                             str(saved_kb.get("moveUp")))

        r = send(args.port, "return tostring(engine.setNotificationOverrides({debug={log=true}}))")
        passed &= check("setNotificationOverrides accepted", r == "true", r)
        passed &= check("legacy config/notifications.yaml NOT modified by save",
                         open("config/notifications.yaml").read()
                             == legacy_before["config/notifications.yaml"])
        if os.path.exists("config/notifications.local.yaml"):
            saved_notif = load_yaml("config/notifications.local.yaml")["categories"]
            passed &= check("saved notification override took effect",
                             saved_notif.get("debug", {}).get("log") is True,
                             str(saved_notif.get("debug")))

        quit_engine(args.port, proc)
        proc = None

        print("3. post-check: saving local config state did not dirty git")
        post = git_status(["config", ".gitignore"])
        passed &= check("clean after save round-trip", post == "", post.strip())

        print(f"\n  {'PASS' if passed else 'FAIL'}: config-state boundary"
              + ("" if passed else " — see failures above"))
        return 0 if passed else 1
    finally:
        if proc is not None:
            quit_engine(args.port, proc)
        restore_local_files(backups)


if __name__ == "__main__":
    sys.exit(main())
