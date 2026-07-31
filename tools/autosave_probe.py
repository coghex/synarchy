#!/usr/bin/env python3
"""Headless autosave probe (#913).

Gates the interval autosave feature end to end in ONE engine boot on an
isolated resource root. The world is entered through the REAL gameplay
path (`uiManager.showMenu("world_view")`), which builds and shows an
ordinary generated page -- never an arena, since a save containing an
arena page hangs the world thread on load (known issue #365).

What it proves, in order:

  1. DEFAULT OFF. With `enabled: false` and a one-minute configured
     interval, a bounded dwell LONGER than one interval produces neither
     an autosave request nor an autosave slot.
  2. THE INTERVAL FIRES, AND HANDS THE WORLD BACK. Enabled, one-minute
     interval: a real wall-clock wait produces a save, and an UNPAUSED
     world comes back unpaused at its exact prior fast-forward time
     scale.
  3. CLASSIFICATION IS DURABLE + VISIBLE. `engine.listSaves()` reports
     `autosave = true` for the slot the scheduler wrote and `false` for a
     manual save, and `scripts/save_browser.lua`'s own row-label function
     tags the autosave row.
  4. A PAUSED WORLD STAYS PAUSED. An autosave that began from a paused
     world leaves it paused and zero-scaled.
  5. THE PLAYER WINS. A NET-ZERO pause/resume pair during the request --
     the player's last intent being the same boolean the save started
     from, so a value comparison would see nothing -- still suppresses
     restoration: the world stays paused and zero-scaled instead of being
     handed back, against phase 2's untouched control.
  6. A FAILED AUTOSAVE STAYS PAUSED. An accepted autosave whose storage
     write fails leaves the engine paused and the visible world
     zero-scaled, and `engine.getSaveStatus()` exposes the rendered
     failure outcome carrying its `StoragePhase`.
  7. SKIPS ARE SILENT. A deadline reached outside a gameplay view creates
     no save request and no failure event.
  8. A PAUSE-CONFIGURED save_load EVENT WINS. With the `save_load`
     category configured to pause, a SUCCESSFUL autosave still ends
     paused (and zero-scaled).
  9. MANUAL SAVES ARE NEVER OVERWRITTEN. A pre-existing MANUAL save
     sitting on an `autosave-<n>` name fails the attempt with a
     `save_load` failure, overwrites nothing, and rotates nothing.
 10. ROTATION IS ORDERED AND OWNED. Repeated autosaves keep `autosave-1`
     newest, and only classified-autosave slots are ever replaced.
 11. RETENTION. Reducing `rotation_depth` and disabling autosave both
     RETAIN existing higher-numbered generations untouched.

Usage:
  python3 tools/autosave_probe.py [--port 9913] [--keep-root]

Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import shutil
import stat
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot, send, send_json, quit_engine, poll_until  # noqa: E402

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LOG = "/tmp/autosave_probe_engine.log"

# The probe drives the REAL gameplay entry path
# (uiManager.showMenu("world_view") -> worldView.show), which creates and
# shows its own `main_world` page. Its generation parameters are shrunk
# first so this stays a ~30-second worldgen rather than the 128-tile
# default. Deliberately a real world.init page, never an arena: a save
# containing an arena page hangs the world thread on load (#365).
WORLD_SIZE = 32
WORLD_SEED = 42
PLATES = 3

# Resolved from world.getActiveWorldId() once the view is up -- every
# page-scoped query below must address the page the pause/save paths
# actually operate on (the VISIBLE one), not a name this probe assumed.
PAGE = ""


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str, detail: str = "") -> bool:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}"
              + (f"  ({detail})" if detail else ""), flush=True)
        if not cond:
            self.failed += 1
        return bool(cond)


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: scripts/assets/data symlinked (read-only
    content, safe to share), config/ COPIED (this probe writes
    config/save.local.yaml into it and must never touch the developer's
    own settings), and its own empty saves/."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    config_dst = os.path.join(root, "config")
    if not os.path.exists(config_dst):
        shutil.copytree(os.path.join(REPO, "config"), config_dst)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def write_save_config(root: str, enabled: bool, interval: int, depth: int) -> None:
    with open(os.path.join(root, "config", "save.local.yaml"), "w") as f:
        f.write("save:\n"
                f"  enabled: {'true' if enabled else 'false'}\n"
                f"  interval_minutes: {interval}\n"
                f"  rotation_depth: {depth}\n")


def slot_dirs(root: str) -> list[str]:
    return sorted(os.path.basename(p) for p in
                  glob.glob(os.path.join(root, "saves", "*"))
                  if os.path.isdir(p))


def autosave_slots(root: str) -> list[str]:
    return sorted(n for n in slot_dirs(root) if n.startswith("autosave-"))


def dump(port: int) -> dict:
    return send_json(port, "return require('scripts.autosave').dump()")


def listing(port: int) -> dict[str, dict]:
    rows = send_json(port, "return engine.listSaves()") or []
    return {r["name"]: r for r in rows}


def reconfigure(port: int, enabled: bool, interval: int, depth: int) -> None:
    send(port, f"engine.setSaveConfig({{enabled = {str(enabled).lower()}, "
               f"intervalMinutes = {interval}, rotationDepth = {depth}}}); "
               f"require('scripts.autosave').reload(); return 'ok'")


def force_deadline(port: int) -> None:
    """Make the NEXT scheduler tick treat the interval as elapsed. The
    scheduler's own eligibility gate, rotation, and request path all still
    run exactly as they would on a real deadline -- only the waiting is
    skipped, which is what keeps this probe minutes rather than hours.

    Waits for the scheduler's world epoch to be established first: the
    tick that first sees a gameplay world starts a fresh interval, which
    would otherwise overwrite the deadline this just forced."""
    poll_until(30, lambda: dump(port).get("scheduledWorld") is not None)
    send(port, "local a = require('scripts.autosave'); "
               "a.nextDueAt = a.now() - 1; return 'ok'")


def wait_for_attempts(port: int, target: int, seconds: float) -> bool:
    return poll_until(seconds,
                      lambda: dump(port)["stats"]["attempts"] >= target) or False


def save_settled(port: int, seconds: float = 90.0) -> bool:
    """Wait until no save transaction is in flight (outcome present)."""
    def done():
        st = send_json(port, "return engine.getSaveStatus()")
        return bool(st) and st.get("outcome") is not None
    return poll_until(seconds, done) or False


def paused(port: int) -> bool:
    return send(port, "return tostring(engine.isPaused())") == "true"


def time_scale(port: int) -> float:
    return float(send(port, f"return world.getTimeScale('{PAGE}')"))


def set_time_scale(port: int, scale: float) -> None:
    send(port, f"world.setTimeScale('{PAGE}', {scale}); return 'ok'")


def event_log_text(port: int) -> str:
    rows = send_json(port, "return engine.getEventLog()") or []
    return "\n".join(str(r.get("text", "")) for r in rows)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9913)
    ap.add_argument("--keep-root", action="store_true",
                     help="leave the temporary resource root on disk")
    args = ap.parse_args()

    chk = Checks()
    base = tempfile.mkdtemp(prefix="autosave_probe_")
    root = make_isolated_root(base)
    saves_dir = os.path.join(root, "saves")
    proc = None
    try:
        # ---------------------------------------------------------
        # 1. Default OFF: a bounded dwell longer than one interval.
        # ---------------------------------------------------------
        print("1. shipped default is OFF -- a dwell past one interval saves nothing")
        write_save_config(root, enabled=False, interval=1, depth=3)
        proc = boot(args.port, log=LOG, args=["--resource-root", root],
                    ready_timeout=180)

        # The eligibility predicate the issue names is
        # uiManager.isGameplayView(), so the world must be entered the way
        # a player enters one -- showMenu("world_view") -> worldView.show,
        # which CREATES AND SHOWS its own page. A world.init of a separate
        # page would leave that page invisible, and every pause/save/
        # restore path operates on the VISIBLE page, so the probe would
        # then be measuring a page nothing under test ever touches.
        # worldParams shrinks the world worldView would otherwise build at
        # its 128/10 default. The full ui_manager boot never runs headless
        # (it gates on a GPU font atlas), but showMenu still sets the
        # authoritative currentMenu isGameplayView() reads -- the same
        # approach tools/action_outcome_layer_a_check.py already uses.
        global PAGE
        send(args.port,
             "local wv = require('scripts.world_view'); "
             f"wv.worldParams = {{seed = {WORLD_SEED}, "
             f"worldSize = {WORLD_SIZE}, plateCount = {PLATES}}}; "
             "require('scripts.ui_manager').showMenu('world_view'); return 'ok'")
        send(args.port, "return tostring(world.waitForInit(300))", timeout=320.0)
        chk.ok(poll_until(60, lambda: send(
                   args.port, "return tostring(world.getActiveWorldId())")
                   not in ("nil", "")) or False,
               "the gameplay world became active")
        PAGE = send(args.port, "return world.getActiveWorldId()")
        print(f"   active page: {PAGE}")
        chk.ok(send(args.port,
                    "return tostring(require('scripts.ui_manager').isGameplayView())")
               == "true", "engine is in a gameplay view")

        d = dump(args.port)
        chk.ok(d["config"]["enabled"] is False, "config reports autosave disabled")
        chk.ok(d.get("nextDueAt") is None, "no schedule exists while disabled")

        dwell = 75.0
        print(f"   dwelling {dwell:.0f}s (> one 1-minute interval)...")
        time.sleep(dwell)
        d = dump(args.port)
        chk.ok(d["stats"]["attempts"] == 0,
               "no autosave request during the dwell", str(d["stats"]))
        chk.ok(autosave_slots(root) == [],
               "no autosave slot created during the dwell",
               str(autosave_slots(root)))
        chk.ok(send(args.port, "return tostring(engine.getSaveStatus() == nil)")
               == "true", "no save transaction was ever begun")

        # ---------------------------------------------------------
        # 2. Enabled: a REAL interval fires and hands the world back.
        # ---------------------------------------------------------
        print("2. a real one-minute interval fires and restores the exact scale")
        reconfigure(args.port, enabled=True, interval=1, depth=3)
        send(args.port, "engine.setPaused(false); return 'ok'")
        set_time_scale(args.port, 7)
        chk.ok(poll_until(10, lambda: abs(time_scale(args.port) - 7.0) < 1e-6)
               or False, "fast-forward time scale 7 established")
        chk.ok(not paused(args.port), "world is unpaused before the interval")

        d = dump(args.port)
        chk.ok(d.get("secondsUntilDue") is not None
               and 0 < d["secondsUntilDue"] <= 60,
               "enabling autosave started a fresh full interval",
               str(d.get("secondsUntilDue")))

        print("   waiting up to 100s for the real interval deadline...")
        chk.ok(wait_for_attempts(args.port, 1, 100.0),
               "the interval produced an autosave request")
        chk.ok(save_settled(args.port), "the autosave transaction settled")
        chk.ok(poll_until(30, lambda: not paused(args.port)) or False,
               "a successful autosave from an UNPAUSED world resumes it")
        chk.ok(poll_until(30, lambda: abs(time_scale(args.port) - 7.0) < 1e-6)
               or False, "it restores the exact prior fast-forward scale",
               str(time_scale(args.port)))
        chk.ok("autosave-1" in autosave_slots(root),
               "the autosave landed in the reserved autosave-1 slot",
               str(autosave_slots(root)))

        # ---------------------------------------------------------
        # 3. Classification is durable and visible.
        # ---------------------------------------------------------
        print("3. the autosave/manual classification is durable and shown")
        rows = listing(args.port)
        chk.ok(rows.get("autosave-1", {}).get("autosave") is True,
               "engine.listSaves() reports autosave-1 as an autosave",
               json.dumps(rows.get("autosave-1")))
        send(args.port, f"engine.saveWorld('{PAGE}', 'manual_slot'); return 'ok'")
        chk.ok(save_settled(args.port), "the manual save settled")
        rows = listing(args.port)
        chk.ok(rows.get("manual_slot", {}).get("autosave") is False,
               "engine.listSaves() reports a manual save as manual",
               json.dumps(rows.get("manual_slot")))
        label = send(args.port,
                     "return require('scripts.save_browser').rowLabel("
                     "{name = 'autosave-1', autosave = true, timestamp = 't'})")
        chk.ok("Autosave" in label,
               "the save browser labels an autosave row", label)
        label = send(args.port,
                     "return require('scripts.save_browser').rowLabel("
                     "{name = 'manual_slot', autosave = false, timestamp = 't'})")
        chk.ok("Autosave" not in label,
               "the save browser does NOT label a manual row", label)

        # ---------------------------------------------------------
        # 4. A world that began paused stays paused and zero-scaled.
        # ---------------------------------------------------------
        print("4. an autosave from a PAUSED world leaves it paused + zero-scaled")
        send(args.port, "require('scripts.pause').set(true); return 'ok'")
        chk.ok(paused(args.port), "world paused before the autosave")
        before = dump(args.port)["stats"]["attempts"]
        force_deadline(args.port)
        chk.ok(wait_for_attempts(args.port, before + 1, 30.0),
               "the forced deadline produced an autosave request")
        chk.ok(save_settled(args.port), "the autosave transaction settled")
        chk.ok(paused(args.port), "still paused after a successful autosave")
        chk.ok(abs(time_scale(args.port)) < 1e-6,
               "visible world stays zero-scaled", str(time_scale(args.port)))

        # ---------------------------------------------------------
        # 5. A player transition during the request suppresses restore.
        # ---------------------------------------------------------
        print("5. a pause transition during the request suppresses restoration")
        send(args.port, "engine.setPaused(false); return 'ok'")
        set_time_scale(args.port, 5)
        chk.ok(poll_until(10, lambda: abs(time_scale(args.port) - 5.0) < 1e-6)
               or False, "pre-save scale 5 established")
        chk.ok(not paused(args.port), "pre-save pause boolean is FALSE")
        before = dump(args.port)["stats"]["attempts"]
        # One chunk, so the toggles land inside the transaction's own
        # window: engine.saveWorld returns as soon as the WorldSave
        # command is queued, while the world thread's capture/encode/write
        # is still ahead of it. The returned flag reports whether the
        # transaction really was still in flight -- a probe that toggled
        # AFTER it finished would prove nothing.
        #
        # The toggles are a NET-ZERO pair: the player's LAST pause intent
        # (false) is the same boolean the save started from, so an
        # implementation that merely compared booleans would see nothing
        # to stop it and would hand the world back. Only a generation
        # notices two transitions that cancel out.
        still_running = send(
            args.port,
            "local a = require('scripts.autosave'); "
            f"a.performSave('{PAGE}'); "
            "engine.setPaused(true); engine.setPaused(false); "
            "local s = engine.getSaveStatus(); "
            "return tostring(s ~= nil and s.outcome == nil)", timeout=30.0)
        chk.ok(still_running == "true",
               "the pause toggles landed while the save was still in flight",
               still_running)
        chk.ok(dump(args.port)["stats"]["attempts"] == before + 1,
               "the autosave request was made")
        chk.ok(save_settled(args.port), "the autosave transaction settled")
        # The control for this pair is phase 2, whose identical but
        # untouched autosave ended UNPAUSED at its exact prior scale.
        chk.ok(paused(args.port),
               "restoration was suppressed: the save's own pause still stands")
        chk.ok(abs(time_scale(args.port)) < 1e-6,
               "and the prior time scale was NOT handed back",
               str(time_scale(args.port)))

        # ---------------------------------------------------------
        # 6. An accepted autosave that FAILS stays paused + zero-scaled.
        # ---------------------------------------------------------
        print("6. an accepted autosave that fails keeps the safety ratchet")
        for name in autosave_slots(root):
            shutil.rmtree(os.path.join(saves_dir, name))
        send(args.port, "engine.setPaused(false); return 'ok'")
        set_time_scale(args.port, 5)
        poll_until(10, lambda: abs(time_scale(args.port) - 5.0) < 1e-6)
        # A read-only saves/ makes publishGeneration fail at
        # PhaseDirectoryCreate. Rotation itself is a no-op here (no slot
        # in range exists), so this is a genuine SAVE failure, not a
        # rotation refusal.
        os.chmod(saves_dir, stat.S_IRUSR | stat.S_IXUSR)
        try:
            before = dump(args.port)["stats"]["attempts"]
            force_deadline(args.port)
            chk.ok(wait_for_attempts(args.port, before + 1, 30.0),
                   "the failing autosave was still ACCEPTED as a request")
            chk.ok(save_settled(args.port), "the failed transaction settled")
            status = send_json(args.port, "return engine.getSaveStatus()") or {}
            outcome = str(status.get("outcome", ""))
            chk.ok("SaveAborted" in outcome,
                   "getSaveStatus() reports the aborted outcome", outcome)
            chk.ok("save storage failed" in outcome and "Phase" in outcome,
                   "the rendered outcome carries its StoragePhase", outcome)
            chk.ok(paused(args.port), "a failed autosave leaves the engine paused")
            chk.ok(abs(time_scale(args.port)) < 1e-6,
                   "a failed autosave leaves the world zero-scaled",
                   str(time_scale(args.port)))
        finally:
            os.chmod(saves_dir, stat.S_IRWXU)

        # ---------------------------------------------------------
        # 7. A skip is silent: no request, no failure event.
        # ---------------------------------------------------------
        print("7. a deadline outside a gameplay view skips silently")
        send(args.port,
             "require('scripts.ui_manager').showMenu('main_menu'); return 'ok'")
        chk.ok(send(args.port,
                    "return tostring(require('scripts.ui_manager').isGameplayView())")
               == "false", "no longer in a gameplay view")
        before = dump(args.port)
        before_log = event_log_text(args.port)
        force_deadline(args.port)
        chk.ok(poll_until(30, lambda: dump(args.port)["stats"]["skips"]
                          > before["stats"]["skips"]) or False,
               "the deadline was consumed as a skip")
        d = dump(args.port)
        chk.ok(d["stats"]["attempts"] == before["stats"]["attempts"],
               "a skip creates no save request")
        chk.ok(d["stats"]["failures"] == before["stats"]["failures"],
               "a skip is not counted as a failure")
        new_events = event_log_text(args.port)[len(before_log):]
        chk.ok("Autosave failed" not in new_events,
               "a skip emits no failure notification", new_events.strip()[:200])
        chk.ok(d.get("secondsUntilDue") is not None
               and 0 < d["secondsUntilDue"] <= 60,
               "the cadence continued rather than being suspended",
               str(d.get("secondsUntilDue")))
        send(args.port,
             "require('scripts.ui_manager').showMenu('world_view'); return 'ok'")

        # ---------------------------------------------------------
        # 8. A pause-configured save_load event stays authoritative.
        # ---------------------------------------------------------
        print("8. a pause-configured save_load event survives the restoration")
        send(args.port,
             "engine.setNotificationOverrides({save_load = {pause = true}}); "
             "return 'ok'")
        send(args.port, "engine.setPaused(false); return 'ok'")
        set_time_scale(args.port, 5)
        poll_until(10, lambda: abs(time_scale(args.port) - 5.0) < 1e-6)
        before = dump(args.port)["stats"]["attempts"]
        force_deadline(args.port)
        chk.ok(wait_for_attempts(args.port, before + 1, 30.0),
               "the autosave request was made")
        chk.ok(save_settled(args.port), "the autosave transaction settled")
        chk.ok(poll_until(30, lambda: paused(args.port)) or False,
               "the pause-configured success event wins over restoration")
        chk.ok(poll_until(30, lambda: abs(time_scale(args.port)) < 1e-6) or False,
               "and the resulting world is zero-scaled, not half-paused",
               str(time_scale(args.port)))
        send(args.port,
             "engine.setNotificationOverrides({save_load = {pause = false}}); "
             "return 'ok'")

        # ---------------------------------------------------------
        # 9. A manual save on an autosave-<n> name is never overwritten.
        # ---------------------------------------------------------
        print("9. a manual save squatting on an autosave name blocks the attempt")
        send(args.port, f"engine.saveWorld('{PAGE}', 'autosave-2'); return 'ok'")
        chk.ok(save_settled(args.port), "the manual autosave-2 save settled")
        rows = listing(args.port)
        chk.ok(rows.get("autosave-2", {}).get("autosave") is False,
               "the squatting save is classified MANUAL",
               json.dumps(rows.get("autosave-2")))
        manual_stamp = rows["autosave-2"]["timestamp"]
        before_slots = autosave_slots(root)
        before = dump(args.port)
        before_log = event_log_text(args.port)
        force_deadline(args.port)
        chk.ok(poll_until(30, lambda: dump(args.port)["stats"]["failures"]
                          > before["stats"]["failures"]) or False,
               "the collision is reported as an autosave FAILURE")
        chk.ok(dump(args.port)["stats"]["attempts"] == before["stats"]["attempts"],
               "no save request was made")
        new_events = event_log_text(args.port)[len(before_log):]
        chk.ok("Autosave failed" in new_events,
               "the failure reaches the save_load notification category",
               new_events.strip()[:200])
        rows = listing(args.port)
        chk.ok(rows.get("autosave-2", {}).get("autosave") is False
               and rows["autosave-2"]["timestamp"] == manual_stamp,
               "the manual save was not overwritten")
        chk.ok(autosave_slots(root) == before_slots,
               "nothing was partially rotated",
               f"{before_slots} -> {autosave_slots(root)}")

        # ---------------------------------------------------------
        # 10. Normal rotation: autosave-1 stays newest, owned slots only.
        # ---------------------------------------------------------
        print("10. rotation keeps autosave-1 newest and replaces only owned slots")
        shutil.rmtree(os.path.join(saves_dir, "autosave-2"))
        for i in range(3):
            before = dump(args.port)["stats"]["attempts"]
            force_deadline(args.port)
            chk.ok(wait_for_attempts(args.port, before + 1, 30.0),
                   f"autosave {i + 1} requested")
            chk.ok(save_settled(args.port), f"autosave {i + 1} settled")
        rows = listing(args.port)
        chk.ok(autosave_slots(root) == ["autosave-1", "autosave-2", "autosave-3"],
               "the family filled to the configured depth",
               str(autosave_slots(root)))
        chk.ok(all(rows.get(n, {}).get("autosave") is True
                   for n in ("autosave-1", "autosave-2", "autosave-3")),
               "every rotated generation is still classified autosave")
        stamps = [rows[n]["timestamp"] for n in
                  ("autosave-1", "autosave-2", "autosave-3")]
        chk.ok(stamps == sorted(stamps, reverse=True),
               "autosave-1 is newest and higher numbers are older", str(stamps))
        chk.ok("manual_slot" in slot_dirs(root),
               "the unrelated manual save is untouched by rotation")

        # ---------------------------------------------------------
        # 11. Retention on depth reduction and on disable.
        # ---------------------------------------------------------
        print("11. reducing the depth and disabling both RETAIN excess slots")
        retained_stamp = listing(args.port)["autosave-3"]["timestamp"]
        reconfigure(args.port, enabled=True, interval=1, depth=2)
        before = dump(args.port)["stats"]["attempts"]
        force_deadline(args.port)
        chk.ok(wait_for_attempts(args.port, before + 1, 30.0),
               "an autosave ran at the reduced depth")
        chk.ok(save_settled(args.port), "it settled")
        rows = listing(args.port)
        chk.ok("autosave-3" in autosave_slots(root),
               "the now-excess autosave-3 is retained",
               str(autosave_slots(root)))
        chk.ok(rows["autosave-3"]["timestamp"] == retained_stamp,
               "and is left completely untouched -- never selected, never rewritten")

        reconfigure(args.port, enabled=False, interval=1, depth=2)
        chk.ok(dump(args.port).get("nextDueAt") is None,
               "disabling clears the active schedule")
        slots_at_disable = autosave_slots(root)
        time.sleep(5)
        chk.ok(autosave_slots(root) == slots_at_disable,
               "disabling retains every existing autosave generation",
               str(autosave_slots(root)))

        print(f"\n  {'PASS' if chk.failed == 0 else 'FAIL'}: autosave (#913)"
              + ("" if chk.failed == 0 else f" -- {chk.failed} failure(s)"))
        return 0 if chk.failed == 0 else 1
    finally:
        if proc is not None:
            quit_engine(args.port, proc)
        if os.path.isdir(saves_dir):
            os.chmod(saves_dir, stat.S_IRWXU)
        if args.keep_root:
            print(f"  (resource root kept at {root})")
        else:
            shutil.rmtree(base, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
