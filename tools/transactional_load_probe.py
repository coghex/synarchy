#!/usr/bin/env python3
"""Headless whole-session LOAD TRANSACTION probe (issue #763, save-overhaul
C2 — the C2-specific acceptance gate).

engine.loadSave used to mutate the live session incrementally, page by
page, merging a save's pages into whatever else was already live
(#214/#218/#191) and remapping the active page onto a hardcoded
"main_world". Issue #763 replaces that with one coordinated, staged
transaction: decode/validate/stage the ENTIRE replacement session without
touching any live ref, then publish it atomically — the old session stays
completely valid and paused until publish commits, and publish REPLACES
the whole session rather than merging into it. This probe drives that
transaction end to end over the real debug console (the round trip spans
the Lua thread, the world thread, and the save barrier, so it can't be an
hspec test — see tools/README.md's rationale for the sibling save-side
probes this mirrors).

Companion probes: tools/multiworld_save_probe.py already gates the
gold-standard save → quit → fresh-restart → load happy path (including
saved-page-id preservation and identity mapping) and the arena special
case; tools/save_barrier_probe.py / tools/save_pause_probe.py /
tools/lua_orphan_prune_probe.py gate the barrier reuse, pause semantics,
and Lua reconcile this issue builds on. This probe is deliberately
narrower and focuses on what's NEW here:

  1. Several deliberately invalid loads (missing save, corrupt save,
     missing content definition) each leave the current session
     completely unchanged AND paused (requirements 3/15), and report a
     terminal LoadFailed via engine.getLoadStatus().
  2. Mutual exclusion (requirement 1): a save request while a load is in
     flight, and a second load request while one is already in flight,
     are both rejected with a clear result — never silently queued or
     merged.
  3. A successful load REPLACES the complete session rather than merging:
     a page that existed only in the pre-load session (never part of the
     save) does not survive publication.
  4. Haskell and Lua state agree immediately after publication (the
     loaded units are visible to both unit.exists and the Lua AI
     reconcile in the same observation).
  5. A paused dwell after publish advances no gameplay state, and
     unpausing lands on the default time scale (never a stale pre-save
     speed).
  6. Repeated loads in the same session do not accumulate ghost pages —
     loading twice leaves exactly the second load's pages live.
  7. Stale old-session work (requirement 12): commands fired for the
     replacement page's own id WHILE staging is still running have no
     effect on the published result — staging builds the new page
     entirely from the decoded save, never from any live ref. The
     narrower captureLocked-boundary case (a command still queued at the
     exact instant publication starts) isn't reproducible from a
     black-box TCP client — that's covered instead by the deterministic
     pure hspec case, Test.Headless.Load.Status's "captureLocked
     authorized-command discard" group.

Usage:
  python3 tools/transactional_load_probe.py [--port 9220] [--seed 42]

Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import glob
import os
import shutil
import sys
import time
import uuid
from probelib import boot, quit_engine, send, send_json, wait_load_published

SAVE_PREFIX = "tload_probe_"


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def bootstrap_defs(port: int, include_units: bool = True) -> None:
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]
    if include_units:
        loaders.append(("data/units/*.yaml", "engine.loadUnitYaml"))
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    for script, dt in [("unit_stats", 0.1), ("unit_resources", 0.2),
                       ("unit_ai", 0.1)]:
        send(port, f"engine.loadScript('scripts/{script}.lua', {dt}); return 'ok'")


def as_int(s: str):
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def page_exists(port: int, page: str) -> bool:
    """A page's registration oracle: world.getDate returns nil for a page
    that isn't (or is no longer) registered."""
    r = send(port, f"return world.getDate('{page}')")
    return r not in ("nil", "null", "")


def wait_active(port: int, page: str, secs: float = 15.0) -> bool:
    deadline = time.time() + secs
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == page:
            return True
        time.sleep(0.2)
    return False


def find_flat_strip(port: int):
    lua = (
        "local function f() for gy=-8,8 do for gx=-8,6 do "
        "local za=world.getTerrainAt(gx,gy) local zb=world.getTerrainAt(gx+1,gy) "
        "local fa=world.getFluidAt(gx,gy) local fb=world.getFluidAt(gx+1,gy) "
        "if za and zb and za==zb and not fa and not fb "
        "then return gx..','..gy..','..za end end end return 'none' end return f()"
    )
    for _ in range(10):
        res = send(port, lua).strip('"')
        if res and res != "none" and res.count(",") == 2:
            gx, gy, z = (int(v) for v in res.split(","))
            return gx, gy, z
        time.sleep(0.5)
    return None


def populate_page(port: int, page: str, seed: int, size: int = 64,
                  plates: int = 3) -> tuple[int, int]:
    send(port, f"world.init('{page}', {seed}, {size}, {plates}); return 'ok'")
    send(port, "return world.waitForInit(180)", timeout=190)
    send(port, f"world.show('{page}'); return 'ok'")
    if not wait_active(port, page):
        sys.exit(f"FAIL: {page} never became active")
    send(port, "return world.loadChunksInRegion(-2,-2,2,2)")
    send(port, "return world.waitForChunks(120)", timeout=125)
    strip = find_flat_strip(port)
    if not strip:
        sys.exit(f"FAIL: no flat dry ground found on {page}")
    gx, gy, z = strip
    uid = as_int(send(port, f"return unit.spawn('acolyte', {gx}, {gy}, {z}, 'player')"))
    bid = as_int(send(port, f"return building.spawn('cargo_hold_S', {gx + 2}, {gy})"))
    if uid is None or uid < 0:
        sys.exit(f"FAIL: unit.spawn rejected on {page}")
    if bid is None or bid < 0:
        sys.exit(f"FAIL: building.spawn rejected on {page}")
    return uid, bid


def do_save(port: int, page: str, slot: str) -> str:
    save_file = os.path.join("saves", slot, "world.synworld")
    saved = send(port, f"return engine.saveWorld('{page}', '{slot}')")
    if saved.strip() != "true":
        sys.exit(f"FAIL: engine.saveWorld returned {saved!r}")
    for _ in range(100):
        if os.path.exists(save_file):
            break
        time.sleep(0.1)
    if not os.path.exists(save_file):
        sys.exit(f"FAIL: save file never appeared at {save_file}")
    return save_file


def load_status(port: int):
    return send_json(port, "return engine.getLoadStatus()")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9220)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--seed2", type=int, default=7)
    ap.add_argument("--size", type=int, default=64)
    args = ap.parse_args()

    run_id = uuid.uuid4().hex[:12]
    slot_valid = f"{SAVE_PREFIX}valid_{run_id}"
    slot_corrupt = f"{SAVE_PREFIX}corrupt_{run_id}"
    slot_valid2 = f"{SAVE_PREFIX}valid2_{run_id}"
    save_dirs = [os.path.join("saves", s)
                 for s in (slot_valid, slot_corrupt, slot_valid2)]
    for d in save_dirs:
        if os.path.exists(d):
            sys.exit(f"refusing to run: {d} already exists")

    log = f"/tmp/transactional_load_probe_{args.port}.log"
    proc = None
    chk = Checks()

    try:
        proc = boot(args.port, log=log, label="engine")
        bootstrap_defs(args.port)

        # ── Build the baseline session: two real pages, real entities ──
        print("--- building baseline session ---")
        u_alpha, b_alpha = populate_page(args.port, "alpha", args.seed, args.size)
        u_beta, b_beta = populate_page(args.port, "beta", args.seed2, args.size)
        send(args.port, "world.hide('beta'); return 'ok'")
        send(args.port, "world.show('alpha'); return 'ok'")
        wait_active(args.port, "alpha")
        for _ in range(40):
            if (send(args.port, f"return unit.exists({u_alpha})") == "true" and
                    send(args.port, f"return unit.exists({u_beta})") == "true"):
                break
            time.sleep(0.1)
        baseline_date = send(args.port, "return world.getDate('alpha')")
        print(f"baseline: alpha unit=#{u_alpha} beta unit=#{u_beta} "
              f"date={baseline_date}")

        # "beta" stays LIVE (just hidden) through the save below, so it IS
        # captured by engine.saveWorld (which snapshots every live page,
        # not only the requested one) — it's the "survives because it
        # WAS in the save" half of the replacement-vs-merge check.
        do_save(args.port, "alpha", slot_valid)
        print(f"saved valid session -> {slot_valid} (alpha + beta)")

        # "gamma" is created AFTER the save above, so it is live at load
        # time but was never captured by ANY save file — the "does NOT
        # survive because it was never in the save" half of the
        # replacement-vs-merge check (requirement 8: no unrelated-page
        # collision resolution needed, because nothing outside the save
        # survives at all).
        u_gamma, _b_gamma = populate_page(args.port, "gamma", args.seed + 1000, 8)
        # world.show does not promote an ALREADY-visible page to the head
        # of the visible stack (documented world-thread quirk — see
        # tools/multiworld_save_probe.py's docstring); hide gamma
        # explicitly to make alpha active again.
        send(args.port, "world.hide('gamma'); return 'ok'")
        if not wait_active(args.port, "alpha"):
            sys.exit("FAIL: alpha never became active again after gamma setup")

        # A corrupt save: a real slot directory whose world.synworld is
        # garbage bytes, not a valid envelope at all.
        os.makedirs(os.path.join("saves", slot_corrupt), exist_ok=True)
        with open(os.path.join("saves", slot_corrupt, "world.synworld"), "wb") as f:
            f.write(b"not a real save file, deliberately corrupt for #763")

        # ── 1. Deliberately invalid loads leave the session untouched ──
        print("\n--- invalid loads leave the current session unchanged ---")

        def assert_untouched(label: str) -> None:
            chk.ok(send(args.port, "return world.getActiveWorldId()").strip('"')
                   == "alpha", f"{label}: active page is still alpha")
            chk.ok(send(args.port, f"return unit.exists({u_alpha})") == "true",
                   f"{label}: alpha's unit still exists")
            chk.ok(send(args.port, f"return unit.exists({u_beta})") == "true",
                   f"{label}: beta's unit still exists (not merged away)")
            chk.ok(send(args.port, "return world.getDate('alpha')")
                   == baseline_date,
                   f"{label}: alpha's date is unchanged")
            # Requirement 3: a load request pauses synchronously at
            # acceptance and a FAILED load never restores the pre-request
            # pause state — every attempt below leaves the session paused.
            chk.ok(send(args.port, "return engine.isPaused()") == "true",
                   f"{label}: session left paused after the failed attempt")

        loaded = send(args.port, "return engine.loadSave('no_such_save_xyz')")
        chk.ok(loaded.strip() == "false", "missing save: engine.loadSave returns false")
        status = load_status(args.port)
        chk.ok(isinstance(status, dict) and status.get("phase") == "LoadFailed",
               f"missing save: getLoadStatus reports LoadFailed ({status})")
        assert_untouched("missing save")
        send(args.port, "require('scripts.pause').set(false); return 'ok'", expect_result=False)

        loaded = send(args.port, f"return engine.loadSave('{slot_corrupt}')")
        chk.ok(loaded.strip() == "false", "corrupt save: engine.loadSave returns false")
        status = load_status(args.port)
        chk.ok(isinstance(status, dict) and status.get("phase") == "LoadFailed",
               f"corrupt save: getLoadStatus reports LoadFailed ({status})")
        assert_untouched("corrupt save")
        send(args.port, "require('scripts.pause').set(false); return 'ok'", expect_result=False)

        # ── 2. Mutual exclusion: save/load never overlap ────────────────
        print("\n--- mutual exclusion (requirement 1) ---")
        loaded = send(args.port, f"return engine.loadSave('{slot_valid}')")
        chk.ok(loaded.strip() == "true", "valid load: engine.loadSave accepted")

        # Inject stale old-session work (requirement 12): staging builds
        # the replacement page entirely from the decoded save file, never
        # from any live ref, so old-session commands for the SAME page id
        # ("alpha") processed normally (unlocked) WHILE staging is still
        # running must have no effect on the published result — this is
        # deterministic (staging is measurably slow: real chunk gen, not
        # a sub-tick race) unlike the captureLocked boundary itself,
        # which is covered instead by the always-reproducible pure
        # hspec case (Test.Headless.Load.Status's "captureLocked
        # authorized-command discard" group, World.Thread.partitionAuthorized)
        # — a live black-box TCP round trip can't reliably land a call
        # inside a window that narrow.
        poison_year = 9999
        for _ in range(5):
            send(args.port, f"world.setDate('alpha', {poison_year}, 12, 31); "
                             "return 'ok'", expect_result=False, timeout=2)
            time.sleep(0.2)

        # Race the mutual-exclusion window: fire these immediately, before
        # the async staging transaction can possibly have published yet.
        save_during_load = send(args.port,
            f"return engine.saveWorld('alpha', '{SAVE_PREFIX}race_{run_id}')")
        second_load = send(args.port, f"return engine.loadSave('{slot_valid}')")
        mid_status = load_status(args.port)
        still_inflight = (isinstance(mid_status, dict)
                          and mid_status.get("phase") not in
                          ("LoadPublished", "LoadFailed"))
        if still_inflight:
            chk.ok(save_during_load.strip() == "false",
                   "engine.saveWorld rejected while a load is in flight")
            chk.ok(second_load.strip() == "false",
                   "a second engine.loadSave rejected while one is in flight")
        else:
            # The race window closed before either racing call landed
            # (a very fast machine/small world) — the mutual-exclusion
            # CODE PATH is still covered by the two checks above whenever
            # it wins the race; when it doesn't, print rather than fail so
            # this probe doesn't flake on timing alone.
            print("  [SKIP] mutual-exclusion race window closed before the "
                  "racing calls landed (load already terminal) — timing-"
                  "dependent, not a failure")

        # ── 3. Wait for the (first) valid load to publish ───────────────
        published, status = wait_load_published(args.port, 180)
        chk.ok(published, f"valid load: transaction publishes ({status})")
        if not published:
            print("FAIL: cannot continue without a published load", file=sys.stderr)
            return 1

        post_date = send(args.port, "return world.getDate('alpha')")
        chk.ok(f'"year":{poison_year}' not in post_date,
               f"stale world.setDate('alpha', {poison_year}, ...) fired "
               f"during staging did not survive into the replacement "
               f"session (got {post_date})")

        send(args.port, "return world.waitForInit(180)", timeout=190)
        time.sleep(1.5)

        # ── 4. Complete replacement, not a merge ─────────────────────────
        print("\n--- complete replacement (not a merge) ---")
        # "alpha" is the loaded save's own page id (never remapped).
        chk.ok(wait_active(args.port, "alpha"),
               "alpha (the saved page's own id) is active after publish")
        chk.ok(send(args.port, f"return unit.exists({u_alpha})") == "true",
               "alpha's unit survived its own page's load")
        # "beta" WAS live (hidden, not destroyed) when "alpha" was saved,
        # so engine.saveWorld captured it too — it must survive, exactly
        # like a real multi-page save (tools/multiworld_save_probe.py).
        chk.ok(page_exists(args.port, "beta"),
               "beta (WAS part of the save, just hidden) survived the load")
        chk.ok(send(args.port, f"return unit.exists({u_beta})") == "true",
               "beta's unit survived the load")
        # "gamma" was created AFTER the save and was never part of it — a
        # load REPLACES the whole session, so it must NOT survive, unlike
        # the old merge contract's "preserve unrelated live pages" (#191).
        chk.ok(not page_exists(args.port, "gamma"),
               "gamma (never part of the save) did NOT survive the load "
               "(no more #191 merge-preservation)")
        chk.ok(send(args.port, f"return unit.exists({u_gamma})") == "false",
               "gamma's unit did NOT survive the load")

        # ── 5. Haskell and Lua agree immediately post-publication ───────
        print("\n--- Haskell/Lua agreement after publication ---")
        ai_state = send(args.port,
            f"local s=require('scripts.unit_ai').getState({u_alpha}); "
            f"return s and 'present' or 'nil'").strip('"')
        chk.ok(send(args.port, f"return unit.exists({u_alpha})") == "true"
               and ai_state == "present",
               f"loaded unit is visible to both unit.exists (Haskell) and "
               f"unit_ai.getState (Lua) (ai_state={ai_state})")

        # ── 6. Paused dwell advances nothing; unpause uses default speed ──
        print("\n--- paused dwell + default-speed unpause ---")
        chk.ok(send(args.port, "return engine.isPaused()") == "true",
               "published session begins paused")
        date_a = send(args.port, "return world.getDate('alpha')")
        time.sleep(2.0)
        date_b = send(args.port, "return world.getDate('alpha')")
        chk.ok(date_a == date_b,
               f"no gameplay time advanced during a {2.0}s paused dwell "
               f"({date_a} -> {date_b})")
        # Unpause through scripts.pause (the single source of truth for
        # the paired engine.setPaused + world.setTimeScale effect — see
        # pause.lua's module header), not the raw engine.setPaused
        # binding, which only flips the engine flag and leaves the
        # timescale restoration to pause.lua.
        send(args.port, "require('scripts.pause').set(false); return 'ok'",
             expect_result=False)
        time.sleep(0.5)
        ts = send(args.port, "return world.getTimeScale('alpha')")
        chk.ok(ts.strip() in ("1", "1.0"),
               f"unpausing a loaded session uses the default time scale "
               f"(got {ts})")
        send(args.port, "require('scripts.pause').set(true); return 'ok'",
             expect_result=False)

        # ── 7. Missing content definition rejects the load outright ─────
        print("\n--- missing gameplay definition rejects the load ---")
        do_save(args.port, "alpha", slot_valid2)
        # Simulate "the def is no longer registered" without a second
        # process: temporarily clear the live unit registry's def for
        # 'acolyte' via a throwaway world.init-free path isn't exposed to
        # Lua, so this is instead covered end-to-end by booting a second,
        # def-light engine against the SAME save file.
        proc2_log = f"/tmp/transactional_load_probe_{args.port + 1}.log"
        quit_engine(args.port, proc)
        proc = None
        proc2 = boot(args.port, log=proc2_log, label="engine (no unit defs)")
        try:
            bootstrap_defs(args.port, include_units=False)
            pre_active = send(args.port, "return world.getActiveWorldId()")
            loaded = send(args.port, f"return engine.loadSave('{slot_valid2}')")
            chk.ok(loaded.strip() == "false",
                   "load with a missing unit definition is rejected")
            status = load_status(args.port)
            chk.ok(isinstance(status, dict) and status.get("phase") == "LoadFailed",
                   f"missing-def load reports LoadFailed via getLoadStatus "
                   f"({status})")
            chk.ok(send(args.port, "return world.getActiveWorldId()")
                   == pre_active,
                   "the (empty) pre-load session is unaffected by the "
                   "rejected load")
        finally:
            quit_engine(args.port, proc2)
            proc2 = None

        # ── 8. No ghost accumulation across repeated loads ──────────────
        print("\n--- repeated loads don't accumulate ghost pages ---")
        proc = boot(args.port, log=log, label="engine (repeat-load)")
        bootstrap_defs(args.port)
        for i in range(2):
            loaded = send(args.port, f"return engine.loadSave('{slot_valid}')")
            chk.ok(loaded.strip() == "true", f"repeat load #{i + 1}: accepted")
            published, status = wait_load_published(args.port, 180)
            chk.ok(published, f"repeat load #{i + 1}: publishes ({status})")
            send(args.port, "return world.waitForInit(180)", timeout=190)
            time.sleep(1.0)
        chk.ok(wait_active(args.port, "alpha"),
               "after two repeated loads, alpha is (still) the active page")
        chk.ok(page_exists(args.port, "beta"),
               "beta (genuinely part of the save) is present after "
               "repeated loads, same as after one")
        chk.ok(not page_exists(args.port, "gamma"),
               "no ghost 'gamma' page (never part of the save) "
               "reappeared across repeated loads")

        # ── 9. A save's PRIMARY page need not be the visible one ────────
        print("\n--- a non-visible primary page becomes active on load ---")
        # engine.saveWorld is explicitly page-targeted (WriteWorld.hs's
        # sgActivePage/sgVisiblePages split) -- a debug/headless caller
        # may save a page that isn't even the one on screen. The loaded
        # session must still make THAT page active, not whatever
        # happened to be visible at save time.
        send(args.port, "world.hide('alpha'); return 'ok'")
        send(args.port, "world.show('beta'); return 'ok'")
        if not wait_active(args.port, "beta"):
            chk.ok(False, "setup: beta never became active for the "
                          "non-visible-primary scenario")
        else:
            slot_primary = f"{SAVE_PREFIX}primary_{run_id}"
            save_dirs.append(os.path.join("saves", slot_primary))
            saved = send(args.port, f"return engine.saveWorld('alpha', '{slot_primary}')")
            chk.ok(saved.strip() == "true",
                   "saving 'alpha' (hidden, non-visible) as primary is accepted")
            save_file = os.path.join("saves", slot_primary, "world.synworld")
            for _ in range(100):
                if os.path.exists(save_file):
                    break
                time.sleep(0.1)
            loaded = send(args.port, f"return engine.loadSave('{slot_primary}')")
            chk.ok(loaded.strip() == "true", "loading the non-visible-primary save is accepted")
            published, status = wait_load_published(args.port, 180)
            chk.ok(published, f"non-visible-primary load: transaction publishes ({status})")
            if published:
                chk.ok(wait_active(args.port, "alpha"),
                       "the save's PRIMARY page ('alpha') is active after load, "
                       "even though it was hidden (not 'beta') at save time")

        # ── 10. Missing material reference rejects the load outright ────
        # Runs LAST and deliberately mutates the live 'alpha' page with
        # no attempt to clean it up afterward: world.setCell's edit-log
        # entry is a permanent append (WorldEdits is a per-chunk HISTORY,
        # never compacted at runtime) that a later engine.saveWorld('alpha',
        # ...) would inherit and reject too, so this must run once every
        # other section that still needs a clean 'alpha' has finished.
        print("\n--- missing material reference rejects the load ---")
        # Round 3 review: the issue's own acceptance criteria names
        # "material" explicitly alongside unit/item/building/recipe as a
        # definition kind whose absence must reject a load. Unlike those
        # (Text-keyed, freely add/removable via YAML), a MaterialId is a
        # Word8 index that's ALWAYS structurally valid, so there's no
        # live API to "unregister" one the way missing-def above swaps
        # in a def-light second process -- instead this injects a raw,
        # never-registered id (200 -- confirmed absent from every
        # data/materials/*.yaml entry) directly into a real edit via
        # world.setCell's numeric-id path (Engine.Scripting.Lua.API
        # .World.Edit.worldSetCellFn accepts any 0-255 value with no
        # registry check), proving the SAME validation path
        # (missingMaterialReferences) a genuinely older/incompatible
        # save would hit.
        send(args.port, "world.show('alpha'); return 'ok'")
        wait_active(args.port, "alpha")
        strip = find_flat_strip(args.port)
        chk.ok(strip is not None, "setup: found a flat tile on alpha to "
                                   "carve a bad material into")
        if strip:
            mgx, mgy, mz = strip
            set_ok = send(args.port,
                f"return world.setCell('alpha', {mgx}, {mgy}, {mz}, 200)")
            chk.ok(set_ok.strip() == "true",
                   "world.setCell accepts the raw (unregistered) id 200")
            slot_badmat = f"{SAVE_PREFIX}badmat_{run_id}"
            save_dirs.append(os.path.join("saves", slot_badmat))
            do_save(args.port, "alpha", slot_badmat)
            pre_active = send(args.port, "return world.getActiveWorldId()")
            loaded = send(args.port,
                f"return engine.loadSave('{slot_badmat}')")
            chk.ok(loaded.strip() == "false",
                   "load referencing an unregistered material id is "
                   "rejected")
            status = load_status(args.port)
            outcome = str(status.get("outcome", "")) if isinstance(status, dict) else ""
            chk.ok(isinstance(status, dict) and status.get("phase") == "LoadFailed"
                   and "material" in outcome,
                   f"missing-material load reports LoadFailed naming "
                   f"'material' via getLoadStatus ({status})")
            chk.ok(send(args.port, "return world.getActiveWorldId()")
                   == pre_active,
                   "the pre-load session (still alpha) is unaffected by "
                   "the rejected load")
            paused = send(args.port, "return engine.isPaused()")
            chk.ok(paused.strip() == "true",
                   "the pre-load session stays paused after the "
                   "rejected load (requirement 3/15)")

        print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: "
              f"{chk.failed} check(s) failed")
        return 0 if chk.failed == 0 else 1

    finally:
        if proc is not None:
            quit_engine(args.port, proc)
        for d in save_dirs:
            if os.path.basename(d).startswith(SAVE_PREFIX) and os.path.isdir(d):
                shutil.rmtree(d, ignore_errors=True)
        race_dir = os.path.join("saves", f"{SAVE_PREFIX}race_{run_id}")
        if os.path.isdir(race_dir):
            shutil.rmtree(race_dir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
