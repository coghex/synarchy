#!/usr/bin/env python3
"""Manual real-thread smoke probe for the #757 save boundary, extended
(#758) to also prove the immutable-snapshot capture properties:
a mutation completed before capture appears in the saved snapshot
(the pre-existing fluid-spread check below); a mutation made
immediately after the barrier releases does NOT alter that
already-captured save (only reachable now that #758 releases the
barrier before encode+disk I/O rather than after); and a later save
captures that later mutation as its own, distinct boundary. Also
proves (#758 review round 2 follow-up) that a genuine disk-level write
failure surfaces as a real SaveFailed outcome rather than crashing the
world thread or wedging the barrier open forever."""
from __future__ import annotations
import argparse, json, os, shutil, subprocess, sys, tempfile, time, uuid
from pathlib import Path
import probe_protocol
from probelib import boot, quit_engine, send, wait_load_published

SAVE = "probe_barrier_" + uuid.uuid4().hex[:12]
RESAVE = SAVE + "_resave"
SAVE2 = SAVE + "_later"
REPO = Path(__file__).resolve().parent.parent

def make_isolated_root(base):
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory, so this probe never touches a real player's saves
    (round-6 review, issue #767 requirement 15's cross-referenced-probe
    isolation gap)."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root

def wait(predicate, what, timeout=30):
    end = time.time() + timeout
    while time.time() < end:
        value = predicate()
        if value: return value
        time.sleep(.2)
    raise RuntimeError("timed out waiting for " + what)

def fluid_at(port, gx, gy):
    """Fluid state at one tile -- {} when dry (or unloaded). Uses
    getAreaFluid (JSON-friendly table) rather than getFluidAt, whose
    documented arity is a raw multi-value/nil Lua return, not a table."""
    cells = json.loads(send(port, f"return world.getAreaFluid({gx},{gy},0)"))
    return next((c for c in cells if c["x"] == gx and c["y"] == gy), {})

def other_kind(natural_type):
    """A setFluidTile kind string guaranteed to produce an OBSERVABLE
    change from whatever a tile's natural type already is -- this
    seed's worldgen isn't guaranteed dry land (or any specific type) at
    a fixed offset, so rather than assuming a starting state, just pick
    a target different from it. setFluidTile's kind->type mapping:
    "river"->River, "ocean"->Ocean, anything else (incl. "water")->Lake."""
    return "river" if natural_type == "lake" else "water"

PROBE_CHECKS = [
    ('save_accepted', 'first save is accepted'),
    ('owners_acknowledged', 'capture acknowledges every save owner'),
    ('later_save_accepted', 'later save is accepted after the mutation'),
    ('write_failure_accepted', 'disk-failure save reaches disk IO'),
    ('write_failure_outcome', 'disk write failure reports SaveAborted'),
    ('followup_accepted', 'save is accepted after the disk write failure'),
    ('load_accepted', 'first snapshot load is accepted'),
    ('load_published', 'first snapshot load publishes'),
    ('load_paused', 'loaded session is paused'),
    ('spread_restored', 'pre-boundary simulation spread survives the save'),
    ('paused_spread_frozen', 'loaded fluid state remains frozen while paused'),
    ('post_capture_mutation_absent', 'post-capture mutation is absent from the first snapshot'),
    ('resave_accepted', 'unchanged snapshot can be saved again'),
    ('snapshot_size_stable', 'unchanged fluid snapshot grows by at most 1024 bytes'),
    ('later_load_accepted', 'later snapshot load is accepted'),
    ('later_load_published', 'later snapshot load publishes'),
    ('later_mutation_restored', 'later snapshot includes the mutation before its boundary'),
]
DESCRIPTOR = probe_protocol.build_descriptor('save_barrier', PROBE_CHECKS)


def main():
    ap = argparse.ArgumentParser(); ap.add_argument("--port", type=int, default=9143); ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 descriptor without booting")
    a = ap.parse_args()
    if a.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(a, rep)
    except Exception as exc:
        rep.abort(str(exc))
        raise
    finally:
        rep.close()


def _run(a, rep):
    tmpdir = tempfile.mkdtemp(prefix="save_barrier_probe_")
    try:
        root = make_isolated_root(tmpdir)
        return _exercise(a, root, rep)
    finally:
        shutil.rmtree(tmpdir, ignore_errors=True)

def _exercise(a, root, rep):
    path = os.path.join(root, "saves", SAVE); resave_path = os.path.join(root, "saves", RESAVE); path2 = os.path.join(root, "saves", SAVE2); p = boot(
        a.port,
        log=rep.engine_log_path("save_barrier_probe.log", "/tmp/save_barrier_probe.log"),
        args=["--resource-root", root] + rep.engine_args()
    )
    try:
        send(a.port, f'world.init("barrier",{a.seed},64,3)', expect_result=False); send(a.port, "return world.waitForInit(300)", timeout=305); send(a.port, 'world.show("barrier")', expect_result=False)
        # The post-release-mutation check below touches tiles well
        # outside the auto-loaded spawn area -- load their chunks
        # explicitly first (chunk, not tile, coordinates).
        send(a.port, "return world.loadChunksInRegion(-2,-2,3,3)")
        send(a.port, "return world.waitForChunks(120)", timeout=125)
        # This is a real World -> simulation -> World path: the edit is
        # accepted by worldQueue, synchronizes its chunk into simQueue, and
        # fluid settling publishes a WorldApplyFluids writeback.  The source
        # cell itself is changed synchronously by World, so assert a distinct
        # neighboring cell whose fluid state changes only after the sim's
        # writeback instead.
        def area_fluid():
            return {
                (cell["x"], cell["y"]): cell
                for cell in json.loads(send(a.port, "return world.getAreaFluid(0,0,3)"))
            }

        area_before = area_fluid()
        send(a.port, 'world.setFluidTile("barrier", 0, 0, "water")', expect_result=False)

        def spread():
            after = area_fluid()
            return next(
                (
                    (coord, cell)
                    for coord, cell in after.items()
                    if coord != (0, 0) and area_before.get(coord) != cell
                ),
                None,
            )

        spread_coord, spread_before = wait(spread, "simulation fluid spread writeback")
        if not rep.check(
            'save_accepted',
            send(a.port, f'return engine.saveWorld("barrier","{SAVE}")').strip() == 'true',
            DESCRIPTOR.label('save_accepted')
        ):
            raise RuntimeError("save rejected")
        def state():
            raw = send(a.port, "return engine.getSaveStatus()"); return json.loads(raw) if raw != "nil" else None
        s = wait(state, "save status", 10)
        if not rep.check(
            'owners_acknowledged',
            s['ownerCount'] == s['acknowledgedOwners'],
            DESCRIPTOR.label('owners_acknowledged')
        ):
            raise RuntimeError("save reached capture without all owners")
        wait(lambda: os.path.isfile(os.path.join(path, "world.synworld")), "save file")

        # #758: the barrier releases as soon as the snapshot is captured
        # and validated -- BEFORE encoding/disk I/O. A mutation issued the
        # instant the file appears (i.e. after the whole save, capture
        # included, has demonstrably finished) must never have been able
        # to reach the ALREADY-CAPTURED snapshot that produced that file.
        # The mutation coordinate is well outside the (0,0)-radius-3
        # area_fluid() window above, so it can't collide with the
        # spread-detection dict.
        mx, my = 20, 20
        natural = fluid_at(a.port, mx, my).get("type")
        mutate_kind = other_kind(natural)
        mutated_type = "river" if mutate_kind == "river" else "lake"
        send(a.port, f'world.setFluidTile("barrier", {mx}, {my}, "{mutate_kind}")', expect_result=False)
        wait(lambda: fluid_at(a.port, mx, my).get("type") == mutated_type,
             "post-release mutation to take effect")

        # A LATER save, after the mutation, must capture it as ITS OWN
        # distinct boundary -- neither save shares captured state with
        # the other.
        if not rep.check(
            'later_save_accepted',
            send(a.port, f'return engine.saveWorld("barrier","{SAVE2}")').strip() == 'true',
            DESCRIPTOR.label('later_save_accepted')
        ):
            raise RuntimeError("second save rejected")
        wait(lambda: os.path.isfile(os.path.join(path2, "world.synworld")), "second save file")

        # #758 review round 2 follow-up: a genuine disk-level write failure
        # (the save's own directory PATH already occupied by a plain file,
        # so createDirectoryIfMissing inside writeSaveFiles must fail) must
        # surface as a real SaveFailed outcome via engine.getSaveStatus(),
        # not crash the world thread or leave the barrier stuck open
        # forever (saveInProgress permanently True, refusing every later
        # save) -- the exact risk an uncaught IO exception in writeSaveFiles
        # would create, since it runs AFTER the barrier's capture lock has
        # already released.
        WFAIL = SAVE + "_writefail"
        wfail_path = os.path.join(root, "saves", WFAIL)
        if os.path.isdir(wfail_path): shutil.rmtree(wfail_path)
        elif os.path.exists(wfail_path): os.remove(wfail_path)
        with open(wfail_path, "w") as f: f.write("occupying this path with a plain file")
        try:
            if not rep.check(
                'write_failure_accepted',
                send(a.port, f'return engine.saveWorld("barrier","{WFAIL}")').strip() == 'true',
                DESCRIPTOR.label('write_failure_accepted')
            ):
                raise RuntimeError("write-failure save rejected before it ever reached disk I/O")
            def failed_status():
                raw = send(a.port, "return engine.getSaveStatus()")
                st = json.loads(raw) if raw != "nil" else None
                return st if st and st.get("phase") == "SaveFailed" else None
            fs = wait(failed_status, "disk write failure to surface as SaveFailed", 15)
            if not rep.check(
                'write_failure_outcome',
                'SaveAborted' in fs.get('outcome', ''),
                DESCRIPTOR.label('write_failure_outcome')
            ):
                raise RuntimeError(f"expected a SaveAborted outcome, got {fs!r}")
        finally:
            os.remove(wfail_path)
        # The world thread must still be alive and the barrier must have
        # unblocked itself (saveInProgress back to False): an ordinary save
        # issued right after must still be accepted and actually complete.
        WFAIL_FOLLOWUP = SAVE + "_writefail_followup"
        followup_path = os.path.join(root, "saves", WFAIL_FOLLOWUP)
        if not rep.check(
            'followup_accepted',
            send(a.port, f'return engine.saveWorld("barrier","{WFAIL_FOLLOWUP}")').strip() == 'true',
            DESCRIPTOR.label('followup_accepted')
        ):
            raise RuntimeError("save rejected right after a prior write failure -- barrier stuck open?")
        wait(lambda: os.path.isfile(os.path.join(followup_path, "world.synworld")), "follow-up save file")
        shutil.rmtree(followup_path, ignore_errors=True)
    finally:
        quit_engine(a.port, p)
        try: p.wait(timeout=15)
        except subprocess.TimeoutExpired: p.kill()
    p = boot(
        a.port,
        log=rep.engine_log_path("save_barrier_probe_reload.log", "/tmp/save_barrier_probe_reload.log"),
        args=["--resource-root", root] + rep.engine_args()
    )
    try:
        if not rep.check(
            'load_accepted',
            send(a.port, f'return engine.loadSave("{SAVE}")').strip() == 'true',
            DESCRIPTOR.label('load_accepted')
        ):
            raise RuntimeError("load rejected")
        # Issue #763: a load only ACCEPTS synchronously -- wait for the
        # whole-session transaction to publish before the saved page
        # ("barrier", its own id verbatim -- no more main_world remap)
        # exists live at all.
        if not rep.check(
            'load_published',
            bool(wait_load_published(a.port, 300)[0]),
            DESCRIPTOR.label('load_published')
        ):
            raise RuntimeError("load transaction did not publish")
        send(a.port, "return world.waitForInit(300)", timeout=305); send(a.port, 'world.show("barrier")', expect_result=False); time.sleep(1)
        if not rep.check(
            'load_paused',
            send(a.port, 'return engine.isPaused()').strip() == 'true',
            DESCRIPTOR.label('load_paused')
        ):
            raise RuntimeError("load was not paused")
        reloaded_spread = area_fluid().get(spread_coord)
        if not rep.check(
            'spread_restored',
            reloaded_spread == spread_before,
            DESCRIPTOR.label('spread_restored')
        ):
            raise RuntimeError(
                "pre-boundary World->Sim->World spread was not saved: "
                f"{spread_coord}: expected {spread_before!r}, got {reloaded_spread!r}"
            )
        paused_fluid = reloaded_spread
        time.sleep(2)
        if not rep.check(
            'paused_spread_frozen',
            area_fluid().get(spread_coord) == paused_fluid,
            DESCRIPTOR.label('paused_spread_frozen')
        ):
            raise RuntimeError("loaded world spread mutated while paused")
        # The mutation issued right after the FIRST save's barrier
        # released must be ABSENT from that already-captured save --
        # the tile's type must still match its PRE-mutation (natural)
        # state, not the mutated one.
        loaded_far = fluid_at(a.port, mx, my).get("type")
        if not rep.check(
            'post_capture_mutation_absent',
            loaded_far == natural,
            DESCRIPTOR.label('post_capture_mutation_absent')
        ):
            raise RuntimeError(
                "a mutation made after barrier release altered an "
                f"already-captured save: expected natural type {natural!r} "
                f"at ({mx},{my}), got {loaded_far!r}"
            )
        first_size = os.path.getsize(os.path.join(path, "world.synworld"))
        if not rep.check(
            'resave_accepted',
            send(a.port, f'return engine.saveWorld("barrier","{RESAVE}")').strip() == 'true',
            DESCRIPTOR.label('resave_accepted')
        ):
            raise RuntimeError("resave rejected")
        wait(lambda: os.path.isfile(os.path.join(resave_path, "world.synworld")), "resave file")
        second_size = os.path.getsize(os.path.join(resave_path, "world.synworld"))
        if not rep.check(
            'snapshot_size_stable',
            not (second_size > first_size + 1024),
            DESCRIPTOR.label('snapshot_size_stable')
        ):
            raise RuntimeError(
                "fluid snapshot grew across an unchanged save/load/save cycle: "
                f"first={first_size}, second={second_size}"
            )
    finally:
        quit_engine(a.port, p); shutil.rmtree(path, ignore_errors=True); shutil.rmtree(resave_path, ignore_errors=True)
    p = boot(
        a.port,
        log=rep.engine_log_path("save_barrier_probe_reload2.log", "/tmp/save_barrier_probe_reload2.log"),
        args=["--resource-root", root] + rep.engine_args()
    )
    try:
        # The SECOND save -- taken AFTER the post-release mutation --
        # must capture it as its own distinct boundary.
        if not rep.check(
            'later_load_accepted',
            send(a.port, f'return engine.loadSave("{SAVE2}")').strip() == 'true',
            DESCRIPTOR.label('later_load_accepted')
        ):
            raise RuntimeError("second-save load rejected")
        if not rep.check(
            'later_load_published',
            bool(wait_load_published(a.port, 300)[0]),
            DESCRIPTOR.label('later_load_published')
        ):
            raise RuntimeError("second-save load transaction did not publish")
        send(a.port, "return world.waitForInit(300)", timeout=305); send(a.port, 'world.show("barrier")', expect_result=False); time.sleep(1)
        loaded_far2 = fluid_at(a.port, mx, my).get("type")
        if not rep.check(
            'later_mutation_restored',
            loaded_far2 == mutated_type,
            DESCRIPTOR.label('later_mutation_restored')
        ):
            raise RuntimeError(
                "a later save did not capture a mutation made before its "
                f"own boundary: expected {mutated_type!r} at ({mx},{my}), "
                f"got {loaded_far2!r}"
            )
    finally:
        quit_engine(a.port, p); shutil.rmtree(path2, ignore_errors=True)
    rep.note('PASS: save owners acknowledged, post-release mutation isolated, later save captured its own boundary, a disk write failure surfaced as SaveFailed without wedging the barrier, and loaded session stayed paused')

if __name__ == "__main__": sys.exit(main())
