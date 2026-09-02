#!/usr/bin/env python3
"""Headless location content-spawning probe (#90) + ruin probe (#91).

Issues #88/#89 give locations a definition and a place in the world;
this checks the `contents` list actually spawns things when a
location's chunk loads, end to end:

  1. Visiting a `ruin_small` (#91: a partially-collapsed room) spawns
     its contents — two `ruin_common` loot-table rolls, as ground
     items — plus its persisted uniform 0..3 `nomad_primitive` encounter
     roll per ruin, and no buildings. #921 removed the two fixed-position `radio` /
     `canteen_steel_2l` entries this used to also expect: a ruin
     guarantees NO specific item, so the only assertion left about
     WHICH items appear is that each resolves to a registered def. The
     geometry is a damaged `room_small`: all 25 floors present, a
     breached perimeter (some but not all of the 20 wall segments),
     exactly 3 corner posts, and every piece carrying the pack's
     "damaged" variant texture path.
  1b/1c. Loot selection is seed-stable per placed instance (#948): two
     further INDEPENDENT fresh processes regenerate the same seed from
     scratch — one visiting the ruins in the same order as check 1, one
     in the exact reverse — and every stable ruin instance ends up with
     the same loot multiset it got the first time. Reversing the load
     order must not swap or shift which ruin owns which reward.
  2. The one-time content-spawn flag AND the damaged geometry survive a
     save -> quit -> fresh restart -> load: revisiting the same chunk
     does NOT respawn (counts stay exactly the same, not doubled), each
     instance keeps the exact loot it was first given (#948 — nothing is
     re-rolled across save/load or chunk eviction), the breach pattern
     replays identically, and the pieces still resolve to the damaged
     variant art (the #91 variant round-trip).
  3. An unknown content `id` — an unregistered unit, and a loot table
     rolling an unregistered item — logs a warning and is skipped
     rather than crashing the engine. An unknown content `kind` is NOT
     part of this: since #1708 the vocabulary is closed at the YAML
     boundary, so such a file fails to decode and registers nothing;
     that rejection is pinned by hspec "Location spatial bounds", and
     the fixture here is checked to LOAD so the runtime id paths below
     are actually reached. Also covers the fixed-position `kind: item`
     dispatch path, which #921 left no SHIPPED location exercising: a
     probe-local def spawns one at a declared `position` and it must
     land on exactly that tile.
  4. Location discovery (#780): stamping a ruin's geometry and spawning
     its contents do NOT discover it; a hostile unit standing on it
     doesn't either; a player-faction unit that SEES it does (#1230),
     flipping `world.listPlacedLocations()`'s `discovered`
     field and emitting exactly one `location_discovery` player event;
     re-checking without the unit moving emits no duplicate; the
     discovered state survives save -> quit -> fresh restart -> load
     alongside the geometry/contents from check 2.
  5. Per-unit location knowledge (#915): the unit that can see it
     gains its OWN memory of the location — keyed by the
     (page, instance id) identity, recorded while the sim is PAUSED
     (acquisition mirrors the discovery tick's pause independence) — a
     second player unit standing elsewhere does not, a unit arriving at
     an ALREADY-discovered location still learns it without a second
     lifecycle transition or event, and the memory survives the same
     save -> restart -> load round trip. A save also carries two
     RESOLVING sibling memories plus one naming an instance id no page
     ever allocated: the load still succeeds, the integrity graph logs
     exactly one lua.unit_ai/location_instance diagnostic naming the
     field and page/id, the real onSaveLoaded reconcile drops only that
     entry, and no lifecycle changes. Phase 4 generates two same-seed
     pages so the SAME instance id names a different real location on
     each, and checks the two units' memories stay distinct.

  6. Location naming (#1101): a world with a #1092 language provenance
     names every placed ruin in that language -- a generated native
     name (never the `ldLabel` "Small Ruin") plus a non-empty English
     gloss, both reaching Lua through `world.listPlacedLocations`. The
     SAME terrain seed with NO provenance falls back to `ldLabel` with
     the `gloss` key absent entirely, so the generated case is a real
     outcome rather than a field that is always populated. Both names
     and glosses then survive save -> quit -> fresh process -> load
     byte-exact, and re-generating the identical seed + language in a
     fresh process reproduces them exactly.

Headless skips the GUI data-loading step, so defs are registered by
hand here (items/units/buildings/loot_tables/locations), same as
tools/location_overlay_probe.py does for locations alone.

EVERY file this invocation creates -- its five fixture YAMLs, its
engine log, and the throwaway resource root with the save slots #1620
already moved into it -- lives under ONE directory this process owns,
and goes away again on every handled exit (#1884). Before that the
fixtures and the log were fixed `/tmp` names no run cleaned up, which
two concurrent runs collided on: `tools/run_probes.py --jobs N` and
`tools/probe_flake.py`'s machine-wide port lease both make that
concurrency a supported mode. `--keep-artifacts` retains the directory
instead, and names it, for diagnosing a failure -- which matters more
here than for an ordinary artifact, because the engine log is not only
diagnostics: two checks below ASSERT against it.

Since #2095 this file is the stable FACADE: the CLI, the artifact guard,
the eight-process sequence, and the compatibility exports other probes
import. Every scenario assertion belongs to an owner under
`tools/location_content/` -- `content`, `knowledge`, `dispatch` and
`naming` -- reached with the live port this file opened and the
`ScenarioState` it threads between them. No owner boots an engine, and
nothing crosses between them through a module global.

Usage:
  python3 tools/location_content_probe.py
  python3 tools/location_content_probe.py --seed 42 --size 64 --port 9190
  python3 tools/location_content_probe.py --keep-artifacts

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import os
import tempfile
import time

from probelib import FixtureNotRegistered, load_ai_stack, quit_engine, send
from probe_runner_diagnostics import FailureEmitter   # durable failure records (#1982)

from location_content import content, dispatch, knowledge, naming
from location_content.engine_queries import (gen_world, load_defs,
                                             load_registries, placed_ready,
                                             ruin_geometry, spawn_counts,
                                             wait_floor)
# Re-exported, NOT wrapped: `tools/test_location_content_probe.py` asserts
# object IDENTITY against `tools/portal_ghost_probe.py`'s imports and pins
# `save_and_wait`'s exact signature, so a delegating wrapper would break
# both. `REPO` is here for the same reason -- that companion resolves the
# repository through this module -- and is unused by the code below.
from location_content.invocation import (REPO, ROOT_PREFIX, RunArtifacts,
                                         ScenarioState, _PhaseAborted,
                                         abandon_engine, boot_isolated,
                                         load_and_wait, make_isolated_root,
                                         release_artifacts,
                                         remove_isolated_root, save_and_wait)

#: #1982 — this run's durable failure records, built at import so the
#: offset each carries is measured from the probe's own start. It lives
#: here, in the file the runner launches: the invocation module makes
#: engines and trees, and takes this emitter as an argument on the one
#: path that has a failure to record, rather than deciding for the probe
#: how a failure is reported.
FAILURE = FailureEmitter("location_content_probe")

#: The nine names `tools/portal_ghost_probe.py`,
#: `tools/portal_location_probe.py` and `tools/location_embark_probe.py`
#: import from here (#2095 requirement 9), plus `REPO` -- which the
#: companion self-test reads -- and this file's own entry points. Listed
#: so the compatibility surface is a declaration rather than an accident
#: of which imports happen to be above it.
__all__ = [
    "load_defs", "gen_world", "placed_ready", "wait_floor",
    "make_isolated_root", "remove_isolated_root", "save_and_wait",
    "ruin_geometry", "spawn_counts",
    "REPO", "main", "run",
]


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9190)
    ap.add_argument("--keep-artifacts", action="store_true",
                    help="keep this run's artifact directory (its five "
                         "fixture YAMLs, the engine log, and the isolated "
                         "resource root with its save slots) instead of "
                         "deleting it, and name it in the summary — for "
                         "diagnosing a failure")
    args = ap.parse_args()

    # One artifact directory per invocation, holding the throwaway
    # resource root (#1620 requirement 5 — slot names carry that root's
    # own random token, so no developer-visible save slot is created,
    # mutated or rotated) AND, since #1884, the five fixture YAMLs and
    # the engine log that used to be fixed /tmp names.
    #
    # The guard starts HERE, one statement after that directory exists,
    # because everything between this point and the release below can
    # fail with invocation-owned state already on disk: `build` stages
    # incrementally — the root, three symlinks, a copied `config/`,
    # `saves/`, then the log and fixture directories — so a permission,
    # source or disk-space failure part-way through leaves a partial
    # tree that nothing outside this guard would remove.
    art = RunArtifacts(tempfile.mkdtemp(prefix=ROOT_PREFIX))
    # The WHOLE random suffix, not the text after the last underscore:
    # mkdtemp's alphabet includes '_', so splitting on it can throw most
    # of the entropy away (and can leave nothing at all).
    token = os.path.basename(art.base)[len(ROOT_PREFIX):]
    rc = 1
    try:
        art.build()
        rc = run(args, art, token)
    except SystemExit as exc:
        # How `probelib.boot` ends a run whose engine died before READY
        # or never printed it. Reported rather than allowed to exit, so
        # the release below stays on the path and the summary names the
        # abort. A `finally` alone would still release, but the run's
        # own result would be lost with the propagating exit.
        # Durable (#1982), and the one path where it matters most: `run`
        # never reached its own report, so this record is the ONLY thing
        # naming why the run ended. The engine log is read here, before
        # the release below removes the tree holding it.
        FAILURE.check(f"the run aborted before finishing: {exc}")
        FAILURE.context_log(art.engine_log)
        FAILURE.context("artifact root", art.base)
    finally:
        # Every engine this run LAUNCHED must be dead before its files
        # go: each phase already quits its own in a `finally`, but that
        # leaves two windows — an interrupt inside `probelib.boot`'s
        # three-minute READY wait, and one inside `quit_engine` itself,
        # which sends, waits, then hard-kills. `abandon_engine` is a
        # no-op on a handle that has already exited, so the orderly
        # shutdowns above are untouched.
        for proc in art.launched:
            abandon_engine(proc, FAILURE)
        # Reported, never swallowed, and reported even when `run` is
        # leaving by an exception — a root that survived is exactly the
        # artifact #1620 requirement 6 forbids, and since #1884 the
        # fixtures and log inside it are covered by the same removal.
        leftover = release_artifacts(art, args.keep_artifacts)
        if leftover:
            FAILURE.check(leftover)
        elif rc != 0 and not args.keep_artifacts:
            # A failure's primary evidence is the engine log, and some
            # paths above have already named it — `probelib.boot`'s
            # abort message quotes the path verbatim. It has just been
            # deleted with the rest of the tree, so say so here rather
            # than leave the operator chasing a path that is gone.
            print("  (this run's engine log and fixture YAMLs went with its "
                  "artifact directory — re-run with --keep-artifacts to keep "
                  "them)")
    return 1 if leftover else rc


def run(args, art: RunArtifacts, token: str) -> int:
    """The eight-process sequence, and nothing else.

    Seven `boot_isolated` call sites; the loot-stability one runs twice
    (same order, then reversed), so a passing run launches eight engine
    processes. Every scenario assertion lives in a `location_content.*`
    owner, reached with the live port this function opened and the
    `ScenarioState` it threads between them — no owner boots an engine
    of its own, and no owner reaches another through a module global.
    """
    slot_content = f"loc_content_probe_{token}"
    slot_naming = f"loc_naming_probe_{token}"

    failures: list[str] = []
    state = ScenarioState()

    # ---- Process 1: content spawns when a ruin's chunk loads, and a
    #      player-faction unit that SEES one discovers and remembers it.
    #      Both owners read the same live page, in that order: the loot
    #      baseline is captured before any discovery unit exists. ----
    proc = boot_isolated(args.port, art)
    try:
        load_defs(args.port)
        gen_world(args.port, "wa", args.seed, args.size)
        content.observe_initial_content(args, state, failures)
        if state.ruins:
            knowledge.observe_initial_discovery(args, state, failures)
            # Process 4 reads this fixture from a FRESH process, so the
            # save must be COMPLETE — not merely accepted — before that
            # process boots (#1620).
            state.saved_content = save_and_wait(args.port, "wa", slot_content,
                                                failures, log=art.engine_log)
    finally:
        quit_engine(args.port, proc)

    # ---- Processes 2 and 3 (#948): loot selection is seed-stable per
    #      placed instance. Two more INDEPENDENT fresh processes generate
    #      the same seed from scratch — one visiting the ruins in the same
    #      order as process 1, one in the exact reverse — and each ruin
    #      instance must end up with the same loot multiset it got in
    #      process 1. Before that issue the rolls came off the shared,
    #      entropy-seeded stat RNG, so both runs would disagree with
    #      process 1 and the reversed run would additionally SWAP which
    #      ruin got which reward.
    #
    #      ONE call site, TWO launches. Unrolling this loop, flattening
    #      it to one case or growing it to three changes the process
    #      count the acceptance pins, which is why the companion
    #      self-test asserts on the loop and not merely on the count of
    #      `boot_isolated` call sites. ----
    if state.ruins and state.loot1 and not failures:
        for label, reverse in (("same order", False), ("reversed order", True)):
            proc = boot_isolated(args.port, art)
            try:
                load_defs(args.port)
                gen_world(args.port, "wa", args.seed, args.size)
                content.check_loot_stability(args, state, failures,
                                             label, reverse)
            finally:
                quit_engine(args.port, proc)

    # ---- Process 4: save -> quit -> fresh restart -> load -> revisit does
    #      NOT respawn (one-time flag persisted, independent of the
    #      structure.hasAt geometry check), and the discovery lifecycle
    #      and the per-unit memories ride the same round trip. ----
    if state.ruins and state.saved_content and not failures:
        proc = boot_isolated(args.port, art)
        try:
            load_defs(args.port)
            # #915: the unit AI stack must be loaded BEFORE the load, so
            # the (required) lua.unit_ai component has a registered
            # reader — and so onSaveLoaded's reconcile, which scrubs a
            # location memory whose instance is absent from the restored
            # session, actually runs.
            load_ai_stack(args.port)
            # Issue #763: engine.loadSave only ACCEPTS synchronously -- the
            # saved page ("wa", its own id verbatim -- no more main_world
            # remap) doesn't exist live until the transaction publishes.
            if not load_and_wait(args.port, slot_content, failures,
                                 log=art.engine_log):
                raise _PhaseAborted
            send(args.port, "world.show('wa'); return 'ok'")
            time.sleep(1.0)
            # Interleaved deliberately: the two owners sharing this
            # process report their halves in the order they always did,
            # so the run's printed diagnostics are unchanged.
            content.check_reload_counts_and_loot(args, state, failures)
            knowledge.check_discovery_survived(args, state, failures)
            content.check_geometry_replay(args, state, failures)
            knowledge.check_memory_survived(args, art, state, failures)
        except _PhaseAborted:
            pass
        finally:
            quit_engine(args.port, proc)
    elif not state.ruins:
        failures.append("phase 2 skipped: no ruins from phase 1")

    # ---- Process 5: an unknown content id logs a warning and is skipped,
    #      not a crash. Also covers a loot_table rolling an item id that
    #      isn't registered, and the fixed-position `kind: item` branch.
    #      An unknown content KIND is deliberately absent from the
    #      fixture: #1708 closed that vocabulary at the YAML boundary, so
    #      an entry naming one would fail the whole file's decode and
    #      leave bogus_ruin unregistered, taking the unknown-ID checks
    #      down with it. ----
    (bogus_yaml, bogus_loot_yaml, quinoa_yaml,
     quinoa_loot_yaml) = dispatch.write_rejection_fixtures(art)
    proc = boot_isolated(args.port, art)
    try:
        load_defs(args.port)
        dispatch.register_rejection_fixtures(args.port, bogus_yaml,
                                             bogus_loot_yaml, quinoa_yaml,
                                             quinoa_loot_yaml)
        gen_world(args.port, "wc", args.seed, args.size)
        dispatch.check_unknown_content(args, art, failures)
    finally:
        quit_engine(args.port, proc)

    # ---- Process 6: a building AND a unit content entry spawn correctly
    #      on a HIDDEN, non-active page, and #915's cross-page identity
    #      check then runs on the very site the dispatch owner
    #      established — handed back through here rather than shared. ----
    dense_yaml = dispatch.write_dense_fixture(art)
    proc = boot_isolated(args.port, art)
    try:
        # Registries only — NOT ruin_small.yaml, which would contend with
        # dense_ruin for chunk (0,0) and make the placement non-deterministic
        # (mirrors tools/location_overlay_probe.py's isolated DENSE_YAML use).
        load_registries(args.port)
        dispatch.register_dense_fixture(args.port, dense_yaml)
        site = dispatch.check_hidden_page_dispatch(args, failures)
        if site is not None:
            knowledge.check_cross_page_instance_isolation(args, site, failures)
    finally:
        quit_engine(args.port, proc)

    # ---- Process 7 (#1101): a placed location is named in its world's
    #      own generated language, falls back to the definition label
    #      when the world has none, and both survive save/load. ----
    proc = boot_isolated(args.port, art)
    try:
        load_defs(args.port)
        if naming.check_generated_names(args, state, failures):
            # The fresh process below reads this fixture (#1620).
            state.saved_naming = save_and_wait(args.port, "ln", slot_naming,
                                               failures, log=art.engine_log)
    finally:
        quit_engine(args.port, proc)

    # ---- Process 8: every name and gloss comes back from that save
    #      byte-exact, and regenerating the same seed + language in a
    #      fresh process reproduces them. ----
    if state.named and state.saved_naming and not failures:
        proc = boot_isolated(args.port, art)
        try:
            load_defs(args.port)
            load_ai_stack(args.port)
            if not load_and_wait(args.port, slot_naming, failures,
                                 log=art.engine_log):
                raise _PhaseAborted
            send(args.port, "world.show('ln'); return 'ok'")
            time.sleep(1.0)
            naming.check_names_survived_reload(args, state, failures)
        except _PhaseAborted:
            pass
        finally:
            quit_engine(args.port, proc)

    print("-" * 56)
    if failures:
        # Durable records rather than the unflushed stderr print this was
        # (#1982): `run_probes.py` merges this probe's stderr into a
        # block-buffered stdout pipe and prints only its last 25 lines, so
        # a printed `FAIL:` overtook the buffered checks and landed above
        # the retained tail. These are read back from the COMPLETE
        # capture. Emitted here, inside `run`, because `main`'s cleanup
        # removes the artifact tree the engine log lives in.
        FAILURE.report(failures)
        FAILURE.context_log(art.engine_log)
        FAILURE.context("artifact root", art.base)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        raise SystemExit(1)
