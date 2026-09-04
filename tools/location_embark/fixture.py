#!/usr/bin/env python3
"""Phase 0: the fallback-seed fixture and its durable `SAVE_BASE`
(#2164).

One owner for the whole of "is there a world these sessions can run
against, and is it on disk" — the seed order, the per-seed headless
process each candidate is generated and discarded in, and the
request-specific save publication the two loading sessions depend on
(#1746). It boots nothing on its own behalf beyond those retries:
`location_embark_probe.run_probe` calls it once, before any offscreen
session exists, and suppresses every session when it comes back without
a seed.
"""
from __future__ import annotations

from probelib import boot, quit_engine
from location_content_probe import gen_world, load_defs, placed_ready
from .constants import FIXTURE_PAGE, SAVE_BASE
from .invocation import RunArtifacts, save_and_wait, set_log


# --------------------------------------------------------------------------
# Phase 1: headless fixture prep
# --------------------------------------------------------------------------
def prepare_fixture(port: int, seeds: list[int], size: int,
                     art: RunArtifacts, min_ruins: int = 2,
                     page: str = FIXTURE_PAGE):
    """Try each seed in turn until one places >= min_ruins ruin_small
    locations, then save it as SAVE_BASE and wait for that save's own
    request to become durable.

    Returns `(seed, ruins)` when the fixture is on disk, `(None, [])`
    if every candidate seed falls short — a fail-fast diagnostic, never
    a silent generation-density change (out of scope per the issue) —
    and `(None, ruins)` when a seed qualified but its save was refused
    or never completed. Those last two are distinct on purpose: the
    caller may only report "no seed qualified" for the first, and every
    session that would LOAD this slot is suppressed for both (#1746).
    A qualifying seed whose save fails is not retried on the next seed;
    the failure is the save, not the world.

    Every retry boots into `art.root`, so the seed that eventually wins
    writes its fixture save there and the ones that don't write nothing
    the developer's root can see."""
    for candidate in seeds:
        set_log(art.log("engine_prep"))
        proc = boot(port, log=art.log("engine_prep"), args=art.boot_args(),
                    label=f"prep engine (seed {candidate})")
        try:
            load_defs(port)
            gen_world(port, page, candidate, size)
            ruins = [e for e in placed_ready(port)
                     if e.get("id") == "ruin_small" and "bounds" in e]
            print(f"  seed {candidate}: {len(ruins)} ruin_small placed")
            if len(ruins) >= min_ruins:
                if not save_and_wait(port, page, SAVE_BASE, "phase 0"):
                    return None, ruins
                return candidate, ruins
        finally:
            quit_engine(port, proc)
    return None, []
