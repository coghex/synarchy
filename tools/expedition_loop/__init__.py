"""Stage owners behind `tools/expedition_loop_probe.py` (#2092).

The probe stays ONE registered, integrated scenario — one fixed-seed
expedition carried through preparation, travel, discovery, extraction,
return, control comparison, save and a fresh-process load, sharing one
world, one experimental control, one save handoff and one fingerprint.
What moved here is WHO owns each part of it, so adding the next stage
(#917's checks landed inside these owners; epic #1229's confrontation is
a NEW stage between travel and extraction) is a local change rather than
another 900 lines of `main`.

  * `constants` — every stable value the scenario is defined by: the
    page and slot, the def names, the site bounds, the departure
    contract, the control's bar, the objective ids, the stage list.
  * `harness` — the run's substrate: `Checks` (stage attribution,
    aggregate reporting), `SetupError` / `StageAbort`, the throwaway
    resource root, the shared bootstrap, `boot_probe`, the two shared
    stage-aware check helpers, the `ExpeditionState` handoff record and
    the one `Fingerprint` accumulator.
  * `readers` — every engine/world/entity query more than one owner
    needs, plus the deterministic geometry over their answers.
  * `setup` — the world, the seed-stable site and ruin choice, the
    portal and the roster it spawns, colony storage, the loot rolls.
  * `prepare` — water, provisioning, capacity levelling, and the muster
    that calibrates the shared departure (its public interface to
    `travel`).
  * `travel` — the shared leg, sight-based discovery, the paired
    sampling, and the survival control scored from it.
  * `extract` — the retrieval orders, the walk home, and the deposit
    into colony storage.
  * `persistence` — the save capture, and the fresh-process reload.

THESE ARE LIBRARIES, NOT PROBES. Nothing here is registered in
`tools/probe_runner_registry.py`, appears in `tools/ci_probes.py` or
`docs/probe_census.json`, or is independently runnable, and none of the
module names ends in `_probe` — `run_probes.py`'s own docstring reserves
that suffix for a self-contained regression harness. There is exactly
one `expedition_loop` registration and one executable, the facade.

None of these modules parses a CLI, allocates a port, creates a resource
root, boots or quits an engine, publishes a fingerprint, or keeps a
failure counter of its own. The facade owns all of that; an owner is
handed a live port and the shared state and asserts against it.
"""
