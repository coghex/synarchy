"""Scenario owners behind `tools/location_content_probe.py` (#2095).

The probe is one executable and one aggregate command; what moved here
is WHO owns each contract it checks, so adding a scenario no longer
means growing a single 980-line `run`.

  * `invocation` — everything that is not a scenario assertion:
    the invocation's artifact tree, its isolated resource root, the one
    boot funnel, engine disposal, the save/load request helpers, and the
    `ScenarioState` record the facade threads between owners.
  * `engine_queries` — the read/act helpers more than one scenario uses.
  * `content` — ruin geometry, content and encounter spawning,
    registered-item validation, per-instance loot stability under
    same/reverse visit order, and fresh-process no-respawn persistence.
  * `knowledge` — sight-based player discovery, exact-once discovery
    events, per-unit memories, dangling-reference reconciliation, their
    save/load survival, and same-instance-id isolation across pages.
  * `dispatch` — unknown unit/item identifiers, the warning-log
    assertions, fixed-position item placement, and valid unit/building
    content dispatch. Owns all five inline YAML fixtures, because it is
    the scenario that consumes them.
  * `naming` — generated-language names and glosses, the label fallback
    without provenance, save/load preservation, and deterministic fresh
    regeneration.

No module here boots an engine: `location_content_probe.run` owns the
process sequence (seven `boot_isolated` call sites, eight launches,
because the loot-stability site runs once for same order and once for
reversed) and hands each owner the live port it opened.
"""
