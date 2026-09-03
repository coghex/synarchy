"""Family owners behind `tools/preview_probe.py` (#2089).

The probe stays ONE registered, manual-only, `needs-gpu` executable with
one aggregate pass/fail. What moved here is WHO owns each family of
scenarios, so a developer changing one family can run only its boots
(`python3 tools/preview_probe.py --only <family>`) and adding a scenario
is a local change to one owner plus one entry in the facade's inventory.

  * `harness` — the shared mechanics every family uses and none may
    duplicate: the single `boot_preview` launcher (the only call into
    `probelib.boot`, so every launch allocates its own retained log
    through the one shared `LOGS` instance — #1763), the `check` line,
    the `previewManager.dump()` reader, real key-tap/hold and pointer
    helpers, window/framebuffer geometry, the async state poll, the
    engine-authoritative trimmed-loading check and its chrome allowlist,
    the filesystem-derived simple-category expectation, the forced-
    replay oracle (#1833), and the derived first grouped item.
  * `simple` — bare simple-category list mode and focused-item mode
    (#886, #2026): phases `1.` and `2.`.
  * `units` — the acolyte viewer, the promoted tiller declaration, and
    the rest of the shipped roster (#887, #1257, #1260, #1261): phases
    `3.`, `4.` and `4b.`. Owns the unit-asset expectations, so it is
    listed under `tools/ci_expensive_gates.py`'s `UNIT_ASSET_GLOBS`.
  * `buildings` — the normal, no-built-state and YAML-free building
    viewers (#888): phases `5.`, `6.` and `7.`.
  * `dispatch` — grouped flora/structure items and the canonical
    category sweep (#888, epic #427): phases `8.` and `9.`.
  * `zoom` — centered bounded zoom over all six display kinds (#1907):
    phase `11.`, the number `docs/engine_contracts.md` names.

THESE ARE LIBRARIES, NOT PROBES. Nothing here is registered in
`tools/probe_runner_registry.py`, appears in `tools/ci_probes.py` or
`docs/probe_census.json`, or is independently runnable, and none of the
module names ends in `_probe`. There is exactly one `preview`
registration and one executable, the facade, whose `FAMILIES` inventory
is the single authority for which scenarios exist, which family each
belongs to, and the order the aggregate runs them in.

No family module parses a CLI, chooses a port, or calls `probelib.boot`
directly; every scenario is handed a port, launches through
`harness.boot_preview`, and tears its own engine down in a `finally`.
"""
