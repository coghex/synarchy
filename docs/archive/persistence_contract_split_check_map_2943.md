# persistence_contract split: check map (#2943)

Evidence for requirement 7 of #2943: every check of the old
`tools/persistence_contract_probe.py` (at `e53b748e1`) is owned by exactly one
probe now, moved verbatim and not weakened.

## Method

Every `chk.ok(...)` call site was extracted from the old file and from the three
new files (`tools/persistence_contract_probe.py`,
`tools/persistence_system_cycles_probe.py`,
`tools/persistence_probe_common.py`) with `ast`, keyed by its top-level function
and compared as `ast.unparse` source text (condition and label). Result:

- old call sites: **142**; new: core 67 + system cycles 71 + shared 6 = 144;
- **0** old call sites missing from the new files;
- **2** new call sites, both fixture checks the new mind engines need (below).

The only edit inside a moved function is `blood_cycles`, which loses its five
`mind_*` calls and their return values; its own 16 checks are unchanged.

## Focused runs (2026-11-07, macOS/aarch64, other checkouts building alongside)

- `python3 tools/run_probes.py --only persistence_contract --exact`: PASS, 265.4 s
- `python3 tools/run_probes.py --only persistence_system_cycles --exact`: PASS, 641.5 s

(A first system-cycles attempt failed in 0.5 s because its first default port,
9278, was held by another checkout's engine; the default is 9279 now.) Both
values are in `PROBE_EXPECTED_SECONDS`.

## Ownership

| Function | Old | Owner now | Checks |
|---|---|---|---|
| `build_scenario`, `assert_reset_policy`, `assert_faction_roster`, `assert_nondefault_map_mode` | engines A-D | `persistence_contract` (core) | 14 + 5 + 1 + 2 |
| `main` (engines A-D: save/load acceptance, request ids, ghost page, dwell, faction, speed, four-generation comparison) | main | `persistence_contract` (core) | 14 |
| `mind_setup` | blood engine N | `persistence_contract` (core), engine `mind1` | 10 |
| `mind_loaded` | blood engine O | `persistence_contract` (core), engine `mind2` | 10 |
| `mind_defaults` | blood engine O | `persistence_contract` (core), engine `mind2` | 3 |
| `mind_switch` | blood engine O | `persistence_contract` (core), engine `mind2` | 2 |
| `mind_second` | blood engine P | `persistence_contract` (core), engine `mind3` | 4 |
| `engagement_cycles` (E, F, G) | system cycle | `persistence_system_cycles` | 12 |
| `exhaustion_sleep_cycles` (H, I, J) | system cycle | `persistence_system_cycles` | 16 |
| `cardio_circulation_cycles` (K, L, M) | system cycle | `persistence_system_cycles` | 19 |
| `blood_cycles` (N, O, P; mind calls removed) | system cycle | `persistence_system_cycles` | 16 |
| `relation_cause_cycles` (Q, R, S, T) | system cycle | `persistence_system_cycles` | 8 |
| `save_and_wait` (3 per call), `engagement_load` (3 per call) | shared helpers | `persistence_probe_common`, run by both probes | 3 + 3 |

All 29 `mind_*` checks are in the core.

## Allowed additions (new setup checks, not replacements)

The mind helpers used to ride on the blood engines' fixture, so the new mind
engines repeat the fixture checks they depend on. They replace no moved check:

- `mind_cycles`: `the six units spawned` and `every unit has settled into a
  standing pose and the engine is paused` (copies of the same two `blood_cycles`
  checks, which stay in the blood cycle too);
- through the shared helpers, per mind engine: `save_and_wait` (3 checks) for
  `mind1` and `mind2`, and `engagement_load` (3 checks) for `mind2` and `mind3`.

## Mind engines

| Engine | Registry | Does |
|---|---|---|
| `mind1` | PRE-S11b (`make_legacy_root`) | world.init, `ensure_chunks`, six `standard` spawns, settle and pause, `mind_setup`, save `mind1`; the save is copied to the shipped root |
| `mind2` | shipped | load `mind1` (paused, no tick), `mind_loaded`, `mind_defaults`, `mind_switch` (still paused), save `mind2` |
| `mind3` | shipped | load `mind2`, `mind_second` |

Each is booted through `engagement_boot` and loaded through `engagement_load`
(definitions, `ENG_SCRIPTS`, paused unit AI, #2865), and writes its own
`enginemind1.log` to `enginemind3.log`, which cannot collide with engines A-D's
`engineA.log` to `engineD.log`.
