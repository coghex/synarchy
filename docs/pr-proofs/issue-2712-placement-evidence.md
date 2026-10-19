# #2712 (SCN-12): placement construction and capture — evidence

Contract: [scenario_format.md](../scenario_format.md#placement-construction-and-capture-scn-12-2712) and
[engine_contracts.md](../engine_contracts.md#scenario-placement-structures-buildings-and-real-locations-2712).
The fixture recipe is [issue-2712-make-z17.py.txt](issue-2712-make-z17.py.txt).

## Owner decisions (relayed by the project manager, 2026-10-19)

1. **"v3 as proposed (Recommended)"** — additive format v3, identity v2 → v3 migration, v1/v2 fixtures frozen; `locations[].containers` maps slot → `{item, realized (default false), profile (pending only)}`; same optional-reference rules as `significant_items`; capture never opens or rolls a shell.
2. **"Add structures[].variant (Recommended)"** — optional pack variant name in the same v3; omitted = default art.
3. **"No extra units (Recommended)"** — scenario buildings are stored with `biSpawnRemaining = 0`; scenario units come only from `units[]`.
4. **"Confirm (Recommended)"** — encounter occupants are exactly the units that join by `encounter`; zero joined = empty ruin; a unit joining a definition with no encounter entry is rejected with a diagnostic.

## Validation

Production profile, `cabal-3.16.1.0`. Every group executed a nonzero number of examples; all passed.

| Group (`--match`) | Examples |
|---|---|
| `Scenario.Placement` | 70 |
| `Scenario.Schema` | 48 |
| `Scenario.Items` | 52 |
| `Scenario.Units` | 52 |
| `Scenario.World` | 62 |
| `Location instance identity` | 75 |
| `location stamp completion` | 12 |
| `location stamp commit gating` | 11 |
| `Location significant contents` | 39 |
| `Location container shells` | 74 |
| `Power.Network` | 58 |
| `Page incarnation entity teardown` | 57 |
| `persistence reference integrity` | 77 |
| `save components` | 177 |
| `save migrations` | 61 |
| `Gameplay.Tags` | 128 |
| `persistence contract` | 8 |
| `Structure.ArtCatalog` | 41 |
| `structure construction frames` | 91 |
| `structure destruction presentation lifecycle` | 97 |
| `Building` | 214 |
| `Save` | 140 |

Audits: `persistence_inventory_audit`, `enum_append_only_audit`, `engine_env_capability_audit`,
`save_compat_audit` (45 baselines, 51 fixtures), `haskell_module_budget`, `lua_module_budget`,
`unicode_operator_audit`, `cabal_module_audit`, `ci_parity_audit`, `haddock_link_audit`,
`sdist_manifest_audit`, `ci_expensive_gates --self-test`; self-tests `test_save_compat_audit*.py`
(including the `--only-reproducibility` member and the descriptor oracle row for
`z17-scenario-placement.bin`).

## Mutation checks

Nineteen mutants of the placement adapters and the persistence rules; each was applied to the source, the `Scenario.Placement` group run, and the source restored. A
mutant counts only if the group fails by an assertion (a compile error is not a kill).

| Mutant | Killed by |
|---|---|
| ceiling default level `surface + 2` → `surface + 1` | structures: derived levels |
| scenario building spawn counter `0` → `-1` | buildings: partly charged battery / zero spawn counter |
| `liContentsSpawned` always set | locations: empty ruin; references: binding to a rejected building's item |
| geometry marker (`wgpLocationStamped`) never set | locations: empty ruin; property: location placed exactly when its footprint fits |
| location bounds ignored (`Nothing` for the arena) | locations: whole footprint outside the map |
| variant dropped on capture | structures: derived levels and variants |
| power node charge ignored (`charge * 0`) | buildings: partly charged battery |
| a building or location reference held on two pages not flagged by integrity | scenario identity: two buildings / two locations on different pages |
| a restore keeps the references of buildings it did not produce | scenario identity: a restore keeps only the references of the buildings it produced |
| a retired building stays in the manager | buildings: retires a building whose item references cannot be held |
| references written as ids verbatim (no bracketless conversion / collision handling) | capture: names omitted ids and colliding spellings document-wide |
| a pending shell's ownership unchecked | bindings: held / nested pending shells refused |
| unbound significant obligations kept | locations: empty ruin; rejected-reference bindings; clearance never stranded |
| a significant item held off the ground not latched taken | bindings: stored significant item passes integrity |
| the clearance notice seated before bindings | initial clearance notice: empty ruin / taken-at-birth |
| omitted guaranteed content not constructed | defaults: omitted construction; recapture adds no copy |
| a stated empty map still expanding | defaults: stated empty stays empty; containers; naming |
| the loot roll keyed on the wrong entry index | matrix: omitted content equals the ordinary rolls |
| `incidental_content: false` still expanding | matrix / defaults: stated empty |

## Retained fixture

`z17-scenario-placement` (buildings v4, world-pages v16) is built by the group's `retained save-compat
fixture` example through the real adapters and encoder, with the donor save `z16-scenario-world`
supplying the metadata and Lua components; its canonical summary is derived from the real decoded
snapshot.
