# PSP-4 current save test coverage map

PSP-4 prepares current behavior tests for PSP-5's coordinated retirement. It does not activate a current-only default, remove compatibility behavior, or change a runtime registration. The Hspec migration group remains registered until PSP-5.

## Current assertions and their owners

| Assertion | Before | Current owner after preparation |
|---|---|---|
| Current page core: significant obligations and one-shot clearance notice | `Test.Headless.World.Save.Compat.Historical.languageProvenanceSpec` | `current page-core persistence` (synthetic `WorldPagesDTO`) |
| Current page identity/location/river etymology sources; river name/gloss; location name/gloss; full encounter; nonzero clock remainder; language provenance | `Test.Headless.World.Save.Compat.Historical.languageProvenanceSpec` | `current page-core persistence` (synthetic current DTO values) |
| Exact fluid remainders and identity-specific flora removal through the current world-edits codec | `Test.Headless.Sim.ExactFluid.saveSpec` | `world-edits current persistence` (synthetic snapshot and current codec version) |
| Cell and flora-occurrence tag admission, including invalid cell/occurrence and finite-arena bounds | Fixture-only `Test.Headless.World.Save.Compat.TagTargets.tagTargetsSpec` | `current tag admission` (synthetic current pages through generated chunks, flora-reference resolution, edit replay, arena clipping, slope application and tag reconciliation) |
| Lua `unit_ai` owner/kind/id validation | `Test.Headless.Lua.SaveModules.Components.spec` examples built with v2/v3 payload labels | `Lua persistence components / current Lua reference validation`, using current `unit_ai` v11 payloads with valid `__owner` wrappers; intentional missing/mismatched-owner cases remain negative |
| Lua location-memory current payload and current declaration | `Test.Headless.Lua.UnitAiLocations` schema-evolution group using v4 literals and an uninterrupted historical acceptance loop | Current declaration and payload assertions use v11; historical decode/migration examples remain separately described |
| Position hold current round trip and transient harvest approach reset | `Test.Headless.Lua.UnitAiHold`, `UnitAiYieldProximity`, and `UnitAiHarvest` owner groups | Existing owner tests pin current v11 payload/version and meaningful values; old-version acceptance stays in migration examples |
| Current registry, assembly, integrity, required/optional component behavior, recursive item physical values, non-default simulation/crafting/power/climate, diagnostics, and references | `Test.Headless.World.Save.Components`, its component owners, and `Test.Headless.World.Save.Integrity` | Retained current synthetic groups; old DTO decode cases remain in the migration family |
| Current river names, movement hazard, flora authored names, and container knowledge | `Test.Headless.World.Save.Compat.Baselines` read tracked compatibility payloads | `Test.Headless.World.Save.Contract` round-trips `richRiverNames` with gloss and etymology, the `FallProhibited` movement hazard, and authored flora names at edit/crop/designation references; `Test.Headless.Building.Knowledge` covers populated current knowledge through the real codec. Fixture-backed current-version baseline examples were removed from the historical family. |

## Validation record

All commands ran from the issue worktree. Each selected Hspec group was nonempty.

| Command | Result |
|---|---|
| `cabal build all` | passed |
| `cabal build synarchy-test-headless` | passed |
| `cabal test synarchy-test-headless --test-options='--match "save components"'` | 179 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "persistence contract"'` | 9 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "persistence reference integrity"'` | 77 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "Lua persistence components"'` | 106 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "save migrations"'` | 112 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "current page-core persistence"'` | 7 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "world-edits current persistence"'` | 1 example passed |
| `cabal test synarchy-test-headless --test-options='--match "current tag admission"'` | 2 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "current Lua reference validation"'` | 7 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "the hold rides the lua.unit_ai component"'` | 2 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "lua.unit_ai current schema and historical migration"'` | 5 examples passed |
| `cabal test synarchy-test-headless --test-options='--match "classifies the approach records transient"'` | 1 example passed |
| `cabal test synarchy-test-headless --test-options='--match "lua.unit_ai payload shape"'` | 1 example passed |
| `python3 tools/test_save_compat_audit.py --mode current` | 7 members / 28 assertions passed |
| `python3 tools/persistence_inventory_audit.py` | passed (250 root-owner fields, 3 Lua modules, 10 typed fields, 10 Lua kinds) |
| `python3 tools/test_persistence_inventory_audit.py` | 173 groups / 264 assertions passed |

The machine-read inventory at `docs/persistence_state_inventory.md` is consumed by the persistence audit, so its updated coverage links ship with the implementation PR.

## PSP-5 bounded retirement list

At gate activation, PSP-5 may remove the historical-only obligations below as one coordinated change, after confirming each current owner above is still registered and nonempty:

- Historical DTO and legacy-envelope migration examples in `Test.Headless.World.Save.Compat.Historical` and `Compat.Legacy`.
- Manifest-driven fixture decoding, old-version defaults, and fixture registrations/consumers under `Test.Headless.World.Save.Compat.Baselines` and `Compat.TagTargets`, plus the historical corpus files and audit registration. Current-version baseline assertions were removed from these fixture-reading families; their distinguishing current expectations now live in the current owners above.
- Lua historical accepted-version and migration cases, including tracked v1 Lua fixtures and `building_spawn` v1/v2 migrations; keep current-version schema, reference, payload and reset assertions.
- Exact-fluid old-version migrations in `Test.Headless.Sim.ExactFluid.saveSpec`; keep `world-edits current persistence`.

PSP-5 owns the default/gate switch and deletion. This preparation does not add a historical mode, optional suite, retirement manifest, or claim that retirement has started.
