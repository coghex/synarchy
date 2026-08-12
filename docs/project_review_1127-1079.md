# Project Review Findings: PRs #1127–#1079

These entries record focused evidence from the senior review of merged PRs #1127 through #1079 for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

## Status

- [ ] PRR-1. Lua's Haskell-component registry omits container knowledge
- [ ] PRR-2. Modern saves can silently omit all container knowledge
- [ ] PRR-3. Non-finite container observations pass save validation
- [ ] PRR-4. Restored-entity apply context is mutable across components
- [ ] PRR-5. Persisted language provenance accepts unconstructible versions

## 1. Container-knowledge persistence

### PRR-1. Lua's Haskell-component registry omits container knowledge

> **Captured note:** Keep Lua's declared Haskell-component dependency set synchronized with the real Haskell registry. PR #1126 registered `container-knowledge` on the Haskell side but did not add it to `save_modules.lua`, so a Lua component cannot declare that real dependency without being rejected as structurally invalid.

**Verification:** Verified — a direct Lua registration whose only dependency is `container-knowledge` produces `component 'probe_dep' depends on unregistered 'container-knowledge'` from the current registry.

**Evidence:**

- `src/World/Save/Component.hs:113` — `containerKnowledgeCodec` is a registered Haskell gameplay component and therefore appears in `componentKnownIds`.
- `scripts/lib/save_modules.lua:174` — the hand-maintained mirror says a new entry must be added whenever the Haskell component set grows.
- `scripts/lib/save_modules.lua:182` — `HASKELL_COMPONENT_IDS` ends at `power-nodes`; `container-knowledge` is absent.
- `scripts/lib/save_modules.lua:264` — registry validation reports any dependency absent from both the Lua registry and that mirror as unregistered.
- `test-headless/Test/Headless/Lua/SaveModules.hs:596` — the dependency test proves one old Haskell id (`units`) and one fake id, but does not compare the mirror with the authoritative Haskell registry or exercise the new component.
- The direct probe registered a valid synthetic component with `deps={"container-knowledge"}` and received exactly one static error naming that real component as unregistered.

**Handoff context:**

- **Current behavior:** Lua modules may declare dependencies on the older Haskell save components, but declaring the real `container-knowledge` dependency makes `registryStaticErrors` fail every save/load operation.
- **Expected behavior:** Every id in the authoritative Haskell component registry is accepted by Lua dependency validation, preferably through a drift-proof generated/introspected set or an audit that compares the mirror exactly.
- **Scope and constraints:** Introduced by PR #1126 / issue #1087 when the new Haskell component was added. Preserve rejection of genuinely unknown ids and the distinction between Haskell dependencies and Lua-to-Lua ordering edges.
- **Remaining uncertainty:** No current Lua component declares this particular dependency, so the defect is latent until a persistent module needs container-knowledge ordering or documents the dependency honestly.

### PRR-2. Modern saves can silently omit all container knowledge

> **Captured note:** Distinguish a genuinely pre-#1087 envelope from a current envelope that lost its `container-knowledge` descriptor and payload. Making the component permanently optional admits old saves, but it also turns complete omission from any newly written save into a successful load that resets every remembered container to never-inspected.

**Verification:** Verified at the codec boundary — stripping the complete component from a freshly encoded current snapshot is accepted and yields an empty knowledge map; no field at this decision point proves that the input predates the feature.

**Evidence:**

- `src/World/Save/Component/Knowledge.hs:146` — the codec declares `csRequired = False` without an introduction-version or legacy-only condition.
- `src/World/Save/Component.hs:109` — registry assembly explicitly leaves every page at the empty default whenever the component is absent.
- `src/World/Save/Component.hs:134` — the current writer nevertheless emits every registered component, including this one, even when its map is empty.
- `src/World/Save/Component/Types.hs:230` — absence skips both decode validation and the assembly fold solely because `ccRequired` is false.
- `test-headless/Test/Headless/Building/Knowledge.hs:742` — the legacy-absence test starts with `encodeFor knowledgeSnapshot`, removes `container-knowledge` from those current bytes, and expects successful decode to `emptyContainerKnowledge`; the bytes carry no asserted provenance distinguishing that mutation from an old save.
- `test-headless/Test/Headless/World/Save/Components.hs:706` — the registry gate permanently pins this as the single optional gameplay component rather than making absence conditional on a recognized legacy shape.

**Handoff context:**

- **Current behavior:** Deleting both the descriptor and payload from an otherwise current, valid save is interpreted as “no container has ever been inspected,” even when the omitted component held populated records.
- **Expected behavior:** Supported historical saves still default honestly, while a save from a format generation that should contain this component fails closed if it is wholly absent.
- **Scope and constraints:** Surfaced in PR #1126 / issue #1087. Preserve all tracked pre-A3 baselines and the existing rule that a present malformed/unsupported payload is fatal. A fix may need an envelope-era discriminator, a recognized-legacy migration path, or a component-introduction contract rather than simply flipping `required` to true.
- **Remaining uncertainty:** Ordinary production writes always include the component, so reaching this path requires an incomplete/corrupted envelope or another writer; the impact once reached is confirmed data loss rather than a load error.

### PRR-3. Non-finite container observations pass save validation

> **Captured note:** Reject non-finite remembered weights and reveal times, not only negative ones. IEEE `NaN` makes both `< 0` checks false, and positive infinity also passes, even though neither value can be produced by a valid finite observation or safely exposed to UI age/weight calculations.

**Verification:** Verified — evaluating `ccValidate containerKnowledgeCodec` on a record with `crdStoredWeight = 0/0` and `crdRevealedAt = 1/0` returns `[]` in the current library.

**Evidence:**

- `src/World/Save/Component/Knowledge.hs:102` — the validator documents non-negative mass and monotonically advancing game time as component invariants.
- `src/World/Save/Component/Knowledge.hs:128` — remembered weight is rejected only when `crdStoredWeight < 0`.
- `src/World/Save/Component/Knowledge.hs:133` — reveal time is rejected only when `crdRevealedAt < 0`.
- `src/Engine/Scripting/Lua/API/Buildings/Knowledge.hs:91` — accepted stored weight is pushed directly to Lua as a number.
- `src/Engine/Scripting/Lua/API/Buildings/Knowledge.hs:96` — accepted reveal time is likewise pushed directly to Lua.
- `test-headless/Test/Headless/Building/Knowledge.hs:759` — malformed coverage corrupts the cereal byte stream; it does not construct decodable non-finite scalar values that reach component validation.

**Handoff context:**

- **Current behavior:** A structurally decodable component carrying `NaN` or positive infinity passes validation and publishes the values into the remembered record and Lua query surface.
- **Expected behavior:** Persisted observations accept only finite, non-negative weights and finite, non-negative reveal times; corrupt values fail the all-or-nothing load before live state changes.
- **Scope and constraints:** Introduced with PR #1126 / issue #1087's new floating-point DTO fields. Do not rederive historical stored weight from current item definitions; finiteness validation is independent of that deliberate compatibility rule.
- **Remaining uncertainty:** The production observation path normally derives both values from finite engine state, so the reproduced ingress is corrupted/adversarial save data rather than ordinary gameplay.

## 2. Lua load application

### PRR-4. Restored-entity apply context is mutable across components

> **Captured note:** Enforce issue #900's read-only restored-entity context. `applyAll` hands every component the same ordinary Lua table, so one component can delete or rewrite unit/building membership or owner pages and thereby change which rows a later component applies.

**Verification:** Verified — a direct two-component probe had the first component delete `entities.unit[7]`; the later component observed the deletion and failed with the injected `CONTEXT_MUTATED` marker.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Save/Bridge.hs:319` — Haskell pushes ordinary nested Lua tables for `unit`, `building`, and `unitPage`, with no read-only wrapper.
- `scripts/lib/save_modules.lua:739` — `prepareLoad` stores that table directly in `_pendingEntities`.
- `scripts/lib/save_modules.lua:947` — `applyAll` reads the same stored table once for the whole forward pass.
- `scripts/lib/save_modules.lua:1025` — the identical `entities` object is passed to every registered component's `apply` callback.
- `scripts/lib/save_modules.lua:826` — `applyEntityRows` trusts membership from the mutable per-kind subtable, so an earlier mutation changes absent-owner filtering.
- `test-headless/Test/Headless/Lua/SaveModules.hs:1389` — per-entity tests exercise filtering, rollback, page lookup, and contextless compatibility, but no component attempts to mutate the shared context or prove it read-only.

**Handoff context:**

- **Current behavior:** Apply order can affect restored ownership: a buggy or future component may corrupt the context and cause sibling rows to be dropped, retained, or assigned the wrong owner page despite Haskell having supplied the correct `KnownEntities` value.
- **Expected behavior:** Every component observes one immutable restored-entity snapshot, or receives an isolated copy whose mutation cannot affect another component or the registry transaction.
- **Scope and constraints:** Surfaced in PR #1089 / issue #900 requirement 1, which explicitly called the generic context read-only. Preserve contextless rollback semantics: rollback applies old snapshots with no restored-session filter.
- **Remaining uncertainty:** The two current per-entity production callbacks only read the context, so the demonstrated cross-component contamination needs a faulty/new callback rather than occurring in today's normal load.

## 3. Language provenance

### PRR-5. Persisted language provenance accepts unconstructible versions

> **Captured note:** Validate every persisted `GeneratorVersion` before publishing a world identity. The DTO accepts any `Int` and `world-pages` validation never checks it against `supportedGeneratorVersions`, so a save can successfully load provenance that `generateProfile` immediately rejects as unconstructible.

**Verification:** Partially verified — the unrestricted decode-to-live path and missing validation are confirmed statically, and `generateProfile` rejects every value outside 1–5; no current production writer emits such a value.

**Evidence:**

- `src/World/Save/Component/Page.hs:244` — `LanguageProvenanceDTO.lpdVersion` is an unrestricted serialized `Int`.
- `src/World/Save/Component/Page.hs:255` — decode wraps that integer directly in `GeneratorVersion` without validation.
- `src/World/Save/Component/Page.hs:825` — `validatePages` checks page ids and location allocators, but not language provenance or generator support.
- `src/Language/Generated/Profile.hs:32` — profile reconstruction accepts versions 1–5 and returns `UnsupportedGeneratorVersion` for every other integer.
- `src/Language/Generated/Types.hs:100` — the version list is explicitly defined as every version a save may carry and the set the dispatcher can build.
- `test-headless/Test/Headless/World/Identity.hs:166` — the recovered-provenance test proves reconstruction only for the valid fixture version; save-component coverage has no unsupported-version rejection case.

**Handoff context:**

- **Current behavior:** A decoded page may advertise a language seed/version through `world.getLanguageProvenance` even though name suggestion, naming, or etymology reconstruction for that provenance fails as unsupported.
- **Expected behavior:** A successfully loaded non-absent provenance is reconstructible by this build, matching #1092's core guarantee; unsupported persisted versions should fail descriptively before publication rather than creating a half-usable named world.
- **Scope and constraints:** Originated in PR #1123 / issue #1092. Historical versions must remain accepted and names/glosses must never be rerendered. Later schemas also embed provenance in etymology and generated entity-name records, so the processor should inventory every decode ingress rather than patching only the original identity field.
- **Remaining uncertainty:** Forward-save compatibility policy may intentionally permit loading rendered text while language tooling is unavailable; if so, the documentation and query/result types need to state that weaker contract because it conflicts with the current “reconstructible from both together” invariant.
