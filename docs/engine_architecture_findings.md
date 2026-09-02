# Engine Architecture and Operational Findings

This report captures a read-only critique of Synarchy’s current engine architecture, concentrating on ownership enforcement, scheduling, cross-language contracts, persistence, validation coverage, and scale visibility. Texture-system telemetry is excluded because implementation work on that instrumentation was already in progress when this report was drafted.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The review began from a conversational architecture critique and a follow-up request to identify valuable telemetry surfaces. It inspected the current primary checkout at `master@8070b82c`, including the core monad and environment, capability projections, queue and worker-loop infrastructure, world rendering, the Haskell/Lua boundary, persistence components, save/load coordination, Cabal test definitions, and behavior-probe classification.

Repository-wide counts were used only to establish scale and were not treated as defects by themselves. `python3 tools/ci_probes.py --status` was run to inspect the current probe classifications. No implementation was changed, no full Hspec or CI gate was run, and no graphical/GPU session or scale benchmark was executed.

## Status

- [x] EA-1. Capability projections narrow visibility without enforcing ownership — [#1890]
- [x] EA-2. Inter-thread queues have neither workload bounds nor backlog telemetry — [#1910]
- [x] EA-3. The Lua API is a large manually maintained runtime ABI — [#1995]
- [x] EA-4. Persistence discards structured information and decodes components twice — [#1919]
- [ ] EA-5. Integrated and graphical behavior remains mostly outside blocking validation — [deferred]: #1426 census 3/91 migrated; no GPU host exists
- [x] EA-6. Dynamic scene assembly relies on unmeasured small-colony assumptions — [#1921]

---

## Ownership and scheduling

### [#1890] EA-1. Capability projections narrow visibility without enforcing ownership

The completed `EngineEnv` capability split materially reduces accidental field reach, but its records remain projections of publicly accessible mutable containers. They enforce which fields a consumer can name, not which operations or thread roles are permitted on those fields. Correct writer ownership and multi-field protocols therefore continue to depend on documentation, audits, and reviewer knowledge.

**Evidence:**

- `src/Engine/Core/Monad.hs:19-35` — `EngineM` is concretely tied to `EngineEnv` and `EngineState`; capability narrowing does not vary the monad environment.
- `docs/engineenv_capability_inventory.md:39-47` — the shared environment currently contains exactly 83 fields.
- `docs/engineenv_capability_inventory.md:132-151` — the canonical convention requires exported capability records whose fields alias the exact `IORef`, `TVar`, and `Queue` handles held by `EngineEnv`.
- `src/Engine/Core/Capability/RenderHandoff.hs:87-152` — one representative capability publicly exposes seven live mutable handles and constructs them through direct projection.

**Handoff context:**

- **Current behavior:** Capability records provide compile-time field-set narrowing, while mutation authority and thread-role restrictions remain external contracts over publicly accessible mutable handles.
- **Expected direction:** At least the most correctness-sensitive domains should make owner-only mutation structurally distinct from command-producing or snapshot-reading access.
- **Scope and constraints:** Preserve the completed capability inventory and its audits. An incremental pilot around one domain such as `WorldSim` or `UnitCombat` is preferable to another repository-wide environment rewrite. Any new capability or `EngineEnv` field must follow the documented approval and inventory procedure.
- **Remaining uncertainty:** This review did not enumerate every current mutation site or prove a live unauthorized-write defect. The finding concerns what the boundary can enforce, not evidence that every present consumer violates its documented role.

### [#1910] EA-2. Inter-thread queues have neither workload bounds nor backlog telemetry

The common queue abstraction is an unbounded STM `TQueue`, and several important consumers drain until empty or flush the entire queue at once. No shared queue surface reports depth, high-water mark, message age, processing rate, or a per-tick budget. A producer that outpaces its consumer can therefore turn into unbounded memory growth, long-tail latency, or starvation without an engine-level diagnostic identifying the cause.

**Evidence:**

- `src/Engine/Core/Queue.hs:8-39` — `Queue` is a thin wrapper over `TQueue`; writes are unbounded and the public operations expose neither size nor telemetry.
- `src/World/Thread.hs:97-117` — every world tick builds world quads, sleeps a fixed 16.6 ms after completing its work, and recursively drains world commands until the queue is empty.
- `src/Engine/Input/Thread/Dispatch.hs:48-74` — input processing likewise continues recursively until no event remains.
- `src/Engine/Scripting/Lua/Message.hs:47-81` — the render-side Lua consumer atomically flushes the complete queue and processes the entire captured list in one frame.

**Handoff context:**

- **Current behavior:** FIFO ordering is strong and several barrier protocols rely on it, but admission, processing budgets, coalescing, backlog observability, and overload behavior are not part of the queue contract.
- **Expected direction:** Queue users should have bounded, observable workload behavior appropriate to each message family, including depth and oldest-message age at minimum.
- **Scope and constraints:** Preserve input-barrier FIFO semantics, save/load publication ordering, and messages that cannot safely be dropped. Replaceable state publications may be coalescible, while commands and resource-lifecycle messages may require lossless bounded handling. Instrumentation must be cheap enough to remain enabled during ordinary engine runs.
- **Remaining uncertainty:** No sustained-load experiment was run, so actual backlog sizes and the first queue to saturate at realistic colony scale remain unknown.

---

## Runtime interfaces and persistence

### [#1995] EA-3. The Lua API is a large manually maintained runtime ABI

The Haskell/Lua boundary now comprises hundreds of functions registered by string, with argument conventions and result-table fields implemented manually. The registration structure is sensibly divided by namespace, but there is no single machine-readable contract from which registration, documentation, result shapes, and compatibility checks are derived.

**Evidence:**

- `src/Engine/Scripting/Lua/API.hs:1-45` — the top-level registrar sequences the namespace-specific API modules against a full `EngineEnv`.
- `src/Engine/Scripting/Lua/API/Register/UI.hs:10-103` — one namespace manually registers dozens of string-named functions into a global table.
- `src/Engine/Scripting/Lua/API/Internal.hs:28-40` — the common wrapper accepts only a raw function name and action, installs it by `Lua.setfield`, and converts uncaught Haskell failures into Lua errors.
- A repository-wide count at draft time found approximately 606 `registerLuaFunction` registrations and 707 `Lua.setfield` result-field writes under the Lua API.

**Handoff context:**

- **Current behavior:** Function existence, argument interpretation, return-table shape, documentation, and caller expectations can drift independently and are primarily protected by focused tests and probes.
- **Expected direction:** Public Lua functions should have one declarative contract capable of driving registration metadata, documentation, shape validation, and telemetry identity. High-frequency consumers should have bulk or batched interfaces where fine-grained calls dominate tick cost.
- **Scope and constraints:** Preserve Lua as the policy, UI, and high-level orchestration layer. Do not require a wholesale generated binding rewrite before proving the shape on one namespace. Telemetry labels must be fully qualified and low-cardinality; arguments and entity IDs must not become labels.
- **Remaining uncertainty:** No runtime call-frequency or duration profile was captured, so the highest-volume functions and the present cost of boundary crossings are unknown.

### [#1919] EA-4. Persistence discards structured information and decodes components twice

The component persistence design provides strong versioning and validation, but its type-erased registry validates a component by decoding it and then decodes the same component again during assembly. Separately, the outer load API reconstructs structured progress by searching rendered error text for phase names and a compatibility phrase, even though the load-status subsystem itself already models phases explicitly.

**Evidence:**

- `src/World/Save/Component/Types.hs:167-199` — `RegisteredComponent` stores separate validation and application functions, with the application contract explicitly documenting that it decodes again.
- `src/World/Save/Component/Types.hs:221-239` — both `rcDecodeErrors` and `rcApply` independently invoke `decodeComponentValue`.
- `src/World/Save/Serialize.hs:175-209` — `phaseFor` identifies load progress through `Text.isInfixOf` checks over rendered diagnostic text.
- `src/Engine/Load/Status.hs:48-78,110-137` — load phases and the failure phase are already represented as structured data elsewhere in the transaction model.

**Handoff context:**

- **Current behavior:** Component bytes are decoded twice on a successful load, and wording changes in lower-layer diagnostics can silently reduce the accuracy of reported load progress.
- **Expected direction:** A component should decode and validate once before contributing its retained value to assembly, and structured failure phase information should survive through storage selection and the public load boundary.
- **Scope and constraints:** Preserve the frozen DTO rules, per-component migrations, compatibility fixtures, deterministic error ordering, all-or-nothing assembly, and human-readable rendered diagnostics. Rendering should occur at the presentation edge rather than replace structured error data internally.
- **Remaining uncertainty:** The performance impact of duplicate decoding has not been measured against representative large saves. The current text parser deliberately degrades conservatively, so no false-success behavior was identified.

---

## Validation and scale

### [deferred] EA-5. Integrated and graphical behavior remains mostly outside blocking validation

> **Deferred:** Both halves of this finding's expected direction are currently unscopable, for different reasons. **Probe promotion** is owned by open epic #1426, whose arc landed the machinery — #1441 reports reliability-qualified candidates and explicitly leaves breadth, cost, runner support and the promotion decision to a human — but that report yields nothing yet: `docs/probe_census.json` (`probe-census/v3`, 91 rows) carries 88 `legacy` rows against 3 on `probe-result/v1`, and #1441 requires `probe-result/v1` plus a complete current cohort. **Periodic GPU execution** has no host: the project owner confirmed on 2026-08-30 that the only graphics-capable machine is a laptop that is frequently powered off, so there is no runner to schedule against at any cadence. Clears when enough census rows carry `probe-result/v1` for #1441's report to produce real candidates, or when a GPU-capable runner becomes available — whichever comes first; the residual simulated-time vertical-scenario work can then be scoped against what those actually leave uncovered.

Headless logic coverage is extensive and the probe inventory has improved substantially, including the removal of all currently classified base-failing probes. Nevertheless, only a minority of registered behavior probes are suitable for blocking CI, while real graphical tests require manual execution on a graphics-capable machine.

**Evidence:**

- `tools/ci_probes.py:18-22` — only broad, cheap, deterministic smoke probes are admitted to CI; flaky, scenario-heavy, targeted, worldgen-heavy, GPU-dependent, and base-failing probes remain manual.
- `tools/ci_probes.py:52-118` — the current CI-eligible set contains 13 probes.
- `tools/ci_probes.py:120-165` — every remaining probe is maintained in a separate reason-carrying manual-only registry; `--status` reported 78 manual-only probes out of 91 total.
- `synarchy.cabal:1010-1018` — the graphical suite errors before Hspec on a machine without a display and is therefore only compiled by automated gates.
- `tools/ci-local.sh:81-98` — the local full gate builds both suites but executes only the headless suite.

**Handoff context:**

- **Current behavior:** Pure logic, headless integration, deterministic world output, and selected engine scenarios are strongly checked. Many complete gameplay, timing-sensitive, and rendered UI paths still rely on deliberate manual probes.
- **Expected direction:** The engine should gain deterministic simulated-time vertical scenarios and periodic execution on a supported GPU environment, increasing integration confidence without promoting known timing-flaky probes directly into blocking CI.
- **Scope and constraints:** Preserve the honest CI-eligibility criteria and do not mask nondeterminism with retries or weaken assertions. GPU coverage can be periodic or pre-release rather than necessarily per pull request.
- **Remaining uncertainty:** No full suite, manual-probe sweep, offscreen session, or graphical run was performed for this draft, so it does not claim a current validation failure.

### [#1921] EA-6. Dynamic scene assembly relies on unmeasured small-colony assumptions

Static terrain rendering has a strong cache and invalidation design, but dynamic world quads are regenerated on every world tick across visible pages. Comments explicitly justify some paths as cheap because they currently contain only a handful of objects, and the package defines no continuous performance-benchmark component establishing the colony sizes at which those assumptions remain safe.

**Evidence:**

- `src/World/Thread.hs:97-105` — `updateWorldTiles` is called every world tick before a fixed post-work sleep.
- `src/World/Render.hs:98-183` — ground items, spoil, blood, units, buildings, and structures are regenerated through the dynamic path, including visible-page traversals.
- `src/World/Render.hs:152-159` — unit regeneration is described as cheap because it handles a “handful of sprites.”
- `src/World/Render.hs:232-235` — the combined dynamic run is described as small before the frame loop sorts and merges it.
- `synarchy.cabal:162-1060` — the package defines the library, executable, and two test suites but no benchmark stanza or checked scale target.

**Handoff context:**

- **Current behavior:** The renderer avoids unnecessary static-terrain rebuilds, but dynamic CPU scene assembly is still sized by present content density rather than an explicit measured colony target.
- **Expected direction:** Define representative entity-density and scene-complexity targets, instrument scanned and emitted object/quad counts plus per-category duration, and optimize only the paths measurements show exceeding their budgets.
- **Scope and constraints:** Preserve static cache correctness, camera responsiveness, depth ordering, and cross-thread immutable publication. Texture-system telemetry alone will not expose CPU scene-assembly work. Avoid speculative spatial-index or dirty-set changes before collecting measurements.
- **Remaining uncertainty:** No scale benchmark or profile was run, so this is a verified observability and capacity-planning gap rather than a demonstrated current frame-rate defect.
