# Lua API contract design

The Haskell/Lua boundary is a 613-function runtime ABI whose every element —
name, arity, argument types, return-table shape — exists only inside the
implementation that serves it. Nothing derives registration, documentation,
shape validation, or telemetry identity from a shared contract, and nothing
checks that a name a Lua script calls is a name the engine registers. A verb
that drifts fails at the moment a player reaches it, not at build time.

Source: `docs/engine_architecture_findings.md` finding EA-3, approved as an
epic rather than a single issue on 2026-08-30.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Give the Lua API a checked contract instead of a manually maintained ABI — [#1995]
- [x] LAC-1. Gate every Lua call site against the engine's real registration set — [#1996]
- [ ] LAC-2. Pilot a declarative registration contract on the `UI` namespace
- [ ] LAC-3. Profile the boundary and record the rollout verdict

## Epic contract

- **Goal:** A Lua verb that no registration provides can no longer reach
  `master`, one namespace proves a declarative contract that derives its own
  registration and shape metadata, and an evidence-backed verdict says whether
  that shape is worth the remaining 26 namespaces.
- **Done when:** (1) a blocking gate rejects a call site naming an unregistered
  verb, with no false positive on the current tree; (2) one namespace's
  registrations are derived from a declarative contract that the same gate
  consumes; and (3) a measured call-frequency and duration profile plus a
  recorded verdict on further rollout and on any bulk interface exist in
  `docs/lua_api_contract.md`.
- **Users and operators:** Engine and gameplay developers writing Lua, and
  reviewers who currently have no mechanical signal that a boundary crossing is
  well-formed.
- **Arc label:** `lua` (existing), plus `tech-debt`.

## Current state and evidence

Verified at `master@1845ac29`.

**The registration surface.** `src/Engine/Scripting/Lua/API.hs:31-45` sequences
13 registrar modules under one `Lua.runWith`, each taking a full `EngineEnv`.
Those 13 modules install **27** Lua global namespaces — several install more
than one (`Craft.hs` → `craft`/`power`/`repair`; `Camera.hs` →
`camera`/`combat`/`injury`/`thought`; `Designation.hs` →
`structure`/`construction`/`chop`/`till`/`plant`). Distribution:

| Namespace | Verbs | | Namespace | Verbs | | Namespace | Verbs |
|---|---|---|---|---|---|---|---|
| `engine` | 111 | | `building` | 34 | | `chop` | 8 |
| `unit` | 111 | | `camera` | 18 | | `till` | 8 |
| `world` | 103 | | `structure` | 17 | | `equipment` | 8 |
| `UI` | 80 | | `craft` | 15 | | `debug` | 7 |
| `construction` | 14 | | `item` | 15 | | `flora` | 7 |
| `blood` | 11 | | `input` | 9 | | `faction` | 6 |
| `power` | 9 | | `plant` | 6 | | `combat` | 3 |
| `repair` | 3 | | `infection` | 2 | | `injury` | 2 |
| `loot` | 2 | | `substance` | 2 | | `thought` | 2 |

Total 613 registrations. `src/Engine/Scripting/Lua/` additionally carries **719**
`Lua.setfield` result-field writes, which is the return-shape half of the same
problem.

**Re-verified at `master@ff692087` (2026-08-30).** The tree has grown since
`1845ac29`: **617** registrations and **742** `Lua.setfield` writes. The 13
registrar modules and 27 namespaces are unchanged, and each namespace is still
exactly one `Lua.newtable` … `Lua.setglobal (Lua.Name "<ns>")` block. The
per-namespace verb table above is otherwise current. (`shellSandbox`, a 28th
`setglobal`, is `API/Shell.hs`'s sandbox table, not an API namespace; it is
indexed only dynamically and registers no verbs.)

**Return-shape distribution.** Which namespace actually returns the richest
result tables, measured as `Lua.setfield` writes in the implementation modules
each registrar draws on:

| Namespace | Verbs | `Lua.setfield` writes | Implementation modules | Impl. LOC |
|---|---|---|---|---|
| `unit` | 111 | **150** | `API/Units/` | 7,231 |
| `world` (+ 1 sibling) | 113 | **181** | `API/World/`, `API/WorldQuery/`, `API/Forage/`, `API/Flora.hs` | 4,644 + |
| `equipment` | 8 | 65 | `API/Equipment/` | — |
| **`UI`** | **80** | **35** | `API/UI/` (8 modules) | **1,973** |
| `item` | 15 | 33+ | `API/Items/`, `API/Blood.hs`, `API/LootTables.hs` | — |
| `engine` | 111 | ~150 across ~25 modules | `API/Core.hs`, `API/Save.hs`, … | — |

`UI` is the only namespace whose registrar maps 1:1 onto one implementation
subtree (`Register/UI.hs` imports exactly `API.UI`, which re-exports exactly
`API/UI/*`), so its 35 is exact. `unit`'s 150 is likewise all its own —
`loadUnitYamlFn`, the one `API/Units/` function `engine` also uses, writes no
result fields. Other namespaces share implementation modules across registrars,
so their figures are attributions rather than partitions.

Two further facts bear on the pilot choice. `UI` is by a wide margin the
smallest self-contained conversion (1,973 implementation lines behind one
re-export module, versus 7,231 for `Units/`). And `UI` returns **bare multiple
values** as well as tables — `UI.placePopup(anchorX, …) -> x, y, flipped`
(`API/UI/Placement.hs:39-73`) pushes three unnamed results — so a descriptor
piloted there must model both return shapes, which a table-only namespace would
not exercise.

**The contract carrier.** `src/Engine/Scripting/Lua/API/Internal.hs:29-33`:

```haskell
registerLuaFunction ∷ BS.ByteString → Lua.LuaE Lua.Exception Lua.NumResults
                    → Lua.LuaE Lua.Exception ()
registerLuaFunction name action = do
    Lua.pushHaskellFunction (action `Catch.catch` handler)
    Lua.setfield (-2) (Lua.Name name)
```

A raw byte string and an action. Arity, argument types, and return shape are
implied by the action's body — every `Lua.tointeger 1` / `Lua.tonumber 2` is a
positional convention no caller can discover mechanically.

**Nothing observes the surface.** A `grep` across all 202 `tools/*.py` finds no
audit of registration names or call sites. No Lua API reference document exists.
The one Lua-touching tool (`text_encoding_probe.py`) covers unrelated behavior,
and the engine-free `Test.Headless.Lua.MessageStrictness` spec that replaced the
former #622 probe (#2161) observes message-field strictness, not registrations.

**The drift is real, not hypothetical.** `scripts/ui/bar.lua:296-297` called
`UI.setSpriteColor`, which no registration provides — the registered verb is
`UI.setColor`. Filed as **#1914**. It survived since commit `9af2585c` because
`bar.setFillColor` has no callers yet; the first caller would have hit
`attempt to call a nil value` at runtime. This is the exact failure class LAC-1
exists to make impossible.

**Naive matching does not work.** A throwaway regex over `scripts/` (3,880
namespaced call sites) reported 17 unresolved names. Sixteen were false
positives from local Lua tables shadowing a registered global name:
`unitAi.till.*` and `unitAi.plant.*` (sub-tables on the `unitAi` singleton, where
the regex matched the `till.key` tail), `local item = startupLoader.items[idx]`
shadowing the `item` global, and `local combat = require("scripts.unit_ai_combat")`
shadowing the `combat` global. Exactly one — `UI.setSpriteColor` — was real. A
gate at that precision would be discarded within a week.

**Prior art on this exact tool shape is expensive.** Three PRs building
regex-based static audits in this repository burned review rounds one syntax
variant at a time: PR #704 (12 rounds; fixed-width regex windows bridging
adjacent calls), PR #1128 (4 rounds; ended only by replacing per-line matching
with a real lexer plus a fail-loud catch-all), and PR #1309 (14 rounds; a
hand-rolled format parser inside a CI gate). LAC-1 inherits that lesson as a
hard requirement rather than a hope.

**Tracker.** No open or closed issue covers the Lua ABI surface (five
differently-phrased `--state all` searches plus all 35 open titles). #618 and
#622 hardened individual Lua API call paths; epic #1890's three children are
`EngineEnv` capability read/write authority only.

## Desired experience

A developer adding or renaming a Lua verb learns at `make ci` time — not from a
player-reachable crash — that a script still names the old one. A developer
reading a namespace can see what each verb takes and returns without reading the
Haskell body. A reviewer gets a mechanical signal that a boundary crossing is
well-formed. None of that requires Lua to stop being the policy, UI, and
orchestration layer, and none of it requires 27 namespaces to change at once.

## Scope

### In scope

- Detecting call sites that name a verb no registration provides.
- One namespace's registrations derived from a declarative contract.
- A call-frequency and duration profile of the boundary.
- A recorded verdict on rolling the contract out further and on whether any bulk
  or batched interface is justified.

### Out of scope

- A wholesale generated-binding rewrite across all 27 namespaces. The finding's
  own constraint: prove the shape on one namespace first.
- Changing Lua's role. Lua stays the policy, UI, and high-level orchestration
  layer.
- Fixing #1914, which is filed and scoped separately.
- Rewriting the 719 `Lua.setfield` result-shape writes, beyond whatever the one
  piloted namespace covers.

## Design

The arc is deliberately ordered detection → contract → measurement, because each
stage supplies the thing the next one cannot fabricate.

**LAC-1 (detection)** needs no design decision to be useful: the registered set
is mechanically extractable today, and the one live defect proves the gap is
real. It also produces the extractor LAC-2 must consume rather than duplicate.

**LAC-2 (contract)** converts `UI` to a Haskell descriptor record (D-3, D-4).
Because the descriptor is not readable from Python, LAC-1's extractor is not
replaced — it gains a second registration shape to recognize, and must report the
identical registered set for `UI` before and after the conversion. That
before/after equality is the pilot's own correctness check. What is still open is
how a converted `UI` coexists with 26 unconverted namespaces without dragging all
613 call sites into one PR (Q-4).

**LAC-3 (measurement)** exists because the finding's "bulk or batched interfaces
where fine-grained calls dominate tick cost" is unjustifiable without a profile,
and because a rollout verdict grounded in nothing is how a 27-namespace rewrite
gets adopted by default rather than on evidence. It stops at the measurement and
the verdict (D-6); anything the numbers justify is filed afterwards with those
numbers in hand.

## Decisions

### D-1. Three dependency-ordered slices: detect, then pilot, then measure

Approved 2026-08-30. The arc lands as LAC-1 → LAC-2 → LAC-3 in strict order, not
as one issue and not in parallel. LAC-1's extractor is LAC-2's input; LAC-3's
profile is what makes the rollout verdict and any bulk-interface proposal
evidence-backed rather than speculative.

**Consequences:** the epic has no omnibus implementation PR; a slice that
discovers its successor is unnecessary records that as the verdict rather than
proceeding.

### D-2. LAC-1 rejects unresolvable calls only

Approved 2026-08-30. Resolves Q-1. The gate fails on exactly one condition: a
call site names a verb its namespace does not register. It does not flag
registered-but-uncalled verbs, and it does not check argument counts.

**Rationale:** this is the only form with a proven live instance (#1914) and the
only one that can reach zero false positives on the current tree, which is what
lets LAC-1 be blocking on its first PR rather than advisory. Reverse coverage
would need every non-`scripts/` caller enumerated — hspec suites, ~85 probes,
and the debug console all call verbs — and would otherwise report dead surface
that is not dead. Arity checking needs LAC-2's contract, so requiring it here
inverts the arc's order.

**Consequences:** dead ABI surface stays unmeasured for now; if LAC-3's profile
makes it interesting, it becomes a follow-up rather than a retrofit into LAC-1.

### D-3. The contract is a Haskell descriptor record

Approved 2026-08-30. Resolves Q-2. The registrar takes the verb's name, argument
kinds, return shape, and documentation as a data value alongside its action,
rather than a raw `ByteString` name.

**Rationale:** it is type-checked at the point of definition, introduces no new
file format and no codegen step, and cannot drift from the implementation it sits
beside. The external-manifest and generated-binding alternatives both create a
cross-language agreement problem — the same shape that took PR #1309 fourteen
review rounds — and generated bindings additionally require the wholesale rewrite
the source finding explicitly warns against.

**Consequences:** the descriptor is not directly readable from Python, so LAC-1's
extractor survives into LAC-2 rather than being replaced by a manifest read; it
gains a second shape to recognize. Rejected alternatives are preserved in Q-2's
history above.

### D-4. `UI` is the pilot namespace

Approved 2026-08-30. Resolves the first half of Q-3. LAC-2 converts the `UI`
namespace's 80 verbs.

**Rationale (corrected 2026-08-30).** Four grounds, none of them return-table
volume:

1. **It is the one live defect's home.** #1914's `UI.setSpriteColor` is the
   failure this whole arc exists to make impossible.
2. **It is the cleanest pilot boundary in the API.** `UI` is the only namespace
   whose registrar maps 1:1 onto one implementation subtree —
   `Register/UI.hs` imports exactly `API.UI`, which re-exports exactly
   `API/UI/*`. Every alternative shares implementation modules across
   registrars, or installs two namespaces from one module.
3. **It exercises BOTH return shapes.** `UI` returns result tables (35
   `Lua.setfield` writes, all in `API/UI/Property.hs`) and bare multiple values
   — `UI.placePopup(anchorX, …) -> x, y, flipped` pushes three unnamed results
   (`API/UI/Placement.hs:39-73`). A table-only namespace would leave the
   multi-value shape unmodelled, and the descriptor has to carry both.
4. **It is by far the smallest self-contained conversion** — 1,973
   implementation lines behind one re-export module, versus 7,231 for
   `API/Units/`. D-5 holds LAC-2 to one reviewable PR, and this is the
   candidate that keeps that comfortable.

**Alternatives and why they lost.** `unit` (150 result-field writes, 111 verbs)
would exercise return shapes roughly 4× harder, and was the strongest
challenger; it lost on D-5's one-PR constraint at 7,231 implementation lines.
`world` (181 writes) is richer still but its registrar installs two namespaces
from one module, making the pilot boundary the least clean of the three.
`craft` (15 verbs) and `power` (9) would land faster but are too small to
surface the problems the contract exists to solve. `engine` (111) is a
catch-all spanning FPS, config, textures, saves, keybinds and tutorials, making
it the least representative of a typical namespace.

**Superseded rationale.** This decision originally read that `UI`'s "verbs
return the richest result tables in the API". That was false — `UI` is fifth at
35 writes — and the original comparison never evaluated `unit` or `world`. The
conclusion survived the correction; the reasoning above replaces it. See
§Current state and evidence, "Return-shape distribution".

### D-5. A second registrar function, not a changed signature

Approved 2026-08-30. Resolves Q-4. LAC-2 adds a new descriptor-taking registrar
alongside the existing `registerLuaFunction`, which is left untouched. `UI`'s 80
call sites convert; the other 26 namespaces' 533 call sites do not change.

**Rationale:** it is the only option that keeps LAC-2 to one reviewable PR.
Changing `registerLuaFunction`'s signature drags all 613 sites into that PR and
forces 26 namespaces to carry placeholder descriptors that assert nothing —
contradicting the arc's prove-it-on-one-namespace constraint. A
default-descriptor variant, where `registerLuaFunction` internally synthesizes a
name-only descriptor, was rejected for a subtler reason: it leaves one function
carrying two very different guarantees with nothing marking which sites are real,
so the gate cannot distinguish an unconverted verb from an under-specified one.

**Consequences:** the tree carries two registration conventions between LAC-2 and
whatever retires the old one. That is deliberate and bounded — D-7's verdict is
what decides whether the old convention is retired at all. LAC-1's gate must
recognize both shapes (already required by D-3).

### D-6. LAC-3 delivers the profile and the verdict, and stops

Approved 2026-08-30. Resolves Q-5. LAC-3 measures per-verb call counts and
durations and records the verdict. It does not build a bulk or batched
interface.

**Rationale:** the source finding's bulk-interface direction is unjustifiable
before the profile exists, and a slice whose second half is conditional on its
own first half cannot be sized or given acceptance criteria. Any batched
interface the numbers warrant becomes its own issue, filed with the measurement
in its background — which is a better issue than one written blind.

**Consequences:** the arc closes without a performance change. That is the
intended outcome: the arc's deliverable is a checked contract and the evidence to
decide what comes next, not an optimization.

### D-7. The verdict lands in a new `docs/lua_api_contract.md`

Approved 2026-08-30. Resolves Q-6. LAC-3 creates a durable document for the Lua
boundary — the registered surface, the descriptor convention, and the rollout
verdict — rather than writing into `docs/engine_contracts.md` or leaving the
verdict in this design document.

**Rationale:** it mirrors how `docs/engineenv_capability_inventory.md` serves the
capability arc, and gives future work a place to look for the boundary's contract
that is not a working design artifact. `engine_contracts.md` is already large and
frequently rewritten, which raises docs-lane conflict risk.

**Consequences:** the document lands through the docs lane, not through LAC-3's
implementation PR — an implementation PR carrying a tracked-doc edit costs its
approval label when the docs lane rewrites the same file.

## Open questions

*All resolved. Each entry below preserves the alternatives that lost.*

### Q-1. What exactly does LAC-1's gate reject, and how strict is it?

*Resolved by D-2.* Rejected alternatives: reverse coverage (untrustworthy
without enumerating non-`scripts/` callers) and arity checking (inverts the
arc's dependency order).

### Q-2. What carries the declarative contract in LAC-2?

*Resolved by D-3.* Rejected alternatives: an external YAML/JSON manifest read by
both the registrar and the gate — directly machine-readable and able to carry
documentation, but it drifts from the Haskell unless a second agreement gate
exists; and generated bindings from such a manifest — drift-free by
construction, but a new build step and a wholesale convention change.

### Q-3. Which namespace is the pilot?

*Resolved by D-4* (`UI`). The second half of the original question — whether
LAC-3 must deliver a bulk interface — is carried forward as Q-5. D-4's
rationale was corrected on 2026-08-30 after measurement contradicted it; the
choice was re-examined as Q-7 and `UI` was confirmed.

### Q-4. How do the piloted namespace and the other 26 coexist?

*Resolved by D-5.* Rejected alternatives: changing `registerLuaFunction`'s
signature (drags all 613 sites into one PR and forces 26 namespaces to carry
placeholder descriptors), and a default name-only descriptor synthesized inside
`registerLuaFunction` (one function carrying two guarantees, with nothing marking
which sites are real).

### Q-5. Does LAC-3 deliver a bulk interface, or only the profile and verdict?

*Resolved by D-6* — profile and verdict only. The rejected alternative was
building a batched interface within LAC-3 when the numbers warrant one, which
leaves the slice unsized until its own first half lands.

### Q-6. Where is LAC-3's verdict recorded?

*Resolved by D-7* — a new `docs/lua_api_contract.md`. Rejected alternatives: a
section in `docs/engine_contracts.md` (already large and frequently rewritten,
raising docs-lane conflict risk) and this design document (a working artifact,
not where a future developer looks for the boundary's contract).

### Q-7. Does `UI` stay the pilot now that its stated rationale is false?

*Resolved by D-4 (corrected 2026-08-30)* — yes, `UI` stays, on four grounds
that do not include return-table volume: the live defect's home, the only 1:1
registrar↔subtree boundary in the API, the only candidate exercising both
result tables and bare multiple values, and the smallest self-contained
conversion. Rejected alternatives: `unit` (150 result-field writes, the
strongest challenger, lost on D-5's one-PR constraint at 7,231 implementation
lines) and `world` (181 writes, but its registrar installs two namespaces from
one module).

## Verification strategy

- LAC-1's gate is mutation-tested before its first review round, per the
  repository's three prior regex-audit PRs: rename a registered verb and confirm
  its call sites fail; shadow a global with a local of the same name and confirm
  no false positive; isolate one of several sibling call sites; and make an
  unparsed-but-call-shaped construct a loud failure rather than a silent skip.
- A new `tools/*.py` audit must join both `tools/ci-local.sh` and exactly one
  of `ci.yml`'s two audited workers — `static-audits` for an engine-free gate,
  `test-and-audits` only if it needs a Cabal build product — or
  `tools/ci_parity_audit.py` fails.
- The gate must report zero findings on the current tree once #1914 lands, and
  must fail on the tree before it.
- LAC-2's piloted namespace keeps its existing hspec and probe coverage green
  with no behavior change; the contract is a representation change, not a
  semantic one.
- LAC-3's telemetry labels stay fully qualified and low-cardinality — namespace
  and verb name only, never arguments or entity IDs.

**Repository constraints that do not apply, checked deliberately.** No slice
touches the save format, a save component, or a `Serialize` enum, so no
`csVersion` bump, frozen DTO, or compatibility fixture is involved. No slice
touches worldgen output, so the tier-3 baseline re-capture and `world_check`
do not apply. D-3's descriptor is a compile-time representation with no runtime
behavior change, so no determinism surface moves. The one documentation
obligation is D-7's new `docs/lua_api_contract.md`, which lands through the docs
lane rather than an implementation PR.

## Delivery plan

### LAC-1. Gate every Lua call site against the engine's real registration set

- **Outcome:** A blocking audit that fails when a Lua call site names a verb no
  registration provides, and on nothing else (D-2).
- **Scope:** Extract the registered namespace→verb map from the registrar
  modules; extract namespaced call sites from `scripts/`, modelling local-name
  shadowing; fail on an unresolvable call; join `make ci` and CI.
- **Phase:** 1
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1
- **Acceptance signals:** Reports zero findings on the current tree; fails on a
  tree carrying #1914's defect; every mutation case in the verification strategy
  is covered by its own self-test.
- **Out of scope:** Arity or return-shape checking; reverse coverage; any change
  to `registerLuaFunction`.
- **Open questions:** `None`

### LAC-2. Pilot a declarative registration contract on the `UI` namespace

- **Outcome:** The `UI` namespace's 80 registrations carry a Haskell descriptor
  record, and LAC-1's gate recognizes that shape.
- **Scope:** The descriptor type and a new registrar function taking it (D-5);
  `UI`'s 80 verbs converted; the gate taught to extract from the descriptor shape
  as well as the raw-name shape. `registerLuaFunction` and the other 26
  namespaces' 533 call sites are untouched.
- **Phase:** 2
- **Depends on:** LAC-1
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-4, D-5
- **Acceptance signals:** `UI`'s verbs are unchanged behaviorally; the UI hspec
  suites and probes stay green; the gate reports the same registered set for
  `UI` before and after the conversion; the descriptor expresses both `UI`
  return shapes — `Property.hs`'s result tables and `UI.placePopup`'s bare
  multiple values.
- **Out of scope:** The other 26 namespaces; retiring `registerLuaFunction`;
  documentation generation; telemetry.
- **Open questions:** `None`

### LAC-3. Profile the boundary and record the rollout verdict

- **Outcome:** A measured call-frequency and duration profile of the Lua
  boundary, and a recorded verdict on further rollout and on any bulk interface.
- **Scope:** Per-verb call counts and durations under a representative session;
  the verdict and its evidence written to a new `docs/lua_api_contract.md` (D-7)
  through the docs lane.
- **Phase:** 3
- **Depends on:** LAC-2
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-6, D-7
- **Acceptance signals:** The profile names the highest-volume verbs with real
  numbers; the verdict cites them.
- **Out of scope:** Building any bulk or batched interface (D-6); acting on the
  verdict; rolling out to the remaining namespaces.
- **Open questions:** `None`
