# Capability mutation authority design

The `EngineEnv` capability split (epic #537, issues #889–#899) is complete and
narrows *which fields a module can name*. It does not narrow *what a module may
do with a field it can name*. Every capability record is a total projection
aliasing the same live `IORef`/`TVar`/`Queue` handles `EngineEnv` holds, so a
consumer that legitimately reads a field can also write it, regardless of the
writer role the inventory declares for it. This arc decides whether — and how —
mutation authority becomes a checked boundary rather than a documented one.

Source: `docs/engine_architecture_findings.md` finding EA-1, approved as an
epic rather than a single issue on 2026-08-29.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Make capability records enforce mutation authority, not just field visibility — [#1890]
- [x] CMA-1. Gate every capability field's write sites against a checked-in writing-module map — [#1892]
- [x] CMA-2. Give `ContentRegistries` a structural mutation-authority boundary — [#1896]
- [ ] CMA-3. Record the pilot's verdict and the rollout recommendation

## Epic contract

- **Goal:** Every capability field's writing modules are pinned by a blocking
  gate that fails on an undeclared writer or a stale declaration, and one
  capability additionally carries a structural boundary making a non-writer's
  mutation a compile error — with an explicit, evidence-backed verdict on
  whether that boundary is worth rolling out further.
- **Done when:** (1) the write-site gate runs in CI and `make ci`, rejecting both
  a write from a module outside a field's checked-in writing-module map and a
  mapped module that no longer writes that field (D-2a — module granularity, not
  §5's roles); (2) `ContentRegistries` carries the structural boundary (D-6a); and
  (3) the verdict and its evidence are recorded in
  `docs/engineenv_capability_inventory.md`. Rolling the boundary out to the
  remaining capabilities is deliberately a **separate arc**, opened only if the
  pilot earns it (D-3).
- **Users and operators:** Engine maintainers and any agent or reviewer changing
  multi-threaded state. No player-visible behavior changes; this is a
  correctness-boundary and review-cost arc.
- **Arc label:** `tech-debt` (existing), plus `epic` (existing).

## Current state and evidence

### The capability split enforces visibility, and says so itself

- `src/Engine/Core/Monad.hs:19-35` — `EngineM` is concretely tied to `EngineEnv`
  and `EngineState`; its Haddock states that capability narrowing happens
  "through the explicit projected records in `Engine.Core.Capability.*`
  (issues #537/#889), never by varying this monad's environment." There is no
  capability typeclass layer to hang authority off.
- `src/Engine/Core/Capability/WorldSim.hs` header, and the identical sentence in
  `docs/engineenv_capability_inventory.md` §5 `world-sim-render-handoff`:
  *"This record grants no new read or write authority — it only removes the
  ability to reach fields a world/sim consumer has no business touching."*
  The current boundary's limit is documented, not accidental.
- Thirteen record/view types across eight capability identifiers, exposing
  **103** accessors in total, each aliasing a live mutable handle.

### One structural boundary already exists, and it is narrow by construction

`docs/engineenv_capability_inventory.md` §3.1 (landed by #891) is the precedent:
a capability with a thread-private field is exposed as **two** interfaces — the
full record and a strictly narrower worker-safe view — and
`tools/engine_env_capability_audit.py` enforces it in CI and `make ci`, checking
`RENDER_MAIN_ONLY_MODULES` and `ENGINE_STATE_REF_OWNERS` in *both* directions so
a stale entry fails too. The doc's own words: *"This is enforced, not merely
documented."*

That mechanism gates **which modules may name a field**. It has no notion of
read versus write. Four of the five capability splits are §3.1 thread-privacy
splits; none is an authority split.

### The declared writer sets are prose, and nothing checks them against code

`tools/engine_env_capability_audit.py` (1,932 lines) validates §5's
Readers/Writers cells for **grammar and citation presence only** — a strict
cell grammar of backtick-quoted, slash-joined role names with one trailing
parenthetical, plus a check that a source-file citation appears somewhere in the
row. It contains no `writeIORef` / `modifyIORef` / `atomicModifyIORef'` handling
of any kind; a grep for those tokens in the tool returns nothing.

So the inventory can claim `pathingConfigRef` has no writers, or that
`worldQuadsRef` is written only by `WorldThread`, and a change that falsifies
either claim passes every gate. This is the same drift class issue #1669 closed
for the field *count*, still open for the ownership *claims*.

### §5 declares ROLES; a source scan yields MODULES; nothing maps between them

Established while attempting to draft CMA-1, and the reason D-2a amends D-2.

- The audit carries no role→module mapping. `RENDER_MAIN_ONLY_MODULES` (14
  modules) and `INPUT_LUA_ONLY_MODULES` are per-*capability* import allowlists
  for the two §3.1 boundaries. `THREAD_ROLES` is nine bare strings consumed only
  by the cell-grammar validator.
- The mapping is not well-defined at module granularity. §3.1 names
  `World.Render.BloodQuads` as deliberately dual-domain, and it writes
  `textureSystemRef` at `src/World/Render/BloodQuads.hs:111`. That field's §5 row
  disambiguates only in prose — annotating that BloodQuads' upload/dispose
  functions run on `MainRender`, "NOT the world thread's `updateWorldTiles`
  quad-building path" — and cites lines 76 and 161, not 111. The role is a
  property of the **function**, not the module.
- Of the 87 §5 rows: **43 declare two or more writer roles**, 41 exactly one, 3
  none.

### Passing the raw handle onward is the dominant idiom

Measured on `master@19af28ea`, across `src/` + `app/`, over all 103 capability
accessors: of **321** accessor uses, **208** read or write inline and **113
(35%) pass the handle onward** — into helper parameters, and into context
records that mix several capabilities:

```haskell
-- src/Building/Knowledge/Live.hs:102-105, as measured
{ coBuildings = bcBuildingManagerRef bld
, coWorlds    = wsWorldManagerRef sim
, coItems     = crItemManagerRef reg
, coGameTime  = wsGameTimeRef sim }

-- src/Building/Thread/Command.hs:80
forgetContainerEverywhere (wsWorldManagerRef sim) bid
```

This is the measurement D-7 turns on: any boundary drawn at the *record* is
gone the moment the `IORef` is extracted, and a third of all uses do exactly
that.

CMA-2 (#1896) has since landed, and `coItems` is the field it wrapped:
it now reads `coItems = crvItemManagerRef reg` with type
`ReadOnlyRef ItemManager`, so that one site is a pass-on the boundary
now survives rather than one it leaks through. The count above is the
`master@19af28ea` measurement and is left as measured; `coGameTime`
beside it is a `world-sim-render-handoff` field and stays a raw
`IORef`.

### `RenderHandoff`'s read-only surface is three relationships, not eight

Derived from §5's own Readers/Writers cells for the eight
`world-sim-render-handoff` handoff fields:

| Role | Read-only on |
|---|---|
| `LuaThread` | `worldPreviewGenerationRef` |
| `MainRender` | `worldQuadsRef` |
| `WorldThread` | `structureWallCatalogRef` |

`texPaletteRef` and `texPaletteHandlesRef` are read *and* written by both their
roles; `worldPreviewRef` and `zoomAtlasDataRef` are written by both theirs; and
`bloodDisposeQueue`'s only "reader" is `MainRender` *draining* it, which mutates
— a read-only view over it would be meaningless.

RenderHandoff's accessors appear in **6 modules**, with 13 inline uses and 4
pass-on sites (all four the same `enqueueBloodDisposalForPage` call shape).
This section records the role-level view that informed D-6; the module-level
view below is what superseded it in D-6a.

### The §6.1 exemption removes most of `RenderHandoff`'s pilot surface

D-4 exempts the 24 permanent full-access modules, and that interacts with D-6's
pilot choice in a way neither decision anticipated. Two of `RenderHandoff`'s
three role-level read-only relationships are held by §6.1 modules reading the
**raw** `EngineEnv`, not the capability record:

| Relationship | Reader | Access path | In scope? |
|---|---|---|---|
| `LuaThread` → `worldPreviewGenerationRef` | `Engine.Scripting.Lua.Thread.Dispatch:357` | raw `env` | No — §6.1 |
| `MainRender` → `worldQuadsRef` | `Engine.Loop.Frame:233` | raw `env` | No — §6.1 |
| `WorldThread` → `structureWallCatalogRef` | `Structure.Render:114` | `rhStructureWallCatalogRef` | Yes |

A newtype on the capability record cannot reach a module that reads the raw
field, so those two are out of scope twice over.

Measured at **module** granularity — which is what the mechanism keys on, and
which differs from the role view — the in-scope read-only surface per capability,
§6.1 excluded, is:

| Capability | Module-field pairs | Fields | Consumer modules |
|---|---|---|---|
| `WorldSim` | 36 | 7 | ~30 |
| `RenderView` | 31 | 9 | ~12 |
| `ContentRegistries` | 7 | 4 | ~5 |
| `RenderHandoff` | 3 | 3 | 1 (`Structure.Render`) |

Note `RenderHandoff`'s three in-scope pairs are `structureWallCatalogRef`,
`texPaletteRef` and `texPaletteHandlesRef` — the latter two look read-write at
role granularity because `LuaThread` and `WorldThread` both write them, but
`Structure.Render` itself only reads them. All three sit in one module.

### The audit has no `--self-test` flag

`main()` parses no arguments at all. The self-test is a separate 2,010-line
`tools/test_engine_env_capability_audit.py`, which is what `make ci` and CI run
alongside the audit — the project's usual `test_<tool>.py` convention. Any
acceptance command naming `engine_env_capability_audit.py --self-test` is wrong.

### The mutation surface is small enough to audit

Measured on `master@b5061c18`:

| Surface | Sites | Modules |
|---|---|---|
| Direct writes naming a raw `EngineEnv` field accessor | 64 | 14 |
| Direct writes through a capability record accessor | 45 | 21 |

(`src/` + `app/`, textual matches on `writeIORef` / `modifyIORef'` /
`atomicModifyIORef'` applied to a known field accessor.) Roughly 109 direct
sites total — tractable for a checked-in, both-directions allowlist of the shape
the audit already uses. **Caveat:** these are *direct* textual sites. A helper
that takes an `IORef` as an argument and writes it is invisible to a textual
scan. D-5 settles how CMA-1 treats that residue; Q-4 is where it becomes
evidence.

### Real narrowing is available

§5's per-field Readers/Writers matrix already shows many roles that read without
writing — `MainRender` reads `enginePausedRef` but never writes it; `UnitThread`
reads `materialRegistryRef` but never writes it; `gameTimeRef` has five reader
roles and two writer roles. So an authority boundary would restrict real
consumers, not merely restate the status quo.

Equally, several fields are genuinely multi-writer across threads —
`unitManagerRef` has four writer roles, `statRNGRef` four — so any mechanism must
express "several owners" without collapsing to "everyone."

### There is no existing read-only reference abstraction

No `newtype ReadOnly`/`RO`/`Readable`/`WriteOnly` exists anywhere under `src/`.
Any reference-wrapping mechanism is a new abstraction, not an extension of one.

*(As measured. CMA-2 (#1896) is what created one —
`Engine.Core.ReadOnlyRef.ReadOnlyRef` — so a later slice extends that
rather than inventing a second.)*

### The permanent full-access cohort sits outside any capability mechanism

`PERMANENT_DEFINER` + `PERMANENT_IMPORTERS` is a closed, audited allowlist of 24
production modules that hold unrestricted `EngineEnv` access by job description
(§6.1), and §6.2's temporary ceiling is empty and shrink-only. Those 24 modules
reach every field directly and would bypass a capability-record-level mechanism
entirely. D-4 puts them out of scope for this arc.

### Report corrections

The EA-1 finding was drafted against `master@8070b82c` and three of its figures
have since drifted. None changes its conclusion, and they are recorded here so
the arc works from live numbers:

- `EngineEnv` has **87** fields, not 83 (verified against the live declaration).
- `RenderHandoffCapability` exposes **8** handles, not 7 (`structureWallCatalogRef`
  joined in #1712).
- Separately, `docs/engineenv_capability_inventory.md` §2.1's own summary table
  is stale in the same way: it says `RenderHandoffCapability` (7) and
  `WorldSimCapability` (9) where the live records carry 8 and 11. That drift is
  incidental to this arc — noted here, not adopted as a slice.

## Scope

### In scope

- A write-site gate pinning each field's writing modules, both directions
  (CMA-1, D-2a).
- One pilot capability carrying a structural authority boundary (CMA-2), and a
  recorded verdict on rollout (CMA-3).
- The changes those imply to `tools/engine_env_capability_audit.py`, its
  self-test, and `docs/engineenv_capability_inventory.md` — including §2.1's
  canonical capability-record convention if CMA-2's mechanism changes it.

### Out of scope

- Re-opening the completed capability split (#537). Field-to-capability
  assignments are settled and this arc does not move a field between records.
- Introducing a capability typeclass layer or otherwise parameterizing `EngineM`'s
  environment. Issue #931 deliberately removed `EngineM`'s vestigial environment
  type parameter; that is not being undone.
- Any player-visible behavior change, save-format change, or worldgen-output
  change.
- Fixing §2.1's stale record-size counts (see **Report corrections**).
- Constraining the 24 permanent full-access modules of §6.1 (D-4). They keep
  whole-session orchestration authority by job description, and the closed
  both-directions allowlist that already governs them is untouched.
- Rolling the pilot's boundary out to the remaining capabilities (D-3). That is
  a separate arc, opened on CMA-3's verdict.

## Design

*(Deliberately thin. The design below states only what is settled or
constraint-level. CMA-1's shape is settled by D-2/D-4/D-5; CMA-2's structural
mechanism is Q-4, deliberately deferred until CMA-1 reports.)*

### Constraints any mechanism must satisfy

1. **Preserve the completed inventory and its audits.** §5's classifications, the
   §6.1 closed allowlist, and the §3.1 render/input boundaries all stay green.
2. **Express multi-owner fields honestly.** Several fields have two to four
   legitimate writer roles; "one owner per field" is not an available
   simplification.
3. **Not explode into a role × capability matrix.** Capability records are
   per-*capability*; authority is per-*role*. §3.1's precedent is exactly one
   narrower view per capability, driven by thread-privacy. Eight capabilities ×
   six roles is not a design, it is a symptom.
4. **Survive indirection.** A mechanism that only constrains direct
   `writeIORef (field cap)` sites is defeated by passing the `IORef` to a helper.
   D-5 settles this for CMA-1 — the gate detects and counts the residue without
   attributing it — and the residue count is then evidence for Q-4, where a
   handle-travelling mechanism is the candidate that would actually close it.
5. **Stay warning-clean under `-Werror`** and change no runtime behavior; this is
   a compile-time and gate-time boundary only.

### Proposals

Both proposals from this document's first pass have been decided.

- **P-1** (`RenderHandoff` is the right pilot, not `WorldSim`/`UnitCombat`) —
  **promoted to D-6**.
- **P-2** (the write-site gate is worth doing regardless of the mechanism) —
  **promoted to D-2**.

## Decisions

### D-1. This arc is delivered as an epic, not a single issue

Approved 2026-08-29 while dispositioning EA-1. The mechanism is an unresolved
design decision that changes §2.1's canonical capability convention and
determines every later slice, and the work lands across thirteen records plus
the audit tool. Compressing it into one issue would hand a solver a guess.

**Consequences:** child slices are filed one at a time through
`/process-design-doc`; EA-1 stays `[deferred]` in
`docs/engine_architecture_findings.md` until the epic exists and takes its `[#N]`.

### D-2. The gate comes first; the structural mechanism is chosen afterward

Approved 2026-08-29, and **amended the same day** — see D-2a, which supersedes
this entry's original module-versus-role wording.

CMA-1 extends `tools/engine_env_capability_audit.py` to locate each field's
actual write sites and check them against a checked-in **both-directions** map
of exactly the shape `RENDER_MAIN_ONLY_MODULES` already uses, so a stale
declared entry fails as loudly as an undeclared write. The type-level mechanism
is not chosen now; it is chosen for CMA-2 with the gate's own findings in hand.

**Rationale:** the gate closes the drift hole on its own, ships without Haskell
churn, and makes every later slice verifiable. Choosing the structural mechanism
before knowing which fields actually have clean writer sets would be choosing
blind.

**Consequences:**

- The gate reasons about **direct** write sites. A `writeIORef` reached by
  passing a capability handle into a helper is not attributable by a textual
  scan, and this limitation was explicit when the option was chosen. D-5 settles
  how CMA-1 treats that residue.
- An audit is what EA-1 names as the inadequate status quo, so CMA-1 alone does
  not discharge the finding. CMA-2 is what answers it.
- Resolves Q-1 only as far as CMA-1. The pilot's mechanism is D-7, settled
  ahead of CMA-1 because its deciding evidence was directly measurable.

### D-2a. The gate checks writer MODULES, not §5's writer ROLES

Approved 2026-08-29, amending D-2 after investigation showed its original
wording was not implementable. D-2 said the gate would "reject any [write] whose
module is outside §5's declared writer set" — but §5 declares **thread roles**
and a source scan yields **modules**, and the repository carries no mapping
between them.

The gate therefore maintains **its own checked-in field → writing-modules map**,
independent of §5's role cells, checked in both directions: a write from a module
not in a field's map fails, and a mapped module that no longer writes that field
fails too.

**Rationale — why role-level is not available:**

- No role→module mapping exists. `RENDER_MAIN_ONLY_MODULES` and
  `INPUT_LUA_ONLY_MODULES` are per-*capability* import allowlists for two
  specific §3.1 boundaries; `THREAD_ROLES` is nine bare strings used only to
  validate cell grammar.
- The mapping is not merely missing, it is **not well-defined at module
  granularity**. §3.1 names `World.Render.BloodQuads` as deliberately
  dual-domain; it writes `textureSystemRef` at
  `src/World/Render/BloodQuads.hs:111`, and that field's §5 row resolves the
  role only in prose, annotating that BloodQuads' upload/dispose functions run on
  `MainRender` and not the world thread's `updateWorldTiles` path. The role
  differs **per function**, not per module.
- Scale: of the 87 §5 rows, **43 declare two or more writer roles**, 41 exactly
  one, and 3 none. The ambiguous case is the majority.

**Consequences:**

- The gate verifies a **weaker and different property** than D-2's original
  wording claimed: "the set of modules writing this field is what we last
  declared", not "§5's role claim is true". That is the honest description and
  the epic's own `Done when` must not overstate it.
- §5's Readers/Writers cells stay prose, still unverified by machine. CMA-1
  narrows the drift hole rather than closing it: a *new* writer cannot appear
  unnoticed, but an already-wrong role claim stays wrong.
- The map is a ratchet seeded from today's real write sites, so CMA-1's first
  live run is also the first inventory of them.

**Rejected alternatives:**

- **Role-level checking as D-2 literally said.** Needs a per-*function* role
  map because of dual-domain modules. That is its own arc at least, and it is
  the thing CMA-2's structural mechanism may make unnecessary.
- **Cross-checking declared modules against §5's citations.** §5's citations are
  illustrative rather than exhaustive — the `textureSystemRef` row cites
  BloodQuads lines 76 and 161 but not the write at 111 — so this would be noisy
  in both directions.

### D-3. Bounded arc: gate, one pilot, then stop and reassess

Approved 2026-08-29. The epic is three slices and completes without covering all
eight capabilities. CMA-3 records an evidence-backed verdict; rolling out further
is a separate arc opened only if the pilot earns it.

**Rationale:** proves the pattern once before paying for it eight times. Epic
#537 ran the full-rollout shape successfully, so it stays available — this
declines to commit to it up front, not to ever do it.

**Consequences:** CMA-3 is a real deliverable with a written verdict, not a
ceremonial closing slice. A verdict of "not worth rolling out" is a legitimate,
successful outcome of this epic.

### D-4. The 24 permanent full-access modules are exempt

Approved 2026-08-29. §6.1's closed allowlist — `PERMANENT_DEFINER` plus 23
`PERMANENT_IMPORTERS` — holds whole-session orchestration authority by job
description, and this arc does not constrain it. The existing both-directions
equality check over that set is untouched.

**Consequences:**

- The larger half of the mutation surface stays outside the boundary: of the
  ~109 direct sites measured, 64 (across 14 modules) go through a raw `EngineEnv`
  field accessor rather than a capability record.
- CMA-1's gate must therefore *scope itself* to capability-narrowed consumers and
  say so, rather than reporting the §6.1 cohort as violations. How the exemption
  is expressed is CMA-1's own design detail; that it exists is settled here.

### D-5. CMA-1 detects indirection and reports it, without attributing it

Approved 2026-08-29. Alongside the direct-site authority check, the gate flags
every place a capability handle is passed onward as an argument and reports those
as an unattributable **residue** with a count. Violations block; the residue
reports.

**Rationale:** the residue count is the evidence Q-4 and CMA-3 both turn on. A
small residue means a textual gate is nearly sufficient and the structural pilot
may not be worth rolling out; a large one is the argument for a mechanism that
travels with the handle. Deciding the mechanism without that number would be
guessing.

**Consequences:** full interprocedural attribution is explicitly rejected for
this arc — it is Haskell dataflow analysis written in Python, and the project has
burned long review cycles on hand-rolled analyzers before (PR #1309, 14 rounds;
PR #1463, 7 rounds). The residue stays a reported number, not a resolved one.

### D-6. `RenderHandoff` is the pilot capability (SUPERSEDED by D-6a)

**Superseded 2026-08-29** — see D-6a. The reasoning below was sound at role
granularity but did not account for D-4's §6.1 exemption, which removes two of
the three relationships it relied on.

Approved 2026-08-29, promoting P-1 over EA-1's own suggestion of `WorldSim` or
`UnitCombat`. Its eight fields are genuine single-producer / single-consumer
handoffs — `worldQuadsRef` published by `WorldThread` and read by `MainRender`;
`worldPreviewRef` and `zoomAtlasDataRef` produced by `WorldThread` and consumed
to `Nothing` by `MainRender`; `bloodDisposeQueue` enqueued by `WorldThread` and
drained by `MainRender` — so mostly one writer role per field.

**Rationale:** the two capabilities EA-1 nominated are the most multi-writer in
the tree (`unitManagerRef` has four writer roles, `statRNGRef` four), where an
authority boundary would narrow almost nothing and so would prove nothing.

**Consequences:** the pilot demonstrates the mechanism on a favourable case, and
a small one — RenderHandoff's whole read-only surface is three relationships
(see **Current state and evidence**). That is deliberate for a pilot, and
CMA-3's verdict must say so explicitly rather than generalising
`RenderHandoff`'s result to the multi-writer capabilities untested.

### D-6a. `ContentRegistries` is the pilot capability

Approved 2026-08-29, superseding D-6 after measurement showed `RenderHandoff`'s
in-scope surface is 3 module-field pairs in a single module (`Structure.Render`).

`ContentRegistries`' in-scope surface is the four registries
`crItemManagerRef`, `crEquipmentClassManagerRef`, `crRecipeManagerRef` and
`crSubstanceManagerRef`.

**Corrected by #1896's issue review, and the correction is large.** The
figure this decision was approved on — "7 in-scope module-field pairs across 4
fields and ~5 consumer modules" — undercounted by a factor of five. Measured at
`master@1845ac2910ed` and re-measured unchanged when CMA-2 was implemented, the
four accessors occur in **35 production module-field pairs across 30 modules**
(23 / 3 / 6 / 3 respectively, excluding the capability module itself).
Subtracting the four single-field writer modules leaves **31 read-only pairs
across 26 modules**, and CMA-2 migrated all of them in one change.

`Engine.Scripting.Lua.API.Items.Defs` was also miscounted here as a *reader* of
`crItemManagerRef`: `loadItemYamlFn` hands the handle to `registerItemDefs`,
which mutates it with `atomicModifyIORef'`. It is one of the four raw writers,
not one of the readers.

**Rationale:**

- Registries are content catalogues — loaded once at boot, read everywhere — so
  read-only is the semantically obvious case rather than an imposed one.
- It **exercises the pass-on pattern the newtype was chosen for** (D-7), which
  `RenderHandoff` never would: `Building/Knowledge/Live.hs:104` packs
  `crItemManagerRef reg` into a multi-capability context record. A pilot that
  never crosses a module boundary would not test the property D-7 exists to
  buy. *(Corrected: the two further sites this bullet originally cited are
  not pass-ons. `Unit/Thread/Movement.hs:170` and `Combat/Wounds/Tick.hs:82`
  both apply `readIORef` immediately — they are inline reads. And
  `crInfectionManagerRef`, the second of those, is not one of the four
  selected fields at all. So the pass-on rationale rests on ONE selected-field
  context pass-on, which #1896 duly delivered as
  `ContainerObserver.coItems ∷ ReadOnlyRef ItemManager`.)*
- It stays within one PR — but not "comfortably", and not for the reason
  originally given. *(Corrected: this bullet claimed the pilot was small
  "unlike `RenderView` (31 pairs) or `WorldSim` (36 pairs, 24 modules on one
  field)". The corrected measurement makes `ContentRegistries`' own read-only
  surface 31 pairs across 26 modules — the same scale as the alternative it
  rejects on size. `WorldSim` remains materially larger. CMA-3 must weigh the
  rollout question against the corrected number, not this comparison.)*

**Consequences:**

- The pilot is still a favourable case chosen deliberately, and CMA-3's verdict
  must say so — D-6's caveat carries over unchanged.
- `RenderHandoff` is no longer touched by this arc.

### D-7. The pilot's mechanism is a read-only reference newtype

Approved 2026-08-29, resolving Q-4. CMA-2 introduces a `ReadOnlyRef a` wrapping
an `IORef a` and exporting only the type name, a construction function and a
read — no constructor, no unwrap, no write primitive at all — and the pilot
capability hands non-writer consumers the wrapped form. *(Corrected: this
sentence named `RenderHandoff`, which D-6a had already superseded as the pilot
and which this arc does not touch at all.)*

**As landed (#1896).** `Engine.Core.ReadOnlyRef` exports
`ReadOnlyRef`, `toReadOnlyRef :: IORef a -> ReadOnlyRef a` and
`readReadOnlyRef :: ReadOnlyRef a -> IO a`, and nothing else. Construction is
deliberately PUBLIC: the guarantee bought is "a module handed only the wrapped
form cannot write", not unforgeability — the raw handle is what confers
authority, and a private constructor would only have blocked the view
projection and the test fixtures that build a context record by hand.
`ContentRegistries` keeps the raw writer record; the new
`Engine.Core.Capability.ContentRegistriesView` is what readers take.

**Rationale — the pass-on measurement decides it.** 113 of 321 capability-accessor
uses (35%) pass the raw handle onward, into helper parameters and into context
records that mix several capabilities. Per-role views and accessor-only modules
both draw their boundary at the *record*, and that boundary ends the moment the
`IORef` is extracted — so a third of all uses would walk straight through it,
and `Building.Knowledge.Live`'s multi-capability context record defeats them
outright. A newtype travels with the handle, so `coItems :: ReadOnlyRef
ItemManager` stays read-only wherever it is passed. *(Corrected: the worked
example here was `coGameTime :: ReadOnlyRef Double`. `coGameTime` is
`wsGameTimeRef sim`, a `world-sim-render-handoff` field outside this arc's
scope, and it stays a raw `IORef`. `coItems` is the one `ContainerObserver`
field #1896 wrapped, and the arc's only selected-field pass-on.)*

**Consequences:**

- A new abstraction with no precedent in the tree (no `ReadOnly`/`RO`/`Readable`
  newtype exists under `src/`). CMA-2 establishes both the type and the
  convention for using it — the latter as §2.1's abstract-wrapper extension in
  `docs/engineenv_capability_inventory.md`, which is where a later slice reads
  the rules from.
- Full rollout would propagate the wrapper into helper signatures and shared
  context records. That cost is real and lands on the **rollout arc**, not this
  epic — it is precisely what CMA-3's verdict weighs.
- The pilot demonstrates 31 read-only module-field pairs across
  `ContentRegistries`' 4 fields, in 26 modules (D-6a, as corrected — the
  original "7 pairs across ~5 modules" was wrong). Proving the mechanism works
  is not the same as proving it is worth rolling out, and CMA-3 must not read a
  successful pilot as a mandate.
- The load-bearing demonstration is the ONE pass-on site —
  `Building/Knowledge/Live.hs:104`'s context record — exactly the case a
  record-level boundary misses and this one catches. *(Corrected: the two
  other sites this bullet cited are inline `readIORef` calls, and one of them
  names an out-of-scope registry. CMA-3 should weigh a single demonstrated
  pass-on, not three.)*

**Rejected alternatives:** per-role capability views and accessor-only modules,
both for the record-level escape above. Accessor-only modules additionally carry
the largest churn, since each of the 113 pass-on sites would become an
owner-module call.

## Open questions

### Q-1. What is the enforcement mechanism?

**Resolved by D-2** for CMA-1 (a write-site gate), as amended by **D-2a**
(module granularity, not §5's roles). The structural mechanism for the pilot was
then settled separately as Q-4 → **D-7**.

### Q-2. How far does the arc go?

**Resolved by D-3** — bounded: gate, one pilot, then a recorded verdict.

### Q-3. Are the 24 permanent full-access modules in scope?

**Resolved by D-4** — exempt.

### Q-4. Which structural mechanism does the pilot use?

**Resolved by D-7** — a read-only reference newtype. Settled ahead of CMA-1
rather than after it, because the deciding evidence (the 35% pass-on rate) was
directly measurable without the gate.

The three candidates it chose between, kept for the record:

- **Per-role capability views** — extends §3.1's landed two-interface pattern.
  No new abstractions, but omitting an accessor removes *read* access too, since
  it is the same accessor; this likely forces splitting each field's accessor
  into separate read and write projections, and risks constraint 3's role ×
  capability matrix.
- **A read-only reference newtype** (e.g. `ReadOnlyRef a` exporting only a read
  primitive) — uniform, and the only candidate that survives indirection
  (constraint 4) because the wrapper travels with the handle. New abstraction
  with no precedent in the tree; changes the types of a subset of the 103
  accessors and every read site.
- **Accessor-only modules** — stop exporting the mutating primitive to
  non-owners, mediating writes through owner-module verbs. Strongest
  encapsulation, largest churn.

**Affected slice:** CMA-2 only, and the fence is lifted: with D-7 signed off,
CMA-2 no longer stops and asks. It still depends on CMA-1 landing first.

## Verification strategy

*(Arc-level; per-slice acceptance is added during processing.)*

- `tools/engine_env_capability_audit.py` and its separate self-test
  `tools/test_engine_env_capability_audit.py` both stay green in CI and
  `make ci`, and gain coverage for whatever this arc adds. Note the project's
  ratchet convention: a new audited constant needs the self-test extended in
  the same PR, not just the audit.
- The existing capability boundaries keep passing unchanged: §3.1's render and
  input boundaries, §6.1's permanent-importer equality check, and the E8
  projection check.
- Warning-clean `-Werror` builds of the library, executable, and both test
  suites.
- No behavior change is expected anywhere, so the headless hspec suite and the
  worldgen baselines must be untouched. A slice that moves either is doing
  something this arc did not intend.
- `docs/engineenv_capability_inventory.md` is amended in the same slice as any
  convention change (§2.1) or new audited constant, per §6.4's approval
  procedure.

## Delivery plan

Three slices, strictly sequential: each depends on the one before, and CMA-2 is
additionally gated on a signoff checkpoint (Q-4).

### CMA-1. Gate every capability field's write sites against a checked-in writing-module map

- **Outcome:** `tools/engine_env_capability_audit.py` rejects a capability-field
  write from a module outside that field's checked-in writing-module map, and
  rejects a mapped module that no longer writes that field. The indirection
  residue is counted and listed on every run.
- **Scope:** the write-site check; its checked-in both-directions field →
  writing-modules map, seeded from the real write sites; the §6.1 exemption; the
  residue scan; and matching cases in
  `tools/test_engine_env_capability_audit.py`.
- **Phase:** 1
- **Depends on:** `none`
- **Ordering:** `critical path` — CMA-2's mechanism choice and CMA-3's verdict
  both read this slice's output.
- **Relevant decisions:** D-2, D-2a, D-4, D-5
- **Acceptance signals:**
  - `python3 tools/test_engine_env_capability_audit.py` passes with new cases for
    each of: a write from an unmapped module (rejected); a mapped module with no
    backing write (rejected); a §6.1 module's write (not reported as a
    violation); a handle passed to a helper (reported as residue, not a
    violation).
  - `python3 tools/engine_env_capability_audit.py` runs green against the live
    tree with the seeded map, and prints the residue count.
  - The existing §3.1 render/input boundaries, §6.1 equality check, E8 projection
    check, and §1 field-total check all stay green.
- **Out of scope:** any Haskell change; constraining the §6.1 cohort; resolving
  a residue entry to an originating module; correcting or machine-verifying §5's
  Readers/Writers role cells, which stay prose (D-2a).
- **Open questions:** `None`
- **Note:** the audit is already invoked by CI and `make ci`, so the new check
  rides the existing invocation rather than adding a gate.

### CMA-2. Give `ContentRegistries` a structural mutation-authority boundary

- **Outcome:** a module that only reads a `ContentRegistries` field cannot
  mutate it — a compile error, not a review catch — and the guarantee survives
  the handle being passed into a helper or packed into a context record.
- **Scope:** introduce `ReadOnlyRef` (D-7); hand the wrapped form to the 31
  in-scope read-only module-field pairs across `ContentRegistries`' 4 fields
  (`crItemManagerRef`, `crEquipmentClassManagerRef`, `crRecipeManagerRef`,
  `crSubstanceManagerRef`); migrate all 26 consumer modules, including the one
  selected-field pass-on site; amend §2.1's canonical capability-record
  convention to cover the wrapped form (§6.4's approval procedure applies); and
  update CMA-1's writing-module map if any accessor is renamed. *(The original
  "7 pairs / ~5 modules / three pass-on sites" figures were corrected by
  #1896's issue review — see D-6a.)*

  **As delivered,** the four field types could not simply change: one record
  cannot vary its field types per consumer, so `ContentRegistriesCapability`
  stayed the raw WRITER interface for its four legitimate `X.loadYaml` writers
  and a separate `ContentRegistriesViewCapability` carries the wrapped form for
  every reader. `crvInfectionManagerRef` rides on that view as a raw `IORef` so
  `API.Units.Combat`, the one module mixing a selected registry with an
  out-of-scope one, needs no raw record. The writing-module map needed no edit:
  no accessor was renamed on the writers' side, and a `ReadOnlyRef` field has no
  write to declare.
- **Phase:** 2
- **Depends on:** `CMA-1`
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-6a, D-7
- **Acceptance signals:**
  - A rejected mutation is demonstrated as a compile failure in the PR.
    *(Delivered mechanically rather than as prose:
    `tools/test_read_only_ref_compile.py` compiles five fixtures against the
    real built library and prints each command and complete diagnostic — two
    positive controls that must compile, so a broken environment cannot make
    the rejections look like the boundary.)*
  - The guarantee is shown to survive a pass-on: a read-only handle given to a
    helper or stored in a context record still cannot be written.
  - No behavior change: the headless hspec suite and the worldgen baselines are
    untouched. A diff that moves either is doing something this arc did not
    intend.
  - Warning-clean `-Werror` builds of the library, executable, and both test
    suites.
  - Every existing capability-audit boundary stays green, CMA-1's gate included.
- **Out of scope:** any capability other than `ContentRegistries`; the §6.1
  cohort; rolling the boundary out further.
- **Open questions:** `None` — Q-4 is resolved by D-7, and the pilot target by
  D-6a. This slice depends on CMA-1 (#1892) landing first.

### CMA-3. Record the pilot's verdict and the rollout recommendation

- **Outcome:** `docs/engineenv_capability_inventory.md` carries a written verdict
  on whether the boundary is worth rolling out to the remaining capabilities,
  citing CMA-1's residue count and CMA-2's measured churn.
- **Scope:** the verdict and its evidence; any §2.1 or §6.4 wording the pilot
  proved necessary. The verdict must state explicitly that `RenderHandoff` is a
  favourable case and must not generalise its result to the multi-writer
  capabilities (D-6).
- **Phase:** 3
- **Depends on:** `CMA-2`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-6
- **Acceptance signals:**
  - The verdict names one recommendation — roll out, do not roll out, or roll out
    to a named subset — and cites both numbers.
  - EA-1 in `docs/engine_architecture_findings.md` can be re-dispositioned
    against it.
  - `tools/engine_env_capability_audit.py` stays green against the amended doc
    (it parses §6.1 and §1, so a doc edit can fail it).
- **Out of scope:** filing the rollout arc, if the verdict recommends one.
- **Open questions:** `None`
- **Note:** this slice is likely **docs-only**, which in this repository has no
  PR lane — `/solve` and `/autosolve` are unreachable for it, and it lands
  through the docs worktree instead. Processing should expect that rather than
  discover it.

## Source notes

From EA-1's own handoff context, preserved because it shaped the epic
disposition and one part of it is being contradicted:

> **Expected direction:** At least the most correctness-sensitive domains should
> make owner-only mutation structurally distinct from command-producing or
> snapshot-reading access.
>
> **Scope and constraints:** Preserve the completed capability inventory and its
> audits. An incremental pilot around one domain such as `WorldSim` or
> `UnitCombat` is preferable to another repository-wide environment rewrite.

D-6 overrides that pilot suggestion on evidence: `WorldSim` and `UnitCombat` are
the most multi-writer capabilities in the tree, and `RenderHandoff` has the
cleanest producer/consumer ownership. The finding's "expected direction" is
otherwise carried forward intact, and CMA-2 is the slice that answers it — CMA-1
alone does not, since EA-1 names audits as the inadequate status quo.
