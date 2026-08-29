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
- [ ] CMA-1. Check every capability field's write sites against its declared writer set
- [ ] CMA-2. Give `RenderHandoff` a structural mutation-authority boundary
- [ ] CMA-3. Record the pilot's verdict and the rollout recommendation

## Epic contract

- **Goal:** The capability inventory's declared writer set for every field is
  checked against the code by a blocking gate, and one capability additionally
  carries a structural boundary making a non-writer's mutation a compile error —
  with an explicit, evidence-backed verdict on whether that boundary is worth
  rolling out further.
- **Done when:** (1) the write-site gate runs in CI and `make ci`, rejecting both
  a code write outside a field's declared writer set and a declared writer entry
  no code backs; (2) `RenderHandoff` carries the structural boundary (D-6); and
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

- A write-site gate that checks §5's declared writer sets against the code
  (CMA-1).
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

Approved 2026-08-29. CMA-1 extends `tools/engine_env_capability_audit.py` to
locate each field's actual write sites and reject any whose module is outside
§5's declared writer set, with a checked-in **both-directions** module map of
exactly the shape `RENDER_MAIN_ONLY_MODULES` already uses — so a stale declared
entry fails as loudly as an undeclared write. The type-level mechanism is not
chosen now; it is chosen for CMA-2 with the gate's own findings in hand.

**Rationale:** the gate closes the §5-drift hole on its own, ships without
Haskell churn, and makes every later slice verifiable. Choosing the structural
mechanism before knowing which fields actually have clean writer sets would be
choosing blind.

**Consequences:**

- The gate reasons about **direct** write sites. A `writeIORef` reached by
  passing a capability handle into a helper is not attributable by a textual
  scan, and this limitation was explicit when the option was chosen. Q-5 asks
  whether CMA-1 must nonetheless *detect* such indirection.
- An audit is what EA-1 names as the inadequate status quo, so CMA-1 alone does
  not discharge the finding. CMA-2 is what answers it.
- Resolves Q-1 only as far as CMA-1. The pilot's mechanism is Q-4.

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

### D-6. `RenderHandoff` is the pilot capability

Approved 2026-08-29, promoting P-1 over EA-1's own suggestion of `WorldSim` or
`UnitCombat`. Its eight fields are genuine single-producer / single-consumer
handoffs — `worldQuadsRef` published by `WorldThread` and read by `MainRender`;
`worldPreviewRef` and `zoomAtlasDataRef` produced by `WorldThread` and consumed
to `Nothing` by `MainRender`; `bloodDisposeQueue` enqueued by `WorldThread` and
drained by `MainRender` — so mostly one writer role per field.

**Rationale:** the two capabilities EA-1 nominated are the most multi-writer in
the tree (`unitManagerRef` has four writer roles, `statRNGRef` four), where an
authority boundary would narrow almost nothing and so would prove nothing.

**Consequences:** the pilot demonstrates the mechanism on a favourable case. That
is deliberate for a pilot, and CMA-3's verdict must say so explicitly rather than
generalising `RenderHandoff`'s result to the multi-writer capabilities untested.

## Open questions

### Q-1. What is the enforcement mechanism?

**Resolved by D-2** for CMA-1 (a write-site gate). The structural mechanism for
the pilot remains open as Q-4.

### Q-2. How far does the arc go?

**Resolved by D-3** — bounded: gate, one pilot, then a recorded verdict.

### Q-3. Are the 24 permanent full-access modules in scope?

**Resolved by D-4** — exempt.

### Q-4. Which structural mechanism does the pilot use?

Deliberately deferred by D-2 until CMA-1's gate exists and has reported. The
three candidates carried forward from Q-1:

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

**Affected slice:** CMA-2 only. **Stop/ask behavior:** CMA-2 is not filed until
CMA-1 has landed and this question is answered with explicit signoff; a
`/process-design-doc` run that reaches CMA-2 with Q-4 open must stop and ask
rather than pick a mechanism.

**What resolves it:** a user decision informed by CMA-1's report — specifically,
how many fields have clean single-role writer sets and how much indirection the
gate found.

## Verification strategy

*(Arc-level; per-slice acceptance is added during processing.)*

- `tools/engine_env_capability_audit.py --self-test` plus its own CI and
  `make ci` runs stay green, and gain coverage for whatever this arc adds. Note
  the project's ratchet convention: a new audited constant needs the self-test
  extended in the same PR, not just the audit.
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

### CMA-1. Check every capability field's write sites against its declared writer set

- **Outcome:** `tools/engine_env_capability_audit.py` rejects a capability-field
  write from a module outside that field's §5 writer set, and rejects a declared
  writer entry no code backs. The indirection residue is counted and listed on
  every run.
- **Scope:** the write-site check; its checked-in both-directions module map; the
  §6.1 exemption; the residue scan; the tool's `--self-test` extended to cover
  all four behaviors; and any §5 writer-cell corrections the first live run
  surfaces, made in the same PR.
- **Phase:** 1
- **Depends on:** `none`
- **Ordering:** `critical path` — CMA-2's mechanism choice and CMA-3's verdict
  both read this slice's output.
- **Relevant decisions:** D-2, D-4, D-5
- **Acceptance signals:**
  - `python3 tools/engine_env_capability_audit.py --self-test` passes with new
    cases for each of: a write outside the declared set (rejected); a declared
    writer with no backing write (rejected); a §6.1 module's write (not reported
    as a violation); a handle passed to a helper (reported as residue, not a
    violation).
  - The audit runs green against the live tree, having either confirmed §5's
    writer cells or corrected them.
  - The residue count is printed on every run.
  - The existing §3.1 render/input boundaries, §6.1 equality check, E8 projection
    check, and §1 field-total check all stay green.
- **Out of scope:** any Haskell change; constraining the §6.1 cohort; resolving
  a residue entry to an originating module.
- **Open questions:** `None`
- **Note:** the audit is already invoked by CI and `make ci`, so the new check
  rides the existing invocation rather than adding a gate.

### CMA-2. Give `RenderHandoff` a structural mutation-authority boundary

- **Outcome:** a module that §5 records as a non-writer of a `RenderHandoff`
  field cannot mutate it — a compile error, not a review catch.
- **Scope:** the Q-4 mechanism applied to `RenderHandoffCapability`'s eight
  fields; the §2.1 canonical-convention amendment if the mechanism changes the
  convention (§6.4's approval procedure applies); the audit ratcheted to enforce
  the new shape.
- **Phase:** 2
- **Depends on:** `CMA-1`
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-6; mechanism pending **Q-4**
- **Acceptance signals:**
  - A rejected mutation is demonstrated as a compile failure in the PR.
  - No behavior change: the headless hspec suite and the worldgen baselines are
    untouched. A diff that moves either is doing something this arc did not
    intend.
  - Warning-clean `-Werror` builds of the library, executable, and both test
    suites.
  - Every existing capability-audit boundary stays green.
- **Out of scope:** any capability other than `RenderHandoff`; the §6.1 cohort;
  rolling the boundary out further.
- **Open questions:** **Q-4** — this slice is not filed until CMA-1 has landed
  and Q-4 is signed off. A `/process-design-doc` run reaching CMA-2 with Q-4 open
  stops and asks.

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
