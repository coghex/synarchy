## Background

Epic #2718 requires exact water accounting through every edit. This Phase 1
slice replaces the edit path for sessions on the new hydraulic test backend.
It depends on #2721 and #2723, including their approved review amendments.

Verified on `master` (`a1a98d326e75`):

- `World.Edit.Apply` retains the old fluid surface or deletes fluid when
  filling; digging retains the surface or reveals unaccounted groundwater.
  `WeSetFluidTile` replaces the column with one full level.
- Terrain edits and spoil promotions call `syncEditToSim`;
  `Sim.Chunk.applyChunkEdit` replaces chunk fluid from published tiles,
  discarding newer simulation transfers.
- `World.Thread.Command.Edit.Dig` publishes spoil, spoil promotions, whole
  chunk yields, designation progress, and slope changes incrementally.
  Final completion additionally rolls gems, removes the designation, and
  deletes the terrain level. These effects do not all wait for completion.
- `world.digTile` returns no acknowledgement. `scripts/unit_ai_dig.lua`
  treats disappearance of a designation as completion and may award XP even
  when it was cancelled. Releasing the Lua claim does not undo partial work.
- Legacy solidification removes up to eight remaining fluid units beneath
  the new stone (`Sim.Chunk.applyReactionCommit`).

## Owner decisions and scope

The owner selected **one atomic transaction per hydraulically affected dig
increment** on 2026-09-29. Progressive digging remains visible after each
commit. Cancellation preserves earlier committed increments and discards
only uncommitted work. Completion XP requires the final acknowledged commit.
The increment owns its yield, spoil, and every resulting spoil promotion.

Retain the existing decisions: fill preserves quantity over the raised bed;
digging preserves quantity and draws only #2723's finite aquifer top-up;
fluid placement adds exactly eight units as a named, deduplicated source;
and reservations use an explicit test provider. Unlike-fluid placement uses
the reaction path rather than replacing existing fluid.

Deliver the implementation, tests, contract updates, and evidence together
in one PR. Normal sessions retain the legacy backend until RVR-29. The real
CRS-6 reservation subset remains required before production activation or
any future player-orderable wet-edit feature.

## Requirements

1. **Eligibility and identity.** Determine hydraulic involvement from current
   authority across the complete effect footprint, not displayed fluid.
   Include wet cells, barriers/interfaces, dry banks opened beside water,
   initially dry excavations eligible for finite aquifer extraction, and
   hydraulically affected secondary spoil promotions. Give each affected
   increment or standalone edit a session/page-scoped command identity.
   Repeated submissions of that identity cannot create additional pending
   work or effects; terminal outcomes remain deduplicated under #2721.

2. **Latest-state reconciliation.** Admit against the latest hydraulic state
   at a step boundary, including accepted neighbouring transfers. Preserve
   page-incarnation and edit-generation fences. In test-backend sessions,
   no edit path may reseed whole-chunk hydraulics from stale published tiles.
   This also applies to immediate unrelated dry edits and spoil promotions
   in a chunk with newer transfers elsewhere.

3. **Conservative geometry.** Fill preserves quantity while raising the bed;
   dig preserves quantity and atomically debits/credits any top-up under
   #2723 using the newly committed geometry. Partial progress must not mint
   water, and retries cannot repeat extraction. Placement adds exactly eight
   units once, with unlike fluid handled through accounted reactions.
   Solidification preserves reaction ratios, admission fences, durable stone,
   and occupant handling, but the new backend conserves remaining fluid
   instead of applying legacy volume removal beneath the stone. Refuse
   unrepresentable changes or account excess under #2721; never clip it.

4. **Atomic incremental work.** An affected increment stays pending until
   admitted. Its proposed slope/terrain, designation progress, water change,
   aquifer debit, yield, spoil deposits, spoil promotions, and any material
   consumption remain uncommitted. Previously committed increments remain
   visible. Publish the increment's effects as one coherent transaction;
   ordinary revalidation and regrounding follow successful publication.
   Only the final increment also completes the designation and produces its
   completion-only effects, including the gem outcome and job completion.
   Preserve existing yield rates, spoil-disposal rules, and gem eligibility.

5. **Acknowledgement and cancellation.** Integrate pending and terminal
   outcomes through the real `world.digTile` API and Lua dig-job lifecycle.
   A pending acknowledgement is not a completion receipt. Repeated work
   submissions while an increment is pending must not accumulate duplicate
   work or effects. Subsequent work proceeds from acknowledged progress.
   Award completion XP once, only for acknowledged final completion;
   designation disappearance alone is insufficient on this backend.
   Cancellation/refusal changes none of that increment's committed gameplay
   state, preserves prior commits, and awards no completion XP. Reserved
   resources follow the applicable refund contract. If commit wins a race
   with cancellation, report committed and preserve its effects; do not
   manufacture a rollback. Release reservations only after a definitive
   committed, cancelled, or refused outcome.

6. **Complete reservations.** The increment owns secondary spoil promotions
   in the same transaction and reservation footprint. Include the dig site,
   affected spoil state, promotion tiles, and dependent hydraulic faces.
   Acquire the complete bounded footprint atomically or retry holding no
   subset. Serialize overlaps by command identity; allow disjoint work.
   Pending reservations participate in job admission and final movement
   checks. Revalidate geometry, hydraulic revisions, and spoil legality after
   waiting; changed secondary effects cannot escape the reserved footprint.
   A failure to admit any constituent effect refuses the whole increment.

7. **Persistence and replacement boundaries.** Extend #2721's transaction
   model and inventory classifications to cover pending edits, reserved
   effects, command outcomes, and deduplication state. Preserve pending
   domain records through its test codec; process-local reservation handles
   are not durable substitutes for those records. A coherent capture may
   contain pending intent alongside unchanged committed state, or the whole
   committed transaction, never half its terrain/water/yield effects.
   Verify first commit and duplicate rejection after a round trip. Session
   replacement must fence old commands, reservations, callbacks, and receipts
   even when the replacement reuses a page ID. Preserve historical edit-log
   replay, particularly replacement semantics of old `WeSetFluidTile` records.
   Use explicit test-backend capture/restore and replacement fixtures here;
   production hydraulic save wiring remains #2727/RVR-09. Ordinary legacy
   saves and loads retain their current behavior.

8. **Timing and publication evidence.** At speed 1.0 measure request-to-
   admission and admission-to-publication at the shared gameplay/render
   projection boundary. Record commit timing separately if useful; it alone
   is only a proxy for visible response. Exercise rendered partial work,
   pending final work, cancellation, and conservative fill/dig through an
   offscreen test-backend scenario. Report observed response against P-2's
   200 ms target, with workload, timing boundaries, backlog, refusal counts,
   and any measurement limitations. Archive the evidence in the same PR.

9. **Documentation and delivery.** Update Q-20/P-2 in
   `docs/designs/river_runtime_design.md` with the selected incremental commit,
   acknowledgement, cancellation, placement, and reservation contracts.
   Update `docs/engine_contracts.md` to distinguish legacy solidification
   volume removal from new-backend conservation. Update
   `docs/persistence_state_inventory.md` for the state introduced here.
   Complete these documents and the evidence before final PR review; this
   PR closes the issue when all requirements are met.

## Acceptance

Run from the implementation worktree:

```bash
cabal build all
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "Sim.Fluid.ConservativeEdits"'
cabal test synarchy-test-headless --test-options='--match "Sim.Fluid.Aquifer"'
cabal test synarchy-test-headless --test-options='--match "Sim.Fluid.Durable"'
cabal test synarchy-test-headless --test-options='--match "world.digTile admission"'
cabal test synarchy-test-headless --test-options='--match "unlike-fluid reaction"'
cabal test synarchy-test-headless --test-options='--match "solidification"'
cabal test synarchy-test-headless --test-options='--match "persistence contract"'
python3 tools/persistence_inventory_audit.py
python3 tools/test_persistence_inventory_audit.py
python3 tools/enum_append_only_audit.py
python3 tools/lua_module_budget.py
```

Expected: a warning-clean build, passing applicable audits, and non-empty
matching test groups. Add `Sim.Fluid.ConservativeEdits` coverage proving:

- Exact balances for fill/full fill, wet and aquifer-eligible dry digging,
  partial increments, depleted aquifers, placement onto water/lava, and
  solidification, with only declared sources/sinks changing totals.
- Transfers survive races with edits/writebacks and with unrelated immediate
  dry edits or spoil promotions in the same chunk.
- A pending partial or final increment publishes none of its proposed effects;
  commit publishes all once. Cancellation retains earlier committed progress.
- Real Lua API/job integration handles duplicate submissions, pending final
  work, refusal, cancellation, and commit/cancel races without repeated yields,
  spoil, aquifer extraction, gem effects, or completion XP.
- Secondary spoil promotions share admission and cancellation with their
  parent increment; overlapping footprints serialize, disjoint work proceeds,
  and changed revisions/footprints cannot partially commit.
- Test-backend codec/capture and session replacement at pending, committing,
  and completed boundaries preserve coherent state and reject stale-session
  commands. Historical fluid-placement replay retains its original meaning.
- Unrelated dry/cosmetic edits remain immediate and legacy sessions retain
  their existing behavior, including their solidification semantics.

Add a reproducible probe with this interface in this PR:

```bash
python3 tools/conservative_edits_probe.py --offscreen --port 9018 --output artifacts/conservative-edits
```

The probe explicitly selects the test backend and test reservation provider,
uses the real edit API and Lua job lifecycle, and captures paired screenshots,
query/receipt evidence, and timing data for requirement 8. It must fail on
inconsistent publication or missing expected transitions. Include a legacy
control. It must start no visible window, use an isolated resource/save root,
and stop only its own engine. Commit the reproducible evidence archive and
measurement verdict with the implementation; headless results alone do not
prove rendered behavior. Report target misses rather than disguising them
with admission-to-commit timing.

Run any additional subsystem gates selected by the final changed paths under
the repository instructions, including save-compat selection when applicable.

## Out of scope

- Real CRS-6 reservations (#1997), production activation (RVR-29), and
  production hydraulic save/reconstruction wiring (#2727/RVR-09).
- Player-orderable dams, new materials/recipes, and spoil-disposal changes.
- Compact-owned promotion on edit (RVR-17), a new fluid kernel, or a new clock.
- Blanket hydraulic delays for unrelated dry/cosmetic edits.

## Related

- Epic #2718, RVR-06; prerequisites #2721 and #2723.
- #1997: real reservation capability required before production rollout.
- #2485/#2490: reaction admission, durable stone, and occupant handling.
- #1596/#2477: edit-generation and page-incarnation fences.
- RVR-13 and RVR-17 build on this protocol.

<!-- issue-origin:claude -->
