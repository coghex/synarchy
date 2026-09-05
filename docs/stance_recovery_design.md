# Atomic stance recovery design

Stance recovery must preserve stance spent by combat while Lua is calculating the next recovery amount. The first repair replaces a stale absolute write with a bounded relative operation on the engine's current value.

**Delivery recommendation: one issue, one PR.** The engine operation, Lua migration, and regression tests form one complete repair. An umbrella epic would add tracking without a separate deliverable. Stamina, a general resource API, simulation ticks, and chunk residency remain later work.

Design state: `exploring`

This is the requested review draft. Owner direction is recorded under Decisions; the detailed API contract below is the proposed implementation of that direction. No tracker artifact has been created.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]` deliberately not tracked separately · `[deferred]` blocked on a stated precondition.

## Processing status

- [x] EPIC. Preserve combat spending during stance recovery — [no-issue]: one standalone delivery slice; no umbrella needed
- [ ] SR-1. Apply stance recovery atomically against the current stored value

Processing note: the terminal EPIC row records the decision to avoid an umbrella, not completed implementation. Process SR-1 as one standalone issue, with no parent issue or epic checklist to update. The owner expressly requested a judgment on whether an epic was necessary.

## Outcome contract

- **Goal:** Every accepted stance recovery applies to the value current at its commit, preserving intervening combat spending.
- **Done when:** Ordinary recovery uses the new operation; a controlled combat/recovery interleaving preserves both effects; rate, bounds, absence behavior, and existing combat admission remain covered by focused tests.
- **Users:** Players whose units fight while recovering stance; maintainers adding resource mutations.
- **Tracker relationship:** The concrete source finding is CH-1 in [simulation_consistency_findings.md](simulation_consistency_findings.md). Closed [#2328](https://github.com/coghex/synarchy/issues/2328) protects strike admission and spending but cannot protect against a later stale recovery write. [#1890](https://github.com/coghex/synarchy/issues/1890) addresses mutation access, not this operation's atomicity. Neither is a dependency for this repair.
- **Arc label:** None proposed.

## Verified current state

Source examined at `da96202c863b7d563f4968d34cb685d2e622e73c`, 2026-09-05. No matching design document or matching open stance-recovery issue was found in the creation-time search. Recheck the tracker when filing.

`scripts/unit_resource_injury.lua:20` reads stance, reads effective dexterity and agility, calculates recovery, and calls `unit.setStat` with an absolute replacement. Recovery currently equals:

```text
(0.35 + 0.12 × (effective dexterity + effective agility)) × dt
```

An absent dexterity or agility contributes 1. Lua currently skips recovery when stance is absent or already at least 1. The normal physiology loop calls this function for living units (`scripts/unit_resources.lua:77`).

`unit.setStat` uses `atomicModifyIORef'`, but inserts the supplied value rather than adjusting the current one (`src/Engine/Scripting/Lua/API/Units/Stats.hs:343`). The preceding Lua read is outside that transaction. In addition, `getStat` reads a modifier-adjusted value while `setStat` writes the stored base (`Stats.hs:292,628`). Those are distinct semantics.

Combat admission and spending use stored stance directly; absence means 1 (`src/Combat/Resolution/Admission.hs:111`, `src/Combat/Resolution/Wear.hs:164`). A quick attack spends 0.25 and a heavy attack spends 0.5 (`src/Combat/Resolution/Constants.hs:178`). Strike admission and its unit-manager effects now commit in one atomic update (`Admission.hs:183`).

The [retained reproduction](audit_evidence/2026-09-05/stance_interleaving.lua) runs the actual recovery module with a simulated engine-boundary interleaving:

| Event | Stance |
| --- | ---: |
| Recovery reads the old value | 0.600 |
| Quick attack spends 0.250 | 0.350 |
| Old recovery writes its calculated replacement | 0.659 |
| Correct value preserving the 0.059 recovery and attack cost | 0.409 |

The reproduction establishes the overwrite mechanism, not its frequency in a running game. The implementation regression should go further and exercise the real engine operation and production spending helper.

## Scope

Included: one bounded stance-recovery API, its authoritative update, the shipped Lua caller, focused regression coverage, and concise API/contract documentation.

Excluded: stamina migration; a generic adjustment API for all stats; modifier redesign; moving physiology into Haskell; changing combat costs or admission; tick scheduling; page activation; chunk caching; new queues, workers, persistent fields, or assets. Existing raw setters retain their initialization/debug behavior. This repair protects the normal recovery path, not an explicit later debug overwrite.

## Proposed design

### The engine receives an amount

```lua
local newStance, reason = unit.recoverStance(uid, amount)
```

`amount` is the full intended recovery (`rate * dt`). It is not an absolute target and is not the difference between a previously clamped target and an old stance reading.

The operation executes synchronously using the existing unit-manager atomic update. Its linearization point is that update: lookup, current-value read, addition, bounds enforcement, and publication happen together. A separate `readIORef` followed by an atomic replacement would reproduce the defect and does not meet this contract.

| Situation | Proposed result and effect |
| --- | --- |
| Existing unit, finite stored stance | Store `clamp(current + amount, 0, 1)`; return the committed stored value |
| Existing unit, stance absent | Return 1; keep the entry absent, preserving implicit full stance without a lazy stat roll |
| Zero amount and valid stance in range | Successful no-op; return current stance |
| Full stance | Return 1 without introducing a stale Lua-side eligibility decision |
| Unit absent at commit | Return `nil, "no_such_unit"`; create nothing |
| Invalid unit ID | Return `nil, "invalid_unit_id"`; write nothing |
| Amount negative, nonnumeric, NaN, or infinite | Return `nil, "invalid_amount"`; write nothing |
| Existing stored stance non-finite | Return `nil, "invalid_stance"`; write nothing |

Validate the ID as an integral value in the actual `UnitId` range before narrowing it; invalid IDs must not wrap onto another unit. Validate the amount before mutation. Large finite amounts must saturate safely, without publishing a non-finite intermediate. Stable reason tokens are sufficient; this does not require a new event/notification system. Invalid argument checks precede unit lookup.

Recovery changes stored stance, exactly the value combat spends. It does not apply a stance modifier and write that effective result back into the base. Tests should pin this distinction explicitly. Dexterity and agility remain effective readings used to determine the rate. This deliberately resolves recovery's existing base/effective ambiguity; it is not a general redesign of modifier semantics.

### Lua computes the rate and requests recovery

`tickStance` keeps the existing formula and missing-stat fallbacks, then sends the recovery amount. Remove its read of current stance, its current-stance early return, and its absolute `setStat` write. The engine owns the full/absent checks and cap against current state. Keeping an early Lua read would avoid the overwrite but could still suppress recovery because an already-stale reading said the unit was full.

The normal living-unit/pause eligibility remains with the current physiology orchestration. The new primitive does not change pose, revive units, or introduce an independent ticking path. If the unit vanishes during rate calculation, the commit returns the named missing-unit result. Invalid input/state refusals should remain diagnosable through the existing Lua error path; they must not be silently converted into successful recovery.

Rate inputs can change between their reads and commit. Small differences of this kind are accepted for this slice. The requirement is that the accumulated stance value is current at commit. There is no retry of the Lua callback, which may be embedded in a larger update containing other effects.

### Integration and ownership

Add the new binding in a cohesive owner under `Engine.Scripting.Lua.API.Units` and register it directly in `src/Engine/Scripting/Lua/API/Register/Unit.hs`. The `API.Units` facade explicitly preserves its original export surface; its new binding must not be added there simply for convenience. Direct imports already exist for newer medical and transfer APIs.

Keep mutation through the existing `UnitCombatCapability` reference and `atomicModifyIORef'`. A small pure bounded-update helper can support testing, but a generic public resource framework is not a prerequisite. Choose the exact helper/module placement during implementation; it is not a new architectural boundary requiring another issue.

No save representation changes: stance remains the same entry in `uiStats`, implicit full stance remains representable by absence, and no component version or global save-version bump is needed. No RNG is consumed by the new operation. The Lua rate's existing stat reads retain their ordinary semantics.

### Ordering tolerance

Recovery and combat spending both operate on the current value at their own commits. Away from a bound, the example's two orderings both produce 0.409, within floating-point tolerance. At a bound, different orderings can legitimately differ: from 0.95, recovery of 0.10 followed by spending 0.25 yields 0.75; spending first and recovering second yields 0.80. The owner accepts this ordering sensitivity. Neither outcome permits a stale absolute write to erase the debit.

## Decisions

### D-1. Begin with stance recovery

The owner accepted the stance-first recommendation and requested a design draft. Deliver the smallest complete repair before extending the approach to stamina or broader simulation work.

### D-2. Relative mutation is the selected direction

The owner proposed supplying an offset so the engine applies it against its current value. The design adopts that direction and places bounds enforcement in the same commit. The precise API signature and edge-case table remain proposals in this review draft.

### D-3. Exact deterministic ordering is unnecessary

The owner accepts small execution-order fluctuations. Preserve operations rather than imposing replay determinism or a fixed cross-thread order. The tick-rate disagreement remains important future work but is not a dependency here.

### D-4. One issue is sufficient

The owner delegated the single-issue-versus-epic judgment. This design chooses one standalone issue because the binding, Lua migration, and tests are inseparable parts of one result. There is no independently useful infrastructure phase to land first.

## Alternatives considered

- **General `adjustStat` API:** Potentially useful later, but requires shared rules for base/effective values, missing pools, bounds, and dynamic maxima. Stance supplies a small, explicit first contract.
- **Version check and retry:** Useful when an entire calculation depends on a consistent snapshot. Independent addition does not need this, and retrying a complete Lua callback can duplicate effects.
- **Single mutation-owning worker:** A possible broader simulation architecture. Queueing and lifecycle work are unnecessary to serialize this one update because the shared manager already has an atomic mutation primitive.
- **Only improving combat's transaction:** Already addressed by #2328; it cannot prevent a later stale Lua write.

## Open questions

No product decision prevents completing this draft. Review the proposed API/result contract and explicit stored-stance semantics before treating the document as ready for issue processing. There are no dependencies on art, world-page policy, clock redesign, or residency design. Exact module and test-file placement can be settled in the implementation issue.

## Verification strategy

Use controlled scheduling rather than probabilistic race loops:

1. Drive the shipped Lua recovery function through the real registered recovery binding. Interpose a test hook during its rate calculation that applies production `spendStrikeCost` to the shared unit manager. With stance 0.600, a quick strike, dexterity/agility 1, and `dt = 0.1`, assert stored stance 0.409. This must fail against the old recovery implementation. Use a fixture with defined rate stats so the hook does not depend on random rolls.
2. Cover debit-before-recovery and recovery-before-debit. Assert the expected serial results, including saturation where order legitimately matters. Check the engine's returned value agrees with what that recovery committed.
3. Cover ordinary recovery, zero amount, full stance, absent stance remaining absent, removal before commit, invalid IDs/amounts, and non-finite stored stance. Refusals leave the manager unchanged.
4. Give the unit a stance modifier and verify recovery adjusts the stored base without baking in the modifier. Retain effective dexterity/agility rate behavior.
5. Preserve existing combat admission checks using the focused `Test.Headless.Combat.Admission` suite. Confirm no unrelated fields, modifiers, inventory, or pose are replaced by recovery.

Use the existing quiet headless harness and bare Lua fixtures; no generated world or graphical run is necessary. The standalone audit reproduction is historical evidence, not the production regression gate.

During implementation, run the new targeted Hspec describe(s), the existing combat-admission describe, and `python3 tools/lua_module_budget.py` because `unit_resource_injury.lua` belongs to its capped resource family. Run other gates only when their actual inputs change. The new registration and shipped caller should be exercised by the integration test itself. Do not require full CI, all behavior probes, worldgen baselines, or save compatibility regeneration for this repair. Exact commands and discovered describe names belong in the eventual issue.

## Delivery plan

### SR-1. Apply stance recovery atomically against the current stored value

- **Outcome:** Combat spending survives ordinary Lua stance recovery, including an intervening strike during recovery calculation.
- **Scope:** Engine operation and direct registration; Lua caller migration; focused tests; concise contract documentation for stored-value and absence semantics.
- **Phase:** Complete repair.
- **Depends on:** None.
- **Ordering:** Can land first; independent of timing, background-page, and residency work.
- **Relevant decisions:** D-1 through D-4.
- **Acceptance signals:** The production-boundary interleaving yields 0.409; normal rates and caps remain correct; missing-unit/implicit-full behavior is explicit; invalid inputs write nothing; existing combat admission still passes.
- **Out of scope:** Stamina and other resources; generic API rollout; deterministic replay; worker ownership overhaul; tick scheduling; chunk caching; report disposition fields.
- **Open questions:** No unresolved product dependency; the proposed contract is subject to this draft's review. Helper placement does not require another slice.

## Later work retained outside this delivery

After stance lands, examine stamina's resource update, dynamic maximum, initialization, and post-update death/collapse checks before choosing whether to extend the primitive. Simply replacing its final setter would leave those decisions based on stale values. That follow-up may justify a wider design, but this document does not precommit an epic.

The [architecture conversation](architecture_conversation_audit_2026-09-05.md) remains the record for fixed gameplay ticks, continued home-colony simulation, and explicit retention of gameplay-critical chunks. None is silently folded into SR-1.
