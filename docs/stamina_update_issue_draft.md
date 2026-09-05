# Preserve combat stamina costs during physiology updates and evaluate exhaustion from committed values

Proposed labels: `bug`, `lua`

Draft only; not filed. Verified against `13fa8126ad8d0126abd04eeaf77705aa1809a087` on 2026-09-05. One issue and one PR; no umbrella epic or dependency on the stance-recovery repair.

## Background

The Lua physiology tick reads current stamina, calculates drain/recovery, and writes an absolute replacement. Combat can spend stamina between that read and write. The replacement erases the combat cost, and the following exhaustion checks use the stale pre-update value and calculated replacement too.

`scripts/unit_resource_tick.lua:37-46` reads the maximum/current value and handles initialization; `:146-155` calculates and writes the absolute replacement; `:179-205` checks death and collapse. `src/Engine/Scripting/Lua/API/Units/Stats.hs:343` makes the individual setter atomic but does not make the preceding calculation atomic.

Combat already commits its own resource cost atomically. `src/Combat/Resolution/Wear.hs:142-174` spends 25% of effective maximum stamina for a heavy attack, floors at zero, and delegates exhaustion consequences to the resource tick. Fixing that combat transaction cannot prevent a subsequent stale Lua replacement.

The retained controlled reproduction loads the shipped resource tick and acolyte stamina configuration. With maximum stamina 10, endurance 1, idle recovery, and `dt = 0.1`, it injects a 2.5 combat debit during the endurance read after the initial stamina snapshot:

- Starting at 6, the current tick writes **6.05**; preserving the debit and recovery would yield **3.55**.
- Starting at 2, the debit reaches zero, but recovery writes **2.05** and requests no death.
- In the control starting at zero with no intervening debit, the existing rule requests death even though recovery has written 0.05. The missed death is therefore a stale-state defect, not an intentional rescue rule.

This reproduction executes real Lua code with a simulated API-boundary interleaving. It establishes the mechanism, not live-thread frequency. The implementation tests must exercise the real engine mutation boundary and production spending helper.

## Requirements

1. **Apply a signed stamina adjustment against current stored state.** Lua computes the intended net amount, `(regen - drain) * dt`, before clamping. The engine reads the current unit and stamina, resolves its effective maximum, adds the amount, clamps, and publishes in one `atomicModifyIORef'` operation on the existing unit manager. An absolute target or an offset derived from a stale clamped target is insufficient. Preserve unrelated unit fields and concurrent writes to them.

2. **Resolve the bound from the committing unit record.** Preserve the existing explicit `max_stamina` precedence and otherwise effective `endurance * 10`, including active additive/percentage modifiers and expiry. Reuse or factor the existing `Combat.Resolution.Common.maxStaminaFor` policy rather than create a third formula. Use one captured game-time sample for modifier expiry. The resource value adjusted is stored stamina, as combat spends it; do not bake modifiers on stamina itself into its stored base. Retain the normal Lua configuration/maximum-eligibility gates and rate-input semantics.

3. **Return the actual commit result.** Expose a synchronous stamina-specific operation returning a record containing `before`, `after`, `maximum`, and `initialized`, or an explicit refusal. The three numeric values describe the same update; `before` is the stored value before recovery/drain, `after` is what was actually committed, and `maximum` is the bound used. The exact API name is an implementation choice. Avoid widening the frozen `API.Units` facade; register a new binding through a direct import, as the current unit registrar does for newer APIs.

4. **Initialize only a still-absent pool.** If stamina is absent at commit, fill it to the current eligible maximum and return `initialized = true`; `before` is absent and the caller skips ordinary adjustment consequences for that initialization pass, preserving first-tick behavior. If combat created or changed stamina while Lua was calculating, take the existing-pool path, apply the intended delta, and return `initialized = false`. Never refill based on an earlier nil observation. Do not return from Lua before it has computed the intended delta solely because an earlier stamina read was nil.

5. **Use the committed result for exhaustion.** The stamina branch's existing death and collapse checks consume returned `before`, `after`, and `maximum`. Preserve kill-before-collapse ordering, `kill_on_zero` on either before or after, existing configured thresholds, dead/collapsed pose guards, and the separate cross-resource revival gate. When the operation observes `before = 0`, subsequent positive recovery must not hide it from `kill_on_zero`. A missing-unit or refused result causes no threshold action. Other resources retain their present path.

6. **Handle zero and negligible deltas correctly.** A zero net change still obtains a current commit result and evaluates exhaustion; a stale Lua comparison such as `abs(next - current) > 1e-4` must not bypass the authoritative operation. If an insignificant-write optimization is retained, apply it inside the transaction and return the value actually left in storage. Clamp against a maximum that changed during Lua calculation.

7. **Make refusal non-mutating.** Invalid/out-of-range unit IDs, non-finite deltas, non-finite stored stamina, and a non-positive/non-finite resolved maximum return a named failure without changing state. A unit absent at commit is a normal named refusal and is never recreated. Large finite deltas saturate safely without publishing infinity or NaN. Invalid-state/argument failures remain diagnosable; they are not presented as successful updates.

8. **Keep the repair bounded.** Preserve existing idle, movement, caffeine, and organ-failure rate calculations and all three shipped stamina configurations (acolyte, brown bear, red squirrel). No new worker, queue, `EngineEnv` field, persistent data, RNG draw in the mutation primitive, or save-version change is needed. Do not retry an entire Lua callback. Small ordering differences and slightly older rate inputs are acceptable; the accumulated pool and bound must be authoritative at commit.

## Acceptance

Add a targeted headless describe named **`stamina resource commit`**. Drive the real registered engine operation and the shipped `tickResource` function with controlled test-only interleavings; the required evidence is more than a test of a new arithmetic helper.

- Interpose the production `spendStrikeCost` update during Lua rate calculation. Verify the 6 → debit 2.5 → recovery 0.05 scenario ends at 3.55. The same test must fail against the old Lua update.
- In the 2 → debit to zero case, assert the engine reports before 0 and after 0.05, and Lua requests death without also requesting collapse. Retain a no-interleaving zero control.
- Exercise debit-before-update and update-before-debit, plus a negative physiology delta. Verify both mutations survive, allowing the documented serial-order differences near bounds.
- Exercise a still-absent pool and a pool populated/spent after any earlier Lua observation but before commit. Only the former initializes; the latter must never refill to maximum.
- Change an explicit maximum or its relevant modifiers during rate calculation. Verify the commit uses the new effective bound and the returned threshold denominator matches it. Cover derived endurance and explicit-maximum precedence. Preserve the existing no-maximum eligibility path.
- Test a modifier on stamina itself: the update changes stored stamina without compounding the modifier into the base. Preserve effective rate inputs.
- Test zero delta at zero stamina, a negligible delta with an intervening debit, reduced maximum, upper/lower saturation, missing unit, and invalid numeric inputs. Refusals preserve the manager.
- Check death/collapse requests consume the engine-returned values, respect existing pose guards, and retain the separate cross-resource revive policy. Use fixture values from the three shipped stamina configurations rather than only one synthetic resource.

Use the quiet headless harness, defined rate inputs, and controlled barriers/hooks rather than sleep-based race probabilities. No graphical session or generated world is required.

Run:

```bash
cabal test synarchy-test-headless --test-options='--match "stamina resource commit"'
cabal test synarchy-test-headless --test-options='--match "maxStaminaFor resolves max_stamina through effective stats"'
cabal test synarchy-test-headless --test-options='--match "Combat admission revalidates at commit"'
python3 tools/lua_module_budget.py
```

If helper extraction changes an existing test's describe/module placement, retain equivalent focused coverage and record the updated command. Run additional gates only for their changed inputs; full CI, broad physiology probes, worldgen baselines, and save-compatibility regeneration are not default acceptance work for this issue. New code must compile warning-free under the repository's normal production profile.

## Scope boundaries

This issue does not introduce a universal resource API, change stance recovery, redesign simulation clocks, change page visibility/simulation eligibility, or alter chunk residency. It does not guarantee that a transient zero occurring entirely between resource commits is latched forever; a combat debit arriving after the commit remains visible to the next normal physiology tick. It preserves the existing consequence policy using an authoritative observation.

The same PR may document the mutation contract and update the finding's narrative evidence. It must leave the canonical report's checklist and heading disposition fields to the report-processing lane.

## Related

- [CH-2: reproduced stamina overwrite and missed exhaustion checks](simulation_consistency_findings.md#ch-2-lua-stamina-updates-can-erase-combat-costs-and-bypass-exhaustion-checks).
- [Controlled Lua reproduction](audit_evidence/2026-09-05/stamina_interleaving.lua).
- [#1735](https://github.com/coghex/synarchy/issues/1735): effective combat maximum policy, already landed.
- [#2328](https://github.com/coghex/synarchy/issues/2328): atomic strike admission and spending, already landed.
- [Stance recovery design](stance_recovery_design.md): adjacent independent repair, explicitly excludes stamina.
- [Architecture audit and owner direction](architecture_conversation_audit_2026-09-05.md).

Creation-time deduplication found no matching open stamina-overwrite issue. Refresh before filing. When publishing the issue body, convert the relative document links to repository links at the available revision; these drafts currently live uncommitted in the docs worktree and must not be presented as already published files.
