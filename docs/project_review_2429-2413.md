# Project Review Findings: PRs #2429–#2413

Reviewed `coghex/synarchy` against current source at
`e89e61bb112318765e44728fbbf31b86f3426499`: PRs #2429, #2428, #2427,
#2426, #2424, #2423, #2422, #2421, #2418, #2420, #2419 and #2413, in
merge order, plus direct documentation commit `88a952ab6` within their landing
interval. This report preserves a current probe defect encountered while
checking #2428's reported validation failure; the comment-only PR did not
introduce it. No production code or tracker artifact was changed.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. The lash-out preference probe can grade an expired attacker as a policy failure

## 1. Mental-state probe fixture validity

### PRR-1. The lash-out preference probe can grade an expired attacker as a policy failure

> **Captured note:** Phase 9a of `mental_state_probe.py` needs to establish
> that its staged attacker is still recent and eligible when lash-out chooses
> a target. It currently requires that attacker even after intervening setup
> can consume the production ten-second preference window. PR #2428 reported
> this assertion failing and then passing on a rerun; that observation prompted
> this check, but does not establish which fixture condition caused that run.

**Verification:** Complete current-source trace plus a controlled execution of
the actual `pickLashoutTarget` closure from `scripts/unit_ai_mental.lua`, reached
through Lua's debug upvalue inspection without editing the module. With a
standing attacker one tile away, a standing decoy half a tile away, and a hit
timestamp of zero, the production picker returned the attacker at game time 9
and the decoy at game time 11. Both are correct. The probe's unconditional
attacker assertion accepts only the first result, although its post-hit setup
contains independent waits that permit the second precondition. No live engine
or wall-clock flake-frequency experiment was run for this finding.

**Evidence:**

- `tools/mental_state_probe.py:672` — the real-hit poll checks attacker identity,
  not the hit timestamp or its age at eventual target selection.
- `tools/mental_state_probe.py:699` — subsequent commands clear combat state,
  stop the units, revive them, read position, and request a teleport; the
  simulation is not paused during this setup.
- `tools/mental_state_probe.py:723` — teleport settlement may consume five
  seconds before setup continues.
- `tools/mental_state_probe.py:733` — the closer decoy is spawned only after
  the hit and teleport, through `spawn_acolyte`.
- `tools/probelib.py:509` and `tools/probelib.py:522` — that helper allows up
  to ten seconds for the new unit's AI state and water-goal clear. Each console
  operation also has its own reply/idle wait (`:58`). These are not a shared
  budget measured against the hit's game-time age.
- `tools/mental_state_probe.py:735` and `tools/mental_state_probe.py:742` —
  force the break and poll for the first non-nil target without revalidating
  hit age, attacker eligibility, or the setup's measured distances.
- `scripts/unit_ai_mental.lua:36` and `scripts/unit_ai_mental.lua:106` —
  preference applies only when `gameTime - att.at <= 10` and the attacker is
  eligible; otherwise the nearest eligible unit is the correct choice.
- `src/Engine/Scripting/Lua/API/Units/Combat.hs:444` — the public attacker
  query exposes the stored timestamp as well as identity, so the fixture can
  observe the condition it currently omits.

**Handoff context:**

- **Current behavior:** A successful setup can outlive the recent-attacker
  window and report a false policy failure when the engine correctly selects
  the nearer decoy. Reviving an attacker earlier in setup is also not an
  observation of its eligibility at selection time.
- **Expected behavior:** Establish and retain the preference case's actual
  preconditions at the decision boundary. A fixture that cannot establish them
  must fail as setup, not grade correct fallback behavior as a production
  regression. Preserve a genuine recent-attacker-over-nearer-decoy assertion;
  accepting either target or passing a retry would remove its coverage.
- **Scope and constraints:** A bounded phase-9a fixture correction, retaining
  the real-hit path and the downstream real-attack checks. Do not widen the
  production preference window, change target policy, or globally freeze game
  time merely to make the assertion pass. Prefer observable synchronization
  and explicitly controlled decision timing over additional sleeps.
- **Verification target:** Cover immediate valid selection, delayed setup
  crossing ten game seconds, and an attacker becoming ineligible. The first
  must still require the attacker; the latter cases must be classified or
  restaged deliberately. Verify the correction through the actual AI selection
  path, not only a duplicate policy implementation.
- **Deduplication:** All-state searches for `mental_state_probe`, attacker,
  lash-out, race and window found #717, #1483, #1709 and #1713, whose repairs
  concern different behavior. Open #2521 acknowledges the existing lash-out
  flake and treats the probe as A/B evidence during faction migration; it does
  not specify a repair for this expired-attacker fixture. Coordinate with that
  migration before choosing the final test setup; do not duplicate its faction
  work. Existing local reports cover retaliation and airborne/lunge defects,
  not this missing recency precondition.
- **Remaining uncertainty:** This proves an admitted false-failure schedule,
  not the cause of #2428's particular failed run. The relative contributions
  of age expiry, subsequent movement and renewed combat in ordinary runs remain
  unmeasured.
