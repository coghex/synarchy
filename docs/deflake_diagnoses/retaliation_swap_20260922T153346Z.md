# retaliation_swap — 2026-09-22T15:33:46Z

Status: concluded — confirmed fixture failure and uncontrolled lunge damage; exact original damaging hits remain unproven.

## Selection and measurement identity

Explicitly selected first in the owner's requested order, before `river_naming`.
Revision `7adcfa31c1e007b7c5409e9e3876b6f79d32911b`; measurement
`2026-09-22T15:33:46Z`; invocation `retaliation_swap-20260922T153346Z-37189-43d7f849`. Current cohort contains one
accepted ten-run sample: 2 failures, 8 passes, tolerance 0, fresh at selection.
No deferral, previous diagnosis, or canonical outcome existed. Claims were empty
at selection. Raw result validated with `probe_census.validate_result`; producer
handoff's embedded result, probe, commit, timestamp, and invocation matched.

[Original result](/private/var/folders/xs/kyf0vrg92c340wk3jncyp1fr0000gn/T/synarchy-probe-flake/retaliation_swap-20260922T153346Z-37189-43d7f849/probe-flake-result.json)
[Original handoff](/private/var/folders/xs/kyf0vrg92c340wk3jncyp1fr0000gn/T/synarchy-probe-flake/retaliation_swap-20260922T153346Z-37189-43d7f849/probe-flake-result.json-handoff.json)

Targets are MISSING rather than failed assertions: `fresh_log`, `fresh_swap`,
`fresh_complete`, `fresh_sentinel`, `stale_staged_hit`, `stale_log`,
`stale_no_swap`, `stale_complete`. Both retained failures end with a fixture
warning before behavioral grading:

- Run 1: staging left five wounds and 41.3% blood; fresh-case precondition
  captured 41.4%, below the required 50% rise threshold. Only `export` and
  `fresh_staged_hit` were reached, both PASS.
- Run 9: all fresh checks PASS; the stale staging hit left three wounds and
  45.9% blood, then the stale-case precondition captured 45.2%. All seven
  reached checks PASS; the remaining three stale checks are MISSING.

All retained stdout, events, and engine logs were inspected. No retained log
reports `Lua error in update()`. These measurements demonstrate an intermittent
fixture failure, not a failing retaliation-swap assertion. Passing-run raw
artifacts were removed by the measurement producer; its result retains their
check maps. Retained files and diagnostic warnings are indexed in
`original-inspection.json` in the diagnostic artifact directory below.

## Canonical gate and coordination

`python3 tools/deflake_diagnosis.py --handoff <original handoff above> --json`
from the primary checkout exited 2:

> deflake_diagnosis: handoff-rejected: handoff.invocation ran in /Users/vincentcoghlan/work/synarchy-flake-60c7168f, which is not the primary checkout /Users/vincentcoghlan/work/synarchy; `/deflake` runs there — before this workflow's comparison worktrees exist — so a handoff from anywhere else names a checkout nothing has established is one

The authentic handoff was not modified. This rejection is separate from the
fixture failure. No canonical outcome or repair acceptance was recorded.

Canonical claim `7f9f77fe59e54c5aa6be6dee3237a908`, owner PID 81286, acquired
`2026-09-22T16:16:52.189736Z`. A persistent `probe_claim_lease.Renewer` renewed a
180-second lease every 30 seconds throughout investigation. Acquisition was
audited through `probe_census.record_claim`; deferral and measurement identity
were rechecked after acquisition. The claim's requested-runs value identifies
the selected ten-run measurement, not a new census measurement.

## Experiments and evidence

Reused the clean detached diagnosis checkout
`/Users/vincentcoghlan/work/synarchy-deflake-mental-20260922`, confirmed at the
recorded revision. `probe_engine.prepare_executable` completed a freshness
build and executable lookup under the exclusive build lock before measurement
holds. macOS/aarch64, GHC 9.12.2, Cabal 3.16.1.0; headless port 9483, four RTS
capabilities. Original configuration bytes were hash-verified and installed
under registry-declared shared `repo-config`/`cabal-build` resource holds.

Artifacts and scripts:
`/Users/vincentcoghlan/work/.deflake-diagnostics/retaliation_swap_20260922T153346Z`

Executed commands from the primary checkout; the script changes cwd to the
isolated checkout and supplies the freshly prepared engine:

```sh
python3 -u /Users/vincentcoghlan/work/.deflake-diagnostics/retaliation_swap_20260922T153346Z/experiment.py baseline 3
python3 -u /Users/vincentcoghlan/work/.deflake-diagnostics/retaliation_swap_20260922T153346Z/experiment.py near 1
```

The three baseline cases were fixed in advance, not retried until a result
changed. `experiment-baseline.py` retains the exact baseline wrapper.
Instrumentation delegates all original APIs: it records forwarded attack
arguments, mirrors events read by the existing combat-log consumer, and
captures blood/wounds before and after staging. It introduces no competing
log drainer. Diagnostic console calls check live claim ownership.

All three baseline cases passed all ten checks. The original sub-50%-blood
failure was **not reproduced** in this bounded sample. However, the trace
exposes an uncontrolled damage path:

| Case/window | Landed hit | Raw energy | Blood after staging |
|---|---|---|---|
| Baseline 1 fresh | ordinary stab | 0.6200132 | 100% |
| Baseline 1 stale | lunge stab | 78.83821 | 99.38% |
| Baseline 2 fresh | ordinary slash | 0.48514697 | 100% |
| Baseline 2 stale | lunge stab | 259.37527 | 96.94% |
| Baseline 3 fresh | ordinary slash | 2.004802 | 97.6% |
| Baseline 3 stale | ordinary slash | 1.2314911 | 100% |

The strongest lunge landed with attacker strength only `0.016751945` and
impact speed `3.397891`, producing both a stab wound and a fracture. Exactly
one hit per staging leg was observed; extra landed attacks are not needed to
explain why the alleged per-hit damage bound is unsound. The original failing
runs lack attack arguments and hit payloads, so this does not prove their
specific wound distributions came from lunges.

Relevant code:

- `tools/retaliation_swap_probe.py:463`: `neuter` limits `strength_base` and
  increases toughness, claiming this bounds what one swing costs.
- `tools/retaliation_swap_probe.py:868`: `stage_hit` starts live AI combat
  without first establishing ordinary melee geometry. The flanking geometry
  at line 842 is used later by `run_case`, after the damaging setup.
- `scripts/unit_ai_combat_lunge.lua:174`: out-of-reach AI attacks can choose a
  lunge; both the landing and in-place pounce pass impact speed to `combat.attack`.
- `src/Combat/Resolution/Damage.hs:240`: strength scales muscular swing work.
  Lines 273–286 add full-body lunge energy/momentum independently of strength:
  `0.5 * lungeMomentumScale * bodyMass * speed^2` in the energy channel.
- `src/Combat/Resolution.hs:414`: one landed hit can create multiple wounds.
- `src/Combat/Wounds/Tick.hs:108`: wound/blood ticks traverse engine unit state,
  independently of the probe's Lua `unit.getAllIds` wrapper. Excluding the
  subject from Lua AI iteration does not suspend its bleeding.
- `tools/retaliation_swap_probe.py:1005`: `check_preconditions` correctly refuses
  to grade subjects at or below the 50% blood threshold.

The `near` trial establishes staging geometry before the real-hit leg: suspend
Lua unit enumeration briefly, stop the pair, then place the attacker 0.1 tiles
from the subject. The original stage function restores attacker-only ticking.
The original real hit, fresh/stale checks, survival gates, and sampling/time
limits remain intact. No production or tracked probe source is edited.

## Conclusion and remaining gap

Confirmed fixture failure: the real-hit staging leg can spend too much of its
subject's blood before the retaliation branch is graded. Confirmed design gap:
limiting strength does not bound the lunges the fixture actually issues.
Lunge damage is the likely explanation for the original destructive hits,
with moderate causal confidence for those exact two runs because their hit
payloads were not retained and the three new baselines did not fail.

Corrective direction: establish and verify safe ordinary-melee geometry before
staging, preserving a real recent-attacker record and all later branch checks;
retain attack/refusal and wound details when fixture setup fails. Do not relax
the blood threshold, fabricate a recent attacker, or retry failed fixtures
until green. Also correct stale fixture comments claiming blood never recovers
and that narrowing Lua enumeration suspends engine wound ticks; neither claim
holds at this revision, although slow recovery does not repair this setup.

No production fix, tracker issue, PR, or publication is part of this diagnosis.
A future repair needs bounded repeated validation; one passing trial is not
proof of elimination. These trials do not satisfy canonical ten-run committed
repair gates. Measurements and tolerance remain unchanged.

## Completed trial and cleanup

The close-range trial exited 0 and passed all ten unchanged checks. Both real
staging hits were ordinary attacks: every forwarded call had three arguments,
with no lunge impact/reach. Fresh blood remained 100%; stale blood was 99.94%
after dressing and 100% at grading. This demonstrates the proposed way to
exclude the uncontrolled lunge path; it does not prove that every original
failure had that cause or that the probe is now generally reliable.

Four diagnostic engines (three baselines, one trial) shut down through the
probe's `quit_engine` finally block. Measurement resource holds were released
on each managed exit. The canonical claim is released by `owner.py` on the
release marker; final verification is retained in `cleanup.json` beside the
artifacts. The clean detached checkout remains for the next requested probe.
No canonical outcome was recorded.
