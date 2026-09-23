# Mental-efficiency probe findings

One verified probe-fixture defect from the September 22, 2026 deflake diagnosis,
prepared for one-at-a-time disposition through `$process-report`.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition.

## Methodology

Investigated `mental_efficiency` at revision
`7adcfa31c1e007b7c5409e9e3876b6f79d32911b`. Its retained measurement at
`2026-09-22T14:31:33Z` failed in all 10 runs, with the same two failing checks
and 18 passing checks each time. Authentic results were validated and all
retained event streams and engine logs inspected. An isolated checkout of that
revision reproduced the failure and tested a single fixture change, including
a far → near → far reversal. Current primary-checkout code was rechecked at
the same revision when preparing this report.

Experiments used macOS on Apple Silicon, GHC 9.12.2, Cabal 3.16.1.0, the
recorded configuration hashes, and a headless engine on port 9353 with four
RTS capabilities. No implementation change has been delivered. Tracker
deduplication and issue disposition remain for `$process-report`.

The [full diagnosis](deflake_diagnoses/mental_efficiency_20260922T143133Z.md)
records commands, configuration, measurement identity, ownership cleanup, and
the separate canonical handoff rejection. Local diagnostic evidence is retained
under `/Users/vincentcoghlan/work/.deflake-diagnostics/mental_efficiency_20260922T143133Z/`.

## Status

- [ ] CH-1. Mental-efficiency combat fixture places unarmed targets outside melee reach

---

## 1. Combat sampling fixture

### CH-1. Mental-efficiency combat fixture places unarmed targets outside melee reach

**Verification:** Verified, high confidence. The probe spawns its attacker at
`(2,2)` and target at `(4,2)`, removes the attacker's weapons, and pins height
to 1.8 m. This creates a 2-tile separation with only 0.75 tiles of unarmed
reach. The combat worker correctly refuses every attack as `out_of_reach`.
The sampling loop discards non-hit events, exhausts its attempts, and reports
zero hits at both mental-effectiveness settings.

Consequently, both `combat_samples` and `damage_energy_unchanged` fail.
The damage check receives two absent means and uses an infinite fallback
ratio; it never compares landed-hit energy. This consistently fails the probe
and prevents its combat section from exercising the intended invariance.
Both symptoms belong to this single fixture concern.

**Evidence:**

- `tools/mental_efficiency_probe.py:375` — spawns the pair two tiles apart;
  lines 387 and 394 remove weapons and pin attacker height to 1.8 m.
- `src/Combat/Resolution/Admission.hs:87` — `attackRangeTiles` computes
  `height / 2.4 + blade_length / 100`, yielding 0.75 tiles here.
  `checkAdmission` at line 134 refuses separation greater than live reach.
- `tools/mental_efficiency_probe.py:407` — enqueues attacks and collects only
  `hit` events, losing the explanatory refusal reason.
- `tools/mental_efficiency_probe.py:555` — attempts up to 20 swings per
  effectiveness setting; absent means produce the failing fallback ratio.
- `test-headless/Test/Headless/Combat/Admission.hs:361` — existing tests require
  refusal outside melee reach and acceptance at the reach bound.
- `test-headless/Test/Headless/Combat/MentalEffectiveness.hs:265` — deterministic
  tests own the damage-energy/recovery invariance that this live-engine probe
  is intended to complement.

**Reproduction and causal evidence:**

| Experiment | Result |
|---|---|
| Original retained measurement, 10 complete runs | Both combat checks failed in every run: `lo=0 hi=0`, both damage means absent. |
| Minimal original separation, one attempt at each effectiveness | Two `refused/out_of_reach` events; queried reach `0.74999994039536`. |
| Minimal target moved to `(2.5,2)` | Two landed hits, each with raw energy `79.416626`. |
| Minimal original separation restored | Both requests again refused as `out_of_reach`. |
| Complete original probe reproduced | Exit 1; 40 out-of-reach refusals, zero hits, the same two failed checks and 18 passing checks. |
| Complete probe with only target x changed from 4 to 2.5 | Exit 0; all 20 unchanged checks passed, six hits per effectiveness, mean energy ratio `1.000`. |

The trial changed the spawn command through an external diagnostic wrapper;
tracked probe and production source remained unchanged. It retained the
original assertions, sampling limits, and timing. Transcripts recorded the
probe's own drain results without introducing a competing log consumer.

The retained `experiment-summary.json`, `minimal/console.jsonl`,
`full-baseline/events.jsonl`, `full-near/events.jsonl`, and `experiment.py`
provide the evidence and reproducer beneath the local artifact directory above.
The full diagnosis includes the exact invocation commands.

**Handoff context:**

- **Current behavior:** The combat fixture reliably requests invalid attacks;
  the second assertion reports a damage comparison failure without samples.
- **Expected direction:** The fixture must place combatants within valid live
  melee reach and collect real hits at both effectiveness settings. Rejected
  attacks should provide enough diagnostic detail to explain fixture failure.
  The successful half-tile trial demonstrates a narrow corrective direction.
- **Scope and constraints:** Preserve production reach enforcement, the
  unarmed/pinned-stat comparison, both effectiveness settings, existing check
  meanings, and energy tolerance. Do not compensate with longer waits, more
  retries, or weakened assertions. Validate the complete corrected probe and
  relevant focused tests; keep any required implementation documentation and
  verification evidence in the eventual code PR.
- **Remaining uncertainty:** The cause is demonstrated at the recorded revision.
  One passing full diagnostic trial does not establish general flake
  elimination. No ten-run committed-repair verification or Hspec run was
  performed during diagnosis. Recheck current code and tracker duplicates
  before drafting an issue.

The canonical diagnosis gate separately rejected the original handoff because
it originated in `synarchy-flake-60c7168f` rather than the primary checkout.
The authentic handoff was not changed, and no canonical outcome was recorded.
That workflow limitation does not invalidate the directly observed fixture
defect; it must not be represented as a successful canonical repair verdict.
Resolving the handoff-origin contract is outside this finding's scope.
