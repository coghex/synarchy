# mental_efficiency — 2026-09-22T14:31:33Z

Status: concluded — confirmed probe fixture defect; canonical workflow acceptance blocked by the original handoff gate.

## Measurement and selection

- Probe: `mental_efficiency`; revision: `7adcfa31c1e007b7c5409e9e3876b6f79d32911b`.
- Measurement: `2026-09-22T14:31:33Z`, invocation `mental_efficiency-20260922T143133Z-91889-c6081a87`.
- Current cohort: one complete 10-run sample; 10 failures, no timeouts, 18 checks pass in every run. Tolerance 0/10; canonical summary says fresh and over tolerance. No deferral or prior outcome; no prior diagnosis directory existed. Selected ahead of stale `lua_orphan_prune`; `role` is deferred.
- Targets: `combat_samples` and `damage_energy_unchanged`, both FAIL in every run; no MISSING targets.
- All ten event streams show `lo=0 hi=0` and both damage means `None`. This is a missing-hit symptom, not evidence of unequal damage energy.
- Result validated with `probe_census.validate_result`; producer handoff's embedded result equals the retained result. Probe, commit, timestamp, invocation, and paths agree with the census sample. All retained stdout/event/engine-log files inspected; stdout is empty and no separate stderr file is retained. Engine logs show ordinary boot/arena initialization, audio underrun warnings, and unrelated structure-art warnings.

Original evidence directory:
`/private/var/folders/xs/kyf0vrg92c340wk3jncyp1fr0000gn/T/synarchy-probe-flake/mental_efficiency-20260922T143133Z-91889-c6081a87`

[Original result](/private/var/folders/xs/kyf0vrg92c340wk3jncyp1fr0000gn/T/synarchy-probe-flake/mental_efficiency-20260922T143133Z-91889-c6081a87/probe-flake-result.json)
[Producer handoff](/private/var/folders/xs/kyf0vrg92c340wk3jncyp1fr0000gn/T/synarchy-probe-flake/mental_efficiency-20260922T143133Z-91889-c6081a87/probe-flake-result.json-handoff.json)

## Canonical gate

Command from the primary checkout:

```sh
python3 tools/deflake_diagnosis.py --handoff /private/var/folders/xs/kyf0vrg92c340wk3jncyp1fr0000gn/T/synarchy-probe-flake/mental_efficiency-20260922T143133Z-91889-c6081a87/probe-flake-result.json-handoff.json --json
```

Exit 2, exact rejection:

> deflake_diagnosis: handoff-rejected: handoff.invocation ran in /Users/vincentcoghlan/work/synarchy-flake-60c7168f, which is not the primary checkout /Users/vincentcoghlan/work/synarchy; `/deflake` runs there — before this workflow's comparison worktrees exist — so a handoff from anywhere else names a checkout nothing has established is one

The handoff was not modified. This operational gate rejection is separate from the probe failure. No canonical outcome or repair acceptance has been recorded; the required accepted original handoff and controlled ten-run comparison batches are absent.

## Ownership and experiment setup

Canonical claim token `54b50a7d0a4e49e691776b794c77cd1d`, PID `29003`, acquired `2026-09-22T15:18:44.983336Z`; 180-second lease renewed every 30 seconds by a persistent owner process. Acquisition audited through `probe_census.record_claim`; deferral and measurement identity rechecked after acquisition. Requested-runs field mirrors the selected 10-run evidence, not a new measurement.

Isolated detached checkout:
`/Users/vincentcoghlan/work/synarchy-deflake-mental-20260922`, pinned to the recorded commit.

Diagnostic artifacts and scripts: `/tmp/deflake-mental-20260922/`.
Preparation uses `probe_engine.prepare_executable(timeout=1800)` before any measurement resource hold. Experiments acquire the registry-declared shared `repo-config` and `cabal-build` holds; install the producer's three configuration files only after verifying their SHA-256 hashes; run headless on port 9353 with four RTS capabilities. The claim is checked before every diagnostic console call.

## Hypothesis tested

The unchanged probe spawns attacker `(2,2)` and target `(4,2)`, removes weapons, and pins attacker height 1.8 m. `Combat.Resolution.Admission.attackRangeTiles` permits `1.8 / 2.4 = 0.75` tiles unarmed. The separation is 2 tiles. `checkAdmission` refuses out-of-reach requests before hit resolution; `combat_damage_sample` ignores non-hit events, so its damage comparison later lacks data.

Relevant owners: `tools/mental_efficiency_probe.py:353`, `src/Combat/Resolution/Admission.hs:79`, `src/Combat/Resolution.hs:121`, `src/Engine/Scripting/Lua/API/Combat.hs:45`. The Lua API only enqueues; its true return is not proof of a landed attack. Commit `27843fc1f342bd2ce965f1bd1f525b38a7a40e7c` introduced the reach revalidation for #2328; history identifies the compatibility change, but no historical endpoint or bisect has been executed.

The planned discriminating experiment was far → near (target x=2.5) → restored-far at both effectiveness settings, followed by complete baseline and one-factor trial runs. All were completed below.

## Experiments and results

All runtime experiments used the recorded revision, not a newer-code comparison. Source remained unmodified; the trial changed one target-spawn command through the retained Python wrapper. The original `combat_damage_sample` and `_run` functions executed unchanged, including sampling limits, assertions, and timings. Console instrumentation recorded the same drain return values the probe read; it did not add a competing event consumer. Only the minimal experiment added pre-attack position/range observations.

Preparation:

```sh
git worktree add --detach /Users/vincentcoghlan/work/synarchy-deflake-mental-20260922 7adcfa31c1e007b7c5409e9e3876b6f79d32911b
python3 /tmp/deflake-mental-20260922/prepare.py
```

The helper called `probe_engine.prepare_executable` with an 1800-second aggregate allowance and no supplied executable. It waited approximately eight minutes behind `deflake portal_location` (PID 34058) then `deflake resource_root` (PID 4177), without bypassing or cancelling either holder. It acquired the exclusive build hold and successfully ran `cabal build exe:synarchy` and `cabal list-bin exe:synarchy`. Exit 0. Both original and diagnostic build plans identify `ghc-9.12.2`, `osx`, `aarch64`; Cabal 3.16.1.0. No global toolchain changes.

Actual experiment commands (each stdout/stderr was redirected to the corresponding `*-output.txt`):

```sh
python3 /tmp/deflake-mental-20260922/experiment.py minimal
python3 /tmp/deflake-mental-20260922/experiment.py full-baseline
python3 /tmp/deflake-mental-20260922/experiment.py full-near
```

The wrapper changes cwd to the isolated checkout, takes shared resource holds, verifies/copies the original configuration, and supplies the prepared engine via `SYNARCHY_PROBE_ENGINE_EXE`. Each engine uses `--headless --port 9353 +RTS -N4 -RTS`; `SYNARCHY_ROOT` is unset. Actual configuration hashes and launcher identity are in each `environment.json`.

| Experiment | Observation | Exit |
|---|---|---|
| Initial minimal setup attempt | Missing diagnostic `engine/` log directory caused `FileNotFoundError` before engine launch. Preserved separately; corrected wrapper directory creation. This is not a probe result. | 1 |
| Minimal original separation, one attempt per effectiveness | Actual separation 2.0, range `0.74999994039536`; both requests emitted `refused`, reason `out_of_reach`; no hit values. | Part of minimal exit 0 |
| Minimal target x=2.5, one attempt per effectiveness | Actual separation 0.5, same range; both requests landed, raw energy `79.416626` in each. | Part of minimal exit 0 |
| Minimal restored target x=4, one attempt per effectiveness | Both requests again emitted `refused/out_of_reach`; no hit values. | Part of minimal exit 0 |
| Complete original probe with transcript instrumentation | Exactly 40 `refused/out_of_reach` events, zero hits. `combat_samples`: `lo=0 hi=0`; both damage means `None`. Same two failed checks and 18 passing checks as retained evidence. | 1 |
| Complete probe, only target x=4 → x=2.5 | All 20 original checks PASS. Six landed hits per effectiveness, both mean raw energies `79.417`, ratio `1.000`; no refusal. Cooldowns identical. | 0 |

## Conclusion and corrective direction

**Confirmed, high confidence: a consistent probe fixture defect, not an intermittent failure or demonstrated damage-formula regression.** The fixture requests unarmed attacks across 2 tiles despite only 0.75 tiles of reach. The combat worker correctly refuses them. The sampling loop silently discards refusal events and finishes with empty samples; `damage_energy_unchanged` then fails because its fallback ratio is infinity when both means are absent. The second failure is downstream of unavailable combat samples, not an observed change in damage energy.

The controlled far → near → far experiment establishes the causal chain; the complete original reproduces the selected failure, and a one-factor fixture intervention makes every original check pass. Retained failures on ten original runs show consistency at this revision; one complete passing diagnostic trial is not proof of general flake elimination.

Smallest corrective direction: position the unarmed target comfortably inside the attacker's queried live reach (the trial used `(2.5,2)` versus attacker `(2,2)`), verify the fixture's page/range preconditions, and include refusal kind/reason in sampling diagnostics. Preserve the existing effectiveness settings, energy assertion, check identities, and production admission rules. Do not lengthen timeouts, increase retries, or loosen the energy tolerance. The existing combat-admission tests explicitly require refusal outside reach and acceptance at the bound; the probe must respect that contract.

No production or probe source change was made, no issue/PR was created, and documentation remains local. No Hspec suite was executed: validation here is the real-engine minimal reversal and complete baseline/trial probes. A delivered repair would need its own focused validation and canonical controlled batches, after the authentic handoff-origin incompatibility is resolved. No canonical outcome was recorded, no census measurement or tolerance changed, and no deferral resumed. Only the claim acquisition audit was added to the census.

## Retained evidence and cleanup

A durable local copy of the diagnostic scripts, build log, gate result, console transcripts, engine logs, protocol events, original-evidence summary, environment manifests, and exits is retained at:

`/Users/vincentcoghlan/work/.deflake-diagnostics/mental_efficiency_20260922T143133Z/`

[Experiment summary](/Users/vincentcoghlan/work/.deflake-diagnostics/mental_efficiency_20260922T143133Z/experiment-summary.json)
[Minimal transcript](/Users/vincentcoghlan/work/.deflake-diagnostics/mental_efficiency_20260922T143133Z/minimal/console.jsonl)
[Original full probe events](/Users/vincentcoghlan/work/.deflake-diagnostics/mental_efficiency_20260922T143133Z/full-baseline/events.jsonl)
[Nearby full probe events](/Users/vincentcoghlan/work/.deflake-diagnostics/mental_efficiency_20260922T143133Z/full-near/events.jsonl)
[Diagnostic wrapper](/Users/vincentcoghlan/work/.deflake-diagnostics/mental_efficiency_20260922T143133Z/experiment.py)

The copied scripts retain their actual `/tmp/deflake-mental-20260922` working-artifact path. Original artifacts have not been edited or replaced. The isolated checkout is retained clean, with its built executable and ignored runtime configuration, for reproducibility. No tracked experimental edits exist. All three engines were stopped through `quit_engine`; their resource holds were released by managed context exit. The owner process releases only token `54b50a7d0a4e49e691776b794c77cd1d` on managed exit. The final ownership/process/clean-tree verification is retained in [cleanup.json](/Users/vincentcoghlan/work/.deflake-diagnostics/mental_efficiency_20260922T143133Z/cleanup.json).
