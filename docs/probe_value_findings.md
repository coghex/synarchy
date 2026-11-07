# Probe value findings

Which of Synarchy's 103 registered behaviour probes earn their runtime, grouped into concerns with a proposed disposition for each and ranked by expected savings. Analysis only: no probe, engine or suite was run to produce it.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

**Request.** The owner asked on 2026-11-07 (request `user-syn-probe-streamline-20261107`, item 4 of the probe and streamlining plan) to find which probes earn their runtime. Method follows kanban epic coghex/kanban#785 ("Cut the test suites down to what earns its runtime"): cost measured per unit, cost concentrated in a few units, duplicated scenarios named, then a disposition per concern. Findings are ordered by expected savings, largest first.

**Cost basis.** Existing measurements only; where none exists the table says `n/m` (not measured) and no number is estimated.

- *Run C / A / B.* The three quruntul shakedown runs, which executed every registered probe once through `tools/run_probes.py --only KEY --exact --jobs 1`: run C `5578e845` on 2026-10-19 at `acc77479e` (98 of the 103 current probes measured), run A `82223792` on 2026-10-04 at `c42dc9441`, run B `6816afa8` on 2026-10-05 at `110d9ae90`. Source: `result.json` under `<git-common-dir>/quruntul/runs/<run id>/` (local lab state, not tracked; a compact copy of the per-probe figures used here is kept under `~/.local/state/synarchy-probe-value-20261107/`). Each figure is one trial, wall-clock, serial, and includes engine boot; `FAIL` marks a failed trial (a trial that hit the runner's 900 s cap reads 900). They predate master by 19 days (run C) to 34 days (run A), so a figure is evidence of cost at that revision, not of today's cost.
- *Census.* `docs/probe_census.json` holds timing samples for only 11 of 103 probes (newest 2026-09-02), so it cannot rank anything; see PRB-14.
- *CI.* `tools/probe_runner_registry.py:577-599` (`PROBE_EXPECTED_SECONDS`, measured on PR run 33666483367 on 2026-09-02 plus later additions) for the 14 CI-eligible probes.

**Totals (run C).** 98 probes measured, 14,634 s (4.1 h) serial. The ten slowest are 6,878 s (47%), the twenty-five slowest 10,820 s (74%); 68 of the 98 measured probes cost under 100 s each, so merging small probes saves boot overhead only (`text_encoding` boots, runs and quits in 3 s). Nine probes failed in run C (3,320 s, 23% of the total): `persistence_contract_sweep`, `farm_ai`, `repair_ai`, `physiology`, `tutorial_hud`, `location_overlay`, `follow_command_priority`, `construction_blueprint_footprint`, `combat_anim`.

**Savings estimate.** Firm, meaning the whole measured cost of a probe that is retired, made opt-in, or stripped of work another registered probe already does: `expedition_loop` 1,119 s + `persistence_contract_sweep` up to 900 s (its cap) + `unified_transfer` 726 s + `repair_ai` 680 s = **3,425 s, 23% of run C**. Further, only bounded above by the current time of the probes involved and not estimated: `farm_ai`, `construction`, `bleeding_trail`, `cargo_capacity` (1,806 s together), the merges (219 s) and `tutorial_hud` if it is retired (269 s). If every bound were fully realised the saving would be 5,719 s (39%). Retiring or merging manual probes saves lab time, not pull-request CI time; only PRB-8 touches the CI lane.

**Value.** What each probe alone guards was read from its module docstring, `MANUAL_ONLY_REASONS` in `tools/ci_probes.py`, and the Hspec groups that cite the same issue numbers (`test-headless/`). Hspec overlap was matched by issue number and group name, then spot-checked by reading the named groups; it was not compared assertion by assertion, and each finding says so where it matters. "Last caught a real regression" could not be established for any probe: see PRB-14.

**Constraints observed.** In-process tests do not prove real-engine behaviour (during #2557 an in-process check missed plants the running engine loaded around the camera), so every proposal to replace or retire names the behaviour it would lose. `persistence_contract` is the documented real-engine save/load gate (`docs/persistence_contract.md:652`) and stays. Owner decisions respected: simulations are optional, local and never CI (`z3hd4feg`, 2026-10-08, `tools/ci_probes.py:136-150`); `expedition_retrieval` retired as too sandbox-dependent (#2813); legacy-save migration probes are opt-in or gone (#2912). Not repeated here: #2931 (keep failing-probe evidence) and #2932 (machine-wide GPU/desktop probe lock). Not a finding either: #531 found that merging many probes onto one long-lived engine is infeasible because their state is global, so every merge below is of two probes with the same fixture family, saving one boot.

## Status

- [ ] PRB-1. `expedition_loop` costs 1,119 s per run and is the lab's slowest probe
- [ ] PRB-2. `persistence_contract_sweep` re-runs nine probes it does not own and has hit its cap in all three runs
- [ ] PRB-3. `farm_ai` scans natural terrain over TCP and costs 782 s
- [ ] PRB-4. `unified_transfer` re-proves what three stage probes and Hspec already gate
- [ ] PRB-5. `repair_ai` is flaky and shadowed by an Hspec group; `construction` costs 3-4x its recorded time
- [ ] PRB-6. `bleeding_trail` spends its time in real-time dwells that Hspec already grades
- [ ] PRB-7. `tutorial_hud` failed three runs of three with no retained reason
- [ ] PRB-8. The CI probe set: `cargo_capacity` loops to overfill a 200 kg store; `persistence_contract` varied 4.5x
- [ ] PRB-9. Three pairs of companion probes pay two boots for one fixture
- [ ] PRB-10. Save/load and fluid fresh-process probes: keep, with one cost to explain
- [ ] PRB-11. Physiology, mind, combat and AI probes: keep, with three red in the latest run
- [ ] PRB-12. Location, rendering and GPU probes: keep, with two red in the latest run
- [ ] PRB-13. Boot, config and encoding contract probes are cheap and earn their keep
- [ ] PRB-14. The cost and value data needed to rank probes is not tracked

---

## Long scenarios and nested probes

### PRB-1. `expedition_loop` costs 1,119 s per run and is the lab's slowest probe

`expedition_loop` plays the first expedition end to end across two engine boots and is the single largest line in the sweep: 1,119 s in run C (one probe is 7.6% of the sweep), 1,132 s in run B, and in run A it hit the then-default 900 s cap. It is manual-only and the quruntul lab offers every probe that is not deferred or opt-in, so each lab rotation spends about 19 minutes on it.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `expedition_loop` | manual | 1119 | 900 FAIL / 1132 | keep, but opt-in only |

**Evidence:**

- `.git/quruntul/runs/5578e845-1297-4390-a09b-d356e27c495d/result.json` (run C) — 1,119 s, passed; run B 1,132 s passed; run A `TIMEOUT (900.0s)`.
- `tools/probe_runner_registry.py:504-505` — the override that exists because the scenario takes about 30 minutes; "a 3600s budget with margin, not a measured runtime".
- `tools/ci_probes.py:565` — manual-only on real-worldgen grounds; `tools/expedition_loop_probe.py` has 31 commits, the second most-touched probe script.
- `.quruntul/adapter.py:24` and `:52` — `OPT_IN_ONLY` already removes a probe from lab selection without unregistering it (#2912 did this for the legacy-save migration probe); it is empty today.
- Issue #2813 — the owner retired `expedition_retrieval` on 2026-10-12 because it failed 4 of 7 runs on an unchanged base; `expedition_loop` passed 2 of 3 runs here and only lost run A to the timeout.

**Handoff context:**

- **Current behavior:** Every lab rotation runs it; it is the only probe over the integrated loop (colony, water, provisioning, journey, discovery, extraction, confrontation, return, fresh-process reload).
- **Expected direction:** Keep the probe and its documented gate for changes to the expedition arc, but add it to the adapter's opt-in set so the routine lab rotation stops scheduling it. Saves 1,119 s per rotation. Retirement, as with `expedition_retrieval`, is the owner's call and is not proposed because the evidence for it here is weaker (2 of 3 passes).
- **Scope and constraints:** The probe must stay runnable with `python3 tools/run_probes.py --only expedition_loop --exact`; `python3 .quruntul/checks.py` must stay green; docs that name it as the arc's final gate (its docstring cites `docs/expedition_gameplay_loop.md` step 9) need to say it is opt-in.
- **Remaining uncertainty:** How often the lab actually selects it (selection is staleness-driven, not read here); which phase dominates the 1,119 s (the probe reports a total only); whether the Hspec groups `Unit.RuinOccupantSurvival` and `Location discovery` cover enough of the loop to make an opt-in acceptable to the owner.

### PRB-2. `persistence_contract_sweep` re-runs nine probes it does not own and has hit its cap in all three runs

The sweep is a meta-probe: besides its own four-boot structural comparison it fans out to ten other registered probes by default. Those probes are already registered, measured and runnable on their own. It reached the runner's 900 s cap in runs A, B and C, so it has never reported a complete pass, and its own comparison fails on `COMPARE_MISMATCH`.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `persistence_contract_sweep` | manual | 900 FAIL | 900 FAIL / 900 FAIL | keep, but drop the nested fan-out |

**Evidence:**

- `tools/persistence_contract_sweep.py:178-189` — `SELECTABLE_CROSS_REFERENCED_PROBE_KEYS` = chop, till, crop, plant, construction, power, transactional_load, save_barrier, craft_bill, save_storage, persistence_integrity; the default set drops only `craft_bill` (docstring `:66`, "ten of eleven keys").
- Run C per-probe cost of the nine measured nested probes: till 74 + crop 53 + plant 62 + construction 560 + power 133 + transactional_load 158 + save_barrier 31 + save_storage 53 + persistence_integrity 96 = **1,220 s**; `chop` is census-deferred (no measurement).
- Run A log of the sweep: `FAIL: all 4 generations are structurally IDENTICAL ... COMPARE_MISMATCH` at +168.5 s, then the 900 s `TIMEOUT`.
- `docs/persistence_contract.md:652` — the compact `persistence_contract` probe is "the required engine-level save/load gate"; the sweep keeps "its own selection and ownership rules". `src/World/Save/CLAUDE.md:123` lists the nested probes individually.

**Handoff context:**

- **Current behavior:** One lab slot spends its full 900 s cap and reports `TIMEOUT`, duplicating probes that run separately.
- **Expected direction:** Keep the sweep's own contribution (a real generated world at worldSize 64 across four fresh processes) and stop running the nested probes by default, leaving `--cross-probe-keys` for explicit use. Saves up to 900 s per run. Separately the owner should decide what to do with the `COMPARE_MISMATCH`: if it is real save nondeterminism it is a product defect, not a probe defect, and the sweep is the only place it shows.
- **Scope and constraints:** `python3 tools/test_persistence_contract_sweep.py` guards the selectable list; `CLAUDE.md` names it as the self-test for changes to `SELECTABLE_CROSS_REFERENCED_PROBE_KEYS`; `tools/README.md` rows for the sweep must follow.
- **Remaining uncertainty:** The sweep's own phase on a passing run is unmeasured (it has never passed); whether the mismatch is nondeterminism or an artefact of the probe's comparison is undiagnosed.

### PRB-3. `farm_ai` scans natural terrain over TCP and costs 782 s

`farm_ai` is the slowest probe that is not a multi-boot scenario, and the registry itself attributes the time to a tile scan.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `farm_ai` | manual | 782 FAIL | 693 / 689 | keep, but give it its own fixture |
| `foraging` | manual | 81 | 84 / 92 | keep as is |
| `crop` | manual | 53 | 60 / 56 | keep as is |
| `plant` | manual | 62 | 69 / 64 | keep as is |
| `till` | manual | 74 | 76 / 77 | keep as is |
| `flora_growth` | manual | 48 | 51 / 50 | keep as is |
| `chop` | manual | n/m | n/m | keep as is (census-deferred) |
| `role` | manual | n/m | n/m | keep as is (census-deferred) |

**Evidence:**

- Run C 782 s (failed: `re-designating a claimed tile plants the NEW crop, not the stale one`), run A 693 s, run B 689 s.
- `tools/ci_probes.py:586-588` — "slowest registered probe (~11 min observed, O(n^2) TCP tile scan over natural terrain)"; `tools/probe_runner_registry.py:491-493` repeats it.
- `tools/farm_ai_probe.py:249-250` — nested `range(... , 4)` loops over a `span * 16` window.
- `foraging` got its own registered fixture species in #1766 ("a deterministic harvestable target instead of natural placement"; #2058 did the same for `chop`) and costs 81 s in run C; the other flora probes cost 48-74 s (`till` 74, `plant` 62, `crop` 53, `flora_growth` 48).
- Hspec groups citing the same issues (#331, #332, #336): `UnitAiHarvest`, `DesignationSeam`, `FloraGrowth`, `UnitAiYieldProximity` — matched by issue number only; they grade decisions in-process.

**Handoff context:**

- **Current behavior:** Every run pays the scan and depends on what worldgen happened to place.
- **Expected direction:** Give `farm_ai` the same kind of registered fixture the chop and foraging probes use, replacing the scan; keep the real-engine till, plant, grow and auto-harvest chain, which Hspec cannot walk. Saves up to 782 s; no estimate of the achievable runtime is made (the 81 s `foraging` figure shows only the scale of a fixtured AI probe). The red check should be diagnosed in the same change.
- **Scope and constraints:** Gates #336's `world.plantRowCropAt` placement path and the CropPlot scan in `world.findHarvestableFlora`; keep the persistence-leg of the probe.
- **Remaining uncertainty:** How much of the 782 s is the scan versus AI wait time; whether the red check is a probe or a game defect.

## Overlap with focused gates

### PRB-4. `unified_transfer` re-proves what three stage probes and Hspec already gate

The transfer arc (#1013) has a focused gate per slice. `unified_transfer` is the arc's integration gate, an offscreen boot that replays all of them in one session. It failed in runs A and B at different stages and passed in run C.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `unified_transfer` | manual | 726 | 886 FAIL / 658 FAIL | retire |
| `transfer_order` | manual | 504 | 563 / 663 | keep as is |
| `item_list_widget` | manual | 367 | 363 / 389 FAIL | merge with `transfer_context_menu` |
| `transfer_context_menu` | manual | 99 | 112 / 56 FAIL | merge with `item_list_widget` |

**Evidence:**

- `tools/unified_transfer_probe.py:1-13` — "Every earlier slice of the arc has its own focused gate. This one proves they hold together"; shaped after `expedition_loop`.
- Run A 886 s FAIL (stage `modeA`, 1 of 72 checks), run B 658 s FAIL (stage `save`: `session names the exact endpoint identities (got None)`, which cascaded into `load`), run C 726 s passed.
- Stage gates: `tools/transfer_order_probe.py:10` lists what only a real engine can prove (the walk, commit, restart-resume); `transfer_context_menu_probe.py` owns the menu routes; `item_list_widget_probe.py` owns the widget. Hspec files citing the same issues: `Transfer.hs`, `TransferOrderApi.hs`, `TransferGestures.hs`, `Container.hs`, `ContainerWindowStack.hs`, `NestedContents.hs`.
- `item_list_widget` and `transfer_context_menu` use the same throwaway 1x1 storage fixture and the same hit-test technique (both docstrings), and each boots its own offscreen engine (367 s and 99 s in run C).
- `tools/ci_probes.py:503` — needs a GPU, manual-only.

**Handoff context:**

- **Current behavior:** Three offscreen boots plus one integrated boot cover overlapping ground; the integrated one is the costliest and the least stable.
- **Expected direction:** Retire `unified_transfer` (726 s). Behaviour that would be lost: a single fixed-seed session moving one exact item instance in both directions between all three endpoint classes through real rendered UI, and its fresh-process identity re-check with a diffable `FINGERPRINT`, i.e. the cross-slice interaction coverage; the owner accepted the same kind of loss for `expedition_retrieval` (#2813). Separately merge `item_list_widget` and `transfer_context_menu` into one offscreen boot (saves at most the 99 s of the smaller). `transfer_order` stays: it is the only walking test of the unit job.
- **Scope and constraints:** The cross-class round trip needs an owner decision; `.quruntul/checks.py` and `tools/test_run_probes.py` follow registry changes.
- **Remaining uncertainty:** Whether the Hspec groups cover every one of `unified_transfer`'s 72 checks was not compared; `tools/unified_transfer_probe.py` stage `modeA` and `save` failures were not diagnosed (no retained failure output beyond the runner tail, see #2931).

### PRB-5. `repair_ai` is flaky and shadowed by an Hspec group; `construction` costs 3-4x its recorded time

Two AI-job probes where the cost or the instability does not match what only they guard.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `repair_ai` | manual | 680 FAIL | 310 / 600 FAIL | retire |
| `construction` | manual | 560 | 525 / 580 | keep, but make cheaper |
| `craft_bill` | manual | 485 | 477 / 499 FAIL | keep as is |
| `power_workshop` | manual | 90 | 112 FAIL / 110 | keep as is |
| `power` | manual | 133 | 133 / 131 | keep as is |
| `machine_shop` | manual | 55 | 56 / 56 | keep as is |
| `wire` | manual | 89 | 79 / 75 | keep as is |

**Evidence:**

- `repair_ai`: run A 310 s passed, run B 600 s FAIL, run C 680 s FAIL (`52/57 checks passed`, including `broken weapon (condition 0) repaired first`); `tools/ci_probes.py:233` records that it fails with "a DIFFERENT failing-check set each run (3/3 solo runs failed, no two alike, #724)".
- `test-headless/Test/Headless/Lua/UnitAiRepairGround.hs:484` and following (1,077 lines) grade repair target selection, claims (including dead-claimant self-heal), and taking, and for its phase 10 (ground target) `tools/repair_ai_probe.py:58-63` calls the Hspec describe "the primary gate ... since repair_ai is manual-only/flaky".
- `construction`: `tools/ci_probes.py:244-248` records "4 phases, ~123-168s solo; 14/14 checks passed all 3 solo runs (#724)"; runs A, B, C measured 525, 580, 560 s, so the growth predates the 2026-10-18 arena-generation commit. The script has 23 commits.

**Handoff context:**

- **Current behavior:** `repair_ai` spends up to 680 s and fails most runs; `construction` spends about 560 s where it was recorded at about 150 s.
- **Expected direction:** Retire `repair_ai` (680 s). Behaviour that would be lost: the live `repair_job` walking, fetching from the technomule and repairing under the real `unit_ai` tick (its phases 1-4 and the ordering timeline), which is also the AI-timing part that flakes. Make `construction` cheaper by locating which phase grew and restoring the recorded budget; no estimate is made. `craft_bill`, `power_workshop`, `power`, `machine_shop` and `wire` stay: they are 55-485 s, pass, and no overlap was established beyond the Hspec `Bills.hs` files for the bill backend.
- **Scope and constraints:** `repair` and `repair_item` (CI) stay; `tools/ci_probes.py`, `tools/README.md` and the census follow a retirement.
- **Remaining uncertainty:** Whether Hspec's stub-VM groups cover phases 1 and 3 (own-inventory and mule spare gear) was not checked; the `construction` phase times are unrecorded; `craft_bill` is flaky on CI per `tools/ci_probes.py:201` and failed in run B, with no phase breakdown.

### PRB-6. `bleeding_trail` spends its time in real-time dwells that Hspec already grades

The blood family is covered in-process by six Hspec files; the real-engine probes add the unit-thread wiring. `bleeding_trail` is by far the heaviest and grew from 288 s to 379 s.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `bleeding_trail` | manual | 379 | 289 / 287 | keep, but make cheaper |
| `blood_impact` | manual | 53 | 53 / 54 | keep as is |
| `blood_decal` | manual | 15 | 15 / 15 | keep as is |
| `blood_gpu_lifecycle` | manual | 37 | 214 FAIL / 38 | keep as is |
| `injury_log` | manual | 42 | 41 / 41 | keep as is |

**Evidence:**

- Run C 379 s, runs A and B 289 and 287 s (commit `6aac0895c`, 2026-10-17, "Extend the bleeding-trail and persistence probes with Nominal blood", #2837, which added the four `nominal_*` checks).
- `tools/bleeding_trail_probe.py:55-58` — the pool-dwell check is "timed against REAL elapsed seconds — `world.setTimeScale` advances only the world calendar ... so it cannot fast-forward a dwell".
- Hspec: `Blood/Trail.hs:89-196` (gating, conserved volume, timestep-partition invariance), `Unit/PhysiologyBloodModes/WoundFold.hs` (the probe's own text says the pool's "strictly ADDITIVE" claim is asserted in Hspec), `Blood/Impact.hs` mirrors `blood_impact` (wound kind and severity to style and volume), `Blood/Types.hs` and `Blood/Texture.hs` mirror `blood_decal`, `Blood/Teardown.hs` and `Blood/LuaApi.hs` cover the disposal logic of `blood_gpu_lifecycle`.
- `blood_decal` 15 s, `blood_impact` 53 s, `blood_gpu_lifecycle` 37 s (214 s FAIL in run A, a fixture error filed as #2804), `injury_log` 42 s.

**Handoff context:**

- **Current behavior:** The 17 declared checks run over several real-time dwells, repeating bounds and invariances that Hspec already proves.
- **Expected direction:** Make `bleeding_trail` cheaper by keeping only the checks that need the unit thread (a real wounded unit moving leaves a trail, clot and death stop it, an internal wound stays dry, a stationary unit pools) and dropping the bound, cap and timescale checks that Hspec grades. Behaviour that would be lost if the dropped checks stop running: the accumulator's agreement with real elapsed time under the live tick at several time scales. Saves at most 379 s; not estimated. The other four blood probes stay: they cost 15-53 s, and `blood_gpu_lifecycle` is the GPU half of the disposal contract that `Blood/Teardown.hs` grades only as queued work.
- **Scope and constraints:** The probe reports `probe-result/v1` checks; the lab and census key tests by check name, so removed checks need the census row reconciled (`python3 .quruntul/checks.py`).
- **Remaining uncertainty:** Which individual checks Hspec fully duplicates was matched by group name, not assertion.

### PRB-7. `tutorial_hud` failed three runs of three with no retained reason

`tutorial_hud` is the GPU-backed gate for the tutorial checklist HUD. It failed in runs A, B and C (299, 300, 269 s) and the runner tail names no failed check.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `tutorial` | manual | 190 | 216 / 217 | keep as is |
| `tutorial_hud` | manual | 269 FAIL | 299 FAIL / 300 FAIL | keep only if its failure is fixed; otherwise retire |

**Evidence:**

- The runner tail of each run ends with `tutorial_hud_probe.py ... [timeout 900s] FAIL (…s)` and `FAILED: FAIL tutorial_hud_probe.py`, with no failed check named.
- `tools/tutorial_hud_probe.py:4` — "The GPU-backed complement to the pure 'Tutorial HUD' hspec group": the headless group grades the model, geometry and input policies; this probe grades that the surface is drawn over a real world and that real mouse input behaves as those policies promise. Hspec: `UI/TutorialHud`, `TutorialProgress`, `TutorialEvaluation`, `CaptionFit`.
- `tutorial` (190 s) is the integration gate across four boots and two worldgens (`tools/tutorial_probe.py:112`) and passes.

**Handoff context:**

- **Current behavior:** A 270-300 s probe that has not been green for the three runs measured.
- **Expected direction:** Keep it only if its failure is diagnosed and fixed (once #2931 retains the failure output); if the reason cannot be found it carries no value and should be retired (269 s). Behaviour at stake: the HUD actually rendered over a world and real click/drag routing.
- **Scope and constraints:** Offscreen, GPU; subject to the manual probe-slot rules.
- **Remaining uncertainty:** The failing check is unknown, so whether this is a probe or a HUD defect is open.

## Cheaper by design

### PRB-8. The CI probe set: `cargo_capacity` loops to overfill a 200 kg store; `persistence_contract` varied 4.5x

The 14 CI-eligible probes are the only ones that cost pull-request time, about 887 s serial by the expected-seconds table. Two anomalies stand out.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `persistence_contract` | CI | 686 | 154 / 154 | keep as is (documented gate) |
| `cargo_capacity` | CI | 85 | 86 / 86 | keep, but make cheaper |
| `craft` | CI | 98 | 98 / 98 | keep as is |
| `cooking` | CI | 42 | 42 / 42 | keep as is |
| `consumable_effects` | CI | 55 | 56 / 56 | keep as is |
| `repair` | CI | 56 | 56 / 56 | keep as is |
| `repair_item` | CI | 60 | 62 / 60 | keep as is |
| `canteen_instance` | CI | 36 | 36 / 36 | keep as is |
| `content_registry` | CI | 44 | 47 / 45 | keep as is |
| `flora_condition` | CI | n/m | n/m | keep as is |
| `scenario_entrypoint` | CI | n/m | n/m | keep as is |
| `audio_null` | CI | 6 | 7 / 6 | keep as is |
| `debug_console_boot` | CI | 6 | 6 / 6 | keep as is |
| `preview_cli` | CI | 23 | 24 / 22 | keep as is |

**Evidence:**

- `tools/cargo_capacity_probe.py:91` — a 200-iteration loop (about 90 accepted deposits of a 2.2 kg canteen to reach the 200 kg capacity), each iteration two console round trips; 85-86 s in runs A, B and C for a 119-line probe. The store's capacity sets the iteration count; the probe also loads every units, buildings, items and equipment YAML through one console call per file (`bootstrap()`), so the loop and the bootstrap are the candidate drivers and their split is unmeasured. Hspec `Unit/CargoApi.hs:208` (cross-page refusals) has no instance-weight assertion that I found.
- `persistence_contract` ran 154 s in runs A and B and 686 s in run C; `PROBE_EXPECTED_SECONDS` says 206 s.
- `flora_condition` and `scenario_entrypoint` have no shakedown measurement (they landed after it); their expected seconds are 50 and 70.
- `craft`, `cooking`, `consumable_effects`, `repair`, `repair_item`, `canteen_instance` each boot an arena with the same defs and cost 36-98 s; their docstrings say they build on one another (`cooking` "on top of ... `craft_probe`") and #531 found that sharing an engine across such probes is not feasible.

**Handoff context:**

- **Current behavior:** `cargo_capacity` pays an overfill loop on every selected PR.
- **Expected direction:** Make `cargo_capacity` cheaper by giving it a small-capacity throwaway storage def (the technique `transfer_context_menu_probe.py` already uses) so the overfill needs a handful of deposits, and by measuring whether the YAML bootstrap matters; saves at most 85 s of CI and lab time. Keep `persistence_contract` as the documented gate but find out why one run took 4.5x as long, since it is the CI lane's longest probe. The rest of the set earns its slot: most carry a comment in `tools/ci_probes.py` naming what only they prove.
- **Scope and constraints:** CI-eligible set changes require `python3 tools/ci_probes.py --self-test` and `python3 .quruntul/checks.py`.
- **Remaining uncertainty:** Whether run C's 686 s was machine contention or a change in the probe; how much of `cargo_capacity`'s 85 s is the loop; whether a small def still exercises #189 (the bug is overfilling by base weight, so it should).

### PRB-9. Three pairs of companion probes pay two boots for one fixture

Three pairs share a fixture family, an issue, or a stated dependency, and each could be one probe with two sections.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `save_barrier` | manual | 31 | 37 / 34 | merge with `save_storage` |
| `save_storage` | manual | 53 | 65 / 61 | merge with `save_barrier` |
| `save_pause` | manual | 30 | 38 / 33 | merge with `pause_speed` |
| `pause_speed` | manual | 36 | 44 / 38 | merge with `save_pause` |
| `collapse_crawl` | manual | 95 | 95 / 86 | merge with `concussion_revive` |
| `concussion_revive` | manual | 59 | 59 / 59 | merge with `collapse_crawl` |

**Evidence:**

- `tools/save_storage_probe.py:7` — "on top of what tools/save_barrier_probe.py already proves" (#757/#758/#762); 31 s and 53 s.
- `save_pause` and `pause_speed` both guard pause semantics (#42, #1572, #1599); 30 s and 36 s.
- `tools/concussion_revive_probe.py:4` — "Companion to collapse_crawl_probe.py" (#304); 95 s and 59 s; both `TARGETED` in `tools/ci_probes.py:350-351`.

**Handoff context:**

- **Current behavior:** Six registered probes, six engine boots, six census/lab rows.
- **Expected direction:** Merge each pair into one probe, keeping every check: saves one boot and teardown per pair, at most 31 + 30 + 59 = 120 s, and three fewer scripts and registry rows to maintain. No behaviour is lost by a merge; the cost is that a failure in one half now fails the other's run.
- **Scope and constraints:** Probe keys are cited by docs, `tools/README.md`, `tools/ci_probes.py` and the census; renames follow `tools/test_run_probes.py` and `.quruntul/checks.py`.
- **Remaining uncertainty:** The real boot-and-teardown overhead per probe is not measured; the cheapest probes in run C (3-10 s) suggest it is well under the figures above. `circadian` and `circadian_species` were not paired: the latter keeps the real `go_to_sleep` AI live (its docstring), so the fixtures differ.

## Keep as is

### PRB-10. Save/load and fluid fresh-process probes: keep, with one cost to explain

These probes prove state across a process boundary, which Hspec structurally cannot.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `transactional_load` | manual | 158 | 533 / 525 | keep as is |
| `autosave` | manual | 215 | 212 / 216 | keep as is |
| `multiworld_save` | manual | 102 | 123 / 110 | keep as is |
| `persistence_integrity` | manual | 96 | 97 / 96 | keep as is |
| `lua_orphan_prune` | manual | 55 | 61 / 58 | keep as is |
| `gameplay_tag_persistence` | manual | 395 | n/m | keep as is |
| `fluid_exact_restart` | manual | 13 | 13 / 13 | keep as is |
| `fluid_reaction` | manual | 63 | 64 / 63 | keep as is |

**Evidence:**

- `tools/multiworld_save_probe.py` states it "can't be an hspec test: the round-trip spans the Lua thread ..."; `tools/persistence_integrity_probe.py:6` and `tools/fluid_reaction_probe.py` each name the Hspec group that proves the in-process half and the process-boundary half only they prove.
- `transactional_load` dropped from 533 s (runs A, B) to 158 s (run C), commit `9d7b08d78`.
- `autosave` 212-216 s is real-time by construction: `tools/ci_probes.py` records the validated 1-minute minimum for `interval_minutes` forces multiple 60 s waits.
- `gameplay_tag_persistence` costs 395 s in run C (new since runs A and B), `SLOW_WORLDGEN` per `tools/ci_probes.py:524`, with the Hspec group `Gameplay.Tags.Persistence` as its in-process counterpart.

**Handoff context:**

- **Current behavior:** 8 probes, 1,097 s in run C, all passing.
- **Expected direction:** Keep as is. No saving is proposed. The one open question is where `gameplay_tag_persistence`'s 395 s goes (a worldSize-16 generation and several fresh processes per its reason); record phase times before judging it.
- **Scope and constraints:** Save-affecting changes still follow `src/World/Save/CLAUDE.md`; `persistence_contract` remains the gate.
- **Remaining uncertainty:** `gameplay_tag_persistence` has a single measurement.

### PRB-11. Physiology, mind, combat and AI probes: keep, with three red in the latest run

Twenty-three probes covering Lua gameplay logic that has no in-process equivalent beyond the formula tests.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `physiology` | manual | 432 FAIL | 431 FAIL / 431 FAIL | keep as is (physiology changes are accepted against it); fix its threshold flake |
| `thermo_altitude` | manual | 79 | 110 / 92 | keep as is |
| `state_of_mind` | manual | 45 | 47 / 46 | keep as is |
| `circadian` | manual | 37 | 39 / 37 | keep as is |
| `circadian_species` | manual | 65 | 66 / 65 | keep as is |
| `mental_state` | manual | 183 | 181 / 176 FAIL | keep as is |
| `mental_efficiency` | manual | 97 | 104 / 104 | keep as is |
| `thought` | manual | 80 | 78 / 77 | keep as is |
| `sleep` | manual | 100 | 100 / 100 | keep as is |
| `meal_waste` | manual | 32 | 32 / 32 | keep as is |
| `item_temp` | manual | 64 | 70 / 68 | keep as is |
| `item_instance` | manual | 49 | 52 / 51 | keep as is |
| `combat_anim` | manual | 27 FAIL | 26 FAIL / 26 FAIL | keep as is (fix its failure) |
| `lunge` | manual | 60 | 66 / 66 | keep as is |
| `retaliation_swap` | manual | 161 | 125 / 98 | keep as is |
| `follow_command_priority` | manual | 71 FAIL | 87 FAIL / 64 | keep as is (fix its failure) |
| `position_hold` | manual | 159 | 158 / 145 | keep as is |
| `wander_hazard` | manual | 146 | 146 / 146 | keep as is |
| `movement` | manual | 60 | 60 / 60 | keep as is |
| `action_outcome` | manual | 50 | 54 / 52 | keep as is |
| `disarm` | manual | 30 | 30 / 30 | keep as is |
| `medic_coord` | manual | 35 | 37 / 35 | keep as is |
| `infection` | manual | 60 | 60 / 60 | keep as is |

**Evidence:**

- `physiology` failed in runs A, B and C (431-432 s) on `temperate (22C/0.5): circ min` at 0.70-0.73 against a 0.75 threshold (`tools/ci_probes.py:237-243`: RNG-rolled units; threshold sits at the limit); physiology changes are accepted against it (#2761's acceptance text requires a full run).
- `follow_command_priority` failed in runs A and C and `combat_anim` in all three (`ConnectionResetError` in run A, `texture handle batch 1: response contains a non-integer` in run C; #2792, the console-cap fix, is closed).
- `mental_efficiency` states Hspec `Combat.MentalEffectiveness` is "the deterministic authority" for the formulas and the probe proves the Haskell/Lua plumbing (docstring); `mental_state` is a 5,395-line script, 183 s.
- `role` and `chop` are census-deferred until biome tree assets land (`docs/probe_census.json` `deferred`).

**Handoff context:**

- **Current behavior:** 2,122 s in run C; `physiology`, `follow_command_priority` and `combat_anim` red.
- **Expected direction:** Keep as is. No saving proposed. Fix `physiology`'s threshold flake (a red gate that physiology changes are accepted against costs 432 s and gives no signal) and the two red combat probes; none of them is slower than its guard justifies.
- **Scope and constraints:** Run `tools/physiology_probe.py` after any physiology change; do not change its thresholds, stimuli or checks without the owner (per #2761's acceptance text).
- **Remaining uncertainty:** `mental_state`'s 5,395 lines are a maintenance cost (21 commits) but no runtime cost beyond 183 s; any split is a refactor, not part of this report.

### PRB-12. Location, rendering and GPU probes: keep, with two red in the latest run

Nineteen probes whose value is in pixels, windows, GPU resources or generated-world placement, which headless Hspec cannot see.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `location_content` | manual | 436 | 714 / 697 | keep as is |
| `location_overlay` | manual | 113 FAIL | 150 FAIL / 134 FAIL | keep as is (fix its failure) |
| `location_stamp_idempotent` | manual | 97 | 113 / 105 | keep as is |
| `location_embark` | manual | 199 | 35 FAIL / 216 FAIL | keep as is |
| `portal_ghost` | manual | 33 | 36 / 33 | keep as is |
| `portal_location` | manual | 27 | 31 / 27 | keep as is |
| `remote_warning_page_guard` | manual | 17 | 17 / 17 | keep as is |
| `river_naming` | manual | 13 | 6 / 6 | keep as is |
| `etymology` | manual | 40 | 43 FAIL / 40 FAIL | keep as is |
| `preview` | manual | 376 | 381 / 381 | keep as is |
| `offscreen` | manual | 122 | 210 FAIL / 200 FAIL | keep as is |
| `fluid_reaction_visual` | manual | 313 | 314 / 314 | keep as is |
| `fluid_exact_restart_render` | manual | 48 | 48 / 48 | keep as is |
| `scene_stats` | manual | 19 | 25 / 22 | keep as is |
| `scene_primitives` | manual | 18 | 20 / 19 | keep as is |
| `structure_rotation` | manual | 22 | 24 / 22 | keep as is |
| `structure_construction` | manual | 42 | 43 / 42 | keep as is |
| `construction_blueprint_footprint` | manual | 46 FAIL | 62 FAIL / 53 FAIL | keep as is (fix its failure) |
| `audio_manual` | manual | n/m | n/m | keep as is |

**Evidence:**

- `tools/ci_probes.py` NEEDS_GPU reasons for the offscreen ones; CLAUDE.md "Headless success does not prove visual correctness".
- Red in run C: `location_overlay` (`engine.loadSave ... was not accepted` in phase 2, 113 s), `construction_blueprint_footprint` (`the 1x1 control's ghost is visibly a different, smaller shape`, 46 s, red in all three runs). `offscreen` failed in runs A and B on a load race (`debug_rebind_b init completes`) and passed in run C (122 s); `location_embark` failed in runs A and B and passed in run C; `etymology` failed in runs A and B and passed in run C.
- `preview` 376 s (24 commits) is the real-GLFW browser probe; `fluid_reaction_visual` 313 s is the probe that checks a solidification appears in both the tile render and the zoom map.
- `location_content` costs 436 s in run C and 697-714 s in runs A and B; 38 commits, the most-touched probe script, thirteen scenarios in one file.

**Handoff context:**

- **Current behavior:** 1,981 s in run C; two red, three others recovered.
- **Expected direction:** Keep as is; fix the two red probes. Scheduling these is #2932's concern, not repeated here. `location_content` is a candidate for splitting by owner phase if its 38 commits keep costing review rounds, but that is a refactor with no runtime saving.
- **Scope and constraints:** Sprite and render signoff rules in CLAUDE.md still apply.
- **Remaining uncertainty:** Which of `location_content`'s phases accounts for its 436-714 s spread.

### PRB-13. Boot, config and encoding contract probes are cheap and earn their keep

Five probes of 3-12 s each guard contracts only a real boot can show.

| Probe | Gate | Run C (s) | Runs A / B (s) | Proposal |
|---|---|---|---|---|
| `config_state` | manual | 7 | 8 / 8 | keep as is |
| `config_migration` | manual | 10 | 10 / 10 | keep as is |
| `resource_root` | manual | 12 | 16 / 14 | keep as is |
| `startup_asset_logging` | manual | 9 | 10 / 9 | keep as is |
| `text_encoding` | manual | 3 | 3 / 3 | keep as is |

**Evidence:**

- Run C: 7, 10, 12, 9 and 3 s. `config_state` step 1 checks `git status` of the real `config/` directory is clean; `resource_root` proves the executable runs from a directory outside the repo; `config_migration` boots the legacy-config upgrade path (`Engine.Core.Init.migrateLegacyConfig`).
- Hspec `ConfigState` (12 references) covers the in-process side.
- `startup_asset_logging` guards #1930's ownership split of per-file DEBUG and one INFO aggregate per registry family across the real loaders.

**Handoff context:**

- **Current behavior:** 41 s in total.
- **Expected direction:** Keep as is. Replacing them with Hspec would save under 41 s and lose the real-process checks above.
- **Scope and constraints:** Tests that write `config/` must use `withIsolatedResourceRoot` (CLAUDE.md).
- **Remaining uncertainty:** None at draft time.

## Measurement

### PRB-14. The cost and value data needed to rank probes is not tracked

Ranking probes by what they cost and what they catch needed three sources that are not in the repository, and the value side is not recorded anywhere.

**Evidence:**

- `docs/probe_census.json` has a `current` timing sample for 11 of 103 probes, the newest dated 2026-09-02; the other 92 rows have empty `attempts`, `history` and `outcomes`.
- Full per-probe timing exists only in the lab ledger runs under `<git-common-dir>/quruntul/runs/` (three shakedown runs, the latest 2026-10-19), which are local and untracked.
- No probe reports phase times: each reports a total and its checks, so for most of the slowest probes in run C (`transactional_load`, `gameplay_tag_persistence`, `location_content`, `physiology`, `construction`, `craft_bill`, `preview`, `fluid_reaction_visual`, `transfer_order`, `item_list_widget`) the cost driver cannot be identified without running them.
- "Last caught a real regression" is not recorded. A search of the 7,353 commit messages on `master` for "caught/found/detected/exposed/surfaced ... by ... probe" finds one hit, and it concerns a defect in the probe itself (`5b388fe8f`, #1260). The follow-up issues of the 2026-10-04 shakedown that cite run `82223792` (#2791, #2792, #2793, #2794, #2800, #2804, #2654, #2802, #2813) by title all repair probes or the probe tooling; of the roughly 150 issues with "probe" in the title that a GitHub search returned, the titles I read are fixes to probes, the runner or the census.

**Handoff context:**

- **Current behavior:** Cost can be read only by someone with the local lab ledger; value is inferred from docstrings.
- **Expected direction:** Record per-probe wall time from a recent run in a tracked place (the census is the natural owner), emit per-phase times in `probe-result/v1` so cost drivers are visible, and give "found a defect" a convention (for example a line in the fixing PR body or issue naming the probe) so the next review of this kind has a value column. Without the third, every future audit reaches the same "no evidence either way".
- **Scope and constraints:** `docs/probe_census.json` is PR-only (listed in `~/.config/docs-push.json`); `tools/test_probe_census.py` and `python3 .quruntul/checks.py` apply. Not a duplicate of #2931 (failure evidence) or #2932 (GPU lock).
- **Remaining uncertainty:** The issue-title sample is a GitHub relevance-ranked search limited to 150, so it understates catches filed under other titles.
