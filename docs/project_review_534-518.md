# Project Review Findings: PRs #534–#518

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #534, #533, #528, #532, #525, #524, #523, #522, #521, #520, #519, and #518 — for later one-at-a-time disposition. The same first-parent window also contains direct commits `73f5a546` (`notify timeout`), `418a58d8` (`updating markdown files`), and `4c1d800c` (`mac os cabal hook`).

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The glacier-pair cleanup, thought and unified state-of-mind systems, data-driven locomotion injury flags, dead `scsGenFluid` removal, crop content, and plant-designation UI remain consistent with their linked issues in focused current verification. The `state_of_mind`, `thought`, and `crop` probes passed; `consumable_effects` and `plant` passed when rerun sequentially after the parallel-run defect recorded below; the focused `injurySpeedMult` and `harvestOpen` hspec groups passed 7 and 6 examples respectively. #532's original CI cache has since been substantially reworked by #671, #784, and #790, and its current content-addressed image plus bounded cache design did not yield a retained concern. #534's remaining local `jget` wrappers are already tracked by open issue #1160. The three direct commits did not yield another finding: they adjust notification failure handling and documentation, add the still-valid `AGENTS.md` symlink, and install the still-current macOS Cabal quarantine hook. Four current concerns remain.

## Status

- [x] PRR-1. Parallel probes race while mutating the shared Cabal build directory — [#1570]
- [x] PRR-2. `make ci` no longer runs every locally reproducible CI gate it claims to mirror — [no-issue]
- [x] PRR-3. Coffee's consumable effects have no production gameplay caller — [#1580]
- [x] PRR-4. Auto-harvest ignores the farming skill it claims to scale by — [#1582]

## 1. Parallel probe build isolation

### [#1570] PRR-1. Parallel probes race while mutating the shared Cabal build directory

> **Captured note:** Stop the aggregate probe runner from launching multiple `cabal run` processes against the same `dist-newstyle` at once. Parallel probes need execution isolation as well as distinct engine ports: the runner can build or resolve one executable before fan-out, or each Cabal invocation must have an isolated build directory, but concurrent tasks must not mutate the same inplace package database.

**Verification:** Verified by source tracing and a current six-probe reproduction. PR #534 centralized engine launch in `probelib.boot`, which still implements every launch as `cabal run`. The later parallel runner starts multiple probe scripts concurrently, and each script independently reaches that shared boot path. With three jobs, three probes failed in approximately one to four seconds with mutually incompatible mutations of `dist-newstyle`'s inplace package state: missing paths during `removeDirectoryRecursive` and an already-existing `package.conf.inplace`. The other three continued and passed. Rerunning two of the immediate failures sequentially made both pass; the third entered its normal worldgen-heavy execution and reached the aggregate's six-minute timeout rather than reproducing the Cabal startup error. This is a runner-induced build race, not a common feature regression in the selected probes.

**Evidence:**

- Issue #529 / PR #534 extracted the shared boot/send/teardown harness while preserving independently runnable probes. `tools/probelib.py:120-141` currently opens the engine log and starts `cabal run -v0 exe:synarchy -- ...` for every `boot` call.
- `tools/run_probes.py:308-339` starts each selected probe as a separate Python process. Its parallel branch at `:423-444` submits multiple scripts to a thread pool and asserts there is “No isolation issue” because they have separate processes, ports, and save names, but it does not account for their shared Cabal build state.
- `python3 tools/run_probes.py --only consumable_effects,state_of_mind,thought,plant,crop,farm_ai --exact --jobs 3 --retries 0 --timeout 360 --tail 80` produced immediate failures for `consumable_effects`, `plant`, and `farm_ai`. Their engine output respectively reported `package.cache: removeDirectoryRecursive:fstatat: does not exist`, `ghc-pkg-9.12.2: cannot create: .../package.conf.inplace already exists`, and `package.conf.inplace: removeDirectoryRecursive:unlinkat: does not exist`.
- In that same invocation, `state_of_mind`, `crop`, and `thought` passed in 42.8, 53.4, and 72.6 seconds. The failures therefore were not a generally broken engine build or a runner-wide inability to boot.
- `python3 tools/run_probes.py --only consumable_effects,plant,farm_ai --exact --jobs 1 --retries 0 --timeout 360 --tail 80` then passed `consumable_effects` in 49.9 seconds and `plant` in 48.4 seconds. `farm_ai` timed out only after 360 seconds in its running engine; `tools/ci_probes.py --status` already classifies it as the slowest registered probe, with an observed runtime around eleven minutes.
- CI invokes the parallel branch with `--jobs 2 --retries 1`. Its solo retry can conceal this infrastructure failure when the feature probe subsequently passes, but only after spending a failed attempt and presenting the first run as a probe failure.
- The preceding `project_review_609-535.md` report records a different parallel-runner problem: adjacent base ports can overlap a probe's secondary port. This finding remains distinct even when every probe uses exactly one non-overlapping port; the collision is in the shared Cabal package/build directory and needs a different remedy.
- Targeted all-state tracker searches for parallel probes racing `dist-newstyle`, `package.conf.inplace`, and concurrent Cabal probe builds found no existing issue. Findings-report searches found the port-allocation concern above but no shared-build-state concern.

**Handoff context:**

- **Current behavior:** `--jobs N` runs up to N probe scripts at once, and each can invoke Cabal against the primary checkout's same `dist-newstyle`. Cabal's inplace package registration/removal is not isolated across those launches, so an otherwise healthy probe can die before the engine starts. The configured solo retry often turns the defect into latency and noise instead of a final red result.
- **Expected behavior:** Parallel probe execution uses an already-built executable or otherwise guarantees that concurrent launches cannot mutate common Cabal state. A failed probe then reflects its own engine or assertions rather than another probe rebuilding/registering the same package.
- **Scope and constraints:** Surfaced from #534's shared boot path interacting with #536's parallel aggregate runner. Preserve direct one-probe invocation from the repository root, engine restart within a probe, resource-root behavior, distinct logs/ports/save names, process-group teardown, and the runner's existing result/retry accounting. Do not solve this merely by adding retries; deterministic isolation is the contract.
- **Remaining uncertainty:** The ownership boundary is a design choice. The aggregate runner could prebuild and pass a resolved executable to the harness, the harness could prefer an already-resolved binary in runner mode, or concurrent launches could use separate Cabal build directories. The reproduction establishes the shared-state defect, not which interface is least disruptive.

## 2. Local CI gate drift

### [no-issue] PRR-2. `make ci` no longer runs every locally reproducible CI gate it claims to mirror

> **Disposition:** No issue — fixed by #1355 (closed 2026-08-20). `tools/ci-local.sh:220-221` now runs both `ci_probes.py --self-test` and `ci_expensive_gates.py --self-test`; `tools/ci_parity_audit.py` compares the two files' `python3 tools/*.py` invocations in both directions on both sides (42 identical, 7 reason-carrying exemptions on a current run) so the set cannot silently drift again; and the claim is narrowed to `ci.yml`'s `test-and-audits` worker in both the script and `CLAUDE.md`. Behavior probes now have the explicit answer the finding asked for: a separate PR-only `behavior-probes` job that `make ci` deliberately excludes, with that three-job wiring and its two required commands structurally pinned by the same audit.

> **Captured note:** Reconcile `make ci` with the current CI workflow, or narrow the promise made by the script and repository instructions. At minimum, the cheap probe-policy and expensive-gate self-tests that CI runs unconditionally should not be absent from a command documented as the exact same gate set; the intended local contract for CI's path-selected behavior probes also needs an explicit answer.

**Verification:** Verified as current command-set drift, with the final policy choice still open. Issue #527 deliberately accepted that two independently maintained definitions could drift, but required `make ci` to mirror CI and documented green locally as predicting green in CI. `tools/ci-local.sh` still repeats that guarantee and runs a fixed sixteen-stage sequence. The current workflow has since added two unconditional policy self-tests and a blocking PR-only behavior-probe stage, none of which the local script invokes. A broken gate mapping, expensive-gate selector, or selected feature probe can therefore fail CI after `make ci` has returned green. The local script also always builds the graphical suite and runs the quick world check while CI path-selects those expensive steps, so neither direction is an exact command mirror now.

**Evidence:**

- Issue #527 / PR #533 introduced `make ci` specifically so one command ran the same gate set in the same order and “green locally ⇒ green in CI.” Its out-of-scope section acknowledged the definitions were not wired to a shared source and explicitly accepted future drift risk.
- `tools/ci-local.sh:1-5` still says it executes the same checks as `.github/workflows/ci.yml`, in the same order, and that green locally predicts green CI. `:57-113` enumerates sixteen stages ending at `world_check.py --quick`; it never calls `ci_probes.py`, `ci_expensive_gates.py`, or `run_probes.py`.
- `.github/workflows/ci.yml:425-431` unconditionally runs `python3 tools/ci_probes.py --self-test` and `python3 tools/ci_expensive_gates.py --self-test` on every workflow run. The local gate omits both millisecond-scale checks.
- `.github/workflows/ci.yml:433-461` also selects behavior probes from a pull request's changed paths and executes them with `run_probes.py --exact --retries 1 --jobs 2`. This is a blocking feature gate, not an optional diagnostic in CI, while `make ci` executes no behavior probe.
- Conversely, the workflow guards the graphical build and quick world check with path-derived expensive-gate outputs, whereas `ci-local.sh` always runs both. Running extra expensive checks locally does not repair the missing CI checks or make the advertised implication sound.
- The current repository instructions repeat that `make ci` runs the exact CI checks and that CI's blocking path-selective probe map lives in `tools/ci_probes.py`. They separately caution that `make ci` is an explicit full-gate operation rather than an iteration loop, so this finding does not propose making ordinary agent iteration more expensive.
- `python3 tools/ci_probes.py --self-test` and `python3 tools/ci_expensive_gates.py --self-test` both passed during this review. That establishes the present policies are internally valid; it does not make them part of the local gate.
- Targeted all-state tracker searches for `make ci` drift, omitted probe-policy self-tests, and missing behavior probes found closed #527 plus unrelated issues, but no open reconciliation. Findings-report searches found no existing concern for this current mismatch.

**Handoff context:**

- **Current behavior:** A user can run the documented full local pre-push gate successfully while CI later rejects the same revision because a selector self-test or path-selected behavior probe fails. The local run can also do unnecessary graphical/worldgen work for paths CI would classify as cheap.
- **Expected behavior:** The implementation and documentation agree on one honest contract. If `make ci` is meant to predict all locally reproducible build-test CI gates, it includes the unconditional selector checks and has a deliberate way to select relevant behavior probes. If it is only the heavyweight build/hspec/audit/world-check subset, the “exact” and green-implies-green claims are narrowed accordingly.
- **Scope and constraints:** Surfaced from #533 / issue #527 after later CI evolution. Do not attempt to reproduce GitHub-only image publication, permissions, or event plumbing locally. Preserve the rule that agents run `make ci` only on explicit request, the prod profile, visible failure output, and restoration of `cabal.project.local`. A shared manifest or an audit comparing the two definitions could reduce repeat drift, but is not required by this finding.
- **Remaining uncertainty:** PR-only probe selection needs a product decision because a local checkout may not have a canonical pull-request base. An explicit base argument, merge-base convention, full CI-eligible smoke set, or a documented exclusion could each be coherent. There is no comparable ambiguity about the two unconditional pure-Python self-tests: they are locally reproducible and currently omitted.

## 3. Consumable gameplay integration

### [#1580] PRR-3. Coffee's consumable effects have no production gameplay caller

> **Captured note:** Connect `scripts.consumable.drink` to an actual gameplay action for a held coffee instance. The quality-, temperature-, hydration-, caffeine-, and mood-effect mechanism currently works only when a probe or debug-console caller invokes it directly, so the cooking epic's final payoff cannot be reached through normal play.

**Verification:** Verified as a current integration gap; whether issue #347 intentionally accepted mechanism-only delivery despite its goal and done condition is the remaining specification judgement. The effect function is implemented and its focused probe passes, but a repository-wide production-script search finds no import or call. The only call site is the probe, which directly requires the module through the debug console. The live need-driven drink action is a separate canteen-water implementation and does not inspect coffee or delegate to the consumable mechanism. Thus players and autonomous units can brew and carry coffee, but no production input or AI path applies the shipped coffee effects.

**Evidence:**

- Issue #347's goal calls drinking the payoff of the cooking/consumables epic, and its done condition requires drinking coffees of varying quality and temperature to produce different hydration, caffeine, mood, and warmth outcomes. It does not limit completion to a debug API.
- `scripts/consumable.lua:1-9` describes itself as “Mechanism-only,” names direct debug-console/tests/future-AI use, and explicitly leaves autonomous consumption to a follow-up. `:71-127` correctly computes the effects and drains the selected instance when called.
- A current recursive search for `consumable.drink` or a production `require` of `scripts.consumable` finds only `tools/consumable_effects_probe.py:142-144`; no script registered in the game imports the module.
- `scripts/unit_ai_needs.lua:62-130` implements `drink_from_canteen` around the configured water canteen. It finds only that defName, computes plain hydration itself, drains it, and triggers the animation; it neither recognizes `coffee_pot` nor calls `consumable.drink`.
- `tools/consumable_effects_probe.py:142-144` invokes `require('scripts.consumable').drink(...)` directly over the debug socket. Its detailed effect assertions therefore prove the mechanism but cannot detect the absence of a player or AI entry point.
- The focused `consumable_effects` probe passed sequentially in 49.9 seconds during this review, including hot/cold and quality-scaled outcomes. The initial parallel failure was the Cabal build race recorded as PRR-1, not an effect failure.
- The linked cooking epic #342 is closed and identifies #347 as the final consumable-effects payoff. Targeted all-state searches for a coffee drink action, consumable caller, or missing AI/UI integration found no open follow-up; findings-report searches found a separate temperature-stacking concern but no reachability concern.

**Handoff context:**

- **Current behavior:** Coffee can be brewed with instance quality and temperature, and the dormant function calculates the desired effects. Normal gameplay exposes no action that calls it; only a developer probe/debug expression can make a unit consume coffee.
- **Expected behavior:** At least one supported gameplay route selects a real held coffee instance and invokes the consumable effect path, making the epic's outcome observable without developer tooling. The route and its tests cover reachability in addition to the mechanism's arithmetic.
- **Scope and constraints:** Surfaced from #528 / issue #347. Preserve exact-instance fill mutation, the existing quality/temperature curves, canteen-water survival behavior, and the current effect probe. The first route could be a player item action, an explicit unit order, or an autonomous utility-AI action; do not silently make stimulant consumption mandatory merely to create a caller.
- **Remaining uncertainty:** The PR's own text narrowed delivery to mechanism-only even though the linked issue and epic read as gameplay completion. The processor should decide whether that was an accepted scope change or an unfinished integration. If autonomous AI is chosen, its thirst/caffeine/mood priority is genuine design work; a manual consume action may be the smaller first slice.

## 4. Farming-skill effect on harvest

### [#1582] PRR-4. Auto-harvest ignores the farming skill it claims to scale by

> **Captured note:** Make farming skill observably gate or scale automatic harvesting, and add a comparative regression that distinguishes low- and high-skill workers. The current action is instant for every eligible unit; farming affects later role derivation and gains XP after success, but it does not change harvest eligibility, duration, or yield.

**Verification:** Verified against the linked issue, current data contract, AI implementation, and probe. Issue #336 calls the behavior skill-gated and requires harvest skill to scale speed or yield like mining. The acolyte definition promises the same `0.5 + farming/100` rate factor used for planting. In the live action, utility is distance times a categorical role multiplier; execution calls `world.harvestFlora` immediately upon adjacency. No farming-skill read occurs anywhere in the harvest utility or execute path. The only farming operation is XP granted after a successful harvest. The end-to-end probe checks that one acolyte eventually harvests and that its skill increases, so it passes the loop without testing the promised skill dependency.

**Evidence:**

- Issue #336 is titled “skill-gated auto-harvest.” Its scope says a unit with farming/harvest skill automatically harvests ripe plants and that harvest skill scales speed/yield like mining.
- `data/units/acolyte.yaml:79-83` documents farming as scaling planting and auto-harvest rate by `0.5 + farming/100`, with XP gained per completed action. That live data commentary still presents rate scaling as a current contract.
- `scripts/unit_ai_farm.lua:433-448` finds the nearest harvestable flora and returns `harvest_base_utility * distFactor * roles.weight(s, "auto_harvest")`. It never calls `unit.getSkill` and imposes no minimum farming level.
- `scripts/unit_ai_farm.lua:450-487` walks to the target and calls `world.harvestFlora` immediately when adjacent. There is no work-progress accumulator, rate calculation, or skill-dependent yield; `grantWorkXP(..., "farming", ...)` occurs only after yields already exist.
- `scripts/unit_roles.lua:138-155` makes `roles.weight` a categorical role-family entry-utility multiplier. Roleless units and laborers receive `1.0`, matching specialists receive the on-role bonus, and other active specialists receive an off-role damp. That prioritizes Farmers but does not gate harvesting or continuously scale it by farming skill.
- `tools/farm_ai_probe.py:434-501` fast-forwards one wheat plot, lets one acolyte harvest it, asserts grain appeared, and then asserts farming XP increased. It never sets or compares harvest workers at different farming levels, measures duration/throughput, or compares yield.
- The focused `harvestOpen` hspec group passed all 6 examples during this review, confirming harvestability windows and yield eligibility rather than AI skill scaling. The registered `farm_ai` probe is explicitly manual-only and worldgen-heavy; its current registry note reports roughly eleven minutes because of its broad scenario and expensive scans, and this review's six-minute focused attempt timed out after successful engine/world startup rather than reporting an assertion failure.
- Targeted all-state searches for farming-skill harvest rate, skill-gated auto-harvest, and instant harvest found only the closed #336 specification and no open correction. Findings-report searches found no existing concern for the missing skill effect.

**Handoff context:**

- **Current behavior:** Any unit whose action registry includes auto-harvest can perform the same instant harvest regardless of farming level. Role can alter how often it wins arbitration, and completed work teaches farming, but skill itself has no causal effect on the action it is said to govern.
- **Expected behavior:** Farming level has a specified, observable effect on auto-harvest eligibility or throughput — progress duration, yield, or another explicitly chosen axis — and tests would fail if the skill read were removed. The data comments, issue contract, and behavior agree on that axis.
- **Scope and constraints:** Surfaced from #521 / issue #336. Preserve crop/wild-flora discovery, role weighting, collection of exact ground yields, race recovery, animation, and farming XP. Avoid using role membership as a proxy for the skill contract: role is derived/coarse arbitration state, while the issue and YAML promise direct skill scaling.
- **Remaining uncertainty:** The original issue left some harvesting details open and says “speed / yield”; the YAML resolves that toward rate, while the implementation comment calls picking intentionally instant. Maintainers may choose a short progress accumulator, a yield modifier, or an explicit minimum-skill gate, but should update the other contracts if they deliberately reject rate scaling. The missing dependency itself is not uncertain.
