# Epic Review Findings: Epic #479 — Circadian rhythm (sleep pressure, exhaustion, dusk-driven sleep AI)

This report records the current-HEAD review of epic #479 at
`650578ad6de470a8d97e5808987e75441d07e834`. The epic declares four
implementation children, #610 through #613; all four are closed as completed,
each implementation pull request merged after approval, and the owner's closure
comment confirms that the full build order shipped. The resulting architecture
remains coherent: exhaustion, multi-day sleep pressure, and live circadian urge
remain distinct signals; the sleep utility composes all three; flat dry ground,
a real `Sleeping` pose, sleep-only pressure recovery, and the existing healing
multiplier close the action loop; and the brown bear proves a non-default
species phase. Three later repairs also closed the current defects previously
found in this arc: #1939 seeds the initial wake-boundary sample, #1945 derives
automatic wake time from the species phase, and #1948 applies exhaustion to the
ambient meander cap. Two new documentation mistakes survive: the epic body
still shows an unfinished build order and the wrong save-version outcome, while
two source-module headers still describe the completed #611/#612 integration as
future work.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. Epic #479 leaves all four completed children unchecked and records the wrong save-version outcome — [#479]
- [x] ER-2. The exhaustion and circadian headers still describe the completed integration as future work — [#2234]

## 1. Epic completion and save-history steering

### [#479] ER-1. Epic #479 leaves all four completed children unchecked and records the wrong save-version outcome

> **Captured note:** Epic #479 is closed and its owner closure comment confirms
> that #610 through #613 all shipped, but every build-order box remains unchecked
> and the body says appending `Sleeping` needs no save-version bump even though
> child #612 deliberately shipped it as save v81.

**Verification:** The contradiction is confined to the live tracker body. All
four children are closed, all four implementation pull requests merged, and the
closure comment explicitly enumerates the complete build order. The current
implementation and historical save record agree on `Sleeping` as the trailing
`Pose` constructor introduced in v81. The current focused behavior probes and
module gates pass, so the stale checklist and version statement are not evidence
of a surviving implementation or compatibility defect.

**Evidence:**

- [Epic #479's live body](https://github.com/coghex/synarchy/issues/479) —
  presents #610, #611, #612, and #613 as unchecked and says the append-only
  `Sleeping` constructor needs no save-version bump.
- [The owner's closure comment](https://github.com/coghex/synarchy/issues/479#issuecomment-4922863800)
  — says the full four-child build order shipped and closed, while retaining a
  real bed structure as an explicit later concern rather than unfinished epic
  scope.
- [Child #612](https://github.com/coghex/synarchy/issues/612) — repeats the
  pre-implementation no-bump expectation, while its merged
  [PR #640](https://github.com/coghex/synarchy/pull/640) records the accepted
  implementation outcome as “Engine (`Pose`, save v81).”
- `docs/history/savedata_version_changelog.md:176` — preserves v81 as the
  historical generation that appended `Sleeping` for #612.
- `src/Unit/Sim/Types.hs:183` — the live positional `Serialize` enum retains
  `Sleeping` as its trailing constructor.
- Current focused checks: `circadian_probe.py`,
  `circadian_species_probe.py`, `sleep_probe.py`, and `physiology_probe.py` all
  pass on a freshly checked current executable; `tools/ci_probes.py --self-test`,
  `tools/lua_module_budget.py`, and `tools/test_encumbrance_speed.lua` also pass.

**Handoff context:**

- **Current behavior:** A reader opening the closed epic sees four unfinished
  children and a specific save-policy claim that disagrees with the issue state,
  closure comment, merged implementation, and durable v81 history.
- **Expected behavior:** Mark #610 through #613 complete and replace the
  no-bump claim with the historical result: `Sleeping` was appended and #612
  shipped it with save v81. Prefer linking the version changelog over restating
  a mutable compatibility rule in the tracker.
- **Scope and constraints:** Tracker-body-only correction to epic #479. Do not
  change the current persistence codec, enum order, compatibility fixtures,
  child issues, or implementation merely to make the stale prose true. Preserve
  the bed/furniture deferral and the other explicit out-of-scope decisions.
- **Verification target:** The live epic marks all four declared children
  complete and describes the `Sleeping` save outcome consistently with PR #640
  and the v81 changelog entry.
- **Deduplication:** A 3,000-item all-state tracker inventory plus targeted
  searches for #479's unchecked build order, `Sleeping` save bump, and v81
  outcome found no corrective owner beyond the closed epic and its children.
  The docs-worktree report corpus has no pending owner for this tracker-body
  correction.
- **Remaining uncertainty:** None about the two contradictions. The only
  editorial choice is whether to keep the old no-bump expectation as explicitly
  superseded design history or replace it with the landed outcome.

## 2. Intermediate-phase source comments

### [#2234] ER-2. The exhaustion and circadian headers still describe the completed integration as future work

> **Captured note:** `exhaustion.lua` still calls sleep pressure a future meter
> and assigns feeding exhaustion into sleep AI to future #612 work, while
> `circadian.lua` still says nothing consumes sleep pressure or circadian urge.
> The live `unit_ai_sleep.lua` has consumed all three signals since #612.

**Verification:** Both comments describe the intermediate state after #610 or
#611, not the completed epic. The live sleep utility reads sleep pressure,
computes the live circadian urge, inverts exhaustion restedness, and adds all
three weighted terms. The end-to-end sleep probe passes that utility
composition and the later pose/recovery action loop. This is source guidance
drift, not a runtime omission.

**Evidence:**

- `scripts/exhaustion.lua:3` — calls #611's landed sleep-pressure resource a
  “future” meter.
- `scripts/exhaustion.lua:15` — says feeding exhaustion into sleep AI “is
  #612's job, once sleep pressure + circadian urge (#611) exist,” although both
  children are closed and their integration is live.
- `scripts/circadian.lua:3` — accurately describes the live signal through line
  8, then lines 9–10 incorrectly say nothing consumes either signal and assign
  their combination to future #612 work.
- `scripts/unit_ai_sleep.lua:30` — imports both `circadian` and `exhaustion`;
  lines 187–205 read sleep pressure and combine its deficit with circadian urge
  and exhaustion deficit in the current utility.
- `scripts/unit_ai_sleep.lua:239` — holds the real `Sleeping` phase while
  `unit_resource_tick` applies sleep-only pressure recovery, completing the
  integration the stale headers still predict.
- Current focused checks: the four arc probes pass, including the sleep probe's
  rested-versus-exhausted utility comparison and the circadian probes' raw and
  per-species curves; the Lua module-budget gate passes with
  `unit_ai_sleep.lua` at 308/500 lines.

**Handoff context:**

- **Current behavior:** Maintainers reading the two small physiology modules are
  told that #611 and #612 have not landed and that their exported values have no
  consumer, while the consumer sits in the current sleep module.
- **Expected behavior:** Rewrite the two headers in present tense: exhaustion is
  separate from the landed multi-day sleep-pressure resource and has both a
  passive movement effect and a weighted sleep-utility consumer; circadian urge
  is consumed with pressure and exhaustion by `unit_ai_sleep.lua`.
- **Scope and constraints:** Comment-only source correction in
  `scripts/exhaustion.lua` and `scripts/circadian.lua`. Preserve the three-signal
  model, resource rates, movement multiplier, utility weights, wake behavior,
  and module ownership.
- **Verification target:** Neither header describes #611/#612 as future or says
  the live signals are unconsumed; the comments point to the current owner and
  `tools/lua_module_budget.py` remains green.
- **Deduplication:** Exact all-state tracker searches for the quoted future-work
  sentences and a docs-worktree corpus search found no owner for this
  intermediate-phase drift. `docs/explore_report.md` EXPL-43 is adjacent but
  distinct: it owns the incorrect `unit.getCircadianUrge` namespace comment at
  `scripts/circadian.lua:63`, not these header claims or `exhaustion.lua`.
- **Remaining uncertainty:** None. The comments are contradicted by direct live
  imports, calls, and passing behavior coverage.
