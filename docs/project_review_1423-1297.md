# Project Review Findings: PRs #1423–#1297

This entry records the senior review of merged PRs #1423 through #1297 — 58
PRs across five batches, merged 2026-08-13 through 2026-08-19 — and moves the
review cursor: `project_review_1296.md` (PRs #1296–#1271) was the previous
newest entry, ranges below it were skipped as deliberately clear, and
everything from #1297 up through #1423 is covered here. **PR #1411 (merged
2026-08-19T18:04Z, after #1423) is included in this sweep despite its lower
number** — a future sweep resuming from the merge-date position of #1423
must not re-review it. Direct first-parent master commits in the window were
docs-lane report dispositions plus one reviewed direct test-file push
(`d1d5f33e`, removing a `test_findings_report_audit.py` assertion that
measured processing-lane backlog rather than lexer behavior — a justified
un-wedge of a master-wide red, verified correct).

The sweep produced one finding, filed as #1444 during the review with the
owner's approval. One fixed-later observation and the cleared ranges are
recorded below. No other concern survived verification.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] PRR-1. EXCLUSIVE_RESOURCES misses the engine-boot config writers — [#1444]

## 1. Probe scheduling

### [#1444] PRR-1. EXCLUSIVE_RESOURCES misses the engine-boot config writers

PR #1422 (issue #1322) serializes `config_migration` against `config_state`,
but every engine-booting probe writes `config/` when the local files are
absent — `Engine.Asset.YamlNotifications.loadOverrides` materializes
`config/notifications.local.yaml` on every boot that finds it missing, and
`migrateLegacyConfig` copies legacy over absent local files — and absent-local
is exactly the fixture state both config probes hold for most of their
runtime. Concrete flip verified in `tools/config_migration_probe.py` Phase 1:
a foreign engine materializing registry defaults inside the cleared window
makes the probe's own engine skip the notifications migration, failing
`legacy debug.log=true is effective` while `created by migration` passes for
a file a foreign engine wrote. Exposure is spurious parallel-sweep verdicts,
not checkout corruption (both probes clear-then-restore). Filed as #1444 with
requirements, acceptance commands, and out-of-scope boundaries.

## Fixed later (no action)

- PR #1327 made `bulk` a required item-definition field and did not run
  `transfer_order_probe.py`, whose inline item fixture it silently zeroed
  (every scenario failed as `empty_batch`). Fully remediated in-window: #1343
  made zero-registration fixtures abort loudly at setup
  (`probelib.load_fixture_yaml` / `FixtureNotRegistered`), #1344 repaired the
  fixture.

## Cleared

Everything else in #1423–#1297 cleared verification, including targeted
re-checks of: #1423's fail-closed audit classification (no remaining
implicit-threshold caller at HEAD; `world_stress.py`'s total `severity_of`
deliberately preserved); #1409's pre-update CI resolve genuinely running
against the image's baked index; #1407's 21 deletions having no remaining
references; #1406's byte-wise canonical ordering and non-persisted
`idSourcePath`; #1405's `Maybe`-port passthrough resolving to the same 8008
default through `patchBootConfig`; #1393's `chunkRegionCoords` reproducing
the old x-outer/y-inner enumeration exactly; the #1350/#1353/#1387/#1403
escort-arc slices composing consistently (one shared 7.5 lock, both-side
holds, session-failure noticing, and the integrated gate); #1346's
active-page claim-key assumption verified against
`Engine.Scripting.Lua.API.Units.List` (unit AI ticks active-page units only);
#1335's genuinely half-away-from-zero temperature rounding; #1334's
mutation-tested random-stream isolation; the #1309→#1317 atlas arc's
contracts matching what CLAUDE.md now states (including #1313's 19/19
mutation catches); the #1306/#1307/#1308 save-wire hardening trio's negative
verification against real production declarations; and #1298's release-plan
spec still asserting the resolution the production release path uses after
#1407 deleted the dead `getTextureSlotIndex` wrapper (the shader-side truth
is the GPU `handleToSlot` table synced at every `btsHandleMap` mutation).
