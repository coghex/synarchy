# Synarchy workflow test — issue2648 / PR2762

PR2762 merged by the already-active external drainer at2026-09-30 17:08:17UTC. Merge `8e317ca88b79dd34ae8b7db1e5004ba178d013dc`, head `ea31b0d0e475b9313a7541b69fa07f5280ab50df`, base master `d974d0b4ab777182d85dddc82755ab2a04eb0903`. Issue2648 closed. Primary master is clean; the drainer removed only this PR's verified worktree and branches. No additional issue was undertaken.

The quiet Chop fixture now uses the shared logging harness without changing its assertions or command logger. The import guard preserves CPP and audits the actual configured compiler invocation captured by the Custom Setup hook, with pinned Cabal3.16.1.0 and content-bound configuration/compiler provenance. Owner-approved scope certifies actual native macOS and Linux CI configurations, not a hypothetical matrix. It adds no extra full project build or structured dependency.

Solver: original Claude Code session `050c8640-aafd-4565-9d72-e604e62102bf`, `claude-opus-5-5`. Every newly invoked issue/PR reviewer was explicitly `gpt-6.1-sol`, effort `xhigh`. No model substitution.

Validation: exact-head CI36744783577 fully green;281 guard self-tests;14 real-Cabal regression steps;515 modules across two captured ways;10,553 CI examples,0 failures,1 pending. Native evidence includes fresh unset/quiet/stderr captures,20 Chop and8 logging examples with zero failures, required forbidden-import injection/restoration, compiler symlink retarget/restoration and stale imported-config controls. Final6.1-Sol review12 APPROVE17:12:14UTC,23.55min, no required blockers.

## Material workflow failure

The drainer merged237.63seconds before final review12 exited, retaining approval of prior0db after a master update, despite the `blocked` hold. I relied on a hold label this installed drainer does not honor. Installed selection and final gates check approve/changes labels, current-head rejection and review-approved status, but do not enforce configured blocking labels or a positive exact-head approval marker. Root's stricter manual gate correctly refused the old marker; no root merge was attempted. Canonical publication of the finishedEA approval then refused `PR #2762 is not open`. No replacement marker, reopening, global service change, revert or shared-tool patch was attempted. Final same-tree review and CI pass, but the user's exact-head-before-merge requirement was violated and cannot be repaired retroactively. The owned hold label remains on the merged PR.

## Hiccups and proposed improvements

| Stage | Symptom / recovery | Proposed improvement |
| --- | --- | --- |
| Scope and reviews1–6 | Source lexical/CPP guesses missed compiler behavior; proposed CPP ban required owner decision. Owner chose preserving CPP and actual configured-host checking. | Reassess architecture after two same-class review failures; size fixture and guard separately; define certified configurations early. |
| Reviews7–9 | mtime cache false refusal; incomplete generic build-info/options/include paths; wrong builddir; omitted imported settings; source-only injection refused. Owner approved actual argv capture and version-pinned provenance reader. | Inspect the real compiler boundary first; use tiny compiler-built contract packages early. |
| Capture/stale cache | A successful build could still reuse stale imported configuration. | Bind configuration closure hashes to actual successful captures; retain edit→build→record regression. |
| Review10 | Same-version compiler symlink retarget bypassed old-target hash. Fixed0db;14-step real-Cabal controls and fresh11b approval passed. | Validate the bytes currently executed at record and scan boundaries. |
| Canonical issue preflight | Combined --review --dry-run published review but omitted assignment history; default roster rejected requested6.1 marker. | Reject that combination or honor no-model/no-write dry-run. This task used supported explicit6.1 pins; shared history/roster untouched. |
| Capacity / execution transport | Root capacity failure; two execution resets; parent callback disappeared. Existing agent outcomes were inspected before recovery; durable PID/exit files avoided duplicates. | Keep durable agents/watchers and reusable isolated reviewer evidence; restore parent callbacks reliably. |
| External drainer / finalization | Retained old approval; ignored own hold; deleted worktree during active review; post-merge publication refused. | Enforce blocking labels and positive current-head approval at selection and immediately before merge; coordinate active worktree ownership. Owner-directed Kanban work remains outside this issue. |

Twelve completed PR reviews consumed about230.5minutes of reviewer wall time; two amended issue reviews add~6.5+6.1minutes. An interrupted review11 attempt is excluded. First solver03:15:45→merge17:08:17 elapsed13h52m32s, including owner decisions, implementation, CI and interruptions. All canonical findings were required acceptance failures; optional hardening was excluded.

- PR: https://github.com/coghex/synarchy/pull/2762
- Final CI: https://github.com/coghex/synarchy/actions/runs/36744783577
- Prior canonical0db approval: https://github.com/coghex/synarchy/pull/2762#issuecomment-5915404377
- Full evidence: `/tmp/synarchy-workflow-2648/`; final review: `review-result-12.json`; incident: `final-workflow-incident.json`; detailed history: `workflow-test-2648-hiccups.md`.

These durable reports are uncommitted in docs-wip, not landed or pushed. All own Opus/reviewer/watcher processes have exited. No engineering blocker remains in the final code; canonical exact-head publication is unavailable after merge and the merge-gate failure requires separate owner-directed follow-up.
