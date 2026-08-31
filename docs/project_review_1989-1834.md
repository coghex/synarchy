# Project Review Findings: PRs #1989–#1834

This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1989, #1838, #1852, #1851, #1847, #1843, #1841, #1835, #1840, #1836, #1839, and #1834 — plus direct first-parent commits `4960d4d9`, `0dd0cdc8`, `99d73d07`, `83fddc35`, `91444631`, `19af28ea`, `1f591b9d`, and `dc470999` in the same landing interval. The review read each pull request, its linked specification where one existed, merged diff and commits, then traced the surviving behavior at current HEAD. The first three direct commits were mentioned in the preceding project-review report but were re-audited here because report reconciliation does not itself establish direct-commit cursor coverage. PR #1989 produced the one current concern below. The other eleven selected pull requests and all eight direct documentation commits produced no separate current concern, and no concern was explicitly excluded from this batch.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. A HUD visibility transition can retire sticky tutorial rows before any frame renders them

## 1. Tutorial presentation acknowledgement

### PRR-1. A HUD visibility transition can retire sticky tutorial rows before any frame renders them

> **Captured note:** Do not treat the update that changes the tutorial page from hidden to visible as proof that its sticky rows were presented. That same synchronous call can acknowledge the rows and delete them again before the renderer ever snapshots the visible page.

**Verification:** Verified against PR #1989 / issue #1941, the current Lua-to-UI mutation path, the renderer's snapshot boundary, and the focused headless coverage. On the hidden-to-visible edge, `tutorialHud.update()` first shows the page, immediately acknowledges every laid-out sticky row, then rebuilds after the model hides those rows. Both page visibility and element deletion mutate the shared UI manager synchronously. No completed render or renderer acknowledgement occurs between them, so the next renderer snapshot may observe only the final visible-but-empty page. The passing focused test explicitly treats the transition itself as presentation, confirming that the gate encodes rather than detects this path.

**Evidence:**

- Issue #1941's approved specification clarifies that acknowledgement counts only when a sticky row is actually exposed on a visible, open HUD page; merely building behind a hidden page does not count. PR #1989 likewise says acknowledgement reports the viewport from “the frame just past,” never one built and unbuilt by the same tick.
- `scripts/tutorial_hud.lua:447-486` — `acknowledgePresentedRows` uses `_hudVisible` and the previously laid-out `_rows` as its entire presentation witness, then immediately calls `tutorialProgress.acknowledgePresented` for those ids. It receives no render/frame acknowledgement.
- `scripts/tutorial_hud.lua:793-815` — on a hidden-to-visible transition, the same `update()` call sets `_hudVisible`, invokes `applyPageVisibility()` to show the page, invokes `acknowledgePresentedRows()`, observes the resulting model change, and calls `rebuild()` to remove the retired rows. The comment's “frame just past” premise is false on this edge: the page was hidden until line 799.
- `scripts/tutorial_hud.lua:493-512` — rebuilding deletes every prior row element through `UI.deleteElement`, while page visibility is changed through `UI.showPage`/`UI.hidePage`.
- `src/Engine/Scripting/Lua/API/UI/Page.hs:56-63` and `src/Engine/Scripting/Lua/API/UI/Hierarchy.hs:62-79` — showing the page and deleting its elements synchronously mutate the same shared UI-manager reference from Lua.
- `src/UI/Render.hs:102-120` — the renderer learns the UI state only when `renderUIPages` snapshots that manager. There is no handshake forcing this read between the same update's show and delete mutations; correctness would therefore depend on an incidental cross-thread race.
- `test-headless/Test/Headless/UI/TutorialHud.hs:641-667` — the hidden-HUD regression sets `hud.visible = true`, calls `th.update(0)` twice, and requires the rows to be gone. Its comment explicitly says “the transition alone is the presentation”; the fixture has no renderer and cannot establish that a row was painted between those calls.
- `tools/tutorial_hud_probe.py:641-724` — the graphical phase reads rows from the synchronous open build, then only checks that an `already_latched.png` screenshot request answers before polling for retirement. It does not assert that the sticky row glyphs or markers occur in that captured frame, so an empty-panel capture can still satisfy the #1941 phase.
- Review-time focused gates passed: `--match "Tutorial HUD"` (22 examples), `--match "Tutorial progress"` (36), the repair-ground-target group (24), both field-toolbox groups (3 and 2), and location-loot determinism (20). The two tutorial groups demonstrate that model retirement and the current transition assumption are covered; neither supplies a renderer observation at the rising edge.
- All-state tracker searches for tutorial sticky-row visibility, presentation frames, and acknowledgement found only closed #1941 and its predecessors #996/#960. The findings-report corpus contains the processed concern that became #1941, but no entry for this implementation's hidden-to-visible race.

**Handoff context:**

- **Current behavior:** If the checklist was built open while the gameplay HUD was hidden, the update that notices `hud.visible` becoming true can spend every sticky suppression and delete those rows before any frame renders them. The completed terminal branch then disappears without satisfying #996's “seen at least once” guarantee, even though the model and current tests say it was presented.
- **Expected behavior:** A sticky row is acknowledged only after the visible, open page has crossed a presentation boundary that guarantees the row was available to a rendered frame. The hidden-to-visible mutation itself is not that proof. Collapsed panels and rows outside the viewport must remain non-presented exactly as they are now.
- **Scope and constraints:** Preserve `getViewModel()` as a pure read, durable completion and load reconstruction, idempotent acknowledgement, live-subobjective reactivation, the non-persisted presentation policy, HUD visibility/collapse/scroll gates, and issue #1941's explicit rejection of a timed or minimum-duration interval. This finding does not require persisting UI history or changing tutorial evaluation.
- **Verification target:** Add deterministic coverage that builds sticky rows behind a hidden HUD and proves the visibility-edge update cannot retire them without a presentation boundary. Extend the graphical acceptance path to assert the row glyphs or markers are present in at least one captured frame before the empty completed state, rather than only asserting that the screenshot request returned.
- **Deduplication:** No separate tracker owner or findings-report entry was found. Closed #1941 owns the intended lifecycle but is the source of this incomplete implementation, not an open owner of the surviving defect.
- **Remaining uncertainty:** The safest renderer-to-Lua presentation signal is a design choice. A later update after a guaranteed frame opportunity, an explicit frame generation/token, or another render-observable acknowledgement could satisfy the contract; the confirmed defect is that `_hudVisible` changing inside the current call does not.
