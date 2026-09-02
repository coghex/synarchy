# Epic Review Findings: Epic #741 — Harden UI input ownership, widget behavior, and responsive layout

This report records the current-HEAD review of epic #741 at
`dc9721904ed9abc7aae91c9fe60155c6d0e58a4b`. The epic declares nine
implementation children, #742 through #750; all nine are closed as completed
and their implementation pull requests merged. The resulting architecture
remains coherent at current HEAD: one paint-ordered page model governs modal
scope, pointer and scroll ownership are independent element policies, plain and
Shift wheel input share one dispatcher, release activation is invalidation
guarded, editable widgets use code-point-safe helpers, rendering and hit-testing
share clipping and interactive bounds, and menu/gameplay resizes preserve state
through their documented lifecycle. Later work also closed the two notable
historical seams: route-affecting page/ancestor changes now cancel pending
activations, and #750 adopted real clipping viewports for all four gameplay log
panels. One new current steering mistake survives: the closed epic's live body
still leaves every completed child unchecked.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. Epic #741 leaves all nine completed children unchecked — [#741]

## 1. Epic completion steering

### [#741] ER-1. Epic #741 leaves all nine completed children unchecked

> **Captured note:** Epic #741 is closed and its owner closure comment confirms
> that #742 through #750 all landed, but every phase-plan checkbox in the live
> body remains unchecked.

**Verification:** The contradiction is confined to the live tracker body. All
nine declared children are closed, all nine implementation pull requests
merged, and the owner's closure comment explicitly says every child landed and
that no follow-up work was dropped. The current contracts, implementation, and
focused gates agree with the completed arc. This is not evidence of a surviving
UI implementation defect; it is stale project-steering state on the epic that
can make completed work read as an unfinished roadmap.

**Evidence:**

- [Epic #741's live body](https://github.com/coghex/synarchy/issues/741) — the
  Phase A, B, and C lists still show #742 through #750 as nine unchecked items.
- [The owner's closure comment](https://github.com/coghex/synarchy/issues/741#issuecomment-5076066305)
  — enumerates all nine landed children, explains that the boxes were never
  ticked as they merged, and states that no follow-up work is being dropped.
- `docs/engine_contracts.md:902` and `docs/engine_contracts.md:1059` — preserve
  the completed arc's authoritative input-routing and responsive-lifecycle
  contracts.
- `src/UI/InputOwnership.hs:125` and `src/UI/Manager/Query.hs:153` — route
  pointer/scroll ownership through the current modal scope, paint order,
  clipping, and interactive bounds.
- `src/UI/ControlActivation.hs:135` and `src/UI/ControlActivation.hs:177` —
  snapshot page and ancestor route epochs and reject invalidated releases,
  including the historical page/ancestor interruption seams repaired later.
- `scripts/ui_manager_boot.lua:307` and `scripts/ui_manager_resize.lua:20` —
  enforce the current menu/gameplay resize split and post-HUD reflow ordering.
- `scripts/event_log.lua:340`, `scripts/combat_log.lua:325`,
  `scripts/injury_log_panel.lua:284`, and `scripts/unit_log.lua:350` — all four
  gameplay logs now install real clipping viewports, completing #747's
  deliberately assigned #750 adoption work.
- Current focused checks: `UI.InputOwnership` (37 examples),
  `UI.ElementInputPolicy` (36), `Input.WheelPolicy` (22),
  `UI.ControlActivation` (33), `UI.FocusNavigation` (50),
  `UI.UnicodeTextEditing` (7), `UI.Clipping` (50), `UI.PopupPlacement` (25),
  `UI.ResponsiveMenus` (110), `UI.InteractiveBounds` (61), and
  `UI.ResponsiveGameplay` (112) all pass — 543 examples, zero failures.

**Handoff context:**

- **Current behavior:** A reader opening the closed epic sees all nine phases
  presented as unfinished even though the issue state, every child state, the
  closure comment, and the live architecture say the arc is complete.
- **Expected behavior:** Mark #742 through #750 complete in the epic's Phase A,
  B, and C checklists so the durable roadmap agrees with the completed arc.
- **Scope and constraints:** Tracker-body-only correction to epic #741. Do not
  change the implementation, tests, child issues, dependency history, or
  closure comment merely to resolve stale checklist ink.
- **Verification target:** The closed epic shows all nine declared children as
  checked and retains the same child set, phase structure, and compatibility
  decisions.
- **Deduplication:** All-state tracker searches for #741's phase-plan boxes,
  title, and combined modal/responsive scope found no corrective owner beyond
  the epic itself. The docs-worktree report corpus has no pending owner for this
  tracker-body correction; matches for unchecked UI values or the separate
  #745 specification are unrelated.
- **Remaining uncertainty:** None about the contradiction. The correction is a
  mechanical checklist update.
