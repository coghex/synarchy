# Epic Review Findings: Epic #349 — State of mind and thoughts

This report records the current-HEAD review of epic #349 at
`650578ad6de470a8d97e5808987e75441d07e834`. The epic has no native GitHub
sub-issues and declares three in-scope implementation children in its build
order: #350 (unified state of mind), #351 (thought generation and log), and
#352 (mental-state episodes). All three are closed and their implementation
pull requests #523, #524, and #716 merged. The body and closure comment
explicitly excluded #353 as a separately designed deferred follow-on, so it is
not counted as an epic child; it later completed through PR #874 and now
connects concentration and euphoria to combat and crafting. The current arc is
functionally coherent: physiology and psychology feed one aggregate without
changing the consciousness-only collapse gates, thoughts form the promised
two-way mood loop and surface in the unit log, mental states add hysteretic
episodes and AI consequences, and the later effectiveness integration has one
cross-language calculation. Later work also repaired mental-break preemption,
catatonia/lash-out behavior, airborne transition handling, non-finite
effectiveness, and two weak psychology-probe oracles. HPA-4 already owns the
remaining current defect where thought history survives session replacement,
so it is not duplicated here. One new current steering mistake survives: the
epic and `brain.lua` still describe already-landed consumers as deferred or
future work, and the latter calls `state_of_mind` read-only despite #352
thresholding it directly.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. The epic and brain contract still describe landed psychology consumers as future work — [#2233]

## 1. Current psychology ownership

### [#2233] ER-1. The epic and brain contract still describe landed psychology consumers as future work

> **Captured note:** Epic #349 still says #353 remains deferred and leaves it
> unchecked after #353 landed, while `brain.lua` still calls
> `state_of_mind` a read-only summary for future #351/#353 tie-ins even though
> #352's live mental-state machine thresholds that aggregate directly.

**Verification:** This is current documentation and project-steering drift, not
a missing runtime feature. The epic was correctly closed with #353 excluded at
the time, but #353 later closed through PR #874 and the body still states it
“remains deferred” in three places. In the implementation, `brain.lua`'s header
still says the aggregate is read-only and names #351 and #353 as future
consumers. The current update order explicitly feeds freshly computed
`state_of_mind` into #352's threshold machine; #351 consumes the mood substrate,
and #353 consumes concentration plus the euphoria state through the canonical
Haskell effectiveness function. The stale description therefore obscures the
real boundary while the implementation and focused checks agree with one
another.

**Evidence:**

- [Epic #349's live body](https://github.com/coghex/synarchy/issues/349) — says
  “#353 remains deferred,” leaves #353 unchecked in the build order, and lists
  that integration as deferred despite its later completion.
- [Issue #353](https://github.com/coghex/synarchy/issues/353) and
  [PR #874](https://github.com/coghex/synarchy/pull/874) — closed and merged on
  2026-07-22, after the epic's 2026-07-10 closure.
- `scripts/brain.lua:40-46` — calls `state_of_mind` a read-only summary for
  logs, UI, and “future tie-ins,” naming the already-landed #351 and #353.
- `scripts/unit_resources.lua:102-121` — computes the aggregate, applies the
  thought mood nudge, and then invokes #352's mental-state threshold machine
  over the freshly computed value.
- `scripts/mental_state.lua:1-28` — defines stressed, break, and euphoria
  transitions and behavioral consequences directly from `state_of_mind`.
- `src/Combat/Resolution/Common.hs:97-121` — documents and implements #353's
  current shared concentration/euphoria effectiveness consumer.
- Current focused verification passed: `Mental effectiveness` (39 examples),
  `state_of_mind`, `thought`, `mental_state`, and `mental_efficiency` (four
  real-engine probes), plus the probe-policy self-test and Lua module-budget
  audit.

**Handoff context:**

- **Current behavior:** The closed epic presents implemented #353 work as
  still deferred, and the owning aggregate's source contract tells maintainers
  it is only a read-only future substrate even though current behavior already
  depends on it.
- **Expected behavior:** Preserve the historical fact that #353 was not needed
  to close the original three-child epic, but mark the follow-on's present
  completion. Update `brain.lua` to state the real ownership: physiological
  collapse/confusion remains consciousness-only; `state_of_mind` drives #352's
  psychological state machine; thoughts consume and move mood; and #353 derives
  effectiveness from concentration plus euphoria.
- **Scope and constraints:** Documentation and tracker-steering corrections
  only. Do not retune mood, consciousness, thoughts, state thresholds,
  effectiveness, or the original child boundary. Keep #353 classified as a
  later follow-on rather than retroactively making it a prerequisite of epic
  completion.
- **Verification target:** The epic no longer describes #353 as currently
  deferred, and the `brain.lua` header agrees with the live update order and
  consumer ownership. The focused psychology probes and `Mental effectiveness`
  group remain unchanged and green.
- **Deduplication:** All-state tracker searches for #349/#353 deferral,
  `state_of_mind` future tie-ins, and stale psychology documentation found only
  the source epic and already-completed implementation issues. Open psychology
  issues and pull requests are empty. The docs-worktree report corpus has no
  pending owner for this steering correction; HPA-4 owns the distinct
  save-replacement history defect, while #1733, #1709, #1759, and #1761 are
  closed implementation or probe corrections.
- **Remaining uncertainty:** None about the contradiction. The wording should
  distinguish original epic scope from present project state so historical
  closure is not rewritten.
