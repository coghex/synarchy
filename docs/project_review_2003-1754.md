# Project Review Findings: PRs #2003–#1754

This report records the senior review of the next twelve uncovered merged pull
requests in merge-date order — #2003, #1779, #1775, #1777, #1774, #1778,
#1776, #1773, #1764, #1756, #1755, and #1754. The review read each pull
request, its linked specification, merged diff and commits, then traced the
surviving behavior at current HEAD. The first-parent landing interval also
contains direct documentation commits `3cf352c` and `ae68014`, audited here,
plus nine documentation commits already individually audited in the overlapping
#2002–#1783 batch (`dc470999`, `1f591b9d`, `19af28ea`, `91444631`,
`83fddc35`, `99d73d07`, `0dd0cdc8`, `4960d4d9`, and `87ae3951`). Their
current descendants retain their intended design, findings-disposition,
probe-census, and review-record roles; the findings-report audit passes and the
audio concept consolidation left no surviving reference to the deleted file.

The sweep produced two confirmed current findings from PRs #1779 and #1775.
The other ten selected pull requests produced no separate current concern, and
no concern was explicitly excluded from this batch. Focused checks passed for
Lua log-source classification (18/18), location stamping (11/11), Vulkan
bindless requirements (43/43), handle-range refusal (7/7), UI structural
ownership (37/37), load-time pause gating (3/3), canonical chunk queuing (7/7),
pause-speed preservation (12/12), mental effectiveness (39/39), `unit.addXP`
(9/9), stopped transitions (21/21), allocator floors (10/10), location-instance
allocation (12/12), swapchain resize requests (11/11), and food nutrition
(46/46): 286 examples in total. No full headless suite, graphical session,
worldgen tier, world check, behavior probe, baseline capture, or `make ci` was
run.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. A declined queued piece can still leave a location stamped complete
- [ ] PRR-2. Bindless-capacity comments deny the effective limit the code enforces

## 1. Atomic location materialization

### PRR-1. A declined queued piece can still leave a location stamped complete

> **Captured note:** Make PR #1779's durable completion marker depend on the
> world thread actually committing every accepted structure placement, not on
> `structure.place` having staged and queued each one. The Lua result closes
> synchronous refusal paths, but the explicitly recognized eviction window can
> still retract a piece before the unconditional marker behind it is applied.

**Verification:** Confirmed by a complete static trace through the current real
queues and handlers. `structure.place` returns true immediately after staging a
piece and queuing `WorldSetStructure`. If the target chunk evicts before the
world thread handles that command, the handler deliberately declines the edit
and retracts only its staged entry. `location_stamper` has already interpreted
the earlier true as materialization and queues `WorldMarkLocationStamped`, whose
handler inserts the durable marker without checking the authoritative structure
overlay, edit log, target residency, or the declined attempt token. The next
host-chunk load therefore sees the marker and skips the geometry retry. The
focused 11-example gate passes because it replaces `scripts.structures` and the
world API with synchronous Lua stubs; it cannot exercise either production
queue or the second residency check.

**Evidence:**

- `scripts/location_stamper.lua:64-68` — any builder result with zero immediate
  failures queues the completion marker; there is no later acknowledgement
  boundary.
- `src/Engine/Scripting/Lua/API/Structure.hs:142-191` — the first residency
  check stages and queues the piece, then returns true. Its own contract at
  `:91-93` says the chunk may still evict before the world-thread check.
- `src/World/Thread/Command/Edit/Structure.hs:33-68` — that second check can
  decline the queued placement; the failure branch retracts the staging token
  and logs, but appends no edit and sends no completion result to the stamper.
- `src/Engine/Scripting/Lua/API/World/Edit.hs:244-264` —
  `world.markLocationStamped` independently queues the marker and carries no
  structure-attempt identities or success acknowledgement.
- `src/World/Thread/Command/Location.hs:93-105` — the marker handler writes
  `wgpLocationStamped` regardless of whether any preceding structure command
  committed.
- `test-headless/Test/Headless/Location/Stamping.hs:24-30,68-106` — the new gate
  explicitly runs without an engine, page, loaded chunk, or real structure API;
  its stub turns placement return values and marker persistence into immediate
  synchronous state.
- Issue #1719 explicitly put the post-acceptance #1674 eviction path out of
  scope even though its title and durable marker contract require actual
  materialization. #1674 repairs the staged phantom only; it does not retract or
  suppress a later location marker.

**Handoff context:**

- **Current behavior:** A piece can pass Lua's residency check, return true, and
  then be declined after its chunk evicts. The durable location marker still
  lands, so a partial or empty authoritative structure can be recorded as
  complete and skipped on every later load.
- **Expected behavior:** `wgpLocationStamped` becomes durable only after every
  attempted piece has committed to the authoritative overlay/edit log. Any
  declined accepted attempt leaves the host unmarked so the existing later
  host-chunk dispatch can retry it.
- **Scope and constraints:** Preserve asynchronous world-thread ownership,
  explicit page targeting, edit-log persistence, #1674's token-specific stage
  retraction, keyed retry idempotency, player-edit protection, and content
  spawning's independent one-time flag. The correction need not make neighbor
  loading redispatch a still-loaded host chunk.
- **Verification target:** Use the real Lua API and world queue in a headless
  fixture: begin a location stamp while its target is loaded, evict that chunk
  after `structure.place` queues its command but before dispatch, drain the set
  and marker commands, and prove both that no authoritative piece and no
  completion marker remain. A later host dispatch with residency restored must
  retry, commit, and mark exactly once. Retain the 11 synchronous aggregation
  cases.
- **Deduplication:** All-state tracker searches for asynchronous location
  markers, structure eviction, and partial-stamp decline found only closed
  #1719. `docs/project_review_432-412.md` PRR-6 was dispositioned to that issue;
  its full-materialization expectation includes this path, but #1719 explicitly
  excluded it. Closed #1674 owns stage retraction rather than marker completion,
  so no current tracker artifact owns this surviving boundary.
- **Remaining uncertainty:** The race was proven structurally rather than forced
  under real concurrent timing. Its two residency checks and decline branch are
  deliberate production behavior, so only its frequency—not reachability or
  result—remains unmeasured.

## 2. Vulkan descriptor-capacity documentation

### PRR-2. Bindless-capacity comments deny the effective limit the code enforces

> **Captured note:** Finish PR #1775's final effective-capacity correction by
> updating the round-nine comments it left above `CapacityScope` and
> `bindlessCapacityChecks`. They still say there is no combined effective limit
> and that neither paired field can substitute for the other, directly
> contradicting the round-ten implementation, tests, final review, and Vulkan
> Valid Usage rule immediately below them.

**Verification:** Confirmed at current HEAD and against the focused gate. For
every paired descriptor class, `bindlessCapacityChecks` separately preserves
the ordinary-only population check but evaluates the all-set population against
`max ordinary updateAfterBind`. The passing boundary case accepts an
update-after-bind value one below the requirement when the ordinary field
supplies it. The earlier prose says that exact case cannot happen. `git blame`
attributes both contrary blocks to PR #1775's round-nine commit `f982f8f`, while
the effective maximum was restored by its next commit `3915c91` without
updating them; all 43 focused examples pass because the executable contract is
correct.

**Evidence:**

- `src/Engine/Graphics/Vulkan/Texture/Requirements.hs:211-215` — the scope
  overview says there is no combined effective limit.
- `src/Engine/Graphics/Vulkan/Texture/Requirements.hs:399-408` — the function
  contract says neither paired limit substitutes for the other and every
  update-after-bind field is independently enforced.
- `src/Engine/Graphics/Vulkan/Texture/Requirements.hs:424-442` — the actual
  all-set check reports the effective field and uses the maximum of the paired
  values, with a correct local comment explaining that ordinary headroom can
  supply the total.
- `test-headless/Test/Headless/Graphics/BindlessFeatures.hs:523-535` — the
  focused boundary test requires acceptance when the update-after-bind field is
  short but the ordinary counterpart meets the all-set total.
- The official Vulkan descriptor-set Valid Usage statements for these all-set
  totals specify the maximum of the ordinary and update-after-bind limits,
  while separate statements constrain layouts without
  `UPDATE_AFTER_BIND_POOL_BIT`.

**Handoff context:**

- **Current behavior:** Runtime device acceptance is correct, but two prominent
  contract blocks teach maintainers the rejected round-nine interpretation and
  contradict the correct implementation only a few lines later. A future
  cleanup following those comments would reintroduce the repeatedly reviewed
  false-negative device rejection.
- **Expected behavior:** The comments distinguish the two simultaneous VUID
  scopes: the non-update-after-bind population has its independent ordinary
  check, while the all-set population uses the effective maximum of each paired
  ordinary/update-after-bind capacity. An update-after-bind-only field such as
  the all-pools limit remains unpaired.
- **Scope and constraints:** Documentation and any equally contrary test prose
  only. Preserve the current capacity inventory, descriptor counts, effective
  maximum, ordinary-only check, all-pools exception, diagnostics, and the
  passing 43-example behavior.
- **Verification target:** Re-run `--match "Vulkan bindless feature
  requirements"` and search the module/test narrative for claims that there is
  no effective maximum or that a lower paired update-after-bind value is an
  independent hard ceiling.
- **Deduplication:** Exact-phrase and semantic all-state tracker searches found
  no owner. Closed #1689 and #975 are the implementation and single-definition
  histories; neither tracks the surviving contradictory comments. No pending
  findings report records this documentation defect.
- **Remaining uncertainty:** None. The contradiction is local and the intended
  wording is fixed by the code, boundary test, final PR review, and Vulkan
  specification.
