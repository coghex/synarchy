# Project Review Findings: direct commits `ea2c03d`–`16daead`

This report reviews 12 direct first-parent commits, newest-first, from January 27 through February 1, 2026. Each suspicious change was traced into current master (`4c2a26d2`) before being recorded.

Because this is old code, defects repaired or eliminated by later work are summarized only as historical observations. The three full findings below still exist in current code.

Status legend: `[ ]` unprocessed · `[#N]` filed issue · `[no-issue]` verified but no issue needed · `[deferred]` intentionally postponed

## Review cursor

Reviewed commits:

1. `ea2c03dd` — basic UI system
2. `31dba18f` — properly handled window resize
3. `46b4f57d` — memory leak fix
4. `7ecd0555` — Vulkan.Command submodularized
5. `44a44a11` — Engine.Scene submodularized
6. `6f328490` — Engine.Loop submoduled out
7. `520429cc` — Scene.Graph cleanup
8. `2b550020` — organized graph code
9. `cc768d2b` — some more cleanup
10. `f17c3766` — Lua.Backend submoduled out
11. `029f39e4` — some cleanup
12. `16daead6` — lua bugfix

Next direct-commit cursor: `f753927e5af535379050dfc30150b3a3ef91fc62`.

## Status

- [x] PRR-7. Framebuffer resize events do not request swapchain recreation — [#1693]
- [ ] PRR-8. UI attachment can create multiple owners and stale descendant page ownership
- [ ] PRR-9. Lua tick intervals can busy-spin or permanently stall the scheduler

## Historical observations

- `46b4f57d` introduced a `Recreate.hs` module that was neither listed in Cabal nor compatible with that commit’s pipeline APIs. `31dba18f` rewrote and integrated the functionality the following day.
- `6f328490` and `f17c3766` used `utctDayTime` for elapsed-time calculations, causing discontinuities at UTC midnight. Later commits replaced both paths with monotonic-enough POSIX-based time sources.
- `16daead6` correctly removed an accidentally nested duplicate `calculateBoxHeight` definition.
- Several suspicious graph and UI behaviors from this period were eliminated by later scene-graph restructuring and the UI hardening work. They are not recorded as current defects.

## 1. Window and swapchain lifecycle

### [#1693] PRR-7. Framebuffer resize events do not request swapchain recreation

> Captured note: The window-resize work updates the engine’s recorded framebuffer dimensions and notifies Lua, but it does not directly tell the renderer to rebuild the swapchain.

**Classification:** Verified current defect; visible impact is platform and WSI dependent.

#### Verification

The GLFW framebuffer callback queues `FramebufferResize` in `src/Engine/Input/Callback.hs`. Its current handler in `src/Engine/Input/Thread/Dispatch.hs` only:

1. updates the framebuffer-size reference; and
2. sends the Lua resize notification.

It does not mark or request swapchain recreation.

`src/Engine/Loop/Frame.hs` recreates the swapchain only when acquire or presentation returns an out-of-date or suboptimal result. A successful presentation after a resize follows the normal path and retains the old swapchain.

This differs from the explicit VSync and MSAA handlers in `src/Engine/Scripting/Lua/Message/Video.hs`, which call `recreateSwapchain`. `handleSetResolution` and ordinary user resizing do not.

The swapchain extent is fixed when constructed in `src/Engine/Graphics/Vulkan/Swapchain.hs`. Therefore, on a WSI that continues returning success after a resize, the old extent can remain active until some unrelated event causes recreation.

The [Khronos swapchain-recreation guidance](https://docs.vulkan.org/tutorial/latest/03_Drawing_a_triangle/04_Swap_chain_recreation.html) explicitly handles framebuffer-resize notification because automatic out-of-date reporting is not guaranteed on every platform. Vulkan also permits platform-specific scaling behavior when surface and swapchain extents differ, as described by the [surface-maintenance proposal](https://docs.vulkan.org/features/latest/features/proposals/VK_EXT_surface_maintenance1.html).

`tools/video_window_check.py` verifies that the recorded window and framebuffer dimensions change, but its final rendered-picture check remains manual. It cannot detect this defect when the driver continues returning success.

#### History

`31dba18f` claimed to properly handle window resizing, but integrated only the dimension and Lua-notification path. `029f39e4` later removed a nearby “Trigger swapchain recreation” TODO without adding the missing trigger.

#### Handoff context

Current behavior:

- A resize updates engine and Lua geometry.
- Swapchain recreation depends on the driver returning an exceptional status.
- Some platforms may continue rendering through the old extent, with scaled, stretched, stale, or otherwise platform-specific output.

Expected behavior:

- Every real framebuffer-size change should schedule exactly one safe swapchain recreation.
- Minimize and restore behavior must remain safe.
- Acquire/present semaphore lifecycle must remain valid.

Useful verification:

- Add a GPU-independent seam proving that a framebuffer resize schedules or consumes a recreation request.
- Manually resize on a platform that can continue returning success.
- Preserve the existing Lua/UI resize notification and VSync/MSAA behavior.

Deduplication:

- Closed issue #7 covered integrating current size references into callbacks, not the missing render-side trigger.
- Closed issue #9 concerned dynamic viewport/scissor handling and does not own this lifecycle defect.
- No matching current finding or tracker owner was found.

Remaining uncertainty:

- The exact visible artifact depends on the active platform and driver; it was not reproduced graphically during this report-only review.

## 2. UI hierarchy ownership

### PRR-8. UI attachment can create multiple owners and stale descendant page ownership

> Captured note: The UI hierarchy’s attachment functions append structural references without enforcing one-owner membership, and cross-page subtree attachment updates only the subtree root’s page.

**Classification:** Partially verified current defect; structural corruption was reproduced, while no current shipped Lua caller was found exercising the problematic reuse path.

#### Verification

`addElementToPage` in `src/UI/Manager/Hierarchy.hs` appends the element handle to the page root list without:

- checking whether it is already present;
- removing it from a previous page or parent; or
- rejecting an already-attached element.

Calling it twice creates duplicate structural references.

`addChildElement` similarly appends to the new parent and updates only the immediate child’s `ueParent` and `uePage`. Descendants retain their previous page handles.

This conflicts with the same module’s documented detach-and-reattach support and handle reuse. Removal in `src/UI/Manager/Core.hs` trusts the element’s single recorded current owner, so it cannot reliably clean multiple stale references.

A current-code REPL reproduction produced:

```text
Just [ElementHandle 1, ElementHandle 1]
(Just (PageHandle 2),Just (PageHandle 1))
```

The first result is a page containing the same root twice. The second shows a moved subtree whose root belongs to page 2 while its grandchild still belongs to page 1.

Rendering and hit testing recursively traverse page roots and child lists, so duplicate links can cause duplicate traversal. Focus, page visibility, and input-scope logic consult each element’s stored page; a descendant painted beneath page 2 can therefore still be scoped or cleared as a page-1 element.

#### History

The append-only ownership behavior originated with the initial UI implementation in `ea2c03dd`. Later cycle checks and activation-epoch protections did not add ownership validation or recursive page propagation.

#### Handoff context

Current behavior:

- Repeated attachment can make one element structurally reachable more than once.
- Moving a detached subtree between pages can leave descendants associated with the old page.
- Subsequent detach or delete operations can clean only the owner recorded on the element.

Expected behavior:

- Every live element should have at most one structural owner.
- Attachment should either be idempotent, reject attached elements, or atomically relocate them.
- Moving a subtree between pages should update page ownership consistently for every descendant.

Constraints:

- Preserve cycle prevention.
- Preserve the rule that attachment alone does not invalidate activation through unrelated global epoch changes.
- Account for control focus, text focus, visibility, navigation, input scope, rendering, and hit testing.
- Tests can exercise the manager directly without a GPU.

Deduplication:

- No matching hierarchy-owner or reparenting issue was found.
- CH-122’s completed UI-tree review covered layer ordering, clipping, interactive bounds, paint order, and page epochs, but not element ownership.
- No pending findings report owns this invariant.

Remaining uncertainty:

- Existing production Lua appears to attach freshly created elements. The confirmed corruption path currently depends on direct API reuse or reparenting, although that reuse is explicitly supported by the public hierarchy API.

## 3. Lua scheduler timing

### PRR-9. Lua tick intervals can busy-spin or permanently stall the scheduler

> Captured note: Lua tick intervals accept zero, negative, NaN, and infinite values, but the scheduler assumes a usable finite interval.

**Classification:** Verified current defect; zero-rate scripts exist in the repository today.

#### Verification

`setTickIntervalFn` and the script-loading path in `src/Engine/Scripting/Lua/API/Core.hs` accept any Lua number without checking that it is finite or nonnegative.

`src/Engine/Scripting/Lua/Thread.hs` then:

1. finds the minimum next-wake time;
2. clamps ordinary waits to a one-millisecond floor;
3. considers due scripts ready;
4. advances `nextTick` by adding `tickRate`.

Current arithmetic produces these outcomes:

- `0`: due every millisecond and never advances, creating an approximately 1 kHz update loop.
- Negative: remains overdue and moves farther into the past.
- `NaN`: is never due but can keep the scheduler polling at the minimum delay.
- Positive infinity: never becomes due and currently converts to a zero-microsecond timeout on the active GHC toolchain, producing an immediate loop.
- Negative infinity: remains perpetually overdue.

Zero is not merely theoretical. Several current behavior probes load event-driven Lua fixtures with a `0.0` tick rate, including `tools/input_check.py` and `tools/action_outcome_probe.py`. Those callers appear to intend “event-only,” but the scheduler interprets zero as “call update repeatedly at the minimum delay.”

#### History

`f17c3766` preserved the unchecked interval contract while splitting the Lua backend into modules. The underlying behavior predates that structural split, but it remained present when the affected scheduler and API were moved and remains present on current master.

#### Handoff context

Current behavior:

- The API reports success for unusable intervals.
- Zero-rate event modules create continuous polling.
- Other invalid numeric values can stall ticking or busy-spin the worker.

Expected behavior:

- Tick intervals should have an explicit finite-value policy.
- Given existing callers, zero should likely mean event-only rather than being rejected, unless those callers are migrated.
- Positive finite intervals should retain current tick behavior.
- No accepted interval should produce a zero-timeout loop or perpetual minimum-delay polling.

Constraints:

- Preserve pause/resume behavior.
- Preserve script-path deduplication and queue responsiveness.
- Decide and document the minimum supported positive interval.
- Add scheduler/API tests for zero, negative, NaN, infinities, and an ordinary positive rate.

Deduplication:

- Searches for tick interval, script tick, scheduler spin, and `setTickInterval` found no existing owner.
- Existing Lua findings concern script identity and loading behavior, not scheduler interval validation.

Remaining uncertainty:

- The repository’s zero-rate usage strongly suggests an event-only convention, but that contract is not currently stated explicitly.
