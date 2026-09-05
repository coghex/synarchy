# Give overdue Lua updates service during sustained message traffic

Filed labels: `bug`, `lua`

Approved by the owner and filed as [#2415](https://github.com/coghex/synarchy/issues/2415) on 2026-09-05. Source verified against `13fa8126ad8d0126abd04eeaf77705aa1809a087`. One issue and one PR; independent of the stance/stamina repairs and the future gameplay-clock design. Implementation has not started in this audit.

## Background

The Lua worker gives queued messages priority until their queue is empty, then does the same for console commands. It runs timed script updates only when a subsequent message wait times out. Continuous traffic can therefore keep AI and physiology waiting indefinitely while movement on another worker continues.

Three production paths admit this failure:

- `src/Engine/Scripting/Lua/Thread.hs:396-449`: only the timeout branch calls `runDueScripts`. Repeated message wakeups can prevent timers from running even when each intervening drain finishes.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:46-58`: `processLuaMsgs` recursively consumes messages until empty, including arrivals during dispatch. A continuously replenished queue prevents return to the scheduler.
- `src/Engine/Scripting/Lua/Thread/Console.hs:46-56`: `processDebugCommands` has the same unbounded-drain behavior.

This is verified from production control flow, not an executed flood experiment. Its frequency during ordinary play is unmeasured. The intended improvement is straightforward: waiting updates receive a turn even while messages keep arriving.

## Requirements

1. **Use finite ordinary-work batches.** Adopt named initial limits of **32 engine messages and 8 console dequeue attempts per scheduler round**. These are proposed starting constants, not measured optimal values. Count cancelled/unclaimable console entries against the console limit too. Leave remaining entries queued, preserve FIFO within each queue, and neither drop nor dispatch an entry twice. A message obtained by the blocking wait counts toward the following round's engine-message budget.

2. **Check timers independently of queue emptiness.** An ordinary runnable round processes up to the engine-message limit, checks eligibility and runs one due-script pass, then processes up to the console limit. Check fresh lifecycle/transaction state before switching work classes. Repeat without an idle wait while queued work remains or a timed script is already due. Do not repeatedly run overdue-script passes to exhaustion before allowing console/message progress. Between timer eligibility checks, ordinary queue work is bounded by the two limits; one handler, console command, or due-script pass is not preempted. This is a service-opportunity guarantee, not a millisecond deadline or a guarantee of constant simulation speed under overload.

3. **Retain efficient idle waiting and prompt wakeups.** When no work is ready, retain `Q.readQueueTimeout` and `schedulerSleepMicros` with their existing monotonic clock, sleep bounds, and paused/event-only exclusions. Both message wakeup and timeout return to the same scheduling policy. Do not use asynchronous timeout around a committed queue read, unconditional polling, or a mandatory sleep after every busy round.

4. **Preserve the save/load boundary when splitting a drain.** Check `ownerGated ... SaveLua` at round entry and between individual ordinary message/console dispatches so a handler that starts a transaction cannot leave the rest of its batch running through the park. Authorized inline transaction work retains its existing path. After publication releases the owner gate, `LuaSaveLoaded` may still be behind more than one batch of messages: continue consuming engine messages in FIFO batches, but admit no timed-script pass or ordinary console execution until required reconciliation has completed. Use the existing load lifecycle (`LoadWaitingPublish` spans publication and reconciliation), or an equivalent precise predicate over existing state; queue emptiness alone is insufficient. Do not skip ahead to reorder the reconciliation message. Preserve reconciliation-failure reporting, paused gameplay, and access to diagnostics. Do not blanket-disable diagnostics during asynchronous load staging by gating every `loadInProgress` phase.

5. **Keep existing script and command semantics.** Retain `runDueScripts`' captured due set, single deadline advance before callbacks, configured-interval `dt`, and callback-driven rescheduling behavior (#2205). Script pause and interval-zero behavior are unchanged; gameplay pause must not become a new blanket ban on UI updates. Recheck worker stop/pause state between ordinary dispatches and before starting another work class. Keep debug-command claim/cancel/complete semantics (#2282), stale-command cancellation during `LuaSaveLoaded`, and shutdown reply handling. Long-running Lua code remains nonpreemptible.

6. **Bound the ordinary scheduler without weakening synchronous APIs.** `API/InputInject.hs:148-152` and existing input/UI tests call `processLuaMsgs` to finish synchronous input settlement. Preserve those callers' drain-to-completion contract. Add bounded scheduler entry points, sharing dispatch primitives where useful; do not silently change every existing drain caller to return after 32 entries. Preserve the exhaustive console helper's existing callers as well. The fairness bound covers the outer scheduler, not arbitrary nested work inside a synchronous command or transaction handler. No new worker, persistent state, `EngineEnv` field, or save-format change is needed.

## Acceptance

Add a targeted headless describe named **`Lua scheduler fairness`**. Exercise the production scheduling path, extracting a small clock/wait/dispatch seam if necessary; testing a separate imitation loop or only an arithmetic budget helper is insufficient. Use controlled clock advancement and finite replenishment beyond the batch limits rather than timing-sensitive live floods.

- Repeated message wakeups cross a script deadline without any timeout: the script still runs. This must fail against the old message-only branch.
- Engine handlers replenish the queue past 32 dispatches: a due update runs while the queue remains nonempty. Console commands also make progress.
- Console work replenishes past 8 dequeue attempts, including cancelled entries: timers and engine messages continue receiving turns. Verify FIFO, exact-once dispatch, and retained leftovers in both queues.
- Continuously due timed work cannot consume repeated passes while denying the configured message/console opportunities. Assert work counts and ordering rather than an invented wall-clock latency target.
- An idle scheduler blocks through the existing timeout primitive, wakes for a message, and counts that message within its batch. Paused/event-only scripts do not force a busy loop.
- Owner parking prevents ordinary dispatch. A message or console command that establishes the park prevents later ordinary work in the same round. Worker stop/pause transitions likewise prevent subsequent ordinary work until permitted.
- Place `LuaSaveLoaded` beyond the first engine batch after publication. No timer or stale console command executes before reconciliation. Exercise successful and failed reconciliation, preserving command cancellation, error disposition, and diagnostic access. Retain asynchronous staging diagnostics.
- Preserve synchronous input settlement with more than one scheduler batch of pending messages. Retain existing callback self-rescheduling, other-script rescheduling, load/kill during a due pass, and console cancellation coverage.

Run the new describe and the focused existing regression groups:

```bash
cabal test synarchy-test-headless --test-options='--match "Lua scheduler fairness"'
cabal test synarchy-test-headless --test-options='--match "Lua tick-interval policy"'
cabal test synarchy-test-headless --test-options='--match "Lua scheduler reentrancy"'
cabal test synarchy-test-headless --test-options='--match "save snapshot barrier owner park"'
cabal test synarchy-test-headless --test-options='--match "LuaSaveLoaded"'
cabal test synarchy-test-headless --test-options='--match "debug-console command cancellation"'
cabal test synarchy-test-headless --test-options='--match "debug-queue shutdown drain"'
```

Use the quiet headless harness. New code must compile warning-free in the normal production profile. Run other gates only when their inputs change; no world generation, graphical launch, full suite, or full local CI is needed for this scheduler repair.

## Scope boundaries

This issue supplies fair scheduling opportunities within the existing Lua worker. It does not synchronize gameplay clocks, introduce catch-up ticks, replace configured-interval `dt`, decide background-colony simulation, bound queue memory, preempt Lua, or redesign other workers. Those architectural decisions remain separate. A slow individual callback can still delay everything on the Lua owner.

The implementation may update the finding's narrative evidence, but leaves report disposition fields to the report-processing lane.

## Related

- [CH-3: Lua timer starvation](simulation_consistency_findings.md#2415-ch-3-timed-lua-updates-can-starve-behind-sustained-message-traffic).
- [Architecture audit and continuation](architecture_conversation_audit_2026-09-05.md).
- [#1910](https://github.com/coghex/synarchy/issues/1910): queue telemetry; explicitly retained existing drain termination behavior.
- [#1695](https://github.com/coghex/synarchy/issues/1695), [#2204](https://github.com/coghex/synarchy/issues/2204), [#2205](https://github.com/coghex/synarchy/issues/2205): interval validity, timing, and callback rescheduling; preserve their contracts.
- [#2221](https://github.com/coghex/synarchy/issues/2221), [#763](https://github.com/coghex/synarchy/issues/763), [#2282](https://github.com/coghex/synarchy/issues/2282): owner parking, load reconciliation, and command lifecycle.

Tracker searches were refreshed before filing and found no matching issue. The published body retains this specification, uses immutable source links, and identifies the unpublished audit documents as local provenance rather than broken repository links. This file remains the local approved draft record; the GitHub issue is the implementation handoff.
