# Project Review Findings: PRs #2508–#2239

Reviewed `coghex/synarchy` PRs #2508, #2255, #2253, #2252, #2250,
#2249, #2248, #2246, #2247, #2245, #2242, and #2239 against their
linked specifications, commit messages, landed patches, and current callers.
Their GitHub patches match their first-parent landing diffs after stripping
diff metadata. The older landing interval contains only PR merges; #2508's
new landing was included separately. Verification used `57a6a4385`, with
the finding rechecked at `7b2833f79`; the intervening #2537 addition does
not touch its failure path. The previously excluded #2377 concern stays
excluded. No implementation or tracker was changed.

Focused headless checks passed: 3 frame-file ordering, 62 rotated-wall,
121 video-domain, 32 destruction-lifecycle, 13 frame-assembly, 6 accepted
load exception, 21 debug-socket, and 20 worker-lifecycle examples.
The probe README owner and its composed acceptance checks passed, as did
the readiness-label policy checks and mutation checks. The capability
self-test split preserves all 113 pre-existing explicit cases in their
original order, followed by the existing writer cases; its current expanded
suite had passed 237 groups / 538 assertions during this sweep. The docs
move preserves the original root verbatim in its archive and routes live
rules and gates to the named authorities. No full CI or fresh GPU session
was run; the headless render checks do not establish pixel correctness.
Later queue-cancellation and crash-diagnostic repairs (#2282/#2283) were
accounted for rather than reported again.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. Make console teardown bounds effective inside worker cleanup

## 1. Console and worker teardown

### PRR-1. Make console teardown bounds effective inside worker cleanup

> **Captured note:** PR #2246 installs timeout-bounded console joins inside
> worker cleanup that PR #2239 runs under an uninterruptible mask. The
> timeout cannot interrupt that cleanup, so the advertised join bound is
> ineffective in its production calling context.

**Verification:** Executed the real packaged `startDebugServer` and
`stopDebugConsole` in GHCi using a dynamically allocated loopback port.
The existing accept-injection seam waits on an MVar until a separate
thread releases it after 3.2 seconds. The ordinary stop returned after
2.001114 seconds; the same stop under `uninterruptibleMask_` returned
after 3.201095 seconds. The latter waited for the injected release instead
of applying its two-second timeout and killing the interruptibly waiting
accept thread. Both runs completed and their release helpers were joined.
The real socket suite's 21 examples passed but do not cover this calling
mask. No engine, window, or Lua state was launched for this reproduction.

**Evidence:**

- `src/Engine/Core/Thread.hs:374` — the shared loop runs uninterruptibly; only `wsTick` is unmasked at line 395.
- `src/Engine/Core/Thread.hs:385` — a cooperative stop calls `wsOnStop` in that mask; forced termination does the same at line 418, and crash cleanup at line 426 retains it too.
- `src/Engine/Scripting/Lua/Thread.hs:91` and `:116` — both Lua cleanup paths call `stopDebugConsole` before draining commands and closing Lua.
- `src/Engine/Scripting/Lua/DebugServer/Listener.hs:179` — the first accept-thread `readMVar` is wrapped in `timeout`, which cannot interrupt the inherited uninterruptible mask.
- `src/Engine/Scripting/Lua/DebugServer/Listener.hs:182` and `:189` — synchronous `killThread` calls also occur outside the following join timeout; a target that cannot receive the exception can block the sender.
- `src/Engine/Scripting/Lua/DebugServer/Listener.hs:192` — the documented per-thread bound is 2,000,000 microseconds; lines 147–149 claim bounded teardown even for a thread refusing to die.
- `src/Engine/Core/Thread.hs:463` and `:476` — the outer shutdown owner still has separate graceful and forced bounds. This finding is not a claim that its caller waits forever: it can instead reach its fatal-shutdown path without completing Lua cleanup.

Reproduction from the reviewed checkout, using its built package:

```sh
cabal exec -- ghci -v0 -ignore-dot-ghci -package synarchy
```

```haskell
import Engine.Scripting.Lua.DebugServer
import qualified Network.Socket as N
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import GHC.Clock
:set -XOverloadedStrings
let choosePort = bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close (\s -> do { N.bind s (N.SockAddrInet 0 (N.tupleToHostAddress (127,0,0,1))); N.SockAddrInet p _ <- N.getSocketName s; pure (fromIntegral p) })
let probe masked = do { p <- choosePort; entered <- newEmptyMVar; release <- newEmptyMVar; released <- newEmptyMVar; let { cfg = (defaultDebugServerConfig p (const (pure Nothing))) { dscAccept = \s -> putMVar entered () >> takeMVar release >> N.accept s } }; Right c <- startDebugServer cfg; takeMVar entered; _ <- forkIO (threadDelay 3200000 >> putMVar release () >> putMVar released ()); before <- getMonotonicTimeNSec; (if masked then uninterruptibleMask_ else id) (stopDebugConsole c); after <- getMonotonicTimeNSec; print (masked, fromIntegral (after-before)/1e9 :: Double); readMVar released }
probe False
probe True
:quit
```

**Handoff context:**

- **Current behavior:** Console cleanup inherits a mask that disables its timeout mechanism. An accept loop not woken by socket closure delays the worker's remaining cleanup beyond the stated bound; a permanently unresponsive thread can prevent that cleanup from finishing.
- **Expected behavior:** The stated console stop/kill/join bounds remain effective through the real cooperative, forced, and crash cleanup paths. Preserve exactly-once worker cleanup, quiet intentional console shutdown, and the no-use-after-Lua-close ownership contract; do not simply remove the mask without accounting for those guarantees.
- **Scope and constraints:** This is the integration of #2170/#2246 with #2165/#2239, not an authentication or evaluator-policy change. Decide explicitly what incomplete console teardown permits before freeing Lua, including what the outer fatal-shutdown path reports. A join timeout alone does not bound synchronous exception delivery.
- **Verification target:** Run the real listener through the actual worker cleanup callbacks with a delayed interruptible accept operation, and separately exercise a target that delays exception delivery. Assert elapsed bounds and completed resource ownership transitions, not only a filled worker-done cell or standalone unmasked shutdown. Retain debug-socket and worker-lifecycle focused suites.
- **Deduplication:** Open/closed searches for `stopDebugConsole`, listener shutdown, console timeout, and uninterruptible cleanup found the antecedents #2170/#2165 and subsequent #2282/#2283 repairs, but no issue tracking this inherited-mask mismatch. Local project-review reports contain no duplicate.
- **Remaining uncertainty:** The elapsed-time reproduction injects the accept stall; no operating-system accept failure was induced and no ordinary shutdown frequency is claimed. The production masking context and remaining cleanup order are established by current-code trace. The synchronous kill-delivery risk is static evidence, not a separate indefinitely blocked experiment.
