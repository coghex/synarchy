# Coordinated test harness findings

This report preserves three current defects in manual probe fixtures and
oracles identified by the approved 2026-08-31 coordinated-test assessment.
Each finding concerns the reliability of test signals, not a confirmed
product regression.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The source assessment was
`20260831T202105Z-ui-debug-console-completion-combat-lunge-5fc34a`. It
correlated six observations and confirmed four test-harness defects.

The confirmed harness candidates were rechecked against current master
`c6e9652192c5f2074ae9f7e8b47c1d3769784826`. The relevant probe and
production paths have not changed since the assessed revisions. No scenarios
were rerun; this report relies on the retained test reports, their raw
artifacts, and current source inspection.

A search of the local findings corpus found that the lunge replacement-target
oracle defect is already preserved as unprocessed finding PRR-1 in
`docs/project_review_2004-1710.md`, so it is not duplicated here. The
inconclusive transfer-session observation and expected debug-console behavior
were also omitted.

This drafting pass did not search or mutate the GitHub tracker. Tracker
deduplication and final disposition belong to the one-at-a-time
`process-report` workflow. No production code, test code, remote state, or
tracker state was changed.

## Status

- [ ] TH-1. Lua strict-message probe no longer reaches its advertised exception path
- [ ] TH-2. Offscreen portal coverage depends on an unseeded bounded search
- [ ] TH-3. Construction probe treats an unsupported 30-second sample as failure

---

## False-green regression coverage

### TH-1. Lua strict-message probe no longer reaches its advertised exception path

`tools/lua_strict_msg_probe.py` still describes itself as regression coverage
for a deferred exception caused by strict UTF-8 decoding of malformed Lua
text. Its actual assertions only establish that the engine remains alive and
that a subsequent `return 1+1` command succeeds.

The production path no longer supplies the exception that the probe claims to
exercise. `Engine.Scripting.Lua.API.Text.setText` decodes the submitted bytes
with `decodeUtf8Lenient`, replacing malformed input instead of throwing.
Consequently, the probe can pass without testing the deferred-exception
handling or the `Strict`/`StrictData` forcing behavior named by its
documentation.

Evidence:

- `tools/lua_strict_msg_probe.py:2-23` says malformed UTF-8 reaches strict
  decoding and exercises exception forcing.
- `tools/lua_strict_msg_probe.py:49-53` declares only `engine_alive` and
  `console_responsive` assertions.
- `tools/lua_strict_msg_probe.py:81-121` sends the malformed byte, waits, and
  checks process liveness plus the result of `return 1+1`; it does not prove
  that an exception was raised or logged.
- `src/Engine/Scripting/Lua/API/Text.hs:105-116` uses `decodeUtf8Lenient`
  before constructing the text request.
- `.git/codex-test/reports/20260831T191648Z-probe-lua-strict-msg-ce178a.test-result.md`
  reports a clean 1/1 result in 4.8 seconds, despite the advertised exception
  stimulus being absent.

Expected direction:

A probe advertised as deferred-exception regression coverage should
deterministically prove that the throwing construction path occurred before
grading engine survival and responsiveness. If no current Lua-to-engine
request can safely and deterministically create that condition, the probe
should be renamed, rescoped, or retired instead of retaining a false claim of
coverage.

Preserve the production path's intentional lenient text handling, the
`Strict`/`StrictData` hardening, and the value of a real-engine liveness and
follow-up responsiveness check. Do not reintroduce strict UTF-8 decoding
solely to make the existing probe stimulus throw.

It remains uncertain whether another current request type can reproduce the
original deferred exception without adding test-only production behavior.

## Nondeterministic manual-probe fixtures

### TH-2. Offscreen portal coverage depends on an unseeded bounded search

The offscreen probe creates an unseeded generated world and searches ten fixed
candidate coordinates for a buildable remote portal location. When none is
valid for that random world, it records a failed assertion and returns from
the portal phase before exercising the remote warning, modal, cancellation,
confirmation, or placement behavior.

One retained run failed exactly this setup search, while a later run of the
unchanged probe on another generated world reached and passed the complete
portal path. The differing outcomes establish fixture variability: the first
failure does not identify a product regression, and a clean later run does
not make the bounded search deterministic.

Evidence:

- `tools/offscreen_probe.py:319-343` returns the first supplied coordinate
  satisfying both buildability and remoteness checks, or `None` when the
  candidates are exhausted.
- `tools/offscreen_probe.py:377-395` supplies ten fixed candidates and records
  a failed normal assertion before returning when none works.
- `tools/offscreen_probe.py:1053-1071` creates the world through the real UI
  without fixing its generation seed.
- `.git/codex-test/reports/20260831T181523Z-probe-offscreen-5e5831.test-result.md`
  reports the failed setup assertion and contains no remote-portal
  screenshots.
- `.git/codex-test/reports/20260831T200303Z-probe-offscreen-46d8b0.test-result.md`
  records a complete successful portal path from the unchanged probe on a
  later unseeded world.

Expected direction:

The fixture should deterministically establish the buildable-and-remote
preconditions, or explicitly classify inability to establish them as a setup
or inconclusive result distinct from a product assertion failure. Diagnostic
evidence for rejected candidates should remain available.

Preserve the real offscreen Vulkan boot, UI-driven world creation, portal
ghost and modal behavior, cancel and confirm paths, nearby placement, final
location icon, and the later save/load/restart coverage. The probe may remain
manual and GPU-dependent.

The appropriate deterministic fixture—such as a pinned generated world,
fixture-owned terrain, or a reproducible coordinate derivation—still requires
design. The retained evidence demonstrates variability but does not establish
its frequency.

### TH-3. Construction probe treats an unsupported 30-second sample as failure

After a construction job is claimed, the probe requires observable progress
within 30 seconds. It then independently allows another 30 seconds for the
slope mask and 60 seconds for terminal completion.

A retained run missed only the first progress deadline: the slope check and
the final floor-placement and designation-clearing checks subsequently
passed. The engine reported no error. This means the failed assertion was a
timing sample unsupported by a demonstrated scheduling contract, rather than
evidence that construction was stuck or incorrect.

Evidence:

- `tools/construction_probe.py:234-242` allows 20 seconds for a claim and then
  requires progress greater than zero within 30 seconds.
- `tools/construction_probe.py:244-251` separately waits up to another
  30 seconds for the slope mask.
- `tools/construction_probe.py:253-258` allows up to 60 further seconds for
  both floors to be placed and the designations to clear.
- `.git/codex-test/reports/20260831T193619Z-probe-construction-3291d8.test-result.md`
  reports 67/68 assertions: the intermediate progress sample failed, while
  the later slope and terminal construction assertions passed.
- An earlier run at revision `3c34c260` passed all 68 assertions, further
  indicating that the fixed intermediate deadline is sensitive to scheduling.

Expected direction:

The intermediate assertion should be causally tied to construction state or
supported by a measured and documented scheduling bound. Eventual valid
progress and completion should not be reported as a product failure solely
because one unsupported wall-clock cutoff was missed.

Preserve the real AI scheduler, inventory sourcing, slope progress, complete
construction outcome, and later scenario phases. Simply enlarging the timeout
without evidence would retain the same unsupported oracle in a slower form.

The retained artifacts do not contain exact claim-to-first-progress latency,
and only one missed sample has been preserved. The recurrence rate and any
legitimate upper bound therefore remain unknown.
