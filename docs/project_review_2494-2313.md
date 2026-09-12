# Project Review Findings: PRs #2494–#2313

Reviewed `coghex/synarchy` PRs #2494, #2341, #2340, #2331, #2309,
#2312, #2322, #2321, #2320, #2319, #2318, and #2313 against their linked
specifications, commits, first-parent landed patches, and surviving code.
Verification completed at `cfd30002dde1901f91ad7db251a07f1e01889330`;
the two newest landings since initial inspection changed documentation and
comments only. The older landing interval contains PR merges only; intervening
direct publications through #2494 were covered in preceding batches. Newer
landings are selected in the next batch. The previously excluded #2377 concern
remains excluded. No implementation or tracker changes were made.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. Correct the load-publication comment that assigns Lua queue discard to the render consumer
- [ ] PRR-2. Make the self-test flag check accept the supported explicit-argument parser call

## 1. Load-publication ownership documentation

### PRR-1. Correct the load-publication comment that assigns Lua queue discard to the render consumer

> **Captured note:** PR #2309 moved stale Lua-to-engine queue discard to
> the Lua producer before publication, but `publishStagedSession` still
> directs readers to a consumer-thread flush that no longer exists.

**Verification:** Traced the current successful staged-load path through
`commitLoadPublish`, the render owner gate, and publication. The producer
discards stale Lua-to-engine work before queuing publication. The render
gate does not perform that discard. The same publication comment also
incorrectly says Input is never a save owner; active input is now included.
Focused snapshot-barrier tests (30) and transactional-load tests (18) pass.
This is a verified documentation defect, not an alleged runtime failure.

**Evidence:**

- `src/World/Load/Publish.hs:77` — says Input is not a SaveOwner and is not quiesced.
- `src/World/Load/Publish.hs:87` — assigns Lua-to-engine discard to its own consumer and refers to the render save-barrier gate.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:503` — conditionally registers `SaveInput` when input is active.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:583` — successful Lua application enters `commitLoadPublish`.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:612` — producer-side discard precedes publication request dispatch.
- `src/Engine/Loop/Mode.hs:275` — render owner gating contains no corresponding stale-queue flush.

**Handoff context:**

- **Current behavior:** The implementation and save/load contract describe the producer-side cutover correctly; the publication comment teaches the obsolete thread ownership and rationale.
- **Expected behavior:** Documentation at publication must accurately distinguish each queue's discard owner and the conditional input owner membership.
- **Scope and constraints:** Correct the stale explanation without changing the working barrier or resurrecting the consumer-side flush removed during #2221/#2309 review.
- **Verification target:** Cross-check the revised comment against `commitLoadPublish`, render gating, input-owner registration, and the save/load transaction section of `docs/engine_contracts.md`.
- **Deduplication:** Open/closed searches for publication comments, consumer-thread Lua handling, and stale cutover found the original #2221 work but no correction for this remaining comment. #1078 concerns an older Mode-only pre-park explanation; #2381 concerns Combat acknowledgment comments. Existing findings reports contain no equivalent entry.
- **Remaining uncertainty:** None about the documentation mismatch; no new runtime failure was established.

## 2. Shared self-test contract validation

### PRR-2. Make the self-test flag check accept the supported explicit-argument parser call

> **Captured note:** The shared self-test audit touched by PR #2321 accepts
> only the literal `selftestlib.parse_verbose()` spelling, incorrectly
> failing a script that calls the supported `parse_verbose(argv)` API.

**Verification:** `python3 tools/test_selftestlib.py` exits 1 with exactly
one failed assertion: `test_probe_census_promotion.py accepts -v/--verbose`
(280 assertions executed). Running that target script without a flag,
with `-v`, and with `--verbose` succeeds in all three cases (66 assertions
each); both flags enable detailed passing-case output. The failure is
therefore in the audit's recognition of the call, not in flag support.

**Evidence:**

- `tools/test_selftestlib.py:359` — the flag-offering test scans source text.
- `tools/test_selftestlib.py:362` — checks the exact empty-argument call or `add_verbose_option(`, excluding explicit arguments.
- `tools/test_probe_census_promotion.py:553` — passes `argv` to `selftestlib.parse_verbose`.
- `tools/selftestlib.py:123` — explicitly supports an optional argument list and consumes both verbose flag spellings.

**Handoff context:**

- **Current behavior:** A conforming script makes the shared audit fail, leaving its overall verdict persistently red.
- **Expected behavior:** Accept supported argument-bearing calls while continuing to reject a converted script that offers neither verbose flag path.
- **Scope and constraints:** This failure predates #2321/#2313 and was explicitly acknowledged in their validation notes; retaining it is not a disposition. Correct the validator, not the conforming promotion script. Do not weaken the roster or drop the assertion.
- **Verification target:** The full focused `test_selftestlib.py` run should pass, with regression cases covering empty and explicit arguments plus a genuinely flagless negative control. Check both verbose spellings on the promotion script.
- **Deduplication:** Open/closed searches for `test_probe_census_promotion.py` with `verbose`, `parse_verbose(argv)`, and selftestlib flag checks found no issue for this defect. #2130, #2129, #2131 and related split work acknowledge or concern the surrounding refactors, not its correction. No equivalent entry was found in the local findings reports.
- **Remaining uncertainty:** No claim is made that this standalone audit is currently a CI job; its directly reproduced failure is sufficient evidence.
