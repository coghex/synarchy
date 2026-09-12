# Project Review Findings: PRs #2532–#2256

Reviewed `coghex/synarchy` PRs #2532, #2267, #2266, #2265, #2264,
#2263, #2261, #2260, #2259, #2258, #2257, and #2256 against their
linked specifications, commit messages, landed patches, and current code.
The older landing interval contains only PR merges; the new #2532 landing
was also included. Verification used `28bd42c0140698efd28e9bd8433073583307b8d1`;
subsequent merges are reserved for the next batch. The previously excluded
#2377 concern remains excluded. No implementation or tracker was changed.

The rebuilt headless executable passed 127 tutorial, 20 migration, 6 YAML
attribution, 79 preview, 53 randomized transfer-session, 11 scheduler
reentrancy, 19 monotonic-clock, 170 generated-language, 35 suggestion,
72 etymology, 26 world-identity, 23 grouped-log, and 16 Settings-revert
examples. Two injected directory-sync cases passed. The in-flight self-test
passed 432 assertions; its 43 non-runner definitions moved unchanged and
all 28 cases retained their order. The playtest offline self-test passed.
No full CI or fresh GPU session was run; headless presentation tests are
not a claim of pixel correctness. The Settings save-failure baseline gap
was fixed later by #2271/#2202 and is not a new finding here.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. Make migration diagnostics distinguish publication from durability
- [ ] PRR-2. Bound combat-group participant retention as well as event retention

## 1. Config migration diagnostics

### PRR-1. Make migration diagnostics distinguish publication from durability

> **Captured note:** PR #2266's destination-failure warning and failure
> contract promise default fallback and a retry on the next boot. After
> #2271 introduced a post-rename durability failure, those promises are
> false for that failure phase: the complete local file already exists.

**Verification:** Complete current-code trace, supported by the existing
real-file fault-injection test. `--match "reports an unconfirmed directory
sync"` passed two examples, including the write case that returns `Left`
while retaining the complete replacement. The migration warning itself
was inspected statically, not produced by injecting a real boot failure.

**Evidence:**

- `src/Engine/Core/ConfigWrite.hs:153` — copying reads the source and delegates to the shared writer.
- `src/Engine/Core/ConfigWrite.hs:227` — after successful rename, a failed directory sync returns `Left "could not confirm durable ..."`; the published file remains.
- `test-headless/Test/Headless/Core/ConfigWrite.hs:260` — injected sync failure verifies that exact outcome and complete visible replacement.
- `src/Engine/Core/Init.hs:230` — every copy `Left` is prefixed with “the destination could not be written”, default fallback, and next-boot retry claims.
- `src/Engine/Core/Init.hs:181` — the accompanying contract also promises no local file for any failure.
- `src/Engine/Core/Init.hs:91` and `:373` — boot selects the present local file after migration, not the default.
- `src/Engine/Core/Init.hs:192` — that present file suppresses the next migration attempt.

**Handoff context:**

- **Current behavior:** A post-publication durability failure is logged with recovery instructions that contradict both this boot's selected file and the next boot's existence gate.
- **Expected behavior:** Diagnostics and comments describe what is known at the failed phase. Do not promise absence, fallback, or automatic retry after publication has already occurred.
- **Scope and constraints:** Preserve atomic publication, failure-as-unsuccessful durability reporting, source preservation, and the existing migration eligibility rules. This is a diagnostic/contract correction, not permission to roll back or delete an already published config.
- **Verification target:** Cover pre-rename and post-rename failures at the migration diagnostic boundary; check actual path selection and retry eligibility alongside the emitted warning. Keep the existing shared writer's phase tests.
- **Deduplication:** Open/closed searches for migration durability, config destination, and migration sync found #2210 and #2202 as antecedents. Neither separately records this surviving warning mismatch; local findings searches found no duplicate. #2202 explicitly defines the post-rename outcome that exposes it.
- **Remaining uncertainty:** No real disk failure was induced. The writer outcome is fault-injection evidence; the migration continuation is a static trace.

## 2. Grouped combat history

### PRR-2. Bound combat-group participant retention as well as event retention

> **Captured note:** PR #2257 bounds group count and each group's events,
> but a continuously active combat group's `participants` set still grows
> for the entire session. The bounded-history specification omitted that
> retained state and therefore does not achieve bounded grouped memory.

**Verification:** Ran the real `scripts.combat_log` module in the installed
Lua interpreter, without modifying it. Stubbed only the clock, unit-name
query, and owned event drain, following the headless fixture's approach.
Feeding 10,000 one-second-spaced events from one attacker to a fresh target
each time returned:

```text
groups=1 grouped_events=200 flat_events=200 participants=10001
```

The 23 existing grouped-log examples also passed. This is retained-identity
growth, not a measured frame-time or out-of-memory claim.

**Evidence:**

- `scripts/combat_log.lua:412` — `findBattle` reuses a recent group whenever either participant is already in its set.
- `scripts/combat_log.lua:538` — each group owns a participant table.
- `scripts/combat_log.lua:563` — every newly encountered attacker/target is added permanently to a reused group.
- `scripts/combat_log.lua:569` — overflow removes event rows only; it never removes the corresponding participant identities.
- `scripts/combat_log.lua:573` — every new event refreshes group recency, so a stream with gaps below 120 seconds keeps the group eligible indefinitely.
- `scripts/combat_log.lua:505` — group eviction runs only on new-group admission; the one-group stream never reaches it.
- `test-headless/Test/Headless/Lua/GroupedLogRetention.hs:304` — the 2,000-event case repeatedly uses the same encounter/participants and checks event rows, not growing participant state.

Run from the repository root; this feeds the actual `update`, not a copied
grouping algorithm:

```sh
lua - <<'LUA'
NOW = 0
engine = {gameTime = function() return NOW end}
unit = {getInfo = function(uid)
    return {defName='probe_unit', displayName='U'..uid}
end}
local pending = {}
combat = {drainEvents = function()
    local events = pending; pending = {}; return events
end}
local m = require('scripts.combat_log')
for j = 1, 10000 do
    NOW = j
    pending = {{kind='miss', attacker=1, target=j+1,
                ts=j, seq=j, payload={}}}
    m.update(0.1)
end
local count = 0
for _ in pairs(m.battles[1].participants) do count = count + 1 end
print('groups='..#m.battles..' grouped_events='..#m.battles[1].events
    ..' flat_events='..#m.allEvents..' participants='..count)
LUA
```

**Handoff context:**

- **Current behavior:** Capped visible event history can retain an uncapped set of identities no remaining event mentions. A common attacker keeps all successive targets attached to one recent group.
- **Expected behavior:** Grouped retention has an explicit bound that includes participant bookkeeping, with deterministic behavior when the chosen history horizon is exceeded.
- **Scope and constraints:** Combat only; injury groups have a single victim identity. Preserve the 200-event flat history and its `unitEntries` consumers. The original requirement to preserve rejoining for retained groups must be reconciled explicitly with participant retirement: blindly pruning identities changes that behavior. Do not silently choose a new grouping policy.
- **Verification target:** Add a shared-participant/changing-participant stream well beyond the event cap; assert retained identity bounds, event order, and documented rejoin behavior for identities outside the retained horizon. Keep existing group-eviction and UI-state tests.
- **Deduplication:** Open/closed searches for combat participants and participant retention found only the completed #2189 specification (and unrelated simulation coordination). Its body/reviews cover group/event caps, not participant retention. Local reports contain no matching participant-growth finding.
- **Remaining uncertainty:** The eventual retention/rejoin policy needs a decision during disposition. The unbounded growth is reproduced; its frequency and memory cost in ordinary gameplay were not measured.
