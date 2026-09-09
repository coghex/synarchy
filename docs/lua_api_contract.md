# Lua API contract and rollout verdict

This records the Lua boundary contract and the owner-run evidence for
[#2483](https://github.com/coghex/synarchy/issues/2483), using instrumentation
in [PR #2589](https://github.com/coghex/synarchy/pull/2589).
All source counts and measurements below refer to commit `9f92dc0b291784dd6404dba8a21c57e981895cb2`.
The verdict was prepared on 2026-09-09 for owner signoff before publication.

## Verdict

**Descriptor rollout: do not roll out beyond `UI`.** Keep the 82-verb `UI`
pilot. This capture establishes which verbs are busy, but it does not measure
the maintenance benefit of converting another namespace or establish a need
for a broad conversion. The existing descriptor checks provide concrete
coverage for `UI`; expansion should require a separate, specific contract need.
No additional namespace conversion is part of this decision.

**Bulk interfaces: not decidable from this profile.** `unit.getInventory`
accounts for 4,489.334 ms, 49.96% of summed recorded duration, from only 10,199
calls (0.505% of all calls). This makes inventory reads the first candidate
for a focused investigation. It does not distinguish costly inventory
construction from avoidable polling, scheduler/GC delays, or crossing overhead,
and it does not identify the Lua callers or their arguments.

The owner reported smooth play with no noticed stutters, along with gameplay
bugs to record separately. There was no combat in the capture. Prioritize
those observed bugs; these numbers alone do not justify a broad performance
project. A future inventory investigation should attribute callers and compare
the same workload before proposing a narrow query, cache, or bulk interface.

Instrumentation disposition: retain the always-on counters delivered by
#2589 for now. Their overhead has not been measured against an uninstrumented
build; retention is not a claim that they are free. Gating, removing, or
optimizing instrumentation requires a separate measured decision.

## Registered surface

The registration audit at the stated commit finds **647 registrations across
27 namespaces in 13 registrar modules**, with zero findings. This includes the
two diagnostic verbs added by #2483. The issue's older baseline of 636 verbs
predates intervening registrations; this commit had 645 before these two additions.
Counts below cover authored engine registrations, excluding stock Lua library
members retained in augmented tables.

| Namespace | Registered verbs |
|---|---:|
| `UI` | 82 |
| `blood` | 11 |
| `building` | 34 |
| `camera` | 18 |
| `chop` | 13 |
| `combat` | 3 |
| `construction` | 18 |
| `craft` | 15 |
| `debug` | 11 |
| `engine` | 112 |
| `equipment` | 8 |
| `faction` | 6 |
| `flora` | 7 |
| `infection` | 2 |
| `injury` | 2 |
| `input` | 9 |
| `item` | 19 |
| `loot` | 4 |
| `plant` | 6 |
| `power` | 9 |
| `repair` | 3 |
| `structure` | 20 |
| `substance` | 2 |
| `thought` | 2 |
| `till` | 8 |
| `unit` | 114 |
| `world` | 109 |
| **Total** | **647** |

The source of truth is the registration expressions under
[`API/Register/`](https://github.com/coghex/synarchy/blob/9f92dc0b291784dd6404dba8a21c57e981895cb2/src/Engine/Scripting/Lua/API/Register).
Reproduce the inventory with `python3 tools/lua_registration_audit.py` at the
stated revision; its `collect_registrations` result supplies each namespace's
verb set. The audit checks Lua references against installed names and checks
that each telemetry namespace matches the global table being installed.

## Descriptor convention and adding a verb

[#2479](https://github.com/coghex/synarchy/issues/2479) established a plain
Haskell `LuaVerb` record next to the installed action. See
[`Descriptor.hs`](https://github.com/coghex/synarchy/blob/9f92dc0b291784dd6404dba8a21c57e981895cb2/src/Engine/Scripting/Lua/API/Descriptor.hs) and the
[`UI` registrar](https://github.com/coghex/synarchy/blob/9f92dc0b291784dd6404dba8a21c57e981895cb2/src/Engine/Scripting/Lua/API/Register/UI.hs).
Descriptors are metadata. They do not validate arguments at runtime or change
Lua coercions, fallbacks, errors, or return values. Raw-name registration and
descriptor registration coexist and feed the same counters.

For a new `UI` verb:

1. Implement the action and establish its actual argument and result behavior.
   Register it with `registerLuaVerb callStats "UI" (luaVerb "verbName" args returns doc) action`
   in the table-installation block, preserving the literal namespace and verb
   name that the audit recognizes. Use the runtime's existing `callStats`.
2. Describe positional arguments with `argReq` or `argOpt`, documenting omitted
   and nil defaults and the implementation's coercions. Distinguish `TInteger`
   from `TNumber`, nullable values, named record fields, and arrays. Describe
   invalid-handle behavior instead of assuming validation that does not exist.
3. Use `retNone` for zero stack results. Use `retVals` with ordered `resVal`
   entries for returned values; one nullable result is different from no result,
   and multiple bare results are different from a table. Keep the descriptor in
   the list returned by `installUIAPI`, so tests inspect the installed descriptors.
4. Extend the relevant behavioral tests and the `UI descriptor` headless tests
   to compare metadata with actual results. `verbMalformations` only establishes
   structural consistency; it cannot prove that the action matches the record.
   Run the registration audit and the applicable targeted headless groups.
   Run `test_lua_registration_audit.py` when changing the audit or its grammar.

Other namespaces keep `registerLuaFunction callStats "namespace" "verbName" action`
under this verdict. Preserve the shared exception guard and instrumentation;
do not install a direct Haskell callback that bypasses the wrapper. A verb that
adds state must also follow the repository's persistence and capability rules.

## Measurement contract

`debug.getLuaCallStats()` returns an independent snapshot:

```lua
{ available = boolean, sequence = integer, verbs = {
  { id = "namespace.verb", count = integer,
     totalDurationNs = integer, maxDurationNs = integer }
} }
```

`verbs` is a dense, sorted array of called verbs only. Reads do not mutate
counters. Both diagnostic verbs are excluded from instrumentation.
`debug.resetLuaCallStats()` clears entries and advances the sequence; before
any recorded completion in the new window, `available` is false and `verbs`
is empty. Sequence starts at zero for a runtime and advances on recorded
completion/unwind and reset; reset does not put it back at zero. Calls already
in flight at reset are discarded when they finish, including failed calls;
new calls started after reset can contribute normally. Lua integer fields
saturate at the signed Lua integer limit rather than wrapping.

Both registration paths count calls completing or unwinding through the
shared wrapper, without changing Lua errors or Haskell exception/cancellation
propagation. State belongs to the Lua runtime's registration closures and is
transient, never serialized; there is no new `EngineEnv` field.

Duration is inclusive monotonic elapsed time inside the Haskell action. It can
include waits, scheduling, GC, and nested Lua callbacks; nested verb totals can
overlap. It excludes subsequent queued work and does not measure isolated
crossing overhead, exclusive CPU time, full Lua script execution, or all
instrumentation bookkeeping. Haskell-to-Lua entry points are not separately
profiled. There is no per-script, per-argument, or per-entity attribution.
These diagnostics are available through the engine TCP console; they are
withheld from the restricted in-game shell.

## Owner-run rendered capture

- Source/executable: commit `9f92dc0b291784dd6404dba8a21c57e981895cb2`, production profile, macOS ARM64.
- Platform reported by the helper: `macOS-26.6-arm64-arm-64bit-Mach-O`.
- World: `Wogyu-jywúkyjb` (Wind of Havens), seed `1334661219`, page `main_world`.
- Owner manually launched the built game with
  `cabal run exe:synarchy -- --port 9123`, loaded the world and spawned units
  near an enemy location. With the HUD visible, the owner sent units toward a
  ruin. The window ended before they arrived; **no fighting occurred**.
  Exact unit count and the full starting save were not retained.
- Reset request began at `2026-09-09T12:55:36.329909+00:00`. The helper reset counters,
  checked an empty snapshot in the same console request, waited 120 seconds
  while the owner played, then requested one final snapshot.
- Requested duration: **120 seconds**. Clock bounds on the measured window:
  **120.005071–120.022848 seconds**. Reset round trip: **4.797 ms**;
  final snapshot round trip: **12.980 ms**.
- Final snapshot: **170 called verbs**, **2,019,753 calls**,
  **8,986.083 ms summed inclusive elapsed time**. Sequence advanced from
  **272,176** to **2,291,929**, exactly matching the counted calls.

The raw activity label says `enemy-location encounter`; the owner's subsequent
clarification above is authoritative about what happened. The raw file is
preserved unchanged in the [capture evidence](lua_api_profile_20260909.md),
with its checksum and the exact capture helper.

This is one interactive exploration sample. It is not a combat benchmark,
a frame-time recording, a worst-case load test, or a deterministic replay.
The owner noticed no stutters; the capture does not independently measure that.
Unretained unit count, save state, camera/HUD interactions, and activity timing
limit repeatability and comparisons with other sessions.

### Twenty highest verbs by call count

| Verb | Calls | Total elapsed (ms) | Maximum call (ms) |
|---|---:|---:|---:|
| `unit.getStat` | 865,928 | 481.425 | 9.090 |
| `unit.setStat` | 218,169 | 720.465 | 20.062 |
| `world.getFluidAt` | 193,031 | 43.707 | 3.298 |
| `unit.getWounds` | 136,490 | 538.529 | 9.464 |
| `unit.getInfo` | 100,830 | 854.723 | 25.782 |
| `engine.gameTime` | 46,430 | 8.286 | 2.640 |
| `unit.getPose` | 45,457 | 30.504 | 3.137 |
| `unit.getActivity` | 42,098 | 12.671 | 0.102 |
| `engine.getTextWidth` | 25,140 | 100.424 | 9.698 |
| `UI.setText` | 24,927 | 56.194 | 4.230 |
| `unit.getBlood` | 20,401 | 70.528 | 9.898 |
| `unit.getCurrentAnim` | 16,515 | 4.964 | 0.278 |
| `unit.getPain` | 15,392 | 6.100 | 0.322 |
| `unit.getFaction` | 14,138 | 5.311 | 0.311 |
| `unit.getSkill` | 12,470 | 5.253 | 0.023 |
| `UI.setPosition` | 10,692 | 21.340 | 5.382 |
| `faction.areAllies` | 10,612 | 4.669 | 0.026 |
| `UI.setColor` | 10,531 | 28.329 | 7.880 |
| `unit.getInventory` | 10,199 | 4,489.334 | 259.075 |
| `equipment.getAccessories` | 10,036 | 140.068 | 16.488 |

### Twenty highest verbs by total elapsed duration

| Verb | Calls | Total elapsed (ms) | Maximum call (ms) |
|---|---:|---:|---:|
| `unit.getInventory` | 10,199 | 4,489.334 | 259.075 |
| `unit.getInfo` | 100,830 | 854.723 | 25.782 |
| `unit.setStat` | 218,169 | 720.465 | 20.062 |
| `unit.getWounds` | 136,490 | 538.529 | 9.464 |
| `unit.getStat` | 865,928 | 481.425 | 9.090 |
| `equipment.getLoadout` | 10,034 | 250.620 | 8.365 |
| `unit.getVisibleTiles` | 9,308 | 219.074 | 8.273 |
| `equipment.getAccessories` | 10,036 | 140.068 | 16.488 |
| `equipment.getClass` | 4,352 | 123.243 | 8.239 |
| `world.listPlacedLocations` | 1,333 | 117.660 | 8.365 |
| `engine.getTextWidth` | 25,140 | 100.424 | 9.698 |
| `world.getLocationAwareness` | 1,197 | 79.352 | 7.840 |
| `unit.getBlood` | 20,401 | 70.528 | 9.898 |
| `UI.setText` | 24,927 | 56.194 | 4.230 |
| `unit.getSelected` | 5,164 | 48.292 | 1.869 |
| `world.getFluidAt` | 193,031 | 43.707 | 3.298 |
| `unit.transferEndpointInfo` | 80 | 41.909 | 2.292 |
| `unit.recomputeBody` | 5,505 | 41.801 | 8.974 |
| `item.listGround` | 1,514 | 38.188 | 8.574 |
| `UI.findHoverTarget` | 1,197 | 33.789 | 0.843 |

Totals and maxima are displayed in milliseconds to three decimal places;
the evidence retains original integer nanoseconds. Each list is sorted by its
named metric descending, with verb ID as the tie-breaker.

## Reading the results

The overall rate is approximately **16,830 calls/second**. High frequency alone
is not evidence that the work is expensive: `unit.getStat` contributes 42.87%
of calls but only 5.36% of recorded duration, averaging about 0.556 microseconds
per call inside its action. Derived stat formulas intentionally reread inputs;
this profile cannot establish which repeated reads are redundant.

`unit.getInventory` averages about 440.174 microseconds per call and reaches a
259.075 ms single-call maximum. Its implementation builds item tables including
weights, contents signatures and display metadata. Some AI callers request
inventory while looking for particular consumables. These source observations
make narrower reads plausible, but the aggregate counters do not show which
callers caused the measured cost. The maximum does not establish a visible
259 ms frame stall because timing includes waiting and scheduling.

The 8.986 seconds of summed elapsed actions in a 120-second window is not a CPU
utilization measurement or a complete estimate of boundary cost. In particular,
it neither proves a performance crisis nor proves that instrumentation overhead
is negligible. The supported outcome is a retained baseline and a prioritized
investigation candidate, with bulk-interface design still undecided.

## Validation and delivery

Instrumentation validation at the stated revision passed `cabal build all`,
the headless test build, 79 targeted examples across call statistics, UI
descriptors, exception guards, faction, digging admission and map-image
admission, plus the registration, capability, persistence, Unicode and module
budget gates. A separate headless smoke verified reset/empty state and exactly
one recorded `engine.getBootProfile` call. Those checks validate the mechanism;
the tables above use only the owner's rendered capture.

The instrumentation received canonical opposite-agent approval in PR #2589.
The issue closes only after that PR merges and this contract and its evidence
land on `master` through the documentation lane. Publication records the owner's
signoff on the two verdicts above; it does not authorize implementing a batch API
or extending the descriptor rollout.
