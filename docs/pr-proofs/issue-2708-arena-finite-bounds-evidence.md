# #2708 — optional finite arena bounds: validation evidence

Branch `issue-2708-finite-arena`, from `master` at `6f305b1b4`. Logs live under
`~/.local/state/synarchy-evidence/2708/` (named per row). Commands run from the
issue worktree with the production profile and `cabal-3.16.1.0`.

## What was run, and at which commit

The heavy tier ran at `ee4f463a0` plus the uncommitted fixture/probe/doc edits
that became `2065034a8`. Two commits land after it. Their source delta is four
files and cannot reach generation:

| File | Change |
|---|---|
| `src/Unit/Thread/Command/MotionGuard.hs`, `Motion.hs`, `Lifecycle.hs` | an order refused at the rim logs at debug, a spawn/teleport/leap logs a warning (a `Bool` on `motionPositionsAdmitted`) |
| `src/Engine/Scripting/Lua/API/World/Lifecycle.hs` | `==` → `≡` (unicode audit) |

| # | Check | Result | Log |
|---|---|---|---|
| — | `world_check` BEFORE, clean base `6f305b1b4` | PASS 21/21 | `world_check_before.log` |
| 1 | fixture `z15-arena-finite` captured and registered | `test-headless/data/save-compat/z15-arena-finite.bin`, sha256 `dcec965d144901008dd92d686d72ebb364e685b6154a9a28c9cc9f1ec9cf4148` | `finite/` |
| 2 | `python3 tools/multiworld_save_probe.py --arena --port 9219` | PASS, 34 checks, 0 failed (expandable-arena assertions kept; finite section adds six) | `probe-arena.log` |
| 3 | migration scenario for `z15-arena-finite` and `z14-arena-legacy-shallow` (fresh process, resave, restart, reload) | both PASS | `migration-z14-z15.log` |
| 4 | `SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless` | first run 11779 examples, 4 failures (version pins, below); **final head `16d6336b8`: 11781 examples, 0 failures** | `full-suite.log`, `full-suite-final.log` |
| 5 | `world_check` AFTER | PASS 21/21, identical to BEFORE apart from timings | `world_check_after.log` |
| 6 | `world_baseline` recapture | 21 baselines, 0 failures, **no diff under `tools/baselines/`** | `world_baseline.log` |
| 7 | `world_check` final | PASS 21/21 | `world_check_final.log` |

The first full run (11779 examples) failed exactly four examples, all pinned to
world-pages version 14: the generated-identity input-version list, the registry
current version, the pinned payload length+fingerprint (twice: two tables), and
the FOURTEEN-version unsupported-version message. They were updated to v15; no
behavioural example failed.

## Finite-arena fresh-process evidence

The probe builds a 100×40 arena (X −50..49, Y −19..20), edits the four rectangle
corners, places a unit and edge water, saves, quits, restarts and loads. After
the load it asserts, from the live engine:

- the rectangle is restored with no missing tile and no terrain outside it;
- the four corner edits were replayed;
- a unit ordered off the map stays on it (position `(43.19, 20.37)`);
- the edge water is still inside (4 tiles) and none stands outside (0);
- the 12 remaining chunks load on demand, and chunks beyond the map are refused
  (queued 0, none resident);
- loading and probing left the save file's bytes unchanged.

## Gates

| Gate | Result |
|---|---|
| `arena finite bounds` (new) | 87 examples, 0 failures |
| `arena generation consistency` | 16, 0 failures |
| `sim chunk admission` | 12, 0 failures |
| `persistence contract` | 8, 0 failures |
| `save migrations` / `generated world identity` | 61 / 27, 0 failures |
| `save components` (version pins) | 177, 0 failures |
| `tools/save_compat_audit.py` | 43 baselines, 49 fixtures intact |
| `tools/test_save_compat_audit*.py`, `test_persistence_inventory_audit.py`, `test_engine_env_capability_audit.py`, `test_run_probes.py`, `test_probe_census.py`, `test_probe_flake.py` | pass |
| `persistence_inventory_audit`, `engine_env_capability_audit`, `enum_append_only_audit`, `haskell_module_budget`, `unicode_operator_audit`, `haddock_link_audit`, `cabal_module_audit`, `ci_parity_audit` | pass |

## Reproducing the fixture

`docs/pr-proofs/issue-2708-capture-finite-arena.py.txt` is the capture script
(drives a headless engine through `world.initArena("finite_world", 20, 10)`,
edits, saves). Register with
`python3 tools/save_compat_audit.py --add-baseline …`; the descriptor oracle row
is in `tools/test_save_compat_audit_codec.py`.
