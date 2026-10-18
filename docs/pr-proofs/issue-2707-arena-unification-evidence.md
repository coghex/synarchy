# Evidence for #2707 (SCN-07): unified arena generation

Branch `issue-2707-arena-unify`, base `5450be9f5` (master before this change). Arena
evidence is recorded here separately because the ordinary generated-world baseline
corpus (`tools/baselines/`) contains no arena and therefore proves nothing about arena
correctness (issue requirement 10).

## Arena-specific evidence

1. **Production-path consistency** — hspec group `arena generation consistency`
   (16 examples): startup, camera, queued demand (beyond the footprint, at negative and
   far non-wrapping coordinates), overlapping and repeated demand, eviction and
   readmission with and without edits, staged restore, and a retained legacy save; all
   compared against an independent description of the sixteen-cell base, and against
   each other.
2. **Real fresh-process restoration** — `python3 tools/multiworld_save_probe.py --arena --port 9219`:

```text
  [PASS] main_world display name stored trimmed ({'name': 'Aldermoor Deep', 'gloss': 'the deep home'})
  [PASS] main_world gloss stored ({'name': 'Aldermoor Deep', 'gloss': 'the deep home'})
  [PASS] arena second_world is unnamed (getIdentity nil)
  [PASS] getIdentity of a missing page is nil
saved -> saves/mw_probe_f5668f8beddb/world.synworld (5089376 bytes)
  [PASS] main_world unit #1 survived the load
  [PASS] second_world unit #2 survived the load
  [PASS] main_world is the active page after load
  [PASS] main_world unit #1 is on main_world ([1])
  [PASS] second_world unit #2 is NOT on main_world ([1])
  [PASS] main_world building #1 is on main_world ([1])
  [PASS] second_world restored and can be shown
  [PASS] second_world unit #2 is on second_world ([2])
  [PASS] main_world unit #1 is NOT on second_world ([2])
  [PASS] second_world building #2 is on second_world ([2])
  [PASS] arena edit at (6,6) replayed on load (z=1, expected 1)
  [PASS] arena base vegetation is fully readable after load (256 values)
  [PASS] arena base vegetation survives the save/load round trip unchanged (0/256 tiles differ)
  [PASS] expanded arena chunks load after the fresh-process load (256 values)
  [PASS] expanded arena base vegetation matches the saved session's (0/256 tiles differ)
  [PASS] edit on expanded terrain at (56,8) replayed after expansion (z=1, expected 1)
  [PASS] loading and expanding left the save file's bytes unchanged
  [PASS] primary identity followed its page to main_world ({'gloss': 'the deep home', 'name': 'Aldermoor Deep'})
  [PASS] arena second_world restored unnamed
  [PASS] listSaves has an entry for slot 'mw_probe_f5668f8beddb'
  [PASS] listSaves worldName is the display name ('Aldermoor Deep')
  [PASS] listSaves worldGloss is the gloss ('the deep home')
  [PASS] save-slot name and world name are distinct
  [PASS] 4-argument world.init page is unnamed (getIdentity nil)
PASS: 0 check(s) failed
```

3. **Legacy save adoption** — fixture `z14-arena-legacy-shallow`, captured on the base
   build (reproduction below). The group asserts every chunk is rebuilt on the uniform
   foundation with both persisted edits replayed and the file bytes untouched; the
   save-compat migration scenario proves it loads, re-saves in current format, reloads
   in a fresh process and keeps its canonical state:

```text

=== z14-arena-legacy-shallow / z14-arena-legacy-shallow (expected active page: legacy_arena) ===
placed 177403-byte fixture at /var/folders/xs/kyf0vrg92c340wk3jncyp1fr0000gn/T/save_compat_migration_probe_hdmjbo9i/root/saves/probe_z14-arena-legacy-shallow_legacy/world.synworld
  [PASS] engine.loadSave('probe_z14-arena-legacy-shallow_legacy') accepted the fixture (got 'true')
  [PASS] fixture's load transaction published through the normal #763 staging/publish path
  [PASS] session's active page is 'legacy_arena' (got 'legacy_arena')
  [PASS] session begins paused, same as any loaded session
  [PASS] no gameplay time advanced during a 2s paused dwell ({"day":1,"year":1,"absoluteDay":0,"dayOfYear":0,"month":1} -> {"day":1,"year":1,"absoluteDay":0,"dayOfYear":0,"month":1})
  [PASS] re-saving the session under a new slot succeeded (got 'true')
  [PASS] re-saved current-format file appeared at /var/folders/xs/kyf0vrg92c340wk3jncyp1fr0000gn/T/save_compat_migration_probe_hdmjbo9i/root/saves/probe_z14-arena-legacy-shallow_resaved/world.synworld
  [PASS] re-saved file is a real current-format re-encode, not a copy of the input fixture's bytes
  [PASS] after the initial migration-load: real dumped Haskell state matches the fixture's declared canonical summary
  [PASS] fresh engine B has no pre-load active world (got 'null')
  [PASS] engine.loadSave('probe_z14-arena-legacy-shallow_resaved') accepted the re-saved current-format file (got 'true')
  [PASS] re-saved file's load transaction published
  [PASS] active page survived the restart -> reload round trip (got 'legacy_arena', expected 'legacy_arena')
  [PASS] reloaded session begins paused
  [PASS] re-saving the reloaded session under a new slot succeeded (got 'true')
  [PASS] reloaded session's re-saved file appeared at /var/folders/xs/kyf0vrg92c340wk3jncyp1fr0000gn/T/save_compat_migration_probe_hdmjbo9i/root/saves/probe_z14-arena-legacy-shallow_resaved_b/world.synworld
  [PASS] after the resave/restart/reload round trip: real dumped Haskell state matches the fixture's declared canonical summary
  [PASS] unpausing the reloaded session uses the default time scale (got 1.0)
PASS 0 failed; 0 skipped
```

### Reproducing the legacy fixture

```sh
git worktree add ../issue-2707-base 5450be9f5      # master before #2707
cd ../issue-2707-base && cabal build all
python3 capture_legacy_arena.py --port 9291 --out /tmp/legacy_arena.synworld
```

`capture_legacy_arena.py` is retained as `docs/pr-proofs/issue-2707-capture-legacy-arena.py.txt`.
The result was then normalised and registered with
`synarchy-save-codec` and `python3 tools/save_compat_audit.py --add-baseline …` (see the
manifest provenance of `z14-arena-legacy-shallow`). The migration scenario for that
fixture alone: `save_compat_migration_probe.declared_complete_session_fixtures()`
filtered to its id, then `run_one_fixture(checks, 9276, fixture)`.

## Ordinary generated worlds are unchanged

- `python3 tools/world_check.py` on the base build (BEFORE): `Summary: PASS=21 (total: 21)`
- the same on this branch (AFTER): `Summary: PASS=21 (total: 21)`; the two
  outputs are line-for-line identical apart from timings.
- `python3 tools/world_baseline.py` recapture: `Captured 21 baselines, 0 failures`;
  **zero diff** under `tools/baselines/` (byte-identical), so nothing is committed there.
- `python3 tools/world_check.py` after the recapture: `Summary: PASS=21 (total: 21)`.

## Full tier

`SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless`: **11694 examples, 0 failures**.
