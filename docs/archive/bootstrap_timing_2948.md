# Bootstrap timing, per-file loop against the one-call arena profile (#2948)

Evidence write-up for #2948 (report finding PRB-15). Measurement only: it
recommends nothing and changes no probe, `tools/probelib.py` or
`scripts/startup_loader.lua`. The rollout is a separate decision.

Run: `python3 tools/bootstrap_timing.py --output <report.json>` at
`e99993157d598eace8a1d5974bb98f6d2988e1f5` (PR branch `issue-2948-bootstrap-timing`),
macOS arm64, a production `exe:synarchy` built in that worktree, port 9473,
2026-11-07, with other agents' work running (load average 8 to 15). The
bootstrap-timing/v1 document is [bootstrap_timing_2948.json](bootstrap_timing_2948.json).

## Output

```
bootstrap-timing/v1  headless --arena, 3 fresh engines per path, console idle 0.3 s per round trip
engine boot (launch -> READY, polled every 0.05 s): A 2.967 0.171 0.216 | B 0.238 0.158 0.171 | median of all 0.194 s

                              path A (per-file loop)     path B (one call)
console round trips                               75                     1
load wall s (each)              25.381 25.083 25.551     2.396 2.159 2.472
load wall s (median)                          25.381                 2.396
median s / round trip                          0.308                 2.396
idle floor s (trips*idle)                     22.500                 0.300
engine-side load verbs s                       2.155                 2.086

median(A) - median(B) = +22.985 s (negative would mean B slower)
against the ~34 s per-engine figure (an estimate from PRB-15, not measured here): A is 74.7%, B is 7.0%, signed saving +67.6%
scope: this headless --arena bootstrap comparison only: B also loads vegetation, infections, recipes, loot tables, loot profiles and locations, and both paths include the console's per-round-trip idle wait

family       returned count counts        A files  A total  B files  B total
material     textures                          13      219       13      219
vegetation   textures                           -        -        5       77
substance    definitions                        4       16        4       16
infection    definitions                        -        -        1        8
recipe       definitions                        -        -        6       19
item         definitions                       39       67       39       67
equipment    classes                            1        1        1        1
building     definitions                        8        8        8        8
faction      declarations                       1        8        1        8
physiology   systems                            1       11        1       11
unit         definitions                        8        8        8        8
loot_table   tables (0 or 1 per file)           -        -        1        1
loot_profile profiles (0 or 1 per file)         -        -        1        1
location     definitions                        -        -        1        1

registry entries after the path (counts of names; materials count materials, not textures; faction, vegetation and loot_table have no list verb):
registry           A     B
substance         16    16
item              67    67
equipment          1     1
material          73    73
building           8     8
unit               8     8
physiology         6     6
infection          0     8
recipe             0    19
location           0     1
loot_profile       0     1
```

## Reading it

Path A is the eight-family loop of `cargo_capacity_probe.py` `bootstrap()`: 75
console round trips, each paying `probelib.send`'s 0.3 s idle wait, so 22.5 s is
the floor before any work is done. Path B is `build('arena')` plus `runAll()` as
one console call. The engine-side time of the per-family loader verbs, read from
the unreset `debug.getLuaCallStats` window, is about 2 s on both paths (8
`loadUnitYaml` calls are most of it), so the difference between the paths is
the round trips and their idle waits, not the engine's parse.

Median(A) minus median(B) is +22.985 s. Against the roughly 34 s per-engine
figure of `persistence_contract` and `gameplay_tag_persistence`, which is an
estimate recorded in PRB-15 (a probe total divided by its engine count), not a
newly measured end-to-end probe total, path A is about 75% (25.4 s) of a typical
probe engine and path B about 7% (2.4 s), so one-call loading would save about
23 s, about two thirds, per engine. That is this headless bootstrap comparison
only, and B is a superset: it also loads 15 more files in six more families
(vegetation, infections, recipes, loot tables, loot profiles, locations), 90
files against 75, and registers infections, recipes, locations and loot
profiles the probes' private loops never register, so a probe moved onto it
could see different state. The per-family totals count different things
(textures, declarations, systems or definitions) and are comparable only within a
family; where both paths load a family they match, and the registries
queryable on both (substances, items, equipment, materials, buildings, units,
physiology) are the same size afterwards, which shows equal counts and does not
prove the definitions are identical. The bare engine boot to `READY` is about
0.2 s here (the first engine of a fresh binary took 3.0 s, a cold start the median
absorbs), so the rest of a probe engine's 34 s beyond the bootstrap is world
setup and the probe's own work, which this tool does not measure.

Three engines per path is the issue's minimum; the three load times of each
path agree to within 0.5 s (A) and 0.3 s (B).
