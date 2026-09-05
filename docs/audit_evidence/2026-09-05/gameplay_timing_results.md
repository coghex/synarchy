# Controlled gameplay timing results

Source revision: `2922bb476be795c9fd3d33eb65962b7eccca39ed`, 2026-09-05. The primary checkout remained clean. These are direct production-function executions with controlled inputs, not live load tests.

## Calendar and elapsed cap

Executed `cabal repl lib:synarchy --repl-options=-v0` in the reviewed checkout, feeding [gameplay_timing.ghci](gameplay_timing.ghci) on stdin. The retained file contains the same executable expressions as the successful temporary script; comments identify the scope.

```text
("default_240_quarter_second_ticks",(WorldTime {wtHour = 10, wtMinute = 0},WorldDate {wdYear = 1, wdMonth = 1, wdDay = 1},0))
("same_60_seconds_one_clock_call",(WorldTime {wtHour = 11, wtMinute = 0},WorldDate {wdYear = 1, wdMonth = 1, wdDay = 1},0))
("scale60_600_ticks_of_0_1",(WorldTime {wtHour = 22, wtMinute = 0},WorldDate {wdYear = 1, wdMonth = 1, wdDay = 3},0))
("scale60_480_ticks_of_0_125",(WorldTime {wtHour = 18, wtMinute = 0},WorldDate {wdYear = 1, wdMonth = 1, wdDay = 3},0))
("independent_cap_4_short_vs_1_long",1.0,0.25)
```

The trailing tuple integer is only the final call's rolled-day count; accumulated date is in `WorldDate`. The single 60-second call demonstrates the pure calendar function's partition difference; it is not an elapsed value the normal capped world worker admits. The cap comparison proves the production helper's arithmetic, not a concurrently scheduled worker experiment.

## Lua resource and eligible-work timing

Executed `lua <docs-worktree>/docs/audit_evidence/2026-09-05/gameplay_timing.lua /Users/vincentcoghlan/work/synarchy` using [the retained script](gameplay_timing.lua):

```text
same 1s observation: ten callbacks=6.50; two callbacks=6.10
production workInterval: 3s gap=3.0 credited; 6s gap=0.0 credited
Controlled timing assertions passed; no concurrent stat mutation was injected.
```

The one-second window is a stipulated scheduling comparison. The script executes the shipped resource calculation/configuration with engine API stubs, and the shipped work-interval policy; it does not execute `runDueScripts`. Production scheduler source separately establishes that a late callback still receives its configured interval.

## Movement partitioning

Executed `cabal repl lib:synarchy --repl-options=-v0` in the reviewed checkout with [movement_timing.ghci](movement_timing.ghci) on stdin:

```text
4 x 0.25: x=1.25 path=[(1.5,0.5),(2.5,0.5)] target=Just (MoveTarget {mtTargetX = 2.5, mtTargetY = 0.5, mtSpeed = 1.0, mtHazard = FallPermitted})
10 x 0.10: x=1.4000002 path=[(1.5,0.5),(2.5,0.5)] target=Just (MoveTarget {mtTargetX = 2.5, mtTargetY = 0.5, mtSpeed = 1.0, mtHazard = FallPermitted})
20 x 0.05: x=1.4499997 path=[(1.5,0.5),(2.5,0.5)] target=Just (MoveTarget {mtTargetX = 2.5, mtTargetY = 0.5, mtSpeed = 1.0, mtHazard = FallPermitted})
```

Start is `(0.4, 0.5)` with a supplied tile-center path. All schedules total one second at speed 1 tile/second. No terrain or route generation participates; `FallPermitted` excludes the protected-step ceiling. This isolates actual `tickUnit` waypoint and arrival-tolerance behavior. Ordinary play frequency remains unmeasured.

## Source-derived examples, not executed experiments

The report's combat example assumes 50 ms of work per combat pass plus its fixed sleep; the fluid example assumes 50 ms of work plus its 100 ms sleep. Their rates follow from production loop structure. No artificial workload was injected into either live worker, and no end-to-end conservation or gameplay-rate claim is made from those examples alone.

No full suite, full CI, graphical engine, world generation, or broad behavior-probe sweep was run for this documentation audit.
