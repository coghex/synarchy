#!/usr/bin/env python3
"""Attribution, calibration and projection for the CI build-drift measurement (#2746).

Reads what ``ci_build_drift.py`` collected (runs), planned and replayed
(per-step recompiled module lists) and computes, per sampled run:

* the replayed module sets of the three builds that matter: ``D`` (snapshot ->
  the PR's base: master drift), ``O`` (base -> tested tree: the PR's own diff)
  and ``U`` (snapshot -> tested tree in one build, which is what CI did);
* an attribution of ``U`` that assigns every module exactly once;
* the calibration that turns module counts into estimated seconds, and the
  projected build seconds under each policy.

Module identity is ``(component, module)``: the same module name in the
library and in a test suite is two different compilations.

Usage:
  python3 tools/ci_build_drift_report.py --runs runs.json --plan plan.json \\
      --replay replay.json [--lanes runs_lanes.json] --out report.json
  python3 tools/ci_build_drift_report.py --self-test
"""

from __future__ import annotations

import argparse
import json
import math
import random
import sys
from pathlib import Path

LIB_EXE = ("lib", "synarchy", "synarchy-save-codec")
TESTS = ("synarchy-test-headless",)
GROUPS = {"lib_exe": LIB_EXE, "tests": TESTS}
TOLERANCE = 0.05


def module_set(result: dict | None, group: str) -> frozenset:
    """Modules ``result`` recompiled in ``group``; an absent step recompiled none."""
    if result is None:
        return frozenset()
    mods = result["modules"]
    return frozenset((c, m) for c in GROUPS[group] for m in mods.get(c, []))


def attribute(drift: frozenset, own: frozenset, unsplit: frozenset) -> dict:
    """Assign each module of the unsplit build to exactly one cause.

    Convention (reproducible, stated in the document): a module the
    snapshot->base replay recompiled is *drift*, because master alone would
    have forced it whatever the PR contained; a module only the base->tested
    replay recompiled is *own*; a module in neither but recompiled by the
    unsplit build is *residual* (an ABI interaction no single step shows).
    Sequential replay can compile one module twice (*overlap*) or compile one
    the unsplit build never needs (*cancelled*, e.g. an interface that changed
    and changed back); neither is double counted in the attribution.
    """
    union = drift | own
    n_unsplit = len(unsplit)
    return {
        "drift_replay": len(drift),
        "own_replay": len(own),
        "overlap": len(drift & own),
        "union": len(union),
        "unsplit": n_unsplit,
        "cancelled": len(union - unsplit),
        "drift": len(unsplit & drift),
        "own": len((unsplit - drift) & own),
        "residual": len(unsplit - union),
        # the same unsplit build under the opposite convention (own claims the overlap),
        # and with the overlap split evenly: the range the convention can move a share
        "drift_own_first": len(unsplit - own),
        "own_own_first": len(unsplit & own),
        "drift_split": len((unsplit & drift) - own) + len(unsplit & drift & own) / 2,
        "own_split": len((unsplit & own) - drift) + len(unsplit & drift & own) / 2,
        "union_error": (len(union) - n_unsplit) / n_unsplit if n_unsplit else 0.0,
    }


def ols(points: list[tuple[float, float]]) -> dict:
    """Least-squares line y = a + b*x with its R^2 (no external dependencies)."""
    n = len(points)
    mx = sum(x for x, _ in points) / n
    my = sum(y for _, y in points) / n
    sxx = sum((x - mx) ** 2 for x, _ in points)
    sxy = sum((x - mx) * (y - my) for x, y in points)
    b = sxy / sxx if sxx else 0.0
    a = my - b * mx
    ss_res = sum((y - (a + b * x)) ** 2 for x, y in points)
    ss_tot = sum((y - my) ** 2 for _, y in points)
    return {"intercept": a, "slope": b, "r2": 1 - ss_res / ss_tot if ss_tot else 1.0,
            "n": n, "rmse": math.sqrt(ss_res / n)}


def percentile(values: list[float], q: float) -> float:
    """Linear-interpolation percentile (the 'type 7' estimator NumPy defaults to)."""
    ordered = sorted(values)
    if not ordered:
        raise ValueError("percentile of an empty sample")
    pos = (len(ordered) - 1) * q
    lo, hi = math.floor(pos), math.ceil(pos)
    return ordered[lo] + (ordered[hi] - ordered[lo]) * (pos - lo)


def bootstrap(values: list[float], q: float, draws: int = 4000, seed: int = 2746) -> tuple[float, float]:
    """Central 90 % interval of the q-percentile under resampling of the runs."""
    rng = random.Random(seed)
    stats = sorted(percentile([rng.choice(values) for _ in values], q) for _ in range(draws))
    return stats[int(0.05 * draws)], stats[int(0.95 * draws) - 1]


def estimate(fit: dict, count: float) -> float:
    return max(0.0, fit["intercept"] + fit["slope"] * count)


def step_counts(replay: dict, step_id: str, group: str) -> frozenset | None:
    return module_set(replay[step_id], group) if step_id in replay else None


def find_policy_build(replay: dict, run_id: int, snap: int) -> dict | None:
    """The replayed build from snapshot ``snap`` to the run's tested tree (any planner's id)."""
    for key, result in replay.items():
        parts = key.split(":")
        if parts[0] in ("pol", "alt2", "alt4", "lag") and parts[1:] == [str(run_id), str(snap)]:
            return result
    return None


def policy_sets(plan_run: dict, replay: dict, group: str) -> dict:
    """Modules recompiled per policy for one run, from the replayed builds.

    ``size{N}`` is the EPOCH_SIZE=N policy against the writers' real outcomes
    (``size8`` is today's, and equals ``unsplit`` when the planner's rule
    reproduces the restored snapshot); ``own`` is the ideal every-push policy
    whose snapshot is the run's base, whatever the writers had finished.  A
    policy whose snapshot is the restored one reuses ``unsplit`` and one whose
    snapshot is the base reuses ``own``; any other needs its own replay and a
    missing one is an error rather than a guess.
    """
    rid, sigma, base = plan_run["run_id"], plan_run["snapshot"], plan_run["base_count"]
    unsplit = module_set(replay[f"unsplit:{rid}"], group)
    drift = module_set(replay.get(f"drift:{sigma}:{base}"), group) if base != sigma else frozenset()
    own = module_set(replay.get(f"own:{rid}"), group) if base != sigma else unsplit
    out = {"unsplit": unsplit, "drift": drift, "own": own}
    for size, snap in plan_run["policy_snapshot"].items():
        if snap is None:
            out[f"size{size}"] = None
        elif snap == sigma:
            out[f"size{size}"] = unsplit
        elif snap == base:
            out[f"size{size}"] = own
        else:
            found = find_policy_build(replay, rid, snap)
            if found is None:
                raise KeyError(f"run {rid}: no replayed build from snapshot {snap}; re-plan with --resume")
            out[f"size{size}"] = module_set(found, group)
    return out


def per_run_rows(runs: list[dict], plan: dict, replay: dict) -> list[dict]:
    by_id = {r["run_id"]: r for r in runs}
    rows = []
    for plan_run in plan["runs"]:
        rid = plan_run["run_id"]
        if f"unsplit:{rid}" not in replay:
            continue
        rec = by_id[rid]
        row = {"run_id": rid, "snapshot": plan_run["snapshot"], "base": plan_run["base_count"],
               "position": rec["epoch"]["position"], "outcome": rec["dist"]["outcome"],
               "lib_exe_seconds": rec["lib_exe_seconds"], "test_suite_seconds": rec["test_suite_seconds"]}
        for group in GROUPS:
            sets = policy_sets(plan_run, replay, group)
            row[group] = {
                "attribution": attribute(sets["drift"], sets["own"], sets["unsplit"]),
                "policy": {k: (len(v) if v is not None else None) for k, v in sets.items()},
            }
        rows.append(row)
    return rows


def calibrate(rows: list[dict]) -> dict:
    """Seconds = intercept + slope * recompiled modules, per build step."""
    fits = {}
    for group, key in (("lib_exe", "lib_exe_seconds"), ("tests", "test_suite_seconds")):
        points = [(r[group]["attribution"]["unsplit"], r[key]) for r in rows if r[key] is not None]
        fits[group] = ols(points)
    return fits


def project(rows: list[dict], fits: dict, policies: dict[str, tuple[str, float]]) -> dict:
    """Estimated build seconds per run and policy; summary percentiles over the runs.

    ``policies`` maps a name to (module-count policy key, test-suite compile
    multiplier).  The multiplier scales only the slope of the test-suite fit:
    optimisation level changes what a recompiled module costs, not how many
    modules recompile or what the link and cabal overhead are.
    """
    out = {}
    for name, (key, tests_scale) in policies.items():
        per_run = []
        for r in rows:
            counts = {g: r[g]["policy"].get(key) for g in GROUPS}
            if any(v is None for v in counts.values()):
                continue
            lib = estimate(fits["lib_exe"], counts["lib_exe"])
            tests = estimate({**fits["tests"], "slope": fits["tests"]["slope"] * tests_scale},
                             counts["tests"])
            per_run.append({"run_id": r["run_id"], "lib_exe": lib, "tests": tests, "total": lib + tests})
        totals = [x["total"] for x in per_run]
        out[name] = {"per_run": per_run, "n": len(totals),
                     "median": percentile(totals, 0.5), "p95": percentile(totals, 0.95),
                     "median_ci90": bootstrap(totals, 0.5), "p95_ci90": bootstrap(totals, 0.95)}
    return out


LANES = ("rest", "world")
LANE_JOB = "headless-lanes ({})"
LANE_BUILD = "Build headless test suite"
LANE_RUN = "Headless lane"
AUDIT_LIB, AUDIT_TESTS = "Build (library + executable)", "Build test suites"
PROBE_BUILD = "Build behavior probe prerequisites"


def _median(values: list[float]) -> float:
    return percentile(values, 0.5)


def lane_stats(lane_runs: list[dict]) -> dict:
    """Measured structure of the post-#2745 pull-request runs: what a lane job costs besides its build.

    ``lane_scale`` is lane build seconds over the audit job's library+suite
    build seconds in the same run (same cache, same change): a lane builds the
    library and the suite but no executable.
    """
    runs = [r for r in lane_runs if r["event"] == "pull_request" and LANE_JOB.format("rest") in r["jobs"]]
    out: dict = {"n": len(runs), "wall": [r["wall_seconds"] for r in runs]}
    scale, fixed = [], []
    for r in runs:
        audit = r["jobs"]["test-and-audits"]
        lib, tests = audit["steps"].get(AUDIT_LIB), audit["steps"].get(AUDIT_TESTS)
        if lib is not None and tests is not None and audit["seconds"]:
            fixed.append(audit["seconds"] - lib - tests)
        for lane in LANES:
            job = r["jobs"][LANE_JOB.format(lane)]
            build = job["steps"].get(LANE_BUILD)
            if build and lib is not None and tests is not None and lib + tests > 200:
                scale.append(build / (lib + tests))
    out["lane_scale"] = _median(scale)
    out["audit_fixed"] = _median(fixed)
    for lane in LANES:
        jobs = [r["jobs"][LANE_JOB.format(lane)] for r in runs]
        build = [j["steps"].get(LANE_BUILD) for j in jobs]
        run = [j["steps"].get(LANE_RUN) for j in jobs]
        out[lane] = {
            "overhead": _median([j["seconds"] - b - u for j, b, u in zip(jobs, build, run)
                                 if b is not None and u is not None and j["seconds"]]),
            "run": {"p25": percentile([u for u in run if u], 0.25), "p50": _median([u for u in run if u]),
                    "p75": percentile([u for u in run if u], 0.75)}}
    return out


def whole_run(rows: list[dict], runs: list[dict], fits: dict, lanes: dict, policy: str,
              tests_scale: float = 1.0, run_scale: float | dict = 1.0, run_quantile: str = "p50") -> list[dict]:
    """Estimated wall seconds of each sampled run's pipeline under a policy.

    Wall time is the resolve-image jobs plus the longest remaining job (the
    jobs run in parallel), not a sum of builds or a difference of medians:
    each lane job is its fixed cost plus its build plus its Hspec run; the
    audit job is its fixed cost plus the library and suite builds; the probe
    job's prerequisite build scales with the library estimate; static audits
    keep the run's own measured time.
    """
    by_id = {r["run_id"]: r for r in runs}
    out = []
    for row in rows:
        rec = by_id[row["run_id"]]
        jobs = rec["jobs"]
        counts = {g: row[g]["policy"].get(policy) for g in GROUPS}
        base_counts = {g: row[g]["policy"]["size8"] for g in GROUPS}
        if any(v is None for v in counts.values()):
            continue
        lib = estimate(fits["lib_exe"], counts["lib_exe"])
        tests = estimate({**fits["tests"], "slope": fits["tests"]["slope"] * tests_scale}, counts["tests"])
        lib_base = estimate(fits["lib_exe"], base_counts["lib_exe"])
        build = lib + tests
        # a measured run-time *gain* is within the run-to-run noise of identical builds
        # (see the document), so it is never credited: each lane's ratio is floored at 1
        ratio = {lane: max(1.0, run_scale[lane] if isinstance(run_scale, dict) else run_scale) for lane in LANES}
        times = {f"lane {lane}": lanes[lane]["overhead"] + lanes["lane_scale"] * build
                 + lanes[lane]["run"][run_quantile] * ratio[lane] for lane in LANES}
        times["audits"] = lanes["audit_fixed"] + build
        beh = jobs["behavior-probes"]
        prereq = beh["steps"].get(PROBE_BUILD) or 0
        times["probes"] = beh["seconds"] - prereq + prereq * (lib / lib_base if lib_base else 1.0)
        times["static"] = jobs["static-audits"]["seconds"]
        resolve = max(jobs.get("resolve-image / resolve", {"seconds": 0})["seconds"] or 0,
                      jobs.get("resolve-image / tag", {"seconds": 0})["seconds"] or 0)
        critical = max(times, key=times.get)
        out.append({"run_id": row["run_id"], "wall": resolve + times[critical] + 5,
                    "critical": critical, "build": build})
    return out


def self_test() -> int:
    d = frozenset({("lib", "A"), ("lib", "B")})
    o = frozenset({("lib", "B"), ("lib", "C")})
    u = frozenset({("lib", "A"), ("lib", "B"), ("lib", "C"), ("lib", "D")})
    a = attribute(d, o, u)
    assert (a["drift"], a["own"], a["residual"], a["overlap"], a["cancelled"]) == (2, 1, 1, 1, 0), a
    assert a["drift"] + a["own"] + a["residual"] == a["unsplit"]
    assert a["drift_split"] + a["own_split"] + a["residual"] == a["unsplit"]
    assert (a["drift_own_first"], a["own_own_first"]) == (2, 2)
    cancelled = attribute(frozenset({("lib", "A")}), frozenset(), frozenset())
    assert cancelled["cancelled"] == 1 and cancelled["unsplit"] == 0 and cancelled["union_error"] == 0.0

    fit = ols([(0, 10), (10, 30), (20, 50), (30, 70)])
    assert abs(fit["intercept"] - 10) < 1e-9 and abs(fit["slope"] - 2) < 1e-9 and fit["r2"] > 0.999999
    assert estimate({"intercept": -5.0, "slope": 1.0}, 2) == 0.0

    data = [1, 2, 3, 4, 5]
    assert percentile(data, 0.5) == 3 and percentile(data, 1.0) == 5
    assert abs(percentile(data, 0.95) - 4.8) < 1e-9
    lo, hi = bootstrap(data, 0.5)
    assert 1 <= lo <= 3 <= hi <= 5

    res = {"modules": {"lib": ["A"], "synarchy-test-headless": ["T"]}}
    assert module_set(res, "lib_exe") == {("lib", "A")} and module_set(None, "tests") == frozenset()
    print("ci_build_drift_report self-test: ok")
    return 0


def fmt_s(seconds: float) -> str:
    seconds = round(seconds)
    return f"{seconds // 60}m{seconds % 60:02d}s"


def render(runs: list[dict], plan: dict, rows: list[dict], fits: dict, lanes: dict,
           opt: dict | None) -> str:
    """Markdown tables for the document; every number comes from the inputs."""
    by_id = {r["run_id"]: r for r in runs}
    out: list[str] = []
    w = out.append
    w("### Sampled runs\n")
    w("| Run | PR | Hit | Epoch pos. | Snapshot | Base | Attempt | lib+exe s | suites s |")
    w("|---|---:|---|---:|---:|---:|---:|---:|---:|")
    for p, row in zip(plan["runs"], rows):
        rec = by_id[p["run_id"]]
        w(f"| {p['run_id']} | #{rec['pr_number']} | {rec['dist']['outcome'].replace('_HIT', '').lower()} "
          f"| {rec['epoch']['position']}/8 | {p['snapshot']} (`{p['snapshot_sha'][:9]}`) "
          f"| {p['base_count']} (`{p['base'][:9]}`) | {rec['attempt']} "
          f"| {row['lib_exe_seconds']} | {row['test_suite_seconds']} |")
    for group, title in (("lib_exe", "Library and executables"), ("tests", "Headless test suite")):
        w(f"\n### Recompiled modules: {title}\n")
        w("| Run | D drift replay | O own replay | overlap | D∪O | U unsplit | cancelled | residual | (D∪O−U)/U |")
        w("|---|---:|---:|---:|---:|---:|---:|---:|---:|")
        for row in rows:
            a = row[group]["attribution"]
            w(f"| {row['run_id']} | {a['drift_replay']} | {a['own_replay']} | {a['overlap']} | {a['union']} "
              f"| {a['unsplit']} | {a['cancelled']} | {a['residual']} | {100 * a['union_error']:.1f}% |")
    w("\n### Attribution of the unsplit build (modules, library+executables / suites)\n")
    w("| Run | drift-first drift | drift-first own | own-first drift | split drift | residual |")
    w("|---|---:|---:|---:|---:|---:|")
    for row in rows:
        a, b = row["lib_exe"]["attribution"], row["tests"]["attribution"]
        w(f"| {row['run_id']} | {a['drift']} / {b['drift']} | {a['own']} / {b['own']} "
          f"| {a['drift_own_first']} / {b['drift_own_first']} | {a['drift_split']:g} / {b['drift_split']:g} "
          f"| {a['residual']} / {b['residual']} |")
    w("\n### Calibration (seconds = intercept + slope x recompiled modules)\n")
    w("| Step | intercept s | slope s/module | R^2 | RMSE s | n |")
    w("|---|---:|---:|---:|---:|---:|")
    for group, name in (("lib_exe", "Build (library + executable)"), ("tests", "Build test suites")):
        f = fits[group]
        w(f"| {name} | {f['intercept']:.1f} | {f['slope']:.3f} | {f['r2']:.3f} | {f['rmse']:.1f} | {f['n']} |")
    w("\n### Estimated seconds by cause (drift-first convention, calibration above)\n")
    w("| Run | observed | fixed | drift | own | residual |")
    w("|---|---:|---:|---:|---:|---:|")
    for row in rows:
        fixed = fits["lib_exe"]["intercept"] + fits["tests"]["intercept"]
        drift = own = resid = 0.0
        for group in GROUPS:
            slope, a = fits[group]["slope"], row[group]["attribution"]
            drift += slope * a["drift"]
            own += slope * a["own"]
            resid += slope * a["residual"]
        observed = (row["lib_exe_seconds"] or 0) + (row["test_suite_seconds"] or 0)
        w(f"| {row['run_id']} | {observed} | {fixed:.0f} | {drift:.0f} | {own:.0f} | {resid:.0f} |")
    scale = (opt or {})
    policies = {"today (size 8)": ("size8", 1.0), "size 4": ("size4", 1.0), "size 2": ("size2", 1.0),
                "size 1": ("size1", 1.0), "ideal every push": ("own", 1.0)}
    for level in ("O1", "O0"):
        if level in scale:
            policies[f"suites -{level}"] = ("size8", scale[level]["compile_cpu_ratio"])
    for level in ("O1", "O0"):
        if level in scale:
            policies[f"size 1 + suites -{level}"] = ("size1", scale[level]["compile_cpu_ratio"])
    proj = project(rows, fits, policies)
    w("\n### Projected build seconds per run (library+exe step plus suites step)\n")
    w("| Policy | median | 90% CI | p95 | 90% CI |")
    w("|---|---:|---|---:|---|")
    for name, v in proj.items():
        w(f"| {name} | {fmt_s(v['median'])} | {fmt_s(v['median_ci90'][0])}-{fmt_s(v['median_ci90'][1])} "
          f"| {fmt_s(v['p95'])} | {fmt_s(v['p95_ci90'][0])}-{fmt_s(v['p95_ci90'][1])} |")
    w("\n### Projected whole-run wall time (lane jobs of #2745)\n")
    w("| Policy | lane run | median | p95 | critical job |")
    w("|---|---|---:|---:|---|")
    cases = [(n, k, 1.0, 1.0, q) for n, (k, _) in policies.items() for q in ("p50",)]
    for name, (key, tscale) in policies.items():
        run_scale = 1.0
        if "suites -" in name:
            run_scale = scale[name.rsplit("-", 1)[1]]["run_ratio_by_lane"]
        for quantile in ("p25", "p50", "p75"):
            res = whole_run(rows, runs, fits, lanes, key, tscale, run_scale, quantile)
            walls = [x["wall"] for x in res]
            crit = max(set(x["critical"] for x in res), key=[x["critical"] for x in res].count)
            w(f"| {name} | {quantile} | {fmt_s(percentile(walls, 0.5))} | {fmt_s(percentile(walls, 0.95))} | {crit} |")
    return "\n".join(out) + "\n"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--runs", type=Path)
    parser.add_argument("--plan", type=Path)
    parser.add_argument("--replay", type=Path)
    parser.add_argument("--lanes", type=Path, help="post-#2745 run records (collect --jobs-only)")
    parser.add_argument("--opt", type=Path, help="JSON of measured -O1/-O0 compile and run ratios")
    parser.add_argument("--out", type=Path, help="write the markdown tables here")
    args = parser.parse_args(argv)
    if args.self_test:
        return self_test()
    if not (args.runs and args.plan and args.replay and args.lanes):
        parser.error("--runs, --plan, --replay and --lanes are required")
    runs = json.loads(args.runs.read_text())
    plan = json.loads(args.plan.read_text())
    replay = json.loads(args.replay.read_text())
    rows = per_run_rows(runs, plan, replay)
    fits = calibrate(rows)
    lanes = lane_stats(json.loads(args.lanes.read_text()))
    opt = json.loads(args.opt.read_text()) if args.opt else None
    text = render(runs, plan, rows, fits, lanes, opt)
    (args.out.write_text(text) if args.out else print(text))
    return 0


if __name__ == "__main__":
    sys.exit(main())
