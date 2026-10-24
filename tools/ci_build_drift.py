#!/usr/bin/env python3
"""Measure how much of a pull request's CI build replays earlier master changes (#2746).

The project cache holds one immutable ``dist-newstyle`` snapshot per
(image, toolchain, plan, epoch) key (``ci_cache_epoch.py``).  A pull request
restores a snapshot and recompiles everything that differs from it: the master
changes between the snapshot and the PR's base ("drift") plus the PR's own
diff.  CI builds run with ``-v0``, so the logs carry seconds but no module
lists; this tool recovers the module lists by replaying the builds locally and
joins them to the seconds the runs recorded.

Stages (each reads and writes plain JSON, so a stage can be re-run alone):

  collect   Read CI runs and their ``test-and-audits`` logs through ``gh``:
            restored key, epoch position, tested merge, step seconds, and
            which master push wrote each snapshot.
  plan      Choose the sample and derive, per run, the snapshot commit CI
            actually restored, the PR's base, and its tested tree.
  replay    Build the snapshot, base and tested trees in one replay worktree,
            counting the modules each build recompiled per Cabal component.
  report    Attribute seconds, fit the calibration, and project policies.

Nothing here changes CI behaviour: it only reads GitHub and builds in a
worktree the caller names.  See ``docs/ci_build_drift_measurement.md``.

Usage:
  python3 tools/ci_build_drift.py collect --since 2026-09-24 --until 2026-10-01 --out runs.json
  python3 tools/ci_build_drift.py plan --runs runs.json --out plan.json
  python3 tools/ci_build_drift.py replay --plan plan.json --worktree WT --state-dir DIR --out replay.json
  python3 tools/ci_build_drift.py --self-test
"""

from __future__ import annotations

import argparse
import contextlib
import datetime as dt
import io
import json
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO_ROOT / "tools"))

import ci_cache_epoch as epoch_mod  # noqa: E402

DEFAULT_REPO = "coghex/synarchy"
JOB_NAME = "test-and-audits"
STEP_LIB = "Build (library + executable)"
STEP_TESTS = "Build test suites"
STEP_SAVE = "Save project build cache (dist-newstyle)"
CABAL = "cabal-3.16.1.0"
# The two build invocations CI makes for a pull request that touches no
# graphics-facing path; the graphical suite is skipped in every sampled run.
BUILD_COMMANDS: tuple[tuple[str, ...], ...] = (
    ("build", "all"),
    ("build", "synarchy-test-headless"),
)

EPOCH_RE = re.compile(
    r"CI_CACHE_EPOCH epoch=(\d+) relevant_count=(\d+) position=(\d+)/(\d+) "
    r"next_refresh_in=(\d+) anchor=(\w+) ref=(\w+) fallback=(\S+)")
PR_REF_RE = re.compile(r"git checkout --progress --force refs/remotes/pull/(\d+)/merge")
MERGE_RE = re.compile(r"HEAD is now at (\w+) Merge (\w+) into (\w+)")
REPORT_RE = re.compile(
    r"CI_CACHE_REPORT cache=(\S+) outcome=(\S+) primary_key=(\S+) matched_key=(\S*)")
PUBLISHED_RE = re.compile(r"Cache saved with key: (dist-v3-\S+)")
CACHE_SIZE_RE = re.compile(r"Cache Size: ~(\d+) MB \((\d+) B\)")
RESTORED_RE = re.compile(r"Cache restored from key: (\S+)")
COMPILING_RE = re.compile(r"^\[\s*(\d+) of (\d+)\] Compiling (\S+)")
BANNER_RE = re.compile(r"^Building (library|executable|test suite) (?:'([^']+)' )?for ")
KEY_RE = re.compile(
    r"^dist-v3-(?P<os>\w+)-(?P<image>\S+?)-ghc-(?P<ghc>[\d.]+)-cabal-(?P<cabal>[\d.]+)-"
    r"(?P<plan>[0-9a-f]{64})-epoch-(?P<epoch>\d+)$")


def parse_time(text: str) -> dt.datetime:
    return dt.datetime.fromisoformat(text.replace("Z", "+00:00"))


def step_seconds(step: dict | None) -> int | None:
    """Whole seconds a successful step took; None for a missing or skipped one."""
    if not step or step.get("conclusion") != "success":
        return None
    return int((parse_time(step["completed_at"]) - parse_time(step["started_at"])).total_seconds())


def parse_log(text: str) -> dict:
    """Facts one ``test-and-audits`` log records about its cache and merge."""
    out: dict = {"pr_number": None, "epoch": None, "merge": None, "dist": None, "dist_size_bytes": None,
                 "graphical_skipped": "Skipping graphical test-suite build" in text}
    m = EPOCH_RE.search(text)
    if m:
        keys = ("epoch", "relevant_count", "position", "size", "next_refresh_in",
                "anchor", "ref", "fallback")
        out["epoch"] = {k: (int(v) if v.isdigit() else v) for k, v in zip(keys, m.groups())}
    m = PR_REF_RE.search(text)
    if m:
        out["pr_number"] = int(m.group(1))
    m = MERGE_RE.search(text)
    if m:
        out["merge"] = {"merge_prefix": m.group(1), "head": m.group(2), "base": m.group(3)}
    for cache, outcome, primary, matched in REPORT_RE.findall(text):
        if cache == "dist-newstyle":
            out["dist"] = {"outcome": outcome, "primary_key": primary, "matched_key": matched}
    # The first "Cache Size" after the dist restore is the project snapshot's.
    idx = text.find("Cache hit for: dist-v3-")
    if idx < 0:
        idx = text.find("Cache restored from key: dist-v3-")
    if idx >= 0:
        m = CACHE_SIZE_RE.search(text, idx)
        if m:
            out["dist_size_bytes"] = int(m.group(2))
    return out


def parse_publication(text: str) -> str | None:
    """The project-cache key this job's save step actually published, if its log says so.

    A concurrent writer that loses the race for an immutable key finishes its
    save step successfully after only a warning ("Unable to reserve cache with
    key ..."), so a green step proves nothing: only "Cache saved with key" does.
    """
    match = PUBLISHED_RE.search(text)
    return match.group(1) if match else None


def split_key(key: str) -> dict | None:
    m = KEY_RE.match(key or "")
    return m.groupdict() if m else None


def job_seconds(job: dict) -> dict:
    """Wall seconds of a job and of each of its steps, from the Actions API."""
    def span(item: dict) -> int | None:
        if not item.get("started_at") or not item.get("completed_at"):
            return None
        return int((parse_time(item["completed_at"]) - parse_time(item["started_at"])).total_seconds())
    return {"seconds": span(job), "conclusion": job.get("conclusion"),
            "completed_at": job.get("completed_at"),
            "steps": {s["name"]: span(s) for s in job["steps"] if s.get("conclusion") == "success"}}


WRITER_JOB = "project-cache"


def writer_evidence(job: dict, all_jobs: list[dict], writer_text: str = "") -> dict:
    """Whether this run could write the project cache, whether it published, and when it finished.

    Since #2745 a separate ``project-cache`` job, after the verdict, saves the
    snapshot; before it, the ``test-and-audits`` job's own save step did.  Both
    workflows are read: the writer job is the ``project-cache`` job when the run
    has one, otherwise the audit job, and ``writer_text`` is that job's log.
    Publication needs the log's "Cache saved with key" line, not just a green step.
    """
    writer = next((j for j in all_jobs if j["name"] == WRITER_JOB), job)
    published = parse_publication(writer_text)
    return {"writer_ran": writer.get("conclusion") == "success",
            "published_key": published, "saved_project_cache": published is not None,
            "writer_completed_at": writer.get("completed_at")}


def run_record(run: dict, job: dict, text: str, all_jobs: list[dict] | None = None,
               writer_text: str | None = None) -> dict:
    steps = {s["name"]: s for s in job["steps"]}
    facts = parse_log(text)
    return {
        "run_id": run["databaseId"],
        "attempt": run.get("attempt", 1),
        "event": run["event"],
        "branch": run["headBranch"],
        "head_sha": run["headSha"],
        "created_at": run["createdAt"],
        "wall_seconds": int((parse_time(run["updatedAt"]) - parse_time(run["startedAt"])).total_seconds()),
        "jobs": {j["name"]: job_seconds(j) for j in (all_jobs or [job])},
        "job_id": job["id"],
        "job_started_at": job["started_at"],
        "job_completed_at": job["completed_at"],
        "lib_exe_seconds": step_seconds(steps.get(STEP_LIB)),
        "test_suite_seconds": step_seconds(steps.get(STEP_TESTS)),
        **writer_evidence(job, all_jobs or [job], text if writer_text is None else writer_text),
        **facts,
    }


def gh_api(path: str, *extra: str) -> str:
    proc = subprocess.run(["gh", "api", path, *extra], capture_output=True, text=True)
    if proc.returncode != 0:
        raise RuntimeError(f"gh api {path} failed: {proc.stderr.strip()}")
    return proc.stdout


def log_cache_path(cache_dir: Path, run_id: int, job_id: int) -> Path:
    # a rerun is a new job with new timestamps and cache lines: never reuse another attempt's log
    return cache_dir / f"log_{run_id}_{job_id}.txt"


def collect(repo: str, since: str, until: str, cache_dir: Path, jobs_only: bool = False) -> list[dict]:
    """Records for every successful CI run created in [since, until]."""
    listing = subprocess.run(
        ["gh", "run", "list", "-R", repo, "--workflow", "ci.yml",
         "--created", f"{since}..{until}", "--limit", "1000",
         "--json", "databaseId,event,conclusion,headBranch,headSha,createdAt,attempt,startedAt,updatedAt"],
        capture_output=True, text=True, check=True)
    runs = [r for r in json.loads(listing.stdout)
            if r["conclusion"] == "success" and r["event"] in ("push", "pull_request")]
    records = []
    cache_dir.mkdir(parents=True, exist_ok=True)
    for run in sorted(runs, key=lambda r: r["createdAt"]):
        jobs = json.loads(gh_api(f"repos/{repo}/actions/runs/{run['databaseId']}/jobs", "--paginate"))
        job = next((j for j in jobs["jobs"] if j["name"] == JOB_NAME), None)
        if job is None:
            continue
        if jobs_only:
            records.append(run_record(run, job, "", jobs["jobs"]))
            continue
        log_path = log_cache_path(cache_dir, run["databaseId"], job["id"])
        if not log_path.exists():
            log_path.write_text(gh_api(f"repos/{repo}/actions/jobs/{job['id']}/logs"))
        # since #2745 the project-cache job, not the audit job, publishes: read its log too
        writer = next((j for j in jobs["jobs"] if j["name"] == WRITER_JOB), None)
        writer_text = None
        if writer is not None:
            writer_path = log_cache_path(cache_dir, run["databaseId"], writer["id"])
            if writer.get("conclusion") == "success" and not writer_path.exists():
                writer_path.write_text(gh_api(f"repos/{repo}/actions/jobs/{writer['id']}/logs"))
            writer_text = writer_path.read_text() if writer_path.exists() else ""
        records.append(run_record(run, job, log_path.read_text(), jobs["jobs"], writer_text))
    return records


def snapshot_writers(records: list[dict]) -> dict[str, list[dict]]:
    """Master pushes that saved each project-cache key, oldest first."""
    writers: dict[str, list[dict]] = {}
    for rec in records:
        if rec["event"] == "push" and rec["saved_project_cache"] and rec["dist"]:
            writers.setdefault(rec["published_key"], []).append(
                {"run_id": rec["run_id"], "sha": rec["head_sha"],
                 "completed_at": rec["writer_completed_at"]})
    # by publication time, not run creation: a re-run of an old push publishes later than newer runs
    for entries in writers.values():
        entries.sort(key=lambda w: parse_time(w["completed_at"]))
    return writers


def restored_snapshot(rec: dict, writers: dict[str, list[dict]]) -> dict | None:
    """The master job whose tree the run restored, if the evidence names one.

    A key is immutable, so the writer is the last push that saved the matched
    key before the restore.  Two writers of one key mean the entry was evicted
    and re-seeded in between.
    """
    dist = rec.get("dist")
    if not dist or not dist["matched_key"]:
        return None
    started = parse_time(rec["job_started_at"])
    before = [w for w in writers.get(dist["matched_key"], []) if parse_time(w["completed_at"]) <= started]
    return max(before, key=lambda w: parse_time(w["completed_at"])) if before else None


# --- plan -------------------------------------------------------------------


def master_chain(repo: Path, ref: str, anchor: str = epoch_mod.EPOCH_ANCHOR) -> dict:
    """First-parent history after the anchor, with each commit's relevant count.

    ``index[sha]`` is the ``relevant_count`` ``ci_cache_epoch.py`` reports for
    that commit as a ref; ``at[n]`` is the commit that made the count n.
    """
    marker = "CI_CACHE_COMMIT "
    text = git(repo, "log", "--first-parent", "--diff-merges=first-parent",
               "--no-renames", "--name-only", "--reverse",
               f"--format={marker}%H\t%cI\t%s", f"{anchor}..{ref}")
    commits: list[dict] = []
    for raw in text.splitlines():
        if raw.startswith(marker):
            sha, when, subject = raw[len(marker):].split("\t", 2)
            commits.append({"sha": sha, "committed_at": when, "subject": subject, "paths": []})
        elif raw.strip() and commits:
            commits[-1]["paths"].append(raw.strip())
    index: dict[str, int] = {}
    at: dict[int, str] = {}
    count = 0
    for commit in commits:
        commit["relevant"] = epoch_mod.is_build_relevant(commit["paths"])
        count += commit["relevant"]
        commit["count"] = count
        index[commit["sha"]] = count
        if commit["relevant"]:
            at[count] = commit["sha"]
    return {"commits": commits, "index": index, "at": at}


def plan_state_name(count: int) -> str:
    return f"c{count}"


def legacy_policy_done(step_id: str, run_id: int, snap: int) -> bool:
    """Whether ``step_id`` is an earlier-planner ``alt``/``lag`` build of the same (run, snapshot)."""
    parts = step_id.split(":")
    return parts[0] in ("alt2", "alt4", "lag") and parts[1:] == [str(run_id), str(snap)]


def build_steps(runs: list[dict], chain: dict, chain_runs: frozenset[int] = frozenset(),
                done: frozenset[str] = frozenset(),
                existing: frozenset[str] = frozenset()) -> list[dict]:
    """Ordered replay steps for the sampled runs, with state lifetimes.

    Each run record carries ``run_id``, ``snapshot`` (the relevant count CI
    restored), ``base_count``, ``head`` and ``policy_snapshot`` (size -> relevant count, see
    ``policy_snapshot``).
    Each policy is replayed from the snapshot it would have restored, unless
    that snapshot is the restored one (identical to ``unsplit``) or the base
    (``own``).  ``chain_runs`` names runs whose
    master drift is also replayed one master change at a time.

    ``done`` lists step ids whose results already exist and ``existing`` the
    saved states still on disk, so an interrupted replay re-plans only what is
    left: a state a pending step needs is rebuilt from the nearest live one
    (or from nothing, which is a cold build).
    """
    at = chain["at"]
    steps: list[dict] = []
    live: set[str] = set(existing)

    def nearest_live(count: int) -> str | None:
        counts = [int(n[1:]) for n in live]
        return plan_state_name(min(counts, key=lambda c: (abs(c - count), c))) if counts else None

    def need(count: int) -> str:
        name = plan_state_name(count)
        if name not in live:
            steps.append({"id": f"state:{count}", "from": nearest_live(count),
                          "commit": at[count], "save_as": name})
            live.add(name)
        return name

    def add(step: dict, wants: int | None = None) -> None:
        if step["id"] in done:
            return
        if wants is not None:
            step["from"] = need(wants)
        steps.append(step)
        if step.get("save_as"):
            live.add(step["save_as"])

    for run in sorted(runs, key=lambda r: (r["base_count"], r["run_id"])):
        rid, sigma, base = run["run_id"], run["snapshot"], run["base_count"]
        tested = {"commit": f"tested:{rid}"}
        drift_id = f"drift:{sigma}:{base}"
        if base != sigma and drift_id not in done and not any(st["id"] == drift_id for st in steps):
            # one measurement per (snapshot, base) pair, whether or not the base state already exists
            drift = {"id": drift_id, "commit": at[base]}
            if plan_state_name(base) not in live:
                drift["save_as"] = plan_state_name(base)
            add(drift, wants=sigma)
        for count in (range(sigma + 2, base + 1) if rid in chain_runs else ()):
            add({"id": f"chain:{count - 1}:{count}", "commit": at[count]}, wants=count - 1)
        if base != sigma:
            add({"id": f"own:{rid}", **tested}, wants=base)
        add({"id": f"unsplit:{rid}", **tested}, wants=sigma)
        for snap in sorted({v for v in run["policy_snapshot"].values() if v is not None} - {sigma, base}):
            if snap in at and not any(legacy_policy_done(i, rid, snap) for i in done):
                add({"id": f"pol:{rid}:{snap}", **tested}, wants=snap)
    last_use: dict[str, int] = {}
    for position, step in enumerate(steps):
        if step["from"]:
            last_use[step["from"]] = position
    for name, position in last_use.items():
        steps[position].setdefault("drop_after", []).append(name)
    return steps


def key_tuple(primary_key: str) -> tuple | None:
    """(os, image, ghc, cabal, plan): what makes two dist-v3 keys the same family and plan."""
    key = split_key(primary_key)
    return (key["os"], key["image"], key["ghc"], key["cabal"], key["plan"]) if key else None


def writer_entries(records: list[dict], chain: dict) -> list[dict]:
    """Every successful master writer job: position, relevant count, key family and finish time."""
    position = {c["sha"]: i for i, c in enumerate(chain["commits"])}
    out = []
    for rec in records:
        if rec["event"] != "push" or not rec["dist"] or not rec["writer_ran"] or rec["head_sha"] not in position:
            continue
        key = key_tuple(rec["dist"]["primary_key"])
        if key is None:
            continue
        out.append({"position": position[rec["head_sha"]], "count": chain["index"][rec["head_sha"]],
                    "key": key, "completed": parse_time(rec["writer_completed_at"])})
    return sorted(out, key=lambda e: (e["position"], e["completed"]))


def key_at(entries: list[dict], base_position: int) -> tuple | None:
    """The key family and plan the base commit's own build used (newest writer at or before it)."""
    before = [e for e in entries if e["position"] <= base_position]
    return before[-1]["key"] if before else None


POLICY_SIZES = (1, 2, 4, 8)


def published_snapshots(entries: list[dict], size: int | None) -> list[dict]:
    """The snapshots a policy publishes, derived from the writers before any restore is chosen.

    A policy's key names (family, plan, group of ``size`` relevant changes), or
    a single commit when ``size`` is None ('a snapshot per successful push').  An
    immutable key is published by the first eligible successful job to finish
    saving it; a job that finishes later (or restored it at start) publishes
    nothing.  Writers of another image, toolchain or plan publish keys of their
    own, kept apart by the key tuple.
    """
    first: dict[tuple, dict] = {}
    for e in entries:
        ident = (e["key"], e["position"] if size is None else e["count"] // size)
        if ident not in first or e["completed"] < first[ident]["completed"]:
            first[ident] = {**e, "group": ident[1]}
    return sorted(first.values(), key=lambda p: p["completed"])


def restore_snapshot(published: list[dict], key: tuple, group: int, started_at: str,
                     base_count: int) -> int | None:
    """Relevant count of the snapshot the restore ladder picks from a policy's published snapshots.

    The ladder, over snapshots finished before the run started: the exact key
    (family, plan, group); else the newest-created key of the same family and
    plan (the same-plan prefix); else the newest-created key of the same image
    and toolchain under another plan (the last prefix).  A key of another image or
    toolchain is never restored.  Newest means last to finish saving, which is
    GitHub's choice among prefix matches, not the one nearest the base.
    """
    started = parse_time(started_at)
    ready = [p for p in published if p["completed"] <= started]
    for tier in (
        [p for p in ready if p["key"] == key and p["group"] == group],
        [p for p in ready if p["key"] == key],
        [p for p in ready if p["key"][:4] == key[:4]],
    ):
        if tier:
            chosen = max(tier, key=lambda p: p["completed"])
            if chosen["count"] > base_count:
                raise ValueError(f"the restore picks a snapshot of change {chosen['count']}, newer than the "
                                 f"base {base_count}; restoring from the future is not modelled")
            return chosen["count"]
    return None


def make_plan(records: list[dict], chain: dict, run_ids: list[int],
              chain_ids: list[int] = (), done: frozenset[str] = frozenset(),
              existing: frozenset[str] = frozenset(), repo: Path | None = None) -> dict:
    """Sample plan: per run, the restored snapshot, base, head, tested tree and policy snapshots."""
    writers = snapshot_writers(records)
    entries = writer_entries(records, chain)
    position = {c["sha"]: i for i, c in enumerate(chain["commits"])}
    published = {size: published_snapshots(entries, size) for size in (*POLICY_SIZES, None)}
    by_id = {r["run_id"]: r for r in records}
    runs = []
    for run_id in run_ids:
        rec = by_id[run_id]
        snap = restored_snapshot(rec, writers)
        if snap is None or snap["sha"] not in chain["index"]:
            raise RuntimeError(f"run {run_id}: no identifiable restored snapshot")
        base = rec["merge"]["base"]
        full_base = next(c["sha"] for c in chain["commits"] if c["sha"].startswith(base))
        base_count = chain["index"][full_base]
        bkey = key_at(entries, position[full_base])
        if bkey is None:
            raise RuntimeError(f"run {run_id}: no master writer at or before its base {full_base[:9]}")
        started = rec["job_started_at"]
        policies = {str(size): restore_snapshot(published[size], bkey, base_count // size, started, base_count)
                    for size in POLICY_SIZES}
        policies["every"] = restore_snapshot(published[None], bkey, position[full_base], started, base_count)
        runs.append({"run_id": run_id, "attempt": rec["attempt"], "snapshot": chain["index"][snap["sha"]],
                     "snapshot_sha": snap["sha"], "snapshot_writer_run": snap["run_id"], "base": full_base,
                     "base_count": chain["index"][full_base], "head": rec["merge"]["head"],
                     "tested_tree": (git(repo, "merge-tree", "--write-tree", full_base, rec["merge"]["head"])
                                     .splitlines()[0] if repo else None),
                     "restored_key": rec["dist"]["matched_key"], "primary_key": rec["dist"]["primary_key"],
                     "policy_snapshot": policies})
    unreconciled = [(r["run_id"], r["snapshot"], r["policy_snapshot"]["8"]) for r in runs
                    if r["policy_snapshot"]["8"] != r["snapshot"]]
    if unreconciled:
        raise RuntimeError("today's policy does not reproduce the restored snapshot for (run, observed, "
                           f"modelled): {unreconciled}; the baseline would not be the observed builds")
    missing = [(r["run_id"], k) for r in runs for k, v in r["policy_snapshot"].items() if v is None]
    if missing:
        raise RuntimeError(f"no snapshot available under some policy (cold build): {missing}; "
                           "model the cold build or drop the run from every policy alike")
    return {"runs": runs, "steps": build_steps(runs, chain, frozenset(chain_ids), done, existing)}


# --- fan-out ----------------------------------------------------------------

IMPORT_RE = re.compile(r'^import\s+(?:safe\s+)?(?:qualified\s+)?(?:"[^"]*"\s+)?([A-Z][\w.]*)', re.M)
SOURCE_ROOTS = (("src/", "lib"), ("test-headless/", "test"), ("app/", "app"))


def source_module(path: str) -> tuple[str, str] | None:
    for root, kind in SOURCE_ROOTS:
        if path.startswith(root) and path.endswith(".hs"):
            return path[len(root):-3].replace("/", "."), kind
    return None


def import_graph(repo: Path, commit: str) -> tuple[dict[str, set[str]], dict[str, str]]:
    """Static module import graph of the tree at ``commit`` (an upper bound on recompilation)."""
    graph: dict[str, set[str]] = {}
    kind_of: dict[str, str] = {}
    names = git(repo, "ls-tree", "-r", "--name-only", commit, "src", "test-headless", "app").split()
    for path in names:
        parsed = source_module(path)
        if parsed is None:
            continue
        module, kind = parsed
        graph[module] = set(IMPORT_RE.findall(git(repo, "show", f"{commit}:{path}")))
        kind_of[module] = kind
    return graph, kind_of


def reverse_closure(graph: dict[str, set[str]], start: str) -> set[str]:
    reverse: dict[str, set[str]] = {}
    for module, deps in graph.items():
        for dep in deps:
            reverse.setdefault(dep, set()).add(module)
    seen: set[str] = set()
    stack = [start]
    while stack:
        for importer in reverse.get(stack.pop(), ()):
            if importer not in seen:
                seen.add(importer)
                stack.append(importer)
    return seen


def fan_out(repo: Path, before: str, after: str, top: int = 8) -> dict:
    """Changed modules ranked by how many library/test modules transitively import them."""
    changed = git(repo, "diff", "--name-only", before, after, "--", "src", "app", "test-headless").split()
    graph, kind_of = import_graph(repo, after)
    rows = []
    for path in changed:
        parsed = source_module(path)
        if parsed is None or parsed[0] not in graph:
            continue
        closure = reverse_closure(graph, parsed[0])
        rows.append({"module": parsed[0], "lib": sum(kind_of[m] == "lib" for m in closure),
                     "test": sum(kind_of[m] == "test" for m in closure)})
    rows.sort(key=lambda r: (-r["lib"], -r["test"], r["module"]))
    return {"changed_files": len(changed), "top": rows[:top]}


# --- replay -----------------------------------------------------------------


def git(repo: Path, *args: str) -> str:
    proc = subprocess.run(["git", "-C", str(repo), *args], capture_output=True, text=True)
    if proc.returncode != 0:
        raise RuntimeError(f"git {' '.join(args)} failed: {proc.stderr.strip() or proc.stdout.strip()}")
    return proc.stdout.strip()


def tested_commit(repo: Path, base: str, head: str, ref: str) -> str:
    """The merge GitHub tested, rebuilt from its two parents.

    GitHub's ``refs/pull/N/merge`` commit is not fetchable once the PR's
    merge ref moves on, so the tree is re-derived by merging the recorded head
    into the recorded base.  A conflict is an error, never a guess.
    """
    tree = git(repo, "merge-tree", "--write-tree", base, head).splitlines()[0]
    commit = git(repo, "commit-tree", tree, "-p", base, "-p", head,
                 "-m", f"ci-build-drift replay merge of {head[:9]} into {base[:9]}")
    git(repo, "update-ref", ref, commit)
    return commit


def parse_build_log(text: str) -> dict[str, list[str]]:
    """Modules each Cabal component recompiled, in compile order."""
    components: dict[str, list[str]] = {}
    current = "setup"  # Custom Setup.hs compiles before the first component banner
    for line in text.splitlines():
        banner = BANNER_RE.match(line)
        if banner:
            kind, name = banner.groups()
            current = {"library": "lib"}.get(kind, name or kind)
            components.setdefault(current, [])
            continue
        match = COMPILING_RE.match(line)
        if match:
            components.setdefault(current, []).append(match.group(3))
    return components


class Replay:
    """One replay worktree whose ``dist-newstyle`` is swapped between saved states."""

    def __init__(self, worktree: Path, state_dir: Path):
        self.worktree = worktree
        self.state_dir = state_dir
        state_dir.mkdir(parents=True, exist_ok=True)
        (state_dir / "logs").mkdir(exist_ok=True)

    @property
    def dist(self) -> Path:
        return self.worktree / "dist-newstyle"

    def _discard(self) -> None:
        # The one directory this tool ever deletes: a fixed name inside its own state dir.
        trash = self.state_dir / "_discard"
        assert trash.parent == self.state_dir and trash.name == "_discard"
        if trash.exists():
            shutil.rmtree(trash)

    def restore(self, name: str | None) -> None:
        """Make ``dist-newstyle`` a clone of saved state ``name`` (empty when None)."""
        self._discard()
        if self.dist.exists():
            self.dist.rename(self.state_dir / "_discard")
        if name is not None:
            subprocess.run(["cp", "-Rpc", str(self.state_dir / name), str(self.dist)], check=True)

    def has_state(self, name: str) -> bool:
        return (self.state_dir / name).exists()

    def save(self, name: str) -> None:
        target = self.state_dir / name
        if not target.exists():
            subprocess.run(["cp", "-Rpc", str(self.dist), str(target)], check=True)

    def drop(self, name: str) -> None:
        target = self.state_dir / name
        assert target.parent == self.state_dir and name not in ("", ".", "..", "logs")
        if target.exists():
            shutil.rmtree(target)

    def build(self, label: str, commit: str) -> dict:
        """Check ``commit`` out and run CI's build commands, recording what recompiled."""
        git(self.worktree, "checkout", "--detach", "--force", commit)
        modules: dict[str, list[str]] = {}
        started = time.monotonic()
        for command in BUILD_COMMANDS:
            log = self.state_dir / "logs" / f"{label}.{command[-1]}.log"
            with log.open("w") as handle:
                proc = subprocess.run([CABAL, *command, "-v1"], cwd=self.worktree,
                                      stdout=handle, stderr=subprocess.STDOUT)
            if proc.returncode != 0:
                raise RuntimeError(f"{label}: cabal {' '.join(command)} failed; see {log}")
            for component, mods in parse_build_log(log.read_text()).items():
                modules.setdefault(component, []).extend(mods)
        return {"label": label, "commit": commit,
                "wall_seconds": round(time.monotonic() - started, 1),
                "modules": modules,
                "counts": {c: len(m) for c, m in modules.items()}}


def run_steps(replay: Replay, steps: list[dict], tested: dict[str, str],
              results_path: Path) -> None:
    """Execute plan steps in order.

    A step with a recorded result is skipped, except a state-producing step
    whose saved state is gone (dropped after use): that one runs again to
    recreate the state, and its recorded measurement is kept, not replaced.
    ``tested`` maps ``tested:<run>`` placeholders to the reconstructed merge
    commits.  A state is dropped after the last step that restores it.
    """
    results = json.loads(results_path.read_text()) if results_path.exists() else {}
    for step in steps:
        sid = step["id"]
        measured = sid in results
        rebuild = bool(step.get("save_as")) and not replay.has_state(step["save_as"])
        if not measured or rebuild:
            replay.restore(step["from"])  # None starts from nothing: a cold build
            commit = tested.get(step["commit"], step["commit"])
            result = replay.build(sid, commit)
            if step.get("save_as"):
                replay.save(step["save_as"])
            if not measured:
                results[sid] = result
                results_path.write_text(json.dumps(results, indent=1))
            print(f"{sid}: {result['counts']} in {result['wall_seconds']}s"
                  f"{' (state rebuilt, recorded measurement kept)' if measured else ''}", flush=True)
        for name in step.get("drop_after", []):
            replay.drop(name)


# --- self-test --------------------------------------------------------------

FIXTURE_LOG = """\
2026-09-27T12:54:57Z [command]/usr/bin/git checkout --progress --force refs/remotes/pull/2739/merge
2026-09-27T12:54:57Z HEAD is now at d1502696e Merge 5d7f4f576adfdff255e726ad8eb68271218132e7 into e7bed43d54882a3e299119cd211b43d7f6a15a80
CI_CACHE_EPOCH epoch=52 relevant_count=419 position=3/8 next_refresh_in=5 anchor=dd221f522276901ed08d5cb7574e9ce67ba9e2d9 ref=e7bed43d54882a3e299119cd211b43d7f6a15a80 fallback=false
Cache hit for: dist-v3-Linux-ghcr.io/coghex/synarchy-ci:ci-f90dae38568ffe9f-ghc-9.12.2-cabal-3.16.1.0-20edd55a88069cc396151ca71f7f933cdf022252dcc35116f0e49088c34527c2-epoch-52
Received 395054423 of 395054423 (100.0%), 214.1 MBs/sec
Cache Size: ~377 MB (395054423 B)
CI_CACHE_REPORT cache=dist-newstyle outcome=EXACT_HIT primary_key=dist-v3-Linux-ghcr.io/coghex/synarchy-ci:ci-f90dae38568ffe9f-ghc-9.12.2-cabal-3.16.1.0-20edd55a88069cc396151ca71f7f933cdf022252dcc35116f0e49088c34527c2-epoch-52 matched_key=dist-v3-Linux-ghcr.io/coghex/synarchy-ci:ci-f90dae38568ffe9f-ghc-9.12.2-cabal-3.16.1.0-20edd55a88069cc396151ca71f7f933cdf022252dcc35116f0e49088c34527c2-epoch-52
Skipping graphical test-suite build: no graphics-facing paths changed.
"""

FIXTURE_BUILD = """\
Building library for synarchy-0.1.0.0..
[ 1 of 3] Compiling Alpha ( src/Alpha.hs, x.o )
[ 2 of 3] Compiling Beta ( src/Beta.hs, x.o )
Building executable 'synarchy' for synarchy-0.1.0.0..
[ 1 of 1] Compiling Main ( app/Main.hs, x.o )
Building test suite 'synarchy-test-headless' for synarchy-0.1.0.0..
[ 1 of 2] Compiling Spec ( test-headless/Spec.hs, x.o )
"""


KEY_A = ("Linux", "ghcr.io/x/ci:ci-aaaa", "9.12.2", "3.16.1.0", "a" * 64)
KEY_B = ("Linux", "ghcr.io/x/ci:ci-bbbb", "9.12.2", "3.16.1.0", "a" * 64)    # same plan, rotated image
KEY_A2 = ("Linux", "ghcr.io/x/ci:ci-aaaa", "9.12.2", "3.16.1.0", "c" * 64)   # same image, new plan


def _when(hour: int) -> dt.datetime:
    return dt.datetime(2026, 9, 27, tzinfo=dt.timezone.utc) + dt.timedelta(hours=hour)


def _entry(position: int, count: int, hour: int, key: tuple = KEY_A) -> dict:
    return {"position": position, "count": count, "completed": _when(hour), "key": key}


def check_restore_model() -> None:
    start = _when(12).isoformat()
    # 416's writer was cancelled: it never appears; 417 seeds epoch 52 three days late
    entries = [_entry(0, 414, 1), _entry(1, 415, 2), _entry(3, 417, 6), _entry(4, 418, 7), _entry(5, 419, 20)]
    def ask(size, base, key=KEY_A, group=None):
        pub = published_snapshots(entries, size)
        return restore_snapshot(pub, key, base // size if group is None else group, start, base)
    assert ask(8, 418) == 417        # the group's key is published by the first writer to finish (417)
    assert ask(1, 419) == 418        # 419 had not finished: the same-plan prefix restores the newest ready
    assert ask(2, 418) == 418
    assert ask(4, 419) == 417
    assert ask(8, 415) == 414        # earliest writer of the group 408-415
    # a writer that lost the race to publish an immutable key publishes nothing
    twin = entries + [_entry(6, 417, 9)]   # a second job at count 417, finishing later
    assert [p["completed"] for p in published_snapshots(twin, 8) if p["group"] == 52] == [_when(6)]
    # the published set, not the latest writer, feeds the ladder: count 416 seeds plan A, 417 hits that
    # key (publishes nothing), 418 builds plan B and has not finished when the run starts
    plan_change = [_entry(0, 416, 1, KEY_A), _entry(1, 417, 5, KEY_A), _entry(2, 418, 20, KEY_A2)]
    pub8 = published_snapshots(plan_change, 8)
    assert sorted(p["count"] for p in pub8) == [416, 418]
    assert restore_snapshot(pub8, KEY_A2, 418 // 8, start, 418) == 416     # not 417, which published nothing
    assert restore_snapshot(published_snapshots(plan_change, 1), KEY_A2, 418, start, 418) == 417
    # image rotation with the same plan hash: only the base's own image family is exact; another image is never restored
    rotated = [_entry(0, 416, 1, KEY_A), _entry(1, 417, 3, KEY_B)]
    pub = published_snapshots(rotated, 8)
    assert restore_snapshot(pub, KEY_B, 418 // 8, start, 418) == 417
    assert restore_snapshot(published_snapshots([_entry(0, 416, 1, KEY_A)], 8), KEY_B, 52, start, 418) is None
    # the last prefix: same image and toolchain, older plan
    replan = [_entry(0, 410, 1, KEY_A), _entry(1, 417, 30, KEY_A2)]
    assert restore_snapshot(published_snapshots(replan, 8), KEY_A2, 52, start, 417) == 410
    # a restore from the future is refused, not modelled
    try:
        restore_snapshot(published_snapshots([_entry(0, 420, 1)], 8), KEY_A, 52, start, 417)
    except ValueError:
        pass
    else:
        raise AssertionError("a snapshot newer than the base must not be restored silently")
    # 'every successful push': a key per commit; the base's own commit when ready, else the newest-created ready one
    pushes = [_entry(1, 417, 2), _entry(2, 418, 7), _entry(3, 419, 20)]
    assert restore_snapshot(published_snapshots(pushes, None), KEY_A, 3, start, 419) == 418   # own push not finished
    later = pushes + [_entry(5, 419, 9)]   # a later push at the same count, finished before the run started
    pub = published_snapshots(later, None)
    assert restore_snapshot(pub, KEY_A, 3, start, 419) == 419    # newest-created ready key, same compiled content
    assert restore_snapshot(pub, KEY_A, 5, start, 419) == 419    # exact key of the base commit itself
    assert key_at(rotated, 1) == KEY_B
    assert key_tuple("dist-v3-Linux-ghcr.io/x/ci:ci-aaaa-ghc-9.12.2-cabal-3.16.1.0-" + "a" * 64 + "-epoch-52") == KEY_A


class StubReplay:
    """Records what the executor does to saved states; builds nothing."""

    def __init__(self, states: set[str]):
        self.states, self.restored, self.built = set(states), [], []

    def has_state(self, name: str) -> bool:
        return name in self.states

    def restore(self, name: str | None) -> None:
        assert name is None or name in self.states, f"restoring missing state {name}"
        self.restored.append(name)

    def build(self, label: str, commit: str) -> dict:
        self.built.append(label)
        return {"label": label, "commit": commit, "wall_seconds": 0.0, "modules": {}, "counts": {}}

    def save(self, name: str) -> None:
        self.states.add(name)

    def drop(self, name: str) -> None:
        self.states.discard(name)


def check_plan_invariants() -> None:
    """Every step restores a state an earlier step (or the disk) provided and has not dropped."""
    chain = {"at": {n: f"sha{n}" for n in range(410, 436)}}

    def snaps(sigma: int, base: int, **over: int) -> dict:
        return {"snapshot": sigma, "base_count": base,
                "policy_snapshot": {"1": over.get("one", base), "2": over.get("two", sigma),
                                    "4": over.get("four", sigma), "8": sigma,
                                    "every": over.get("every", base)}}
    runs = [{"run_id": 1, **snaps(417, 417)},
            {"run_id": 2, **snaps(417, 419, one=418, two=418, every=418)},
            {"run_id": 3, **snaps(420, 423, one=422, two=422, every=422)},
            {"run_id": 4, **snaps(424, 430, two=430, four=428)},
            # the same base 419 as run 2 but another restored snapshot (418 instead of 417)
            {"run_id": 5, **snaps(418, 419)}]
    for done, existing in ((frozenset(), frozenset({"c417", "c418"})),
                           (frozenset({"unsplit:1", "drift:417:419"}), frozenset())):
        steps = build_steps(runs, chain, frozenset({2}), done, existing)
        provided = set(existing)
        for step in steps:
            assert step["from"] is None or step["from"] in provided, (step, sorted(provided))
            if step.get("save_as"):
                provided.add(step["save_as"])
            provided.difference_update(step.get("drop_after", []))
        ids = [step["id"] for step in steps]
        assert len(set(ids)) == len(ids) and not set(ids) & done, ids
    # every (snapshot, base) pair is measured, even when the base state already exists
    steps = build_steps(runs, chain, frozenset(), frozenset(), frozenset({"c417", "c418"}))
    ids = {step["id"] for step in steps}
    assert {"drift:417:419", "drift:418:419"} <= ids, sorted(ids)

    # planner and executor together: a completed sample whose saved states were dropped
    import tempfile
    with tempfile.TemporaryDirectory() as tmp:
        results = Path(tmp) / "replay.json"
        results.write_text(json.dumps({"unsplit:1": {"counts": {}}, "drift:417:419": {"counts": {"kept": 1}}}))
        steps = build_steps(runs, chain, frozenset(), frozenset({"unsplit:1", "drift:417:419"}), frozenset())
        stub = StubReplay(set())
        with contextlib.redirect_stdout(io.StringIO()):
            run_steps(stub, steps, {f"tested:{i}": f"merge{i}" for i in range(1, 6)}, results)
        recorded = json.loads(results.read_text())
        assert recorded["drift:417:419"] == {"counts": {"kept": 1}}, "a rebuilt state must not replace its measurement"
        assert all(i in recorded for i in ("own:2", "unsplit:5", "drift:418:419")), sorted(recorded)
        # a state-producing step that already has a result but lost its state runs again
        again = [{"id": "state:417", "from": None, "commit": "sha417", "save_as": "c417"}]
        results.write_text(json.dumps({"state:417": {"counts": {"old": 1}}}))
        stub = StubReplay(set())
        with contextlib.redirect_stdout(io.StringIO()):
            run_steps(stub, again, {}, results)
        assert stub.built == ["state:417"] and "c417" in stub.states
        assert json.loads(results.read_text())["state:417"] == {"counts": {"old": 1}}


def check_plan_change_writer_lag() -> None:
    """End to end: make_plan across a plan change whose writer is still running."""
    def key(plan: str, epoch: int) -> str:
        return f"dist-v3-Linux-ghcr.io/x/ci:ci-aaaa-ghc-9.12.2-cabal-3.16.1.0-{plan * 64}-epoch-{epoch}"
    chain = {"commits": [{"sha": f"s{n}"} for n in (416, 417, 418)],
             "index": {"s416": 416, "s417": 417, "s418": 418}, "at": {416: "s416", 417: "s417", 418: "s418"}}
    def push(run_id: int, sha: str, plan: str, done: str, saved: bool) -> dict:
        k = key(plan, 52)
        return {"run_id": run_id, "event": "push", "head_sha": sha, "dist": {"primary_key": k, "matched_key": k},
                "writer_ran": True, "saved_project_cache": saved, "published_key": k if saved else None,
                "writer_completed_at": done}
    records = [push(1, "s416", "a", "2026-09-27T01:00:00Z", True),    # seeds plan A
               push(2, "s417", "a", "2026-09-27T02:00:00Z", False),   # exact hit: publishes nothing
               push(3, "s418", "b", "2026-09-27T20:00:00Z", True)]    # plan B, still running at the run's start
    pr = {"run_id": 9, "event": "pull_request", "attempt": 1, "job_started_at": "2026-09-27T12:00:00Z",
          "merge": {"base": "s418", "head": "headsha"},
          "dist": {"primary_key": key("b", 52), "matched_key": key("a", 52)}}
    plan = make_plan(records + [pr], chain, [9])
    run = plan["runs"][0]
    assert run["snapshot"] == 416 and run["policy_snapshot"]["8"] == 416, run     # the observed restore, reconciled
    assert run["policy_snapshot"]["1"] == 417 and run["policy_snapshot"]["every"] == 417, run
    # an unreconciled baseline is refused rather than projected from a snapshot nobody restored
    wrong = {**pr, "dist": {"primary_key": key("b", 52), "matched_key": key("a", 52)}}
    wrong_records = [push(1, "s416", "a", "2026-09-27T01:00:00Z", True), push(2, "s417", "a", "2026-09-27T02:00:00Z", True)]
    try:
        make_plan(wrong_records + [wrong], chain, [9])
    except RuntimeError as error:
        assert "does not reproduce" in str(error) or "no identifiable" in str(error), error
    else:
        raise AssertionError("the baseline must reconcile with the restored snapshot")


def check_writer_evidence() -> None:
    def job(name: str, conclusion: str, done: str, saved: bool = False) -> dict:
        steps = [{"name": STEP_SAVE, "conclusion": "success" if saved else "skipped"}]
        return {"name": name, "conclusion": conclusion, "completed_at": done, "steps": steps}
    published = "dist-v3-Linux-ghcr.io/x/ci:ci-aaaa-ghc-9.12.2-cabal-3.16.1.0-" + "a" * 64 + "-epoch-52"
    won = f"2026-09-27T13:00:00Z Cache saved with key: {published}\n"
    lost = ("##[warning]Failed to save: Unable to reserve cache with key "
            f"{published}, another job may be creating this cache.\n")
    assert parse_publication(won) == published and parse_publication(lost) is None
    audit = job(JOB_NAME, "success", "2026-09-27T13:00:00Z", saved=True)
    # historical workflow: the audit job wrote the snapshot, and its own log says so
    old = writer_evidence(audit, [audit], won)
    assert old == {"writer_ran": True, "published_key": published, "saved_project_cache": True,
                   "writer_completed_at": "2026-09-27T13:00:00Z"}, old
    # a green save step whose log shows the lost race published nothing
    loser = writer_evidence(audit, [audit], lost)
    assert loser["writer_ran"] is True and loser["saved_project_cache"] is False and loser["published_key"] is None
    # current workflow: the separate project-cache job publishes, later; its log, not the audit job's, decides
    audit2 = job(JOB_NAME, "success", "2026-10-20T13:00:00Z", saved=False)
    writer = job(WRITER_JOB, "success", "2026-10-20T13:12:00Z", saved=True)
    new = writer_evidence(audit2, [audit2, writer], won)
    assert new["saved_project_cache"] and new["writer_completed_at"] == "2026-10-20T13:12:00Z", new
    assert writer_evidence(audit2, [audit2, writer], lost)["saved_project_cache"] is False
    skipped = writer_evidence(audit2, [audit2, job(WRITER_JOB, "skipped", "2026-10-20T13:13:00Z")], "")
    assert skipped["writer_ran"] is False and skipped["saved_project_cache"] is False
    assert job_seconds({"name": "x", "started_at": "2026-10-20T13:00:00Z", "completed_at": "2026-10-20T13:01:00Z",
                        "steps": []})["completed_at"] == "2026-10-20T13:01:00Z"
    assert log_cache_path(Path("c"), 5, 7) != log_cache_path(Path("c"), 5, 8)  # a rerun is a different job
    # a publisher followed by a green no-op save: only the publisher is a writer of the key
    def push_rec(run_id: int, done: str, saved: bool) -> dict:
        return {"run_id": run_id, "event": "push", "head_sha": f"sha{run_id}", "saved_project_cache": saved,
                "published_key": published if saved else None, "writer_completed_at": done,
                "dist": {"primary_key": published}}
    writers = snapshot_writers([push_rec(1, "2026-09-27T02:00:00Z", True), push_rec(2, "2026-09-27T02:05:00Z", False)])
    assert [w["run_id"] for w in writers[published]] == [1]


def self_test() -> int:
    facts = parse_log(FIXTURE_LOG)
    assert facts["pr_number"] == 2739, facts
    assert facts["epoch"]["epoch"] == 52 and facts["epoch"]["position"] == 3, facts
    assert facts["merge"] == {"merge_prefix": "d1502696e",
                              "head": "5d7f4f576adfdff255e726ad8eb68271218132e7",
                              "base": "e7bed43d54882a3e299119cd211b43d7f6a15a80"}, facts
    assert facts["dist"]["outcome"] == "EXACT_HIT", facts
    assert facts["dist_size_bytes"] == 395054423, facts
    assert facts["graphical_skipped"] is True
    assert split_key(facts["dist"]["primary_key"])["epoch"] == "52"
    assert split_key("deps-v2-Linux-ghc9.12.2-abc") is None

    assert parse_build_log(FIXTURE_BUILD) == {
        "lib": ["Alpha", "Beta"], "synarchy": ["Main"], "synarchy-test-headless": ["Spec"]}

    step = {"conclusion": "success", "started_at": "2026-09-27T12:00:00Z",
            "completed_at": "2026-09-27T12:03:07Z"}
    assert step_seconds(step) == 187
    assert step_seconds({**step, "conclusion": "skipped"}) is None
    assert step_seconds(None) is None

    key = facts["dist"]["primary_key"]

    def push(run_id: int, done: str, saved: bool) -> dict:
        return {"run_id": run_id, "event": "push", "created_at": done, "head_sha": f"sha{run_id}",
                "saved_project_cache": saved, "published_key": key if saved else None,
                "writer_ran": True, "writer_completed_at": done,
                "job_completed_at": done, "dist": {"primary_key": key}}

    records = [push(1, "2026-09-24T20:30:00Z", True), push(2, "2026-09-26T01:00:00Z", False),
               push(3, "2026-09-27T01:40:00Z", True)]
    writers = snapshot_writers(records)
    assert [w["run_id"] for w in writers[key]] == [1, 3]
    pr = {"dist": {"matched_key": key}, "job_started_at": "2026-09-26T12:00:00Z"}
    assert restored_snapshot(pr, writers)["run_id"] == 1
    pr["job_started_at"] = "2026-09-27T12:00:00Z"
    assert restored_snapshot(pr, writers)["run_id"] == 3
    pr["job_started_at"] = "2026-09-20T12:00:00Z"
    assert restored_snapshot(pr, writers) is None
    # an old run re-run later republishes the key after newer runs did: the restore picks the
    # latest publication before the restore, not the latest-created run
    reseed = [push(1, "2026-10-12T19:57:00Z", True), push(3, "2026-09-27T01:40:00Z", True)]
    reseed[0]["created_at"], reseed[1]["created_at"] = "2026-09-24T20:22:00Z", "2026-09-27T01:32:00Z"
    writers2 = snapshot_writers(reseed)
    assert [w["run_id"] for w in writers2[key]] == [3, 1]
    pr["job_started_at"] = "2026-10-13T12:00:00Z"
    assert restored_snapshot(pr, writers2)["run_id"] == 1
    pr["job_started_at"] = "2026-10-01T12:00:00Z"
    assert restored_snapshot(pr, writers2)["run_id"] == 3
    graph = {"A": {"B"}, "B": {"C"}, "C": set(), "D": {"C"}}
    assert reverse_closure(graph, "C") == {"A", "B", "D"} and reverse_closure(graph, "A") == set()
    assert IMPORT_RE.findall('import qualified Data.Map as M\nimport "base" Prelude\nimport safe Foo.Bar (x)\n') \
        == ["Data.Map", "Prelude", "Foo.Bar"]
    assert source_module("src/World/Types.hs") == ("World.Types", "lib")
    assert source_module("test-headless/Spec.hs") == ("Spec", "test") and source_module("README.md") is None
    check_restore_model()
    check_plan_change_writer_lag()
    check_plan_invariants()
    check_writer_evidence()
    print("ci_build_drift self-test: ok")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--self-test", action="store_true")
    sub = parser.add_subparsers(dest="stage")
    col = sub.add_parser("collect")
    col.add_argument("--repo", default=DEFAULT_REPO)
    col.add_argument("--since", required=True)
    col.add_argument("--until", required=True)
    col.add_argument("--cache-dir", type=Path, required=True)
    col.add_argument("--jobs-only", action="store_true",
                     help="skip the per-run job log (timings only)")
    col.add_argument("--out", type=Path, required=True)
    pln = sub.add_parser("plan")
    pln.add_argument("--runs", type=Path, required=True)
    pln.add_argument("--worktree", type=Path, required=True)
    pln.add_argument("--ids", type=int, nargs="+", required=True)
    pln.add_argument("--chain-ids", type=int, nargs="*", default=[],
                     help="runs whose drift is also replayed one master change at a time")
    pln.add_argument("--resume", type=Path, help="replay results to treat as done")
    pln.add_argument("--state-dir", type=Path, help="saved states still on disk (with --resume)")
    pln.add_argument("--ref", default="origin/master")
    pln.add_argument("--out", type=Path, required=True)
    fan = sub.add_parser("fan-out")
    fan.add_argument("--worktree", type=Path, required=True)
    fan.add_argument("before")
    fan.add_argument("after")
    rep = sub.add_parser("replay")
    rep.add_argument("--plan", type=Path, required=True)
    rep.add_argument("--worktree", type=Path, required=True)
    rep.add_argument("--state-dir", type=Path, required=True)
    rep.add_argument("--out", type=Path, required=True)
    args = parser.parse_args(argv)
    if args.self_test:
        return self_test()
    if args.stage == "collect":
        records = collect(args.repo, args.since, args.until, args.cache_dir, args.jobs_only)
        args.out.write_text(json.dumps(records, indent=1))
        print(f"{len(records)} successful runs -> {args.out}")
        return 0
    if args.stage == "fan-out":
        print(json.dumps(fan_out(args.worktree, args.before, args.after), indent=1))
        return 0
    if args.stage == "plan":
        chain = master_chain(args.worktree, args.ref)
        done = frozenset(json.loads(args.resume.read_text())) if args.resume else frozenset()
        existing = frozenset(p.name for p in args.state_dir.iterdir()
                             if re.fullmatch(r"c\d+", p.name)) if args.state_dir else frozenset()
        plan = make_plan(json.loads(args.runs.read_text()), chain, args.ids, args.chain_ids, done, existing,
                         args.worktree)
        args.out.write_text(json.dumps(plan, indent=1))
        print(f"{len(plan['runs'])} runs, {len(plan['steps'])} steps -> {args.out}")
        return 0
    if args.stage == "replay":
        plan = json.loads(args.plan.read_text())
        replay = Replay(args.worktree, args.state_dir)
        tested = {f"tested:{run['run_id']}":
                  tested_commit(args.worktree, run["base"], run["head"], f"refs/ci-drift-2746/{run['run_id']}")
                  for run in plan["runs"]}
        run_steps(replay, plan["steps"], tested, args.out)
        return 0
    parser.print_help()
    return 2


if __name__ == "__main__":
    sys.exit(main())
