#!/usr/bin/env python3
"""Aggregate probe-flake-result/v1 cohort documents into one summary.

Reads every <cohorts>/<cell>/inv*.json plus each cell's cohort.txt and
exit_codes.txt, classifies every raw attempt, and writes summary.json.

Classification (report-level analysis; the raw terminal outcome in each
result document is never rewritten):

  pass                   outcome PASS
  behavioral_failure     outcome FAIL with at least one FAIL check
  setup_failure          outcome FAIL with no FAIL check and at least one
                         MISSING check -> Reporter.abort() or a boot that
                         never reached the first check
  timeout                outcome TIMEOUT (censored elapsed)
  harness_error          the measurement's error_run (protocol stream
                         untrustworthy)

A setup_failure whose retained evidence matches a known infrastructure
signature is additionally tagged `infra_signature`.
"""
import json, os, re, statistics, sys
from pathlib import Path

INFRA_PATTERNS = [
    ("cabal_inplace_race", re.compile(
        r"package\.conf\.inplace|package\.cache|removeDirectoryRecursive|"
        r"cannot create:.*inplace|ghc-pkg", re.I)),
    ("engine_never_ready", re.compile(
        r"never printed READY|exited before READY", re.I)),
    ("port", re.compile(r"address already in use|bind|EADDRINUSE", re.I)),
    ("oom_or_signal", re.compile(
        r"out of memory|Killed|signal 9|heap overflow", re.I)),
]


def evidence_text(run_doc):
    d = run_doc.get("artifact_dir")
    if not d:
        return ""
    p = Path(d)
    chunks = []
    for name in ("stdout.txt", "events.jsonl"):
        f = p / name
        if f.is_file():
            chunks.append(f.read_text(encoding="utf-8", errors="replace"))
    engine = p / "engine"
    if engine.is_dir():
        for f in sorted(engine.rglob("*")):
            if f.is_file() and f.stat().st_size < 4_000_000:
                chunks.append(f.read_text(encoding="utf-8", errors="replace"))
    return "\n".join(chunks)


def classify(run_doc):
    outcome = run_doc["outcome"]
    checks = run_doc.get("checks", {})
    if outcome == "PASS":
        return "pass", []
    if outcome == "TIMEOUT":
        return "timeout", []
    if outcome == "HARNESS_ERROR":
        return "harness_error", []
    failed = [c for c, v in checks.items() if v == "FAIL"]
    missing = [c for c, v in checks.items() if v == "MISSING"]
    if failed:
        return "behavioral_failure", failed
    if missing:
        return "setup_failure", missing
    return "behavioral_failure", []


def infra_tags(run_doc):
    text = evidence_text(run_doc)
    return [name for name, pat in INFRA_PATTERNS if pat.search(text)]


def summarize_elapsed(values):
    if not values:
        return None
    v = sorted(values)
    def pct(q):
        if len(v) == 1:
            return v[0]
        i = q * (len(v) - 1)
        lo, hi = int(i), min(int(i) + 1, len(v) - 1)
        return v[lo] + (v[hi] - v[lo]) * (i - lo)
    return {
        "n": len(v),
        "min": round(v[0], 1),
        "median": round(statistics.median(v), 1),
        "p90": round(pct(0.90), 1),
        "max": round(v[-1], 1),
        "mean": round(statistics.fmean(v), 1),
    }


def parse_kv(path):
    out = {}
    if path.is_file():
        for line in path.read_text(encoding="utf-8").splitlines():
            if "=" in line and not line.startswith("cohort "):
                k, _, val = line.partition("=")
                out[k.strip()] = val.strip()
            elif line.startswith("cohort "):
                for token in line.split()[1:]:
                    k, _, val = token.partition("=")
                    out[k] = val
    return out


def main(root):
    root = Path(root)
    cells = []
    for cell_dir in sorted(root.iterdir()):
        if not cell_dir.is_dir():
            continue
        meta = parse_kv(cell_dir / "cohort.txt")
        exits = {}
        ec = cell_dir / "exit_codes.txt"
        if ec.is_file():
            for line in ec.read_text().splitlines():
                m = re.match(r"inv(\d+) exit=(\d+)", line.strip())
                if m:
                    exits[int(m.group(1))] = int(m.group(2))
        docs = []
        for f in sorted(cell_dir.glob("inv*.json")):
            docs.append((f.name, json.loads(f.read_text(encoding="utf-8"))))
        requested = sum(d["requested_runs"] for _, d in docs)
        counts = {k: 0 for k in ("pass", "behavioral_failure", "setup_failure",
                                 "timeout", "harness_error")}
        elapsed_ok, elapsed_timeout, tags, failed_checks = [], [], {}, {}
        attempts = []
        for name, doc in docs:
            runs = list(doc["runs"])
            if doc.get("error_run"):
                runs.append(doc["error_run"])
            for r in runs:
                kind, detail = classify(r)
                counts[kind] += 1
                if kind == "timeout":
                    elapsed_timeout.append(r["elapsed_seconds"])
                else:
                    elapsed_ok.append(r["elapsed_seconds"])
                if kind == "behavioral_failure":
                    for c in detail:
                        failed_checks[c] = failed_checks.get(c, 0) + 1
                if kind in ("setup_failure", "harness_error", "timeout"):
                    for t in infra_tags(r):
                        tags[t] = tags.get(t, 0) + 1
                attempts.append({
                    "invocation": name, "index": r["index"],
                    "outcome": r["outcome"], "class": kind,
                    "elapsed_seconds": r["elapsed_seconds"],
                    "artifact_dir": r.get("artifact_dir"),
                })
        peaks = [d["peak_concurrency"] for _, d in docs]
        completed = sum(len(d["runs"]) for _, d in docs)
        started = int(meta.get("epoch_start", 0) or 0)
        ended = int(meta.get("epoch_end", 0) or 0)
        cells.append({
            "cell": cell_dir.name,
            "probe": meta.get("probe"),
            "rts_capabilities": int(meta.get("rts_caps", 0) or 0),
            "requested_concurrency": int(meta.get("concurrency", 0) or 0),
            "runs_per_invocation": int(meta.get("runs_per_invocation", 0) or 0),
            "invocations": len(docs),
            "achieved_peak_concurrency_min": min(peaks) if peaks else None,
            "achieved_peak_concurrency_max": max(peaks) if peaks else None,
            "achieved_peak_concurrency_all": peaks,
            "requested_attempts": requested,
            "completed_attempts": completed,
            "counts": counts,
            "failure_rate_incl_timeouts": (
                round((counts["behavioral_failure"] + counts["setup_failure"]
                       + counts["timeout"]) / requested, 4) if requested else None),
            "behavioral_failure_rate": (
                round(counts["behavioral_failure"] / requested, 4)
                if requested else None),
            "failed_checks": failed_checks,
            "infrastructure_signatures": tags,
            "elapsed_noncensored": summarize_elapsed(elapsed_ok),
            "elapsed_timeouts_censored": sorted(round(v, 1) for v in elapsed_timeout),
            "invocation_exit_codes": exits,
            "cohort_wall_seconds": (ended - started) if ended and started else None,
            # An INDEPENDENT achieved-parallelism figure that does not
            # depend on the registry's sampling instants: total probe
            # run time divided by the cohort's own wall clock. A cohort
            # that really ran 8 runs of ~150s in ~175s of wall time was
            # really ~8-way concurrent, whatever any single invocation's
            # three `peak_concurrency` samples happened to observe.
            "mean_achieved_parallelism": (
                round(sum(elapsed_ok + elapsed_timeout) / (ended - started), 2)
                if ended and started and (ended - started) > 0 else None),
            "commit_sha": meta.get("commit"),
            "tree_clean": meta.get("tree_clean"),
            "started_utc": meta.get("started_utc"),
            "finished_utc": meta.get("finished_utc"),
            "attempts": attempts,
        })
    print(render(cells))


def render(cells):
    """Indented JSON, except that each attempt row stays on one line.

    A 168-entry attempt list at `indent=2` is one field per line, which
    reviews badly for no gain. Each row is emitted compactly instead;
    the parsed document is identical either way.
    """
    stash, prepared = [], []
    for cell in cells:
        cell = dict(cell)
        marks = []
        for attempt in cell["attempts"]:
            marks.append(f"\x00{len(stash)}\x00")
            stash.append(attempt)
        cell["attempts"] = marks
        prepared.append(cell)
    text = json.dumps({"cells": prepared}, indent=2, sort_keys=True)
    for i, attempt in enumerate(stash):
        text = text.replace(
            json.dumps(f"\x00{i}\x00"),
            json.dumps(attempt, sort_keys=True, separators=(", ", ": ")))
    return text


if __name__ == "__main__":
    main(sys.argv[1])
