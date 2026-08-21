#!/usr/bin/env python3
"""Token accounting and the local aggregate usage ledger for playtests.

Provider-specific token payloads are normalized by ``agent.py``.  This module
keeps the runner's policy deliberately simple: player cost is input plus output
tokens, displayed with compact K/M/G suffixes.  Cached input remains input; it
is not added a second time when a provider's ``input_tokens`` already includes
it.
"""
from __future__ import annotations

import argparse
import datetime as dt
import fcntl
import glob
import json
import os
import subprocess
import tempfile


def _token_int(value) -> int:
    try:
        return max(0, int(value or 0))
    except (TypeError, ValueError):
        return 0


def usage_total(usage: dict | None) -> int | None:
    """Return input+output, or None when a provider reported no usage."""
    if not isinstance(usage, dict):
        return None
    if usage.get("input_tokens") is None and usage.get("output_tokens") is None:
        return None
    return _token_int(usage.get("input_tokens")) + _token_int(
        usage.get("output_tokens"))


def compact_tokens(value: int | None) -> str:
    if value is None:
        return "—"
    n = max(0, int(value))
    units = ((1_000_000_000, "G"), (1_000_000, "M"), (1_000, "K"))
    for scale, suffix in units:
        if n >= scale:
            rendered = f"{n / scale:.1f}".rstrip("0").rstrip(".")
            return rendered + suffix
    return str(n)


def default_usage_log(repo_cwd: str) -> str | None:
    """Resolve the shared-Git local ledger used by coordinated test runs."""
    try:
        result = subprocess.run(
            ["git", "rev-parse", "--path-format=absolute", "--git-common-dir"],
            cwd=repo_cwd, text=True, capture_output=True, check=False)
    except OSError:
        return None
    if result.returncode != 0 or not result.stdout.strip():
        return None
    return os.path.join(result.stdout.strip(), "codex-test", "playtest-usage.md")


def _row(meta_path: str, ledger_path: str) -> dict | None:
    try:
        with open(meta_path, encoding="utf-8") as f:
            meta = json.load(f)
    except (OSError, ValueError):
        return None
    totals = meta.get("usage_totals")
    if not isinstance(totals, dict) or usage_total(totals) is None:
        # Traces written before harness 0.2 stored usage only per turn. Fold
        # those records so the first ledger rebuild includes historical runs.
        input_tokens = 0
        output_tokens = 0
        found = False
        try:
            with open(os.path.join(os.path.dirname(meta_path), "turns.jsonl"),
                      encoding="utf-8") as f:
                for line in f:
                    if not line.strip():
                        continue
                    turn = json.loads(line)
                    usage = ((turn.get("player") or {}).get("usage")
                             if isinstance(turn, dict) else None)
                    if usage_total(usage) is None:
                        continue
                    found = True
                    input_tokens += _token_int(usage.get("input_tokens"))
                    output_tokens += _token_int(usage.get("output_tokens"))
        except (OSError, ValueError):
            found = False
        if not found:
            return None
        totals = {"input_tokens": input_tokens, "output_tokens": output_tokens}
    player = meta.get("player_model") or {}
    stamp = meta.get("ended_at") or meta.get("started_at")
    try:
        date = dt.datetime.fromtimestamp(float(stamp)).astimezone().strftime("%Y-%m-%d")
    except (TypeError, ValueError, OSError):
        date = "unknown"
    ledger_dir = os.path.dirname(os.path.abspath(ledger_path))
    trace_dir = os.path.dirname(os.path.abspath(meta_path))
    try:
        run = os.path.relpath(trace_dir, ledger_dir)
    except ValueError:
        run = trace_dir
    return {
        "date": date,
        "run": run,
        "backend": player.get("backend") or "unknown",
        "model": player.get("model") or "unknown",
        "effort": player.get("effort") or "—",
        "turns": _token_int(meta.get("turns")),
        "stop": meta.get("stop_reason") or "unfinished",
        "tokens": usage_total(totals) or 0,
        "budget": meta.get("player_token_budget"),
        "ended_at": float(stamp or 0),
    }


def update_usage_log(ledger_path: str, artifacts_root: str,
                     extra_trace_dir: str | None = None) -> None:
    """Rebuild the Markdown ledger atomically from durable trace metadata."""
    ledger_path = os.path.abspath(ledger_path)
    os.makedirs(os.path.dirname(ledger_path), exist_ok=True)
    lock_path = ledger_path + ".lock"
    with open(lock_path, "a+", encoding="utf-8") as lock:
        fcntl.flock(lock.fileno(), fcntl.LOCK_EX)
        paths = set(glob.glob(os.path.join(os.path.abspath(artifacts_root),
                                           "**", "meta.json"), recursive=True))
        if extra_trace_dir:
            paths.add(os.path.join(os.path.abspath(extra_trace_dir), "meta.json"))
        rows = [r for p in paths if (r := _row(p, ledger_path)) is not None]
        rows.sort(key=lambda r: (r["ended_at"], r["run"]))
        total = sum(r["tokens"] for r in rows)
        lines = [
            "# Playtest usage",
            "",
            "Local, unversioned accounting for naive-player model calls. Token "
            "values are provider-reported input plus output; account-plan "
            "remaining is not exposed by the noninteractive CLIs.",
            "",
            f"Runs: {len(rows)} · Player tokens: {compact_tokens(total)}",
            "",
            "| Date | Run | Player | Effort | Turns | Stop | Tokens | Budget |",
            "|---|---|---|---:|---:|---|---:|---:|",
        ]
        for r in rows:
            player = f'{r["backend"]} / {r["model"]}'
            run = str(r["run"]).replace("|", "\\|")
            stop = str(r["stop"]).replace("|", "\\|")
            lines.append(
                f'| {r["date"]} | `{run}` | {player} | {r["effort"]} | '
                f'{r["turns"]} | {stop} | {compact_tokens(r["tokens"])} | '
                f'{compact_tokens(r["budget"])} |')
        lines.append("")
        fd, tmp_path = tempfile.mkstemp(
            prefix=".playtest-usage.", dir=os.path.dirname(ledger_path), text=True)
        try:
            with os.fdopen(fd, "w", encoding="utf-8") as f:
                f.write("\n".join(lines))
            os.replace(tmp_path, ledger_path)
        finally:
            if os.path.exists(tmp_path):
                os.unlink(tmp_path)


def selftest() -> int:
    failures = []

    def check(name, condition):
        print(f"  [{'ok' if condition else 'FAIL'}] {name}")
        if not condition:
            failures.append(name)

    check("compact token suffixes",
          [compact_tokens(n) for n in
           (999, 1_000, 1_500, 4_500_000, 2_000_000_000)]
          == ["999", "1K", "1.5K", "4.5M", "2G"])
    check("usage is input plus output", usage_total(
        {"input_tokens": 1200, "output_tokens": 34,
         "cache_read_input_tokens": 900}) == 1234)
    with tempfile.TemporaryDirectory() as tmp:
        artifacts = os.path.join(tmp, "artifacts")
        trace = os.path.join(artifacts, "run-one")
        os.makedirs(trace)
        with open(os.path.join(trace, "meta.json"), "w", encoding="utf-8") as f:
            json.dump({
                "started_at": 1_700_000_000, "ended_at": 1_700_000_010,
                "turns": 2, "stop_reason": "turn_budget_exhausted",
                "player_token_budget": 200_000,
                "player_model": {"backend": "codex-cli", "model": "luna",
                                 "effort": "medium"},
                "usage_totals": {"input_tokens": 1400, "output_tokens": 100},
            }, f)
        ledger = os.path.join(tmp, "playtest-usage.md")
        update_usage_log(ledger, artifacts)
        with open(ledger, encoding="utf-8") as f:
            text = f.read()
        check("ledger collates trace metadata",
              "run-one" in text and "1.5K" in text and "200K" in text)
        legacy = os.path.join(artifacts, "legacy-run")
        os.makedirs(legacy)
        with open(os.path.join(legacy, "meta.json"), "w", encoding="utf-8") as f:
            json.dump({
                "started_at": 1_700_000_020, "turns": 1,
                "stop_reason": "turn_budget_exhausted",
                "player_model": {"backend": "codex-cli", "model": "luna",
                                 "effort": "medium"},
            }, f)
        with open(os.path.join(legacy, "turns.jsonl"), "w", encoding="utf-8") as f:
            f.write(json.dumps({"player": {"usage": {
                "input_tokens": 2100, "output_tokens": 50}}}) + "\n")
        update_usage_log(ledger, artifacts)
        with open(ledger, encoding="utf-8") as f:
            text = f.read()
        check("ledger folds pre-0.2 per-turn usage",
              "legacy-run" in text and "2.1K" in text)
    if failures:
        print(f"usage selftest: FAILED ({len(failures)})")
        return 1
    print("usage selftest: all checks passed")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--selftest", action="store_true")
    ap.add_argument("--artifacts-root")
    ap.add_argument("--out")
    args = ap.parse_args()
    if args.selftest:
        return selftest()
    if not args.artifacts_root or not args.out:
        ap.error("--artifacts-root and --out are required")
    update_usage_log(args.out, args.artifacts_root)
    print(f"playtest usage: wrote {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
