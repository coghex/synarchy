#!/usr/bin/env python3
"""H2 critic (#648): oracle-grounded friction triage over an H1 trace.

Consumes a session-trace directory OFFLINE (never drives the game,
never modifies code) and emits a UX report: `report.md`
(human-readable) + `findings.json` (machine-readable). The player (H1)
reported ALL friction naively; this is where a strong multimodal
model, armed with the ground truth the player never saw, adjudicates
every friction point as **defect** or **intended** — both buckets kept,
nothing silently dropped, every finding grounded in specific turns +
oracle records.

Pipeline:
  1. deterministic pre-analysis (pure Python, LLM-free, unit-testable):
     per-turn signals + the cross-source joins from the foundation
     issues — outcome rejected/noop/deadclick + no event + no visual
     change => silent-failure candidate; deadclick/no-widget-at-click
     => phantom-affordance candidate; player-claims-nothing-happened
     while the oracle shows feedback => feedback-was-shown; stuck
     loops; crash. Every candidate gets a stable id the critic MUST
     adjudicate.
  2. one multimodal LLM call: the session digest + the player manual
     (the intended mental model) + screenshots of the friction turns,
     with a structured-output findings schema.
  3. validation (enums, candidate coverage, one verdict per candidate
     — one bounded repair pass for anything unadjudicated or
     conflictingly adjudicated) and deterministic rendering, so
     report.md and findings.json always agree.

Usage:
  python3 tools/playtest/critic.py <trace_dir>            # writes into the trace dir
  python3 tools/playtest/critic.py <trace_dir> --out DIR --model claude-opus-5
  python3 tools/playtest/critic.py --selftest             # offline, no API key
  python3 tools/playtest/critic.py --eval                 # canned trace + REAL model
                                                          # (needs ANTHROPIC_API_KEY)
"""
from __future__ import annotations

import argparse
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.dirname(HERE))

# The implementation lives in one module per ownership boundary (#2069);
# this file is the documented command and performs CLI dispatch only.
# See tools/playtest/README.md ("The critic") for the ownership table.
from critic_contract import (DEFAULT_EFFORT, DEFAULT_MAX_FRAMES,  # noqa: E402
                             DEFAULT_MAX_TOKENS, DEFAULT_MODEL)
from critic_model import Critic  # noqa: E402
from critic_pipeline import run_critic  # noqa: E402


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("trace_dir", nargs="?", help="H1 session-trace directory")
    ap.add_argument("--model", default=DEFAULT_MODEL)
    ap.add_argument("--effort", default=DEFAULT_EFFORT,
                    choices=["low", "medium", "high"])
    ap.add_argument("--max-tokens", type=int, default=DEFAULT_MAX_TOKENS)
    ap.add_argument("--max-frames", type=int, default=DEFAULT_MAX_FRAMES,
                    help="screenshot budget for the multimodal call")
    ap.add_argument("--manual", default=None,
                    help="player manual path (default: the trace's, then C1)")
    ap.add_argument("--out", default=None,
                    help="output dir (default: the trace dir itself)")
    ap.add_argument("--selftest", action="store_true",
                    help="offline pipeline check (no API key)")
    ap.add_argument("--eval", action="store_true",
                    help="run the REAL model against the canned planted-issue "
                         "trace and assert the verdicts (needs an API key)")
    args = ap.parse_args()

    if args.selftest:
        # test support stays out of a production run's import graph:
        # the fixture and the fake critics load only on this branch
        from critic_selftest import selftest
        return selftest()
    if args.eval:
        from critic_eval import eval_run
        return eval_run(args.model, args.effort)
    if not args.trace_dir:
        ap.error("trace_dir required (or --selftest / --eval)")
    critic = Critic(model=args.model, effort=args.effort,
                    max_tokens=args.max_tokens)
    report_path, findings_path = run_critic(
        args.trace_dir, critic, manual_path=args.manual,
        out_dir=args.out, max_frames=args.max_frames)
    print(f"critic: wrote {report_path}")
    print(f"critic: wrote {findings_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
