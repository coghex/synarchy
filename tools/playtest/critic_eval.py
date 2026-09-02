"""Real-model evaluation for the playtest critic (#648, extracted by
#2069): the key-holder's acceptance run, opt-in only.

`python3 tools/playtest/critic.py --eval` runs the REAL model
(`critic_model.Critic`) through the production pipeline against the
canned planted-issue trace and asserts the verdicts: the planted silent
failure comes back a missing-feedback defect, the planted missed
feedback comes back intended (or minor discoverability), and every
candidate is adjudicated. Needs ANTHROPIC_API_KEY (or an `ant auth
login` profile) and network access; nothing else in the harness does.
The artifacts are copied to `sessions/critic_eval_last/` for inspection.

The façade imports this module lazily inside its `--eval` branch only.
Consumes the production owners plus the `canned_trace` fixture.
"""
from __future__ import annotations

import json
import os
import shutil
import sys
import tempfile

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from canned_trace import build_canned_trace  # noqa: E402
from critic_evidence import uncovered  # noqa: E402
from critic_model import Critic  # noqa: E402
from critic_pipeline import run_critic  # noqa: E402


def eval_run(model: str, effort: str) -> int:
    """The acceptance run: the REAL model against the canned trace.
    Needs ANTHROPIC_API_KEY (or an `ant auth login` profile)."""
    failures = []

    def check(name, ok, detail=""):
        print(f"  [{'ok' if ok else 'FAIL'}] {name}"
              + (f" — {detail}" if detail else ""))
        if not ok:
            failures.append(name)

    with tempfile.TemporaryDirectory() as tmp:
        tdir = build_canned_trace(os.path.join(tmp, "trace"))
        report_path, findings_path = run_critic(
            tdir, Critic(model=model, effort=effort))
        with open(findings_path) as f:
            data = json.load(f)
        cands = data["candidates"]
        by_cid = {}
        for f_ in data["findings"]:
            for cid in f_["evidence"]["candidate_ids"]:
                by_cid.setdefault(cid, []).append(f_)

        def cid_for_turn(n):
            return next((c["cid"] for c in cands if c["turn"] == n), None)

        f1 = by_cid.get(cid_for_turn(1), [])
        check("planted silent failure flagged as a missing-feedback defect",
              any(f_["verdict"] == "defect"
                  and f_["category"] in ("missing-feedback", "soft-lock")
                  for f_ in f1),
              json.dumps([(f_["category"], f_["verdict"]) for f_ in f1]))
        f3 = by_cid.get(cid_for_turn(3), [])
        check("planted missed-feedback adjudicated intended (or minor "
              "discoverability)",
              any(f_["verdict"] == "intended"
                  or (f_["category"] == "discoverability"
                      and f_["severity"] in ("minor", "polish"))
                  for f_ in f3),
              json.dumps([(f_["category"], f_["verdict"]) for f_ in f3]))
        check("every candidate adjudicated", not uncovered(data, cands))
        print(f"  report: {report_path}")
        # keep the artifacts for inspection
        keep = os.path.join(_HERE, "sessions", "critic_eval_last")
        shutil.rmtree(keep, ignore_errors=True)
        shutil.copytree(tmp, keep)
        print(f"  artifacts copied to {keep}")

    if failures:
        print(f"critic eval: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("critic eval: all checks passed")
    return 0
