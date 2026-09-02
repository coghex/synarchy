"""Pipeline orchestration for the playtest critic (#648, extracted
by #2069): one `run_critic` composing analysis, transport and validation.

Loads the trace's metadata and turns ONCE, builds the signals,
candidates, digest and batches once, performs the initial batched
adjudication (every candidate's own frames in the call that judges it),
runs at most one bounded repair pass for anything left unadjudicated or
conflictingly adjudicated, validates and reconciles, attaches only the
screenshots each finding's call actually saw, and writes a matching
`findings.json` + `report.md` — the candidate list and the per-call
audit (`adjudication_calls`) embedded so nothing is silently dropped.

The `critic` argument is duck-typed: anything with an
`adjudicate(digest, manual, frames, ask=None)` method, so the production
`critic_model.Critic` and the self-test's deterministic fakes share this
one path. Consumes `trace`, `critic_contract`, `critic_signals` and
`critic_evidence`; it does not import the model owner.
"""
from __future__ import annotations

import json
import os
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from critic_contract import DEFAULT_MAX_FRAMES  # noqa: E402
from critic_evidence import (ValidationCtx, assign_ids, render_report,  # noqa: E402
                             uncovered, validate_findings)
from critic_signals import (build_digest, build_signals,  # noqa: E402
                            friction_candidates, plan_batches)
from trace import load_meta, load_turns  # noqa: E402


def run_critic(trace_dir: str, critic, manual_path: str | None = None,
               out_dir: str | None = None,
               max_frames: int = DEFAULT_MAX_FRAMES) -> tuple[str, str]:
    meta = load_meta(trace_dir)
    turns = load_turns(trace_dir)
    signals = build_signals(trace_dir, turns)
    candidates = friction_candidates(meta, signals)
    digest = build_digest(meta, signals, candidates)
    batches, frame_warnings = plan_batches(trace_dir, turns, candidates,
                                           max_frames)

    manual = ""
    manual_path = manual_path or meta.get("manual_path") or os.path.join(
        os.path.dirname(os.path.dirname(_HERE)), "docs", "player_manual.md")
    try:
        with open(manual_path, encoding="utf-8") as f:
            manual = f.read()
    except OSError:
        manual = "(manual unavailable)"

    # Adjudicate in batches so every candidate's own screenshot is in
    # the call that judges it (finding 3): each call carries the full
    # digest, its batch's frames, and an explicit only-these-ids ask.
    audit_calls: list[dict] = []
    findings: list[dict] = []
    summary = None

    def one_call(subset, frames, ask):
        call_no = len(audit_calls) + 1
        audit_calls.append({"call": call_no,
                            "candidate_ids": [c["cid"] for c in subset],
                            "frames": [n for n, _ in frames]})
        result = critic.adjudicate(digest, manual, frames, ask=ask)
        for f in result.get("findings") or []:
            if isinstance(f, dict):
                f["adjudication_call"] = call_no
                findings.append(f)
        return result.get("summary") or ""

    for subset, frames in batches:
        ask = None
        if len(batches) > 1:
            ask = ("This pass adjudicates ONLY these candidate ids: "
                   + (", ".join(c["cid"] for c in subset) or "(none)")
                   + ". The other candidates are handled in separate passes "
                   "with their own screenshots — do not emit findings for "
                   "them here.")
        s = one_call(subset, frames, ask)
        if summary is None:
            summary = s

    data = {"summary": summary or "", "findings": findings}
    ctx = ValidationCtx(candidates, turns, audit_calls)
    data, warnings = validate_findings(data, candidates, ctx)

    # one bounded repair pass for anything left unadjudicated (or
    # covered only by findings that failed the evidence discipline) —
    # WITH those candidates' own frames
    missing = uncovered(data, candidates)
    if missing:
        repair_batches, _ = plan_batches(trace_dir, turns, missing, max_frames)
        try:
            for subset, frames in repair_batches:
                ask = ("These candidate ids were left unadjudicated (or "
                       "covered only by findings that failed the evidence "
                       "discipline): "
                       + ", ".join(c["cid"] for c in subset)
                       + ". Produce findings covering ONLY these ids now, "
                       "each citing its turn(s), the player's own words when "
                       "a note exists, and the grounding oracle record "
                       "(same schema; earlier findings are already recorded).")
                one_call(subset, frames, ask)
            data = {"summary": data["summary"], "findings": findings}
            ctx = ValidationCtx(candidates, turns, audit_calls)
            data, warnings = validate_findings(data, candidates, ctx)
        except Exception as e:  # keep the report; warn honestly
            warnings.append(f"repair pass failed: {e}")
    warnings = frame_warnings + warnings

    assign_ids(data["findings"])
    turns_by_n = {t.get("turn"): t for t in turns}
    frames_by_call = {a["call"]: set(a["frames"]) for a in audit_calls}
    for f in data["findings"]:
        # attach only screenshots the model was actually shown in the
        # call that produced this finding — a report must never imply
        # the critic saw a frame it didn't (review finding 3). A cited
        # turn can contribute up to two: its own pre-step screenshot
        # (key `n`) and, when it was shown, that SAME turn's own
        # post-step frame (#775, key `-n` — see critic_signals.plan_batches) rather
        # than borrowing a following turn's.
        shown = frames_by_call.get(f.get("adjudication_call"), set())
        refs = []
        for n in f["evidence"]["turns"]:
            t = turns_by_n.get(n) or {}
            if n in shown and t.get("screenshot"):
                refs.append(t["screenshot"])
            if -n in shown:
                post_ref = (t.get("oracle") or {}).get("post_screenshot")
                if post_ref:
                    refs.append(post_ref)
        f["screenshots"] = refs
        f["evidence"]["screenshots"] = f["screenshots"]
    data["critic_model"] = getattr(critic, "model", "fake")
    data["candidates"] = candidates  # the full pre-analysis, for audit
    data["adjudication_calls"] = audit_calls  # who saw which frames

    out_dir = out_dir or trace_dir
    os.makedirs(out_dir, exist_ok=True)
    findings_path = os.path.join(out_dir, "findings.json")
    with open(findings_path, "w") as f:
        json.dump(data, f, indent=2, sort_keys=True)
        f.write("\n")
    report_path = os.path.join(out_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(render_report(meta, data, warnings, turns))
    return report_path, findings_path
