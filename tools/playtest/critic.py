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
  3. validation (enums, candidate coverage — one bounded repair pass
     for anything unadjudicated) and deterministic rendering, so
     report.md and findings.json always agree.

Usage:
  python3 tools/playtest/critic.py <trace_dir>            # writes into the trace dir
  python3 tools/playtest/critic.py <trace_dir> --out DIR --model claude-opus-4-8
  python3 tools/playtest/critic.py --selftest             # offline, no API key
  python3 tools/playtest/critic.py --eval                 # canned trace + REAL model
                                                          # (needs ANTHROPIC_API_KEY)
"""
from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.dirname(HERE))

from trace import load_meta, load_turns  # noqa: E402

DEFAULT_MODEL = "claude-opus-4-8"   # strong + multimodal; runs once per session
DEFAULT_EFFORT = "high"
DEFAULT_MAX_TOKENS = 16000
DEFAULT_MAX_FRAMES = 8

CATEGORIES = ("soft-lock", "missing-feedback", "phantom-affordance",
              "misleading-affordance", "discoverability", "doc-mismatch",
              "crash", "other")
SEVERITIES = ("blocker", "major", "minor", "polish")
VERDICTS = ("defect", "intended")
CONFIDENCES = ("high", "medium", "low")

FINDINGS_SCHEMA = {
    "type": "object",
    "properties": {
        "summary": {"type": "string",
                    "description": "3-6 sentence session-level summary."},
        "findings": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "title": {"type": "string"},
                    "category": {"type": "string", "enum": list(CATEGORIES)},
                    "severity": {"type": "string", "enum": list(SEVERITIES)},
                    "verdict": {"type": "string", "enum": list(VERDICTS)},
                    "confidence": {"type": "string", "enum": list(CONFIDENCES)},
                    "evidence": {
                        "type": "object",
                        "properties": {
                            "turns": {"type": "array", "items": {"type": "integer"}},
                            "candidate_ids": {"type": "array",
                                              "items": {"type": "string"}},
                            "player_quote": {"type": "string"},
                            "oracle": {"type": "string",
                                       "description": "The specific oracle record(s) grounding this."},
                        },
                        "required": ["turns", "candidate_ids", "player_quote",
                                     "oracle"],
                        "additionalProperties": False,
                    },
                    "root_cause_hypothesis": {"type": "string"},
                },
                "required": ["title", "category", "severity", "verdict",
                             "confidence", "evidence", "root_cause_hypothesis"],
                "additionalProperties": False,
            },
        },
    },
    "required": ["summary", "findings"],
    "additionalProperties": False,
}

SYSTEM_PROMPT = """\
You are the critic in a naive-player UX playtest harness. A cheap, \
deliberately naive LLM "player" played the game seeing only \
screenshots and narrated its experience; you are the sharp analyst \
with the ground truth it never had. Your job: walk EVERY friction \
point, decide what is a real defect versus the player just being \
naive, and write it up so a maintainer can act on it.

You receive:
- the minimal player manual the player was given (the INTENDED mental \
model — a mismatch between it and observed behavior is a doc-mismatch \
or a manual gap);
- a per-turn session digest: the player's observation / action / \
EXPECTATION / note, the exact injected inputs, and the ORACLE the \
player never saw (widget dump with bounds+labels, event-log deltas, \
action-outcome records where available, pause/menu state), plus \
harness-computed signals and joins;
- screenshots of the friction turns (you must actually look at them \
to judge visual clarity and whether feedback was visible);
- a list of FRICTION CANDIDATES with ids (C1, C2, ...). You MUST \
adjudicate every candidate id in exactly the findings that cover it — \
none may be left uncovered.

Categories: soft-lock, missing-feedback, phantom-affordance, \
misleading-affordance, discoverability, doc-mismatch, crash, other.
Severities: blocker, major, minor, polish.
Verdicts: "defect" (grounded in oracle evidence) or "intended" \
(working as designed; the player was naive — say WHY, citing the \
oracle, e.g. "feedback WAS shown: the event log has 'Notes saved' and \
the frame changed; the player missed it". A missed-but-present cue \
may still deserve a minor discoverability finding — judge it).

The canonical joins (use them, don't parrot them):
- outcome rejected/noop/deadclick + no user-facing event + no visible \
frame change => missing-feedback / silent failure (defect).
- deadclick where the player treated a spot as interactive + no \
widget at that point => phantom-affordance. If a widget IS there but \
the player couldn't find/use it => discoverability.
- a stuck loop (same action, no change, repeatedly) => strong \
missing-feedback signal.
- the player followed the manual and still got stuck => doc-mismatch \
or manual gap.
- player expectation != oracle reality on a SUCCESSFUL action => \
misleading affordance/label.
- an engine crash in the trace => crash (blocker).

Evidence discipline — this is what makes the report trustworthy:
- every finding cites specific turn numbers, the player's own words, \
and the oracle record that grounds it;
- NO ungrounded findings: a hunch without oracle backing is either \
dropped or explicitly confidence=low with the gap named;
- when outcome records are absent (older traces), reason from events \
+ frames + widgets and lower your confidence accordingly.
- merge duplicates: one finding may cover several candidates (e.g. a \
click and its stuck repeats).

Output exactly the JSON schema you were given. Findings should be \
few and sharp, not exhaustive prose.
"""


# ---------------------------------------------------------------------------
# 1. Deterministic pre-analysis
# ---------------------------------------------------------------------------

def _file_hash(path: str) -> str | None:
    try:
        with open(path, "rb") as f:
            return hashlib.sha256(f.read()).hexdigest()
    except OSError:
        return None


def widget_at(widgets, x, y):
    """The first visible widget whose bounds contain (x, y) — the F3
    join used to tell phantom-affordance from discoverability."""
    if not isinstance(widgets, list):
        return None
    for w in widgets:
        b = w.get("bounds") if isinstance(w, dict) else None
        if not isinstance(b, dict):
            continue
        try:
            if (b["x"] <= x <= b["x"] + b["w"]
                    and b["y"] <= y <= b["y"] + b["h"]):
                return w
        except (KeyError, TypeError):
            continue
    return None


BAD_OUTCOMES = ("rejected", "noop", "deadclick", "partial")


def build_signals(trace_dir: str, turns: list[dict]) -> list[dict]:
    """Per-turn derived facts, all deterministic."""
    hashes = [_file_hash(os.path.join(trace_dir, t.get("screenshot", "")))
              for t in turns]
    signals = []
    for i, t in enumerate(turns):
        player = t.get("player") or {}
        action = player.get("action") or {}
        oracle = t.get("oracle") or {}
        events = oracle.get("event_log_new") or []
        outcomes = oracle.get("outcomes") or []
        acks = t.get("acks") or []
        ack_errors = [a for a in acks
                      if isinstance(a, dict) and "error" in a]
        # the action's visible effect shows up in the NEXT frame
        changed = (hashes[i + 1] != hashes[i]) if i + 1 < len(turns) else None
        clicked = None
        if action.get("do") in ("click", "drag") and action.get("x") is not None:
            clicked = widget_at(oracle.get("widgets"),
                                float(action.get("x", -1)),
                                float(action.get("y", -1)))
        bad = [o for o in outcomes
               if isinstance(o, dict) and o.get("outcome") in BAD_OUTCOMES]
        widgets = oracle.get("widgets")
        signals.append({
            "turn": t.get("turn", i + 1),
            "observation": (player.get("observation") or "").strip(),
            "note": (player.get("note") or "").strip(),
            "expectation": (player.get("expectation") or "").strip(),
            "action": action,
            "injected": t.get("injected") or [],
            "acks": acks,
            "events": events,
            "outcomes": outcomes,
            "bad_outcomes": bad,
            "ack_errors": ack_errors,
            "visual_change_next": changed,
            "clicked_widget": clicked,
            "widgets": widgets if isinstance(widgets, list) else [],
            "current_menu": oracle.get("current_menu"),
            "paused": oracle.get("paused"),
            "stuck": bool(t.get("stuck")),
        })
    return signals


def friction_candidates(meta: dict, signals: list[dict]) -> list[dict]:
    """Everything the critic must adjudicate, with the joins pre-tagged
    as machine hints (the model judges; these steer and ground it)."""
    cands = []

    def add(turn, reasons, oracle_excerpt):
        cands.append({"cid": f"C{len(cands) + 1}", "turn": turn,
                      "reasons": reasons, "oracle": oracle_excerpt})

    for s in signals:
        reasons = []
        # What feedback signals existed this turn — CONTEXT for the
        # critic, never a gate. In a live unpaused world the frame
        # almost always changes and unrelated events fire, so gating a
        # bad outcome on "no feedback anywhere" would suppress nearly
        # every real silent failure (the false-negative class flagged
        # in review). The critic judges whether the feedback actually
        # correlated with the action.
        feedback_bits = []
        if s["events"]:
            feedback_bits.append(f"events fired: {json.dumps(s['events'])}")
        if s["visual_change_next"]:
            feedback_bits.append("the next frame differs visibly")
        feedback = ("; ".join(feedback_bits)
                    if feedback_bits else "NO feedback of any kind "
                    "(no events, next frame byte-identical)")
        if s["bad_outcomes"]:
            kind = s["bad_outcomes"][0].get("outcome")
            if not feedback_bits:
                reasons.append(f"silent-failure-join: action outcome {kind!r} "
                               "with no user-facing event and no visible "
                               "frame change")
            else:
                reasons.append(f"bad-outcome-join: action outcome {kind!r} "
                               f"({s['bad_outcomes'][0].get('reason', 'no reason')}); "
                               f"{feedback} — judge whether that feedback "
                               "actually informed the player about THIS action "
                               "or was unrelated (a silent failure can hide "
                               "behind unrelated noise)")
        if s["action"].get("do") == "click" and s["clicked_widget"] is None:
            reasons.append("phantom-affordance-join: the click hit no widget "
                           f"(per the F3 dump); {feedback}")
        if s["note"] and ("noth" in s["note"].lower()
                          or "broken" in s["note"].lower()
                          or "work" in s["note"].lower()) \
                and feedback_bits:
            reasons.append("feedback-was-shown-join: the player claims no "
                           f"effect, but the oracle shows: {feedback}")
        if s["stuck"]:
            reasons.append("stuck-loop: same action with no visible change, "
                           "repeatedly — strong missing-feedback signal")
        if s["ack_errors"]:
            reasons.append(f"input rejected by the engine: {s['ack_errors']}")
        if s["note"] and not reasons:
            reasons.append("player-reported friction (note)")
        if reasons:
            add(s["turn"], reasons, {
                "events": s["events"], "outcomes": s["outcomes"],
                "clicked_widget": s["clicked_widget"],
                "visual_change_next": s["visual_change_next"],
            })

    if meta.get("stop_reason") == "engine_crash":
        add(meta.get("turns", 0) or 0,
            ["engine crash ended the session (crash/blocker)"],
            {"crash_detail": meta.get("crash_detail"),
             "engine_log_tail": (meta.get("engine_log_tail") or "")[-1500:]})
    return cands


def build_digest(meta: dict, signals: list[dict],
                 candidates: list[dict]) -> str:
    persona = meta.get("persona") or {}
    lines = [
        "SESSION",
        f"  persona: {persona.get('name')} — goal: {persona.get('goal')}",
        f"  stop_reason: {meta.get('stop_reason')}  turns: {meta.get('turns')}"
        f"  world_seed: {meta.get('world_seed')}",
        "",
        "TURNS (player fields are what the naive player wrote; oracle/"
        "signals are ground truth it never saw)",
    ]
    prev_widgets_key = None
    for s in signals:
        lines.append(f"turn {s['turn']}:")
        if s["observation"]:
            lines.append(f"  observation: {s['observation']}")
        lines.append(f"  action: {json.dumps(s['action'], sort_keys=True)}")
        if s["expectation"]:
            lines.append(f"  expectation: {s['expectation']}")
        if s["note"]:
            lines.append(f"  note: {s['note']}")
        if s["injected"]:
            lines.append(f"  injected: {json.dumps(s['injected'])}")
            lines.append(f"  acks: {json.dumps(s['acks'])}")
        # Full F3 widget dump (compact fields), deduped when identical
        # to the previous turn's so long sessions stay readable.
        compact = [{k: w.get(k) for k in
                    ("id", "label", "type", "bounds", "enabled", "visible")}
                   for w in s["widgets"] if isinstance(w, dict)]
        key = json.dumps(compact, sort_keys=True)
        if key == prev_widgets_key:
            lines.append("  widgets: (unchanged from previous turn)")
        else:
            lines.append(f"  widgets: {json.dumps(compact, sort_keys=True)}")
            prev_widgets_key = key
        lines.append(f"  oracle: menu={s['current_menu']!r} paused={s['paused']} "
                     f"events={json.dumps(s['events'])} "
                     f"outcomes={json.dumps(s['outcomes'])} "
                     f"clicked_widget={json.dumps(s['clicked_widget'])} "
                     f"visual_change_next={s['visual_change_next']} "
                     f"stuck={s['stuck']}")
    lines.append("")
    lines.append("FRICTION CANDIDATES (adjudicate every id)")
    for c in candidates:
        lines.append(f"{c['cid']} (turn {c['turn']}):")
        for r in c["reasons"]:
            lines.append(f"  - {r}")
    if not candidates:
        lines.append("(none — a clean session; say so in the summary)")
    return "\n".join(lines)


def select_frames(trace_dir: str, turns: list[dict], candidates: list[dict],
                  max_frames: int) -> tuple[list[tuple[int, str]], list[str]]:
    """Screenshots the critic actually looks at. Priority order keeps
    the evidence honest under the budget: (1) every candidate's OWN
    turn — each adjudicated friction point should be seen — then
    (2) each candidate's next turn (to judge visible effects), then
    (3) the session bookends. Returns (frames, warnings); a candidate
    whose own frame didn't fit the budget is warned about explicitly,
    so the report never silently pretends the critic saw it."""
    by_turn = {t.get("turn"): t for t in turns}

    def path_of(n):
        t = by_turn.get(n)
        if not t:
            return None
        p = os.path.join(trace_dir, t.get("screenshot", ""))
        return p if os.path.isfile(p) else None

    passes = [
        [(c["cid"], c["turn"]) for c in candidates],            # own turns
        [(None, c["turn"] + 1) for c in candidates],            # effects
        [(None, turns[0].get("turn", 1)) if turns else (None, 0),
         (None, turns[-1].get("turn", len(turns))) if turns else (None, 0)],
    ]
    out: list[tuple[int, str]] = []
    seen: set[int] = set()
    unseen_cands: list[str] = []
    for p, wants in enumerate(passes):
        for cid, n in wants:
            if n in seen:
                continue
            path = path_of(n)
            if path is None:
                continue
            if len(out) >= max_frames:
                if p == 0 and cid:
                    unseen_cands.append(cid)
                continue
            seen.add(n)
            out.append((n, path))
    out.sort(key=lambda pair: pair[0])
    warnings = []
    if unseen_cands:
        warnings.append(
            "frame budget (--max-frames) exhausted: the critic did NOT see "
            "screenshots for candidate(s) " + ", ".join(unseen_cands)
            + " — treat their visual judgments as lower-confidence")
    return out, warnings


# ---------------------------------------------------------------------------
# 2. The critic model
# ---------------------------------------------------------------------------

class Critic:
    """One multimodal, oracle-armed adjudication call."""

    def __init__(self, model: str = DEFAULT_MODEL, effort: str = DEFAULT_EFFORT,
                 max_tokens: int = DEFAULT_MAX_TOKENS):
        try:
            import anthropic
        except ImportError as e:
            raise SystemExit(
                "the critic needs the Anthropic SDK: pip install anthropic\n"
                "(--selftest doesn't)") from e
        self._anthropic = anthropic
        self.client = anthropic.Anthropic()
        self.model = model
        self.effort = effort
        self.max_tokens = max_tokens

    def adjudicate(self, digest: str, manual: str,
                   frames: list[tuple[int, str]],
                   ask: str | None = None) -> dict:
        content = []
        for n, path in frames:
            with open(path, "rb") as f:
                data = base64.standard_b64encode(f.read()).decode()
            content.append({"type": "text", "text": f"[screenshot of turn {n}]"})
            content.append({"type": "image",
                            "source": {"type": "base64",
                                       "media_type": "image/png", "data": data}})
        content.append({"type": "text", "text":
                        "THE PLAYER MANUAL (the intended mental model)\n---\n"
                        + manual + "\n---\n\n" + digest
                        + "\n\n" + (ask or "Produce the findings JSON now.")})
        response = self.client.messages.create(
            model=self.model,
            max_tokens=self.max_tokens,
            thinking={"type": "adaptive"},
            output_config={"effort": self.effort,
                           "format": {"type": "json_schema",
                                      "schema": FINDINGS_SCHEMA}},
            system=[{"type": "text", "text": SYSTEM_PROMPT,
                     "cache_control": {"type": "ephemeral"}}],
            messages=[{"role": "user", "content": content}],
        )
        text = next((b.text for b in response.content if b.type == "text"), "")
        return json.loads(text)


class FakeCritic:
    """Deterministic stand-in for --selftest: adjudicates mechanically
    from the pre-tagged joins so the full pipeline (digest, coverage
    validation, writers) runs with no API key. Not a substitute for
    --eval, which runs the real model on the canned trace."""

    def adjudicate(self, digest: str, manual: str, frames, ask=None) -> dict:
        findings = []
        # parse candidate blocks back out of the digest
        cur = None
        for line in digest.splitlines():
            if line.startswith("C") and "(turn " in line:
                cid, rest = line.split(" ", 1)
                cur = (cid.rstrip(":"), int(rest.split("turn ")[1].split(")")[0]))
            elif cur and line.strip().startswith("- "):
                reason = line.strip()[2:]
                cid, turn = cur
                if reason.startswith("silent-failure-join"):
                    findings.append(self._mk(
                        "Silent failure", "missing-feedback", "major", "defect",
                        cid, turn, reason))
                elif reason.startswith("bad-outcome-join"):
                    findings.append(self._mk(
                        "Rejection masked by unrelated feedback",
                        "missing-feedback", "major", "defect",
                        cid, turn, reason))
                elif reason.startswith("phantom-affordance-join"):
                    findings.append(self._mk(
                        "Phantom affordance", "phantom-affordance", "minor",
                        "defect", cid, turn, reason))
                elif reason.startswith("feedback-was-shown-join"):
                    findings.append(self._mk(
                        "Feedback shown but missed", "discoverability", "minor",
                        "intended", cid, turn, reason))
                elif reason.startswith("engine crash"):
                    findings.append(self._mk(
                        "Engine crash", "crash", "blocker", "defect",
                        cid, turn, reason))
                else:
                    findings.append(self._mk(
                        "Player friction", "other", "polish", "intended",
                        cid, turn, reason))
                cur = (cid, turn)
        # merge multiple findings for the same cid into the first
        merged: dict[str, dict] = {}
        for f in findings:
            cid = f["evidence"]["candidate_ids"][0]
            merged.setdefault(cid, f)
        return {"summary": "[fake critic] mechanical adjudication of joins.",
                "findings": list(merged.values())}

    @staticmethod
    def _mk(title, category, severity, verdict, cid, turn, reason):
        return {"title": title, "category": category, "severity": severity,
                "verdict": verdict, "confidence": "high",
                "evidence": {"turns": [turn], "candidate_ids": [cid],
                             "player_quote": "", "oracle": reason},
                "root_cause_hypothesis": "[fake critic]"}


# ---------------------------------------------------------------------------
# 3. Validation + rendering
# ---------------------------------------------------------------------------

def _grounded(f: dict) -> bool:
    """The evidence-discipline gate: a finding only counts if it cites
    turn(s) AND an oracle record. (The player quote may legitimately be
    empty — e.g. a crash the player never got to comment on.)"""
    ev = f.get("evidence") or {}
    return bool(ev.get("turns")) and bool((ev.get("oracle") or "").strip())


def validate_findings(data: dict, candidates: list[dict]) -> tuple[dict, list[str]]:
    warnings = []
    data = dict(data or {})
    data.setdefault("summary", "")
    findings = [f for f in (data.get("findings") or []) if isinstance(f, dict)]
    for f in findings:
        if f.get("category") not in CATEGORIES:
            warnings.append(f"finding {f.get('title')!r}: bad category "
                            f"{f.get('category')!r} -> other")
            f["category"] = "other"
        if f.get("severity") not in SEVERITIES:
            f["severity"] = "minor"
        if f.get("verdict") not in VERDICTS:
            warnings.append(f"finding {f.get('title')!r}: bad verdict "
                            f"{f.get('verdict')!r} -> intended")
            f["verdict"] = "intended"
        if f.get("confidence") not in CONFIDENCES:
            f["confidence"] = "low"
        ev = f.get("evidence") or {}
        ev.setdefault("turns", [])
        ev.setdefault("candidate_ids", [])
        ev.setdefault("player_quote", "")
        ev.setdefault("oracle", "")
        f["evidence"] = ev
        if not _grounded(f):
            # ungrounded findings never count as coverage, and are
            # marked so the reader can't mistake them for evidence
            warnings.append(f"finding {f.get('title')!r} is UNGROUNDED "
                            "(no turns and/or no oracle record) — excluded "
                            "from candidate coverage, confidence forced low")
            f["confidence"] = "low"
    data["findings"] = findings
    missing = [c["cid"] for c in uncovered(data, candidates)]
    return data, warnings + ([f"unadjudicated candidates: {', '.join(missing)}"]
                             if missing else [])


def uncovered(data: dict, candidates: list[dict]) -> list[dict]:
    """Candidates not yet covered by a GROUNDED finding — an
    ungrounded claim of coverage doesn't count (review finding 4)."""
    covered = {cid for f in data.get("findings", []) if _grounded(f)
               for cid in (f.get("evidence") or {}).get("candidate_ids", [])}
    return [c for c in candidates if c["cid"] not in covered]


_SEV_RANK = {s: i for i, s in enumerate(SEVERITIES)}


def _assign_ids(findings: list[dict]) -> None:
    findings.sort(key=lambda f: (0 if f["verdict"] == "defect" else 1,
                                 _SEV_RANK.get(f["severity"], 9),
                                 min(f["evidence"]["turns"] or [999])))
    for i, f in enumerate(findings, 1):
        f["id"] = f"F{i:02d}"


def _screenshot_refs(f: dict, turns_by_n: dict) -> list[str]:
    refs = []
    for n in f["evidence"]["turns"]:
        t = turns_by_n.get(n)
        if t and t.get("screenshot"):
            refs.append(t["screenshot"])
    return refs


def render_report(meta: dict, data: dict, warnings: list[str],
                  turns: list[dict]) -> str:
    persona = meta.get("persona") or {}
    turns_by_n = {t.get("turn"): t for t in turns}
    defects = [f for f in data["findings"] if f["verdict"] == "defect"]
    intended = [f for f in data["findings"] if f["verdict"] != "defect"]

    def block(f):
        out = [f"### {f['id']}: {f['title']}",
               "",
               f"- **category:** {f['category']}  **severity:** {f['severity']}"
               f"  **verdict:** {f['verdict']}  **confidence:** {f['confidence']}",
               f"- **turns:** {', '.join(str(n) for n in f['evidence']['turns']) or '—'}"]
        if f["evidence"]["player_quote"]:
            out.append(f"- **player:** “{f['evidence']['player_quote']}”")
        out.append(f"- **oracle evidence:** {f['evidence']['oracle'] or '—'}")
        if f.get("root_cause_hypothesis"):
            out.append(f"- **root-cause hypothesis:** {f['root_cause_hypothesis']}")
        for ref in f.get("screenshots", []):
            out.append(f"\n![turn screenshot]({ref})")
        return "\n".join(out)

    lines = [
        f"# UX playtest report — {persona.get('name', 'session')}",
        "",
        f"- **goal:** {persona.get('goal', '—')}",
        f"- **stop reason:** {meta.get('stop_reason')} after {meta.get('turns')} turns"
        f"  (world seed: {meta.get('world_seed')})",
        f"- **player model:** {json.dumps(meta.get('player_model'))}",
        f"- **critic:** {json.dumps(data.get('critic_model'))}",
        "",
        "## Summary",
        "",
        data.get("summary", ""),
        "",
        f"## Defects ({len(defects)})",
        "",
    ]
    lines += [block(f) + "\n" for f in defects] or ["(none)\n"]
    lines += [f"## Intended behavior the player tripped on ({len(intended)})",
              "",
              "Working as designed per the oracle — kept visible so the "
              "maintainer can override the critic's call.",
              ""]
    lines += [block(f) + "\n" for f in intended] or ["(none)\n"]
    if warnings:
        lines += ["## Critic warnings", ""]
        lines += [f"- {w}" for w in warnings]
        lines.append("")
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# 4. Orchestration
# ---------------------------------------------------------------------------

def run_critic(trace_dir: str, critic, manual_path: str | None = None,
               out_dir: str | None = None,
               max_frames: int = DEFAULT_MAX_FRAMES) -> tuple[str, str]:
    meta = load_meta(trace_dir)
    turns = load_turns(trace_dir)
    signals = build_signals(trace_dir, turns)
    candidates = friction_candidates(meta, signals)
    digest = build_digest(meta, signals, candidates)
    frames, frame_warnings = select_frames(trace_dir, turns, candidates,
                                           max_frames)

    manual = ""
    manual_path = manual_path or meta.get("manual_path") or os.path.join(
        os.path.dirname(os.path.dirname(HERE)), "docs", "player_manual.md")
    try:
        with open(manual_path, encoding="utf-8") as f:
            manual = f.read()
    except OSError:
        manual = "(manual unavailable)"

    data = critic.adjudicate(digest, manual, frames)
    data, warnings = validate_findings(data, candidates)

    # one bounded repair pass for anything left unadjudicated (or only
    # covered by ungrounded findings) — WITH the missing candidates'
    # own frames, so the repair judges from the same evidence
    missing = uncovered(data, candidates)
    if missing:
        repair_frames, _ = select_frames(trace_dir, turns, missing, max_frames)
        ask = ("These candidate ids were left unadjudicated (or covered only "
               "by findings with no grounding evidence): "
               + ", ".join(c["cid"] for c in missing)
               + ". Produce findings covering ONLY these ids now, each citing "
               "its turns and oracle record "
               "(same schema; the earlier findings are already recorded).")
        try:
            extra = critic.adjudicate(digest, manual, repair_frames, ask=ask)
            data["findings"] += (extra.get("findings") or [])
            data, warnings = validate_findings(data, candidates)
        except Exception as e:  # keep the report; warn honestly
            warnings.append(f"repair pass failed: {e}")
    warnings = frame_warnings + warnings

    _assign_ids(data["findings"])
    turns_by_n = {t.get("turn"): t for t in turns}
    for f in data["findings"]:
        f["screenshots"] = _screenshot_refs(f, turns_by_n)
        f["evidence"]["screenshots"] = f["screenshots"]
    data["critic_model"] = getattr(critic, "model", "fake")
    data["candidates"] = candidates  # the full pre-analysis, for audit
    data["frames_shown"] = [n for n, _ in frames]  # what the critic saw

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


# ---------------------------------------------------------------------------
# Selftest (offline) + eval (real model on the canned trace)
# ---------------------------------------------------------------------------

def selftest() -> int:
    import tempfile
    from canned_trace import build_canned_trace
    failures = []

    def check(name, ok, detail=""):
        print(f"  [{'ok' if ok else 'FAIL'}] {name}"
              + (f" — {detail}" if detail else ""))
        if not ok:
            failures.append(name)

    check("widget_at hit", widget_at([{"bounds": {"x": 0, "y": 0, "w": 10, "h": 10}}],
                                     5, 5) is not None)
    check("widget_at miss", widget_at([{"bounds": {"x": 0, "y": 0, "w": 10, "h": 10}}],
                                      50, 5) is None)

    with tempfile.TemporaryDirectory() as tmp:
        tdir = build_canned_trace(os.path.join(tmp, "trace"))
        meta = load_meta(tdir)
        turns = load_turns(tdir)
        signals = build_signals(tdir, turns)
        cands = friction_candidates(meta, signals)

        def cand_for(turn):
            return next((c for c in cands if c["turn"] == turn), None)

        c1 = cand_for(1)
        check("planted silent failure joins on turn 1",
              c1 is not None and any(r.startswith("silent-failure-join")
                                     for r in c1["reasons"]),
              str(c1 and c1["reasons"]))
        c3 = cand_for(3)
        check("planted missed-feedback carries the contradicting oracle",
              c3 is not None and any(r.startswith("feedback-was-shown-join")
                                     for r in c3["reasons"]),
              str(c3 and c3["reasons"]))
        c4 = cand_for(4)
        check("planted phantom affordance joins on turn 4",
              c4 is not None and any(r.startswith("phantom-affordance-join")
                                     for r in c4["reasons"]),
              str(c4 and c4["reasons"]))
        check("quiet turn produces no candidate", cand_for(5) is None)
        c6 = cand_for(6)
        check("rejected outcome masked by unrelated feedback still a candidate",
              c6 is not None and any(r.startswith("bad-outcome-join")
                                     for r in c6["reasons"]),
              str(c6 and c6["reasons"]))

        digest = build_digest(meta, signals, cands)
        check("digest carries the full turn record",
              "observation:" in digest and "injected:" in digest
              and "acks:" in digest and '"Place Marker"' in digest
              and "menu='world_view'" in digest,
              "missing fields" if not all(x in digest for x in
                  ("observation:", "injected:")) else "")

        frames, fwarn = select_frames(tdir, turns, cands, max_frames=2)
        cand_turns = [c["turn"] for c in cands]
        check("frame budget prioritizes candidates' own turns",
              [n for n, _ in frames] == sorted(cand_turns[:2]),
              str([n for n, _ in frames]))
        check("starved candidates are warned about explicitly",
              fwarn and all(c["cid"] in fwarn[0]
                            for c in cands[2:]), str(fwarn))

        report_path, findings_path = run_critic(tdir, FakeCritic())
        with open(findings_path) as f:
            data = json.load(f)
        check("findings.json written and parseable",
              isinstance(data.get("findings"), list)
              and len(data["findings"]) >= 3)
        check("every candidate adjudicated",
              not uncovered(data, cands))
        check("both verdict buckets present",
              {f["verdict"] for f in data["findings"]} >= {"defect", "intended"})
        check("enums valid",
              all(f["category"] in CATEGORIES and f["severity"] in SEVERITIES
                  and f["verdict"] in VERDICTS for f in data["findings"]))
        check("evidence grounded (turns + oracle) on every finding",
              all(f["evidence"]["turns"] and f["evidence"]["oracle"]
                  for f in data["findings"]))
        report = open(report_path).read()
        check("report has both sections",
              "## Defects" in report
              and "## Intended behavior the player tripped on" in report)
        check("report references screenshots",
              "frames/turn_" in report)

        # coverage repair: a critic that ignores candidates gets one
        # bounded repair ask, then an honest warning
        class LazyCritic(FakeCritic):
            def __init__(self):
                self.asks = 0

            def adjudicate(self, digest, manual, frames, ask=None):
                self.asks += 1
                if ask is None:
                    return {"summary": "lazy", "findings": []}
                return FakeCritic.adjudicate(self, digest, manual, frames)

        lazy = LazyCritic()
        rp, fp = run_critic(tdir, lazy, out_dir=os.path.join(tmp, "lazy"))
        with open(fp) as f:
            lazy_data = json.load(f)
        check("repair pass recovers unadjudicated candidates",
              lazy.asks == 2 and not uncovered(lazy_data, cands))

        # ungrounded coverage doesn't count: a critic claiming coverage
        # with no turns/oracle triggers the repair pass and a warning
        class UngroundedCritic(FakeCritic):
            def __init__(self):
                self.asks = 0

            def adjudicate(self, digest, manual, frames, ask=None):
                self.asks += 1
                if ask is None:
                    return {"summary": "hand-waving", "findings": [{
                        "title": "vibes", "category": "other",
                        "severity": "minor", "verdict": "defect",
                        "confidence": "high",
                        "evidence": {"turns": [], "candidate_ids":
                                     [c["cid"] for c in cands],
                                     "player_quote": "", "oracle": ""},
                        "root_cause_hypothesis": ""}]}
                return FakeCritic.adjudicate(self, digest, manual, frames)

        ug = UngroundedCritic()
        rp2, fp2 = run_critic(tdir, ug, out_dir=os.path.join(tmp, "ungrounded"))
        with open(fp2) as f:
            ug_data = json.load(f)
        ug_report = open(rp2).read()
        check("ungrounded findings don't count as coverage (repair forced)",
              ug.asks == 2 and not uncovered(ug_data, cands))
        check("ungrounded finding flagged in the report",
              "UNGROUNDED" in ug_report)

    if failures:
        print(f"critic selftest: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("critic selftest: all checks passed")
    return 0


def eval_run(model: str, effort: str) -> int:
    """The acceptance run: the REAL model against the canned trace.
    Needs ANTHROPIC_API_KEY (or an `ant auth login` profile)."""
    import tempfile
    from canned_trace import build_canned_trace
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
        keep = os.path.join(HERE, "sessions", "critic_eval_last")
        import shutil
        shutil.rmtree(keep, ignore_errors=True)
        shutil.copytree(tmp, keep)
        print(f"  artifacts copied to {keep}")

    if failures:
        print(f"critic eval: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("critic eval: all checks passed")
    return 0


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
        return selftest()
    if args.eval:
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
