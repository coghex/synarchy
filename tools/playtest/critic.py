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

Evidence discipline — this is what makes the report trustworthy, and \
it is MECHANICALLY VERIFIED after your reply:
- every finding cites specific turn numbers (including each covered \
candidate's own turn) and the oracle record that grounds it;
- player_quote must be copied VERBATIM from the player's recorded \
words (their note/observation/expectation) for the cited turns — \
never paraphrase inside the quote field. It may be empty ONLY when \
the trace recorded no player words at all for those turns (e.g. a \
crash before the player ever spoke);
- the oracle field must restate the candidate's RECORDED data: \
include at least one verbatim atom from the digest — the outcome \
value and its reason, an event text, or the clicked widget's label — \
for each covered candidate. Naming a harness join tag does NOT count; \
free prose that references nothing recorded is rejected as \
unverifiable. When a candidate has NO outcome/event/widget records, \
state the absence by quoting the digest's oracle-line fragments \
verbatim (e.g. "events=[]", "outcomes=[]", "visual_change_next=False") \
— never assert facts the record doesn't contain;
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

    def add(turn, reasons, oracle_excerpt, player_note="", player_words=""):
        cands.append({"cid": f"C{len(cands) + 1}", "turn": turn,
                      "player_note": player_note,
                      "player_words": player_words or player_note,
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
        # Phantom-affordance requires the F4 deadclick contract (or,
        # in traces without outcome records, an explicitly fed-back
        # nothing + a player note). F3 enumerates UI widgets, not the
        # game world — an ordinary successful world click hits no
        # widget and must NOT become a candidate.
        deadclicked = any(isinstance(o, dict) and o.get("outcome") == "deadclick"
                          for o in s["outcomes"])
        if s["action"].get("do") == "click" and s["clicked_widget"] is None \
                and (deadclicked
                     or (not s["outcomes"] and not feedback_bits and s["note"])):
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
            }, player_note=s["note"],
                player_words=(s["note"] or s["observation"]
                              or s["expectation"]))

    if meta.get("stop_reason") == "engine_crash":
        crash_turn = meta.get("turns", 0) or 0
        s_last = next((s for s in signals if s["turn"] == crash_turn), None)
        add(crash_turn,
            ["engine crash ended the session (crash/blocker)"],
            {"stop_reason": "engine_crash",
             "crash_detail": meta.get("crash_detail"),
             "engine_log_tail": (meta.get("engine_log_tail") or "")[-1500:]},
            player_words=(s_last and (s_last["note"] or s_last["observation"]
                                      or s_last["expectation"]) or ""))
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
        # Full, LOSSLESS F3 widget dump — state-bearing fields (value,
        # focused, hovered, ...) included, so a toggle flipping value
        # is a real change. Deduped only when the serialized records
        # are byte-identical to the previous turn's.
        key = json.dumps(s["widgets"], sort_keys=True)
        if key == prev_widgets_key:
            lines.append("  widgets: (unchanged from previous turn)")
        else:
            lines.append(f"  widgets: {key}")
            prev_widgets_key = key
        lines.append(f"  oracle: menu={s['current_menu']!r} paused={s['paused']} "
                     f"events={json.dumps(s['events'])} "
                     f"outcomes={json.dumps(s['outcomes'])} "
                     f"clicked_widget={json.dumps(s['clicked_widget'])} "
                     f"visual_change_next={s['visual_change_next']} "
                     f"stuck={s['stuck']}")
    lines.append("")
    lines.append("FRICTION CANDIDATES (adjudicate every id; quote the "
                 "player's own words when a note exists)")
    for c in candidates:
        lines.append(f"{c['cid']} (turn {c['turn']}):")
        if c.get("player_note"):
            lines.append(f"  player_note: {c['player_note']}")
        elif c.get("player_words"):
            lines.append(f"  player_words: {c['player_words']}")
        for r in c["reasons"]:
            lines.append(f"  - {r}")
    if not candidates:
        lines.append("(none — a clean session; say so in the summary)")
    return "\n".join(lines)


def _frame_path(trace_dir: str, by_turn: dict, n: int) -> str | None:
    t = by_turn.get(n)
    if not t:
        return None
    p = os.path.join(trace_dir, t.get("screenshot", ""))
    return p if os.path.isfile(p) else None


def plan_batches(trace_dir: str, turns: list[dict], candidates: list[dict],
                 max_frames: int
                 ) -> tuple[list[tuple[list[dict], list[tuple[int, str]]]],
                            list[str]]:
    """Split the adjudication into calls such that EVERY candidate's
    own-turn screenshot is actually shown in the call that adjudicates
    it (the H2 multimodal-evidence requirement) — a warning about an
    unseen frame is not a substitute. Greedy: pack candidates into a
    batch while their frames (own turn, then the following turn for
    the visible effect) fit --max-frames; overflow starts a new call.
    A candidate-free session gets one call with the session bookends.
    Returns (batches, warnings)."""
    by_turn = {t.get("turn"): t for t in turns}
    batches: list[tuple[list[dict], list[tuple[int, str]]]] = []
    cur: list[dict] = []
    cur_frames: list[tuple[int, str]] = []
    warnings: list[str] = []

    def flush():
        nonlocal cur, cur_frames
        if cur:
            batches.append((cur, sorted(cur_frames)))
        cur, cur_frames = [], []

    for c in candidates:
        own = _frame_path(trace_dir, by_turn, c["turn"])
        own_needed = (1 if own and all(n != c["turn"] for n, _ in cur_frames)
                      else 0)
        if cur and len(cur_frames) + own_needed > max_frames:
            flush()
            own_needed = 1 if own else 0
        if own and own_needed and len(cur_frames) < max_frames:
            cur_frames.append((c["turn"], own))
        elif own and own_needed:
            # only possible when max_frames < 1
            warnings.append(f"frame budget too small: candidate {c['cid']}"
                            f" adjudicated WITHOUT its turn-{c['turn']} "
                            "screenshot")
        cur.append(c)
        nxt = _frame_path(trace_dir, by_turn, c["turn"] + 1)
        if nxt and len(cur_frames) < max_frames \
                and all(n != c["turn"] + 1 for n, _ in cur_frames):
            cur_frames.append((c["turn"] + 1, nxt))
    flush()

    if not batches:
        frames: list[tuple[int, str]] = []
        for n in ([turns[0].get("turn"), turns[-1].get("turn")]
                  if turns else []):
            p = _frame_path(trace_dir, by_turn, n)
            if p and len(frames) < max_frames \
                    and all(m != n for m, _ in frames):
                frames.append((n, p))
        batches = [([], sorted(frames))]
    return batches, warnings


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
        import re
        only = set(re.findall(r"C\d+", ask)) if ask else None
        findings = []
        # parse candidate blocks back out of the digest
        cur = None
        note = ""
        for line in digest.splitlines():
            if line.startswith("C") and "(turn " in line:
                cid, rest = line.split(" ", 1)
                cur = (cid.rstrip(":"), int(rest.split("turn ")[1].split(")")[0]))
                note = ""
            elif cur and line.strip().startswith(("player_note: ",
                                                  "player_words: ")):
                note = line.strip().split(": ", 1)[1]
            elif cur and line.strip().startswith("- "):
                reason = line.strip()[2:]
                cid, turn = cur
                if only is not None and cid not in only:
                    continue
                if reason.startswith("silent-failure-join"):
                    findings.append(self._mk(
                        "Silent failure", "missing-feedback", "major", "defect",
                        cid, turn, reason, note))
                elif reason.startswith("bad-outcome-join"):
                    findings.append(self._mk(
                        "Rejection masked by unrelated feedback",
                        "missing-feedback", "major", "defect",
                        cid, turn, reason, note))
                elif reason.startswith("phantom-affordance-join"):
                    findings.append(self._mk(
                        "Phantom affordance", "phantom-affordance", "minor",
                        "defect", cid, turn, reason, note))
                elif reason.startswith("feedback-was-shown-join"):
                    findings.append(self._mk(
                        "Feedback shown but missed", "discoverability", "minor",
                        "intended", cid, turn, reason, note))
                elif reason.startswith("engine crash"):
                    findings.append(self._mk(
                        "Engine crash", "crash", "blocker", "defect",
                        cid, turn, reason, note))
                else:
                    findings.append(self._mk(
                        "Player friction", "other", "polish", "intended",
                        cid, turn,
                        reason + " — record shows events=[] outcomes=[]",
                        note))
                cur = (cid, turn)
        # merge multiple findings for the same cid into the first
        merged: dict[str, dict] = {}
        for f in findings:
            cid = f["evidence"]["candidate_ids"][0]
            merged.setdefault(cid, f)
        return {"summary": "[fake critic] mechanical adjudication of joins.",
                "findings": list(merged.values())}

    @staticmethod
    def _mk(title, category, severity, verdict, cid, turn, reason, quote=""):
        return {"title": title, "category": category, "severity": severity,
                "verdict": verdict, "confidence": "high",
                "evidence": {"turns": [turn], "candidate_ids": [cid],
                             "player_quote": quote, "oracle": reason},
                "root_cause_hypothesis": "[fake critic]"}


# ---------------------------------------------------------------------------
# 3. Validation + rendering
# ---------------------------------------------------------------------------

def _norm(s: str) -> str:
    return " ".join((s or "").lower().split())


def _anchor_strings(cand: dict) -> set[str]:
    """Verbatim atoms from the candidate's RECORDED oracle data — a
    finding's oracle prose must contain at least one of these, so
    fabricated 'evidence' the trace never recorded can't pass
    coverage. Only recorded data qualifies (outcome values/reasons/
    verbs, event texts/cats, the clicked widget's label/id, crash
    details) — harness-derived join tags are NOT anchors, since
    parroting a tag proves nothing about the record. A candidate with
    no recorded atoms at all (e.g. a pure stuck-loop) has an empty set
    and the anchor check is skipped for it."""
    atoms: list[str] = []
    o = cand.get("oracle") or {}
    for rec in o.get("outcomes") or []:
        if isinstance(rec, dict):
            for k in ("outcome", "reason", "verb"):
                if isinstance(rec.get(k), str):
                    atoms.append(rec[k])
    for e in o.get("events") or []:
        if isinstance(e, dict):
            for k in ("text", "cat"):
                if isinstance(e.get(k), str):
                    atoms.append(e[k])
    w = o.get("clicked_widget")
    if isinstance(w, dict):
        for k in ("label", "id", "name"):
            if isinstance(w.get(k), str):
                atoms.append(w[k])
    if isinstance(o.get("crash_detail"), str):
        atoms.append(o["crash_detail"])
    if o.get("stop_reason") == "engine_crash":
        # recorded in meta.stop_reason, so these ARE record-grounded
        atoms += ["engine_crash", "crash"]
    anchors = {_norm(a) for a in atoms if len(_norm(a)) >= 4}
    if not anchors:
        # No positive atoms (a pure player-note candidate): the record
        # still says something — that NOTHING was recorded. The oracle
        # field must acknowledge that by quoting the digest's literal
        # absence fragments instead of inventing facts; these are the
        # exact substrings the digest's oracle line carries.
        if not (o.get("events") or []):
            anchors.add(_norm("events=[]"))
        if not (o.get("outcomes") or []):
            anchors.add(_norm("outcomes=[]"))
        anchors.add(_norm(
            f"visual_change_next={o.get('visual_change_next')}"))
    return anchors


class ValidationCtx:
    """Everything coverage validation checks findings against."""

    def __init__(self, candidates: list[dict], turns: list[dict],
                 audit_calls: list[dict]):
        self.by_cid = {c["cid"]: c for c in candidates}
        # every candidate's own turn is adjudicable by construction —
        # a crash before the first recorded turn yields a candidate at
        # a turn number with no turn record, and it must still be
        # coverable rather than auto-rejected as nonexistent
        self.valid_turns = ({t.get("turn") for t in turns}
                            | {c["turn"] for c in candidates})
        # frames actually shown, per adjudication call number
        self.frames_by_call = {a["call"]: set(a["frames"]) for a in audit_calls}
        self.trace_frames = {t.get("turn") for t in turns
                             if t.get("screenshot")}
        # the player's recorded words per turn — quotes must come from here
        self.player_text = {}
        for t in turns:
            p = t.get("player") or {}
            self.player_text[t.get("turn")] = _norm(" ".join(
                str(p.get(k) or "") for k in
                ("note", "observation", "expectation")))
        self.anchors_by_cid = {c["cid"]: _anchor_strings(c)
                               for c in candidates}


def coverage_of(f: dict, ctx: ValidationCtx, warnings: list[str]) -> set[str]:
    """The candidate ids this finding VALIDLY covers, applying the
    evidence discipline (review finding 4): turns must be non-empty,
    real trace turns; an oracle record must be cited; each claimed
    candidate's own turn must be among the cited turns; a candidate
    born from a player note requires a non-empty player quote; and the
    candidate's own frame must have been SHOWN in the call that
    produced the finding (finding 3) when it exists in the trace.
    Violations strip coverage (forcing the repair pass) and warn."""
    ev = f.get("evidence") or {}
    title = f.get("title")
    turns = ev.get("turns") or []
    if not turns or not (ev.get("oracle") or "").strip():
        warnings.append(f"finding {title!r} is UNGROUNDED (no turns and/or "
                        "no oracle record) — excluded from coverage, "
                        "confidence forced low")
        f["confidence"] = "low"
        return set()
    bogus = [n for n in turns if n not in ctx.valid_turns]
    if bogus:
        warnings.append(f"finding {title!r} cites nonexistent turn(s) "
                        f"{bogus} — excluded from coverage, confidence "
                        "forced low")
        f["confidence"] = "low"
        return set()
    # the quote must be the player's ACTUAL recorded words for the
    # cited turns: a fabricated quote poisons the whole finding, and
    # an EMPTY quote is only acceptable when the trace recorded no
    # player words at all for those turns (e.g. a pre-first-turn
    # crash) — otherwise every finding cites the player's own voice
    quote = _norm(ev.get("player_quote") or "")
    recorded = " | ".join(ctx.player_text.get(n, "") for n in turns)
    if quote and quote not in recorded:
        warnings.append(f"finding {title!r} attributes words to the "
                        "player that the trace never recorded — "
                        "excluded from coverage, confidence forced low")
        f["confidence"] = "low"
        return set()
    if not quote and _norm(recorded):
        warnings.append(f"finding {title!r} cites turns where the player "
                        "wrote words but provides no player_quote — "
                        "coverage stripped (quote the player verbatim)")
        f["confidence"] = "low"
        return set()
    oracle_text = _norm(ev.get("oracle") or "")
    covered = set()
    shown = ctx.frames_by_call.get(f.get("adjudication_call"), set())
    for cid in ev.get("candidate_ids") or []:
        cand = ctx.by_cid.get(cid)
        if cand is None:
            warnings.append(f"finding {title!r} claims unknown candidate "
                            f"{cid!r} — ignored")
            continue
        if cand["turn"] not in turns:
            warnings.append(f"finding {title!r} claims {cid} but does not "
                            f"cite its turn {cand['turn']} — coverage "
                            "stripped for that candidate")
            continue
        anchors = ctx.anchors_by_cid.get(cid, set())
        if not any(a in oracle_text for a in anchors):
            warnings.append(f"finding {title!r} claims {cid} but its oracle "
                            "evidence references none of that candidate's "
                            "RECORDED oracle data (outcome/reason/event/"
                            "widget) — coverage stripped as unverifiable")
            continue
        if cand["turn"] in ctx.trace_frames and cand["turn"] not in shown:
            warnings.append(f"finding {title!r} claims {cid} but the model "
                            f"was never shown turn {cand['turn']}'s "
                            "screenshot in that call — coverage stripped "
                            "(repair will re-adjudicate with the frame)")
            continue
        covered.add(cid)
    return covered


def validate_findings(data: dict, candidates: list[dict],
                      ctx: ValidationCtx) -> tuple[dict, list[str]]:
    warnings: list[str] = []
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
        f["covers"] = sorted(coverage_of(f, ctx, warnings))
        if f["evidence"]["candidate_ids"] and not f["covers"]:
            # a finding whose every claimed candidate was stripped is
            # unverified — it must not be presented as confident
            f["confidence"] = "low"
    data["findings"] = findings
    missing = [c["cid"] for c in uncovered(data, candidates)]
    return data, warnings + ([f"unadjudicated candidates: {', '.join(missing)}"]
                             if missing else [])


def uncovered(data: dict, candidates: list[dict]) -> list[dict]:
    """Candidates not covered by a VALIDLY grounded finding (per
    coverage_of — computed into f['covers'] during validation)."""
    covered = {cid for f in data.get("findings", [])
               for cid in f.get("covers", [])}
    return [c for c in candidates if c["cid"] not in covered]


_SEV_RANK = {s: i for i, s in enumerate(SEVERITIES)}


def _assign_ids(findings: list[dict]) -> None:
    findings.sort(key=lambda f: (0 if f["verdict"] == "defect" else 1,
                                 _SEV_RANK.get(f["severity"], 9),
                                 min(f["evidence"]["turns"] or [999])))
    for i, f in enumerate(findings, 1):
        f["id"] = f"F{i:02d}"


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
    batches, frame_warnings = plan_batches(trace_dir, turns, candidates,
                                           max_frames)

    manual = ""
    manual_path = manual_path or meta.get("manual_path") or os.path.join(
        os.path.dirname(os.path.dirname(HERE)), "docs", "player_manual.md")
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

    _assign_ids(data["findings"])
    turns_by_n = {t.get("turn"): t for t in turns}
    frames_by_call = {a["call"]: set(a["frames"]) for a in audit_calls}
    for f in data["findings"]:
        # attach only screenshots the model was actually shown in the
        # call that produced this finding — a report must never imply
        # the critic saw a frame it didn't (review finding 3)
        shown = frames_by_call.get(f.get("adjudication_call"), set())
        f["screenshots"] = [ref for n, ref in
                            ((n, (turns_by_n.get(n) or {}).get("screenshot"))
                             for n in f["evidence"]["turns"])
                            if ref and n in shown]
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
        c7 = cand_for(7)
        check("note-only friction is a candidate",
              c7 is not None and c7["reasons"] ==
              ["player-reported friction (note)"], str(c7 and c7["reasons"]))

        digest = build_digest(meta, signals, cands)
        check("digest carries the full turn record",
              "observation:" in digest and "injected:" in digest
              and "acks:" in digest and '"Place Marker"' in digest
              and "menu='world_view'" in digest,
              "missing fields" if not all(x in digest for x in
                  ("observation:", "injected:")) else "")

        # ordinary WORLD clicks are not phantom candidates: F3 lists UI
        # widgets, not world objects (round-2 review blocker)
        base_click = {
            "turn": 1, "observation": "", "note": "", "expectation": "",
            "action": {"do": "click", "x": 5, "y": 5},
            "injected": ["return input.click(5.0, 5.0)"],
            "acks": [{"ok": True}], "events": [], "outcomes": [],
            "bad_outcomes": [], "ack_errors": [],
            "visual_change_next": True, "clicked_widget": None,
            "widgets": [], "current_menu": "world_view", "paused": True,
            "stuck": False,
        }
        accepted_click = dict(base_click,
                              outcomes=[{"verb": "move", "outcome": "accepted"}])
        check("world click with accepted outcome is no phantom candidate",
              friction_candidates({}, [accepted_click]) == [])
        check("outcome-less world click with visible effect is no candidate",
              friction_candidates({}, [base_click]) == [])

        # widget STATE changes must not dedupe as \"unchanged\"
        s_a = dict(base_click, widgets=[{"id": "toggle:x", "value": False}])
        s_b = dict(base_click, turn=2, widgets=[{"id": "toggle:x", "value": True}])
        s_c = dict(s_b, turn=3)
        check("widget value flip is a real change in the digest",
              "(unchanged from previous turn)"
              not in build_digest({}, [s_a, s_b], []))
        check("byte-identical widget lists still dedupe",
              "(unchanged from previous turn)"
              in build_digest({}, [s_b, s_c], []))

        # batching honesty: a tight frame budget means MORE calls, never
        # an unseen candidate frame
        batches, bwarn = plan_batches(tdir, turns, cands, max_frames=2)
        check("tight frame budget splits into more calls",
              len(batches) >= 3, str([len(b[0]) for b in batches]))
        check("every candidate's own frame is in its own call",
              all(any(n == c["turn"] for n, _ in frames)
                  for subset, frames in batches for c in subset))
        check("no starvation warnings needed once batched",
              not bwarn, str(bwarn))

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
        check("adjudication audit records calls + frames",
              isinstance(data.get("adjudication_calls"), list)
              and all("frames" in a and "candidate_ids" in a
                      for a in data["adjudication_calls"]))
        fbc = {a["call"]: set(a["frames"])
               for a in data["adjudication_calls"]}
        check("findings only attach screenshots their call actually saw",
              all(int(ref.rsplit("_", 1)[1].split(".")[0])
                  in fbc.get(f_["adjudication_call"], set())
                  for f_ in data["findings"]
                  for ref in f_.get("screenshots", [])))

        # batched end-to-end: max_frames=2 still covers everything
        rp4, fp4 = run_critic(tdir, FakeCritic(),
                              out_dir=os.path.join(tmp, "batched"),
                              max_frames=2)
        with open(fp4) as f:
            data4 = json.load(f)
        check("batched adjudication covers every candidate",
              not uncovered(data4, cands)
              and len(data4["adjudication_calls"]) >= 3,
              f"calls={len(data4.get('adjudication_calls', []))}")

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

        # evidence-alignment negatives: nonexistent turns, and a
        # player-noted candidate claimed without quoting the player
        def _one_shot(bad_finding):
            class BadCritic(FakeCritic):
                def __init__(self):
                    self.asks = 0

                def adjudicate(self, digest, manual, frames, ask=None):
                    self.asks += 1
                    if ask is None:
                        return {"summary": "x", "findings": [dict(bad_finding)]}
                    return FakeCritic.adjudicate(self, digest, manual, frames)
            return BadCritic()

        mm = _one_shot({"title": "wrong turn", "category": "other",
                        "severity": "minor", "verdict": "defect",
                        "confidence": "high",
                        "evidence": {"turns": [999], "candidate_ids": ["C1"],
                                     "player_quote": "q", "oracle": "made up"},
                        "root_cause_hypothesis": ""})
        rp3, fp3 = run_critic(tdir, mm, out_dir=os.path.join(tmp, "mismatch"))
        with open(fp3) as f:
            mm_data = json.load(f)
        check("nonexistent-turn evidence rejected, repair recovers",
              mm.asks == 2 and not uncovered(mm_data, cands))
        check("nonexistent-turn warning surfaces in the report",
              "nonexistent turn" in open(rp3).read())

        c1_turn = next(c["turn"] for c in cands if c["cid"] == "C1")
        ql = _one_shot({"title": "no quote", "category": "missing-feedback",
                        "severity": "major", "verdict": "defect",
                        "confidence": "high",
                        "evidence": {"turns": [c1_turn],
                                     "candidate_ids": ["C1"],
                                     "player_quote": "",
                                     "oracle": "outcome rejected"},
                        "root_cause_hypothesis": ""})
        rp5, fp5 = run_critic(tdir, ql, out_dir=os.path.join(tmp, "quoteless"))
        with open(fp5) as f:
            ql_data = json.load(f)
        check("player-noted candidate without the player's quote is not "
              "covered until repaired",
              ql.asks == 2 and not uncovered(ql_data, cands))

        # fabricated evidence: right turn, right cid, shown frame — but
        # words the player never said and oracle prose referencing
        # nothing the trace recorded. Both must strip coverage.
        fab = _one_shot({"title": "fabricated", "category": "missing-feedback",
                         "severity": "major", "verdict": "defect",
                         "confidence": "high",
                         "evidence": {"turns": [c1_turn],
                                      "candidate_ids": ["C1"],
                                      "player_quote": "words the player never said",
                                      "oracle": "made-up oracle fact"},
                         "root_cause_hypothesis": ""})
        rp6, fp6 = run_critic(tdir, fab, out_dir=os.path.join(tmp, "fabricated"))
        with open(fp6) as f:
            fab_data = json.load(f)
        fab_report = open(rp6).read()
        check("fabricated quote/oracle rejected, repair recovers",
              fab.asks == 2 and not uncovered(fab_data, cands))
        check("fabricated quote warned about explicitly",
              "never recorded" in fab_report)
        check("stripped finding is demoted to low confidence",
              all(f_["confidence"] == "low" for f_ in fab_data["findings"]
                  if f_["title"] == "fabricated"))

        # anchoring unit checks against the note-less, outcome-derived
        # candidate (turn 6): the player still WROTE words that turn
        # (observation/expectation), so a verbatim quote of those is
        # required; the oracle must contain a recorded atom
        c6cand = next(c for c in cands if c["turn"] == 6)
        uctx = ValidationCtx(cands, turns,
                             [{"call": 1, "frames": [6]}])
        ok_f = {"title": "t", "category": "missing-feedback",
                "severity": "major", "verdict": "defect",
                "confidence": "high", "adjudication_call": 1,
                "evidence": {"turns": [6], "candidate_ids": [c6cand["cid"]],
                             "player_quote":
                                 "Placing a marker now that things settled.",
                             "oracle": "the action came back rejected: "
                                       "insufficient materials"},
                "root_cause_hypothesis": ""}
        w1: list[str] = []
        check("note-less candidate: verbatim quote + recorded atom covers",
              coverage_of(ok_f, uctx, w1) == {c6cand["cid"]}, str(w1))
        noq_f = dict(ok_f, evidence=dict(ok_f["evidence"], player_quote=""))
        w1b: list[str] = []
        check("empty quote rejected when the player wrote words that turn",
              coverage_of(noq_f, uctx, w1b) == set()
              and any("no player_quote" in w for w in w1b), str(w1b))
        bad_f = dict(ok_f, evidence=dict(ok_f["evidence"],
                                         oracle="something vague"))
        w2: list[str] = []
        check("unanchored oracle prose is rejected",
              coverage_of(bad_f, uctx, w2) == set()
              and any("RECORDED oracle data" in w for w in w2), str(w2))
        tag_f = dict(ok_f, evidence=dict(
            ok_f["evidence"],
            oracle="clearly a bad-outcome-join / silent-failure-join here"))
        w3: list[str] = []
        check("a harness join tag alone does not anchor oracle evidence",
              coverage_of(tag_f, uctx, w3) == set(), str(w3))

        # NO candidate skips oracle validation: even a note-only one
        # has anchors by construction (the recorded ABSENCE fragments),
        # so fabricated oracle prose cannot cover it
        u2 = ValidationCtx(cands, turns, [{"call": 1, "frames": [7]}])
        check("every candidate has a non-empty anchor set",
              all(u2.anchors_by_cid[c["cid"]] for c in cands),
              str({c["cid"]: sorted(u2.anchors_by_cid[c["cid"]])
                   for c in cands if not u2.anchors_by_cid[c["cid"]]}))
        c7_quote = "I feel a bit lost. Nothing on this screen tells me " \
                   "what I should do next."
        fab7 = {"title": "n", "category": "other", "severity": "polish",
                "verdict": "intended", "confidence": "high",
                "adjudication_call": 1,
                "evidence": {"turns": [7], "candidate_ids": [c7["cid"]],
                             "player_quote": c7_quote,
                             "oracle": "made-up oracle fact"},
                "root_cause_hypothesis": ""}
        w7: list[str] = []
        check("fabricated oracle on a note-only candidate is rejected",
              coverage_of(dict(fab7), u2, w7) == set()
              and any("RECORDED oracle data" in w for w in w7), str(w7))
        ok7 = dict(fab7, evidence=dict(
            fab7["evidence"],
            oracle="the record shows no signals at all: events=[], "
                   "outcomes=[], and no visible frame change"))
        w8: list[str] = []
        check("absence-fragment oracle covers the note-only candidate",
              coverage_of(ok7, u2, w8) == {c7["cid"]}, str(w8))

        # a crash BEFORE the first recorded turn must still be
        # adjudicable: its candidate turn has no turn record, no frame,
        # and no player words — an empty quote is legitimately allowed
        crash_cands = friction_candidates(
            {"stop_reason": "engine_crash", "turns": 0,
             "crash_detail": "boom"}, [])
        check("pre-first-turn crash yields a candidate",
              len(crash_cands) == 1 and crash_cands[0]["turn"] == 0,
              str(crash_cands))
        cctx = ValidationCtx(crash_cands, [], [{"call": 1, "frames": []}])
        crash_f = {"title": "c", "category": "crash", "severity": "blocker",
                   "verdict": "defect", "confidence": "high",
                   "adjudication_call": 1,
                   "evidence": {"turns": [0],
                                "candidate_ids": [crash_cands[0]["cid"]],
                                "player_quote": "",
                                "oracle": "the engine crashed at startup: boom"},
                   "root_cause_hypothesis": ""}
        w4: list[str] = []
        check("pre-first-turn crash candidate is coverable",
              coverage_of(crash_f, cctx, w4) == {crash_cands[0]["cid"]},
              str(w4))

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
