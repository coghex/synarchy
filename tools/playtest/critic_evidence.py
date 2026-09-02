"""Evidence validation and deterministic rendering for the playtest
critic (#648, extracted by #2069): what makes the report trustworthy.

Owns the mechanical half of the evidence discipline the system prompt
announces, and the rendering that keeps `report.md` and `findings.json`
in agreement:

  * normalization and the recorded-data ANCHORS (`_anchor_strings`) —
    verbatim atoms from the candidate's recorded oracle, or the
    explicit recorded-absence fragments for a candidate with none;
  * `ValidationCtx` — the recorded trace data findings are checked
    against, including which frames each adjudication call was shown;
  * `coverage_of` — per-finding coverage: candidate turn coverage,
    verbatim player words when present, an anchored oracle record, and
    adjudication-call frame ownership; violations strip coverage, warn,
    and demote the finding to low confidence;
  * `validate_findings` / `uncovered` — enum validation and the
    uncovered-candidate detection that drives the bounded repair pass;
  * `reconcile_verdicts` — one candidate, one published verdict (#1873):
    agreeing duplicates publish with a warning, conflicting verdicts are
    withdrawn candidate-scoped;
  * `assign_ids` — stable finding ids by verdict, severity, first turn;
  * `render_report` — the Markdown report with its defect and intended
    sections, screenshot references and critic warnings.

Consumes `critic_contract` (the enums) and the recorded trace data it
is handed; never model transport or orchestration.
"""
from __future__ import annotations

import json
import os
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from critic_contract import CATEGORIES, CONFIDENCES, SEVERITIES, VERDICTS  # noqa: E402


def _norm(s: str) -> str:
    return " ".join((s or "").lower().split())


def _anchor_strings(cand: dict) -> set[str]:
    """Verbatim atoms from the candidate's RECORDED oracle data — a
    finding's oracle prose must contain at least one of these, so
    fabricated 'evidence' the trace never recorded can't pass
    coverage. Only recorded data qualifies (outcome values/reasons/
    kinds, event texts/cats, the clicked widget's label/id, crash
    details) — harness-derived join tags are NOT anchors, since
    parroting a tag proves nothing about the record. A candidate with
    no recorded atoms at all (e.g. a pure stuck-loop) has an empty set
    and the anchor check is skipped for it."""
    atoms: list[str] = []
    o = cand.get("oracle") or {}
    for rec in o.get("outcomes") or []:
        if isinstance(rec, dict):
            # `kind` is the F4 record's action identifier (the live
            # engine + canned fixture spelling); outcome/reason are its
            # verdict and cause. All three are recorded, so all three
            # ground a finding.
            for k in ("outcome", "reason", "kind"):
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
            f"visual_change={o.get('visual_change')}"))
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
    # one candidate, one published verdict (#1873) — run BEFORE
    # uncovered() is consulted, so a conflicted candidate reaches the
    # repair pass exactly as an unadjudicated one does
    reconcile_verdicts(data, warnings)
    missing = [c["cid"] for c in uncovered(data, candidates)]
    return data, warnings + ([f"unadjudicated candidates: {', '.join(missing)}"]
                             if missing else [])


def uncovered(data: dict, candidates: list[dict]) -> list[dict]:
    """Candidates not covered by a VALIDLY grounded finding (per
    coverage_of — computed into f['covers'] during validation)."""
    covered = {cid for f in data.get("findings", [])
               for cid in f.get("covers", [])}
    return [c for c in candidates if c["cid"] not in covered]


def reconcile_verdicts(data: dict, warnings: list[str]) -> None:
    """One friction candidate, one published verdict (#1873).

    `validate_findings` grounds each finding independently and
    `uncovered` reduces coverage to a set union, so two separately
    valid findings could adjudicate the SAME candidate as `defect` and
    `intended` at once — and the duplicate coverage *suppressed* the
    repair pass instead of triggering it. Both verdicts were then
    printed, one per report section, with no candidate id rendered to
    tell the maintainer they described one observed moment.

    Detection is by candidate identity over the evidence-VALID
    `f["covers"]` (never the raw `evidence.candidate_ids`, which
    deliberately retains stripped claims for audit), and it is
    candidate-scoped: a finding covering C1 and C2 where only C1
    conflicts keeps C2 and stays publishable.

      * verdicts AGREE — published as-is with a warning. Two distinct
        defects observed at one friction moment stay expressible; the
        warning is the audit trail, not an error.
      * verdicts DIFFER — the candidate's adjudication is withdrawn
        from every claimant, which makes it uncovered so the existing
        bounded repair pass re-asks for it. A claimant left covering
        nothing is withdrawn from publication entirely, so neither
        report.md nor findings.json presents a valid opposite-verdict
        pair for one candidate.

    Withdrawals are recorded on the finding (`conflict_withdrawn`) and
    honoured on the repair round's re-validation: the first-pass
    conflict is settled once, so the repair finding is the replacement
    adjudication rather than a third party to the same argument. Their
    warning is re-emitted from that record, so it survives a SUCCESSFUL
    repair — necessary because `render_report` prints no candidate ids.
    A conflict the repair round repeats simply leaves the candidate
    unadjudicated; there is no second repair.

    Deterministic by construction: candidates are walked in sorted id
    order and claimants in `data["findings"]` order.
    """
    findings = data.get("findings") or []

    # honour earlier rounds' withdrawals before anything else
    for f in findings:
        already = set(f.get("conflict_withdrawn") or [])
        if already:
            f["covers"] = [c for c in (f.get("covers") or [])
                           if c not in already]

    by_cid: dict[str, list[dict]] = {}
    for f in findings:
        for cid in f.get("covers") or []:
            by_cid.setdefault(cid, []).append(f)

    for cid in sorted(by_cid):
        claimants = by_cid[cid]
        if len(claimants) < 2:
            continue
        verdicts = {f["verdict"] for f in claimants}
        if len(verdicts) == 1:
            warnings.append(
                f"candidate {cid} is adjudicated by {len(claimants)} findings "
                f"that AGREE ({sorted(verdicts)[0]}): "
                + "; ".join(repr(f["title"]) for f in claimants)
                + " — all published (one friction moment can expose more "
                  "than one distinct issue)")
            continue
        for f in claimants:
            f["conflict_withdrawn"] = sorted(
                set(f.get("conflict_withdrawn") or []) | {cid})
            f["covers"] = [c for c in f["covers"] if c != cid]

    withdrawn: dict[str, list[dict]] = {}
    for f in findings:
        for cid in f.get("conflict_withdrawn") or []:
            withdrawn.setdefault(cid, []).append(f)
    for cid in sorted(withdrawn):
        warnings.append(
            f"candidate {cid} was adjudicated CONFLICTINGLY by "
            f"{len(withdrawn[cid])} findings ("
            + "; ".join(f"{f['verdict']}: {f['title']!r}"
                        for f in withdrawn[cid])
            + ") — none of them publishes a verdict for it; whether the "
              "bounded repair pass then adjudicated it is answered by "
              "the unadjudicated-candidates warning")

    data["findings"] = [f for f in findings
                        if f.get("covers") or not f.get("conflict_withdrawn")]


_SEV_RANK = {s: i for i, s in enumerate(SEVERITIES)}


def assign_ids(findings: list[dict]) -> None:
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
