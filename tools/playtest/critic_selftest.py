"""Offline self-test for the playtest critic (#648, extracted by
#2069) — test support, never imported by a production run.

`python3 tools/playtest/critic.py --selftest` is still the documented
entry point; the façade imports this module lazily inside that branch
only, so a plain `critic.py <trace_dir>` run never loads the fake
critics or the canned fixture. The whole run stays offline: no Cabal
build, no engine boot, no GPU or window, no network request, no model
login or API key.

It owns the deterministic test doubles (`FakeCritic`, and the
lazy/ungrounded/one-shot/duplicating critics defined inside `selftest`)
and every check, in one sequence over one canned trace, exactly as the
pre-split `selftest()` ran them: the click-correlation matrix
(#783/#749/#1750), the signal and candidate joins on the planted turns,
the F4 outcome-key and #730 input-routing regressions, the digest's
widget dedup, batch honesty and the two-frame floor (#775), the full
pipeline against the fake critic, coverage repair, the evidence
discipline's negatives, one-verdict-per-candidate reconciliation
(#1873), the anchoring unit checks and the pre-first-turn crash.

Consumes the production owners (`critic_click`, `critic_signals`,
`critic_evidence`, `critic_pipeline`, `critic_contract`) plus the
`canned_trace` fixture and the `trace` loader.
"""
from __future__ import annotations

import hashlib
import json
import os
import re
import sys
import tempfile

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from canned_trace import build_canned_trace  # noqa: E402
from critic_click import is_left_click, routing_aware_records, widget_at  # noqa: E402
from critic_contract import CATEGORIES, SEVERITIES, VERDICTS  # noqa: E402
from critic_evidence import ValidationCtx, coverage_of, uncovered  # noqa: E402
from critic_pipeline import run_critic  # noqa: E402
from critic_signals import (build_digest, build_signals,  # noqa: E402
                            friction_candidates, plan_batches)
from trace import load_meta, load_turns  # noqa: E402


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


def selftest() -> int:
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

    # #783: passive label/panel records (control=False) can never
    # satisfy correlation, even covering the click.
    passive_panel = {"id": "panel:p1", "type": "panel", "control": False,
                      "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}
    check("a passive panel record alone does not correlate to a click over it (#783)",
          widget_at([passive_panel], 10, 10) is None)
    passive_label = {"id": "label:l1", "type": "label", "control": False,
                      "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}
    check("a passive label record alone does not correlate to a click over it (#783)",
          widget_at([passive_label], 10, 10) is None)

    # #783: a shown-but-disabled control (enabled=False) is still
    # correlation-eligible — disabled-ness explains a dead click, it
    # doesn't remove the control.
    disabled_button = {"id": "button:b1", "type": "button", "control": True,
                        "enabled": False, "visible": True,
                        "bounds": {"x": 0, "y": 0, "w": 50, "h": 50}, "paintKey": 5}
    check("a shown-but-disabled control is still identified at its bounds (#783)",
          widget_at([disabled_button], 10, 10) is disabled_button)

    # #783: overlapping controls resolve by paintKey (the engine's own
    # page-band + accumulated-zIndex hit-test key), not by list order.
    lo_control = {"id": "button:lo", "control": True, "paintKey": 10,
                  "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}
    hi_control = {"id": "button:hi", "control": True, "paintKey": 20000,
                  "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}
    check("overlapping controls resolve to the higher paintKey (#783)",
          widget_at([lo_control, hi_control], 10, 10) is hi_control)
    check("...independent of the dump list's own order (#783)",
          widget_at([hi_control, lo_control], 10, 10) is hi_control)

    # #783 (round-1 review): paintKey alone is NOT a total order —
    # ordinary siblings sharing a band and zIndex tie on it, exactly
    # like two default-zIndex buttons on the same page. topHitBy
    # breaks that tie by paint order (later wins); widget_at must
    # match, using paintOrder, regardless of the dump list's order.
    early_tied = {"id": "button:early", "control": True, "paintKey": 10,
                  "paintOrder": 1, "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}
    later_tied = {"id": "button:later", "control": True, "paintKey": 10,
                  "paintOrder": 2, "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}
    check("equal paintKey resolves by the higher paintOrder (later-painted wins) (#783)",
          widget_at([early_tied, later_tied], 10, 10) is later_tied)
    check("...independent of the dump list's own (reversed) order (#783)",
          widget_at([later_tied, early_tied], 10, 10) is later_tied)

    # #783: a normal, non-overlapping enabled control resolves on its
    # own bounds; a hidden/inactive record never shadows a real hit and
    # is ineligible on its own.
    normal_control = {"id": "button:n1", "control": True, "paintKey": 0,
                       "visible": True, "bounds": {"x": 200, "y": 200, "w": 50, "h": 50}}
    check("a normal non-overlapping enabled control resolves normally (#783)",
          widget_at([normal_control], 210, 210) is normal_control)
    hidden_control = {"id": "button:h1", "control": True, "paintKey": 999,
                       "visible": False, "bounds": {"x": 200, "y": 200, "w": 50, "h": 50}}
    check("a hidden control alone is ineligible, and never shadows a visible one at the same spot (#783)",
          widget_at([hidden_control], 210, 210) is None
          and widget_at([hidden_control, normal_control], 210, 210) is normal_control)

    # #749: when a record carries interactiveBounds (the effective
    # expanded-visual pointer rect of a migrated box-backed control), the
    # join tests containment against IT, not the content-only `bounds` —
    # so a click on the visible border correlates to the control. A
    # record without interactiveBounds keeps using `bounds`.
    migrated = {"id": "button:m1", "control": True, "paintKey": 0,
                "bounds": {"x": 20, "y": 20, "w": 60, "h": 60},
                "interactiveBounds": {"x": 4, "y": 4, "w": 92, "h": 92}}
    check("a click on the expanded interactive border correlates (outside content bounds) (#749)",
          widget_at([migrated], 10, 10) is migrated)
    check("...and a click outside even the interactive bounds still misses (#749)",
          widget_at([migrated], 2, 2) is None)
    content_only = {"id": "button:c1", "control": True, "paintKey": 0,
                    "bounds": {"x": 20, "y": 20, "w": 60, "h": 60}}
    check("a record with no interactiveBounds falls back to content bounds (#749)",
          widget_at([content_only], 10, 10) is None
          and widget_at([content_only], 30, 30) is content_only)
    # #749 (review r2): a fully-clipped / collapsed control is marked
    # non-hittable (interactiveBounds == False) by the engine — the
    # oracle must NOT correlate a click over its content bounds to it,
    # because the real router cannot hit it either.
    nonhittable = {"id": "button:nh", "control": True, "paintKey": 999,
                   "bounds": {"x": 20, "y": 20, "w": 60, "h": 60},
                   "interactiveBounds": False}
    check("a non-hittable (False interactiveBounds) control is skipped, never falls back to content bounds (#749)",
          widget_at([nonhittable], 30, 30) is None
          and widget_at([nonhittable, content_only], 30, 30) is content_only)
    degenerate = {"id": "button:dg", "control": True, "paintKey": 999,
                  "bounds": {"x": 20, "y": 20, "w": 60, "h": 60},
                  "interactiveBounds": {"x": 20, "y": 20, "w": 60, "h": 0}}
    check("a degenerate (zero-extent) interactiveBounds is skipped (#749)",
          widget_at([degenerate], 30, 20) is None)

    # ---- #1750: routing-aware left-click correlation ----------------
    # Every fixture below carries the engine-owned routing facts
    # (inScope/pointerBlocking/leftClickTarget/leftClickAffordance) the
    # producer now emits, so `routing_aware_records` selects the
    # route-aware join. Geometry is shared: all of them cover (10, 10).
    def rec(rid, **kw):
        base = {"id": rid, "control": True, "visible": True,
                "bounds": {"x": 0, "y": 0, "w": 100, "h": 100},
                "inScope": True, "pointerBlocking": False,
                "leftClickTarget": False, "leftClickAffordance": False,
                "paintKey": 0, "paintOrder": 0}
        base.update(kw)
        return base

    # An ACTIVE left control: clickable + an onClick callback, which is
    # exactly what makes elementBlocksPointer fire.
    def active(rid, **kw):
        return rec(rid, pointerBlocking=True, leftClickTarget=True,
                   leftClickAffordance=True, enabled=True, **kw)

    check("routing_aware_records detects the new facts, and rejects a legacy set",
          routing_aware_records([active("button:a")])
          and not routing_aware_records([{"id": "button:legacy", "control": True}])
          and not routing_aware_records("nope"))

    # 5.1/5.4 — empty exclusive-modal space. The modal itself owns the
    # boundary; the HUD control below reports inScope=False, so the
    # click correlates to nothing rather than to an unreachable button.
    hud_under_modal = active("button:hud", inScope=False, paintKey=100)
    check("empty exclusive-modal space never correlates a lower-page control (#1750)",
          widget_at([hud_under_modal], 10, 10, route_aware=True) is None)
    check("...while the same record still correlates on the legacy path",
          widget_at([hud_under_modal], 10, 10) is hud_under_modal)

    # 5.1/5.4 — a passive, callback-less pointer blocker (an explicit
    # UI.setPointerBlocking panel) suppresses every lower control. It
    # is admitted to the dump as control=False occlusion evidence.
    passive_blocker = rec("element:blocker", control=False,
                          pointerBlocking=True, paintKey=20000)
    lower_button = active("button:lower", paintKey=10)
    check("a passive callback-less blocker suppresses the control below it (#1750)",
          widget_at([lower_button, passive_blocker], 10, 10,
                    route_aware=True) is None)
    check("...and is never itself the correlated control (control=False) (#1750)",
          widget_at([passive_blocker], 10, 10, route_aware=True) is None)
    check("...independent of the dump list's own order (#1750)",
          widget_at([passive_blocker, lower_button], 10, 10,
                    route_aware=True) is None)

    # 5.5 vs 5.1 — a DISABLED control that did not opt into blocking is
    # not a pointer surface at all (setClickable(false) drops the
    # implicit block), so the enabled control below it wins, exactly as
    # routePointer would resolve it.
    disabled_nonblocking = rec("button:disabled_hi", enabled=False,
                               leftClickAffordance=True, paintKey=30000)
    check("a lower enabled control wins over a higher NON-blocking disabled control (#1750)",
          widget_at([lower_button, disabled_nonblocking], 10, 10,
                    route_aware=True) is lower_button)
    check("...whereas the legacy join would have picked the higher disabled one",
          widget_at([lower_button, disabled_nonblocking], 10, 10)
          is disabled_nonblocking)

    # 5.3 — a disabled control that DID opt into pointer blocking
    # consumes the click; #783 keeps it correlatable as the affordance
    # that explains the dead click, and the control below stays
    # unreachable.
    disabled_blocking = rec("button:disabled_block", enabled=False,
                            pointerBlocking=True, leftClickAffordance=True,
                            paintKey=30000)
    check("an explicitly blocking disabled control suppresses the lower control "
          "and correlates to itself (#1750)",
          widget_at([lower_button, disabled_blocking], 10, 10,
                    route_aware=True) is disabled_blocking)

    # 5.5 — a lone shown disabled affordance, with nothing blocking
    # anywhere: #783's behavior is preserved unchanged.
    check("a lone shown disabled affordance is still correlatable (#783 preserved)",
          widget_at([disabled_nonblocking], 10, 10,
                    route_aware=True) is disabled_nonblocking)

    # 5.4, explicit: in scope, shown, control=True, but neither an
    # effective blocking surface nor a disabled LEFT affordance — e.g.
    # an enabled control carrying only a right-click callback. Nothing
    # correlates (routePointer's RouteMiss); the fallback must not
    # widen to enabled non-blocking controls.
    right_only_nonblocking = rec("button:rightonly", enabled=True,
                                 paintKey=40)
    check("an in-scope shown control that neither blocks nor offers a left "
          "affordance correlates to nothing (#1750, RouteMiss)",
          widget_at([right_only_nonblocking], 10, 10, route_aware=True) is None)
    # ...and a right-click-only control that DOES block (clickable +
    # onRightClick makes elementBlocksPointer fire) suppresses the
    # lower control without correlating itself.
    right_only_blocker = rec("button:rightblock", enabled=True,
                             pointerBlocking=True, paintKey=40000)
    check("a blocking right-click-only control suppresses the lower control "
          "and correlates nothing (#1750)",
          widget_at([lower_button, right_only_blocker], 10, 10,
                    route_aware=True) is None)

    # Requirement 7 — a record set with no routing fields keeps the
    # legacy (paintKey, paintOrder) winner. Asserted as the actual
    # winner, not merely "no exception": treating absent pointerBlocking
    # as False would drop to the disabled-affordance fallback and pick
    # the DISABLED record over the enabled one.
    legacy_enabled = {"id": "button:legacy_enabled", "control": True,
                      "visible": True, "enabled": True, "paintKey": 10,
                      "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}
    legacy_disabled = {"id": "button:legacy_disabled", "control": True,
                       "visible": True, "enabled": False, "paintKey": 5,
                       "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}
    legacy_set = [legacy_disabled, legacy_enabled]
    check("a legacy record set is not routing-aware and keeps its "
          "(paintKey, paintOrder) winner (#1750 req 7)",
          not routing_aware_records(legacy_set)
          and widget_at(legacy_set, 10, 10) is legacy_enabled)

    # Requirement 1/8 — the route-aware contract covers default and
    # explicit left clicks only. Everything else keeps the legacy join.
    check("is_left_click accepts an omitted button and an explicit left",
          is_left_click({"do": "click"})
          and is_left_click({"do": "click", "button": None})
          and is_left_click({"do": "click", "button": ""})
          and is_left_click({"do": "click", "button": "left"})
          and is_left_click({"do": "click", "button": "Left"}))
    check("is_left_click rejects other buttons, drag, and unusable values",
          not is_left_click({"do": "click", "button": "right"})
          and not is_left_click({"do": "click", "button": "middle"})
          and not is_left_click({"do": "click", "button": " left "})
          and not is_left_click({"do": "click", "button": 1})
          and not is_left_click({"do": "drag", "button": "left"}))

    # The build_signals join end to end: which record set it reads, and
    # when it goes route-aware. `routing_widgets` is the pre-injection
    # capture; `widgets` here is a deliberately DIFFERENT (post-callback)
    # set, so preferring the wrong one is visible in the result.
    def click_turn(n, oracle, **action_kw):
        action = {"do": "click", "x": 10, "y": 10}
        action.update(action_kw)
        return {"turn": n, "player": {"action": action}, "oracle": oracle}

    post_only = active("button:post_only", paintKey=1)
    routing_turns = [
        # 1: routing set says the HUD control is under a modal → no
        # correlation, even though the post-injection set (the modal
        # closed itself) shows a plainly clickable button.
        click_turn(1, {"routing_widgets": [hud_under_modal],
                       "widgets": [post_only]}),
        # 2: no routing_widgets key at all (a pre-#1750 trace) → the
        # legacy join over `widgets`.
        click_turn(2, {"widgets": legacy_set}),
        # 3: right-click over the same routing set → legacy join, which
        # does correlate the out-of-scope record.
        click_turn(3, {"routing_widgets": [hud_under_modal],
                       "widgets": [post_only]}, button="right"),
    ]
    with tempfile.TemporaryDirectory() as rtmp:
        rsig = build_signals(rtmp, routing_turns)
    check("the click join prefers the pre-injection routing record set (#1750)",
          rsig[0]["clicked_widget"] is None,
          str(rsig[0]["clicked_widget"]))
    check("a trace with no routing_widgets falls back to `widgets` and the "
          "legacy winner (#1750 req 7)",
          rsig[1]["clicked_widget"] is legacy_enabled,
          str(rsig[1]["clicked_widget"]))
    check("a right click keeps the legacy join even on a routing-aware set "
          "(#1750 req 8)",
          rsig[2]["clicked_widget"] is hud_under_modal,
          str(rsig[2]["clicked_widget"]))

    with tempfile.TemporaryDirectory() as tmp:
        tdir = build_canned_trace(os.path.join(tmp, "trace"))
        meta = load_meta(tdir)
        turns = load_turns(tdir)
        signals = build_signals(tdir, turns)
        cands = friction_candidates(meta, signals)

        # #775: the expectation-vs-reality join must consume the
        # correctly associated visible-change for each turn — the
        # runner's own per-turn `oracle.visual_change` (turns 3 and 6
        # visibly change; every other turn in the fixture does not) —
        # not a cross-turn hashes[i+1] guess.
        by_turn_signal = {s["turn"]: s for s in signals}
        check("build_signals derives visual_change from this turn's own "
              "oracle field for every planted turn",
              [by_turn_signal[n]["visual_change"] for n in range(1, 8)]
              == [False, False, True, False, False, True, False],
              str({n: by_turn_signal[n]["visual_change"] for n in range(1, 8)}))

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
            "acks": [{"ok": True}], "events": [], "event_log_gaps": [],
            "outcomes": [],
            "bad_outcomes": [], "ack_errors": [],
            "visual_change": True, "clicked_widget": None,
            "widgets": [], "current_menu": "world_view", "paused": True,
            "stuck": False,
        }
        accepted_click = dict(base_click,
                              outcomes=[{"kind": "move", "outcome": "accepted"}])
        check("world click with accepted outcome is no phantom candidate",
              friction_candidates({}, [accepted_click]) == [])
        check("outcome-less world click with visible effect is no candidate",
              friction_candidates({}, [base_click]) == [])

        # F4 oracle-key regression (#726): the live producer
        # (PlaytestEngine.oracle_events) writes action outcomes under
        # `action_outcomes`; a stale critic read of `outcomes` dropped
        # every real F4 record before candidate generation. Drive the
        # issue's own repro through the real build_signals +
        # friction_candidates path (no engine, no key) for each oracle
        # shape a trace can carry.
        rec = {"kind": "marker.place", "outcome": "rejected",
               "reason": "insufficient materials"}

        def _f4_turn(oracle):
            return {"turn": 1, "screenshot": "missing.png",
                    "player": {"action": {"do": "wait"}, "note": "",
                               "observation": "", "expectation": ""},
                    "oracle": oracle}

        for label, oracle in (
                ("canonical action_outcomes", {"action_outcomes": [rec]}),
                ("legacy outcomes fallback", {"outcomes": [rec]}),
                ("both keys (same record)",
                 {"action_outcomes": [rec], "outcomes": [rec]})):
            f4_sig = build_signals(tmp, [_f4_turn(oracle)])
            f4_cands = friction_candidates({}, f4_sig)
            check(f"F4 {label}: exactly one outcome record reaches the signal",
                  len(f4_sig[0]["outcomes"]) == 1, str(f4_sig[0]["outcomes"]))
            check(f"F4 {label}: rejected outcome yields one friction candidate",
                  len(f4_cands) == 1
                  and any(r.startswith("silent-failure-join")
                          or r.startswith("bad-outcome-join")
                          for r in f4_cands[0]["reasons"]),
                  str(f4_cands and f4_cands[0]["reasons"]))
        # an intentionally-empty canonical list wins over a legacy list:
        # records the live producer drained to nothing must not be
        # resurrected by the both-present fallback.
        empty_sig = build_signals(tmp, [_f4_turn(
            {"action_outcomes": [], "outcomes": [rec]})])
        check("F4 empty canonical action_outcomes suppresses legacy fallback",
              empty_sig[0]["outcomes"] == []
              and friction_candidates({}, empty_sig) == [],
              str(empty_sig[0]["outcomes"]))

        # #730: a non-click Layer A input-routing outcome must reach the
        # SAME production join `rec` above exercises for a "marker.place"
        # world action — not a click, and not tested in isolation from
        # build_signals/friction_candidates. Shaped exactly like what
        # Engine.Input.Thread.recordKeyOutcome pushes when a key reaches
        # a recognized text-input domain but matches none of its editing
        # actions (a real silent-keyboard-routing-failure shape, the
        # regression class #730 exists to make visible).
        key_rec = {"kind": "input.key", "outcome": "noop",
                   "handler": "shell_text",
                   "reason": "shell_text: key matched no recognized action"}
        key_sig = build_signals(tmp, [_f4_turn({"action_outcomes": [key_rec]})])
        key_cands = friction_candidates({}, key_sig)
        check("non-click Layer A (input.key noop) reaches the signal",
              len(key_sig[0]["outcomes"]) == 1
              and key_sig[0]["bad_outcomes"] == [key_rec],
              str(key_sig[0]))
        check("non-click Layer A (input.key noop) yields a friction candidate "
              "through the real join (#730)",
              len(key_cands) == 1
              and any(r.startswith("silent-failure-join")
                      or r.startswith("bad-outcome-join")
                      for r in key_cands[0]["reasons"]),
              str(key_cands and key_cands[0]["reasons"]))

        # #783: a dead click covered ONLY by a passive panel/label
        # record — through the real build_signals -> friction_candidates
        # path — must still join phantom-affordance-join, not be
        # suppressed by the passive record satisfying the old
        # first-match join.
        def _click_turn(x, y, widgets, outcome):
            return {"turn": 1, "screenshot": "missing.png",
                    "player": {"action": {"do": "click", "x": x, "y": y},
                               "note": "", "observation": "", "expectation": ""},
                    "oracle": {"widgets": widgets,
                               "action_outcomes": [{"kind": "click",
                                                     "outcome": outcome}]}}

        dead_behind_panel_sig = build_signals(tmp, [_click_turn(
            10, 10,
            [{"id": "panel:p1", "type": "panel", "control": False,
              "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}],
            "deadclick")])
        dead_behind_panel_cands = friction_candidates({}, dead_behind_panel_sig)
        check("dead click covered only by a passive panel still joins "
              "phantom-affordance-join (#783)",
              dead_behind_panel_sig[0]["clicked_widget"] is None
              and len(dead_behind_panel_cands) == 1
              and any(r.startswith("phantom-affordance-join")
                      for r in dead_behind_panel_cands[0]["reasons"]),
              str(dead_behind_panel_sig[0]["clicked_widget"]))

        dead_behind_label_sig = build_signals(tmp, [_click_turn(
            10, 10,
            [{"id": "label:l1", "type": "label", "control": False,
              "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}}],
            "deadclick")])
        dead_behind_label_cands = friction_candidates({}, dead_behind_label_sig)
        check("dead click covered only by a passive label still joins "
              "phantom-affordance-join (#783)",
              dead_behind_label_sig[0]["clicked_widget"] is None
              and len(dead_behind_label_cands) == 1
              and any(r.startswith("phantom-affordance-join")
                      for r in dead_behind_label_cands[0]["reasons"]),
              str(dead_behind_label_sig[0]["clicked_widget"]))

        # #783: overlapping controls/pages through the real join — the
        # click lands on the topmost (higher-paintKey) control, and
        # correlates it (no phantom-affordance-join), independent of
        # which order the dump listed them in.
        overlap_widgets = [
            {"id": "button:hi", "type": "button", "control": True,
             "paintKey": 20000, "enabled": True,
             "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}},
            {"id": "button:lo", "type": "button", "control": True,
             "paintKey": 10, "enabled": True,
             "bounds": {"x": 0, "y": 0, "w": 100, "h": 100}},
        ]
        overlap_sig = build_signals(tmp, [_click_turn(
            10, 10, overlap_widgets, "accepted")])
        check("an overlapping click correlates to the topmost (higher "
              "paintKey) control regardless of dump order (#783)",
              overlap_sig[0]["clicked_widget"] is not None
              and overlap_sig[0]["clicked_widget"]["id"] == "button:hi",
              str(overlap_sig[0]["clicked_widget"]))

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

        # #775 (pr-review rounds 1-2): a single candidate can need up to
        # two frames of its own (pre + post-step, #775) — a budget
        # below that floor could never show both for even one
        # candidate, so an under-budget request is CLAMPED to 2 (with
        # an honest warning), guaranteeing every candidate's own+post
        # pair lands together rather than merely warning when one is
        # dropped.
        batches1, bwarn1 = plan_batches(tdir, turns, cands, max_frames=1)
        check("max_frames below the 2-frame floor is clamped, with a "
              "warning explaining why",
              any("raised to 2" in w for w in bwarn1), str(bwarn1))
        check("every candidate still gets BOTH its own pre-step and "
              "post-step frame even when max_frames was requested as 1",
              all(any(n == c["turn"] for n, _ in frames)
                  and any(n == -c["turn"] for n, _ in frames)
                  for subset, frames in batches1 for c in subset))
        check("no per-candidate frame-starvation warning fires once "
              "clamped (the clamp itself is the only warning)",
              not any("post-step screenshot" in w or "adjudicated WITHOUT"
                      in w for w in bwarn1),
              str(bwarn1))

        # #775: the fixture's LAST turn (7) still gets its own post-step
        # frame shown to the critic — impossible before this fix, which
        # had no turn 8 to borrow "the visible result" from.
        check("the final turn's own post-step frame is offered to the "
              "critic (no following turn needed)",
              any(n == -c7["turn"] for subset, frames in batches
                  for n, _ in frames))

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
        # a ref is either a turn's pre-step "screenshot" (positive key)
        # or that SAME turn's post-step "post_screenshot" (#775,
        # negative key — see critic_signals.plan_batches) — map back by matching
        # against the known trace records rather than parsing
        # filenames, since "..._post.png" doesn't fit the old
        # trailing-number convention.
        by_turn_ref = {t.get("turn"): t for t in turns}

        def _ref_key(ref):
            for n, t in by_turn_ref.items():
                if t.get("screenshot") == ref:
                    return n
                if (t.get("oracle") or {}).get("post_screenshot") == ref:
                    return -n
            return None

        check("findings only attach screenshots their call actually saw",
              all(_ref_key(ref) in fbc.get(f_["adjudication_call"], set())
                  for f_ in data["findings"]
                  for ref in f_.get("screenshots", [])))

        # #2220: a report's evidence images must resolve from wherever
        # the report was written. `_ref_key` doubles as the "spelled
        # exactly as the trace records it" oracle: it matches a ref
        # against the trace's own `screenshot` / `post_screenshot`
        # strings and returns None for anything rebased or invented.
        IMG_REF = re.compile(r"!\[[^\]]*\]\(([^)]+)\)")
        default_refs = IMG_REF.findall(report)
        check("the default-location report still spells every image link "
              "exactly as the trace records it (#2220 changes nothing here)",
              bool(default_refs)
              and all(_ref_key(r) is not None for r in default_refs),
              str([r for r in default_refs if _ref_key(r) is None][:3]))

        def _trace_files():
            # content-addressed, so "unmodified" means the bytes are
            # unchanged rather than merely the timestamps
            snap = {}
            for root, _dirs, names in os.walk(tdir):
                for name in names:
                    path = os.path.join(root, name)
                    with open(path, "rb") as fh:
                        snap[os.path.relpath(path, tdir)] = hashlib.sha256(
                            fh.read()).hexdigest()
            return snap

        # the two path relationships `--out` can name that the default
        # run above does not cover: a directory beside the trace, and
        # one nested inside it.
        before_out = _trace_files()
        for where, out_dir, outside in (
                ("a sibling dir", os.path.join(tmp, "elsewhere"), True),
                ("a dir inside the trace",
                 os.path.join(tdir, "nested-report"), False)):
            rp_o, fp_o = run_critic(tdir, FakeCritic(), out_dir=out_dir)
            base = os.path.dirname(rp_o)
            refs = IMG_REF.findall(open(rp_o).read())
            check(f"a report written to {where} still references screenshots",
                  bool(refs))
            check("...every image target is relative, never a "
                  f"machine-specific absolute path ({where})",
                  all(not os.path.isabs(r) for r in refs), str(refs[:3]))
            unresolved = [r for r in refs
                          if not os.path.exists(os.path.join(base, r))]
            check("...and resolves to an existing file from the report's "
                  f"own directory ({where})",
                  not unresolved, str(unresolved[:3]))
            inside = os.path.abspath(tdir) + os.sep
            check("...each landing on a trace-owned frame, nothing copied "
                  f"out ({where})",
                  all(os.path.abspath(os.path.join(base, r)).startswith(inside)
                      for r in refs))
            with open(fp_o) as f:
                data_o = json.load(f)
            attached = [ref for f_ in data_o["findings"]
                        for ref in f_.get("screenshots", [])]
            check("findings.json keeps its trace-relative screenshot paths "
                  f"({where})",
                  bool(attached)
                  and all(_ref_key(ref) is not None for ref in attached),
                  str([r for r in attached if _ref_key(r) is None][:3]))
            check(f"...with evidence.screenshots still mirroring them ({where})",
                  all(f_["evidence"]["screenshots"] == f_["screenshots"]
                      for f_ in data_o["findings"]))
            check("...and adjudication_calls still keyed by signed turn "
                  f"numbers ({where})",
                  all(isinstance(n, int) and not isinstance(n, bool)
                      for a in data_o["adjudication_calls"]
                      for n in a["frames"]))
            if outside:
                # the trace stays the sole owner of its frames: an
                # --out run beside it neither rewrites nor removes a
                # single file under it (a nested --out legitimately
                # adds its own two artifacts, so it is exempt).
                check("...leaving every file under the trace untouched "
                      f"({where})",
                      _trace_files() == before_out)

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

        # max_frames generous enough for ONE main-pass call (#775: each
        # candidate now needs its own pre AND post frame, no more
        # sharing with a neighboring candidate's own frame, so the
        # fixture's 6 candidates need up to 12 distinct frames) — these
        # critics gate their behavior on `ask is None`, which is only
        # true when the main pass is a single call.
        ONE_CALL_FRAMES = 20

        lazy = LazyCritic()
        rp, fp = run_critic(tdir, lazy, out_dir=os.path.join(tmp, "lazy"),
                           max_frames=ONE_CALL_FRAMES)
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
        rp2, fp2 = run_critic(tdir, ug, out_dir=os.path.join(tmp, "ungrounded"),
                             max_frames=ONE_CALL_FRAMES)
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
        rp3, fp3 = run_critic(tdir, mm, out_dir=os.path.join(tmp, "mismatch"),
                             max_frames=ONE_CALL_FRAMES)
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
        rp5, fp5 = run_critic(tdir, ql, out_dir=os.path.join(tmp, "quoteless"),
                             max_frames=ONE_CALL_FRAMES)
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
        rp6, fp6 = run_critic(tdir, fab, out_dir=os.path.join(tmp, "fabricated"),
                             max_frames=ONE_CALL_FRAMES)
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

        # ------------------------------------------------------------
        # #1873: one candidate, one published verdict. Two separately
        # VALID findings adjudicating the same friction candidate used
        # to publish both (one per report section) while their
        # duplicate coverage SUPPRESSED the repair pass — and the
        # report prints no candidate ids, so the maintainer could not
        # see the two sections described one observed moment.
        # ------------------------------------------------------------
        def _covering(data_, cid):
            return [f_ for f_ in data_["findings"]
                    if cid in f_.get("covers", [])]

        class DupCritic(FakeCritic):
            """Emits a SECOND finding for `cid`, cloned from the
            mechanical one so it passes the identical evidence
            discipline (same turns, same verbatim quote, same anchored
            oracle) — the duplicate is valid, which is the whole point.
            `flip` gives the clone the opposite verdict; `repeat` does
            it again on the repair call; `also` consolidates a SECOND
            candidate into the original finding instead of leaving it
            its own, so only one of that finding's candidates
            conflicts."""

            def __init__(self, cid, flip=True, repeat=False, also=None,
                         also_turn=None):
                self.asks = 0
                self.cid, self.flip, self.repeat = cid, flip, repeat
                self.also, self.also_turn = also, also_turn

            def adjudicate(self, digest, manual, frames, ask=None):
                self.asks += 1
                out = FakeCritic.adjudicate(self, digest, manual, frames, ask)
                if ask is not None and not self.repeat:
                    return out
                base = next((f_ for f_ in out["findings"]
                             if f_["evidence"]["candidate_ids"] == [self.cid]),
                            None)
                if base is None:
                    return out
                if self.also:
                    base["evidence"]["candidate_ids"].append(self.also)
                    base["evidence"]["turns"].append(self.also_turn)
                    out["findings"] = [f_ for f_ in out["findings"]
                                       if f_["evidence"]["candidate_ids"]
                                       != [self.also]]
                clone = json.loads(json.dumps(base))
                clone["title"] = base["title"] + " (second reading)"
                clone["evidence"]["candidate_ids"] = [self.cid]
                clone["evidence"]["turns"] = [
                    t for t in base["evidence"]["turns"]
                    if t != self.also_turn]
                if self.flip:
                    clone["verdict"] = ("intended" if base["verdict"] == "defect"
                                        else "defect")
                out["findings"].append(clone)
                return out

        conf = DupCritic("C1", flip=True)
        rpc, fpc = run_critic(tdir, conf,
                              out_dir=os.path.join(tmp, "conflict"),
                              max_frames=ONE_CALL_FRAMES)
        with open(fpc) as f:
            conf_data = json.load(f)
        conf_report = open(rpc).read()
        check("a conflicting duplicate adjudication TRIGGERS the repair "
              "pass (duplicate coverage used to suppress it)",
              conf.asks == 2, f"asks={conf.asks}")
        check("...every candidate still ends adjudicated after repair",
              not uncovered(conf_data, cands),
              str([c["cid"] for c in uncovered(conf_data, cands)]))
        check("...the conflicted candidate publishes exactly ONE verdict",
              len(_covering(conf_data, "C1")) == 1,
              json.dumps([(f_["title"], f_["verdict"])
                          for f_ in _covering(conf_data, "C1")]))
        check("...and neither losing finding reaches findings.json or "
              "gets a report block (only the warning names them)",
              not any(f_["title"].endswith("(second reading)")
                      for f_ in conf_data["findings"])
              and not any(line.startswith("### ") and "(second reading)" in line
                          for line in conf_report.splitlines()),
              json.dumps([f_["title"] for f_ in conf_data["findings"]]))
        check("...with the conflict named in the report — candidate and "
              "both verdicts — even though the repair succeeded",
              "CONFLICTINGLY" in conf_report
              and "candidate C1" in conf_report
              and "defect: " in conf_report and "intended: " in conf_report,
              conf_report[conf_report.find("## Critic warnings"):][:400])

        conf2 = DupCritic("C1", flip=True, repeat=True)
        rpc2, fpc2 = run_critic(tdir, conf2,
                                out_dir=os.path.join(tmp, "conflict2"),
                                max_frames=ONE_CALL_FRAMES)
        with open(fpc2) as f:
            conf2_data = json.load(f)
        conf2_report = open(rpc2).read()
        check("a conflict the repair response REPEATS gets no second "
              "repair pass", conf2.asks == 2, f"asks={conf2.asks}")
        check("...and the candidate ends honestly unadjudicated rather "
              "than doubly published",
              [c["cid"] for c in uncovered(conf2_data, cands)] == ["C1"]
              and not _covering(conf2_data, "C1"),
              json.dumps([(f_["title"], f_["verdict"], f_["covers"])
                          for f_ in conf2_data["findings"]]))
        check("...surfaced as BOTH an unadjudicated and a conflict warning",
              "unadjudicated candidates: C1" in conf2_report
              and "CONFLICTINGLY" in conf2_report)

        agree = DupCritic("C1", flip=False)
        rpa, fpa = run_critic(tdir, agree,
                              out_dir=os.path.join(tmp, "agree"),
                              max_frames=ONE_CALL_FRAMES)
        with open(fpa) as f:
            agree_data = json.load(f)
        agree_report = open(rpa).read()
        check("an AGREEING duplicate adjudication asks for NO repair",
              agree.asks == 1, f"asks={agree.asks}")
        check("...and keeps both findings published (one friction moment "
              "can expose two distinct issues)",
              len(_covering(agree_data, "C1")) == 2
              and {f_["verdict"] for f_ in _covering(agree_data, "C1")}
              == {"defect"},
              json.dumps([(f_["title"], f_["verdict"])
                          for f_ in _covering(agree_data, "C1")]))
        check("...still surfaced as a warning rather than applied silently",
              "AGREE" in agree_report and "candidate C1" in agree_report)

        c2_turn = next(c["turn"] for c in cands if c["cid"] == "C2")
        scoped = DupCritic("C1", flip=True, also="C2", also_turn=c2_turn)
        rpsc, fpsc = run_critic(tdir, scoped,
                                out_dir=os.path.join(tmp, "scoped"),
                                max_frames=ONE_CALL_FRAMES)
        with open(fpsc) as f:
            scoped_data = json.load(f)
        consolidated = [f_ for f_ in scoped_data["findings"]
                        if f_["evidence"]["candidate_ids"] == ["C1", "C2"]]
        check("a conflict is candidate-SCOPED: a finding consolidating "
              "C1+C2 keeps its uncontested C2 and stays published",
              len(consolidated) == 1
              and consolidated[0]["covers"] == ["C2"],
              json.dumps([(f_["title"], f_["evidence"]["candidate_ids"],
                           f_["covers"]) for f_ in scoped_data["findings"]]))
        check("...only the conflicted candidate is re-asked, and every "
              "candidate ends adjudicated",
              scoped.asks == 2 and not uncovered(scoped_data, cands),
              f"asks={scoped.asks} uncovered="
              + str([c["cid"] for c in uncovered(scoped_data, cands)]))
        check("...and C2 keeps exactly one published verdict",
              len(_covering(scoped_data, "C2")) == 1,
              json.dumps([(f_["title"], f_["verdict"])
                          for f_ in _covering(scoped_data, "C2")]))

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
