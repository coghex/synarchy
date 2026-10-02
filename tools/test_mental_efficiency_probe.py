#!/usr/bin/env python3
"""Unit tests for the mental-efficiency probe's combat sampling (#2772).

`tools/mental_efficiency_probe.py`'s combat sample once placed an unarmed
attacker two tiles from its target, outside its 0.75-tile live reach.
Every strike was refused at commit as `out_of_reach` (#2328), the sampler
discarded the refusals, and the two combat checks failed as a bare
`lo=0 hi=0` and an infinite fallback ratio. A passing probe run cannot
show the failure paths that would expose that again, so they are pinned
here:

  * The sample placement is inside the pinned attacker's unarmed reach.
  * Refusal-only sampling records each refusal's reason, and the
    `combat_samples` failure detail names it (`refused:out_of_reach`)
    along with the attempts that stood out of live reach.
  * `damage_energy_unchanged` fails with an explicit "no samples" detail
    and no numeric ratio whenever either side, or both, is empty; with
    samples on both sides it computes the ratio against the unchanged
    `0.95 < ratio < 1.0526` tolerance.
  * Both combat details reach standalone human output and the structured
    `probe-result/v1` stream.

No engine: the sampler runs against a fake console in well under a
second.

Usage:
  python3 tools/test_mental_efficiency_probe.py [-v]
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import io
import json
import tempfile
from pathlib import Path

import selftestlib
from selftestlib import FAILURES, expect

import probe_protocol
import mental_efficiency_probe as probe


class FakeConsole:
    """Answers the sampler's console calls; every strike gets `events`."""

    def __init__(self, events, separation=2.0, reach=0.75):
        self.events = events
        self.separation = separation
        self.reach = reach
        self.next_id = 100
        self.attacks = 0

    def send(self, port, code, *args, **kwargs):
        if "unit.spawn" in code:
            self.next_id += 1
            return str(self.next_id)
        if "combat.attack" in code:
            self.attacks += 1
        return "ok"

    def send_json(self, port, code, *args, **kwargs):
        if "getAttackRange" in code:
            return {"range": self.reach, "sep": self.separation}
        if "drainEvents" in code:
            return list(self.events)
        return None

    @staticmethod
    def poll_until(seconds, fn, interval=0.3):
        return fn()


def with_console(console, body):
    saved = (probe.send, probe.send_json, probe.poll_until)
    probe.send, probe.send_json, probe.poll_until = (
        console.send, console.send_json, console.poll_until)
    try:
        return body()
    finally:
        probe.send, probe.send_json, probe.poll_until = saved


REFUSED = [{"kind": "refused", "payload": {"mode": "quick", "reason": "out_of_reach"}}]


def hit(raw):
    return [{"kind": "hit", "payload": {"raw": raw}}]


def test_placement_is_inside_unarmed_reach() -> None:
    (ax, ay), (tx, ty) = probe.COMBAT_ATTACKER_POS, probe.COMBAT_TARGET_POS
    separation = max(abs(ax - tx), abs(ay - ty))
    reach = 1.8 / 2.4  # pinned height 1.8 m, no blade (Admission.attackRangeTiles)
    expect(separation <= reach,
           f"combat sample separation {separation} is within unarmed reach {reach}")


def test_refusal_only_sampling_reports_the_reason() -> None:
    console = FakeConsole(REFUSED, separation=2.0)
    sample = with_console(console, lambda: probe.combat_damage_sample(
        9353, 0.0, False, want=6, retries=20))
    expect(sample.vals == [], "refused strikes contribute no samples")
    expect(console.attacks == 20, "every retry still requests one strike")
    expect(sample.outcomes == {"refused:out_of_reach": 20},
           f"each refusal is counted by its reason: {dict(sample.outcomes)}")
    expect(len(sample.reach_problems) == 20,
           "every attempt beyond live reach is recorded")
    ok, detail = probe.combat_samples_verdict(sample, sample)
    expect(not ok, "no landed hits fails combat_samples")
    expect("lo=0 hi=0" in detail, f"the counts stay in the detail: {detail}")
    expect("refused:out_of_reach" in detail,
           f"the refusal reason survives into the failure detail: {detail}")
    expect("separation 2.00 > live reach 0.75" in detail,
           f"the fixture's out-of-reach geometry is named: {detail}")


def test_landed_hits_are_sampled() -> None:
    console = FakeConsole(hit(79.4), separation=0.5)
    sample = with_console(console, lambda: probe.combat_damage_sample(
        9353, 1.0, True, want=6, retries=20))
    expect(sample.vals == [79.4] * 6, "six landed hits end the sampling early")
    expect(sample.outcomes == {"hit": 6} and sample.reach_problems == [],
           f"no refusal or reach problem is invented: {sample.summary()}")
    ok, detail = probe.combat_samples_verdict(sample, sample)
    expect(ok and "lo=6 hi=6" in detail, f"enough samples pass: {detail}")


def test_strike_outcomes() -> None:
    expect(probe.strike_outcome(hit(5.0)) == (5.0, "hit"), "a hit yields its raw energy")
    expect(probe.strike_outcome([{"kind": "miss", "payload": {}}]) == (None, "miss"),
           "a miss is not a sample")
    expect(probe.strike_outcome([]) == (None, "no-event"), "silence is reported")
    expect(probe.strike_outcome(None) == (None, "no-event"), "a timed-out poll is reported")
    expect(probe.strike_outcome([{"kind": "refused", "payload": {}}]) == (None, "refused:unknown"),
           "a refusal without a reason still reads as a refusal")


def test_damage_energy_without_samples() -> None:
    for lo, hi, name in (([], [79.4] * 6, "empty low"),
                         ([79.4] * 6, [], "empty high"),
                         ([], [], "both empty")):
        ok, detail = probe.damage_energy_verdict(lo, hi)
        expect(not ok, f"{name}: damage_energy_unchanged fails")
        expect(detail.startswith("no samples"), f"{name}: explicit no-samples detail: {detail}")
        expect("ratio=" not in detail and "inf" not in detail,
               f"{name}: no numeric ratio is reported: {detail}")


def test_damage_energy_with_samples() -> None:
    ok, detail = probe.damage_energy_verdict([80.0] * 6, [80.0] * 6)
    expect(ok and "ratio=1.000" in detail, f"equal means pass with a ratio: {detail}")
    ok, detail = probe.damage_energy_verdict([80.0] * 6, [84.5] * 6)
    expect(not ok and "ratio=1.056" in detail,
           f"a shift past the unchanged tolerance fails: {detail}")


def test_details_reach_both_outputs() -> None:
    sample = probe.CombatSample(outcomes=probe.Counter({"refused:out_of_reach": 20}))
    _, detail = probe.combat_samples_verdict(sample, sample)
    label = "enough landed hits sampled at both effectiveness levels"
    saved = probe._REPORTER

    stream = io.StringIO()
    probe._REPORTER = probe_protocol.Reporter(probe.DESCRIPTOR, stream=stream)
    try:
        probe.check(True, False, label, detail, show=True)
    finally:
        probe._REPORTER.close()
        probe._REPORTER = saved
    text = stream.getvalue()
    expect(f"[FAIL] {label}" in text, "the check keeps its standalone label")
    expect("refused:out_of_reach" in text, f"standalone output shows the reason: {text!r}")

    with tempfile.TemporaryDirectory(prefix="mental-efficiency-") as tmp:
        events_path = Path(tmp) / "events.jsonl"
        probe._REPORTER = probe_protocol.Reporter(
            probe.DESCRIPTOR, events_path=str(events_path), stream=io.StringIO())
        try:
            probe.check(True, False, label, detail, show=True)
        finally:
            probe._REPORTER.close()
            probe._REPORTER = saved
        events = [json.loads(line) for line in events_path.read_text().splitlines()]
    checks = [e for e in events if e.get("event") == probe_protocol.EVENT_CHECK]
    infos = [e for e in events if e.get("event") == probe_protocol.EVENT_DIAGNOSTIC]
    expect(len(checks) == 1 and checks[0].get("id") == "combat_samples",
           f"the check keeps its stable id: {checks}")
    expect(checks and "refused:out_of_reach" in checks[0].get("detail", {}).get("detail", ""),
           f"the structured check carries the reason: {checks}")
    expect(infos and "refused:out_of_reach" in infos[0].get("message", ""),
           f"the structured stream carries the INFO line: {infos}")


def main() -> int:
    selftestlib.parse_verbose()
    test_placement_is_inside_unarmed_reach()
    test_refusal_only_sampling_reports_the_reason()
    test_landed_hits_are_sampled()
    test_strike_outcomes()
    test_damage_energy_without_samples()
    test_damage_energy_with_samples()
    test_details_reach_both_outputs()
    if FAILURES:
        print(f"{len(FAILURES)} failure(s)")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "mental_efficiency combat sampling: all cases pass")


if __name__ == "__main__":
    raise SystemExit(main())
