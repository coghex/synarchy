#!/usr/bin/env python3
"""Apply, verify and gate on the `reviewed:approve` staleness outcome (#2184).

`.github/workflows/review-gate.yml` decides on every synchronize push
whether a prior approval survives (`tools/review_gate_decision.py`, #1679).
That decision was fail-closed; everything downstream of it was not.

The two defects this module closes
----------------------------------
**The required check never saw the strip.** `review-approved` read
`github.event.pull_request.labels`, which is the IMMUTABLE event payload:
on a synchronize push to an approved pull request it still carries
`reviewed:approve`, so the required check went green before the strip had
even been attempted. The workflow's comment claimed the removal "fires an
`unlabeled` event, which re-runs review-approved" — that is not how
GitHub Actions behaves. **A label edit made with `GITHUB_TOKEN` creates no
workflow run at all**, by design, so the promised re-run never happened
and the required check stayed green on a head nobody had approved. Only
the PR drainer's own label check was enforcing staleness.

**The removal itself was fail-open.** `gh pr edit ... --remove-label
reviewed:approve || true` masked permission, API and transport failures
equally, so a strip decision whose mutation FAILED was indistinguishable
at the merge path from a keep decision.

The repair
----------
`--sync` runs inside `dismiss-stale-approval` after the decision. It
performs the removal when the decision is STRIP, then reads the label
state back from GitHub AUTHORITATIVELY — a fresh API read, not the event
payload — and reports:

  decision  keep | strip          (what the decision script concluded)
  label     true | false | unknown (what the authoritative read found)

`--gate` runs inside `review-approved` and turns that pair into the
required check's conclusion. On a synchronize event the check is green
only when the dismissal job SUCCEEDED, its decision was positively KEEP,
and the authoritative read found the label present. A strip, a failed or
unverifiable removal, a failed read, a missing or unrecognised decision,
or a dismissal job that did not succeed all leave it red for that head,
with no dependence on a later label event. On every other event —
`opened`, `reopened`, `labeled`, `unlabeled` — the payload's own label
list still decides, which is what makes re-approval recover the gate.

Fail-closed, and why the reason codes matter
--------------------------------------------
Every predicate here selects the RED/FAIL direction unless it can
positively prove the green one, and each returns its OWN reason code
rather than falling through to a neighbour's. Several codes share a
conclusion, so bypassing one rule would be invisible if only the
conclusion were asserted; the reason codes are what `--mutation-test`
checks against.

Job conclusion vs. gate conclusion
----------------------------------
They are deliberately different questions. `dismiss-stale-approval`'s own
conclusion answers "did the mutation this decision called for actually
take effect, and do we know it?" — so a correct STRIP whose label is
verifiably gone SUCCEEDS, while a label still present after the attempt,
or a read that could not be performed, FAILS with a message naming the
pull request and the specific error. That is what makes the drainer's
existing inference sound: a green `dismiss-stale-approval` run with
`reviewed:approve` still present can only have come from a KEEP.

`review-approved`'s conclusion answers "may this head merge?", which a
correct strip must answer NO to even though the job that stripped it
succeeded.

Usage:
  python3 tools/review_gate_label_policy.py --sync --decision keep|strip \
      --pr N --repo owner/name
  python3 tools/review_gate_label_policy.py --gate --event-action ACTION \
      --payload-label true|false --dismissal-result RESULT \
      --dismissal-decision DECISION --dismissal-label true|false|unknown
  python3 tools/review_gate_label_policy.py --self-test
  python3 tools/review_gate_label_policy.py --mutation-test

Exit codes: 0 = success / gate open, 1 = failure / gate closed.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import NamedTuple

#: The label the whole gate is about.
APPROVAL_LABEL = "reviewed:approve"

#: The one event action whose required check is decided by the dismissal
#: job rather than by the event payload's own label list.
SYNCHRONIZE = "synchronize"

KEEP = "keep"
STRIP = "strip"

#: `--sync`'s `label` output when the authoritative read could not be
#: performed. It is a third value on purpose: an unreadable state must not
#: be spellable as either `true` or `false`, both of which a gate rule is
#: entitled to act on.
UNKNOWN = "unknown"

#: The workflow this module is wired into. `--self-test` reads it, so the
#: wiring is checked by the same command as the policy.
DEFAULT_WORKFLOW = (
    Path(__file__).resolve().parent.parent / ".github" / "workflows" / "review-gate.yml"
)


class SyncOutcome(NamedTuple):
    """What the dismissal job did, and whether it may report success."""

    ok: bool
    decision: str
    label: str
    reason: str
    detail: str


class GateVerdict(NamedTuple):
    """Whether `review-approved` may conclude success for this head."""

    open: bool
    reason: str
    detail: str


class GitHubCli:
    """The single seam every `gh` invocation in this module goes through.

    `run` never raises: it answers (ok, stdout, error), so each caller
    decides what a failure means instead of an exception unwinding past a
    fail-closed branch. The self-test subclasses this to script each
    outcome — a removal that fails, a read that fails, a label that is
    still there afterwards — against the real policy code rather than a
    copy of it, because none of those can be provoked from a live GitHub.
    """

    def run(self, *args: str) -> tuple[bool, str, str]:
        try:
            completed = subprocess.run(
                ("gh", *args),
                capture_output=True,
                text=True,
                check=False,
            )
        except OSError as error:  # gh absent, PATH broken, ...
            return False, "", str(error)
        if completed.returncode != 0:
            message = (completed.stderr or completed.stdout or "").strip()
            return False, "", message or f"gh exited {completed.returncode}"
        return True, completed.stdout, ""


def remove_label(cli: GitHubCli, repo: str, pr: str, label: str) -> tuple[bool, str]:
    """Attempt the removal. Answers (ok, error text)."""
    ok, _, error = cli.run(
        "pr", "edit", pr, "--repo", repo, "--remove-label", label
    )
    return ok, error


def read_labels(cli: GitHubCli, repo: str, pr: str) -> tuple[bool, frozenset[str], str]:
    """Read the pull request's CURRENT labels from GitHub.

    This is the authoritative read the whole repair rests on: the event
    payload is a snapshot taken before this job ran, and only a fresh
    query can say what the label state is now. Answers
    (ok, label names, error text); a response that cannot be parsed is a
    read FAILURE, never an empty label set, because "unreadable" and
    "absent" must not collapse into one another.
    """
    ok, text, error = cli.run("pr", "view", pr, "--repo", repo, "--json", "labels")
    if not ok:
        return False, frozenset(), error
    try:
        document = json.loads(text)
    except json.JSONDecodeError as parse_error:
        return False, frozenset(), f"unparseable `gh pr view --json labels`: {parse_error}"
    if not isinstance(document, dict):
        return False, frozenset(), f"`gh pr view --json labels` is not an object: {text!r}"
    entries = document.get("labels")
    if not isinstance(entries, list):
        return False, frozenset(), f"`gh pr view --json labels` has no label list: {text!r}"
    names: set[str] = set()
    for entry in entries:
        if not isinstance(entry, dict) or not isinstance(entry.get("name"), str):
            return False, frozenset(), f"unrecognised label record: {entry!r}"
        names.add(entry["name"])
    return True, frozenset(names), ""


def sync_outcome(
    cli: GitHubCli,
    repo: str,
    pr: str,
    decision: str,
    label: str = APPROVAL_LABEL,
) -> SyncOutcome:
    """Apply the decision, verify it, and say whether the job may pass."""
    if decision not in (KEEP, STRIP):
        return SyncOutcome(
            False, decision, UNKNOWN, "decision-unrecognised",
            f"PR #{pr}: the staleness decision is {decision!r}, "
            f"which is neither {KEEP!r} nor {STRIP!r}")

    removal_attempted = decision == STRIP
    removal_ok = False
    removal_error = ""
    if removal_attempted:
        removal_ok, removal_error = remove_label(cli, repo, pr, label)

    read_ok, names, read_error = read_labels(cli, repo, pr)
    if not read_ok:
        # Fail-closed for BOTH decisions: an unverifiable label state is
        # exactly the state the drainer must not infer a keep from.
        return SyncOutcome(
            False, decision, UNKNOWN, "read-failed",
            f"PR #{pr}: could not read the label state back from GitHub "
            f"after a {decision} decision: {read_error}")

    present = label in names
    if decision == KEEP:
        if present:
            return SyncOutcome(
                True, KEEP, "true", "keep-present",
                f"PR #{pr}: this push left the reviewed patch identical and "
                f"{label} is still present")
        return SyncOutcome(
            True, KEEP, "false", "keep-absent",
            f"PR #{pr}: this push left the reviewed patch identical, but "
            f"{label} is not present, so the gate stays closed")

    if not present:
        if removal_ok:
            return SyncOutcome(
                True, STRIP, "false", "strip-removed",
                f"PR #{pr}: the reviewed patch changed; {label} was removed "
                f"and the read back confirms it is gone")
        return SyncOutcome(
            True, STRIP, "false", "strip-already-absent",
            f"PR #{pr}: the reviewed patch changed; the removal reported "
            f"{removal_error!r} but the read back confirms {label} is "
            f"already absent")

    if not removal_ok:
        return SyncOutcome(
            False, STRIP, "true", "strip-removal-failed",
            f"PR #{pr}: the reviewed patch changed, but removing {label} "
            f"failed and it is still present: {removal_error}")
    return SyncOutcome(
        False, STRIP, "true", "strip-still-present",
        f"PR #{pr}: the reviewed patch changed and removing {label} "
        f"reported success, yet the read back still finds it present — "
        f"unexpected state, refusing to report a clean strip")


def gate_verdict(
    event_action: str,
    payload_label: str,
    dismissal_result: str,
    dismissal_decision: str,
    dismissal_label: str,
) -> GateVerdict:
    """Whether the required `review-approved` check may conclude success.

    Off the synchronize path the event payload's own label list decides,
    unchanged: that is what `opened`/`reopened` gate on initially and what
    a reviewer's `labeled` event recovers the gate with.
    """
    if event_action != SYNCHRONIZE:
        if payload_label.strip().lower() == "true":
            return GateVerdict(
                True, "payload-label_present",
                f"{APPROVAL_LABEL} present on this {event_action or 'unknown'} "
                f"event — gate open")
        return GateVerdict(
            False, "payload-label-absent",
            f"no {APPROVAL_LABEL} on this {event_action or 'unknown'} event — "
            f"review verdict pending")

    if dismissal_result != "success":
        return GateVerdict(
            False, "dismissal-not-successful",
            f"dismiss-stale-approval concluded {dismissal_result!r}, not "
            f"'success' — the staleness of this head is unproven")
    if dismissal_decision != KEEP:
        return GateVerdict(
            False, "decision-not-keep",
            f"the staleness decision for this head was "
            f"{dismissal_decision or '(missing)'!r}, not {KEEP!r} — "
            f"re-approve the updated code")
    if dismissal_label.strip().lower() != "true":
        return GateVerdict(
            False, "label-not-verified-present",
            f"the authoritative label read reported "
            f"{dismissal_label or '(missing)'!r}, so {APPROVAL_LABEL} is not "
            f"proven present on this head")
    return GateVerdict(
        True, "keep-verified",
        f"this push left the reviewed patch identical and {APPROVAL_LABEL} "
        f"is confirmed present — gate open")


def _emit_outputs(pairs: dict[str, str]) -> None:
    """Publish step outputs, when running inside GitHub Actions."""
    destination = os.environ.get("GITHUB_OUTPUT", "")
    if not destination:
        return
    with open(destination, "a", encoding="utf-8") as handle:
        for key, value in pairs.items():
            handle.write(f"{key}={value}\n")


def _run_sync(args: argparse.Namespace) -> int:
    outcome = sync_outcome(GitHubCli(), args.repo, args.pr, args.decision)
    _emit_outputs({"decision": outcome.decision, "label_present": outcome.label})
    stream = sys.stdout if outcome.ok else sys.stderr
    print(f"[{outcome.reason}] {outcome.detail}", file=stream)
    return 0 if outcome.ok else 1


def _run_gate(args: argparse.Namespace) -> int:
    verdict = gate_verdict(
        args.event_action,
        args.payload_label,
        args.dismissal_result,
        args.dismissal_decision,
        args.dismissal_label,
    )
    stream = sys.stdout if verdict.open else sys.stderr
    print(f"[{verdict.reason}] {verdict.detail}", file=stream)
    return 0 if verdict.open else 1


# ---------------------------------------------------------------------------
# Self-test (issue #2184)
# ---------------------------------------------------------------------------


class ScriptedCli(GitHubCli):
    """A `gh` that answers exactly what a case needs it to.

    Every branch of the policy above turns on an outcome a live GitHub
    cannot be made to produce on demand — a removal that is refused, a
    read that fails, a label that survives its own successful removal — so
    the cases script those answers and run the REAL policy against them.
    """

    def __init__(
        self,
        *,
        removal_ok: bool = True,
        removal_error: str = "",
        read_ok: bool = True,
        read_error: str = "",
        labels_before: tuple[str, ...] = (APPROVAL_LABEL,),
        labels_after: tuple[str, ...] | None = None,
        read_payload: str | None = None,
    ) -> None:
        self.removal_ok = removal_ok
        self.removal_error = removal_error
        self.read_ok = read_ok
        self.read_error = read_error
        self.labels_before = labels_before
        self.labels_after = labels_after
        self.read_payload = read_payload
        self.removed = False
        self.calls: list[tuple[str, ...]] = []

    def run(self, *args: str) -> tuple[bool, str, str]:
        self.calls.append(args)
        if args[:2] == ("pr", "edit"):
            if self.removal_ok:
                self.removed = True
                return True, "", ""
            return False, "", self.removal_error
        if args[:2] == ("pr", "view"):
            if not self.read_ok:
                return False, "", self.read_error
            if self.read_payload is not None:
                return True, self.read_payload, ""
            names = self.labels_before
            if self.removed:
                names = (
                    self.labels_after
                    if self.labels_after is not None
                    else tuple(n for n in self.labels_before if n != APPROVAL_LABEL)
                )
            return True, json.dumps({"labels": [{"name": n} for n in names]}), ""
        raise AssertionError(f"unscripted gh invocation: {args}")


def _load_workflow(path: Path) -> dict:
    try:
        import yaml  # type: ignore
    except ImportError:  # pragma: no cover - exercised only on a bare toolchain
        raise SystemExit(
            "review_gate_label_policy.py --self-test needs PyYAML to read\n"
            f"{path}. Install the pinned toolchain:\n"
            "    python3 -m pip install --user -r tools/requirements-assets.txt")
    try:
        document = yaml.safe_load(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise SystemExit(f"cannot read {path}: {error}")
    except yaml.YAMLError as error:
        raise SystemExit(f"cannot parse {path}: {error}")
    if not isinstance(document, dict):
        raise SystemExit(f"{path} is not a YAML mapping")
    return document


def _condition(value: object) -> str:
    """Normalise a job `if:` to its bare expression."""
    text = str(value).strip()
    if text.startswith("${{") and text.endswith("}}"):
        text = text[3:-2]
    return " ".join(text.split())


def _steps_text(job: object) -> str:
    """Every `run:` body and `env:` value of a job, as one blob."""
    if not isinstance(job, dict):
        return ""
    pieces: list[str] = []
    for step in job.get("steps") or []:
        if not isinstance(step, dict):
            continue
        pieces.append(str(step.get("run", "")))
        env = step.get("env")
        if isinstance(env, dict):
            pieces.extend(f"{key}={value}" for key, value in env.items())
    return "\n".join(pieces)


def _wiring_failures(path: Path) -> list[str]:
    """Pin the workflow wiring the policy above is useless without.

    The policy is only reachable if `review-approved` waits for
    `dismiss-stale-approval`, survives that job being skipped on the four
    non-synchronize events, and is handed the dismissal's real result and
    outputs. None of that lives in this file, and none of it is observable
    from any other gate in this repository — the workflow runs on an event
    no CI job here ever sees — so it is pinned from here.
    """
    document = _load_workflow(path)
    failures: list[str] = []

    # PyYAML reads a bare `on:` key as the boolean True (YAML 1.1).
    triggers = document.get("on", document.get(True))
    types = []
    if isinstance(triggers, dict):
        pull_request = triggers.get("pull_request")
        if isinstance(pull_request, dict):
            types = list(pull_request.get("types") or [])
    for expected in ("opened", "reopened", "synchronize", "labeled", "unlabeled"):
        if expected not in types:
            failures.append(
                f"on.pull_request.types no longer includes {expected!r}: {types}")

    jobs = document.get("jobs")
    if not isinstance(jobs, dict):
        return failures + ["the workflow declares no jobs mapping"]

    gate = jobs.get("review-approved")
    dismissal = jobs.get("dismiss-stale-approval")
    if not isinstance(gate, dict):
        return failures + ["no `review-approved` job — that name IS the required check"]
    if not isinstance(dismissal, dict):
        return failures + ["no `dismiss-stale-approval` job"]

    needs = gate.get("needs")
    needs = [needs] if isinstance(needs, str) else list(needs or [])
    if "dismiss-stale-approval" not in needs:
        failures.append(
            "`review-approved` does not need `dismiss-stale-approval`, so it can "
            f"conclude before the staleness decision exists: needs={needs}")

    condition = _condition(gate.get("if", ""))
    if condition != "!cancelled()":
        failures.append(
            "`review-approved`'s `if:` must be exactly `!cancelled()`: any other "
            "condition either skips the required check when its dependency is "
            "skipped (the four non-synchronize events) or fails to run it when "
            f"the dependency fails — found {condition!r}")

    if _condition(dismissal.get("if", "")) != "github.event.action == 'synchronize'":
        failures.append(
            "`dismiss-stale-approval` no longer runs exactly on synchronize: "
            f"{_condition(dismissal.get('if', ''))!r}")

    outputs = dismissal.get("outputs")
    outputs = outputs if isinstance(outputs, dict) else {}
    for key in ("decision", "label_present"):
        if key not in outputs:
            failures.append(
                f"`dismiss-stale-approval` does not export the {key!r} output the "
                "gate reads")

    dismissal_text = _steps_text(dismissal)
    if "review_gate_decision.py" not in dismissal_text:
        failures.append("`dismiss-stale-approval` no longer runs review_gate_decision.py")
    if "review_gate_label_policy.py" not in dismissal_text or "--sync" not in dismissal_text:
        failures.append(
            "`dismiss-stale-approval` no longer runs review_gate_label_policy.py --sync")

    gate_text = _steps_text(gate)
    if "review_gate_label_policy.py" not in gate_text or "--gate" not in gate_text:
        failures.append(
            "`review-approved` no longer runs review_gate_label_policy.py --gate")
    for expression in (
        "needs.dismiss-stale-approval.result",
        "needs.dismiss-stale-approval.outputs.decision",
        "needs.dismiss-stale-approval.outputs.label_present",
        "github.event.action",
    ):
        if expression not in gate_text:
            failures.append(f"`review-approved` no longer reads {expression}")
    return failures


def _self_test(workflow: Path) -> int:  # noqa: C901 - a flat list of cases reads best flat
    failures: list[str] = []

    def check(label: str, actual: object, expected: object) -> None:
        if actual != expected:
            failures.append(f"{label}: got {actual!r}, want {expected!r}")

    def sync_case(
        label: str,
        *,
        decision: str,
        expect_ok: bool,
        expect_reason: str,
        expect_label: str,
        expect_in_detail: tuple[str, ...] = (),
        **cli_kwargs: object,
    ) -> None:
        cli = ScriptedCli(**cli_kwargs)  # type: ignore[arg-type]
        outcome = sync_outcome(cli, "coghex/synarchy", "1142", decision)
        check(f"{label}: ok", outcome.ok, expect_ok)
        check(f"{label}: reason", outcome.reason, expect_reason)
        check(f"{label}: label output", outcome.label, expect_label)
        check(f"{label}: decision output", outcome.decision, decision)
        for fragment in expect_in_detail:
            if fragment not in outcome.detail:
                failures.append(
                    f"{label}: detail does not mention {fragment!r}: {outcome.detail!r}")
        removal_expected = decision == STRIP
        attempted = any(call[:2] == ("pr", "edit") for call in cli.calls)
        check(f"{label}: removal attempted", attempted, removal_expected)
        read = any(call[:2] == ("pr", "view") for call in cli.calls)
        check(f"{label}: authoritative read performed", read, True)

    print("-- the six dismissal outcomes, plus the two the corrections split out")

    # 1. KEEP, label present: the #842/#1679 no-op branch update. Green.
    sync_case("keep/present", decision=KEEP, expect_ok=True,
              expect_reason="keep-present", expect_label="true",
              labels_before=(APPROVAL_LABEL, "bug"))

    # 2. KEEP, label absent: nothing was approved (or a human removed it).
    #    The job did its job, but the gate must not open.
    sync_case("keep/absent", decision=KEEP, expect_ok=True,
              expect_reason="keep-absent", expect_label="false",
              labels_before=("bug",))

    # 3. STRIP, removal succeeded, label verifiably gone.
    sync_case("strip/removed-now", decision=STRIP, expect_ok=True,
              expect_reason="strip-removed", expect_label="false",
              labels_before=(APPROVAL_LABEL, "bug"))

    # 4. STRIP, removal refused because the label was not there. The old
    #    `|| true` existed for exactly this case; the read back is what
    #    now distinguishes it from a removal that genuinely failed.
    sync_case("strip/already-absent", decision=STRIP, expect_ok=True,
              expect_reason="strip-already-absent", expect_label="false",
              removal_ok=False,
              removal_error="failed to update: 'reviewed:approve' not found",
              labels_before=("bug",),
              expect_in_detail=("#1142", "not found"))

    # 5. STRIP, removal errored, label still there: the fail-open defect.
    sync_case("strip/removal-error-still-present", decision=STRIP, expect_ok=False,
              expect_reason="strip-removal-failed", expect_label="true",
              removal_ok=False,
              removal_error="HTTP 403: Resource not accessible by integration",
              labels_before=(APPROVAL_LABEL,),
              expect_in_detail=("#1142", "HTTP 403"))

    # 6. STRIP, removal reported success, label still there anyway.
    sync_case("strip/unexpectedly-present", decision=STRIP, expect_ok=False,
              expect_reason="strip-still-present", expect_label="true",
              labels_before=(APPROVAL_LABEL,),
              labels_after=(APPROVAL_LABEL,),
              expect_in_detail=("#1142", "unexpected state"))

    # 7/8. The read itself fails: unverifiable, on either decision.
    for decision in (KEEP, STRIP):
        sync_case(f"read-failed/{decision}", decision=decision, expect_ok=False,
                  expect_reason="read-failed", expect_label=UNKNOWN,
                  read_ok=False, read_error="HTTP 502: Bad gateway",
                  expect_in_detail=("#1142", "HTTP 502"))

    print("-- an unreadable read is never an empty label set")
    for label, payload in (
        ("unparseable json", "{not json"),
        ("not an object", '["reviewed:approve"]'),
        ("no label list", '{"labels": "reviewed:approve"}'),
        ("unrecognised record", '{"labels": [{"nome": "reviewed:approve"}]}'),
    ):
        cli = ScriptedCli(read_payload=payload)
        outcome = sync_outcome(cli, "coghex/synarchy", "1142", KEEP)
        check(f"{label}: reason", outcome.reason, "read-failed")
        check(f"{label}: ok", outcome.ok, False)
        check(f"{label}: label output", outcome.label, UNKNOWN)

    print("-- an unrecognised decision never mutates and never passes")
    cli = ScriptedCli()
    outcome = sync_outcome(cli, "coghex/synarchy", "1142", "maybe")
    check("decision/unrecognised: reason", outcome.reason, "decision-unrecognised")
    check("decision/unrecognised: ok", outcome.ok, False)
    check("decision/unrecognised: label output", outcome.label, UNKNOWN)
    check("decision/unrecognised: touched GitHub", cli.calls, [])

    print("-- the required check's conclusion, per dismissal outcome")

    def gate_case(
        label: str,
        *,
        expect_open: bool,
        expect_reason: str,
        action: str = SYNCHRONIZE,
        payload: str = "true",
        result: str = "success",
        decision: str = KEEP,
        verified: str = "true",
    ) -> None:
        verdict = gate_verdict(action, payload, result, decision, verified)
        check(f"{label}: open", verdict.open, expect_open)
        check(f"{label}: reason", verdict.reason, expect_reason)

    # The only green synchronize path there is.
    gate_case("gate synchronize keep/verified", expect_open=True,
              expect_reason="keep-verified")

    # The defect this issue is about: the payload still says approved.
    gate_case("gate synchronize strip", expect_open=False,
              expect_reason="decision-not-keep", decision=STRIP)
    gate_case("gate synchronize missing decision", expect_open=False,
              expect_reason="decision-not-keep", decision="")
    gate_case("gate synchronize garbage decision", expect_open=False,
              expect_reason="decision-not-keep", decision="Keep")
    gate_case("gate synchronize keep but label absent", expect_open=False,
              expect_reason="label-not-verified-present", verified="false")
    gate_case("gate synchronize keep but label unknown", expect_open=False,
              expect_reason="label-not-verified-present", verified=UNKNOWN)
    gate_case("gate synchronize keep but no label output", expect_open=False,
              expect_reason="label-not-verified-present", verified="")
    for result in ("failure", "cancelled", "skipped", ""):
        gate_case(f"gate synchronize dismissal {result or '(missing)'}",
                  expect_open=False, expect_reason="dismissal-not-successful",
                  result=result)

    # A green synchronize verdict must not be reachable from the payload.
    gate_case("gate synchronize payload cannot open a stripped head",
              expect_open=False, expect_reason="decision-not-keep",
              payload="true", decision=STRIP)

    # The four other events keep their existing, payload-driven behaviour,
    # which is how a reviewer's re-approval reopens the gate.
    for action in ("opened", "reopened", "labeled", "unlabeled"):
        gate_case(f"gate {action} labelled", expect_open=True,
                  expect_reason="payload-label_present", action=action,
                  payload="true", result="skipped", decision="", verified=UNKNOWN)
        gate_case(f"gate {action} unlabelled", expect_open=False,
                  expect_reason="payload-label-absent", action=action,
                  payload="false", result="skipped", decision="", verified=UNKNOWN)
    gate_case("gate labeled with a garbage payload flag", expect_open=False,
              expect_reason="payload-label-absent", action="labeled",
              payload="yes", result="skipped", decision="", verified=UNKNOWN)

    print("-- the dismissal outcome feeds the gate end to end")
    for label, decision, cli_kwargs, expect_open in (
        ("no-op branch update", KEEP, {"labels_before": (APPROVAL_LABEL,)}, True),
        ("content change", STRIP, {"labels_before": (APPROVAL_LABEL,)}, False),
        ("content change, removal refused", STRIP,
         {"removal_ok": False, "removal_error": "boom", "labels_before": ("bug",)}, False),
    ):
        outcome = sync_outcome(
            ScriptedCli(**cli_kwargs), "coghex/synarchy", "1142", decision)  # type: ignore[arg-type]
        result = "success" if outcome.ok else "failure"
        verdict = gate_verdict(
            SYNCHRONIZE, "true", result, outcome.decision, outcome.label)
        check(f"end to end/{label}", verdict.open, expect_open)

    print("-- the workflow wiring the policy is useless without")
    for failure in _wiring_failures(workflow):
        failures.append(f"workflow wiring: {failure}")

    if failures:
        for failure in failures:
            print(f"  FAIL {failure}")
        print(f"\n{len(failures)} review_gate_label_policy self-test check(s) failed")
        return 1
    print("review_gate_label_policy self-test: all checks pass")
    return 0


# ---------------------------------------------------------------------------
# Mutation test (issue #2184)
# ---------------------------------------------------------------------------
#
# A self-test proves the policy agrees with the cases beside it; it does
# not prove the cases would NOTICE the policy changing. This copies the
# file, defeats one rule in the copy, and requires `--self-test` on the
# copy to fail. Every fail-closed branch is covered, including the two
# defects #2184 is about: reading the gate from the event payload, and
# treating the removal as best effort.
#
# It runs by hand as this issue's acceptance evidence and is not part of
# the gate set: it re-runs the whole self-test once per mutation, and a
# mutation whose anchor stops matching is an error naming the anchor,
# never a quietly smaller run.


def _replace_once(source: str, old: str, new: str) -> str:
    if source.count(old) != 1:
        raise SystemExit(
            f"mutation anchor matched {source.count(old)} times (want 1): {old!r}")
    return source.replace(old, new, 1)


#: (label, mutation). Each must make `--self-test` fail.
_MUTATIONS: tuple[tuple[str, object], ...] = (
    ("trust the removal's exit code instead of the read back",
     lambda t: _replace_once(t, "    present = label in names",
                             "    present = label in names and not removal_attempted")),
    ("treat an unreadable label state as absent",
     lambda t: _replace_once(t, "    read_ok, names, read_error = read_labels(cli, repo, pr)",
                             "    read_ok, names, read_error = read_labels(cli, repo, pr)\n"
                             "    read_ok = True")),
    ("let an unparseable read answer an empty label set",
     lambda t: _replace_once(
         t, '        return False, frozenset(), f"unparseable `gh pr view --json labels`: {parse_error}"',
         '        return True, frozenset(), ""')),
    ("pass a strip whose label survived",
     lambda t: _replace_once(
         t, '            False, STRIP, "true", "strip-removal-failed",',
         '            True, STRIP, "true", "strip-removal-failed",')),
    ("pass a strip whose successful removal did not take",
     lambda t: _replace_once(
         t, '        False, STRIP, "true", "strip-still-present",',
         '        True, STRIP, "true", "strip-still-present",')),
    ("accept an unrecognised decision",
     lambda t: _replace_once(t, "    if decision not in (KEEP, STRIP):",
                             "    if False:")),
    ("skip the removal on a strip decision",
     lambda t: _replace_once(t, "    removal_attempted = decision == STRIP",
                             "    removal_attempted = False")),
    ("decide the synchronize gate from the event payload (the #2184 defect)",
     lambda t: _replace_once(t, "    if event_action != SYNCHRONIZE:",
                             "    if True:")),
    ("open the gate when the dismissal job did not succeed",
     lambda t: _replace_once(t, '    if dismissal_result != "success":',
                             "    if False:")),
    ("open the gate on a strip decision",
     lambda t: _replace_once(t, "    if dismissal_decision != KEEP:",
                             "    if False:")),
    ("open the gate without a verified label",
     lambda t: _replace_once(t, '    if dismissal_label.strip().lower() != "true":',
                             "    if False:")),
    ("close the gate on a labelled non-synchronize event",
     lambda t: _replace_once(t, '        if payload_label.strip().lower() == "true":',
                             "        if False:")),
    ("report the strip outcome without naming the pull request",
     lambda t: _replace_once(t, '            f"PR #{pr}: the reviewed patch changed, but removing {label} "',
                             '            f"the reviewed patch changed, but removing {label} "')),
    ("swallow the captured removal error",
     lambda t: _replace_once(t, '            f"failed and it is still present: {removal_error}")',
                             '            f"failed and it is still present")')),
    ("swallow the read error",
     lambda t: _replace_once(t, '            f"after a {decision} decision: {read_error}")',
                             '            f"after a {decision} decision")')),
)


#: Everything from this line down is the harness, not the policy. It is
#: split off before a mutation is applied and reattached afterwards, so an
#: anchor can never match the table that names it.
_HARNESS_MARKER = "# Mutation test (issue #2184)"


def _mutation_test(workflow: Path) -> int:
    whole = Path(__file__).resolve().read_text(encoding="utf-8")
    head, marker, tail = whole.partition(_HARNESS_MARKER)
    if not marker:
        raise SystemExit(f"harness marker not found: {_HARNESS_MARKER}")
    survivors: list[str] = []
    with tempfile.TemporaryDirectory(prefix="review-gate-policy-mutation-") as tmp:
        copy = Path(tmp) / "review_gate_label_policy.py"

        def run_self_test() -> int:
            return subprocess.run(
                (sys.executable, str(copy), "--self-test", "--workflow", str(workflow)),
                capture_output=True, text=True, check=False).returncode

        # The control: an unmutated copy must PASS, or every "mutation
        # killed" below would be meaningless.
        copy.write_text(whole, encoding="utf-8")
        if run_self_test() != 0:
            print("  FAIL: the unmutated copy does not pass its own self-test")
            return 1
        for label, mutate in _MUTATIONS:
            copy.write_text(mutate(head) + marker + tail, encoding="utf-8")  # type: ignore[operator]
            if run_self_test() == 0:
                survivors.append(label)
                print(f"  SURVIVED: {label}")
            else:
                print(f"  killed:   {label}")
    if survivors:
        print(f"\n{len(survivors)} mutation(s) survived the self-test:")
        for label in survivors:
            print(f"  - {label}")
        return 1
    print(f"\nreview_gate_label_policy mutation test: all {len(_MUTATIONS)} "
          "mutations killed")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sync", action="store_true",
                        help="apply and verify the staleness decision")
    parser.add_argument("--gate", action="store_true",
                        help="conclude the required review-approved check")
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--mutation-test", action="store_true",
                        help="defeat one policy rule at a time in a copy of this "
                             "file and require --self-test to fail (manual)")
    parser.add_argument("--workflow", default=str(DEFAULT_WORKFLOW),
                        help="the review-gate workflow the self-test pins")
    parser.add_argument("--decision", default="", help="--sync: keep or strip")
    parser.add_argument("--pr", default="", help="--sync: the pull-request number")
    parser.add_argument("--repo", default="", help="--sync: owner/name")
    parser.add_argument("--event-action", default="")
    parser.add_argument("--payload-label", default="")
    parser.add_argument("--dismissal-result", default="")
    parser.add_argument("--dismissal-decision", default="")
    parser.add_argument("--dismissal-label", default="")
    args = parser.parse_args(argv)

    if args.self_test:
        return _self_test(Path(args.workflow))
    if args.mutation_test:
        return _mutation_test(Path(args.workflow).resolve())
    if args.sync:
        if not args.pr or not args.repo:
            parser.error("--sync needs --pr and --repo")
        return _run_sync(args)
    if args.gate:
        return _run_gate(args)
    parser.error("one of --sync, --gate, --self-test or --mutation-test is required")
    return 2  # unreachable; parser.error exits


if __name__ == "__main__":
    sys.exit(main())
