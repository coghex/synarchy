#!/usr/bin/env python3
"""Workflow parsing, gate-set comparison and job topology (issue #1355).

The middle layer of the CI/`make ci` parity audit
(`tools/ci_parity_audit.py`). It consumes `tools/ci_parity_shell.py`'s
lexer and `tools/ci_parity_config.py`'s production constants, and knows
nothing about save-compat policy or the self-test.

What it owns
------------
Declared-shell validation, YAML job and step lookup, extraction of a
job's executable `run:` bodies, the per-job and local-side invocation
collections, the two-way gate-set comparison over the audited jobs'
union with its cross-job duplicate rejection and stale-exemption
detection, aggregate-job dependency validation, the static-audit job's
topology, the behavior-probe job's condition and required commands, the
unit-asset gate's pinned selection, and the expression normalization
those checks need.

Two audited jobs, one union (#2272)
-----------------------------------
`make ci` mirrors the gate sets of BOTH `test-and-audits` and
`static-audits`. Each job's set is collected separately and their
intersection is rejected before the union is taken: a command run by two
CI jobs is work CI pays for twice, and a union taken first would hide it
behind a set that still matches `tools/ci-local.sh` exactly. Each job is
also required to yield at least one invocation, so a job emptied by an
edit fails here rather than shrinking the comparison.

What it deliberately does NOT do
--------------------------------
Compare `if:` expressions as part of the gate-set audit. That comparison
is over the gate SET, not over the conditions guarding it, because CI is
path-selective where `make ci` is unconditional by design. The one check
that IS conditional on both sides -- the save-compat reproducibility
member -- gets its own owner, `tools/ci_parity_save_compat.py`, which
consumes the primitives here.

This module is a library: it has no command line of its own. Run the
audit through `python3 tools/ci_parity_audit.py`.
"""
from __future__ import annotations

import os
import sys

try:
    import yaml  # type: ignore
except ImportError:  # pragma: no cover - exercised only on a bare toolchain
    raise SystemExit(
        "ci_parity_audit.py needs PyYAML to read .github/workflows/ci.yml.\n"
        "Install the pinned toolchain:\n"
        "    python3 -m pip install --user -r tools/requirements-assets.txt\n"
        "(PyYAML is already required by tools/pack_atlas.py, which `make ci` "
        "and CI both run, so this adds no new dependency.)")

# `tools/` has no `__init__.py`, so it is an implicit namespace package and
# `import tools.ci_parity_workflow` from the repository root leaves `tools/`
# itself OFF sys.path. Every sibling import below runs at IMPORT time, so
# this has to come before the first one or that spelling raises
# ModuleNotFoundError while `python3 tools/ci_parity_audit.py` keeps working.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ci_parity_config import (
    AGGREGATE_JOB,
    AGGREGATE_NEEDS,
    AUDITED_JOB,
    AUDITED_JOBS,
    EXEMPT_COMMANDS,
    IMAGE_JOB,
    LOCAL_GATE_LABEL,
    PROBE_JOB,
    PROBE_JOB_IF,
    PROBE_REQUIRED_COMMANDS,
    STATIC_AUDIT_JOB,
    UNIT_ASSET_CI_IF,
    UNIT_ASSET_COMMAND,
    UNIT_ASSET_GATE,
    UNIT_ASSET_SELECTOR_COMMAND,
    WORKFLOW_LABEL,
    WORKFLOW_PATH,
    WORKFLOW_UNION_LABEL,
    workflow_label,
)
from ci_parity_shell import AuditError, extract_invocations


#: Shells whose `run:` bodies ci_parity_shell.py's lexer actually
#: understands.
#: GitHub also accepts `python`, `pwsh` and friends, where the body is not
#: shell at all — parsing one of those as shell would invent or lose
#: invocations, so it is refused rather than guessed at.
_SUPPORTED_SHELLS = frozenset({"bash", "sh"})


def _check_shell(declared: object, where: str, what: str) -> None:
    if declared is None:
        return
    if not isinstance(declared, str) or declared not in _SUPPORTED_SHELLS:
        raise AuditError(
            f"{where}: {what} selects shell {declared!r}. This audit reads "
            "`run:` bodies as shell; it will not parse another language as "
            f"one. Supported: {sorted(_SUPPORTED_SHELLS)}.")


def _declared_shell(container: object) -> object:
    """`defaults.run.shell` of a workflow or job mapping, if any."""
    if not isinstance(container, dict):
        return None
    defaults = container.get("defaults")
    if not isinstance(defaults, dict):
        return None
    run = defaults.get("run")
    if not isinstance(run, dict):
        return None
    return run.get("shell")


def workflow_run_blocks(yaml_text: str, job: str, where: str) -> list[str]:
    """The executable `run:` bodies of one workflow job, in order."""
    try:
        document = yaml.safe_load(yaml_text)
    except yaml.YAMLError as error:
        raise AuditError(f"{where}: could not parse as YAML ({error}).") from error
    if not isinstance(document, dict):
        raise AuditError(f"{where}: top level is not a YAML mapping.")
    jobs = document.get("jobs")
    if not isinstance(jobs, dict):
        raise AuditError(f"{where}: no `jobs:` mapping.")
    if job not in jobs:
        raise AuditError(
            f"{where}: no `{job}` job. This audit mirrors the gate sets of "
            f"exactly {list(AUDITED_JOBS)}; if one was renamed, update "
            "AUDITED_JOBS in tools/ci_parity_config.py deliberately.")
    definition = jobs[job]
    if not isinstance(definition, dict):
        raise AuditError(f"{where}: the `{job}` job is not a mapping.")
    _check_shell(_declared_shell(document), where, "the workflow default")
    _check_shell(_declared_shell(definition), where, f"the `{job}` job default")
    steps = definition.get("steps")
    if not isinstance(steps, list) or not steps:
        raise AuditError(f"{where}: the `{job}` job declares no steps.")
    blocks: list[str] = []
    for index, step in enumerate(steps):
        if not isinstance(step, dict):
            raise AuditError(f"{where}: step {index} is not a mapping.")
        command = step.get("run")
        if command is None:
            continue
        if not isinstance(command, str):
            raise AuditError(
                f"{where}: step {index}'s `run:` is not a string.")
        _check_shell(step.get("shell"), where, f"step {index}")
        blocks.append(command)
    return blocks


def workflow_invocations(yaml_text: str, job: str = AUDITED_JOB,
                         where: str = WORKFLOW_LABEL) -> set[str]:
    found: set[str] = set()
    for block in workflow_run_blocks(yaml_text, job, where):
        found.update(extract_invocations(block, where))
    return found


def local_gate_invocations(shell_text: str,
                           where: str = LOCAL_GATE_LABEL) -> set[str]:
    return set(extract_invocations(shell_text, where))


def workflow_job_invocations(
        yaml_text: str,
        jobs: tuple[str, ...] = AUDITED_JOBS) -> dict[str, set[str]]:
    """Each audited job's own invocation set, kept SEPARATE.

    Separate rather than pre-merged because the audit has to reject a
    command run by two audited jobs before it may take their union
    (#2272): the union of a duplicated set is indistinguishable from the
    union of a correctly split one, so merging first would make that
    check impossible to write.
    """
    return {job: workflow_invocations(yaml_text, job, workflow_label(job))
            for job in jobs}


def audit_gate_sets(
    ci_job_commands: dict[str, set[str]],
    local_commands: set[str],
    exempt_commands: tuple[tuple[str, str], ...] = EXEMPT_COMMANDS,
) -> list[str]:
    """Compare the audited jobs' union with the local gate set.

    `ci_job_commands` maps each audited job to its OWN invocation set.
    Returns a list of human-readable problems.
    """
    problems: list[str] = []

    for command, reason in exempt_commands:
        if not reason.strip():
            problems.append(
                f"exemption {command!r} carries no stated reason.")
    if not ci_job_commands:
        problems.append(
            "no audited workflow job was collected at all; an empty job set "
            "cannot certify parity.")
    for job, commands in ci_job_commands.items():
        if not commands:
            problems.append(
                f"{workflow_label(job)} yielded no `python3 tools/*.py` "
                "invocations; an empty gate set cannot certify parity.")
    if not local_commands:
        problems.append(
            f"{LOCAL_GATE_LABEL} yielded no `python3 tools/*.py` invocations; "
            "an empty gate set cannot certify parity.")

    # Duplicates BEFORE the union. A gate run by two audited jobs is work
    # CI pays for twice, and every check below would still pass: the union
    # is the same set either way.
    audited = list(ci_job_commands)
    for index, left in enumerate(audited):
        for right in audited[index + 1:]:
            shared_by_both = ci_job_commands[left] & ci_job_commands[right]
            for command in sorted(shared_by_both):
                problems.append(
                    f"run by BOTH {workflow_label(left)} and "
                    f"{workflow_label(right)}: {command}. Each audited job "
                    "runs a gate at most once; pick the job that owns it.")

    ci_commands: set[str] = set()
    for commands in ci_job_commands.values():
        ci_commands |= commands

    exempt_command_set = {command for command, _ in exempt_commands}
    def exempt(command: str) -> bool:
        return command in exempt_command_set

    ci_only = sorted(c for c in ci_commands - local_commands if not exempt(c))
    local_only = sorted(c for c in local_commands - ci_commands if not exempt(c))

    for command in ci_only:
        owners = ", ".join(job for job in audited
                           if command in ci_job_commands[job])
        problems.append(
            f"run by .github/workflows/ci.yml (job: {owners}) but not by "
            f"{LOCAL_GATE_LABEL}: {command}")
    for command in local_only:
        problems.append(
            f"run by {LOCAL_GATE_LABEL} but not by {WORKFLOW_UNION_LABEL}: "
            f"{command}")

    everything = ci_commands | local_commands
    for command, _reason in exempt_commands:
        if command not in everything:
            problems.append(
                f"stale exemption: no side runs `{command}`. Remove the entry "
                "rather than leaving policy that guards nothing.")
    return problems


def normalise_expression(text: str) -> str:
    """Collapse an expression's whitespace for comparison.

    Public because tools/ci_parity_save_compat.py compares
    `if:` expressions with the same normalization these
    topology checks use; two spellings of it would be two
    different notions of "the same guard".
    """
    return " ".join(text.split())


def workflow_steps(yaml_text: str, job: str = AUDITED_JOB,
                   where: str = WORKFLOW_LABEL) -> list[dict]:
    """The audited worker's steps as mappings, `if:` included.

    workflow_run_blocks above deliberately returns only `run:` text,
    because the gate-set comparison is about commands. This wiring check
    needs the conditions too.
    """
    try:
        document = yaml.safe_load(yaml_text)
    except yaml.YAMLError as error:
        raise AuditError(
            f"{where}: could not parse as YAML ({error}).") from error
    if not isinstance(document, dict):
        raise AuditError(f"{where}: top level is not a YAML mapping.")
    jobs = document.get("jobs")
    if not isinstance(jobs, dict) or job not in jobs:
        raise AuditError(f"{where}: no `{job}` job.")
    definition = jobs[job]
    if not isinstance(definition, dict):
        raise AuditError(f"{where}: the `{job}` job is not a mapping.")
    steps = definition.get("steps")
    if not isinstance(steps, list) or not steps:
        raise AuditError(f"{where}: the `{job}` job declares no steps.")
    for index, step in enumerate(steps):
        if not isinstance(step, dict):
            raise AuditError(f"{where}: step {index} is not a mapping.")
    return steps


def audit_parallel_gate_wiring(yaml_text: str) -> list[str]:
    """Pin the stable aggregate and both parallel workers.

    The PR drainer merges with ``--admin`` and deliberately watches one exact
    CI context, ``build-test``. Branch protection alone therefore cannot make
    a newly split worker blocking: the stable context itself must depend on
    both workers and fail when either PR result is not successful.
    """
    try:
        document = yaml.safe_load(yaml_text)
    except yaml.YAMLError as error:
        raise AuditError(
            f"{WORKFLOW_PATH}: could not parse as YAML ({error}).") from error
    jobs = document.get("jobs") if isinstance(document, dict) else None
    if not isinstance(jobs, dict):
        return [f"{WORKFLOW_PATH}: no `jobs:` mapping."]

    problems: list[str] = []
    for job in (AUDITED_JOB, STATIC_AUDIT_JOB, PROBE_JOB, AGGREGATE_JOB):
        if not isinstance(jobs.get(job), dict):
            problems.append(f"{WORKFLOW_PATH}: no `{job}` job.")
    if problems:
        return problems

    aggregate = jobs[AGGREGATE_JOB]
    raw_needs = aggregate.get("needs")
    needs = ({raw_needs} if isinstance(raw_needs, str)
             else set(raw_needs) if isinstance(raw_needs, list)
             else set())
    if needs != AGGREGATE_NEEDS:
        problems.append(
            f"{WORKFLOW_PATH}: `{AGGREGATE_JOB}` must need exactly "
            f"{sorted(AGGREGATE_NEEDS)}, got {sorted(needs)}.")
    if normalise_expression(str(aggregate.get("if") or "")) != "always()":
        problems.append(
            f"{WORKFLOW_PATH}: `{AGGREGATE_JOB}` must use `if: always()` so "
            "a failed or skipped dependency cannot skip the stable verdict.")

    aggregate_steps = aggregate.get("steps")
    steps = ([step for step in aggregate_steps if isinstance(step, dict)]
             if isinstance(aggregate_steps, list) else [])
    verdict_steps = [step for step in steps
                     if "TESTS_RESULT" in str(step.get("run") or "")]
    if len(verdict_steps) != 1:
        problems.append(
            f"{WORKFLOW_PATH}: `{AGGREGATE_JOB}` must have exactly one "
            "worker-result verdict step.")
    else:
        env = verdict_steps[0].get("env")
        expected_env = {
            "EVENT_NAME": "${{ github.event_name }}",
            "TESTS_RESULT": "${{ needs.test-and-audits.result }}",
            "AUDITS_RESULT": "${{ needs.static-audits.result }}",
            "PROBES_RESULT": "${{ needs.behavior-probes.result }}",
        }
        if env != expected_env:
            problems.append(
                f"{WORKFLOW_PATH}: `{AGGREGATE_JOB}` worker-result env must "
                f"be exactly {expected_env}, got {env}.")
    aggregate_text = "\n".join(
        str(step.get("run") or "") for step in steps)
    for token in ("pull_request",
                  'test "$TESTS_RESULT" = success',
                  # Unconditional on purpose: STATIC_AUDIT_JOB carries no
                  # job-level `if:`, so a failure, a cancellation and a skip
                  # are all non-success on every event this workflow runs on.
                  'test "$AUDITS_RESULT" = success',
                  'test "$PROBES_RESULT" = success',
                  'test "$PROBES_RESULT" = skipped'):
        if token not in aggregate_text and token not in str(aggregate_steps):
            problems.append(
                f"{WORKFLOW_PATH}: `{AGGREGATE_JOB}` does not enforce "
                f"{token!r} in its worker-result verdict.")

    # The static-audit worker's whole reason to exist is that it waits on
    # nothing but the image and skips on nothing (#2272). Both halves are
    # one edit away from being undone, and neither would fail any other
    # check here: a `needs: test-and-audits` would put it right back behind
    # the build, and a docs-only or event condition would take the audits
    # off exactly the changes #1490 proved need them.
    static = jobs[STATIC_AUDIT_JOB]
    if str(static.get("if") or "").strip():
        problems.append(
            f"{WORKFLOW_PATH}: `{STATIC_AUDIT_JOB}` must carry no job-level "
            "`if:`. It runs on every event the workflow runs on -- docs-only "
            "pull requests and docs-only master pushes included -- because "
            "#1490's cause was a docs-only push breaking an engine-free "
            "audit.")
    raw_static_needs = static.get("needs")
    static_needs = ({raw_static_needs} if isinstance(raw_static_needs, str)
                    else set(raw_static_needs)
                    if isinstance(raw_static_needs, list) else set())
    if static_needs != {IMAGE_JOB}:
        problems.append(
            f"{WORKFLOW_PATH}: `{STATIC_AUDIT_JOB}` must need exactly "
            f"['{IMAGE_JOB}'], got {sorted(static_needs)}. Anything else "
            "makes the engine-free gates wait for a build again, which is "
            "the whole cost this split removed.")
    static_image = ((static.get("container") or {}).get("image")
                    if isinstance(static.get("container"), dict) else None)
    audited_image = ((jobs[AUDITED_JOB].get("container") or {}).get("image")
                     if isinstance(jobs[AUDITED_JOB].get("container"), dict)
                     else None)
    if static_image != audited_image:
        problems.append(
            f"{WORKFLOW_PATH}: `{STATIC_AUDIT_JOB}` runs in "
            f"{static_image!r} but `{AUDITED_JOB}` runs in "
            f"{audited_image!r}. Both must resolve the SAME CI image, which "
            "is what guarantees the moved gates keep the Python toolchain "
            ".github/ci/Dockerfile pins for them.")

    probe = jobs[PROBE_JOB]
    if normalise_expression(str(probe.get("if") or "")) != PROBE_JOB_IF:
        problems.append(
            f"{WORKFLOW_PATH}: `{PROBE_JOB}` must remain PR-only with "
            f"`if: {PROBE_JOB_IF}`.")
    commands = workflow_invocations(yaml_text, PROBE_JOB,
                                    f"{WORKFLOW_PATH} (job: {PROBE_JOB})")
    missing = PROBE_REQUIRED_COMMANDS - commands
    for command in sorted(missing):
        problems.append(
            f"{WORKFLOW_PATH}: `{PROBE_JOB}` no longer runs required "
            f"command `{command}`.")
    return problems


def audit_unit_asset_gate_wiring(yaml_text: str) -> list[str]:
    """Pin the unit-asset gate's selection where the gate now lives.

    Requirement 4 of #2272: the path-selective unit-asset gate keeps its
    pull-request base-diff selection and its unconditional behavior on
    every other event, wherever it ends up. It ended up in
    STATIC_AUDIT_JOB, and its selector moved with it -- so the two halves
    can now drift apart in a way the gate-set comparison cannot see: that
    comparison proves both commands exist somewhere, never that the guard
    reads the output the selector writes. A guard left naming
    `test-and-audits`' selector id would read an always-empty value and
    silently stop running the gate on every pull request, exactly the
    false green the save-compat wiring check exists to prevent for its own
    conditional member.
    """
    where = workflow_label(STATIC_AUDIT_JOB)
    problems: list[str] = []
    steps = workflow_steps(yaml_text, STATIC_AUDIT_JOB, where)

    gate_steps = [step for step in steps
                  if UNIT_ASSET_COMMAND in str(step.get("run") or "")]
    if len(gate_steps) != 1:
        problems.append(
            f"{where}: expected exactly one step running "
            f"`{UNIT_ASSET_COMMAND}`, found {len(gate_steps)}.")
    else:
        condition = normalise_expression(str(gate_steps[0].get("if") or ""))
        if condition != normalise_expression(UNIT_ASSET_CI_IF):
            problems.append(
                f"{where}: the step running `{UNIT_ASSET_COMMAND}` is "
                f"guarded by {condition!r}, not by the pinned "
                f"{normalise_expression(UNIT_ASSET_CI_IF)!r}. That guard is "
                "path-selective on pull requests and unconditional on every "
                "other event, so a path the selector does not list can never "
                "leave the inventory unchecked on master.")

    selector_steps = [step for step in steps
                      if UNIT_ASSET_SELECTOR_COMMAND
                      in str(step.get("run") or "")]
    if len(selector_steps) != 1:
        problems.append(
            f"{where}: expected exactly one step running "
            f"`{UNIT_ASSET_SELECTOR_COMMAND}`, found {len(selector_steps)}. "
            "The selector belongs in the same job as the gate it decides.")
        return problems

    step_id = str(selector_steps[0].get("id") or "")
    reference = f"steps.{step_id}.outputs.{UNIT_ASSET_GATE}"
    if not step_id:
        problems.append(
            f"{where}: the step running `{UNIT_ASSET_SELECTOR_COMMAND}` has "
            "no `id:`, so no guard can name its output.")
    elif reference not in normalise_expression(UNIT_ASSET_CI_IF):
        problems.append(
            f"{where}: the pinned guard does not read `{reference}`, so the "
            "step that decides the gate and the step that consumes the "
            "decision are not connected.")
    if f"{UNIT_ASSET_GATE}=" not in str(selector_steps[0].get("run")):
        problems.append(
            f"{where}: the selector step never writes a "
            f"`{UNIT_ASSET_GATE}=` output, so its guard reads an "
            "always-empty value and the gate silently stops running on "
            "pull requests.")
    return problems
