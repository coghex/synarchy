#!/usr/bin/env python3
"""Workflow parsing, gate-set comparison and job topology (issue #1355).

The middle layer of the CI/`make ci` parity audit
(`tools/ci_parity_audit.py`). It consumes `tools/ci_parity_shell.py`'s
lexer and `tools/ci_parity_config.py`'s production constants, and knows
nothing about save-compat policy or the self-test.

What it owns
------------
Declared-shell validation, YAML job and step lookup, extraction of a
job's executable `run:` bodies, the workflow-side and local-side
invocation collections, the two-way gate-set comparison with its
stale-exemption detection, aggregate-job dependency validation, the
behavior-probe job's condition and required commands, and the
expression normalization those checks need.

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
    EXEMPT_COMMANDS,
    LOCAL_GATE_LABEL,
    PROBE_JOB,
    PROBE_JOB_IF,
    PROBE_REQUIRED_COMMANDS,
    WORKFLOW_LABEL,
    WORKFLOW_PATH,
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
            f"{where}: no `{job}` job. This audit mirrors exactly that job's "
            "gate set; if it was renamed, update AUDITED_JOB deliberately.")
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


def audit_gate_sets(
    ci_commands: set[str],
    local_commands: set[str],
    exempt_commands: tuple[tuple[str, str], ...] = EXEMPT_COMMANDS,
) -> list[str]:
    """Compare two gate sets. Returns a list of human-readable problems."""
    problems: list[str] = []

    for command, reason in exempt_commands:
        if not reason.strip():
            problems.append(
                f"exemption {command!r} carries no stated reason.")
    if not ci_commands:
        problems.append(
            f"{WORKFLOW_LABEL} yielded no `python3 tools/*.py` invocations; "
            "an empty gate set cannot certify parity.")
    if not local_commands:
        problems.append(
            f"{LOCAL_GATE_LABEL} yielded no `python3 tools/*.py` invocations; "
            "an empty gate set cannot certify parity.")

    exempt_command_set = {command for command, _ in exempt_commands}
    def exempt(command: str) -> bool:
        return command in exempt_command_set

    ci_only = sorted(c for c in ci_commands - local_commands if not exempt(c))
    local_only = sorted(c for c in local_commands - ci_commands if not exempt(c))

    for command in ci_only:
        problems.append(
            f"run by {WORKFLOW_LABEL} but not by {LOCAL_GATE_LABEL}: "
            f"{command}")
    for command in local_only:
        problems.append(
            f"run by {LOCAL_GATE_LABEL} but not by {WORKFLOW_LABEL}: "
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
    for job in (AUDITED_JOB, PROBE_JOB, AGGREGATE_JOB):
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
                  'test "$PROBES_RESULT" = success',
                  'test "$PROBES_RESULT" = skipped'):
        if token not in aggregate_text and token not in str(aggregate_steps):
            problems.append(
                f"{WORKFLOW_PATH}: `{AGGREGATE_JOB}` does not enforce "
                f"{token!r} in its worker-result verdict.")

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
