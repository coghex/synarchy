#!/usr/bin/env python3
"""CI / `make ci` gate-set parity audit (issue #1355).

`CLAUDE.md` promises that `make ci` runs the checks CI runs, so a green
local gate predicts a green push. Nothing enforced that, and it had
already drifted: `.github/workflows/ci.yml`'s probe-runner self-test step
had no counterpart in `tools/ci-local.sh` at all, so a broken
path-to-probe or path-to-expensive-gate mapping surfaced only after a
push, as a PR mis-selecting its own gates. Every other cross-file
contract in this repository is mechanically enforced (persistence
inventory, EngineEnv capability inventory, findings-report status, enum
append-only, cabal module inventory); this one — the gate that decides
whether a change is safe to push — was not.

What this compares
------------------
The set of `python3 tools/*.py` invocations run by the workflow's
`test-and-audits` worker against the set run by `tools/ci-local.sh`, in both
directions, minus a hard-coded exemption list. An invocation is compared
at COMMAND granularity INCLUDING ARGUMENTS, so
`ci_expensive_gates.py --self-test` and
`ci_expensive_gates.py --stdin --gate worldgen` are two different checks,
and retuning an existing check's flags on one side alone is drift.

What this deliberately does NOT compare
---------------------------------------
* **Conditional control flow.** CI path-selects the graphical test-suite
  build, the unit-asset gate and `world_check` on pull requests;
  `ci-local.sh` runs each unconditionally. That asymmetry is the design
  (a local gate has no PR base to be selective about and is meant to be
  conservative), so parity here is over the gate SET, not over the `if:`
  expressions guarding it.
* **Non-Python steps.** `cabal build`/`cabal test`, environment
  preparation and the cache actions are not part of the audited gate set.
  A `python3` command that names no `tools/*.py` script — `python3 -m pip
  install ...`, say — is environment preparation too and is ignored.
* **Comments.** Only executable content counts: YAML comments are dropped
  by the YAML parser, shell comments by `ci_parity_shell.py`'s lexer.
  A
  commented-out check does not run, so it must not count as running.
* **Other jobs.** Gate-set parity inspects only `test-and-audits`.
  `resolve-image` builds the CI image and has no local counterpart by
  construction; `behavior-probes` runs the opt-in, real-engine tier that
  `make ci` deliberately excludes; `build-test` is the stable aggregate
  context consumed by branch protection and the PR drainer. This audit
  separately pins that three-job wiring and the two probe-runner commands.

Failing loudly rather than silently
-----------------------------------
The real hazard for an audit like this is a shape its extractor does not
recognise: a missed invocation reads as parity that is not there. So the
extractor refuses rather than shrugs. A `python` interpreter appearing
anywhere other than as the head of a command (`xargs python3 ...`), a
`tools/*.py` script executed directly instead of through an interpreter,
an unterminated quote or `$(`/`${`, a missing audited worker, and an
empty invocation set on either side are each an error naming the offending
text — never a quietly smaller comparison.

Usage:
  python3 tools/ci_parity_audit.py
  python3 tools/ci_parity_audit.py --self-test
Exit codes: 0 = the two gate sets agree, 1 = they do not (or a self-test
check failed, or the input could not be parsed).

Where each layer lives
----------------------
This file is the public facade: the two commands, the repository
orchestration, the reporting, and the compatibility re-exports. The
layers it composes each have one owner, and dependencies run one way --
nothing below imports this file.

  tools/ci_parity_shell.py       the shell lexer and invocation
                                 extractor, and `AuditError`. A leaf: no
                                 repository paths, no YAML, no policy.
  tools/ci_parity_config.py      the production constants above --
                                 paths, job names, labels, the probe
                                 contract and EXEMPT_COMMANDS. A leaf
                                 both this facade and the workflow owner
                                 import, which is what single-sources
                                 them without a cycle.
  tools/ci_parity_workflow.py    workflow YAML parsing, the two-way
                                 gate-set comparison, and the aggregate
                                 and probe job topology.
  tools/ci_parity_save_compat.py the save-compat reproducibility wiring
                                 (#1360): the executable local block and
                                 the pinned CI guards.
  tools/test_ci_parity_audit.py  every fixture and self-test case,
                                 reached only through `--self-test`.
"""
from __future__ import annotations

import argparse
import os
import sys

# `tools/` has no `__init__.py`, so it is an implicit namespace package and
# `import tools.ci_parity_audit` from the repository root leaves `tools/`
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
    LOCAL_GATE_PATH,
    PROBE_JOB,
    PROBE_JOB_IF,
    PROBE_REQUIRED_COMMANDS,
    REPO_ROOT,
    WORKFLOW_LABEL,
    WORKFLOW_PATH,
)
from ci_parity_save_compat import audit_save_compat_reproducibility_wiring
from ci_parity_shell import (
    AuditError,
    extract_invocations,
    split_shell_commands,
)
from ci_parity_workflow import (
    audit_gate_sets,
    audit_parallel_gate_wiring,
    local_gate_invocations,
    workflow_invocations,
    workflow_run_blocks,
    workflow_steps,
)

#: The public surface, re-exported from the owners above. Every name here
#: is the CANONICAL object, not a wrapper: `ci_parity_audit.audit_gate_sets
#: is ci_parity_workflow.audit_gate_sets`, and the self-test asserts
#: exactly that for each one. A copied wrapper would let production and
#: the self-test drift apart silently, which is the failure mode
#: requirement 22 of #2159 exists to prevent.
__all__ = [
    "AuditError",
    "AGGREGATE_JOB",
    "AGGREGATE_NEEDS",
    "AUDITED_JOB",
    "EXEMPT_COMMANDS",
    "LOCAL_GATE_LABEL",
    "LOCAL_GATE_PATH",
    "PROBE_JOB",
    "PROBE_JOB_IF",
    "PROBE_REQUIRED_COMMANDS",
    "REPO_ROOT",
    "WORKFLOW_LABEL",
    "WORKFLOW_PATH",
    "audit_gate_sets",
    "audit_parallel_gate_wiring",
    "audit_save_compat_reproducibility_wiring",
    "extract_invocations",
    "local_gate_invocations",
    "main",
    "main_self_test",
    "run_repository_audit",
    "split_shell_commands",
    "workflow_invocations",
    "workflow_run_blocks",
    "workflow_steps",
]


def run_repository_audit() -> int:
    try:
        yaml_text = WORKFLOW_PATH.read_text(encoding="utf-8")
        shell_text = LOCAL_GATE_PATH.read_text(encoding="utf-8")
        ci_commands = workflow_invocations(yaml_text)
        local_commands = local_gate_invocations(shell_text)
        problems = audit_gate_sets(ci_commands, local_commands)
        # The gate SET agreeing is not enough for a check both files run
        # conditionally (#1360): they must also agree on the condition.
        problems.extend(
            audit_save_compat_reproducibility_wiring(yaml_text, shell_text))
        problems.extend(audit_parallel_gate_wiring(yaml_text))
    except OSError as error:
        print(f"ci_parity_audit.py: {error}")
        return 1
    except AuditError as error:
        print(f"ci_parity_audit.py: {error}")
        return 1

    if problems:
        print("CI / `make ci` gate-set parity audit FAILED "
              f"({len(problems)} problem(s)):")
        for problem in problems:
            print(f"  {problem}")
        print()
        print("Every `python3 tools/*.py` check the workflow's "
              f"`{AUDITED_JOB}` job runs must also run in "
              f"{LOCAL_GATE_LABEL}, and vice versa, unless it is on this "
              "audit's hard-coded EXEMPT_COMMANDS list, where every entry "
              "carries its reason.")
        return 1

    shared = len(ci_commands & local_commands)
    exempted = len(ci_commands | local_commands) - shared
    print(f"ci_parity_audit.py: {shared} check(s) run identically in "
          f"{WORKFLOW_LABEL} and {LOCAL_GATE_LABEL}; {exempted} exempt.")
    return 0



def main_self_test() -> int:
    # Imported HERE rather than at module scope: the self-test owner
    # imports this facade to assert its re-exports are the canonical
    # objects, and a module-level import either way would be a cycle.
    import test_ci_parity_audit

    try:
        failures = test_ci_parity_audit.self_test()
    except AuditError as error:
        print(f"self-test aborted: a fixture failed to parse: {error}")
        return 1
    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for failure in failures:
            print(f"  FAIL: {failure}")
        return 1
    print("ci_parity_audit.py self-test: all checks passed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Audit that tools/ci-local.sh runs the same "
                    "`python3 tools/*.py` gate set as .github/workflows/"
                    f"ci.yml's `{AUDITED_JOB}` job.")
    parser.add_argument("--self-test", action="store_true",
                        help="run the audit's own fixture checks instead of "
                             "auditing the repository")
    args = parser.parse_args()
    if args.self_test:
        return main_self_test()
    return run_repository_audit()


if __name__ == "__main__":
    raise SystemExit(main())
