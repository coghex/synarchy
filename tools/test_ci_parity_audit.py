#!/usr/bin/env python3
"""Fixtures and self-test cases for the CI parity audit (issues #1355, #2159).

Every synthetic fixture and every assertion `--self-test` runs lives
here, and nowhere else. `tools/ci_parity_audit.py --self-test` is still
the only way to run them: this module registers no CI command and no
parity exemption of its own, and exposes no command line.

Where the assertions point
--------------------------
At the CANONICAL owners -- `ci_parity_shell`, `ci_parity_config`,
`ci_parity_workflow`, `ci_parity_save_compat` -- never at a facade
alias. A constant read through `ci_parity_audit` would let a mutation of
it pass while production kept reading the real one, which is exactly the
drift a self-test exists to catch.

The facade is imported all the same, for two structural jobs the split
itself created (#2159): proving each re-exported name IS the canonical
object rather than a copied wrapper, and proving the facade has not
regrown a lexer, a harness or a fixture body of its own. To keep that
from being an import cycle, `ci_parity_audit.main_self_test` imports
THIS module from inside its `--self-test` branch, not at module scope.

Isolation
---------
No repository workflow or local-gate file is read or written. The
save-compat block runs against temporary files under the stand-in
interpreter, so the real `ci_expensive_gates.py` still makes the
decision -- it must be the genuine one -- while the expensive test is
only recorded. No Cabal invocation, no network, no engine, no GPU, and
no hosted-CI interaction.
"""
from __future__ import annotations

import ast
import os
import sys
from pathlib import Path

# `tools/` has no `__init__.py`, so it is an implicit namespace package and
# `import tools.test_ci_parity_audit` from the repository root leaves
# `tools/` itself OFF sys.path. Every sibling import below runs at IMPORT
# time, so this has to come before the first one or that spelling raises
# ModuleNotFoundError while `python3 tools/ci_parity_audit.py --self-test`
# keeps working.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_parity_audit  # noqa: E402
import ci_parity_config  # noqa: E402
import ci_parity_save_compat  # noqa: E402
import ci_parity_shell  # noqa: E402
import ci_parity_workflow  # noqa: E402
import ci_expensive_gates  # noqa: E402
from ci_parity_config import (  # noqa: E402
    AUDITED_JOB,
    EXEMPT_COMMANDS,
    LOCAL_GATE_LABEL,
    STATIC_AUDIT_JOB,
    STATIC_AUDIT_LABEL,
    WORKFLOW_LABEL,
    WORKFLOW_UNION_LABEL,
)
from ci_parity_save_compat import (  # noqa: E402
    LOCAL_BLOCK_BEGIN,
    LOCAL_BLOCK_END,
    audit_save_compat_reproducibility_wiring,
)
from ci_parity_shell import (  # noqa: E402
    AuditError,
    cabal_subcommand,
    extract_cabal_commands,
    extract_invocations,
)
from ci_parity_workflow import (  # noqa: E402
    audit_cabal_verbosity,
    audit_gate_sets,
    audit_parallel_gate_wiring,
    audit_unit_asset_gate_wiring,
    local_gate_invocations,
    workflow_invocations,
    workflow_job_invocations,
)


_FIXTURE_WORKFLOW = """\
name: fixture
on: [push]
jobs:
  resolve-image:
    runs-on: ubuntu-latest
    steps:
      # A comment mentioning python3 tools/comment_in_other_job.py
      - name: elsewhere
        run: python3 tools/other_job_only.py
  build-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      # A YAML comment mentioning python3 tools/yaml_comment_only.py
      - name: single-line run
        run: python3 tools/single.py
      - name: block run
        run: |
          # a shell comment mentioning python3 tools/shell_comment_only.py
          python3 tools/block_one.py
          python3 tools/block_two.py --flag value
      - name: environment preparation
        run: python3 -m pip install --no-cache-dir PyYAML==6.0.2
      - name: pipeline member with a continuation
        run: |
          printf '%s\\n' "$CHANGED" | python3 tools/piped.py --stdin --gate worldgen | \\
            sed 's/^/worldgen=/' >> "$GITHUB_OUTPUT"
      - name: command substitution
        run: |
          ONLY="$(printf '%s\\n' "$CHANGED" | python3 tools/substituted.py --stdin)"
"""

_FIXTURE_EXPECTED = {
    "python3 tools/single.py",
    "python3 tools/block_one.py",
    "python3 tools/block_two.py --flag value",
    "python3 tools/piped.py --stdin --gate worldgen",
    "python3 tools/substituted.py --stdin",
}

_FIXTURE_LOCAL = """\
#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

# a shell comment mentioning python3 tools/shell_comment_only.py
echo "==> [1/5] single (not a real python3 tools/echoed.py invocation)"
python3 tools/single.py
python3 tools/block_one.py
python3 tools/block_two.py --flag value
printf '%s\\n' "$CHANGED" | python3 tools/piped.py --stdin --gate worldgen | \\
  sed 's/^/worldgen=/' > /dev/null
ONLY="$(python3 tools/substituted.py --stdin)"
"""


def _expect(failures: list[str], condition: bool, message: str) -> None:
    if not condition:
        failures.append(message)


def _raises(failures: list[str], label: str, thunk, needle: str) -> None:
    try:
        thunk()
    except AuditError as error:
        if needle not in str(error):
            failures.append(
                f"{label}: error did not name {needle!r}; got {error}")
        return
    failures.append(f"{label}: expected an AuditError, none was raised")


def _self_test() -> list[str]:
    failures: list[str] = []

    # 1. Extraction covers every shape the real workflow uses, counts
    #    neither comments nor another job, and ignores `python3 -m pip`.
    ci = workflow_invocations(_FIXTURE_WORKFLOW, "build-test", "fixture-ci")
    _expect(failures, ci == _FIXTURE_EXPECTED,
            f"fixture workflow extraction: expected {sorted(_FIXTURE_EXPECTED)}, "
            f"got {sorted(ci)}")
    local = local_gate_invocations(_FIXTURE_LOCAL, "fixture-local")
    _expect(failures, local == _FIXTURE_EXPECTED,
            f"fixture shell extraction: expected {sorted(_FIXTURE_EXPECTED)}, "
            f"got {sorted(local)}")

    # 2. Parity passes with nothing exempted. The CI side is a MAPPING of
    #    audited job to that job's own set; a single-job mapping is the
    #    degenerate case these mutations only need one job for.
    one_job = {AUDITED_JOB: ci}
    _expect(failures, audit_gate_sets(one_job, local, ()) == [],
            "matched gate sets should report no problems, got "
            f"{audit_gate_sets(one_job, local, ())}")

    # 3. A non-exempt CI-only invocation fails and names the command.
    dropped = local - {"python3 tools/block_two.py --flag value"}
    problems = audit_gate_sets(one_job, dropped, ())
    _expect(failures,
            any("python3 tools/block_two.py --flag value" in p
                and "not by" in p and LOCAL_GATE_LABEL in p for p in problems),
            "a CI-only invocation should be reported as missing locally, got "
            f"{problems}")

    # 4. A non-exempt local-only invocation fails and names the command,
    #    pointing at the two-job UNION rather than at one job: it is
    #    missing from both, so naming either would name the wrong place.
    added = local | {"python3 tools/local_extra.py"}
    problems = audit_gate_sets(one_job, added, ())
    _expect(failures,
            any("python3 tools/local_extra.py" in p
                and WORKFLOW_UNION_LABEL in p for p in problems),
            "a local-only invocation should be reported as missing in CI, got "
            f"{problems}")

    # 5. Changing an invocation's ARGUMENTS is drift in both directions.
    retuned = (local - {"python3 tools/block_two.py --flag value"}) | {
        "python3 tools/block_two.py --flag other"}
    problems = audit_gate_sets(one_job, retuned, ())
    _expect(failures,
            any("--flag value" in p for p in problems)
            and any("--flag other" in p for p in problems),
            "an argument change should be reported from both sides, got "
            f"{problems}")

    # 6. Every hard-coded exemption is accepted when it appears CI-side only.
    shared = {"python3 tools/shared.py"}
    exempt_side = set(shared)
    for command, _reason in EXEMPT_COMMANDS:
        exempt_side.add(command)
    problems = audit_gate_sets({AUDITED_JOB: exempt_side}, shared)
    _expect(failures, problems == [],
            f"the real exemptions should be accepted, got {problems}")

    # 6b. …and each is load-bearing: without the list, each one is drift.
    problems = audit_gate_sets({AUDITED_JOB: exempt_side}, shared, ())
    for command, _reason in EXEMPT_COMMANDS:
        _expect(failures, any(command in p for p in problems),
                f"exemption {command!r} is not load-bearing: it was not "
                "reported as drift with the list emptied")
    # 7. A stale exemption fails, naming what nothing runs.
    problems = audit_gate_sets({AUDITED_JOB: shared}, shared)
    for command, _reason in EXEMPT_COMMANDS:
        _expect(failures,
                any("stale exemption" in p and command in p for p in problems),
                f"a stale exemption for {command!r} should be reported, got "
                f"{problems}")
    # 8. Every exemption states a reason.
    for command, reason in EXEMPT_COMMANDS:
        _expect(failures, bool(reason.strip()),
                f"exemption {command!r} has no reason")
    _expect(failures,
            audit_gate_sets({AUDITED_JOB: shared | {"python3 tools/x.py"}},
                            shared,
                            (("python3 tools/x.py", "   "),)) != [],
            "a blank exemption reason should be reported")

    # 9. Vacuity on either side is a failure, not a pass. Checked with BOTH
    #    sides empty, so drift cannot report the failure on vacuity's
    #    behalf and hide the fact that nothing checks for it.
    problems = audit_gate_sets({}, set(), ())
    _expect(failures,
            any("no audited workflow job" in p for p in problems),
            f"collecting no audited job at all should fail, got {problems}")
    _expect(failures,
            any("empty gate set" in p and LOCAL_GATE_LABEL in p
                for p in problems),
            f"an empty local gate set should fail on its own, got {problems}")

    # 10. Shapes the extractor refuses rather than silently dropping.
    _raises(failures, "interpreter not at the head of a command",
            lambda: extract_invocations("xargs python3 tools/foo.py", "fixture"),
            "other than")
    _raises(failures, "directly executed tools script",
            lambda: extract_invocations("tools/foo.py --x", "fixture"),
            "directly")
    _raises(failures, "unterminated substitution",
            lambda: extract_invocations('X="$(python3 tools/foo.py"', "fixture"),
            "unterminated")
    _raises(failures, "unterminated quote",
            lambda: extract_invocations("echo 'oops", "fixture"),
            "unterminated")
    _raises(failures, "missing audited job",
            lambda: workflow_invocations(_FIXTURE_WORKFLOW, "no-such-job",
                                         "fixture"),
            "no-such-job")

    # 9b. The two-job union (#2272): every way the split itself can go
    #     wrong, each a mutation of one known-good two-job pair.
    failures.extend(_union_gate_set_self_test())

    # 10b. A `run:` body that is not shell is refused, not parsed as shell.
    for variant, label in (
        ("      - name: not shell\n        shell: python\n"
         "        run: print('python3 tools/not_a_gate.py')\n",
         "a step-level non-shell `shell:`"),
    ):
        _raises(failures, label,
                lambda v=variant: workflow_invocations(
                    _FIXTURE_WORKFLOW + v, "build-test", "fixture"),
                "will not parse")
    _raises(failures, "a job-level non-shell default",
            lambda: workflow_invocations(
                _FIXTURE_WORKFLOW.replace(
                    "  build-test:\n    runs-on: ubuntu-latest\n",
                    "  build-test:\n    runs-on: ubuntu-latest\n"
                    "    defaults:\n      run:\n        shell: python\n"),
                "build-test", "fixture"),
            "will not parse")
    _raises(failures, "a workflow-level non-shell default",
            lambda: workflow_invocations(
                _FIXTURE_WORKFLOW.replace(
                    "on: [push]\n",
                    "on: [push]\ndefaults:\n  run:\n    shell: python\n"),
                "build-test", "fixture"),
            "will not parse")
    _expect(failures,
            workflow_invocations(
                _FIXTURE_WORKFLOW.replace(
                    "      - name: single-line run\n",
                    "      - name: single-line run\n        shell: bash\n"),
                "build-test", "fixture") == _FIXTURE_EXPECTED,
            "an explicit `shell: bash` should still be read")

    # 11. A quoted mention of an interpreter is text, not an invocation.
    _expect(failures,
            extract_invocations('echo "run python3 tools/foo.py"', "fixture")
            == [],
            "a quoted mention should not be extracted as an invocation")

    # 12. A path-qualified interpreter is an invocation (and compares
    #     unequal to the bare form, which is the honest answer).
    _expect(failures,
            extract_invocations("/usr/bin/python3 tools/foo.py --x", "fixture")
            == ["/usr/bin/python3 tools/foo.py --x"],
            "a path-qualified interpreter should be extracted")
    _expect(failures,
            extract_invocations("python3 -m pip install PyYAML==6.0.2",
                                "fixture") == [],
            "`python3 -m pip install` is environment preparation, not a gate")

    # 13. The save-compat reproducibility wiring (#1360). Built from
    #     synthetic fixtures so the cases are mutations of a known-good
    #     pair, not of this repository's live files -- and the local half
    #     is genuinely EXECUTED, so "the block still selects correctly"
    #     is observed rather than asserted about its text.
    failures.extend(_save_compat_wiring_self_test())

    # 14. The stable build-test context must aggregate every parallel
    #     worker, and the split-out audit worker must keep its topology.
    failures.extend(_parallel_gate_wiring_self_test())

    # 14b. The unit-asset gate's selector and guard travelled together.
    failures.extend(_unit_asset_wiring_self_test())

    # 14c. Both entry points keep every Cabal build and test quiet
    #      (#1920) -- a contract the gate-set comparison cannot see,
    #      because `cabal` steps are outside the compared set.
    failures.extend(_cabal_verbosity_self_test())

    return failures


# --------------------------------------------------------------------------
# The two-job union (#2272)
# --------------------------------------------------------------------------
# `make ci` is one script, so the union of the two audited jobs is the only
# thing it can mirror -- which is exactly why the union alone is not enough
# to audit against. Three distinct mutations of one known-good pair pin the
# three ways the split can go wrong, and two more pin per-job vacuity.

_UNION_WORKFLOW = """\
name: fixture
on: [push, pull_request]
jobs:
  test-and-audits:
    runs-on: ubuntu-latest
    steps:
      - run: python3 tools/cabal_backed.py
  static-audits:
    runs-on: ubuntu-latest
    steps:
      - run: python3 tools/engine_free.py
      - run: echo not-a-gate
"""

_UNION_LOCAL = """\
#!/usr/bin/env bash
set -euo pipefail
python3 tools/cabal_backed.py
python3 tools/engine_free.py
"""


def _union_gate_set_self_test() -> list[str]:
    failures: list[str] = []
    jobs = workflow_job_invocations(_UNION_WORKFLOW)
    local = local_gate_invocations(_UNION_LOCAL, "fixture-local")

    # (a) The known-good pair passes: each job owns one gate, `make ci`
    #     runs both, nothing is shared. Every mutation below starts here,
    #     so a check that never fires shows up as a mutation that passes.
    _expect(failures, set(jobs) == {AUDITED_JOB, STATIC_AUDIT_JOB},
            f"the union fixture should yield both audited jobs, got "
            f"{sorted(jobs)}")
    _expect(failures, audit_gate_sets(jobs, local, ()) == [],
            "the known-good two-job fixture should pass, got "
            f"{audit_gate_sets(jobs, local, ())}")

    # (b) A LOCAL command absent from BOTH CI jobs. The union is what it
    #     is missing from, so the diagnostic must name the union rather
    #     than send the reader to one arbitrary job.
    local_only = local | {"python3 tools/local_only.py"}
    problems = audit_gate_sets(jobs, local_only, ())
    _expect(failures,
            any("python3 tools/local_only.py" in problem
                and WORKFLOW_UNION_LABEL in problem for problem in problems),
            "a local command absent from both CI jobs should be reported "
            f"against the union, got {problems}")

    # (c) A CI-union command absent locally, from EACH job in turn -- so a
    #     comparison that had quietly narrowed back to one job would fail
    #     one of these two rather than both passing.
    for job, command in ((AUDITED_JOB, "python3 tools/cabal_backed.py"),
                         (STATIC_AUDIT_JOB, "python3 tools/engine_free.py")):
        problems = audit_gate_sets(jobs, local - {command}, ())
        _expect(failures,
                any(command in problem and f"job: {job}" in problem
                    and LOCAL_GATE_LABEL in problem for problem in problems),
                f"{command!r} running only in {job} should be reported as "
                f"missing locally and named to that job, got {problems}")

    # (d) One command run by BOTH CI jobs. The union is unchanged, so it
    #     still matches the local side exactly: only a check that looks at
    #     the two sets BEFORE merging them can see this at all.
    duplicated = {AUDITED_JOB: set(jobs[AUDITED_JOB]),
                  STATIC_AUDIT_JOB: set(jobs[STATIC_AUDIT_JOB])}
    duplicated[STATIC_AUDIT_JOB].add("python3 tools/cabal_backed.py")
    _expect(failures,
            audit_gate_sets(duplicated, local, ()) != []
            and any("run by BOTH" in problem and "cabal_backed" in problem
                    for problem in audit_gate_sets(duplicated, local, ())),
            "a command run by both audited jobs should be reported, got "
            f"{audit_gate_sets(duplicated, local, ())}")

    # (e) Either audited job MISSING from the workflow refuses, naming it.
    for job in (AUDITED_JOB, STATIC_AUDIT_JOB):
        renamed = _UNION_WORKFLOW.replace(f"  {job}:\n", "  renamed-away:\n")
        _raises(failures, f"missing audited job {job}",
                lambda text=renamed: workflow_job_invocations(text),
                job)

    # (f) Either audited job PRESENT but yielding no auditable command
    #     fails on its own account, rather than shrinking the comparison
    #     to whatever the other job happens to run.
    for job, label in ((AUDITED_JOB, WORKFLOW_LABEL),
                       (STATIC_AUDIT_JOB, STATIC_AUDIT_LABEL)):
        emptied = {name: (set() if name == job else set(commands))
                   for name, commands in jobs.items()}
        problems = audit_gate_sets(emptied, local, ())
        _expect(failures,
                any("empty gate set" in problem and label in problem
                    for problem in problems),
                f"an emptied {job} should fail on its own, got {problems}")
    return failures


_PARALLEL_GATE_WORKFLOW_GOOD = """\
name: fixture
on: [push, pull_request]
jobs:
  test-and-audits:
    needs: resolve-image
    runs-on: ubuntu-latest
    container:
      image: ${{ needs.resolve-image.outputs.image }}
    steps:
      - run: python3 tools/audit.py
  static-audits:
    needs: resolve-image
    runs-on: ubuntu-latest
    container:
      image: ${{ needs.resolve-image.outputs.image }}
    steps:
      - run: python3 tools/static_audit.py
  behavior-probes:
    if: github.event_name == 'pull_request'
    runs-on: ubuntu-latest
    steps:
      - run: python3 tools/ci_probes.py --stdin
      - run: >-
          python3 tools/run_probes.py
          --only "${{ steps.probe-selection.outputs.only }}"
          --exact --retries 1 --jobs 2
  build-test:
    if: always()
    needs: [test-and-audits, static-audits, behavior-probes]
    runs-on: ubuntu-latest
    steps:
      - env:
          EVENT_NAME: ${{ github.event_name }}
          TESTS_RESULT: ${{ needs.test-and-audits.result }}
          AUDITS_RESULT: ${{ needs.static-audits.result }}
          PROBES_RESULT: ${{ needs.behavior-probes.result }}
        run: |
          test "$TESTS_RESULT" = success
          test "$AUDITS_RESULT" = success
          if [ "$EVENT_NAME" = pull_request ]; then
            test "$PROBES_RESULT" = success
          else
            test "$PROBES_RESULT" = skipped
          fi
"""


def _parallel_gate_wiring_self_test() -> list[str]:
    failures: list[str] = []

    def problems(text: str) -> list[str]:
        return audit_parallel_gate_wiring(text)

    _expect(failures, problems(_PARALLEL_GATE_WORKFLOW_GOOD) == [],
            "the known-good parallel gate fixture should pass, got "
            f"{problems(_PARALLEL_GATE_WORKFLOW_GOOD)}")
    mutations = (
        ("drop aggregate probe dependency",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             "needs: [test-and-audits, static-audits, behavior-probes]",
             "needs: [test-and-audits, static-audits]"), "must need exactly"),
        # The #2272 aggregate wiring, from both halves: the stable context
        # has to DEPEND on the split-out worker and has to ASSERT its
        # result. Dropping either one alone leaves the other looking fine.
        ("drop aggregate static-audit dependency",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             "needs: [test-and-audits, static-audits, behavior-probes]",
             "needs: [test-and-audits, behavior-probes]"),
         "must need exactly"),
        ("drop static-audit verdict assertion",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             '          test "$AUDITS_RESULT" = success\n', ""),
         'test "$AUDITS_RESULT" = success'),
        ("drop static-audit verdict env",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             "          AUDITS_RESULT: ${{ needs.static-audits.result }}\n",
             ""),
         "worker-result env must be exactly"),
        # ...and the worker's own topology, which is the whole point of the
        # split: image-only `needs`, and no condition at all.
        ("static audits waiting for the build job",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             "  static-audits:\n    needs: resolve-image\n",
             "  static-audits:\n    needs: [resolve-image, test-and-audits]\n"),
         "must need exactly ['resolve-image']"),
        ("static audits given a job-level condition",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             "  static-audits:\n    needs: resolve-image\n",
             "  static-audits:\n    if: github.event_name == 'pull_request'\n"
             "    needs: resolve-image\n"),
         "must carry no job-level"),
        ("static audits on a different image",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             "  static-audits:\n    needs: resolve-image\n"
             "    runs-on: ubuntu-latest\n"
             "    container:\n"
             "      image: ${{ needs.resolve-image.outputs.image }}\n",
             "  static-audits:\n    needs: resolve-image\n"
             "    runs-on: ubuntu-latest\n"),
         "must resolve the SAME CI image"),
        ("drop the static-audit job outright",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             "  static-audits:\n    needs: resolve-image\n"
             "    runs-on: ubuntu-latest\n"
             "    container:\n"
             "      image: ${{ needs.resolve-image.outputs.image }}\n"
             "    steps:\n      - run: python3 tools/static_audit.py\n", ""),
         "no `static-audits` job"),
        ("drop always",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace("    if: always()\n", ""),
         "must use `if: always()`"),
        ("drop probe runner",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             "      - run: >-\n          python3 tools/run_probes.py\n"
             "          --only \"${{ steps.probe-selection.outputs.only }}\"\n"
             "          --exact --retries 1 --jobs 2\n", ""),
         "no longer runs required command"),
        ("accept failed probes",
         _PARALLEL_GATE_WORKFLOW_GOOD.replace(
             'test "$PROBES_RESULT" = success',
             'test "$PROBES_RESULT" = failure'),
         'test "$PROBES_RESULT" = success'),
    )
    for label, mutated, needle in mutations:
        got = problems(mutated)
        _expect(failures, any(needle in problem for problem in got),
                f"{label} should fail with {needle!r}, got {got}")
    return failures


_UNIT_ASSET_WORKFLOW_GOOD = """\
name: fixture
on: [push, pull_request]
jobs:
  static-audits:
    runs-on: ubuntu-latest
    steps:
      - name: Select path-relevant unit-asset gate
        id: expensive-gates
        run: |
          printf '%s\\n' "$CHANGED" | python3 tools/ci_expensive_gates.py --stdin --gate unit-assets | \\
            sed 's/^/unit-assets=/' >> "$GITHUB_OUTPUT"
      - name: Unit asset inventory, freshness and budget
        if: >-
          github.event_name != 'pull_request'
          || steps.expensive-gates.outputs.unit-assets == 'true'
        run: |
          python3 tools/test_pack_atlas.py
          python3 tools/pack_atlas.py --validate-only --strict
"""


def _unit_asset_wiring_self_test() -> list[str]:
    """The gate and its selector must have travelled together (#2272).

    The gate-set comparison proves both commands exist somewhere in the
    two-job union; it can say nothing about whether the guard reads the
    output the selector writes. A guard still naming `test-and-audits`'
    selector step would read an always-empty value here and stop running
    the gate on every pull request, with the union still matching
    `tools/ci-local.sh` exactly.
    """
    failures: list[str] = []

    def problems(text: str = _UNIT_ASSET_WORKFLOW_GOOD) -> list[str]:
        return audit_unit_asset_gate_wiring(text)

    _expect(failures, problems() == [],
            f"the known-good unit-asset fixture should pass, got {problems()}")

    mutations = (
        # The regression the move makes possible: the gate is here, its
        # selector is not, so its guard reads a value nothing writes.
        ("selector left behind in the other job",
         _UNIT_ASSET_WORKFLOW_GOOD.replace(
             "      - name: Select path-relevant unit-asset gate\n"
             "        id: expensive-gates\n"
             "        run: |\n"
             "          printf '%s\\n' \"$CHANGED\" | python3 "
             "tools/ci_expensive_gates.py --stdin --gate unit-assets | \\\n"
             "            sed 's/^/unit-assets=/' >> \"$GITHUB_OUTPUT\"\n",
             ""),
         "expected exactly one step running"),
        # Losing the master half of the guard: a path the selector does
        # not list would then leave the inventory unchecked on master.
        ("gate made pull-request-selective on every event",
         _UNIT_ASSET_WORKFLOW_GOOD.replace(
             "          github.event_name != 'pull_request'\n"
             "          || steps.expensive-gates.outputs.unit-assets == "
             "'true'\n",
             "          steps.expensive-gates.outputs.unit-assets == "
             "'true'\n"),
         "not by the pinned"),
        # Losing the pull-request half: the gate would run on every PR.
        ("gate made unconditional",
         _UNIT_ASSET_WORKFLOW_GOOD.replace(
             "        if: >-\n"
             "          github.event_name != 'pull_request'\n"
             "          || steps.expensive-gates.outputs.unit-assets == "
             "'true'\n",
             ""),
         "guarded by"),
        # The selector runs but publishes nothing the guard can read.
        ("selector output never published",
         _UNIT_ASSET_WORKFLOW_GOOD.replace("sed 's/^/unit-assets=/'",
                                           "sed 's/^/unrelated=/'"),
         "never writes"),
        # ...or publishes under a step id no guard names.
        ("selector step renamed out from under the guard",
         _UNIT_ASSET_WORKFLOW_GOOD.replace("        id: expensive-gates\n",
                                           "        id: other-gates\n"),
         "does not read"),
        ("selector step with no id at all",
         _UNIT_ASSET_WORKFLOW_GOOD.replace("        id: expensive-gates\n",
                                           ""),
         "has no `id:`"),
    )
    for label, mutated, needle in mutations:
        got = problems(mutated)
        _expect(failures, any(needle in problem for problem in got),
                f"{label} should fail with {needle!r}, got {got}")
    return failures


# --------------------------------------------------------------------------
# Cabal build/test verbosity (#1920)
# --------------------------------------------------------------------------
# `cabal` steps are outside the gate-set comparison by that section's
# documented scope, so a command that quietly went verbose again compares
# equal to nothing at all. These fixtures are mutations of one known-good
# pair covering both entry points, both covered subcommands, the shapes
# that must NOT be covered, and vacuity on each side separately.

_CABAL_WORKFLOW_GOOD = """\
name: fixture
on: [push]
jobs:
  test-and-audits:
    runs-on: ubuntu-latest
    steps:
      - name: Toolchain
        run: /usr/local/.ghcup/bin/cabal --version
      - name: Resolve dependency plan
        run: |
          cabal build all -v0 --dry-run
          cabal update
      - name: Build
        run: cabal build all -v0
      - name: Build test suites
        run: |
          cabal build synarchy-test-headless -v0
          if [ "${{ steps.gates.outputs.graphical }}" = true ]; then
            cabal build synarchy-test-graphical -v0
          else
            echo "CABAL_DIR=/usr/local/cabal"
          fi
      - name: Headless test suite
        run: |
          SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless -v0 --test-show-details=direct
  behavior-probes:
    runs-on: ubuntu-latest
    steps:
      - name: Probe build
        run: cabal build exe:synarchy synarchy-test-headless
"""

_CABAL_LOCAL_GOOD = """#!/usr/bin/env bash
set -euo pipefail
step "cabal library module inventory audit"
cabal build all -v0
cabal build synarchy-test-headless -v0
SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless -v0 --test-show-details=direct
"""


def _cabal_verbosity_self_test() -> list[str]:
    """Every way the quiet setting can come undone, and one that cannot.

    The good pair must pass while carrying each shape the real files
    carry: an env-prefixed command, a path-qualified `cabal --version`
    that is not a build, a bare `cabal update`, a quoted mention, an
    `echo` of a value ending in `cabal`, and a verbose `behavior-probes`
    job that is deliberately out of scope.
    """
    failures: list[str] = []

    def problems(workflow: str = _CABAL_WORKFLOW_GOOD,
                 local: str = _CABAL_LOCAL_GOOD) -> list[str]:
        return audit_cabal_verbosity(workflow, local)

    _expect(failures, problems() == [],
            f"the known-good Cabal pair should pass, got {problems()}")

    # The out-of-scope job really is out of scope: the good pair passes
    # with `behavior-probes` verbose, and stays passing when that job is
    # made MORE verbose, so this is coverage of the exclusion rather than
    # an accident of the fixture.
    louder = _CABAL_WORKFLOW_GOOD.replace(
        "        run: cabal build exe:synarchy synarchy-test-headless\n",
        "        run: |\n"
        "          cabal build exe:synarchy synarchy-test-headless\n"
        "          cabal test synarchy-test-headless\n")
    _expect(failures, problems(workflow=louder) == [],
            "the behavior-probes job is out of scope for #1920 and must not "
            f"be audited, got {problems(workflow=louder)}")

    # The long-form spelling is the same setting, not drift.
    long_form = _CABAL_WORKFLOW_GOOD.replace("cabal build all -v0\n",
                                             "cabal build all --verbose=0\n")
    _expect(failures, problems(workflow=long_form) == [],
            "`--verbose=0` is verbosity 0 too and must be accepted, got "
            f"{problems(workflow=long_form)}")

    workflow_mutations = (
        ("the CI library build re-verbosed",
         _CABAL_WORKFLOW_GOOD.replace("run: cabal build all -v0\n",
                                      "run: cabal build all\n"),
         "cabal build all"),
        ("the CI dependency-plan dry run re-verbosed",
         _CABAL_WORKFLOW_GOOD.replace("cabal build all -v0 --dry-run",
                                      "cabal build all --dry-run"),
         "--dry-run"),
        ("the CI test-suite build re-verbosed",
         _CABAL_WORKFLOW_GOOD.replace("cabal build synarchy-test-headless -v0",
                                      "cabal build synarchy-test-headless"),
         "cabal build synarchy-test-headless"),
        ("the CI graphical build re-verbosed",
         _CABAL_WORKFLOW_GOOD.replace(
             "cabal build synarchy-test-graphical -v0",
             "cabal build synarchy-test-graphical"),
         "cabal build synarchy-test-graphical"),
        ("the CI headless test step re-verbosed",
         _CABAL_WORKFLOW_GOOD.replace(
             "cabal test synarchy-test-headless -v0",
             "cabal test synarchy-test-headless"),
         "cabal test synarchy-test-headless"),
        ("a near-miss verbosity flag",
         _CABAL_WORKFLOW_GOOD.replace("run: cabal build all -v0\n",
                                      "run: cabal build all -v1\n"),
         "cabal build all -v1"),
        ("every covered CI command removed",
         _CABAL_WORKFLOW_GOOD.replace("cabal build", "cabal update #")
                             .replace("cabal test", "cabal update #"),
         "no `cabal build`/`cabal test` command was found"),
    )
    for label, mutated, needle in workflow_mutations:
        got = problems(workflow=mutated)
        _expect(failures, any(needle in problem for problem in got),
                f"{label} should fail naming {needle!r}, got {got}")

    local_mutations = (
        ("the local library build re-verbosed",
         _CABAL_LOCAL_GOOD.replace("cabal build all -v0", "cabal build all"),
         "cabal build all"),
        ("the local test-suite build re-verbosed",
         _CABAL_LOCAL_GOOD.replace("cabal build synarchy-test-headless -v0",
                                   "cabal build synarchy-test-headless"),
         "cabal build synarchy-test-headless"),
        ("the local suite run re-verbosed",
         _CABAL_LOCAL_GOOD.replace("cabal test synarchy-test-headless -v0",
                                   "cabal test synarchy-test-headless"),
         "cabal test synarchy-test-headless"),
        ("every covered local command removed",
         "#!/usr/bin/env bash\nset -euo pipefail\ncabal update\n",
         "no `cabal build`/`cabal test` command was found"),
    )
    for label, mutated, needle in local_mutations:
        got = problems(local=mutated)
        _expect(failures, any(needle in problem for problem in got),
                f"{label} should fail naming {needle!r}, got {got}")

    # The extractor's own contract, asserted where it is owned. A quoted
    # mention is text; a value token ending in `cabal` is a value; an
    # env-prefixed command is the command; `cabal --version` has no
    # subcommand at all and is therefore never covered.
    _expect(failures,
            extract_cabal_commands('step "cabal module audit"', "fixture")
            == [],
            "a quoted mention should not be read as a Cabal command")
    _expect(failures,
            extract_cabal_commands('echo "CABAL_DIR=/usr/local/cabal"',
                                   "fixture") == [],
            "an echoed value ending in `cabal` should not be read as a "
            "Cabal command")
    _expect(failures,
            extract_cabal_commands("SYNARCHY_FULL_TESTS=1 cabal test x -v0",
                                   "fixture") == [["cabal", "test", "x",
                                                   "-v0"]],
            "a leading environment assignment should be stripped")
    _expect(failures,
            cabal_subcommand(["/usr/local/.ghcup/bin/cabal", "--version"])
            is None,
            "`cabal --version` names no subcommand and must not be covered")
    _expect(failures,
            cabal_subcommand(["cabal", "build", "all", "-v0"]) == "build",
            "`cabal build all -v0` should read as the `build` subcommand")
    _raises(failures, "a cabal not at the head of its command",
            lambda: extract_cabal_commands("xargs cabal build all", "fixture"),
            "other than the head")

    return failures


_WIRING_LOCAL_GOOD = """#!/usr/bin/env bash
set -euo pipefail
SAVE_COMPAT_PATHS="$(python3 tools/ci_expensive_gates.py --local-changed-paths)"
printf 'package synarchy\\n  ghc-options: -fforce-recomp\\n' > "$LOCAL"
python3 tools/test_save_compat_audit.py --without-reproducibility
python3 tools/save_compat_audit.py
# >>> save-compat reproducibility selection >>>
SAVE_COMPAT_REPRO="$(printf '%s\\n' "$SAVE_COMPAT_PATHS" | python3 tools/ci_expensive_gates.py --stdin --gate save-compat)"
if [ "$SAVE_COMPAT_REPRO" = true ]; then
  python3 tools/test_save_compat_audit.py --only-reproducibility
else
  echo skipped
fi
# <<< save-compat reproducibility selection <<<
"""

_WIRING_CI_GOOD = """
name: fixture
on: push
jobs:
  test-and-audits:
    runs-on: ubuntu-latest
    steps:
      - name: Select expensive path-relevant gates
        id: expensive-gates
        run: |
          printf '%s\\n' "$CHANGED" | python3 tools/ci_expensive_gates.py --stdin --gate save-compat | \\
            sed 's/^/save-compat=/' >> "$GITHUB_OUTPUT"
      - name: Save compatibility audit
        if: >-
          steps.docs-fast-path.outputs.docs_only != 'true'
          || steps.expensive-gates.outputs.save-compat == 'true'
        run: |
          python3 tools/test_save_compat_audit.py --without-reproducibility
          python3 tools/save_compat_audit.py
      - name: Save compatibility fixture reproducibility
        if: steps.expensive-gates.outputs.save-compat == 'true'
        run: python3 tools/test_save_compat_audit.py --only-reproducibility
"""


def _save_compat_wiring_self_test() -> list[str]:
    failures: list[str] = []

    def problems(ci: str = _WIRING_CI_GOOD,
                 local: str = _WIRING_LOCAL_GOOD) -> list[str]:
        return audit_save_compat_reproducibility_wiring(ci, local)

    # (a) The known-good pair passes. Every mutation below starts here,
    #     so a check that never fires would show up as a mutation that
    #     also passes.
    _expect(failures, problems() == [],
            f"the known-good wiring fixture should pass, got {problems()}")

    # (b) The local block that runs the member UNCONDITIONALLY is the
    #     failure this whole section exists for: the gate sets still
    #     match exactly, so only executing the block can catch it.
    unconditional = _WIRING_LOCAL_GOOD.replace(
        'if [ "$SAVE_COMPAT_REPRO" = true ]; then\n  '
        'python3 tools/test_save_compat_audit.py --only-reproducibility\n'
        'else\n  echo skipped\nfi\n',
        "python3 tools/test_save_compat_audit.py --only-reproducibility\n")
    _expect(failures,
            any("an unrelated change" in p for p in problems(local=unconditional)),
            "a local block running the member unconditionally should fail, "
            f"got {problems(local=unconditional)}")

    # (c) A local block guarded by a DIFFERENT gate's decision reverses
    #     both directions: worldgen selects on the Lua sample and not on
    #     the save sample.
    other_gate = _WIRING_LOCAL_GOOD.replace("--gate save-compat",
                                            "--gate worldgen")
    _expect(failures, problems(local=other_gate) != [],
            "a local block consulting a different gate should fail")

    # (d) A local block that never runs the member at all.
    never = _WIRING_LOCAL_GOOD.replace(
        "python3 tools/test_save_compat_audit.py --only-reproducibility\n",
        "echo would-run\n")
    _expect(failures,
            any("not reachable at all" in p for p in problems(local=never)),
            f"a block that never runs the member should fail, "
            f"got {problems(local=never)}")

    # (e) Reintroducing the BARE invocation on either side puts the repl
    #     back on every run.
    bare_local = _WIRING_LOCAL_GOOD.replace(
        "python3 tools/test_save_compat_audit.py --without-reproducibility",
        "python3 tools/test_save_compat_audit.py")
    _expect(failures,
            any("runs the bare" in p for p in problems(local=bare_local)),
            f"a bare local invocation should fail, got {problems(local=bare_local)}")
    bare_ci = _WIRING_CI_GOOD.replace(
        "python3 tools/test_save_compat_audit.py --without-reproducibility",
        "python3 tools/test_save_compat_audit.py")
    _expect(failures,
            any("runs the bare" in p for p in problems(ci=bare_ci)),
            f"a bare CI invocation should fail, got {problems(ci=bare_ci)}")

    # (f) Dropping the markers refuses rather than falling back to
    #     trusting the shell text.
    unmarked = _WIRING_LOCAL_GOOD.replace(LOCAL_BLOCK_BEGIN, "(removed)")
    _expect(failures,
            any("markers" in p for p in problems(local=unmarked)),
            f"a block with no markers should fail, got {problems(local=unmarked)}")

    # (f2) ...and so does a DUPLICATED block. Only the first marker pair
    #      is ever executed, so a second one invoking the member
    #      unconditionally would otherwise sail through: the gate sets
    #      still match (same command), the executed block still selects
    #      correctly, and `make ci` would run the `cabal repl` on every
    #      unrelated change with this audit reporting no problem at all.
    duplicated = _WIRING_LOCAL_GOOD + (
        "# " + LOCAL_BLOCK_BEGIN + "\n"
        "python3 tools/test_save_compat_audit.py --only-reproducibility\n"
        "# " + LOCAL_BLOCK_END + "\n")
    _expect(failures,
            any("exactly one of each" in p
                for p in problems(local=duplicated)),
            "a duplicated selection block should fail, got "
            f"{problems(local=duplicated)}")

    # (f3) A duplicated block is refused even when the extra one is
    #      harmless, because "the block this audit executes" and "the
    #      block that runs" must be the same block, not merely agree.
    duplicated_inert = _WIRING_LOCAL_GOOD + (
        "# " + LOCAL_BLOCK_BEGIN + "\n"
        "echo nothing-to-see-here\n"
        "# " + LOCAL_BLOCK_END + "\n")
    _expect(failures,
            any("exactly one of each" in p
                for p in problems(local=duplicated_inert)),
            "a duplicated selection block should fail even when inert, got "
            f"{problems(local=duplicated_inert)}")

    # (f4) A lone extra BEGIN marker is a malformed block, not a second
    #      one -- also refused, so the count check cannot be satisfied by
    #      pairing up stray markers.
    stray_begin = _WIRING_LOCAL_GOOD + "# " + LOCAL_BLOCK_BEGIN + "\n"
    _expect(failures,
            any("exactly one of each" in p
                for p in problems(local=stray_begin)),
            "a stray extra begin marker should fail, got "
            f"{problems(local=stray_begin)}")

    # (g) Making the reproducibility member unconditional fails, and so
    #     does swapping either guard for another gate's output.
    no_selection = _WIRING_CI_GOOD.replace(
        "        if: steps.expensive-gates.outputs.save-compat == 'true'\n",
        "        if: always()\n")
    _expect(failures,
            any("guarded by" in p for p in problems(ci=no_selection)),
            f"making the reproducibility member unconditional should fail, "
            f"got {problems(ci=no_selection)}")
    unguarded_main = _WIRING_CI_GOOD.replace(
        "        if: >-\n"
        "          steps.docs-fast-path.outputs.docs_only != 'true'\n"
        "          || steps.expensive-gates.outputs.save-compat == 'true'\n",
        "")
    _expect(failures,
            any("main audit may skip" in p
                for p in problems(ci=unguarded_main)),
            f"making the main audit unconditional should fail, "
            f"got {problems(ci=unguarded_main)}")
    wrong_output = _WIRING_CI_GOOD.replace(
        "steps.expensive-gates.outputs.save-compat == 'true'",
        "steps.expensive-gates.outputs.worldgen == 'true'")
    _expect(failures, problems(ci=wrong_output) != [],
            "a guard reading another gate's output should fail")

    # (h) A selector step that computes the decision but never publishes
    #     it leaves the guard reading an empty value.
    unpublished = _WIRING_CI_GOOD.replace("sed 's/^/save-compat=/'",
                                          "sed 's/^/unrelated=/'")
    _expect(failures,
            any("never writes" in p for p in problems(ci=unpublished)),
            f"an unpublished selector output should fail, "
            f"got {problems(ci=unpublished)}")

    # (i2) Resolving the local changed paths AFTER the scratch
    #      cabal.project.local write. Both commands still run, the sets
    #      still match, and the block still guards correctly -- only the
    #      ORDER is wrong, which is what would let this gate's own
    #      scratch edit select the gate once a change tracks that file.
    late_resolve = _WIRING_LOCAL_GOOD.replace(
        'SAVE_COMPAT_PATHS="$(python3 tools/ci_expensive_gates.py '
        '--local-changed-paths)"\n'
        "printf 'package synarchy\\n  ghc-options: -fforce-recomp\\n' "
        '> "$LOCAL"\n',
        "printf 'package synarchy\\n  ghc-options: -fforce-recomp\\n' "
        '> "$LOCAL"\n'
        'SAVE_COMPAT_PATHS="$(python3 tools/ci_expensive_gates.py '
        '--local-changed-paths)"\n')
    _expect(failures,
            any("AFTER" in p for p in problems(local=late_resolve)),
            "resolving the changed paths after the scratch write should "
            f"fail, got {problems(local=late_resolve)}")

    # (i3) Dropping the local resolution entirely.
    no_resolve = _WIRING_LOCAL_GOOD.replace(
        'SAVE_COMPAT_PATHS="$(python3 tools/ci_expensive_gates.py '
        '--local-changed-paths)"\n', "")
    _expect(failures,
            any("no local changed-path list" in p
                for p in problems(local=no_resolve)),
            "dropping the local changed-path resolution should fail, got "
            f"{problems(local=no_resolve)}")

    # (i) A CI reproducibility step with no `if:` at all -- the exact
    #     "runs on every PR again" regression, from the other side.
    unguarded_ci = _WIRING_CI_GOOD.replace(
        "        if: steps.expensive-gates.outputs.save-compat == 'true'\n",
        "")
    _expect(failures,
            any("guarded by" in p for p in problems(ci=unguarded_ci)),
            f"an unguarded CI step should fail, got {problems(ci=unguarded_ci)}")

    return failures


# --------------------------------------------------------------------------
# Structural checks over the split itself (#2159)
# --------------------------------------------------------------------------
# The split's ownership claims -- one owner per layer, no owner importing
# the facade, the facade holding no implementation body, every re-export
# being the canonical object -- are exactly the kind of property that
# reads as true today and quietly regrows tomorrow. So each is a check
# here rather than an observation in a review.

TOOLS = Path(__file__).resolve().parent

#: Every production owner the split created, from the FILESYSTEM rather
#: than a hand-kept list. Ground truth: a scan built this way can never
#: resolve to fewer modules than exist, which is how a post-split
#: structural check otherwise goes quietly vacuous when someone adds a
#: sixth owner and forgets to register it.
EXTRACTED = tuple(sorted(path for path in TOOLS.glob("ci_parity_*.py")
                         if path.name != "ci_parity_audit.py"))

#: The facade, and this module.
FACADE = TOOLS / "ci_parity_audit.py"
SELF_TEST_OWNER = Path(__file__).resolve()

#: The names `ci_parity_audit` defines itself rather than re-exporting.
#: Requirement 20 of #2159: the facade owns orchestration, reporting and
#: CLI dispatch, and nothing else.
FACADE_OWN_CALLABLES = frozenset({"run_repository_audit", "main_self_test",
                                  "main"})
#: ...and the only module-level VALUE it may bind. A fixture corpus, a
#: shim template, a lexer table or a production constant regrowing here
#: would each show up as a new name in this set.
FACADE_OWN_BINDINGS = frozenset({"__all__"})

#: The owner modules a re-exported name may be canonically defined by.
CANONICAL_OWNERS = (ci_parity_config, ci_parity_shell, ci_parity_workflow,
                    ci_parity_save_compat)


def _module_tree(path: Path) -> ast.Module:
    return ast.parse(path.read_text(encoding="utf-8"), filename=str(path))


def _top_level_callables(tree: ast.Module) -> set[str]:
    return {node.name for node in tree.body
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef,
                                 ast.ClassDef))}


def _top_level_bindings(tree: ast.Module) -> set[str]:
    names: set[str] = set()
    for node in tree.body:
        if isinstance(node, ast.Assign):
            names.update(target.id for target in node.targets
                         if isinstance(target, ast.Name))
        elif isinstance(node, ast.AnnAssign) and isinstance(node.target,
                                                            ast.Name):
            names.add(node.target.id)
    return names


def _defined_names(path: Path) -> set[str]:
    """Names a module DEFINES at top level -- imports deliberately excluded.

    Attribute presence cannot answer "who owns this?": ci_parity_workflow
    imports WORKFLOW_LABEL from ci_parity_config, so both modules have the
    attribute and only one defines it.
    """
    tree = _module_tree(path)
    return _top_level_callables(tree) | _top_level_bindings(tree)


def _imports_facade(tree: ast.Module) -> bool:
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            if any(alias.name.split(".")[-1] == "ci_parity_audit"
                   for alias in node.names):
                return True
        elif isinstance(node, ast.ImportFrom):
            module = node.module or ""
            if module.split(".")[-1] == "ci_parity_audit":
                return True
    return False


def _structural_self_test() -> list[str]:
    failures: list[str] = []

    # 15. The split actually happened, and this scan can see it. A glob
    #     that resolved to nothing would make every check below vacuous.
    names = {path.name for path in EXTRACTED}
    expected_owners = {"ci_parity_shell.py", "ci_parity_config.py",
                       "ci_parity_workflow.py", "ci_parity_save_compat.py"}
    _expect(failures, expected_owners <= names,
            f"the production owners {sorted(expected_owners - names)} are "
            f"missing from the filesystem scan, which found {sorted(names)}")

    # 16. No extracted owner imports the facade. Dependencies run one way
    #     (requirement 21): shell is a leaf, workflow consumes shell,
    #     save-compat consumes both, and the facade composes all of them.
    for path in EXTRACTED:
        _expect(failures, not _imports_facade(_module_tree(path)),
                f"{path.name} imports ci_parity_audit; extracted owners must "
                "not depend on the facade that composes them")

    # 17. The shell lexer stays a leaf: no repository paths, no YAML, no
    #     job topology, no save-compat policy, no exemptions
    #     (requirement 8). Checked as an import ban, which is the shape a
    #     future edit would have to take to break it.
    shell_tree = _module_tree(TOOLS / "ci_parity_shell.py")
    banned = {"yaml", "ci_parity_config", "ci_parity_workflow",
              "ci_parity_save_compat", "ci_expensive_gates"}
    imported: set[str] = set()
    for node in ast.walk(shell_tree):
        if isinstance(node, ast.Import):
            imported.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom):
            imported.add((node.module or "").split(".")[0])
    _expect(failures, not (imported & banned),
            f"ci_parity_shell.py imports {sorted(imported & banned)}; the "
            "lexer is a leaf and must know nothing about which repository "
            "it is reading")

    # 18. The facade holds no implementation body: no lexer, no
    #     save-compat harness, no fixture corpus, no production constant.
    facade_tree = _module_tree(FACADE)
    facade_callables = _top_level_callables(facade_tree)
    _expect(failures, facade_callables == set(FACADE_OWN_CALLABLES),
            f"ci_parity_audit.py should define exactly "
            f"{sorted(FACADE_OWN_CALLABLES)} at top level, got "
            f"{sorted(facade_callables)}")
    facade_bindings = _top_level_bindings(facade_tree)
    _expect(failures, facade_bindings == set(FACADE_OWN_BINDINGS),
            f"ci_parity_audit.py should bind exactly "
            f"{sorted(FACADE_OWN_BINDINGS)} at module level, got "
            f"{sorted(facade_bindings)}; a fixture, shim template or "
            "production constant living here is the split coming undone")

    # 19. Every re-exported name IS the canonical object, defined by
    #     exactly one owner (requirements 4 and 22). A copied wrapper
    #     would satisfy a name check and still let production and the
    #     self-test read different code.
    definitions = {path.name: _defined_names(path)
                   for path in EXTRACTED}
    for name in ci_parity_audit.__all__:
        if name in FACADE_OWN_CALLABLES:
            continue
        owners = [module for module in CANONICAL_OWNERS
                  if name in definitions.get(
                      Path(module.__file__).name, set())]
        if len(owners) != 1:
            failures.append(
                f"re-exported {name!r} is defined by "
                f"{[m.__name__ for m in owners]}, not by exactly one owner")
            continue
        _expect(failures,
                getattr(ci_parity_audit, name) is getattr(owners[0], name),
                f"ci_parity_audit.{name} is not {owners[0].__name__}.{name}; "
                "a re-export must be the canonical object, never a copy")

    # 20. Every parity module still faces the save-compat coverage whose
    #     selection it validates. `tools/ci_parity_audit.py` was already
    #     an exact pattern in ci_expensive_gates.SAVE_COMPAT_GLOBS before
    #     this split; leaving an extracted owner unmatched would silently
    #     narrow that selector, so each is checked through the real
    #     selector rather than by reading the glob list.
    for path in (FACADE, SELF_TEST_OWNER, *EXTRACTED):
        relative = f"tools/{path.name}"
        _expect(failures,
                ci_expensive_gates.selected("save-compat", [relative]),
                f"{relative} does not select the save-compat gate; add it to "
                "SAVE_COMPAT_GLOBS in tools/ci_expensive_gates.py, or an "
                "edit to it can change WHEN that coverage runs without "
                "facing the coverage itself")

    # 21. `tools/` is an implicit namespace package, so `import
    #     tools.ci_parity_shell` from the repository root leaves `tools/`
    #     itself off sys.path. A module that re-exports a sibling without
    #     inserting its own directory first works under `python3
    #     tools/...` and fails under that spelling -- invisible to any
    #     gate that only runs it the normal way.
    import subprocess

    repo_root = TOOLS.parent
    environment = {key: value for key, value in os.environ.items()
                   if key != "PYTHONPATH"}
    for path in (FACADE, SELF_TEST_OWNER, *EXTRACTED):
        module = f"tools.{path.stem}"
        proc = subprocess.run(
            [sys.executable, "-c", f"import {module}"],
            cwd=str(repo_root), env=environment,
            capture_output=True, text=True)
        _expect(failures, proc.returncode == 0,
                f"`import {module}` from the repository root failed: "
                f"{(proc.stderr or '').strip()}")

    return failures


def self_test() -> list[str]:
    """Every self-test case, as a list of failure descriptions.

    The single entry point `ci_parity_audit.main_self_test` dispatches to.
    """
    failures = _self_test()
    failures.extend(_structural_self_test())
    return failures
