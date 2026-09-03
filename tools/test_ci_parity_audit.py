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
    EXEMPT_COMMANDS,
    LOCAL_GATE_LABEL,
    WORKFLOW_LABEL,
)
from ci_parity_save_compat import (  # noqa: E402
    LOCAL_BLOCK_BEGIN,
    LOCAL_BLOCK_END,
    audit_save_compat_reproducibility_wiring,
)
from ci_parity_shell import AuditError, extract_invocations  # noqa: E402
from ci_parity_workflow import (  # noqa: E402
    audit_gate_sets,
    audit_parallel_gate_wiring,
    local_gate_invocations,
    workflow_invocations,
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

    # 2. Parity passes with nothing exempted.
    _expect(failures, audit_gate_sets(ci, local, ()) == [],
            "matched gate sets should report no problems, got "
            f"{audit_gate_sets(ci, local, ())}")

    # 3. A non-exempt CI-only invocation fails and names the command.
    dropped = local - {"python3 tools/block_two.py --flag value"}
    problems = audit_gate_sets(ci, dropped, ())
    _expect(failures,
            any("python3 tools/block_two.py --flag value" in p
                and "not by" in p and LOCAL_GATE_LABEL in p for p in problems),
            "a CI-only invocation should be reported as missing locally, got "
            f"{problems}")

    # 4. A non-exempt local-only invocation fails and names the command.
    added = local | {"python3 tools/local_extra.py"}
    problems = audit_gate_sets(ci, added, ())
    _expect(failures,
            any("python3 tools/local_extra.py" in p and WORKFLOW_LABEL in p
                for p in problems),
            "a local-only invocation should be reported as missing in CI, got "
            f"{problems}")

    # 5. Changing an invocation's ARGUMENTS is drift in both directions.
    retuned = (local - {"python3 tools/block_two.py --flag value"}) | {
        "python3 tools/block_two.py --flag other"}
    problems = audit_gate_sets(ci, retuned, ())
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
    problems = audit_gate_sets(exempt_side, shared)
    _expect(failures, problems == [],
            f"the real exemptions should be accepted, got {problems}")

    # 6b. …and each is load-bearing: without the list, each one is drift.
    problems = audit_gate_sets(exempt_side, shared, ())
    for command, _reason in EXEMPT_COMMANDS:
        _expect(failures, any(command in p for p in problems),
                f"exemption {command!r} is not load-bearing: it was not "
                "reported as drift with the list emptied")
    # 7. A stale exemption fails, naming what nothing runs.
    problems = audit_gate_sets(shared, shared)
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
            audit_gate_sets(shared | {"python3 tools/x.py"}, shared,
                            (("python3 tools/x.py", "   "),)) != [],
            "a blank exemption reason should be reported")

    # 9. Vacuity on either side is a failure, not a pass. Checked with BOTH
    #    sides empty, so drift cannot report the failure on vacuity's
    #    behalf and hide the fact that nothing checks for it.
    problems = audit_gate_sets(set(), set(), ())
    _expect(failures,
            any("empty gate set" in p and WORKFLOW_LABEL in p
                for p in problems),
            f"an empty CI gate set should fail on its own, got {problems}")
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

    # 14. The stable build-test context must aggregate both parallel workers.
    failures.extend(_parallel_gate_wiring_self_test())

    return failures


_PARALLEL_GATE_WORKFLOW_GOOD = """\
name: fixture
on: [push, pull_request]
jobs:
  test-and-audits:
    runs-on: ubuntu-latest
    steps:
      - run: python3 tools/audit.py
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
    needs: [test-and-audits, behavior-probes]
    runs-on: ubuntu-latest
    steps:
      - env:
          EVENT_NAME: ${{ github.event_name }}
          TESTS_RESULT: ${{ needs.test-and-audits.result }}
          PROBES_RESULT: ${{ needs.behavior-probes.result }}
        run: |
          test "$TESTS_RESULT" = success
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
             "needs: [test-and-audits, behavior-probes]",
             "needs: [test-and-audits]"), "must need exactly"),
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
