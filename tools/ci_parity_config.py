#!/usr/bin/env python3
"""Single-sourced production configuration for the CI parity audit (#1355).

Every constant the audit judges this repository by lives here and only
here: the two files compared, the workflow jobs pinned, the aggregate's
dependencies, the probe job's condition and required commands, the
labels the diagnostics name, and the complete `EXEMPT_COMMANDS`
inventory with each entry's reason.

Why a module of its own: `tools/ci_parity_workflow.py` consumes all of
it, `tools/ci_parity_audit.py` consumes it too, and no extracted owner
may import the facade. A leaf both can import is what keeps the values
single-sourced without an import cycle. The self-test reads them from
HERE rather than through a facade alias, so mutating one reaches the
production code that uses it.

Deliberately NOT imported by `tools/ci_parity_shell.py`: the lexer is a
leaf that knows nothing about which repository it is reading.

This module is a library: it has no command line of its own.
"""
from __future__ import annotations

from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
WORKFLOW_PATH = REPO_ROOT / ".github" / "workflows" / "ci.yml"
LOCAL_GATE_PATH = REPO_ROOT / "tools" / "ci-local.sh"

#: The workflow worker whose gate set `make ci` mirrors.
AUDITED_JOB = "test-and-audits"

#: The stable status context the drainer and branch protection consume.
AGGREGATE_JOB = "build-test"

#: The separate real-engine worker that runs alongside AUDITED_JOB on PRs.
PROBE_JOB = "behavior-probes"

AGGREGATE_NEEDS = frozenset({AUDITED_JOB, PROBE_JOB})
PROBE_JOB_IF = "github.event_name == 'pull_request'"
PROBE_REQUIRED_COMMANDS = frozenset({
    "python3 tools/ci_probes.py --stdin",
    ("python3 tools/run_probes.py --only "
     "${{ steps.probe-selection.outputs.only }} "
     "--exact --retries 1 --jobs 2"),
})

WORKFLOW_LABEL = ".github/workflows/ci.yml (job: %s)" % AUDITED_JOB
LOCAL_GATE_LABEL = "tools/ci-local.sh"

# Invocations one side deliberately runs and the other does not, each
# keyed on the EXACT command so a neighbouring form is not exempted with
# it. Most run only in CI; the exemption is direction-agnostic, and the
# one local-only entry is marked as such. `--self-test` proves each entry
# is accepted, and the repository run proves each is still live: an entry
# matching nothing on either side is a stale exemption and fails, so this
# list cannot outlive its reason.
EXEMPT_COMMANDS: tuple[tuple[str, str], ...] = (
    (
        "python3 tools/ci_expensive_gates.py --stdin --gate worldgen",
        "CI path-selection orchestration: reads a changed-file list and "
        "prints whether CI needs the worldgen gate for that change. "
        "`make ci` runs world_check unconditionally, so there is nothing "
        "local for the selector to decide. Its --self-test form is NOT "
        "exempt and does run locally.",
    ),
    (
        "python3 tools/ci_expensive_gates.py --stdin --gate graphical",
        "CI path-selection orchestration, as above: `make ci` builds the "
        "graphical test suite unconditionally.",
    ),
    (
        "python3 tools/ci_expensive_gates.py --stdin --gate unit-assets",
        "CI path-selection orchestration, as above: `make ci` runs the "
        "unit-asset gate unconditionally.",
    ),
    (
        "python3 tools/ci_docs_fast_path.py --stdin --explain",
        "CI path-selection orchestration for the docs-only fast path "
        "(#1490): reads one push's changed-path range and prints whether "
        "CI may skip the Haskell build. `make ci` builds and tests "
        "unconditionally against a working tree, not a push range, so "
        "there is nothing local for the selector to decide. Its "
        "--self-test form is NOT exempt and does run locally.",
    ),
    (
        "python3 tools/ci_expensive_gates.py --local-changed-paths",
        "LOCAL-only, and the one entry that runs here rather than in CI "
        "(#1360). It resolves the changed-path list `make ci` judges "
        "itself by -- paths differing from the merge base with the "
        "default branch, tracked working-tree edits included -- because "
        "`make ci` has no pull-request base sha to be handed. CI already "
        "has one and pipes it in directly. The DECISION both files then "
        "reach is NOT exempt: both run "
        "`ci_expensive_gates.py --stdin --gate save-compat`, and "
        "audit_save_compat_reproducibility_wiring over in "
        "tools/ci_parity_save_compat.py checks they feed "
        "it into the same guarded command.",
    ),
    (
        "python3 tools/ci_cache_report.py",
        "CI cache-outcome reporting (#1358): classifies what the two "
        "actions/cache restore steps got from the outputs those steps "
        "published into the runner's environment. `make ci` restores no "
        "GitHub Actions cache, so there is no outcome for it to classify "
        "-- and this reports rather than gates, so it fails nothing "
        "either way. Its --self-test form is NOT exempt and does run "
        "locally, which is what keeps the classification and its ci.yml "
        "wiring honest from `make ci`.",
    ),
    (
        ("python3 tools/ci_cache_epoch.py --ref $CACHE_REF "
         "--github-output $GITHUB_OUTPUT "
         "--github-summary $GITHUB_STEP_SUMMARY"),
        "CI cache-key orchestration: derives a pull request's immutable "
        "project-cache epoch from its base SHA and a master push's epoch "
        "from its pushed SHA, then publishes runner outputs. `make ci` "
        "does not restore or save GitHub Actions caches, so it has no key "
        "to select. The tool's --self-test form is NOT exempt and runs on "
        "both sides.",
    ),
)
