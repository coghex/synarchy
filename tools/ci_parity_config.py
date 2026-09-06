#!/usr/bin/env python3
"""Single-sourced production configuration for the CI parity audit (#1355).

Every constant the audit judges this repository by lives here and only
here: the two files compared, the workflow jobs pinned, the aggregate's
dependencies, the probe job's condition and required commands, the
unit-asset gate's pinned guard, the labels the diagnostics name, and the
complete `EXEMPT_COMMANDS` inventory with each entry's reason.

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

#: The Cabal-backed worker: the build, hspec, the two save-compatibility
#: steps and world_check. Named on its own as well as in AUDITED_JOBS
#: because the save-compat wiring checks inspect THIS job specifically --
#: the commands they guard are the ones #2272 deliberately left here.
AUDITED_JOB = "test-and-audits"

#: The engine-free worker #2272 split out of it: every `python3 tools/*.py`
#: gate that needs no Cabal build product, so none of them queue behind the
#: build.
STATIC_AUDIT_JOB = "static-audits"

#: The workflow workers whose gate sets `make ci` mirrors, TOGETHER. The
#: audit keeps each job's set separate, rejects any command run by two of
#: them, and only then compares the union with tools/ci-local.sh: a set
#: union taken first would hide a gate that CI pays for twice.
AUDITED_JOBS = (AUDITED_JOB, STATIC_AUDIT_JOB)

#: The job that resolves the CI image, and the only thing STATIC_AUDIT_JOB
#: may wait on. Anything else in its `needs` puts it back behind a build.
IMAGE_JOB = "resolve-image"

#: The stable status context the drainer and branch protection consume.
AGGREGATE_JOB = "build-test"

#: The separate real-engine worker that runs alongside AUDITED_JOBS on PRs.
PROBE_JOB = "behavior-probes"

AGGREGATE_NEEDS = frozenset({AUDITED_JOB, STATIC_AUDIT_JOB, PROBE_JOB})
PROBE_JOB_IF = "github.event_name == 'pull_request'"
PROBE_REQUIRED_COMMANDS = frozenset({
    "python3 tools/ci_probes.py --stdin",
    ("python3 tools/run_probes.py --only "
     "${{ steps.probe-selection.outputs.only }} "
     "--exact --retries 1 --jobs 2"),
})

def workflow_label(job: str) -> str:
    """How a diagnostic names one workflow job."""
    return ".github/workflows/ci.yml (job: %s)" % job


WORKFLOW_LABEL = workflow_label(AUDITED_JOB)
STATIC_AUDIT_LABEL = workflow_label(STATIC_AUDIT_JOB)
#: How a diagnostic names the two-job union -- used for a command
#: tools/ci-local.sh runs and NEITHER audited job does, where naming one
#: job would point at the wrong place to add it.
WORKFLOW_UNION_LABEL = ".github/workflows/ci.yml (jobs: %s)" % ", ".join(
    AUDITED_JOBS)
LOCAL_GATE_LABEL = "tools/ci-local.sh"

#: The unit-asset gate's command, its pinned guard, and the selector
#: invocation that guard's output must come from (#2272 requirement 4).
#: The gate moved to STATIC_AUDIT_JOB with its selector; this pins that it
#: kept BOTH halves of its behavior -- path-selective on pull requests,
#: unconditional on every other event, so a path the selector does not
#: list can never leave the inventory unchecked on master. Pinned rather
#: than parsed: `if:` is a GitHub expression, and a half-understood
#: evaluator would be a worse check than an exact match.
UNIT_ASSET_GATE = "unit-assets"
UNIT_ASSET_COMMAND = "python3 tools/pack_atlas.py --validate-only --strict"
UNIT_ASSET_SELECTOR_COMMAND = (
    "python3 tools/ci_expensive_gates.py --stdin --gate " + UNIT_ASSET_GATE)
UNIT_ASSET_CI_IF = (
    "github.event_name != 'pull_request' "
    "|| steps.expensive-gates.outputs.unit-assets == 'true'")

#: Cabal build/test verbosity (#1920). At Cabal's default verbosity a cold
#: build prints the resolved plan, a Configuring/Preprocessing/Building
#: banner per component and one `[N of M] Compiling` line per module --
#: 750 library, 270 headless-test and 6 graphical-test modules of routine
#: progress that buried the tests and audits both logs exist to show.
#: `-v0` drops exactly that and nothing else: a -Werror violation still
#: arrives with its error code, warning-flag name, source excerpt and
#: caret, a non-fatal -Wall warning still prints, a failed build still
#: names its package, a failed suite still names itself, a solver failure
#: still prints its trace, and every exit status is unchanged. Whether a
#: build was cold is reported explicitly by the cache-status step (#1358).
#:
#: Requirement 6 of #1920 is that neither entry point is quiet while the
#: other is verbose, and nothing else enforces it: `cabal` steps are
#: outside the gate-set comparison above by that section's documented
#: scope, so a re-verbosed command there would compare equal to nothing
#: at all. Only `build` and `test` are covered -- `cabal update`,
#: `cabal --version` and `cabal sdist` emit no compilation progress and
#: are left alone -- and only in the Cabal-owning audited job and the
#: local gate: the `behavior-probes` job is out of scope for #1920 and is
#: deliberately NOT audited here.
#:
#: Both spellings Cabal accepts for verbosity 0 are allowed, so a long-form
#: edit reads as compliant rather than as drift; nothing else counts, and a
#: `-v1` restored on one side alone is the regression this pins.
CABAL_QUIET_FLAGS = ("-v0", "--verbose=0")
CABAL_QUIET_SUBCOMMANDS = frozenset({"build", "test"})
CABAL_QUIET_JOB = AUDITED_JOB

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
