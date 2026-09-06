#!/usr/bin/env python3
"""`/deflake` diagnosis: the mechanics of turning a measurement into a fix (#1437).

`tools/deflake.py` (#1436) answers "how often does this probe fail".
That is a NUMBER, and a number is not a fix. This module owns the step
after it: given one complete measurement handoff, decide mechanically
whether the evidence supports a probe-side repair — and, when it does
not, which declared non-repair route the invocation must take instead.

    python3 tools/deflake_diagnosis.py --diagnosis <document.json>
    python3 tools/deflake_diagnosis.py --manifest <checkout-root>
    python3 tools/test_deflake_diagnosis.py       # the deterministic gate

Nothing here boots an engine, runs a probe, opens a port, edits a file
in a worktree, or talks to GitHub. It reads documents and answers
questions about them. The expensive half — twenty real probe runs across
two worktrees — is performed by `tools/probe_flake.py` exactly as
`/deflake` already performs ten, and its retained
`probe-flake-result/v1` documents are this module's input.

Why a tracked module rather than skill prose
--------------------------------------------
The `/deflake` workflow surface is not tracked in this repository
(`.gitignore` ignores `.claude/`), so a repository test cannot cover
prose. The rules below are the MECHANICAL half — the ones a test can
hold to — and they live here so `tools/test_deflake_diagnosis.py` can
assert them:

* the entry gate over a #1436 handoff, including one-probe enforcement;
* the closed producer-provenance contract (#1661) — the launcher
  grammars, artifact topology and retention, identity delegation, and
  the measurement-apparatus repair scope;
* X-out-of-10 arithmetic, taken from `probe_census` rather than restated;
* the configuration manifest, including CONFIRMED ABSENCE;
* MISSING evaluation and stable check identity;
* same-environment verification acceptance and the no-retry rule;
* the required diagnosis evidence and preservation attestations; and
* the one-PR limit.

The JUDGEMENT half stays with the agent running the workflow: whether a
diagnosis is convincing, whether a repair is minimal, whether an
expectation is genuinely obsolete. Those are reviewer questions and this
module does not pretend to answer them. What it does instead is refuse a
route whose machine-checkable evidence is absent — a repair with no
stated cause, no evidence, or no preservation attestation cannot be
declared at all.

The entry gate
--------------
One invocation consumes exactly ONE handoff naming exactly ONE probe.
`deflake-handoff/v1` is deliberately thin: it carries the probe key, X,
the pointers to the retained artifacts, and the invocation and
configuration records — and it EMBEDS or REFERENCES the original
`probe-flake-result/v1` document rather than paraphrasing it. That is
not tidiness. `probe_census.ingest_result` deliberately drops ports,
per-run check maps, descriptor labels, the artifact root, the invocation
directory and the exact command, so a handoff reconstructed from the
durable census row alone cannot identify the baseline invocation, and
this gate refuses it.

Descriptor equality, not identifier normalization
-------------------------------------------------
The descriptor is compared WHOLE — identifiers, their order, and their
LABELS. A label is the check's stated meaning, so a batch that kept
every identifier while relabelling one to describe a different assertion
has changed what it measures and said so nowhere.

`probe-result/v1` already requires STATIC descriptor identifiers and
puts every runtime value in an event's `detail`
(`tools/probe_protocol.py`). So a per-run undeclared or changing
identifier is a MALFORMED protocol result — a rejected handoff — and
never a diagnosis outcome to be scored. This module compares
descriptors for exact equality (identifiers and order) and refuses
anything else, rather than inventing the normalization pass that is
already this issue's declared non-goal.

The scoped MISSING rule
-----------------------
Probes abort. A run that FAILs check three and stops cannot, by
construction, emit checks four onward, and `probe_protocol` records
those as MISSING. Demanding "every expected check in every run" would
therefore be unsatisfiable for any probe with an X above zero, so the
rule is scoped:

* every TARGET check identifier has ZERO MISSING across all ten runs;
* every PASSING run emits every expected check, MISSING zero;
* an accepted failing or timed-out run may omit only checks after its
  own abort point — and because `probe_protocol` enforces declared
  ORDER, "after the abort point" is exactly "a contiguous suffix of the
  declared order", which is checked rather than assumed; and
* no expected identifier disappears from the batch as a whole.

The suffix allowance is for the checks that are NOT targets: an accepted
failing run may abort, but it may not abort BEFORE a target, because a
run that never reached the target did not demonstrate the target was
fixed. Since the targets are every non-PASS identifier and an aborting
probe's are a suffix of the descriptor, that means a verification's
accepted failing runs must not abort before the end — restrictive for an
X above zero, but satisfiable (a run that FAILs its last check, or
reports a failed check and keeps going, emits everything) and exactly
what the contract says. See `missing_problems`.

Same environment means the same MEASUREMENT, not the same characters
--------------------------------------------------------------------
The conditions travel the WHOLE chain — handoff, then controlled
baseline, then verification — and each link is compared to the one
before it. Comparing only the last pair would let BOTH controlled
batches agree on some arbitrary timeout while the handoff sat at the
defaults, and an agreement between two batches is not the measurement
the handoff was taken under. The first link crosses LAUNCHERS, which is
the point: `/deflake` supplies its run count and capability count from
its own constants while the harness reads them from a command line, and
that is where the two accounts of one contract are made to agree.

The baseline and the verification batch necessarily differ in the
worktree they run from and in where they write, and `probe_flake` leases
ports dynamically. So "the same invocation" is compared on
BEHAVIOR-AFFECTING settings with effective defaults filled in — probe,
run count, RTS capabilities, retry policy — while `--result`,
`--artifact-root` and the observed ports are recorded and permitted to
differ.

A command is first checked against the REAL interface of the tool that
ran it — and there are TWO tools, because the three batches do not come
from one command. `/deflake` (#1436) does not shell out: it calls
`probe_flake.measure` IN PROCESS, and its CLI has no `--probe`, no
`--runs` and no RTS override at all. So the handoff's command is
`python3 tools/deflake.py`, whose measurement contract comes from
`/deflake`'s own constants and whose probe comes from the result document
its selector produced; the two CONTROLLED batches are the ones the issue
spells as `probe_flake.py --probe … --runs 10`. Requiring a
`probe_flake.py` argv everywhere would have made a truthful #1436 handoff
impossible to submit while accepting an argv nobody ran.

Within a tool's own surface, every option must be one it actually accepts
and be spelled the way its argparse would read it — `--runs` and
`--rts-caps` are `type=int`, so `--runs 10.0` is refused here exactly as
the harness would have refused it rather than compared as the number ten,
and both carry the producer's own POSITIVE constraint on top of that
grammar, since `int()` accepts `0` while `measure` refuses it before
opening a port.
`--result` is required of `probe_flake.py`, because the document is
written only `if args.result` and a command without one produced no
evidence at all; it is optional for `/deflake`, which retains the
document beside its artifacts either way. ORDER is part of the grammar
too: argv[0] is the interpreter, argv[1] the script, and only argv[2:]
are options, because Python rejects an option it does not know before the
script runs. And the argv must be a PYTHON 3 INTERPRETER — `python3`, or a
version-qualified `python3.<minor>[.<patch>]` at or above the 3.10
syntax floor these tools' runtime-evaluated `X | None` annotations
impose; not bare `python`, which is whichever of the two that machine
means, not `python2`, which cannot parse these programs at all, and not
`python3.9`, which names a version that could not have run the program
whose document quotes it — named rather than given by path, since a
document cannot show which binary sits at `/tmp/counterfeit/python3` —
running one of those two scripts. The script is
RESOLVED FROM THE DIRECTORY THE COMMAND RAN IN — which is what Python
does with a relative path — and must then be the tool the DECLARED
checkout ships: `<worktree>/tools/probe_flake.py` for a controlled
batch, `<directory>/tools/deflake.py` for the handoff. Matching only the
file name would admit `/tmp/counterfeit/probe_flake.py`, a different
program spelled the same way; resolving against the checkout instead let
an invocation in a SUBDIRECTORY write `tools/probe_flake.py`, mean a
counterfeit nested beside it, and be compared to the real one. And the
handoff's directory
must be the PRIMARY checkout rather than any path calling itself one —
`/deflake` runs there, before this workflow's comparison worktrees
exist,
because an option the CLI does not have — `--timeout`, say — would
compare equal across two batches while describing a measurement neither
could have run, and `/bin/echo .../probe_flake.py --probe role --runs 10`
has the right shape and measures nothing.

Each command is then bound to ITS OWN result document: the probe, run
count and capability count it passed must be the ones that document
reports. Comparing the two invocations to each other is not enough —
two commands that agreed on another probe at twenty runs would compare
equal while their documents both claimed the handoff's contract. What
the agreed value has to BE stays where it already lives, so one fact is
not scored twice: the probe against the handoff, and the run and
capability counts in `controlled_problems`, which routes them.

One sweep covers all three batches — the handoff included, since a
handoff whose result tree was rewritten under a comparison worktree is
exactly as unusable as a verification that wrote into one, and its own
extra `artifacts` are swept with it.

Destinations must sit outside every worktree, registered or DECLARED:
the artifact root is guarded by `probe_flake.check_artifact_root`, but
`--result` is written wherever it is pointed — and it is opened relative
to the process's directory, so a destination is joined onto the recorded
invocation directory and normalised before anyone asks which worktree it
lands in. Where a batch SAYS it wrote is bound by the same rule — its own
`artifact_root`, `invocation_dir` and retained artifact paths — and its
declared `--artifact-root` must agree with the root its result document
reports, or neither record constrains the other.

Two labels are not two worktrees, either: the declared paths are
canonicalised, neither may contain the other, and each invocation must
have run inside the worktree its section names. Deliberately NOT "must
be a registered `git worktree`" — the workflow is required to remove or
hand off both comparison worktrees when it finishes, so by the time its
document is evaluated the paths it correctly names may be gone. BOTH
declarations are collected before either batch is validated, so every
path is checked against both comparison states; after cleanup neither
is registered anywhere, and a batch checked only against its own
declaration could name an artifact root inside the other one.

Both worktrees are also attested SOURCE-CLEAN at measurement time. The
recorded SHA cannot reveal an uncommitted change, and "the clean
comparison worktree must remain unmodified" is a contract about its
source — the gitignored configuration state it must ALSO reproduce is
recorded separately in its own manifest, whose entries must be members
of the `config/*.local.yaml` family and nothing else. Two manifests that
agree perfectly about `../outside.local.yaml` establish nothing about
the state the probes actually read.

The artifact layout is the one the harness creates
--------------------------------------------------
Every path is ABSOLUTE and fully RESOLVED, because
`check_artifact_root` calls `Path.resolve` on its root before a run
begins: no `.`, no `..`, no doubled separator, no trailing slash, and no
unresolved symlink. A lexical check alone would have accepted
`/tmp/evidence/forged/../artifacts/…` as the artifact root, and on a
host where `/tmp` is a link to `/private/tmp` it would call two
different places the same one.

`new_invocation_dir` puts the invocation directory DIRECTLY under that
root and GENERATES its name — `{probe}-{%Y%m%dT%H%M%SZ}-{pid}-{uuid8}` —
from a real clock and a real process, both of which are checked for
MEANING and not only shape: eight digits, `T`, six digits and `Z` also
matches `99999999T999999Z`, and a bare digit run also matches a pid of
0, and neither was ever produced. The name is split from the RIGHT, so
the three generated fields come off the end and the probe segment is
left whole — then required to EQUAL the document's own probe, which a
left-to-right split would misattribute the day a hyphenated probe key
were registered. Every run
directory is
`invocation_dir / f"run-{index:03d}"` — so three recorded values
determine the whole layout and nothing in it is free. Checking only "is
this inside a worktree" left a batch able to swap a failed run's
directory for an unrelated external path and keep `repair-pr`, though no
harness run could produce that. Pinning the exact path also makes the run
directories unique by construction.

The containment sweep over the paths a result NAMES stays, and
`--artifact-root` being OPTIONAL is where it earns its keep: with the
option supplied, every path derives from a root the agreement rule has
tied to an already-checked destination; omitted — which is legitimate,
since `default_artifact_root` supplies a temporary directory — nothing
else constrains the root the document reports.

Artifacts are paired with outcomes, and handed on
-------------------------------------------------
`probe_flake.measure` deletes a run's directory the moment it passes and
retains every unsuccessful one, so `artifact_dir` pairs with the run's
outcome exactly and `retained_artifacts` is literally the list of the
non-null ones. Both directions are checked. "No successful-run raw
artifacts remain" is one of verification's own success conditions, and a
FAILING run whose directory has gone is a failure nobody can diagnose —
which is precisely the evidence #1438 and #1439 are handed.

That evidence then has to reach them: an emitted outcome names the
retained artifacts of EVERY batch this invocation ran, not just the
handoff's. The batch that went wrong is usually the verification, whose
logs an outcome built from the handoff alone would never mention.

An invalid batch is a ROUTE, not a rejection
--------------------------------------------
"Verification remains above X, contains any MISSING result, becomes
invalid, or only partially improves the rate" is ONE list in the issue
and every entry on it goes to #1439. So a harness error, a short batch or
a contended machine in the VERIFICATION reaches `partial-improvement`
with its evidence retained, and the same in the BASELINE reaches
`cannot-reproduce` — never a gate rejection, which would report an
invocation that got nowhere and lose the artifacts it did keep. What
stays a rejection is a document that is not a `probe-flake-result/v1` at
all, and a descriptor whose identities changed.

The repair is frozen before it is verified
------------------------------------------
`probe_flake` records `git rev-parse HEAD` and cannot see uncommitted
source changes, so a verification batch run against a dirty worktree
measures something no commit contains. A declared repair therefore
requires a source-clean repair worktree and a verification result whose
`commit_sha` equals the repair commit being proposed — and a `base_sha`
equal to the handoff's own baseline commit, since the clean comparison
worktree and the repair worktree are cut from one common SHA.

Weakening an assertion is never a fix
-------------------------------------
The shapes a machine can see are refused here: a descriptor that lost,
renamed or RELABELLED an identifier, a target check that became MISSING,
a run count below the policy, a retry policy that lets any passing
attempt count, a repair declared without the three preservation
attestations, a repair that edits the measurement APPARATUS rather than
the probe under diagnosis (`HARNESS_MODULES`) — `measure`'s timeout and
starting port are module constants, so lengthening one would buy a
calmer verification while both command records still compared equal, and
the two batches would have been run by different harnesses — and a
`changed_paths` entry that reaches production code through a traversal:
`tools/../src/Engine/Core/Init.hs` begins with `tools/`, so the scope
check is applied to the NORMALISED repository-relative path or it checks
nothing.
Whether a surviving assertion was quietly BROADENED is a reviewer
judgement this module cannot make, and it says so rather than implying
coverage it does not have.

Routes
------
Exactly one per invocation, and only the first opens a pull request:

* `repair-pr` — a confidently diagnosed, successfully verified
  probe-side repair. One PR, one probe, one root cause.
* `handoff-rejected` — the entry gate refused. No code change, no PR.
* `no-target` — a schema-valid handoff whose measurement went all-PASS.
  `/deflake` writes one, so it is a legitimate input with nothing to
  diagnose rather than a malformed document; it runs no batch at all.
  Handed off to #1439.
* `cannot-reproduce` — the controlled baseline reproduced neither an
  over-X result nor a MISSING target, or its configuration could not be
  recreated from the handoff's manifest, or another process held the
  probe's declared resources. Handed off to #1439. Those are three
  different findings and the emitted `reason` says which: only the
  first is a statement about the PROBE, and #1439 recommends a de-list
  from that one alone.
* `production-defect` — the assertion is right and the product is
  wrong. The probe is not touched; handed off to #1438.
* `no-confident-fix` — several failures with no one established
  probe-side cause. Handed off to #1439.
* `partial-improvement` — the repaired batch improved but stayed above
  X, violated the scoped MISSING rule, was not comparable to the
  baseline, or never became a controlled measurement. Handed off to
  #1439, with the `reason` naming which — the last two are facts about
  the INVOCATION rather than about either result document, so a
  consumer handed only the documents could not derive them.

Emitting a handoff here means emitting `deflake-diagnosis-outcome/v1`:
the route, the machine-readable REASON it was taken for, the owning
issue, the identity of the `/deflake` invocation consumed, the probe and
targets, the baseline SHA and X, the
configuration manifest, references to the controlled results, the
diagnosis evidence, the preservation attestations, and the repair and
verification evidence when the route has them. #1437 owns that PRODUCER
record; what #1438 and #1439 DO with it is theirs to define, this module
does not invent their contracts, and filing an issue is not its job.
`tools/deflake_outcome.py` is #1439's consumer: it takes this record,
re-checks the evidence for itself, and appends one stable non-success
outcome to the probe's census row — or returns an actionable
non-success. It opens no pull request either.

The handoff this module CONSUMES is #1659's, in #1659's own spelling:
`invocation.argv`/`cwd`/`timeout` and a bare `configuration` list — and
`argv` is `sys.argv`, whose [0] is the SCRIPT, because `deflake.main`
passes `list(sys.argv)` and Python never puts the interpreter there.
`require_producer_invocation` and `require_producer_configuration` adapt
those at the boundary, so everything downstream reads one vocabulary
while the producer stays the authority on what it writes.

Where each rule now lives
-------------------------
Since #2041 the DOCUMENT contract is `tools/deflake_contract.py`: the
schemas, the route and reason vocabulary, `HandoffError` and
`RouteRefused`, the configuration manifest, the launcher grammar and
invocation records, the path and worktree-containment rules, the result
document, descriptor, controlled-result and MISSING rules, and
`Handoff`/`require_handoff` itself. It is a library — no CLI, no route,
no evidence requirement — and it imports neither this module nor any
consumer of it.

This file keeps the DIAGNOSIS: `HARNESS_MODULES`, `CAUSE_CATEGORIES`
and `ATTESTATIONS`, the `EXIT_*` table, `Outcome` and `Diagnosis`, the
declared-worktree helpers, the resource-hold rule, the two-batch
topology, `evaluate`, the evidence, preservation and repair
requirements, and the CLI above.

It also stays the COMPATIBILITY façade. The block after the imports
BINDS the contract's own objects rather than copying them, so
`deflake_diagnosis.HandoffError is deflake_contract.HandoffError` and
every existing `deflake_diagnosis.<name>` call site keeps resolving to
the one definition there has ever been.

    python3 tools/deflake_diagnosis.py --diagnosis <document.json>
    python3 tools/test_deflake_diagnosis.py       # the deterministic gate
"""
from __future__ import annotations

import argparse
import importlib
import json
import os
import sys
from pathlib import Path

# `tools/` carries no `__init__.py`, so it is an implicit namespace
# package and every module in it has TWO import spellings: `tools.<name>`
# from the repository root, and the bare one a caller who put `tools/`
# on `sys.path` uses. Python treats those as DIFFERENT modules, so a
# façade loaded as `tools.deflake_diagnosis` that resolved its contract
# by bare name would bind a SECOND copy of it — and then
# `except tools.deflake_diagnosis.HandoffError` would stop catching what
# `tools.deflake_contract` raises, which is the one guarantee the
# re-export block below exists to make. The contract is therefore
# resolved under the spelling that loaded THIS module; the path
# insertion remains for the bare spelling and for running this file
# directly as a script.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_flake  # noqa: E402
import probe_engine  # noqa: E402
import probe_runner_resources  # noqa: E402


def _sibling(name: str):
    """One `tools/` module, under the spelling that loaded this one."""
    return importlib.import_module(
        f"{__package__}.{name}" if __package__ else name)


_contract = _sibling("deflake_contract")

# ==========================================================================
# The document contract, re-exported
# ==========================================================================
# Every name below is the contract module's OWN object, BOUND rather
# than copied, so `deflake_diagnosis.X is deflake_contract.X` holds for
# all of them and a caller that has not migrated yet reads the one
# definition there has ever been. A redefinition here would be a second
# definition free to drift — and for the two exceptions it would
# silently stop catching what the contract raises.

# The interchange schemas and the measurement constants they carry.
HANDOFF_SCHEMA = _contract.HANDOFF_SCHEMA
DIAGNOSIS_SCHEMA = _contract.DIAGNOSIS_SCHEMA
MANIFEST_SCHEMA = _contract.MANIFEST_SCHEMA
OUTCOME_SCHEMA = _contract.OUTCOME_SCHEMA
RUN_COUNT = _contract.RUN_COUNT
RTS_CAPABILITIES = _contract.RTS_CAPABILITIES
CONFIG_GLOB = _contract.CONFIG_GLOB
SHA256_RE = _contract.SHA256_RE

# Identity, the interpreter grammar and the launcher definitions.
require_commit = _contract.require_commit
INTERPRETER_MINOR_FLOOR = _contract.INTERPRETER_MINOR_FLOOR
INTERPRETER_RE = _contract.INTERPRETER_RE
interpreter_problem = _contract.interpreter_problem
Launcher = _contract.Launcher
DESTINATION_OPTIONS = _contract.DESTINATION_OPTIONS
REQUIRED_OPTIONS = _contract.REQUIRED_OPTIONS
INVOCATION_DEFAULTS = _contract.INVOCATION_DEFAULTS
CONDITION_OPTIONS = _contract.CONDITION_OPTIONS
POSITIVE_OPTIONS = _contract.POSITIVE_OPTIONS
HARNESS_OPTIONS = _contract.HARNESS_OPTIONS
HARNESS_LAUNCHER = _contract.HARNESS_LAUNCHER
DEFLAKE_LAUNCHER = _contract.DEFLAKE_LAUNCHER
LAUNCHERS = _contract.LAUNCHERS

# The route and reason vocabulary, and their compatibility table.
ROUTE_REPAIR = _contract.ROUTE_REPAIR
ROUTE_HANDOFF_REJECTED = _contract.ROUTE_HANDOFF_REJECTED
ROUTE_CANNOT_REPRODUCE = _contract.ROUTE_CANNOT_REPRODUCE
ROUTE_PRODUCTION_DEFECT = _contract.ROUTE_PRODUCTION_DEFECT
ROUTE_NO_CONFIDENT_FIX = _contract.ROUTE_NO_CONFIDENT_FIX
ROUTE_PARTIAL_IMPROVEMENT = _contract.ROUTE_PARTIAL_IMPROVEMENT
ROUTE_NO_TARGET = _contract.ROUTE_NO_TARGET
ROUTES = _contract.ROUTES
ROUTE_OWNER = _contract.ROUTE_OWNER
ROUTES_THAT_CHANGE_CODE = _contract.ROUTES_THAT_CHANGE_CODE
REASON_NO_NON_PASS_CHECK = _contract.REASON_NO_NON_PASS_CHECK
REASON_CONFIGURATION_NOT_RECREATED = (
    _contract.REASON_CONFIGURATION_NOT_RECREATED)
REASON_BASELINE_NOT_CONTROLLED = _contract.REASON_BASELINE_NOT_CONTROLLED
REASON_BASELINE_OBSERVED_NOTHING = _contract.REASON_BASELINE_OBSERVED_NOTHING
REASON_DIAGNOSIS_DECLARED = _contract.REASON_DIAGNOSIS_DECLARED
REASON_VERIFICATION_NOT_CONTROLLED = (
    _contract.REASON_VERIFICATION_NOT_CONTROLLED)
REASON_VERIFICATION_NOT_COMPARABLE = (
    _contract.REASON_VERIFICATION_NOT_COMPARABLE)
REASON_VERIFICATION_MISSING_RULE = _contract.REASON_VERIFICATION_MISSING_RULE
REASON_VERIFICATION_OVER_TOLERANCE = (
    _contract.REASON_VERIFICATION_OVER_TOLERANCE)
REASON_VERIFICATION_ACCEPTED = _contract.REASON_VERIFICATION_ACCEPTED
REASONS = _contract.REASONS
ROUTE_REASONS = _contract.ROUTE_REASONS
CONTROLLED_REASONS = _contract.CONTROLLED_REASONS

# The two exception classifications. Bound, never subclassed: an `except`
# clause here has to catch what the contract itself raises.
HandoffError = _contract.HandoffError
RouteRefused = _contract.RouteRefused

# The configuration manifest.
config_manifest = _contract.config_manifest
require_manifest = _contract.require_manifest
manifest_entries = _contract.manifest_entries
manifest_differences = _contract.manifest_differences

# Invocation records, effective settings and the path rules.
TOOLS_DIR = _contract.TOOLS_DIR
INVOCATION_GENERATED_FIELDS = _contract.INVOCATION_GENERATED_FIELDS
INVOCATION_STAMP_RE = _contract.INVOCATION_STAMP_RE
INVOCATION_PID_RE = _contract.INVOCATION_PID_RE
INVOCATION_UNIQUE_RE = _contract.INVOCATION_UNIQUE_RE
INVOCATION_STAMP_FORMAT = _contract.INVOCATION_STAMP_FORMAT
invocation_name_problem = _contract.invocation_name_problem
parse_command = _contract.parse_command
require_invocation = _contract.require_invocation
require_producer_invocation = _contract.require_producer_invocation
require_producer_configuration = _contract.require_producer_configuration
effective_settings = _contract.effective_settings
invocation_differences = _contract.invocation_differences
destinations = _contract.destinations
worktree_paths = _contract.worktree_paths
primary_checkout = _contract.primary_checkout
require_path = _contract.require_path
inside_any_worktree = _contract.inside_any_worktree
_path_forms = _contract._path_forms

# Result documents, descriptors and the MISSING rules.
require_result = _contract.require_result
descriptor_ids = _contract.descriptor_ids
controlled_problems = _contract.controlled_problems
descriptor_of = _contract.descriptor_of
require_descriptor = _contract.require_descriptor
require_controlled = _contract.require_controlled
non_pass_ids = _contract.non_pass_ids
missing_targets = _contract.missing_targets
failure_count = _contract.failure_count
missing_problems = _contract.missing_problems

# The handoff itself, and the artifact topology it is held to.
Handoff = _contract.Handoff
require_handoff = _contract.require_handoff
require_topology = _contract.require_topology
result_paths = _contract.result_paths
_check_paths = _contract._check_paths


# The measurement apparatus: the modules that DECIDE what a run is, as
# opposed to the probe whose behavior is being diagnosed. A repair may
# not touch them.
#
# It is not a style rule. `probe_flake.measure`'s timeout and starting
# port are module constants that neither CLI exposes, so a repair that
# lengthened `DEFAULT_TIMEOUT` would produce a calmer verification while
# both command records still compared equal — the baseline and the
# verification would have been run by two different harnesses. The two
# comparison worktrees share one base commit and the clean one is
# attested source-clean, so a repair changing these files is the only
# way they can differ at all.
HARNESS_MODULES = (
    "tools/probe_flake.py",
    "tools/probe_protocol.py",
    "tools/probe_census.py",
    "tools/probe_census_contract.py",
    "tools/probe_census_records.py",
    "tools/probe_census_summary.py",
    "tools/probe_census_storage.py",
    "tools/probe_claim.py",
    "tools/probe_claim_storage.py",
    "tools/probe_claim_lease.py",
    "tools/probe_claim_orchestration.py",
    "tools/probe_resource_lock.py",
    "tools/probe_select.py",
    "tools/probe_engine.py",
    "tools/probelib.py",
    "tools/run_probes.py",
    "tools/probe_runner_registry.py",
    "tools/probe_runner_diagnostics.py",
    "tools/probe_runner_resources.py",
    "tools/probe_runner_lifecycle.py",
    "tools/probe_runner_scheduler.py",
    "tools/deflake.py",
    "tools/deflake_diagnosis.py",
    "tools/deflake_contract.py",
)

# The probe-side defect categories of the issue's diagnosis boundary.
# A declared repair names exactly one: "several independent causes" is
# the `no-confident-fix` route, not a list here.
CAUSE_CATEGORIES = (
    "fixture-construction",
    "setup-precondition",
    "competing-actor",
    "observation",
    "oracle-wiring",
    "check-identity",
    "fixture-discovery",
    "obsolete-expectation",
)

# The three preservation claims a repair must make explicitly. They are
# ATTESTATIONS, not proofs: the machine checks the shapes it can see
# (identifiers, MISSING, run counts, retries) and requires the agent to
# state the rest on the record, so a reviewer knows what was claimed.
ATTESTATIONS = (
    "product_behavior_preserved",
    "check_identities_preserved",
    "no_assertion_weakened",
)

EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_REFUSED = 3


# ==========================================================================
# The diagnosis
# ==========================================================================
class Outcome:
    """One diagnosis invocation's route and everything it must report."""

    def __init__(self, route: str, *, probe=None, detail: str = "",
                 owner_issue=None, opens_pull_request: bool = False,
                 targets=(), baseline_failures=None,
                 verification_failures=None, acceptable_failures=None,
                 artifacts=(), notes=(), handoff=None, source=None,
                 reason=None):
        if reason not in ROUTE_REASONS.get(route, ()):
            # Not a defensive check: the route and the reason are two
            # halves of one statement, and a pair this module could not
            # have produced is a bug here rather than something a
            # consumer should be asked to reconcile.
            raise AssertionError(
                f"the {route!r} route cannot be reached for the reason "
                f"{reason!r}; its reasons are "
                f"{', '.join(ROUTE_REASONS.get(route, ()))}")
        self.reason = reason
        self.handoff = handoff
        # The diagnosis document this outcome was derived from, kept so
        # the emitted artifact can carry its evidence, attestations and
        # repair record without `evaluate` threading each one separately.
        self.source = source
        self.route = route
        self.probe = probe
        self.detail = detail
        self.owner_issue = owner_issue
        self.opens_pull_request = opens_pull_request
        self.targets = list(targets)
        self.baseline_failures = baseline_failures
        self.verification_failures = verification_failures
        self.acceptable_failures = acceptable_failures
        self.artifacts = list(artifacts)
        self.notes = list(notes)

    @staticmethod
    def _batch_reference(section) -> dict | None:
        """One controlled batch, by the values that IDENTIFY its evidence.

        A reference, not a copy: the result documents stay on disk beside
        their retained artifacts, and #1438/#1439 need to find them, not
        to receive them inline.
        """
        if not isinstance(section, dict):
            return None
        result = section.get("result") or {}
        return {
            "worktree": section.get("worktree"),
            "commit_sha": result.get("commit_sha"),
            "timestamp_utc": result.get("timestamp_utc"),
            "artifact_root": result.get("artifact_root"),
            "invocation_dir": result.get("invocation_dir"),
            "retained_artifacts": list(result.get("retained_artifacts") or []),
        }

    def _handoff_identity(self) -> dict | None:
        """WHICH `/deflake` invocation this diagnosis consumed.

        The census row cannot answer this — `probe_census.ingest_result`
        drops the ports, the per-run check maps, the artifact root, the
        invocation directory and the exact command — so the identity is
        taken from the handoff's own embedded result and invocation.
        """
        if self.handoff is None:
            return None
        result = self.handoff.result
        return {
            "probe": self.handoff.probe,
            "commit_sha": self.handoff.commit_sha,
            "acceptable_failures": self.handoff.acceptable_failures,
            "targets": list(self.handoff.targets),
            "expected_checks": list(self.handoff.expected_checks),
            # The WHOLE descriptor, labels included. The identifier list
            # above is the stable contract, but a label is the check's
            # stated MEANING, and a consumer holding a supplied
            # measurement to identifiers alone would accept a batch that
            # relabelled one to describe a different assertion. A route
            # with a single measurement has nothing else to compare it
            # against, so the record has to carry it.
            "expected_descriptor": _deep_copy(
                self.handoff.expected_descriptor),
            "timestamp_utc": result.get("timestamp_utc"),
            "artifact_root": result.get("artifact_root"),
            "invocation_dir": result.get("invocation_dir"),
            "command": list(self.handoff.invocation["command"]),
            "directory": self.handoff.invocation["directory"],
            "retained_artifacts": list(self.handoff.artifacts),
        }

    def to_document(self) -> dict:
        """The one versioned artifact #1438 and #1439 consume.

        #1437 owns the PRODUCER record; what those issues do with it is
        theirs to define. Everything the approved spec addition names is
        here — route, input-handoff identity, probe, targets, baseline
        SHA, X, configuration manifest, controlled result references,
        diagnosis evidence, preservation attestations, and the repair
        commit and verification evidence when the route has them — so a
        downstream consumer never has to re-read the diagnosis document
        this was derived from.
        """
        source = self.source or {}
        baseline = self._batch_reference(source.get("baseline"))
        verification = self._batch_reference(source.get("verification"))
        return {
            "schema": OUTCOME_SCHEMA,
            "route": self.route,
            # WHY, for a program. `detail` says the same thing in prose.
            "reason": self.reason,
            "probe": self.probe,
            "detail": self.detail or None,
            "owner_issue": self.owner_issue,
            "opens_pull_request": self.opens_pull_request,
            "targets": self.targets,
            "acceptable_failures": self.acceptable_failures,
            "baseline_failures": self.baseline_failures,
            "verification_failures": self.verification_failures,
            "retained_artifacts": self.artifacts,
            "notes": self.notes or None,
            "handoff": self._handoff_identity(),
            "baseline_sha": (self.handoff.commit_sha
                             if self.handoff is not None else None),
            "configuration": (self.handoff.configuration
                              if self.handoff is not None else None),
            "baseline": baseline,
            "verification": verification,
            "diagnosis": source.get("diagnosis"),
            "attestations": source.get("attestations"),
            "repair": source.get("repair"),
        }


def _deep_copy(value):
    """A copy no later mutation of the original can reach through."""
    return json.loads(json.dumps(value))


def declared_worktrees(document) -> list:
    """Every worktree the document's own sections declare.

    Collected BEFORE either batch is validated, so each batch's paths are
    checked against both comparison states rather than only its own. That
    is what still holds once the worktrees have been removed — which the
    workflow requires — and they are no longer registered anywhere.
    """
    declared = []
    for key in ("baseline", "verification"):
        section = document.get(key)
        if not isinstance(section, dict):
            continue
        value = section.get("worktree")
        if isinstance(value, str) and value:
            declared.append(value)
    return declared


def _declared_worktree(section, what: str) -> str:
    """The worktree a section names, as a usable path.

    One check, called by both readers: `_require_batch` needs it before
    it can bind the recorded script to `<worktree>/tools/…`, and
    `_require_worktree` needs it to bind the invocation directory. Two
    copies would mean neither could be shown to be the one doing the
    work.
    """
    declared = section.get("worktree")
    if not isinstance(declared, str) or not declared:
        raise HandoffError(f"{what} names no worktree")
    return declared


def _require_worktree(section, *, what: str) -> Path:
    """The section's declared worktree, canonical and really measured in.

    Two raw strings are not two worktrees: `/tmp/tree` and `/tmp/tree/.`
    name one checkout. So the declared path is canonicalised, and the
    invocation is bound to it — without that binding a section could
    declare one worktree and measure in another, and the "not the same
    worktree" rule would compare labels nobody used.

    Deliberately NOT "must be a registered `git worktree`". The workflow
    is required to remove or hand off BOTH comparison worktrees when it
    finishes, so by the time its document is evaluated the paths it
    correctly names may no longer exist — and a rule that rejected a
    truthful record for being tidy would push agents into leaving
    worktrees behind. What replaces it is stronger for the risk that
    matters: every destination is checked against the DECLARED worktrees
    as well as the registered ones, so nothing may be written into
    either comparison state whether or not it is still checked out.
    """
    declared = _declared_worktree(section, what)
    directory = section["invocation"]["directory"]
    if inside_any_worktree(directory, [declared]) is None:
        raise HandoffError(
            f"{what} declares the worktree {declared} but its invocation "
            f"ran in {directory}; a section that measures somewhere other "
            f"than the worktree it names is not describing one state")
    return sorted(_path_forms(declared))[0]


def resource_hold_problems(section, *, what: str, probe: str) -> list:
    """Why a controlled batch was not isolated from the rest of the machine.

    `probe_flake.measure` records `peak_concurrency`, which counts other
    FLAKE-HARNESS invocations and nothing else (`tools/probe_flake.py`
    keeps its own registry). An independent `tools/run_probes.py` sweep
    holding the same repository-relative resource would not appear there
    at all, so `peak_concurrency: 1` cannot prove the isolation this
    comparison depends on — `probe_resource_lock` is what coordinates
    across processes, and the approved spec addition requires the batch
    to have held the probe's DECLARED interests.

    The hold has to span the CONFIGURATION INSTALL as well as the runs:
    the manifest is reproduced into the worktree before the batch starts,
    and a concurrent sweep that rewrote `config/*.local.yaml` in between
    would leave the recorded manifest describing a state the runs never
    saw.

    A malformed block RAISES — that is a malformed input. A hold that was
    not obtained is RETURNED as a problem, because "another process held
    the resource" is a measurement that did not happen under control,
    which this workflow routes to #1439 rather than rejecting.
    """
    record = section.get("resource_hold")
    if not isinstance(record, dict):
        raise HandoffError(
            f"{what} records no `resource_hold`; `peak_concurrency` counts "
            f"only other flake-harness runs, so a batch that did not hold "
            f"{probe!r}'s declared cross-process interests has not shown it "
            f"was isolated from an independent `run_probes.py` sweep")
    for field, expected in (
            ("exclusive", sorted(probe_runner_resources.exclusive_resources(probe))),
            ("shared", sorted(probe_runner_resources.shared_resources(probe)))):
        declared = record.get(field)
        if not isinstance(declared, list) or not all(
                isinstance(name, str) for name in declared):
            raise HandoffError(
                f"{what}.resource_hold.{field} must be a list of resource "
                f"names, got {declared!r}")
        if sorted(declared) != expected:
            raise HandoffError(
                f"{what}.resource_hold.{field} is {sorted(declared)} where "
                f"`probe_runner_resources` declares {expected} for {probe!r}; "
                f"the "
                f"interests are the probe's own, not this batch's to choose")
    if record.get("covers_configuration_install") is not True:
        raise HandoffError(
            f"{what}.resource_hold does not state that it covered the "
            f"configuration install; the manifest is reproduced into the "
            f"worktree BEFORE the runs begin, so a hold taken afterwards "
            f"leaves the recorded configuration describing a state the "
            f"measurement never ran under")
    problems = []
    if record.get("held") is not True:
        problems.append(
            f"{what} did not obtain {probe!r}'s declared resource hold "
            f"({record.get('detail') or 'no reason recorded'}); another "
            f"process owned the resource, so this batch did not run under "
            f"the controlled conditions the comparison assumes")
    return problems


def _require_batch(section, *, what: str, handoff: Handoff,
                   worktrees) -> list:
    """One controlled measurement side, and why it is unusable if it is.

    Raises for a malformed input; RETURNS the reasons the batch is not a
    usable controlled measurement, which the caller routes rather than
    rejects (see `controlled_problems`).
    """
    if not isinstance(section, dict):
        raise HandoffError(f"{what} must be a JSON object")
    result = section.get("result")
    if not isinstance(result, dict):
        raise HandoffError(
            f"{what} carries no {probe_flake.RESULT_SCHEMA!r} document")
    require_result(result, f"{what}.result")
    require_descriptor(result, handoff.expected_descriptor, f"{what}.result")
    if result["probe"] != handoff.probe:
        raise HandoffError(
            f"{what}.result measured {result['probe']!r}, not "
            f"{handoff.probe!r}")
    # A controlled batch runs the harness out of the worktree it DECLARES,
    # which is the state it is supposed to be measuring — its invocation
    # directory may be a subdirectory of that, and the tool is not there.
    declared = _declared_worktree(section, what)
    invocation = require_invocation(section.get("invocation"),
                                   f"{what}.invocation",
                                   launcher=HARNESS_LAUNCHER, root=declared)
    require_manifest(section.get("configuration"), f"{what}.configuration")

    # A command and the document it produced have to be one measurement.
    # Comparing the two invocations to EACH OTHER is not enough: two
    # commands that agreed on another probe, twenty runs and eight
    # capabilities would compare equal while their result documents both
    # claimed the handoff's contract.
    settings = effective_settings(invocation, f"{what}.invocation",
                                  result=result)
    for label, recorded, reported in (
            ("--probe", settings["probe"], result["probe"]),
            ("--runs", settings["runs"], result["requested_runs"]),
            ("--rts-caps", settings["rts_caps"], result["rts_capabilities"])):
        if recorded != reported:
            raise HandoffError(
                f"{what}.invocation passed {label} {recorded!r} where its "
                f"own result document reports {reported!r}; the command and "
                f"the document it produced describe one measurement or "
                f"neither constrains the other")
    # Only the AGREEMENT is enforced here. What the agreed value has to
    # BE is already owned elsewhere and stays there, so one fact is not
    # scored twice: the probe by the handoff check just below, and the
    # run count and capability count by `controlled_problems`, which
    # routes them to #1439 rather than rejecting them. Two commands that
    # agreed with EACH OTHER on twenty runs would therefore need result
    # documents saying twenty as well — and those are not measurements.

    # Both comparison worktrees are source-clean AT MEASUREMENT TIME. The
    # recorded SHA cannot reveal an uncommitted change, so the clean side
    # needs the same attestation the repair side does — "the clean
    # comparison worktree must remain unmodified" is a contract about its
    # SOURCE, and the gitignored configuration state it must ALSO
    # reproduce is recorded separately in its own manifest.
    if section.get("source_clean") is not True:
        raise HandoffError(
            f"{what} was not recorded as source-clean at measurement time "
            f"(got {section.get('source_clean')!r}); an uncommitted change "
            f"is invisible to the commit the result document names, so a "
            f"batch measured over one measures something no commit "
            f"contains")

    if what == "baseline" and result["commit_sha"] != handoff.commit_sha:
        raise HandoffError(
            f"the controlled baseline measured commit "
            f"{result['commit_sha']} where the handoff's baseline is "
            f"{handoff.commit_sha}; the clean comparison worktree is "
            f"created AT the handoff's baseline SHA and the repair "
            f"worktree from that same SHA, so a baseline taken at another "
            f"commit is not the control this comparison needs — if the "
            f"intended base moved, recreate BOTH states on one new common "
            f"SHA and repeat the baseline")
    trees = list(worktrees) + [_require_worktree(section, what=what)]
    # `worktrees` already carries BOTH declared comparison worktrees (see
    # `declared_worktrees`), which matters once they have been removed:
    # after cleanup neither appears in `worktree_paths()`, and a batch
    # checked only against its own declaration could name an artifact
    # root inside the OTHER comparison state and pass.

    _launcher, options = parse_command(
        invocation["command"], f"{what}.invocation.command",
        script_first=bool(invocation.get("script_first")))
    declared_root = options.get("--artifact-root")
    if declared_root is not None:
        told = _path_forms(declared_root, invocation["directory"])
        reported = _path_forms(result["artifact_root"],
                               invocation["directory"])
        if not told & reported:
            raise HandoffError(
                f"{what} was told to write artifacts to {declared_root} but "
                f"its result document reports {result['artifact_root']}; the "
                f"command and the document it produced have to describe one "
                f"measurement, or neither constrains the other")
    _check_paths(what=what, invocation=invocation, result=result,
                 trees=trees)
    return (controlled_problems(result, what=f"{what}.result")
            + resource_hold_problems(section, what=what, probe=handoff.probe))


class Diagnosis:
    """One invocation: one handoff, one probe, at most one pull request.

    The one-PR limit is a property of this object rather than a rule
    somebody has to remember: `open_pull_request` may be reached once,
    and only on the repair route.
    """

    def __init__(self, handoff: Handoff):
        self.handoff = handoff
        self.pull_requests = 0

    def open_pull_request(self, outcome: Outcome) -> int:
        if outcome.route not in ROUTES_THAT_CHANGE_CODE:
            raise RouteRefused(
                f"the {outcome.route!r} route opens no pull request; it "
                f"stops without a repair and emits its declared handoff")
        if self.pull_requests:
            raise RouteRefused(
                f"this invocation already opened a pull request for "
                f"{self.handoff.probe!r}; one invocation opens at most one "
                f"PR for one probe-side root cause, and a second cause is a "
                f"second measurement")
        self.pull_requests += 1
        return self.pull_requests


def evaluate(document, *, worktrees=(), primary=None) -> Outcome:
    """The route one diagnosis document's own evidence supports.

    Raises `HandoffError` when the entry gate refuses the input and
    `RouteRefused` when a well-formed document declares a route its
    evidence denies — a repair whose verification stayed above X, for
    instance, which is `partial-improvement` and not a repair the agent
    may relabel.
    """
    if not isinstance(document, dict):
        raise HandoffError(
            f"diagnosis must be a JSON object, got {type(document).__name__}")
    schema = document.get("schema")
    if schema != DIAGNOSIS_SCHEMA:
        raise HandoffError(
            f"diagnosis is {schema!r}, expected {DIAGNOSIS_SCHEMA!r}")
    # The comparison worktrees are collected BEFORE the handoff is
    # admitted, so its own evidence is held to the same containment rule
    # the batches are — including once those worktrees have been removed
    # and no longer appear in `worktree_paths()`.
    trees = list(worktrees) + declared_worktrees(document)
    handoff = require_handoff(document.get("handoff"), worktrees=trees,
                              primary=primary)
    route = document.get("route")
    if route not in ROUTES:
        raise HandoffError(
            f"diagnosis declares the route {route!r}; the declared routes "
            f"are {', '.join(ROUTES)}")
    if route == ROUTE_HANDOFF_REJECTED:
        raise HandoffError(
            f"diagnosis declares {ROUTE_HANDOFF_REJECTED!r} while carrying a "
            f"handoff this gate accepted; that route is what a REFUSED "
            f"handoff produces, and it is never a conclusion drawn after one "
            f"was admitted")

    # A handoff with no targets ends here, before any batch is read: there
    # is nothing to reproduce, nothing to repair and no PR to open, so the
    # invocation hands the measurement to #1439 and stops. Declaring any
    # OTHER route over such a handoff is the mislabelling this refuses —
    # `cannot-reproduce` would claim a batch failed to reproduce something
    # that was never observed.
    if not handoff.targets:
        if route != ROUTE_NO_TARGET:
            raise RouteRefused(
                f"the handoff's measurement observed no non-PASS check, so "
                f"there is no target to diagnose and {route!r} is not its "
                f"outcome; this is the {ROUTE_NO_TARGET!r} outcome for "
                f"#{ROUTE_OWNER[ROUTE_NO_TARGET]}")
        for section in ("baseline", "verification"):
            if document.get(section) is not None:
                raise RouteRefused(
                    f"the {ROUTE_NO_TARGET!r} route runs no {section} batch: "
                    f"it stops before creating repair work, so a {section} "
                    f"result here describes work the route forbids")
        return Outcome(
            ROUTE_NO_TARGET, probe=handoff.probe,
            reason=REASON_NO_NON_PASS_CHECK,
            owner_issue=ROUTE_OWNER[ROUTE_NO_TARGET],
            detail=(f"the handoff's {RUN_COUNT}-run measurement observed no "
                    f"non-PASS check, so it identifies nothing to diagnose"),
            targets=[], acceptable_failures=handoff.acceptable_failures,
            artifacts=_accumulate(handoff.artifacts, handoff.result),
            handoff=handoff, source=document)
    if route == ROUTE_NO_TARGET:
        raise RouteRefused(
            f"diagnosis declares {ROUTE_NO_TARGET!r} while its handoff names "
            f"the target check(s) {', '.join(handoff.targets)}; that route is "
            f"what an all-PASS measurement produces")

    owner = ROUTE_OWNER[route]
    # Every retained artifact this invocation has, from every batch it
    # ran — not just the handoff's. A #1439 route exists to hand the
    # evidence on, and the batch that went wrong is usually the
    # VERIFICATION, whose logs would otherwise never be named at all.
    # Deduplicated, not concatenated. The envelope requires `artifacts` to
    # EQUAL `result.retained_artifacts`, so a real handoff names every
    # retained directory twice and a plain `+` would report each of them
    # twice in the outcome.
    artifacts = _accumulate(handoff.artifacts, handoff.result)
    common = {
        "handoff": handoff,
        "source": document,
        "probe": handoff.probe,
        "targets": handoff.targets,
        "acceptable_failures": handoff.acceptable_failures,
        "owner_issue": owner,
        "artifacts": artifacts,
    }

    baseline_section = document.get("baseline")
    if baseline_section is None:
        raise HandoffError(
            "diagnosis carries no controlled pre-fix baseline; the #1436 "
            "census result triggers diagnosis but is not itself the "
            "controlled base-versus-branch comparison")
    baseline_invalid = _require_batch(baseline_section, what="baseline",
                                      handoff=handoff, worktrees=trees)
    baseline = baseline_section["result"]
    baseline_failures = failure_count(baseline)
    common["baseline_failures"] = baseline_failures
    common["artifacts"] = _accumulate(common["artifacts"], baseline)

    # The HANDOFF's manifest defines both batches' initial configuration —
    # not whatever `config/` happened to hold when the diagnosis started.
    # `Engine.Core.Init.migrateLegacyConfig` can materialize an absent
    # local file during a first boot, so "the files that are there now"
    # and "the files the measurement read" are different questions.
    #
    # A mismatch means the recorded bytes could not be reproduced, and the
    # approved correction makes that the #1439 cannot-reproduce OUTCOME
    # rather than a refusal: the invocation genuinely could not establish
    # the condition, which is a result to hand on with its evidence, not a
    # malformed input to reject.
    config_problems = manifest_differences(
        handoff.configuration, baseline_section["configuration"],
        left_name="the handoff", right_name="the clean comparison worktree")
    if config_problems:
        detail = ("the controlled baseline did not reproduce the handoff's "
                  "configuration state, so it is not the same condition: "
                  + "; ".join(config_problems))
        if route == ROUTE_CANNOT_REPRODUCE:
            _require_evidence(document, route)
            return Outcome(route, detail=detail,
                           reason=REASON_CONFIGURATION_NOT_RECREATED,
                           **common)
        raise RouteRefused(
            f"{detail}; recorded bytes that cannot be recovered exactly are "
            f"the {ROUTE_CANNOT_REPRODUCE!r} outcome for "
            f"#{ROUTE_OWNER[ROUTE_CANNOT_REPRODUCE]}")

    # The conditions travel the WHOLE chain: handoff -> baseline ->
    # verification. Comparing only the last pair let both controlled
    # batches agree on some arbitrary timeout and starting port while the
    # handoff sat at the defaults — an agreement between two batches is
    # not the measurement the handoff was taken under, and neither CLI
    # can set those values anyway.
    #
    # Compared across LAUNCHERS, which is the point: `/deflake` supplies
    # its run count and capability count from its own constants and the
    # harness reads them from a command line, so this is where those two
    # accounts of one contract are made to agree.
    replay = invocation_differences(
        handoff.invocation, baseline_section["invocation"],
        results=(handoff.result, baseline), names=("handoff", "baseline"))
    if replay:
        raise RouteRefused(
            "the controlled baseline did not replay the conditions the "
            "handoff was measured under, so it is not the same "
            "measurement: " + "; ".join(replay))

    reproduced = probe_census.tolerance_state(
        handoff.acceptable_failures, baseline["requested_runs"],
        baseline["completed_runs"], baseline_failures)
    observed = set(non_pass_ids(baseline))
    hit = [cid for cid in handoff.targets if cid in observed]
    # The SECOND qualification, independent of the aggregate arithmetic.
    missing_hit = missing_targets(baseline, handoff.targets)

    # An INVALID baseline reproduced nothing: it is not evidence that the
    # probe is fine, so it goes to #1439 with what it did retain rather
    # than being rejected as a malformed input.
    if baseline_invalid and route != ROUTE_CANNOT_REPRODUCE:
        raise RouteRefused(
            f"the controlled baseline is not a usable measurement, so it "
            f"established nothing to repair from; this is the "
            f"{ROUTE_CANNOT_REPRODUCE!r} outcome for #1439: "
            + "; ".join(baseline_invalid))

    if route == ROUTE_CANNOT_REPRODUCE:
        _require_evidence(document, route)
        if baseline_invalid:
            return Outcome(route, detail=(
                "the controlled baseline never became a measurement: "
                + "; ".join(baseline_invalid)),
                reason=REASON_BASELINE_NOT_CONTROLLED, **common)
        if reproduced == probe_census.TOLERANCE_OVER and hit:
            raise RouteRefused(
                f"the controlled baseline DID reproduce an over-tolerance "
                f"result ({baseline_failures}/{RUN_COUNT} against an X of "
                f"{handoff.acceptable_failures}) with the target check(s) "
                f"{', '.join(hit)} non-PASS, so {ROUTE_CANNOT_REPRODUCE!r} "
                f"is not this measurement's outcome")
        if missing_hit:
            raise RouteRefused(
                f"the controlled baseline DID reproduce the target check(s) "
                f"{', '.join(missing_hit)} as MISSING, which qualifies as a "
                f"pre-fix defect however the aggregate count fell "
                f"({baseline_failures}/{RUN_COUNT} against an X of "
                f"{handoff.acceptable_failures}), so "
                f"{ROUTE_CANNOT_REPRODUCE!r} is not this measurement's "
                f"outcome")
        return Outcome(route, detail=(
            f"the controlled baseline observed {baseline_failures}/"
            f"{RUN_COUNT} failures against an X of "
            f"{handoff.acceptable_failures}"),
            reason=REASON_BASELINE_OBSERVED_NOTHING, **common)

    # EITHER qualification is enough (approved correction, 2026-08-24):
    # the batch is over tolerance, or a target was reproducibly MISSING.
    # The second is not reachable through the first — a run whose target
    # check was never emitted can still be classified PASS, so a 0/X
    # baseline can carry a reproduced defect the aggregate cannot see.
    # Verification is NOT relaxed to match: it must still come in at or
    # below X *and* satisfy the MISSING rules.
    if reproduced != probe_census.TOLERANCE_OVER and not missing_hit:
        raise RouteRefused(
            f"the controlled baseline observed {baseline_failures}/"
            f"{RUN_COUNT} failures against an X of "
            f"{handoff.acceptable_failures} ({reproduced}) and reported no "
            f"target check MISSING; a repair may only proceed from a "
            f"baseline that exceeds X or reproducibly loses a target, so "
            f"this is the {ROUTE_CANNOT_REPRODUCE!r} outcome for #1439")
    if not hit:
        raise RouteRefused(
            f"the controlled baseline never observed the target check(s) "
            f"{', '.join(handoff.targets)} as FAIL or MISSING, so it did "
            f"not reproduce the pattern the target was identified from; "
            f"this is the {ROUTE_CANNOT_REPRODUCE!r} outcome for #1439")

    if route in (ROUTE_PRODUCTION_DEFECT, ROUTE_NO_CONFIDENT_FIX):
        _require_evidence(document, route)
        if document.get("verification") is not None:
            raise RouteRefused(
                f"the {route!r} route runs no verification batch: it opens "
                f"no pull request and changes no probe, so a verification "
                f"result here means a repair was attempted and the route is "
                f"mislabelled")
        return Outcome(route, detail=_evidence_detail(document, route),
                       reason=REASON_DIAGNOSIS_DECLARED, **common)

    verification_section = document.get("verification")
    if verification_section is None:
        raise HandoffError(
            f"the {route!r} route requires a verification batch; a repair "
            f"is only ever accepted against a fresh {RUN_COUNT}-run "
            f"measurement in the repair worktree")
    verification_invalid = _require_batch(
        verification_section, what="verification", handoff=handoff,
        worktrees=trees)
    verification = verification_section["result"]
    verification_failures = failure_count(verification)
    common["verification_failures"] = verification_failures
    common["artifacts"] = _accumulate(common["artifacts"], verification)

    clean = _require_worktree(baseline_section, what="baseline")
    repair_tree = _require_worktree(verification_section, what="verification")
    if (clean == repair_tree or clean in repair_tree.parents
            or repair_tree in clean.parents):
        raise RouteRefused(
            f"the baseline worktree {baseline_section['worktree']} and the "
            f"verification worktree {verification_section['worktree']} are "
            f"not two separate states; the clean comparison worktree stays "
            f"at the baseline commit and unmodified, and the repair lives "
            f"in its own")
    comparability = [
        f"the verification batch did not run under the baseline's "
        f"conditions, so the two are not comparable: {difference}"
        for difference in invocation_differences(
            baseline_section["invocation"], verification_section["invocation"],
            results=(baseline, verification))
    ] + [
        f"the two comparison worktrees do not hold the same configuration "
        f"state: {difference}"
        for difference in manifest_differences(
            baseline_section["configuration"],
            verification_section["configuration"],
            left_name="the clean comparison worktree",
            right_name="the repair worktree")
    ]
    verification_destinations = set()
    for path in destinations(verification_section["invocation"]):
        verification_destinations |= _path_forms(
            path, verification_section["invocation"]["directory"])
    for path in destinations(baseline_section["invocation"]):
        forms = _path_forms(path, baseline_section["invocation"]["directory"])
        if forms & verification_destinations:
            raise RouteRefused(
                f"both batches wrote to {path}; the baseline evidence must "
                f"survive the verification that follows it")

    # The command's destinations are not the whole story: `--artifact-root`
    # is OPTIONAL, and two batches that both let `default_artifact_root`
    # supply it share a root legitimately. What they can never share is
    # the INVOCATION DIRECTORY beneath it — `new_invocation_dir` creates a
    # fresh collision-free one per invocation, stamped with the time, the
    # pid and a uuid — so a verification reporting the baseline's is
    # reporting the baseline's artifacts as its own.
    shared = _path_forms(baseline["invocation_dir"]) & _path_forms(
        verification["invocation_dir"])
    if shared:
        raise RouteRefused(
            f"both batches report the invocation directory "
            f"{verification['invocation_dir']}; "
            f"`probe_flake.new_invocation_dir` creates a fresh one per "
            f"invocation, so a verification naming the baseline's is "
            f"claiming the baseline's artifacts as its own — sharing the "
            f"artifact ROOT is fine, sharing what sits under it is not")

    # Nor may either batch write INTO the other's invocation directory.
    # Equality alone let a verification point `--result` at, say, the
    # baseline's retained `run-001/events.jsonl` — distinct paths, and the
    # baseline's evidence overwritten by the run that was supposed to be
    # compared against it. Containment is the rule; a shared artifact ROOT
    # stays legitimate because a root CONTAINS both invocation
    # directories rather than sitting inside either.
    for mine, theirs, label, other in (
            (baseline_section, verification, "the baseline", "verification"),
            (verification_section, baseline, "the verification", "baseline")):
        their_dir = theirs["invocation_dir"]
        paths = [("wrote", path) for path in destinations(mine["invocation"])]
        paths += [(f"reports {name} at", path)
                  for name, path in result_paths(mine["result"])]
        for verb, path in paths:
            if inside_any_worktree(path, [their_dir],
                                   base=mine["invocation"]["directory"]):
                raise RouteRefused(
                    f"{label} {verb} {path}, inside the {other} batch's own "
                    f"invocation directory {their_dir}; one batch writing "
                    f"into the other's artifacts contaminates the evidence "
                    f"the comparison is made of")

    # A verification batch is ACCEPTED only when both halves hold: the
    # count is at or below X, and the scoped MISSING rule is intact. The
    # issue names them together for a reason — "verification remains
    # above X, contains any MISSING result, becomes invalid, or only
    # partially improves the rate" is ONE list, and every entry on it
    # goes to #1439 rather than to a pull request.
    missing = missing_problems(verification, targets=set(handoff.targets),
                               what="the verification batch")
    problems = verification_invalid + comparability + missing
    state = probe_census.tolerance_state(
        handoff.acceptable_failures, verification["requested_runs"],
        verification["completed_runs"], verification_failures)
    accepted = state == probe_census.TOLERANCE_ACCEPTABLE and not problems

    if route == ROUTE_PARTIAL_IMPROVEMENT:
        if accepted:
            raise RouteRefused(
                f"the verification batch reached {verification_failures}/"
                f"{RUN_COUNT} against an X of "
                f"{handoff.acceptable_failures} with the MISSING rule "
                f"intact; that is an accepted verification, so "
                f"{ROUTE_PARTIAL_IMPROVEMENT!r} is not its outcome")
        _require_evidence(document, route)
        explanation = ("; ".join(problems) if problems else
                       f"stayed above the X of "
                       f"{handoff.acceptable_failures}")
        return Outcome(route, detail=(
            f"the verification batch went from {baseline_failures} to "
            f"{verification_failures} failures out of {RUN_COUNT} but "
            f"{explanation}"),
            reason=_verification_reason(verification_invalid, comparability,
                                        missing, state),
            **common)

    # The repair route: everything above held, so the remaining questions
    # are the ones about the repair itself.
    if problems:
        raise RouteRefused(
            f"the verification batch is not an accepted verification "
            f"whatever its failure count; this is the "
            f"{ROUTE_PARTIAL_IMPROVEMENT!r} outcome for #1439: "
            + "; ".join(problems))
    if state != probe_census.TOLERANCE_ACCEPTABLE:
        raise RouteRefused(
            f"the verification batch observed {verification_failures}/"
            f"{RUN_COUNT} failures against an X of "
            f"{handoff.acceptable_failures} ({state}); a repair is accepted "
            f"only at or below X, so this is the "
            f"{ROUTE_PARTIAL_IMPROVEMENT!r} outcome for #1439")
    _require_evidence(document, route)
    _require_repair(document, verification_section,
                    baseline_sha=handoff.commit_sha)
    return Outcome(route, opens_pull_request=True, detail=(
        f"{baseline_failures}/{RUN_COUNT} before, {verification_failures}/"
        f"{RUN_COUNT} after, against an X of "
        f"{handoff.acceptable_failures}"),
        reason=REASON_VERIFICATION_ACCEPTED, **common)


def _verification_reason(invalid, comparability, missing, state) -> str:
    """Which half of the acceptance gate the verification actually failed.

    In the order the gate builds its problem list, so the reason names
    the FIRST thing that made the batch unacceptable rather than the
    last thing checked. `state` is consulted only once none of the three
    problem groups fired, which is exactly the case the count decides.

    Not derivable downstream: whether the two comparison worktrees held
    the same configuration, and whether the batch ran under control, are
    facts about the invocation rather than about either result document,
    so a consumer handed only the documents could not tell an
    over-tolerance verification from an incomparable one.
    """
    if invalid:
        return REASON_VERIFICATION_NOT_CONTROLLED
    if comparability:
        return REASON_VERIFICATION_NOT_COMPARABLE
    if missing:
        return REASON_VERIFICATION_MISSING_RULE
    del state
    return REASON_VERIFICATION_OVER_TOLERANCE


def _accumulate(artifacts, document) -> list:
    """`artifacts` plus everything `document` retained, order preserved."""
    combined = list(artifacts)
    for path in document.get("retained_artifacts") or []:
        if path not in combined:
            combined.append(path)
    return combined


def _require_evidence(document, route: str) -> None:
    """The diagnosis every route must state, and the repair's attestations.

    Deliberately a check on PRESENCE and SHAPE. Whether the evidence is
    convincing is the reviewer's call, but a route declared with no
    stated cause and no evidence at all cannot be reviewed, so it cannot
    be declared.
    """
    diagnosis = document.get("diagnosis")
    if not isinstance(diagnosis, dict):
        raise HandoffError(
            f"the {route!r} route states no diagnosis; every route records "
            f"why the measurement ended where it did")
    evidence = diagnosis.get("evidence")
    if (not isinstance(evidence, list) or not evidence
            or not all(isinstance(item, str) and item.strip()
                       for item in evidence)):
        raise HandoffError(
            f"the {route!r} route records no diagnosis evidence; the "
            f"evidence is what makes the route reviewable rather than "
            f"asserted")
    category = diagnosis.get("category")
    if route == ROUTE_REPAIR:
        if category not in CAUSE_CATEGORIES:
            raise HandoffError(
                f"a repair names the one probe-side cause it fixes, from "
                f"{', '.join(CAUSE_CATEGORIES)}; got {category!r}. Several "
                f"independent causes are the {ROUTE_NO_CONFIDENT_FIX!r} "
                f"route, not a list here")
        attestations = document.get("attestations")
        if not isinstance(attestations, dict):
            raise HandoffError(
                "a repair records its preservation attestations explicitly")
        for name in ATTESTATIONS:
            if attestations.get(name) is not True:
                raise HandoffError(
                    f"a repair must attest {name} (got "
                    f"{attestations.get(name)!r}); weakening an assertion is "
                    f"never a fix, and the shapes a machine cannot see — a "
                    f"broadened expected value, an assertion quietly dropped "
                    f"from a still-passing check — have to be claimed on the "
                    f"record for a reviewer to check")
    elif category is not None and category not in CAUSE_CATEGORIES:
        raise HandoffError(
            f"the {route!r} route names the cause category {category!r}, "
            f"which is not one of {', '.join(CAUSE_CATEGORIES)}")


def _evidence_detail(document, route: str) -> str:
    diagnosis = document.get("diagnosis") or {}
    return diagnosis.get("summary") or f"{route}: see the recorded evidence"


def _require_repair(document, verification_section, *,
                    baseline_sha: str) -> None:
    """The repair is frozen, clean, measured, and on the common baseline."""
    repair = document.get("repair")
    if not isinstance(repair, dict):
        raise HandoffError("a repair route records its `repair` block")
    commit = require_commit(
        repair.get("commit_sha"), "the repair's `commit_sha`",
        because="the repair is committed and frozen BEFORE it is verified")
    base = require_commit(
        repair.get("base_sha"), "the repair's `base_sha`",
        because="the repair worktree is created from the SAME SHA as the "
                "clean comparison worktree, and a repair whose lineage is "
                "unstated cannot be shown to share one")
    if base != baseline_sha:
        raise HandoffError(
            f"the repair is based on {base} while the controlled baseline "
            f"measured {baseline_sha}; the two comparison states share one "
            f"common SHA or they are not a comparison")
    if verification_section["result"]["commit_sha"] != commit:
        raise HandoffError(
            f"the verification batch measured commit "
            f"{verification_section['result']['commit_sha']} while the "
            f"proposed repair is {commit}; `probe_flake` records only "
            f"`git rev-parse HEAD` and cannot see uncommitted source, so a "
            f"verification against another commit measures something this "
            f"pull request does not contain")
    changed = repair.get("changed_paths")
    if (not isinstance(changed, list) or not changed
            or not all(isinstance(path, str) and path for path in changed)):
        raise HandoffError("the repair records no changed paths")
    for path in changed:
        if Path(path).is_absolute():
            raise HandoffError(
                f"the repair names the absolute path {path!r}; changed paths "
                f"are repository-relative, so an absolute one is not a "
                f"statement about this repository at all")
        if os.path.normpath(path) != path or ".." in Path(path).parts:
            raise HandoffError(
                f"the repair names {path!r}, which is not its normalised "
                f"repository-relative form ({os.path.normpath(path)!r}); "
                f"`tools/../src/Engine/Core/Init.hs` begins with `tools/` "
                f"and changes production code, so the scope check is applied "
                f"to the normalised path or it checks nothing")
    apparatus = [path for path in changed if path in HARNESS_MODULES]
    if apparatus:
        raise HandoffError(
            f"the repair changes {', '.join(apparatus)}, which is the "
            f"measurement apparatus rather than the probe under diagnosis. "
            f"`probe_flake.measure`'s timeout and starting port are module "
            f"constants no command line exposes, so a repair that changed "
            f"one would produce a calmer verification while both command "
            f"records still compared equal — the two batches would have "
            f"been run by different harnesses, and the comparison would "
            f"mean nothing")
    offenders = [path for path in changed
                 if not (path.startswith("tools/")
                         or path.startswith("test-headless/"))]
    if offenders:
        raise HandoffError(
            f"the repair changes {', '.join(offenders)}; production Haskell "
            f"and Lua behavior changes are outside this workflow's repair "
            f"scope — an assertion that is right about a product that is "
            f"wrong is the {ROUTE_PRODUCTION_DEFECT!r} route for #1438")


# ==========================================================================
# CLI
# ==========================================================================
def _load(path: str, what: str):
    try:
        return json.loads(Path(path).read_text(encoding="utf-8"))
    except OSError as error:
        raise HandoffError(f"{what} at {path} is unreadable ({error})") from None
    except json.JSONDecodeError as error:
        raise HandoffError(f"{what} at {path} is not JSON ({error})") from None


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    group = ap.add_mutually_exclusive_group(required=True)
    group.add_argument("--diagnosis", metavar="PATH",
                       help=f"evaluate a {DIAGNOSIS_SCHEMA} document")
    group.add_argument("--handoff", metavar="PATH",
                       help=f"check a {HANDOFF_SCHEMA} document against the "
                            f"entry gate and stop")
    group.add_argument("--manifest", metavar="ROOT",
                       help=f"print the {MANIFEST_SCHEMA} for a checkout")
    ap.add_argument("--json", action="store_true",
                    help="print the outcome document instead of prose")
    args = ap.parse_args(argv)

    if args.manifest:
        print(json.dumps(config_manifest(args.manifest), indent=2,
                         sort_keys=True))
        return EXIT_OK

    try:
        if args.handoff:
            handoff = require_handoff(_load(args.handoff, "handoff"),
                                      worktrees=worktree_paths(),
                                      primary=primary_checkout())
            if args.json:
                print(json.dumps({"accepted": True, "probe": handoff.probe,
                                  "targets": list(handoff.targets),
                                  "acceptable_failures":
                                      handoff.acceptable_failures},
                                 indent=2, sort_keys=True))
            else:
                print(f"handoff accepted: {handoff.probe}, targets "
                      f"{', '.join(handoff.targets)}, X="
                      f"{handoff.acceptable_failures}/{RUN_COUNT}")
            return EXIT_OK
        document = _load(args.diagnosis, "diagnosis")
        outcome = evaluate(document, worktrees=worktree_paths(),
                           primary=primary_checkout())
        # The one-PR limit, exercised rather than described: a repair
        # route reaches it exactly once per invocation, and every other
        # route is refused by the session itself rather than by prose.
        if outcome.opens_pull_request:
            Diagnosis(outcome.handoff).open_pull_request(outcome)
    except HandoffError as error:
        print(f"deflake_diagnosis: {ROUTE_HANDOFF_REJECTED}: {error}",
              file=sys.stderr)
        return EXIT_REJECTED
    except RouteRefused as error:
        print(f"deflake_diagnosis: route refused: {error}", file=sys.stderr)
        return EXIT_REFUSED

    if args.json:
        print(json.dumps(outcome.to_document(), indent=2, sort_keys=True))
    else:
        owner = (f" — handed off to #{outcome.owner_issue}"
                 if outcome.owner_issue else "")
        print(f"{outcome.route}: {outcome.detail}{owner}")
        if outcome.artifacts:
            print("retained artifacts:")
            for path in outcome.artifacts:
                print(f"  {path}")
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
