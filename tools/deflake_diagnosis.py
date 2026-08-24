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
the harness would have refused it rather than compared as the number ten.
`--result` is required of `probe_flake.py`, because the document is
written only `if args.result` and a command without one produced no
evidence at all; it is optional for `/deflake`, which retains the
document beside its artifacts either way. ORDER is part of the grammar
too: argv[0] is the interpreter, argv[1] the script, and only argv[2:]
are options, because Python rejects an option it does not know before the
script runs. And the argv must be a PYTHON 3 INTERPRETER — `python3`, optionally
versioned; not bare `python`, which is whichever of the two that machine
means, and not `python2`, which cannot parse these programs at all —
named rather than given by path, since a document cannot show which
binary sits at `/tmp/counterfeit/python3` — running one of those two
scripts. The script is
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
0, and neither was ever produced. Every run
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
* `cannot-reproduce` — the controlled baseline did not reproduce an
  over-X result. Handed off to #1439.
* `production-defect` — the assertion is right and the product is
  wrong. The probe is not touched; handed off to #1438.
* `no-confident-fix` — several failures with no one established
  probe-side cause. Handed off to #1439.
* `partial-improvement` — the repaired batch improved but stayed above
  X, or the batch became invalid. Handed off to #1439.

Emitting a handoff here means emitting `deflake-diagnosis-outcome/v1`
naming the route, the owning issue and the retained evidence. What #1438
and #1439 then DO with it is theirs to define; this module does not
invent their contracts, and filing an issue is not its job.
"""
from __future__ import annotations

import argparse
import fnmatch
import hashlib
import json
import os
import re
import sys
from datetime import datetime
from pathlib import Path, PurePosixPath

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake  # noqa: E402
import probe_census  # noqa: E402
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402

HANDOFF_SCHEMA = "deflake-handoff/v1"
DIAGNOSIS_SCHEMA = "deflake-diagnosis/v1"
MANIFEST_SCHEMA = "deflake-config-manifest/v1"
OUTCOME_SCHEMA = "deflake-diagnosis-outcome/v1"

# The measurement contract, taken from the modules that own it rather
# than restated. A private copy that drifted would let this module
# accept a batch `/deflake` and the census would both reject.
RUN_COUNT = probe_census.POLICY_RUN_COUNT
RTS_CAPABILITIES = probe_flake.DEFAULT_RTS_CAPS

# The gitignored per-worktree configuration family (`.gitignore`). Probes
# symlink the invoking worktree's `config/` into their isolated resource
# roots, so two comparison worktrees that disagree here are measuring two
# different conditions. An EMPTY manifest is the expected default, not an
# edge case, and it is recorded explicitly so absence compares as
# rigorously as contents do.
CONFIG_GLOB = "config/*.local.yaml"

# `/deflake`'s own N and capability count, spelled as the thing every
# supplied invocation is measured against.
SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
COMMIT_RE = re.compile(r"^[0-9a-f]{40}$")

# The two programs that really produce this lab's measurements, and the
# only kind of program that runs either — `/bin/echo .../probe_flake.py
# --probe role --runs 10` has the right SHAPE and measures nothing.
#
# There are TWO because the three batches do not come from one command.
# `/deflake` (#1436) does NOT shell out: it calls `probe_flake.measure`
# in process with its own fixed run count and capability count, and its
# CLI has no `--probe` and no `--runs` at all. So requiring a
# `probe_flake.py` argv everywhere would have made a truthful #1436
# handoff impossible to submit while accepting an argv that never ran.
# `python3`, optionally versioned. NOT bare `python` and NOT `python2`:
# every tool in this lab is a Python 3 program — `from __future__ import
# annotations`, `X | None` annotations, `dict[str, str]` — so `python2
# tools/probe_flake.py` is a SyntaxError, not a measurement, and bare
# `python` is whichever of the two that machine happens to mean, which a
# document cannot settle.
INTERPRETER_RE = re.compile(r"^python3(\.\d+){0,2}$")


class Launcher:
    """One program that produces a `probe-flake-result/v1` document.

    `values` are options taking a value and `flags` are argparse's
    `store_true` ones, which take none — reading a flag as though it
    consumed the next token would silently swallow a real argument.
    `conditions` are the behavior-affecting options this launcher reads
    from its command line; `fixed` are the ones it does not expose at
    all and supplies itself, which is what makes `/deflake`'s
    measurement contract a property of the module rather than of a
    command nobody typed.
    """

    def __init__(self, script, *, values, flags=(), destinations=(),
                 required=(), conditions=(), defaults=None, fixed=None,
                 probe_from_result=False, describes=""):
        self.script = script
        self.values = frozenset(values)
        self.flags = frozenset(flags)
        self.destinations = tuple(destinations)
        self.required = tuple(required)
        self.conditions = tuple(conditions)
        self.defaults = dict(defaults or {})
        self.fixed = dict(fixed or {})
        self.probe_from_result = probe_from_result
        self.describes = describes

    @property
    def options(self) -> frozenset:
        return self.values | self.flags


# `probe_flake.main`'s ENTIRE option surface. Hard-coded because argparse
# builds its parser inside `main`, and kept honest by
# `test_deflake_diagnosis`, which reads the real `--help` and fails if the
# two ever disagree.
#
# Destinations must DIFFER between the batches — writing both to one path
# would destroy the comparison — so they are dropped before conditions
# are compared, and validated separately. `--result` is REQUIRED as well
# as a destination: `probe_flake.main` writes the document only `if
# args.result`, so a command without one produced no evidence, and being
# required and being a destination are orthogonal properties.
DESTINATION_OPTIONS = ("--result", "--artifact-root")
REQUIRED_OPTIONS = ("--probe", "--runs", "--result")
INVOCATION_DEFAULTS = {"--rts-caps": RTS_CAPABILITIES}
CONDITION_OPTIONS = ("--probe", "--runs") + tuple(INVOCATION_DEFAULTS)

HARNESS_OPTIONS = frozenset(REQUIRED_OPTIONS) | frozenset(
    DESTINATION_OPTIONS) | frozenset(CONDITION_OPTIONS)

HARNESS_LAUNCHER = Launcher(
    "probe_flake.py",
    values=HARNESS_OPTIONS,
    destinations=DESTINATION_OPTIONS,
    required=REQUIRED_OPTIONS,
    conditions=CONDITION_OPTIONS,
    defaults=INVOCATION_DEFAULTS,
    describes="the controlled baseline and verification batches")

# `deflake.main`'s entire surface: one flag and one optional destination,
# and deliberately no probe, run-count or RTS override — "a forced probe
# would let an operator manufacture a `recorded` outcome that proves
# nothing about the selection it skipped". So the probe comes from the
# result document the selector produced, and the two counts come from
# `/deflake`'s own constants. `--result` is optional there because the
# document is retained beside its artifacts either way.
DEFLAKE_LAUNCHER = Launcher(
    "deflake.py",
    values=("--result",),
    flags=("--json",),
    destinations=("--result",),
    fixed={"runs": deflake.CENSUS_RUN_COUNT,
           "rts_caps": deflake.RTS_CAPABILITIES},
    probe_from_result=True,
    describes="the #1436 census handoff")

LAUNCHERS = {launcher.script: launcher
             for launcher in (HARNESS_LAUNCHER, DEFLAKE_LAUNCHER)}

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
    "tools/probe_claim.py",
    "tools/probe_resource_lock.py",
    "tools/probe_select.py",
    "tools/probe_engine.py",
    "tools/probelib.py",
    "tools/run_probes.py",
    "tools/deflake.py",
    "tools/deflake_diagnosis.py",
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

ROUTE_REPAIR = "repair-pr"
ROUTE_HANDOFF_REJECTED = "handoff-rejected"
ROUTE_CANNOT_REPRODUCE = "cannot-reproduce"
ROUTE_PRODUCTION_DEFECT = "production-defect"
ROUTE_NO_CONFIDENT_FIX = "no-confident-fix"
ROUTE_PARTIAL_IMPROVEMENT = "partial-improvement"

ROUTES = (ROUTE_REPAIR, ROUTE_HANDOFF_REJECTED, ROUTE_CANNOT_REPRODUCE,
          ROUTE_PRODUCTION_DEFECT, ROUTE_NO_CONFIDENT_FIX,
          ROUTE_PARTIAL_IMPROVEMENT)

# Which issue owns each non-repair route's handoff. `repair-pr` owns its
# own standalone pull request and `handoff-rejected` hands off to nobody
# — the gate refused before any work began, and there is nothing to
# report onward.
ROUTE_OWNER = {
    ROUTE_REPAIR: None,
    ROUTE_HANDOFF_REJECTED: None,
    ROUTE_CANNOT_REPRODUCE: 1439,
    ROUTE_PRODUCTION_DEFECT: 1438,
    ROUTE_NO_CONFIDENT_FIX: 1439,
    ROUTE_PARTIAL_IMPROVEMENT: 1439,
}

# The one route that may touch the probe's source. Every other route
# stops without a repair PR and emits its declared handoff.
ROUTES_THAT_CHANGE_CODE = frozenset({ROUTE_REPAIR})

EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_REFUSED = 3


class HandoffError(Exception):
    """A handoff or diagnosis document the entry gate refuses.

    Distinct from a diagnosis ROUTE on purpose: a malformed input never
    reached a diagnosis, so it is not a `cannot-reproduce` or a
    `no-confident-fix` and must not be recorded as one.
    """


class RouteRefused(Exception):
    """A well-formed document whose declared route the evidence denies."""


# ==========================================================================
# Configuration manifest
# ==========================================================================
def _digest(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(65536), b""):
            digest.update(block)
    return digest.hexdigest()


def config_manifest(root) -> dict:
    """The `config/*.local.yaml` state of one checkout, as a document.

    Sorted by relative path so two manifests compare as data rather than
    as directory order, and an empty `entries` list is a POSITIVE
    statement that the family is absent — which is the expected default
    in this repository, since none of the four paths is tracked.
    """
    base = Path(root).expanduser().resolve()
    entries = []
    for path in sorted(base.glob(CONFIG_GLOB)):
        if not path.is_file():
            continue
        entries.append({
            "path": path.relative_to(base).as_posix(),
            "sha256": _digest(path),
        })
    return {"schema": MANIFEST_SCHEMA, "root": str(base), "entries": entries}


def require_manifest(document, what: str) -> dict:
    """`document` as a valid manifest, or the refusal that names it."""
    if not isinstance(document, dict):
        raise HandoffError(
            f"{what} must be a JSON object, got {type(document).__name__}")
    schema = document.get("schema")
    if schema != MANIFEST_SCHEMA:
        raise HandoffError(
            f"{what} is {schema!r}, expected {MANIFEST_SCHEMA!r}")
    entries = document.get("entries")
    if not isinstance(entries, list):
        raise HandoffError(
            f"{what} has no `entries` list; record an empty list to state "
            f"that no {CONFIG_GLOB} file exists, rather than omitting it — "
            f"absence has to be asserted, not inferred from a missing key")
    seen = set()
    for position, entry in enumerate(entries):
        where = f"{what} entry {position}"
        if not isinstance(entry, dict):
            raise HandoffError(f"{where} must be a JSON object")
        relative = entry.get("path")
        if not isinstance(relative, str) or not relative:
            raise HandoffError(f"{where} has no `path`")
        if relative in seen:
            raise HandoffError(f"{where} repeats the path {relative!r}")
        seen.add(relative)
        parts = PurePosixPath(relative).parts
        if (len(parts) != 2 or parts[0] != "config"
                or not fnmatch.fnmatch(parts[1], "*.local.yaml")):
            raise HandoffError(
                f"{where} names {relative!r}, which is not a member of the "
                f"gitignored {CONFIG_GLOB} family; a manifest whose entries "
                f"can point anywhere — `../outside.local.yaml`, an absolute "
                f"path, a nested directory — establishes equality of "
                f"something other than the per-worktree configuration state "
                f"the probes actually read")
        digest = entry.get("sha256")
        if not isinstance(digest, str) or not SHA256_RE.match(digest):
            raise HandoffError(
                f"{where} has no SHA-256 digest for {relative!r}, got "
                f"{digest!r}")
    return document


def manifest_entries(document) -> dict:
    return {entry["path"]: entry["sha256"] for entry in document["entries"]}


def manifest_differences(left, right, *, left_name: str,
                         right_name: str) -> list:
    """Every way two configuration manifests disagree, absence included."""
    a = manifest_entries(left)
    b = manifest_entries(right)
    problems = []
    for path in sorted(set(a) - set(b)):
        problems.append(
            f"{path} exists in {left_name} and is absent from {right_name}")
    for path in sorted(set(b) - set(a)):
        problems.append(
            f"{path} exists in {right_name} and is absent from {left_name}")
    for path in sorted(set(a) & set(b)):
        if a[path] != b[path]:
            problems.append(
                f"{path} differs: {left_name} {a[path][:12]}…, "
                f"{right_name} {b[path][:12]}…")
    return problems


# ==========================================================================
# Invocation records
# ==========================================================================
# Where a checkout keeps the tools this lab runs. A recorded script path
# must be this directory of the worktree the command says it ran in —
# `Path(script).name` alone would accept `/tmp/counterfeit/probe_flake.py`.
TOOLS_DIR = "tools"

# `new_invocation_dir`'s generated name: the probe, a UTC stamp, the pid
# and eight hex characters of a uuid4. Serialized by the harness and by
# nothing else, so an arbitrary name like `role-not-a-harness-directory`
# is a forged layout however coherent the rest of the document is. The
# STAMP and the PID are checked for meaning as well as shape below —
# `\d{8}T\d{6}Z` alone matches `99999999T999999Z`, which no clock ever
# produced, and `\d+` matches a pid of 0, which no process ever has.
INVOCATION_DIR_RE = re.compile(
    r"^(?P<probe>[a-z][a-z_]*)-(?P<stamp>\d{8}T\d{6}Z)-(?P<pid>\d+)"
    r"-[0-9a-f]{8}$")
INVOCATION_STAMP_FORMAT = "%Y%m%dT%H%M%SZ"


def parse_command(command, what: str, *, launcher=None, root=None,
                  base=None):
    """One recorded argument vector, as `(launcher, {option: value})`.

    ORDER is part of the grammar, not decoration: `python3 --probe role
    tools/probe_flake.py --runs 10` is rejected by PYTHON before the
    script runs, so argv[0] must be the interpreter, argv[1] the script,
    and only argv[2:] are that script's options. Parsing
    order-insensitively accepted a command the shell would never have got
    past.

    `--flag value` and `--flag=value` are the same thing for an option
    that takes a value; a `store_true` flag takes none, and reading one
    as though it did would swallow a real argument.
    """
    if not isinstance(command, list) or not command:
        raise HandoffError(f"{what} must be a non-empty list of arguments")
    if not all(isinstance(token, str) for token in command):
        raise HandoffError(f"{what} must contain only strings")
    if len(command) < 2:
        raise HandoffError(
            f"{what} is {command}, where an invocation is a Python "
            f"interpreter and the script it ran")
    program, script = command[0], command[1]
    if os.sep in program or (os.altsep and os.altsep in program):
        raise HandoffError(
            f"{what} runs the interpreter by path ({program!r}); a document "
            f"cannot show which binary sits at an arbitrary path, so the "
            f"interpreter is named — `python3` — and resolved from PATH like "
            f"every command this lab records. `/tmp/counterfeit/python3` is "
            f"exactly the shape this refuses")
    if not INTERPRETER_RE.match(program):
        raise HandoffError(
            f"{what} runs {program!r}, which is not a Python interpreter; a "
            f"command with the right shape and another program — "
            f"`/bin/echo .../probe_flake.py --probe role --runs 10` — "
            f"measures nothing")
    if script.startswith("-"):
        raise HandoffError(
            f"{what} passes {script!r} before the script it ran; ORDER is "
            f"part of the grammar — Python rejects an option it does not "
            f"know before the script runs at all, so argv[1] is the script "
            f"and every option comes after it")
    found = LAUNCHERS.get(Path(script).name)
    if found is None:
        raise HandoffError(
            f"{what} runs {script!r}; the programs that produce a "
            f"{probe_flake.RESULT_SCHEMA} document are "
            f"{', '.join(sorted(LAUNCHERS))}")
    if root is not None:
        # Resolved from the DIRECTORY THE COMMAND RAN IN, because that is
        # what Python does with a relative script path — then required to
        # be the tool the declared checkout ships. Resolving against the
        # checkout instead let an invocation in a subdirectory name a
        # counterfeit nested `tools/probe_flake.py` and have it compared
        # to the real one.
        expected = Path(root) / TOOLS_DIR / found.script
        if not (_path_forms(script, base if base is not None else root)
                & _path_forms(expected)):
            raise HandoffError(
                f"{what} runs {script} from {base if base is not None else root}, "
                f"where the checkout it declares keeps that tool at "
                f"{expected}; matching only the file NAME would admit "
                f"`/tmp/counterfeit/{found.script}`, and resolving from the "
                f"checkout rather than the working directory would admit a "
                f"counterfeit nested beside it")
    if launcher is not None and found is not launcher:
        raise HandoffError(
            f"{what} runs {found.script}, where {launcher.describes} come "
            f"from {launcher.script}")
    options: dict = {}
    index = 2
    while index < len(command):
        token = command[index]
        if not token.startswith("--"):
            raise HandoffError(
                f"{what} carries the positional token {token!r} after "
                f"{Path(script).name}; every remaining argument is one of "
                f"that script's options, so a bare token ran something the "
                f"comparison cannot see")
        inline = "=" in token
        name, inline_value = (token.split("=", 1) if inline
                              else (token, None))
        if name not in found.options:
            raise HandoffError(
                f"{what} names {name}, which `{found.script}` does not "
                f"accept; its whole option surface is "
                f"{', '.join(sorted(found.options))}, so a command carrying "
                f"anything else was never run by the shipped tool")
        if name in options:
            raise HandoffError(f"{what} repeats {name}")
        if name in found.flags:
            if inline:
                raise HandoffError(
                    f"{what} passes a value to {name}, which is a flag")
            options[name] = True
        elif inline:
            options[name] = inline_value
        else:
            index += 1
            if index >= len(command):
                raise HandoffError(f"{what}: {token} has no value")
            options[name] = command[index]
        index += 1
    for option in found.required:
        if option not in options:
            raise HandoffError(f"{what} names no {option}")
    return found, options


def _integer(value, what: str) -> int:
    """A supplied option value as the harness's argparse would read it.

    `probe_flake.main` declares `--runs` and `--rts-caps` as `type=int`,
    so argparse calls `int(token)` and REFUSES anything it raises on.
    Accepting a float spelling here instead — `--runs 10.0` — would let a
    fabricated command compare numerically equal to a real one while
    `probe_flake` would have exited before measuring anything. So this is
    `int()` and nothing wider: the same grammar, deliberately.
    """
    if isinstance(value, bool):
        raise HandoffError(f"{what} must be an integer, got {value!r}")
    if isinstance(value, int):
        return value
    try:
        return int(value)
    except (TypeError, ValueError):
        raise HandoffError(
            f"{what} must be an integer the harness's own `type=int` would "
            f"accept, got {value!r}; `probe_flake` would have refused this "
            f"command before it measured anything") from None


def require_invocation(document, what: str, *, launcher=None,
                       root=None) -> dict:
    """One recorded invocation, validated against the tool that ran it.

    The record is the COMMAND plus the things the command does not say:
    the directory it ran in, the ports the harness actually leased, and
    the retry policy in force. Ports are evidence, never a condition —
    they are leased dynamically and differ on every run.
    """
    if not isinstance(document, dict):
        raise HandoffError(
            f"{what} must be a JSON object, got {type(document).__name__}")
    directory = document.get("directory")
    if not isinstance(directory, str) or not directory:
        raise HandoffError(f"{what} has no `directory`")
    parse_command(document.get("command"), f"{what}.command",
                  launcher=launcher,
                  root=directory if root is None else root,
                  base=directory)
    # `probe_flake.measure`'s own defaults, which NEITHER CLI exposes.
    # Recorded because they are behavior-affecting and invisible to the
    # command, so without them two identical argv strings could describe
    # two different measurements.
    for field, expected in (("timeout_seconds", probe_flake.DEFAULT_TIMEOUT),
                            ("start_port", probe_flake.PORT_MIN)):
        value = document.get(field)
        if isinstance(value, bool) or not isinstance(value, (int, float)):
            raise HandoffError(
                f"{what} records no `{field}`; it is the value "
                f"`probe_flake.measure` ran under, which no command line "
                f"names — this checkout's is {expected!r}")
        if value != expected:
            raise HandoffError(
                f"{what} records `{field}` as {value!r} where "
                f"`probe_flake.measure` applies {expected!r}; NEITHER CLI "
                f"exposes this setting, so the default is the only value a "
                f"real measurement can have used, and a record naming "
                f"another one describes a run that did not happen")
    retries = document.get("retries")
    if retries != 0:
        raise HandoffError(
            f"{what} records a retry policy of {retries!r}; a measurement "
            f"where any passing attempt counts as a pass measures a "
            f"different quantity, so this lab's retry policy is 0 and "
            f"nothing else (`tools/run_probes.py --retries` is exactly the "
            f"behavior being excluded)")
    ports = document.get("ports")
    if ports is not None:
        if (not isinstance(ports, list)
                or not all(isinstance(port, int) and not isinstance(port, bool)
                           for port in ports)):
            raise HandoffError(f"{what}.ports must be a list of integers")
    return document


def effective_settings(invocation, what: str, *, result=None) -> dict:
    """The behavior-affecting conditions one invocation actually ran under.

    Destinations and the program tokens are dropped: the script path
    differs by worktree and `--result`/`--artifact-root` MUST differ, so
    comparing them would refuse every legitimate pair. An absent option
    is filled in with the effective default its own tool would apply,
    because "the caller declined to override a default" is not a
    difference in conditions.

    A launcher that does not EXPOSE a setting supplies it instead:
    `/deflake` has no `--runs` and no `--rts-caps`, so its contract comes
    from its own constants, and the probe it measured comes from the
    result document its selector produced rather than from a command
    that never named one.
    """
    launcher, options = parse_command(invocation["command"],
                                      f"{what}.command")
    settings = dict(launcher.fixed)
    if launcher.probe_from_result:
        if not isinstance(result, dict):
            raise HandoffError(
                f"{what} was run by {launcher.script}, which chooses the "
                f"probe itself, so its conditions can only be read beside "
                f"the result document it produced")
        settings["probe"] = result["probe"]
    else:
        settings["probe"] = options["--probe"]
    if "--runs" in launcher.conditions:
        settings["runs"] = _integer(options["--runs"],
                                    f"{what}.command --runs")
    for option, default in launcher.defaults.items():
        key = option.lstrip("-").replace("-", "_")
        settings[key] = (_integer(options[option], f"{what}.command {option}")
                         if option in options else default)
    settings["retries"] = invocation["retries"]
    settings["timeout_seconds"] = invocation["timeout_seconds"]
    settings["start_port"] = invocation["start_port"]
    return settings


def invocation_differences(baseline, verification, *, results=(None, None),
                           names=("baseline", "verification")) -> list:
    """Every behavior-affecting difference between two invocations.

    Used for two pairs: handoff-versus-baseline and
    baseline-versus-verification. The first crosses LAUNCHERS, which is
    the point — `/deflake` supplies its run count and capability count
    from its own constants while the harness reads them from a command
    line, and this is where the two accounts of one contract are made to
    agree.
    """
    left = effective_settings(baseline, f"{names[0]}.invocation",
                              result=results[0])
    right = effective_settings(verification, f"{names[1]}.invocation",
                               result=results[1])
    return [f"{key}: {names[0]} {left[key]!r}, {names[1]} {right[key]!r}"
            for key in sorted(set(left) | set(right))
            if left.get(key) != right.get(key)]


def destinations(invocation) -> list:
    """The paths one invocation wrote to, as recorded in its command."""
    launcher, options = parse_command(invocation["command"],
                                      "invocation.command")
    return [options[name] for name in launcher.destinations
            if name in options]


def worktree_paths() -> list:
    """Every registered worktree, the primary checkout FIRST.

    Delegated to `probe_flake`, which already computes exactly this for
    `check_artifact_root` and puts `run_probes.REPO_ROOT` at the head.
    Answering it a second way is how the artifact root and the result
    document would come to disagree about what "inside a worktree" means.
    """
    return probe_flake._worktree_paths()


def primary_checkout():
    """The checkout `/deflake` runs in, or None if it cannot be resolved."""
    trees = worktree_paths()
    return trees[0] if trees else None


def _path_forms(path, base=None) -> set:
    """Every absolute form `path` can denote, for containment purposes.

    A destination reaches `probe_flake` as it was typed, and
    `write_result` opens it RELATIVE TO THE PROCESS'S DIRECTORY — so
    `results/verify.json` from a repair worktree lands inside that
    worktree while matching no absolute registered path at all. The
    recorded invocation directory is what makes a relative destination
    mean something, so it is joined on before anything is compared.

    Both a lexical (`normpath`, so `..` collapses) and a filesystem
    (`resolve`, so a symlinked worktree matches) form are produced. The
    lexical one keeps the rule decidable for a path that does not exist
    yet — which every destination is, before its batch runs — and the
    resolved one is what catches `/tmp` being a link to `/private/tmp`.
    """
    raw = Path(path).expanduser()
    if base is not None and not raw.is_absolute():
        raw = Path(base).expanduser() / raw
    forms = {Path(os.path.normpath(str(raw)))}
    try:
        forms.add(raw.resolve())
    except OSError:
        pass
    return forms


def inside_any_worktree(path, worktrees, *, base=None) -> str | None:
    """The worktree containing `path`, or None.

    Worktrees are supplied rather than discovered, so the rule is
    testable without a git checkout; the CLI passes `worktree_paths()`.
    """
    candidates = _path_forms(path, base)
    for tree in worktrees:
        for form in _path_forms(tree):
            for candidate in candidates:
                if candidate == form or form in candidate.parents:
                    return str(tree)
    return None


# ==========================================================================
# Result documents
# ==========================================================================
def require_result(document, what: str) -> dict:
    """A `probe-flake-result/v1` document, validated CANONICALLY.

    `probe_census.validate_result` is the shipped validator for this
    schema and it is what runs here — declared shape first, then #1493's
    cross-field invariants. Re-deriving a subset of it locally is the
    failure mode this delegation exists to avoid: `_rule_pass_run_has_no
    _failed_check` alone is what stops a document whose runs all say
    PASS while their check maps carry FAIL, which `failure_count` would
    otherwise count as a spotless batch and admit as a verified repair.

    Its `timestamp_utc` goes through `probe_census.parse_timestamp` too, so
    it names a real UTC instant rather than merely having the right
    shape. Two further rules are this module's own, because the
    canonical validator does not make them: a result attributed to no
    commit is not evidence, and the identifier SHAPE rule
    (`probe_protocol.CHECK_ID_RE`) that a self-consistent document
    carrying a runtime value in an identifier still satisfies.
    """
    try:
        probe_census.validate_result(document)
    except probe_census.CensusError as error:
        raise HandoffError(f"{what}: {error}") from None
    probe = document["probe"]
    commit = document.get("commit_sha")
    if not isinstance(commit, str) or not COMMIT_RE.match(commit):
        raise HandoffError(
            f"{what} does not name a resolved commit ({commit!r}); "
            f"`probe_flake` writes the literal 'unknown' when git could not "
            f"be consulted, and a measurement nobody can attribute to a "
            f"commit is not evidence")
    try:
        probe_census.parse_timestamp(document.get("timestamp_utc"),
                                     f"{what}.timestamp_utc")
    except probe_census.CensusError as error:
        raise HandoffError(
            f"{error}; the handoff records WHEN its baseline was measured, "
            f"and `probe_flake.Measurement` stamps every document from a "
            f"real clock") from None
    ids = []
    for position, entry in enumerate(document["checks"]):
        cid = entry["id"]
        if not probe_protocol.CHECK_ID_RE.match(cid):
            raise HandoffError(
                f"{what}.checks[{position}] has no stable identifier "
                f"({cid!r}); `probe-result/v1` identifiers are static and "
                f"carry no runtime value, so this is a malformed protocol "
                f"result rather than something to diagnose")
        ids.append(cid)
    for position, run in enumerate(document["runs"]):
        where = f"{what}.runs[{position}]"
        absent = [cid for cid in ids if cid not in run["checks"]]
        if absent:
            raise HandoffError(
                f"{where} omits declared identifiers entirely "
                f"({', '.join(absent)}); `probe_protocol.parse_event_stream` "
                f"reports every declared check as PASS, FAIL or MISSING, so "
                f"a key that is simply absent means the document was not "
                f"written by the harness")
    _require_indices(document, what)
    _require_retention(document, what)
    require_topology(document, what)
    return document


def _require_indices(document, what: str) -> None:
    """The consecutive numbering `probe_flake.measure` emits.

    `measure` runs `for index in range(1, runs + 1)` and appends one
    record per index, so a valid document's run indices are exactly
    `1..completed_runs` in order — and the run that broke the stream,
    when there is one, is the NEXT index, because it is the one the loop
    was on when it stopped.

    Without this, ten otherwise-valid records could all be numbered `1`:
    a single run replayed ten times, counted as a complete ten-run
    verification. Every other rule here reads a run's index — the
    retention pairing and the `run-{index:03d}` topology both do — so
    leaving the sequence unchecked let a forged layout satisfy them all
    against one repeated number.
    """
    # Counted against the records PRESENT, not against `completed_runs`:
    # whether that tally agrees is `controlled_problems`' rule, and one
    # fact answered in two places is one fact nobody can locate.
    indices = [run["index"] for run in document["runs"]]
    expected = list(range(1, len(document["runs"]) + 1))
    if indices != expected:
        raise HandoffError(
            f"{what} numbers its runs {indices} where "
            f"`probe_flake.measure` emits {expected}; the loop runs "
            f"`range(1, runs + 1)` and appends one record per index, so a "
            f"sequence that repeats, skips or reorders is not a record of "
            f"{len(expected)} runs")
    broken = document.get("error_run")
    if isinstance(broken, dict) and broken["index"] != len(expected) + 1:
        raise HandoffError(
            f"{what} numbers its harness-error run {broken['index']} where "
            f"the loop was on {len(expected) + 1}; the run that broke the "
            f"stream is the one after the last completed one")


def _require_retention(document, what: str) -> None:
    """Successful runs kept nothing; unsuccessful ones kept everything.

    `probe_flake.measure` deletes a run's directory the moment the run
    passes and records `artifact_dir: null` for it, and retains the
    directory of every FAIL, TIMEOUT and harness-error run. So the
    pairing is exact in both directions, and `retained_artifacts` is
    literally the list of the non-null ones.

    Both halves matter to this workflow, which is why they are checked
    rather than assumed. "No successful-run raw artifacts remain" is one
    of verification's own success conditions; and a FAILING run whose
    directory has gone is a failure nobody can diagnose, which is the
    evidence #1438 and #1439 are handed. Neither shape can be produced
    by the shipped harness, so both are malformed input rather than a
    measurement that went wrong.
    """
    records = [(f"{what}.runs[{position}]", run)
               for position, run in enumerate(document["runs"])]
    broken = document.get("error_run")
    if isinstance(broken, dict):
        records.append((f"{what}.error_run", broken))
    retained = []
    for where, run in records:
        directory = run.get("artifact_dir")
        if run.get("outcome") == probe_flake.RUN_PASS:
            if directory is not None:
                raise HandoffError(
                    f"{where} passed and still names the artifact directory "
                    f"{directory}; a successful run's raw artifacts are "
                    f"deleted as soon as it passes, so a passing run that "
                    f"kept its directory was not written by the harness — "
                    f"and leaving successful-run artifacts behind is exactly "
                    f"what verification may not do")
            continue
        if directory is None:
            raise HandoffError(
                f"{where} did not pass ({run.get('outcome')!r}) and names no "
                f"artifact directory; every unsuccessful run's artifacts are "
                f"retained, and a failure whose logs are gone is a failure "
                f"nobody can diagnose")
        retained.append(directory)
    declared = document.get("retained_artifacts") or []
    if list(declared) != retained:
        raise HandoffError(
            f"{what}.retained_artifacts is {list(declared)} where its own "
            f"runs retained {retained}; the list is exactly the non-null "
            f"artifact directories in run order, so a document where the two "
            f"disagree is naming evidence it does not have, or hiding "
            f"evidence it does")


def descriptor_ids(document) -> list:
    return [entry["id"] for entry in document["checks"]]


def controlled_problems(document, *, what: str) -> list:
    """Why `document` is not a usable controlled measurement, if it isn't.

    Reported rather than raised, because "the batch became invalid" is a
    declared OUTCOME of this workflow and not a malformed input. The
    issue's own list — "verification remains above X, contains any
    MISSING result, becomes invalid, or only partially improves the
    rate" — routes every entry to #1439, so a harness error in the
    verification batch must reach `partial-improvement` with its
    evidence retained, never a gate rejection that reports nothing.

    Four conditions: the measurement did not abort, it is the policy's
    own size, it ran at the fixed capability count, and nothing else ran
    beside it. The last matters because an overlapping flake-harness
    invocation shares the machine, and a comparison made against a
    contended side is not controlled.

    Structural validity is NOT here — `require_result` raises for that,
    because a document that is not a `probe-flake-result/v1` at all is a
    malformed input rather than a measurement that went wrong.
    """
    problems = []
    if document.get("status") != "ok":
        problems.append(
            f"{what} has status {document.get('status')!r}; a harness error "
            f"has no trustworthy failure rate at all, so it is not a "
            f"controlled comparison side")
    requested = document.get("requested_runs")
    completed = document.get("completed_runs")
    if requested != RUN_COUNT:
        problems.append(
            f"{what} requested {requested!r} runs; this lab's measurement "
            f"contract is exactly {RUN_COUNT}, which is the basis X is "
            f"stated on")
    if completed != requested:
        problems.append(
            f"{what} completed {completed!r} of {requested!r} runs; an "
            f"incomplete batch is not a measurement")
    if len(document["runs"]) != completed:
        problems.append(
            f"{what} reports {completed!r} completed runs but carries "
            f"{len(document['runs'])} run record(s)")
    caps = document.get("rts_capabilities")
    if caps != RTS_CAPABILITIES:
        problems.append(
            f"{what} ran at {caps!r} RTS capabilities; the fixed condition "
            f"is {RTS_CAPABILITIES}, and an unpinned run measures a "
            f"different condition on every machine")
    peak = document.get("peak_concurrency")
    if peak != 1:
        problems.append(
            f"{what} observed peak concurrency {peak!r}; another flake "
            f"harness invocation overlapped this measurement")
    return problems


def descriptor_of(document) -> list:
    """The document's ordered descriptor, identifiers AND labels."""
    return [{"id": entry["id"], "label": entry["label"]}
            for entry in document["checks"]]


def require_descriptor(document, expected, what: str) -> None:
    """The whole declared descriptor, which a route may never reinterpret.

    Identifiers, their order, AND their labels. The label is the check's
    stated MEANING — `probe_protocol.build_descriptor` carries it as the
    human-readable half of the contract — so a verification that kept
    every identifier while relabelling one to describe a different
    assertion has changed what the batch measures and reported the
    change nowhere.

    A rename, a removal, a reorder or a relabel is a REJECTION and not a
    #1439 outcome: each is an attempt to change the reporting protocol,
    which needs a separately approved mapping this issue does not invent.
    """
    if expected is None:
        return
    found = descriptor_of(document)
    if list(expected) == found:
        return
    ids, expected_ids = descriptor_ids(document), [e["id"] for e in expected]
    if ids != expected_ids:
        raise HandoffError(
            f"{what} declares the checks {ids} where the handoff's expected "
            f"descriptor is {expected_ids}; identifiers and their order are "
            f"the stable contract, and a rename or removal is a separately "
            f"approved protocol change this issue does not invent a mapping "
            f"for")
    changed = [f"{e['id']}: {e['label']!r} -> {f['label']!r}"
               for e, f in zip(expected, found) if e["label"] != f["label"]]
    raise HandoffError(
        f"{what} relabels {'; '.join(changed)}; a label is the check's "
        f"stated MEANING, so keeping the identifier while describing a "
        f"different assertion changes what the batch measures and says so "
        f"nowhere")


def require_controlled(document, *, what: str, expected=None) -> dict:
    """A usable controlled measurement, or the refusal that names why.

    Used where an unusable measurement really IS a malformed input: the
    handoff's own result document, whose invalidity means the handoff
    "omits required evidence" and stops the invocation at the gate.
    """
    require_result(document, what)
    problems = controlled_problems(document, what=what)
    if problems:
        raise HandoffError(problems[0])
    require_descriptor(document, expected, what)
    return document


def non_pass_ids(document) -> list:
    """Every declared identifier that was ever FAIL or MISSING."""
    ids = descriptor_ids(document)
    seen = set()
    for run in document["runs"]:
        for cid, result in run["checks"].items():
            if result != probe_protocol.PASS:
                seen.add(cid)
    return [cid for cid in ids if cid in seen]


def failure_count(document) -> int:
    """Runs that failed or timed out — the quantity X is stated against."""
    return sum(1 for run in document["runs"]
               if run["outcome"] in (probe_flake.RUN_FAIL,
                                     probe_flake.RUN_TIMEOUT))


def missing_problems(document, *, targets, what: str) -> list:
    """Every violation of the scoped MISSING rule in one batch.

    **A target check has zero MISSING across all ten runs.** That is the
    approved correction, applied as written. The suffix allowance below
    is for the checks that are NOT targets: an accepted failing run may
    abort, and everything after its abort point is MISSING, but it may
    not abort before a target — because a run that never reached the
    target did not demonstrate the target was fixed.

    The consequence is worth naming rather than discovering. The targets
    are every non-PASS identifier of the handoff's measurement, and for
    a probe that ABORTS those form a suffix of the descriptor — so a
    verification's accepted failing runs must not abort before the end.
    That is restrictive for an X above zero; it is not unsatisfiable (a
    run that FAILs its last check, or that reports a failed check and
    keeps going, emits everything), and it is what the contract says.
    Relaxing it needs an approved spec change, not a reading.

    The other three: no MISSING at all in a run that PASSED; MISSING in a
    failing run must be a contiguous suffix of the declared order, which
    is what "after the abort point" MEANS and is checked rather than
    assumed even though `probe_protocol` enforces declared order; and no
    identifier may vanish from the batch as a whole.
    """
    ids = descriptor_ids(document)
    order = {cid: position for position, cid in enumerate(ids)}
    problems = []
    emitted = set()
    for run in document["runs"]:
        index = run.get("index")
        results = run["checks"]
        emitted |= {cid for cid, result in results.items()
                    if result != probe_protocol.MISSING}
        missing = sorted((cid for cid, result in results.items()
                          if result == probe_protocol.MISSING),
                         key=order.__getitem__)
        for cid in missing:
            if cid in targets:
                problems.append(
                    f"{what} run {index} reports the target check {cid!r} as "
                    f"MISSING; a target has zero MISSING across all "
                    f"{len(document['runs'])} runs, because a run that never "
                    f"reached it did not demonstrate it was fixed — and a "
                    f"target that stops being emitted has not been fixed, it "
                    f"has stopped being measured")
        if not missing:
            continue
        if run["outcome"] == probe_flake.RUN_PASS:
            problems.append(
                f"{what} run {index} passed while omitting "
                f"{', '.join(missing)}; a run that reached the end must "
                f"emit every expected check")
            continue
        positions = [order[cid] for cid in missing]
        if positions != list(range(min(positions), len(ids))):
            problems.append(
                f"{what} run {index} omits {', '.join(missing)}, which is "
                f"not a contiguous suffix of the declared order; an aborted "
                f"run loses everything after its abort point and nothing "
                f"before it, so this is a malformed result rather than an "
                f"abort")
    vanished = [cid for cid in ids if cid not in emitted]
    if vanished:
        problems.append(
            f"{what} never emitted {', '.join(vanished)} in any of its "
            f"{len(document['runs'])} runs; an identifier that disappears "
            f"from the batch as a whole has been removed from the "
            f"measurement, whatever any single run did")
    return problems


# ==========================================================================
# The handoff
# ==========================================================================
class Handoff:
    """One complete #1436 measurement handoff, validated.

    The probe key, X, the ordered expected descriptor, the observed
    non-PASS identifiers, the exact invocation, the configuration
    manifest and the retained artifacts. Exactly one probe: the
    workflow's whole bound is one invocation, one probe, one root cause.
    """

    def __init__(self, document, *, acceptable_failures: int):
        self.document = document
        self.probe = document["probe"]
        self.acceptable_failures = acceptable_failures
        self.result = document["result"]
        self.invocation = document["invocation"]
        self.configuration = document["configuration"]
        self.expected_checks = descriptor_ids(self.result)
        self.expected_descriptor = descriptor_of(self.result)
        self.targets = tuple(document["targets"])
        self.artifacts = list(document.get("artifacts") or [])

    @property
    def commit_sha(self) -> str:
        return self.result["commit_sha"]


def _is_string_list(value) -> bool:
    """A list of non-empty strings, and nothing else.

    Checked BEFORE anything iterates: `list(42)` raises `TypeError`, and
    a malformed document must reach a caller as this module's own
    controlled refusal rather than as a traceback out of the CLI.
    """
    return (isinstance(value, list)
            and all(isinstance(item, str) and item for item in value))


def require_handoff(document, *, worktrees=(), primary=None) -> Handoff:
    """`document` as a usable handoff, or the entry gate's refusal.

    Every refusal here is `handoff-rejected`: the invocation stops
    without changing code and without opening a PR, and it is NOT
    recorded as a diagnosis outcome, because no diagnosis happened.
    """
    what = "handoff"
    if not isinstance(document, dict):
        raise HandoffError(
            f"{what} must be a JSON object, got {type(document).__name__}")
    schema = document.get("schema")
    if schema != HANDOFF_SCHEMA:
        raise HandoffError(f"{what} is {schema!r}, expected {HANDOFF_SCHEMA!r}")

    probe = document.get("probe")
    if isinstance(probe, list):
        raise HandoffError(
            f"{what} names {len(probe)} probes ({probe!r}); one invocation "
            f"diagnoses exactly one probe, so several candidates are "
            f"several invocations")
    if not isinstance(probe, str) or not probe:
        raise HandoffError(f"{what} names no probe")
    registered = probe_flake.registered_scripts()
    if probe not in registered:
        raise HandoffError(
            f"{what} names {probe!r}, which is not a registered probe key "
            f"in `tools/run_probes.py`")
    if probe_flake.protocol_status(probe) != probe_protocol.PROTOCOL_VERSION:
        raise HandoffError(
            f"{what} names {probe!r}, which does not implement "
            f"{probe_protocol.PROTOCOL_VERSION}; a probe with no descriptor "
            f"has no per-check evidence to diagnose")

    try:
        acceptable = probe_census.require_acceptable_failures(
            document.get("acceptable_failures"),
            f"{what}'s `acceptable_failures`")
    except probe_census.CensusError as error:
        raise HandoffError(
            f"{str(error)}; X is #1430's validated integer out of "
            f"{RUN_COUNT} runs and this workflow neither invents nor "
            f"changes it") from None

    result = document.get("result")
    if not isinstance(result, dict):
        raise HandoffError(
            f"{what} carries no embedded {probe_flake.RESULT_SCHEMA!r} "
            f"document. The durable census row is not a substitute: "
            f"`probe_census.ingest_result` drops the ports, the per-run "
            f"check maps, the descriptor labels, the artifact root, the "
            f"invocation directory and the exact command, so a handoff "
            f"rebuilt from it cannot identify the baseline invocation")
    require_controlled(result, what=f"{what}.result")
    if result["probe"] != probe:
        raise HandoffError(
            f"{what} names {probe!r} but its result document measured "
            f"{result['probe']!r}")

    expected = document.get("expected_checks")
    if expected is not None and not _is_string_list(expected):
        raise HandoffError(
            f"{what}.expected_checks must be a list of identifiers, got "
            f"{expected!r}")
    if expected is not None and list(expected) != descriptor_ids(result):
        raise HandoffError(
            f"{what}.expected_checks is {list(expected)} where its own "
            f"result document declares {descriptor_ids(result)}; the "
            f"descriptor is the ordered contract and the two cannot "
            f"disagree")

    # The targets are not a SELECTION from the measurement, they ARE it:
    # the issue's entry gate says every non-PASS identifier for the probe
    # is a diagnosis input, so a handoff that named a subset would let a
    # repair be declared verified while another observed failure was
    # quietly left out of scope. Equality, in the descriptor's own order.
    observed = non_pass_ids(result)
    targets = document.get("targets")
    if targets is not None and not _is_string_list(targets):
        raise HandoffError(
            f"{what}.targets must be a list of identifiers, got {targets!r}")
    if not isinstance(targets, list) or not targets:
        raise HandoffError(
            f"{what} names no target check identifiers; every non-PASS "
            f"identifier for this probe is a diagnosis input, and this "
            f"measurement observed {', '.join(observed) or 'none'}")
    if len(set(targets)) != len(targets):
        raise HandoffError(f"{what}.targets repeats an identifier: {targets}")
    if list(targets) != observed:
        unknown = [cid for cid in targets
                   if cid not in descriptor_ids(result)]
        if unknown:
            raise HandoffError(
                f"{what} targets identifiers the descriptor never declared: "
                f"{', '.join(unknown)}")
        raise HandoffError(
            f"{what}.targets is {list(targets)} where its own measurement's "
            f"non-PASS identifiers are {observed}, in that order. All of "
            f"them are diagnosis inputs: naming a subset would let a repair "
            f"be verified while another observed failure stayed out of "
            f"scope, and naming a check that never went non-PASS targets "
            f"something this measurement did not see")

    # `/deflake` runs in the PRIMARY checkout — it is the step BEFORE this
    # workflow creates its comparison worktrees, it claims a probe and
    # writes the census from there, and CLAUDE.md's working-tree
    # discipline puts that class of tool nowhere else. Binding the record
    # only to the directory it claims would accept
    # `/tmp/not-a-synarchy-checkout` as a checkout, which is the one thing
    # a path cannot assert about itself.
    require_invocation(document.get("invocation"), f"{what}.invocation",
                       launcher=DEFLAKE_LAUNCHER)
    directory = document["invocation"]["directory"]
    if primary is not None and not (_path_forms(directory)
                                    & _path_forms(primary)):
        raise HandoffError(
            f"{what}.invocation ran in {directory}, which is not the primary "
            f"checkout {primary}; `/deflake` runs there — before this "
            f"workflow's comparison worktrees exist — so a handoff from "
            f"anywhere else names a checkout nothing has established is one")
    settings = effective_settings(document["invocation"],
                                  f"{what}.invocation", result=result)
    if settings["probe"] != probe:
        raise HandoffError(
            f"{what}.invocation measured {settings['probe']!r} where the "
            f"handoff names {probe!r}")
    if settings["runs"] != RUN_COUNT:
        raise HandoffError(
            f"{what}.invocation requested {settings['runs']!r} runs; the "
            f"measurement contract is exactly {RUN_COUNT}")
    if settings["rts_caps"] != RTS_CAPABILITIES:
        raise HandoffError(
            f"{what}.invocation ran at {settings['rts_caps']!r} RTS "
            f"capabilities; the fixed condition is {RTS_CAPABILITIES}")

    require_manifest(document.get("configuration"), f"{what}.configuration")

    artifacts = document.get("artifacts")
    if artifacts is not None and (not isinstance(artifacts, list)
                                  or not all(isinstance(path, str)
                                             for path in artifacts)):
        raise HandoffError(f"{what}.artifacts must be a list of paths")
    _check_paths(what=what, invocation=document["invocation"], result=result,
                 extra=artifacts or (), trees=worktrees)
    # No "failing runs must have retained evidence" rule here: since
    # `_require_retention`, `retained_artifacts` IS the list of the
    # unsuccessful runs' own directories, so a measurement with failures
    # and no retained evidence is unrepresentable rather than merely
    # refused. `artifacts` stays a place for a handoff to name anything
    # further it kept.
    return Handoff(document, acceptable_failures=acceptable)


# ==========================================================================
# The diagnosis
# ==========================================================================
class Outcome:
    """One diagnosis invocation's route and everything it must report."""

    def __init__(self, route: str, *, probe=None, detail: str = "",
                 owner_issue=None, opens_pull_request: bool = False,
                 targets=(), baseline_failures=None,
                 verification_failures=None, acceptable_failures=None,
                 artifacts=(), notes=(), handoff=None):
        self.handoff = handoff
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

    def to_document(self) -> dict:
        return {
            "schema": OUTCOME_SCHEMA,
            "route": self.route,
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
        }


def _require_canonical(path, what: str) -> None:
    """A path exactly as `Path.resolve` would have serialized it.

    `probe_flake.check_artifact_root` calls `Path.resolve` on its root
    before a run begins and every other path is built from it, so a real
    result document carries fully RESOLVED absolute paths — no `.`, no
    `..`, no doubled separator, no trailing slash, and no unresolved
    symlink. A lexical `normpath` check alone would accept
    `/tmp/evidence/forged/../artifacts/...`, which the harness could not
    have written, and on a host where `/tmp` links to `/private/tmp` it
    would call two different places the same one.
    """
    if not isinstance(path, str) or not path:
        raise HandoffError(f"{what} is not a path ({path!r})")
    if not Path(path).is_absolute():
        raise HandoffError(
            f"{what} is the relative path {path!r}; `check_artifact_root` "
            f"resolves its root before a run begins, so every path a real "
            f"result document carries is absolute")
    resolved = str(Path(path).resolve())
    if resolved != path:
        raise HandoffError(
            f"{what} is {path!r}, which is not the spelling "
            f"`Path.resolve` produces ({resolved!r}); "
            f"`check_artifact_root` RESOLVES its root, so a real document's "
            f"paths carry no `.`, no `..`, no doubled separator, no trailing "
            f"slash AND no unresolved symlink — on this host `/tmp` is a "
            f"link to `/private/tmp`, and a lexical check alone would call "
            f"two different places the same one")


def require_topology(document, what: str) -> None:
    """The artifact layout `probe_flake.measure` actually creates.

    `new_invocation_dir` puts the invocation directory DIRECTLY under the
    artifact root and names it after the probe, and every run directory
    is `invocation_dir / f"run-{index:03d}"` — the run's own index, so
    the whole layout is determined by three recorded values and nothing
    is free.

    Checking only "is it inside a worktree" left a batch able to replace
    a failed run's directory with an unrelated external path and keep
    `repair-pr`, though no harness run could produce that layout. Naming
    the exact path also makes the run directories unique by
    construction, which a uniqueness test would only approximate.
    """
    for key in ("artifact_root", "invocation_dir"):
        _require_canonical(document[key], f"{what}.{key}")
    root = Path(document["artifact_root"])
    invocation = Path(document["invocation_dir"])
    if invocation.parent != root:
        raise HandoffError(
            f"{what} reports the invocation directory {invocation} under the "
            f"artifact root {root}; `probe_flake.new_invocation_dir` creates "
            f"it as a DIRECT child of the root, so this pair was not "
            f"produced by a harness run")
    generated = INVOCATION_DIR_RE.fullmatch(invocation.name)
    problem = None
    if generated is None:
        problem = "it is not the generated shape"
    elif generated["probe"] != document["probe"]:
        problem = f"it names the probe {generated['probe']!r}"
    else:
        try:
            datetime.strptime(generated["stamp"], INVOCATION_STAMP_FORMAT)
        except ValueError:
            problem = f"{generated['stamp']!r} is not a real UTC instant"
        if problem is None and int(generated["pid"]) <= 0:
            problem = f"{generated['pid']!r} is not a process id"
    if problem is not None:
        raise HandoffError(
            f"{what} reports the invocation directory {invocation.name!r}: "
            f"{problem}. `probe_flake.new_invocation_dir` generates "
            f"`{{probe}}-{{%Y%m%dT%H%M%SZ}}-{{pid}}-{{uuid8}}` from a real "
            f"clock and a real process, so this is not a directory this "
            f"measurement created")
    records = [(f"{what}.runs[{position}]", run)
               for position, run in enumerate(document["runs"])]
    broken = document.get("error_run")
    if isinstance(broken, dict):
        records.append((f"{what}.error_run", broken))
    for where, run in records:
        directory = run.get("artifact_dir")
        if directory is None:
            continue
        _require_canonical(directory, where)
        expected = invocation / f"run-{run['index']:03d}"
        if Path(directory) != expected:
            raise HandoffError(
                f"{where} reports the artifact directory {directory} where "
                f"run {run['index']} of this invocation would be at "
                f"{expected}; every run directory is "
                f"`invocation_dir / f\"run-{{index:03d}}\"`, so a path "
                f"anywhere else is evidence from somewhere other than this "
                f"measurement")


def result_paths(document) -> list:
    """Every filesystem path a result document itself names.

    The invocation's `--result` and `--artifact-root` say where a batch
    was TOLD to write; these say where it reports having written. Both
    have to stay outside every worktree, and checking only the first
    would accept a document whose own artifact root is inside the repair
    worktree while its separately recorded command names an external one.
    """
    paths = []
    for key in ("artifact_root", "invocation_dir"):
        value = document.get(key)
        if isinstance(value, str) and value:
            paths.append((key, value))
    for position, run in enumerate(document["runs"]):
        value = run.get("artifact_dir")
        if isinstance(value, str) and value:
            paths.append((f"runs[{position}].artifact_dir", value))
    broken = document.get("error_run")
    if isinstance(broken, dict):
        value = broken.get("artifact_dir")
        if isinstance(value, str) and value:
            paths.append(("error_run.artifact_dir", value))
    for position, value in enumerate(document.get("retained_artifacts") or []):
        if isinstance(value, str) and value:
            paths.append((f"retained_artifacts[{position}]", value))
    return paths


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


def _check_paths(*, what: str, invocation, result, extra=(), trees) -> None:
    """Nothing this measurement touched may sit in a repository worktree.

    Shared by the handoff and by both controlled batches, because the
    rule does not care which of the three produced the evidence: a
    handoff whose result tree was rewritten under a comparison worktree
    is exactly as unusable as a verification that wrote into one.
    """
    base = invocation["directory"]
    checked = [(f"{what} wrote {path}", path)
               for path in destinations(invocation)]
    checked += [(f"{what}.result reports {label} at {path}", path)
                for label, path in result_paths(result)]
    checked += [(f"{what} retained {path}", path) for path in extra]
    for description, path in checked:
        tree = inside_any_worktree(path, trees, base=base)
        if tree is not None:
            raise HandoffError(
                f"{description}, inside the working tree {tree}; a "
                f"measurement's result document and raw artifacts must all "
                f"stay outside every worktree, or they enter a commit or "
                f"wedge the drainer's cleanup")


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

    _launcher, options = parse_command(invocation["command"],
                                       f"{what}.invocation.command")
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
    return controlled_problems(result, what=f"{what}.result")


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

    owner = ROUTE_OWNER[route]
    # Every retained artifact this invocation has, from every batch it
    # ran — not just the handoff's. A #1439 route exists to hand the
    # evidence on, and the batch that went wrong is usually the
    # VERIFICATION, whose logs would otherwise never be named at all.
    artifacts = list(handoff.artifacts) + list(
        handoff.result.get("retained_artifacts") or [])
    common = {
        "handoff": handoff,
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

    config_problems = manifest_differences(
        handoff.configuration, baseline_section["configuration"],
        left_name="the handoff", right_name="the clean comparison worktree")
    if config_problems:
        raise RouteRefused(
            "the controlled baseline did not reproduce the handoff's "
            "configuration state, so it is not the same condition: "
            + "; ".join(config_problems))

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
                + "; ".join(baseline_invalid)), **common)
        if reproduced == probe_census.TOLERANCE_OVER and hit:
            raise RouteRefused(
                f"the controlled baseline DID reproduce an over-tolerance "
                f"result ({baseline_failures}/{RUN_COUNT} against an X of "
                f"{handoff.acceptable_failures}) with the target check(s) "
                f"{', '.join(hit)} non-PASS, so {ROUTE_CANNOT_REPRODUCE!r} "
                f"is not this measurement's outcome")
        return Outcome(route, detail=(
            f"the controlled baseline observed {baseline_failures}/"
            f"{RUN_COUNT} failures against an X of "
            f"{handoff.acceptable_failures}"), **common)

    if reproduced != probe_census.TOLERANCE_OVER:
        raise RouteRefused(
            f"the controlled baseline observed {baseline_failures}/"
            f"{RUN_COUNT} failures against an X of "
            f"{handoff.acceptable_failures} ({reproduced}); a repair may "
            f"only proceed from a baseline that exceeds X, so this is the "
            f"{ROUTE_CANNOT_REPRODUCE!r} outcome for #1439")
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
                       **common)

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
    problems = verification_invalid + comparability + missing_problems(
        verification, targets=set(handoff.targets),
        what="the verification batch")
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
        reason = ("; ".join(problems) if problems else
                  f"stayed above the X of {handoff.acceptable_failures}")
        return Outcome(route, detail=(
            f"the verification batch went from {baseline_failures} to "
            f"{verification_failures} failures out of {RUN_COUNT} but "
            f"{reason}"), **common)

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
        f"{handoff.acceptable_failures}"), **common)


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
    commit = repair.get("commit_sha")
    if not isinstance(commit, str) or not COMMIT_RE.match(commit):
        raise HandoffError(
            f"the repair names no resolved commit ({commit!r}); the repair "
            f"is committed and frozen BEFORE it is verified")
    base = repair.get("base_sha")
    if not isinstance(base, str) or not COMMIT_RE.match(base):
        raise HandoffError(
            f"the repair names no resolved base commit ({base!r}); the "
            f"repair worktree is created from the SAME SHA as the clean "
            f"comparison worktree, and a repair whose lineage is unstated "
            f"cannot be shown to share one")
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
