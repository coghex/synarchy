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
`build-test` job against the set run by `tools/ci-local.sh`, in both
directions, minus a hard-coded exemption list. An invocation is compared
at COMMAND granularity INCLUDING ARGUMENTS, so `ci_probes.py --self-test`
and `ci_probes.py --stdin` are two different checks, and retuning an
existing check's flags on one side alone is drift.

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
  by the YAML parser, shell comments by this module's own lexer. A
  commented-out check does not run, so it must not count as running.
* **Other jobs.** Only `build-test` is inspected. `resolve-image` builds
  the CI image and has no local counterpart by construction.

Failing loudly rather than silently
-----------------------------------
The real hazard for an audit like this is a shape its extractor does not
recognise: a missed invocation reads as parity that is not there. So the
extractor refuses rather than shrugs. A `python` interpreter appearing
anywhere other than as the head of a command (`xargs python3 ...`), a
`tools/*.py` script executed directly instead of through an interpreter,
an unterminated quote or `$(`/`${`, a missing `build-test` job, and an
empty invocation set on either side are each an error naming the offending
text — never a quietly smaller comparison.

Usage:
  python3 tools/ci_parity_audit.py
  python3 tools/ci_parity_audit.py --self-test
Exit codes: 0 = the two gate sets agree, 1 = they do not (or a self-test
check failed, or the input could not be parsed).
"""
from __future__ import annotations

import argparse
import re
import shlex
from pathlib import Path

try:
    import yaml  # type: ignore
except ImportError:  # pragma: no cover - exercised only on a bare toolchain
    raise SystemExit(
        "ci_parity_audit.py needs PyYAML to read .github/workflows/ci.yml.\n"
        "Install the pinned toolchain:\n"
        "    python3 -m pip install --user -r tools/requirements-assets.txt\n"
        "(PyYAML is already required by tools/pack_atlas.py, which `make ci` "
        "and CI both run, so this adds no new dependency.)")

REPO_ROOT = Path(__file__).resolve().parent.parent
WORKFLOW_PATH = REPO_ROOT / ".github" / "workflows" / "ci.yml"
LOCAL_GATE_PATH = REPO_ROOT / "tools" / "ci-local.sh"

#: The one workflow job whose gate set `make ci` mirrors.
AUDITED_JOB = "build-test"

WORKFLOW_LABEL = ".github/workflows/ci.yml (job: %s)" % AUDITED_JOB
LOCAL_GATE_LABEL = "tools/ci-local.sh"

# Invocations CI runs that `make ci` deliberately does not, each keyed on
# the EXACT command CI runs so a neighbouring form is not exempted with it.
# `--self-test` proves each entry is accepted, and the repository run
# proves each is still live: an entry matching nothing on either side is a
# stale exemption and fails, so this list cannot outlive its reason.
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
        "python3 tools/ci_probes.py --stdin",
        "CI path-selection orchestration for the behaviour-probe gate: "
        "picks which CI-eligible probes a change needs. `make ci` runs no "
        "behaviour probes at all (they are an opt-in tier, see CLAUDE.md), "
        "so it has nothing to select. Its --self-test form is NOT exempt "
        "and does run locally.",
    ),
)

# Scripts exempt however they are invoked, because the exemption is a
# property of the script rather than of one argument list.
EXEMPT_SCRIPTS: tuple[tuple[str, str], ...] = (
    (
        "tools/run_probes.py",
        "Boots real engines. The probe sweep costs minutes to tens of "
        "minutes, and CLAUDE.md's testing tiers keep it opt-in rather than "
        "part of any default gate. It runs locally perfectly well — it is "
        "excluded for that cost and that tier contract, not because it "
        "cannot. Exempt by script rather than by exact command so retuning "
        "CI's --jobs/--retries does not fail this audit.",
    ),
)

_INTERPRETER_RE = re.compile(r"python[0-9]*(?:\.[0-9]+)*")
_TOOLS_SCRIPT_RE = re.compile(r"tools/[A-Za-z0-9_.\-/]+\.py")

# Unquoted characters that end one command and begin another (or end it
# outright, in the case of a redirection target).
_COMMAND_BREAKS = set("|&;\n(){}<>")


class AuditError(Exception):
    """A parse the audit refuses to guess its way past."""


def _read_delimited(text: str, start: int, open_ch: str, close_ch: str,
                    where: str) -> tuple[str, int]:
    """Read a balanced `open_ch`/`close_ch` body whose opener is consumed.

    Returns the body and the index just past the matching closer. Quoting
    inside the body is respected so a delimiter in a string does not
    unbalance the scan.
    """
    depth = 1
    i = start
    n = len(text)
    quote: str | None = None
    while i < n:
        ch = text[i]
        if quote == "'":
            if ch == "'":
                quote = None
            i += 1
            continue
        if ch == "\\" and i + 1 < n:
            i += 2
            continue
        if quote == '"':
            if ch == '"':
                quote = None
            i += 1
            continue
        if ch == "'":
            quote = "'"
            i += 1
            continue
        if ch == '"':
            quote = '"'
            i += 1
            continue
        if ch == open_ch:
            depth += 1
        elif ch == close_ch:
            depth -= 1
            if depth == 0:
                return text[start:i], i + 1
        i += 1
    raise AuditError(
        f"{where}: unterminated {open_ch}...{close_ch} starting at offset "
        f"{start - 1}; the audit will not guess where the command ends.")


def split_shell_commands(text: str, where: str) -> list[str]:
    """Split shell text into individual command segments.

    Quote-, comment- and substitution-aware: shell comments are dropped,
    `$(...)` bodies are lifted out and split in their own right (so a
    command hidden inside a substitution is still seen), and `${...}` is
    consumed whole so a `${{ ... }}` workflow expression cannot look like
    a brace group.
    """
    segments: list[str] = []
    current: list[str] = []
    quote: str | None = None
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if quote == "'":
            current.append(ch)
            if ch == "'":
                quote = None
            i += 1
            continue
        if ch == "\\" and i + 1 < n:
            # Kept verbatim (line continuations included): shlex re-reads it.
            current.append(ch)
            current.append(text[i + 1])
            i += 2
            continue
        if ch == "$" and text.startswith("$(", i):
            inner, i = _read_delimited(text, i + 2, "(", ")", where)
            segments.extend(split_shell_commands(inner, where))
            current.append(" ")
            continue
        if ch == "$" and text.startswith("${", i):
            body, i = _read_delimited(text, i + 2, "{", "}", where)
            current.append("${" + body + "}")
            continue
        if quote == '"':
            current.append(ch)
            if ch == '"':
                quote = None
            i += 1
            continue
        if ch == '"':
            quote = '"'
            current.append(ch)
            i += 1
            continue
        if ch == "'":
            quote = "'"
            current.append(ch)
            i += 1
            continue
        if ch == "#" and (not current or current[-1].isspace()):
            newline = text.find("\n", i)
            i = n if newline == -1 else newline
            continue
        if ch in _COMMAND_BREAKS:
            segments.append("".join(current))
            current = []
            i += 1
            continue
        current.append(ch)
        i += 1
    if quote is not None:
        raise AuditError(
            f"{where}: unterminated {quote} quote; the audit will not guess "
            "where the command ends.")
    segments.append("".join(current))
    return segments


def _is_interpreter(token: str) -> bool:
    """True for `python`, `python3`, `python3.12`, and path-qualified forms.

    Path-qualified so `/usr/bin/python3 tools/x.py` is recognised as an
    invocation rather than quietly ignored; it then compares unequal to a
    bare `python3 tools/x.py`, which is the honest answer — the two files
    would be running the check under different interpreters.
    """
    return _INTERPRETER_RE.fullmatch(token.rsplit("/", 1)[-1]) is not None


def _tools_script(tokens: list[str]) -> str | None:
    for token in tokens:
        if _TOOLS_SCRIPT_RE.fullmatch(token):
            return token
    return None


def extract_invocations(text: str, where: str) -> list[str]:
    """Every `python3 tools/*.py ...` command in `text`, normalized.

    Normalization is `shlex` tokenization rejoined by single spaces, so
    quoting differences between the two files compare equal while
    arguments still compare exactly.
    """
    invocations: list[str] = []
    for segment in split_shell_commands(text, where):
        stripped = segment.strip()
        if not stripped:
            continue
        try:
            tokens = shlex.split(stripped)
        except ValueError as error:
            raise AuditError(
                f"{where}: could not tokenize shell command {stripped!r} "
                f"({error}).") from error
        if not tokens:
            continue
        head = tokens[0]
        if _is_interpreter(head):
            if _tools_script(tokens) is None:
                # `python3 -m pip install ...` and friends: environment
                # preparation, not part of the audited gate set.
                continue
            invocations.append(" ".join(tokens))
            continue
        if any(_is_interpreter(token) for token in tokens):
            raise AuditError(
                f"{where}: a Python interpreter appears somewhere other than "
                f"the head of a command, in {stripped!r}. This audit compares "
                "`python3 tools/*.py` commands; rewrite the step as a plain "
                "invocation, or teach this audit the new shape deliberately.")
        if _TOOLS_SCRIPT_RE.fullmatch(head):
            raise AuditError(
                f"{where}: {head} is executed directly rather than through a "
                f"Python interpreter, in {stripped!r}. This audit compares "
                f"`python3 tools/*.py` commands; invoke it as "
                f"`python3 {head}`.")
    return invocations


#: Shells whose `run:` bodies this module's lexer actually understands.
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


def _script_of(command: str) -> str | None:
    return _tools_script(command.split())


def audit_gate_sets(
    ci_commands: set[str],
    local_commands: set[str],
    exempt_commands: tuple[tuple[str, str], ...] = EXEMPT_COMMANDS,
    exempt_scripts: tuple[tuple[str, str], ...] = EXEMPT_SCRIPTS,
) -> list[str]:
    """Compare two gate sets. Returns a list of human-readable problems."""
    problems: list[str] = []

    for command, reason in exempt_commands:
        if not reason.strip():
            problems.append(
                f"exemption {command!r} carries no stated reason.")
    for script, reason in exempt_scripts:
        if not reason.strip():
            problems.append(
                f"exemption for script {script!r} carries no stated reason.")

    if not ci_commands:
        problems.append(
            f"{WORKFLOW_LABEL} yielded no `python3 tools/*.py` invocations; "
            "an empty gate set cannot certify parity.")
    if not local_commands:
        problems.append(
            f"{LOCAL_GATE_LABEL} yielded no `python3 tools/*.py` invocations; "
            "an empty gate set cannot certify parity.")

    exempt_command_set = {command for command, _ in exempt_commands}
    exempt_script_set = {script for script, _ in exempt_scripts}

    def exempt(command: str) -> bool:
        return (command in exempt_command_set
                or _script_of(command) in exempt_script_set)

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
    for script, _reason in exempt_scripts:
        if not any(_script_of(command) == script for command in everything):
            problems.append(
                f"stale exemption: no side runs `{script}`. Remove the entry "
                "rather than leaving policy that guards nothing.")

    return problems


def run_repository_audit() -> int:
    try:
        ci_commands = workflow_invocations(
            WORKFLOW_PATH.read_text(encoding="utf-8"))
        local_commands = local_gate_invocations(
            LOCAL_GATE_PATH.read_text(encoding="utf-8"))
    except OSError as error:
        print(f"ci_parity_audit.py: {error}")
        return 1
    except AuditError as error:
        print(f"ci_parity_audit.py: {error}")
        return 1

    problems = audit_gate_sets(ci_commands, local_commands)
    if problems:
        print("CI / `make ci` gate-set parity audit FAILED "
              f"({len(problems)} problem(s)):")
        for problem in problems:
            print(f"  {problem}")
        print()
        print("Every `python3 tools/*.py` check the workflow's "
              f"`{AUDITED_JOB}` job runs must also run in "
              f"{LOCAL_GATE_LABEL}, and vice versa, unless it is on this "
              "audit's hard-coded exemption list (see EXEMPT_COMMANDS / "
              "EXEMPT_SCRIPTS, each entry with its reason).")
        return 1

    shared = len(ci_commands & local_commands)
    exempted = len(ci_commands | local_commands) - shared
    print(f"ci_parity_audit.py: {shared} check(s) run identically in "
          f"{WORKFLOW_LABEL} and {LOCAL_GATE_LABEL}; {exempted} exempt.")
    return 0


# --------------------------------------------------------------------------
# Self-test
# --------------------------------------------------------------------------

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
    _expect(failures, audit_gate_sets(ci, local, (), ()) == [],
            "matched gate sets should report no problems, got "
            f"{audit_gate_sets(ci, local, (), ())}")

    # 3. A non-exempt CI-only invocation fails and names the command.
    dropped = local - {"python3 tools/block_two.py --flag value"}
    problems = audit_gate_sets(ci, dropped, (), ())
    _expect(failures,
            any("python3 tools/block_two.py --flag value" in p
                and "not by" in p and LOCAL_GATE_LABEL in p for p in problems),
            "a CI-only invocation should be reported as missing locally, got "
            f"{problems}")

    # 4. A non-exempt local-only invocation fails and names the command.
    added = local | {"python3 tools/local_extra.py"}
    problems = audit_gate_sets(ci, added, (), ())
    _expect(failures,
            any("python3 tools/local_extra.py" in p and WORKFLOW_LABEL in p
                for p in problems),
            "a local-only invocation should be reported as missing in CI, got "
            f"{problems}")

    # 5. Changing an invocation's ARGUMENTS is drift in both directions.
    retuned = (local - {"python3 tools/block_two.py --flag value"}) | {
        "python3 tools/block_two.py --flag other"}
    problems = audit_gate_sets(ci, retuned, (), ())
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
    for script, _reason in EXEMPT_SCRIPTS:
        exempt_side.add(f"python3 {script} --only foo --exact --jobs 2")
    problems = audit_gate_sets(exempt_side, shared)
    _expect(failures, problems == [],
            f"the real exemptions should be accepted, got {problems}")

    # 6b. …and each is load-bearing: without the list, each one is drift.
    problems = audit_gate_sets(exempt_side, shared, (), ())
    for command, _reason in EXEMPT_COMMANDS:
        _expect(failures, any(command in p for p in problems),
                f"exemption {command!r} is not load-bearing: it was not "
                "reported as drift with the list emptied")
    for script, _reason in EXEMPT_SCRIPTS:
        _expect(failures, any(script in p for p in problems),
                f"exemption for {script!r} is not load-bearing: it was not "
                "reported as drift with the list emptied")

    # 7. A stale exemption fails, naming what nothing runs.
    problems = audit_gate_sets(shared, shared)
    for command, _reason in EXEMPT_COMMANDS:
        _expect(failures,
                any("stale exemption" in p and command in p for p in problems),
                f"a stale exemption for {command!r} should be reported, got "
                f"{problems}")
    for script, _reason in EXEMPT_SCRIPTS:
        _expect(failures,
                any("stale exemption" in p and script in p for p in problems),
                f"a stale exemption for {script!r} should be reported, got "
                f"{problems}")

    # 8. Every exemption states a reason.
    for command, reason in EXEMPT_COMMANDS:
        _expect(failures, bool(reason.strip()),
                f"exemption {command!r} has no reason")
    for script, reason in EXEMPT_SCRIPTS:
        _expect(failures, bool(reason.strip()),
                f"exemption for {script!r} has no reason")
    _expect(failures,
            audit_gate_sets(shared | {"python3 tools/x.py"}, shared,
                            (("python3 tools/x.py", "   "),), ()) != [],
            "a blank exemption reason should be reported")

    # 9. Vacuity on either side is a failure, not a pass. Checked with BOTH
    #    sides empty, so drift cannot report the failure on vacuity's
    #    behalf and hide the fact that nothing checks for it.
    problems = audit_gate_sets(set(), set(), (), ())
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

    return failures


def main_self_test() -> int:
    try:
        failures = _self_test()
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
