#!/usr/bin/env python3
"""Save-compat reproducibility wiring (issue #1360).

The gate-set comparison in tools/ci_parity_workflow.py is a SET
comparison: it proves both files run
`test_save_compat_audit.py --only-reproducibility`, and nothing more.
That is not enough for a check that is deliberately CONDITIONAL on
both sides -- each file could run it under a different condition, or
under none, and the set would still match. Requirement 7 of #1360 is
that the two agree about WHEN it runs, so this module pins the
condition itself.

It does that behaviourally on the local side rather than by reading the
shell and believing it: the marked block in tools/ci-local.sh is
EXTRACTED and EXECUTED against synthetic changed-path lists, with
`python3` shimmed so the real selector still decides and the expensive
test is only recorded, never run. On the CI side the guard is a GitHub
expression this audit has no evaluator for, so it is pinned to its
exact canonical text and its provenance is checked -- the output it
reads must be the one the selector step writes from
`--gate save-compat`. Between them, a change that makes either side
select differently fails here.

Why an owner of its own: agreement that a command exists on both sides
is NOT agreement about when it runs, so this check must not be folded
back into the gate-set comparison. It consumes the shell lexer and the
workflow primitives, and nothing consumes it but
`tools/ci_parity_audit.py`.

This module is a library: it has no command line of its own. Run the
audit through `python3 tools/ci_parity_audit.py`.
"""
from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

# `tools/` has no `__init__.py`, so it is an implicit namespace package and
# `import tools.ci_parity_save_compat` from the repository root leaves `tools/`
# itself OFF sys.path. Every sibling import below runs at IMPORT time, so
# this has to come before the first one or that spelling raises
# ModuleNotFoundError while `python3 tools/ci_parity_audit.py` keeps working.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ci_parity_config import (
    LOCAL_GATE_LABEL,
    REPO_ROOT,
    WORKFLOW_LABEL,
)
from ci_parity_shell import AuditError
from ci_parity_workflow import (
    local_gate_invocations,
    normalise_expression,
    workflow_invocations,
    workflow_steps,
)


#: The gate whose decision both files must consult.
SAVE_COMPAT_GATE = "save-compat"

#: The main audit command. Local `make ci` always runs it; CI skips it only
#: for a docs-only range that the same selector proves cannot affect it.
SAVE_COMPAT_WITHOUT_COMMAND = (
    "python3 tools/test_save_compat_audit.py --without-reproducibility")
#: ... the one that must run only when the gate selects ...
SAVE_COMPAT_ONLY_COMMAND = (
    "python3 tools/test_save_compat_audit.py --only-reproducibility")
#: ... and the bare form neither side may run, because it would put the
#: reproducibility member's `cabal repl` back on every pull request.
SAVE_COMPAT_BARE_COMMAND = "python3 tools/test_save_compat_audit.py"

#: The selector invocation both files reach their decision through.
SAVE_COMPAT_SELECTOR_COMMAND = (
    "python3 tools/ci_expensive_gates.py --stdin --gate " + SAVE_COMPAT_GATE)

#: The local-only resolution that feeds it, and the scratch write it
#: must precede. `cabal.project.local` is not gitignored, so a change can
#: track one and CI's gate selects on it; if ci-local.sh resolved its
#: changed paths AFTER writing its own temporary copy, that scratch edit
#: to a tracked file would look like the candidate's. Order, not
#: pattern, is what keeps the two apart -- so the order is checked.
SAVE_COMPAT_LOCAL_PATHS_COMMAND = (
    "python3 tools/ci_expensive_gates.py --local-changed-paths")
SAVE_COMPAT_LOCAL_SCRATCH_WRITE = "printf 'package synarchy"

#: The variable the marked block reads its changed-path list from. The
#: audit exports it when executing that block, and requires the block to
#: consume it rather than re-derive the list (which would put the
#: resolution back after the scratch write).
SAVE_COMPAT_PATHS_VAR = "SAVE_COMPAT_PATHS"

#: The exact guards CI's two Cabal-backed save-compat steps carry. Pinned
#: rather than parsed: `if:` is a GitHub expression, and a half-understood
#: evaluator would be a worse check than an exact match. Non-docs-only
#: master pushes feed every tracked file to the selector, preserving the
#: post-merge backstop; docs-only pushes and PRs use their real range.
SAVE_COMPAT_CI_IF = "steps.expensive-gates.outputs.save-compat == 'true'"
SAVE_COMPAT_AUDIT_CI_IF = (
    "steps.docs-fast-path.outputs.docs_only != 'true' "
    "|| steps.expensive-gates.outputs.save-compat == 'true'")

#: The markers delimiting the executable selection block in ci-local.sh.
LOCAL_BLOCK_BEGIN = ">>> save-compat reproducibility selection >>>"
LOCAL_BLOCK_END = "<<< save-compat reproducibility selection <<<"

#: One changed path that must select the gate, and one that must not.
#: The decision itself comes from ci_expensive_gates.py, whose own
#: --self-test walks the full trigger-path table; these two only have to
#: be unambiguous representatives of each direction.
SAVE_COMPAT_POSITIVE_SAMPLE = ["src/World/Save/Envelope/Codec.hs"]
SAVE_COMPAT_NEGATIVE_SAMPLE = ["scripts/unit_ai.lua"]

_GATES_SCRIPT = REPO_ROOT / "tools" / "ci_expensive_gates.py"


def extract_local_block(shell_text: str,
                        where: str = LOCAL_GATE_LABEL) -> str:
    """The executable selection block delimited by the two markers."""
    begin = shell_text.find(LOCAL_BLOCK_BEGIN)
    end = shell_text.find(LOCAL_BLOCK_END)
    if begin < 0 or end < 0:
        raise AuditError(
            f"{where}: could not find the `{LOCAL_BLOCK_BEGIN}` / "
            f"`{LOCAL_BLOCK_END}` markers around the save-compat "
            "reproducibility selection. This audit executes that block to "
            "prove `make ci` selects the same way CI does; without the "
            "markers there is nothing to execute, and it will not fall "
            "back to trusting the text.")
    if end < begin:
        raise AuditError(
            f"{where}: the block markers are in the wrong order.")
    body_start = shell_text.index("\n", begin) + 1
    return shell_text[body_start:end].rsplit("\n", 1)[0]


# The stand-in `python3` the extracted block runs under. It delegates
# every real selector call to ci_expensive_gates.py -- the decision must
# be the genuine one, or this proves nothing -- serves the synthetic
# changed-path list in place of a git query, and records rather than runs
# the expensive test. Any other command is a block that has grown
# something this audit was not told about, and fails loudly.
_SHIM_TEMPLATE = r'''import subprocess
import sys

argv = sys.argv[1:]
gates = {gates!r}
paths = {paths!r}
marker = {marker!r}

if argv[:2] == ["tools/ci_expensive_gates.py", "--local-changed-paths"]:
    sys.stdout.write("".join(p + "\n" for p in paths))
    sys.exit(0)
if argv[:1] == ["tools/ci_expensive_gates.py"]:
    sys.exit(subprocess.run([sys.executable, gates] + argv[1:]).returncode)
if argv[:1] == ["tools/test_save_compat_audit.py"]:
    with open(marker, "a", encoding="utf-8") as handle:
        handle.write(" ".join(argv[1:]) + "\n")
    sys.exit(0)
sys.stderr.write("unexpected command in the extracted block: %r\n" % (argv,))
sys.exit(3)
'''


def run_local_block(block: str, changed_paths: list[str],
                    gates_script: Path = _GATES_SCRIPT) -> tuple[bool, str]:
    """Execute the extracted block and report whether it ran the member.

    Returns (ran, diagnostic).
    """
    import stat
    import tempfile

    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        marker = root / "ran.txt"
        shim_dir = root / "bin"
        shim_dir.mkdir()
        shim = shim_dir / "python3"
        shim.write_text(
            "#!" + sys.executable + "\n"
            + _SHIM_TEMPLATE.format(gates=str(gates_script),
                                    paths=list(changed_paths),
                                    marker=str(marker)),
            encoding="utf-8")
        shim.chmod(shim.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP
                   | stat.S_IXOTH)
        env = dict(os.environ)
        env["PATH"] = f"{shim_dir}{os.pathsep}{env.get('PATH', '')}"
        # ci-local.sh resolves this at the top of the script, before its
        # scratch cabal.project.local exists; the block only consumes it.
        # Supplied here so the extracted block runs the same way, and so
        # a block that re-derived it instead would still be exercised
        # (the shim answers --local-changed-paths too).
        env[SAVE_COMPAT_PATHS_VAR] = "".join(
            path + "\n" for path in changed_paths)
        proc = subprocess.run(
            ["bash", "-euo", "pipefail", "-c", block], cwd=str(root),
            env=env, capture_output=True, text=True)
        output = (proc.stdout or "") + (proc.stderr or "")
        if proc.returncode != 0:
            return False, (f"the extracted block exited {proc.returncode}: "
                           f"{output.strip()}")
        if not marker.exists():
            return False, output.strip()
        return True, marker.read_text(encoding="utf-8").strip()


def audit_save_compat_reproducibility_wiring(
        yaml_text: str, shell_text: str,
        gates_script: Path = _GATES_SCRIPT) -> list[str]:
    """Check both files select the reproducibility member the same way."""
    if str(gates_script.parent) not in sys.path:
        sys.path.insert(0, str(gates_script.parent))
    try:
        import ci_expensive_gates  # type: ignore
    except ImportError as error:  # pragma: no cover - a broken checkout
        return [f"could not import {gates_script}: {error}"]

    problems: list[str] = []

    # --- CI side ------------------------------------------------------
    steps = workflow_steps(yaml_text)
    guarded = [s for s in steps
               if SAVE_COMPAT_ONLY_COMMAND in str(s.get("run") or "")]
    if len(guarded) != 1:
        problems.append(
            f"{WORKFLOW_LABEL}: expected exactly one step running "
            f"`{SAVE_COMPAT_ONLY_COMMAND}`, found {len(guarded)}.")
    else:
        condition = normalise_expression(str(guarded[0].get("if") or ""))
        if condition != normalise_expression(SAVE_COMPAT_CI_IF):
            problems.append(
                f"{WORKFLOW_LABEL}: the step running "
                f"`{SAVE_COMPAT_ONLY_COMMAND}` is guarded by "
                f"{condition!r}, not by the pinned "
                f"{normalise_expression(SAVE_COMPAT_CI_IF)!r}. Changing "
                "when that coverage runs is a deliberate act: update "
                "SAVE_COMPAT_CI_IF in this audit in the same change.")

    main_audit = [s for s in steps
                  if SAVE_COMPAT_WITHOUT_COMMAND in str(s.get("run") or "")]
    if len(main_audit) != 1:
        problems.append(
            f"{WORKFLOW_LABEL}: expected exactly one step running "
            f"`{SAVE_COMPAT_WITHOUT_COMMAND}`, found {len(main_audit)}.")
    else:
        condition = normalise_expression(str(main_audit[0].get("if") or ""))
        if condition != normalise_expression(SAVE_COMPAT_AUDIT_CI_IF):
            problems.append(
                f"{WORKFLOW_LABEL}: the step running "
                f"`{SAVE_COMPAT_WITHOUT_COMMAND}` is guarded by "
                f"{condition!r}, not by the pinned "
                f"{normalise_expression(SAVE_COMPAT_AUDIT_CI_IF)!r}. The "
                "main audit may skip only an unrelated docs-only range.")

    selector_steps = [
        s for s in steps
        if SAVE_COMPAT_SELECTOR_COMMAND in str(s.get("run") or "")]
    if len(selector_steps) != 1:
        problems.append(
            f"{WORKFLOW_LABEL}: expected exactly one step running "
            f"`{SAVE_COMPAT_SELECTOR_COMMAND}`, found "
            f"{len(selector_steps)}.")
    else:
        step_id = str(selector_steps[0].get("id") or "")
        reference = f"steps.{step_id}.outputs.{SAVE_COMPAT_GATE}"
        if not step_id:
            problems.append(
                f"{WORKFLOW_LABEL}: the step running "
                f"`{SAVE_COMPAT_SELECTOR_COMMAND}` has no `id:`, so no "
                "guard can name its output.")
        elif reference not in normalise_expression(SAVE_COMPAT_CI_IF):
            problems.append(
                f"{WORKFLOW_LABEL}: the pinned guard does not read "
                f"`{reference}`, so the step that decides the gate and "
                "the step that consumes the decision are not connected.")
        if f"{SAVE_COMPAT_GATE}=" not in str(selector_steps[0].get("run")):
            problems.append(
                f"{WORKFLOW_LABEL}: the selector step never writes a "
                f"`{SAVE_COMPAT_GATE}=` output, so its guard reads an "
                "always-empty value and the coverage silently stops "
                "running on pull requests.")

    ci_commands = workflow_invocations(yaml_text)
    local_commands = local_gate_invocations(shell_text)
    for label, commands in ((WORKFLOW_LABEL, ci_commands),
                            (LOCAL_GATE_LABEL, local_commands)):
        if SAVE_COMPAT_WITHOUT_COMMAND not in commands:
            problems.append(
                f"{label}: does not run `{SAVE_COMPAT_WITHOUT_COMMAND}`, "
                "so the main save-compat members are not reachable.")
        if SAVE_COMPAT_ONLY_COMMAND not in commands:
            problems.append(
                f"{label}: does not run `{SAVE_COMPAT_ONLY_COMMAND}`, so "
                "the reproducibility coverage is not reachable at all.")
        if SAVE_COMPAT_BARE_COMMAND in commands:
            problems.append(
                f"{label}: runs the bare `{SAVE_COMPAT_BARE_COMMAND}`, "
                "which puts the reproducibility member's `cabal repl` "
                "back on every run regardless of the selector.")
        if SAVE_COMPAT_SELECTOR_COMMAND not in commands:
            problems.append(
                f"{label}: does not run `{SAVE_COMPAT_SELECTOR_COMMAND}`, "
                "so it is not reaching its decision through the gate the "
                "other side uses.")

    if SAVE_COMPAT_LOCAL_PATHS_COMMAND not in local_commands:
        problems.append(
            f"{LOCAL_GATE_LABEL}: does not run "
            f"`{SAVE_COMPAT_LOCAL_PATHS_COMMAND}`, so it has no local "
            "changed-path list to reach the shared decision with.")
    else:
        resolve_at = shell_text.find(SAVE_COMPAT_LOCAL_PATHS_COMMAND)
        scratch_at = shell_text.find(SAVE_COMPAT_LOCAL_SCRATCH_WRITE)
        if scratch_at < 0:
            problems.append(
                f"{LOCAL_GATE_LABEL}: could not find the scratch "
                f"`{SAVE_COMPAT_LOCAL_SCRATCH_WRITE}...` write, so this "
                "audit cannot check that the changed-path resolution "
                "still precedes it. If that write moved or changed shape, "
                "update SAVE_COMPAT_LOCAL_SCRATCH_WRITE deliberately.")
        elif resolve_at > scratch_at:
            problems.append(
                f"{LOCAL_GATE_LABEL}: resolves its changed paths AFTER "
                "writing its own temporary cabal.project.local. That file "
                "is not gitignored, so once a change tracks one, this "
                "gate's own scratch edit would select the save-compat "
                "gate on every run. Resolve before the write.")
        if SAVE_COMPAT_PATHS_VAR not in shell_text:
            problems.append(
                f"{LOCAL_GATE_LABEL}: the marked block no longer reads "
                f"${SAVE_COMPAT_PATHS_VAR}.")

    # --- local side, executed -----------------------------------------
    try:
        block = extract_local_block(shell_text)
    except AuditError as error:
        problems.append(str(error))
        return problems

    for label, sample in (("a save-touching change",
                           SAVE_COMPAT_POSITIVE_SAMPLE),
                          ("an unrelated change",
                           SAVE_COMPAT_NEGATIVE_SAMPLE)):
        expected = ci_expensive_gates.selected(SAVE_COMPAT_GATE, sample)
        ran, detail = run_local_block(block, sample, gates_script)
        if ran != expected:
            problems.append(
                f"{LOCAL_GATE_LABEL}: for {label} ({sample}) the selector "
                f"says {expected} but the executed block "
                f"{'ran' if ran else 'did not run'} the reproducibility "
                "member. CI's guard consumes the same selector decision, "
                f"so the two would disagree. Detail: {detail!r}")
        elif ran and detail != "--only-reproducibility":
            problems.append(
                f"{LOCAL_GATE_LABEL}: the guarded command was {detail!r}, "
                "not `--only-reproducibility`.")

    return problems
