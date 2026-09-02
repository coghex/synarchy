#!/usr/bin/env python3
"""Offline self-test coordinator for the playtest harness (#2040).

`python3 tools/playtest/run.py --selftest` is still the documented
entry point; this module is the thin dispatch behind it. The checks
themselves live in one module per ownership boundary — see COMPONENTS —
and each component owns its own fixtures, fake classes and temporary
trace directories, so none of them depends on state or a trace dir
produced by an earlier one.

Components run SEQUENTIALLY in one process and are never parallelized:
several of them patch a process-global for the duration of a check
(`time.sleep`, `agent.subprocess.run`, `launch.teardown_setup`,
`launch.subprocess.Popen`), each restored in its own `finally`, and
that discipline only holds while exactly one component is running.

The whole run stays offline: no Cabal build, engine boot, GPU, window,
network access, model call, or external login.

Output is the pre-split format plus component identification around it.
Each check still prints `  [ok] <name>` / `  [FAIL] <name> — <detail>`,
and the run still ends with exactly `selftest: FAILED (N): <names>` or
`selftest: all checks passed`. What is added is a `selftest: component
<name>` header before each component's checks, and the component's name
qualifying each entry in the failure roll-up.
"""
from __future__ import annotations

import importlib
import os
import sys
import traceback

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

MODULE_PREFIX = "selftest_"

# The registry: every component, in the deterministic order they run.
COMPONENTS = ("session", "player", "engine", "setup")


def discover_components(directory: str = _HERE) -> tuple[str, ...]:
    """The registry's INDEPENDENT source of truth: the component modules
    actually present on disk. Deliberately not derived from COMPONENTS —
    a coverage check that enumerated the registry would still pass after
    a registry entry was deleted, which is exactly the silent shortening
    the check exists to prevent."""
    return tuple(sorted(
        entry[len(MODULE_PREFIX):-len(".py")]
        for entry in os.listdir(directory)
        if entry.startswith(MODULE_PREFIX) and entry.endswith(".py")))


def _coverage_detail(registered: tuple[str, ...],
                     discovered: tuple[str, ...]) -> str:
    """Name the disagreement in BOTH directions: a registered component
    with no module, and a module no registry entry runs."""
    unregistered = [n for n in discovered if n not in registered]
    missing = [n for n in registered if n not in discovered]
    parts = []
    if missing:
        parts.append("registered but absent: " + ", ".join(missing))
    if unregistered:
        parts.append("present but unregistered: " + ", ".join(unregistered))
    return "; ".join(parts)


def selftest() -> int:
    """Run every registered component and aggregate their failures.
    Returns 0 when all checks passed, 1 when any did — the same int
    `main()` hands back as the process exit status."""
    failures: list[str] = []

    def component_check(component):
        def check(name, ok, detail=""):
            print(f"  [{'ok' if ok else 'FAIL'}] {name}"
                  + (f" — {detail}" if detail else ""))
            if not ok:
                failures.append(f"{component}: {name}")
        return check

    registered = tuple(COMPONENTS)
    discovered = discover_components()
    print("selftest: component registry")
    coverage_check = component_check("registry")
    coverage_check("every component module on disk is registered, and "
                   "every registered component has a module",
                   set(registered) == set(discovered),
                   _coverage_detail(registered, discovered))

    for component in registered:
        print(f"selftest: component {component}")
        check = component_check(component)
        try:
            module = importlib.import_module(MODULE_PREFIX + component)
            if getattr(module, "NAME", None) != component:
                check("component module declares its own registry name",
                      False, f"NAME={getattr(module, 'NAME', None)!r}")
            module.run(check)
        except Exception:
            # A component that dies mid-way is one aggregated failure, not
            # a run that stops: the remaining components still report, and
            # every global patch is restored by its own `finally` before
            # the exception leaves the component.
            traceback.print_exc()
            check("component ran to completion", False,
                  "raised; see the traceback above")

    if failures:
        print(f"selftest: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("selftest: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(selftest())
