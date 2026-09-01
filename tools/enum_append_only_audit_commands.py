"""Audit and baseline-update orchestration for the enum audit.

The two production modes, sharing one scanner, one carrier computation,
one comparison and one deterministic renderer so they can never disagree
about what the baseline should contain.

Both keep their `root` parameter: the self-test drives synthetic trees
exclusively through these two entry points.
"""
from __future__ import annotations

from pathlib import Path

from enum_append_only_audit_model import (
    BASELINE_REL,
    REPO_ROOT,
    SOURCE_DIRS,
    AuditError,
)
from enum_append_only_audit_baseline import load_baseline, render_baseline
from enum_append_only_audit_carrier import compute_wire_carriers
from enum_append_only_audit_report import compare, guidance_lines, report
from enum_append_only_audit_scan import scan_repository


def run_repository_audit(root: Path = REPO_ROOT) -> int:
    try:
        scan = scan_repository(root)
        if not scan.guarded:
            print(f"no `Generic`-derived `Serialize` sum types found under "
                  f"{'/, '.join(SOURCE_DIRS)}/ — the audit would pass "
                  f"vacuously")
            return 1
        path = root / BASELINE_REL
        baseline = load_baseline(path)
        carriers = compute_wire_carriers(root, scan)
        stale = (path.read_text(encoding="utf-8")
                 != render_baseline(scan.guarded, carriers))
    except AuditError as err:
        print(f"enum_append_only_audit.py: {err}")
        return 1
    return report(compare(scan.guarded, baseline, carriers), carriers,
                  len(scan.guarded), stale)


def run_update_baseline(root: Path = REPO_ROOT) -> int:
    """Ratchet the baseline over append-compatible changes only."""
    path = root / BASELINE_REL
    try:
        scan = scan_repository(root)
        if not scan.guarded:
            print("refusing to write a vacuous baseline: no guarded sum "
                  "types found")
            return 1
        carriers = compute_wire_carriers(root, scan)
        existing = load_baseline(path) if path.exists() else {}
    except AuditError as err:
        print(f"enum_append_only_audit.py: {err}")
        return 1
    findings = compare(scan.guarded, existing, carriers)
    incompatible = [f for f in findings if not f.compatible]
    if incompatible:
        print(f"refusing to update {BASELINE_REL}: {len(incompatible)} "
              f"change(s) are NOT appends. Rewriting the baseline over them "
              f"would erase the evidence that saved bytes changed meaning.")
        for finding in incompatible:
            for line in finding.lines:
                print(f"  {line}")
            for line in guidance_lines(finding, carriers):
                print(f"  {line}")
        return 1
    rendered = render_baseline(scan.guarded, carriers)
    before = path.read_text(encoding="utf-8") if path.exists() else ""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(rendered, encoding="utf-8")
    if findings:
        print(f"{BASELINE_REL}: recorded {len(findings)} append-compatible "
              f"change(s)")
        for finding in findings:
            print(f"  {finding.lines[0]}")
    elif before != rendered:
        print(f"{BASELINE_REL}: refreshed the reachability attribution "
              f"({len(scan.guarded)} guarded sum types, no constructor list "
              f"changed)")
    else:
        print(f"{BASELINE_REL}: already up to date ({len(scan.guarded)} "
              f"guarded sum types)")
    return 0
