#!/usr/bin/env python3
"""Audit the active pre-release current-save safety wiring."""
from __future__ import annotations

REQUIRED_FRAGMENTS = (
    "tools/current_save_schema_audit.py --self-test",
    "tools/current_save_schema_audit.py --base-ref",
    "tools/test_save_compat_audit.py",
)
RETIRED_FRAGMENTS = (
    "--only-reproducibility",
    "--without-reproducibility",
    "--gate save-compat",
    "tools/save_compat_migration_probe.py",
)


def audit_current_save_policy_wiring(workflow_text: str,
                                     local_text: str) -> list[str]:
    problems: list[str] = []
    for label, text in (("workflow", workflow_text), ("ci-local.sh", local_text)):
        for fragment in REQUIRED_FRAGMENTS:
            if fragment not in text:
                problems.append(f"{label} is missing current-save check {fragment!r}")
        for fragment in RETIRED_FRAGMENTS:
            if fragment in text:
                problems.append(f"{label} still reaches retired save policy {fragment!r}")
    if "github.event.before" not in workflow_text:
        problems.append("workflow does not use the full push before/after range")
    if "github.event.pull_request.base.sha" not in workflow_text:
        problems.append("workflow does not compare pull requests with their base SHA")
    if "origin/master" not in local_text or "--base-ref" not in local_text:
        problems.append("local gate does not compare against origin/master")
    return problems
