#!/usr/bin/env python3
"""config_state's `probe-result/v1` migration contract (#2087).

This is the one batch-migrated probe whose `_run` reaches the working
tree before it boots -- it inspects `git status` and backs the local
config files up -- so its contract neutralizes those three seams and
restores them on every outcome. The neutralization lives here rather
than as a branch in the shared driver, so nothing about config_state's
fixture is visible to another probe's owner.
"""
from __future__ import annotations

from . import support

PROBE = "config_state"


def _neutralize_working_tree(module):
    """Stop `_run` touching `config/` before its first engine boot."""
    saved = (module.git_status, module.backup_local_files,
             module.restore_local_files)

    module.git_status = lambda _paths: ""
    module.backup_local_files = lambda: {}
    module.restore_local_files = lambda _backups: None

    def restore():
        (module.git_status, module.backup_local_files,
         module.restore_local_files) = saved

    return restore


def test_config_state_migration() -> None:
    support.batch_contract(
        PROBE, "config_state_probe.py", 9165,
        support.probe_checks("config_state_probe"),
        invoke=support.namespace_invoke(no_fall=False),
        patch=_neutralize_working_tree)


TESTS = (test_config_state_migration,)
