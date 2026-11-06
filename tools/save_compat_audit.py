#!/usr/bin/env python3
"""Current-save record audit and shared compiled codec facade.

The executable validates only ``tools/save_compat_current/record.json`` and
its explicitly listed current fixtures. Pre-release historical audit,
fixture registration, migration and reproducibility commands were retired by
PSP-5. Unsupported historical command-line options fail through argparse.

The compiled codec and structural comparison functions remain public here
because ``persistence_snapshot.py``, ``persistence_contract_probe.py`` and
``persistence_contract_sweep.py`` use this module as their stable import
boundary. Real-codec operations execute the built
``exe:synarchy-save-codec``; run ``cabal build all`` first.

Usage::

  python3 tools/save_compat_audit.py [--mode current]
"""
from __future__ import annotations

import argparse
import sys

import save_compat_audit_current as current_audit
from save_compat_audit_codec import (  # noqa: F401
    COMPARE_DECODE_FAILED, COMPARE_ERROR, COMPARE_MISMATCH, COMPARE_OK,
    compare_session_snapshots, dump_canonical_summary,
)
from save_compat_audit_common import REPO_ROOT  # noqa: F401

__all__ = ["REPO_ROOT", "compare_session_snapshots", "dump_canonical_summary",
           "main"]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Audit the current save schema and current fixture.")
    parser.add_argument("--mode", choices=["current"], default="current",
                        help="current-only mode (the default)")
    parser.parse_args(argv)
    return current_audit.cmd_audit()


if __name__ == "__main__":
    sys.exit(main())
