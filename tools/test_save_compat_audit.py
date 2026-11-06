#!/usr/bin/env python3
"""Current-save audit tests and compiled codec/comparison regressions.

Pre-release historical migration assertions were retired by PSP-5. The
remaining members exercise the current record, the real compiled codec
bridge and the structural comparison API used by persistence probes.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import test_save_compat_audit_codec as codec_tests  # noqa: E402
import test_save_compat_audit_current as current_tests  # noqa: E402
import selftestlib  # noqa: E402
from selftestlib import FAILURES  # noqa: E402

ALL_TESTS = [*current_tests.TESTS, *codec_tests.TESTS]
CURRENT_TESTS = list(current_tests.TESTS)
REQUIRED_CURRENT_TEST_NAMES = (
    "test_current_record_checks_real_compiled_codec_and_records_identity",
    "test_current_selection_does_not_read_historical_manifest_or_files",
    "test_stale_or_dishonest_current_descriptors_fail_closed",
    "test_empty_and_duplicate_current_selections_are_rejected",
    "test_current_seed_and_comparison_outcomes_use_current_inputs",
    "test_corrupt_current_checksum_and_unsupported_framing_are_refused",
    "test_compare_tool_error_is_not_a_fixture_verdict",
)


def duplicate_members(tests: list) -> list[str]:
    seen: set = set()
    duplicates: list[str] = []
    for fn in tests:
        if fn in seen:
            duplicates.append(fn.__name__)
        else:
            seen.add(fn)
    return duplicates


def missing_current_tests(tests: list) -> list[str]:
    registered = {fn.__name__ for fn in tests}
    return [name for name in REQUIRED_CURRENT_TEST_NAMES if name not in registered]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Current-save audit, compiled codec and comparison tests.",
        epilog="The removed historical reproducibility flags are unsupported.")
    parser.add_argument("--mode", choices=["current"], default="current",
                        help="current-only mode (the default)")
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(argv)
    selftestlib.begin(args.verbose)
    duplicates = duplicate_members(ALL_TESTS)
    missing = missing_current_tests(CURRENT_TESTS)
    if duplicates or missing or not CURRENT_TESTS:
        if duplicates:
            print(f"members listed more than once: {duplicates}")
        if missing:
            print(f"required current tests missing from selection: {missing}")
        if not CURRENT_TESTS:
            print("no current tests selected -- refusing a vacuous pass")
        return 1
    for fn in ALL_TESTS:
        fn()
    if FAILURES:
        print(f"\n{len(FAILURES)} failure(s)")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, f"\nall current save-audit tests passed ({len(ALL_TESTS)} members; "
           f"{len(CURRENT_TESTS)} current-policy assertions)")


if __name__ == "__main__":
    sys.exit(main())
