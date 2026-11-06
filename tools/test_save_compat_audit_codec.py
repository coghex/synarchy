#!/usr/bin/env python3
"""Focused tests for the compiled current-save codec bridge."""
from __future__ import annotations

import json
import os
import stat
import tempfile
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit_codec as codec  # noqa: E402
import save_compat_audit_common as common  # noqa: E402
import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

CURRENT_FIXTURE = common.REPO_ROOT / "tools/save_compat_current/fixtures/current-session.bin"
CURRENT_EXPECTED = common.REPO_ROOT / "tools/save_compat_current/fixtures/current-session.expected.json"


def _fake_helper(path: Path, marker: str) -> Path:
    path.write_text(f'#!/bin/sh\necho "{marker}"\nexit 0\n', encoding="utf-8")
    path.chmod(path.stat().st_mode | stat.S_IXUSR)
    return path


class _helper_env:
    def __init__(self, path: Path) -> None:
        self.path = path

    def __enter__(self) -> None:
        self.previous = os.environ.get(codec.ENV_CODEC_EXE)
        os.environ[codec.ENV_CODEC_EXE] = str(self.path)

    def __exit__(self, *_exc) -> None:
        if self.previous is None:
            os.environ.pop(codec.ENV_CODEC_EXE, None)
        else:
            os.environ[codec.ENV_CODEC_EXE] = self.previous


def test_current_fixture_matches_independent_summary() -> None:
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as tmp:
        output = Path(tmp) / "summary.json"
        ok, tail = codec.dump_canonical_summary(CURRENT_FIXTURE, output)
        expect(ok, f"expected the retained current fixture to decode: {tail}")
        actual = json.loads(output.read_text(encoding="utf-8"))
        expected = json.loads(CURRENT_EXPECTED.read_text(encoding="utf-8"))
        expect(actual == expected, "compiled current-fixture summary differs from its independent expectation")


def test_bad_current_fixture_reports_codec_error() -> None:
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as tmp:
        broken = Path(tmp) / "broken.bin"
        broken.write_bytes(b"not a save envelope")
        output = Path(tmp) / "summary.json"
        ok, tail = codec.dump_canonical_summary(broken, output)
        expect(not ok and str(broken) in tail and "BadMagic" in tail,
               f"expected fixture path and codec error in diagnostic, got {tail!r}")
        expect(not output.exists(), "failed decode must not leave a summary")


def test_success_marker_without_output_is_failure() -> None:
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as tmp:
        root = Path(tmp)
        output = root / "summary.json"
        with _helper_env(_fake_helper(root / "helper", "DUMP_OK")):
            ok, tail = codec.dump_canonical_summary(CURRENT_FIXTURE, output)
        expect(not ok and "wrote no canonical summary" in tail,
               f"expected missing-output diagnosis, got {tail!r}")
        expect(not output.exists(), "missing helper output must not be mistaken for success")


def test_pre_resolved_helper_is_used_and_missing_path_fails_closed() -> None:
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as tmp:
        root = Path(tmp)
        helper = _fake_helper(root / "helper", "DUMP_OK")
        with _helper_env(helper):
            resolved, why = codec.resolve_codec_exe()
        expect(resolved == str(helper), f"expected pre-resolved helper path, got {resolved!r}: {why}")
        missing = root / "missing-helper"
        with _helper_env(missing):
            resolved, why = codec.resolve_codec_exe()
        expect(resolved is None and str(missing) in why,
               f"missing pre-resolved helper must fail closed, got {resolved!r}: {why}")


TESTS = [
    test_current_fixture_matches_independent_summary,
    test_bad_current_fixture_reports_codec_error,
    test_success_marker_without_output_is_failure,
    test_pre_resolved_helper_is_used_and_missing_path_fails_closed,
]


def main() -> int:
    selftestlib.begin()
    for test in TESTS:
        test()
    return selftestlib.concluded(1 if FAILURES else 0,
                                 f"save_compat_audit_codec: {len(TESTS)} current codec tests")


if __name__ == "__main__":
    raise SystemExit(main())
