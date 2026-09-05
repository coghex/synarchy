#!/usr/bin/env python3
"""Resilience and presentation cases for `test_probe_external_evidence.py` (#2187).

The seven cases here own damaged-input resilience, state-root
resolution and presentation: absent state is the normal success, a
damaged registry is non-fatal, `NaN` / `Infinity` / `-Infinity` never
reach the emitted document, full matching history is never truncated,
the reader is presentation-only and wired into none of the lab's
statistics, a linked worktree resolves through the common Git
directory, and rendering is total.

`test_state_root_resolves_through_the_common_git_dir` is the ONE case
that legitimately shells out: it builds its own scratch repository with
a real linked worktree and runs outside the forbidden-subprocess
tripwire, which is why this module imports `subprocess` directly.

`CASES` is this owner's inventory in the order the aggregate runs it.
This module holds case bodies and that inventory only; `python3
tools/test_probe_external_evidence.py --only resilience` is the way
to run it.
"""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import tempfile
from pathlib import Path

from probe_external_evidence_test_support import (  # noqa: E402
    NonInteraction, build_state, check, check_equal, evidence, make_run, read,
)


def test_absent_state_is_success_not_error() -> None:
    """An absent `codex-test` tree is the normal no-evidence result."""
    with tempfile.TemporaryDirectory() as tmp:
        missing = Path(tmp) / "no-such-state"
        result = read(missing, "role")
        check_equal(result["state"], evidence.STATE_ABSENT, "the state reads as absent")
        check_equal(result["runs"], [], "no runs are reported")
        check_equal(result["diagnostics"], [], "absence is not a diagnostic")
        check_equal(result["test_id"], "probe:role", "the mapped id is still reported")
        check(not missing.exists(), "resolving absent state creates nothing")
        check_equal(evidence.main(["--probe", "role", "--state-root", str(missing)]),
                    evidence.EXIT_OK, "the CLI exits 0 on absent state")
        check("no external evidence" in evidence.render(result),
              "the rendering says so plainly")


def test_damaged_registry_is_non_fatal() -> None:
    """A malformed or missing registry diagnoses; it never raises."""
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        (state / evidence.REGISTRY_FILENAME).write_text("{ not json", encoding="utf-8")
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("malformed registry")
        check_equal(result["state"], evidence.STATE_PRESENT, "the state is still present")
        check_equal(result["runs"], [], "an unparseable registry yields no runs")
        check(any("cannot parse" in d for d in result["diagnostics"]),
              "the parse failure is diagnosed", str(result["diagnostics"]))

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [], {})
        (state / evidence.REGISTRY_FILENAME).unlink()
        result = read(state, "role")
        check(any("does not" in d for d in result["diagnostics"]),
              "a missing registry beside present state is diagnosed",
              str(result["diagnostics"]))

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {},
                            schema="codex-test-coordinator/v99")
        result = read(state, "role")
        check(any("v99" in d for d in result["diagnostics"]),
              "an unexpected schema is diagnosed", str(result["diagnostics"]))
        check_equal([r["run_id"] for r in result["runs"]], ["run"],
                    "an unexpected schema is still read best-effort")

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [], {})
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = ["not-an-object", make_run("probe:role", "good")]
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))
        result = read(state, "role")
        check_equal([r["run_id"] for r in result["runs"]], ["good"],
                    "a malformed record is skipped, the good one kept")
        check(any("not an object" in d for d in result["diagnostics"]),
              "the malformed record is diagnosed", str(result["diagnostics"]))


def test_non_finite_numbers_never_reach_the_output() -> None:
    """`NaN`/`Infinity` in the registry are malformed state, and diagnosed.

    Python's `json` reads those non-standard constants happily and
    writes them straight back, so without this the reader would present
    a damaged registry with no diagnostic AND emit invalid JSON from
    `--json`.
    """
    for token in ("NaN", "Infinity", "-Infinity"):
        with tempfile.TemporaryDirectory() as tmp:
            state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
            document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
            (state / evidence.REGISTRY_FILENAME).write_text(
                json.dumps(document).replace('"elapsed_seconds": 288.783',
                                             f'"elapsed_seconds": {token}'),
                encoding="utf-8")
            with NonInteraction(state) as guard:
                result = read(state, "role")
                guard.assert_untouched(f"{token} registry")
            check_equal(result["runs"], [],
                        f"a registry carrying {token} contributes no runs")
            check(any("cannot parse" in d and token.lstrip("-") in d
                      for d in result["diagnostics"]),
                  f"{token} is diagnosed as a parse failure",
                  str(result["diagnostics"]))
            check_equal(evidence.main(["--probe", "role", "--json",
                                       "--state-root", str(state)]),
                        evidence.EXIT_OK,
                        f"the CLI still exits 0 with {token} in the registry")

    # Second layer: nothing non-finite survives the field readers, so no
    # hand-built or computed value can reintroduce one.
    for bad in (float("nan"), float("inf"), float("-inf")):
        check_equal(evidence._number_or_none(bad), None,
                    f"{bad!r} reads as unavailable")
    check_equal(evidence._number_or_none(288.783), 288.783,
                "a finite duration is kept")
    check_equal(evidence._number_or_none(True), None, "a bool is not a duration")

    # And the emitted document is STRICT JSON: no NaN, no Infinity.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        result = read(state, "role")
        strict = json.dumps(result, allow_nan=False)
        json.loads(strict, parse_constant=evidence._reject_json_constant)
        check(True, "the evidence document is strict, constant-free JSON")


def test_full_history_is_never_truncated() -> None:
    """No default limit silently drops known history."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", f"run-{index:03d}",
                     claimed_at=f"2026-08-{(index % 28) + 1:02d}T{index % 24:02d}:00:00Z")
            for index in range(60)
        ]
        runs.append(make_run("probe:chop", "unrelated"))
        state = build_state(Path(tmp), runs, {})
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("full history")
        check_equal(len(result["runs"]), 60, "every matching run is reported")
        claimed = [r["claimed_at"] for r in result["runs"]]
        check_equal(claimed, sorted(claimed, reverse=True),
                    "the whole history stays newest-first")


def test_presentation_only() -> None:
    """The reader is not wired into the lab's statistics."""
    source = Path(evidence.__file__).read_text(encoding="utf-8")
    check("import probe_census" not in source,
          "the reader does not import the census")
    check("import probe_flake" not in source,
          "the reader does not import the flake harness")
    check(not hasattr(evidence, "probe_census"),
          "the reader exposes no census handle")
    for forbidden in ("write_text", "write_bytes", "mkdir", "os.replace", "flock"):
        check(f"{forbidden}(" not in source,
              f"the reader contains no {forbidden} call")


def test_state_root_resolves_through_the_common_git_dir() -> None:
    """A linked worktree resolves to the MAIN checkout's git directory."""
    if shutil.which("git") is None:                       # pragma: no cover
        check(False, "git is available for the resolution case")
        return
    with tempfile.TemporaryDirectory() as tmp:
        main = Path(tmp) / "main"
        main.mkdir()
        env = dict(os.environ, GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_SYSTEM=os.devnull)

        def git(*args: str, cwd: Path = main) -> None:
            subprocess.run(["git", "-C", str(cwd), *args], check=True,
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, env=env)

        git("init", "-q", "-b", "main")
        git("config", "user.email", "test@example.com")
        git("config", "user.name", "Test")
        (main / "file.txt").write_text("x\n", encoding="utf-8")
        git("add", "file.txt")
        git("commit", "-qm", "seed")
        linked = Path(tmp) / "linked"
        git("worktree", "add", "-q", "--detach", str(linked))

        expected = (main / ".git" / evidence.STATE_DIRNAME).resolve()
        check_equal(evidence.resolve_state_root(main).resolve(), expected,
                    "the main checkout resolves to its own git dir")
        check_equal(evidence.resolve_state_root(linked).resolve(), expected,
                    "a linked worktree resolves to the MAIN git dir")
        check(not (linked / ".git").is_dir(),
              "the linked worktree's .git is a pointer file, not a directory")
        check(not expected.exists(), "resolution creates no state tree")

        outside = Path(tmp) / "not-a-repo"
        outside.mkdir()
        try:
            evidence.resolve_state_root(outside)
            check(False, "a non-repository is rejected")
        except evidence.EvidenceRejected as exc:
            check("common directory" in str(exc),
                  "the rejection explains what could not be resolved", str(exc))


def test_render_is_total() -> None:
    """Every field renders, including when all of them are unavailable."""
    with tempfile.TemporaryDirectory() as tmp:
        bare = {"test_id": "probe:role", "run_id": "bare", "status": "claimed"}
        state = build_state(Path(tmp), [bare], {})
        result = read(state, "role")
        text = evidence.render(result)
        check("bare" in text, "the run id renders")
        check(text.count("unavailable") >= 4,
              "every unavailable field renders as unavailable", text)
        check("makes no scheduling decision" in text,
              "the read-only note renders")
        json.dumps(result)  # raises if a value is not JSON-serializable
        check(True, "the evidence document is JSON-serializable")


#: This owner's cases, in the order the aggregate has always run them.
CASES = (
    test_absent_state_is_success_not_error,
    test_damaged_registry_is_non_fatal,
    test_non_finite_numbers_never_reach_the_output,
    test_full_history_is_never_truncated,
    test_presentation_only,
    test_state_root_resolves_through_the_common_git_dir,
    test_render_is_total,
)
