#!/usr/bin/env python3
"""The real-codec bridge for the save-compatibility tool (issue #2049,
requirement 7; converted from GHCi to a compiled helper by issue #2273).

A LEAF service (requirement 15): the ONE owner of every real-codec
operation this tool performs, and of the subprocess protocol behind
them. It imports only the shared definitions owner.

It owns the three operations, and the one binary that serves them:

  - fixed-timestamp rewriting (`set_fixture_timestamp`, and its
    in-place `normalize_fixture_timestamp` form);
  - decoded fixture-descriptor dumping (`dump_fixture_descriptors`);
  - canonical-summary dumping (`dump_canonical_summary`).

Everything else CALLS these; nothing else re-implements one
(requirement 16). In particular save_compat_audit_manifest's
`verify_fixture_descriptors` -- which turns a raw descriptor dump into
manifest violations -- is the manifest audit's, not this module's: it
takes a manifest dict and emits violation strings, so it belongs with
the audit that aggregates them, and it reaches the real bytes only
through `dump_fixture_descriptors` here.

Issue #2273: no operation here starts GHCi
--------------------------------------------
Until #2273 each operation was a GHCi program fed to
`cabal repl test:synarchy-test-headless` on stdin, so every call loaded
the 348-module test suite into the interpreter to reach a handful of
LIBRARY functions -- 91-133 s for the save-compatibility audit and
64-88 s for the fixture-reproducibility member on the CI critical path,
for work that is a few decodes.

Those three programs are now `app-save-codec/Main.hs`, a compiled
`exe:synarchy-save-codec` that `cabal build all` produces beside the
engine, and every operation below execs that binary. It decodes and
re-encodes through the SAME library functions the GHCi programs
imported -- `World.Save.Envelope.decodeSessionEnvelope`,
`World.Save.Envelope.Codec.decodeEnvelope` and `.encodeEnvelope` --
so this is a change of how the real codec is REACHED, never a second
decoder.

Requirement 8's operational shape is preserved across that conversion:
each operation keeps its own success marker (`NORMALIZE_OK` /
`DESCRIPTOR_DUMP_OK` / `DUMP_OK`), a 60-line diagnostic tail on failure,
and guaranteed temporary-file cleanup. These are still NOT
interchangeable with save_compat_audit_register's
`_run_real_codec_validation`, which is a different invocation entirely
(`cabal test synarchy-test-headless --test-options=--match "save
migrations"`, judged by its return code, 40-line tail) and stays with
the registration owner it validates for.

Resolving the binary (requirement 6)
------------------------------------
`resolve_codec_exe` answers an ABSOLUTE path, in one order:

  1. `SYNARCHY_SAVE_CODEC_EXE`, if set to a non-empty value. This is the
     pre-resolved handoff -- the same contract shape
     `probe_engine.ENV_ENGINE_EXE` gives a probe its already-built
     `exe:synarchy` (#1570) -- so a caller that has already resolved the
     binary invokes this bridge with NO Cabal contact in the child
     process at all. An explicitly exported path that does not exist is
     an error, never a silent fall-through to resolution: the export is
     a statement about which build to use.
  2. Otherwise one read-only `cabal list-bin exe:synarchy-save-codec`,
     cached for the life of the process. That is a plan query, not a
     build and not an interpreter: it neither compiles nor loads a
     module. It answers a path whether or not that file is current, so
     `cabal build all` remains the caller's responsibility exactly as it
     was before -- CI and `tools/ci-local.sh` both run it before any
     save-compatibility command.
  3. Only if that path does not EXIST, one `cabal build` of the target,
     then the query again. This is a compatibility bridge, not a
     freshness guarantee: it makes an ABSENT binary buildable, and says
     nothing about a stale one, which is why `cabal build all` is still
     the caller's job.

     It exists for the three probes that reach this module without a
     preceding `cabal build all` -- `persistence_contract`,
     `persistence_contract_sweep` and `save_compat_migration`, whose
     runner preflight builds `exe:synarchy` and nothing else (#1570).
     Before #2273 the `cabal repl` those calls went through built
     whatever it needed; a compiled helper has to be able to say the
     same. All three already hold the `cabal-build` resource
     EXCLUSIVELY (`tools/probe_runner_resources.py`), because they drive
     Cabal themselves, so a build here is inside a hold that already
     exists rather than a new concurrent mutation of `dist-newstyle`.
     Converting those probes off Cabal entirely is the declared
     follow-up; this keeps them working until then, and never fires in
     CI, where `cabal build all` has already run.

The public façade is tools/save_compat_audit.py.
"""
from __future__ import annotations

import json
import os
import subprocess
import tempfile
from pathlib import Path

import save_compat_audit_common as common

#: The Cabal target that produces the compiled codec helper (#2273).
CODEC_TARGET = "exe:synarchy-save-codec"

#: The pre-resolved-binary handoff (requirement 6). Its value is an
#: ABSOLUTE path to an already-built `exe:synarchy-save-codec`; its
#: presence is what lets a caller skip Cabal entirely. Deliberately
#: distinct from `probe_engine.ENV_ENGINE_EXE`: that one names the
#: ENGINE, and a caller may legitimately hold both.
ENV_CODEC_EXE = "SYNARCHY_SAVE_CODEC_EXE"

#: Generous ceiling on one helper invocation. A decode is milliseconds;
#: this exists so a wedged subprocess fails the audit instead of hanging
#: a CI job. The GHCi path needed 1800 s because it compiled first.
CODEC_TIMEOUT_SECONDS = 300

#: Ceiling on a `cabal` call. Longer than CODEC_TIMEOUT_SECONDS because
#: step 3 of the resolution below can actually compile the helper on a
#: cold tree, which the helper's own invocations never do.
CABAL_TIMEOUT_SECONDS = 1800

#: Cached answer of the `cabal list-bin` branch of `resolve_codec_exe`,
#: so a batch of operations pays that query at most once. `None` means
#: "not resolved yet"; the environment branch is never cached, so a test
#: repointing `SYNARCHY_SAVE_CODEC_EXE` is always honoured.
_CACHED_CODEC_EXE: str | None = None


def resolve_codec_exe() -> tuple[str | None, str]:
    """The absolute path of the compiled codec helper, or (None, why).

    See the module docstring for the resolution order and why an
    explicitly exported path is never silently ignored.
    """
    global _CACHED_CODEC_EXE
    exported = os.environ.get(ENV_CODEC_EXE, "").strip()
    if exported:
        if not Path(exported).is_file():
            return None, (
                f"{ENV_CODEC_EXE} names {exported!r}, which is not a file -- "
                f"export the absolute path of an already-built "
                f"{CODEC_TARGET}, or unset it to resolve through cabal")
        return exported, ""
    if _CACHED_CODEC_EXE is not None:
        return _CACHED_CODEC_EXE, ""
    path, why = _list_bin()
    if path is None:
        return None, why
    if not Path(path).is_file():
        # Step 3: absent, not stale. Build once, then ask again -- and if
        # the answer is still absent, report that rather than looping.
        built, why = _cabal(["build", CODEC_TARGET])
        if not built:
            return None, why
        path, why = _list_bin()
        if path is None:
            return None, why
        if not Path(path).is_file():
            return None, (
                f"`cabal build {CODEC_TARGET}` reported success but "
                f"{path!r} still does not exist")
    _CACHED_CODEC_EXE = path
    return path, ""


def _cabal(args: list[str]) -> tuple[bool, str]:
    """Run one `cabal` command in the repository root."""
    try:
        proc = subprocess.run(
            ["cabal", *args], cwd=common.REPO_ROOT, capture_output=True,
            text=True, timeout=CABAL_TIMEOUT_SECONDS)
    except FileNotFoundError:
        return False, "'cabal' was not found on PATH"
    except subprocess.TimeoutExpired:
        return False, f"`cabal {' '.join(args)}` timed out"
    if proc.returncode != 0:
        tail = "\n".join(
            ((proc.stdout or "") + (proc.stderr or "")).splitlines()[-60:])
        return False, f"`cabal {' '.join(args)}` failed: {tail}"
    return True, (proc.stdout or "")


def _list_bin() -> tuple[str | None, str]:
    """The path `cabal list-bin` names for the codec target, unverified."""
    ok, output = _cabal(["list-bin", CODEC_TARGET])
    if not ok:
        return None, output
    # `cabal list-bin` can precede its answer with warnings, so the path
    # is the LAST non-empty line, not the whole of stdout.
    lines = [ln.strip() for ln in output.splitlines() if ln.strip()]
    if not lines:
        return None, (
            f"`cabal list-bin {CODEC_TARGET}` named no path at all (run "
            f"`cabal build all` first)")
    return lines[-1], ""


def _run_codec(args: list[str], marker: str) -> tuple[bool, str]:
    """Run one codec-helper operation, judging it by BOTH its exit status
    and its own success marker.

    Returns (ok, diagnostic-tail-on-failure). The diagnostic is the last
    60 lines of the helper's combined output, which for every failure
    path in `app-save-codec/Main.hs` names the offending fixture and the
    production codec's own error text (issue #2273 requirement 5).
    """
    exe, why = resolve_codec_exe()
    if exe is None:
        return False, why
    try:
        proc = subprocess.run(
            [exe, *args], cwd=common.REPO_ROOT, capture_output=True,
            text=True, timeout=CODEC_TIMEOUT_SECONDS)
    except FileNotFoundError:
        return False, f"{exe} was not found -- run `cabal build all` first"
    except subprocess.TimeoutExpired:
        return False, (f"{Path(exe).name} {args[0]} exceeded "
                       f"{CODEC_TIMEOUT_SECONDS}s")
    output = (proc.stdout or "") + (proc.stderr or "")
    if proc.returncode != 0 or marker not in output:
        return False, "\n".join(output.splitlines()[-60:])
    return True, ""


def set_fixture_timestamp(fixture_path: Path, timestamp: str,
                          output_path: Path | None = None) -> tuple[bool, str]:
    """Rewrite ONLY `fixture_path`'s metadata component's smTimestamp to
    `timestamp`, writing the result to `output_path` (default: in place).

    Every other component's version/required/payload bytes are re-encoded
    verbatim through the real envelope codec -- see
    `app-save-codec/Main.hs`'s `set-timestamp` for why that is a decode/
    re-encode rather than a binary patch. Returns
    (ok, diagnostic-tail-on-failure).
    """
    args = ["set-timestamp", "--fixture", str(fixture_path),
            "--timestamp", timestamp]
    if output_path is not None:
        args += ["--output", str(output_path)]
    return _run_codec(args, "NORMALIZE_OK")


def normalize_fixture_timestamp(fixture_path: Path) -> tuple[bool, str]:
    """Overwrite fixture_path's metadata smTimestamp with
    common.FIXED_GENERATED_TIMESTAMP, in place.

    The reproducibility guarantee `--generate-session` depends on:
    `engine.saveWorld` stamps the current wall-clock time into
    smTimestamp, so without this two generation runs over identical
    inputs differ in bytes and sha256 purely from when they ran.
    Returns (ok, diagnostic-tail-on-failure).
    """
    return set_fixture_timestamp(fixture_path, common.FIXED_GENERATED_TIMESTAMP)


def dump_fixture_descriptors(
        fixture_paths: list[Path]) -> tuple[dict[str, list[dict]] | None, str]:
    """Decode every path in fixture_paths' RAW envelope manifest.

    Returns (path-string -> [{"id","version","required"}, ...] for every
    fixture, "") on success, or (None, diagnostic) on any decode or
    subprocess failure. The helper fails the whole batch on one
    undecodable fixture, so a `dict` answered here always covers every
    path asked for.

    The audit's version-coverage checks previously trusted a baseline's
    declared components[] versions entirely from the manifest JSON, never
    cross-checked against what a fixture's OWN bytes contain;
    save_compat_audit_manifest.verify_fixture_descriptors uses this dump
    to grind that claim against real, decoded descriptors.
    """
    if not fixture_paths:
        return {}, ""
    with tempfile.NamedTemporaryFile(
            suffix=".json", dir=common.REPO_ROOT, delete=False) as tf:
        output_path = Path(tf.name)
    try:
        ok, tail = _run_codec(
            ["descriptors", "--output", str(output_path)]
            + [str(p) for p in fixture_paths],
            "DESCRIPTOR_DUMP_OK")
        if not ok:
            return None, tail
        # A helper that reported success without leaving usable output
        # behind is its own failure mode, distinct from a decode failure
        # (issue #2273 requirement 4): report it as one rather than
        # raising an unhandled JSONDecodeError from the read below.
        #
        # ABSENCE is not the test. `NamedTemporaryFile(delete=False)`
        # above has already created the path, so a helper that wrote
        # nothing leaves an EMPTY file, not a missing one -- the
        # condition to judge is whether the file parses as the JSON
        # object the helper promised.
        try:
            document = json.loads(output_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            return None, (
                f"the codec helper reported DESCRIPTOR_DUMP_OK but wrote no "
                f"readable descriptor output at {output_path}: {error}")
        if not isinstance(document, dict):
            return None, (
                f"the codec helper reported DESCRIPTOR_DUMP_OK but wrote "
                f"{type(document).__name__}, not the path-keyed object the "
                f"descriptor dump promises, at {output_path}")
        return document, ""
    finally:
        output_path.unlink(missing_ok=True)


def dump_canonical_summary(fixture_path: Path, output_path: Path) -> tuple[bool, str]:
    """Derive fixture_path's canonical summary from its real decoded
    SessionSnapshot/SaveMetadata and write it to output_path.

    Returns (ok, diagnostic-tail-on-failure). A helper that reported
    success without leaving output_path behind is reported as a failure
    in its own words, which is a different diagnosis from a decode
    failure (issue #2273 requirement 4).
    """
    ok, tail = _run_codec(
        ["summary", "--fixture", str(fixture_path),
         "--output", str(output_path)],
        "DUMP_OK")
    if not ok:
        return False, tail
    # Emptiness, not absence: a caller may hand this an output path that
    # already exists (a rollback test's pre-existing summary, say), so
    # "the file is there" is not evidence the helper wrote it. Every real
    # canonical summary is a non-empty JSON object.
    if not output_path.exists() or output_path.stat().st_size == 0:
        return False, (
            f"the codec helper reported DUMP_OK but wrote no canonical "
            f"summary at {output_path}")
    return True, ""
