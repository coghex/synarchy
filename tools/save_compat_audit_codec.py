#!/usr/bin/env python3
"""The real-codec bridge for the save tooling (issue #2049,
requirement 7; converted from GHCi to a compiled helper by issue #2273,
extended to the persistence-contract comparison by #2274).

A LEAF service (requirement 15): the ONE owner of every real-codec
operation the Python save tooling performs, and of the subprocess
protocol behind them. It imports only the shared definitions owner.

It owns the four operations, and the one binary that serves them:

  - fixed-timestamp rewriting (`set_fixture_timestamp`, and its
    in-place `normalize_fixture_timestamp` form);
  - decoded fixture-descriptor dumping (`dump_fixture_descriptors`);
  - canonical-summary dumping (`dump_canonical_summary`);
  - structural session comparison (`compare_session_snapshots`).

Everything else CALLS these; nothing else re-implements one
(requirement 16). In particular save_compat_audit_manifest's
`verify_fixture_descriptors` -- which turns a raw descriptor dump into
manifest violations -- is the manifest audit's, not this module's: it
takes a manifest dict and emits violation strings, so it belongs with
the audit that aggregates them, and it reaches the real bytes only
through `dump_fixture_descriptors` here. The same division puts
`tools/persistence_snapshot.py`'s diagnostic diff there rather than
here: this module answers "are these saves identical, and if not which
one diverged", and that module turns the answer into a probe's failure
text.

Issues #2273/#2274: no operation here starts GHCi
--------------------------------------------------
Until #2273 each operation was a GHCi program fed to
`cabal repl test:synarchy-test-headless` on stdin, so every call loaded
the 348-module test suite into the interpreter to reach a handful of
LIBRARY functions -- 91-133 s for the save-compatibility audit and
64-88 s for the fixture-reproducibility member on the CI critical path,
for work that is a few decodes.

Those programs are now `app-save-codec/Main.hs`, a compiled
`exe:synarchy-save-codec` that `cabal build all` produces beside the
engine, and every operation below execs that binary. It decodes and
re-encodes through the SAME library functions the GHCi programs
imported -- `World.Save.Envelope.decodeSessionEnvelope`,
`World.Save.Envelope.Codec.decodeEnvelope` and `.encodeEnvelope` --
so this is a change of how the real codec is REACHED, never a second
decoder.

`compare_session_snapshots` is the fourth and last member of that
family (#2274). `tools/persistence_snapshot.py` kept its own `cabal
repl` after #2273, which is the entire reason the `persistence_contract`
probe had to hold the shared Cabal build state EXCLUSIVELY and the
`behavior-probes` CI job had to build `synarchy-test-headless` at all.
Moving it here leaves no Python surface in this repository that starts
GHCi to reach the save codec.

Requirement 8's operational shape is preserved across that conversion:
each operation keeps its own success marker (`NORMALIZE_OK` /
`DESCRIPTOR_DUMP_OK` / `DUMP_OK` / `COMPARE_OK`), a 60-line diagnostic
tail on failure, and guaranteed temporary-file cleanup. These are still
NOT interchangeable with save_compat_audit_register's
`_run_real_codec_validation`, which is a different invocation entirely
(`cabal test synarchy-test-headless --test-options=--match "save
migrations"`, judged by its return code, 40-line tail) and stays with
the registration owner it validates for.

`compare_session_snapshots` is the one operation whose non-`OK` outcomes
are ANSWERS rather than malfunctions, so it does not go through
`_run_codec`: "these saves differ" and "one of them will not decode" are
results the caller must be able to tell apart from "the helper broke",
and a bare (ok, tail) pair cannot express three outcomes.

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
  2. Otherwise ONE `cabal build exe:synarchy-save-codec` followed by ONE
     read-only `cabal list-bin`, cached together for the life of the
     process.

     The build is unconditional on purpose, and this is the whole reason
     the branch is shaped this way rather than "build only when the file
     is missing". `cabal list-bin` answers a path whether or not that
     file is current, so accepting a helper because it EXISTS would hand
     a caller a binary built before the last edit to `app-save-codec/` or
     to an imported `World.Save.*` module -- and decode it against
     yesterday's codec while reporting today's verdict. The `cabal repl`
     this replaced compiled from current sources every time; a compiled
     helper has to make the same promise. `probe_engine.resolve_executable`
     resolves `exe:synarchy` with exactly this shape, and for exactly
     this reason (#1570).

     Since #2274 branch 1 is what the AGGREGATE probe runner takes:
     `tools/run_probes.py`'s preflight resolves this helper beside
     `exe:synarchy` and hands both to every probe it launches, so no
     probe process reaches branch 2 at all. That handoff is what let
     `persistence_contract` stop holding `cabal-build` EXCLUSIVELY, and
     it is why branch 2 must never be a silent fallback for a probe the
     runner launched: a build there would be a concurrent mutation of
     `dist-newstyle` under a merely SHARED hold, which is #1570's defect
     exactly.

     Branch 2 therefore serves the DIRECT, hand-run path only. The
     persistence probes prepare this helper up front, before they boot
     anything, the same way `probe_engine.prepare_executable` prepares
     the engine (#1913) -- inside an EXCLUSIVE `cabal-build` hold, and
     outside the clock that times probe work. `save_compat_migration`
     still declares `cabal-build` exclusively, so its own reach into
     this module is inside a hold either way.

     On a tree where `cabal build all` has already run -- CI, `make ci`,
     and any ordinary local invocation -- the build is a plan check that
     compiles nothing and costs no measurable time.

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
    # Build FIRST, then locate. Not "locate, and build if absent": a
    # present-but-stale helper is the failure this ordering exists to
    # prevent, and `cabal list-bin` cannot tell the two apart.
    built, why = _cabal(["build", CODEC_TARGET])
    if not built:
        return None, why
    path, why = _list_bin()
    if path is None:
        return None, why
    if not Path(path).is_file():
        return None, (
            f"`cabal build {CODEC_TARGET}` reported success but {path!r} "
            f"does not exist")
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


def _invoke_codec(args: list[str]) -> tuple[int | None, str, str]:
    """Exec one codec-helper operation, without judging its outcome.

    Returns (returncode, combined output, launch diagnostic). A
    returncode of None means the helper never ran -- it could not be
    resolved, was not there, or overran `CODEC_TIMEOUT_SECONDS` -- and
    the third member then says which; every caller has to report that as
    something other than an answer about the fixtures.

    Split out of `_run_codec` (#2274) because `compare_session_snapshots`
    needs the raw status and output: its non-zero exits are legitimate
    ANSWERS, not malfunctions, and a helper that never launched has to
    stay distinguishable from one that reported a mismatch.
    """
    exe, why = resolve_codec_exe()
    if exe is None:
        return None, "", why
    try:
        proc = subprocess.run(
            [exe, *args], cwd=common.REPO_ROOT, capture_output=True,
            text=True, timeout=CODEC_TIMEOUT_SECONDS)
    except FileNotFoundError:
        return None, "", f"{exe} was not found -- run `cabal build all` first"
    except subprocess.TimeoutExpired:
        return None, "", (f"{Path(exe).name} {args[0]} exceeded "
                          f"{CODEC_TIMEOUT_SECONDS}s")
    return proc.returncode, (proc.stdout or "") + (proc.stderr or ""), ""


def _run_codec(args: list[str], marker: str) -> tuple[bool, str]:
    """Run one codec-helper operation, judging it by BOTH its exit status
    and its own success marker.

    Returns (ok, diagnostic-tail-on-failure). The diagnostic is the last
    60 lines of the helper's combined output, which for every failure
    path in `app-save-codec/Main.hs` names the offending fixture and the
    production codec's own error text (issue #2273 requirement 5).
    """
    returncode, output, why = _invoke_codec(args)
    if returncode is None:
        return False, why
    if returncode != 0 or marker not in output:
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
    success without actually producing a summary is reported as a failure
    in its own words, which is a different diagnosis from a decode
    failure (issue #2273 requirement 4).
    """
    # The helper writes to a path THIS call created, which is then moved
    # into place. Neither the file's existence nor its size at
    # `output_path` is evidence the helper produced anything: callers
    # legitimately hand this an output path that already holds a summary
    # (`--generate-session --force` regenerating over a registered
    # fixture's own `*.expected.json`), and a helper that exited 0 having
    # written nothing would leave that older content sitting there,
    # non-empty, to be read back as this run's answer.
    #
    # Staging also means a failure leaves `output_path` byte-untouched,
    # which is what the generation transaction's rollback wants anyway.
    # Every I/O step below is inside the (ok, diagnostic) contract, not
    # outside it. Staging can fail before the helper ever runs -- a
    # `--summary` naming a parent directory that does not exist is the
    # ordinary case -- and `cmd_generate` rolls the fixture and summary
    # back on a RETURNED failure, not on an exception, so an OSError
    # escaping here would leave a newly generated, unregistered fixture
    # on disk. The pre-#2273 path could not do that: the helper itself
    # failed on an unwritable output and exited non-zero.
    output_path = Path(output_path)
    staged: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
                suffix=".json", dir=output_path.parent, delete=False) as tf:
            staged = Path(tf.name)
        ok, tail = _run_codec(
            ["summary", "--fixture", str(fixture_path),
             "--output", str(staged)],
            "DUMP_OK")
        if not ok:
            return False, tail
        if staged.stat().st_size == 0:
            return False, (
                f"the codec helper reported DUMP_OK but wrote no canonical "
                f"summary for {fixture_path}")
        os.replace(staged, output_path)
        return True, ""
    except OSError as error:
        return False, (
            f"could not write the canonical summary for {fixture_path} to "
            f"{output_path}: {error}")
    finally:
        if staged is not None:
            staged.unlink(missing_ok=True)


#: `compare_session_snapshots`'s three fixture-describing outcomes, plus
#: the one that describes THIS bridge instead. Named so a caller branches
#: on a constant rather than on a string literal it has to keep in sync
#: with the helper's stdout markers.
COMPARE_OK = "ok"
COMPARE_MISMATCH = "mismatch"
COMPARE_DECODE_FAILED = "decode_failed"
COMPARE_ERROR = "error"

#: The `compare` protocol: for each fixture-describing outcome, the
#: stdout marker that names it and the exit status that must accompany
#: it.
#:
#: THREE independent signals have to agree before an outcome is
#: believed -- exactly one protocol line, the EXACT exit status that
#: line carries, and the report's own `outcome` field -- and none of
#: them alone is the answer. Each is checked for the value the protocol
#: assigns it, never for a weaker property that happens to hold: "one
#: distinct outcome" is not "one line", and "nonzero" is not "1".
#:
#: The reason is that the marker travels beside attacker-shaped data.
#: `DECODE_FAILED`'s line carries the offending PATH, and a save file
#: whose own name contains `COMPARE_OK` is a perfectly ordinary path for
#: a probe to hand this: a substring scan over the combined output would
#: then read a decode failure as success and let
#: `compare_session_files` pass on saves it never compared. The marker
#: is therefore matched as a whole LINE (a `show`-escaped Haskell string
#: can never contain a real newline, so no path can forge one), and the
#: exit status and the report are cross-checked against it, so forging
#: an outcome would take three simultaneous lies rather than one
#: filename.
_COMPARE_PROTOCOL = (
    ("COMPARE_OK", COMPARE_OK, 0),
    ("COMPARE_MISMATCH", COMPARE_MISMATCH, 1),
    ("DECODE_FAILED", COMPARE_DECODE_FAILED, 1),
)


def _compare_protocol_lines(output: str) -> list[tuple[str, str, int, str]]:
    """Every `compare` protocol LINE in `output`, in order.

    A protocol line is one whose first field -- the text before the
    first `:`, stripped -- EQUALS one of the three markers. Two
    properties follow, and both are load-bearing:

      * a marker appearing anywhere else on the line is not a marker.
        `DECODE_FAILED`'s line carries the offending fixture path, and a
        save named `gen2-COMPARE_OK.synworld` is an ordinary thing for a
        probe to hand this;
      * a marker-SHAPED token is not one either, because the head is
        compared for equality rather than by prefix, so
        `NOT_COMPARE_OK` and `COMPARE_OKAY` both fail to match.

    A Haskell `show`-escaped string can never contain a real newline, so
    no fixture path can split itself across lines to forge a head.
    """
    found = []
    for line in output.splitlines():
        head = line.split(":", 1)[0].strip()
        for marker, outcome, status in _COMPARE_PROTOCOL:
            if head == marker:
                found.append((marker, outcome, status, line.strip()))
                break
    return found


def compare_session_snapshots(
        paths: list[Path]) -> tuple[str, dict | None, str]:
    """Decode every path and report whether all are structurally equal.

    The canonical persistence-state comparison (#767 requirement 1),
    reached through the compiled helper since #2274. Every file is
    compared against the FIRST on both halves the contract names: the
    decoded `SessionSnapshot` (derived `Eq`) and every `lua.<module>`
    component's raw canonical payload bytes.

    Returns `(outcome, report, diagnostic)`:

      * `COMPARE_OK` -- every file matched. `report` names the reference
        and two empty difference lists; `diagnostic` is empty.
      * `COMPARE_MISMATCH` -- at least one file differed. `report`'s
        `snapshotDiffers` and `luaComponentDiffers` name WHICH, in the
        order given, so a caller can diff the reference against the file
        that first diverged rather than against whichever file happened
        to be second.
      * `COMPARE_DECODE_FAILED` -- at least one file would not decode.
        `report`'s `decodeErrors` pairs each path with the production
        codec's own error text.
      * `COMPARE_ERROR` -- the helper could not be resolved, could not be
        run, overran its allowance, or answered in a way this bridge
        will not believe: no protocol line, more than one (whether or
        not they agree), an exit status other than the exact one that
        line carries, or a report that is missing,
        unreadable, or names a different outcome. `report` is None and
        `diagnostic` says which. This is
        a statement about the toolchain, never about the saves.

    Fewer than two paths is `COMPARE_OK` with no subprocess at all: a
    single save is trivially equal to itself, and the helper refuses the
    call as a usage error rather than answering it.
    """
    if len(paths) < 2:
        return COMPARE_OK, {"outcome": COMPARE_OK,
                            "reference": str(paths[0]) if paths else None,
                            "snapshotDiffers": [],
                            "luaComponentDiffers": []}, ""
    with tempfile.NamedTemporaryFile(
            suffix=".json", dir=common.REPO_ROOT, delete=False) as tf:
        report_path = Path(tf.name)
    try:
        returncode, output, why = _invoke_codec(
            ["compare", "--output", str(report_path)]
            + [str(p) for p in paths])
        if returncode is None:
            return COMPARE_ERROR, None, why
        tail = "\n".join(output.splitlines()[-60:])
        markers = ", ".join(m for m, _, _ in _COMPARE_PROTOCOL)

        lines = _compare_protocol_lines(output)
        if not lines:
            return COMPARE_ERROR, None, (
                f"the codec helper's `compare` exited {returncode} without "
                f"a protocol line reporting any of {markers}:\n{tail}")
        # EXACTLY one, not "one distinct outcome". A run that announced
        # its verdict twice did something this bridge does not model --
        # the helper emits one line per run and nothing retries it --
        # and "they agreed, so it is fine" is the reasoning that let a
        # marker-shaped path through in the first place. Two identical
        # lines are as much a malfunction as two conflicting ones; only
        # the diagnostic differs.
        if len(lines) != 1:
            outcomes = sorted({outcome for _m, outcome, _s, _l in lines})
            return COMPARE_ERROR, None, (
                f"the codec helper's `compare` reported {len(lines)} "
                f"protocol lines in one run"
                + (f", naming conflicting outcomes ({', '.join(outcomes)})"
                   if len(outcomes) > 1
                   else f", all naming {outcomes[0]}")
                + f":\n{tail}")
        _marker, outcome, expected_status, line = lines[0]
        # The EXACT status the protocol assigns that line, not merely a
        # matching zero/nonzero sense. A mismatch that exited 2 is a
        # helper that did something other than `exitFailure` after
        # deciding -- an unhandled exception on the way out, say -- and
        # reading it as an ordinary fixture mismatch reports a verdict
        # this bridge has no reason to trust.
        if returncode != expected_status:
            return COMPARE_ERROR, None, (
                f"the codec helper's `compare` reported {line!r} but exited "
                f"{returncode}; the protocol assigns that line exit status "
                f"{expected_status}:\n{tail}")

        # ABSENCE is not the test, for the reason `dump_fixture_descriptors`
        # states: `NamedTemporaryFile(delete=False)` already created the
        # path, so a helper that wrote nothing leaves an EMPTY file -- the
        # condition to judge is whether it parses as the object the
        # protocol promises. The report is REQUIRED, not a refinement: it
        # is the third of the three signals that have to agree, and a
        # verdict this bridge cannot corroborate is a toolchain problem to
        # report as one rather than a claim about the saves to pass on.
        try:
            report = json.loads(report_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            return COMPARE_ERROR, None, (
                f"the codec helper's `compare` reported {line!r} but wrote "
                f"no readable report at {report_path}: {error}\n{tail}")
        if not isinstance(report, dict):
            return COMPARE_ERROR, None, (
                f"the codec helper's `compare` reported {line!r} but wrote "
                f"{type(report).__name__}, not the object the protocol "
                f"promises:\n{tail}")
        if report.get("outcome") != outcome:
            return COMPARE_ERROR, None, (
                f"the codec helper's `compare` reported {line!r} but its "
                f"report names outcome {report.get('outcome')!r}:\n{tail}")
        return outcome, report, ("" if outcome == COMPARE_OK else line)
    finally:
        report_path.unlink(missing_ok=True)
