#!/usr/bin/env python3
"""Atomic fixture/baseline registration (issue #2049, requirement 11).

The ONE owner of `--add-baseline`: fixture-entry construction,
new-baseline descriptor validation, duplicate-fixture refusal and
`--force`, atomic manifest writes, the complete-session real-codec
validation, the component-focused no-op, and rollback to the manifest's
exact prior contents on failure. It consumes the manifest audit
(`load_manifest`) and the shared definitions owner, and nothing
consumes it but `--generate-session`, which DELEGATES here rather than
repeating any of it (requirement 14).

`_run_real_codec_validation` is registration's own subprocess and lives
here, NOT with save_compat_audit_codec's GHCi bridge (requirement 8
correction): it is a different invocation -- `cabal test
synarchy-test-headless --test-options=--match "save migrations"`,
judged by the process return code, reported with a 40-line tail and its
own `--skip-validation` hint. The three GHCi operations keep their own
markers and 60-line tails over in that module. Do not unify the four
behind one wrapper.

The public façade is tools/save_compat_audit.py.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import sys
from pathlib import Path

import save_compat_audit_common as common
import save_compat_audit_manifest as manifest_audit

def _write_manifest_atomically(
        manifest: dict, manifest_path: Path | None = None) -> None:
    """Write the manifest via a same-directory temp file + atomic rename,
    so a crash/interruption mid-write can never leave a half-written,
    unparseable manifest.json behind.

    @manifest_path@ defaults to 'common.MANIFEST_PATH', resolved HERE
    rather than bound as a default argument, so the self-test's
    rebinding of that module attribute is actually seen (issue #2049
    requirement 18)."""
    manifest_path = (common.MANIFEST_PATH if manifest_path is None
                     else manifest_path)
    tmp = manifest_path.with_name(manifest_path.name + ".tmp")
    tmp.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    tmp.replace(manifest_path)


def _build_fixture_entry(args: argparse.Namespace) -> dict:
    fpath = common.REPO_ROOT / args.path
    if not fpath.exists():
        raise SystemExit(f"path '{args.path}' does not exist -- generate the "
                          f"fixture through the real codec FIRST (see "
                          f"docs/save_compat/manifest.json's own "
                          f"'provenance' fields for worked examples), then "
                          f"run this command to register it")
    data = fpath.read_bytes()
    entry = {
        "id": args.fixture_id,
        "path": args.path,
        "kind": args.kind,
        "sha256": hashlib.sha256(data).hexdigest(),
        "sizeBytes": len(data),
        "provenance": args.provenance or "(fill in: how was this fixture generated?)",
        "expectedCanonicalSummary": None,
    }
    if args.summary:
        summary_path = common.REPO_ROOT / args.summary
        if not summary_path.exists():
            raise SystemExit(f"--summary path '{args.summary}' does not "
                              f"exist -- author the expected-canonical-"
                              f"summary JSON first (see an existing "
                              f"*.expected.json for the schema), then "
                              f"register together")
        try:
            json.loads(summary_path.read_text(encoding="utf-8"))
        except ValueError as e:
            raise SystemExit(f"--summary path '{args.summary}' is not valid "
                              f"JSON: {e}")
        entry["expectedCanonicalSummary"] = args.summary
    elif args.kind == "complete-session":
        raise SystemExit("a 'complete-session' fixture needs --summary "
                          "(requirement 12/14: every complete-session "
                          "fixture must have an expected canonical result "
                          "to validate against)")
    return entry


def _run_real_codec_validation() -> tuple[bool, str]:
    """Run the SAME "save migrations" hspec gate CI already runs
    (test-headless's manifest-driven "manifest-declared fixtures decode
    and migrate to their expected canonical result" test), scoped via
    --match: the real, already-existing proof that every complete-
    session fixture the manifest declares actually decodes/migrates/
    assembles correctly through World.Save.Envelope/Component's real
    registry and matches its own expectedCanonicalSummary -- not merely
    that its bytes are present and checksummed (which is all the rest of
    this module's audit() can ever prove, since only Haskell can run the
    cereal codec). Returns (passed, last-40-lines-of-output)."""
    try:
        proc = subprocess.run(
            ["cabal", "test", "synarchy-test-headless",
             "--test-options=--match \"save migrations\""],
            cwd=common.REPO_ROOT, capture_output=True, text=True,
            timeout=1800)
    except FileNotFoundError:
        return False, ("'cabal' was not found on PATH -- cannot validate "
                        "through the real codec in this environment; pass "
                        "--skip-validation if that is expected here (the "
                        "checked-in CI gate will still catch a bad fixture "
                        "on the next push)")
    output = (proc.stdout or "") + (proc.stderr or "")
    tail = "\n".join(output.splitlines()[-40:])
    return proc.returncode == 0, tail


def _finalize_manifest_write(
        manifest: dict, manifest_path: Path, kind: str,
        skip_validation: bool, success_message: str) -> int:
    """Write the manifest, then -- for a "complete-session" fixture,
    unless --skip-validation was passed -- run it through the REAL
    codec (requirement 21: "an explicit real-codec generation mode/
    helper that validates the output"), rolling the manifest back to its
    exact prior content if that validation fails. A "component-focused"
    fixture (e.g. a single Lua component payload) has no generic gate to
    run -- test-headless's manifest-driven test only ever iterates
    complete-session fixtures -- so this is a documented no-op for that
    kind: write a hand-authored hspec test exercising it directly
    instead (see Test.Headless.Lua.SaveModules's "tracked v1 fixtures
    from disk" for the pattern this repo already follows)."""
    previous_text = (manifest_path.read_text(encoding="utf-8")
                      if manifest_path.exists() else None)
    _write_manifest_atomically(manifest, manifest_path)

    if kind != "complete-session":
        print(f"{success_message}\n(kind='{kind}': no generic real-codec "
              f"validation gate exists for this kind -- add or extend a "
              f"hand-written hspec test exercising it directly, mirroring "
              f"Test.Headless.Lua.SaveModules's \"tracked v1 fixtures from "
              f"disk\" pattern, then run it yourself)")
        return 0
    if skip_validation:
        print(f"{success_message}\n(--skip-validation passed: NOT run "
              f"through the real codec here -- the checked-in CI gate "
              f"will still catch a bad fixture on the next push)")
        return 0

    ok, tail = _run_real_codec_validation()
    if ok:
        print(f"{success_message}\nvalidated through the real codec: "
              f"cabal test synarchy-test-headless --test-options="
              f"'--match \"save migrations\"' PASSED")
        return 0

    if previous_text is None:
        manifest_path.unlink()
    else:
        manifest_path.write_text(previous_text, encoding="utf-8")
    print(f"REAL-CODEC VALIDATION FAILED -- rolled '{manifest_path}' back "
          f"to its previous content (the fixture bytes/summary files "
          f"already on disk were left untouched; only this registration "
          f"was undone). Fix the fixture and re-run --add-baseline. Last "
          f"lines of `cabal test`'s output:\n{tail}", file=sys.stderr)
    return 1


def cmd_add_baseline(args: argparse.Namespace) -> int:
    manifest = manifest_audit.load_manifest(common.MANIFEST_PATH)
    existing_baseline = next(
        (b for b in manifest.get("baselines", []) if b.get("id") == args.baseline_id),
        None)

    try:
        new_fixture = _build_fixture_entry(args)
    except SystemExit as e:
        print(e, file=sys.stderr)
        return 1

    if existing_baseline is None:
        # A brand new baseline: requires the full descriptor so the
        # manifest entry is complete on creation, never a bare fixture
        # with no declared components/migration target to check it
        # against.
        missing = [flag for flag, val in
                   [("--description", args.description),
                    ("--migration-target", args.migration_target),
                    ("--migrated-by", args.migrated_by),
                    ("--components", args.components)]
                   if not val]
        if missing:
            print(f"baseline '{args.baseline_id}' does not exist yet -- "
                  f"creating a NEW baseline also requires: {', '.join(missing)}",
                  file=sys.stderr)
            return 1
        try:
            components = json.loads(args.components)
        except ValueError as e:
            print(f"--components is not valid JSON: {e}", file=sys.stderr)
            return 1
        manifest.setdefault("baselines", []).append({
            "id": args.baseline_id,
            "description": args.description,
            "declaredAt": args.declared_at or "(fill in: YYYY-MM-DD)",
            "declaredByIssue": args.declared_by_issue,
            "supportStatus": "supported",
            "migrationTarget": args.migration_target,
            "migratedBy": args.migrated_by,
            "components": components,
            "fixtures": [new_fixture],
        })
        return _finalize_manifest_write(
            manifest, common.MANIFEST_PATH, args.kind,
            args.skip_validation,
            f"created baseline '{args.baseline_id}' with fixture "
            f"'{args.fixture_id}': sha256={new_fixture['sha256']} "
            f"sizeBytes={new_fixture['sizeBytes']}")

    existing_fixture = next(
        (f for f in existing_baseline.get("fixtures", [])
         if f.get("id") == args.fixture_id), None)
    if existing_fixture is not None and not args.force:
        print(f"refusing to overwrite existing fixture '{args.fixture_id}' "
              f"on baseline '{args.baseline_id}' -- pass --force if this is "
              f"a deliberate re-registration (e.g. after regenerating "
              f"through the real codec)", file=sys.stderr)
        return 1
    if existing_fixture is not None:
        existing_baseline["fixtures"] = [
            new_fixture if f.get("id") == args.fixture_id else f
            for f in existing_baseline["fixtures"]]
    else:
        existing_baseline.setdefault("fixtures", []).append(new_fixture)
    return _finalize_manifest_write(
        manifest, common.MANIFEST_PATH, args.kind, args.skip_validation,
        f"registered fixture '{args.fixture_id}' on baseline "
        f"'{args.baseline_id}': sha256={new_fixture['sha256']} "
        f"sizeBytes={new_fixture['sizeBytes']}")
