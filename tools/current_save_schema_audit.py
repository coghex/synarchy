#!/usr/bin/env python3
"""Current-schema save guard (issue #2916, PSP-2 of #2914).

Before release a persisted shape change advances every affected
component, metadata, Lua or framing version, and each affected carrier
then accepts only its current version, with its affected exact-shape
legacy routes disabled. No historical decoder is analysed and no
migration or historical fixture is required.

This driver is orchestration and comparison ONLY. Every shape, version,
registration and acceptance fact comes from the COMPILED extractor (the
"current save schema" group of `synarchy-test-headless`): GHC
Generic/Typeable walks of the actual codec DTO types, the live Haskell
registry, the metadata and framing values, and the Lua registry
evaluated after the production registrations load in the headless Lua
harness. No source parser or source-text fingerprint is authoritative.

    --base-ref REF      compare the candidate against REF (merge-base)
    --update-record     rewrite docs/save_compat/current_schema.json from
                        the compiled evidence; this does NOT replace the
                        independent --base-ref check
    --self-test         comparator, base-record and bootstrap rules, plus
                        the compiled fixture catalogue
    --json              machine-readable result on stdout

Routine comparison reads the validated record COMMITTED at the base
revision and compiles ONLY the candidate; it never builds the base. That
record's own freshness rests on the merge-time check PSP-5 (#2919)
enforces on every merged commit, which this tool states and never claims
to have rebuilt. Only first introduction (a base containing none of the
guard's artifacts) extracts the actual pre-guard base as well.

Docs: docs/current_save_schema_guard.md. Not wired into CI, `make ci`
or any default until PSP-5.
"""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from current_save_schema_compare import (  # noqa: E402
    RecordError, check_fresh, compare, parse_record, render_record)
from current_save_schema_git import (  # noqa: E402
    GUARD_ARTIFACTS, RECORD_REL, EvidenceError, blob_id, check_distinct,
    guard_artifacts_at, read_blob, resolve)

ROOT = Path(__file__).resolve().parent.parent


def resolve_base_record(root: Path, rev, bootstrap_fn):
    """The base-side record and its provenance.

    A base containing any guard artifact must carry a valid record: a
    missing or malformed one FAILS, and its absence can never enable the
    bootstrap. Only a base containing none of them is bootstrapped."""
    present = guard_artifacts_at(root, rev.base_sha)
    if not present:
        record, prov = bootstrap_fn(root, rev)
        return record, dict(prov, mode="bootstrap", guardArtifacts=[])
    text = read_blob(root, rev.base_sha, RECORD_REL)
    if text is None:
        raise EvidenceError(
            f"the base {rev.base_sha[:12]} already contains the guard "
            f"({', '.join(present)}) but no {RECORD_REL}: a missing base "
            f"record never enables the bootstrap, and an empty or "
            f"candidate-side base is never substituted")
    try:
        record = parse_record(text, f"base record {RECORD_REL}@{rev.base_sha[:12]}")
    except RecordError as exc:
        raise EvidenceError(str(exc)) from exc
    return record, {
        "mode": "committed-record",
        "recordBlob": blob_id(root, rev.base_sha, RECORD_REL),
        "validation": "structural (format, extractor, complete coverage, "
                      "consistent versions/acceptance/ownership, required "
                      "fields)",
        "freshness": "NOT rebuilt here: it rests on the merge-time check "
                     "PSP-5 (#2919) enforces on every merged commit",
        "guardArtifacts": present}


def check(root: Path, base_ref: str, *, extract=None, bootstrap_fn=None) -> dict:
    """Run the comparison. `extract`/`bootstrap_fn` are injectable for
    the self-test; the defaults build and run the real extractor."""
    import current_save_schema_extract as ex
    extract = extract or (lambda r: ex.build_and_run(r))
    bootstrap_fn = bootstrap_fn or ex.bootstrap_base
    rev = resolve(root, base_ref)
    problems = check_distinct(rev)
    result: dict = {"baseRef": rev.base_ref, "baseTip": rev.base_tip,
                    "baseSha": rev.base_sha, "candidateSha": rev.candidate_sha,
                    "candidateEdits": rev.edit_identity, "problems": problems,
                    "notes": []}
    if problems:
        return result
    base, prov = resolve_base_record(root, rev, bootstrap_fn)
    result["base"] = prov
    ev = extract(root)
    result["candidate"] = {
        "examples": ev.examples, "failures": ev.failures,
        "extractorSha256": ev.executable_sha256, "toolchain": ev.toolchain,
        "mode": "compiled from the candidate tree only"}
    committed = root / RECORD_REL
    result["problems"] += check_fresh(
        ev.record, committed.read_text() if committed.is_file() else None,
        RECORD_REL)
    found, notes = compare(base, ev.record)
    result["problems"] += found
    result["notes"] += notes
    return result


def update_record(root: Path, extract=None) -> Path:
    import current_save_schema_extract as ex
    ev = (extract or (lambda r: ex.build_and_run(r)))(root)
    path = root / RECORD_REL
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(render_record(ev.record))
    return path


def render(result: dict) -> str:
    lines = [f"base      {result['baseRef']} -> tip {result['baseTip'][:12]}, "
             f"merge-base {result['baseSha'][:12]}",
             f"candidate {result['candidateSha'][:12]} "
             f"(tracked edits: {result['candidateEdits']})"]
    base = result.get("base")
    if base:
        if base["mode"] == "bootstrap":
            lines.append("base record: BOOTSTRAP -- the guard did not exist at "
                         "the base; the actual pre-guard base was extracted "
                         f"(adapter {base['adapterPatchSha256'][:12]}, "
                         f"{base['examples']} examples)")
        else:
            lines.append(f"base record: committed blob {base['recordBlob']} "
                         f"(validated structurally; {base['freshness']})")
    cand = result.get("candidate")
    if cand:
        lines.append(f"candidate extraction: {cand['examples']} examples, "
                     f"{cand['failures']} failures, extractor "
                     f"{cand['extractorSha256'][:12]}, ghc "
                     f"{cand['toolchain'].get('ghc')} ({cand['mode']})")
    lines += [f"note: {n}" for n in result["notes"]]
    if result["problems"]:
        lines.append(f"FAIL ({len(result['problems'])} problem(s)):")
        lines += [f"  - {p}" for p in result["problems"]]
    else:
        lines.append("OK: no current-schema violation")
    return "\n".join(lines)


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--base-ref")
    ap.add_argument("--update-record", action="store_true")
    ap.add_argument("--self-test", action="store_true")
    ap.add_argument("--json", action="store_true")
    ap.add_argument("--root", type=Path, default=ROOT, help=argparse.SUPPRESS)
    args = ap.parse_args(argv)
    if args.self_test:
        import current_save_schema_selftest as st
        return st.run()
    try:
        if args.update_record:
            path = update_record(args.root)
            print(f"wrote {path.relative_to(args.root)} from compiled evidence "
                  f"(this does not replace the --base-ref check)")
            return 0
        if not args.base_ref:
            ap.error("--base-ref is required (or --update-record / --self-test)")
        result = check(args.root, args.base_ref)
    except EvidenceError as exc:
        print(f"FAIL: {exc}", file=sys.stderr)
        return 1
    print(json.dumps(result, indent=2, sort_keys=True) if args.json
          else render(result))
    return 1 if result["problems"] else 0


if __name__ == "__main__":
    sys.exit(main())
