#!/usr/bin/env python3
"""Current-schema guard for the save format (issue #2916, PSP-2 of #2914).

Before public release a persisted shape change advances every affected
component, metadata, Lua or framing version, and each affected carrier
then accepts ONLY its current version (owner decision, 2026-11-05;
design: docs/designs/prerelease_save_policy_design.md, D-1/D-5, P-2).
This tool enforces that mechanically. It does not analyse whether an old
decoder could still be safe -- an affected carrier either accepts exactly
one version and has its legacy routes off, or it fails.

=== What it checks

  1. The record `docs/save_compat/current_schema.json` is exactly the
     projection of the CURRENT source (stale, missing, extra or
     unregistered carriers fail with the carrier named).
  2. With `--base-ref REF`, the candidate source is compared with the
     source at the merge-base of REF and HEAD, both computed FROM SOURCE
     (the record is never the comparison input, so regenerating or
     deleting it cannot hide drift). Every detected shape change --
     including an enum append -- needs a strictly higher real on-disk
     version for that carrier, every affected carrier accepts exactly its
     current version, and every enabled exact-shape legacy route that
     decodes through an affected carrier is disabled. The resolved base
     and candidate SHAs are printed; a comparison of a commit with itself
     (no uncommitted changes) fails, and so does an unresolvable base.
  3. With no base ref, only (1) runs (offline, no git).

=== Carriers and what "shape" covers

  component   every `componentCodec ComponentSpec`: the `data`/`newtype`/
              `type` declarations (records, enums, newtypes, aliases)
              transitively reachable from its `*DTO*` wire roots, plus
              every hand-written `Serialize` instance on a reached type
  metadata    `SaveMetadata` and its closure; the frozen
              `World.Save.Compat.MetadataV<n>` mirrors count as its LEGACY
              shape while that version is still accepted
  lua         each `saveMods.register` module. A Lua payload has no
              declared schema, so a Lua module is a custom-serializer
              carrier: its mechanical shape is the shared wire encoding,
              `scripts/lib/data_codec.lua`, which every Lua carrier owns
  framing     `envelope_framing_fingerprint` (Types.hs bindings + Codec.hs)
              and `currentEnvelopeVersion`
  legacy      the B1 `session` (SessionV90) and B2 `lua-state` carriers,
              read only through the `b1-session` / `b2-lua-state` routes,
              which are enabled iff `tryLegacy*Fallbacks` still names
              their decoders

A carrier's LEGACY shape is what only its still-accepted older versions
reach (frozen DTOs named by `csOlderVersions` migrations). Editing one
while it is still reachable fails until the older version is removed;
no bump is needed if the current shape is unchanged.

=== Canonical forms

Anything the guard reads must be in ONE canonical form (owner ruling,
2026-11-05, "Strict canonical forms"). Any other spelling -- an alias, a
qualified or forwarding call, a comment inside the construct, an
expression, a trailing operator -- is not parsed: the build fails with
`<file>:<line>: unsupported syntax: rewrite in canonical form`, so a
mutation makes the guard fail instead of pass.

  Haskell version bindings   `name = <integer literal>` alone on its line,
                             defined once: currentEnvelopeVersion,
                             metadataComponentVersion,
                             legacyMetadataComponentVersion,
                             luaStateComponentVersion,
                             sessionComponentVersion; `csVersion = <int>`
  metadata accepted versions one `metadataComponentInputVersions =
                             [ <binding or integer>, ... ]`
  older component decoders   `atVersion <int> <name>` where `<name>` has an
                             explicit signature `<name> ∷ <FooDTOvN> → ...`
                             whose first argument is the *DTO* wire root
  Lua registration           `local <name> = require("scripts.lib.
                             save_modules")` on its own line, then
                             `saveMods.register("<id>", {`; exactly one
                             `version = <int>,`, `required = <bool>,` and
                             `inputVersions = { <ints> },` per table
  framing decoder            every use is `decodeEnvelope
                             defaultEnvelopeLimits currentEnvelopeVersion`;
                             Codec.hs rejects exactly `version ≢
                             expectedVersion`
  legacy-route dispatch      `tryLegacyEnvelopeFallbacks` and
                             `tryLegacyMetadataFallbacks` in one of four
                             exact forms (see `dispatcher_forms`); a route
                             is disabled only by its explicit form

Whitespace, comments outside those constructs and strictness bangs never
change a shape; string-literal contents always do.

=== Limits of source inspection (read before trusting a green run)

  - Arbitrary custom serializers: a hand-written `put`/`get` is hashed as
    text of the instance block only. Helpers it calls elsewhere, and a
    change in MEANING with identical text, are invisible.
  - Lua payloads: only the shared data codec is mechanical. A new or
    changed `aiState` field in any `scripts/unit_ai_*.lua`, or any other
    Lua table shape, is NOT detected.
  - Positional values: two same-typed positional fields swapped, or a
    unit/encoding change on an unchanged type, produce no source diff.
  - Type resolution is by bare name (qualifiers are dropped); a name
    declared in two modules fails as ambiguous unless listed in
    `AMBIGUOUS_TYPE_NAMES`. Types from outside src/ and app/ are not
    followed.
  - Comment, layout, strictness-bang and import-only edits are ignored by
    design.

An encoding or meaning change outside mechanical detection STILL requires
a version bump, current-only acceptance, explicit review evidence and a
test that distinguishes the old meaning from the new one.

=== Usage

  python3 tools/current_save_schema_audit.py --self-test
  python3 tools/current_save_schema_audit.py                 # record only
  python3 tools/current_save_schema_audit.py --base-ref origin/master
  python3 tools/current_save_schema_audit.py --update-record
  python3 tools/current_save_schema_audit.py --base-ref origin/master --json

`--update-record` regenerates the record from source and nothing else;
it is a projection, never a way to pass the base-ref check. Exit 0 =
pass, 1 = a rule failed or evidence is missing.

Modules (each imports only those above it): `_model` reads one tree,
`_compare` holds the rules, `_git` resolves and exports revisions,
`_selftest` is loaded only by `--self-test`.
"""
from __future__ import annotations

import argparse
import json
import sys
import tempfile
from pathlib import Path

import current_save_schema_compare as compare
import current_save_schema_git as vcs
import current_save_schema_model as model

REPO_ROOT = Path(__file__).resolve().parent.parent


def load_record(root: Path) -> dict | None:
    path = root / model.RECORD_REL
    if not path.is_file():
        return None
    return json.loads(path.read_text(encoding="utf-8"))


def evaluate(root: Path, base_ref: str | None) -> dict:
    """Run the checks; the result carries the resolved evidence."""
    result: dict = {"ok": False, "problems": [], "baseRef": base_ref,
                    "baseSha": None, "candidateSha": None, "dirty": None,
                    "carriers": 0, "routes": 0}
    problems: list[str] = result["problems"]
    try:
        cand = model.build_model(root)
    except model.SchemaError as exc:
        problems.append(str(exc))
        return result
    result["carriers"], result["routes"] = len(cand.carriers), len(cand.routes)
    problems += compare.check_record(
        cand, load_record(root), model.RECORD_REL)
    if base_ref is not None:
        try:
            ev = vcs.resolve(root, base_ref)
        except vcs.EvidenceError as exc:
            problems.append(f"comparison evidence missing: {exc}")
            return result
        result.update(baseSha=ev.base_sha, candidateSha=ev.candidate_sha,
                      dirty=ev.dirty, describe=ev.describe())
        evidence_problems = compare.check_evidence(ev)
        if evidence_problems:
            problems += evidence_problems
        else:
            with tempfile.TemporaryDirectory(prefix="schema-base-") as tmp:
                try:
                    vcs.export_tree(root, ev.base_sha, Path(tmp))
                    base = model.build_model(Path(tmp))
                except (vcs.EvidenceError, model.SchemaError) as exc:
                    problems.append(
                        f"comparison evidence missing: base {ev.base_sha} "
                        f"could not be read: {exc}")
                    return result
            problems += compare.compare(base, cand)
    result["ok"] = not problems
    return result


def run_check(root: Path, base_ref: str | None, as_json: bool = False) -> int:
    result = evaluate(root, base_ref)
    if as_json:
        print(json.dumps(result, indent=2, sort_keys=True))
        return 0 if result["ok"] else 1
    if result.get("describe"):
        print(f"current-schema: {result['describe']}")
    for problem in result["problems"]:
        print(f"FAIL: {problem}")
    if not result["ok"]:
        return 1
    scope = "against the base" if base_ref else "record only"
    print(f"current-schema: OK ({result['carriers']} carriers, "
          f"{result['routes']} legacy routes, {scope})")
    return 0


def run_update(root: Path) -> int:
    try:
        cand = model.build_model(root)
    except model.SchemaError as exc:
        print(f"FAIL: {exc}")
        return 1
    path = root / model.RECORD_REL
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(compare.render_record(cand), encoding="utf-8")
    print(f"wrote {model.RECORD_REL} ({len(cand.carriers)} carriers); "
          f"review the diff -- this does not satisfy --base-ref")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Guard that every persisted shape change advances the "
                    "affected carrier's version and leaves it accepting "
                    "only that version.")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--self-test", action="store_true",
                       help="run the guard's synthetic base/candidate suite")
    group.add_argument("--update-record", action="store_true",
                       help=f"regenerate {model.RECORD_REL} from source")
    group.add_argument("--base-ref", metavar="REF",
                       help="also compare the candidate with the merge-base "
                            "of REF and HEAD (e.g. origin/master)")
    parser.add_argument("--json", action="store_true",
                        help="print the result, resolved SHAs included, "
                             "as JSON")
    args = parser.parse_args()
    if args.self_test:
        from current_save_schema_selftest import main_self_test
        return main_self_test()
    if args.update_record:
        return run_update(REPO_ROOT)
    return run_check(REPO_ROOT, args.base_ref, args.json)


if __name__ == "__main__":
    sys.exit(main())
