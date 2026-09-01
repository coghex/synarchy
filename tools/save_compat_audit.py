#!/usr/bin/env python3
"""Save-compatibility audit + fixture registration tool (issue #766,
save-overhaul C4, requirements 13/14).

Guards docs/save_compat/manifest.json -- the machine-readable record of
every save-format baseline this build is declared to keep loadable
through explicit migrations -- against silent drift:

  - Every fixture the manifest declares actually exists on disk.
  - A tracked binary fixture's bytes have not been hand-edited (its
    sha256 matches the manifest's recorded value).
  - The manifest's envelopeFramingVersion agrees with
    World.Save.Envelope.currentEnvelopeVersion -- a framing bump without
    an explicit manifest update (a new format epoch) fails loudly rather
    than silently reinterpreting every tracked fixture under a changed
    contract.
  - Every baseline's frozen-DTO source (World.Save.Compat.SessionV90)
    fingerprint matches what the manifest recorded when that baseline was
    declared -- refactoring a frozen type changes its fingerprint, so an
    editor must consciously re-run --add-baseline (or acknowledge the
    change) rather than silently altering historical bytes.
  - Every baseline's declared components[] cross-checks against the REAL
    current Haskell (World.Save.Component.*'s ccVersion/ccInputVers/
    ccRequired) and Lua (scripts/unit_ai_save.lua's/scripts/
    building_spawn.lua's version/inputVersions/required) registries (see
    real_component_registry() / audit_component_versions()): a declared
    component/version must still exist and still be an accepted input
    version (catches a decoder silently dropped); every REQUIRED
    component -- regardless of how many versions it accepts -- must be
    tracked by at least one baseline (catches a brand-new required
    component shipping with no baseline ever proving it has an accounted
    default/migration policy, which a version-count-only check can never
    see for a component that has only ever had one version); and every
    component with more than one accepted input version must additionally
    have its OLDEST one tracked by some baseline (catches a version bump
    that shipped with no compatibility fixture ever validating the
    historical shape it migrates from).
  - Round-6 review's per-baseline (not merely aggregate-across-baselines)
    required-component coverage (see audit_modern_baseline_components_
    complete() / audit_b1_migration_covers_page_scoped_components()):
    every "current"-target baseline whose components[] doesn't declare
    the frozen legacy "session" id is a MODERN per-component-registry
    session, and a valid one of those can never structurally omit any
    required component (decodeEnvelope refuses an incomplete modern
    envelope outright) -- so its components[] must declare ALL of them,
    or the manifest is under-documenting what its own fixture actually
    contains. The b1-initial-session baseline can never declare that
    full set (it IS the frozen {metadata, session} alternative), so its
    real guarantee is checked differently: World.Save.Compat.SessionV90.
    migrateSessionV90's own source must still reference the named apply*
    helper for every current page-scoped component -- the closest a
    static Python audit can get to "this legacy migration still threads
    every required component through", short of literally compiling it.

This is a static presence/fingerprint check, not itself a proof that a
fixture migrates correctly -- that real decode/migrate/assemble/
canonical-result cross-check (requirement 14) lives in test-headless's
"save migrations" hspec gate ("manifest-declared fixtures decode and
migrate to their expected canonical result", which reads this SAME
manifest and every fixture/expectedCanonicalSummary it declares), backed
by tools/save_compat_migration_probe.py's real-engine round trip. Run:
cabal test synarchy-test-headless --test-options='--match "save migrations"'

Usage:
  python3 tools/save_compat_audit.py                # blocking audit (CI)

  # Register a fixture on an EXISTING baseline (checksum + summary,
  # atomically):
  python3 tools/save_compat_audit.py --add-baseline \\
      --baseline-id b1-initial-session --fixture-id my-fixture \\
      --path test-headless/data/save-compat/my-fixture.bin \\
      --kind complete-session \\
      --summary test-headless/data/save-compat/my-fixture.expected.json

  # Register a fixture AND create its baseline entry together (id not
  # yet declared):
  python3 tools/save_compat_audit.py --add-baseline \\
      --baseline-id my-new-baseline --fixture-id my-fixture \\
      --path test-headless/data/save-compat/my-fixture.bin \\
      --kind complete-session \\
      --summary test-headless/data/save-compat/my-fixture.expected.json \\
      --description "..." --migration-target current \\
      --migrated-by "World.Save.Compat.SessionV90.migrateSessionV90" \\
      --components '[{"id":"metadata","version":1,"required":true}, ...]'

  Either form refuses to overwrite an already-registered fixture id
  without --force. The raw fixture BYTES and --summary JSON must already
  exist (generated through the real codec -- see the manifest's own
  "provenance" fields for worked examples, and tools/README.md /
  docs/save_compat -- for a Haskell "complete-session" fixture that
  means a real headless-engine boot + engine.saveWorld, or a GHCi/cabal
  repl session calling World.Save.Envelope.Codec.encodeEnvelope
  directly; for a Lua "component-focused" fixture, a GHCi/cabal repl
  session driving a real HsLua VM through scripts/lib/data_codec.lua's
  M.encode -- see test-headless/data/save-compat/lua-unit-ai-v1.bin's
  manifest provenance for a worked example); this command performs the
  atomic bookkeeping (checksum, size, manifest/summary wiring) AND, for
  a "complete-session" fixture, immediately runs it through the SAME
  real codec test-headless's CI gate uses (cabal test
  synarchy-test-headless --test-options='--match "save migrations"'),
  automatically rolling the manifest back to its exact prior content if
  that fails -- so a bad fixture registration is never left committed
  even locally. Pass --skip-validation to register without running that
  check (e.g. no cabal toolchain available here); a "component-focused"
  fixture has no generic gate to run this way at all (see
  _finalize_manifest_write's docstring) and needs its own hand-written
  hspec test instead.

  # GENERATE a brand-new CURRENT-format complete-session fixture through
  # the real codec end to end (requirement 21: a real generation mode,
  # not just validation of already-hand-built bytes) -- boots an actual
  # headless engine, inits a world, optionally spawns ONE building and/
  # or ONE unit, calls engine.saveWorld (the SAME production save path
  # real gameplay uses), then derives its canonical summary DIRECTLY
  # from the real decoded snapshot (see dump_canonical_summary) rather
  # than hand-transcribing values -- then registers + validates exactly
  # like --add-baseline above (this literally delegates to it once the
  # bytes/summary exist):
  python3 tools/save_compat_audit.py --generate-session \\
      --baseline-id my-new-baseline --fixture-id my-fixture \\
      --path test-headless/data/save-compat/my-fixture.bin \\
      --summary test-headless/data/save-compat/my-fixture.expected.json \\
      --seed 42 --world-size 8 --plate-count 3 \\
      --spawn-building cargo_hold_S --spawn-unit acolyte \\
      --setup-lua "return unit.addItem({uid}, 'bandage')" \\
      --setup-lua "return unit.depositToCargo({uid}, {bid}, 'bandage')" \\
      --description "..." --migration-target current \\
      --migrated-by "..." --components '[...]'

  This can only ever produce a fixture at the CURRENT wire format -- a
  live engine never writes a historical shape. A baseline documenting an
  OLDER version (a frozen legacy DTO, or a component spliced back to an
  earlier ccInputVers) is inherently a distinct, bespoke operation (there
  is no "generate a v1 payload" button in the live game either), and
  stays the manual decode/splice-then---add-baseline workflow this
  manifest's own fixtures' "provenance" fields document (see
  b3-lua-versioned-session-v1 for the most recent worked example).

  Stages the fixture, its summary, AND the manifest together (round-6
  review): a failure at ANY stage -- generation, canonical-summary
  derivation, or the real-codec registration/validation --add-baseline
  itself runs -- restores ALL THREE to their exact prior state (or
  removes whichever ones didn't exist before this invocation), never
  leaving an orphaned or stale-but-checksum-mismatched file behind.

Exit codes: 0 = every declared fixture/fingerprint is intact,
1 = one or more violations (see printed detail).

Module layout (issue #2049)
---------------------------
This file is the public executable FAÇADE and holds only the contract
above, the CLI, its per-mode argument validation, and command dispatch.
Every implementation body lives with its owner:

  save_compat_audit_common.py       shared paths/locations/constants
  save_compat_audit_components.py   Haskell/Lua component discovery and
                                    the version-coverage policy
  save_compat_audit_fingerprint.py  source fingerprints and envelope/
                                    metadata version discovery
  save_compat_audit_codec.py        the real-codec (GHCi) bridge
  save_compat_audit_manifest.py     the blocking manifest audit
  save_compat_audit_register.py     --add-baseline registration
  save_compat_audit_generate.py     --generate-session generation

Dependencies run one way: common is imported by everyone; components,
fingerprint and codec are leaf services; the manifest audit consumes
those; registration consumes the audit; generation consumes the codec
bridge and delegates to registration; this façade imports each command
owner for dispatch only.

`REPO_ROOT` and `dump_canonical_summary` are deliberately RE-EXPORTED
here. Four sibling tools import them from this module by name --
tools/persistence_snapshot.py, tools/persistence_contract_sweep.py,
tools/persistence_contract_probe.py and
tools/save_compat_migration_probe.py -- and the split must not break a
CI-eligible probe or a tier-2 gate. A re-export is not an owner-specific
implementation body.
"""
from __future__ import annotations

import argparse
import sys

import save_compat_audit_generate as generate
import save_compat_audit_manifest as manifest_audit
import save_compat_audit_register as register
from save_compat_audit_codec import dump_canonical_summary  # noqa: F401
from save_compat_audit_common import REPO_ROOT  # noqa: F401

#: Re-exported for the four sibling tools that import them from this
#: module by name (see the module docstring). Named here so a future
#: edit cannot drop them as "unused" -- they have no in-module caller.
__all__ = ["REPO_ROOT", "dump_canonical_summary", "main"]


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--add-baseline", action="store_true",
                     help="atomically register a fixture (and, if new, its "
                          "whole baseline entry) instead of auditing")
    ap.add_argument("--generate-session", action="store_true",
                     help="generate a brand-new CURRENT-format complete-"
                          "session fixture through a real headless engine, "
                          "then register it exactly like --add-baseline "
                          "(requirement 21's real generation mode)")
    ap.add_argument("--seed", type=int, default=42,
                     help="--generate-session only, default 42")
    ap.add_argument("--world-size", type=int, default=8,
                     help="--generate-session only, default 8")
    ap.add_argument("--plate-count", type=int, default=3,
                     help="--generate-session only, default 3")
    ap.add_argument("--page-id", default="generated_page",
                     help="--generate-session only, default 'generated_page'")
    ap.add_argument("--world-name", default=None,
                     help="--generate-session only: the page's #707 display "
                          "name. Required before --language-seed can attach "
                          "provenance (there is no identity to attach it to "
                          "without one)")
    ap.add_argument("--world-gloss", default=None,
                     help="--generate-session only: the display name's "
                          "English gloss")
    ap.add_argument("--language-seed", default=None,
                     help="--generate-session only: the #1092 language seed, "
                          "as a decimal string, that --world-name was "
                          "rendered from. Attaching it is what makes the "
                          "generated fixture's placed locations carry real "
                          "generated names and glosses (#1101) rather than "
                          "definition labels")
    ap.add_argument("--language-version", type=int, default=None,
                     help="--generate-session only: the language's generator "
                          "version; defaults to the engine's current one")
    ap.add_argument("--name-expr", default=None,
                     help="--generate-session only: the #1104 encoded name "
                          "expression --world-name was rendered from (e.g. "
                          "'Modifier:ASH:LAND'), exactly as world.suggestName "
                          "reports it. Attaching it is what gives the "
                          "generated fixture's page identity a real etymology "
                          "source rather than the absent case")
    ap.add_argument("--spawn-building", default=None,
                     help="--generate-session only: a real building def "
                          "name to spawn at (0,0), e.g. cargo_hold_S")
    ap.add_argument("--spawn-unit", default=None,
                     help="--generate-session only: a real unit def name "
                          "to spawn, e.g. acolyte (at --spawn-unit-at)")
    ap.add_argument("--spawn-unit-at", default="0,0", metavar="GX,GY",
                     help="--generate-session only: the tile to spawn "
                          "--spawn-unit on, default '0,0'. A fixture that "
                          "must capture state a unit only acquires SOMEWHERE "
                          "specific (e.g. #915's per-unit location memory, "
                          "learned by SEEING a placed location) spawns it "
                          "there rather than at the origin")
    ap.add_argument("--settle-seconds", type=float, default=0.0,
                     help="--generate-session only: seconds to let the "
                          "engine + Lua ticks run between the spawns and "
                          "engine.saveWorld, for state that is acquired by "
                          "a tick rather than written by a spawn verb")
    ap.add_argument("--setup-lua", action="append", default=None,
                     metavar="STMT",
                     help="--generate-session only, repeatable: a "
                          "single-line Lua statement run after the spawns "
                          "and before the settle, with {bid}/{uid} "
                          "substituted for the spawned ids. For state a "
                          "real ACTION writes rather than a spawn verb or "
                          "a tick (e.g. #1087's container knowledge, "
                          "revealed only by a completed storage "
                          "interaction). A statement that errors or "
                          "returns false/nil fails generation")
    ap.add_argument("--require-lua", default=None, metavar="EXPR",
                     help="--generate-session only: a single-line Lua "
                          "expression that must evaluate to true before "
                          "the save, re-tried across --settle-seconds. "
                          "Generation FAILS if it never becomes true, so a "
                          "fixture cannot silently come out missing the very "
                          "state it was created to capture")
    ap.add_argument("--port", type=int, default=9280,
                     help="--generate-session only: debug-console port "
                          "for the generation engine boot")
    ap.add_argument("--baseline-id", help="baseline id (new or existing)")
    ap.add_argument("--fixture-id", help="fixture id within that baseline")
    ap.add_argument("--path", help="fixture file path, repo-relative -- "
                                    "already generated through the real codec")
    ap.add_argument("--kind", choices=["complete-session", "component-focused"],
                     help="fixture kind (requirement 11)")
    ap.add_argument("--summary", help="expected-canonical-summary JSON path, "
                                        "repo-relative (required for "
                                        "complete-session fixtures)")
    ap.add_argument("--provenance", help="how this fixture was generated "
                                           "(free text, recorded verbatim)")
    ap.add_argument("--description", help="baseline description (new baseline only)")
    ap.add_argument("--migration-target", help="e.g. 'current' (new baseline only)")
    ap.add_argument("--migrated-by", help="the migration function/codec path "
                                            "(new baseline only)")
    ap.add_argument("--components", help="JSON array of {id,version,required} "
                                           "(new baseline only)")
    ap.add_argument("--declared-at", help="YYYY-MM-DD (new baseline only)")
    ap.add_argument("--declared-by-issue", type=int, default=766,
                     help="new baseline only, default 766")
    ap.add_argument("--force", action="store_true",
                     help="allow re-registering an already-recorded fixture id")
    ap.add_argument("--skip-validation", action="store_true",
                     help="don't run the new/updated complete-session "
                          "fixture through the real codec (cabal test "
                          "synarchy-test-headless --test-options='--match "
                          "\"save migrations\"') before keeping the "
                          "registration -- only for environments with no "
                          "cabal toolchain; the checked-in CI gate still "
                          "catches a bad fixture on the next push")
    args = ap.parse_args()
    if args.generate_session:
        if not args.baseline_id or not args.fixture_id or not args.path \
                or not args.summary:
            ap.error("--generate-session requires --baseline-id, "
                     "--fixture-id, --path, and --summary")
        return generate.cmd_generate(args)
    if args.add_baseline:
        if not args.baseline_id or not args.fixture_id or not args.path or not args.kind:
            ap.error("--add-baseline requires --baseline-id, --fixture-id, "
                     "--path, and --kind")
        return register.cmd_add_baseline(args)
    return manifest_audit.cmd_audit(args)


if __name__ == "__main__":
    sys.exit(main())
