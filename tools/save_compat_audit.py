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
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
MANIFEST_PATH = REPO_ROOT / "docs" / "save_compat" / "manifest.json"
FIXTURE_DATA_DIR = REPO_ROOT / "test-headless" / "data" / "save-compat"
ENVELOPE_SOURCE_PATH = REPO_ROOT / "src" / "World" / "Save" / "Envelope.hs"
SESSION_V90_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Compat" / "SessionV90.hs")
ENVELOPE_TYPES_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Envelope" / "Types.hs")
ENVELOPE_CODEC_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Envelope" / "Codec.hs")

# The cabal file whose `common lang` stanza supplies every extension the
# library's modules -- Codec.hs among them -- already get without
# declaring anything locally (issue #1416). Deliberately NOT a
# hard-coded copy of that list: a hard-coded one that outlived a stanza
# edit would keep treating a now-EFFECTIVE local declaration as
# redundant, which is exactly the false negative
# envelope_framing_fingerprint must not have.
CABAL_PATH = REPO_ROOT / "synarchy.cabal"

# The ONE authoritative list of Haskell-owned gameplay components (round-
# 16 review): World.Save.Component.saveComponentRegistry itself, not a
# hand-maintained guess at which files declare them.
COMPONENT_REGISTRY_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Component.hs")

# Every source file that MIGHT declare a Haskell-owned gameplay
# component's ComponentId literal and/or its ComponentCodec (built
# through `componentCodec ComponentSpec { ... }`) -- see
# real_component_registry(). Round-16 review: previously a hand-
# maintained fixed list of exactly 4 files, so a brand-new component
# added in a NEW file under this same directory (the established
# convention every existing component already follows) was invisible to
# this audit with no error raised at all. Globbing the directory
# `saveComponentRegistry` itself draws every codec from means a new file
# is picked up automatically; real_component_registry() ALSO cross-
# checks every codec name saveComponentRegistry actually references
# against what this scan found, so even a component defined somewhere
# ELSE entirely still fails loudly instead of silently vanishing.
HASKELL_COMPONENT_SOURCE_PATHS = sorted(
    (REPO_ROOT / "src" / "World" / "Save" / "Component").glob("*.hs"))

REGISTER_COMPONENT_RE = re.compile(r"registerComponent\s+(\w+)")

# Every genuine Lua-module save-persistence registration call site
# (round-16 review): globbed across ALL of scripts/ rather than a fixed
# 2-file list, so a new registered Lua module in ANY file is discovered
# automatically -- mirrors the Haskell-side fix's identical reasoning.
LUA_SAVE_MODS_REGISTER_RE = re.compile(r'saveMods\.register\(\s*"(\w+)"')


def discover_lua_save_modules(scripts_root: Path = REPO_ROOT / "scripts") -> list[tuple[Path, str]]:
    """Every (file, module id) pair where a REAL `saveMods.register("id",
    {...})` call site exists, discovered by scanning every .lua file
    under scripts/ -- not a hand-maintained guess at which 2 files do
    this (round-16 review)."""
    found = []
    for path in sorted(scripts_root.rglob("*.lua")):
        text = path.read_text(encoding="utf-8")
        for lua_id in LUA_SAVE_MODS_REGISTER_RE.findall(text):
            found.append((path, lua_id))
    return found

CURRENT_ENVELOPE_VERSION_RE = re.compile(
    r"^currentEnvelopeVersion\s*=\s*(\d+)", re.MULTILINE)
METADATA_COMPONENT_VERSION_RE = re.compile(
    r"^metadataComponentVersion\s*=\s*(\d+)", re.MULTILINE)
# #913: "metadata" is the one component whose accepted-input-version set
# is not a singleton derivable from its current version -- it gained a
# frozen v1 predecessor (World.Save.Compat.MetadataV1) when smAutosave
# was appended. Parsed from the SAME source binding
# World.Save.Envelope.decodeMetadataComponent actually gates on, so
# dropping a historical decoder there shows up here immediately rather
# than only when someone remembers to edit this tool too.
METADATA_COMPONENT_INPUT_VERSIONS_RE = re.compile(
    r"^metadataComponentInputVersions\s*=\s*\[([^\]]*)\]", re.MULTILINE)
LEGACY_METADATA_COMPONENT_VERSION_RE = re.compile(
    r"^legacyMetadataComponentVersion\s*=\s*(\d+)", re.MULTILINE)
# #2021: metadata reached v3, so the input-version list now names TWO
# frozen predecessors by binding (legacy v1 and predecessor v2) rather
# than one. Resolving those names from a hard-coded pair would need a
# tool edit per bump -- and a forgotten edit fails LOUDLY but for the
# wrong reason. Instead every top-level `<name> :: Word32` /
# `<name> = <int>` pair in the envelope source is resolvable, which is
# exactly the shape each of those bindings has. An entry naming anything
# else still raises.
WORD32_BINDING_RE = re.compile(
    r"^(\w+)\s*(?:∷|::)\s*Word32\s*\n\1\s*=\s*(\d+)\s*$", re.MULTILINE)
SESSION_COMPONENT_VERSION_RE = re.compile(
    r"^sessionComponentVersion\s*=\s*(\d+)", re.MULTILINE)
COMPONENT_ID_LITERAL_RE = re.compile(
    r"(\w+)\s*=\s*ComponentId\s*\"([^\"]+)\"")
# Issue #1093: EVERY gameplay codec is now built by the one shared
# construction, `<codec> = componentCodec ComponentSpec { ... }`, whose
# accepted-version set is the current `csVersion` plus one `atVersion <n>`
# entry per older version in `csOlderVersions`. The previous pair of
# regexes (positional `serializeCodec <id> <ver> <True|False>` and a
# hand-rolled `ComponentCodec { ccId = ... }` record) are both gone with
# the syntax they parsed -- a codec written either old way is no longer
# discovered here, and real_component_registry()'s cross-check against
# saveComponentRegistry then fails LOUDLY naming it, rather than this
# tool silently reporting it as an undiscovered component.
#
# Parsed by lexing the record block (brace-depth match + top-level field
# split), NOT by one big ordered regex: field ORDER and line breaks are
# then irrelevant, and a spec missing a field this audit needs raises
# instead of silently matching into the NEXT codec's fields.
COMPONENT_SPEC_HEAD_RE = re.compile(
    r"(\w+)\s*=\s*componentCodec\s+ComponentSpec\b")
# The HEAD of one `csOlderVersions` element: anchored at the element's
# start and requiring an INTEGER LITERAL version. Deliberately not a
# `findall` over the whole field value (issue #1275): a scan would
# silently skip any element it does not recognize -- `atVersion
# someVersionConstant f`, a hand-built `ComponentVersion { cvVersion = 3,
# ... }`, a helper expression -- and derive an accepted-version set with
# entries missing.
#
# Matching this pattern is NECESSARY BUT NOT SUFFICIENT. It only proves
# the element STARTS the right way; `_parse_older_version_entry` below
# then consumes the single build argument and requires the element to be
# EXHAUSTED, so a trailing operator/helper application cannot ride along
# behind a readable prefix (`atVersion 1 migrateV1 `seq` atVersion
# futureVersion migrateFuture` would otherwise be recorded as v1 while
# the ComponentVersion actually built is the unreadable second one).
AT_VERSION_ENTRY_RE = re.compile(r"atVersion\s+(\d+)(?![\w'])")
# One atomic build argument: a (possibly qualified) identifier. The other
# accepted shape is a balanced parenthesized group, lexed rather than
# matched, so nesting and string literals inside it are handled.
BUILD_IDENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_'.]*")
LUA_MODULE_VERSION_RE = re.compile(r"\bversion\s*=\s*(\d+)")
LUA_MODULE_INPUT_VERSIONS_RE = re.compile(
    r"\binputVersions\s*=\s*\{([^}]*)\}")
LUA_MODULE_REQUIRED_RE = re.compile(r"\brequired\s*=\s*(true|false)")


def strip_haskell_line_comments(text: str) -> str:
    """Blank out `--` line comments so a comment's punctuation can never
    be read as record structure. Safe for these files: they spell every
    operator in Unicode (→/⇒/∷), so `--` only ever starts a comment."""
    return "\n".join(re.sub(r"--.*$", "", line) for line in text.splitlines())


def _record_block(text: str, open_index: int) -> str:
    """The contents of the record block whose opening `{` is at
    `open_index`, matched by brace depth (so a nested record update is
    handled) and skipping string literals. Raises rather than guessing
    if the block is unterminated or carries a `{- -}` comment this
    simple lexer cannot account for."""
    depth = 0
    i = open_index
    while i < len(text):
        ch = text[i]
        if ch == '"':
            i += 1
            while i < len(text) and text[i] != '"':
                i += 2 if text[i] == "\\" else 1
            i += 1
            continue
        if text.startswith("{-", i):
            raise ValueError(
                "a ComponentSpec record block contains a `{- -}` block "
                "comment, which this lexer deliberately refuses to guess "
                "past -- use `--` line comments inside a spec, or teach "
                "this parser about block comments")
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return text[open_index + 1:i]
        i += 1
    raise ValueError(
        "unterminated ComponentSpec record block -- the `{` at offset "
        f"{open_index} never closes")


def _split_top_level_fields(block: str) -> list[str]:
    """Split a record block's contents on the `,` separators at nesting
    depth 0, so a field whose value is a list/tuple/nested record stays
    in one piece."""
    fields: list[str] = []
    depth = 0
    start = 0
    i = 0
    while i < len(block):
        ch = block[i]
        if ch == '"':
            i += 1
            while i < len(block) and block[i] != '"':
                i += 2 if block[i] == "\\" else 1
        elif ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif ch == "," and depth == 0:
            fields.append(block[start:i])
            start = i + 1
        i += 1
    fields.append(block[start:])
    return [f for f in (f.strip() for f in fields) if f]


def _consume_build_atom(text: str) -> int | None:
    """The length of the ONE atomic build argument at the start of
    `text` -- a balanced `( ... )` group or a (possibly qualified)
    identifier -- or None when `text` does not start with either.

    Lexed rather than regex-matched so nesting depth and string literals
    are actually tracked; the caller's fail-closed contract depends on
    knowing exactly where the argument ENDS, not merely that one is
    present."""
    if not text:
        return None
    if text[0] == "(":
        depth = 0
        i = 0
        while i < len(text):
            ch = text[i]
            if ch == '"':
                i += 1
                while i < len(text) and text[i] != '"':
                    i += 2 if text[i] == "\\" else 1
                i += 1
                continue
            if ch in "([{":
                depth += 1
            elif ch in ")]}":
                depth -= 1
                if depth == 0:
                    return i + 1
            i += 1
        return None
    m = BUILD_IDENT_RE.match(text)
    return m.end() if m else None


def _parse_older_version_entry(codec: str, element: str, where: str) -> int:
    """The version of ONE `csOlderVersions` element, requiring the whole
    element to be exactly `atVersion <integer literal> <one build
    argument>` and nothing more (issue #1275).

    Exhausting the element is the point. Validating only the head would
    read `atVersion 1 migrateV1 `seq` atVersion futureVersion
    migrateFuture` as a plain v1 while the ComponentVersion the element
    really evaluates to carries an unreadable version -- precisely the
    silently-incomplete accepted-version set this parse exists to
    prevent."""
    def reject(why: str) -> ValueError:
        return ValueError(
            f"{where}: codec '{codec}' has a csOlderVersions entry this "
            f"audit cannot enumerate ({element!r}): {why}. Every entry must "
            f"be exactly `atVersion <integer literal> <build>` -- a "
            f"non-literal version, a hand-built ComponentVersion, a helper "
            f"expression, or anything trailing the build argument would "
            f"otherwise be silently omitted from, or misread into, the "
            f"accepted-version set")

    m = AT_VERSION_ENTRY_RE.match(element)
    if m is None:
        raise reject("it does not begin with `atVersion <integer literal>`")
    rest = element[m.end():].lstrip()
    consumed = _consume_build_atom(rest)
    if consumed is None:
        raise reject("no build argument (an identifier or a balanced "
                     "parenthesized expression) follows the version")
    trailing = rest[consumed:].strip()
    if trailing:
        raise reject(f"unread text follows the build argument ({trailing!r})")
    return int(m.group(1))


def _parse_older_versions(codec: str, raw: str, where: str) -> list[int]:
    """The `csOlderVersions` versions IN DECLARATION ORDER, keeping
    duplicates (issue #1275).

    Fails CLOSED, and does so by EXHAUSTING what it reads rather than by
    recognizing shapes it happens to know. The field must be a literal
    list, and `_parse_older_version_entry` must consume each element
    completely; a non-list value (a named helper, a `concat`, a
    variable), a non-literal version argument, a directly-constructed
    `ComponentVersion`, and anything trailing an otherwise-readable
    element all raise rather than being skipped or half-read. A skipped
    or misread element is worse than no parse at all here: it would
    silently narrow the accepted-version set this audit then compares the
    whole fixture manifest against."""
    text = raw.strip()
    if not (text.startswith("[") and text.endswith("]")):
        raise ValueError(
            f"{where}: codec '{codec}' has a csOlderVersions value this "
            f"audit cannot enumerate ({raw!r}); it must be a literal "
            f"`[ atVersion <n> <build>, ... ]` list, because a partially "
            f"read table would yield an accepted-version set missing real "
            f"decoders")
    return [_parse_older_version_entry(codec, element, where)
            for element in _split_top_level_fields(text[1:-1])]


def _check_older_versions(codec: str, current: int, older: list[int],
                          where: str) -> None:
    """Reject a malformed `csOlderVersions` table, naming the codec and
    the first offending version in declaration order (issue #1275).

    Defense-in-depth ONLY. The authoritative boundary is
    `World.Save.Component.Types.componentCodec`, which rejects the same
    two rules at codec construction so a malformed table never reaches a
    live dispatch table at all; this audit reads the same declarations
    from source and must therefore agree with it. Its job here is to stop
    ERASING the evidence -- before this, `sorted({current} | set(older))`
    normalized a duplicate, a current-as-older, and a future entry into
    an apparently valid accepted set, so the one tool that exists to
    catch schema-evolution mistakes hid this particular one."""
    seen: set[int] = set()
    for v in older:
        if v == current:
            raise ValueError(
                f"{where}: codec '{codec}' lists v{v} in csOlderVersions, "
                f"but that IS its current csVersion -- the real current "
                f"decoder shadows it, so its frozen DTO is never reached")
        if v > current:
            raise ValueError(
                f"{where}: codec '{codec}' lists v{v} in csOlderVersions, "
                f"which is NEWER than its current v{current} -- the reader "
                f"would advertise and accept a version no writer has ever "
                f"produced")
        if v in seen:
            raise ValueError(
                f"{where}: codec '{codec}' lists v{v} in csOlderVersions "
                f"more than once -- only the first decoder for a repeated "
                f"version is ever reached by the dispatch table's lookup")
        seen.add(v)


def discover_component_specs(text: str, where: str = "<source>") -> list[dict]:
    """Every `<codec> = componentCodec ComponentSpec { ... }` declaration
    in one Haskell source, as
    {codec, componentIdIdent, currentVersion, inputVersions, required}.

    `inputVersions` is derived exactly the way
    'World.Save.Component.Types.componentCodec' derives `ccInputVers` --
    the current `csVersion` plus each `csOlderVersions` entry's
    `atVersion <n>`, sorted ascending -- so this audit reads the SAME
    single declaration the reader dispatches on rather than a separately
    parsed list that could disagree with it (issue #1093).

    Issue #1275: that derivation now goes through `_parse_older_versions`
    (fails closed on any element it cannot enumerate) and
    `_check_older_versions` (rejects a duplicate, the current version, or
    a future version). Both mirror the authoritative Haskell-side check
    in `componentCodec`; for a well-formed table the resulting
    `inputVersions` is byte-for-byte what it always was."""
    clean = strip_haskell_line_comments(text)
    specs: list[dict] = []
    for head in COMPONENT_SPEC_HEAD_RE.finditer(clean):
        codec = head.group(1)
        brace = re.match(r"\s*\{", clean[head.end():])
        if brace is None:
            raise ValueError(
                f"{where}: `{codec} = componentCodec ComponentSpec` is not "
                f"followed by a `{{ ... }}` record block")
        block = _record_block(clean, head.end() + brace.end() - 1)
        values: dict[str, str] = {}
        for field in _split_top_level_fields(block):
            m = re.match(r"(\w+)\s*=\s*(.*)", field, re.S)
            if m:
                values[m.group(1)] = m.group(2).strip()
        missing = [f for f in ("csComponent", "csVersion", "csRequired",
                               "csOlderVersions") if f not in values]
        if missing:
            raise ValueError(
                f"{where}: codec '{codec}' is built by componentCodec but "
                f"its ComponentSpec has no {', '.join(missing)} field -- did "
                f"the spec's field names change without updating this audit?")
        if not re.fullmatch(r"\d+", values["csVersion"]):
            raise ValueError(
                f"{where}: codec '{codec}' has a non-literal csVersion "
                f"({values['csVersion']!r}); this audit needs the real "
                f"schema version, not an expression")
        if values["csRequired"] not in ("True", "False"):
            raise ValueError(
                f"{where}: codec '{codec}' has a non-literal csRequired "
                f"({values['csRequired']!r})")
        current = int(values["csVersion"])
        # Issue #1275: parse the raw declaration in order, keeping
        # duplicates, and REJECT a malformed table -- rather than
        # `sorted({current} | set(older))`, which erased exactly the
        # evidence a schema-evolution mistake leaves behind.
        older = _parse_older_versions(codec, values["csOlderVersions"], where)
        _check_older_versions(codec, current, older, where)
        specs.append({
            "codec": codec,
            "componentIdIdent": values["csComponent"],
            "currentVersion": current,
            "inputVersions": sorted(older + [current]),
            "required": values["csRequired"] == "True"})
    return specs


def load_manifest(path: Path = MANIFEST_PATH) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def _strip_line_comments(block: str) -> str:
    """Drop every `--` line comment, leaving the lines themselves in
    place. Split out of _normalize_haskell_block so the envelope
    framing fingerprint can strip comments BEFORE its own pragma pass
    (issue #1416) without a commented-out pragma being mistaken for a
    live one; _normalize_haskell_block still calls it, unchanged."""
    return "\n".join(
        re.sub(r"--.*$", "", line) for line in block.splitlines())


def _normalize_haskell_block(block: str) -> str:
    """Strip line comments and collapse whitespace, so a documentation-
    only edit never moves a fingerprint but a REAL structural change
    (field added/removed/reordered, logic changed) always does."""
    return re.sub(r"\s+", " ", _strip_line_comments(block)).strip()


# Any `{-# ... #-}` block pragma, possibly spread over several lines,
# and the LANGUAGE ones specifically.
BLOCK_PRAGMA_RE = re.compile(r"\{-#.*?#-\}", re.DOTALL)
LANGUAGE_PRAGMA_RE = re.compile(r"\{-#\s*LANGUAGE\s(.*?)#-\}", re.DOTALL)
EXTENSION_NAME_RE = re.compile(r"^[A-Za-z][A-Za-z0-9_']*$")


def _extension_state(name: str) -> tuple[str, bool]:
    """An extension's EFFECTIVE state as a (base name, enabled) pair.
    `NoImplicitPrelude` and `ImplicitPrelude` name the same extension in
    opposite states, so comparing bare names would call a local
    `{-# LANGUAGE ImplicitPrelude #-}` redundant against an inherited
    `NoImplicitPrelude` when it in fact REVERSES it (issue #1416)."""
    if name.startswith("No") and len(name) > 2 and name[2].isupper():
        return (name[2:], False)
    return (name, True)


def _cabal_stanza_body(text: str, header_re: str, cabal_path: Path) -> list:
    """Every line of one top-level cabal stanza's body (the lines after
    its column-0 header, up to the next column-0 non-blank line)."""
    lines = text.splitlines()
    start = None
    for i, line in enumerate(lines):
        if re.match(header_re, line):
            start = i + 1
            break
    if start is None:
        raise ValueError(
            f"could not find a stanza matching {header_re!r} in "
            f"{cabal_path} -- did the cabal file get restructured? "
            f"envelope_framing_fingerprint derives Codec.hs's inherited "
            f"extension set from it and must not guess")
    body = []
    for line in lines[start:]:
        if line.strip() and not line[:1].isspace():
            break
        body.append(line)
    return body


def _cabal_field(body: list, field: str, cabal_path: Path) -> str:
    """One cabal field's complete value from a stanza body, following
    cabal's continuation rule: a continuation line is indented strictly
    more than the field name itself. Cabal `--` comments are dropped, so
    an explanatory line inside a continuation is not mistaken for part
    of the value."""
    def uncommented(line: str) -> str:
        return re.sub(r"--.*$", "", line)

    for i, line in enumerate(body):
        m = re.match(rf"^(\s*){re.escape(field)}\s*:(.*)$", line)
        if not m:
            continue
        indent = len(m.group(1))
        parts = [uncommented(m.group(2))]
        for cont in body[i + 1:]:
            if not cont.strip():
                break
            if len(cont) - len(cont.lstrip()) <= indent:
                break
            parts.append(uncommented(cont))
        return " ".join(parts)
    raise ValueError(
        f"could not find a `{field}:` field in the stanza read from "
        f"{cabal_path} -- envelope_framing_fingerprint derives Codec.hs's "
        f"inherited extension set from it and must not guess")


def inherited_default_extensions(
        cabal_path: Path = CABAL_PATH) -> dict[str, bool]:
    """The EFFECTIVE extension state Codec.hs gets for free, derived
    live from synarchy.cabal's `common lang` stanza (issue #1416) --
    never a hard-coded copy, which would silently widen the redundancy
    exception the first time that stanza changed.

    Also checks that the `library` stanza (the component Codec.hs
    belongs to) actually imports `lang`: if it stopped doing so, every
    one of these extensions would become EFFECTIVE when declared
    locally, and treating such a declaration as redundant would be
    precisely the false negative this fingerprint forbids. A missing or
    unparseable stanza/field is a hard error, never a silently empty
    inherited set (which would degrade to "nothing is redundant" --
    safe, but invisibly so)."""
    text = cabal_path.read_text(encoding="utf-8")
    lang_body = _cabal_stanza_body(text, r"^common\s+lang\s*$", cabal_path)
    library_body = _cabal_stanza_body(text, r"^library\s*$", cabal_path)
    imported = {part.strip() for part in
                _cabal_field(library_body, "import", cabal_path).split(",")}
    if "lang" not in imported:
        raise ValueError(
            f"{cabal_path}'s `library` stanza no longer imports `lang` "
            f"(imports: {sorted(imported)}) -- Codec.hs therefore inherits "
            f"nothing from `common lang`, so envelope_framing_fingerprint "
            f"must not treat any local LANGUAGE declaration as redundant")
    raw = _cabal_field(lang_body, "default-extensions", cabal_path)
    names = [tok.strip() for tok in raw.split(",")]
    names = [n for n in names if n]
    if not names:
        raise ValueError(
            f"{cabal_path}'s `common lang` declares no default-extensions "
            f"-- refusing to proceed with an empty inherited set")
    inherited: dict[str, bool] = {}
    for name in names:
        if not EXTENSION_NAME_RE.match(name):
            raise ValueError(
                f"{cabal_path}'s `common lang` default-extensions contains "
                f"{name!r}, which is not a bare extension name -- refusing "
                f"to guess Codec.hs's inherited extension set")
        base, enabled = _extension_state(name)
        inherited[base] = enabled
    return inherited


def _header_gap_end(text: str, pos: int) -> int | None:
    """The end of the run of whitespace and NESTED Haskell block
    comments starting at `pos` -- i.e. everything that may legally sit
    between two module-header pragmas without ending the header. None
    if a block comment is never closed, which makes the header
    unreadable and must leave it alone.

    `{-#` opens a pragma, not a comment, so it is never consumed here;
    inside a comment it nests and is balanced by its own `#-}`. Skipping
    these matters because `_strip_line_comments` removes only `--`
    comments: a `{- ... -}` haddock block before the pragma would
    otherwise end the header walk, and a redundant `LANGUAGE`
    declaration behind it would stay fingerprinted (issue #1416
    requirement 1). The comments themselves are preserved verbatim in
    the hash, exactly as before."""
    i = pos
    n = len(text)
    while True:
        while i < n and text[i].isspace():
            i += 1
        if not text.startswith("{-", i) or text.startswith("{-#", i):
            return i
        depth = 0
        j = i
        while j < n:
            if text.startswith("{-", j):
                depth += 1
                j += 2
            elif text.startswith("-}", j):
                depth -= 1
                j += 2
                if depth == 0:
                    break
            else:
                j += 1
        if depth != 0:
            return None
        i = j


def _drop_redundant_language_pragmas(
        text: str, inherited: dict[str, bool]) -> str:
    """Drop those locally declared LANGUAGE extensions in the module
    header that Codec.hs already inherits from synarchy.cabal's
    `common lang` stanza, so an edit that adds or removes a declaration
    the module was getting anyway leaves its effective extension set --
    and therefore its behavior, and therefore the bytes it writes --
    unchanged (issue #1416; PR #1001's removal of the already-inherited
    `UnicodeSyntax` is the representative case).

    The exception is deliberately gated on what can be established
    WITHOUT modelling GHC's extension-implication graph, which this tool
    has no way to know. Two rules do that, in order.

    First, the leading INERT PREFIX. While every declaration so far has
    been dropped, the state is provably still exactly the inherited one,
    so a declaration that merely restates it -- in EITHER polarity --
    changes nothing and drops as well. A positive one re-enables an
    extension whose implied closure the stanza already applied; a
    negative one disables something already off, and `No` propagates
    nothing. That is what makes a lone
    `{-# LANGUAGE NoImplicitPrelude #-}` against an inherited
    `NoImplicitPrelude` a provable no-op with no table involved.

    Past that prefix the state may already differ from the inherited
    one, so a narrower rule applies: every REMAINING declaration must be
    in POSITIVE form, and no extension may be named twice anywhere in
    the header. Such a remainder is a plain set of enables layered on a
    state that still contains each inherited extension's own implied
    closure, so removing one of those inherited names cannot change
    anything. This is the rule that keeps requirement 1's representative
    case working: in `{-# LANGUAGE Strict, UnicodeSyntax #-}` the
    non-inherited `Strict` ends the prefix, and the inherited
    `UnicodeSyntax` still drops.

    Once a header turns something OFF past the prefix -- directly
    (`Strict, NoImplicitPrelude`) or by re-enabling after a disable
    (`NoTypeFamilies, TypeFamilyDependencies`, where the second
    declaration reinstates the `TypeFamilies` the first removed, since
    GHC's `TypeFamilyDependencies` implies it) -- redundancy stops being
    decidable from the names alone, and the remainder is kept verbatim.
    The same goes for an extension named twice, and for a token that is
    not a bare extension name. Over-keeping is the fail-safe direction:
    it costs a fingerprint move that did not have to happen, where the
    other direction hides a real change.

    ONE case is knowingly given up to that boundary, and it is a decided
    trade-off rather than an oversight: an inherited NEGATIVE re-declared
    past the prefix (`{-# LANGUAGE Strict, NoImplicitPrelude #-}`) does
    leave the effective set unchanged, and is still retained, because
    proving THAT requires knowing whether the preceding `Strict` implies
    `ImplicitPrelude` -- the very table this tool must not pretend to
    have. Handling it would mean checking one in, with an
    unknown-extension fallback, and accepting its drift; the project
    owner chose the conservative boundary instead on 2026-08-19. Issue
    #1416's own enumerated acceptance cases are unaffected, and if a
    shipped module ever does write that shape the cost is one explicable
    fingerprint move, not a hidden change.

    Only `LANGUAGE` is in scope -- `OPTIONS_GHC` and every other block
    pragma passes through verbatim, as do extensions merely implied by
    `default-language: GHC2024`, which are likewise not treated as
    inherited.

    Only the module HEADER is touched -- the leading run of block
    pragmas (with whitespace and nested `{- ... -}` comments allowed
    between them, and preserved verbatim), which is the only place GHC
    accepts a `LANGUAGE` declaration. Scanning the whole file instead
    would match pragma-SHAPED ordinary source: a string literal (or
    quasiquote, or block comment) reading
    `"{-# LANGUAGE UnicodeSyntax #-}"` would be erased, colliding with
    the same literal naming a different inherited extension even though
    such a literal can itself be part of what the codec writes. The walk
    stops at the first token that is not whitespace, a block comment or
    a block pragma, so anything past the header -- including a stray
    mid-file `LANGUAGE` pragma GHC would reject anyway -- stays
    fingerprinted verbatim."""
    # Split the header's leading pragma run off from the rest of the
    # module, keeping the gaps so whitespace, block comments and
    # non-LANGUAGE pragmas all survive in place.
    gaps: list[str] = []
    pragmas: list[str] = []
    pos = 0
    while True:
        gap_end = _header_gap_end(text, pos)
        if gap_end is None:
            break
        match = BLOCK_PRAGMA_RE.match(text, gap_end)
        if match is None:
            break
        gaps.append(text[pos:gap_end])
        pragmas.append(match.group(0))
        pos = match.end()
    rest = text[pos:]

    declared: list[list[str]] = []
    for pragma in pragmas:
        m = LANGUAGE_PRAGMA_RE.fullmatch(pragma)
        if m is None:
            declared.append([])
            continue
        names = [token.strip() for token in m.group(1).split(",")]
        names = [name for name in names if name]
        if not all(EXTENSION_NAME_RE.match(name) for name in names):
            return text
        declared.append(names)

    flat = [name for names in declared for name in names]
    states = [_extension_state(name) for name in flat]

    # The leading INERT PREFIX: while every declaration so far has been
    # dropped, the state is provably still exactly the inherited one, so
    # a declaration restating it -- in either polarity -- changes
    # nothing and drops too. This is what makes a lone
    # `{-# LANGUAGE NoImplicitPrelude #-}` against an inherited
    # `NoImplicitPrelude` a no-op with no implication table involved:
    # nothing ran before it, `No` propagates nothing, and the extension
    # was already off.
    prefix = 0
    for base, enabled in states:
        if inherited.get(base) is not enabled:
            break
        prefix += 1
    dropped = set(range(prefix))

    # Past the prefix the state may already differ from the inherited
    # one, so only the gate that needs no implication graph applies:
    # remaining declarations all POSITIVE, and no extension named twice
    # anywhere in the header.
    rest_states = states[prefix:]
    decidable = (all(enabled for _, enabled in rest_states)
                 and len({base for base, _ in states}) == len(states))
    if decidable:
        for offset, (base, _) in enumerate(rest_states):
            if inherited.get(base) is True:
                dropped.add(prefix + offset)

    out: list[str] = []
    index = 0
    for gap, pragma, names in zip(gaps, pragmas, declared):
        out.append(gap)
        if not names:
            out.append(pragma)
            continue
        kept = [name for offset, name in enumerate(names)
                if index + offset not in dropped]
        index += len(names)
        if kept:
            out.append("{-# LANGUAGE " + ", ".join(kept) + " #-}")
    out.append(rest)
    return "".join(out)


DTO_TYPE_NAME_RE = re.compile(r"\b(\w+DTO(?:v\d+)?)\b")


def _find_type_definition(name: str, search_paths: list) -> str | None:
    """Find one type's `data`/`newtype` declaration by name across
    multiple files, trying each until found. Returns None (not a raise)
    if genuinely absent everywhere searched -- the caller decides
    whether that's expected (a name that merely CONTAINS "DTO" as part
    of a longer identifier, e.g. a function name, rather than being
    itself a locally-defined type) or worth investigating."""
    for path in search_paths:
        text = path.read_text(encoding="utf-8")
        if re.search(rf"^(?:data|newtype)\s+{re.escape(name)}\b", text, re.MULTILINE):
            try:
                return _extract_toplevel_block(text, name)
            except ValueError:
                continue
    return None


def _transitive_dto_blocks(seed_blocks_text: str, search_paths: list) -> list:
    """Starting from every DTO-named type REFERENCED inside
    seed_blocks_text, recursively resolve each one's own `data`/
    `newtype` block (searching search_paths), plus every DTO name IT in
    turn references, to a fixed point (round-16 review: a frozen type's
    wire layout depends on every LEAF DTO it embeds, not just its own
    immediately-visible field types)."""
    resolved: dict[str, str] = {}
    pending = list(dict.fromkeys(DTO_TYPE_NAME_RE.findall(seed_blocks_text)))
    while pending:
        name = pending.pop(0)
        if name in resolved:
            continue
        block = _find_type_definition(name, search_paths)
        if block is None:
            continue
        resolved[name] = block
        for referenced in DTO_TYPE_NAME_RE.findall(block):
            if referenced not in resolved:
                pending.append(referenced)
    return [resolved[name] for name in sorted(resolved)]


def frozen_dto_fingerprint(source_path: Path = SESSION_V90_SOURCE_PATH) -> str:
    """A stable fingerprint over the frozen DTO type declarations in
    World.Save.Compat.SessionV90 -- every `data ... = ...` block up to
    its closing `deriving` line -- PLUS every leaf DTO type those
    declarations embed, transitively resolved wherever it's actually
    defined (World.Save.Component.Page/.Entities/.Session/.WorldGen, per
    SessionV90's own module docstring: "every non-global-allocator field
    composes EXISTING frozen leaf/component DTOs ... rather than
    re-freezing them"). Round-16 review: SessionV90.hs's own blocks
    alone said nothing about a field reordered on one of THOSE embedded
    types (WorldGenParamsDTO, WorldEditDTO, GroundItemsDTO, ...) -- the
    actual B1 wire bytes for that leaf type would silently change with
    nothing here noticing. Comment/haddock changes don't move this (a
    documentation-only edit shouldn't force a manifest update);
    reordering, adding, or removing a FIELD anywhere in this transitive
    closure does, since that changes a positional cereal wire layout
    requirement 10 is guarding."""
    text = source_path.read_text(encoding="utf-8")
    own_blocks = re.findall(
        r"^data \w+ = \w+.*?deriving\s*\([^)]*\)", text,
        re.MULTILINE | re.DOTALL)
    if not own_blocks:
        raise ValueError(
            f"no frozen `data ... deriving (...)` blocks found in "
            f"{source_path} -- did the module get restructured?")
    leaf_blocks = _transitive_dto_blocks(
        "\n".join(own_blocks), HASKELL_COMPONENT_SOURCE_PATHS)
    normalized = "\n---\n".join(
        _normalize_haskell_block(b) for b in own_blocks + leaf_blocks)
    return hashlib.sha256(normalized.encode("utf-8")).hexdigest()


# Every top-level Types.hs binding that determines the envelope's actual
# ON-DISK byte layout (round-15 review): the manifest's own wire shape
# (ComponentId/ComponentDescriptor/EnvelopeManifest -- field order is
# the cereal-derived positional layout), the 4-byte magic prefix, and
# the hand-rolled (non-cereal) header scalar codec (fnv1a64/
# encodeW32/decodeW32/encodeW64/decodeW64) World.Save.Envelope.Codec's
# header construction/parsing calls directly. Deliberately EXCLUDES
# EnvelopeLimits/EnvelopeError -- soft config and Haskell-side error
# reporting, neither of which changes what the bytes on disk MEAN.
ENVELOPE_FRAMING_WIRE_BINDINGS = [
    "ComponentId", "ComponentDescriptor", "EnvelopeManifest",
    "envelopeMagic", "fnv1a64", "encodeW32", "decodeW32", "encodeW64",
    "decodeW64",
]


def _extract_toplevel_block(text: str, name: str) -> str:
    """Extract one top-level Haskell type declaration or binding by name
    -- from its own `data name`/`newtype name` header, or its
    `name ::`/`name =` line, through the next BLANK line (this
    codebase's own convention: every top-level item is followed by a
    blank line before the next comment/definition), so a multi-line
    `deriving stock (...)` / `deriving anyclass (...)` split, or a
    `where`-clause, is captured along with it."""
    m = re.search(
        rf"^(?:data|newtype)\s+{re.escape(name)}\b.*?(?=\n\n)|"
        rf"^{re.escape(name)}\s*(?:∷|=).*?(?=\n\n)",
        text, re.MULTILINE | re.DOTALL)
    if not m:
        raise ValueError(
            f"could not find top-level binding '{name}' -- did it get "
            f"renamed or restructured?")
    return m.group(0)


def envelope_framing_fingerprint(
        types_path: Path = ENVELOPE_TYPES_SOURCE_PATH,
        codec_path: Path = ENVELOPE_CODEC_SOURCE_PATH,
        cabal_path: Path = CABAL_PATH) -> str:
    """Round-15 review: envelopeFramingVersion alone is just an integer
    someone has to remember to bump -- it says nothing about whether the
    ACTUAL on-disk byte layout (header/manifest/checksum framing) still
    matches what the manifest was declared against. A framing-layout
    change (reordering ComponentDescriptor's fields, changing the magic
    bytes, altering the checksum algorithm, restructuring
    encodeEnvelope/decodeEnvelope's header construction) could ship
    while leaving envelopeFramingVersion untouched, silently producing a
    new wire format with nothing catching it -- exactly the "new wire
    format without the required format epoch" gap this fingerprint
    closes, mirroring frozen_dto_fingerprint's identical technique
    (comment/haddock-insensitive, reacts to any real structural change)
    applied to the envelope's OWN framing instead of a component's
    frozen DTO. Covers every wire-relevant Types.hs binding
    (ENVELOPE_FRAMING_WIRE_BINDINGS) plus the ENTIRE Codec.hs module --
    every line of that file IS the framing contract (its own module
    docstring: "the pure, side-effect-free tagged-envelope codec").

    ONE narrow exception (issue #1416), on top of the shared
    comment-and-whitespace normalization: a locally declared LANGUAGE
    extension whose effective state Codec.hs ALREADY INHERITS from
    synarchy.cabal's `common lang` stanza is dropped before hashing, so
    adding or removing such a declaration -- PR #1001 deleted an
    already-inherited `UnicodeSyntax` -- does not move this fingerprint.
    That edit cannot change the module's effective extension set, its
    behavior, or the bytes it writes, so a moved fingerprint there was
    pure noise.

    The exception is exactly that narrow, because a false NEGATIVE here
    is the expensive direction. It is not a blanket exclusion of
    LANGUAGE pragmas: an extension the module does NOT inherit (its
    `Strict`), and one whose local declaration REVERSES an inherited
    default (`ImplicitPrelude` against `common lang`'s
    `NoImplicitPrelude`), are both effective and both still fingerprinted
    -- extensions like those can change what unchanged source MEANS while
    every version still compiles. The inherited set is read live from the
    cabal stanza rather than hard-coded, so it cannot drift into covering
    an extension that has since become effective. Nothing else is
    excluded: imports, the module header and export list, `OPTIONS_GHC`
    and every other block pragma, and of course all the code, still move
    this fingerprint."""
    types_text = types_path.read_text(encoding="utf-8")
    blocks = [_extract_toplevel_block(types_text, name)
              for name in ENVELOPE_FRAMING_WIRE_BINDINGS]
    # Only the CODEC text gets the pragma pass: _normalize_haskell_block
    # is shared with frozen_dto_fingerprint, whose recorded value must
    # not move. Comments come off first so a commented-out pragma can
    # never be read as a live declaration.
    codec_text = _drop_redundant_language_pragmas(
        _strip_line_comments(codec_path.read_text(encoding="utf-8")),
        inherited_default_extensions(cabal_path))
    normalized = "\n---\n".join(
        _normalize_haskell_block(b) for b in blocks + [codec_text])
    return hashlib.sha256(normalized.encode("utf-8")).hexdigest()


def current_envelope_version(path: Path = ENVELOPE_SOURCE_PATH) -> int:
    text = path.read_text(encoding="utf-8")
    m = CURRENT_ENVELOPE_VERSION_RE.search(text)
    if not m:
        raise ValueError(f"could not find currentEnvelopeVersion in {path}")
    return int(m.group(1))


def metadata_input_versions(envelope_text: str, current: int) -> list[int]:
    """Every "metadata" schema version World.Save.Envelope can decode.

    Resolves the literal `metadataComponentInputVersions` list, whose
    entries are either integers or one of the `Word32` version bindings
    declared alongside it (`legacyMetadataComponentVersion`,
    `predecessorMetadataComponentVersion`, and any future sibling).
    Raises if the list is missing or an entry cannot be resolved -- an unparseable declaration must fail
    loudly rather than silently degrade to "only the current version",
    which would wrongly accuse every historical baseline of declaring a
    version whose decoder had been removed."""
    m = METADATA_COMPONENT_INPUT_VERSIONS_RE.search(envelope_text)
    if not m:
        raise ValueError(
            f"could not find metadataComponentInputVersions in "
            f"{ENVELOPE_SOURCE_PATH}")
    names = {name: int(value)
             for name, value in WORD32_BINDING_RE.findall(envelope_text)}
    # The CURRENT version is supplied by the caller (which reads it the
    # same way every other check does), so a disagreement between the two
    # can never be papered over by the generic scan above.
    names["metadataComponentVersion"] = current
    legacy_m = LEGACY_METADATA_COMPONENT_VERSION_RE.search(envelope_text)
    if legacy_m:
        names["legacyMetadataComponentVersion"] = int(legacy_m.group(1))
    versions: list[int] = []
    for raw in m.group(1).split(","):
        token = raw.strip()
        if not token:
            continue
        if token.isdigit():
            versions.append(int(token))
        elif token in names:
            versions.append(names[token])
        else:
            raise ValueError(
                f"metadataComponentInputVersions in {ENVELOPE_SOURCE_PATH} "
                f"carries an entry this parser cannot resolve: {token!r}")
    if not versions:
        raise ValueError(
            f"metadataComponentInputVersions in {ENVELOPE_SOURCE_PATH} is empty")
    return sorted(set(versions))


def real_component_registry() -> dict[str, dict]:
    """Every save component this build's REAL source currently declares,
    id -> {"currentVersion": int, "inputVersions": [int, ...],
    "required": bool} -- parsed directly from the Haskell/Lua source, not
    hand-maintained here, so a schema bump, a removed historical decoder,
    or a newly required component shows up the next time this runs, not
    only when someone remembers to update this tool too (requirement 19's
    version cross-check).

    Raises if a source file's expected declarations can't be found at
    all (the parser itself is stale, e.g. after a real rename) -- that
    must fail loudly, not silently audit against an empty registry."""
    registry: dict[str, dict] = {}

    envelope_text = ENVELOPE_SOURCE_PATH.read_text(encoding="utf-8")
    m = METADATA_COMPONENT_VERSION_RE.search(envelope_text)
    if not m:
        raise ValueError(
            f"could not find metadataComponentVersion in {ENVELOPE_SOURCE_PATH}")
    # metadata is unconditionally required -- every envelope carries it
    # (World.Save.Envelope's own decode refuses one that doesn't).
    metadata_current = int(m.group(1))
    registry["metadata"] = {
        "currentVersion": metadata_current,
        "inputVersions": metadata_input_versions(envelope_text, metadata_current),
        "required": True}

    # "session" is the ONE frozen legacy component (World.Save.Compat.
    # SessionV90) -- its current version is its only ever version; a
    # further schema change adds a new frozen type instead of bumping
    # this one (the frozen-DTO boundary rule), so inputVersions is
    # always the singleton [currentVersion]. Unconditionally required --
    # it IS the whole legacy envelope's one gameplay component.
    session_text = SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8")
    m = SESSION_COMPONENT_VERSION_RE.search(session_text)
    if not m:
        raise ValueError(
            f"could not find sessionComponentVersion in {SESSION_V90_SOURCE_PATH}")
    registry["session"] = {
        "currentVersion": int(m.group(1)), "inputVersions": [int(m.group(1))],
        "required": True}

    # "lua-state" is the OTHER retired, hardcoded-in-Envelope.hs legacy
    # id (round-7 review, issue #766 requirement 3's "#760" baseline):
    # the single opaque pre-#761 Lua blob, recognized only by
    # World.Save.Envelope.decodeB2SessionEnvelope's fallback, never by
    # the modern registry or the live Lua registry. Its internal payload
    # is never actually decoded (only checked for emptiness), so unlike
    # "session" there is no real "version" to parse from source -- this
    # is a fixed bookkeeping placeholder matching what every tracked
    # fixture for it is encoded at.
    registry["lua-state"] = {"currentVersion": 1, "inputVersions": [1], "required": True}

    id_literals: dict[str, str] = {}
    for path in HASKELL_COMPONENT_SOURCE_PATHS:
        text = path.read_text(encoding="utf-8")
        for ident, sid in COMPONENT_ID_LITERAL_RE.findall(text):
            id_literals[ident] = sid

    discovered_codec_names: set[str] = set()
    for path in HASKELL_COMPONENT_SOURCE_PATHS:
        text = path.read_text(encoding="utf-8")
        for spec in discover_component_specs(text, str(path)):
            sid = id_literals.get(spec["componentIdIdent"])
            if sid is None:
                raise ValueError(
                    f"{path}: componentCodec references unknown component "
                    f"id identifier '{spec['componentIdIdent']}' -- did a "
                    f"ComponentId binding get renamed without updating this "
                    f"parser?")
            discovered_codec_names.add(spec["codec"])
            registry[sid] = {
                "currentVersion": spec["currentVersion"],
                "inputVersions": spec["inputVersions"],
                "required": spec["required"]}

    # Round-16 review: cross-check against the ONE authoritative list --
    # World.Save.Component.saveComponentRegistry's own entries -- rather
    # than trust that HASKELL_COMPONENT_SOURCE_PATHS' glob (or, before
    # that, its hand-maintained fixed file list) happened to find
    # everything registered. A codec referenced there that this scan
    # never actually discovered ANYWHERE (a new file outside the globbed
    # directory, a rename, a typo) fails loudly here instead of silently
    # leaving that component out of the entire registry with no error at
    # all -- exactly the "new required component registered from another
    # module" gap this closes.
    registry_source_text = COMPONENT_REGISTRY_SOURCE_PATH.read_text(encoding="utf-8")
    registry_list_block = _extract_toplevel_block(
        registry_source_text, "saveComponentRegistry")
    authoritative_codec_names = REGISTER_COMPONENT_RE.findall(registry_list_block)
    if not authoritative_codec_names:
        raise ValueError(
            f"no 'registerComponent <codec>' entries found in "
            f"saveComponentRegistry ({COMPONENT_REGISTRY_SOURCE_PATH}) -- "
            f"did the registry get restructured?")
    missing_codec_names = [
        name for name in authoritative_codec_names
        if name not in discovered_codec_names]
    if missing_codec_names:
        raise ValueError(
            f"saveComponentRegistry references "
            f"{', '.join(missing_codec_names)}, but no matching codec "
            f"definition was found anywhere under "
            f"{HASKELL_COMPONENT_SOURCE_PATHS[0].parent} in the expected "
            f"`<codec> = componentCodec ComponentSpec {{ ... }}` form -- a "
            f"component registered from a module this scan never looked at, "
            f"a renamed/typo'd codec binding, or a codec that hand-rolls the "
            f"'ComponentCodec' record instead of going through the shared "
            f"construction (issue #1093) would otherwise be silently absent "
            f"from the ENTIRE real_component_registry() with no error at all")

    discovered_lua_modules = discover_lua_save_modules()
    if not discovered_lua_modules:
        raise ValueError(
            f"no 'saveMods.register(\"...\", {{...}})' call sites found "
            f"anywhere under {REPO_ROOT / 'scripts'} -- did every "
            f"registered Lua save module get removed, or did the call "
            f"site's shape change?")
    for path, lua_id in discovered_lua_modules:
        text = path.read_text(encoding="utf-8")
        vm = LUA_MODULE_VERSION_RE.search(text)
        ivm = LUA_MODULE_INPUT_VERSIONS_RE.search(text)
        rm = LUA_MODULE_REQUIRED_RE.search(text)
        if not vm or not ivm or not rm:
            raise ValueError(
                f"could not find version/inputVersions/required in {path} "
                f"-- did saveMods.register('{lua_id}', {{...}})'s "
                f"declaration shape change?")
        input_versions = [int(v.strip()) for v in ivm.group(1).split(",")
                           if v.strip()]
        registry[f"lua.{lua_id}"] = {
            "currentVersion": int(vm.group(1)), "inputVersions": input_versions,
            "required": rm.group(1) == "true"}

    return registry


def audit_component_versions(
        manifest: dict, real_registry: dict,
        verified_tracked: dict[str, set[int]] | None = None) -> list[str]:
    """Cross-check every baseline's declared components[] against the
    REAL current registry (requirement 19):

      - a declared component id must still exist in the real registry
        (catches a component renamed/removed without retiring or
        updating the baseline that references it);
      - a declared version must still be one of the real codec's
        accepted input versions (catches "removal of a declared
        decoder" -- ccInputVers/inputVersions shrinking out from under
        a tracked historical fixture);
      - every REQUIRED component (round-3 review: reads ccRequired/Lua's
        required flag, not just version counts) must be tracked by AT
        LEAST ONE baseline's components[], regardless of how many
        input versions it accepts -- a brand-new required component
        with a single input version was previously invisible to this
        audit entirely (it never has ">1 input version", the ONLY case
        the prior check looked at), so it could ship with genuinely NO
        baseline ever proving it has an accounted default/migration
        policy for a session that predates it, while this audit still
        passed. An OPTIONAL component has no such obligation -- a
        legacy save legitimately lacking it is exactly requirement 9's
        "unknown/absent optional component" case, not a compatibility
        gap;
      - every component with more than one accepted input version (a
        real migration exists) must have its OLDEST accepted version
        tracked by at least one baseline's components[] -- proving
        that historical migration is exercised by a real fixture, not
        merely present in code (catches "a component-version bump
        without a migration/frozen DTO" ever being validated);
      - round-10 review: a component's CURRENT version must ALSO be
        tracked by some baseline, once ANY version of it is tracked at
        all -- the oldest-version check above proves the OLD shape has
        coverage, but says nothing about the NEW one. Bumping e.g.
        craft-bills from v2 to v3 (inputVersions=[1,2,3]) while the
        manifest still only tracks v1/v2 previously passed silently:
        v1 (oldest) was covered, so the only other check that ever ran
        found nothing wrong, despite v3 having no frozen DTO, fixture,
        or migration ever declared or validated anywhere. A component
        with NO baseline tracking it at all is unaffected here (that is
        either the required-zero-coverage violation above, or a
        legitimate optional component requirement 9 already allows to
        have no fixture at all).

    Round-12 review: @verified_tracked@ (from 'verify_fixture_descriptors',
    which decodes every tracked "complete-session" fixture's REAL
    envelope manifest) replaces the manifest-JSON-parsed tracked-versions
    map below for the required-zero-coverage/oldest/current checks below
    when available (i.e. some complete-session fixture actually exists to
    verify against) -- a baseline's components[] claim is no longer
    trusted at face value for THOSE checks, only for the unknown-id/
    invalid-version checks immediately below (which are about the CLAIM's
    own internal consistency against the real codec registry, independent
    of whether any fixture backs it -- still worth catching even when no
    real fixture exists yet to verify against). @None@ (no complete-
    session fixture found anywhere, e.g. a synthetic test manifest) falls
    back to the prior manifest-JSON-trusting behavior unchanged."""
    violations: list[str] = []
    manifest_tracked_versions: dict[str, set[int]] = {}

    for baseline in manifest.get("baselines", []):
        bid = baseline.get("id")
        for comp in baseline.get("components", []):
            comp_id = comp.get("id")
            comp_ver = comp.get("version")
            manifest_tracked_versions.setdefault(comp_id, set()).add(comp_ver)
            real = real_registry.get(comp_id)
            if real is None:
                violations.append(
                    f"baseline '{bid}' declares component '{comp_id}', which "
                    f"no longer exists in the real component registry -- a "
                    f"component was renamed or removed without retiring or "
                    f"updating this baseline")
                continue
            if comp_ver not in real["inputVersions"]:
                violations.append(
                    f"baseline '{bid}' declares component '{comp_id}' at "
                    f"version {comp_ver}, but the real codec's currently "
                    f"accepted input versions are "
                    f"{sorted(real['inputVersions'])} -- support for that "
                    f"historical version was removed without retiring or "
                    f"updating this baseline")

    tracked_versions = (
        verified_tracked if verified_tracked is not None
        else manifest_tracked_versions)

    for comp_id, real in real_registry.items():
        tracked = tracked_versions.get(comp_id, set())
        if real.get("required") and not tracked:
            violations.append(
                f"component '{comp_id}' is REQUIRED (accepted input "
                f"versions {sorted(real['inputVersions'])}) but is not "
                f"tracked by ANY manifest baseline's components[] -- a "
                f"required component with no baseline exercising it has "
                f"no proof its default/migration policy for a session "
                f"predating it was ever considered; add it to some "
                f"baseline's components[] (backed by a fixture covering "
                f"it) before this gap ships")
            continue
        if not tracked:
            # An optional component with NO baseline tracking it at all
            # is requirement 9's legitimate "a legacy save may simply
            # lack this" case -- no obligation, same as before.
            continue
        current = real["currentVersion"]
        if current not in tracked:
            violations.append(
                f"component '{comp_id}' is currently at version {current} "
                f"(tracked versions: {sorted(tracked)}), but no manifest "
                f"baseline declares/tracks a fixture at its CURRENT "
                f"version {current} -- a version bump with no fixture/"
                f"frozen-DTO/migration ever exercising the NEW version has "
                f"no compatibility coverage proving that shape was "
                f"actually validated, even though an older input version "
                f"is tracked elsewhere (round-10 review)")
        if len(real["inputVersions"]) <= 1:
            continue
        oldest = min(real["inputVersions"])
        if oldest not in tracked:
            violations.append(
                f"component '{comp_id}' accepts input versions "
                f"{sorted(real['inputVersions'])} (a migration exists from "
                f"v{oldest}), but no manifest baseline declares/tracks a "
                f"fixture at v{oldest} -- that historical migration has no "
                f"tracked compatibility coverage")

    return violations


# The b1-initial-session baseline's ONLY migration path is
# World.Save.Compat.SessionV90.migrateSessionV90, which threads every
# page-scoped modern component's construction through one of these named
# helpers (see the function's own source: `base = basePageSnapshots
# (...); afterEdits <- applyWorldEdits 1 (...) base; ...`) -- this
# mapping is a maintained LOOKUP (Python cannot itself discover a
# Haskell helper's name), but round-13 review: whether a REQUIRED
# component even NEEDS an entry here at all is no longer trusted to
# whoever edits this dict by hand -- audit_b1_migration_covers_
# page_scoped_components (below) now derives the set of components that
# need SOME accounted policy directly from the REAL registry, so a
# brand-new required component that nobody added here shows up as its
# own violation, not silent gap.
SESSION_V90_APPLY_HELPER_FOR_COMPONENT = {
    "world-edits":    "applyWorldEdits",
    "world-activity": "applyWorldActivity",
    "buildings":      "applyBuildings",
    "units":          "applyUnits",
    "unit-sim":       "applyUnitSim",
    "craft-bills":    "applyCraftBills",
    "power-nodes":    "applyPowerNodes",
}

# Components migrateSessionV90 constructs WITHOUT a named per-component
# apply* helper, because they are built directly as part of the ONE
# SessionSnapshot/PageSnapshot record literal GHC already forces to be
# total (so they can never silently go unconstructed the way a
# forgotten apply* call for a page-scoped component could):
#   - "core-session"/"texture-palette": global (not page-scoped) fields
#     -- allocators/camera and the texture palette -- set directly in
#     that one record literal.
#   - "world-pages": the FOUNDATION every page is built from
#     (basePageSnapshots, from the frozen v90 blob's own worldgen
#     params) -- not a component layered ON TOP of that foundation via
#     an apply* call the way every other page-scoped component is.
# "metadata"/"session" are B1's own INPUT being migrated FROM, not a
# component migrateSessionV90 constructs; "lua-state"/"lua.*" are B2/B3
# concerns entirely -- B1 predates Lua persistence altogether and
# always defaults every current Lua module via isMigratingLegacyBaseline,
# unrelated to any page-scoped Haskell helper.
SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS = {
    "core-session", "texture-palette", "world-pages", "metadata", "session",
    "lua-state",
}


def audit_b1_migration_covers_page_scoped_components(
        real_registry: dict, source_path: Path = SESSION_V90_SOURCE_PATH,
) -> list[str]:
    """Requirement 5 (issue #766): "introducing a new required component
    requires a migration/default policy for every supported older
    baseline". The b1-initial-session baseline can never simply declare
    coverage for a page-scoped component in its manifest components[]
    (it is structurally {metadata, session} only) -- its ACTUAL
    guarantee lives in migrateSessionV90's source threading every
    current page-scoped component through its own named apply* helper.
    This is the closest thing a static Python audit can check without
    literally compiling Haskell: if a future required page-scoped
    component's helper name isn't referenced anywhere in this file,
    something was renamed/removed/forgotten with nothing left to prove
    B1 sessions still migrate it.

    Round-13 review: previously only checked the FIXED
    SESSION_V90_APPLY_HELPER_FOR_COMPONENT dict's own entries against the
    source -- a brand-new required Haskell component that nobody
    remembered to ALSO add to that dict was invisible to this audit
    entirely (modern-baseline completeness explicitly exempts the
    b1-shaped baseline from needing full components[] coverage, so
    nothing else would catch it either). Now derives the set of
    components requiring SOME accounted B1 policy directly from
    real_registry (every REQUIRED, non-Lua id minus the explicitly
    justified SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS exemptions above),
    so an unclassified new required component is its own violation
    rather than a silent gap."""
    text = source_path.read_text(encoding="utf-8")
    violations = [
        f"World.Save.Compat.SessionV90.migrateSessionV90 (the "
        f"b1-initial-session baseline's ONLY migration path) no longer "
        f"references '{helper}' for component '{comp_id}' -- a new "
        f"required page-scoped component, or one whose construction "
        f"helper was renamed/removed, would silently have no accounted "
        f"default/migration policy for a session predating it"
        for comp_id, helper in SESSION_V90_APPLY_HELPER_FOR_COMPONENT.items()
        if helper not in text
    ]
    for comp_id, real in real_registry.items():
        if (not real.get("required") or comp_id.startswith("lua.")
                or comp_id in SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS
                or comp_id in SESSION_V90_APPLY_HELPER_FOR_COMPONENT):
            continue
        violations.append(
            f"component '{comp_id}' is REQUIRED in the real registry but "
            f"has NO known migration-helper mapping in "
            f"SESSION_V90_APPLY_HELPER_FOR_COMPONENT and is not listed in "
            f"SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS either -- B1 "
            f"compatibility for this component has never been verified at "
            f"all (round-13 review: this is exactly the 'a brand-new "
            f"required component nobody classified' gap this audit exists "
            f"to catch); add its migrateSessionV90 helper name to "
            f"SESSION_V90_APPLY_HELPER_FOR_COMPONENT, or -- if it is "
            f"genuinely built directly into the base record literal like "
            f"core-session/texture-palette/world-pages -- add it to "
            f"SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS instead")
    return violations


def audit_modern_baseline_components_complete(
        manifest: dict, real_registry: dict) -> list[str]:
    """Requirement 5's other half: a baseline that is NOT b1-shaped (its
    components[] doesn't declare the frozen legacy "session" component)
    is, by construction, a MODERN per-component-registry session -- and
    every one of those components (except "session"/"lua-state", each
    RETIRED, mutually-exclusive ALTERNATE representations for state the
    modern registry also covers: the frozen v90 blob vs the split
    Haskell components, and the single opaque pre-#761 Lua blob vs
    lua.unit_ai/lua.building_spawn -- a baseline declaring one is exempt
    from needing the modern equivalent it stands in for) is
    unconditionally REQUIRED (decodeEnvelope refuses a modern envelope
    missing one outright, see componentRequiredIds/
    MissingRequiredComponent). So a valid tracked "current"-target
    modern-shaped fixture cannot possibly omit any of them -- if a
    baseline's own components[] doesn't declare one, that baseline's
    manifest entry is under-documenting what its own fixture genuinely
    contains, precisely the gap round-5 review flagged (a future
    required component could be added to only ONE such baseline and
    never show up as a coverage gap in the OTHERS, since the
    all-baselines-aggregate check alone can't see that)."""
    modern_required_ids = {
        cid for cid, info in real_registry.items()
        if info.get("required") and cid not in ("session", "lua-state")
    }
    violations: list[str] = []
    for baseline in manifest.get("baselines", []):
        bid = baseline.get("id")
        declared_ids = {c.get("id") for c in baseline.get("components", [])}
        if "session" in declared_ids:
            continue  # b1-shaped: the frozen legacy alternative, exempt
        if baseline.get("migrationTarget") != "current":
            continue  # e.g. decode-only historical evidence, not a
                       # migration-acceptance baseline at all
        exempt_ids = ({"lua.unit_ai", "lua.building_spawn"}
                      if "lua-state" in declared_ids else set())
        missing = sorted(modern_required_ids - exempt_ids - declared_ids)
        if missing:
            violations.append(
                f"baseline '{bid}' is modern-shaped (its components[] "
                f"omits the legacy 'session' id) and targets 'current', "
                f"but a valid modern complete-session fixture cannot "
                f"structurally omit a required component -- yet its "
                f"components[] doesn't declare {missing}. Either this "
                f"baseline's own tracked fixture genuinely lacks them "
                f"(impossible for a real modern session) or its "
                f"components[] list is under-documented relative to "
                f"what the fixture actually contains -- add them")
    return violations


def _iter_fixtures(manifest: dict):
    for baseline in manifest.get("baselines", []):
        for fixture in baseline.get("fixtures", []):
            yield baseline, fixture


def audit_no_orphaned_fixture_files(
        manifest: dict, fixture_dir: Path = FIXTURE_DATA_DIR) -> list[str]:
    """Round-19 (post-approval) review: every check above verifies a
    DECLARED fixture's path exists and matches -- none of them verify the
    other direction, that every file actually sitting in fixture_dir is
    declared by SOME baseline. An orphaned file (left over from a rename,
    or a --generate-session/--add-baseline run that wrote bytes but was
    never wired into a baseline) gives no compatibility guarantee at all:
    it is never decoded, never migrated, never checksummed -- silently
    inert, yet sitting right alongside real tracked fixtures where it
    looks tracked."""
    if not fixture_dir.is_dir():
        return []
    referenced = set()
    for _baseline, fixture in _iter_fixtures(manifest):
        for key in ("path", "expectedCanonicalSummary"):
            val = fixture.get(key)
            if val:
                referenced.add((REPO_ROOT / val).resolve())
    violations = []
    for path in sorted(fixture_dir.iterdir()):
        if not path.is_file():
            continue
        if path.resolve() not in referenced:
            violations.append(
                f"'{path.relative_to(REPO_ROOT)}' exists under "
                f"{fixture_dir.relative_to(REPO_ROOT)}/ but is not "
                f"referenced by any manifest baseline's fixture 'path' or "
                f"'expectedCanonicalSummary' -- an orphaned fixture file is "
                f"never decoded, migrated, or checksummed by this audit or "
                f"the manifest-driven hspec gate, giving no compatibility "
                f"guarantee at all despite looking tracked; register it in "
                f"a baseline's fixtures[] (see --add-baseline) or delete it")
    return violations


def audit(manifest: dict, fixture_dir: Path = FIXTURE_DATA_DIR) -> list[str]:
    violations: list[str] = []

    declared_framing = manifest.get("envelopeFramingVersion")
    real_framing = current_envelope_version()
    if declared_framing != real_framing:
        violations.append(
            f"manifest envelopeFramingVersion ({declared_framing}) disagrees "
            f"with World.Save.Envelope.currentEnvelopeVersion ({real_framing}) "
            f"-- a framing bump is a new format epoch and must update the "
            f"manifest deliberately, never silently")

    declared_fingerprint = manifest.get("frozenDtoFingerprint")
    real_fingerprint = frozen_dto_fingerprint()
    if declared_fingerprint != real_fingerprint:
        violations.append(
            f"manifest frozenDtoFingerprint ({declared_fingerprint}) disagrees "
            f"with the current World.Save.Compat.SessionV90 frozen-DTO field "
            f"layout ({real_fingerprint}) -- a field was added/removed/"
            f"reordered on an already-shipped frozen DTO (requirement 10), or "
            f"the manifest needs a deliberate update alongside the change")

    declared_framing_fingerprint = manifest.get("envelopeFramingFingerprint")
    real_framing_fingerprint = envelope_framing_fingerprint()
    if declared_framing_fingerprint != real_framing_fingerprint:
        violations.append(
            f"manifest envelopeFramingFingerprint ({declared_framing_fingerprint}) "
            f"disagrees with the current World.Save.Envelope.Codec/.Types wire "
            f"layout ({real_framing_fingerprint}) -- round-15 review: "
            f"envelopeFramingVersion alone is just an integer someone has to "
            f"remember to bump; this fingerprint catches an actual byte-layout "
            f"change (ComponentDescriptor's fields, the magic bytes, the "
            f"checksum algorithm, encodeEnvelope/decodeEnvelope's header "
            f"construction) shipping with envelopeFramingVersion left "
            f"untouched -- a new wire format with no format epoch. A moved "
            f"fingerprint is NOT by itself proof the wire format changed: it "
            f"reacts to any structural edit surviving normalization, and "
            f"plenty of those (a renamed local binding, a refactored helper) "
            f"leave the bytes identical. So decide first which happened. If "
            f"the on-disk layout really did change, bump "
            f"envelopeFramingVersion -- a deliberate, reviewed format epoch -- "
            f"and update this fingerprint together, or revert the change. If "
            f"the bytes are unchanged, record the new fingerprint alone and "
            f"leave envelopeFramingVersion exactly where it is")

    real_registry = real_component_registry()
    verified_tracked, descriptor_violations = verify_fixture_descriptors(manifest)
    violations.extend(descriptor_violations)
    violations.extend(
        audit_component_versions(manifest, real_registry, verified_tracked))
    violations.extend(audit_modern_baseline_components_complete(manifest, real_registry))
    violations.extend(audit_b1_migration_covers_page_scoped_components(real_registry))
    violations.extend(audit_no_orphaned_fixture_files(manifest, fixture_dir))

    for baseline, fixture in _iter_fixtures(manifest):
        fid = fixture.get("id", "<unnamed>")
        path_str = fixture.get("path")
        if not path_str:
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' has no path")
            continue
        fpath = REPO_ROOT / path_str
        if not fpath.exists():
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' path "
                f"'{path_str}' does not exist")
            continue
        fixture_kind = fixture.get("kind")
        expected_sha = fixture.get("sha256")
        summary_path_str = fixture.get("expectedCanonicalSummary")
        if fixture_kind == "complete-session":
            missing = [
                name for name, val in
                (("sha256", expected_sha), ("expectedCanonicalSummary", summary_path_str))
                if not val]
            if missing:
                # Round-9 review: a checksum-less and/or summary-less
                # "complete-session" entry bypasses BOTH this audit (the
                # checksum/summary checks below are skipped entirely when
                # sha256 is None) AND Test.Headless.World.Save.Compat's own
                # manifest-driven hspec gate (which only iterates
                # complete-session fixtures that HAVE a checksum) --
                # letting a baseline claim full end-to-end migration
                # coverage with no tracked binary, no expected canonical
                # summary, and no decode/migrate/assemble validation ever
                # run against it. Only "component-focused" fixtures (a
                # real hspec gate elsewhere is the audit trail instead --
                # see b3-lua-versioned-hspec-coverage/historical-b1-
                # session-recovered) may legitimately omit either.
                violations.append(
                    f"baseline '{baseline.get('id')}' fixture '{fid}' is "
                    f"declared \"kind\": \"complete-session\" but is "
                    f"missing {' and '.join(missing)} -- a complete-session "
                    f"fixture with no tracked checksum and/or no expected "
                    f"canonical summary is never actually decoded/migrated/"
                    f"validated by ANY gate (this audit skips checksum-less "
                    f"fixtures entirely, and the hspec manifest gate only "
                    f"selects complete-session fixtures WITH a checksum) -- "
                    f"either supply both through --add-baseline, or mark "
                    f"this fixture \"component-focused\" if its real "
                    f"coverage genuinely lives elsewhere")
        if expected_sha is None:
            # A component-focused fixture recorded as inline source (e.g.
            # recovered git history embedded as a hex literal) rather than
            # a tracked binary blob -- nothing to checksum here; its own
            # provenance field is the audit trail.
            continue
        actual_sha = hashlib.sha256(fpath.read_bytes()).hexdigest()
        if actual_sha != expected_sha:
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' at "
                f"'{path_str}' has drifted: sha256 {actual_sha} != manifest's "
                f"recorded {expected_sha} -- tracked fixtures must never be "
                f"hand-edited; regenerate through the real codec and "
                f"re-register with --add-baseline")
            continue
        expected_size = fixture.get("sizeBytes")
        actual_size = fpath.stat().st_size
        if expected_size is not None and expected_size != actual_size:
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' size "
                f"{actual_size} != manifest's recorded {expected_size}")

        if summary_path_str:
            summary_path = REPO_ROOT / summary_path_str
            if not summary_path.exists():
                violations.append(
                    f"baseline '{baseline.get('id')}' fixture '{fid}' "
                    f"declares expectedCanonicalSummary '{summary_path_str}' "
                    f"which does not exist")

    for baseline in manifest.get("baselines", []):
        if not baseline.get("fixtures"):
            violations.append(
                f"baseline '{baseline.get('id')}' has no fixtures -- every "
                f"declared baseline needs at least one (requirement 14)")

    return violations


def cmd_audit(args: argparse.Namespace) -> int:
    manifest = load_manifest(MANIFEST_PATH)
    violations = audit(manifest)
    if violations:
        print(f"{len(violations)} save-compatibility violation(s):")
        for v in violations:
            print(f"  - {v}")
        return 1
    n_baselines = len(manifest.get("baselines", []))
    n_fixtures = sum(len(b.get("fixtures", [])) for b in manifest.get("baselines", []))
    print(f"save-compatibility audit: {n_baselines} baseline(s), "
          f"{n_fixtures} fixture(s) all intact")
    return 0


def _write_manifest_atomically(manifest: dict, manifest_path: Path = MANIFEST_PATH) -> None:
    """Write the manifest via a same-directory temp file + atomic rename,
    so a crash/interruption mid-write can never leave a half-written,
    unparseable manifest.json behind."""
    tmp = manifest_path.with_name(manifest_path.name + ".tmp")
    tmp.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    tmp.replace(manifest_path)


def _build_fixture_entry(args: argparse.Namespace) -> dict:
    fpath = REPO_ROOT / args.path
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
        summary_path = REPO_ROOT / args.summary
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
            cwd=REPO_ROOT, capture_output=True, text=True, timeout=1800)
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


class GenerationError(Exception):
    """A real-engine fixture-generation step failed (requirement 21)."""


#: How long --require-lua keeps retrying after --settle-seconds has
#: elapsed. Generous: a predicate that depends on a Lua tick can miss its
#: first window on a loaded machine, and the failure mode this exists to
#: prevent (a silently state-free fixture) is far worse than a slow run.
PREDICATE_RETRY_SECONDS = 30.0


def _parse_tile(text: str) -> tuple[int, int]:
    """Parse a "GX,GY" CLI tile argument."""
    try:
        gx_s, gy_s = text.split(",", 1)
        return int(gx_s.strip()), int(gy_s.strip())
    except ValueError:
        raise SystemExit(f"--spawn-unit-at expects 'GX,GY', got {text!r}")


def _make_isolated_gen_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory -- mirrors tools/save_compat_migration_probe.py's
    make_isolated_root/tools/save_storage_probe.py's own helper, so a
    generated fixture never touches a real player's saves."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO_ROOT, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def _bootstrap_gen_defs(send, port: int) -> None:
    """Load the defs a headless boot skips (no loading screen) but
    engine.saveWorld's own content still needs to resolve real
    building/unit/recipe names -- mirrors tools/multiworld_save_probe.py/
    tools/save_compat_migration_probe.py's identical helper. Only needed
    when actually spawning something (an entity-free session never
    references any def at all)."""
    import glob
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
        ("data/recipes/*.yaml",    "engine.loadRecipeYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    # Locations too (#915): they are placed AT world.init from the
    # registry, so a def registered later can never appear in the
    # generated world -- and a fixture that must capture per-unit
    # location knowledge needs a real placed location to know about.
    for path in sorted(glob.glob("data/locations/*.yaml")):
        send(port, f"engine.loadLocationYaml('{path}'); return 'ok'")


def render_setup_lua(stmt: str, bid, uid) -> str:
    """Substitute ONLY the two documented placeholders into a
    --setup-lua statement.

    Deliberately NOT str.format: Lua statements routinely contain
    ordinary braces (`{1, 2}`, `for k, v in pairs(t) do end`, a table
    constructor of any kind), and str.format reads every one of those as
    a format field -- raising KeyError/ValueError on a perfectly valid
    statement, or worse, silently consuming it. A plain two-token
    replace has no such surface. An unspawned side substitutes the Lua
    literal `nil`, so a statement referencing it fails loudly in Lua
    rather than interpolating the Python string "None"."""
    return (stmt
            .replace("{bid}", "nil" if bid is None else str(bid))
            .replace("{uid}", "nil" if uid is None else str(uid)))


def generate_current_format_session(
        port: int, page_id: str, seed: int, world_size: int, plate_count: int,
        spawn_building: str, spawn_unit: str, out_path: Path,
        spawn_unit_at: tuple[int, int] = (0, 0),
        settle_seconds: float = 0.0,
        setup_lua: list[str] | None = None,
        require_lua: str | None = None,
        world_name: str | None = None,
        world_gloss: str | None = None,
        language_seed: str | None = None,
        language_version: int | None = None,
        name_expr: str | None = None) -> None:
    """Boot a REAL headless engine (isolated resource root -- see
    _make_isolated_gen_root), init a world, optionally spawn ONE building
    and/or ONE unit through the SAME engine.saveWorld/building.spawn/
    unit.spawn verbs every other probe in this repo already uses, then
    save it -- producing genuine CURRENT-format envelope bytes through
    the real World.Save.Storage/Envelope.Codec production path (the
    exact same one an ordinary player save takes), not a hand-built or
    spliced value. Raises GenerationError on any rejected step.

    @setup_lua@ statements run after the spawns and before the settle,
    each sent as one debug-console line with `{bid}`/`{uid}` substituted
    for the ids the spawns above actually returned. Some state is
    written by neither a spawn verb nor a tick, but only by a real
    player/AI ACTION -- #1087's container knowledge is revealed by a
    completed storage interaction, never by proximity -- and a fixture
    that cannot stage that action can only ever capture the feature's
    empty default. A statement whose reply starts with a Lua error, or
    which is literally `false`/`nil`, fails generation: silently
    proceeding would produce exactly the hollow fixture this exists to
    prevent.

    @world_name@/@world_gloss@/@language_seed@ give the page a #1092
    language provenance, which is what makes its placed locations carry
    real generated names and glosses (#1101) instead of definition
    labels. Without them the fixture can only ever capture the
    no-language fallback -- an empty gloss on every location -- which
    would leave the new field untested by the very fixture registered to
    cover its wire version.

    @name_expr@ (#1104) is the encoded semantic expression the world
    name was rendered from -- world.suggestName's own `expr` reply. It is
    what puts a #1104 etymology source on the page's own identity; the
    page's locations and rivers acquire theirs from the language itself,
    so they need nothing here. Same reasoning as the provenance above: a
    fixture generated without it can only capture the absent case.

    This can only ever produce a fixture at the CURRENT wire format -- a
    live engine never writes a historical shape (see this module's own
    docstring for why a historical baseline stays a manual operation)."""
    from probelib import boot, send, quit_engine
    tmpdir = tempfile.mkdtemp(prefix="save_compat_gen_")
    slot = "generated"
    proc = None
    try:
        root = _make_isolated_gen_root(tmpdir)
        proc = boot(port, log=f"/tmp/save_compat_gen_{page_id}.log",
                    args=["--resource-root", root], ready_timeout=180)
        if spawn_building or spawn_unit:
            _bootstrap_gen_defs(send, port)
        init_args = f"'{page_id}', {seed}, {world_size}, {plate_count}"
        if world_name is not None:
            init_args += f", '{world_name}'"
            init_args += (f", '{world_gloss}'" if world_gloss is not None
                          else ", nil")
            if language_seed is not None:
                init_args += f", '{language_seed}'"
                # A name expression can only ride on the generated-name
                # path, and world.init reads it as argument 9 -- so the
                # version argument must be present (even as its default)
                # before it can be supplied positionally.
                if language_version is not None or name_expr is not None:
                    init_args += (f", {language_version}"
                                  if language_version is not None else ", nil")
                if name_expr is not None:
                    init_args += f", '{name_expr}'"
        inited = send(port, f"world.init({init_args}); return 'ok'")
        if "ok" not in inited:
            raise GenerationError(f"world.init failed: {inited!r}")
        time.sleep(1.0)  # let generation settle before saving/spawning

        # world.show (not just world.init) puts the page in wmVisible --
        # mirrors tools/multiworld_save_probe.py's identical note: without
        # it, building.spawn/canPlaceAt's visible-page terrain read
        # can reject a spawn, and the saved snapshot's own visiblePages/
        # live-camera-owner-page would come out empty/null instead of
        # matching an ordinary player session's shape.
        send(port, f"world.show('{page_id}'); return 'ok'")
        active_deadline = time.time() + 10.0
        while time.time() < active_deadline:
            if send(port, "return world.getActiveWorldId()").strip('"') == page_id:
                break
            time.sleep(0.2)
        else:
            raise GenerationError(f"'{page_id}' never became the active world")

        def as_int(s: str):
            try:
                return int(float(s))
            except (TypeError, ValueError):
                return None

        # unit.spawn/building.spawn return the new entity's id (a
        # non-negative integer, as a string) on success, not a boolean --
        # mirrors tools/multiworld_save_probe.py's as_int/bid<0 convention.
        bid = uid = None
        if spawn_building:
            r = send(port, f"return building.spawn('{spawn_building}', 0, 0)")
            bid = as_int(r)
            if bid is None or bid < 0:
                raise GenerationError(
                    f"building.spawn('{spawn_building}') rejected: {r!r}")
        if spawn_unit:
            ux, uy = spawn_unit_at
            r = send(port, f"return unit.spawn('{spawn_unit}', {ux}, {uy}, 0, "
                            f"'player')")
            uid = as_int(r)
            if uid is None or uid < 0:
                raise GenerationError(
                    f"unit.spawn('{spawn_unit}') at ({ux},{uy}) rejected: {r!r}")

        for stmt in (setup_lua or []):
            rendered = render_setup_lua(stmt, bid, uid)
            reply = send(port, rendered).strip()
            if (reply.startswith("error") or reply.startswith("Error")
                    or reply in ("false", "nil", '"false"', '"nil"')):
                raise GenerationError(
                    f"--setup-lua statement {rendered!r} did not succeed: "
                    f"{reply!r}")

        # Some state is not written by a spawn verb at all -- it is
        # ACQUIRED by a tick once the entity is in the right place (#915's
        # per-unit location memory is ingested by the unit-AI update from
        # world.getLocationAwareness). Let those ticks run, and refuse to
        # save until the caller's own predicate says the state is actually
        # there: a fixture that silently comes out WITHOUT the shape it
        # exists to track is worse than no fixture, because every audit
        # downstream then passes on it.
        if settle_seconds > 0:
            time.sleep(settle_seconds)
        if require_lua:
            deadline = time.time() + max(settle_seconds, PREDICATE_RETRY_SECONDS)
            while True:
                r = send(port, f"return ({require_lua}) and 'y' or 'n'")
                if r.strip().strip('"') == "y":
                    break
                if time.time() >= deadline:
                    raise GenerationError(
                        f"--require-lua never became true: {require_lua!r}")
                time.sleep(0.5)

        saved = send(port, f"return engine.saveWorld('{page_id}', '{slot}')")
        if saved.strip() != "true":
            raise GenerationError(f"engine.saveWorld failed: {saved!r}")
        saved_path = os.path.join(root, "saves", slot, "world.synworld")
        for _ in range(100):
            if os.path.isfile(saved_path):
                break
            time.sleep(0.1)
        if not os.path.isfile(saved_path):
            raise GenerationError(f"saved file never appeared at {saved_path}")
        out_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(saved_path, out_path)
    finally:
        if proc is not None:
            quit_engine(port, proc)
        shutil.rmtree(tmpdir, ignore_errors=True)

    # Round-11 review: normalize the freshly-generated fixture's
    # smTimestamp to a fixed constant AFTER the engine has already
    # exited -- engine.saveWorld's own wall-clock timestamp would
    # otherwise make two runs over identical inputs produce different
    # bytes/checksums, defeating requirement 21's reproducibility intent.
    ok, tail = normalize_fixture_timestamp(out_path)
    if not ok:
        raise GenerationError(
            f"timestamp normalization failed (fixture at {out_path} is "
            f"the raw, un-normalized engine.saveWorld output): {tail}")


# A small, permanent GHCi program (run via `cabal repl` subprocess) that
# derives a fixture's canonical-summary JSON DIRECTLY from its real,
# decoded SessionSnapshot/SaveMetadata -- not from live engine queries,
# several of which (hour/minute of day, in particular) have no debug-
# console verb to read at all. Mirrors EXACTLY the schema
# test-headless/Test/Headless/World/Save/Compat.hs's ExpectedSummary/
# ExpectedPage/Expected* Aeson types parse -- the two must be kept in
# sync by hand if that schema ever grows a field.
GHCI_DUMP_SUMMARY_TEMPLATE = r"""
:set -XOverloadedStrings -XTypeApplications
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text as T
import Data.List (sortOn)
import World.Save.Envelope (decodeSessionEnvelope)
import World.Save.Snapshot
import World.Save.Types
import World.Page.Types (WorldPageId(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import Item.Ground (GroundItems(..))
import Item.Types (ItemInstance(..))

bytes <- BS.readFile "{fixture_path}"

:{{
let luaNames = HS.fromList ["unit_ai", "building_spawn"]
    decoded = decodeSessionEnvelope luaNames luaNames bytes
:}}

:{{
case decoded of
  Left err -> putStrLn ("DUMP_FAILED: decode: " ++ T.unpack err)
  Right (meta, snap, luaComponents, isMig) -> do
    let dumpItem i = Aeson.object
          [ "defName" .= iiDefName i, "instanceId" .= iiInstanceId i
          , "currentFill" .= iiCurrentFill i, "quality" .= iiQuality i
          , "condition" .= iiCondition i, "weight" .= iiWeight i
          , "contents" .= map dumpItem (iiContents i) ]
        dumpBuilding (bid, b) = Aeson.object
          [ "id" .= unBuildingId bid, "defName" .= bisDefName b
          , "anchorX" .= bisAnchorX b, "anchorY" .= bisAnchorY b
          , "gridZ" .= bisGridZ b, "buildProgress" .= bisBuildProgress b ]
        dumpUnit (uid, u) = Aeson.object
          [ "id" .= unUnitId uid, "defName" .= uisDefName u
          , "gridX" .= uisGridX u, "gridY" .= uisGridY u
          , "gridZ" .= uisGridZ u, "facing" .= T.pack (show (uisFacing u))
          , "activity" .= uisActivity u, "pose" .= uisPose u
          , "inventory" .= map dumpItem (uisInventory u) ]
        dumpSim (uid, s) = Aeson.object
          [ "unitId" .= unUnitId uid, "realX" .= usRealX s, "realY" .= usRealY s
          , "gridZ" .= usGridZ s, "pose" .= T.pack (show (usPose s))
          , "state" .= T.pack (show (usState s))
          , "facing" .= T.pack (show (usFacing s)) ]
        dumpBill b = Aeson.object
          [ "id" .= unBillId (cbId b), "station" .= unBuildingId (cbStation b)
          , "recipe" .= cbRecipe b, "remaining" .= cbRemaining b
          , "claimant" .= fmap unUnitId (cbClaimant b)
          , "mode" .= T.pack (show (cbMode b)) ]
        dumpNode n = Aeson.object
          [ "id" .= unPowerNodeId (pnId n), "building" .= unBuildingId (pnBuilding n)
          , "role" .= T.pack (show (pnRole n)), "peakWatts" .= pnPeakWatts n
          , "capacityWh" .= pnCapacityWh n, "storedWh" .= pnStoredWh n ]
        dumpPage (WorldPageId pid, page) = Aeson.object
          [ "pageId" .= pid
          , "buildingCount" .= HM.size (bsnInstances (pgsBuildings page))
          , "unitCount" .= HM.size (usnInstances (pgsUnits page))
          , "unitSimStateCount" .= HM.size (pgsUnitSimStates page)
          , "craftBillCount" .= HM.size (cbsBills (pgsCraftBills page))
          , "powerNodeCount" .= HM.size (pnsNodes (pgsPowerNodes page))
          , "groundItemCount" .= HM.size (gisItems (pgsGroundItems page))
          , "timeHour" .= pgsTimeHour page, "timeMinute" .= pgsTimeMinute page
          , "dateYear" .= pgsDateYear page, "dateMonth" .= pgsDateMonth page
          , "dateDay" .= pgsDateDay page
          , "mapMode" .= T.pack (show (pgsMapMode page))
          , "buildings" .= map dumpBuilding
              (sortOn (unBuildingId . fst)
                 (HM.toList (bsnInstances (pgsBuildings page))))
          , "units" .= map dumpUnit
              (sortOn (unUnitId . fst) (HM.toList (usnInstances (pgsUnits page))))
          , "unitSimStates" .= map dumpSim
              (sortOn (unUnitId . fst) (HM.toList (pgsUnitSimStates page)))
          , "craftBills" .= map dumpBill
              (sortOn cbId (HM.elems (cbsBills (pgsCraftBills page))))
          , "powerNodes" .= map dumpNode
              (sortOn pnId (HM.elems (pnsNodes (pgsPowerNodes page))))
          ]
        cam = snapLiveCamera snap
        WorldPageId activePageText = snapActivePage snap
        summary = Aeson.object
          [ "metadata" .= Aeson.object
              [ "seed" .= smSeed meta, "worldSize" .= smWorldSize meta
              , "plateCount" .= smPlateCount meta, "worldName" .= smWorldName meta
              , "worldGloss" .= smWorldGloss meta ]
          , "gameTime" .= snapGameTime snap
          , "nextItemId" .= snapNextItemId snap
          , "nextBuildingId" .= snapNextBuildingId snap
          , "nextUnitId" .= snapNextUnitId snap
          , "camera" .= Aeson.object
              [ "ownerPage" .= fmap (\(WorldPageId p) -> p) (lcsOwnerPage cam)
              , "x" .= lcsX cam, "y" .= lcsY cam, "zoom" .= lcsZoom cam
              , "facing" .= T.pack (show (lcsFacing cam)) ]
          , "activePage" .= activePageText
          , "visiblePages" .= map (\(WorldPageId p) -> p) (snapVisiblePages snap)
          , "pages" .= map dumpPage
              (sortOn (\(WorldPageId p, _) -> p) (HM.toList (snapPages snap)))
          , "luaComponentCount" .= length luaComponents
          , "isMigratedLegacyBaseline" .= isMig
          ]
    BSL.writeFile "{output_path}" (Aeson.encode summary)
    putStrLn "DUMP_OK"
:}}
"""

# Fixed placeholder ISO-8601 timestamp (round-11 review), matching the
# same constant test-headless/Test/Headless/World/Save/Compat.hs's own
# hand-built SaveMetadata values already use -- NOT a real save time,
# deliberately, so two --generate-session runs over identical inputs
# produce byte-identical fixtures/checksums.
FIXED_GENERATED_TIMESTAMP = "2026-07-16T00:00:00.000000Z"

# A small, permanent GHCi program (run via `cabal repl`, mirroring
# GHCI_DUMP_SUMMARY_TEMPLATE's own subprocess pattern) that overwrites
# ONLY a freshly-generated fixture's "metadata" component's smTimestamp
# field with FIXED_GENERATED_TIMESTAMP, leaving every other
# component's version/required/payload bytes completely untouched.
#
# Round-11 review: engine.saveWorld (the real production save path
# --generate-session deliberately reuses, per requirement 21's "a real
# generation mode") always stamps the CURRENT WALL-CLOCK time into
# smTimestamp (Engine.Scripting.Lua.API.Save's getCurrentTime call,
# by design -- an ordinary player save needs each save to carry a
# distinct real timestamp). That means two --generate-session runs
# over IDENTICAL seed/world-size/plate-count/spawn arguments produce
# DIFFERENT envelope bytes and sha256s purely from wall-clock drift,
# defeating the reproducibility requirement 21 itself demands (a
# fixture's checksum must depend only on its declared generation
# inputs, not on when the command happened to run). This step
# normalizes that ONE field post-generation, via the real envelope
# codec (decode the raw manifest/payloads, rebuild every component's
# spec verbatim except metadata's, re-encode) rather than a hand-rolled
# binary patch -- so the fix stays correct through any future envelope
# framing change, exactly like every other fixture-generation step in
# this file.
GHCI_NORMALIZE_TIMESTAMP_TEMPLATE = r"""
:set -XOverloadedStrings
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import World.Save.Envelope.Codec
import World.Save.Envelope.Types
import World.Save.Envelope (currentEnvelopeVersion, metadataComponentId)
import World.Save.Component (componentKnownIds)
import World.Save.Types (SaveMetadata(..))

bytes <- BS.readFile "{fixture_path}"

:{{
let knownAll = HS.insert metadataComponentId
                 (HS.insert (ComponentId "lua.unit_ai")
                    (HS.insert (ComponentId "lua.building_spawn") componentKnownIds))
-- Structural re-encode only: knownAll widens what may APPEAR, while
-- the reader-required set stays EMPTY. Reusing knownAll for both would
-- demand that whatever fixture is being normalized carry every
-- component the current build knows about -- including any OPTIONAL one
-- added after the fixture was captured (#1087's container-knowledge),
-- which by definition it need not.
in case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion knownAll HS.empty bytes of
     Left e -> putStrLn ("NORMALIZE_FAILED: decode: " ++ show e)
     Right decoded ->
       case S.decode
              (HM.lookupDefault BS.empty metadataComponentId (dePayloads decoded))
              :: Either String SaveMetadata of
         Left e -> putStrLn ("NORMALIZE_FAILED: metadata decode: " ++ e)
         Right meta -> do
           let fixedMeta = meta {{ smTimestamp = "{fixed_timestamp}" }}
               newSpecs =
                 [ ( cdId d, cdVersion d, cdRequired d
                   , if cdId d == metadataComponentId
                        then S.encode fixedMeta
                        else HM.lookupDefault BS.empty (cdId d) (dePayloads decoded) )
                 | d <- emComponents (deManifest decoded) ]
           case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion newSpecs of
             Left e -> putStrLn ("NORMALIZE_FAILED: encode: " ++ show e)
             Right outBytes -> do
               BS.writeFile "{fixture_path}" outBytes
               putStrLn "NORMALIZE_OK"
:}}
"""


def normalize_fixture_timestamp(fixture_path: Path) -> tuple[bool, str]:
    """Run GHCI_NORMALIZE_TIMESTAMP_TEMPLATE via a `cabal repl` subprocess
    to overwrite fixture_path's metadata smTimestamp with
    FIXED_GENERATED_TIMESTAMP, in place. Returns (ok, diagnostic-tail-on-
    failure)."""
    script = GHCI_NORMALIZE_TIMESTAMP_TEMPLATE.format(
        fixture_path=str(fixture_path), fixed_timestamp=FIXED_GENERATED_TIMESTAMP)
    try:
        proc = subprocess.run(
            ["cabal", "repl", "test:synarchy-test-headless"],
            input=script, cwd=REPO_ROOT, capture_output=True, text=True,
            timeout=1800)
    except FileNotFoundError:
        return False, "'cabal' was not found on PATH"
    output = (proc.stdout or "") + (proc.stderr or "")
    if "NORMALIZE_OK" not in output:
        return False, "\n".join(output.splitlines()[-60:])
    return True, ""


# A small, permanent GHCi program (run via `cabal repl`, mirroring the
# other GHCI_*_TEMPLATE constants' subprocess pattern) that decodes a
# batch of REAL tracked fixture files' RAW envelope manifests -- their
# actual on-disk (id, version, required) descriptors, exactly as the
# real codec sees them -- and writes them all out as one JSON object
# keyed by fixture path. A single, UNIVERSAL known-id set (every
# Haskell/live-Lua modern id, plus BOTH retired legacy ids "session"
# and "lua-state") is used for every fixture regardless of which shape
# it actually is, since this only needs the envelope's STRUCTURAL
# manifest -- no application-level decode/migration -- to succeed for
# any of B1/B2/B3/C3's tracked shapes (round-12 review).
#
# Round-12 review: tools/save_compat_audit.py's version-coverage checks
# (audit_component_versions) previously trusted a baseline's declared
# components[] versions as-is, entirely from the manifest JSON -- never
# cross-checked against what a fixture's OWN bytes actually contain.
# Bumping only the manifest's declared version (with no fixture change
# at all) satisfied every coverage check while validating nothing.
# verify_fixture_descriptors (below) uses this dump to grind that
# claim against real, decoded descriptors before trusting it.
GHCI_DUMP_DESCRIPTORS_TEMPLATE = r"""
:set -XOverloadedStrings
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as AK
import World.Save.Envelope.Codec
import World.Save.Envelope.Types
import World.Save.Envelope (currentEnvelopeVersion, metadataComponentId)
import World.Save.Component (componentKnownIds)
import World.Save.Compat.SessionV90 (sessionComponentId)

:{
let universalKnown = HS.insert metadataComponentId
        (HS.insert sessionComponentId
            (HS.insert (ComponentId "lua-state")
                (HS.insert (ComponentId "lua.unit_ai")
                    (HS.insert (ComponentId "lua.building_spawn")
                        componentKnownIds))))
    cidText (ComponentId t) = t
    dumpOne path = do
      bytes <- BS.readFile path
      pure $ case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                     universalKnown HS.empty bytes of
        Left e -> (path, Left (show e))
        Right decoded -> (path, Right
          [ Aeson.object
              [ "id" .= cidText (cdId d), "version" .= cdVersion d
              , "required" .= cdRequired d ]
          | d <- emComponents (deManifest decoded) ])
in do
  results <- mapM dumpOne ("__FIXTURE_PATHS__" :: [FilePath])
  let failed = [ (p, e) | (p, Left e) <- results ]
  if not (null failed)
    then putStrLn ("DESCRIPTOR_DUMP_FAILED: " ++ show failed)
    else do
      let obj = Aeson.object
            [ AK.fromString p .= descs | (p, Right descs) <- results ]
      BSL.writeFile "__OUTPUT_PATH__" (Aeson.encode obj)
      putStrLn "DESCRIPTOR_DUMP_OK"
:}
"""


def dump_fixture_descriptors(
        fixture_paths: list[Path]) -> tuple[dict[str, list[dict]] | None, str]:
    """Run GHCI_DUMP_DESCRIPTORS_TEMPLATE via a single `cabal repl`
    subprocess to decode every path in fixture_paths' RAW envelope
    manifest. Returns (path-string -> [{"id","version","required"}, ...]
    for every fixture, "") on success, or (None, diagnostic) on any
    decode/subprocess failure."""
    if not fixture_paths:
        return {}, ""
    haskell_list = "[" + ",".join(
        json.dumps(str(p)) for p in fixture_paths) + "]"
    with tempfile.NamedTemporaryFile(
            suffix=".json", dir=REPO_ROOT, delete=False) as tf:
        output_path = Path(tf.name)
    try:
        script = (GHCI_DUMP_DESCRIPTORS_TEMPLATE
            .replace('"__FIXTURE_PATHS__"', haskell_list)
            .replace("__OUTPUT_PATH__", str(output_path)))
        try:
            proc = subprocess.run(
                ["cabal", "repl", "test:synarchy-test-headless"],
                input=script, cwd=REPO_ROOT, capture_output=True, text=True,
                timeout=1800)
        except FileNotFoundError:
            return None, "'cabal' was not found on PATH"
        output = (proc.stdout or "") + (proc.stderr or "")
        if "DESCRIPTOR_DUMP_OK" not in output or not output_path.exists():
            return None, "\n".join(output.splitlines()[-60:])
        return json.loads(output_path.read_text(encoding="utf-8")), ""
    finally:
        output_path.unlink(missing_ok=True)


def verify_fixture_descriptors(
        manifest: dict) -> tuple[dict[str, set[int]] | None, list[str]]:
    """Round-12 review: ground audit_component_versions' coverage checks
    in REAL fixture bytes, not a baseline's self-reported components[]
    claim. Decodes every "complete-session" fixture with a tracked
    checksum (the only fixtures whose bytes genuinely carry a full,
    real component manifest -- a "component-focused" fixture may be an
    isolated Lua payload or inline source, not a full envelope) and:

      - flags any baseline components[] entry (id, version, required)
        that does NOT match ANY of that baseline's own real, decoded
        fixtures' descriptors -- catching a manifest edit that claims a
        version bump with no fixture ever actually re-encoded at it;
      - returns the VERIFIED (fixture-backed) id -> {tracked versions}
        map for audit_component_versions to use instead of trusting the
        manifest JSON directly.

    Returns (None, []) when there is nothing to verify against at all
    (no complete-session fixture anywhere -- e.g. a synthetic test
    manifest exercising an unrelated check), telling the caller to fall
    back to the manifest-JSON-trusting behavior unchanged. Returns
    (None, [diagnostic]) if the decode step itself fails (no cabal on
    PATH, a genuinely corrupt fixture, etc.) -- the caller's other,
    unrelated checks still run normally, but every version-coverage
    check this powers falls back too until fixed, with that fact
    surfaced as its own violation rather than silently trusting
    unverified claims."""
    complete_session_paths: dict[str, tuple[str, Path]] = {}
    for baseline, fixture in _iter_fixtures(manifest):
        if fixture.get("kind") != "complete-session" or not fixture.get("sha256"):
            continue
        path_str = fixture.get("path")
        if not path_str:
            continue
        fpath = REPO_ROOT / path_str
        if not fpath.exists():
            continue
        complete_session_paths[path_str] = (baseline.get("id"), fpath)

    if not complete_session_paths:
        return None, []

    dumped, tail = dump_fixture_descriptors(
        [p for (_, p) in complete_session_paths.values()])
    if dumped is None:
        return None, [
            "could not verify manifest components[] against real fixture "
            "descriptors (every version-coverage check below falls back to "
            "trusting the manifest's own claim until this is fixed): " + tail]

    violations: list[str] = []
    verified_tracked: dict[str, set[int]] = {}
    for path_str, (bid, fpath) in complete_session_paths.items():
        descs = dumped.get(str(fpath))
        if descs is None:
            violations.append(
                f"baseline '{bid}' fixture at '{path_str}' was not decoded "
                f"(missing from the descriptor dump's own output) -- "
                f"cannot verify its declared components[] against real bytes")
            continue
        for d in descs:
            verified_tracked.setdefault(d["id"], set()).add(d["version"])

    for baseline in manifest.get("baselines", []):
        bid = baseline.get("id")
        this_baseline_descs = [
            dumped[str(fpath)]
            for (owner_bid, fpath) in complete_session_paths.values()
            if owner_bid == bid and str(fpath) in dumped
        ]
        if not this_baseline_descs:
            # Nothing to verify against (e.g. this baseline's only
            # fixtures are "component-focused" or checksum-less) --
            # audit_component_versions' own existing checks still cover
            # id/version-validity against the real codec registry.
            continue
        for comp in baseline.get("components", []):
            comp_id, comp_ver, comp_req = (
                comp.get("id"), comp.get("version"), comp.get("required"))
            if not any(
                    any(d["id"] == comp_id and d["version"] == comp_ver
                        and d["required"] == comp_req for d in descs)
                    for descs in this_baseline_descs):
                violations.append(
                    f"baseline '{bid}' declares component '{comp_id}' at "
                    f"version {comp_ver} (required={comp_req}), but NONE of "
                    f"its own real, decoded fixtures actually carry a "
                    f"matching descriptor -- this baseline's components[] "
                    f"claim is not backed by any tracked fixture's bytes "
                    f"(round-12 review: a manifest-only version bump with "
                    f"no fixture re-encoded at it must not silently satisfy "
                    f"this baseline's own coverage)")

    return verified_tracked, violations


def dump_canonical_summary(fixture_path: Path, output_path: Path) -> tuple[bool, str]:
    """Run GHCI_DUMP_SUMMARY_TEMPLATE via a `cabal repl` subprocess to
    derive fixture_path's canonical summary and write it to output_path.
    Returns (ok, diagnostic-tail-on-failure)."""
    script = GHCI_DUMP_SUMMARY_TEMPLATE.format(
        fixture_path=str(fixture_path), output_path=str(output_path))
    try:
        proc = subprocess.run(
            ["cabal", "repl", "test:synarchy-test-headless"],
            input=script, cwd=REPO_ROOT, capture_output=True, text=True,
            timeout=1800)
    except FileNotFoundError:
        return False, "'cabal' was not found on PATH"
    output = (proc.stdout or "") + (proc.stderr or "")
    if "DUMP_OK" not in output or not output_path.exists():
        return False, "\n".join(output.splitlines()[-60:])
    return True, ""


def cmd_generate(args: argparse.Namespace) -> int:
    """--generate-session: produce a brand-new CURRENT-format complete-
    session fixture through the real engine + real codec end to end,
    then delegate straight to cmd_add_baseline for the SAME atomic
    registration + real-codec validation --add-baseline already does
    (this only ever produces a "complete-session" fixture, so args.kind
    is fixed here rather than asked for).

    Round-6 review: stages fixture + summary + manifest together and
    rolls ALL of them back on ANY downstream failure (dump derivation or
    manifest real-codec validation) -- not just the manifest. Without
    this, a validation failure left new fixture/summary bytes sitting on
    disk unregistered, or (with --force) clobbered a PREVIOUSLY-tracked
    fixture's bytes with new-but-invalid content while the manifest
    (correctly rolled back on its own) still pointed at the OLD
    checksum -- either way, a state the NEXT audit run would immediately
    flag as drifted, or that would simply litter the repo with orphaned
    files."""
    fixture_path = REPO_ROOT / args.path
    summary_path = REPO_ROOT / args.summary
    if (fixture_path.exists() or summary_path.exists()) and not args.force:
        print(f"refusing to overwrite an existing file at '{args.path}' "
              f"or '{args.summary}' -- pass --force if this is deliberate",
              file=sys.stderr)
        return 1

    # Captured BEFORE any write, so a failure at ANY stage below can
    # restore both files to their EXACT prior state (or remove them, if
    # they didn't exist before this invocation) -- never leaving a
    # half-written or stale-but-mismatched pair behind.
    orig_fixture = fixture_path.read_bytes() if fixture_path.exists() else None
    orig_summary = summary_path.read_text(encoding="utf-8") if summary_path.exists() else None

    def restore_files() -> None:
        if orig_fixture is None:
            fixture_path.unlink(missing_ok=True)
        else:
            fixture_path.write_bytes(orig_fixture)
        if orig_summary is None:
            summary_path.unlink(missing_ok=True)
        else:
            summary_path.write_text(orig_summary, encoding="utf-8")

    try:
        generate_current_format_session(
            port=args.port, page_id=args.page_id, seed=args.seed,
            world_size=args.world_size, plate_count=args.plate_count,
            spawn_building=args.spawn_building, spawn_unit=args.spawn_unit,
            out_path=fixture_path, spawn_unit_at=_parse_tile(args.spawn_unit_at),
            settle_seconds=args.settle_seconds, setup_lua=args.setup_lua,
            require_lua=args.require_lua, world_name=args.world_name,
            world_gloss=args.world_gloss, language_seed=args.language_seed,
            language_version=args.language_version,
            name_expr=args.name_expr)
    except GenerationError as e:
        # Round-16 review: generate_current_format_session no longer
        # ONLY writes fixture_path as an untouchable-if-failed last step
        # -- since round-11's normalize_fixture_timestamp call, a
        # GenerationError can ALSO be raised AFTER shutil.copyfile has
        # already overwritten fixture_path with newly-generated (but
        # not-yet-normalized) bytes, e.g. clobbering a previously-tracked
        # fixture under --force with no rollback. restore_files() is
        # always safe to call here regardless of which stage failed --
        # it is a no-op when fixture_path was never actually touched.
        restore_files()
        print(f"fixture generation failed (fixture/summary restored to "
              f"their prior state): {e}", file=sys.stderr)
        return 1

    ok, tail = dump_canonical_summary(fixture_path, summary_path)
    if not ok:
        restore_files()
        print(f"canonical-summary derivation failed (fixture/summary "
              f"restored to their prior state): {tail}", file=sys.stderr)
        return 1

    args.kind = "complete-session"
    if not args.provenance:
        args.provenance = (
            f"Generated through the real codec (tools/save_compat_audit.py "
            f"--generate-session): a real headless engine booted in an "
            f"isolated resource root, world.init('{args.page_id}', "
            f"{args.seed}, {args.world_size}, {args.plate_count}"
            + (f", '{args.world_name}'" if args.world_name else "")
            + (f" named in the generated language seeded "
               f"{args.language_seed}" if args.language_seed else "")
            + ")"
            + (f", building.spawn('{args.spawn_building}', 0, 0)"
               if args.spawn_building else "")
            + (f", unit.spawn('{args.spawn_unit}', {args.spawn_unit_at}, "
               f"0, 'player')" if args.spawn_unit else "")
            + (f", then " + "; ".join(args.setup_lua)
               if args.setup_lua else "")
            + (f", settled {args.settle_seconds}s"
               if args.settle_seconds else "")
            + (f" and held until `{args.require_lua}`"
               if args.require_lua else "")
            + f", then engine.saveWorld -- the exact production save path "
              f"an ordinary player save takes. Its canonical summary was "
              f"derived directly from the real decoded SessionSnapshot "
              f"(dump_canonical_summary), not hand-transcribed.")
    rc = cmd_add_baseline(args)
    if rc != 0:
        restore_files()
        print(f"registration/validation failed -- fixture/summary "
              f"restored to their prior state too (not just the "
              f"manifest)", file=sys.stderr)
    return rc


def cmd_add_baseline(args: argparse.Namespace) -> int:
    manifest = load_manifest(MANIFEST_PATH)
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
            manifest, MANIFEST_PATH, args.kind, args.skip_validation,
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
        manifest, MANIFEST_PATH, args.kind, args.skip_validation,
        f"registered fixture '{args.fixture_id}' on baseline "
        f"'{args.baseline_id}': sha256={new_fixture['sha256']} "
        f"sizeBytes={new_fixture['sizeBytes']}")


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
        return cmd_generate(args)
    if args.add_baseline:
        if not args.baseline_id or not args.fixture_id or not args.path or not args.kind:
            ap.error("--add-baseline requires --baseline-id, --fixture-id, "
                     "--path, and --kind")
        return cmd_add_baseline(args)
    return cmd_audit(args)


if __name__ == "__main__":
    sys.exit(main())
