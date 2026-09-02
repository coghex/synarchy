#!/usr/bin/env python3
"""Haskell/Lua save-component discovery for the save-compatibility tool
(issue #2049, requirement 5).

A LEAF service (requirement 15): it reads SOURCE and answers what the
build's real component registry currently is, plus the version-coverage
policy expressed purely over that registry. It never loads the manifest
file, writes anything, or spawns a subprocess.

It owns, in one place:

  - Lua persistence-module discovery (`discover_lua_save_modules`);
  - Haskell `componentCodec` declaration parsing
    (`discover_component_specs` and its lexers);
  - `csOlderVersions` parsing and validation (`_parse_older_versions`,
    `_check_older_versions`);
  - real component-registry construction (`real_component_registry`);
  - the required/current/oldest version coverage policy
    (`audit_component_versions`).

That last one is requirement 5's "coverage policy" and is owned HERE,
not by the manifest audit: it is a function of the REAL registry's own
semantics (which components are required, which versions each codec
still accepts). Requirement 9's "component/version and required-
component coverage" is the aggregate audit CALLING it -- there is
exactly one implementation, never a second copy (requirement 16).

`_extract_toplevel_block` and `metadata_input_versions` are imported
from save_compat_audit_fingerprint, which owns the Haskell top-level
lexer and the envelope/metadata input-version discovery. That edge runs
one way, so the leaf services stay acyclic.

The public façade is tools/save_compat_audit.py.
"""
from __future__ import annotations

import re
from pathlib import Path

import save_compat_audit_common as common
import save_compat_audit_fingerprint as fingerprint

REGISTER_COMPONENT_RE = re.compile(r"registerComponent\s+(\w+)")

# Every genuine Lua-module save-persistence registration call site
# (round-16 review): globbed across ALL of scripts/ rather than a fixed
# 2-file list, so a new registered Lua module in ANY file is discovered
# automatically -- mirrors the Haskell-side fix's identical reasoning.
LUA_SAVE_MODS_REGISTER_RE = re.compile(r'saveMods\.register\(\s*"(\w+)"')


def discover_lua_save_modules(
        scripts_root: Path | None = None) -> list[tuple[Path, str]]:
    """Every (file, module id) pair where a REAL `saveMods.register("id",
    {...})` call site exists, discovered by scanning every .lua file
    under scripts/ -- not a hand-maintained guess at which 2 files do
    this (round-16 review).

    @scripts_root@ defaults to 'common.REPO_ROOT / "scripts"', resolved
    HERE rather than bound as a default argument (issue #2049
    requirement 18)."""
    if scripts_root is None:
        scripts_root = common.REPO_ROOT / "scripts"
    found = []
    for path in sorted(scripts_root.rglob("*.lua")):
        text = path.read_text(encoding="utf-8")
        for lua_id in LUA_SAVE_MODS_REGISTER_RE.findall(text):
            found.append((path, lua_id))
    return found

METADATA_COMPONENT_VERSION_RE = re.compile(
    r"^metadataComponentVersion\s*=\s*(\d+)", re.MULTILINE)

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

    envelope_text = common.ENVELOPE_SOURCE_PATH.read_text(encoding="utf-8")
    m = METADATA_COMPONENT_VERSION_RE.search(envelope_text)
    if not m:
        raise ValueError(
            f"could not find metadataComponentVersion in "
            f"{common.ENVELOPE_SOURCE_PATH}")
    # metadata is unconditionally required -- every envelope carries it
    # (World.Save.Envelope's own decode refuses one that doesn't).
    metadata_current = int(m.group(1))
    registry["metadata"] = {
        "currentVersion": metadata_current,
        "inputVersions": fingerprint.metadata_input_versions(
            envelope_text, metadata_current),
        "required": True}

    # "session" is the ONE frozen legacy component (World.Save.Compat.
    # SessionV90) -- its current version is its only ever version; a
    # further schema change adds a new frozen type instead of bumping
    # this one (the frozen-DTO boundary rule), so inputVersions is
    # always the singleton [currentVersion]. Unconditionally required --
    # it IS the whole legacy envelope's one gameplay component.
    session_text = common.SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8")
    m = SESSION_COMPONENT_VERSION_RE.search(session_text)
    if not m:
        raise ValueError(
            f"could not find sessionComponentVersion in "
            f"{common.SESSION_V90_SOURCE_PATH}")
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
    for path in common.HASKELL_COMPONENT_SOURCE_PATHS:
        text = path.read_text(encoding="utf-8")
        for ident, sid in COMPONENT_ID_LITERAL_RE.findall(text):
            id_literals[ident] = sid

    discovered_codec_names: set[str] = set()
    for path in common.HASKELL_COMPONENT_SOURCE_PATHS:
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
    registry_source_text = common.COMPONENT_REGISTRY_SOURCE_PATH.read_text(
        encoding="utf-8")
    registry_list_block = fingerprint._extract_toplevel_block(
        registry_source_text, "saveComponentRegistry")
    authoritative_codec_names = REGISTER_COMPONENT_RE.findall(registry_list_block)
    if not authoritative_codec_names:
        raise ValueError(
            f"no 'registerComponent <codec>' entries found in "
            f"saveComponentRegistry "
            f"({common.COMPONENT_REGISTRY_SOURCE_PATH}) -- "
            f"did the registry get restructured?")
    missing_codec_names = [
        name for name in authoritative_codec_names
        if name not in discovered_codec_names]
    if missing_codec_names:
        raise ValueError(
            f"saveComponentRegistry references "
            f"{', '.join(missing_codec_names)}, but no matching codec "
            f"definition was found anywhere under "
            f"{common.HASKELL_COMPONENT_SOURCE_PATHS[0].parent} in the "
            f"expected "
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
            f"anywhere under {common.REPO_ROOT / 'scripts'} -- did every "
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
