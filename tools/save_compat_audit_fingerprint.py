#!/usr/bin/env python3
"""Source fingerprinting for the save-compatibility tool (issue #2049,
requirement 6).

A LEAF service (requirement 15): it reads Haskell/cabal SOURCE and
answers questions about it, and imports nothing from this tool beyond
the shared definitions owner. Nothing here loads the manifest, writes a
file, or spawns a subprocess.

It owns, in one place:

  - live Cabal default-extension discovery (`inherited_default_extensions`);
  - module-header and pragma normalization (`_strip_line_comments`,
    `_normalize_haskell_block`, `_drop_redundant_language_pragmas`);
  - frozen DTO transitive-block discovery (`_transitive_dto_blocks`);
  - frozen-DTO fingerprinting (`frozen_dto_fingerprint`);
  - envelope-framing fingerprinting (`envelope_framing_fingerprint`);
  - current envelope and metadata input-version discovery
    (`current_envelope_version`, `metadata_input_versions`).

`_extract_toplevel_block` lives here too, as the one Haskell top-level
lexer both fingerprints already used; save_compat_audit_components
imports it rather than growing a second copy (requirement 16). That
components -> fingerprint edge is the only one between two leaf
services, and it runs one way, so the dependency graph stays acyclic.

The public façade is tools/save_compat_audit.py.
"""
from __future__ import annotations

import hashlib
import re
from pathlib import Path

import save_compat_audit_common as common

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
        cabal_path: Path | None = None) -> dict[str, bool]:
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
    safe, but invisibly so).

    @cabal_path@ defaults to 'common.CABAL_PATH', resolved HERE rather
    than bound as a default argument, so the self-test's rebinding of
    that module attribute is actually seen (issue #2049 requirement 18).
    """
    cabal_path = common.CABAL_PATH if cabal_path is None else cabal_path
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


def frozen_dto_fingerprint(source_path: Path | None = None) -> str:
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
    requirement 10 is guarding.

    @source_path@ defaults to 'common.SESSION_V90_SOURCE_PATH', resolved
    HERE rather than bound as a default argument (issue #2049
    requirement 18).
    """
    source_path = (common.SESSION_V90_SOURCE_PATH if source_path is None
                   else source_path)
    text = source_path.read_text(encoding="utf-8")
    own_blocks = re.findall(
        r"^data \w+ = \w+.*?deriving\s*\([^)]*\)", text,
        re.MULTILINE | re.DOTALL)
    if not own_blocks:
        raise ValueError(
            f"no frozen `data ... deriving (...)` blocks found in "
            f"{source_path} -- did the module get restructured?")
    leaf_blocks = _transitive_dto_blocks(
        "\n".join(own_blocks), common.HASKELL_COMPONENT_SOURCE_PATHS)
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
        types_path: Path | None = None,
        codec_path: Path | None = None,
        cabal_path: Path | None = None) -> str:
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
    this fingerprint.

    All three paths default to their 'common' counterparts, resolved
    HERE rather than bound as default arguments (issue #2049
    requirement 18).
    """
    types_path = (common.ENVELOPE_TYPES_SOURCE_PATH if types_path is None
                  else types_path)
    codec_path = (common.ENVELOPE_CODEC_SOURCE_PATH if codec_path is None
                  else codec_path)
    cabal_path = common.CABAL_PATH if cabal_path is None else cabal_path
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

CURRENT_ENVELOPE_VERSION_RE = re.compile(
    r"^currentEnvelopeVersion\s*=\s*(\d+)", re.MULTILINE)


def current_envelope_version(path: Path | None = None) -> int:
    path = common.ENVELOPE_SOURCE_PATH if path is None else path
    text = path.read_text(encoding="utf-8")
    m = CURRENT_ENVELOPE_VERSION_RE.search(text)
    if not m:
        raise ValueError(f"could not find currentEnvelopeVersion in {path}")
    return int(m.group(1))

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
            f"{common.ENVELOPE_SOURCE_PATH}")
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
                f"metadataComponentInputVersions in {common.ENVELOPE_SOURCE_PATH} "
                f"carries an entry this parser cannot resolve: {token!r}")
    if not versions:
        raise ValueError(
            f"metadataComponentInputVersions in "
            f"{common.ENVELOPE_SOURCE_PATH} is empty")
    return sorted(set(versions))
