"""Current-schema model for tools/current_save_schema_audit.py (issue #2916).

Reads one source TREE (the real checkout, a `git archive` of a base
revision, or a synthetic tree in the self-test) and answers: which save
carriers exist, at which real on-disk version, which older versions they
still accept, and a digest of the declarations that make up their
CURRENT shape. Nothing here talks to git, writes a file, or compares two
trees -- that is `current_save_schema_compare`.

A carrier is one independently versioned thing on disk:

  component   a Haskell `componentCodec ComponentSpec` (csVersion,
              csOlderVersions)
  metadata    the `"metadata"` component (metadataComponentVersion,
              metadataComponentInputVersions)
  lua         a `saveMods.register("id", {...})` module (version,
              inputVersions)
  framing     the envelope framing (currentEnvelopeVersion)
  legacy      a retired carrier only an exact-shape fallback ROUTE reads:
              the B1 `session` (SessionV90) and the B2 `lua-state` blob

Its SHAPE is a digest of (a) every `data`/`newtype`/`type` declaration
transitively reachable from the carrier's wire roots, (b) every
hand-written `Serialize` instance on a reached type, and (c) for Lua
and framing carriers the owning sources named in `OWNER_SOURCES`.
Comments, layout and strictness bangs never move it.

Its LEGACY SHAPE is the same digest over what only the still-accepted
OLDER versions reach (the frozen DTOs named by `csOlderVersions`
migrations, the frozen metadata mirrors). It exists because, once the
old audits are retired, nothing else guards a declaration that only an
old decoder reaches (issue #2916 review, correction 1).

Fail-closed contract: anything this reader cannot classify raises
`SchemaError` naming the boundary, never a silent omission.
"""
from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass, field
from pathlib import Path

import save_compat_audit_components as components
import save_compat_audit_fingerprint as fingerprint
from enum_append_only_audit_carrier import (
    first_argument_types,
    top_level_blocks,
)
from enum_append_only_audit_model import (
    QUALIFIED_RE,
    AuditError,
    Declaration,
    Scan,
)
from enum_append_only_audit_parse import (
    declaration_blocks,
    split_top_level,
    strip_haskell_comments,
)
from enum_append_only_audit_scan import scan_repository

RECORD_REL = "docs/save_compat/current_schema.json"
RECORD_FORMAT = 1

ENVELOPE_REL = "src/World/Save/Envelope.hs"
ENVELOPE_TYPES_REL = "src/World/Save/Envelope/Types.hs"
ENVELOPE_CODEC_REL = "src/World/Save/Envelope/Codec.hs"
COMPONENT_REGISTRY_REL = "src/World/Save/Component.hs"
COMPONENT_DIR_REL = "src/World/Save/Component"
CABAL_REL = "synarchy.cabal"
SCRIPTS_REL = "scripts"
DATA_CODEC_REL = "scripts/lib/data_codec.lua"
COMPONENT_MODULE_PREFIX = "World.Save.Component."
COMPONENT_TYPES_MODULE = "World.Save.Component.Types"

# Ownership of the carriers that are not `componentCodec` records.
# Every module/type named here must still exist: a stale entry fails.
METADATA_ROOT = "World.Save.Types.SaveMetadata"
SESSION_V90_MODULE = "World.Save.Compat.SessionV90"
METADATA_FROZEN_MODULE = "World.Save.Compat.MetadataV{n}"

# Bare type names that are DECLARED in more than one module yet reached
# by a carrier, with why unioning every same-named declaration is
# acceptable. An ambiguous name that is not listed fails: resolving a
# bare name against the wrong module could hide a real change.
AMBIGUOUS_TYPE_NAMES: dict[str, str] = {}

# The exact-shape legacy fallback routes. `entry` is the dispatcher the
# route is reached through; the route is ENABLED iff that dispatcher's
# body names one of `callees`. `carriers` are the carriers whose shape
# the route decodes through, so a shape change in any of them shows the
# route as affected.
# `carriers` are the carriers whose shape each route decodes through.
LEGACY_ROUTES = {
    "b1-session": {"carriers": ("metadata", "session")},
    "b2-lua-state": {
        "carriers": ("metadata", "lua-state"),
        # B2 decodes every REQUIRED Haskell component of its era through
        # the shared codecs (`assembleSnapshot`), so a version change in
        # any of them affects the route too.
        "required_components": True,
    },
}

# The two dispatchers, each with the decoder of route b1-session then of
# route b2-lua-state. A dispatcher is read ONLY in one of four exact
# forms (route on/off for each); every other spelling -- a qualified or
# forwarding call, an alias, a renamed decoder -- is refused, so a
# mutation can never read as "disabled".
DISPATCHERS = (
    ("tryLegacyEnvelopeFallbacks", "decodeLegacySessionEnvelope",
     "decodeB2SessionEnvelope"),
    ("tryLegacyMetadataFallbacks", "decodeLegacySessionMetadata",
     "decodeB2SessionMetadata"),
)


class SchemaError(Exception):
    """A boundary this reader could not classify; always names it."""


@dataclass
class Carrier:
    id: str
    kind: str
    current_version: int
    accepted: list[int]
    shape: str
    legacy_shape: str
    owners: list[str]
    legacy_owners: list[str]
    custom_owners: list[str]
    owner_sources: list[str]
    decl_hashes: dict[str, str] = field(default_factory=dict)
    legacy_hashes: dict[str, str] = field(default_factory=dict)
    required: bool = False

    def to_record(self) -> dict:
        return {
            "kind": self.kind,
            "currentVersion": self.current_version,
            "acceptedVersions": self.accepted,
            "shape": self.shape,
            "legacyShape": self.legacy_shape,
            "owners": self.owners,
            "legacyOwners": self.legacy_owners,
            "customOwners": self.custom_owners,
            "ownerSources": self.owner_sources,
        }


@dataclass
class Route:
    id: str
    enabled: bool
    carriers: list[str]

    def to_record(self) -> dict:
        return {"enabled": self.enabled, "carriers": self.carriers}


@dataclass
class Model:
    carriers: dict[str, Carrier]
    routes: dict[str, Route]

    def to_record(self) -> dict:
        return {
            "format": RECORD_FORMAT,
            "carriers": {k: self.carriers[k].to_record()
                         for k in sorted(self.carriers)},
            "routes": {k: self.routes[k].to_record()
                       for k in sorted(self.routes)},
        }


# ---------------------------------------------------------------------------
# Declarations, custom serializers and closures


def _sha(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


_TOKEN_RE = re.compile(
    r'"(?:\\.|[^"\\\n])*"|::|->|=>|[\w\'.]+|\S')
_OPERATORS = {"::": "∷", "->": "→", "=>": "⇒"}
_WIRE_CLASSES = ("Generic", "Serialize")
CANONICAL = "unsupported syntax: rewrite in canonical form"


def _tokens(text: str, drop_bangs: bool = False) -> str:
    """Whitespace-free token stream: `a∷b`, `a ∷ b` and `f (x)` / `f(x)`
    spell the same thing. String literals are one token and are never
    altered. `drop_bangs` removes strictness/laziness marks, which are not
    wire-relevant on a field type."""
    out = []
    for tok in _TOKEN_RE.findall(text):
        tok = _OPERATORS.get(tok, tok)
        if drop_bangs and tok in ("!", "~"):
            continue
        out.append(tok)
    return " ".join(out)


def unsupported(where: str, what: str, expected: str) -> "SchemaError":
    return SchemaError(f"{where}: {CANONICAL} ({what}; expected {expected})")


def _line(text: str, offset: int) -> int:
    return text.count("\n", 0, offset) + 1


def declaration_digest(decl: Declaration, params: str = "") -> str:
    """Digest of one declaration's wire-relevant text: kind, name, type
    parameters (their ORDER decides what an applied alias or newtype
    means), constructor/field text with comments, layout and strictness
    gone, and how `Generic`/`Serialize` are derived."""
    deriving = sorted(
        f"{strategy or '-'}:{cls}"
        for strategy, names in decl.deriving_classes.items()
        for cls in names if cls in _WIRE_CLASSES)
    return _sha(f"{decl.kind} {decl.name} "
                f"[{_tokens(params, True)}] = "
                f"{_tokens(decl.body, True)} | {','.join(deriving)}")


def declaration_params(root: Path, scan: Scan) -> dict[str, str]:
    """Qualified declaration -> the text between its name and its `=`
    (the type parameters), which the reused parser discards."""
    out: dict[str, str] = {}
    for module, rel in sorted(scan.module_paths.items()):
        text = strip_haskell_comments(
            (root / rel).read_text(encoding="utf-8"))
        for _line, block in declaration_blocks(text):
            head = split_top_level(block, "=")[0]
            m = re.match(r"(?:data|newtype|type)[ \t]+([A-Z][\w']*)(.*)",
                         head, re.S)
            if m:
                deriving = re.search(
                    r"(?<![\w'])deriving(?![\w'])", m.group(2))
                out[f"{module}.{m.group(1)}"] = (
                    m.group(2)[:deriving.start()] if deriving
                    else m.group(2))
    return out


@dataclass
class Instance:
    module: str
    head: str            # bare head type name, e.g. "HashMap"
    digest: str

    @property
    def key(self) -> str:
        return f"{self.module}:{self.head}"


_INSTANCE_HEAD_RE = re.compile(
    r"(?<![\w'])(?:[A-Z]\w*\.)*Serialize(?![\w'])\s*\(*\s*"
    r"(?:[A-Z]\w*\.)*([A-Z][\w']*)")


def discover_instances(root: Path, scan: Scan) -> list[Instance]:
    """Every hand-written `instance ... Serialize T` in src/ and app/."""
    found: list[Instance] = []
    for module, rel in sorted(scan.module_paths.items()):
        text = strip_haskell_comments(
            (root / rel).read_text(encoding="utf-8"))
        for block in top_level_blocks(text):
            if not block.startswith("instance"):
                continue
            flat = " ".join(block.split())
            head = re.match(r"instance\s+(.*?)(?:\swhere\b|$)", flat)
            if head is None:
                continue
            tail = re.split(r"⇒|=>", head.group(1))[-1]
            m = _INSTANCE_HEAD_RE.search(tail)
            if m is None:
                continue
            found.append(Instance(module, m.group(1), _sha(_tokens(flat))))
    return found


class Index:
    """Declaration lookup for one scanned tree."""

    def __init__(self, root: Path, scan: Scan) -> None:
        self.root = root
        self.scan = scan
        self.by_qualified = {d.qualified: d for d in scan.declarations}
        self.by_name: dict[str, list[Declaration]] = {}
        for d in scan.declarations:
            self.by_name.setdefault(d.name, []).append(d)
        self.params = declaration_params(root, scan)
        self.instances = discover_instances(root, scan)
        self.instances_by_head: dict[str, list[Instance]] = {}
        for inst in self.instances:
            self.instances_by_head.setdefault(inst.head, []).append(inst)
        self._check_serializer_ambiguity()

    def _check_serializer_ambiguity(self) -> None:
        for head, insts in sorted(self.instances_by_head.items()):
            keys = sorted({i.key for i in insts})
            if len(insts) > len(keys):
                raise SchemaError(
                    f"custom-serializer ambiguity: more than one "
                    f"`instance Serialize {head}` in the same module "
                    f"({', '.join(keys)})")
            for decl in self.by_name.get(head, ()):
                derived = any(
                    "Serialize" in names
                    for strategy, names in decl.deriving_classes.items()
                    if strategy != "newtype") or \
                    "Serialize" in decl.deriving_classes.get("newtype", ())
                if derived:
                    raise SchemaError(
                        f"custom-serializer ambiguity: {decl.qualified} "
                        f"both derives Serialize and has a hand-written "
                        f"instance ({', '.join(keys)}) -- say which one "
                        f"owns its wire shape")

    def digest(self, decl: Declaration) -> str:
        return declaration_digest(decl, self.params.get(decl.qualified, ""))

    def resolve(self, name: str, via: str) -> list[Declaration]:
        decls = self.by_name.get(name, [])
        if len(decls) > 1 and name not in AMBIGUOUS_TYPE_NAMES:
            raise SchemaError(
                f"ambiguous coverage: type name `{name}` reached via {via} "
                f"is declared in {', '.join(sorted(d.qualified for d in decls))}"
                f"; list it in AMBIGUOUS_TYPE_NAMES with why unioning them "
                f"is acceptable")
        return decls


@dataclass
class Closure:
    decls: dict[str, Declaration] = field(default_factory=dict)
    customs: dict[str, Instance] = field(default_factory=dict)
    paths: dict[str, str] = field(default_factory=dict)

    def merge(self, other: "Closure") -> None:
        self.decls.update(other.decls)
        self.customs.update(other.customs)
        self.paths.update(other.paths)


def closure_of(index: Index, seeds: list[Declaration],
               label: str) -> Closure:
    """Breadth-first closure over type references, with the hand-written
    instances of every reached type name."""
    out = Closure()
    queue: list[Declaration] = []
    for seed in sorted(seeds, key=lambda d: d.qualified):
        if seed.qualified not in out.decls:
            out.decls[seed.qualified] = seed
            out.paths[seed.qualified] = seed.name
            queue.append(seed)
    names_seen: set[str] = set()

    def reach_instances(name: str) -> None:
        if name in names_seen:
            return
        names_seen.add(name)
        for inst in index.instances_by_head.get(name, ()):
            out.customs[inst.key] = inst

    for seed in queue:
        reach_instances(seed.name)
    while queue:
        cur = queue.pop(0)
        for name in sorted(set(QUALIFIED_RE.findall(cur.body))):
            reach_instances(name)
            for decl in index.resolve(
                    name, f"{label}: {out.paths[cur.qualified]}"):
                if decl.qualified in out.decls:
                    continue
                out.decls[decl.qualified] = decl
                out.paths[decl.qualified] = (
                    out.paths[cur.qualified] + " → " + decl.name)
                queue.append(decl)
    return out


def _hashes(index: Index, closure: Closure) -> dict[str, str]:
    out = {q: index.digest(d) for q, d in closure.decls.items()}
    out.update({f"instance {k}": i.digest
                for k, i in closure.customs.items()})
    return out


def _shape(hashes: dict[str, str], extra: list[str] = ()) -> str:
    lines = [f"{k} {v}" for k, v in sorted(hashes.items())]
    lines += list(extra)
    return _sha("\n".join(lines))


def _carrier(index: Index, cid: str, kind: str, current: int,
             accepted: list[int], current_seeds: list[Declaration],
             legacy_seeds: list[Declaration], extra: list[str] = (),
             owner_sources: list[str] = (), required: bool = False
             ) -> Carrier:
    cur = closure_of(index, current_seeds, cid)
    old = closure_of(index, legacy_seeds, f"{cid} (legacy)")
    legacy_only = Closure(
        {q: d for q, d in old.decls.items() if q not in cur.decls},
        {k: i for k, i in old.customs.items() if k not in cur.customs})
    cur_hashes = _hashes(index, cur)
    old_hashes = _hashes(index, legacy_only)
    return Carrier(
        id=cid, kind=kind, current_version=current,
        accepted=sorted(accepted), shape=_shape(cur_hashes, extra),
        legacy_shape=_shape(old_hashes) if old_hashes else "",
        owners=sorted(cur.decls), legacy_owners=sorted(legacy_only.decls),
        custom_owners=sorted(cur.customs),
        owner_sources=sorted(owner_sources),
        decl_hashes=cur_hashes, legacy_hashes=old_hashes, required=required)


# ---------------------------------------------------------------------------
# Haskell components


def _wire_root_decls(index: Index) -> dict[str, list[Declaration]]:
    """`*DTO*`-named declarations in component modules, by bare name."""
    roots: dict[str, list[Declaration]] = {}
    for d in index.scan.declarations:
        if d.module.startswith(COMPONENT_MODULE_PREFIX) \
                and d.module != COMPONENT_TYPES_MODULE and "DTO" in d.name:
            roots.setdefault(d.name, []).append(d)
    return roots


def _field_value(block: str, field_name: str) -> tuple[str, str]:
    """(block with the field removed, the field's value text)."""
    m = re.search(rf"(?<![\w']){field_name}\s*=", block)
    if m is None:
        return block, ""
    start = m.start()
    depth, i = 0, m.end()
    while i < len(block):
        ch = block[i]
        if ch == '"':
            i += 1
            while i < len(block) and block[i] != '"':
                i += 2 if block[i] == "\\" else 1
        elif ch in "([{":
            depth += 1
        elif ch in ")]}":
            if depth == 0:
                break
            depth -= 1
        elif ch == "," and depth == 0:
            break
        i += 1
    return block[:start] + block[i:], block[m.end():i]


def component_codecs(index: Index) -> dict[str, dict]:
    """Component id -> {version, inputVersions, required, current/older
    seed declarations, owner source}, from every `componentCodec` record
    under the component directory, cross-checked against
    `saveComponentRegistry`."""
    root = index.root
    comp_dir = root / COMPONENT_DIR_REL
    paths = sorted(comp_dir.glob("*.hs"))
    id_literals: dict[str, str] = {}
    for path in paths:
        id_literals.update(dict(components.COMPONENT_ID_LITERAL_RE.findall(
            path.read_text(encoding="utf-8"))))
    roots = _wire_root_decls(index)
    result: dict[str, dict] = {}
    codec_names: set[str] = set()
    for path in paths:
        raw = path.read_text(encoding="utf-8")
        rel = path.relative_to(root).as_posix()
        specs = components.discover_component_specs(raw, rel)
        if not specs:
            continue
        text = strip_haskell_comments(raw)
        blocks = top_level_blocks(text)
        signatures = {}
        for block in blocks:
            sig = re.match(r"([a-z][A-Za-z0-9_']*)\s*(?:∷|::)", block)
            if sig:
                signatures[sig.group(1)] = block
        for spec in specs:
            sid = id_literals.get(spec["componentIdIdent"])
            if sid is None:
                raise SchemaError(
                    f"{rel}: codec `{spec['codec']}` names component id "
                    f"`{spec['componentIdIdent']}`, which no ComponentId "
                    f"literal defines")
            if sid in result:
                raise SchemaError(
                    f"ambiguous coverage: component `{sid}` is built by "
                    f"two codecs ({result[sid]['source']} and {rel})")
            codec_names.add(spec["codec"])
            block = next((b for b in blocks
                          if re.match(rf"{re.escape(spec['codec'])}\b", b)
                          and "componentCodec" in b), None)
            if block is None:
                raise SchemaError(
                    f"{rel}: cannot find codec `{spec['codec']}`'s block")
            current_text, older_text = _field_value(block, "csOlderVersions")

            def wire_types(names: set[str]) -> set[str]:
                return {n for n in names if n in roots}

            def helper_types(chunk: str) -> set[str]:
                found = wire_types(set(QUALIFIED_RE.findall(chunk)))
                for helper in set(re.findall(
                        r"(?<![A-Za-z0-9_'])([a-z][A-Za-z0-9_']*)", chunk)):
                    if helper in signatures and helper != spec["codec"]:
                        sig = signatures[helper]
                        parts = re.split(r"∷|::", sig, maxsplit=1)
                        if len(parts) == 2:
                            found |= wire_types(set(QUALIFIED_RE.findall(
                                parts[1].split("\n\n")[0])))
                        found |= wire_types(first_argument_types(sig))
                return found

            own_sig = signatures.get(spec["codec"], "")
            cur_names = helper_types(current_text + "\n" + own_sig)
            if not cur_names:
                raise SchemaError(
                    f"unknown coverage: component `{sid}` ({rel}, codec "
                    f"`{spec['codec']}`) names no *DTO* wire type this "
                    f"reader can resolve")
            old_names: set[str] = set()
            for entry in components._split_top_level_fields(
                    older_text.strip()[1:-1]):
                em = re.fullmatch(
                    r"atVersion\s+(\d+)\s+([a-z][\w']*)", entry.strip())
                where = f"{rel}:{_line(raw, raw.find(entry.strip()))}"
                if em is None:
                    raise unsupported(
                        where, f"component `{sid}` older-version entry "
                        f"{entry.strip()!r}",
                        "`atVersion <integer> <named decoder>`")
                helper = em.group(2)
                sig = signatures.get(helper)
                wired = (wire_types(first_argument_types(sig))
                         if sig else set())
                if not wired:
                    raise unsupported(
                        where, f"component `{sid}` v{em.group(1)} decoder "
                        f"`{helper}` has no explicit type signature naming "
                        f"a *DTO* wire root as its first argument",
                        f"`{helper} ∷ <FooDTOv{em.group(1)}> → ...`")
                old_names |= wired
            old_names -= cur_names

            def decls(names: set[str]) -> list[Declaration]:
                return [d for n in sorted(names) for d in roots[n]]

            result[sid] = {
                "version": spec["currentVersion"],
                "accepted": spec["inputVersions"],
                "required": spec["required"], "source": rel,
                "current": decls(cur_names),
                "legacy": decls(old_names)}
    registry = fingerprint._extract_toplevel_block(
        (root / COMPONENT_REGISTRY_REL).read_text(encoding="utf-8") + "\n\n",
        "saveComponentRegistry")
    registered = components.REGISTER_COMPONENT_RE.findall(registry)
    if not registered:
        raise SchemaError(
            f"unknown coverage: no registerComponent entries in "
            f"saveComponentRegistry ({COMPONENT_REGISTRY_REL})")
    missing = sorted(set(registered) - codec_names)
    if missing:
        raise SchemaError(
            f"unknown coverage: saveComponentRegistry registers "
            f"{', '.join(missing)}, but no `componentCodec ComponentSpec` "
            f"record of that name was found under {COMPONENT_DIR_REL}")
    unregistered = sorted(codec_names - set(registered))
    if unregistered:
        raise SchemaError(
            f"unknown coverage: codec(s) {', '.join(unregistered)} are "
            f"built under {COMPONENT_DIR_REL} but not registered in "
            f"saveComponentRegistry")
    return result


# ---------------------------------------------------------------------------
# Lua modules


_LUA_STRING_RE = re.compile(
    r'"(?:\\.|[^"\\\n])*"|\'(?:\\.|[^\'\\\n])*\'')
_LUA_LONG_OPEN_RE = re.compile(r"\[(=*)\[")


def blank_lua_comments(text: str) -> str:
    """The text with every comment replaced by spaces (newlines kept), so
    offsets and line numbers still point at the real file. String
    literals, including long strings, are left alone."""
    out: list[str] = []
    i, n = 0, len(text)

    def blank(chunk: str) -> str:
        return "".join("\n" if c == "\n" else " " for c in chunk)

    while i < n:
        if text.startswith("--", i):
            long = _LUA_LONG_OPEN_RE.match(text, i + 2)
            if long:
                close = "]" + long.group(1) + "]"
                end = text.find(close, long.end())
                end = n if end < 0 else end + len(close)
            else:
                end = text.find("\n", i)
                end = n if end < 0 else end
            out.append(blank(text[i:end]))
            i = end
            continue
        m = _LUA_STRING_RE.match(text, i)
        if m:
            out.append(m.group(0))
            i = m.end()
            continue
        long = _LUA_LONG_OPEN_RE.match(text, i)
        if long:
            close = "]" + long.group(1) + "]"
            end = text.find(close, long.end())
            end = n if end < 0 else end + len(close)
            out.append(text[i:end])
            i = end
            continue
        out.append(text[i])
        i += 1
    return "".join(out)


_LUA_TOKEN_RE = re.compile(
    r'"(?:\\.|[^"\\\n])*"|\'(?:\\.|[^\'\\\n])*\'|\[(=*)\[.*?\]\1\]'
    r"|[A-Za-z_]\w*|\d+(?:\.\d+)?|\S", re.S)


def lua_tokens(text: str) -> str:
    """Comment- and whitespace-free Lua token stream; string contents are
    never altered."""
    return " ".join(m.group(0) for m in
                    _LUA_TOKEN_RE.finditer(blank_lua_comments(text)))


def _lua_table_span(blanked: str, open_index: int) -> tuple[str, str, int]:
    """(depth-1 text, depth-<=2 text, offset of the closing brace) of the
    table opened at `open_index` in comment-blanked text; deeper content
    is blanked."""
    flat: list[str] = []
    wide: list[str] = []
    depth, i, n = 0, open_index, len(blanked)
    while i < n:
        m = _LUA_STRING_RE.match(blanked, i)
        if m:
            chunk = m.group(0)
            flat.append(chunk if depth <= 1 else " " * len(chunk))
            wide.append(chunk if depth <= 2 else " " * len(chunk))
            i = m.end()
            continue
        ch = blanked[i]
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return "".join(flat), "".join(wide), i
        flat.append(ch if depth <= 1 or ch == "\n" else " ")
        wide.append(ch if depth <= 2 or ch == "\n" else " ")
        i += 1
    raise SchemaError("unterminated saveMods.register table")


_LUA_REQUIRE_RE = re.compile(
    r"""require\s*\(?\s*["']scripts\.lib\.save_modules["']\s*\)?""")
_LUA_BIND_RE = re.compile(
    r"^[ \t]*local\s+[A-Za-z_]\w*\s*=\s*require\s*\(\s*"
    r'"scripts\.lib\.save_modules"\s*\)[ \t]*$', re.M)
_LUA_ALIAS_RE = re.compile(
    r"([A-Za-z_]\w*)\s*=\s*require\s*\(\s*[\"']scripts\.lib\.save_modules")
_LUA_REGISTER_USE_RE = re.compile(
    r"""(?P<recv>[A-Za-z_][\w.]*|\))\s*"""
    r"""(?:[.:]\s*register(?![\w])|\[\s*["']register["']\s*\])""")
_LUA_REGISTER_CANON_RE = re.compile(
    r'(?<![\w.])saveMods\s*\.\s*register\s*\(\s*"([A-Za-z_]\w*)"\s*,\s*\{')


def discover_lua_registrations(root: Path) -> list[tuple[Path, str, int]]:
    """Every registration of the Lua save-module registry under
    scripts/, as (file, id, offset of its table's `{`).

    The one canonical form is `saveMods.register("<id>", {` (whitespace
    is layout) after `local <name> = require("scripts.lib.save_modules")`
    on its own line. Any other use of THAT registry's `register` --
    called on another alias or on the require expression, single quotes,
    a comment inside the call head, an alias of the function or of the
    registry object, an indexed lookup, a dynamic id -- is refused,
    never interpreted, so a mutation cannot make a carrier vanish. A
    `register` on some unrelated table is not the registry and is not
    read."""
    sites: list[tuple[Path, str, int]] = []
    for path in sorted((root / SCRIPTS_REL).rglob("*.lua")):
        raw = path.read_text(encoding="utf-8")
        text = blank_lua_comments(raw)
        if "save_modules" not in text:
            continue
        rel = path.relative_to(root).as_posix()
        for m in _LUA_REQUIRE_RE.finditer(text):
            start = text.rfind("\n", 0, m.start()) + 1
            end = text.find("\n", m.end())
            line = text[start:end if end >= 0 else len(text)]
            if not _LUA_BIND_RE.fullmatch(line):
                raise unsupported(
                    f"{rel}:{_line(text, m.start())}",
                    "the save-module registry is required in another form",
                    '`local <name> = require("scripts.lib.save_modules")` '
                    "alone on its line")
        aliases = {m.group(1) for m in _LUA_ALIAS_RE.finditer(text)}
        for m in re.finditer(r"(?<![\w.])saveMods(?![\w])(?!\s*\.\s*[A-Za-z_])",
                             text):
            line = text[text.rfind("\n", 0, m.start()) + 1:m.end()]
            if not re.fullmatch(r"\s*local\s+saveMods\s*", line.split("=")[0]
                                if "=" in line else line):
                raise unsupported(
                    f"{rel}:{_line(text, m.start())}",
                    "the registry object is used other than as "
                    "`saveMods.<name>`", "`saveMods.<name>(...)` only")
        for use in _LUA_REGISTER_USE_RE.finditer(text):
            recv = use.group("recv")
            if recv != ")" and recv not in aliases:
                continue
            where = f"{rel}:{_line(text, use.start())}"
            m = _LUA_REGISTER_CANON_RE.search(text, use.start())
            if recv != "saveMods" or m is None or m.start() != use.start() \
                    or raw[m.start():m.end()] != text[m.start():m.end()]:
                if recv == ")" and not _LUA_REQUIRE_RE.search(
                        text[max(0, use.start() - 80):use.start() + 1]):
                    continue
                raise unsupported(
                    where, "a use of the registry's register that is not "
                    "the canonical registration call",
                    '`saveMods.register("<id>", {` with no comment inside '
                    "the call head")
            sites.append((path, m.group(1), m.end() - 1))
    return sites


def lua_modules(root: Path) -> dict[str, dict]:
    found = discover_lua_registrations(root)
    if not found:
        raise SchemaError(
            f"unknown coverage: no saveMods.register call sites under "
            f"{SCRIPTS_REL}")
    seen: dict[str, Path] = {}
    per_file: dict[Path, int] = {}
    for path, lua_id, _ in found:
        if lua_id in seen:
            raise SchemaError(
                f"ambiguous coverage: Lua save module `{lua_id}` is "
                f"registered in both {seen[lua_id].relative_to(root)} and "
                f"{path.relative_to(root)}")
        seen[lua_id] = path
        per_file[path] = per_file.get(path, 0) + 1
    if not (root / DATA_CODEC_REL).is_file():
        raise SchemaError(
            f"unknown coverage: shared Lua save codec {DATA_CODEC_REL} "
            f"is missing")
    modules: dict[str, dict] = {}
    for path, lua_id, brace in found:
        rel = path.relative_to(root).as_posix()
        if per_file[path] > 1:
            raise SchemaError(
                f"ambiguous coverage: {rel} registers {per_file[path]} Lua "
                f"save modules; one registration per file is required so "
                f"each module's owning source is unambiguous")
        raw = path.read_text(encoding="utf-8")
        text = blank_lua_comments(raw)
        flat, wide, _end = _lua_table_span(text, brace)
        where = f"{rel}:{_line(text, brace)}"
        if len(re.findall(r"(?<![\w.])inputVersions(?![\w])", wide)) != 1:
            raise unsupported(where, f"lua.{lua_id} names inputVersions "
                              "other than exactly once",
                              "one `inputVersions = { <integers> },`")
        fields = {}
        for name, pattern in (
                ("version", r"(?<![\w.])version\s*=\s*(\d+)\s*,"),
                ("inputVersions", r"(?<![\w.])inputVersions\s*=\s*"
                 r"\{([^{}]*)\}\s*,"),
                ("required", r"(?<![\w.])required\s*=\s*(true|false)\s*,")):
            haystack = wide if name == "inputVersions" else flat
            hits = list(re.finditer(pattern, haystack))
            loose = re.findall(rf"(?<![\w.]){name}\s*=(?!=)", haystack)
            if len(hits) != 1 or len(loose) != 1:
                raise unsupported(
                    where, f"lua.{lua_id}'s `{name}` is not exactly one "
                    f"literal field", f"one `{name} = <literal>,`")
            if name == "inputVersions":
                start = brace + hits[0].start(1)
                if raw[start:start + len(hits[0].group(1))] \
                        != text[start:start + len(hits[0].group(1))]:
                    raise unsupported(where, "a comment inside inputVersions",
                                      "a bare `{ 1, 2, 3 }`")
            fields[name] = hits[0].group(1)
        entries = [e.strip() for e in fields["inputVersions"].split(",")
                   if e.strip()]
        if not entries or any(not re.fullmatch(r"\d+", e) for e in entries):
            raise unsupported(
                where, f"lua.{lua_id}'s inputVersions entries {entries}",
                "integer literals only")
        modules[lua_id] = {
            "version": int(fields["version"]),
            "accepted": sorted(int(e) for e in entries),
            "required": fields["required"] == "true", "source": rel}
    return modules


# ---------------------------------------------------------------------------
# Model assembly


def _binding_int(text: str, name: str, where: str) -> int:
    """A version binding is ONE definition `name = <integer literal>`;
    an expression, a second definition or a trailing operator is not
    parsed, it is refused."""
    clean = strip_haskell_comments(text)
    defs = []
    for m in re.finditer(rf"^{re.escape(name)}(?![\w'])(.*)$", clean, re.M):
        rest = m.group(1)
        if re.match(r"\s*(?:∷|::)", rest) or not rest.strip():
            continue
        defs.append((_line(clean, m.start()), rest))
    if not defs:
        raise SchemaError(
            f"unknown coverage: could not find `{name}` in {where}")
    if len(defs) > 1:
        raise unsupported(f"{where}:{defs[1][0]}",
                          f"`{name}` is defined more than once",
                          f"a single `{name} = <integer>`")
    line, rest = defs[0]
    m = re.fullmatch(r"\s*=\s*(\d+)\s*", rest)
    if m is None:
        raise unsupported(f"{where}:{line}", f"`{name}`{rest.rstrip()}",
                          f"`{name} = <integer literal>` alone on its line")
    return int(m.group(1))


def _check_metadata_input_list(text: str, where: str) -> None:
    blocks = [_tokens(b) for b in top_level_blocks(strip_haskell_comments(text))
              if re.match(r"metadataComponentInputVersions(?![\w'])", b)]
    defs = [b for b in blocks if "∷" not in b]
    if len(defs) != 1 or not re.fullmatch(
            r"metadataComponentInputVersions = \[ [\w']+(?: , [\w']+)* \]",
            defs[0]):
        raise unsupported(
            where, f"metadataComponentInputVersions is {defs or 'missing'}",
            "one definition: `metadataComponentInputVersions = "
            "[ <binding or integer>, ... ]`")


def dispatcher_forms(entry: str, b1: str, b2: str
                     ) -> dict[str, tuple[bool, bool]]:
    """The exact token streams a dispatcher may take, mapped to
    (b1-session enabled, b2-lua-state enabled)."""
    forms = {}
    for b1_on in (True, False):
        for b2_on in (True, False):
            tail = f"{b2} bytes" if b2_on else "NotLegacyShaped"
            rhs = (f"case {b1} bytes of NotLegacyShaped → {tail} other → other"
                   if b1_on else tail)
            param = "bytes" if (b1_on or b2_on) else "_"
            forms[f"{entry} {param} = {rhs}"] = (b1_on, b2_on)
    return forms


def route_states(text: str) -> tuple[bool, bool]:
    clean = strip_haskell_comments(text)
    states = set()
    for entry, b1, b2 in DISPATCHERS:
        blocks = [(_tokens(b), b) for b in top_level_blocks(clean)
                  if re.match(rf"{re.escape(entry)}(?![\w'])", b)]
        defs = [t for t, _ in blocks if "∷" not in t]
        if not blocks:
            raise SchemaError(
                f"unknown coverage: legacy-route dispatcher `{entry}` not "
                f"found in {ENVELOPE_REL}")
        forms = dispatcher_forms(entry, b1, b2)
        if len(defs) != 1 or defs[0] not in forms:
            at = _line(clean, clean.find(entry + " "))
            raise unsupported(
                f"{ENVELOPE_REL}:{at}",
                f"dispatcher `{entry}` is {defs[0] if defs else 'missing'!r}",
                "one of: " + " | ".join(sorted(forms)))
        states.add(forms[defs[0]])
    if len(states) != 1:
        raise unsupported(
            ENVELOPE_REL, "the envelope and metadata dispatchers disagree "
            "on which legacy routes are enabled",
            "both dispatchers in the same on/off form")
    return next(iter(states))


_FRAMING_GUARD = ("when ( version ≢ expectedVersion ) "
                  "( Left ( UnsupportedEnvelopeVersion version ) )")
_DECODE_ENVELOPE_RE = re.compile(
    r"(?<![\w'])(?:[A-Z][\w']*\.)*decodeEnvelope(?![\w'])")
_IMPORT_RE = re.compile(r"^import\b.*(?:\n[ \t]+.*)*", re.M)
_MODULE_HEAD_RE = re.compile(r"^module\b.*?\bwhere\b", re.M | re.S)
CANONICAL_DECODE = "decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion"


def _check_framing_dispatch(root: Path, scan: Scan) -> None:
    """The framing carrier accepts exactly `currentEnvelopeVersion` only
    if (1) the decoder rejects every version but the one it is handed,
    with no other disjunct, and (2) every use of the decoder is the one
    canonical call `decodeEnvelope defaultEnvelopeLimits
    currentEnvelopeVersion`. Parenthesised, qualified, renamed or
    partially applied uses are not interpreted; they are refused."""
    codec = _tokens(strip_haskell_comments(
        (root / ENVELOPE_CODEC_REL).read_text(encoding="utf-8")))
    if codec.count(_FRAMING_GUARD) != 1 \
            or codec.count("UnsupportedEnvelopeVersion") != 1:
        raise unsupported(
            ENVELOPE_CODEC_REL, "the decoder's version rejection",
            f"exactly one `{_FRAMING_GUARD}` and no other "
            f"UnsupportedEnvelopeVersion")
    calls = 0
    for module, rel in sorted(scan.module_paths.items()):
        if rel == ENVELOPE_CODEC_REL:
            continue
        text = strip_haskell_comments((root / rel).read_text(encoding="utf-8"))
        for imp in _IMPORT_RE.finditer(text):
            if "decodeEnvelope" in imp.group(0) and re.search(
                    r"\b(?:as|qualified|hiding)\b", imp.group(0)):
                raise unsupported(
                    f"{rel}:{_line(text, imp.start())}",
                    "decodeEnvelope is imported renamed, qualified or "
                    "hidden", "a plain unqualified import")
        body = _MODULE_HEAD_RE.sub(lambda m: re.sub(r"\S", " ", m.group(0))
                                   if "decodeEnvelope" not in m.group(0)
                                   else m.group(0), text)
        body = _IMPORT_RE.sub(lambda m: re.sub(r"[^\n]", " ", m.group(0)),
                              body)
        for use in _DECODE_ENVELOPE_RE.finditer(body):
            line = _line(body, use.start())
            following = _tokens(body[use.start():use.start() + 400])
            if not following.startswith(CANONICAL_DECODE) \
                    or use.group(0) != "decodeEnvelope":
                raise unsupported(
                    f"{rel}:{line}", f"use of decodeEnvelope "
                    f"{following[:90]!r}", f"`{CANONICAL_DECODE}`")
            calls += 1
    if not calls:
        raise SchemaError(
            "unknown coverage: no decodeEnvelope call site found, so the "
            "framing carrier's accepted version cannot be established")


def _strip_block_comments_keep_pragmas(text: str) -> str:
    out, i, depth = [], 0, 0
    while i < len(text):
        if text.startswith("{-#", i) and depth == 0:
            end = text.find("#-}", i)
            end = len(text) if end < 0 else end + 3
            out.append(text[i:end])
            i = end
        elif text.startswith("{-", i):
            depth += 1
            i += 2
        elif depth and text.startswith("-}", i):
            depth -= 1
            i += 2
        elif depth:
            out.append("\n" if text[i] == "\n" else " ")
            i += 1
        else:
            out.append(text[i])
            i += 1
    return "".join(out)


def framing_digest(root: Path) -> str:
    """The framing sources -- the wire bindings of Envelope/Types.hs and
    the whole of Codec.hs -- as a comment- and whitespace-insensitive
    token stream. The inherited-LANGUAGE rule is reused from
    `envelope_framing_fingerprint` (whose own value is left alone: the
    manifest records it)."""
    types_text = (root / ENVELOPE_TYPES_REL).read_text(encoding="utf-8")
    blocks = [fingerprint._extract_toplevel_block(types_text, name)
              for name in fingerprint.ENVELOPE_FRAMING_WIRE_BINDINGS]
    codec = fingerprint._drop_redundant_language_pragmas(
        fingerprint._strip_line_comments(
            (root / ENVELOPE_CODEC_REL).read_text(encoding="utf-8")),
        fingerprint.inherited_default_extensions(root / CABAL_REL))
    parts = [_tokens(_strip_block_comments_keep_pragmas(
        fingerprint._strip_line_comments(b))) for b in blocks]
    parts.append(_tokens(_strip_block_comments_keep_pragmas(codec)))
    return _sha("\n---\n".join(parts))


def build_model(root: Path) -> Model:
    root = Path(root)
    try:
        return _build_model(root)
    except (AuditError, ValueError) as exc:
        raise SchemaError(f"{CANONICAL}: {exc}") from exc


def _build_model(root: Path) -> Model:
    scan = scan_repository(root)
    index = Index(root, scan)
    carriers: dict[str, Carrier] = {}

    def need_module(module: str) -> None:
        if module not in scan.module_paths:
            raise SchemaError(
                f"unknown coverage: module `{module}` named by the "
                f"current-schema ownership map does not exist")

    # Haskell gameplay components.
    for cid, info in sorted(component_codecs(index).items()):
        carriers[cid] = _carrier(
            index, cid, "component", info["version"], info["accepted"],
            info["current"], info["legacy"], owner_sources=[info["source"]],
            required=info["required"])

    # Metadata.
    envelope_text = (root / ENVELOPE_REL).read_text(encoding="utf-8")
    meta_current = _binding_int(
        envelope_text, "metadataComponentVersion", ENVELOPE_REL)
    _check_metadata_input_list(envelope_text, ENVELOPE_REL)
    _binding_int(envelope_text, "legacyMetadataComponentVersion",
                 ENVELOPE_REL)
    meta_accepted = fingerprint.metadata_input_versions(
        envelope_text, meta_current)
    meta_root = index.by_qualified.get(METADATA_ROOT)
    if meta_root is None:
        raise SchemaError(
            f"unknown coverage: metadata root type {METADATA_ROOT} missing")
    meta_legacy: list[Declaration] = []
    for version in meta_accepted:
        if version == meta_current:
            continue
        module = METADATA_FROZEN_MODULE.format(n=version)
        need_module(module)
        meta_legacy += [d for d in scan.declarations if d.module == module
                        and "Serialize" in {c for cs in
                                            d.deriving_classes.values()
                                            for c in cs}]
    carriers["metadata"] = _carrier(
        index, "metadata", "metadata", meta_current, meta_accepted,
        [meta_root], meta_legacy, owner_sources=[ENVELOPE_REL])

    # Lua modules: no declared payload schema, so they are
    # custom-serializer carriers owned by their registering file's
    # registration plus the shared data codec.
    lua_infos = lua_modules(root)
    codec_digest = _sha(lua_tokens(
        (root / DATA_CODEC_REL).read_text(encoding="utf-8")))
    for lua_id, info in sorted(lua_infos.items()):
        carriers[f"lua.{lua_id}"] = Carrier(
            id=f"lua.{lua_id}", kind="lua", current_version=info["version"],
            accepted=info["accepted"],
            shape=_sha(f"lua-codec {codec_digest}"), legacy_shape="",
            owners=[], legacy_owners=[], custom_owners=[],
            owner_sources=sorted([info["source"], DATA_CODEC_REL]),
            required=info["required"])

    # Framing.
    framing = framing_digest(root)
    framing_version = _binding_int(
        envelope_text, "currentEnvelopeVersion", ENVELOPE_REL)
    _check_framing_dispatch(root, scan)
    carriers["framing"] = Carrier(
        id="framing", kind="framing", current_version=framing_version,
        accepted=[framing_version], shape=framing, legacy_shape="",
        owners=[], legacy_owners=[], custom_owners=[],
        owner_sources=[ENVELOPE_TYPES_REL, ENVELOPE_CODEC_REL])

    # Retired carriers read only through legacy routes.
    session_module = SESSION_V90_MODULE
    need_module(session_module)
    session_seeds = [
        d for d in scan.declarations if d.module == session_module
        and d.kind == "data" and "Serialize" in {
            c for cs in d.deriving_classes.values() for c in cs}]
    if not session_seeds:
        raise SchemaError(
            f"unknown coverage: {session_module} declares no Serialize "
            f"DTO for the B1 session route")
    session_text = (root / scan.module_paths[session_module]).read_text(
        encoding="utf-8")
    carriers["session"] = _carrier(
        index, "session", "legacy",
        _binding_int(session_text, "sessionComponentVersion",
                     session_module), [], session_seeds, [],
        owner_sources=[scan.module_paths[session_module]])
    carriers["session"].accepted = [carriers["session"].current_version]
    lua_state_version = _binding_int(
        envelope_text, "luaStateComponentVersion", ENVELOPE_REL)
    carriers["lua-state"] = Carrier(
        id="lua-state", kind="legacy", current_version=lua_state_version,
        accepted=[lua_state_version],
        shape=_sha(f"lua-state {lua_state_version}"), legacy_shape="",
        owners=[], legacy_owners=[], custom_owners=[],
        owner_sources=[ENVELOPE_REL])

    # Legacy routes, read from the dispatchers' exact forms.
    b1_on, b2_on = route_states(envelope_text)
    states = {"b1-session": b1_on, "b2-lua-state": b2_on}
    routes: dict[str, Route] = {}
    for rid, spec in sorted(LEGACY_ROUTES.items()):
        carrier_ids = list(spec["carriers"])
        if spec.get("required_components"):
            carrier_ids += sorted(c.id for c in carriers.values()
                                  if c.kind == "component" and c.required)
        routes[rid] = Route(rid, states[rid], carrier_ids)
    return Model(carriers, routes)
