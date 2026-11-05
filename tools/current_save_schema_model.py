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
LEGACY_ROUTES = {
    "b1-session": {
        "entries": ("tryLegacyEnvelopeFallbacks",
                    "tryLegacyMetadataFallbacks"),
        "callees": ("decodeLegacySessionEnvelope",
                    "decodeLegacySessionMetadata"),
        "carriers": ("metadata", "session"),
    },
    "b2-lua-state": {
        "entries": ("tryLegacyEnvelopeFallbacks",
                    "tryLegacyMetadataFallbacks"),
        "callees": ("decodeB2SessionEnvelope", "decodeB2SessionMetadata"),
        "carriers": ("metadata", "lua-state"),
    },
}


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


_BANG_RE = re.compile(r"(?<![\w'])[!~](?=[\w(\[])")
_WIRE_CLASSES = ("Generic", "Serialize")


def declaration_digest(decl: Declaration) -> str:
    """Digest of one declaration's wire-relevant text: kind, name,
    constructor/field text with comments, layout and strictness gone,
    and how `Generic`/`Serialize` are derived."""
    body = " ".join(decl.body.split())
    body = _BANG_RE.sub("", body)
    body = body.replace("::", "∷").replace("->", "→")
    deriving = sorted(
        f"{strategy or '-'}:{cls}"
        for strategy, names in decl.deriving_classes.items()
        for cls in names if cls in _WIRE_CLASSES)
    return _sha(f"{decl.kind} {decl.name} = {body} | {','.join(deriving)}")


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
            found.append(Instance(module, m.group(1),
                                  _sha(flat.replace("::", "∷"))))
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


def _shape(closure: Closure, extra: list[str] = ()) -> str:
    lines = [f"decl {q} {declaration_digest(d)}"
             for q, d in sorted(closure.decls.items())]
    lines += [f"inst {k} {i.digest}"
              for k, i in sorted(closure.customs.items())]
    lines += list(extra)
    return _sha("\n".join(lines))


def _carrier(index: Index, cid: str, kind: str, current: int,
             accepted: list[int], current_seeds: list[Declaration],
             legacy_seeds: list[Declaration], extra: list[str] = (),
             owner_sources: list[str] = ()) -> Carrier:
    cur = closure_of(index, current_seeds, cid)
    old = closure_of(index, legacy_seeds, f"{cid} (legacy)")
    legacy_only = Closure(
        {q: d for q, d in old.decls.items() if q not in cur.decls},
        {k: i for k, i in old.customs.items() if k not in cur.customs})
    return Carrier(
        id=cid, kind=kind, current_version=current,
        accepted=sorted(accepted), shape=_shape(cur, extra),
        legacy_shape=_shape(legacy_only) if legacy_only.decls
        or legacy_only.customs else "",
        owners=sorted(cur.decls), legacy_owners=sorted(legacy_only.decls),
        custom_owners=sorted(cur.customs),
        owner_sources=sorted(owner_sources),
        decl_hashes={q: declaration_digest(d)
                     for q, d in sorted(cur.decls.items())})


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
            old_names = helper_types(older_text) - cur_names
            if not cur_names:
                raise SchemaError(
                    f"unknown coverage: component `{sid}` ({rel}, codec "
                    f"`{spec['codec']}`) names no *DTO* wire type this "
                    f"reader can resolve")
            if spec["inputVersions"] != [spec["currentVersion"]] \
                    and not old_names and older_text.strip() not in ("", "[]"):
                raise SchemaError(
                    f"unknown coverage: component `{sid}` ({rel}) accepts "
                    f"older versions but its migrations name no frozen "
                    f"*DTO* type this reader can resolve")

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


def _lua_table_text(text: str, open_index: int) -> tuple[str, str]:
    """(depth-1 text, depth-<=2 text) of the table opened at
    `open_index`: comments dropped, string literals kept, and anything
    nested deeper blanked. The depth-1 text is where `version` and
    `required` must be declared; the depth-<=2 text additionally shows
    the `inputVersions = { ... }` list."""
    flat: list[str] = []
    wide: list[str] = []
    depth, i, n = 0, open_index, len(text)
    while i < n:
        ch = text[i]
        if text.startswith("--", i):
            if text.startswith("--[[", i):
                end = text.find("]]", i)
                i = n if end < 0 else end + 2
            else:
                end = text.find("\n", i)
                i = n if end < 0 else end
            continue
        if ch in "\"'":
            quote, j = ch, i + 1
            while j < n and text[j] != quote:
                j += 2 if text[j] == "\\" else 1
            if depth == 1:
                flat.append(text[i:j + 1])
            if depth <= 2:
                wide.append(text[i:j + 1])
            i = j + 1
            continue
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return "".join(flat), "".join(wide)
        if depth <= 1:
            flat.append(ch)
        if depth <= 2:
            wide.append(ch)
        i += 1
    raise SchemaError("unterminated saveMods.register table")


def lua_modules(root: Path) -> dict[str, dict]:
    scripts = root / SCRIPTS_REL
    found = components.discover_lua_save_modules(scripts)
    if not found:
        raise SchemaError(
            f"unknown coverage: no saveMods.register call sites under "
            f"{SCRIPTS_REL}")
    seen: dict[str, Path] = {}
    per_file: dict[Path, int] = {}
    for path, lua_id in found:
        if lua_id in seen:
            raise SchemaError(
                f"ambiguous coverage: Lua save module `{lua_id}` is "
                f"registered in both {seen[lua_id].relative_to(root)} and "
                f"{path.relative_to(root)}")
        seen[lua_id] = path
        per_file[path] = per_file.get(path, 0) + 1
    codec = root / DATA_CODEC_REL
    if not codec.is_file():
        raise SchemaError(
            f"unknown coverage: shared Lua save codec {DATA_CODEC_REL} "
            f"is missing")
    modules: dict[str, dict] = {}
    for lua_id, path in seen.items():
        rel = path.relative_to(root).as_posix()
        if per_file[path] > 1:
            raise SchemaError(
                f"ambiguous coverage: {rel} registers {per_file[path]} Lua "
                f"save modules; one registration per file is required so "
                f"each module's owning source is unambiguous")
        text = path.read_text(encoding="utf-8")
        m = re.search(
            rf'saveMods\.register\(\s*"{re.escape(lua_id)}"\s*,\s*\{{', text)
        if m is None:
            raise SchemaError(
                f"{rel}: cannot read the saveMods.register(\"{lua_id}\", "
                f"{{...}}) table")
        table, wide = _lua_table_text(text, m.end() - 1)
        fields = {}
        for name, pattern in (
                ("version", r"(?<![\w.])version\s*=\s*(\d+)\s*,"),
                ("inputVersions",
                 r"(?<![\w.])inputVersions\s*=\s*\{([^}]*)\}"),
                ("required", r"(?<![\w.])required\s*=\s*(true|false)")):
            fm = re.search(pattern, wide if name == "inputVersions"
                           else table)
            if fm is None:
                raise SchemaError(
                    f"{rel}: lua.{lua_id} declares no literal `{name}` "
                    f"in its saveMods.register table")
            fields[name] = fm.group(1)
        modules[lua_id] = {
            "version": int(fields["version"]),
            "accepted": sorted(int(v) for v in
                               re.findall(r"\d+", fields["inputVersions"])),
            "required": fields["required"] == "true", "source": rel}
    return modules


def _lua_normalized(path: Path) -> str:
    text = path.read_text(encoding="utf-8")
    text = re.sub(r"--\[\[.*?\]\]", "", text, flags=re.S)
    text = re.sub(r"--[^\n]*", "", text)
    return " ".join(text.split())


# ---------------------------------------------------------------------------
# Model assembly


def _binding_int(text: str, name: str, where: str) -> int:
    m = re.search(rf"^{name}\s*=\s*(\d+)\s*$", text, re.M)
    if m is None:
        raise SchemaError(
            f"unknown coverage: could not read `{name}` in {where}")
    return int(m.group(1))


def _dispatcher_body(text: str, name: str) -> str:
    blocks = [b for b in top_level_blocks(strip_haskell_comments(text))
              if re.match(rf"{re.escape(name)}(?![\w'])", b)]
    if not blocks:
        raise SchemaError(
            f"unknown coverage: legacy-route dispatcher `{name}` not found "
            f"in {ENVELOPE_REL}")
    return "\n".join(blocks)


def build_model(root: Path) -> Model:
    root = Path(root)
    try:
        return _build_model(root)
    except (AuditError, ValueError) as exc:
        raise SchemaError(f"unclassifiable source: {exc}") from exc


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
            info["current"], info["legacy"], owner_sources=[info["source"]])

    # Metadata.
    envelope_text = (root / ENVELOPE_REL).read_text(encoding="utf-8")
    meta_current = _binding_int(
        envelope_text, "metadataComponentVersion", ENVELOPE_REL)
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
    codec_digest = _sha(_lua_normalized(root / DATA_CODEC_REL))
    for lua_id, info in sorted(lua_infos.items()):
        carriers[f"lua.{lua_id}"] = Carrier(
            id=f"lua.{lua_id}", kind="lua", current_version=info["version"],
            accepted=info["accepted"],
            shape=_sha(f"lua-codec {codec_digest}"), legacy_shape="",
            owners=[], legacy_owners=[], custom_owners=[],
            owner_sources=sorted([info["source"], DATA_CODEC_REL]))

    # Framing.
    framing = fingerprint.envelope_framing_fingerprint(
        root / ENVELOPE_TYPES_REL, root / ENVELOPE_CODEC_REL,
        root / CABAL_REL)
    framing_version = fingerprint.current_envelope_version(
        root / ENVELOPE_REL)
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

    # Legacy routes.
    routes: dict[str, Route] = {}
    for rid, spec in sorted(LEGACY_ROUTES.items()):
        bodies = "\n".join(_dispatcher_body(envelope_text, e)
                           for e in spec["entries"])
        enabled = any(re.search(rf"(?<![\w']){c}(?![\w'])", bodies)
                      for c in spec["callees"])
        routes[rid] = Route(rid, enabled, list(spec["carriers"]))
    return Model(carriers, routes)
