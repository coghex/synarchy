"""Record validation and base-versus-candidate rules for
tools/current_save_schema_audit.py (issue #2916).

Both sides are records produced by the COMPILED extractor (the
"current save schema" hspec group): shapes come from GHC Generic/Typeable
walks, versions from the live registries. This module reads no source
text. It only compares two such records, and checks that a record is
structurally trustworthy.

Per carrier (a Haskell component, the metadata component, the framing, a
Lua module, or a retired legacy carrier):

  shape changed, version not strictly higher   -> same-version drift
  version lower than the base's                -> downgrade
  AFFECTED (shape changed or version higher)   -> accepts exactly its
                                                  current version, and
                                                  every enabled legacy
                                                  route it owns is off
  new carrier                                  -> covered, accepts only
                                                  its current version
  carrier gone                                 -> reported; its record
                                                  entry must be gone too

Unchanged carriers owe nothing: removing one old registration alone never
demands retiring another carrier's. No historical decoder is analysed;
declarations reachable only through a still-accepted older decoder or an
enabled legacy route are NOT recorded -- a limit the design accepts
(D-5/D-7), stated in docs/current_save_schema_guard.md, never claimed
as covered.
"""
from __future__ import annotations

import json
from collections import deque

RECORD_FORMAT = 1
EXTRACTOR = "current-save-schema/1"
TOP_KEYS = ("format", "extractor", "types", "carriers", "routes", "extras")
CARRIER_KEYS = ("class", "version", "accepted", "required", "root",
                "reaches", "owners", "detail")
CLASSES = ("component", "metadata", "framing", "lua-module", "legacy")
ENTRY_KEYS = ("directFullSession", "directMetadataOnly",
              "publicFullSession", "publicMetadataOnly")
ENTRY_POINTS = ("decodeSaveEnvelopeMetadata",
                "decodeSaveEnvelopeMetadataClassified",
                "decodeSessionEnvelope", "decodeSessionEnvelopeClassified")
FRAMING_KEYS = ("advertised", "currentEnvelopeVersion", "envelopeMagic",
                "fnv1a64Probe", "headerSize", "layoutProbeHex")
SHAPE_KEYS = ("kind", "detail", "dataType", "newtype", "constructors", "args")
KINDS = ("generic", "primitive", "library", "newtype", "custom")
# The one place a record may carry human prose: ignored when comparing.
DESCRIPTIVE_DETAIL_KEYS = ("contract",)


class RecordError(Exception):
    """A record that cannot be trusted as evidence."""


def render_record(record: dict) -> str:
    return json.dumps(record, indent=2, sort_keys=True) + "\n"


def parse_record(text: str, where: str) -> dict:
    try:
        record = json.loads(text)
    except ValueError as exc:
        raise RecordError(f"{where} is not valid JSON: {exc}") from exc
    problems = validate_record(record)
    if problems:
        raise RecordError(f"{where} failed validation: "
                          + "; ".join(problems[:6])
                          + (" ..." if len(problems) > 6 else ""))
    return record


def _edges(node: dict) -> list[str]:
    shape = node.get("shape", {})
    out = [f["type"] for c in shape.get("constructors", [])
           for f in c.get("fields", [])]
    return out + list(shape.get("args", []))


def validate_record(rec) -> list[str]:
    """Structural validation: format, complete coverage, consistent
    versions, acceptance and ownership, required fields. Never raises:
    a record the checks below cannot even traverse is reported as
    malformed."""
    try:
        return _validate_record(rec)
    except (TypeError, KeyError, AttributeError, ValueError, IndexError) as exc:
        return [f"record is malformed ({type(exc).__name__}: {exc})"]


def _is_strs(x) -> bool:
    return isinstance(x, list) and all(isinstance(e, str) for e in x)


def _validate_record(rec) -> list[str]:
    if not isinstance(rec, dict):
        return ["record is not an object"]
    problems: list[str] = []
    for key in TOP_KEYS:
        if key not in rec:
            problems.append(f"missing top-level `{key}`")
    if problems:
        return problems
    if rec["format"] != RECORD_FORMAT:
        problems.append(f"format {rec['format']!r} is not {RECORD_FORMAT}")
    if rec["extractor"] != EXTRACTOR:
        problems.append(f"extractor {rec['extractor']!r} is not {EXTRACTOR!r}")
    types, carriers, routes = rec["types"], rec["carriers"], rec["routes"]
    if not isinstance(types, dict) or not isinstance(carriers, dict) \
            or not isinstance(routes, dict):
        return problems + ["types, carriers and routes must be objects"]
    if not isinstance(rec["extras"], dict):
        return problems + ["extras must be an object"]
    wire = rec["extras"].get("luaWire")
    if not (isinstance(wire, dict) and isinstance(wire.get("probeHex"), str)
            and wire["probeHex"] and wire.get("owner")):
        problems.append("extras.luaWire is missing its codec probe")
    node_problems: list[str] = []
    for key, node in types.items():
        node_problems += _validate_node(key, node, types)
    problems += node_problems
    reached_by: dict[str, set[str]] = {k: set() for k in types}
    for cid, c in carriers.items():
        problems += _validate_carrier(cid, c, types, not node_problems)
        if isinstance(c, dict) and isinstance(c.get("reaches"), list):
            for k in c["reaches"]:
                if k in reached_by:
                    reached_by[k].add(cid)
    for key, who in reached_by.items():
        if not who:
            problems.append(f"type `{key}` is reached by no carrier")
    classes = {c.get("class") for c in carriers.values()
               if isinstance(c, dict)}
    for need in ("component", "metadata", "framing", "lua-module", "legacy"):
        if need not in classes:
            problems.append(f"no carrier of class `{need}`: coverage is "
                            f"incomplete")
    problems += _validate_routes(routes, carriers)
    return problems


def _validate_node(key, node, types) -> list[str]:
    if not isinstance(node, dict) or not isinstance(node.get("shape"), dict):
        return [f"type `{key}` has no shape"]
    shape = node["shape"]
    out = []
    for k in SHAPE_KEYS:
        if k not in shape:
            out.append(f"type `{key}` shape is missing `{k}`")
    if out:
        return out
    if shape["kind"] not in KINDS:
        out.append(f"type `{key}` has unknown kind {shape['kind']!r}")
    if not isinstance(shape["newtype"], bool) \
            or not isinstance(shape["constructors"], list) \
            or not _is_strs(shape["args"]) \
            or not isinstance(shape["detail"], str) \
            or not isinstance(shape["dataType"], str):
        return out + [f"type `{key}` shape has mistyped fields"]
    for con in shape["constructors"]:
        if not (isinstance(con, dict) and isinstance(con.get("name"), str)
                and isinstance(con.get("record"), bool)
                and isinstance(con.get("fields"), list)):
            out.append(f"type `{key}` has a malformed constructor")
            continue
        for f in con["fields"]:
            if not (isinstance(f, dict) and isinstance(f.get("type"), str)
                    and isinstance(f.get("selector"), str)):
                out.append(f"type `{key}` has a malformed field")
    if shape["kind"] == "custom":
        declared = node.get("declared")
        if not (isinstance(declared, dict)
                and all(isinstance(declared.get(k), str) and declared[k]
                        for k in ("contract", "test"))
                and declared.get("sampled") is True):
            out.append(f"custom type `{key}` lacks its declared contract, "
                       f"owning test or sample evidence")
        if not shape["detail"]:
            out.append(f"custom type `{key}` names no owner")
    if out:
        return out
    for edge in _edges(node):
        if edge not in types:
            out.append(f"type `{key}` names unreached type `{edge}`")
    return out


def _closure(root, types) -> set:
    seen, queue = set(), deque([root])
    while queue:
        cur = queue.popleft()
        if cur in seen or cur not in types:
            continue
        seen.add(cur)
        queue.extend(_edges(types[cur]))
    return seen


def _validate_carrier(cid, c, types, nodes_ok) -> list[str]:
    if not isinstance(c, dict):
        return [f"carrier `{cid}` is not an object"]
    missing = [k for k in CARRIER_KEYS if k not in c]
    if missing:
        return [f"carrier `{cid}` is missing `{missing[0]}`"]
    out: list[str] = []
    if c["class"] not in CLASSES:
        out.append(f"carrier `{cid}` has unknown class {c['class']!r}")
    if not isinstance(c["version"], int) or isinstance(c["version"], bool) \
            or c["version"] < 0:
        return out + [f"carrier `{cid}` has a bad version"]
    acc = c["accepted"]
    if not (isinstance(acc, list) and all(isinstance(v, int) for v in acc)
            and acc == sorted(set(acc))):
        return out + [f"carrier `{cid}` accepted set is not a sorted set of "
                      f"integers"]
    if not isinstance(c["required"], bool):
        out.append(f"carrier `{cid}` required is not a boolean")
    if c["class"] != "legacy" and c["version"] not in acc:
        out.append(f"carrier `{cid}` does not accept its own version "
                   f"{c['version']}")
    if any(v > c["version"] for v in acc):
        out.append(f"carrier `{cid}` accepts a version newer than its "
                   f"current {c['version']}")
    if c["class"] in ("component", "metadata", "framing", "lua-module"):
        detail = c["detail"] if isinstance(c["detail"], dict) else {}
        adv = detail.get("advertised")
        if not (isinstance(adv, list) and all(
                isinstance(v, int) and not isinstance(v, bool) for v in adv)) \
                or adv != acc:
            out.append(f"carrier `{cid}` advertises {adv} but its "
                       f"decoder accepts {acc}")
    if c["class"] in ("metadata", "framing"):
        detail = c["detail"] if isinstance(c["detail"], dict) else {}
        eps = detail.get("entryPoints")
        if not (_is_strs(eps) and sorted(eps) == sorted(ENTRY_POINTS)):
            out.append(f"carrier `{cid}` must record exactly the probed entry "
                       f"points {', '.join(ENTRY_POINTS)}")
    if c["class"] == "framing":
        detail = c["detail"] if isinstance(c["detail"], dict) else {}
        for k in ("currentEnvelopeVersion", "envelopeMagic", "headerSize"):
            v = detail.get(k)
            if not isinstance(v, int) or isinstance(v, bool) or v <= 0:
                out.append(f"framing `{k}` is not a positive integer")
        for k in ("fnv1a64Probe", "layoutProbeHex"):
            v = detail.get(k)
            if not isinstance(v, str) or not v:
                out.append(f"framing `{k}` is not a recorded probe")
        if detail.get("currentEnvelopeVersion") != c["version"]:
            out.append("framing currentEnvelopeVersion disagrees with the "
                       "carrier version")
    if not _is_strs(c["reaches"]) or not _is_strs(c["owners"]) \
            or not (c["root"] is None or isinstance(c["root"], str)):
        return out + [f"carrier `{cid}` root/reaches/owners are mistyped"]
    if c["root"] is None:
        if c["class"] in ("component", "metadata", "framing"):
            out.append(f"carrier `{cid}` ({c['class']}) has no wire root")
        if c["reaches"]:
            out.append(f"carrier `{cid}` has no root but reaches types")
    else:
        if c["root"] not in types:
            return out + [f"carrier `{cid}` root `{c['root']}` is not a "
                          f"recorded type"]
        want = sorted(_closure(c["root"], types)) if nodes_ok else None
        if want is not None and sorted(c["reaches"]) != want:
            out.append(f"carrier `{cid}` reaches {len(c['reaches'])} type(s) "
                       f"but its root's transitive closure is {len(want)}")
    if c["class"] == "lua-module":
        owners = c["owners"]
        own = [o for o in owners if o != "scripts/lib/data_codec.lua"]
        if len(set(owners)) != len(owners) or not all(owners) \
                or len(own) != 1 or not own[0].endswith(".lua") \
                or "scripts/lib/data_codec.lua" not in owners:
            out.append(f"carrier `{cid}` must name exactly one owning "
                       f".lua script plus the shared "
                       f"scripts/lib/data_codec.lua, all distinct and "
                       f"non-empty")
    return out


def _validate_routes(routes, carriers) -> list[str]:
    out: list[str] = []
    if not routes:
        return ["no legacy route is recorded"]
    components = {k for k, c in carriers.items()
                  if isinstance(c, dict) and c.get("class") == "component"}
    required = {k for k in components if carriers[k].get("required") is True}
    need = {"b1-session": {"metadata", "legacy:session"} | components,
            "b2-session": {"metadata", "legacy:lua-state"} | required}
    for rid in need:
        if rid not in routes:
            out.append(f"legacy route `{rid}` is not recorded")
    for rid, r in routes.items():
        if not isinstance(r, dict) or not isinstance(r.get("owners"), list) \
                or not isinstance(r.get("enabled"), dict):
            out.append(f"route `{rid}` is malformed")
            continue
        if not r["owners"]:
            out.append(f"route `{rid}` has no owners")
        for owner in r["owners"]:
            if owner not in carriers:
                out.append(f"route `{rid}` is owned by unknown carrier "
                           f"`{owner}`")
        if sorted(r["enabled"]) != sorted(ENTRY_KEYS) or not all(
                isinstance(v, bool) for v in r["enabled"].values()):
            out.append(f"route `{rid}` must record exactly the entry points "
                       f"{', '.join(ENTRY_KEYS)} as booleans")
        missing = sorted(need.get(rid, set()) - set(r["owners"]))
        if missing:
            out.append(f"route `{rid}` ownership is incomplete: missing "
                       f"{', '.join(missing[:4])}")
    return out


# ---------------------------------------------------------------- shapes

def _strip_descriptive(detail):
    if isinstance(detail, dict):
        return {k: v for k, v in detail.items()
                if k not in DESCRIPTIVE_DETAIL_KEYS}
    return detail


def carrier_shape(rec: dict, cid: str) -> dict:
    """What a carrier's wire shape is, for comparison: every reachable
    type's shape (the descriptive `declared` text excluded), plus the
    non-type facts that are part of the wire (framing constants and
    probes; the shared Lua codec probe for Lua modules)."""
    c = rec["carriers"][cid]
    shape = {"class": c["class"], "root": c["root"],
             "types": {k: rec["types"][k]["shape"] for k in c["reaches"]}}
    if c["class"] == "framing":
        shape["detail"] = _strip_descriptive(c["detail"])
    if c["class"] == "lua-module":
        shape["luaWire"] = ((rec["extras"] or {}).get("luaWire") or {}).get("probeHex")
    return shape


def _changed_types(base_shape: dict, cand_shape: dict) -> list[str]:
    old, new = base_shape.get("types", {}), cand_shape.get("types", {})
    return sorted([k for k in new if k not in old]
                  + [k for k in old if k not in new]
                  + [k for k in new if k in old and new[k] != old[k]])


def reach_path(rec: dict, root: str | None, target: str) -> list[str]:
    """A path of type keys from a carrier's root to `target`."""
    if root is None or root not in rec["types"]:
        return []
    prev = {root: None}
    queue = deque([root])
    while queue:
        cur = queue.popleft()
        if cur == target:
            path = []
            while cur is not None:
                path.append(cur)
                cur = prev[cur]
            return path[::-1]
        for nxt in _edges(rec["types"].get(cur, {})):
            if nxt not in prev and nxt in rec["types"]:
                prev[nxt] = cur
                queue.append(nxt)
    return []


def _short(key: str) -> str:
    return key.rsplit(".", 1)[-1] if "." in key else key


def _describe_change(base, cand, cid, base_shape, cand_shape) -> str:
    changed = _changed_types(base_shape, cand_shape)
    if not changed:
        if base_shape.get("detail") != cand_shape.get("detail"):
            return "the framing constants or layout probe changed"
        if base_shape.get("luaWire") != cand_shape.get("luaWire"):
            return "the shared Lua wire encoder (scripts/lib/data_codec.lua) changed"
        return "its root changed"
    shown = changed[0]
    path = (reach_path(cand, cand["carriers"][cid]["root"], shown)
            or reach_path(base, base["carriers"][cid]["root"], shown))
    via = (" (reached " + " -> ".join(_short(k) for k in path) + ")"
           if len(path) > 1 else "")
    more = f" and {len(changed) - 1} more" if len(changed) > 1 else ""
    return f"type `{shown}`{via}{more} changed"


def _route_enabled(route: dict) -> bool:
    return any(v is True for v in route["enabled"].values())


# ------------------------------------------------------------ comparison

def compare(base: dict, cand: dict) -> tuple[list[str], list[str]]:
    """(problems, notes). Pure over two validated records."""
    problems: list[str] = []
    notes: list[str] = []
    affected: dict[str, str] = {}
    bc, cc = base["carriers"], cand["carriers"]

    for cid, c in sorted(cc.items()):
        b = bc.get(cid)
        if b is None:
            if c["class"] != "legacy" and c["accepted"] != [c["version"]]:
                problems.append(
                    f"carrier `{cid}` is a new carrier but accepts "
                    f"{c['accepted']}; a new carrier accepts only its "
                    f"current version {c['version']}")
            notes.append(f"new carrier `{cid}` enters coverage")
            continue
        if c["version"] < b["version"]:
            problems.append(f"carrier `{cid}`: version went down, "
                            f"{b['version']} -> {c['version']}")
            continue
        bumped = c["version"] > b["version"]
        base_shape = carrier_shape(base, cid)
        cand_shape = carrier_shape(cand, cid)
        shape_changed = base_shape != cand_shape
        if shape_changed and not bumped:
            problems.append(
                f"carrier `{cid}`: same-version drift -- "
                f"{_describe_change(base, cand, cid, base_shape, cand_shape)}"
                f", but its on-disk version is still {c['version']}; bump "
                f"it and drop every older version it accepts")
        if not (shape_changed or bumped):
            continue
        why = ("its version was bumped" if bumped
               else "its shape changed")
        affected[cid] = why
        if c["class"] != "legacy" and c["accepted"] != [c["version"]]:
            older = [v for v in c["accepted"] if v != c["version"]]
            problems.append(
                f"carrier `{cid}` is affected ({why}) but still accepts "
                f"older version(s) {older}; it must accept exactly "
                f"{c['version']}")

    for cid in sorted(set(bc) - set(cc)):
        notes.append(f"carrier `{cid}` is no longer covered")

    for rid, route in sorted(cand["routes"].items()):
        hit = [(o, affected[o]) for o in route["owners"] if o in affected]
        if hit and _route_enabled(route):
            on = sorted(k for k, v in route["enabled"].items() if v is True)
            who = "; ".join(f"`{o}` ({w})" for o, w in hit)
            problems.append(
                f"legacy route `{rid}` is still enabled ({', '.join(on)}) "
                f"but decodes through an affected carrier: {who}; disable "
                f"every entry point of the route")
    return problems, notes


def check_fresh(candidate: dict, committed_text: str | None, rel: str) -> list[str]:
    """The committed record must be exactly the compiled evidence."""
    if committed_text is None:
        return [f"{rel} is missing; run --update-record to generate it"]
    try:
        have = json.loads(committed_text)
    except ValueError as exc:
        return [f"{rel} is not valid JSON: {exc}"]
    if have == candidate and committed_text == render_record(candidate):
        return []
    problems = []
    for section in ("types", "carriers", "routes"):
        h, w = have.get(section, {}), candidate[section]
        for key in sorted(set(h) | set(w)):
            if key not in w:
                problems.append(f"{rel}: stale {section[:-1]} entry `{key}` "
                                f"is no longer in the compiled evidence")
            elif key not in h:
                problems.append(f"{rel}: {section[:-1]} `{key}` is in the "
                                f"compiled evidence but missing from the record")
            elif h[key] != w[key]:
                problems.append(f"{rel}: {section[:-1]} `{key}` is out of date")
    if not problems:
        problems.append(f"{rel} differs from the compiled evidence")
    return problems + ["run --update-record, then review the diff"]
