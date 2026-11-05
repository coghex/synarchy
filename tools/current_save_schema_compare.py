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
    versions, acceptance and ownership, required fields."""
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
    for key, node in types.items():
        if not isinstance(node, dict) or "shape" not in node:
            problems.append(f"type `{key}` has no shape")
            continue
        for edge in _edges(node):
            if edge not in types:
                problems.append(f"type `{key}` names unreached type `{edge}`")
    for cid, c in carriers.items():
        if not isinstance(c, dict):
            problems.append(f"carrier `{cid}` is not an object")
            continue
        for key in CARRIER_KEYS:
            if key not in c:
                problems.append(f"carrier `{cid}` is missing `{key}`")
        if any(k not in c for k in CARRIER_KEYS):
            continue
        if c["class"] not in CLASSES:
            problems.append(f"carrier `{cid}` has unknown class {c['class']!r}")
        if not isinstance(c["version"], int) or c["version"] < 0:
            problems.append(f"carrier `{cid}` has a bad version")
            continue
        acc = c["accepted"]
        if not (isinstance(acc, list) and all(isinstance(v, int) for v in acc)
                and acc == sorted(set(acc))):
            problems.append(f"carrier `{cid}` accepted set is not a sorted "
                            f"set of integers")
            continue
        if c["class"] != "legacy" and c["version"] not in acc:
            problems.append(f"carrier `{cid}` does not accept its own "
                            f"version {c['version']}")
        if any(v > c["version"] for v in acc):
            problems.append(f"carrier `{cid}` accepts a version newer than "
                            f"its current {c['version']}")
        if c["root"] is not None:
            if c["root"] not in types:
                problems.append(f"carrier `{cid}` root `{c['root']}` is not "
                                f"a recorded type")
            if c["root"] not in c["reaches"]:
                problems.append(f"carrier `{cid}` does not reach its root")
        for k in c["reaches"]:
            if k not in types:
                problems.append(f"carrier `{cid}` reaches unrecorded `{k}`")
        if c["class"] in ("component", "metadata", "framing") \
                and c["root"] is None:
            problems.append(f"carrier `{cid}` ({c['class']}) has no wire root")
        if c["class"] == "lua-module" and not c["owners"]:
            problems.append(f"carrier `{cid}` names no owning source")
    classes = {c.get("class") for c in carriers.values()
               if isinstance(c, dict)}
    for need in ("component", "metadata", "framing", "lua-module", "legacy"):
        if need not in classes:
            problems.append(f"no carrier of class `{need}`: coverage is "
                            f"incomplete")
    for rid, r in routes.items():
        if not isinstance(r, dict) or "owners" not in r or "enabled" not in r:
            problems.append(f"route `{rid}` is malformed")
            continue
        for owner in r["owners"]:
            if owner not in carriers:
                problems.append(f"route `{rid}` is owned by unknown "
                                f"carrier `{owner}`")
        if not isinstance(r["enabled"], dict) or not r["enabled"]:
            problems.append(f"route `{rid}` records no entry-point state")
    if not routes:
        problems.append("no legacy route is recorded")
    return problems


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
