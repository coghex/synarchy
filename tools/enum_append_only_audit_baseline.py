"""Baseline loading and rendering for the enum append-only audit.

The one owner of `docs/save_compat/enum_baseline.json`: its schema
validation on the way in, and the deterministic rendering on the way
out. Consumes only the shared models, so the audit and the baseline
ratchet read and write through exactly the same code.
"""
from __future__ import annotations

import json
from pathlib import Path

from enum_append_only_audit_model import (
    BASELINE_REL,
    AuditError,
    BaselineEntry,
    Carrier,
    Constructor,
    GuardedType,
)


def load_baseline(path: Path) -> dict[str, BaselineEntry]:
    """Read the checked-in constructor baseline.

    Strict about the CONTRACT (`constructors`): a malformed entry fails
    rather than being skipped, because a skipped entry is an unguarded
    enum. Lenient about the informational attribution, which a
    hand-written entry may legitimately omit — its absence only costs
    detail in one diagnostic, and `run_repository_audit`'s
    regenerated-file check catches it anyway."""
    if not path.exists():
        raise AuditError(f"{BASELINE_REL}: baseline file is missing")
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as err:
        raise AuditError(f"{BASELINE_REL}: not valid JSON ({err})") from err
    if not isinstance(raw, dict) or not isinstance(raw.get("types"), dict):
        raise AuditError(
            f"{BASELINE_REL}: expected an object with a `types` object")
    out: dict[str, list[Constructor]] = {}
    for qualified, entry in raw["types"].items():
        if not isinstance(entry, dict) or "constructors" not in entry:
            raise AuditError(
                f"{BASELINE_REL}: `{qualified}` has no `constructors` list")
        raw_ctors = entry["constructors"]
        if not isinstance(raw_ctors, list) or len(raw_ctors) < 2:
            raise AuditError(
                f"{BASELINE_REL}: `{qualified}`'s `constructors` must be a "
                f"list of at least two entries (only sums are guarded)")
        ctors: list[Constructor] = []
        for item in raw_ctors:
            if not isinstance(item, dict) or "name" not in item \
                    or "arity" not in item or "payload" not in item:
                raise AuditError(
                    f"{BASELINE_REL}: `{qualified}` has a constructor entry "
                    f"without all of `name`, `arity` and `payload`")
            arity = item["arity"]
            if not isinstance(arity, int) or isinstance(arity, bool) \
                    or arity < 0:
                raise AuditError(
                    f"{BASELINE_REL}: `{qualified}`'s `{item['name']}` has a "
                    f"non-integer arity {arity!r}")
            payload = item["payload"]
            if not isinstance(payload, list) \
                    or not all(isinstance(slot, str) for slot in payload):
                raise AuditError(
                    f"{BASELINE_REL}: `{qualified}`'s `{item['name']}` has a "
                    f"`payload` that is not a list of field strings")
            if len(payload) != arity:
                raise AuditError(
                    f"{BASELINE_REL}: `{qualified}`'s `{item['name']}` "
                    f"declares arity {arity} but {len(payload)} payload "
                    f"field(s) — the two describe the same thing and a "
                    f"disagreement means the entry was hand-edited")
            ctors.append(Constructor(str(item["name"]),
                                     tuple(str(slot) for slot in payload)))
        recorded: list[tuple[str, str]] = []
        for item in entry.get("carriers", []):
            if not isinstance(item, dict) or "carrier" not in item \
                    or "via" not in item:
                raise AuditError(
                    f"{BASELINE_REL}: `{qualified}` has a `carriers` entry "
                    f"without both `carrier` and `via`")
            recorded.append((str(item["carrier"]), str(item["via"])))
        on_wire = entry.get("onSaveWire")
        out[qualified] = BaselineEntry(
            constructors=ctors,
            components=tuple(str(c) for c in entry.get("components", [])),
            carriers=tuple(recorded),
            source=str(entry.get("source", "")),
            on_save_wire=on_wire if isinstance(on_wire, bool) else None)
    if not out:
        raise AuditError(
            f"{BASELINE_REL}: declares no types — a vacuous baseline would "
            f"pass against anything")
    return out


def render_baseline(guarded: dict[str, GuardedType],
                    carriers: dict[str, list[Carrier]]) -> str:
    """Serialize the guarded set as the baseline file's content."""
    types: dict[str, object] = {}
    for qualified in sorted(guarded):
        entry = guarded[qualified]
        recorded = sorted(carriers.get(qualified, ()),
                          key=lambda c: (c.sort_key, c.path))
        types[qualified] = {
            "source": entry.rel_path,
            "onSaveWire": qualified in carriers,
            "components": sorted({component for carrier in recorded
                                  for component in carrier.components}),
            "carriers": [{"carrier": c.label, "via": " → ".join(c.path)}
                         for c in recorded],
            "constructors": [{"name": c.name, "arity": c.arity,
                              "payload": list(c.payload)}
                             for c in entry.constructors],
        }
    document = {
        "_comment": (
            "Golden constructor lists for every positionally-serialized "
            "sum type (issue #1145). Generated and checked by "
            "tools/enum_append_only_audit.py -- do not hand-edit to make "
            "the audit pass: a change that is not a pure append is a "
            "save-format break, not a baseline update. The `constructors` "
            "list is the contract: each entry's `name` pins what its "
            "positional tag means and its `payload` pins the ordered "
            "field slots that follow the tag (`arity` is their count), "
            "because cereal writes those fields positionally too. "
            "`source`/`onSaveWire`/`components`/`carriers` record where "
            "each type sat on the save wire when it was captured, so a "
            "type that is later renamed, moved, or deleted -- and "
            "therefore can no longer be walked -- still reports which "
            "components and historical shapes carried it."),
        "types": types,
    }
    return json.dumps(document, indent=2, ensure_ascii=False) + "\n"
