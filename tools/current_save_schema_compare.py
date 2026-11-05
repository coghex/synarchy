"""Base-versus-candidate rules for tools/current_save_schema_audit.py
(issue #2916).

Both sides are `current_save_schema_model.Model`s computed FROM SOURCE.
The checked-in record is never an input to the comparison, only a
projection of the candidate that must match it (`check_record`), so
regenerating, editing or deleting the record cannot bless a same-version
change, and an initial record introduction is still compared against a
real base model rather than against nothing.

The rules, per carrier:

  shape changed, version not strictly higher     -> same-version drift
  version lower than the base's                  -> downgrade
  AFFECTED (shape changed, legacy shape changed
    or real version higher)                      -> accepts exactly its
                                                    current version, and
                                                    every enabled legacy
                                                    route that decodes
                                                    through it is off
  new carrier                                    -> covered, accepts only
                                                    its current version
  retired legacy carrier (session, lua-state)    -> a shape change needs
                                                    its route disabled;
                                                    its own version has no
                                                    meaning to bump

Unchanged carriers owe nothing. No old decoder is ever analysed.
"""
from __future__ import annotations

import json
from dataclasses import dataclass

from current_save_schema_model import Carrier, Model


@dataclass
class Evidence:
    """What the comparison was made between. `base_sha` and
    `candidate_sha` are the resolved commits; `dirty` is whether the
    candidate working tree differs from `candidate_sha`."""
    base_ref: str
    base_sha: str
    candidate_sha: str
    dirty: bool = False

    def describe(self) -> str:
        dirty = " (+ uncommitted changes)" if self.dirty else ""
        return (f"base {self.base_ref} = {self.base_sha}; "
                f"candidate = {self.candidate_sha}{dirty}")


def check_evidence(ev: Evidence) -> list[str]:
    """Missing or self-referential comparison evidence fails."""
    problems = []
    if not ev.base_sha or not ev.candidate_sha:
        problems.append(
            "comparison evidence missing: the base or candidate revision "
            "did not resolve to a commit")
    elif ev.base_sha == ev.candidate_sha and not ev.dirty:
        problems.append(
            f"candidate-to-itself comparison: base {ev.base_ref} resolves "
            f"to {ev.base_sha}, the candidate commit, with no uncommitted "
            f"changes -- compare against the revision the change started "
            f"from (the merge-base of the target branch)")
    return problems


def _changed(base: Carrier, cand: Carrier) -> str:
    old, new = base.decl_hashes, cand.decl_hashes
    names = sorted(
        [q for q in new if q not in old] + [q for q in old if q not in new]
        + [q for q in new if q in old and new[q] != old[q]])
    if not names:
        return "custom serializer or owner source changed"
    shown = ", ".join(names[:6]) + (" ..." if len(names) > 6 else "")
    return f"declarations changed: {shown}"


def compare(base: Model, cand: Model) -> list[str]:
    problems: list[str] = []
    affected: dict[str, str] = {}

    for cid, c in sorted(cand.carriers.items()):
        if c.current_version not in c.accepted:
            problems.append(
                f"carrier `{cid}`: current version {c.current_version} is "
                f"not in its accepted versions {c.accepted}")
        if any(v > c.current_version for v in c.accepted):
            problems.append(
                f"carrier `{cid}`: accepts a version newer than its "
                f"current {c.current_version} ({c.accepted})")
        b = base.carriers.get(cid)
        if b is None:
            if c.kind != "legacy" and c.accepted != [c.current_version]:
                problems.append(
                    f"carrier `{cid}` is new but accepts "
                    f"{c.accepted}; a new carrier accepts only its "
                    f"current version {c.current_version}")
            continue
        if c.current_version < b.current_version:
            problems.append(
                f"carrier `{cid}`: version went DOWN, "
                f"{b.current_version} -> {c.current_version}")
            continue
        bumped = c.current_version > b.current_version
        shape_changed = c.shape != b.shape
        known = {**b.decl_hashes, **b.legacy_hashes}
        legacy_edited = sorted(
            q for q, digest in c.legacy_hashes.items()
            if known.get(q) != digest)
        legacy_changed = bool(legacy_edited)
        if c.kind == "legacy":
            if shape_changed:
                affected[cid] = "its frozen shape changed"
            continue
        if shape_changed and not bumped:
            problems.append(
                f"carrier `{cid}`: same-version drift -- "
                f"{_changed(b, c)}, but its on-disk version is still "
                f"{c.current_version}; bump it (and drop every older "
                f"version it accepts)")
        if not (shape_changed or bumped or legacy_changed):
            continue
        why = ("its version was bumped" if bumped else
               "its shape changed" if shape_changed else
               "a declaration only an older accepted version reaches changed")
        affected[cid] = why
        if c.accepted != [c.current_version]:
            older = [v for v in c.accepted if v != c.current_version]
            problems.append(
                f"carrier `{cid}` is affected ({why}) but still accepts "
                f"older version(s) {older}; it must accept exactly "
                f"{c.current_version}")

    for rid, route in sorted(cand.routes.items()):
        hit = [(cid, affected[cid]) for cid in route.carriers
               if cid in affected]
        if route.enabled and hit:
            who = "; ".join(f"`{cid}` ({why})" for cid, why in hit)
            problems.append(
                f"legacy route `{rid}` is still enabled but decodes "
                f"through an affected carrier: {who}; disable the route")
    return problems


def check_record(cand: Model, record: dict | None, rel: str) -> list[str]:
    """The checked-in record must be exactly the candidate's projection."""
    if record is None:
        return [f"{rel} is missing; run --update-record to generate it"]
    want = cand.to_record()
    if record == want:
        return []
    problems: list[str] = []
    for section in ("carriers", "routes"):
        have, expect = record.get(section, {}), want[section]
        for key in sorted(set(have) | set(expect)):
            if key not in expect:
                problems.append(
                    f"{rel}: stale {section[:-1]} entry `{key}` is no "
                    f"longer in the source")
            elif key not in have:
                problems.append(
                    f"{rel}: {section[:-1]} `{key}` is in the source but "
                    f"missing from the record")
            elif have[key] != expect[key]:
                fields = sorted(f for f in set(have[key]) | set(expect[key])
                                if have[key].get(f) != expect[key].get(f))
                problems.append(
                    f"{rel}: {section[:-1]} `{key}` is out of date "
                    f"({', '.join(fields)})")
    if record.get("format") != want["format"]:
        problems.append(f"{rel}: record format {record.get('format')!r} "
                        f"is not {want['format']!r}")
    if not problems:
        problems.append(f"{rel} differs from the source")
    return problems + ["run --update-record, then review the diff"]


def render_record(model: Model) -> str:
    return json.dumps(model.to_record(), indent=2, sort_keys=True) + "\n"
