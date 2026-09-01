"""Compatibility classification and reporting for the enum audit.

The one owner of what a difference MEANS and what the operator is told
to do about it: the both-directions comparison over module-qualified
identities, the exact / append-compatible / incompatible / missing /
extra classifications, and the migration guidance — from a fresh
attribution walk when the type is still declared, and from the
baseline's own recorded attribution when it is not.

Reachability affects only the "On the wire in" guidance. A guarded type
no save-wire DTO reaches stays guarded and says so.
"""
from __future__ import annotations

from dataclasses import dataclass

from enum_append_only_audit_model import (
    BASELINE_REL,
    SOURCE_DIRS,
    BaselineEntry,
    Carrier,
    Constructor,
    GuardedType,
)


@dataclass
class Finding:
    qualified: str
    compatible: bool       # True = append-compatible; baseline must ratchet
    lines: list[str]
    # Set when the LIVE declaration is gone (renamed, moved, deleted, or
    # no longer qualifying), so its attribution must be read back from
    # the baseline instead of walked.
    recorded: BaselineEntry | None = None


def classify(qualified: str, entry: GuardedType,
             baseline: list[Constructor]) -> Finding | None:
    """Compare one type's live constructor list against its baseline."""
    live = entry.constructors
    if live == baseline:
        return None
    if live[:len(baseline)] == baseline:
        added = ", ".join(c.render() for c in live[len(baseline):])
        return Finding(qualified, True, [
            f"{qualified} ({entry.where()})",
            f"    appended {added} after {baseline[-1].render()} — "
            f"APPEND-COMPATIBLE.",
        ])
    lines = [f"{qualified} ({entry.where()})"]
    lines.extend(describe_incompatibility(baseline, live))
    return Finding(qualified, False, lines)


def describe_incompatibility(baseline: list[Constructor],
                             live: list[Constructor]) -> list[str]:
    """Say WHICH tag positions changed meaning, not merely that they did."""
    lines: list[str] = []
    for index in range(max(len(baseline), len(live))):
        was = baseline[index] if index < len(baseline) else None
        now = live[index] if index < len(live) else None
        if was == now:
            continue
        if was is None:
            lines.append(f"    tag {index}: added {now.render()} after an "
                         f"already-changed tag (not an append)")
        elif now is None:
            lines.append(f"    tag {index}: {was.render()} REMOVED — every "
                         f"saved value carrying this tag is unreadable")
        elif was.name != now.name:
            lines.append(f"    tag {index}: was {was.render()}, now "
                         f"{now.render()} — every saved {was.name} decodes "
                         f"as {now.name}")
        elif was.arity != now.arity:
            lines.append(f"    tag {index}: {was.name} carried {was.arity} "
                         f"field(s), now carries {now.arity} — the payload "
                         f"after this tag changes shape")
        else:
            lines.append(f"    tag {index}: {was.name} still carries "
                         f"{was.arity} field(s), but their PAYLOAD changed "
                         f"— every saved {was.name} decodes its bytes into "
                         f"the wrong fields")
            for slot in range(was.arity):
                if was.payload[slot] == now.payload[slot]:
                    continue
                lines.append(f"      field {slot}: was `{was.payload[slot]}`"
                             f", now `{now.payload[slot]}`")
    return lines


def compare(guarded: dict[str, GuardedType],
            baseline: dict[str, BaselineEntry]) -> list[Finding]:
    """Cross-check the discovered set against the baseline BOTH ways."""
    findings: list[Finding] = []
    for qualified in sorted(guarded):
        if qualified not in baseline:
            entry = guarded[qualified]
            findings.append(Finding(qualified, True, [
                f"{qualified} ({entry.where()})",
                f"    newly qualifies for the guarded set with "
                f"{len(entry.constructors)} constructors and has no "
                f"baseline entry — APPEND-COMPATIBLE.",
            ]))
            continue
        finding = classify(qualified, guarded[qualified],
                           baseline[qualified].constructors)
        if finding is not None:
            findings.append(finding)
    for qualified in sorted(baseline):
        if qualified in guarded:
            continue
        recorded = baseline[qualified]
        where = f" — last seen in {recorded.source}" if recorded.source else ""
        findings.append(Finding(qualified, False, [
            f"{qualified} (baseline only{where})",
            "    has a baseline entry but no longer qualifies for the "
            "guarded set — it was renamed, moved to another module, lost "
            "its `Generic`-derived `Serialize` instance, stopped being a "
            "sum, or was deleted.",
            f"    Every one of those changes what already-saved bytes mean: "
            f"the {len(recorded.constructors)} tag(s) it defined "
            f"({', '.join(c.render() for c in recorded.constructors)}) no "
            f"longer decode to anything.",
        ], recorded=recorded))
    return findings


# The migration every incompatible change needs, whether the live
# declaration is still there to walk or not. Declared once so the two
# paths below cannot drift into giving different instructions.
_MIGRATION_STEPS = [
    "    Do this instead of editing the declaration in place:",
    "      1. Freeze the CURRENT shape as a versioned DTO that stays "
    "decodable — including a frozen copy of the OLD enum, since the "
    "historical bytes still carry the old tags.",
    "      2. Bump `ccVersion` on EVERY component listed above, and add "
    "the outgoing version to that component's `ccInputVers` (through "
    "`csOlderVersions`/`atVersion`) so the reader still accepts it.",
    "      3. Migrate from the frozen DTO into the changed type.",
    "      4. Then make the change you wanted — appending at the END, "
    "which needs no migration at all, if that is enough — and ratchet "
    "this baseline.",
]

_OFF_WIRE_NOTE = (
    "    No save-wire DTO reaches it, so no component version needs "
    "bumping — but this type does derive positional `Serialize`, so "
    "confirm nothing outside src/World/Save writes it to disk before "
    "changing it.")


def carrier_lines(qualified: str,
                  carriers: dict[str, list[Carrier]]) -> list[str]:
    """The migration guidance for one incompatible change to a type that
    is STILL declared: every affected component and historical shape,
    walked fresh, and what to do instead."""
    entries = carriers.get(qualified, [])
    if not entries:
        return [_OFF_WIRE_NOTE]
    lines = ["    On the wire in:"]
    for carrier in sorted(entries, key=lambda c: (c.sort_key, c.path)):
        lines.append(f"      {carrier.label}")
        lines.append(f"        via {' → '.join(carrier.path)}")
    return lines + _MIGRATION_STEPS


def recorded_carrier_lines(entry: BaselineEntry) -> list[str]:
    """The same guidance for a type whose declaration is GONE.

    A renamed, moved, or deleted guarded type cannot be walked — there
    is nothing left to reach — yet it is precisely the change that needs
    the components named, because every tag it defined stops decoding.
    So the attribution captured alongside its constructor list is read
    back instead of recomputed."""
    if not entry.carriers:
        if entry.components:
            # An entry written before `carriers` existed, or hand-added:
            # the flat component list is still enough to name them.
            lines = ["    The baseline recorded it on the wire in "
                     "(attribution as captured, not a fresh walk — the "
                     "declaration is gone):"]
            lines.extend(f'      "{component}"'
                         for component in entry.components)
            return lines + _MIGRATION_STEPS
        return [
            "    The baseline recorded no save-wire carrier for it, so no "
            "component version needs bumping — but confirm that is still "
            "true (the declaration is gone, so this cannot be re-derived) "
            "before dropping the entry."]
    lines = ["    The baseline recorded it on the wire in (attribution as "
             "captured, not a fresh walk — the declaration is gone):"]
    for label, via in entry.carriers:
        lines.append(f"      {label}")
        lines.append(f"        via {via}")
    return lines + _MIGRATION_STEPS


def guidance_lines(finding: Finding,
                   carriers: dict[str, list[Carrier]]) -> list[str]:
    """Migration guidance for one incompatible finding, from whichever
    attribution is available: a fresh walk when the type is still
    declared, the baseline's own record when it is not."""
    if finding.recorded is not None:
        return recorded_carrier_lines(finding.recorded)
    return carrier_lines(finding.qualified, carriers)


def report(findings: list[Finding], carriers: dict[str, list[Carrier]],
           guarded_count: int, stale_attribution: bool = False) -> int:
    compatible = [f for f in findings if f.compatible]
    incompatible = [f for f in findings if not f.compatible]
    if not findings:
        if stale_attribution:
            print(f"{BASELINE_REL}: every constructor list matches, but its "
                  f"`source`/`onSaveWire`/`components`/`carriers` "
                  f"attribution no longer matches the code.")
            print("  Nothing is broken on the wire — but a diagnostic that "
                  "names the wrong components is worse than none, so the "
                  "file is kept regenerated rather than merely append-checked.")
            print("  Refresh it with: "
                  "python3 tools/enum_append_only_audit.py --update-baseline")
            return 1
        print(f"enum_append_only_audit.py: {guarded_count} guarded sum "
              f"type(s) match {BASELINE_REL}")
        return 0
    if incompatible:
        print(f"{len(incompatible)} INCOMPATIBLE constructor change(s) — "
              f"these silently reinterpret already-saved bytes:")
        for finding in incompatible:
            for line in finding.lines:
                print(f"  {line}")
            for line in guidance_lines(finding, carriers):
                print(f"  {line}")
            print()
    if compatible:
        print(f"{len(compatible)} append-compatible change(s) — allowed, but "
              f"{BASELINE_REL} must record them:")
        for finding in compatible:
            for line in finding.lines:
                print(f"  {line}")
        print("  An append stays safe only while the baseline ratchets "
              "forward with it: leave the old list in place and a later "
              "removal or rename of the appended constructor would still "
              "look like a prefix match.")
        print(f"  The guarded set is every `data` under "
              f"{'/, '.join(SOURCE_DIRS)}/ that derives `Serialize` through "
              f"`Generic` and declares two or more constructors — see "
              f"tools/enum_append_only_audit.py's module docstring for why "
              f"that rule, and what it deliberately does not cover.")
        if not incompatible:
            print("  Record them with: "
                  "python3 tools/enum_append_only_audit.py --update-baseline")
    return 1
