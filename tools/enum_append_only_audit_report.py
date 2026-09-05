"""Compatibility classification and reporting for the enum audit.

The one owner of what a difference MEANS and what the operator is told
to do about it: the both-directions comparison over module-qualified
identities, the exact / append-compatible / incompatible / missing /
extra / relocated classifications, and the migration guidance — from a
fresh attribution walk when the type is still declared, and from the
baseline's own recorded attribution when it is not.

Reachability affects only the "On the wire in" guidance. A guarded type
no save-wire DTO reaches stays guarded and says so.
"""
from __future__ import annotations

import re
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


def wire_attribution(qualified: str,
                     carriers: dict[str, list[Carrier]]) -> tuple:
    """One type's save-wire attribution, spelled exactly as
    `render_baseline` records it, so a freshly walked attribution and a
    captured one are directly comparable."""
    recorded = sorted(carriers.get(qualified, ()),
                      key=lambda c: (c.sort_key, c.path))
    return (
        qualified in carriers,
        tuple(sorted({component for carrier in recorded
                      for component in carrier.components})),
        tuple((carrier.label, " → ".join(carrier.path))
              for carrier in recorded),
    )


# The two shapes `compute_wire_carriers` builds a carrier LABEL in. A
# codec carrier is `"<component>" — <the codec's module>`; a root-module
# carrier is `<the declaring module> — <why that module is a wire root>`.
# Both name a MODULE, and both are what `owner_blind_attribution` has to
# be able to look past.
_CODEC_CARRIER_LABEL_RE = re.compile(
    r'^(?P<head>"[^"]*") — (?P<module>[A-Z][A-Za-z0-9_\']*'
    r"(?:\.[A-Z][A-Za-z0-9_']*)*)$")
_ROOT_CARRIER_LABEL_RE = re.compile(
    r"^(?P<module>[A-Z][A-Za-z0-9_']*(?:\.[A-Z][A-Za-z0-9_']*)*) — "
    r"(?P<tail>.+)$", re.DOTALL)


def carrier_label_without_owner(label: str) -> tuple:
    """One carrier label with the MODULE it names replaced by its form.

    A carrier label is built from two facts: WHO carries the type (a
    component id, or the reason a root module is on the wire) and WHICH
    module the carrying declaration currently lives in. Only the first is
    a wire fact. The second is ownership metadata that a pure module move
    — issue #2135 split `World.Save.Component.Page`'s three codecs into
    three owner modules — legitimately changes without any saved byte
    changing meaning.

    The FORM is kept in the returned tuple so the two shapes can never
    compare equal to each other: a codec carrier turning into a
    root-module carrier is a change in who carries the type, not in where
    a declaration lives. Anything matching neither shape is compared
    verbatim, so an unrecognised label is strict rather than permissive."""
    codec = _CODEC_CARRIER_LABEL_RE.match(label)
    if codec is not None:
        return ("codec", codec.group("head"))
    root = _ROOT_CARRIER_LABEL_RE.match(label)
    if root is not None:
        return ("root", root.group("tail"))
    return ("literal", label)


def owner_blind_attribution(attribution: tuple) -> tuple:
    """A save-wire attribution with every carrier label's module elided.

    Everything else is left exactly as it was: the on-wire status, the
    component set, the number of carriers, their order, and each one's
    `via` path. So a difference in any of THOSE still fails the
    comparison in `relocations()` — this only stops the module name
    embedded in a label from being read as one."""
    on_wire, components, carriers = attribution
    return (on_wire, components,
            tuple((carrier_label_without_owner(label), path)
                  for label, path in carriers))


def recorded_attribution(entry: BaselineEntry) -> tuple:
    """The same tuple, read back from a baseline entry. An entry that
    never captured `onSaveWire` is read as claiming whatever its
    components/carriers imply — and one that captured no attribution at
    all therefore claims to be OFF the wire, which is the honest
    reading: a relocation must not be the thing that quietly puts it
    on."""
    on_wire = entry.on_save_wire
    if on_wire is None:
        on_wire = bool(entry.carriers) or bool(entry.components)
    return (on_wire, tuple(entry.components), tuple(entry.carriers))


def relocations(guarded: dict[str, GuardedType],
                baseline: dict[str, BaselineEntry],
                carriers: dict[str, list[Carrier]]) -> dict[str, str]:
    """Baseline key → live key, for types whose OWNING MODULE moved and
    whose wire contract did not.

    A baseline entry with no live counterpart is normally the audit's
    loudest failure, because a rename, a deletion, a lost `Serialize`
    instance and a module move all look identical from the baseline's
    side, and three of those four stop already-saved bytes decoding. A
    module move does NOT: the type is still declared, still guarded,
    still reached through the same codec, and its tags still mean
    exactly what they meant — only the file that owns the declaration
    changed. That is a real refactor (issue #2098 split the worldgen DTO
    graph into owner modules), and it must be able to ratchet the
    baseline's ownership metadata through `--update-baseline` rather
    than being indistinguishable from a deletion.

    So the recognition is deliberately narrow — every clause below is
    load-bearing, and anything failing one stays INCOMPATIBLE:

    - the live type's BARE NAME is unchanged (a rename is not a move);
    - it lives at a DIFFERENT module-qualified key (this never touches a
      type the baseline already matches);
    - exactly ONE unmatched live type answers to that bare name, and it
      answers to exactly ONE unmatched baseline entry — an ambiguous
      pairing is not evidence of anything;
    - its constructor list is IDENTICAL to the baseline's, slot for
      slot. A move that also reorders, renames, appends to or re-pays a
      constructor is still the byte-reinterpreting change the audit
      exists to catch, and is reported as one;
    - its freshly walked save-wire ATTRIBUTION — on-wire status,
      components, carrier count and order, and every carrier's `via`
      path — equals the attribution the baseline captured, once each
      carrier label's MODULE is elided by `owner_blind_attribution`.
      This clause is what stops a deletion wearing a module move's
      clothes: attribution is walked by bare TYPE NAME, so dropping a
      persisted enum from its DTO and adding an unrelated OFF-wire enum
      with the same name and constructors elsewhere would otherwise
      pair, and `--update-baseline` would rewrite the entry to
      `onSaveWire: false` with no components — erasing the very
      attribution the diagnostic for a later deletion depends on.

      Eliding the module is what #2135 needed and is the ONLY relaxation
      here. A carrier label names the CODEC's module (or, for a root
      carrier, the declaring one), so moving a codec out of the module
      that also declares its own wire sum — which is exactly what
      splitting `World.Save.Component.Page` into three owners does —
      changes the label for a type whose bytes did not move. Before
      #2135 that could not happen: #2098 moved DTO declarations while
      leaving the codecs in place, so the labels happened to stay
      identical, and this clause was written assuming they always would.
      Every other component of the attribution is still compared exactly,
      so a change to the component set, to the on-wire status, or to any
      `via` path remains INCOMPATIBLE — each with its own case in
      `--self-test`.

    A genuine DELETION therefore still fails twice over: nothing answers
    to the bare name, and if something does, its attribution does not
    match."""
    unmatched_live: dict[str, list[str]] = {}
    for qualified, entry in guarded.items():
        if qualified in baseline:
            continue
        unmatched_live.setdefault(entry.name, []).append(qualified)
    unmatched_baseline: dict[str, list[str]] = {}
    for qualified in baseline:
        if qualified in guarded:
            continue
        unmatched_baseline.setdefault(qualified.rsplit(".", 1)[-1],
                                      []).append(qualified)
    moved: dict[str, str] = {}
    for bare, sources in unmatched_baseline.items():
        destinations = unmatched_live.get(bare, [])
        if len(sources) != 1 or len(destinations) != 1:
            continue
        source, destination = sources[0], destinations[0]
        if guarded[destination].constructors != baseline[source].constructors:
            continue
        if owner_blind_attribution(
                    wire_attribution(destination, carriers)) \
                != owner_blind_attribution(
                    recorded_attribution(baseline[source])):
            continue
        moved[source] = destination
    return moved


def compare(guarded: dict[str, GuardedType],
            baseline: dict[str, BaselineEntry],
            carriers: dict[str, list[Carrier]]) -> list[Finding]:
    """Cross-check the discovered set against the baseline BOTH ways.

    `carriers` is REQUIRED rather than defaulted: `relocations()` reads
    it to prove a relocated type still sits on the same save wire, and a
    caller that forgot to pass it would silently treat every type as off
    the wire."""
    findings: list[Finding] = []
    moved = relocations(guarded, baseline, carriers)
    arrived = {destination: source for source, destination in moved.items()}
    for qualified in sorted(guarded):
        if qualified not in baseline:
            entry = guarded[qualified]
            if qualified in arrived:
                source = arrived[qualified]
                recorded = baseline[source]
                where = (f" (last recorded in {recorded.source})"
                         if recorded.source else "")
                findings.append(Finding(qualified, True, [
                    f"{qualified} ({entry.where()})",
                    f"    RELOCATED from {source}{where} with its "
                    f"{len(entry.constructors)} constructor(s) unchanged "
                    f"({', '.join(c.render() for c in entry.constructors)})"
                    f" — the declaration moved between modules and no "
                    f"saved byte changed meaning.",
                    f"    Its save-wire attribution — on-wire status, "
                    f"components and every carrier's `via` path — is "
                    f"unchanged too, so only the baseline's ownership "
                    f"metadata (its qualified key, its `source`, and the "
                    f"module named inside a carrier label) is stale.",
                ]))
                continue
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
        if qualified in guarded or qualified in moved:
            continue
        recorded = baseline[qualified]
        where = f" — last seen in {recorded.source}" if recorded.source else ""
        findings.append(Finding(qualified, False, [
            f"{qualified} (baseline only{where})",
            "    has a baseline entry but no longer qualifies for the "
            "guarded set — it was renamed, lost its `Generic`-derived "
            "`Serialize` instance, stopped being a sum, or was deleted. "
            "(A pure module MOVE that keeps the name and every "
            "constructor is recognised as a relocation instead; this is "
            "not one.)",
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


@dataclass(frozen=True)
class Coverage:
    """The three figures the success line reports.

    They move TOGETHER on a routine append — every guarded type added
    since 2026-08-15 moved all three — which is why none of them is
    written down in prose any more (issue #2299): the paragraph in
    `docs/engine_contracts.md` telling readers not to hand-count these
    was itself three appends stale."""
    guarded: int
    on_save_wire: int
    component_named: int


def coverage_counts(guarded: dict[str, GuardedType],
                    baseline: dict[str, BaselineEntry]) -> Coverage:
    """The discovered guarded-set size, plus how many BASELINE entries
    record `onSaveWire: true` and how many record a NON-EMPTY
    `components` list.

    The last two are read back from the baseline rather than recomputed
    from the carrier walk on purpose: they are exactly the two per-type
    fields the contract document points a reader at, and the success
    path is reached only when that file's attribution already matches
    the code (`run_repository_audit` fails on `stale` otherwise), so the
    two derivations cannot disagree there.

    Component-naming is counted per TYPE, not per component name: a type
    carried by two components counts once, which is what makes the
    figure comparable with the guarded total beside it. An entry whose
    `on_save_wire` was never captured (a hand-added one) is `None` and
    counts as not on the wire, like a captured `False`."""
    return Coverage(
        guarded=len(guarded),
        on_save_wire=sum(1 for entry in baseline.values()
                         if entry.on_save_wire),
        component_named=sum(1 for entry in baseline.values()
                            if entry.components))


def report(findings: list[Finding], carriers: dict[str, list[Carrier]],
           coverage: Coverage, stale_attribution: bool = False) -> int:
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
        print(f"enum_append_only_audit.py: {coverage.guarded} guarded sum "
              f"type(s) match {BASELINE_REL} "
              f"({coverage.on_save_wire} on the save wire, "
              f"{coverage.component_named} named by a live component)")
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
