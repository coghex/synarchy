#!/usr/bin/env python3
"""Add-only inventory + ordinal ratchet for the shipped concept catalogue.

Two issues share this artifact. #1717 pinned every shipped concept id's
PRESENCE and exact STRING; #1868 added each id's append-only ORDINAL,
which is the order `Language.Generated.Root.assignRoots` places concepts
in. The ordinal is what makes an ADDITION root-stable: placement used to
be ascending-id order, so a new id that sorted before an incumbent could
take the root that incumbent would have had and force it to reroll to a
completely different one.

`src/Language/Semantic/Types.hs` states the rule plainly — "Ids may be
added, never renamed or reused" — and until this audit nothing enforced
any part of it. This audit enforces THREE of those, not all of it:
removal, rename and unratcheted addition (see "What this audit cannot
see" below), plus the ordinal shape #1868 needs. A change could drop
`MEMORY`, add a conforming `RECOLLECTION`, and leave every existing
check green: the headless suite (`Test.Headless.Language.Semantic`)
pins the catalogue VERSION, a minimum COUNT, the six domains, the 20-30
per-domain balance and the presence of all four authored forms, and
every one of those is an aggregate that names no individual id.

=== Why an id is immutable once shipped

A `ConceptId` is not editorial text; it is persisted identity and a
derivation input.

  * `src/Language/Etymology.hs` reports a concept the current catalogue
    no longer carries as `EtyUnavailable (EtyInvalidConcept ...)`. Every
    already-persisted `EtymologySource` naming a removed id therefore
    loses its etymology, in every existing save.
  * `src/Language/Generated/Hash.hs` seeds each concept's native root
    from `conceptIdText`. The id string is the concept-specific seed
    input — generator version, language seed and retry attempt are mixed
    in too, so a rename does not MATHEMATICALLY guarantee a different
    surface root in every language, but it changes the deterministic
    seed and so re-rolls the root wherever the hash lands elsewhere.

A rename is both hazards at once, which is why this audit reports it as
a removal AND an addition rather than as a single "renamed" finding.

=== What is guarded, and what is deliberately NOT

Guarded: the PRESENCE of every id the catalogue has ever shipped, its
exact STRING, and its ORDINAL — so removal, rename and unratcheted
addition fail, and so does an artifact whose ordinals no longer form the
append-only sequence root assignment reads. That is the whole boundary
this gate can hold; "never reused" is not part of it (see below).

Not guarded, deliberately (issue #1717 requirement 5):

  * The four English forms (`singular`, `plural`, `modifier`,
    `possessive`). The etymology surface check compares the rebuilt
    NATIVE name, and the stored gloss is passed through unchecked, so
    re-wording a form invalidates nothing persisted. It can change
    etymology PRESENTATION — `Language.Etymology` rebuilds displayed
    morpheme lemmas from the current forms — but presentation is not
    identity, and freezing wording would make every copy-edit a
    ratchet step.
  * `domain`. `ceDomain` has no consumer anywhere in `src/` or `app/`
    outside the type and the YAML parser, and never reaches a save.

That every form is authored and non-empty — which
`Language.Etymology`'s `missingConcepts` depends on — is already
enforced, by `Test.Headless.Language.Semantic` against the real
catalogue and by `Language.Semantic.Catalogue`'s own fail-loud
validation at load time. Requirement 5 PERMITS this audit to re-check
it and does not require it; a second enforcement point for a rule that
already has one could only drift, so this audit stays strictly an ID
inventory. The self-test's re-worded-form and changed-domain cases pin
that scope rather than merely observing it.

=== What this audit cannot see

Same-string REPURPOSING — the "never reused" half of the contract
comment's rule. Because every authored form and the domain may
legitimately change, an entry that keeps its id while its meaning is
rewritten from "memory" to "mountain" is indistinguishable here from an
ordinary copy-edit. Detecting it would mean freezing exactly the fields
requirement 5 keeps editable, so REUSE remains a review policy and is
not behaviour this gate enforces. The gate covers removal, rename and
unratcheted addition, and nothing claims otherwise: the diagnostics
below restate the RULE an id is held to, never the set of violations
this audit can see.

=== The artifact and the ratchet

`data/language/concept_id_baseline.json` records every id the catalogue
has shipped, each with its append-only ordinal. Today the id set equals
the current inventory: the shipped set grew monotonically (54 -> 150 ->
151 ids) and the current catalogue still contains every historical id,
so the initial artifact was seeded from it. It lives under `data/`, not
`docs/`, because `Language.Semantic.Catalogue` READS it at run time
through the resource root (#636) alongside `concepts.yaml`; it is
generated, machine-read and load-bearing, and an implementation PR that
ratchets it is not a documentation change.

The ordinals seeded for the 151 shipped ids are ascending-id RANK, which
is the order the old `sort ids` placement used — so seeding reproduced
every language's existing roots byte for byte and needed no
`currentGeneratorVersion` bump. Note that `data/language/concepts.yaml`
is grouped by DOMAIN, so the authored file order is not the placement
order and cannot seed one.

`--update-baseline` is the only way an addition passes, and it is
MONOTONIC: it records catalogue ids the artifact lacks, and REFUSES any
run that would drop an artifact id — so it can never double as a "make
the failure go away" button for a removal or a rename. A rename is
refused in update mode for exactly that reason.

It is also APPEND-ONLY in the ordinal dimension: it preserves every
recorded `(id, ordinal)` pair exactly and gives each new id a distinct
ordinal after the previous maximum, assigned in ascending `ConceptId`
order so one update introducing several ids is deterministic and a
repeat run is a no-op. The JSON is the sole historical authority, so
this audit cannot see that a hand-edited ordinal USED to be something
else; what it does enforce is that the artifact is a well-formed
append-only sequence — ids unique, ordinals unique, and the recorded
ordinals exactly `0 .. n-1` — and that the catalogue and the artifact
name the same ids. A hand-edit that renumbers is caught by that shape
check or by the goldens in `Test.Headless.Language.Generated`, which pin
a real seed's whole root map.

Partial backstops exist and are not substitutes.
`Test.Headless.Location.Naming` pins three exact names and glosses, and
`Test.Headless.River.Naming` pins the `RIVER` forms, eight exact native
names and three gloss associations. Both are indirect goldens over the
handful of concepts their samples happen to use, on one provenance;
neither inventories the shipped ids.

Usage:

  python3 tools/concept_id_inventory_audit.py
  python3 tools/concept_id_inventory_audit.py --update-baseline
  python3 tools/concept_id_inventory_audit.py --self-test

Exit status: 0 = the shipped inventory is intact, 1 = it is not (or a
self-test check failed, or an update was refused).
"""

from __future__ import annotations

import argparse
import contextlib
import io
import json
import sys
import tempfile
from pathlib import Path

try:
    import yaml  # PyYAML
except ImportError:
    sys.stderr.write(
        "error: PyYAML is required. Install with:\n"
        "    python3 -m pip install --user -r tools/requirements-assets.txt\n"
    )
    sys.exit(2)

REPO_ROOT = Path(__file__).resolve().parent.parent

BASELINE_REL = "data/language/concept_id_baseline.json"
CATALOGUE_REL = "data/language/concepts.yaml"

# Schema of the artifact this audit writes and reads. Version 1 recorded
# a bare sorted `ids` array; version 2 (#1868) records `concepts` as
# `{id, ordinal}` objects, because root assignment now places concepts in
# ordinal order. There is no reader for version 1 — the relocation from
# `docs/` to `data/` and the schema change landed together, so a version
# 1 artifact can only be a stale working copy.
BASELINE_VERSION = 2

UPDATE_COMMAND = "python3 tools/concept_id_inventory_audit.py --update-baseline"

# The rationale a removal diagnostic must carry (requirement 2). Kept as
# one constant so the self-test asserts the text the audit actually
# prints rather than a paraphrase of it.
IMMUTABLE_RATIONALE = (
    "A shipped concept id is IMMUTABLE: it may be added, never renamed,\n"
    "removed or reused. Language.Etymology reports a concept the catalogue no\n"
    "longer carries as EtyInvalidConcept, so every already-persisted\n"
    "EtymologySource naming one of these ids loses its etymology; and\n"
    "Language.Generated.Hash seeds each concept's native root from the id\n"
    "string, so a renamed id re-roots the concept in generated languages.\n"
    "A rename is both hazards at once, so it is reported twice: once as the\n"
    "missing historical id here, once as an unexpected new one.")

RATCHET_INSTRUCTION = (
    "Adding a concept id is a deliberate ratchet step, never an automatic\n"
    "pass. If these additions are intended, record them with:\n"
    f"    {UPDATE_COMMAND}\n"
    "which appends each new id after the highest recorded ordinal, leaving\n"
    "every existing concept's root exactly where it is.")


class AuditError(Exception):
    """An input this audit cannot compare — a missing or malformed file."""


# --------------------------------------------------------------------------
# Inputs
# --------------------------------------------------------------------------


def load_catalogue_ids(root: Path) -> list[str]:
    """Every `id` in the catalogue, in authored order.

    Parsed with PyYAML — the same library `pack_atlas.py` and
    `ci_parity_audit.py` use — rather than a line scanner, so the flow
    entries the catalogue is authored in and any future block form read
    identically.
    """
    path = root / CATALOGUE_REL
    try:
        text = path.read_text(encoding="utf-8")
    except OSError as error:
        raise AuditError(f"cannot read {CATALOGUE_REL}: {error}") from error
    try:
        document = yaml.safe_load(text)
    except yaml.YAMLError as error:
        raise AuditError(f"{CATALOGUE_REL} is not valid YAML: {error}") from error
    if not isinstance(document, dict):
        raise AuditError(f"{CATALOGUE_REL} is not a YAML mapping")
    entries = document.get("concepts")
    if not isinstance(entries, list):
        raise AuditError(f"{CATALOGUE_REL} has no `concepts:` list")
    ids: list[str] = []
    for index, entry in enumerate(entries):
        if not isinstance(entry, dict):
            raise AuditError(
                f"{CATALOGUE_REL} concept #{index + 1} is not a mapping")
        raw = entry.get("id")
        # YAML would happily hand back an int or a bool for an unquoted
        # scalar; an id is a string and nothing else, exactly as
        # Language.Semantic.Catalogue's validateId requires.
        if not isinstance(raw, str) or not raw:
            raise AuditError(
                f"{CATALOGUE_REL} concept #{index + 1} has no string `id:` "
                f"(got {raw!r})")
        ids.append(raw)
    if not ids:
        raise AuditError(f"{CATALOGUE_REL} declares no concepts")
    return ids


def load_baseline_ordinals(root: Path) -> dict[str, int]:
    """The recorded shipped inventory, as `{id: ordinal}`.

    Every structural rule the append-only sequence rests on is checked
    here rather than at the comparison, so a malformed artifact is one
    loud failure naming the offending entry instead of a comparison run
    against nonsense. `Language.Semantic.Catalogue` re-validates what
    root PLACEMENT needs (unique ids, unique ordinals, and agreement
    with the catalogue's id set); the contiguity rule below is this
    audit's alone, because it is a property of the RATCHET's writing
    rather than of reading an artifact.
    """
    path = root / BASELINE_REL
    try:
        text = path.read_text(encoding="utf-8")
    except OSError as error:
        raise AuditError(f"cannot read {BASELINE_REL}: {error}") from error
    try:
        document = json.loads(text)
    except json.JSONDecodeError as error:
        raise AuditError(f"{BASELINE_REL} is not valid JSON: {error}") from error
    if not isinstance(document, dict):
        raise AuditError(f"{BASELINE_REL} is not a JSON object")
    version = document.get("version")
    # `isinstance(True, int)` is True in Python, so bools are excluded
    # explicitly here and for every ordinal below.
    if isinstance(version, bool) or not isinstance(version, int):
        raise AuditError(
            f"{BASELINE_REL} has no integer `version` (got {version!r})")
    if version != BASELINE_VERSION:
        raise AuditError(
            f"{BASELINE_REL} is schema version {version}; this audit reads "
            f"version {BASELINE_VERSION}. Regenerate it with "
            f"`{UPDATE_COMMAND}`.")
    entries = document.get("concepts")
    if not isinstance(entries, list) or not entries:
        raise AuditError(f"{BASELINE_REL} has no non-empty `concepts` array")

    ordinals: dict[str, int] = {}
    by_ordinal: dict[int, str] = {}
    for index, entry in enumerate(entries):
        if not isinstance(entry, dict):
            raise AuditError(
                f"{BASELINE_REL} `concepts` entry #{index + 1} is not an "
                f"object (got {entry!r})")
        identifier = entry.get("id")
        if not isinstance(identifier, str) or not identifier:
            raise AuditError(
                f"{BASELINE_REL} `concepts` entry #{index + 1} has no string "
                f"`id` (got {identifier!r})")
        ordinal = entry.get("ordinal")
        if isinstance(ordinal, bool) or not isinstance(ordinal, int):
            raise AuditError(
                f"{BASELINE_REL} entry {identifier} has no integer `ordinal` "
                f"(got {ordinal!r})")
        if identifier in ordinals:
            raise AuditError(
                f"{BASELINE_REL} records {identifier} more than once")
        if ordinal in by_ordinal:
            raise AuditError(
                f"{BASELINE_REL} gives ordinal {ordinal} to both "
                f"{by_ordinal[ordinal]} and {identifier}; an ordinal is a "
                f"placement position, so it identifies exactly one concept")
        ordinals[identifier] = ordinal
        by_ordinal[ordinal] = identifier

    expected = set(range(len(ordinals)))
    if set(by_ordinal) != expected:
        strays = sorted(set(by_ordinal) - expected)
        holes = sorted(expected - set(by_ordinal))
        raise AuditError(
            f"{BASELINE_REL} ordinals are not the append-only sequence "
            f"0..{len(ordinals) - 1}: "
            f"unexpected {strays}, missing {holes}. Ids are never removed and "
            f"each addition takes the next ordinal, so the recorded ordinals "
            f"are always exactly that range")
    return ordinals


def load_baseline_ids(root: Path) -> list[str]:
    """The recorded shipped inventory, in placement (ordinal) order."""
    ordinals = load_baseline_ordinals(root)
    return sorted(ordinals, key=lambda identifier: ordinals[identifier])


def write_baseline(root: Path, ordinals: dict[str, int]) -> None:
    path = root / BASELINE_REL
    path.parent.mkdir(parents=True, exist_ok=True)
    document = {
        "comment": [
            "GENERATED RATCHET — do not hand-edit; see "
            "tools/concept_id_inventory_audit.py.",
            "Every concept id data/language/concepts.yaml has ever shipped, "
            "with the ordinal Language.Generated.Root.assignRoots places it "
            "at. An id is immutable once it appears here: it may be added, "
            "never renamed, removed or reused, and its ordinal never moves.",
            "Ordinals are append-only: a new id takes the next one after the "
            "highest recorded, which is what leaves every existing concept's "
            "generated root exactly where it is (#1868). The 151 seeded "
            "ordinals are ascending-id rank, reproducing the placement order "
            "that predated the ordinal.",
            "This artifact records an id's presence, exact string and "
            "ordinal and nothing else, so the audit fails a removal, a "
            "rename and an unratcheted addition. Same-string reuse -- "
            "keeping an id while repurposing what it means -- is review "
            "policy, not something this artifact can detect: the authored "
            "English forms and the domain stay editable.",
            f"Record a deliberate addition with `{UPDATE_COMMAND}`, which "
            "refuses any run that would drop an id and never rewrites a "
            "recorded ordinal.",
        ],
        "version": BASELINE_VERSION,
    }
    ordered = sorted(ordinals, key=lambda i: (ordinals[i], i))
    # One concept per LINE rather than json.dumps' expanded nesting, so a
    # ratchet step reads as a single added line in review. Every value is
    # emitted through json.dumps, so this is formatting only -- the
    # result is ordinary JSON and load_baseline_ordinals reads it back
    # with a plain json.loads.
    head = json.dumps(document, indent=2, ensure_ascii=False)[:-2].rstrip()
    entries = ",\n".join(
        "    " + json.dumps({"id": identifier, "ordinal": ordinals[identifier]},
                            ensure_ascii=False)
        for identifier in ordered)
    path.write_text(f"{head},\n  \"concepts\": [\n{entries}\n  ]\n}}\n",
                    encoding="utf-8")


def extend_ordinals(recorded: dict[str, int],
                    added: list[str]) -> dict[str, int]:
    """`recorded`, plus each id in `added` at the next free ordinal.

    Every recorded pair is carried through unchanged — the append-only
    half of the ratchet — and several ids arriving in one update are
    assigned in ascending id order so the result does not depend on the
    order the catalogue happened to author them in.
    """
    extended = dict(recorded)
    next_ordinal = max(recorded.values()) + 1 if recorded else 0
    for identifier in sorted(added):
        extended[identifier] = next_ordinal
        next_ordinal += 1
    return extended


# --------------------------------------------------------------------------
# Comparison — the one core both the repository audit and the self-test run
# --------------------------------------------------------------------------


def duplicate_ids(ids: list[str]) -> list[str]:
    """Ids the catalogue declares more than once, in first-seen order.

    The inventory is a SET, so a duplicate makes "the shipped ids"
    ill-defined and would let a rename hide behind a collision. The
    Haskell loader rejects duplicates too (`DuplicateConceptId`); this
    keeps the comparison itself honest rather than trusting that.
    """
    seen: set[str] = set()
    repeated: list[str] = []
    for identifier in ids:
        if identifier in seen and identifier not in repeated:
            repeated.append(identifier)
        seen.add(identifier)
    return repeated


def compare_inventories(baseline: list[str],
                        catalogue: list[str]) -> tuple[list[str], list[str]]:
    """`(missing, added)` — baseline ids the catalogue dropped, and
    catalogue ids the baseline has never recorded."""
    baseline_set = set(baseline)
    catalogue_set = set(catalogue)
    missing = sorted(baseline_set - catalogue_set)
    added = sorted(catalogue_set - baseline_set)
    return missing, added


def _report_missing(missing: list[str]) -> None:
    print(f"  {len(missing)} recorded id(s) are ABSENT from "
          f"{CATALOGUE_REL}:")
    for identifier in missing:
        print(f"    {identifier}")
    print()
    for line in IMMUTABLE_RATIONALE.splitlines():
        print(f"  {line}")
    print()


def _report_added(added: list[str]) -> None:
    print(f"  {len(added)} id(s) in {CATALOGUE_REL} are NOT recorded in "
          f"{BASELINE_REL}:")
    for identifier in added:
        print(f"    {identifier}")
    print()
    for line in RATCHET_INSTRUCTION.splitlines():
        print(f"  {line}")
    print()


# --------------------------------------------------------------------------
# Entry points
# --------------------------------------------------------------------------


def run_repository_audit(root: Path = REPO_ROOT) -> int:
    try:
        catalogue = load_catalogue_ids(root)
        baseline = load_baseline_ids(root)
    except AuditError as error:
        print(f"concept id inventory audit: {error}")
        return 1

    repeated = duplicate_ids(catalogue)
    if repeated:
        print("CONCEPT ID INVENTORY UNREADABLE")
        print(f"  {CATALOGUE_REL} declares {len(repeated)} id(s) more than "
              f"once: {', '.join(repeated)}")
        print("  Concept ids are identities, so the shipped inventory is a "
              "set; a\n  duplicate makes it ill-defined and would let a "
              "rename hide behind a\n  collision.")
        return 1

    missing, added = compare_inventories(baseline, catalogue)
    if not missing and not added:
        print(f"concept id inventory: {len(baseline)} shipped id(s) intact "
              f"with ordinals 0..{len(baseline) - 1} "
              f"({CATALOGUE_REL} carries {len(catalogue)})")
        return 0

    print("CONCEPT ID INVENTORY BROKEN")
    if missing:
        _report_missing(missing)
    if added:
        _report_added(added)
    return 1


def run_update_baseline(root: Path = REPO_ROOT) -> int:
    try:
        catalogue = load_catalogue_ids(root)
    except AuditError as error:
        print(f"concept id inventory audit: {error}")
        return 1
    try:
        recorded = load_baseline_ordinals(root)
    except AuditError as error:
        # Seeding a brand-new artifact is the one case where no recorded
        # inventory is not itself the failure.
        if not (root / BASELINE_REL).exists():
            recorded = {}
        else:
            print(f"concept id inventory audit: {error}")
            return 1
    baseline = sorted(recorded, key=lambda identifier: recorded[identifier])

    repeated = duplicate_ids(catalogue)
    if repeated:
        print("REFUSED: cannot ratchet an ill-defined inventory")
        print(f"  {CATALOGUE_REL} declares {len(repeated)} id(s) more than "
              f"once: {', '.join(repeated)}")
        return 1

    missing, added = compare_inventories(baseline, catalogue)
    if missing:
        # The ratchet is monotonic. A removal — and therefore a rename,
        # which arrives as a removal plus an addition — is refused here
        # rather than recorded, so `--update-baseline` can never launder
        # a broken inventory into a passing one.
        print("REFUSED: --update-baseline records additions only")
        _report_missing(missing)
        if added:
            print("  The additions below were NOT recorded, because this run "
                  "would have\n  dropped the id(s) above:")
            for identifier in added:
                print(f"    {identifier}")
            print()
        print("  Restore the missing id(s) in "
              f"{CATALOGUE_REL} and run this again.")
        return 1

    if not added:
        print(f"concept id inventory: already records all {len(catalogue)} "
              f"catalogue id(s); nothing to ratchet")
        return 0

    extended = extend_ordinals(recorded, added)
    write_baseline(root, extended)
    print(f"concept id inventory: recorded {len(added)} new id(s) in "
          f"{BASELINE_REL}:")
    for identifier in sorted(added, key=lambda i: extended[i]):
        print(f"    {identifier} (ordinal {extended[identifier]})")
    return 0


# --------------------------------------------------------------------------
# Self-test — the same core, over synthetic trees
# --------------------------------------------------------------------------

_CATALOGUE_HEADER = ("# fixture catalogue\n"
                     "version: 1\n"
                     "concepts:\n")


def _catalogue(*entries: tuple[str, str, str]) -> str:
    """A catalogue authored in the shipped flow-mapping style.

    Each entry is `(id, domain, singular)`; the other three forms are
    derived so a fixture reads like the real file.
    """
    text = _CATALOGUE_HEADER
    for identifier, domain, singular in entries:
        text += (f"  - {{ id: {identifier}, domain: {domain}, "
                 f"singular: {singular}, plural: {singular}s, "
                 f"modifier: {singular}, "
                 f"possessive: \"{singular}'s\" }}\n")
    return text


_CLEAN_ENTRIES = (
    ("LAND", "place", "land"),
    ("MEMORY", "emotion", "memory"),
    ("WOLF", "creature", "wolf"),
)


def _materialize(root: Path, tree: dict[str, str]) -> None:
    for rel, content in tree.items():
        path = root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


def _run(tree: dict[str, str], update: bool = False) -> tuple[int, str]:
    """Run the audit's real entry point against a synthetic tree.

    The self-test drives `run_repository_audit` / `run_update_baseline`
    themselves — the same PyYAML parse, the same comparison, the same
    diagnostics — rather than a re-implementation, which is what makes a
    passing case here evidence about the shipped audit.
    """
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        _materialize(root, tree)
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            code = (run_update_baseline(root) if update
                    else run_repository_audit(root))
        text = buffer.getvalue()
        if update:
            written = root / BASELINE_REL
            text += "\n<<baseline>>\n"
            text += (written.read_text(encoding="utf-8") if written.exists()
                     else "<<absent>>")
        return code, text


_CLEAN_BASELINE_CACHE: str | None = None


def _clean_baseline_text() -> str:
    """The baseline the clean fixture ratchets to, produced by the
    audit's OWN writer rather than transcribed — so the clean cases
    prove that recording and checking agree."""
    global _CLEAN_BASELINE_CACHE
    if _CLEAN_BASELINE_CACHE is None:
        _, out = _run({CATALOGUE_REL: _catalogue(*_CLEAN_ENTRIES)},
                      update=True)
        _CLEAN_BASELINE_CACHE = out.split("<<baseline>>\n", 1)[1]
    return _CLEAN_BASELINE_CACHE


def _tree(*entries: tuple[str, str, str]) -> dict[str, str]:
    """The clean baseline, against a catalogue of `entries`."""
    return {CATALOGUE_REL: _catalogue(*entries),
            BASELINE_REL: _clean_baseline_text()}


def _self_test() -> list[str]:
    failures: list[str] = []

    def expect_clean(label: str, tree: dict[str, str]) -> None:
        code, out = _run(tree)
        if code != 0:
            failures.append(f"{label}: expected a clean pass, got exit "
                            f"{code}:\n{out}")

    def expect_fail(label: str, tree: dict[str, str], *needles: str,
                    update: bool = False) -> None:
        code, out = _run(tree, update=update)
        if code == 0:
            failures.append(f"{label}: expected a failure, got a clean pass:"
                            f"\n{out}")
            return
        for needle in needles:
            if needle not in out:
                failures.append(
                    f"{label}: output did not mention {needle!r}:\n{out}")

    # 0. The writer's own artifact is well-formed, and the seeded
    #    ordinals are ascending-id rank — the placement order that
    #    predated the ordinal, which is what made seeding root-neutral.
    document = json.loads(_clean_baseline_text())
    if document.get("version") != BASELINE_VERSION:
        failures.append(f"writer: unexpected schema version: "
                        f"{document.get('version')!r}")
    if document.get("concepts") != [{"id": "LAND", "ordinal": 0},
                                    {"id": "MEMORY", "ordinal": 1},
                                    {"id": "WOLF", "ordinal": 2}]:
        failures.append(f"writer: unexpected recorded concepts: "
                        f"{document.get('concepts')!r}")

    # 1. The tree the baseline was captured from passes.
    expect_clean("clean tree", _tree(*_CLEAN_ENTRIES))

    # 2. Requirement 2 — a REMOVAL fails, names the id, and carries the
    #    immutable-id rationale.
    expect_fail("removal",
                _tree(("LAND", "place", "land"), ("WOLF", "creature", "wolf")),
                "CONCEPT ID INVENTORY BROKEN",
                "1 recorded id(s) are ABSENT",
                "MEMORY",
                "A shipped concept id is IMMUTABLE",
                "EtyInvalidConcept",
                "Language.Generated.Hash")

    # 3. Requirement 3 — a RENAME fails as BOTH a removal and an
    #    addition, so arriving with a replacement entry does not
    #    launder it.
    renamed = _tree(("LAND", "place", "land"),
                    ("RECOLLECTION", "emotion", "memory"),
                    ("WOLF", "creature", "wolf"))
    expect_fail("rename", renamed,
                "CONCEPT ID INVENTORY BROKEN",
                "1 recorded id(s) are ABSENT",
                "MEMORY",
                "A shipped concept id is IMMUTABLE",
                "are NOT recorded in",
                "RECOLLECTION",
                UPDATE_COMMAND)
    # ...and update mode refuses it, naming both halves, so the ratchet
    #    cannot be used to bless one.
    expect_fail("rename under --update-baseline", renamed,
                "REFUSED: --update-baseline records additions only",
                "MEMORY",
                "A shipped concept id is IMMUTABLE",
                "RECOLLECTION",
                update=True)
    code, out = _run(renamed, update=True)
    if out.split("<<baseline>>\n", 1)[1] != _clean_baseline_text():
        failures.append("rename under --update-baseline: the artifact was "
                        f"rewritten anyway:\n{out}")

    # 4. Requirement 4 — an UNRATCHETED addition fails, and the message
    #    carries the ratchet instruction.
    added = _tree(*_CLEAN_ENTRIES, ("FROST", "element", "frost"))
    expect_fail("unratcheted addition", added,
                "CONCEPT ID INVENTORY BROKEN",
                "1 id(s) in",
                "FROST",
                "deliberate ratchet step",
                UPDATE_COMMAND)
    if "are ABSENT from" in _run(added)[1]:
        failures.append("unratcheted addition: reported a removal too")

    # 4b. ...and the ratchet accepts it, after which the tree passes.
    code, out = _run(added, update=True)
    if code != 0:
        failures.append(f"ratchet: --update-baseline refused an addition:"
                        f"\n{out}")
    else:
        ratcheted = dict(added)
        ratcheted[BASELINE_REL] = out.split("<<baseline>>\n", 1)[1]
        expect_clean("after ratcheting the addition", ratcheted)
        recorded = json.loads(ratcheted[BASELINE_REL]).get("concepts")
        # FROST sorts FIRST alphabetically and still lands LAST: the
        # ordinal is append-only, which is the whole point — an added id
        # must never take a placement position an incumbent already had.
        if recorded != [{"id": "LAND", "ordinal": 0},
                        {"id": "MEMORY", "ordinal": 1},
                        {"id": "WOLF", "ordinal": 2},
                        {"id": "FROST", "ordinal": 3}]:
            failures.append(f"ratchet: unexpected recorded concepts: "
                            f"{recorded!r}")
        # ...and a second run changes nothing at all.
        code, again = _run(ratcheted, update=True)
        if code != 0 or again.split("<<baseline>>\n", 1)[1] != \
                ratcheted[BASELINE_REL]:
            failures.append(f"ratchet: a repeat --update-baseline was not "
                            f"idempotent:\n{again}")

    # 4c. A REMOVAL is refused by the ratchet as well — the monotonicity
    #     that keeps --update-baseline from doubling as a pass button.
    expect_fail("removal under --update-baseline",
                _tree(("LAND", "place", "land"), ("WOLF", "creature", "wolf")),
                "REFUSED: --update-baseline records additions only",
                "MEMORY",
                update=True)

    # 5. Requirement 5 — re-wording any of the four forms PASSES. The
    #    etymology surface check compares the rebuilt native name and the
    #    stored gloss is passed through, so wording is display data.
    reworded = _tree(("LAND", "place", "land"),
                     ("MEMORY", "emotion", "recollection"),
                     ("WOLF", "creature", "wolf"))
    expect_clean("re-worded singular/plural/modifier/possessive", reworded)

    # 6. Requirement 5 — changing a `domain` PASSES. `ceDomain` has no
    #    consumer outside its type and parser and never reaches a save.
    expect_clean("changed domain",
                 _tree(("LAND", "place", "land"),
                       ("MEMORY", "mythic", "memory"),
                       ("WOLF", "creature", "wolf")))

    # 7. Reordering the catalogue PASSES: the inventory is a set, and
    #    authored order is editorial.
    expect_clean("reordered catalogue",
                 _tree(("WOLF", "creature", "wolf"),
                       ("LAND", "place", "land"),
                       ("MEMORY", "emotion", "memory")))

    # 8. A duplicated id makes the inventory ill-defined and fails
    #    before any comparison, in both modes.
    duplicated = _tree(*_CLEAN_ENTRIES, ("LAND", "place", "landmass"))
    expect_fail("duplicate id", duplicated,
                "CONCEPT ID INVENTORY UNREADABLE", "LAND")
    expect_fail("duplicate id under --update-baseline", duplicated,
                "REFUSED: cannot ratchet an ill-defined inventory", "LAND",
                update=True)

    # 9. Malformed inputs are a loud failure, never a vacuous pass.
    expect_fail("missing catalogue", {BASELINE_REL: _clean_baseline_text()},
                f"cannot read {CATALOGUE_REL}")
    expect_fail("missing baseline",
                {CATALOGUE_REL: _catalogue(*_CLEAN_ENTRIES)},
                f"cannot read {BASELINE_REL}")
    expect_fail("catalogue without a concepts list",
                {CATALOGUE_REL: "version: 1\n",
                 BASELINE_REL: _clean_baseline_text()},
                "has no `concepts:` list")
    expect_fail("non-string id",
                {CATALOGUE_REL: (_CATALOGUE_HEADER +
                                 "  - { id: 42, domain: place, "
                                 "singular: land }\n"),
                 BASELINE_REL: _clean_baseline_text()},
                "has no string `id:`")
    expect_fail("unparseable catalogue",
                {CATALOGUE_REL: "concepts: [\n",
                 BASELINE_REL: _clean_baseline_text()},
                "is not valid YAML")
    expect_fail("unparseable baseline",
                {CATALOGUE_REL: _catalogue(*_CLEAN_ENTRIES),
                 BASELINE_REL: "{\n"},
                "is not valid JSON")
    expect_fail("baseline without a concepts array",
                {CATALOGUE_REL: _catalogue(*_CLEAN_ENTRIES),
                 BASELINE_REL: f'{{"version": {BASELINE_VERSION}}}\n'},
                "has no non-empty `concepts` array")

    # 9b. #1868 — every ordinal-shape rule fails loudly and names the
    #     offending entry, in BOTH modes: a malformed artifact must never
    #     be silently ratcheted into a well-formed one.
    def with_baseline(document: object) -> dict[str, str]:
        return {CATALOGUE_REL: _catalogue(*_CLEAN_ENTRIES),
                BASELINE_REL: json.dumps(document) + "\n"}

    def ordinal_doc(*pairs: object) -> dict[str, object]:
        return {"version": BASELINE_VERSION, "concepts": list(pairs)}

    expect_fail("superseded schema version",
                with_baseline({"version": 1,
                               "ids": ["LAND", "MEMORY", "WOLF"]}),
                "is schema version 1", UPDATE_COMMAND)
    expect_fail("non-integer version",
                with_baseline({"version": "2", "concepts": []}),
                "has no integer `version`")
    expect_fail("entry without an ordinal",
                with_baseline(ordinal_doc({"id": "LAND", "ordinal": 0},
                                          {"id": "MEMORY"},
                                          {"id": "WOLF", "ordinal": 2})),
                "MEMORY", "has no integer `ordinal`")
    expect_fail("non-integer ordinal",
                with_baseline(ordinal_doc({"id": "LAND", "ordinal": 0},
                                          {"id": "MEMORY", "ordinal": "1"},
                                          {"id": "WOLF", "ordinal": 2})),
                "MEMORY", "has no integer `ordinal`")
    expect_fail("boolean ordinal",
                with_baseline(ordinal_doc({"id": "LAND", "ordinal": 0},
                                          {"id": "MEMORY", "ordinal": True},
                                          {"id": "WOLF", "ordinal": 2})),
                "MEMORY", "has no integer `ordinal`")
    duplicate_ordinal = with_baseline(
        ordinal_doc({"id": "LAND", "ordinal": 0},
                    {"id": "MEMORY", "ordinal": 0},
                    {"id": "WOLF", "ordinal": 2}))
    expect_fail("duplicate ordinal", duplicate_ordinal,
                "gives ordinal 0 to both", "LAND", "MEMORY")
    expect_fail("duplicate ordinal under --update-baseline",
                duplicate_ordinal, "gives ordinal 0 to both", update=True)
    expect_fail("duplicate baseline id",
                with_baseline(ordinal_doc({"id": "LAND", "ordinal": 0},
                                          {"id": "LAND", "ordinal": 1},
                                          {"id": "MEMORY", "ordinal": 2},
                                          {"id": "WOLF", "ordinal": 3})),
                "records LAND more than once")
    renumbered = with_baseline(ordinal_doc({"id": "LAND", "ordinal": 10},
                                           {"id": "MEMORY", "ordinal": 11},
                                           {"id": "WOLF", "ordinal": 12}))
    expect_fail("ordinals outside the append-only sequence", renumbered,
                "are not the append-only sequence 0..2")
    expect_fail("ordinals outside the sequence under --update-baseline",
                renumbered, "are not the append-only sequence 0..2",
                update=True)
    expect_fail("baseline entry that is not an object",
                with_baseline(ordinal_doc("LAND")),
                "entry #1 is not an object")

    # 9c. #1868 — several ids arriving in ONE update take consecutive
    #     ordinals in ascending id order, so the assignment does not
    #     depend on the order the catalogue authored them in, and every
    #     recorded pair survives verbatim.
    many = dict(_tree(*_CLEAN_ENTRIES,
                      ("ZEPHYR", "celestial", "zephyr"),
                      ("FROST", "element", "frost"),
                      ("OATH", "mythic", "oath")))
    code, out = _run(many, update=True)
    if code != 0:
        failures.append(f"multi-id ratchet: refused:\n{out}")
    else:
        recorded = json.loads(out.split("<<baseline>>\n", 1)[1])["concepts"]
        if recorded != [{"id": "LAND", "ordinal": 0},
                        {"id": "MEMORY", "ordinal": 1},
                        {"id": "WOLF", "ordinal": 2},
                        {"id": "FROST", "ordinal": 3},
                        {"id": "OATH", "ordinal": 4},
                        {"id": "ZEPHYR", "ordinal": 5}]:
            failures.append(f"multi-id ratchet: unexpected recorded "
                            f"concepts: {recorded!r}")
        # The same three ids authored in a different catalogue order must
        # ratchet to the identical ordinals.
        shuffled = dict(_tree(("OATH", "mythic", "oath"),
                              ("WOLF", "creature", "wolf"),
                              ("ZEPHYR", "celestial", "zephyr"),
                              ("LAND", "place", "land"),
                              ("FROST", "element", "frost"),
                              ("MEMORY", "emotion", "memory")))
        shuffled_out = _run(shuffled, update=True)[1]
        if json.loads(shuffled_out.split("<<baseline>>\n", 1)[1])["concepts"] \
                != recorded:
            failures.append("multi-id ratchet: the recorded ordinals "
                            "depended on the catalogue's authored order:\n"
                            f"{shuffled_out}")

    # 10. Seeding: --update-baseline writes a first artifact when none
    #     exists, and the resulting tree passes.
    code, out = _run({CATALOGUE_REL: _catalogue(*_CLEAN_ENTRIES)}, update=True)
    if code != 0:
        failures.append(f"seeding: --update-baseline refused:\n{out}")

    # 11. The REAL tree: the shipped catalogue and the checked-in
    #     artifact agree, and the artifact is not vacuous.
    try:
        shipped = load_catalogue_ids(REPO_ROOT)
        shipped_ordinals = load_baseline_ordinals(REPO_ROOT)
        recorded = load_baseline_ids(REPO_ROOT)
    except AuditError as error:
        failures.append(f"real tree: {error}")
    else:
        # #1868 requirement 2: the 151 seeded ordinals are ascending-id
        # RANK, which is the order the pre-ordinal `sort ids` placement
        # used. That identity is what made seeding produce byte-identical
        # roots for every existing language, so it is pinned here and not
        # merely described in the comment block.
        seeded = [identifier for identifier, ordinal
                  in sorted(shipped_ordinals.items(), key=lambda p: p[1])
                  if ordinal < 151]
        if len(seeded) != 151 or seeded != sorted(seeded):
            failures.append(
                "real tree: ordinals 0..150 are not the 151 seeded ids in "
                "ascending-id order, so the seeding no longer reproduces the "
                "placement order that predated the ordinal")
        if len(recorded) < 150:
            failures.append(f"real tree: {BASELINE_REL} records only "
                            f"{len(recorded)} id(s); the shipped catalogue "
                            f"has carried at least 150 since #713")
        for expected in ("LAND", "RIVER", "WOLF"):
            if expected not in recorded:
                failures.append(f"real tree: `{expected}` is not recorded in "
                                f"{BASELINE_REL}")
        strays = sorted(set(recorded) - set(shipped))
        if strays:
            failures.append(f"real tree: {BASELINE_REL} records id(s) the "
                            f"catalogue does not carry: {strays}")

    return failures


def main_self_test() -> int:
    failures = _self_test()
    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for failure in failures:
            print(f"  FAIL: {failure}")
        return 1
    print("concept_id_inventory_audit.py self-test: all checks passed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Audit that every concept id data/language/concepts.yaml "
                    "has shipped is still present, and that a new one was "
                    "ratcheted deliberately.")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--self-test", action="store_true",
                       help="run the audit's own fixture checks instead of "
                            "auditing the repository")
    group.add_argument("--update-baseline", action="store_true",
                       help=f"record newly added ids in {BASELINE_REL} "
                            f"(refuses any run that would drop one)")
    args = parser.parse_args()
    if args.self_test:
        return main_self_test()
    if args.update_baseline:
        return run_update_baseline()
    return run_repository_audit()


if __name__ == "__main__":
    raise SystemExit(main())
