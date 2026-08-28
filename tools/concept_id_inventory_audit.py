#!/usr/bin/env python3
"""Add-only inventory ratchet for the shipped concept catalogue (#1717).

`src/Language/Semantic/Types.hs` states the rule plainly — "Ids may be
added, never renamed or reused" — and until this audit nothing enforced
any part of it. This audit enforces THREE of those, not all of it:
removal, rename and unratcheted addition (see "What this audit cannot
see" below). A change could drop `MEMORY`, add a conforming
`RECOLLECTION`, and leave every existing check green: the headless
suite
(`Test.Headless.Language.Semantic`) pins the catalogue VERSION, a
minimum COUNT, the six domains, the 20-30 per-domain balance and the
presence of all four authored forms, and every one of those is an
aggregate that names no individual id.

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

Guarded: the PRESENCE of every id the catalogue has ever shipped, and
its exact STRING — so removal, rename and unratcheted addition fail.
That is the whole boundary this gate can hold; "never reused" is not
part of it (see below).

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

`docs/language/concept_id_baseline.json` records the sorted union of
every id the catalogue has shipped. Today that equals the current
inventory: the shipped set grew monotonically (54 -> 150 -> 151 ids) and
the current catalogue still contains every historical id, so the initial
artifact was seeded from it.

`--update-baseline` is the only way an addition passes, and it is
MONOTONIC: it records catalogue ids the artifact lacks, and REFUSES any
run that would drop an artifact id — so it can never double as a "make
the failure go away" button for a removal or a rename. A rename is
refused in update mode for exactly that reason.

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

BASELINE_REL = "docs/language/concept_id_baseline.json"
CATALOGUE_REL = "data/language/concepts.yaml"

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
    f"    {UPDATE_COMMAND}")


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


def load_baseline_ids(root: Path) -> list[str]:
    """The recorded shipped inventory."""
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
    ids = document.get("ids")
    if not isinstance(ids, list) or not ids:
        raise AuditError(f"{BASELINE_REL} has no non-empty `ids` array")
    for entry in ids:
        if not isinstance(entry, str) or not entry:
            raise AuditError(
                f"{BASELINE_REL} `ids` holds a non-string entry {entry!r}")
    return ids


def write_baseline(root: Path, ids: list[str]) -> None:
    path = root / BASELINE_REL
    path.parent.mkdir(parents=True, exist_ok=True)
    document = {
        "comment": [
            "GENERATED RATCHET — do not hand-edit; see "
            "tools/concept_id_inventory_audit.py.",
            "Every concept id data/language/concepts.yaml has ever shipped. "
            "An id is immutable once it appears here: it may be added, never "
            "renamed, removed or reused.",
            "This artifact records an id's presence and exact string and "
            "nothing else, so the audit fails a removal, a rename and an "
            "unratcheted addition. Same-string reuse -- keeping an id while "
            "repurposing what it means -- is review policy, not something "
            "this artifact can detect: the authored English forms and the "
            "domain stay editable.",
            f"Record a deliberate addition with `{UPDATE_COMMAND}`, which "
            "refuses any run that would drop an id.",
        ],
        "version": 1,
        "ids": sorted(ids),
    }
    path.write_text(json.dumps(document, indent=2, ensure_ascii=False) + "\n",
                    encoding="utf-8")


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
        baseline = load_baseline_ids(root)
    except AuditError as error:
        # Seeding a brand-new artifact is the one case where no recorded
        # inventory is not itself the failure.
        if not (root / BASELINE_REL).exists():
            baseline = []
        else:
            print(f"concept id inventory audit: {error}")
            return 1

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

    write_baseline(root, sorted(set(baseline) | set(catalogue)))
    print(f"concept id inventory: recorded {len(added)} new id(s) in "
          f"{BASELINE_REL}:")
    for identifier in added:
        print(f"    {identifier}")
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

    # 0. The writer's own artifact is well-formed and sorted.
    document = json.loads(_clean_baseline_text())
    if document.get("ids") != ["LAND", "MEMORY", "WOLF"]:
        failures.append(f"writer: unexpected recorded ids: "
                        f"{document.get('ids')!r}")

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
        recorded = json.loads(ratcheted[BASELINE_REL]).get("ids")
        if recorded != ["FROST", "LAND", "MEMORY", "WOLF"]:
            failures.append(f"ratchet: unexpected recorded ids: {recorded!r}")

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
    expect_fail("baseline without an ids array",
                {CATALOGUE_REL: _catalogue(*_CLEAN_ENTRIES),
                 BASELINE_REL: '{"version": 1}\n'},
                "has no non-empty `ids` array")

    # 10. Seeding: --update-baseline writes a first artifact when none
    #     exists, and the resulting tree passes.
    code, out = _run({CATALOGUE_REL: _catalogue(*_CLEAN_ENTRIES)}, update=True)
    if code != 0:
        failures.append(f"seeding: --update-baseline refused:\n{out}")

    # 11. The REAL tree: the shipped catalogue and the checked-in
    #     artifact agree, and the artifact is not vacuous.
    try:
        shipped = load_catalogue_ids(REPO_ROOT)
        recorded = load_baseline_ids(REPO_ROOT)
    except AuditError as error:
        failures.append(f"real tree: {error}")
    else:
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
