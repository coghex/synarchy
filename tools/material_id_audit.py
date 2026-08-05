#!/usr/bin/env python3
"""Material id/name correspondence audit (issue #1118).

`src/World/Material.hs` hardcodes the numeric material ids as compile-time
constants (`matGranite = MaterialId 1`); `data/materials/*.yaml`
independently declares the same materials with explicit ids (`- id: 1 /
name: granite`). Both tables run to 255 and nothing verified they agree.

The hazard is not that an insertion shifts later ids — ids are explicit and
sparse on both sides, so nothing renumbers automatically. It is that an
independently changed, reused, or newly assigned catalogue id leaves the
Haskell constants resolving the OLD or WRONG material: the constants are
literals, so nothing fails to compile and no test fails. 51 of the
constants have no consumer outside `Material.hs`, so a drift in those is
invisible to every code path as well as to the compiler.

The contract this enforces is one-to-one, both directions:

  * Every non-exempt `mat<Name> = MaterialId <n>` in `src/World/Material.hs`
    maps to exactly one catalogue entry, pairing `<Name>` converted from
    PascalCase to snake_case against the entry's `name` and `<n>` against
    its `id`.
  * Ids and canonical names are each unique on both sides. Without that,
    a duplicate catalogue entry or a colliding constant could mask a
    one-sided addition (and the runtime loader would not object: the
    registry fold overwrites the vector slot at an existing id).
  * `matAir = MaterialId 0` is the one deliberate Haskell-only constant —
    air is the empty-tile reservation, not a content material, so it has
    no catalogue entry by design. That exemption is declared in
    `EXEMPT_CONSTANTS` below rather than inferred, and it is checked for
    liveness: a stale exemption fails, as does a catalogue entry that
    claims an exempt name or id.

Catalogue discovery mirrors the runtime loader
(`Engine.Asset.YamlMaterials.loadMaterialDirectory`): the `data/materials`
directory only, non-recursive, `.yaml` and `.yml` both accepted even
though the checked-in catalogue currently uses only `.yaml`.

The catalogue parser here is a deliberately small, strict, standard-library
reader for this one flat file family — the CI image installs `python3`
without PyYAML, and this audit must not add an undeclared dependency.
Being strict is the point: an item missing `id`/`name`, an unexpected
nesting level, or an inconsistent list indent is reported rather than
silently skipped, so drift cannot hide behind a parse the audit shrugged
off. Vacuity is a failure too — zero constants, zero catalogue files, or
zero catalogue entries cannot pass.

Out of scope (see the issue): generating the constants from the YAML,
removing the unreferenced constants (CH-90), and the bare `MaterialId n`
literals in the test suites. This only detects disagreement.

Usage:
  python3 tools/material_id_audit.py
  python3 tools/material_id_audit.py --self-test
Exit codes: 0 = the two tables agree, 1 = they do not (or a self-test
check failed).
"""
from __future__ import annotations

import argparse
import re
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
MATERIAL_HS = REPO_ROOT / "src" / "World" / "Material.hs"
CATALOGUE_DIR = REPO_ROOT / "data" / "materials"

# Mirrors Engine.Asset.YamlMaterials.loadMaterialDirectory's `isYaml`.
CATALOGUE_EXTENSIONS = (".yaml", ".yml")

# The Haskell-only constants, with the id each is pinned to. Air is the
# empty-tile reservation rather than a content material, so it is
# deliberately absent from the content catalogue. Pinning the id keeps
# the exemption from being a wildcard: `matAir` moving off 0 still fails.
EXEMPT_CONSTANTS = {"matAir": 0}

# A top-level constant definition. Anchored at column 0 (a top-level
# Haskell binding) so the multi-name type signatures above each group
# (`matGranite, matDiorite, matGabbro ∷ MaterialId`) cannot match.
_CONSTANT_RE = re.compile(
    r"^(mat[A-Za-z0-9_']*)[ \t]*=[ \t]*MaterialId[ \t]+(\d+)[ \t]*$")

# A YAML list item (`  - id: 70`) and a plain `key: value` line.
_ITEM_RE = re.compile(r"^([ \t]*)-[ \t]*(.*)$")
_KEY_RE = re.compile(r"^([ \t]*)([A-Za-z_][A-Za-z0-9_]*)[ \t]*:[ \t]*(.*)$")


@dataclass(frozen=True)
class Constant:
    """A `mat<Name> = MaterialId <n>` binding in src/World/Material.hs."""
    identifier: str          # matBituminousCoal
    name: str                # bituminous_coal
    mat_id: int
    line: int

    def where(self) -> str:
        return f"src/World/Material.hs:{self.line}"


@dataclass(frozen=True)
class CatalogueEntry:
    """One `- id: / name:` entry in a data/materials/*.yaml file."""
    name: str
    mat_id: int
    source: str              # e.g. data/materials/carbonaceous.yaml
    line: int

    def where(self) -> str:
        return f"{self.source}:{self.line}"


def snake_case(pascal: str) -> str:
    """`BituminousCoal` -> `bituminous_coal`.

    The catalogue's canonical names are lower snake_case with no digits
    or acronym runs, so a boundary before each uppercase letter is exact
    for this vocabulary. An already-underscored segment is preserved
    rather than doubled."""
    out: list[str] = []
    for i, ch in enumerate(pascal):
        if ch.isupper() and i > 0 and out and out[-1] != "_":
            out.append("_")
        out.append(ch.lower())
    return "".join(out)


def strip_comment(line: str) -> str:
    """Drop a YAML `#` comment, honouring double quotes.

    The catalogue's quoted values are texture paths, which never contain
    `#`; the quote handling is here so a future one cannot be truncated
    into a silently different value."""
    in_quote = False
    for i, ch in enumerate(line):
        if ch == '"':
            in_quote = not in_quote
        elif ch == "#" and not in_quote:
            # `#` only starts a comment at the start of a token.
            if i == 0 or line[i - 1] in " \t":
                return line[:i]
    return line


def unquote(value: str) -> str:
    value = value.strip()
    if len(value) >= 2 and value[0] == value[-1] and value[0] in "\"'":
        return value[1:-1]
    return value


def parse_constants(text: str) -> tuple[list[Constant], list[str]]:
    """Every `mat* = MaterialId n` binding, plus structural problems."""
    constants: list[Constant] = []
    problems: list[str] = []
    for lineno, raw in enumerate(text.splitlines(), start=1):
        match = _CONSTANT_RE.match(raw)
        if match is None:
            continue
        identifier, digits = match.group(1), match.group(2)
        stem = identifier[len("mat"):]
        if not stem:
            problems.append(
                f"src/World/Material.hs:{lineno}: constant `{identifier}` has "
                "no name after the `mat` prefix")
            continue
        constants.append(Constant(identifier=identifier,
                                  name=snake_case(stem),
                                  mat_id=int(digits),
                                  line=lineno))
    return constants, problems


def parse_catalogue_file(text: str, source: str
                         ) -> tuple[list[CatalogueEntry], list[str]]:
    """The `materials:` list of one catalogue file.

    Strict by design: anything this reader does not recognise inside the
    list is reported rather than skipped, so a structural change cannot
    quietly shrink the set being compared."""
    entries: list[CatalogueEntry] = []
    problems: list[str] = []
    lines = text.splitlines()

    start: int | None = None
    for i, raw in enumerate(lines):
        stripped = strip_comment(raw)
        if not stripped.strip():
            continue
        key = _KEY_RE.match(stripped)
        if key is not None and key.group(1) == "" and key.group(2) == "materials":
            start = i + 1
            break
    if start is None:
        problems.append(f"{source}: no top-level `materials:` key")
        return entries, problems

    # Per-item accumulator: (fields, dash line number).
    fields: dict[str, tuple[str, int]] = {}
    item_line = 0
    dash_indent: int | None = None
    key_indent: int | None = None
    open_item = False

    def flush() -> None:
        nonlocal fields, open_item
        if not open_item:
            return
        entry_problems = []
        if "id" not in fields:
            entry_problems.append("no `id`")
        if "name" not in fields:
            entry_problems.append("no `name`")
        if entry_problems:
            problems.append(
                f"{source}:{item_line}: material entry has "
                + " and ".join(entry_problems))
        else:
            raw_id = fields["id"][0]
            if not re.fullmatch(r"\d+", raw_id):
                problems.append(
                    f"{source}:{fields['id'][1]}: `id: {raw_id}` is not a "
                    "non-negative integer")
            else:
                name = unquote(fields["name"][0])
                if not name:
                    problems.append(
                        f"{source}:{fields['name'][1]}: `name` is empty")
                else:
                    entries.append(CatalogueEntry(name=name,
                                                  mat_id=int(raw_id),
                                                  source=source,
                                                  line=item_line))
        fields = {}
        open_item = False

    for i in range(start, len(lines)):
        lineno = i + 1
        stripped = strip_comment(lines[i])
        if not stripped.strip():
            continue
        indent = len(stripped) - len(stripped.lstrip())
        item = _ITEM_RE.match(stripped)
        if item is None and indent == 0:
            # A sibling top-level key ends the materials list. Other
            # top-level keys are not this audit's business. A list item
            # is checked first because YAML lets a block sequence sit at
            # the same indent as the key introducing it.
            break
        if item is not None:
            this_indent = len(item.group(1))
            if dash_indent is None:
                dash_indent = this_indent
                key_indent = this_indent + 2
            elif this_indent != dash_indent:
                problems.append(
                    f"{source}:{lineno}: list item indented {this_indent}, "
                    f"expected {dash_indent} — inconsistent `materials:` list")
                continue
            flush()
            open_item = True
            item_line = lineno
            inline = _KEY_RE.match(item.group(2))
            if inline is None:
                problems.append(
                    f"{source}:{lineno}: list item is not a `key: value` "
                    "mapping")
            else:
                fields[inline.group(2)] = (inline.group(3).strip(), lineno)
            continue
        key = _KEY_RE.match(stripped)
        if key is None or not open_item or indent != key_indent:
            problems.append(
                f"{source}:{lineno}: unexpected line inside `materials:` "
                f"(expected a field at indent {key_indent}): "
                f"{stripped.strip()!r}")
            continue
        fields[key.group(2)] = (key.group(3).strip(), lineno)
    flush()

    return entries, problems


def discover_catalogue_files(directory: Path) -> list[Path]:
    """The catalogue files the runtime loader would read: this directory
    only (never a subdirectory), `.yaml` and `.yml` both."""
    if not directory.is_dir():
        return []
    return sorted(p for p in directory.iterdir()
                  if p.is_file() and p.suffix in CATALOGUE_EXTENSIONS)


def _duplicate_problems(constants: list[Constant],
                        entries: list[CatalogueEntry]) -> list[str]:
    """Uniqueness of ids and names on each side, independently."""
    problems: list[str] = []

    by_identifier: dict[str, Constant] = {}
    for const in constants:
        prior = by_identifier.get(const.identifier)
        if prior is not None:
            problems.append(
                f"`{const.identifier}` is defined twice ({prior.where()} and "
                f"{const.where()})")
        else:
            by_identifier[const.identifier] = const

    for label, key in (("id", "mat_id"), ("name", "name")):
        seen: dict[object, Constant] = {}
        for const in constants:
            value = getattr(const, key)
            prior = seen.get(value)
            if prior is not None and prior.identifier != const.identifier:
                problems.append(
                    f"two Haskell constants share {label} {value!r}: "
                    f"`{prior.identifier}` ({prior.where()}) and "
                    f"`{const.identifier}` ({const.where()})")
            else:
                seen[value] = const

    for label, key in (("id", "mat_id"), ("name", "name")):
        seen_entry: dict[object, CatalogueEntry] = {}
        for entry in entries:
            value = getattr(entry, key)
            prior_entry = seen_entry.get(value)
            if prior_entry is not None:
                problems.append(
                    f"two catalogue entries share {label} {value!r}: "
                    f"{prior_entry.where()} and {entry.where()}")
            else:
                seen_entry[value] = entry

    return problems


def _exemption_problems(constants: list[Constant],
                        entries: list[CatalogueEntry]) -> list[str]:
    """The declared Haskell-only constants are live, pinned, and really
    absent from the catalogue."""
    problems: list[str] = []
    by_identifier = {c.identifier: c for c in constants}
    for identifier, expected_id in sorted(EXEMPT_CONSTANTS.items()):
        const = by_identifier.get(identifier)
        if const is None:
            problems.append(
                f"exempt constant `{identifier}` is declared in "
                "EXEMPT_CONSTANTS but no longer defined in "
                "src/World/Material.hs — drop the stale exemption")
            continue
        if const.mat_id != expected_id:
            problems.append(
                f"exempt constant `{identifier}` is pinned to id "
                f"{expected_id} but {const.where()} assigns "
                f"{const.mat_id}")
        for entry in entries:
            if entry.name == const.name:
                problems.append(
                    f"catalogue entry {entry.where()} declares `{entry.name}`, "
                    f"but `{identifier}` is exempted as Haskell-only")
            elif entry.mat_id == const.mat_id:
                problems.append(
                    f"catalogue entry {entry.where()} claims id "
                    f"{entry.mat_id}, reserved by the exempt constant "
                    f"`{identifier}`")
    return problems


def _correspondence_problems(constants: list[Constant],
                             entries: list[CatalogueEntry]) -> list[str]:
    """The one-to-one pairing itself.

    Joined on both axes so each drift reports exactly once: a renumber
    surfaces on the name join, a rename on the id join, and a genuinely
    one-sided entry (sharing neither) on the leftovers."""
    problems: list[str] = []
    hs_by_name = {c.name: c for c in constants}
    hs_by_id = {c.mat_id: c for c in constants}
    yaml_by_name = {e.name: e for e in entries}
    yaml_by_id = {e.mat_id: e for e in entries}

    for mat_id in sorted(hs_by_id.keys() & yaml_by_id.keys()):
        const, entry = hs_by_id[mat_id], yaml_by_id[mat_id]
        if const.name != entry.name:
            problems.append(
                f"id {mat_id}: `{const.identifier}` ({const.where()}) means "
                f"`{const.name}`, catalogue says `{entry.name}` "
                f"({entry.where()})")

    for name in sorted(hs_by_name.keys() & yaml_by_name.keys()):
        const, entry = hs_by_name[name], yaml_by_name[name]
        if const.mat_id != entry.mat_id:
            problems.append(
                f"`{name}`: `{const.identifier}` = MaterialId {const.mat_id} "
                f"({const.where()}), catalogue id {entry.mat_id} "
                f"({entry.where()})")

    for const in sorted(constants, key=lambda c: c.mat_id):
        if const.name in yaml_by_name or const.mat_id in yaml_by_id:
            continue
        problems.append(
            f"`{const.identifier}` = MaterialId {const.mat_id} "
            f"({const.where()}) has no catalogue entry — add one to "
            "data/materials/, or exempt it deliberately")

    for entry in sorted(entries, key=lambda e: e.mat_id):
        if entry.name in hs_by_name or entry.mat_id in hs_by_id:
            continue
        problems.append(
            f"catalogue `{entry.name}` id {entry.mat_id} ({entry.where()}) "
            "has no Haskell constant — add "
            f"`mat{''.join(p.capitalize() for p in entry.name.split('_'))}` "
            "to src/World/Material.hs")

    return problems


def audit(material_hs: str,
          catalogue: list[tuple[str, str]]) -> list[str]:
    """The whole check over already-read sources.

    `catalogue` is [(display path, file text)] in discovery order. Pure,
    so `--self-test` drives the identical logic the repository run does."""
    problems: list[str] = []

    constants, const_problems = parse_constants(material_hs)
    problems.extend(const_problems)

    entries: list[CatalogueEntry] = []
    for source, text in catalogue:
        file_entries, file_problems = parse_catalogue_file(text, source)
        entries.extend(file_entries)
        problems.extend(file_problems)

    # Vacuity guards: a broken parser, an empty directory, or a moved
    # constant block must fail loudly rather than agree about nothing.
    if not catalogue:
        problems.append(
            "no catalogue files discovered in data/materials "
            f"({'/'.join(CATALOGUE_EXTENSIONS)}) — nothing to compare against")
    if not constants:
        problems.append(
            "no `mat* = MaterialId n` constants found in "
            "src/World/Material.hs — nothing to compare")
    if catalogue and not entries:
        problems.append(
            "catalogue files were discovered but declare no material "
            "entries — nothing to compare")
    if not constants or not entries:
        return problems

    problems.extend(_duplicate_problems(constants, entries))
    problems.extend(_exemption_problems(constants, entries))

    non_exempt = [c for c in constants if c.identifier not in EXEMPT_CONSTANTS]
    problems.extend(_correspondence_problems(non_exempt, entries))
    return problems


def run_repository_audit() -> int:
    if not MATERIAL_HS.is_file():
        print(f"material_id_audit: missing {MATERIAL_HS}", file=sys.stderr)
        return 1
    catalogue = [(str(p.relative_to(REPO_ROOT)), p.read_text(encoding="utf-8"))
                 for p in discover_catalogue_files(CATALOGUE_DIR)]
    problems = audit(MATERIAL_HS.read_text(encoding="utf-8"), catalogue)
    if problems:
        print(f"{len(problems)} material id/name correspondence problem(s):")
        for problem in problems:
            print(f"  FAIL: {problem}")
        print("\nsrc/World/Material.hs and data/materials/*.yaml are two "
              "hand-maintained copies of one table; they must agree by both "
              "id and name.")
        return 1
    constants, _ = parse_constants(MATERIAL_HS.read_text(encoding="utf-8"))
    files = discover_catalogue_files(CATALOGUE_DIR)
    entry_count = sum(
        len(parse_catalogue_file(p.read_text(encoding="utf-8"),
                                 str(p.relative_to(REPO_ROOT)))[0])
        for p in files)
    print(f"material id audit: {len(constants)} Haskell constants "
          f"({len(EXEMPT_CONSTANTS)} exempt) match {entry_count} catalogue "
          f"entries across {len(files)} file(s)")
    return 0


# --------------------------------------------------------------------------
# Self-test
#
# Synthetic fixtures only — the point is proving each drift the audit
# claims to catch really does fail it, and that the clean baseline really
# does pass. The repository run above is the separate, real check.
# --------------------------------------------------------------------------

_HS_CLEAN = """\
matAir ∷ MaterialId
matAir = MaterialId 0

matGranite, matDiorite ∷ MaterialId
matGranite = MaterialId 1
matDiorite = MaterialId 2

matBituminousCoal ∷ MaterialId
matBituminousCoal = MaterialId 71
"""

_YAML_CLEAN = """\
materials:
  - id: 1
    name: granite
    hardness: 0.9
  - id: 2
    name: diorite   # inline comment
    tile: "assets/textures/world/diorite/diorite.png"
"""

_YAML_CLEAN_B = """\
# a second catalogue file
materials:
  - id: 71
    name: bituminous_coal
"""

_CLEAN_CATALOGUE = [("a.yaml", _YAML_CLEAN), ("b.yaml", _YAML_CLEAN_B)]


def _self_test() -> list[str]:
    failures: list[str] = []

    def expect_clean(label: str, hs: str,
                     catalogue: list[tuple[str, str]]) -> None:
        problems = audit(hs, catalogue)
        if problems:
            failures.append(f"{label}: expected no problems, got {problems}")

    def expect_fail(label: str, hs: str, catalogue: list[tuple[str, str]],
                    needle: str) -> None:
        problems = audit(hs, catalogue)
        if not problems:
            failures.append(f"{label}: expected a failure, got none")
        elif not any(needle in p for p in problems):
            failures.append(
                f"{label}: expected a problem mentioning {needle!r}, "
                f"got {problems}")

    # 0. The clean fixture agrees — otherwise every case below is vacuous.
    expect_clean("clean fixture", _HS_CLEAN, _CLEAN_CATALOGUE)

    # 1. A renumbered catalogue id.
    expect_fail("catalogue id changed", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN.replace("- id: 1", "- id: 9")),
                 ("b.yaml", _YAML_CLEAN_B)],
                "catalogue id 9")
    # ...and the same drift authored on the Haskell side.
    expect_fail("Haskell id changed",
                _HS_CLEAN.replace("matGranite = MaterialId 1",
                                  "matGranite = MaterialId 9"),
                _CLEAN_CATALOGUE, "`matGranite` = MaterialId 9")

    # 2. A renamed material, either side.
    expect_fail("catalogue name changed", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN.replace("name: granite",
                                                "name: granitite")),
                 ("b.yaml", _YAML_CLEAN_B)],
                "catalogue says `granitite`")
    expect_fail("Haskell name changed",
                _HS_CLEAN.replace("matGranite = MaterialId 1",
                                  "matGranitite = MaterialId 1"),
                _CLEAN_CATALOGUE, "means `granitite`")

    # 3. One-sided entries, both directions.
    expect_fail("Haskell-only constant",
                _HS_CLEAN + "matSchist ∷ MaterialId\nmatSchist = MaterialId 43\n",
                _CLEAN_CATALOGUE, "`matSchist` = MaterialId 43")
    expect_fail("catalogue-only entry", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN + "  - id: 43\n    name: schist\n"),
                 ("b.yaml", _YAML_CLEAN_B)],
                "catalogue `schist` id 43")

    # 4. Collisions on one side, which would otherwise mask a one-sided
    #    addition once the dictionaries overwrite each other.
    expect_fail("duplicate Haskell id",
                _HS_CLEAN.replace("matDiorite = MaterialId 2",
                                  "matDiorite = MaterialId 1"),
                _CLEAN_CATALOGUE, "two Haskell constants share id 1")
    expect_fail("duplicate Haskell constant definition",
                _HS_CLEAN + "matGranite = MaterialId 1\n",
                _CLEAN_CATALOGUE, "`matGranite` is defined twice")
    expect_fail("duplicate catalogue id", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN),
                 ("b.yaml", _YAML_CLEAN_B.replace("- id: 71", "- id: 1"))],
                "two catalogue entries share id 1")
    expect_fail("duplicate catalogue name", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN),
                 ("b.yaml", _YAML_CLEAN_B.replace("name: bituminous_coal",
                                                  "name: granite"))],
                "two catalogue entries share name 'granite'")
    expect_fail("repeated identical catalogue pair", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN + "  - id: 1\n    name: granite\n"),
                 ("b.yaml", _YAML_CLEAN_B)],
                "two catalogue entries share")

    # 5. The matAir exemption: accepted as Haskell-only, but not a wildcard.
    if any("matAir" in p or "`air`" in p
           for p in audit(_HS_CLEAN, _CLEAN_CATALOGUE)):
        failures.append("matAir exemption: clean fixture flagged matAir")
    expect_fail("exempt constant removed",
                _HS_CLEAN.replace("matAir = MaterialId 0", ""),
                _CLEAN_CATALOGUE, "stale exemption")
    expect_fail("exempt constant renumbered",
                _HS_CLEAN.replace("matAir = MaterialId 0",
                                  "matAir = MaterialId 5"),
                _CLEAN_CATALOGUE, "pinned to id 0")
    expect_fail("catalogue claims the exempt name", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN + "  - id: 200\n    name: air\n"),
                 ("b.yaml", _YAML_CLEAN_B)],
                "exempted as Haskell-only")
    expect_fail("catalogue claims the exempt id", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN + "  - id: 0\n    name: vacuum\n"),
                 ("b.yaml", _YAML_CLEAN_B)],
                "reserved by the exempt constant")

    # 6. Vacuity: nothing discovered must fail, never pass.
    expect_fail("no catalogue files", _HS_CLEAN, [],
                "no catalogue files discovered")
    expect_fail("no constants", "-- nothing here\n", _CLEAN_CATALOGUE,
                "no `mat* = MaterialId n` constants found")
    expect_fail("catalogue files with no entries", _HS_CLEAN,
                [("a.yaml", "materials:\n")],
                "declare no material entries")

    # 7. Parse strictness: a malformed entry is reported, not skipped —
    #    otherwise a broken file would silently shrink the compared set.
    expect_fail("entry missing name", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN),
                 ("b.yaml", "materials:\n  - id: 71\n    hardness: 0.2\n")],
                "no `name`")
    expect_fail("entry missing id", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN),
                 ("b.yaml", "materials:\n  - name: bituminous_coal\n")],
                "no `id`")
    expect_fail("non-integer id", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN),
                 ("b.yaml", "materials:\n  - id: 7x\n    name: bituminous_coal\n")],
                "is not a non-negative integer")
    # ...but a legal reformat is not drift: YAML lets a block sequence sit
    # at the same indent as the key introducing it, and a later top-level
    # key still ends the list.
    expect_clean("zero-indent sequence", _HS_CLEAN,
                 [("a.yaml", _YAML_CLEAN),
                  ("b.yaml", "materials:\n- id: 71\n  name: bituminous_coal\n"
                             "version: 2\n")])
    expect_fail("no materials key", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN),
                 ("b.yaml", "minerals:\n  - id: 71\n    name: bituminous_coal\n")],
                "no top-level `materials:` key")
    expect_fail("unexpected nesting", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN),
                 ("b.yaml", "materials:\n  - id: 71\n    name: bituminous_coal\n"
                            "    yields:\n      chunk: coal\n")],
                "unexpected line inside `materials:`")

    # 8. The PascalCase -> snake_case pairing is a real conversion, not a
    #    case-insensitive compare.
    for pascal, expected in (("BituminousCoal", "bituminous_coal"),
                             ("SandyClayLoam", "sandy_clay_loam"),
                             ("Sand", "sand")):
        got = snake_case(pascal)
        if got != expected:
            failures.append(f"snake_case({pascal!r}): expected "
                            f"{expected!r}, got {got!r}")
    expect_fail("catalogue name unseparated", _HS_CLEAN,
                [("a.yaml", _YAML_CLEAN),
                 ("b.yaml", _YAML_CLEAN_B.replace("bituminous_coal",
                                                  "bituminouscoal"))],
                "catalogue says `bituminouscoal`")

    # 9. Discovery matches the runtime loader: both extensions, this
    #    directory only.
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        (root / "a.yaml").write_text("materials:\n", encoding="utf-8")
        (root / "b.yml").write_text("materials:\n", encoding="utf-8")
        (root / "notes.txt").write_text("ignore me\n", encoding="utf-8")
        (root / "nested").mkdir()
        (root / "nested" / "c.yaml").write_text("materials:\n", encoding="utf-8")
        found = [p.name for p in discover_catalogue_files(root)]
        if found != ["a.yaml", "b.yml"]:
            failures.append(
                "discovery: expected ['a.yaml', 'b.yml'] (both extensions, "
                f"non-recursive), got {found}")
        missing = root / "does-not-exist"
        if discover_catalogue_files(missing):
            failures.append("discovery: a missing directory should yield []")

    return failures


def main_self_test() -> int:
    failures = _self_test()
    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for failure in failures:
            print(f"  FAIL: {failure}")
        return 1
    print("material_id_audit.py self-test: all checks passed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Audit that src/World/Material.hs's mat* constants and "
                    "data/materials/*.yaml declare the same ids and names.")
    parser.add_argument("--self-test", action="store_true",
                        help="run the audit's own fixture checks instead of "
                             "auditing the repository")
    args = parser.parse_args()
    if args.self_test:
        return main_self_test()
    return run_repository_audit()


if __name__ == "__main__":
    raise SystemExit(main())
