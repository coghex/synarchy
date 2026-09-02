"""Runtime icon-family inventories and the global basename index (#1740,
split by #2142 requirement 10).

The ONE owner of the runtime side: parsing the panel's `ICON_SUBDIRS`
list and the startup loader's preload family list, and building the
ordered `basename -> supplying family` index over the retained families
exactly as `buildIconIndex` does — iterating the families IN ORDER and
assigning last, so a basename present in two families resolves to the
LAST one. Duplicate-basename evidence is returned beside the index rather
than dropped, and so is every family whose directory is missing; the
audit owner decides what each means.

`build_index`'s `restrict_to` exists for the self-test's family-local
comparison only: production always passes the whole ordered inventory,
so the shipped last-wins semantics are unchanged.

Requiring the two inventories to AGREE and requiring each family's
`<family>_unknown.png` fallback are audit-side judgements over what this
module returns; they live with the audit owner.

Consumes the shared leaf (`bare_name_icon_asset_core`) and, for the one
Lua lexer, `clean_lua` from the Lua owner — the inventories are Lua
source, and a second cleaner here would be the duplication requirement 20
forbids.
"""
from __future__ import annotations

import re
from pathlib import Path

from bare_name_icon_asset_core import ICON_ROOT, CheckError
from bare_name_icon_asset_lua import clean_lua


def _literal_list(body: str, label: str, what: str) -> list:
    names = re.findall(r"\"([^\"]*)\"|'([^']*)'", body)
    values = [a or b for a, b in names]
    residue = re.sub(r"\"[^\"]*\"|'[^']*'|[\s,]", "", body)
    if residue:
        raise CheckError(
            f"{label}: {what} is not a plain list of string literals "
            f"(unexpected {residue!r}); the extractor refuses rather than "
            f"guessing the family inventory")
    if not values:
        raise CheckError(f"{label}: {what} is empty")
    return values


def panel_families(root: Path, spec: dict) -> list:
    label = spec["path"]
    path = root / label
    if not path.is_file():
        raise CheckError(f"{label}: expected family inventory source is missing")
    cleaned = clean_lua(path.read_text(encoding="utf-8"), label)
    match = re.search(
        r"(?:local\s+)?" + re.escape(spec["name"]) + r"\s*=\s*\{(?P<body>[^}]*)\}",
        cleaned)
    if not match:
        raise CheckError(f"{label}: `{spec['name']}` was not found")
    return _literal_list(match.group("body"), label, f"`{spec['name']}`")


def loader_families(root: Path, spec: dict) -> list:
    label = spec["path"]
    path = root / label
    if not path.is_file():
        raise CheckError(f"{label}: expected family inventory source is missing")
    cleaned = clean_lua(path.read_text(encoding="utf-8"), label)
    calls = [m.start() for m in re.finditer(re.escape(spec["anchor"]), cleaned)]
    if len(calls) != 1:
        raise CheckError(
            f"{label}: expected exactly one `{spec['anchor']}` call, found "
            f"{len(calls)}; the extractor refuses rather than guessing which "
            f"one carries the icon family inventory")
    preceding = list(re.finditer(r"ipairs\(\s*\{(?P<body>[^}]*)\}\s*\)",
                                 cleaned[:calls[0]]))
    if not preceding:
        raise CheckError(
            f"{label}: no `ipairs({{...}})` family list precedes "
            f"`{spec['anchor']}`")
    return _literal_list(preceding[-1].group("body"), label,
                         "the icon preload family list")


def build_index(root: Path, families: list, restrict_to=None):
    """basename -> supplying family, mirroring buildIconIndex's last-wins order."""
    index, duplicates, missing_dirs = {}, [], []
    order = families if restrict_to is None else [restrict_to]
    for family in order:
        directory = root / ICON_ROOT / family
        if not directory.is_dir():
            missing_dirs.append(family)
            continue
        for entry in sorted(directory.iterdir()):
            if entry.suffix != ".png":
                continue
            basename = entry.stem
            if basename in index and index[basename] != family:
                duplicates.append((basename, index[basename], family))
            index[basename] = family
    return index, duplicates, missing_dirs
