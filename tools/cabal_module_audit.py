#!/usr/bin/env python3
"""Cabal library inventory audit (issue #972, CH-28).

`synarchy.cabal` uses an explicit module inventory, so a `src/**/*.hs`
file that never makes it into the `library` stanza is invisible to the
whole toolchain: GHC never compiles it, `-Wall -Werror` never lints it,
and `cabal sdist` never ships it. That rot is undetectable by every
other gate -- the build stays green precisely BECAUSE the file is not
part of it. CH-28 found four such modules (352 lines, one of them
carrying an unused import `-Werror` would have rejected on sight); this
audit is what keeps a fifth from accumulating.

Direction of the check: every module declared under `src/` must appear
in the library inventory. The converse (an inventory entry with no
file) needs no guard here -- it fails `cabal build all`, which is
already a blocking CI gate.

Membership alone is not enough (issue #1280). Cabal resolves a listed
name to exactly ONE path under `hs-source-dirs: src` -- `Existing` to
`src/Existing.hs` and nowhere else -- so a second file that *declares*
an already-listed name is still dead: it satisfies a name-membership
check while cabal never selects it. Two further checks close that hole:

  * DUPLICATE -- more than one source file resolves to the same module
    name. Cabal compiles at most one of them; the rest are dead files
    hidden behind a listed name.
  * MISMATCH -- a file's declaration contradicts its own path. The
    inventory can name the declaration (no such path exists) or the
    path (GHC rejects the file), so either way the file is unselectable.

A file's path identity is derived relative to the `source_root` handed
to the audit, never `REPO_ROOT`, so synthetic trees and any-cwd runs
behave identically to a repository run.

Scope: the top-level unnamed `library` stanza ONLY. The executable and
test-suite stanzas carry their own `other-modules` lists (`App.*`,
`Test.*`); folding those in would pad the inventory with names no
`src/` module has and could mask a genuinely unlisted library module.

Usage:
  python3 tools/cabal_module_audit.py
Exit codes: 0 = every src/ module is listed exactly once and agrees
with its path, 1 = one or more findings of any kind.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import NamedTuple

REPO_ROOT = Path(__file__).resolve().parent.parent
CABAL_PATH = REPO_ROOT / "synarchy.cabal"
SOURCE_ROOT = REPO_ROOT / "src"

# The two fields that together form a component's module inventory.
# `library` currently declares only `exposed-modules`; an absent
# `other-modules` is an empty set, never an error.
MODULE_FIELDS = ("exposed-modules", "other-modules")

# A cabal field key: `name:` at some indent. Module list entries never
# contain a colon, so this doubles as the "the list ended" sentinel --
# `default-extensions:` terminates `exposed-modules`, which is what
# keeps `FlexibleInstances`/`Strict`/`StrictData` out of the inventory.
_FIELD_RE = re.compile(r"^(\s*)([A-Za-z][A-Za-z0-9-]*)\s*:(.*)$")
# A stanza header sits at column 0. `library` alone is the main library;
# `library <name>` would be a sublibrary and is deliberately not ours.
_LIBRARY_HEADER_RE = re.compile(r"^library\s*$")
_MODULE_DECL_RE = re.compile(r"^module\s+([A-Za-z][A-Za-z0-9_.']*)", re.M)
_MODULE_TOKEN_RE = re.compile(r"[A-Za-z][A-Za-z0-9_.']*")


def strip_comment(line: str) -> str:
    """Drop a cabal `--` comment. Module names cannot contain `--`, so
    cutting at the first occurrence is safe for every line we parse."""
    idx = line.find("--")
    return line if idx < 0 else line[:idx]


def library_stanza_lines(cabal_text: str) -> list[str]:
    """The lines of the top-level unnamed `library` stanza.

    The stanza runs from its column-0 header to the next column-0
    non-blank, non-comment line (the following stanza header)."""
    lines = cabal_text.splitlines()
    start: int | None = None
    for i, line in enumerate(lines):
        if _LIBRARY_HEADER_RE.match(strip_comment(line).rstrip()):
            start = i
            break
    if start is None:
        return []
    body: list[str] = []
    for line in lines[start + 1:]:
        stripped = strip_comment(line).strip()
        if stripped and not line[:1].isspace():
            break
        body.append(line)
    return body


def parse_module_field(stanza_lines: list[str], field: str) -> list[str]:
    """Every module named by `field` within an already-scoped stanza.

    Handles the three shapes the real file uses: the first module
    sharing the field line (`exposed-modules: UPrelude`), `--` comment
    and blank lines interleaved with the list, and termination at the
    next field key rather than at a blank line or EOF."""
    modules: list[str] = []
    i, n = 0, len(stanza_lines)
    while i < n:
        head = _FIELD_RE.match(strip_comment(stanza_lines[i]))
        if head is None or head.group(2).lower() != field:
            i += 1
            continue
        indent = len(head.group(1))
        modules.extend(_MODULE_TOKEN_RE.findall(head.group(3)))
        i += 1
        while i < n:
            text = strip_comment(stanza_lines[i])
            if not text.strip():
                i += 1
                continue
            if _FIELD_RE.match(text):
                break
            if len(text) - len(text.lstrip()) <= indent:
                break
            modules.extend(_MODULE_TOKEN_RE.findall(text))
            i += 1
    return modules


def library_modules(cabal_text: str) -> set[str]:
    """The library stanza's complete module inventory."""
    stanza = library_stanza_lines(cabal_text)
    listed: set[str] = set()
    for field in MODULE_FIELDS:
        listed.update(parse_module_field(stanza, field))
    return listed


class SourceFile(NamedTuple):
    """One `src/**/*.hs` file, under both names it can be known by.

    `name` is the identity the inventory is checked against (the
    declaration when there is one). `path_name` is what cabal would
    resolve this path to under `hs-source-dirs: src`; the two agreeing
    is what makes the file selectable at all."""
    name: str
    path_name: str
    declared: str | None
    display: str


def declared_module_name(path: Path) -> str | None:
    """The module name a source file declares, or None when it declares
    none (a file cabal could not select by declaration either way)."""
    match = _MODULE_DECL_RE.search(path.read_text(encoding="utf-8"))
    return match.group(1) if match else None


def path_module_name(path: Path, source_root: Path) -> str:
    """The module name cabal resolves this path to, derived relative to
    the audited source root rather than REPO_ROOT so a synthetic tree
    anywhere on disk behaves exactly like the repository's own."""
    rel = path.relative_to(source_root).with_suffix("")
    return ".".join(rel.parts)


def collect_source_files(source_root: Path) -> list[SourceFile]:
    """Every source file, nested directories included, sorted by the
    name it is audited under then by path."""
    found: list[SourceFile] = []
    for path in sorted(source_root.rglob("*.hs")):
        declared = declared_module_name(path)
        path_name = path_module_name(path, source_root)
        try:
            display = str(path.relative_to(REPO_ROOT))
        except ValueError:
            display = str(path)
        found.append(SourceFile(declared or path_name, path_name,
                                declared, display))
    return sorted(found, key=lambda f: (f.name, f.display))


def collect_source_modules(source_root: Path) -> list[tuple[str, str]]:
    """(module name, repo-relative path) for every source file, nested
    directories included, sorted by module name."""
    return [(f.name, f.display) for f in collect_source_files(source_root)]


def audit(cabal_text: str,
          source_modules: list[tuple[str, str]]) -> list[tuple[str, str]]:
    """Every source module absent from the library inventory."""
    listed = library_modules(cabal_text)
    return [entry for entry in sorted(source_modules)
            if entry[0] not in listed]


def duplicate_modules(
        source_files: list[SourceFile]) -> list[tuple[str, list[str]]]:
    """(module name, every path claiming it) for each name more than one
    source file resolves to.

    Cabal selects exactly one path per listed name, so every path past
    the first is a file nothing compiles -- and a listed name is what
    hides it from the membership check."""
    by_name: dict[str, list[str]] = {}
    for f in source_files:
        by_name.setdefault(f.name, []).append(f.display)
    return [(name, sorted(paths))
            for name, paths in sorted(by_name.items()) if len(paths) > 1]


def path_mismatches(
        source_files: list[SourceFile]) -> list[tuple[str, str, str]]:
    """(path, declared name, path-derived name) for each file whose
    declaration contradicts its own path.

    A file declaring a name its path does not spell is unselectable
    either way: listing the declaration points cabal at a path that does
    not exist, and listing the path hands GHC a file whose header
    disagrees. Files with no declaration at all are excluded -- there is
    nothing to contradict, and the path is already their identity."""
    return [(f.display, f.declared, f.path_name)
            for f in source_files
            if f.declared is not None and f.declared != f.path_name]


def run(cabal_text: str, source_root: Path) -> int:
    source_files = collect_source_files(source_root)
    source_modules = [(f.name, f.display) for f in source_files]
    listed = library_modules(cabal_text)
    unlisted = audit(cabal_text, source_modules)
    duplicates = duplicate_modules(source_files)
    mismatches = path_mismatches(source_files)
    print(f"  library inventory: {len(listed)} module(s)")
    print(f"  {source_root.name}/ source tree: "
          f"{len(source_modules)} module(s)")

    # Every class of finding is reported before returning, so a tree
    # with both a duplicate and an unlisted module names both rather
    # than hiding one behind the other's exit.
    if duplicates:
        print(f"\n{len(duplicates)} module name(s) claimed by more than "
              f"one source file:")
        for name, paths in duplicates:
            print(f"  {name}")
            for path in paths:
                print(f"    {path}")
        print("\nCabal resolves each listed name to exactly one path, so "
              "every other file\nhere is never compiled. Rename or delete "
              "the impostor(s).")
    if mismatches:
        print(f"\n{len(mismatches)} source file(s) whose declaration "
              f"contradicts their path:")
        for path, declared, path_name in mismatches:
            print(f"  {path} declares `{declared}` "
                  f"but its path names `{path_name}`")
        print("\nCabal cannot select either name: the declared one has no "
              "such path, and\nthe path-derived one has a mismatched "
              "header. Make the two agree.")
    if unlisted:
        print(f"\n{len(unlisted)} source module(s) missing from "
              f"synarchy.cabal's library inventory:")
        for name, path in unlisted:
            print(f"  {name} ({path})")
        print("\nAdd each module to the library stanza's exposed-modules "
              "or other-modules,\nor delete the file if it is dead code.")
    if duplicates or mismatches or unlisted:
        return 1
    print("\nEvery library source module is listed in synarchy.cabal")
    return 0


def main() -> int:
    return run(CABAL_PATH.read_text(encoding="utf-8"), SOURCE_ROOT)


if __name__ == "__main__":
    raise SystemExit(main())
