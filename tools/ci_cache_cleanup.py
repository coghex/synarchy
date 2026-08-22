#!/usr/bin/env python3
"""Preview or delete superseded GitHub Actions project-build caches.

The default invocation is deliberately read-only.  It lists master-scoped
``dist-v3`` caches and proposes only epochs older than the newest three in each
compatible key family.  Dependency-store caches and caches belonging to pull
request merge refs are never selected by the default command.

Legacy ``dist-v2`` caches are an explicit second selection.  They are useful as
the bootstrap fallback while the first ``dist-v3`` epoch is being seeded, so
``--include-legacy`` refuses to select them until a v3 project cache exists in
the same ref.  Actual deletion requires the additional ``--delete`` flag and
uses exact numeric cache IDs.

Examples:
  python3 tools/ci_cache_cleanup.py
  python3 tools/ci_cache_cleanup.py --include-legacy
  python3 tools/ci_cache_cleanup.py --include-legacy --delete
  python3 tools/ci_cache_cleanup.py --self-test
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from dataclasses import dataclass
from typing import Any


DEFAULT_REF = "refs/heads/master"
DEFAULT_KEEP = 3
CURRENT_RE = re.compile(
    r"^(?P<family>dist-v3-.+)-(?P<plan>[0-9a-f]{64})-epoch-"
    r"(?P<epoch>[0-9]+)$")
LEGACY_RE = re.compile(r"^dist-v2-")


class CleanupError(RuntimeError):
    """The cache inventory or requested deletion was not safe to process."""


@dataclass(frozen=True)
class CacheEntry:
    cache_id: int
    key: str
    ref: str
    size: int
    created_at: str
    last_accessed_at: str


@dataclass(frozen=True)
class Candidate:
    entry: CacheEntry
    reason: str


def _entry(raw: dict[str, Any]) -> CacheEntry:
    try:
        return CacheEntry(
            cache_id=int(raw["id"]),
            key=str(raw["key"]),
            ref=str(raw["ref"]),
            size=int(raw.get("sizeInBytes", 0)),
            created_at=str(raw.get("createdAt", "")),
            last_accessed_at=str(raw.get("lastAccessedAt", "")),
        )
    except (KeyError, TypeError, ValueError) as error:
        raise CleanupError(f"malformed cache-list entry: {raw!r}: {error}") from error


def list_caches(ref: str, repo: str | None) -> list[CacheEntry]:
    command = [
        "gh", "cache", "list", "--ref", ref, "--limit", "10000", "--json",
        "id,key,ref,sizeInBytes,createdAt,lastAccessedAt",
    ]
    if repo:
        command.extend(["--repo", repo])
    proc = subprocess.run(command, capture_output=True, text=True)
    if proc.returncode != 0:
        detail = (proc.stderr or proc.stdout).strip()
        raise CleanupError(f"gh cache list failed: {detail}")
    try:
        raw = json.loads(proc.stdout)
    except json.JSONDecodeError as error:
        raise CleanupError(f"gh cache list returned invalid JSON: {error}") from error
    if not isinstance(raw, list):
        raise CleanupError("gh cache list returned a non-list JSON document")
    return [_entry(item) for item in raw]


def candidates(entries: list[CacheEntry], ref: str, keep: int,
               include_legacy: bool) -> tuple[list[Candidate], list[str]]:
    if keep < 1:
        raise CleanupError("--keep must be at least 1")

    notes: list[str] = []
    current: dict[str, list[tuple[int, CacheEntry]]] = {}
    legacy: list[CacheEntry] = []
    for entry in entries:
        if entry.ref != ref:
            continue
        match = CURRENT_RE.fullmatch(entry.key)
        if match:
            current.setdefault(match.group("family"), []).append(
                (int(match.group("epoch")), entry))
        elif LEGACY_RE.match(entry.key):
            legacy.append(entry)

    selected: list[Candidate] = []
    for family, family_entries in sorted(current.items()):
        ordered = sorted(
            family_entries,
            key=lambda item: (item[0], item[1].created_at, item[1].cache_id),
            reverse=True,
        )
        for epoch, entry in ordered[keep:]:
            selected.append(Candidate(
                entry,
                f"epoch {epoch} is older than the newest {keep} compatible "
                f"snapshots for {family}"))

    if include_legacy:
        if current:
            for entry in legacy:
                selected.append(Candidate(
                    entry, "legacy dist-v2 snapshot; a dist-v3 replacement exists"))
        elif legacy:
            notes.append(
                "legacy dist-v2 caches were NOT selected because this ref has no "
                "successfully seeded dist-v3 cache yet")

    selected.sort(key=lambda item: (item.entry.key, item.entry.cache_id))
    return selected, notes


def _size(size: int) -> str:
    value = float(size)
    for unit in ("B", "KiB", "MiB", "GiB", "TiB"):
        if value < 1024 or unit == "TiB":
            return f"{value:.1f} {unit}"
        value /= 1024
    raise AssertionError("unreachable")


def delete_cache(cache_id: int, repo: str | None) -> None:
    command = ["gh", "cache", "delete", str(cache_id)]
    if repo:
        command.extend(["--repo", repo])
    proc = subprocess.run(command, capture_output=True, text=True)
    if proc.returncode != 0:
        detail = (proc.stderr or proc.stdout).strip()
        raise CleanupError(f"gh cache delete {cache_id} failed: {detail}")


def self_test() -> int:
    failures: list[str] = []

    def check(condition: bool, message: str) -> None:
        if not condition:
            failures.append(message)

    def cache(cache_id: int, key: str, ref: str = DEFAULT_REF,
              size: int = 10) -> CacheEntry:
        stamp = f"2026-08-{cache_id:02d}T00:00:00Z"
        return CacheEntry(cache_id, key, ref, size, stamp, stamp)

    plan = "a" * 64
    family = f"dist-v3-Linux-ghc-9.12.2-cabal-3.16.1.0-{plan}"
    inventory = [
        cache(1, f"{family}-epoch-1"),
        cache(2, f"{family}-epoch-2"),
        cache(3, f"{family}-epoch-3"),
        cache(4, f"{family}-epoch-4"),
        cache(5, f"dist-v2-Linux-ghc-9.12.2-cabal-3.16.1.0-{plan}"),
        cache(6, "deps-v2-Linux-ghc9.12.2-plan"),
        cache(7, f"{family}-epoch-0", "refs/pull/7/merge"),
    ]
    chosen, notes = candidates(inventory, DEFAULT_REF, 3, False)
    check([item.entry.cache_id for item in chosen] == [1],
          f"default must select only the fourth-newest master epoch, got {chosen}")
    check(not notes, f"ordinary epoch cleanup needs no note, got {notes}")

    chosen, notes = candidates(inventory, DEFAULT_REF, 3, True)
    check([item.entry.cache_id for item in chosen] == [5, 1],
          f"legacy opt-in must add v2 but not deps/PR caches, got {chosen}")
    check(not notes, f"seeded legacy cleanup needs no refusal, got {notes}")

    legacy_only = [cache(8, "dist-v2-Linux-old")]
    chosen, notes = candidates(legacy_only, DEFAULT_REF, 3, True)
    check(not chosen, "legacy must be retained until a v3 replacement exists")
    check(len(notes) == 1 and "NOT selected" in notes[0],
          f"legacy refusal must be explicit, got {notes}")

    try:
        candidates(inventory, DEFAULT_REF, 0, False)
    except CleanupError as error:
        check("at least 1" in str(error), f"invalid keep diagnostic: {error}")
    else:
        failures.append("--keep 0 must fail")

    if failures:
        print("ci_cache_cleanup self-test: FAILED")
        for failure in failures:
            print(f"  - {failure}")
        return 1
    print("ci_cache_cleanup self-test: all cases pass")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Dry-run-first cleanup for superseded project-build caches.")
    parser.add_argument("--repo", help="OWNER/REPO (default: current gh repo)")
    parser.add_argument("--ref", default=DEFAULT_REF,
                        help=f"exact cache ref (default: {DEFAULT_REF})")
    parser.add_argument("--keep", type=int, default=DEFAULT_KEEP,
                        help=f"v3 epochs to retain per family (default: {DEFAULT_KEEP})")
    parser.add_argument("--include-legacy", action="store_true",
                        help="also select dist-v2 after a v3 cache has been seeded")
    parser.add_argument("--delete", action="store_true",
                        help="delete the exact IDs shown; omitted means dry run")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args(argv)

    if args.self_test:
        return self_test()
    try:
        inventory = list_caches(args.ref, args.repo)
        selected, notes = candidates(
            inventory, args.ref, args.keep, args.include_legacy)
    except CleanupError as error:
        print(f"ci_cache_cleanup: {error}", file=sys.stderr)
        return 1

    mode = "DELETE" if args.delete else "DRY RUN"
    print(f"CI cache cleanup: {mode}; ref={args.ref}; keep={args.keep}")
    for note in notes:
        print(f"NOTE: {note}")
    if not selected:
        print("No caches selected.")
        return 0

    total = sum(item.entry.size for item in selected)
    for item in selected:
        entry = item.entry
        print(
            f"id={entry.cache_id} size={_size(entry.size)} "
            f"last_accessed={entry.last_accessed_at or '-'} key={entry.key}\n"
            f"  reason: {item.reason}")
    print(f"Selected {len(selected)} cache(s), {_size(total)} total.")

    if not args.delete:
        print("Dry run only. Re-run with --delete to remove these exact IDs.")
        return 0

    for item in selected:
        try:
            delete_cache(item.entry.cache_id, args.repo)
        except CleanupError as error:
            print(f"ci_cache_cleanup: {error}", file=sys.stderr)
            return 1
        print(f"Deleted cache id={item.entry.cache_id} key={item.entry.key}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
