#!/usr/bin/env python3
"""Reject fruiting flora art that duplicates another stage in its family.

The harvest window can be gameplay-significant even when the annual stage is
only a texture change.  Discover every ``fruiting`` annual-cycle texture from
the shipped flora YAML, then compare its file bytes with every other PNG in
the same texture directory.  A duplicate makes the harvestable state visually
indistinguishable and fails the audit.

Usage:
  python3 tools/fruiting_texture_audit.py
  python3 tools/fruiting_texture_audit.py --self-test
"""
from __future__ import annotations

import argparse
import hashlib
import io
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import TextIO

import yaml


REPO_ROOT = Path(__file__).resolve().parent.parent


class AuditError(Exception):
    """The shipped declarations could not be audited safely."""


@dataclass(frozen=True)
class Duplicate:
    fruiting: Path
    sibling: Path
    species: tuple[str, ...]


def _relative_texture(root: Path, value: object, context: str) -> Path:
    if not isinstance(value, str) or not value:
        raise AuditError(f"{context}: expected a non-empty texture path")
    path = Path(value)
    if path.is_absolute() or ".." in path.parts:
        raise AuditError(f"{context}: texture path must stay repository-relative: {value}")
    return root / path


def discover_fruiting_textures(root: Path) -> dict[Path, set[str]]:
    """Map each declared fruiting PNG to the species that use it."""
    flora_dir = root / "data" / "flora"
    if not flora_dir.is_dir():
        raise AuditError(f"missing flora data directory: {flora_dir}")

    found: dict[Path, set[str]] = {}
    for yaml_path in sorted((*flora_dir.glob("*.yaml"), *flora_dir.glob("*.yml"))):
        try:
            document = yaml.safe_load(yaml_path.read_text(encoding="utf-8")) or {}
        except (OSError, yaml.YAMLError) as exc:
            raise AuditError(f"cannot read {yaml_path}: {exc}") from exc
        if not isinstance(document, dict):
            raise AuditError(f"{yaml_path}: top-level document must be a mapping")
        entries = document.get("flora", [])
        if not isinstance(entries, list):
            raise AuditError(f"{yaml_path}: top-level `flora` must be a list")

        for entry in entries:
            if not isinstance(entry, dict):
                raise AuditError(f"{yaml_path}: flora entry must be a mapping")
            cycle = entry.get("annualCycle") or []
            if not isinstance(cycle, list):
                raise AuditError(f"{yaml_path}: annualCycle must be a list")
            fruiting_stages = [
                stage for stage in cycle
                if isinstance(stage, dict) and stage.get("tag") == "fruiting"
            ]
            if not fruiting_stages:
                continue

            species = entry.get("name")
            if not isinstance(species, str) or not species:
                raise AuditError(f"{yaml_path}: fruiting flora entry has no name")
            tex_dir = _relative_texture(
                root, entry.get("texDir"), f"{yaml_path}:{species}:texDir"
            )
            for stage in fruiting_stages:
                texture = stage.get("texture")
                if not isinstance(texture, str) or not texture:
                    raise AuditError(
                        f"{yaml_path}:{species}:fruiting: expected a texture filename"
                    )
                fruiting_path = tex_dir / texture
                if not fruiting_path.is_file():
                    raise AuditError(
                        f"{yaml_path}:{species}:fruiting: missing {fruiting_path}"
                    )
                found.setdefault(fruiting_path, set()).add(species)

    if not found:
        raise AuditError(f"no fruiting annual-cycle textures found under {flora_dir}")
    return found


def audit(root: Path) -> tuple[dict[Path, set[str]], list[Duplicate]]:
    targets = discover_fruiting_textures(root)
    duplicates: list[Duplicate] = []
    for fruiting, species in sorted(targets.items()):
        fruiting_digest = hashlib.sha256(fruiting.read_bytes()).digest()
        for sibling in sorted(fruiting.parent.glob("*.png")):
            if sibling == fruiting or not sibling.is_file():
                continue
            if hashlib.sha256(sibling.read_bytes()).digest() == fruiting_digest:
                duplicates.append(
                    Duplicate(fruiting, sibling, tuple(sorted(species)))
                )
    return targets, duplicates


def run_audit(root: Path, stdout: TextIO, stderr: TextIO) -> int:
    try:
        targets, duplicates = audit(root)
    except (AuditError, OSError) as exc:
        print(f"FRUITING TEXTURE AUDIT FAILED: {exc}", file=stderr)
        return 1

    if duplicates:
        print("FRUITING TEXTURE AUDIT FAILED:", file=stderr)
        for duplicate in duplicates:
            fruiting = duplicate.fruiting.relative_to(root)
            sibling = duplicate.sibling.relative_to(root)
            species = ", ".join(duplicate.species)
            print(
                f"  {fruiting} ({species}) is byte-identical to {sibling}",
                file=stderr,
            )
        return 1

    species_count = sum(len(species) for species in targets.values())
    print(
        "OK — "
        f"{len(targets)} fruiting texture(s) for {species_count} flora declaration(s) "
        "are unique within their families",
        file=stdout,
    )
    return 0


def run_self_test() -> None:
    with tempfile.TemporaryDirectory(prefix="fruiting-texture-audit-") as tmp:
        root = Path(tmp)
        flora_dir = root / "data" / "flora"
        art_dir = root / "assets" / "textures" / "flora" / "fixture_berry"
        flora_dir.mkdir(parents=True)
        art_dir.mkdir(parents=True)
        (flora_dir / "fixture.yaml").write_text(
            """flora:
  - name: fixture_berry
    texDir: assets/textures/flora/fixture_berry
    annualCycle:
      - tag: fruiting
        texture: matured_fruiting.png
""",
            encoding="utf-8",
        )
        fruiting = art_dir / "matured_fruiting.png"
        sibling = art_dir / "matured.png"
        fruiting.write_bytes(b"same-png-bytes")
        sibling.write_bytes(b"same-png-bytes")

        targets, duplicates = audit(root)
        assert targets == {fruiting: {"fixture_berry"}}, targets
        assert duplicates == [
            Duplicate(fruiting, sibling, ("fixture_berry",))
        ], duplicates
        duplicate_stdout = io.StringIO()
        duplicate_stderr = io.StringIO()
        assert run_audit(root, duplicate_stdout, duplicate_stderr) == 1
        assert "is byte-identical to" in duplicate_stderr.getvalue()

        sibling.write_bytes(b"different-png-bytes")
        clean_stdout = io.StringIO()
        clean_stderr = io.StringIO()
        assert run_audit(root, clean_stdout, clean_stderr) == 0
        assert "are unique within their families" in clean_stdout.getvalue()
        assert not clean_stderr.getvalue()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--self-test", action="store_true", help="run isolated duplicate fixtures"
    )
    args = parser.parse_args(argv)

    if args.self_test:
        try:
            run_self_test()
        except (AssertionError, AuditError) as exc:
            print(f"SELF-TEST FAILED: {exc}", file=sys.stderr)
            return 1
        print("OK — fruiting texture audit self-test passed")
        return 0

    return run_audit(REPO_ROOT, sys.stdout, sys.stderr)


if __name__ == "__main__":
    raise SystemExit(main())
