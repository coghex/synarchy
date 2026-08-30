#!/usr/bin/env python3
"""Reject decoded-identical frames in YAML-declared vegetation families.

The renderer selects a vegetation variant by id, and the YAML registry maps
each id to a separate PNG path.  Distinct paths are not enough: two PNG files
can have different compression or metadata while decoding to the same visible
RGBA pixels.  Such a pair makes the runtime variant selection visually inert.

This audit reads the runtime inventory in ``data/vegetation/*.yaml``.  Every
family declaring at least two variants is checked pairwise after decoding to
RGBA.  Single-frame families naturally pass; ``tilled_soil`` deliberately uses
that contract (#333).

Usage:
  python3 tools/vegetation_variant_audit.py
Self-test:
  python3 tools/test_vegetation_variant_audit.py
Exit codes: 0 = no decoded-RGBA duplicate pairs, 1 = collision or invalid input.
"""
from __future__ import annotations

import sys
from itertools import combinations
from pathlib import Path

import yaml
from PIL import Image, UnidentifiedImageError


REPO_ROOT = Path(__file__).resolve().parent.parent
VEGETATION_DATA = Path("data/vegetation")


class AuditError(Exception):
    """The runtime inventory or one of its images could not be audited."""


def _shown(path: Path, repo_root: Path) -> str:
    try:
        return path.relative_to(repo_root).as_posix()
    except ValueError:
        return path.as_posix()


def _families(repo_root: Path) -> list[tuple[str, list[str]]]:
    data_dir = repo_root / VEGETATION_DATA
    yaml_paths = sorted(data_dir.glob("*.yaml")) + sorted(data_dir.glob("*.yml"))
    if not yaml_paths:
        raise AuditError(
            f"no vegetation registries found under {_shown(data_dir, repo_root)}"
        )

    families: list[tuple[str, list[str]]] = []
    seen_names: dict[str, Path] = {}
    for yaml_path in yaml_paths:
        try:
            document = yaml.safe_load(yaml_path.read_text(encoding="utf-8")) or {}
        except (OSError, yaml.YAMLError) as exc:
            raise AuditError(
                f"could not read {_shown(yaml_path, repo_root)}: {exc}"
            ) from exc

        entries = document.get("vegetation") if isinstance(document, dict) else None
        if not isinstance(entries, list):
            raise AuditError(
                f"{_shown(yaml_path, repo_root)} must contain a vegetation list"
            )

        for index, entry in enumerate(entries, 1):
            where = f"{_shown(yaml_path, repo_root)} vegetation entry {index}"
            if not isinstance(entry, dict):
                raise AuditError(f"{where} must be a mapping")
            name = entry.get("name")
            variants = entry.get("variants")
            if not isinstance(name, str) or not name:
                raise AuditError(f"{where} must name its family")
            if name in seen_names:
                earlier = _shown(seen_names[name], repo_root)
                raise AuditError(
                    f"vegetation family {name!r} is declared more than once "
                    f"({earlier} and {_shown(yaml_path, repo_root)})"
                )
            if (
                not isinstance(variants, list)
                or not variants
                or not all(isinstance(path, str) and path for path in variants)
            ):
                raise AuditError(f"{where} must declare one or more variant paths")
            seen_names[name] = yaml_path
            families.append((name, variants))

    return families


def _decoded_rgba(path: Path, repo_root: Path) -> tuple[tuple[int, int], bytes]:
    if not path.is_file():
        raise AuditError(f"missing vegetation frame: {_shown(path, repo_root)}")
    try:
        with Image.open(path) as source:
            source.load()
            rgba = source.convert("RGBA")
            return rgba.size, rgba.tobytes()
    except (OSError, UnidentifiedImageError) as exc:
        raise AuditError(
            f"could not decode vegetation frame {_shown(path, repo_root)}: {exc}"
        ) from exc


def audit(repo_root: Path) -> tuple[list[str], int, int]:
    families = _families(repo_root)
    collisions: list[str] = []
    multi_variant = 0

    for name, variants in families:
        if len(variants) < 2:
            continue
        multi_variant += 1
        decoded = [
            (_shown(repo_root / rel, repo_root), _decoded_rgba(repo_root / rel, repo_root))
            for rel in variants
        ]
        for (left_path, left), (right_path, right) in combinations(decoded, 2):
            if left == right:
                collisions.append(
                    f"vegetation family {name!r} has decoded-RGBA duplicate "
                    f"frames: {left_path} and {right_path}"
                )

    return collisions, len(families), multi_variant


def main(repo_root: Path = REPO_ROOT) -> int:
    try:
        collisions, family_count, multi_variant = audit(Path(repo_root))
    except AuditError as exc:
        print(f"ERROR: {exc}")
        return 1

    if collisions:
        for collision in collisions:
            print(f"ERROR: {collision}")
        return 1

    print(
        "OK — "
        f"{family_count} YAML-declared vegetation families checked "
        f"({multi_variant} multi-variant); no decoded-RGBA duplicate frames"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
