"""Repository scanning for the enum append-only audit.

The one owner of the GUARDED SET: source enumeration, per-module
declaration collection, duplicate detection, and the three-condition
discovery rule stated in `tools/enum_append_only_audit.py`'s module
docstring. Wire-carrier attribution reads this scan and never feeds it —
reachability is a diagnostic and must not add or remove a guarded type.
"""
from __future__ import annotations

import re
from pathlib import Path

from enum_append_only_audit_model import (
    SOURCE_DIRS,
    AuditError,
    GuardedType,
    Scan,
)
from enum_append_only_audit_parse import (
    declaration_blocks,
    module_name_of,
    parse_constructors,
    parse_declaration,
    qualifies_as_guarded,
    strip_haskell_comments,
)


def iter_source_files(root: Path) -> list[Path]:
    files: list[Path] = []
    for directory in SOURCE_DIRS:
        base = root / directory
        if base.is_dir():
            files.extend(sorted(base.rglob("*.hs")))
    return files


def scan_repository(root: Path) -> Scan:
    """Parse every shipped Haskell module, and pick out the guarded sums."""
    guarded: dict[str, GuardedType] = {}
    declarations: list[Declaration] = []
    module_paths: dict[str, str] = {}
    for path in iter_source_files(root):
        rel = path.relative_to(root).as_posix()
        text = strip_haskell_comments(path.read_text(encoding="utf-8"))
        module = module_name_of(rel, text)
        module_paths[module] = rel
        standalone = re.search(
            r"^deriving[ \t]+.*(?<![A-Za-z0-9_'])Serialize(?![A-Za-z0-9_'])",
            text, re.M)
        if standalone:
            raise AuditError(
                f"{rel}: standalone `deriving ... Serialize` is not a form "
                f"this audit can classify — it attaches an instance to a "
                f"type whose own declaration carries no evidence of it")
        for line, block in declaration_blocks(text):
            decl = parse_declaration(rel, module, line, block)
            declarations.append(decl)
            if not qualifies_as_guarded(decl):
                continue
            constructors = parse_constructors(decl)
            if len(constructors) < 2:
                continue
            if decl.qualified in guarded:
                raise AuditError(
                    f"{decl.where()}: `{decl.qualified}` is declared twice")
            guarded[decl.qualified] = GuardedType(
                module=decl.module, name=decl.name, rel_path=decl.rel_path,
                line=decl.line, constructors=constructors)
    return Scan(guarded=guarded, declarations=declarations,
                module_paths=module_paths)
