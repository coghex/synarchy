"""Infection-YAML reference discovery for the bare-name icon gate (#1740,
split by #2142 requirement 7).

The ONE owner of the YAML side: configured source-directory discovery,
safe node-graph loading (compose, not safe_load, so every diagnostic
carries the REAL `file:line` the way every Lua one does), `icon:` scalar
extraction, non-empty-string validation, and zero-file / zero-reference
rejection.

PyYAML is imported here, at module top, because this is the module that
needs it — and the façade's import chain reaches this module at startup
on BOTH public invocations (the audit owner imports it unconditionally,
and the self-test's YAML cases reach `extract_yaml` through the audit),
so the dependency diagnostic below fires eagerly, with the same text and
exit status it always had, before either command does anything.

Consumes only the shared leaf (`bare_name_icon_asset_core`).
"""
from __future__ import annotations

from pathlib import Path

from bare_name_icon_asset_core import CheckError, Reference

try:
    import yaml  # type: ignore
except ImportError:  # pragma: no cover - exercised only on a bare toolchain
    raise SystemExit(
        "bare_name_icon_asset_check.py needs PyYAML to read "
        "data/infections/*.yaml.\n"
        "Install the pinned toolchain:\n"
        "    python3 -m pip install --user -r tools/requirements-assets.txt\n"
        "(PyYAML is already required by tools/pack_atlas.py and "
        "tools/ci_parity_audit.py, which `make ci` and CI both run, so this "
        "adds no new dependency.)")


def extract_yaml(root: Path, spec: dict) -> list:
    directory = root / spec["dir"]
    if not directory.is_dir():
        raise CheckError(
            f"{spec['dir']}: expected authoritative YAML directory is missing")
    files = sorted(p for p in directory.iterdir() if p.suffix in (".yaml", ".yml"))
    if not files:
        raise CheckError(
            f"{spec['dir']}: expected authoritative YAML source produced no "
            f"files; the extractor refuses rather than silently narrowing")
    references = []
    for path in files:
        label = str(path.relative_to(root))
        # compose, not safe_load: the NODE graph carries source marks, so a
        # missing basename can name the real `file:line` the way every Lua
        # diagnostic does.
        try:
            documents = list(yaml.compose_all(path.read_text(encoding="utf-8")))
        except yaml.YAMLError as error:
            raise CheckError(f"{label}: could not be parsed as YAML ({error})")
        found = []

        def walk(node):
            if isinstance(node, yaml.MappingNode):
                for key_node, value_node in node.value:
                    if (isinstance(key_node, yaml.ScalarNode)
                            and key_node.value == spec["key"]):
                        line = value_node.start_mark.line + 1
                        if (not isinstance(value_node, yaml.ScalarNode)
                                or value_node.tag != "tag:yaml.org,2002:str"
                                or not value_node.value.strip()):
                            raise CheckError(
                                f"{label}:{line}: `{spec['key']}:` must be a "
                                f"non-empty string basename")
                        found.append((value_node.value.strip(), line))
                    else:
                        walk(value_node)
            elif isinstance(node, yaml.SequenceNode):
                for item in node.value:
                    walk(item)

        for document in documents:
            walk(document)
        for value, line in found:
            references.append(
                Reference(value, label, line, f"{label} `{spec['key']}:`"))
    if not references:
        raise CheckError(
            f"{spec['dir']}: no `{spec['key']}:` scalars found; the extractor "
            f"refuses rather than silently narrowing coverage")
    return references
