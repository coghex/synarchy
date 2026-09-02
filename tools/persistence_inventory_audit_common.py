#!/usr/bin/env python3
"""Shared immutable definitions for the persistence-inventory audit
(issue #2124, the ownership split of tools/persistence_inventory_audit.py).

The one owner of every repository path and source-scope constant the
split modules share. It holds DATA only -- no scanner, no parser, no
policy, no filesystem read beyond the one import-time directory glob
that defines `COMPONENT_CODEC_FILES` -- which is what lets every other
owner import it without creating a cycle (the same convention
tools/save_compat_audit_common.py established for #2049).

Read these through the module, never `from ... import` them: a
self-test that rebinds a path on THIS module to point at a temporary
tree needs every consumer to see the rebinding, and a name bound into
another module at import time would silently keep pointing at the real
tree. Every consumer therefore spells them `common.<NAME>` at call
time.

Repository SCOPE lives here; the root-owner declarations
(`ROOT_RECORDS`) stay on the façade, tools/persistence_inventory_audit.py,
beside the orchestration that consumes them. See that façade's module
docstring for what the tool as a whole guards and how it is run.
"""
from __future__ import annotations

from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
INVENTORY_PATH = REPO_ROOT / "docs" / "persistence_state_inventory.md"
SCRIPTS_DIR = "scripts"

# The Haskell `saveComponentRegistry` list of `registerComponent <codec>`.
COMPONENT_REGISTRY_LIST_FILE = "src/World/Save/Component.hs"
# Where the codecs named in that list are defined (each built via
# `<codec> = componentCodec ComponentSpec { csComponent = <ident>, ... }`).
# GLOBBED, not a
# hand-maintained file list: a component added in a NEW file under the
# same directory (the convention every existing component follows) was
# otherwise invisible to this audit entirely -- `derive_registered_
# component_ids` simply never resolved its codec and silently dropped
# it from the registered set, so its missing `### Save components` row
# went unreported. Mirrors tools/save_compat_audit.py's identical fix,
# and `derive_registered_component_ids` now ALSO raises when a
# registered codec cannot be resolved anywhere, so a component defined
# somewhere else entirely still fails loudly instead of vanishing.
COMPONENT_CODEC_FILES = tuple(
    str(p.relative_to(REPO_ROOT))
    for p in sorted((REPO_ROOT / "src/World/Save/Component").glob("*.hs")))
# Where `<componentIdIdent> = ComponentId "<literal>"` bindings live.
COMPONENT_ID_TYPES_FILE = "src/World/Save/Component/Types.hs"
# Where the envelope wires in any non-gameplay-registry component
# (currently just `metadata`) as a direct component spec tuple.
COMPONENT_ENVELOPE_FILE = "src/World/Save/Envelope.hs"
