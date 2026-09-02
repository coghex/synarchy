#!/usr/bin/env python3
"""Shared immutable definitions for the save-compatibility tool (issue
#2049, requirement 4).

The one owner of every repository path, manifest/fixture location,
source-path inventory and codec constant the split modules share. It
holds DATA only -- no scanner, no parser, no subprocess, no policy --
which is what lets every other owner import it without creating a cycle
(requirement 15: "common definitions may be imported by every owner").

Read these through the module, never `from ... import` them: the
self-test rebinds `HASKELL_COMPONENT_SOURCE_PATHS`,
`COMPONENT_REGISTRY_SOURCE_PATH` and `MANIFEST_PATH` on THIS module to
point its synthetic trees at a temporary directory, and a name bound
into another module at import time would silently keep pointing at the
real tree -- which for `MANIFEST_PATH` means rewriting the real tracked
`docs/save_compat/manifest.json`. Every consumer therefore spells them
`common.<NAME>` at call time, and every default argument that used to
carry one of these values is a `None` sentinel resolved in the body.

The public façade is tools/save_compat_audit.py; see its module
docstring for what the tool as a whole guards and how it is run.
"""
from __future__ import annotations

from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
MANIFEST_PATH = REPO_ROOT / "docs" / "save_compat" / "manifest.json"
FIXTURE_DATA_DIR = REPO_ROOT / "test-headless" / "data" / "save-compat"
ENVELOPE_SOURCE_PATH = REPO_ROOT / "src" / "World" / "Save" / "Envelope.hs"
SESSION_V90_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Compat" / "SessionV90.hs")
ENVELOPE_TYPES_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Envelope" / "Types.hs")
ENVELOPE_CODEC_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Envelope" / "Codec.hs")

# The cabal file whose `common lang` stanza supplies every extension the
# library's modules -- Codec.hs among them -- already get without
# declaring anything locally (issue #1416). Deliberately NOT a
# hard-coded copy of that list: a hard-coded one that outlived a stanza
# edit would keep treating a now-EFFECTIVE local declaration as
# redundant, which is exactly the false negative
# envelope_framing_fingerprint must not have.
CABAL_PATH = REPO_ROOT / "synarchy.cabal"

# The ONE authoritative list of Haskell-owned gameplay components (round-
# 16 review): World.Save.Component.saveComponentRegistry itself, not a
# hand-maintained guess at which files declare them.
COMPONENT_REGISTRY_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Component.hs")

# Every source file that MIGHT declare a Haskell-owned gameplay
# component's ComponentId literal and/or its ComponentCodec (built
# through `componentCodec ComponentSpec { ... }`) -- see
# real_component_registry(). Round-16 review: previously a hand-
# maintained fixed list of exactly 4 files, so a brand-new component
# added in a NEW file under this same directory (the established
# convention every existing component already follows) was invisible to
# this audit with no error raised at all. Globbing the directory
# `saveComponentRegistry` itself draws every codec from means a new file
# is picked up automatically; real_component_registry() ALSO cross-
# checks every codec name saveComponentRegistry actually references
# against what this scan found, so even a component defined somewhere
# ELSE entirely still fails loudly instead of silently vanishing.
HASKELL_COMPONENT_SOURCE_PATHS = sorted(
    (REPO_ROOT / "src" / "World" / "Save" / "Component").glob("*.hs"))

# Fixed placeholder ISO-8601 timestamp (round-11 review), matching the
# same constant test-headless/Test/Headless/World/Save/Compat.hs's own
# hand-built SaveMetadata values already use -- NOT a real save time,
# deliberately, so two --generate-session runs over identical inputs
# produce byte-identical fixtures/checksums.
FIXED_GENERATED_TIMESTAMP = "2026-07-16T00:00:00.000000Z"
