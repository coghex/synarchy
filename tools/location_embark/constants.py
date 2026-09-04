#!/usr/bin/env python3
"""The facts every owner behind `tools/location_embark_probe.py` shares.

Each name here is a single source: the world page the fixture is
generated on, the two save slots the three sessions hand between
themselves, and the two content identifiers the checks are written
against. They live in their own module so no owner can grow a second
spelling of one, and so the facade can re-export the two slot names its
own suppression messages have to print.
"""
from __future__ import annotations

PORTAL = "acolyte_portal"
RUIN_LABEL = "Small Ruin"  # data/locations/ruin_small.yaml `label`
# The world page phase 0 generates the fixture on. A load keeps every
# saved page's OWN id (#763: no main_world remap), so every session below
# must address this same page -- both are named from this one constant so
# they cannot drift apart again.
FIXTURE_PAGE = "ew"

#: The two save slots, named inside THIS invocation's own resource root
#: (below) rather than the developer's live one. Uniqueness is a
#: property of the complete path, not of the key: two invocations own
#: two different roots, so two runs can hold these same two keys without
#: touching each other's files — and a developer slot that happens to
#: share a name is in a third root entirely, never opened.
SAVE_BASE = "location_embark_base"     # portal-free fixture, loaded by (a) and (b)
SAVE_LOCAL = "location_embark_local"   # (b)'s own save, reloaded by (c)
