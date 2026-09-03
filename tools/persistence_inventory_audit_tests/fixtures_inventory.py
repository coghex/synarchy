#!/usr/bin/env python3
"""Synthetic inventory documents shared across families (#2138).

`SYNTHETIC_INVENTORY_COMPLETE` classifies every field and module the
shared Haskell and Lua fixtures declare, so a group that wants a CLEAN
audit passes it; `SYNTHETIC_INVENTORY_MISSING_ONE` withholds exactly one
classification, so a group that wants a violation passes that. Both are
read by more than one family.

An inventory document only one family reads stays with that family. This
module imports no case owner (#2138 requirement 16).
"""
from __future__ import annotations



# Owner-scoped inventory: the `### EngineEnv` heading classifies the
# EngineEnv fixture fields, `### Lua persistence registry` classifies
# the Lua registry fixture -- matching the real inventory doc's scheme
# (docs/persistence_state_inventory.md), NOT the coarser `## N.`
# section number (several distinct owners can share one numbered
# section there, e.g. WorldManager/WorldState both under "## 3.").
SYNTHETIC_INVENTORY_COMPLETE = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | Exclude |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |

## 12. Test coverage map

### Test coverage map

| Component | Canonical inspection path | Round-trip assertion | Reset/rebuild assertion | Focused test |
|---|---|---|---|---|
| `lua.unit_ai` | fake path | fake probe | — | fake test |
"""

SYNTHETIC_INVENTORY_MISSING_ONE = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""
