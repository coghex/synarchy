"""Shared model support for the world-audit family (#2224).

The constants, result types and tile-neighbourhood helpers every other
world_audit module builds on. This is the bottom of the dependency
order: it imports no check owner, no classification policy, no command
façade, no self-test module, and no world_audit consumer, so a check
owner can depend on it without a cycle.

Not a command. `tools/world_audit.py` re-exports everything here, and
that façade is what consumers import.
"""
from __future__ import annotations

import math
from collections import Counter
from dataclasses import dataclass, field
from typing import Any


# ----- Constants -----------------------------------------------------------

SEA_LEVEL = 0  # Must match World/Constants.hs
CHUNK_SIZE = 16  # Must match World/Chunk/Types.hs
INT64_MIN = -(2**63)


# ----- Data types ----------------------------------------------------------

@dataclass
class Issue:
    category: str
    x: int
    y: int
    details: str

    def to_dict(self) -> dict[str, Any]:
        return {"x": self.x, "y": self.y, "details": self.details}


@dataclass
class AuditResult:
    seed: int | None = None
    world_size: int | None = None
    region: tuple[int, int, int, int] | None = None
    tile_count: int = 0
    fluid_stats: dict[str, int] = field(default_factory=dict)
    elevation_stats: dict[str, Any] = field(default_factory=dict)
    issues: list[Issue] = field(default_factory=list)

    def summary(self) -> dict[str, int]:
        counts: Counter[str] = Counter()
        for issue in self.issues:
            counts[issue.category] += 1
        return dict(sorted(counts.items()))

    def to_dict(self) -> dict[str, Any]:
        # Sort issues for stable output: by category, then by (x, y)
        sorted_issues = sorted(self.issues, key=lambda i: (i.category, i.x, i.y))
        # Group by category
        grouped: dict[str, list[dict[str, Any]]] = {}
        for issue in sorted_issues:
            grouped.setdefault(issue.category, []).append(issue.to_dict())
        return {
            "seed": self.seed,
            "worldSize": self.world_size,
            "region": list(self.region) if self.region else None,
            "tileCount": self.tile_count,
            "fluidStats": dict(sorted(self.fluid_stats.items(),
                                       key=lambda kv: (kv[0] is None, kv[0] or ""))),
            "elevationStats": self.elevation_stats,
            "summary": self.summary(),
            "issues": grouped,
        }


# ----- Tile helpers --------------------------------------------------------

def chunk_of(v: int) -> int:
    """Floor-divide tile coord by chunk size to get chunk coord.

    Must match Haskell's `floorDiv` for negative values.
    """
    return math.floor(v / CHUNK_SIZE)


def crosses_chunk_boundary(x1: int, y1: int, x2: int, y2: int) -> bool:
    return chunk_of(x1) != chunk_of(x2) or chunk_of(y1) != chunk_of(y2)


def neighbors4(x: int, y: int) -> list[tuple[int, int]]:
    return [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
