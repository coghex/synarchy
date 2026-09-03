#!/usr/bin/env python3
"""What every case owner in this package shares, and nothing else (#2138).

Two things, both of which would otherwise be copied into six children:

`TOOLS_DIR` and the `sys.path` entry it installs. Anything run as
`python3 tools/<name>.py` has `tools/` on `sys.path` already, but a
module INSIDE this package does not: `Path(__file__).resolve().parent`
resolves to the package directory, so a child importing
`persistence_inventory_audit` would fail wherever the façade had not
already been imported. Resolving it once here is what lets
`--family haskell` work in a fresh interpreter, and `topology.py` reads
the same constant rather than recomputing a second notion of where the
owner modules live.

`expect`, re-exported from the shared #1922 runner so a child needs one
import rather than a path dance followed by a second one. It is the same
function object `selftestlib` defines -- this module does not wrap it,
count separately, or define an `expect` of its own, because a second
assertion facility is exactly what #1922 removed.

Per #2138 requirement 16 this module imports no case owner, so the
dependency direction through the package is one-way: support and the
fixture modules, then the case owners, then the façade.
"""
from __future__ import annotations

import sys
from pathlib import Path

#: `tools/` -- this package's parent, and where the production audit
#: modules and `selftestlib` live.
TOOLS_DIR = Path(__file__).resolve().parent.parent

if str(TOOLS_DIR) not in sys.path:
    sys.path.insert(0, str(TOOLS_DIR))

from selftestlib import expect  # noqa: E402  -- needs TOOLS_DIR on sys.path

__all__ = ["TOOLS_DIR", "expect"]
