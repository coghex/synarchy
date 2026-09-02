"""Shared immutable contracts of the bare-name icon gate (#1740, split by
#2142 requirement 4).

The LEAF of the split: it imports nothing from the other
`bare_name_icon_asset_*` owners, and every one of them imports from here.
What lives here is exactly what more than one owner reads —

  * the repository and icon-root concepts (`REPO_ROOT`, `ICON_ROOT`);
  * `CheckError`, the ONE class every extractor raises and the audit and
    self-test both catch — it must stay a single class object so an
    `except CheckError` in one module matches a raise in another;
  * `Reference`, the authoritative bare-name reference record every
    extractor produces and the audit consumes;
  * the source-location support genuinely shared by the Lua and Haskell
    cleaners: `blank_span` (same-length blanking that keeps indices and
    line numbers aligned with the original text) and `LineMap`.

Nothing here reads a file, parses a language, walks the asset tree or
formats a diagnostic. The public façade is tools/bare_name_icon_asset_check.py.
"""
from __future__ import annotations

import bisect
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
ICON_ROOT = "assets/textures/icons"


class CheckError(Exception):
    """A loud extraction failure: the check refuses rather than narrows."""


def blank_span(span: str) -> str:
    return "".join("\n" if ch == "\n" else " " for ch in span)


class LineMap:
    def __init__(self, text: str) -> None:
        self.starts = [0]
        for idx, ch in enumerate(text):
            if ch == "\n":
                self.starts.append(idx + 1)

    def line_of(self, index: int) -> int:
        return bisect.bisect_right(self.starts, index)


class Reference:
    __slots__ = ("basename", "source", "line", "site", "row")

    def __init__(self, basename, source, line, site, row=None):
        self.basename = basename
        self.source = source
        self.line = line
        self.site = site
        #: Which ROW of the site this reference belongs to -- the enclosing
        #: table key (`dodge`), else the entry's own identifying literal field
        #: (`stat = "neuro"`), else None for a rowless site such as an anchor.
        #: Cross-family pins bind to this, so a pin cannot be satisfied by an
        #: unrelated reference that merely shares the basename.
        self.row = row

    def where(self) -> str:
        return f"{self.source}:{self.line} ({self.site})"
