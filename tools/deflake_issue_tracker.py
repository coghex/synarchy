#!/usr/bin/env python3
"""The tracker boundary one production-defect issue is filed across (#1438).

`tools/deflake_issue.py` files ONE tracker issue for a diagnosed
production defect. This module owns the two operations that reach a
tracker at all — RECONCILE a publication key against it, and CREATE one
issue — and the validation of what comes back.

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate

Its ownership runs from a stable publication key, or a rendered title
and body, to a validated issue identity: the `Publication` interface the
gate fakes, the `gh`-backed implementation, the search, the creation,
the JSON and URL parsing, and the two identity validators. Nothing here
walks an artifact tree or touches a census — it consumes what the
document owner rendered and answers with an identity the façade hands
on — and nothing here edits, labels or closes an issue: the review lane
owns an issue once it exists.

The marker is verified, not trusted
-----------------------------------
A tracker search matches text anywhere, and an issue this workflow files
QUOTES engine logs — so an issue that merely mentions a publication key
comes back from the search index. What separates the two is the
document owner's `carries_key`: one STANDALONE marker line outside every
code fence. That predicate, and the `body_origin` read beside it, are
CALLED rather than restated here, which makes the document owner this
module's one sibling dependency and the family's only permitted edge
between two extracted owners. A second spelling of either would let a
search-index match be recorded as a publication.
"""
from __future__ import annotations

import importlib
import json
import os
import re
import subprocess
import sys
import tempfile

# `tools/` carries no `__init__.py`, so it is an implicit namespace
# package, and every module in it has TWO import spellings: the
# `tools.<name>` one used from the repository root, and the bare one a
# caller who put `tools/` on `sys.path` uses. Python treats those as
# DIFFERENT modules, so resolving a sibling by bare name from a file
# that was itself loaded as `tools.<name>` loads a second copy of it —
# and then `tools.deflake_issue.issue_body is not
# tools.deflake_issue_document.issue_body`, `except
# tools.deflake_issue.PublicationFailed` stops catching what
# `tools.deflake_issue_tracker` raises, and a substituted
# `MAX_BODY_CHARS` lands on a module nothing renders through.
#
# So every dependency is resolved under the spelling that loaded THIS
# module, and the path insertion below remains for the bare spelling and
# for running this file directly as a script.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))


def _sibling(name: str):
    """One `tools/` module, under the spelling that loaded this one."""
    return importlib.import_module(
        f"{__package__}.{name}" if __package__ else name)


deflake_handoff = _sibling("deflake_handoff")
_document = _sibling("deflake_issue_document")

# The document owner's own objects, bound and not copied: a second
# spelling of the standalone-marker rule would let a search-index match
# be recorded as this attempt's publication.
ORIGIN_MARKER = _document.ORIGIN_MARKER
PUBLICATION_MARKER = _document.PUBLICATION_MARKER
body_origin = _document.body_origin
carries_key = _document.carries_key

NonSuccess = deflake_handoff.NonSuccess


class PublicationFailed(NonSuccess):
    """The tracker could not be reconciled or written.

    A NonSuccess because the ending is the same one: nothing is
    recorded, nothing else is attempted, and the attempt stays
    resumable. Its own type so a caller — and the gate — can tell a
    boundary failure from evidence that did not support the route.
    """


class Publication:
    """What a publisher must answer. Injected so a gate can fake it.

    Two operations and no more: RECONCILE a publication key against the
    tracker, and CREATE one issue. Nothing here edits an issue, labels
    one, or closes one — the review lane owns an issue once it exists.

    `find` answers with the issue's `number`, `url` AND `body`, or None.
    The body is not a convenience: it is what proves the match is the
    marker line rather than a quotation, and it is where the issue's own
    `issue-origin` brand is read from. `create` answers with `number`
    and `url`, since this workflow wrote that body itself.
    """

    def find(self, key: str):
        raise NotImplementedError

    def create(self, *, title: str, body: str):
        raise NotImplementedError


ISSUE_URL = re.compile(r"^https://[^\s]+/issues/(\d+)$")


class GitHubPublication(Publication):
    """The real boundary: `gh`, and nothing else.

    Constructed but never used by the document paths, so the module
    imports and every test runs on a machine with no `gh` and no
    network.
    """

    def __init__(self, repo: str | None = None, *, timeout: float = 120.0):
        self.repo = repo
        self.timeout = timeout

    def _gh(self, *args) -> str:
        argv = ["gh", *args]
        if self.repo:
            argv += ["--repo", self.repo]
        try:
            done = subprocess.run(argv, capture_output=True, text=True,
                                  timeout=self.timeout)
        except (OSError, subprocess.SubprocessError) as error:
            raise PublicationFailed(
                f"`{' '.join(argv)}` could not be run ({error}); nothing was "
                f"recorded, so the attempt stays resumable") from None
        if done.returncode != 0:
            raise PublicationFailed(
                f"`{' '.join(argv)}` exited {done.returncode} "
                f"({done.stderr.strip()[:400]}); nothing was recorded, so "
                f"the attempt stays resumable")
        return done.stdout

    def find(self, key: str):
        stdout = self._gh("issue", "list", "--state", "all", "--limit", "50",
                          "--search", key, "--json", "number,url,body")
        try:
            rows = json.loads(stdout or "[]")
        except json.JSONDecodeError as error:
            raise PublicationFailed(
                f"`gh issue list` did not answer with JSON ({error})"
            ) from None
        if not isinstance(rows, list):
            raise PublicationFailed(
                "`gh issue list` did not answer with a list of issues")
        # The oldest match wins, so a resume that races a duplicate
        # still converges on one issue rather than alternating.
        matches = sorted(
            (row for row in rows
             if isinstance(row, dict) and carries_key(row.get("body"), key)),
            key=lambda row: row.get("number") or 0)
        if not matches:
            return None
        return {"number": matches[0].get("number"),
                "url": matches[0].get("url"),
                "body": matches[0].get("body")}

    def create(self, *, title: str, body: str):
        handle, name = tempfile.mkstemp(prefix="deflake_issue_",
                                        suffix=".md")
        try:
            with os.fdopen(handle, "w", encoding="utf-8") as stream:
                stream.write(body)
            stdout = self._gh("issue", "create", "--title", title,
                              "--body-file", name)
        finally:
            try:
                os.unlink(name)
            except OSError:
                pass
        # `gh issue create` prints the new issue's URL on its last line.
        lines = [line.strip() for line in (stdout or "").splitlines()
                 if line.strip()]
        url = lines[-1] if lines else ""
        match = ISSUE_URL.match(url)
        if not match:
            raise PublicationFailed(
                f"`gh issue create` did not print an issue URL (got "
                f"{url[:200]!r}); the issue may exist, so re-run this "
                f"workflow — the publication key reconciles it rather than "
                f"filing a second one")
        return {"number": int(match.group(1)), "url": url}


def require_issue_identity(value, key: str, origin: str) -> dict:
    """A CREATED issue, held to a shape the census can store.

    `origin` is the caller's here, and correctly so: this workflow wrote
    that body and put the marker in it. A RECONCILED issue is the other
    case, and it reads its brand off the issue instead.
    """
    number, url = _require_number_and_url(value)
    return {"number": number, "url": url, "publication_key": key,
            "origin": origin}


def _require_number_and_url(value) -> tuple:
    if not isinstance(value, dict):
        raise PublicationFailed(
            f"the publication boundary answered with "
            f"{type(value).__name__}, not an issue identity")
    number = value.get("number")
    if not isinstance(number, int) or isinstance(number, bool) or number < 1:
        raise PublicationFailed(
            f"the publication boundary answered with the issue number "
            f"{number!r}, which is not a positive integer")
    url = value.get("url")
    if not isinstance(url, str) or not url.startswith("https://"):
        raise PublicationFailed(
            f"the publication boundary answered with the issue URL {url!r}, "
            f"which is not an absolute https URL a reviewer can open")
    return number, url


def require_reconciled_issue(value, key: str) -> dict:
    """An EXISTING issue this attempt was already filed as.

    Held to more than a created one, because nothing here wrote it: the
    marker line has to be in the fetched body — a search index match is
    not evidence — and the recorded brand is the one the ISSUE carries,
    not the one the resuming invocation happens to run under. An issue
    with no readable origin marker is not one this workflow filed, so it
    is a publication failure rather than something to record under a
    guess.
    """
    number, url = _require_number_and_url(value)
    body = value.get("body")
    if not carries_key(body, key):
        raise PublicationFailed(
            f"the publication boundary answered with issue #{number}, whose "
            f"body carries no `{PUBLICATION_MARKER}` line for {key}; a "
            f"tracker search matches text anywhere, so an issue that only "
            f"quotes the key is not the one this attempt was filed as")
    origin = body_origin(body)
    if origin is None:
        raise PublicationFailed(
            f"issue #{number} carries this attempt's publication key but no "
            f"readable `{ORIGIN_MARKER}` marker, so it is not one this "
            f"workflow filed and the brand its review routes on cannot be "
            f"recorded")
    return {"number": number, "url": url, "publication_key": key,
            "origin": origin}
