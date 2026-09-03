#!/usr/bin/env python3
"""The census on disk: one worktree, one lock, one write funnel (#2131).

Every other census owner is pure. This one is not, and it is the ONLY
one that is not: it resolves the `docs-wip` worktree, reads the
manifest, holds the cross-process lock, serializes the candidate,
enforces the preservation and append-only contracts, and atomically
replaces the file. `probe_census_contract.py` says what is admissible
and `probe_census_records.py` says what a mutation does; this module is
what makes one of those mutations durable, and it is deliberately the
only place in the tree that can.

`update()` is that funnel, and its ORDER is the contract every stored
mutation inherits:

  1. read the stored document under the lock, and refuse a substituted
     path before a byte is trusted;
  2. validate the stored schema and its invariants;
  3. apply exactly ONE in-memory mutation, from the records owner;
  4. serialize deterministically;
  5. enforce preservation and the append-only collections;
  6. validate the complete candidate;
  7. refuse to replace byte-identical content;
  8. atomically replace, and fsync the destination directory.

`record_result`, `record_claim`, `record_outcome`, `record_policy`,
`record_deferral` and `seed` are wrappers around that one call, not
alternative writers, and there is no second path to the file.

`DocsWorktreeMissing` and `CensusDurabilityUnconfirmed` live here
because both are facts about the disk rather than about a document, and
both stay re-exported from `tools/probe_census.py` for their existing
callers. `TOUCH_ANY` lives here for the same reason: it is what a
mutation returns to declare which rows it was allowed to change, which
is a term of this transaction and of nothing else.

`STAGING_PREFIX` is swept under the lock by `_clear_staging`, and
`tools/probe_census_page.py`'s `PAGE_STAGING_PREFIX` is deliberately NOT
an extension of it — see that module's own note. Neither prefix may be
re-derived from the other, and `LOCK_NOTE` stays the bytes it is.

This module has no CLI and is not a gate of its own. Every command is
still `python3 tools/probe_census.py`.
"""
from __future__ import annotations

import contextlib
import fcntl
import json
import os
import stat
import subprocess
import sys
import tempfile
from contextlib import contextmanager
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_probes  # noqa: E402
import probe_engine  # noqa: E402
from probe_census_contract import (  # noqa: E402
    CENSUS_SCHEMA, CensusError, DEFAULT_ACCEPTABLE_FAILURES,
    _refuse_inconsistent, census_invariants, validate_census, validate_document,
    validate_result,
)
from probe_census_records import (  # noqa: E402
    _deep_copy, _is_x, _refuse_policy, build_manifest, find_entry, find_outcome,
    ingest_claim, ingest_outcome, ingest_result, policy_invariants,
    reconcile_inventory, render_manifest, set_deferral, set_policy,
)


MANIFEST_RELPATH = "docs/probe_census.json"
DOCS_BRANCH = "docs-wip"

LOCK_SUFFIX = ".lock"
LOCK_NOTE = (b"tools/probe_census.py holds a cross-process flock on this "
             b"file while it rewrites docs/probe_census.json. It is "
             b"untracked scratch state; deleting it while no writer is "
             b"running is harmless.\n")
STAGING_PREFIX = ".probe_census."
STAGING_SUFFIX = ".tmp"

# `mutate` returns the probe keys it is allowed to have changed, or this
# sentinel for an inventory-wide operation (`--seed`), which may append
# rows but still may not reorder or drop one.
TOUCH_ANY = object()


class DocsWorktreeMissing(Exception):
    """No worktree is on `docs-wip`; the caller must create one."""


class CensusDurabilityUnconfirmed(Exception):
    """The replacement is ALREADY VISIBLE, and making it durable failed.

    Deliberately NOT a `CensusError`. That type's whole contract is the
    one `update` states — a failure before `os.replace` leaves the old
    authoritative bytes untouched — and by the time this is raised the
    new complete census is what every later reader will parse. Reporting
    it as an ordinary refusal would tell a caller its measurement was
    not recorded when it may well have been, and census ingestion is
    append-only and deliberately non-idempotent, so a caller that
    believed that and retried would duplicate the sample.

    It exists so a caller can distinguish the two sides DETERMINISTICALLY
    (#1436): the staging write and the pre-replacement fsync raise
    `OSError` from inside `_atomic_replace`'s try/except, the
    post-replacement directory fsync sits outside it, and both used to
    reach the caller as an indistinguishable bare `OSError`. Classifying
    them by exception message would be guesswork; this is the signal.

    `error` is the underlying failure and `target` the census that was
    replaced, so a report can name both.
    """

    def __init__(self, message: str, *, target, error):
        super().__init__(message)
        self.target = Path(target)
        self.error = error


# ==========================================================================
# The docs worktree
# ==========================================================================
CREATE_DOCS_WORKTREE = (
    f"  git worktree add ~/work/synarchy-docs -b {DOCS_BRANCH} origin/master")


def _worktree_records(stdout: str) -> list[dict]:
    """`git worktree list --porcelain` as one dict per blank-line record.

    Attributes are parsed whole rather than line-matched, because
    `prunable` is an attribute of the record it follows: a registered
    worktree whose directory is gone still prints its `worktree` and
    `branch` lines, and only the trailing `prunable <reason>` says it is
    no longer usable.
    """
    records: list[dict] = []
    current: dict = {}
    for line in stdout.splitlines():
        if not line.strip():
            if current:
                records.append(current)
            current = {}
            continue
        name, _, value = line.partition(" ")
        current[name] = value.strip()
    if current:
        records.append(current)
    return records


def resolve_docs_worktree(repo_root: str | None = None) -> Path:
    """The worktree whose branch is `docs-wip`, resolved BY BRANCH.

    The same idiom `tools/docs_land.sh` uses. A missing docs worktree is
    an actionable stop, never a silent fall back to the primary checkout
    (which the PR drainer must be able to fast-forward) and never an
    implicit `git worktree add` performed as a side effect.

    A REGISTERED-BUT-UNUSABLE worktree is the same stop. Git keeps
    listing a worktree whose directory has been deleted, marking the
    record `prunable`; returning that path anyway would let the writer
    recreate the directory and publish the census outside any worktree
    at all — silently, in a place nobody will ever land from.
    """
    root = repo_root or probe_engine.REPO_ROOT
    try:
        done = subprocess.run(["git", "worktree", "list", "--porcelain"],
                              cwd=root, text=True, capture_output=True,
                              timeout=30)
    except (OSError, subprocess.SubprocessError) as error:
        raise DocsWorktreeMissing(
            f"could not list git worktrees ({error})") from None
    if done.returncode != 0:
        raise DocsWorktreeMissing(
            f"could not list git worktrees: {done.stderr.strip()}")
    for record in _worktree_records(done.stdout):
        if record.get("branch") != f"refs/heads/{DOCS_BRANCH}":
            continue
        path = Path(record.get("worktree", ""))
        if "prunable" in record:
            raise DocsWorktreeMissing(
                f"the worktree registered for {DOCS_BRANCH} at {path} is "
                f"prunable ({record['prunable'] or 'unusable'}). Clear the "
                f"stale registration and recreate it with:\n"
                f"  git worktree prune\n{CREATE_DOCS_WORKTREE}")
        if not path.is_dir() or not (path / ".git").exists():
            raise DocsWorktreeMissing(
                f"the worktree registered for {DOCS_BRANCH} at {path} is not "
                f"a usable checkout. Clear the stale registration and "
                f"recreate it with:\n"
                f"  git worktree prune\n{CREATE_DOCS_WORKTREE}")
        return path
    raise DocsWorktreeMissing(
        f"no worktree is on branch {DOCS_BRANCH}. Create one with:\n"
        f"{CREATE_DOCS_WORKTREE}")


def manifest_path(repo_root: str | None = None) -> Path:
    return resolve_docs_worktree(repo_root) / MANIFEST_RELPATH


def load(path: Path):
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise DocsWorktreeMissing(
            f"manifest {path} is unreadable ({error})") from None
    except ValueError as error:
        raise ValueError(f"manifest {path} is not valid JSON: {error}") from None


# ==========================================================================
# The atomic write path
# ==========================================================================
def lock_path(target: Path) -> Path:
    """The one lock file guarding `target`, keyed by its RESOLVED path.

    ONE stable identity per target, so two processes naming the same
    census by different paths always contend and two different censuses
    never do.
    """
    resolved = Path(target).resolve()
    return resolved.parent / f".{resolved.name}{LOCK_SUFFIX}"


def _refuse_substituted(path: Path, what: str) -> None:
    """A symlinked, hard-linked or non-regular census path is refused.

    All three resolved paths get this — the census target, its
    directory, and the lock — not the lock alone. `os.replace` replaces
    the LINK, so following a symlinked `probe_census.json` would write
    the census wherever the link points (the primary checkout included),
    and replacing a hard-linked target silently strands the other name
    on the old bytes. Either defeats "leave the old authoritative bytes
    unchanged".
    """
    if path.parent.is_symlink():
        raise CensusError(
            f"refusing to use {path.parent}: the {what} directory may not be "
            f"a symlink")
    try:
        info = os.lstat(path)
    except FileNotFoundError:
        return
    except OSError as error:
        raise CensusError(f"could not stat the {what} {path} ({error})") from None
    if stat.S_ISLNK(info.st_mode):
        raise CensusError(
            f"refusing to use {path}: the {what} may not be a symlink")
    if not stat.S_ISREG(info.st_mode):
        raise CensusError(
            f"refusing to use {path}: the {what} must be a regular file "
            f"(got mode {stat.S_IFMT(info.st_mode):#o})")
    if info.st_nlink != 1:
        raise CensusError(
            f"refusing to use {path}: the {what} must have exactly one link "
            f"(got {info.st_nlink})")


@contextmanager
def _locked(target: Path):
    _refuse_substituted(Path(target), "census")
    guard = lock_path(target)
    _refuse_substituted(guard, "census lock")
    try:
        # The census directory is created here rather than at
        # replacement time so the lock exists for the very first writer
        # too. Reaching this point already means a `docs-wip` worktree
        # resolved, so nothing is created anywhere else.
        guard.parent.mkdir(parents=True, exist_ok=True)
        # O_NOFOLLOW closes the race between the lstat above and this
        # open: a lock path swapped for a symlink in between must fail
        # rather than be followed, or the note below would land wherever
        # it points — the primary checkout included.
        fd = os.open(str(guard), os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    except OSError as error:
        raise CensusError(
            f"could not open the census lock {guard} ({error})") from None
    # Checked and refused BEFORE the lock is taken, so the failure is not
    # routed through an unlock this file may not support.
    try:
        info = os.fstat(fd)
    except OSError as error:
        os.close(fd)
        raise CensusError(
            f"could not stat the census lock {guard} ({error})") from None
    # `st_nlink == 1` as well as regular: `O_NOFOLLOW` stops a SYMLINK,
    # but a HARD LINK planted at the lock path is the same inode as some
    # file elsewhere, and the note below would be written into it.
    if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
        os.close(fd)
        raise CensusError(
            f"refusing to use {guard}: the census lock must be a regular file "
            f"with exactly one link (got mode {stat.S_IFMT(info.st_mode):#o}, "
            f"{info.st_nlink} links)")
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        # The lock file is deliberately never unlinked: removing a HELD
        # flock file lets the next writer create a fresh inode, lock
        # that, and lose an update. It is a small untracked file in the
        # docs worktree — which is where CLAUDE.md's working-tree
        # discipline says an uncommitted file belongs — so it says so
        # itself for whoever finds it in `git status`. Re-stat under the
        # lock: another writer may have filled it since the open.
        if os.fstat(fd).st_size == 0:
            os.write(fd, LOCK_NOTE)
        yield
    finally:
        try:
            fcntl.flock(fd, fcntl.LOCK_UN)
        except OSError:
            # Closing the descriptor releases the lock regardless, and a
            # failing unlock must never mask the error that got us here.
            pass
        finally:
            os.close(fd)


def _clear_staging(directory: Path) -> None:
    """Remove staging files a killed writer left behind.

    Called under the lock, so nothing live is ever removed. A stale
    staging file is never authoritative — only `os.replace` makes a
    candidate the census — but it should not accumulate either.
    """
    try:
        candidates = list(directory.iterdir())
    except OSError:
        return
    for entry in candidates:
        name = entry.name
        if name.startswith(STAGING_PREFIX) and name.endswith(STAGING_SUFFIX):
            try:
                entry.unlink()
            except OSError:
                pass


def _atomic_replace(target: Path, payload: bytes, *, what: str = "census",
                    prefix: str = STAGING_PREFIX) -> None:
    """Install `payload` as `target` in one step.

    The staging file is a SIBLING so the rename never crosses a
    filesystem, the bytes are fsynced before the rename so a crash
    cannot promote a short file, and the directory is fsynced after so
    the rename itself is durable.

    `what` and `prefix` name the artifact for a caller that is not the
    census itself — `tools/probe_census_page.py` writes the generated
    page into the same directory, and needs a staging prefix
    `_clear_staging` does NOT sweep, since that sweep runs under the
    census lock and would otherwise unlink a live page staging file.
    """
    target.parent.mkdir(parents=True, exist_ok=True)
    fd, staged = tempfile.mkstemp(dir=str(target.parent),
                                  prefix=prefix, suffix=STAGING_SUFFIX)
    staged_path = Path(staged)
    try:
        # `mkstemp` creates with O_EXCL, so the staging path cannot be a
        # pre-planted symlink; assert the resulting inode anyway, since
        # this is the third path the substitution rule names.
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
            raise CensusError(
                f"refusing to use {staged_path}: the {what} staging file must "
                f"be a regular file with exactly one link")
        with os.fdopen(fd, "wb") as handle:
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(staged_path, 0o644)
        os.replace(str(staged_path), str(target))
    except BaseException:
        try:
            staged_path.unlink()
        except OSError:
            pass
        raise
    # Everything above either succeeded or left the old bytes in place.
    # From here the replacement is VISIBLE, so a failure is a durability
    # question and not a "did it happen" question — and the two have to
    # be told apart by their type rather than by their message, because
    # a caller's recovery differs completely (#1436). Nothing is retried
    # or rolled back here: the rename has landed and undoing it would
    # discard a committed append-only update.
    try:
        dir_fd = os.open(str(target.parent), os.O_RDONLY)
    except OSError as error:
        raise CensusDurabilityUnconfirmed(
            f"the {what} at {target} was replaced, but its directory could "
            f"not be opened to make the rename durable ({error}); the new "
            f"content is already visible", target=target, error=error) from None
    try:
        os.fsync(dir_fd)
    except OSError as error:
        raise CensusDurabilityUnconfirmed(
            f"the {what} at {target} was replaced, but the directory fsync "
            f"that makes the rename durable failed ({error}); the new "
            f"content is already visible", target=target, error=error) from None
    finally:
        with contextlib.suppress(OSError):
            os.close(dir_fd)


# ==========================================================================
# The preservation contract
# ==========================================================================
def _entry_map(document) -> dict:
    return {entry["key"]: entry for entry in (document or {}).get("probes") or []
            if isinstance(entry, dict) and isinstance(entry.get("key"), str)}


def _sample_total(census) -> int:
    """Every retained sample a census record holds, current and archived.

    Counts only what is countable and never raises. The declared schema
    now refuses a cohort whose `samples` is not a list before this ever
    sees one, so the tolerance is no longer load-bearing for STORED
    state — but it is kept, because this also runs over the CANDIDATE,
    which the schema has not yet checked at that point, and a list that
    BECOMES uncountable still reads as a drop from its old length.
    """
    if not isinstance(census, dict):
        return 0
    history = census.get("history")
    cohorts = list(history) if isinstance(history, list) else []
    if census.get("current") is not None:
        cohorts.append(census["current"])
    total = 0
    for cohort in cohorts:
        if not isinstance(cohort, dict):
            continue
        samples = cohort.get("samples")
        if isinstance(samples, list):
            total += len(samples)
    return total


POLICY_FIELDS = ("acceptable_failures", "acceptable_failures_justification",
                 "estimated_worst_case_seconds")
MEASUREMENT_FIELDS = ("current", "history", "attempts")
# #1434's claim log is its own aspect, not a measurement field: a claim
# is recorded BEFORE the measurement runs and may not carry a cohort, a
# sample or an attempt with it, and a measurement may not append a
# claim. Keeping the two sets disjoint is what makes each operation's
# `touched` aspect mean exactly one thing.
CLAIM_FIELDS = ("claims",)
# #1439's de-flake outcome log, a fourth aspect for the same reason the
# claim log was a third: an outcome is appended AFTER a diagnosis, is
# idempotent on its attempt identity, and may not create a cohort, a
# sample, an attempt or a claim on its way in.
OUTCOME_FIELDS = ("outcomes",)
# A maintainer-controlled availability gate. It neither rewrites policy
# nor discards evidence; it only tells the selector to pause this row until
# the recorded resume condition is satisfied.
DEFERRAL_FIELDS = ("deferred",)

# Each mutating aspect, the record fields it exclusively owns, and the
# operation a reader should be told about when a candidate touched a
# field it does not own. One table rather than four hand-written blocks,
# so a fifth aspect is a row here instead of another pair of loops that
# can be forgotten.
ASPECT_FIELDS = {
    "policy": POLICY_FIELDS,
    "measurements": MEASUREMENT_FIELDS,
    "claims": CLAIM_FIELDS,
    "outcomes": OUTCOME_FIELDS,
    "deferral": DEFERRAL_FIELDS,
}
ASPECT_LABEL = {
    "policy": "a policy update",
    "measurements": "a measurement ingestion",
    "claims": "a claim acquisition",
    "outcomes": "a diagnosis outcome",
    "deferral": "a deferral update",
}
# The aspects whose append-only logs `_append_only` compares. A policy
# update appends nothing, so it is not one of them.
APPENDING_ASPECTS = ("measurements", "claims", "outcomes")

INVENTORY_FIELDS = ("key", "script", "classification", "protocol")


def _is_initialized_x(was, now) -> bool:
    """`was`/`now` is exactly the unset-X-to-default transition (#1430).

    `_is_x` rather than a bare `now == DEFAULT_ACCEPTABLE_FAILURES`
    because `False == 0`, and a candidate that turned an unset X into a
    boolean has not initialized anything.
    """
    return (was is None and _is_x(now)
            and now == DEFAULT_ACCEPTABLE_FAILURES)


def _census_of(entry) -> dict:
    census = (entry or {}).get("census")
    return census if isinstance(census, dict) else {}


def _append_only(key: str, was: dict, now: dict) -> list[str]:
    """`history`, `attempts`, `claims` and `outcomes` grew by appending."""
    problems: list[str] = []
    for field in ("history", "attempts", "claims", "outcomes"):
        previous = was.get(field)
        current = now.get(field)
        previous = [] if previous is None else previous
        current = [] if current is None else current
        # Reported rather than sliced blindly: a stored `history: 5` is a
        # field this comparison cannot be made against, and slicing it
        # would raise from inside the preservation check.
        if not isinstance(previous, list) or not isinstance(current, list):
            problems.append(
                f"probe {key!r} `{field}` must be a list to compare "
                f"append-only, got {type(previous).__name__} before and "
                f"{type(current).__name__} after")
            continue
        if current[:len(previous)] != previous:
            problems.append(
                f"probe {key!r} `{field}` is append-only, but the candidate "
                f"rewrote or discarded an existing entry")
    # Archiving MOVES a cohort out of `current` into `history`, so the
    # append-only check above cannot see a cohort that was dropped
    # instead. Retained measurements only ever grow.
    if _sample_total(now) < _sample_total(was):
        problems.append(
            f"probe {key!r} lost retained measurements "
            f"({_sample_total(was)} before, {_sample_total(now)} after)")
    return problems


def _check_preserved(before, after, touched) -> list[str]:
    """Every way a candidate disturbed what it had no business touching.

    JSON serialization necessarily rewrites the whole file, so this is
    what makes "changes only the affected probe's record" real. It
    compares the candidate against the document it would replace and
    knows nothing about field shapes — this is the preservation
    contract, a different question from the declared schema validation
    that brackets it.

    `touched` maps a probe key to the aspects its operation may change
    (`"policy"`, `"measurements"`), or is `TOUCH_ANY` for `--seed`.
    Everything else must come through untouched: an unrelated row
    deeply equal and in the same position, a measurement leaving policy
    alone, a policy update leaving every cohort, sample and attempt
    alone, and no operation at all shrinking the retained measurements.
    """
    if before is None:
        return []
    problems: list[str] = []
    inventory = touched is TOUCH_ANY
    before_keys = [e.get("key") for e in (before.get("probes") or [])
                   if isinstance(e, dict)]
    after_keys = [e.get("key") for e in (after.get("probes") or [])
                  if isinstance(e, dict)]
    if inventory:
        # `--seed` may APPEND newly registered probes; it may not
        # reorder or drop one.
        if after_keys[:len(before_keys)] != before_keys:
            return ["the candidate reordered or dropped existing inventory "
                    "entries"]
    elif after_keys != before_keys:
        return [f"the candidate changed the inventory order or membership "
                f"({len(before_keys)} entries before, {len(after_keys)} after)"]

    old = _entry_map(before)
    new = _entry_map(after)
    for key, entry in old.items():
        candidate = new.get(key)
        if candidate is None:
            problems.append(f"the candidate dropped probe {key!r}")
            continue
        was, now = _census_of(entry), _census_of(candidate)
        if inventory:
            # Reconciliation refreshes inventory columns and may archive
            # a cohort on CI promotion. It never touches policy and
            # never changes a deferral or loses a measurement.
            problems += _append_only(key, was, now)
            for field in POLICY_FIELDS:
                if now.get(field) == was.get(field):
                    continue
                if field == "acceptable_failures" and _is_initialized_x(
                        was.get(field), now.get(field)):
                    # The ONE policy transition reconciliation may make
                    # (#1430): an unset X becomes the default. Stated
                    # here rather than assumed, so a candidate that
                    # moves any other policy value — or moves X from
                    # one number to another — is still refused.
                    continue
                problems.append(
                    f"probe {key!r}: reconciliation changed policy field "
                    f"`{field}`")
            for field in DEFERRAL_FIELDS:
                if now.get(field) != was.get(field):
                    problems.append(
                        f"probe {key!r}: reconciliation changed deferral "
                        f"field `{field}`")
            continue
        allowed = set(touched.get(key) or ())
        if not allowed:
            if candidate != entry:
                problems.append(
                    f"the candidate modified unrelated probe {key!r}")
            continue
        for field in INVENTORY_FIELDS:
            if candidate.get(field) != entry.get(field):
                problems.append(
                    f"probe {key!r}: the candidate changed inventory field "
                    f"`{field}`, which only --seed may refresh")
        if "policy" not in allowed:
            for field in POLICY_FIELDS:
                if now.get(field) != was.get(field):
                    problems.append(
                        f"probe {key!r}: the candidate changed policy field "
                        f"`{field}`")
        if allowed & set(APPENDING_ASPECTS):
            # One append-only comparison covers `history`, `attempts`,
            # `claims` and `outcomes` together; the per-aspect equality
            # checks below are what keep an operation inside the aspect
            # it declared.
            problems += _append_only(key, was, now)
        # Names the OPERATION, not just the field: "a policy update may
        # not touch `attempts`" is the sentence a reader needs. The
        # operation is whichever aspect the mutation declared, so the
        # wording stays true as aspects are added rather than being a
        # chain of two-way guesses.
        label = next((ASPECT_LABEL[aspect] for aspect in ASPECT_LABEL
                      if aspect in allowed), "this operation")
        for aspect, fields in ASPECT_FIELDS.items():
            if aspect in allowed or aspect == "policy":
                # `policy` is reported by the dedicated loop above,
                # which names the offending field rather than the
                # operation, and is the wording #1430's tests pin.
                continue
            for field in fields:
                if now.get(field) != was.get(field):
                    problems.append(
                        f"probe {key!r}: the candidate changed `{field}`, "
                        f"which {label} may not touch")
    return problems


def read_for_update(path: Path):
    """The census exactly as stored, or None when it does not exist.

    Unreadable or non-JSON state is a controlled refusal here rather
    than something the writer repairs, and the document is returned
    UNMIGRATED so each operation can decide for itself: only `--seed`
    migrates.
    """
    _refuse_substituted(Path(path), "census")
    if not path.exists():
        return None
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise CensusError(f"census {path} is unreadable ({error})") from None
    except ValueError as error:
        raise CensusError(
            f"census {path} is not valid JSON: {error}") from None


def require_current_schema(document, path: Path) -> dict:
    """The census as a v2 document, or the refusal that names `--seed`.

    `--record` and the policy operations never migrate and never seed:
    an absent or still-v1 census is a controlled stop naming the one
    operation that fixes it. No measurement is lost by the refusal —
    `probe_flake --result PATH` writes the result document to an
    external path, so the operator seeds and re-runs `--record` on the
    same file.
    """
    if document is None:
        raise CensusError(
            f"census {path} does not exist yet; create it with "
            f"`python3 tools/probe_census.py --seed`")
    if not isinstance(document, dict):
        raise CensusError(
            f"census {path} must be a JSON object, got "
            f"{type(document).__name__}")
    schema = document.get("schema")
    if schema != CENSUS_SCHEMA:
        raise CensusError(
            f"census {path} is {schema!r}, not {CENSUS_SCHEMA!r}; migrate it "
            f"in place with `python3 tools/probe_census.py --seed`, which "
            f"never overwrites census data")
    return document


def update(path: Path, mutate) -> dict:
    """One locked read-modify-write of the census at `path`.

    `mutate` receives the stored document (or None when the target does
    not exist yet) and returns `(candidate, touched)`, where `touched`
    is the set of probe keys it is allowed to have changed or
    `TOUCH_ANY` for an inventory operation. The lock is held from the
    read through serialization and the preservation checks to the
    replacement, and the bytes that are checked are exactly the bytes
    installed.

    Declared schema validation (#1492) brackets the mutation: the stored
    document is checked BEFORE `mutate` transforms it, and the complete
    candidate is checked immediately before the replacement — against
    the serialized bytes, so what is validated is exactly what a later
    reader will parse.

    Any failure before `os.replace` — a schema violation on either side,
    a refusing mutation, an unserializable candidate, a preservation
    violation, a staging write that dies — leaves the old authoritative
    bytes untouched.

    AFTER the replacement there is exactly one thing left to fail, and
    it raises `CensusDurabilityUnconfirmed` rather than the `CensusError`
    that promise belongs to: the directory fsync that makes the rename
    durable. The new census is already what a later reader parses by
    then, so a caller must not treat it as a refusal and must not retry
    an append-only ingestion against it.
    """
    path = Path(path)
    with _locked(path):
        before = read_for_update(path)
        if before is not None:
            validate_census(before, f"census {path}")
        try:
            candidate, touched = mutate(before)
            payload = render_manifest(
                candidate, f"the candidate census for {path}").encode("utf-8")
            # Compare against the bytes a later reader will see, not the
            # in-memory object that produced them.
            installed = json.loads(payload.decode("utf-8"))
            problems = _check_preserved(before, installed, touched)
        except CensusError:
            raise
        except (TypeError, ValueError, KeyError, AttributeError,
                IndexError) as error:
            # The safety boundary the issue requires, at the ONE funnel
            # every mutation passes through, and covering the WHOLE
            # candidate derivation — the mutation, the serialization and
            # the preservation comparison alike, since malformed stored
            # state reaches all three. A structural or type error met
            # while performing the operation becomes a controlled
            # refusal instead of a traceback. It is not schema
            # validation — the declared schema owns shape on both
            # sides of this, and #1493 owns the cross-field invariants;
            # this reports only what actually blocked the operation.
            raise CensusError(
                f"census {path} is structurally malformed for this operation "
                f"({type(error).__name__}: {error})") from None
        if problems:
            raise CensusError("refusing to install this candidate census: " +
                              "; ".join(problems[:5]))
        # The last gate before the replacement. It runs AFTER the
        # preservation comparison so a candidate that both loses data
        # and violates the schema reports the loss, which is the more
        # actionable of the two, and it validates the SERIALIZED bytes
        # rather than the object that produced them. A candidate is
        # always the CURRENT schema whatever the stored document was, so
        # the definition is named rather than rediscovered from the
        # candidate's own field.
        validate_document(installed, CENSUS_SCHEMA,
                          f"the candidate census for {path}")
        # The cross-field invariants (#1493) on the same two sides the
        # schema is applied to. The stored check above is what refuses
        # to rewrite an inconsistent census; this one is what refuses to
        # CREATE one, so no mutation can install state the next read
        # would reject.
        _refuse_inconsistent(census_invariants(installed),
                             f"the candidate census for {path}")
        # The acceptable-failure policy (#1430), on the CANDIDATE only.
        # Deliberately not bracketed like the two checks above: a census
        # written before this policy existed holds null Xs, and `--seed`
        # must be able to read one in order to initialize them. What no
        # operation may do is install a policy-invalid census.
        _refuse_policy(policy_invariants(installed),
                       f"the candidate census for {path}")
        _clear_staging(path.parent)
        if path.exists() and path.read_bytes() == payload:
            # A drift-free `--seed` is genuinely a no-op: leave the file,
            # its inode and its mtime exactly as they are.
            return installed
        _atomic_replace(path, payload)
        return installed


def ensure_document(path: Path) -> dict:
    """Create, migrate, or reconcile the census at `path`, losing nothing.

    An ABSENT target gets a fresh v2 seed. An existing one is migrated
    to the current schema and reconciled against the live registry —
    never regenerated, so accumulated census data cannot be overwritten
    by a freshly generated inventory.
    """
    def mutate(before):
        if before is None:
            return build_manifest(), TOUCH_ANY
        return reconcile_inventory(before), TOUCH_ANY
    return update(path, mutate)


def refuse_ci_eligible_measurement(probe: str) -> None:
    """A CI-eligible probe takes no census sample (#1431).

    "A promoted probe receives no further census samples" is a STORAGE
    invariant, not only a reporting one. `probe_flake.resolve_probe`
    already refuses to RUN a CI-eligible probe, but a result document
    outlives the run that produced it: one measured before a promotion,
    or replayed from an artifact tree afterwards, would otherwise be
    ingested into a row whose classification says it is CI's now.
    Eligibility is read LIVE from `tools/ci_probes.py`, the same
    authority `classification` reads, never from the stored row — a
    census not yet reconciled by `--seed` still holds the old label.

    The refusal covers a harness error too. It is not a judgement about
    the sample: nothing about a promoted probe belongs in the census's
    append-only record, and its retained history stays exactly as the
    promotion left it.
    """
    if probe in ci_probes.CI_ELIGIBLE:
        raise CensusError(
            f"probe {probe!r} is CI-eligible, so the census accepts no "
            f"measurement for it: CI runs it on every matching PR, and a "
            f"promoted probe keeps its manifest row and its retained history "
            f"while receiving no further samples. tools/ci_probes.py is the "
            f"authority on that classification.")


def record_result_installed(path: Path, result) -> tuple:
    """Ingest one result and return `(probe, installed_census)`.

    `update` already returns the candidate it installed;
    `record_result` discards it and answers only the probe key. A caller
    that needs a field of the row it just wrote — #1659's handoff needs
    the acceptable-failure count — must read it from THAT document and
    not from a later reread: the lock is released when `update` returns,
    so a second read can answer with another agent's edit and attribute
    it to this measurement.
    """
    validate_result(result)
    refuse_ci_eligible_measurement(result["probe"])
    touched: list[str] = []

    def mutate(before):
        document = require_current_schema(before, path)
        candidate, probe = ingest_result(document, result)
        touched.append(probe)
        return candidate, {probe: {"measurements"}}
    installed = update(path, mutate)
    return touched[0], installed


def record_result(path: Path, result) -> str:
    """Ingest one `probe-flake-result/v1` document. Returns the probe.

    The result is validated against its declared schema HERE — before
    the census is locked, and so before one nested ingestion field is
    read. That ordering is what makes `runs[i].checks` safe to reach at
    all: a truthy non-object there used to raise from inside the
    transaction rather than refuse in front of it.

    Live CI eligibility is refused in the same place and for the same
    reason: in front of the lock, so a promoted probe's census bytes
    are never even opened for a measurement it may not receive.
    """
    return record_result_installed(path, result)[0]


def record_claim(path: Path, probe: str, claim) -> str:
    """Durably record one successful claim acquisition. Returns the probe.

    The same locked read-modify-write every other mutation uses, so a
    recorder contending with a measurement ingestion or a policy edit
    serializes against it rather than losing an update. A replay of an
    already-recorded token installs the identical bytes, which `update`
    recognizes as a no-op and leaves the file, its inode and its mtime
    alone.
    """
    def mutate(before):
        document = require_current_schema(before, path)
        candidate, key = ingest_claim(document, probe, claim)
        return candidate, {key: {"claims"}}
    update(path, mutate)
    return probe


def record_outcome_installed(path: Path, probe: str, outcome, *,
                             reconcile=None) -> tuple:
    """Record one outcome and return `(probe, resumed, installed)`.

    `installed` is the record as it now sits in the census — which is
    the reconciled one when `reconcile` ran, so a caller reports what
    the census holds rather than the candidate it proposed.

    `resumed` is decided INSIDE the transaction, while the lock is held
    — it is true exactly when the stored census already carried this
    attempt identity. A caller that instead compared the file's bytes
    before and after would be reading them outside the lock, and a
    concurrent writer's unrelated edit would make a genuine first append
    look like a resume.

    `reconcile(candidate, stored)` is the same window offered to the
    caller. Idempotency is the WHOLE record, so a caller whose record
    carries a field it cannot derive — a wall-clock stamp is the one
    that exists — needs the STORED record to reproduce itself, and the
    only race-free place to read it is here. It runs only when this
    attempt is already recorded, and whatever it returns is what
    `ingest_outcome` then holds to that stored record: it can make a
    replay identical, and it cannot make two genuinely different
    outcomes agree.
    """
    seen: list[tuple] = []

    def mutate(before):
        document = require_current_schema(before, path)
        candidate = outcome
        attempt = (candidate.get("attempt")
                   if isinstance(candidate, dict) else None)
        stored = None
        if isinstance(attempt, str) and attempt:
            row = find_entry(document, probe)
            stored = find_outcome((row or {}).get("census"), attempt)
        if stored is not None and reconcile is not None:
            candidate = reconcile(candidate, stored)
        seen.append((stored is not None, candidate))
        installed, key = ingest_outcome(document, probe, candidate)
        return installed, {key: {"outcomes"}}
    update(path, mutate)
    resumed, record = seen[-1]
    return probe, resumed, _deep_copy(record)


def record_outcome(path: Path, probe: str, outcome) -> str:
    """Durably record one stable de-flake outcome. Returns the probe.

    The same locked read-modify-write every other mutation uses, so an
    outcome append contending with a measurement ingestion, a claim
    acquisition or a policy edit serializes against it rather than
    losing an update. Read, validation, append, serialization validation
    and the atomic replacement all happen under the one lock `update`
    holds. A resume of an already-recorded attempt installs the
    identical bytes, which `update` recognizes as a no-op and leaves the
    file, its inode and its mtime alone.

    There is deliberately no second state store and no second write
    path: #1428's writer is the one that owns this file.
    """
    return record_outcome_installed(path, probe, outcome)[0]


def record_policy(path: Path, probe: str, **fields) -> str:
    def mutate(before):
        document = require_current_schema(before, path)
        candidate, key = set_policy(document, probe, **fields)
        return candidate, {key: {"policy"}}
    update(path, mutate)
    return probe


def record_deferral(path: Path, probe: str, **fields) -> str:
    """Durably defer or resume one probe through the census writer."""
    def mutate(before):
        document = require_current_schema(before, path)
        candidate, key = set_deferral(document, probe, **fields)
        return candidate, {key: {"deferral"}}
    update(path, mutate)
    return probe


def seed(repo_root: str | None = None) -> Path:
    """#1425's entry point. It no longer regenerates over an existing file."""
    path = manifest_path(repo_root)
    ensure_document(path)
    return path
