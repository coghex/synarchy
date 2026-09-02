#!/usr/bin/env python3
"""Unit tests for the location probes' private `config/` tree (#1729).

`tools/location_content_probe.py`, `tools/location_overlay_probe.py` and
`tools/location_stamp_idempotent_probe.py` each build a throwaway
resource root for one invocation, and `tools/portal_ghost_probe.py`
imports the first of those builders and hands the same root to both its
headless writer and its offscreen reader. All four used to SYMLINK
`config/` in beside `scripts`, `assets` and `data`, described as
"read-only content, safe to share".

`config/` is not read-only content. Engine initialization is itself a
writer: `src/Engine/Asset/YamlNotifications.hs` materializes
`config/notifications.local.yaml` from registry defaults whenever that
file is absent, and `src/Engine/Core/Init.hs` migrates tracked legacy
configuration into absent local files. Through an alias those writes
landed in the developer's own checkout, and teardown — which unlinks the
alias rather than descending it — left them there. A personal
`*.local.yaml` was also visible to the run, so a local override could
decide what the probe observed.

All four probes boot a real engine and three of them are long; the
portal-ghost one needs a GPU. So the half of the contract that is pure
Python is pinned here, where it costs milliseconds, rather than only
being observable from a run neither gate can make:

  * The private `config/` is a real copy — not a symlink, not an
    `os.path.samefile` alias, and neither is any file inside it.
  * A seeded source `*.local.yaml` is absent from the private root, at
    the top level and nested.
  * Creating a new `*.local.yaml` AND mutating an existing copied file
    through the private root leave the source tree's entry names, entry
    types, file bytes and mode bits all unchanged.
  * A read-only source `config/` still yields a private copy this run
    can write and remove, recursively, while the source's own mode bits
    stay exactly as they were.
  * Removal never follows the `scripts`/`assets`/`data` symlinks.
  * The checkout's REAL `config/` survives a builder call and a write
    through the resulting root byte-for-byte. That comparison is a full
    manifest — entry names, entry types, file contents and mode bits,
    ignored entries included — because `.gitignore` hides the exact
    `*.local.yaml` paths at issue, so `git status --porcelain config/`
    cannot see the failure this file exists to catch.
  * `portal_ghost_probe.py` obtains the behavior by importing the
    corrected builder, which is asserted rather than assumed.

No engine, no world, no GPU: every test here runs against temporary
directories in well under a second.

Usage:
  python3 tools/test_location_probe_config_isolation.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import os
import shutil
import stat
import sys
import tempfile
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import location_content_probe as content  # type: ignore  # noqa: E402
import location_overlay_probe as overlay  # type: ignore  # noqa: E402
import location_stamp_idempotent_probe as stamp  # type: ignore  # noqa: E402
import portal_ghost_probe as portal  # type: ignore  # noqa: E402
# Since #2095 `location_content_probe` RE-EXPORTS its root builder from
# the invocation module the scenario split put it in. The re-export is
# the same function object — which is what
# `test_portal_ghost_shares_the_corrected_builder` below asserts against
# `content` — but the builder reads `REPO` from the module that DEFINES
# it, so the synthetic-checkout fixture must patch that module's global.
from location_content import invocation as content_builder  # type: ignore  # noqa: E402

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

#: Every DISTINCT root builder in scope, registered against the module
#: that DEFINES it rather than one that re-exports it — the fixture
#: below swaps `REPO` on the registered module, and a builder resolves
#: that name in its own globals. `portal_ghost_probe` is deliberately
#: absent: it does not define one, and the test below pins that it still
#: shares this list's first entry.
BUILDERS = (
    ("location_content_probe", content_builder),
    ("location_overlay_probe", overlay),
    ("location_stamp_idempotent_probe", stamp),
)

#: The content families that ARE safe to share, and must stay symlinks:
#: the probes read them and never write them, and `shutil.rmtree`
#: unlinks a symlink rather than descending it, so teardown cannot reach
#: the checkout through one.
CONTENT_FAMILIES = ("scripts", "assets", "data")

#: A name no checkout carries, so the real-checkout test below can tell
#: an entry IT created apart from anything that was already there.
SENTINEL = "probe_config_isolation_selftest.local.yaml"


# ---------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------
def manifest(top: str) -> dict[str, tuple]:
    """Every entry under `top`, relative path -> (kind, mode, payload).

    Symlinks are recorded by their target text and never followed, so a
    tree containing one can be compared without reading through it.
    Ignored files are recorded exactly like tracked ones: `.gitignore`
    excludes the `*.local.yaml` paths this whole contract is about, so a
    comparison that only saw tracked entries would be blind to the
    regression (#1729).
    """
    out: dict[str, tuple] = {}

    def walk(directory: str, prefix: str) -> None:
        for entry in sorted(os.listdir(directory)):
            full = os.path.join(directory, entry)
            rel = f"{prefix}/{entry}" if prefix else entry
            mode = os.lstat(full).st_mode
            if stat.S_ISLNK(mode):
                out[rel] = ("link", stat.S_IMODE(mode), os.readlink(full))
            elif stat.S_ISDIR(mode):
                out[rel] = ("dir", stat.S_IMODE(mode), None)
                walk(full, rel)
            else:
                with open(full, "rb") as handle:
                    out[rel] = ("file", stat.S_IMODE(mode), handle.read())

    walk(top, "")
    return out


def _chmod_writable(top: str) -> None:
    """Restore owner write+search throughout a tree, so a fixture that
    deliberately made itself read-only can still remove itself."""
    for path, dirs, files in os.walk(top):
        for name in [None, *dirs, *files]:
            target = path if name is None else os.path.join(path, name)
            with contextlib.suppress(OSError):
                mode = os.lstat(target).st_mode
                if not stat.S_ISLNK(mode):
                    os.chmod(target, stat.S_IMODE(mode) | stat.S_IRWXU)


def _write_synthetic_checkout(repo: str) -> None:
    """A stand-in checkout: three content families with a file each, and
    a `config/` carrying tracked defaults, a nested directory, and
    seeded `*.local.yaml` overrides at both levels."""
    for family in CONTENT_FAMILIES:
        os.makedirs(os.path.join(repo, family))
        marker = os.path.join(repo, family, f"{family}_marker.txt")
        with open(marker, "w") as handle:
            handle.write(f"{family} content\n")
    config = os.path.join(repo, "config")
    os.makedirs(os.path.join(config, "nested"))
    for rel, body in (
        ("keybinds_default.yaml", "tracked: keybinds\n"),
        ("pathing.yaml", "tracked: pathing\n"),
        ("nested/extra_default.yaml", "tracked: nested\n"),
        ("video.local.yaml", "personal: video override\n"),
        ("nested/inner.local.yaml", "personal: nested override\n"),
    ):
        with open(os.path.join(config, rel), "w") as handle:
            handle.write(body)


@contextlib.contextmanager
def synthetic_checkout(module, read_only: bool = False):
    """`module.REPO` pointed at a stand-in checkout for the duration.

    With `read_only`, the `config/` tree is handed over mode 0555/0444 —
    what a read-only mount, a read-only-restored CI cache or an archive
    unpacked without write bits gives the builder. `shutil.copytree`
    reproduces those bits, so without the builder's own relaxation the
    private copy is one this run can neither write nor unlink.
    """
    repo = tempfile.mkdtemp(prefix="test_loc_cfg_repo_")
    _write_synthetic_checkout(repo)
    config = os.path.join(repo, "config")
    if read_only:
        for path, _dirs, files in os.walk(config):
            for name in files:
                os.chmod(os.path.join(path, name), 0o444)
        os.chmod(os.path.join(config, "nested"), 0o555)
        os.chmod(config, 0o555)
    original = module.REPO
    module.REPO = repo
    try:
        yield repo
    finally:
        module.REPO = original
        _chmod_writable(repo)
        shutil.rmtree(repo, ignore_errors=True)


@contextlib.contextmanager
def built_root(module):
    """One invocation's base with the module's own root built inside it,
    always cleaned up however the test leaves it."""
    base = tempfile.mkdtemp(prefix="test_loc_cfg_run_")
    try:
        yield base, module.make_isolated_root(base)
    finally:
        _chmod_writable(base)
        shutil.rmtree(base, ignore_errors=True)


# ---------------------------------------------------------------------
# The private copy
# ---------------------------------------------------------------------
def test_config_is_a_private_copy_and_not_an_alias() -> None:
    print("\ntest_config_is_a_private_copy_and_not_an_alias")
    for name, module in BUILDERS:
        with synthetic_checkout(module) as repo, \
             built_root(module) as (_b, root):
            src = os.path.join(repo, "config")
            dst = os.path.join(root, "config")
            expect(os.path.isdir(dst) and not os.path.islink(dst),
                   f"{name}: the root's config/ is a real directory, not a "
                   f"symlink")
            expect(not os.path.samefile(src, dst),
                   f"{name}: the root's config/ is not an alias of the "
                   f"checkout's")
            aliased = []
            for rel in ("keybinds_default.yaml", "pathing.yaml",
                        "nested/extra_default.yaml"):
                copied = os.path.join(dst, rel)
                if os.path.islink(copied) or os.path.samefile(
                        os.path.join(src, rel), copied):
                    aliased.append(rel)
            expect(not aliased,
                   f"{name}: every copied config file is its own regular "
                   f"file, not a link or alias (aliased: {aliased})")
            with open(os.path.join(dst, "keybinds_default.yaml")) as handle:
                body = handle.read()
            expect(body == "tracked: keybinds\n",
                   f"{name}: a tracked default is copied with its content "
                   f"intact")
            expect(os.path.isfile(os.path.join(dst, "nested",
                                               "extra_default.yaml")),
                   f"{name}: a nested tracked default is copied too")


def test_the_developers_local_overrides_are_absent() -> None:
    print("\ntest_the_developers_local_overrides_are_absent")
    for name, module in BUILDERS:
        with synthetic_checkout(module), built_root(module) as (_b, root):
            dst = os.path.join(root, "config")
            expect(not os.path.exists(os.path.join(dst, "video.local.yaml")),
                   f"{name}: a seeded top-level *.local.yaml is not in the "
                   f"private root")
            expect(not os.path.exists(os.path.join(dst, "nested",
                                                   "inner.local.yaml")),
                   f"{name}: a seeded nested *.local.yaml is not either")


def test_the_content_families_are_still_shared_symlinks() -> None:
    print("\ntest_the_content_families_are_still_shared_symlinks")
    for name, module in BUILDERS:
        with synthetic_checkout(module) as repo, \
             built_root(module) as (_b, root):
            for family in CONTENT_FAMILIES:
                target = os.path.join(root, family)
                expect(os.path.islink(target) and os.path.samefile(
                           target, os.path.join(repo, family)),
                       f"{name}: {family}/ is still a symlink shared with the "
                       f"checkout")
            saves = os.path.join(root, "saves")
            expect(os.path.isdir(saves) and not os.path.islink(saves)
                   and not os.listdir(saves),
                   f"{name}: saves/ is this run's own empty directory")


def test_two_invocations_get_two_config_trees() -> None:
    print("\ntest_two_invocations_get_two_config_trees")
    for name, module in BUILDERS:
        with synthetic_checkout(module):
            with built_root(module) as (_b1, first), \
                 built_root(module) as (_b2, second):
                one = os.path.join(first, "config")
                two = os.path.join(second, "config")
                expect(one != two and not os.path.samefile(one, two),
                       f"{name}: two invocations own two separate config "
                       f"trees")
                with open(os.path.join(one, SENTINEL), "w") as handle:
                    handle.write("first only\n")
                expect(not os.path.exists(os.path.join(two, SENTINEL)),
                       f"{name}: a write in one invocation's config/ is "
                       f"invisible to the other's")


# ---------------------------------------------------------------------
# Writes through the root
# ---------------------------------------------------------------------
def test_writes_through_the_root_never_reach_the_source() -> None:
    print("\ntest_writes_through_the_root_never_reach_the_source")
    for name, module in BUILDERS:
        with synthetic_checkout(module) as repo:
            src = os.path.join(repo, "config")
            before = manifest(src)
            with built_root(module) as (_b, root):
                dst = os.path.join(root, "config")
                # What the engine does: materialise an absent local file...
                with open(os.path.join(dst, "notifications.local.yaml"),
                          "w") as handle:
                    handle.write("materialised: by the engine\n")
                # ...and rewrite one that was copied in.
                with open(os.path.join(dst, "pathing.yaml"), "w") as handle:
                    handle.write("rewritten: by the run\n")
                with open(os.path.join(dst, "nested", "extra_default.yaml"),
                          "w") as handle:
                    handle.write("rewritten: nested\n")
                after = manifest(src)
            expect(sorted(before) == sorted(after),
                   f"{name}: the source config/ gained and lost no entry "
                   f"(added {sorted(set(after) - set(before))}, removed "
                   f"{sorted(set(before) - set(after))})")
            differing = sorted(rel for rel in before
                               if rel in after and before[rel] != after[rel])
            expect(not differing,
                   f"{name}: every source entry keeps its type, bytes and "
                   f"mode (differing: {differing})")


def test_the_real_checkout_config_survives_a_run_untouched() -> None:
    print("\ntest_the_real_checkout_config_survives_a_run_untouched")
    for name, module in BUILDERS:
        src = os.path.join(str(module.REPO), "config")
        before = manifest(src)
        with built_root(module) as (_b, root):
            dst = os.path.join(root, "config")
            with open(os.path.join(dst, SENTINEL), "w") as handle:
                handle.write("written through this run's private root\n")
            mutated = next((rel for rel in sorted(before)
                            if before[rel][0] == "file"), None)
            if mutated is not None:
                with open(os.path.join(dst, mutated), "wb") as handle:
                    handle.write(b"rewritten by the self-test\n")
            after = manifest(src)
        added = sorted(set(after) - set(before))
        removed = sorted(set(before) - set(after))
        differing = sorted(rel for rel in before
                           if rel in after and before[rel] != after[rel])
        # Self-healing: if the alias came back, this test just wrote into
        # the developer's checkout. Put it back before reporting, so a
        # failing run leaves the tree exactly as it found it.
        for rel in differing:
            kind, mode, payload = before[rel]
            if kind == "file":
                with open(os.path.join(src, rel), "wb") as handle:
                    handle.write(payload)
                os.chmod(os.path.join(src, rel), mode)
        if added == [SENTINEL]:
            stray = os.path.join(src, SENTINEL)
            if os.path.isfile(stray) and not os.path.islink(stray):
                os.remove(stray)
        expect(not added,
               f"{name}: the checkout's config/ gained no entry, ignored "
               f"ones included (added: {added})")
        expect(not removed,
               f"{name}: the checkout's config/ lost no entry "
               f"(removed: {removed})")
        expect(not differing,
               f"{name}: every checkout config/ entry keeps its type, bytes "
               f"and mode (differing: {differing})")


# ---------------------------------------------------------------------
# A read-only source
# ---------------------------------------------------------------------
def test_a_read_only_source_still_yields_a_writable_tree() -> None:
    print("\ntest_a_read_only_source_still_yields_a_writable_tree")
    for name, module in BUILDERS:
        with synthetic_checkout(module, read_only=True):
            base = tempfile.mkdtemp(prefix="test_loc_cfg_ro_")
            try:
                root = module.make_isolated_root(base)
                dst = os.path.join(root, "config")
                unwritable = []
                for path, dirs, files in os.walk(dst):
                    subdirs = (os.path.join(path, d) for d in dirs)
                    for entry in [path, *subdirs]:
                        if not os.access(entry, os.W_OK | os.X_OK):
                            unwritable.append(entry)
                    for entry in (os.path.join(path, f) for f in files):
                        if not os.access(entry, os.W_OK):
                            unwritable.append(entry)
                expect(not unwritable,
                       f"{name}: every directory and file in the private "
                       f"copy is owner-writable (blocked: {unwritable})")
                # `os.access` answers from the mode bits; this proves
                # the write itself lands, which is what the engine does
                # to `config/` on nearly every boot.
                try:
                    with open(os.path.join(dst, "nested",
                                           "extra_default.yaml"), "w") as h:
                        h.write("the engine can rewrite this\n")
                    rewritten = None
                except OSError as exc:
                    rewritten = exc
                expect(rewritten is None,
                       f"{name}: a nested copied file really is rewritable "
                       f"(got {rewritten!r})")
            finally:
                _chmod_writable(base)
                shutil.rmtree(base, ignore_errors=True)


def test_the_read_only_source_keeps_its_own_mode_bits() -> None:
    print("\ntest_the_read_only_source_keeps_its_own_mode_bits")
    for name, module in BUILDERS:
        with synthetic_checkout(module, read_only=True) as repo:
            src = os.path.join(repo, "config")
            before = manifest(src)
            base = tempfile.mkdtemp(prefix="test_loc_cfg_ro2_")
            try:
                module.make_isolated_root(base)
                leftover = module.remove_isolated_root(base)
            finally:
                _chmod_writable(base)
                shutil.rmtree(base, ignore_errors=True)
            after = manifest(src)
            expect(before == after,
                   f"{name}: the read-only source is neither chmodded nor "
                   f"rewritten — only the copy is relaxed")
            expect(leftover is None,
                   f"{name}: a read-only source is not a cleanup failure "
                   f"(got {leftover!r})")


def test_a_read_only_source_still_yields_a_removable_tree() -> None:
    print("\ntest_a_read_only_source_still_yields_a_removable_tree")
    for name, module in BUILDERS:
        with synthetic_checkout(module, read_only=True):
            base = tempfile.mkdtemp(prefix="test_loc_cfg_ro3_")
            try:
                module.make_isolated_root(base)
                leftover = module.remove_isolated_root(base)
                expect(leftover is None and not os.path.exists(base),
                       f"{name}: the run removes its own tree instead of "
                       f"reporting residue (got {leftover!r})")
            finally:
                _chmod_writable(base)
                shutil.rmtree(base, ignore_errors=True)


# ---------------------------------------------------------------------
# Teardown
# ---------------------------------------------------------------------
def test_removal_never_follows_the_content_symlinks() -> None:
    print("\ntest_removal_never_follows_the_content_symlinks")
    for name, module in BUILDERS:
        with synthetic_checkout(module, read_only=True) as repo:
            before = {family: manifest(os.path.join(repo, family))
                      for family in CONTENT_FAMILIES}
            base = tempfile.mkdtemp(prefix="test_loc_cfg_rm_")
            try:
                module.make_isolated_root(base)
                leftover = module.remove_isolated_root(base)
            finally:
                _chmod_writable(base)
                shutil.rmtree(base, ignore_errors=True)
            expect(leftover is None,
                   f"{name}: a clean removal reports nothing "
                   f"(got {leftover!r})")
            after = {family: manifest(os.path.join(repo, family))
                     for family in CONTENT_FAMILIES}
            expect(before == after,
                   f"{name}: the real scripts/, assets/ and data/ are "
                   f"untouched — rmtree unlinked the symlinks")
            expect(os.path.isdir(os.path.join(repo, "config")),
                   f"{name}: and the source config/ is still there")


# ---------------------------------------------------------------------
# The importing probe
# ---------------------------------------------------------------------
def test_portal_ghost_shares_the_corrected_builder() -> None:
    print("\ntest_portal_ghost_shares_the_corrected_builder")
    expect(portal.make_isolated_root is content.make_isolated_root,
           "portal_ghost_probe builds its root with location_content_probe's "
           "own corrected builder")
    expect(portal.remove_isolated_root is content.remove_isolated_root,
           "...and tears it down with that module's remover")


def test_no_builder_symlinks_config_any_more() -> None:
    print("\ntest_no_builder_symlinks_config_any_more")
    for name, module in BUILDERS:
        source = Path(module.__file__).read_text(encoding="utf-8")
        expect('"scripts", "assets", "data", "config"' not in source,
               f"{name}: config/ is no longer in the symlinked family tuple")
        expect("shutil.ignore_patterns(\"*.local.yaml\")" in source,
               f"{name}: config/ is copied without the developer's local "
               f"overrides")


def main() -> int:
    selftestlib.parse_verbose()
    test_config_is_a_private_copy_and_not_an_alias()
    test_the_developers_local_overrides_are_absent()
    test_the_content_families_are_still_shared_symlinks()
    test_two_invocations_get_two_config_trees()
    test_writes_through_the_root_never_reach_the_source()
    test_the_real_checkout_config_survives_a_run_untouched()
    test_a_read_only_source_still_yields_a_writable_tree()
    test_the_read_only_source_keeps_its_own_mode_bits()
    test_a_read_only_source_still_yields_a_removable_tree()
    test_removal_never_follows_the_content_symlinks()
    test_portal_ghost_shares_the_corrected_builder()
    test_no_builder_symlinks_config_any_more()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nAll location probe config-isolation tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
