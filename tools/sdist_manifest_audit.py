#!/usr/bin/env python3
"""Source-distribution completeness audit (#2175).

`synarchy.cabal`'s `extra-source-files` is a hand-maintained manifest of
the tracked files a shipped runtime path or the packaged headless suite
opens by RELATIVE path. Nothing checked it, and it had already drifted:
on `bbda7d122` the tarball omitted the eight compiled unit atlas indexes
(`Unit.Atlas.Load` refuses a whole unit whose `atlas/index.json` is
missing, and there is no per-frame fallback, so an sdist-built binary
boots with no animated units), the entire `test-headless/data/`
fixture corpus, the two `docs/save_compat/*.json` documents, and the one
`tools/playtest/` module a headless example reads. #635/#666 made
`cabal sdist --list-only` merely SUCCEED; a glob that matches nothing
still succeeds, so success never meant completeness.

The failure mode is silent by construction. Every one of those files is
present in a git checkout, so the whole test suite and every probe pass
against the repository while the tarball they describe cannot build a
catalogue, register a unit, or run its own save-compatibility gate. The
only thing that can see the gap is a comparison against what git tracks.

What this compares
------------------
Two sides, both derived at run time -- there is no frozen file count
anywhere in this module, because the corpus grows (the save-compat
fixture set gained a pair between this issue's verification point and
its review, which is exactly how a constant would rot):

* **Expected** -- every file `git ls-files` reports that matches one of
  `REQUIRED_FAMILIES`, the resource families a shipped runtime path or
  the headless suite reads.
* **Actual** -- every path `cabal sdist --list-only` prints, normalized
  out of its `./` prefix.

Five rules, so the contract is two-sided rather than a subset check:

  1. **Completeness.** Every expected file appears in the listing.
  2. **Live patterns.** Every family matches at least one tracked file.
     A family whose directory was renamed would otherwise pass rule 1
     vacuously, which is the same shape of defect as the drift itself.
  3. **Nothing untracked ships.** Every listing entry is a tracked file.
     `extra-source-files` globs the WORKING TREE, not the index, so a
     widened pattern would sweep gitignored local state
     (`config/*.local.yaml`, #638/#786) into a published tarball. This
     is the general form of that guarantee and needs no pattern list.
  4. **Named exclusions.** No listing entry matches
     `FORBIDDEN_PATTERNS` or equals one of `FORBIDDEN_PATHS`. Rule 3
     already covers today's tree; these keep the two DELIBERATE
     exclusions on the record as positive assertions, so a future change
     to what git tracks cannot quietly make shipping them legal.
  5. **Reviewed config.** Every tracked `config/*.yaml` is either
     covered by a required family or named in `FORBIDDEN_PATHS`. A new
     tracked config file is then a reviewed decision -- ship it or say
     why not -- instead of an omission nobody notices.

The three legacy config files are the exclusions rule 4 and rule 5 pin.
`config/keybinds.yaml`, `config/video.yaml` and `config/notifications.yaml`
are one-time migration SOURCES (#638/#786/#1937): a fresh install that
found them would inherit a stale developer's settings, and
`Engine.Core.Init` treats their absence as the ordinary case. That is
also why requirement 1's "every tracked file the engine reads" is scoped
to REQUIRED runtime resources: those three are read, optionally, and
deliberately do not ship.

Cost
----
Normal mode runs `cabal sdist --list-only` exactly ONCE (~3 s, no build
product needed) and does everything else in memory; `--self-test` runs
it not at all. `test_sdist_manifest_audit`-style coverage lives in this
module's own `--self-test` because every rule is a pure function of two
sets, which synthetic listings exercise far more sharply than the real
tree can.

Usage:
  python3 tools/sdist_manifest_audit.py              # audit the tree
  python3 tools/sdist_manifest_audit.py --self-test  # fixtures only
Exit codes: 0 = clean, 1 = a violation (or, under --self-test, a fixture
behaved wrongly).
"""
from __future__ import annotations

import argparse
import fnmatch
import io
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

REPO_ROOT = Path(__file__).resolve().parent.parent

#: The resource families a shipped runtime path or the packaged headless
#: suite reads by relative path. Each entry is (glob, why it ships); the
#: EXPECTED INVENTORY is whatever `git ls-files` reports for the glob, so
#: adding a fixture or an atlas needs no edit here.
REQUIRED_FAMILIES: tuple[tuple[str, str], ...] = (
    ("cabal.project",
     "the index-state pin (#1354); without it an unpacked tarball "
     "resolves against whatever Hackage looks like on the day it is "
     "built and fails to compile -- vulkan 3.26.6 -> 3.27 broke exactly "
     "that during #2175"),
    ("cbits/*.h",
     "headers for the c-sources; sdist does not pick these up from "
     "include-dirs on its own"),
    ("data/**/*.yaml",
     "game-data catalogues: materials, vegetation, flora, units, "
     "recipes, buildings, locations"),
    ("data/language/concept_id_baseline.json",
     "the one non-YAML runtime resource under data/; "
     "Language.Semantic.Catalogue loads it beside concepts.yaml (#1868)"),
    ("assets/**/*.png",
     "every texture the bindless renderer and the preview path load"),
    ("assets/**/*.ttf",
     "the fonts stb_truetype rasterizes"),
    ("assets/**/*.json",
     "the compiled unit atlas indexes (#1258); Unit.Atlas.Load refuses "
     "a whole unit whose atlas/index.json is missing, with no per-frame "
     "fallback"),
    ("scripts/**/*.lua",
     "the Lua game logic the scripting thread loads from the resource "
     "root"),
    ("test-headless/data/**/*.bin",
     "the save-compatibility fixture envelopes Test.Headless.World.Save"
     ".Compat decodes"),
    ("test-headless/data/**/*.json",
     "each save-compat fixture's expected canonical post-migration "
     "state"),
    ("test-headless/data/**/*.txt",
     "Test.Headless.World.FloraOrder's golden and pre-canonical "
     "reference orderings"),
    ("docs/save_compat/*.json",
     "manifest.json drives the suite's \"manifest-declared fixtures\" "
     "describe; enum_baseline.json is tools/enum_append_only_audit.py's "
     "maintained baseline"),
    ("tools/playtest/critic_click.py",
     "Test.Headless.UI.InteractiveBounds reads the module that DEFINES "
     "widget_at for its playtest-oracle example (#2069 moved it out of "
     "the critic.py facade)"),
    ("config/*_default.yaml",
     "the versioned config templates a fresh install starts from"),
    ("config/pathing.yaml",
     "the one shipped config that is tuning rather than a user default"),
)

#: Entries that must never appear, however the manifest is written.
#: Rule 3 subsumes the first two on today's tree -- they are gitignored,
#: so they are untracked -- but naming them keeps the leak they describe
#: a stated contract rather than an accident of .gitignore.
FORBIDDEN_PATTERNS: tuple[tuple[str, str], ...] = (
    ("config/*.local.yaml",
     "gitignored per-machine runtime state (#638/#786): keybinds, "
     "video and notification settings a glob over config/ would leak"),
    ("config/*.legacy-neutral.local.yaml",
     "the migration's neutral-placeholder sentinels (#1937), which are "
     "per-machine state too"),
)

#: Tracked files that are deliberately NOT shipped, each with its reason.
#: These are the legacy migration sources (#638/#786/#1937): reading one
#: on a fresh install would import a stale developer's settings.
FORBIDDEN_PATHS: dict[str, str] = {
    "config/keybinds.yaml":
        "legacy migration source; a fresh install must start from "
        "keybinds_default.yaml, not a developer's bindings",
    "config/video.yaml":
        "legacy migration source; a fresh install must start from "
        "video_default.yaml, not a developer's resolution",
    "config/notifications.yaml":
        "legacy migration source; there is no default template and "
        "Engine.Core.Init treats its absence as the ordinary case",
}

#: Rule 5's scope: the tracked paths every one of which must be either
#: shipped by a family or excluded by name.
REVIEWED_CONFIG_GLOB = "config/*.yaml"

SDIST_COMMAND = ("cabal", "sdist", "--list-only")


# --------------------------------------------------------------------
# Glob matching
# --------------------------------------------------------------------

def matches(pattern: str, path: str) -> bool:
    """Cabal's `extra-source-files` glob semantics, over a POSIX path.

    `*` matches within ONE path segment (never across `/`, which
    `fnmatch` alone would happily do, silently widening every family
    here); `**` matches zero or more whole segments and is only
    meaningful as a segment of its own.
    """
    return _match_segments(pattern.split("/"), path.split("/"))


def _match_segments(pattern: list[str], parts: list[str]) -> bool:
    if not pattern:
        return not parts
    head, rest = pattern[0], pattern[1:]
    if head == "**":
        # Zero or more segments: try every split point, shortest first.
        for taken in range(len(parts) + 1):
            if _match_segments(rest, parts[taken:]):
                return True
        return False
    if not parts:
        return False
    if not fnmatch.fnmatchcase(parts[0], head):
        return False
    return _match_segments(rest, parts[1:])


# --------------------------------------------------------------------
# The audit itself -- a pure function of two sets
# --------------------------------------------------------------------

def family_members(pattern: str, tracked: set[str]) -> list[str]:
    """The tracked files one required family covers, in path order."""
    return sorted(p for p in tracked if matches(pattern, p))


def covered_files(tracked: set[str]) -> set[str]:
    """Every tracked file some required family ships, deduplicated."""
    covered: set[str] = set()
    for pattern, _ in REQUIRED_FAMILIES:
        covered.update(family_members(pattern, tracked))
    return covered


def audit(listing: set[str], tracked: set[str]) -> list[str]:
    """Every violation of the five rules, as reportable lines."""
    problems: list[str] = []

    covered: set[str] = set()
    for pattern, reason in REQUIRED_FAMILIES:
        family = family_members(pattern, tracked)
        if not family:
            problems.append(
                f"required family '{pattern}' matches no tracked file -- "
                f"the pattern is stale, or what it shipped is gone "
                f"({reason})")
            continue
        covered.update(family)
        missing = [p for p in family if p not in listing]
        if missing:
            shown = ", ".join(missing[:6])
            more = f" (+{len(missing) - 6} more)" if len(missing) > 6 else ""
            problems.append(
                f"{len(missing)} of {len(family)} tracked files matching "
                f"'{pattern}' are absent from the source distribution: "
                f"{shown}{more} -- {reason}")

    untracked = sorted(p for p in listing if p not in tracked)
    if untracked:
        shown = ", ".join(untracked[:6])
        more = f" (+{len(untracked) - 6} more)" if len(untracked) > 6 else ""
        problems.append(
            f"{len(untracked)} source-distribution entries are untracked "
            f"working-tree files: {shown}{more} -- extra-source-files globs "
            f"the WORKING TREE, not the index, so whatever sits in a "
            f"resource directory ships. Either the manifest widened onto "
            f"gitignored local state (config/*.local.yaml, #638/#786), or a "
            f"scratch file is sitting where a family globs; a tarball built "
            f"from this tree really would carry it")

    for pattern, reason in FORBIDDEN_PATTERNS:
        hits = sorted(p for p in listing if matches(pattern, p))
        if hits:
            problems.append(
                f"forbidden entries matching '{pattern}' are in the source "
                f"distribution: {', '.join(hits)} -- {reason}")

    for path, reason in sorted(FORBIDDEN_PATHS.items()):
        if path in listing:
            problems.append(
                f"'{path}' is deliberately excluded but ships: {reason}")

    unreviewed = sorted(
        p for p in tracked
        if matches(REVIEWED_CONFIG_GLOB, p)
        and p not in covered and p not in FORBIDDEN_PATHS)
    if unreviewed:
        problems.append(
            f"tracked config files are neither shipped by a required "
            f"family nor named in FORBIDDEN_PATHS: "
            f"{', '.join(unreviewed)} -- decide whether each is a shipped "
            f"template or an excluded migration source, and record it")

    return problems


# --------------------------------------------------------------------
# The two live inputs
# --------------------------------------------------------------------

def tracked_files(root: Path) -> set[str]:
    out = subprocess.run(["git", "ls-files", "-z"], cwd=root, check=True,
                         capture_output=True, text=True).stdout
    return {p for p in out.split("\0") if p}


def sdist_listing(root: Path) -> set[str]:
    """`cabal sdist --list-only`, normalized. Invoked exactly once."""
    result = subprocess.run(SDIST_COMMAND, cwd=root, capture_output=True,
                            text=True)
    if result.returncode != 0:
        raise SystemExit(
            f"`{' '.join(SDIST_COMMAND)}` failed (exit {result.returncode}):\n"
            f"{result.stderr.strip() or result.stdout.strip()}")
    entries = set()
    for raw in result.stdout.splitlines():
        line = raw.strip()
        if not line:
            continue
        entries.add(line[2:] if line.startswith("./") else line)
    return entries


# --------------------------------------------------------------------
# Self-test
# --------------------------------------------------------------------

#: A synthetic tree standing in for the real one: one file per required
#: family (two where a family's plurality is the point), both legacy
#: config exclusions, and nothing else.
FIXTURE_TRACKED = {
    "cbits/lua_debug.h",
    "data/materials/glacial.yaml",
    "data/language/concept_id_baseline.json",
    "assets/textures/ui/blank.png",
    "assets/fonts/shell.ttf",
    "assets/textures/units/acolyte/atlas/index.json",
    "assets/textures/units/tiller/atlas/index.json",
    "scripts/hud.lua",
    "test-headless/data/save-compat/b1-initial-session.bin",
    "test-headless/data/save-compat/b1-initial-session.expected.json",
    "test-headless/data/flora-order/seed42-w64-golden.txt",
    "docs/save_compat/manifest.json",
    "docs/save_compat/enum_baseline.json",
    "tools/playtest/critic_click.py",
    "config/keybinds_default.yaml",
    "config/pathing.yaml",
    # Excluded by name, so tracked but never listed.
    "config/keybinds.yaml",
    "config/video.yaml",
    "config/notifications.yaml",
    # Tracked, in no family, and correctly absent from the tarball --
    # FIXTURE_NOT_SHIPPED below. They prove rule 5 is scoped to config/
    # rather than demanding that the whole repository ship, and that
    # rule 1 does not claim a sibling of a literal family.
    "src/Engine/Loop.hs",
    "tools/pack_atlas.py",
    "docs/persistence_contract.md",
    # Shipped by cabal itself, not by a family: they must be tracked
    # (rule 3) and must not trip rule 5 or a family's completeness.
    "synarchy.cabal",
    "LICENSE",
    "README.md",
    "cabal.project",
}

#: Tracked files no family covers, so a correct manifest leaves them out.
FIXTURE_NOT_SHIPPED = {
    "src/Engine/Loop.hs",
    "tools/pack_atlas.py",
    "docs/persistence_contract.md",
}

#: What a correct manifest produces from FIXTURE_TRACKED: every family
#: member, and neither the excluded nor the uncovered files.
FIXTURE_LISTING = {
    p for p in FIXTURE_TRACKED
    if p not in FORBIDDEN_PATHS and p not in FIXTURE_NOT_SHIPPED
}


def self_test() -> int:
    selftestlib.parse_verbose()

    # -- the glob matcher, which every rule is built on ---------------
    expect(matches("data/**/*.yaml", "data/units/acolyte.yaml"),
           "** spans one intermediate segment")
    expect(matches("data/**/*.yaml", "data/a/b/c/d.yaml"),
           "** spans several intermediate segments")
    expect(matches("data/**/*.yaml", "data/top.yaml"),
           "** matches zero segments")
    expect(not matches("data/**/*.yaml", "data/units/acolyte.json"),
           "** family respects the extension")
    expect(not matches("config/*.yaml", "config/nested/x.yaml"),
           "* does not cross a path separator")
    expect(matches("config/*.local.yaml", "config/video.local.yaml"),
           "a forbidden pattern matches the local state it names")
    expect(not matches("config/*.local.yaml", "config/video_default.yaml"),
           "a forbidden pattern does not catch the template beside it")
    expect(matches("tools/playtest/critic_click.py",
                   "tools/playtest/critic_click.py"),
           "a literal family matches exactly itself")
    expect(not matches("tools/playtest/critic_click.py",
                       "tools/playtest/critic.py"),
           "a literal family does not match its sibling")

    # -- rule-by-rule fixtures ---------------------------------------
    def check(name: str, listing: set[str], tracked: set[str],
              clean: bool, needle: str | None = None) -> None:
        problems = audit(listing, tracked)
        if not expect(bool(problems) != clean,
                      f"{name}: {'clean' if clean else 'flagged'}"):
            print(f"    got: {problems if problems else 'no problems'}")
            return
        if needle is not None:
            expect(any(needle in p for p in problems),
                   f"{name}: report names '{needle}'")

    check("a complete listing", FIXTURE_LISTING, FIXTURE_TRACKED, clean=True)

    check("a missing expected resource",
          FIXTURE_LISTING - {"assets/textures/units/tiller/atlas/index.json"},
          FIXTURE_TRACKED, clean=False, needle="atlas/index.json")

    check("a missing save-compat fixture",
          FIXTURE_LISTING - {
              "test-headless/data/save-compat/b1-initial-session.bin"},
          FIXTURE_TRACKED, clean=False, needle="b1-initial-session.bin")

    check("a family shipped by nothing at all",
          {p for p in FIXTURE_LISTING if not p.startswith("docs/save_compat/")},
          FIXTURE_TRACKED, clean=False, needle="docs/save_compat/*.json")

    check("a family whose tracked files are gone (stale pattern)",
          {p for p in FIXTURE_LISTING if not p.startswith("docs/save_compat/")},
          {p for p in FIXTURE_TRACKED if not p.startswith("docs/save_compat/")},
          clean=False, needle="matches no tracked file")

    check("a forbidden config inclusion",
          FIXTURE_LISTING | {"config/video.local.yaml"},
          FIXTURE_TRACKED, clean=False, needle="config/*.local.yaml")

    check("a forbidden neutral-placeholder inclusion",
          FIXTURE_LISTING | {"config/video.legacy-neutral.local.yaml"},
          FIXTURE_TRACKED, clean=False,
          needle="config/*.legacy-neutral.local.yaml")

    check("a legacy migration source that ships",
          FIXTURE_LISTING | {"config/keybinds.yaml"},
          FIXTURE_TRACKED | {"config/keybinds.yaml"},
          clean=False, needle="deliberately excluded but ships")

    check("an untracked entry",
          FIXTURE_LISTING | {"dist-newstyle/build/leaked.o"},
          FIXTURE_TRACKED, clean=False, needle="untracked working-tree files")

    check("a new tracked config file nobody decided about",
          FIXTURE_LISTING, FIXTURE_TRACKED | {"config/audio.yaml"},
          clean=False, needle="config/audio.yaml")

    check("a new config TEMPLATE that the family already covers",
          FIXTURE_LISTING | {"config/audio_default.yaml"},
          FIXTURE_TRACKED | {"config/audio_default.yaml"}, clean=True)

    # -- normal mode's cost, asserted on the production entry point ---
    # Requirement 5 is "make ci's wall time is not measurably changed
    # beyond the one `cabal sdist --list-only` invocation", so the count
    # is part of the contract, not an implementation detail. Drive
    # main() -- not sdist_listing() -- so a future caller that consults
    # the listing twice is caught.
    calls: list[tuple[str, ...]] = []

    def counting_run(cmd, **kwargs):
        calls.append(tuple(cmd))
        if tuple(cmd[:2]) == ("git", "ls-files"):
            return subprocess.CompletedProcess(
                cmd, 0, stdout="\0".join(sorted(FIXTURE_TRACKED)), stderr="")
        return subprocess.CompletedProcess(
            cmd, 0, stdout="".join(f"./{p}\n" for p in sorted(FIXTURE_LISTING)),
            stderr="")

    real_run, real_argv, real_stdout = subprocess.run, sys.argv, sys.stdout
    try:
        subprocess.run = counting_run          # type: ignore[assignment]
        sys.argv = ["sdist_manifest_audit.py"]
        sys.stdout = io.StringIO()             # its summary is not ours
        status = main()
    finally:
        subprocess.run, sys.argv = real_run, real_argv
        sys.stdout = real_stdout
    expect(status == 0, "main() passes on a correct synthetic tree")
    expect(calls.count(SDIST_COMMAND) == 1,
           f"main() runs {' '.join(SDIST_COMMAND)} exactly once "
           f"(ran it {calls.count(SDIST_COMMAND)} time(s))")
    expect(sum(1 for c in calls if c[:2] == ("git", "ls-files")) == 1,
           "main() enumerates tracked files exactly once")

    # A `./`-prefixed listing is what cabal actually prints, and the
    # fixtures above use bare paths; this is the one place the
    # normalization itself is exercised end to end.
    expect(not any("./" in problem for problem in
                   audit({p for p in FIXTURE_LISTING}, FIXTURE_TRACKED)),
           "normalized listing entries compare against tracked paths")

    # -- the real constants are internally coherent -------------------
    expect(all(matches(REVIEWED_CONFIG_GLOB, p) for p in FORBIDDEN_PATHS),
           "every FORBIDDEN_PATHS entry is inside rule 5's scope")
    expect(not any(matches(pattern, path)
                   for pattern, _ in REQUIRED_FAMILIES
                   for path in FORBIDDEN_PATHS),
           "no required family claims a deliberately excluded file")
    expect(len(REQUIRED_FAMILIES) >= 15,
           "the required-family registry was not truncated")

    if FAILURES:
        print(f"sdist_manifest_audit self-test: {len(FAILURES)} failure(s)")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "sdist_manifest_audit self-test: all fixtures behaved as expected")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--self-test", action="store_true",
                        help="run the fixture suite instead of the tree")
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args()
    if args.self_test:
        return self_test()

    tracked = tracked_files(REPO_ROOT)
    listing = sdist_listing(REPO_ROOT)
    problems = audit(listing, tracked)
    if problems:
        print("source distribution manifest audit FAILED:")
        for problem in problems:
            print(f"  {problem}")
        print("\nFix synarchy.cabal's extra-source-files, or record the "
              "exclusion in tools/sdist_manifest_audit.py.")
        return 1
    print(f"source distribution manifest audit: {len(listing)} entries, all "
          f"tracked; {len(covered_files(tracked))} files across "
          f"{len(REQUIRED_FAMILIES)} required resource families all ship; "
          f"{len(FORBIDDEN_PATHS)} legacy config sources and "
          f"{len(FORBIDDEN_PATTERNS)} local-state patterns stay out")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
