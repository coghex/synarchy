#!/usr/bin/env python3
"""Select path-relevant expensive CI gates.

On pull requests, this retains the full blocking Hspec suite while skipping
the graphical test-suite compilation, the quick worldgen-output check, the
unit-asset inventory validation, and the save-compat fixture
reproducibility test when their inputs were untouched. Pushes to master run
all of them as a post-merge backstop.

Patterns are matched with ``fnmatch``, where ``*`` crosses ``/`` and
``**`` carries no special meaning — write ``dir/*`` for a whole subtree.

``--local-changed-paths`` (issue #1360) prints the changed-path list
``tools/ci-local.sh`` feeds back into ``--stdin``, so `make ci` reaches a
gate decision through the very same command CI runs rather than through a
second, separately-drifting matcher. It is the local counterpart of CI's
``git diff --name-only <pr-base> HEAD``: paths changed relative to the
merge base with the checked-out default branch, TRACKED working-tree edits
included, and — when no merge base can be established — the sentinel that
makes every gate select, because a local gate that cannot tell what
changed must be conservative rather than silently skip coverage.
"""
from __future__ import annotations

import argparse
import fnmatch
import subprocess
import sys
from collections.abc import Callable
from pathlib import Path


WORLDGEN_GLOBS = [
    "app/App/Dump.hs", "app/Main.hs",
    "src/Engine/Core/Init.hs", "src/Engine/Scripting/Lua/API/World/GenConfig*",
    # Generation-family subtrees use a `Name*` prefix (not `Name/*`) so each
    # family's facade module (e.g. src/World/Generate.hs, src/World/Fluids.hs)
    # matches alongside its directory. Deliberately NOT src/World/* wholesale:
    # the gameplay subtrees there (saves, designations, cursors, power,
    # render-side Tile texturing, ...) cannot shift a bare --dump's
    # terrain/material/fluid/ice/ore layers, and must not trigger the gate.
    "src/World/Generate*", "src/World/Geology*", "src/World/Hydrology*",
    "src/World/Fluid*", "src/World/Flora*", "src/World/Weather*",
    "src/World/Ocean*", "src/World/Magma*",
    "src/World/Material*", "src/World/Plate*", "src/World/Chunk*",
    "src/World/Region*", "src/World/Tile/*", "src/World/Vegetation*",
    "src/World/Grid.hs", "src/World/Scale.hs", "src/World/Constants.hs",
    "src/World/Base.hs",
    "src/World/ZoomMap*", "src/World/Types*",
    # A bare --dump does not read generation output directly (#1318): it
    # drives the live pipeline and then reads back what the world thread
    # left in wsTilesRef. The whole verified sequence is in scope here.
    #   * WorldInit orchestration — app/App/Dump.hs queues WorldInit,
    #     World.Thread.Command delegates it to World.Thread.Command.Init,
    #     which builds the generation parameters, the centre chunk, the
    #     live tile state and the remaining chunk queue.
    #   * Chunk loading — World.Thread's tick runs
    #     World.Thread.ChunkLoading.drainInitQueues, which is what the
    #     dump's waitForChunks waits on.
    #   * SimFastSettleAll — Sim.Thread runs real simulateActiveTick fluid
    #     simulation, then emits acknowledged FluidWriteback batches built
    #     from fluid, terrain, rendered surface and side decorations.
    #   * Writeback application — World.Thread.Command's
    #     handleApplyFluidsCommand overwrites lcFluidMap,
    #     lcTerrainSurfaceMap, lcSurfaceMap and lcSideDeco on the live
    #     chunk, and the dump reads terrainZ/surfaceZ/fluidType/fluidSurf
    #     straight out of those just-overwritten fields.
    # So a change to any of these stages can move baseline-observed dump
    # output. src/Sim* is whole-tree (and picks up a future src/Sim.hs
    # facade) because every module there feeds that settle, including ones
    # added later; the world-thread entries are exact paths instead, so the
    # excluded src/World/Thread/ siblings above stay excluded.
    "src/Sim*",
    "src/World/Thread.hs", "src/World/Thread/ChunkLoading.hs",
    "src/World/Thread/Command.hs", "src/World/Thread/Command/Init.hs",
    "config/world_gen_default.yaml", "data/materials/*", "data/flora/*",
    "data/vegetation/*", "tools/world_*.py", "tools/baselines/*",
]

GRAPHICAL_GLOBS = [
    "app/*", "cbits/*", "test/*", "assets/*", "config/video*.yaml",
    "src/Engine/Graphics/*", "src/Engine/Scene/*", "src/Engine/Asset/*",
    "src/Engine/Loop/*", "src/UI/*", "src/World/Render/*",
    "synarchy.cabal", "cabal.project", "cabal.project.*",
]

# The unit-asset gate: tools/test_pack_atlas.py plus tools/pack_atlas.py
# --validate-only --strict. That one command is three checks in one —
# the #1257 inventory, #1258's freshness comparison against a fresh
# regeneration, and #1262's image/slot and resident-memory budgets — so
# every path below selects all three and there is nothing to select
# them separately with.
#
# These are fnmatch patterns, NOT globs — `*` crosses `/` and `**` means
# nothing special — so `assets/textures/units/*` already covers the whole
# subtree, generated `<unit>/atlas/` artifacts included, and
# `data/units/*.yaml` would also match a nested path under that
# directory. Anything that can move which PNGs exist, which frames
# are declared, how a declaration decodes, or how the gate itself runs
# belongs here.
UNIT_ASSET_GLOBS = [
    # The assets and their declarations. Source frames and the
    # compiler-owned atlas/ artifacts both live under this one subtree.
    "assets/textures/units/*", "data/units/*.yaml",
    # The checker, its self-test, this selector, and the CI wiring that
    # invokes them. Since issue #2054 the checker is a façade over one
    # `tools/pack_atlas_<owner>.py` implementation module per concern
    # (shared, image, declarations, inventory, compiler, index, budget),
    # and a change to any owner has to select this gate exactly as a
    # change to the façade does. That family is matched by prefix, unlike
    # SAVE_COMPAT_GLOBS' explicit per-module list below: nothing
    # unrelated under tools/ shares the `pack_atlas_` prefix, and a
    # future owner module must not be able to escape the gate by being
    # left off a list. `tools/pack_atlas_*.py` does not match the
    # self-test family, which is matched by its own prefix below.
    "tools/pack_atlas.py", "tools/pack_atlas_*.py",
    # Since issue #2061 the self-test is a façade of the same shape,
    # over `tools/test_pack_atlas_support.py` and one case owner per
    # concern (validation, compiler, budget). Matched by prefix for the
    # same reason: a case owner that dropped off an explicit list would
    # stop selecting the gate its own cases are the gate for.
    # `tools/test_pack_atlas*.py` covers the façade and every owner, and
    # is the same pattern the issue's acceptance compile command uses.
    "tools/test_pack_atlas*.py",
    "tools/ci_expensive_gates.py", "tools/ci-local.sh", "Makefile",
    ".github/workflows/ci.yml", ".github/ci/Dockerfile",
    # The budget policy the strict run enforces (#1262). Editing a
    # threshold changes what the gate demands, so it has to re-run.
    "tools/unit_texture_budget.json",
    # The runtime that parses the generated index and RECOMPUTES the
    # compiler's source digest. Same rationale as the decoders below:
    # `Unit.Atlas.Digest` has to reproduce `pack_atlas.py`'s digest
    # byte for byte, so a change on either side has to face the other.
    "src/Unit/Atlas/*",
    # The pinned Python toolchain the compiler runs on (#1258). The
    # self-test fails when it disagrees with the Dockerfile, so a pin
    # edit has to re-run this gate.
    "tools/requirements-assets.txt",
    # The unit-YAML schema/loader, the preview metadata loader, and the
    # gameplay registration loader — the three decoders that have to
    # agree with the checker about the declaration shape.
    "src/Engine/Asset/YamlUnits.hs", "src/Engine/Asset/YamlList.hs",
    "src/Engine/Preview/Unit.hs",
    "src/Engine/Scripting/Lua/API/Units/Yaml.hs",
    # The boot path that feeds every data/units/*.yaml to loadUnitYaml:
    # it is what makes an asset-only file runtime-visible at all, so a
    # change here can break the exclusion the gate exists to protect.
    "scripts/startup_loader.lua",
    # Registration of the headless group and the module list that makes
    # it compile. Either one silently un-registers Asset.UnitInventory,
    # which turns its --match gate vacuous rather than failing.
    "test-headless/Spec.hs", "synarchy.cabal",
    # Their tests and the preview probe.
    "test-headless/Test/Headless/Asset/UnitInventory.hs",
    "test-headless/Test/Headless/Asset/TextureFallback.hs",
    "test-headless/Test/Headless/Preview/UnitAnimation.hs",
    "tools/preview_probe.py",
    # The preview probe's unit family (#2089): the filesystem-derived
    # animation list, the compiled-index read, the YAML fps/loop scanner
    # and the roster moved out of the facade into this owner, so an edit
    # here is an edit to a unit-asset expectation. Its siblings under
    # tools/preview/ own no unit expectation and stay unselected.
    "tools/preview/units.py",
]

# The save-compat gate (#1360): the ONE member of
# tools/test_save_compat_audit.py that spawns a `cabal repl` --
# test_normalize_fixture_timestamp_makes_generation_reproducible, reached
# by `--only-reproducibility`. Everything else in that module, and the
# whole of tools/save_compat_audit.py, stays unconditional on every pull
# request; only this member is selected here.
#
# The test decodes a tracked fixture through the real envelope codec,
# rewrites its `metadata` payload's smTimestamp, re-encodes, and proves
# normalize_fixture_timestamp collapses the two variants to identical
# bytes. So the inputs that can move its result are: the audit tooling
# that owns normalize_fixture_timestamp, the fixture corpus and manifest
# it reads, the Haskell modules its GHCi setup imports, and the build
# definition that decides what `cabal repl test:synarchy-test-headless`
# even loads.
#
# NB these are fnmatch patterns, not globs: `*` crosses `/`.
SAVE_COMPAT_GLOBS = [
    # The audit and its self-test. The reproducibility member and its
    # GHCi setup script live in the self-test; normalize_fixture_timestamp
    # -- the very thing that member covers -- lives in the codec bridge
    # since issue #2049 split the tool into owner modules, so EVERY owner
    # is named here. Explicit per-module patterns, not a blanket
    # `tools/save_compat*`: that would newly capture the unrelated
    # tools/save_compat_migration_probe.py, whose negative case sits
    # beside save_storage_probe.py's below.
    #
    # The self-test side is a PREFIX pattern instead, because issue
    # #2073 split it into a façade plus seven sibling modules and a
    # future owner must not be able to escape this gate by being left
    # off a list. `tools/test_save_compat_audit*.py` matches the façade
    # (`*` matches empty) and every sibling, and nothing else under
    # tools/ carries that prefix -- the production modules are named
    # `save_compat_audit_*`, without the `test_`.
    "tools/save_compat_audit.py", "tools/test_save_compat_audit*.py",
    "tools/save_compat_audit_common.py",
    "tools/save_compat_audit_components.py",
    "tools/save_compat_audit_fingerprint.py",
    "tools/save_compat_audit_codec.py",
    "tools/save_compat_audit_manifest.py",
    "tools/save_compat_audit_register.py",
    "tools/save_compat_audit_generate.py",
    # The manifest the audit reads, and the tracked fixture corpus the
    # test decodes. `_CURRENT_FORMAT_FIXTURE_PATH` points into the
    # second, and is re-pointed whenever the metadata component's
    # version is bumped, so the whole directory is in scope rather than
    # one file that would silently stop being the current one.
    "docs/save_compat/*", "test-headless/data/save-compat/*",
    # The save format itself. Whole subtree plus any future
    # src/World/Save.hs facade: the GHCi setup imports
    # World.Save.Envelope.Codec/.Types, World.Save.Envelope,
    # World.Save.Component and World.Save.Types directly, and the
    # frozen compat mirrors under Compat/ decide which fixtures still
    # decode at all.
    "src/World/Save*",
    # The build definition. `cabal repl test:synarchy-test-headless`
    # resolves its module set, dependency bounds and options from the
    # cabal file and EVERY cabal.project file cabal applies, so any of
    # them can change whether the repl loads or what it loads.
    # `cabal.project*` covers the whole family on purpose, including
    # `.local`: that file is NOT gitignored, so a change can legitimately
    # track one, and cabal would then apply it in CI. What keeps
    # tools/ci-local.sh's own TEMPORARY cabal.project.local from
    # selecting the gate is not this pattern but the ORDER over in that
    # script -- it resolves its changed-path list before it writes the
    # scratch file -- plus local_changed_paths listing tracked paths
    # only.
    "synarchy.cabal", "cabal.project*",
    # The CI toolchain image: the GHC/cabal versions and the pinned
    # index snapshot the repl actually runs against. BOTH files that
    # define it -- the image tag is a hash of the reusable workflow's
    # own bytes concatenated with the Dockerfile's, so an edit to the
    # build recipe alone (context, options, validation) mints a new
    # image just as a Dockerfile edit does, and can move what the repl
    # runs under.
    ".github/ci/Dockerfile", ".github/workflows/ci-image.yml",
    # The wiring that selects and runs this gate on both sides, and the
    # audit that keeps those two sides honest. An edit to any of them
    # can change WHEN the coverage runs, so it has to face the coverage
    # itself.
    #
    # Issue #2159 split that audit into a facade plus four production
    # owners and a self-test owner, and the save-compat wiring the
    # facade used to hold is now a module of its own -- so EVERY one of
    # them is named here. Dropping any would silently narrow this
    # selector: the module that decides when the reproducibility member
    # runs would stop selecting the member it decides about. Explicit
    # per-module patterns rather than a blanket `tools/ci_parity*`,
    # matching the form used for the #2049 owners above, so a future
    # `tools/ci_parity_*_probe.py` cannot be swept in unexamined; the
    # parity audit's own self-test enumerates `tools/ci_parity_*.py`
    # from the filesystem and fails if one of them stops selecting here.
    "tools/ci_expensive_gates.py", "tools/ci_parity_audit.py",
    "tools/ci_parity_shell.py", "tools/ci_parity_config.py",
    "tools/ci_parity_workflow.py", "tools/ci_parity_save_compat.py",
    "tools/test_ci_parity_audit.py",
    "tools/ci-local.sh", "Makefile", ".github/workflows/ci.yml",
]


# Every selectable gate. A dict rather than a chain of conditionals on
# purpose: the previous two-way `A if gate == "worldgen" else B` made an
# unrecognised gate name silently inherit GRAPHICAL_GLOBS, so a new gate
# could look wired up while actually running on the graphical patterns.
GATE_GLOBS: dict[str, list[str]] = {
    "worldgen": WORLDGEN_GLOBS,
    "graphical": GRAPHICAL_GLOBS,
    "unit-assets": UNIT_ASSET_GLOBS,
    "save-compat": SAVE_COMPAT_GLOBS,
}


# The names the CLI accepts. Kept beside GATE_GLOBS and cross-checked in
# the self-test so the two can never drift apart.
GATE_CHOICES = ("worldgen", "graphical", "unit-assets", "save-compat")


#: Emitted by ``--local-changed-paths`` when no merge base with the
#: default branch can be established, and understood by ``selected`` as
#: selecting EVERY gate. A local gate that cannot tell what changed must
#: run the coverage, not skip it; a sentinel says that out loud instead
#: of relying on some path that happens to match every pattern table.
#: Deliberately not a legal path, so a real changed file can never be it.
CONSERVATIVE_SENTINEL = "!!ci-expensive-gates:unresolved-base!!"


def selected(gate: str, changed_files: list[str]) -> bool:
    """Whether a PR touching ``changed_files`` needs ``gate``."""
    try:
        patterns = GATE_GLOBS[gate]
    except KeyError:
        raise ValueError(f"unknown gate: {gate!r}") from None
    # Checked AFTER the gate name, so an unknown gate still raises rather
    # than being answered `True` by a conservative caller.
    if CONSERVATIVE_SENTINEL in changed_files:
        return True
    return any(any(fnmatch.fnmatch(path, pattern) for pattern in patterns)
               for path in changed_files)


def _git(args: list[str], cwd: Path | str) -> tuple[int, str]:
    try:
        proc = subprocess.run(["git", *args], cwd=str(cwd),
                              capture_output=True, text=True)
    except (OSError, FileNotFoundError):
        return 1, ""
    return proc.returncode, proc.stdout


def default_branch_ref(cwd: Path | str) -> str | None:
    """The checked-out clone's default branch, as a rev git can resolve.

    `origin/HEAD` is the honest answer when the clone records one; the
    ordered fallbacks cover a clone that never fetched it and a worktree
    of a repository whose remote is named something else.
    """
    code, out = _git(["symbolic-ref", "--quiet", "refs/remotes/origin/HEAD"],
                     cwd)
    if code == 0 and out.strip().startswith("refs/remotes/"):
        return out.strip()[len("refs/remotes/"):]
    for candidate in ("origin/master", "origin/main", "master", "main"):
        code, _ = _git(["rev-parse", "--verify", "--quiet",
                        candidate + "^{commit}"], cwd)
        if code == 0:
            return candidate
    return None


def local_changed_paths(cwd: Path | str = ".") -> list[str]:
    """The changed-path list `make ci` feeds back into ``--stdin``.

    The local counterpart of CI's ``git diff --name-only <pr-base> HEAD``:
    every TRACKED path differing from the merge base with the default
    branch, which covers commits made on the branch, staged edits and
    unstaged edits in one diff. Untracked files are deliberately absent —
    that is what keeps ``tools/ci-local.sh``'s temporary, untracked
    ``cabal.project.local`` from selecting a gate.

    ``--no-renames`` makes a rename the delete+add pair, so moving a file
    OUT of a gate's path table still selects that gate. Returns
    ``[CONSERVATIVE_SENTINEL]`` — which selects every gate — when the
    default branch or the merge base cannot be resolved.
    """
    base = default_branch_ref(cwd)
    if base is None:
        return [CONSERVATIVE_SENTINEL]
    code, out = _git(["merge-base", "HEAD", base], cwd)
    merge_base = out.strip()
    if code != 0 or not merge_base:
        return [CONSERVATIVE_SENTINEL]
    code, out = _git(["diff", "--no-renames", "--name-only", merge_base], cwd)
    if code != 0:
        return [CONSERVATIVE_SENTINEL]
    return [line.strip() for line in out.splitlines() if line.strip()]


def _local_changed_paths_failures() -> list[str]:
    """Behavioural checks for ``--local-changed-paths`` (#1360).

    Run against throwaway git repositories rather than this checkout, so
    the result does not depend on what the developer happens to have
    uncommitted. Requirement 7's local semantics are exactly these three
    cases: a committed branch edit is seen, an unstaged and an untracked
    working-tree file are treated differently, a TRACKED
    cabal.project.local edit is seen (cabal applies it, so CI selects on
    it), and an unresolvable base runs everything.
    """
    def run(args: list[str], cwd: str) -> None:
        subprocess.run(["git", *args], cwd=cwd, check=True,
                       capture_output=True, text=True)

    def new_repo(tmp: str) -> None:
        # `git init` then an explicit symbolic-ref rather than
        # `--initial-branch`: the flag needs git >= 2.28, and a machine
        # with `init.defaultBranch` set to something else would
        # otherwise give these fixtures a branch name
        # default_branch_ref does not look for.
        run(["init", "--quiet", "."], tmp)
        run(["symbolic-ref", "HEAD", "refs/heads/master"], tmp)
        run(["config", "user.email", "gate@example.invalid"], tmp)
        run(["config", "user.name", "gate self-test"], tmp)
        # A developer's global commit hooks have nothing to do with
        # these throwaway repositories and must not be able to fail them.
        run(["config", "core.hooksPath", str(Path(tmp) / "no-hooks")], tmp)

    try:
        return _local_changed_paths_cases(new_repo, run)
    except subprocess.CalledProcessError as error:
        return [f"--local-changed-paths self-test could not drive git: "
                f"{error.cmd} exited {error.returncode}: "
                f"{(error.stderr or '').strip()}"]
    except OSError as error:
        return [f"--local-changed-paths self-test could not drive git: "
                f"{error}"]


def _local_changed_paths_cases(
        new_repo: Callable[[str], None],
        run: Callable[[list[str], str], None]) -> list[str]:
    """The three cases, factored out so their git errors are catchable."""
    import tempfile

    failures: list[str] = []

    with tempfile.TemporaryDirectory() as tmp:
        new_repo(tmp)
        Path(tmp, "README.md").write_text("base\n", encoding="utf-8")
        run(["add", "README.md"], tmp)
        run(["commit", "--quiet", "--no-verify", "-m", "base"], tmp)
        run(["checkout", "--quiet", "-b", "feature"], tmp)
        Path(tmp, "tools").mkdir()
        Path(tmp, "tools/save_compat_audit.py").write_text("x\n",
                                                           encoding="utf-8")
        run(["add", "tools/save_compat_audit.py"], tmp)
        run(["commit", "--quiet", "--no-verify", "-m",
             "committed branch edit"], tmp)
        # An unstaged edit to a TRACKED file, and an UNTRACKED scratch
        # file standing in for tools/ci-local.sh's cabal.project.local.
        Path(tmp, "README.md").write_text("edited\n", encoding="utf-8")
        Path(tmp, "cabal.project.local").write_text("scratch\n",
                                                    encoding="utf-8")
        paths = local_changed_paths(tmp)
        if "tools/save_compat_audit.py" not in paths:
            failures.append(
                "--local-changed-paths missed a path committed on the "
                f"branch: {paths}")
        if "README.md" not in paths:
            failures.append(
                "--local-changed-paths missed an unstaged edit to a tracked "
                f"file: {paths}")
        if "cabal.project.local" in paths:
            failures.append(
                "--local-changed-paths listed an UNTRACKED scratch file; "
                "tools/ci-local.sh's own cabal.project.local would then "
                f"select gates: {paths}")
        if not selected("save-compat", paths):
            failures.append(
                "a save-touching local change did not select the "
                f"save-compat gate: {paths}")
        if selected("worldgen", paths):
            failures.append(
                "a save-only local change selected the worldgen gate: "
                f"{paths}")

    with tempfile.TemporaryDirectory() as tmp:
        new_repo(tmp)
        Path(tmp, "scripts").mkdir()
        Path(tmp, "scripts/unit_ai.lua").write_text("-- base\n",
                                                    encoding="utf-8")
        run(["add", "scripts/unit_ai.lua"], tmp)
        run(["commit", "--quiet", "--no-verify", "-m", "base"], tmp)
        run(["checkout", "--quiet", "-b", "feature"], tmp)
        Path(tmp, "scripts/unit_ai.lua").write_text("-- edited\n",
                                                    encoding="utf-8")
        paths = local_changed_paths(tmp)
        if selected("save-compat", paths):
            failures.append(
                "an unrelated local change selected the save-compat gate, "
                f"so `make ci` would still pay for the repl: {paths}")

    with tempfile.TemporaryDirectory() as tmp:
        # cabal.project.local is not gitignored, so a change CAN track
        # one -- and cabal applies it, so CI's save-compat gate selects
        # on it. A TRACKED edit to it must therefore be listed here too.
        # This is exactly why tools/ci-local.sh resolves its changed
        # paths BEFORE writing its own scratch copy: after the write,
        # this listing could not tell the candidate's edit from the
        # gate's own.
        new_repo(tmp)
        Path(tmp, "cabal.project.local").write_text("-- base\n",
                                                    encoding="utf-8")
        run(["add", "cabal.project.local"], tmp)
        run(["commit", "--quiet", "--no-verify", "-m", "base"], tmp)
        run(["checkout", "--quiet", "-b", "feature"], tmp)
        Path(tmp, "cabal.project.local").write_text("-- edited\n",
                                                    encoding="utf-8")
        paths = local_changed_paths(tmp)
        if "cabal.project.local" not in paths:
            failures.append(
                "--local-changed-paths dropped a TRACKED cabal.project.local "
                f"edit, which cabal would apply in CI: {paths}")
        if not selected("save-compat", paths):
            failures.append(
                "a tracked cabal.project.local edit did not select the "
                f"save-compat gate: {paths}")

    with tempfile.TemporaryDirectory() as tmp:
        # No commits at all, and therefore no default branch to diff
        # against: the conservative answer is to run everything.
        new_repo(tmp)
        paths = local_changed_paths(tmp)
        if paths != [CONSERVATIVE_SENTINEL]:
            failures.append(
                "an unresolvable base did not yield the conservative "
                f"sentinel: {paths}")
        elif not selected("save-compat", paths):
            failures.append(
                "the conservative sentinel did not select the save-compat "
                "gate")

    return failures


def self_test() -> int:
    cases = [
        ("worldgen", ["src/World/Geology/Timeline.hs"], True),
        # Facade modules sitting NEXT to their directory must match too —
        # the original `Name/*` globs silently missed these.
        ("worldgen", ["src/World/Generate.hs"], True),
        ("worldgen", ["src/World/Fluids.hs"], True),
        ("worldgen", ["src/World/Plate.hs"], True),
        ("worldgen", ["src/World/Magma/Pool.hs"], True),
        ("worldgen", ["src/World/Material/Id.hs"], True),
        ("worldgen", ["src/World/Weather.hs"], True),
        # The stages a bare --dump reads THROUGH (#1318): the simulation
        # settle and the world-thread writeback that overwrite the terrain,
        # surface and fluid fields the dump prints. Both a top-level and a
        # nested src/Sim path are pinned, so the whole-tree pattern cannot
        # be narrowed back to a handful of literals without failing here.
        ("worldgen", ["src/Sim/Thread.hs"], True),
        ("worldgen", ["src/Sim/Fluid/Active.hs"], True),
        ("worldgen", ["src/Sim/Fluid/Types.hs"], True),
        ("worldgen", ["src/Sim/State/Types.hs"], True),
        ("worldgen", ["src/Sim/Command/Types.hs"], True),
        # A module added to that tree later must select the gate too.
        ("worldgen", ["src/Sim/Fluid/Future.hs"], True),
        ("worldgen", ["src/World/Thread.hs"], True),
        ("worldgen", ["src/World/Thread/ChunkLoading.hs"], True),
        ("worldgen", ["src/World/Thread/Command.hs"], True),
        ("worldgen", ["src/World/Thread/Command/Init.hs"], True),
        # Non-generation src/World subtrees must NOT trigger the gate — a
        # save/designation/cursor/power change never shifts bare --dump
        # output. These sit right beside the four world-thread paths above,
        # so they also pin that those stayed exact rather than becoming
        # src/World/Thread/* wholesale.
        ("worldgen", ["src/World/Save/Storage.hs"], False),
        ("worldgen", ["src/World/Thread/Command/Save.hs"], False),
        ("worldgen", ["src/World/Thread/Command/Cursor.hs"], False),
        ("worldgen", ["src/World/Thread/Command/Edit.hs"], False),
        ("worldgen", ["src/World/Thread/Cursor.hs"], False),
        ("worldgen", ["src/World/Thread/Power.hs"], False),
        ("worldgen", ["src/World/Mine/Types.hs"], False),
        ("worldgen", ["scripts/unit_ai.lua"], False),
        ("worldgen", ["data/materials/stone.yaml"], True),
        ("graphical", ["src/Engine/Graphics/Vulkan/Device.hs"], True),
        ("graphical", ["test/Test/Engine/Graphics/Vulkan/Device.hs"], True),
        ("graphical", ["scripts/crafting_panel.lua"], False),
        ("graphical", ["synarchy.cabal"], True),
        # unit-assets (#1257). The positives below deliberately lead with
        # paths GRAPHICAL_GLOBS does NOT match: `assets/*` is already a
        # graphical pattern, so a suite whose positives all sat under
        # assets/textures/units/ would still pass if this gate silently
        # fell back to the graphical patterns.
        ("unit-assets", ["data/units/acolyte.yaml"], True),
        ("unit-assets", ["tools/pack_atlas.py"], True),
        # ...and every implementation owner behind the façade (#2054),
        # each pinned by name so the prefix pattern cannot be narrowed to
        # a subset without failing here.
        ("unit-assets", ["tools/pack_atlas_shared.py"], True),
        ("unit-assets", ["tools/pack_atlas_image.py"], True),
        ("unit-assets", ["tools/pack_atlas_declarations.py"], True),
        ("unit-assets", ["tools/pack_atlas_inventory.py"], True),
        ("unit-assets", ["tools/pack_atlas_compiler.py"], True),
        ("unit-assets", ["tools/pack_atlas_index.py"], True),
        ("unit-assets", ["tools/pack_atlas_budget.py"], True),
        ("unit-assets", ["tools/test_pack_atlas.py"], True),
        # Every #2061 self-test owner, named one by one: the prefix
        # pattern above is only load-bearing if each real filename
        # actually matches it.
        ("unit-assets", ["tools/test_pack_atlas_support.py"], True),
        ("unit-assets", ["tools/test_pack_atlas_validation.py"], True),
        ("unit-assets", ["tools/test_pack_atlas_compiler.py"], True),
        ("unit-assets", ["tools/test_pack_atlas_budget.py"], True),
        ("unit-assets", ["tools/ci_expensive_gates.py"], True),
        ("unit-assets", ["tools/ci-local.sh"], True),
        ("unit-assets", ["Makefile"], True),
        ("unit-assets", [".github/workflows/ci.yml"], True),
        ("unit-assets", [".github/ci/Dockerfile"], True),
        ("unit-assets", ["tools/requirements-assets.txt"], True),
        ("unit-assets", ["src/Engine/Asset/YamlUnits.hs"], True),
        ("unit-assets", ["src/Engine/Preview/Unit.hs"], True),
        ("unit-assets", ["src/Engine/Scripting/Lua/API/Units/Yaml.hs"], True),
        ("unit-assets",
         ["test-headless/Test/Headless/Asset/UnitInventory.hs"], True),
        ("unit-assets", ["tools/preview_probe.py"], True),
        # The preview probe's unit family owns the unit-asset expectations
        # since #2089 and must select the gate; the buildings family
        # beside it owns none and must not, which pins the entry as the
        # one owner rather than a tools/preview/* prefix.
        ("unit-assets", ["tools/preview/units.py"], True),
        ("unit-assets", ["tools/preview/buildings.py"], False),
        ("unit-assets", ["scripts/startup_loader.lua"], True),
        ("unit-assets", ["test-headless/Spec.hs"], True),
        ("unit-assets", ["synarchy.cabal"], True),
        ("unit-assets",
         ["assets/textures/units/tiller/animations/idle/south/frame_000.png"],
         True),
        ("unit-assets", ["data/units/white_tailed_deer.yaml"], True),
        # The GENERATED artifacts (#1258/#1260/#1261): an atlas sheet and
        # a unit index. Both are what the freshness comparison and the
        # image budget read, so a hand-edit to either must re-run the
        # gate — and both must be covered explicitly, since the source
        # frames above sit in a different part of the same subtree.
        ("unit-assets", ["assets/textures/units/acolyte/atlas/idle.png"], True),
        ("unit-assets", ["assets/textures/units/acolyte/atlas/index.json"],
         True),
        # The budget policy and the runtime that shares the index and
        # digest contract with the compiler (#1262).
        ("unit-assets", ["tools/unit_texture_budget.json"], True),
        ("unit-assets", ["src/Unit/Atlas/Digest.hs"], True),
        ("unit-assets", ["src/Unit/Atlas/Index.hs"], True),
        # ...and negatives, so the gate cannot be trivially always-true.
        ("unit-assets", ["scripts/crafting_panel.lua"], False),
        ("unit-assets", ["assets/textures/icons/skill/climbing.png"], False),
        ("unit-assets", ["data/materials/stone.yaml"], False),
        ("unit-assets", ["src/World/Geology/Timeline.hs"], False),
        ("unit-assets", ["docs/texture_infrastructure.md"], False),
        ("unit-assets", ["docs/asset_generation.md"], False),
        # A neighbouring unit-ish path that is NOT part of this gate:
        # buildings are never compiled to atlases (D-8), so a building
        # asset must not drag the unit inventory in.
        ("unit-assets",
         ["assets/textures/buildings/acolyte_portal/idle/frame_000.png"],
         False),
        ("unit-assets", ["src/Unit/Render.hs"], False),
        # A path selecting one gate must not drag in the others.
        ("worldgen", ["tools/pack_atlas.py"], False),
        ("worldgen", ["data/units/acolyte.yaml"], False),
        ("graphical", ["data/units/acolyte.yaml"], False),
        ("graphical", ["tools/pack_atlas.py"], False),
        # ...nor do the façade's owner modules (#2054): a pack_atlas
        # module never drags in an unrelated expensive gate.
        ("worldgen", ["tools/pack_atlas_shared.py"], False),
        ("worldgen", ["tools/pack_atlas_image.py"], False),
        ("worldgen", ["tools/pack_atlas_declarations.py"], False),
        ("worldgen", ["tools/pack_atlas_inventory.py"], False),
        ("worldgen", ["tools/pack_atlas_compiler.py"], False),
        ("worldgen", ["tools/pack_atlas_index.py"], False),
        ("worldgen", ["tools/pack_atlas_budget.py"], False),
        ("graphical", ["tools/pack_atlas_shared.py"], False),
        ("graphical", ["tools/pack_atlas_image.py"], False),
        ("graphical", ["tools/pack_atlas_declarations.py"], False),
        ("graphical", ["tools/pack_atlas_inventory.py"], False),
        ("graphical", ["tools/pack_atlas_compiler.py"], False),
        ("graphical", ["tools/pack_atlas_index.py"], False),
        ("graphical", ["tools/pack_atlas_budget.py"], False),
        # ...including the #1318 additions, which are worldgen-only.
        ("graphical", ["src/Sim/Thread.hs"], False),
        ("graphical", ["src/Sim/Fluid/Active.hs"], False),
        ("graphical", ["src/World/Thread.hs"], False),
        ("graphical", ["src/World/Thread/ChunkLoading.hs"], False),
        ("graphical", ["src/World/Thread/Command.hs"], False),
        ("graphical", ["src/World/Thread/Command/Init.hs"], False),
        ("unit-assets", ["src/Sim/Thread.hs"], False),
        ("unit-assets", ["src/World/Thread/Command/Init.hs"], False),
        # save-compat (#1360). Requirement 8: both directions pinned --
        # a save-touching change selects the reproducibility member, an
        # unrelated change does not. The positives walk the whole
        # trigger-path table, so narrowing any entry fails here.
        ("save-compat", ["tools/save_compat_audit.py"], True),
        ("save-compat", ["tools/test_save_compat_audit.py"], True),
        # Issue #2073's self-test owner modules, each reached through the
        # `tools/test_save_compat_audit*.py` prefix rather than a literal
        # of its own. A case per module all the same: the prefix is what
        # makes them select, and a case per module is what proves the
        # prefix still spans the family after one is renamed or another
        # is added.
        ("save-compat", ["tools/test_save_compat_audit_support.py"], True),
        ("save-compat", ["tools/test_save_compat_audit_manifest.py"], True),
        ("save-compat", ["tools/test_save_compat_audit_envelope.py"], True),
        ("save-compat", ["tools/test_save_compat_audit_register.py"], True),
        ("save-compat",
         ["tools/test_save_compat_audit_reproducibility.py"], True),
        ("save-compat", ["tools/test_save_compat_audit_discovery.py"], True),
        ("save-compat", ["tools/test_save_compat_audit_coverage.py"], True),
        # Issue #2049's owner modules. Each is named individually, so a
        # PR touching only one of them still pays for the repl coverage
        # that exercises it -- the codec bridge in particular owns
        # normalize_fixture_timestamp, which is exactly what the
        # reproducibility member proves.
        ("save-compat", ["tools/save_compat_audit_common.py"], True),
        ("save-compat", ["tools/save_compat_audit_components.py"], True),
        ("save-compat", ["tools/save_compat_audit_fingerprint.py"], True),
        ("save-compat", ["tools/save_compat_audit_codec.py"], True),
        ("save-compat", ["tools/save_compat_audit_manifest.py"], True),
        ("save-compat", ["tools/save_compat_audit_register.py"], True),
        ("save-compat", ["tools/save_compat_audit_generate.py"], True),
        ("save-compat", ["docs/save_compat/manifest.json"], True),
        ("save-compat", ["docs/save_compat/enum_baseline.json"], True),
        ("save-compat",
         ["test-headless/data/save-compat/f1-autosave-classification.bin"],
         True),
        ("save-compat",
         ["test-headless/data/save-compat/k1-new-fixture.expected.json"],
         True),
        # The Haskell the GHCi setup imports: the envelope codec and its
        # types, the component registry, the metadata DTO, and the
        # frozen compat mirrors. A facade module added beside the
        # directory later must match too, which is why the pattern is
        # `src/World/Save*` rather than `src/World/Save/*`.
        ("save-compat", ["src/World/Save/Envelope/Codec.hs"], True),
        ("save-compat", ["src/World/Save/Envelope/Types.hs"], True),
        ("save-compat", ["src/World/Save/Envelope.hs"], True),
        ("save-compat", ["src/World/Save/Component.hs"], True),
        ("save-compat", ["src/World/Save/Component/Session.hs"], True),
        ("save-compat", ["src/World/Save/Types.hs"], True),
        ("save-compat", ["src/World/Save/Compat/SessionV90.hs"], True),
        ("save-compat", ["src/World/Save.hs"], True),
        # The build definition the repl target resolves from.
        ("save-compat", ["synarchy.cabal"], True),
        ("save-compat", ["cabal.project"], True),
        ("save-compat", ["cabal.project.freeze"], True),
        # cabal.project.local is not gitignored, so a change CAN track
        # one, and cabal applies it in CI -- it has to select. The gate
        # this script's own scratch copy would otherwise trip is closed
        # by tools/ci-local.sh capturing its changed-path list BEFORE it
        # writes that file, not by excluding the path here.
        ("save-compat", ["cabal.project.local"], True),
        ("save-compat", [".github/ci/Dockerfile"], True),
        # The reusable image workflow is the OTHER half of the image
        # identity hash, so a PR editing only it still changes the
        # toolchain the repl runs under.
        ("save-compat", [".github/workflows/ci-image.yml"], True),
        # The wiring on both sides, and the parity audit over it.
        ("save-compat", ["tools/ci_expensive_gates.py"], True),
        ("save-compat", ["tools/ci_parity_audit.py"], True),
        # Issue #2159's owner modules. Each is named individually in
        # SAVE_COMPAT_GLOBS, so a PR touching only one still pays for the
        # coverage whose selection that module decides -- the save-compat
        # owner most of all, since it IS the wiring check.
        ("save-compat", ["tools/ci_parity_shell.py"], True),
        ("save-compat", ["tools/ci_parity_config.py"], True),
        ("save-compat", ["tools/ci_parity_workflow.py"], True),
        ("save-compat", ["tools/ci_parity_save_compat.py"], True),
        ("save-compat", ["tools/test_ci_parity_audit.py"], True),
        ("save-compat", ["tools/ci-local.sh"], True),
        ("save-compat", ["Makefile"], True),
        ("save-compat", [".github/workflows/ci.yml"], True),
        # ...and the negatives, so the gate cannot be trivially
        # always-true. An unrelated PR -- gameplay Lua, worldgen, unit
        # art, a doc -- must not pay for the repl.
        ("save-compat", ["scripts/unit_ai.lua"], False),
        ("save-compat", ["src/World/Geology/Timeline.hs"], False),
        ("save-compat", ["src/World/Thread/Command/Init.hs"], False),
        ("save-compat", ["src/Unit/Atlas/Digest.hs"], False),
        ("save-compat", ["data/units/acolyte.yaml"], False),
        ("save-compat", ["docs/persistence_contract.md"], False),
        ("save-compat", ["docs/code_health_findings.md"], False),
        ("save-compat", ["tools/world_check.py"], False),
        # ...and the workflows that are NOT the toolchain: naming
        # ci-image.yml exactly rather than .github/workflows/* keeps
        # these out.
        ("save-compat", [".github/workflows/ntfy-notify.yml"], False),
        ("save-compat", [".github/workflows/review-gate.yml"], False),
        ("save-compat", ["tools/pack_atlas.py"], False),
        ("save-compat", ["tools/pack_atlas_shared.py"], False),
        ("save-compat", ["tools/pack_atlas_image.py"], False),
        ("save-compat", ["tools/pack_atlas_declarations.py"], False),
        ("save-compat", ["tools/pack_atlas_inventory.py"], False),
        ("save-compat", ["tools/pack_atlas_compiler.py"], False),
        ("save-compat", ["tools/pack_atlas_index.py"], False),
        ("save-compat", ["tools/pack_atlas_budget.py"], False),
        # The save-adjacent Haskell that is NOT the format: the world
        # thread's save command and the barrier live outside
        # src/World/Save, and a save PROBE is not the fixture corpus.
        ("save-compat", ["src/World/Thread/Command/Save.hs"], False),
        ("save-compat", ["src/Engine/Save/Barrier.hs"], False),
        ("save-compat", ["tools/save_storage_probe.py"], False),
        # ...and a save-compat-PREFIXED probe is still not the tool: the
        # patterns above name each owner module exactly, so this stays
        # unselected. A blanket `tools/save_compat*` would capture it.
        ("save-compat", ["tools/save_compat_migration_probe.py"], False),
        # A path selecting one gate must not drag in the others, in
        # either direction.
        ("worldgen", ["tools/test_save_compat_audit.py"], False),
        ("worldgen", ["tools/test_save_compat_audit_coverage.py"], False),
        ("worldgen", ["tools/save_compat_audit_codec.py"], False),
        ("unit-assets", ["tools/save_compat_audit_manifest.py"], False),
        ("unit-assets",
         ["tools/test_save_compat_audit_discovery.py"], False),
        ("worldgen", ["src/World/Save/Envelope/Codec.hs"], False),
        ("unit-assets", ["src/World/Save/Envelope/Codec.hs"], False),
        ("unit-assets", ["docs/save_compat/manifest.json"], False),
        ("save-compat", ["assets/textures/units/acolyte/atlas/idle.png"],
         False),
        # The unresolved-base sentinel selects EVERY gate (#1360): a
        # local gate that cannot tell what changed runs the coverage.
        ("save-compat", [CONSERVATIVE_SENTINEL], True),
        ("worldgen", [CONSERVATIVE_SENTINEL], True),
        ("graphical", [CONSERVATIVE_SENTINEL], True),
        ("unit-assets", [CONSERVATIVE_SENTINEL], True),
        # ...and it is a sentinel, not a pattern: a path that merely
        # resembles it must not select anything.
        ("save-compat", ["!!ci-expensive-gates:something-else!!"], False),
    ]
    failures = []
    for gate, files, expected in cases:
        actual = selected(gate, files)
        if actual != expected:
            failures.append(f"{gate} {files}: expected {expected}, got {actual}")

    # Every gate the CLI accepts must have its own pattern list, and
    # every pattern list must be reachable from the CLI. The dispatch
    # bug this replaced was exactly a gate name the CLI accepted while
    # `selected` had no patterns of its own for it.
    cli_gates = set(GATE_CHOICES)
    if cli_gates != set(GATE_GLOBS):
        failures.append(
            f"--gate choices {sorted(cli_gates)} do not match the gates with "
            f"patterns {sorted(GATE_GLOBS)}")
    for gate in sorted(cli_gates & set(GATE_GLOBS)):
        if not any(case_gate == gate for case_gate, _, _ in cases):
            failures.append(f"gate '{gate}' has no self-test case")
        if not any(case_gate == gate and expected
                   for case_gate, _, expected in cases):
            failures.append(f"gate '{gate}' has no POSITIVE self-test case")
        if not any(case_gate == gate and not expected
                   for case_gate, _, expected in cases):
            failures.append(f"gate '{gate}' has no NEGATIVE self-test case")
    try:
        selected("no-such-gate", ["anything"])
    except ValueError:
        pass
    else:
        failures.append(
            "an unknown gate name did not raise — it silently inherited "
            "another gate's patterns")
    # ...including when the conservative sentinel is present: "run
    # everything" must not become "accept any gate name".
    try:
        selected("no-such-gate", [CONSERVATIVE_SENTINEL])
    except ValueError:
        pass
    else:
        failures.append(
            "an unknown gate name was answered True by the conservative "
            "sentinel instead of raising")

    failures.extend(_local_changed_paths_failures())

    if failures:
        for failure in failures:
            print(f"SELF-TEST FAIL: {failure}", file=sys.stderr)
        return 1
    print("ci_expensive_gates self-test: all cases pass")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--gate", choices=GATE_CHOICES)
    parser.add_argument("--changed", nargs="*", default=[])
    parser.add_argument("--stdin", action="store_true")
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument(
        "--local-changed-paths", action="store_true",
        help="print the changed paths `make ci` should judge itself by, "
             "one per line, for piping back into --stdin (#1360).")
    args = parser.parse_args()
    if args.self_test:
        return self_test()
    if args.local_changed_paths:
        if args.gate:
            parser.error(
                "--local-changed-paths prints paths; pipe them into a "
                "separate --stdin --gate run rather than combining the two, "
                "so `make ci` reaches its decision through the same command "
                "CI runs.")
        for path in local_changed_paths():
            print(path)
        return 0
    if not args.gate:
        parser.error(
            "--gate is required unless --self-test or "
            "--local-changed-paths is used")
    files = list(args.changed)
    if args.stdin:
        files.extend(line.strip() for line in sys.stdin if line.strip())
    print(str(selected(args.gate, files)).lower())
    return 0


if __name__ == "__main__":
    sys.exit(main())
