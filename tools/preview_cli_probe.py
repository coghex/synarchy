#!/usr/bin/env python3
"""No-boot CLI-contract probe (#886 --preview epic #427; #1012 CH-58; #1191).

Every check here is a PRE-BOOT rejection or exit — no GPU, no window, no
engine thread ever starts (App.Main/App.Cli/Engine.Preview.Discovery
reject before 'App.Preview.runPreview' is ever called). This is what makes
the probe CI-eligible: a classifier or path-containment regression fails
PRs directly instead of waiting for a manual, needs-gpu dev-machine run
(tools/preview_probe.py, which keeps the real-boot browser checks).

It started as the --preview contract (#886), which is still checks 1-8,
and now covers the whole argv-to-dispatch layer: mode compatibility
(#1012/CH-58, check 9) and handled-value validation (#1191, check 10).
The filename predates that widening.

Checks:
  1. A bare --preview (no target at all) errors and exits 1, no silent
     fallthrough to the normal boot path (regression risk: hangs on a
     real graphical boot instead).
  2. Every explicitly unexposed category name (equipment, hud, facemap,
     utility, vegetation) is an ordinary unknown-category error listing
     exactly the canonical set — no compatibility aliases.
  3. Every grouped category (units, flora, buildings, structures,
     including structures — the #428-reorganization addition) with no
     item prints the "select a specific ..." guidance and exits 0.
  4. A nonexistent simple-category item path rejects before boot.
  5. Path-containment: an absolute item path, a leading ".." traversal,
     and a ".." component in the middle of the path all reject before
     boot (never touching a file outside the category root).
  6. A directory given as a simple-category item rejects before boot.
  7. --preview units/<name> (#887): an unknown unit, a name carrying
     path structure, absolute/".."/"." traversal shapes, and a unit
     directory with no animations/ subtree all reject before boot —
     exactly like the simple-category rejections above, so a bad unit
     target can never reach a window either. A KNOWN unit is
     deliberately NOT booted here (that would open a real GLFW window,
     which is why tools/preview_probe.py stays manual-only/needs-gpu).
  8. --preview <flora|buildings|structures>/<item> (#888): the same
     pre-boot rejections for the remaining grouped categories — an
     unknown item, a name carrying path structure, absolute/".."/"."
     traversal shapes, a symlinked item directory, and a FILE where a
     browsable item directory was expected (assets/textures/flora holds
     unknown_flora.png beside its real species folders). Valid targets
     are deliberately NOT booted here (that would open a real GLFW
     window — see tools/preview_probe.py).
  9. Mode-specific flags (CH-58, #1012): a flag from app/Main.hs's
     incompatibleFlagTable given to a boot mode that doesn't honour it
     (e.g. --seed with --headless, --port with --dump, --seeds with
     --dump) exits 1 before any engine/window/server starts, naming
     both the flag and the selected mode in stderr — one case per row
     of the table, including the distinct --plates/--ages spellings and
     the --language-report/--seeds pairing.
 9b. Boot-mode precedence (#1086): argv naming TWO competing mode
     selectors resolves to the higher-precedence one, asserted through
     the incompatible-flag rejection's exact stderr line and exit code
     — one case per boundary of
     language-report > dump > preview > offscreen > headless. Since
     app/Main.hs now derives ONE boot-mode value and feeds it to both
     the rejection and the dispatch, the mode these lines name is the
     mode that would have booted; before, the two encodings could
     disagree and only this text would have said so. (The last
     boundary, headless > graphical, has no mixed-selector form —
     graphical is what argv naming no selector resolves to — and is
     covered by check 9's own --headless rows plus hspec
     `--match "App.Cli"`.)
 10. Present-but-malformed values (#1191, #1481): every affected
     spelling (--seed/--worldSize/--plates/--ages/--port), an empty and
     an unknown --dump= layer selection plus empty segments, a
     malformed and a non-positive --size, and every malformed --region
     shape (non-numeric, too few, too many, partially numeric, and no
     operand at all) exit 1 pre-boot naming the flag and the offending
     token — never the silent fall-through to a default that made
     `--seed not-a-number` produce a full, valid, WRONG dump at seed 42,
     or `--region bogus` dump the wrong 17x17 chunks of the world
     (CH-67, closed by #1481). Also pins the two orderings the fix has
     to preserve: validation runs ahead of mode-specific early exits and
     regardless of whether the value would be consumed (a malformed
     --port fails even for a bare grouped --preview; a malformed --ages
     fails even when a valid --plates wins), while check 9's
     mode-compatibility rejection still takes priority over it (a
     malformed --seed or --region given to --headless is reported as
     unsupported in headless mode, not as malformed). Omitting a flag
     entirely still keeps its documented default, --region's included.
     The pure four-outcome parser coverage is hspec
     `--match "App.Cli"`.
 10d. An explicit --dump= selection really emits those layers and no
     others (#1086): the prefix strip is stripPrefix now rather than a
     separately-maintained length, and a real dump is what proves the
     selected set is unchanged — a strip one character too long would
     still have parsed SOME selection.

Usage:
  python3 tools/preview_cli_probe.py

Exit 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys

import probe_engine

LOG = "/tmp/preview_cli_probe_engine.log"

UNEXPOSED_CATEGORIES = ["equipment", "hud", "facemap", "utility", "vegetation"]
GROUPED_CATEGORIES = ["units", "flora", "buildings", "structures"]
CANONICAL_LIST_TEXT = "icons, items, ui, world, units, flora, buildings, structures"


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f"  ({detail})" if detail else ""))
    return ok


def run_cli(*extra_args: str, timeout: float = 30.0) -> subprocess.CompletedProcess:
    # The aggregate runner resolves ONE executable up front and hands it
    # over through the environment (#1570), so this probe never adds a
    # `cabal run` to a parallel sweep; run by hand it keeps the same
    # `cabal run` fallback, with the same arguments and the same cwd.
    cmd = probe_engine.engine_command(extra_args)
    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)


def check_missing_target() -> bool:
    print("1. bare --preview (no target at all): exit 1, no READY, no silent fallthrough")
    # A regression here falls through to a real graphical boot, which
    # blocks indefinitely (a window waiting on user input) rather than
    # exiting — bound the wait so a regression FAILs fast instead of
    # hanging the probe, and never sits out the full default timeout.
    try:
        r = run_cli("--preview", timeout=15.0)
    except subprocess.TimeoutExpired:
        return check("missing-target", False,
                     "process did not exit within 15s — likely fell through "
                     "to a real graphical boot instead of erroring")
    ok = (r.returncode == 1
          and "READY" not in r.stdout
          and "--preview requires a target" in r.stderr)
    return check("missing-target", ok,
                 f"rc={r.returncode} stderr={r.stderr.strip()!r}")


def check_unexposed_category(cat: str) -> bool:
    r = run_cli("--preview", cat)
    stderr = r.stderr.strip()
    ok = (r.returncode == 1
          and "READY" not in r.stdout
          and cat in stderr
          and CANONICAL_LIST_TEXT in stderr)
    return check(f"unexposed category '{cat}'", ok, f"rc={r.returncode} stderr={stderr!r}")


def check_grouped_no_item(cat: str) -> bool:
    r = run_cli("--preview", cat)
    ok = (r.returncode == 0
          and "READY" not in r.stdout
          and f"select a specific {cat}" in r.stdout)
    return check(f"grouped category '{cat}', no item", ok,
                 f"rc={r.returncode} stdout={r.stdout.strip()!r}")


def check_nonexistent_simple_item() -> bool:
    print("4. nonexistent simple-category item: exit 1, no READY, pre-boot")
    r = run_cli("--preview", "icons/this/does/not/exist.png")
    ok = (r.returncode == 1
          and "READY" not in r.stdout
          and "no such texture" in r.stderr)
    return check("nonexistent-simple-item", ok,
                 f"rc={r.returncode} stderr={r.stderr.strip()!r}")


def check_path_containment() -> bool:
    print("5. path containment: absolute / .. traversal reject before boot")
    results = []
    for label, target in [
        ("absolute path", "icons//etc/passwd"),
        ("leading .. traversal", "icons/../../../etc/passwd"),
        ("mid-path .. traversal", "icons/skill/../../ui/box"),
    ]:
        r = run_cli("--preview", target)
        ok = (r.returncode == 1
              and "READY" not in r.stdout
              and "must stay within the category" in r.stderr)
        results.append(check(f"containment: {label}", ok,
                             f"rc={r.returncode} stderr={r.stderr.strip()!r}"))
    return all(results)


def check_directory_as_item() -> bool:
    print("6. a directory given as the item: exit 1, no READY, pre-boot")
    r = run_cli("--preview", "icons/skill")
    ok = (r.returncode == 1
          and "READY" not in r.stdout
          and "directory" in r.stderr)
    return check("directory-as-item", ok,
                 f"rc={r.returncode} stderr={r.stderr.strip()!r}")


def check_unit_targets() -> bool:
    """#887: every --preview units/<name> rejection, pre-boot.

    The expected substrings come straight from
    Engine.Preview.Unit.unitFocusErrorMessage — a wording change there
    without one here is exactly the drift this catches.
    """
    print("7. units/<name> pre-boot rejections (#887): unknown / unsafe / "
          "no-animations")
    results = []
    for label, target, expect in [
        ("unknown unit", "units/nosuch", "no such unit"),
        ("name with path structure", "units/acolyte/animations",
         "must be a single directory name"),
        ("absolute path", "units//etc", "must be a single directory name"),
        ("leading .. traversal", "units/../../etc",
         "must be a single directory name"),
        ("bare dot", "units/.", "must be a single directory name"),
        ("bare dot-dot", "units/..", "must be a single directory name"),
    ]:
        r = run_cli("--preview", target)
        ok = (r.returncode == 1
              and "READY" not in r.stdout
              and expect in r.stderr)
        results.append(check(f"units: {label}", ok,
                             f"rc={r.returncode} stderr={r.stderr.strip()!r}"))

    # A real directory under assets/textures/units that holds no
    # animations/ subtree: the one rejection that needs a fixture,
    # created and removed here so the repo tree is never left dirty.
    empty_unit = os.path.join("assets", "textures", "units",
                              "_cli_probe_empty_887")
    created = False
    try:
        if not os.path.exists(empty_unit):
            os.mkdir(empty_unit)
            created = True
        r = run_cli("--preview", "units/_cli_probe_empty_887")
        ok = (r.returncode == 1
              and "READY" not in r.stdout
              and "no animations" in r.stderr)
        results.append(check("units: directory with no animations/ subtree", ok,
                             f"rc={r.returncode} stderr={r.stderr.strip()!r}"))
    finally:
        if created:
            os.rmdir(empty_unit)

    return all(results)


def check_grouped_item_targets() -> bool:
    """#888: every --preview <flora|buildings|structures>/<item>
    rejection, pre-boot.

    The expected substrings come straight from
    Engine.Preview.Discovery.itemDirErrorMessage — the ONE containment
    rule every grouped category now shares (units restates the same
    outcomes in its own vocabulary, checked above) — so a wording change
    there without one here is exactly the drift this catches.
    """
    print("8. flora/buildings/structures item pre-boot rejections (#888): "
          "unknown / unsafe / not-a-directory")
    results = []
    structure_msg = "must be a single directory name"
    for label, target, expect in [
        ("unknown flora", "flora/nosuch", "no such item"),
        ("unknown building", "buildings/nosuch", "no such item"),
        ("unknown structure", "structures/nosuch", "no such item"),
        ("flora name with path structure", "flora/scots_pine/matured.png",
         structure_msg),
        ("building name with path structure", "buildings/acolyte_portal/idle",
         structure_msg),
        ("structures absolute path", "structures//etc", structure_msg),
        ("buildings leading .. traversal", "buildings/../../etc", structure_msg),
        ("flora bare dot", "flora/.", structure_msg),
        ("flora bare dot-dot", "flora/..", structure_msg),
        # A REAL file sitting beside the real item directories: every
        # grouped category root ships an unknown_<category>.png fallback
        # texture, which must not browse as if it were an item folder.
        ("flora file where a directory is required", "flora/unknown_flora.png",
         "is a file, not a browsable item directory"),
    ]:
        r = run_cli("--preview", target)
        ok = (r.returncode == 1
              and "READY" not in r.stdout
              and expect in r.stderr)
        results.append(check(f"grouped item: {label}", ok,
                             f"rc={r.returncode} stderr={r.stderr.strip()!r}"))

    # A symlinked item directory: refused unconditionally, because
    # doesDirectoryExist follows links and browsing one would load
    # another tree's textures (breaking trimmed loading). Created and
    # removed here so the repo tree is never left dirty.
    link = os.path.join("assets", "textures", "flora", "_cli_probe_link_888")
    created = False
    try:
        if not os.path.exists(link) and not os.path.islink(link):
            os.symlink("scots_pine", link)
            created = True
        r = run_cli("--preview", "flora/_cli_probe_link_888")
        ok = (r.returncode == 1
              and "READY" not in r.stdout
              and "must not be a symlink" in r.stderr)
        results.append(check("grouped item: symlinked item directory", ok,
                             f"rc={r.returncode} stderr={r.stderr.strip()!r}"))
    finally:
        if created:
            os.unlink(link)

    return all(results)


# (argv beyond the incompatible flag itself, the rejected flag, the
# selected boot mode) — one row per app/Main.hs's incompatibleFlagTable
# entry (CH-58, #1012). Every case rejects pre-boot, so none of these
# ever bind a port or touch the GPU.
INCOMPATIBLE_FLAG_CASES = [
    (["--headless", "--seed", "42"], "--seed", "headless"),
    (["--headless", "--worldSize", "64"], "--worldSize", "headless"),
    (["--headless", "--plates", "3"], "--plates", "headless"),
    (["--headless", "--ages", "3"], "--ages", "headless"),
    (["--headless", "--region", "0,0,1,1"], "--region", "headless"),
    (["--headless", "--size", "100x100"], "--size", "headless"),
    (["--dump", "--port", "9099"], "--port", "dump"),
    (["--dump", "--arena"], "--arena", "dump"),
    (["--dump", "--seeds", "0:1"], "--seeds", "dump"),
    (["--language-report", "--seed", "42"], "--seed", "language-report"),
]


# (argv, rejected flag, the mode precedence must select, the honoured
# list that mode is missing from) — one row per precedence boundary
# (#1086). Every argv here names TWO mode selectors, so the mode in the
# rejection is evidence of which one won; each rejects pre-boot.
PRECEDENCE_FLAG_CASES = [
    # language-report > dump: --seed is honoured only in dump.
    (["--language-report", "--dump", "--seed", "42"],
     "--seed", "language-report", "dump"),
    # dump > preview: --port is honoured in preview, not in dump.
    (["--dump", "--preview", "icons", "--port", "9099"],
     "--port", "dump", "headless, graphical, offscreen, preview"),
    # preview > offscreen: --size is honoured only in offscreen.
    (["--preview", "icons", "--offscreen", "--size", "100x100"],
     "--size", "preview", "offscreen"),
    # offscreen > headless: --seed is honoured only in dump, and the
    # mode named is what tells those two apart.
    (["--offscreen", "--headless", "--seed", "42"],
     "--seed", "offscreen", "dump"),
]


def check_boot_mode_precedence() -> bool:
    print("9b. boot-mode precedence with two competing selectors "
          "(#1086): the rejection names the mode that would have booted")
    results = []
    for extra_args, flag, mode, honoured in PRECEDENCE_FLAG_CASES:
        expected = (f"{flag} is not supported in {mode} mode "
                    f"(only honoured in {honoured})")
        try:
            r = run_cli(*extra_args, timeout=15.0)
        except subprocess.TimeoutExpired:
            results.append(check(f"precedence: {' '.join(extra_args)}", False,
                                 "process did not exit within 15s — a mode "
                                 "selector reached a real boot"))
            continue
        ok = (r.returncode == 1
              and "READY" not in r.stdout
              and expected in r.stderr)
        results.append(check(f"precedence: {' '.join(extra_args)} -> {mode}",
                             ok, f"rc={r.returncode} want={expected!r} "
                                 f"stderr={r.stderr.strip()!r}"))
    return all(results)


def check_incompatible_flags() -> bool:
    print("9. mode-specific flags rejected in modes that ignore them "
          "(CH-58): exit 1, pre-boot")
    results = []
    for extra_args, flag, mode in INCOMPATIBLE_FLAG_CASES:
        r = run_cli(*extra_args, timeout=15.0)
        ok = (r.returncode == 1
              and "READY" not in r.stdout
              and flag in r.stderr
              and mode in r.stderr)
        results.append(check(f"incompatible: {flag} given to {mode} mode", ok,
                             f"rc={r.returncode} stderr={r.stderr.strip()!r}"))
    return all(results)


# (label, extra args, substrings every one of which must appear in stderr).
# Each case exits 1 pre-boot with NO READY on stdout and NO dump JSON on
# stdout. Modes are chosen so check 9's compatibility table does not fire
# first: the numeric flags ride --dump (which honours them), --port rides
# --headless/--offscreen/--preview, and --size rides --offscreen.
MALFORMED_VALUE_CASES = [
    ("--seed",
     ["--dump", "--seed", "not-a-number", "--worldSize", "16",
      "--region", "0,0,0,0"],
     ["--seed", "not-a-number"]),
    ("--worldSize",
     ["--dump", "--worldSize", "sixteen", "--region", "0,0,0,0"],
     ["--worldSize", "sixteen"]),
    ("--plates",
     ["--dump", "--worldSize", "16", "--plates", "many",
      "--region", "0,0,0,0"],
     ["--plates", "many"]),
    ("--ages",
     ["--dump", "--worldSize", "16", "--ages", "many",
      "--region", "0,0,0,0"],
     ["--ages", "many"]),
    ("--port",
     ["--headless", "--port", "not-a-port"],
     ["--port", "not-a-port"]),
    # A handled flag with no operand at all is present-but-invalid, not
    # absence — it must name the flag rather than silently default.
    ("--port with no operand", ["--headless", "--port"], ["--port"]),
    ("--size", ["--offscreen", "--size", "not-a-size"],
     ["--size", "not-a-size"]),
    # Positivity stays a --size-specific rule, and rejects rather than
    # falling back to the video-config resolution.
    ("--size non-positive", ["--offscreen", "--size", "0x100"],
     ["--size", "0x100"]),
    ("--dump= empty selection", ["--dump="], ["--dump=", "terrain"]),
    ("--dump= unknown layer",
     ["--dump=bogus_layer_typo", "--worldSize", "16", "--region", "0,0,0,0"],
     ["--dump=", "bogus_layer_typo"]),
    # An empty SEGMENT is reported as empty, not as an unknown layer "".
    ("--dump= trailing empty segment",
     ["--dump=terrain,", "--worldSize", "16", "--region", "0,0,0,0"],
     ["--dump=", "empty"]),
    ("--dump= interior empty segment",
     ["--dump=terrain,,fluid", "--worldSize", "16", "--region", "0,0,0,0"],
     ["--dump=", "empty"]),
    # #1191 eager validation: a malformed value must fail even when the
    # selected mode would exit before ever consuming it. A bare grouped
    # --preview otherwise prints its guidance and exits 0.
    ("--port for a bare grouped --preview (never consumed)",
     ["--preview", "units", "--port", "not-a-port"],
     ["--port", "not-a-port"]),
    # ...and even when a valid --plates takes precedence over --ages.
    ("--ages while a valid --plates wins",
     ["--dump", "--worldSize", "16", "--plates", "3", "--ages", "nonsense",
      "--region", "0,0,0,0"],
     ["--ages", "nonsense"]),
    # --region (#1481, CH-67) was the last flag still answering a typo
    # with its documented default. One row per malformed SHAPE the old
    # parser swallowed, since each reaches the rejection differently.
    ("--region non-numeric",
     ["--dump", "--worldSize", "16", "--region", "bogus"],
     ["--region", "bogus"]),
    ("--region too few coordinates",
     ["--dump", "--worldSize", "16", "--region", "1,2,3"],
     ["--region", "1,2,3"]),
    ("--region too many coordinates",
     ["--dump", "--worldSize", "16", "--region", "1,2,3,4,5"],
     ["--region", "1,2,3,4,5"]),
    ("--region partially numeric",
     ["--dump", "--worldSize", "16", "--region", "1,2,3,x"],
     ["--region", "1,2,3,x"]),
    ("--region with no operand",
     ["--dump", "--worldSize", "16", "--region"],
     ["--region"]),
]

# Mode compatibility (check 9) must keep its priority over value
# validation: these name the MODE, and must NOT report the value as
# malformed.
MALFORMED_IN_WRONG_MODE_CASES = [
    (["--headless", "--seed", "not-a-number"], "--seed", "headless"),
    (["--dump", "--port", "not-a-port"], "--port", "dump"),
    (["--headless", "--region", "nonsense"], "--region", "headless"),
]


def check_malformed_values() -> bool:
    print("10. present-but-malformed values rejected pre-boot (#1191): "
          "exit 1, flag + offending token named, no default substituted")
    results = []
    for label, extra_args, expected in MALFORMED_VALUE_CASES:
        try:
            r = run_cli(*extra_args, timeout=30.0)
        except subprocess.TimeoutExpired:
            results.append(check(f"malformed {label}", False,
                                 "process did not exit within 30s — a "
                                 "malformed value reached a real boot"))
            continue
        problems = []
        # app/Main.hs's orExitCli defines CliError rejection as
        # ExitFailure 1 specifically, so "nonzero" is too weak a
        # reading of the contract.
        if r.returncode != 1:
            problems.append(f"exit status {r.returncode} (want 1)")
        if "READY" in r.stdout:
            problems.append("a READY marker reached stdout")
        if r.stdout.strip():
            problems.append(f"stdout was not empty: {r.stdout.strip()[:60]!r}")
        for want in expected:
            if want not in r.stderr:
                problems.append(f"stderr never names {want!r}")
        results.append(check(f"malformed {label}", not problems,
                             f"rc={r.returncode} " + ("; ".join(problems)
                             if problems else r.stderr.strip()[:80])))
    return all(results)


def check_mode_priority_over_value() -> bool:
    print("10b. mode compatibility still outranks value validation: a "
          "malformed value in a mode that ignores the flag names the MODE")
    results = []
    for extra_args, flag, mode in MALFORMED_IN_WRONG_MODE_CASES:
        r = run_cli(*extra_args, timeout=30.0)
        ok = (r.returncode == 1
              and "READY" not in r.stdout
              and f"{flag} is not supported in {mode} mode" in r.stderr
              and "invalid value" not in r.stderr)
        results.append(check(f"{flag} malformed in {mode} mode", ok,
                             f"rc={r.returncode} stderr={r.stderr.strip()!r}"))
    return all(results)


def check_omission_still_defaults() -> bool:
    """#1191 requirement 4: only a PRESENT malformed value is an error."""
    print("10c. omitting a flag still keeps its documented default")
    try:
        r = run_cli("--dump", "--worldSize", "16", "--region", "0,0,0,0",
                    timeout=180.0)
    except subprocess.TimeoutExpired:
        return check("omission defaults", False, "dump did not finish in 180s")
    problems = []
    if r.returncode != 0:
        problems.append(f"exit status {r.returncode} (want 0)")
    # The dump banner echoes the effective values; an omitted --seed must
    # still be 42 and an omitted --plates must still be derived.
    if "seed=42" not in r.stderr:
        problems.append("omitted --seed no longer defaults to 42")
    try:
        parsed = json.loads(r.stdout)
        if not parsed:
            problems.append("dump produced no tiles")
    except (json.JSONDecodeError, ValueError) as e:
        problems.append(f"stdout is not valid JSON: {e}")
    results = [check("omission defaults", not problems,
                     f"rc={r.returncode} " + "; ".join(problems))]

    # --region's own default is checked separately because the run above
    # pins it to one chunk for speed, so its banner cannot show it
    # (#1481). Now that a malformed --region is rejected, the default
    # must still be reachable by OMITTING the flag — the banner is what
    # names the region actually dumped, and the parser's own Right
    # Nothing is hspec `--match "chunk region"`.
    try:
        rd = run_cli("--dump", "--worldSize", "16", timeout=180.0)
    except subprocess.TimeoutExpired:
        return all(results + [check("omitted --region still defaults",
                                    False, "dump did not finish in 180s")])
    region_problems = []
    if rd.returncode != 0:
        region_problems.append(f"exit status {rd.returncode} (want 0)")
    if "region=(-8,-8,8,8)" not in rd.stderr:
        region_problems.append(
            "banner does not name the documented default region: "
            f"{rd.stderr.strip().splitlines()[:1]}")
    results.append(check("omitted --region still defaults", not region_problems,
                         f"rc={rd.returncode} " + "; ".join(region_problems)))
    return all(results)


# Layer name -> the tile keys it (and only it) contributes, from
# App.Dump.tileToJSON. A layer left out of the selection must contribute
# NONE of its keys.
DUMP_LAYER_KEYS = {
    "terrain": ["terrainZ", "surfaceZ", "waterTableZ", "waterTableSummer",
                "waterTableWinter"],
    "material": ["matId"],
    "fluid": ["fluidType", "fluidSurf"],
    "ice": ["iceSurf", "iceMode"],
    "ore": ["oreId", "oreTopZ", "oreCount"],
    "slope": ["slope", "hardness"],
}


def check_dump_layer_selection() -> bool:
    """#1086: --dump=<selection> still selects exactly those layers.

    Deliberately a REAL dump rather than a parse check: the prefix strip
    changed, and only the emitted tile records prove the selection that
    survived it is the one the user typed. The suffix is mixed-case on
    purpose — matching stays case-insensitive, and 'elevation' stays an
    alias for 'terrain'.
    """
    print("10d. an explicit --dump= selection emits exactly those layers")
    selected = ["terrain", "ice"]
    try:
        r = run_cli("--dump=Elevation,ICE", "--worldSize", "16",
                    "--region", "0,0,0,0", timeout=180.0)
    except subprocess.TimeoutExpired:
        return check("dump layer selection", False,
                     "dump did not finish in 180s")
    problems = []
    if r.returncode != 0:
        problems.append(f"exit status {r.returncode} (want 0)")
    try:
        tiles = json.loads(r.stdout)
    except (json.JSONDecodeError, ValueError) as e:
        return check("dump layer selection", False,
                     f"stdout is not valid JSON: {e}")
    if not tiles:
        return check("dump layer selection", False, "dump produced no tiles")
    keys = set(tiles[0].keys())
    for layer, layer_keys in DUMP_LAYER_KEYS.items():
        missing = [k for k in layer_keys if k not in keys]
        present = [k for k in layer_keys if k in keys]
        if layer in selected and missing:
            problems.append(f"selected layer {layer} is missing {missing}")
        if layer not in selected and present:
            problems.append(f"unselected layer {layer} emitted {present}")
    return check("dump layer selection", not problems,
                 f"rc={r.returncode} " + ("; ".join(problems) if problems
                                          else f"{len(tiles)} tiles"))


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    # Every registered probe accepts --port (#723) so tools/run_probes.py
    # can force a non-default port on any of them uniformly, even one
    # like this that boots no engine at all and has no use for it.
    ap.add_argument("--port", type=int, default=None)
    ap.parse_args()

    results = [check_missing_target()]

    print("2. unexposed categories (no compatibility aliases): exit 1, canonical list")
    results += [check_unexposed_category(c) for c in UNEXPOSED_CATEGORIES]

    print("3. grouped categories, no item: exit 0, guidance printed")
    results += [check_grouped_no_item(c) for c in GROUPED_CATEGORIES]

    results.append(check_nonexistent_simple_item())
    results.append(check_path_containment())
    results.append(check_directory_as_item())
    results.append(check_unit_targets())
    results.append(check_grouped_item_targets())
    results.append(check_incompatible_flags())
    results.append(check_boot_mode_precedence())
    results.append(check_malformed_values())
    results.append(check_mode_priority_over_value())
    results.append(check_omission_still_defaults())
    results.append(check_dump_layer_selection())

    passed = all(results)
    print(f"\n  {'PASS' if passed else 'FAIL'}: no-boot CLI contract"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
