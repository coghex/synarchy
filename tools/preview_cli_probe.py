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
 10. Present-but-malformed values (#1191): every affected spelling
     (--seed/--worldSize/--plates/--ages/--port), an empty and an
     unknown --dump= layer selection plus empty segments, and a
     malformed and a non-positive --size all exit 1 pre-boot naming the
     flag and the offending token — never the silent fall-through to a
     default that made `--seed not-a-number` produce a full, valid,
     WRONG dump at seed 42. Also pins the two orderings the fix has to
     preserve: validation runs ahead of mode-specific early exits and
     regardless of whether the value would be consumed (a malformed
     --port fails even for a bare grouped --preview; a malformed --ages
     fails even when a valid --plates wins), while check 9's
     mode-compatibility rejection still takes priority over it (a
     malformed --seed given to --headless is reported as unsupported in
     headless mode, not as malformed). Omitting a flag entirely still
     keeps its documented default. The pure four-outcome parser
     coverage is hspec `--match "App.Cli"`.

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

LOG = "/tmp/preview_cli_probe_engine.log"

UNEXPOSED_CATEGORIES = ["equipment", "hud", "facemap", "utility", "vegetation"]
GROUPED_CATEGORIES = ["units", "flora", "buildings", "structures"]
CANONICAL_LIST_TEXT = "icons, items, ui, world, units, flora, buildings, structures"


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f"  ({detail})" if detail else ""))
    return ok


def run_cli(*extra_args: str, timeout: float = 30.0) -> subprocess.CompletedProcess:
    cmd = ["cabal", "run", "-v0", "exe:synarchy", "--", *extra_args]
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
]

# Mode compatibility (check 9) must keep its priority over value
# validation: these name the MODE, and must NOT report the value as
# malformed.
MALFORMED_IN_WRONG_MODE_CASES = [
    (["--headless", "--seed", "not-a-number"], "--seed", "headless"),
    (["--dump", "--port", "not-a-port"], "--port", "dump"),
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
        if r.returncode == 0:
            problems.append("exit 0")
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
    return check("omission defaults", not problems,
                 f"rc={r.returncode} " + "; ".join(problems))


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
    results.append(check_malformed_values())
    results.append(check_mode_priority_over_value())
    results.append(check_omission_still_defaults())

    passed = all(results)
    print(f"\n  {'PASS' if passed else 'FAIL'}: no-boot CLI contract"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
