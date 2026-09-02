#!/usr/bin/env python3
"""
pack_atlas.py — unit-animation asset inventory validator AND the
deterministic per-animation atlas compiler.

Two modes, one tool:

    --validate-only   the authoritative asset inventory gate (#1257),
                      extended by #1258 to also verify any generated
                      atlas index it finds against its own sources.
    --compile         compile the declared frames into one lossless PNG
                      atlas per ANIMATION plus a generated per-unit
                      index (#1258).

Per `docs/texture_infrastructure.md` (TEX-2), KTX2 encoding stays
deferred to TEX-5 and runtime sampling to TEX-3.

WHAT IT VALIDATES (issue #1257)
-------------------------------

Discovery is FILESYSTEM-FIRST. The physical inventory is every PNG
beneath

    assets/textures/units/<unit>/animations/<animation>/<direction>/

and the declarations under `data/units/*.yaml` are checked AGAINST it,
not used to decide what to look at. That ordering is the whole point:
the previous version harvested unit names from the YAML files and so
never examined the three shipped asset trees that had no YAML at all.

Every committed animation PNG must be owned by exactly one
animation-frame declaration. There is no directory-level or glob-level
exemption mechanism — a file is either declared or it is a failure.

Two declaration forms live under `data/units/`:

  units:        a gameplay unit. Registered by
                `Engine.Asset.YamlUnits.loadUnitYaml`, loads textures,
                spawnable. `name` and `sprite` are mandatory.

  asset_units:  an ASSET-ONLY unit (#1257). Declares animation frames
                for this validator and for the `--preview units/<name>`
                browser, and nothing else — never returned by
                `loadUnitYaml`, so never registered, never loaded into
                the gameplay unit registry, not listable or spawnable.
                `name` and `animations` are mandatory; gameplay fields
                are refused outright rather than ignored.
                The form remains supported for shipped art that must be
                atlas-validated and previewable without registering as a
                gameplay unit. Runtime registration follows the top-level
                key rather than filesystem presence.

A file may hold either key or both. A file holding neither is an error
(that is what a mistyped top-level key looks like), and so is a key
present with an explicit null — the engine's own decoder reads that as
absent and refuses the file, so accepting it here would leave the gate
green while startup failed.

INVARIANTS ENFORCED
-------------------

  * a unit identifier is one lowercase `[a-z0-9_]+` path component; an
    animation identifier is the same, plus the one narrowly matched
    approved exception `<lowercase>_RH_<lowercase>` for the documented
    asymmetric-weapon animations — see ANIM_IDENT_RE;
  * direction names come from the engine's own direction vocabulary and
    sit at the direction level of the tree;
  * frame files are named `frame_NNN.png`, with exactly three digits;
  * a declared path is relative, free of `..`, free of symlinks, and
    resolves inside the exact expected
    `<unit>/animations/<animation>/<direction>/` directory — cross-unit,
    cross-animation and cross-direction references are all named as
    such;
  * every declared frame exists AS A REGULAR FILE, DECODES as a real
    PNG, and every physical frame is declared;
  * every frame of ONE animation decodes to the same pixel size —
    different DIRECTIONS of that animation may still hold different
    frame COUNTS, which is a separate axis;
  * one physical frame is claimed by at most one animation-frame slot
    (reuse as a unit's `sprite`, `directional_sprites` entry or
    `portrait` is deliberately legal and is NOT a duplicate claim);
  * `flip: true` declares exactly the canonical five authored directions
    (south, south-east, east, north-east, north) and `flip: false`
    declares exactly all eight;
  * per direction, frame indices start at 0, ascend in the order they
    are declared (playback follows that order), and have no gaps or
    duplicates — different directions of one animation may still hold
    different counts;
  * an asset-only entry declares exactly `name` and `animations` — a
    whitelist, so an unknown key fails as surely as a gameplay one;
  * animation and direction keys are strings, never coerced (YAML
    resolves an unquoted `123:` to an int that would stringify into a
    valid-looking identifier);
  * `fps` is a positive number that survives the engine's 32-bit
    `Float` — `.nan` and `.inf` are real floats to PyYAML and neither
    fails a positivity test, an unbounded-precision int makes
    `math.isfinite` raise rather than answer, and `1.0e+100` /
    `1.0e-100` fit a double but load as infinity / zero — and `loop` is
    a boolean, rejected rather than coerced when it is not;
  * no symlink appears anywhere in the walk (unit directory,
    `animations/` root, animation directory, direction directory, or
    frame), so nothing can be linked past the inventory.

WHAT IT COMPILES (issue #1258)
------------------------------

`--compile` turns the validated declarations into DERIVED artifacts.
Sources are never touched: individual PNG frames remain the editable
artwork (D-1) and unit YAML remains the only hand-edited semantic
authority (D-11).

    assets/textures/units/<unit>/atlas/<animation>.png   one atlas per
                                                         ANIMATION (D-2)
    assets/textures/units/<unit>/atlas/index.json        the generated
                                                         per-unit index

Layout. One ROW per AUTHORED direction, in `ATLAS_DIRECTION_ORDER` —
the engine's own `Unit.Direction` order `S, SW, W, NW, N, NE, E, SE`,
restricted to the directions this animation actually authors, so a
`flip: true` animation has five rows and a `flip: false` one has eight.
Each direction's row index is nevertheless recorded EXPLICITLY, so the
runtime reads a row rather than re-deriving the order.

Columns are the animation's maximum authored frame count. A shorter row
is rectangularized with transparent RGBA8 zero slots and NOTHING ELSE
(D-5): they exist so the sheet is a rectangle, and the index's
per-direction `frame_count` is the only frame authority — no such slot
is addressable as a frame.

Cell geometry is exact INTEGER pixels, at a stride widened by the
one-texel extrusion gutter (#2076). Each cell occupies a physical SLOT
of `(cell_width + 2 * CELL_PADDING)` x `(cell_height + 2 *
CELL_PADDING)`, and frame `c` of the direction whose row is `r` has its
LOGICAL cell at `x = c * slot_width + CELL_PADDING`,
`y = r * slot_height + CELL_PADDING`, `cell_width` x `cell_height`.
Every frame of one animation must decode to those same cell
dimensions; a mismatch is a compile error, never an implicit rescale or
crop (D-6 — nothing here resamples or blends).

The gutter around each slot is filled by copying that cell's own
outermost texels outward, corners included, so a bilinear tap taken
anywhere inside a logical cell reads only that cell's colours instead
of bleeding into the neighbouring frame. Nearest sampling is unchanged
by construction: the index addresses the INNER cell, so no fragment
centre moves and the picture stays pixel-identical.

Every atlas cell is a byte-for-byte copy of its source frame's
canonical decoded RGBA8 samples, alpha included, and the gutter around
it is a byte-for-byte copy of that frame's own edge texels.
"Byte-for-byte" is about those decoded samples, not about PNG-encoded
file bytes: the engine's own upload path decodes to RGBA8 as well
(`Engine.Scripting.Lua.Message.Texture`'s convertRGBA8).

The index. One JSON document per unit, generated end to end — see
`pack_atlas_compiler.build_index_document` for the exact schema. It carries a
`schema_version` (the FORMAT contract TEX-3 will parse, bumped when the
document shape changes) separately from `tool_version` (this compiler's
own revision), a documented `direction_order`, and per animation: the
storage format and atlas path, atlas/cell dimensions, the extrusion
gutter (`cell_padding`), columns, rows, each authored direction's row
and REAL frame count, the mirroring declaration, and two digests.

Digests are `sha256` and are named as such in the document:

  * `source_digest` is PER ANIMATION, over a canonically ordered,
    length-prefixed stream of that animation's own inputs — name, flip,
    fps, loop, cell geometry including `cell_padding`, and for every
    direction in atlas order its declared frame paths and their
    canonical decoded RGBA8 pixels. Per-animation
    is the point: one animation's edit must not invalidate an unrelated
    atlas (D-12).
  * `atlas_digest` is over the atlas's decoded RGBA8 CONTENT
    (dimensions + samples), not its file bytes, so it stays meaningful
    across PNG encoders while still pinning every pixel.

Determinism and locality. A clean rebuild from identical sources under
an unchanged toolchain produces identical artifacts. An incremental run
compares each artifact against what it would generate and WRITES ONLY
ON A REAL DIFFERENCE, so editing one animation rewrites that
animation's atlas and its unit index and nothing else — unrelated
atlases are not even opened for writing. Note that an mtime-only touch
of a frame changes nothing: the digest is over content.

Obsolete compiler-owned output — an atlas for an animation that was
deleted or renamed — is removed from the unit's own `atlas/` directory
during a compile of that unit. Nothing outside that directory is ever
removed, so source artwork and other units' artifacts are structurally
out of reach.

STALENESS
---------

`--validate-only` is index-aware. A unit with NO index is valid HERE:
this tool validates declarations against art, and an uncompiled tree is
a legitimate intermediate state of a working copy. The ENGINE is
stricter — since #1261 it refuses to register a unit that declares
animations and ships no compiled artifacts. Every shipped declaration,
gameplay or asset-only, is compiled and tracked. Where an index DOES
exist it is regenerated from the sources and compared, so a stale
source digest, a hand-edited or non-canonically serialized index, a
missing indexed atlas, and an atlas whose pixels do not match its
sources are all reported — and a tampered index cannot certify a
tampered atlas, because the comparison is against a fresh regeneration
rather than against the numbers the file itself carries.

USAGE
-----

    python3 tools/pack_atlas.py --validate-only
        Validate the whole unit corpus. Exit 0 on success, non-zero
        with a report on any issue.

    python3 tools/pack_atlas.py --validate-only --unit acolyte
        Restrict both the declarations and the filesystem walk to one
        unit. A name with neither a declaration nor an asset tree is an
        error, not an empty success.

    python3 tools/pack_atlas.py --validate-only --strict
        Also treat warnings as errors. Every inventory violation above
        is an ERROR regardless of this flag, and so is every
        frame-content finding and every image/slot budget breach;
        `--strict` runs no extra checks, it only promotes the warnings.
        Two things warn: non-PNG debris in the animation tree, and the
        resident-memory budget below. The latter is why CI and
        `make ci` pass `--strict` — a breach is a project decision to
        make, and this is what stops it passing unnoticed.

    python3 tools/pack_atlas.py --compile [--unit acolyte]
        Compile atlases and indices. Refuses to run at all if the
        inventory does not validate.

    python3 tools/pack_atlas.py --compile --check
        Report what a compile WOULD change and change nothing. Exits
        non-zero if anything is out of date — the shape a CI freshness
        gate wants.

    python3 tools/pack_atlas.py {--validate-only|--compile} --root <dir>
        Operate on an alternative tree holding `data/units/` and
        `assets/textures/units/`. Used by tools/test_pack_atlas.py so
        its fixtures never touch the shipped assets.

Exactly one of `--validate-only` / `--compile` is required: writing
derived artifacts is never something this tool does by default.

FRAME CONTENTS (issue #1311)
----------------------------

Every DECLARED frame is opened and decoded, not merely stat-ed. #1257
stopped at the file boundary, so a truncated, corrupt or mislabelled
frame passed the gate and failed later — at texture-upload time, or
visibly in game. `validate_frame_image` closes that gap in three
checks, because each one covers ground the others cannot:

  * a full `decode_rgba8` runs the filters, interlace passes and colour
    conversion — catching a truncated stream, corrupt compressed data,
    a non-image, and (through its own format check) a valid image of
    some other format renamed to `.png` — but never looks at an IDAT
    chunk's checksum, because Pillow reads and discards those four
    bytes while streaming pixel data;
  * Pillow's own `verify()` then CRCs the chunks, which is what catches
    an intact payload under a wrong checksum. It never decompresses, so
    it could not have replaced the pass above;
  * and `locate_png_stream_end` covers the terminal chunk, which
    `verify()` breaks ON without checksumming and the decoder never
    reads at all, plus anything appended after the image ends. It walks
    chunk FRAMING only — length, type, payload, CRC — decoding nothing
    and knowing no chunk type but IEND, and it runs only after Pillow
    has CRC-validated that sequence, so it cannot disagree with the
    real decoder about where a chunk lies. Its answer feeds two
    constant comparisons: that IEND's own 12 bytes are the fixed
    constant its empty payload makes them, and that the file ends
    there. Checking the FILE's last bytes would not do: appending a
    second canonical IEND leaves a perfect tail while the real image
    ended 12 bytes earlier.

Together they reject a truncated file, corrupt compressed data, a bad
chunk checksum anywhere including the terminal one, a structurally
invalid stream, a non-image wearing a `.png` name, and a valid image of
some OTHER format renamed to `.png` (the engine's loader is a PNG
loader). Every legitimate PNG colour type
— paletted, greyscale, greyscale+alpha, 16-bit, interlaced — is
accepted: the rule is "decodes as a PNG", never "is already RGBA8".

The pixel size each frame decodes to is then compared ACROSS one
animation, which is the constraint the atlas cell geometry rests on
(D-6 forbids resampling, so the compiler has no way to reconcile a
mismatch). This says nothing about frame COUNTS: different directions
of one animation legitimately hold different numbers of frames.

Content findings are ERRORS, in plain `--validate-only` as much as
under `--strict`.

WHAT IT STILL DOES NOT VALIDATE
-------------------------------

Non-animation unit textures. `portrait.png`, `directional_sprites`
entries, `sprite`, and `unknown_unit/rotations/*.png` are checked for
EXISTENCE only: the inventory's scope is `animations/`, and those files
are referenced from hard-coded Haskell or non-animation YAML fields.

REQUIREMENTS
------------

PyYAML for the declarations, Pillow for image decode/encode. Both are
now load-bearing for `--validate-only`: since #1311 the inventory gate
decodes every declared frame, so an absent Pillow is a loud ERROR
naming the install command, never a silent skip of the content checks
— a gate that skipped them would print OK while validating nothing.

Pillow is still imported LAZILY, but that now only spares a run with no
declared frames to decode (`--help`, an empty root, an argument error).

`tools/requirements-assets.txt` pins both, is what the CI image
installs, and is therefore the reference toolchain for byte-identical
output. Install it with:

    python3 -m pip install --user -r tools/requirements-assets.txt

Validation does NOT require that exact toolchain: every recorded digest
is over canonical decoded RGBA8, so a different Pillow build verifies a
committed atlas just as well as the one that wrote it.

"""
# MODULE LAYOUT (issue #2054)
# --------------------------------------------------------------------
# This file is the public executable façade: the documentation above
# (which `--help` prints verbatim), CLI parsing, mode dispatch, the
# composition of the validation gate, and the printed report. Every
# implementation body lives with its owner, one `tools/pack_atlas_<owner>.py`
# each:
#
#   pack_atlas_shared        identifier and filename rules, the direction
#                            vocabulary and row order, atlas/index format
#                            constants, digest domain tags, the records
#                            that cross an owner boundary, the scalar
#                            rules, and the path helpers more than one
#                            owner reads
#   pack_atlas_image         Pillow access, canonical RGBA8 decoding, PNG
#                            container validation, the frame-content
#                            check, and the runtime-`Float` helpers
#   pack_atlas_declarations  data/units/*.yaml parsing
#   pack_atlas_inventory     path ownership, the filesystem-first walk,
#                            and the inventory gate (`validate_sources`)
#   pack_atlas_compiler      planning, extrusion, digests, the generated
#                            index, PNG encoding, the compiler-owned
#                            output directory, and `compile_unit`
#   pack_atlas_index         stored-index freshness against a fresh
#                            regeneration
#   pack_atlas_budget        the unit-texture budget policy and tally
#
# Dependencies run one way: shared is a leaf; image consumes shared;
# declarations consume shared and image; inventory consumes
# declarations, image and shared; the compiler consumes image and
# shared; index validation consumes the compiler's planning/digest
# interfaces, image and shared; the budget consumes shared alone; this
# façade composes them. None of the owners parses a command line.


from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import List, Optional, Tuple

from pack_atlas_budget import mib, validate_budget
from pack_atlas_compiler import compile_unit
from pack_atlas_index import validate_indices
from pack_atlas_inventory import validate_sources
from pack_atlas_shared import UNIT_IDENT_RE, BudgetTally, Issue, Report, Totals


REPO_ROOT = Path(__file__).resolve().parent.parent


# --------------------------------------------------------------------
# Driver
# --------------------------------------------------------------------


def validate(
    root: Path, only_unit: Optional[str], report: Report,
) -> Tuple[Totals, Optional[BudgetTally]]:
    """The full `--validate-only` gate: inventory, freshness, budget."""
    totals, decls = validate_sources(root, only_unit, report)
    # Index checks run even when the inventory already failed: a stale
    # artifact and a broken declaration are independent findings, and
    # reporting only the first would hide the other behind a fix. The
    # budget follows for the same reason: it reads the STORED index, so
    # it stays meaningful — and stays reported — while a freshness
    # failure is being fixed.
    validate_indices(report, root, decls, only_unit)
    budget = validate_budget(report, root, decls, only_unit)
    return totals, budget


def print_report(report: Report) -> None:
    def fmt(issue: Issue) -> str:
        return f"  [{issue.where}] {issue.msg}"

    if report.errors:
        print(f"ERRORS ({len(report.errors)}):")
        for e in report.errors:
            print(fmt(e))
    if report.warnings:
        print(f"WARNINGS ({len(report.warnings)}):")
        for w in report.warnings:
            print(fmt(w))


def cmd_validate(
    root: Path, target_unit: Optional[str], strict: bool,
) -> int:
    report = Report()
    totals, budget = validate(root, target_unit, report)
    print_report(report)

    if not report.errors and not report.warnings:
        print(
            f"OK — {totals.units} unit declaration(s) "
            f"({totals.asset_only} asset-only), {totals.animations} "
            f"animation(s), {totals.frames} frame(s); every animation PNG "
            f"on disk is owned exactly once.")
        # Printed on success, not only on breach: the budget's whole
        # value is that the numbers are visible while they are still
        # healthy, so a regression shows up as a moved number in a diff
        # rather than only as a threshold crossing years later.
        if budget is not None and budget.units:
            print(
                f"BUDGET — {budget.images} resident animation image(s) for "
                f"{budget.animations} animation(s) across {budget.units} "
                f"compiled unit(s) ({budget.frames} logical frames); "
                f"{mib(budget.resident_bytes)} decoded RGBA8 resident.")

    return 1 if report.has_failures(strict) else 0


def cmd_compile(
    root: Path, target_unit: Optional[str], strict: bool, dry_run: bool,
) -> int:
    """Compile atlases and indices — or, with `dry_run`, report the work.

    Compilation never runs against an inventory that does not validate:
    the declarations ARE the compiler's contract, so producing derived
    artifacts from a corpus that fails it would launder a broken
    declaration into a tracked artifact.
    """
    report = Report()
    _, decls = validate_sources(root, target_unit, report)
    if report.has_failures(strict):
        print_report(report)
        print("refusing to compile: the source inventory does not validate.")
        return 1

    written: List[str] = []
    removed: List[str] = []
    unchanged = 0
    for decl in sorted(decls, key=lambda d: d.name):
        outcome = compile_unit(report, root, decl, dry_run)
        if outcome is None:
            continue
        written.extend(outcome.written)
        removed.extend(outcome.removed)
        unchanged += outcome.unchanged

    print_report(report)
    verb = "would write" if dry_run else "wrote"
    scrub = "would remove" if dry_run else "removed"
    for path in written:
        print(f"  {verb}: {path}")
    for path in removed:
        print(f"  {scrub}: {path}")

    tally = (f"{verb} {len(written)} artifact(s), {scrub} {len(removed)}, "
             f"{unchanged} already current.")
    if report.has_failures(strict):
        return 1
    if dry_run and (written or removed):
        print(f"OUT OF DATE — {tally}")
        return 1
    print(f"OK — {tally}")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument(
        "--validate-only",
        action="store_true",
        help="Validate the inventory and any generated atlas index; write "
             "nothing.",
    )
    ap.add_argument(
        "--compile",
        action="store_true",
        help="Compile one lossless PNG atlas per animation plus the "
             "generated per-unit index.",
    )
    ap.add_argument(
        "--check",
        action="store_true",
        help="With --compile: report what would change and change nothing, "
             "exiting non-zero if any artifact is out of date.",
    )
    ap.add_argument(
        "--unit",
        help="Restrict the run to a single unit by name (e.g. 'acolyte').",
    )
    ap.add_argument(
        "--strict",
        action="store_true",
        help="Also treat warnings as errors. Inventory violations are errors "
             "either way.",
    )
    ap.add_argument(
        "--root",
        default=str(REPO_ROOT),
        help="Tree holding data/units/ and assets/textures/units/ "
             "(default: the repository root).",
    )
    args = ap.parse_args()

    # Writing derived artifacts is never a default. Requiring the mode
    # explicitly also keeps every existing `--validate-only` call site
    # meaning exactly what it did before the compiler existed.
    if args.validate_only == args.compile:
        sys.stderr.write(
            "error: pass exactly one of --validate-only or --compile\n")
        return 2
    if args.check and not args.compile:
        sys.stderr.write("error: --check applies to --compile\n")
        return 2

    root = Path(args.root).resolve()
    if not root.is_dir():
        sys.stderr.write(f"error: --root is not a directory: {root}\n")
        return 2
    if args.unit is not None and not UNIT_IDENT_RE.match(args.unit):
        sys.stderr.write(f"error: not a unit name: {args.unit}\n")
        return 2

    if args.compile:
        return cmd_compile(root, args.unit, args.strict, args.check)
    return cmd_validate(root, args.unit, args.strict)


if __name__ == "__main__":
    sys.exit(main())
