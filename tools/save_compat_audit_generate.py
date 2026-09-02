#!/usr/bin/env python3
"""Live current-format fixture generation (issue #2049, requirement 12).

The ONE owner of `--generate-session`: generation-argument parsing, the
throwaway resource root, the skipped-headless-loader bootstrap, safe
`{bid}`/`{uid}` substitution, the real headless-engine boot and save,
the optional building/unit spawns, the setup statements, settle window
and required predicate, and the three-file transaction that stages
fixture, summary and manifest together.

It consumes the codec bridge (save_compat_audit_codec's
`normalize_fixture_timestamp` and `dump_canonical_summary`) and
DELEGATES registration to save_compat_audit_register's
`cmd_add_baseline` (requirement 14) -- there is no second
checksum calculation, manifest writer, validation command, rollback
helper or fixture-entry construction here (requirement 16).

Requirement 13's transaction is two composed halves and the order
matters: `cmd_generate` captures the fixture and summary bytes BEFORE
any write and calls `restore_files()` on a generation error, a failed
summary dump, or a non-zero registration, while
`_finalize_manifest_write` independently restores the manifest text.
Keep them composed in that order.

The public façade is tools/save_compat_audit.py.
"""
from __future__ import annotations

import argparse
import os
import shutil
import sys
import tempfile
import time
from pathlib import Path

import save_compat_audit_codec as codec
import save_compat_audit_common as common
import save_compat_audit_register as register

class GenerationError(Exception):
    """A real-engine fixture-generation step failed (requirement 21)."""


#: How long --require-lua keeps retrying after --settle-seconds has
#: elapsed. Generous: a predicate that depends on a Lua tick can miss its
#: first window on a loaded machine, and the failure mode this exists to
#: prevent (a silently state-free fixture) is far worse than a slow run.
PREDICATE_RETRY_SECONDS = 30.0


def _parse_tile(text: str) -> tuple[int, int]:
    """Parse a "GX,GY" CLI tile argument."""
    try:
        gx_s, gy_s = text.split(",", 1)
        return int(gx_s.strip()), int(gy_s.strip())
    except ValueError:
        raise SystemExit(f"--spawn-unit-at expects 'GX,GY', got {text!r}")


def _make_isolated_gen_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory -- mirrors tools/save_compat_migration_probe.py's
    make_isolated_root/tools/save_storage_probe.py's own helper, so a
    generated fixture never touches a real player's saves."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(common.REPO_ROOT, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def _bootstrap_gen_defs(send, port: int) -> None:
    """Load the defs a headless boot skips (no loading screen) but
    engine.saveWorld's own content still needs to resolve real
    building/unit/recipe names -- mirrors tools/multiworld_save_probe.py/
    tools/save_compat_migration_probe.py's identical helper. Only needed
    when actually spawning something (an entity-free session never
    references any def at all)."""
    import glob
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
        ("data/recipes/*.yaml",    "engine.loadRecipeYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    # Locations too (#915): they are placed AT world.init from the
    # registry, so a def registered later can never appear in the
    # generated world -- and a fixture that must capture per-unit
    # location knowledge needs a real placed location to know about.
    for path in sorted(glob.glob("data/locations/*.yaml")):
        send(port, f"engine.loadLocationYaml('{path}'); return 'ok'")


def render_setup_lua(stmt: str, bid, uid) -> str:
    """Substitute ONLY the two documented placeholders into a
    --setup-lua statement.

    Deliberately NOT str.format: Lua statements routinely contain
    ordinary braces (`{1, 2}`, `for k, v in pairs(t) do end`, a table
    constructor of any kind), and str.format reads every one of those as
    a format field -- raising KeyError/ValueError on a perfectly valid
    statement, or worse, silently consuming it. A plain two-token
    replace has no such surface. An unspawned side substitutes the Lua
    literal `nil`, so a statement referencing it fails loudly in Lua
    rather than interpolating the Python string "None"."""
    return (stmt
            .replace("{bid}", "nil" if bid is None else str(bid))
            .replace("{uid}", "nil" if uid is None else str(uid)))


def generate_current_format_session(
        port: int, page_id: str, seed: int, world_size: int, plate_count: int,
        spawn_building: str, spawn_unit: str, out_path: Path,
        spawn_unit_at: tuple[int, int] = (0, 0),
        settle_seconds: float = 0.0,
        setup_lua: list[str] | None = None,
        require_lua: str | None = None,
        world_name: str | None = None,
        world_gloss: str | None = None,
        language_seed: str | None = None,
        language_version: int | None = None,
        name_expr: str | None = None) -> None:
    """Boot a REAL headless engine (isolated resource root -- see
    _make_isolated_gen_root), init a world, optionally spawn ONE building
    and/or ONE unit through the SAME engine.saveWorld/building.spawn/
    unit.spawn verbs every other probe in this repo already uses, then
    save it -- producing genuine CURRENT-format envelope bytes through
    the real World.Save.Storage/Envelope.Codec production path (the
    exact same one an ordinary player save takes), not a hand-built or
    spliced value. Raises GenerationError on any rejected step.

    @setup_lua@ statements run after the spawns and before the settle,
    each sent as one debug-console line with `{bid}`/`{uid}` substituted
    for the ids the spawns above actually returned. Some state is
    written by neither a spawn verb nor a tick, but only by a real
    player/AI ACTION -- #1087's container knowledge is revealed by a
    completed storage interaction, never by proximity -- and a fixture
    that cannot stage that action can only ever capture the feature's
    empty default. A statement whose reply starts with a Lua error, or
    which is literally `false`/`nil`, fails generation: silently
    proceeding would produce exactly the hollow fixture this exists to
    prevent.

    @world_name@/@world_gloss@/@language_seed@ give the page a #1092
    language provenance, which is what makes its placed locations carry
    real generated names and glosses (#1101) instead of definition
    labels. Without them the fixture can only ever capture the
    no-language fallback -- an empty gloss on every location -- which
    would leave the new field untested by the very fixture registered to
    cover its wire version.

    @name_expr@ (#1104) is the encoded semantic expression the world
    name was rendered from -- world.suggestName's own `expr` reply. It is
    what puts a #1104 etymology source on the page's own identity; the
    page's locations and rivers acquire theirs from the language itself,
    so they need nothing here. Same reasoning as the provenance above: a
    fixture generated without it can only capture the absent case.

    This can only ever produce a fixture at the CURRENT wire format -- a
    live engine never writes a historical shape (see this module's own
    docstring for why a historical baseline stays a manual operation)."""
    from probelib import boot, send, quit_engine
    tmpdir = tempfile.mkdtemp(prefix="save_compat_gen_")
    slot = "generated"
    proc = None
    try:
        root = _make_isolated_gen_root(tmpdir)
        proc = boot(port, log=f"/tmp/save_compat_gen_{page_id}.log",
                    args=["--resource-root", root], ready_timeout=180)
        if spawn_building or spawn_unit:
            _bootstrap_gen_defs(send, port)
        init_args = f"'{page_id}', {seed}, {world_size}, {plate_count}"
        if world_name is not None:
            init_args += f", '{world_name}'"
            init_args += (f", '{world_gloss}'" if world_gloss is not None
                          else ", nil")
            if language_seed is not None:
                init_args += f", '{language_seed}'"
                # A name expression can only ride on the generated-name
                # path, and world.init reads it as argument 9 -- so the
                # version argument must be present (even as its default)
                # before it can be supplied positionally.
                if language_version is not None or name_expr is not None:
                    init_args += (f", {language_version}"
                                  if language_version is not None else ", nil")
                if name_expr is not None:
                    init_args += f", '{name_expr}'"
        inited = send(port, f"world.init({init_args}); return 'ok'")
        if "ok" not in inited:
            raise GenerationError(f"world.init failed: {inited!r}")
        time.sleep(1.0)  # let generation settle before saving/spawning

        # world.show (not just world.init) puts the page in wmVisible --
        # mirrors tools/multiworld_save_probe.py's identical note: without
        # it, building.spawn/canPlaceAt's visible-page terrain read
        # can reject a spawn, and the saved snapshot's own visiblePages/
        # live-camera-owner-page would come out empty/null instead of
        # matching an ordinary player session's shape.
        send(port, f"world.show('{page_id}'); return 'ok'")
        active_deadline = time.time() + 10.0
        while time.time() < active_deadline:
            if send(port, "return world.getActiveWorldId()").strip('"') == page_id:
                break
            time.sleep(0.2)
        else:
            raise GenerationError(f"'{page_id}' never became the active world")

        def as_int(s: str):
            try:
                return int(float(s))
            except (TypeError, ValueError):
                return None

        # unit.spawn/building.spawn return the new entity's id (a
        # non-negative integer, as a string) on success, not a boolean --
        # mirrors tools/multiworld_save_probe.py's as_int/bid<0 convention.
        bid = uid = None
        if spawn_building:
            r = send(port, f"return building.spawn('{spawn_building}', 0, 0)")
            bid = as_int(r)
            if bid is None or bid < 0:
                raise GenerationError(
                    f"building.spawn('{spawn_building}') rejected: {r!r}")
        if spawn_unit:
            ux, uy = spawn_unit_at
            r = send(port, f"return unit.spawn('{spawn_unit}', {ux}, {uy}, 0, "
                            f"'player')")
            uid = as_int(r)
            if uid is None or uid < 0:
                raise GenerationError(
                    f"unit.spawn('{spawn_unit}') at ({ux},{uy}) rejected: {r!r}")

        for stmt in (setup_lua or []):
            rendered = render_setup_lua(stmt, bid, uid)
            reply = send(port, rendered).strip()
            if (reply.startswith("error") or reply.startswith("Error")
                    or reply in ("false", "nil", '"false"', '"nil"')):
                raise GenerationError(
                    f"--setup-lua statement {rendered!r} did not succeed: "
                    f"{reply!r}")

        # Some state is not written by a spawn verb at all -- it is
        # ACQUIRED by a tick once the entity is in the right place (#915's
        # per-unit location memory is ingested by the unit-AI update from
        # world.getLocationAwareness). Let those ticks run, and refuse to
        # save until the caller's own predicate says the state is actually
        # there: a fixture that silently comes out WITHOUT the shape it
        # exists to track is worse than no fixture, because every audit
        # downstream then passes on it.
        if settle_seconds > 0:
            time.sleep(settle_seconds)
        if require_lua:
            deadline = time.time() + max(settle_seconds, PREDICATE_RETRY_SECONDS)
            while True:
                r = send(port, f"return ({require_lua}) and 'y' or 'n'")
                if r.strip().strip('"') == "y":
                    break
                if time.time() >= deadline:
                    raise GenerationError(
                        f"--require-lua never became true: {require_lua!r}")
                time.sleep(0.5)

        saved = send(port, f"return engine.saveWorld('{page_id}', '{slot}')")
        if saved.strip() != "true":
            raise GenerationError(f"engine.saveWorld failed: {saved!r}")
        saved_path = os.path.join(root, "saves", slot, "world.synworld")
        for _ in range(100):
            if os.path.isfile(saved_path):
                break
            time.sleep(0.1)
        if not os.path.isfile(saved_path):
            raise GenerationError(f"saved file never appeared at {saved_path}")
        out_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(saved_path, out_path)
    finally:
        if proc is not None:
            quit_engine(port, proc)
        shutil.rmtree(tmpdir, ignore_errors=True)

    # Round-11 review: normalize the freshly-generated fixture's
    # smTimestamp to a fixed constant AFTER the engine has already
    # exited -- engine.saveWorld's own wall-clock timestamp would
    # otherwise make two runs over identical inputs produce different
    # bytes/checksums, defeating requirement 21's reproducibility intent.
    ok, tail = codec.normalize_fixture_timestamp(out_path)
    if not ok:
        raise GenerationError(
            f"timestamp normalization failed (fixture at {out_path} is "
            f"the raw, un-normalized engine.saveWorld output): {tail}")


def cmd_generate(args: argparse.Namespace) -> int:
    """--generate-session: produce a brand-new CURRENT-format complete-
    session fixture through the real engine + real codec end to end,
    then delegate straight to register.cmd_add_baseline for the SAME atomic
    registration + real-codec validation --add-baseline already does
    (this only ever produces a "complete-session" fixture, so args.kind
    is fixed here rather than asked for).

    Round-6 review: stages fixture + summary + manifest together and
    rolls ALL of them back on ANY downstream failure (dump derivation or
    manifest real-codec validation) -- not just the manifest. Without
    this, a validation failure left new fixture/summary bytes sitting on
    disk unregistered, or (with --force) clobbered a PREVIOUSLY-tracked
    fixture's bytes with new-but-invalid content while the manifest
    (correctly rolled back on its own) still pointed at the OLD
    checksum -- either way, a state the NEXT audit run would immediately
    flag as drifted, or that would simply litter the repo with orphaned
    files."""
    fixture_path = common.REPO_ROOT / args.path
    summary_path = common.REPO_ROOT / args.summary
    if (fixture_path.exists() or summary_path.exists()) and not args.force:
        print(f"refusing to overwrite an existing file at '{args.path}' "
              f"or '{args.summary}' -- pass --force if this is deliberate",
              file=sys.stderr)
        return 1

    # Captured BEFORE any write, so a failure at ANY stage below can
    # restore both files to their EXACT prior state (or remove them, if
    # they didn't exist before this invocation) -- never leaving a
    # half-written or stale-but-mismatched pair behind.
    orig_fixture = fixture_path.read_bytes() if fixture_path.exists() else None
    orig_summary = summary_path.read_text(encoding="utf-8") if summary_path.exists() else None

    def restore_files() -> None:
        if orig_fixture is None:
            fixture_path.unlink(missing_ok=True)
        else:
            fixture_path.write_bytes(orig_fixture)
        if orig_summary is None:
            summary_path.unlink(missing_ok=True)
        else:
            summary_path.write_text(orig_summary, encoding="utf-8")

    try:
        generate_current_format_session(
            port=args.port, page_id=args.page_id, seed=args.seed,
            world_size=args.world_size, plate_count=args.plate_count,
            spawn_building=args.spawn_building, spawn_unit=args.spawn_unit,
            out_path=fixture_path, spawn_unit_at=_parse_tile(args.spawn_unit_at),
            settle_seconds=args.settle_seconds, setup_lua=args.setup_lua,
            require_lua=args.require_lua, world_name=args.world_name,
            world_gloss=args.world_gloss, language_seed=args.language_seed,
            language_version=args.language_version,
            name_expr=args.name_expr)
    except GenerationError as e:
        # Round-16 review: generate_current_format_session no longer
        # ONLY writes fixture_path as an untouchable-if-failed last step
        # -- since round-11's codec.normalize_fixture_timestamp call, a
        # GenerationError can ALSO be raised AFTER shutil.copyfile has
        # already overwritten fixture_path with newly-generated (but
        # not-yet-normalized) bytes, e.g. clobbering a previously-tracked
        # fixture under --force with no rollback. restore_files() is
        # always safe to call here regardless of which stage failed --
        # it is a no-op when fixture_path was never actually touched.
        restore_files()
        print(f"fixture generation failed (fixture/summary restored to "
              f"their prior state): {e}", file=sys.stderr)
        return 1

    ok, tail = codec.dump_canonical_summary(fixture_path, summary_path)
    if not ok:
        restore_files()
        print(f"canonical-summary derivation failed (fixture/summary "
              f"restored to their prior state): {tail}", file=sys.stderr)
        return 1

    args.kind = "complete-session"
    if not args.provenance:
        args.provenance = (
            f"Generated through the real codec (tools/save_compat_audit.py "
            f"--generate-session): a real headless engine booted in an "
            f"isolated resource root, world.init('{args.page_id}', "
            f"{args.seed}, {args.world_size}, {args.plate_count}"
            + (f", '{args.world_name}'" if args.world_name else "")
            + (f" named in the generated language seeded "
               f"{args.language_seed}" if args.language_seed else "")
            + ")"
            + (f", building.spawn('{args.spawn_building}', 0, 0)"
               if args.spawn_building else "")
            + (f", unit.spawn('{args.spawn_unit}', {args.spawn_unit_at}, "
               f"0, 'player')" if args.spawn_unit else "")
            + (f", then " + "; ".join(args.setup_lua)
               if args.setup_lua else "")
            + (f", settled {args.settle_seconds}s"
               if args.settle_seconds else "")
            + (f" and held until `{args.require_lua}`"
               if args.require_lua else "")
            + f", then engine.saveWorld -- the exact production save path "
              f"an ordinary player save takes. Its canonical summary was "
              f"derived directly from the real decoded SessionSnapshot "
              f"(dump_canonical_summary), not hand-transcribed.")
    rc = register.cmd_add_baseline(args)
    if rc != 0:
        restore_files()
        print(f"registration/validation failed -- fixture/summary "
              f"restored to their prior state too (not just the "
              f"manifest)", file=sys.stderr)
    return rc
