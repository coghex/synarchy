"""The enum append-only audit's synthetic mutation suite.

Proves the audit can FAIL, rather than passing vacuously (requirement
4), on synthetic trees rather than the real one: each fixture is a
miniature repository holding the modules the audit's root/liveness
rules require, plus whatever enum the case is about.

Loaded only by `tools/enum_append_only_audit.py --self-test`. Nothing in
the production path imports it, and it reaches production only through
the two public entry points below.
"""
from __future__ import annotations

import contextlib
import io
import json
import tempfile
from pathlib import Path

from enum_append_only_audit_model import BASELINE_REL, REPO_ROOT, AuditError
from enum_append_only_audit_commands import (
    run_repository_audit,
    run_update_baseline,
)
from enum_append_only_audit_scan import scan_repository


_TYPES_HS = """\
module World.Save.Component.Types where

unitSimComponentId ∷ ComponentId
unitSimComponentId = ComponentId "unit-sim"
unitsComponentId ∷ ComponentId
unitsComponentId = ComponentId "units"
worldPagesComponentId ∷ ComponentId
worldPagesComponentId = ComponentId "world-pages"
worldEditsComponentId ∷ ComponentId
worldEditsComponentId = ComponentId "world-edits"
worldActivityComponentId ∷ ComponentId
worldActivityComponentId = ComponentId "world-activity"
"""

# A component module shaped like the real
# `World.Save.Component.PageCore`: its codec decodes INTO a canonical
# type (`WorldPages`) that reaches the whole live snapshot, while the
# bytes it actually writes are the `*DTO*`s. Seeding the canonical type
# instead would attribute every enum in the session to `"world-pages"`.
# It is named for the real owner because `NON_WIRE_COMPONENT_DECLS` is a
# PRODUCTION constant checked against this tree — its `WorldPages` entry
# must name a module that exists here.
_PAGE_CORE_HS = """\
module World.Save.Component.PageCore where

data WorldPages = WorldPages ![PageSnapshot]

data PageCoreDTO = PageCoreDTO
    { pcPageId ∷ !WorldPageId
    } deriving (Show, Eq, Generic, Serialize)

newtype WorldPagesDTO = WorldPagesDTO [PageCoreDTO]
    deriving (Show, Eq, Generic, Serialize)

worldPagesCodec ∷ ComponentCodec WorldPages
worldPagesCodec = componentCodec ComponentSpec
    { csComponent = worldPagesComponentId
    , csVersion   = 1
    , csDecode    = basePageSnapshots
    }

basePageSnapshots ∷ WorldPagesDTO → WorldPages
basePageSnapshots = undefined
"""

# A second component owner, shaped like the real
# `World.Save.Component.PageEdits`: the module declaring a guarded wire
# SUM also declares the codec that puts it on the wire. That pairing is
# what makes a module move change the type's carrier LABEL as well as
# its qualified key — the case #2135's owner split introduced and
# section 3d below mutates. Its `via` path is two hops
# (`WorldEditsDTO → PageEditsDTO → WorldEditDTO`), so a case can change
# the path without touching anything else.
_PAGE_EDITS_HS = """\
module World.Save.Component.PageEdits where

data WorldEditDTO
    = WeDeleteTileD !Int !Int
    | WeAddTileD !Int !Int !Int
    deriving (Show, Eq, Generic, Serialize)

data PageEditsDTO = PageEditsDTO
    { pedEdits ∷ ![WorldEditDTO]
    } deriving (Show, Eq, Generic, Serialize)

newtype WorldEditsDTO = WorldEditsDTO [PageEditsDTO]
    deriving (Show, Eq, Generic, Serialize)

worldEditsCodec ∷ ComponentCodec WorldEditsDTO
worldEditsCodec = componentCodec ComponentSpec
    { csComponent = worldEditsComponentId
    , csVersion   = 1
    }
"""

# NOT a wire module: the canonical in-memory session shape, reachable
# from `WorldPages` but never encoded by `"world-pages"`.
_SNAPSHOT_HS = """\
module World.Save.Snapshot where

data PageSnapshot = PageSnapshot
    { pgsPose ∷ !Pose
    } deriving (Show, Eq, Generic)
"""

_ENTITIES_HS = """\
module World.Save.Component.Entities where

data UnitSimStateDTO = UnitSimStateDTO
    { usdPose ∷ !Pose
    } deriving (Show, Eq, Generic, Serialize)

unitSimCodec ∷ ComponentCodec UnitSimStateDTO
unitSimCodec = componentCodec ComponentSpec
    { csComponent = unitSimComponentId
    , csVersion   = 1
    }

data UnitInstanceDTO = UnitInstanceDTO
    { uidPose ∷ !Pose
    } deriving (Show, Eq, Generic, Serialize)

unitsCodec ∷ ComponentCodec UnitInstanceDTO
unitsCodec = componentCodec ComponentSpec
    { csComponent = unitsComponentId
    , csVersion   = 1
    , csEncode    = map toUnitInstanceDTO . unitsOf
    }

toUnitInstanceDTO ∷ UnitInstanceSnapshot → UnitInstanceDTO
toUnitInstanceDTO = undefined
"""

_POSE_HS = """\
module Unit.Sim.Types where

-- | APPEND-ONLY.
data Pose
    = Standing     -- ^ upright
    | Crouching    -- ^ ducked
    | Crawling
    deriving (Show, Eq, Generic, Serialize)
"""

# A guarded sum no save-wire DTO reaches: the pre-emptively guarded half
# of the set, and the only thing that makes the on-wire count differ from
# the guarded total. Kept out of `_source_tree` so the coverage cases can
# add it without shifting every other fixture's baseline.
_DETACHED_HS = """\
module Extra.Detached where

data Detached = DetachedA | DetachedB
    deriving (Show, Eq, Generic, Serialize)
"""

# The modules WIRE_ROOT_EXTRA / WIRE_ROOT_GLOB_EXCLUSIONS require to
# exist. `World.Save.Types` carries a real shape so the fixtures cover
# the legacy bridge: `UnitInstanceSnapshot` is the LIVE side that
# `toUnitInstanceDTO` converts FROM, so a codec must not seed itself
# from it — but the bridge module is a wire root in its own right, so
# `ToolMode` is still reported as on the wire.
_STUB_MODULES = {
    "src/World/Save/Types.hs":
        "module World.Save.Types where\n\n"
        "data UnitInstanceSnapshot = UnitInstanceSnapshot\n"
        "    { uisTool ∷ !ToolMode\n"
        "    } deriving (Show, Eq, Generic, Serialize)\n",
    "src/World/Save/Envelope/Types.hs":
        "module World.Save.Envelope.Types where\n",
    "src/World/Save/Reference.hs": "module World.Save.Reference where\n",
    "src/World/Tool/Types.hs":
        "module World.Tool.Types where\n\n"
        "data ToolMode = DefaultTool | InfoTool | MineTool\n"
        "    deriving (Show, Eq, Generic, Serialize)\n",
}

_CLEAN_BASELINE_CACHE: str | None = None


def _source_tree() -> dict[str, str]:
    """The fixture repository, with no baseline file yet."""
    tree = dict(_STUB_MODULES)
    tree["src/World/Save/Component/Types.hs"] = _TYPES_HS
    tree["src/World/Save/Component/Entities.hs"] = _ENTITIES_HS
    tree["src/World/Save/Component/PageCore.hs"] = _PAGE_CORE_HS
    tree["src/World/Save/Component/PageEdits.hs"] = _PAGE_EDITS_HS
    tree["src/World/Save/Snapshot.hs"] = _SNAPSHOT_HS
    tree["src/Unit/Sim/Types.hs"] = _POSE_HS
    return tree


def _clean_baseline_text() -> str:
    """The baseline the fixture tree captures to, produced by the audit's
    OWN writer rather than transcribed — which is what makes the
    `expect_clean` cases prove that capturing and checking agree."""
    global _CLEAN_BASELINE_CACHE
    if _CLEAN_BASELINE_CACHE is None:
        _, out = _run(_source_tree(), update=True)
        _CLEAN_BASELINE_CACHE = out.split("<<baseline>>\n", 1)[1]
    return _CLEAN_BASELINE_CACHE


def _clean_tree() -> dict[str, str]:
    tree = _source_tree()
    tree[BASELINE_REL] = _clean_baseline_text()
    return tree


def _rewrite_baseline(edit) -> str:
    """The clean baseline with `edit` applied to its parsed document."""
    document = json.loads(_clean_baseline_text())
    edit(document)
    return json.dumps(document, indent=2, ensure_ascii=False) + "\n"


def _materialize(root: Path, tree: dict[str, str]) -> None:
    for rel, content in tree.items():
        path = root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


def _run(tree: dict[str, str], update: bool = False) -> tuple[int, str]:
    """Run the audit against a synthetic tree, capturing its output."""
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        _materialize(root, tree)
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            code = (run_update_baseline(root) if update
                    else run_repository_audit(root))
        text = buffer.getvalue()
        if update:
            written = root / BASELINE_REL
            text += "\n<<baseline>>\n"
            text += written.read_text(encoding="utf-8") if written.exists() \
                else "<<absent>>"
        return code, text


def _pose(*alternatives: str) -> str:
    """A `Pose` module declaring exactly these alternatives."""
    body = "\n    | ".join(alternatives)
    return ("module Unit.Sim.Types where\n\n"
            f"data Pose\n    = {body}\n"
            "    deriving (Show, Eq, Generic, Serialize)\n")


def _self_test() -> list[str]:
    failures: list[str] = []

    def expect_clean(label: str, tree: dict[str, str]) -> None:
        code, out = _run(tree)
        if code != 0:
            failures.append(f"{label}: expected a clean pass, got exit "
                            f"{code}:\n{out}")

    def expect_fail(label: str, tree: dict[str, str], *needles: str) -> None:
        code, out = _run(tree)
        if code == 0:
            failures.append(f"{label}: expected a failure, got a clean pass")
            return
        for needle in needles:
            if needle not in out:
                failures.append(
                    f"{label}: output did not mention {needle!r}:\n{out}")

    def with_pose(*alternatives: str) -> dict[str, str]:
        tree = _clean_tree()
        tree["src/Unit/Sim/Types.hs"] = _pose(*alternatives)
        return tree

    # 1. The tree the baseline was captured from passes — including with
    #    haddock comments hanging off the constructors, which must not
    #    drop one.
    expect_clean("clean tree", _clean_tree())

    # 1b. Issue #2299: the success line reports all three coverage
    #     figures, each read from its own field. Both fixtures are
    #     INTERNALLY CONSISTENT — source and baseline captured together
    #     by the audit's own writer — because editing `onSaveWire` in a
    #     baseline alone is detected as stale attribution and never
    #     reaches the success path at all.
    #
    #     `Extra.Detached` is guarded but off the wire, and `ToolMode`
    #     rides the bare `World.Save.Types` root with no component, so
    #     all three figures differ: a count taken from the wrong field
    #     cannot coincide with the right one. The second fixture then
    #     moves the wire coverage while leaving the guarded set alone,
    #     so a hard-coded figure fails too.
    def coverage_tree(entities: str) -> dict[str, str]:
        """The fixture tree plus one off-wire guarded sum, with
        `entities` as the `World.Save.Component.Entities` source and the
        baseline the audit's own writer captured from that pair."""
        tree = _source_tree()
        tree["src/Extra/Detached.hs"] = _DETACHED_HS
        tree["src/World/Save/Component/Entities.hs"] = entities
        code, out = _run(tree, update=True)
        if code != 0:
            failures.append(f"coverage fixture: could not capture a "
                            f"baseline:\n{out}")
        tree[BASELINE_REL] = out.split("<<baseline>>\n", 1)[1]
        return tree

    def expect_success_line(label: str, tree: dict[str, str],
                            guarded: int, on_wire: int,
                            named: int) -> None:
        """The WHOLE success output, not a substring: a figure that
        stops being emitted must fail here rather than pass quietly."""
        expected = (f"enum_append_only_audit.py: {guarded} guarded sum "
                    f"type(s) match {BASELINE_REL} ({on_wire} on the save "
                    f"wire, {named} named by a live component)")
        code, out = _run(tree)
        if code != 0:
            failures.append(f"coverage counts ({label}): expected a clean "
                            f"pass, got exit {code}:\n{out}")
        elif out.strip() != expected:
            failures.append(f"coverage counts ({label}): success output was\n"
                            f"  {out.strip()}\nexpected\n  {expected}")

    # `Pose` on the wire in two components, `WorldEditDTO` in one,
    # `ToolMode` on the wire with none, `Detached` off it: 4 / 3 / 2.
    expect_success_line("every figure distinct",
                        coverage_tree(_ENTITIES_HS), 4, 3, 2)
    # The same guarded set with `Pose` dropped out of both DTOs: the
    # guarded total holds while both other figures move.
    unwired = _ENTITIES_HS.replace("usdPose ∷ !Pose", "usdSeq ∷ !Int") \
                          .replace("uidPose ∷ !Pose", "uidSeq ∷ !Int")
    expect_success_line("wire coverage changed", coverage_tree(unwired),
                        4, 2, 1)

    # 2. Requirement 1: each of the four incompatible mutations fails,
    #    and names the tag whose meaning changed.
    expect_fail("reorder", with_pose("Crouching", "Standing", "Crawling"),
                "INCOMPATIBLE", "tag 0: was Standing/0, now Crouching/0",
                "every saved Standing decodes as Crouching")
    expect_fail("insertion",
                with_pose("Standing", "Sleeping", "Crouching", "Crawling"),
                "INCOMPATIBLE", "tag 1: was Crouching/0, now Sleeping/0")
    expect_fail("removal", with_pose("Standing", "Crawling"),
                "INCOMPATIBLE", "tag 1: was Crouching/0, now Crawling/0",
                "tag 2: Crawling/0 REMOVED")
    expect_fail("rename", with_pose("Standing", "Ducking", "Crawling"),
                "INCOMPATIBLE", "tag 1: was Crouching/0, now Ducking/0")
    expect_fail("arity change",
                with_pose("Standing", "Crouching !Int", "Crawling"),
                "INCOMPATIBLE",
                "Crouching carried 0 field(s), now carries 1")

    # 2b. Issue #1270: a SAME-ARITY payload mutation is the same kind of
    #     silent reinterpretation, and was invisible while the baseline
    #     recorded only name and arity. The fixture's `Pose` carries
    #     payload in both forms — positional and record — because only
    #     the record form makes a reorder of two SAME-TYPED fields
    #     visible at all (positionally, `!Int !Int` swapped is the same
    #     declaration text; nothing static can see it, and the docstring
    #     says so rather than implying otherwise).
    payload_alts = ("Standing",
                    "Crouching !Int !Text",
                    "Crawling { cwFrom ∷ !Int, cwTo ∷ !Int }")

    def payload_tree(*alternatives: str) -> dict[str, str]:
        """The payload-carrying fixture, with the baseline the audit's
        own writer captured from `payload_alts`."""
        tree = _source_tree()
        tree["src/Unit/Sim/Types.hs"] = _pose(*payload_alts)
        code, out = _run(tree, update=True)
        if code != 0:
            failures.append(f"payload fixture: could not capture a "
                            f"baseline:\n{out}")
        tree[BASELINE_REL] = out.split("<<baseline>>\n", 1)[1]
        if alternatives:
            tree["src/Unit/Sim/Types.hs"] = _pose(*alternatives)
        return tree

    expect_clean("payload fixture", payload_tree())
    # The baseline must actually RECORD the field slots, in both forms —
    # otherwise every mutation case below would be passing vacuously.
    for needle in ('"payload": [\n            "Int",\n            "Text"',
                   '"cwFrom ∷ Int",\n            "cwTo ∷ Int"'):
        if needle not in payload_tree()[BASELINE_REL]:
            failures.append(f"payload baseline: missing {needle!r}:"
                            f"\n{payload_tree()[BASELINE_REL]}")
    # A field's serialized TYPE changes: same name, same count, different
    # bytes after the tag.
    expect_fail("payload field type change",
                payload_tree("Standing", "Crouching !Word8 !Text",
                             "Crawling { cwFrom ∷ !Int, cwTo ∷ !Int }"),
                "INCOMPATIBLE",
                "tag 1: Crouching still carries 2 field(s), but their "
                "PAYLOAD changed",
                "field 0: was `Int`, now `Word8`")
    # Two positional fields swap: each slot's type moves.
    expect_fail("payload field reorder (positional)",
                payload_tree("Standing", "Crouching !Text !Int",
                             "Crawling { cwFrom ∷ !Int, cwTo ∷ !Int }"),
                "INCOMPATIBLE",
                "tag 1: Crouching still carries 2 field(s)",
                "field 0: was `Int`, now `Text`",
                "field 1: was `Text`, now `Int`")
    # Two SAME-TYPED record fields swap. This is the case the types alone
    # cannot see, and the reason a slot records its selector.
    expect_fail("payload field reorder (record, identical types)",
                payload_tree("Standing", "Crouching !Int !Text",
                             "Crawling { cwTo ∷ !Int, cwFrom ∷ !Int }"),
                "INCOMPATIBLE",
                "tag 2: Crawling still carries 2 field(s)",
                "field 0: was `cwFrom ∷ Int`, now `cwTo ∷ Int`",
                "field 1: was `cwTo ∷ Int`, now `cwFrom ∷ Int`")
    # The documented consequence of keeping the selector: a rename
    # reports too, exactly as a constructor rename already does.
    expect_fail("record selector rename",
                payload_tree("Standing", "Crouching !Int !Text",
                             "Crawling { cwStart ∷ !Int, cwTo ∷ !Int }"),
                "INCOMPATIBLE",
                "field 0: was `cwFrom ∷ Int`, now `cwStart ∷ Int`")
    # A payload mutation must carry the SAME component/DTO-path
    # attribution and migration guidance every other incompatible change
    # gets — it is the same class of break, so it needs the same answer.
    _, payload_out = _run(payload_tree(
        "Standing", "Crouching !Word8 !Text",
        "Crawling { cwFrom ∷ !Int, cwTo ∷ !Int }"))
    for needle in ('"unit-sim" — World.Save.Component.Entities',
                   '"units" — World.Save.Component.Entities',
                   "via UnitSimStateDTO → Pose",
                   "via UnitInstanceDTO → Pose", "Bump `ccVersion` on EVERY",
                   "`ccInputVers`", "Migrate from the frozen DTO"):
        if needle not in payload_out:
            failures.append(f"payload guidance: missing {needle!r}:"
                            f"\n{payload_out}")

    # 2c. Requirement 2: a WIRE-EQUIVALENT respelling of the very same
    #     fields must stay clean. Every erasure `normalize_field_type`
    #     claims is exercised here at once: strictness markers added and
    #     removed, an `{-# UNPACK #-}` pragma, `::` for `∷`, layout
    #     spread over lines, haddock comments between the fields, and
    #     the parentheses a `!` forces around an otherwise bare type.
    expect_clean("wire-equivalent respelling", payload_tree(
        "Standing",
        "Crouching\n        {-# UNPACK #-} !Int   -- ^ how long\n"
        "        Text",
        "Crawling { cwFrom :: Int   -- ^ from here\n"
        "             , cwTo :: !(Int) }"))
    # ...and the flip side, so that clemency is not blanket: a tuple's
    # parentheses ARE its type, and survive the same treatment.
    tupled = _source_tree()
    tupled["src/Unit/Sim/Types.hs"] = _pose(
        "Standing", "Sleeping !(Int, Int)", "Crawling")
    _, tupled_out = _run(tupled, update=True)
    tupled[BASELINE_REL] = tupled_out.split("<<baseline>>\n", 1)[1]
    if '"(Int, Int)"' not in tupled[BASELINE_REL]:
        failures.append(f"tuple field: parentheses were stripped from the "
                        f"recorded type:\n{tupled[BASELINE_REL]}")
    expect_clean("tuple field respelling", dict(tupled, **{
        "src/Unit/Sim/Types.hs": _pose(
            "Standing", "Sleeping ~(  Int ,Int  )", "Crawling")}))
    expect_fail("tuple field element change", dict(tupled, **{
        "src/Unit/Sim/Types.hs": _pose(
            "Standing", "Sleeping !(Int, Word8)", "Crawling")}),
        "INCOMPATIBLE", "field 0: was `(Int, Int)`, now `(Int, Word8)`")

    # 2d. A record's shared signature (`{ x, y ∷ !Int }`) distributes the
    #     type over every selector ahead of it, in declared order — and a
    #     field left with no signature at all reports rather than being
    #     dropped.
    shared = _source_tree()
    shared["src/Unit/Sim/Types.hs"] = _pose(
        "Standing", "Crawling { cwTo, cwFrom ∷ !Int }", "Crouching")
    _, shared_out = _run(shared, update=True)
    # Deliberately declared against alphabetical order: the recorded
    # slots must follow the DECLARATION, which is what the wire follows.
    needle = '"cwTo ∷ Int",\n            "cwFrom ∷ Int"'
    if needle not in shared_out:
        failures.append(f"shared record signature: missing {needle!r}:"
                        f"\n{shared_out}")
    unsigned = _clean_tree()
    unsigned["src/Extra/Types.hs"] = (
        "module Extra.Types where\n\n"
        "data Unsigned\n"
        "    = UnsignedA { ua ∷ !Int }\n"
        "    | UnsignedB { ub ∷ !Int, uc }\n"
        "    deriving (Show, Eq, Generic, Serialize)\n")
    expect_fail("record field with no type signature", unsigned,
                "carry no type signature")

    # 3. Requirement 6 + the review's multi-component correction: an
    #    incompatible change names EVERY affected component and the DTO
    #    path, and says what to do instead.
    _, reorder_out = _run(with_pose("Crouching", "Standing", "Crawling"))
    for needle in ('"unit-sim" — World.Save.Component.Entities',
                   '"units" — World.Save.Component.Entities',
                   "via UnitSimStateDTO → Pose",
                   "via UnitInstanceDTO → Pose", "Bump `ccVersion` on EVERY",
                   "`ccInputVers`", "Migrate from the frozen DTO"):
        if needle not in reorder_out:
            failures.append(f"migration guidance: missing {needle!r}:"
                            f"\n{reorder_out}")
    # ...and ONLY the affected ones. `"world-pages"` decodes into a
    # canonical type that reaches `Pose`, but the bytes it writes never
    # carry one — seeding a codec's decode TARGET instead of its DTOs
    # would name every component in the session and make the guidance
    # worthless.
    if '"world-pages"' in reorder_out:
        failures.append(
            "migration guidance: named `world-pages`, which carries no "
            f"`Pose` on the wire:\n{reorder_out}")
    # The same trap from the other side: `unitsCodec` names the LIVE
    # `UnitInstanceSnapshot` (as `toUnitInstanceDTO`'s argument), which
    # carries a `ToolMode` its DTO does not. Seeding a codec from the
    # live side would make `"units"` claim every enum in the snapshot.
    tool = _clean_tree()
    tool["src/World/Tool/Types.hs"] = (
        "module World.Tool.Types where\n\n"
        "data ToolMode = InfoTool | DefaultTool | MineTool\n"
        "    deriving (Show, Eq, Generic, Serialize)\n")
    _, tool_out = _run(tool)
    if "World.Save.Types — the transitional" not in tool_out:
        failures.append(f"migration guidance: `ToolMode` rides the legacy "
                        f"bridge and must say so:\n{tool_out}")
    if '"units"' in tool_out:
        failures.append(
            "migration guidance: `units` claimed a `ToolMode` that only its "
            f"codec's LIVE input carries, not its DTO:\n{tool_out}")

    # 3b. The same guidance is required when the DECLARATION IS GONE —
    #     renamed, moved, or deleted — which is the change that most
    #     needs it and the one no fresh walk can produce, since there is
    #     nothing left to reach. The attribution captured beside the
    #     constructor list is read back instead.
    for label, tree in (
            ("deleted", {k: v for k, v in _clean_tree().items()
                         if k != "src/Unit/Sim/Types.hs"}),
            ("renamed", dict(_clean_tree(), **{
                "src/Unit/Sim/Types.hs": _pose(
                    "Standing", "Crouching", "Crawling").replace(
                        "data Pose", "data Posture")}))):
        _, gone_out = _run(tree)
        for needle in ("INCOMPATIBLE", "Unit.Sim.Types.Pose",
                       "last seen in src/Unit/Sim/Types.hs",
                       "Standing/0, Crouching/0, Crawling/0",
                       "attribution as captured",
                       '"unit-sim" — World.Save.Component.Entities',
                       '"units" — World.Save.Component.Entities',
                       "via UnitSimStateDTO → Pose",
                       "via UnitInstanceDTO → Pose",
                       "Bump `ccVersion` on EVERY", "`ccInputVers`",
                       "Migrate from the frozen DTO"):
            if needle not in gone_out:
                failures.append(
                    f"{label} guarded type: guidance missing {needle!r}:"
                    f"\n{gone_out}")
    # A hand-added entry that never recorded any attribution must say so
    # rather than imply the type was safely off the wire.
    def bare_ghost(document) -> None:
        document["types"]["Unit.Sim.Types.Ghost"] = {
            "constructors": [{"name": "GhostA", "arity": 0, "payload": []},
                             {"name": "GhostB", "arity": 0, "payload": []}]}

    bare = _clean_tree()
    bare[BASELINE_REL] = _rewrite_baseline(bare_ghost)
    expect_fail("baseline-only entry with no recorded attribution", bare,
                "Unit.Sim.Types.Ghost", "recorded no save-wire carrier",
                "cannot be re-derived")

    # 3c. A pure MODULE MOVE is the one baseline-only outcome that is
    #     NOT a byte-reinterpreting change (issue #2098's owner split of
    #     the worldgen DTO graph is the motivating case). It ratchets
    #     like an append; everything that merely resembles it does not.
    def relocated(module: str, *alternatives: str) -> dict[str, str]:
        """The clean tree with `Pose` declared in `module` instead."""
        tree = {k: v for k, v in _clean_tree().items()
                if k != "src/Unit/Sim/Types.hs"}
        rel = "src/" + module.replace(".", "/") + ".hs"
        tree[rel] = _pose(*(alternatives or
                            ("Standing", "Crouching", "Crawling"))).replace(
            "module Unit.Sim.Types where", f"module {module} where")
        return tree

    move = relocated("Unit.Sim.Pose")
    code, move_out = _run(move)
    if code == 0:
        failures.append("relocation: must still fail until the baseline "
                        "records the new owner")
    if "INCOMPATIBLE" in move_out:
        failures.append(f"relocation: misreported as a byte-reinterpreting "
                        f"change:\n{move_out}")
    for needle in ("Unit.Sim.Pose.Pose", "RELOCATED from Unit.Sim.Types.Pose",
                   "last recorded in src/Unit/Sim/Types.hs",
                   "Standing/0, Crouching/0, Crawling/0",
                   "no saved byte changed meaning", "--update-baseline"):
        if needle not in move_out:
            failures.append(f"relocation: output did not mention {needle!r}:"
                            f"\n{move_out}")
    # The old key must NOT also be reported as a deletion — one move is
    # one fact, and a duplicate report is what would push a maintainer
    # back toward hand-editing the baseline.
    if "baseline only" in move_out:
        failures.append(f"relocation: also reported as a baseline-only "
                        f"deletion:\n{move_out}")
    # It ratchets through the supported writer, and the ratcheted tree
    # then passes with the ownership metadata pointing at the new owner.
    code, moved_out = _run(move, update=True)
    if code != 0:
        failures.append(f"relocation: --update-baseline refused a pure "
                        f"module move:\n{moved_out}")
    moved_baseline = moved_out.split("<<baseline>>\n", 1)[1]
    for needle in ('"Unit.Sim.Pose.Pose"', '"src/Unit/Sim/Pose.hs"'):
        if needle not in moved_baseline:
            failures.append(f"relocation ratchet: baseline missing {needle!r}:"
                            f"\n{moved_baseline}")
    if '"Unit.Sim.Types.Pose"' in moved_baseline:
        failures.append(f"relocation ratchet: baseline kept the stale "
                        f"qualified key:\n{moved_baseline}")
    expect_clean("relocation ratcheted",
                 dict(move, **{BASELINE_REL: moved_baseline}))

    # The mutation Codex's round-1 review caught: attribution is walked
    # by bare TYPE NAME, so a persisted enum DELETED from its DTO plus an
    # unrelated OFF-wire enum of the same name and constructors
    # elsewhere pairs on every other clause. Absorbing that as a
    # relocation would ratchet the entry to `onSaveWire: false` with no
    # components, erasing the attribution a later deletion's diagnostic
    # reads back — so the attribution must match too.
    lookalike = {k: v for k, v in _clean_tree().items()
                 if k != "src/Unit/Sim/Types.hs"}
    lookalike["src/World/Save/Component/Entities.hs"] = (
        _ENTITIES_HS.replace("usdPose ∷ !Pose", "usdSeq ∷ !Int")
                    .replace("uidPose ∷ !Pose", "uidSeq ∷ !Int"))
    lookalike["src/Extra/Types.hs"] = _pose(
        "Standing", "Crouching", "Crawling").replace(
            "module Unit.Sim.Types where", "module Extra.Types where")
    code, lookalike_out = _run(lookalike)
    if code == 0:
        failures.append("off-wire lookalike: expected a failure")
    for needle in ("INCOMPATIBLE", "Unit.Sim.Types.Pose", "baseline only"):
        if needle not in lookalike_out:
            failures.append(f"off-wire lookalike: output did not mention "
                            f"{needle!r}:\n{lookalike_out}")
    if "RELOCATED" in lookalike_out:
        failures.append(f"off-wire lookalike: absorbed as a relocation, "
                        f"which erases the recorded attribution:"
                        f"\n{lookalike_out}")
    # ...and the ratchet must not write it either, which is the step that
    # would actually destroy the captured components.
    code, lookalike_update = _run(lookalike, update=True)
    if code == 0:
        failures.append(f"off-wire lookalike: --update-baseline erased the "
                        f"recorded attribution:\n{lookalike_update}")
    if "refusing to update" not in lookalike_update:
        failures.append(f"off-wire lookalike: --update-baseline did not "
                        f"refuse loudly:\n{lookalike_update}")
    # The narrower half of the same rule: a move that keeps the type on
    # the wire but changes WHICH components carry it is not a relocation
    # either.
    fewer = relocated("Unit.Sim.Pose")
    fewer["src/World/Save/Component/Entities.hs"] = _ENTITIES_HS.replace(
        "uidPose ∷ !Pose", "uidSeq ∷ !Int")
    code, fewer_out = _run(fewer)
    if code == 0:
        failures.append("narrowed attribution: expected a failure")
    if "RELOCATED" in fewer_out:
        failures.append(f"narrowed attribution: a move that dropped the "
                        f'"units" carrier was absorbed as a relocation:'
                        f"\n{fewer_out}")
    if "INCOMPATIBLE" not in fewer_out:
        failures.append(f"narrowed attribution: not reported as "
                        f"incompatible:\n{fewer_out}")

    # The mutation that must NOT be absorbed: a move that also changes a
    # constructor is still the silent reinterpretation this audit exists
    # to catch.
    expect_fail("relocation with a reorder",
                relocated("Unit.Sim.Pose", "Crouching", "Standing", "Crawling"),
                "INCOMPATIBLE", "Unit.Sim.Types.Pose", "baseline only")
    expect_fail("relocation with an append",
                relocated("Unit.Sim.Pose",
                          "Standing", "Crouching", "Crawling", "Sleeping"),
                "INCOMPATIBLE", "Unit.Sim.Types.Pose", "baseline only")
    # ...nor an AMBIGUOUS pairing: two unmatched live types answering to
    # the same bare name are not evidence of which one moved.
    ambiguous = relocated("Unit.Sim.Pose")
    ambiguous["src/Unit/Sim/Stance.hs"] = _pose(
        "Standing", "Crouching", "Crawling").replace(
            "module Unit.Sim.Types where", "module Unit.Sim.Stance where")
    expect_fail("relocation with an ambiguous destination", ambiguous,
                "INCOMPATIBLE", "Unit.Sim.Types.Pose", "baseline only")
    # 3d. #2135's owner split is the case 3c's attribution clause was
    #     NOT written for. Splitting `World.Save.Component.Page` moved
    #     each codec into the owner module that also declares its own
    #     wire sum, so a moved type's carrier LABEL — which names the
    #     CODEC's module — moves with the declaration. Nothing on the
    #     wire changed, so it must ratchet; and eliding the module inside
    #     a label must not make any OTHER attribution difference
    #     ratchetable, so each of the three that must stay INCOMPATIBLE
    #     gets its own case in exactly this shape.
    def moved_owner(module: str, body: str | None = None) -> dict[str, str]:
        """The clean tree with the `"world-edits"` owner — its guarded
        sum AND the codec whose carrier label names its module — living
        in `module` instead, optionally with an edited `body`."""
        tree = {k: v for k, v in _clean_tree().items()
                if k != "src/World/Save/Component/PageEdits.hs"}
        rel = "src/" + module.replace(".", "/") + ".hs"
        tree[rel] = (_PAGE_EDITS_HS if body is None else body).replace(
            "module World.Save.Component.PageEdits where",
            f"module {module} where")
        return tree

    _MOVED_OWNER = "World.Save.Component.PageEditLog"

    def expect_not_relocated(label: str, tree: dict[str, str],
                             *needles: str) -> None:
        """A move the relaxed label comparison must still refuse."""
        code, out = _run(tree)
        if code == 0:
            failures.append(f"{label}: expected a failure, got a clean pass")
            return
        if "RELOCATED" in out:
            failures.append(f"{label}: absorbed as a relocation despite a "
                            f"real attribution change:\n{out}")
        for needle in ("INCOMPATIBLE",
                       "World.Save.Component.PageEdits.WorldEditDTO",
                       "baseline only") + needles:
            if needle not in out:
                failures.append(
                    f"{label}: output did not mention {needle!r}:\n{out}")
        # ...and the ratchet must refuse it too, which is the step that
        # would actually rewrite the captured attribution.
        code, update_out = _run(tree, update=True)
        if code == 0 or "refusing to update" not in update_out:
            failures.append(f"{label}: --update-baseline did not refuse "
                            f"loudly:\n{update_out}")

    owner_move = moved_owner(_MOVED_OWNER)
    code, owner_out = _run(owner_move)
    if code == 0:
        failures.append("component owner move: must still fail until the "
                        "baseline records the new owner")
    if "INCOMPATIBLE" in owner_out:
        failures.append(f"component owner move: misreported as a "
                        f"byte-reinterpreting change:\n{owner_out}")
    for needle in (f"{_MOVED_OWNER}.WorldEditDTO",
                   "RELOCATED from World.Save.Component.PageEdits."
                   "WorldEditDTO",
                   "WeDeleteTileD/2, WeAddTileD/3",
                   "no saved byte changed meaning", "--update-baseline"):
        if needle not in owner_out:
            failures.append(f"component owner move: output did not mention "
                            f"{needle!r}:\n{owner_out}")
    if "baseline only" in owner_out:
        failures.append(f"component owner move: also reported as a "
                        f"baseline-only deletion:\n{owner_out}")
    code, owner_ratchet = _run(owner_move, update=True)
    if code != 0:
        failures.append(f"component owner move: --update-baseline refused a "
                        f"pure module move:\n{owner_ratchet}")
    owner_baseline = owner_ratchet.split("<<baseline>>\n", 1)[1]
    # The ratchet must move BOTH pieces of ownership metadata: the
    # qualified key and the module named inside the carrier label. A
    # baseline that kept the old label would leave the relaxed
    # comparison permanently papering over a stale record.
    for needle in (f'"{_MOVED_OWNER}.WorldEditDTO"',
                   f'\\"world-edits\\" — {_MOVED_OWNER}'):
        if needle not in owner_baseline:
            failures.append(f"component owner move ratchet: baseline missing "
                            f"{needle!r}:\n{owner_baseline}")
    for stale in ('"World.Save.Component.PageEdits.WorldEditDTO"',
                  '\\"world-edits\\" — World.Save.Component.PageEdits'):
        if stale in owner_baseline:
            failures.append(f"component owner move ratchet: baseline kept "
                            f"{stale!r}:\n{owner_baseline}")
    expect_clean("component owner move ratcheted",
                 dict(owner_move, **{BASELINE_REL: owner_baseline}))

    # The three attribution facts the relaxation must NOT swallow. Each
    # is the same module move with exactly one of them also changed.
    expect_not_relocated(
        "component owner move that changes the COMPONENT",
        moved_owner(_MOVED_OWNER,
                    _PAGE_EDITS_HS.replace("worldEditsComponentId",
                                           "worldActivityComponentId")))
    expect_not_relocated(
        "component owner move that drops the type OFF the wire",
        moved_owner(_MOVED_OWNER,
                    _PAGE_EDITS_HS.replace("pedEdits ∷ ![WorldEditDTO]",
                                           "pedCount ∷ !Int")))
    expect_not_relocated(
        "component owner move that changes the `via` path",
        moved_owner(_MOVED_OWNER,
                    _PAGE_EDITS_HS.replace(
                        "newtype WorldEditsDTO = WorldEditsDTO [PageEditsDTO]",
                        "newtype WorldEditsDTO = WorldEditsDTO [WorldEditDTO]")))
    # ...and a constructor change alongside the move stays what it has
    # always been, in this shape too.
    expect_not_relocated(
        "component owner move with a constructor reorder",
        moved_owner(_MOVED_OWNER,
                    _PAGE_EDITS_HS.replace(
                        "    = WeDeleteTileD !Int !Int\n"
                        "    | WeAddTileD !Int !Int !Int\n",
                        "    = WeAddTileD !Int !Int !Int\n"
                        "    | WeDeleteTileD !Int !Int\n")))

    # A genuine DELETION still fails AND still cannot be ratcheted away.
    # The `deleted`/`renamed` guidance cases above prove the report; this
    # proves `--update-baseline` remains unable to erase the evidence,
    # which is the whole reason a relocation had to be recognised
    # explicitly rather than by relaxing the baseline-only rule.
    for label, tree in (
            ("deleted", {k: v for k, v in _clean_tree().items()
                         if k != "src/Unit/Sim/Types.hs"}),
            ("renamed", dict(_clean_tree(), **{
                "src/Unit/Sim/Types.hs": _pose(
                    "Standing", "Crouching", "Crawling").replace(
                        "data Pose", "data Posture")}))):
        code, out = _run(tree, update=True)
        if code == 0:
            failures.append(f"{label}: --update-baseline wrote over a "
                            f"non-append:\n{out}")
        if "refusing to update" not in out:
            failures.append(f"{label}: --update-baseline did not refuse "
                            f"loudly:\n{out}")

    # 4. Requirement 6: an append is classified as ALLOWED, distinctly
    #    from a failure, and still requires the baseline to ratchet.
    appended = with_pose("Standing", "Crouching", "Crawling", "Sleeping")
    code, out = _run(appended)
    if code == 0:
        failures.append("append: must still fail until the baseline ratchets")
    if "APPEND-COMPATIBLE" not in out or "--update-baseline" not in out:
        failures.append(f"append: not reported as append-compatible:\n{out}")
    if "INCOMPATIBLE" in out:
        failures.append(f"append: misreported as incompatible:\n{out}")

    # 5. The ratchet itself: --update-baseline records the append, the
    #    tree then passes, and REMOVING the appended constructor
    #    afterwards fails (the hole a non-ratcheting baseline leaves).
    code, out = _run(appended, update=True)
    if code != 0:
        failures.append(f"ratchet: --update-baseline refused an append:\n{out}")
    if '"name": "Sleeping"' not in out:
        failures.append(f"ratchet: baseline did not record the append:\n{out}")
    ratcheted = dict(appended)
    ratcheted[BASELINE_REL] = out.split("<<baseline>>\n", 1)[1]
    expect_clean("ratcheted tree", ratcheted)
    regressed = dict(ratcheted)
    regressed["src/Unit/Sim/Types.hs"] = _pose(
        "Standing", "Crouching", "Crawling")
    expect_fail("removal of a previously appended constructor", regressed,
                "INCOMPATIBLE", "tag 3: Sleeping/0 REMOVED")
    renamed_after_append = dict(ratcheted)
    renamed_after_append["src/Unit/Sim/Types.hs"] = _pose(
        "Standing", "Crouching", "Crawling", "Dozing")
    expect_fail("rename of a previously appended constructor",
                renamed_after_append, "INCOMPATIBLE",
                "tag 3: was Sleeping/0, now Dozing/0")

    # 5b. The ratchet carries PAYLOAD too: an appended constructor's
    #     field slots must land in the baseline, or the append would
    #     record a constructor whose payload nothing later compares.
    appended_payload = payload_tree(
        "Standing", "Crouching !Int !Text",
        "Crawling { cwFrom ∷ !Int, cwTo ∷ !Int }",
        "Sleeping { slDepth ∷ !Float }")
    code, out = _run(appended_payload)
    if code == 0 or "APPEND-COMPATIBLE" not in out or "INCOMPATIBLE" in out:
        failures.append(f"payload append: not reported as append-compatible:"
                        f"\n{out}")
    code, out = _run(appended_payload, update=True)
    if code != 0:
        failures.append(f"payload append: --update-baseline refused it:"
                        f"\n{out}")
    if '"slDepth ∷ Float"' not in out:
        failures.append(f"payload append: the appended constructor's payload "
                        f"was not recorded:\n{out}")
    payload_ratcheted = dict(appended_payload)
    payload_ratcheted[BASELINE_REL] = out.split("<<baseline>>\n", 1)[1]
    expect_clean("payload-ratcheted tree", payload_ratcheted)
    # ...and the appended constructor's own payload is guarded from then
    # on, which is the hole a payload-less ratchet would have left.
    expect_fail("payload change to a previously appended constructor",
                dict(payload_ratcheted, **{
                    "src/Unit/Sim/Types.hs": _pose(
                        "Standing", "Crouching !Int !Text",
                        "Crawling { cwFrom ∷ !Int, cwTo ∷ !Int }",
                        "Sleeping { slDepth ∷ !Word8 }")}),
                "INCOMPATIBLE",
                "field 0: was `slDepth ∷ Float`, now `slDepth ∷ Word8`")

    # 6. --update-baseline must never double as a "make it pass" button.
    code, out = _run(with_pose("Crouching", "Standing", "Crawling"),
                     update=True)
    if code == 0:
        failures.append("--update-baseline accepted a reorder")
    if "refusing to update" not in out:
        failures.append(f"--update-baseline: no refusal message:\n{out}")
    # ...including for a payload mutation, which must be refused for the
    # same reason and leave the recorded slots untouched.
    mutated = payload_tree("Standing", "Crouching !Word8 !Text",
                           "Crawling { cwTo ∷ !Int, cwFrom ∷ !Int }")
    code, out = _run(mutated, update=True)
    written = out.split("<<baseline>>\n", 1)[1]
    if code == 0:
        failures.append("--update-baseline accepted a payload mutation")
    if "refusing to update" not in out or "PAYLOAD changed" not in out:
        failures.append(f"--update-baseline: no payload refusal message:"
                        f"\n{out}")
    if written != mutated[BASELINE_REL]:
        failures.append("--update-baseline rewrote the baseline over a "
                        "payload mutation anyway")
    if '"name": "Crouching"' in out.split("<<baseline>>\n", 1)[1] \
            and out.split("<<baseline>>\n", 1)[1].index('"Crouching"') \
            < out.split("<<baseline>>\n", 1)[1].index('"Standing"'):
        failures.append("--update-baseline rewrote the baseline anyway")

    # 7. Guarded-set completeness: a newly qualifying enum with no
    #    baseline entry fails (and is append-compatible).
    new_enum = _clean_tree()
    new_enum["src/Craft/Bills.hs"] = (
        "module Craft.Bills where\n\n"
        "data BillMode = FixedCount | RepeatForever\n"
        "    deriving stock (Show, Eq, Generic)\n"
        "    deriving anyclass (Serialize)\n")
    expect_fail("newly qualifying enum without a baseline entry", new_enum,
                "Craft.Bills.BillMode", "newly qualifies", "APPEND-COMPATIBLE")

    # 8. ...in BOTH directions: a baseline entry with no live type fails.
    def add_ghost(document) -> None:
        document["types"]["Unit.Sim.Types.UnitActivity"] = {
            "constructors": [{"name": "Idle", "arity": 0, "payload": []},
                             {"name": "Walking", "arity": 0, "payload": []}]}

    stale = _clean_tree()
    stale[BASELINE_REL] = _rewrite_baseline(add_ghost)
    expect_fail("stale baseline entry", stale,
                "Unit.Sim.Types.UnitActivity", "baseline only",
                "no longer qualifies")

    # 9. ...on MODULE-QUALIFIED identities: the same type name in
    #    another module is never silently accepted as a match. Where the
    #    constructors are IDENTICAL it is recognised as a relocation and
    #    must ratchet under the NEW key (case 3c); where they are not,
    #    it stays two unrelated facts — a live type with no baseline
    #    entry, and a baseline entry with no live type.
    moved = _clean_tree()
    del moved["src/Unit/Sim/Types.hs"]
    moved["src/Unit/Pose.hs"] = _pose(
        "Standing", "Crawling", "Crouching").replace(
            "module Unit.Sim.Types", "module Unit.Pose")
    expect_fail("same type name in another module", moved,
                "Unit.Pose.Pose", "Unit.Sim.Types.Pose", "baseline only")
    # Neither key is ever assumed to stand for the other: the relocated
    # tree records the NEW qualified key and drops the old one, rather
    # than keeping the baseline pointed at a module that no longer
    # declares the type.
    relocated_baseline = _run(relocated("Unit.Pose"), update=True)[1]
    if '"Unit.Sim.Types.Pose"' in relocated_baseline:
        failures.append(f"module-qualified identity: a relocation kept the "
                        f"old module's key:\n{relocated_baseline}")

    # 10. The guarded-set rule itself: each of the three conditions
    #     genuinely excludes, and none of them excludes too much.
    for label, source in (
            ("newtype", "module Extra.Types where\n\n"
                        "newtype Wrap = Wrap Int\n"
                        "    deriving stock (Generic)\n"
                        "    deriving newtype (Serialize)\n"),
            ("single-constructor record",
             "module Extra.Types where\n\n"
             "data Only = Only { a ∷ !Int, b ∷ !Int }\n"
             "    deriving (Show, Eq, Generic, Serialize)\n"),
            ("sum with no Serialize instance",
             "module Extra.Types where\n\n"
             "data Plain = PlainA | PlainB\n"
             "    deriving (Show, Eq, Generic)\n"),
            ("sum whose Serialize is derived via newtype",
             "module Extra.Types where\n\n"
             "data Odd = OddA | OddB\n"
             "    deriving stock (Show, Generic)\n"
             "    deriving newtype (Serialize)\n"),
            ("type synonym", "module Extra.Types where\n\n"
                             "type Alias = Either Int Bool\n")):
        tree = _clean_tree()
        tree["src/Extra/Types.hs"] = source
        expect_clean(f"not guarded: {label}", tree)
    for label, source, ctors in (
            ("split stock/anyclass deriving clauses",
             "module Extra.Types where\n\n"
             "data Split = SplitA | SplitB\n"
             "    deriving stock (Show, Eq, Generic)\n"
             "    deriving anyclass (Hashable, Serialize)\n",
             "2 constructors"),
            ("unparenthesised single-class deriving clauses",
             "module Extra.Types where\n\n"
             "data Bare = BareA | BareB | BareC\n"
             "    deriving Generic\n"
             "    deriving Serialize\n",
             "3 constructors"),
            ("payload-carrying constructors",
             "module Extra.Types where\n\n"
             "data Payload\n"
             "    = PayA !Int !(Maybe (Int, Int))\n"
             "    | PayB ![(Text, Int)]\n"
             "    deriving (Show, Eq, Generic, Serialize)\n",
             "2 constructors")):
        tree = _clean_tree()
        tree["src/Extra/Types.hs"] = source
        expect_fail(f"guarded: {label}", tree, "Extra.Types.", ctors)

    # 11. Field counting: a record's shared-signature group and a
    #     positional constructor's bracketed types must each count once.
    tree = _clean_tree()
    tree["src/Extra/Types.hs"] = (
        "module Extra.Types where\n\n"
        "data Counted\n"
        "    = CountRec { x, y ∷ !Int, z ∷ !(Maybe (Int, Int)) }\n"
        "    | CountPos !Int !(Maybe Int) ![(Text, Int)]\n"
        "    deriving (Show, Eq, Generic, Serialize)\n")
    code, out = _run(tree, update=True)
    written = out.split("<<baseline>>\n", 1)[1]
    for needle in ('"name": "CountRec",\n          "arity": 3',
                   '"name": "CountPos",\n          "arity": 3'):
        if needle not in written:
            failures.append(f"field counting: missing {needle!r}:\n{written}")

    # 12. Fail-loud, not fail-quiet: a form the reader cannot classify
    #     must report rather than silently leave an enum unguarded.
    for label, rel, source, needle in (
            ("GADT syntax", "src/Extra/Types.hs",
             "module Extra.Types where\n\n"
             "data Gadt where\n"
             "    GA ∷ Gadt\n"
             "    deriving (Generic, Serialize)\n",
             "GADT syntax"),
            ("data family", "src/Extra/Types.hs",
             "module Extra.Types where\n\n"
             "data family Fam a\n",
             "data family/instance"),
            ("deriving via", "src/Extra/Types.hs",
             "module Extra.Types where\n\n"
             "data Viaed = ViaA | ViaB\n"
             "    deriving stock (Generic)\n"
             "    deriving (Serialize) via Wrapper\n",
             "deriving via"),
            ("standalone deriving", "src/Extra/Types.hs",
             "module Extra.Types where\n\n"
             "data Stand = StandA | StandB\n"
             "deriving instance Serialize Stand\n",
             "standalone `deriving ... Serialize`"),
            ("module header disagreeing with its path",
             "src/Extra/Types.hs",
             "module Extra.Other where\n", "module header says")):
        tree = _clean_tree()
        tree[rel] = source
        expect_fail(f"fail-loud: {label}", tree, needle)

    # 13. Vacuity: nothing discovered, or nothing declared, must fail.
    empty_src = {k: v for k, v in _clean_tree().items()
                 if k not in ("src/Unit/Sim/Types.hs",
                              "src/World/Tool/Types.hs",
                              # The `"world-edits"` owner declares the
                              # fixture's third guarded sum, so leaving it
                              # in would make this case prove nothing.
                              "src/World/Save/Component/PageEdits.hs")}
    expect_fail("no guarded types discovered", empty_src,
                "would pass vacuously")
    no_baseline = _clean_tree()
    no_baseline[BASELINE_REL] = json.dumps({"types": {}}, indent=2) + "\n"
    expect_fail("empty baseline", no_baseline, "declares no types")
    missing = {k: v for k, v in _clean_tree().items() if k != BASELINE_REL}
    expect_fail("missing baseline", missing, "baseline file is missing")
    for label, content, needle in (
            ("malformed JSON", "{ nope", "not valid JSON"),
            ("no types object", json.dumps({"nope": {}}),
             "expected an object with a `types` object"),
            ("entry without constructors",
             json.dumps({"types": {"Unit.Sim.Types.Pose": {}}}),
             "has no `constructors` list"),
            ("constructor without an arity",
             json.dumps({"types": {"Unit.Sim.Types.Pose": {"constructors": [
                 {"name": "Standing", "payload": []},
                 {"name": "Crouching", "arity": 0, "payload": []}]}}}),
             "without all of `name`, `arity` and `payload`"),
            ("constructor without a payload",
             json.dumps({"types": {"Unit.Sim.Types.Pose": {"constructors": [
                 {"name": "Standing", "arity": 0},
                 {"name": "Crouching", "arity": 0, "payload": []}]}}}),
             "without all of `name`, `arity` and `payload`"),
            ("non-integer arity",
             json.dumps({"types": {"Unit.Sim.Types.Pose": {"constructors": [
                 {"name": "Standing", "arity": "0", "payload": []},
                 {"name": "Crouching", "arity": 0, "payload": []}]}}}),
             "non-integer arity"),
            ("payload that is not a list of strings",
             json.dumps({"types": {"Unit.Sim.Types.Pose": {"constructors": [
                 {"name": "Standing", "arity": 1, "payload": [7]},
                 {"name": "Crouching", "arity": 0, "payload": []}]}}}),
             "not a list of field strings"),
            ("payload disagreeing with its own arity",
             json.dumps({"types": {"Unit.Sim.Types.Pose": {"constructors": [
                 {"name": "Standing", "arity": 0, "payload": ["Int"]},
                 {"name": "Crouching", "arity": 0, "payload": []}]}}}),
             "declares arity 0 but 1 payload field(s)")):
        tree = _clean_tree()
        tree[BASELINE_REL] = content
        expect_fail(f"malformed baseline: {label}", tree, needle)

    # 14. Root liveness: the carrier attribution's own declared modules
    #     must still exist, so a rename cannot silently shrink the roots.
    for label, dropped, needle in (
            ("WIRE_ROOT_EXTRA", "src/World/Save/Reference.hs",
             "stale WIRE_ROOT_EXTRA entry"),
            ("WIRE_ROOT_GLOB_EXCLUSIONS",
             "src/World/Save/Component/Types.hs",
             "stale WIRE_ROOT_GLOB_EXCLUSIONS entry"),
            ("NON_WIRE_COMPONENT_DECLS",
             "src/World/Save/Component/PageCore.hs",
             "stale NON_WIRE_COMPONENT_DECLS")):
        tree = {k: v for k, v in _clean_tree().items() if k != dropped}
        # Force a finding so the carrier walk runs.
        tree["src/Unit/Sim/Types.hs"] = _pose("Crouching", "Standing",
                                              "Crawling")
        expect_fail(f"root liveness: {label}", tree, needle)

    # 14a. The informational attribution is REGENERATED, not merely
    #      append-checked: a `components` list that no longer matches the
    #      code fails, because a diagnostic naming the wrong components
    #      is worse than none. It is reported as its own thing, never as
    #      a constructor change.
    def misattribute(document) -> None:
        document["types"]["Unit.Sim.Types.Pose"]["components"] = \
            ["world-pages"]

    misattributed = _clean_tree()
    misattributed[BASELINE_REL] = _rewrite_baseline(misattribute)
    code, out = _run(misattributed)
    if code == 0:
        failures.append("stale attribution: expected a failure")
    if "attribution no longer matches" not in out:
        failures.append(f"stale attribution: not reported as such:\n{out}")
    if "INCOMPATIBLE" in out or "APPEND-COMPATIBLE" in out:
        failures.append(f"stale attribution: misreported as a constructor "
                        f"change:\n{out}")

    # 14b. ...and the flip side: a NEW non-`DTO` declaration in a
    #      component module must be classified deliberately, never
    #      dropped from the roots by naming convention alone.
    unnamed = with_pose("Crouching", "Standing", "Crawling")
    unnamed["src/World/Save/Component/Entities.hs"] = _ENTITIES_HS + (
        "\ndata UnitSimStateWire = UnitSimStateWire\n"
        "    { uswPose ∷ !Pose\n"
        "    } deriving (Show, Eq, Generic, Serialize)\n")
    expect_fail("non-`DTO` declaration in a component module", unnamed,
                "is not named `*DTO*`", "NON_WIRE_COMPONENT_DECLS")

    # 14c. A codec whose component id or wire type this reader cannot
    #      resolve fails rather than attributing nothing — under-naming
    #      an affected component is the one direction that is unsafe.
    for label, block, needle in (
            ("unresolvable component id",
             "unknownCodec ∷ ComponentCodec PageCoreDTO\n"
             "unknownCodec = componentCodec ComponentSpec\n"
             "    { csComponent = mysteryComponentId\n"
             "    }\n",
             "does not resolve to a `ComponentId` definition"),
            ("no csComponent at all",
             "namelessCodec ∷ ComponentCodec PageCoreDTO\n"
             "namelessCodec = componentCodec ComponentSpec\n"
             "    { csVersion = 1\n"
             "    }\n",
             "declares no `csComponent`"),
            ("no resolvable wire type",
             "vagueCodec ∷ ComponentCodec WorldPages\n"
             "vagueCodec = componentCodec ComponentSpec\n"
             "    { csComponent = worldPagesComponentId\n"
             "    }\n",
             "names no wire type this reader can resolve")):
        tree = with_pose("Crouching", "Standing", "Crawling")
        tree["src/World/Save/Component/PageCore.hs"] = \
            _PAGE_CORE_HS + "\n" + block
        expect_fail(f"codec discovery: {label}", tree, needle)

    # 15. A guarded type no save-wire DTO reaches says so, rather than
    #     inventing a component to bump.
    orphan = with_pose("Crouching", "Standing", "Crawling")
    orphan["src/World/Save/Component/Entities.hs"] = \
        "module World.Save.Component.Entities where\n"
    expect_fail("unreachable guarded type", orphan,
                "No save-wire DTO reaches it")

    # 16. The comment stripper must not misread code AS a comment (which
    #     would blank a real constructor out of the compared set).
    commented = _clean_tree()
    commented["src/Unit/Sim/Types.hs"] = (
        "module Unit.Sim.Types where\n\n"
        "data Pose\n"
        "    = Standing   {- upright -}  -- ^ the default\n"
        "    | Crouching  -- ^ ducked {- not a real block -}\n"
        "    | Crawling\n"
        "    deriving (Show, Eq, Generic, Serialize)\n")
    expect_clean("comments around the constructors", commented)
    dashes = _clean_tree()
    dashes["src/Extra/Types.hs"] = (
        "module Extra.Types where\n\n"
        "step ∷ Int → Int\n"
        "step a = a --> a\n"
        "data Dashed = DashA | DashB\n"
        "    deriving (Show, Eq, Generic, Serialize)\n")
    expect_fail("a dash-run operator is not a comment", dashes,
                "Extra.Types.Dashed")

    # 17. The real repository must parse without a single unclassifiable
    #     declaration — the check that keeps the fixtures above honest.
    try:
        scan = scan_repository(REPO_ROOT)
    except AuditError as err:
        failures.append(f"real tree: {err}")
    else:
        if len(scan.guarded) < 2:
            failures.append(
                f"real tree: only {len(scan.guarded)} guarded sum type(s) "
                f"discovered — the discovery rule is not finding them")
        # Every type issue #1145 and its review named by hand: the five
        # in the issue body, the seven more its review found reused by
        # `Session`/`Page`/`WorldGen` DTOs, the `ToolMode` the frozen
        # `SessionV90` still carries, and the whole `GeoTimeline` closure
        # `WorldGenParamsDTO.gpGeoTimeline` drags in. Pinning them here
        # is what proves the discovery RULE covers the set someone
        # enumerated by reading the code, rather than merely finding
        # some types.
        for expected in (
                "Unit.Direction.Direction", "Unit.Sim.Types.Pose",
                "Unit.Sim.Types.UnitActivity", "Craft.Bills.BillMode",
                "Power.Types.PowerRole",
                "Engine.Graphics.Camera.CameraFacing",
                "World.Render.Zoom.Types.ZoomMapMode",
                "World.Construct.Types.ConstructStatus",
                "World.Fluid.Types.FluidType",
                "World.Weather.Types.PressureType",
                "World.Weather.Types.SurfaceType",
                "Location.Instance.LocationLifecycle",
                "World.Tool.Types.ToolMode",
                "World.Geology.Timeline.Types.GeoScale",
                "World.Geology.Timeline.Event.GeoEvent",
                "World.Geology.Timeline.Feature.FeatureShape",
                "World.Geology.Timeline.Feature.FeatureActivity",
                "World.Geology.Timeline.Feature.FeatureEvolution",
                "World.Geology.Timeline.Feature.VolcanicFeature",
                "World.Hydrology.Types.HydroFeature",
                "World.Hydrology.Types.HydroEvolution",
                "World.Hydrology.Types.LakeSource"):
            if expected not in scan.guarded:
                failures.append(
                    f"real tree: `{expected}` (named by issue #1145) is not "
                    f"in the discovered guarded set")
    return failures


def main_self_test() -> int:
    failures = _self_test()
    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for failure in failures:
            print(f"  FAIL: {failure}")
        return 1
    print("enum_append_only_audit.py self-test: all checks passed")
    return 0
