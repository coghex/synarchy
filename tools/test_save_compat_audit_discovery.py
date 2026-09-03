#!/usr/bin/env python3
"""Component-discovery and registry cases of the save-compat self-test
(issue #2073).

The sixteen members covering how
`save_compat_audit_components` finds what it audits: `componentCodec`
discovery over synthetic Haskell sources, `csOlderVersions` parsing and
every way a malformed table must be rejected, the source-path set that
is the whole directory rather than a hand-maintained list, and the Lua
persistence-module discovery.

Issue #2073's review pinned that last one --
`test_discover_lua_save_modules_finds_the_real_two_modules` -- to THIS
owner, matching requirement 9's own "Lua persistence-module discovery"
bullet; the issue's 13/17 count had it with the coverage owner instead.
It sits last here, immediately before the coverage owner's first member,
which is exactly where it sits in the pre-split run order.

Requirement 14: the discovery inputs these cases redirect --
`common.HASKELL_COMPONENT_SOURCE_PATHS` and
`common.COMPONENT_REGISTRY_SOURCE_PATH` -- are patched on
`save_compat_audit_common`, the module `save_compat_audit_components`
reads them from at call time.
"""
from __future__ import annotations

import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit_common as common  # noqa: E402
import save_compat_audit_components as components  # noqa: E402
import save_compat_audit_fingerprint as fingerprint  # noqa: E402

from selftestlib import expect  # noqa: E402


def test_detects_registered_component_missing_from_source_scan() -> None:
    print("round-16 review: a component registered in saveComponentRegistry's "
          "own authoritative list, but whose codec definition can't be found "
          "anywhere the scan looked, fails loudly rather than silently "
          "vanishing from the entire registry with no error at all")
    with tempfile.TemporaryDirectory() as d:
        tmp = Path(d)
        registry_path = tmp / "Component.hs"
        registry_path.write_text(
            "saveComponentRegistry ∷ [RegisteredComponent]\n"
            "saveComponentRegistry =\n"
            "    [ registerComponent coreSessionCodec\n"
            "        (\\_ d snap -> Right snap)\n"
            "    , registerComponent totallyMissingCodec\n"
            "        (\\_ d snap -> Right snap)\n"
            "    ]\n"
            "  where\n"
            "    unused = ()\n"
            "\n"
            "-- next binding\n")
        component_dir = tmp / "Component"
        component_dir.mkdir()
        only_file = component_dir / "Session.hs"
        only_file.write_text(
            "coreSessionComponentId = ComponentId \"core-session\"\n"
            "coreSessionCodec = componentCodec ComponentSpec\n"
            "    { csComponent     = coreSessionComponentId\n"
            "    , csVersion       = 1\n"
            "    , csRequired      = True\n"
            "    , csDeps          = []\n"
            "    , csEncode        = encodeCore\n"
            "    , csDecode        = id\n"
            "    , csOlderVersions = []\n"
            "    , csValidate      = const []\n"
            "    }\n")
        old_registry_path = common.COMPONENT_REGISTRY_SOURCE_PATH
        old_haskell_paths = common.HASKELL_COMPONENT_SOURCE_PATHS
        common.COMPONENT_REGISTRY_SOURCE_PATH = registry_path
        common.HASKELL_COMPONENT_SOURCE_PATHS = [only_file]
        try:
            try:
                components.real_component_registry()
                expect(False,
                       "expected real_component_registry() to raise for a "
                       "codec referenced in saveComponentRegistry but never "
                       "found by the source scan")
            except ValueError as e:
                expect("totallyMissingCodec" in str(e),
                       f"expected the error to name the missing codec, got: {e}")
        finally:
            common.COMPONENT_REGISTRY_SOURCE_PATH = old_registry_path
            common.HASKELL_COMPONENT_SOURCE_PATHS = old_haskell_paths


def test_discover_component_specs_derives_input_versions_from_one_declaration() -> None:
    print("issue #1093: the accepted-version set comes from the SAME single "
          "declaration the reader dispatches on -- csVersion plus each "
          "csOlderVersions `atVersion <n>` -- not a separately parsed list")
    specs = components.discover_component_specs("""\
singletonCodec ∷ ComponentCodec OneDTO
singletonCodec = componentCodec ComponentSpec
    { csComponent     = oneComponentId
    , csVersion       = 1
    , csRequired      = True
    , csDeps          = [otherComponentId]
    , csEncode        = \\snap → OneDTO (map f (pages snap))
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = const []
    }

-- Field ORDER deliberately shuffled, and the older-version list spans
-- several lines: neither may change what this parser reads.
evolvedCodec ∷ ComponentCodec ThreeDTO
evolvedCodec = componentCodec ComponentSpec
    { csVersion       = 3
    , csOlderVersions = [ atVersion 2 migrateV2
                        , atVersion 1 migrateV1 ]
    , csComponent     = threeComponentId
    , csRequired      = False
    , csDeps          = []
    , csEncode        = encodeThree
    , csDecode        = buildThree
    , csValidate      = validateThree
    }
""")
    by_codec = {s["codec"]: s for s in specs}
    expect(set(by_codec) == {"singletonCodec", "evolvedCodec"},
           f"expected both codecs discovered, got {sorted(by_codec)}")
    expect(by_codec["singletonCodec"] == {
               "codec": "singletonCodec",
               "componentIdIdent": "oneComponentId",
               "currentVersion": 1, "inputVersions": [1], "required": True},
           f"expected the singleton spec parsed from one declaration, got "
           f"{by_codec['singletonCodec']}")
    expect(by_codec["evolvedCodec"] == {
               "codec": "evolvedCodec",
               "componentIdIdent": "threeComponentId",
               "currentVersion": 3, "inputVersions": [1, 2, 3],
               "required": False},
           f"expected inputVersions [1, 2, 3] derived from csVersion + "
           f"csOlderVersions, got {by_codec['evolvedCodec']}")


def test_discover_component_specs_ignores_commented_out_fields() -> None:
    print("issue #1093: a `--` comment inside a spec cannot contribute a "
          "phantom accepted version or a phantom field")
    specs = components.discover_component_specs("""\
c ∷ ComponentCodec D
c = componentCodec ComponentSpec
    { csComponent     = cId
    , csVersion       = 2
    -- , csVersion       = 9   (an older value left in a comment)
    , csRequired      = True
    , csDeps          = []
    , csEncode        = enc
    , csDecode        = id
    , csOlderVersions = [ atVersion 1 migrateV1 ]  -- not atVersion 7 anything
    , csValidate      = const []
    }
""")
    expect(len(specs) == 1 and specs[0]["inputVersions"] == [1, 2],
           f"expected exactly [1, 2] from the live declarations, got {specs}")


def test_discover_component_specs_raises_on_a_spec_missing_a_needed_field() -> None:
    print("issue #1093: a spec whose fields this audit needs are absent "
          "raises loudly rather than silently matching into the NEXT codec's "
          "fields and reporting a wrong version for it")
    try:
        components.discover_component_specs("""\
brokenCodec = componentCodec ComponentSpec
    { csComponent     = brokenComponentId
    , csRequired      = True
    , csDeps          = []
    , csEncode        = enc
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = const []
    }

laterCodec = componentCodec ComponentSpec
    { csComponent     = laterComponentId
    , csVersion       = 7
    , csRequired      = True
    , csDeps          = []
    , csEncode        = enc
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = const []
    }
""", "synthetic.hs")
        expect(False, "expected discover_component_specs to raise for a spec "
                      "with no csVersion field")
    except ValueError as e:
        expect("brokenCodec" in str(e) and "csVersion" in str(e),
               f"expected the error to name the codec and the missing field, "
               f"got: {e}")


def test_hand_rolled_component_codec_is_no_longer_silently_discovered() -> None:
    print("issue #1093: every gameplay codec now goes through the shared "
          "construction, so a codec that hand-rolls the ComponentCodec "
          "record is NOT discovered -- and the saveComponentRegistry cross-"
          "check then fails loudly naming it, rather than this tool quietly "
          "reporting the component as undiscovered")
    with tempfile.TemporaryDirectory() as d:
        tmp = Path(d)
        registry_path = tmp / "Component.hs"
        registry_path.write_text(
            "saveComponentRegistry ∷ [RegisteredComponent]\n"
            "saveComponentRegistry =\n"
            "    [ registerComponent handRolledCodec\n"
            "        (\\_ d snap -> Right snap)\n"
            "    ]\n"
            "\n"
            "-- next binding\n")
        component_dir = tmp / "Component"
        component_dir.mkdir()
        only_file = component_dir / "Session.hs"
        only_file.write_text(
            "handRolledComponentId = ComponentId \"hand-rolled\"\n"
            "handRolledCodec ∷ ComponentCodec D\n"
            "handRolledCodec = ComponentCodec\n"
            "    { ccId        = handRolledComponentId\n"
            "    , ccVersion   = 2\n"
            "    , ccInputVers = [1, 2]\n"
            "    , ccRequired  = True\n"
            "    }\n")
        old_registry_path = common.COMPONENT_REGISTRY_SOURCE_PATH
        old_haskell_paths = common.HASKELL_COMPONENT_SOURCE_PATHS
        common.COMPONENT_REGISTRY_SOURCE_PATH = registry_path
        common.HASKELL_COMPONENT_SOURCE_PATHS = [only_file]
        try:
            try:
                components.real_component_registry()
                expect(False, "expected real_component_registry() to raise "
                              "for a hand-rolled ComponentCodec record")
            except ValueError as e:
                expect("handRolledCodec" in str(e),
                       f"expected the error to name the codec, got: {e}")
        finally:
            common.COMPONENT_REGISTRY_SOURCE_PATH = old_registry_path
            common.HASKELL_COMPONENT_SOURCE_PATHS = old_haskell_paths


def test_haskell_component_source_paths_discovers_new_files_automatically() -> None:
    print("round-16 review: HASKELL_COMPONENT_SOURCE_PATHS globs the "
          "Component/ directory rather than a fixed file list -- a brand-new "
          "file placed there is picked up with no code change needed")
    expect(len(common.HASKELL_COMPONENT_SOURCE_PATHS) >= 4,
           f"expected at least the 4 known Component/*.hs files, got "
           f"{common.HASKELL_COMPONENT_SOURCE_PATHS}")
    expect(all(p.suffix == ".hs" and p.parent.name == "Component"
               for p in common.HASKELL_COMPONENT_SOURCE_PATHS),
           f"expected every discovered path to be a .hs file directly under "
           f"a Component/ directory, got {common.HASKELL_COMPONENT_SOURCE_PATHS}")


def test_haskell_component_source_paths_is_the_whole_directory() -> None:
    print("issue #2098: HASKELL_COMPONENT_SOURCE_PATHS must equal the "
          "Component/ directory listing exactly -- the >= 4 check above "
          "cannot notice an owner DROPPED from discovery, and a dropped "
          "owner silently leaves its DTOs out of the B1 fingerprint")
    # _transitive_dto_blocks() `continue`s past a leaf it cannot resolve
    # rather than raising, so an owner module missing from the search
    # paths costs the fingerprint its declarations with no error at all.
    # Pinning the set to the real directory is what makes that
    # unrepresentable: the worldgen DTO graph's owners
    # (WorldGenClimate/WorldGenNaming/WorldGenCurrent/WorldGenHistory)
    # joined discovery by being placed in this directory, and any future
    # owner does too.
    directory = common.REPO_ROOT / "src" / "World" / "Save" / "Component"
    expected = sorted(directory.glob("*.hs"))
    expect(expected, f"expected {directory} to contain component sources")
    expect(common.HASKELL_COMPONENT_SOURCE_PATHS == expected,
           f"discovery does not match the directory listing.\n"
           f"  missing from discovery: "
           f"{sorted(set(expected) - set(common.HASKELL_COMPONENT_SOURCE_PATHS))}\n"
           f"  discovered but absent from the directory: "
           f"{sorted(set(common.HASKELL_COMPONENT_SOURCE_PATHS) - set(expected))}")
    # Every worldgen DTO owner is reached, by name, so a rename that
    # moved one out of this directory fails here rather than quietly
    # shrinking the fingerprint's input.
    discovered = {p.name for p in common.HASKELL_COMPONENT_SOURCE_PATHS}
    for owner in ("WorldGen.hs", "WorldGenClimate.hs", "WorldGenNaming.hs",
                  "WorldGenCurrent.hs", "WorldGenHistory.hs"):
        expect(owner in discovered,
               f"worldgen DTO owner {owner} is not in discovery: "
               f"{sorted(discovered)}")


def directory_owner(name: str, paths: list) -> Path:
    """The single discovered path whose file name is `name`."""
    matches = [p for p in paths if p.name == name]
    expect(len(matches) == 1,
           f"expected exactly one discovered {name}, got {matches}")
    return matches[0]


def test_dropping_one_owner_from_discovery_changes_the_fingerprint() -> None:
    print("issue #2098 requirement 8: mutation-removing any single "
          "discovered owner from HASKELL_COMPONENT_SOURCE_PATHS must move "
          "the frozen B1 DTO fingerprint or fail -- proving discovery is "
          "load-bearing rather than incidentally complete")
    baseline = fingerprint.frozen_dto_fingerprint()
    old_paths = common.HASKELL_COMPONENT_SOURCE_PATHS
    # Only the owners the B1 closure actually reaches can move the hash;
    # the mutation is meaningful for those, and the directory-equality
    # test above is what covers the rest.
    reached = []
    try:
        for dropped in old_paths:
            common.HASKELL_COMPONENT_SOURCE_PATHS = [
                p for p in old_paths if p != dropped]
            if fingerprint.frozen_dto_fingerprint() != baseline:
                reached.append(dropped.name)
    finally:
        common.HASKELL_COMPONENT_SOURCE_PATHS = old_paths
    expect(fingerprint.frozen_dto_fingerprint() == baseline,
           "the fingerprint did not return to its unmutated value")
    # SessionV90 seeds the worldgen half of the B1 closure through ONE
    # field, `wp90GenParams ∷ !WorldGenParamsDTOv1`, so the closure
    # reaches the historical owner that now declares that shape and the
    # climate leaves it embeds. Dropping either must move the hash.
    for owner in ("WorldGenHistory.hs", "WorldGenClimate.hs"):
        expect(owner in reached,
               f"dropping {owner} left the B1 fingerprint unchanged, so its "
               f"DTOs are not actually contributing: moved by {reached}")
    # The naming owner is deliberately NOT asserted above: world-pages v1
    # predates the location instance table (it stored three chunk-keyed
    # sets), so no naming DTO is reachable from today's B1 seed at all --
    # that was already true of the pre-split module and is not something
    # this split changed. Its discovery is still load-bearing, and the
    # audit's OWN leaf resolver is what proves it: a location DTO
    # resolves through the globbed paths, and stops resolving the moment
    # the naming owner is dropped from them. `_transitive_dto_blocks`
    # `continue`s past an unresolvable leaf silently, so this is exactly
    # the failure mode a missing owner would produce.
    naming = directory_owner("WorldGenNaming.hs", old_paths)
    for dto in ("LocationInstanceDTOv1", "RiverNameDTO", "NameExprDTO"):
        expect(fingerprint._find_type_definition(dto, old_paths) is not None,
               f"{dto} does not resolve through the discovered owners")
        expect(fingerprint._find_type_definition(
                   dto, [p for p in old_paths if p != naming]) is None,
               f"{dto} still resolved with the naming owner dropped from "
               f"discovery -- this mutation proves nothing")


def _malformed_older_versions_source(current: int, older: str) -> str:
    """One synthetic ComponentSpec whose only interesting field is its
    csOlderVersions declaration."""
    return f"""\
evolvedCodec ∷ ComponentCodec D
evolvedCodec = componentCodec ComponentSpec
    {{ csComponent     = evolvedComponentId
    , csVersion       = {current}
    , csRequired      = True
    , csDeps          = []
    , csEncode        = enc
    , csDecode        = id
    , csOlderVersions = {older}
    , csValidate      = const []
    }}
"""


def _expect_older_versions_rejected(current: int, older: str,
                                    must_mention: list[str],
                                    what: str) -> None:
    try:
        specs = components.discover_component_specs(
            _malformed_older_versions_source(current, older), "synthetic.hs")
        expect(False,
               f"expected discover_component_specs to raise for {what}, got "
               f"{specs}")
    except ValueError as e:
        missing = [m for m in must_mention if m not in str(e)]
        expect(not missing,
               f"expected the {what} error to mention {missing}, got: {e}")


def test_discover_component_specs_rejects_a_duplicate_older_version() -> None:
    print("issue #1275: csOlderVersions repeating a strictly OLDER version "
          "is rejected -- only the first decoder for a repeated version is "
          "ever reached by the dispatch table's lookup, so the second is "
          "silently unreachable. Set-normalizing it (the old "
          "`sorted({current} | set(older))`) erased exactly this evidence")
    _expect_older_versions_rejected(
        4, "[ atVersion 3 migrateV3, atVersion 2 migrateV2, "
           "atVersion 3 migrateV3Again ]",
        ["evolvedCodec", "v3", "more than once"],
        "a duplicate older version")


def test_discover_component_specs_rejects_the_current_version_as_older() -> None:
    print("issue #1275: csOlderVersions listing the CURRENT version is "
          "rejected -- the real current decoder shadows it (sortOn is stable "
          "and the current version is prepended), so its frozen DTO is never "
          "reached. Distinct from the duplicate case above, so uniqueness "
          "and strict ordering are demonstrated independently")
    _expect_older_versions_rejected(
        4, "[ atVersion 4 migrateV4, atVersion 1 migrateV1 ]",
        ["evolvedCodec", "v4", "csVersion"],
        "the current version declared as older")


def test_discover_component_specs_rejects_a_future_older_version() -> None:
    print("issue #1275: csOlderVersions listing a version NEWER than "
          "csVersion is rejected -- the reader would advertise and accept a "
          "version no writer has ever produced")
    _expect_older_versions_rejected(
        4, "[ atVersion 5 migrateV5, atVersion 1 migrateV1 ]",
        ["evolvedCodec", "v5", "NEWER"],
        "a future version declared as older")


def test_discover_component_specs_fails_closed_on_unreadable_older_entries() -> None:
    print("issue #1275: the parse fails CLOSED. A findall scan silently "
          "SKIPPED any element it did not recognize, deriving an accepted-"
          "version set missing real decoders; every one of these shapes must "
          "raise instead")
    for older, what in [
        ("[ atVersion legacyVersion migrateOld ]",
         "a non-literal atVersion argument"),
        ("[ ComponentVersion { cvVersion = 1, cvDecode = decodeV1 } ]",
         "a hand-built ComponentVersion"),
        ("[ frozenAt 1 migrateV1 ]", "a helper expression"),
        ("[ atVersion 1 migrateV1, someOtherEntry ]",
         "one unreadable element beside a readable one"),
        ("olderVersionsOf myComponent", "a non-list value"),
    ]:
        _expect_older_versions_rejected(
            4, older, ["evolvedCodec", "cannot enumerate"], what)


def test_discover_component_specs_exhausts_each_older_entry() -> None:
    print("issue #1275 (review round 1): matching only the HEAD of an entry "
          "is not fail-closed. An element whose readable `atVersion <n>` "
          "prefix is followed by more expression evaluates to a DIFFERENT "
          "ComponentVersion than the one recorded, so the entry must be "
          "consumed COMPLETELY or rejected")
    for older, what in [
        ("[ atVersion 1 migrateV1 `seq` atVersion futureVersion migrateFuture ]",
         "a trailing operator application hiding a non-literal version"),
        ("[ atVersion 1 (id ∷ D → D) `orElse` atVersion 9 migrateV9 ]",
         "a trailing operator application after a parenthesized build"),
        ("[ atVersion 1 migrateV1 extraArgument ]",
         "a second argument after the build"),
        ("[ atVersion 1 ]", "no build argument at all"),
        ("[ atVersion 1 $ migrateV1 ]",
         "an application operator this audit does not read"),
        ("[ atVersion 1 migrateV1 . fixup ]",
         "a composed build expression"),
    ]:
        _expect_older_versions_rejected(
            4, older, ["evolvedCodec", "cannot enumerate"], what)


def test_discover_component_specs_reads_the_real_build_argument_shapes() -> None:
    print("issue #1275 (review round 1): exhausting each entry must not "
          "reject the shapes actually shipped -- a bare identifier, a "
          "qualified name, and a balanced parenthesized expression "
          "(including nested parens and a string literal)")
    for older, expected in [
        ("[ atVersion 1 migrateWorldPagesV1 ]", [1, 4]),
        ("[ atVersion 1 Page.migrateV1 ]", [1, 4]),
        ("[ atVersion 1 (id ∷ WorldActivityDTO → WorldActivityDTO) ]", [1, 4]),
        ("[ atVersion 1 (fmap (dropTag \"v1\") . migrateV1) ]", [1, 4]),
        ("[ atVersion 2 (migrateV2 ∷ D2 → A), atVersion 1 migrateV1 ]",
         [1, 2, 4]),
    ]:
        specs = components.discover_component_specs(
            _malformed_older_versions_source(4, older), "synthetic.hs")
        expect(len(specs) == 1 and specs[0]["inputVersions"] == expected,
               f"expected inputVersions {expected} for {older}, got {specs}")


def test_discover_component_specs_accepts_a_well_formed_multi_version_table() -> None:
    print("issue #1275: a well-formed table is completely unaffected -- "
          "descending, ascending, and single-entry declarations all still "
          "yield the same ascending inputVersions they always did")
    for older, expected in [
        ("[ atVersion 3 m3, atVersion 2 m2, atVersion 1 m1 ]", [1, 2, 3, 4]),
        ("[ atVersion 1 m1, atVersion 2 m2, atVersion 3 m3 ]", [1, 2, 3, 4]),
        ("[ atVersion 1 (id ∷ D → D) ]", [1, 4]),
        ("[]", [4]),
    ]:
        specs = components.discover_component_specs(
            _malformed_older_versions_source(4, older), "synthetic.hs")
        expect(len(specs) == 1 and specs[0]["inputVersions"] == expected,
               f"expected inputVersions {expected} for {older}, got {specs}")


def test_discover_lua_save_modules_finds_the_real_two_modules() -> None:
    print("round-16 review: discover_lua_save_modules scans scripts/ rather "
          "than trusting a fixed 2-file list -- confirm it still finds the "
          "real, currently-registered unit_ai/building_spawn modules")
    discovered = components.discover_lua_save_modules()
    discovered_ids = {lua_id for _path, lua_id in discovered}
    expect({"unit_ai", "building_spawn"} <= discovered_ids,
           f"expected both real modules discovered, got {discovered_ids}")


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12).
TESTS = [
    test_detects_registered_component_missing_from_source_scan,
    test_discover_component_specs_derives_input_versions_from_one_declaration,
    test_discover_component_specs_ignores_commented_out_fields,
    test_discover_component_specs_raises_on_a_spec_missing_a_needed_field,
    test_discover_component_specs_rejects_a_duplicate_older_version,
    test_discover_component_specs_rejects_the_current_version_as_older,
    test_discover_component_specs_rejects_a_future_older_version,
    test_discover_component_specs_fails_closed_on_unreadable_older_entries,
    test_discover_component_specs_exhausts_each_older_entry,
    test_discover_component_specs_reads_the_real_build_argument_shapes,
    test_discover_component_specs_accepts_a_well_formed_multi_version_table,
    test_hand_rolled_component_codec_is_no_longer_silently_discovered,
    test_haskell_component_source_paths_discovers_new_files_automatically,
    test_haskell_component_source_paths_is_the_whole_directory,
    test_dropping_one_owner_from_discovery_changes_the_fingerprint,
    test_discover_lua_save_modules_finds_the_real_two_modules,
]
