"""Self-test for tools/current_save_schema_audit.py (issue #2916).

Three layers, none of which is a model-only substitute for extractor
correctness:

  * validation and comparison rules over small records;
  * the base-record, bootstrap-eligibility and candidate-to-itself rules
    over REAL temporary git repositories, with the extraction injected;
  * the COMPILED fixture catalogue: real Haskell types walked by the real
    extractor (`Test.Headless.World.Save.CurrentSchema.Fixtures`) replayed
    through the comparator. Set SYNARCHY_CS_SKIP_COMPILED=1 to skip that
    layer when no toolchain is available.
"""
from __future__ import annotations

import copy
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import current_save_schema_audit as audit  # noqa: E402
import current_save_schema_compare as cmp  # noqa: E402
import current_save_schema_git as cgit  # noqa: E402
from current_save_schema_compare import RecordError  # noqa: E402


def node(fields=(), kind="generic", **extra):
    shape = {"kind": kind, "detail": "", "dataType": "", "newtype": False,
             "constructors": [{"name": "C", "record": True, "fields": [
                 {"selector": s, "type": t} for s, t in fields]}],
             "args": []}
    shape.update(extra)
    return {"shape": shape, "declared": None}


def carrier(cls, version, accepted=None, root=None, reaches=(), owners=(),
            detail=None, required=True):
    return {"class": cls, "version": version,
            "accepted": [version] if accepted is None else accepted,
            "required": required, "root": root, "reaches": list(reaches),
            "owners": list(owners), "detail": detail}


def mini(version=1, accepted=None, leaf_fields=(("n", "Int"),),
         route_on=True, lua_version=1, codec="aa", framing="ff"):
    return {
        "format": 1, "extractor": "current-save-schema/1",
        "types": {"M.Root": node([("leaf", "M.Leaf")]),
                  "M.Leaf": node(leaf_fields), "Int": node(kind="primitive"),
                  "M.Meta": node([("x", "Int")]), "M.Frame": node([("y", "Int")])},
        "carriers": {
            "haskell:a": carrier("component", version, accepted, "M.Root",
                                 ["M.Root", "M.Leaf", "Int"]),
            "haskell:b": carrier("component", 1, None, "M.Root",
                                 ["M.Root", "M.Leaf", "Int"]),
            "metadata": carrier("metadata", 3, [1, 2, 3], "M.Meta",
                                ["M.Meta", "Int"]),
            "framing": carrier("framing", 1, None, "M.Frame",
                               ["M.Frame", "Int"], detail={"layout": framing}),
            "lua:m": carrier("lua-module", lua_version, None, None, [],
                             ["scripts/m.lua", "scripts/lib/data_codec.lua"]),
            "legacy:session": carrier("legacy", 90, [90])},
        "routes": {"b1": {"owners": ["metadata", "haskell:a", "haskell:b"],
                          "enabled": {"publicFullSession": route_on,
                                      "directFullSession": route_on}}},
        "extras": {"luaWire": {"probeHex": codec}}}


def problems_of(base, cand):
    return cmp.compare(base, cand)[0]


class RecordValidation(unittest.TestCase):
    def test_valid(self):
        self.assertEqual(cmp.validate_record(mini()), [])

    def test_rejections(self):
        def broken(mut):
            r = mini()
            mut(r)
            return cmp.validate_record(r)
        cases = {
            "top key": lambda r: r.pop("routes"),
            "format": lambda r: r.update(format=2),
            "extractor": lambda r: r.update(extractor="x"),
            "own version": lambda r: r["carriers"]["haskell:a"].update(accepted=[2]),
            "newer accepted": lambda r: r["carriers"]["haskell:a"].update(accepted=[1, 5]),
            "unsorted": lambda r: r["carriers"]["metadata"].update(accepted=[3, 1]),
            "root unrecorded": lambda r: r["carriers"]["haskell:a"].update(root="Nope"),
            "unreached edge": lambda r: r["types"]["M.Leaf"]["shape"]["constructors"][0]["fields"].append({"selector": "z", "type": "Gone"}),
            "route owner": lambda r: r["routes"]["b1"]["owners"].append("ghost"),
            "no routes": lambda r: r.update(routes={}),
            "no lua": lambda r: r["carriers"].pop("lua:m"),
            "no framing": lambda r: r["carriers"].pop("framing"),
            "missing field": lambda r: r["carriers"]["metadata"].pop("detail"),
        }
        for name, mut in cases.items():
            with self.subTest(name):
                self.assertTrue(broken(mut), name)

    def test_parse_rejects_garbage(self):
        for text in ("", "{", "[]", '{"format": 1}'):
            with self.assertRaises(RecordError):
                cmp.parse_record(text, "x")


class Comparator(unittest.TestCase):
    def test_unchanged(self):
        self.assertEqual(problems_of(mini(), mini()), [])

    def test_same_version_drift_and_path(self):
        found = problems_of(mini(), mini(leaf_fields=(("n", "Int"), ("m", "Int"))))
        self.assertTrue(any("same-version drift" in p and "haskell:a" in p for p in found))
        self.assertTrue(any("Root -> Leaf" in p or "Leaf" in p for p in found))

    def test_shared_type_reports_every_carrier(self):
        found = problems_of(mini(), mini(leaf_fields=(("m", "Int"),)))
        self.assertTrue(any("`haskell:a`" in p for p in found))
        self.assertTrue(any("`haskell:b`" in p for p in found))

    def test_partial_bump_of_shared_type(self):
        cand = mini(version=2, accepted=[2], leaf_fields=(("m", "Int"),))
        found = problems_of(mini(), cand)
        self.assertFalse(any("`haskell:a`: same-version" in p for p in found))
        self.assertTrue(any("`haskell:b`: same-version" in p for p in found))

    def test_correct_bump_current_only_with_route_off(self):
        cand = mini(version=2, accepted=[2], leaf_fields=(("m", "Int"),), route_on=False)
        cand["carriers"]["haskell:b"].update(version=2, accepted=[2])
        self.assertEqual(problems_of(mini(), cand), [])

    def test_bump_keeping_older_version(self):
        cand = mini(version=2, accepted=[1, 2])
        self.assertTrue(any("still accepts" in p for p in problems_of(mini(), cand)))

    def test_route_enabled_over_affected_owner(self):
        cand = mini(version=2, accepted=[2], route_on=True)
        found = problems_of(mini(), cand)
        self.assertTrue(any("legacy route `b1`" in p and "haskell:a" in p for p in found))

    def test_route_over_unchanged_owners_needs_nothing(self):
        self.assertEqual(problems_of(mini(), mini(route_on=True)), [])

    def test_downgrade(self):
        self.assertTrue(any("went down" in p for p in problems_of(mini(version=2, accepted=[2]), mini())))

    def test_removing_one_older_registration_alone(self):
        base = mini(version=3, accepted=[1, 2, 3])
        self.assertEqual(problems_of(base, mini(version=3, accepted=[2, 3])), [])

    def test_unchanged_carrier_keeps_older_versions(self):
        base = mini(version=3, accepted=[1, 2, 3])
        self.assertEqual(problems_of(base, copy.deepcopy(base)), [])

    def test_new_carrier(self):
        cand = mini()
        cand["carriers"]["haskell:c"] = carrier("component", 2, [1, 2], "M.Root", ["M.Root", "M.Leaf", "Int"])
        self.assertTrue(any("new carrier" in p for p in problems_of(mini(), cand)))
        cand["carriers"]["haskell:c"]["accepted"] = [2]
        found, notes = cmp.compare(mini(), cand)
        self.assertEqual(found, [])
        self.assertTrue(any("new carrier" in n for n in notes))

    def test_removed_carrier_is_reported(self):
        cand = mini()
        cand["carriers"].pop("haskell:b")
        cand["routes"]["b1"]["owners"].remove("haskell:b")
        found, notes = cmp.compare(mini(), cand)
        self.assertEqual(found, [])
        self.assertTrue(any("no longer covered" in n for n in notes))

    def test_lua_bump_must_be_current_only(self):
        cand = mini(lua_version=2)
        cand["carriers"]["lua:m"]["accepted"] = [1, 2]
        self.assertTrue(any("`lua:m`" in p and "still accepts" in p for p in problems_of(mini(), cand)))

    def test_shared_lua_codec_change_needs_every_lua_bump(self):
        found = problems_of(mini(), mini(codec="bb"))
        self.assertTrue(any("`lua:m`" in p and "data_codec" in p for p in found))

    def test_framing_detail_change(self):
        found = problems_of(mini(), mini(framing="gg"))
        self.assertTrue(any("`framing`" in p for p in found))

    def test_declared_prose_is_not_a_shape(self):
        cand = mini()
        cand["types"]["M.Leaf"]["declared"] = {"contract": "reworded"}
        cand["carriers"]["framing"]["detail"] = {"layout": "ff", "contract": "reworded"}
        self.assertEqual(problems_of(mini(), cand), [])

    def test_freshness(self):
        cand = mini()
        self.assertEqual(cmp.check_fresh(cand, cmp.render_record(cand), "r"), [])
        self.assertTrue(cmp.check_fresh(cand, None, "r"))
        self.assertTrue(cmp.check_fresh(cand, "{", "r"))
        stale = mini(leaf_fields=(("zz", "Int"),))
        self.assertTrue(cmp.check_fresh(cand, cmp.render_record(stale), "r"))
        gone = copy.deepcopy(cand)
        gone["carriers"]["extra"] = carrier("component", 1, None, "M.Root", ["M.Root"])
        self.assertTrue(any("stale" in p for p in cmp.check_fresh(cand, cmp.render_record(gone), "r")))


def git(root, *args):
    subprocess.run(["git", "-C", str(root), *args], check=True,
                   capture_output=True,
                   env=dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
                            GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t"))


def commit(root, files, message="c"):
    for rel, text in files.items():
        p = root / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        if text is None:
            p.unlink(missing_ok=True)
        else:
            p.write_text(text)
    git(root, "add", "-A")
    git(root, "commit", "-q", "-m", message)


class Evidence:
    def __init__(self, record):
        self.record = record
        self.examples, self.failures = 41, 0
        self.executable_sha256 = "0" * 64
        self.toolchain = {"ghc": "9.12.2"}


class BaseRecords(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        git(self.root, "init", "-q", "-b", "master")
        commit(self.root, {"README": "x"}, "base")
        self.boots = []

    def tearDown(self):
        self.tmp.cleanup()

    def bootstrap(self, root, rev):
        self.boots.append(rev.base_sha)
        return mini(), {"mode": "bootstrap", "adapterPatchSha256": "ab" * 32,
                        "examples": 40}

    def run_check(self, cand_record, **kw):
        return audit.check(self.root, "master", extract=lambda r: Evidence(cand_record),
                           bootstrap_fn=self.bootstrap)

    def start_branch(self):
        git(self.root, "checkout", "-q", "-b", "work")

    def test_first_introduction_bootstraps(self):
        self.start_branch()
        commit(self.root, {cgit.GUARD_ARTIFACTS[0]: "# guard"}, "introduce")
        result = self.run_check(mini())
        self.assertEqual(result["base"]["mode"], "bootstrap")
        self.assertEqual(len(self.boots), 1)

    def test_later_missing_record_never_bootstraps(self):
        commit(self.root, {cgit.GUARD_ARTIFACTS[0]: "# guard"}, "guard lands")
        self.start_branch()
        commit(self.root, {"x": "y"}, "change")
        with self.assertRaises(cgit.EvidenceError) as cm:
            self.run_check(mini())
        self.assertIn("never enables the bootstrap", str(cm.exception))
        self.assertEqual(self.boots, [])

    def test_malformed_and_unvalidated_base_records_fail(self):
        for text in ("{", json.dumps({"format": 1}), json.dumps(mini() | {"routes": {}})):
            with self.subTest(text[:20]):
                git(self.root, "checkout", "-q", "-B", "master", "HEAD")
                commit(self.root, {cgit.RECORD_REL: text}, "bad record")
                self.start_branch()
                commit(self.root, {"x": text[:3]}, "change")
                with self.assertRaises(cgit.EvidenceError):
                    self.run_check(mini())
                self.assertEqual(self.boots, [])
                git(self.root, "checkout", "-q", "master")
                git(self.root, "branch", "-q", "-D", "work")

    def land_base(self, record):
        commit(self.root, {cgit.RECORD_REL: cmp.render_record(record)}, "guard lands")
        self.start_branch()
        commit(self.root, {"x": "y"}, "change")

    def test_candidate_to_itself_is_rejected(self):
        commit(self.root, {cgit.RECORD_REL: cmp.render_record(mini())}, "guard")
        result = self.run_check(mini())
        self.assertTrue(any("candidate-to-itself" in p for p in result["problems"]))

    def test_same_version_mutation_fails_before_and_after_regeneration_or_deletion(self):
        self.land_base(mini())
        mutated = mini(leaf_fields=(("n", "Int"), ("m", "Int")))
        before = self.run_check(mutated)
        self.assertTrue(any("same-version drift" in p for p in before["problems"]))
        (self.root / cgit.RECORD_REL).write_text(cmp.render_record(mutated))
        after_regen = self.run_check(mutated)
        self.assertTrue(any("same-version drift" in p for p in after_regen["problems"]))
        self.assertFalse(any(cgit.RECORD_REL in p for p in after_regen["problems"]))
        (self.root / cgit.RECORD_REL).unlink()
        after_delete = self.run_check(mutated)
        self.assertTrue(any("same-version drift" in p for p in after_delete["problems"]))
        self.assertTrue(any("missing" in p for p in after_delete["problems"]))

    def test_correct_bump_with_current_only_dispatch_passes(self):
        self.land_base(mini())
        good = mini(version=2, accepted=[2], leaf_fields=(("m", "Int"),), route_on=False)
        good["carriers"]["haskell:b"].update(version=2, accepted=[2])
        (self.root / cgit.RECORD_REL).write_text(cmp.render_record(good))
        result = self.run_check(good)
        self.assertEqual(result["problems"], [])
        self.assertEqual(result["base"]["mode"], "committed-record")
        self.assertEqual(self.boots, [])

    def test_stale_committed_record_fails_freshness(self):
        self.land_base(mini())
        stale = mini()
        cand = mini(version=2, accepted=[2], route_on=False)
        cand["carriers"]["haskell:b"].update(version=1)
        (self.root / cgit.RECORD_REL).write_text(cmp.render_record(stale))
        result = self.run_check(cand)
        self.assertTrue(any("out of date" in p for p in result["problems"]))

    def test_missing_ref(self):
        with self.assertRaises(cgit.EvidenceError):
            audit.check(self.root, "no-such-ref", extract=lambda r: Evidence(mini()),
                        bootstrap_fn=self.bootstrap)


class Adapter(unittest.TestCase):
    ADD = ("diff --git a/src/A.hs b/src/A.hs\nnew file mode 100644\n"
           "--- /dev/null\n+++ b/src/A.hs\n@@ -0,0 +1 @@\n+x = 1\n")
    EDIT = ("diff --git a/src/World/Save/Envelope.hs b/src/World/Save/Envelope.hs\n"
            "--- a/src/World/Save/Envelope.hs\n+++ b/src/World/Save/Envelope.hs\n"
            "@@ -1 +1 @@\n-metadataComponentVersion = 3\n+metadataComponentVersion = 4\n")
    ALLOWED = ("diff --git a/src/World/Save/Envelope.hs b/src/World/Save/Envelope.hs\n"
               "--- a/src/World/Save/Envelope.hs\n+++ b/src/World/Save/Envelope.hs\n"
               "@@ -1 +1 @@\n-componentCodec ∷ S.Serialize d ⇒ ComponentSpec d a → ComponentCodec a\n+componentCodec\n")

    def test_additions_are_fine(self):
        self.assertEqual(cgit.adapter_problems(self.ADD.encode()), [])

    def test_a_version_edit_is_refused(self):
        found = cgit.adapter_problems(self.EDIT.encode())
        self.assertTrue(any("metadataComponentVersion = 3" in p for p in found))

    def test_hook_rewrites_are_allowed(self):
        self.assertEqual(cgit.adapter_problems(self.ALLOWED.encode()), [])


@unittest.skipIf(os.environ.get("SYNARCHY_CS_SKIP_COMPILED"),
                 "compiled fixture layer skipped by SYNARCHY_CS_SKIP_COMPILED")
class CompiledFixtures(unittest.TestCase):
    """Real compiled types, walked by the real extractor, replayed here."""

    @classmethod
    def setUpClass(cls):
        import current_save_schema_extract as ex
        cls.evidence = ex.build_and_run(audit.ROOT, want_fixtures=True)

    def test_catalogue_is_substantial(self):
        self.assertGreaterEqual(len(self.evidence.fixtures), 25)

    def test_every_case(self):
        for case in self.evidence.fixtures:
            with self.subTest(case["name"]):
                if case["candidateError"]:
                    self.assertEqual(case["verdict"], "fail")
                    self.assertTrue(any(case["match"] in e for e in case["candidateError"]))
                    continue
                found, _ = cmp.compare(case["base"], case["candidate"])
                if case["verdict"] == "pass":
                    self.assertEqual(found, [])
                else:
                    self.assertTrue(
                        any(case["match"].lower() in p.lower() for p in found),
                        f"expected a problem mentioning {case['match']!r}, got {found}")


def run() -> int:
    suite = unittest.defaultTestLoader.loadTestsFromModule(sys.modules[__name__])
    result = unittest.TextTestRunner(verbosity=1).run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(run())
