"""Synthetic base/candidate suite for the current-schema guard (issue
#2916). Loaded only by `--self-test`; no production module imports it.

Every case mutates a copy of `current_save_schema_fixture.base_files()`,
builds a model for both trees from SOURCE and applies the rules. The git
cases build a throwaway repository and drive the real `run_check`.
"""
from __future__ import annotations

import contextlib
import io
import json
import os
import subprocess
import tempfile
from pathlib import Path

import current_save_schema_audit as audit
import current_save_schema_compare as compare
import current_save_schema_fixture as fixture
import current_save_schema_model as model

ALPHA = "src/World/Save/Component/Alpha.hs"
BETA = "src/World/Save/Component/Beta.hs"
ENVELOPE = "src/World/Save/Envelope.hs"
CODEC = "src/World/Save/Envelope/Codec.hs"
QUIRK = "src/Quirk/Types.hs"
META = "src/World/Save/Types.hs"
META_V1 = "src/World/Save/Compat/MetadataV1.hs"
SESSION = "src/World/Save/Compat/SessionV90.hs"
LUA_ONE = "scripts/one.lua"
LUA_TWO = "scripts/two.lua"
DATA_CODEC = "scripts/lib/data_codec.lua"
REGISTRY = "src/World/Save/Component.hs"

Edit = tuple[str, str, str]

ALPHA_BUMP: list[Edit] = [
    (ALPHA, "csVersion       = 2", "csVersion       = 3"),
    (ALPHA, "csOlderVersions = [ atVersion 1 migrateAlphaDTOv1 ]",
     "csOlderVersions = []")]
BETA_BUMP: list[Edit] = [(BETA, "csVersion       = 1", "csVersion       = 2")]
ALPHA_FIELD: Edit = (ALPHA, "    , adShared ∷ !SharedDTO\n",
                     "    , adShared ∷ !SharedDTO\n    , adExtra  ∷ !Int\n")
SHARED_FIELD: Edit = (ALPHA, "    , sdRight ∷ !Int\n",
                      "    , sdRight ∷ !Int\n    , sdMid   ∷ !Int\n")
META_FIELD: Edit = (META, "    , smExtra ∷ !Int\n",
                    "    , smExtra ∷ !Int\n    , smMore  ∷ !Int\n")
META_BUMP: list[Edit] = [
    (ENVELOPE, "metadataComponentVersion = 2", "metadataComponentVersion = 3")]
META_CURRENT_ONLY: Edit = (
    ENVELOPE,
    "    [ legacyMetadataComponentVersion\n    , metadataComponentVersion ]",
    "    [ metadataComponentVersion ]")
B1_OFF: list[Edit] = [
    (ENVELOPE, "decodeLegacySessionEnvelope", "legacyOffEnvelope"),
    (ENVELOPE, "decodeLegacySessionMetadata", "legacyOffMetadata")]
B2_OFF: list[Edit] = [
    (ENVELOPE, "decodeB2SessionEnvelope", "legacyOffB2Envelope"),
    (ENVELOPE, "decodeB2SessionMetadata", "legacyOffB2Metadata")]
LUA_ONE_CURRENT: list[Edit] = [
    (LUA_ONE, "version       = 2,", "version       = 3,"),
    (LUA_ONE, "inputVersions = { 1, 2 }", "inputVersions = { 3 }")]
DATA_CODEC_EDIT: Edit = (DATA_CODEC, "tostring(v)", "tostring(v) .. '!'")
LUA_TWO_CURRENT: list[Edit] = [
    (LUA_TWO, "version       = 1,", "version       = 2,"),
    (LUA_TWO, "inputVersions = { 1 }", "inputVersions = { 2 }")]
QUIRK_EDIT: Edit = (QUIRK, "putWord8 (fromIntegral n)",
                    "putWord8 (fromIntegral n + 1)")

LUA_THREE = """\
saveMods.register("three", {
    version       = %d,
    inputVersions = { %s },
    required      = false,
})
"""


def apply(files: dict[str, str], edits: list[Edit]) -> dict[str, str]:
    out = dict(files)
    for rel, old, new in edits:
        if old not in out.get(rel, ""):
            raise AssertionError(f"fixture drifted: {old!r} not in {rel}")
        out[rel] = out[rel].replace(old, new)
    return out


def build(files: dict[str, str]) -> model.Model:
    with tempfile.TemporaryDirectory(prefix="schema-st-") as tmp:
        fixture.write_tree(Path(tmp), files)
        return model.build_model(Path(tmp))


class Suite:
    def __init__(self) -> None:
        self.failures: list[str] = []
        self.count = 0
        self.base_files = fixture.base_files()
        self.base = build(self.base_files)

    def check(self, ok: bool, name: str, detail: str = "") -> None:
        self.count += 1
        if not ok:
            self.failures.append(f"{name}: {detail}")

    def compare_case(self, name: str, edits: list[Edit],
                     expect: str | None, files: dict[str, str] | None = None
                     ) -> None:
        """expect None = clean pass; "!text" = SchemaError containing
        text; any other string = a problem containing it."""
        try:
            cand = build(files if files is not None
                         else apply(self.base_files, edits))
        except model.SchemaError as exc:
            self.check(expect is not None and expect.startswith("!")
                       and expect[1:] in str(exc), name, f"SchemaError: {exc}")
            return
        problems = compare.compare(self.base, cand)
        joined = "\n".join(problems)
        if expect is None:
            self.check(not problems, name, joined)
        elif expect.startswith("!"):
            self.check(False, name, f"no SchemaError; got: {joined or 'ok'}")
        else:
            self.check(expect in joined, name,
                       f"expected {expect!r}; got: {joined or 'pass'}")


def run_cases(s: Suite) -> None:
    c = s.compare_case
    # --- unchanged / non-shape edits pass
    c("unchanged", [], None)
    c("comment, layout and bang edits are ignored", [
        (ALPHA, "    { adName   ∷ !Text   -- ^ display name",
         "    { adName   ∷ Text    {- moved -} -- ^ renamed doc"),
        (ALPHA, "-- | The alpha payload.", "-- | Reworded.\n\n"),
        (QUIRK, "instance Serialize Quirk where",
         "-- comment\ninstance   Serialize   Quirk   where")], None)
    c("off-wire enum append needs no bump", [
        (QUIRK, "data OffWire = OffA | OffB", "data OffWire = OffA | OffB | OffC")
    ], None)
    # --- shape changes without a bump fail, naming the carrier
    c("field add", [ALPHA_FIELD], "carrier `alpha`: same-version drift")
    c("field remove", [(ALPHA, "    , adAlias  ∷ !AlphaAlias\n", "")],
      "carrier `alpha`: same-version drift")
    c("field reorder", [(ALPHA, "    , adKind   ∷ !AlphaKindDTO\n"
                         "    , adAlias  ∷ !AlphaAlias\n",
                         "    , adAlias  ∷ !AlphaAlias\n"
                         "    , adKind   ∷ !AlphaKindDTO\n")],
      "carrier `alpha`: same-version drift")
    c("field retype", [(ALPHA, "adName   ∷ !Text", "adName   ∷ !Int")],
      "carrier `alpha`: same-version drift")
    c("enum append without bump", [
        (ALPHA, "data AlphaKindDTO = KindA | KindB",
         "data AlphaKindDTO = KindA | KindB | KindC")],
      "AlphaKindDTO")
    c("alias retarget", [(ALPHA, "type AlphaAlias = Int",
                          "type AlphaAlias = Word32")], "AlphaAlias")
    c("newtype payload", [(ALPHA, "newtype AlphaWrap = AlphaWrap Int",
                           "newtype AlphaWrap = AlphaWrap Word32")],
      "AlphaWrap")
    c("derivation strategy", [
        (ALPHA, "newtype AlphaWrap = AlphaWrap Int\n"
         "    deriving stock (Show, Generic)\n"
         "    deriving anyclass (Serialize)",
         "newtype AlphaWrap = AlphaWrap Int\n"
         "    deriving stock (Show, Generic)\n"
         "    deriving newtype (Serialize)")], "AlphaWrap")
    c("newly persisted type enters coverage", [
        (ALPHA, ALPHA_FIELD[1], ALPHA_FIELD[1] + "    , adOff ∷ !OffWire\n")],
      "carrier `alpha`: same-version drift")
    c("custom serializer edit", [QUIRK_EDIT],
      "carrier `beta`: same-version drift")
    c("framing edit", [(CODEC, "encodeEnvelope n = n",
                        "encodeEnvelope n = n + 1")],
      "carrier `framing`: same-version drift")
    c("metadata edit", [META_FIELD], "carrier `metadata`: same-version drift")
    c("shared Lua codec edit", [DATA_CODEC_EDIT],
      "carrier `lua.one`: same-version drift")
    c("downgrade", [(ALPHA, "csVersion       = 2", "csVersion       = 1"),
                    (ALPHA, "csOlderVersions = [ atVersion 1 migrateAlphaDTOv1 ]",
                     "csOlderVersions = []")], "version went DOWN")
    # --- correct bumps pass without migrations or fixtures
    c("field add with a correct bump", [ALPHA_FIELD] + ALPHA_BUMP, None)
    c("enum append with a correct bump", [
        (ALPHA, "data AlphaKindDTO = KindA | KindB",
         "data AlphaKindDTO = KindA | KindB | KindC")] + ALPHA_BUMP, None)
    c("custom serializer bump", [QUIRK_EDIT] + BETA_BUMP, None)
    c("bump with no detectable shape change", BETA_BUMP, None)
    c("framing bump", [
        (CODEC, "encodeEnvelope n = n", "encodeEnvelope n = n + 1"),
        (ENVELOPE, "currentEnvelopeVersion = 1",
         "currentEnvelopeVersion = 2")], None)
    c("Lua bump, current only", LUA_ONE_CURRENT, None)
    c("shared codec edit, every Lua carrier bumped",
      [DATA_CODEC_EDIT] + LUA_ONE_CURRENT + LUA_TWO_CURRENT, None)
    # --- affected carriers must be current-only
    c("bump retaining the old version", [
        ALPHA_FIELD, (ALPHA, "csVersion       = 2", "csVersion       = 3"),
        (ALPHA, "[ atVersion 1 migrateAlphaDTOv1 ]",
         "[ atVersion 2 migrateAlphaDTOv1, atVersion 1 migrateAlphaDTOv1 ]")],
      "carrier `alpha` is affected (its version was bumped) but still "
      "accepts older version(s) [1, 2]")
    c("bump retaining v1 only", [
        ALPHA_FIELD, (ALPHA, "csVersion       = 2", "csVersion       = 3")],
      "still accepts older version(s) [1]")
    c("Lua bump retaining inputVersions", [
        (LUA_ONE, "version       = 2,", "version       = 3,")],
      "carrier `lua.one` is affected")
    c("version-only bump retaining an old version", [
        (ALPHA, "csVersion       = 2", "csVersion       = 3"),
        (ALPHA, "[ atVersion 1 migrateAlphaDTOv1 ]",
         "[ atVersion 2 migrateAlphaDTOv1, atVersion 1 migrateAlphaDTOv1 ]")],
      "still accepts")
    # --- shared shapes and partial bumps
    c("shared shape, partial bump", [SHARED_FIELD] + ALPHA_BUMP,
      "carrier `beta`: same-version drift")
    c("shared shape, full bump", [SHARED_FIELD] + ALPHA_BUMP + BETA_BUMP
      + B1_OFF, None)
    c("shared shape reaches the frozen session route", [SHARED_FIELD]
      + ALPHA_BUMP + BETA_BUMP, "legacy route `b1-session` is still enabled")
    # --- legacy-only declarations
    c("frozen DTO edited while still accepted", [
        (ALPHA, "    { a1Name ∷ !Text", "    { a1Name ∷ !Int")],
      "carrier `alpha` is affected (a declaration only an older accepted "
      "version reaches changed)")
    c("frozen DTO edited and the version dropped", [
        (ALPHA, "    { a1Name ∷ !Text", "    { a1Name ∷ !Int"),
        (ALPHA, "csOlderVersions = [ atVersion 1 migrateAlphaDTOv1 ]",
         "csOlderVersions = []")], None)
    c("frozen metadata mirror edited while accepted", [
        (META_V1, "smv1Seed ∷ !Int", "smv1Seed ∷ !Word32")],
      "carrier `metadata` is affected")
    c("frozen metadata mirror edited, v1 dropped", [
        (META_V1, "smv1Seed ∷ !Int", "smv1Seed ∷ !Word32"),
        META_CURRENT_ONLY], None)
    # --- metadata and legacy routes
    c("metadata bump retaining v1", META_BUMP + [META_FIELD],
      "carrier `metadata` is affected")
    c("metadata bump, current only, routes still on",
      META_BUMP + [META_FIELD, META_CURRENT_ONLY],
      "legacy route `b1-session` is still enabled")
    c("metadata bump, one route still on",
      META_BUMP + [META_FIELD, META_CURRENT_ONLY] + B1_OFF,
      "legacy route `b2-lua-state` is still enabled")
    c("metadata bump, current only, routes off",
      META_BUMP + [META_FIELD, META_CURRENT_ONLY] + B1_OFF + B2_OFF, None)
    c("frozen session shape edit, route on", [
        (SESSION, "s90Name   ∷ !Text", "s90Name   ∷ !Int")],
      "legacy route `b1-session` is still enabled")
    c("frozen session shape edit, route off", [
        (SESSION, "s90Name   ∷ !Text", "s90Name   ∷ !Int")] + B1_OFF, None)
    # --- added / removed carriers
    c("new Lua carrier, current only", [(
        "scripts/three.lua", "", "")], None,
        files={**s.base_files, "scripts/three.lua": LUA_THREE % (1, "1")})
    c("new Lua carrier accepting an older version", [], "carrier `lua.three` "
      "is new but accepts [1, 2]",
      files={**s.base_files, "scripts/three.lua": LUA_THREE % (2, "1, 2")})
    without_two = {k: v for k, v in s.base_files.items() if k != LUA_TWO}
    c("removed Lua carrier", [], None, files=without_two)
    # --- unknown or ambiguous coverage
    c("registry names an unknown codec", [
        (REGISTRY, "registerComponent betaCodec\n",
         "registerComponent betaCodec\n    , registerComponent gammaCodec\n")],
      "!registers gammaCodec")
    c("codec not registered", [
        (REGISTRY, "    , registerComponent betaCodec\n", "")],
      "!not registered in saveComponentRegistry")
    c("codec names no wire type", [
        (BETA, "BetaDTO", "BetaPayload")], "!names no *DTO* wire type")
    c("duplicate Lua module id", [
        (LUA_TWO, 'register("two"', 'register("one"')],
      "!ambiguous coverage: Lua save module `one`")
    c("Lua version not literal", [
        (LUA_ONE, "version       = 2,", "version       = someConst,")],
      "!declares no literal `version`")
    c("ambiguous type name", [
        (QUIRK, "data OffWire = OffA | OffB",
         "data AlphaKindDTO = Other\n\ndata OffWire = OffA | OffB")],
      "!ambiguous coverage: type name `AlphaKindDTO`")
    c("derived and custom serializer", [
        (QUIRK, "data Quirk = Quirk !Int",
         "data Quirk = Quirk !Int\n    deriving anyclass (Serialize)")],
      "!custom-serializer ambiguity")
    c("missing route dispatcher", [
        (ENVELOPE, "tryLegacyMetadataFallbacks", "tryOtherFallbacks")],
      "!legacy-route dispatcher `tryLegacyMetadataFallbacks`")
    c("missing shared Lua codec", [], "!shared Lua save codec",
      files={k: v for k, v in s.base_files.items() if k != DATA_CODEC})
    c("missing frozen metadata mirror", [], "!MetadataV1",
      files={k: v for k, v in s.base_files.items() if k != META_V1})


def run_record_cases(s: Suite) -> None:
    rel = model.RECORD_REL
    good = json.loads(compare.render_record(s.base))
    s.check(not compare.check_record(s.base, good, rel), "record matches")
    s.check("missing" in "".join(compare.check_record(s.base, None, rel)),
            "deleted record fails")
    stale = json.loads(compare.render_record(s.base))
    stale["carriers"]["alpha"]["currentVersion"] = 1
    problems = "\n".join(compare.check_record(s.base, stale, rel))
    s.check("`alpha` is out of date" in problems, "edited record fails",
            problems)
    extra = json.loads(compare.render_record(s.base))
    extra["carriers"]["ghost"] = extra["carriers"]["alpha"]
    s.check("stale carrier entry `ghost`" in "\n".join(
        compare.check_record(s.base, extra, rel)), "stale entry fails")
    short = json.loads(compare.render_record(s.base))
    del short["carriers"]["beta"]
    s.check("carrier `beta` is in the source but missing" in "\n".join(
        compare.check_record(s.base, short, rel)), "missing entry fails")
    # Regenerating the record for a drifted candidate passes the record
    # check but cannot pass the comparison.
    cand = build(apply(s.base_files, [ALPHA_FIELD]))
    regenerated = json.loads(compare.render_record(cand))
    s.check(not compare.check_record(cand, regenerated, rel),
            "regenerated record is self-consistent")
    s.check(any("same-version drift" in p
                for p in compare.compare(s.base, cand)),
            "regenerating the record cannot bless drift")
    s.check(any("same-version drift" in p
                for p in compare.compare(s.base, cand)),
            "bootstrap (base without a record) is still compared")
    # Evidence.
    ev = compare.Evidence
    s.check(compare.check_evidence(ev("b", "aaa", "bbb")) == [],
            "distinct revisions pass")
    s.check("candidate-to-itself" in "".join(
        compare.check_evidence(ev("b", "aaa", "aaa"))), "self comparison")
    s.check(compare.check_evidence(ev("b", "aaa", "aaa", dirty=True)) == [],
            "dirty tree against its own commit is a real comparison")
    s.check("missing" in "".join(compare.check_evidence(ev("b", "", "bbb"))),
            "unresolved base")


def git(cwd: Path, *args: str) -> str:
    env = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
               GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t")
    proc = subprocess.run(["git", "-C", str(cwd), *args], env=env,
                          capture_output=True, text=True)
    if proc.returncode != 0:
        raise AssertionError(f"git {args}: {proc.stderr}")
    return proc.stdout.strip()


def commit(root: Path, files: dict[str, str], message: str,
           with_record: bool = True) -> str:
    fixture.write_tree(root, files)
    if with_record:
        audit.run_update(root)
    git(root, "add", "-A")
    git(root, "commit", "-q", "-m", message)
    return git(root, "rev-parse", "HEAD")


def check_output(root: Path, ref: str) -> tuple[int, str]:
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        rc = audit.run_check(root, ref)
    return rc, buf.getvalue()


def run_git_cases(s: Suite) -> None:
    with tempfile.TemporaryDirectory(prefix="schema-git-") as tmp:
        root = Path(tmp)
        git(root, "init", "-q", "-b", "base")
        with contextlib.redirect_stdout(io.StringIO()):
            c0 = commit(root, s.base_files, "base", with_record=False)
        rc, out = check_output(root, "base")
        s.check(rc == 1 and "candidate-to-itself" in out
                and "record is missing" in out or "missing" in out,
                "base ref equal to HEAD fails", out)
        rc, out = check_output(root, "no-such-ref")
        s.check(rc == 1 and "evidence missing" in out, "unresolvable base",
                out)
        # Initial record introduction: record added, nothing else changed.
        git(root, "checkout", "-q", "-b", "feature")
        with contextlib.redirect_stdout(io.StringIO()):
            c1 = commit(root, {}, "introduce the record")
        rc, out = check_output(root, "base")
        s.check(rc == 0 and c0 in out and c1 in out,
                "bootstrap passes and records both SHAs", out)
        # Drift with a regenerated record fails.
        drift = apply(s.base_files, [ALPHA_FIELD])
        with contextlib.redirect_stdout(io.StringIO()):
            commit(root, drift, "drift")
        rc, out = check_output(root, "base")
        s.check(rc == 1 and "same-version drift" in out,
                "drift with a regenerated record fails", out)
        # Deleting the record cannot help either.
        (root / model.RECORD_REL).unlink()
        rc, out = check_output(root, "base")
        s.check(rc == 1 and "same-version drift" in out
                and "record is missing" in out or "missing" in out,
                "deleting the record fails", out)
        # A correct bump passes.
        good = apply(s.base_files, [ALPHA_FIELD] + ALPHA_BUMP)
        with contextlib.redirect_stdout(io.StringIO()):
            c2 = commit(root, good, "bump")
        rc, out = check_output(root, "base")
        s.check(rc == 0 and c2 in out, "correct bump passes", out)
        # An uncommitted drift is compared against the merge-base.
        fixture.write_tree(root, apply(good, [QUIRK_EDIT]))
        rc, out = check_output(root, "base")
        s.check(rc == 1 and "carrier `beta`" in out
                and "uncommitted" in out, "dirty candidate is compared", out)
        git(root, "checkout", "-q", "--", ".")
        # The base is the merge-base: the target branch moving on is not
        # read as this branch reverting it.
        git(root, "checkout", "-q", "base")
        with contextlib.redirect_stdout(io.StringIO()):
            commit(root, apply(s.base_files, BETA_BUMP), "base moves on",
                   with_record=False)
        git(root, "checkout", "-q", "feature")
        rc, out = check_output(root, "base")
        s.check(rc == 0, "merge-base, not the moved tip, is the base", out)


def run_real_tree(s: Suite) -> None:
    import save_compat_audit_components as components
    real = model.build_model(audit.REPO_ROOT)
    registry = components.real_component_registry()
    for rid, info in registry.items():
        carrier = real.carriers.get(rid)
        s.check(carrier is not None
                and carrier.current_version == info["currentVersion"]
                and carrier.accepted == sorted(info["inputVersions"]),
                f"real tree agrees with real_component_registry: {rid}",
                str(carrier))
    s.check(set(registry) <= set(real.carriers),
            "every discovered component is a carrier")
    s.check(real.carriers["framing"].accepted
            == [real.carriers["framing"].current_version],
            "real framing accepts only its current version")


def main_self_test() -> int:
    suite = Suite()
    run_cases(suite)
    run_record_cases(suite)
    run_git_cases(suite)
    run_real_tree(suite)
    if suite.failures:
        for failure in suite.failures:
            print(f"FAIL {failure}")
        print(f"{len(suite.failures)} of {suite.count} checks failed")
        return 1
    print(f"current-schema self-test: {suite.count} checks passed")
    return 0
