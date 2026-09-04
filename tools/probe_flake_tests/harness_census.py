#!/usr/bin/env python3
"""Census-manifest integration (#2087).

The fixture case owns its own manifest and always runs. The real-registry
case validates `docs/probe_census.json`, which is written only into the
`docs-wip` worktree and deliberately never published, so it runs when that
worktree resolves and records a clear skip otherwise.
"""
from __future__ import annotations

import json
import shutil
import subprocess
import tempfile
from pathlib import Path

from .support import ci_probes, probe_census, probe_flake, probe_protocol, probe_runner_registry
from .support import SyntheticTree, expect_raises, skip, expect

def test_manifest_fixture() -> None:
    print("\n-- census manifest (self-owned fixture) --")
    with SyntheticTree(keys=("synthetic", "legacyprobe")) as tree:
        probe_flake.PROTOCOL_PROBES = {
            "synthetic": probe_protocol.PROTOCOL_VERSION}
        ci_probes.CI_ELIGIBLE = {"legacyprobe"}
        manifest = probe_census.build_manifest()
        expect(probe_census.validate_manifest(manifest) == [],
               "a freshly built manifest validates against the live registry")
        expect({e["key"] for e in manifest["probes"]} ==
               {"synthetic", "legacyprobe"},
               "the manifest lists every registered probe exactly once")
        expect([e for e in manifest["probes"]
                if e["key"] == "synthetic"][0]["protocol"] == "probe-result/v1",
               "a migrated probe is recorded as probe-result/v1")
        expect([e for e in manifest["probes"]
                if e["key"] == "legacyprobe"][0]["protocol"] == "legacy",
               "an unmigrated probe stays visibly legacy")
        expect([e for e in manifest["probes"]
                if e["key"] == "legacyprobe"][0]["classification"] ==
               "ci-eligible",
               "the classification comes from tools/ci_probes.py")

        def mutated(fn):
            doc = json.loads(json.dumps(manifest))
            fn(doc)
            return probe_census.validate_manifest(doc)

        expect(any("missing entry" in p for p in
                   mutated(lambda d: d["probes"].pop(0))),
               "a missing entry is rejected")
        expect(any("duplicate" in p for p in
                   mutated(lambda d: d["probes"].append(d["probes"][0]))),
               "a duplicate entry is rejected")
        expect(any("extra entry" in p for p in
                   mutated(lambda d: d["probes"].append(
                       {"key": "ghost", "script": "ghost_probe.py",
                        "classification": "manual-only", "protocol": "legacy"}))),
               "an extra entry naming no registered probe is rejected")
        expect(any("classification" in p for p in
                   mutated(lambda d: d["probes"][0].update(
                       {"classification": "manual-only"
                        if d["probes"][0]["classification"] == "ci-eligible"
                        else "ci-eligible"}))),
               "a classification disagreeing with ci_probes.py is rejected")
        expect(any("protocol status" in p for p in
                   mutated(lambda d: d["probes"][0].update(
                       {"protocol": "probe-result/v9"}))),
               "a protocol status disagreeing with the in-repo registry is rejected")
        expect(any("script" in p for p in
                   mutated(lambda d: d["probes"][0].update(
                       {"script": "wrong_probe.py"}))),
               "a script name disagreeing with the registry is rejected")
        expect(any("schema" in p for p in
                   mutated(lambda d: d.update({"schema": "probe-census/v9"}))),
               "an unexpected manifest schema is rejected")
        expect(probe_census.validate_manifest([]) != [],
               "a non-object manifest is rejected")
        expect(probe_census.validate_manifest({"schema": "probe-census/v1"}) != [],
               "a manifest with no probes list is rejected")

        # Seeding writes into the resolved docs worktree, never elsewhere.
        # A real git repository with no `docs-wip` worktree is the case
        # that must name the repair; a directory that is not a
        # repository at all is a different, also-reported failure.
        scratch = Path(tempfile.mkdtemp(prefix="probe-flake-git-"))
        try:
            subprocess.run(["git", "init", "-q", str(scratch)],
                           check=True, capture_output=True)
            expect_raises(probe_census.DocsWorktreeMissing,
                          lambda: probe_census.resolve_docs_worktree(str(scratch)),
                          "with no docs-wip worktree, seeding stops with an "
                          "actionable error", "git worktree add")
        finally:
            shutil.rmtree(scratch, ignore_errors=True)
        expect_raises(probe_census.DocsWorktreeMissing,
                      lambda: probe_census.resolve_docs_worktree(str(tree.root)),
                      "outside a git repository the manifest is never written "
                      "anyway", "could not list git worktrees")


def test_manifest_real_registry() -> None:
    print("\n-- census manifest (real registry, 86 probes) --")
    manifest = probe_census.build_manifest()
    expect(len(manifest["probes"]) == len(probe_runner_registry.PROBES),
           f"the manifest lists all {len(probe_runner_registry.PROBES)} registered probes")
    expect(len({e["key"] for e in manifest["probes"]}) == len(probe_runner_registry.PROBES),
           "each registered probe appears exactly once")
    expect(probe_census.validate_manifest(manifest) == [],
           "the built manifest agrees with probe_runner_registry.PROBES and ci_probes.py")
    ci = sum(1 for e in manifest["probes"]
             if e["classification"] == "ci-eligible")
    expect(ci == len(ci_probes.CI_ELIGIBLE),
           f"{ci} entries are CI-eligible, matching tools/ci_probes.py")
    migrated = [e["key"] for e in manifest["probes"]
                if e["protocol"] != "legacy"]
    expect(migrated == ["blood_decal", "blood_impact", "circadian",
                        "circadian_species", "collapse_crawl", "concussion_revive",
                        "config_state", "disarm", "injury_log", "lua_orphan_prune",
                        "lua_strict_msg", "machine_shop", "meal_waste",
                        "mental_efficiency", "position_hold",
                        "remote_warning_page_guard", "role", "state_of_mind",
                        "text_encoding", "thermo_altitude", "thought", "wire"],
           f"the twenty-two migrated probes are probe-result/v1 probes in "
           f"probe_runner_registry.PROBES order (got {migrated})")

    # The REAL docs-wip manifest, only when one is resolvable.
    try:
        path = probe_census.manifest_path()
    except probe_census.DocsWorktreeMissing as error:
        skip(f"no docs-wip worktree resolvable, so the real manifest is not "
             f"validated here ({str(error).splitlines()[0]})")
        return
    if not path.exists():
        skip(f"{path} has not been seeded yet")
        return
    problems = probe_census.validate_manifest(probe_census.load(path))
    expect(problems == [], f"the seeded {path} agrees with the live registry "
                           f"({problems[:3]})")


TESTS = (
    test_manifest_fixture,
    test_manifest_real_registry,
)
