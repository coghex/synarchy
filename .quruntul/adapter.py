"""Synarchy's quruntul adapter: its test suites, and how to build and start each.

quruntul (https://github.com/coghex/quruntul) imports this file from the pinned
checkout it measures. It invents no inventory of its own; it reads Synarchy's:

- the two Hspec suites in synarchy.cabal: `synarchy-test-headless` (CI runs
  it; measured in slices of 400 examples, so the first flake batches never
  sweep the whole suite at once) and `synarchy-test-graphical` (CI only
  compiles it; it needs a display, so it is a desktop suite and never a
  routine target);
- every probe registered in `tools/probe_runner_registry.PROBES`, classified
  by `tools/ci_probes.CI_ELIGIBLE`. A manual-only probe on the
  `probe-result/v1` protocol (`tools/probe_flake.PROTOCOL_PROBES`) is a
  `command` suite whose tests are its declared checks, run once per trial
  through `tools/probe_flake.py --runs 1` (see `probe_trial.py`). Every other
  probe is an `exit` suite with one test, `run`, launched through the
  coordinated `tools/run_probes.py --only KEY --exact --jobs 1`.

Builds and launches go through Synarchy's own machinery, so its cross-process
`cabal-build` lock, probe resource holds and port spans keep applying. The
census (`docs/probe_census.json`, read from the `docs-wip` worktree when there
is one) is used in two ways: a census-deferred probe is not offered, and a
probe's existing census measurement seeds its tests' first status. The adapter
imports nothing from quruntul; the context supplies `Suite`, `Prepared` and
`digest`. Check it with `python3 .quruntul/checks.py`.
"""
from __future__ import annotations

import importlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time

HSPEC_SLICE = 400
LOCK_WAIT_SECONDS = 1800  # AGENTS.md: wait for cabal-build for up to 30 minutes
BUILD_SECONDS = 5400  # a cold engine and codec build, in prepare's own budget
TRIAL_MARGIN_SECONDS = 300  # process start-up and teardown around a probe
# Probes run_probes.py cannot launch unattended: they need a mode flag on a
# direct invocation (tools/ci_probes.py MANUAL_ONLY_REASONS says so).
DIRECT_ONLY = {"audio_manual": "needs --interactive or --offscreen-check, which run_probes.py cannot pass"}
# The run-level outcome of a protocol probe, beside its declared checks: a probe
# can report every check PASS and still exit nonzero, which probe_flake records
# as a failed run.
RUN_CHECK = "run"
# Everything a suite's behaviour can depend on, taken from the pinned revision:
# the sources, every tool a trial executes or imports (tools/, and this
# adapter's own runner in .quruntul/), and the build configuration. Whole trees
# are hashed deliberately; a narrower list is how an input gets left out.
IDENTITY_TREES = ("src", "app", "app-save-codec", "scripts", "data", "config", "assets", "cbits", "test",
                  "test-headless", "tools", ".quruntul", "BuildSupport",
                  "docs/save_compat")  # the save-compat baselines the spec and probe read
IDENTITY_FILES = ("synarchy.cabal", "cabal.project", "cabal.project.freeze", "Setup.hs",
                  "docs/audio_authoring.md")  # read by the headless audio catalog spec
# Exit probes run through run_probes.py --port BASE, which binds the probe's
# declared span from BASE. Spans are laid end to end in registry order, so no
# two suites share a port however many sessions run them at once.
PORT_BASE = 20000


def _tools(checkout: Path):
    """Synarchy's tool modules as the checkout being measured has them."""
    path = str(checkout / "tools")
    if path not in sys.path:
        sys.path.insert(0, path)
    names = ("probe_runner_registry", "ci_probes", "probe_flake", "probe_protocol", "probe_resource_lock")
    return {name: importlib.import_module(name) for name in names}


def _git(checkout: Path, *args: str) -> str:
    return subprocess.run(["git", *args], cwd=checkout, capture_output=True, text=True, check=True).stdout.strip()


def _census(checkout: Path) -> dict:
    """The probe census, preferring the docs-wip copy that its tools maintain."""
    listing = _git(checkout, "worktree", "list", "--porcelain")
    docs, current = None, None
    for line in listing.splitlines():
        if line.startswith("worktree "):
            current = line[len("worktree "):]
        elif line == "branch refs/heads/docs-wip":
            docs = current
    for candidate in ([Path(docs) / "docs" / "probe_census.json"] if docs else []) + [checkout / "docs" / "probe_census.json"]:
        if candidate.is_file():
            try:
                return {p["key"]: p for p in json.loads(candidate.read_text())["probes"]}
            except (ValueError, KeyError, TypeError):
                continue
    return {}


def _identity_inputs(checkout: Path, revision: str) -> dict[str, str]:
    """Git object ids of every identity input present at the revision."""
    found = {}
    for path in IDENTITY_TREES + IDENTITY_FILES:
        try:
            found[path] = _git(checkout, "rev-parse", f"{revision}:{path}")
        except subprocess.CalledProcessError:
            continue
    return found


def _port_bases(registry, keys) -> dict[str, int]:
    """A disjoint [base, base + declared span) for each key, in registry order."""
    bases, cursor = {}, PORT_BASE
    for key in keys:
        bases[key] = cursor
        cursor += int(registry.port_span(key))
    return bases


def _suite_id(key: str) -> str:
    return "probe:" + key.replace("_", "-")


class Synarchy:
    name = "synarchy"
    flake_trials = 10
    refresh_days = 7
    playtest = True  # the Synarchy $playtest workflow runs tools/playtest; $test may fall back to it

    def suites(self, ctx):
        checkout = ctx.checkout
        tools = _tools(checkout)
        census = _census(checkout)
        base = dict(inputs=_identity_inputs(checkout, ctx.revision))
        suites = [
            ctx.Suite(id="synarchy-test-headless", kind="ci", framework="hspec",
                      description="GPU-free Hspec specs (test-headless/), run by CI", area="headless",
                      identity=ctx.digest(dict(base, suite="headless")), trial_seconds=3600, batch_seconds=43200,
                      batch_tests=HSPEC_SLICE, data=dict(target="synarchy-test-headless")),
            ctx.Suite(id="synarchy-test-graphical", kind="probe", framework="hspec",
                      description="Hspec specs that initialise GLFW (test/); CI only compiles them", area="graphical",
                      platforms=["Darwin"], desktop=True, identity=ctx.digest(dict(base, suite="graphical")),
                      trial_seconds=3600, batch_seconds=43200, batch_tests=HSPEC_SLICE,
                      data=dict(target="synarchy-test-graphical")),
        ]
        registry, eligible = tools["probe_runner_registry"], tools["ci_probes"].CI_ELIGIBLE
        protocol = tools["probe_flake"].PROTOCOL_PROBES
        ports = _port_bases(registry, [key for key, _, _ in registry.PROBES])
        for key, script, description in registry.PROBES:
            entry = census.get(key) or {}
            if (entry.get("census") or {}).get("deferred") or key in DIRECT_ONLY:
                continue  # the census's deferral stays authoritative while the census exists
            ci = key in eligible
            framework = "command" if key in protocol and not ci else "exit"
            checks = self._describe(checkout, tools, key, script) + [RUN_CHECK] if framework == "command" else []
            timeout = float(registry.effective_timeout(key))
            # prepare builds; a trial only waits for the probe's resources and runs it.
            trial = int(timeout + LOCK_WAIT_SECONDS + TRIAL_MARGIN_SECONDS)
            suites.append(ctx.Suite(
                id=_suite_id(key), kind="ci" if ci else "probe", framework=framework,
                description=description, area=key.split("_")[0],
                desktop=key == "preview",  # Hidden GLFW windows still need the desktop claim.
                identity=ctx.digest(dict(base, key=key)),
                checks=checks, trial_seconds=trial, batch_seconds=86400,
                data=dict(key=key, script=script, port=ports[key], span=int(registry.port_span(key)))))
        return suites

    @staticmethod
    def _describe(checkout: Path, tools: dict, key: str, script: str) -> list[str]:
        """A protocol probe's declared checks, from its no-engine `--describe`."""
        done = subprocess.run([sys.executable, str(checkout / "tools" / script), "--describe"], cwd=checkout,
                              capture_output=True, text=True, timeout=120)
        if done.returncode:
            raise RuntimeError(f"{script} --describe exited {done.returncode}: {done.stderr.strip()[-500:]}")
        return list(tools["probe_protocol"].parse_descriptor(done.stdout, key).ids)

    def prepare(self, ctx, suite):
        checkout = ctx.checkout
        if suite.framework == "hspec":
            target = suite.data["target"]
            hold = self._cabal_lock(checkout, f"quruntul build of {target}")
            try:
                build = ctx.run(["cabal", "build", target], "build", 5400)
            finally:
                hold.release()
            if build["outcome"] != "passed":
                raise RuntimeError(f"build {build['outcome']}; see {build['log']}")
            executable = subprocess.run(["cabal", "list-bin", "-v0", target], cwd=checkout, capture_output=True,
                                        text=True, timeout=300).stdout.strip()
            if not executable or not Path(executable).is_file():
                raise RuntimeError(f"cabal cannot name the built executable of {target}")
            return ctx.Prepared(argv=[executable], cwd=str(checkout), environment={},
                                provenance=dict(target=target, executable=executable))
        key = suite.data["key"]
        environment = self._preflight(ctx)
        if suite.framework == "command":
            argv = [sys.executable, str(checkout / ".quruntul" / "probe_trial.py"), key]
        else:
            argv = [sys.executable, "tools/run_probes.py", "--only", key, "--exact", "--jobs", "1",
                    "--port", str(suite.data["port"])]
        return ctx.Prepared(argv=argv, cwd=str(checkout), environment=environment,
                            provenance=dict(key=key, executables=environment))

    @staticmethod
    def _preflight(ctx) -> dict[str, str]:
        """Build and locate the engine and codec once per batch, in prepare's budget."""
        result = ctx.run([sys.executable, str(ctx.checkout / ".quruntul" / "preflight.py")], "preflight",
                         LOCK_WAIT_SECONDS + BUILD_SECONDS)
        if result["outcome"] != "passed":
            raise RuntimeError(f"the engine preflight {result['outcome']}; see {result['log']}")
        lines = [line for line in Path(result["log"]).read_text(errors="replace").splitlines()
                 if line.startswith("QURUNTUL_EXECUTABLES ")]
        if not lines:
            raise RuntimeError(f"the engine preflight named no executables; see {result['log']}")
        return json.loads(lines[-1].split(" ", 1)[1])

    def outcomes(self, ctx, suite, trial):
        """A protocol probe's checks from its probe-flake-result/v1 document."""
        path = Path(trial["prefix"] + ".probe-flake.json")
        if not path.is_file():
            return None
        document = json.loads(path.read_text())
        # Only a valid measurement is evidence. probe_flake writes "harness-error"
        # for malformed or truncated protocol output; then there is nothing to
        # read, and quruntul stops the batch as blocked.
        if document.get("schema") != "probe-flake-result/v1" or document.get("status") != "ok":
            return None
        runs = document.get("runs") or []
        if not runs:
            return None
        mapped = {"PASS": "passed", "FAIL": "failed", "MISSING": "missing"}
        outcomes = {}
        for check, counts in document.get("check_counts", {}).items():
            outcome = next((mapped[k] for k in ("FAIL", "MISSING", "PASS") if counts.get(k)), None)
            if outcome:
                outcomes[check] = outcome
        # A run can report every check PASS and still fail (nonzero exit) or time out.
        outcomes[RUN_CHECK] = "passed" if runs[-1].get("outcome") == "PASS" else "failed"
        return outcomes

    def seed(self, ctx, suite, trial):
        """A probe's current census cohort becomes its tests' first status.

        Every sample in the cohort counts, so a failure recorded by an earlier
        sample is never hidden by a later clean one."""
        key = suite.data.get("key")
        entry = (_census(ctx.checkout).get(key) or {}).get("census") or {} if key else {}
        current = entry.get("current")
        if not current:
            return {}
        samples = current.get("samples") or [current]
        runs = sum(s.get("completed_runs", 0) for s in samples)
        failures = sum(s.get("failure_count") or 0 for s in samples)
        counts_by_check: dict[str, dict[str, int]] = {}
        for sample in samples:
            for check, counts in (sample.get("check_counts") or {}).items():
                total = counts_by_check.setdefault(check, {"PASS": 0, "FAIL": 0, "MISSING": 0})
                for outcome in total:
                    total[outcome] += counts.get(outcome, 0)
        evidence = dict(census_commit=current.get("commit_sha"), samples=len(samples), completed_runs=runs,
                        failure_count=failures)
        if suite.framework == "exit":
            if failures:
                return {"run": dict(status="flaky", reason=f"census: {failures} failed of {runs}", evidence=evidence)}
            return {"run": dict(status="stable", reason=f"census: {runs} clean runs", evidence=evidence)} if runs >= 10 else {}
        seeds = {}
        if failures:
            seeds[RUN_CHECK] = dict(status="flaky", reason=f"census: {failures} failed runs of {runs}", evidence=evidence)
        elif runs >= 10:
            seeds[RUN_CHECK] = dict(status="stable", reason=f"census: {runs} clean runs", evidence=evidence)
        for check, counts in counts_by_check.items():
            if check not in suite.checks:
                continue  # measured under a name the probe no longer declares
            bad = counts.get("FAIL", 0) + counts.get("MISSING", 0)
            if bad:
                seeds[check] = dict(status="flaky", reason=f"census: {bad} failed or missing of {runs}", evidence=evidence)
            elif counts.get("PASS", 0) >= 10:
                seeds[check] = dict(status="stable", reason=f"census: {counts['PASS']} clean runs", evidence=evidence)
        return seeds

    @staticmethod
    def _cabal_lock(checkout: Path, purpose: str):
        lock = _tools(checkout)["probe_resource_lock"]
        return lock.wait_acquire(exclusive=("cabal-build",), namespace=lock.repository_namespace(checkout),
                                 purpose=purpose, deadline=time.monotonic() + LOCK_WAIT_SECONDS)


def adapter():
    return Synarchy()
