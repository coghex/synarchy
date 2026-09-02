#!/usr/bin/env python3
"""Engine-preparation cases (#1913), split out by #2093.

The 4 cases pinning that the engine is prepared BEFORE the measurement
takes its resource hold: the ordering itself, namespace agreement
between preparation and the hold, a preparation failure releasing the
claim and measuring nothing, and the REAL preparation against the REAL
hold in one namespace -- with a fake `cabal` on `PATH` -- observing that
a would-be child preparation is excluded rather than assuming it.

These are the only cases that ASSIGN
`probe_runner_resources.ENGINE_EXECUTABLE` themselves, and they do it
under the shared `saved_runner_executable` -- the same helper `run`
uses to give back what `deflake` installs -- so the one writable
production module global this gate touches has one save/restore. The
`PATH` save/restore is the real-preparation case's alone.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_deflake.py --only preparation
"""
from __future__ import annotations

import json
import os
import sys
import time
import uuid
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_engine  # type: ignore  # noqa: E402
import probe_resource_lock  # type: ignore  # noqa: E402
import probe_runner_resources  # type: ignore  # noqa: E402
from deflake_selftest_support import (  # noqa: E402
    PREPARED_ENGINE, FakeClaim, Preparer, Recorder, Scratch, expect,
    held_resources, measurement, run, saved_runner_executable)


# --------------------------------------------------------------------------
# The engine is prepared BEFORE the measurement's hold (#1913)
#
# A probe that was handed no executable prepares its own, and that
# preparation takes `cabal-build` EXCLUSIVELY. A measurement holds the
# same resource — shared for an ordinary probe, exclusive for the three
# that drive Cabal themselves — for the whole of its runs, and
# `probe_runner_lifecycle.run_one` strips the inherited runner variables on the way
# down, so a child could neither see that hold nor upgrade past it. It
# would wait out its whole allowance for a holder blocked waiting on it.
#
# The fix is ordering, and these cases pin the ordering rather than the
# absence of a hang.
# --------------------------------------------------------------------------
FAKE_CABAL_SRC = """\
#!/usr/bin/env python3
import json, sys
from pathlib import Path
config = json.loads(Path(__file__).with_name("cabal.json").read_text())
with open(config["calls"], "a") as fh:
    fh.write(" ".join(sys.argv[1:]) + "\\n")
step = sys.argv[1] if len(sys.argv) > 1 else ""
if step == "build":
    sys.exit(0)
if step == "list-bin":
    print(config["engine"])
    sys.exit(0)
sys.exit(9)
"""


def test_the_engine_is_prepared_before_the_resource_hold() -> None:
    print("\n-- the engine is prepared before the measurement takes its hold")
    scratch = Scratch()
    order: list[str] = []
    try:
        with saved_runner_executable():
            probe_runner_resources.ENGINE_EXECUTABLE = None
            prepare = Preparer(observe=lambda: order.append("prepare"))

            def take(probe, *, namespace=None, repo_root=None):
                order.append("resources")
                return held_resources(probe, namespace=namespace,
                                      repo_root=repo_root)

            class Watching(Recorder):
                def __call__(self, *args, **kwargs):
                    order.append("measure")
                    self.seen_executable = probe_runner_resources.ENGINE_EXECUTABLE
                    return super().__call__(*args, **kwargs)

            measure = Watching(measurement(scratch))
            result = run(scratch, prepare_engine=prepare, acquire_resources=take,
                         measure=measure,
                         record_result=probe_census.record_result_installed)
            expect(result.outcome == deflake.OUTCOME_RECORDED,
                   f"the measurement is recorded ({result.outcome}: "
                   f"{result.detail})")
            expect(order == ["prepare", "resources", "measure"],
                   f"and preparation precedes the hold, which precedes the "
                   f"runs (got {order})")
            expect(len(prepare.calls) == 1,
                   f"one preparation for the whole measurement, not one per "
                   f"run (got {prepare.calls})")
            expect(getattr(measure, "seen_executable", None)
                   == PREPARED_ENGINE,
                   f"and the resolved path is already installed as the "
                   f"runner's executable when the runs start, so no child "
                   f"prepares its own (got "
                   f"{getattr(measure, 'seen_executable', None)!r})")
    finally:
        scratch.cleanup()


def test_preparation_and_the_hold_resolve_one_namespace() -> None:
    print("\n-- both name the same namespace, so they cannot lock past "
          "each other")
    scratch = Scratch()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    seen: list = []
    try:
        with saved_runner_executable():
            prepare = Preparer()

            def take(probe, *, namespace=None, repo_root=None):
                seen.append(namespace)
                return held_resources(probe, namespace=namespace)

            run(scratch, prepare_engine=prepare, acquire_resources=take,
                namespace=namespace, measure=Recorder(measurement(scratch)),
                record_result=probe_census.record_result_installed)
            expect(prepare.calls
                   and prepare.calls[0]["namespace"] == namespace,
                   f"preparation is told the measurement's own namespace "
                   f"(got {prepare.calls})")
            expect(seen == [namespace],
                   f"which is the one the hold is taken in (got {seen})")
            expect(deflake._probe_resource_namespace(namespace=namespace)
                   == namespace,
                   "resolved through one helper, so a future edit cannot "
                   "make the two disagree")
    finally:
        scratch.cleanup()


def test_a_preparation_failure_runs_nothing_and_gives_the_claim_back() -> None:
    print("\n-- a preparation that fails releases the claim and measures "
          "nothing")
    scratch = Scratch()
    try:
        with saved_runner_executable():
            claim = FakeClaim()
            measure = Recorder(measurement(scratch))
            held: list = []
            result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                         prepare_engine=Preparer(raises=(
                             probe_engine.EnginePreparationError(
                                 "the engine executable could not be "
                                 "prepared: `cabal build exe:synarchy` "
                                 "failed with exit status 1"))),
                         acquire_resources=lambda *a, **kw: held.append(1),
                         measure=measure)
            expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
                   f"the outcome is a managed error ({result.outcome}: "
                   f"{result.detail})")
            expect("could not be prepared" in result.detail,
                   f"naming preparation and carrying Cabal's reason "
                   f"(got {result.detail!r})")
            expect(held == [], "no resource hold was taken")
            expect(measure.calls == [], "and nothing was measured")
            expect(claim.released, "while the claim went back")
            expect(result.ownership == deflake.OWNERSHIP_NONE,
                   f"so the invocation owns nothing (got {result.ownership})")
    finally:
        scratch.cleanup()


def test_the_real_preparation_runs_outside_the_real_hold() -> None:
    print("\n-- the REAL preparation and the REAL hold, in one namespace, "
          "without deadlocking")
    scratch = Scratch()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    engine = scratch.root / "fake-synarchy"
    engine.write_text("#!/bin/sh\nexit 0\n")
    engine.chmod(0o755)
    calls = scratch.root / "cabal-calls.txt"
    (scratch.root / "cabal.json").write_text(
        json.dumps({"calls": str(calls), "engine": str(engine)}))
    cabal = scratch.root / "cabal"
    cabal.write_text(FAKE_CABAL_SRC)
    cabal.chmod(0o755)
    saved_path = os.environ.get("PATH", "")
    conflicts: list = []
    try:
        os.environ["PATH"] = f"{scratch.root}{os.pathsep}{saved_path}"
        with saved_runner_executable():
            probe_runner_resources.ENGINE_EXECUTABLE = None

            class Watching(Recorder):
                def __call__(self, *args, **kwargs):
                    # Inside the measurement's hold: an exclusive
                    # `cabal-build` interest — which is exactly what a
                    # child probe's own preparation would take — is
                    # refused. That is the deadlock this ordering
                    # avoids, observed rather than assumed.
                    self.seen_executable = probe_runner_resources.ENGINE_EXECUTABLE
                    try:
                        probe_resource_lock.acquire(
                            exclusive={probe_engine.BUILD_RESOURCE},
                            namespace=namespace,
                            purpose="test_deflake would-be child").release()
                    except probe_resource_lock.ResourceBusy as busy:
                        conflicts.append(busy.resource)
                    return super().__call__(*args, **kwargs)

            measure = Watching(measurement(scratch))
            started = time.monotonic()
            result = run(scratch, acquire_resources=deflake._acquire_probe_resources,
                         prepare_engine=deflake._prepare_probe_engine,
                         namespace=namespace, measure=measure,
                         record_result=probe_census.record_result_installed)
            elapsed = time.monotonic() - started
            expect(result.outcome == deflake.OUTCOME_RECORDED,
                   f"the measurement completes ({result.outcome}: "
                   f"{result.detail})")
            expect(elapsed < 60.0,
                   f"promptly, rather than sitting out a preparation "
                   f"allowance (took {elapsed:.1f} s)")
            recorded = [line for line in
                        (calls.read_text().splitlines() if calls.exists()
                         else []) if line]
            expect(recorded == ["build exe:synarchy", "list-bin exe:synarchy"],
                   f"having really built, exactly once, through the real "
                   f"preparation (got {recorded})")
            expect(conflicts == [probe_engine.BUILD_RESOURCE],
                   f"and the hold covering the runs really does exclude a "
                   f"would-be child preparation, so the ordering is what "
                   f"makes this work (got {conflicts})")
            expect(getattr(measure, "seen_executable", None) == str(engine),
                   f"which is why the child needs none: it is handed the "
                   f"prepared path (got "
                   f"{getattr(measure, 'seen_executable', None)!r})")
    finally:
        os.environ["PATH"] = saved_path
        for entry in probe_resource_lock.LOCK_ROOT.glob(
                f"{probe_resource_lock.SHARED_PREFIX}-{namespace}-*"):
            try:
                entry.unlink()
            except OSError:
                pass
        scratch.cleanup()


#: This owner's inventory, in the order the aggregate has always run it.
CASES = (
    test_the_engine_is_prepared_before_the_resource_hold,
    test_preparation_and_the_hold_resolve_one_namespace,
    test_a_preparation_failure_runs_nothing_and_gives_the_claim_back,
    test_the_real_preparation_runs_outside_the_real_hold,
)
