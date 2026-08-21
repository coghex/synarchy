#!/usr/bin/env python3
"""Report which restore outcome each of CI's two caches got (#1358).

`.github/workflows/ci.yml`'s `build-test` job restores two caches -- the
cabal dependency store and the `dist-newstyle` project build products --
each keyed on the dependency plan hash with a broad `restore-keys`
prefix fallback (#790). That gives three possible outcomes per cache:

  * **exact hit** -- the primary key itself was present;
  * **prefix hit** -- the primary key missed and a `restore-keys` prefix
    matched an older snapshot for this GHC/Cabal, so cabal rebuilds
    whatever the plan changed since it;
  * **full miss** -- nothing restored at all, and this run rebuilds every
    dependency from source.

All three exit zero. `actions/cache` does log `Cache restored from key:
...` or `Cache not found for input keys: ...`, but nothing classifies
those into a stable, greppable token, so a systematic regression into
full misses -- a cache backend change, a key-scheme edit, GitHub's 10 GB
per-repository LRU eviction, or branch-scoping denying a PR access to
master's cache -- would multiply every build's cost indefinitely while
turning nothing red. Nobody would notice except as "CI feels slow
lately".

Classification, and why it is not "cache-hit is a boolean"
----------------------------------------------------------
The outcome is read from the restore action's OWN outputs, which are set
from the same `cacheKey` value its log line is printed from, so the
report cannot disagree with what actually happened:

  * `cache-hit == 'true'`                     -> EXACT_HIT
  * `cache-hit == 'false'`, matched key set   -> PREFIX_HIT
  * no matched key                            -> FULL_MISS

`cache-hit` has THREE states, not two. On a full miss `actions/cache`
returns before setting the output at all (its source says so in as many
words: "`cache-hit` is intentionally not set to `false` here to preserve
existing behavior"), so the variable arrives EMPTY -- while a prefix hit
sets it to the string `'false'`. Collapsing every non-`'true'` value into
one outcome would therefore report the ordinary post-dependency-change
prefix hit as a cold cache, which is the exact false alarm this report
exists not to raise. The matched key is what separates them, and it is
taken from the action rather than reconstructed: a guessed key would be
the report drifting from the run.

One residual state is neither: `cache-hit == 'false'` with NO primary key
and NO matched key, which is what the action publishes when the runner's
cache service is unavailable. Nothing was restored, so it is reported as
a FULL_MISS -- but the human line says which of the two it was rather
than inventing a fourth token.

Prominence, and the promise not to cry wolf
-------------------------------------------
A full miss also emits a non-failing GitHub annotation: `warning` on a
master push, where the previous master build populated the cache and a
miss is close to always wrong, and `notice` otherwise, because a pull
request legitimately misses on branch cache scoping or a brand-new key
family. Exact and prefix hits are logged and NOT annotated.

This script never fails the build. A first build after a genuine
dependency change legitimately misses, and a gate that cries wolf gets
ignored, so the plain run exits 0 for every outcome. The only failing
mode is `--self-test`, which checks this module's own classification and
-- because a silently mis-wired reporter is indistinguishable from a
healthy cache -- the workflow wiring it depends on.

The wiring check
----------------
`--self-test` re-reads `.github/workflows/ci.yml` and fails if:

  * either cache is no longer restored by `actions/cache/restore` under
    the step id this module names. This is the load-bearing one: the
    COMBINED `actions/cache` action keeps the matched key in step state
    for its own post step and publishes only `cache-hit`, so reverting
    the split would leave `cache-matched-key` permanently empty and
    every prefix hit would be reported as a full miss -- a false alarm
    on every run after a dependency change, with nothing failing.
  * a restore step's cached `path` stops being the one this module names
    in its human output.
  * the reporting step's `env:` block stops binding any variable read
    below to that step's output, or binds it to the wrong step.

Usage:
  python3 tools/ci_cache_report.py              # read the environment, report
  python3 tools/ci_cache_report.py --self-test  # check the logic and the wiring
Exit codes: 0 always for a report (a cache miss is news, not a failure);
1 from --self-test when a check fails.
"""

from __future__ import annotations

import argparse
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Mapping

try:
    import yaml  # type: ignore
except ImportError:  # pragma: no cover - exercised only on a bare toolchain
    raise SystemExit(
        "ci_cache_report.py needs PyYAML to read .github/workflows/ci.yml.\n"
        "Install the pinned toolchain:\n"
        "    python3 -m pip install --user -r tools/requirements-assets.txt\n"
        "(PyYAML is already required by tools/pack_atlas.py and "
        "tools/ci_parity_audit.py, which `make ci` and CI both run, so this "
        "adds no new dependency.)")

REPO_ROOT = Path(__file__).resolve().parent.parent
WORKFLOW_PATH = REPO_ROOT / ".github" / "workflows" / "ci.yml"

#: The one workflow job whose caches this reports on.
AUDITED_JOB = "build-test"
WORKFLOW_LABEL = ".github/workflows/ci.yml (job: %s)" % AUDITED_JOB

#: This script's path as it appears in the workflow's `run:` block, used
#: to find the reporting step whose `env:` wiring is checked.
SCRIPT_NAME = "tools/ci_cache_report.py"

#: Only `actions/cache/restore` publishes cache-primary-key and
#: cache-matched-key as step outputs; the combined action does not.
RESTORE_ACTION_PREFIX = "actions/cache/restore@"

#: Fixed prefix every machine-readable line carries, so a log search for
#: one string finds every cache's outcome.
RECORD_PREFIX = "CI_CACHE_REPORT"

EXACT_HIT = "EXACT_HIT"
PREFIX_HIT = "PREFIX_HIT"
FULL_MISS = "FULL_MISS"

#: The docs-only fast path (#1490) skips both cache steps, which leaves
#: their outputs empty -- indistinguishable from a full miss unless the
#: reporter is told. It is told, rather than being guarded by an `if:`,
#: so the case is covered by --self-test instead of by an untested
#: workflow expression.
DOCS_ONLY_ENV = "CI_CACHE_DOCS_ONLY"
DOCS_FAST_PATH_STEP_ID = "docs-fast-path"

#: `push` means a master push here: ci.yml's push trigger is
#: `branches: [master]`.
EVENT_ENV = "CI_CACHE_EVENT_NAME"

#: (environment-variable suffix, restore-action output name).
OUTPUT_SUFFIXES: tuple[tuple[str, str], ...] = (
    ("HIT", "cache-hit"),
    ("PRIMARY_KEY", "cache-primary-key"),
    ("MATCHED_KEY", "cache-matched-key"),
)


@dataclass(frozen=True)
class CacheSpec:
    """One cache this report classifies."""

    key: str          #: stable token in the machine-readable record
    label: str        #: human description
    step_id: str      #: id of its `actions/cache/restore` step in ci.yml
    path: str         #: the `with: path:` that step caches
    env_prefix: str   #: prefix of the environment variables carrying it


CACHES: tuple[CacheSpec, ...] = (
    CacheSpec(
        key="deps-store",
        label="cabal dependency store",
        step_id="cache",
        path="/usr/local/cabal/store",
        env_prefix="CI_CACHE_DEPS",
    ),
    CacheSpec(
        key="dist-newstyle",
        label="project build products",
        step_id="dist-cache",
        path="dist-newstyle",
        env_prefix="CI_CACHE_DIST",
    ),
)


def expected_env_bindings() -> tuple[tuple[str, str], ...]:
    """Every environment variable the report reads and its ci.yml source."""
    bindings: list[tuple[str, str]] = [
        (DOCS_ONLY_ENV,
         "${{ steps.%s.outputs.docs_only }}" % DOCS_FAST_PATH_STEP_ID),
        (EVENT_ENV, "${{ github.event_name }}"),
    ]
    for spec in CACHES:
        for suffix, output in OUTPUT_SUFFIXES:
            bindings.append((
                f"{spec.env_prefix}_{suffix}",
                "${{ steps.%s.outputs.%s }}" % (spec.step_id, output),
            ))
    return tuple(bindings)


def classify(cache_hit: str, matched_key: str) -> str:
    """Classify one cache from the restore action's own two outputs.

    See the module docstring: `cache_hit` is tri-state, and the matched
    key is what separates an ordinary prefix hit from a cold cache.
    """
    if cache_hit.strip() == "true":
        return EXACT_HIT
    if matched_key.strip():
        return PREFIX_HIT
    return FULL_MISS


def _escape_annotation(text: str) -> str:
    """Escape the three characters a workflow-command message may not carry."""
    return (text.replace("%", "%25")
                .replace("\r", "%0D")
                .replace("\n", "%0A"))


def describe(spec: CacheSpec, outcome: str, primary: str, matched: str) -> str:
    """The human line that accompanies one cache's record."""
    where = f"{spec.label} ({spec.path})"
    if outcome == EXACT_HIT:
        return f"{where}: exact hit -- restored the primary key {primary or matched}."
    if outcome == PREFIX_HIT:
        return (f"{where}: prefix hit -- restored {matched}; the exact key "
                f"{primary} was not present, so this build rebuilds whatever "
                "the plan changed since that snapshot.")
    if primary:
        return (f"{where}: FULL MISS -- neither the exact key {primary} nor "
                "its restore-keys prefix matched anything, so this build pays "
                "for a cold cache.")
    return (f"{where}: FULL MISS -- the restore step published no primary key "
            "at all, so either it did not run or the runner's cache service "
            "was unavailable; nothing was restored.")


def annotate(spec: CacheSpec, primary: str, event: str) -> str:
    """The non-failing GitHub annotation a full miss emits."""
    if event.strip() == "push":
        severity = "warning"
        detail = ("a master push should restore the cache master's own "
                  "previous build populated, so a full miss here is close to "
                  "always wrong: check the key scheme, the repository cache "
                  "quota (LRU eviction) and the cache backend")
    else:
        severity = "notice"
        detail = ("on a pull request this can be legitimate -- branch cache "
                  "scoping, a brand-new key family, or LRU eviction -- but a "
                  "run of them is not")
    message = (f"The {spec.label} cache ({spec.path}) restored nothing for "
               f"{primary or '(no primary key)'}; {detail}. Reporting only "
               "(#1358): the build is not failed.")
    return f"::{severity} title=Cold CI cache::{_escape_annotation(message)}"


def build_report(env: Mapping[str, str]) -> list[str]:
    """Every line the report prints, given the runner's environment."""
    if env.get(DOCS_ONLY_ENV, "").strip() == "true":
        return [
            f"{RECORD_PREFIX} skipped=docs-only-fast-path",
            "Both cache steps were skipped by the docs-only fast path "
            "(#1490), so there is no restore outcome to classify.",
        ]

    event = env.get(EVENT_ENV, "")
    lines: list[str] = []
    for spec in CACHES:
        cache_hit = env.get(f"{spec.env_prefix}_HIT", "")
        primary = env.get(f"{spec.env_prefix}_PRIMARY_KEY", "").strip()
        matched = env.get(f"{spec.env_prefix}_MATCHED_KEY", "").strip()
        outcome = classify(cache_hit, matched)
        lines.append(
            f"{RECORD_PREFIX} cache={spec.key} outcome={outcome} "
            f"primary_key={primary} matched_key={matched}")
        lines.append(describe(spec, outcome, primary, matched))
        if outcome == FULL_MISS:
            lines.append(annotate(spec, primary, event))
    return lines


def _normalize_expression(text: str) -> str:
    """Compare workflow expressions without depending on inner spacing."""
    return "".join(str(text).split())


def check_wiring(document: object) -> list[str]:
    """Check ci.yml still wires this report to the outputs it classifies.

    Returns a list of problems; empty means the wiring is intact.
    """
    problems: list[str] = []

    jobs = document.get("jobs") if isinstance(document, dict) else None
    if not isinstance(jobs, dict) or not isinstance(jobs.get(AUDITED_JOB), dict):
        return [f"{WORKFLOW_LABEL}: no `{AUDITED_JOB}` job to inspect."]
    steps = jobs[AUDITED_JOB].get("steps")
    if not isinstance(steps, list) or not steps:
        return [f"{WORKFLOW_LABEL}: the job declares no steps."]

    by_id: dict[str, list[dict]] = {}
    for step in steps:
        if isinstance(step, dict) and isinstance(step.get("id"), str):
            by_id.setdefault(step["id"], []).append(step)

    for spec in CACHES:
        found = by_id.get(spec.step_id, [])
        if len(found) != 1:
            problems.append(
                f"{WORKFLOW_LABEL}: expected exactly one step with `id: "
                f"{spec.step_id}` restoring the {spec.label} cache, found "
                f"{len(found)}.")
            continue
        step = found[0]
        uses = str(step.get("uses", "")).strip()
        if not uses.startswith(RESTORE_ACTION_PREFIX):
            problems.append(
                f"{WORKFLOW_LABEL}: step `{spec.step_id}` must use "
                f"`{RESTORE_ACTION_PREFIX}...`, not `{uses or '(nothing)'}`. "
                "The combined `actions/cache` action keeps the matched key in "
                "step state and publishes only cache-hit, so every prefix hit "
                "would be reported as a full miss.")
        with_block = step.get("with")
        path = str((with_block or {}).get("path", "")).strip() \
            if isinstance(with_block, dict) else ""
        if path != spec.path:
            problems.append(
                f"{WORKFLOW_LABEL}: step `{spec.step_id}` caches `{path}`, but "
                f"this report describes it as `{spec.path}`.")

    report_steps = [
        step for step in steps
        if isinstance(step, dict) and isinstance(step.get("run"), str)
        and SCRIPT_NAME in step["run"]
    ]
    if len(report_steps) != 1:
        problems.append(
            f"{WORKFLOW_LABEL}: expected exactly one step running "
            f"`{SCRIPT_NAME}`, found {len(report_steps)}.")
        return problems

    env_block = report_steps[0].get("env")
    if not isinstance(env_block, dict):
        problems.append(
            f"{WORKFLOW_LABEL}: the step running `{SCRIPT_NAME}` declares no "
            "`env:` block, so the report would classify an empty environment "
            "as a full miss on both caches.")
        return problems

    for name, expected in expected_env_bindings():
        if name not in env_block:
            problems.append(
                f"{WORKFLOW_LABEL}: the step running `{SCRIPT_NAME}` does not "
                f"set `{name}`, which the report reads; expected "
                f"`{expected}`.")
        elif _normalize_expression(env_block[name]) != _normalize_expression(expected):
            problems.append(
                f"{WORKFLOW_LABEL}: `{name}` is bound to "
                f"`{env_block[name]}`, expected `{expected}`.")

    return problems


def _valid_wiring_document() -> dict:
    """A minimal workflow document `check_wiring` must accept."""
    steps: list[dict] = [
        {"name": "Select docs-only fast path",
         "id": DOCS_FAST_PATH_STEP_ID,
         "run": "echo docs_only=false >> \"$GITHUB_OUTPUT\""},
    ]
    for spec in CACHES:
        steps.append({
            "name": f"Restore {spec.label}",
            "id": spec.step_id,
            "uses": f"{RESTORE_ACTION_PREFIX}deadbeef",
            "with": {"path": spec.path, "key": "some-key"},
        })
    steps.append({
        "name": "Report cache restore outcomes",
        "env": dict(expected_env_bindings()),
        "run": f"python3 {SCRIPT_NAME} --self-test\npython3 {SCRIPT_NAME}\n",
    })
    return {"jobs": {AUDITED_JOB: {"steps": steps}}}


def _self_test() -> int:
    failures: list[str] = []

    def check(condition: bool, message: str) -> None:
        if not condition:
            failures.append(message)

    deps, dist = CACHES

    # 1. The three outcomes, read exactly as the restore action publishes
    #    them.
    check(classify("true", "deps-v2-Linux-ghc9.12.2-abc") == EXACT_HIT,
          "cache-hit 'true' must classify as an exact hit")
    check(classify("false", "deps-v2-Linux-ghc9.12.2-") == PREFIX_HIT,
          "cache-hit 'false' with a matched key must classify as a prefix hit")
    check(classify("", "") == FULL_MISS,
          "an empty cache-hit with no matched key must classify as a full miss")

    # 2. The two states that must NOT be collapsed into one another.
    check(classify("false", "deps-v2-Linux-ghc9.12.2-") != FULL_MISS,
          "a prefix hit must not be reported as a cold cache")
    check(classify("false", "") == FULL_MISS,
          "cache-hit 'false' with no matched key (the cache service being "
          "unavailable) restored nothing and must classify as a full miss")

    # 3. Surrounding whitespace, which shell interpolation can introduce,
    #    changes nothing.
    check(classify(" true \n", "") == EXACT_HIT,
          "a padded cache-hit must still classify as an exact hit")
    check(classify("false", "  key  ") == PREFIX_HIT,
          "a padded matched key must still classify as a prefix hit")

    # 4. A prefix hit names the key the action actually restored.
    env = {
        EVENT_ENV: "pull_request",
        f"{deps.env_prefix}_HIT": "false",
        f"{deps.env_prefix}_PRIMARY_KEY": "deps-v2-Linux-ghc9.12.2-new",
        f"{deps.env_prefix}_MATCHED_KEY": "deps-v2-Linux-ghc9.12.2-old",
        f"{dist.env_prefix}_HIT": "true",
        f"{dist.env_prefix}_PRIMARY_KEY": "dist-v2-Linux-x",
        f"{dist.env_prefix}_MATCHED_KEY": "dist-v2-Linux-x",
    }
    lines = build_report(env)
    check(any(line == f"{RECORD_PREFIX} cache={deps.key} outcome={PREFIX_HIT} "
                      "primary_key=deps-v2-Linux-ghc9.12.2-new "
                      "matched_key=deps-v2-Linux-ghc9.12.2-old"
              for line in lines),
          f"a prefix hit must emit a record naming both keys, got {lines!r}")
    check(any(line == f"{RECORD_PREFIX} cache={dist.key} outcome={EXACT_HIT} "
                      "primary_key=dist-v2-Linux-x matched_key=dist-v2-Linux-x"
              for line in lines),
          f"an exact hit must emit its own record, got {lines!r}")
    check(not any(line.startswith("::") for line in lines),
          "hits must not be annotated -- a report that cries wolf gets ignored")

    # 5. A full miss is annotated, at master-push prominence on a push and
    #    at notice prominence elsewhere, and never fails.
    miss = {
        f"{deps.env_prefix}_HIT": "",
        f"{deps.env_prefix}_PRIMARY_KEY": "deps-v2-Linux-ghc9.12.2-abc",
        f"{deps.env_prefix}_MATCHED_KEY": "",
        f"{dist.env_prefix}_HIT": "",
        f"{dist.env_prefix}_PRIMARY_KEY": "dist-v2-Linux-abc",
        f"{dist.env_prefix}_MATCHED_KEY": "",
    }
    pushed = build_report({**miss, EVENT_ENV: "push"})
    check(sum(1 for line in pushed if line.startswith("::warning ")) == 2,
          f"a full miss on a master push must warn for each cache, got {pushed!r}")
    check(not any(line.startswith("::notice") for line in pushed),
          "a master push must not downgrade a full miss to a notice")
    requested = build_report({**miss, EVENT_ENV: "pull_request"})
    check(sum(1 for line in requested if line.startswith("::notice ")) == 2,
          f"a full miss on a PR must emit a notice per cache, got {requested!r}")
    check(not any(line.startswith("::warning") for line in requested),
          "a pull-request full miss must not use master-push prominence")
    check(all(f"{RECORD_PREFIX} cache={spec.key} outcome={FULL_MISS}" in
              "\n".join(pushed) for spec in CACHES),
          f"each cache needs its own greppable FULL_MISS record, got {pushed!r}")

    # 6. The cache service being unavailable publishes no primary key; the
    #    outcome token stays one of the three and the human line says which
    #    condition it was.
    unavailable = build_report({
        EVENT_ENV: "push",
        f"{deps.env_prefix}_HIT": "false",
        f"{deps.env_prefix}_PRIMARY_KEY": "",
        f"{deps.env_prefix}_MATCHED_KEY": "",
        f"{dist.env_prefix}_HIT": "false",
        f"{dist.env_prefix}_PRIMARY_KEY": "",
        f"{dist.env_prefix}_MATCHED_KEY": "",
    })
    check(any("published no primary key" in line for line in unavailable),
          f"an absent primary key must be named, got {unavailable!r}")
    check(all(outcome not in "\n".join(unavailable)
              for outcome in (EXACT_HIT, PREFIX_HIT)),
          f"an absent primary key restored nothing, got {unavailable!r}")

    # 7. The docs-only fast path skips both cache steps; their empty
    #    outputs must not be read as a cold cache.
    skipped = build_report({DOCS_ONLY_ENV: "true", EVENT_ENV: "push"})
    check(skipped[0] == f"{RECORD_PREFIX} skipped=docs-only-fast-path",
          f"a docs-only push must report the skip, got {skipped!r}")
    check(not any(line.startswith("::") for line in skipped),
          "a docs-only push must not annotate a full miss it did not have")
    check(FULL_MISS not in "\n".join(skipped),
          "a docs-only push must not emit an outcome token at all")

    # 8. Annotation messages escape the characters a workflow command
    #    reserves, so a key carrying one cannot truncate the annotation.
    escaped = annotate(deps, "key-100%\nsecond line", "push")
    check("\n" not in escaped and "%0A" in escaped and "%25" in escaped,
          f"annotation messages must be escaped, got {escaped!r}")

    # 9. The wiring the classification depends on. A valid document passes;
    #    each way of breaking it is reported.
    check(check_wiring(_valid_wiring_document()) == [],
          "the reference wiring document must pass: "
          f"{check_wiring(_valid_wiring_document())!r}")

    def mutate(edit, expect: str) -> None:
        document = _valid_wiring_document()
        edit(document["jobs"][AUDITED_JOB]["steps"])
        problems = check_wiring(document)
        check(any(expect in problem for problem in problems),
              f"a wiring mutation should be reported with {expect!r}, got "
              f"{problems!r}")

    def find(steps: list[dict], step_id: str) -> dict:
        return next(step for step in steps if step.get("id") == step_id)

    def report_step(steps: list[dict]) -> dict:
        return next(step for step in steps
                    if SCRIPT_NAME in str(step.get("run", "")))

    mutate(lambda steps: find(steps, dist.step_id).__setitem__(
               "uses", "actions/cache@deadbeef"),
           "combined `actions/cache` action")
    mutate(lambda steps: find(steps, deps.step_id).__setitem__("id", "renamed"),
           f"`id: {deps.step_id}`")
    mutate(lambda steps: find(steps, dist.step_id)["with"].__setitem__(
               "path", "somewhere-else"),
           "this report describes it as")
    mutate(lambda steps: steps.append(dict(find(steps, deps.step_id))),
           f"`id: {deps.step_id}`")
    mutate(lambda steps: steps.remove(report_step(steps)),
           f"exactly one step running `{SCRIPT_NAME}`")
    mutate(lambda steps: steps.append(dict(report_step(steps))),
           f"exactly one step running `{SCRIPT_NAME}`")
    mutate(lambda steps: report_step(steps).pop("env"),
           "declares no `env:` block")
    mutate(lambda steps: report_step(steps)["env"].pop(
               f"{dist.env_prefix}_MATCHED_KEY"),
           f"does not set `{dist.env_prefix}_MATCHED_KEY`")
    mutate(lambda steps: report_step(steps)["env"].__setitem__(
               f"{dist.env_prefix}_HIT",
               "${{ steps.%s.outputs.cache-hit }}" % deps.step_id),
           f"`{dist.env_prefix}_HIT` is bound to")
    mutate(lambda steps: report_step(steps)["env"].__setitem__(
               EVENT_ENV, "${{ github.ref }}"),
           f"`{EVENT_ENV}` is bound to")
    mutate(lambda steps: report_step(steps)["env"].__setitem__(
               DOCS_ONLY_ENV, "${{ steps.other.outputs.docs_only }}"),
           f"`{DOCS_ONLY_ENV}` is bound to")

    check(check_wiring({"jobs": {}}) != [],
          "a workflow with no build-test job must be reported")
    check(check_wiring({"jobs": {AUDITED_JOB: {"steps": []}}}) != [],
          "a build-test job with no steps must be reported")

    # 10. Inner spacing in a workflow expression is not drift.
    spaced = _valid_wiring_document()
    spaced["jobs"][AUDITED_JOB]["steps"][-1]["env"][EVENT_ENV] = \
        "${{   github.event_name   }}"
    check(check_wiring(spaced) == [],
          "expression spacing must not be reported as drift")

    # 11. The real workflow, which is what this all exists to keep honest.
    try:
        live = yaml.safe_load(WORKFLOW_PATH.read_text(encoding="utf-8"))
    except (OSError, yaml.YAMLError) as error:
        failures.append(f"could not read {WORKFLOW_PATH}: {error}")
    else:
        for problem in check_wiring(live):
            failures.append(problem)

    if failures:
        print("ci_cache_report self-test: FAILED")
        for failure in failures:
            print(f"  - {failure}")
        return 1
    print("ci_cache_report self-test: all cases pass")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Report which restore outcome each of CI's two caches "
                    "got (#1358). Reads the restore steps' own outputs from "
                    "the environment and prints one greppable record per "
                    "cache; a full miss also emits a non-failing annotation.")
    parser.add_argument(
        "--self-test", action="store_true",
        help="check the classification and the ci.yml wiring it depends on")
    args = parser.parse_args()

    if args.self_test:
        return _self_test()

    for line in build_report(os.environ):
        print(line)
    # Reporting only: a cold cache is news, never a build failure (#1358).
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
