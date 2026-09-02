#!/usr/bin/env bash
#
# Local mirror of the CI gate (#527). `make ci` runs this; it executes the
# same checks .github/workflows/ci.yml's test-and-audits worker runs, in the same
# order, so a green run here predicts a green run in CI ("green locally =>
# green in CI").
#
# That parity is enforced, not merely intended (#1355): the last step runs
# tools/ci_parity_audit.py, which compares this file's `python3 tools/*.py`
# invocations against that job's, at command-and-arguments granularity and
# in BOTH directions, and fails on any difference outside its hard-coded,
# reason-carrying exemption list. Adding a check to one file without the
# other now fails immediately instead of surfacing after a push. Two
# things the audit deliberately does not compare: conditional control flow
# (CI path-selects the graphical build, the unit-asset gate, world_check
# and -- since #1364 -- the hspec step's SYNARCHY_FULL_TESTS=1 full tier
# on PRs; this file runs all four unconditionally, which is what makes it
# conservative), and the non-Python steps around them.
#
# -Werror is part of synarchy.cabal's checked-in warning policy now
# (#1057), so every build of the `synarchy` package -- here, in CI, or a
# plain `cabal build` -- already compiles under it. That closes the hole
# this comment used to describe: a module compiled warm *without*
# -Werror (because the flag was only ever injected here, at gate-run
# time, via a scoped cabal.project.local) was never re-checked just
# because -Werror got added afterwards, since GHC's recompilation
# avoidance doesn't treat warning flags as affecting object code
# (confirmed by hand: this let an unused-field warning ship past
# `make ci` and fail CI, issue #869).
#
# Still scoped here via a temporary cabal.project.local: -fforce-recomp,
# needed for the remaining gap -- cross-module interactions in files that
# don't get recompiled, the same one .github/workflows/ci.yml's
# no-clean-backstop trade-off note accepts for CI's cache. It forces
# every module of the `synarchy` package to be genuinely rechecked every
# run, trading warm-build reuse for a result you can actually trust;
# already-built dependencies are unaffected and stay cached, so this is
# not as costly as a full clean build. The exe it builds is the one
# world_check drives.
#
# Any pre-existing cabal.project.local is backed up and restored on exit,
# so your dev config is left untouched whether the gate passes or fails.
set -euo pipefail

# Absolute path to THIS script, resolved before the cd below so the step
# counter can read it back regardless of the caller's CWD.
SELF="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"

# Run from the repo root regardless of caller CWD.
cd "$(dirname "$0")/.."

# Step labels are GENERATED, never hand-typed. Each `[N/M]` used to be a
# literal, so inserting a gate meant editing N on every later step and M
# on all of them: a three-line addition rewrote every label in the file,
# and two gate-adding PRs open at once conflicted by construction even
# when their changes were completely independent. That is not a
# hypothetical -- #1724, #1836 and #1704 each added one gate in the same
# week, and every pairing of them collided, costing a merge and a forced
# re-review apiece. The numbers carried no information git could merge.
#
# `step` announces and advances; `substep` re-announces the CURRENT step
# for a gate that reports more than once (the save-compat selection
# below). M is counted from this file's own `step ` call sites, so
# adding a gate is now a genuine three-line insertion.
STEP_TOTAL="$(grep -c '^step ' "$SELF")"
STEP_N=0

if [ "$STEP_TOTAL" -lt 1 ]; then
  echo "ci-local.sh: could not count its own steps in $SELF" >&2
  exit 1
fi

step() {
  STEP_N=$((STEP_N + 1))
  echo "==> [$STEP_N/$STEP_TOTAL] $1"
}

substep() {
  echo "==> [$STEP_N/$STEP_TOTAL] $1"
}

LOCAL=cabal.project.local
BACKUP=

restore() {
  if [ -n "$BACKUP" ]; then
    mv -f "$BACKUP" "$LOCAL"
  else
    rm -f "$LOCAL"
  fi
}
trap restore EXIT

if [ -e "$LOCAL" ]; then
  BACKUP="$(mktemp)"
  cp "$LOCAL" "$BACKUP"
fi

# Resolve THIS working tree's own changed paths before the scratch
# cabal.project.local below exists (#1360). The order is load-bearing,
# not incidental: cabal.project.local is not gitignored, so a change can
# legitimately track one -- and CI's save-compat gate selects on it,
# because cabal would apply it there. Capturing after the write would
# report this gate's own scratch edit to a tracked file as if it were
# the candidate's, which is exactly what requirement 7 forbids;
# capturing before reports the candidate's real edit and nothing else.
# The list feeds the save-compat decision at step 13.
SAVE_COMPAT_PATHS="$(python3 tools/ci_expensive_gates.py --local-changed-paths)"

# -fforce-recomp so a warm build can't mask a warning a fresh build would
# catch; see the header comment above for why -Werror itself no longer
# needs to be injected here.
printf 'package synarchy\n  ghc-options: -fforce-recomp\n' > "$LOCAL"

step "build (library + executable, -Werror)"
cabal build all

step "build test suites"
cabal build synarchy-test-headless
cabal build synarchy-test-graphical

step "headless hspec suite (full tier)"
# SYNARCHY_FULL_TESTS=1 turns the full-tier examples from pending into
# real runs (#1364) -- today exactly one, the w128 seed-42 volcano
# exposure regression in test-headless/Test/Headless/WorldGen/Exposure.hs.
# CI applies it only when its worldgen selector fires; this file runs
# every gate unconditionally (see the header note), so it applies it
# always and stays the conservative side of that parity. It must be set
# on the `cabal test` process itself, and it must be `1` rather than ''
# -- the test's guard treats ANY present value, empty string included, as
# enabled.
SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless --test-show-details=direct

step "test audit"
python3 tools/test_audit.py

# The executable specification of what tools/world_determinism.py means
# by "content-identical" (#1724): a reversed tile array and a
# reordered-key tile must hash equal, while a changed field, a missing
# tile and an unstable canonical form must not. Issue #23 / PR #34 chose
# content identity over byte identity deliberately, and this is the only
# place that choice is asserted. world_check.py hashing six real seeds
# against their baselines (#1361) does not cover it: the engine emits
# tiles in a stable order, so a regression that made the checker
# order-SENSITIVE would still produce matching hashes and pass. Pure
# Python, no engine, no GPU, no network, sub-second -- and unconditional
# on both sides rather than behind the worldgen selector, because the
# contract lives in tools/ and can be broken by a change that selector
# would not fire on.
step "world determinism content-identity self-test"
python3 tools/test_determinism.py

step "lua module line budget"
python3 tools/lua_module_budget.py

step "lua duplicate function audit"
python3 tools/test_lua_duplicate_function_audit.py
python3 tools/lua_duplicate_function_audit.py

step "lua registration audit"
python3 tools/test_lua_registration_audit.py
python3 tools/lua_registration_audit.py

step "haskell module line budget"
python3 tools/test_haskell_module_budget.py
python3 tools/haskell_module_budget.py

step "unicode operator audit"
python3 tools/test_unicode_operator_audit.py
python3 tools/unicode_operator_audit.py

step "lua strict-decode audit"
python3 tools/lua_strict_decode_audit.py --self-test
python3 tools/lua_strict_decode_audit.py

step "persistence inventory audit"
python3 tools/test_persistence_inventory_audit.py
python3 tools/persistence_inventory_audit.py

step "EngineEnv capability inventory audit"
python3 tools/test_engine_env_capability_audit.py
python3 tools/engine_env_capability_audit.py

# Local `make ci` keeps both commands unconditional. CI runs them for
# every non-docs-only change and every save-compat input change, but skips
# this Cabal-backed step after its docs-only selector proves the range is
# unrelated documentation.
# --without-reproducibility (#1360) drops exactly ONE member of the
# self-test module -- the one that spawns its own `cabal repl` to build
# two timestamp variants -- and the block below runs that member when
# this working tree's own changes touch a path that can move its result.
# Every other member and the whole real audit still run on every local
# invocation.
step "save compatibility audit"
python3 tools/test_save_compat_audit.py --without-reproducibility
python3 tools/save_compat_audit.py

# The reproducibility member, selected by the SAME decision CI takes
# (#1360, requirement 7): $SAVE_COMPAT_PATHS was resolved at the top of
# this script, against the merge base with the default branch and before
# the scratch cabal.project.local existed, and is piped here into the
# very same `ci_expensive_gates.py --stdin --gate save-compat` command
# .github/workflows/ci.yml runs. Not a second matcher that could drift:
# one script, one pattern table, one answer. An unresolvable base yields
# the selector's conservative sentinel, which selects the gate, so a
# detached or shallow checkout runs the coverage rather than skipping it.
# tools/ci_parity_audit.py fails if the two files stop agreeing about
# which gate decides this or which command it guards.
# >>> save-compat reproducibility selection >>>
# tools/ci_parity_audit.py EXTRACTS the lines between these two markers
# and EXECUTES them against synthetic changed-path lists (supplied as
# $SAVE_COMPAT_PATHS), so this block's real behaviour -- not a grep for
# its text -- is what proves `make ci` selects the reproducibility
# member from the same decision CI's `if:` takes. Keep the markers, and
# keep the block reading $SAVE_COMPAT_PATHS rather than re-deriving it:
# re-deriving it here would put the resolution back after the scratch
# write.
#
# That extraction runs this block through `bash -c` with nothing but the
# text between the markers, so the `substep` helper defined at the top of
# this file does not exist there. Announcing through a no-op fallback
# keeps the isolated run working (and keeps what the audit measures --
# which command the selection guards -- exactly the same) while the real
# `make ci` run still prints the numbered sub-report.
command -v substep >/dev/null 2>&1 || substep() { :; }
SAVE_COMPAT_REPRO="$(printf '%s\n' "$SAVE_COMPAT_PATHS" | python3 tools/ci_expensive_gates.py --stdin --gate save-compat)"
if [ "$SAVE_COMPAT_REPRO" = true ]; then
  substep "save compatibility fixture reproducibility (selected)"
  python3 tools/test_save_compat_audit.py --only-reproducibility
else
  substep "save compatibility fixture reproducibility: skipped (no save-format, fixture, save-tooling or Cabal path changed)"
fi
# <<< save-compat reproducibility selection <<<

step "enum append-only audit"
python3 tools/enum_append_only_audit.py --self-test
python3 tools/enum_append_only_audit.py

step "cabal library module inventory audit"
python3 tools/test_cabal_module_audit.py
python3 tools/cabal_module_audit.py

step "material id/name correspondence audit"
python3 tools/material_id_audit.py --self-test
python3 tools/material_id_audit.py

# Cheap, no-engine guard (issue #1740): fails if an authoritative
# bare-name icon reference does not resolve through the runtime's GLOBAL
# icon index. Unit-info panel icons are referenced by bare basename, and
# scripts/unit_info_v2_panel_engine.lua consults a row's "<kind>_unknown"
# placeholder only when the basename misses that index -- so a deleted or
# misspelled basename renders a placeholder instead of erroring, which is
# indistinguishable from art that has not landed yet. Nothing verified
# those references, and the tracked tree really had drifted
# (knowledge_basic_cuisine was mapped but absent). It also pins the two
# runtime icon-family inventories to each other and to the per-family
# fallback assets. Unconditional rather than path-selective: it reads a
# handful of scripts and directory listings and costs milliseconds, and
# either the Lua maps or the assets can drift alone.
step "bare-name icon asset check"
python3 tools/bare_name_icon_asset_check.py --self-test
python3 tools/bare_name_icon_asset_check.py

# Cheap, no-engine guard (issue #1717): fails if a concept id
# data/language/concepts.yaml has ever shipped is missing from it, or if
# a new one arrived without being ratcheted into
# data/language/concept_id_baseline.json. A ConceptId is persisted
# identity -- Language.Etymology reports one the catalogue no longer
# carries as EtyInvalidConcept, losing that etymology in every existing
# save -- and Language.Generated.Hash seeds each concept's native root
# from the id string, so a rename re-roots the concept. Nothing else
# names an individual id: the headless suite's catalogue checks are all
# aggregates (version, count, domains, balance, forms), and the naming
# goldens cover only the handful of concepts their samples use.
# Unconditional rather than path-selective: it is a two-file comparison
# costing milliseconds, and either side can drift alone. Since #1868 the
# artifact also records each id's append-only ORDINAL -- the order
# Language.Generated.Root places concepts in -- and the audit fails an
# artifact whose ordinals are no longer that append-only sequence.
step "concept id inventory audit"
python3 tools/concept_id_inventory_audit.py --self-test
python3 tools/concept_id_inventory_audit.py

step "findings report status audit"
python3 tools/test_findings_report_audit.py
python3 tools/findings_report_audit.py

# Cheap, no-engine guard (issue #1704): fails if an F4 Tier 1 (Layer A)
# input area's mapping is stranded -- a producer renamed or moved out
# from under the checker -- or if its instrumentation was deleted
# outright. The plain coverage report cannot tell those two apart and
# exits 0 for both, which is exactly how #787's input-thread split left
# five fully instrumented Layer A areas reporting as gaps for months
# with nothing failing. Unconditional rather than path-selective: the
# stranding is caused by the very rename that moves the file a
# path filter would have keyed on, and the whole check is a handful of
# regex searches costing milliseconds. Tier 2/3 gaps stay deliberate
# fast-follows (#646) -- this gate ignores them, and the plain report
# keeps exit status 0.
step "F4 action-outcome Tier 1 coverage mapping gate"
python3 tools/action_outcome_coverage.py --self-test
python3 tools/action_outcome_coverage.py --verify-tier1

# One command, three checks: the #1257 inventory, #1258's freshness
# comparison against a fresh regeneration, and #1262's image/slot and
# resident-memory budgets. --strict is what makes a budget breach fail
# rather than merely print.
step "unit asset inventory, freshness and budget"
python3 tools/test_pack_atlas.py
python3 tools/pack_atlas.py --validate-only --strict

# The #428 guard that had never been wired to anything (#1705): every
# `assets/textures/...` reference in data/, scripts/, src/, app/ and
# config/ must resolve on disk, because a missed reference after a
# texture move renders MAGENTA in-engine rather than erroring. It runs
# in a fraction of a second, so it is unconditional on both sides rather
# than path-selective. The self-test runs first, and covers the lexical
# comment-vs-code pass the checker needs to tell a Haddock
# counterexample from a runtime path -- without it a green run below
# could be a checker that had quietly stopped scanning.
step "texture path existence check"
python3 tools/test_check_texture_paths.py
python3 tools/check_texture_paths.py

step "world_check --quick"
python3 tools/world_check.py --quick

# Validate the probe-runner harness itself (cheap, no engine, no GPU) --
# the same checks, in the same order, as ci.yml's "probe runner
# self-tests" step. ci_probes/ci_expensive_gates cover the path->probe
# and path->gate mappings, which would otherwise only surface after a
# push as a PR mis-selecting its own gates; test_run_probes covers
# run_probes.py's process-group teardown and --exact key rejection;
# test_persistence_contract_sweep covers the cross-probe registry-drift
# guard; test_action_outcome_probe covers action_outcome_probe.py's
# fixture classification against a fake console, so its branches are
# checked without that probe's own ~8-minute real engine, and
# (#1793) find_mixed_box's anchor and 5x5 fluid reads;
# test_tillable_fluid_filter is #1793's own: the tillable-tile
# scans in till_probe, plant_probe and farm_ai_probe read
# world.getFluidAt through its ARITY contract, so a wet tile is
# never returned as tillable. It owns the fake console that
# reproduces the debug console's tab-joined multi-return, which
# test_action_outcome_probe imports rather than re-deriving. Those
# three probes each boot a real engine and generate a world; this
# companion boots nothing;
# test_probelib pins probelib.send_json's result contract against a real
# socket and fails if a probe grows a private JSON console wrapper again
# (#1160), and owns probe_engine.py's launcher contract -- runner mode
# execs the resolved binary (#1570), while direct mode BUILDS one before
# the READY deadline starts, holding cabal-build exclusively, owning its
# build's process group, and reporting a preparation failure as one
# (#1913); test_probe_flake mutation-covers probe_protocol.py,
# probe_flake.py and probe_census.py's parsers, and every #1426 protocol
# migration extends it (#1475); test_probe_census is the census's own
# self-test -- the record, its atomic writer, the declared schema, and
# #1429's cohort, freshness and staleness semantics -- against synthetic
# documents in throwaway temporary trees, touching no docs worktree;
# test_probe_claim is #1434's -- the atomic per-probe claim, its lease,
# ownership-safe takeover and release, and the claim-aware orchestration
# boundary -- racing real interpreters against a shared barrier file and
# SIGKILLing one of them, because a claim that must hold between OS
# processes cannot be proved by threads.
# test_probe_resource_lock is #1436's cross-process reader/writer lock --
# the half probe_runner_resources' in-process ledger cannot provide --
# proved with
# separate interpreters and one SIGKILLed holder; test_deflake is the
# /deflake orchestrator, driven entirely through injected adapters, with
# probe_census and probe_flake.Measurement themselves real against
# throwaway censuses. No probe is ever executed by any of them, and the
# real engine-booting ten-run measurement is deliberately NOT wired into
# this gate or CI (tools/README.md states why).
# test_location_embark_probe is #1569's: the artifact ownership of
# tools/location_embark_probe.py -- one invocation-owned directory,
# --resource-root on every boot, release on a pass, a phase-0 return, an
# exception and a boot abort, residue as a failing check, a pre-existing
# same-named save slot left byte-identical, and a read-only checkout
# still yielding a removable tree. It also carries #1746's second
# contract for the same probe: both of that probe's saves return the
# API's own acceptance Boolean and then wait for their OWN request id to
# reach SaveCaptureComplete, and a save that is refused, never reports a
# request id, fails or times out suppresses every session that would
# read the slot. That probe is manual-only needs-gpu, so without this
# companion both contracts are only ever observed by a GPU run neither
# gate can make; the companion boots nothing.
# test_location_probe_config_isolation is #1729's: the private `config/`
# tree tools/location_content_probe.py, location_overlay_probe.py and
# location_stamp_idempotent_probe.py each stage, and that
# tools/portal_ghost_probe.py shares by importing the first of them. All
# four used to SYMLINK config/ in beside the content families, calling
# it read-only content -- but engine init is itself a config/ writer, so
# through that alias a run created files in the developer's own checkout
# and teardown, which unlinks the alias rather than descending it, left
# them there. This drives each builder against synthetic, read-only and
# real source trees, asserting a copy that is no symlink and no
# samefile alias, an absent seeded *.local.yaml, a source left byte- and
# mode-identical after both a new local file and a rewrite through the
# root, a read-only source still yielding a writable removable tree, and
# a teardown that never follows the content symlinks. Three of those
# probes are long and the fourth is manual-only needs-gpu, so without
# this companion the contract is only ever observed by runs neither gate
# can make; the companion boots nothing.
# test_probe_root_cleanup is #1791's: the staging half of the isolated-root
# contract that tools/foraging_probe.py, flora_growth_probe.py,
# farm_ai_probe.py and item_temp_probe.py each carry. A failure while
# STAGING the run's throwaway root used to bypass the cleanup guard
# entirely and leave the invocation-owned tree on disk. It drives each
# probe's real main() in a subprocess with injected staging and removal
# faults, asserting a non-zero run, a visible cause, an absent base, an
# untouched checkout behind the symlinks, and no engine.quit() aimed at
# whoever else holds the port. All four probes are manual-only, so
# without this companion the boundary is only ever observed by long
# engine runs; the companion boots nothing.
# test_flora_growth_probe is #1682's: the artifact ownership of
# tools/flora_growth_probe.py, the other half of what #1616 started.
# Its two fixture YAMLs and its engine log were fixed /tmp names --
# probe_berry.yaml, probe_clover.yaml, flora_growth_probe_engine.log --
# each written with a truncating open(..., "w"), carrying no invocation
# identity and cleaned up by nothing, so two concurrent runs collided on
# all three while a developer's same-named file was truncated outright.
# All three now live under the one directory the invocation already
# owned. This drives the probe's real main() with run_probe substituted:
# disjoint paths for two invocations, no legacy /tmp name, release after
# a pass, an early return, an exception, a boot abort and a handled
# Ctrl-C, opt-in --keep-artifacts retaining on both a pass and a failure
# and naming only what the run actually produced (never calling a
# directory that was never created empty), a cleanup failure making an
# otherwise clean run non-zero, a read-only checkout still yielding a
# removable tree with the source's own modes untouched, and an outside
# same-named decoy left byte-identical. It pins the teardown boundary
# structurally as well as behaviourally, because probelib.boot waits up
# to three minutes for READY and a caller that learns of the engine only
# from the return value owns nothing for that whole span: boot now hands
# the handle over the statement after its Popen (on_launch, an appended
# optional parameter no existing caller passes), and the probe disposes
# of an engine it merely LAUNCHED directly rather than through the port,
# since a boot fails on a busy port exactly because somebody else's
# instance holds it. test_probelib owns the launcher half, including
# that an interrupt or a failing callback DURING the hand-off kills the
# child there rather than let it escape holding the port. The shutdown
# is guarded the same way: quit_engine sends, waits and hard-kills, all
# interruptible, so it runs inside a finally whose fallback kills the
# engine outright. It also pins what the probe still proves:
# the registration order placement hashes are indexed by (sorted real
# flora, then probe_berry, then probe_clover), both fixture bodies by
# sha256, and load_fixture_yaml still stopping the run at setup on a
# fixture that registers nothing. That probe is manual-only and
# worldgen-heavy; the companion boots nothing.
# test_location_content_probe is #1884's: the artifact ownership of
# tools/location_content_probe.py, the other half of what #1620 started.
# Its five fixture YAMLs and its engine log were fixed /tmp names --
# loc_content_probe_bogus.yaml, _bogus_loot.yaml, _quinoa.yaml,
# _quinoa_loot.yaml, _dense.yaml and location_content_engine.log -- each
# written with a truncating open(..., "w"), carrying no invocation
# identity and cleaned up by nothing, so two concurrent runs collided on
# all six while a developer's same-named file was truncated outright.
# The log collision is the sharp one: the probe ASSERTS against that log
# twice, so a foreign truncation could turn a passing phase into a
# failure or a failure into a pass. All six now live under the one
# directory the invocation already owned for its save slots. This drives
# the probe's real main() with run substituted: disjoint paths for two
# invocations, five absolute fixture paths inside the run's own tree, no
# legacy /tmp name anywhere in the module, every one of those six legacy
# paths left exactly as the run found it, release after a pass, a
# failure, an early return, an exception, a _PhaseAborted, a boot abort
# and a handled Ctrl-C, an engine the run merely LAUNCHED killed BEFORE
# the tree it was writing into is removed, opt-in --keep-artifacts
# retaining on a pass, a failure and a boot abort while naming only what
# the run actually produced, a cleanup failure making an otherwise clean
# run non-zero through #1620's own reporting, and an outside same-named
# decoy left byte-identical. It also pins what the probe still proves,
# scanning the COMPLETE reorganized surface (#2095 -- the facade plus
# every scenario owner under tools/location_content/, so an
# exclusion-style check cannot go vacuous once the bodies leave run):
# all seven boot CALL SITES through the one funnel that hands each this
# invocation's log and registers its process as it is launched, the
# regeneration site still a loop over the two visit orders so a run
# LAUNCHES eight processes, both log-reading ASSERTIONS reading that
# same log, the five fixture bodies by sha256 resolved at their single
# source, their registration order and loaders, load_fixture_yaml still
# guarding every one of them, and the three helpers portal_ghost_probe
# imports still the same function objects. That probe is manual-only and
# launches eight engine processes; the companion boots nothing.
# test_movement_probe is #1586's: tools/movement_probe.py --list is a
# metadata query answered from scripts/movement_arena.lua before any
# boot(), for every --mode, and the derived view is held to the runtime
# M.listCourses() by every real course run. That probe is manual-only, so
# without this companion a reintroduced boot() on the listing path, a
# mode dispatch that preempts --list again, or a silently empty inventory
# would only ever be noticed by hand; the companion boots nothing.
# test_farm_ai_probe is #1760's: farm_ai_probe.py's phase-9 capstone
# judges the auto-harvest by the yield's IDENTITY rather than by its
# location at one instant, following the exact harvested instance off
# the ground and into whoever picked it up. Its four classifications --
# produced-on-ground, carried, moved-after-pickup and the only failing
# one, never-produced -- are scheduling-dependent in a real run, so this
# companion drives YieldTrail.ingest with decoded samples and pins all
# four deterministically, plus the arming baseline, the owning-page
# (item.getGroundForUnit, #1666) resolution contract and the plot-tile
# scoping. That probe is manual-only and takes about eleven minutes;
# this boots nothing.
# test_probe_boot_logs is #1763's: tools/preview_probe.py and
# tools/offscreen_probe.py each launch several engines in one run, and
# probelib.boot opens its log truncating, so launches sharing a path
# used to destroy each other's capture -- preview kept only the last of
# about twenty-two, and offscreen's port-reusing restart overwrote the
# long session that preceded it. This companion pins the allocation and
# reporting halves: a distinct path per launch including a repeated
# phase, earlier captures intact under a truncating open, the
# three-engine lifecycle with its restart, a phase-to-path map that
# survives a boot which exits before READY, and no preview call site
# falling back to the shared per-port default. Both probes need a GPU
# and are manual-only, so without this companion a re-shared log would
# only ever be noticed by a dev-machine run; the companion boots
# nothing.
# test_item_list_widget_probe is #1911's: the #1251 unit-to-unit escort
# scenario in tools/item_list_widget_probe.py stages an escort and a
# target that must be OUTSIDE the transfer contract's reach at the
# instant a Mode A session is created. That precondition used to record
# its result and DISCARD the Boolean, so a staging loop that exhausted
# its four terrain-sensitive retries went on to create the session and
# grade five checks against a pair already in reach -- where "the pair
# opens" passes with no approach at all and "the target did not move for
# the whole of the approach" measures a walk that never happened. It is
# terminal now, and this companion is what executes that path: a
# scripted console parks the target at a maximum-axis gap of exactly
# 1.0, and the run is asserted to fail at SETUP, to send no
# transfer_session.create, to grade none of the five, to leave the
# simulation running with the session cleared, and to retain all four
# attempted destinations and paused snapshots in the failure line. Its
# positive control drives the same scenario over a console that lets the
# gap open, so the negative result is a real difference rather than a
# scenario that stops early either way. That probe is manual-only
# needs-gpu and takes about fifteen minutes, and the failing path
# depends on live terrain, so an ordinary run cannot be relied on to
# reach it; the companion boots nothing.
#
# tools/test_deflake_diagnosis.py (#1437) is deliberately absent from
# this list as well, and from the CI job it mirrors: that issue's
# approved rereview amendment scopes the diagnosis lab's own self-test
# to manual invocation. It is engine-free and takes seconds -- run it by
# hand when touching tools/deflake_diagnosis.py.
step "probe runner self-tests"
python3 tools/ci_probes.py --self-test
python3 tools/ci_expensive_gates.py --self-test
python3 tools/ci_docs_fast_path.py --self-test
python3 tools/test_run_probes.py
python3 tools/test_persistence_contract_sweep.py
python3 tools/test_action_outcome_probe.py
python3 tools/test_tillable_fluid_filter.py
python3 tools/test_probelib.py
python3 tools/test_probe_flake.py
python3 tools/test_probe_census.py
python3 tools/test_probe_claim.py
python3 tools/test_probe_resource_lock.py
python3 tools/test_deflake.py
python3 tools/test_location_embark_probe.py
python3 tools/test_location_probe_config_isolation.py
python3 tools/test_probe_root_cleanup.py
python3 tools/test_flora_growth_probe.py
python3 tools/test_location_content_probe.py
python3 tools/test_movement_probe.py
python3 tools/test_farm_ai_probe.py
python3 tools/test_probe_boot_logs.py
python3 tools/test_item_list_widget_probe.py

# The decision .github/workflows/review-gate.yml makes on every
# synchronize push: keep `reviewed:approve` only when the push left the
# PR's own patch identical (#1679). That job runs on a different event
# and mutates a label rather than producing a check, so nothing else
# here would notice it regressing; this self-test is the only thing that
# observes its policy. It builds throwaway commit graphs in a temporary
# directory -- no engine, no network, no GitHub, about a second.
step "review-gate decision self-test"
python3 tools/review_gate_decision.py --self-test

# The other half of that gate (#2184): what the workflow DOES with the
# decision. The removal used to be `|| true` and the required check read
# the event payload, so a strip that failed and a strip that never
# reached the required check both looked like a healthy gate. This pins
# the applied-and-verified outcome policy and the review-gate.yml wiring
# it is useless without -- that workflow runs on an event nothing here
# ever sees. Pure, network-free, sub-second.
step "review-gate label policy self-test"
python3 tools/review_gate_label_policy.py --self-test

# Cheap, no-engine self-test of CI's cache-outcome report (#1358). The
# report itself runs only in CI -- `make ci` restores no GitHub Actions
# cache, so it has no outcome to classify -- but its classification and
# the ci.yml wiring that classification reads are checked here, because a
# mis-wired reporter is indistinguishable from a healthy cache: reverting
# either cache step to the combined `actions/cache` action would empty
# `cache-matched-key` and turn every prefix hit into a reported cold
# cache, with nothing failing.
step "CI cache policy and report self-tests"
python3 tools/ci_cache_epoch.py --self-test
python3 tools/ci_cache_cleanup.py --self-test
python3 tools/ci_cache_report.py --self-test

# The gate that keeps this file honest (#1355): fails if a
# `python3 tools/*.py` check runs in ci.yml's test-and-audits worker and not
# here, or here and not there, outside the audit's hard-coded exemption
# list. Without it the two drift silently, and they already had --- the
# original five of the probe-runner self-tests above ran only in CI.
step "CI/local gate parity audit"
python3 tools/ci_parity_audit.py --self-test
python3 tools/ci_parity_audit.py

if [ "$STEP_N" -ne "$STEP_TOTAL" ]; then
  echo "ci-local.sh: ran $STEP_N steps but labelled them /$STEP_TOTAL --" \
       "the running counter and the 'step' call sites disagree." >&2
  exit 1
fi

echo "==> make ci: all gates passed"
