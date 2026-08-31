# Project Review Findings: PRs #681–#669

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #681, #680, #679, #678, #677, #676, #675, #674, #673, #672, #670, and #669 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The river-identification, world-render, Lua world/world-query/equipment/units, geology-timeline, and save-command splits retain their intended behavior in the current tree, and PR #676's documentation remains accurate. PR #674's original load modules were deliberately superseded by #763's transactional load architecture; the current two-page fresh-process save/load probe passes. PR #673's mutable CI-image publication race was fixed later by #784, and current CI runs are green. PR #672's passive-control correlation defect was fixed by #783; its remaining modal-scope disagreement is already captured as PRR-5 in `docs/project_review_859-848.md` and is not duplicated here. PR #670's explicit resource-root flag still accepts an empty value as the current directory.

## Status

- [x] PRR-1. An empty explicit resource root silently selects the current directory — [#1949]

## 1. Resource-root argument validation

### [#1949] PRR-1. An empty explicit resource root silently selects the current directory

> **Captured note:** Reject an explicitly empty `--resource-root` value instead of interpreting it as the current directory. An unset shell variable must not override `SYNARCHY_ROOT` and silently select whichever checkout happens to be the caller's cwd.

**Verification:** Verified with the current executable. From the repository root, a dump launched with an intentionally invalid `SYNARCHY_ROOT` and `--resource-root ""` exited 0, printed `resource root: /Users/vincentcoghlan/work/synarchy (from --resource-root)`, and generated the world. The empty flag value therefore won precedence, became the cwd through `makeAbsolute`, and bypassed both the invalid environment root and the missing-value failure. The canonical resource-root probe passes because it covers an absent flag, a nonexistent non-empty path, and a trailing bare flag, but not an explicitly supplied empty argument.

**Evidence:**

- PR #670 / issue #636 established the contract `--resource-root <path>` over `SYNARCHY_ROOT` over cwd, required explicit-root failures to be actionable, and added a bare-flag failure so a missing value could not silently fall back to cwd.
- `app/App/Cli.hs:159-169` returns the token following a string flag without validating its content. `parseStrArg` at lines 182-192 consequently returns `Just ""` for an explicitly empty shell argument rather than treating it as malformed.
- `app/App/ResourceRoot.hs:41-47` rejects the flag only when `parseStrArg` returns `Nothing`. The empty argument returns `Just ""`, so it bypasses the `requires a path argument` exit.
- `app/App/ResourceRoot.hs:48-54` rejects an empty `SYNARCHY_ROOT` value but accepts every `Just p` from the flag, including `p == ""`. The empty flag therefore overrides a valid or invalid environment value despite not naming a resource root.
- `app/App/ResourceRoot.hs:55-69` passes the empty string to `makeAbsolute`, which resolves it to the current directory; if that directory contains the four resource families, validation succeeds and the process reports that cwd as explicitly selected by the flag.
- Live reproduction: `SYNARCHY_ROOT=/definitely/not/a/synarchy/root $(cabal list-bin exe:synarchy) --dump --seed 7 --worldSize 32 --region 0,0,0,0 --resource-root ""` exited 0 from the repository root and identified that root as coming from `--resource-root`.
- `tools/resource_root_probe.py:95-126` covers no root, a nonexistent explicit root, and `--resource-root` as the last argv token. It has no empty-string case, so the full probe currently passes all seven checks while leaving this precedence path untested.
- `README.md:19-28` and `CLAUDE.md:546-559` document the flag operand as a path and cwd only as the fallback when no explicit root is supplied; neither describes an empty flag as an alias for cwd.
- Full tracker and findings-report searches found the closed source issue #636 but no follow-up for empty explicit resource-root values or their precedence over `SYNARCHY_ROOT`.

**Handoff context:**

- **Current behavior:** `--resource-root ""` is treated as a present, highest-precedence explicit root whose filesystem meaning becomes cwd. A common invocation such as `--resource-root "$SYNARCHY_ROOT_OVERRIDE"` therefore silently loads cwd resources when that variable is unset, even if `SYNARCHY_ROOT` points elsewhere.
- **Expected behavior:** An explicitly empty flag operand is malformed and exits 1 before boot with an actionable resource-root argument error. Non-empty flag paths continue to win over non-empty `SYNARCHY_ROOT`, and cwd remains the fallback only when neither mechanism supplies a root.
- **Scope and constraints:** Surfaced from PR #670 / issue #636. Preserve relative and absolute non-empty paths, paths containing spaces, all six boot modes, pure JSON/READY stdout, resource-family validation, and the documented output-path chdir behavior. Extend the focused resource-root probe with both the empty argument and its precedence over a non-empty environment value.
- **Remaining uncertainty:** The original issue explicitly rejected a flag with no following argv token but did not name the distinct empty-token case. Filesystem APIs conventionally resolve an empty path relative to cwd, so the processor should confirm that this was not meant as a hidden cwd alias; the documented `<path>` grammar and the environment variable's existing empty-value rejection both point toward treating it as invalid.
