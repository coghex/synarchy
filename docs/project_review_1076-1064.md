# Project Review Findings: PRs #1076–#1064

These entries record focused evidence from the senior review of merged PRs #1076 through #1064 for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

## Status

- [x] PRR-1. Cabal inventory audit accepts duplicate declared modules — [#1280]

## 1. Cabal library inventory enforcement

### [#1280] PRR-1. Cabal inventory audit accepts duplicate declared modules

> **Captured note:** Validate each `src/` file's Cabal-resolvable path as well as its declared module name. The audit lets an orphan file impersonate an already-listed module, so two source paths declaring one inventoried name still produce a successful result even though Cabal compiles only the canonical path.

**Verification:** Verified — a synthetic tree containing both `src/Existing.hs` and `src/Orphan.hs`, each declaring `module Existing`, against a library inventory listing only `Existing`, is collected as two source files but makes the current audit print “Every library source module is listed” and exit 0.

**Evidence:**

- `tools/cabal_module_audit.py:123` — source identity is derived from the declaration whenever one is present; the path-derived name is used only as a fallback when no declaration is found.
- `tools/cabal_module_audit.py:133` — collection preserves both files as separate `(name, path)` entries, so the information needed to detect duplicate declarations or a declaration/path mismatch is still available at this point.
- `tools/cabal_module_audit.py:147` — the actual audit asks only whether each entry's declared name belongs to the Cabal-name set; a second path carrying an already-listed name therefore satisfies the same membership check.
- `tools/cabal_module_audit.py:155` — `run` prints different inventory/source counts but bases its exit code only on that name-membership result, so the reproduced `1 module / 2 files` mismatch exits successfully.
- `tools/test_cabal_module_audit.py:199` — the declaration-vs-path test positively establishes that the declaration wins, but covers only a declaration whose name is unlisted; there is no case where a mismatched or duplicate declaration reuses an already-listed name.
- `synarchy.cabal:921` — the library resolves modules under `hs-source-dirs: src`, making filesystem placement part of whether a listed module actually selects a particular source file.

**Handoff context:**

- **Current behavior:** The live tree currently has 714 distinct files and 714 listed names, but the CI guard can return success after an extra dead `.hs` file is added if that file repeats any name already present in the library inventory. The displayed count mismatch is informational only.
- **Expected behavior:** Every `src/**/*.hs` path is either the unique canonical source for exactly one listed library module or makes the audit fail with its path and the declaration/path or duplicate-name conflict.
- **Scope and constraints:** Surfaced while reviewing PR #1066 / issue #972, whose purpose is to prevent source files from remaining invisible to Cabal and CI. Preserve library-stanza scoping, nested discovery, any-cwd execution, clear unlisted-module diagnostics, and the existing successful 714-to-714 live-tree check.
- **Remaining uncertainty:** The defect is reproduced in the audit itself, but the processor should choose whether the canonical rule is path-derived identity, declaration/path equality, unique declarations, or all three; Cabal features such as generated modules or future alternate source roots may affect the most general implementation.
