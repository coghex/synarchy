# Project Review Findings: PRs #1986–#1908

This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1986, #1985, #1984, #1964, #1962, #1951, #1943, #1942, #1936, #1923, #1905, and #1908 — plus direct first-parent commit `4960d4d9` in the same landing interval. The review read each pull request, its linked specification where one existed, merged diff and commits, then traced the surviving behavior at current HEAD. One new current audit-enforcement mistake is retained below for later one-at-a-time disposition. The other eleven pull requests and the direct documentation commit produced no separate current concern, and no concern was explicitly excluded from this batch.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. Parenthesizing a capability projection silently removes its writers from the audit

## 1. EngineEnv capability writer enforcement

### PRR-1. Parenthesizing a capability projection silently removes its writers from the audit

> **Captured note:** Make the EngineEnv capability writer audit either parse semantically inert parentheses around a projection's right-hand side or fail closed when it cannot derive a capability accessor. Today changing `fcField = fieldOne env` to the equivalent `fcField = (fieldOne env)` erases that capability field from the derived accessor map, so a direct `IORef` write through it is classified as an ordinary non-write and passes every blocking check.

**Verification:** Verified against `master@c11455e7` with a two-module synthetic production tree using the audit's public pure helpers; this recheck followed the post-selection merge of PR #1987. With the parenthesized projection, `parse_projection_bindings` and `capability_accessor_map` both returned `{}`, `scan_capability_writes` returned `{'fieldOne': set()}`, and the consumer's `writeIORef (fkFieldOne (toFakeCapability env)) 1` site was classified as `('Consumer.Mod', 'other', None)`. `audit_mutation_sites` consequently returned no violation. Removing only the projection parentheses restores the binding and writer attribution. The shipped self-test and live audit both pass, confirming that this legal projection spelling is outside their current coverage.

**Evidence:**

- `tools/engine_env_capability_audit.py:1757-1765` — `_PROJECTION_BINDING_RE` accepts only a bare or qualified accessor immediately followed by `env`; it permits no grouping around the accessor application.
- `tools/engine_env_capability_audit.py:1768-1802` — `parse_projection_bindings` returns only regex matches from the projection body, with no validation that every record field was understood.
- `tools/engine_env_capability_audit.py:2543-2582` — `capability_accessor_map` derives the entire capability-accessor ownership map from those parsed bindings and silently skips anything absent from them.
- `tools/engine_env_capability_audit.py:3160-3168` — when the mutation scanner cannot resolve the first-argument accessor to a field, it records the primitive occurrence as `other` rather than `unclassifiable`.
- `tools/engine_env_capability_audit.py:3191-3208` — requirement 6 blocks only sites classified as `unclassifiable`; the erased capability accessor therefore never reaches the closed-form safety check.
- `tools/test_engine_env_capability_audit.py:1898-1915` — the synthetic capability used to prove capability-to-`EngineEnv` canonicalization covers only the unparenthesized `fkFieldOne = fieldOne env` shape.
- `docs/engineenv_capability_inventory.md:159-174` — the canonical capability-record convention requires a total one-way projection but imposes no expression-formatting restriction that would make parentheses invalid.
- `docs/engineenv_capability_inventory.md:1212-1217` — the writing-module authority promises to recognize direct mutation through an `EngineEnv` accessor or any capability-record accessor projecting it.
- Closed issue #1892 and PR #1905 describe the capability accessor map as derived from live projections and require every mutation-primitive occurrence to classify so an unmodeled spelling fails loudly. This miss happens upstream of that safety net instead. Blame attributes the projection parser to the completed capability-boundary work and its reuse for writer canonicalization to PR #1905; no later merge corrects the hole.
- `python3 tools/test_engine_env_capability_audit.py` and `python3 tools/engine_env_capability_audit.py` both exited 0 during this review. The focused reproduction above also exited 0 while reporting an empty writer set and zero blocking violations.

**Handoff context:**

- **Current behavior:** A harmless parenthesis in a capability projection removes the record field from the derived capability-accessor map. Every direct write through that selector then disappears from `CAPABILITY_WRITER_MODULES` enforcement, residue reporting, and requirement 6 while the gate remains green.
- **Expected behavior:** Syntactically equivalent total projections canonicalize to the same live `EngineEnv` field. If a projection shape cannot be understood safely, the audit fails with the capability module, projection, and missing record field instead of treating every consumer use as unrelated code.
- **Scope and constraints:** This is a Python audit and self-test defect from PR #1905 / issue #1892, not a runtime Haskell defect. Preserve the deliberate direct-`IORef` boundary, import-scope rules, pass-on residue semantics, §6.1 exemption, and both-directions writer-map checks. Do not broaden the tool into general Haskell dataflow or lexical-scope analysis.
- **Verification target:** Add a synthetic capability whose projection uses redundant parentheses, assert that its selector still maps to the underlying `EngineEnv` field and that an undeclared consumer write is rejected, then keep both `tools/test_engine_env_capability_audit.py` and the live `tools/engine_env_capability_audit.py` passing. Also pin a fail-closed case for a genuinely unreadable projection so another syntax variant cannot erase a selector silently.
- **Deduplication:** The up-front open-issue inventory and all-state searches for capability projections, writer maps, `CAPABILITY_WRITER_MODULES`, and parenthesized projections found only closed issue #1892, whose implementation introduced the vulnerable enforcement. No open or closed follow-up issue and no project-review/findings-report entry owns this exact parser gap.
- **Remaining uncertainty:** None.
