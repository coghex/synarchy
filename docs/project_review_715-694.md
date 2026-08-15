# Project Review Findings: PRs #715–#694

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #715, #714, #712, #711, #704, #705, #701, #703, #702, #696, #695, and #694 — for later one-at-a-time disposition. The same first-parent window contains one direct non-PR commit, `aa18b24b` (`Document July 2026 project assessment`); it added an explicitly archived history document and introduced no separate current concern.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #714's modifier-lifetime fix, #712's semantic-name layer, #711's offscreen rendering path, #705's swapchain capture guard, and the structural splits in #703, #702, #695, and #694 retain their intended behavior in the current tree. PR #715's optional persona flavor path accepts unusable model output as a success. PR #704's registered real-engine behavior probe no longer passes against current master. PR #701's critic validation permits contradictory verdicts for one candidate. PR #696's lenient player reply path crashes on valid JSON values that are not objects. No separate current concern was found for the other reviewed PRs.

## Status

- [ ] PRR-1. Persona flavoring marks empty or non-string model output as successful
- [ ] PRR-2. The registered F4 behavior probe no longer passes on master
- [ ] PRR-3. The critic can publish conflicting verdicts for one friction candidate
- [ ] PRR-4. Lenient player replies crash the harness on valid non-object JSON

## 1. Persona flavor output validation

### PRR-1. Persona flavoring marks empty or non-string model output as successful

> **Captured note:** Validate the model's flavor object before freezing it into a persona. Empty or wrong-typed `name`/`blurb` values must not leave the deterministic template unchanged—or stringify `null`—while provenance claims that flavoring succeeded.

**Verification:** Verified. The structured-output schema requires two strings but permits empty strings, while the compatibility fallback accepts arbitrary parsed JSON without validating it against that schema. `llm_flavor` silently retains the template when the resulting strings are empty and always writes `flavored: true`; `null` values are converted into the literal name `none` and temperament `None`. Pure injected-completion reproductions exercised both cases without an external model call.

**Evidence:**

- PR #715 / issue #649 added deterministic persona generation plus optional LLM-authored flavor that is frozen into the resulting spec for reproducible playtests.
- `tools/playtest/personas.py:74-84` requires `name` and `blurb` to be strings but supplies no non-empty constraint, so `{"name":"","blurb":""}` is valid even when the model honors structured output.
- `tools/playtest/personas.py:359-391` deliberately retries without structured output when a model or configuration rejects the schema, then returns any JSON object it can parse without applying `FLAVOR_SCHEMA` itself.
- `tools/playtest/personas.py:394-411` obtains fields with `.get`, stringifies both through `_slug` or `str`, conditionally replaces the original prose only when the converted value is non-empty, and unconditionally records `{"model": model, "flavored": true}`.
- Injecting a completion of `{"name":"","blurb":""}` left the deterministic name and temperament unchanged but marked the persona flavored. Injecting `{"name":null,"blurb":null}` produced the literal name `none`, temperament `None`, and the same success provenance.
- `tools/playtest/personas.py:565-582` self-tests one well-formed fake completion and the deterministic-core freeze, but has no empty, missing, or wrong-type flavor case. The complete persona self-test currently passes.
- Full tracker and findings-report searches found the closed source issue #649 and its parent playtest work, but no follow-up for validating flavor output before recording success.

**Handoff context:**

- **Current behavior:** A legal empty structured response performs no flavor rewrite but is recorded as successful. On the intentional unstructured fallback, `null` or other wrong-typed fields can be stringified into bogus persona text or likewise leave stale template text under successful provenance.
- **Expected behavior:** The output has non-empty fields of the expected types before it is frozen and marked flavored. Unusable output is either rejected clearly or falls back explicitly without claiming that LLM flavoring succeeded.
- **Scope and constraints:** Surfaced from PR #715 / issue #649. Preserve seed-deterministic axes, goal, tendencies, sampling metadata, valid flavor freezing, and compatibility with models that do not support structured output if that fallback remains supported. Do not regenerate prose downstream or silently change the deterministic core.
- **Remaining uncertainty:** The desired failure policy is a product choice: abort the requested `--llm` operation, retry, or emit the unflavored template with explicit failure provenance. The defect is the false success and invalid coercion, not a prescribed policy.

## 2. Action-outcome behavior probe drift

### PRR-2. The registered F4 behavior probe no longer passes on master

> **Captured note:** Keep the real-engine action-outcome probe runnable against the world and build-tool semantics it is meant to certify. Its default fixtures currently leave the chop regression unexercised and drive portal placement only as far as the newer confirmation boundary.

**Verification:** Verified as a current verification failure. Running `python3 tools/action_outcome_probe.py --port 19420` on current master exited nonzero after most of its action-outcome checks passed. The default generated world yielded no flora at the probe's sparse chop sample points, and the isolated portal arena now correctly opens the remote-settlement confirmation introduced later in #779 instead of immediately spawning and recording an accepted placement. The portal root cause is structural and deterministic for an arena with no placed locations; the chop fixture's failure is seed/world-layout dependent but occurred with the probe's default seed and settings.

**Evidence:**

- PR #704 / issue #646 introduced the F4 action-outcome oracle and its real-engine probe. `tools/ci_probes.py:245-248` still registers `action_outcome` as manual-only because it is slow and needs a real generated mixed box and tree; that classification does not make a failing default run expected.
- The current default run passed record/drain validation, fractional-coordinate preservation, wire rejection and acceptance, the till partial path, unloaded and missing-page rejection, and framebuffer-coordinate conversion. It failed only the chop fixture and portal-placement assertion, then reported `SOME FAILED` and exited 1.
- `tools/action_outcome_probe.py:96-120` samples every fourth coordinate within a fixed region and returns `None` unless one sampled tile carries flora and produces a genuine partial 5×5 designation. Lines 279-288 turn the absence of such a sample into a hard failure suggesting another seed, so the advertised default invocation is not a dependable check of the regression.
- `tools/action_outcome_probe.py:327-388` creates an empty `portal_probe` arena, arms a starting portal, calls the real `buildTool.handleMouseDown` path once, and immediately expects an accepted outcome plus one newly spawned building. The observed result was an empty drain and a building count of 0→0.
- After later PR #779, `scripts/build_tool.lua:1003-1022` classifies a portal as remote when it is farther than the threshold from every placed location—or when the page has no placed locations at all—and opens a confirmation modal without spawning or exiting placement. The probe's new empty arena therefore necessarily stops at that modal boundary.
- `scripts/build_tool.lua:910-941` retains the shared `commitStartingPlacement` helper that spawns the portal and records the accepted action outcome after direct or confirmed placement. The probe never invokes the confirmation handler or that helper after opening the warning.
- File history shows the portal fixture came from PR #704's review work and was not updated for #779's confirmation semantics. Full tracker and findings-report searches found the closed coordinate-space follow-up #774, the closed remote-placement feature #779, and the separate stale coverage-report concern, but no follow-up for this behavior probe's present failures.

**Handoff context:**

- **Current behavior:** The repository's registered behavior probe exits 1 on a correct current build. One required worldgen fixture may not exist at its sampled coordinates, while the portal assertion encodes the pre-confirmation interaction and interprets the newer warning as a failed placement.
- **Expected behavior:** The probe's documented/default invocation builds deterministic enough fixtures to exercise the promised partial-chop contract and follows the current portal confirmation flow—or scopes its accepted-placement assertion to the shared commit boundary—then exits zero when those contracts work.
- **Scope and constraints:** Surfaced from PR #704 / issue #646, with portal drift introduced by #779. Preserve real public Lua/action-outcome coverage, verification of the chop `requested = applied + dropped` count, accepted portal placement without a rejection reason, and the probe's honest slow/worldgen-heavy classification. Do not bypass the user-facing confirmation in a way that falsely claims to test the complete click flow.
- **Remaining uncertainty:** The processor should choose between constructing deterministic flora, searching the generated world more robustly, or pinning a proven world fixture. For the portal, it should decide whether a headless confirmation stub or a focused call to the shared post-confirmation helper best states the intended contract.

## 3. Critic candidate adjudication

### PRR-3. The critic can publish conflicting verdicts for one friction candidate

> **Captured note:** Enforce one coherent adjudication per friction candidate. A finding may legitimately cover several candidates, but one candidate must not independently satisfy multiple findings with conflicting `defect` and `intended` verdicts.

**Verification:** Verified with the current pure validation path. Two individually grounded findings were constructed for the same real candidate: one classified it as `defect`, the other as `intended`. `validate_findings` accepted both, assigned the candidate to both `covers` arrays, emitted no warning, and considered the candidate fully adjudicated.

**Evidence:**

- PR #701 / issue #648 added the post-session critic, including candidate selection, batched adjudication, evidence validation, and a bounded repair pass for candidates left uncovered.
- `tools/playtest/critic.py:818-890` validates each finding independently. It checks the cited candidate, turn, player quote, oracle anchor, and shown frame, then returns a set of valid candidate IDs for that finding.
- `tools/playtest/critic.py:893-926` stores that result as each finding's `covers`, but never builds a reverse candidate-to-findings index or checks duplicate coverage and contradictory verdicts.
- `tools/playtest/critic.py:929-934` reduces all coverage to a set union. A candidate present once or many times is equally “covered,” so duplicate adjudications suppress the repair path rather than trigger reconciliation.
- A pure reproduction using the current trace/candidate fixtures supplied two evidence-valid findings covering candidate `C1`, one with verdict `defect` and one with verdict `intended`. The validated result retained both `('Defect view', 'defect', ['C1'])` and `('Intended view', 'intended', ['C1'])`, with an empty warnings list.
- `tools/playtest/critic.py:1024-1057` accumulates results across calls and batches into one flat findings list, making cross-call duplicate adjudication structurally possible even though the prompt asks a batch not to emit other candidates.
- The self-tests cover batching, missing-candidate repair, and invalid evidence (`tools/playtest/critic.py:1538-1580` onward), but the deterministic fake critic emits a coherent per-candidate result and does not exercise duplicate or conflicting coverage. The full critic self-test currently passes.
- Full tracker and findings-report searches found the closed source issue #648 and unrelated critic work, but no follow-up for duplicate or contradictory candidate adjudications.

**Handoff context:**

- **Current behavior:** Evidence discipline is enforced within each finding, but the final report can contain two separately valid claims that give the same observed friction mutually incompatible verdicts. Coverage and repair logic treat that contradiction as complete success.
- **Expected behavior:** Each candidate receives one coherent final adjudication. It remains valid for one finding to consolidate several candidate IDs, but repeated coverage of one candidate is rejected, reconciled deterministically, or sent through a bounded repair that cannot publish incompatible verdicts.
- **Scope and constraints:** Surfaced from PR #701 / issue #648. Preserve multi-candidate consolidation, multi-batch frame discipline, verbatim player/oracle grounding, low-confidence downgrades, and bounded failure handling. The fix must distinguish “one finding covers many candidates” from “one candidate is claimed by many findings.”
- **Remaining uncertainty:** The processor should select the reconciliation policy: reject all duplicates, keep a deterministic winner only when compatible, merge compatible findings, or ask the critic to repair the conflict. Conflicting verdicts need handling regardless of that choice.

## 4. Lenient player reply parsing

### PRR-4. Lenient player replies crash the harness on valid non-object JSON

> **Captured note:** Treat a valid JSON value that is not an object as unusable model output and downgrade it to the same recorded wait used for malformed JSON. The lenient path promises that a confused response becomes playtest data rather than terminating the session.

**Verification:** Verified with direct pure reproductions. Passing the valid JSON replies `[]`, `null`, and `"wait"` through `_lenient_parse` succeeds, returning a list, `None`, and a string respectively. Passing each result to `normalize_turn` raises `AttributeError` because it immediately calls `.get`, outside the parse exception handler.

**Evidence:**

- PR #696 / issue #647 added the autonomous player harness with a structured reply schema and a deliberate lenient fallback for configurable models that reject structured output.
- `tools/playtest/agent.py:123-130` returns the result of `json.loads` without requiring a mapping. Consequently any JSON scalar, array, or `null` is considered successfully parsed.
- `tools/playtest/agent.py:133-146` documents that unusable replies become recorded `wait` actions, but assumes its argument is a dictionary and calls `data.get` repeatedly before it can normalize the action.
- `tools/playtest/agent.py:193-204` makes the lenient route intentionally reachable: after a structured-output `BadRequestError`, the player retries without the schema and keeps that mode for the rest of the session.
- `tools/playtest/agent.py:205-219` catches `ValueError` and `TypeError` only around `_lenient_parse`; `normalize_turn(data)` executes afterward. A successfully parsed non-object therefore bypasses the fallback record and terminates the turn with `AttributeError`.
- Direct calls reproduced `AttributeError: 'list' object has no attribute 'get'` for `[]`, equivalent errors for `NoneType` from `null`, and `str` from `"wait"`.
- The player/runner self-test currently passes and covers normal structured replies, invalid action normalization, and fallback behavior, but not valid non-object JSON at the normalization boundary.
- Full tracker and findings-report searches for the parser and failure shape found the closed source issue #647 but no follow-up covering non-object JSON replies.

**Handoff context:**

- **Current behavior:** Malformed JSON is safely recorded as a wait, but syntactically valid JSON of the wrong top-level type crashes the playtest session. This is most directly exposed by models or configurations using the intentional unstructured compatibility path.
- **Expected behavior:** Every parsed value that is not a mapping is classified as unusable and produces a wait plus a diagnostic note, while raw response text and usage data remain available for later criticism.
- **Scope and constraints:** Surfaced from PR #696 / issue #647. Preserve the structured schema for supporting models, the configurable-model fallback, non-oracular player inputs, raw response retention, usage accounting, and normalization of malformed action objects. The narrow boundary check should not hide transport, authentication, or engine failures.
- **Remaining uncertainty:** The default model may normally enforce the object schema, so the failure is likely latent in the explicitly supported fallback/configurable-model path rather than common in default sessions. That affects priority, not reproducibility.
