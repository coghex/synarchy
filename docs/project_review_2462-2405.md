# Project Review Findings: PRs #2462–#2405

Reviewed `coghex/synarchy` PRs #2462, #2417, #2416, #2412, #2414,
#2411, #2410, #2409, #2408, #2407, #2406, and #2405, including their
linked specifications, commits, merged changes, and surviving code at
`60c7168f2303299acbe1a4f0ec331e89fd0d4ebf`. The intervening direct
documentation commits `e89e61bb112318765e44728fbbf31b86f3426499` and
`7680c9ef6e73822c4e9bca1fa2016b90cf0f82b9` were also reviewed; other
direct commits in the interval were covered in preceding batches.
No implementation or tracker changes were made. GPU-dependent probe
changes were assessed from their real control paths and focused offline
tests, not claimed as newly verified rendered runs.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. The tshow spelling guard silently accepts an in-scope wrapper before an unspaced composition dot

## 1. Shared text-rendering spelling guard

### PRR-1. The tshow spelling guard silently accepts an in-scope wrapper before an unspaced composition dot

> **Captured note:** PR #2411's guard consumes the composition after `show`
> as part of its name. Consequently, removing whitespace from a valid
> in-scope wrapper changes an audit failure into a clean result. Correct
> lexical recognition without reopening #2177's deliberately excluded
> expression-analysis scope.

**Verification:** Importing the current `tools/tshow_spelling_audit.py` and
calling `find_violations` on a module importing `Data.Text` qualified as `T`
reports one violation for `T.pack . show . id`, but none for
`T.pack . show.id`, `T.pack (show.id $ x)`, or
`T.pack $ show.id $ x`. This is not invalid Haskell:
`ghc -ignore-dot-ghci -e 'Data.Text.unpack (Data.Text.pack . show.id $ (12::Int))'`
exits successfully and prints `"12"`. The existing guard self-test passes
all 124 fixtures and its production-tree run passes, so those checks do
not cover this lexical boundary. The guard remains unchanged at the
subsequent `f39e041087466c98a91e17140c04f602c1375f3e` checkout.

**Evidence:**

- `tools/tshow_spelling_audit.py:896` — `_names_show` decides whether the
  lexeme after a recognized connector names the `Show` method.
- `tools/tshow_spelling_audit.py:907` — the loop consumes identifier
  characters and every dot; `show.id` becomes one candidate rather than
  the bare `show` followed by a composition operator.
- `tools/tshow_spelling_audit.py:910` — splitting the complete run at its
  final dot produces `id` as the head, and returns false without refusal.
- `tools/tshow_spelling_audit.py:1031` — the failed name test drops the
  otherwise recognized wrapper from the violation list.
- Issue #2177 requirement 4 explicitly includes `pack . show`,
  `pack (show …)`, and `pack $ show …`, measures adjacency modulo
  whitespace/comments, and does not analyze the surrounding expression.
  Its scope note and both approval comments do not exclude this case.

**Handoff context:**

- **Current behavior:** Valid source containing a closed-list spelling
  can pass the mirrored CI guard simply because the next composition
  operator has no surrounding spaces.
- **Expected behavior:** The recognized `show` token must remain visible
  when followed by an unqualified composition operator; an unsupported
  ambiguity should be refused explicitly instead of silently certified.
- **Scope and constraints:** Keep the correction within name-token
  recognition and focused fixtures. Preserve binding-based Text/Show
  resolution, record-selector handling, exemptions, and the explicit
  operator-section, prefix-operator, and expression-analysis exclusions.
  This is a guard-coverage defect, not evidence of changed rendered bytes
  in the production rewrites.
- **Verification target:** Assert matching results for spaced and compact
  versions of each affected closed-list form, including a recognized
  qualified `show`; retain negative record-selector and non-Text cases.
  Run the guard self-test, production scan, and applicable shared-lexer
  tests if that lexer changes.
- **Deduplication:** Open/closed tracker searches for `tshow` found the
  completed consolidation issues #1099 and #2177, not this new guard hole.
  `docs/project_review_2007-1456.md` PRR-1 and code-health CH-75 concern
  the earlier missing consolidation/enforcement, addressed by #2177.
- **Remaining uncertainty:** No current production use of this compact
  spelling was established. The demonstrated failure is the regression
  gate's acceptance of valid synthetic source, not an observed gameplay
  failure.
