# Project Review Findings: PRs #2475–#2357

Reviewed `coghex/synarchy` PRs #2475, #2469, #2472, #2365, #2364,
#2363, #2359, #2362, #2360, #2361, #2358, and #2357 against their linked
specifications, commits, first-parent landed patches, and surviving code at
`167e3e88f093eaa2b9919d482cf2339b2b60b9b7`. Also reviewed the direct
documentation publication at that commit; other direct commits within this
interval were covered in previous batches. Existing findings in `docs/bugs.md`
are not duplicated here. No implementation or tracker changes were made.
GPU-dependent behavior was traced and checked with focused offline tests,
not claimed as newly rendered evidence.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. Haddock open-type resolution ignores imports that withhold record fields

## 1. Qualified Haddock link validation

### PRR-1. Haddock open-type resolution ignores imports that withhold record fields

> **Captured note:** PR #2358's `open_type_fields` follows the module that
> declares a type but does not preserve the field restrictions on the path
> to its re-exporter. A `T(..)` export therefore makes unavailable record
> fields look exported, silently accepting genuinely dead qualified links.

**Verification:** Reproduced through the current audit's real `main` using
the temporary-root fixture in `tools/test_haddock_link_audit.py`. `Alpha`
exports `T(..)` and declares `data T = T { secret :: Int }`. `Beta` exports
`T(..)` but imports only `Alpha (T)`. A comment linking `'Beta.secret'`
returns exit 0 and `No new dead qualified haddock links (0 still baselined).`
Replacing the import with `import Alpha hiding (secret)` produces the same
false clean result. GHC successfully checks each `Beta` module, but rejects
a separate consumer of `Beta.secret` with GHC-76037: the module does not
export `secret`. The positive control, `import Alpha (T(..))`, passes both
the audit and GHC consumer check. Temporary fixtures were cleaned up.

The existing self-test passes all 56 cases / 141 assertions, and the
production-tree audit passes with 120 baselined links. Those successes do
not cover these restricted-import cases. The audit implementation is
unchanged between PR #2358 and the reviewed HEAD.

**Evidence:**

- `tools/haddock_link_audit.py:545` — `open_type_fields` claims to resolve
  the fields represented by a module's `T(..)` export.
- `tools/haddock_link_audit.py:566` — its import walk only checks whether
  the source module names the type, then unions recursively discovered
  fields. It ignores the import's selected names and hiding restrictions,
  as well as whether an intermediate module exports those fields.
- `tools/haddock_link_audit.py:574` — `import_supplies` implements import
  restrictions for the separate `module X` re-export path; the open-type
  branch does not apply an equivalent field-availability check.
- `tools/haddock_link_audit.py:608` — `exports_symbol` trusts the resulting
  field set and returns true; `find_findings` consequently discards the
  candidate at line 667.
- Haskell's export rule limits `T(..)` to the constructors and fields
  currently in scope, not every field in its original declaration.
  [Haskell 2010 Report, §5.2–5.3](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html).

**Handoff context:**

- **Current behavior:** A supported, valid restricted import can conceal
  a new dead link from the CI ratchet without a baseline addition.
- **Expected behavior:** Resolve type identity and field availability
  separately, respecting restrictions and intermediate export surfaces.
  Do not reject a valid `T(..)` link merely because the field is available
  only under a qualified name; that is legal for this export form.
- **Scope and constraints:** Correct the audit and focused fixtures, not
  production Haskell exports. Preserve #2292's comment-aware detection,
  intentional exclusions, and generated-baseline policy. Do not widen the
  audit to unrelated link classes or hand-edit baseline allowances.
- **Verification target:** The selected-type-only and hidden-field fixtures
  must report the link and fail with an empty baseline; a full-field import
  must remain clean. Add a restricted intermediate-module case and qualified
  positive controls, then run the existing self-test and production audit.
- **Deduplication:** Open/closed tracker searches for Haddock audit,
  restricted imports, and record fields found #2292's original guard but
  no separate corrective issue for this path. Local project-review reports
  contain no equivalent finding. The earlier review-round fixes concern
  namesake types and `module X` import restrictions, not this surviving
  `T(..)` branch.
- **Remaining uncertainty:** No shipped dead-link occurrence hidden by this
  exact path was established. The verified current defect is the guard's
  false-clean result on valid, explicitly in-scope source.
