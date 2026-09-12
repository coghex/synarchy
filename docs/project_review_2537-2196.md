# Project Review Findings: PRs #2537–#2196

Reviewed `coghex/synarchy` PRs #2537, #2238, #2237, #2235, #2190,
#2222, #2207, #2200, #2197, #2181, #2133, and #2196 against their
specifications, commit messages, landed patches, and current consumers.
Their GitHub patches agree with their first-parent landing diffs after
normalizing diff metadata. Also reviewed direct documentation commits
`ea2ad316ddac9d23788111a362d8df7b421022d4` and
`f4f2bb699d6c4012a771f35baafdf4a2a8dedbb3` in the older landing interval.
Verification used the package rebuilt at `7b2833f79`; the three failure
paths remain unchanged at `92948204f`. The previously excluded #2377
concern remains excluded. No implementation or tracker was changed.

Focused checks passed: 15 step-protocol, 4 load-replacement, 112 randomized
interactive-bounds, 63 structure-ghost, 48 seam, 42 plan, 12 no-residue,
46 generated-library, and 2 preanalysis-boundary headless examples.
The ten migrated probe protocol fixtures passed 120 assertions; writer
self-tests passed 173 groups / 260 assertions; promotion passed 66 checks;
Haddock self-tests passed 43 checks and the live audit passed. Art-critic
preanalysis and its caller self-tests passed without remote API calls.
Mechanical AST/body and case-order comparisons checked the large Python and
Haskell moves rather than treating file splits as behavioral proof.
The expedition facade's help/import smoke passed; its expensive integrated
scenario was not rerun. No full CI or fresh GPU session was run.

Later flora-design corrections, already-tracked capability-summary drift
(#2269), the already-reported preanalysis Markdown-path problem, and the
already-reported promotion `parse_verbose` call mismatch are not new entries.
The direct census update retains historical commit-qualified measurements;
its seven added ten-run samples have internally consistent counts, not a
new claim about current probe reliability.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. Propagate nested session-reset failures to load reconciliation
- [ ] PRR-2. Verify retained payload integrity before reporting unchanged publication
- [ ] PRR-3. Reject case aliases of reserved generated-library payload names

## 1. Load reconciliation

### PRR-1. Propagate nested session-reset failures to load reconciliation

> **Captured note:** PR #2238 adds correctness-relevant session resets under
> a registry that logs and swallows each failure. The outer load callback
> consequently appears successful even when one of those resets failed.

**Verification:** Ran the real `scripts/ui_manager_menu.lua` and
`scripts/ui/view_teardown.lua` in Lua, with unrelated menu dependencies
stubbed and `world.getActiveWorldId()` returning nil. Injected an error in
`thought_log.clearSession`; the other five reset hooks recorded their calls.
Calling the real `uiManager.onSaveLoaded` through `pcall` returned
`true, nil`, with one named error logged and all five other resets attempted.
Thus isolation works but error propagation does not. The four existing
load-replacement examples passed; they cover normal clearing, idempotence,
and pre-bootstrap behavior, not this nested error.

**Evidence:**

- `scripts/ui/view_teardown.lua:380` — the registry loops over applicable hooks; line 385 catches each error and line 387 logs it without accumulating or returning failure.
- `scripts/ui_manager_menu.lua:215` — `onSaveLoaded` calls that registry and then returns normally.
- `src/Engine/Scripting/Lua/Util.hs:86` — the reconciliation broadcast uses the reporting callback boundary, which can observe only errors escaping the module call.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:363` — collects broadcast failures; line 391 chooses `finishLoad` for an empty list instead of line 393's `failReconciliation`.
- `docs/engine_contracts.md:2482` — post-publication reconciliation failure is an unsuccessful load, not rollback to the old session. The dispatch comment explicitly identifies #1204's prohibition on swallowed callback failures.

**Handoff context:**

- **Current behavior:** A failed nested session reset can leave old-session state behind while its enclosing reconciliation callback reports success.
- **Expected behavior:** Attempt every reset, preserve isolation, and propagate the collected failure identities through the existing unsuccessful reconciliation terminal status.
- **Scope and constraints:** This is #2156/#2238's integration with #1204, not a request for general Lua quarantine, instruction budgets, rollback, or a different ordinary menu/resize error policy.
- **Verification target:** Exercise the production `LuaSaveLoaded` path with the real UI-manager callback and a nested reset fault. Assert other hooks run, the nested owner is identified, and the load has an unsuccessful reconciliation terminal status. Retain normal, idempotent, and pre-bootstrap cases.
- **Deduplication:** Open/closed searches for teardown failures, reconciliation hooks, and save-loaded `pcall` found #2156 and #1204 as antecedents, but no owner for this nested regression. HPA-17's no-issue disposition expressly retains transaction-visible callback errors; it does not waive them.
- **Remaining uncertainty:** Callback success and the swallowed nested error were executed; the resulting engine terminal status follows the complete current caller trace rather than a full engine load reproduction. No ordinary failure frequency is claimed.

## 2. Generated-library publication integrity

### PRR-2. Verify retained payload integrity before reporting unchanged publication

> **Captured note:** PR #2133's idempotent publication branch compares
> descriptor digests after a size-only check of the existing payload. It
> discards valid incoming bytes even when the retained bytes are corrupt.

**Verification:** Used the real packaged Haskell API in a
`withSystemTempDirectory` fixture. Published `PayloadFile "map.bin" "good"`,
overwrote that fixture's payload with the same-sized `"oops"`, and confirmed
`verifyEntryDirectory` rejected its digest. Republishing the original
`"good"` returned `Right (PublishReport PublishedUnchanged [])`; reading
the final payload still returned `"oops"` and deep verification still
failed. The temporary fixture was automatically removed. All 46 existing
generated-library examples passed; their idempotence case uses an intact
existing payload and their corruption case damages staging instead.

**Evidence:**

- `src/World/GeneratedLibrary/Publish.hs:210` — `examineExisting` calls the cheap `readEntryDirectory` at line 213.
- `src/World/GeneratedLibrary/Publish.hs:215` — equal descriptor-derived digests discard verified staging at line 216 and return `PublishedUnchanged` at line 218.
- `src/World/GeneratedLibrary/Entry.hs:49` — the cheap reader derives its digest from the record after checking listed files.
- `src/World/GeneratedLibrary/Entry.hs:126` — `checkListed` checks presence, symlinks, and size, not payload content.
- `src/World/GeneratedLibrary/Entry.hs:68` — the separate deep check already hashes each payload through `checkDigest` at line 148.

Reproduction in `cabal exec -- ghci -v0 -ignore-dot-ghci -package synarchy`:

```haskell
:set -XOverloadedStrings
import qualified World.GeneratedLibrary as L
import World.GeneratedLibrary.Entry
import World.Page.GeneratedId
import qualified Data.ByteString.Char8 as B
import System.IO.Temp
import System.FilePath
withSystemTempDirectory "pr2133-corruption" $ \root -> do { let { cfg = L.defaultLibraryConfig { L.lcRoot = root </> "generated-worlds", L.lcSavesDirectory = root </> "saves" } }; Right lib <- L.openLibrary cfg; gid <- newGeneratedWorldId; let { files = [L.PayloadFile "map.bin" "good"]; path = L.entryDirectory lib gid </> "map.bin" }; L.publishEntry lib gid files >>= print; B.writeFile path "oops"; verifyEntryDirectory (L.entryDirectory lib gid) >>= print; L.publishEntry lib gid files >>= print; B.readFile path >>= print; verifyEntryDirectory (L.entryDirectory lib gid) >>= print }
```

**Handoff context:**

- **Current behavior:** Correct republishing does not repair same-sized corruption and returns unqualified unchanged success.
- **Expected behavior:** Reuse is successful only when the retained payload actually has the required integrity. Valid incoming content must repair an invalid final entry or produce an honest structured failure, never silently preserve corrupt bytes as an unchanged success.
- **Scope and constraints:** Preserve ID stability, pin protection, atomic replacement, recovery, and durability contracts from #2024. Cheap listings need not become deep scans; the correction concerns publication's reuse decision. The library is a foundation with no current worldgen/save/load runtime consumer, so this is not evidence of live gameplay save loss.
- **Verification target:** Corrupt an already-published payload without changing its length, republish the correct bytes under the same ID, and assert both publication disposition and retained byte-level integrity. Keep intact idempotence and replacement/recovery cases.
- **Deduplication:** Open/closed searches for generated-library publication, republish integrity, and `PublishedUnchanged` found #2024 and umbrella #2017, but no separate defect owner; local findings contain no duplicate.
- **Remaining uncertainty:** None about the reproduced API behavior. Runtime integration and ordinary corruption frequency are not measured.

### PRR-3. Reject case aliases of reserved generated-library payload names

> **Captured note:** PR #2133 rejects the literal reserved name
> `entry.record` but accepts `ENTRY.RECORD`. On a case-insensitive filesystem
> the subsequent metadata write replaces that payload, yet publication
> reports success.

**Verification:** In another automatically cleaned macOS temporary root,
called `publishEntry lib gid [PayloadFile "ENTRY.RECORD" "payload"]`.
It returned `Right (PublishReport PublishedNew [])`. Immediate `lookupEntry`
reported `EntryUnreadable`: the payload was 167 bytes although its descriptor
said 7. `verifyEntryDirectory` returned the same failure. This used the real
API and native case-insensitive filesystem, not a mocked path comparison.
The existing 46-example library suite passed; reserved-name tests exercise
the exact lowercase spelling, while a separate test already rejects
case-insensitive duplicate payload names.

**Evidence:**

- `src/World/GeneratedLibrary/Layout.hs:221` — reserved names are checked by exact membership, including `entry.record` at line 226.
- `src/World/GeneratedLibrary/Layout.hs:235` — payload duplicate detection separately uses case folding, so this namespace already recognizes case aliases.
- `src/World/GeneratedLibrary/Publish.hs:153` — payload bytes are verified before the metadata record is written at line 159.
- `src/World/GeneratedLibrary/Publish.hs:192` — `writeRecord` writes the fixed `entry.record` path; this aliases the accepted uppercase payload on the tested filesystem. Its reread verifies the record only, not the now-overwritten payload.
- `src/World/GeneratedLibrary/Publish.hs:212` — a new final directory commits and reports `PublishedNew` without detecting that collision.

Using the same imports and GHCi setup as PRR-2:

```haskell
withSystemTempDirectory "pr2133-reserved" $ \root -> do { Right lib <- L.openLibrary (L.defaultLibraryConfig { L.lcRoot = root </> "generated-worlds", L.lcSavesDirectory = root </> "saves" }); gid <- newGeneratedWorldId; L.publishEntry lib gid [L.PayloadFile "ENTRY.RECORD" "payload"] >>= print; L.lookupEntry lib gid >>= print; verifyEntryDirectory (L.entryDirectory lib gid) >>= print }
```

**Handoff context:**

- **Current behavior:** An accepted payload filename aliases library-owned metadata and publication itself creates an unreadable entry.
- **Expected behavior:** Reject reserved-name aliases before payload writes, using a filename-equivalence policy valid on supported filesystems. Successful publication must not contain a payload overwritten by its own record.
- **Scope and constraints:** Preserve the payload-neutral library and structured validation failure contract. This is separate from PRR-2: it needs neither an existing final entry nor external corruption. No game integration or art changes are required.
- **Verification target:** Cover uppercase and mixed-case variants of reserved names, assert validation refusal before publication, and preserve any existing final entry. Verify every accepted publication passes byte-level entry validation on the supported case-insensitive platform.
- **Deduplication:** Open/closed searches combining `entry.record` with case and library with reserved found only antecedent #2024, umbrella #2017, and unrelated results; local reports and bugs contain no matching defect.
- **Remaining uncertainty:** The overwrite was reproduced on macOS; a case-sensitive Linux filesystem does not alias these two spellings. No production game caller is currently wired to this foundation API.
