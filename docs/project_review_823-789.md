# Project Review Findings: PRs #823–#789

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #823, #821, #820, #819, #810, #818, #817, #809, #808, #804, #803, and #789 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #821's input-thread split, #820's ordered immutable CI-image publication, #810's blood-texture teardown, #818's movement-probe demotion, #817's plan-keyed cache bound, #809's Unicode-safe editable widgets, #808's state-of-mind probe isolation, #803's starvation wording, and #789's generated-language v1 contract retain their intended behavior in the current tree. No separate current concern was found for those PRs.

## Status

- [x] PRR-1. Direct tile selection leaves both deferred cursor-selection arms live — [#1702]
- [x] PRR-2. Nested structure content can stamp beyond a location's authoritative bounds — [#1708]
- [x] PRR-3. Neutral legacy placeholders become permanent local settings on every first boot — [#1937]
- [x] PRR-4. Partial notification overrides migrate successfully by turning omitted fields off — [#1938]

## 1. Direct tile selection versus deferred cursor arms

### [#1702] PRR-1. Direct tile selection leaves both deferred cursor-selection arms live

> **Captured note:** Give direct tile selection the same authoritative disarm rule as direct chunk selection. A synchronous `world.selectTile` should clear both old render-time selection arms before either renderer can retarget or erase the tile it just committed.

**Verification:** Partially verified structurally. The direct chunk handler added by PR #823 explicitly clears both `zoomSelectNow` and `worldSelectNow`, and its comment explains why either stale arm can clobber a new direct selection. The older direct tile handler performs the opposing-selection clear but leaves both arms untouched. Both arming APIs remain publicly registered, so a Lua caller can arm a deferred selection, then make a direct tile selection before the relevant render pass consumes the arm. No current production script still drives the arming APIs, which makes this a latent public-API race rather than a reproduced player-facing path today.

**Evidence:**

- Issue #813 / PR #823 identified a direct selection as authoritative over any still-pending deferred arm. `src/World/Thread/Command/Cursor/Select.hs:223-236` explains that a stale zoom arm can later clobber the fresh selection and a stale tile arm can later clear its opposing selection.
- `src/World/Thread/Command/Cursor/Select.hs:239-248` implements that rule for chunks by setting both `zoomSelectNow = False` and `worldSelectNow = False` in the same atomic update as `zoomSelectedPos`.
- `src/World/Thread/Command/Cursor/Select.hs:187-208` implements the symmetric direct tile operation, but updates only `worldSelectedTile` and `zoomSelectedPos`. It preserves the prior values of both arm flags.
- `src/World/Thread/Command/Cursor/Select.hs:45-61` and `:106-117` still provide real commands that set the zoom and tile arms independently.
- `src/World/Render/Zoom/Cursor.hs:70-109` consumes a surviving `zoomSelectNow` by committing the current hover chunk and clearing `worldSelectedTile`.
- `src/World/Render/CursorQuads.hs:106-126` consumes a surviving `worldSelectNow` by replacing `worldSelectedTile` with the current hover result and, on a successful commit, clearing `zoomSelectedPos`.
- `src/Engine/Scripting/Lua/API/Register/World.hs:50,65` still registers `world.setZoomCursorSelect` and `world.setWorldCursorSelect`; these are not dead internal constructors even though repository search finds no live production-script call after the click paths moved to direct selection.
- `test-headless/Test/Headless/World/SelectChunk.hs:240-276` pins the fresh-chunk-over-both-arms rule. There is no symmetric direct-tile-over-both-arms regression case.
- Tracker searches found the closed ownership issue #135 and click-snapshot issue #813, but no follow-up for the direct tile handler's asymmetric disarming.

**Handoff context:**

- **Current behavior:** A caller can arm `worldSelectNow`, then queue `world.selectTile`; the direct handler commits the requested tile, but the next tile render consumes the old arm and retargets selection to whatever is currently under hover. A surviving zoom arm can similarly commit later and erase the fresh tile selection.
- **Expected behavior:** A successful direct tile selection atomically clears both deferred arms, sets the requested tile, and clears the opposing chunk selection. A later render may update hover state but cannot reinterpret an already accepted direct selection.
- **Scope and constraints:** Surfaced in PR #823 / issue #813. Preserve page scoping, exact picked-z selection from #367, the #135 newest-selection rule, off-map no-op semantics, and the lower-level deferred APIs for callers that deliberately want render-time hover resolution.
- **Remaining uncertainty:** The repository's current HUD and context-menu callers use `pickTile` plus `selectTile` without first arming either legacy path. The processor should decide whether a still-exposed Lua primitive is enough current surface to file, or whether this waits for a production caller or an explicit API deprecation decision.

## 2. Nested location geometry

### [#1708] PRR-2. Nested structure content can stamp beyond a location's authoritative bounds

> **Captured note:** Validate the full footprint of nested structure content, not only its anchor offset. Passing the outer definition's bounds to a builder relocated within those same bounds translates the entire outer-sized footprint a second time.

**Verification:** Verified structurally; latent in current authored data. The YAML loader accepts a fixed content position whenever the point itself lies inside the location bounds. A `kind: structure` entry then invokes its builder at that offset while passing the outer definition unchanged. Both available structure builders expand the full outer bounds around their received anchor, so any nonzero offset shifts part of their geometry outside the bounds advertised for placement, discovery, portals, and map annotation. The shipped `ruin_small` definition currently has no nested structure entry, so this is an accepted-schema defect awaiting authored use rather than a malformed current ruin.

**Evidence:**

- Issue #777 requires one authoritative box shared by structure geometry and downstream spatial consumers, and requires fixed-position content to remain valid only inside that box.
- `src/Engine/Asset/YamlLocations.hs:171-203` rejects inverted bounds and checks each fixed content `position` with point containment only. The validation does not branch on `lycKind` or account for a nested builder's footprint.
- `scripts/locations.lua:453-468` resolves `kind: structure`, computes an offset within the outer definition, and calls the nested builder at `(gx + ox, gy + oy)` while passing the same outer `def` and its bounds.
- `scripts/locations.lua:198-228` implements `room_small` by adding `def.bounds.min/max` to the anchor it receives and stamping that whole rectangle. `room_small_damaged` repeats the same bounds translation at `:263-269` before stamping its full geometry.
- With outer bounds `(-2,-2)..(2,2)`, a nested `room_small` at valid fixed position `(2,0)` stamps x offsets `0..4` relative to the location anchor. Half of that nested room lies beyond the advertised `max_x = 2` even though the YAML passes validation.
- Randomly positioned structure content has the same issue: `contentOffset` may choose any point in the bounds, then the nested builder expands the bounds again around that point.
- Repository data search finds no current `kind: structure` entry under `data/locations/`; only `ruin_small` and its top-level `room_small_damaged` builder are authored today.
- Tracker and findings-report searches found no issue owning nested-builder footprint validation or the double translation of location bounds.

**Handoff context:**

- **Current behavior:** The schema accepts nested structures whose geometry can extend beyond the exact box every downstream location query treats as authoritative. Placement separation can admit overlap, discovery can ignore visibly stamped tiles, and portal/map logic can reason from a smaller footprint than the world mutation.
- **Expected behavior:** Every accepted content entry has a footprint contained by the parent location's bounds. Nested builders either declare their own relative footprint for containment validation or use coordinates/geometry that do not reinterpret the parent's bounds around a shifted anchor.
- **Scope and constraints:** Surfaced in PR #819 / issue #777. Preserve the current top-level `ruin_small` 5×5 geometry, deterministic scatter, cylindrical coordinate handling, and the single authoritative spatial contract; avoid reintroducing a separate hardcoded room radius.
- **Remaining uncertainty:** No shipped definition currently exercises nested structure content, and the intended semantics of that older content kind are not documented beyond the Lua comments. The processor should verify whether it is a supported authoring feature; if not, rejecting/removing the kind may be cleaner than expanding the bounds model.

## 3. Neutral migration and future defaults

### [#1937] PRR-3. Neutral legacy placeholders become permanent local settings on every first boot

> **Captured note:** Do not promote a byte-for-byte neutral legacy placeholder into durable player state. A player who never saved settings should continue inheriting later tracked video/keybinding defaults instead of being pinned forever to the defaults from their first post-migration boot.

**Verification:** Partially verified as a product-contract problem; the filesystem behavior is direct. PR #804 deliberately tracks neutral legacy files so the migration source always exists, then blindly copies a valid source whenever the local file is absent. Consequently every clean checkout creates full `video.local.yaml` and `keybinds.local.yaml` files on first boot, and all later boots prefer those copies over the tracked defaults. Initial values are correct because the two pairs are currently identical; the unresolved judgment is whether future default changes are intended to reach players who have never made a settings choice.

**Evidence:**

- Issue #786 requires fresh clones to continue using the current versioned video/keybinding templates, while issue #638's underlying split made local state win only after a user Save.
- `src/Engine/Core/Init.hs:82-110` says the tracked legacy files are neutral placeholders whose purpose is to guarantee a readable migration source. It also says a failed/missing migration falls back to versioned defaults.
- `src/Engine/Core/Init.hs:111-125` checks only whether the local path exists, validates the legacy file, and copies it verbatim. It does not compare a legacy placeholder with the current template or record whether the local file represents an actual user choice.
- `src/Engine/Core/Init.hs:169-180` runs that migration before resolving keybindings and video. The resulting local file immediately wins over `keybinds_default.yaml` or `video_default.yaml`.
- `config/video.yaml` and `config/video_default.yaml` are currently byte-equivalent in values; `config/keybinds.yaml` and `config/keybinds_default.yaml` are currently identical. That proves why first-boot values look correct, not why the generated local copies should outrank a later template revision.
- `tools/config_migration_probe.py:190-224` requires a boot against the real committed neutral files to create all three local files. Its idempotence phase then requires the migrated local value to win permanently, so the gate currently enforces this promotion.
- The notification placeholder is `categories: {}` and therefore continues to defer absent categories to registry defaults; the freeze is specifically the full video and keybinding documents copied into local state.
- Tracker searches found closed #638/#786 but no later issue deciding how untouched neutral migrations interact with changed shipped defaults.

**Handoff context:**

- **Current behavior:** The first boot of an uncustomized checkout turns the then-current full video and keybinding defaults into gitignored local state. A later release can change the tracked defaults, but that player keeps the older values despite never pressing Save or editing legacy settings.
- **Expected behavior:** Real pre-#786 player preferences migrate and remain authoritative, while a neutral placeholder remains equivalent to “no player state” across later default revisions. A first actual Save may then materialize durable local state normally.
- **Scope and constraints:** Surfaced in PR #804 / issue #786. Preserve recovery of genuinely modified legacy files, existing-local precedence, idempotence, git-clean runtime writes, and actionable fallback logging. Comparing the legacy file to the neutral/default document or recording migration provenance are possible directions, not prescribed solutions.
- **Remaining uncertainty:** The repository may intentionally define “first boot” as implicit acceptance of all current defaults, even without a Save action. If maintainers want that policy, this should be dispositioned no-issue and the #638/#786 docs should state that shipped default changes are non-migrating after first boot.

## 4. Partial notification migration

### [#1938] PRR-4. Partial notification overrides migrate successfully by turning omitted fields off

> **Captured note:** Treat field-sparse legacy notification entries as incomplete migration input, or overlay their missing fields onto the registry defaults. A present category with one authored checkbox currently receives `false` for the other two before the migration validator ever sees it.

**Verification:** Verified structurally. `migrateLegacyConfig` validates notifications with the same permissive `OverridesFile` decoder used by the runtime loader. That decoder accepts a missing `categories` map and accepts each missing `log`, `popup`, or `pause` field by substituting `False`. The migrated entry then replaces the registry's entire checkbox triple. A partial file therefore neither fails safely nor preserves omitted defaults; it silently converts omission into an explicit opt-out.

**Evidence:**

- Issue #786 explicitly requires “missing, malformed, or partial legacy state” to fail safely with a valid fallback, and its acceptance requires the upgrade probe to prove malformed/partial legacy behavior.
- `src/Engine/Asset/YamlNotifications.hs:29-33` parses every `CategorySettings` field with `.:? ... .!= False`, so `{ log: true }` is accepted as `{ log: true, popup: false, pause: false }`.
- `src/Engine/Asset/YamlNotifications.hs:76-83` also treats an absent top-level `categories` field as an empty map; notification migration therefore has almost no schema-incomplete state distinct from malformed YAML.
- `src/Engine/Core/Init.hs:111-125` regards any successful `FromJSON OverridesFile` decode as valid and copies it verbatim to `notifications.local.yaml`.
- `src/Engine/Asset/YamlNotifications.hs:140-154` uses registry defaults only when the category key is absent. Once a partial category decoded into the overrides map, its whole false-filled triple replaces the registry defaults.
- `data/notification_categories.yaml:30-41` gives `survival_critical` defaults `{ log: true, popup: true, pause: true }`. A valid legacy entry containing only `survival_critical: { log: true }` migrates and silently disables both its critical popup and pause behavior.
- `tools/config_migration_probe.py:315-346` tests malformed legacy input only for video. Its notification phase uses a complete `debug` triple, and `test-headless/Test/Headless/Core/ConfigState.hs:128-161` tests malformed/schema-incomplete migration with a synthetic type whose field is required. Neither gate exercises the real permissive notification schema.
- Historical documentation acknowledges that sparse notification entries migrate successfully, but does not account for omitted fields becoming false instead of retaining registry defaults.
- Tracker searches found only the closed source issue #786, not a follow-up owning this unmet partial-notification acceptance case.

**Handoff context:**

- **Current behavior:** A syntactically valid partial legacy notification document is copied and made authoritative. Omitted checkbox fields on a present category become false, which can suppress critical popups or automatic pause without a warning.
- **Expected behavior:** Migration either rejects partial category triples and falls back loudly, as #786 requests, or defines a key-level overlay in which omitted fields inherit that category's registry defaults. The validator and runtime loader agree on the chosen meaning.
- **Scope and constraints:** Surfaced in PR #804 / issue #786. Preserve complete legacy overrides, unknown-category warnings, default materialization, settings round trips, and the ability for a player to explicitly save `false`; add coverage using a real `OverridesFile` fixture with one or two omitted fields.
- **Remaining uncertainty:** The permissive parser predates PR #804 and may intentionally treat hand-authored partial files as explicit false values. That policy conflicts with #786's partial-migration wording; the processor should decide whether migration needs a stricter decoder than ordinary runtime loading or whether both paths should adopt overlay semantics.
