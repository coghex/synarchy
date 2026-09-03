# scripts/ — Lua game logic and UI

Loaded when you work under `scripts/`. `Engine.Scripting.Lua.API.*`
exposes engine functionality to Lua; `engine.loadScript` paths are
relative to the repo root.

## Module layout and budgets

`scripts/unit_ai.lua` (#538) is the unit-AI entry/orchestration module
only (singleton registration, tunables/action-registry wiring, per-unit
dispatch, engine lifecycle). Every domain's utility/execute bodies live
in `scripts/unit_ai_*.lua` submodules, each capped at 500 lines
(`tools/lua_module_budget.py`, in CI and `make ci`). Shared plumbing is
in `unit_ai_core.lua`; the inventory→ground→mule materials-sourcing
ladder in `unit_ai_fetch.lua`. Submodules reach the shared singleton via
`package.loaded["scripts.unit_ai"]`; public API functions stay attached
to the `unitAi` table from whichever submodule owns them. The 500-line
limit is a per-split ratchet for families listed in the budget tool,
not a tree-wide size policy.

## Random streams (#1330)

**`math.random` is GAMEPLAY's stream.** Its per-state entropy is
established once by `Lua.openlibs` before `scripts/init.lua` loads, and
eleven gameplay modules draw from it — so **nothing under `scripts/` may
call `math.randomseed`** (reseeding makes two engines launched in the
same second share one simulation), and non-gameplay code keeps its OWN
stream: `scripts/ui/random.lua` (SplitMix64) is the UI widget kit's.
Gate: hspec `--match "random stream ownership"`. History:
`docs/engine_contracts.md` §Lua random streams.

## Lua-owned persistent state

Lua state persists via `scripts/lib/save_modules.lua`
(`saveModules.register(id, spec)` — versioned
snapshot/decode/validate/apply, dependency-ordered, `required` vs
optional-with-`default`; `registerResetHook` for non-durable modules),
with canonical data-only payloads from `scripts/lib/data_codec.lua`
(decoding never executes code). A required component's failure aborts
the whole save/load. Typed persistent references are Lua
`{__ref=kind, id=N}` tables (`unit_ai_save_refs.lua`,
`building_spawn.lua`). Read `docs/persistence_contract.md` before adding
a module to the registry; `tools/persistence_inventory_audit.py` fails
when a new module lacks a classification row.

## Page qualification

The AI finders page-qualify every candidate against the ACTING unit
(`scripts/unit_ai_page.lua`) instead of trusting the active page that
`unit.getAllIds` / `building.getActiveIds` / `craft.getBills` each
snapshot separately, and revalidate every PERSISTED building reference
(`deliveryClaim.bid`, `craftJob.bid`, `repairJob.bid`) before it can
steer a walk or reach a verb. The lax AI cargo verbs refuse a
cross-page endpoint pair (#1673). Gate: hspec `--match "AI page pairing"`.

## UI contracts

`UI.*` (Haskell) handles focus, text input and rendering; layout and
behavior are driven from here. Regression suites: `Test.Headless.UI.*`.

**Text coordinates:** `UI.TextBuffer`/`UI.getCursor`/`UI.setCursor` use
zero-based Unicode code-point offsets. Lua strings are UTF-8 byte
arrays — editable widgets must use `scripts/ui/utf8_safe.lua`, never
`#text` or byte-based `string.sub`. The debug console's own input line
(`scripts/shell.lua`) holds the same contract on
`cursorPos`/`inputScrollOffset` and every derived path — tab completion
(which snaps its byte-wise agreement point back to a character
boundary), the ghost hint, and the scroll/measure walk. The Delete key
arrives as `onTextDelete` (`LuaTextDelete`), not `onDelete`; a
`config/shell_history.txt` line that isn't valid UTF-8 is dropped at
load. Gate: hspec `--match "Lua.ShellInput"`.

**Text display (#1159):** read-only DISPLAY paths — wrapping,
truncation, and any other per-character walk — advance one code point
at a time, or non-ASCII text renders as mojibake and gets measured once
per byte. Lua PATTERNS are byte-oriented too, so `gmatch(".")` is a byte
loop. Pixel-width wrapping goes through `scripts/ui/text_wrap.lua` —
`byCharacter` (the debug console) and `byWord` (all three log panels) —
rather than a fourth private copy. Unlike `utf8_safe`, it never raises
on malformed UTF-8 and never drops a byte. Gate: hspec
`--match "Lua.TextWrapping"`.

**Pointer, scroll, and focus routing (#742–#749):** the six contracts
are in `docs/engine_contracts.md` §UI input routing — read it before
touching hit-testing, activation, clipping, or wheel handling. On sight:

- Pages live on six `UILayer`s; `uiLayerBand` is the single paint-order
  source of truth for BOTH hit-testing and rendering. Whether a page
  blocks input is the separate per-page `upInputExclusive` flag
  (`LayerModal` defaults exclusive); the topmost visible exclusive page
  owns the modal boundary, and empty modal space consumes. `LayerDebug`
  is pass-through above any modal.
- Click callback, pointer-blocking, and scroll-capture are three
  independent per-element policies; wheel routing picks the topmost
  in-scope scroll-capturing surface by the same paint-order walk, never
  the click machinery.
- Plain and Shift wheel go through the IDENTICAL engine pipeline —
  don't reintroduce `UI.isInputBlocked()` self-gates in Lua handlers;
  the engine decides once, upstream.
- Press→release activation is epoch-guarded; unrelated sibling/child
  churn must never cancel an activation — don't "simplify" it back to a
  global counter. Keyboard CONTROL focus is independent of text focus
  and reports as `controlFocused`.
- `UI.Clipping.effectiveClip` is the ONE helper rendering and
  hit-testing both consult; `UI.placePopup` is the one placement
  algorithm for floating content.
- All hit-testing uses the INTERACTIVE rect
  (`UI.InteractiveBounds.interactiveRect`); visible overflow never
  enlarges a target unless opted in via `UI.setInteractiveOverflow`.

**Container window stack (#1238/#1250/#2155):**
`scripts/cargo_inventory_panel.lua` is THE container window and the sole
public singleton, and owns an ordered STACK of levels, not one popup.
Since #2155 it is a FAÇADE over two focused owners:
`scripts/cargo_inventory_endpoints.lua` (the building/unit ENDPOINTS
table, the remembered-knowledge/age/weight/empty presentation, the five
shared `endpoint*` helpers, the `endpoint` level kind) and
`scripts/cargo_inventory_render.lua` (layout constants, header
baselines and labels, item-list parameter completion, pane measurement
and placement, element teardown, row menus, scroll capture — level-kind
agnostic). Dependency direction is one-way: the façade imports both, the
renderer imports the endpoint owner for the single-owned `ageText` it
must measure against, and NEITHER extracted module imports the façade or
lets endpoint policy import the renderer (the tab spec and row colour are
injected as values). Only the façade is engine-loaded; the other two are
`require`-only and define no `on*` function. Two windows never coexist at one level: opening container B where
A is open REPLACES A and discards every deeper level, and an EXTERNAL
request always targets the base. Only the DEEPEST level is interactive,
and nothing enforces that by hand — a level past the base gets its own
`LayerModal` page, so #742's boundary makes every shallower level
painted-but-unclickable. Escape closes ONE level per press. The stack is
transient session UI: `hud.createUI()` snapshots and restores the whole
thing across a resize, and `uiManager.onSaveLoaded` drops it. Level
kinds, descent, pane semantics, `paneWidgetName`, and the teardown
REASONS (`"layout"` being the one that does not fire `onClose`):
`docs/engine_contracts.md` §Container window stack. Gates: hspec
`--match "container window stack"` / `"Container knowledge"` /
`"Nested item contents"` / `"Item list widget"` /
`"Transfer context menu"` / `"cargo_inventory_panel"`, plus
`tools/item_list_widget_probe.py` (manual-only, `needs-gpu`).

**Responsive lifecycle (#748 menus / #750 gameplay):**
`scripts/ui/responsive.lua` owns the supported envelope (formal minimum
800x600); out-of-envelope combinations degrade best-effort — never
crash, never invalid geometry, fixed actions stay reachable. Menu
screens register via `responsive.register` + `responsive.notifyResize`;
gameplay surfaces stay OFF that registry (they're reached through
`ui_manager_boot.lua`'s manual forward or the engine's automatic
`broadcastToModules` resize — registering a broadcast-reached module
DOUBLE-FIRES it). For any new screen/panel: a geometry rebuild must
preserve state a semantic re-entry may reset, and restores must not
re-fire `onChange`/`onSelect`; keyboard control focus survives rebuilds
BY NAME; panel content derives from the panel's REAL bounds, never an
independently recomputed value; zIndex ACCUMULATES through the parent
chain, so leave wrapper/viewport elements at 0; hud rebuilds first and
dependent surfaces `reflow()` after it. Full rules:
`docs/engine_contracts.md` §Responsive UI lifecycle.

**Headless UI tests** use a bare Lua backend + synthetic texture/font
handles (`engine.getTextWidth` returns 0 there — stub it when a test
needs real measurement); the shared fixture wipes `package.loaded`
between cases. The full `ui_manager` boot never runs headless (it gates
on `fontsReady`, which needs a GPU font atlas) — use `--offscreen` for
end-to-end UI verification.

## Gameplay contracts touched from Lua

Roles, crafting and bills, power, farming, construction, position hold,
transfers and the expedition verbs each have a section in
`docs/engine_contracts.md`; the root `CLAUDE.md` §Domain contracts lists
the rule to know on sight and the gate for each.
