# Player Events

Design for the player-facing event-notification system. Replaces the
informal "engine.logWarn isn't visible to the player" pattern with a
proper events pipeline that the player controls.

Status: design accepted 2026-05-18. Phase 1 ready to implement.

---

## 1. Overview

Engine code (Haskell) and game-logic code (Lua) emit **events**: short
text messages tagged with a **category**. Events are routed to up to
three surfaces, each enabled per-category by player settings:

| Surface       | What happens                                                |
|---------------|-------------------------------------------------------------|
| **Event log** | Event is appended to a ring buffer (~1000 entries)          |
| **Popup**     | A persistent popup with text + OK button is added to a stack |
| **Pause**     | `enginePausedRef = True` is written; player must unpause     |

The player edits per-category checkboxes in a Notifications settings
tab to choose which surfaces fire. Defaults are loaded from a YAML
content registry and saved (per-player) to a config YAML.

**This is not** a replacement for `engine.logWarn` / `logInfo` /
`logError` / `logDebug`. Those remain stdout-only dev tools. Events
are for things the **player** should be aware of.

---

## 2. Design decisions

| # | Decision                                                          |
|---|-------------------------------------------------------------------|
| 1 | Categories are an **open YAML registry**, not a closed enum       |
| 2 | Players **cannot** add new categories at runtime (YAML only)      |
| 3 | **No** severity field; categories carry visuals + filtering       |
| 4 | Categories define `text_color` (RGBA) — affects popup AND log     |
| 5 | Three independent player checkboxes per category: log / popup / pause |
| 6 | Popup and pause are **orthogonal** — dismissing popup does NOT unpause |
| 7 | Popups stack diagonally in 6 slots, then **cycle back** to slot 1  |
| 8 | Popups persist until OK is clicked — no time-based fade            |
| 9 | Event-store ring buffer of ~1000 entries; oldest dropped first    |
| 10| Event log is **per-session** — resets on engine restart           |
| 11| Pause is direct `writeIORef enginePausedRef True` from emitEvent  |
| 12| Unknown category in emit → dev warning, drop event entirely       |
| 13| No event coalescing in Phase 1                                    |
| 14| Popup data model carries a `buttons :: [PopupButton]` field (forward-compat); Phase 1 always uses `[OK → dismiss]` |
| 15| Phase 1 popup text supports `\n` line breaks; auto-wraps at ~40% framebuffer width |
| 16| Phase 1 popup shows only event text — no category header          |

---

## 3. Architecture

```
                            ┌─────────────────┐
                            │  YAML registry  │
                            │  data/...yaml   │
                            └────────┬────────┘
                                     │ loaded at boot
                                     ▼
                            ┌─────────────────┐
                            │ NotificationCfg │
                            │ (HashMap by ID) │
   ┌───────────────────────►│   in EngineEnv  │
   │                        └────────┬────────┘
   │                                 │
┌──┴───────────┐                     │ per-category settings
│ Haskell      │   emitEvent          │
│ subsystems   │────────────────────► │
│ (Save, Unit, │                     │
│  World, …)   │  ┌──────────────────┴──────────────────┐
└──────────────┘  │           emitEvent (env, …)        │
                  │  1. build Event { category, text,   │
┌──────────────┐  │     gameTime, source, buttons }     │
│ Lua scripts  │──┤  2. lookup category in cfg          │
│ via          │  │  3. unknown? logWarn + drop         │
│ engine.emit  │  │  4. if cfg.log    → append to ring  │
│   Event(…)   │  │  5. if cfg.popup  → push to popup queue │
└──────────────┘  │  6. if cfg.pause  → engine paused = T  │
                  └────────────────────┬────────────────┘
                                       │
                ┌──────────────────────┼──────────────────────┐
                ▼                      ▼                      ▼
       ┌─────────────────┐   ┌─────────────────┐   ┌─────────────────┐
       │  Event ring     │   │ Popup queue     │   │ enginePausedRef │
       │  (TVar Seq)     │   │ → broadcast to  │   │   ← True        │
       │  ~1000 entries  │   │   onPopupShow   │   │                 │
       └────────┬────────┘   └────────┬────────┘   └─────────────────┘
                │                     │
                │                     ▼
                │            ┌─────────────────┐
                │            │ scripts/        │
                │            │   popup.lua     │
                │            │ (renders +      │
                │            │  OK dismiss)    │
                │            └─────────────────┘
                │
                │ Phase 2 reader
                ▼
       ┌─────────────────┐
       │ scripts/        │
       │  event_log.lua  │
       │ (hotkey panel)  │
       └─────────────────┘
```

---

## 4. Data model

### 4.1 Event record

```haskell
data Event = Event
  { evCategory :: !Text         -- e.g. "save_load"
  , evText     :: !Text         -- player-visible message
  , evGameTime :: !Double       -- gameTimeRef at emit
  , evSource   :: !Text         -- subsystem tag, e.g. "World.Save"
                                -- (dev-debug only, not displayed in P1)
  , evButtons  :: ![PopupButton] -- forward-compat; P1 = [OK → dismiss]
  } deriving (Show, Eq, Generic)
-- No Serialize derivation: events are per-session, never saved.

data PopupButton = PopupButton
  { pbLabel  :: !Text
  , pbAction :: !PopupAction
  } deriving (Show, Eq, Generic)

data PopupAction
  = ActDismiss
  -- P2 extensions (NOT in P1):
  --   | ActGoTo !Int !Int          -- move camera to grid (gx, gy)
  --   | ActLuaCallback !Text       -- invoke a registered Lua handler
  --   | ActChain ![PopupAction]
  deriving (Show, Eq, Generic)
```

### 4.2 Category registry — `data/notification_categories.yaml`

```yaml
# Registry of player-facing event categories.
# Loaded once at engine boot. Cannot be modified at runtime.
#
# id           — unique snake_case identifier, used by emitEvent
# display_name — user-visible label in the notifications settings tab
# description  — longer text for the future settings-tab tooltip
# text_color   — RGBA list [r, g, b, a] floats 0.0–1.0; applies to BOTH
#                popup text AND event-log entry
# default_settings:
#   log   — default value of "event log"  checkbox
#   popup — default value of "popup"      checkbox
#   pause — default value of "pause"      checkbox

categories:
  - id: save_load
    display_name: Save / Load
    description: Save and load operations succeeded or failed.
    text_color: [1.0, 1.0, 1.0, 1.0]
    default_settings: { log: true,  popup: true,  pause: false }

  - id: survival_critical
    display_name: Survival (Critical)
    description: Unit death, organ failure, dying of thirst or hunger.
    text_color: [1.0, 0.3, 0.3, 1.0]
    default_settings: { log: true,  popup: true,  pause: true  }

  - id: survival_warning
    display_name: Survival (Warning)
    description: Unit hungry, thirsty, or low health.
    text_color: [1.0, 0.8, 0.4, 1.0]
    default_settings: { log: true,  popup: true,  pause: false }

  - id: combat
    display_name: Combat
    description: Combat events between units.
    text_color: [1.0, 0.6, 0.6, 1.0]
    default_settings: { log: true,  popup: true,  pause: false }

  - id: building
    display_name: Building / Construction
    description: Construction progress and completion events.
    text_color: [0.8, 0.9, 1.0, 1.0]
    default_settings: { log: true,  popup: false, pause: false }

  - id: weather
    display_name: Weather
    description: Significant weather changes (storms, seasonal shifts).
    text_color: [0.7, 0.9, 1.0, 1.0]
    default_settings: { log: true,  popup: false, pause: false }

  - id: unit_status
    display_name: Unit Status
    description: Spawning, recruitment, idle state changes.
    text_color: [1.0, 1.0, 1.0, 1.0]
    default_settings: { log: true,  popup: false, pause: false }

  - id: debug
    display_name: Debug
    description: Development events surfaced to the in-game log.
    text_color: [0.6, 0.6, 0.6, 1.0]
    default_settings: { log: false, popup: false, pause: false }
```

### 4.3 Player overrides — `config/notifications.yaml`

Auto-created on first run by copying `default_settings` from the
registry. The player edits it through the settings tab (Phase 2) or
by editing the file directly.

```yaml
categories:
  save_load:         { log: true, popup: true,  pause: false }
  survival_critical: { log: true, popup: true,  pause: true  }
  survival_warning:  { log: true, popup: true,  pause: false }
  combat:            { log: true, popup: true,  pause: false }
  building:          { log: true, popup: false, pause: false }
  weather:           { log: true, popup: false, pause: false }
  unit_status:       { log: true, popup: false, pause: false }
  debug:             { log: false, popup: false, pause: false }
```

If a category appears in the registry but not in this file (e.g. a
new category was added in a later patch), the registry's
`default_settings` are used and the entry is auto-added on next save.

If a category appears in this file but not in the registry, it is
ignored with a dev-log warning.

### 4.4 Resolved settings in memory

```haskell
data CategoryCfg = CategoryCfg
  { ccId          :: !Text
  , ccDisplayName :: !Text
  , ccDescription :: !Text
  , ccTextColor   :: !(Float, Float, Float, Float)
  , ccLog         :: !Bool          -- from player overrides
  , ccPopup       :: !Bool          -- "
  , ccPause       :: !Bool          -- "
  } deriving (Show, Eq, Generic)

-- HashMap keyed by category id, in EngineEnv.
type NotificationCfg = HashMap Text CategoryCfg
```

`NotificationCfg` is loaded at boot and is **immutable for the
session**. Phase 1 doesn't change settings at runtime; Phase 2's
settings-tab UI rewrites `config/notifications.yaml` and reloads.

---

## 5. Phase 1 components

### 5.1 Event store

A bounded ring buffer of ~1000 events.

```haskell
-- In Engine.Core.State:
data EngineEnv = EngineEnv
  { ...
  , eventStoreRef    :: !(TVar (Seq Event))
  , notificationCfg  :: !NotificationCfg  -- immutable after init
  , popupQueueRef    :: !(TVar (Seq Event))  -- pending popups
  }

eventStoreCap :: Int
eventStoreCap = 1000

pushEvent :: TVar (Seq Event) -> Event -> STM ()
pushEvent ref ev = modifyTVar' ref $ \s ->
  let s' = s Seq.|> ev
  in if Seq.length s' > eventStoreCap
     then Seq.drop (Seq.length s' - eventStoreCap) s'
     else s'
```

`TVar (Seq Event)` allows multi-writer (world thread, unit thread,
Lua thread can all emit) with atomic reads from the Lua thread for
the log panel.

### 5.2 Haskell API — `emitEvent`

```haskell
-- src/Engine/Event.hs (new module)

module Engine.Event
  ( Event(..)
  , PopupButton(..)
  , PopupAction(..)
  , emitEvent
  , readEventLog
  ) where

import qualified Data.Sequence as Seq
import Control.Concurrent.STM
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logWarn, LogCategory(..), LoggerState)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import qualified Engine.Core.Queue as Q

-- | Emit a player-visible event. Honors player notification settings:
--   appends to log, queues a popup, and/or pauses the engine.
--
--   If `category` is not in the registry, the event is dropped with
--   a dev-log warning. This is the loud-fail path for typos.
emitEvent :: EngineEnv -> Text -> Text -> Text -> IO ()
emitEvent env category source text = do
  case HM.lookup category (notificationCfg env) of
    Nothing -> do
      logger <- readIORef (loggerRef env)
      logWarn logger CatEvent $
        "emitEvent: unknown category '" <> category <> "' from "
          <> source <> "; event dropped: " <> text

    Just cfg -> do
      now <- readIORef (gameTimeRef env)
      let ev = Event
            { evCategory = category
            , evText     = text
            , evGameTime = now
            , evSource   = source
            , evButtons  = [PopupButton "OK" ActDismiss]  -- P1 default
            }

      when (ccLog cfg) $
        atomically $ pushEvent (eventStoreRef env) ev

      when (ccPopup cfg) $ do
        atomically $ modifyTVar' (popupQueueRef env) (Seq.|> ev)
        Q.writeQueue (luaQueue env) (LuaShowPopup ev)
        -- (Or pass just the fields we need; see §5.5 wire format.)

      when (ccPause cfg) $
        writeIORef (enginePausedRef env) True

readEventLog :: EngineEnv -> IO [Event]
readEventLog env =
  toList <$> readTVarIO (eventStoreRef env)
```

A new `CatEvent` log category is added to `Engine.Core.Log` so the
unknown-category warnings can be filtered separately from `CatLua`.

### 5.3 Lua API — `engine.emitEvent`

```haskell
-- src/Engine/Scripting/Lua/API/Event.hs (new)

emitEventFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
emitEventFn env = do
  catArg  <- Lua.tostring 1
  textArg <- Lua.tostring 2
  case (catArg, textArg) of
    (Just catBS, Just textBS) ->
      Lua.liftIO $ emitEvent env
        (TE.decodeUtf8 catBS)
        "Lua"
        (TE.decodeUtf8 textBS)
    _ -> pure ()
  return 0
```

Registered in `Engine.Scripting.Lua.API` next to existing API
registrations:

```haskell
registerLuaFunction "emitEvent" (emitEventFn env)
```

Lua call shape:

```lua
engine.emitEvent("save_load", "Game saved: " .. saveName)
engine.emitEvent("survival_critical", "Unit X has died of thirst")
```

### 5.4 Popup wire format & Lua subscriber

Engine→Lua message:

```haskell
-- In Engine.Scripting.Lua.Types:
data LuaMsg = ...
            | LuaShowPopup !Text !Text !(Float, Float, Float, Float) ![Text]
            --             category text  text_color                 button labels
            deriving (Eq, Show)
```

The wire format flattens just what the Lua side needs (category for
filtering, text for display, color for styling, button labels for
buttons). `PopupAction` isn't sent — Phase 1 hardcodes dismiss
behavior on the Lua side. Phase 2 needs a richer protocol.

Lua thread broadcast in `Engine.Scripting.Lua.Thread`:

```haskell
LuaShowPopup category text (r,g,b,a) buttons ->
  broadcastToModules ls "onShowPopup"
    [ ScriptString category
    , ScriptString text
    , ScriptNumber (realToFrac r)
    , ScriptNumber (realToFrac g)
    , ScriptNumber (realToFrac b)
    , ScriptNumber (realToFrac a)
    , ScriptStringList buttons
    ]
```

(The exact list-encoding for buttons depends on what
`broadcastToModules` supports — may need to be flattened differently
or passed as a comma-separated string. Implementation detail.)

### 5.5 Popup module — `scripts/popup.lua` (new file)

Responsibilities:
- Subscribe to `onShowPopup`
- Maintain a queue of pending popups
- Place each popup at one of 6 cycling slots (center + 5 stepped
  positions)
- Render the panel + text + OK button per popup
- On OK click, remove the popup; the next queued popup, if any,
  takes the vacated slot

```lua
-- Module state
popup.queue       = {}     -- list of popup records waiting to render
popup.active      = {}     -- list of currently-rendered popups
popup.slotCount   = 6
popup.slotOffset  = 24     -- pixels per stack step (* uiscale)
popup.fbW         = 0
popup.fbH         = 0
popup.boxTexSet   = nil
popup.font        = nil

-- Popup record
-- { id, category, text, color = {r,g,b,a}, buttons = {"OK"},
--   slot = 1..6, panelId, labelId, buttonId,
--   onClickHandlers = { [okBoxHandle] = function() ... end } }

function popup.init(boxTex, font, w, h) ... end

function popup.onShowPopup(category, text, r, g, b, a, buttons)
  table.insert(popup.queue, {
    category = category,
    text     = text,
    color    = {r, g, b, a},
    buttons  = buttons,  -- list of labels
  })
  popup.drainQueue()
end

function popup.drainQueue()
  while #popup.queue > 0 and #popup.active < popup.maxActive do
    local entry = table.remove(popup.queue, 1)
    popup.spawn(entry)
  end
end

-- Slot picker: find shallowest free slot 1..6; if all taken, cycle
-- back to slot 1 (visually overlapping the older popup in that slot).
function popup.pickSlot()
  local occupied = {}
  for _, p in ipairs(popup.active) do
    occupied[p.slot] = (occupied[p.slot] or 0) + 1
  end
  -- Find slot with fewest occupants; ties broken by lower number.
  local bestSlot, bestCount = 1, occupied[1] or 0
  for s = 2, popup.slotCount do
    local n = occupied[s] or 0
    if n < bestCount then bestSlot, bestCount = s, n end
  end
  return bestSlot
end

function popup.spawn(entry)
  local slot = popup.pickSlot()
  local cx, cy = popup.fbW / 2, popup.fbH / 2
  local off = (slot - 1) * popup.slotOffset * scale.get()
  local px, py = cx + off, cy + off

  -- Build panel + wrapped text + OK button at (px, py).
  -- Register click handler on OK that calls popup.dismiss(p).
  -- Z-index: later popups render on top of earlier ones in the same slot.

  local p = { ... }
  table.insert(popup.active, p)
end

function popup.dismiss(p)
  -- Destroy UI elements, remove from active, drain queue if waiting.
end

function popup.update(dt)
  -- Phase 1: nothing to do per-frame (popups are persistent).
  -- Phase 2 might tick fade-in animations etc.
end

function popup.shutdown() ... end
```

**Memory bound for the queue**: cap pending popups (not yet rendered)
at e.g. 50 entries. Beyond that, drop oldest. Avoids unbounded
accumulation if hundreds of events fire while the player is AFK with
popups stacked. Events are still in the log buffer.

### 5.6 init.lua hook

```lua
-- Add to init() in scripts/init.lua:
popup = require("scripts.popup")
popup.init(boxTexSet, menuFont, fbW, fbH)
-- The onShowPopup broadcast will reach popup module automatically
-- because broadcastToModules iterates all loaded scripts.
```

### 5.7 Settings loader

```haskell
-- In Engine.Asset.YamlNotifications (new module) or similar:
loadNotificationCfg :: FilePath -> FilePath -> IO NotificationCfg
loadNotificationCfg registryPath overridesPath = do
  registry <- decodeFileThrow registryPath  -- data/notification_categories.yaml
  overrides <- tryDecode overridesPath       -- config/notifications.yaml
  let merged = mergeOverrides registry (fromMaybe HM.empty overrides)
  -- If overridesPath didn't exist, write defaults so the file exists.
  unless (isJust overrides) $
    writeOverrides overridesPath (defaultsFromRegistry registry)
  pure merged
```

Loaded once at engine boot, stashed in `EngineEnv.notificationCfg`.

---

## 6. Phase 2 — sketches (not implementing in this round)

### 6.1 Event log panel

Hotkey opens a panel showing the ring buffer. Newest at top.
Per-entry: timestamp (game-time), category display_name (with
`text_color`), event text.

Filter UI: checkbox list of categories. Hide entries from unchecked
categories.

Scrolling: standard scroll panel over up to 1000 entries.

Implementation: `scripts/event_log.lua`, reads `engine.getEventLog()`
(a new Lua-API function returning the full log as a Lua table).

### 6.2 Notifications settings tab

A tab in the existing settings menu. For each category in the
registry, a row showing display_name + three checkboxes
(log / popup / pause) + a tooltip area showing description on hover.

Writes `config/notifications.yaml` on close. Reload of
`notificationCfg` happens via a queued engine message
(`LuaReloadNotificationCfg`); active popups and log entries are
unaffected.

### 6.3 Multi-button popups

The data model already has `evButtons :: [PopupButton]` and the
`PopupAction` ADT. Phase 2 adds:

- API surface: `emitEventWith :: EngineEnv -> Text -> Text -> Text -> [PopupButton] -> IO ()` 
  (and Lua equivalent)
- Wire protocol: extend `LuaShowPopup` to carry buttons with actions,
  not just labels
- New `PopupAction` variants:
  - `ActGoTo Int Int` — Lua side moves camera to grid (gx, gy)
  - `ActLuaCallback Text` — invokes a named Lua function with the popup id
  - `ActChain [PopupAction]` — fire multiple actions in sequence
- Popup module renders all buttons in a horizontal row at the bottom

Example call site once Phase 2 lands:

```lua
engine.emitEventWith(
  "survival_critical",
  "Unit Bob has died at (102, 47)",
  { { label = "Go To", action = { kind = "go_to", gx = 102, gy = 47 } },
    { label = "OK",    action = { kind = "dismiss" } } })
```

### 6.4 Keyboard shortcuts

- `Esc` — dismiss topmost active popup
- `Shift+Esc` — dismiss all active popups
- Hotkey (TBD; `L`?) — toggle event log panel

### 6.5 Coalescing rules

When multiple events fire from the same category within a short
window (e.g. 200ms), optionally collapse to "N events: …". Per-category
opt-in via a `coalesce_window` field in the YAML registry.

Examples:
- `survival_critical` 5 simultaneous deaths → "5 units died of thirst"
- `combat` rapid swings → single "Combat in progress" + log of detail

### 6.6 Future actions / buttons

- **Go To** — pan camera to the event location (needs `(gx, gy)` in
  the emit call). Useful for unit deaths, building completions.
- **Pin** — keep popup until manually closed (overrides any future
  auto-dismiss logic)
- **Open Info** — for unit events, open the unit info panel
- **Dismiss Type** — dismiss this popup AND silence this category
  for the rest of the session (one-click "don't show me again")

---

## 7. Migration of existing call sites

Phase 1 rewires a small set of currently-`engine.logXxx` calls that
should be player-facing. Everything else stays as dev-log.

| Site                                          | Today                       | After migration                                             |
|-----------------------------------------------|-----------------------------|-------------------------------------------------------------|
| `world_view.lua:saveGame` success             | `engine.logInfo`            | `engine.emitEvent("save_load", "Game saved: " .. name)`     |
| `world_view.lua:saveGame` failure             | `engine.logWarn`            | `engine.emitEvent("save_load", "Save failed: " .. name)`    |
| `main_menu.lua:loadAndShowSave` failure       | inline label + `engine.logError` | inline label kept; ALSO emitEvent for log history    |
| `saveWorldFn` validation failures (Save.hs)   | `logWarn logger CatLua`     | `emitEvent env "save_load" "World.Save" "Save failed: ..."` |
| `handleWorldSaveCommand` Right/Left at `Save.hs:128-135` | `logInfo`+`sendGenLog` | replace `sendGenLog` calls with `emitEvent`; keep `logInfo` |

Everything else stays:
- `scripts/build_tool.lua` logs ("BuildTool: placed …") — dev info, stays
- `scripts/world_manager.lua` logs ("Creating world: …") — dev info, stays  
- `scripts/flora_loader.lua` / `scripts/building_loader.lua` logs — dev info, stays
- `pause.lua` "Game paused/resumed" log — dev info, stays
- `scripts/lib/save_modules.lua` pcall-warns — dev only, stays
- `collectLuaBlobs` / `restoreLuaBlobs` warnings — dev only, stays
- `listSaves` corrupt-save skips — dev only (Phase 2 may surface in log panel)
- Orphan IDs on load (`Save.hs:275-279, 296-300`) — already uses
  `sendGenLog` for visible summary; that's fine, keep it

---

## 8. Phase 1 implementation plan

### 8.1 Files

**New:**
- `src/Engine/Event.hs` — `Event`/`PopupButton`/`PopupAction` types,
  `emitEvent`, `readEventLog`
- `src/Engine/Asset/YamlNotifications.hs` — loader for the two YAMLs
- `src/Engine/Scripting/Lua/API/Event.hs` — Lua API binding
- `data/notification_categories.yaml` — content registry
- `scripts/popup.lua` — popup display module
- `docs/player_events.md` — this file

**Modified:**
- `src/Engine/Core/State.hs` — add `eventStoreRef`, `notificationCfg`,
  `popupQueueRef` to `EngineEnv`
- `src/Engine/Core/Init.hs` — initialize the above at boot
- `src/Engine/Core/Log.hs` — add `CatEvent` log category
- `src/Engine/Scripting/Lua/Types.hs` — add `LuaShowPopup` constructor
- `src/Engine/Scripting/Lua/Thread.hs` — broadcast case for
  `LuaShowPopup`
- `src/Engine/Scripting/Lua/API.hs` — register `emitEventFn`
- `scripts/init.lua` — load `scripts/popup.lua`, init it
- `scripts/world_view.lua` — migrate `saveGame` calls
- `scripts/main_menu.lua` — migrate `loadAndShowSave` failure path
- `src/World/Thread/Command/Save.hs` — migrate validation-failure and
  save-result paths
- `src/Engine/Scripting/Lua/API/Save.hs` — migrate `saveWorldFn`
  validation warnings to events

### 8.2 Implementation order

1. **Types + state plumbing** — `Event.hs`, `EngineEnv` extensions,
   `LuaMsg` constructor, broadcast wiring. No behavior yet.
2. **YAML loader + init** — load registry + overrides at boot, stash
   `notificationCfg`. Auto-create overrides file on first run.
3. **`emitEvent` implementation** — log path + popup path + pause path,
   gated by category cfg. Don't migrate call sites yet.
4. **Lua API binding** — `engine.emitEvent` registered, smoke-testable
   via the debug console.
5. **`scripts/popup.lua`** — popup rendering, OK click, slot cycling,
   queue draining. Init from `init.lua`.
6. **Smoke tests** — extend `tools/test_lua_save_api.sh` with popup
   assertions.
7. **Migrate call sites** — flip the 5-ish call sites in §7. Verify
   end-to-end.

### 8.3 Estimated size

| Component                          | LOC (Haskell) | LOC (Lua) |
|------------------------------------|---------------|-----------|
| Types, EngineEnv, broadcast        | ~50           | —         |
| YAML loader + auto-defaults        | ~80           | —         |
| `emitEvent` body                   | ~40           | —         |
| Lua API binding                    | ~25           | —         |
| `data/notification_categories.yaml`| ~50 yaml      | —         |
| `scripts/popup.lua`                | —             | ~200      |
| `init.lua` registration            | —             | ~5        |
| Migrated call sites (~5 sites)     | ~10           | ~10       |
| **Total**                          | **~205**      | **~215**  |

Plus ~30 lines of smoke-test assertions and the doc itself.

### 8.4 Testing

End-to-end via `tools/test_lua_save_api.sh`:

```bash
echo "[N] engine.emitEvent dispatches to log + popup"
# Emit a save_load event; verify it appears in log.
lua "engine.emitEvent('save_load', 'smoke-test event')" > /dev/null
# Check ring buffer contains the event
assert_eq "event in log" "true" "engine.getEventLog and #engine.getEventLog() > 0"

echo "[N] survival_critical pauses the engine"
lua "engine.setPaused(false)" > /dev/null
sleep 0.3
assert_eq "engine not paused" "false" "engine.isPaused()"
lua "engine.emitEvent('survival_critical', 'fake death')" > /dev/null
sleep 0.3
assert_eq "engine paused"     "true"  "engine.isPaused()"

echo "[N] unknown category drops with dev warning"
# Should not crash and should not add to log
local n = #engine.getEventLog()
lua "engine.emitEvent('does_not_exist', 'should drop')" > /dev/null
assert_eq "log unchanged"     "true"  "#engine.getEventLog() == " .. n
```

Manual verification: spawn the engine in GUI mode, trigger save/load
failures, confirm popups appear and dismiss correctly. The smoke
test can't verify popup *visuals*, but it can verify the event store
state and the pause side effect.

---

## 9. Non-goals (out of scope)

- **Replacing `engine.logXxx`** — those stay as dev-only stdout logs.
- **Localization** — event text is freeform English. If/when L10n
  happens, events would gain a key + interpolation. Future work.
- **Sound effects** — no audio cues for events in Phase 1 or 2.
- **Notification priorities** — the three checkboxes per category are
  the only player-facing knobs. No "urgent" override that ignores
  player settings.
- **Network / remote events** — single-player only.
- **Sub-categories / hierarchies** — flat namespace. If granularity
  is needed (e.g. `combat.melee` vs `combat.ranged`), split into
  two top-level categories.
- **Action callbacks across save/load** — popups don't persist; if a
  popup with a Phase 2 "Go To" button is showing when the game is
  saved, it's gone on load. No replay.

---

## 10. Open questions

None that block Phase 1. Listed here for future-reference if they
come up:

- Should the event log persist to disk on engine shutdown for
  post-mortem debugging? (Currently: no, per-session only.)
- Should there be a max-stack-depth on popup re-cycling (e.g. cap at
  3 popups visible in any single slot)? Phase 2 might benefit.
- Player-defined custom categories via in-game UI? Currently YAML-only.
