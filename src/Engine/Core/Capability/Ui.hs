-- | The UI\/focus\/HUD half of the @ui-hud-events@ capability (epic
--   #537, issue #897 — E7a): exactly the four fields
--   'docs/engineenv_capability_inventory.md' SS7.7 splits off from the
--   event\/notification\/popup half (#898), in SS5's own table order.
--
--   Follows the capability-record convention
--   ('docs/engineenv_capability_inventory.md' SS2.1 is its one
--   authoritative statement, not restated here).
--
--   == Field prefix
--
--   The convention prefixes fields with the record's own initials,
--   appending a @c@ for a single-word name (@cc@\/@rc@\/@ic@ for
--   @Core@\/@Render@\/@Input@). That would make this record's prefix
--   @uc@ — already 'Engine.Core.Capability.UnitCombat.UnitCombatCapability'\'s
--   (#895). Two capability records sharing one prefix would be
--   actively misleading in any module that holds both, so this record
--   uses __@uic@__ — @ui@ plus the same trailing @c@.
--
--   == No thread-private field, so no split record
--
--   Unlike @render-gpu-asset@ (SS3.1) and @input-lua-transport@
--   (SS7.3), this capability owns nothing one thread privately owns:
--   SS5 records a reader and a writer on more than one thread for
--   'uicUiManagerRef' and 'uicFocusManagerRef', and the two
--   single-role fields ('uicHudActivePageRef' — @WorldThread@;
--   'uicTextBuffersRef' — read on @LuaThread@, written on
--   @MainRender@) are ordinary session\/boot state, not a
--   thread-private allocator or handoff slot. So there is one record
--   here, not a main-only\/worker-safe pair, and
--   @tools/engine_env_capability_audit.py@ needs no import boundary
--   for it beyond the SS6 ratchet.
--
--   == Concurrency contract these handles carry (SS5)
--
--   'uicUiManagerRef' is genuinely multi-writer —
--   @LuaThread@\/@InputThread@\/@WorldThread@\/@MainRender@ all mutate
--   it — and every writer uses @atomicModifyIORef'@ rather than a
--   read\/modify\/write pair. The input thread's keyboard and
--   character dispatch validate focus and control focus inside ONE
--   such atomic transition precisely because they race the Lua
--   thread's concurrent element mutations (#745). Projecting this
--   record changes none of that: it hands out the same container, so
--   the atomicity discipline lives at the call sites exactly as
--   before.
--
--   == Lifecycle (SS5, and @World.Load.Publish.resetTransientState@)
--
--   Three of the four are @session-replaced@ and ARE reset by a load
--   publish — 'uicUiManagerRef' (text focus + control focus cleared),
--   'uicFocusManagerRef' (current focus cleared, registered targets
--   kept) and 'uicHudActivePageRef' (reset to 'Nothing', then
--   resynced from @wmVisible@). 'uicTextBuffersRef' is
--   @boot-process@: the scene-object text map is NOT touched by
--   @resetTransientState@, and its entries follow their scene objects'
--   own lifetimes instead. That coupling is enforced in
--   "Engine.Scripting.Lua.Message.Scene" and nowhere else, which states
--   the invariant in full and routes all four transitions through one
--   pair of helpers: an entry exists exactly when the active scene
--   graph holds a TEXT node at that id (#1961). Note the transition
--   that is easy to miss — 'Engine.Scene.Graph.addNode' is a
--   @Map.insert@, so spawning a SPRITE over a live text node replaces
--   it and retires its entry too. Those are the map's only writers and
--   only removers, so a session boundary has nothing left to reset.
--
--   Like the other capability modules, this one imports only the
--   narrow slice of @Engine.Core.State@ it needs (the bare 'EngineEnv'
--   type plus its four field accessors) rather than @EngineEnv(..)@ or
--   a bare import, so it is not itself a full-@EngineEnv@-access
--   consumer under the SS6 ratchet.
module Engine.Core.Capability.Ui
  ( UiCapability(..)
  , toUiCapability
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Data.IORef (IORef)
import Engine.Scene.Base (ObjectId)
import UI.ShellFocus (FocusManager)
import UI.Types (UIPageManager)
import World.Types (WorldPageId)
import Engine.Core.State
  ( EngineEnv
  , uiManagerRef, focusManagerRef, hudActivePageRef, textBuffersRef
  )

-- | The UI\/focus\/HUD slice of @ui-hud-events@: the whole UI page
--   tree (pages, elements, text\/control focus, tooltip state — the
--   editable-widget @UI.TextBuffer@s among them, carried on each
--   element as @ueTextBuffer@ inside 'uicUiManagerRef' and reached by
--   @ElementHandle@, never by 'ObjectId'), the
--   Lua-facing focus-target registry, the page id the global HUD info
--   panel currently reflects, and — a separate mechanism entirely —
--   the SCENE-OBJECT text cache keyed by scene 'ObjectId' that
--   @engine.getText@ answers from. See
--   'docs/engineenv_capability_inventory.md' SS5 @ui-hud-events@ and
--   SS7.7.
data UiCapability = UiCapability
  { -- | Multi-writer via @atomicModifyIORef'@ across
    --   @LuaThread@\/@InputThread@\/@WorldThread@\/@MainRender@;
    --   @session-replaced@ (a load publish clears text and control
    --   focus, and Lua rebuilds the tree).
    uicUiManagerRef      ∷ IORef UIPageManager
    -- | Shell\/console TEXT focus ("UI.ShellFocus"), not either
    --   game-UI focus system. @InputThread@ only READS it
    --   (@Thread.Keyboard@\/@Thread.Char@, to decide whether a
    --   keystroke belongs to the debug console); @LuaThread@ reads and
    --   writes it (@API.ShellFocus@). #745's Tab\/Shift+Tab
    --   control-focus traversal never touches this ref — it transitions
    --   'uicUiManagerRef'. @session-replaced@ — a load publish clears
    --   the CURRENT focus only, keeping registered targets.
  , uicFocusManagerRef   ∷ IORef FocusManager
    -- | @WorldThread@ only (@World.Thread.Cursor@'s HUD
    --   refresh-on-active-world-change, #129); @session-replaced@.
  , uicHudActivePageRef  ∷ IORef (Maybe WorldPageId)
    -- | Read on @LuaThread@ (@API.Text@), written on @MainRender@
    --   (@Engine.Scripting.Lua.Message.Scene@, dispatched by
    --   @processLuaMessages@ — never the Lua thread itself);
    --   @boot-process@, and deliberately NOT reset by
    --   @World.Load.Publish.resetTransientState@ because entries are
    --   created and removed with their scene nodes instead (#1961).
    --   The scene-OBJECT text cache, not editable-widget text.
  , uicTextBuffersRef    ∷ IORef (Map.Map ObjectId Text)
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toUiCapability ∷ EngineEnv → UiCapability
toUiCapability env = UiCapability
  { uicUiManagerRef     = uiManagerRef env
  , uicFocusManagerRef  = focusManagerRef env
  , uicHudActivePageRef = hudActivePageRef env
  , uicTextBuffersRef   = textBuffersRef env
  }
