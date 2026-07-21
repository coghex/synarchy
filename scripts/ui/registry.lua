-- UI widget introspection oracle (F3, #645).
--
-- Widgets live in scattered per-module Lua tables (button, checkbox,
-- textbox, dropdown, toggle, slider, tabbar, list, label, panel — each
-- with its own private instance table and no shared registry). This
-- module aggregates each module's own dump() accessor into one
-- normalized view for ui.dumpWidgets(), rather than introducing a
-- second, redundant registration path each widget's new()/destroy()
-- would need to keep in sync.
--
-- Oracle-only: read by the playtest harness and its critic (H2) to
-- tell a discoverability bug (the control exists, enabled, on-screen,
-- but the player never found it) from a correctness bug (the control
-- genuinely isn't there). Never fed to the player agent — the player
-- perceives only pixels, by design (see #641).
local registry = {}

local button   = require("scripts.ui.button")
local checkbox = require("scripts.ui.checkbox")
local textbox  = require("scripts.ui.textbox")
local randbox  = require("scripts.ui.randbox")
local dropdown = require("scripts.ui.dropdown")
local toggle   = require("scripts.ui.toggle")
local slider   = require("scripts.ui.slider")
local tabbar   = require("scripts.ui.tabbar")
local uiList   = require("scripts.ui.list")
local label    = require("scripts.ui.label")
local panel    = require("scripts.ui.panel")

local WIDGET_MODULES = {
    button, checkbox, textbox, randbox, dropdown, toggle,
    slider, tabbar, uiList, label, panel,
}

-- Modules whose dump() records are passive context geometry — labels
-- and panels — rather than genuine input controls (#783). Every other
-- module's dump() is a real widget kind (button/checkbox/textbox/...),
-- correlation-eligible even when shown-but-disabled (a widget module's
-- own dump() never gates on info.clickable, only pageVisible/visible).
local PASSIVE_MODULES = { [label] = true, [panel] = true }

-- ui.dumpWidgets() -> JSON array of every currently-rendered UI
-- element (bounds in the same F1/F2 framebuffer-pixel space),
-- aggregated across the widget modules above. Each module's own
-- dump() already excludes widgets on a currently-hidden page or that
-- are themselves (or through a hidden ancestor) not visible.
--
-- Every record carries three fields for click-to-control correlation
-- (#783, repairing #645's original contract): `control` is false ONLY
-- for passive label/panel records — they stay in the dump for critic
-- CONTEXT but must never satisfy a click join, explicit rather than
-- inferred from `type` so a caller can't mistake one for an input
-- surface. `paintKey`/`paintOrder` are the element's engine-side
-- paint/hit-test ordering (UI.getElementInfo's fields of the same
-- name — page-layer-band plus accumulated zIndex, and this element's
-- position in the full paint traversal), exactly what
-- UI.Manager.Query.topHitBy resolves overlapping hits with — including
-- ties, since ordinary same-band/same-zIndex siblings share a
-- `paintKey` and topHitBy breaks that tie by paint order (later wins).
-- A caller ranks by `(paintKey, paintOrder)`, both descending, to pick
-- the same topmost control the UI input router would for a given
-- point, independent of Lua table/module iteration order. Records
-- with no handle (can't happen for a rendered widget, but kept
-- defensive) get paintKey/paintOrder 0.
--
-- Some screens (the main menu, notably) build their clickable elements
-- with raw UI.newBox/UI.setOnClick calls instead of going through a
-- widget module — "box + centered label, no button widget", per
-- main_menu.lua's own comment. Those would otherwise be entirely
-- invisible to this oracle, so as a second pass every element on a
-- visible page is bulk-read from the engine (UI.getVisibleElements)
-- and any INTERACTIVE one (has an onClick/onRightClick callback), that
-- is itself visible and not already reported by a widget module above,
-- is included too. Its label prefers the engine-resolved visible text
-- (UI.getVisibleElements walks the element's own text-render children,
-- e.g. "Create World" for the main menu's create_world_box) and falls
-- back to the engine-side name (e.g. "create_world_box") only when no
-- text child exists (an icon-only control). Every such element is a
-- genuine control (filtered to el.interactive above), so control=true.
function registry.dumpWidgets()
    local out = {}
    local known = {}
    for _, mod in ipairs(WIDGET_MODULES) do
        local isControl = not PASSIVE_MODULES[mod]
        for _, widget in ipairs(mod.dump()) do
            widget.control = isControl
            local info = widget.handle and UI.getElementInfo(widget.handle)
            widget.paintKey = (info and info.paintKey) or 0
            widget.paintOrder = (info and info.paintOrder) or 0
            -- #745 review round 4: keyboard CONTROL focus, distinct
            -- from the pre-existing text-only `focused` each widget
            -- module's own dump() already sets — added centrally here
            -- (like paintKey/paintOrder above) so every widget family
            -- reports it without a per-family dump() change.
            widget.controlFocused = (info and info.controlFocused) or false
            -- #749: the effective (clip-intersected) interactive rect a
            -- real pointer hit resolves against — the phantom-affordance
            -- join (tools/playtest/critic.py's widget_at) prefers this
            -- over the content `bounds` above so a click on a migrated
            -- control's visible border correlates to the control. nil
            -- (widget module dumps with no live handle, or a fully
            -- clipped element) falls back to `bounds`.
            widget.interactiveBounds = info and info.interactiveBounds or nil
            table.insert(out, widget)
            if widget.handle then known[widget.handle] = true end
        end
    end

    for _, el in ipairs(UI.getVisibleElements()) do
        if el.interactive and el.visible and not known[el.handle] then
            table.insert(out, {
                id = "element:" .. el.handle,
                name = el.name,
                type = "button",
                bounds = { x = el.x, y = el.y, w = el.width, h = el.height },
                -- #749: effective interactive rect (see the first pass
                -- above) for the phantom-affordance join.
                interactiveBounds = el.interactiveBounds,
                label = el.text or el.name,
                enabled = el.clickable,
                visible = el.visible,
                hovered = el.hovered,
                focused = el.focused,
                controlFocused = el.controlFocused,
                screen = el.page,
                handle = el.handle,
                control = true,
                paintKey = el.paintKey or 0,
                paintOrder = el.paintOrder or 0,
            })
        end
    end

    return out
end

return registry
