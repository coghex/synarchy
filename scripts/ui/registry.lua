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
local dropdown = require("scripts.ui.dropdown")
local toggle   = require("scripts.ui.toggle")
local slider   = require("scripts.ui.slider")
local tabbar   = require("scripts.ui.tabbar")
local uiList   = require("scripts.ui.list")
local label    = require("scripts.ui.label")
local panel    = require("scripts.ui.panel")

local WIDGET_MODULES = {
    button, checkbox, textbox, dropdown, toggle,
    slider, tabbar, uiList, label, panel,
}

-- ui.dumpWidgets() -> JSON array of every currently-rendered
-- interactive UI element (bounds in the same F1/F2 framebuffer-pixel
-- space), aggregated across the widget modules above. Each module's
-- own dump() already excludes widgets on a currently-hidden page.
--
-- Some screens (the main menu, notably) build their clickable elements
-- with raw UI.newBox/UI.setOnClick calls instead of going through a
-- widget module — "box + centered label, no button widget", per
-- main_menu.lua's own comment. Those would otherwise be entirely
-- invisible to this oracle, so as a second pass every element on a
-- visible page is bulk-read from the engine (UI.getVisibleElements)
-- and any INTERACTIVE one (has an onClick/onRightClick callback) not
-- already reported by a widget module above is included too, with a
-- best-effort label (its engine-side name, e.g. "create_world_box" —
-- text content isn't queryable back from a raw UI.newText element, so
-- this is what's available without one).
function registry.dumpWidgets()
    local out = {}
    local known = {}
    for _, mod in ipairs(WIDGET_MODULES) do
        for _, widget in ipairs(mod.dump()) do
            table.insert(out, widget)
            if widget.handle then known[widget.handle] = true end
        end
    end

    for _, el in ipairs(UI.getVisibleElements()) do
        if el.interactive and not known[el.handle] then
            table.insert(out, {
                id = "element:" .. el.handle,
                name = el.name,
                type = "button",
                bounds = { x = el.x, y = el.y, w = el.width, h = el.height },
                label = el.name,
                enabled = el.clickable,
                visible = el.visible,
                hovered = el.hovered,
                focused = el.focused,
                screen = el.page,
                handle = el.handle,
            })
        end
    end

    return out
end

return registry
