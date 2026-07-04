-- Crafting Station Bills Panel (#330)
--
-- The player-facing view onto the #329 craft-bill queue: a station
-- panel with a "Recipes" browser (add a standing order) and a
-- "Queue" list (the station's current craft.getBills(bid) — remaining
-- count, progress, claimant, cancellable). The craft AI (#329) does
-- the actual claiming/fetching/working; this panel only reads/writes
-- the bill queue via craft.addBill / craft.cancelBill.
--
-- Opened by right-click → "Bills" on any building whose def offers a
-- craft operation (init.lua's building right-click handler, alongside
-- the existing cargo "Contents" entry). Mounted on hud.world_page,
-- same lifecycle as cargo_inventory_panel / item_contents_panel —
-- registered in ui/view_teardown.lua so a view transition can't leave
-- it open-but-invisible.
--
-- Bill "modes" supported: a fixed count (blank-vs-numeric textbox, per
-- recipe row) or repeat-forever (blank count) — exactly the two modes
-- craft.addBill itself understands. Deliberately NOT implemented,
-- because the backend has no data for them (adding it is out of this
-- issue's "UI on the existing bill model" scope):
--   * "Until-count" mode — there's no stock-target tracking anywhere
--     in Craft.Bills; addBill only knows a fixed remaining count or
--     forever. Needs its own design (issue filed as a follow-up).
--   * Pause — CraftBill carries no paused flag; releaseBill only
--     forces an unclaim, which the AI just re-claims on its next scan.
--   * Manual reorder — bills already run oldest-first
--     (billsForStation sorts by id) and there's no priority field to
--     rewrite.
--
-- Singleton via package.loaded (engine.loadScript uses dofile).
-- Public API: setup(opts), show(bid), closeIfOpen(), isOpen(),
--             handleKeyDown(key), recipesForStation(bid) [testable],
--             formatRecipeSummary(def), parseCount(text).
-- Engine hooks: init, update(dt), shutdown.

local craftingPanel = package.loaded["scripts.crafting_panel"] or {}
package.loaded["scripts.crafting_panel"] = craftingPanel

local panel   = require("scripts.ui.panel")
local label   = require("scripts.ui.label")
local button  = require("scripts.ui.button")
local textbox = require("scripts.ui.textbox")
local scale   = require("scripts.ui.scale")

-----------------------------------------------------------
-- Layout constants (base units; uiscale applied at draw time)
-----------------------------------------------------------
local PANEL_W_FRAC = 0.66
local PANEL_H_FRAC = 0.72
local PAD_X        = 28
local PAD_TOP      = 24
local PAD_BOT      = 20
local TITLE_FONT   = 20
local TITLE_H      = 26
local CLOSE_BTN_SZ = 32
local HEADER_FONT  = 15
local HEADER_H     = 20
local NAME_FONT    = 15
local SUMMARY_FONT = 12
local ROW_H        = 46      -- recipe row: name+controls line + summary line
local QROW_H       = 26      -- queue row: single line
local ROW_GAP      = 6
local COL_GAP      = 24
local COUNT_TB_W   = 56
local COUNT_TB_H   = 26
local ADD_BTN_W    = 56
local ADD_BTN_H    = 26
local CANCEL_BTN_SZ = 22
local MAX_RECIPE_ROWS = 12
local MAX_QUEUE_ROWS  = 12
local REFRESH_INTERVAL = 1.0
local MAX_NAME_CHARS = 22

local TITLE_COL   = { 1.0, 1.0, 1.0, 1.0 }
local HEADER_COL  = { 0.8, 0.8, 0.8, 1.0 }
local NAME_COL    = { 1.0, 1.0, 1.0, 1.0 }
local SUMMARY_COL = { 0.75, 0.75, 0.75, 1.0 }
local EMPTY_COL   = { 0.6, 0.6, 0.6, 1.0 }
local ERROR_COL   = { 1.0, 0.45, 0.45, 1.0 }
local CLAIMED_COL = { 0.65, 0.9, 0.65, 1.0 }
local UNCLAIMED_COL = { 0.75, 0.75, 0.75, 1.0 }

-----------------------------------------------------------
-- State
-----------------------------------------------------------
craftingPanel.hud = nil

craftingPanel.state = craftingPanel.state or {
    open          = false,
    bid           = nil,
    panelId       = nil,
    elements      = {},   -- chrome + recipe-column {kind, id}
    queueElements = {},   -- queue-column {kind, id}, rebuilt on refresh
    layout        = nil,  -- {rightX, rightW, contentTop} for refreshQueue
    statusLabelId = nil,
    refreshTimer  = 0,
}

craftingPanel._itemNames = nil

-----------------------------------------------------------
-- HUD hookup
-----------------------------------------------------------
function craftingPanel.setup(opts)
    craftingPanel.hud = opts
end

-----------------------------------------------------------
-- Helpers (pure-ish: only touch engine read-only globals, no UI.*
-- calls — exercisable directly from the debug console for testing)
-----------------------------------------------------------

local function itemDisplayName(defName)
    if not craftingPanel._itemNames then
        craftingPanel._itemNames = {}
        for _, d in ipairs(item.listDefs() or {}) do
            craftingPanel._itemNames[d.name] = d.displayName
        end
    end
    return craftingPanel._itemNames[defName] or defName
end

local function formatIngredients(list)
    local parts = {}
    for _, ing in ipairs(list or {}) do
        parts[#parts + 1] = string.format("%d %s", ing.count,
            itemDisplayName(ing.item))
    end
    return table.concat(parts, " + ")
end

-- Recipes this station's operations offer, craft-only (repair-tagged
-- recipes are refused by craft.addBill and don't belong in this
-- queue). Sorted by id for a stable listing order.
function craftingPanel.recipesForStation(bid)
    local ops = building.getOperations(bid)
    if not ops or #ops == 0 then return {} end
    local opSet = {}
    for _, op in ipairs(ops) do opSet[op] = true end
    local out = {}
    for _, rid in ipairs(craft.getNames() or {}) do
        local def = craft.get(rid)
        if def and opSet[def.station] and not def.repairAxis then
            out[#out + 1] = def
        end
    end
    table.sort(out, function(a, b) return a.id < b.id end)
    return out
end

-- "2 Iron Ore + 1 Coal (fuel)  ->  1 Steel Bar"
function craftingPanel.formatRecipeSummary(def)
    local text = formatIngredients(def.inputs)
    if def.fuel then
        text = text .. string.format(" + %d %s (fuel)", def.fuel.count,
            itemDisplayName(def.fuel.item))
    end
    return text .. "  ->  " .. formatIngredients(def.outputs)
end

-- "12" -> 12, "" / "0" / "-3" / "abc" -> nil. nil is craft.addBill's
-- own convention for an omitted/non-positive count: repeat forever.
function craftingPanel.parseCount(text)
    local n = tonumber(text)
    if not n then return nil end
    n = math.floor(n)
    if n < 1 then return nil end
    return n
end

local function truncate(text, maxChars)
    text = text or ""
    if #text <= maxChars then return text end
    return string.sub(text, 1, maxChars - 2) .. ".."
end

local function claimantName(uid)
    if not uid then return nil end
    local info = unit.getInfo(uid)
    if not info then return "unit_" .. tostring(uid) end
    if info.name and info.name ~= "" then return info.name end
    if info.displayName and info.displayName ~= "" then return info.displayName end
    return info.defName or ("unit_" .. tostring(uid))
end

-----------------------------------------------------------
-- Teardown
-----------------------------------------------------------
local function destroyElementList(list)
    for _, e in ipairs(list) do
        if e.kind == "label" then label.destroy(e.id)
        elseif e.kind == "button" then button.destroy(e.id)
        elseif e.kind == "textbox" then textbox.destroy(e.id)
        end
    end
end

local function destroyQueue()
    local s = craftingPanel.state
    destroyElementList(s.queueElements)
    s.queueElements = {}
end

local function destroyAll()
    local s = craftingPanel.state
    destroyQueue()
    destroyElementList(s.elements)
    s.elements = {}
    s.statusLabelId = nil
    if s.panelId then
        panel.destroy(s.panelId)
        s.panelId = nil
    end
    s.layout = nil
end

-----------------------------------------------------------
-- Status line (add-bill error feedback)
-----------------------------------------------------------
local function setStatus(text, isError)
    local s = craftingPanel.state
    if not s.statusLabelId then return end
    label.setText(s.statusLabelId, text or "")
    label.setColor(s.statusLabelId, isError and ERROR_COL or SUMMARY_COL)
end

-----------------------------------------------------------
-- Queue column (rebuilt on: open, add, cancel, and a periodic timer —
-- kept separate from the recipe column so a live-progress refresh
-- never wipes a count textbox the player is mid-typing into)
-----------------------------------------------------------
local function renderQueue()
    local s = craftingPanel.state
    local h = craftingPanel.hud
    local L = s.layout
    if not h or not L then return end
    destroyQueue()

    local uiscale = scale.get()
    local rowH = math.floor(QROW_H * uiscale)
    local rowGap = math.floor(ROW_GAP * uiscale)
    local x = L.rightX
    local w = L.rightW
    local y = L.contentTop

    local bills = craft.getBills(s.bid) or {}

    if #bills == 0 then
        local id = label.new({
            name = "crafting_panel_queue_empty", text = "No bills queued yet.",
            font = h.menuFont, fontSize = SUMMARY_FONT, color = EMPTY_COL,
            page = h.page, uiscale = uiscale, x = x, y = y + rowH,
        })
        table.insert(s.queueElements, { kind = "label", id = id })
        return
    end

    local nameW   = math.floor(w * 0.38)
    local remW    = math.floor(w * 0.12)
    local progW   = math.floor(w * 0.14)
    local cancelW = math.floor(CANCEL_BTN_SZ * uiscale)
    local claimW  = w - nameW - remW - progW - cancelW - math.floor(12 * uiscale)

    local shown = math.min(#bills, MAX_QUEUE_ROWS)
    for i = 1, shown do
        local bill = bills[i]
        local rowY = y + (i - 1) * (rowH + rowGap)
        local def = craft.get(bill.recipe)
        local recipeName = truncate(def and def.name or bill.recipe, MAX_NAME_CHARS)
        local remaining = (bill.remaining < 0) and "inf" or tostring(bill.remaining)
        local pct = string.format("%d%%", math.floor((bill.progress or 0) * 100 + 0.5))
        local who = claimantName(bill.claimant)

        local nameId = label.new({
            name = "crafting_panel_q_name_" .. i, text = recipeName,
            font = h.menuFont, fontSize = NAME_FONT, color = NAME_COL,
            page = h.page, uiscale = uiscale,
            x = x, y = rowY + math.floor(NAME_FONT * uiscale),
        })
        table.insert(s.queueElements, { kind = "label", id = nameId })

        local remId = label.new({
            name = "crafting_panel_q_rem_" .. i, text = remaining,
            font = h.menuFont, fontSize = NAME_FONT, color = SUMMARY_COL,
            page = h.page, uiscale = uiscale,
            x = x + nameW, y = rowY + math.floor(NAME_FONT * uiscale),
        })
        table.insert(s.queueElements, { kind = "label", id = remId })

        local progId = label.new({
            name = "crafting_panel_q_prog_" .. i, text = pct,
            font = h.menuFont, fontSize = NAME_FONT, color = SUMMARY_COL,
            page = h.page, uiscale = uiscale,
            x = x + nameW + remW, y = rowY + math.floor(NAME_FONT * uiscale),
        })
        table.insert(s.queueElements, { kind = "label", id = progId })

        local claimId = label.new({
            name = "crafting_panel_q_claim_" .. i,
            text = truncate(who or "--", 14),
            font = h.menuFont, fontSize = NAME_FONT,
            color = who and CLAIMED_COL or UNCLAIMED_COL,
            page = h.page, uiscale = uiscale,
            x = x + nameW + remW + progW,
            y = rowY + math.floor(NAME_FONT * uiscale),
        })
        table.insert(s.queueElements, { kind = "label", id = claimId })

        local billId = bill.id
        local cancelId = button.new({
            name = "crafting_panel_q_cancel_" .. i, text = "X",
            x = x + w - cancelW, y = rowY,
            width = CANCEL_BTN_SZ, height = CANCEL_BTN_SZ,
            fontSize = 14, uiscale = uiscale,
            page = h.page, font = h.menuFont,
            textureSet = h.boxTexSet,
            bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
            zIndex = 202,
            onClick = function()
                craft.cancelBill(billId)
                renderQueue()
            end,
        })
        table.insert(s.queueElements, { kind = "button", id = cancelId })
    end

    if #bills > MAX_QUEUE_ROWS then
        local moreId = label.new({
            name = "crafting_panel_q_more", text = "...and "
                .. (#bills - MAX_QUEUE_ROWS) .. " more",
            font = h.menuFont, fontSize = SUMMARY_FONT, color = EMPTY_COL,
            page = h.page, uiscale = uiscale,
            x = x, y = y + shown * (rowH + rowGap) + math.floor(SUMMARY_FONT * uiscale),
        })
        table.insert(s.queueElements, { kind = "label", id = moreId })
    end
end
craftingPanel.refreshQueue = renderQueue

-----------------------------------------------------------
-- Full rebuild (chrome + recipe column + queue column)
-----------------------------------------------------------
local function rebuild(bid)
    local h = craftingPanel.hud
    if not h or not h.page then return end
    destroyAll()

    local s = craftingPanel.state
    local uiscale = scale.get()
    local fbW, fbH = h.fbW, h.fbH
    local panelW = math.floor(fbW * PANEL_W_FRAC)
    local panelH = math.floor(fbH * PANEL_H_FRAC)
    local panelX = math.floor((fbW - panelW) / 2)
    local panelY = math.floor((fbH - panelH) / 2)

    s.panelId = panel.new({
        name = "crafting_panel", page = h.page,
        x = panelX, y = panelY, width = panelW, height = panelH,
        textureSet = h.boxTexSet, color = { 0.1, 0.1, 0.1, 0.92 },
        tileSize = 64, zIndex = 200,
        padding = { top = PAD_TOP, bottom = PAD_BOT, left = PAD_X, right = PAD_X },
        uiscale = uiscale,
    })
    local b = panel.getContentBounds(s.panelId)
    local cx = panelX + b.x
    local cy = panelY + b.y
    local cw = b.width

    local info = building.getInfo(bid)
    local title = (info and info.displayName or "Station") .. " -- Bills"
    local titleId = label.new({
        name = "crafting_panel_title", text = title,
        font = h.menuFont, fontSize = TITLE_FONT, color = TITLE_COL,
        page = h.page, uiscale = uiscale,
        x = cx, y = cy + math.floor(TITLE_FONT * uiscale),
    })
    table.insert(s.elements, { kind = "label", id = titleId })

    local closeSz = math.floor(CLOSE_BTN_SZ * uiscale)
    local closeId = button.new({
        name = "crafting_panel_close", text = "X",
        x = cx + cw - closeSz,
        y = cy,
        width = CLOSE_BTN_SZ, height = CLOSE_BTN_SZ,
        fontSize = 16, uiscale = uiscale,
        page = h.page, font = h.menuFont, textureSet = h.boxTexSet,
        bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
        zIndex = 202,
        onClick = function() craftingPanel.closeIfOpen() end,
    })
    table.insert(s.elements, { kind = "button", id = closeId })

    local colGap = math.floor(COL_GAP * uiscale)
    local leftW = math.floor((cw - colGap) * 0.58)
    local rightW = cw - leftW - colGap
    local leftX = cx
    local rightX = cx + leftW + colGap
    local headerY = cy + math.floor(TITLE_H * uiscale) + math.floor(6 * uiscale)

    local recHeaderId = label.new({
        name = "crafting_panel_rec_header", text = "Recipes",
        font = h.menuFont, fontSize = HEADER_FONT, color = HEADER_COL,
        page = h.page, uiscale = uiscale,
        x = leftX, y = headerY + math.floor(HEADER_FONT * uiscale),
    })
    table.insert(s.elements, { kind = "label", id = recHeaderId })

    local queueHeaderId = label.new({
        name = "crafting_panel_queue_header", text = "Queue",
        font = h.menuFont, fontSize = HEADER_FONT, color = HEADER_COL,
        page = h.page, uiscale = uiscale,
        x = rightX, y = headerY + math.floor(HEADER_FONT * uiscale),
    })
    table.insert(s.elements, { kind = "label", id = queueHeaderId })

    local contentTop = headerY + math.floor(HEADER_H * uiscale)
        + math.floor(4 * uiscale)
    local rowH = math.floor(ROW_H * uiscale)
    local rowGap = math.floor(ROW_GAP * uiscale)

    local recipes = craftingPanel.recipesForStation(bid)
    if #recipes == 0 then
        local emptyId = label.new({
            name = "crafting_panel_rec_empty",
            text = "No recipes offered by this station.",
            font = h.menuFont, fontSize = SUMMARY_FONT, color = EMPTY_COL,
            page = h.page, uiscale = uiscale,
            x = leftX, y = contentTop + rowH,
        })
        table.insert(s.elements, { kind = "label", id = emptyId })
    else
        local shown = math.min(#recipes, MAX_RECIPE_ROWS)
        local tbW = math.floor(COUNT_TB_W * uiscale)
        local btnW = math.floor(ADD_BTN_W * uiscale)
        for i = 1, shown do
            local def = recipes[i]
            local rowY = contentTop + (i - 1) * (rowH + rowGap)

            local nameId = label.new({
                name = "crafting_panel_rec_name_" .. i,
                text = truncate(def.name, MAX_NAME_CHARS),
                font = h.menuFont, fontSize = NAME_FONT, color = NAME_COL,
                page = h.page, uiscale = uiscale,
                x = leftX, y = rowY + math.floor(NAME_FONT * uiscale),
                tooltip = craftingPanel.formatRecipeSummary(def),
            })
            table.insert(s.elements, { kind = "label", id = nameId })

            local summaryId = label.new({
                name = "crafting_panel_rec_summary_" .. i,
                text = truncate(craftingPanel.formatRecipeSummary(def), 46),
                font = h.menuFont, fontSize = SUMMARY_FONT, color = SUMMARY_COL,
                page = h.page, uiscale = uiscale,
                x = leftX,
                y = rowY + math.floor(NAME_FONT * uiscale)
                    + math.floor((SUMMARY_FONT + 4) * uiscale),
            })
            table.insert(s.elements, { kind = "label", id = summaryId })

            local tbId = textbox.new({
                name = "crafting_panel_rec_count_" .. i,
                width = COUNT_TB_W, height = COUNT_TB_H,
                x = leftX + leftW - tbW - btnW - math.floor(8 * uiscale),
                y = rowY,
                page = h.page, font = h.menuFont, fontSize = 16,
                textType = textbox.Type.NUMBER,
                zIndex = 201,
            })
            table.insert(s.elements, { kind = "textbox", id = tbId })

            local recipeId = def.id
            local capturedTb = tbId
            local addId = button.new({
                name = "crafting_panel_rec_add_" .. i, text = "Add",
                x = leftX + leftW - btnW, y = rowY,
                width = ADD_BTN_W, height = ADD_BTN_H,
                fontSize = 14, uiscale = uiscale,
                page = h.page, font = h.menuFont, textureSet = h.boxTexSet,
                bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
                zIndex = 201,
                onClick = function()
                    local count = craftingPanel.parseCount(
                        textbox.getValue(capturedTb))
                    local billId, err = craft.addBill(bid, recipeId, count)
                    if billId then
                        textbox.setText(capturedTb, "")
                        setStatus("", false)
                        renderQueue()
                    else
                        setStatus(err or "could not add bill", true)
                    end
                end,
            })
            table.insert(s.elements, { kind = "button", id = addId })
        end

        if #recipes > MAX_RECIPE_ROWS then
            local moreId = label.new({
                name = "crafting_panel_rec_more",
                text = "...and " .. (#recipes - MAX_RECIPE_ROWS) .. " more",
                font = h.menuFont, fontSize = SUMMARY_FONT, color = EMPTY_COL,
                page = h.page, uiscale = uiscale,
                x = leftX,
                y = contentTop + shown * (rowH + rowGap)
                    + math.floor(SUMMARY_FONT * uiscale),
            })
            table.insert(s.elements, { kind = "label", id = moreId })
        end
    end

    local statusY = panelY + panelH - PAD_BOT - math.floor(4 * uiscale)
    s.statusLabelId = label.new({
        name = "crafting_panel_status", text = "",
        font = h.menuFont, fontSize = SUMMARY_FONT, color = SUMMARY_COL,
        page = h.page, uiscale = uiscale, x = leftX, y = statusY,
    })
    table.insert(s.elements, { kind = "label", id = s.statusLabelId })

    s.layout = { rightX = rightX, rightW = rightW, contentTop = contentTop }
    renderQueue()
end

-----------------------------------------------------------
-- Public lifecycle
-----------------------------------------------------------

-- Open (or refresh) the panel for a station building id. Refused if
-- the building is gone, or the assets aren't ready yet.
function craftingPanel.show(bid)
    if not bid or not craftingPanel.hud then return end
    if not building.getInfo(bid) then return end
    local s = craftingPanel.state
    s.bid = bid
    s.open = true
    s.refreshTimer = 0
    rebuild(bid)
end

function craftingPanel.closeIfOpen()
    local s = craftingPanel.state
    if not s.open then return end
    destroyAll()
    s.open = false
    s.bid = nil
end

function craftingPanel.isOpen()
    return craftingPanel.state.open == true
end

-- Esc closes the popup. Returns true if consumed (init.lua's Escape
-- cascade, same tier as cargo_inventory_panel / item_contents_panel).
function craftingPanel.handleKeyDown(key)
    if key == "Escape" and craftingPanel.state.open then
        craftingPanel.closeIfOpen()
        return true
    end
    return false
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------
function craftingPanel.init(scriptId)
    engine.logInfo("Crafting panel initializing...")
end

-- Periodic queue refresh (progress/claimant/remaining tick without a
-- button click) plus an auto-close if the station was demolished
-- while the panel was open. The recipe column never rebuilds here —
-- only renderQueue — so an in-progress count textbox is never wiped.
function craftingPanel.update(dt)
    local s = craftingPanel.state
    if not s.open then return end
    if not building.getInfo(s.bid) then
        craftingPanel.closeIfOpen()
        return
    end
    s.refreshTimer = s.refreshTimer + dt
    if s.refreshTimer >= REFRESH_INTERVAL then
        s.refreshTimer = 0
        renderQueue()
    end
end

function craftingPanel.shutdown()
    craftingPanel.closeIfOpen()
    engine.logInfo("Crafting panel shut down")
end

return craftingPanel
