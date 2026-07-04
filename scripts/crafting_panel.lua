-- Crafting Station Bills Panel (#330)
--
-- The player-facing view onto the #329 craft-bill queue: a station
-- panel with a "Recipes" browser (add a standing order — fixed count,
-- repeat-forever, or "until" a target ground-stock level) and a
-- "Queue" list (the station's current craft.getBills(bid) — remaining
-- count, progress, claimant, pause/reorder/cancel). The craft AI
-- (#329) does the actual claiming/fetching/working; this panel only
-- reads/writes the bill queue via the craft.* verbs.
--
-- Opened by right-click → "Bills" on any building whose def offers a
-- craft operation (init.lua's building right-click handler, alongside
-- the existing cargo "Contents" entry). Mounted on hud.world_page,
-- same lifecycle as cargo_inventory_panel / item_contents_panel —
-- registered in ui/view_teardown.lua so a view transition can't leave
-- it open-but-invisible.
--
-- Bill modes: fixed count and repeat-forever call craft.addBill
-- directly (blank/zero/negative count = repeat forever, its own
-- convention). "Until" mode is computed client-side at Add time: it
-- reads the CURRENT ground stock of the recipe's first output
-- (item.listGround(), summed by defName — the same "dropped at the
-- station" pile the craft AI's fetch ladder sources from, #329), and
-- queues an ordinary fixed-count bill for max(0, target - current).
-- This is a snapshot, not a continuously-reappraised target — the
-- engine has no persisted "stock target" concept, and this needed no
-- new one to satisfy "make until I have N" for the common case of
-- topping up a pile once. Recipe rows also show a ground-stock
-- readiness dot using the same tally, with the same "ground only, not
-- the full carried/technomule/cargo fetch ladder" scope note.
--
-- Pause and manual reorder are real backend features (Craft.Bills'
-- cbPaused / cbSeq, save v73) — see craft.setBillPaused /
-- craft.reorderBill.
--
-- Recipes and bills both paginate (Prev/Next) rather than hard-cap,
-- so every recipe and every queued bill stays reachable regardless of
-- how many there are.
--
-- Singleton via package.loaded (engine.loadScript uses dofile).
-- Public API: setup(opts), show(bid), closeIfOpen(), isOpen(),
--             handleKeyDown(key), recipesForStation(bid) [testable],
--             formatRecipeSummary(def), parseCount(text),
--             groundStockTally(), recipeAvailability(def, tally).
-- Engine hooks: init, update(dt), shutdown.

local craftingPanel = package.loaded["scripts.crafting_panel"] or {}
package.loaded["scripts.crafting_panel"] = craftingPanel

local panel    = require("scripts.ui.panel")
local label    = require("scripts.ui.label")
local button   = require("scripts.ui.button")
local textbox  = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local scale    = require("scripts.ui.scale")

-----------------------------------------------------------
-- Layout constants (base units; uiscale applied at draw time)
-----------------------------------------------------------
local PANEL_W_FRAC = 0.72
local PANEL_H_FRAC = 0.74
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
local ROW_H        = 48      -- recipe row: name+controls line + summary line
local QROW_H       = 30      -- queue row: single line, several small buttons
local ROW_GAP      = 6
local COL_GAP      = 22
local DOT_SZ       = 10
local UNTIL_CB_SZ  = 16
local COUNT_TB_W   = 50
local COUNT_TB_H   = 24
local ADD_BTN_W    = 44
local ADD_BTN_H    = 24
local CANCEL_BTN_SZ = 20
local REORDER_BTN_SZ = 18
local PAUSE_BTN_W  = 52
local PAUSE_BTN_H  = 20
local PAGE_BTN_W   = 26
local PAGE_BTN_H   = 22
local PAGER_H      = 28
local REFRESH_INTERVAL = 1.0
local MAX_NAME_CHARS = 20

local TITLE_COL     = { 1.0, 1.0, 1.0, 1.0 }
local HEADER_COL    = { 0.8, 0.8, 0.8, 1.0 }
local NAME_COL      = { 1.0, 1.0, 1.0, 1.0 }
local SUMMARY_COL   = { 0.75, 0.75, 0.75, 1.0 }
local EMPTY_COL     = { 0.6, 0.6, 0.6, 1.0 }
local ERROR_COL     = { 1.0, 0.45, 0.45, 1.0 }
local CLAIMED_COL   = { 0.65, 0.9, 0.65, 1.0 }
local UNCLAIMED_COL = { 0.75, 0.75, 0.75, 1.0 }
local READY_COL     = { 0.4, 0.9, 0.4, 1.0 }
local MISSING_COL   = { 0.85, 0.35, 0.3, 1.0 }
local PAUSED_COL    = { 0.9, 0.75, 0.35, 1.0 }

-----------------------------------------------------------
-- State
-----------------------------------------------------------
craftingPanel.hud = nil

craftingPanel.state = craftingPanel.state or {
    open            = false,
    bid             = nil,
    panelId         = nil,
    chromeElements  = {},
    recipeElements  = {},
    queueElements   = {},
    recipes         = {},   -- cached recipesForStation(bid), static while open
    recipeRows      = {},   -- {recipeId, tbId, cbId} for the CURRENTLY shown page
    recipeInputs    = {},   -- [recipeId] = {count=text, until_=bool}, survives rebuilds
    recipePage      = 1,
    queuePage       = 1,
    layout          = nil,  -- {leftX,leftW,rightX,rightW,contentTop,contentBottom}
    statusLabelId   = nil,
    refreshTimer    = 0,
}

craftingPanel._itemNames = nil
craftingPanel._whitePixelTex = nil

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

-- Ground-item count per defName on the active world, summed by row
-- (each item.listGround() entry is one instance). The scope note in
-- the module header applies: this is ground stock only, not the
-- fetch ladder's carried/technomule/cargo rungs.
function craftingPanel.groundStockTally()
    local tally = {}
    for _, g in ipairs(item.listGround() or {}) do
        tally[g.defName] = (tally[g.defName] or 0) + 1
    end
    return tally
end

local function recipeNeeds(def)
    local needs = {}
    local function add(defName, count)
        needs[defName] = (needs[defName] or 0) + count
    end
    for _, ing in ipairs(def.inputs or {}) do add(ing.item, ing.count) end
    if def.fuel then add(def.fuel.item, def.fuel.count) end
    return needs
end

-- { ready = bool, missing = {"Iron Ore (1/3)", ...} } against a
-- groundStockTally() snapshot.
function craftingPanel.recipeAvailability(def, tally)
    local missing = {}
    for defName, need in pairs(recipeNeeds(def)) do
        local have = tally[defName] or 0
        if have < need then
            missing[#missing + 1] = string.format("%s (%d/%d)",
                itemDisplayName(defName), have, need)
        end
    end
    table.sort(missing)
    return { ready = (#missing == 0), missing = missing }
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

local function ensureWhitePixel()
    if not craftingPanel._whitePixelTex then
        craftingPanel._whitePixelTex =
            engine.loadTexture("assets/textures/utility/white.png")
    end
    return craftingPanel._whitePixelTex
end

-----------------------------------------------------------
-- Teardown
-----------------------------------------------------------
local function destroyElementList(list)
    for _, e in ipairs(list) do
        if e.kind == "label" then label.destroy(e.id)
        elseif e.kind == "button" then button.destroy(e.id)
        elseif e.kind == "textbox" then textbox.destroy(e.id)
        elseif e.kind == "checkbox" then checkbox.destroy(e.id)
        elseif e.kind == "sprite" then UI.deleteElement(e.id)
        end
    end
end

local function destroyQueue()
    local s = craftingPanel.state
    destroyElementList(s.queueElements)
    s.queueElements = {}
end

local function destroyRecipes()
    local s = craftingPanel.state
    destroyElementList(s.recipeElements)
    s.recipeElements = {}
    s.recipeRows = {}
end

local function destroyChrome()
    local s = craftingPanel.state
    destroyElementList(s.chromeElements)
    s.chromeElements = {}
    s.statusLabelId = nil
    if s.panelId then
        panel.destroy(s.panelId)
        s.panelId = nil
    end
    s.layout = nil
end

local function destroyAll()
    destroyQueue()
    destroyRecipes()
    destroyChrome()
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
-- Shared pager (Prev/Next + "Page p/n"), appended to `elemList`.
-- No-op (renders nothing) when there's only one page, so a queue/
-- recipe list that later grows past one page doesn't need a rebuild
-- of anything else.
-----------------------------------------------------------
local function buildPager(elemList, namePrefix, x, y, uiscale, page,
                           totalPages, onChange)
    if totalPages <= 1 then return end
    local h = craftingPanel.hud
    local btnW = PAGE_BTN_W

    local prevId = button.new({
        name = namePrefix .. "_prev", text = "<",
        x = x, y = y, width = PAGE_BTN_W, height = PAGE_BTN_H,
        fontSize = 14, uiscale = uiscale, page = h.page, font = h.menuFont,
        textureSet = h.boxTexSet,
        bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
        zIndex = 201,
        onClick = function() if page > 1 then onChange(page - 1) end end,
    })
    table.insert(elemList, { kind = "button", id = prevId })

    local lblId = label.new({
        name = namePrefix .. "_lbl", text = "Page " .. page .. "/" .. totalPages,
        font = h.menuFont, fontSize = SUMMARY_FONT, color = SUMMARY_COL,
        page = h.page, uiscale = uiscale,
        x = x + math.floor(btnW * uiscale) + math.floor(6 * uiscale),
        y = y + math.floor(PAGE_BTN_H * uiscale * 0.7),
    })
    table.insert(elemList, { kind = "label", id = lblId })
    local lblW = select(1, label.getSize(lblId))

    local nextBtnId = button.new({
        name = namePrefix .. "_next", text = ">",
        x = x + math.floor(btnW * uiscale) + math.floor(6 * uiscale) + lblW
            + math.floor(6 * uiscale),
        y = y, width = PAGE_BTN_W, height = PAGE_BTN_H,
        fontSize = 14, uiscale = uiscale, page = h.page, font = h.menuFont,
        textureSet = h.boxTexSet,
        bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
        zIndex = 201,
        onClick = function() if page < totalPages then onChange(page + 1) end end,
    })
    table.insert(elemList, { kind = "button", id = nextBtnId })
end

-----------------------------------------------------------
-- Recipes column (rebuilt on: open, page change, and the periodic
-- timer — a snapshot/restore of each row's textbox+checkbox around the
-- rebuild means a periodic refresh never loses what the player was
-- mid-typing).
-----------------------------------------------------------
local renderRecipes

local function snapshotRecipeInputs()
    local s = craftingPanel.state
    for _, row in ipairs(s.recipeRows) do
        s.recipeInputs[row.recipeId] = {
            count  = textbox.getValue(row.tbId),
            until_ = checkbox.isChecked(row.cbId),
        }
    end
end

renderRecipes = function()
    local s = craftingPanel.state
    local h = craftingPanel.hud
    local L = s.layout
    if not h or not L then return end
    snapshotRecipeInputs()
    destroyRecipes()

    local uiscale = scale.get()
    local x, w = L.leftX, L.leftW
    local y = L.contentTop

    local recipes = s.recipes
    if #recipes == 0 then
        local emptyId = label.new({
            name = "crafting_panel_rec_empty",
            text = "No recipes offered by this station.",
            font = h.menuFont, fontSize = SUMMARY_FONT, color = EMPTY_COL,
            page = h.page, uiscale = uiscale, x = x, y = y + math.floor(NAME_FONT * uiscale),
        })
        table.insert(s.recipeElements, { kind = "label", id = emptyId })
        return
    end

    local rowH = math.floor(ROW_H * uiscale)
    local rowGap = math.floor(ROW_GAP * uiscale)
    local pagerH = math.floor(PAGER_H * uiscale)
    local availH = (L.contentBottom - y) - pagerH
    local perPage = math.max(1, math.floor((availH + rowGap) / (rowH + rowGap)))
    local totalPages = math.max(1, math.ceil(#recipes / perPage))
    s.recipePage = math.max(1, math.min(s.recipePage, totalPages))

    local tally = craftingPanel.groundStockTally()
    local dotSz = math.floor(DOT_SZ * uiscale)
    local cbSz = math.floor(UNTIL_CB_SZ * uiscale)
    local tbW = math.floor(COUNT_TB_W * uiscale)
    local addW = math.floor(ADD_BTN_W * uiscale)
    local textLeft = x + dotSz + math.floor(8 * uiscale)
    local controlsRight = x + w
    local addX = controlsRight - addW
    local tbX = addX - math.floor(6 * uiscale) - tbW
    local cbX = tbX - math.floor(6 * uiscale) - cbSz

    local startIdx = (s.recipePage - 1) * perPage + 1
    local endIdx = math.min(#recipes, startIdx + perPage - 1)
    local row = 0
    for i = startIdx, endIdx do
        row = row + 1
        local def = recipes[i]
        local rowY = y + (row - 1) * (rowH + rowGap)
        local saved = s.recipeInputs[def.id]

        local avail = craftingPanel.recipeAvailability(def, tally)
        local dotId = UI.newSprite("crafting_panel_rec_dot_" .. i,
            dotSz, dotSz, ensureWhitePixel(),
            (avail.ready and READY_COL or MISSING_COL)[1],
            (avail.ready and READY_COL or MISSING_COL)[2],
            (avail.ready and READY_COL or MISSING_COL)[3], 1.0, h.page)
        UI.addToPage(h.page, dotId, x,
            rowY + math.floor((math.floor(NAME_FONT * uiscale) - dotSz) / 2))
        UI.setZIndex(dotId, 202)
        UI.setClickable(dotId, true)
        UI.setTooltip(dotId, avail.ready
            and "Ready (ground stock covers this recipe)"
            or "Missing on the ground: " .. table.concat(avail.missing, ", "))
        table.insert(s.recipeElements, { kind = "sprite", id = dotId })

        local nameId = label.new({
            name = "crafting_panel_rec_name_" .. i,
            text = truncate(def.name, MAX_NAME_CHARS),
            font = h.menuFont, fontSize = NAME_FONT, color = NAME_COL,
            page = h.page, uiscale = uiscale,
            x = textLeft, y = rowY + math.floor(NAME_FONT * uiscale),
            tooltip = craftingPanel.formatRecipeSummary(def),
        })
        table.insert(s.recipeElements, { kind = "label", id = nameId })

        local summaryId = label.new({
            name = "crafting_panel_rec_summary_" .. i,
            text = truncate(craftingPanel.formatRecipeSummary(def), 40),
            font = h.menuFont, fontSize = SUMMARY_FONT, color = SUMMARY_COL,
            page = h.page, uiscale = uiscale,
            x = textLeft,
            y = rowY + math.floor(NAME_FONT * uiscale)
                + math.floor((SUMMARY_FONT + 4) * uiscale),
        })
        table.insert(s.recipeElements, { kind = "label", id = summaryId })

        local untilLblId = label.new({
            name = "crafting_panel_rec_untillbl_" .. i, text = "until",
            font = h.menuFont, fontSize = SUMMARY_FONT, color = SUMMARY_COL,
            page = h.page, uiscale = uiscale,
            x = cbX - math.floor(34 * uiscale),
            y = rowY + math.floor(NAME_FONT * uiscale),
            tooltip = "Checked: treat the count as a target GROUND-STOCK "
                .. "level for this recipe's first output — queues "
                .. "max(0, target - current ground stock) crafts. "
                .. "Unchecked: the count is a fixed number of crafts "
                .. "(blank/0 = repeat forever).",
        })
        table.insert(s.recipeElements, { kind = "label", id = untilLblId })

        local cbId = checkbox.new({
            name = "crafting_panel_rec_until_" .. i,
            x = cbX, y = rowY, size = UNTIL_CB_SZ,
            page = h.page, uiscale = uiscale,
            default = saved and saved.until_ or false,
            zIndex = 201,
        })
        table.insert(s.recipeElements, { kind = "checkbox", id = cbId })

        local tbId = textbox.new({
            name = "crafting_panel_rec_count_" .. i,
            width = COUNT_TB_W, height = COUNT_TB_H,
            x = tbX, y = rowY,
            page = h.page, font = h.menuFont, fontSize = 15,
            textType = textbox.Type.NUMBER,
            default = saved and saved.count or "",
            zIndex = 201,
        })
        table.insert(s.recipeElements, { kind = "textbox", id = tbId })

        local recipeId = def.id
        local capturedTb, capturedCb = tbId, cbId
        local addId = button.new({
            name = "crafting_panel_rec_add_" .. i, text = "Add",
            x = addX, y = rowY,
            width = ADD_BTN_W, height = ADD_BTN_H,
            fontSize = 13, uiscale = uiscale,
            page = h.page, font = h.menuFont, textureSet = h.boxTexSet,
            bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
            zIndex = 201,
            onClick = function()
                local text = textbox.getValue(capturedTb)
                local wantUntil = checkbox.isChecked(capturedCb)
                local count
                if wantUntil then
                    local target = tonumber(text)
                    if not target or target < 1 then
                        setStatus("Enter a target amount for 'until'", true)
                        return
                    end
                    local outDef = def.outputs and def.outputs[1]
                    local have = outDef
                        and (craftingPanel.groundStockTally()[outDef.item] or 0)
                        or 0
                    local needed = math.floor(target) - have
                    if needed <= 0 then
                        s.recipeInputs[recipeId] = { count = "", until_ = false }
                        setStatus(string.format(
                            "Already have %d on the ground -- no bill added",
                            have), false)
                        renderRecipes()
                        return
                    end
                    count = needed
                else
                    count = craftingPanel.parseCount(text)
                end
                local billId, err = craft.addBill(s.bid, recipeId, count)
                if billId then
                    s.recipeInputs[recipeId] = { count = "", until_ = false }
                    setStatus("", false)
                    craftingPanel.refreshQueue()
                    renderRecipes()
                else
                    setStatus(err or "could not add bill", true)
                end
            end,
        })
        table.insert(s.recipeElements, { kind = "button", id = addId })

        table.insert(s.recipeRows, { recipeId = def.id, tbId = tbId, cbId = cbId })
    end

    buildPager(s.recipeElements, "crafting_panel_rec_pager",
        x, y + perPage * (rowH + rowGap), uiscale,
        s.recipePage, totalPages,
        function(newPage) s.recipePage = newPage; renderRecipes() end)
end
craftingPanel.refreshRecipes = renderRecipes

-----------------------------------------------------------
-- Queue column (rebuilt on: open, add/cancel/pause/reorder, page
-- change, and the periodic timer)
-----------------------------------------------------------
local renderQueue

renderQueue = function()
    local s = craftingPanel.state
    local h = craftingPanel.hud
    local L = s.layout
    if not h or not L then return end
    destroyQueue()

    local uiscale = scale.get()
    local x, w = L.rightX, L.rightW
    local y = L.contentTop

    local bills = craft.getBills(s.bid) or {}
    if #bills == 0 then
        local id = label.new({
            name = "crafting_panel_queue_empty", text = "No bills queued yet.",
            font = h.menuFont, fontSize = SUMMARY_FONT, color = EMPTY_COL,
            page = h.page, uiscale = uiscale, x = x,
            y = y + math.floor(NAME_FONT * uiscale),
        })
        table.insert(s.queueElements, { kind = "label", id = id })
        return
    end

    local rowH = math.floor(QROW_H * uiscale)
    local rowGap = math.floor(ROW_GAP * uiscale)
    local pagerH = math.floor(PAGER_H * uiscale)
    local availH = (L.contentBottom - y) - pagerH
    local perPage = math.max(1, math.floor((availH + rowGap) / (rowH + rowGap)))
    local totalPages = math.max(1, math.ceil(#bills / perPage))
    s.queuePage = math.max(1, math.min(s.queuePage, totalPages))

    local reorderSz = math.floor(REORDER_BTN_SZ * uiscale)
    local pauseW = math.floor(PAUSE_BTN_W * uiscale)
    local cancelW = math.floor(CANCEL_BTN_SZ * uiscale)
    local gap = math.floor(4 * uiscale)
    local cancelX = x + w - cancelW
    local pauseX  = cancelX - gap - pauseW
    local downX   = pauseX - gap - reorderSz
    local upX     = downX - reorderSz
    local nameW   = math.floor((upX - x) * 0.42)
    local remW    = math.floor((upX - x) * 0.16)
    local progW   = math.floor((upX - x) * 0.18)

    local startIdx = (s.queuePage - 1) * perPage + 1
    local endIdx = math.min(#bills, startIdx + perPage - 1)
    local row = 0
    for i = startIdx, endIdx do
        row = row + 1
        local bill = bills[i]
        local rowY = y + (row - 1) * (rowH + rowGap)
        local def = craft.get(bill.recipe)
        local recipeName = truncate(def and def.name or bill.recipe, MAX_NAME_CHARS)
        local remaining = (bill.remaining < 0) and "inf" or tostring(bill.remaining)
        local pct = string.format("%d%%", math.floor((bill.progress or 0) * 100 + 0.5))
        local who = claimantName(bill.claimant)
        local rowNameCol = bill.paused and PAUSED_COL or NAME_COL

        local nameId = label.new({
            name = "crafting_panel_q_name_" .. i, text = recipeName,
            font = h.menuFont, fontSize = NAME_FONT, color = rowNameCol,
            page = h.page, uiscale = uiscale,
            x = x, y = rowY + math.floor(NAME_FONT * uiscale),
            tooltip = bill.paused and "Paused -- won't draw a new worker" or nil,
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
            text = truncate(who or "--", 12),
            font = h.menuFont, fontSize = NAME_FONT,
            color = who and CLAIMED_COL or UNCLAIMED_COL,
            page = h.page, uiscale = uiscale,
            x = x + nameW + remW + progW,
            y = rowY + math.floor(NAME_FONT * uiscale),
        })
        table.insert(s.queueElements, { kind = "label", id = claimId })

        local billId = bill.id
        -- Up/Down: only rendered when a move that direction is
        -- possible (index 1 of the FULL station queue, not just this
        -- page) — reorderBill already refuses at either end, but
        -- skipping the button there reads clearer than a button that
        -- silently no-ops.
        if i > 1 then
            local upId = button.new({
                name = "crafting_panel_q_up_" .. i, text = "^",
                x = upX, y = rowY, width = REORDER_BTN_SZ, height = REORDER_BTN_SZ,
                fontSize = 12, uiscale = uiscale, page = h.page, font = h.menuFont,
                textureSet = h.boxTexSet,
                bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
                zIndex = 202,
                onClick = function()
                    craft.reorderBill(billId, "up")
                    renderQueue()
                end,
            })
            table.insert(s.queueElements, { kind = "button", id = upId })
        end
        if i < #bills then
            local downId = button.new({
                name = "crafting_panel_q_down_" .. i, text = "v",
                x = downX, y = rowY, width = REORDER_BTN_SZ, height = REORDER_BTN_SZ,
                fontSize = 12, uiscale = uiscale, page = h.page, font = h.menuFont,
                textureSet = h.boxTexSet,
                bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
                zIndex = 202,
                onClick = function()
                    craft.reorderBill(billId, "down")
                    renderQueue()
                end,
            })
            table.insert(s.queueElements, { kind = "button", id = downId })
        end

        local paused = bill.paused and true or false
        local pauseId = button.new({
            name = "crafting_panel_q_pause_" .. i,
            text = paused and "Resume" or "Pause",
            x = pauseX, y = rowY, width = PAUSE_BTN_W, height = PAUSE_BTN_H,
            fontSize = 11, uiscale = uiscale, page = h.page, font = h.menuFont,
            textureSet = h.boxTexSet,
            bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
            zIndex = 202,
            onClick = function()
                craft.setBillPaused(billId, not paused)
                renderQueue()
            end,
        })
        table.insert(s.queueElements, { kind = "button", id = pauseId })

        local cancelId = button.new({
            name = "crafting_panel_q_cancel_" .. i, text = "X",
            x = cancelX, y = rowY, width = CANCEL_BTN_SZ, height = CANCEL_BTN_SZ,
            fontSize = 13, uiscale = uiscale, page = h.page, font = h.menuFont,
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

    buildPager(s.queueElements, "crafting_panel_q_pager",
        x, y + perPage * (rowH + rowGap), uiscale,
        s.queuePage, totalPages,
        function(newPage) s.queuePage = newPage; renderQueue() end)
end
craftingPanel.refreshQueue = renderQueue

-----------------------------------------------------------
-- Chrome (panel + title + close + column headers + status line).
-- Built once per open(); recipe/queue columns render independently.
-----------------------------------------------------------
local function renderChrome(bid)
    local h = craftingPanel.hud
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
    table.insert(s.chromeElements, { kind = "label", id = titleId })

    local closeSz = math.floor(CLOSE_BTN_SZ * uiscale)
    local closeId = button.new({
        name = "crafting_panel_close", text = "X",
        x = cx + cw - closeSz, y = cy,
        width = CLOSE_BTN_SZ, height = CLOSE_BTN_SZ,
        fontSize = 16, uiscale = uiscale,
        page = h.page, font = h.menuFont, textureSet = h.boxTexSet,
        bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
        zIndex = 202,
        onClick = function() craftingPanel.closeIfOpen() end,
    })
    table.insert(s.chromeElements, { kind = "button", id = closeId })

    local colGap = math.floor(COL_GAP * uiscale)
    local leftW = math.floor((cw - colGap) * 0.56)
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
    table.insert(s.chromeElements, { kind = "label", id = recHeaderId })

    local queueHeaderId = label.new({
        name = "crafting_panel_queue_header", text = "Queue",
        font = h.menuFont, fontSize = HEADER_FONT, color = HEADER_COL,
        page = h.page, uiscale = uiscale,
        x = rightX, y = headerY + math.floor(HEADER_FONT * uiscale),
    })
    table.insert(s.chromeElements, { kind = "label", id = queueHeaderId })

    local contentTop = headerY + math.floor(HEADER_H * uiscale)
        + math.floor(4 * uiscale)
    local statusY = panelY + panelH - PAD_BOT - math.floor(4 * uiscale)
    local contentBottom = statusY - math.floor(10 * uiscale)

    s.statusLabelId = label.new({
        name = "crafting_panel_status", text = "",
        font = h.menuFont, fontSize = SUMMARY_FONT, color = SUMMARY_COL,
        page = h.page, uiscale = uiscale, x = leftX, y = statusY,
    })
    table.insert(s.chromeElements, { kind = "label", id = s.statusLabelId })

    s.layout = {
        leftX = leftX, leftW = leftW, rightX = rightX, rightW = rightW,
        contentTop = contentTop, contentBottom = contentBottom,
    }
end

-----------------------------------------------------------
-- Public lifecycle
-----------------------------------------------------------

-- Open (or refresh) the panel for a station building id. Refused if
-- the building is gone, or the assets aren't ready yet.
function craftingPanel.show(bid)
    if not bid or not craftingPanel.hud then return end
    if not building.getInfo(bid) then return end
    destroyAll()
    local s = craftingPanel.state
    s.bid = bid
    s.open = true
    s.refreshTimer = 0
    s.recipePage = 1
    s.queuePage = 1
    s.recipeInputs = {}
    s.recipes = craftingPanel.recipesForStation(bid)
    renderChrome(bid)
    renderRecipes()
    renderQueue()
end

function craftingPanel.closeIfOpen()
    local s = craftingPanel.state
    if not s.open then return end
    destroyAll()
    s.open = false
    s.bid = nil
    s.recipes = {}
    s.recipeInputs = {}
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

-- Periodic refresh (queue progress/claimant/remaining, and recipe
-- ground-stock availability) plus an auto-close if the station was
-- demolished while the panel was open. Recipe rows snapshot + restore
-- their textbox/checkbox around every rebuild (see
-- snapshotRecipeInputs), so this never wipes an in-progress count the
-- player is mid-typing.
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
        renderRecipes()
    end
end

function craftingPanel.shutdown()
    craftingPanel.closeIfOpen()
    engine.logInfo("Crafting panel shut down")
end

return craftingPanel
