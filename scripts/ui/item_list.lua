-- Shared item-list widget (#1088, epic #1013 phase C0).
--
-- ONE renderer for every panel that shows a list of items: the cargo
-- contents popup, the item-container contents popup, the unit-info
-- inventory section, and (later) C1's paired transfer view. Before
-- this module those three hosts each carried their own stack key,
-- grouper, tab computation, row builder, UTF-8 truncation and dirty
-- check; a fourth copy for C1 is what this extraction exists to avoid.
--
-- The widget owns: optional grouping and the canonical stack key,
-- category normalization + tab computation, row creation/layout/
-- teardown, representative-instance right-click routing, UTF-8-safe
-- truncation, selected-tab routing, rebuild invalidation, the optional
-- capacity/weight header, and the optional footer.
--
-- The HOST owns: its window or HUD section, data acquisition, its
-- title and surrounding chrome, the presentation policy it supplies
-- through the callbacks below, and the contents of any right-click
-- menu. The widget never creates, positions or closes a window, and
-- never learns what "Withdraw" or "Equip" mean.
--
-- Rendering within host-supplied bounds:
--
--   +-- p.y ------------------------------------------+
--   |  [optional header]                              |
--   |  [optional tab strip]     (scripts/ui/tabbar)   |
--   |  row 0 ... row n-1                              |
--   |  [optional footer]                              |
--   +-- p.y + p.height --------------------------------+
--
-- Metrics are BASE values multiplied by an effective scale, so each
-- host keeps its own visual proportions verbatim. When `fitVertical`
-- is set the widget derives a LOCAL effective scale
-- (responsive.fitScale) against the height needed for the tab strip
-- plus AT LEAST ONE row plus the footer, which is what keeps the
-- unit-info inventory reachable at a narrow, high-scale, still-
-- supported combination.
--
-- Invalidation: the widget owns the final rebuild comparison. A host
-- keeps NO hash of its own -- it asks `itemList.isStale(id, params)`
-- and rebuilds its own chrome plus the widget when the answer is yes.
-- Values the widget cannot see (a formatter's output, a repair-status
-- badge) reach the comparison through the host's `presentationKey`.
--
-- Since #1268 the widget also owns TRACKED TEMPERATURE presentation:
-- it summarizes a group's members while grouping them, stores the
-- summary on the row, signs it, and exposes the two join helpers both
-- raw-item hosts render it with. See "Tracked temperature
-- presentation" below.

local label         = require("scripts.ui.label")
local scale         = require("scripts.ui.scale")
local tabbar        = require("scripts.ui.tabbar")
local brokenOverlay = require("scripts.ui.broken_overlay")
local textWrap      = require("scripts.ui.text_wrap")
local responsive    = require("scripts.ui.responsive")

local itemList = {}

-----------------------------------------------------------
-- Constants / module state
-----------------------------------------------------------

local RIGHT_CLICK_CALLBACK = "onItemListRightClick"

local lists  = {}
local nextId = 1
local whitePixelTex = nil

function itemList.init()
    if not whitePixelTex then
        whitePixelTex = engine.loadTexture("assets/textures/utility/white.png", "ui")
    end
    engine.logDebug("ItemList module initialized")
end

-----------------------------------------------------------
-- Pure data shaping
-----------------------------------------------------------

-- A missing or empty category normalizes to "Misc" -- consistently in
-- BOTH tab discovery and tab counting, and in the active-tab filter.
-- The pre-#1088 cargo panel discovered tabs from the normalized value
-- but counted against the raw one, so an uncategorized item produced a
-- "Misc (0)" tab whose rows the filter nonetheless listed.
function itemList.normalizeCategory(category)
    if category == nil or category == "" then return "Misc" end
    return category
end

-- The canonical stack key. Rows merge only when every VISIBLE identity
-- field matches: defName + quality + condition + current fill + weight
-- (raw gems roll a per-instance weight and the row shows weight x
-- count) + weapon-only sharpness (non-weapon gear carries a mutated
-- sharpness it never displays, so splitting on it would be an
-- invisible, confusing split) + the nested contents signature (#67A).
--
-- What merging licenses is bounded by what a host can DO to a row
-- (#1268). Three kinds of row action, and they read a group
-- differently:
--
--   TEMPERATURE-INSENSITIVE, SINGLE-INSTANCE -- Equip / Unequip,
--   Contents, Prioritize / Un-prioritize Repair, and the singular
--   "Store 1" / "Retrieve 1". The members are interchangeable for
--   these, so routing them to the representative instanceId is correct
--   (#67).
--
--   BATCH -- "Store all" / "Retrieve all". These do not inherit that:
--   they name every member explicitly through `instanceIds` (#1249),
--   because the transfer contract takes exact item references and a
--   representative cannot stand for twelve of them.
--
--   TEMPERATURE-SENSITIVE -- "Drink" (#1580). Merging is NOT a claim
--   that the members are mechanically identical: tracked temperature
--   (#344) is deliberately not a key field (it cools continuously, so
--   keying on it would split and re-merge a row forever), and
--   consumption effects scale continuously with it
--   (scripts/consumable.lua). So such an action may NOT inherit the
--   representative and must let the player pick the exact instance:
--   scripts/consumable_gestures.lua walks `instanceIds` and fans the
--   gesture out into one submenu entry per member, each labelled with
--   its OWN effective temperature. Any further temperature-sensitive
--   action owes the same.
--
-- The group's own temperature is presented honestly alongside -- see
-- `tempSummary` below.
--
-- Returns nil for an equipped entry when the host asked for equipped
-- items to stay separate -- each occupies a distinct slot.
function itemList.stackKey(it, separateEquipped)
    if separateEquipped and it.equipped then return nil end
    return table.concat({
        tostring(it.defName),
        tostring(it.quality     or "_"),
        tostring(it.condition   or "_"),
        tostring(it.currentFill or "_"),
        tostring(it.weight      or "_"),
        it.weapon and tostring(it.sharpness or "_") or "_",
        tostring(it.contentsKey or ""),
    }, "|")
end

-----------------------------------------------------------
-- Tracked temperature presentation (#1268)
--
-- Raw items carry `temp` -- the TRACKED iiTemp (#344), present only
-- while the item is hotter or colder than its surroundings, so an
-- ABSENT field means "at ambient" and is displayed as such. It is
-- deliberately absent from the stack key above, so a group can hold
-- members at genuinely different temperatures and only a whole-group
-- summary describes it honestly.
--
-- Everything a host puts on screen is derived here, in one place: the
-- row text, the tooltip line and the invalidation signature all read
-- the SAME summary string, so they cannot round differently or drift
-- apart between the two raw-item hosts.
-----------------------------------------------------------

-- A tracked temperature is a real, finite number; anything else means
-- ambient. The non-finite guard is what keeps a NaN out of the min/max
-- walk below, where every comparison against it is false and a group's
-- range would silently collapse to one bogus endpoint.
local function isTracked(t)
    return type(t) == "number" and t == t
       and t ~= math.huge and t ~= -math.huge
end

-- One whole degree Celsius, rounded half away from zero so a value and
-- its negation present symmetrically. This is the ONLY rounding in the
-- feature and it happens BEFORE the single-value-vs-range decision, so
-- 41.6 and 42.3 are one presented value ("42°C") rather than the
-- degenerate range "42–42°C".
--
-- The result is always an INTEGER, saturating rather than raising for
-- an absurd magnitude: math.floor answers a FLOAT when the value is
-- too large to hold an integer, and string.format's "%d" raises on
-- one. iiTemp is a 32-bit float and unit.setItemTemp takes whatever
-- Lua hands it, so `unit.setItemTemp(uid, iid, 1e38)` is reachable --
-- and it would otherwise abort a whole panel rebuild rather than
-- render one silly row.
function itemList.roundTemp(t)
    local r = (t >= 0) and math.floor(t + 0.5) or -math.floor(-t + 0.5)
    return math.tointeger(r)
        or (r < 0 and math.mininteger or math.maxinteger)
end

-- The four summary forms, decided entirely by the distinct ROUNDED
-- values a group holds plus whether any member was at ambient:
--
--   all ambient                 -> "ambient"
--   one presented tracked value -> "42°C"        (negatives keep the sign)
--   several                     -> "35–42°C"
--   ambient plus tracked        -> "ambient + 35–42°C"
--
-- Never the representative's own value presented as the group's.
local function summarizeTemp(acc)
    if not acc.tracked then return "ambient" end
    local body
    if acc.min == acc.max then
        body = string.format("%d°C", acc.min)
    else
        body = string.format("%d–%d°C", acc.min, acc.max)
    end
    if acc.ambient then return "ambient + " .. body end
    return body
end

-- Fold ONE member into its group's accumulator.
local function accumulateTemp(acc, it)
    if isTracked(it.temp) then
        local d = itemList.roundTemp(it.temp)
        if not acc.tracked then
            acc.tracked, acc.min, acc.max = true, d, d
        else
            if d < acc.min then acc.min = d end
            if d > acc.max then acc.max = d end
        end
    else
        acc.ambient = true
    end
end

-- The summary `groupItems` stored on a row, or nil for a row this
-- widget did not group. The two host helpers below say NOTHING when it
-- is absent, which is what keeps the "never synthesize" rule
-- structural: `itemList.prepare` hands PRE-GROUPED rows straight
-- through (the item-contents popup's deliberately coarse by-defName
-- grouping), and the widget never saw those members -- inventing a
-- summary there would present the representative's temperature as the
-- whole group's, the exact failure this feature exists to avoid.
function itemList.tempSummary(row)
    return row and row.tempSummary or nil
end

-- The ROW-TEXT form: a host's own name text with the summary appended.
-- Both raw-item hosts join it through here, so a row and its tooltip
-- can never disagree and the two windows can never drift apart. It
-- rides the existing name column and inherits the widget's UTF-8-safe
-- truncation -- appending to every row (ambient ones included) costs
-- name width, which is the accepted consequence of the presentation.
function itemList.withTempSuffix(baseName, row)
    local s = itemList.tempSummary(row)
    if not s then return baseName or "" end
    return (baseName or "") .. " · " .. s
end

-- The TOOLTIP form: a labeled line in the lowercase "label: value"
-- style every other hint line uses (scripts/unit_info_v2_items.lua).
function itemList.tempHintLine(row)
    local s = itemList.tempSummary(row)
    if not s then return nil end
    return "temperature: " .. s
end

-- Fold ONE member's instance identity into its group's membership list:
-- distinct ids, in raw first-appearance order. A member with no numeric
-- instanceId contributes nothing (the item-contents hosts' pre-grouped
-- rows and the unit-info silhouette's synthetic entries both reach the
-- widget without one), so an absent identity is an absence rather than
-- a `nil` hole in the list.
local function accumulateInstance(acc, it)
    local iid = it.instanceId
    if type(iid) ~= "number" then return end
    if acc.seen[iid] then return end
    acc.seen[iid] = true
    acc.list[#acc.list + 1] = iid
end

-- Copy-and-count grouping in FIRST-APPEARANCE order. The first item of
-- each group stays its representative instance; the shared count field
-- is `count`.
--
-- Grouping is also the one place every member of a row is visible, so
-- it is where the temperature summary is computed and stored
-- (`tempSummary`). A host callback could not do it: the widget's data
-- signature records a callback's PRESENCE, never its output, so a
-- summary produced only in `rowName`/`rowTooltip` would never
-- invalidate a row.
--
-- It is also the only place a row's COMPLETE membership is visible, so
-- it is where `instanceIds` is recorded (#1249). Before that, a merged
-- row kept nothing but its representative and its `count`, which is
-- enough for the temperature-insensitive single-instance actions
-- documented on `stackKey` but NOT for a batch gesture: "Store all" on
-- a row of twelve rations is twelve distinct instance ids, never a
-- count (the transfer contract takes an ordered list of exact item
-- references and refuses duplicates). Read it through
-- `itemList.rowInstanceIds` rather than off the row.
function itemList.groupItems(items, opts)
    local separateEquipped = opts and opts.separateEquipped
    local groups = {}
    local accs   = {}
    local ids    = {}
    local seen   = {}
    for _, it in ipairs(items or {}) do
        local key = itemList.stackKey(it, separateEquipped)
        local idx = key and seen[key]
        if idx then
            groups[idx].count = groups[idx].count + 1
            accumulateTemp(accs[idx], it)
            accumulateInstance(ids[idx], it)
        else
            local copy = {}
            for k, v in pairs(it) do copy[k] = v end
            copy.count = 1
            groups[#groups + 1] = copy
            accs[#groups] = { ambient = false, tracked = false }
            ids[#groups]  = { list = {}, seen = {} }
            accumulateTemp(accs[#groups], it)
            accumulateInstance(ids[#groups], it)
            if key then seen[key] = #groups end
        end
    end
    for i, g in ipairs(groups) do
        g.tempSummary = summarizeTemp(accs[i])
        -- Overwrites rather than merges: `copy` above cloned every field
        -- the raw item carried, so a source that already had an
        -- `instanceIds` field must not survive as this row's membership.
        g.instanceIds = ids[i].list
    end
    return groups
end

-- The ordered instance identities a row stands for, as a FRESH list a
-- caller may keep or mutate without reaching into the widget's model.
--
-- A row this widget grouped answers with its recorded membership. A row
-- it did not (`preGrouped` — the item-contents levels, whose rows the
-- engine already grouped by defName) has no membership to report, so
-- this answers with the representative alone: that is honestly all such
-- a row identifies, and a batch built from it would otherwise be a
-- guess. An entry with neither answers empty, which every gesture
-- treats as "offer nothing" rather than as a batch of zero.
function itemList.rowInstanceIds(row)
    if not row then return {} end
    if type(row.instanceIds) == "table" then
        local out = {}
        for _, v in ipairs(row.instanceIds) do out[#out + 1] = v end
        return out
    end
    if type(row.instanceId) == "number" then return { row.instanceId } end
    return {}
end

-- "All" first, then one tab per category in first-appearance order.
-- Counts are grouped DISPLAY-ROW counts, not summed item quantities:
-- a row standing for twelve identical items contributes one.
function itemList.computeTabs(rows)
    local tabs = { { name = "All", count = #rows } }
    local seen = { All = true }
    for _, r in ipairs(rows) do
        local c = itemList.normalizeCategory(r.category)
        if not seen[c] then
            seen[c] = true
            tabs[#tabs + 1] = { name = c, count = 0 }
        end
    end
    for _, r in ipairs(rows) do
        local c = itemList.normalizeCategory(r.category)
        for _, t in ipairs(tabs) do
            if t.name == c then t.count = t.count + 1 end
        end
    end
    return tabs
end

function itemList.filterByTab(rows, activeTab)
    if not activeTab or activeTab == "All" then return rows end
    local out = {}
    for _, r in ipairs(rows) do
        if itemList.normalizeCategory(r.category) == activeTab then
            out[#out + 1] = r
        end
    end
    return out
end

-- Defensive UTF-8-safe truncation with a ".." suffix (#618's rule, as
-- implemented by the unit-info inventory before this extraction).
--
-- #1107 moved the implementation to scripts/ui/text_wrap.lua, the shared
-- pixel-width fitting module, when the save browser's row columns needed
-- the identical rule; this stays as the widget's own documented entry
-- point so its hosts keep one name for it. Its contract is text_wrap's --
-- see textWrap.truncateToWidth for the full case list.
function itemList.truncateToWidth(text, font, fontPx, maxPx)
    return textWrap.truncateToWidth(text, font, fontPx, maxPx)
end

-- Normalize the host's input into the model the renderer and the
-- invalidation comparison both read. Pure -- a host calls this to size
-- its own window before any element exists, then hands the SAME model
-- back to new() so grouping is never computed twice.
--
-- Pre-grouped rows (unit.getItemContents' deliberately coarse
-- by-defName grouping) pass through with their order and grouping
-- exactly as supplied: the widget never re-splits or re-sorts them.
function itemList.prepare(p)
    local rows
    if p.preGrouped then
        rows = p.items or {}
    else
        rows = itemList.groupItems(p.items,
            { separateEquipped = p.separateEquipped })
    end

    local tabs, activeTab = nil, "All"
    if p.tabs then
        tabs = itemList.computeTabs(rows)
        activeTab = p.activeTab or "All"
        local stillValid = false
        for _, t in ipairs(tabs) do
            if t.name == activeTab then stillValid = true; break end
        end
        if not stillValid then activeTab = "All" end
    end

    local visible = tabs and itemList.filterByTab(rows, activeTab) or rows
    return { rows = rows, tabs = tabs, activeTab = activeTab, visible = visible }
end

-----------------------------------------------------------
-- Invalidation
-----------------------------------------------------------

-- Nested row values are signed BY VALUE, never by table identity.
-- The engine reconstructs `weapon` and `buffs` as fresh tables on
-- every inventory read, so `tostring(t)` -- an address -- would report
-- an unchanged list as stale on every single poll and rebuild the
-- panel forever. That is the reason these two helpers exist at all.

-- The nested weapon values a row can put on screen, plus the bare
-- presence flag that is all the signature carried before #1269. The
-- widget never indexes `weapon` in its own rendering, so a host is
-- free to supply a plain truthy marker instead of the engine's table;
-- such a row carries nothing but its presence, and signs exactly that.
local function weaponSignature(w)
    if not w then return "_" end
    if type(w) ~= "table" then return "w:" .. tostring(w) end
    return table.concat({
        "w",
        tostring(w.bladeLength or "_"),
        tostring(w.baseSharpness or "_"),
        tostring(w.stabEffectiveness or "_"),
        tostring(w.slashEffectiveness or "_"),
        tostring(w.bluntEffectiveness or "_"),
    }, ",")
end

-- Buff list: its length and ORDER, then each entry's four displayed
-- values. A row renders one line per buff in list order, so this is a
-- sequence signature and not a set one -- swapping two entries is a
-- visible change.
local function buffsSignature(bs)
    if bs == nil then return "_" end
    if type(bs) ~= "table" then return "b:" .. tostring(bs) end
    local parts = { "b" .. #bs }
    for _, b in ipairs(bs) do
        if type(b) == "table" then
            parts[#parts + 1] = table.concat({
                tostring(b.stat or "_"),
                tostring(b.amount or "_"),
                tostring(b.percent or "_"),
                tostring(b.scalesWithCondition or false),
            }, ",")
        else
            parts[#parts + 1] = tostring(b)
        end
    end
    return table.concat(parts, ";")
end

-- A row's complete membership (#1249), as a LENGTH plus every id in
-- order. Two rows of twelve rations differing in one member are a
-- different batch target even though every displayed value matches, so
-- this is a sequence signature and not a count.
local function instanceIdsSignature(ids)
    if type(ids) ~= "table" then return "_" end
    local parts = { "n" .. #ids }
    for _, v in ipairs(ids) do parts[#parts + 1] = tostring(v) end
    return table.concat(parts, ",")
end

-- Every value SUPPLIED IN THE ROW that the widget's own rendering or a
-- host callback (rowName, rowTooltip, rowColor, rowWeightText) can put
-- on screen. A field any of those reads and this list omits is a field
-- whose change leaves stale text -- and a stale baked-in tooltip --
-- on screen, because the widget answers "nothing changed" and keeps
-- the elements it already built.
--
-- Membership (#1249) is the one entry here that is NOT displayed: a
-- row's context menu acts on the exact instances it stands for, so
-- swapping a non-representative member for another of the same def,
-- quality and weight -- which changes no rendered pixel and no count --
-- must still rebuild, or the menu built from the previous row would
-- queue an order against an instance that has left the container.
--
-- State that is NOT in the row is deliberately not this function's
-- job: it reaches the comparison through the host's presentationKey
-- (see `signatures` below).
--
-- Temperature signs its PRESENTED summary and never the raw `temp`
-- the representative happens to carry (#1268). That is the whole
-- stability contract: cooling moves the raw value continuously, so a
-- change staying inside one displayed whole degree leaves this string
-- identical and rebuilds nothing, while crossing a degree boundary,
-- switching between tracked and ambient, or moving a group's displayed
-- minimum or maximum all change it and do rebuild.
local function rowSignature(r)
    return table.concat({
        tostring(r.defName or ""),
        tostring(r.displayName or ""),
        tostring(r.count or 1),
        itemList.normalizeCategory(r.category),
        tostring(r.make or ""),
        tostring(r.material or ""),
        tostring(r.quality or "_"),
        tostring(r.qualityTier or "_"),
        tostring(r.condition or "_"),
        tostring(r.currentFill or "_"),
        tostring(r.capacity or "_"),
        tostring(r.fill or "_"),
        tostring(r.weight or "_"),
        weaponSignature(r.weapon),
        tostring(r.sharpness or "_"),
        buffsSignature(r.buffs),
        tostring(r.contentsKey or ""),
        tostring(r.tempSummary or "_"),
        r.equipped and "e" or "i",
        tostring(r.equippedSlot or ""),
        tostring(r.accessoryIndex or "_"),
        tostring(r.instanceId or "_"),
        instanceIdsSignature(r.instanceIds),
        tostring(r.iconTex or "_"),
        tostring(r.unequippable or false),
    }, "/")
end

-- Two signatures, because a host that must size its own window from
-- the data cannot know its bounds yet when it asks whether anything
-- changed. `data` covers the widget's own presentation inputs plus
-- every row value `rowSignature` lists above -- which is every field
-- supplied in a row that the widget or a host callback can display,
-- nested weapon and buff detail included. State a callback consumes
-- from ANYWHERE ELSE (a formatter's output, repair claim/priority
-- state) is invisible here and reaches the comparison only through the
-- host's `presentationKey`. `layout` adds the bounds and row cap.
local function signatures(p, model)
    model = model or itemList.prepare(p)
    local parts = {
        "rows=" .. #model.rows,
        "tab=" .. tostring(model.activeTab),
        "pre=" .. tostring(p.preGrouped and 1 or 0),
        "eq=" .. tostring(p.separateEquipped and 1 or 0),
        "rc=" .. tostring(p.onRowRightClick and 1 or 0),
        "tt=" .. tostring(p.rowTooltip and 1 or 0),
        "s=" .. tostring(p.uiscale or scale.get()),
        "empty=" .. tostring(p.emptyText or ""),
        "footer=" .. tostring(p.footer and p.footer.text or ""),
        "hdr=" .. tostring(p.header and
            (tostring(p.header.stored) .. "/" .. tostring(p.header.capacity))
            or ""),
        "pk=" .. tostring(p.presentationKey or ""),
    }
    if model.tabs then
        for _, t in ipairs(model.tabs) do
            parts[#parts + 1] = "t:" .. t.name .. ":" .. t.count
        end
    end
    for _, r in ipairs(model.rows) do
        parts[#parts + 1] = rowSignature(r)
    end
    local data = table.concat(parts, "|")
    local layout = table.concat({
        tostring(p.x), tostring(p.y),
        tostring(p.width), tostring(p.height),
        tostring(p.maxRows or "fit"),
    }, ",")
    return data, layout, model
end

itemList.signatures = signatures

-- Has anything changed that must recreate elements? A caller that
-- already knows its bounds (an embedded section) passes them and gets
-- the bounds compared too; one that derives its bounds FROM the data
-- (a popup sized to its row count) omits them and is answered on the
-- data alone.
function itemList.isStale(id, p)
    local inst = lists[id]
    if not inst then return true end
    local data, layout = signatures(p)
    if data ~= inst.dataSig then return true end
    if p.width ~= nil and layout ~= inst.layoutSig then return true end
    return false
end

-- Force the next isStale() to answer true (a player action whose
-- effect the host wants reflected in the SAME frame).
function itemList.invalidate(id)
    local inst = lists[id]
    if inst then inst.dataSig = nil end
end

-----------------------------------------------------------
-- Rendering
-----------------------------------------------------------

local function track(inst, kind, id)
    inst.elements[#inst.elements + 1] = { kind = kind, id = id }
end

local function destroyElements(inst)
    for _, e in ipairs(inst.elements) do
        if e.kind == "label" then label.destroy(e.id)
        else UI.deleteElement(e.id) end
    end
    inst.elements = {}
    inst.rows = {}
    if inst.tabbarId then
        tabbar.destroy(inst.tabbarId)
        inst.tabbarId = nil
    end
end

local function buildTabs(inst, s, originY)
    local p = inst.params
    local t = p.tabs
    if not t then return 0 end
    local defs = {}
    for i, td in ipairs(inst.model.tabs) do
        defs[i] = { name = td.name .. " (" .. td.count .. ")", key = td.name }
    end
    local labelScale = p.tabLabelUiscale or s
    inst.tabbarId = tabbar.new({
        name           = inst.name .. "_tabs",
        page           = p.page,
        font           = p.font,
        layout         = t.mode or "row",
        tabs           = defs,
        x              = p.x,
        y              = originY,
        width          = p.width,
        wrapWidth      = t.wrapWidth or p.width,
        shrinkToFit    = t.shrinkToFit,
        centerRows     = t.centerRows,
        tabHeightPx    = math.floor((t.tabHeight or 28) * s),
        tileSizePx     = t.tileSize or 16,
        textPaddingPx  = math.floor((t.textPad or 8) * labelScale),
        measureFontPx  = math.floor((t.fontSize or 13) * labelScale),
        labelFontSize  = t.fontSize or 13,
        labelUiscale   = labelScale,
        gapPx          = math.floor((t.gap or 0) * s),
        rowGapPx       = math.floor((t.rowGap or 0) * s),
        minTabWidth    = t.minTabWidth or 20,
        textColor      = t.textColor,
        selectedTextColor = t.selectedTextColor,
        zIndex         = p.zBase or 10,
        labelZIndex    = (p.zBase or 10) + 1,
        selectedKey    = inst.model.activeTab,
        onChange       = function(key)
            if p.onTabChange then p.onTabChange(key) end
        end,
    })
    return tabbar.getStripHeight(inst.tabbarId)
end

-- Number of tab ROWS the strip will occupy, without creating anything.
-- Needed by the local vertical fit, which must know the strip's height
-- before it can decide the scale everything else is built at.
local function planTabRowCount(inst)
    local p = inst.params
    local t = p.tabs
    if not t then return 0 end
    if (t.mode or "row") ~= "wrapped" then return 1 end
    local labelScale = p.tabLabelUiscale or (p.uiscale or scale.get())
    local names = {}
    for i, td in ipairs(inst.model.tabs) do
        names[i] = td.name .. " (" .. td.count .. ")"
    end
    local widths = tabbar.measureTabs(p.font, names,
        math.floor((t.fontSize or 13) * labelScale),
        math.floor((t.textPad or 8) * labelScale))
    return #tabbar.planWrap(widths, t.wrapWidth or p.width, 0)
end

local function buildRows(inst, s, listY, listBottom)
    local p = inst.params
    local rowH    = math.floor((p.rowHeight or 32) * s)
    local rowPad  = math.floor((p.rowPad or 2) * s)
    local iconSz  = math.floor((p.iconSize or 28) * s)
    local textPad = math.floor((p.textPad or 12) * s)
    local inset   = math.floor((p.contentInset or 0) * s)
    local gap     = math.floor((p.nameRightGap or 24) * s)
    local fontSz  = p.rowFontSize or 13
    local fontPx  = math.floor(fontSz * (p.rowLabelUiscale or s))
    local baseline = math.floor(rowH / 2) + math.floor(fontSz * 0.3)
    local listX   = p.x + inset
    local listW   = p.width - 2 * inset
    local zBase   = p.zBase or 10

    local visible = inst.model.visible
    local maxRows = p.maxRows
    if not maxRows then
        maxRows = math.max(0,
            math.floor((listBottom - listY) / (rowH + rowPad)))
    end

    -- Scroll offset (#1238): how many leading rows of the ACTIVE TAB's
    -- filtered list are skipped. Clamped here, at the one point that
    -- knows both the row count and the visible capacity -- a host sizes
    -- its window before either exists. The clamped value is written
    -- back onto `inst` (and onto the params, so a rebuild from the same
    -- table starts where the widget actually was) so a host that
    -- restores a saved offset against SHRUNKEN contents lands on the
    -- last full page instead of an empty one.
    local capacity  = math.max(0, maxRows)
    local maxOffset = math.max(0, #visible - capacity)
    local offset    = math.floor(tonumber(p.scrollOffset) or 0)
    if offset < 0 then offset = 0 end
    if offset > maxOffset then offset = maxOffset end
    inst.rowCapacity  = capacity
    inst.maxScroll    = maxOffset
    inst.scrollOffset = offset
    p.scrollOffset    = offset

    if #visible == 0 then
        if p.emptyText then
            local lbl = label.new({
                name = inst.name .. "_empty", text = p.emptyText,
                font = p.font, fontSize = fontSz,
                color = p.emptyColor or { 0.7, 0.7, 0.7, 1.0 },
                page = p.page, uiscale = p.rowLabelUiscale or s,
            })
            local h = label.getElementHandle(lbl)
            UI.addToPage(p.page, h, listX + textPad, listY + baseline)
            UI.setZIndex(h, zBase + 1)
            track(inst, "label", lbl)
        end
        return
    end

    for i = 1, math.min(#visible - offset, capacity) do
        local row  = visible[offset + i]
        local rowY = listY + (i - 1) * (rowH + rowPad)

        -- Optional full-row backdrop (the unit-info equipped tint).
        local bg = p.rowBackground and p.rowBackground(row) or nil
        if bg then
            local bgId = UI.newSprite(inst.name .. "_rowbg_" .. i,
                listW, rowH, whitePixelTex,
                bg[1], bg[2], bg[3], bg[4], p.page)
            UI.addToPage(p.page, bgId, listX, rowY)
            UI.setZIndex(bgId, zBase)
            track(inst, "sprite", bgId)
        end

        -- Icon plus the shared broken-equipment overlay. A supplied
        -- rowIcon is AUTHORITATIVE, including when it returns nil: the
        -- item-contents host answers nil for a negative (missing) icon
        -- handle, and an `or row.iconTex` fallback would put that very
        -- handle back and render an undefined-texture sprite.
        local iconTex
        if p.rowIcon then iconTex = p.rowIcon(row) else iconTex = row.iconTex end
        if iconTex then
            local iconY = rowY + math.floor((rowH - iconSz) / 2)
            local iconId = UI.newSprite(inst.name .. "_icon_" .. i,
                iconSz, iconSz, iconTex, 1.0, 1.0, 1.0, 1.0, p.page)
            UI.addToPage(p.page, iconId, listX + textPad, iconY)
            UI.setZIndex(iconId, zBase + 1)
            track(inst, "sprite", iconId)
            local oid = brokenOverlay.add(p.page,
                inst.name .. "_broken_" .. i, row.condition,
                listX + textPad, iconY, iconSz, iconSz, zBase + 2)
            if oid then track(inst, "sprite", oid) end
        end

        -- Right-aligned weight column, built FIRST so its measured
        -- width bounds the name column's available space.
        local wW = 0
        local wText = p.rowWeightText and p.rowWeightText(row) or nil
        if wText then
            local wLbl = label.new({
                name = inst.name .. "_w_" .. i, text = wText,
                font = p.font, fontSize = fontSz,
                color = p.weightColor or { 0.85, 0.85, 0.85, 1.0 },
                page = p.page, uiscale = p.rowLabelUiscale or s,
            })
            local wH = label.getElementHandle(wLbl)
            wW = select(1, label.getSize(wLbl))
            UI.addToPage(p.page, wH,
                listX + listW - textPad - wW, rowY + baseline)
            UI.setZIndex(wH, zBase + 1)
            track(inst, "label", wLbl)
        end

        -- Display name, truncated so it never runs into the weight
        -- column. Measured at the size the label actually renders at.
        local nameX     = listX + textPad + iconSz + textPad
        local nameRight = listX + listW - textPad - wW - gap
        local nameMaxPx = math.max(0, nameRight - nameX)
        local rawName   = p.rowName and p.rowName(row)
                          or (row.displayName or row.defName or "?")
        local shownName = itemList.truncateToWidth(rawName, p.font,
                                                   fontPx, nameMaxPx)
        local nameLbl = label.new({
            name = inst.name .. "_name_" .. i,
            text = shownName,
            font = p.font, fontSize = fontSz,
            color = p.rowColor and p.rowColor(row) or { 1.0, 1.0, 1.0, 1.0 },
            page = p.page, uiscale = p.rowLabelUiscale or s,
        })
        local nameH = label.getElementHandle(nameLbl)
        UI.addToPage(p.page, nameH, nameX, rowY + baseline)
        UI.setZIndex(nameH, zBase + 1)
        track(inst, "label", nameLbl)

        -- Transparent full-row surface. It hosts the row's tooltip
        -- (hover hit-testing is purely geometric, so this works with or
        -- without a click callback) and, ONLY when the host supplied
        -- one, the right-click action. With no callback no click
        -- callback and no pointer-blocking opt-in is registered, so the
        -- row consumes nothing.
        local tip = p.rowTooltip and p.rowTooltip(row) or nil
        local hitId = UI.newSprite(inst.name .. "_hit_" .. i,
            listW, rowH, whitePixelTex, 1.0, 1.0, 1.0, 0.0, p.page)
        UI.addToPage(p.page, hitId, listX, rowY)
        UI.setZIndex(hitId, zBase + 3)
        if tip or p.onRowRightClick then
            UI.setClickable(hitId, true)
        end
        if p.onRowRightClick then
            UI.setOnRightClick(hitId, RIGHT_CLICK_CALLBACK)
        end
        if tip then UI.setTooltipRich(hitId, tip) end
        track(inst, "sprite", hitId)
        -- `text` is what the label ACTUALLY renders (post-truncation)
        -- and `rawText` the host's full composed row name; `tooltip` is
        -- the rich tip baked into the hit element. All three exist for
        -- dump() -- no Lua API reads a rendered label or an element's
        -- tooltip content back, so without them a probe could only
        -- assert its own expectations about what a row says (#1268).
        -- `index` is the SLOT (1 = topmost rendered row); `dataIndex`
        -- is the row's place in the filtered list, which is the only
        -- way an introspecting probe can tell a scrolled list from an
        -- unscrolled one (#1238).
        inst.rows[#inst.rows + 1] =
            { hitId = hitId, item = row, index = i, dataIndex = offset + i,
              text = shownName, rawText = rawName, tooltip = tip }
    end
end

local function build(inst)
    local p = inst.params
    local uiscale = p.uiscale or scale.get()

    -- Pads come either as a BASE value scaled with everything else (an
    -- embedded section that also opts into the local vertical fit) or
    -- as a final pixel value (a popup whose own height arithmetic
    -- already used a literal gap).
    local function pad(px, base, s)
        if px ~= nil then return px end
        return math.floor((base or 0) * s)
    end

    local headerH = p.header and math.floor((p.headerHeight or 20) * uiscale) or 0
    local topPad  = pad(p.tabTopPadPx, p.tabTopPad, uiscale)
    local botPad  = pad(p.tabBottomPadPx, p.tabBottomPad, uiscale)
    local footerH = p.footer and math.floor((p.footerHeight or 22) * uiscale) or 0

    -- Local vertical fit: shrink this widget's own chrome (never
    -- another section's scale) until the tab strip, at least one item
    -- row and the footer all fit the host-supplied height.
    local s = uiscale
    if p.fitVertical and p.height then
        local nTabRows = planTabRowCount(inst)
        local tabH     = math.floor((p.tabs and p.tabs.tabHeight or 28) * uiscale)
        local rowGap   = math.floor((p.tabs and p.tabs.rowGap or 0) * uiscale)
        local naturalMinH = headerH + topPad + nTabRows * tabH
            + math.max(0, nTabRows - 1) * rowGap + botPad
            + (math.floor((p.rowHeight or 32) * uiscale)
               + math.floor((p.rowPad or 2) * uiscale))
            + footerH
        s = responsive.fitScale(naturalMinH, p.height, uiscale)
        headerH = p.header and math.floor((p.headerHeight or 20) * s) or 0
        topPad  = pad(p.tabTopPadPx, p.tabTopPad, s)
        botPad  = pad(p.tabBottomPadPx, p.tabBottomPad, s)
        footerH = p.footer and math.floor((p.footerHeight or 22) * s) or 0
    end
    inst.scale = s

    local cursorY = p.y

    -- Optional capacity/weight header from host-supplied values. The
    -- widget never queries a capacity or inventory manager itself.
    if p.header then
        local text = string.format("%.2f / %.2f kg",
            p.header.stored or 0, p.header.capacity or 0)
        local lbl = label.new({
            name = inst.name .. "_header", text = text,
            font = p.font, fontSize = p.headerFontSize or 13,
            color = p.headerColor or { 0.85, 0.85, 0.85, 1.0 },
            page = p.page, uiscale = p.rowLabelUiscale or s,
        })
        local h = label.getElementHandle(lbl)
        UI.addToPage(p.page, h, p.x + math.floor((p.contentInset or 0) * s),
            cursorY + math.floor((p.headerFontSize or 13) * 0.85))
        UI.setZIndex(h, (p.zBase or 10) + 1)
        track(inst, "label", lbl)
        cursorY = cursorY + headerH
    end

    if p.tabs then
        cursorY = cursorY + topPad
        local stripH = buildTabs(inst, s, cursorY)
        cursorY = cursorY + stripH + botPad
    end

    local listBottom = (p.height and (p.y + p.height - footerH))
                       or (cursorY + footerH)
    buildRows(inst, s, cursorY, listBottom)

    -- Optional footer, right-aligned along the bottom edge.
    if p.footer then
        local inset = math.floor((p.contentInset or 0) * s)
        local textPad = math.floor((p.textPad or 12) * s)
        local fontSz = p.footerFontSize or p.rowFontSize or 13
        local lbl = label.new({
            name = inst.name .. "_footer", text = p.footer.text or "",
            font = p.font, fontSize = fontSz,
            color = p.footer.color or { 0.85, 0.85, 0.85, 1.0 },
            page = p.page, uiscale = p.rowLabelUiscale or s,
        })
        local h = label.getElementHandle(lbl)
        local w = select(1, label.getSize(lbl))
        UI.addToPage(p.page, h,
            p.x + inset + (p.width - 2 * inset) - textPad - w,
            p.y + (p.height or 0) - footerH
                + math.floor(footerH / 2) + math.floor(fontSz * 0.3))
        UI.setZIndex(h, (p.zBase or 10) + 1)
        track(inst, "label", lbl)
    end
end

-----------------------------------------------------------
-- Creation / destruction
-----------------------------------------------------------

function itemList.new(p)
    if not whitePixelTex then itemList.init() end
    local id = nextId
    nextId = nextId + 1
    local data, layout, model = signatures(p, p.model)
    local inst = {
        id = id,
        name = p.name or ("item_list_" .. id),
        params = p,
        model = model,
        elements = {},
        rows = {},
        dataSig = data,
        layoutSig = layout,
    }
    lists[id] = inst
    build(inst)
    return id
end

function itemList.destroy(id)
    local inst = lists[id]
    if not inst then return end
    destroyElements(inst)
    lists[id] = nil
end

function itemList.destroyAll()
    for id, _ in pairs(lists) do itemList.destroy(id) end
    lists = {}
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function itemList.getModel(id)
    local inst = lists[id]
    return inst and inst.model or nil
end

function itemList.getActiveTab(id)
    local inst = lists[id]
    return inst and inst.model.activeTab or nil
end

-- Tab records straight from the tab strip that actually exists
-- ({name, key, width, height, x, y, boxId, labelId}).
function itemList.getTabs(id)
    local inst = lists[id]
    if not inst or not inst.tabbarId then return {} end
    return tabbar.getTabs(inst.tabbarId)
end

function itemList.getTabBarId(id)
    local inst = lists[id]
    return inst and inst.tabbarId or nil
end

-- Rendered rows: { hitId, item, index }. `item` is the representative
-- instance of its group.
function itemList.getRows(id)
    local inst = lists[id]
    return inst and inst.rows or {}
end

function itemList.getScale(id)
    local inst = lists[id]
    return inst and inst.scale or nil
end

-----------------------------------------------------------
-- Scrolling (#1238)
--
-- A window whose contents outgrow its row cap has to be reachable, and
-- the nesting stack has to be able to SAVE and RESTORE where each of
-- its levels was. So the offset is real widget state, not a host's
-- private bookkeeping: the widget owns the clamp (only it knows the
-- visible capacity), reports the clamped value back, and a host that
-- restores an offset against changed contents gets the same clamp for
-- free.
--
-- Deliberately NOT part of the invalidation signature: scrolling is a
-- pure re-render of unchanged data, so a host must never see its own
-- scroll as "the data went stale". `setScrollOffset` rebuilds the rows
-- itself instead.
-----------------------------------------------------------

-- The offset actually in effect (post-clamp), 0 for a never-scrolled
-- or unknown list.
function itemList.getScrollOffset(id)
    local inst = lists[id]
    return inst and inst.scrollOffset or 0
end

-- The largest offset this list's current contents admit: rows beyond
-- the visible capacity, never negative.
function itemList.maxScrollOffset(id)
    local inst = lists[id]
    return inst and inst.maxScroll or 0
end

-- How many rows fit at once (the host's `maxRows`, or the count the
-- supplied bounds allow).
function itemList.rowCapacity(id)
    local inst = lists[id]
    return inst and inst.rowCapacity or 0
end

-- Scroll to an absolute offset; returns the CLAMPED offset actually
-- applied. A no-op request rebuilds nothing.
function itemList.setScrollOffset(id, offset)
    local inst = lists[id]
    if not inst then return 0 end
    local want = math.floor(tonumber(offset) or 0)
    if want < 0 then want = 0 end
    if inst.maxScroll and want > inst.maxScroll then want = inst.maxScroll end
    if want == inst.scrollOffset then return inst.scrollOffset end
    inst.params.scrollOffset = want
    destroyElements(inst)
    build(inst)
    return inst.scrollOffset
end

-- Relative scroll (one wheel notch is one row); returns the clamped
-- offset actually applied.
function itemList.scrollBy(id, delta)
    local inst = lists[id]
    if not inst then return 0 end
    return itemList.setScrollOffset(id,
        (inst.scrollOffset or 0) + math.floor(tonumber(delta) or 0))
end

function itemList.count()
    local n = 0
    for _ in pairs(lists) do n = n + 1 end
    return n
end

-----------------------------------------------------------
-- Click routing
-----------------------------------------------------------

function itemList.isItemListCallback(callbackName)
    return callbackName == RIGHT_CLICK_CALLBACK
end

-- Route a row right-click back to the owning host with the EXACT
-- rendered row and its representative identity.
function itemList.handleCallback(callbackName, elemHandle)
    if callbackName ~= RIGHT_CLICK_CALLBACK then return false end
    for _, inst in pairs(lists) do
        local cb = inst.params.onRowRightClick
        if cb then
            for _, r in ipairs(inst.rows) do
                if r.hitId == elemHandle then
                    return cb(r.item, r) and true or false
                end
            end
        end
    end
    return false
end

-----------------------------------------------------------
-- Introspection (F3, #645)
-----------------------------------------------------------

-- One entry per rendered row. Reported even when the row carries no
-- right-click action (item contents) -- registry.lua's generic
-- fallback pass only sees elements that HAVE a click callback, so
-- without this a probe could not verify that panel's rows at all.
--
-- `label` stays the bare item name it always was (probes filter on
-- it). What a row PRESENTS is separate: `text` is the string the label
-- renders after truncation, `rawText` the host's full composed name
-- before it, `tempSummary` the group summary both that and the tooltip
-- derive from, and `tooltipText`/`tooltipHint` the rich tip's two
-- halves. Those five are the only read path for either surface --
-- nothing in the Lua API reports a rendered label or an element's
-- tooltip content (#1268).
function itemList.dump()
    local out = {}
    for id, inst in pairs(lists) do
        for _, r in ipairs(inst.rows) do
            local info = r.hitId and UI.getElementInfo(r.hitId) or nil
            if info and info.pageVisible and info.visible then
                table.insert(out, {
                    id = "item_list:" .. id .. ":" .. r.index,
                    name = inst.name .. "_row_" .. r.index,
                    listId = id,
                    slot = r.index,
                    dataIndex = r.dataIndex,
                    scrollOffset = inst.scrollOffset or 0,
                    type = "item_list",
                    bounds = {
                        x = info.x, y = info.y,
                        w = info.width, h = info.height,
                    },
                    label = r.item.displayName or r.item.defName or "?",
                    text        = r.text,
                    rawText     = r.rawText,
                    tempSummary = r.item.tempSummary,
                    tooltipText = type(r.tooltip) == "table"
                                    and r.tooltip.text or nil,
                    tooltipHint = type(r.tooltip) == "table"
                                    and r.tooltip.hint or nil,
                    count = r.item.count or 1,
                    category = itemList.normalizeCategory(r.item.category),
                    defName = r.item.defName,
                    instanceId = r.item.instanceId,
                    -- The row's COMPLETE membership (#1249), so an
                    -- offscreen probe can prove "Store all" queued every
                    -- id the row stands for rather than trusting the
                    -- count. Reported through the same accessor the
                    -- gestures use, so the two can never disagree.
                    instanceIds = itemList.rowInstanceIds(r.item),
                    rightClick = inst.params.onRowRightClick ~= nil,
                    enabled = info.clickable,
                    visible = info.visible,
                    hovered = info.hovered,
                    focused = info.focused,
                    value = false,
                    screen = info.page,
                    handle = info.handle,
                })
            end
        end
    end
    return out
end

return itemList
