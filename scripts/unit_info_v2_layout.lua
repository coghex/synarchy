-- Unit info v2 shared layout constants (#542 split from unit_info_v2.lua).
--
-- Pane/section sizing, colours, and sub-tab styling constants shared
-- by two or more of the split's section modules, plus the two small
-- generic layout helpers (planSubTabRows, placeDivider) that don't
-- belong to any single section either. Kept together rather than
-- guessing which future module might need which constant.

local responsive = require("scripts.ui.responsive")

local M = {}

-----------------------------------------------------------
-- Layout constants (base size; multiplied by uiscale)
-----------------------------------------------------------

M.PANEL_W       = 340     -- pane width
M.PANEL_PAD     = 12      -- outer padding inside the pane
M.SECTION_PAD   = 18      -- horizontal padding around section text (clears the 9-patch border)
M.SECTION_GAP   = 10      -- vertical gap between section content and divider

M.TABS_H    = 88   -- room for extra top padding above the tab row
M.HEADER_H  = 84   -- 4 rows × ~21px (Name / Type / Role / Action)
M.STATS_H   = 280
M.EQUIP_H   = 272   -- fits a 256-tall humanoid silhouette + 8px top/bot
-- Inventory section takes remaining vertical room.

-- Equipment section layout. Silhouette + slot grid on the left,
-- accessory list on the right. SILHOUETTE_PAD is the gap between the
-- pane border and the silhouette; ACCESSORY_GAP separates the
-- silhouette from the right-side accessory list.
M.SILHOUETTE_PAD = 8
M.ACCESSORY_GAP  = 12

-- Inventory section layout. Tab strip on top (re-uses the menu tab
-- 9-patch like the stats sub-tabs), scrollable item list below, total
-- weight footer at the bottom.
M.INV_TAB_FONT_SIZE  = 13
M.INV_TAB_TEXT_PAD   = 8     -- horizontal padding inside each tab
M.INV_TAB_H          = 26
M.INV_TAB_GAP        = 0     -- flush like the sub-tabs
M.INV_TAB_TOP_PAD    = 6
M.INV_TAB_BOTTOM_PAD = 6
M.INV_ROW_H          = 32    -- one row per item (matches icon size)
M.INV_ROW_PAD        = 2
M.INV_ICON_SIZE      = 28
M.INV_FOOTER_H       = 22
M.INV_TEXT_PAD       = 8     -- horizontal pad inside a row
M.INV_EQUIP_TINT     = { 1.0, 0.95, 0.55, 0.18 }  -- soft amber bg for equipped rows

M.TAB_GAP          = 4    -- horizontal gap between adjacent tabs
M.TAB_INNER_PAD    = 4    -- padding inside a tab around its sprite
M.TAB_TOP_PAD      = 12   -- extra space between section top and the tab row
M.TAB_BOTTOM_PAD   = 4    -- space between the tab row and the bottom of the strip
M.ARROW_W          = 24   -- scroll-arrow button width
M.ARROW_GAP        = 4    -- gap between arrows and tab strip
M.ARROW_PANEL_INSET = 12  -- extra inset so arrows sit visibly inside the pane's 9-patch border
-- Active tab's backdrop is a shaped sprite (assets/.../unittabselected.png).
-- Inactive tabs hide the backdrop entirely (alpha 0). The portrait
-- sprite is the click target and renders on top.
M.ACTIVE_BG      = { 1.0, 1.0, 1.0, 1.0 }   -- show the texture untinted
M.INACTIVE_BG    = { 1.0, 1.0, 1.0, 0.0 }
M.ACTIVE_TINT    = { 1.0, 1.0, 1.0, 1.0 }
M.INACTIVE_TINT  = { 1.0, 1.0, 1.0, 1.0 }
-- Arrows are click-only — no visible background. The chevron label
-- supplies the visual.
M.ARROW_BG       = { 1.0, 1.0, 1.0, 0.0 }

M.OUTER_TILE = 64           -- tile size for the big outer pane (uses bigger 9-patch corners)
M.DIVIDER_THICKNESS = 2     -- horizontal rule between sections
M.DIVIDER_INSET     = 24    -- horizontal gap between divider and pane edge
M.DIVIDER_COLOR     = { 0.7, 0.7, 0.7, 1.0 }   -- same grey as tooltip separator

-- Sub-tabs (Status, Stats, Mental, …) inside the stats section, and
-- reused by the inventory section's category tab strip.
-- Styled to match scripts/ui/tabbar.lua (settings + create-world menus):
-- 9-patch box, dark text on unselected, white text on selected.
M.SUB_TAB_LIST       = { "Status", "Physical", "Mental", "Skill", "Knowledge" }
M.SUB_TAB_FONT_SIZE  = 14
M.SUB_TAB_TEXT_PAD   = 10   -- horizontal padding inside each sub-tab around the label
M.SUB_TAB_ROW_H      = 32   -- per-row height — matches settingsMenu.baseSizes.tabHeight
M.SUB_TAB_TILE       = 16   -- 9-patch corner size for the tab box textures
M.SUB_TAB_GAP        = 0    -- menu-style tabs sit flush against each other
M.SUB_TAB_ROW_GAP    = 4    -- vertical gap between wrapped rows
M.SUB_TAB_TOP_PAD    = 6    -- gap below the section's top divider before the first row
M.SUB_TAB_TEXT_COLOR     = { 0.0, 0.0, 0.0, 1.0 }
M.SUB_TAB_SEL_TEXT_COLOR = { 1.0, 1.0, 1.0, 1.0 }

-- Content rows inside a stats panel (icon on the left, value on the right).
-- Hovering the icon brings up the stat's tooltip (name + description).
M.CONTENT_ROW_H      = 48
M.CONTENT_FONT_SIZE  = 20
-- Conditions / injuries use a smaller font than the big stat numbers — the
-- name reads better small, severity is shown by COLOUR (severityColor) with
-- the word + effects in the tooltip.
M.CONDITION_FONT_SIZE = 14
M.CONTENT_LEFT_PAD   = 16
M.CONTENT_RIGHT_PAD  = 16
M.CONTENT_DIM_COLOR  = { 0.78, 0.78, 0.78, 1.0 }
M.CONTENT_VAL_COLOR  = { 1.0, 1.0, 1.0, 1.0 }
M.CONTENT_TOP_PAD    = 8
M.ICON_SIZE          = 40
-- Reserve this many pixels on the right when a scrollbar is needed.
-- Sized to fit the scrollbar widget (button 24 + gap 8) at uiscale 1.
M.SCROLLBAR_RESERVE  = 32

-----------------------------------------------------------
-- Shared helpers
-----------------------------------------------------------

-- Compute and capture sub-tab row breakdown given the section rect.
-- Returns a list of rows: { {start, end, totalW, count}, ... } using
-- pre-measured tab widths. Shared by the stats sub-tab strip and the
-- inventory category tab strip.
function M.planSubTabRows(rect, tabWidths)
    local availW = rect.w - 2 * M.SECTION_PAD
    local rows = {}
    local cur = { startIdx = 1, totalW = 0, count = 0 }
    for i = 1, #tabWidths do
        local tw = tabWidths[i]
        local extra = (cur.count > 0) and M.SUB_TAB_GAP or 0
        if cur.count > 0 and cur.totalW + extra + tw > availW then
            cur.endIdx = i - 1
            rows[#rows + 1] = cur
            cur = { startIdx = i, totalW = tw, count = 1 }
        else
            cur.totalW = cur.totalW + extra + tw
            cur.count = cur.count + 1
        end
    end
    cur.endIdx = #tabWidths
    rows[#rows + 1] = cur
    return rows
end

-- Horizontal rule between sections. Drawn as a thin tinted sprite
-- using the same 1×1 white pixel + grey tint pattern as the tooltip
-- separator, so the visual language matches. Inset on each side so
-- the rule visibly stops short of the pane's inner border instead of
-- running right up to (or through) the 9-patch corner pixels.
function M.placeDivider(unitInfoV2, x, y, w, uiscale)
    if not unitInfoV2.whitePixelTex then return end
    local inset = math.floor(M.DIVIDER_INSET * uiscale)
    local sprId = UI.newSprite(
        "unit_info_v2_div_" .. tostring(#unitInfoV2.dividerIds + 1),
        w - 2 * inset, M.DIVIDER_THICKNESS,
        unitInfoV2.whitePixelTex,
        M.DIVIDER_COLOR[1], M.DIVIDER_COLOR[2],
        M.DIVIDER_COLOR[3], M.DIVIDER_COLOR[4],
        unitInfoV2.page
    )
    UI.addToPage(unitInfoV2.page, sprId, x + inset, y)
    UI.setZIndex(sprId, 11)
    table.insert(unitInfoV2.dividerIds, sprId)
end

-- #750 round-16 review: at a narrow, high-scale, still-C2-supported
-- combination (e.g. 800x2160@4x), the 4 fixed section heights below
-- (tabs/header/stats/equipment) alone — before gap/divider overhead —
-- already exceed the whole framebuffer, driving inventory's remaining
-- height negative (silently omitted) and pushing equipment's own rect
-- past the bottom edge. Fits a LOCAL, vertical-only scale for these
-- heights (never contentW/panelW's own scale, or any section
-- submodule's own internal uiscale) against whatever height remains
-- after reserving a minimum sliver for inventory — the same
-- `responsive.fitScale` technique used elsewhere in this codebase for
-- an analogous "fixed chrome doesn't fit the available space" gap.
-- Best-effort: each section's own CONTENT (rendered by its own
-- submodule) still uses the full uiscale internally — a full content
-- re-flow across five independent submodules is a follow-up — but
-- this guarantees every section's RECT, and so inventory's own
-- existence, stays within the framebuffer and reachable. Returns
-- outerPad, sectGap, tabsH, headerH, statsH, equipH, dThick, minInvH.
function M.fitVerticalSections(uiscale, fbH)
    local outerPad = math.floor(M.PANEL_PAD * uiscale)
    local sectGap  = math.floor(M.SECTION_GAP * uiscale)
    local tabsH    = math.floor(M.TABS_H   * uiscale)
    local headerH  = math.floor(M.HEADER_H * uiscale)
    local statsH   = math.floor(M.STATS_H  * uiscale)
    local equipH   = math.floor(M.EQUIP_H  * uiscale)
    local dThick   = math.floor(M.DIVIDER_THICKNESS * uiscale)
    local minInvH  = math.floor(60 * uiscale)

    local perSectionOverhead = 2 * sectGap + dThick
    local fixedNaturalTotal = 2 * outerPad
        + (tabsH + headerH + statsH + equipH) + 4 * perSectionOverhead
    local vScale = responsive.fitScale(
        fixedNaturalTotal, math.max(minInvH, fbH - minInvH), uiscale)
    if vScale < uiscale then
        outerPad = math.floor(M.PANEL_PAD          * vScale)
        sectGap  = math.floor(M.SECTION_GAP        * vScale)
        tabsH    = math.floor(M.TABS_H             * vScale)
        headerH  = math.floor(M.HEADER_H           * vScale)
        statsH   = math.floor(M.STATS_H            * vScale)
        equipH   = math.floor(M.EQUIP_H            * vScale)
        dThick   = math.floor(M.DIVIDER_THICKNESS  * vScale)
    end
    return outerPad, sectGap, tabsH, headerH, statsH, equipH, dThick, minInvH
end

return M
