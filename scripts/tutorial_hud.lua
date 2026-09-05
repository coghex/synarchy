-- Tutorial objective CHECKLIST HUD (#960, phase 3 of the tutorial epic
-- #956). #957 defined the tree, #958 owns progress and the view model,
-- #959 evaluates it; this module is the only thing that DRAWS any of
-- it, and it draws nothing else.
--
-- It is a gameplay HUD surface, not a settings screen, a modal, or an
-- interactive quest log. Three consequences shape the whole file:
--
--   * Everything rendered comes from `tutorialProgress.getViewModel()`.
--     This module never calls `engine.getTutorialTree()` and never
--     touches the tutorial registry: reveal/hide is #958's rule, stated
--     once, and duplicating it here is exactly the drift that split the
--     two slices apart. It also means the hspec gate can drive real
--     rendering by injecting a tree through `tutorialProgress.setTree`.
--   * This surface is also #958's PRESENTATION witness (#1941). It is
--     the only thing that can say a row was actually put in front of
--     the player, so it reports exactly the rows it laid out on a
--     visible, open page back through
--     `tutorialProgress.acknowledgePresented`, which retires the #996
--     hide suppression that kept an already-latched branch on screen.
--     Reading the view model is not presentation and never acknowledges
--     anything.
--     Since #2056 a VISIBLE, OPEN page is necessary but not
--     sufficient: showing a page and deleting its elements are both
--     synchronous writes to the shared UI-manager ref from this
--     thread, and the renderer reads that ref only when it snapshots
--     it, so this module could once show, acknowledge and destroy a
--     whole branch inside one uninterrupted call. It now arms a
--     presentation token (`UI.armPresentation`) after every change to
--     its viewport and acknowledges only once `UI.isPresented` says a
--     COMPLETED renderer snapshot held that exact viewport. See
--     `armPresentation` below and the Haskell-side contract in
--     UI.Manager.Presentation.
--   * Rows are DISPLAY-ONLY. They carry a tooltip and capture the
--     wheel, and that is all: no click callback and no pointer
--     blocking, so a click over the checklist reaches the terrain
--     underneath (#743's three input policies are independent —
--     src/UI/Manager/Query.hs's elementBlocksPointer /
--     elementCapturesScroll). Only the toggle is a real control.
--   * `completed` and `checked` are NEVER collapsed into one
--     "checkbox". A full/composite objective LATCHES (durable,
--     monotonic); a subobjective CHECKS (live, reversible, recomputed
--     every session). The view model reports exactly one of them per
--     row and so does this module's rendering (`[x]` vs `(x)`) and its
--     dump().
--
-- Presentation state (open/closed + scroll offset) lives here on the
-- package.loaded singleton and is deliberately NOT persisted -- but a
-- Lua singleton outlives a save load and a mid-process world
-- generation, so both entry points reset it explicitly:
-- `onSaveLoaded` (the one broadcast every load trigger reaches, menu-
-- driven or console) and `worldManager.createWorld` (the one funnel a
-- newly generated world goes through, which already resets #958's
-- progress for the same reason). Evaluation keeps running while the
-- panel is closed -- scripts/tutorial_eval.lua ticks independently of
-- anything here.

local tutorialHud = package.loaded["scripts.tutorial_hud"] or {}
package.loaded["scripts.tutorial_hud"] = tutorialHud

local scale           = require("scripts.ui.scale")
local reservedRegions = require("scripts.ui.reserved_regions")
local responsive      = require("scripts.ui.responsive")
local textWrap        = require("scripts.ui.text_wrap")

-- Presentation state. Survives a rebuild, a resize, a UI-scale change,
-- a HUD hide/show and a zoom-band change; reset only by the two
-- session-entry hooks at the bottom of this file.
tutorialHud.open         = tutorialHud.open         or false
tutorialHud.scrollOffset = tutorialHud.scrollOffset or 0

tutorialHud.page = tutorialHud.page or nil
tutorialHud.fbW  = tutorialHud.fbW  or 0
tutorialHud.fbH  = tutorialHud.fbH  or 0

-- Requirement 7 introspection: how many times the surface has been
-- rebuilt from scratch. A real framebuffer resize must bump this by
-- exactly ONE across the whole two-step path below -- the engine
-- broadcast records the new size, and the single rebuild happens in
-- the post-hud `reflow()`.
tutorialHud.rebuildCount = tutorialHud.rebuildCount or 0

-- Live element bookkeeping (handles are invalid after a rebuild).
tutorialHud._els        = tutorialHud._els        or {}
tutorialHud._rows       = tutorialHud._rows       or {}
tutorialHud._toggle     = tutorialHud._toggle     or nil
tutorialHud._hudVisible = tutorialHud._hudVisible or false
tutorialHud._sig        = tutorialHud._sig        or nil
tutorialHud._toggleLabel  = tutorialHud._toggleLabel  or nil
tutorialHud._assetsReady  = tutorialHud._assetsReady  or false
-- #2056: the presentation token standing for the CURRENT viewport --
-- these rows, on this page, at this visibility. Re-minted by
-- armPresentation() below every time any of those three change, so
-- evidence gathered for an older viewport can never authorise this one.
-- nil means nothing is armed, which acknowledges nothing.
tutorialHud._presentToken = tutorialHud._presentToken or nil

local TOGGLE_CALLBACK = "onTutorialHudToggle"

-- Unscaled layout constants. Every runtime size is one of these times
-- the live UI scale, with ONE deliberate exception: the toggle box is
-- also fitted to its own measured caption (#1419 -- `toggleW` alone is
-- narrower than "> Objectives" renders at the shipped font, so the last
-- characters painted past the box AND past the right edge of a
-- 1280x720 frame). `engine.getTextWidth` reports 0 in the bare-Lua
-- headless UI fixture, so that measurement is treated as OPTIONAL: a 0
-- (or absent font) leaves the box at exactly its historical constant
-- width, which is what keeps row height, scroll range and every
-- existing geometry assertion measurement-free there. A test that wants
-- the fitted path stubs `engine.getTextWidth`, the idiom
-- Test.Headless.UI.ResponsiveGameplay already uses.
local BASE = {
    panelW   = 240,
    rowH     = 20,
    fontSize = 14,
    indent   = 12,
    margin   = 8,
    gap      = 4,
    toggleW  = 132,
    toggleH  = 22,
}

-- The gameplay HUD, if it has been required at all. Read through
-- package.loaded rather than require() so this module never forces
-- scripts/hud.lua to load (world_manager.lua reaches us from its
-- createWorld funnel, which must not drag the HUD in behind it).
local function hudModule()
    return package.loaded["scripts.hud"]
end

-- The shared font and 9-slice box textures this surface draws with.
-- Both are owned by scripts/hud.lua and only exist once ui_manager has
-- run hud.init -- which happens well AFTER this module is
-- engine.loadScript'd (scripts/init_loader.lua loads it at ~line 164,
-- ui_manager at ~line 246, and hud.init is further gated on fontsReady,
-- i.e. a real GPU font atlas). So the very first rebuild legitimately
-- has neither, and produces an unlabelled, textureless toggle and no
-- row text.
local function hudAssets()
    local hudMod = hudModule()
    if hudMod == nil then return nil, nil end
    return hudMod.menuFont, hudMod.boxTexSet
end

-- Toolbar clusters currently on screen. hud.getToolbarRects() is the
-- authoritative list (log / map / tool); an empty result simply means
-- the toolbar does not exist yet, which is not an error here.
local function toolbarRects()
    local hudMod = hudModule()
    if hudMod and type(hudMod.getToolbarRects) == "function" then
        local ok, rects = pcall(hudMod.getToolbarRects)
        if ok and type(rects) == "table" then return rects end
    end
    return {}
end

-----------------------------------------------------------
-- Caption measurement
--
-- The toggle draws one of exactly two captions, and #1419's defect was
-- that neither fits the constant box: at 1280x720 @1x the shipped font
-- painted both of them across glyph columns x=1147..1279 inside a box
-- whose exclusive right edge was x=1272, so the control read
-- "> Objecti" (PR #991 recorded the same overrun from x=1143). The box
-- is therefore fitted to the caption rather than the other way round.
--
-- BOTH variants are measured and the WIDER one reserved, because the
-- toggle rect is contractually identical open and closed (the "small
-- reachable right-edge toggle" guarantee, gated in the "Tutorial HUD"
-- suite): a box that tracked only the live caption would jump sideways
-- on every toggle.
--
-- Measurement is OPTIONAL by design. `engine.getTextWidth` needs a real
-- GPU font atlas and answers 0 in the bare-Lua headless UI fixture; a 0
-- (or a missing font, or a missing API) yields 0 here and leaves the
-- box at its historical constant width, so nothing in that fixture
-- becomes measurement-dependent. Non-finite and negative answers are
-- discarded the same way rather than propagated into geometry.
-----------------------------------------------------------

local function captionText(open)
    return (open and "v " or "> ") .. "Objectives"
end

local CAPTIONS = { captionText(false), captionText(true) }

-- Pad reserved on EACH side of the caption inside the box. Glyph ink is
-- placed at the pen position plus the glyph's bearing and is bounded by
-- its bitmap, not by the advance sum engine.getTextWidth returns
-- (Engine.Graphics.Font.Draw.layoutTextUI vs
-- Engine.Graphics.Font.Util.calculateTextWidthScaled), so both ends need
-- slack. This is the same expression the caption's own x offset already
-- used, now reserved on the right as well.
local function captionPadFor(indent)
    return math.max(2, math.floor(math.max(0, indent) / 2))
end

-- Widest rendered caption at `fontSize`, in whole pixels; 0 when
-- nothing can measure.
local function captionWidth(font, fontSize)
    if font == nil then return 0 end
    if type(engine) ~= "table" or type(engine.getTextWidth) ~= "function" then
        return 0
    end
    local widest = 0
    for _, text in ipairs(CAPTIONS) do
        local ok, w = pcall(engine.getTextWidth, font, text, fontSize)
        -- `w ~= w` rejects NaN; the upper bound rejects inf.
        if ok and type(w) == "number" and w == w and w < math.huge
           and w > widest then
            widest = w
        end
    end
    return math.ceil(widest)
end

-- #1581: fit one row's rendered string into the width its indent leaves
-- inside the panel, through the ONE shared width-fitting helper (#1157)
-- rather than a sixth private copy of the truncation rule.
--
-- Guarded exactly like captionWidth above, and for the same reason: the
-- headless UI fixture runs a bare Lua backend where engine.getTextWidth
-- may be absent or answer 0, and an unmeasurable font means "no bound at
-- all" -- the pre-#1581 string, unchanged. A raising helper must never
-- cost the row its text.
local function fitRowText(text, font, fontSize, budget)
    if font == nil then return text end
    if type(engine) ~= "table" or type(engine.getTextWidth) ~= "function" then
        return text
    end
    local ok, fitted = pcall(textWrap.truncateToWidth,
                             text, font, fontSize, budget)
    if ok and type(fitted) == "string" then return fitted end
    return text
end

-- How many times the box/font fit below may be re-derived. One
-- responsive.fitScale pass would suffice with exact arithmetic, but
-- scale.applyAllWith FLOORS every scaled size and the font size has its
-- own floor, so the re-derived natural width can land a pixel over
-- budget. Bounded regardless: the loop also stops as soon as a pass
-- stops making progress.
local TOGGLE_FIT_PASSES = 4

-- Everything the toggle box draws with, fitted as ONE unit: the box's
-- width and height and the caption's font size and pad all come from a
-- single local effective scale, per CLAUDE.md's "shrink a box's font
-- together with its box, never separately". The caption is never shrunk
-- on its own, and the stored/configured UI scale is never touched --
-- only this one control's layout.
--
-- `availW`/`availH` are the space the right edge can actually give the
-- control. When the natural width already fits, `uiscale` is returned
-- unchanged and the result is byte-identical to the pre-#1419 geometry
-- for an unmeasurable caption.
local function fitToggle(uiscale, availW, availH, font)
    local eff, fit = uiscale, nil
    for _ = 1, TOGGLE_FIT_PASSES do
        local s        = scale.applyAllWith(BASE, eff)
        local fontSize = math.max(6, s.fontSize)
        local pad      = captionPadFor(s.indent)
        local capW     = captionWidth(font, fontSize)
        local natural  = math.max(s.toggleW, pad * 2 + capW)
        fit = { w = natural, h = math.max(1, math.min(s.toggleH, availH)),
                fontSize = fontSize, pad = pad, capW = capW }
        if natural <= availW then break end
        -- fitScale clamps at its own floor, so a pass that cannot
        -- shrink any further reports the same scale back; stop there
        -- instead of spinning. Below the formal minimum framebuffer the
        -- caption simply cannot fit, and the caller's clamp keeps the
        -- geometry valid (degrade, never crash -- responsive.lua).
        local nextEff = responsive.fitScale(natural, availW, eff)
        if not (nextEff < eff) then break end
        eff = nextEff
    end
    return fit
end

-----------------------------------------------------------
-- Geometry
-----------------------------------------------------------

-- Right-anchored geometry for a given framebuffer and active-row count.
--
-- The toggle rect depends ONLY on the framebuffer, the UI scale, the
-- toolbar clusters and the font's own caption metrics (#1419, which
-- measures BOTH caption variants precisely so this stays true) -- never
-- on the open/closed state and never on the row count. That is what
-- makes "small reachable right-edge toggle" verifiable: its bounds are
-- assertably identical open and closed, and the list is laid out so its
-- BOTTOM edge meets the toggle's top edge (rows stack upward from the
-- toggle) instead of the toggle sliding around underneath a growing
-- list.
--
-- Out-of-envelope combinations degrade best-effort per
-- scripts/ui/responsive.lua: floors and clamps keep every rect
-- non-negative and inside the framebuffer rather than producing
-- invalid geometry.
local function computeLayout(fbW, fbH, rowCount)
    local s = scale.applyAll(BASE)
    local rowH    = math.max(1, s.rowH)
    local margin  = math.max(0, s.margin)
    local gap     = math.max(0, s.gap)

    -- Width: capped to the framebuffer and, like unit_info_v2's
    -- flush-right column, to the space remaining right of every
    -- toolbar cluster that is NOT the bottom-right map toggle (which
    -- this surface clears vertically instead, below).
    --
    -- Deliberately does NOT constrain against unit_info_v2's own
    -- flush-right, full-height pane (reserved_regions PRIORITY 90).
    -- That pane spans the entire height whenever a unit is selected,
    -- so yielding to it would mean either giving up the right edge
    -- entirely or moving the toggle whenever the selection changes --
    -- and a toggle whose screen bounds shift with unrelated gameplay
    -- state is exactly what requirement 2's "small reachable
    -- right-edge toggle" and requirement 6's reachability guarantee
    -- rule out. This is therefore an ACCEPTED, documented overlap,
    -- the same call unit_info_v2 itself made about its own
    -- pre-existing map_toggle overlap: the checklist is transparent,
    -- non-blocking text, so it never makes that pane unusable, and
    -- the checklist starts collapsed and is closed by one click.
    local rects = toolbarRects()
    local sideClusters = {}
    for _, rc in ipairs(rects) do
        if rc.name ~= "map_toggle" then
            sideClusters[#sideClusters + 1] = rc
        end
    end
    --
    -- The readable-width floor is itself capped to the framebuffer: a
    -- flat `math.max(20, ...)` would defeat the framebuffer cap on any
    -- positive width under 20px and push the toggle straight off the
    -- right edge (panelX/toggleX clamp to 0, so the extra width has
    -- nowhere to go) -- an INVALID out-of-envelope geometry rather
    -- than a degraded one.
    local minWidth = math.max(1, math.min(20, fbW))
    local panelW = math.min(s.panelW, fbW,
        reservedRegions.maxRightAnchoredWidth(0, fbH, sideClusters, fbW))
    panelW = math.max(minWidth, panelW)

    -- The toggle's own fit, against the width the right edge can give
    -- it (panelW already carries the framebuffer cap and the reserved
    -- toolbar clusters). Its natural width is whatever holds the wider
    -- caption; only when that exceeds panelW does the effective scale
    -- shrink the box and its font together.
    local font   = hudAssets()
    local fit    = fitToggle(scale.get(), panelW, math.max(1, fbH), font)
    local toggleH = fit.h
    local toggleW = math.max(math.min(minWidth, panelW),
                              math.min(fit.w, panelW))
    local panelX  = math.max(0, fbW - panelW - margin)
    local toggleX = math.max(0, fbW - toggleW - margin)

    -- Sit above the bottom edge, and above any toolbar cluster whose
    -- horizontal span overlaps the toggle's (in practice the
    -- bottom-right map toggle). This is deterministic, unlike a
    -- minimal-displacement nudge, so the toggle never jumps sideways
    -- as unrelated clusters appear.
    local bottom = fbH - margin
    for _, rc in ipairs(rects) do
        if rc.x < toggleX + toggleW and toggleX < rc.x + rc.w then
            bottom = math.min(bottom, rc.y - gap)
        end
    end
    local toggleY = math.max(0, math.min(bottom - toggleH,
                                          math.max(0, fbH - toggleH)))

    -- Clamped at 0 so a framebuffer too short to hold the toggle plus a
    -- margin reports an empty viewport at the top edge rather than a
    -- negative one.
    local listBottom = math.max(0, toggleY - gap)
    local maxListH   = math.max(0, listBottom - margin)
    local capacity   = math.max(0, math.floor(maxListH / rowH))
    local visible    = math.min(rowCount, capacity)
    local listH      = visible * rowH

    return {
        uiscale     = scale.get(),
        panelX      = panelX,
        panelW      = panelW,
        rowH        = rowH,
        fontSize    = math.max(6, s.fontSize),
        indent      = math.max(0, s.indent),
        toggle      = { x = toggleX, y = toggleY, w = toggleW, h = toggleH },
        -- Caption metrics travel with the rect they were fitted to, so
        -- the build path and dump() can never disagree about them.
        toggleFontSize = fit.fontSize,
        captionPad     = fit.pad,
        captionWidth   = fit.capW,
        listBottom  = listBottom,
        listTop     = listBottom - listH,
        listH       = listH,
        capacity    = capacity,
        visibleRows = visible,
    }
end

-----------------------------------------------------------
-- Model
-----------------------------------------------------------

-- #958's progress singleton, or nil when it is not reachable at all.
-- package.loaded first (the module registers itself there), require()
-- only as a fallback, so this never forces a load order. Shared by the
-- two things that talk to #958 from here: the read below and the
-- presentation acknowledgement further down.
local function progressModule()
    local progress = package.loaded["scripts.tutorial_progress"]
    if progress ~= nil then return progress end
    local ok, mod = pcall(require, "scripts.tutorial_progress")
    if not ok then return nil end
    return mod
end

-- Exactly the rows the model reports as active, in the model's own
-- order. `active == false` rows are retained completed history and are
-- out of scope for this view; the order is #958's pre-order display
-- walk, already sorted by the loader, so it is never re-sorted here.
local function activeRows()
    local progress = progressModule()
    if progress == nil then return {} end
    if type(progress.getViewModel) ~= "function" then return {} end
    local ok, model = pcall(progress.getViewModel)
    if not ok or type(model) ~= "table" or type(model.rows) ~= "table" then
        return {}
    end
    local out = {}
    for _, row in ipairs(model.rows) do
        if row.active == true then out[#out + 1] = row end
    end
    return out
end

-- The two progress semantics render differently on purpose: a square
-- bracket for a LATCHED full/composite objective, a round one for a
-- LIVE subobjective check that can still come back off.
local function markerFor(row)
    if row.kind == "subobjective" then
        return row.checked and "(x)" or "( )"
    end
    return row.completed and "[x]" or "[ ]"
end

local function contentSignature(rows)
    local font, boxTex = hudAssets()
    local parts = { tostring(tutorialHud.open), tostring(tutorialHud.scrollOffset),
                    tostring(tutorialHud.fbW), tostring(tutorialHud.fbH),
                    tostring(scale.get()),
                    -- ASSET READINESS is part of what was rendered, not
                    -- just of what it looked like: without it, a session
                    -- whose objectives happen not to change after boot
                    -- would keep the asset-less first build forever --
                    -- an invisible toggle over an invisible checklist.
                    -- Folding the handles in here makes the update tick
                    -- rebuild the moment hud.init supplies them.
                    tostring(font), tostring(boxTex) }
    for _, row in ipairs(rows) do
        parts[#parts + 1] = tostring(row.id) .. markerFor(row)
    end
    return table.concat(parts, "|")
end

-- #2056: mint a token for the viewport as it stands RIGHT NOW.
--
-- Called from the only two places that can change what this surface is
-- putting in front of the player: rebuild() (content, geometry, the
-- open flag and the scroll offset all funnel through it) and
-- applyPageVisibility() (the page appearing or disappearing). Between
-- them they cover every path #1941 left racy -- setOpen, setScrollOffset,
-- reflow, resetPresentation, a content-driven rebuild, and the
-- hidden-to-visible edge in update().
--
-- Arming AFTER those mutations is the whole point: UI.armPresentation
-- writes the token into the same UI-manager IORef the elements and the
-- page visibility were just written to, so a renderer snapshot carrying
-- this token necessarily carries this viewport (see
-- UI.Manager.Presentation). Re-minting is also what INVALIDATES stale
-- evidence -- a token published for the previous viewport can never
-- reach the new one, because the counter only goes up.
local function armPresentation()
    if type(UI.armPresentation) ~= "function" then
        -- An engine without the boundary cannot prove presentation, so
        -- it must never claim it: leaving the token nil denies
        -- acknowledgement rather than falling back to #1941's race.
        tutorialHud._presentToken = nil
        return
    end
    local ok, token = pcall(UI.armPresentation)
    tutorialHud._presentToken = (ok and type(token) == "number"
                                 and token > 0) and token or nil
end

-- #1941: report the rows this surface has actually PUT IN FRONT OF THE
-- PLAYER, so #958 can retire the #996 hide suppression that kept an
-- already-latched branch on screen and let the ordinary hide rule
-- resume.
--
-- TWO gates, and #2056 added the second because the first is not
-- sufficient:
--
--   * `_hudVisible`: applyPageVisibility paints this page by it, so a
--     build that happened behind a hidden gameplay HUD has put nothing
--     in front of anyone, however complete the model was.
--   * `UI.isPresented(_presentToken)`: the page being VISIBLE only says
--     the Lua thread has asked for it to be painted. UI.showPage and
--     UI.deleteElement both mutate the shared UI-manager ref
--     synchronously from this thread, and the renderer sees either only
--     when it snapshots that ref -- so before #2056 a checklist built
--     open behind a hidden HUD could be shown, acknowledged and
--     destroyed inside ONE update() call, retiring a terminal branch
--     the player never saw. The token proves a completed renderer
--     snapshot really held THIS viewport. It is a boundary, never a
--     duration: #1941's rejection of a timed or minimum-exposure
--     interval stands, and one frame is enough.
--
-- Everything else the rule needs is already true of `_rows`, which is
-- why nothing else is re-checked here. It holds precisely the slice the
-- viewport laid out -- emptied by destroyElements and repopulated only
-- by the OPEN build path, and then only for
-- `scrollOffset .. +visibleRows`. So a collapsed panel and a row
-- scrolled out of range are both excluded by construction rather than
-- by a second condition that could drift from the build; #2056 keeps
-- that property by RE-ARMING on every such change instead of adding
-- conditions here.
--
-- Fetching the model (activeRows) never reaches here, which is what
-- keeps getViewModel a pure read.
--
-- Driven from the UPDATE TICK, never from rebuild() itself, and BEFORE
-- this tick's own rebuild: what it reports is a viewport a frame has
-- already drawn, so a build is never unmade by the same tick that
-- created it, and rebuild() stays a pure build with no cross-module
-- side effect.
--
-- Idempotent and cheap: acknowledgePresented only writes ids that are
-- still sticky, so every call after the first is a no-op.
local function acknowledgePresentedRows()
    if not tutorialHud._hudVisible then return end
    -- Early-out only: an empty list acknowledges nothing either way.
    if #tutorialHud._rows == 0 then return end
    if not tutorialHud.isPresented() then return end
    local progress = progressModule()
    if progress == nil
            or type(progress.acknowledgePresented) ~= "function" then
        return
    end
    local ids = {}
    for _, row in ipairs(tutorialHud._rows) do ids[#ids + 1] = row.id end
    pcall(progress.acknowledgePresented, ids)
end

-----------------------------------------------------------
-- Build / teardown
-----------------------------------------------------------

local function destroyElements()
    for _, h in ipairs(tutorialHud._els) do
        UI.deleteElement(h)
    end
    tutorialHud._els         = {}
    tutorialHud._rows        = {}
    tutorialHud._toggle      = nil
    tutorialHud._toggleLabel = nil
end

local function track(handle)
    if handle then tutorialHud._els[#tutorialHud._els + 1] = handle end
end

local function applyPageVisibility()
    if not tutorialHud.page then return end
    if tutorialHud._hudVisible then
        UI.showPage(tutorialHud.page)
    else
        UI.hidePage(tutorialHud.page)
    end
    -- #2056: the page appearing or disappearing changes what is in
    -- front of the player as surely as a rebuild does, and the arm has
    -- to follow the show/hide so the token stands for the page's NEW
    -- state.
    armPresentation()
end

-- Destroy and recreate every element from the live model + geometry.
-- The single build path: opening/closing, scrolling, a content change,
-- a real resize and a scale-only rescale all land here, so there is
-- exactly one place where geometry and content can disagree, and none.
function tutorialHud.rebuild()
    if not tutorialHud.page then return false end
    local font, boxTex = hudAssets()
    tutorialHud._assetsReady = (font ~= nil) and (boxTex ~= nil)
    destroyElements()
    tutorialHud.rebuildCount = tutorialHud.rebuildCount + 1

    local rows = activeRows()
    local lay  = computeLayout(tutorialHud.fbW, tutorialHud.fbH, #rows)

    -- Clamp the preserved offset against the LIVE range on every
    -- rebuild, not just on a geometry change: subobjective checks are
    -- reversible, so rows appear and disappear during play and a
    -- stale offset would otherwise strand the remaining rows off the
    -- top of the viewport.
    local maxOffset = math.max(0, #rows - lay.capacity)
    if tutorialHud.scrollOffset > maxOffset then
        tutorialHud.scrollOffset = maxOffset
    end
    if tutorialHud.scrollOffset < 0 then tutorialHud.scrollOffset = 0 end

    tutorialHud._layout = lay
    tutorialHud._rowCount = #rows
    tutorialHud._maxOffset = maxOffset

    -- Toggle: the one real control. A box when the HUD's shared box
    -- textures are available, a bare element otherwise (headless /
    -- pre-texture boot) -- either way clickable, pointer-blocking by
    -- virtue of its callback, and never scroll-capturing, so the wheel
    -- over it still reaches gameplay.
    local t = lay.toggle
    local toggleH
    if boxTex then
        toggleH = UI.newBox("tutorial_hud_toggle", t.w, t.h,
            boxTex, math.max(1, math.floor(lay.fontSize / 2)),
            1.0, 1.0, 1.0, 1.0, 0, tutorialHud.page)
    else
        toggleH = UI.newElement("tutorial_hud_toggle", t.w, t.h, tutorialHud.page)
    end
    if toggleH then
        UI.addToPage(tutorialHud.page, toggleH, t.x, t.y)
        UI.setZIndex(toggleH, 10)
        UI.setClickable(toggleH, true)
        UI.setOnClick(toggleH, TOGGLE_CALLBACK)
        UI.setTooltip(toggleH, tutorialHud.open
            and "Hide the objective checklist"
            or  "Show the objective checklist")
        track(toggleH)
        tutorialHud._toggle = toggleH
        if font then
            -- Font size and pad come from the FIT, not from the list's
            -- own scaled sizes: the box and its caption shrink together
            -- or not at all.
            local caption = captionText(tutorialHud.open)
            local capH = UI.newText("tutorial_hud_toggle_label", caption, font,
                lay.toggleFontSize, 1.0, 1.0, 1.0, 1.0, tutorialHud.page)
            if capH then
                UI.addToPage(tutorialHud.page, capH,
                    t.x + lay.captionPad,
                    t.y + math.floor((t.h + lay.toggleFontSize) / 2))
                UI.setZIndex(capH, 11)
                track(capH)
                tutorialHud._toggleLabel = capH
            end
        end
    end

    if not tutorialHud.open then
        tutorialHud._sig = contentSignature(rows)
        return true
    end

    -- Rows, top-down inside a viewport whose bottom edge is the
    -- toggle's top edge. No background element of any kind: the list
    -- is transparent over terrain, and the row hit boxes exist only to
    -- carry the YAML tooltip and to capture the wheel over exactly the
    -- visible list.
    for i = 1, lay.visibleRows do
        local row = rows[tutorialHud.scrollOffset + i]
        if row == nil then break end
        local rowY = lay.listTop + (i - 1) * lay.rowH
        local hit = UI.newElement("tutorial_hud_row_" .. tostring(row.id),
            lay.panelW, lay.rowH, tutorialHud.page)
        if hit then
            UI.addToPage(tutorialHud.page, hit, lay.panelX, rowY)
            UI.setZIndex(hit, 1)
            -- #743's independent policies: capture the wheel, block
            -- nothing, register no callback.
            UI.setScrollCapture(hit, true)
            if row.tooltip and row.tooltip ~= "" then
                UI.setTooltip(hit, row.tooltip)
            end
            track(hit)
        end
        local textH = nil
        if font then
            local indent = math.min(lay.indent * math.max(0, row.depth or 0),
                                     math.max(0, lay.panelW - lay.indent))
            -- #1581: the panel is RIGHT-ANCHORED and nothing clips a row,
            -- so a string wider than the budget its indent leaves paints
            -- toward and past the right framebuffer edge -- which every
            -- shipped first_session row below the root actually did. The
            -- caption's fitToggle solves this for the toggle alone; rows
            -- are a fixed-height list, so they truncate instead of
            -- shrinking a font the whole list shares.
            --
            -- The right-hand slack is captionPadFor's, for captionPadFor's
            -- reason: glyph ink is placed at the pen position plus the
            -- glyph's bearing and is bounded by its bitmap, not by the
            -- advance sum engine.getTextWidth returns, so a string that
            -- fits by advance can still put ink a few pixels further right.
            local budget = lay.panelW - indent - captionPadFor(lay.indent)
            local rowText = fitRowText(
                markerFor(row) .. " " .. tostring(row.label or row.id),
                font, lay.fontSize, budget)
            -- An empty fit means the budget could not hold even the
            -- ellipsis (an out-of-envelope framebuffer/scale combination).
            -- Drawing nothing is the shared helper's own answer there;
            -- drawing the full string would put it across the framebuffer.
            textH = rowText ~= "" and UI.newText(
                "tutorial_hud_row_label_" .. tostring(row.id), rowText,
                font, lay.fontSize, 1.0, 1.0, 1.0, 1.0, tutorialHud.page) or nil
            if textH then
                UI.addToPage(tutorialHud.page, textH,
                    lay.panelX + indent,
                    rowY + math.floor((lay.rowH + lay.fontSize) / 2))
                UI.setZIndex(textH, 2)
                track(textH)
            end
        end
        tutorialHud._rows[#tutorialHud._rows + 1] = {
            id        = row.id,
            kind      = row.kind,
            label     = row.label,
            tooltip   = row.tooltip,
            depth     = row.depth,
            relation  = row.relation,
            completed = row.completed,
            checked   = row.checked,
            marker    = markerFor(row),
            handle    = hit,
            textHandle = textH,
            x = lay.panelX, y = rowY, w = lay.panelW, h = lay.rowH,
        }
    end

    tutorialHud._sig = contentSignature(rows)
    -- #2056: last, after every element of this viewport exists. An arm
    -- placed earlier would stand for a half-built list.
    armPresentation()
    return true
end

-----------------------------------------------------------
-- Presentation controls
-----------------------------------------------------------

function tutorialHud.isOpen()
    return tutorialHud.open == true
end

-- #2056: has the CURRENT viewport been in front of a completed renderer
-- snapshot? False whenever nothing is armed, which is the state a
-- GPU-less --headless engine stays in permanently -- it draws no frame,
-- so it can never honestly answer yes, and tools/tutorial_probe.py
-- performs the model transition explicitly instead of waiting for one.
function tutorialHud.isPresented()
    local token = tutorialHud._presentToken
    if type(token) ~= "number" then return false end
    if type(UI.isPresented) ~= "function" then return false end
    local ok, presented = pcall(UI.isPresented, token)
    return ok and presented == true
end

function tutorialHud.setOpen(open)
    local want = open and true or false
    if tutorialHud.open == want then return false end
    tutorialHud.open = want
    tutorialHud.rebuild()
    return true
end

function tutorialHud.toggle()
    return tutorialHud.setOpen(not tutorialHud.open)
end

-- Collapse the panel and forget the scroll offset. PRESENTATION ONLY:
-- nothing about tutorial PROGRESS is touched (that is #958's, and its
-- own reset/apply paths own it).
function tutorialHud.resetPresentation()
    tutorialHud.open         = false
    tutorialHud.scrollOffset = 0
    tutorialHud.rebuild()
end

function tutorialHud.setScrollOffset(offset)
    local n = math.floor(tonumber(offset) or 0)
    local maxOffset = tutorialHud._maxOffset or 0
    if n < 0 then n = 0 end
    if n > maxOffset then n = maxOffset end
    if n == tutorialHud.scrollOffset then return false end
    tutorialHud.scrollOffset = n
    tutorialHud.rebuild()
    return true
end

-----------------------------------------------------------
-- Engine callbacks
-----------------------------------------------------------

-- The toggle's own click callback name, broadcast straight to this
-- module by Engine.Scripting.Lua.Thread.Dispatch's LuaUIClickEvent --
-- no ui_manager routing table entry needed, the same way
-- preview_manager.lua handles its own list clicks.
function tutorialHud.onTutorialHudToggle(elemHandle)
    if tutorialHud._toggle == nil or elemHandle ~= tutorialHud._toggle then
        return false
    end
    tutorialHud.toggle()
    return true
end

-- Wheel over a row hit box. `onUIScroll` is broadcast to every
-- loadScript'd module, so this needs no entry in
-- scripts/ui_manager_scroll.lua's require()-only chain; the handle
-- check is what keeps it scoped to this surface.
function tutorialHud.onUIScroll(elemHandle, _dx, dy)
    if not tutorialHud.open then return false end
    local owned = false
    for _, row in ipairs(tutorialHud._rows) do
        if row.handle == elemHandle then owned = true break end
    end
    if not owned then return false end
    local step = 0
    if dy > 0 then step = -1 elseif dy < 0 then step = 1 end
    if step == 0 then return false end
    return tutorialHud.setScrollOffset(tutorialHud.scrollOffset + step)
end

-- The engine's own broadcast to every loadScript'd module. It records
-- the new dimensions and DELIBERATELY DOES NOT REBUILD.
--
-- broadcastToModules fans out in script-map order, so this module can
-- (and does) receive the resize BEFORE ui_manager forwards it to
-- scripts/hud.lua -- and this surface anchors against
-- hud.getToolbarRects(), which until that forward still describes the
-- toolbar at the OLD framebuffer size. Rebuilding here would lay the
-- toggle out against stale toolbar bounds and could leave it sitting
-- on top of the newly positioned map toggle. The rebuild happens in
-- reflow() below instead, which ui_manager runs after hud has been
-- rebuilt at the new size -- exactly the split popup.lua and
-- unit_info_v2.lua already use (scripts/ui_manager_boot.lua), and the
-- reason this module still must NOT join that file's manual
-- onFramebufferResize forward set, which would double-fire it.
function tutorialHud.onFramebufferResize(width, height)
    if (width or 0) <= 0 or (height or 0) <= 0 then return end
    tutorialHud.fbW = width
    tutorialHud.fbH = height
end

-- The geometry-dependent half of a resize, run once hud's toolbar
-- clusters exist at the new size. Also the scale-only entry point
-- (uiManager.notifyGameplayRescale), which has no engine broadcast to
-- pair with and so passes the dimensions itself.
function tutorialHud.reflow(width, height)
    if (width or 0) > 0 and (height or 0) > 0 then
        tutorialHud.fbW = width
        tutorialHud.fbH = height
    end
    tutorialHud.rebuild()
end

-- A save load replaces the whole session but not this Lua singleton,
-- so the panel would otherwise come back open, scrolled to wherever
-- the pre-load session left it. Presentation only; #958's save
-- component restores the progress itself.
function tutorialHud.onSaveLoaded(_survUnitIds, _survBuildingIds)
    tutorialHud.resetPresentation()
end

-- hud.hide()'s teardown sweep reaches this through
-- scripts/ui/view_teardown.lua. Hiding the page is all it does -- the
-- open flag and the scroll offset must survive, so this is a
-- suppress, never a clear.
function tutorialHud.onHudHidden()
    tutorialHud._hudVisible = false
    applyPageVisibility()
end

function tutorialHud.update(_dt)
    if not tutorialHud.page then return end
    local hudMod     = hudModule()
    local hudVisible = (hudMod ~= nil) and (hudMod.visible == true)
    if hudVisible ~= tutorialHud._hudVisible then
        tutorialHud._hudVisible = hudVisible
        applyPageVisibility()
    end
    -- #1941: report what this surface has been showing since the last
    -- tick, BEFORE any rebuild below can replace it. Every tick, not
    -- only after a rebuild -- a row can reach the screen with no
    -- content change at all, because hud.show() flips `_hudVisible`
    -- just above and paints a page that was built while hidden.
    --
    -- #2056: which is exactly why the rising edge above must not
    -- acknowledge in this same call. applyPageVisibility() has just
    -- re-armed, so the token stands for a page shown MICROSECONDS ago
    -- that no renderer snapshot can yet have held; the gate inside
    -- refuses, and a later tick -- once a frame has really drawn it --
    -- is what spends the suppression.
    acknowledgePresentedRows()
    if not hudVisible then return end
    -- Content churn only: opening, scrolling and resizing rebuild
    -- directly. Objectives complete and subobjectives check/uncheck
    -- from #959's evaluation tick, which this module never drives.
    -- Retiring a sticky row above is itself such a change, so the
    -- rebuild that takes it off the list happens here.
    if contentSignature(activeRows()) ~= tutorialHud._sig then
        tutorialHud.rebuild()
    end
end

function tutorialHud.init(_scriptId)
    engine.logDebug("Tutorial HUD initializing...")
    if tutorialHud.page == nil then
        -- "overlay", the same pass-through band scripts/hud.lua's own
        -- pages use. NOT "modal": a modal page defaults to
        -- input-exclusive and its empty space would consume every
        -- gameplay click around the checklist.
        tutorialHud.page = UI.newPage("tutorial_hud_overlay", "overlay")
    end
    local w, h = engine.getFramebufferSize()
    tutorialHud.fbW = w or tutorialHud.fbW
    tutorialHud.fbH = h or tutorialHud.fbH
    -- Requirement 2: collapsed at fresh boot, whatever a hot-reloaded
    -- singleton was left holding.
    tutorialHud.open         = false
    tutorialHud.scrollOffset = 0
    tutorialHud._hudVisible  = false
    tutorialHud.rebuild()
    applyPageVisibility()
end

function tutorialHud.shutdown()
    destroyElements()
    if tutorialHud.page then
        UI.hidePage(tutorialHud.page)
        UI.deletePage(tutorialHud.page)
        tutorialHud.page = nil
    end
    tutorialHud._hudVisible = false
end

-----------------------------------------------------------
-- Introspection (requirement 7)
-----------------------------------------------------------

-- Everything a headless spec or an offscreen probe needs to assert
-- against, with `completed` and `checked` kept apart exactly as the
-- view model reports them (a full/composite row carries only
-- `completed`, a subobjective row only `checked`).
function tutorialHud.dump()
    local lay = tutorialHud._layout
        or computeLayout(tutorialHud.fbW, tutorialHud.fbH, 0)
    local rows, ids = {}, {}
    for i, row in ipairs(tutorialHud._rows) do
        rows[i] = {
            id        = row.id,
            kind      = row.kind,
            label     = row.label,
            tooltip   = row.tooltip,
            depth     = row.depth,
            relation  = row.relation,
            completed = row.completed,
            checked   = row.checked,
            marker    = row.marker,
            handle    = row.handle,
            textHandle = row.textHandle,
            x = row.x, y = row.y, w = row.w, h = row.h,
            pointerBlocking = row.handle and UI.isPointerBlocking(row.handle) or false,
            scrollCapture   = row.handle and UI.isScrollCapturing(row.handle) or false,
        }
        ids[i] = row.id
    end
    local activeIds = {}
    for i, row in ipairs(activeRows()) do activeIds[i] = row.id end
    return {
        open         = tutorialHud.open,
        hudVisible   = tutorialHud._hudVisible,
        visible      = tutorialHud.open and tutorialHud._hudVisible,
        page         = tutorialHud.page,
        rows         = rows,
        rowIds       = ids,
        activeIds    = activeIds,
        activeCount  = #activeIds,
        scrollOffset = tutorialHud.scrollOffset,
        scrollRange  = tutorialHud._maxOffset or 0,
        capacity     = lay.capacity,
        rebuildCount = tutorialHud.rebuildCount,
        -- #2056: the live presentation boundary. `presentToken` is the
        -- token standing for exactly the VIEWPORT above; `presented` is
        -- whether a completed renderer snapshot has held that viewport
        -- yet. Note it says nothing about ROWS: a collapsed panel's
        -- empty viewport is presented as honestly as a full one, and
        -- acknowledgement is gated on `presented` AND a non-empty
        -- `rows` -- two independent conditions.
        presentToken = tutorialHud._presentToken or 0,
        presented    = tutorialHud.isPresented(),
        -- Whether hud's shared font AND box textures existed at build
        -- time. False means this build predates hud.init and is
        -- deliberately unlabelled/textureless; the update tick rebuilds
        -- as soon as they arrive (see contentSignature).
        assetsReady  = tutorialHud._assetsReady == true,
        toggle       = {
            x = lay.toggle.x, y = lay.toggle.y,
            w = lay.toggle.w, h = lay.toggle.h,
            handle = tutorialHud._toggle,
            label  = tutorialHud._toggleLabel,
            -- #1419: the caption the box was FITTED to. `captionWidth`
            -- is the measured width of the WIDER of the two variants
            -- (0 where nothing can measure), so `captionX + captionWidth`
            -- is the right edge the caption is reserved out to in EITHER
            -- state -- half-open, like the in-frame checks.
            caption      = captionText(tutorialHud.open),
            captionX     = lay.toggle.x + lay.captionPad,
            captionPad   = lay.captionPad,
            captionWidth = lay.captionWidth,
            fontSize     = lay.toggleFontSize,
        },
        panelX  = lay.panelX,
        panelW  = lay.panelW,
        listTop = lay.listTop,
        listBottom = lay.listBottom,
        rowH    = lay.rowH,
        fbW     = tutorialHud.fbW,
        fbH     = tutorialHud.fbH,
        uiscale = lay.uiscale,
    }
end

return tutorialHud
