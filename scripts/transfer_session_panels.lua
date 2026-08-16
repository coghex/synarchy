-- Escort session panels (#1250, epic #1013 slice UIT-3B): the two
-- flanking panels a Mode A transfer session opens once its source unit
-- has walked to the destination and stopped there.
--
-- This module owns exactly ONE LEVEL KIND of the container-window stack
-- (scripts/cargo_inventory_panel.lua), the same way
-- scripts/item_contents_panel.lua owns its two (D-13): no page, no
-- panel, no singleton, no Escape handling, no per-tick refresh.
-- Opening, closing, modality, resize restore and the nesting path all
-- belong to the manager. The SESSION's own lifecycle — who is held, what
-- phase it is in, when it ends — belongs to
-- scripts/transfer_session.lua; this file is what that session looks
-- like on screen and what its rows do.
--
-- D-9's stated exception, and the reason a pane is a concept at all:
-- Mode A is ONE nesting level owning TWO panels. Both are painted, both
-- are interactive, neither is modal over the other, Escape closes the
-- pair in one press, and opening any other container replaces them
-- together. That falls out of being one level rather than being
-- arranged by bookkeeping here.
--
-- Two panes, one presentation:
--
--   source       the held unit's own inventory. Its rows offer
--                "Store 1" / "Store all" into the destination.
--   destination  the endpoint the unit walked to — a storage building
--                or another unit. Its rows offer "Retrieve 1" /
--                "Retrieve all" into the held unit.
--
-- Both render through cargo_inventory_panel's OWN endpoint machinery
-- (endpointView / endpointListParams / endpointStillThere /
-- endpointChildOf), so an escort pane shows a unit or a container
-- exactly the way a lone container window does — same header, same
-- capacity and stored-weight line, same tabs, same rows, same "as of…"
-- age on a remembered one. Nothing about an endpoint is re-derived here.
--
-- The entries themselves are scripts/transfer_gestures.lua's shared
-- 1-and-all builder, given this module's own `submit` — so Mode A and
-- Mode B cannot disagree about which exact instances a merged row
-- stands for, when a gesture is omitted, or how a single-instance row
-- differs from a stack. What differs is only that Mode A commits
-- immediately, because the unit is already standing there.
--
-- Public API:
--   levelKinds()          -- the manager's lookup
--   open(session)         -- push the escort level for `session`
--   closeFor(sessionId)   -- close it, if it is still the level open

local M = package.loaded["scripts.transfer_session_panels"] or {}
package.loaded["scripts.transfer_session_panels"] = M

local scale           = require("scripts.ui.scale")
local reservedRegions = require("scripts.ui.reserved_regions")

-- The manager is required lazily: it requires this module too (for
-- levelKinds), and a top-level require in both directions is a load
-- cycle.
local function manager()
    return require("scripts.cargo_inventory_panel")
end

local PANE_SOURCE      = "source"
local PANE_DESTINATION = "destination"

-- Base gap between the two panels, scaled at draw time. Small: the
-- panels are already framebuffer-clamped and reserved-region-nudged, so
-- this only has to read as "two panels", not reserve real estate.
local PANE_GAP = 24

-- Which endpoint a pane shows. The whole kind is written against this
-- one function, which is why "the source pane" and "the unit side"
-- never have to be the same sentence twice.
local function endpointFor(src, paneKey)
    if paneKey == PANE_DESTINATION then return src.destination end
    return src.source
end

-----------------------------------------------------------
-- Row actions
-----------------------------------------------------------

-- The player-visible name of a unit, following the same #264 precedence
-- every other surface that names one uses. Local rather than borrowed
-- from unit_ai_pickup: this is a UI module and must not pull the AI in.
local function unitLabel(uid)
    local info = uid and unit.getInfo(uid)
    if not info then return "Unit" end
    if info.name and info.name ~= "" then return info.name end
    if info.displayName and info.displayName ~= "" then return info.displayName end
    return info.defName or "Unit"
end

-- ONE gesture, committed on the spot.
--
-- The check comes first and the commit is AUTHORITATIVE. They are given
-- the IDENTICAL request, which is the point: the check reports a
-- whole-request refusal (the destination drifted out of reach, the
-- endpoint stopped being eligible) without mutating anything, and the
-- commit then re-validates every item against live state at the instant
-- it moves. Nothing here re-derives proximity, capacity or eligibility
-- — src/Unit/Transfer.hs owns all three, and a second rule in Lua would
-- be free to disagree with the one that actually decides.
--
-- D-1: a partial batch commits what fits and reports the remainder. No
-- single item ever half-moves, because each is committed through the
-- contract's own atomic per-item path.
local function commitNow(heldUid, source, destination, defName, ids, label,
                         verb)
    local items = {}
    for i, iid in ipairs(ids) do
        items[i] = { instanceId = iid, defName = defName }
    end
    local request = { source = source, destination = destination,
                      items = items }
    local itemName = (label and label ~= "" and label) or defName

    local function warn(text)
        engine.emitEventForUnit("unit_warning", text, heldUid)
    end

    local checked = unit.checkTransfer(request)
    if not checked then
        warn(string.format("%s can't %s %s -- malformed request",
                           unitLabel(heldUid), verb, itemName))
        return false
    end
    -- A whole-request rejection names ONE reason and mutates nothing,
    -- so it is reported straight from the check. Requirement 6's
    -- drifted-target case lands here, with the contract's own proximity
    -- reason, and the session deliberately stays open: the player can
    -- walk the target back or close the window themselves.
    if checked.accepted == false then
        warn(string.format("%s can't %s %s -- %s", unitLabel(heldUid), verb,
                           itemName, tostring(checked.reason)))
        return false
    end

    local result = unit.commitTransfer(request)
    if not result then
        warn(string.format("%s can't %s %s -- malformed request",
                           unitLabel(heldUid), verb, itemName))
        return false
    end
    if result.accepted == false then
        warn(string.format("%s can't %s %s -- %s", unitLabel(heldUid), verb,
                           itemName, tostring(result.reason)))
        return false
    end

    local moved, refused, cause = 0, 0, nil
    for _, o in ipairs(result.outcomes or {}) do
        if o.state == "completed" then
            moved = moved + 1
        else
            refused = refused + 1
            cause = cause or o.cause or o.reason
        end
    end
    if moved > 0 then
        engine.emitEventForUnit("unit_event", string.format(
            "%s %sd %s%s", unitLabel(heldUid), string.lower(verb), itemName,
            (moved > 1) and string.format(" x%d", moved) or ""), heldUid)
    end
    -- D-1 / requirement 4: whatever did not fit is reported by COUNT and
    -- by the contract's own returned reason, so a partial batch is
    -- visibly partial rather than silently short. A zero completion
    -- reports the same way — it is the same fact with a different count.
    if refused > 0 then
        warn(string.format("%s couldn't %s %d x %s -- %s", unitLabel(heldUid),
                           verb, refused, itemName,
                           tostring(cause or "refused")))
    end
    return moved > 0
end

-----------------------------------------------------------
-- Placement
--
-- The pair FLANKS the framebuffer centre — the source unit on the left,
-- the destination endpoint on the right, reading left-to-right as
-- "from -> to". The centre is where the pair itself is: the session's
-- one camera snap (requirement 1) centred them there on the transition
-- to this state, and it is recomputed from the endpoints' LIVE
-- positions, so the panels frame what they describe without a
-- world->screen projection this engine does not expose to Lua.
--
-- UI.placePopup owns the direction and the framebuffer clamp for each
-- panel (#747, the one placement algorithm for floating content), and
-- reserved_regions owns the arbitration afterwards: a panel-versus-panel
-- avoidance is exactly that machinery with the sibling panel as the
-- reserved rect. Both panes additionally keep clear of the
-- always-reachable toolbar clusters (#750).
-----------------------------------------------------------

-- The toolbar clusters, or an empty list when there is no HUD to ask
-- (every headless UI fixture, and the moment before hud.createUI has
-- built its toggles). pcall-isolated like every other cross-module
-- introspection read on this path.
local function toolbarRects()
    local ok, rects = pcall(function()
        return require("scripts.hud").getToolbarRects()
    end)
    if not ok or type(rects) ~= "table" then return {} end
    return rects
end

local function fitClear(rect, reserved, fbW, fbH)
    if #reserved == 0 then return rect end
    return reservedRegions.avoidReserved(rect, reserved, fbW, fbH)
end

local function placePanes(_level, measures, hud)
    local fbW = (hud and hud.fbW) or 0
    local fbH = (hud and hud.fbH) or 0
    if fbW <= 0 or fbH <= 0 then
        fbW, fbH = engine.getFramebufferSize()
    end
    fbW = fbW or 0
    fbH = fbH or 0

    local cx  = math.floor(fbW * 0.5)
    local gap = math.floor(PANE_GAP * scale.get() * 0.5)
    local reserved = toolbarRects()

    local out = {}
    local lm, rm = measures[1], measures[2]

    local lx, ly = UI.placePopup(cx - gap,
                                 math.floor((fbH - lm.h) * 0.5),
                                 0, 0, lm.w, lm.h, "left")
    local left = fitClear({ x = math.floor(lx), y = math.floor(ly),
                            w = lm.w, h = lm.h }, reserved, fbW, fbH)
    out[1] = { x = left.x, y = left.y }

    if not rm then return out end

    local rx, ry = UI.placePopup(cx + gap,
                                 math.floor((fbH - rm.h) * 0.5),
                                 0, 0, rm.w, rm.h, "right")
    -- The left pane is now a reservation of its own, so the right one
    -- is nudged clear of BOTH it and the toolbar in a single pass —
    -- avoidReserved evaluates every candidate against the whole set,
    -- which is what stops a push that clears one from re-entering the
    -- other.
    local withSibling = { { x = left.x, y = left.y, w = lm.w, h = lm.h } }
    for _, r in ipairs(reserved) do withSibling[#withSibling + 1] = r end
    local right = fitClear({ x = math.floor(rx), y = math.floor(ry),
                             w = rm.w, h = rm.h }, withSibling, fbW, fbH)
    out[2] = { x = right.x, y = right.y }
    return out
end

-----------------------------------------------------------
-- The level kind
-----------------------------------------------------------

-- The base level of the stack when it is THIS session's escort level,
-- or nil. Deliberately narrow: a session whose window was already
-- replaced by another container must not close, or refresh, whatever is
-- open now on its way out.
local function escortLevel(sessionId)
    local base = manager().getLevel(1)
    if not base or base.src.kind ~= "escort" then return nil end
    if sessionId ~= nil and base.src.sessionId ~= sessionId then return nil end
    return base
end

local KINDS = nil

-- Built on first use rather than at load time: the tab strip is the
-- container window's own (an escort pane must not style its tabs
-- differently from a lone window's), and reading it at module scope
-- would require the manager while the manager is still requiring this.
local function buildKinds()
    local m = manager()
    return {
      escort = {
          panelWidthBase = 440,
          maxRows        = 10,
          tabs           = m.endpointTabSpec(),
          paneKeys       = { PANE_SOURCE, PANE_DESTINATION },
          placePanes     = placePanes,

          view = function(src, paneKey)
              local ep = endpointFor(src, paneKey)
              if not ep then return nil end
              return manager().endpointView(ep.kind, ep.id)
          end,

          -- Both endpoints, because either one vanishing leaves nothing
          -- coherent to show. The manager closes the level (and with it
          -- the session) on a nil from either.
          stillThere = function(src)
              local m = manager()
              for _, key in ipairs({ PANE_SOURCE, PANE_DESTINATION }) do
                  local ep = endpointFor(src, key)
                  if not ep or not m.endpointStillThere(ep.kind, ep.id) then
                      return false
                  end
              end
              return true
          end,

          listParams = function(src, view, paneKey)
              local ep = endpointFor(src, paneKey)
              local p = manager().endpointListParams(ep.kind, view)
              -- The pane key joins the rebuild comparison because the two
              -- panes are two INDEPENDENT list instances describing two
              -- endpoints that can look identical (two acolytes of the
              -- same species carrying the same load). Without it, a
              -- presentation change on one could read as no change at all.
              p.presentationKey = paneKey .. "|" .. tostring(p.presentationKey)
              return p
          end,

          -- Mode A's row menus. Direction is derived from WHICH PANE the
          -- player right-clicked, so one builder produces both and a
          -- swapped pair is unrepresentable.
          transferMenu = function(src, row, paneKey)
              local gestures = require("scripts.transfer_gestures")
              local from = endpointFor(src, paneKey)
              local to   = endpointFor(src, paneKey == PANE_DESTINATION
                                              and PANE_SOURCE
                                              or PANE_DESTINATION)
              if not (from and to) then return {} end
              -- The held unit is the session's source in BOTH directions:
              -- it is the acolyte the player is standing there, and every
              -- warning and event this gesture emits belongs in its log.
              local heldUid = src.source and src.source.id
              if type(heldUid) ~= "number" then return {} end
              local verb = (paneKey == PANE_DESTINATION) and "Retrieve"
                                                         or "Store"
              -- Captured from the ROW, which is the only place a
              -- player-legible item name exists on this path.
              local label = row and (row.displayName or row.defName)
              return gestures.entries({
                  verb        = verb,
                  row         = row,
                  executor    = heldUid,
                  source      = from,
                  destination = to,
                  submit      = function(_executor, source, destination,
                                         defName, ids)
                      commitNow(heldUid, source, destination, defName, ids,
                                label, verb)
                      -- Both panes refresh in the same gesture: an item
                      -- left one endpoint and reached the other, and the
                      -- weights on BOTH headers moved. Addressed by
                      -- SESSION rather than by "the deepest level", so a
                      -- commit can only ever redraw its own window. The
                      -- session stays open whatever the outcome was.
                      manager().refreshLevel(escortLevel(src.sessionId))
                  end,
              })
          end,

          childOf = function(src, row, paneKey)
              local ep = endpointFor(src, paneKey)
              if not ep then return nil end
              return manager().endpointChildOf(ep.kind, ep.id, row)
          end,

          -- Coupled close (requirement 7). Closing the level IS ending the
          -- session — through Escape, through another container replacing
          -- it, through an endpoint vanishing, through Exit to Menu. A
          -- "layout" close never reaches here (the manager filters it), so
          -- a resize rebuilds both panes with the session and its hold
          -- intact.
          onClose = function(src, reason)
              require("scripts.transfer_session").onLevelClosed(src.sessionId,
                                                                reason)
          end,
      },
    }
end

function M.levelKinds()
    if not KINDS then KINDS = buildKinds() end
    return KINDS
end

-----------------------------------------------------------
-- Entry points, called by scripts/transfer_session.lua
-----------------------------------------------------------

-- The level identity for one session. Endpoint identities are copied
-- rather than referenced so a level surviving a snapshot/restore round
-- trip describes the same two endpoints it was opened on.
local function levelSrc(session)
    return {
        kind        = "escort",
        sessionId   = session.id,
        source      = { kind = session.source.kind, id = session.source.id },
        destination = { kind = session.destination.kind,
                        id   = session.destination.id },
    }
end

-- Push the escort level at the BASE of the stack, replacing whatever
-- container window was open. An external request always targets the
-- base (#1238), and a session is as external as a request gets.
--
-- The anchor arguments are unused by this kind (placePanes ignores
-- them) but are still supplied, because the manager records them for
-- the resize snapshot and every other level kind reads them.
function M.open(session)
    if type(session) ~= "table" then return false end
    local fbW, fbH = engine.getFramebufferSize()
    return manager().openLevel(levelSrc(session),
                               math.floor((fbW or 0) * 0.5),
                               math.floor((fbH or 0) * 0.5), 0, "replaced")
end

-- Close the escort level, if the base level is still THIS session's.
-- Idempotent, and deliberately narrow (see `escortLevel`).
--
-- The `onClose` hook this fires re-enters transfer_session, which is
-- harmless BY ORDERING rather than by a flag: that module clears its
-- active session BEFORE calling here, so the re-entry finds no session
-- with this id and stops. Both directions of the coupling therefore run
-- exactly once, whichever end started it.
function M.closeFor(sessionId)
    if not escortLevel(sessionId) then return false end
    manager().closeIfOpen("session_ended")
    return true
end

return M
