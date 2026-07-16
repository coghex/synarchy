{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Physical MOUSE-BUTTON dispatch (#787): click/right-click/
--   middle-click routing (tooltip lock, UI hit-testing via
--   'UI.InputOwnership.routePointer', modal-boundary blocking),
--   press/release pairing, and F4 (#730) drag-vs-click classification
--   deferred to release. Split out of 'Engine.Input.Thread' to keep
--   that facade a thin thread-loop entrypoint; reached only through
--   'Engine.Input.Thread.Dispatch.dispatchInput'.
module Engine.Input.Thread.Mouse
  ( dispatchMouseEvent
  , uiDragThresholdPx
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef (readIORef, writeIORef, newIORef, atomicModifyIORef')
import Engine.Core.Log (logDebug, LogCategory(..))
import Engine.Core.State
import Engine.Input.Types
import Engine.Scripting.Lua.Types
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import Engine.Graphics.Viewport (viewportDegenerate)
import Engine.Input.Inject (windowToFb)
import qualified Engine.Core.Queue as Q
import UI.Tooltip (isTooltipLocked, isTooltipVisible, isPointInLockedTooltip
                  , clearTooltipLock, toggleTooltipLock)
import UI.InputOwnership (PointerKind(..), InputRoute(..), routePointer
                          , isPointerSurfaceBlocked)
import UI.Manager.Query (isElementDragActivation)
import UI.ControlActivation (PendingActivation(..), ActivationOutcome(..)
                             , activationOutcomeName, beginActivation)
import Engine.Input.Thread.Mouse.Activation (resolvePendingActivation)
import UI.FocusNavigation (isEligibleControl)
import UI.Manager (setControlFocus, clearControlFocus, getControlFocus)

-- | F4 (#730 review round 2): window-pixel movement between a
--   ClickUI-routed press and its release beyond which the gesture
--   reads as a UI-widget drag rather than a plain click — matches
--   scripts/unit_drag_select.lua's own DRAG_THRESHOLD for the
--   game-world case (both operate in the same window-coordinate
--   space).
uiDragThresholdPx ∷ Double
uiDragThresholdPx = 4

-- | The real per-event dispatch for 'InputMouseEvent' — split out of
--   'dispatchInput' the same way 'Engine.Input.Thread.Keyboard.dispatchKeyEvent'
--   is.
dispatchMouseEvent ∷ EngineEnv → InputState → GLFW.MouseButton → (Double, Double) → GLFW.MouseButtonState → IO InputState
dispatchMouseEvent env inpSt btn pos state = do
    let lq = luaQueue env
        (x, y) = pos
    logger ← readIORef (loggerRef env)

    (winW, winH) ← readIORef (windowSizeRef env)
    (fbW, fbH) ← readIORef (framebufferSizeRef env)
    -- F4 (#774): the oracle's recorded `where` must share F1/F2/F3's
    -- framebuffer-pixel space, not the window coords the routing/
    -- hit-test math below (mouseX/mouseY) and every Lua-dispatched
    -- event still use — 'windowToFb' is the inverse of
    -- 'Engine.Input.Inject.fbToWindow'. Falls back to the raw window
    -- coordinate on a degenerate viewport, same as every other
    -- degenerate-guarded conversion in this module.
    let toFb ∷ (Double, Double) → (Double, Double)
        toFb wp = case windowToFb (winW, winH) (fbW, fbH) wp of
            Just fb → fb
            Nothing → wp

    -- F4 (#646, #730 review round 2) Layer A: routes that consume a
    -- press WITHOUT ever queuing a Lua event (ClickSwallowed, and a
    -- ClickUI whose widget has no handler for this button) are
    -- otherwise invisible to the oracle. Record those routes right
    -- here, where the decision is made. A ClickUI route (one that
    -- DOES queue a LuaUIClickEvent/LuaUIRightClickEvent, or the
    -- right-click-consumed-by-a-left-only-control fallback) is
    -- deliberately NOT recorded here at press time any more —
    -- H1's `drag` action can start on a UI control exactly as
    -- easily as on game-world empty space, and whether the whole
    -- gesture is really a plain click or a widget drag can only be
    -- known once the matching release arrives. 'pendingUIClickRef'
    -- carries that route's (callback, press-x, press-y) forward to
    -- the release handling below, which records EXACTLY ONE
    -- "input.click" or "input.drag" outcome for it — never both,
    -- mirroring scripts/unit_drag_select.lua's own defer-to-release
    -- fix for game-world drags.
    pendingUIClickRef ← newIORef (Nothing ∷ Maybe (Text, Text, Double, Double))
    -- #745: a DISCRETE (non-drag-activation) ClickUI press's activation
    -- decision, deferred to the matching release — see
    -- 'UI.ControlActivation'. 'Nothing' for a drag-activation control
    -- (fires immediately, exactly as before #745) or any non-ClickUI
    -- route.
    pendingActivationRef ← newIORef (Nothing ∷ Maybe PendingActivation)
    let recordRouteOutcome ∷ Text → Maybe Text → IO ()
        recordRouteOutcome outcome handler = do
            gt ← readIORef (gameTimeRef env)
            let (whereX, whereY) = toFb (x, y)
            pushActionOutcome (actionOutcomeRef env) ActionOutcome
                { aoTs = gt, aoKind = "input.click", aoOutcome = outcome
                -- The real click position (review round 9 — these
                -- routes previously hard-coded Nothing/Nothing,
                -- losing the location entirely; the critic needs it
                -- to identify a phantom affordance). Framebuffer
                -- coords (#774 — converted from the window-space x/y
                -- used for routing/Lua dispatch below), matching
                -- scripts/init_mouse.lua's recordClick for the
                -- game-chain routes.
                , aoWhereX = Just whereX, aoWhereY = Just whereY, aoTarget = Nothing
                , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
                , aoReason = Nothing, aoHandler = handler
                }

    -- Each press is routed exactly one way (ClickRoute): to the
    -- game (LuaMouseDownEvent), to a UI element (LuaUIClickEvent /
    -- right-click), or swallowed with no Lua event (tooltip lock,
    -- minimized window). The route is recorded per button so the
    -- matching release can hand it to Lua, and swallowed presses
    -- are kept out of inpMouseBtns so button pollers (camera
    -- middle-drag) don't react to clicks the tooltip ate.
    mRoute ← if state ≢ GLFW.MouseButtonState'Pressed then return Nothing else fmap Just $ do
        logDebug logger CatInput $ "Mouse button pressed: button=" <> T.pack (show btn)
                                <> ", pos=(" <> T.pack (show x) <> "," <> T.pack (show y) <> ")"

        let scaleX = fromIntegral fbW / fromIntegral winW
            scaleY = fromIntegral fbH / fromIntegral winH
            mouseX = realToFrac x * scaleX
            mouseY = realToFrac y * scaleY

        logDebug logger CatUI $ "Click at (" <> T.pack (show mouseX) <> ", " <> T.pack (show mouseY) <> ")"

        uiMgr ← readIORef (uiManagerRef env)
        let mousePos = (mouseX, mouseY)

        -- Zero-size window/framebuffer (minimize): winW/winH = 0 makes
        -- the scale division yield NaN/Infinity, and fbW/fbH = 0
        -- collapses the scaled coord to (0,0) — either way the hit-test
        -- below would dispatch a bogus UI/game event. Drop the click.
        if viewportDegenerate winW winH fbW fbH
          then do
            recordRouteOutcome "noop" (Just "degenerate_viewport")
            return ClickSwallowed
          else case btn of
          -- Middle button: toggle tooltip lock when a tooltip is up.
          -- Otherwise hit-test the UI so a middle-click over a
          -- pointer-blocking UI/menu surface can't fall through to
          -- gameplay middle-click behavior (the loop uses it for
          -- camera dragging — see Engine.Loop.Camera). Middle-click
          -- has no UI handler to dispatch to and exists purely to
          -- pan the camera, so isPointerSurfaceBlocked consults
          -- elementBlocksPointer (#743 — a menu/panel/HUD background
          -- that opts in via ueBlocksPointer, or any control with a
          -- registered click callback, still blocks it) OR a modal
          -- boundary existing at all (#742 review round 1: a gap in
          -- the modal's own layout must not leak a middle-click
          -- through to panning behind it). #743 narrowed this from
          -- "any sized element" pre-#742 parity — a purely visual,
          -- non-blocking element no longer swallows middle-click on
          -- its own. A blocked point SWALLOWS the click: the camera
          -- middle-drag polls inpMouseBtns directly (bypassing the
          -- route), so only ClickSwallowed — which keeps the button
          -- out of inpMouseBtns — actually stops the drag. Empty
          -- (non-blocking, no-modal) space still routes the
          -- middle-click to the world.
          GLFW.MouseButton'3 →
            if isTooltipVisible uiMgr
              then do
                atomicModifyIORef' (uiManagerRef env) $ \m →
                    (toggleTooltipLock m, ())
                recordRouteOutcome "accepted" (Just "tooltip_lock_toggle")
                return ClickSwallowed
              else if isPointerSurfaceBlocked mousePos uiMgr
                then do
                    logDebug logger CatUI
                        "Middle-click swallowed by UI surface"
                    -- #743 review round 5: middle-click has no Lua
                    -- event of its own to ride a focus-clear on
                    -- (unlike a RouteElement click), so — same as
                    -- the left/right-click RouteBlocked cases above
                    -- — clear stale UI focus explicitly here; a
                    -- focused textbox must not stay captured just
                    -- because the consuming surface has no callback.
                    Q.writeQueue lq LuaUIFocusLost
                    recordRouteOutcome "noop" (Just "ui_surface_block")
                    return ClickSwallowed
                else do
                    -- scripts/init_mouse.lua's onMouseDown only
                    -- branches on MOUSE_LEFT/MOUSE_RIGHT, so a middle
                    -- press reaching "game" falls through the whole
                    -- chain with no recordClick call of its own (it
                    -- drives camera-drag polling instead, not the
                    -- click-dispatch chain) — record it here or it's
                    -- invisible to F4 entirely (review round 2).
                    -- Deferred to release (#730 review round 3), same
                    -- as the UI routes below: an H1 `drag` action can
                    -- specify button="middle" just as easily as
                    -- "left", and a middle-button press+hold+release
                    -- IS the camera-drag gesture, not a discrete
                    -- click — recording immediately at press would
                    -- leave it permanently misclassified as
                    -- "input.click" even when the camera actually panned.
                    Q.writeQueue lq (LuaMouseDownEvent btn x y)
                    writeIORef pendingUIClickRef
                        (Just ("input.click", "camera_drag", x, y))
                    return ClickGame

          -- All other buttons: if a tooltip is locked, intercept the
          -- click. Inside the locked box → swallow (the locked
          -- tooltip is the topmost UI and consumes clicks on itself).
          -- Outside → release the lock + hide, then dispatch the
          -- click normally so the user-perceivable behavior is
          -- "first click anywhere off the tooltip dismisses it AND
          -- still does whatever it would have done."
          _ → do
            let locked      = isTooltipLocked uiMgr
                clickInside = locked ∧ isPointInLockedTooltip mousePos uiMgr
            if clickInside
              then do
                recordRouteOutcome "accepted" (Just "tooltip_lock_dismiss")
                return ClickSwallowed
              else do
                when locked $
                    atomicModifyIORef' (uiManagerRef env) $ \m →
                        (clearTooltipLock m, ())
                -- Re-read manager after the unlock mutation so we
                -- don't hit-test against the now-hidden tooltip page.
                uiMgr' ← if locked
                            then readIORef (uiManagerRef env)
                            else return uiMgr
                case btn of
                  -- #742: routePointer scopes the underlying element
                  -- search to pages at-or-above the topmost visible
                  -- input-exclusive modal page (if any), so a miss on
                  -- the modal can no longer fall through to a lower
                  -- page's owned control. With no modal boundary the
                  -- scope is every visible page, so behaviour here is
                  -- unchanged from the pre-#742 global search.
                  GLFW.MouseButton'1 → do
                    let leftRoute = routePointer PointerLeftClick mousePos uiMgr'
                    -- #745 review rounds 1 & 2: a left click also moves
                    -- keyboard CONTROL focus — landing on an eligible
                    -- non-text control focuses it, anything else (a
                    -- text field, a blocked/consumed route, a miss)
                    -- clears it, mirroring how this same click already
                    -- clears the pre-existing TEXT focus (LuaUIFocusLost)
                    -- on those same non-focusing routes below. Reports
                    -- an actual transition the same way Tab/Escape/
                    -- validation-repair already do.
                    mChanged ← atomicModifyIORef' (uiManagerRef env) $ \m →
                        let target = case leftRoute of
                                RouteElement eh _ | isEligibleControl eh m → Just eh
                                _ → Nothing
                        in ( maybe (clearControlFocus m) (`setControlFocus` m) target
                           , if target ≡ getControlFocus m then Nothing else Just target )
                    case mChanged of
                        Just newFocus → Q.writeQueue lq (LuaUIControlFocusChanged newFocus)
                        Nothing → return ()
                    case leftRoute of
                        RouteElement elemHandle callback → do
                            -- #745: a drag-activation control (slider
                            -- knob, scrollbar thumb) fires immediately,
                            -- exactly as every control did before #745
                            -- — its press IS the start of a drag. Every
                            -- other (discrete) control defers: only a
                            -- LuaUIPressBeginEvent fires now (pending
                            -- visual only), and the matching release
                            -- decides via 'resolveActivation' whether
                            -- LuaUIClickEvent ever fires at all.
                            if isElementDragActivation elemHandle uiMgr'
                                then Q.writeQueue lq
                                        (LuaUIClickEvent elemHandle callback x y)
                                else do
                                    Q.writeQueue lq
                                        (LuaUIPressBeginEvent elemHandle callback)
                                    writeIORef pendingActivationRef $ Just $
                                        beginActivation PointerLeftClick elemHandle
                            logDebug logger CatUI $
                                "UI element left-clicked: " <> callback
                            writeIORef pendingUIClickRef
                                (Just ("input.click", callback, x, y))
                            return ClickUI
                        -- #743: a pointer-blocking element with no
                        -- left-click callback of its own (e.g. a
                        -- right-click-only control, or a scroll-
                        -- capturing log panel background) consumes
                        -- the press — no fake Lua callback, but it
                        -- can't fall through to a lower element,
                        -- page, or gameplay either. Clears stale UI
                        -- focus exactly like the miss path below,
                        -- since no LuaUIClickEvent rides to do it.
                        RouteBlocked _elemHandle → do
                            Q.writeQueue lq LuaUIFocusLost
                            recordRouteOutcome "noop" (Just "ui_pointer_block")
                            return ClickSwallowed
                        -- RouteConsumedNoHandler never arises for
                        -- left-click (only PointerRightClick's
                        -- left-click fallback produces it) — a plain
                        -- miss, whether genuinely empty or stopped at
                        -- a modal boundary, is forwarded to Lua
                        -- exactly as before #742: the debug overlay's
                        -- parallel hit-test and the shell still get
                        -- first refusal on it regardless of any
                        -- boundary, and gameplay's own handlers
                        -- additionally consult isGameplayBlocked
                        -- before acting.
                        _ → do
                            Q.writeQueue lq LuaUIFocusLost
                            Q.writeQueue lq (LuaMouseDownEvent btn x y)
                            return ClickGame

                  GLFW.MouseButton'2 →
                    case routePointer PointerRightClick mousePos uiMgr' of
                        RouteElement elemHandle callback → do
                            -- #745: see the left-click RouteElement
                            -- branch above — the same drag-activation
                            -- exemption and press/release deferral
                            -- applies uniformly across buttons.
                            if isElementDragActivation elemHandle uiMgr'
                                then Q.writeQueue lq
                                        (LuaUIRightClickEvent elemHandle callback x y)
                                else do
                                    Q.writeQueue lq
                                        (LuaUIPressBeginEvent elemHandle callback)
                                    writeIORef pendingActivationRef $ Just $
                                        beginActivation PointerRightClick elemHandle
                            logDebug logger CatUI $
                                "UI element right-clicked: " <> callback
                            writeIORef pendingUIClickRef
                                (Just ("input.rightClick", callback, x, y))
                            return ClickUI
                        -- No right-click handler under the cursor
                        -- (within the modal-scoped search), but an
                        -- ordinary clickable control is still there:
                        -- consume the click so it can't fall through
                        -- to gameplay (mirrors the left-click blocking
                        -- semantics); only truly empty (or
                        -- modal-blocked) UI space routes the
                        -- right-click to the world.
                        RouteConsumedNoHandler _elemHandle leftClickCallback → do
                            -- Consumed by a clickable control with no
                            -- right-click handler (e.g. an ordinary
                            -- button). Left-clicking such a control
                            -- clears textbox/dropdown focus via the
                            -- dispatched click event; the right-click
                            -- has no event to ride, so clear focus
                            -- explicitly here, otherwise a focused
                            -- widget stays captured.
                            Q.writeQueue lq LuaUIFocusLost
                            logDebug logger CatUI
                                "Right-click consumed by clickable UI element (no handler)"
                            -- #743 review round 6: NO callback ever
                            -- fires for this route — no
                            -- LuaUIRightClickEvent, and the control's
                            -- own left-click callback must not fire
                            -- either (that's the whole point of this
                            -- route) — so recording it as an
                            -- "accepted" input.click (the pre-#743
                            -- behavior, deferred to release like a
                            -- real fired click) mislabeled a dead
                            -- consumption as a real action, and its
                            -- release read as downRoute "ui" despite
                            -- nothing firing. Recorded immediately as
                            -- a UI no-op instead — same "noop"
                            -- classification #743's RouteBlocked case
                            -- below uses — with the consuming
                            -- control's OWN left-click callback name
                            -- preserved as the handler for diagnostic
                            -- identity (#730 review round 8's
                            -- "records the consuming handler"
                            -- acceptance still holds, just under
                            -- outcome "noop" rather than "accepted").
                            recordRouteOutcome "noop" (Just leftClickCallback)
                            return ClickSwallowed
                        -- #743: a pointer-blocking element with
                        -- neither a right-click callback nor a
                        -- left-click one to fall back to (e.g. a
                        -- scroll-capturing log panel background) —
                        -- consumed, no fake callback, same
                        -- explicit-focus-clear rationale as the
                        -- left-click RouteBlocked case above.
                        RouteBlocked _elemHandle → do
                            Q.writeQueue lq LuaUIFocusLost
                            recordRouteOutcome "noop" (Just "ui_pointer_block")
                            return ClickSwallowed
                        RouteMiss → do
                            -- A right-click that misses all UI clears
                            -- focus before reaching gameplay, exactly
                            -- like the left-click miss path above —
                            -- otherwise a focused textbox/dropdown
                            -- keeps capturing the keyboard.
                            Q.writeQueue lq LuaUIFocusLost
                            Q.writeQueue lq (LuaMouseDownEvent btn x y)
                            return ClickGame

                  _ → do
                    -- GLFW mouse buttons 4-8 (side/extra buttons):
                    -- Dispatch.hs maps anything past MouseButton'3 to
                    -- Lua button 0, and init_mouse.lua's onMouseDown
                    -- only branches on MOUSE_LEFT/MOUSE_RIGHT, so this
                    -- reaches the end of that chain with no defined
                    -- behavior anywhere and no recordClick call
                    -- (review round 3) — record it here instead.
                    Q.writeQueue lq (LuaMouseDownEvent btn x y)
                    recordRouteOutcome "noop" (Just "unmapped_button")
                    return ClickGame

    -- The release ALWAYS goes to Lua — UI widget drags (slider
    -- knob, scrollbar tab) start from a LuaUIClickEvent and rely
    -- on uiManager.onMouseUp to end them. The press's route rides
    -- along so handlers wanting strict down/up pairing can filter
    -- on "game".
    when (state ≡ GLFW.MouseButtonState'Released) $ do
        logDebug logger CatInput $ "Mouse button released: button=" <> T.pack (show btn)
                                <> ", pos=(" <> T.pack (show x) <> "," <> T.pack (show y) <> ")"
        let downRoute = Map.findWithDefault ClickGame btn (inpMouseRoutes inpSt)
        Q.writeQueue lq (LuaMouseUpEvent btn x y downRoute)

        -- #745: resolve any pending discrete-control activation for
        -- this button BEFORE recording its F4 outcome below (see
        -- 'Engine.Input.Thread.Mouse.Activation') — its Activate/
        -- Cancel becomes the recorded aoOutcome below.
        mActivationOutcome ← resolvePendingActivation env x y winW winH fbW fbH
            (Map.lookup btn (inpPendingActivation inpSt))

        -- F4 (#730 review rounds 2 & 3): resolve a deferred
        -- ClickUI/middle-button-camera-drag press's ONE outcome
        -- now that the whole gesture is known — the original click
        -- kind if the release landed close to the press (a plain
        -- click, or a below-threshold H1 `drag`), else "input.drag"
        -- (a real widget/camera drag gesture, past the same
        -- threshold scripts/unit_drag_select.lua uses for the
        -- game-world case). Never both — this IS the press-time
        -- record that PR #704 used to fire unconditionally (UI) or
        -- 'recordRouteOutcome' fired immediately (camera_drag).
        -- Keyed on 'inpPendingUIClick' presence alone, not
        -- 'downRoute' — the only two producers (the ClickUI
        -- branches and the middle-button camera_drag branch above)
        -- are the only routes that ever populate it.
        case Map.lookup btn (inpPendingUIClick inpSt) of
                Nothing → return ()
                Just (clickKind, callback, px, py) → do
                    gt ← readIORef (gameTimeRef env)
                    -- The threshold compare stays in window pixels
                    -- (dx/dy/movedPx) — uiDragThresholdPx is defined
                    -- to match scripts/unit_drag_select.lua's window-
                    -- space DRAG_THRESHOLD; only the chosen location is
                    -- converted to framebuffer space (#774) for the
                    -- recorded outcome.
                    let dx = x - px
                        dy = y - py
                        movedPx = sqrt (dx * dx + dy * dy)
                        (kind, whereXWin, whereYWin) =
                            if movedPx ≥ uiDragThresholdPx
                                then ("input.drag", x, y)
                                else (clickKind, px, py)
                        (whereX, whereY) = toFb (whereXWin, whereYWin)
                        -- #745: a deferred discrete control's F4
                        -- outcome truthfully reflects whether it
                        -- actually activated; every other route (drag-
                        -- activation control, camera-drag) has no
                        -- 'mActivationOutcome' and keeps recording
                        -- "accepted" exactly as before #745.
                        outcomeName = maybe "accepted" activationOutcomeName mActivationOutcome
                        cancelReason = case mActivationOutcome of
                            Just (Cancel reason) → Just reason
                            _                    → Nothing
                    pushActionOutcome (actionOutcomeRef env) ActionOutcome
                        { aoTs = gt, aoKind = kind, aoOutcome = outcomeName
                        , aoWhereX = Just whereX, aoWhereY = Just whereY
                        , aoTarget = Nothing
                        , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
                        , aoReason = cancelReason, aoHandler = Just callback
                        }

    mPendingUIClick ← readIORef pendingUIClickRef
    mPendingActivation ← readIORef pendingActivationRef
    return $ case mRoute of
        Just route → inpSt
            { inpMousePos    = pos
            , inpMouseRoutes = Map.insert btn route (inpMouseRoutes inpSt)
            , inpMouseBtns   = if route ≡ ClickSwallowed
                then inpMouseBtns inpSt
                else Map.insert btn True (inpMouseBtns inpSt)
            , inpPendingUIClick = case mPendingUIClick of
                Just pc → Map.insert btn pc (inpPendingUIClick inpSt)
                Nothing → Map.delete btn (inpPendingUIClick inpSt)
            , inpPendingActivation = case mPendingActivation of
                Just pa → Map.insert btn pa (inpPendingActivation inpSt)
                Nothing → Map.delete btn (inpPendingActivation inpSt)
            }
        Nothing → inpSt
            { inpMousePos  = pos
            , inpMouseBtns = Map.insert btn False (inpMouseBtns inpSt)
            -- Consumed above (if it was ever set) — a released
            -- button carries no pending click/activation into the
            -- next press (#745).
            , inpPendingUIClick = Map.delete btn (inpPendingUIClick inpSt)
            , inpPendingActivation = Map.delete btn (inpPendingActivation inpSt)
            }
