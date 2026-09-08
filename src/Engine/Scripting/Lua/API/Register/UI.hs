-- | Installs the @UI@ global table — the pilot namespace for the
--   declarative registration contract (#2479, epic #1995 LAC-2).
--
--   Every verb here goes through
--   'Engine.Scripting.Lua.API.Internal.registerLuaVerb', which installs
--   the action through 'registerLuaFunction' exactly as before and
--   carries an 'Engine.Scripting.Lua.API.Descriptor.LuaVerb' beside it.
--   The descriptor is metadata: nothing consults it at call time, so
--   every verb accepts what it accepted before and returns what it
--   returned before. What changes is that arity, argument kinds and
--   return shape are now written down where the action is registered
--   instead of being implied by the action's body.
--
--   The metadata is read off the implementations in
--   "Engine.Scripting.Lua.API.UI" and describes what they really do,
--   coercions and fallbacks included — @Lua.tointeger@ accepts a
--   numeric string (#1497), @Lua.toboolean@ never fails and treats an
--   omitted argument as @false@, and a verb whose required arguments
--   are missing is usually a silent no-op rather than an error.
--
--   'installUIAPI' returns the descriptors the very expressions below
--   installed, so a test can hold the metadata against the live table
--   without a second list that could disagree with the first.
module Engine.Scripting.Lua.API.Register.UI
  ( registerUIAPI
  , installUIAPI
  ) where

import UPrelude
import Engine.Scripting.Lua.API.Internal (registerLuaVerb)
import Engine.Scripting.Lua.API.Descriptor
import Engine.Scripting.Lua.API.UI
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @UI@ global table.
registerUIAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerUIAPI = void ∘ installUIAPI

-- * Shared argument and result shapes
--
--   These recur verbatim across the namespace; naming them once keeps
--   the 82 descriptors below readable and keeps one wording for one
--   fact.

-- | The element handle almost every verb takes first.
elementArg ∷ LuaArg
elementArg = argReq "elementHandle" TInteger elementHandleDoc

elementHandleDoc ∷ Text
elementHandleDoc = "Handle from UI.newElement/newBox/newText/newSprite. Read with Lua.tointeger, which also accepts a numeric string; a missing or non-numeric handle makes the call a no-op."

-- | The page handle the page-scoped verbs take.
pageArg ∷ LuaArg
pageArg = argReq "pageHandle" TInteger pageHandleDoc

pageHandleDoc ∷ Text
pageHandleDoc = "Handle from UI.newPage. Read with Lua.tointeger, which also accepts a numeric string; a missing or non-numeric handle makes the call a no-op."

-- | A boolean flag read with @Lua.toboolean@, which coerces instead of
--   validating: omitted or @nil@ is @false@, and any other value but
--   @false@ is @true@.
flagArg ∷ Text → Text → LuaArg
flagArg name what = argOpt name TBoolean
    (what <> " Read with Lua.toboolean, so an omitted or nil argument means false and any other value means true.")

-- | The @{x, y, w, h}@ sub-table 'UI.getEffectiveClip' and the
--   @effectiveClip@/@interactiveBounds@ info fields all push.
rectRecord ∷ Text → LuaType
rectRecord what = TRecord
    [ recField "x" TNumber (what <> " left edge, in framebuffer pixels.")
    , recField "y" TNumber (what <> " top edge, in framebuffer pixels.")
    , recField "w" TNumber (what <> " width, in framebuffer pixels.")
    , recField "h" TNumber (what <> " height, in framebuffer pixels.")
    ]

-- | The record 'Engine.Scripting.Lua.API.UI.Property.pushElementInfoTable'
--   builds, shared verbatim by @UI.getElementInfo@ (one element) and
--   @UI.getVisibleElements@ (every element on every visible page).
elementInfoRecord ∷ LuaType
elementInfoRecord = TRecord
    [ recField "handle" TInteger "This element's own handle."
    , recField "paintKey" TInteger "Page-band + accumulated-zIndex ordering key hit resolution actually uses (#783); not a total order on its own."
    , recField "paintOrder" TInteger "Traversal-position tiebreak applied at equal paintKey; later-painted wins."
    , recField "x" TNumber "Absolute content-left position, in framebuffer pixels."
    , recField "y" TNumber "Absolute content-top position, in framebuffer pixels."
    , recField "width" TNumber "Content width, in framebuffer pixels."
    , recField "height" TNumber "Content height, in framebuffer pixels."
    , recField "visible" TBoolean "Effective visibility: this element AND every ancestor up to the page root."
    , recField "clickable" TBoolean "The raw ueClickable opt-in on this element."
    , recField "interactive" TBoolean "Has a left OR right click callback; deliberately conflates the two, unlike leftClickTarget."
    , recField "zIndex" TInteger "This element's own z-index, ignoring its page band and its ancestors'."
    , recField "name" TString "The element's name, as given at creation."
    , recField "text" (TNullable TString) "Best-effort visible caption: this element's own text, else its first text-rendering child; nil when neither exists."
    , recField "page" TString "The owning page's name; empty when the page is gone."
    , recField "pageVisible" TBoolean "Whether the owning page is currently shown; coarser than visible."
    , recField "hovered" TBoolean "Whether this element is the manager's current hover target."
    , recField "focused" TBoolean "TEXT-input focus (upmGlobalFocus) only."
    , recField "controlFocused" TBoolean "Keyboard CONTROL focus (upmControlFocus, #745), reported separately from focused."
    , recField "pointerBlocking" TBoolean "Effective elementBlocksPointer, not just the raw opt-in (#743)."
    , recField "inScope" TBoolean "isPageInScope for this element's own page: the modal-scope decision routePointer applies first (#1750)."
    , recField "leftClickTarget" TBoolean "Active left-click target: clickable AND an onClick callback is registered."
    , recField "leftClickAffordance" TBoolean "An onClick callback is registered at all, independent of clickable; true with leftClickTarget false is a shown-but-disabled control."
    , recField "scrollCapturing" TBoolean "Effective elementCapturesScroll, not just the raw opt-in (#743)."
    , recField "clipsChildren" TBoolean "The raw ueClipChildren opt-in: whether this element clips its own descendants."
    , recField "interactiveOverflow" TBoolean "The raw ueInteractiveOverflow opt-in (#749)."
    , recField "effectiveClip" (TNullable (rectRecord "The clip this element is subject to:"))
        "Intersection of every clipping ancestor's bounds, or nil when unclipped (#747)."
    , recField "interactiveBounds" (TNullable (rectRecord "The interactive rect a hit resolves against:"))
        "Clip-intersected interactive rect (#749), or nil when the element is entirely clipped away."
    ]

-- | Install the @UI@ global table, yielding every descriptor it
--   installed, in registration order.
installUIAPI ∷ EngineEnv → Lua.LuaE Lua.Exception [LuaVerb]
installUIAPI env = do
  Lua.newtable
  descriptors ← sequence
    -- Pages
    [ registerLuaVerb (luaVerb "newPage"
        [ argReq "name" TString "Page name. Read with Lua.tostring, which also accepts a number."
        , argReq "layer" TString "Layer band: hud, overlay, menu, modal, tooltip or debug, matched case-insensitively; any other string falls back to menu."
        ]
        (retVals [resVal "pageHandle" (TNullable TInteger)
            "The new page's handle, or nil when either argument is missing."])
        "Create a page on the named layer band.")
        (uiNewPageFn env)
    , registerLuaVerb (luaVerb "deletePage"
        [pageArg]
        retNone
        "Delete a page and recursively every element it owns, reporting any resulting control-focus change.")
        (uiDeletePageFn env)
    , registerLuaVerb (luaVerb "showPage"
        [pageArg]
        retNone
        "Show a page. Its elements become eligible for rendering and hit-testing.")
        (uiShowPageFn env)
    , registerLuaVerb (luaVerb "hidePage"
        [pageArg]
        retNone
        "Hide a page, clearing text and control focus held on it and reporting the control-focus change (#745).")
        (uiHidePageFn env)
    , registerLuaVerb (luaVerb "setPageInputExclusive"
        [ pageArg
        , flagArg "exclusive" "Whether this page establishes an input-exclusive modal boundary."
        ]
        retNone
        "Override a page's default modal-boundary classification, which creation derives from its layer (#742).")
        (uiSetPageInputExclusiveFn env)
    , registerLuaVerb (luaVerb "isPageInputExclusive"
        [pageArg]
        (retVals [resVal "exclusive" TBoolean
            "Whether the page is input-exclusive; false for an unknown handle."])
        "Read a page's modal-boundary classification (#742).")
        (uiIsPageInputExclusiveFn env)
    , registerLuaVerb (luaVerb "isInputBlocked"
        []
        (retVals [resVal "blocked" TBoolean
            "True while any visible page establishes an input-exclusive modal boundary."])
        "Whether gameplay input is currently blocked by a modal UI boundary (#742).")
        (uiIsInputBlockedFn env)
    , registerLuaVerb (luaVerb "isPageInScope"
        [pageArg]
        (retVals [resVal "inScope" TBoolean
            "True when the page is at or above the modal boundary, or there is no boundary; an unknown page is never in scope."])
        "Whether a page is inside the current modal scope (#742).")
        (uiIsPageInScopeFn env)

    -- Element creation
    , registerLuaVerb (luaVerb "newElement"
        [ argReq "name" TString "Element name."
        , argReq "width" TNumber "Content width, in framebuffer pixels."
        , argReq "height" TNumber "Content height, in framebuffer pixels."
        , pageArg
        ]
        (retVals [resVal "elementHandle" (TNullable TInteger)
            "The new element's handle, or nil when any argument is missing."])
        "Create a bare element with no render data of its own.")
        (uiNewElementFn env)
    , registerLuaVerb (luaVerb "newBox"
        [ argReq "name" TString "Element name."
        , argReq "width" TNumber "Content width, in framebuffer pixels."
        , argReq "height" TNumber "Content height, in framebuffer pixels."
        , argReq "boxTextureHandle" TInteger "Handle from UI.loadBoxTextures naming the nine-slice set."
        , argReq "tileSize" TNumber "Edge/corner tile size, in framebuffer pixels."
        , argReq "r" TNumber "Red, 0..1."
        , argReq "g" TNumber "Green, 0..1."
        , argReq "b" TNumber "Blue, 0..1."
        , argReq "a" TNumber "Alpha, 0..1."
        , argReq "overflow" TNumber "How far the border bleeds outside the content rect, in framebuffer pixels."
        , pageArg
        ]
        (retVals [resVal "elementHandle" (TNullable TInteger)
            "The new element's handle, or nil when any argument is missing."])
        "Create a nine-slice box element.")
        (uiNewBoxFn env)
    , registerLuaVerb (luaVerb "newText"
        [ argReq "name" TString "Element name."
        , argReq "text" TString "Initial caption."
        , argReq "fontHandle" TInteger "Font handle from the asset API."
        , argReq "size" TNumber "Font size, in framebuffer pixels."
        , argReq "r" TNumber "Red, 0..1."
        , argReq "g" TNumber "Green, 0..1."
        , argReq "b" TNumber "Blue, 0..1."
        , argReq "a" TNumber "Alpha, 0..1."
        , pageArg
        ]
        (retVals [resVal "elementHandle" (TNullable TInteger)
            "The new element's handle, or nil when any argument is missing."])
        "Create a text element.")
        (uiNewTextFn env)
    , registerLuaVerb (luaVerb "newSprite"
        [ argReq "name" TString "Element name."
        , argReq "width" TNumber "Content width, in framebuffer pixels."
        , argReq "height" TNumber "Content height, in framebuffer pixels."
        , argReq "textureHandle" TInteger "Texture handle from the asset API."
        , argReq "r" TNumber "Red, 0..1."
        , argReq "g" TNumber "Green, 0..1."
        , argReq "b" TNumber "Blue, 0..1."
        , argReq "a" TNumber "Alpha, 0..1."
        , pageArg
        ]
        (retVals [resVal "elementHandle" (TNullable TInteger)
            "The new element's handle, or nil when any argument is missing."])
        "Create a sprite element.")
        (uiNewSpriteFn env)

    -- The element tree
    , registerLuaVerb (luaVerb "addToPage"
        [ pageArg
        , elementArg
        , argReq "x" TNumber "Position relative to the page, in framebuffer pixels."
        , argReq "y" TNumber "Position relative to the page, in framebuffer pixels."
        ]
        retNone
        "Attach an element to a page at a position.")
        (uiAddToPageFn env)
    , registerLuaVerb (luaVerb "addChild"
        [ argReq "parentHandle" TInteger elementHandleDoc
        , argReq "childHandle" TInteger elementHandleDoc
        , argReq "x" TNumber "Position relative to the parent, in framebuffer pixels."
        , argReq "y" TNumber "Position relative to the parent, in framebuffer pixels."
        ]
        retNone
        "Attach an element as a child of another element.")
        (uiAddChildFn env)
    , registerLuaVerb (luaVerb "removeElement"
        [elementArg]
        retNone
        "Detach an element from its parent without deleting it, reporting any resulting control-focus change.")
        (uiRemoveElementFn env)
    , registerLuaVerb (luaVerb "deleteElement"
        [elementArg]
        retNone
        "Delete an element and its subtree, reporting any resulting control-focus change.")
        (uiDeleteElementFn env)
    , registerLuaVerb (luaVerb "findElementAt"
        [ argReq "x" TNumber "Framebuffer-pixel x to hit-test."
        , argReq "y" TNumber "Framebuffer-pixel y to hit-test."
        ]
        (retVals [resVal "elementHandle" (TNullable TInteger)
            "Top-most visible element whose bounds contain the point, or nil."])
        "Hit-test every visible page for the top-most element at a point.")
        (uiFindElementAtFn env)
    , registerLuaVerb (luaVerb "getElementOnClick"
        [elementArg]
        (retVals [resVal "callbackName" (TNullable TString)
            "The element's registered left-click callback name, or nil when it has none or the handle is unknown."])
        "Read an element's left-click callback name.")
        (uiGetElementOnClickFn env)
    , registerLuaVerb (luaVerb "findHoverTarget"
        [ argReq "x" TNumber "Framebuffer-pixel x to hit-test."
        , argReq "y" TNumber "Framebuffer-pixel y to hit-test."
        ]
        (retVals
            [ resVal "elementHandle" (TNullable TInteger)
                "Nearest ancestor of the element at the point carrying a left-click callback, or nil. UI.Manager.Query.findClickableAncestor tests ueOnClick alone, so an ancestor whose clickable is false is still reported."
            , resVal "callbackName" (TNullable TString)
                "That ancestor's registered left-click callback name, or nil."
            ])
        "Find the left-click AFFORDANCE a hover at this point sits over. This is getElementInfo's leftClickAffordance, not its leftClickTarget: a shown-but-disabled control is reported here even though a real click over it would not activate. Always pushes two values; both are nil together.")
        (uiFindHoverTargetFn env)

    -- Per-element text buffers
    , registerLuaVerb (luaVerb "enableTextInput"
        [elementArg]
        retNone
        "Give an element a text buffer, making it eligible for text focus.")
        (uiEnableTextInputFn env)
    , registerLuaVerb (luaVerb "getTextInput"
        [elementArg]
        (retVals [resVal "text" (TNullable TString)
            "The buffer's content, or nil when the element has no text buffer."])
        "Read an element's text-buffer content.")
        (uiGetTextFn env)
    , registerLuaVerb (luaVerb "setTextInput"
        [ elementArg
        , argReq "text" TString "Replacement content. Read with Lua.tostring, which also accepts a number."
        ]
        retNone
        "Replace an element's text-buffer content, leaving the cursor at the end.")
        (uiSetTextInputFn env)
    , registerLuaVerb (luaVerb "getCursor"
        [elementArg]
        (retVals [resVal "position" (TNullable TInteger)
            "Zero-based code-point offset, or nil when the element has no text buffer."])
        "Read an element's text cursor position. Positions are code-point offsets, never UTF-8 byte offsets.")
        (uiGetCursorFn env)
    , registerLuaVerb (luaVerb "setCursor"
        [ elementArg
        , argReq "position" TInteger "Zero-based code-point offset; clamped to the buffer's length."
        ]
        retNone
        "Move an element's text cursor.")
        (uiSetCursorFn env)
    , registerLuaVerb (luaVerb "insertChar"
        [ elementArg
        , argReq "char" TString "Only its first code point is inserted; an empty string inserts nothing."
        ]
        retNone
        "Insert one character at the cursor.")
        (uiInsertCharFn env)
    , registerLuaVerb (luaVerb "deleteBackward"
        [elementArg]
        retNone
        "Delete the character before the cursor (Backspace).")
        (uiDeleteBackwardFn env)
    , registerLuaVerb (luaVerb "deleteForward"
        [elementArg]
        retNone
        "Delete the character at the cursor (Delete).")
        (uiDeleteForwardFn env)
    , registerLuaVerb (luaVerb "cursorLeft"
        [elementArg]
        retNone
        "Move the text cursor one code point left.")
        (uiCursorLeftFn env)
    , registerLuaVerb (luaVerb "cursorRight"
        [elementArg]
        retNone
        "Move the text cursor one code point right.")
        (uiCursorRightFn env)
    , registerLuaVerb (luaVerb "cursorHome"
        [elementArg]
        retNone
        "Move the text cursor to the start of the buffer.")
        (uiCursorHomeFn env)
    , registerLuaVerb (luaVerb "cursorEnd"
        [elementArg]
        retNone
        "Move the text cursor to the end of the buffer.")
        (uiCursorEndFn env)

    -- Text focus and control focus (#745)
    , registerLuaVerb (luaVerb "setFocus"
        [elementArg]
        retNone
        "Give an element TEXT-input focus, deciding which buffer receives typed characters.")
        (uiSetFocusFn env)
    , registerLuaVerb (luaVerb "clearFocus"
        []
        retNone
        "Clear text-input focus.")
        (uiClearFocusFn env)
    , registerLuaVerb (luaVerb "getFocus"
        []
        (retVals [resVal "elementHandle" (TNullable TInteger)
            "The text-focused element, or nil when nothing holds text focus."])
        "Read the text-input focus.")
        (uiGetFocusFn env)
    , registerLuaVerb (luaVerb "hasFocus"
        [elementArg]
        (retVals [resVal "focused" TBoolean
            "Whether this element holds text focus; false for a missing or unknown handle."])
        "Whether an element holds text-input focus.")
        (uiHasFocusFn env)
    , registerLuaVerb (luaVerb "setControlFocus"
        [elementArg]
        retNone
        "Give an element keyboard CONTROL focus, deciding what Enter/Space activates (#745). Reports the transition to Lua.")
        (uiSetControlFocusFn env)
    , registerLuaVerb (luaVerb "clearControlFocus"
        []
        retNone
        "Clear keyboard control focus, reporting the transition to Lua (#745).")
        (uiClearControlFocusFn env)
    , registerLuaVerb (luaVerb "getControlFocus"
        []
        (retVals [resVal "elementHandle" (TNullable TInteger)
            "The control-focused element, or nil when nothing holds control focus."])
        "Read the keyboard control focus (#745).")
        (uiGetControlFocusFn env)
    , registerLuaVerb (luaVerb "hasControlFocus"
        [elementArg]
        (retVals [resVal "focused" TBoolean
            "Whether this element holds control focus; false for a missing or unknown handle."])
        "Whether an element holds keyboard control focus (#745).")
        (uiHasControlFocusFn env)

    -- Element properties
    , registerLuaVerb (luaVerb "setPosition"
        [ elementArg
        , argReq "x" TNumber "Position relative to the parent or page, in framebuffer pixels."
        , argReq "y" TNumber "Position relative to the parent or page, in framebuffer pixels."
        ]
        retNone
        "Move an element.")
        (uiSetPositionFn env)
    , registerLuaVerb (luaVerb "setSize"
        [ elementArg
        , argReq "width" TNumber "Content width, in framebuffer pixels."
        , argReq "height" TNumber "Content height, in framebuffer pixels."
        ]
        retNone
        "Resize an element's content rect.")
        (uiSetSizeFn env)
    , registerLuaVerb (luaVerb "setVisible"
        [ elementArg
        , flagArg "visible" "Whether this element is shown."
        ]
        retNone
        "Show or hide one element. A visible child of a hidden ancestor is still off screen.")
        (uiSetVisibleFn env)
    , registerLuaVerb (luaVerb "isPageVisible"
        [pageArg]
        (retVals [resVal "visible" TBoolean
            "Whether the page is shown; false for an unknown handle."])
        "Whether a page is currently shown.")
        (uiIsPageVisibleFn env)
    , registerLuaVerb (luaVerb "getElementInfo"
        [elementArg]
        (retVals [resVal "info" (TNullable elementInfoRecord)
            "The engine's authoritative state for this element, or nil when the handle is missing or unknown."])
        "Read one element's full engine-side state.")
        (uiGetElementInfoFn env)
    , registerLuaVerb (luaVerb "getVisibleElements"
        []
        (retVals [resVal "elements" (TArray elementInfoRecord)
            "One record per element on every currently-visible page, in page order; an empty table when nothing is visible."])
        "Bulk-read every element on every visible page, in the same record shape UI.getElementInfo returns.")
        (uiGetVisibleElementsFn env)
    , registerLuaVerb (luaVerb "setClickable"
        [ elementArg
        , flagArg "clickable" "Whether this element accepts clicks."
        ]
        retNone
        "Set an element's clickable opt-in.")
        (uiSetClickableFn env)
    , registerLuaVerb (luaVerb "setPointerBlocking"
        [ elementArg
        , flagArg "blocking" "Whether this element consumes pointer input with no click callback of its own."
        ]
        retNone
        "Set an element's explicit pointer-blocking opt-in (#743).")
        (uiSetPointerBlockingFn env)
    , registerLuaVerb (luaVerb "isPointerBlocking"
        [elementArg]
        (retVals [resVal "blocking" TBoolean
            "The effective predicate: the explicit opt-in OR the callback-derived default. False for an unknown handle."])
        "Whether an element actually consumes a pointer event right now (#743).")
        (uiIsPointerBlockingFn env)
    , registerLuaVerb (luaVerb "setScrollCapture"
        [ elementArg
        , flagArg "captures" "Whether this element consumes wheel input with no click callback of its own."
        ]
        retNone
        "Set an element's explicit scroll-capture opt-in (#743).")
        (uiSetScrollCaptureFn env)
    , registerLuaVerb (luaVerb "isScrollCapturing"
        [elementArg]
        (retVals [resVal "capturing" TBoolean
            "The effective predicate, not just the raw opt-in. False for an unknown handle."])
        "Whether an element actually consumes a wheel event right now (#743).")
        (uiIsScrollCapturingFn env)
    , registerLuaVerb (luaVerb "setDragActivation"
        [ elementArg
        , flagArg "dragActivation" "Whether this control activates on drag instead of on discrete release."
        ]
        retNone
        "Opt a control out of the discrete release-activation contract (#745). Only a slider knob or scrollbar thumb should.")
        (uiSetDragActivationFn env)
    , registerLuaVerb (luaVerb "setSteppable"
        [ elementArg
        , flagArg "steppable" "Whether arrow keys step this control while it holds control focus."
        ]
        retNone
        "Opt a control in to arrow-key stepping (#745).")
        (uiSetSteppableFn env)
    , registerLuaVerb (luaVerb "setTabIndex"
        [ elementArg
        , argReq "index" TInteger "Explicit Tab-traversal position; unset elements sort by paint-traversal position."
        ]
        retNone
        "Set an element's explicit Tab order (#745).")
        (uiSetTabIndexFn env)
    , registerLuaVerb (luaVerb "setInteractiveOverflow"
        [ elementArg
        , flagArg "interactive" "Whether the visible border counts as part of the hit target."
        ]
        retNone
        "Opt an element's expanded visual bounds into interaction instead of content-only bounds (#749).")
        (uiSetInteractiveOverflowFn env)
    , registerLuaVerb (luaVerb "isInteractiveOverflow"
        [elementArg]
        (retVals [resVal "interactive" TBoolean
            "The raw opt-in on this element; false for an unknown handle."])
        "Read an element's interactive-overflow opt-in (#749).")
        (uiIsInteractiveOverflowFn env)
    , registerLuaVerb (luaVerb "setClipChildren"
        [ elementArg
        , flagArg "clips" "Whether this element clips its descendants to its own bounds."
        ]
        retNone
        "Opt an element into clipping its descendants (#747). It never clips itself.")
        (uiSetClipChildrenFn env)
    , registerLuaVerb (luaVerb "isClipChildren"
        [elementArg]
        (retVals [resVal "clips" TBoolean
            "The raw opt-in on this element, not the clip it is itself subject to. False for an unknown handle."])
        "Read an element's clip-children opt-in (#747).")
        (uiIsClipChildrenFn env)
    , registerLuaVerb (luaVerb "getEffectiveClip"
        [elementArg]
        (retVals [resVal "clip" (TNullable (rectRecord "The clip this element is subject to:"))
            "Intersection of every clipping ancestor's bounds, or nil when unclipped or the handle is missing."])
        "Read the clip an element is actually subject to (#747), the same value rendering and hit-testing consult.")
        (uiGetEffectiveClipFn env)
    , registerLuaVerb (luaVerb "setZIndex"
        [ elementArg
        , argReq "z" TInteger "Z-index within the element's own page band."
        ]
        retNone
        "Set an element's z-index.")
        (uiSetZIndexFn env)
    , registerLuaVerb (luaVerb "setColor"
        [ elementArg
        , argReq "r" TNumber "Red, 0..1."
        , argReq "g" TNumber "Green, 0..1."
        , argReq "b" TNumber "Blue, 0..1."
        , argReq "a" TNumber "Alpha, 0..1."
        ]
        retNone
        "Recolor an element, dispatching on its render-data variant (box, sprite or text). An element with no render data is untouched.")
        (uiSetColorFn env)
    , registerLuaVerb (luaVerb "setText"
        [ elementArg
        , argReq "text" TString "Replacement caption."
        ]
        retNone
        "Replace a text element's caption.")
        (uiSetTextFn env)
    , registerLuaVerb (luaVerb "setSpriteTexture"
        [ elementArg
        , argReq "textureHandle" TInteger "Texture handle from the asset API."
        ]
        retNone
        "Rebind a sprite's texture, leaving its UVs and mirror flag alone.")
        (uiSetSpriteTextureFn env)
    , registerLuaVerb (luaVerb "setSpriteUV"
        [ elementArg
        , argReq "u0" TNumber "Left texture coordinate."
        , argReq "v0" TNumber "Top texture coordinate."
        , argReq "u1" TNumber "Right texture coordinate."
        , argReq "v1" TNumber "Bottom texture coordinate."
        ]
        retNone
        "Narrow a sprite to a sub-rect of its texture (#1259). Any missing argument leaves the sprite untouched; use UI.setSpriteFrame for a live animation frame.")
        (uiSetSpriteUVFn env)
    , registerLuaVerb (luaVerb "setSpriteFrame"
        [ elementArg
        , argReq "textureHandle" TInteger "Atlas texture handle for this frame."
        , argReq "u0" TNumber "Left texture coordinate."
        , argReq "v0" TNumber "Top texture coordinate."
        , argReq "u1" TNumber "Right texture coordinate."
        , argReq "v1" TNumber "Bottom texture coordinate."
        , flagArg "flipX" "Whether the frame is drawn horizontally mirrored."
        ]
        retNone
        "Publish texture, sub-rect and mirror as one manager transition (#1259) so the render thread never sees half a frame. Any missing required argument leaves the sprite untouched.")
        (uiSetSpriteFrameFn env)
    , registerLuaVerb (luaVerb "setSpriteFlipX"
        [ elementArg
        , flagArg "flipX" "Whether the sprite is drawn horizontally mirrored."
        ]
        retNone
        "Mirror a sprite horizontally (#887). Visual only: geometry and interactive bounds are untouched.")
        (uiSetSpriteFlipXFn env)
    , registerLuaVerb (luaVerb "setOnClick"
        [ elementArg
        , argReq "callbackName" TString "Name of the Lua callback to invoke on a left click."
        ]
        retNone
        "Register an element's left-click callback name.")
        (uiSetOnClickFn env)
    , registerLuaVerb (luaVerb "setOnRightClick"
        [ elementArg
        , argReq "callbackName" TString "Name of the Lua callback to invoke on a right click."
        ]
        retNone
        "Register an element's right-click callback name.")
        (uiSetOnRightClickFn env)
    , registerLuaVerb (luaVerb "removeFromPage"
        [ pageArg
        , elementArg
        ]
        retNone
        "Detach an element from a page, reporting any resulting control-focus change.")
        (uiRemoveFromPageFn env)

    -- Box textures
    , registerLuaVerb (luaVerb "setBoxTextures"
        [ elementArg
        , argReq "boxTextureHandle" TInteger "Handle from UI.loadBoxTextures."
        ]
        retNone
        "Rebind a box element's nine-slice texture set.")
        (uiSetBoxTexturesFn env)
    , registerLuaVerb (luaVerb "loadBoxTextures"
        [ argReq "texCenter" TInteger "Center fill texture handle."
        , argReq "texN" TInteger "North edge texture handle."
        , argReq "texS" TInteger "South edge texture handle."
        , argReq "texE" TInteger "East edge texture handle."
        , argReq "texW" TInteger "West edge texture handle."
        , argReq "texNE" TInteger "North-east corner texture handle."
        , argReq "texNW" TInteger "North-west corner texture handle."
        , argReq "texSE" TInteger "South-east corner texture handle."
        , argReq "texSW" TInteger "South-west corner texture handle."
        ]
        (retVals [resVal "boxTextureHandle" (TNullable TInteger)
            "The registered nine-slice set's handle, or nil when any argument is missing."])
        "Register a nine-slice texture set for use by UI.newBox and UI.setBoxTextures.")
        (uiLoadBoxTexturesFn env)

    -- Tooltips
    , registerLuaVerb (luaVerb "setTooltip"
        [ elementArg
        , argReq "text" TString "Tooltip body text."
        ]
        retNone
        "Attach a plain-text tooltip to an element.")
        (uiSetTooltipFn env)
    , registerLuaVerb (luaVerb "setTooltipRich"
        [ elementArg
        , argReq "content" tooltipContentRecord
            "Tooltip content table. A non-table makes the call a no-op; every field inside is optional."
        ]
        retNone
        "Attach a tooltip with optional body text, a hint line, a width cap and a sprite row.")
        (uiSetTooltipRichFn env)
    , registerLuaVerb (luaVerb "clearTooltip"
        [elementArg]
        retNone
        "Remove any tooltip attached to an element.")
        (uiClearTooltipFn env)
    , registerLuaVerb (luaVerb "setTooltipStyle"
        [ argReq "style" tooltipStyleRecord
            "Style table. A non-table makes the call a no-op; every field inside is optional and an omitted one keeps its previous value."
        ]
        retNone
        "Configure the global tooltip look: fonts, box textures, colors, padding, delays and offsets.")
        (uiSetTooltipStyleFn env)
    , registerLuaVerb (luaVerb "lockTooltip"
        []
        retNone
        "Freeze the currently-shown tooltip in place. A no-op when no tooltip is visible.")
        (uiLockTooltipFn env)
    , registerLuaVerb (luaVerb "unlockTooltip"
        []
        retNone
        "Release the tooltip lock and hide the tooltip.")
        (uiUnlockTooltipFn env)
    , registerLuaVerb (luaVerb "toggleTooltipLock"
        []
        retNone
        "Lock the tooltip when one is showing and unlocked; otherwise unlock and hide.")
        (uiToggleTooltipLockFn env)
    , registerLuaVerb (luaVerb "isTooltipLocked"
        []
        (retVals [resVal "locked" TBoolean "Whether a tooltip is currently locked in place."])
        "Whether the tooltip is locked.")
        (uiIsTooltipLockedFn env)

    -- #2056: the presentation boundary. Two verbs, no more — see
    -- Engine.Scripting.Lua.API.UI.Presentation.
    , registerLuaVerb (luaVerb "armPresentation"
        []
        (retVals [resVal "token" TInteger
            "A monotonic token standing for everything written to the UI manager so far. Never nil."])
        "Mint a presentation token. Arm it only once the page carrying the content is showing, and re-arm on every change.")
        (uiArmPresentationFn env)
    , registerLuaVerb (luaVerb "isPresented"
        [ argReq "token" TInteger "A token from UI.armPresentation. Type-checked first, so a numeric string is NOT accepted here (#1497); a missing, non-numeric or zero token is false."
        ]
        (retVals [resVal "presented" TBoolean
            "True once a renderer snapshot taken at or after the token was armed has been rendered to completion. Always false under GPU-less --headless."])
        "Whether the content a token stands for has actually reached the screen (#2056).")
        (uiIsPresentedFn env)

    -- #747: the shared floating-placement contract
    , registerLuaVerb (luaVerb "placePopup"
        [ argReq "anchorX" TNumber "Anchor rect left edge, in framebuffer pixels."
        , argReq "anchorY" TNumber "Anchor rect top edge, in framebuffer pixels."
        , argReq "anchorW" TNumber "Anchor rect width, in framebuffer pixels."
        , argReq "anchorH" TNumber "Anchor rect height, in framebuffer pixels."
        , argReq "contentW" TNumber "Popup content width, in framebuffer pixels."
        , argReq "contentH" TNumber "Popup content height, in framebuffer pixels."
        , argOpt "direction" TString "Preferred direction: below, above, right or left. Omitted, nil or unrecognized means anchored placement, which clamps at the anchor without flipping."
        ]
        (retVals
            [ resVal "x" TNumber "Placed left edge, in framebuffer pixels."
            , resVal "y" TNumber "Placed top edge, in framebuffer pixels."
            , resVal "flipped" TBoolean "Whether the preferred direction was flipped to fit the framebuffer."
            ])
        "Place a floating popup against an anchor, clamped to the current framebuffer. Always pushes three bare values, never a table; a missing required argument yields 0, 0, false.")
        (uiPlacePopupFn env)
    , registerLuaVerb (luaVerb "fitVisibleRows"
        [ argReq "preferredCount" TInteger "How many rows the caller would like to show."
        , argReq "rowHeight" TNumber "Row height, in framebuffer pixels."
        , argReq "availableHeight" TNumber "Space available for rows, in framebuffer pixels."
        ]
        (retVals [resVal "count" TInteger
            "How many rows actually fit; 0 when any argument is missing."])
        "How many rows of a given height fit in the available space.")
        (uiFitVisibleRowsFn env)
    ]
  Lua.setglobal (Lua.Name "UI")
  pure descriptors

-- | The @UI.setTooltipRich@ content table. Every field is optional;
--   a sprite carrying both @texture@ and @frames@ uses @frames@.
tooltipContentRecord ∷ LuaType
tooltipContentRecord = TRecord
    [ recField "text" (TNullable TString) "Tooltip body text; omitted means no body line."
    , recField "hint" (TNullable TString) "Secondary hint line, shown after its own delay; omitted means none."
    , recField "maxWidth" (TNullable TNumber) "Width cap in framebuffer pixels; omitted means uncapped."
    , recField "sprites" (TNullable (TArray tooltipSpriteRecord))
        "Sprite row; omitted or a non-table means no sprites. A sprite entry resolving to no frames is dropped."
    ]

-- | One entry of @UI.setTooltipRich@'s @sprites@ array.
tooltipSpriteRecord ∷ LuaType
tooltipSpriteRecord = TRecord
    [ recField "texture" (TNullable TInteger) "Static texture handle; ignored when frames is present and non-empty."
    , recField "frames" (TNullable (TArray TInteger)) "Animation frame texture handles, in order."
    , recField "frameMs" (TNullable TInteger) "Milliseconds per frame; omitted means 100, and the value is floored at 1."
    , recField "w" (TNullable TNumber) "Draw width in framebuffer pixels; omitted means 32."
    , recField "h" (TNullable TNumber) "Draw height in framebuffer pixels; omitted means 32."
    ]

-- | The @UI.setTooltipStyle@ table. Every field is optional and an
--   omitted one keeps its previous value. A color is a four-element
--   @{r, g, b, a}@ array; a missing component reads as 1.
tooltipStyleRecord ∷ LuaType
tooltipStyleRecord = TRecord
    [ recField "font" (TNullable TInteger) "Body font handle."
    , recField "fontSize" (TNullable TNumber) "Body font size, in framebuffer pixels."
    , recField "padding" (TNullable TNumber) "Padding inside the tooltip box, in framebuffer pixels."
    , recField "boxTextures" (TNullable TInteger) "Nine-slice handle from UI.loadBoxTextures for the tooltip box."
    , recField "boxTileSize" (TNullable TNumber) "Nine-slice tile size, in framebuffer pixels."
    , recField "mouseOffsetX" (TNullable TNumber) "Horizontal offset from the cursor, in framebuffer pixels."
    , recField "mouseOffsetY" (TNullable TNumber) "Vertical offset from the cursor, in framebuffer pixels."
    , recField "dwellMs" (TNullable TNumber) "Hover dwell before the tooltip appears, in milliseconds."
    , recField "hintDelayMs" (TNullable TNumber) "Extra delay before the hint line appears, in milliseconds."
    , recField "spriteGap" (TNullable TNumber) "Gap between sprites in the sprite row, in framebuffer pixels."
    , recField "textColor" (TNullable colorArray) "Body text color."
    , recField "bgColor" (TNullable colorArray) "Tooltip box background color."
    , recField "hintFontSize" (TNullable TNumber) "Hint line font size, in framebuffer pixels."
    , recField "hintColor" (TNullable colorArray) "Hint line color."
    , recField "separatorColor" (TNullable colorArray) "Separator rule color."
    , recField "separatorThickness" (TNullable TNumber) "Separator rule thickness, in framebuffer pixels."
    , recField "separatorTexture" (TNullable TInteger) "Separator rule texture handle."
    ]

-- | A @{r, g, b, a}@ color array as the tooltip style reads it: a
--   four-element sequence whose missing components read as 1.
colorArray ∷ LuaType
colorArray = TArray TNumber
