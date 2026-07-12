module Engine.Scripting.Lua.API.Register.UI
  ( registerUIAPI
  ) where

import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.UI
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @UI@ global table.
registerUIAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerUIAPI env = do
  Lua.newtable
  registerLuaFunction "newPage"    (uiNewPageFn env)
  registerLuaFunction "deletePage" (uiDeletePageFn env)
  registerLuaFunction "showPage"   (uiShowPageFn env)
  registerLuaFunction "hidePage"   (uiHidePageFn env)
  registerLuaFunction "setPageInputExclusive" (uiSetPageInputExclusiveFn env)
  registerLuaFunction "isPageInputExclusive"  (uiIsPageInputExclusiveFn env)
  registerLuaFunction "isInputBlocked"        (uiIsInputBlockedFn env)
  registerLuaFunction "isPageInScope"         (uiIsPageInScopeFn env)

  registerLuaFunction "newElement" (uiNewElementFn env)
  registerLuaFunction "newBox"     (uiNewBoxFn env)
  registerLuaFunction "newText"    (uiNewTextFn env)
  registerLuaFunction "newSprite"  (uiNewSpriteFn env)

  registerLuaFunction "addToPage"      (uiAddToPageFn env)
  registerLuaFunction "addChild"       (uiAddChildFn env)
  registerLuaFunction "removeElement"  (uiRemoveElementFn env)
  registerLuaFunction "deleteElement"  (uiDeleteElementFn env)
  registerLuaFunction "findElementAt" (uiFindElementAtFn env)
  registerLuaFunction "getElementOnClick" (uiGetElementOnClickFn env)
  registerLuaFunction "findHoverTarget" (uiFindHoverTargetFn env)

  registerLuaFunction "enableTextInput" (uiEnableTextInputFn env)
  registerLuaFunction "getTextInput" (uiGetTextFn env)
  registerLuaFunction "setTextInput" (uiSetTextInputFn env)
  registerLuaFunction "getCursor" (uiGetCursorFn env)
  registerLuaFunction "setCursor" (uiSetCursorFn env)
  registerLuaFunction "insertChar" (uiInsertCharFn env)
  registerLuaFunction "deleteBackward" (uiDeleteBackwardFn env)
  registerLuaFunction "deleteForward" (uiDeleteForwardFn env)
  registerLuaFunction "cursorLeft" (uiCursorLeftFn env)
  registerLuaFunction "cursorRight" (uiCursorRightFn env)
  registerLuaFunction "cursorHome" (uiCursorHomeFn env)
  registerLuaFunction "cursorEnd" (uiCursorEndFn env)

  registerLuaFunction "setFocus"       (uiSetFocusFn env)
  registerLuaFunction "clearFocus"     (uiClearFocusFn env)
  registerLuaFunction "getFocus"       (uiGetFocusFn env)
  registerLuaFunction "hasFocus"       (uiHasFocusFn env)

  registerLuaFunction "setPosition"  (uiSetPositionFn env)
  registerLuaFunction "setSize"      (uiSetSizeFn env)
  registerLuaFunction "setVisible"   (uiSetVisibleFn env)
  registerLuaFunction "isPageVisible" (uiIsPageVisibleFn env)
  registerLuaFunction "getElementInfo" (uiGetElementInfoFn env)
  registerLuaFunction "getVisibleElements" (uiGetVisibleElementsFn env)
  registerLuaFunction "setClickable" (uiSetClickableFn env)
  registerLuaFunction "setZIndex"    (uiSetZIndexFn env)
  registerLuaFunction "setColor"     (uiSetColorFn env)
  registerLuaFunction "setText"      (uiSetTextFn env)
  registerLuaFunction "setSpriteTexture" (uiSetSpriteTextureFn env)
  registerLuaFunction "setOnClick"   (uiSetOnClickFn env)
  registerLuaFunction "setOnRightClick" (uiSetOnRightClickFn env)
  registerLuaFunction "removeFromPage" (uiRemoveFromPageFn env)

  registerLuaFunction "setBoxTextures" (uiSetBoxTexturesFn env)
  registerLuaFunction "loadBoxTextures" (uiLoadBoxTexturesFn env)

  registerLuaFunction "setTooltip"        (uiSetTooltipFn env)
  registerLuaFunction "setTooltipRich"    (uiSetTooltipRichFn env)
  registerLuaFunction "clearTooltip"      (uiClearTooltipFn env)
  registerLuaFunction "setTooltipStyle"   (uiSetTooltipStyleFn env)
  registerLuaFunction "lockTooltip"       (uiLockTooltipFn env)
  registerLuaFunction "unlockTooltip"     (uiUnlockTooltipFn env)
  registerLuaFunction "toggleTooltipLock" (uiToggleTooltipLockFn env)
  registerLuaFunction "isTooltipLocked"   (uiIsTooltipLockedFn env)

  Lua.setglobal (Lua.Name "UI")
