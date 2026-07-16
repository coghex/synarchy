{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for the shared #747 floating-placement contract
--   ("UI.PopupPlacement") — one framebuffer-coordinate placement
--   algorithm for dropdown option lists and context menu roots/
--   submenus, replacing the two divergent implementations
--   (@scripts/ui/dropdown.lua@'s unconditional below-placement and
--   @scripts/ui/context_menu.lua@'s independent framebuffer clamp).
module Engine.Scripting.Lua.API.UI.Placement
  ( uiPlacePopupFn
  , uiFitVisibleRowsFn
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import UI.PopupPlacement

-- | Parse the direction string argument; an unrecognized string
--   degrades to 'PopupAnchored' (place at the anchor, clamp only —
--   never worse than ignoring the preference entirely).
parseDirection ∷ Text → PopupDirection
parseDirection "below"    = PopupBelow
parseDirection "above"    = PopupAbove
parseDirection "right"    = PopupRight
parseDirection "left"     = PopupLeft
parseDirection _          = PopupAnchored

-- | UI.placePopup(anchorX, anchorY, anchorW, anchorH, contentW,
--   contentH, direction) -> x, y, flipped
--
--   @direction@ is one of "below"/"above"/"right"/"left"/"anchored".
--   Reads the current framebuffer size directly (the same value
--   @engine.getFramebufferSize()@ reports) so callers don't have to
--   round-trip it themselves.
uiPlacePopupFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiPlacePopupFn env = do
    axArg   ← Lua.tonumber 1
    ayArg   ← Lua.tonumber 2
    awArg   ← Lua.tonumber 3
    ahArg   ← Lua.tonumber 4
    cwArg   ← Lua.tonumber 5
    chArg   ← Lua.tonumber 6
    dirArg  ← Lua.tostring 7

    case (axArg, ayArg, awArg, ahArg, cwArg, chArg) of
        (Just ax, Just ay, Just aw, Just ah, Just cw, Just ch) → do
            (fbW, fbH) ← Lua.liftIO $ readIORef (framebufferSizeRef env)
            let direction = maybe PopupAnchored (parseDirection ∘ TE.decodeUtf8Lenient) dirArg
                req = PlacementRequest
                    { prAnchorX      = realToFrac ax
                    , prAnchorY      = realToFrac ay
                    , prAnchorW      = realToFrac aw
                    , prAnchorH      = realToFrac ah
                    , prContentW     = realToFrac cw
                    , prContentH     = realToFrac ch
                    , prPreferred    = direction
                    , prFramebufferW = fromIntegral fbW
                    , prFramebufferH = fromIntegral fbH
                    }
                placement = placePopup req
            Lua.pushnumber (realToFrac (plX placement))
            Lua.pushnumber (realToFrac (plY placement))
            Lua.pushboolean (plFlipped placement)
            return 3
        _ → do
            Lua.pushnumber 0
            Lua.pushnumber 0
            Lua.pushboolean False
            return 3

-- | UI.fitVisibleRows(preferredCount, rowHeight, availableHeight) ->
--   count — how many rows actually fit; see 'fitVisibleRows'.
uiFitVisibleRowsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiFitVisibleRowsFn _env = do
    preferredArg ← Lua.tointeger 1
    rowHArg      ← Lua.tonumber 2
    availArg     ← Lua.tonumber 3
    case (preferredArg, rowHArg, availArg) of
        (Just preferred, Just rowH, Just avail) → do
            let count = fitVisibleRows (fromIntegral preferred) (realToFrac rowH) (realToFrac avail)
            Lua.pushinteger (fromIntegral count)
        _ → Lua.pushinteger 0
    return 1
