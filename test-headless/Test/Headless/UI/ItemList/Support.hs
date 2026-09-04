-- | Shared bare-Lua-backend plumbing for the Item list widget gate
--   (#1088, epic #1013 phase C0; split out under #2147).
--
--   Same technique as 'Test.Headless.UI.InputOwnership' and
--   'Test.Headless.UI.TransferContextMenu': a real Lua backend with the
--   full Lua API registered, so @scripts.ui.item_list@ and
--   @scripts.ui.tabbar@ are the real production modules any caller's
--   @require@ would get, driven against real UI elements on a real page
--   -- with no world, units or buildings.
--
--   A leaf: it imports no behavior owner, so the dependency runs one
--   way, facade -> owners -> here. It holds only what more than one
--   owner needs; a result record belongs to the single owner that
--   decodes it, never here.
module Test.Headless.UI.ItemList.Support
    ( newBareLuaBackend
    , evalDebug
    , run
    , luaLines
    , decodeOr
    , setupLua
    ) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON, decode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

run ∷ LuaBackendState → Text → IO ()
run ls stmt = do
    r ← evalDebug ls stmt
    r `shouldNotSatisfy` isLuaError

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

decodeOr ∷ FromJSON α ⇒ Text → IO α
decodeOr r = case decode (BL.fromStrict (TE.encodeUtf8 r)) of
    Nothing → do
        expectationFailure ("failed to decode: " ⧺ T.unpack r)
        error "unreachable"
    Just v → pure v

-- | Shared per-case setup: the widget's own texture load needs a
-- resident white pixel, and every rendering case wants a predictable
-- text metric (the synthetic boot reports 0 for every string).
setupLua ∷ Text
setupLua = T.concat
    [ "local il = require('scripts.ui.item_list'); il.init(); "
    , "require('scripts.ui.tabbar').init(); "
    , "engine.getTextWidth = function(_, s, px) return #s * (px or 10) end; "
    -- Minimal host policy every rendering case reuses. Bounds are big
    -- enough that nothing is clipped out by the row cap.
    , "function baseParams(pg, items) return { "
    , "  name = 'probe_list', page = pg, font = 1, "
    , "  x = 0, y = 0, width = 400, height = 4000, "
    , "  items = items, uiscale = 1.0, maxRows = 300, "
    , "  rowHeight = 32, rowPad = 2, iconSize = 28, textPad = 12, "
    , "  rowFontSize = 13, zBase = 10, "
    , "  rowWeightText = function(g) "
    , "    return string.format('%.2f kg', (g.weight or 0) * (g.count or 1)) end, "
    , "} end; "
    ]
