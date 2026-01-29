module Engine.Scripting.Lua.Util
  ( isValidRef
  , broadcastToModules
  ) where

import UPrelude
import Engine.Scripting.Types (ScriptValue)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaScript(..))
import Engine.Scripting.Lua.Script (callModuleFunction)
import Engine.Graphics.Font.Data (FontAtlas(..))
import Engine.Asset.Types (GlyphInfo(..))
import Engine.Input.Types (Key(..))
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_, when)

-- | Check if a Lua reference is valid (not nil)
isValidRef ∷ Lua.Reference → Bool
isValidRef (Lua.Reference n) = n ≠ fromIntegral Lua.refnil

-- | Broadcast an event to all loaded script modules
broadcastToModules ∷ LuaBackendState → T.Text → [ScriptValue] → IO ()
broadcastToModules ls funcName args = do
    scriptsMap ← readTVarIO (lbsScripts ls)
    forM_ (Map.elems scriptsMap) $ \script →
        when (isValidRef (scriptModuleRef script)) $ do
            _ ← callModuleFunction (lbsLuaState ls) (scriptModuleRef script) funcName args
            return ()
