{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Items
    ( loadItemYamlFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlItems
import Item.Types

-- | item.loadYaml(path) — parses a YAML file of item defs, loads each
--   item's sprite, and registers the defs into the ItemManager.
--   Returns the number of defs loaded.
loadItemYamlFn ∷ EngineEnv → LuaBackendState
               → Lua.LuaE Lua.Exception Lua.NumResults
loadItemYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadItemYaml logger filePath
                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc def → do
                    -- Load the sprite texture so it's ready for any
                    -- future inventory grid UI. Register under
                    -- "item_<defName>" so other systems can fetch it.
                    let regName = "item_" <> iydName def
                    handle ← loadAndRegister env backendState lteq
                                regName (T.unpack (iydSprite def))

                    let container = fmap
                            (\c → ItemContainer
                                { icCapacity = iycCapacity c
                                , icHolds    = iycHolds c
                                })
                            (iydContainer def)
                        food = fmap
                            (\f → ItemFood { ifNutrition = iyfNutrition f })
                            (iydFood def)
                        itemDef = ItemDef
                            { idName        = iydName def
                            , idDisplayName = if T.null (iydDisplayName def)
                                              then iydName def
                                              else iydDisplayName def
                            , idTexture     = handle
                            , idWeight      = iydWeight def
                            , idContainer   = container
                            , idFood        = food
                            }

                    atomicModifyIORef' (itemManagerRef env) $ \im →
                        (ItemManager
                            { imDefs = HM.insert (iydName def) itemDef
                                                (imDefs im) }, ())

                    return (acc + 1)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadItemYaml: loaded " <> T.pack (show total)
                    <> " item definitions from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1
