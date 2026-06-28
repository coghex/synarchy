{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua bindings for the keybinding system. Exposes 'keyBindingsRef' to
--   Lua so the Input settings UI can read and edit bindings.
--
--   The action → @[key]@ model lives in "Engine.Input.Bindings"; this
--   module is a thin Lua façade over it. Key names are validated through
--   'textToKey' (the canonical vocabulary), and @Escape@ / @Grave@ are
--   refused — they are engine-hardcoded (escape cascade, shell toggle)
--   and must never be reassigned.
module Engine.Scripting.Lua.API.Keybinds
  ( getKeybindsFn
  , setActionKeysFn
  , addActionKeyFn
  , removeActionKeyFn
  , saveKeybindsFn
  , loadDefaultKeybindsFn
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Input.Bindings (KeyBindings, loadKeyBindings, saveKeyBindings)
import Engine.Input.Types (textToKey)

-- | A key name is bindable when it parses to a real key AND is not one of
--   the two engine-reserved names. Rejecting them here means no action can
--   ever capture Escape or Grave, regardless of which action is edited.
isBindableKey ∷ Text → Bool
isBindableKey k = k /= "Escape" ∧ k /= "Grave" ∧ isJust (textToKey k)

-- | Actions whose keys the engine handles outside the binding table
--   (Escape cascade, Grave shell toggle — see "Engine.Input.Thread").
--   Editing them would persist a config the runtime cannot honor, so the
--   edit functions refuse them outright.
reservedActions ∷ [Text]
reservedActions = ["escape", "openShell"]

isEditableAction ∷ Text → Bool
isEditableAction a = a `notElem` reservedActions

-- | Push a 'KeyBindings' map as a Lua table @{ action = { key1, ... } }@.
--   Leaves exactly one value (the outer table) on the stack.
pushBindings ∷ KeyBindings → Lua.LuaE Lua.Exception ()
pushBindings bindings = do
    Lua.newtable
    forM_ (Map.toList bindings) $ \(action, keys) → do
        Lua.newtable
        forM_ (zip [1..] keys) $ \(i, k) → do
            Lua.pushstring (TE.encodeUtf8 k)
            Lua.rawseti (-2) i
        Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 action))

-- | Strictly read a Lua string array at the given stack index into
--   @[Text]@. Returns 'Nothing' (caller rejects the whole call) if the
--   argument is not a table or if *any* element is not a string — so a
--   partial/garbage list never produces a partial update. An empty table
--   yields @Just []@ (a valid unbind).
readKeyList ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe [Text])
readKeyList idx = do
    isT ← Lua.istable idx
    if not isT
      then return Nothing
      else do
        n ← Lua.rawlen idx
        let go i acc
              | i > fromIntegral n = return (Just (reverse acc))
              | otherwise = do
                  _ ← Lua.rawgeti idx i
                  ty ← Lua.ltype (-1)
                  ms ← if ty ≡ Lua.TypeString then Lua.tostring (-1)
                                              else return Nothing
                  Lua.pop 1
                  case ms of
                      Just bs → go (i + 1) (TE.decodeUtf8 bs : acc)
                      Nothing → return Nothing
        go 1 []

-- | engine.getKeybinds() → { action = { key1, key2, ... }, ... }
getKeybindsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getKeybindsFn env = do
    bindings ← Lua.liftIO $ readIORef (keyBindingsRef env)
    pushBindings bindings
    return 1

-- | engine.setActionKeys(action, {keys}) → bool
--   Replace an action's key list. Rejected (no change, returns false) if
--   the action is reserved, the second arg is not a string-table, or any
--   key is invalid/reserved. An empty list is allowed (unbinds the
--   action). The action need not already exist.
setActionKeysFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setActionKeysFn env = do
    actionArg ← Lua.tostring 1
    mkeys ← readKeyList 2
    case (actionArg, mkeys) of
        (Just actionBS, Just keys)
          | isEditableAction (TE.decodeUtf8 actionBS)
          , all isBindableKey keys → do
            let action = TE.decodeUtf8 actionBS
            Lua.liftIO $ atomicModifyIORef' (keyBindingsRef env) $ \b →
                (Map.insert action keys b, ())
            Lua.pushboolean True
        _ → Lua.pushboolean False
    return 1

-- | engine.addActionKey(action, key) → bool
--   Append a key to an action (creating the action if absent). No-op if the
--   key is already bound to that action. Rejected if the action is reserved
--   or the key is invalid.
addActionKeyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
addActionKeyFn env = do
    actionArg ← Lua.tostring 1
    keyArg ← Lua.tostring 2
    case (actionArg, keyArg) of
        (Just actionBS, Just keyBS)
          | isEditableAction (TE.decodeUtf8 actionBS)
          , isBindableKey (TE.decodeUtf8 keyBS) → do
            let action = TE.decodeUtf8 actionBS
                key    = TE.decodeUtf8 keyBS
            Lua.liftIO $ atomicModifyIORef' (keyBindingsRef env) $ \b →
                let cur = Map.findWithDefault [] action b
                    new = if key `elem` cur then cur else cur ++ [key]
                in (Map.insert action new b, ())
            Lua.pushboolean True
        _ → Lua.pushboolean False
    return 1

-- | engine.removeActionKey(action, key) → bool
--   Remove a key from an action's list. Returns false if the action is
--   reserved, or if the action or key was not present (nothing changed).
--   The action entry is kept even when its list becomes empty (an action
--   with no keys is simply unbound).
removeActionKeyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
removeActionKeyFn env = do
    actionArg ← Lua.tostring 1
    keyArg ← Lua.tostring 2
    case (actionArg, keyArg) of
        (Just actionBS, Just keyBS)
          | isEditableAction (TE.decodeUtf8 actionBS) → do
            let action = TE.decodeUtf8 actionBS
                key    = TE.decodeUtf8 keyBS
            changed ← Lua.liftIO $ atomicModifyIORef' (keyBindingsRef env) $ \b →
                case Map.lookup action b of
                    Just cur | key `elem` cur →
                        (Map.insert action (filter (/= key) cur) b, True)
                    _ → (b, False)
            Lua.pushboolean changed
        _ → Lua.pushboolean False
    return 1

-- | engine.saveKeybinds() → bool
--   Persist the live bindings to config/keybinds.yaml.
saveKeybindsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveKeybindsFn env = do
    Lua.liftIO $ do
        bindings ← readIORef (keyBindingsRef env)
        logger ← readIORef (loggerRef env)
        saveKeyBindings logger "config/keybinds.yaml" bindings
    Lua.pushboolean True
    return 1

-- | engine.loadDefaultKeybinds() → { action = { keys } }
--   Load config/keybinds_default.yaml into the live ref (for a Reset
--   action) and return the resulting table.
loadDefaultKeybindsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadDefaultKeybindsFn env = do
    bindings ← Lua.liftIO $ do
        logger ← readIORef (loggerRef env)
        defaults ← loadKeyBindings logger "config/keybinds_default.yaml"
        writeIORef (keyBindingsRef env) defaults
        return defaults
    pushBindings bindings
    return 1
