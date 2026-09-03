{-# LANGUAGE Strict #-}
-- | Lua bindings for the keybinding system. Exposes 'keyBindingsRef' to
--   Lua so the Input settings UI can read and edit bindings.
--
--   The action → @[key]@ model lives in "Engine.Input.Bindings"; this
--   module is a thin Lua façade over it. Key names are validated through
--   'parseKeyName' (so side-specific modifier names like @LeftShift@ are
--   accepted, not just the merged @Shift@), and @Escape@ / @Grave@ are
--   refused — they are engine-hardcoded (escape cascade, shell toggle)
--   and must never be reassigned.
module Engine.Scripting.Lua.API.Keybinds
  ( getKeybindsFn
  , setActionKeysFn
  , addActionKeyFn
  , removeActionKeyFn
  , removeActionKeysMatchingFn
  , saveKeybindsFn
  , loadDefaultKeybindsFn
  , keyMatchesActionFn
  , getCurrentKeyNameFn
  ) where

import UPrelude
import Engine.Scripting.Lua.Util (isDenseArray)
import qualified HsLua as Lua
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
-- #892 (E4): this module runs on the @LuaThread@ and is the other of
-- the two production holders of the FULL 'InputCapability' — it is the
-- sole non-boot owner of the Lua-private @onKeyDown@ current-key
-- handoff ('icCurrentKeyDownRef'). The logger rides `core-init` (#889).
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.Input (InputCapability(..), toInputCapability)
import Engine.Input.Bindings
  ( KeyBindings, loadKeyBindings, saveKeyBindings, keyMatchesAction
  , reservedActions, parseKeyName, glfwKeyName, isReservedKeyName )

-- | A key name is bindable when it resolves to at least one real GLFW key
--   AND does not resolve to a reserved one (Escape / Grave) under any
--   spelling. Validating through 'parseKeyName' (not the merged
--   'keyToText' vocabulary) means the side-specific modifier names the
--   editor now captures — "LeftShift", "RightCtrl", … — are accepted, not
--   just the merged "Shift"/"Ctrl".
isBindableKey ∷ Text → Bool
isBindableKey k = not (isReservedKeyName k) ∧ not (null (parseKeyName k))

-- 'reservedActions' (the actions the engine handles outside the binding
-- table) is defined in "Engine.Input.Bindings" so the loader and the edit
-- API share one source of truth.
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
--   argument is not a table, is not a pure sequence (any non-array /
--   associative key), or has any non-string element — so a partial or
--   malformed list never produces a partial update. An empty table yields
--   @Just []@ (a valid unbind).
--
--   The pure-sequence check compares the dense length ('rawlen') against
--   the total entry count: any extra associative key (e.g. @{ "W", x=1 }@)
--   or a stray non-array key (e.g. @{ foo = "W" }@) makes the counts
--   differ and is rejected. The element loop then rejects holes (a nil in
--   the @1..n@ range) and any non-string value.
readKeyList ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe [Text])
readKeyList idx = do
    isT ← Lua.istable idx
    if not isT
      then return Nothing
      else do
        n     ← Lua.rawlen idx
        dense ← isDenseArray idx
        if not dense
          then return Nothing   -- sparse / associative / mixed → reject
          else do
            let go i acc
                  | i > fromIntegral n = return (Just (reverse acc))
                  | otherwise = do
                      _ ← Lua.rawgeti idx i
                      ty ← Lua.ltype (-1)
                      ms ← if ty ≡ Lua.TypeString then Lua.tostring (-1)
                                                  else return Nothing
                      Lua.pop 1
                      case ms of
                          Just bs → go (i + 1) (TE.decodeUtf8Lenient bs : acc)
                          Nothing → return Nothing
            go 1 []

-- | engine.getKeybinds() → { action = { key1, key2, ... }, ... }
getKeybindsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getKeybindsFn env = do
    bindings ← Lua.liftIO $ readIORef (icKeyBindingsRef (toInputCapability env))
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
          | isEditableAction (TE.decodeUtf8Lenient actionBS)
          , all isBindableKey keys → do
            let action = TE.decodeUtf8Lenient actionBS
            Lua.liftIO $ atomicModifyIORef' (icKeyBindingsRef (toInputCapability env)) $ \b →
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
          | isEditableAction (TE.decodeUtf8Lenient actionBS)
          , isBindableKey (TE.decodeUtf8Lenient keyBS) → do
            let action = TE.decodeUtf8Lenient actionBS
                key    = TE.decodeUtf8Lenient keyBS
            Lua.liftIO $ atomicModifyIORef' (icKeyBindingsRef (toInputCapability env)) $ \b →
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
          | isEditableAction (TE.decodeUtf8Lenient actionBS) → do
            let action = TE.decodeUtf8Lenient actionBS
                key    = TE.decodeUtf8Lenient keyBS
            changed ← Lua.liftIO $ atomicModifyIORef' (icKeyBindingsRef (toInputCapability env)) $ \b →
                case Map.lookup action b of
                    Just cur | key `elem` cur →
                        (Map.insert action (filter (≢ key) cur) b, True)
                    _ → (b, False)
            Lua.pushboolean changed
        _ → Lua.pushboolean False
    return 1

-- | engine.removeActionKeysMatching(action, key) → bool
--   Subtract the given key's GLFW key set from each of the action's stored
--   keys. A stored key fully covered by the removal is dropped; a merged
--   key only partially covered is replaced by the name(s) for the side(s)
--   that remain; a non-overlapping key is kept verbatim. So removing
--   @RightShift@ from an action bound to the merged @Shift@ leaves
--   @LeftShift@ — the user reassigned one physical key, not both. This is
--   the *semantic* sibling of 'removeActionKeyFn' (which removes one exact
--   name), used by the editor's "Remap". Returns true if anything changed;
--   reserved actions and an unparseable key (empty removal set) are no-ops.
removeActionKeysMatchingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
removeActionKeysMatchingFn env = do
    actionArg ← Lua.tostring 1
    keyArg ← Lua.tostring 2
    case (actionArg, keyArg) of
        (Just actionBS, Just keyBS)
          | isEditableAction (TE.decodeUtf8Lenient actionBS) → do
            let action = TE.decodeUtf8Lenient actionBS
                remove = parseKeyName (TE.decodeUtf8Lenient keyBS)
                -- Rewrite one stored key into the name(s) that survive the
                -- subtraction (empty = dropped; unchanged length = kept as
                -- its original name; otherwise re-named from the remainder).
                rewrite sk =
                  let cur       = parseKeyName sk
                      remaining = filter (`notElem` remove) cur
                  in if null remaining                 then []
                     else if length remaining ≡ length cur then [sk]
                     else map glfwKeyName remaining
            changed ← Lua.liftIO $ atomicModifyIORef' (icKeyBindingsRef (toInputCapability env)) $ \b →
                case Map.lookup action b of
                    Just curList →
                        let newList = concatMap rewrite curList
                        in if newList ≡ curList
                           then (b, False)
                           else (Map.insert action newList b, True)
                    Nothing → (b, False)
            Lua.pushboolean changed
        _ → Lua.pushboolean False
    return 1

-- | engine.saveKeybinds() → bool
--   Persist the live bindings to config/keybinds.local.yaml. @true@ when
--   the file was durably replaced, @false@ when the write failed —
--   'Engine.Input.Bindings.saveKeyBindings' logs the path and the cause
--   at warning level, and the live bindings stay applied for the session
--   either way (its haddock states that policy).
--
--   Before #2202 this returned @true@ unconditionally and let a
--   filesystem failure escape as a Lua error, which the settings screen
--   reported as nothing at all.
saveKeybindsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveKeybindsFn env = do
    written ← Lua.liftIO $ do
        bindings ← readIORef (icKeyBindingsRef (toInputCapability env))
        logger ← readIORef (ccLoggerRef (toCoreCapability env))
        saveKeyBindings logger "config/keybinds.local.yaml" bindings
    Lua.pushboolean (either (const False) (const True) written)
    return 1

-- | engine.loadDefaultKeybinds() → { action = { keys } }
--   Load config/keybinds_default.yaml into the live ref (for a Reset
--   action) and return the resulting table.
loadDefaultKeybindsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadDefaultKeybindsFn env = do
    bindings ← Lua.liftIO $ do
        logger ← readIORef (ccLoggerRef (toCoreCapability env))
        defaults ← loadKeyBindings logger "config/keybinds_default.yaml"
        writeIORef (icKeyBindingsRef (toInputCapability env)) defaults
        return defaults
    pushBindings bindings
    return 1

-- | engine.keyMatchesAction(key, action) → bool
--   True when the key that triggered the current onKeyDown event is bound to
--   the action. Inside an onKeyDown dispatch it matches the exact physical
--   key the engine recorded (currentKeyDownRef), so it resolves which side
--   of a merged modifier was pressed with no race and no dropped tap. The
--   `key` argument is the merged name the handler received; it is used only
--   as a fallback if the function is somehow called outside a key-down
--   dispatch (no recorded key), where each GLFW key the name covers is
--   tested.
-- | engine.getCurrentKeyName() → string | nil
--   The canonical name of the exact physical key currently being
--   dispatched to @onKeyDown@ (read from 'currentKeyDownRef'), preserving
--   the side of a merged modifier — i.e. @LeftShift@ rather than the
--   merged @Shift@ that the @onKeyDown@ string collapses to. Returns nil
--   outside a key-down dispatch. The keybind editor calls this during
--   capture so the key it binds round-trips to the exact key pressed.
getCurrentKeyNameFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getCurrentKeyNameFn env = do
    mKey ← Lua.liftIO $ readIORef (icCurrentKeyDownRef (toInputCapability env))
    case mKey of
        Just g  → Lua.pushstring (TE.encodeUtf8 (glfwKeyName g))
        Nothing → Lua.pushnil
    return 1

keyMatchesActionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
keyMatchesActionFn env = do
    keyArg ← Lua.tostring 1
    actionArg ← Lua.tostring 2
    case (keyArg, actionArg) of
        (Just keyBS, Just actionBS) → do
            let name   = TE.decodeUtf8Lenient keyBS
                action = TE.decodeUtf8Lenient actionBS
            matches ← Lua.liftIO $ do
                bindings ← readIORef (icKeyBindingsRef (toInputCapability env))
                mKey     ← readIORef (icCurrentKeyDownRef (toInputCapability env))
                return $ case mKey of
                    Just g  → keyMatchesAction g action bindings
                    Nothing → any (\g → keyMatchesAction g action bindings)
                                  (parseKeyName name)
            Lua.pushboolean matches
        _ → Lua.pushboolean False
    return 1
