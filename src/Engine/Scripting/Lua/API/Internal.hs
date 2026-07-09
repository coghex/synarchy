module Engine.Scripting.Lua.API.Internal
  ( registerLuaFunction
  ) where

import UPrelude
import qualified Control.Monad.Catch as Catch
import Control.Exception (SomeException, SomeAsyncException
                         , fromException, displayException)
import qualified HsLua as Lua
import qualified Data.ByteString.Char8 as BS

-- | Register a Haskell function in the table on top of the stack.
--
--   The action is guarded so that any non-Lua Haskell exception
--   thrown inside an API function (partial @VU.!@/@V.!@, 'error',
--   arithmetic) is converted into a regular Lua error instead of
--   escaping. hslua's own wrapper ('pushHaskellFunction' →
--   @exceptionToError@) only converts exceptions of the LuaError
--   type — anything else unwinds through @lua_pcall@'s C frames as
--   a Haskell exception, reaches the Lua thread's top-level handler,
--   and tears the whole engine down. With the guard, the error is
--   raised as a Lua error, caught by the 'Lua.pcall' isolation in
--   "Engine.Scripting.Lua.Script", logged, and the callback skipped.
--
--   'Lua.Exception's are re-thrown for hslua's own (richer)
--   conversion; async exceptions are re-thrown so shutdown's
--   killThread still works.
registerLuaFunction ∷ BS.ByteString → Lua.LuaE Lua.Exception Lua.NumResults
                    → Lua.LuaE Lua.Exception ()
registerLuaFunction name action = do
    Lua.pushHaskellFunction (action `Catch.catch` handler)
    Lua.setfield (-2) (Lua.Name name)
  where
    handler ∷ SomeException → Lua.LuaE Lua.Exception Lua.NumResults
    handler e
        | Just (le ∷ Lua.Exception) ← fromException e = Catch.throwM le
        | Just (ae ∷ SomeAsyncException) ← fromException e = Catch.throwM ae
        | otherwise = do
            Lua.pushstring $ "Haskell exception in " <> name <> ": "
                <> BS.pack (displayException e)
            Lua.error
