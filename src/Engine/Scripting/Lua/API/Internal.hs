module Engine.Scripting.Lua.API.Internal
  ( registerLuaFunction
  , registerLuaVerb
  , guardLuaAction
  ) where

import UPrelude
import Engine.Scripting.Lua.CallStats (LuaCallStats, withLuaCallStats)
import qualified Control.Monad.Catch as Catch
import Control.Exception (SomeException, SomeAsyncException
                         , fromException, displayException)
import qualified HsLua as Lua
import qualified Data.ByteString.Char8 as BS
import Engine.Scripting.Lua.API.Descriptor (LuaVerb(..))

-- | Wrap one API action in the boundary's exception guard.
--
--   Any non-Lua Haskell exception thrown inside an API function
--   (partial @VU.!@/@V.!@, 'error', arithmetic) is converted into a
--   regular Lua error instead of escaping. hslua's own wrapper
--   ('Lua.pushHaskellFunction' → @exceptionToError@) only converts
--   exceptions of the LuaError type — anything else unwinds through
--   @lua_pcall@'s C frames as a Haskell exception, reaches the Lua
--   thread's top-level handler, and tears the whole engine down. With
--   the guard, the error is raised as a Lua error, caught by the
--   'Lua.pcall' isolation in "Engine.Scripting.Lua.Script", logged, and
--   the callback skipped.
--
--   'Lua.Exception's are re-thrown for hslua's own (richer)
--   conversion; async exceptions are re-thrown so shutdown's
--   killThread still works.
--
--   It is a named top-level function rather than a @where@ clause so
--   that BOTH registrars provably share one guard (#2479) — and so a
--   test can establish those three guarantees directly, without
--   unwinding a Haskell exception through hslua's C frames to observe
--   the asynchronous case.
guardLuaAction ∷ BS.ByteString → Lua.LuaE Lua.Exception Lua.NumResults
               → Lua.LuaE Lua.Exception Lua.NumResults
guardLuaAction name action = action `Catch.catch` handler
  where
    handler ∷ SomeException → Lua.LuaE Lua.Exception Lua.NumResults
    handler e
        | Just (le ∷ Lua.Exception) ← fromException e = Catch.throwM le
        | Just (ae ∷ SomeAsyncException) ← fromException e = Catch.throwM ae
        | otherwise = do
            Lua.pushstring $ "Haskell exception in " <> name <> ": "
                <> BS.pack (displayException e)
            Lua.error

-- | Register a Haskell function in the table on top of the stack,
--   behind 'guardLuaAction'.
registerLuaFunction ∷ LuaCallStats → BS.ByteString → BS.ByteString → Lua.LuaE Lua.Exception Lua.NumResults
                    → Lua.LuaE Lua.Exception ()
registerLuaFunction callStats namespace name action = do
    Lua.pushHaskellFunction (guardLuaAction name
        (withLuaCallStats callStats (namespace <> "." <> name) action))
    Lua.setfield (-2) (Lua.Name name)

-- | Register a Haskell function under its DESCRIPTOR's name (#2479,
--   epic #1995 decision D-5) — the second registrar, alongside an
--   shared 'registerLuaFunction' rather than in place of it, so the
--   26 namespaces this pilot does not convert keep their registrations
--   exactly as they are.
--
--   Installation is 'registerLuaFunction' itself, so the two registrars
--   cannot drift: same stack discipline, same 'guardLuaAction'. The
--   descriptor is metadata carried beside the action and is never
--   consulted at call time — no argument count or kind is enforced from
--   it, and a verb accepts exactly what it accepted before.
--
--   It yields the descriptor it installed so a registrar can publish
--   its own manifest from the very expressions that did the installing,
--   rather than from a second list that could disagree with them.
registerLuaVerb ∷ LuaCallStats → BS.ByteString → LuaVerb → Lua.LuaE Lua.Exception Lua.NumResults
                → Lua.LuaE Lua.Exception LuaVerb
registerLuaVerb callStats namespace verb action =
    verb <$ registerLuaFunction callStats namespace (verbName verb) action
