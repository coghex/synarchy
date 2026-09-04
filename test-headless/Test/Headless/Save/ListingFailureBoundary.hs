-- | #2333: what the three PUBLIC Lua verbs built on
--   'World.Save.Serialize.listSaves' do when @saves\/@ itself cannot be
--   surveyed.
--
--   This is the boundary the defect was actually felt at.
--   'Engine.Scripting.Lua.API.Internal.registerLuaFunction' converts an
--   escaped Haskell exception into a LUA ERROR, and a Lua error is not
--   a value any caller's fallback can see:
--
--   * @scripts\/main_menu.lua@ and @scripts\/save_browser.lua@ both
--     write @engine.listSaves() or {}@. A raised error skips the @or@
--     entirely and takes the menu build down with it.
--   * @scripts\/autosave.lua@ reads @ok, reason = engine.…@ and calls
--     @reportFailure(reason)@. A raised error never reaches that line,
--     so a cycle that could not even look at @saves\/@ reported nothing
--     at all.
--
--   Containment is therefore only half the fix; the other half is that
--   the refusal arrives as a VALUE. These examples drive the real,
--   registered verbs through the debug console — the same
--   @loadstring@+@pcall@ primitive the TCP console uses — so an escaped
--   exception would show up here exactly as it shows up in the game: as
--   an @error:@ result.
--
--   The fault needs no injection: a plain FILE occupying the @saves@
--   name makes @createDirectoryIfMissing@ fail for real. It is written
--   inside this fixture's own isolated resource root, never the
--   developer's checkout.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "unsurveyable saves directory"'@.
module Test.Headless.Save.ListingFailureBoundary (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.IORef (newIORef)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectory)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import World.Save.Serialize (savesDirectory)

-- | Replace this fixture root's own @saves\/@ directory with a plain
--   file, so every path that reaches @createDirectoryIfMissing True
--   "saves"@ fails with a genuine 'IOException'.
--
--   Both preconditions are asserted: the directory really was there to
--   begin with (the isolated root creates it empty), and a file really
--   is there afterwards. Without them a fixture that quietly did
--   nothing would let every assertion below pass for the wrong reason.
occupySavesWithFile ∷ IO ()
occupySavesWithFile = do
    doesDirectoryExist savesDirectory `shouldReturn` True
    removeDirectory savesDirectory
    BS.writeFile savesDirectory "not a directory"
    doesFileExist savesDirectory `shouldReturn` True

-- | The bare Lua backend every UI gate in this suite builds: the full
--   production API registered, nothing preloaded.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | A raised Haskell exception surfaces through the console exactly the
--   way a Lua @error@ does.
isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

spec ∷ Spec
spec = around (withIsolatedResourceRoot ∘ withHeadlessEngine) $
    describe "an unsurveyable saves directory reaches Lua as a value \
             \(#2333)" $ do
        it "engine.listSaves() returns an empty table, so the \
           \`engine.listSaves() or {}` both menu scripts rely on is \
           \reachable at all" $ \env → do
            occupySavesWithFile
            ls ← newBareLuaBackend env
            out ← executeDebugLua (lbsLuaState ls) $ T.unwords
                [ "local s = engine.listSaves() or 'FELL_BACK';"
                , "if s == 'FELL_BACK' then return 'nil' end;"
                , "return type(s) .. ':' .. tostring(#s)" ]
            out `shouldNotSatisfy` isLuaError
            -- A real, empty table -- not nil, and not a raised error.
            out `shouldBe` "\"table:0\""

        it "engine.prepareAutosaveCycle() returns false plus a reason \
           \naming saves/, which is what autosave.lua's reportFailure \
           \reads" $ \env → do
            occupySavesWithFile
            ls ← newBareLuaBackend env
            out ← executeDebugLua (lbsLuaState ls) $ T.unwords
                [ "local ok, reason = engine.prepareAutosaveCycle(2);"
                , "return tostring(ok) .. '|' .. tostring(reason)" ]
            out `shouldNotSatisfy` isLuaError
            T.unpack out `shouldContain` "false|"
            T.unpack out `shouldContain` "autosave refused"
            T.unpack out `shouldContain` savesDirectory

        it "engine.finalizeAutosaveRotation() reports the same way" $
            \env → do
                occupySavesWithFile
                ls ← newBareLuaBackend env
                out ← executeDebugLua (lbsLuaState ls) $ T.unwords
                    [ "local ok, reason = engine.finalizeAutosaveRotation(2);"
                    , "return tostring(ok) .. '|' .. tostring(reason)" ]
                out `shouldNotSatisfy` isLuaError
                T.unpack out `shouldContain` "false|"
                T.unpack out `shouldContain` "autosave rotation refused"
                T.unpack out `shouldContain` savesDirectory
