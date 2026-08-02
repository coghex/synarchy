-- | @engine.loadScript@ / @engine.killScript@ state identity (#1059):
--   both used to be handed the registrar's 'Lua.State' as an explicit
--   parameter, threaded through 'registerEngineAPI' — the one registrar
--   of thirteen that took one. Reading it back off the backend
--   ('lbsLuaState') removes that oddity, but ONLY if the state the
--   handlers end up running on is unchanged.
--
--   The tempting third option — handler-local @Lua.state@, as
--   "Engine.Scripting.Lua.Debug" uses — is NOT equivalent, and this is
--   the spec that says so: inside a 'Lua.pushHaskellFunction' callback
--   that yields the INVOKING state, which under a coroutine is the
--   coroutine's thread state rather than the canonical one. So these
--   specs drive both verbs from inside a real Lua coroutine (where the
--   three candidates genuinely differ) and read back, from the loaded
--   module's own chunk and callbacks, which state each actually ran on
--   — via @coroutine.running()@'s is-main flag in
--   scripts/lua_script_state_fixture.lua. A load that merely
--   "succeeded" would not distinguish them.
--
--   Bare Lua backend, no world, no script pre-loaded — same technique
--   as "Test.Headless.Lua.DebugQueue" and "Test.Headless.Lua.PauseGate".
module Test.Headless.Lua.ScriptState (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.STM (readTVarIO)
import Data.IORef (newIORef)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaScript(..))

-- | The fixture path, exactly as a script would name it: relative to
--   the resource root, which the test suite already runs from.
fixturePath ∷ BS.ByteString
fixturePath = "scripts/lua_script_state_fixture.lua"

-- | A bare Lua backend with the full API registered and no script
--   loaded — so the fixture below is genuinely not-yet-loaded and
--   'loadScriptFn' cannot take its dedup-by-path shortcut.
newBareBackend ∷ EngineEnv → IO LuaBackendState
newBareBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run a chunk on the backend's canonical state, surfacing the Lua
--   error text rather than a bare status.
runLua ∷ LuaBackendState → BS.ByteString → IO (Either T.Text ())
runLua ls src = Lua.runWith (lbsLuaState ls) $ do
    status ← Lua.dostring src
    case status of
        Lua.OK → pure (Right ())
        _ → do
            err ← Lua.tostring (-1)
            Lua.pop 1
            pure (Left (maybe "unknown error" TE.decodeUtf8Lenient err))

-- | Same, failing the example outright on a Lua error — every chunk
--   below is expected to run clean.
runLua_ ∷ LuaBackendState → BS.ByteString → IO ()
runLua_ ls src = runLua ls src ⌦ \case
    Right () → pure ()
    Left err → expectationFailure $ "Lua chunk failed: " ⧺ T.unpack err

-- | Read one field off a global table, with the caller deciding how to
--   convert whatever type it holds.
withTableField ∷ LuaBackendState → BS.ByteString → BS.ByteString
               → (Lua.Type → Lua.LuaE Lua.Exception (Maybe α))
               → IO (Maybe α)
withTableField ls tbl field convert = Lua.runWith (lbsLuaState ls) $ do
    tyTbl ← Lua.getglobal (Lua.Name tbl)
    result ← if tyTbl ≡ Lua.TypeTable
        then do
            tyField ← Lua.getfield (-1) (Lua.Name field)
            v ← convert tyField
            Lua.pop 1
            pure v
        else pure Nothing
    Lua.pop 1
    pure result

readBool ∷ LuaBackendState → BS.ByteString → BS.ByteString → IO (Maybe Bool)
readBool ls tbl field = withTableField ls tbl field $ \ty →
    if ty ≡ Lua.TypeBoolean then Just ⊚ Lua.toboolean (-1) else pure Nothing

readInt ∷ LuaBackendState → BS.ByteString → BS.ByteString → IO (Maybe Lua.Integer)
readInt ls tbl field = withTableField ls tbl field $ \ty →
    if ty ≡ Lua.TypeNumber then Lua.tointeger (-1) else pure Nothing

-- | Call @engine.loadScript@ on the fixture from inside a real
--   coroutine, recording into @probe@ both the script id it returned
--   and — as the control for the whole comparison — that the call site
--   really was off the main state.
loadFromCoroutine ∷ LuaBackendState → BS.ByteString → IO ()
loadFromCoroutine ls sidField = runLua_ ls $ BS.unlines
    [ "probe = probe or {}"
    , "local co = coroutine.create(function()"
    , "    local _, onMain = coroutine.running()"
    , "    probe.callerOnMain = onMain"
    , "    probe." <> sidField <> " = engine.loadScript('" <> fixturePath <> "', 3600.0)"
    , "end)"
    , "local ok, err = coroutine.resume(co)"
    , "probe.loadOk = ok"
    , "probe.loadErr = tostring(err)"
    ]

spec ∷ SpecWith EngineEnv
spec = describe "Lua script-state identity (#1059)" $ do
    it "runs a coroutine-issued engine.loadScript on the backend's \
       \canonical state, not the invoking coroutine's" $ \env → do
        ls ← newBareBackend env
        loadFromCoroutine ls "sid"

        -- Control: the call really did arrive from a coroutine, so
        -- "canonical" and "invoking" state are genuinely different here.
        readBool ls "probe" "loadOk"       ⌦ (`shouldBe` Just True)
        readBool ls "probe" "callerOnMain" ⌦ (`shouldBe` Just False)

        -- The contract: the module's own chunk ran on the main state.
        readInt  ls "luaScriptStateFixture" "loads"        ⌦ (`shouldBe` Just 1)
        readBool ls "luaScriptStateFixture" "loadedOnMain" ⌦ (`shouldBe` Just True)

        -- ... and so did the init callback the load fires.
        readInt  ls "luaScriptStateFixture" "inits"      ⌦ (`shouldBe` Just 1)
        readBool ls "luaScriptStateFixture" "initOnMain" ⌦ (`shouldBe` Just True)

        -- The script is tracked, under the id handed back to Lua.
        sid ← readInt ls "probe" "sid"
        scripts ← readTVarIO (lbsScripts ls)
        map (fromIntegral . scriptId) (Map.elems scripts)
            `shouldBe` maybe [] (:[]) sid

    it "runs a coroutine-issued engine.killScript's shutdown on that \
       \same canonical state, drops the script, and leaves the path \
       \free for a fresh reload" $ \env → do
        ls ← newBareBackend env
        loadFromCoroutine ls "sid"
        Just sid ← readInt ls "probe" "sid"

        runLua_ ls $ BS.unlines
            [ "local co = coroutine.create(function()"
            , "    engine.killScript(probe.sid)"
            , "end)"
            , "probe.killOk = coroutine.resume(co)"
            ]
        readBool ls "probe" "killOk" ⌦ (`shouldBe` Just True)

        readInt  ls "luaScriptStateFixture" "shutdowns"      ⌦ (`shouldBe` Just 1)
        readBool ls "luaScriptStateFixture" "shutdownOnMain" ⌦ (`shouldBe` Just True)

        afterKill ← readTVarIO (lbsScripts ls)
        Map.null afterKill `shouldBe` True

        -- A fresh reload of the same path: proves the kill really
        -- released the dedup entry (a still-loaded path would hand back
        -- the OLD id without re-running the chunk), and that both verbs
        -- left the canonical state usable.
        loadFromCoroutine ls "sid2"
        Just sid2 ← readInt ls "probe" "sid2"
        sid2 `shouldNotBe` sid
        readInt  ls "luaScriptStateFixture" "loads"        ⌦ (`shouldBe` Just 2)
        readBool ls "luaScriptStateFixture" "loadedOnMain" ⌦ (`shouldBe` Just True)
        readInt  ls "luaScriptStateFixture" "inits"        ⌦ (`shouldBe` Just 2)

        reloaded ← readTVarIO (lbsScripts ls)
        map (fromIntegral . scriptId) (Map.elems reloaded) `shouldBe` [sid2]
