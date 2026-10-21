{-# LANGUAGE OverloadedStrings #-}
-- | What "Lua scenario API" (#2715) and its startup group share: a fixture
--   engine with the PRODUCTION scenario verbs registered over a resource
--   root that is not the launch directory, and the helpers that call them
--   as real Lua.
module Test.Headless.Lua.ScenarioApi.Support
    ( Api(..)
    , withApi
    , call
    , put
    , observed
    , raises
    , loadViaLua
    , saveViaLua
    , readInt
    ) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket)
import Control.Monad ((>=>))
import Data.IORef (readIORef)
import qualified Data.Text as T
import qualified HsLua as Lua
import System.Directory
import System.FilePath ((</>))
import System.Random (randomRIO)
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status
import Engine.Save.Barrier (readSaveStatus)
import Engine.Scripting.Lua.API.Register.Engine (registerEngineAPIWith)
import Engine.Scripting.Lua.API.Scenario (ScenarioApi(..))
import Engine.Scripting.Lua.CallStats (newLuaCallStats)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Scenario.Path (ScenarioRoots(..))
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))

data Api = Api { aH ∷ H, aLaunch ∷ FilePath, aRoot ∷ FilePath }

-- | The launch directory and the resource root are different places, both
--   with a space in their names.
withApi ∷ (Api → IO ()) → IO ()
withApi act = withH $ \h → scratch $ \base → do
    let launch = base </> "launch dir"
        root = base </> "resource root"
    mapM_ (createDirectoryIfMissing True) [launch, root </> "scenarios"]
    stats ← newLuaCallStats
    let api = ScenarioApi (pure (ScenarioRoots launch root))
    Lua.runWith (lbsLuaState (fxLua (hFx h))) $
        registerEngineAPIWith api stats (fxEnv (hFx h)) (fxLua (hFx h))
    -- a one-line, deterministic rendering of any Lua value
    _ ← lua h $ T.concat
        [ "function __ser(v) if type(v) ~= 'table' then return tostring(v) end "
        , "local keys = {} for k in pairs(v) do keys[#keys + 1] = k end "
        , "table.sort(keys, function(a, b) return tostring(a) < tostring(b) end) "
        , "local out = {} for _, k in ipairs(keys) do "
        , "out[#out + 1] = tostring(k) .. '=' .. __ser(v[k]) end "
        , "return '{' .. table.concat(out, ',') .. '}' end return 'ok'" ]
    act (Api h launch root)
  where
    scratch f = bracket make removePathForcibly (canonicalizePath >=> f)
    make = do
        tmp ← getTemporaryDirectory
        n ← randomRIO (0 ∷ Int, 10 ^ (9 ∷ Int))
        let dir = tmp </> ("lua scenario api " ⧺ show n)
        createDirectoryIfMissing True dir
        pure dir

call ∷ Api → Text → IO Text
call a = lua (aH a)

-- | Write a scenario into the scenario folder under a name.
put ∷ Api → FilePath → String → IO FilePath
put a name text = do
    let path = aRoot a </> "scenarios" </> name
    createDirectoryIfMissing True (aRoot a </> "scenarios")
    writeFile path text
    pure path

-- | The whole observable line: the load status, the save status, the
--   scenario status and the pause flag. Rejections must leave it as it was.
observed ∷ Api → IO (Maybe (Int, String), String, Bool, Text)
observed a = do
    let env = fxEnv (hFx (aH a))
        sl = toSaveLoadCapability env
    generic ← readLoadStatus (slLoadStatusRef sl)
    saveSt ← readSaveStatus (slSaveBarrierRef sl)
    paused ← readIORef (enginePausedRef env)
    scn ← call a "return __ser(engine.getScenarioStatus())"
    pure (fmap (\l → (lsRequestId l, show (lsPhase l))) generic, show saveSt, paused, scn)

-- | Call a verb that must raise a Lua error; the message contains @needle@.
raises ∷ Api → Text → Text → Expectation
raises a chunk needle = do
    r ← call a ("local ok, err = pcall(function() " <> chunk <> " end) return tostring(ok) .. '|' .. tostring(err)")
    let (ok, msg) = T.breakOn "|" r
    (ok, T.unpack msg) `shouldSatisfy` \(o, m) → o ≡ "false" ∧ T.unpack needle `isInfix` m
  where isInfix n h = T.pack n `T.isInfixOf` T.pack h

-- | Accept a load through the verb and answer its id.
loadViaLua ∷ Api → Text → IO Int
loadViaLua a name = do
    r ← call a ("local id, why = engine.loadScenario(" <> name <> ") return tostring(id) .. '|' .. __ser(why)")
    case T.breakOn "|" r of
        (idText, _) | Just n ← readInt idText → pure n
        _ → fail ("the load was not accepted: " ⧺ T.unpack r)

readInt ∷ Text → Maybe Int
readInt t = case reads (T.unpack t) of [(n, "")] → Just n; _ → Nothing

-- | Accept a capture through the verb and answer its id.
saveViaLua ∷ Api → Text → Text → IO Int
saveViaLua a name opts = do
    r ← call a ("local id, why = engine.saveScenario(" <> name <> opts <> ") return tostring(id) .. '|' .. __ser(why)")
    case T.breakOn "|" r of
        (idText, _) | Just n ← readInt idText → pure n
        _ → fail ("the capture was not accepted: " ⧺ T.unpack r)
