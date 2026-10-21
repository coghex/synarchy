{-# LANGUAGE OverloadedStrings #-}
-- | "Lua scenario API" (#2715, SCN-15 of epic #2698): the PRODUCTION
--   @engine.loadScenario@, @engine.saveScenario@ and
--   @engine.getScenarioStatus@ verbs, called as real Lua against the
--   production registration, the production services ("Scenario.Load" and
--   "Scenario.Capture"), the real save barrier and world thread, and the
--   stand-in owners of "Test.Headless.Scenario.Load.Harness" — with a
--   resource root of the example's own that is NOT the launch directory.
--
--   What this gate owns: strict argument and option handling, the path
--   policy's refusals reaching the caller as rejections, admission versus
--   completion (rejections allocate nothing and change nothing; an
--   accepted id is the id the status reports), busy rejection beside an
--   unchanged active status, the status vocabulary (stage, outcome,
--   structured warnings, recovery, publication), explicit overwrite, and a
--   captured file that reloads without touching its source. The
--   whole-process evidence (CLI boot, console readiness, exit codes) is
--   @tools/scenario_entrypoint_probe.py@'s.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua scenario API"'@.
module Test.Headless.Lua.ScenarioApi (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar (putMVar, takeMVar)
import Data.IORef (readIORef)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import Engine.Core.State (EngineEnv(..))
import Scenario.Capture.Types (CaptureOutcome(..), ScenarioCaptureStatus(..))
import Scenario.Load.Types
import Test.Headless.Lua.ScenarioApi.Support
import Test.Headless.Scenario.Capture.Support (awaitCapture, withPump)
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Unit.Types (UnitManager(..))

spec ∷ Spec
spec = describe "Lua scenario API" $ aroundAll withApi $ do
    argumentSpec
    rejectionSpec
    completionSpec
    warningSpec
    failureSpec
    busySpec
    saveSpec

-- * Strict arguments

argumentSpec ∷ SpecWith Api
argumentSpec = describe "strict arguments and options" $ do

    it "refuses a path that is not a string, without coercing a number into one" $ \a → do
        before ← observed a
        forM_ ["", "nil", "5", "'5', 6", "true", "{}", "{'a'}", "print", "5.5"] $ \args →
            raises a ("engine.loadScenario(" <> args <> ")") "engine.loadScenario"
        raises a "engine.loadScenario(5)" "path must be a string"
        raises a "engine.saveScenario(5)" "path must be a string"
        raises a "engine.saveScenario({})" "path must be a string"
        observed a `shouldReturn` before

    it "refuses a path that is not valid UTF-8, rather than loading a different file" $ \a → do
        before ← observed a
        raises a "engine.loadScenario('bad\\xff\\xfe')" "not valid UTF-8"
        raises a "engine.saveScenario('bad\\xff\\xfe')" "not valid UTF-8"
        observed a `shouldReturn` before

    it "refuses a wrong argument count for every verb" $ \a → do
        raises a "engine.loadScenario()" "takes 1 argument(s), got 0"
        raises a "engine.loadScenario('a', 'b')" "takes 1 argument(s), got 2"
        raises a "engine.saveScenario()" "takes 1 to 2 argument(s), got 0"
        raises a "engine.saveScenario('a', {}, 3)" "takes 1 to 2 argument(s), got 3"
        raises a "engine.getScenarioStatus(1)" "takes 0 argument(s), got 1"

    it "refuses options that are not a table, unknown options and wrong-typed values" $ \a → do
        before ← observed a
        raises a "engine.saveScenario('x', 5)" "options must be a table"
        raises a "engine.saveScenario('x', 'overwrite')" "options must be a table"
        raises a "engine.saveScenario('x', true)" "options must be a table"
        raises a "engine.saveScenario('x', {overwrite = 'yes'})" "overwrite must be a boolean"
        raises a "engine.saveScenario('x', {overwrite = 1})" "overwrite must be a boolean"
        raises a "engine.saveScenario('x', {overwrite = nil, force = true})" "unknown option"
        raises a "engine.saveScenario('x', {overwrite = true, extra = 1})" "unknown option"
        raises a "engine.saveScenario('x', {true})" "option names must be strings"
        observed a `shouldReturn` before

    it "does not run a metamethod while it reads the options" $ \a → do
        before ← observed a
        raises a
            "engine.saveScenario('x', setmetatable({force = true}, {__index = function() error('index ran') end, \
            \__pairs = function() error('pairs ran') end}))"
            "unknown option"
        observed a `shouldReturn` before

-- * Rejections

rejectionSpec ∷ SpecWith Api
rejectionSpec = describe "rejection is immediate and changes nothing" $ do

    it "answers nil plus a reason, a message and the resolved path, for a missing file" $ \a → do
        before ← observed a
        r ← call a "local id, why = engine.loadScenario('absent') return tostring(id) .. '|' .. why.reason .. '|' .. why.path"
        r `shouldBe` T.pack ("nil|not_found|" ⧺ aRoot a </> "scenarios" </> "absent.yaml")
        observed a `shouldReturn` before

    it "answers invalid_path for traversal, a foreign extension and an empty name" $ \a → do
        before ← observed a
        forM_ [("'../x'", "leaves the scenario folder"), ("'x.yml'", "unsupported extension"), ("''", "empty")] $ \(arg, why) → do
            r ← call a ("local id, rej = engine.loadScenario(" <> arg <> ") return tostring(id) .. '|' .. rej.reason .. '|' .. rej.message")
            let (idText, rest) = T.breakOn "|" r
            idText `shouldBe` "nil"
            T.unpack rest `shouldSatisfy` \m → "|invalid_path|" `isPrefix` m ∧ why `isInfix` m
        observed a `shouldReturn` before

    it "refuses a symlink that leaves the scenario folder, for a load and for a capture" $ \a → do
        outside ← pure (aLaunch a </> "outside.yaml")
        writeFile outside (doc ["units: []"])
        createFileLink outside (aRoot a </> "scenarios" </> "escape.yaml")
        before ← observed a
        r1 ← call a "local id, rej = engine.loadScenario('escape') return tostring(id) .. '|' .. rej.reason"
        r1 `shouldBe` "nil|invalid_path"
        r2 ← call a "local id, rej = engine.saveScenario('escape', {overwrite = true}) return tostring(id) .. '|' .. rej.reason"
        r2 `shouldBe` "nil|invalid_path"
        observed a `shouldReturn` before
        BS.readFile outside `shouldReturn` BS.pack (map (fromIntegral ∘ fromEnum) (doc ["units: []"]))

    it "allocates no identity: the next accepted request is the one after the last" $ \a → do
        _ ← put a "ids.yaml" (doc ["units: []"])
        n1 ← loadViaLua a "'ids'"
        _ ← pumpUntilTerminal (aH a) n1 60
        _ ← call a "return tostring(engine.loadScenario('absent-1')) .. tostring(engine.loadScenario('../absent-2'))"
        n2 ← loadViaLua a "'ids'"
        n2 `shouldBe` n1 + 1
        _ ← pumpUntilTerminal (aH a) n2 60
        pure ()
  where
    isPrefix p m = p `T.isPrefixOf` T.pack m
    isInfix p m = T.pack p `T.isInfixOf` T.pack m

-- * Admission versus completion

completionSpec ∷ SpecWith Api
completionSpec = describe "an accepted request" $ do

    it "answers its id, then reports that operation through to its ready result, paused" $ \a → do
        path ← put a "ready.yaml" (doc ["units: []"])
        n ← loadViaLua a "'ready'"
        -- admitted, not finished: the status already names this operation
        early ← call a "local s = engine.getScenarioStatus() return s.kind .. '|' .. s.id .. '|' .. tostring(s.done)"
        T.unpack early `shouldSatisfy` \e → ("load|" ⧺ show n ⧺ "|") `isPrefixOfS` e
        _ ← pumpUntilTerminal (aH a) n 60
        done ← call a $ T.concat
            [ "local s = engine.getScenarioStatus() "
            , "return s.kind .. '|' .. s.id .. '|' .. s.source .. '|' .. s.stage .. '|' .. s.outcome "
            , ".. '|' .. tostring(s.done) .. '|' .. tostring(s.published) .. '|' .. s.recovery "
            , ".. '|' .. #s.warnings .. '|' .. tostring(s.failure)" ]
        done `shouldBe` T.pack ("load|" ⧺ show n ⧺ "|" ⧺ path ⧺ "|ready|ready|true|true|none|0|nil")
        call a "return tostring(engine.isPaused())" `shouldReturn` "true"

    it "resolves the name with and without its extension to the same file" $ \a → do
        path ← put a "twice.yaml" (doc ["units: []"])
        forM_ ["'twice'", "'twice.yaml'", "'./twice'"] $ \arg → do
            n ← loadViaLua a arg
            _ ← pumpUntilTerminal (aH a) n 60
            call a "return engine.getScenarioStatus().source" `shouldReturn` T.pack path

    it "resolves an absolute path through the verb, outside the scenario folder" $ \a → do
        let path = aLaunch a </> "abs ünï.yaml"
        writeFile path (doc ["units: []"])
        -- a long-bracket literal: the path's UTF-8 bytes go in verbatim
        n ← loadViaLua a ("[==[" <> T.pack path <> "]==]")
        _ ← pumpUntilTerminal (aH a) n 60
        call a "return engine.getScenarioStatus().source" `shouldReturn` T.pack path

    it "keeps a terminal result readable until the next operation is admitted, then reports the new one" $ \a → do
        _ ← put a "keep.yaml" (doc ["units: []"])
        n1 ← loadViaLua a "'keep'"
        _ ← pumpUntilTerminal (aH a) n1 60
        s1 ← call a "return __ser(engine.getScenarioStatus())"
        call a "return __ser(engine.getScenarioStatus())" `shouldReturn` s1
        -- a rejected request is not an operation
        _ ← call a "return tostring(engine.loadScenario('absent'))"
        call a "return __ser(engine.getScenarioStatus())" `shouldReturn` s1
        n2 ← loadViaLua a "'keep'"
        n2 `shouldBe` n1 + 1
        call a "return engine.getScenarioStatus().id" `shouldReturn` T.pack (show n2)
        _ ← pumpUntilTerminal (aH a) n2 60
        pure ()
  where isPrefixOfS p m = p `isPrefixOf'` m
        isPrefixOf' p m = T.pack p `T.isPrefixOf` T.pack m

-- * Warnings

warningSpec ∷ SpecWith Api
warningSpec = describe "partial content" $ do

    it "loads the usable subset, ready, with its structured warnings and exactly the retained content" $ \a → do
        _ ← put a "partial.yaml" $ doc
            [ "units:"
            , "  - {id: ghost, definition: no_such_unit, x: 1, y: 1}"
            , "  - {id: loner, definition: acolyte, x: 2, y: 2}" ]
        n ← loadViaLua a "'partial'"
        s ← pumpUntilTerminal (aH a) n 60
        slsOutcome s `shouldBe` Just ScenarioReady
        -- the retained content: the valid unit only
        um ← readIORef (unitManagerRef (fxEnv (hFx (aH a))))
        Map.elems (umScenarioRefs um) `shouldBe` ["loner"]
        -- the warnings, as a script sees them
        call a "return #engine.getScenarioStatus().warnings" `shouldReturn` "1"
        call a (T.concat
            [ "local w = engine.getScenarioStatus().warnings[1] "
            , "return w.path .. '|' .. w.code .. '|' .. w.detail .. '|' .. w.effect" ])
            `shouldReturn` "units[0].definition|unknown_definition|no_such_unit|entry_rejected"
        call a "return engine.getScenarioStatus().outcome" `shouldReturn` "ready"
        call a "return tostring(engine.isPaused())" `shouldReturn` "true"

-- * Failures

failureSpec ∷ SpecWith Api
failureSpec = describe "unsuccessful loads" $ do

    let failed a name text expectKind expectSource = do
            _ ← put a name text
            before ← sessionFingerprint (aH a)
            n ← loadViaLua a (T.pack (show (dropExt name)))
            s ← pumpUntilTerminal (aH a) n 60
            slsPublished s `shouldBe` False
            r ← call a $ T.concat
                [ "local s = engine.getScenarioStatus() "
                , "return s.outcome .. '|' .. s.stage .. '|' .. s.recovery .. '|' .. s.failure.kind "
                , ".. '|' .. tostring(s.failure.source) .. '|' .. s.failure.stage .. '|' .. tostring(s.published)" ]
            r `shouldBe` ("failed|failed|preserved|" <> expectKind <> "|" <> expectSource <> "|preflight|false")
            -- the previous session stands, and a fallback is never reported as the load
            sessionFingerprint (aH a) `shouldReturn` before
        dropExt = takeWhile (≢ '.')

    it "reports a syntax error as a source failure and keeps the previous session" $ \a →
        failed a "syntax.yaml" "version: 3\nunits: [unclosed\n" "source" "syntax_error"

    it "reports an unsupported version" $ \a →
        failed a "future.yaml" "version: 99\n" "source" "version_unsupported"

    it "reports a missing version and a document that is not a mapping" $ \a → do
        failed a "nover.yaml" "units: []\n" "source" "version_missing"
        failed a "list.yaml" "- 1\n- 2\n" "source" "not_a_mapping"

    it "lets a later valid load succeed" $ \a → do
        _ ← put a "after.yaml" (doc ["units: []"])
        n ← loadViaLua a "'after'"
        s ← pumpUntilTerminal (aH a) n 60
        slsOutcome s `shouldBe` Just ScenarioReady
        call a "return engine.getScenarioStatus().outcome" `shouldReturn` "ready"

-- * Busy

busySpec ∷ SpecWith Api
busySpec = describe "a busy line" $ do

    it "rejects a load and a save at once, leaves the active status and the pause alone, and lets a retry succeed" $ \a → do
        _ ← put a "held.yaml" (doc ["units: []"])
        let h = aH a
        hold ← newHold
        armSeams h (holdAt PointPreflight hold)
        n ← loadViaLua a "'held'"
        pump ← startPump h
        takeMVar (holdReached hold)
        before ← observed a
        r ← call a $ T.concat
            [ "local l, lrej = engine.loadScenario('held') "
            , "local s, srej = engine.saveScenario('held-out') "
            , "return tostring(l) .. '|' .. lrej.reason .. '|' .. tostring(s) .. '|' .. srej.reason" ]
        r `shouldBe` "nil|busy|nil|busy"
        observed a `shouldReturn` before
        call a "return engine.getScenarioStatus().id" `shouldReturn` T.pack (show n)
        -- the busy request queued nothing: the same operation finishes
        putMVar (holdRelease hold) ()
        s ← await h n 60
        stopPump pump
        disarm h
        slsRequestId s `shouldBe` n
        slsOutcome s `shouldBe` Just ScenarioReady
        -- the caller retries explicitly and is admitted under a new identity
        n2 ← loadViaLua a "'held'"
        n2 `shouldBe` n + 1
        _ ← pumpUntilTerminal h n2 60
        pure ()

-- * Save

saveSpec ∷ SpecWith Api
saveSpec = describe "saving" $ do

    it "refuses to overwrite by default, overwrites on explicit intent, and reloads the result without touching its source" $ \a → do
        let h = aH a
        source ← put a "origin.yaml" (doc ["units:", "  - {id: loner, definition: acolyte, x: 2, y: 2}"])
        sourceBefore ← BS.readFile source
        n0 ← loadViaLua a "'origin'"
        _ ← pumpUntilTerminal h n0 60
        let saveStatus = T.concat
                [ "local s = engine.getScenarioStatus() "
                , "return s.kind .. '|' .. s.outcome .. '|' .. tostring(s.published) .. '|' .. tostring(s.overwrite) "
                , ".. '|' .. tostring(s.replaced) .. '|' .. tostring(s.failure and s.failure.kind)" ]
            out = aRoot a </> "scenarios" </> "copy.yaml"
        -- 1. a new name: saved, published, no replacement
        n1 ← saveViaLua a "'copy'" ""
        s1 ← withPump h (awaitCapture h n1 60)
        fmap isSucceeded (cstOutcome s1) `shouldBe` Just True
        call a saveStatus `shouldReturn` "save|saved|true|false|false|nil"
        call a "return engine.getScenarioStatus().destination" `shouldReturn` T.pack out
        firstBytes ← BS.readFile out
        -- 2. the default refuses an existing file and changes no byte
        n2 ← saveViaLua a "'copy'" ""
        s2 ← withPump h (awaitCapture h n2 60)
        fmap isSucceeded (cstOutcome s2) `shouldBe` Just False
        call a saveStatus `shouldReturn` "save|failed|false|false|nil|overwrite_refused"
        BS.readFile out `shouldReturn` firstBytes
        -- 2b. an explicit nil, an empty table and overwrite = false are the same default
        forM_ [", nil", ", {}", ", {overwrite = false}"] $ \opts → do
            nx ← saveViaLua a "'copy'" opts
            sx ← withPump h (awaitCapture h nx 60)
            fmap isSucceeded (cstOutcome sx) `shouldBe` Just False
            BS.readFile out `shouldReturn` firstBytes
        -- 3. explicit overwrite replaces it
        n3 ← saveViaLua a "'copy'" ", {overwrite = true}"
        s3 ← withPump h (awaitCapture h n3 60)
        fmap isSucceeded (cstOutcome s3) `shouldBe` Just True
        call a saveStatus `shouldReturn` "save|saved|true|true|true|nil"
        -- the arena stays paused, and the identities are distinct operations
        call a "return tostring(engine.isPaused())" `shouldReturn` "true"
        [n1, n2, n3] `shouldSatisfy` \ns → length (nubInts ns) ≡ 3
        -- 4. the captured file reloads; its source was never rewritten
        n4 ← loadViaLua a "'copy'"
        r4 ← pumpUntilTerminal h n4 60
        slsOutcome r4 `shouldBe` Just ScenarioReady
        BS.readFile source `shouldReturn` sourceBefore

    it "writes a folder-relative capture into a scenario folder it creates on first use" $ \a → do
        let h = aH a
            fresh = aRoot a </> "scenarios"
        removePathForcibly fresh
        n ← saveViaLua a "'made'" ""
        s ← withPump h (awaitCapture h n 60)
        fmap isSucceeded (cstOutcome s) `shouldBe` Just True
        doesFileExist (fresh </> "made.yaml") `shouldReturn` True
  where
    isSucceeded = \case CaptureSucceeded _ → True; _ → False
    nubInts = foldr (\x acc → if x `elem` acc then acc else x : acc) []

