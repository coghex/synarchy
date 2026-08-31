{-# LANGUAGE OverloadedStrings #-}
-- | The debug console's JSON serializer never names one JSON member
--   twice (#1955).
--
--   Lua keys and JSON member names are not in bijection. Numeric @1@ and
--   string @"1"@ both convert to the name @1@; lenient UTF-8 decoding
--   maps every invalid byte string onto U+FFFD; and booleans, tables,
--   functions, userdata and threads used to share the single literal
--   name @\<key\>@. 'Engine.Scripting.Lua.API.Shell.luaValueToText' then
--   joined the collected pairs without checking uniqueness, so the
--   console emitted objects with a repeated member — output that is
--   valid JSON and parses fine while quietly dropping half its entries.
--   That is the silent-wrong-answer failure #319 / PR #322 removed from
--   the same serializer's VALUES, one level worse: the consumer never
--   learns anything was lost.
--
--   The consumer is real and documented: @tools/probelib.py@'s
--   @send_json@ is @send@ plus @json.loads@, so every assertion here
--   decodes the console's own text with aeson before judging it, rather
--   than eyeballing bytes.
--
--   Driven through 'executeDebugLua' against a real Lua backend state —
--   the same primitive the TCP console runs, and the same technique as
--   "Test.Headless.Power.Placement" and "Test.Headless.UI.Clipping" —
--   because the defect lives in the boundary, not in a pure function
--   anyone can call. The shared serializer is what both surfaces use
--   ('executeDebugLua' and @shellTryLoadAndRun@'s sandbox), so fixing it
--   there covers the in-game shell too.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "debug console table keys"'@.
module Test.Headless.Lua.ConsoleTableKeys (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (Value(..), decodeStrict)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (newIORef)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))

spec ∷ SpecWith EngineEnv
spec = do

    -- * The collision itself

    it "refuses a table mixing numeric 1 with string \"1\"" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "return {[1]='numeric', ['1']='string'}"
        r `shouldBe` "\"<duplicate key \\\"1\\\">\""
        decodeJson r `shouldBe` Just (String "<duplicate key \"1\">")

    it "refuses the issue's four-key reproduction instead of losing two \
       \values" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls
            "return {[1]='numeric', ['1']='string', [true]='bool', \
            \[false]='false-bool'}"
        -- Before #1955 this decoded to a two-member dict, having dropped
        -- 'numeric' and 'false-bool' (or the other two — Lua's traversal
        -- order decided, and that is not a contract).
        decodeJson r `shouldBe` Just (String "<duplicate key \"1\">")

    it "refuses numeric 1.5 colliding with string \"1.5\"" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "return {[1.5]='numeric', ['1.5']='string'}"
        decodeJson r `shouldBe` Just (String "<duplicate key \"1.5\">")

    it "refuses two distinct invalid-UTF-8 string keys, which lenient \
       \decoding collapses onto one replacement character" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls
            "return {[string.char(128)]='low', [string.char(129)]='high'}"
        decodeJson r `shouldBe` Just (String "<duplicate key \"\65533\">")

    it "names an unrepresentable nested table in place, leaving the \
       \parent object intact" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "return {outer = {[1]='numeric', ['1']='string'}}"
        r `shouldBe` "{\"outer\":\"<duplicate key \\\"1\\\">\"}"
        decodeJson r `shouldBe`
            Just (Object (KM.fromList
                [("outer", String "<duplicate key \"1\">")]))

    -- * Key types that used to share one name

    it "gives the two boolean keys names of their own" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "return {[true]='yes', [false]='no'}"
        objectMembers r `shouldBe`
            Just [ ("<boolean: false>", String "no")
                 , ("<boolean: true>", String "yes") ]

    it "gives two distinct table keys names of their own" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "local t = {}; t[{}] = 'first'; t[{}] = 'second'; \
                         \return t"
        case objectMembers r of
            Nothing → expectationFailure ("not a JSON object: " <> show r)
            Just ms → do
                map snd (sort ms) `shouldBe` [String "first", String "second"]
                map fst ms `shouldSatisfy` all (T.isPrefixOf "<table: ")

    it "distinguishes a table key from a function key" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "local t = {}; t[{}] = 'tbl'; t[print] = 'fn'; \
                         \return t"
        case objectMembers r of
            Nothing → expectationFailure ("not a JSON object: " <> show r)
            Just ms → do
                map snd (sort ms) `shouldBe` [String "fn", String "tbl"]
                sort (map (T.takeWhile (≢ ':') . fst) ms)
                    `shouldBe` ["<function", "<table"]

    -- * Shapes the 67 probe modules depend on, unchanged

    it "renders an all-string-keyed table as the same object as before" $
        \env → do
            ls ← newBareLuaBackend env
            r ← evalDebug ls "return {alpha=1, beta='two'}"
            -- Byte-level: still a brace-wrapped, comma-joined member
            -- list. Lua's traversal order decides which member comes
            -- first, so the set is what can be pinned.
            rawMembers r `shouldBe` Just (sort ["\"alpha\":1", "\"beta\":\"two\""])
            decodeJson r `shouldBe`
                Just (Object (KM.fromList
                    [("alpha", Number 1), ("beta", String "two")]))

    it "renders a consecutive array unchanged" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "return {10, 20, 30}"
        r `shouldBe` "[10,20,30]"

    it "still renders numeric-looking string keys as an ordered array" $
        \env → do
            ls ← newBareLuaBackend env
            -- Array classification parses the RENDERED key text, so a
            -- table keyed only by the strings "1" and "2" has always come
            -- back as an array. Distinct names, no collision: unchanged.
            r ← evalDebug ls "return {['1']='a', ['2']='b'}"
            r `shouldBe` "[\"a\",\"b\"]"

    it "still renders a sparse integer-keyed table as a full object" $
        \env → do
            ls ← newBareLuaBackend env
            r ← evalDebug ls "return {[1]='a', [5]='b'}"
            objectMembers r `shouldBe`
                Just [("1", String "a"), ("5", String "b")]

    it "renders an empty table as an empty object" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "return {}"
        r `shouldBe` "{}"

    -- * PR #322's value rules and the rest of the frozen surface

    it "keeps the quoted non-finite stand-ins and the tab-joined \
       \multiple-return form" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "return math.huge, -math.huge, 0/0"
        r `shouldBe` "\"inf\"\t\"-inf\"\t\"nan\""

    it "keeps C0 control characters escaped" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "return 'a' .. string.char(1) .. 'b'"
        r `shouldBe` "\"a\\u0001b\""
        decodeJson r `shouldBe` Just (String "a\SOHb")

    it "keeps finite-number formatting" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "return {1, 2.5, -3}"
        r `shouldBe` "[1,2.5,-3]"

    it "keeps the depth guard" $ \env → do
        ls ← newBareLuaBackend env
        r ← evalDebug ls "local t = {}; local c = t; \
                         \for _ = 1, 12 do c.n = {}; c = c.n end; return t"
        r `shouldSatisfy` T.isInfixOf "\"<max depth>\""

-- * Plumbing

-- | A real Lua backend with the full Lua API registered and nothing
--   loaded, exactly as "Test.Headless.Power.Placement" builds one. Each
--   example gets its own so a table left in a global cannot leak
--   between them.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console uses.
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

-- | What @probelib.send_json@ sees: @json.loads@ on the console's own
--   text. 'Nothing' means the console emitted something no consumer
--   could parse at all.
decodeJson ∷ Text → Maybe Value
decodeJson = decodeStrict . TE.encodeUtf8

-- | The decoded object's members, sorted by name — 'Nothing' when the
--   result is not a JSON object. Sorting is what makes an assertion
--   independent of Lua's traversal order, and the member COUNT is what
--   catches a silently reduced dictionary.
objectMembers ∷ Text → Maybe [(Text, Value)]
objectMembers t = case decodeJson t of
    Just (Object o) → Just (sort [ (AK.toText k, v) | (k, v) ← KM.toList o ])
    _               → Nothing

-- | The raw, still-encoded member texts of a brace-wrapped object,
--   sorted. Used where the point is that the BYTES are unchanged rather
--   than that the decode agrees; only safe for members containing no
--   comma of their own.
rawMembers ∷ Text → Maybe [Text]
rawMembers t = do
    inner ← T.stripSuffix "}" =≪ T.stripPrefix "{" t
    pure (sort (T.splitOn "," inner))
