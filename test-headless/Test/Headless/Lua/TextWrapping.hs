-- | #1159: the shared pixel-width wrapping helper
-- (@scripts/ui/text_wrap.lua@) and its two production shapes — the debug
-- console's character wrap (@shell.wrapText@) and the log panels' word wrap
-- with a character hard-break (@textWrap.byWord@).
--
-- These run the REAL Lua modules on a headless backend. The only stub is
-- 'engine.getTextWidth', which headless returns 0 for; here it measures ten
-- pixels per code point (so wrapping actually happens), REFUSES any candidate
-- that isn't valid UTF-8 (so a byte-fragment can never be silently measured),
-- and records every call (so "not once per byte" is a measured fact, not a
-- source grep).
module Test.Headless.Lua.TextWrapping (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)

spec ∷ Spec
spec = do
    around withHeadlessEngine $ do
        describe "debug console character wrap" $ do
            it "breaks between code points, never inside a UTF-8 sequence" $ \env → do
                ls ← wrapBackend env
                -- 12 code points spanning one-, two-, three-, and four-byte
                -- characters; every line boundary lands mid-group.
                eval ls
                    "local mixed='aé界🙂Bé界🙂cé界🙂'; \
                    \local lines=__shell.wrapText(mixed,35,1); \
                    \local valid=true; \
                    \for _,l in ipairs(lines) do if utf8.len(l)==nil then valid=false end end; \
                    \return #lines..'|'..tostring(valid) \
                    \..'|'..tostring(table.concat(lines)==mixed) \
                    \..'|'..table.concat(lines,'/')"
                    `shouldReturn` "4|true|true|aé界/🙂Bé/界🙂c/é界🙂"

            it "measures once per character, not once per byte" $ \env → do
                ls ← wrapBackend env
                eval ls
                    "local mixed='aé界🙂Bé界🙂cé界🙂'; \
                    \__measured={}; __shell.wrapText(mixed,35,1); \
                    \return #__measured..'|'..#mixed..'|'..utf8.len(mixed)"
                    `shouldReturn` "12|30|12"

            it "leaves pure-ASCII line breaks exactly where they were" $ \env → do
                ls ← wrapBackend env
                -- ASCII bytes and ASCII code points coincide, so these pin the
                -- pre-#1159 output: a plain split, spaces kept verbatim (this
                -- wrap has no word awareness), empty input, and a single
                -- character already wider than the line.
                eval ls
                    "local function j(t) return table.concat(t,'/') end; \
                    \local empty=__shell.wrapText('',35,1); \
                    \return j(__shell.wrapText('abcdefghij',35,1)) \
                    \..'|'..j(__shell.wrapText('ab cd ef',35,1)) \
                    \..'|['..j(empty)..']'..#empty \
                    \..'|'..j(__shell.wrapText('abc',5,1))"
                    `shouldReturn` "abc/def/ghi/j|ab /cd /ef|[]1|a/b/c"

        describe "log-panel word wrap" $ do
            it "hard-breaks an over-wide word between code points" $ \env → do
                ls ← wrapBackend env
                eval ls
                    "local word='界🙂é界🙂é'; \
                    \local lines=__tw.byWord(word,35,1,18); \
                    \local valid=true; \
                    \for _,l in ipairs(lines) do if utf8.len(l)==nil then valid=false end end; \
                    \return #lines..'|'..tostring(valid) \
                    \..'|'..tostring(table.concat(lines)==word) \
                    \..'|'..table.concat(lines,'/')"
                    `shouldReturn` "2|true|true|界🙂é/界🙂é"

            it "leaves pure-ASCII word wrapping and its hard break unchanged" $ \env → do
                ls ← wrapBackend env
                eval ls
                    "local function j(t) return table.concat(t,'/') end; \
                    \local empty=__tw.byWord('',55,1,18); \
                    \return j(__tw.byWord('aa bb cc',55,1,18)) \
                    \..'|'..j(__tw.byWord('  spaced   out  ',95,1,18)) \
                    \..'|['..j(empty)..']'..#empty \
                    \..'|'..j(__tw.byWord('abcdefgh',35,1,18))"
                    `shouldReturn` "aa bb/cc|spaced/out|[]1|abc/def/gh"

    -- The panels' wrap was three byte-identical private copies; they now share
    -- the one implementation tested above, so pinning the delegation is what
    -- makes that coverage theirs.
    describe "wrapping call sites (#1159 audit)" $ do
        it "the three log panels delegate their word wrap to the shared helper" $ do
            sources ← mapM TIO.readFile
                [ "scripts/combat_log.lua"
                , "scripts/injury_log_panel.lua"
                , "scripts/unit_log.lua"
                ]
            mapM_ (\src → do
                      T.isInfixOf "require(\"scripts.ui.text_wrap\")" src `shouldBe` True
                      T.isInfixOf "textWrap.byWord" src `shouldBe` True
                      T.isInfixOf "gmatch(\".\")" src `shouldBe` False)
                  sources
        it "the debug console wraps through the shared helper, byte loop gone" $ do
            src ← TIO.readFile "scripts/shell.lua"
            T.isInfixOf "textWrap.byCharacter" src `shouldBe` True
            T.isInfixOf "for i = 1, #text" src `shouldBe` False
            -- The unused word-wrapping twin is resolved by removal: byWord
            -- above is the one word-wrapping implementation left.
            T.isInfixOf "wrapTextByWord" src `shouldBe` False

-- | A Lua backend with the real wrapping modules loaded and a deterministic,
-- UTF-8-validating, call-recording width function.
wrapBackend ∷ EngineEnv → IO LuaBackendState
wrapBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    setup ← eval ls $ T.unwords
        [ "_G.__measured={};"
        , "engine.getTextWidth=function(_,t,_) local n,b=utf8.len(t);"
        , "assert(n,'invalid UTF-8 measured at byte '..tostring(b));"
        , "__measured[#__measured+1]=t; return n*10 end;"
        , "_G.__tw=require('scripts.ui.text_wrap');"
        , "_G.__shell=require('scripts.shell');"
        , "return 'ready'"
        ]
    setup `shouldBe` "ready"
    pure ls

eval ∷ LuaBackendState → T.Text → IO T.Text
eval ls code = do
    out ← executeDebugLua (lbsLuaState ls) code
    -- executeDebugLua JSON-serializes string return values; these fixtures
    -- avoid quotes and backslashes, so stripping the outer pair is enough.
    pure $ if T.length out ≥ 2 ∧ T.head out ≡ '"' ∧ T.last out ≡ '"'
        then T.dropEnd 1 (T.drop 1 out)
        else out
