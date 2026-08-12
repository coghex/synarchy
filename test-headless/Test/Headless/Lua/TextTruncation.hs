-- | #1189: the two live byte-unsafe UI truncators.
--
-- @scripts/crafting_panel.lua@'s @truncate@ (a code-point CAP) and
-- @scripts/unit_info_v2_panel_engine.lua@'s @abbreviateToWidth@ (a pixel-width
-- abbreviation) both counted bytes and sliced by byte offset, so a cut landing
-- inside a multi-byte UTF-8 sequence produced mojibake or a measurement
-- failure — and @truncate@'s @maxChars@ was a lie for any non-ASCII name
-- (22 CJK characters capped at 20 came back as 8).
--
-- These run the REAL production helpers, reached through their own modules;
-- a copied algorithm would say nothing about these six call sites. The only
-- stub is 'engine.getTextWidth', which headless returns 0 for: here it
-- measures ten pixels per code point (so abbreviation actually happens),
-- REFUSES any argument that isn't valid UTF-8 (so every intermediate
-- candidate is checked, not just the returned one), and records every call
-- (so "once per character, not once per byte" is measured, not asserted from
-- a source grep).
module Test.Headless.Lua.TextTruncation (spec) where

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
spec = around withHeadlessEngine $ do

    describe "the width stub itself" $ do
        it "rejects a byte fragment, so the guard below is load-bearing" $ \env → do
            ls ← truncBackend env
            -- '界' is E7 95 8C; measuring its lead byte alone must raise.
            -- Without this, a helper that never produced a fragment and a
            -- guard that never fired would look identical.
            out ← eval ls "local ok = pcall(engine.getTextWidth, nil, \
                          \string.sub('界', 1, 1), 1); return tostring(ok)"
            out `shouldBe` "false"

    describe "crafting bill panel character cap" $ do
        it "leaves pure-ASCII caps byte-identical, two ASCII periods and all" $ \env → do
            ls ← truncBackend env
            -- Pins the pre-#1189 ASCII contract exactly: the already-fits
            -- branch, the boundary where the cap is met exactly, the cut
            -- (18 kept + ".."), a second cap width, and the empty/nil inputs.
            eval ls
                "local t=__cp.truncate; \
                \return t('short name',20)..'|'..t('abcdefghijklmnopqrst',20) \
                \..'|'..t('abcdefghijklmnopqrstu',20) \
                \..'|'..t('abcdefghijklmnop',12) \
                \..'|['..t('',20)..']['..t(nil,20)..']'"
                `shouldReturn`
                "short name|abcdefghijklmnopqrst|abcdefghijklmnopqr..\
                \|abcdefghij..|[][]"

        it "cuts multi-byte names at whole characters, and maxChars means characters" $ \env → do
            ls ← truncBackend env
            -- Before #1189 the emoji cut returned invalid UTF-8 and the CJK
            -- cut returned 8 code points where 20 were asked for.
            eval ls
                "local t=__cp.truncate; \
                \local function d(s) local r=t(s,20); \
                \  return r..'~'..tostring(utf8.len(r)) end; \
                \return d('Café Brûlée Ampoule Réactive') \
                \..'|'..d(__cjk)..'|'..d(__emoji) \
                \..'|'..d('aé界🙂Bé界🙂cé界🙂dé界🙂eé界🙂fé')"
                `shouldReturn` T.concat
                    [ "Café Brûlée Ampoul..~20|"
                    , T.replicate 18 "界", "..~20|"
                    , T.replicate 18 "🙂", "..~20|"
                    , "aé界🙂Bé界🙂cé界🙂dé界🙂eé..~20"
                    ]

        it "returns a multi-byte name that already fits completely unchanged" $ \env → do
            ls ← truncBackend env
            -- 11 code points but 14 bytes: the byte count alone would have
            -- cut this at a cap of 12.
            eval ls
                "local t=__cp.truncate; local s='Café Brûlée'; \
                \return tostring(t(s,20)==s)..'|'..tostring(t(s,11)==s) \
                \..'|'..utf8.len(s)..'|'..#s..'|'..t(s,12)"
                `shouldReturn` "true|true|11|14|Café Brûlée"

    describe "unit info stat abbreviation" $ do
        it "leaves pure-ASCII abbreviation byte-identical, one U+2026 and all" $ \env → do
            ls ← truncBackend env
            -- Pins the pre-#1189 ASCII contract: fits, the maxW<=0 guard, a
            -- real cut keeping its single trailing ellipsis, and the
            -- nothing-fits floor.
            eval ls
                "local a=__pe.abbreviateToWidth; \
                \local cut=a('Severely Bruised Left Arm',100,1); \
                \local _,n=cut:gsub('…',''); \
                \return a('Bruised',200,1)..'|'..a('Bruised',0,1) \
                \..'|'..cut..'|'..n..'|'..a('Bruised',5,1)"
                `shouldReturn` "Bruised|Bruised|Severely …|1|…"

        it "sweeps every cut position of a mixed string without ever measuring a fragment" $ \env → do
            ls ← truncBackend env
            -- 13 widths across a 1/2/3/4-byte string exercise cut points at
            -- and between every character boundary. The stub raises on any
            -- invalid argument, so completing the sweep at all is the proof
            -- that no intermediate candidate split a sequence; the pinned
            -- output additionally proves each cut is where it belongs.
            eval ls
                "local a=__pe.abbreviateToWidth; local out={}; local valid=true; \
                \for w=10,130,10 do local r=a(__mixed,w,1); \
                \  if utf8.len(r)==nil then valid=false end; out[#out+1]=r end; \
                \return tostring(valid)..'|'..table.concat(out,'/')"
                `shouldReturn`
                "true|…/…/aé…/aé界…/aé界🙂…/aé界🙂B…/aé界🙂Bé…/aé界🙂Bé界…\
                \/aé界🙂Bé界🙂…/aé界🙂Bé界🙂c…/aé界🙂Bé界🙂cé…\
                \/aé界🙂Bé界🙂cé界🙂/aé界🙂Bé界🙂cé界🙂"

        it "measures once per character, not once per byte" $ \env → do
            ls ← truncBackend env
            -- 12 code points over 30 bytes: one fits-check plus one candidate
            -- per character down to the one that fits. The byte walk this
            -- replaced measured 30 candidates, 18 of them split sequences.
            eval ls
                "__measured={}; __pe.abbreviateToWidth(__mixed,35,1); \
                \return #__measured..'|'..#__mixed..'|'..utf8.len(__mixed)"
                `shouldReturn` "12|30|12"

    -- Requirement 3/4: the two dead twins go away entirely. Their only other
    -- reference was the baseSizes cap they read, and the real tab path
    -- (tabUnitName + pixel-auto-sized tabs) never called either one.
    describe "truncation call sites (#1189 audit)" $ do
        it "the dead tab truncators and their caps are gone from both log panels" $ \_ → do
            sources ← mapM TIO.readFile
                [ "scripts/combat_log.lua"
                , "scripts/injury_log_panel.lua"
                ]
            mapM_ (\src → do
                      T.isInfixOf "truncateTabName" src `shouldBe` False
                      T.isInfixOf "tabNameMax" src `shouldBe` False
                      -- The comment describing the deleted cap goes with it.
                      T.isInfixOf "12-char display cap" src `shouldBe` False)
                  sources
        it "both live truncators delegate their boundaries to utf8_safe" $ \_ → do
            engineSrc ← TIO.readFile "scripts/unit_info_v2_panel_engine.lua"
            craftSrc  ← TIO.readFile "scripts/crafting_panel.lua"
            mapM_ (\src →
                      T.isInfixOf "require(\"scripts.ui.utf8_safe\")" src
                          `shouldBe` True)
                  [engineSrc, craftSrc]
            -- The byte-count walk and the byte-count cap are both gone.
            T.isInfixOf "n = #text" engineSrc `shouldBe` False
            T.isInfixOf "#text <= maxChars" craftSrc `shouldBe` False

-- | A Lua backend with the real panel modules loaded and a deterministic,
-- UTF-8-validating, call-recording width function.
truncBackend ∷ EngineEnv → IO LuaBackendState
truncBackend env = do
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
        , "_G.__cp=require('scripts.crafting_panel');"
        , "_G.__pe=require('scripts.unit_info_v2_panel_engine');"
        -- 22 characters wide in each width class, so a cap of 20 always cuts.
        , "_G.__cjk=string.rep('界',22);"
        , "_G.__emoji=string.rep('🙂',22);"
        -- One-, two-, three- and four-byte characters interleaved: 12 code
        -- points over 30 bytes, so a byte walk and a character walk disagree
        -- at almost every position.
        , "_G.__mixed='aé界🙂Bé界🙂cé界🙂';"
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
