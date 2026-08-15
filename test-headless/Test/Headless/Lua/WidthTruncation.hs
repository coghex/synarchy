-- | #1157: the ONE width-bounded truncator
-- (@textWrap.truncateToWidth@ in @scripts/ui/text_wrap.lua@) and the five
-- surfaces that reach it.
--
-- Five modules each carried a private binary search over the cut length,
-- and they disagreed three ways beneath that: two ellipsis forms
-- (@\"...\"@ in the popup and the event log, @\"..\"@ in the three
-- inventory panels), three guard sets, and two width measurements (the
-- candidate WITH its ellipsis versus the prefix plus a precomputed
-- ellipsis width). One operation, visibly different results, in the same
-- UI.
--
-- These run the REAL shared module; a copied algorithm would say nothing
-- about the surfaces. The only stub is 'engine.getTextWidth', which
-- headless returns 0 for: here it measures ten pixels per code point (so
-- truncation actually happens), REFUSES any argument that isn't valid
-- UTF-8 (so every intermediate candidate is checked, not just the
-- returned one), and records every call — which is what lets
-- \"measured one way, ellipsis included\" be a measured fact rather than
-- a source grep.
module Test.Headless.Lua.WidthTruncation (spec) where

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

        describe "the width stub itself" $ do
            it "rejects a byte fragment, so the sweeps below are load-bearing" $ \env → do
                ls ← truncBackend env
                -- '界' is E7 95 8C; measuring its lead byte alone must
                -- raise. Without this, a helper that never produced a
                -- fragment and a guard that never fired would look the
                -- same from here.
                eval ls "local ok = pcall(engine.getTextWidth, nil, \
                        \string.sub('界', 1, 1), 1); return tostring(ok)"
                    `shouldReturn` "false"

        -- Requirement 3 as the issue review pinned it, case by case. Each
        -- of these was guarded by some subset of the five private copies
        -- and by none of the others.
        describe "the defensive contract" $ do
            it "returns nil and empty input unchanged, whatever the width" $ \env → do
                ls ← truncBackend env
                eval ls
                    "local f=__tw.truncateToWidth; \
                    \return tostring(f(nil,1,10,100))..'|'..tostring(f(nil,1,10,0)) \
                    \..'|['..f('',1,10,100)..']['..f('',1,10,-5)..']'"
                    `shouldReturn` "nil|nil|[][]"

            it "returns text that already fits unchanged, including at the exact boundary" $ \env → do
                ls ← truncBackend env
                -- 10px per code point: 'abc' is exactly 30px, and one
                -- pixel less fits neither 'a..' (30px) nor 'ab..'.
                eval ls
                    "local f=__tw.truncateToWidth; \
                    \return f('abc',1,10,100)..'|'..f('abc',1,10,30) \
                    \..'|'..f('abc',1,10,29)"
                    `shouldReturn` "abc|abc|.."

            it "drops the field on a non-positive width, never drawing it full width" $ \env → do
                ls ← truncBackend env
                -- The one case where #1157 changed #1107's answer: a
                -- collapsed column used to come back with the ORIGINAL
                -- text, which is drawn across whatever sits beside it.
                -- A budget of zero cannot fit the ellipsis either, so the
                -- uniform rule and the unfittable-ellipsis rule agree.
                eval ls
                    "local f=__tw.truncateToWidth; \
                    \return '['..f('abcdef',1,10,0)..']['..f('abcdef',1,10,-50)..']'"
                    `shouldReturn` "[][]"

            it "treats a nil width as no bound at all" $ \env → do
                ls ← truncBackend env
                -- Distinct from zero: a caller that passed no bound gets
                -- its text, a caller that measured a bound of nothing
                -- gets nothing.
                eval ls "return __tw.truncateToWidth('abcdef',1,10,nil)"
                    `shouldReturn` "abcdef"

            it "returns \"\" when the ellipsis alone does not fit, and the ellipsis when no character does" $ \env → do
                ls ← truncBackend env
                -- ".." is 20px. 19px fits nothing at all; 20-29px fits the
                -- dots but not one 10px character; the pre-#1157 lo = 1
                -- searches returned an over-width first character here.
                eval ls
                    "local f=__tw.truncateToWidth; \
                    \return '['..f('abcdef',1,10,19)..']['..f('abcdef',1,10,20) \
                    \..']['..f('abcdef',1,10,29)..']['..f('abcdef',1,10,30)..']'"
                    `shouldReturn` "[][..][..][a..]"

            it "keeps the longest fitting prefix at every width across a sweep" $ \env → do
                ls ← truncBackend env
                -- 'abcdefghij' is 100px. Every result must itself fit,
                -- and the kept prefix must grow by exactly one character
                -- per 10px of budget.
                eval ls
                    "local f=__tw.truncateToWidth; local out={}; \
                    \for px=20,100,10 do out[#out+1]=f('abcdefghij',1,10,px) end; \
                    \return table.concat(out,'/')"
                    `shouldReturn`
                    "../a../ab../abc../abcd../abcde../abcdef../abcdefg../abcdefghij"

            it "never returns a result wider than the budget it was given" $ \env → do
                ls ← truncBackend env
                -- The property behind the pinned sweep above, over a
                -- mixed-width string at every pixel budget from 0 to 200.
                eval ls
                    "local f=__tw.truncateToWidth; local over=0; \
                    \for px=0,200 do local r=f(__mixed,1,10,px); \
                    \  if engine.getTextWidth(1,r,10)>px then over=over+1 end end; \
                    \return tostring(over)"
                    `shouldReturn` "0"

        -- Requirement 4. The three-byte and four-byte cases are the ones a
        -- byte-offset cut mangles most often, and the stub raises on any
        -- fragment, so completing a sweep at all is the proof.
        describe "UTF-8 boundary safety" $ do
            it "cuts two-, three- and four-byte characters whole, at every width" $ \env → do
                ls ← truncBackend env
                eval ls
                    "local f=__tw.truncateToWidth; local bad=0; \
                    \for _,s in ipairs({__two,__three,__four,__mixed}) do \
                    \  for px=0,200 do local r=f(s,1,10,px); \
                    \    if utf8.len(r)==nil then bad=bad+1 end end end; \
                    \return tostring(bad)"
                    `shouldReturn` "0"

            it "keeps whole characters at each cut of a mixed-width string" $ \env → do
                ls ← truncBackend env
                -- 12 code points over 30 bytes, so a byte cut and a
                -- character cut disagree at almost every position. The
                -- last two entries are the whole string: 12 code points
                -- is 120px, so that is where it stops being truncated.
                eval ls
                    "local f=__tw.truncateToWidth; local out={}; \
                    \for px=20,130,10 do out[#out+1]=f(__mixed,1,10,px) end; \
                    \return table.concat(out,'/')"
                    `shouldReturn`
                    "../a../aé../aé界../aé界🙂../aé界🙂B../aé界🙂Bé../aé界🙂Bé界..\
                    \/aé界🙂Bé界🙂../aé界🙂Bé界🙂c../aé界🙂Bé界🙂cé界🙂\
                    \/aé界🙂Bé界🙂cé界🙂"

        -- Requirement 5. Four of the five private copies measured the
        -- candidate with its ellipsis and one measured the prefix plus a
        -- precomputed ellipsis width; those are the same number only
        -- while the font path applies no kerning.
        describe "one measurement rule" $ do
            it "measures complete candidates only, ellipsis included" $ \env → do
                ls ← truncBackend env
                -- Every recorded argument is either the full input (the
                -- already-fits pre-check) or a candidate ending in "..".
                -- A bare prefix measurement would appear here as neither.
                eval ls
                    "__measured={}; __tw.truncateToWidth('abcdefghij',1,10,55); \
                    \local bare=0; \
                    \for _,m in ipairs(__measured) do \
                    \  if m~='abcdefghij' and m:sub(-2)~='..' then bare=bare+1 end end; \
                    \return tostring(bare)..'|'..tostring(#__measured>1)"
                    `shouldReturn` "0|true"

            it "measures the ellipsis-alone candidate rather than an ellipsis width to add" $ \env → do
                ls ← truncBackend env
                -- The unfittable-ellipsis pre-check is itself the cut = 0
                -- candidate, so it is a complete candidate too.
                eval ls
                    "__measured={}; local r=__tw.truncateToWidth('abcdef',1,10,19); \
                    \return '['..r..']|'..table.concat(__measured,'/')"
                    `shouldReturn` "[]|abcdef/.."

    -- The five hosts are what the issue is actually about; pinning the
    -- delegation is what makes the coverage above theirs. Three of them
    -- reach the helper through #1088's shared item-list widget, which is
    -- exactly the arrangement the issue anticipated.
    describe "truncation call sites (#1157 audit)" $ do
        it "no host carries a private width-bounded truncator any more" $ do
            sources ← mapM TIO.readFile
                [ "scripts/popup.lua"
                , "scripts/event_log.lua"
                , "scripts/unit_info_v2_inventory.lua"
                , "scripts/item_contents_panel.lua"
                , "scripts/cargo_inventory_panel.lua"
                ]
            mapM_ (\src → do
                      T.isInfixOf "function truncateToWidth" src `shouldBe` False
                      -- The stale comment describing an algorithm the
                      -- cargo panel never ran goes with the copy.
                      T.isInfixOf "Drop one character at a time" src `shouldBe` False)
                  sources
        it "the two direct hosts route to the shared module" $ do
            sources ← mapM TIO.readFile
                [ "scripts/popup.lua", "scripts/event_log.lua" ]
            mapM_ (\src → do
                      T.isInfixOf "require(\"scripts.ui.text_wrap\")" src `shouldBe` True
                      T.isInfixOf "textWrap.truncateToWidth" src `shouldBe` True
                      -- Neither appends an ellipsis of its own any more;
                      -- both used to append the three-dot form the
                      -- inventory panels never rendered.
                      T.isInfixOf ".. \"...\"" src `shouldBe` False
                      T.isInfixOf ".. \"..\"" src `shouldBe` False)
                  sources
            -- And the surviving form is decided in exactly one place.
            shared ← TIO.readFile "scripts/ui/text_wrap.lua"
            T.isInfixOf "local ELLIPSIS = \"..\"" shared `shouldBe` True
        it "the three inventory hosts route through the shared item-list widget" $ do
            sources ← mapM TIO.readFile
                [ "scripts/unit_info_v2_inventory.lua"
                , "scripts/item_contents_panel.lua"
                , "scripts/cargo_inventory_panel.lua"
                ]
            mapM_ (\src → T.isInfixOf "require(\"scripts.ui.item_list\")" src
                              `shouldBe` True)
                  sources
            widget ← TIO.readFile "scripts/ui/item_list.lua"
            T.isInfixOf "require(\"scripts.ui.text_wrap\")" widget `shouldBe` True
            T.isInfixOf "textWrap.truncateToWidth" widget `shouldBe` True

-- | A Lua backend with the real shared module loaded and a deterministic,
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
        , "_G.__tw=require('scripts.ui.text_wrap');"
        -- Two-, three- and four-byte characters on their own, so a cut
        -- inside each width class is exercised in isolation...
        , "_G.__two=string.rep('é',12);"
        , "_G.__three=string.rep('界',12);"
        , "_G.__four=string.rep('🙂',12);"
        -- ...and interleaved with ASCII: 12 code points over 30 bytes, so
        -- a byte walk and a character walk disagree almost everywhere.
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
