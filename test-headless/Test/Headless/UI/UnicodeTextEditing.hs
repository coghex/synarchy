{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
-- | #746: the Lua editable-widget side of UI.TextBuffer's Unicode contract.
-- These tests use the real headless UI and text-input APIs plus the production
-- Lua widget modules. The only stubs are GPU asset loading and scrollbar
-- construction, neither of which participates in text editing.
module Test.Headless.UI.UnicodeTextEditing (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, writeIORef)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Focus (createFocusManager)
import UI.Types (emptyUIPageManager)

spec ∷ Spec
spec = around withHeadlessEngine $ do
    describe "shared code-point/byte conversion helper" $ do
        it "converts, clamps, and slices only at code-point boundaries" $ \env → do
            ls ← newBackend env
            eval ls
                "local u=require('scripts.ui.utf8_safe'); \
                \local mixed='A界🙂é'; \
                \return table.concat({u.codepointLength('ASCII'), \
                \u.codepointLength('café'), u.byteOffset('café',0), \
                \u.byteOffset('café',3), u.byteOffset('café',4), \
                \u.prefix('café',3), u.slice('café',1,4), \
                \u.codepointLength(mixed), u.slice(mixed,1,4), \
                \u.clampIndex(mixed,-7), u.clampIndex(mixed,99), \
                \u.snapToCharBoundary('café',4), \
                \u.snapToCharBoundary('café',5)}, '|')"
                `shouldReturn` "5|4|1|4|6|caf|afé|5|界🙂e|0|5|3|5"

    describe "textbox" $ do
        it "focuses at the true end and edits one code point at a time" $ \env → do
            ls ← widgetBackend env
            eval ls
                "_G.__tb=textbox.new({name='名字🙂',page=__page,font=1, \
                \width=36,default='A🙂界é',textType=textbox.Type.TEXT,uiscale=1}); \
                \textbox.focus(__tb); \
                \return textbox.getCursor(__tb)..'|'..textbox.getValue(__tb)..'|'..textbox.formatDisplayText(__tb)"
                `shouldReturn` "4|A🙂界é|A🙂界é"
            eval ls
                "textbox.onHome(); textbox.onCursorRight(); textbox.onCharInput('東'); \
                \textbox.onDelete(); textbox.onBackspace(); \
                \return textbox.getValue(__tb)..'|'..textbox.getCursor(__tb)"
                `shouldReturn` "A界é|1"
            eval ls
                "textbox.setCursor(__tb,-20); local lo=textbox.getCursor(__tb); \
                \textbox.setCursor(__tb,200); \
                \return lo..'|'..textbox.getCursor(__tb)"
                `shouldReturn` "0|3"
            eval ls
                "textbox.setText(__tb,''); textbox.focus(__tb); textbox.onBackspace(); \
                \textbox.onDelete(); textbox.onEnd(); \
                \return textbox.getValue(__tb)..'|'..textbox.getCursor(__tb)"
                `shouldReturn` "|0"

        it "measures whole prefixes, keeps suffixes outside the cursor, and clips whole code points" $ \env → do
            ls ← widgetBackend env
            eval ls
                "_G.__tb=textbox.new({name='clip',page=__page,font=1,width=36, \
                \default='A🙂界é',textType=textbox.Type.SCALE,uiscale=1}); \
                \textbox.focus(__tb); textbox.setCursor(__tb,2); \
                \local prefix=false; local clipped=false; local valid=true; \
                \for _,s in ipairs(__measured) do valid=valid and utf8.len(s)~=nil; \
                \if s=='A🙂' then prefix=true end end; \
                \for _,s in ipairs(__rendered) do valid=valid and utf8.len(s)~=nil; \
                \if s=='éx' then clipped=true end end; \
                \return tostring(prefix)..'|'..tostring(clipped)..'|'..tostring(valid)..'|'..textbox.getCursor(__tb)"
                `shouldReturn` "true|true|true|2"
            eval ls
                "_G.__long=textbox.new({name='long',page=__page,font=1,width=36, \
                \default=string.rep('界🙂é',20),textType=textbox.Type.TEXT,uiscale=1}); \
                \textbox.focus(__long); local found=false; local valid=true; \
                \for _,s in ipairs(__rendered) do valid=valid and utf8.len(s)~=nil; \
                \if s=='🙂é' then found=true end end; \
                \return tostring(found)..'|'..tostring(valid)..'|'..textbox.getCursor(__long)"
                `shouldReturn` "true|true|60"

    describe "editable dropdown" $ do
        it "uses code-point cursor prefixes and preserves Unicode filtering and selection" $ \env → do
            ls ← widgetBackend env
            eval ls
                "_G.__dd=dropdown.new({name='言語🙂',page=__page,font=1,uiscale=1, \
                \options={{value='tokyo',text='東京🙂'},{value='cafe',text='café'}, \
                \{value='combining',text='élan'}},default='tokyo'}); \
                \dropdown.focus(__dd); return UI.getCursor(dropdown.getElementHandle(__dd))"
                `shouldReturn` "3"
            eval ls
                "dropdown.onCursorLeft(); dropdown.onCharInput('é'); dropdown.onDelete(); \
                \dropdown.onBackspace(); local h=dropdown.getElementHandle(__dd); \
                \return UI.getTextInput(h)..'|'..UI.getCursor(h)"
                `shouldReturn` "東京|2"
            eval ls
                "local h=dropdown.getElementHandle(__dd); UI.setTextInput(h,'東京🙂'); \
                \UI.setCursor(h,2); dropdown.updateDisplay(__dd); local seen=false; \
                \for _,s in ipairs(__measured) do if s=='東京' then seen=true end end; \
                \local prefix=dropdown.findBestMatch({options={{value='c',text='café'}}},'caf'); \
                \local nonLatin=dropdown.findBestMatch({options={{value='t',text='東京🙂'}}},'東'); \
                \UI.setTextInput(h,'café'); dropdown.onSubmit(); \
                \return tostring(seen)..'|'..prefix..'|'..nonLatin..'|' \
                \..dropdown.getValue(__dd)..'|'..dropdown.getText(__dd)"
                `shouldReturn` "true|1|1|cafe|café"

    describe "randbox" $ do
        it "counts its maximum in code points and preserves surrounding Unicode while editing" $ \env → do
            ls ← widgetBackend env
            eval ls
                "_G.__rb=randbox.new({name='世界名🙂',page=__page,font=1,uiscale=1, \
                \randType=randbox.Type.NAME,default=string.rep('é',23)}); \
                \randbox.focus(__rb); randbox.onCharInput('A'); randbox.onCharInput('B'); \
                \local v=randbox.getValue(__rb); \
                \return utf8.len(v)..'|'..v:sub(-1)..'|'..UI.getCursor(randbox.getElementHandle(__rb))"
                `shouldReturn` "24|A|24"
            eval ls
                "randbox.setValue(__rb,'a🙂b'); local h=randbox.getElementHandle(__rb); \
                \UI.setCursor(h,1); randbox.onDelete(); randbox.onCharInput('Z'); \
                \return randbox.getValue(__rb)..'|'..UI.getCursor(h)"
                `shouldReturn` "aZb|2"

        it "leaves ASCII numeric, decimal, scale, and seed validation unchanged" $ \env → do
            ls ← widgetBackend env
            eval ls
                "local n=textbox.new({page=__page,font=1,default='12',textType=textbox.Type.NUMBER}); \
                \textbox.focus(n); textbox.onCharInput('é'); textbox.onCharInput('.'); textbox.onCharInput('3'); \
                \local nv=textbox.getValue(n); \
                \local d=textbox.new({page=__page,font=1,default='1.2',textType=textbox.Type.DECIMAL}); \
                \textbox.focus(d); textbox.onCharInput('.'); textbox.onCharInput('3'); \
                \local dv=textbox.getValue(d); \
                \local s=textbox.new({page=__page,font=1,default='2x',textType=textbox.Type.SCALE}); \
                \textbox.focus(s); textbox.onCharInput('.'); textbox.onCharInput('5'); \
                \local sv=textbox.getValue(s); \
                \local h=randbox.new({page=__page,font=1,uiscale=1,randType=randbox.Type.HEX_SEED,default='12'}); \
                \randbox.focus(h); randbox.onCharInput('G'); randbox.onCharInput('a'); \
                \local hv=randbox.getValue(h); \
                \return nv..'|'..dv..'|'..sv..'|'..hv..'|' \
                \..tostring(dropdown.numericValidator('7',''))..'|'..tostring(dropdown.numericValidator('é',''))"
                `shouldReturn` "123|1.23|2.5|12a|true|false"

    describe "state and introspection" $ do
        it "preserves exact Unicode through submit, focus, visibility, resize, rebuild, generation, and ui.dumpWidgets" $ \env → do
            ls ← widgetBackend env
            out ← eval ls
                "local submitted=nil; \
                \local t=textbox.new({name='欄🙂',page=__page,font=1,default='é東京🙂',uiscale=1}); \
                \textbox.focus(t); local ok,value=textbox.onSubmit(); submitted=value; \
                \textbox.setVisible(t,false); UI.setSize(textbox.getElementHandle(t),420,44); \
                \textbox.setPosition(t,17,19); textbox.setVisible(t,true); \
                \local saved=textbox.getValue(t); textbox.destroy(t); \
                \t=textbox.new({name='欄🙂',page=__page,font=1,default=saved,uiscale=1}); \
                \local changed=nil; local r=randbox.new({name='世界🙂',page=__page,font=1,uiscale=1, \
                \randType=randbox.Type.NAME,default='café',onChange=function(v) changed=v end}); \
                \randbox.randomize(r); randbox.focus(r); \
                \local generated=randbox.getValue(r); local generatedCursor=UI.getCursor(randbox.getElementHandle(r)); \
                \randbox.setValue(r,'東京🙂'); randbox.setVisible(r,false); randbox.setVisible(r,true); \
                \local d=dropdown.new({name='選択🙂',page=__page,font=1,uiscale=1, \
                \options={{value='v',text='café界🙂'}},default='v'}); \
                \ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                \local seenT,seenR,seenD=false,false,false; \
                \for _,w in ipairs(widgets) do \
                \if w.name=='欄🙂' and w.value=='é東京🙂' then seenT=true end; \
                \if w.name=='世界🙂' and w.value=='東京🙂' then seenR=true end; \
                \if w.name=='選択🙂' and w.label=='café界🙂' then seenD=true end end; \
                \return submitted..'|'..saved..'|'..generatedCursor..'|'..utf8.len(generated) \
                \..'|'..tostring(seenT)..'|'..tostring(seenR)..'|'..tostring(seenD)"
            out `shouldSatisfy` T.isPrefixOf "é東京🙂|é東京🙂|"
            out `shouldSatisfy` T.isSuffixOf "|true|true|true"

-- | Register the real Lua API against a fresh headless UI manager.
newBackend ∷ EngineEnv → IO LuaBackendState
newBackend env = do
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (focusManagerRef env) createFocusManager
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Production widgets on the real UI/text API. Width is deterministic:
-- every code point measures as ten pixels, and every measured/rendered string
-- is recorded so tests can prove no byte-fragment ever crosses that boundary.
widgetBackend ∷ EngineEnv → IO LuaBackendState
widgetBackend env = do
    ls ← newBackend env
    setup ← eval ls $ T.unwords
        [ "_G.__measured={}; _G.__rendered={};"
        , "local realSetText=UI.setText;"
        , "UI.setText=function(h,t) table.insert(__rendered,t); return realSetText(h,t) end;"
        , "engine.getTextWidth=function(_,t,_) local n,b=utf8.len(t);"
        , "assert(n,'invalid UTF-8 measured at byte '..tostring(b));"
        , "table.insert(__measured,t); return n*10 end;"
        , "engine.loadTexture=function() return 1 end;"
        , "engine.getUIScale=function() return 1 end;"
        , "local bt=UI.loadBoxTextures(1,1,1,1,1,1,1,1,1);"
        , "package.loaded['scripts.ui.box_textures']={load=function() return bt end};"
        , "package.loaded['scripts.ui.scrollbar']={init=function() end};"
        , "_G.__page=UI.newPage('unicode','hud'); UI.showPage(__page);"
        , "_G.textbox=require('scripts.ui.textbox'); textbox.init();"
        , "_G.dropdown=require('scripts.ui.dropdown'); dropdown.init();"
        , "_G.randbox=require('scripts.ui.randbox'); randbox.init();"
        , "return 'ready'"
        ]
    setup `shouldBe` "ready"
    pure ls

eval ∷ LuaBackendState → T.Text → IO T.Text
eval ls code = do
    out ← executeDebugLua (lbsLuaState ls) code
    -- executeDebugLua JSON-serializes string return values. These fixtures
    -- deliberately avoid quotes/backslashes in their compact result strings,
    -- so remove only that outer JSON pair for readable expectations.
    pure $ if T.length out ≥ 2 ∧ T.head out ≡ '"' ∧ T.last out ≡ '"'
        then T.dropEnd 1 (T.drop 1 out)
        else out
