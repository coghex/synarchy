-- | #2636: a typed edit in a focused editable dropdown must be
--   COMMITTED when focus is lost by pointer, not silently discarded.
--
--   @scripts\/ui\/dropdown.lua@'s @onClickOutside@ always submitted a
--   pending edit, but nothing reached it: the input thread queues
--   'Engine.Scripting.Lua.Types.LuaUIFocusLost' BEFORE the outside
--   @LuaMouseDownEvent@ on a left-click miss
--   ('Engine.Input.Thread.Mouse'), and @uiManager.onUIFocusLost@
--   answered it with @dropdown.unfocusAll@, which overwrites the raw
--   edit with the selected option's text. Clicking another control lost
--   the edit the same way, through @ui_manager_widgets@'s shared
--   @handleNonTextBoxClick@ cleanup, which runs before the clicked
--   control's own callback — so typing a resolution and pressing Apply
--   applied the OLD one.
--
--   Every example here therefore drives the REAL dispatch order rather
--   than calling @dropdown.onClickOutside@ directly (which was green
--   throughout the defect): @manager.onUIFocusLost()@ followed by the
--   outside click, and @manager.onButtonClick@ /
--   @manager.onCheckboxClick@ / @manager.onDropdownOptionClick@ with
--   real widget element handles.
--
--   Reachable on its own as
--   @--match \"dropdown commit on focus loss\"@.
--
--   The full @ui_manager@ boot never runs headless (it gates on
--   @fontsReady@, which needs a GPU font atlas — see @scripts\/CLAUDE.md@),
--   so the fixture seeds the @scripts.ui_manager@ singleton itself and
--   requires the two routing submodules that own the routes under test
--   (@ui_manager_widgets@, @ui_manager_input@) onto it. Only the HUD and
--   the settings menu are stubbed; the dropdown, button, checkbox and
--   routing modules are the production ones.
module Test.Headless.UI.DropdownCommit (spec) where

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
import UI.ShellFocus (createFocusManager)
import UI.Types (emptyUIPageManager)

spec ∷ Spec
spec = around withHeadlessEngine $
    describe "dropdown commit on focus loss (#2636)" $ do

        it "commits a valid typed edit when focus is lost to empty space, \
           \firing onChange exactly once" $ \env → do
            ls ← dropdownBackend env
            eval ls
                "dropdown.focus(__dd); dropdown.setRawText(__dd,'1920x1080'); \
                \manager.onUIFocusLost(); dropdown.onClickOutside(500,500); \
                \return __changed..'|'..dropdown.getValue(__dd) \
                \..'|'..tostring(__lastValue)..'|'..dropdown.getRawText(__dd)"
                `shouldReturn` "1|1920x1080|1920x1080|1920x1080"

        it "commits the edit BEFORE a real button's own callback runs, so an \
           \Apply-style handler reads the new selection" $ \env → do
            ls ← dropdownBackend env
            eval ls
                "_G.__seen='unset'; \
                \local b=button.new({name='apply',page=__page,font=1,uiscale=1, \
                \  x=0,y=300,text='Apply', \
                \  onClick=function() __seen=dropdown.getValue(__dd) end}); \
                \dropdown.focus(__dd); dropdown.setRawText(__dd,'1920x1080'); \
                \manager.onButtonClick(button.getElementHandle(b)); \
                \return __changed..'|'..__seen..'|'..dropdown.getValue(__dd)"
                `shouldReturn` "1|1920x1080|1920x1080"

        it "commits the edit BEFORE a real checkbox's own callback runs" $ \env → do
            ls ← dropdownBackend env
            eval ls
                "_G.__seen='unset'; \
                \local c=checkbox.new({name='vsync',page=__page,uiscale=1, \
                \  default=false, \
                \  onChange=function() __seen=dropdown.getValue(__dd) end}); \
                \dropdown.focus(__dd); dropdown.setRawText(__dd,'1920x1080'); \
                \manager.onCheckboxClick(checkbox.getElementHandle(c)); \
                \return __changed..'|'..__seen..'|'..dropdown.getValue(__dd) \
                \..'|'..tostring(checkbox.isChecked(c))"
                `shouldReturn` "1|1920x1080|1920x1080|true"

        it "still cancels the edit on Escape, restoring the previous selection \
           \without firing onChange" $ \env → do
            ls ← dropdownBackend env
            eval ls
                "dropdown.focus(__dd); dropdown.setRawText(__dd,'1920x1080'); \
                \local handled=manager.onUIEscape(); \
                \return tostring(handled)..'|'..__changed..'|'..dropdown.getValue(__dd) \
                \..'|'..dropdown.getRawText(__dd)"
                `shouldReturn` "true|0|1280x720|1280x720"

        it "still reverts a typed edit that matches no option, without firing \
           \onChange" $ \env → do
            ls ← dropdownBackend env
            eval ls
                "dropdown.focus(__dd); dropdown.setRawText(__dd,'not-a-mode'); \
                \manager.onUIFocusLost(); dropdown.onClickOutside(500,500); \
                \return __changed..'|'..dropdown.getValue(__dd) \
                \..'|'..dropdown.getRawText(__dd)"
                `shouldReturn` "0|1280x720|1280x720"

        it "commits once across repeated focus-loss and outside dispatch" $ \env → do
            ls ← dropdownBackend env
            eval ls
                "dropdown.focus(__dd); dropdown.setRawText(__dd,'1920x1080'); \
                \for _=1,3 do manager.onUIFocusLost(); \
                \  dropdown.onClickOutside(500,500) end; \
                \return __changed..'|'..dropdown.getValue(__dd)"
                `shouldReturn` "1|1920x1080"

        it "selects the CLICKED option exactly once when the pending text \
           \matches a different option" $ \env → do
            ls ← dropdownBackend env
            eval ls
                "dropdown.openList(__dd); dropdown.focus(__dd); \
                \dropdown.setRawText(__dd,'1920x1080'); \
                \local target=nil; \
                \for h=1,400 do local id,act,slot=dropdown.findByElementHandle(h); \
                \  if id==__dd and act=='option' and slot==2 then target=h end end; \
                \local found=(target~=nil); \
                \manager.onDropdownOptionClick(target); \
                \return tostring(found)..'|'..__changed..'|'..dropdown.getValue(__dd) \
                \..'|'..tostring(__lastValue)"
                `shouldReturn` "true|1|1600x900|1600x900"

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

-- | The production dropdown\/button\/checkbox modules and the two real
--   @ui_manager@ routing submodules, on the real UI and text-input API.
--
--   @scripts.ui_manager@ is seeded directly rather than required: the
--   entry module pulls in the whole boot\/menu\/panel tree, none of which
--   participates in these routes, and its own header documents that the
--   submodules attach their callbacks onto whatever table
--   @package.loaded[\"scripts.ui_manager\"]@ already holds. @currentMenu@
--   is what @onUIEscape@ branches on, so it is seeded to the production
--   default.
dropdownBackend ∷ EngineEnv → IO LuaBackendState
dropdownBackend env = do
    ls ← newBackend env
    setup ← eval ls $ T.unwords
        [ "_G.__changed=0; _G.__lastValue=nil;"
        , "engine.getTextWidth=function(_,t,_) return #t*10 end;"
        , "engine.loadTexture=function() return 1 end;"
        , "engine.getUIScale=function() return 1 end;"
        , "local bt=UI.loadBoxTextures(1,1,1,1,1,1,1,1,1);"
        , "package.loaded['scripts.ui.box_textures']={load=function() return bt end};"
        , "package.loaded['scripts.hud']={visible=false,"
        , "  onMouseDown=function() end};"
        , "package.loaded['scripts.settings_menu']={"
        , "  isCapturingKey=function() return false end,"
        , "  cancelKeyCapture=function() end,"
        , "  onTextBoxSubmit=function() end};"
        , "package.loaded['scripts.ui_manager']={currentMenu='main',"
        , "  moduleReady={},fbW=1280,fbH=720};"
        , "_G.__page=UI.newPage('dd_commit','hud'); UI.showPage(__page);"
        , "_G.dropdown=require('scripts.ui.dropdown'); dropdown.init();"
        , "_G.button=require('scripts.ui.button'); button.init();"
        , "_G.checkbox=require('scripts.ui.checkbox'); checkbox.init();"
        , "require('scripts.ui_manager_widgets');"
        , "require('scripts.ui_manager_input');"
        , "_G.manager=package.loaded['scripts.ui_manager'];"
        , "_G.__dd=dropdown.new({name='resolution',page=__page,font=1,"
        , "  uiscale=1,x=0,y=0,options={"
        , "    {value='1280x720',text='1280x720'},"
        , "    {value='1600x900',text='1600x900'},"
        , "    {value='1920x1080',text='1920x1080'}},"
        , "  default='1280x720',"
        , "  onChange=function(v) __changed=__changed+1; __lastValue=v end});"
        , "return 'ready'"
        ]
    setup `shouldBe` "ready"
    pure ls

eval ∷ LuaBackendState → T.Text → IO T.Text
eval ls code = do
    out ← executeDebugLua (lbsLuaState ls) code
    -- executeDebugLua JSON-serializes string return values; these
    -- fixtures avoid quotes and backslashes entirely, so stripping the
    -- outer pair keeps the expectations readable.
    pure $ if T.length out ≥ 2 ∧ T.head out ≡ '"' ∧ T.last out ≡ '"'
        then T.dropEnd 1 (T.drop 1 out)
        else out
