{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #814 gate: scripts/ui/slider.lua must expose ONE authoritative
--   @slider.findByElementHandle@ that recognizes all four assembly
--   handles (left cap, track, knob, right cap) and resolves each to the
--   owning slider id, while hover highlighting stays gated to the
--   track+knob targets only (caps must never become hover-interactive).
--   Boots a real headless Lua backend and drives the real
--   scripts/ui/slider.lua module through the production
--   loadstring+pcall debug-console primitive ('executeDebugLua') — the
--   same technique 'Test.Headless.UI.InputOwnership' uses for
--   scripts/debug.lua. 'UI.newSprite'/'engine.loadTexture' are pure
--   CPU-side (no Vulkan/window dependency), so this needs no GPU.
module Test.Headless.UI.Slider (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)

spec ∷ Spec
spec = around withHeadlessEngine $
    describe "scripts/ui/slider.lua element lookup (#814)" $ do
        it "findByElementHandle resolves every assembly handle to the owning slider id" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls sliderSetupLua
            setup `shouldNotSatisfy` isLuaError

            evalDebug ls "require('scripts.ui.slider').findByElementHandle(_G.__leftCapId) == _G.__sliderId"
                ≫= (`shouldBe` "true")
            evalDebug ls "require('scripts.ui.slider').findByElementHandle(_G.__trackId) == _G.__sliderId"
                ≫= (`shouldBe` "true")
            evalDebug ls "require('scripts.ui.slider').findByElementHandle(_G.__knobId) == _G.__sliderId"
                ≫= (`shouldBe` "true")
            evalDebug ls "require('scripts.ui.slider').findByElementHandle(_G.__rightCapId) == _G.__sliderId"
                ≫= (`shouldBe` "true")
            evalDebug ls "require('scripts.ui.slider').findByElementHandle(999999) == nil"
                ≫= (`shouldBe` "true")

        it "hovering the track or knob raises the highlight, and clears on leave" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls sliderSetupLua
            setup `shouldNotSatisfy` isLuaError

            evalDebug ls "UI.getElementInfo(_G.__highlightId).visible" ≫= (`shouldBe` "false")

            enterTrack ← evalDebug ls "require('scripts.ui.slider').onHoverEnter(_G.__trackId)"
            enterTrack `shouldNotSatisfy` isLuaError
            evalDebug ls "UI.getElementInfo(_G.__highlightId).visible" ≫= (`shouldBe` "true")

            leaveTrack ← evalDebug ls "require('scripts.ui.slider').onHoverLeave(_G.__trackId)"
            leaveTrack `shouldNotSatisfy` isLuaError
            evalDebug ls "UI.getElementInfo(_G.__highlightId).visible" ≫= (`shouldBe` "false")

            enterKnob ← evalDebug ls "require('scripts.ui.slider').onHoverEnter(_G.__knobId)"
            enterKnob `shouldNotSatisfy` isLuaError
            evalDebug ls "UI.getElementInfo(_G.__highlightId).visible" ≫= (`shouldBe` "true")

            leaveKnob ← evalDebug ls "require('scripts.ui.slider').onHoverLeave(_G.__knobId)"
            leaveKnob `shouldNotSatisfy` isLuaError
            evalDebug ls "UI.getElementInfo(_G.__highlightId).visible" ≫= (`shouldBe` "false")

        it "hovering a cap does not raise the highlight, even though findByElementHandle owns it" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls sliderSetupLua
            setup `shouldNotSatisfy` isLuaError

            enterLeft ← evalDebug ls "require('scripts.ui.slider').onHoverEnter(_G.__leftCapId)"
            enterLeft `shouldNotSatisfy` isLuaError
            evalDebug ls "UI.getElementInfo(_G.__highlightId).visible" ≫= (`shouldBe` "false")

            enterRight ← evalDebug ls "require('scripts.ui.slider').onHoverEnter(_G.__rightCapId)"
            enterRight `shouldNotSatisfy` isLuaError
            evalDebug ls "UI.getElementInfo(_G.__highlightId).visible" ≫= (`shouldBe` "false")

            evalDebug ls "UI.getElementInfo(_G.__leftCapId).clickable" ≫= (`shouldBe` "false")
            evalDebug ls "UI.getElementInfo(_G.__rightCapId).clickable" ≫= (`shouldBe` "false")
            evalDebug ls "UI.getElementInfo(_G.__trackId).clickable" ≫= (`shouldBe` "true")
            evalDebug ls "UI.getElementInfo(_G.__knobId).clickable" ≫= (`shouldBe` "true")

        it "preserves visibility and destruction across all four handles plus the highlight" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls sliderSetupLua
            setup `shouldNotSatisfy` isLuaError

            hideCall ← evalDebug ls "require('scripts.ui.slider').setVisible(_G.__sliderId, false)"
            hideCall `shouldNotSatisfy` isLuaError
            evalDebug ls "UI.getElementInfo(_G.__leftCapId).visible" ≫= (`shouldBe` "false")
            evalDebug ls "UI.getElementInfo(_G.__trackId).visible" ≫= (`shouldBe` "false")
            evalDebug ls "UI.getElementInfo(_G.__rightCapId).visible" ≫= (`shouldBe` "false")
            evalDebug ls "UI.getElementInfo(_G.__knobId).visible" ≫= (`shouldBe` "false")
            evalDebug ls "UI.getElementInfo(_G.__highlightId).visible" ≫= (`shouldBe` "false")

            showCall ← evalDebug ls "require('scripts.ui.slider').setVisible(_G.__sliderId, true)"
            showCall `shouldNotSatisfy` isLuaError
            evalDebug ls "UI.getElementInfo(_G.__trackId).visible" ≫= (`shouldBe` "true")

            destroyCall ← evalDebug ls "require('scripts.ui.slider').destroy(_G.__sliderId)"
            destroyCall `shouldNotSatisfy` isLuaError
            evalDebug ls "UI.getElementInfo(_G.__leftCapId) == nil" ≫= (`shouldBe` "true")
            evalDebug ls "UI.getElementInfo(_G.__trackId) == nil" ≫= (`shouldBe` "true")
            evalDebug ls "UI.getElementInfo(_G.__rightCapId) == nil" ≫= (`shouldBe` "true")
            evalDebug ls "UI.getElementInfo(_G.__knobId) == nil" ≫= (`shouldBe` "true")
            evalDebug ls "UI.getElementInfo(_G.__highlightId) == nil" ≫= (`shouldBe` "true")
            evalDebug ls "require('scripts.ui.slider').findByElementHandle(_G.__trackId) == nil"
                ≫= (`shouldBe` "true")

-- * Real-Lua-backend helper (mirrors Test.Headless.UI.InputOwnership's
--   newBareLuaBackend — a real Lua backend with the full Lua API
--   registered and nothing preloaded, so scripts/ui/slider.lua is
--   pulled in the same way any real caller's @require@ would).

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console itself uses.
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Creates a shown page, requires the real slider module, builds one
--   slider on it, and resolves every element's handle by name into
--   globals the assertions above read — 'UI.getVisibleElements()' is
--   the same bulk introspection read scripts/ui/registry.lua's
--   dumpWidgets() falls back to, so this doesn't assume anything about
--   handle numbering.
sliderSetupLua ∷ Text
sliderSetupLua = T.concat
    [ "local page = UI.newPage('test_slider_page', 'hud'); "
    , "UI.showPage(page); "
    , "local sliderMod = require('scripts.ui.slider'); "
    , "sliderMod.init(); "
    , "_G.__sliderId = sliderMod.new({name='test_slider', x=0, y=0, "
    , "width=200, height=24, capWidth=8, knobWidth=16, "
    , "min=0, max=100, default=50, page=page}); "
    , "for _, e in ipairs(UI.getVisibleElements()) do "
    , "  if e.name == 'test_slider_left' then _G.__leftCapId = e.handle end; "
    , "  if e.name == 'test_slider_track' then _G.__trackId = e.handle end; "
    , "  if e.name == 'test_slider_right' then _G.__rightCapId = e.handle end; "
    , "  if e.name == 'test_slider_knob' then _G.__knobId = e.handle end; "
    , "  if e.name == 'test_slider_hl' then _G.__highlightId = e.handle end; "
    , "end"
    ]
