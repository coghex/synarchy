-- | #1914 gate: scripts/ui/bar.lua's public recolor API,
--   @bar.setFillColor@, must reach a verb the engine actually
--   registers. It used to call @UI.setSpriteColor@, which no
--   'registerLuaFunction' installs anywhere under
--   src/Engine/Scripting/Lua/API/, so every call raised
--   @attempt to call a nil value@; the registered verb taking that same
--   @(elementHandle, r, g, b, a)@ argument list is 'UI.setColor', which
--   dispatches on the element's render-data variant and reaches
--   'UI.Manager.Property.setSpriteColor' for a sprite.
--
--   Boots a real headless Lua backend with the full production
--   'registerLuaAPI' surface and nothing preloaded — the same
--   device-free widget pattern 'Test.Headless.UI.Slider' uses — so
--   nothing here defines 'UI.setSpriteColor' or stands in for
--   'UI.setColor'. Reverting the call-site fix therefore reproduces the
--   nil-function failure rather than passing vacuously.
--
--   'UI.getElementInfo' deliberately exposes no colour field, so the
--   oracle is the engine's own render state: the bar's real @<name>_fl@
--   and @<name>_fc@ elements are located by name in the live
--   'UIPageManager' and their 'ussColor' read directly.
module Test.Headless.UI.BarFillColor (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import UI.Types ( UIElement(..), UIRenderData(..), UISpriteStyle(..)
                , UIPageManager(..) )
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

spec ∷ Spec
spec = around withBarEngine $
    describe "scripts/ui/bar.lua fill recolor (#1914)" $ do
        it "setFillColor recolors both fill sprites through a registered verb" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls barSetupLua
            setup `shouldNotSatisfy` isLuaError

            spriteColorByName env "test_bar_fl" ≫= (`shouldBe` Just initialFill)
            spriteColorByName env "test_bar_fc" ≫= (`shouldBe` Just initialFill)

            recolor ← evalDebug ls
                "require('scripts.ui.bar').setFillColor(_G.__barId, 0.75, 0.625, 0.5, 0.25)"
            recolor `shouldNotSatisfy` isLuaError

            spriteColorByName env "test_bar_fl" ≫= (`shouldBe` Just recoloredFill)
            spriteColorByName env "test_bar_fc" ≫= (`shouldBe` Just recoloredFill)

        it "setFillColor on an unknown bar id returns early and touches nothing" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls barSetupLua
            setup `shouldNotSatisfy` isLuaError

            unknown ← evalDebug ls
                "require('scripts.ui.bar').setFillColor(999999, 0.75, 0.625, 0.5, 0.25)"
            unknown `shouldNotSatisfy` isLuaError

            spriteColorByName env "test_bar_fl" ≫= (`shouldBe` Just initialFill)
            spriteColorByName env "test_bar_fc" ≫= (`shouldBe` Just initialFill)

-- | Engine init is itself a @config/@ writer, so isolation is
--   established around it, never inside (#1357). No world page is
--   needed here, so the world worker never has to start.
withBarEngine ∷ (EngineEnv → IO α) → IO α
withBarEngine action = withIsolatedResourceRoot (withHeadlessEngineNoWorld action)

-- | The bar's creation-time fill colour, and the colour the recolor
--   asks for. Both are exactly representable as 'Float', so the
--   comparison needs no tolerance.
initialFill, recoloredFill ∷ (Float, Float, Float, Float)
initialFill   = (0.125, 0.25, 0.375, 0.5)
recoloredFill = (0.75, 0.625, 0.5, 0.25)

-- * Real-Lua-backend helper (mirrors Test.Headless.UI.Slider's
--   newBareLuaBackend — a real Lua backend with the full Lua API
--   registered and nothing preloaded, so scripts/ui/bar.lua is pulled
--   in the same way any real caller's @require@ would).

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

-- | The colour the engine will actually render the uniquely named
--   sprite element with. 'Nothing' when no element carries that name,
--   when more than one does, or when the one that does is not a sprite.
spriteColorByName ∷ EngineEnv → Text → IO (Maybe (Float, Float, Float, Float))
spriteColorByName env name = do
    mgr ← readIORef (uiManagerRef env)
    let matches = filter ((≡ name) . ueName) (Map.elems (upmElements mgr))
    pure $ case matches of
        [elem] → case ueRenderData elem of
            RenderSprite style → Just (ussColor style)
            _                  → Nothing
        _ → Nothing

-- | Creates a shown page and builds one real bar on it through the
--   production module. Synthetic texture handles keep this GPU-free
--   ('UI.newSprite' only stores the handle), an explicit @uiscale@
--   avoids depending on the engine's configured scale, and a non-zero
--   @progress@ keeps both fill sprites in the visible state a live
--   caller would recolor them in.
barSetupLua ∷ Text
barSetupLua = T.concat
    [ "local page = UI.newPage('test_bar_page', 'hud'); "
    , "UI.showPage(page); "
    , "local barMod = require('scripts.ui.bar'); "
    , "_G.__barId = barMod.new({name='test_bar', page=page, x=0, y=0, "
    , "width=200, height=24, capWidth=8, uiscale=1, progress=0.5, "
    , "trackLeftTex=1, trackCenterTex=1, trackRightTex=1, "
    , "fillLeftTex=1, fillCenterTex=1, "
    , "fillColor={0.125, 0.25, 0.375, 0.5}})"
    ]
