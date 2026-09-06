-- | The fixture and bare-Lua support both owners of the @UI.ResponsiveMenus@
--   hierarchy share: 'Test.Headless.UI.ResponsiveMenus' (the responsive
--   menu screens) and 'Test.Headless.UI.ResponsiveMenus.DebugConsole' (the
--   shell debug console's responsive lifecycle).
--
--   These live here rather than in either owner because the menus module is
--   the aggregate façade: it composes the console owner's cases into its own
--   @around withMenusEngine@ block, so a definition the console owner also
--   needs cannot sit in the module importing it without a cycle. There is
--   exactly one definition of each — neither owner carries a copy.
--
--   The full ui_manager boot sequence never reaches menu construction
--   headless — it gates on fontsReady, which only flips once the graphics
--   asset pipeline finishes loading a font, and that pipeline never runs
--   without a GPU (see CLAUDE.md's headless notes and
--   'Engine.Scripting.Lua.Message''s @whenGraphical@ gate on font loading).
--   So both owners boot each screen or script module directly on a bare Lua
--   backend with synthetic texture/font handles — the same technique
--   'Test.Headless.UI.InputOwnership' uses for scripts/debug.lua — rather
--   than going through uiManager.init().
module Test.Headless.UI.ResponsiveMenus.Fixture
    ( -- * Fixture
      withMenusEngine
    , normalizeUIScale
    , menusBaselineUIScale
      -- * Lua source assembly
    , luaLines
      -- * Lua backend + eval helpers
    , newBareLuaBackend
    , eval
    , isLuaError
    , evalOk
    , evalBool
    , evalInt
    , evalJSON
    , decodeProbe
    ) where

import UPrelude
import Test.Hspec (expectationFailure)
import Data.Aeson (FromJSON, decode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Shell (setupShellSandbox)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

-- | Join Lua statements/fragments with a single space — every multi-line
--   snippet in both owners is built this way instead of GHC string-gap
--   continuations, which are easy to get subtly wrong (a missing space
--   before a line-continuing backslash silently glues two tokens
--   together) in suites whose whole point is long inline Lua text.
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- | The canonical UI scale every example in this suite starts from.
--   Matches the tracked @config/video_default.yaml@'s @ui_scale@, so a
--   machine with no local overlay sees no behavioral change at all.
menusBaselineUIScale ∷ Float
menusBaselineUIScale = 1.0

-- | Pin the engine's in-memory UI scale to 'menusBaselineUIScale',
--   preserving every other 'VideoConfig' field exactly as engine
--   initialization resolved it (#1266).
--
--   This is an in-memory mutation ONLY — the same narrow one
--   @engine.setUIScale@ performs ('Engine.Scripting.Lua.API.Config'),
--   whose persistence is a separate @saveVideoConfig@ call this suite
--   never makes.
--
--   That fact alone never made the whole SUITE non-writing, and #1357
--   found the gap: three examples (two in
--   'Test.Headless.UI.ResponsiveMenus', one in
--   'Test.Headless.UI.ResponsiveGameplay') drive the real
--   @settingsMenu.onDefaults()@, whose keybind reset is write-through
--   by contract — it called the production @engine.saveKeybinds()@ and
--   silently replaced the developer's @config/keybinds.local.yaml@.
--   What guarantees #1266's \"tests never modify, truncate or
--   regenerate @config/*.local.yaml@\" is now the filesystem boundary
--   in 'withMenusEngine' below, not this function.
normalizeUIScale ∷ EngineEnv → IO ()
normalizeUIScale env =
    atomicModifyIORef' (videoConfigRef env) $ \c →
        (c { vcUIScale = menusBaselineUIScale }, ())

-- | Every example here runs against its own freshly booted headless
--   engine, and 'Engine.Core.Init' populates that engine's
--   'videoConfigRef' from the developer's @config/video.local.yaml@
--   when one exists, falling back to @config/video_default.yaml@
--   otherwise (#638/#786's local-overlay contract, which is correct
--   and out of scope here). Without this wrapper an example's
--   effective UI scale is therefore whatever the developer last saved
--   from the Settings menu, and cases whose geometry assertions were
--   written against an implicit 1x flip verdict on a machine carrying
--   @ui_scale: 1.5@ — two of them did (#1266).
--
--   So establish the canonical baseline BEFORE the example body runs,
--   which is before any of its Lua modules or menu geometry
--   initialize. Cases that intentionally exercise a different scale
--   are unaffected: they already state it themselves with an explicit
--   @engine.setUIScale(...)@ as their first Lua statement, which
--   overrides this baseline exactly as it overrode the inherited
--   value before.
--
--   'Test.Headless.UI.ResponsiveGameplay.resetFixture' does the same
--   normalization for the same reason; it just folds it into a
--   shared-fixture reset rather than a per-example wrapper, because
--   that suite shares one engine across its cases.
--
--   The wrapper ALSO establishes #1357's filesystem boundary, and does
--   so OUTSIDE 'withHeadlessEngine': engine initialization is itself a
--   config writer (see 'Test.Headless.Harness.Isolation'), so isolating
--   only after the engine came up would already be too late.
withMenusEngine ∷ (EngineEnv → IO α) → IO α
withMenusEngine action = withIsolatedResourceRoot $
    withHeadlessEngine $ \env → do
        normalizeUIScale env
        action env

-- * Lua backend + eval helpers (mirrors Test.Headless.UI.InputOwnership)

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    -- Production order (Engine.Scripting.Lua.Thread.luaStartup): register
    -- the API, then build the console sandbox. Since #1958 that sandbox is
    -- also where scripts/shell.lua looks for completion candidates, so the
    -- two debug-console cases that need a live ghost
    -- ('Test.Headless.UI.ResponsiveMenus.DebugConsole') have nothing to
    -- complete against without it.
    setupShellSandbox (lbsLuaState ls)
    pure ls

eval ∷ LuaBackendState → Text → IO Text
eval ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls code = do
    t ← eval ls code
    when (isLuaError t) $ expectationFailure ("Lua error: " ⧺ T.unpack t)
    pure t

evalBool ∷ LuaBackendState → Text → IO Bool
evalBool ls code = do
    t ← evalOk ls code
    case t of
        "true"  → pure True
        "false" → pure False
        other   → do
            expectationFailure ("expected boolean, got: " ⧺ T.unpack other)
            pure False

evalInt ∷ LuaBackendState → Text → IO Int
evalInt ls code = do
    t ← evalOk ls code
    case reads (T.unpack t) of
        [(n, "")] → pure n
        _         → do
            expectationFailure ("expected integer, got: " ⧺ T.unpack t)
            pure 0

evalJSON ∷ LuaBackendState → Text → IO Text
evalJSON = evalOk

-- | Decode one JSON probe result, failing the example with the raw text
--   when it does not parse.
decodeProbe ∷ FromJSON α ⇒ String → Text → IO α
decodeProbe what r =
    maybe (fail ("failed to decode " ⧺ what ⧺ ": " ⧺ T.unpack r)) pure
          (decode (BL.fromStrict (TE.encodeUtf8 r)))
