-- | Shared fixture plumbing for the #750 responsive-gameplay gate
--   ('Test.Headless.UI.ResponsiveGameplay' and its owner-scoped
--   fragments, #2126). The facade is the ONLY caller of
--   'withSharedFixture': it wraps the whole aggregate in exactly one
--   @aroundAll withSharedFixture@, so the complete suite boots ONE
--   'EngineEnv' and ONE 'LuaBackendState', and every owner module
--   receives that pair as its 'SharedFixture'. Owner modules never
--   call 'withSharedFixture', 'withHeadlessEngine' or 'aroundAll'
--   themselves — that would multiply engine boots, which is exactly
--   the shape CH-116 rejected.
--
--   Besides the fixture itself this module carries only the eval
--   helpers every owner drives Lua through, 'luaLines', and the two
--   'FromJSON' row types that more than one owner decodes ('RectRow',
--   'WidthCapProbe'). Owner-specific decoders live beside their
--   owning specs.
module Test.Headless.UI.ResponsiveGameplay.Fixture
  ( SharedFixture
  , withSharedFixture
  , resetFixture
  , newBareLuaBackend
  , luaLines
  , eval
  , isLuaError
  , evalOk
  , evalBool
  , evalInt
  , evalJSON
  , RectRow(..)
  , WidthCapProbe(..)
  ) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), withObject, (.:))
import qualified Data.Text as T
import Data.IORef (newIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (vcUIScale)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine, installHudWorldPage)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import UI.Types (emptyUIPageManager)

-- | The one booted engine + Lua VM pair every owner fragment receives
--   (#2126). Owner specs are @SpecWith SharedFixture@; only the facade
--   constructs one.
type SharedFixture = (EngineEnv, LuaBackendState)

-- | Join Lua statements with a single space instead of GHC string-gap
--   continuations (mirrors ResponsiveMenus — a missing space before a
--   continuation backslash silently glues two tokens together).
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- #750 round-6 review: share ONE booted headless engine AND ONE Lua VM
-- across every case in this module, per the issue's own cost guardrail
-- spec addition read literally ("share one booted headless engine + Lua
-- environment across cases") — round 5 only shared the engine, giving
-- each case its own fresh 'newBareLuaBackend' Lua VM. 'withSharedFixture'
-- boots both exactly once for the whole module; 'resetFixture' (called
-- first in every case) resets whatever either could have accumulated
-- from an earlier case:
--   * The shared engine's own UIPageManager (Haskell-side page/element
--     tree) — cleared back to empty. Every case asserts on freshly-
--     created handles or RELATIVE counts (never a hardcoded absolute
--     handle number or page/element count), so this composes safely.
--   * engine.setUIScale's target (videoConfigRef's vcUIScale) — several
--     cases call it for a band-boundary/out-of-envelope exemplar; reset
--     to 1.0, preserving every other VideoConfig field as-is.
--   * The shared Lua VM's OWN module cache (package.loaded) — with the
--     Lua VM itself now shared, require('scripts.hud') etc. would
--     otherwise keep returning whichever EARLIER case's already-
--     initialized module table (hud.uiCreated=true, a selected tool,
--     popup.active entries, ...) instead of a pristine one. Wiped
--     entirely so every case's own require() calls re-execute each
--     .lua file from scratch — verified against a real running engine
--     (not just this suite) that this reproduces an identical fresh-
--     module state to a brand new Lua VM. The native UI/engine/world
--     API tables are untouched: they're plain Lua globals registered by
--     registerLuaAPI, never entries in package.loaded, so wiping the
--     module cache can't disturb them.
-- The engine-level event/combat/injury log ring buffers are NOT reset
-- (no such reset primitive is exposed to a test) — every case that
-- touches them already asserts existence/relative-preservation rather
-- than an exact count, so cross-case accumulation there is inert by
-- construction.
--
-- #1357: the shared fixture also establishes the filesystem boundary,
-- OUTSIDE 'withHeadlessEngine' — this module's round-11 case drives the
-- real @settingsMenu.onDefaults()@, whose write-through keybind reset
-- persisted through the production @engine.saveKeybinds()@ straight into
-- the developer's @config/keybinds.local.yaml@. Engine initialization is
-- itself a config writer, so the wrap has to sit outside it (see
-- 'Test.Headless.Harness.Isolation').
withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
withSharedFixture action = withIsolatedResourceRoot $
    withHeadlessEngine $ \env → do
        ls ← newBareLuaBackend env
        action (env, ls)

resetFixture ∷ EngineEnv → LuaBackendState → IO ()
resetFixture env ls = do
    writeIORef (uiManagerRef env) emptyUIPageManager
    atomicModifyIORef' (videoConfigRef env) $ \c → (c { vcUIScale = 1.0 }, ())
    -- #1366: every case here boots the HUD, and hud.createUI() submits
    -- six cursor-texture commands against hud.worldId ("main_world").
    -- Without the page they take the correct-but-noisy missing-page
    -- branch; see 'installHudWorldPage' for why the page carries no
    -- generation parameters and is not visible.
    installHudWorldPage env
    cleared ← evalOk ls
        "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end; return true"
    cleared `shouldBe` "true"

-- * Lua backend + eval helpers (mirrors Test.Headless.UI.ResponsiveMenus /
--   Test.Headless.UI.InputOwnership)
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
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

-- * FromJSON row types decoded by more than one owner

data WidthCapProbe = WidthCapProbe { wcpW ∷ Int, wcpInFrame ∷ Bool } deriving Show
instance FromJSON WidthCapProbe where
    parseJSON = withObject "WidthCapProbe" $ \o →
        WidthCapProbe <$> o .: "w" <*> o .: "inFrame"

data RectRow = RectRow { rrX ∷ Double, rrY ∷ Double, rrW ∷ Double, rrH ∷ Double } deriving Show
instance FromJSON RectRow where
    parseJSON = withObject "RectRow" $ \o →
        RectRow <$> o .: "x" <*> o .: "y" <*> o .: "w" <*> o .: "h"
