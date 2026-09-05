-- | Everything the five owner modules of the "Tutorial HUD" gate share:
--   the fixture itself (one headless engine, one bare Lua backend), the
--   per-case reset, the tutorial-tree constructors both tree sources are
--   built from, and the Lua eval/decode plumbing.
--
--   The fixture lives here and is entered ONCE, by
--   'Test.Headless.UI.TutorialHud.spec'. Nothing in this directory
--   except that facade may call 'withSharedFixture', and nothing may
--   boot a second engine or Lua state: the whole aggregate is one
--   'EngineEnv' and one 'LuaBackendState', and 'resetFixture' — which
--   every example calls as its first statement — is what keeps the
--   cases independent of each other.
--
--   'newBareLuaBackend' is deliberately NOT exported: an owner module
--   that could reach it could grow its own Lua lifecycle, which is the
--   one thing this split must not make possible.
module Test.Headless.UI.TutorialHud.Support
    ( Fixture
    , withSharedFixture
    , resetFixture
    , luaLines
    , treeHelpers
    , bootAt
    , subId
    , evalOk
    , decodeOr
    ) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON, decode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (vcUIScale)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine, installHudWorldPage)
import Tutorial.Types (emptyTutorialRegistry)
import UI.Types (emptyUIPageManager)

-- | Join Lua statements with a single space (mirrors ResponsiveMenus /
--   ResponsiveGameplay — a missing space before a continuation
--   backslash silently glues two tokens together).
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- * Fixture — one booted engine + one Lua VM for the whole aggregate,
--   reset before every case (the ResponsiveGameplay contract).

-- | What every owner-scoped @SpecWith@ fragment in this directory is
--   handed: the shared engine and the shared Lua backend, in that order.
type Fixture = (EngineEnv, LuaBackendState)

withSharedFixture ∷ (Fixture → IO ()) → IO ()
withSharedFixture action = withHeadlessEngine $ \env → do
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
    -- The tutorial registry is shared engine state that only
    -- 'Test.Headless.Tutorial.Definitions' and this suite ever touch,
    -- and both put it back empty rather than trusting the other to.
    writeIORef (tutorialRegistryRef env) emptyTutorialRegistry
    cleared ← evalOk ls
        "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end; return true"
    cleared `shouldBe` "true"

-- * Lua fixtures

-- | Tree constructors, plus the shipped @first_session@ SHAPE
--   (place_portal -> secure_water -> prepare_expedition{water, food})
--   written as the table @engine.getTutorialTree()@ hands Lua.
treeHelpers ∷ Text
treeHelpers = luaLines
    [ "local function node(id, kind, order, children, subs)"
    , "    return { id = id, kind = kind, label = id .. ' label',"
    , "             tooltip = id .. ' tooltip', evaluator = id .. '_eval',"
    , "             order = order, children = children or {},"
    , "             subobjectives = subs or {} } end;"
    , "local function shippedShape()"
    , "    local water = node('prepare_water', 'subobjective', 1);"
    , "    local food  = node('prepare_food',  'subobjective', 2);"
    , "    local exp   = node('prepare_expedition', 'composite', 1, {}, {water, food});"
    , "    local sec   = node('secure_water', 'full', 1, {exp});"
    , "    local root  = node('place_portal', 'full', 1, {sec});"
    , "    return { id = 'first_session', root = root } end;"
    -- A composite root with `n` subobjectives — n+1 active rows, the
    -- cheapest way to overflow any viewport for the scrolling cases.
    , "local function wideTree(n)"
    , "    local subs = {};"
    , "    for i = 1, n do"
    , "        subs[i] = node(string.format('sub_%03d', i), 'subobjective', i) end;"
    , "    return { id = 'first_session',"
    , "             root = node('root', 'composite', 1, {}, subs) } end;"
    -- #1941: the shipped SHAPE, widened. A composite that can latch
    -- before its ancestor reveals it (so it can go sticky, which
    -- `wideTree`'s root never can — a root is reveal-eligible from the
    -- start) carrying enough subobjectives to overflow a viewport, so
    -- the sticky row itself can be scrolled out of range.
    , "local function stickyTree(n)"
    , "    local subs = {};"
    , "    for i = 1, n do"
    , "        subs[i] = node(string.format('sub_%03d', i), 'subobjective', i) end;"
    , "    local branch = node('branch', 'composite', 1, {}, subs);"
    , "    return { id = 'first_session',"
    , "             root = node('gate', 'full', 1, {branch}) } end;"
    -- Latch `branch` and every subobjective BEFORE `gate` completes, so
    -- `branch`'s first reveal finds it already complete (#996's case).
    , "local function preLatch(tp, n)"
    , "    for i = 1, n do"
    , "        tp.setSubobjectiveChecked(string.format('sub_%03d', i), true) end;"
    , "    tp.completeObjective('branch');"
    , "    tp.completeObjective('gate') end;"
    ]

-- | Boot hud + the tutorial HUD at a given framebuffer size, with a
--   tree already injected. @hud.init@'s synthetic handles are the same
--   ones ResponsiveGameplay uses (box textures 1, font 2).
bootAt ∷ Int → Int → Text → Text
bootAt w h treeExpr = luaLines
    [ treeHelpers
    , "local hud = require('scripts.hud');"
    , "hud.init(1, 2, " <> tshow w <> ", " <> tshow h <> ");"
    , "hud.createUI();"
    , "hud.visible = true;"
    , "local tp = require('scripts.tutorial_progress');"
    , "tp.reset();"
    , "if " <> treeExpr <> " ~= nil then tp.setTree(" <> treeExpr <> ") end;"
    , "local th = require('scripts.tutorial_hud');"
    , "th.init();"
    -- reflow(), not onFramebufferResize: the broadcast half only
    -- records dimensions (see the resize-ordering case below).
    , "th.reflow(" <> tshow w <> ", " <> tshow h <> ");"
    , "th.update(0);"
    ]

-- | @wideTree@'s zero-padded subobjective id for index @n@ (row @n+1@,
--   since row 1 is the composite root).
subId ∷ Int → Text
subId n = "sub_" <> T.justifyRight 3 '0' (tshow n)

decodeOr ∷ FromJSON a ⇒ Text → IO a
decodeOr t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just v  → pure v
    Nothing → do
        expectationFailure ("failed to decode: " ⧺ T.unpack t)
        fail "unreachable"

-- * Lua backend + eval helpers (mirrors Test.Headless.UI.ResponsiveGameplay)

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls code = do
    t ← executeDebugLua (lbsLuaState ls) code
    when (isLuaError t) $ expectationFailure ("Lua error: " ⧺ T.unpack t)
    pure t
