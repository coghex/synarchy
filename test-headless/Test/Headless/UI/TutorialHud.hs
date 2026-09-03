-- | The "Tutorial HUD" gate (#960, phase 3 of the tutorial epic #956):
--   @scripts/tutorial_hud.lua@, the one surface that renders #958's
--   tutorial view model.
--
--   Same headless technique (and the same constraint) as
--   'Test.Headless.UI.ResponsiveGameplay': the full ui_manager boot
--   never reaches gameplay UI headless (it gates on fontsReady, which
--   needs a GPU font atlas), so this suite boots @scripts/hud.lua@
--   directly with synthetic texture/font handles and then boots the
--   tutorial HUD on top of it. @engine.getTextWidth@ measures 0 in that
--   fixture, so every assertion here is geometry- or count-based and
--   the module under test derives row height and scroll range from the
--   UI scale rather than from measured text. The one exception is
--   #1419's toggle-caption fit, whose own group STUBS
--   @engine.getTextWidth@ with a deterministic non-zero metric (the
--   'Test.Headless.UI.ResponsiveGameplay' idiom) and restores it
--   immediately, precisely so the rest of this suite stays
--   measurement-free — an unmeasurable caption leaves the toggle at
--   exactly its historical constant width.
--
--   The tutorial TREE arrives two ways on purpose: injected through
--   @tutorialProgress.setTree@ (the same injection point #958's own
--   gate uses, for the shapes a hand-authored tree can produce), and
--   loaded for real from @data/tutorials@ through
--   @engine.loadTutorialDir@ so the shipped YAML's labels and tooltips
--   are proven to reach the screen. The shared headless engine does not
--   populate the tutorial registry on its own, and this suite puts it
--   back empty afterwards, exactly as 'Test.Headless.Tutorial.Definitions'
--   does.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Tutorial HUD"'@.
module Test.Headless.UI.TutorialHud (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:), (.:?))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (vcUIScale)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine, installHudWorldPage)
import Tutorial.Types (emptyTutorialRegistry)
import UI.Manager.Presentation (snapshotArmedToken, witnessPresentation)
import UI.Types (emptyUIPageManager)

-- | Join Lua statements with a single space (mirrors ResponsiveMenus /
--   ResponsiveGameplay — a missing space before a continuation
--   backslash silently glues two tokens together).
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- * Fixture — one booted engine + one Lua VM for the whole module,
--   reset before every case (the ResponsiveGameplay contract).

withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
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

-- * The presentation boundary (#2056)
--
-- The boundary is a real handshake with the RENDERER, and this fixture
-- has no renderer: @renderUIPages@ bails out headless for want of a
-- bindless texture system, which is exactly the honest answer (a
-- GPU-less engine draws no frame, so it witnesses nothing). So the
-- boundary is crossed here by performing, against the same shared UI
-- manager, EXACTLY the two steps 'UI.Render.renderUIPages' performs —
-- read the manager once, then publish that snapshot's own armed token —
-- rather than by poking the witness to a value of the test's choosing.
--
-- Keeping the two steps separately callable is what makes the
-- stale-evidence cases possible: a snapshot BEGUN before a mutation and
-- FINISHED after it publishes the token it actually held, which is the
-- in-flight frame the boundary has to refuse.

-- | The renderer's read: the token this snapshot is carrying.
beginSnapshot ∷ EngineEnv → IO Word64
beginSnapshot env = snapshotArmedToken ⊚ readIORef (uiManagerRef env)

-- | The renderer's publication, once that snapshot has been rendered.
finishSnapshot ∷ EngineEnv → Word64 → IO ()
finishSnapshot env token =
    atomicModifyIORef' (uiManagerRef env)
                       (\m → (witnessPresentation token m, ()))

-- | One complete, uninterrupted renderer snapshot of whatever is on
--   screen right now.
renderSnapshot ∷ EngineEnv → IO ()
renderSnapshot env = beginSnapshot env ⌦ finishSnapshot env

-- | The shipped shape with its prepare branch ALREADY latched and both
--   subobjectives checked before the branch is ever revealed — #996's
--   case, and the one #1941 retires. Ends with the branch revealed, the
--   panel still collapsed and nothing presented.
stickyBoot ∷ Int → Int → Text
stickyBoot w h = luaLines
    [ bootAt w h "shippedShape()"
    , "tp.setSubobjectiveChecked('prepare_water', true);"
    , "tp.setSubobjectiveChecked('prepare_food', true);"
    , "tp.completeObjective('prepare_expedition');"
    , "tp.completeObjective('place_portal');"
    , "tp.completeObjective('secure_water');"
    ]

-- | Re-bind the three module locals. Every boundary case runs as
--   SEVERAL console chunks — the snapshot has to happen between two of
--   them, from Haskell — and @bootAt@'s locals do not outlive the chunk
--   that declared them.
reModules ∷ Text
reModules = luaLines
    [ "local tp = require('scripts.tutorial_progress');"
    , "local th = require('scripts.tutorial_hud');"
    , "local hud = require('scripts.hud');"
    ]

-- | The three ids of the already-latched prepare branch, comma-joined
--   as every case here reports them.
prepareBranch ∷ Text
prepareBranch = T.intercalate ","
    [ "prepare_expedition", "prepare_water", "prepare_food" ]

-- | 'executeDebugLua' serialises a bare STRING return as JSON, quotes
--   included, so a case that returns one compares against this rather
--   than the raw text (a table return decodes through 'decodeOr'
--   instead and needs none of it).
quoted ∷ Text → Text
quoted t = "\"" <> t <> "\""

-- * Decoded probe shapes

data StateProbe = StateProbe
    { spOpen ∷ Bool, spVisible ∷ Bool, spRowCount ∷ Int
    , spScrollOffset ∷ Int, spScrollRange ∷ Int, spRebuilds ∷ Int
    , spToggleX ∷ Double, spToggleY ∷ Double
    , spToggleW ∷ Double, spToggleH ∷ Double
    , spRowIds ∷ Text } deriving (Show, Eq)
instance FromJSON StateProbe where
    parseJSON = withObject "StateProbe" $ \o →
        StateProbe <$> o .: "open" <*> o .: "visible" <*> o .: "rowCount"
                    <*> o .: "scrollOffset" <*> o .: "scrollRange"
                    <*> o .: "rebuilds"
                    <*> o .: "tx" <*> o .: "ty" <*> o .: "tw" <*> o .: "th"
                    <*> o .: "rowIds"

data RowProbe = RowProbe
    { rpId ∷ Text, rpKind ∷ Text, rpLabel ∷ Text, rpTooltip ∷ Text
    , rpMarker ∷ Text
    , rpCompleted ∷ Maybe Bool, rpChecked ∷ Maybe Bool
    , rpBlocks ∷ Bool, rpCaptures ∷ Bool, rpOnClick ∷ Maybe Text
    , rpDepth ∷ Int } deriving (Show, Eq)
instance FromJSON RowProbe where
    parseJSON = withObject "RowProbe" $ \o →
        RowProbe <$> o .: "id" <*> o .: "kind" <*> o .: "label"
                  <*> o .: "tooltip" <*> o .: "marker"
                  <*> o .:? "completed" <*> o .:? "checked"
                  <*> o .: "blocks" <*> o .: "captures" <*> o .:? "onClick"
                  <*> o .: "depth"

data ActiveRowsProbe = ActiveRowsProbe
    { arpRowIds ∷ Text, arpActiveIds ∷ Text } deriving (Show, Eq)
instance FromJSON ActiveRowsProbe where
    parseJSON = withObject "ActiveRowsProbe" $ \o →
        ActiveRowsProbe <$> o .: "rowIds" <*> o .: "activeIds"

data BandProbe = BandProbe
    { bpW ∷ Int, bpH ∷ Int, bpScale ∷ Double
    , bpToggleInFrame ∷ Bool, bpToggleClearOfToolbars ∷ Bool
    , bpPanelInFrame ∷ Bool, bpListInFrame ∷ Bool
    , bpCapacity ∷ Int, bpRangeOk ∷ Bool
    , bpToggleStable ∷ Bool } deriving (Show, Eq)
instance FromJSON BandProbe where
    parseJSON = withObject "BandProbe" $ \o →
        BandProbe <$> o .: "w" <*> o .: "h" <*> o .: "scale"
                   <*> o .: "toggleInFrame" <*> o .: "toggleClear"
                   <*> o .: "panelInFrame" <*> o .: "listInFrame"
                   <*> o .: "capacity" <*> o .: "rangeOk"
                   <*> o .: "toggleStable"

-- | Compact state readout used by most cases.
stateProbeLua ∷ Text
stateProbeLua = luaLines
    [ "local d = th.dump();"
    , "return { open = d.open, visible = d.visible, rowCount = #d.rows,"
    , "         scrollOffset = d.scrollOffset, scrollRange = d.scrollRange,"
    , "         rebuilds = d.rebuildCount, tx = d.toggle.x, ty = d.toggle.y,"
    , "         tw = d.toggle.w, th = d.toggle.h,"
    -- Joined, not an array: an EMPTY Lua table serialises as a JSON
    -- object, and "no rows at all" is exactly what several cases here
    -- need to assert.
    , "         rowIds = table.concat(d.rowIds, ',') }"
    ]

decodeOr ∷ FromJSON a ⇒ Text → IO a
decodeOr t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just v  → pure v
    Nothing → do
        expectationFailure ("failed to decode: " ⧺ T.unpack t)
        fail "unreachable"

spec ∷ Spec
spec = aroundAll withSharedFixture $ do

    describe "presentation lifecycle (#960 requirements 2/5)" $ do
        it "starts collapsed at a fresh boot, with the toggle already present" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "shippedShape()", stateProbeLua ]
            st ← decodeOr r
            spOpen st `shouldBe` False
            spVisible st `shouldBe` False
            spRowCount st `shouldBe` 0
            spScrollOffset st `shouldBe` 0
            -- The toggle is a persistent HUD button: it exists (and has
            -- real extent) even while the list is collapsed.
            spToggleW st `shouldSatisfy` (> 0)
            spToggleH st `shouldSatisfy` (> 0)
            spToggleX st `shouldSatisfy` (≥ 0)
            spToggleY st `shouldSatisfy` (≥ 0)
            spRowIds st `shouldBe` ""
            -- Nothing to scroll while collapsed, and init() built the
            -- surface exactly once before this suite's own resize.
            spScrollRange st `shouldBe` 0
            spRebuilds st `shouldSatisfy` (> 0)

        it "the toggle opens and closes the list, and its bounds are identical in both states" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "shippedShape()"
                , "local closed = th.dump();"
                -- Click the real element the way the engine does: the
                -- broadcast hands the module its own callback name.
                , "th.onTutorialHudToggle(closed.toggle.handle);"
                , "local opened = th.dump();"
                , "th.onTutorialHudToggle(opened.toggle.handle);"
                , "local reclosed = th.dump();"
                , "return { openedRows = #opened.rows, openedOpen = opened.open,"
                , "         reclosedRows = #reclosed.rows, reclosedOpen = reclosed.open,"
                , "         sameX = (closed.toggle.x == opened.toggle.x),"
                , "         sameY = (closed.toggle.y == opened.toggle.y),"
                , "         sameW = (closed.toggle.w == opened.toggle.w),"
                , "         sameH = (closed.toggle.h == opened.toggle.h) }"
                ]
            probe ← decodeOr r ∷ IO ToggleProbe
            tpOpenedOpen probe `shouldBe` True
            tpOpenedRows probe `shouldBe` 1     -- only the root is active
            tpReclosedOpen probe `shouldBe` False
            tpReclosedRows probe `shouldBe` 0
            (tpSameX probe, tpSameY probe) `shouldBe` (True, True)
            (tpSameW probe, tpSameH probe) `shouldBe` (True, True)

        it "a save load collapses the panel and drops the scroll offset, without touching progress" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1024 768 "wideTree(60)"
                , "th.setOpen(true); th.setScrollOffset(5);"
                , "tp.completeObjective('root');"
                , "local before = th.dump();"
                -- The engine broadcast every load trigger reaches.
                , "th.onSaveLoaded({}, {});"
                , "local after = th.dump();"
                , "return { beforeOpen = before.open, beforeOffset = before.scrollOffset,"
                , "         afterOpen = after.open, afterOffset = after.scrollOffset,"
                , "         progressKept = tp.isCompleted('root') }"
                ]
            probe ← decodeOr r ∷ IO ResetProbe
            (rsBeforeOpen probe, rsBeforeOffset probe) `shouldBe` (True, 5)
            (rsAfterOpen probe, rsAfterOffset probe) `shouldBe` (False, 0)
            -- Presentation only: #958 still owns the durable latch.
            rsProgressKept probe `shouldBe` True

        it "generating a new world mid-process resets the panel (worldManager.createWorld's funnel)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1024 768 "wideTree(60)"
                , "th.setOpen(true); th.setScrollOffset(4);"
                , "local worldManager = require('scripts.world_manager');"
                -- createWorld resets tutorial state BEFORE it queues
                -- the WorldInit command; stubbing world.init keeps this
                -- case from starting a real generation in the shared
                -- engine, and the original is restored either way.
                , "local realInit = world.init;"
                , "world.init = function() error('stubbed', 0) end;"
                , "pcall(worldManager.createWorld, { worldId = 'tutorial_hud_spec' });"
                , "world.init = realInit;"
                , "local after = th.dump();"
                , "return { afterOpen = after.open, afterOffset = after.scrollOffset,"
                , "         restored = (world.init == realInit) }"
                ]
            probe ← decodeOr r ∷ IO CreateWorldProbe
            cwAfterOpen probe `shouldBe` False
            cwAfterOffset probe `shouldBe` 0
            cwRestored probe `shouldBe` True

        it "picks up hud's font and box textures once they exist, without waiting for the model to change" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                -- The REAL boot order: scripts/init_loader.lua
                -- loadScript's this module well BEFORE ui_manager, and
                -- hud.init (which supplies hud.menuFont/hud.boxTexSet)
                -- is gated further still on fontsReady. So the first
                -- build legitimately has no assets at all.
                , "local tp = require('scripts.tutorial_progress');"
                , "tp.reset(); tp.setTree(shippedShape());"
                , "local th = require('scripts.tutorial_hud');"
                , "th.init(); th.reflow(1280, 720); th.setOpen(true);"
                , "local early = th.dump();"
                -- ui_manager boots hud and shows it. The tutorial model
                -- is deliberately NOT touched from here on: the surface
                -- must repaint off asset readiness alone.
                , "local hud = require('scripts.hud');"
                , "hud.init(1, 2, 1280, 720); hud.createUI(); hud.visible = true;"
                , "th.update(0);"
                , "local ready = th.dump();"
                -- A second tick must NOT rebuild again (nothing changed).
                , "th.update(0);"
                , "local settled = th.dump();"
                , "return { earlyAssets = early.assetsReady,"
                , "         earlyRows = #early.rows,"
                , "         earlyToggleLabel = (early.toggle.label ~= nil),"
                , "         earlyRowText = (early.rows[1] ~= nil"
                , "             and early.rows[1].textHandle ~= nil),"
                , "         earlyRebuilds = early.rebuildCount,"
                , "         readyAssets = ready.assetsReady,"
                , "         readyRows = #ready.rows,"
                , "         readyToggleLabel = (ready.toggle.label ~= nil),"
                , "         readyRowText = (ready.rows[1] ~= nil"
                , "             and ready.rows[1].textHandle ~= nil),"
                , "         readyRebuilds = ready.rebuildCount,"
                , "         settledRebuilds = settled.rebuildCount,"
                , "         openKept = ready.open }"
                ]
            probe ← decodeOr r ∷ IO AssetProbe
            -- Before hud.init: rows exist as real (hit-testable,
            -- scroll-capturing) elements, but nothing is drawable.
            apEarlyAssets probe `shouldBe` False
            apEarlyRows probe `shouldSatisfy` (> 0)
            apEarlyToggleLabel probe `shouldBe` False
            apEarlyRowText probe `shouldBe` False
            -- One tick after hud.init, with the model untouched, the
            -- surface has rebuilt itself with real text and a real box.
            apReadyAssets probe `shouldBe` True
            apReadyRows probe `shouldBe` apEarlyRows probe
            apReadyToggleLabel probe `shouldBe` True
            apReadyRowText probe `shouldBe` True
            apReadyRebuilds probe - apEarlyRebuilds probe `shouldBe` 1
            -- ...and then settles: asset readiness is part of the
            -- signature, not a per-tick rebuild trigger.
            apSettledRebuilds probe `shouldBe` apReadyRebuilds probe
            apOpenKept probe `shouldBe` True

        it "a HUD hide/show round trip preserves open state and scroll offset" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1024 768 "wideTree(60)"
                , "th.setOpen(true); th.setScrollOffset(3);"
                , "local shown = UI.isPageVisible(th.page);"
                -- hud.hide() sets hud.visible = false and then runs the
                -- teardown sweep this module registers in.
                , "hud.visible = false;"
                , "require('scripts.ui.view_teardown').run('hudHide');"
                , "local hidden = th.dump();"
                , "local pageHidden = UI.isPageVisible(th.page);"
                , "hud.visible = true; th.update(0);"
                , "local back = th.dump();"
                , "return { shown = shown, pageHidden = pageHidden,"
                , "         hiddenOpen = hidden.open, hiddenOffset = hidden.scrollOffset,"
                , "         backOpen = back.open, backOffset = back.scrollOffset,"
                , "         backRows = #back.rows,"
                , "         pageShown = UI.isPageVisible(th.page) }"
                ]
            probe ← decodeOr r ∷ IO HudCycleProbe
            hcShown probe `shouldBe` True
            hcPageHidden probe `shouldBe` False
            -- Presentation-only teardown: the LOGICAL state survives.
            (hcHiddenOpen probe, hcHiddenOffset probe) `shouldBe` (True, 3)
            (hcBackOpen probe, hcBackOffset probe) `shouldBe` (True, 3)
            hcBackRows probe `shouldSatisfy` (> 0)
            hcPageShown probe `shouldBe` True

    describe "rendering the #958 view model (requirements 1/3/4)" $ do
        it "renders the shipped YAML tree's own labels and tooltips, revealing rows as objectives latch" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                , "local hud = require('scripts.hud');"
                , "hud.init(1, 2, 1280, 720); hud.createUI(); hud.visible = true;"
                -- The REAL registry, loaded from the shipped directory.
                , "local loaded = engine.loadTutorialDir('data/tutorials');"
                , "local tp = require('scripts.tutorial_progress'); tp.reset();"
                , "local th = require('scripts.tutorial_hud');"
                , "th.init(); th.reflow(1280, 720); th.setOpen(true);"
                , "local first = th.dump();"
                , "tp.completeObjective('first_session_place_portal'); th.rebuild();"
                , "local second = th.dump();"
                , "tp.completeObjective('first_session_secure_water'); th.rebuild();"
                , "local third = th.dump();"
                , "return { loaded = (loaded ~= false),"
                , "         firstIds = table.concat(first.rowIds, ','),"
                , "         secondIds = table.concat(second.rowIds, ','),"
                , "         thirdIds = table.concat(third.rowIds, ','),"
                , "         firstLabel = first.rows[1].label,"
                , "         firstTooltip = first.rows[1].tooltip,"
                , "         treeId = (engine.getTutorialTree() or {}).id }"
                ]
            probe ← decodeOr r ∷ IO YamlProbe
            ypLoaded probe `shouldBe` True
            ypTreeId probe `shouldBe` Just "first_session"
            ypFirstIds probe `shouldBe` "first_session_place_portal"
            -- Labels/tooltips come straight from data/tutorials/first_session.yaml.
            ypFirstLabel probe `shouldBe` "Place portal"
            ypFirstTooltip probe `shouldBe`
                "Zoom in for the build tool, then anchor your colony."
            -- Completing the root REVEALS its child and keeps the root
            -- itself active (its subtree is not done yet).
            ypSecondIds probe `shouldBe`
                "first_session_place_portal,first_session_secure_water"
            -- Completing that child in turn hides the now-finished root
            -- (#958's active-view rule) and reveals the composite with
            -- its live subobjectives. Order is the model's own pre-order
            -- display walk — never re-sorted here.
            ypThirdIds probe `shouldBe` T.intercalate ","
                [ "first_session_secure_water"
                , "first_session_prepare_expedition"
                , "first_session_prepare_water", "first_session_prepare_food" ]
            -- Leave the shared registry as this suite found it.
            writeIORef (tutorialRegistryRef env) emptyTutorialRegistry

        it "keeps latched completion and live subobjective checks distinct, and never renders inactive rows" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "shippedShape()"
                , "tp.completeObjective('place_portal');"
                , "tp.completeObjective('secure_water');"
                , "tp.setSubobjectiveChecked('prepare_water', true);"
                , "th.setOpen(true);"
                , "local d = th.dump();"
                , "local out = {};"
                , "for i, row in ipairs(d.rows) do"
                , "    out[i] = { id = row.id, kind = row.kind, label = row.label,"
                , "               tooltip = row.tooltip, marker = row.marker,"
                , "               completed = row.completed, checked = row.checked,"
                , "               blocks = row.pointerBlocking,"
                , "               captures = row.scrollCapture,"
                , "               onClick = UI.getElementOnClick(row.handle),"
                , "               depth = row.depth } end;"
                , "return out"
                ]
            rows ← decodeOr r ∷ IO [RowProbe]
            -- 'place_portal' latched AND its only child latched, so it
            -- has left the active view entirely — retained history is
            -- reported by the model with active = false and must never
            -- be rendered here.
            map rpId rows `shouldBe`
                [ "secure_water", "prepare_expedition"
                , "prepare_water", "prepare_food" ]
            let byId i = case [x | x ← rows, rpId x ≡ i] of
                            (x:_) → x
                            []    → error ("no row " ⧺ T.unpack i)
            -- A latched FULL objective reports `completed` and only
            -- `completed`; a live SUBOBJECTIVE reports `checked` and
            -- only `checked`. Collapsing the two is the bug #958 exists
            -- to prevent, so the split is asserted on both sides.
            rpCompleted (byId "secure_water") `shouldBe` Just True
            rpChecked (byId "secure_water") `shouldBe` Nothing
            rpMarker (byId "secure_water") `shouldBe` "[x]"
            rpKind (byId "secure_water") `shouldBe` "full"
            rpKind (byId "prepare_water") `shouldBe` "subobjective"
            rpCompleted (byId "prepare_expedition") `shouldBe` Just False
            rpMarker (byId "prepare_expedition") `shouldBe` "[ ]"
            rpChecked (byId "prepare_water") `shouldBe` Just True
            rpCompleted (byId "prepare_water") `shouldBe` Nothing
            rpMarker (byId "prepare_water") `shouldBe` "(x)"
            rpChecked (byId "prepare_food") `shouldBe` Just False
            rpMarker (byId "prepare_food") `shouldBe` "( )"
            -- Depth drives indentation only; the model's order stands.
            map rpDepth rows `shouldBe` [1, 2, 3, 3]
            -- Requirement 4: display-only rows.
            forM_ rows $ \row → do
                rpBlocks row `shouldBe` False
                rpOnClick row `shouldBe` Nothing
                rpCaptures row `shouldBe` True
                rpTooltip row `shouldSatisfy` (not ∘ T.null)
                rpLabel row `shouldSatisfy` (not ∘ T.null)

        it "drops a row from the active view when its whole subtree is satisfied" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "shippedShape()"
                , "tp.completeObjective('place_portal');"
                , "tp.completeObjective('secure_water');"
                , "tp.completeObjective('prepare_expedition');"
                , "tp.setSubobjectiveChecked('prepare_water', true);"
                , "tp.setSubobjectiveChecked('prepare_food', true);"
                , "th.setOpen(true);"
                , "local done = th.dump();"
                -- Reversible: unchecking a subobjective brings the
                -- composite (and its chain) straight back.
                , "tp.setSubobjectiveChecked('prepare_food', false); th.rebuild();"
                , "local reopened = th.dump();"
                , "return { doneIds = table.concat(done.rowIds, ','),"
                , "         reopenedIds = table.concat(reopened.rowIds, ',') }"
                ]
            probe ← decodeOr r ∷ IO ReverseProbe
            rvDoneIds probe `shouldBe` ""
            -- Only the composite chain comes back: the two full
            -- objectives above it stay finished, since an unchecked
            -- subobjective does not un-complete anything durable.
            rvReopenedIds probe `shouldBe` T.intercalate ","
                [ "prepare_expedition", "prepare_water", "prepare_food" ]

        -- #996: the composite (and both prepare subobjectives) latches
        -- BEFORE secure_water ever reveals it — the shipped acolyte spawn
        -- kit satisfies both prepare subobjectives on its own. The real
        -- HUD must still show the branch, in authored order, the first
        -- time it is ever revealed rather than rendering nothing.
        it "renders an already-latched prepare branch the first time it \
           \is revealed, instead of an empty checklist" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "shippedShape()"
                , "tp.setSubobjectiveChecked('prepare_water', true);"
                , "tp.setSubobjectiveChecked('prepare_food', true);"
                , "tp.completeObjective('prepare_expedition');"
                , "tp.completeObjective('place_portal');"
                , "tp.completeObjective('secure_water');"
                , "th.setOpen(true); th.rebuild();"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ',') }"
                ]
            probe ← decodeOr r ∷ IO ActiveRowsProbe
            let want = T.intercalate ","
                    [ "prepare_expedition", "prepare_water", "prepare_food" ]
            arpRowIds probe `shouldBe` want
            arpActiveIds probe `shouldBe` want

        -- #1941: and then it RETIRES. This surface is the only thing
        -- that can testify a row was actually put in front of the
        -- player, so #958 hands it that job.
        --
        -- #2056 is what makes that testimony true. Before it, ONE Lua
        -- update tick counted as presentation, and the four cases below
        -- encoded that: the tick showed the page, acknowledged, and
        -- rebuilt without the rows, all inside one uninterrupted call
        -- that no renderer snapshot had to fall between. Every case here
        -- now separates the two halves — nothing retires until a
        -- COMPLETED snapshot has held exactly these rows, and then it
        -- retires promptly.
        it "does NOT retire on the update tick alone — no snapshot has \
           \held the rows, so the branch is still waiting (#2056)" $ \(env, ls) → do
            resetFixture env ls
            built ← evalOk ls $ luaLines
                [ stickyBoot 1280 720
                , "th.setOpen(true);"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ',') }"
                ]
            shown ← decodeOr built ∷ IO ActiveRowsProbe
            -- #996, unchanged: opening renders the whole branch.
            arpRowIds shown `shouldBe` prepareBranch
            arpActiveIds shown `shouldBe` prepareBranch
            -- Ticks are free; frames are not. However many times the
            -- module reports, nothing has been rendered, so nothing may
            -- be acknowledged.
            ticked ← evalOk ls $ luaLines
                [ reModules
                , "local before = th.dump().rebuildCount;"
                , "for _ = 1, 8 do th.update(0) end;"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ','),"
                , "         presented = d.presented,"
                , "         rebuilds = d.rebuildCount - before }"
                ]
            held ← decodeOr ticked ∷ IO BoundaryProbe
            bpRowIds held `shouldBe` prepareBranch
            bpActiveIds held `shouldBe` prepareBranch
            bpPresented held `shouldBe` False
            bpRebuilds held `shouldBe` 0

        it "retires on the first tick after a completed snapshot has \
           \held exactly those rows (#1941 via #2056)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls $ luaLines
                [ stickyBoot 1280 720, "th.setOpen(true); return true" ]
            -- The renderer sees the open panel.
            renderSnapshot env
            after ← evalOk ls $ luaLines
                [ reModules
                , "local before = th.dump().rebuildCount;"
                , "local presented = th.dump().presented;"
                , "th.update(0);"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ','),"
                , "         presented = presented,"
                , "         rebuilds = d.rebuildCount - before }"
                ]
            probe ← decodeOr after ∷ IO BoundaryProbe
            -- The boundary really had been crossed before the tick ran.
            bpPresented probe `shouldBe` True
            -- The reporting tick spends the suppression and rebuilds
            -- ONCE: the ordinary completed-history view, an empty
            -- checklist.
            bpActiveIds probe `shouldBe` ""
            bpRowIds probe `shouldBe` ""
            bpRebuilds probe `shouldBe` 1

        it "never acknowledges from a COLLAPSED panel, however many \
           \frames are drawn over it (#1941)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls $ luaLines [ stickyBoot 1280 720, "return true" ]
            -- Ticks AND frames with the panel closed: the HUD is
            -- visible and the branch is active in the model, but the
            -- checklist lays out no rows at all, so nothing is
            -- presented. The frames are what makes this stronger than
            -- the pre-#2056 version, which could only tick.
            suppressed ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 3 do th.update(0) end;"
                , "return table.concat(th.dump().activeIds, ',')" ]
            renderSnapshot env
            stillSuppressed ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 3 do th.update(0) end;"
                , "return table.concat(th.dump().activeIds, ',')" ]
            suppressed `shouldBe` quoted prepareBranch
            stillSuppressed `shouldBe` quoted prepareBranch
            -- Opened, it renders — and a snapshot taken BEFORE the open
            -- (above) buys nothing, because opening re-armed.
            exposed ← evalOk ls $ luaLines
                [ reModules
                , "th.setOpen(true); th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            exposed `shouldBe` quoted prepareBranch
            renderSnapshot env
            retired ← evalOk ls $ luaLines
                [ reModules
                , "th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            retired `shouldBe` quoted ""

        it "never acknowledges while the gameplay HUD is HIDDEN, and \
           \the rising edge alone is NOT the presentation (#2056)" $ \(env, ls) → do
            resetFixture env ls
            -- Open, but with the whole HUD hidden: the page is not
            -- painted, so a build behind it presents nothing — and
            -- neither do the frames drawn while it is hidden.
            _ ← evalOk ls $ luaLines
                [ stickyBoot 1280 720
                , "hud.visible = false; th.update(0);"
                , "th.setOpen(true); return true" ]
            renderSnapshot env
            suppressed ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 3 do th.update(0) end;"
                , "return table.concat(th.dump().activeIds, ',')" ]
            suppressed `shouldBe` quoted prepareBranch
            -- THE DEFECT #2056 CLOSED. The HUD comes back with no
            -- content change of any kind. Before #2056 this one call
            -- showed the page, acknowledged the rows and deleted them
            -- again with no frame in between, retiring a terminal
            -- branch the player had never seen. The rows must survive
            -- the rising edge, and survive every further tick until a
            -- frame has actually drawn them.
            edge ← evalOk ls $ luaLines
                [ reModules
                , "hud.visible = true;"
                , "th.update(0);"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ','),"
                , "         presented = d.presented,"
                , "         rebuilds = 0 }"
                ]
            edged ← decodeOr edge ∷ IO BoundaryProbe
            bpRowIds edged `shouldBe` prepareBranch
            bpActiveIds edged `shouldBe` prepareBranch
            bpPresented edged `shouldBe` False
            stillHeld ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 5 do th.update(0) end;"
                , "return table.concat(th.dump().activeIds, ',')" ]
            stillHeld `shouldBe` quoted prepareBranch
            -- And then the frame arrives, and it retires.
            renderSnapshot env
            exposed ← evalOk ls $ luaLines
                [ reModules
                , "th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            exposed `shouldBe` quoted ""

        it "never acknowledges a sticky row scrolled OUT of the \
           \viewport, and does once it is scrolled back (#1941)" $ \(env, ls) → do
            resetFixture env ls
            -- Scroll the composite itself off the top. Its
            -- subobjectives stay on screen, so the panel is busy — but
            -- the only STICKY id is no longer laid out, and frames
            -- drawn in that state present it no more than ticks do.
            suppressed ← evalOk ls $ luaLines
                [ bootAt 1024 768 "stickyTree(60)"
                , "preLatch(tp, 60);"
                , "th.setOpen(true);"
                , "th.setScrollOffset(1);"
                , "for _ = 1, 3 do th.update(0) end;"
                , "local d = th.dump();"
                , "return tostring(d.rows[1] ~= nil and d.rows[1].id or 'none')"
                , "    .. '/' .. tostring(d.activeIds[1] or 'none')"
                ]
            renderSnapshot env
            stillSuppressed ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 3 do th.update(0) end;"
                , "local d = th.dump();"
                , "return tostring(d.rows[1] ~= nil and d.rows[1].id or 'none')"
                , "    .. '/' .. tostring(d.activeIds[1] or 'none')"
                ]
            -- The viewport starts at the first subobjective while the
            -- model still reports the sticky composite as active: the
            -- row was never rendered, so it was never presented.
            let offscreen = quoted (subId 1 <> "/branch")
            suppressed `shouldBe` offscreen
            stillSuppressed `shouldBe` offscreen
            -- Back to the top. The sticky row IS in the viewport now,
            -- but the scroll re-armed, so the snapshot above — taken
            -- while it was off screen — authorises nothing.
            stale ← evalOk ls $ luaLines
                [ reModules
                , "th.setScrollOffset(0);"
                , "for _ = 1, 3 do th.update(0) end;"
                , "return table.concat(th.dump().rowIds, ',') ~= ''"
                ]
            stale `shouldBe` "true"
            renderSnapshot env
            exposed ← evalOk ls $ luaLines
                [ reModules
                , "th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            -- Retired: the whole branch leaves the active view, so the
            -- rebuilt viewport is empty.
            exposed `shouldBe` quoted ""

        -- The in-flight frame, which is the case a bare "a frame has
        -- happened since" counter cannot tell apart from a good one.
        -- The snapshot is BEGUN while the open panel is on screen and
        -- FINISHED after a rebuild has destroyed and recreated every
        -- element in it. What it publishes is the token it actually
        -- held, so the rows it never saw stay unacknowledged.
        it "refuses evidence from a snapshot that predates a rebuild \
           \(#2056)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls $ luaLines
                [ stickyBoot 1280 720, "th.setOpen(true); return true" ]
            inFlight ← beginSnapshot env
            -- The renderer is still working on that snapshot when the
            -- panel rebuilds — same rows, all-new elements.
            _ ← evalOk ls $ luaLines
                [ reModules, "th.reflow(1280, 720); return true" ]
            finishSnapshot env inFlight
            stale ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 5 do th.update(0) end;"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ','),"
                , "         presented = d.presented, rebuilds = 0 }"
                ]
            probe ← decodeOr stale ∷ IO BoundaryProbe
            bpPresented probe `shouldBe` False
            bpRowIds probe `shouldBe` prepareBranch
            bpActiveIds probe `shouldBe` prepareBranch
            -- A snapshot of the rebuilt list does authorise it.
            renderSnapshot env
            exposed ← evalOk ls $ luaLines
                [ reModules
                , "th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            exposed `shouldBe` quoted ""

    describe "scoped wheel capture and scrolling (requirements 4/7)" $ do
        it "captures the wheel only over the visible list — never on the toggle, never off it" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1024 768 "wideTree(60)"
                , "th.setOpen(true);"
                , "local d = th.dump();"
                -- A control that is not part of the list: hud's own
                -- toolbar exists on a different page entirely.
                , "local foreign = UI.newElement('tutorial_hud_spec_foreign', 10, 10, th.page);"
                -- Every handle below is invalidated by the first
                -- scroll (which rebuilds), so the policy flags are read
                -- up front rather than in the returned table.
                , "local toggleCaptures = UI.isScrollCapturing(d.toggle.handle);"
                , "local rowCaptures = UI.isScrollCapturing(d.rows[1].handle);"
                , "local handledForeign = th.onUIScroll(foreign, 0, -1);"
                , "local offsetAfterForeign = th.dump().scrollOffset;"
                , "local handledToggle = th.onUIScroll(d.toggle.handle, 0, -1);"
                , "local handledRow = th.onUIScroll(d.rows[1].handle, 0, -1);"
                , "local afterRow = th.dump();"
                , "th.setOpen(false);"
                , "local closedDump = th.dump();"
                , "return { toggleCaptures = toggleCaptures,"
                , "         rowCaptures = rowCaptures,"
                , "         handledForeign = handledForeign,"
                , "         offsetAfterForeign = offsetAfterForeign,"
                , "         handledToggle = handledToggle,"
                , "         handledRow = handledRow,"
                , "         offsetAfterRow = afterRow.scrollOffset,"
                , "         closedRows = #closedDump.rows }"
                ]
            probe ← decodeOr r ∷ IO WheelProbe
            wpToggleCaptures probe `shouldBe` False
            wpRowCaptures probe `shouldBe` True
            wpHandledForeign probe `shouldBe` False
            wpOffsetAfterForeign probe `shouldBe` 0
            wpHandledToggle probe `shouldBe` False
            wpHandledRow probe `shouldBe` True
            wpOffsetAfterRow probe `shouldBe` 1
            -- Collapsed: no row elements at all, so nothing on this
            -- surface can capture the wheel away from gameplay.
            wpClosedRows probe `shouldBe` 0

        it "scrolls a list longer than the viewport, clamping at both ends" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1024 768 "wideTree(60)"
                , "th.setOpen(true);"
                , "local d = th.dump();"
                , "local top = d.rowIds[1];"
                , "th.setScrollOffset(1);"
                , "local oneDown = th.dump();"
                , "th.setScrollOffset(10000);"
                , "local clampedDown = th.dump();"
                , "th.setScrollOffset(-5);"
                , "local clampedUp = th.dump();"
                , "return { activeCount = d.activeCount, capacity = d.capacity,"
                , "         range = d.scrollRange, top = top,"
                , "         oneDownTop = oneDown.rowIds[1],"
                , "         oneDownCount = #oneDown.rows,"
                , "         maxOffset = clampedDown.scrollOffset,"
                , "         maxTop = clampedDown.rowIds[1],"
                , "         maxLast = clampedDown.rowIds[#clampedDown.rowIds],"
                , "         minOffset = clampedUp.scrollOffset }"
                ]
            probe ← decodeOr r ∷ IO ScrollProbe
            -- 60 subobjectives + the composite root.
            scActiveCount probe `shouldBe` 61
            scCapacity probe `shouldSatisfy` (\c → c > 0 ∧ c < 61)
            scRange probe `shouldBe` (61 - scCapacity probe)
            scTop probe `shouldBe` "root"
            scOneDownTop probe `shouldBe` "sub_001"
            scOneDownCount probe `shouldBe` scCapacity probe
            scMaxOffset probe `shouldBe` scRange probe
            -- Scrolled to the end: the last model row is on screen, and
            -- the window starts exactly `range` rows in (row 1 is the
            -- composite root, so row n+1 is sub_n).
            scMaxLast probe `shouldBe` "sub_060"
            scMaxTop probe `shouldBe` subId (scRange probe)
            scMinOffset probe `shouldBe` 0

        it "re-clamps a preserved offset when live content shrinks under it" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1024 768 "wideTree(60)"
                , "th.setOpen(true);"
                , "local d = th.dump();"
                , "th.setScrollOffset(d.scrollRange);"
                , "local deep = th.dump();"
                -- Subobjective checks are live and reversible, so the
                -- active row set genuinely shrinks during play.
                , "tp.setTree(wideTree(3)); th.rebuild();"
                , "local shrunk = th.dump();"
                , "return { deepOffset = deep.scrollOffset, deepRange = deep.scrollRange,"
                , "         shrunkOffset = shrunk.scrollOffset,"
                , "         shrunkRange = shrunk.scrollRange,"
                , "         shrunkRows = #shrunk.rows, shrunkTop = shrunk.rowIds[1] }"
                ]
            probe ← decodeOr r ∷ IO ClampProbe
            clDeepOffset probe `shouldSatisfy` (> 0)
            clDeepRange probe `shouldBe` clDeepOffset probe
            clShrunkRange probe `shouldBe` 0
            clShrunkOffset probe `shouldBe` 0
            clShrunkRows probe `shouldBe` 4
            clShrunkTop probe `shouldBe` Just "root"

    describe "the gameplay-surface resize lifecycle (requirements 5/6/8)" $ do
        it "a real framebuffer resize rebuilds exactly once and preserves open state and offset" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "wideTree(60)"
                , "th.setOpen(true); th.setScrollOffset(4);"
                , "local before = th.dump();"
                -- The REAL order: broadcastToModules fans the resize out
                -- in script-map order, so this module can be reached
                -- while hud still holds the PRE-resize toolbar;
                -- ui_manager rebuilds hud and only then reflows this
                -- surface.
                , "th.onFramebufferResize(1920, 1080);"
                , "local broadcast = th.dump();"
                , "hud.init(1, 2, 1920, 1080); hud.createUI();"
                , "th.reflow();"
                , "local after = th.dump();"
                -- Clear of the toolbar clusters at their NEW positions,
                -- which only holds if the rebuild ran after hud's.
                , "local reserved = require('scripts.ui.reserved_regions');"
                , "local clear = true;"
                , "for _, rc in ipairs(hud.getToolbarRects()) do"
                , "  if reserved.rectsOverlap({x=after.toggle.x, y=after.toggle.y,"
                , "      w=after.toggle.w, h=after.toggle.h}, rc) then clear = false end end;"
                , "return { beforeRebuilds = before.rebuildCount,"
                , "         broadcastRebuilds = broadcast.rebuildCount,"
                , "         afterRebuilds = after.rebuildCount,"
                , "         beforeOpen = before.open, afterOpen = after.open,"
                , "         beforeOffset = before.scrollOffset,"
                , "         afterOffset = after.scrollOffset,"
                , "         clearOfToolbars = clear,"
                , "         movedRight = (after.toggle.x > before.toggle.x) }"
                ]
            probe ← decodeOr r ∷ IO ResizeProbe
            -- The broadcast half must NOT rebuild: hud.getToolbarRects()
            -- still describes the pre-resize toolbar at that point, so a
            -- rebuild there would anchor the toggle against stale bounds.
            rzBroadcastRebuilds probe `shouldBe` rzBeforeRebuilds probe
            -- Requirement 8: ONE rebuild per real resize across the whole
            -- path — the module stays out of ui_manager_boot's manual
            -- onFramebufferResize forward set (which would double-fire
            -- it) and takes its single rebuild from the post-hud reflow,
            -- exactly like popup/unit_info_v2.
            rzAfterRebuilds probe - rzBeforeRebuilds probe `shouldBe` 1
            (rzBeforeOpen probe, rzAfterOpen probe) `shouldBe` (True, True)
            (rzBeforeOffset probe, rzAfterOffset probe) `shouldBe` (4, 4)
            rzMovedRight probe `shouldBe` True
            rzClearOfToolbars probe `shouldBe` True

        it "a scale-only change reaches it exactly once through uiManager.notifyGameplayRescale" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                -- Everything except the module under test is stubbed,
                -- the same technique UI.ResponsiveGameplay uses for
                -- this fan-out; the hud stub keeps the fields the
                -- tutorial HUD reads off it.
                , "local stub = { onFramebufferResize = function() end,"
                , "               reflow = function() end };"
                , "local hudStub = { onFramebufferResize = function() end,"
                , "                  visible = true, menuFont = 2, boxTexSet = 1,"
                , "                  getToolbarRects = function() return {} end };"
                , "local tp = require('scripts.tutorial_progress');"
                , "tp.reset(); tp.setTree(wideTree(60));"
                , "local th = require('scripts.tutorial_hud');"
                , "th.init(); th.reflow(1920, 1080);"
                , "th.setOpen(true); th.setScrollOffset(6);"
                , "package.loaded['scripts.world_view'] = stub;"
                , "package.loaded['scripts.hud'] = hudStub;"
                , "package.loaded['scripts.ui.context_menu'] = stub;"
                , "package.loaded['scripts.build_tool_remote_warning'] = stub;"
                , "package.loaded['scripts.popup'] = stub;"
                , "package.loaded['scripts.event_log'] = stub;"
                , "package.loaded['scripts.combat_log'] = stub;"
                , "package.loaded['scripts.injury_log_panel'] = stub;"
                , "package.loaded['scripts.unit_log'] = stub;"
                , "package.loaded['scripts.unit_info_v2'] = stub;"
                , "package.loaded['scripts.debug'] = stub;"
                , "package.loaded['scripts.test_arena'] = stub;"
                , "local uiManager = require('scripts.ui_manager');"
                , "uiManager.moduleReady.worldView = true;"
                , "uiManager.moduleReady.hud = true;"
                , "uiManager.moduleReady.buildToolRemoteWarning = true;"
                , "uiManager.moduleReady.popupsAndLogs = true;"
                , "uiManager.moduleReady.testArena = true;"
                , "local before = th.dump();"
                , "engine.setUIScale(2.0);"
                , "uiManager.notifyGameplayRescale(1920, 1080);"
                , "local after = th.dump();"
                , "return { beforeRebuilds = before.rebuildCount,"
                , "         afterRebuilds = after.rebuildCount,"
                , "         beforeOpen = before.open, afterOpen = after.open,"
                , "         beforeOffset = before.scrollOffset,"
                , "         afterOffset = after.scrollOffset,"
                , "         grewRows = (after.rowH > before.rowH) }"
                ]
            probe ← decodeOr r ∷ IO RescaleProbe
            rsclAfterRebuilds probe - rsclBeforeRebuilds probe `shouldBe` 1
            (rsclBeforeOpen probe, rsclAfterOpen probe) `shouldBe` (True, True)
            (rsclBeforeOffset probe, rsclAfterOffset probe) `shouldBe` (6, 6)
            -- The new scale really did reach the layout.
            rsclGrewRows probe `shouldBe` True

        it "keeps reachable right-anchored geometry and a scrollable list across every supported band" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                , "local responsive = require('scripts.ui.responsive');"
                , "local reserved = require('scripts.ui.reserved_regions');"
                , "local hud = require('scripts.hud');"
                , "local tp = require('scripts.tutorial_progress');"
                , "tp.reset(); tp.setTree(wideTree(60));"
                , "local th = require('scripts.tutorial_hud');"
                , "local out = {};"
                -- Derived from responsive.lua's own bands table (never
                -- hand-copied), at both scale bounds of each band.
                , "for _, band in ipairs(responsive.bands) do"
                , "  for _, h in ipairs({ band.minH, band.maxH }) do"
                , "    for _, sc in ipairs({ band.minScale, band.maxScale }) do"
                , "      local w = math.max(responsive.MIN_WIDTH, math.floor(h * 16 / 9));"
                , "      engine.setUIScale(sc);"
                , "      hud.init(1, 2, w, h); hud.createUI(); hud.visible = true;"
                , "      th.init(); th.reflow(w, h);"
                , "      local closed = th.dump();"
                , "      th.setOpen(true);"
                , "      local d = th.dump();"
                , "      local t = d.toggle;"
                , "      local clear = true;"
                , "      for _, rc in ipairs(hud.getToolbarRects()) do"
                , "        if reserved.rectsOverlap({x=t.x,y=t.y,w=t.w,h=t.h}, rc) then"
                , "          clear = false end end;"
                , "      out[#out + 1] = {"
                , "        w = w, h = h, scale = sc,"
                , "        toggleInFrame = (t.x >= 0 and t.y >= 0"
                , "            and (t.x + t.w) <= w and (t.y + t.h) <= h),"
                , "        toggleClear = clear,"
                , "        panelInFrame = (d.panelX >= 0 and (d.panelX + d.panelW) <= w),"
                , "        listInFrame = (d.listTop >= 0 and d.listBottom <= h"
                , "            and d.listTop <= d.listBottom),"
                , "        capacity = d.capacity,"
                , "        rangeOk = (d.scrollRange == math.max(0, d.activeCount - d.capacity)),"
                , "        toggleStable = (closed.toggle.x == t.x and closed.toggle.y == t.y"
                , "            and closed.toggle.w == t.w and closed.toggle.h == t.h) };"
                , "      th.setOpen(false);"
                , "    end end end;"
                , "return out"
                ]
            rows ← decodeOr r ∷ IO [BandProbe]
            -- 4 bands x 2 heights x 2 scales.
            length rows `shouldBe` 16
            forM_ rows $ \row → do
                let ctx = " at " ⧺ show (bpW row) ⧺ "x" ⧺ show (bpH row)
                            ⧺ " @" ⧺ show (bpScale row)
                (show (bpToggleInFrame row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpToggleClearOfToolbars row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpPanelInFrame row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpListInFrame row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpRangeOk row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpToggleStable row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                (show (bpCapacity row > 0) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)

        it "degrades safely (no crash, no invalid geometry) outside the supported envelope" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                , "local hud = require('scripts.hud');"
                , "local tp = require('scripts.tutorial_progress');"
                , "tp.reset(); tp.setTree(wideTree(60));"
                , "local th = require('scripts.tutorial_hud');"
                , "th.init();"
                -- The last two are DEGENERATE-but-positive sizes: a
                -- readable-width floor that ignored the framebuffer
                -- would push the toggle straight off the right edge
                -- there (review round 1). The 4K combo's scale is
                -- OUT OF BAND for its height (band 1601+ starts at
                -- 1.5) but IN the engine's 0.5–4.0 domain: a scale
                -- below 0.5 is refused by engine.setUIScale (#2198)
                -- and would silently leave the previous scale in
                -- place, making the case vacuous.
                , "local combos = { {320, 240, 4.0}, {800, 600, 4.0},"
                , "                 {3840, 2160, 0.5}, {640, 480, 0.5},"
                , "                 {12, 9, 1.0}, {1, 1, 4.0} };"
                , "local ok = true;"
                , "for _, c in ipairs(combos) do"
                , "  engine.setUIScale(c[3]);"
                , "  hud.init(1, 2, c[1], c[2]); hud.createUI();"
                , "  local good, err = pcall(function()"
                , "      th.reflow(c[1], c[2]); th.setOpen(true);"
                , "      local d = th.dump();"
                , "      assert(d.toggle.w > 0 and d.toggle.h > 0, 'toggle collapsed');"
                , "      assert(d.toggle.x >= 0 and d.toggle.y >= 0, 'toggle off-screen');"
                , "      assert(d.toggle.x + d.toggle.w <= c[1], 'toggle overflows width');"
                , "      assert(d.toggle.y + d.toggle.h <= c[2], 'toggle overflows height');"
                , "      assert(d.panelW > 0 and d.panelX >= 0, 'panel invalid');"
                , "      assert(d.capacity >= 0, 'negative capacity');"
                , "      assert(d.listTop >= 0 and d.listBottom >= d.listTop,"
                , "             'invalid list viewport');"
                , "      assert(d.scrollOffset >= 0 and d.scrollOffset <= d.scrollRange,"
                , "             'offset outside range');"
                , "      assert(#d.rows <= d.capacity, 'more rows than capacity');"
                , "      th.setOpen(false); end);"
                , "  if not good then ok = tostring(err) end end;"
                -- A 0x0 minimize must never be adopted as geometry,
                -- on either half of the resize path.
                , "local prior = th.dump();"
                , "th.onFramebufferResize(0, 0);"
                , "th.reflow(0, 0);"
                , "local after = th.dump();"
                , "return { ok = tostring(ok), minimizeIgnored = (after.fbW == prior.fbW"
                , "         and after.fbH == prior.fbH) }"
                ]
            probe ← decodeOr r ∷ IO DegradeProbe
            dgOk probe `shouldBe` "true"
            dgMinimizeIgnored probe `shouldBe` True

        it "cannot be driven below the engine's minimum UI scale: engine.setUIScale(0.25) is refused and the stored scale is unchanged (#2198)" $ \(env, ls) → do
            resetFixture env ls
            before ← vcUIScale ⊚ readIORef (videoConfigRef env)
            r ← evalOk ls "return engine.setUIScale(0.25)"
            r `shouldBe` "false"
            after ← vcUIScale ⊚ readIORef (videoConfigRef env)
            after `shouldBe` before

    -- #1419: the toggle's caption used to paint past its own box AND
    -- past the right edge of a 1280x720 frame ("> Objecti"), because
    -- the box was a bare constant and nothing ever measured the text.
    -- The runtime fix fits the box to the WIDER of the two captions and
    -- — only when the right edge cannot give it that much — shrinks the
    -- box and its font together through one local effective scale.
    describe "the toggle caption fit (#1419)" $ do

        -- The stub is the ResponsiveGameplay idiom, and it is what makes
        -- this group meaningful at all: synthetic font handles make the
        -- real engine.getTextWidth answer 0, which would leave the fit a
        -- no-op. It is deliberately CHARACTER-SENSITIVE so the two
        -- captions measure DIFFERENTLY (open's leading "v " is the wider
        -- one), which is what proves the fit reserves the wider variant
        -- rather than whichever one happens to be live. Restored before
        -- the case returns, so nothing else in the shared fixture sees
        -- it.
        it "keeps both captions inside the toggle box and the frame at every band endpoint" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                , "local responsive = require('scripts.ui.responsive');"
                , "local hud = require('scripts.hud');"
                , "local tp = require('scripts.tutorial_progress');"
                , "tp.reset(); tp.setTree(wideTree(60));"
                , "local th = require('scripts.tutorial_hud');"
                , "local origGTW = engine.getTextWidth;"
                , "engine.getTextWidth = function(_, text, size)"
                , "  local total = 0;"
                , "  for i = 1, #text do"
                , "    local c = string.sub(text, i, i);"
                , "    total = total + (((c == 'v') and 1.4) or 0.8) * size;"
                , "  end; return total end;"
                , "local combos = {};"
                -- Derived from responsive.lua's own bands table (never
                -- hand-copied), at both scale bounds of each band, and
                -- at BOTH the 16:9-derived width and responsive.
                -- MIN_WIDTH -- the narrow end (e.g. 800x2160 @4x) is
                -- where the right edge really does have to squeeze the
                -- control.
                , "for _, band in ipairs(responsive.bands) do"
                , "  for _, h in ipairs({ band.minH, band.maxH }) do"
                , "    for _, sc in ipairs({ band.minScale, band.maxScale }) do"
                , "      combos[#combos + 1] = { math.max(responsive.MIN_WIDTH,"
                , "                                math.floor(h * 16 / 9)), h, sc };"
                , "      combos[#combos + 1] = { responsive.MIN_WIDTH, h, sc };"
                , "    end end end;"
                , "local out = {};"
                , "for _, c in ipairs(combos) do"
                , "  local w, h, sc = c[1], c[2], c[3];"
                , "  engine.setUIScale(sc);"
                , "  hud.init(1, 2, w, h); hud.createUI(); hud.visible = true;"
                , "  th.init(); th.reflow(w, h);"
                , "  th.setOpen(false); local closed = th.dump();"
                , "  th.setOpen(true);  local opened = th.dump();"
                , "  local stable = (closed.toggle.x == opened.toggle.x"
                , "      and closed.toggle.y == opened.toggle.y"
                , "      and closed.toggle.w == opened.toggle.w"
                , "      and closed.toggle.h == opened.toggle.h);"
                , "  for _, d in ipairs({ closed, opened }) do"
                , "    local t = d.toggle;"
                , "    local live = math.ceil(engine.getTextWidth("
                , "        hud.menuFont, t.caption, t.fontSize));"
                , "    out[#out + 1] = {"
                , "      w = w, h = h, scale = sc, open = d.open,"
                , "      caption = t.caption, fontSize = t.fontSize,"
                , "      boxX = t.x, boxW = t.w,"
                , "      captionX = t.captionX, captionW = t.captionWidth,"
                , "      liveW = live,"
                , "      measured = (t.captionWidth > 0),"
                -- The reserved width must cover the caption ACTUALLY on
                -- screen, in either state.
                , "      coversLive = (t.captionWidth >= live),"
                -- Half-open pixel geometry, matching the existing
                -- in-frame checks: the last occupied column is
                -- captionX + captionW - 1.
                , "      fitsLeft = (t.captionX >= t.x),"
                , "      fitsRight = ((t.captionX + t.captionWidth) <= (t.x + t.w)),"
                , "      inFrame = ((t.x + t.w) <= w),"
                , "      boxStable = stable };"
                , "  end end;"
                , "engine.getTextWidth = origGTW;"
                , "return out"
                ]
            rows ← decodeOr r ∷ IO [CaptionProbe]
            -- 4 bands x 2 heights x 2 scales x 2 widths x 2 captions.
            length rows `shouldBe` 64
            length (filter cpOpen rows) `shouldBe` 32
            forM_ rows $ \row → do
                -- Both variants really were sampled, not one of them
                -- twice: the marker is what distinguishes them, and it
                -- is also the character the stub weighs differently.
                T.take 1 (cpCaption row)
                    `shouldBe` (if cpOpen row then "v" else ">")
                let ctx = " for " ⧺ T.unpack (cpCaption row)
                            ⧺ " at " ⧺ show (cpW row) ⧺ "x" ⧺ show (cpH row)
                            ⧺ " @" ⧺ show (cpScale row)
                            ⧺ " box x=" ⧺ show (cpBoxX row)
                            ⧺ " w=" ⧺ show (cpBoxW row)
                            ⧺ " caption x=" ⧺ show (cpCaptionX row)
                            ⧺ " w=" ⧺ show (cpCaptionW row)
                            ⧺ " live=" ⧺ show (cpLiveW row)
                            ⧺ " font=" ⧺ show (cpFontSize row)
                    yes f = (show (f row) ⧺ ctx) `shouldBe` ("True" ⧺ ctx)
                yes cpMeasured
                yes cpCoversLive
                yes cpFitsLeft
                yes cpFitsRight
                yes cpInFrame
                yes cpBoxStable

-- * Remaining decoded probe shapes

-- | One (band endpoint, caption variant) sample of #1419's fit.
data CaptionProbe = CaptionProbe
    { cpW ∷ Int, cpH ∷ Int, cpScale ∷ Double, cpOpen ∷ Bool
    , cpCaption ∷ Text, cpFontSize ∷ Double
    , cpBoxX ∷ Double, cpBoxW ∷ Double
    , cpCaptionX ∷ Double, cpCaptionW ∷ Double, cpLiveW ∷ Double
    , cpMeasured ∷ Bool, cpCoversLive ∷ Bool
    , cpFitsLeft ∷ Bool, cpFitsRight ∷ Bool, cpInFrame ∷ Bool
    , cpBoxStable ∷ Bool } deriving (Show, Eq)
instance FromJSON CaptionProbe where
    parseJSON = withObject "CaptionProbe" $ \o →
        CaptionProbe <$> o .: "w" <*> o .: "h" <*> o .: "scale"
                      <*> o .: "open" <*> o .: "caption" <*> o .: "fontSize"
                      <*> o .: "boxX" <*> o .: "boxW"
                      <*> o .: "captionX" <*> o .: "captionW" <*> o .: "liveW"
                      <*> o .: "measured" <*> o .: "coversLive"
                      <*> o .: "fitsLeft" <*> o .: "fitsRight"
                      <*> o .: "inFrame" <*> o .: "boxStable"

data ToggleProbe = ToggleProbe
    { tpOpenedRows ∷ Int, tpOpenedOpen ∷ Bool
    , tpReclosedRows ∷ Int, tpReclosedOpen ∷ Bool
    , tpSameX ∷ Bool, tpSameY ∷ Bool, tpSameW ∷ Bool, tpSameH ∷ Bool }
instance FromJSON ToggleProbe where
    parseJSON = withObject "ToggleProbe" $ \o →
        ToggleProbe <$> o .: "openedRows" <*> o .: "openedOpen"
                     <*> o .: "reclosedRows" <*> o .: "reclosedOpen"
                     <*> o .: "sameX" <*> o .: "sameY"
                     <*> o .: "sameW" <*> o .: "sameH"

data AssetProbe = AssetProbe
    { apEarlyAssets ∷ Bool, apEarlyRows ∷ Int, apEarlyToggleLabel ∷ Bool
    , apEarlyRowText ∷ Bool, apEarlyRebuilds ∷ Int
    , apReadyAssets ∷ Bool, apReadyRows ∷ Int, apReadyToggleLabel ∷ Bool
    , apReadyRowText ∷ Bool, apReadyRebuilds ∷ Int
    , apSettledRebuilds ∷ Int, apOpenKept ∷ Bool }
instance FromJSON AssetProbe where
    parseJSON = withObject "AssetProbe" $ \o →
        AssetProbe <$> o .: "earlyAssets" <*> o .: "earlyRows"
                    <*> o .: "earlyToggleLabel" <*> o .: "earlyRowText"
                    <*> o .: "earlyRebuilds"
                    <*> o .: "readyAssets" <*> o .: "readyRows"
                    <*> o .: "readyToggleLabel" <*> o .: "readyRowText"
                    <*> o .: "readyRebuilds" <*> o .: "settledRebuilds"
                    <*> o .: "openKept"

data ResetProbe = ResetProbe
    { rsBeforeOpen ∷ Bool, rsBeforeOffset ∷ Int
    , rsAfterOpen ∷ Bool, rsAfterOffset ∷ Int, rsProgressKept ∷ Bool }
instance FromJSON ResetProbe where
    parseJSON = withObject "ResetProbe" $ \o →
        ResetProbe <$> o .: "beforeOpen" <*> o .: "beforeOffset"
                    <*> o .: "afterOpen" <*> o .: "afterOffset"
                    <*> o .: "progressKept"

data CreateWorldProbe = CreateWorldProbe
    { cwAfterOpen ∷ Bool, cwAfterOffset ∷ Int, cwRestored ∷ Bool }
instance FromJSON CreateWorldProbe where
    parseJSON = withObject "CreateWorldProbe" $ \o →
        CreateWorldProbe <$> o .: "afterOpen" <*> o .: "afterOffset"
                          <*> o .: "restored"

data HudCycleProbe = HudCycleProbe
    { hcShown ∷ Bool, hcPageHidden ∷ Bool
    , hcHiddenOpen ∷ Bool, hcHiddenOffset ∷ Int
    , hcBackOpen ∷ Bool, hcBackOffset ∷ Int, hcBackRows ∷ Int
    , hcPageShown ∷ Bool }
instance FromJSON HudCycleProbe where
    parseJSON = withObject "HudCycleProbe" $ \o →
        HudCycleProbe <$> o .: "shown" <*> o .: "pageHidden"
                       <*> o .: "hiddenOpen" <*> o .: "hiddenOffset"
                       <*> o .: "backOpen" <*> o .: "backOffset"
                       <*> o .: "backRows" <*> o .: "pageShown"

data YamlProbe = YamlProbe
    { ypLoaded ∷ Bool, ypFirstIds ∷ Text, ypSecondIds ∷ Text
    , ypThirdIds ∷ Text, ypFirstLabel ∷ Text, ypFirstTooltip ∷ Text
    , ypTreeId ∷ Maybe Text }
instance FromJSON YamlProbe where
    parseJSON = withObject "YamlProbe" $ \o →
        YamlProbe <$> o .: "loaded" <*> o .: "firstIds" <*> o .: "secondIds"
                   <*> o .: "thirdIds" <*> o .: "firstLabel"
                   <*> o .: "firstTooltip" <*> o .:? "treeId"

data ReverseProbe = ReverseProbe { rvDoneIds ∷ Text, rvReopenedIds ∷ Text }
instance FromJSON ReverseProbe where
    parseJSON = withObject "ReverseProbe" $ \o →
        ReverseProbe <$> o .: "doneIds" <*> o .: "reopenedIds"

-- | #1941: the two views a retirement passes through — the build that
--   rendered the sticky rows, and the tick that reported them presented
--   and rebuilt without them.
-- | #2056: one reading of the checklist taken across the presentation
--   boundary — what is laid out, what the model still holds active,
--   whether a completed renderer snapshot has held THESE rows, and how
--   many rebuilds the observed window cost.
data BoundaryProbe = BoundaryProbe
    { bpRowIds ∷ Text, bpActiveIds ∷ Text
    , bpPresented ∷ Bool, bpRebuilds ∷ Int } deriving (Show, Eq)
instance FromJSON BoundaryProbe where
    parseJSON = withObject "BoundaryProbe" $ \o →
        BoundaryProbe <$> o .: "rowIds" <*> o .: "activeIds"
                       <*> o .: "presented" <*> o .: "rebuilds"

data WheelProbe = WheelProbe
    { wpToggleCaptures ∷ Bool, wpRowCaptures ∷ Bool
    , wpHandledForeign ∷ Bool, wpOffsetAfterForeign ∷ Int
    , wpHandledToggle ∷ Bool, wpHandledRow ∷ Bool
    , wpOffsetAfterRow ∷ Int, wpClosedRows ∷ Int }
instance FromJSON WheelProbe where
    parseJSON = withObject "WheelProbe" $ \o →
        WheelProbe <$> o .: "toggleCaptures" <*> o .: "rowCaptures"
                    <*> o .: "handledForeign" <*> o .: "offsetAfterForeign"
                    <*> o .: "handledToggle" <*> o .: "handledRow"
                    <*> o .: "offsetAfterRow" <*> o .: "closedRows"

data ScrollProbe = ScrollProbe
    { scActiveCount ∷ Int, scCapacity ∷ Int, scRange ∷ Int
    , scTop ∷ Text, scOneDownTop ∷ Text, scOneDownCount ∷ Int
    , scMaxOffset ∷ Int, scMaxTop ∷ Text, scMaxLast ∷ Text
    , scMinOffset ∷ Int }
instance FromJSON ScrollProbe where
    parseJSON = withObject "ScrollProbe" $ \o →
        ScrollProbe <$> o .: "activeCount" <*> o .: "capacity" <*> o .: "range"
                     <*> o .: "top" <*> o .: "oneDownTop" <*> o .: "oneDownCount"
                     <*> o .: "maxOffset" <*> o .: "maxTop" <*> o .: "maxLast"
                     <*> o .: "minOffset"

data ClampProbe = ClampProbe
    { clDeepOffset ∷ Int, clDeepRange ∷ Int, clShrunkOffset ∷ Int
    , clShrunkRange ∷ Int, clShrunkRows ∷ Int, clShrunkTop ∷ Maybe Text }
instance FromJSON ClampProbe where
    parseJSON = withObject "ClampProbe" $ \o →
        ClampProbe <$> o .: "deepOffset" <*> o .: "deepRange"
                    <*> o .: "shrunkOffset" <*> o .: "shrunkRange"
                    <*> o .: "shrunkRows" <*> o .:? "shrunkTop"

data ResizeProbe = ResizeProbe
    { rzBeforeRebuilds ∷ Int, rzBroadcastRebuilds ∷ Int, rzAfterRebuilds ∷ Int
    , rzBeforeOpen ∷ Bool, rzAfterOpen ∷ Bool
    , rzBeforeOffset ∷ Int, rzAfterOffset ∷ Int, rzMovedRight ∷ Bool
    , rzClearOfToolbars ∷ Bool }
instance FromJSON ResizeProbe where
    parseJSON = withObject "ResizeProbe" $ \o →
        ResizeProbe <$> o .: "beforeRebuilds" <*> o .: "broadcastRebuilds"
                     <*> o .: "afterRebuilds"
                     <*> o .: "beforeOpen" <*> o .: "afterOpen"
                     <*> o .: "beforeOffset" <*> o .: "afterOffset"
                     <*> o .: "movedRight" <*> o .: "clearOfToolbars"

data RescaleProbe = RescaleProbe
    { rsclBeforeRebuilds ∷ Int, rsclAfterRebuilds ∷ Int
    , rsclBeforeOpen ∷ Bool, rsclAfterOpen ∷ Bool
    , rsclBeforeOffset ∷ Int, rsclAfterOffset ∷ Int, rsclGrewRows ∷ Bool }
instance FromJSON RescaleProbe where
    parseJSON = withObject "RescaleProbe" $ \o →
        RescaleProbe <$> o .: "beforeRebuilds" <*> o .: "afterRebuilds"
                      <*> o .: "beforeOpen" <*> o .: "afterOpen"
                      <*> o .: "beforeOffset" <*> o .: "afterOffset"
                      <*> o .: "grewRows"

data DegradeProbe = DegradeProbe { dgOk ∷ Text, dgMinimizeIgnored ∷ Bool }
instance FromJSON DegradeProbe where
    parseJSON = withObject "DegradeProbe" $ \o →
        DegradeProbe <$> o .: "ok" <*> o .: "minimizeIgnored"

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
