-- | The Tutorial HUD's presentation LIFECYCLE (#960 requirements 2/5):
--   the state a fresh boot starts in, the toggle, the two resets a save
--   load and a new world funnel through, late asset readiness, and the
--   HUD hide/show round trip.
--
--   A fixture-consuming fragment of 'Test.Headless.UI.TutorialHud':
--   the engine, the Lua backend and the per-case reset all belong to
--   'Test.Headless.UI.TutorialHud.Support', and this module boots
--   neither.
module Test.Headless.UI.TutorialHud.Lifecycle (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), withObject, (.:))
import Test.Headless.UI.TutorialHud.Support
    ( Fixture, bootAt, decodeOr, evalOk, luaLines, resetFixture, treeHelpers )

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

spec ∷ SpecWith Fixture
spec = do

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
