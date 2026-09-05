-- | The Tutorial HUD's scoped wheel capture and scrolling (requirements
--   4/7): only the visible list captures the wheel, offsets clamp at
--   both ends, and a preserved offset re-clamps when live content
--   shrinks under it.
--
--   A fixture-consuming fragment of 'Test.Headless.UI.TutorialHud':
--   the engine, the Lua backend and the per-case reset all belong to
--   'Test.Headless.UI.TutorialHud.Support', and this module boots
--   neither.
module Test.Headless.UI.TutorialHud.Scrolling (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Test.Headless.UI.TutorialHud.Support
    ( Fixture, bootAt, decodeOr, evalOk, luaLines, resetFixture, subId )

-- * Decoded probe shapes

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

spec ∷ SpecWith Fixture
spec = do

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
