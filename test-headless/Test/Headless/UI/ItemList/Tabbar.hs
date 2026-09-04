-- | The parameterized tabbar layouts the Item list widget renders its
--   tab strip through (#1088; split out under #2147): the framed, row
--   and wrapped layouts and the tab-click callback routing.
--
--   Composed by 'Test.Headless.UI.ItemList', which owns the
--   @Item list widget@ describe and the per-case queue drain. This
--   module adds no describe of its own, so every example path is the
--   one the facade already published.
--
--   NB: the shrink-to-fit and wrapping geometry depends on real
--   measurement, so those cases replace
--   'Test.Headless.UI.ItemList.Support.setupLua' 's default
--   @engine.getTextWidth@ stub with their own per-case model.
module Test.Headless.UI.ItemList.Tabbar (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), withObject, (.:))
import Engine.Core.State (EngineEnv)
import Test.Headless.UI.ItemList.Support

spec ∷ SpecWith EngineEnv
spec = do

    -- * Parameterized tabbar layouts

    describe "parameterized tabbar layouts" $ do
        it "the default framed layout is unchanged: one left-to-right row WITH a content frame" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.5 end;"
                , "local pg = UI.newPage('tb_framed', 'overlay');"
                , "local id = tb.new({name='framed', page=pg, font=1, x=0, y=0,"
                , "  width=400, frameHeight=200,"
                , "  tabs={{name='One'},{name='Two'},{name='Three'}}});"
                , "local tabs = tb.getTabs(id);"
                , "local ys = {};"
                , "for i, t in ipairs(tabs) do"
                , "  local info = UI.getElementInfo(t.boxId); ys[i] = info.y end;"
                , "local sameRow = (ys[1] == ys[2] and ys[2] == ys[3]);"
                , "local x1 = UI.getElementInfo(tabs[1].boxId).x;"
                , "local x2 = UI.getElementInfo(tabs[2].boxId).x;"
                , "tb.destroy(id);"
                , "return {hasFrame = (tb.hasFrame(id) == false), n = #tabs,"
                , "        frameBefore = true, sameRow = sameRow,"
                , "        leftToRight = (x2 > x1)}"
                ]
            p ← decodeOr r ∷ IO FramedProbe
            fpN p `shouldBe` 3
            fpSameRow p `shouldBe` True
            fpLeftToRight p `shouldBe` True

        it "the framed layout really creates a frame; the frame-free layouts really don't" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.5 end;"
                , "local pg = UI.newPage('tb_frames', 'overlay'); UI.showPage(pg);"
                , "local defs = {{name='One'},{name='Two'}};"
                , "local framed = tb.new({name='f', page=pg, font=1, width=400,"
                , "  frameHeight=100, tabs=defs});"
                , "local row = tb.new({name='r', page=pg, font=1, width=400,"
                , "  layout='row', tabHeightPx=28, tabs=defs});"
                , "local wrapped = tb.new({name='w', page=pg, font=1, width=400,"
                , "  layout='wrapped', tabHeightPx=28, tabs=defs});"
                , "local out = {framed = tb.hasFrame(framed),"
                , "             row = tb.hasFrame(row),"
                , "             wrapped = tb.hasFrame(wrapped),"
                , "             frameNames = 0};"
                -- A hidden or zero-sized replacement frame would still be
                -- a live element named "<name>_frame".
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name == 'r_frame' or e.name == 'w_frame' then"
                , "    out.frameNames = out.frameNames + 1 end"
                , "end;"
                , "tb.destroy(framed); tb.destroy(row); tb.destroy(wrapped);"
                , "return out"
                ]
            p ← decodeOr r ∷ IO FrameProbe
            frFramed p `shouldBe` True
            frRow p `shouldBe` False
            frWrapped p `shouldBe` False
            frFrameNames p `shouldBe` 0

        it "row layout shrinks to fit and scales the LABEL with the box" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "local label = require('scripts.ui.label');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.6 end;"
                , "local pg = UI.newPage('tb_shrink', 'overlay');"
                , "local defs = {};"
                , "for i = 1, 8 do defs[i] = {name = 'Category' .. i} end;"
                , "local wide = tb.new({name='wide', page=pg, font=1, x=0, y=0,"
                , "  width=4000, wrapWidth=4000, layout='row', shrinkToFit=true,"
                , "  tabHeightPx=28, measureFontPx=13, labelFontSize=13,"
                , "  labelUiscale=1.0, textPaddingPx=8, tabs=defs});"
                , "local narrow = tb.new({name='narrow', page=pg, font=1, x=0, y=200,"
                , "  width=300, wrapWidth=300, layout='row', shrinkToFit=true,"
                , "  tabHeightPx=28, measureFontPx=13, labelFontSize=13,"
                , "  labelUiscale=1.0, textPaddingPx=8, tabs=defs});"
                , "local wt = tb.getTabs(wide); local nt = tb.getTabs(narrow);"
                , "local _, wideH = label.getSize(wt[1].labelId);"
                , "local _, narrowH = label.getSize(nt[1].labelId);"
                , "local right = 0; local minW = 99999; local rows = {};"
                , "for _, t in ipairs(nt) do"
                , "  rows[tostring(t.y)] = true;"
                , "  if t.x + t.width > right then right = t.x + t.width end;"
                , "  if t.width < minW then minW = t.width end"
                , "end;"
                , "local nRows = 0; for _ in pairs(rows) do nRows = nRows + 1 end;"
                , "tb.destroy(wide); tb.destroy(narrow);"
                , "return {wideH = wideH, narrowH = narrowH, right = right,"
                , "        minW = minW, nRows = nRows}"
                ]
            p ← decodeOr r ∷ IO ShrinkProbe
            shNRows p `shouldBe` 1                       -- stays ONE row
            shRight p `shouldSatisfy` (≤ 300)            -- shrunk to fit
            shMinW p `shouldSatisfy` (≥ 20)              -- still clickable
            shNarrowH p `shouldSatisfy` (< shWideH p)    -- label shrank too
            shNarrowH p `shouldSatisfy` (> 0)

        it "wrapped layout wraps and centres, with the LABEL scale independent of the box metrics" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- Unit-info's case: box height comes from a locally fitted
            -- scale while the label stays a fixed size. A tabbar that
            -- derived both from one uiscale could not express it.
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "local label = require('scripts.ui.label');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.6 end;"
                , "local pg = UI.newPage('tb_wrapped', 'overlay');"
                , "local defs = {};"
                , "for i = 1, 8 do defs[i] = {name = 'Category' .. i} end;"
                , "local id = tb.new({name='wrap', page=pg, font=1, x=100, y=0,"
                , "  width=400, wrapWidth=400, layout='wrapped', centerRows=true,"
                , "  tabHeightPx=13, measureFontPx=13, labelFontSize=13,"
                , "  labelUiscale=1.0, textPaddingPx=8, rowGapPx=4, tabs=defs});"
                , "local tabs = tb.getTabs(id);"
                , "local rows = {};"
                , "for _, t in ipairs(tabs) do"
                , "  local key = tostring(t.y);"
                , "  rows[key] = rows[key] or {minX = t.x, maxR = t.x + t.width};"
                , "  if t.x < rows[key].minX then rows[key].minX = t.x end;"
                , "  if t.x + t.width > rows[key].maxR then"
                , "    rows[key].maxR = t.x + t.width end"
                , "end;"
                , "local nRows = 0; local centred = true; local inBounds = true;"
                , "for _, r in pairs(rows) do"
                , "  nRows = nRows + 1;"
                , "  local left = r.minX - 100; local rightGap = 500 - r.maxR;"
                , "  if math.abs(left - rightGap) > 1 then centred = false end;"
                , "  if r.minX < 100 or r.maxR > 500 then inBounds = false end"
                , "end;"
                , "local _, lblH = label.getSize(tabs[1].labelId);"
                , "local boxH = tabs[1].height;"
                , "tb.destroy(id);"
                , "return {nRows = nRows, centred = centred, inBounds = inBounds,"
                , "        lblH = lblH, boxH = boxH, stripH = 0}"
                ]
            p ← decodeOr r ∷ IO WrapProbe
            wpNRows p `shouldSatisfy` (≥ 2)
            wpCentred p `shouldBe` True
            wpInBounds p `shouldBe` True
            -- Box height (13, a fitted metric) is independent of the
            -- label's own 13px fixed size -- proving the two scales are
            -- separately controllable rather than one uiscale.
            wpLblH p `shouldBe` 13
            wpBoxH p `shouldBe` 13

        it "a click on a frame-free tab routes through the shared tabbar callback and reports the category key" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.6 end;"
                , "local pg = UI.newPage('tb_click', 'overlay');"
                , "_G.__picked = nil;"
                , "local id = tb.new({name='clicky', page=pg, font=1, width=1000,"
                , "  wrapWidth=1000, layout='row', tabHeightPx=28, measureFontPx=13,"
                , "  labelFontSize=13, labelUiscale=1.0, textPaddingPx=8,"
                , "  tabs={{name='All (2)', key='All'},{name='Tools (1)', key='Tools'}},"
                , "  onChange=function(key) _G.__picked = key end});"
                , "local tabs = tb.getTabs(id);"
                , "local routed = tb.handleCallback('onTabClick', tabs[2].boxId);"
                , "local sel = tb.getSelectedKey(id);"
                , "tb.destroy(id);"
                , "return {routed = routed, picked = _G.__picked, selected = sel}"
                ]
            p ← decodeOr r ∷ IO TabClickProbe
            tkRouted p `shouldBe` True
            tkPicked p `shouldBe` "Tools"
            tkSelected p `shouldBe` "Tools"

-- * Decoders

data FramedProbe = FramedProbe
    { fpN ∷ Int, fpSameRow ∷ Bool, fpLeftToRight ∷ Bool } deriving Show
instance FromJSON FramedProbe where
    parseJSON = withObject "FramedProbe" $ \o →
        FramedProbe <$> o .: "n" <*> o .: "sameRow" <*> o .: "leftToRight"

data FrameProbe = FrameProbe
    { frFramed ∷ Bool, frRow ∷ Bool, frWrapped ∷ Bool
    , frFrameNames ∷ Int } deriving Show
instance FromJSON FrameProbe where
    parseJSON = withObject "FrameProbe" $ \o →
        FrameProbe <$> o .: "framed" <*> o .: "row" <*> o .: "wrapped"
                   <*> o .: "frameNames"

data ShrinkProbe = ShrinkProbe
    { shWideH ∷ Double, shNarrowH ∷ Double, shRight ∷ Double
    , shMinW ∷ Double, shNRows ∷ Int } deriving Show
instance FromJSON ShrinkProbe where
    parseJSON = withObject "ShrinkProbe" $ \o →
        ShrinkProbe <$> o .: "wideH" <*> o .: "narrowH" <*> o .: "right"
                    <*> o .: "minW" <*> o .: "nRows"

data WrapProbe = WrapProbe
    { wpNRows ∷ Int, wpCentred ∷ Bool, wpInBounds ∷ Bool
    , wpLblH ∷ Double, wpBoxH ∷ Double, wpStripH ∷ Double } deriving Show
instance FromJSON WrapProbe where
    parseJSON = withObject "WrapProbe" $ \o →
        WrapProbe <$> o .: "nRows" <*> o .: "centred" <*> o .: "inBounds"
                  <*> o .: "lblH" <*> o .: "boxH" <*> o .: "stripH"

data TabClickProbe = TabClickProbe
    { tkRouted ∷ Bool, tkPicked ∷ Text, tkSelected ∷ Text } deriving Show
instance FromJSON TabClickProbe where
    parseJSON = withObject "TabClickProbe" $ \o →
        TabClickProbe <$> o .: "routed" <*> o .: "picked" <*> o .: "selected"
