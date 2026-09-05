-- | #1419's measured toggle-caption fit: the one group in this suite
--   that STUBS @engine.getTextWidth@, because the shared fixture's
--   synthetic font handles measure 0 and would leave the fit a no-op.
--   The stub is character-sensitive, both caption variants are sampled
--   at every responsive-band endpoint, and the original measurement
--   function is restored before the case returns so nothing else in the
--   shared fixture ever sees it.
--
--   A fixture-consuming fragment of 'Test.Headless.UI.TutorialHud':
--   the engine, the Lua backend and the per-case reset all belong to
--   'Test.Headless.UI.TutorialHud.Support', and this module boots
--   neither.
module Test.Headless.UI.TutorialHud.CaptionFit (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), withObject, (.:))
import qualified Data.Text as T
import Test.Headless.UI.TutorialHud.Support
    ( Fixture, decodeOr, evalOk, luaLines, resetFixture, treeHelpers )

-- * Decoded probe shapes

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

spec ∷ SpecWith Fixture
spec = do

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
