-- | The missing-glyph fallback (#1097).
--
--   Before this, a character the font could not draw produced no quad
--   and no advance, and said nothing. Three separate sites had to agree
--   for that to stay merely wrong rather than corrupting: the two
--   'layoutText' variants and 'calculateTextWidth'. They agreed by all
--   yielding nothing; they now have to agree on the fallback mark's
--   advance instead, which is what the bulk of this module checks.
--
--   None of it needs a GPU. Atlas generation is pure stb-in-IO, so the
--   two real fonts here are generated for real — gothic.ttf is missing
--   fifteen printable ASCII characters from its cmap, which is exactly
--   the case a map-key check cannot see (stb resolves those to glyph 0
--   and hands back .notdef pixels).
module Test.Headless.Graphics.FontFallback (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf, sort)
import Engine.Asset.Handle (FontHandle(..), TextureHandle(..))
import Engine.Asset.Types (GlyphInfo(..))
import Engine.Core.Log
  (LogConfig(..), LoggerState, defaultLogConfig, initLogger)
import Engine.Graphics.Font.Atlas (nextPowerOf2)
import Engine.Graphics.Font.Data
  (FontAtlas(..), FontCache(..), GlyphInstance(..), defaultFontCache)
import Engine.Graphics.Font.Draw (layoutText, layoutTextUI)
import Engine.Graphics.Font.Fallback
  ( FallbackMark(..), fallbackMark, fallbackOnEdge, isIntentionallyEmpty
  , isMissingGlyph, missingGlyphMessage, missingGlyphs, resolveGlyph
  , takeUnreportedMissingGlyphs )
import Engine.Graphics.Font.Repertoire (printableAscii)
import Engine.Graphics.Font.SDF (generateSDFFontAtlas, sdfAtlasErrorMessage)
import Engine.Graphics.Font.Util (calculateTextWidthScaled)

-- | The fifteen printable ASCII characters gothic.ttf's cmap does not
--   map. Written out rather than derived, so a font swap that quietly
--   changes coverage fails here instead of testing nothing.
gothicGaps ∷ [Char]
gothicGaps = "/:;<=>[\\]^`{|}~"

-- | A font whose cmap covers all of @[' '..'~']@.
completeFontPath ∷ FilePath
completeFontPath = "assets/fonts/arcade.ttf"

-- | A font that does not.
narrowFontPath ∷ FilePath
narrowFontPath = "assets/fonts/gothic.ttf"

bakedRange ∷ [Char]
bakedRange = [' '..'~']

white ∷ (Float, Float, Float, Float)
white = (1, 1, 1, 1)

-- * Synthetic atlas
--
--   Metrics derived from the character code so a substituted glyph is
--   visible in the failure, and a fallback mark that resembles none of
--   them.

syntheticGlyph ∷ Char → GlyphInfo
syntheticGlyph c = GlyphInfo
    { giUVRect  = (n / 256, 0, (n + 8) / 256, 8 / 256)
    , giSize    = (n / 4, n / 3)
    , giBearing = (1, negate (n / 3))
    , giAdvance = n / 4 + 2
    }
  where n = fromIntegral (fromEnum c) ∷ Float

syntheticFallback ∷ GlyphInfo
syntheticFallback = GlyphInfo
    { giUVRect  = (0.5, 0.5, 0.75, 0.75)
    , giSize    = (22, 33)
    , giBearing = (2, -33)
    , giAdvance = 26
    }

-- | An atlas covering exactly the characters named.
syntheticAtlas ∷ [Char] → FontAtlas
syntheticAtlas covered = FontAtlas
    { faTexture       = TextureHandle 0
    , faGlyphData     = Map.fromList [ (c, syntheticGlyph c) | c ← covered ]
    , faFallbackGlyph = syntheticFallback
    , faAtlasWidth    = 256
    , faAtlasHeight   = 256
    , faFontSize      = 48
    , faLineHeight    = 60
    , faBaseline      = 40
    , faAtlasBitmap   = []
    , faDescriptorSet = Nothing
    , faImageView     = Nothing
    , faSampler       = Nothing
    }

-- * The pre-#1097 algorithms, reproduced
--
--   Written out verbatim rather than reused, for the same reason
--   "Test.Headless.Graphics.UniformLayout" spells its offsets out: a
--   regression check that shares code with the implementation agrees
--   with any mistake in it.

legacyWidth ∷ FontAtlas → Float → String → Double
legacyWidth atlas desiredSize str =
    let base = sum [ maybe 0 (realToFrac . giAdvance)
                           (Map.lookup c (faGlyphData atlas))
                   | c ← str ]
    in base * (realToFrac desiredSize / fromIntegral (faFontSize atlas))

legacyLayoutUI ∷ FontAtlas → Float → Float → Float → Text
               → (Float, Float, Float, Float) → V.Vector GlyphInstance
legacyLayoutUI atlas desiredSize startX startY text color =
    let scaleFactor = desiredSize / fromIntegral (faFontSize atlas)
        (_, instances) = foldl (step scaleFactor) (startX, []) (T.unpack text)
    in V.fromList (reverse instances)
  where
    step scaleFactor (currentX, acc) char =
        case Map.lookup char (faGlyphData atlas) of
            Nothing → (currentX, acc)
            Just gi →
                let (bearingX, bearingY) = giBearing gi
                    (w, h) = giSize gi
                    inst = GlyphInstance
                        { instancePosition =
                            (currentX + bearingX * scaleFactor
                            , startY + bearingY * scaleFactor)
                        , instanceSize = (w * scaleFactor, h * scaleFactor)
                        , instanceUVRect = giUVRect gi
                        , instanceColor = color }
                in (currentX + giAdvance gi * scaleFactor, inst : acc)

legacyLayoutWorld ∷ FontAtlas → Float → Float → Float → Float → Float
                  → Text → (Float, Float, Float, Float)
                  → V.Vector GlyphInstance
legacyLayoutWorld atlas desiredSize startX startY screenW screenH text color =
    let scaleFactor = desiredSize / fromIntegral (faFontSize atlas)
        (_, instances) = foldl (step scaleFactor) (startX, []) (T.unpack text)
    in V.fromList (reverse instances)
  where
    step scaleFactor (currentX, acc) char =
        case Map.lookup char (faGlyphData atlas) of
            Nothing → (currentX, acc)
            Just gi →
                let (bearingX, bearingY) = giBearing gi
                    (w, h) = giSize gi
                    pxX = currentX + bearingX * scaleFactor
                    pxY = startY - bearingY * scaleFactor
                    inst = GlyphInstance
                        { instancePosition =
                            ( (pxX / screenW) * 2.0 - 1.0
                            , 1.0 - (pxY / screenH) * 2.0 )
                        , instanceSize =
                            ( (w * scaleFactor / screenW) * 2.0
                            , (h * scaleFactor / screenH) * 2.0 )
                        , instanceUVRect = giUVRect gi
                        , instanceColor = color }
                in (currentX + giAdvance gi * scaleFactor, inst : acc)

-- * Helpers

quietLogger ∷ IO LoggerState
quietLogger = initLogger defaultLogConfig { lcEnableByDefault = False }

-- | A generous device limit, so the packing planner (#1098) is never
--   the reason a fixture fails to build.
roomyLimit ∷ Int
roomyLimit = 16384

-- | The printable-ASCII atlas this module has always tested. Since
--   #1098 the repertoire is a parameter, so it is named explicitly
--   rather than assumed.
asciiAtlas ∷ FilePath → IO FontAtlas
asciiAtlas path = do
    logger ← quietLogger
    result ← generateSDFFontAtlas logger path printableAscii roomyLimit
    case result of
        Left err → error $ "atlas generation failed: "
                             ⧺ T.unpack (sdfAtlasErrorMessage err)
        Right atlas → return atlas

-- | The pen position the layout reached, read off the trailing glyph:
--   its quad sits at @penX + bearingX * scale@.
penAfter ∷ FontAtlas → Float → Text → Char → Float
penAfter atlas size text tail' =
    let insts = layoutTextUI atlas size 0 0 (text <> T.singleton tail') white
        scaleFactor = size / fromIntegral (faFontSize atlas)
        bearingX = maybe 0 (fst . giBearing) (resolveGlyph atlas tail')
    in case V.toList (V.drop (V.length insts - 1) insts) of
        [GlyphInstance (x, _) _ _ _] → x - bearingX * scaleFactor
        _ → error "penAfter: trailing glyph produced no quad"

-- | Float comparison with room for the Double↔Float hop between
--   measurement and layout.
shouldBeNear ∷ Float → Float → Expectation
shouldBeNear actual expected =
    abs (actual - expected) `shouldSatisfy` (< 1e-3)

-- | Samples of the atlas inside a glyph's UV rect.
samplesUnder ∷ FontAtlas → GlyphInfo → [Word8]
samplesUnder atlas gi =
    let bitmap = VU.fromList (faAtlasBitmap atlas)
        aw = faAtlasWidth atlas
        ah = faAtlasHeight atlas
        (u0, v0, u1, v1) = giUVRect gi
        px u = round (u * fromIntegral aw) ∷ Int
        py v = round (v * fromIntegral ah) ∷ Int
    in [ bitmap VU.! (y * aw + x)
       | y ← [py v0 .. py v1 - 1], x ← [px u0 .. px u1 - 1]
       , y ≥ 0, y < ah, x ≥ 0, x < aw, y * aw + x < VU.length bitmap ]

-- | Do two UV rects overlap?
uvOverlaps ∷ GlyphInfo → GlyphInfo → Bool
uvOverlaps a b =
    let (au0, av0, au1, av1) = giUVRect a
        (bu0, bv0, bu1, bv1) = giUVRect b
    in au0 < bu1 ∧ bu0 < au1 ∧ av0 < bv1 ∧ bv0 < av1

spec ∷ Spec
spec = do
    -- Two real atlases, generated once and shared. Both go through the
    -- production generator; no GPU is involved.
    completeAtlas ← runIO $ asciiAtlas completeFontPath
    narrowAtlas ← runIO $ asciiAtlas narrowFontPath

    describe "resolution" $ do
        let atlas = syntheticAtlas "AB "

        it "substitutes the fallback mark for a character not in the map" $ do
            resolveGlyph atlas 'Z' `shouldBe` Just syntheticFallback
            isMissingGlyph atlas 'Z' `shouldBe` True

        it "leaves a covered character alone" $ do
            resolveGlyph atlas 'A' `shouldBe` Just (syntheticGlyph 'A')
            isMissingGlyph atlas 'A' `shouldBe` False

        it "keeps space's own advance and draws nothing for it" $ do
            resolveGlyph atlas ' ' `shouldBe` Just (syntheticGlyph ' ')
            isMissingGlyph atlas ' ' `shouldBe` False
            V.length (layoutTextUI atlas 48 0 0 " " white) `shouldBe` 1

        it "never marks tab, CR or LF, which the atlas deliberately lacks" $
            forM_ ['\t', '\r', '\n'] $ \c → do
                isIntentionallyEmpty c `shouldBe` True
                resolveGlyph atlas c `shouldBe` Nothing
                isMissingGlyph atlas c `shouldBe` False
                layoutTextUI atlas 48 0 0 (T.singleton c) white
                    `shouldBe` V.empty
                calculateTextWidthScaled atlas 48 [c] `shouldBe` 0

    describe "measurement and layout agree" $ do
        let atlas = syntheticAtlas "AB "
            probe = "A?B"   -- '?' is absent

        it "measures a missing character as the fallback's advance" $ do
            calculateTextWidthScaled atlas 48 "?" `shouldBe`
                realToFrac (giAdvance syntheticFallback)
            calculateTextWidthScaled atlas 48 "?" `shouldSatisfy` (> 0)

        it "lays out a visible quad for a missing character" $ do
            let insts = layoutTextUI atlas 48 0 0 "?" white
            V.length insts `shouldBe` 1
            case V.head insts of
                GlyphInstance _ (w, h) uv _ → do
                    w `shouldSatisfy` (> 0)
                    h `shouldSatisfy` (> 0)
                    uv `shouldBe` giUVRect syntheticFallback

        it "advances the UI pen by exactly what measurement reports" $
            forM_ [24, 48, 96] $ \size →
                penAfter atlas size probe 'A' `shouldBeNear`
                    realToFrac (calculateTextWidthScaled atlas size (T.unpack probe))

        it "advances the world pen by the same amount" $ do
            let insts = layoutText atlas 48 0 0 800 600 (probe <> "A") white
                uiInsts = layoutTextUI atlas 48 0 0 (probe <> "A") white
            V.length insts `shouldBe` V.length uiInsts
            V.length insts `shouldBe` 4

        it "reports every distinct missing character once, in order" $
            missingGlyphs atlas "?A??!" `shouldBe` "?!"

    describe "covered text is untouched" $ do
        let covered = syntheticAtlas bakedRange
            sample = "The quick brown fox, 0123456789!"

        it "measures a fully covered string exactly as before" $
            forM_ [12, 32.5, 96] $ \size →
                calculateTextWidthScaled covered size (T.unpack sample)
                    `shouldBe` legacyWidth covered size (T.unpack sample)

        it "lays out UI text identically to the pre-fallback algorithm" $
            layoutTextUI covered 32 17 43 sample white
                `shouldBe` legacyLayoutUI covered 32 17 43 sample white

        it "lays out world text identically to the pre-fallback algorithm" $
            layoutText covered 32 17 43 1280 720 sample white
                `shouldBe` legacyLayoutWorld covered 32 17 43 1280 720 sample white

        it "agrees with the real complete atlas too" $ do
            missingGlyphs completeAtlas (T.pack bakedRange) `shouldBe` []
            layoutTextUI completeAtlas 32 5 9 sample white
                `shouldBe` legacyLayoutUI completeAtlas 32 5 9 sample white
            calculateTextWidthScaled completeAtlas 32 (T.unpack sample)
                `shouldBe` legacyWidth completeAtlas 32 (T.unpack sample)

    describe "generated atlases" $ do
        it "publishes every character a complete font covers" $ do
            sort (Map.keys (faGlyphData completeAtlas)) `shouldBe` bakedRange

        it "omits the characters a narrow font's cmap does not map" $ do
            -- stb rasterizes these through glyph 0, so they exist as
            -- pixels; only the cmap knows they are not real.
            sort (Map.keys (faGlyphData narrowAtlas))
                `shouldBe` filter (`notElem` gothicGaps) bakedRange
            forM_ gothicGaps $ \c → do
                isMissingGlyph narrowAtlas c `shouldBe` True
                resolveGlyph narrowAtlas c
                    `shouldBe` Just (faFallbackGlyph narrowAtlas)

        it "gives both atlases a drawable fallback mark" $
            forM_ [completeAtlas, narrowAtlas] $ \atlas → do
                let fb = faFallbackGlyph atlas
                    (w, h) = giSize fb
                w `shouldSatisfy` (> 0)
                h `shouldSatisfy` (> 0)
                giAdvance fb `shouldSatisfy` (> 0)
                -- Above the 0.7 threshold the font fragment shader
                -- applies, i.e. it actually paints something.
                let samples = samplesUnder atlas fb
                samples `shouldSatisfy` (not . null)
                maximum samples
                    `shouldSatisfy` (\v → fromIntegral v > fallbackOnEdge)

        it "keeps the fallback mark clear of every packed glyph" $
            forM_ [completeAtlas, narrowAtlas] $ \atlas →
                forM_ (Map.elems (faGlyphData atlas)) $ \gi →
                    uvOverlaps gi (faFallbackGlyph atlas) `shouldBe` False

        it "does not grow the atlas to make room for the mark" $ do
            -- The grid, recomputed from the published glyphs: reserving
            -- the mark's cell must not have needed a row of its own.
            -- Since #1098 the column count is chosen rather than fixed
            -- at 16, so the expectation is the cheapest power-of-two
            -- grid that holds one cell per glyph plus the mark's —
            -- written out here rather than taken from the planner.
            let glyphs = Map.elems (faGlyphData completeAtlas)
                cellW = round (maximum (map (fst . giSize) glyphs)) + 2 ∷ Int
                cellH = round (maximum (map (snd . giSize) glyphs)) + 2 ∷ Int
                cells = length bakedRange + 1
                grids = [ (w * h, max w h, cols, w, h)
                        | cols ← [1 .. length bakedRange]
                        , let rows = (cells + cols - 1) `div` cols
                              w = nextPowerOf2 (cols * cellW)
                              h = nextPowerOf2 (rows * cellH)
                        , w ≤ roomyLimit, h ≤ roomyLimit ]
                (_, _, _, expectW, expectH) = minimum grids
            faAtlasWidth completeAtlas `shouldBe` expectW
            faAtlasHeight completeAtlas `shouldBe` expectH

        it "publishes space in both atlases, with its own advance" $
            forM_ [completeAtlas, narrowAtlas] $ \atlas → do
                isMissingGlyph atlas ' ' `shouldBe` False
                fmap giAdvance (Map.lookup ' ' (faGlyphData atlas))
                    `shouldSatisfy` maybe False (> 0)

        it "measures narrow-font text containing a gap as non-zero" $ do
            let gap = T.pack (take 1 gothicGaps)
            calculateTextWidthScaled narrowAtlas 32 (T.unpack gap)
                `shouldSatisfy` (> 0)
            V.length (layoutTextUI narrowAtlas 32 0 0 gap white) `shouldBe` 1

    describe "the synthesized mark" $ do
        it "stays inside the cell the atlas already reserved" $ do
            let mark = fallbackMark 48 6 30 40
            fmWidth mark `shouldSatisfy` (≤ 30)
            fmHeight mark `shouldSatisfy` (≤ 40)
            length (fmPixels mark) `shouldBe` fmWidth mark * fmHeight mark

        it "is positive and drawable even in a degenerate cell" $ do
            let mark = fallbackMark 48 6 1 1
            fmWidth mark `shouldBe` 1
            fmHeight mark `shouldBe` 1
            fmAdvance mark `shouldSatisfy` (> 0)
            maximum (fmPixels mark)
                `shouldSatisfy` (\v → fromIntegral v > fallbackOnEdge)

        it "paints an outline, not a filled block" $ do
            let mark = fallbackMark 48 0 200 200
                w = fmWidth mark
                h = fmHeight mark
                px = VU.fromList (fmPixels mark)
                at x y = px VU.! (y * w + x)
            -- Middle of the top edge is inked; the centre is not.
            fromIntegral (at (w `div` 2) 0) `shouldSatisfy` (> fallbackOnEdge)
            fromIntegral (at (w `div` 2) (h `div` 2))
                `shouldSatisfy` (< fallbackOnEdge)

    describe "once-per-(font, codepoint) diagnostics" $ do
        let atlas = syntheticAtlas "AB "
            fontA = FontHandle 7
            fontB = FontHandle 8
            seeded = defaultFontCache
                { fcFonts = Map.fromList [(fontA, atlas), (fontB, atlas)] }

        it "reports a pair the first time and never again" $ do
            ref ← newIORef seeded
            first ← takeUnreportedMissingGlyphs ref fontA atlas "A?B"
            again ← takeUnreportedMissingGlyphs ref fontA atlas "A?B"
            third ← takeUnreportedMissingGlyphs ref fontA atlas "??"
            first `shouldBe` "?"
            again `shouldBe` []
            third `shouldBe` []

        it "shares the claim across entry paths" $ do
            -- Layout and measurement hand the same ref the same pair;
            -- only the first caller through gets it.
            ref ← newIORef seeded
            fromLayout ← takeUnreportedMissingGlyphs ref fontA atlas "?"
            fromMeasure ← takeUnreportedMissingGlyphs ref fontA atlas "x?y"
            fromLayout `shouldBe` "?"
            fromMeasure `shouldBe` "xy"

        it "treats a different font or codepoint as its own pair" $ do
            ref ← newIORef seeded
            _ ← takeUnreportedMissingGlyphs ref fontA atlas "?"
            otherFont ← takeUnreportedMissingGlyphs ref fontB atlas "?"
            otherChar ← takeUnreportedMissingGlyphs ref fontA atlas "!"
            otherFont `shouldBe` "?"
            otherChar `shouldBe` "!"

        it "hands a pair to exactly one of many concurrent callers" $ do
            ref ← newIORef seeded
            let racers = 24 ∷ Int
            done ← forM [1..racers] $ \_ → do
                slot ← newEmptyMVar
                _ ← forkIO $ takeUnreportedMissingGlyphs ref fontA atlas "?"
                                 ⌦ putMVar slot
                return slot
            claims ← concat ⊚ mapM takeMVar done
            claims `shouldBe` "?"
            cache ← readIORef ref
            Set.toList (fcMissingReported cache) `shouldBe` [(fontA, '?')]

        it "names both identities in the diagnostic" $ do
            -- U+00D7, the multiplication sign the inventory panels
            -- render and the baked ASCII range does not contain.
            let msg = T.unpack (missingGlyphMessage (FontHandle 3) '\215')
            msg `shouldSatisfy` ("FontHandle 3" `isInfixOf`)
            msg `shouldSatisfy` ("U+00D7" `isInfixOf`)

        it "leaves fully covered text off the shared claim entirely" $ do
            ref ← newIORef seeded
            claimed ← takeUnreportedMissingGlyphs ref fontA atlas "AB AB"
            claimed `shouldBe` []
            cache ← readIORef ref
            Set.null (fcMissingReported cache) `shouldBe` True
