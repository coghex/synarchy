-- | Widening the shipped SDF atlases past printable ASCII (#1098).
--
--   Everything here runs on the CPU: atlas generation is stb-in-IO and
--   the packing planner is pure, so the three tracked fonts are
--   generated for real and no GPU is involved. The device limit the
--   planner is bounded by is a parameter for exactly that reason — the
--   production path passes the real @maxImageDimension2D@, and these
--   tests pass whatever the case needs.
--
--   The concrete codepoints below are written out rather than
--   discovered by the same cmap query under test. A test that asks the
--   font which characters it has and then asserts it has them stays
--   green through any coverage regression.
module Test.Headless.Graphics.FontRepertoire (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (isInfixOf, sort)
import Engine.Asset.Handle (FontHandle(..))
import Engine.Asset.Types (GlyphInfo(..))
import Engine.Core.Log
  ( LogBackend(..), LogConfig(..), LogEntry(..), LogLevel(..)
  , LogCategory(..), LoggerState, defaultLogConfig, initLogger )
import Engine.Graphics.Font.Atlas
  ( AtlasPlan(..), atlasPayloadBytes, nextPowerOf2, planAtlasGrid )
import Engine.Graphics.Font.Data (FontAtlas(..))
import Engine.Graphics.Font.Fallback (isMissingGlyph, resolveGlyph)
import Engine.Graphics.Font.Repertoire
  ( FontKey(..), asciiWithCurlyQuotes, bitmapFontKey, canonicalRepertoire
  , extendedLatin, printableAscii, repertoireChars, repertoireForFont
  , repertoireSize, sdfFontKey, sdfSizeSentinel )
import Engine.Graphics.Font.SDF
  ( CoverageReport(..), SDFAtlasError(..), coverageReport
  , coverageReportMessage, generateSDFFontAtlas, sdfAtlasErrorMessage
  , sdfBaseSize, sdfPadding )
import Engine.Graphics.Font.STB
  ( freeSTBFont, hasSTBCodepoint, loadSTBFont, renderSTBGlyphSDF
  , scaleForPixelHeight )
import Engine.Graphics.Font.Util (calculateTextWidthScaled)

-- * The tracked fonts, and what they are known to supply

arcadePath, gothicPath, shellPath ∷ FilePath
arcadePath = "assets/fonts/arcade.ttf"
gothicPath = "assets/fonts/gothic.ttf"
shellPath  = "assets/fonts/shell.ttf"

-- | Requested non-ASCII characters @arcade.ttf@ genuinely draws. Pinned
--   so a font swap that drops accented Latin fails here rather than
--   quietly reducing this file to a tautology.
arcadeSuppliedNonAscii ∷ [Char]
arcadeSuppliedNonAscii =
  [ '\x00E9'  -- é  LATIN SMALL LETTER E WITH ACUTE
  , '\x00C6'  -- Æ  LATIN CAPITAL LETTER AE
  , '\x0141'  -- Ł  LATIN CAPITAL LETTER L WITH STROKE
  , '\x017E'  -- ž  LATIN SMALL LETTER Z WITH CARON
  , '\x2019'  -- ’  RIGHT SINGLE QUOTATION MARK
  , '\x2026'  -- …  HORIZONTAL ELLIPSIS
  ]

-- | The three characters 'extendedLatin' asks @arcade.ttf@ for that its
--   cmap does not map. These are the requested-but-absent case: they
--   must reach the #1097 fallback, not a @.notdef@ entry.
arcadeMissing ∷ [Char]
arcadeMissing = ['\x2010', '\x2032', '\x2033']

-- | The fifteen printable ASCII characters @gothic.ttf@ lacks, and the
--   four curly quotes it does supply.
gothicGaps, gothicCurlyQuotes ∷ [Char]
gothicGaps = "/:;<=>[\\]^`{|}~"
gothicCurlyQuotes = ['\x2018', '\x2019', '\x201C', '\x201D']

-- | Requested non-ASCII characters @shell.ttf@ supplies. The last three
--   are the ones @arcade.ttf@ does NOT have, so the two fonts cannot
--   both be satisfied by the same wrong answer.
shellSuppliedNonAscii ∷ [Char]
shellSuppliedNonAscii =
  [ '\x00E9'  -- é  LATIN SMALL LETTER E WITH ACUTE
  , '\x0141'  -- Ł  LATIN CAPITAL LETTER L WITH STROKE
  , '\x2026'  -- …  HORIZONTAL ELLIPSIS
  , '\x2010'  -- ‐  HYPHEN
  , '\x2032'  -- ′  PRIME
  , '\x2033'  -- ″  DOUBLE PRIME
  ]

-- | Requested Latin Extended-A characters @shell.ttf@ does not map —
--   a sample of its 39 gaps, including the first and the last.
shellMissingSample ∷ [Char]
shellMissingSample = ['\x010A', '\x0128', '\x014B', '\x017F']

-- | A generous device limit, well above anything the tracked fonts
--   need. Real devices report at least 4096 for @maxImageDimension2D@.
roomyLimit ∷ Int
roomyLimit = 16384

-- * Helpers

quietLogger ∷ IO LoggerState
quietLogger = initLogger defaultLogConfig { lcEnableByDefault = False }

-- | A logger whose backend records every emitted entry. The default
--   minimum level is Info, which is where the coverage report lands, so
--   nothing else this module triggers reaches it.
capturingLogger ∷ IO (LoggerState, IORef [LogEntry])
capturingLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)

generateOrFail ∷ FilePath → [Char] → IO FontAtlas
generateOrFail path chars = do
    logger ← quietLogger
    result ← generateSDFFontAtlas logger path (canonicalRepertoire chars) roomyLimit
    case result of
        Left err → fail $ "atlas generation failed: "
                            ⧺ T.unpack (sdfAtlasErrorMessage err)
        Right atlas → return atlas

generateExpectingError ∷ FilePath → [Char] → Int → IO SDFAtlasError
generateExpectingError path chars limit = do
    logger ← quietLogger
    result ← generateSDFFontAtlas logger path (canonicalRepertoire chars) limit
    case result of
        Left err → return err
        Right _  → fail "expected atlas generation to fail, but it succeeded"

-- | The atlas samples under a glyph's UV rect — its actual raster data,
--   independent of where in the sheet it landed.
samplesUnder ∷ FontAtlas → GlyphInfo → [Word8]
samplesUnder atlas gi =
    [ bitmap VU.! (y * aw + x)
    | y ← [py v0 .. py v1 - 1], x ← [px u0 .. px u1 - 1]
    , y ≥ 0, y < ah, x ≥ 0, x < aw, y * aw + x < VU.length bitmap ]
  where
    bitmap = VU.fromList (faAtlasBitmap atlas)
    aw = faAtlasWidth atlas
    ah = faAtlasHeight atlas
    (u0, v0, u1, v1) = giUVRect gi
    px u = round (u * fromIntegral aw) ∷ Int
    py v = round (v * fromIntegral ah) ∷ Int

-- | Requirement 5's rule, written out independently of the planner so
--   the planner is compared against the specification rather than
--   against itself: over every column count from 1 to @glyphCount@,
--   cheapest R8 payload wins, then smaller longest side, then fewer
--   columns. Returns @(columns, atlasWidth, atlasHeight)@.
oracleColumnsAndSize ∷ Int → Int → Int → Int → Maybe (Int, Int, Int)
oracleColumnsAndSize glyphCount cellWidth cellHeight limit
    | glyphCount ≤ 0  = Nothing
    | null candidates = Nothing
    | otherwise       = Just (project (foldr1 pick candidates))
  where
    candidates =
        [ (w * h, max w h, columns, w, h)
        | columns ← [1 .. glyphCount]
        , let rows = ceilingDiv (glyphCount + 1) columns
              w = nextPowerOf2 (columns * cellWidth)
              h = nextPowerOf2 (rows * cellHeight)
        , w ≤ limit, h ≤ limit ]
    ceilingDiv a b = (a + b - 1) `div` b
    pick a@(pa, la, ca, _, _) b@(pb, lb, cb, _, _)
        | (pa, la, ca) ≤ (pb, lb, cb) = a
        | otherwise                   = b
    project (_, _, columns, w, h) = (columns, w, h)

spec ∷ Spec
spec = do
    -- Three real atlases at the shipped policy, generated once each.
    arcadeWide ← runIO $ generateOrFail arcadePath
                            (repertoireChars (repertoireForFont arcadePath))
    arcadeAscii ← runIO $ generateOrFail arcadePath
                            (repertoireChars printableAscii)
    gothicWide ← runIO $ generateOrFail gothicPath
                            (repertoireChars (repertoireForFont gothicPath))
    shellWide ← runIO $ generateOrFail shellPath
                            (repertoireChars (repertoireForFont shellPath))

    describe "canonical repertoires" $ do
        it "sorts and deduplicates whatever order it is handed" $ do
            canonicalRepertoire "cbaa" `shouldBe` canonicalRepertoire "abc"
            repertoireChars (canonicalRepertoire "cbaa") `shouldBe` "abc"

        it "spells out the shipped policies exactly" $ do
            repertoireChars printableAscii `shouldBe` [' '..'~']
            repertoireSize printableAscii `shouldBe` 95
            -- 95 ASCII + 94 Latin-1 (U+00A1-U+00FF less soft hyphen)
            -- + 128 Latin Extended-A + 13 punctuation marks.
            repertoireSize extendedLatin `shouldBe` 330
            repertoireSize asciiWithCurlyQuotes `shouldBe` 99
            '\x00AD' `elem` repertoireChars extendedLatin `shouldBe` False
            forM_ ['\x00A1', '\x00FF', '\x0100', '\x017F', '\x2014', '\x2033'] $
                \c → (c `elem` repertoireChars extendedLatin) `shouldBe` True

        it "assigns each tracked font its policy and everything else ASCII" $ do
            repertoireForFont arcadePath `shouldBe` extendedLatin
            repertoireForFont shellPath `shouldBe` extendedLatin
            repertoireForFont gothicPath `shouldBe` asciiWithCurlyQuotes
            repertoireForFont "assets/fonts/unknown.ttf" `shouldBe` printableAscii

        it "resolves the policy from a prefixed or absolute path too" $ do
            repertoireForFont "/opt/game/assets/fonts/arcade.ttf"
                `shouldBe` extendedLatin
            repertoireForFont "../assets/fonts/gothic.ttf"
                `shouldBe` asciiWithCurlyQuotes

        it "leaves an unrelated font sharing a basename on ASCII" $ do
            -- The policy belongs to the tracked asset, not to the name:
            -- a font that merely happens to be called gothic.ttf must
            -- not inherit gothic's curly-quote repertoire.
            repertoireForFont "mods/gothic.ttf" `shouldBe` printableAscii
            repertoireForFont "arcade.ttf" `shouldBe` printableAscii
            repertoireForFont "assets/fonts/mods/shell.ttf"
                `shouldBe` printableAscii
            repertoireForFont "/opt/other/fonts/arcade.ttf"
                `shouldBe` printableAscii
            -- …and the keys follow, so no aliasing in the cache either.
            sdfFontKey "mods/gothic.ttf" `shouldNotBe` sdfFontKey gothicPath

    describe "cmap presence is not rasterization" $ do
        it "reports SPACE as supplied even though it rasterizes to nothing" $ do
            logger ← quietLogger
            Just font ← loadSTBFont logger arcadePath
            scale ← scaleForPixelHeight font (fromIntegral sdfBaseSize)
            supplied ← hasSTBCodepoint font ' '
            raster ← renderSTBGlyphSDF font ' ' scale sdfPadding
            freeSTBFont font
            supplied `shouldBe` True
            -- No outline, so stb hands back nothing to pack…
            fmap (\(_, _, _, _, px) → null px) raster
                `shouldSatisfy` maybe True id
            -- …and SPACE is still published, with its own advance.
            Map.member ' ' (faGlyphData arcadeWide) `shouldBe` True
            fmap giAdvance (Map.lookup ' ' (faGlyphData arcadeWide))
                `shouldSatisfy` maybe False (> 0)

        it "reports an unmapped character as absent though it rasterizes fine" $ do
            logger ← quietLogger
            Just font ← loadSTBFont logger gothicPath
            scale ← scaleForPixelHeight font (fromIntegral sdfBaseSize)
            supplied ← hasSTBCodepoint font '/'
            raster ← renderSTBGlyphSDF font '/' scale sdfPadding
            freeSTBFont font
            supplied `shouldBe` False
            -- stb resolved it to glyph 0 and rasterized .notdef, which
            -- is exactly why presence cannot be inferred from pixels.
            fmap (\(_, _, _, _, px) → not (null px)) raster
                `shouldBe` Just True

    describe "requested characters intersected with the cmap" $ do
        it "publishes the non-ASCII characters arcade genuinely supplies" $
            forM_ arcadeSuppliedNonAscii $ \c → do
                Map.member c (faGlyphData arcadeWide) `shouldBe` True
                isMissingGlyph arcadeWide c `shouldBe` False
                calculateTextWidthScaled arcadeWide 48 [c] `shouldSatisfy` (> 0)

        it "omits a requested character the font lacks, leaving it to #1097" $
            forM_ arcadeMissing $ \c → do
                Map.member c (faGlyphData arcadeWide) `shouldBe` False
                isMissingGlyph arcadeWide c `shouldBe` True
                resolveGlyph arcadeWide c
                    `shouldBe` Just (faFallbackGlyph arcadeWide)
                -- It still measures, because the mark has an advance.
                calculateTextWidthScaled arcadeWide 48 [c] `shouldSatisfy` (> 0)

        it "gives gothic its four curly quotes and none of its ASCII gaps" $ do
            forM_ gothicCurlyQuotes $ \c →
                Map.member c (faGlyphData gothicWide) `shouldBe` True
            forM_ gothicGaps $ \c → do
                Map.member c (faGlyphData gothicWide) `shouldBe` False
                resolveGlyph gothicWide c
                    `shouldBe` Just (faFallbackGlyph gothicWide)

        it "does not bake .notdef cells for gothic's unsupported ASCII" $ do
            -- 99 requested, 84 supplied. If the fifteen gaps were still
            -- packed as .notdef the grid would have to hold 100 cells,
            -- so comparing against the 85-cell grid is what proves they
            -- are gone rather than merely unpublished.
            Map.size (faGlyphData gothicWide) `shouldBe` 84
            let glyphs = Map.elems (faGlyphData gothicWide)
                cellW = round (maximum (map (fst . giSize) glyphs)) + 2 ∷ Int
                cellH = round (maximum (map (snd . giSize) glyphs)) + 2 ∷ Int
            case ( oracleColumnsAndSize 84 cellW cellH roomyLimit
                 , oracleColumnsAndSize 99 cellW cellH roomyLimit ) of
                (Just (_, w84, h84), Just (_, w99, h99)) → do
                    (faAtlasWidth gothicWide, faAtlasHeight gothicWide)
                        `shouldBe` (w84, h84)
                    -- The two grids genuinely differ, so the check above
                    -- is not satisfiable by the .notdef layout.
                    (w84 * h84) `shouldSatisfy` (< w99 * h99)
                _ → expectationFailure "no feasible grid for gothic"

        it "intersects shell's own cmap, which differs from arcade's" $ do
            -- The third shipped font, generated for real. Its Latin
            -- Extended-A coverage is patchier than arcade's while it
            -- supplies three punctuation marks arcade lacks, so the two
            -- atlases cannot both be right by accident.
            Map.size (faGlyphData shellWide) `shouldBe` 291
            forM_ shellSuppliedNonAscii $ \c → do
                Map.member c (faGlyphData shellWide) `shouldBe` True
                isMissingGlyph shellWide c `shouldBe` False
                calculateTextWidthScaled shellWide 48 [c] `shouldSatisfy` (> 0)
            forM_ shellMissingSample $ \c → do
                Map.member c (faGlyphData shellWide) `shouldBe` False
                resolveGlyph shellWide c
                    `shouldBe` Just (faFallbackGlyph shellWide)
            -- The three arcade lacks and shell has, and vice versa.
            forM_ arcadeMissing $ \c →
                Map.member c (faGlyphData shellWide) `shouldBe` True
            Map.member '\x0141' (faGlyphData arcadeWide) `shouldBe` True
            Map.member '\x010A' (faGlyphData arcadeWide) `shouldBe` True
            -- And it lands on the grid the rule selects for 291 glyphs.
            let glyphs = Map.elems (faGlyphData shellWide)
                cellW = round (maximum (map (fst . giSize) glyphs)) + 2 ∷ Int
                cellH = round (maximum (map (snd . giSize) glyphs)) + 2 ∷ Int
            case oracleColumnsAndSize 291 cellW cellH roomyLimit of
                Just (_, w, h) →
                    (faAtlasWidth shellWide, faAtlasHeight shellWide)
                        `shouldBe` (w, h)
                Nothing → expectationFailure "no feasible grid for shell"

        it "reports shell's coverage as the PR evidence records it" $ do
            (logger, entriesRef) ← capturingLogger
            _ ← generateSDFFontAtlas logger shellPath extendedLatin roomyLimit
            entries ← readIORef entriesRef
            case entries of
                [entry] → do
                    let msg = T.unpack (leMessage entry)
                    msg `shouldSatisfy` (shellPath `isInfixOf`)
                    msg `shouldSatisfy` ("requested=330" `isInfixOf`)
                    msg `shouldSatisfy` ("supplied=291" `isInfixOf`)
                    msg `shouldSatisfy` ("missing=39" `isInfixOf`)
                    msg `shouldSatisfy` ("[U+010A U+010B" `isInfixOf`)
                    msg `shouldSatisfy` ("U+017F]" `isInfixOf`)
                other → expectationFailure $
                    "expected one coverage entry, got " ⧺ show (length other)

        it "publishes a mapped glyph that draws nothing, with its advance" $ do
            -- U+00A0 NO-BREAK SPACE: arcade.ttf maps it and gives it a
            -- real advance, but it has no outline, so stb rasterizes
            -- nothing. Coverage is the cmap's answer alone — gating on
            -- the raster would swap that advance for the fallback
            -- mark's and paint a box where the font asked for a gap.
            --
            -- It is reachable only through an explicit repertoire: the
            -- shipped Latin-1 request starts at U+00A1.
            ('\x00A0' `elem` repertoireChars extendedLatin) `shouldBe` False
            atlas ← generateOrFail arcadePath ['\x00A0', 'A']
            Map.member '\x00A0' (faGlyphData atlas) `shouldBe` True
            isMissingGlyph atlas '\x00A0' `shouldBe` False
            resolveGlyph atlas '\x00A0'
                `shouldNotBe` Just (faFallbackGlyph atlas)
            fmap giAdvance (Map.lookup '\x00A0' (faGlyphData atlas))
                `shouldSatisfy` maybe False (> 0)
            -- It draws nothing, which is the whole point.
            fmap giSize (Map.lookup '\x00A0' (faGlyphData atlas))
                `shouldBe` Just (0, 0)

        it "produces the same canonical intersection every time" $ do
            again ← generateOrFail arcadePath
                        (repertoireChars (repertoireForFont arcadePath))
            let keys = Map.keys (faGlyphData arcadeWide)
            Map.keys (faGlyphData again) `shouldBe` keys
            -- Map.keys is ascending by construction; assert the
            -- published set is what the canonical request permits.
            keys `shouldBe` sort keys
            filter (`notElem` repertoireChars extendedLatin) keys `shouldBe` []

        it "is insensitive to the order and duplication of the request" $ do
            let wide = repertoireChars extendedLatin
            shuffled ← generateOrFail arcadePath (reverse wide ⧺ wide)
            Map.keys (faGlyphData shuffled)
                `shouldBe` Map.keys (faGlyphData arcadeWide)
            ( faAtlasWidth shuffled, faAtlasHeight shuffled )
                `shouldBe` ( faAtlasWidth arcadeWide, faAtlasHeight arcadeWide )

    describe "supplied ASCII glyphs survive the widening" $ do
        let asciiKeys = Map.keys (faGlyphData arcadeAscii)

        it "keeps every ASCII glyph the narrow atlas published" $
            filter (`notElem` Map.keys (faGlyphData arcadeWide)) asciiKeys
                `shouldBe` []

        it "keeps their sizes, bearings and advances identical" $
            forM_ asciiKeys $ \c →
                case ( Map.lookup c (faGlyphData arcadeAscii)
                     , Map.lookup c (faGlyphData arcadeWide) ) of
                    (Just narrow, Just wide) → do
                        giSize wide `shouldBe` giSize narrow
                        giBearing wide `shouldBe` giBearing narrow
                        giAdvance wide `shouldBe` giAdvance narrow
                    _ → expectationFailure $ "missing glyph for " ⧺ show c

        it "keeps their SDF raster data identical" $
            -- UVs necessarily move — the sheet is a different size — so
            -- the comparison is of the samples under each rect.
            forM_ asciiKeys $ \c →
                case ( Map.lookup c (faGlyphData arcadeAscii)
                     , Map.lookup c (faGlyphData arcadeWide) ) of
                    (Just narrow, Just wide) →
                        samplesUnder arcadeWide wide
                            `shouldBe` samplesUnder arcadeAscii narrow
                    _ → expectationFailure $ "missing glyph for " ⧺ show c

        it "keeps the baseline, line height and measured layout" $ do
            faBaseline arcadeWide `shouldBe` faBaseline arcadeAscii
            faLineHeight arcadeWide `shouldBe` faLineHeight arcadeAscii
            faFontSize arcadeWide `shouldBe` faFontSize arcadeAscii
            let sample = "The quick brown fox, 0123456789!"
            forM_ [12, 32.5, 96] $ \size →
                calculateTextWidthScaled arcadeWide size sample
                    `shouldBe` calculateTextWidthScaled arcadeAscii size sample

    describe "packing planner" $ do
        it "matches the specified rule over a range of shapes" $
            forM_ [ (n, cw, ch)
                  | n ← [1, 2, 7, 15, 84, 95, 291, 327]
                  , (cw, ch) ← [(32, 32), (64, 32), (56, 62), (51, 56)] ] $
                \(n, cw, ch) →
                    case (planAtlasGrid n cw ch roomyLimit
                         , oracleColumnsAndSize n cw ch roomyLimit) of
                        (Just plan, Just (columns, w, h)) → do
                            apColumns plan `shouldBe` columns
                            apAtlasWidth plan `shouldBe` w
                            apAtlasHeight plan `shouldBe` h
                            apRows plan `shouldSatisfy`
                                (\r → r * columns ≥ n + 1)
                        (Nothing, Nothing) → return ()
                        _ → expectationFailure $
                                "planner and rule disagree for " ⧺ show (n, cw, ch)

        it "picks the smallest R8 payload" $ do
            -- 15 glyphs in 64x64 cells: four column counts pay 65536
            -- and the rest pay more.
            case planAtlasGrid 15 64 64 roomyLimit of
                Just plan → do
                    atlasPayloadBytes plan `shouldBe` 65536
                    apCellWidth plan `shouldBe` 64
                    apCellHeight plan `shouldBe` 64
                Nothing → expectationFailure "expected a feasible plan"

        it "breaks a payload tie on the smaller longest side" $
            -- cols 1, 2, 4 and 8 all pay 65536; only cols=4 is square.
            case planAtlasGrid 15 64 64 roomyLimit of
                Just plan → do
                    apColumns plan `shouldBe` 4
                    (apAtlasWidth plan, apAtlasHeight plan) `shouldBe` (256, 256)
                Nothing → expectationFailure "expected a feasible plan"

        it "breaks a remaining tie on the smaller column count" $
            -- 7 glyphs in 32x32 cells: cols=2 gives 64x128 and cols=4
            -- gives 128x64 — same payload, same longest side.
            case planAtlasGrid 7 32 32 roomyLimit of
                Just plan → do
                    atlasPayloadBytes plan `shouldBe` 8192
                    max (apAtlasWidth plan) (apAtlasHeight plan) `shouldBe` 128
                    apColumns plan `shouldBe` 2
                    (apAtlasWidth plan, apAtlasHeight plan) `shouldBe` (64, 128)
                Nothing → expectationFailure "expected a feasible plan"

        it "rejects every candidate over the device limit" $ do
            planAtlasGrid 327 62 62 256 `shouldBe` Nothing
            planAtlasGrid 15 64 64 128 `shouldBe` Nothing
            -- One power of two lower than the plan it would otherwise
            -- pick is enough to make it infeasible.
            planAtlasGrid 15 64 64 255 `shouldBe` Nothing
            planAtlasGrid 15 64 64 256 `shouldSatisfy` isJust

        it "has nothing to plan for an empty glyph set" $ do
            planAtlasGrid 0 64 64 roomyLimit `shouldBe` Nothing
            planAtlasGrid (-1) 64 64 roomyLimit `shouldBe` Nothing

    describe "descriptive failures" $ do
        it "rejects an empty requested repertoire" $ do
            err ← generateExpectingError arcadePath [] roomyLimit
            err `shouldBe` SDFEmptyRepertoire arcadePath
            let msg = T.unpack (sdfAtlasErrorMessage err)
            msg `shouldSatisfy` (arcadePath `isInfixOf`)
            msg `shouldSatisfy` ("empty" `isInfixOf`)

        it "rejects a repertoire the font's cmap supplies none of" $ do
            -- CJK, which none of the tracked Latin fonts carries.
            err ← generateExpectingError gothicPath ['\x4E00', '\x4E8C'] roomyLimit
            err `shouldBe` SDFNoSuppliedGlyphs gothicPath 2
            let msg = T.unpack (sdfAtlasErrorMessage err)
            msg `shouldSatisfy` (gothicPath `isInfixOf`)
            msg `shouldSatisfy` ("cmap" `isInfixOf`)

        it "rejects a request no grid can fit, before allocating anything" $ do
            err ← generateExpectingError arcadePath
                      (repertoireChars extendedLatin) 128
            case err of
                SDFNoFeasibleLayout path supplied cellW cellH limit → do
                    path `shouldBe` arcadePath
                    supplied `shouldSatisfy` (> 0)
                    cellW `shouldSatisfy` (> 0)
                    cellH `shouldSatisfy` (> 0)
                    limit `shouldBe` 128
                    -- Even the tallest single-column grid overruns it.
                    oracleColumnsAndSize supplied cellW cellH limit
                        `shouldBe` Nothing
                other → expectationFailure $
                    "expected a no-feasible-layout error, got " ⧺ show other
            let msg = T.unpack (sdfAtlasErrorMessage err)
            msg `shouldSatisfy` (arcadePath `isInfixOf`)
            msg `shouldSatisfy` ("maxImageDimension2D=128" `isInfixOf`)

        it "names the font when the file cannot be loaded at all" $ do
            err ← generateExpectingError "assets/fonts/nonexistent.ttf"
                      (repertoireChars printableAscii) roomyLimit
            err `shouldBe` SDFFontLoadFailed "assets/fonts/nonexistent.ttf"
            T.unpack (sdfAtlasErrorMessage err)
                `shouldSatisfy` ("nonexistent.ttf" `isInfixOf`)

    describe "coverage report" $ do
        it "lists missing codepoints in ascending U+XXXX form" $ do
            let report = coverageReport gothicPath asciiWithCurlyQuotes
                            (filter (`notElem` gothicGaps)
                                    (repertoireChars asciiWithCurlyQuotes))
            crFontPath report `shouldBe` gothicPath
            crRequested report `shouldBe` 99
            crSupplied report `shouldBe` 84
            crMissing report `shouldBe` sort gothicGaps
            let msg = T.unpack (coverageReportMessage report)
            msg `shouldSatisfy` (gothicPath `isInfixOf`)
            msg `shouldSatisfy` ("requested=99" `isInfixOf`)
            msg `shouldSatisfy` ("supplied=84" `isInfixOf`)
            msg `shouldSatisfy` ("missing=15" `isInfixOf`)
            msg `shouldSatisfy` ("U+002F U+003A U+003B" `isInfixOf`)
            msg `shouldSatisfy` ("U+007E]" `isInfixOf`)

        it "emits exactly one aggregate entry per generated atlas" $ do
            (logger, entriesRef) ← capturingLogger
            _ ← generateSDFFontAtlas logger arcadePath extendedLatin roomyLimit
            entries ← readIORef entriesRef
            case entries of
                [entry] → do
                    leLevel entry `shouldBe` LevelInfo
                    leCategory entry `shouldBe` CatFont
                    let msg = T.unpack (leMessage entry)
                    msg `shouldSatisfy` (arcadePath `isInfixOf`)
                    msg `shouldSatisfy` ("requested=330" `isInfixOf`)
                    msg `shouldSatisfy` ("supplied=327" `isInfixOf`)
                    msg `shouldSatisfy` ("missing=3" `isInfixOf`)
                    msg `shouldSatisfy`
                        ("[U+2010 U+2032 U+2033]" `isInfixOf`)
                other → expectationFailure $
                    "expected one coverage entry, got " ⧺ show (length other)

        it "reports once per generation, never per rasterized glyph" $ do
            -- Two generations, two reports — and 327 glyphs' worth of
            -- silence in between.
            (logger, entriesRef) ← capturingLogger
            _ ← generateSDFFontAtlas logger arcadePath extendedLatin roomyLimit
            _ ← generateSDFFontAtlas logger gothicPath asciiWithCurlyQuotes
                                     roomyLimit
            entries ← readIORef entriesRef
            length entries `shouldBe` 2

        it "still reports when the request cannot be satisfied" $ do
            (logger, entriesRef) ← capturingLogger
            _ ← generateSDFFontAtlas logger gothicPath
                    (canonicalRepertoire ['\x4E00']) roomyLimit
            entries ← readIORef entriesRef
            map (T.unpack . leMessage) entries `shouldSatisfy`
                any ("supplied=0" `isInfixOf`)

    describe "atlas cache identity" $ do
        it "distinguishes two repertoires for one font path" $ do
            let wideKey = sdfFontKey arcadePath
                asciiKey = FontKey arcadePath sdfSizeSentinel printableAscii
            fkRepertoire wideKey `shouldBe` extendedLatin
            wideKey `shouldNotBe` asciiKey
            Map.size (Map.fromList [ (wideKey, FontHandle 1)
                                   , (asciiKey, FontHandle 2) ])
                `shouldBe` 2

        it "is stable for the same path and policy" $ do
            sdfFontKey arcadePath `shouldBe` sdfFontKey arcadePath
            sdfFontKey arcadePath `shouldNotBe` sdfFontKey gothicPath
            -- Same policy, different font: still separate atlases.
            sdfFontKey arcadePath `shouldNotBe` sdfFontKey shellPath

        it "keeps the size-specific bitmap keys separate from SDF ones" $ do
            bitmapFontKey arcadePath 24 `shouldNotBe` bitmapFontKey arcadePath 96
            bitmapFontKey arcadePath 24 `shouldNotBe` sdfFontKey arcadePath
            fkSize (sdfFontKey arcadePath) `shouldBe` sdfSizeSentinel
