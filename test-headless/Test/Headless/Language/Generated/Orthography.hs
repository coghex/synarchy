-- | Version 5's per-language extended orthography (#1100): the
--   extended-letter inventory and diacritic families, their
--   phonological participation, capitalization, the widened output
--   contract and its code-point length, report-regex parity, and
--   the generated-name fonts' repertoire, rasterization and layout
--   coverage.
module Test.Headless.Language.Generated.Orthography
    ( spec
    ) where

import UPrelude
import Test.Hspec
import Data.Char (toUpper)
import Data.List (nub, sort)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Engine.Asset.Types (GlyphInfo(..))
import Engine.Core.Log
    (LogConfig(..), LoggerState, defaultLogConfig, initLogger)
import Engine.Graphics.Font.Data (FontAtlas(..))
import Engine.Graphics.Font.Fallback (isMissingGlyph)
import Engine.Graphics.Font.Repertoire
    (generatedNameFonts, repertoireForFont)
import Engine.Graphics.Font.SDF (generateSDFFontAtlas, sdfAtlasErrorMessage)
import Engine.Graphics.Font.Util (calculateTextWidthScaled)
import Language.Generated.Types
import Language.Generated.Orthography
import Language.Generated.Onset
import Language.Generated.Boundary
import Language.Generated.Bound
import Language.Generated.Profile
import Language.Generated.Render
import Language.Generated.Report (canonicalExpressions)
import Test.Headless.Language.Generated.Support

-- | The title font. Named here rather than reached for through
--   'generatedNameFonts' because the point of the font-coverage group
--   below is that it is NOT in that list.
gothicFontPath ∷ FilePath
gothicFontPath = "assets/fonts/gothic.ttf"

-- | A real SDF atlas for one font at its shipped repertoire, generated
--   from the checked-in @.ttf@ through the production path.
--
--   Everything involved is CPU-side (stb rasterization plus a pure
--   packing planner), so no GPU is required; the device limit the
--   planner is bounded by is a parameter for exactly that reason, and
--   16384 is above anything the shipped fonts need.
nameFontAtlas ∷ FilePath → IO FontAtlas
nameFontAtlas path = do
    logger ← quietLogger
    result ← generateSDFFontAtlas logger path (repertoireForFont path) 16384
    case result of
        Left err → fail $ "atlas generation failed for " ⧺ path ⧺ ": "
                        ⧺ T.unpack (sdfAtlasErrorMessage err)
        Right a  → pure a
  where
    quietLogger ∷ IO LoggerState
    quietLogger = initLogger defaultLogConfig { lcEnableByDefault = False }

-- | Whether an atlas can actually DRAW a character.
--
--   Three conditions, and the third is the one that matters: #1098's
--   atlas publishes a mapped-but-outline-less character (U+00A0) with a
--   real advance and a zero-sized glyph, so glyph-map membership alone
--   would call an invisible cell "covered". A letter must have real
--   extent.
drawsGlyph ∷ FontAtlas → Char → Bool
drawsGlyph atlas c = case M.lookup c (faGlyphData atlas) of
    Nothing → False
    Just gi → not (isMissingGlyph atlas c)
              ∧ fst (giSize gi) > 0 ∧ snd (giSize gi) > 0

-- | Every extended character in @w@ that the profile which rendered it
--   does not hold — #1100 requirement 1's "an accent is part of the
--   inventory, not applied to output". Compared case-folded, because
--   rendering capitalizes the initial and inventories are lowercase.
foreignExtended ∷ Profile → Text → [Char]
foreignExtended prof w =
    [ c | c ← T.unpack w, isExtendedLetter c, lowerOf c `notElem` own ]
  where
    own = profVowels prof <> profConsonants prof
    lowerOf c = case [ lo | (_, _, _, lo, up) ← extendedLetterTable, up ≡ c ] of
        (lo : _) → lo
        []       → c

spec ∷ Ctx → Spec
spec Ctx{..} = do
    -- #1100: per-language extended orthography. Everything below is
    -- stated over the SAME 256-seed sample tools/language_report.py
    -- gates, so a property that holds here holds for the population the
    -- report measures.
    describe "extended letters are inventory, not decoration (#1100 requirement 1)" $ do
        it "extends the same seed's version-4 inventory rather than \
           \replacing it" $ do
            -- Requirement 1's "inventory, not decoration", stated as a
            -- relation to the UNMARKED language of the same seed rather
            -- than as a comparison of one construction to another:
            -- version 5 keeps version 4's letters in order, and
            -- everything it adds is exactly what
            -- 'profileExtendedChars' reports. A decorating
            -- implementation would substitute rather than extend, and
            -- shows up here as a broken prefix.
            let offenders =
                    [ (s, profVowels four, profVowels five
                      , profConsonants four, profConsonants five
                      , profileExtendedChars five)
                    | s ← [0 .. 63 ∷ Word64]
                    , let four = buildProfileV4 (LangSeed s)
                          five = buildProfileV5 (LangSeed s)
                          keptV = take (length (profVowels four))
                                       (profVowels five)
                          keptC = take (length (profConsonants four))
                                       (profConsonants five)
                          added = drop (length (profVowels four))
                                       (profVowels five)
                                ⧺ drop (length (profConsonants four))
                                       (profConsonants five)
                    , keptV ≢ profVowels four
                      ∨ keptC ≢ profConsonants four
                      ∨ sort (profileExtendedChars five) ≢ sort added ]
            offenders `shouldBe` []
            -- Two fixed languages of that sample, pinned: seed 7 drew
            -- three marked consonants, seed 63 a single marked vowel.
            -- (Seeds 0, 1 and 42 are the golden at the end of this
            -- module.)
            (profileExtendedChars (buildProfileV5 (LangSeed 7))
             , profileDiacritic (buildProfileV5 (LangSeed 7)))
                `shouldBe` ("\x011D\x0125\x0135", Just DiaCircumflex)
            (profileExtendedChars (buildProfileV5 (LangSeed 63))
             , profileDiacritic (buildProfileV5 (LangSeed 63)))
                `shouldBe` ("\x016F", Just DiaRing)

        it "only marks a base sound the language already has" $ do
            -- The rule that ties an accent to the language rather than
            -- to the alphabet: 'á' means "this language distinguishes
            -- its own /a/", so a language without 'a' cannot have it.
            let orphans =
                    [ (profSeed p, marked, base)
                    | p ← v5Profiles
                    , (_, slot, base, marked, _) ← extendedLetterTable
                    , marked `elem` profileExtendedChars p
                    , let inventory = case slot of
                            VowelSlot     → profVowels p
                            ConsonantSlot → profConsonants p
                    , base `notElem` inventory ]
            v5Marked `shouldSatisfy` not ∘ null
            orphans `shouldBe` []

        it "puts a marked letter in the inventory its base belongs to" $ do
            let misplaced =
                    [ (profSeed p, marked)
                    | p ← v5Profiles
                    , (_, slot, _, marked, _) ← extendedLetterTable
                    , let wrong = case slot of
                            VowelSlot     → profConsonants p
                            ConsonantSlot → profVowels p
                    , marked `elem` wrong ]
            misplaced `shouldBe` []

        it "gives one language exactly one diacritic family" $ do
            let familyOf c = [ f | (f, _, _, lo, _) ← extendedLetterTable
                             , lo ≡ c ]
                mixed = [ (profSeed p, profileExtendedChars p)
                        | p ← v5Marked
                        , length (nub (concatMap familyOf
                                        (profileExtendedChars p))) ≢ 1 ]
            mixed `shouldBe` []
            -- And the derived accessor reports that single family
            -- rather than merely the first letter's.
            forM_ v5Marked $ \p →
                case nub (concatMap familyOf (profileExtendedChars p)) of
                    [f] → profileDiacritic p `shouldBe` Just f
                    fs  → expectationFailure $
                            "seed " ⧺ show (langSeedWord (profSeed p))
                            ⧺ " spans diacritic families " ⧺ show fs

        it "keeps the marked set small enough to read as a convention" $ do
            -- Consistency has an upper bound as well as a lower one: a
            -- language that marked everything would be noise, which is
            -- the failure mode the whole design principle rejects.
            let oversized = [ (profSeed p, profileExtendedChars p)
                            | p ← v5Marked
                            , length (profileExtendedChars p)
                                > 2 * maxMarksPerInventory ]
            oversized `shouldBe` []

        it "is drawn by some languages of the canonical sample and not \
           \others" $ do
            -- The acceptance criterion the whole design rests on: the
            -- choice VARIES by seed. One-sided in either direction and
            -- an accent identifies no language in particular.
            length v5Marked `shouldSatisfy` (≥ 100)
            length v5Plain `shouldSatisfy` (≥ 20)
            length v5Marked + length v5Plain `shouldBe` 256
            -- More than one family across the sample, for the same
            -- reason: difference ACROSS worlds, not just within one.
            nub [ profileDiacritic p | p ← v5Marked ]
                `shouldSatisfy` ((≥ 2) ∘ length)

        it "adds no extended letter below generator version 5" $ do
            -- Versions 1-4 are frozen output (#1092 requirement 4). An
            -- accented letter appearing in one would re-render an
            -- existing world's name.
            let historical = concat
                    [ [ buildProfileV1 (LangSeed s) | s ← [0 .. 63] ]
                    , [ buildProfileV2 (LangSeed s) | s ← [0 .. 63] ]
                    , [ buildProfileV3 (LangSeed s) | s ← [0 .. 63] ]
                    , [ buildProfileV4 (LangSeed s) | s ← [0 .. 63] ] ]
            filter (not ∘ null ∘ profileExtendedChars) historical
                `shouldBe` []
            map profileDiacritic historical
                `shouldSatisfy` all (≡ Nothing)
            extendedOrthographyVersion `shouldBe` 5

        it "never emits a mark the rendering language does not hold" $ do
            -- The negative form of the same property, measured on
            -- completed output rather than on the profile: a
            -- post-render substitution pass would show up here as a
            -- character the language never had.
            let offenders = [ (profSeed p, w, foreignExtended p w)
                            | (p, lr) ← v5Assignments
                            , (_, e) ← canonicalExpressions
                            , Right w ← [renderNative p lr e]
                            , not (null (foreignExtended p w)) ]
            offenders `shouldBe` []

        it "actually reaches completed names, in more than one of a \
           \language's own names" $ do
            -- Every rule above is "nothing wrong appears", which a
            -- generator that drew inventories and then never used them
            -- would also satisfy.
            let markedNames p lr =
                    [ w | (_, e) ← canonicalExpressions
                        , Right w ← [renderNative p lr e]
                        , T.any isExtendedLetter w ]
                perLanguage = [ length (markedNames p lr)
                              | (p, lr) ← v5Assignments
                              , not (null (profileExtendedChars p)) ]
            sum perLanguage `shouldSatisfy` (> 0)
            -- A marked language shows its marks across its names, not
            -- in a single one — that is what "a convention" means.
            length (filter (≥ 2) perLanguage)
                `shouldSatisfy` (≥ length perLanguage `div` 2)

    describe "extended letters obey every phonological rule (#1100 requirement 2)" $ do
        it "participates in #1094's admissible-onset relation" $ do
            let markedPairs =
                    [ (profSeed p, a, b)
                    | p ← v5Marked, (a, b) ← onsetPairs (profOnset p)
                    , isExtendedLetter a ∨ isExtendedLetter b ]
                -- Every admitted pair is still drawn from the profile's
                -- own inventory, marked letters included.
                outside =
                    [ (profSeed p, a, b)
                    | p ← v5Profiles, (a, b) ← onsetPairs (profOnset p)
                    , a `notElem` profConsonants p
                      ∨ b `notElem` profConsonants p ]
            markedPairs `shouldSatisfy` not ∘ null
            outside `shouldBe` []

        it "keeps every version-5 relation inside #1094's density band" $ do
            -- The widened inventories change the n(n-1) denominator, so
            -- the band is re-checked rather than assumed to survive.
            filter (not ∘ onsetDensityOk) v5Profiles `shouldBe` []

        it "can be a #1095 boundary-repair segment" $ do
            let marked = [ profSeed p | p ← v5Marked
                         , T.any isExtendedLetter
                                 (boundarySegmentText (profBoundary p)) ]
                outside =
                    [ (profSeed p, c)
                    | p ← v5Profiles
                    , c ← T.unpack (boundarySegmentText (profBoundary p))
                    , c `notElem` (profVowels p <> profConsonants p) ]
            marked `shouldSatisfy` not ∘ null
            outside `shouldBe` []

        it "can be an affix letter" $ do
            let marked = [ profSeed p | p ← v5Marked
                         , T.any isExtendedLetter (plmAffix (profPlural p))
                           ∨ T.any isExtendedLetter
                                   (pmAffix (profPossessive p)) ]
            marked `shouldSatisfy` not ∘ null

        it "can appear in a #1096 bound form, admissibly" $ do
            let boundForms = [ (p, b) | (p, lr) ← v5Assignments
                             , b ← M.elems (lrBound lr) ]
                marked = [ b | (_, b) ← boundForms, T.any isExtendedLetter b ]
                inadmissible = [ (profSeed p, b) | (p, b) ← boundForms
                               , not (boundFormAdmissible p b) ]
            marked `shouldSatisfy` not ∘ null
            inadmissible `shouldBe` []

        it "forms no triple-letter run in any canonical version-5 name" $ do
            -- #1095's guarantee over the widened inventory. The
            -- detector itself had to widen for this to mean anything —
            -- an ASCII-only letter predicate walks straight past 'ááá'.
            let named = [ (s, w) | s ← [0 .. 127 ∷ Word64]
                        , Right w ← nativeRenderingsV5 s ]
                offenders = [ (s, w) | (s, w) ← named, hasTripleRun w ]
            named `shouldSatisfy` ((≥ 600) ∘ length)
            named `shouldSatisfy` any (T.any isExtendedLetter ∘ snd)
            offenders `shouldBe` []

        it "begins no canonical version-5 name with an inadmissible or \
           \repeated two-consonant onset" $ do
            -- The same word-initial scoping the version-2 sweep uses,
            -- re-run over the widened inventories: an extended
            -- consonant is subject to #1094's relation exactly as an
            -- ASCII one is, including at the capitalized initial.
            let checked =
                    [ (profSeed p, w, a, b)
                    | p ← v5Profiles
                    , Right w ← renderingsFor p
                    , (a, b) ← wordInitialOnsets p w ]
                offenders =
                    [ x
                    | x@(sd, _, a, b) ← checked
                    , p ← [buildProfileV5 sd]
                    , a ≡ b ∨ not (admissibleOnset p a b) ]
            checked `shouldSatisfy` not ∘ null
            checked `shouldSatisfy`
                any (\(_, _, a, b) → isExtendedLetter a ∨ isExtendedLetter b)
            offenders `shouldBe` []

        it "detects a triple of a marked letter the same as an ASCII one" $ do
            -- Pins the widened predicate directly, so the zero above is
            -- evidence the guarantee holds rather than evidence the
            -- detector cannot see a violation.
            hasTripleRun "\x00E1\x00E1\x00E1" `shouldBe` True
            hasTripleRun "\x00C1\x00E1\x00E1" `shouldBe` True
            hasTripleRun "a\x00E1\x00E1" `shouldBe` False
            hasTripleRun "\x00E1-\x00E1\x00E1" `shouldBe` False

    describe "capitalization covers extended initials (#1100 requirement 5)" $ do
        it "pairs every repertoire member with the uppercase rendering \
           \actually produces" $
            -- Language.Generated.Render.capitalizeWord uses toUpper, so
            -- the table is only the authority if the two agree. A
            -- member whose simple uppercase were itself, or a different
            -- character, would render an uncapitalized or unlisted
            -- initial.
            forM_ extendedLetterTable $ \(_, _, _, lo, up) → do
                toUpper lo `shouldBe` up
                extendedUppercaseOf lo `shouldBe` Just up
                up `shouldSatisfy` (≢ lo)
                lo `shouldSatisfy` (`elem` outputInventory)
                up `shouldSatisfy` (`elem` outputInventory)

        it "capitalizes a real name whose root starts with a marked \
           \letter" $ do
            let initials = [ (profSeed p, w)
                           | (p, lr) ← v5Assignments
                           , (_, e) ← canonicalExpressions
                           , Right w ← [renderNative p lr e]
                           , Just (c, _) ← [T.uncons w]
                           , isExtendedLetter c ]
                lowercased = [ x | x@(_, w) ← initials
                             , Just (c, _) ← [T.uncons w]
                             , c `elem` extendedLetters ]
            initials `shouldSatisfy` not ∘ null
            lowercased `shouldBe` []

    describe "the output contract over the widened repertoire (#1100 requirements 4, 6)" $ do
        it "holds for every canonical version-5 name" $ do
            let allRenderings = concatMap nativeRenderingsV5 [0 .. 40]
                texts = [ w | Right w ← allRenderings ]
            length texts `shouldBe` length allRenderings
            texts `shouldSatisfy` any (T.any isExtendedLetter)
            filter (not ∘ contractOk) texts `shouldBe` []

        it "admits nothing outside the canonical output inventory" $ do
            let stray = [ (profSeed p, w, c)
                        | (p, lr) ← v5Assignments
                        , (_, e) ← canonicalExpressions
                        , Right w ← [renderNative p lr e]
                        , c ← T.unpack w
                        , c `notElem` outputInventory ]
            stray `shouldBe` []

        it "counts length in code points, not bytes" $ do
            -- #1100 requirement 6. Text.length is already code points;
            -- this pins that the 3-32 contract is being read that way,
            -- by exhibiting a name whose UTF-8 encoding is longer than
            -- its length.
            let wide = [ w | (p, lr) ← v5Assignments
                       , (_, e) ← canonicalExpressions
                       , Right w ← [renderNative p lr e]
                       , T.any isExtendedLetter w ]
            wide `shouldSatisfy` not ∘ null
            forM_ wide $ \w → do
                T.length w `shouldSatisfy` (≤ 32)
                T.length w `shouldSatisfy` (≥ 3)
                BS.length (encodeUtf8 w) `shouldSatisfy` (> T.length w)

        it "accepts and rejects exactly what the report tool's regex \
           \does" $ do
            -- The predicate and @tools/language_report.py@'s
            -- CONTRACT_RE are two statements of ONE contract, so the
            -- cases are mirrored verbatim in that tool's --self-test.
            -- Without the negative half a weaker predicate reports
            -- "zero contract violations" for output the enforced regex
            -- would reject.
            let accepted =
                    [ "Kara", "Kara'b", "Kara-bo", "Kar"
                    , "K\x00E1r\x00F3", "\x00C1r\x00F3-b\x00E1"
                    , "\x00D8ka", "Ka\x00F8-r\x00E1'b" ]
                rejected =
                    [ ("lowercase initial",            "kara")
                    , ("lowercase extended initial",   "\x00E1ra")
                    , ("below the 3-character floor",  "Ka")
                    , ("uppercase in the interior",    "KAra")
                    , ("uppercase extended interior",  "K\x00C1ra")
                    , ("repeated hyphen",              "Kara--bo")
                    , ("repeated apostrophe",          "Kara''bo")
                    , ("hyphen then apostrophe",       "K-'ara")
                    , ("apostrophe then hyphen",       "K'-ara")
                    , ("leading mark",                 "-Kara")
                    , ("leading extended mark",        "-K\x00E1ra")
                    , ("trailing mark",                "Kara-")
                    , ("trailing extended mark",       "K\x00E1r\x00E1-")
                    , ("a digit",                      "Kar3")
                    , ("a letter outside the set",     "Kar\x00E6")
                    , ("a curly quote for the mark",   "Kara\x2019\&b")
                    -- A combining sequence renders identically to the
                    -- accepted precomposed letter and must still be
                    -- rejected: the repertoire is single code points.
                    , ("a combining mark",             "A\x0301ra\x0301")
                    -- Python's `$` matches before a trailing
                    -- newline, so the report tool's regex needs
                    -- `fullmatch` to agree with this predicate.
                    , ("a trailing newline",           "Kara\n")
                    , ("a trailing carriage return",   "Kara\r")
                    , ("an embedded newline",          "Ka\nra")
                    , ("empty",                        "") ]
            filter (not ∘ contractOk) accepted `shouldBe` []
            [ label | (label, w) ← rejected, contractOk w ] `shouldBe` []

        it "describes one canonical set, shared with the report tool" $ do
            -- The single explicit inventory the reviewed spec asks for:
            -- ASCII in both cases, the repertoire in both cases, and the
            -- two marks — sorted, deduplicated, nothing else.
            length outputInventory `shouldBe` 26 * 2 + 61 * 2 + 2
            outputInventory `shouldBe` sort (nub outputInventory)
            length extendedLetterTable `shouldBe` 61
            length extendedLetters `shouldBe` 61
            nameMarks `shouldBe` ['\'', '-']
            filter (not ∘ isNameLetter) outputInventory `shouldBe` nameMarks

    -- #1100 requirement 3: the font decision, proved rather than
    -- asserted. The atlases below are generated from the shipped .ttf
    -- files by the production path, so this is real rasterizable
    -- coverage — not membership in a repertoire list, which is only a
    -- REQUEST, and not membership in the glyph map alone, which #1098's
    -- own tests show can hold for a character that draws nothing.
    describe "every generated-name font supplies the whole repertoire (#1100 requirement 3)" $ do
        it "names the fonts the decision covers, and excludes the title \
           \font" $ do
            generatedNameFonts `shouldBe`
                ["assets/fonts/arcade.ttf", "assets/fonts/shell.ttf"]
            gothicFontPath `shouldSatisfy` (`notElem` generatedNameFonts)

        forM_ generatedNameFonts $ \path → do
            atlas ← runIO (nameFontAtlas path)
            it ("draws every output character in " ⧺ path) $ do
                let absent = [ c | c ← outputInventory, not (drawsGlyph atlas c) ]
                -- The sweep is over the COMPLETE possible output set —
                -- lowercase repertoire, uppercase initials, ASCII
                -- letters and both marks — not the characters some
                -- 1,280-name sample happened to produce.
                length outputInventory `shouldBe` 176
                absent `shouldBe` []

            it ("lays out real marked names in " ⧺ path) $ do
                -- The acceptance criteria's manual step, mechanised.
                -- Generated names reach no UI surface yet (#708's
                -- Phase 2), so there is nothing to photograph; what can
                -- be checked is that real names out of the real
                -- generator measure in a real atlas at the sizes the
                -- game loads these fonts at. The no-fallback sweep
                -- above is what makes a positive width mean something:
                -- the #1097 mark carries an advance of its own, so
                -- width alone would not distinguish a drawn glyph from
                -- a visible substitute.
                let marked = [ w | s ← [0 .. 63 ∷ Word64]
                             , Right w ← nativeRenderingsV5 s
                             , T.any isExtendedLetter w ]
                marked `shouldSatisfy` ((≥ 50) ∘ length)
                forM_ marked $ \w → forM_ [24, 48 ∷ Float] $ \sz →
                    calculateTextWidthScaled atlas sz (T.unpack w)
                        `shouldSatisfy` (> 0)

        gothic ← runIO (nameFontAtlas gothicFontPath)
        it "would fail for the title font, which is why it is excluded" $ do
            -- Without this the group could pass by accident on a
            -- repertoire every shipped font supplies, and the decision
            -- above would be recording a choice that never mattered.
            let absent = [ c | c ← outputInventory, not (drawsGlyph gothic c) ]
            absent `shouldSatisfy` not ∘ null
            gothicFontPath `shouldSatisfy` (`notElem` generatedNameFonts)
