-- | @--language-report@ boot path (#710): dump every requested seed's
--   generated-language profile, signature, and canonical-expression
--   native/English renderings as JSON to stdout, then exit. Reads the
--   production concept catalogue from disk and does pure computation
--   only — no engine init, no world thread, no Lua, no GPU
--   (requirement 17), so it starts and finishes in a fraction of a
--   second regardless of how many seeds are requested.
module App.LanguageReport
  ( runLanguageReport
  ) where

import UPrelude
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Aeson (Value, object, (.=), encode)
import Language.Semantic.Types (catVersion, conceptCount, catalogueErrorText)
import Language.Semantic.Catalogue (conceptCataloguePath, loadCatalogue)
import Language.Generated.Types
import Language.Generated.Onset (onsetTotalPairs)
import Language.Generated.Report

runLanguageReport ∷ (Word64, Word64) → IO ()
runLanguageReport (loSeed, hiSeed) = do
  hPutStrLn stderr $ "language-report: seeds " ⧺ show loSeed ⧺ ":" ⧺ show hiSeed
  result ← loadCatalogue conceptCataloguePath
  case result of
    Left err → do
      hPutStrLn stderr $ T.unpack (catalogueErrorText err)
      exitFailure
    Right cat → do
      hPutStrLn stderr $ "language-report: catalogue version "
          ⧺ show (catVersion cat) ⧺ " (" ⧺ show (conceptCount cat)
          ⧺ " concepts)"
      -- Reports are built at the CURRENT generator version through the
      -- real dispatcher (#1094 requirement 9), so the header version
      -- and the profiles below it can never disagree.
      case mapM (buildSeedReport cat currentGeneratorVersion)
                [loSeed .. hiSeed] of
        Left err → do
          hPutStrLn stderr $ T.unpack (generatorErrorText err)
          exitFailure
        Right reports → do
          let topJSON = object
                  [ "generatorVersion" .= generatorVersionInt currentGeneratorVersion
                  , "catalogueVersion" .= catVersion cat
                  , "conceptCount"     .= conceptCount cat
                  , "seeds"            .= map seedReportJSON reports
                  ]
          BL.putStr (encode topJSON)
          BL.putStr "\n"
          hFlush stdout
          hPutStrLn stderr $ "language-report: done (" ⧺ show (length reports) ⧺ " seeds)"

seedReportJSON ∷ SeedReport → Value
seedReportJSON sr = object
    [ "seed"             .= srSeed sr
    , "profileSignature" .= srProfileSignature sr
    , "profile"          .= profileJSON (srProfile sr)
    , "renderings"        .= map renderingJSON (srRenderings sr)
    , "rootCollisions"   .= srRootCollisions sr
    ]

-- | Everything @tools/language_report.py --check@ needs to EVALUATE the
--   #1094 gates itself, not just print them: the two inventories decide
--   which visible glyphs are consonant- and vowel-capable, @yRole@ and
--   @onsetPairs@ are the style state under test, and the density is
--   emitted as the two integer counts requirement 4 compares rather
--   than as a rounded percentage.
profileJSON ∷ Profile → Value
profileJSON p = object
    [ "version"         .= generatorVersionInt (profVersion p)
    , "consonants"      .= T.pack (profConsonants p)
    , "vowels"          .= T.pack (profVowels p)
    , "yRole"           .= yRoleText p
    , "syllableShapes"  .= map shapeText (profSyllableShapes p)
    , "minSyllables"    .= profMinSyllables p
    , "maxSyllables"    .= profMaxSyllables p
    , "compoundOrder"   .= T.pack (show (profCompoundOrder p))
    , "genitiveOrder"   .= T.pack (show (pmOrder (profPossessive p)))
    , "possessiveAffix" .= pmAffix (profPossessive p)
    , "pluralAffix"     .= plmAffix (profPlural p)
    , "joinStyle"       .= T.pack (show (profJoin p))
    , "onsetPairs"      .= map pairText (onsetPairs (profOnset p))
    , "onsetAdmissible" .= onsetPairCount (profOnset p)
    , "onsetTotal"      .= onsetTotalPairs p
    -- #1095's per-language boundary phonology, emitted for the same
    -- reason the onset relation is: it is style state the report's
    -- diversity metrics must be able to see, and the checker's own
    -- diagnostics name the rule a language actually chose.
    , "boundaryRule"     .= boundaryRuleText (profBoundary p)
    , "boundarySegments" .= boundarySegmentText (profBoundary p)
    ]
  where
    shapeText = T.pack ∘ map segChar ∘ shapeSegments
    segChar ConsonantSlot = 'C'
    segChar VowelSlot     = 'V'
    pairText (a, b) = T.pack [a, b]

renderingJSON ∷ CanonicalRendering → Value
renderingJSON cr = object
    [ "form"        .= crForm cr
    , "native"      .= either (const Nothing) Just (crNative cr)
    , "nativeError" .= either Just (const Nothing) (crNative cr)
    , "gloss"       .= either (const Nothing) Just (crGloss cr)
    , "glossError"  .= either Just (const Nothing) (crGloss cr)
    ]
