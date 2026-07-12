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
      let reports = [ buildSeedReport cat s | s ← [loSeed .. hiSeed] ]
          topJSON = object
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

profileJSON ∷ Profile → Value
profileJSON p = object
    [ "consonants"      .= T.pack (profConsonants p)
    , "vowels"          .= T.pack (profVowels p)
    , "syllableShapes"  .= map shapeText (profSyllableShapes p)
    , "minSyllables"    .= profMinSyllables p
    , "maxSyllables"    .= profMaxSyllables p
    , "compoundOrder"   .= T.pack (show (profCompoundOrder p))
    , "genitiveOrder"   .= T.pack (show (pmOrder (profPossessive p)))
    , "possessiveAffix" .= pmAffix (profPossessive p)
    , "pluralAffix"     .= plmAffix (profPlural p)
    , "joinStyle"       .= T.pack (show (profJoin p))
    ]
  where
    shapeText = T.pack ∘ map segChar ∘ shapeSegments
    segChar ConsonantSlot = 'C'
    segChar VowelSlot     = 'V'

renderingJSON ∷ CanonicalRendering → Value
renderingJSON cr = object
    [ "form"        .= crForm cr
    , "native"      .= either (const Nothing) Just (crNative cr)
    , "nativeError" .= either Just (const Nothing) (crNative cr)
    , "gloss"       .= either (const Nothing) Just (crGloss cr)
    , "glossError"  .= either Just (const Nothing) (crGloss cr)
    ]
