-- | @--language-report@ boot path (#710): dump every requested seed's
--   generated-language profile, signature, and canonical-expression
--   native/English renderings as JSON to stdout, then exit. Reads the
--   production concept catalogue from disk and does pure computation
--   only — no engine init, no world thread, no Lua, no GPU
--   (requirement 17).
--
--   Cost model: a small fixed startup (one catalogue read), then work
--   AND memory linear in the requested range — every seed's report is
--   built and held before anything is written, because the whole
--   document is encoded in one go. A wide range is correspondingly
--   slow and correspondingly large in memory; this is not a
--   constant-time dump.
--
--   The range is all-or-nothing. 'buildSeedReport' runs under 'mapM' in
--   'Either', so ONE rejected seed — an unsupported generator version,
--   or a profile whose root space cannot name the catalogue (#2206) —
--   fails the entire run with status 1 and writes no partial JSON to
--   stdout. Splitting a failing range is the caller's job.
module App.LanguageReport
  ( runLanguageReport
  ) where

import UPrelude
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Aeson (Value, object, (.=), encode)
import Language.Semantic.Types (ConceptId(..), catVersion, conceptCount,
                                 catalogueErrorText)
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue )
import Language.Generated.Types
import Language.Generated.Onset (onsetTotalPairs)
import Language.Generated.Orthography
    (outputInventory, profileDiacriticText, profileExtendedChars)
import Language.Generated.Report

runLanguageReport ∷ (Word64, Word64) → IO ()
runLanguageReport (loSeed, hiSeed) = do
  hPutStrLn stderr $ "language-report: seeds " ⧺ show loSeed ⧺ ":" ⧺ show hiSeed
  result ← loadCatalogue conceptCataloguePath conceptOrdinalPath
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
                  -- #1100: the generator's own statement of every
                  -- character a name can contain.
                  -- @tools/language_report.py@ carries an independent
                  -- literal copy and fails when the two differ, so the
                  -- contract it enforces cannot silently follow a
                  -- widened repertoire it was never reviewed against.
                  , "outputInventory"  .= T.pack outputInventory
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
    -- #1096's bound-form dataset, kept as its OWN arrays beside
    -- "renderings" rather than merged into it: the checker's
    -- distinct-name, profile-signature, and pinned length gates are
    -- measured against the canonical population, and mixing these in
    -- would move those denominators.
    , "boundForms"        .= map boundFormJSON (srBoundForms sr)
    , "boundCollisions"   .= srBoundCollisions sr
    , "boundRenderings"   .= map boundRenderingJSON (srBoundRenderings sr)
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
    , "compoundOrder"   .= tshow (profCompoundOrder p)
    , "genitiveOrder"   .= tshow (pmOrder (profPossessive p))
    , "possessiveAffix" .= pmAffix (profPossessive p)
    , "pluralAffix"     .= plmAffix (profPlural p)
    , "joinStyle"       .= tshow (profJoin p)
    , "onsetPairs"      .= map pairText (onsetPairs (profOnset p))
    , "onsetAdmissible" .= onsetPairCount (profOnset p)
    , "onsetTotal"      .= onsetTotalPairs p
    -- #1095's per-language boundary phonology, emitted for the same
    -- reason the onset relation is: it is style state the report's
    -- diversity metrics must be able to see, and the checker's own
    -- diagnostics name the rule a language actually chose.
    , "boundaryRule"     .= boundaryRuleText (profBoundary p)
    , "boundarySegments" .= boundarySegmentText (profBoundary p)
    -- #1100's per-language orthography, derived from the inventories
    -- above rather than stored beside them. Emitted so the checker can
    -- assert the property that makes an accent a convention instead of
    -- noise: every extended character in a rendered name belongs to the
    -- language that rendered it.
    , "extendedChars"    .= T.pack (profileExtendedChars p)
    , "diacritic"        .= profileDiacriticText p
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

-- | One selected concept's free and bound morphemes. @admissible@ is
--   the Haskell-computed verdict of #1094's exported relation — the one
--   bound-form signal the Python checker cannot derive from the exposed
--   strings without reimplementing generation logic. The prefix and
--   collision rules it evaluates itself from @free@/@bound@ and the two
--   collision totals.
boundFormJSON ∷ BoundFormRecord → Value
boundFormJSON bf = object
    [ "concept"    .= conceptIdText (bfConcept bf)
    , "free"       .= bfFree bf
    , "bound"      .= bfBound bf
    , "admissible" .= bfAdmissible bf
    ]

boundRenderingJSON ∷ BoundSlotRendering → Value
boundRenderingJSON bsr = object
    [ "concept"     .= conceptIdText (bsrConcept bsr)
    , "slot"        .= bsrSlot bsr
    , "native"      .= either (const Nothing) Just (bsrNative bsr)
    , "nativeError" .= either Just (const Nothing) (bsrNative bsr)
    , "shortened"   .= bsrShortened bsr
    ]
