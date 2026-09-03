{-# LANGUAGE Strict #-}
-- | "River naming" and "River identity" (#1102): a world's rivers named
--   in its own generated language, and the association that says which
--   river is which. Pure — no engine. The production concept catalogue
--   is read straight from @data/language/concepts.yaml@, so the scheme
--   these specs exercise is the one that ships.
module Test.Headless.River.Naming (spec) where

import UPrelude
import Test.Hspec
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Semantic.Types
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue )
import Language.Generated.Types
    ( LanguageProvenance(..), LangSeed(..), currentGeneratorVersion )
import Language.Generated.Profile (generateProfile)
import Language.Generated.Root (assignLanguageRoots)
import Test.Headless.Language.Generated.Support (expectRoots)
import Language.Generated.Bound (LanguageRoots(..))
import Language.Naming (mkNamer)
import World.Base (GeoFeatureId(..), GeoCoord(..))
import World.Geology.Timeline.Types
    ( GeoTimeline(..), GeoPeriod(..), GeoScale(..), GeoEvent(..)
    , FeatureShape(..), FeatureActivity(..), PersistentFeature(..)
    , defaultErosionParams, emptyTimeline, noBBox )
import World.Hydrology.Types
    (HydroFeature(..), RiverParams(..), RiverSegment(..))
import World.River.Identity (timelineRivers, timelineRiverFeatureIds)
import World.River.Naming

-- * Fixtures ---------------------------------------------------------

-- | Two DIFFERENT languages. Distinct seeds at the current generator,
--   mirroring "Test.Headless.Location.Naming"'s pair.
provA, provB ∷ LanguageProvenance
provA = LanguageProvenance (LangSeed 0x5EED0000000000A1) currentGeneratorVersion
provB = LanguageProvenance (LangSeed 0x0FF1CE0000000B2C) currentGeneratorVersion

-- | Eight rivers — more than the six-entry head pool, so the pigeonhole
--   makes head RECURRENCE a property of the scheme rather than of a
--   lucky draw.
riverIds ∷ [GeoFeatureId]
riverIds = map GeoFeatureId [0 .. 7]

-- | GOLDEN -- every name 'provA' renders for 'riverIds', in the
--   ascending 'GeoFeatureId' order 'riverNamesToList' fixes with its
--   @sortOn fst@.
--
--   Written out by hand, and never re-derived: an expectation produced
--   by calling 'buildRiverNames' or 'namerOf' a second time compares
--   the generator with itself and measures nothing, which is exactly
--   the hole this list was written to close (#1385).
--
--   /What a failure here means./ The rendering this generator produces
--   for a FIXED seed, a FIXED concept catalogue and FIXED river ids has
--   changed. 'provA' carries 'currentGeneratorVersion', so a version
--   bump changes the provenance itself and lands here too -- that case
--   is DELIBERATE. A move with no accompanying bump is ACCIDENTAL, and
--   costly: river names are write-once (#708 principle 5), so every
--   name already stored in a save keeps the old rendering, and #1104's
--   etymology re-renders a stored name and CHECKS it against the
--   recorded provenance before showing any of it. This is not a list to
--   re-bless until the suite goes green; find the change that moved it
--   first.
goldenNamesA ∷ [Text]
goldenNamesA =
    [ "Ydfyą-jdyebto"          -- 0
    , "Kelyayw-dowąc"          -- 1
    , "Aydeel-nylwo"           -- 2
    , "Ftobącbje-ywetąftla"    -- 3
    , "Oney-ąckoy"             -- 4
    , "Tąracwa-nylwo"          -- 5
    , "Ląew-wnayąeb"           -- 6
    , "Ląew-ywetąftla"         -- 7
    ]

-- | Three of those eight pinned WITH the gloss stored beside them, id
--   and all, so the correspondence is the thing under test rather than
--   two independently ordered vectors that happen to line up.
--
--   Chosen, not sampled: 1 is the RIVER head itself, the concept #1102
--   added for this pool; 6 and 7 share the modifier morpheme @Ląew@ and
--   differ only in their head, so a gloss that tracked position instead
--   of its own name would have to break one of the two. The same
--   re-blessing rule as 'goldenNamesA' applies.
goldenNamedGlossesA ∷ [(GeoFeatureId, Text, Maybe Text)]
goldenNamedGlossesA =
    [ (GeoFeatureId 1, "Kelyayw-dowąc",  Just "Sky River")
    , (GeoFeatureId 6, "Ląew-wnayąeb",   Just "Keep Vale")
    , (GeoFeatureId 7, "Ląew-ywetąftla", Just "Keep Crossing")
    ]

-- | A river's params. Only the fields "World.River.Identity" checks a
--   pairing against carry meaning here; the geometry is a single
--   placeholder segment.
mkRiver ∷ Int → RiverParams
mkRiver n = RiverParams
    { rpSourceRegion = GeoCoord (10 * n) (20 * n)
    , rpMouthRegion  = GeoCoord (10 * n + 5) (20 * n + 5)
    , rpFlowRate     = fromIntegral n + 0.5
    , rpSegments     = V.singleton RiverSegment
        { rsStart = GeoCoord (10 * n) (20 * n)
        , rsEnd   = GeoCoord (10 * n + 5) (20 * n + 5)
        , rsWidth = 3, rsValleyWidth = 9, rsDepth = 2
        , rsFlowRate = fromIntegral n + 0.5
        , rsStartElev = 100, rsEndElev = 90
        }
    }

riverFeature ∷ Int → Int → PersistentFeature
riverFeature fid n = PersistentFeature
    { pfId               = GeoFeatureId fid
    , pfFeature          = HydroShape (RiverFeature (mkRiver n))
    , pfActivity         = FActive
    , pfFormationPeriod  = 0
    , pfLastActivePeriod = 0
    , pfEruptionCount    = 0
    , pfParentId         = Nothing
    }

-- | A period carrying exactly the supplied events. The derived
--   bbox/exploded caches are irrelevant to river identity, which reads
--   'gpEvents' only.
periodOf ∷ [GeoEvent] → GeoPeriod
periodOf evts = GeoPeriod
    { gpName = "age", gpScale = Age, gpDuration = 1, gpDate = 0
    , gpEvents = evts
    , gpErosion = defaultErosionParams
    , gpRegionalErosion = HM.empty
    , gpTaggedEvents = []
    , gpExplodedEvents = V.empty
    , gpPeriodBBox = noBBox
    }

-- | The shape 'World.Geology.Timeline.Compact.compactRiverEvents'
--   leaves behind: one event per active river feature, in @gtFeatures@
--   order, all in one Age period, every other period stripped.
compactedTimeline ∷ [PersistentFeature] → GeoTimeline
compactedTimeline feats = emptyTimeline
    { gtFeatures = feats
    , gtPeriods  =
        [ periodOf []
        , periodOf [ HydroEvent (RiverFeature (riverParamsOf pf))
                   | pf ← feats
                   , FActive ← [pfActivity pf]
                   , HydroShape (RiverFeature _) ← [pfFeature pf] ]
        ]
    }
  where
    riverParamsOf pf = case pfFeature pf of
        HydroShape (RiverFeature rp) → rp
        _ → error "riverParamsOf: not a river"

threeRivers ∷ GeoTimeline
threeRivers = compactedTimeline [ riverFeature 7 1, riverFeature 2 2
                                , riverFeature 5 3 ]

spec ∷ Spec
spec = do
    prodCatE ← runIO $ loadCatalogue conceptCataloguePath conceptOrdinalPath
    let cat = either (error ∘ T.unpack ∘ catalogueErrorText) id prodCatE
        namerOf prov = case mkNamer cat prov of
            Left e  → error ("mkNamer failed: " <> show e)
            Right n → n
        namerA    = namerOf provA
        namerB    = namerOf provB
        builtA    = buildRiverNames (Just namerA) riverIds
        builtB    = buildRiverNames (Just namerB) riverIds
        builtNone = buildRiverNames Nothing riverIds
        namesOf   = map (rvnDisplayName ∘ snd) ∘ riverNamesToList
        glossesOf = map (rvnGloss ∘ snd) ∘ riverNamesToList
        headForms =
            [ T.toLower (ceSingular ce)
            | cid ← riverHeadConcepts, Just ce ← [lookupConcept cid cat] ]
        profileOf prov = case generateProfile (lpVersion prov) (lpSeed prov) of
            Left e  → error ("generateProfile failed: " <> show e)
            Right p → p

    describe "River naming" $ do
        describe "the concept catalogue this draws on" $ do
            it "carries RIVER with all four authored lexical forms (#1102 \
               \added it; the head pool is unusable without them)" $
                case lookupConcept (ConceptId "RIVER") cat of
                    Nothing → expectationFailure "RIVER is missing"
                    Just ce → do
                        ceSingular ce   `shouldBe` "river"
                        cePlural ce     `shouldBe` Just "rivers"
                        ceModifier ce   `shouldBe` Just "river"
                        cePossessive ce `shouldBe` Just "river's"

            it "carries every head the river pool names, with the singular \
               \form the head slot renders from -- so no head is silently \
               \dropped from the pool" $
                riverHeadPool cat `shouldBe` riverHeadConcepts

            it "offers a modifier pool far wider than the head pool -- the \
               \asymmetry that makes a head RECUR while the names differ" $
                length (riverModifierPool cat)
                    `shouldSatisfy` (> 10 * length riverHeadConcepts)

        describe "a world with a language" $ do
            it "names every river, and no two of these eight alike" $ do
                length (namesOf builtA) `shouldBe` length riverIds
                namesOf builtA `shouldSatisfy` all (not ∘ T.null)
                length (nub (namesOf builtA)) `shouldBe` length riverIds

            it "stores an English gloss for every name" $
                glossesOf builtA `shouldSatisfy` all
                    (maybe False (not ∘ T.null))

            it "the gloss is the SAME expression's English reading -- two \
               \words, modifier then head, the head from the river pool" $
                forM_ (riverNamesToList builtA) $ \(_, nm) →
                    case rvnGloss nm of
                        Nothing → expectationFailure "expected a gloss"
                        Just g  → case map T.toLower (T.words g) of
                            [_, h] → h `shouldSatisfy` (`elem` headForms)
                            other  → expectationFailure
                                ("expected a two-word gloss, got " <> show other)

            it "draws every name from ONE language: each is built from that \
               \language's own root assignment" $ do
                let roots = lrFree (expectRoots
                                (assignLanguageRoots (profileOf provA)
                                                      (catOrdinals cat)
                                                      (conceptIds cat)))
                    anyRootIn nm = any (\r → T.toLower r `T.isInfixOf` T.toLower nm)
                                       (M.elems roots)
                namesOf builtA `shouldSatisfy` all anyRootIn

            it "repeats a head across rivers -- the recurrence that makes \
               \the language legible, and the same head morpheme the \
               \world's OWN name would use for that concept" $ do
                let glossHeads = [ last (T.words g)
                                 | Just g ← glossesOf builtA ]
                    repeated   = [ h | h ← nub glossHeads
                                 , length (filter (≡ h) glossHeads) > 1 ]
                repeated `shouldSatisfy` (not ∘ null)

            it "renders exactly these eight names for this language -- \
               \an oracle written out by hand, so a rendering change \
               \has something to fail against" $
                namesOf builtA `shouldBe` goldenNamesA

            it "glosses each of those names as its OWN expression's \
               \English reading: name, gloss and feature id come from \
               \one stored entry" $
                [ (fid, rvnDisplayName nm, rvnGloss nm)
                | (fid, nm) ← riverNamesToList builtA
                , fid `elem` [ i | (i, _, _) ← goldenNamedGlossesA ] ]
                    `shouldBe` goldenNamedGlossesA

            it "keys names by feature id, not by position: reordering the \
               \id list changes nothing" $
                rvnById (buildRiverNames (Just namerA) (reverse riverIds))
                    `shouldBe` rvnById builtA

        describe "two worlds with different languages" $
            it "name the same rivers differently" $
                namesOf builtA `shouldSatisfy`
                    \a → and (zipWith (≢) a (namesOf builtB))

        describe "a world with NO language (#1102 requirement 6)" $ do
            it "names no river at all rather than inventing a language" $
                rvnById builtNone `shouldBe` HM.empty

            it "leaves every lookup empty, which every consumer reads as \
               \'unnamed'" $
                map (`lookupRiverName` builtNone) riverIds
                    `shouldSatisfy` all isNothing

        describe "write-once (#708 principle 5)" $ do
            it "a stored table survives the save DTO round trip unchanged" $
                case S.decode (S.encode builtA) of
                    Right back → back `shouldBe` builtA
                    Left err   → expectationFailure err

            it "would render DIFFERENT names under a catalogue missing \
               \RIVER -- so 'carried across, never re-derived' is a real \
               \guarantee and not a coincidence of stable inputs" $ do
                let shrunk = cat { catConcepts =
                        M.delete (ConceptId "RIVER") (catConcepts cat) }
                    shrunkNamer = case mkNamer shrunk provA of
                        Left e  → error ("mkNamer failed: " <> show e)
                        Right n → n
                namesOf (buildRiverNames (Just shrunkNamer) riverIds)
                    `shouldSatisfy` (≢ namesOf builtA)

    describe "River identity" $ do
        it "pairs every compacted river event with the feature id it was \
           \emitted from, in the timeline's own order" $
            map fst (timelineRivers threeRivers)
                `shouldBe` map (Just ∘ GeoFeatureId) [7, 2, 5]

        it "returns EXACTLY the rivers the query has always returned -- \
           \same values, same order, same count" $
            map snd (timelineRivers threeRivers)
                `shouldBe` map mkRiver [1, 2, 3]

        it "skips features that are not active rivers" $ do
            let mixed = compactedTimeline
                    [ riverFeature 1 1
                    , (riverFeature 9 9) { pfActivity = FDormant }
                    , riverFeature 4 2 ]
            timelineRiverFeatureIds mixed
                `shouldBe` map GeoFeatureId [1, 4]

        it "an EMPTY timeline has no rivers and no ids" $ do
            timelineRivers emptyTimeline `shouldBe` []
            timelineRiverFeatureIds emptyTimeline `shouldBe` []

        it "refuses to guess when the events and the features disagree: \
           \every river comes back with NO id rather than a wrong one" $ do
            let mismatched = threeRivers
                    { gtFeatures = gtFeatures threeRivers
                                     ⧺ [riverFeature 11 11] }
            map fst (timelineRivers mismatched)
                `shouldBe` replicate 3 Nothing
            timelineRiverFeatureIds mismatched `shouldBe` []

        it "refuses a pairing whose geometry does not match, even at the \
           \right count" $ do
            let swapped = threeRivers
                    { gtFeatures = [ riverFeature 7 1, riverFeature 2 99
                                   , riverFeature 5 3 ] }
            map fst (timelineRivers swapped) `shouldBe` replicate 3 Nothing
