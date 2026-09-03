-- | Version 4's bound morphemes (#1096): bound-form generation and
--   selection, version gating, the dependent/head slot matrix, the
--   slot coverage of real languages, and the joins the bound forms
--   create.
module Test.Headless.Language.Generated.Bound
    ( spec
    ) where

import UPrelude
import Test.Hspec
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Generated.Types
import Language.Generated.Orthography
import Language.Generated.Hash (boundSeed, conceptSeed)
import Language.Generated.Onset
import Language.Generated.Boundary
import Language.Generated.Bound
import Language.Generated.Profile
import Language.Generated.Root
import Language.Generated.Render
import Language.Generated.Report (canonicalExpressions, countDuplicateRoots,
                                   boundSlotExpressions)
import Test.Headless.Language.Generated.Support

-- | A hand-built version-4 language for #1096's slot-matrix unit tests,
--   in the same spirit as 'boundaryFixture': every rendered answer below
--   is READ off this fixture rather than inferred from whatever a seed
--   happened to draw.
--
--   The affixes are chosen so that neither mark triggers a boundary
--   repair against the bound form used here — the plural @h@ follows
--   @kab@'s @b@ across an ADMISSIBLE @bh@ onset, and the possessive
--   @'s@ carries its own separator — so each expected string shows the
--   bound form and its mark plainly instead of a repair segment.
--   'boundFixtureRepair' below covers the repaired case.
boundFixture ∷ CompoundOrder → GenitiveOrder → Profile
boundFixture compound genitive = (boundaryFixture BoundaryEpenthetic)
    { profVersion       = GeneratorVersion 4
    , profCompoundOrder = compound
    , profPossessive    = PossessiveMarking genitive "'s"
    , profPlural        = PluralMarking "h"
    }

-- | A version-4 fixture whose @y@ is BOTH a consonant and a vowel, with
--   a relation that admits @by@ and not @ky@ — so the two adjacencies
--   #1096 requirement 4's "consonant-capable" scoping brings into range
--   have different answers, and a test can tell the wider scoping from
--   #1095's consonant-only one rather than only observing that both
--   pass.
dualRoleBoundFixture ∷ Profile
dualRoleBoundFixture = (boundFixture ModifierFirst OwnerFirst)
    { profConsonants = "bhky"
    , profVowels     = "aey"
    , profOnset      = OnsetRelation (S.fromList [('b', 'h'), ('b', 'y')])
    }

-- | The fixture's assignment: one concept with a bound form, one
--   without. @HEAD@ has none, so it doubles as the "a concept with no
--   bound form uses its free form in every slot" case.
boundFixtureRoots ∷ LanguageRoots
boundFixtureRoots = LanguageRoots
    { lrFree  = M.fromList [(cid "DEP", "kaba"), (cid "HEAD", "ota")]
    , lrBound = M.fromList [(cid "DEP", "kab")]
    }

-- | The same assignment with bound morphology switched off — what a
--   pre-version-4 language's rendering of the identical expressions
--   looks like.
boundFixtureFree ∷ LanguageRoots
boundFixtureFree = freeRootsOnly (lrFree boundFixtureRoots)

-- | The one assignment whose bound form's own final segment meets its
--   grammatical mark across an INADMISSIBLE cluster, so #1095's repair
--   has to mediate the join the bound form created.
boundFixtureRepair ∷ LanguageRoots
boundFixtureRepair = LanguageRoots
    { lrFree  = M.fromList [(cid "DEP", "kasa"), (cid "HEAD", "ota")]
    , lrBound = M.fromList [(cid "DEP", "kas")]
    }

-- | The same assignment with one concept's bound form promoted to be
--   its FREE root and bound morphology switched off.
--
--   The reference every slot-matrix property below is stated against:
--   if a dependent slot really selected @c@'s bound form and a head slot
--   really did not, then rendering under this substitution — where that
--   same string is simply the concept's only form — must produce
--   BYTE-IDENTICAL output. It pins the selection without reimplementing
--   ordering, marking, or boundary repair, all of which stay in the
--   production path on both sides of the comparison.
asFreeRoot ∷ ConceptId → LanguageRoots → LanguageRoots
asFreeRoot c lr = freeRootsOnly $ case M.lookup c (lrBound lr) of
    Nothing → lrFree lr
    Just b  → M.insert c b (lrFree lr)

spec ∷ Ctx → Spec
spec Ctx{..} = do
    -- #1096: version 4's bound morphemes.
    describe "bound-form selection (#1096 requirements 2, 3, 5)" $ do
        it "selects PINNED bound forms for fixed seeds" $ do
            -- What "deterministic for the same version, seed, and
            -- catalogue" actually claims: THESE concepts, with THESE
            -- forms, for this seed over the production catalogue. Seed
            -- 0's own map is the pinned golden, so these are the
            -- three other languages of the same fixed set.
            let boundFor s =
                    M.toList (lrBound (rootsFor (buildProfileV4 (LangSeed s))))
            boundFor 1 `shouldBe`
                [ (cid "BLESSING", "hy"), (cid "HAWK", "sy")
                , (cid "HEARTH", "ysy"), (cid "HOUND", "su")
                , (cid "MOON", "uzy"), (cid "SAND", "ypy")
                , (cid "SHAME", "uhu"), (cid "SMOKE", "uf") ]
            boundFor 42 `shouldBe`
                [ (cid "ANGEL", "voko"), (cid "COAL", "vik")
                , (cid "FAMINE", "gokt"), (cid "FATE", "bkeb")
                , (cid "FROST", "ki"), (cid "GATE", "ter")
                , (cid "RAVEN", "bit"), (cid "WINTER", "kta") ]
            boundFor (12345 ∷ Word64) `shouldBe`
                [ (cid "BRIDGE", "piy"), (cid "CROWN", "gaph")
                , (cid "HARBOR", "hayhuha"), (cid "HOLLOW", "ruyk")
                , (cid "HORIZON", "hak"), (cid "SHADOW", "huk")
                , (cid "THUNDER", "yuh"), (cid "TITAN", "hu") ]

        it "does not depend on the order the catalogue is enumerated in" $ do
            -- Requirement 2's "must not depend on catalogue traversal
            -- order", pinned at BOTH levels: the ranking itself, and the
            -- assignment built on top of it.
            let ids = conceptIds prodCat
                shuffled = reverse (take 75 ids) <> drop 75 ids
            forM_ [0, 7, 99 ∷ Word64] $ \s → do
                let p = buildProfileV4 (LangSeed s)
                boundSelectionOrder p ids
                    `shouldBe` boundSelectionOrder p (reverse ids)
                boundSelectionOrder p ids
                    `shouldBe` boundSelectionOrder p shuffled
                lrBound (expectRoots (assignLanguageRoots p prodOrds ids))
                    `shouldBe` lrBound (expectRoots
                        (assignLanguageRoots p prodOrds (reverse ids)))

        it "visits every concept exactly once, in a total order" $ do
            -- The (rank, ConceptId) tie-break is what makes the order
            -- total: without it two concepts that hashed equal would be
            -- ordered by whatever the caller presented first.
            let ids = conceptIds prodCat
                p   = buildProfileV4 (LangSeed 3)
                order = boundSelectionOrder p ids
            length order `shouldBe` length ids
            sort order `shouldBe` sort ids

        it "never accepts more than eight per language" $ do
            maxBoundForms `shouldBe` 8
            let over = [ (profSeed p, M.size (lrBound lr))
                       | (p, lr) ← v4Assignments
                       , M.size (lrBound lr) > maxBoundForms ]
            over `shouldBe` []

        it "accepts some in every language of the canonical sample" $ do
            -- Every rule above is "no bad form exists", which a
            -- generator that quietly stopped selecting anything would
            -- also satisfy.
            let empties = [ profSeed p | (p, lr) ← v4Assignments
                          , M.null (lrBound lr) ]
            empties `shouldBe` []

        it "stores only nonempty strictly-shorter prefixes that retain a \
           \visible letter (requirement 3)" $ do
            let visible = T.any isNameLetter
                offenders =
                    [ (profSeed p, c, free, b)
                    | (p, c, free, b) ← v4BoundForms
                    , T.null b
                      ∨ not (b `T.isPrefixOf` free)
                      ∨ T.length b ≥ T.length free
                      ∨ not (visible b) ]
            v4BoundForms `shouldSatisfy` not ∘ null
            offenders `shouldBe` []

        it "differs from the free form ONLY by deleted terminal \
           \characters" $ do
            -- Stated as its own assertion rather than folded into the
            -- prefix check above: this is the property that excludes
            -- stem substitution, internal deletion, and gradation, and
            -- it deserves to fail by name if it ever stops holding.
            let offenders =
                    [ (profSeed p, c, free, b)
                    | (p, c, free, b) ← v4BoundForms
                    , b <> T.drop (T.length b) free ≢ free ]
            offenders `shouldBe` []

        it "stores only forms its own profile's admissibility relation \
           \accepts (requirement 4)" $ do
            let offenders = [ (profSeed p, c, b)
                            | (p, c, _, b) ← v4BoundForms
                            , not (boundFormAdmissible p b) ]
            offenders `shouldBe` []

            -- The predicate really rejects: a bound form carrying an
            -- inadmissible cluster is refused by the same relation.
            let fx = boundFixture ModifierFirst OwnerFirst
            boundFormAdmissible fx "kab" `shouldBe` True    -- 'bh'-free
            boundFormAdmissible fx "kabh" `shouldBe` True   -- 'bh' admitted
            boundFormAdmissible fx "kabs" `shouldBe` False  -- 'bs' is not
            boundFormAdmissible fx "kabb" `shouldBe` False  -- irreflexive

        it "validates a pair involving a dual-role 'y', which a \
           \consonant-ONLY scoping would have skipped" $ do
            -- Requirement 4 says CONSONANT-CAPABLE, and a dual-role 'y'
            -- (#1094 requirement 6) is exactly that. This is where the
            -- filter is deliberately WIDER than #1095's boundary rule,
            -- which asks about consonant-only pairs because it rewrites
            -- text whose slot provenance it cannot know.
            boundFormAdmissible dualRoleBoundFixture "by" `shouldBe` True
            boundFormAdmissible dualRoleBoundFixture "ky" `shouldBe` False
            boundFormAdmissible dualRoleBoundFixture "yb" `shouldBe` False

            -- And it is load bearing on real languages, not only on a
            -- fixture: candidates exist that this scoping rejects and a
            -- consonant-only one would have let through.
            let narrowOnly p c = consonantCapable p c ∧ not (vowelCapable p c)
                narrowAdmissible p t = and
                    [ not (narrowOnly p a ∧ narrowOnly p b)
                      ∨ admissibleOnset p a b
                    | (a, b) ← T.zip t (T.drop 1 t) ]
                divergent =
                    [ (profSeed p, c, cand)
                    | (p, lr) ← take 64 v4Assignments
                    , (c, r) ← M.toList (lrFree lr)
                    , cand ← boundCandidates p c r
                    , narrowAdmissible p cand
                    , not (boundFormAdmissible p cand) ]
            divergent `shouldSatisfy` not ∘ null

        it "has zero free/free and zero bound-related collisions \
           \(requirement 5)" $ do
            let freeFree = [ (profSeed p, countDuplicateRoots (lrFree lr))
                           | (p, lr) ← v4Assignments
                           , countDuplicateRoots (lrFree lr) ≢ 0 ]
                boundAny = [ (profSeed p, n)
                           | (p, lr) ← v4Assignments
                           , let n = countBoundCollisions (lrFree lr) (lrBound lr)
                           , n ≢ 0 ]
            freeFree `shouldBe` []
            boundAny `shouldBe` []

            -- The bound-collision counter really counts: an assignment
            -- whose bound form equals another concept's free root is
            -- reported, and one that equals another bound form is too.
            countBoundCollisions
                (M.fromList [(cid "A", "kaba"), (cid "B", "kab")])
                (M.fromList [(cid "A", "kab")]) `shouldBe` 1
            countBoundCollisions
                (M.fromList [(cid "A", "kabo"), (cid "B", "kabe")])
                (M.fromList [(cid "A", "kab"), (cid "B", "kab")])
                `shouldBe` 2
            countBoundCollisions (lrFree boundFixtureRoots)
                                  (lrBound boundFixtureRoots) `shouldBe` 0

        it "never rerolls or changes a free root because of a bound form" $ do
            -- Requirement 5: existing free-root uniqueness is untouched,
            -- so the free half of a version-4 assignment is EXACTLY what
            -- assignRoots alone produces.
            let offenders =
                    [ profSeed p
                    | (p, lr) ← take 64 v4Assignments
                    , lrFree lr ≢ expectRoots
                        (assignRoots p prodOrds (conceptIds prodCat)) ]
            offenders `shouldBe` []

        it "ranks by a value domain-separated from root generation" $ do
            -- Requirement 2's "dedicated" value: if the ranking simply
            -- reused the per-concept root seed, which concepts got bound
            -- forms would be a function of the root draw rather than a
            -- selection of its own.
            let p = buildProfileV4 (LangSeed 11)
                ids = take 40 (conceptIds prodCat)
                same = [ c | c ← ids
                       , boundSeed (profVersion p) (profSeed p) c
                           ≡ conceptSeed (profVersion p) (profSeed p) c 0 ]
            same `shouldBe` []

    describe "bound forms exist only from generator version 4 (#1096 requirement 1)" $ do
        it "versions 1-3 assign none at all" $ do
            let historical = [ buildProfileV1 (LangSeed s) | s ← [0 .. 15] ]
                          <> [ buildProfileV2 (LangSeed s) | s ← [0 .. 15] ]
                          <> [ buildProfileV3 (LangSeed s) | s ← [0 .. 15] ]
                offenders = [ (profVersion p, profSeed p)
                            | p ← historical
                            , not (M.null (lrBound (rootsFor p)))
                              ∨ formsBoundMorphemes p ]
            offenders `shouldBe` []
            boundFormVersion `shouldBe` 4

        it "so a historical language renders every dependent slot with \
           \the free form — which is what keeps its goldens identical" $ do
            let exprs = map snd canonicalExpressions
                offenders =
                    [ (profVersion p, profSeed p, e)
                    | p ← [ buildProfileV1 (LangSeed s) | s ← [0 .. 15] ]
                       <> [ buildProfileV2 (LangSeed s) | s ← [0 .. 15] ]
                       <> [ buildProfileV3 (LangSeed s) | s ← [0 .. 15] ]
                    , let lr = rootsFor p
                    , e ← exprs
                    , renderNative p lr e
                        ≢ renderNative p (freeRootsOnly (lrFree lr)) e ]
            offenders `shouldBe` []

        it "version 4 assigns them" $
            filter (not ∘ formsBoundMorphemes) v4Profiles `shouldBe` []

    describe "the bound-form slot matrix (#1096 requirements 6, 7)" $ do
        let dep = cid "DEP"
            hd  = cid "HEAD"
            renderRow prof lr (_, e) = renderNative prof lr e

        it "renders every row of the matrix, both ordering directions" $ do
            -- Read straight off the fixture: DEP's free form is 'kaba'
            -- and its bound form 'kab', HEAD's only form is 'ota', the
            -- plural mark is 'h' and the possessive "'s".
            let modFirst = boundFixture ModifierFirst OwnerFirst
                headFirst = boundFixture HeadFirst HeadFirstGenitive
                rows p = map (renderRow p boundFixtureRoots)
                             (boundSlotExpressions dep hd)
            rows modFirst `shouldBe`
                [ Right "Kaba", Right "Kabota", Right "Kabota"
                , Right "Kabhota", Right "Kab'sota" ]
            rows headFirst `shouldBe`
                [ Right "Kaba", Right "Otakab", Right "Otakab"
                , Right "Otakabh", Right "Otakab's" ]

        it "Bare always uses the free form, in both directions" $ do
            forM_ [ boundFixture ModifierFirst OwnerFirst
                  , boundFixture HeadFirst HeadFirstGenitive ] $ \p → do
                renderNative p boundFixtureRoots (Bare dep)
                    `shouldBe` Right "Kaba"
                renderNative p boundFixtureRoots (Bare dep)
                    `shouldBe` renderNative p boundFixtureFree (Bare dep)

        it "a concept with no bound form uses its free form in every \
           \slot" $ do
            -- HEAD carries no bound form, so putting IT in the dependent
            -- slot must render exactly as a bound-form-free language
            -- would.
            forM_ [ boundFixture ModifierFirst OwnerFirst
                  , boundFixture HeadFirst HeadFirstGenitive ] $ \p →
                forM_ (boundSlotExpressions hd dep) $ \(_, e) →
                    renderNative p boundFixtureRoots e
                        `shouldBe` renderNative p boundFixtureFree e

        it "applies grammatical marking AFTER selecting the bound form, \
           \not before (requirement 7)" $ do
            let p = boundFixture ModifierFirst OwnerFirst
            -- Marking the bound form gives 'kabh'/"kab's"; marking the
            -- free form first and shortening afterwards could not, since
            -- truncation would take the mark off again.
            applyPluralMark p "kab" `shouldBe` "kabh"
            applyPossessiveMark p "kab" `shouldBe` "kab's"
            renderNative p boundFixtureRoots (Of hd Plural dep)
                `shouldBe` Right "Kabhota"
            renderNative p boundFixtureRoots (Possessive dep hd)
                `shouldBe` Right "Kab'sota"
            -- Singular Of applies no number marker at all.
            renderNative p boundFixtureRoots (Of hd Singular dep)
                `shouldBe` Right "Kabota"

        it "sends the join the bound form created through #1095's \
           \boundary repair" $ do
            -- 'kas' meets the 'h' plural across an inadmissible 'sh'
            -- cluster this language rejects, so the repair mediates a
            -- boundary that exists only BECAUSE of the shortening.
            let p = boundFixture ModifierFirst OwnerFirst
            admissibleOnset p 's' 'h' `shouldBe` False
            boundaryNeedsRepair p "kas" "h" `shouldBe` True
            applyPluralMark p "kas" `shouldBe` "kasah"
            renderNative p boundFixtureRepair (Of hd Plural dep)
                `shouldBe` Right "Kasahota"

        it "keeps hyphens and apostrophes exactly as #1095 left them" $ do
            let p = (boundFixture ModifierFirst OwnerFirst)
                        { profJoin = JoinHyphen }
            renderNative p boundFixtureRoots (Modifier dep hd)
                `shouldBe` Right "Kab-ota"
            renderNative p boundFixtureRoots (Possessive dep hd)
                `shouldBe` Right "Kab's-ota"

        it "renders every row from an assignment holding only the \
           \concepts the expression references (requirement 8)" $ do
            -- renderNative never sees a Catalogue, so "no catalogue
            -- scan" is testable directly: hand it a two-concept
            -- assignment carved out of a real 150-concept language and
            -- every row must render, byte-identically.
            let offenders =
                    [ (profSeed p, c, label)
                    | (p, lr) ← take 32 v4Assignments
                    , c ← M.keys (lrBound lr)
                    , let h = headAgainst c
                    , let keep k _ = k ≡ c ∨ k ≡ h
                    , let minimal = LanguageRoots
                              { lrFree  = M.filterWithKey keep (lrFree lr)
                              , lrBound = M.filterWithKey keep (lrBound lr) }
                    , (label, e) ← boundSlotExpressions c h
                    , renderNative p minimal e ≢ renderNative p lr e
                      ∨ isLeft' (renderNative p minimal e) ]
            offenders `shouldBe` []

    describe "the slot matrix holds for real languages (#1096 requirement 6)" $ do
        it "a dependent slot renders exactly as if the bound form WERE \
           \the concept's only form" $ do
            -- The strongest statement available without reimplementing
            -- ordering, marking, or boundary repair: substitute the
            -- bound form in as the free root, switch bound morphology
            -- off, and the production renderer must produce the same
            -- bytes. Covers every row, every seed, both directions.
            let offenders =
                    [ (profSeed p, c, label)
                    | (p, lr) ← v4Assignments
                    , c ← M.keys (lrBound lr)
                    , let h = headAgainst c
                    , (label, e) ← boundSlotExpressions c h
                    , label ≢ "bare"
                    , renderNative p lr e ≢ renderNative p (asFreeRoot c lr) e ]
            offenders `shouldBe` []

        it "a head slot never consults a bound form, whichever side the \
           \profile writes first" $ do
            -- Deleting the HEAD concept's own bound form must change
            -- nothing. If the head slot ever selected one, this would
            -- differ for every language whose head happened to have one.
            let checked =
                    [ (p, lr, c, h, row)
                    | (p, lr) ← v4Assignments
                    , c ← M.keys (lrBound lr)
                    , let h = headAgainst c
                    , M.member h (lrBound lr)
                    , row ← boundSlotExpressions c h ]
                offenders =
                    [ (profSeed p, c, label)
                    | (p, lr, c, h, (label, e)) ← checked
                    , let noHead = lr { lrBound = M.delete h (lrBound lr) }
                    , renderNative p lr e ≢ renderNative p noHead e ]
            -- Only meaningful if the head really carries a bound form
            -- somewhere in the sample.
            checked `shouldSatisfy` not ∘ null
            offenders `shouldBe` []

        it "exercises both compound and both genitive ordering \
           \directions" $ do
            forM_ [ModifierFirst, HeadFirst] $ \o →
                filter ((≡ o) ∘ profCompoundOrder) v4Profiles
                    `shouldSatisfy` not ∘ null
            forM_ [OwnerFirst, HeadFirstGenitive] $ \o →
                filter ((≡ o) ∘ pmOrder ∘ profPossessive) v4Profiles
                    `shouldSatisfy` not ∘ null

        it "produces a visible shortening in completed output" $ do
            -- Requirement 3 shortens a MORPHEME; this is the separate,
            -- weaker-looking but load-bearing claim that the shortening
            -- reaches the finished name. A boundary repair can insert
            -- exactly what the truncation removed, so this is not
            -- implied by "a bound form exists".
            let shortened =
                    [ (profSeed p, c, label)
                    | (p, lr) ← v4Assignments
                    , c ← M.keys (lrBound lr)
                    , (label, e) ← boundSlotExpressions c (headAgainst c)
                    , renderNative p lr e
                        ≢ renderNative p (freeRootsOnly (lrFree lr)) e ]
            shortened `shouldSatisfy` not ∘ null

        it "every bound-slot name satisfies the output contract and \
           \carries no triple-letter run" $ do
            let named =
                    [ (profSeed p, w)
                    | (p, lr) ← v4Assignments
                    , c ← M.keys (lrBound lr)
                    , (_, e) ← boundSlotExpressions c (headAgainst c)
                    , Right w ← [renderNative p lr e] ]
            named `shouldSatisfy` ((≥ 1000) ∘ length)
            filter (not ∘ contractOk ∘ snd) named `shouldBe` []
            filter (hasTripleRun ∘ snd) named `shouldBe` []

    describe "no triple-letter run survives version 4's joins (#1096 requirement 8)" $
        it "holds for every canonical version-4 name across many seeds" $ do
            let named = [ (s, w) | s ← [0 .. 127 ∷ Word64]
                        , Right w ← nativeRenderingsV4 s ]
                offenders = [ (s, w) | (s, w) ← named, hasTripleRun w ]
            named `shouldSatisfy` ((≥ 600) ∘ length)
            named `shouldSatisfy` any (T.isInfixOf "-" ∘ snd)
            named `shouldSatisfy` any (T.isInfixOf "'" ∘ snd)
            named `shouldSatisfy` any (not ∘ T.isInfixOf "-" ∘ snd)
            offenders `shouldBe` []

-- | A rendering failed.
isLeft' ∷ Either α β → Bool
isLeft' (Right _) = False
isLeft' (Left _)  = True
