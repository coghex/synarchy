{-# LANGUAGE Strict #-}
-- | "Language etymology" (#1104): decomposing a generated name into its
--   roots and meanings, and the recurrence policy that says which OTHER
--   names may be shown as sharing a morpheme.
--
--   Pure — no engine. The production concept catalogue is read straight
--   from @data\/language\/concepts.yaml@ and every language is a real
--   generated 'Profile', so what these specs exercise is what ships: a
--   name is built by the SAME renderer the game uses, then handed to the
--   decomposition as if it had come back off disk.
--
--   That construction is deliberate. A hand-written @(name, expression)@
--   pair would pass the surface check only by accident, and would stop
--   testing anything the moment the generator's phonology moved; a name
--   rendered by 'renderNative' and explained by 'decomposeName' pins the
--   real relationship between the two.
module Test.Headless.Language.Etymology (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.English (renderGloss)
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue )
import Language.Generated.Types
    ( GeneratorVersion(..), LangSeed(..), LanguageProvenance(..)
    , CompoundOrder(..), GenitiveOrder(..), Profile(..)
    , PossessiveMarking(..), currentGeneratorVersion, generatorVersionInt )
import Language.Generated.Profile (generateProfile)
import Language.Generated.Root (assignLanguageRoots)
import Test.Headless.Language.Generated.Support (expectRoots)
import Language.Generated.Bound (LanguageRoots(..), assignBoundForms)
import Language.Generated.Render (renderNative)
import Language.Etymology
import Language.Etymology.Source (EtymologySource(..), decodeNameExpr, encodeNameExpr)
import Engine.Scripting.Lua.API.WorldQuery.Etymology
    (EtyEntity(..), eligibleEntities, recurrenceFor, riverAtTile)
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..), LocationLifecycle(..) )
import Location.Bounds (AbsBounds(..))
import World.Base (GeoCoord(..), GeoFeatureId(..))
import qualified Data.Vector as V
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import qualified Data.HashMap.Strict as HM
import World.Geology.Timeline.Types
    ( GeoTimeline(..), GeoPeriod(..), GeoScale(..), GeoEvent(..)
    , FeatureShape(..), FeatureActivity(..), PersistentFeature(..)
    , defaultErosionParams, emptyTimeline, noBBox )
import World.Hydrology.Types
    (HydroFeature(..), RiverParams(..), RiverSegment(..))
import World.Page.Types (WorldIdentity(..), WorldPageId(..))
import World.River.Naming (RiverName(..))

-- * Fixtures ----------------------------------------------------------

-- | Two DIFFERENT languages at the current generator, plus one
--   HISTORICAL version of the first seed — the third is what makes
--   \"same seed, different generator, therefore a different language\"
--   testable rather than assumed.
provA, provB, provAOld ∷ LanguageProvenance
provA    = LanguageProvenance (LangSeed 0x5EED0000000000A1) currentGeneratorVersion
provB    = LanguageProvenance (LangSeed 0x0FF1CE0000000B2C) currentGeneratorVersion
provAOld = LanguageProvenance (LangSeed 0x5EED0000000000A1) (GeneratorVersion 1)

-- | A language whose profile BUILDS but whose root space holds only 144
--   distinct roots against the catalogue's 151 concepts (#2206), so no
--   assignment exists to rebuild a name against.
provShort ∷ LanguageProvenance
provShort = LanguageProvenance (LangSeed 1116) currentGeneratorVersion

profileFor ∷ LanguageProvenance → Profile
profileFor prov = case generateProfile (lpVersion prov) (lpSeed prov) of
    Right p → p
    Left e  → error ("test setup: profile: " <> show e)

rootsFor ∷ Catalogue → LanguageProvenance → LanguageRoots
rootsFor cat prov = expectRoots
    (assignLanguageRoots (profileFor prov) (catOrdinals cat) (conceptIds cat))

-- | The name a language really renders for an expression, and the gloss
--   the catalogue really renders for it — i.e. exactly what the engine
--   would have STORED when it named something with this expression.
storedFor ∷ Catalogue → LanguageProvenance → NameExpr → (Text, Maybe Text)
storedFor cat prov expr =
    ( either (\e → error ("test setup: native: " <> show e)) id
             (renderNative (profileFor prov) (rootsFor cat prov) expr)
    , either (\e → error ("test setup: gloss: " <> show e)) Just
             (renderGloss cat expr) )

sourceFor ∷ LanguageProvenance → NameExpr → EtymologySource
sourceFor prov expr = EtymologySource { esExpr = expr, esLanguage = prov }

-- | Decompose an expression exactly as a stored name would be.
explain ∷ Catalogue → LanguageProvenance → NameExpr → EtymologyResult
explain cat prov expr =
    let (name, gloss) = storedFor cat prov expr
    in decomposeName cat name gloss (Just (sourceFor prov expr))

available ∷ EtymologyResult → Etymology
available (EtyAvailable e) = e
available (EtyUnavailable u) =
    error ("expected an available etymology, got " <> show u)

unavailableOf ∷ EtymologyResult → Maybe EtyUnavailable
unavailableOf (EtyUnavailable u) = Just u
unavailableOf _                  = Nothing

isAvailable ∷ EtymologyResult → Bool
isAvailable EtyAvailable{}   = True
isAvailable EtyUnavailable{} = False

-- | The first morpheme of a decomposition, or a descriptive failure.
--   Total by construction rather than by 'head': every expression these
--   specs build has at least one, and a change that made one empty
--   should say so rather than throw @Prelude.head@.
firstMorpheme ∷ Etymology → EtyMorpheme
firstMorpheme ety = case etyMorphemes ety of
    (m : _) → m
    []      → error ("no morphemes in " <> T.unpack (etyName ety))

-- | The morpheme filling a given role, or a descriptive failure.
morphemeWithRole ∷ EtyRole → Etymology → EtyMorpheme
morphemeWithRole role ety = case [ m | m ← etyMorphemes ety
                                 , emRole m ≡ role ] of
    (m : _) → m
    []      → error ("no " <> T.unpack (etyRoleText role) <> " morpheme in "
                     <> T.unpack (etyName ety))

-- | The morpheme for a given concept, or a descriptive failure.
morphemeFor ∷ ConceptId → Etymology → EtyMorpheme
morphemeFor cid ety = case [ m | m ← etyMorphemes ety, emConcept m ≡ cid ] of
    (m : _) → m
    []      → error ("no morpheme for " <> T.unpack (conceptIdText cid))

-- The five #709 forms, over concepts the shipped catalogue authors every
-- lexical form for.
bareE, modE, ofSingE, ofPlurE, possE ∷ NameExpr
bareE   = Bare (ConceptId "LAND")
modE    = Modifier (ConceptId "ASH") (ConceptId "LAND")
ofSingE = Of (ConceptId "EYE") Singular (ConceptId "STORM")
ofPlurE = Of (ConceptId "EYE") Plural (ConceptId "STORM")
possE   = Possessive (ConceptId "WOLF") (ConceptId "HEART")

allForms ∷ [(String, NameExpr)]
allForms =
    [ ("Bare", bareE), ("Modifier", modE), ("Of singular", ofSingE)
    , ("Of plural", ofPlurE), ("Possessive", possE) ]

-- | A profile with an ordering switch forced, so BOTH values of each
--   independent order are covered rather than whichever a seed happened
--   to draw ('profCompoundOrder' governs Modifier and Of alike;
--   'pmOrder' is a separate switch for the genitive).
withCompoundOrder ∷ CompoundOrder → Profile → Profile
withCompoundOrder o p = p { profCompoundOrder = o }

withGenitiveOrder ∷ GenitiveOrder → Profile → Profile
withGenitiveOrder o p =
    p { profPossessive = (profPossessive p) { pmOrder = o } }

-- | The realized form a DEPENDENT slot takes for a concept: its bound
--   form when the language has one, else its free root. Mirrors
--   'Language.Generated.Render''s own dependent-slot selection, so an
--   ordering assertion below is testing ORDER rather than accidentally
--   re-testing bound-form selection.
dependentForm ∷ LanguageRoots → ConceptId → Text
dependentForm roots cid = fromMaybe
    (fromMaybe "" (M.lookup cid (lrFree roots)))
    (M.lookup cid (lrBound roots))

-- | A concept the shipped catalogue does not carry — requirement 7's
--   invalid-concept case.
ghostE ∷ NameExpr
ghostE = Modifier (ConceptId "ASH") (ConceptId "NOT_A_REAL_CONCEPT")

-- | The one page every example here lives on. These specs cover the
--   WITHIN-page eligibility rules, so a single page is the whole world
--   as far as they are concerned; the cross-page target\/recurrence
--   split (#1265) is 'Test.Headless.Language.EtymologyPageScope', which
--   drives the registered Lua query against two live pages.
purePage ∷ WorldPageId
purePage = WorldPageId "pure_page"

-- | An entity carrying a real generated name.
entityFor
    ∷ Catalogue → Text → Maybe Int → LanguageProvenance → NameExpr
    → EtyEntity
entityFor cat kind ref prov expr =
    let (name, gloss) = storedFor cat prov expr
    in EtyEntity { eePage = purePage, eeKind = kind, eeRef = ref
                 , eeName = name, eeGloss = gloss
                 , eeSource = Just (sourceFor prov expr) }

spec ∷ Spec
spec = beforeAll loadRealCatalogue $ do

    describe "canonical decomposition" $ do
        forM_ allForms $ \(nm, expr) →
            it ("decomposes " <> nm <> ", echoing the stored name and \
                \whole gloss unchanged") $ \cat → do
                let (storedName, storedGloss) = storedFor cat provA expr
                    ety = available (explain cat provA expr)
                etyName ety  `shouldBe` storedName
                etyGloss ety `shouldBe` storedGloss
                etyLanguage ety `shouldBe` provA

        forM_ allForms $ \(nm, expr) →
            it ("concatenating " <> nm <> "'s surface tokens reproduces \
                \the stored name EXACTLY -- capitalization, repaired \
                \boundaries, separators, and grammatical markers \
                \included") $ \cat → do
                let (storedName, _) = storedFor cat provA expr
                    ety = available (explain cat provA expr)
                T.concat (map etyTokenText (etyTokens ety))
                    `shouldBe` storedName

        it "reports each expression's own form" $ \cat →
            map (etyFormText ∘ etyForm ∘ available ∘ explain cat provA ∘ snd)
                allForms
                `shouldBe` ["bare", "modifier", "of", "of-plural", "possessive"]

        it "attaches the semantic role to the CONCEPT, so every slot is \
           \reported by what it means rather than where it landed" $ \cat → do
            let roleOf expr =
                    [ (conceptIdText (emConcept m), etyRoleText (emRole m))
                    | m ← etyMorphemes (available (explain cat provA expr)) ]
            -- Order below is SURFACE order, which the profile chooses;
            -- the pairing of concept to role is what must hold.
            M.fromList (roleOf modE) `shouldBe`
                M.fromList [("ASH", "modifier"), ("LAND", "head")]
            M.fromList (roleOf ofSingE) `shouldBe`
                M.fromList [("EYE", "head"), ("STORM", "complement")]
            M.fromList (roleOf possE) `shouldBe`
                M.fromList [("WOLF", "owner"), ("HEART", "head")]

        it "reports each morpheme's English lemma through the SAME \
           \authored form its own gloss slot reads, so a reading can \
           \never contradict the whole gloss" $ \cat → do
            let lemmas expr = M.fromList
                    [ (conceptIdText (emConcept m), emLemma m)
                    | m ← etyMorphemes (available (explain cat provA expr)) ]
                formText cid kind = case lookupConcept (ConceptId cid) cat of
                    Just ce → fromMaybe "" (formOf kind ce)
                    Nothing → ""
            lemmas modE `shouldBe` M.fromList
                [ ("ASH", formText "ASH" FormModifier)
                , ("LAND", formText "LAND" FormSingular) ]
            lemmas ofPlurE `shouldBe` M.fromList
                [ ("EYE", formText "EYE" FormSingular)
                , ("STORM", formText "STORM" FormPlural) ]
            lemmas possE `shouldBe` M.fromList
                [ ("WOLF", formText "WOLF" FormPossessive)
                , ("HEART", formText "HEART" FormSingular) ]

        it "reports the grammatical marking on the slot that carries it, \
           \and on no other" $ \cat → do
            let marksOf expr =
                    [ (conceptIdText (emConcept m), etyMarkText <$> emMark m)
                    | m ← etyMorphemes (available (explain cat provA expr)) ]
            -- Singular Of marks nothing at all.
            M.fromList (marksOf ofSingE) `shouldBe`
                M.fromList [("EYE", Nothing), ("STORM", Nothing)]
            M.fromList (marksOf ofPlurE) `shouldBe`
                M.fromList [("EYE", Nothing), ("STORM", Just "plural")]
            M.fromList (marksOf possE) `shouldBe`
                M.fromList [("WOLF", Just "possessive"), ("HEART", Nothing)]

        it "a marked slot also reports the affix as REALIZED, and the \
           \mark's own token carries the same text" $ \cat → do
            let ety = available (explain cat provA possE)
                owner = morphemeWithRole RoleOwner ety
                markToks = [ t | t@(TokenMark _ _) ← etyTokens ety ]
            emMarkSurface owner `shouldSatisfy` isJust
            map etyTokenText markToks `shouldBe`
                [fromMaybe "" (emMarkSurface owner)]

        it "treats capitalization as a surface POSITION effect: only the \
           \leading token carries the name's capital, and every \
           \canonical free spelling stays the unmarked lowercase root" $
            \cat → forM_ allForms $ \(_, expr) → do
                let ety  = available (explain cat provA expr)
                    toks = filter (not ∘ T.null ∘ etyTokenText)
                                  (etyTokens ety)
                    free = lrFree (rootsFor cat provA)
                case toks of
                    [] → expectationFailure "no surface tokens"
                    (t : rest) → do
                        -- The stored name begins with the leading
                        -- token's own text, capital and all.
                        etyTokenText t `shouldSatisfy`
                            (`T.isPrefixOf` etyName ety)
                        -- Nothing AFTER the leading token was
                        -- capitalized by the renderer: roots, affixes,
                        -- inserted linkers, and separators all stay as
                        -- the lexicon and the join produced them.
                        forM_ rest $ \r →
                            etyTokenText r `shouldBe`
                                T.toLower (etyTokenText r)
                -- Free spellings are the language's own roots, which are
                -- never capitalized.
                forM_ (etyMorphemes ety) $ \m → do
                    Just (emFree m) `shouldBe` M.lookup (emConcept m) free
                    emFree m `shouldBe` T.toLower (emFree m)

        it "reports every morpheme's canonical FREE spelling, which is \
           \the language's own root for that concept -- never the \
           \capitalized surface form" $ \cat → do
            let ety   = available (explain cat provA modE)
                free  = lrFree (rootsFor cat provA)
            forM_ (etyMorphemes ety) $ \m →
                Just (emFree m) `shouldBe` M.lookup (emConcept m) free

    describe "surface ordering" $ do
        forM_ [ModifierFirst, HeadFirst] $ \o →
            it ("covers profCompoundOrder = " <> show o
                <> " for BOTH compound forms") $ \cat → do
                let prof  = withCompoundOrder o (profileFor provA)
                    roots = expectRoots (assignLanguageRoots prof
                                            (catOrdinals cat) (conceptIds cat))
                    surfaceOf expr = case renderNative prof roots expr of
                        Right t → t
                        Left e  → error (show e)
                    dep    = dependentForm roots (ConceptId "ASH")
                -- The dependent slot leads exactly when the profile says
                -- ModifierFirst, for Modifier and Of alike (they share
                -- the switch).
                (dep `T.isPrefixOf` T.toLower (surfaceOf modE))
                    `shouldBe` (o ≡ ModifierFirst)

        forM_ [OwnerFirst, HeadFirstGenitive] $ \o →
            it ("covers the INDEPENDENT genitive pmOrder = " <> show o) $
                \cat → do
                let prof  = withGenitiveOrder o (profileFor provA)
                    roots = expectRoots (assignLanguageRoots prof
                                            (catOrdinals cat) (conceptIds cat))
                    surface = case renderNative prof roots possE of
                        Right t → t
                        Left e  → error (show e)
                    owner = dependentForm roots (ConceptId "WOLF")
                (owner `T.isPrefixOf` T.toLower surface)
                    `shouldBe` (o ≡ OwnerFirst)

    describe "morpheme identity" $ do
        it "a BOUND realized form and its FREE root are one morpheme -- \
           \same identity, different spelling" $ \cat → do
            -- Find a concept this language really does form a bound form
            -- for, so the case is exercised rather than skipped.
            let prof  = profileFor provA
                roots = rootsFor cat provA
                bound = assignBoundForms prof (lrFree roots)
            case M.toList bound of
                [] → pendingWith "this language formed no bound forms"
                ((cid, boundText) : _) → do
                    -- The same concept in a DEPENDENT slot (bound) and in
                    -- a HEAD slot (free).
                    let asDependent = Modifier cid (ConceptId "LAND")
                        asHead      = Bare cid
                        depOwn = morphemeFor cid
                                     (available (explain cat provA asDependent))
                        hdM  = firstMorpheme
                                   (available (explain cat provA asHead))
                    -- One morpheme, two spellings.
                    emIdentity depOwn `shouldBe` emIdentity hdM
                    emBound depOwn `shouldBe` True
                    -- Up to word-initial capitalization: this language
                    -- writes the dependent slot FIRST, so the bound form
                    -- lands in leading position and carries the name's
                    -- capital. That is a surface-POSITION effect, never
                    -- part of the lexical form (see the dedicated case
                    -- above), which is why the comparison is on letters.
                    T.toLower (emSurface depOwn) `shouldBe` T.toLower boundText
                    -- The canonical free spelling stays the unmarked root.
                    emFree depOwn `shouldBe`
                        fromMaybe "" (M.lookup cid (lrFree roots))
                    -- A head slot is always the free form.
                    emBound hdM `shouldBe` False

        it "the SAME concept in two different generated languages does \
           \NOT share identity, however its spellings compare" $ \cat → do
            let a = firstMorpheme (available (explain cat provA bareE))
                b = firstMorpheme (available (explain cat provB bareE))
            emConcept a `shouldBe` emConcept b
            emIdentity a `shouldNotBe` emIdentity b

        it "the same seed under a DIFFERENT generator version is a \
           \different language, so its morphemes do not link either -- \
           \the direct consequence of keying identity on the whole \
           \provenance" $ \cat → do
            let a   = firstMorpheme (available (explain cat provA bareE))
                old = firstMorpheme (available (explain cat provAOld bareE))
            miLanguage (emIdentity a) `shouldNotBe` miLanguage (emIdentity old)
            emIdentity a `shouldNotBe` emIdentity old

        it "two DIFFERENT concepts never share identity, even when their \
           \realized spellings are identical" $ \cat → do
            let a = firstMorpheme (available (explain cat provA bareE))
                b = firstMorpheme (available (explain cat provA
                                       (Bare (ConceptId "STORM"))))
            -- Forced equality of spelling: identity must still differ.
            emIdentity a `shouldNotBe` emIdentity b
            MorphemeIdentity provA (ConceptId "LAND")
                `shouldNotBe` MorphemeIdentity provA (ConceptId "STORM")

        it "identity text carries the seed as DECIMAL TEXT and the \
           \version, so a Word64 seed survives the boundary intact" $
            \cat → do
                let m = firstMorpheme (available (explain cat provA bareE))
                morphemeIdentityText (emIdentity m) `shouldBe`
                    T.intercalate ":"
                        [ "6840123409045651617"  -- 0x5EED0000000000A1
                        , T.pack (show (generatorVersionInt
                                          (lpVersion provA)))
                        , "LAND" ]

    describe "historical provenance" $
        it "rebuilds the generator version the name RECORDS, not the \
           \current one -- so a name written by an older generator is \
           \still explained, and by its own language" $ \cat → do
            let oldEty = available (explain cat provAOld bareE)
                newEty = available (explain cat provA bareE)
            etyLanguage oldEty `shouldBe` provAOld
            lpVersion (etyLanguage oldEty)
                `shouldNotBe` lpVersion (etyLanguage newEty)
            -- Two versions of one seed really are different languages.
            etyName oldEty `shouldNotBe` etyName newEty

    describe "degrading honestly" $ do
        it "a name with NO etymology source is unavailable, not guessed" $
            \cat →
                unavailableOf (decomposeName cat "Whatever" Nothing Nothing)
                    `shouldBe` Just EtyNoSource

        it "an unconstructible generator version reports itself, naming \
           \the version" $ \cat → do
            let ghostVer = LanguageProvenance (LangSeed 1) (GeneratorVersion 99)
            unavailableOf (decomposeName cat "Whatever" Nothing
                              (Just (sourceFor ghostVer bareE)))
                `shouldBe` Just (EtyUnsupportedVersion 99)

        -- #2206. Profile generation SUCCEEDS for this seed; root
        -- assignment is what fails. The result is the existing
        -- @reconstruction_failed@ wire reason carrying the generator's
        -- own text, not a new reason the Lua/API boundary would have to
        -- learn.
        it "a language whose root space cannot name the catalogue \
           \reports reconstruction_failed, carrying the generator's \
           \own text" $ \cat → do
            -- The page provenance MATCHES the source's, so
            -- decomposeEntityName's foreignness check cannot answer
            -- first and make this pass without ever reaching the
            -- assignment.
            let src = sourceFor provShort bareE
            case decomposeEntityName cat (Just provShort)
                     "Whatever" Nothing (Just src) of
                EtyUnavailable u@(EtyReconstructionFailed why) → do
                    etyUnavailableReason u `shouldBe` "reconstruction_failed"
                    forM_ ["version 5", "144", "151", "shortfall 7"] $
                        \needle → why `shouldSatisfy` T.isInfixOf needle
                other → expectationFailure
                    ("expected reconstruction_failed, got " <> show other)

        -- Not vacuous: the same fixture under a language that CAN name
        -- the catalogue reaches a different outcome entirely, so the
        -- case above is measuring the root space rather than the
        -- placeholder name.
        it "reaches a different outcome for a language that can name the \
           \catalogue" $ \cat →
            unavailableOf (decomposeEntityName cat (Just provA)
                              "Whatever" Nothing (Just (sourceFor provA bareE)))
                `shouldSatisfy` (\u → u ≢ Nothing
                                     ∧ fmap etyUnavailableReason u
                                         ≢ Just "reconstruction_failed")

        it "a referenced concept the catalogue no longer carries reports \
           \itself, naming the concept" $ \cat →
            unavailableOf (decomposeName cat "Whatever" Nothing
                              (Just (sourceFor provA ghostE)))
                `shouldBe` Just (EtyInvalidConcept
                                    (ConceptId "NOT_A_REAL_CONCEPT"))

        it "a reconstruction that does NOT reproduce the stored name is \
           \refused -- the explanation would have described some other \
           \word" $ \cat → do
            let (real, gloss) = storedFor cat provA modE
                tampered = real <> "x"
            case decomposeName cat tampered gloss
                     (Just (sourceFor provA modE)) of
                EtyUnavailable (EtySurfaceMismatch stored rebuilt) → do
                    stored  `shouldBe` tampered
                    rebuilt `shouldBe` real
                other → expectationFailure
                    ("expected a surface mismatch, got " <> show other)

        it "an expression rendered under the WRONG language is refused by \
           \the same check -- a source that does not belong to the name \
           \it sits beside can never explain it" $ \cat → do
            let (nameA, glossA) = storedFor cat provA modE
            -- Same expression, but claiming language B rendered it.
            case decomposeName cat nameA glossA
                     (Just (sourceFor provB modE)) of
                EtyUnavailable EtySurfaceMismatch{} → pure ()
                other → expectationFailure
                    ("expected a surface mismatch, got " <> show other)

        it "every unavailable reason has a stable wire key and a \
           \non-empty player-facing sentence -- nothing reports a blank \
           \or a fabricated cause" $ \_cat → do
            let reasons =
                    [ EtyCustomName, EtyNoSource, EtyNoProvenance
                    , EtyForeignSource
                    , EtyUnsupportedVersion 99
                    , EtyInvalidConcept (ConceptId "X")
                    , EtyReconstructionFailed "why"
                    , EtySurfaceMismatch "a" "b" ]
            map etyUnavailableReason reasons `shouldBe`
                [ "custom", "no_source", "no_provenance", "foreign_source"
                , "unsupported_version", "invalid_concept"
                , "reconstruction_failed", "surface_mismatch" ]
            map etyUnavailableText reasons
                `shouldSatisfy` all (not ∘ T.null)

    -- The surface check proves the expression renders to the stored text
    -- UNDER ITS OWN LANGUAGE. It cannot notice that the language is the
    -- wrong one for the page, so the page's provenance is checked too.
    describe "a source must belong to the page's own language" $ do
        it "refuses a source whose language differs from the page's, even \
           \though that source reconstructs the stored name perfectly" $
            \cat → do
                let (nameB, glossB) = storedFor cat provB modE
                    srcB = sourceFor provB modE
                -- On its own the source is entirely valid...
                decomposeName cat nameB glossB (Just srcB)
                    `shouldSatisfy` isAvailable
                -- ...but the page says this world speaks language A, so
                -- an explanation drawn from B would attribute every
                -- morpheme — and every recurrence link — to a language
                -- this world does not have.
                unavailableOf (decomposeEntityName cat (Just provA)
                                  nameB glossB (Just srcB))
                    `shouldBe` Just EtyForeignSource

        it "refuses a source on a page that records NO language at all -- \
           \there is nothing for it to agree with, and absence is never \
           \repaired by inference" $ \cat → do
            let (name, gloss) = storedFor cat provA modE
            unavailableOf (decomposeEntityName cat Nothing name gloss
                              (Just (sourceFor provA modE)))
                `shouldBe` Just EtyForeignSource

        it "refuses a source whose seed matches but whose GENERATOR \
           \VERSION does not -- two versions of one seed are two \
           \languages, so a version drift is as much a mismatch as a \
           \seed one" $ \cat → do
            let (name, gloss) = storedFor cat provAOld modE
            unavailableOf (decomposeEntityName cat (Just provA) name gloss
                              (Just (sourceFor provAOld modE)))
                `shouldBe` Just EtyForeignSource

        it "accepts a source that DOES belong to the page's language, so \
           \the check rejects mismatches rather than everything" $
            \cat → do
                let (name, gloss) = storedFor cat provA modE
                decomposeEntityName cat (Just provA) name gloss
                    (Just (sourceFor provA modE))
                    `shouldSatisfy` isAvailable

        it "still reports the ordinary absences when there is no source \
           \to check, whatever the page records" $ \cat → do
            unavailableOf (decomposeEntityName cat (Just provA)
                              "Whatever" Nothing Nothing)
                `shouldBe` Just EtyNoSource
            unavailableOf (decomposeEntityName cat Nothing
                              "Whatever" Nothing Nothing)
                `shouldBe` Just EtyNoSource

        it "sourceMatchesPage is exact on the whole provenance" $ \_cat → do
            sourceMatchesPage (Just provA) (sourceFor provA modE)
                `shouldBe` True
            sourceMatchesPage (Just provA) (sourceFor provB modE)
                `shouldBe` False
            sourceMatchesPage (Just provA) (sourceFor provAOld modE)
                `shouldBe` False
            sourceMatchesPage Nothing (sourceFor provA modE)
                `shouldBe` False

    describe "the query is read-only" $
        it "decomposing leaves the stored name, gloss, source, and \
           \provenance byte-for-byte unchanged" $ \cat → do
            let (name, gloss) = storedFor cat provA modE
                src = sourceFor provA modE
                before = (name, gloss, src, esLanguage src)
            _ ← pure (decomposeName cat name gloss (Just src))
            _ ← pure (decomposeName cat name gloss (Just src))
            (name, gloss, src, esLanguage src) `shouldBe` before

    describe "the expression's transport encoding" $ do
        forM_ allForms $ \(nm, expr) →
            it ("round-trips " <> nm <> " through the text token \
                \world.init and world.suggestName exchange") $ \_cat →
                decodeNameExpr (encodeNameExpr expr) `shouldBe` Just expr

        it "refuses anything that is not one of the four shapes, rather \
           \than approximating it" $ \_cat →
            map decodeNameExpr
                [ "", "Bare", "Bare:", "Modifier:ASH"
                , "Of:EYE:many:STORM", "Nonsense:A:B", "Bare:LAND:EXTRA" ]
                `shouldBe` replicate 7 Nothing

    describe "recurrence policy" $ do
        it "the current world and a DISCOVERED location both participate, \
           \while an UNDISCOVERED location does not" $ \cat → do
            -- All three names share the ASH modifier, so only eligibility
            -- decides which of them can appear.
            let worldE = entityFor cat "world" Nothing provA modE
                found  = entityFor cat "location" (Just 1) provA
                             (Modifier (ConceptId "ASH") (ConceptId "KEEP"))
                hidden = entityFor cat "location" (Just 2) provA
                             (Modifier (ConceptId "ASH") (ConceptId "GATE"))
                eligible = eligibleEntities purePage
                    (Just (identityOf worldE))
                    [ instanceOf found LifecycleDiscovered
                    , instanceOf hidden LifecycleUnknown ]
                    Nothing
                ety = available (explain cat provA modE)
                links = recurrenceFor cat worldE eligible ety
                names = concatMap (map eeName ∘ snd) links
            eeName found `shouldSatisfy` (`elem` names)
            eeName hidden `shouldNotSatisfy` (`elem` names)

        it "a recurrence entry exposes the entity's KIND and its already \
           \visible stored name, and nothing else" $ \cat → do
            let worldE = entityFor cat "world" Nothing provA modE
                found  = entityFor cat "location" (Just 1) provA
                             (Modifier (ConceptId "ASH") (ConceptId "KEEP"))
                eligible = eligibleEntities purePage (Just (identityOf worldE))
                               [instanceOf found LifecycleDiscovered] Nothing
                ety   = available (explain cat provA modE)
                links = recurrenceFor cat worldE eligible ety
                entries = concatMap snd links
            map eeKind entries `shouldSatisfy` all (≡ "location")
            map eeName entries `shouldBe` [eeName found]

        it "the inspected entity never appears in its OWN recurrence" $
            \cat → do
                let worldE = entityFor cat "world" Nothing provA modE
                    eligible = eligibleEntities purePage
                                   (Just (identityOf worldE)) [] Nothing
                    ety   = available (explain cat provA modE)
                    links = recurrenceFor cat worldE eligible ety
                concatMap snd links `shouldBe` []

        it "when a WORLD or a LOCATION is inspected, no river \
           \participates at all" $ \cat → do
            let worldE = entityFor cat "world" Nothing provA modE
                riverE = entityFor cat "river" (Just 7) provA
                             (Modifier (ConceptId "ASH") (ConceptId "RIVER"))
                -- The world/location adapters pass Nothing for the river
                -- slot; this is that call.
                eligible = eligibleEntities purePage
                               (Just (identityOf worldE)) [] Nothing
            map eeKind eligible `shouldSatisfy` all (≢ "river")
            eeName riverE `shouldSatisfy` (not ∘ T.null)

        it "when a RIVER is inspected, exactly that river is admitted -- \
           \and swapping to another admits the new one and drops the \
           \first, with no history kept between the two calls" $ \cat → do
            let riverA = entityFor cat "river" (Just 7) provA
                             (Modifier (ConceptId "ASH") (ConceptId "RIVER"))
                riverB = entityFor cat "river" (Just 8) provA
                             (Modifier (ConceptId "IRON") (ConceptId "RIVER"))
                admit r = [ eeRef e | e ← eligibleEntities purePage Nothing []
                                          (Just (GeoFeatureId (fromMaybe 0
                                                     (eeRef r)), riverNameOf r))
                          , eeKind e ≡ "river" ]
            admit riverA `shouldBe` [Just 7]
            admit riverB `shouldBe` [Just 8]

        it "matches on IDENTITY, not spelling: an identical-looking name \
           \from another language contributes nothing" $ \cat → do
            let worldE  = entityFor cat "world" Nothing provA modE
                -- Same expression, DIFFERENT language: its own rendering,
                -- and its own morpheme identities.
                foreign_ = entityFor cat "location" (Just 1) provB modE
                eligible = eligibleEntities purePage (Just (identityOf worldE))
                               [instanceOf foreign_ LifecycleDiscovered]
                               Nothing
                ety   = available (explain cat provA modE)
                links = recurrenceFor cat worldE eligible ety
            concatMap snd links `shouldBe` []

        it "only a name whose etymology VALIDATES participates -- one \
           \with no source is silently absent rather than matched on its \
           \raw text" $ \cat → do
            let worldE = entityFor cat "world" Nothing provA modE
                (nm, gl) = storedFor cat provA
                               (Modifier (ConceptId "ASH") (ConceptId "KEEP"))
                sourceless = EtyEntity purePage "location" (Just 1) nm gl
                                       Nothing
                eligible = eligibleEntities purePage (Just (identityOf worldE))
                               [instanceOf sourceless LifecycleDiscovered]
                               Nothing
                ety   = available (explain cat provA modE)
                links = recurrenceFor cat worldE eligible ety
            concatMap snd links `shouldBe` []

    -- #1104 requirement 10's river entry point is only reachable if a
    -- visible river segment resolves to its own identity. The world
    -- WRAPS on the u axis, so this has to be measured the way the carve
    -- measures it — a raw coordinate delta puts a seam-crossing river a
    -- whole world away from its own water.
    describe "river selection resolves through the wrapped axis" $ do
        it "resolves a tile beside an ordinary, non-seam-crossing river" $
            \_cat →
                riverAtTile seamWorldSize (timelineWith [innerRiver])
                            innerTileOnChannel innerTileY
                    `shouldBe` Just (GeoFeatureId 1)

        it "resolves a tile beside a river whose segment CROSSES the wrap \
           \seam -- the raw-delta reading would report no river at all" $
            \_cat →
                riverAtTile seamWorldSize (timelineWith [seamRiver])
                            seamTileX seamTileY
                    `shouldBe` Just (GeoFeatureId 2)

        it "does not claim a tile that is genuinely far from every \
           \channel, so the wrapped reading has not simply widened the \
           \match" $ \_cat →
            riverAtTile seamWorldSize (timelineWith [innerRiver, seamRiver])
                        0 0
                `shouldBe` Nothing

        it "prefers the NEARER river when two are in range, and reports \
           \no id for a river the timeline cannot identify" $ \_cat → do
            riverAtTile seamWorldSize (timelineWith [innerRiver, seamRiver])
                        innerTileOnChannel innerTileY
                `shouldBe` Just (GeoFeatureId 1)
            -- A timeline whose events cannot be paired with its features
            -- yields no ids at all, so nothing resolves rather than
            -- something wrong.
            riverAtTile seamWorldSize (unpairedTimeline [innerRiver])
                        innerTileOnChannel innerTileY
                `shouldBe` Nothing

-- * Fixture plumbing --------------------------------------------------

loadRealCatalogue ∷ IO Catalogue
loadRealCatalogue = do
    loaded ← loadCatalogue conceptCataloguePath conceptOrdinalPath
    case loaded of
        Right cat → pure cat
        Left err  → error ("test setup: catalogue: " <> show err)

-- | The stored identity an entity stands for, so 'eligibleEntities' is
--   fed the same record shape the engine feeds it.
identityOf ∷ EtyEntity → WorldIdentity
identityOf e = WorldIdentity
    { wiName = eeName e, wiGloss = eeGloss e
    , wiLanguage = esLanguage <$> eeSource e
    , wiEtymology = eeSource e }

instanceOf ∷ EtyEntity → LocationLifecycle → LocationInstance
instanceOf e lc = LocationInstance
    { liId              = LocationInstanceId (fromMaybe 0 (eeRef e))
    , liDefId           = "ruin_small"
    , liChunk           = ChunkCoord 0 0
    , liAnchor          = (0, 0)
    , liBounds          = AbsBounds 0 0 1 1
    , liDisplayName     = eeName e
    , liGloss           = eeGloss e
    , liEtymology       = eeSource e
    , liLifecycle       = lc
    , liContentsSpawned = False
    , liEncounter       = Nothing
    , liSignificant     = []
    , liClearEventEmitted = False
    }

riverNameOf ∷ EtyEntity → RiverName
riverNameOf e = RiverName
    { rvnDisplayName = eeName e, rvnGloss = eeGloss e
    , rvnEtymology = eeSource e }

-- * River-selection fixtures ------------------------------------------

-- | A small world, so the wrap seam is close enough to build a
--   deliberately seam-crossing segment against.
seamWorldSize ∷ Int
seamWorldSize = 8

-- | Half the world's U extent — where 'wrappedDeltaUV' folds. ONLY the
--   u axis (@gx - gy@) wraps; the world is a cylinder, not a torus, so a
--   fixture that put its endpoints far apart in v as well would not be
--   crossing the seam at all — it would just be a long river.
seamHalf ∷ Int
seamHalf = seamWorldSize * chunkSize `div` 2

-- | A tile from its isometric @(u, v)@ coordinates, so a seam-crossing
--   fixture can be stated in the axis that actually wraps rather than
--   in x\/y where the intent is invisible.
fromUV ∷ Int → Int → GeoCoord
fromUV u v = GeoCoord ((u + v) `div` 2) ((v - u) `div` 2)

-- | An ordinary river well inside the world, running along +u.
innerRiver ∷ (Int, RiverParams)
innerRiver = (1, riverFrom (GeoCoord 40 40) (GeoCoord 60 60))

innerTileOnChannel, innerTileY ∷ Int
innerTileOnChannel = 50
innerTileY = 50

-- | A river whose two endpoints sit on OPPOSITE sides of the u wrap
--   seam at the same v: raw, their u values are nearly a world apart;
--   wrapped, they are 8 tiles apart. A tile just inside the seam is on
--   its channel.
--
--   This is the case that discriminates the two readings. Wrapped, the
--   query tile sits a quarter of the way along a short segment. Raw, the
--   segment is ~175 tiles long pointing the other way, the projection
--   comes out NEGATIVE, and the tile resolves to no river at all — which
--   is exactly what a player selecting a seam-crossing river would have
--   got.
seamRiver ∷ (Int, RiverParams)
seamRiver = (2, riverFrom (fromUV (seamHalf - 4) 0)
                          (fromUV (negate seamHalf + 4) 0))

seamTileX, seamTileY ∷ Int
GeoCoord seamTileX seamTileY = fromUV (seamHalf - 2) 0

riverFrom ∷ GeoCoord → GeoCoord → RiverParams
riverFrom start end = RiverParams
    { rpSourceRegion = start
    , rpMouthRegion  = end
    , rpFlowRate     = 1.0
    , rpSegments     = V.singleton RiverSegment
        { rsStart = start, rsEnd = end
        , rsWidth = 6, rsValleyWidth = 12, rsDepth = 2
        , rsFlowRate = 1.0, rsStartElev = 100, rsEndElev = 90
        }
    }

-- | A timeline whose river EVENTS and river FEATURES agree, so
--   "World.River.Identity" can pair them and every river has an id.
timelineWith ∷ [(Int, RiverParams)] → GeoTimeline
timelineWith rs = emptyTimeline
    { gtFeatures = [ riverFeatureOf fid rp | (fid, rp) ← rs ]
    , gtPeriods  = [ periodWith [ HydroEvent (RiverFeature rp) | (_, rp) ← rs ] ]
    }

-- | The same rivers, but with the features' stored params DISAGREEING
--   with the emitted events, so the pairing check fails and no river is
--   identifiable — the case that must yield no id rather than a wrong one.
unpairedTimeline ∷ [(Int, RiverParams)] → GeoTimeline
unpairedTimeline rs = (timelineWith rs)
    { gtFeatures = [ riverFeatureOf fid rp { rpFlowRate = rpFlowRate rp + 99 }
                   | (fid, rp) ← rs ] }

riverFeatureOf ∷ Int → RiverParams → PersistentFeature
riverFeatureOf fid rp = PersistentFeature
    { pfId               = GeoFeatureId fid
    , pfFeature          = HydroShape (RiverFeature rp)
    , pfActivity         = FActive
    , pfFormationPeriod  = 0
    , pfLastActivePeriod = 0
    , pfEruptionCount    = 0
    , pfParentId         = Nothing
    }

periodWith ∷ [GeoEvent] → GeoPeriod
periodWith evs = GeoPeriod
    { gpName = "age", gpScale = Age, gpDuration = 1, gpDate = 0
    , gpEvents = evs
    , gpErosion = defaultErosionParams
    , gpRegionalErosion = HM.empty
    , gpTaggedEvents = []
    , gpExplodedEvents = V.empty
    , gpPeriodBBox = noBBox
    }
