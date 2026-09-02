-- | Language-derived world-name suggestions (#1106): the producer the
--   Create World dice button calls. Everything here is pure and
--   engine-free — the production catalogue is read straight from
--   @data/language/concepts.yaml@ (tests run from the repo root), and
--   the suggester is driven exactly as the Lua bridge drives it.
--
--   What these pin is the behavior the UI cannot check for itself: the
--   sequence is deterministic, a reroll changes the meaning without
--   changing the language, and a different seed is a different
--   language. The Lua-side half of the contract — which names carry
--   provenance, and what a manual edit erases — is in
--   "Test.Headless.UI.CreateWorldControls".
module Test.Headless.Language.Suggest (spec) where

import UPrelude
import Test.Hspec
import Data.Either (isRight)
import Data.List (nub, sort)
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.Catalogue
import Language.Semantic.English (renderGloss)
import Language.Generated.Types
import Language.Generated.Profile (generateProfile)
import Language.Generated.Signature (profileSignature)
import Language.Generated.Render (renderNative)
import Language.Generated.Orthography (outputInventory)
import Language.Suggest
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LanguageCache(..), LuaBackendState(..))
import Engine.Scripting.Lua.API.World.Lifecycle
    ( suggestionStep, suggestionStepLabel, readCatalogueForSuggestions
    , maxSuggestNameOrdinal )
import Test.Headless.Harness (withHeadlessEngine)

-- | Provenance for one world seed at the current generator.
provFor ∷ Word64 → LanguageProvenance
provFor s = LanguageProvenance
    { lpSeed = worldLanguageSeed s, lpVersion = currentGeneratorVersion }

-- | A spread of world seeds standing in for player input: the corners
--   of the 8-hex-digit space Create World produces, plus an arbitrary
--   scatter through the middle.
sampleSeeds ∷ [Word64]
sampleSeeds = [0, 1, 2, 42, 1337, 0x00A3F7C9, 0x7FFFFFFF, 0xFFFFFFFF]
           ⧺ [ fromIntegral (i * 7919 `mod` 0xFFFFFFFF ∷ Int)
             | i ← [1 .. 24 ∷ Int] ]

suggesterFor ∷ Catalogue → Word64 → NameSuggester
suggesterFor cat s = case mkNameSuggester cat (provFor s) of
    Left err  → error (T.unpack (suggestErrorText err))
    Right sgr → sgr

suggestionsFor ∷ Catalogue → Word64 → Int → [NameSuggestion]
suggestionsFor cat s n =
    [ either (error ∘ T.unpack ∘ suggestErrorText) id (suggestNameAt sgr k)
    | k ← [0 .. n - 1] ]
  where sgr = suggesterFor cat s

firstSuggestion ∷ Catalogue → Word64 → NameSuggestion
firstSuggestion cat s =
    either (error ∘ T.unpack ∘ suggestErrorText) id
           (suggestNameAt (suggesterFor cat s) 0)

spec ∷ Spec
spec = describe "world-name suggestions" $ do
    prodCatE ← runIO $ loadCatalogue conceptCataloguePath conceptOrdinalPath
    let prodCat = either (error ∘ T.unpack ∘ catalogueErrorText) id prodCatE

    describe "language seed derived from the world seed" $ do
        -- Requirement 3, and the reviewer's injectivity clause: two
        -- different world seeds are two different languages, not merely
        -- probably-different ones. A collision would silently make two
        -- worlds share a language.
        it "is injective across the supported seed space" $ do
            let seeds = [0 .. 4095] ⧺ sampleSeeds
                           ⧺ [0xFFFFFF00 .. 0xFFFFFFFF] ∷ [Word64]
                langs = map (langSeedWord ∘ worldLanguageSeed) seeds
            length (nub langs) `shouldBe` length (nub seeds)

        -- Injectivity above and non-identity below both survive ANY
        -- relabelling of the mapping, so between them they still admit
        -- a derivation change that silently renames every existing
        -- world's language. This table is the only thing pinning the
        -- mapping ITSELF. Its right-hand side was computed
        -- independently from the construction 'worldLanguageSeed'
        -- documents — @fmix64 (seed `xor` 0x576C616E67536431)@,
        -- reimplemented outside Haskell — rather than captured from a
        -- run of the function under test (#1368).
        it "maps each world seed to one fixed language seed" $
            map (langSeedWord ∘ worldLanguageSeed)
                ([0, 1, 42, 1337, 0xFFFFFFFF] ∷ [Word64])
                `shouldBe`
                    [  3786218519592930629
                    ,  4113614416671679263
                    , 12594706351022957199
                    , 13288172387750703019
                    , 16325447759613160233 ]

        -- Equivalent SPELLINGS of a seed are normalized to one number
        -- before reaching here (settingsTab.seedNumber / generation.lua
        -- both use tonumber(text, 16)), so this layer only has to agree
        -- that one number is one language.
        it "is not the world seed itself" $
            filter (\s → langSeedWord (worldLanguageSeed s) ≡ s) sampleSeeds
                `shouldBe` []

    describe "determinism" $ do
        -- Everything else in this module checks the sequence against
        -- itself or against a property: the two path-agreement examples
        -- below, the reroll diversity, the phonology signature, the
        -- shape coverage, the length bounds and the cross-seed
        -- distinctness all still hold of a DIFFERENT but equally valid
        -- sequence. This is the one example that would notice one
        -- (#1368). Its expected side is literal on purpose — nothing in
        -- 'pinnedSeed42' is recomputed through 'suggestionsFor',
        -- 'suggestNameAt', 'worldLanguageSeed' or either renderer,
        -- because an expectation drawn from the code under test is the
        -- tautology this replaced.
        --
        -- Growing data/language/concepts.yaml moves the draws and so
        -- moves these four values. Re-capture them deliberately; do not
        -- soften the assertion to survive catalogue edits, which is the
        -- coverage it exists to give.
        it "offers one pinned sequence for a fixed world seed" $
            [ (nsExpr sug, nsName sug, nsGloss sug, nsSeed sug, nsVersion sug)
            | sug ← suggestionsFor prodCat 42 4 ]
                `shouldBe` pinnedSeed42

        it "renders both readings from one expression" $
            forM_ sampleSeeds $ \s → do
                let sgr = suggesterFor prodCat s
                forM_ [0 .. 7] $ \k → case suggestNameAt sgr k of
                    Left err → expectationFailure (T.unpack (suggestErrorText err))
                    Right sug → do
                        renderNative (nsuProfile sgr) (nsuRoots sgr) (nsExpr sug)
                            `shouldBe` Right (nsName sug)
                        renderGloss prodCat (nsExpr sug)
                            `shouldBe` Right (nsGloss sug)

        it "agrees with suggestionExprAt" $
            forM_ sampleSeeds $ \s → do
                let sgr = suggesterFor prodCat s
                forM_ [0 .. 7] $ \k →
                    either (const Nothing) (Just ∘ nsExpr) (suggestNameAt sgr k)
                        `shouldBe` suggestionExprAt sgr k

    describe "rerolling" $ do
        -- Requirement 2, the single most important behavior in the
        -- issue: press the dice repeatedly and the meaning changes
        -- while the language does not.
        it "changes the meaning on every adjacent reroll" $
            forM_ sampleSeeds $ \s → do
                let sugs = suggestionsFor prodCat s 40
                forM_ (zip sugs (drop 1 sugs)) $ \(a, b) → do
                    nsExpr a `shouldNotBe` nsExpr b
                    nsGloss a `shouldNotBe` nsGloss b

        it "keeps one language across the whole sequence" $
            forM_ sampleSeeds $ \s → do
                let sugs = suggestionsFor prodCat s 40
                    prov = provFor s
                map nsSeed sugs `shouldBe` map (const (lpSeed prov)) sugs
                map nsVersion sugs `shouldBe` map (const (lpVersion prov)) sugs

        -- The phonology is what a player HEARS staying put. Pinning the
        -- profile signature makes "same language" mean the whole style,
        -- not just a matching seed field.
        it "keeps one phonology across the whole sequence" $
            forM_ sampleSeeds $ \s → do
                let sgr = suggesterFor prodCat s
                case generateProfile currentGeneratorVersion
                                     (worldLanguageSeed s) of
                    Left err → expectationFailure
                        (T.unpack (generatorErrorText err))
                    Right prof → profileSignature (nsuProfile sgr)
                        `shouldBe` profileSignature prof

        it "reaches every name form over a long enough sequence" $ do
            let shapes = [ shapeOf (nsExpr sug)
                         | s ← sampleSeeds
                         , sug ← suggestionsFor prodCat s 40 ]
            sort (nub shapes) `shouldBe`
                ["bare", "modifier", "of_plural", "of_singular", "possessive"]

        it "never names a concept as its own modifier" $
            forM_ sampleSeeds $ \s →
                forM_ (suggestionsFor prodCat s 40) $ \sug →
                    slotsOf (nsExpr sug) `shouldSatisfy`
                        (\cs → length (nub cs) ≡ length cs)

    describe "different seeds" $ do
        it "give different languages" $ do
            let seeds = nub (take 24 sampleSeeds)
                langs = map (nsSeed ∘ firstSuggestion prodCat) seeds
            length (nub (map langSeedWord langs)) `shouldBe` length seeds

        -- Requirement 3's player-visible half: changing the seed offers
        -- a genuinely different name, not the same one respelled.
        it "give different first suggestions" $ do
            let firsts = map (nsName ∘ firstSuggestion prodCat)
                             (nub sampleSeeds)
            length (nub firsts) `shouldSatisfy` (≥ length firsts - 1)

    describe "rendered output" $ do
        it "stays inside the renderer's own length bounds" $
            forM_ sampleSeeds $ \s →
                forM_ (suggestionsFor prodCat s 20) $ \sug → do
                    T.length (nsName sug) `shouldSatisfy` (≥ 3)
                    T.length (nsName sug) `shouldSatisfy` (≤ 32)

        it "always supplies a nonempty gloss" $
            forM_ sampleSeeds $ \s →
                forM_ (suggestionsFor prodCat s 20) $ \sug →
                    nsGloss sug `shouldSatisfy` (not ∘ T.null)

        -- The World Name field admits exactly `outputInventory`
        -- (through world.generatedNameCharacters), so a suggestion that
        -- reached outside it would be a name the player could see but
        -- not retype (#1106 requirement 4). This is what ties the two
        -- ends of that claim together.
        it "uses only characters the name field admits" $ do
            let admitted = outputInventory
            forM_ sampleSeeds $ \s →
                forM_ (suggestionsFor prodCat s 20) $ \sug →
                    forM_ (T.unpack (nsName sug)) $ \c →
                        (nsName sug, c) `shouldSatisfy`
                            (\(_, ch) → ch `elem` admitted)

        -- Not vacuous: the shipped generator really does emit both a
        -- mark and a non-ASCII letter, which is why widening the field
        -- was necessary rather than cosmetic.
        it "reaches beyond plain ASCII letters in practice" $ do
            let names = [ nsName sug
                        | s ← sampleSeeds, sug ← suggestionsFor prodCat s 20 ]
                chars = concatMap T.unpack names
            filter (\c → c ≡ '\'' ∨ c ≡ '-') chars `shouldSatisfy` (not ∘ null)
            filter (> '\x007F') chars `shouldSatisfy` (not ∘ null)

    describe "failure reporting" $ do
        -- A version this build cannot construct is refused, never
        -- silently substituted with the current one — that would suggest
        -- names in a different language than the one being recorded.
        it "refuses an unconstructible generator version" $ do
            let bad = LanguageProvenance
                    { lpSeed = worldLanguageSeed 42
                    , lpVersion = GeneratorVersion 9999 }
            case mkNameSuggester prodCat bad of
                Right _  → expectationFailure "built a profile for version 9999"
                Left err → suggestErrorText err `shouldSatisfy`
                    T.isInfixOf "unsupported language-generator version"

        -- #2206. A profile this build CAN construct, whose root space
        -- still cannot give all 151 concepts a distinct root. Before
        -- the capacity gate this seed did not fail — it never returned,
        -- taking the shared Lua thread with it.
        it "refuses a world seed whose language has too small a root \
           \space" $ do
            case mkNameSuggester prodCat (provFor insufficientWorldSeed) of
                Right _  → expectationFailure
                    "suggested from a 137-root language"
                Left err → do
                    case err of
                        SuggestGenerator _ → pure ()
                        _ → expectationFailure
                            "expected the failure to arrive as SuggestGenerator"
                    let msg = suggestErrorText err
                    forM_ ["version 5", "137", "151", "shortfall 14"] $
                        \needle → msg `shouldSatisfy` T.isInfixOf needle

        -- Not vacuous: the adjacent world seed still suggests, so the
        -- refusal above is that language's own and not the path
        -- breaking for every seed.
        it "still suggests for the adjacent world seed" $
            case mkNameSuggester prodCat
                     (provFor (insufficientWorldSeed - 1)) of
                Right _  → pure ()
                Left err → expectationFailure
                    (T.unpack (suggestErrorText err))

        it "refuses a catalogue with no concepts" $ do
            let noOrdinals = either (error ∘ T.unpack ∘ catalogueErrorText)
                                    id (mkConceptOrdinals [])
                empty = Catalogue { catVersion = 1, catConcepts = mempty
                                  , catOrdinals = noOrdinals }
            case mkNameSuggester empty (provFor 42) of
                Right _  → expectationFailure "suggested from an empty catalogue"
                Left err → suggestErrorText err `shouldSatisfy`
                    T.isInfixOf "empty"

    -- Requirement 8: the dice button runs synchronously on the UI's own
    -- thread. Exactly one press per session may reach the filesystem —
    -- reading `data/language/concepts.yaml` on every press is what the
    -- cache exists to prevent, and a press that FAILS to read it is not
    -- exempt (review round 2).
    describe "the suggestion cache's step decision" $ do
        let step p c = suggestionStepLabel (suggestionStep p c)
            prov42   = provFor 42
            prov7    = provFor 7
            sgr42    = suggesterFor prodCat 42

        it "reads the catalogue only when nothing is cached" $
            step prov42 Nothing `shouldBe` "read"

        it "reuses a cached suggester for the same language" $
            step prov42 (Just (LanguageCache (Right prodCat)
                                             (Just (prov42, sgr42))))
                `shouldBe` "reuse"

        -- Editing the seed is a different language, so the suggester is
        -- rebuilt — but from the cached catalogue, never off disk.
        it "rebuilds from the cached catalogue for a different language" $
            step prov7 (Just (LanguageCache (Right prodCat)
                                            (Just (prov42, sgr42))))
                `shouldBe` "build"

        it "rebuilds without re-reading when no suggester was cached" $
            step prov42 (Just (LanguageCache (Right prodCat) Nothing))
                `shouldBe` "build"

        -- The round-2 blocker: a catalogue that would not load used to
        -- go uncached, so every later press retried the disk read.
        it "reports a cached catalogue failure without re-reading" $ do
            let broken = Just (LanguageCache (Left "no catalogue") Nothing)
            step prov42 broken `shouldBe` "failed"
            step prov7 broken `shouldBe` "failed"

        -- Round 3: caching that failure only helps if the read RETURNS
        -- one. loadCatalogue reports a file it parsed and rejected, but
        -- a missing or unreadable one throws out of BS.readFile — which
        -- would escape past the cache write and put the disk back on
        -- every press.
        it "turns an unreadable catalogue into a value, not an exception" $ do
            result ← readCatalogueForSuggestions
                        "data/language/does_not_exist.yaml" conceptOrdinalPath
            case result of
                Right _  → expectationFailure "read a nonexistent catalogue"
                Left msg → do
                    msg `shouldSatisfy` T.isInfixOf "does_not_exist.yaml"
                    msg `shouldSatisfy` T.isInfixOf "could not be loaded"

        it "reports a catalogue it could read but not validate" $ do
            result ← readCatalogueForSuggestions "data/units/acolyte.yaml"
                                                conceptOrdinalPath
            case result of
                Right _  → expectationFailure "validated a unit definition"
                Left msg → msg `shouldSatisfy` T.isInfixOf "could not be loaded"

        it "still reads the real catalogue" $ do
            result ← readCatalogueForSuggestions conceptCataloguePath
                                                conceptOrdinalPath
            fmap catVersion result `shouldBe` Right (catVersion prodCat)

    -- #1272: the reroll chain is a replay from ordinal zero, so its
    -- cost is Θ(ordinal) — and it runs synchronously on the shared Lua
    -- thread. The domain is therefore bounded at the PUBLIC boundary
    -- (world.suggestName), leaving Language.Suggest's sequence contract
    -- above untouched. These drive the registered Lua function itself,
    -- because both halves of what the bound promises — the return SHAPE
    -- on each side and the guard sitting ahead of resolveSuggestion —
    -- live at that boundary and nowhere else.
    around withHeadlessEngine $
      describe "the ordinal bound at world.suggestName" $ do
        let atOrdinal ∷ Int → Text
            atOrdinal = suggestAt 42
            expectedAt k = case suggestNameAt (suggesterFor prodCat 42) k of
                Left err  → error (T.unpack (suggestErrorText err))
                Right sug → T.concat ["ok|", nsName sug, "|", nsGloss sug]
            step p c = suggestionStepLabel (suggestionStep p c)

        it "accepts the documented maximum and suggests the real name" $ \env → do
            ls ← newBareLuaBackend env
            evalDebug ls (atOrdinal maxSuggestNameOrdinal)
                ≫= (`shouldBe` expectedAt maxSuggestNameOrdinal)

        -- The first refused ordinal: nil plus a reason naming the
        -- maximum, exactly the missing-catalogue failure's shape.
        it "refuses the first ordinal past it, naming the maximum" $ \env → do
            ls ← newBareLuaBackend env
            reply ← evalDebug ls (atOrdinal (maxSuggestNameOrdinal + 1))
            reply `shouldSatisfy` T.isPrefixOf "nil|"
            reply `shouldSatisfy` T.isInfixOf (tshow (maxSuggestNameOrdinal + 1))
            reply `shouldSatisfy` T.isInfixOf (tshow maxSuggestNameOrdinal)
            reply `shouldSatisfy` T.isInfixOf "out of range"

        -- The guard has to sit AHEAD of resolveSuggestion, not inside
        -- it: a refused ordinal must perform no ordinal-proportional
        -- work and change nothing. An untouched cache on a backend
        -- whose only call was the refused one is what proves it — the
        -- cache is written by every path through resolveSuggestion,
        -- including its failures.
        it "leaves a fresh backend's language cache untouched" $ \env → do
            ls ← newBareLuaBackend env
            let cached = isJust ⊚ readIORef (lbsLanguageCache ls)
            cached ≫= (`shouldBe` False)
            _ ← evalDebug ls (atOrdinal (maxSuggestNameOrdinal + 1))
            cached ≫= (`shouldBe` False)
            -- Not vacuous: an ACCEPTED ordinal on the same backend does
            -- populate it, so the check above is measuring the guard.
            _ ← evalDebug ls (atOrdinal 0)
            cached ≫= (`shouldBe` True)

        -- #2206 requirement 8. The whole point of returning rather
        -- than looping: the shared Lua thread survives the failure, so
        -- everything after it still works. Each assertion here runs on
        -- the SAME 'LuaBackendState' the failing call used.
        it "reports an insufficient-root-space seed and leaves the \
           \backend usable" $ \env → do
            ls ← newBareLuaBackend env
            reply ← evalDebug ls (suggestAt insufficientWorldSeed 1)
            reply `shouldSatisfy` T.isPrefixOf "nil|"
            forM_ ["137", "151", "shortfall 14"] $
                \needle → reply `shouldSatisfy` T.isInfixOf needle

            -- An immediately subsequent request is served at all...
            evalDebug ls "return world.generatedNameCharacters()"
                ≫= (`shouldSatisfy` (not ∘ T.null))
            -- ...and a valid later suggestName still returns a normal
            -- suggestion, byte-for-byte what the pure producer offers.
            evalDebug ls (suggestAt 42 1) ≫= (`shouldBe` expectedAt 1)

        -- A per-language failure must not be recorded as a catalogue
        -- failure: 'StepFailed' is sticky, so doing so would poison
        -- every later seed in the session. The cache must hold the
        -- catalogue it DID resolve, with no suggester.
        it "caches the catalogue, not the failure, for an insufficient \
           \seed" $ \env → do
            ls ← newBareLuaBackend env
            _ ← evalDebug ls (suggestAt insufficientWorldSeed 0)
            cached ← readIORef (lbsLanguageCache ls)
            case cached of
                Nothing → expectationFailure "the failing call cached nothing"
                Just lc → do
                    lcCatalogue lc `shouldSatisfy` isRight
                    isJust (lcSuggester lc) `shouldBe` False
            -- And the step it now reports for another language is a
            -- cached-catalogue REBUILD, never the sticky failure.
            step (provFor 42) cached `shouldBe` "build"

        -- Preserved from before the bound: an omitted, non-numeric, or
        -- negative ordinal is still normalized to 0 rather than refused.
        it "still normalizes an omitted or negative ordinal to zero" $ \env → do
            ls ← newBareLuaBackend env
            let zero = expectedAt 0
            evalDebug ls (atOrdinal (-1)) ≫= (`shouldBe` zero)
            evalDebug ls (T.concat
                [ "local sug = world.suggestName(42); "
                , "return 'ok|' .. sug.name .. '|' .. sug.gloss" ])
                ≫= (`shouldBe` zero)

-- | One @world.suggestName@ call, reported as @ok|name|gloss@ or
--   @nil|reason@. Shared by the ordinal-bound cases and #2206's
--   insufficient-root-space ones, which differ only in the seed.
suggestAt ∷ Word64 → Int → Text
suggestAt seed k = T.concat
    [ "local sug, err = world.suggestName(", tshow seed, ", ", tshow k, "); "
    , "if sug == nil then return 'nil|' .. tostring(err) end; "
    , "return 'ok|' .. sug.name .. '|' .. sug.gloss" ]

-- | A world seed whose derived language cannot name the catalogue
--   (#2206): 137 distinct roots against 151 concepts. Before the
--   capacity gate this seed hung the shared Lua thread.
insufficientWorldSeed ∷ Word64
insufficientWorldSeed = 1647

-- | A real Lua backend with the full API registered and nothing
--   preloaded — the same helper 'Test.Headless.UI.Slider' uses, so
--   @world.suggestName@ is reached exactly as a script reaches it.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console uses, and unwrap the string it returned.
--
--   That primitive serializes its reply, so a returned Lua string
--   arrives JSON-quoted. Every chunk below returns one string, so this
--   strips the quoting once rather than writing it into each expected
--   value, where it would read as part of the contract.
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = unquote ⊚ executeDebugLua (lbsLuaState ls) src
  where
    unquote t = case T.stripPrefix "\"" t ≫= T.stripSuffix "\"" of
        Just inner → inner
        Nothing    → t

-- | The first four suggestions world seed 42 offers at generator
--   version 5, written out as literals.
--
--   Each row is one press of the Create World dice: the meaning, the
--   native name and English gloss the player reads, and the #1092
--   provenance the accepted name would be recorded with. The language
--   seed is the same value the table in "language seed derived from the
--   world seed" pins for seed 42, and the version is spelled out rather
--   than taken from 'currentGeneratorVersion' — a generator bump
--   reshapes the whole sequence, so it should land here as a visible
--   change rather than being absorbed silently.
pinnedSeed42 ∷ [(NameExpr, Text, Text, LangSeed, GeneratorVersion)]
pinnedSeed42 =
    [ ( Modifier (ConceptId "AMBER") (ConceptId "SOUL")
      , "Janehba-fbahfahiv", "Amber Soul", seed42, gen5 )
    , ( Bare (ConceptId "OMEN")
      , "Abwvi", "Omen", seed42, gen5 )
    , ( Of (ConceptId "DEMON") Singular (ConceptId "CROSSING")
      , "Wezvij-velihzan", "Demon of Crossing", seed42, gen5 )
    , ( Bare (ConceptId "MIDNIGHT")
      , "Ynaij", "Midnight", seed42, gen5 )
    ]
  where
    seed42 = LangSeed 12594706351022957199
    gen5   = GeneratorVersion 5

-- | Which of the five shapes an expression took, as report text.
shapeOf ∷ NameExpr → String
shapeOf e = case e of
    Bare{}            → "bare"
    Modifier{}        → "modifier"
    Of _ Singular _   → "of_singular"
    Of _ Plural _     → "of_plural"
    Possessive{}      → "possessive"

-- | The concepts an expression names, so a slot repeat is visible.
slotsOf ∷ NameExpr → [ConceptId]
slotsOf e = case e of
    Bare c           → [c]
    Modifier m h     → [m, h]
    Of h _ c         → [h, c]
    Possessive o h   → [o, h]
