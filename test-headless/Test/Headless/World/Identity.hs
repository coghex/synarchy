-- | World identity (#707): the optional, immutable player-facing
--   identity of a world page — a non-empty display name plus an
--   optional English gloss — kept distinct from both the internal
--   routing 'WorldPageId' and the save-slot name.
--
--   Pure coverage: 'mkWorldIdentity' normalization (the owner-resolved
--   trim semantics), independence from 'sanitizeSaveName', and cereal
--   round-trips. Engine coverage (own engine, cheap private w8 pages —
--   see Spec.hs): named/unnamed/arena creation, and identity mapping
--   through 'World.Load.Stage.stageSession' — issue #763 (save-overhaul
--   C2) replaced the old incrementally-merging load path with a
--   whole-session transaction, so a real publish now REPLACES every live
--   page (no more main_world remap or collision rename to prove — saved
--   page ids and identities carry through verbatim). Staging only, never
--   publishing, keeps this test safe to run inside the shared-process
--   hspec world (a real publish would wipe every other spec's live
--   pages). The full save → load → re-save round trip through a REAL
--   publish, plus the Lua surface (world.getIdentity / engine.listSaves
--   fields), live in tools/transactional_load_probe.py and
--   tools/multiworld_save_probe.py.
module Test.Headless.World.Identity (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Either (isLeft)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import System.Directory (doesFileExist, removePathForcibly)
import Data.List (find)
import qualified Data.HashMap.Strict as HM
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Harness
    (getWorldGenParams, sendWorldCommand, waitForWorldInit)
import Test.Headless.Harness.Log (newLogCapture)
import Engine.Core.Log (initLogger)
import Engine.Core.Log.Types (LogConfig(..), LogEntry(..), defaultLogConfig)
import Location.Instance
    (LocationInstance(..), instancesToList)
import World.River.Naming (RiverName(..), riverNamesToList)
import World.Types
import Language.Generated.Types
    ( LanguageProvenance(..), LangSeed(..), GeneratorVersion(..)
    , currentGeneratorVersion, langSeedText )
import Language.Generated.Profile (generateProfile)
import World.Save.Serialize (loadWorld, sanitizeSaveName)
import World.Load.Stage (stageSession, renderStageError)
import World.Load.Publish (publishStagedSession)
import World.Load.Types (StagedSession(..), StagedPage(..))
import Building.Types (BuildingId(..))
import Craft.Bills (addBill, emptyCraftBills, cbsBills)
import Power.Types
    (addPowerNode, emptyPowerNodes, pnsNodes, PowerRole(..))

-- The primary page's display name deliberately contains '/' and ".."
-- — text 'sanitizeSaveName' rejects outright — to prove identity text
-- is display text, not a filename (see the save/load item below). It
-- is a CUSTOM name, so it carries no language provenance (#1092).
namedIdent ∷ WorldIdentity
namedIdent =
    WorldIdentity "Fjord / Upper.. Reach" (Just "the high fjord")
                  Nothing Nothing

-- A page literally named "main_world" saved as a SECONDARY page —
-- issue #763 removed the old active-page-remap-to-main_world behavior
-- entirely, so this no longer "collides" with anything; it just proves
-- staging preserves an arbitrary saved id (including this one) verbatim.
colliderIdent ∷ WorldIdentity
colliderIdent = WorldIdentity "Collider" Nothing Nothing Nothing

-- The provenance of the generated identity below (#1092). The seed is
-- deliberately ABOVE 2^63-1: a signed 64-bit or floating-point carrier
-- would mangle it, so anything that round-trips this value really is
-- carrying the full Word64 range.
testProvenance ∷ LanguageProvenance
testProvenance = LanguageProvenance
    { lpSeed = LangSeed 0xF0E1D2C3B4A59687, lpVersion = GeneratorVersion 1 }

-- A GENERATED identity: rendered name + gloss PLUS the language that
-- produced them. Built through the generated-name construction path,
-- which is the only way provenance is ever attached.
generatedIdent ∷ WorldIdentity
generatedIdent = case mkGeneratedWorldIdentity
                        (Just "Vashenkoro") (Just "the salt reach")
                        testProvenance Nothing of
    Just i  → i
    Nothing → error "generatedIdent: normalization rejected a valid name"

spec ∷ SpecWith EngineEnv
spec = do
    describe "normalization (mkWorldIdentity)" $ do
        it "no name means no identity" $ \_env →
            mkWorldIdentity Nothing Nothing `shouldBe` Nothing

        it "empty name means no identity" $ \_env →
            mkWorldIdentity (Just "") Nothing `shouldBe` Nothing

        it "whitespace-only name means no identity" $ \_env →
            mkWorldIdentity (Just "\t \n") Nothing `shouldBe` Nothing

        it "a gloss cannot exist without a display name" $ \_env → do
            mkWorldIdentity Nothing (Just "the cold place")
                `shouldBe` Nothing
            mkWorldIdentity (Just "  ") (Just "the cold place")
                `shouldBe` Nothing

        it "trims leading/trailing whitespace from the name" $ \_env →
            mkWorldIdentity (Just " Northreach ") Nothing
                `shouldBe`
                    Just (WorldIdentity "Northreach" Nothing Nothing Nothing)

        it "preserves interior whitespace, punctuation, and case" $ \_env →
            mkWorldIdentity (Just "  North reach, the 2nd  ") Nothing
                `shouldBe`
                Just (WorldIdentity "North reach, the 2nd" Nothing Nothing
                                    Nothing)

        it "trims the gloss and keeps it optional" $ \_env → do
            mkWorldIdentity (Just "Northreach") (Just " the cold place ")
                `shouldBe`
                Just (WorldIdentity "Northreach" (Just "the cold place")
                          Nothing Nothing)
            mkWorldIdentity (Just "Northreach") (Just "   ")
                `shouldBe`
                    Just (WorldIdentity "Northreach" Nothing Nothing Nothing)
            mkWorldIdentity (Just "Northreach") Nothing
                `shouldBe`
                    Just (WorldIdentity "Northreach" Nothing Nothing Nothing)

    describe "identity is display text, not a save name" $ do
        it "accepts text sanitizeSaveName rejects ('/' and '..')" $ \_env → do
            -- The premise: these really are rejected as save names…
            sanitizeSaveName "Fjord / Upper.. Reach" `shouldSatisfy` isLeft
            sanitizeSaveName "a/b" `shouldSatisfy` isLeft
            sanitizeSaveName "a..b" `shouldSatisfy` isLeft
            -- …and stored verbatim as display names.
            mkWorldIdentity (Just "Fjord / Upper.. Reach") Nothing
                `shouldBe`
                Just (WorldIdentity "Fjord / Upper.. Reach" Nothing Nothing
                                    Nothing)

    describe "language provenance (#1092)" $ do
        it "the custom-name path never attaches provenance" $ \_env → do
            -- Every mkWorldIdentity result above already asserts this
            -- structurally; stated once as its own contract, since #708
            -- principle 7 is what forbids inferring a language for a
            -- player-entered name.
            (wiLanguage ⌫ mkWorldIdentity (Just "Northreach") Nothing)
                `shouldBe` Nothing
            (wiLanguage ⌫ mkWorldIdentity (Just " Northreach ")
                              (Just "the cold place"))
                `shouldBe` Nothing

        it "the generated-name path attaches exactly the supplied \
           \provenance, with identical normalization" $ \_env → do
            mkGeneratedWorldIdentity (Just "  Vashenkoro  ")
                    (Just "  the salt reach ") testProvenance Nothing
                `shouldBe` Just (WorldIdentity "Vashenkoro"
                                     (Just "the salt reach")
                                     (Just testProvenance) Nothing)
            -- A name that isn't a name is still no identity, provenance
            -- or not — provenance can never conjure one into existence.
            mkGeneratedWorldIdentity (Just "  ") (Just "g") testProvenance
                                     Nothing
                `shouldBe` Nothing
            mkGeneratedWorldIdentity Nothing Nothing testProvenance Nothing
                `shouldBe` Nothing

        it "a recovered provenance rebuilds the SAME profile the seed \
           \originally produced" $ \_env → do
            -- Requirement 1's whole point: the recorded pair is enough
            -- to reconstruct the language, not merely to display it.
            let recovered = wiLanguage generatedIdent
            recovered `shouldBe` Just testProvenance
            case recovered of
                Nothing → expectationFailure "expected provenance"
                Just p  →
                    generateProfile (lpVersion p) (lpSeed p)
                        `shouldBe` generateProfile (lpVersion testProvenance)
                                       (lpSeed testProvenance)

        it "a seed above 2^63-1 survives the text carrier exactly" $ \_env → do
            -- The Lua/JSON surface can't carry an unsigned 64-bit value
            -- as a number, so it carries decimal text; this is what
            -- proves that carrier is lossless at the top of the range.
            langSeedText (LangSeed maxBound)
                `shouldBe` T.pack (show (maxBound ∷ Word64))
            langSeedText (lpSeed testProvenance)
                `shouldBe` "17357386176853808775"

    describe "serialization" $ do
        it "round-trips named, glossed, and absent identities" $ \_env → do
            let roundTrip ∷ Maybe WorldIdentity
                          → Either String (Maybe WorldIdentity)
                roundTrip = S.decode . S.encode
            roundTrip (Just namedIdent) `shouldBe` Right (Just namedIdent)
            roundTrip (Just colliderIdent)
                `shouldBe` Right (Just colliderIdent)
            roundTrip Nothing `shouldBe` Right Nothing

        it "round-trips a generated identity's provenance, seed and \
           \version together" $ \_env → do
            let roundTrip ∷ Maybe WorldIdentity
                          → Either String (Maybe WorldIdentity)
                roundTrip = S.decode . S.encode
            roundTrip (Just generatedIdent)
                `shouldBe` Right (Just generatedIdent)

    describe "page creation" $ do
        it "WorldInit with an identity creates a named page" $ \env → do
            sendWorldCommand env
                (WorldInit (WorldPageId "id_named_w8") 21 8 3
                           (Just namedIdent))
            ws ← waitForWorldInit env (WorldPageId "id_named_w8") 120
            ident ← readIORef (wsIdentityRef ws)
            ident `shouldBe` Just namedIdent

        it "WorldInit without an identity creates an unnamed page" $ \env → do
            sendWorldCommand env
                (WorldInit (WorldPageId "id_unnamed_w8") 23 8 3 Nothing)
            ws ← waitForWorldInit env (WorldPageId "id_unnamed_w8") 120
            ident ← readIORef (wsIdentityRef ws)
            ident `shouldBe` Nothing

        it "WorldInit with a GENERATED identity keeps its language \
           \provenance on the live page (#1092)" $ \env → do
            sendWorldCommand env
                (WorldInit (WorldPageId "id_generated_w8") 27 8 3
                           (Just generatedIdent))
            ws ← waitForWorldInit env (WorldPageId "id_generated_w8") 120
            ident ← readIORef (wsIdentityRef ws)
            ident `shouldBe` Just generatedIdent

    -- #2206. Language seed 1116 builds a profile whose root space holds
    -- 144 distinct roots against the catalogue's 151 concepts, so no
    -- assignment over it exists. Before the capacity gate,
    -- 'resolvePageNamer' did not fail here — it never returned, and the
    -- world thread never finished initializing this page.
    describe "a language too small to name the catalogue (#2206)" $ do
        let shortPage = WorldPageId "id_short_roots_w8"
            okPage    = WorldPageId "id_ample_roots_w8"
            identFor sd = case mkGeneratedWorldIdentity
                                  (Just "Vashenkoro") (Just "the salt reach")
                                  (LanguageProvenance (LangSeed sd)
                                                      currentGeneratorVersion)
                                  Nothing of
                Just i  → i
                Nothing → error "identFor: normalization rejected a valid name"
        -- Both pages use ONE world seed and size, so they differ only in
        -- the language: same terrain, same placements, same rivers.

        it "completes WorldInit, keeps the page identity, warns with the \
           \generator error, and leaves the river table empty" $ \env → do
            (backend, drain) ← newLogCapture
            capture  ← initLogger defaultLogConfig { lcBackend = backend }
            original ← readIORef (loggerRef env)
            ws ← (do writeIORef (loggerRef env) capture
                     sendWorldCommand env
                         (WorldInit shortPage 31 8 3 (Just (identFor 1116)))
                     -- Reaching this line at all is the regression: the
                     -- unbounded reroll made this page never appear.
                     waitForWorldInit env shortPage 120)
                    `finally` writeIORef (loggerRef env) original

            -- The identity is RETAINED, not swapped for another language.
            readIORef (wsIdentityRef ws) `shouldReturn` Just (identFor 1116)

            entries ← drain
            let disabled = [ leMessage e | e ← entries
                           , "Name generation disabled for this world: "
                               `T.isPrefixOf` leMessage e ]
            disabled `shouldSatisfy` (not ∘ null)
            disabled `shouldSatisfy` any (T.isInfixOf "shortfall 7")
            disabled `shouldSatisfy` any (T.isInfixOf "seed 1116")

            params ← getWorldGenParams ws
            case params of
                Nothing → expectationFailure "the initialized page has no gen params"
                Just p  → do
                    -- The observable half of the no-namer fallback on a
                    -- test-scale world: this page's rivers stay
                    -- unnamed. The control below proves the same world
                    -- DOES name them under a language with room for the
                    -- catalogue, so this is not an empty table for want
                    -- of rivers.
                    riverNamesToList (wgpRiverNames p) `shouldBe` []
                    -- The location half of the same fallback: whatever
                    -- this page placed carries its definition's label,
                    -- with no gloss and no etymology. Test-scale worlds
                    -- place none, so the location clause of #2206
                    -- requirement 7 is pinned where it IS observable —
                    -- "Location naming"'s no-namer fixture, plus the
                    -- mkLocationNamer rejection that routes this page
                    -- to it.
                    let insts = instancesToList (wgpLocationInstances p)
                    map liGloss insts `shouldSatisfy` all isNothing
                    map liEtymology insts `shouldSatisfy` all isNothing

        -- The control. Same world seed and size, a language with room
        -- for the catalogue: this page DOES carry generated location
        -- names, so the fallback above is the language's doing and the
        -- assertions on it are not passing over an empty world.
        it "still names this world's rivers under a sufficient language" $
            \env → do
                sendWorldCommand env
                    (WorldInit okPage 31 8 3 (Just (identFor 1117)))
                ws ← waitForWorldInit env okPage 120
                params ← getWorldGenParams ws
                case params of
                    Nothing → expectationFailure
                        "the initialized page has no gen params"
                    Just p  → do
                        let named = riverNamesToList (wgpRiverNames p)
                        named `shouldSatisfy` (not ∘ null)
                        map (rvnDisplayName ∘ snd) named
                            `shouldSatisfy` all (not ∘ T.null)
                        map (rvnGloss ∘ snd) named
                            `shouldSatisfy` all isJust

    describe "language-provenance query (#1092 requirement 5)" $ do
        -- Runs after page creation above, against those same live pages.
        it "reports a generated page's seed and version as one value" $
            \env → do
                mgr ← readIORef (worldManagerRef env)
                pageLanguageProvenance mgr (WorldPageId "id_generated_w8")
                    `shouldReturn` Just testProvenance

        it "reports nothing for a custom-named, an unnamed, or a \
           \nonexistent page — never an inferred language" $ \env → do
            mgr ← readIORef (worldManagerRef env)
            pageLanguageProvenance mgr (WorldPageId "id_named_w8")
                `shouldReturn` Nothing
            pageLanguageProvenance mgr (WorldPageId "id_unnamed_w8")
                `shouldReturn` Nothing
            pageLanguageProvenance mgr (WorldPageId "id_no_such_page")
                `shouldReturn` Nothing

    describe "save/load mapping" $ do
        -- One story, in order: save a multi-page world whose active page
        -- is named (with save-name-hostile text) and whose second page
        -- is literally id'd "main_world"; decode the file; STAGE it
        -- (never publish — issue #763 requirement 6: staging touches no
        -- live ref, so this is safe to run inside the shared-process
        -- hspec world, unlike a real publish, which replaces every live
        -- page process-wide). Every saved page's id and identity must
        -- come through staging completely unchanged (no more main_world
        -- remap or collision rename — loading replaces the complete
        -- session, so nothing survives to collide with).
        it "identities survive save → stage verbatim, under their own\
           \ saved ids" $ \env →
            let slotA = "id_spec_roundtrip"
                cleanup = do
                    removePathForcibly ("saves/" <> slotA)
                    -- WorldSave auto-pauses the engine; don't leak that.
                    writeIORef (enginePausedRef env) False
            in (`finally` cleanup) $ do
            -- A stale dir from an interrupted run could false-pass the
            -- decode below — start clean.
            removePathForcibly ("saves/" <> slotA)

            -- The named primary page exists (created by the page-creation
            -- item above; waitForWorldInit is a cheap re-wait when it has
            -- already finished). The second page is a fresh, literally
            -- "main_world"-id'd page.
            _ ← waitForWorldInit env (WorldPageId "id_named_w8") 120
            sendWorldCommand env
                (WorldInit (WorldPageId "main_world") 25 8 3
                           (Just colliderIdent))
            _ ← waitForWorldInit env (WorldPageId "main_world") 120

            -- Save with id_named_w8 as the primary (active) page.
            sendWorldCommand env
                (WorldSave (WorldPageId "id_named_w8") slotA
                           "2026-07-10T00:00:00.000000Z" [] [] Nothing)
            waitForFile ("saves/" <> slotA <> "/world.synworld")

            -- Decode the file directly: identities and metadata are in
            -- the save exactly as stored.
            logger ← readIORef (loggerRef env)
            (sdA, _, _) ← loadWorld logger slotA HS.empty HS.empty ⌦ either
                (\(_, e) → expectationFailure (T.unpack e)
                        ≫ error "unreachable")
                pure
            sdActivePage sdA `shouldBe` WorldPageId "id_named_w8"
            pageIdentity sdA "id_named_w8" `shouldBe` Just namedIdent
            pageIdentity sdA "main_world" `shouldBe` Just colliderIdent
            -- #1092: the generated page created above rides along in
            -- this same save (WorldSave captures every live page), so
            -- one round trip proves both halves of the contract — a
            -- GENERATED identity keeps its provenance, and the custom
            -- ones above keep their ABSENT provenance rather than
            -- acquiring an inferred one.
            pageIdentity sdA "id_generated_w8" `shouldBe` Just generatedIdent
            (wiLanguage ⌫ pageIdentity sdA "id_generated_w8")
                `shouldBe` Just testProvenance
            (wiLanguage ⌫ pageIdentity sdA "id_named_w8") `shouldBe` Nothing
            -- Save-slot name, world name, and gloss are three distinct
            -- things (requirement 10/12): smName is the slot, and the
            -- world's name is text no save slot could even be called.
            smName (sdMetadata sdA) `shouldBe` slotA
            smWorldName (sdMetadata sdA) `shouldBe` Just (wiName namedIdent)
            smWorldGloss (sdMetadata sdA) `shouldBe` wiGloss namedIdent
            sanitizeSaveName (wiName namedIdent) `shouldSatisfy` isLeft

            -- Stage (not publish): the active page stays "id_named_w8"
            -- and the second page stays "main_world" — both keep their
            -- own saved identity, verbatim. WorldSave captures EVERY
            -- live page in this shared-process engine (other specs'
            -- pages, e.g. "id_unnamed_w8", ride along too), so this only
            -- asserts the two pages under test are present, not an
            -- exact page-set match.
            matReg ← readIORef (materialRegistryRef env)
            staged ← stageSession env logger sdA matReg ⌦ either
                (\e → expectationFailure (T.unpack (renderStageError e))
                        ≫ error "unreachable")
                pure
            ssActivePage staged `shouldBe` WorldPageId "id_named_w8"
            map spPageId (ssPages staged) `shouldContain`
                [WorldPageId "id_named_w8"]
            map spPageId (ssPages staged) `shouldContain`
                [WorldPageId "main_world"]
            stagedIdentity staged "id_named_w8" `shouldReturn` Just namedIdent
            stagedIdentity staged "main_world" `shouldReturn` Just colliderIdent
            -- …and the recovered provenance still rebuilds the same
            -- profile, which is what makes it worth persisting (#1092).
            stagedGenerated ← stagedIdentity staged "id_generated_w8"
            stagedGenerated `shouldBe` Just generatedIdent
            case wiLanguage ⌫ stagedGenerated of
                Nothing → expectationFailure
                    "staged generated page lost its language provenance"
                Just p  →
                    generateProfile (lpVersion p) (lpSeed p)
                        `shouldBe` generateProfile (lpVersion testProvenance)
                                       (lpSeed testProvenance)

    -- Round 9 review (issue #763): 'World.Load.Stage.stagePage' used to
    -- filter every staged page's craft bills and power nodes against
    -- that SAME save's building snapshot, silently dropping any record
    -- whose station/building instance wasn't in it. That contradicts
    -- the documented, pre-existing #758 persistence contract (see
    -- docs/persistence_state_inventory.md and Craft.Bills's own
    -- 'cbStation' doc comment) that a demolished station's bills
    -- "linger, visible + cancellable" rather than vanish across a
    -- save/load round trip. This proves staging preserves such a
    -- dangling record verbatim instead of pruning it.
    describe "dangling craft bills / power nodes survive staging \
              \(issue #758 contract, round 9 review)" $
        it "a craft bill and a power node whose station/building is \
           \absent from the save's own building snapshot are NOT \
           \pruned by stageSession" $ \env →
            let slotB = "id_spec_dangling"
                cleanup = do
                    removePathForcibly ("saves/" <> slotB)
                    writeIORef (enginePausedRef env) False
            in (`finally` cleanup) $ do
            removePathForcibly ("saves/" <> slotB)

            sendWorldCommand env
                (WorldInit (WorldPageId "id_dangling_w8") 33 8 3 Nothing)
            _ ← waitForWorldInit env (WorldPageId "id_dangling_w8") 120

            sendWorldCommand env
                (WorldSave (WorldPageId "id_dangling_w8") slotB
                           "2026-07-19T00:00:00.000000Z" [] [] Nothing)
            waitForFile ("saves/" <> slotB <> "/world.synworld")

            logger ← readIORef (loggerRef env)
            (sdB, _, _) ← loadWorld logger slotB HS.empty HS.empty ⌦ either
                (\(_, e) → expectationFailure (T.unpack e)
                        ≫ error "unreachable")
                pure

            -- Inject a craft bill and a power node riding on a building
            -- id GUARANTEED absent from this freshly-generated page's
            -- (empty) building snapshot -- simulating a station
            -- demolished before the save was ever taken.
            let danglingBuilding = BuildingId 999999
                (dangledBills, danglingBillId) =
                    addBill danglingBuilding "probe_recipe" 1 emptyCraftBills
                (dangledNodes, danglingNodeId) =
                    addPowerNode danglingBuilding PowerSource 100
                        emptyPowerNodes
                injectDangling w
                    | wpsPageId w ≡ WorldPageId "id_dangling_w8" =
                        w { wpsCraftBills = dangledBills
                          , wpsPowerNodes = dangledNodes }
                    | otherwise = w
                sdB' = sdB { sdWorlds = map injectDangling (sdWorlds sdB) }

            matReg ← readIORef (materialRegistryRef env)
            staged ← stageSession env logger sdB' matReg ⌦ either
                (\e → expectationFailure (T.unpack (renderStageError e))
                        ≫ error "unreachable")
                pure

            case find ((≡ WorldPageId "id_dangling_w8") . spPageId)
                      (ssPages staged) of
                Nothing → expectationFailure
                    "id_dangling_w8 missing from the staged session"
                Just sp → do
                    stagedBills ← readIORef
                        (wsCraftBillsRef (spWorldState sp))
                    stagedNodes ← readIORef
                        (wsPowerNodesRef (spWorldState sp))
                    HM.member danglingBillId (cbsBills stagedBills)
                        `shouldBe` True
                    HM.member danglingNodeId (pnsNodes stagedNodes)
                        `shouldBe` True

    describe "arena pages" $ do
        -- Runs AFTER the save/load story so the arena page never rides
        -- along in its save (arena pages save/load fine — #365 — but
        -- would only add noise to the mapping assertions above).
        it "an arena page is unnamed" $ \env → do
            sendWorldCommand env (WorldInitArena (WorldPageId "id_arena"))
            ws ← waitForWorldInit env (WorldPageId "id_arena") 60
            ident ← readIORef (wsIdentityRef ws)
            ident `shouldBe` Nothing

    -- From here on, every example performs a REAL publish -- it replaces
    -- the complete session, so nothing after one of these may assume any
    -- page created EARLIER in this file still exists. Each such example
    -- therefore creates the pages it needs itself (issue #763, round 11
    -- review; issue #1670).
    describe "publishStagedSession invalidates in-flight preview uploads \
             \on EVERY publish, even one with no preview data at all \
             \(round 11 review, issue #763)" $
        it "a staged session whose ssPreview is Nothing (the outcome of \
           \World.Load.Stage's own isArenaParams branch) still bumps \
           \worldPreviewGenerationRef -- a stale upload racing this \
           \publish must never be able to see its own generation as \
           \still current" $ \env →
            let slotC = "id_spec_nopreview"
                cleanup = do
                    removePathForcibly ("saves/" <> slotC)
                    writeIORef (enginePausedRef env) False
            in (`finally` cleanup) $ do
            removePathForcibly ("saves/" <> slotC)

            sendWorldCommand env
                (WorldInit (WorldPageId "id_nopreview_w8") 51 8 3 Nothing)
            _ ← waitForWorldInit env (WorldPageId "id_nopreview_w8") 120

            sendWorldCommand env
                (WorldSave (WorldPageId "id_nopreview_w8") slotC
                           "2026-07-19T00:00:00.000000Z" [] [] Nothing)
            waitForFile ("saves/" <> slotC <> "/world.synworld")

            logger ← readIORef (loggerRef env)
            (sdC, _, _) ← loadWorld logger slotC HS.empty HS.empty ⌦ either
                (\(_, e) → expectationFailure (T.unpack e)
                        ≫ error "unreachable")
                pure

            matReg ← readIORef (materialRegistryRef env)
            staged ← stageSession env logger sdC matReg ⌦ either
                (\e → expectationFailure (T.unpack (renderStageError e))
                        ≫ error "unreachable")
                pure

            -- Force the no-preview outcome directly rather than driving
            -- a real arena-page save (stageSession's own isArenaParams
            -- branch is exercised elsewhere and isn't the thing under
            -- test here) -- this isolates publishStagedSession's own
            -- unconditional-bump contract from staging's decision about
            -- when a preview exists at all.
            let staged' = staged { ssPreview = Nothing }

            genBefore ← readIORef (worldPreviewGenerationRef env)
            publishStagedSession env logger 999999 staged'
            genAfter ← readIORef (worldPreviewGenerationRef env)
            genAfter `shouldSatisfy` (> genBefore)

    -- Runs LAST (issue #1670): another REAL publish, and the one that
    -- replaces the session for good.
    --
    -- 'World.Load.Stage' builds a separate wsZoomCacheRef for EVERY
    -- non-arena staged page but atlas PIXELS for only the active one, so
    -- pairing that single payload with every published page (which is
    -- what World.Load.Publish did before #1670) handed a second visible
    -- page an atlas its own cache did not produce. That is not cosmetic:
    -- World.Render.Zoom.Bake computes col/row from the ASSIGNED atlas's
    -- chunksPerRow over the PAGE'S OWN cache index, so such a page bakes
    -- its quads from another world's pixels, and indexes past the
    -- texture entirely whenever its own cache is the longer one.
    describe "a whole-session load attaches the zoom atlas ONLY to the \
             \page whose own cache produced it (issue #1670)" $
        it "publishing a staged session with two visible non-arena pages \
           \hands zoomAtlasDataRef exactly one target state -- the \
           \active page's own -- and never the other visible generated \
           \page's, which keeps wsZoomAtlasRef at Nothing and renders \
           \through ensureBakedAtlas's per-material fallback" $ \env →
            let slotD = "id_spec_atlas_owner"
                cleanup = do
                    removePathForcibly ("saves/" <> slotD)
                    writeIORef (enginePausedRef env) False
            in (`finally` cleanup) $ do
            removePathForcibly ("saves/" <> slotD)

            -- Two fresh generated (non-arena) pages, BOTH visible, so
            -- the save records a genuine multi-visible-page session.
            -- Distinct seeds keep their zoom caches independently built.
            sendWorldCommand env
                (WorldInit (WorldPageId "id_atlas_owner_w8") 61 8 3 Nothing)
            _ ← waitForWorldInit env (WorldPageId "id_atlas_owner_w8") 120
            sendWorldCommand env
                (WorldInit (WorldPageId "id_atlas_other_w8") 62 8 3 Nothing)
            _ ← waitForWorldInit env (WorldPageId "id_atlas_other_w8") 120
            sendWorldCommand env (WorldShow (WorldPageId "id_atlas_other_w8"))
            sendWorldCommand env (WorldShow (WorldPageId "id_atlas_owner_w8"))

            sendWorldCommand env
                (WorldSave (WorldPageId "id_atlas_owner_w8") slotD
                           "2026-08-25T00:00:00.000000Z" [] [] Nothing)
            waitForFile ("saves/" <> slotD <> "/world.synworld")

            logger ← readIORef (loggerRef env)
            (sdD, _, _) ← loadWorld logger slotD HS.empty HS.empty ⌦ either
                (\(_, e) → expectationFailure (T.unpack e)
                        ≫ error "unreachable")
                pure
            -- The premise this example exists for: the save really does
            -- carry more than one visible non-arena page. Without this
            -- the target-count assertion below could pass vacuously on a
            -- single-page session.
            sdVisiblePages sdD `shouldContain` [WorldPageId "id_atlas_owner_w8"]
            sdVisiblePages sdD `shouldContain` [WorldPageId "id_atlas_other_w8"]

            matReg ← readIORef (materialRegistryRef env)
            staged ← stageSession env logger sdD matReg ⌦ either
                (\e → expectationFailure (T.unpack (renderStageError e))
                        ≫ error "unreachable")
                pure
            ssActivePage staged `shouldBe` WorldPageId "id_atlas_owner_w8"

            -- Staging names the owner, and it is the page whose own
            -- cache built the pixels (today, the active one).
            case ssZoomAtlas staged of
                Nothing → expectationFailure
                    "staged session carries no zoom atlas at all -- the \
                    \publish assertions below would be vacuous"
                Just (ownerPid, _, _, _) →
                    ownerPid `shouldBe` ssActivePage staged

            ownerState ← stagedState staged "id_atlas_owner_w8"
            otherState ← stagedState staged "id_atlas_other_w8"

            -- Clear the handoff slot first: a WorldInit above already
            -- wrote an atlas into it, so reading a Just afterwards would
            -- prove nothing about THIS publish.
            writeIORef (zoomAtlasDataRef env) Nothing
            publishStagedSession env logger 999998 staged

            enqueued ← readIORef (zoomAtlasDataRef env)
            case enqueued of
                Nothing → expectationFailure
                    "publish enqueued no zoom atlas payload at all"
                Just (_, _, _, targets) → do
                    -- WorldState has neither Eq nor Show; a page's own
                    -- private IORef IS its identity, and IORef's Eq is
                    -- pointer equality, so compare through that.
                    length targets `shouldBe` 1
                    map (isSamePage ownerState) targets `shouldBe` [True]
                    map (isSamePage otherState) targets `shouldBe` [False]

            -- Requirement 2: the excluded page renders through the
            -- existing Maybe-Nothing per-material fallback rather than
            -- another page's atlas. (Nothing ever uploads it here --
            -- headless runs no handleZoomAtlasUpload -- so this is the
            -- state the render path would actually see.)
            readIORef (wsZoomAtlasRef otherState) `shouldReturn` Nothing

-- | Whether two 'WorldState' handles are the same page. 'WorldState'
--   derives neither 'Eq' nor 'Show', but each page's own private
--   'Data.IORef.IORef's are its identity and 'Data.IORef.IORef''s 'Eq'
--   is pointer equality, so one field settles it.
isSamePage ∷ WorldState → WorldState → Bool
isSamePage a b = wsZoomCacheRef a ≡ wsZoomCacheRef b

-- | The stored identity of the page saved under @pid@, or Nothing when
--   the page is absent or unnamed.
pageIdentity ∷ SaveData → Text → Maybe WorldIdentity
pageIdentity sd pid =
    case filter ((≡ WorldPageId pid) . wpsPageId) (sdWorlds sd) of
        (w:_) → wpsIdentity w
        []    → Nothing

-- | The identity a 'stageSession' result carries for saved page @pid@,
--   or 'Nothing' when that page isn't in the staged result at all
--   (distinct from a present-but-unnamed page, also 'Nothing' — callers
--   here always cross-check page presence separately via 'ssPages').
stagedIdentity ∷ StagedSession → Text → IO (Maybe WorldIdentity)
stagedIdentity staged pid =
    case find ((≡ WorldPageId pid) . spPageId) (ssPages staged) of
        Nothing → pure Nothing
        Just p  → readIORef (wsIdentityRef (spWorldState p))

-- | The 'WorldState' a 'stageSession' result staged for saved page
--   @pid@. Fails the example when that page is absent, which for the
--   #1670 atlas-ownership assertions would otherwise silently compare
--   against a state nothing produced.
stagedState ∷ StagedSession → Text → IO WorldState
stagedState staged pid =
    case find ((≡ WorldPageId pid) . spPageId) (ssPages staged) of
        Nothing → expectationFailure
                      ("staged session has no page " ⧺ T.unpack pid)
                  ≫ error "unreachable"
        Just p  → pure (spWorldState p)

-- | Poll until the world thread has written the save file. Fails after
--   ~30 s.
waitForFile ∷ FilePath → IO ()
waitForFile path = go (300 ∷ Int)
  where
    go 0 = expectationFailure $ "save file never appeared: " ⧺ path
    go n = do
        exists ← doesFileExist path
        if exists then pure () else threadDelay 100000 ≫ go (n - 1)
