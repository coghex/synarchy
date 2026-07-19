{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
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
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Harness
    (sendWorldCommand, waitForWorldInit)
import World.Types
import World.Save.Serialize (loadWorld, sanitizeSaveName)
import World.Load.Stage (stageSession, renderStageError)
import World.Load.Types (StagedSession(..), StagedPage(..))

-- The primary page's display name deliberately contains '/' and ".."
-- — text 'sanitizeSaveName' rejects outright — to prove identity text
-- is display text, not a filename (see the save/load item below).
namedIdent ∷ WorldIdentity
namedIdent = WorldIdentity "Fjord / Upper.. Reach" (Just "the high fjord")

-- A page literally named "main_world" saved as a SECONDARY page —
-- issue #763 removed the old active-page-remap-to-main_world behavior
-- entirely, so this no longer "collides" with anything; it just proves
-- staging preserves an arbitrary saved id (including this one) verbatim.
colliderIdent ∷ WorldIdentity
colliderIdent = WorldIdentity "Collider" Nothing

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
                `shouldBe` Just (WorldIdentity "Northreach" Nothing)

        it "preserves interior whitespace, punctuation, and case" $ \_env →
            mkWorldIdentity (Just "  North reach, the 2nd  ") Nothing
                `shouldBe` Just (WorldIdentity "North reach, the 2nd" Nothing)

        it "trims the gloss and keeps it optional" $ \_env → do
            mkWorldIdentity (Just "Northreach") (Just " the cold place ")
                `shouldBe`
                Just (WorldIdentity "Northreach" (Just "the cold place"))
            mkWorldIdentity (Just "Northreach") (Just "   ")
                `shouldBe` Just (WorldIdentity "Northreach" Nothing)
            mkWorldIdentity (Just "Northreach") Nothing
                `shouldBe` Just (WorldIdentity "Northreach" Nothing)

    describe "identity is display text, not a save name" $ do
        it "accepts text sanitizeSaveName rejects ('/' and '..')" $ \_env → do
            -- The premise: these really are rejected as save names…
            sanitizeSaveName "Fjord / Upper.. Reach" `shouldSatisfy` isLeft
            sanitizeSaveName "a/b" `shouldSatisfy` isLeft
            sanitizeSaveName "a..b" `shouldSatisfy` isLeft
            -- …and stored verbatim as display names.
            mkWorldIdentity (Just "Fjord / Upper.. Reach") Nothing
                `shouldBe` Just (WorldIdentity "Fjord / Upper.. Reach" Nothing)

    describe "serialization" $ do
        it "round-trips named, glossed, and absent identities" $ \_env → do
            let roundTrip ∷ Maybe WorldIdentity
                          → Either String (Maybe WorldIdentity)
                roundTrip = S.decode . S.encode
            roundTrip (Just namedIdent) `shouldBe` Right (Just namedIdent)
            roundTrip (Just colliderIdent)
                `shouldBe` Right (Just colliderIdent)
            roundTrip Nothing `shouldBe` Right Nothing

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
                           "2026-07-10T00:00:00.000000Z" [])
            waitForFile ("saves/" <> slotA <> "/world.synworld")

            -- Decode the file directly: identities and metadata are in
            -- the save exactly as stored.
            logger ← readIORef (loggerRef env)
            (sdA, _) ← loadWorld logger slotA HS.empty HS.empty ⌦ either
                (\(_, e) → expectationFailure (T.unpack e)
                        ≫ error "unreachable")
                pure
            sdActivePage sdA `shouldBe` WorldPageId "id_named_w8"
            pageIdentity sdA "id_named_w8" `shouldBe` Just namedIdent
            pageIdentity sdA "main_world" `shouldBe` Just colliderIdent
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

    describe "arena pages" $ do
        -- Runs AFTER the save/load story so the arena page never rides
        -- along in its save (arena pages save/load fine — #365 — but
        -- would only add noise to the mapping assertions above).
        it "an arena page is unnamed" $ \env → do
            sendWorldCommand env (WorldInitArena (WorldPageId "id_arena"))
            ws ← waitForWorldInit env (WorldPageId "id_arena") 60
            ident ← readIORef (wsIdentityRef ws)
            ident `shouldBe` Nothing

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

-- | Poll until the world thread has written the save file. Fails after
--   ~30 s.
waitForFile ∷ FilePath → IO ()
waitForFile path = go (300 ∷ Int)
  where
    go 0 = expectationFailure $ "save file never appeared: " ⧺ path
    go n = do
        exists ← doesFileExist path
        if exists then pure () else threadDelay 100000 ≫ go (n - 1)
