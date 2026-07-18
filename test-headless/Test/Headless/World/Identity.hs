{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | World identity (#707): the optional, immutable player-facing
--   identity of a world page — a non-empty display name plus an
--   optional English gloss — kept distinct from both the internal
--   routing 'WorldPageId' and the save-slot name.
--
--   Pure coverage: 'mkWorldIdentity' normalization (the owner-resolved
--   trim semantics), independence from 'sanitizeSaveName', and cereal
--   round-trips. Engine coverage (own engine, cheap private w8 pages —
--   see Spec.hs): named/unnamed/arena creation, and the full
--   save → load → re-save identity mapping, including the active page's
--   remap to @main_world@ and a genuine active-target collision rename
--   to @main_world#2@. The Lua surface (world.getIdentity /
--   engine.listSaves fields) plus the gold-standard quit-and-restart
--   round-trip live in tools/multiworld_save_probe.py.
module Test.Headless.World.Identity (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Either (isLeft)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import System.Directory (doesFileExist, removePathForcibly)
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Harness
    (sendWorldCommand, waitForWorldInit, getWorldState)
import World.Types
import World.Save.Serialize (loadWorld, sanitizeSaveName)

-- The primary page's display name deliberately contains '/' and ".."
-- — text 'sanitizeSaveName' rejects outright — to prove identity text
-- is display text, not a filename (see the save/load item below).
namedIdent ∷ WorldIdentity
namedIdent = WorldIdentity "Fjord / Upper.. Reach" (Just "the high fjord")

-- A page literally named "main_world" saved as a SECONDARY page: on
-- load the active page takes main_world, so this one must be renamed
-- to main_world#2 while keeping this identity.
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
        -- is named (with save-name-hostile text) and whose SECONDARY
        -- page is literally id'd "main_world"; decode the file; load it
        -- (active → main_world, collider → main_world#2); re-save the
        -- loaded world and decode again. Identities must follow their
        -- pages through every remap (#707 requirement 9).
        it "identities survive save → load → re-save across the\
           \ main_world remap and a collision rename" $ \env →
            let slotA = "id_spec_roundtrip"
                slotB = "id_spec_resave"
                cleanup = do
                    removePathForcibly ("saves/" <> slotA)
                    removePathForcibly ("saves/" <> slotB)
                    -- WorldSave auto-pauses the engine; don't leak that.
                    writeIORef (enginePausedRef env) False
            in (`finally` cleanup) $ do
            -- A stale dir from an interrupted run could false-pass the
            -- decode below — start clean.
            removePathForcibly ("saves/" <> slotA)
            removePathForcibly ("saves/" <> slotB)

            -- The named primary page exists (created by the page-creation
            -- item above; waitForWorldInit is a cheap re-wait when it has
            -- already finished). The collider is a fresh secondary page.
            _ ← waitForWorldInit env (WorldPageId "id_named_w8") 120
            sendWorldCommand env
                (WorldInit (WorldPageId "main_world") 25 8 3
                           (Just colliderIdent))
            _ ← waitForWorldInit env (WorldPageId "main_world") 120

            -- Save with id_named_w8 as the primary (active) page.
            sendWorldCommand env
                (WorldSave (WorldPageId "id_named_w8") slotA
                           "2026-07-10T00:00:00.000000Z" HM.empty)
            waitForFile ("saves/" <> slotA <> "/world.synworld")

            -- Decode the file directly: identities and metadata are in
            -- the save exactly as stored.
            logger ← readIORef (loggerRef env)
            sdA ← loadWorld logger slotA ⌦ either
                (\e → expectationFailure (T.unpack e)
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

            -- Load: the active page restores under main_world and keeps
            -- its identity; the saved secondary "main_world" collides
            -- with that and is renamed main_world#2, keeping ITS identity
            -- (RestoreIds.assignRestoreIds).
            sendWorldCommand env
                (WorldLoadSave (WorldPageId "main_world") sdA)
            waitForLiveIdentity env "main_world" (Just namedIdent)
            waitForLiveIdentity env "main_world#2" (Just colliderIdent)

            -- Re-save the loaded world (primary now literally
            -- main_world). Pages snapshot under their LIVE ids — the
            -- renamed page legitimately saves as main_world#2 — but each
            -- page's identity must still be the original one.
            sendWorldCommand env
                (WorldSave (WorldPageId "main_world") slotB
                           "2026-07-10T00:00:01.000000Z" HM.empty)
            waitForFile ("saves/" <> slotB <> "/world.synworld")
            sdB ← loadWorld logger slotB ⌦ either
                (\e → expectationFailure (T.unpack e)
                        ≫ error "unreachable")
                pure
            sdActivePage sdB `shouldBe` WorldPageId "main_world"
            pageIdentity sdB "main_world" `shouldBe` Just namedIdent
            pageIdentity sdB "main_world#2" `shouldBe` Just colliderIdent
            smWorldName (sdMetadata sdB) `shouldBe` Just (wiName namedIdent)
            smWorldGloss (sdMetadata sdB) `shouldBe` wiGloss namedIdent

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

-- | Poll until the LIVE page @pid@ exists with the expected identity
--   (the load path registers a page before writing its refs, so a bare
--   existence check could read too early). Fails after ~30 s.
waitForLiveIdentity ∷ EngineEnv → Text → Maybe WorldIdentity → IO ()
waitForLiveIdentity env pid expected = go (300 ∷ Int)
  where
    go 0 = expectationFailure $
        "page " ⧺ show pid ⧺ " never appeared with identity "
        ⧺ show expected
    go n = do
        mWs ← getWorldState env (WorldPageId pid)
        mIdent ← traverse (readIORef . wsIdentityRef) mWs
        if mIdent ≡ Just expected
            then pure ()
            else threadDelay 100000 ≫ go (n - 1)

-- | Poll until the world thread has written the save file. Fails after
--   ~30 s.
waitForFile ∷ FilePath → IO ()
waitForFile path = go (300 ∷ Int)
  where
    go 0 = expectationFailure $ "save file never appeared: " ⧺ path
    go n = do
        exists ← doesFileExist path
        if exists then pure () else threadDelay 100000 ≫ go (n - 1)
