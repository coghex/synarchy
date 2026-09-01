-- | #1413: the autosave rotation's two internal staging slots stay out
--   of the PUBLIC save listing.
--
--   'World.Save.Autosave.publicSaveListings' is the whole rule, applied
--   once at @engine.listSaves()@. These cases pin the predicate itself
--   against every combination that reaches it, because the two ways of
--   getting it wrong are each a real regression with a plausible
--   motivation:
--
--   * Matching by NAME alone hides a MANUAL save a player deliberately
--     called @autosave-incoming@ — the exact save the blocked cycle's
--     own refusal message asks them to rename or delete.
--   * Matching by CLASSIFICATION alone hides the entire numbered
--     @autosave-\<n\>@ family, which is every autosave the player can
--     actually load.
--
--   A third one only shows up on a ROTATED generation: a numbered slot
--   still carries the embedded 'smName' it was published under
--   (@autosave-incoming@, always), so reading the name from the metadata
--   rather than from the slot identity hides the family too — while
--   looking correct on a freshly written manual save, where the two
--   agree.
--
--   The end-to-end half — the listing observed while a generation is
--   really staged, and after a rotation failure deliberately leaves one
--   in place — is @tools\/autosave_probe.py@ phase 10, which is the only
--   place those states exist on real disk.
module Test.Headless.Save.AutosaveListing (spec) where

import UPrelude
import Test.Hspec
import World.Save.Autosave
    ( autosaveIncomingSlotName, autosaveRetiredSlotName, autosaveSlotName
    , autosaveStagingSlotNames, isAutosaveStagingSlot, publicSaveListings )
import World.Save.Serialize (SaveListing(..))
import World.Save.Types (SaveMetadata(..))

-- | A listing row for @slot@, classified by @auto@. @embedded@ is the
--   generation's own remembered 'smName', which is deliberately allowed
--   to disagree with the slot identity: that is exactly what a rotated
--   autosave generation looks like on disk.
row ∷ Text → Bool → Text → SaveListing
row slot auto embedded = SaveListing
    { slName      = slot
    , slRecovered = False
    , slMetadata  = SaveMetadata
        { smName       = embedded
        , smSeed       = 42
        , smWorldSize  = 64
        , smPlateCount = 3
        , smTimestamp  = "2026-08-19T00:00:00.000000Z"
        , smWorldName  = Nothing
        , smWorldGloss = Nothing
        , smAutosave   = auto
        , smGeneratedWorldIds = []
        }
    }

-- | The ordinary shape: a slot whose embedded name matches its identity.
slot ∷ Text → Bool → SaveListing
slot name auto = row name auto name

-- | What a ROTATED autosave generation really looks like: living in a
--   numbered slot, still remembering the staging name it was published
--   under.
rotated ∷ Int → SaveListing
rotated n = row (autosaveSlotName n) True autosaveIncomingSlotName

spec ∷ Spec
spec = do
    describe "the staging-slot predicate" $ do
        it "hides an autosave-classified generation in either reserved \
           \slot" $ do
            isAutosaveStagingSlot (slot autosaveIncomingSlotName True)
                `shouldBe` True
            isAutosaveStagingSlot (slot autosaveRetiredSlotName True)
                `shouldBe` True

        it "keeps a MANUAL save occupying either reserved name, which is \
           \the save a blocked cycle asks the player to act on" $ do
            isAutosaveStagingSlot (slot autosaveIncomingSlotName False)
                `shouldBe` False
            isAutosaveStagingSlot (slot autosaveRetiredSlotName False)
                `shouldBe` False

        it "keeps the numbered autosave family, however deep" $
            map (isAutosaveStagingSlot . rotated) [1 .. 10]
                `shouldBe` replicate 10 False

        it "reads the SLOT identity, not the generation's embedded name: \
           \a rotated generation still remembers being published as the \
           \staging slot" $ do
            smName (slMetadata (rotated 1))
                `shouldBe` autosaveIncomingSlotName
            isAutosaveStagingSlot (rotated 1) `shouldBe` False

        it "ignores a manual save whose embedded name happens to be a \
           \reserved one" $
            isAutosaveStagingSlot (row "my_colony" False
                                       autosaveRetiredSlotName)
                `shouldBe` False

        it "leaves every ordinary manual save alone" $
            map (isAutosaveStagingSlot . flip slot False)
                ["my_colony", "autosave", "autosave-", "autosave-incoming2"]
                `shouldBe` replicate 4 False

        it "covers exactly the two reserved names rotation stages \
           \through" $
            autosaveStagingSlotNames
                `shouldBe` [autosaveIncomingSlotName, autosaveRetiredSlotName]

    describe "the public listing" $ do
        -- The realistic worst case: a cycle interrupted mid-rotation,
        -- so BOTH staging slots hold a generation at once, in the
        -- newest-first order listSaves already sorted them into.
        let mid = [ slot autosaveIncomingSlotName True
                  , rotated 1
                  , slot "my_colony" False
                  , rotated 2
                  , slot autosaveRetiredSlotName True
                  , rotated 3
                  ]

        it "drops both staging rows and nothing else" $
            map slName (publicSaveListings mid)
                `shouldBe` ["autosave-1", "my_colony", "autosave-2"
                           , "autosave-3"]

        it "preserves the survivors' order, so the caller's newest-first \
           \sort still decides the Continue target" $
            publicSaveListings mid
                `shouldBe` [rotated 1, slot "my_colony" False, rotated 2
                           , rotated 3]

        it "still lists a MANUAL save on a reserved name, beside the \
           \family" $
            map slName (publicSaveListings
                            [ slot autosaveIncomingSlotName False
                            , rotated 1
                            , slot autosaveRetiredSlotName False ])
                `shouldBe` ["autosave-incoming", "autosave-1"
                           , "autosave-retired"]

        it "changes nothing when no generation is staged" $ do
            let quiet = [rotated 1, slot "my_colony" False, rotated 2]
            publicSaveListings quiet `shouldBe` quiet

        it "yields an empty listing rather than a hole when a staged \
           \generation is the only save on disk" $
            publicSaveListings [slot autosaveIncomingSlotName True]
                `shouldBe` []
