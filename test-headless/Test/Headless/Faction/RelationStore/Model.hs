{-# LANGUAGE Strict #-}
-- | The pure half of the "Faction relation store" gate (#2548): the
--   store's identity, merge and removal rules in
--   "Unit.Faction.RelationStore", asserted against EXACT expected
--   overlays rather than against the implementation's own operations, so
--   a mutated rule (last-write-wins, a removal that sweeps another
--   owner, a pair left behind empty) fails here instead of agreeing with
--   itself. No engine and no IO.
module Test.Headless.Faction.RelationStore.Model (spec) where

import UPrelude
import Test.Hspec
import Data.List (permutations)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Headless.Faction.RelationStore.Fixture
import Unit.Faction (FactionRelation(..))
import Unit.Faction.Profile
    ( LiveRelationCause(..), addRelationCause, emptyFactionPolicy
    , factionTagText, liveCausesFor, relationCauseId, relationCauseIdText
    , withLiveCauses )
import Unit.Faction.RelationStore

spec ∷ Spec
spec = describe "pure operations" $ do
    it "starts empty, and an empty store's overlay is empty" $ do
        relationCauseStoreOverlay emptyRelationCauseStore `shouldBe` []
        relationCauseStoreNull emptyRelationCauseStore `shouldBe` True
        relationCauseStoreNull sampleStore `shouldBe` False

    it "adding a cause is idempotent on its identity: the same cause \
       \twice leaves ONE" $ do
        let c = cause "order:attack_1" "raid_a" "raid_b" RelHostile
            once  = addStoredCause c emptyRelationCauseStore
            twice = addStoredCause c once
        twice `shouldBe` once
        relationCauseStoreOverlay twice `shouldBe` [c]

    it "the overlay is one entry per (ordered pair, owner), in pair-then-\
       \owner order" $
        [ ( relationCauseIdText (lrcCause c), factionTagText (lrcSource c)
          , factionTagText (lrcTarget c), lrcRelation c )
        | c ← relationCauseStoreOverlay sampleStore ]
            `shouldBe`
            [ ("report:incident_7", "nomad",  "acolyte", RelHostile)
            , ("order:attack_1",    "raid_a", "farmers", RelNeutral)
            , ("order:attack_1",    "raid_a", "raid_b",  RelHostile)
            , ("report:incident_7", "raid_a", "raid_b",  RelHostile)
            , ("diplomacy:truce_2", "raid_b", "raid_a",  RelAlly) ]

    it "every insertion order of the same causes yields the same store \
       \and the same overlay (all 120 permutations)" $ do
        let stores = map relationCauseStoreFromList (permutations sampleCauses)
        length stores `shouldBe` 120
        forM_ stores $ \s → do
            s `shouldBe` sampleStore
            relationCauseStoreOverlay s
                `shouldBe` relationCauseStoreOverlay sampleStore

    it "a re-add of the same (pair, owner) merges by hostile > neutral > \
       \ally in EITHER order — never last-write-wins — for all nine value \
       \combinations" $
        forM_ [ (a, b) | a ← allRelations, b ← allRelations ] $ \(a, b) → do
            let expected = moreSevere' a b
                add r = addStoredCause (cause "o" "t_a" "t_b" r)
                forward  = add b (add a emptyRelationCauseStore)
                backward = add a (add b emptyRelationCauseStore)
            map lrcRelation (relationCauseStoreOverlay forward)
                `shouldBe` [expected]
            backward `shouldBe` forward

    it "merging does not deduplicate an identity ACROSS pairs: one owner \
       \holds a cause on each of two pairs" $ do
        let s = relationCauseStoreFromList
                    [ cause "o" "a_t" "b_t" RelHostile
                    , cause "o" "a_t" "c_t" RelAlly ]
        map lrcRelation (relationCauseStoreOverlay s)
            `shouldBe` [RelHostile, RelAlly]

    it "installs into the policy exactly as FTS-1's own addRelationCause \
       \does, for every insertion order" $ do
        let viaPolicy cs = foldr
                (\c → addRelationCause (lrcCause c) (lrcSource c)
                          (lrcTarget c) (lrcRelation c))
                emptyFactionPolicy cs
        forM_ (permutations sampleCauses) $ \cs →
            withLiveCauses
                (relationCauseStoreOverlay (relationCauseStoreFromList cs))
                emptyFactionPolicy
                `shouldBe` viaPolicy sampleCauses
        -- And the pair with two owners really holds two causes.
        Map.size (liveCausesFor (withLiveCauses
                (relationCauseStoreOverlay sampleStore) emptyFactionPolicy)
                (tg "raid_a") (tg "raid_b"))
            `shouldBe` 2

    describe "remove-by-owner" $ do
        it "removes that owner's causes on EVERY pair and no other \
           \owner's" $ do
            let s = removeCausesByOwner (relationCauseId "order:attack_1")
                                        sampleStore
            owners s `shouldBe`
                [ "report:incident_7", "report:incident_7"
                , "diplomacy:truce_2" ]
            -- The other owner's cause on the SAME pair survived.
            [ lrcRelation c | c ← relationCauseStoreOverlay s
                            , factionTagText (lrcSource c) ≡ "raid_a"
                            , factionTagText (lrcTarget c) ≡ "raid_b" ]
                `shouldBe` [RelHostile]

        it "an owner that holds nothing, and a repeated removal, are \
           \no-ops" $ do
            let gone = relationCauseId "order:attack_1"
                once = removeCausesByOwner gone sampleStore
            removeCausesByOwner (relationCauseId "nobody") sampleStore
                `shouldBe` sampleStore
            removeCausesByOwner gone once `shouldBe` once
            removeCausesByOwner gone emptyRelationCauseStore
                `shouldBe` emptyRelationCauseStore

        it "removing every owner empties the store" $
            foldr removeCausesByOwner sampleStore
                (map relationCauseId
                    [ "order:attack_1", "report:incident_7"
                    , "diplomacy:truce_2" ])
                `shouldBe` emptyRelationCauseStore

        it "an add followed by its own removal returns the EARLIER store \
           \exactly — a pair never lingers with no cause" $ do
            let c = cause "fresh" "new_a" "new_b" RelHostile
            removeCausesByOwner (lrcCause c)
                (addStoredCause c sampleStore) `shouldBe` sampleStore

    describe "clear-pair-by-owner" $ do
        let order = relationCauseId "order:attack_1"
            cleared = clearPairByOwner order (tg "raid_a") (tg "raid_b")
                                       sampleStore
        it "removes exactly one owner's cause on exactly one ordered pair" $
            owners cleared `shouldBe`
                [ "report:incident_7", "order:attack_1"
                , "report:incident_7", "diplomacy:truce_2" ]

        it "leaves the REVERSE direction, the owner's other pair and \
           \another owner's cause on that pair untouched" $ do
            -- order:attack_1's cause on raid_a→farmers survived.
            [ factionTagText (lrcTarget c)
                | c ← relationCauseStoreOverlay cleared
                , lrcCause c ≡ order ] `shouldBe` ["farmers"]
            -- The reverse pair raid_b→raid_a is untouched.
            clearPairByOwner (relationCauseId "diplomacy:truce_2")
                (tg "raid_a") (tg "raid_b") sampleStore
                `shouldBe` sampleStore

        it "is a no-op when repeated or when nothing matches" $ do
            clearPairByOwner order (tg "raid_a") (tg "raid_b") cleared
                `shouldBe` cleared
            clearPairByOwner order (tg "zz") (tg "yy") sampleStore
                `shouldBe` sampleStore

    describe "the text boundary" $ do
        it "rejects a malformed tag, naming the role and the cause, and \
           \never returns a partial cause" $
            forM_ [ "", " ", "a b", "a\tb", "a\nb", "tab\t" ] $ \bad → do
                let src = mkLiveRelationCause (relationCauseId "c")
                              bad "ok_tag" RelHostile
                    tgt = mkLiveRelationCause (relationCauseId "c")
                              "ok_tag" bad RelHostile
                case src of
                    Left e → do
                        e `shouldSatisfy` T.isInfixOf "source"
                        e `shouldSatisfy` T.isInfixOf "'c'"
                    Right _ → expectationFailure
                        ("accepted a malformed source " <> show bad)
                case tgt of
                    Left e → e `shouldSatisfy` T.isInfixOf "target"
                    Right _ → expectationFailure
                        ("accepted a malformed target " <> show bad)

        it "accepts a valid tag nobody declared — the namespace is open \
           \(D-12), so a runtime-minted tag is a legitimate cause end" $
            fmap lrcSource (mkLiveRelationCause (relationCauseId "c")
                                "minted:team#7" "other" RelHostile)
                `shouldBe` Right (tg "minted:team#7")
  where
    allRelations = [RelAlly, RelNeutral, RelHostile]
    owners s = [ relationCauseIdText (lrcCause c)
               | c ← relationCauseStoreOverlay s ]
    -- An independent statement of the reduction, so the assertion does
    -- not borrow the rule it is checking.
    moreSevere' a b = if sev a ≥ sev b then a else b
    sev r = case r of { RelAlly → 0 ∷ Int; RelNeutral → 1; RelHostile → 2 }
