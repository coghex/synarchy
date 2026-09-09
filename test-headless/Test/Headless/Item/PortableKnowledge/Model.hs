-- | The four states, the two independently-stamped observations, and
--   what an observation does and does not touch (#2512 requirements
--   1 and 3).
--
--   Pure: "Item.Knowledge" is a model with no IO in it at all, so these
--   run against the real production functions with no engine and no
--   refs.
module Test.Headless.Item.PortableKnowledge.Model (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Item.Knowledge
import Item.Types (ItemInstance(..), itemTotalWeight)
import Test.Headless.Item.PortableKnowledge.Fixture

spec ∷ Spec
spec = do
    describe "the four states" $ do
        it "distinguishes never-inspected, weight-only, known-empty and \
           \known-contents, with a DISTINCT id for each -- so no \
           \consumer can render an uninspected crate as an empty one, \
           \or a hefted one as opened" $ do
            let weighed  = observePortableWeight testItems 10 crate
                              emptyPortableKnowledge
                opened   = observePortableContents testItems 20 crate
                              emptyPortableKnowledge
                emptied  = observePortableContents testItems 20 crateEmpty
                              emptyPortableKnowledge
            portableState crateId emptyPortableKnowledge
                `shouldBe` NeverInspected
            portableState crateId weighed `shouldBe` WeightOnly
            portableState crateId emptied `shouldBe` KnownEmpty
            portableState crateId opened  `shouldBe` KnownContents
            let ids = map portableKnowledgeStateId
                          [NeverInspected, WeightOnly, KnownEmpty, KnownContents]
            ids `shouldBe` ["unknown", "weight-only", "empty", "known"]
            length (nubOrd ids) `shouldBe` 4

        it "reads a record carrying NEITHER observation as \
           \never-inspected -- a present-but-empty record is not a \
           \fourth state, and is not 'empty'" $
            portableRecordState (Just (PortableRecord Nothing Nothing))
                `shouldBe` NeverInspected

        it "keeps known-empty and never-inspected apart at the RECORD \
           \level: an observed empty list is a fact, its absence is not" $ do
            portableRecordState
                (Just (PortableRecord Nothing (Just (ContentsObservation [] 5))))
                `shouldBe` KnownEmpty
            portableRecordState Nothing `shouldBe` NeverInspected

    describe "the two observation stamps" $ do
        -- The exact sequence the issue's own acceptance names, corrected
        -- by the cross-agent review: an OPEN refreshes both stamps
        -- (requirement 3 records the weight too), and only a later
        -- WEIGH can drive them apart.
        it "weigh at t1, open at t2, weigh again at t3: the open moves \
           \BOTH stamps to t2, and the second weigh moves only \
           \weighedAt, leaving revealedAt and the remembered contents \
           \exactly where the open left them" $ do
            let k1 = observePortableWeight   testItems 100 crate
                         emptyPortableKnowledge
                k2 = observePortableContents testItems 200 crate k1
                k3 = observePortableWeight   testItems 300 crate k2
                at k = lookupPortable crateId k
            (woAt <$> (at k1 ⌦ prWeight)) `shouldBe` Just 100
            (at k1 ⌦ prContents) `shouldBe` Nothing

            (woAt <$> (at k2 ⌦ prWeight))   `shouldBe` Just 200
            (coAt <$> (at k2 ⌦ prContents)) `shouldBe` Just 200

            (woAt <$> (at k3 ⌦ prWeight))   `shouldBe` Just 300
            (coAt <$> (at k3 ⌦ prContents)) `shouldBe` Just 200
            (coItems <$> (at k3 ⌦ prContents)) `shouldBe` Just [kit]
            portableState crateId k3 `shouldBe` KnownContents

        it "records the crate's WHOLE recursive weight -- its own mass, \
           \its fill and everything nested -- not just its empty weight" $ do
            let k = observePortableWeight testItems 5 crate
                        emptyPortableKnowledge
                w = woWeight <$> (lookupPortable crateId k ⌦ prWeight)
            w `shouldBe` Just (itemTotalWeight testItems crate)
            -- Distinct from the shallow value, so a regression to
            -- 'iiWeight' fails rather than coincidentally passing.
            w `shouldNotBe` Just (iiWeight crate)

    describe "what an observation touches" $ do
        it "remembers full instance COPIES of the contents, field for \
           \field, rather than ids to resolve later" $ do
            let k = observePortableContents testItems 7 crate
                        emptyPortableKnowledge
            (coItems <$> (lookupPortable crateId k ⌦ prContents))
                `shouldBe` Just [kit]

        it "gives a NESTED container no record of its own: the kit \
           \inside an observed crate rides along in the copy, but \
           \nobody opened IT, so it stays never-inspected" $ do
            let k = observePortableContents testItems 7 crate
                        emptyPortableKnowledge
            portableState kitId k     `shouldBe` NeverInspected
            portableState bandageId k `shouldBe` NeverInspected
            HM.keys (pkRecords k)     `shouldBe` [crateId]

        it "is a SNAPSHOT, not a view: after the live crate is emptied \
           \and damaged, the untouched record still reports the old \
           \contents and the old weight, and only a fresh observation \
           \moves it" $ do
            let k       = observePortableContents testItems 7 crate
                              emptyPortableKnowledge
                emptied = crate { iiContents = [], iiCondition = 1 }
            -- The live item really did change, so a record that tracked
            -- it would have to disagree with these assertions.
            itemTotalWeight testItems emptied
                `shouldNotBe` itemTotalWeight testItems crate
            (coItems <$> (lookupPortable crateId k ⌦ prContents))
                `shouldBe` Just [kit]
            (woWeight <$> (lookupPortable crateId k ⌦ prWeight))
                `shouldBe` Just (itemTotalWeight testItems crate)
            portableState crateId k `shouldBe` KnownContents
            -- Re-observing the CHANGED item is what updates it, and only
            -- then.
            let k' = observePortableContents testItems 9 emptied k
            portableState crateId k' `shouldBe` KnownEmpty
            (woWeight <$> (lookupPortable crateId k' ⌦ prWeight))
                `shouldBe` Just (itemTotalWeight testItems emptied)

        it "a weight refresh preserves an existing contents observation \
           \AND its state -- hefting a crate you already opened tells \
           \you nothing new about what is in it" $ do
            let k  = observePortableContents testItems 7 crate
                         emptyPortableKnowledge
                k' = observePortableWeight testItems 8
                         (crate { iiContents = [] }) k
            portableState crateId k' `shouldBe` KnownContents
            (coItems <$> (lookupPortable crateId k' ⌦ prContents))
                `shouldBe` Just [kit]

        it "an OPEN replaces the whole record rather than merging: the \
           \contents observed now are the contents remembered, with no \
           \trace of an earlier look" $ do
            let k  = observePortableContents testItems 7 crate
                         emptyPortableKnowledge
                k' = observePortableContents testItems 9 crateEmpty k
            portableState crateId k' `shouldBe` KnownEmpty
            (coItems <$> (lookupPortable crateId k' ⌦ prContents))
                `shouldBe` Just []

    describe "forgetting and scrubbing" $ do
        it "forgetting drops the record entirely -- afterwards the crate \
           \reads as unknown again, not as empty" $ do
            let k = observePortableContents testItems 7 crate
                        emptyPortableKnowledge
            portableState crateId (forgetPortable crateId k)
                `shouldBe` NeverInspected

        it "retain keeps exactly the records whose instance is live, and \
           \reports the rest rather than dropping them silently" $ do
            let k = observePortableWeight testItems 1 loose
                        (observePortableContents testItems 7 crate
                             emptyPortableKnowledge)
                live = HM.keysSet (HM.fromList [(crateId, ())])
            knownPortableIds (retainPortables live k) `shouldBe` [crateId]
            prunedPortableIds live k `shouldBe` [looseId]

nubOrd ∷ Eq a ⇒ [a] → [a]
nubOrd = foldr (\x acc → x : filter (≢ x) acc) []
