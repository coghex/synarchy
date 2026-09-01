{-# LANGUAGE Strict #-}
-- | "Location significant contents" (#917): the guaranteed
--   significant-item obligations a placed location owes, and the
--   COMPOUND clearance predicate that waits on them beside #916's
--   encounter condition.
--
--   Pure, no engine — the same fixture style as
--   'Test.Headless.Location.Instance', which owns identity/lifecycle
--   and whose encounter coverage this sits beside. The IO-level
--   coverage (a real pickup latching through the ground boundary, and
--   the discovery tick promoting) lives in
--   'Test.Headless.World.LocationDiscovery'; the wire round trip lives
--   in 'Test.Headless.World.Save.Contract' and
--   'Test.Headless.World.Save.Compat'.
module Test.Headless.Location.SignificantContents (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import Location.Bounds (RelBounds(..))
import Location.Instance
import Location.Types
    ( LocationContent(..), LocationDef(..), LocationNaming(..) )
import Language.Semantic.Types (ConceptId(..))
import World.Chunk.Types (ChunkCoord(..))

-- * Fixtures ---------------------------------------------------------

testNaming ∷ LocationNaming
testNaming = LocationNaming [ConceptId "KEEP"] [ConceptId "ASH"]

-- | One authored content entry, every field named explicitly on
--   purpose: 'LocationContent' 's fields are STRICT, so a field added
--   to it fails this fixture to compile rather than letting a wrong
--   default ride through unnoticed.
content ∷ Text → Text → Int → Bool → LocationContent
content kind cid count significant = LocationContent
    { lconKind        = kind
    , lconId          = cid
    , lconCount       = count
    , lconPosition    = Nothing
    , lconFaction     = Nothing
    , lconRolls       = 1
    , lconCountRange  = Nothing
    , lconClearance   = Nothing
    , lconSignificant = significant
    }

encounterContent ∷ Int → Int → LocationContent
encounterContent lo hi = (content "unit" "nomad_primitive" 1 False)
    { lconFaction    = Just "hostile"
    , lconCountRange = Just (lo, hi)
    , lconClearance  = Just "death_only"
    }

mkDef ∷ Text → [LocationContent] → LocationDef
mkDef lid contents = LocationDef
    { ldId         = lid
    , ldLabel      = "Fixture"
    , ldType       = "ruin"
    , ldBuilder    = "room_small"
    , ldAnchor     = []
    , ldMaxCount   = 0
    , ldMinSpacing = 0
    , ldContents   = contents
    , ldBounds     = RelBounds (-2) (-2) 2 2
    , ldMapIcon    = Nothing
    , ldNaming     = testNaming
    }

-- | A def owing TWO significant items beside two incidental entries —
--   two so that one latch can never stand in for the other, and the
--   incidental ones so a rule that counted "every item entry" or "every
--   spawned thing" would fail here.
twoSignificantDef ∷ LocationDef
twoSignificantDef = mkDef "two_significant"
    [ content "loot_table" "ruin_common" 1 False
    , content "item" "rations" 1 False
    , content "item" "processing_unit" 2 True
    ]

-- | The shipped shape: one significant item beside an encounter, i.e.
--   both halves of the compound predicate authored at once.
bothConditionsDef ∷ LocationDef
bothConditionsDef = mkDef "both_conditions"
    [ content "loot_table" "ruin_common" 1 False
    , encounterContent 0 3
    , content "item" "processing_unit" 1 True
    ]

-- | Encounter only — #916's shape, unchanged.
encounterOnlyDef ∷ LocationDef
encounterOnlyDef = mkDef "encounter_only" [ encounterContent 1 1 ]

-- | Significant items only, no encounter.
significantOnlyDef ∷ LocationDef
significantOnlyDef = mkDef "significant_only"
    [ content "item" "processing_unit" 1 True ]

-- | Neither condition: incidental salvage and nothing else.
noConditionDef ∷ LocationDef
noConditionDef = mkDef "no_condition"
    [ content "loot_table" "ruin_common" 1 False
    , content "item" "rations" 3 False
    ]

iid ∷ LocationInstanceId
iid = LocationInstanceId 1

-- | A single-instance table holding @def@, built with @seed@ so an
--   encounter's one-time roll is pinned.
tableFor ∷ Word64 → LocationDef → LocationInstances
tableFor seed def =
    let inst = either (error ∘ show) id
            (newLocationInstanceWithSeed seed Nothing iid (ChunkCoord 0 0) def)
    in emptyLocationInstances
        { lisNextId = unLocationInstanceId iid + 1
        , lisById   = HM.singleton iid inst
        }

instOf ∷ LocationInstances → LocationInstance
instOf = fromMaybe (error "fixture instance missing") ∘ lookupLocationInstance iid

-- | Bind every obligation to a physical item id derived from its slot,
--   as a real content spawn does.
spawnAll ∷ LocationInstances → LocationInstances
spawnAll lis = foldl' bind lis (map lsiSlot (liSignificant (instOf lis)))
  where
    bind acc slot = fromMaybe acc
        (registerLocationSignificantSpawn iid slot (itemIdFor slot) acc)

itemIdFor ∷ Int → Word64
itemIdFor slot = 9000 + fromIntegral slot

-- | Make the location visible, the way sight-based discovery does.
discover ∷ LocationInstances → LocationInstances
discover lis = fromMaybe lis (setLocationLifecycle iid LifecycleDiscovered lis)

-- | Complete the encounter, the way the clearance tick does.
completeEncounter ∷ LocationInstances → LocationInstances
completeEncounter lis = fromMaybe lis (markLocationEncounterCleared iid lis)

takeItem ∷ Word64 → LocationInstances → LocationInstances
takeItem itemId lis = fromMaybe lis (latchLocationSignificantTaken itemId lis)

-- | The 'newLocationInstanceWithSeed' seed that rolls a ZERO-nomad
--   encounter for 'bothConditionsDef' — 'Test.Headless.Location.Instance'
--   pins the roll mapping itself; this only needs one of each outcome.
zeroSeed, positiveSeed ∷ Word64
zeroSeed = seedRolling "zero" (≡ 0)
positiveSeed = seedRolling "positive" (> 0)

-- | The first seed in a bounded scan whose one-time encounter roll
--   satisfies @p@. Bounded and loud rather than a lazy infinite search:
--   if a future change to the roll made one of these outcomes
--   unreachable, an unbounded scan would hang the whole suite instead
--   of naming what it could not find.
seedRolling ∷ Text → (Int → Bool) → Word64
seedRolling label p = case filter (p ∘ rolledFor) [0 .. 63] of
    (s:_) → s
    []    → error (T.unpack ("no seed in [0, 63] rolls a " <> label
                                <> " encounter for this fixture"))

rolledFor ∷ Word64 → Int
rolledFor s = maybe (-1) leRolledCount
    (liEncounter (instOf (tableFor s bothConditionsDef)))

-- * Spec -------------------------------------------------------------

spec ∷ Spec
spec = describe "Location significant contents (#917)" $ do

    describe "authored obligations" $ do
        it "creates one obligation per significant item at PLACEMENT, \
           \before anything has spawned" $ do
            let entries = liSignificant (instOf (tableFor 0 twoSignificantDef))
            map lsiSlot entries `shouldBe` [1, 2]
            map lsiItemDefName entries
                `shouldBe` ["processing_unit", "processing_unit"]
            -- The whole point of fixing cardinality at placement: the
            -- obligation exists with NO item bound, so the loot
            -- condition is already incomplete.
            map lsiInstanceId entries `shouldBe` [Nothing, Nothing]
            map lsiTaken entries `shouldBe` [False, False]

        it "ignores incidental contents entirely — loot tables and \
           \unflagged fixed items alike" $ do
            liSignificant (instOf (tableFor 0 noConditionDef)) `shouldBe` []
            -- 'noConditionDef' authors three unflagged `rations`; a rule
            -- that keyed on `kind: item` rather than on the flag would
            -- produce three obligations here.
            liSignificant (instOf (tableFor 0 encounterOnlyDef)) `shouldBe` []

        it "refuses a significant flag on a non-item content kind, so a \
           \loot draw can never become an obligation" $ do
            -- The YAML boundary rejects this shape outright
            -- ('Engine.Asset.YamlLocations'); this is the belt-and-braces
            -- half, at the one place obligations are derived, for a def
            -- injected some other way.
            let sneaky = mkDef "sneaky"
                    [ content "loot_table" "ruin_common" 1 True
                    , content "unit" "nomad_primitive" 1 True
                    , content "building" "cargo_hold_S" 1 True
                    ]
            significantItemsFromDef sneaky `shouldBe` []

        it "expands an entry's count into one obligation each" $
            map lsiSlot (significantItemsFromDef
                (mkDef "many" [ content "item" "processing_unit" 3 True ]))
                `shouldBe` [1, 2, 3]

    describe "the compound clearance predicate" $ do
        it "reports which conditions a location actually authors" $ do
            let authored def = ( isJust (locationEncounterCondition
                                            (instOf (tableFor 0 def)))
                               , isJust (locationSignificantCondition
                                            (instOf (tableFor 0 def)))
                               , locationAuthorsClearance
                                     (instOf (tableFor 0 def)) )
            authored bothConditionsDef   `shouldBe` (True,  True,  True)
            authored encounterOnlyDef    `shouldBe` (True,  False, True)
            authored significantOnlyDef  `shouldBe` (False, True,  True)
            authored noConditionDef      `shouldBe` (False, False, False)

        it "leaves a location with a completed encounter but an untaken \
           \significant item UNCLEARED" $ do
            let lis = completeEncounter
                        (spawnAll (discover (tableFor positiveSeed
                                                bothConditionsDef)))
                inst = instOf lis
            locationEncounterCondition inst `shouldBe` Just True
            locationSignificantCondition inst `shouldBe` Just False
            locationClearanceSatisfied inst `shouldBe` False
            resolveLocationClearance iid lis `shouldBe` Nothing
            liLifecycle inst `shouldBe` LifecycleDiscovered

        it "leaves a ZERO-nomad location uncleared until its guaranteed \
           \item is taken" $ do
            let lis = spawnAll (discover (tableFor zeroSeed bothConditionsDef))
                inst = instOf lis
            -- The encounter half is satisfied from birth…
            fmap leRolledCount (liEncounter inst) `shouldBe` Just 0
            locationEncounterCondition inst `shouldBe` Just True
            -- …and the location is still not cleared, which is exactly
            -- what requirement 6 authors the significant item for.
            locationClearanceSatisfied inst `shouldBe` False
            liLifecycle inst `shouldBe` LifecycleDiscovered
            resolveLocationClearance iid lis `shouldBe` Nothing

        it "leaves a location with every item taken but a live encounter \
           \UNCLEARED" $ do
            let lis = takeItem (itemIdFor 1)
                        (spawnAll (discover (tableFor positiveSeed
                                                bothConditionsDef)))
                inst = instOf lis
            locationSignificantCondition inst `shouldBe` Just True
            locationEncounterCondition inst `shouldBe` Just False
            locationClearanceSatisfied inst `shouldBe` False
            resolveLocationClearance iid lis `shouldBe` Nothing

        it "needs EVERY significant item, not just one" $ do
            let one = takeItem (itemIdFor 1)
                        (spawnAll (discover (tableFor 0 twoSignificantDef)))
            locationSignificantCondition (instOf one) `shouldBe` Just False
            locationClearanceSatisfied (instOf one) `shouldBe` False
            resolveLocationClearance iid one `shouldBe` Nothing
            let both = takeItem (itemIdFor 2) one
            locationSignificantCondition (instOf both) `shouldBe` Just True
            resolveLocationClearance iid both `shouldSatisfy` isJust

        it "refuses to count an obligation that is marked taken but \
           \names no item — the one shape that could otherwise clear a \
           \location with nothing ever spawned" $ do
            -- No engine path produces this:
            -- 'latchLocationSignificantTaken' matches on a bound id, so
            -- an unbound obligation is unreachable. It is exactly the
            -- shape the session provenance rules cannot see either —
            -- there is no id for them to resolve — which is why the
            -- predicate itself has to refuse it.
            let forged = adjustLocationInstance iid
                    (\i → i { liSignificant =
                        [ e { lsiTaken = True, lsiInstanceId = Nothing }
                        | e ← liSignificant i ] })
                    (discover (tableFor 0 significantOnlyDef))
            map lsiTaken (liSignificant (instOf forged)) `shouldBe` [True]
            locationSignificantCondition (instOf forged) `shouldBe` Just False
            locationClearanceSatisfied (instOf forged) `shouldBe` False
            resolveLocationClearance iid forged `shouldBe` Nothing

        it "keeps an UNSPAWNED obligation incomplete, so an empty \
           \collection can never read as satisfied" $ do
            -- No spawnAll: the obligation exists with no item bound.
            let lis = discover (tableFor 0 significantOnlyDef)
            map lsiInstanceId (liSignificant (instOf lis)) `shouldBe` [Nothing]
            locationSignificantCondition (instOf lis) `shouldBe` Just False
            resolveLocationClearance iid lis `shouldBe` Nothing

        it "never clears a location that authors NO condition, rather \
           \than clearing it through an empty conjunction" $ do
            let lis = discover (tableFor 0 noConditionDef)
            locationClearanceSatisfied (instOf lis) `shouldBe` False
            locationDiscoveryLifecycle (instOf lis)
                `shouldBe` LifecycleDiscovered
            resolveLocationClearance iid lis `shouldBe` Nothing
            -- …and it is not polled forever either: the tick's own
            -- guard is 'locationAuthorsClearance'.
            locationAuthorsClearance (instOf lis) `shouldBe` False

        it "clears a single-condition location on that one condition" $ do
            let encOnly = completeEncounter
                            (discover (tableFor positiveSeed encounterOnlyDef))
            locationClearanceSatisfied (instOf encOnly) `shouldBe` True
            resolveLocationClearance iid encOnly `shouldSatisfy` isJust

            let sigOnly = takeItem (itemIdFor 1)
                            (spawnAll (discover
                                (tableFor 0 significantOnlyDef)))
            locationClearanceSatisfied (instOf sigOnly) `shouldBe` True
            resolveLocationClearance iid sigOnly `shouldSatisfy` isJust

    describe "promotion and feedback" $ do
        it "promotes exactly once, whichever condition is satisfied LAST" $ do
            -- Encounter last.
            let itemFirst = takeItem (itemIdFor 1)
                    (spawnAll (discover (tableFor positiveSeed
                                            bothConditionsDef)))
            resolveLocationClearance iid itemFirst `shouldBe` Nothing
            let encLast = fromMaybe (error "expected clearance")
                    (resolveLocationClearance iid
                        (completeEncounter itemFirst))
            liLifecycle (instOf encLast) `shouldBe` LifecycleCleared
            liClearEventEmitted (instOf encLast) `shouldBe` True
            -- The second call is the exactly-once proof: no further
            -- notice, however many ticks poll it.
            resolveLocationClearance iid encLast `shouldBe` Nothing

            -- Item last, same location shape, opposite order.
            let encFirst = completeEncounter
                    (spawnAll (discover (tableFor positiveSeed
                                            bothConditionsDef)))
            resolveLocationClearance iid encFirst `shouldBe` Nothing
            let itemLast = fromMaybe (error "expected clearance")
                    (resolveLocationClearance iid
                        (takeItem (itemIdFor 1) encFirst))
            liLifecycle (instOf itemLast) `shouldBe` LifecycleCleared
            resolveLocationClearance iid itemLast `shouldBe` Nothing

        it "keeps a completion reached before discovery PRIVATE, then \
           \announces it once when the location is seen" $ do
            let hidden = takeItem (itemIdFor 1)
                    (spawnAll (tableFor 0 significantOnlyDef))
            -- Satisfied, but still unknown: nothing promotes and no
            -- notice is owed yet.
            locationClearanceSatisfied (instOf hidden) `shouldBe` True
            liLifecycle (instOf hidden) `shouldBe` LifecycleUnknown
            liClearEventEmitted (instOf hidden) `shouldBe` False
            resolveLocationClearance iid hidden `shouldBe` Nothing
            -- Sight lands it straight on cleared, and the deferred
            -- notice is spent on that same edge.
            locationDiscoveryLifecycle (instOf hidden)
                `shouldBe` LifecycleCleared
            let seen = fromMaybe (error "expected discovery promotion")
                    (setLocationLifecycle iid
                        (locationDiscoveryLifecycle (instOf hidden)) hidden)
                announced = fromMaybe (error "expected deferred clearance")
                    (resolveLocationClearance iid seen)
            liLifecycle (instOf announced) `shouldBe` LifecycleCleared
            liClearEventEmitted (instOf announced) `shouldBe` True
            resolveLocationClearance iid announced `shouldBe` Nothing

        it "starts the notice SPENT only for a location born already \
           \satisfied, which is #916's zero-roll rule generalized" $ do
            -- A zero-roll encounter with no significant item: nobody
            -- cleared it, so discovering it must not announce one.
            -- 'alwaysZeroEncounterDef' rolls zero for EVERY seed (its
            -- authored range is 0..0), so this is a property of the
            -- shape rather than of a lucky seed.
            liClearEventEmitted (instOf (tableFor 7 alwaysZeroEncounterDef))
                `shouldBe` True
            -- The same def once a significant item is authored beside
            -- it: outstanding, so the notice is unspent and earned.
            liClearEventEmitted
                (instOf (tableFor zeroSeed bothConditionsDef))
                `shouldBe` False
            -- And a location that authors nothing at all starts unspent
            -- and simply never clears.
            liClearEventEmitted (instOf (tableFor 0 noConditionDef))
                `shouldBe` False

    describe "the taken latch" $ do
        it "latches on the item's own physical identity, and only that \
           \one" $ do
            let lis = takeItem (itemIdFor 1)
                        (spawnAll (tableFor 0 twoSignificantDef))
            map (\e → (lsiSlot e, lsiTaken e))
                (sortOn lsiSlot (liSignificant (instOf lis)))
                `shouldBe` [(1, True), (2, False)]

        it "is a no-op for an item no obligation owns — an ordinary \
           \salvage pickup costs nothing" $ do
            latchLocationSignificantTaken 4242
                (spawnAll (tableFor 0 twoSignificantDef)) `shouldBe` Nothing

        it "is a no-op for an obligation that is already taken, so \
           \repeated pickup/drop activity can neither reset it nor \
           \re-promote the location" $ do
            let once = takeItem (itemIdFor 1)
                        (spawnAll (tableFor 0 significantOnlyDef))
            latchLocationSignificantTaken (itemIdFor 1) once
                `shouldBe` Nothing
            -- Nothing anywhere writes False: the item may now be
            -- dropped, transferred, consumed or destroyed.
            map lsiTaken (liSignificant (instOf once)) `shouldBe` [True]

        it "never latches an UNSPAWNED obligation, whatever id is \
           \offered" $
            latchLocationSignificantTaken (itemIdFor 1)
                (tableFor 0 significantOnlyDef) `shouldBe` Nothing

    describe "spawn registration" $ do
        it "binds one item to one slot" $ do
            let bound = fromMaybe (error "expected a binding")
                    (registerLocationSignificantSpawn iid 1 777
                        (tableFor 0 twoSignificantDef))
            map lsiInstanceId (liSignificant (instOf bound))
                `shouldBe` [Just 777, Nothing]

        it "is WRITE-ONCE: a retried spawn cannot repoint a bound slot \
           \and orphan the item it first named" $ do
            let bound = fromMaybe (error "expected a binding")
                    (registerLocationSignificantSpawn iid 1 777
                        (tableFor 0 twoSignificantDef))
            registerLocationSignificantSpawn iid 1 888 bound
                `shouldBe` Nothing
            map lsiInstanceId (liSignificant (instOf bound))
                `shouldBe` [Just 777, Nothing]

        it "refuses an unknown slot and an unknown instance" $ do
            let lis = tableFor 0 twoSignificantDef
            registerLocationSignificantSpawn iid 3 777 lis `shouldBe` Nothing
            registerLocationSignificantSpawn (LocationInstanceId 99) 1 777 lis
                `shouldBe` Nothing

    describe "decoded-table validation" $ do
        it "accepts a well-formed table" $
            locationSignificantItemErrors
                (spawnAll (tableFor 0 twoSignificantDef)) `shouldBe` []

        it "rejects a duplicated slot within one instance" $ do
            let broken = adjustLocationInstance iid
                    (\i → i { liSignificant =
                        [ e { lsiSlot = 1 } | e ← liSignificant i ] })
                    (tableFor 0 twoSignificantDef)
            locationSignificantItemErrors broken
                `shouldBe` [ "location instance #1 declares significant \
                             \slot 1 more than once" ]

        it "rejects an obligation marked taken that names no item" $ do
            let broken = adjustLocationInstance iid
                    (\i → i { liSignificant =
                        [ (head' (liSignificant i))
                            { lsiTaken = True, lsiInstanceId = Nothing } ] })
                    (tableFor 0 significantOnlyDef)
            locationSignificantItemErrors broken
                `shouldBe` [ "location instance #1 significant slot 1 is \
                             \marked taken but names no item instance" ]

        it "rejects one physical item owned by two obligations" $ do
            let broken = adjustLocationInstance iid
                    (\i → i { liSignificant =
                        [ e { lsiInstanceId = Just 555 } | e ← liSignificant i ] })
                    (tableFor 0 twoSignificantDef)
            locationSignificantItemErrors broken
                `shouldBe` [ "significant item instance 555 is owned by more \
                             \than one location obligation: #1 slot 1, \
                             \#1 slot 2" ]

-- | An encounter-only def whose authored range is @0..0@, so it rolls
--   zero occupants under every seed. Kept distinct from
--   'encounterOnlyDef' (range @1..1@) so the born-already-satisfied
--   assertion above is about the SHAPE and not about which seed the
--   fixture happened to draw.
alwaysZeroEncounterDef ∷ LocationDef
alwaysZeroEncounterDef = mkDef "encounter_only_zero" [ encounterContent 0 0 ]

-- | The first element of a fixture list, failing loudly rather than
--   partially. Every use here is over a list this module built itself.
head' ∷ [α] → α
head' (x:_) = x
head' []    = error "significant-contents fixture list is empty"
