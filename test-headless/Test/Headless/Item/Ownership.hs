{-# LANGUAGE Strict #-}
-- | The nested ownership-move boundary (#2487, epic #1231 PLC-4).
--
--   "Item.Ownership" is pure, so every case here drives the real policy
--   directly — no engine, no managers, no stand-in re-implementation of
--   a capacity rule. Weight comes from the production 'itemTotalWeight'
--   applied to a real 'ItemManager' throughout, so a fill that costs
--   kilograms costs them here too.
--
--   __Every capacity case is mutation-tested.__ "This is refused"
--   proves nothing on its own — a boundary that refused everything
--   would pass it. So each capacity case names the ONE bound it is
--   about, loosens it by exactly the margin the guard was short, and
--   asserts the verdict FLIPS. That doubles as the exact-limit
--   coverage: the loosened bound is the inclusive upper bound
--   @Unit.Transfer.fits@ uses.
--
--   __Every fixture weight and bulk is a dyadic rational__ (halves,
--   quarters, eighths). Sums of those are exact in 'Float', so a
--   boundary case that should land exactly ON a capacity does, rather
--   than one ulp either side of it.
--
--   Gate: @cabal test synarchy-test-headless
--   --test-options='--match "Item.Ownership"'@.
module Test.Headless.Item.Ownership (spec) where

import UPrelude
import Test.Hspec
import Data.List (nub, sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isAlphaNum, isLower)
import System.FilePath ((</>))
import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Asset.Handle (TextureHandle(..))
import Item.Types
    ( ItemContainer(..), ItemDef(..), ItemInstance(..), ItemManager(..)
    , ItemStorage(..), itemTotalWeight )
import Item.Ownership

-- * Fixture

-- | An instance with no storage of its own and no contents: the
--   ordinary item everything else is built from.
item ∷ Text → Word64 → Float → Float → ItemInstance
item name iid weight bulk = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = weight
    , iiSharpness   = 0
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just bulk
    , iiStorage     = Nothing
    }

-- | A container that DECLARES storage, holding these contents.
container ∷ Text → Word64 → Float → Float → (Float, Float)
          → [ItemInstance] → ItemInstance
container name iid weight bulk (wCap, bCap) contents =
    (item name iid weight bulk)
        { iiContents = contents
        , iiStorage  = Just (ItemStorage wCap bCap) }

-- | A shipped-style kit: it HOLDS items but authors no @storage:@, so
--   it declares no capacity at all. Every kit and toolbox in
--   @data/items@ is one today, which is why the fail-closed rule bites
--   immediately rather than theoretically.
kitOf ∷ Text → Word64 → Float → Float → [ItemInstance] → ItemInstance
kitOf name iid weight bulk contents =
    (item name iid weight bulk) { iiContents = contents }

-- | The canonical tree. Four levels below the owner —
--   @pack → crate → kit → bandage@ — with a shipped-style kit inside a
--   storage container, which is the arrangement the acceptance criteria
--   name.
--
--   @
--   pack  1   w 1     b 5   storage (100, 100)   recursive 5
--     crate 2   w 2     b 8   storage ( 50,  50)   recursive 4
--       kit   3   w 0.5   b 4   NO STORAGE          recursive 1.25
--         bandage 4  w 0.25  b 0.5
--         gauze   6  w 0.25  b 0.5
--         tape    7  w 0.25  b 0.5
--       pouch 5   w 0.25  b 2   storage ( 20,  20)  recursive 0.25
--       spanner 9 w 0.5   b 1
--   rock 10   w 3     b 1                          recursive 3
--   @
--
--   Capacities here are deliberately generous; every capacity case
--   tightens the ONE bound it is about with 'setStorage', so no test
--   depends on a bound another test tuned.
baseTree ∷ [ItemInstance]
baseTree =
    [ container "pack" 1 1.0 5.0 (100, 100)
        [ container "crate" 2 2.0 8.0 (50, 50)
            [ kitOf "first_aid_kit" 3 0.5 4.0
                [ item "bandage" 4 0.25 0.5
                , item "gauze"   6 0.25 0.5
                , item "tape"    7 0.25 0.5
                ]
            , container "pouch" 5 0.25 2.0 (20, 20) []
            , item "spanner" 9 0.5 1.0
            ]
        ]
    , item "rock" 10 3.0 1.0
    ]

-- | Item definitions for the fixture. Only @canteen@ is a fluid
--   container, and it is the one the filled-contents case needs:
--   'itemTotalWeight' charges @iiCurrentFill × icFillWeight@, so a
--   canteen holding 3 L weighs 3 kg more than the same empty case.
fixtureDefs ∷ ItemManager
fixtureDefs = ItemManager ∘ HM.fromList $
    [ (n, plainDef n) | n ←
        [ "pack", "crate", "first_aid_kit", "pouch", "spanner", "rock"
        , "bandage", "gauze", "tape", "wrench", "anvil", "slab"
        , "balloon", "legacy_tin", "tin", "suit", "feather" ] ]
    ⧺ [ ("canteen", (plainDef "canteen")
            { idContainer = Just ItemContainer
                { icCapacity = 5, icHolds = "water"
                , icFillWeight = 1.0, icDefaultFill = 0 } }) ]
  where
    plainDef n = ItemDef
        { idName = n, idDisplayName = n
        , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
        , idWeight = 0.5, idWeightSpec = Nothing, idBulk = 1.0
        , idStorage = Nothing, idKind = "misc"
        , idCategory = "Misc", idMake = "", idMaterial = ""
        , idQualitySpec = Nothing, idQualityTiers = []
        , idContainer = Nothing, idDefaultContents = [], idFood = Nothing
        , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
        , idBuffs = [], idInsulation = 0, idSourcePath = "test-fixture"
        }

-- | The production weigher, not a test approximation.
weigh ∷ ItemInstance → Float
weigh = itemTotalWeight fixtureDefs

-- | A scene over 'baseTree' with an unlimited root — the ground. Cases
--   about the tree's OWN limits use this, so a carrier limit can never
--   be what refused them.
scene ∷ OwnershipScene
scene = OwnershipScene
    { oscItems    = baseTree
    , oscOther    = []
    , oscCapacity = RootUnlimited
    , oscWeigh    = weigh
    }

-- | 'scene' over a different tree.
onTree ∷ [ItemInstance] → OwnershipScene
onTree items = scene { oscItems = items }

-- | Replace one instance's storage block, wherever it sits. The knob
--   every capacity case turns.
setStorage ∷ Word64 → Maybe ItemStorage → [ItemInstance] → [ItemInstance]
setStorage iid st = map go
  where
    go i | iiInstanceId i ≡ iid = i { iiStorage = st }
         | otherwise = i { iiContents = setStorage iid st (iiContents i) }

-- | Replace one instance's own external bulk, wherever it sits.
setBulk ∷ Word64 → Maybe Float → [ItemInstance] → [ItemInstance]
setBulk iid b = map go
  where
    go i | iiInstanceId i ≡ iid = i { iiBulk = b }
         | otherwise = i { iiContents = setBulk iid b (iiContents i) }

-- | Replace one instance's contents, wherever it sits.
setContents ∷ Word64 → [ItemInstance] → [ItemInstance] → [ItemInstance]
setContents iid cs = map go
  where
    go i | iiInstanceId i ≡ iid = i { iiContents = cs }
         | otherwise = i { iiContents = setContents iid cs (iiContents i) }

-- | Append a child to one container, wherever it sits.
addTo ∷ Word64 → ItemInstance → [ItemInstance] → [ItemInstance]
addTo iid child = map go
  where
    go i | iiInstanceId i ≡ iid = i { iiContents = iiContents i ⧺ [child] }
         | otherwise = i { iiContents = addTo iid child (iiContents i) }

-- | The instance ids of a whole forest, at every depth, in tree order.
idList ∷ [ItemInstance] → [Word64]
idList = concatMap (\i → iiInstanceId i : idList (iiContents i))

-- * Recognisable identities

packId, crateId, kitId, pouchId, bandageId, spannerId, rockId ∷ Word64
packId    = 1
crateId   = 2
kitId     = 3
bandageId = 4
pouchId   = 5
spannerId = 9
rockId    = 10

-- | An id belonging to nothing in the fixture.
absentId ∷ Word64
absentId = 777

-- | The candidate most cases insert: light, small, and in no tree.
gauzePad ∷ ItemInstance
gauzePad = item "gauze" 40 0.25 0.5

-- * Assertion helpers

refusedWith ∷ Either OwnershipRefusal a → OwnershipRefusal → Expectation
refusedWith (Left r)  expected = r `shouldBe` expected
refusedWith (Right _) expected =
    expectationFailure ("expected " <> show expected <> ", got acceptance")

accepted ∷ Either OwnershipRefusal a → Expectation
accepted (Right _) = pure ()
accepted (Left r)  =
    expectationFailure ("expected acceptance, got " <> show r)

-- | The refusal an expression produced, for the reachability roll-up.
reasonOf ∷ Either OwnershipRefusal a → Maybe OwnershipRefusal
reasonOf (Left r)  = Just r
reasonOf (Right _) = Nothing

-- * The suite

spec ∷ Spec
spec = do
    describe "identity and order (requirement 2)" $ do
        it "the instance that arrives is the exact value that left, \
           \descendants and authored order included" $
            case moveInstance scene kitId pouchId of
                Left r → expectationFailure (show r)
                Right mv → do
                    let original = findInstance kitId baseTree
                    Just (omInstance mv) `shouldBe` original
                    findInstance kitId (omItems mv) `shouldBe` original
                    (map iiDefName ∘ iiContents) ⊚ findInstance kitId (omItems mv)
                        `shouldBe` Just ["bandage", "gauze", "tape"]

        it "moves it OUT of its old parent and INTO the new one, \
           \altering no sibling subtree (requirement 8)" $
            case moveInstance scene kitId pouchId of
                Left r → expectationFailure (show r)
                Right mv → do
                    let ids = map iiInstanceId ∘ iiContents
                    ids ⊚ findInstance crateId (omItems mv)
                        `shouldBe` Just [pouchId, spannerId]
                    ids ⊚ findInstance pouchId (omItems mv)
                        `shouldBe` Just [kitId]
                    -- The untouched siblings are the SAME values, not
                    -- merely same-shaped ones.
                    findInstance spannerId (omItems mv)
                        `shouldBe` findInstance spannerId baseTree
                    findInstance rockId (omItems mv)
                        `shouldBe` findInstance rockId baseTree

        it "loses and duplicates nothing: the id set is preserved exactly" $
            case moveInstance scene kitId pouchId of
                Left r → expectationFailure (show r)
                Right mv → do
                    sort (idList (omItems mv)) `shouldBe` sort (idList baseTree)
                    nub (idList (omItems mv)) `shouldBe` idList (omItems mv)

    describe "destination weight capacity (requirement 3)" $ do
        let anvil  = item "anvil" 20 4.0 1.0
            at cap = onTree (setStorage pouchId
                              (Just (ItemStorage cap 20)) baseTree)
        it "refuses a candidate heavier than the target's \
           \isWeightCapacity" $
            insertInstance (at 3.0) pouchId anvil
                `refusedWith` OverTargetWeight
        it "and accepts it at exactly that capacity — the bound is \
           \inclusive, and one unit of slack flips the verdict" $
            accepted (insertInstance (at 4.0) pouchId anvil)
        it "counts what the target already holds at its full recursive \
           \weight: a FILLED canteen inside refuses what an empty one \
           \leaves room for" $ do
            let wrench = item "wrench" 24 0.5 1.5
                canteen f = (item "canteen" 21 0.25 1.0) { iiCurrentFill = f }
                with c = onTree (addTo pouchId c
                            (setStorage pouchId
                              (Just (ItemStorage 3.5 20)) baseTree))
            -- 0.25 case + 3 L × 1 kg/L. This is the production
            -- itemTotalWeight, so the fill is not a test-side fiction.
            weigh (canteen 3) `shouldBe` 3.25
            weigh (canteen 0) `shouldBe` 0.25
            insertInstance (with (canteen 3)) pouchId wrench
                `refusedWith` OverTargetWeight
            accepted (insertInstance (with (canteen 0)) pouchId wrench)

    describe "destination bulk capacity (requirement 3)" $ do
        let balloon = item "balloon" 22 0.25 7.0
            at cap  = onTree (setStorage pouchId
                               (Just (ItemStorage 20 cap)) baseTree)
        it "refuses a candidate bulkier than the target's \
           \isBulkCapacity" $
            insertInstance (at 6.0) pouchId balloon
                `refusedWith` OverTargetBulk
        it "and accepts it at exactly that capacity" $
            accepted (insertInstance (at 7.0) pouchId balloon)
        it "sums the direct children's own EXTERNAL bulk, never their \
           \recursive contents" $ do
            -- The crate's children are kit 4 + pouch 2 + spanner 1 = 7.
            -- The three dressings nested inside the kit are 0.5 each
            -- and must NOT be charged to the crate.
            let feather = item "feather" 23 0.125 0.25
                crateAt cap = onTree (setStorage crateId
                                (Just (ItemStorage 50 cap)) baseTree)
            insertInstance (crateAt 7.0) crateId feather
                `refusedWith` OverTargetBulk
            accepted (insertInstance (crateAt 7.25) crateId feather)
        it "and does NOT charge an ancestor for bulk added deeper (D-5): \
           \a crate exactly at its own bulk still accepts an insert into \
           \its pouch" $ do
            let exact = onTree (setStorage crateId
                                  (Just (ItemStorage 50 7.0)) baseTree)
            accepted (insertInstance exact pouchId (item "wrench" 24 0.5 1.5))

    describe "the weight-bearing ancestor chain (requirement 4)" $ do
        let slab = item "slab" 25 3.0 1.0
            -- The crate already holds kit 1.25 + pouch 0.25 + spanner
            -- 0.5 = 2, and the pack holds the crate at 4.
            crateAt cap = onTree (setStorage crateId
                                    (Just (ItemStorage cap 50)) baseTree)
            packAt cap  = onTree (setStorage packId
                                    (Just (ItemStorage cap 100)) baseTree)
        it "revalidates an ancestor ABOVE the immediate parent, which \
           \alone would have fit" $ do
            -- The pouch's own 20 kg has ample room for a 3 kg slab.
            accepted (insertInstance (crateAt 50) pouchId slab)
            insertInstance (crateAt 4.0) pouchId slab
                `refusedWith` OverAncestorWeight
        it "and accepts at exactly the ancestor's capacity" $
            accepted (insertInstance (crateAt 5.0) pouchId slab)
        it "reaches the WHOLE chain, not just the grandparent" $ do
            insertInstance (packAt 6.0) pouchId slab
                `refusedWith` OverAncestorWeight
            accepted (insertInstance (packAt 7.0) pouchId slab)

    describe "the carrier (requirement 5)" $ do
        let wrench    = item "wrench" 24 0.5 1.5
            feather   = item "feather" 26 0.125 0.25
            carrier c = scene { oscCapacity = RootLimited c }
        it "refuses a move whose net root load would exceed the owner's \
           \capacity" $ do
            sum (map weigh baseTree) `shouldBe` 8.0
            insertInstance (carrier 8.25) pouchId wrench
                `refusedWith` OverRootCapacity
        it "and accepts at exactly that capacity" $
            accepted (insertInstance (carrier 8.5) pouchId wrench)
        it "counts what the owner carries OUTSIDE the edited tree — \
           \equipment and accessories" $ do
            let worn c = (carrier c) { oscOther = [item "suit" 30 5.0 4.0] }
            insertInstance (worn 8.5) pouchId wrench
                `refusedWith` OverRootCapacity
            accepted (insertInstance (worn 13.5) pouchId wrench)
        it "reads a ZERO limited capacity as no room at all, never as \
           \unlimited (the Unit.Transfer.fits sense)" $ do
            insertInstance (carrier 0) pouchId feather
                `refusedWith` OverRootCapacity
            accepted (insertInstance (carrier 8.125) pouchId feather)
        it "while a root with NO capacity concept — the ground — passes \
           \no limit at all" $ do
            -- 15 kg clears every limit inside the tree (pouch 20, crate
            -- 50, pack 100), so the owner's own limit is the only thing
            -- that can refuse it — and the ground has none.
            let boulder = item "anvil" 32 15.0 1.0
            accepted (insertInstance scene pouchId boulder)
            insertInstance (carrier 8.0) pouchId boulder
                `refusedWith` OverRootCapacity
        it "and a relocation WITHIN one owner nets to zero, so a carrier \
           \already exactly at capacity can still rearrange" $ do
            accepted (moveInstance (carrier 8.0) spannerId pouchId)
            accepted (moveInstance (carrier 8.0) rockId pouchId)

    describe "absence fails closed (requirement 3)" $ do
        it "a target whose iiStorage is Nothing accepts no insert — \
           \every shipped kit is one" $ do
            iiStorage ⊚ findInstance kitId baseTree `shouldBe` Just Nothing
            insertInstance scene kitId gauzePad `refusedWith` TargetNotStorage
        it "an ANCESTOR with no storage refuses the insert below it for \
           \the same reason — a storage tin inside a shipped kit" $ do
            let nested = addTo kitId
                    (container "tin" 41 0.25 0.5 (10, 10) []) baseTree
            insertInstance (onTree nested) 41 gauzePad
                `refusedWith` AncestorNotStorage
            -- Give the kit a capacity and the same insert goes through:
            -- what refused it was the ABSENCE, not the shape of the tree.
            accepted (insertInstance
                        (onTree (setStorage kitId
                                   (Just (ItemStorage 10 10)) nested))
                        41 gauzePad)
        it "a candidate whose iiBulk is Nothing is never moved into \
           \storage" $ do
            insertInstance scene pouchId gauzePad { iiBulk = Nothing }
                `refusedWith` CandidateNoBulk
            accepted (insertInstance scene pouchId gauzePad)
        it "and a direct child already inside with no iiBulk refuses too \
           \— an unknowable sum is its own reason, not a bad candidate" $ do
            let legacy = addTo pouchId
                    ((item "legacy_tin" 42 0.25 0.5) { iiBulk = Nothing })
                    baseTree
            insertInstance (onTree legacy) pouchId gauzePad
                `refusedWith` ContentsBulkUnknown
            accepted (insertInstance
                        (onTree (setBulk 42 (Just 0.5) legacy))
                        pouchId gauzePad)

    describe "cycles and duplicates (requirement 6)" $ do
        it "refuses a move into the instance itself" $
            moveInstance scene crateId crateId `refusedWith` WouldCycle
        it "refuses a move into one of the instance's own descendants, \
           \and says CYCLE rather than 'no such target' — the check runs \
           \before the detach that would make the target vanish" $ do
            moveInstance scene packId pouchId  `refusedWith` WouldCycle
            moveInstance scene crateId pouchId `refusedWith` WouldCycle
            moveInstance scene kitId bandageId `refusedWith` WouldCycle
        it "refuses a candidate whose own id is already in the tree" $
            insertInstance scene pouchId (item "bandage" bandageId 0.25 0.5)
                `refusedWith` DuplicateInstanceId
        it "and one whose ROOT id is unique but whose NESTED child \
           \collides — every id in the moved subtree is compared" $ do
            let smuggler cs = kitOf "first_aid_kit" 99 0.5 2.0 cs
            findInstance 99 baseTree `shouldBe` Nothing
            insertInstance scene pouchId
                (smuggler [ item "gauze" 98 0.25 0.5
                          , item "bandage" bandageId 0.25 0.5 ])
                `refusedWith` DuplicateInstanceId
            accepted (insertInstance scene pouchId
                        (smuggler [ item "gauze" 98 0.25 0.5
                                  , item "bandage" 97 0.25 0.5 ]))
        it "an id in oscOther is not part of THIS ownership tree, so it \
           \is neither resolvable nor a duplicate" $ do
            let worn = scene { oscOther = [item "suit" 50 5.0 4.0] }
            findInstance 50 (oscItems worn) `shouldBe` Nothing
            moveInstance worn 50 pouchId `refusedWith` NoSuchInstance
            accepted (insertInstance worn pouchId (item "suit" 50 5.0 4.0))

    describe "resolution by exact id (requirement 8)" $ do
        it "removes at whatever depth the instance sits" $ do
            (orParent ⊚ removeInstance scene bandageId)
                `shouldBe` Right (Just kitId)
            (orIndex  ⊚ removeInstance scene bandageId) `shouldBe` Right 0
            (orParent ⊚ removeInstance scene pouchId)
                `shouldBe` Right (Just crateId)
            (orIndex  ⊚ removeInstance scene pouchId)   `shouldBe` Right 1
            (orParent ⊚ removeInstance scene rockId)    `shouldBe` Right Nothing
            (orIndex  ⊚ removeInstance scene rockId)    `shouldBe` Right 1
        it "refuses an id present nowhere, distinctly from an absent \
           \target" $ do
            removeInstance scene absentId `refusedWith` NoSuchInstance
            moveInstance scene absentId pouchId `refusedWith` NoSuchInstance
            moveInstance scene rockId absentId `refusedWith` NoSuchTarget
            insertInstance scene absentId gauzePad `refusedWith` NoSuchTarget
        it "searches sibling branches without altering them" $
            case removeInstance scene bandageId of
                Left r → expectationFailure (show r)
                Right rm → do
                    findInstance spannerId (orItems rm)
                        `shouldBe` findInstance spannerId baseTree
                    findInstance pouchId (orItems rm)
                        `shouldBe` findInstance pouchId baseTree
                    findInstance rockId (orItems rm)
                        `shouldBe` findInstance rockId baseTree

    describe "atomic refusal (requirement 7)" $ do
        it "a refusal reached AFTER the internal detach still hands the \
           \caller the tree it already had" $ do
            -- moveInstance removes before it inserts, so a destination
            -- refusal happens with the subtree detached internally. The
            -- contract is that no caller ever sees that intermediate.
            let tight = onTree (setStorage pouchId
                                  (Just (ItemStorage 0.125 20)) baseTree)
            moveInstance tight spannerId pouchId
                `refusedWith` OverTargetWeight
            oscItems tight `shouldBe`
                setStorage pouchId (Just (ItemStorage 0.125 20)) baseTree
            sort (idList (oscItems tight)) `shouldBe` sort (idList baseTree)
            nub (idList (oscItems tight)) `shouldBe` idList (oscItems tight)
        it "and so does one reached before it" $ do
            moveInstance scene packId pouchId `refusedWith` WouldCycle
            oscItems scene `shouldBe` baseTree

    describe "removal, rollback, and the round trip (requirements 7+9)" $ do
        it "remove-then-restore is the identity, at every depth and index" $
            mapM_ (\iid → case removeInstance scene iid of
                      Left r → expectationFailure (show iid <> ": " <> show r)
                      Right rm → restoreRemoval rm (orItems rm)
                                     `shouldBe` Right baseTree)
                  [packId, crateId, kitId, pouchId, bandageId, spannerId, rockId]
        it "restores into a parent that would REFUSE an insert — a \
           \rollback recovers the original source, an ordinary reinsert \
           \needs an eligible one" $
            case removeInstance scene bandageId of
                Left r → expectationFailure (show r)
                Right rm → do
                    -- The kit authors no storage: putting the bandage
                    -- back is a rollback, not an insert, and exactly
                    -- one of the two may work.
                    restoreRemoval rm (orItems rm) `shouldBe` Right baseTree
                    insertInstance (onTree (orItems rm)) kitId (orInstance rm)
                        `refusedWith` TargetNotStorage
        it "an ordinary reinsert into an ELIGIBLE parent keeps every \
           \instance but APPENDS — which is exactly why rollback is a \
           \separate entry point" $
            case removeInstance scene pouchId of
                Left r → expectationFailure (show r)
                Right rm → do
                    orIndex rm `shouldBe` 1
                    case insertInstance (onTree (orItems rm)) crateId
                                        (orInstance rm) of
                        Left r → expectationFailure (show r)
                        Right items' → do
                            (map iiInstanceId ∘ iiContents)
                                ⊚ findInstance crateId items'
                                `shouldBe` Just [kitId, spannerId, pouchId]
                            sort (idList items') `shouldBe` sort (idList baseTree)
                            items' `shouldNotBe` baseTree
                    restoreRemoval rm (orItems rm) `shouldBe` Right baseTree
        it "reports a vanished parent rather than dropping the instance, \
           \and appends when that parent has since shrunk" $
            case removeInstance scene bandageId of
                Left r → expectationFailure (show r)
                Right rm → do
                    restoreRemoval rm [] `refusedWith` NoSuchTarget
                    case restoreRemoval rm (setContents kitId [] (orItems rm)) of
                        Left r → expectationFailure (show r)
                        Right items' →
                            (map iiInstanceId ∘ iiContents)
                                ⊚ findInstance kitId items'
                                `shouldBe` Just [bandageId]
        it "a top-level removal records no parent and restores at its \
           \own index" $
            case removeInstance scene packId of
                Left r → expectationFailure (show r)
                Right rm → do
                    orParent rm `shouldBe` Nothing
                    orIndex rm `shouldBe` 0
                    map iiInstanceId (orItems rm) `shouldBe` [rockId]
                    restoreRemoval rm (orItems rm) `shouldBe` Right baseTree

    describe "the refusal vocabulary (requirement 9)" $ do
        it "enumerates every constructor with a distinct, non-blank id" $ do
            length allOwnershipRefusals `shouldBe` 12
            let ids = map ownershipRefusalId allOwnershipRefusals
            nub ids `shouldBe` ids
            filter T.null ids `shouldBe` []
        it "and every one of them is REACHABLE — this suite produces \
           \each at least once, so no enumerated reason is decorative" $ do
            let bulkless = addTo pouchId
                    ((item "legacy_tin" 42 0.25 0.5) { iiBulk = Nothing })
                    baseTree
                tinInKit = addTo kitId
                    (container "tin" 41 0.25 0.5 (10, 10) []) baseTree
                pouchAt s = onTree (setStorage pouchId (Just s) baseTree)
                produced =
                    [ reasonOf (removeInstance scene absentId)
                    , reasonOf (insertInstance scene absentId gauzePad)
                    , reasonOf (insertInstance scene kitId gauzePad)
                    , reasonOf (insertInstance scene pouchId
                                  gauzePad { iiBulk = Nothing })
                    , reasonOf (insertInstance (onTree bulkless) pouchId gauzePad)
                    , reasonOf (insertInstance (onTree tinInKit) 41 gauzePad)
                    , reasonOf (insertInstance (pouchAt (ItemStorage 0.125 20))
                                  pouchId gauzePad)
                    , reasonOf (insertInstance (pouchAt (ItemStorage 20 0.125))
                                  pouchId gauzePad)
                    , reasonOf (insertInstance
                                  (onTree (setStorage crateId
                                     (Just (ItemStorage 0.125 50)) baseTree))
                                  pouchId gauzePad)
                    , reasonOf (insertInstance
                                  scene { oscCapacity = RootLimited 0 }
                                  pouchId gauzePad)
                    , reasonOf (moveInstance scene packId pouchId)
                    , reasonOf (insertInstance scene pouchId
                                  (item "bandage" bandageId 0.25 0.5))
                    ]
            sort (nub (catMaybes produced)) `shouldBe` allOwnershipRefusals

    describe "structural writer guard (requirement 11)" $ do
        it "no production module outside the allowlist writes iiContents" $ do
            found ← allContentsWriters
            sort found `shouldBe` sort contentsWriterAllowlist
        it "and every allowlisted function really does write it — the \
           \list names none that has quietly stopped" $ do
            found ← allContentsWriters
            mapM_ (\entry → (entry, entry `elem` found)
                              `shouldBe` (entry, True))
                  contentsWriterAllowlist
        it "the guard is not vacuous: it FINDS an unauthorized write" $
            contentsWriteSites "src/Fake.hs" unauthorizedSource
                `shouldBe` [("src/Fake.hs", "stashItem")]
        it "and attributes a where-clause write to its enclosing \
           \TOP-LEVEL function, which is what scopes the two Medical \
           \exceptions to consumeBandages and consumeKitFill instead of \
           \to the whole file" $
            contentsWriteSites "src/Fake.hs" whereClauseSource
                `shouldBe` [("src/Fake.hs", "outerFunction")]
        it "and it does not cry wolf: a read, a comparison, a commented \
           \-out write and a haddock mention are not writes" $
            contentsWriteSites "src/Fake.hs" innocentSource `shouldBe` []

-- * The writer guard

-- | Every production module permitted to assign 'iiContents', with the
--   TOP-LEVEL function in it that does so. Function-scoped on purpose:
--   the two medical exceptions are @consumeBandages@ and
--   @consumeKitFill@ CONSUMING a kit's contents, and a module-level
--   exemption would silently license an unrelated third writer in the
--   same file later.
--
--   None of these is an ownership MOVE, which is why they sit outside
--   "Item.Ownership" rather than inside it:
--
--   * @materializeNode@ MINTS a tree (#1418's one mint boundary);
--   * @fromItemInstanceDTO@ REBUILDS one already materialized;
--   * @coolItem@ RE-VALUES temperatures in place, moving nothing;
--   * the two medical draws DESTROY contents rather than re-owning them.
contentsWriterAllowlist ∷ [(FilePath, String)]
contentsWriterAllowlist =
    [ ("src" </> "Item" </> "Materialize.hs", "materializeNode")
    , ("src" </> "Item" </> "Ownership.hs", "removeInstance")
    , ("src" </> "Item" </> "Ownership.hs", "restoreRemoval")
    , ("src" </> "Item" </> "Ownership.hs", "rewriteInstance")
    , ("src" </> "Item" </> "Ownership.hs", "insertInstance")
    , ("src" </> "Item" </> "Temperature.hs", "coolItem")
    , ("src" </> "World" </> "Save" </> "Component" </> "PageActivity.hs"
      , "fromItemInstanceDTO")
    , ("src" </> "Engine" </> "Scripting" </> "Lua" </> "API" </> "Units"
        </> "Medical.hs", "consumeBandages")
    , ("src" </> "Engine" </> "Scripting" </> "Lua" </> "API" </> "Units"
        </> "Medical.hs", "consumeKitFill")
    ]

-- | Every @iiContents@ assignment in the whole production tree, deduped
--   to one entry per (module, enclosing function).
allContentsWriters ∷ IO [(FilePath, String)]
allContentsWriters = do
    srcs ← map ("src" </>) <$> walkFilesWithExtension "src" ".hs"
    apps ← map ("app" </>) <$> walkFilesWithExtension "app" ".hs"
    concat <$> mapM one (sort (srcs ⧺ apps))
  where
    one path = nub ∘ contentsWriteSites path <$> TIO.readFile path

-- | Every assignment to 'iiContents' in one source, as
--   @(path, enclosing top-level function)@.
--
--   Three rules keep it honest, each earned:
--
--   * @iiContents@ must be a whole identifier followed by a single
--     @=@, so a READ (@iiContents it@), a comparison (@≡@ is a
--     different character entirely) and @itemContentsSig@ all pass by;
--   * everything from the first @--@ is dropped, so a commented-out
--     write and a haddock naming the field are not findings;
--   * the enclosing function is the nearest preceding definition
--     starting in column 0, so a write inside a @where@ clause is
--     attributed to the top-level binding that owns it — which is the
--     granularity the two Medical exceptions need.
--
--   A record PATTERN (@ItemInstance { iiContents = cs }@) would also be
--   reported. That is deliberate over-reporting: none exists in the
--   tree today, and production code that destructures nested contents
--   positionally is worth a look rather than a silent pass.
contentsWriteSites ∷ FilePath → Text → [(FilePath, String)]
contentsWriteSites path body =
    [ (path, fn) | (fn, line) ← scoped, assignsContents line ]
  where
    scoped = go "?" (map stripComment (T.lines body))
      where
        go _  []       = []
        go fn (l : ls) = case definitionName l of
            Just fn' → (fn', l) : go fn' ls
            Nothing  → (fn,  l) : go fn  ls

    stripComment l = fst (T.breakOn "--" l)

    -- A top-level definition or signature: an identifier starting in
    -- column 0 that is not one of Haskell's own leading keywords.
    definitionName l = case T.uncons l of
        Just (c, _) | isLower c →
            let name = T.takeWhile isIdent l
            in if T.null name ∨ name `elem` keywords
                   then Nothing
                   else Just (T.unpack name)
        _ → Nothing

    keywords = [ "module", "import", "where", "data", "type", "newtype"
               , "class", "instance", "deriving", "infix", "infixl"
               , "infixr", "foreign", "default" ]

    isIdent c = isAlphaNum c ∨ c ≡ '_' ∨ c ≡ '\''

    assignsContents l = any assigns (occurrences l)
      where
        assigns rest = case T.uncons (T.stripStart rest) of
            Just ('=', after) → case T.uncons after of
                Just ('=', _) → False   -- `==`, a comparison
                _             → True
            _ → False

    -- The remainder after each whole-identifier `iiContents`.
    occurrences = go
      where
        go rest = case T.breakOn "iiContents" rest of
            (_, after) | T.null after → []
            (before, after) →
                let tail' = T.drop (T.length "iiContents") after
                    whole = maybe True (not ∘ identChar) (lastMaybe before)
                          ∧ maybe True (not ∘ identChar) (fstMaybe tail')
                in [tail' | whole] ⧺ go tail'
        identChar c = isAlphaNum c ∨ c ≡ '_' ∨ c ≡ '\'' ∨ c ≡ '.'
        lastMaybe t = if T.null t then Nothing else Just (T.last t)
        fstMaybe t  = if T.null t then Nothing else Just (T.head t)

-- | A module that writes 'iiContents' without permission.
unauthorizedSource ∷ Text
unauthorizedSource = T.unlines
    [ "module Fake where"
    , ""
    , "stashItem ∷ ItemInstance → ItemInstance → ItemInstance"
    , "stashItem child parent = parent { iiContents = child : iiContents parent }"
    ]

-- | The same write, one level down in a @where@ clause.
whereClauseSource ∷ Text
whereClauseSource = T.unlines
    [ "module Fake where"
    , ""
    , "outerFunction ∷ [ItemInstance] → [ItemInstance]"
    , "outerFunction = map helper"
    , "  where"
    , "    helper i = i { iiContents = [] }"
    ]

-- | Reads, comparisons, comments and haddocks — none of them writes.
innocentSource ∷ Text
innocentSource = T.unlines
    [ "-- | Counts what is nested in 'iiContents', which it never writes."
    , "module Fake where"
    , ""
    , "countNested ∷ ItemInstance → Int"
    , "countNested i = length (iiContents i)"
    , ""
    , "sameContents ∷ ItemInstance → ItemInstance → Bool"
    , "sameContents a b = iiContents a ≡ iiContents b"
    , ""
    , "signature ∷ ItemInstance → Text"
    , "signature = itemContentsSig"
    , ""
    , "disabled ∷ ItemInstance → ItemInstance"
    , "disabled i = i   -- i { iiContents = [] } is what this used to do"
    ]
