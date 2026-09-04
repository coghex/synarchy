{-# LANGUAGE Strict #-}
-- | "Location instance identity" (#911): the pure allocator, lifecycle,
--   and pre-#911 chunk-set → instance migration that
--   'Location.Instance' owns, plus the @world-pages@ v1→v2 component
--   migration ('World.Save.Component.PageCore.migrateWorldPagesV1')
--   that
--   feeds it. No engine needed — mirrors
--   'Test.Headless.Location.Discovery' fixture style; the IO-level
--   discovery coverage lives in 'Test.Headless.World.LocationDiscovery'.
module Test.Headless.Location.Instance (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (find, nub, sort)
import qualified Data.Serialize as S
import qualified Data.Text as T
import Location.Bounds (AbsBounds(..), RelBounds(..), translateBounds)
import Location.Instance
import Location.Overlay.Types (LocationOverlay, emptyLocationOverlay)
import Test.Headless.Location.Fixture (expectGeometry)
import Location.Types
    ( LocationContent(..), LocationDef(..), LocationNaming(..), LocationRegistry
    , emptyLocationRegistry, registerLocation )
import Unit.Types (UnitId(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Save.Component.Page
    ( PageCoreDTOv1(..), WorldPagesDTOv1(..), WorldPages(..)
    , migrateWorldPagesV1, toWorldGenParamsDTOv1, WorldIdentityDTOv1(..) )
import World.Save.Snapshot (PageSnapshot(..))
import Language.Semantic.Types (ConceptId(..))

-- | The naming scheme every 'LocationDef' fixture in this module
--   carries (#1101). One concept per pool is enough: these specs are
--   about geometry, lifecycle, and identity, and every one of them
--   builds instances with NO namer, so the pools are never drawn from.
testNaming ∷ LocationNaming
testNaming = LocationNaming
    { lnHeads     = [ConceptId "KEEP"]
    , lnModifiers = [ConceptId "ASH"]
    }


-- * Fixtures — two ruin-shaped defs. "ruin" is 5x5 (margin 6); "camp"
--   is a distinct 3x3 (margin 2) so a test can tell the two apart by
--   their stored geometry alone.

ruinDef, campDef ∷ LocationDef
ruinDef = mkDef "ruin" "Small Ruin" (RelBounds (-2) (-2) 2 2)
campDef = mkDef "camp" "Old Camp"   (RelBounds (-1) (-1) 1 1)

encounterDef ∷ LocationDef
encounterDef = (mkDef "ruin_small" "Small Ruin"
                    (RelBounds (-2) (-2) 2 2))
    { ldContents =
        [ LocationContent "unit" "nomad_primitive" 1 Nothing
            (Just "hostile") 1 (Just (0, 3)) (Just "death_only") False
        ]
    }

mkDef ∷ Text → Text → RelBounds → LocationDef
mkDef lid label bounds = LocationDef
    { ldId              = lid
    , ldLabel           = label
    , ldType            = "ruin"
    , ldBuilder         = "room_small"
    , ldAnchor          = []
    , ldMaxCount        = 0
    , ldMinSpacing      = 0
    , ldContents        = []
    , ldBounds          = bounds
    , ldMapIcon         = Nothing
    , ldNaming          = testNaming
    }

registry ∷ LocationRegistry
registry = foldr registerLocation emptyLocationRegistry [ruinDef, campDef]

-- | Three placements whose chunk coords are deliberately NOT in
--   'HM.HashMap' iteration order, so a build that leaked hashmap order
--   would produce a different id → (definition, anchor) mapping.
overlay3 ∷ LocationOverlay
overlay3 = HM.fromList
    [ (ChunkCoord 2 (-1), "ruin")
    , (ChunkCoord (-3) 4, "camp")
    , (ChunkCoord 0 0,    "ruin")
    ]

instances3 ∷ LocationInstances
instances3 = expectGeometry (buildLocationInstances Nothing registry overlay3)

-- | The pre-#911 per-chunk flags a v1 payload carries for 'overlay3':
--   chunk (0,0) discovered, chunk (2,-1) contents-spawned — two
--   DIFFERENT placements with DIFFERENT flags, so a migration that
--   dropped either set, or crossed them over, cannot pass.
legacyFlags ∷ LocationInstances
legacyFlags = pendingLegacyFlags
    (HS.fromList [ChunkCoord 0 0])       -- discovered
    (HS.fromList [ChunkCoord 2 (-1)])    -- contents spawned

-- | What every placement's (chunk, lifecycle, contents-spawned) must be
--   once 'legacyFlags' has been migrated, in instance-id order.
expectedMigratedState ∷ [(ChunkCoord, LocationLifecycle, Bool)]
expectedMigratedState =
    [ (ChunkCoord (-3) 4, LifecycleUnknown,    False)
    , (ChunkCoord 0 0,    LifecycleDiscovered, False)
    , (ChunkCoord 2 (-1), LifecycleUnknown,    True)
    ]

stateOf ∷ LocationInstances → [(ChunkCoord, LocationLifecycle, Bool)]
stateOf lis =
    [ (liChunk i, liLifecycle i, liContentsSpawned i) | i ← instancesToList lis ]

identityOf ∷ LocationInstances → [(LocationInstanceId, Text, ChunkCoord)]
identityOf lis =
    [ (liId i, liDefId i, liChunk i) | i ← instancesToList lis ]

-- | Two SYNTHETIC instances anchored in the SAME chunk. Placement never
--   produces this ('Location.Overlay.placeDef' rejects a same-chunk
--   collision), so it is built through the allocator directly — the
--   point is that the REPRESENTATION keeps them independently
--   addressable, which the chunk-keyed sets could not.
sameChunk ∷ (LocationInstanceId, LocationInstanceId, LocationInstances)
sameChunk =
    let (a, l1) = expectGeometry
                      (allocateLocationInstance Nothing (ChunkCoord 7 7) ruinDef
                          emptyLocationInstances)
        (b, l2) = expectGeometry
                      (allocateLocationInstance Nothing (ChunkCoord 7 7) campDef
                          l1)
    in (a, b, l2)

spec ∷ Spec
spec = describe "Location instance identity" $ do

    describe "id allocation" $ do
        it "allocates ids from 1, the engine-wide convention" $
            map (unLocationInstanceId . liId) (instancesToList instances3)
                `shouldBe` [1, 2, 3]

        it "assigns ids in the overlay's canonical (cx, cy) order, never \
           \hashmap-iteration order" $
            -- (-3,4) < (0,0) < (2,-1)
            map liChunk (instancesToList instances3)
                `shouldBe` [ChunkCoord (-3) 4, ChunkCoord 0 0, ChunkCoord 2 (-1)]

        it "pins each allocated id to its definition and canonical chunk" $
            -- Hand-stated, never re-derived from a second construction:
            -- 'overlay3' maps (2,-1)→"ruin", (-3,4)→"camp", (0,0)→"ruin",
            -- and ids follow canonical (cx, cy) order — so a build that
            -- leaked hashmap order, or paired a correctly ordered chunk
            -- with the wrong definition, cannot reproduce this list. It
            -- is also the baseline the two cross-path comparisons below
            -- (the pre-#911 migration, the serialization round trip) rest
            -- on.
            identityOf instances3 `shouldBe`
                [ (LocationInstanceId 1, "camp", ChunkCoord (-3) 4)
                , (LocationInstanceId 2, "ruin", ChunkCoord 0    0)
                , (LocationInstanceId 3, "ruin", ChunkCoord 2  (-1)) ]

        it "leaves the allocator strictly above every live id" $ do
            lisNextId instances3 `shouldBe` 4
            locationInstanceAllocatorErrors instances3 `shouldBe` []

        it "reserves an id for an overlay entry whose definition is not \
           \registered, so the remaining ids do not shift" $ do
            let partial = expectGeometry (buildLocationInstances Nothing
                    (registerLocation campDef emptyLocationRegistry) overlay3)
            -- Only "camp" resolves; it keeps id 1 (its (cx,cy) slot), and
            -- the allocator still accounts for all three placements.
            map (\i → (unLocationInstanceId (liId i), liDefId i))
                (instancesToList partial) `shouldBe` [(1, "camp")]
            lisNextId partial `shouldBe` 4

        it "an empty overlay yields an empty table with a fresh allocator" $ do
            let none = expectGeometry
                    (buildLocationInstances Nothing registry
                                            emptyLocationOverlay)
            instancesToList none `shouldBe` []
            lisNextId none `shouldBe` firstLocationInstanceId

        it "allocateLocationInstance hands out the next id and advances it" $ do
            let (iid, after) = expectGeometry
                    (allocateLocationInstance Nothing (ChunkCoord 9 9)
                                              ruinDef instances3)
            unLocationInstanceId iid `shouldBe` 4
            lisNextId after `shouldBe` 5
            locationInstanceAllocatorErrors after `shouldBe` []

        it "flags an instance at or above the page's allocator" $
            locationInstanceAllocatorErrors (instances3 { lisNextId = 2 })
                `shouldSatisfy` (not . null)

        -- #1667: the two comprehensions above both draw from 'lisById',
        -- so an EMPTY table certified any cursor at all -- including 0
        -- and, 'lisdNextId' being an unrestricted wire 'Int', a negative
        -- one. 'allocateLocationInstance' hands the stored cursor out
        -- verbatim, so such a payload would mint id 0 or a negative id
        -- against the convention 'firstLocationInstanceId' documents.
        it "flags an allocator below the first valid id even on an EMPTY \
           \table, which no live id can witness" $ do
            let withCursor n = emptyLocationInstances { lisNextId = n }
            locationInstanceAllocatorErrors (withCursor 0)
                `shouldSatisfy` any (T.isInfixOf "location-instance allocator is 0")
            locationInstanceAllocatorErrors (withCursor (-3))
                `shouldSatisfy` any (T.isInfixOf "location-instance allocator is -3")

        it "still accepts an EMPTY table whose allocator is legitimately \
           \fresh, and one that has legitimately advanced" $ do
            locationInstanceAllocatorErrors emptyLocationInstances `shouldBe` []
            locationInstanceAllocatorErrors
                (emptyLocationInstances { lisNextId = 42 }) `shouldBe` []

        it "reports the allocator floor SEPARATELY from the per-id \
           \comparison, so a table violating both says so twice" $ do
            let both = instances3 { lisNextId = 0 }
                errs = locationInstanceAllocatorErrors both
            errs `shouldSatisfy` any (T.isInfixOf "location-instance allocator is 0")
            errs `shouldSatisfy`
                any (T.isInfixOf "is not below the page's location-instance allocator")

        it "flags an instance stored under a key that is not its own id" $ do
            let inst = expectGeometry
                    (newLocationInstance Nothing (LocationInstanceId 1)
                                         (ChunkCoord 0 0) ruinDef)
                broken = LocationInstances
                    { lisNextId        = 9
                    , lisById          = HM.singleton (LocationInstanceId 5) inst
                    , lisPendingLegacy = Nothing }
            locationInstanceAllocatorErrors broken `shouldSatisfy` (not . null)

    -- #1668: the table's GEOMETRY, beside its ids. The save decode path
    -- rebuilds an 'AbsBounds' from four unrestricted wire 'Int's,
    -- entirely outside the YAML loader's inverted-bounds gate, so a
    -- corrupt payload can carry one. Since #1796 engine placement
    -- CANNOT, without qualification: 'newLocationInstance' builds every
    -- box through 'locationInstanceGeometry', which computes each
    -- component in Integer and refuses the placement outright unless it
    -- is representable, and an accepted translation offsets both ends
    -- of an axis alike. Decode is therefore now the only source -- which
    -- is exactly why this check stays. Rejecting the box is right: an
    -- inverted one fails silently, containing no point at any wrap image
    -- (discovery can never fire) while still reporting intersection
    -- with unrelated terrain (placement blocks valid ground).
    describe "stored-bounds validation" $ do
        let withBounds b = LocationInstances
                { lisNextId        = 2
                , lisById          = HM.singleton (LocationInstanceId 1)
                    ((expectGeometry
                          (newLocationInstance Nothing (LocationInstanceId 1)
                              (ChunkCoord 0 0) ruinDef)) { liBounds = b })
                , lisPendingLegacy = Nothing }

        it "accepts every box an engine-placed table carries" $
            locationInstanceBoundsErrors instances3 `shouldBe` []

        it "accepts an empty table" $
            locationInstanceBoundsErrors emptyLocationInstances `shouldBe` []

        it "accepts a DEGENERATE single-tile box -- inclusive bounds make \
           \min ≡ max a legitimate 1x1 footprint, exactly as the YAML \
           \loader accepts it" $
            locationInstanceBoundsErrors (withBounds (AbsBounds 4 7 4 7))
                `shouldBe` []

        it "accepts a box degenerate on ONE axis only" $ do
            locationInstanceBoundsErrors (withBounds (AbsBounds 4 7 4 9))
                `shouldBe` []
            locationInstanceBoundsErrors (withBounds (AbsBounds 4 7 6 7))
                `shouldBe` []

        it "rejects an x-inverted box, naming the instance, the axis and \
           \both offending coordinates" $
            case locationInstanceBoundsErrors (withBounds (AbsBounds 5 0 2 4)) of
                [msg] → do
                    msg `shouldSatisfy` T.isInfixOf "#1"
                    msg `shouldSatisfy` T.isInfixOf "x axis"
                    msg `shouldSatisfy` T.isInfixOf "minX 5"
                    msg `shouldSatisfy` T.isInfixOf "maxX 2"
                    msg `shouldNotSatisfy` T.isInfixOf "y axis"
                other → expectationFailure
                    ("expected exactly one x-axis error, got " <> show other)

        it "rejects a y-inverted box, naming the y axis alone" $
            case locationInstanceBoundsErrors (withBounds (AbsBounds 0 5 4 2)) of
                [msg] → do
                    msg `shouldSatisfy` T.isInfixOf "y axis"
                    msg `shouldSatisfy` T.isInfixOf "minY 5"
                    msg `shouldSatisfy` T.isInfixOf "maxY 2"
                    msg `shouldNotSatisfy` T.isInfixOf "x axis"
                other → expectationFailure
                    ("expected exactly one y-axis error, got " <> show other)

        it "names BOTH axes when both are inverted -- reporting one \
           \unspecified inversion would not say what to repair" $ do
            let msgs = locationInstanceBoundsErrors
                           (withBounds (AbsBounds 5 5 2 2))
            length msgs `shouldBe` 2
            msgs `shouldSatisfy` any (T.isInfixOf "x axis")
            msgs `shouldSatisfy` any (T.isInfixOf "y axis")

        it "reports EVERY offending instance in the table, keyed by the \
           \map key the entry is addressed under" $ do
            let inst iid b = (expectGeometry
                    (newLocationInstance Nothing iid
                                         (ChunkCoord 0 0) ruinDef))
                        { liBounds = b }
                table = LocationInstances
                    { lisNextId = 4
                    , lisById   = HM.fromList
                        [ (LocationInstanceId 1,
                              inst (LocationInstanceId 1) (AbsBounds 0 0 2 2))
                        , (LocationInstanceId 2,
                              inst (LocationInstanceId 2) (AbsBounds 9 0 1 2))
                        , (LocationInstanceId 3,
                              inst (LocationInstanceId 3) (AbsBounds 0 9 2 1)) ]
                    , lisPendingLegacy = Nothing }
                msgs = locationInstanceBoundsErrors table
            length msgs `shouldBe` 2
            msgs `shouldSatisfy` any (T.isInfixOf "#2")
            msgs `shouldSatisfy` any (T.isInfixOf "#3")
            msgs `shouldNotSatisfy` any (T.isInfixOf "#1")

        it "is independent of the allocator check -- a table can fail one \
           \and pass the other" $ do
            let geometryOnly = withBounds (AbsBounds 5 0 2 4)
            locationInstanceAllocatorErrors geometryOnly `shouldBe` []
            locationInstanceBoundsErrors geometryOnly
                `shouldSatisfy` (not . null)
            locationInstanceAllocatorErrors (instances3 { lisNextId = 2 })
                `shouldSatisfy` (not . null)
            locationInstanceBoundsErrors (instances3 { lisNextId = 2 })
                `shouldBe` []

    describe "stored geometry and display name" $ do
        it "stores the anchor, resolved absolute bounds and name \
           \from the definition at placement time" $
            case lookupLocationInstance (LocationInstanceId 2) instances3 of
                Nothing → expectationFailure "expected instance #2"
                Just inst → do
                    liDefId inst           `shouldBe` "ruin"
                    liAnchor inst          `shouldBe` (8, 8)
                    liBounds inst          `shouldBe` AbsBounds 6 6 10 10
                    liDisplayName inst     `shouldBe` "Small Ruin"

        it "keeps its stored geometry when the definition is edited later — \
           \nothing re-derives it from the registry" $ do
            let edited = registerLocation
                    (mkDef "ruin" "Renamed" (RelBounds (-9) (-9) 9 9))
                    registry
            -- The ONLY registry-consulting path after placement is the v1
            -- migration, and it is a no-op on an already-resolved table.
            resolveLegacyLocationInstances edited overlay3 instances3
                `shouldBe` Right instances3

        it "the anchor is the hosting chunk's centre tile" $
            locationAnchorTileChecked (ChunkCoord (-3) 4)
                `shouldBe` Right (-40, 72)

        it "a page-scoped lookup returns nothing for an unknown id" $
            lookupLocationInstance (LocationInstanceId 99) instances3
                `shouldBe` Nothing

    describe "lifecycle" $ do
        it "every new instance starts unknown" $
            map liLifecycle (instancesToList instances3)
                `shouldBe` replicate 3 LifecycleUnknown

        it "names round-trip across the Lua boundary" $
            map (lifecycleFromName . lifecycleName)
                [minBound .. maxBound ∷ LocationLifecycle]
                `shouldBe` map Just [minBound .. maxBound]

        it "reports an unknown state name as Nothing" $
            lifecycleFromName "explored" `shouldBe` Nothing

        it "orders the six states as the expedition loop documents them" $
            [minBound .. maxBound]
                `shouldBe` [ LifecycleUnknown, LifecycleHinted
                           , LifecycleDiscovered, LifecycleActive
                           , LifecycleCleared, LifecycleDepleted ]

        it "isDiscoveredLifecycle holds at discovered and every later state" $
            map isDiscoveredLifecycle [minBound .. maxBound]
                `shouldBe` [False, False, True, True, True, True]

        describe "promoteLifecycle" $ do
            it "accepts a strictly forward transition" $
                promoteLifecycle LifecycleUnknown LifecycleDiscovered
                    `shouldBe` Just LifecycleDiscovered

            it "accepts hinted → discovered" $
                promoteLifecycle LifecycleHinted LifecycleDiscovered
                    `shouldBe` Just LifecycleDiscovered

            it "refuses a same-state transition (what makes the discovery \
               \event fire exactly once)" $
                promoteLifecycle LifecycleDiscovered LifecycleDiscovered
                    `shouldBe` Nothing

            describe "refuses every backward transition" $
                forM_ [ (LifecycleCleared, LifecycleDiscovered)
                      , (LifecycleDepleted, LifecycleActive)
                      , (LifecycleActive, LifecycleUnknown)
                      , (LifecycleDiscovered, LifecycleHinted)
                      ] $ \(cur, next) →
                    it (show cur <> " → " <> show next) $
                        promoteLifecycle cur next `shouldBe` Nothing

        it "every later state is reachable programmatically" $ do
            let step lis l = fromMaybe lis
                    (setLocationLifecycle (LocationInstanceId 1) l lis)
                walked = foldl step instances3
                    [ LifecycleDiscovered, LifecycleActive
                    , LifecycleCleared, LifecycleDepleted ]
            (liLifecycle <$> lookupLocationInstance (LocationInstanceId 1) walked)
                `shouldBe` Just LifecycleDepleted

        it "setLocationLifecycle is a no-op for an unknown instance id" $
            setLocationLifecycle (LocationInstanceId 99) LifecycleDiscovered
                instances3 `shouldBe` Nothing

        it "setLocationLifecycle leaves every other instance alone" $
            case setLocationLifecycle (LocationInstanceId 2)
                     LifecycleCleared instances3 of
                Nothing → expectationFailure "expected the promotion to land"
                Just after → map liLifecycle (instancesToList after)
                    `shouldBe`
                    [LifecycleUnknown, LifecycleCleared, LifecycleUnknown]

    describe "content-spawn state is per instance" $ do
        it "marking one instance never marks another anchored in the SAME \
           \chunk" $ do
            let (a, b, both) = sameChunk
                after = markLocationContentsSpawned a both
            map (\i → (liId i, liContentsSpawned i)) (instancesToList after)
                `shouldBe` [(a, True), (b, False)]

        it "two instances in one chunk stay independently addressable" $ do
            let (a, b, both) = sameChunk
            map liId (instancesInChunk (ChunkCoord 7 7) both) `shouldBe` [a, b]
            (liDefId <$> lookupLocationInstance a both) `shouldBe` Just "ruin"
            (liDefId <$> lookupLocationInstance b both) `shouldBe` Just "camp"
            -- Distinct STORED geometry, not one shared per-chunk fact.
            (liBounds <$> lookupLocationInstance a both)
                `shouldBe` Just (AbsBounds 118 118 122 122)
            (liBounds <$> lookupLocationInstance b both)
                `shouldBe` Just (AbsBounds 119 119 121 121)

        it "is independent of lifecycle: spawning contents never discovers, \
           \and discovering never spawns" $ do
            let (a, _, both) = sameChunk
                spawned = markLocationContentsSpawned a both
            (liLifecycle <$> lookupLocationInstance a spawned)
                `shouldBe` Just LifecycleUnknown
            case setLocationLifecycle a LifecycleDiscovered both of
                Nothing → expectationFailure "expected the promotion to land"
                Just discovered →
                    (liContentsSpawned <$> lookupLocationInstance a discovered)
                        `shouldBe` Just False

        it "marking is a no-op for an unknown instance id" $
            markLocationContentsSpawned (LocationInstanceId 99) instances3
                `shouldBe` instances3

    describe "persistent ranged encounters (#916)" $ do
        let iid = LocationInstanceId 1
            makeAt iid' coord seed = expectGeometry
                (newLocationInstanceWithSeed seed Nothing iid'
                    coord encounterDef)
            make = makeAt iid (ChunkCoord 0 0)
            rolledAt iid' coord seed = leRolledCount .
                fromMaybe (error "encounter fixture missing") . liEncounter $
                    makeAt iid' coord seed
            rolled = rolledAt iid (ChunkCoord 0 0)
            seedFor n = fromMaybe (error "encounter outcome fixture missing")
                (find ((≡ n) . rolled) [0 .. 255])

        it "rolls one inclusive 0..3 count from the page seed and the \
           \stable instance id, and from nothing else" $ do
            map rolled [0 .. 255] `shouldSatisfy`
                all (\n → n ≥ 0 ∧ n ≤ 3)
            -- Every authored outcome is reachable; the implementation's
            -- modulo over a stateless 64-bit avalanche is the uniform draw.
            sort (nub (map rolled [0 .. 255])) `shouldBe` [0, 1, 2, 3]
            -- The stable instance id IS in the mix. Dropping 'rawId' from
            -- 'encounterFromDef' would give every location on one page the
            -- same encounter count, and would satisfy both assertions
            -- above; sweeping a second id is what notices.
            map (rolledAt (LocationInstanceId 2) (ChunkCoord 0 0)) [0 .. 255]
                `shouldNotBe` map rolled [0 .. 255]
            -- The chunk coord is NOT ("independent of chunk/load order").
            -- Compared over the whole sweep rather than one draw, because
            -- a single 0..3 result collides one time in four by chance.
            map (rolledAt iid (ChunkCoord (-3) 4)) [0 .. 255]
                `shouldBe` map rolled [0 .. 255]
            -- A source-level fixed vector for the canonical id (#948
            -- requirement 8's policy, applied to the neighbouring roll):
            -- a deliberate change to the avalanche or to the id multiplier
            -- must update this list rather than pass silently.
            --
            -- What it protects: fresh-world regeneration consistency —
            -- two processes generating one world seed place identical
            -- encounters. What it is NOT: a save-compatibility contract.
            -- 'leRolledCount' is computed and stored when the instance is
            -- allocated, then serialized and restored verbatim, never
            -- re-derived on load, so no save's contents depend on this
            -- mapping.
            map rolled [0 .. 15]
                `shouldBe` [1, 1, 3, 2, 3, 2, 0, 0, 3, 2, 0, 3, 3, 0, 2, 3]

        it "treats a zero roll as internally clear without revealing it \
           \or inventing a clearance event" $ do
            let seed0 = seedFor 0
                inst = make seed0
                encounter = fromMaybe (error "encounter fixture missing")
                    (liEncounter inst)
            liLifecycle inst `shouldBe` LifecycleUnknown
            locationDiscoveryLifecycle inst `shouldBe` LifecycleCleared
            leRosterComplete encounter `shouldBe` True
            leCleared encounter `shouldBe` True
            -- #917 generalized the notice onto the INSTANCE, and it
            -- starts spent for exactly the case #916 spent it for: a
            -- location born already clearance-satisfied. This def
            -- authors no significant item, so a zero roll satisfies the
            -- whole predicate at birth and nobody cleared anything.
            liClearEventEmitted inst `shouldBe` True
            leOccupants encounter `shouldBe` []

        it "persists the exact allocated roster and homes once, then \
           \refuses a chunk-load retry that tries to replace them" $ do
            let seed2 = seedFor 2
                inst = make seed2
                initial = emptyLocationInstances
                    { lisNextId = 2, lisById = HM.singleton iid inst }
                first = registerLocationEncounterOccupants iid
                    [(UnitId 11, (7, 8)), (UnitId 12, (9, 10))] initial
                retry = registerLocationEncounterOccupants iid
                    [(UnitId 21, (1, 2)), (UnitId 22, (3, 4))] first
                encounter = liEncounter =<< lookupLocationInstance iid retry
            fmap leRosterComplete encounter `shouldBe` Just True
            fmap (map (\o → (leoUnitId o, leoHome o)) . leOccupants) encounter
                `shouldBe` Just
                    [(UnitId 11, (7, 8)), (UnitId 12, (9, 10))]

        it "extends an interrupted registered prefix without replacing or \
           \duplicating its already-created occupant" $ do
            let seed2 = seedFor 2
                inst = make seed2
                initial = emptyLocationInstances
                    { lisNextId = 2, lisById = HM.singleton iid inst }
                prefix = registerLocationEncounterOccupants iid
                    [(UnitId 11, (7, 8))] initial
                resumed = registerLocationEncounterOccupants iid
                    [(UnitId 11, (7, 8)), (UnitId 12, (9, 10))] prefix
                encounter = liEncounter =<< lookupLocationInstance iid resumed
            fmap leRosterComplete encounter `shouldBe` Just True
            fmap (map (\o → (leoUnitId o, leoHome o)) . leOccupants) encounter
                `shouldBe` Just
                    [(UnitId 11, (7, 8)), (UnitId 12, (9, 10))]

        it "keeps an overlong or duplicate roster incomplete instead of \
           \silently clearing against a truncated membership" $ do
            let seed2 = seedFor 2
                inst = make seed2
                initial = emptyLocationInstances
                    { lisNextId = 2, lisById = HM.singleton iid inst }
                duplicate = registerLocationEncounterOccupants iid
                    [(UnitId 11, (7, 8)), (UnitId 11, (9, 10))] initial
                overlong = registerLocationEncounterOccupants iid
                    [ (UnitId 11, (7, 8)), (UnitId 12, (9, 10))
                    , (UnitId 13, (11, 12)) ] initial
                complete lis = leRosterComplete ⊚
                    (liEncounter =<< lookupLocationInstance iid lis)
            complete duplicate `shouldBe` Just False
            complete overlong `shouldBe` Just False

        it "updates one roster member's persisted guard episode state \
           \without changing membership, and marks clearance only once" $ do
            let seed1 = seedFor 1
                inst = make seed1
                initial = emptyLocationInstances
                    { lisNextId = 2, lisById = HM.singleton iid inst }
                rostered = registerLocationEncounterOccupants iid
                    [(UnitId 11, (7, 8))] initial
                engaged = adjustLocationEncounterOccupant iid (UnitId 11)
                    (\o → o { leoEngaged = True }) rostered
                episodic = setLocationEncounterEpisodeState iid True True False
                    engaged
                occupant = lookupLocationInstance iid episodic
                    >>= liEncounter >>= encounterOccupant (UnitId 11)
                encounter = liEncounter =<< lookupLocationInstance iid episodic
            fmap leoUnitId occupant `shouldBe` Just (UnitId 11)
            fmap leoHome occupant `shouldBe` Just (7, 8)
            fmap leoEngaged occupant `shouldBe` Just True
            fmap leActivated encounter `shouldBe` Just True
            fmap leEpisodeActive encounter `shouldBe` Just True
            fmap leAggressionAnnounced encounter `shouldBe` Just True
            -- Activation before discovery is remembered without revealing
            -- the location; ordinary sight later exposes it as active.
            fmap liLifecycle (lookupLocationInstance iid episodic)
                `shouldBe` Just LifecycleUnknown
            fmap locationDiscoveryLifecycle
                (lookupLocationInstance iid episodic)
                `shouldBe` Just LifecycleActive
            markLocationEncounterCleared iid episodic `shouldSatisfy` isJust
            let cleared = fromMaybe (error "expected clearance edge")
                    (markLocationEncounterCleared iid episodic)
            -- #917: completing the ENCOUNTER records only that. It
            -- neither moves the lifecycle nor spends the notice — the
            -- location is still undiscovered, so its completion stays
            -- private until sight reveals it.
            fmap liLifecycle (lookupLocationInstance iid cleared)
                `shouldBe` Just LifecycleUnknown
            fmap liClearEventEmitted (lookupLocationInstance iid cleared)
                `shouldBe` Just False
            resolveLocationClearance iid cleared `shouldBe` Nothing
            -- Once it IS visible, the deferred notice is spent exactly
            -- once and the location lands on cleared.
            let seen = fromMaybe (error "expected discovery promotion")
                    (setLocationLifecycle iid LifecycleDiscovered cleared)
                announced = fromMaybe (error "expected deferred clearance")
                    (resolveLocationClearance iid seen)
            fmap liLifecycle (lookupLocationInstance iid announced)
                `shouldBe` Just LifecycleCleared
            resolveLocationClearance iid announced `shouldBe` Nothing
            markLocationEncounterCleared iid cleared `shouldBe` Nothing

    describe "pre-#911 chunk-set migration" $ do
        let migrated = expectGeometry
                (resolveLegacyLocationInstances registry overlay3 legacyFlags)

        it "creates one instance per overlay entry, with the same ids a \
           \fresh placement of that overlay would allocate" $
            identityOf migrated `shouldBe` identityOf instances3

        it "maps each chunk's discovered / contents-spawned marker onto the \
           \instance occupying it, and nothing else" $
            stateOf migrated `shouldBe` expectedMigratedState

        it "does not invent encounters from current definitions while \
           \reconstructing a pre-encounter save" $ do
            let historicalRegistry = registerLocation encounterDef
                    emptyLocationRegistry
                historicalOverlay = HM.singleton (ChunkCoord 0 0) "ruin_small"
                out = expectGeometry (resolveLegacyLocationInstances
                    historicalRegistry historicalOverlay
                    (pendingLegacyFlags mempty mempty))
            map liEncounter (instancesToList out) `shouldBe` [Nothing]

        it "resolves each instance's bounds / name against the \
           \registered definition" $
            map (\i → (liBounds i, liDisplayName i))
                (instancesToList migrated)
                `shouldBe`
                [ (AbsBounds (-41) 71 (-39) 73, "Old Camp")
                , (AbsBounds 6 6 10 10,         "Small Ruin")
                , (AbsBounds 38 (-10) 42 (-6),  "Small Ruin")
                ]

        it "discards a marker naming a chunk with no overlay entry — it \
           \identifies no placed instance" $ do
            let stray = pendingLegacyFlags
                    (HS.fromList [ChunkCoord 50 50])
                    (HS.fromList [ChunkCoord 60 60])
                out = expectGeometry
                    (resolveLegacyLocationInstances registry overlay3 stray)
            map liLifecycle (instancesToList out)
                `shouldBe` replicate 3 LifecycleUnknown
            map liContentsSpawned (instancesToList out)
                `shouldBe` replicate 3 False

        it "clears the pending carry, and is idempotent" $ do
            lisPendingLegacy migrated `shouldBe` Nothing
            resolveLegacyLocationInstances registry overlay3 migrated
                `shouldBe` Right migrated

        it "leaves a table with nothing pending untouched" $
            resolveLegacyLocationInstances registry overlay3 instances3
                `shouldBe` Right instances3

    describe "world-pages v1 → v2 component migration" $ do
        let v1Params = defaultWorldGenParams
                { wgpSeed              = 4242
                , wgpLocationOverlay   = overlay3
                , wgpLocationStamped   = HS.fromList [ChunkCoord 0 0]
                , wgpLocationInstances = expectGeometry
                    (resolveLegacyLocationInstances registry overlay3
                                                    legacyFlags)
                }
            v1Page = PageCoreDTOv1
                { pc1PageId     = WorldPageId "main_world"
                , pc1GenParams  = toWorldGenParamsDTOv1 v1Params
                , pc1CameraX    = 1
                , pc1CameraY    = 2
                , pc1TimeHour   = 12
                , pc1TimeMinute = 30
                , pc1DateYear   = 1
                , pc1DateMonth  = 2
                , pc1DateDay    = 3
                , pc1MapMode    = ZMDefault
                , pc1Identity   = Just (WorldIdentityDTOv1 "Terra" Nothing)
                }
            -- Go through real bytes, not just the in-memory record: the
            -- point of a frozen v1 DTO is that its ENCODING still decodes.
            decodedV1 = S.decode (S.encode (WorldPagesDTOv1 [v1Page]))
                            ∷ Either String WorldPagesDTOv1
            withMigratedPage k = case decodedV1 of
                Left err → expectationFailure err
                Right dto →
                    case HM.lookup (WorldPageId "main_world")
                             (wpBase (migrateWorldPagesV1 dto)) of
                        Nothing → expectationFailure "expected the page"
                        Just p  → k (pgsGenParams p)

        it "decodes a frozen v1 payload and leaves its location flags \
           \pending, with an empty instance table" $
            withMigratedPage $ \params → do
                let lis = wgpLocationInstances params
                instancesToList lis `shouldBe` []
                lisPendingLegacy lis `shouldSatisfy` isJust

        it "carries the rest of the page core across unchanged" $
            withMigratedPage $ \params → do
                wgpSeed params `shouldBe` 4242
                wgpLocationOverlay params `shouldBe` overlay3

        it "carries wgpLocationStamped across untouched — it stays a chunk \
           \property (#424)" $
            withMigratedPage $ \params →
                wgpLocationStamped params `shouldBe` HS.fromList [ChunkCoord 0 0]

        it "resolving the migrated page against the registry reproduces the \
           \original discovered / contents-spawned state exactly" $
            withMigratedPage $ \params →
                stateOf (expectGeometry
                            (resolveLegacyLocationInstances registry
                                (wgpLocationOverlay params)
                                (wgpLocationInstances params)))
                    `shouldBe` expectedMigratedState

        it "the resolved page re-encodes and round-trips its instance ids, \
           \lifecycles, content flags and allocator" $
            withMigratedPage $ \params → do
                let resolved = expectGeometry
                        (resolveLegacyLocationInstances registry
                            (wgpLocationOverlay params)
                            (wgpLocationInstances params))
                    params2 = params { wgpLocationInstances = resolved }
                case S.decode (S.encode params2) ∷ Either String WorldGenParams of
                    Left err → expectationFailure err
                    Right back → do
                        let back' = wgpLocationInstances back
                        identityOf back' `shouldBe` identityOf resolved
                        stateOf back'    `shouldBe` expectedMigratedState
                        lisNextId back'  `shouldBe` lisNextId resolved
                        lisPendingLegacy back' `shouldBe` Nothing

    -- #1796: the CHECKED construction, the second of the two
    -- complementary boundaries. The authored domain (covered under
    -- "Location spatial bounds") keeps YAML sane; it cannot prove
    -- anything about arbitrary chunk coordinates, because ChunkCoord
    -- holds unrestricted Ints and nothing caps them. This is where that
    -- proof lives: every anchor and translated bound is computed in
    -- Integer and the geometry is built only once all six components
    -- are known representable.
    --
    -- The constants below are exact for chunkSize 16 (anchor = cx*16+8)
    -- on a 64-bit Int, which is the only build this project targets.
    describe "checked geometry construction (#1796)" $ do
        let limitBox   = RelBounds (-2147483647) (-2147483647)
                                   2147483647 2147483647
            limitDef   = mkDef "limit" "At Limit" limitBox
            -- The extremal chunks for a SMALL box: the anchor itself is
            -- the binding constraint.
            maxAnchorChunk = 576460752303423487
            minAnchorChunk = -576460752303423488
            -- The extremal chunks for an AT-LIMIT box: the translation
            -- is the binding constraint, one whole authored limit
            -- earlier.
            maxLimitChunk  = 576460752169205759
            minLimitChunk  = -576460752169205760
            geomOf coord def = locationInstanceGeometry coord def
            failedOn coord def component =
                case locationInstanceGeometry coord def of
                    Left err → lgeComponent err ≡ component
                                   ∧ lgeDefId err ≡ ldId def
                                   ∧ lgeChunk err ≡ coord
                    Right _  → False

        -- Requirement 6: every accepted footprint keeps the geometry it
        -- had, and 'translateBounds' is the oracle for what that is.
        it "reproduces the ordinary, degenerate, negative-offset and \
           \cross-chunk footprints exactly, anchor and box alike" $ do
            let cases =
                    [ (ChunkCoord 0 0,       ruinDef)     -- ordinary
                    , (ChunkCoord 2 (-1),    ruinDef)     -- ordinary, offset
                    , (ChunkCoord (-3) 4,    campDef)     -- negative chunk
                    , (ChunkCoord 0 0,       mkDef "dot" "Dot"
                                                 (RelBounds 0 0 0 0))
                    , (ChunkCoord 7 (-9),    mkDef "neg" "Neg"
                                                 (RelBounds (-9) (-7) (-1) (-2)))
                    , (ChunkCoord (-2) 3,    mkDef "wide" "Wide"
                                                 (RelBounds (-40) (-40) 40 40))
                    ]
            forM_ cases $ \(coord, def) → do
                let ChunkCoord cx cy = coord
                    anchor = (cx * 16 + 8, cy * 16 + 8)
                geomOf coord def
                    `shouldBe` Right (anchor, translateBounds anchor
                                                  (ldBounds def))

        it "the anchor arithmetic is exact at the most extreme chunk \
           \coordinates a small box survives, on both sides" $ do
            fst <$> geomOf (ChunkCoord maxAnchorChunk 0) ruinDef
                `shouldBe` Right (9223372036854775800, 8)
            fst <$> geomOf (ChunkCoord minAnchorChunk 0) ruinDef
                `shouldBe` Right (-9223372036854775800, 8)

        it "one chunk further out, the ANCHOR itself is unrepresentable \
           \-- on both sides, and independently per axis" $ do
            failedOn (ChunkCoord (maxAnchorChunk + 1) 0) ruinDef "anchor.x"
                `shouldBe` True
            failedOn (ChunkCoord (minAnchorChunk - 1) 0) ruinDef "anchor.x"
                `shouldBe` True
            failedOn (ChunkCoord 0 (maxAnchorChunk + 1)) ruinDef "anchor.y"
                `shouldBe` True
            failedOn (ChunkCoord 0 (minAnchorChunk - 1)) ruinDef "anchor.y"
                `shouldBe` True

        -- Requirement 9's extremal-anchor case: an AT-LIMIT authored box
        -- at the furthest chunk whose translation still fits, asserted
        -- as an EXACT box -- and it must be ordered on both axes, which
        -- is precisely what a wrapped result would not be.
        it "accepts an at-limit authored box at the most positive chunk \
           \whose translation is still representable, with exact ordered \
           \bounds" $ do
            let expected = AbsBounds 9223372032559808505 (-2147483639)
                                     9223372036854775799 2147483655
            geomOf (ChunkCoord maxLimitChunk 0) limitDef
                `shouldBe` Right ((9223372034707292152, 8), expected)
            abMinX expected `shouldSatisfy` (≤ abMaxX expected)
            abMinY expected `shouldSatisfy` (≤ abMaxY expected)

        it "accepts an at-limit authored box at the most negative chunk \
           \whose translation is still representable, with exact ordered \
           \bounds" $ do
            let expected = AbsBounds (-9223372036854775799) (-2147483639)
                                     (-9223372032559808505) 2147483655
            geomOf (ChunkCoord minLimitChunk 0) limitDef
                `shouldBe` Right ((-9223372034707292152, 8), expected)
            abMinX expected `shouldSatisfy` (≤ abMaxX expected)
            abMinY expected `shouldSatisfy` (≤ abMaxY expected)

        it "one chunk further out, the TRANSLATION is unrepresentable \
           \even though the anchor still fits -- on both sides" $ do
            -- The anchor alone is fine at these coordinates ...
            locationAnchorTileChecked (ChunkCoord (maxLimitChunk + 1) 0)
                `shouldSatisfy` isRight'
            locationAnchorTileChecked (ChunkCoord (minLimitChunk - 1) 0)
                `shouldSatisfy` isRight'
            -- ... it is the at-limit box's own edge that does not fit.
            failedOn (ChunkCoord (maxLimitChunk + 1) 0) limitDef
                     "bounds.max_x" `shouldBe` True
            failedOn (ChunkCoord (minLimitChunk - 1) 0) limitDef
                     "bounds.min_x" `shouldBe` True

        it "never returns an inverted box: a refused placement yields no \
           \geometry at all" $ do
            let refused = [ geomOf (ChunkCoord (maxAnchorChunk + 1) 0) ruinDef
                          , geomOf (ChunkCoord (minAnchorChunk - 1) 0) ruinDef
                          , geomOf (ChunkCoord (maxLimitChunk + 1) 0) limitDef
                          , geomOf (ChunkCoord (minLimitChunk - 1) 0) limitDef ]
            refused `shouldSatisfy` all (not ∘ isRight')

        it "reports the definition id, the chunk, the component and the \
           \exact unrepresentable value" $
            case locationInstanceGeometry
                     (ChunkCoord (maxAnchorChunk + 1) 0) ruinDef of
                Right _  → expectationFailure "expected a geometry failure"
                Left err → do
                    lgeDefId err     `shouldBe` "ruin"
                    lgeChunk err     `shouldBe`
                        ChunkCoord (maxAnchorChunk + 1) 0
                    lgeComponent err `shouldBe` "anchor.x"
                    lgeValue err     `shouldBe` 9223372036854775816
                    locationGeometryErrorText err `shouldSatisfy`
                        T.isInfixOf "anchor.x"
                    locationGeometryErrorText err `shouldSatisfy`
                        T.isInfixOf "ruin"

        -- Requirement 5: the failure is ATOMIC at every construction
        -- path. None of the three may hand back a table that is missing
        -- one instance, or an allocator that counted one it never built.
        describe "atomic propagation" $ do
            let badChunk  = ChunkCoord (maxAnchorChunk + 1) 0
                badOverlay = HM.fromList
                    [ (ChunkCoord 0 0, "ruin")
                    , (badChunk,       "ruin") ]

            it "buildLocationInstances returns NO partial table when one \
               \placement is unrepresentable" $
                buildLocationInstances Nothing registry badOverlay
                    `shouldSatisfy` (not ∘ isRight')

            it "buildLocationInstances still succeeds when every placement \
               \is representable" $
                buildLocationInstances Nothing registry overlay3
                    `shouldBe` Right instances3

            it "allocateLocationInstance fails without advancing the \
               \allocator or inserting anything" $ do
                allocateLocationInstance Nothing badChunk ruinDef instances3
                    `shouldSatisfy` (not ∘ isRight')
                -- The table it was handed is untouched, so the very next
                -- successful allocation still gets the id the refused one
                -- would have taken.
                case allocateLocationInstance Nothing (ChunkCoord 9 9)
                         ruinDef instances3 of
                    Left _ → expectationFailure "expected a successful allocation"
                    Right (iid, after) → do
                        unLocationInstanceId iid `shouldBe` 4
                        lisNextId after `shouldBe` 5

            it "resolveLegacyLocationInstances propagates the failure \
               \rather than resolving into wrapped geometry" $
                resolveLegacyLocationInstances registry badOverlay
                        (pendingLegacyFlags mempty mempty)
                    `shouldSatisfy` (not ∘ isRight')

            it "resolveLegacyLocationInstances still leaves a table with \
               \nothing pending untouched, bad overlay or not" $
                resolveLegacyLocationInstances registry badOverlay instances3
                    `shouldBe` Right instances3

isRight' ∷ Either a b → Bool
isRight' = either (const False) (const True)
