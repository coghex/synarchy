{-# LANGUAGE Strict #-}
-- | "Location instance identity" (#911): the pure allocator, lifecycle,
--   and pre-#911 chunk-set → instance migration that
--   'Location.Instance' owns, plus the @world-pages@ v1→v2 component
--   migration ('World.Save.Component.Page.migrateWorldPagesV1') that
--   feeds it. No engine needed — mirrors
--   'Test.Headless.Location.Discovery' fixture style; the IO-level
--   discovery coverage lives in 'Test.Headless.World.LocationDiscovery'.
module Test.Headless.Location.Instance (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import Location.Bounds (AbsBounds(..), RelBounds(..))
import Location.Instance
import Location.Overlay.Types (LocationOverlay, emptyLocationOverlay)
import Location.Types
    ( LocationDef(..), LocationRegistry, emptyLocationRegistry
    , registerLocation )
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Save.Component.Page
    ( PageCoreDTOv1(..), WorldPagesDTOv1(..), WorldPages(..)
    , migrateWorldPagesV1, toWorldGenParamsDTOv1, WorldIdentityDTO(..) )
import World.Save.Snapshot (PageSnapshot(..))

-- * Fixtures — two ruin-shaped defs. "ruin" is 5x5 (margin 6); "camp"
--   is a distinct 3x3 (margin 2) so a test can tell the two apart by
--   their stored geometry alone.

ruinDef, campDef ∷ LocationDef
ruinDef = mkDef "ruin" "Small Ruin" (RelBounds (-2) (-2) 2 2) 6
campDef = mkDef "camp" "Old Camp"   (RelBounds (-1) (-1) 1 1) 2

mkDef ∷ Text → Text → RelBounds → Int → LocationDef
mkDef lid label bounds margin = LocationDef
    { ldId              = lid
    , ldLabel           = label
    , ldType            = "ruin"
    , ldBuilder         = "room_small"
    , ldAnchor          = []
    , ldMaxCount        = 0
    , ldMinSpacing      = 0
    , ldContents        = []
    , ldBounds          = bounds
    , ldDiscoveryMargin = margin
    , ldMapIcons        = Nothing
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
instances3 = buildLocationInstances registry overlay3

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
    let (a, l1) = allocateLocationInstance (ChunkCoord 7 7) ruinDef
                      emptyLocationInstances
        (b, l2) = allocateLocationInstance (ChunkCoord 7 7) campDef l1
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

        it "reproduces the same id → (definition, anchor) mapping when the \
           \same placement is recomputed" $
            identityOf (buildLocationInstances registry overlay3)
                `shouldBe` identityOf instances3

        it "leaves the allocator strictly above every live id" $ do
            lisNextId instances3 `shouldBe` 4
            locationInstanceAllocatorErrors instances3 `shouldBe` []

        it "reserves an id for an overlay entry whose definition is not \
           \registered, so the remaining ids do not shift" $ do
            let partial = buildLocationInstances
                    (registerLocation campDef emptyLocationRegistry) overlay3
            -- Only "camp" resolves; it keeps id 1 (its (cx,cy) slot), and
            -- the allocator still accounts for all three placements.
            map (\i → (unLocationInstanceId (liId i), liDefId i))
                (instancesToList partial) `shouldBe` [(1, "camp")]
            lisNextId partial `shouldBe` 4

        it "an empty overlay yields an empty table with a fresh allocator" $ do
            let none = buildLocationInstances registry emptyLocationOverlay
            instancesToList none `shouldBe` []
            lisNextId none `shouldBe` firstLocationInstanceId

        it "allocateLocationInstance hands out the next id and advances it" $ do
            let (iid, after) = allocateLocationInstance (ChunkCoord 9 9)
                                   ruinDef instances3
            unLocationInstanceId iid `shouldBe` 4
            lisNextId after `shouldBe` 5
            locationInstanceAllocatorErrors after `shouldBe` []

        it "flags an instance at or above the page's allocator" $
            locationInstanceAllocatorErrors (instances3 { lisNextId = 2 })
                `shouldSatisfy` (not . null)

        it "flags an instance stored under a key that is not its own id" $ do
            let inst = newLocationInstance (LocationInstanceId 1)
                           (ChunkCoord 0 0) ruinDef
                broken = LocationInstances
                    { lisNextId        = 9
                    , lisById          = HM.singleton (LocationInstanceId 5) inst
                    , lisPendingLegacy = Nothing }
            locationInstanceAllocatorErrors broken `shouldSatisfy` (not . null)

    describe "stored geometry and display name" $ do
        it "stores the anchor, resolved absolute bounds, margin and name \
           \from the definition at placement time" $
            case lookupLocationInstance (LocationInstanceId 2) instances3 of
                Nothing → expectationFailure "expected instance #2"
                Just inst → do
                    liDefId inst           `shouldBe` "ruin"
                    liAnchor inst          `shouldBe` (8, 8)
                    liBounds inst          `shouldBe` AbsBounds 6 6 10 10
                    liDiscoveryMargin inst `shouldBe` 6
                    liDisplayName inst     `shouldBe` "Small Ruin"

        it "keeps its stored geometry when the definition is edited later — \
           \nothing re-derives it from the registry" $ do
            let edited = registerLocation
                    (mkDef "ruin" "Renamed" (RelBounds (-9) (-9) 9 9) 99)
                    registry
            -- The ONLY registry-consulting path after placement is the v1
            -- migration, and it is a no-op on an already-resolved table.
            resolveLegacyLocationInstances edited overlay3 instances3
                `shouldBe` instances3

        it "the anchor is the hosting chunk's centre tile" $
            locationAnchorTile (ChunkCoord (-3) 4) `shouldBe` (-40, 72)

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

    describe "pre-#911 chunk-set migration" $ do
        let migrated = resolveLegacyLocationInstances registry overlay3 legacyFlags

        it "creates one instance per overlay entry, with the same ids a \
           \fresh placement of that overlay would allocate" $
            identityOf migrated `shouldBe` identityOf instances3

        it "maps each chunk's discovered / contents-spawned marker onto the \
           \instance occupying it, and nothing else" $
            stateOf migrated `shouldBe` expectedMigratedState

        it "resolves each instance's bounds / margin / name against the \
           \registered definition" $
            map (\i → (liBounds i, liDiscoveryMargin i, liDisplayName i))
                (instancesToList migrated)
                `shouldBe`
                [ (AbsBounds (-41) 71 (-39) 73, 2, "Old Camp")
                , (AbsBounds 6 6 10 10,         6, "Small Ruin")
                , (AbsBounds 38 (-10) 42 (-6),  6, "Small Ruin")
                ]

        it "discards a marker naming a chunk with no overlay entry — it \
           \identifies no placed instance" $ do
            let stray = pendingLegacyFlags
                    (HS.fromList [ChunkCoord 50 50])
                    (HS.fromList [ChunkCoord 60 60])
                out = resolveLegacyLocationInstances registry overlay3 stray
            map liLifecycle (instancesToList out)
                `shouldBe` replicate 3 LifecycleUnknown
            map liContentsSpawned (instancesToList out)
                `shouldBe` replicate 3 False

        it "clears the pending carry, and is idempotent" $ do
            lisPendingLegacy migrated `shouldBe` Nothing
            resolveLegacyLocationInstances registry overlay3 migrated
                `shouldBe` migrated

        it "leaves a table with nothing pending untouched" $
            resolveLegacyLocationInstances registry overlay3 instances3
                `shouldBe` instances3

    describe "world-pages v1 → v2 component migration" $ do
        let v1Params = defaultWorldGenParams
                { wgpSeed              = 4242
                , wgpLocationOverlay   = overlay3
                , wgpLocationStamped   = HS.fromList [ChunkCoord 0 0]
                , wgpLocationInstances =
                    resolveLegacyLocationInstances registry overlay3 legacyFlags
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
                , pc1Identity   = Just (WorldIdentityDTO "Terra" Nothing)
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
                stateOf (resolveLegacyLocationInstances registry
                            (wgpLocationOverlay params)
                            (wgpLocationInstances params))
                    `shouldBe` expectedMigratedState

        it "the resolved page re-encodes and round-trips its instance ids, \
           \lifecycles, content flags and allocator" $
            withMigratedPage $ \params → do
                let resolved = resolveLegacyLocationInstances registry
                        (wgpLocationOverlay params) (wgpLocationInstances params)
                    params2 = params { wgpLocationInstances = resolved }
                case S.decode (S.encode params2) ∷ Either String WorldGenParams of
                    Left err → expectationFailure err
                    Right back → do
                        let back' = wgpLocationInstances back
                        identityOf back' `shouldBe` identityOf resolved
                        stateOf back'    `shouldBe` expectedMigratedState
                        lisNextId back'  `shouldBe` lisNextId resolved
                        lisPendingLegacy back' `shouldBe` Nothing
