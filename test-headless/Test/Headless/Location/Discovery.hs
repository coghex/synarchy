{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | "Location discovery" (#780): the pure undiscovered→discovered
--   transition detector 'Location.Discovery.findDiscoveries' — bounds/
--   margin/faction/page/seam scenarios — plus the persisted discovery
--   state's default, its independence from the stamped/contents-spawned
--   flags, and its save round-trip. Since #911 that state is the
--   instance's 'Location.Instance.liLifecycle' rather than a chunk set. Mirrors 'Test.Headless.Building.Placement' and
--   'Test.Headless.Location.Bounds' fixture style; no engine needed —
--   see 'Test.Headless.World.LocationDiscovery' for the IO-level
--   (player-event + Lua query) coverage.
module Test.Headless.Location.Discovery (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import Location.Types
    ( LocationDef(..), LocationRegistry, emptyLocationRegistry
    , registerLocation
    )
import Location.Overlay.Types (LocationOverlay)
import Location.Bounds (RelBounds(..))
import Location.Discovery
    (DiscoveryHit(..), findDiscoveries, AwarenessHit(..), findAwareness)
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..), LocationInstances
    , LocationLifecycle(..), buildLocationInstances, instancesToList
    , markLocationContentsSpawned, setLocationLifecycle )
import Unit.Faction (Faction(..), allFactions, factionTag)
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)

-- * Fixtures — one ruin-shaped def (5x5 physical footprint, margin 6),
--   placed at chunk (0,0): anchor tile (8,8), physical AbsBounds
--   (6,6)..(10,10), expanded (discovery-margin) bounds (0,0)..(16,16).

locDef ∷ Text → LocationDef
locDef lid = LocationDef
    { ldId              = lid
    , ldLabel           = "Small Ruin"
    , ldType            = "ruin"
    , ldBuilder         = "room_small"
    , ldAnchor          = []
    , ldMaxCount        = 0
    , ldMinSpacing      = 0
    , ldContents        = []
    , ldBounds          = RelBounds (-2) (-2) 2 2
    , ldDiscoveryMargin = 6
    , ldMapIcons        = Nothing
    }

registry1 ∷ LocationRegistry
registry1 = registerLocation (locDef "loc1") emptyLocationRegistry

overlay1 ∷ LocationOverlay
overlay1 = HM.singleton (ChunkCoord 0 0) "loc1"

loc1Coord ∷ ChunkCoord
loc1Coord = ChunkCoord 0 0

-- | loc1's instance id — the first (and only) id 'buildLocationInstances'
--   allocates over 'overlay1'.
loc1Id ∷ LocationInstanceId
loc1Id = LocationInstanceId 1

-- | The instance table every scenario below runs against (#911).
instances1 ∷ LocationInstances
instances1 = buildLocationInstances registry1 overlay1

-- | 'instances1' with loc1 already at the given lifecycle state.
instancesAt ∷ LocationLifecycle → LocationInstances
instancesAt l = fromMaybe instances1 (setLocationLifecycle loc1Id l instances1)

seamInstances ∷ LocationInstances
seamInstances = buildLocationInstances registry1 seamOverlay

hit1 ∷ DiscoveryHit Int
hit1 = DiscoveryHit loc1Id loc1Coord (8, 8) "Small Ruin" 1

-- | One player unit (id 1) at the given tile; no other units present.
playerAt ∷ Int → Int → [(Int, Faction, Int, Int)]
playerAt gx gy = [ (1, FactionPlayer, gx, gy) ]

-- | The overlay/registry pair used by the cylindrical-seam tests: loc1
--   re-anchored at chunk (1,0) in a 2-chunk-wide world (worldWidthTiles
--   32, halfW 16) — anchor tile (24,8), physical bounds (22,6)..(26,10),
--   expanded bounds (16,0)..(32,16). Mirrors
--   'Test.Headless.Building.Placement's seam fixture.
seamOverlay ∷ LocationOverlay
seamOverlay = HM.singleton (ChunkCoord 1 0) "loc1"

spec ∷ Spec
spec = describe "Location discovery" $ do

    describe "findDiscoveries: bounds + margin" $ do
        it "a player unit outside the expanded bounds does not discover it" $
            findDiscoveries 0 instances1 (playerAt 17 8)
                `shouldBe` []

        describe "a player unit exactly on every expanded edge/corner discovers it" $
            forM_ [ ("west edge", 0, 8), ("east edge", 16, 8)
                  , ("north edge", 8, 0), ("south edge", 8, 16)
                  , ("nw corner", 0, 0), ("ne corner", 16, 0)
                  , ("sw corner", 0, 16), ("se corner", 16, 16)
                  ] $ \(label, gx, gy) →
                it label $
                    findDiscoveries 0 instances1 (playerAt gx gy)
                        `shouldBe` [hit1]

        it "a player unit inside the margin but outside the physical \
           \structure discovers it" $
            -- (8,2): x=8 is within the physical x-range (6..10), but
            -- y=2 is outside the physical y-range (6..10) — inside the
            -- expanded (0..16) halo only.
            findDiscoveries 0 instances1 (playerAt 8 2)
                `shouldBe` [hit1]

        it "a player unit inside the physical bounds discovers it" $
            findDiscoveries 0 instances1 (playerAt 8 8)
                `shouldBe` [hit1]

    describe "findDiscoveries: player-OWNERSHIP contract (#912)" $ do
        -- Every non-player faction, including the RECOGNIZED debug one.
        -- Using the real 'FactionDebug' is the point: debug is allied
        -- with the player and takes player orders, so a model that
        -- answered "friendly?" instead of "owned?" would newly discover
        -- here. An unrecognized tag would have proved nothing.
        forM_ [f | f ← allFactions, f ≢ FactionPlayer] $ \fid →
            it (T.unpack (factionTag fid)
                 <> " standing inside never discovers it") $
                findDiscoveries 0 instances1 [(1, fid, 8, 8)]
                    `shouldBe` []

        it "a debug unit inside does not discover it even while a player \
           \unit stands outside the halo" $
            -- The pairing that catches an ownership→alliance collapse
            -- most directly: the only unit in range is debug.
            findDiscoveries 0 instances1
                [ (1, FactionDebug, 8, 8), (2, FactionPlayer, 40, 40) ]
                    `shouldBe` []

        it "a player unit still discovers it with debug units alongside" $
            -- …and the same scene with the player inside DOES fire, so
            -- the case above is proving exclusion, not a dead fixture.
            findDiscoveries 0 instances1
                [ (1, FactionDebug, 8, 8), (2, FactionPlayer, 8, 8) ]
                    `shouldBe` [DiscoveryHit loc1Id loc1Coord (8, 8)
                                             "Small Ruin" 2]

    describe "findDiscoveries: page scoping" $
        it "the same coordinates on a different page have independent \
           \discovered state" $ do
            -- Page A: fresh (nothing discovered yet) — the unit inside
            -- discovers it.
            findDiscoveries 0 instances1 (playerAt 8 8) `shouldBe` [hit1]
            -- Page B: same definition and coordinate, but ITS OWN
            -- instance table already has loc1 discovered — a caller
            -- passes each page's own persisted state, so this never
            -- re-fires even though the inputs are otherwise identical.
            findDiscoveries 0 (instancesAt LifecycleDiscovered) (playerAt 8 8)
                `shouldBe` []

    describe "findDiscoveries: cylindrical U-seam (mirrors #777's contract)" $ do
        it "a seam-adjacent point is NOT discovered under raw (non-wrapping) coords" $
            findDiscoveries 0 seamInstances (playerAt 8 24)
                `shouldBe` []
        it "the same point IS discovered once the seam wrap is considered" $
            findDiscoveries 2 seamInstances (playerAt 8 24)
                `shouldBe` [DiscoveryHit loc1Id (ChunkCoord 1 0) (24, 8)
                                          "Small Ruin" 1]
        it "a raw-coordinate alias that is not physically inside on both \
           \axes is never discovered, even under wrapping" $
            -- x=24 lands inside the (unshifted) expanded x-range
            -- (16..32) by coincidence, but y=100 is nowhere near any
            -- seam alias's y-range — containment requires both axes,
            -- not a single coincidental one.
            findDiscoveries 2 seamInstances (playerAt 24 100)
                `shouldBe` []

    describe "findDiscoveries: idempotency" $
        it "a location already in the discovered set never re-fires, \
           \even with the same qualifying unit still inside" $ do
            let firstTick = findDiscoveries 0 instances1 (playerAt 8 8)
            firstTick `shouldBe` [hit1]
            let afterFirst = foldr (\h acc →
                    fromMaybe acc (setLocationLifecycle (dhInstance h)
                                       LifecycleDiscovered acc))
                    instances1 firstTick
            findDiscoveries 0 afterFirst (playerAt 8 8) `shouldBe` []

    describe "lifecycle no-regression (#911)" $ do
        forM_ [ LifecycleDiscovered, LifecycleActive
              , LifecycleCleared, LifecycleDepleted ] $ \l →
            it ("an instance already " <> show l
                <> " is never re-reported as a discovery") $
                findDiscoveries 0 (instancesAt l) (playerAt 8 8) `shouldBe` []

        it "a hinted instance still promotes to discovered by proximity" $
            findDiscoveries 0 (instancesAt LifecycleHinted) (playerAt 8 8)
                `shouldBe` [hit1]

        it "discovery never downgrades a later lifecycle state" $ do
            let cleared = instancesAt LifecycleCleared
            setLocationLifecycle loc1Id LifecycleDiscovered cleared
                `shouldBe` Nothing

    describe "findAwareness: per-unit location knowledge (#915)" $ do
        let aware1 uid = AwarenessHit loc1Id loc1Coord (8, 8) "Small Ruin" uid

        it "reports the qualifying player unit inside the halo" $
            findAwareness 0 instances1 (playerAt 8 8) `shouldBe` [aware1 1]

        it "reports nothing for a unit outside the expanded bounds" $
            findAwareness 0 instances1 (playerAt 17 8) `shouldBe` []

        it "reports EVERY qualifying unit, not just the discoverer — two \
           \acolytes in one halo both learn it" $
            -- findDiscoveries attributes the transition to the first
            -- qualifying unit alone; awareness must not inherit that.
            findAwareness 0 instances1
                [ (1, FactionPlayer, 8, 8), (2, FactionPlayer, 0, 0) ]
                `shouldBe` [aware1 1, aware1 2]

        it "keeps reporting a location that is ALREADY discovered — a \
           \unit arriving later still learns it" $ do
            -- The exact pairing that would break if awareness were
            -- gated on the one-time lifecycle promotion: the player-wide
            -- layer is finished with this location, the unit is not.
            forM_ [ LifecycleDiscovered, LifecycleActive
                  , LifecycleCleared, LifecycleDepleted ] $ \l → do
                findDiscoveries 0 (instancesAt l) (playerAt 8 8)
                    `shouldBe` []
                findAwareness 0 (instancesAt l) (playerAt 8 8)
                    `shouldBe` [aware1 1]

        describe "player-OWNERSHIP contract is shared with discovery (#912)" $ do
            forM_ [f | f ← allFactions, f ≢ FactionPlayer] $ \fid →
                it (T.unpack (factionTag fid)
                     <> " standing inside never gains awareness") $
                    findAwareness 0 instances1 [(1, fid, 8, 8)] `shouldBe` []

            it "a debug unit inside gains nothing while a player unit \
               \alongside gains it" $
                findAwareness 0 instances1
                    [ (1, FactionDebug, 8, 8), (2, FactionPlayer, 8, 8) ]
                    `shouldBe` [aware1 2]

        it "shares the seam-aware containment discovery uses — never a \
           \second, independently-drifting geometry" $ do
            findAwareness 0 seamInstances (playerAt 8 24) `shouldBe` []
            findAwareness 2 seamInstances (playerAt 8 24)
                `shouldBe` [AwarenessHit loc1Id (ChunkCoord 1 0) (24, 8)
                                          "Small Ruin" 1]

        it "carries the instance identity and anchor a memory keys on" $
            case findAwareness 0 instances1 (playerAt 8 8) of
                [h] → do
                    ahInstance h `shouldBe` loc1Id
                    ahAnchor h `shouldBe` (8, 8)
                other → expectationFailure ("expected one hit, got " <> show other)

        it "every discovery is also an awareness hit for the same unit \
           \and instance — the two layers cannot disagree about geometry" $ do
            let units = [ (1, FactionDebug, 8, 8), (2, FactionPlayer, 8, 8)
                        , (3, FactionPlayer, 0, 16), (4, FactionPlayer, 40, 40) ]
                discovered = findDiscoveries 0 instances1 units
                aware      = findAwareness 0 instances1 units
            [ (dhInstance d, dhUnit d) | d ← discovered ]
                `shouldBe` [(loc1Id, 2)]
            [ (ahInstance a, ahUnit a) | a ← aware ]
                `shouldBe` [(loc1Id, 2), (loc1Id, 3)]

    describe "WorldGenParams: discovery persistence" $ do
        it "every new location starts undiscovered by default" $
            map liLifecycle (instancesToList instances1)
                `shouldBe` [LifecycleUnknown]

        it "a page with no placed locations has an empty instance table" $
            instancesToList
                (wgpLocationInstances defaultWorldGenParams) `shouldBe` []

        it "is independent of the geometry-stamped and contents-spawned \
           \flags — both already true does not suppress a fresh discovery" $ do
            let p = defaultWorldGenParams
                    { wgpLocationOverlay = overlay1
                    , wgpLocationStamped = HS.singleton loc1Coord
                    , wgpLocationInstances = fromMaybe instances1
                        (pure (markContents instances1))
                    }
            findDiscoveries (wgpWorldSize p) (wgpLocationInstances p)
                             (playerAt 8 8)
                `shouldBe` [hit1]

        it "marking a location discovered never touches the stamped set or \
           \its contents-spawned flag" $ do
            let p0 = defaultWorldGenParams
                        { wgpLocationInstances = markContents instances1 }
                p1 = p0 { wgpLocationInstances =
                            instancesAtFrom (wgpLocationInstances p0)
                                             LifecycleDiscovered }
            wgpLocationStamped p1 `shouldBe` wgpLocationStamped p0
            map liContentsSpawned (instancesToList (wgpLocationInstances p1))
                `shouldBe` [True]

        it "round-trips a discovered location through the save encoding" $ do
            let p = defaultWorldGenParams
                    { wgpLocationInstances = instancesAt LifecycleDiscovered }
            case S.decode (S.encode p) of
                Right p' →
                    map liLifecycle (instancesToList (wgpLocationInstances p'))
                        `shouldBe` [LifecycleDiscovered]
                Left err → expectationFailure err

        it "round-trips an undiscovered location as lifecycle unknown" $ do
            let p = defaultWorldGenParams { wgpLocationInstances = instances1 }
            case S.decode (S.encode p) of
                Right p' →
                    map liLifecycle (instancesToList (wgpLocationInstances p'))
                        `shouldBe` [LifecycleUnknown]
                Left err → expectationFailure err

        it "two pages round-trip their discovery state independently" $ do
            let pA = defaultWorldGenParams
                        { wgpLocationInstances = instancesAt LifecycleDiscovered }
                pB = defaultWorldGenParams
                        { wgpLocationInstances = instances1 }
            case (S.decode (S.encode pA), S.decode (S.encode pB)) of
                (Right pA', Right pB') → do
                    map liLifecycle (instancesToList (wgpLocationInstances pA'))
                        `shouldBe` [LifecycleDiscovered]
                    map liLifecycle (instancesToList (wgpLocationInstances pB'))
                        `shouldBe` [LifecycleUnknown]
                _ → expectationFailure "round-trip decode failed"
  where
    markContents = markLocationContentsSpawned loc1Id
    instancesAtFrom lis l = fromMaybe lis (setLocationLifecycle loc1Id l lis)
