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
import Location.Discovery (DiscoveryHit(..), findDiscoveries)
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..), LocationInstances
    , LocationLifecycle(..), buildLocationInstances, instancesToList
    , markLocationContentsSpawned, setLocationLifecycle )
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
playerAt ∷ Int → Int → [(Int, Text, Int, Int)]
playerAt gx gy = [ (1, "player", gx, gy) ]

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

    describe "findDiscoveries: player-control faction contract" $
        forM_ ["hostile", "wildlife", "neutral", "debug_faction"] $ \fid →
            it (T.unpack fid <> " standing inside never discovers it") $
                findDiscoveries 0 instances1 [(1, fid, 8, 8)]
                    `shouldBe` []

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
