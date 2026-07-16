{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | "Location discovery" (#780): the pure undiscovered→discovered
--   transition detector 'Location.Discovery.findDiscoveries' — bounds/
--   margin/faction/page/seam scenarios — plus the persisted
--   'World.Generate.Types.wgpLocationDiscovered' field's default,
--   independence from the stamped/contents-spawned flags, and save
--   round-trip. Mirrors 'Test.Headless.Building.Placement' and
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
    }

registry1 ∷ LocationRegistry
registry1 = registerLocation (locDef "loc1") emptyLocationRegistry

overlay1 ∷ LocationOverlay
overlay1 = HM.singleton (ChunkCoord 0 0) "loc1"

loc1Coord ∷ ChunkCoord
loc1Coord = ChunkCoord 0 0

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
            findDiscoveries 0 registry1 overlay1 HS.empty (playerAt 17 8)
                `shouldBe` []

        describe "a player unit exactly on every expanded edge/corner discovers it" $
            forM_ [ ("west edge", 0, 8), ("east edge", 16, 8)
                  , ("north edge", 8, 0), ("south edge", 8, 16)
                  , ("nw corner", 0, 0), ("ne corner", 16, 0)
                  , ("sw corner", 0, 16), ("se corner", 16, 16)
                  ] $ \(label, gx, gy) →
                it label $
                    findDiscoveries 0 registry1 overlay1 HS.empty (playerAt gx gy)
                        `shouldBe` [DiscoveryHit loc1Coord (8, 8) "Small Ruin" 1]

        it "a player unit inside the margin but outside the physical \
           \structure discovers it" $
            -- (8,2): x=8 is within the physical x-range (6..10), but
            -- y=2 is outside the physical y-range (6..10) — inside the
            -- expanded (0..16) halo only.
            findDiscoveries 0 registry1 overlay1 HS.empty (playerAt 8 2)
                `shouldBe` [DiscoveryHit loc1Coord (8, 8) "Small Ruin" 1]

        it "a player unit inside the physical bounds discovers it" $
            findDiscoveries 0 registry1 overlay1 HS.empty (playerAt 8 8)
                `shouldBe` [DiscoveryHit loc1Coord (8, 8) "Small Ruin" 1]

    describe "findDiscoveries: player-control faction contract" $
        forM_ ["hostile", "wildlife", "neutral", "debug_faction"] $ \fid →
            it (T.unpack fid <> " standing inside never discovers it") $
                findDiscoveries 0 registry1 overlay1 HS.empty
                    [(1, fid, 8, 8)]
                    `shouldBe` []

    describe "findDiscoveries: page scoping" $
        it "the same coordinates on a different page have independent \
           \discovered state" $ do
            -- Page A: fresh (nothing discovered yet) — the unit inside
            -- discovers it.
            findDiscoveries 0 registry1 overlay1 HS.empty (playerAt 8 8)
                `shouldBe` [DiscoveryHit loc1Coord (8, 8) "Small Ruin" 1]
            -- Page B: same registry/overlay/coordinate, but ITS OWN
            -- discovered set already contains loc1Coord — a caller
            -- passes each page's own persisted state, so this never
            -- re-fires even though the inputs are otherwise identical.
            findDiscoveries 0 registry1 overlay1 (HS.singleton loc1Coord)
                (playerAt 8 8)
                `shouldBe` []

    describe "findDiscoveries: cylindrical U-seam (mirrors #777's contract)" $ do
        it "a seam-adjacent point is NOT discovered under raw (non-wrapping) coords" $
            findDiscoveries 0 registry1 seamOverlay HS.empty (playerAt 8 24)
                `shouldBe` []
        it "the same point IS discovered once the seam wrap is considered" $
            findDiscoveries 2 registry1 seamOverlay HS.empty (playerAt 8 24)
                `shouldBe` [DiscoveryHit (ChunkCoord 1 0) (24, 8) "Small Ruin" 1]
        it "a raw-coordinate alias that is not physically inside on both \
           \axes is never discovered, even under wrapping" $
            -- x=24 lands inside the (unshifted) expanded x-range
            -- (16..32) by coincidence, but y=100 is nowhere near any
            -- seam alias's y-range — containment requires both axes,
            -- not a single coincidental one.
            findDiscoveries 2 registry1 seamOverlay HS.empty (playerAt 24 100)
                `shouldBe` []

    describe "findDiscoveries: idempotency" $
        it "a location already in the discovered set never re-fires, \
           \even with the same qualifying unit still inside" $ do
            let firstTick = findDiscoveries 0 registry1 overlay1 HS.empty
                                             (playerAt 8 8)
            firstTick `shouldBe` [DiscoveryHit loc1Coord (8, 8) "Small Ruin" 1]
            let discoveredNow = HS.fromList (map dhCoord firstTick)
            findDiscoveries 0 registry1 overlay1 discoveredNow (playerAt 8 8)
                `shouldBe` []

    describe "WorldGenParams: discovery persistence" $ do
        it "every new location starts undiscovered by default" $
            wgpLocationDiscovered defaultWorldGenParams `shouldBe` HS.empty

        it "is independent of the geometry-stamped and contents-spawned \
           \flags — both already true does not suppress a fresh discovery" $ do
            let p = defaultWorldGenParams
                    { wgpLocationOverlay = overlay1
                    , wgpLocationStamped = HS.singleton loc1Coord
                    , wgpLocationContentsSpawned = HS.singleton loc1Coord
                    }
            findDiscoveries (wgpWorldSize p) registry1 (wgpLocationOverlay p)
                             (wgpLocationDiscovered p) (playerAt 8 8)
                `shouldBe` [DiscoveryHit loc1Coord (8, 8) "Small Ruin" 1]

        it "marking a location discovered never touches the stamped or \
           \contents-spawned sets" $ do
            let p0 = defaultWorldGenParams
                p1 = p0 { wgpLocationDiscovered = HS.singleton loc1Coord }
            wgpLocationStamped p1 `shouldBe` wgpLocationStamped p0
            wgpLocationContentsSpawned p1 `shouldBe` wgpLocationContentsSpawned p0

        it "round-trips a discovered location through the save encoding" $ do
            let p = defaultWorldGenParams
                    { wgpLocationDiscovered = HS.fromList
                        [ChunkCoord 0 0, ChunkCoord 2 3]
                    }
            case S.decode (S.encode p) of
                Right p' → wgpLocationDiscovered p' `shouldBe` wgpLocationDiscovered p
                Left err → expectationFailure err

        it "round-trips an undiscovered location as an empty set" $
            case S.decode (S.encode defaultWorldGenParams) of
                Right p' → wgpLocationDiscovered p' `shouldBe` HS.empty
                Left err → expectationFailure err

        it "two pages round-trip their discovery state independently" $ do
            let pA = defaultWorldGenParams
                        { wgpLocationDiscovered = HS.singleton (ChunkCoord 0 0) }
                pB = defaultWorldGenParams
                        { wgpLocationDiscovered = HS.empty }
            case (S.decode (S.encode pA), S.decode (S.encode pB)) of
                (Right pA', Right pB') → do
                    wgpLocationDiscovered pA' `shouldBe` HS.singleton (ChunkCoord 0 0)
                    wgpLocationDiscovered pB' `shouldBe` HS.empty
                _ → expectationFailure "round-trip decode failed"
