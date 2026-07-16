{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | "Remote portal warning" (#779): a valid starting-portal placement
--   whose footprint is more than 'remotePortalThresholdTiles' tiles
--   from every placed location on the SAME world page (seam-aware,
--   edge-to-edge) is classified remote and needs an explicit
--   confirmation before it spawns. Pure tests against
--   Building.Placement.remoteCheck/isRemote — the exact function
--   'Engine.Scripting.Lua.API.Buildings.Spawn.buildingRemoteCheckFn'
--   (building.remoteCheck) calls, so these tests exercise the same
--   page-scoped, building-kind-aware entry point the Lua build tool
--   uses, without needing a live engine.
module Test.Headless.Building.RemoteWarning (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import World.Chunk.Types (ChunkCoord(..))
import Building.Types
import Building.Placement (RemoteCheck(..), remoteCheck, isRemote)
import Location.Types
    ( LocationDef(..), LocationRegistry, emptyLocationRegistry
    , registerLocation
    )
import Location.Overlay.Types (LocationOverlay, emptyLocationOverlay)
import Location.Bounds (RelBounds(..), remotePortalThresholdTiles)

-- * Fixtures

mkDef ∷ Bool → Int → Int → BuildingDef
mkDef starting w h = BuildingDef
    { bdName            = "test_building"
    , bdDisplayName     = "Test Building"
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTexture         = TextureHandle 0
    , bdTileW           = w
    , bdTileH           = h
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = starting
    , bdRace            = "acolyte"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdStateAnims      = HM.empty
    , bdPowerDrain      = 0
    }

-- | The starting portal under test: a 1x1 footprint, is_starting=True.
portalDef ∷ BuildingDef
portalDef = mkDef True 1 1

-- | Same footprint, but NOT a starting building — must never warrant
--   the warning regardless of distance.
ordinaryDef ∷ BuildingDef
ordinaryDef = mkDef False 1 1

baseLoc ∷ Text → RelBounds → LocationDef
baseLoc lid bounds = LocationDef
    { ldId              = lid
    , ldLabel           = "Test Location"
    , ldType            = "ruin"
    , ldBuilder         = "room_small"
    , ldAnchor          = []
    , ldMaxCount        = 0
    , ldMinSpacing      = 0
    , ldContents        = []
    , ldBounds          = bounds
    , ldDiscoveryMargin = 6
    , ldMapIcons        = Nothing
    }

-- | Registers one location "loc" at chunk (0,0) (anchor tile (8,8)),
--   with the given RelBounds offset — since a RelBounds offset is an
--   arbitrary integer, this reaches any target absolute box exactly,
--   letting each test dial in a precise nearest-edge distance from a
--   portal footprint anchored at (0,0) without fighting the chunk
--   anchor's fixed (cx*16+8) placement.
registryAndOverlay ∷ RelBounds → (LocationRegistry, LocationOverlay)
registryAndOverlay bounds =
    ( registerLocation (baseLoc "loc" bounds) emptyLocationRegistry
    , HM.singleton (ChunkCoord 0 0) "loc"
    )

-- | A single location whose absolute box west edge sits at
--   @8 + relMinX@ tiles from the origin — placing a 1x1 portal
--   footprint at (0, 8) (same row) then measures a pure west-edge
--   Chebyshev gap of exactly @8 + relMinX@.
westEdgeAt ∷ Int → (LocationRegistry, LocationOverlay)
westEdgeAt relMinX =
    registryAndOverlay (RelBounds relMinX 0 (relMinX + 4) 4)

-- | 'remoteCheck' with the world size moved to the front — reads
--   better at each call site below (@checkAt worldSize locs overlay
--   def gx gy@) than repeating the positional 0 in the middle.
checkAt ∷ Int → LocationRegistry → LocationOverlay → BuildingDef → Int → Int
        → RemoteCheck
checkAt worldSize locs overlay def gx gy = remoteCheck locs overlay worldSize def gx gy

spec ∷ Spec
spec = describe "Remote portal warning" $ do

    describe "nearest-distance threshold (footprint at (0,8), edge test)" $ do
        it "below the threshold is not remote" $ do
            let (locs, overlay) = westEdgeAt 119 -- distance = 127
            checkAt 0 locs overlay portalDef 0 8
                `shouldBe` RemoteDistance (Just 127)
            isRemote (checkAt 0 locs overlay portalDef 0 8) `shouldBe` False

        it "exactly at the threshold is not remote" $ do
            let (locs, overlay) = westEdgeAt 120 -- distance = 128
            checkAt 0 locs overlay portalDef 0 8
                `shouldBe` RemoteDistance (Just remotePortalThresholdTiles)
            isRemote (checkAt 0 locs overlay portalDef 0 8) `shouldBe` False

        it "one tile beyond the threshold is remote" $ do
            let (locs, overlay) = westEdgeAt 121 -- distance = 129
            checkAt 0 locs overlay portalDef 0 8
                `shouldBe` RemoteDistance (Just 129)
            isRemote (checkAt 0 locs overlay portalDef 0 8) `shouldBe` True

    describe "distance to the nearest edge rather than location center" $
        it "measures the near edge (8), not the geometric center (58)" $ do
            -- Location box spans x in [8,108] (anchor (8,8), RelBounds
            -- 0..100) — a center-based measure from footprint (0,8)
            -- would report 58; edge-to-edge reports 8.
            let (locs, overlay) = registryAndOverlay (RelBounds 0 0 100 4)
            checkAt 0 locs overlay portalDef 0 8
                `shouldBe` RemoteDistance (Just 8)

    describe "multiple locations select the true nearest" $
        it "reports the nearer location's distance, not the farther one" $ do
            let nearLoc = baseLoc "loc_near" (RelBounds 20 0 24 4)   -- dist 28
                farLoc  = baseLoc "loc_far"  (RelBounds 500 0 504 4) -- dist 508
                registry = registerLocation farLoc
                            (registerLocation nearLoc emptyLocationRegistry)
                overlay = HM.fromList
                    [ (ChunkCoord 0 0, "loc_near")
                    , (ChunkCoord 1 0, "loc_far")
                    ]
            checkAt 0 registry overlay portalDef 0 8
                `shouldBe` RemoteDistance (Just 28)

    describe "no placed locations" $
        it "treats every placement as remote, distance unknown" $ do
            checkAt 0 emptyLocationRegistry emptyLocationOverlay portalDef 0 0
                `shouldBe` RemoteDistance Nothing
            isRemote (checkAt 0 emptyLocationRegistry emptyLocationOverlay
                              portalDef 0 0)
                `shouldBe` True

    describe "a nearby location across the cylindrical U seam" $ do
        -- worldSize 25 chunks -> worldWidthTiles 400, halfW 200. The
        -- location's absolute box is (198,-200)..(202,-200); the
        -- portal footprint is a single tile at (0,0). Raw (non-
        -- wrapping) distance is 200 tiles (comfortably remote); once
        -- the seam is considered, shifting the footprint by
        -- (+halfW,-halfW) lands it INSIDE the location's box (distance
        -- 0) — the u=gx-gy axis wrap makes them physical neighbours
        -- even though the raw coordinates are far apart (mirrors
        -- Test.Headless.Location.Bounds's seam contract).
        let (locs, overlay) = registryAndOverlay (RelBounds 190 (-208) 194 (-208))

        it "looks remote under a non-wrapping distance" $
            checkAt 0 locs overlay portalDef 0 0
                `shouldBe` RemoteDistance (Just 200)

        it "is not remote once the seam wrap is considered" $ do
            checkAt 25 locs overlay portalDef 0 0
                `shouldBe` RemoteDistance (Just 0)
            isRemote (checkAt 25 locs overlay portalDef 0 0) `shouldBe` False

    describe "a location on another world page being ignored" $
        it "only counts locations in the overlay actually passed for this page" $ do
            let nearLoc = baseLoc "loc_other_page" (RelBounds 0 0 4 4) -- dist 8
                registry = registerLocation nearLoc emptyLocationRegistry
                otherPageOverlay = HM.singleton (ChunkCoord 0 0) "loc_other_page"

            -- On the page that actually places it, the portal is close.
            checkAt 0 registry otherPageOverlay portalDef 0 8
                `shouldBe` RemoteDistance (Just 8)

            -- The SAME registry with THIS page's own (empty) overlay
            -- never sees a location placed only on another page's
            -- overlay — callers always pass the target page's own
            -- overlay (World.Thread.Command.Init / Buildings.Spawn
            -- read wsGenParamsRef off the exact page being checked),
            -- so cross-page isolation falls out of that scoping rather
            -- than anything remoteCheck itself branches on.
            checkAt 0 registry emptyLocationOverlay portalDef 0 8
                `shouldBe` RemoteDistance Nothing

    describe "non-starting buildings never requiring the warning" $
        it "reports NotStartingBuilding regardless of distance, even with no locations" $ do
            checkAt 0 emptyLocationRegistry emptyLocationOverlay
                    ordinaryDef 0 0
                `shouldBe` NotStartingBuilding
            isRemote (checkAt 0 emptyLocationRegistry emptyLocationOverlay
                              ordinaryDef 0 0)
                `shouldBe` False
