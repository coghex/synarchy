{-# LANGUAGE Strict #-}
-- | "Location discovery" (#780, sight-based since #1230): the pure
--   undiscovered→discovered transition detector
--   'Location.Discovery.findDiscoveries' — sight/bounds/faction/page/seam
--   scenarios — plus the persisted discovery
--   state's default, its independence from the stamped/contents-spawned
--   flags, and its save round-trip. Since #911 that state is the
--   instance's 'Location.Instance.liLifecycle' rather than a chunk set. Mirrors 'Test.Headless.Building.Placement' and
--   'Test.Headless.Location.Bounds' fixture style; no engine needed —
--   see 'Test.Headless.World.LocationDiscovery' for the IO-level
--   (player-event + Lua query) coverage.
module Test.Headless.Location.Discovery (spec, instances1, loc1Id) where

import UPrelude
import Test.Hspec
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import Location.Types
    ( LocationDef(..), LocationNaming(..), LocationRegistry
    , emptyLocationRegistry, registerLocation
    )
import Location.Overlay.Types (LocationOverlay)
import Location.Bounds (RelBounds(..))
import Location.Discovery
    ( DiscoveryHit(..), findDiscoveries, AwarenessHit(..), findAwareness
    , UnitSight(..) )
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..), LocationInstances
    , LocationLifecycle(..), buildLocationInstances, instancesToList
    , markLocationContentsSpawned, setLocationLifecycle )
import Test.Headless.Location.Fixture (expectGeometry)
import Unit.Faction (Faction(..), allFactions, factionTag)
import Unit.Faction.Membership
    (localController, resolveLegacyFaction, unitPolicyProfile)
import Unit.Faction.Profile
    ( FactionProfile, aiController, humanController, mkProfile, tagAcolyte )
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
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


-- * Fixtures — one ruin-shaped def (5x5 footprint) placed at chunk
--   (0,0): anchor tile (8,8), AbsBounds (6,6)..(10,10). #1230 removed
--   the discovery-margin halo entirely — that box IS the reveal
--   footprint now, and what varies between scenarios is which tiles a
--   unit can SEE.

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
    , ldMapIcon         = Nothing
    , ldNaming          = testNaming
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
instances1 = expectGeometry (buildLocationInstances Nothing registry1 overlay1)

-- | 'instances1' with loc1 already at the given lifecycle state.
instancesAt ∷ LocationLifecycle → LocationInstances
instancesAt l = fromMaybe instances1 (setLocationLifecycle loc1Id l instances1)

seamInstances ∷ LocationInstances
seamInstances =
    expectGeometry (buildLocationInstances Nothing registry1 seamOverlay)

hit1 ∷ DiscoveryHit Int
hit1 = DiscoveryHit loc1Id loc1Coord (8, 8) "Small Ruin" 1

-- | One player unit (id 1) that can see exactly the given tiles. The
--   sight sets below are written out literally rather than computed:
--   'Unit.LineOfSight.visibleTilesOnPage' owns radius/cone/occlusion and
--   is gated by its own spec, so what these scenarios pin is the
--   CONTACT rule — a location is revealed iff some seen tile lands
--   inside its bounds — independently of how the set was produced.
seeing ∷ [(Int, Int)] → [UnitSight Int]
seeing tiles = [ UnitSight 1 (legacySight FactionPlayer) tiles ]

-- | The sight of a unit STANDING at (gx, gy) and seeing nothing else —
--   a unit's own tile is always in its visible set
--   ('Unit.LineOfSight.visibleTilesOnPage'), so this is the minimal
--   honest sight for a unit at that tile and the direct analogue of the
--   old "unit at this position" fixture.
standingAt ∷ Int → Int → [UnitSight Int]
standingAt gx gy = seeing [(gx, gy)]

-- | Several units, each seeing only its own tile.
unitsAt ∷ [(Int, Faction, Int, Int)] → [UnitSight Int]
unitsAt us =
    [ UnitSight uid (legacySight f) [(gx, gy)] | (uid, f, gx, gy) ← us ]

-- | The policy profile a unit spawned under a legacy faction tag carries
--   (#2515's D-26 mapping, against the acolyte definition's authored
--   @acolyte@ default). The ownership filter reads this, never the tag.
legacySight ∷ Faction → FactionProfile
legacySight = unitPolicyProfile ∘ resolveLegacyFaction [tagAcolyte]

-- | An acolyte with NO controller: it shares the local roster's
--   @acolyte@ tag, so the policy calls the two allies, yet nobody owns
--   it (#2518).
uncontrolledAcolyte ∷ FactionProfile
uncontrolledAcolyte = mkProfile Nothing [tagAcolyte] []

-- | Units of controllers other than the local observer, sharing every
--   tag the local roster carries.
otherPlayerAcolyte, aiAcolyte ∷ FactionProfile
otherPlayerAcolyte = mkProfile (Just (humanController "player_2")) [tagAcolyte] []
aiAcolyte          = mkProfile (Just (aiController "raiders")) [tagAcolyte] []

-- | The overlay/registry pair used by the cylindrical-seam tests: loc1
--   re-anchored at chunk (1,0) in a 2-chunk-wide world (worldWidthTiles
--   32, halfW 16) — anchor tile (24,8), bounds (22,6)..(26,10). Mirrors
--   'Test.Headless.Building.Placement's seam fixture.
seamOverlay ∷ LocationOverlay
seamOverlay = HM.singleton (ChunkCoord 1 0) "loc1"

spec ∷ Spec
spec = describe "Location discovery" $ do

    describe "findDiscoveries: sight ∩ bounds (#1230)" $ do
        it "a player unit that sees nothing inside the bounds does not \
           \discover it, however close it stands" $
            -- (11,8) is one tile east of the bounds' east edge (6..10).
            -- Under the removed 6-tile halo this was a discovery; the
            -- contact rule is now containment in liBounds itself.
            findDiscoveries localController 0 instances1 (standingAt 11 8)
                `shouldBe` []

        describe "seeing ONE occupied tile is enough, on every edge and corner" $
            forM_ [ ("west edge", 6, 8), ("east edge", 10, 8)
                  , ("north edge", 8, 6), ("south edge", 8, 10)
                  , ("nw corner", 6, 6), ("ne corner", 10, 6)
                  , ("sw corner", 6, 10), ("se corner", 10, 10)
                  ] $ \(label, gx, gy) →
                it label $
                    -- The unit itself stands well outside the ruin and
                    -- sees exactly this one tile of it: reveal follows
                    -- SIGHT, never the unit's own position.
                    findDiscoveries localController 0 instances1 (seeing [(40, 40), (gx, gy)])
                        `shouldBe` [hit1]

        it "a unit standing outside but seeing into the bounds discovers it" $
            -- The case the whole issue exists for: no part of this unit
            -- is inside the ruin, and it still maps it.
            findDiscoveries localController 0 instances1 (seeing [(20, 20), (8, 8)])
                `shouldBe` [hit1]

        it "a unit standing INSIDE the bounds always discovers it — its \
           \own tile is always in its visible set" $
            findDiscoveries localController 0 instances1 (standingAt 8 8)
                `shouldBe` [hit1]

        it "a unit that sees many tiles, none of them the location's, \
           \discovers nothing" $
            findDiscoveries localController 0 instances1
                (seeing [ (x, y) | x ← [0 .. 5], y ← [0 .. 5] ])
                `shouldBe` []

        it "a unit with an EMPTY visible set discovers nothing, even \
           \standing on the anchor" $
            -- The degenerate input a caller could hand in for a unit on
            -- a page with no live state: no sight, no reveal — never an
            -- unearned discovery from position alone.
            findDiscoveries localController 0 instances1
                [UnitSight 1 (legacySight FactionPlayer) []]
                `shouldBe` []

    describe "findDiscoveries: player-OWNERSHIP contract (#912)" $ do
        -- Every non-player faction, including the RECOGNIZED debug one.
        -- Using the real 'FactionDebug' is the point: debug is allied
        -- with the player and takes player orders, so a model that
        -- answered "friendly?" instead of "owned?" would newly discover
        -- here. An unrecognized tag would have proved nothing.
        forM_ [f | f ← allFactions, f ≢ FactionPlayer] $ \fid →
            it (T.unpack (factionTag fid)
                 <> " that can see it never discovers it") $
                findDiscoveries localController 0 instances1 (unitsAt [(1, fid, 8, 8)])
                    `shouldBe` []

        it "a debug unit that can see it does not discover it even while \
           \a player unit stands out of sight of it" $
            -- The pairing that catches an ownership→alliance collapse
            -- most directly: the only unit that can see it is debug.
            findDiscoveries localController 0 instances1
                (unitsAt [ (1, FactionDebug, 8, 8), (2, FactionPlayer, 40, 40) ])
                    `shouldBe` []

        it "a player unit still discovers it with debug units alongside" $
            -- …and the same scene with the player inside DOES fire, so
            -- the case above is proving exclusion, not a dead fixture.
            findDiscoveries localController 0 instances1
                (unitsAt [ (1, FactionDebug, 8, 8), (2, FactionPlayer, 8, 8) ])
                    `shouldBe` [DiscoveryHit loc1Id loc1Coord (8, 8)
                                             "Small Ruin" 2]

    describe "findDiscoveries: ownership is not alliance (#2518, FTS-4)" $ do
        let sight uid prof = UnitSight uid prof [(8, 8)]
            playerAt88     = sight 2 (legacySight FactionPlayer)

        it "an uncontrolled acolyte sharing the roster's tag produces \
           \neither discovery nor awareness" $ do
            findDiscoveries localController 0 instances1
                [sight 1 uncontrolledAcolyte] `shouldBe` []
            findAwareness localController 0 instances1
                [sight 1 uncontrolledAcolyte] `shouldBe` []

        it "units of another human or an AI controller discover nothing \
           \for the local observer" $
            forM_ [otherPlayerAcolyte, aiAcolyte] $ \prof → do
                findDiscoveries localController 0 instances1 [sight 1 prof]
                    `shouldBe` []
                findAwareness localController 0 instances1 [sight 1 prof]
                    `shouldBe` []

        it "the same scene with an owned unit alongside does fire, \
           \attributed to the owned unit" $ do
            let scene = [sight 1 uncontrolledAcolyte, sight 3 aiAcolyte, playerAt88]
            map dhUnit (findDiscoveries localController 0 instances1 scene)
                `shouldBe` [2]
            map ahUnit (findAwareness localController 0 instances1 scene)
                `shouldBe` [2]

        it "ownership is observer-relative: the other controller's own \
           \observer discovers through its unit and not through ours" $ do
            let p2    = humanController "player_2"
                scene = [sight 1 otherPlayerAcolyte, playerAt88]
            map dhUnit (findDiscoveries p2 0 instances1 scene) `shouldBe` [1]
            map ahUnit (findAwareness p2 0 instances1 scene) `shouldBe` [1]

    describe "findDiscoveries: page scoping" $
        it "the same coordinates on a different page have independent \
           \discovered state" $ do
            -- Page A: fresh (nothing discovered yet) — the unit that
            -- sees it discovers it.
            findDiscoveries localController 0 instances1 (standingAt 8 8) `shouldBe` [hit1]
            -- Page B: same definition and coordinate, but ITS OWN
            -- instance table already has loc1 discovered — a caller
            -- passes each page's own persisted state, so this never
            -- re-fires even though the inputs are otherwise identical.
            findDiscoveries localController 0 (instancesAt LifecycleDiscovered) (standingAt 8 8)
                `shouldBe` []

    describe "findDiscoveries: cylindrical U-seam (mirrors #777's contract)" $ do
        it "a seam-adjacent seen tile is NOT a contact under raw \
           \(non-wrapping) coords" $
            findDiscoveries localController 0 seamInstances (standingAt 8 24)
                `shouldBe` []
        it "the same seen tile IS a contact once the seam wrap is considered" $
            findDiscoveries localController 2 seamInstances (standingAt 8 24)
                `shouldBe` [DiscoveryHit loc1Id (ChunkCoord 1 0) (24, 8)
                                          "Small Ruin" 1]
        it "a raw-coordinate alias that is not inside on both \
           \axes is never discovered, even under wrapping" $
            -- x=24 lands inside the (unshifted) x-range (22..26) by
            -- coincidence, but y=100 is nowhere near any seam alias's
            -- y-range — containment requires both axes, not a single
            -- coincidental one.
            findDiscoveries localController 2 seamInstances (standingAt 24 100)
                `shouldBe` []

    describe "findDiscoveries: idempotency" $
        it "a location already in the discovered set never re-fires, \
           \even with the same qualifying unit still looking at it" $ do
            let firstTick = findDiscoveries localController 0 instances1 (standingAt 8 8)
            firstTick `shouldBe` [hit1]
            let afterFirst = foldr (\h acc →
                    fromMaybe acc (setLocationLifecycle (dhInstance h)
                                       LifecycleDiscovered acc))
                    instances1 firstTick
            findDiscoveries localController 0 afterFirst (standingAt 8 8) `shouldBe` []

    describe "lifecycle no-regression (#911)" $ do
        forM_ [ LifecycleDiscovered, LifecycleActive
              , LifecycleCleared, LifecycleDepleted ] $ \l →
            it ("an instance already " <> show l
                <> " is never re-reported as a discovery") $
                findDiscoveries localController 0 (instancesAt l) (standingAt 8 8) `shouldBe` []

        it "a hinted instance still promotes to discovered on sight" $
            findDiscoveries localController 0 (instancesAt LifecycleHinted) (standingAt 8 8)
                `shouldBe` [hit1]

        it "discovery never downgrades a later lifecycle state" $ do
            let cleared = instancesAt LifecycleCleared
            setLocationLifecycle loc1Id LifecycleDiscovered cleared
                `shouldBe` Nothing

    describe "findAwareness: per-unit location knowledge (#915)" $ do
        let aware1 uid = AwarenessHit loc1Id loc1Coord (8, 8) "Small Ruin" uid

        it "reports the qualifying player unit that can see it" $
            findAwareness localController 0 instances1 (standingAt 8 8) `shouldBe` [aware1 1]

        it "reports a unit that sees into the bounds from outside them" $
            findAwareness localController 0 instances1 (seeing [(20, 20), (8, 8)])
                `shouldBe` [aware1 1]

        it "reports nothing for a unit that sees no tile of it" $
            findAwareness localController 0 instances1 (standingAt 11 8) `shouldBe` []

        it "reports EVERY qualifying unit, not just the discoverer — two \
           \acolytes who can both see it both learn it" $
            -- findDiscoveries attributes the transition to the first
            -- qualifying unit alone; awareness must not inherit that.
            findAwareness localController 0 instances1
                (unitsAt [ (1, FactionPlayer, 8, 8), (2, FactionPlayer, 10, 6) ])
                `shouldBe` [aware1 1, aware1 2]

        it "keeps reporting a location that is ALREADY discovered — a \
           \unit that sees it later still learns it" $ do
            -- The exact pairing that would break if awareness were
            -- gated on the one-time lifecycle promotion: the player-wide
            -- layer is finished with this location, the unit is not.
            forM_ [ LifecycleDiscovered, LifecycleActive
                  , LifecycleCleared, LifecycleDepleted ] $ \l → do
                findDiscoveries localController 0 (instancesAt l) (standingAt 8 8)
                    `shouldBe` []
                findAwareness localController 0 (instancesAt l) (standingAt 8 8)
                    `shouldBe` [aware1 1]

        describe "player-OWNERSHIP contract is shared with discovery (#912)" $ do
            forM_ [f | f ← allFactions, f ≢ FactionPlayer] $ \fid →
                it (T.unpack (factionTag fid)
                     <> " that can see it never gains awareness") $
                    findAwareness localController 0 instances1 (unitsAt [(1, fid, 8, 8)])
                        `shouldBe` []

            it "a debug unit that can see it gains nothing while a player \
               \unit alongside gains it" $
                findAwareness localController 0 instances1
                    (unitsAt [ (1, FactionDebug, 8, 8), (2, FactionPlayer, 8, 8) ])
                    `shouldBe` [aware1 2]

        it "shares the seam-aware containment discovery uses — never a \
           \second, independently-drifting geometry" $ do
            findAwareness localController 0 seamInstances (standingAt 8 24) `shouldBe` []
            findAwareness localController 2 seamInstances (standingAt 8 24)
                `shouldBe` [AwarenessHit loc1Id (ChunkCoord 1 0) (24, 8)
                                          "Small Ruin" 1]

        it "carries the instance identity and anchor a memory keys on" $
            case findAwareness localController 0 instances1 (standingAt 8 8) of
                [h] → do
                    ahInstance h `shouldBe` loc1Id
                    ahAnchor h `shouldBe` (8, 8)
                other → expectationFailure ("expected one hit, got " <> show other)

        it "every discovery is also an awareness hit for the same unit \
           \and instance — the two layers cannot disagree about geometry" $ do
            let units = unitsAt
                    [ (1, FactionDebug, 8, 8), (2, FactionPlayer, 8, 8)
                    , (3, FactionPlayer, 6, 10), (4, FactionPlayer, 40, 40) ]
                discovered = findDiscoveries localController 0 instances1 units
                aware      = findAwareness localController 0 instances1 units
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
            findDiscoveries localController (wgpWorldSize p) (wgpLocationInstances p)
                             (standingAt 8 8)
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
