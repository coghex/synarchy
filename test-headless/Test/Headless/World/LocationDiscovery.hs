{-# LANGUAGE Strict #-}
-- | IO-level coverage for "Location discovery" (#780) that
--   'Test.Headless.Location.Discovery's pure spec can't reach: the
--   real 'World.Thread.Discovery.tickLocationDiscovery' promoting a
--   placed location's instance lifecycle (#911) through
--   'wsGenParamsRef' and emitting a player event through the real
--   'Engine.PlayerEvent.Emit' surface — and, since #1230, that the tick
--   really does run the real sight calculation. The pure spec injects
--   precomputed 'Location.Discovery.UnitSight' values, so it pins the
--   contact rule but cannot tell whether radius, facing cone, terrain
--   occlusion and the page clock reach it; the sight scenarios here
--   drive the tick against real tile heights and a real time of day.
--   No world/unit thread is started — mirrors
--   'Test.Headless.Unit.LineOfSight's synthetic-page pattern (a bare
--   'initializeEngineHeadless', hand-built 'WorldManager' +
--   'UnitManager') since discovery only reads/writes plain 'IORef's
--   that any thread can touch, so calling the tick function directly
--   is both sufficient and far faster than booting real worldgen.
module Test.Headless.World.LocationDiscovery (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Exception (Exception, throw, try)
import Data.IORef (writeIORef, readIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status (beginLoad, failLoad)
import Engine.PlayerEvent (PlayerEvent(..))
import Engine.PlayerEvent.Emit (StoredEvent(..), readEventLog)
import Location.Types
    ( LocationDef(..), LocationNaming(..), LocationRegistry
    , emptyLocationRegistry, registerLocation
    )
import Location.Overlay.Types (LocationOverlay)
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationInstance(..), LocationInstanceId(..), LocationInstances(..)
    , LocationLifecycle(..), LocationSignificantItem(..)
    , buildLocationInstances, instancesToList
    , setLocationEncounterEpisodeState )
import Location.Bounds (RelBounds(..))
import Test.Headless.Location.Fixture (expectGeometry)
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Engine.Scripting.Lua.API.Items.Ground (pickupGroundOnPage)
import Engine.Scripting.Lua.API.Items.Ground
    (worldSpawnLocationSignificantItemFn)
import Item.Ground (GroundItems(..), spawnGroundItem)
import Item.Types (ItemInstance(..), ItemDef(..), ItemManager(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
import World.Chunk.Types
    (ChunkCoord(..), LoadedChunk(..), chunkSize, wrapChunkCoordU)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import Structure.Types (emptyChunkStructures)
import World.Time.Types (WorldTime(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldState(..), WorldManager(..), emptyWorldState, emptyWorldManager )
import World.Thread.Discovery (tickLocationDiscovery)
import World.Thread.Time (tickWorldTime)
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


-- * Fixtures — same ruin shape as Test.Headless.Location.Discovery:
--   anchor (8,8), bounds (6,6)..(10,10). That box IS the reveal
--   footprint (#1230): sight is tested against the instance's stored
--   bounds and nothing expands them, the @discovery_margin@ halo this
--   fixture used to describe having been removed with the proximity
--   trigger it served.

registry1 ∷ LocationRegistry
registry1 = registerLocation LocationDef
    { ldId = "loc1", ldLabel = "Small Ruin", ldType = "ruin"
    , ldBuilder = "room_small", ldAnchor = [], ldMaxCount = 0
    , ldMinSpacing = 0, ldContents = []
    , ldBounds = RelBounds (-2) (-2) 2 2
    , ldMapIcon = Nothing, ldNaming = testNaming
    } emptyLocationRegistry

overlay1 ∷ LocationOverlay
overlay1 = HM.singleton (ChunkCoord 0 0) "loc1"

-- | Minimal unit fixture. Since #1230 the tick runs the REAL sight
--   calculation, so 'uiFacing', 'uiGridZ' and the @perception@ stat
--   matter as much as page/faction/position — 'facingUnit' below sets
--   them; this keeps the pre-existing cases' south-facing default.
testUnit ∷ WorldPageId → Faction → Float → Float → UnitInstance
testUnit page faction gx gy = UnitInstance
    { uiDefName = "test", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = gx, uiGridY = gy, uiGridZ = 5
    , uiRealZ = 5, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = faction, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = Nothing
    }

-- | A player unit at (gx, gy) facing @dir@ with an explicit perception.
--   Perception 1.0 gives 'Unit.LineOfSight.awareRangeTiles' × 1.0 = a
--   6-tile daytime radius, which is what makes the distances in the
--   sight scenarios below concrete.
facingUnit ∷ WorldPageId → Float → Float → Direction → Float → UnitInstance
facingUnit page gx gy dir perception =
    (testUnit page FactionPlayer gx gy)
        { uiFacing = dir
        , uiStats  = HM.singleton "perception" perception
        }

-- ---- Terrain fixtures (mirrors Test.Headless.Unit.LineOfSight) ----

-- | A flat chunk at z, as one loaded chunk at (0,0) — the ruin fixture
--   sits at anchor (8,8), inside chunk (0,0).
flatChunk ∷ Int → LoadedChunk
flatChunk z =
    let area = chunkSize * chunkSize
        v = VU.replicate area z
    in LoadedChunk
        { lcCoord = ChunkCoord 0 0, lcTiles = V.empty
        , lcSurfaceMap = v, lcTerrainSurfaceMap = v
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

-- | Flat at @flatZ@ except a wall of height @wallZ@ at local x = @wallX@
--   (every y) — high enough to block a sightline crossing that column.
wallChunk ∷ Int → Int → Int → LoadedChunk
wallChunk flatZ wallZ wallX =
    let area = chunkSize * chunkSize
        v = VU.generate area $ \i →
                if i `mod` chunkSize ≡ wallX then wallZ else flatZ
    in (flatChunk flatZ) { lcSurfaceMap = v, lcTerrainSurfaceMap = v }

wtdWith ∷ LoadedChunk → WorldTileData
wtdWith = wtdAt (ChunkCoord 0 0)

-- | One loaded chunk stored under an explicit key — which for a
--   seam-crossing fixture is the CANONICAL ('wrapChunkCoordU') key,
--   exactly as the chunk pipeline stores it, not the raw key a tile's
--   'globalToChunk' produces.
wtdAt ∷ ChunkCoord → LoadedChunk → WorldTileData
wtdAt coord lc = WorldTileData
    { wtdChunks = HM.singleton coord (lc { lcCoord = coord })
    , wtdMaxChunks = 1 }

-- | A fresh page carrying loc1's overlay, registered as the sole
--   (visible) page in a hand-built WorldManager.
newPage ∷ EngineEnv → WorldPageId → IO WorldState
newPage env pageId = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) $ Just pageParams
    writeIORef (worldManagerRef env) $ emptyWorldManager
        { wmWorlds = [(pageId, ws)]
        , wmVisible = [pageId] }
    pure ws

-- | A fresh page carrying loc1's overlay AND real terrain + a real
--   clock, registered as the sole visible page. The plain 'newPage'
--   above leaves both unset, which the pre-#1230 cases could afford
--   because they stood a unit ON the anchor; the sight scenarios below
--   need the tick to read genuine tile heights and a genuine time of
--   day.
newSightPage
    ∷ EngineEnv → WorldPageId → LoadedChunk → WorldTime → IO WorldState
newSightPage env pageId chunk time = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) $ Just pageParams
    writeIORef (wsTilesRef ws) (wtdWith chunk)
    writeIORef (wsTimeRef ws) time
    writeIORef (worldManagerRef env) $ emptyWorldManager
        { wmWorlds = [(pageId, ws)]
        , wmVisible = [pageId] }
    pure ws

-- | The gen params every page below starts from: loc1's overlay plus
--   the instance table world init would build from it (#911).
pageParams ∷ WorldGenParams
pageParams = defaultWorldGenParams
    { wgpLocationOverlay   = overlay1
    , wgpLocationInstances =
        expectGeometry (buildLocationInstances Nothing registry1 overlay1)
    }

-- | The seam fixture (#1230): a 2-chunk-wide world with loc1 placed at
--   chunk (1,0) — anchor tile (24,8), bounds (22,6)..(26,10). Mirrors
--   'Test.Headless.Location.Discovery''s pure seam fixture, which is
--   what makes the pair comparable.
--
--   The point is the coordinate FRAME: at world size 2 the chunk holding
--   those tiles is STORED under its canonical key (0,1), while every
--   tile in it produces the raw key (1,0) through 'globalToChunk'. A
--   terrain lookup that skips 'wrapChunkCoordU' therefore misses a chunk
--   that is loaded, reads \"not loaded → assume flat\", and concludes
--   nothing blocks.
seamOverlay ∷ LocationOverlay
seamOverlay = HM.singleton (ChunkCoord 1 0) "loc1"

seamPageParams ∷ WorldGenParams
seamPageParams = defaultWorldGenParams
    { wgpWorldSize         = 2
    , wgpLocationOverlay   = seamOverlay
    , wgpLocationInstances =
        expectGeometry (buildLocationInstances Nothing registry1 seamOverlay)
    }

-- | The canonical key the seam fixture's terrain must be stored under.
seamChunkKey ∷ ChunkCoord
seamChunkKey = wrapChunkCoordU 2 (ChunkCoord 1 0)

-- | Each placed location's lifecycle on a page, in instance-id order —
--   what the per-chunk discovered set used to answer.
lifecyclesOf ∷ Maybe WorldGenParams → Maybe [LocationLifecycle]
lifecyclesOf = fmap (map liLifecycle . instancesToList . wgpLocationInstances)

initEnv ∷ IO EngineEnv
initEnv = do
    EngineInitResult env ← initializeEngineHeadless
    writeIORef (locationDefsRef env) registry1
    pure env

eventsFor ∷ EngineEnv → Word32 → IO [PlayerEvent]
eventsFor env uid =
    filter ((≡ Just uid) . peUid) ∘ map seEvent ⊚ readEventLog env

eventsOnPage ∷ EngineEnv → Text → IO [PlayerEvent]
eventsOnPage env page =
    filter ((≡ Just page) . peSourcePage) ∘ map seEvent ⊚ readEventLog env

-- | The base page's sole instance carrying an encounter and sitting at
--   @lifecycle@. 'LifecycleUnknown' is where 'buildLocationInstances'
--   leaves it, and is what every pre-existing case here uses. The
--   cost-guard cases below start from 'LifecycleDiscovered' instead:
--   that is the state #1990 is about — a page with nothing left to
--   promote but an encounter still awaiting clearance.
encounterParamsAt
    ∷ LocationLifecycle → [LocationEncounterOccupant] → WorldGenParams
encounterParamsAt lifecycle occupants = pageParams
    { wgpLocationInstances = base
        { lisById = HM.singleton (liId inst) inst
        }
    }
  where
    base = wgpLocationInstances pageParams
    original = case instancesToList base of
        (one:_) → one
        [] → error "location encounter fixture has no base instance"
    encounter = LocationEncounter
        { leRolledCount = length occupants
        , leOccupants = occupants
        , leRosterComplete = True
        , leDeathOnlyClearance = True
        , leActivated = False
        , leEpisodeActive = False
        , leAggressionAnnounced = False
        , leDisengageAnnounced = False
        , leCleared = null occupants
        }
    inst = original { liEncounter = Just encounter
                    , liLifecycle = lifecycle
                    -- #917 moved the one-shot clearance notice onto the
                    -- instance. Seeded here exactly as the real
                    -- constructor seeds it: SPENT for a location born
                    -- already clearance-satisfied (a zero roll with no
                    -- significant item — nobody cleared it), unspent
                    -- while any condition is outstanding.
                    , liClearEventEmitted = null occupants }

occupantAt ∷ UnitId → (Float, Float) → LocationEncounterOccupant
occupantAt uid home = LocationEncounterOccupant uid home False False

newEncounterPage
    ∷ EngineEnv → WorldPageId → [LocationEncounterOccupant] → IO WorldState
newEncounterPage env pageId = newEncounterPageAt env pageId LifecycleUnknown

newEncounterPageAt
    ∷ EngineEnv → WorldPageId → LocationLifecycle
    → [LocationEncounterOccupant] → IO WorldState
newEncounterPageAt env pageId lifecycle occupants = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) $
        Just (encounterParamsAt lifecycle occupants)
    writeIORef (worldManagerRef env) $ emptyWorldManager
        { wmWorlds = [(pageId, ws)]
        , wmVisible = [pageId] }
    pure ws

-- * #1990 cost-guard tripwire

-- | The tripwire the clearance-cost guard is asserted with. Forcing a
--   value of this type raises, and 'Unit.LineOfSight.visibleTilesOnPage'
--   opens by binding @wsTilesRef@ and @wsTimeRef@ under that module's
--   @{-\# LANGUAGE Strict \#-}@, so those binds force to WHNF.
--   'tickLocationDiscovery' reads neither ref itself, so a raise means
--   the tick rasterized a unit's line of sight and nothing else — which
--   is what lets these cases assert the guard without measuring elapsed
--   time (the one mechanism a timing assertion could never make
--   deterministic).
data SightRasterized = SightRasterized deriving (Show)

instance Exception SightRasterized

-- | Arm the tripwire on a page. Written without an intermediate @let@
--   on purpose: this module is @Strict@, so naming the 'throw' would
--   raise here instead of inside the tick.
poisonSight ∷ WorldState → IO ()
poisonSight ws = do
    writeIORef (wsTilesRef ws) (throw SightRasterized)
    writeIORef (wsTimeRef ws) (throw SightRasterized)

-- | Tick an armed page and report whether sight was rasterized.
sightRasterized ∷ EngineEnv → WorldPageId → WorldState → IO Bool
sightRasterized env pageId ws = do
    outcome ← (try (tickLocationDiscovery env pageId ws)
                  ∷ IO (Either SightRasterized ()))
    pure $ case outcome of
        Left SightRasterized → True
        Right ()             → False

tickWithoutSight ∷ EngineEnv → WorldPageId → WorldState → Expectation
tickWithoutSight env pageId ws =
    sightRasterized env pageId ws `shouldReturn` False

tickWithSight ∷ EngineEnv → WorldPageId → WorldState → Expectation
tickWithSight env pageId ws =
    sightRasterized env pageId ws `shouldReturn` True

spec ∷ Spec
spec = beforeAll initEnv $ do
    significantSpec
    describe "Location discovery (#780) — tickLocationDiscovery" $ do

        it "a player-faction unit standing ON the location marks it \
           \discovered and emits exactly one attributable event; \
           \re-ticking with the same unit still there emits no duplicate" $ \env → do
            let pageId = WorldPageId "disc_player"
            ws ← newPage env pageId
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 101)
                    (testUnit pageId FactionPlayer 8 8) }

            tickLocationDiscovery env pageId ws
            mp1 ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp1 `shouldBe` Just [LifecycleDiscovered]

            evs1 ← eventsFor env 101
            map peCategory evs1 `shouldBe` ["location_discovery"]
            map peText evs1 `shouldBe` ["Discovered: Small Ruin"]
            map peCoords evs1 `shouldBe` [Just (8, 8)]
            map peSourcePage evs1 `shouldBe` [Just "disc_player"]

            -- Re-tick: the unit hasn't moved, the location is already
            -- discovered — no second event, no change to the set.
            tickLocationDiscovery env pageId ws
            mp2 ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp2 `shouldBe` Just [LifecycleDiscovered]
            evs2 ← eventsFor env 101
            length evs2 `shouldBe` 1

        it "a zero-occupant ruin stays unknown until sight, then appears \
           \cleared without emitting a fake clearance event" $ \env → do
            let pageId = WorldPageId "disc_zero_encounter"
            ws ← newEncounterPage env pageId []
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 701)
                    (testUnit pageId FactionPlayer 8 8) }

            before ← readIORef (wsGenParamsRef ws)
            lifecyclesOf before `shouldBe` Just [LifecycleUnknown]
            tickLocationDiscovery env pageId ws
            after ← readIORef (wsGenParamsRef ws)
            lifecyclesOf after `shouldBe` Just [LifecycleCleared]
            evs ← eventsOnPage env "disc_zero_encounter"
            map peCategory evs `shouldBe` ["location_discovery"]

        it "an occupied ruin stays discovered until aggression, then clears \
           \once only after every assigned UID is on-page and exactly dead" $
           \env → do
            let pageId = WorldPageId "disc_death_only_encounter"
                uidA = UnitId 711
                uidB = UnitId 712
            ws ← newEncounterPage env pageId
                [occupantAt uidA (7, 8), occupantAt uidB (9, 8)]
            let player = testUnit pageId FactionPlayer 8 8
                nomadA pose = (testUnit pageId FactionHostile 7 8)
                    { uiPose = pose }
                nomadB page pose = (testUnit page FactionHostile 9 8)
                    { uiPose = pose }
                publish a b = writeIORef (unitManagerRef env) $
                    emptyUnitManager { umInstances = HM.fromList
                        [(UnitId 710, player), (uidA, a), (uidB, b)] }

            publish (nomadA "standing") (nomadB pageId "collapsed")
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])

            -- First aggression is the activation edge. The encounter
            -- command uses this pure mutation on the world thread.
            readIORef (wsGenParamsRef ws) >>= mapM_ (\p →
                writeIORef (wsGenParamsRef ws) $ Just p
                    { wgpLocationInstances = setLocationEncounterEpisodeState
                        (LocationInstanceId 1) True True False
                        (wgpLocationInstances p) })
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleActive])

            -- Collapsed/crawling is not death, and a wrong-page occupant
            -- is equally non-clearable at runtime.
            publish (nomadA "dead") (nomadB pageId "collapsed")
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleActive])
            publish (nomadA "dead")
                (nomadB (WorldPageId "somewhere_else") "dead")
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleActive])

            publish (nomadA "dead") (nomadB pageId "dead")
            tickLocationDiscovery env pageId ws
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])
            evs ← eventsOnPage env "disc_death_only_encounter"
            map peCategory evs `shouldBe`
                ["location_discovery", "location_clearance"]

        it "keeps a pre-discovery defeat private, then emits discovery and \
           \the deferred occupied-clear edge exactly once on first sight" $
           \env → do
            let pageId = WorldPageId "disc_hidden_clear_encounter"
                uidA = UnitId 716
            ws ← newEncounterPage env pageId [occupantAt uidA (7, 8)]
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton uidA
                    ((testUnit pageId FactionHostile 7 8) { uiPose = "dead" }) }

            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleUnknown])
            eventsOnPage env "disc_hidden_clear_encounter" `shouldReturn` []

            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.fromList
                    [ (uidA, (testUnit pageId FactionHostile 7 8)
                        { uiPose = "dead" })
                    , (UnitId 715, testUnit pageId FactionPlayer 8 8)
                    ] }
            tickLocationDiscovery env pageId ws
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])
            evs ← eventsOnPage env "disc_hidden_clear_encounter"
            map peCategory evs `shouldBe`
                ["location_discovery", "location_clearance"]

        it "a missing assigned UID stays in the death-only roster and \
           \keeps the encounter uncleared" $ \env → do
            let pageId = WorldPageId "disc_missing_encounter"
                uidA = UnitId 721
                uidMissing = UnitId 722
            ws ← newEncounterPage env pageId
                [occupantAt uidA (7, 8), occupantAt uidMissing (9, 8)]
            -- Aggression before discovery is remembered without leaking the
            -- site. First sight still emits discovery and exposes active.
            readIORef (wsGenParamsRef ws) >>= mapM_ (\p →
                writeIORef (wsGenParamsRef ws) $ Just p
                    { wgpLocationInstances = setLocationEncounterEpisodeState
                        (LocationInstanceId 1) True True False
                        (wgpLocationInstances p) })
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleUnknown])
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.fromList
                    [ (UnitId 720, testUnit pageId FactionPlayer 8 8)
                    , (uidA, (testUnit pageId FactionHostile 7 8)
                        { uiPose = "dead" })
                    ] }
            tickLocationDiscovery env pageId ws
            params ← readIORef (wsGenParamsRef ws)
            lifecyclesOf params `shouldBe` Just [LifecycleActive]
            evs ← eventsOnPage env "disc_missing_encounter"
            map peCategory evs `shouldBe` ["location_discovery"]

        it "a non-player unit standing ON the location discovers \
           \nothing and emits no event" $ \env → do
            let pageId = WorldPageId "disc_hostile"
            ws ← newPage env pageId
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 202)
                    (testUnit pageId FactionHostile 8 8) }

            tickLocationDiscovery env pageId ws
            mp ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp `shouldBe` Just [LifecycleUnknown]
            evs ← eventsFor env 202
            evs `shouldBe` []

        it "a DEBUG-faction unit standing ON the location discovers \
           \nothing and emits no event, even though it is allied with \
           \the player and takes player orders (#912)" $ \env → do
            -- The regression an ownership→alliance collapse would break.
            -- Debug is player-COMMANDABLE and player-ALLIED; discovery
            -- asks player-OWNED, which it is not. Distinct from the
            -- hostile case above precisely because those other two
            -- answers are True here.
            let pageId = WorldPageId "disc_debug"
            ws ← newPage env pageId
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 203)
                    (testUnit pageId FactionDebug 8 8) }

            tickLocationDiscovery env pageId ws
            mp ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp `shouldBe` Just [LifecycleUnknown]
            evs ← eventsFor env 203
            evs `shouldBe` []

        it "a location discovered on a HIDDEN page is attributed to that \
           \page and carries no pannable coords, even when another (active) \
           \page places a location at the very same chunk coordinate" $ \env → do
            let pageActive = WorldPageId "disc_active"
                pageHidden = WorldPageId "disc_hidden"
            wsActive ← emptyWorldState
            writeIORef (wsGenParamsRef wsActive) $ Just pageParams
            wsHidden ← emptyWorldState
            writeIORef (wsGenParamsRef wsHidden) $ Just pageParams
            -- Only pageActive is visible/active; pageHidden is loaded
            -- (simulated) but not shown — mirrors a second live world
            -- page kept around while the player looks at the first.
            writeIORef (worldManagerRef env) $
                emptyWorldManager
                    { wmWorlds = [ (pageActive, wsActive)
                                 , (pageHidden, wsHidden) ]
                    , wmVisible = [pageActive] }
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.fromList
                    [ (UnitId 301, testUnit pageActive FactionPlayer 8 8)
                    , (UnitId 302, testUnit pageHidden FactionPlayer 8 8)
                    ]
                }

            tickLocationDiscovery env pageActive wsActive
            tickLocationDiscovery env pageHidden wsHidden

            mpActive ← readIORef (wsGenParamsRef wsActive)
            mpHidden ← readIORef (wsGenParamsRef wsHidden)
            lifecyclesOf mpActive `shouldBe` Just [LifecycleDiscovered]
            lifecyclesOf mpHidden `shouldBe` Just [LifecycleDiscovered]

            evsActive ← eventsFor env 301
            evsHidden ← eventsFor env 302
            -- Same chunk coordinate on both pages, but each event is
            -- attributed to ITS OWN page and only the active page's
            -- event carries pannable coords.
            map peSourcePage evsActive `shouldBe` [Just "disc_active"]
            map peCoords evsActive `shouldBe` [Just (8, 8)]
            map peSourcePage evsHidden `shouldBe` [Just "disc_hidden"]
            map peCoords evsHidden `shouldBe` [Nothing]

        -- #1230: the tick now runs the REAL sight calculation
        -- ('Unit.LineOfSight.visibleTilesOnPage') rather than a
        -- point-in-halo test. The pure specs in
        -- 'Test.Headless.Location.Discovery' inject precomputed
        -- 'UnitSight' values, so they prove the CONTACT rule but say
        -- nothing about whether the tick actually consults radius,
        -- cone, terrain and clock. These cases drive the tick itself.
        --
        -- One geometry throughout, so exactly one variable moves per
        -- case: the ruin's bounds are (6,6)..(10,10); the unit stands
        -- at (14,8) OUTSIDE them, four tiles east of the nearest
        -- occupied tile (10,8). Terrain is flat z=5 and the unit's
        -- footing is z=5, so its eye sits at z=6. With perception 1.0
        -- the daytime radius is 5 tiles (6.0 × a longitude-local noon
        -- factor just under 1) and the midnight radius is 3, which is
        -- what makes four tiles the discriminating distance.
        it "a player unit standing OUTSIDE the bounds that can SEE into \
           \them discovers the location — reveal follows sight, never \
           \the unit's own position" $ \env → do
            let pageId = WorldPageId "sight_visible"
            ws ← newSightPage env pageId (flatChunk 5) (WorldTime 12 0)
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 501)
                    (facingUnit pageId 14 8 DirW 1.0) }

            tickLocationDiscovery env pageId ws
            mp ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp `shouldBe` Just [LifecycleDiscovered]
            evs ← eventsFor env 501
            map peCategory evs `shouldBe` ["location_discovery"]

        it "the same unit BEYOND its perception radius discovers nothing" $ \env → do
            -- (20,8) is ten tiles from the nearest occupied tile, twice
            -- the daytime radius. Everything else is identical to the
            -- passing case above.
            let pageId = WorldPageId "sight_far"
            ws ← newSightPage env pageId (flatChunk 5) (WorldTime 12 0)
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 502)
                    (facingUnit pageId 20 8 DirW 1.0) }

            tickLocationDiscovery env pageId ws
            mp ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp `shouldBe` Just [LifecycleUnknown]
            eventsFor env 502 >>= (`shouldBe` [])

        it "the same unit at the same distance FACING AWAY discovers \
           \nothing — the 120° cone applies" $ \env → do
            -- Identical position to the passing case; only uiFacing
            -- differs. Every occupied tile lies west of the unit, so
            -- facing east puts all of them behind it.
            let pageId = WorldPageId "sight_cone"
            ws ← newSightPage env pageId (flatChunk 5) (WorldTime 12 0)
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 503)
                    (facingUnit pageId 14 8 DirE 1.0) }

            tickLocationDiscovery env pageId ws
            mp ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp `shouldBe` Just [LifecycleUnknown]
            eventsFor env 503 >>= (`shouldBe` [])

        it "the same unit BEHIND BLOCKING TERRAIN discovers nothing — \
           \a ruin behind a hill stays unknown" $ \env → do
            -- Identical position and facing to the passing case; only
            -- the terrain differs. A wall of height 40 at x=12 sits
            -- between the unit (x=14) and every occupied tile (x ≤ 10),
            -- so every sightline crosses it.
            let pageId = WorldPageId "sight_blocked"
            ws ← newSightPage env pageId (wallChunk 5 40 12) (WorldTime 12 0)
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 504)
                    (facingUnit pageId 14 8 DirW 1.0) }

            tickLocationDiscovery env pageId ws
            mp ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp `shouldBe` Just [LifecycleUnknown]
            eventsFor env 504 >>= (`shouldBe` [])

        it "the same unit AT NIGHT discovers nothing, and the identical \
           \daytime scene discovers it — the tick reads its own page's \
           \clock through the night-scaled radius" $ \env → do
            -- The pair that proves the night factor reaches the binary
            -- set through the tick: same position, same facing, same
            -- flat terrain, same perception; only the page CLOCK moves.
            -- Four tiles is inside the daytime radius (5) and outside
            -- the midnight one (3).
            let nightPage = WorldPageId "sight_night"
                dayPage   = WorldPageId "sight_day"
            wsNight ← newSightPage env nightPage (flatChunk 5) (WorldTime 0 0)
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 505)
                    (facingUnit nightPage 14 8 DirW 1.0) }
            tickLocationDiscovery env nightPage wsNight
            mpNight ← readIORef (wsGenParamsRef wsNight)
            lifecyclesOf mpNight `shouldBe` Just [LifecycleUnknown]
            eventsFor env 505 >>= (`shouldBe` [])

            wsDay ← newSightPage env dayPage (flatChunk 5) (WorldTime 12 0)
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 506)
                    (facingUnit dayPage 14 8 DirW 1.0) }
            tickLocationDiscovery env dayPage wsDay
            mpDay ← readIORef (wsGenParamsRef wsDay)
            lifecyclesOf mpDay `shouldBe` Just [LifecycleDiscovered]
            evsDay ← eventsFor env 506
            map peCategory evsDay `shouldBe` ["location_discovery"]

        -- #1230 round 2 (Codex): sight now drives SEAM-AWARE reveal —
        -- 'Location.Bounds.boundsContainsPoint' tries the bounds' u-wrap
        -- aliases — while terrain lookup resolves a chunk key. Chunks
        -- are STORED u-wrapped, so a raw key from an alias frame misses
        -- a chunk that IS loaded, and the miss reads as "assume flat",
        -- i.e. "nothing blocks". A ruin behind a hill on the seam would
        -- then be revealed through solid ground.
        --
        -- Geometry: world size 2, loc1 at chunk (1,0) → bounds
        -- (22,6)..(26,10), terrain stored under the CANONICAL key (0,1)
        -- that raw key (1,0) wraps to. The unit stands at (28,8) facing
        -- west, two tiles from the nearest occupied tile, with a wall
        -- at x=27 between them — every sightline from x=28 to x≤26
        -- crosses it.
        it "a seam-frame unit does NOT see through a hill stored under \
           \the canonical chunk key — terrain occlusion survives the \
           \u-wrap that reveal's bounds test already honours" $ \env → do
            let pageId = WorldPageId "sight_seam_blocked"
            ws ← emptyWorldState
            writeIORef (wsGenParamsRef ws) $ Just seamPageParams
            writeIORef (wsTilesRef ws)
                (wtdAt seamChunkKey (wallChunk 5 40 11))   -- x=27 → local 11
            writeIORef (wsTimeRef ws) (WorldTime 12 0)
            writeIORef (worldManagerRef env) $
                emptyWorldManager
                    { wmWorlds = [(pageId, ws)]
                    , wmVisible = [pageId] }
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 601)
                    (facingUnit pageId 28 8 DirW 1.0) }

            tickLocationDiscovery env pageId ws
            mp ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp `shouldBe` Just [LifecycleUnknown]
            eventsFor env 601 >>= (`shouldBe` [])

        it "…and the identical seam scene with the hill removed DOES \
           \discover it, so the case above is occlusion working rather \
           \than the geometry being out of reach" $ \env → do
            -- Same page size, same location, same unit position, facing
            -- and clock, same canonical chunk key — only the wall is
            -- gone. Without this pair, a fixture that simply could not
            -- see the ruin would pass the negative above forever.
            let pageId = WorldPageId "sight_seam_open"
            ws ← emptyWorldState
            writeIORef (wsGenParamsRef ws) $ Just seamPageParams
            writeIORef (wsTilesRef ws) (wtdAt seamChunkKey (flatChunk 5))
            writeIORef (wsTimeRef ws) (WorldTime 12 0)
            writeIORef (worldManagerRef env) $
                emptyWorldManager
                    { wmWorlds = [(pageId, ws)]
                    , wmVisible = [pageId] }
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 602)
                    (facingUnit pageId 28 8 DirW 1.0) }

            tickLocationDiscovery env pageId ws
            mp ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mp `shouldBe` Just [LifecycleDiscovered]
            evs ← eventsFor env 602
            map peCategory evs `shouldBe` ["location_discovery"]

        -- Round 12 review (issue #763): World.Load.Stage.stageSession
        -- runs on the world thread BEFORE the save barrier's capture
        -- lock is ever entered, so an ordinary tickWorldTime landing
        -- during that unlocked staging window used to mutate the LIVE,
        -- still-current (pre-load) session's discovery state — a real,
        -- persistent change the #763 contract says a failed/aborted
        -- load must never leave behind. tickWorldTime now gates its own
        -- call to tickLocationDiscovery on Engine.Load.Status.loadInProgress,
        -- independent of the pause flag (which the existing tests above
        -- deliberately never touch, since discovery firing "even while
        -- paused" for a freshly loaded session is #780's own documented
        -- contract, not something this fix should disturb).
        it "a tick landing while a load transaction is in flight does \
           \NOT discover a location a player-faction unit is already \
           \standing on, even though the same tick would normally \
           \discover it instantly; discovery resumes once the \
           \transaction ends (simulated here as a failed/aborted load, \
           \mirroring the #763 'nothing changed' contract)" $ \env → do
            let pageId = WorldPageId "disc_loading"
            ws ← newPage env pageId
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 401)
                    (testUnit pageId FactionPlayer 8 8) }

            Right reqId ← beginLoad (loadStatusRef env) "probe_load"
            tickWorldTime env 1.0
            mpDuring ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mpDuring `shouldBe` Just [LifecycleUnknown]
            evsDuring ← eventsFor env 401
            evsDuring `shouldBe` []

            -- The transaction ends (failed, here) -- loadInProgress goes
            -- false and discovery resumes on the very next tick.
            failLoad (loadStatusRef env) reqId "test abort"
            tickWorldTime env 1.0
            mpAfter ← readIORef (wsGenParamsRef ws)
            lifecyclesOf mpAfter `shouldBe` Just [LifecycleDiscovered]
            evsAfter ← eventsFor env 401
            map peCategory evsAfter `shouldBe` ["location_discovery"]

    -- #1990: an uncleared encounter admits its page to the tick body
    -- (#916/PR #1900 widened the guard so clearance keeps being polled),
    -- but on a page where every location is already discovered that is
    -- clearance work, not discovery work. These cases pin that the
    -- expensive half — per-unit line-of-sight rasterization — is not run
    -- for it, and pin it through the 'SightRasterized' tripwire above
    -- rather than through elapsed time, so a reintroduced sight call
    -- fails deterministically.
    --
    -- Each example uses its own 'WorldPageId': the suite shares one
    -- 'EngineEnv' and one accumulating player-event log, so assertions
    -- here go through 'eventsOnPage'.
    describe "Location discovery clearance cost guard" $ do

        it "DOES rasterize sight on a page that still has something to \
           \discover — the control that proves the tripwire fires" $ \env → do
            -- Without this the four cases below could all pass vacuously.
            let pageId = WorldPageId "cost_guard_control"
                uid = UnitId 741
            ws ← newEncounterPageAt env pageId LifecycleUnknown
                [occupantAt uid (7, 8)]
            poisonSight ws
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.fromList
                    [ (UnitId 740, testUnit pageId FactionPlayer 8 8)
                    , (uid, testUnit pageId FactionHostile 7 8) ] }
            tickWithSight env pageId ws

        it "an already-discovered page with an uncleared encounter \
           \performs no sight evaluation while its occupants are alive" $
           \env → do
            let pageId = WorldPageId "cost_guard_alive"
                uidA = UnitId 751
                uidB = UnitId 752
            ws ← newEncounterPageAt env pageId LifecycleDiscovered
                [occupantAt uidA (7, 8), occupantAt uidB (9, 8)]
            poisonSight ws
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.fromList
                    [ (UnitId 750, testUnit pageId FactionPlayer 8 8)
                    , (uidA, testUnit pageId FactionHostile 7 8)
                    , (uidB, testUnit pageId FactionHostile 9 8) ] }

            tickWithoutSight env pageId ws
            tickWithoutSight env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])
            eventsOnPage env "cost_guard_alive" `shouldReturn` []

        it "still detects the qualifying final death, emits clearance \
           \exactly once, and keeps skipping sight afterwards — all \
           \without one sight evaluation" $ \env → do
            let pageId = WorldPageId "cost_guard_death"
                uidA = UnitId 761
                uidB = UnitId 762
            ws ← newEncounterPageAt env pageId LifecycleDiscovered
                [occupantAt uidA (7, 8), occupantAt uidB (9, 8)]
            poisonSight ws
            let publish a b = writeIORef (unitManagerRef env) $
                    emptyUnitManager { umInstances = HM.fromList
                        [ (UnitId 760, testUnit pageId FactionPlayer 8 8)
                        , (uidA, a), (uidB, b) ] }
                nomad page pose gx = (testUnit page FactionHostile gx 8)
                    { uiPose = pose }

            -- The last qualifying death is the clearing edge, and the
            -- tick sees it with no sight input at all.
            publish (nomad pageId "dead" 7) (nomad pageId "standing" 9)
            tickWithoutSight env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])
            eventsOnPage env "cost_guard_death" `shouldReturn` []

            publish (nomad pageId "dead" 7) (nomad pageId "dead" 9)
            tickWithoutSight env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])
            evs ← eventsOnPage env "cost_guard_death"
            map peCategory evs `shouldBe` ["location_clearance"]
            map peCoords evs `shouldBe` [Just (8, 8)]

            -- Cleared: 'pendingClearance' is now False and nothing is
            -- promotable, so the whole-page early-out fires and the tick
            -- never reaches the unit manager, let alone sight.
            tickWithoutSight env pageId ws
            tickWithoutSight env pageId ws
            after ← eventsOnPage env "cost_guard_death"
            map peCategory after `shouldBe` ["location_clearance"]

        it "keeps the collapsed, crawling, wrong-page and missing \
           \occupant rules on the clearance-only path" $ \env → do
            let pageId = WorldPageId "cost_guard_roster"
                elsewhere = WorldPageId "cost_guard_roster_elsewhere"
                uidA = UnitId 771
                uidB = UnitId 772
            ws ← newEncounterPageAt env pageId LifecycleDiscovered
                [occupantAt uidA (7, 8), occupantAt uidB (9, 8)]
            poisonSight ws
            let publishBoth a b = writeIORef (unitManagerRef env) $
                    emptyUnitManager { umInstances = HM.fromList
                        [ (UnitId 770, testUnit pageId FactionPlayer 8 8)
                        , (uidA, a), (uidB, b) ] }
                publishOne a = writeIORef (unitManagerRef env) $
                    emptyUnitManager { umInstances = HM.fromList
                        [ (UnitId 770, testUnit pageId FactionPlayer 8 8)
                        , (uidA, a) ] }
                nomad page pose gx = (testUnit page FactionHostile gx 8)
                    { uiPose = pose }
                deadA = nomad pageId "dead" 7
                stillUncleared = readIORef (wsGenParamsRef ws) >>= (\p →
                    lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])

            -- Collapsed is not death.
            publishBoth deadA (nomad pageId "collapsed" 9)
            tickWithoutSight env pageId ws
            stillUncleared
            -- Neither is crawling.
            publishBoth deadA (nomad pageId "crawling" 9)
            tickWithoutSight env pageId ws
            stillUncleared
            -- Nor is a corpse that is on some other page.
            publishBoth deadA (nomad elsewhere "dead" 9)
            tickWithoutSight env pageId ws
            stillUncleared
            -- Nor is an assigned UID the manager no longer knows.
            publishOne deadA
            tickWithoutSight env pageId ws
            stillUncleared
            eventsOnPage env "cost_guard_roster" `shouldReturn` []

            -- The whole roster dead on-page is still the one thing that
            -- clears, and it clears here too.
            publishBoth deadA (nomad pageId "dead" 9)
            tickWithoutSight env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])
            evs ← eventsOnPage env "cost_guard_roster"
            map peCategory evs `shouldBe` ["location_clearance"]

        it "runs the clearance-only path on a HIDDEN page and while \
           \paused, attributing the event to that page with no pannable \
           \coords and still evaluating no sight" $ \env → do
            let pageActive = WorldPageId "cost_guard_active"
                pageHidden = WorldPageId "cost_guard_hidden"
                uid = UnitId 781
            wsActive ← emptyWorldState
            writeIORef (wsGenParamsRef wsActive) $ Just pageParams
            wsHidden ← emptyWorldState
            writeIORef (wsGenParamsRef wsHidden) $ Just
                (encounterParamsAt LifecycleDiscovered [occupantAt uid (7, 8)])
            writeIORef (worldManagerRef env) $ emptyWorldManager
                { wmWorlds = [(pageActive, wsActive), (pageHidden, wsHidden)]
                , wmVisible = [pageActive] }
            poisonSight wsHidden
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.fromList
                    [ (UnitId 780, testUnit pageHidden FactionPlayer 8 8)
                    , (uid, (testUnit pageHidden FactionHostile 7 8)
                        { uiPose = "dead" }) ] }

            -- The tick reads no pause flag; clearance must land anyway.
            writeIORef (enginePausedRef env) True
            tickWithoutSight env pageHidden wsHidden
            writeIORef (enginePausedRef env) False

            readIORef (wsGenParamsRef wsHidden) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])
            evs ← eventsOnPage env "cost_guard_hidden"
            map peCategory evs `shouldBe` ["location_clearance"]
            map peCoords evs `shouldBe` [Nothing]
            map peSourcePage evs `shouldBe` [Just "cost_guard_hidden"]

        it "still rasterizes sight for a roster defeated BEFORE \
           \discovery, so the deferred clearance event is never \
           \stranded" $ \env → do
            -- 'markLocationEncounterCleared' only arms the deferred emit
            -- for an instance defeated while still un-discovered — which
            -- is exactly a still-promotable page, so the cost guard must
            -- NOT skip sight here. Armed page first, then the real one.
            let armed = WorldPageId "cost_guard_predeath_armed"
                uidArmed = UnitId 791
            wsArmed ← newEncounterPageAt env armed LifecycleUnknown
                [occupantAt uidArmed (7, 8)]
            poisonSight wsArmed
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.fromList
                    [ (UnitId 790, testUnit armed FactionPlayer 8 8)
                    , (uidArmed, (testUnit armed FactionHostile 7 8)
                        { uiPose = "dead" }) ] }
            tickWithSight env armed wsArmed

            let pageId = WorldPageId "cost_guard_predeath"
                uid = UnitId 793
            ws ← newEncounterPageAt env pageId LifecycleUnknown
                [occupantAt uid (7, 8)]
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton uid
                    ((testUnit pageId FactionHostile 7 8)
                        { uiPose = "dead" }) }
            -- Defeated with nobody watching: private, no event.
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleUnknown])
            eventsOnPage env "cost_guard_predeath" `shouldReturn` []

            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.fromList
                    [ (uid, (testUnit pageId FactionHostile 7 8)
                        { uiPose = "dead" })
                    , (UnitId 792, testUnit pageId FactionPlayer 8 8) ] }
            tickLocationDiscovery env pageId ws
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])
            evs ← eventsOnPage env "cost_guard_predeath"
            map peCategory evs `shouldBe`
                ["location_discovery", "location_clearance"]

-- * #917 significant contents ---------------------------------------

-- | The base page's sole instance carrying @entries@ as its guaranteed
--   significant obligations, plus an optional encounter beside them, so
--   one fixture covers all four authored-condition shapes.
significantParams
    ∷ LocationLifecycle → Maybe [LocationEncounterOccupant]
    → [LocationSignificantItem] → WorldGenParams
significantParams lifecycle mOccupants entries = pageParams
    { wgpLocationInstances = base { lisById = HM.singleton (liId inst) inst } }
  where
    base = wgpLocationInstances pageParams
    original = case instancesToList base of
        (one:_) → one
        [] → error "significant fixture has no base instance"
    encounterFor occupants = LocationEncounter
        { leRolledCount = length occupants
        , leOccupants = occupants
        , leRosterComplete = True
        , leDeathOnlyClearance = True
        , leActivated = False
        , leEpisodeActive = False
        , leAggressionAnnounced = False
        , leDisengageAnnounced = False
        , leCleared = null occupants
        }
    inst = original
        { liEncounter = encounterFor <$> mOccupants
        , liLifecycle = lifecycle
        , liSignificant = entries
        -- Seeded the way the real constructor seeds it: no location
        -- owing an untaken item is ever born already satisfied.
        , liClearEventEmitted = False
        }

newSignificantPage
    ∷ EngineEnv → WorldPageId → LocationLifecycle
    → Maybe [LocationEncounterOccupant] → [LocationSignificantItem]
    → IO WorldState
newSignificantPage env pageId lifecycle mOccupants entries = do
    writeIORef (itemManagerRef env) significantItemDefs
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) $
        Just (significantParams lifecycle mOccupants entries)
    writeIORef (worldManagerRef env) $ emptyWorldManager
        { wmWorlds = [(pageId, ws)], wmVisible = [pageId] }
    pure ws

-- | One spawned, untaken obligation bound to physical item @itemId@.
owed ∷ Int → Word64 → LocationSignificantItem
owed slot itemId = LocationSignificantItem
    { lsiSlot        = slot
    , lsiItemDefName = "processing_unit"
    , lsiInstanceId  = Just itemId
    , lsiTaken       = False
    }

-- | The two item definitions #917's own verb needs registered: it
--   materializes from the obligation's persisted def name, so an
--   unregistered one is a refusal rather than a spawn. `rations` is the
--   decoy the substitution case needs.
significantItemDefs ∷ ItemManager
significantItemDefs = ItemManager $ HM.fromList
    [ ("processing_unit", fixtureDef "processing_unit")
    , ("rations", fixtureDef "rations") ]

fixtureDef ∷ Text → ItemDef
fixtureDef name = ItemDef
    { idName = name, idDisplayName = name
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.4, idWeightSpec = Nothing
    , idBulk = 0.4, idStorage = Nothing, idKind = "misc"
    , idCategory = "Materials", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing
    , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
    , idArmor = Nothing, idUnequippable = False, idBuffs = []
    , idInsulation = 0, idSourcePath = "test-fixture"
    }

groundItem ∷ Word64 → ItemInstance
groundItem iid = ItemInstance
    { iiDefName     = "processing_unit"
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = 0.4
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 0.4
    , iiStorage     = Nothing
    }

-- | Drop @iid@ onto @ws@'s ground and answer its page-local ground id.
dropOnGround ∷ WorldState → Word64 → IO Int
dropOnGround ws iid =
    atomicModifyIORef' (wsGroundItemsRef ws) (spawnGroundItem (groundItem iid) 8 8)

-- | The one ground item on a page, for a fixture that spawned exactly
--   one. Fails loudly rather than silently picking one of several.
onlyGroundId ∷ WorldState → IO Int
onlyGroundId ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    case HM.keys (gisItems gis) of
        [gid] → pure gid
        other → error ("expected exactly one ground item, got "
                          <> show (length other))

groundCount ∷ WorldState → IO Int
groundCount ws = HM.size ∘ gisItems <$> readIORef (wsGroundItemsRef ws)

takenFlags ∷ WorldState → IO [Bool]
takenFlags ws = do
    mp ← readIORef (wsGenParamsRef ws)
    pure [ lsiTaken e
         | p ← maybeToList mp
         , inst ← instancesToList (wgpLocationInstances p)
         , e ← liSignificant inst ]

-- | Call @world.spawnLocationSignificantItem(instanceId, slot, x, y,
--   pageId)@ through the real Lua binding and answer what it handed
--   back.
--
--   This is the ONLY way an obligation is ever filled (#917): the verb
--   spawns the item AND binds it in one engine call, so nothing outside
--   the engine ever chooses which item fills a slot. Driving the real
--   binding is also the only way to observe that the binding is applied
--   by the time the verb RETURNS, which is what closes the pickup race.
spawnSignificant
    ∷ EngineEnv → WorldPageId → Int → Int → (Float, Float) → IO Bool
spawnSignificant env (WorldPageId page) iid slot (x, y) = Lua.run $ do
    Lua.openlibs
    Lua.pushinteger (fromIntegral iid)
    Lua.pushinteger (fromIntegral slot)
    Lua.pushnumber (realToFrac x)
    Lua.pushnumber (realToFrac y)
    Lua.pushstring (TE.encodeUtf8 page)
    _ ← worldSpawnLocationSignificantItemFn env
    Lua.toboolean Lua.top

boundIds ∷ WorldState → IO [Maybe Word64]
boundIds ws = do
    mp ← readIORef (wsGenParamsRef ws)
    pure [ lsiInstanceId e
         | p ← maybeToList mp
         , inst ← instancesToList (wgpLocationInstances p)
         , e ← liSignificant inst ]

significantSpec ∷ SpecWith EngineEnv
significantSpec =
    describe "compound clearance with significant contents (#917)" $ do

        -- The binding is applied by the real Lua verb, ON THIS THREAD,
        -- before it returns — not queued to the world thread. Every
        -- ground pickup runs on this same thread, so a queued binding
        -- would leave a window in which the item is already pickable
        -- with its slot unbound: a pickup there latches nothing, the
        -- binding then names an item already in an inventory, no second
        -- ground pickup is possible, `contents_spawned` blocks a
        -- respawn, and the location can never clear.
        it "binds provenance SYNCHRONOUSLY, so a pickup issued the very \
           \next instant cannot slip between the spawn and the binding" $
           \env → do
            let pageId = WorldPageId "sig_bind_race"
            ws ← newSignificantPage env pageId LifecycleDiscovered Nothing
                     [ (owed 1 0) { lsiInstanceId = Nothing } ]
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 809)
                    (testUnit pageId FactionPlayer 8 8) }

            -- No world thread runs in this suite at all, so a QUEUED
            -- binding would still be unapplied here — which is exactly
            -- the state the racing pickup below would find.
            boundIds ws `shouldReturn` [Nothing]
            spawnSignificant env pageId 1 1 (8, 8) `shouldReturn` True
            bound ← boundIds ws
            bound `shouldSatisfy` all isJust

            -- …and because the item and its binding landed together,
            -- the pickup latches.
            gid ← onlyGroundId ws
            pickupGroundOnPage env ws (UnitId 809) gid `shouldReturn` True
            takenFlags ws `shouldReturn` [True]
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])

        -- #1990's guard, held against #917's own widening. Before
        -- significant contents a zero-roll ruin admitted the clearance
        -- pass for nothing; a version of #917 that polled every
        -- unsatisfied location would have reintroduced exactly that,
        -- on every tick, for the whole life of an unlooted ruin.
        it "does NOT rasterize sight for a discovered location whose \
           \guaranteed item is still on the floor — an unsatisfied \
           \obligation is not clearance WORK, so the page short-circuits" $
           \env → do
            let pageId = WorldPageId "sig_cost_guard"
            ws ← newSignificantPage env pageId LifecycleDiscovered Nothing
                     [owed 1 5081]
            _ ← dropOnGround ws 5081
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 810)
                    (testUnit pageId FactionPlayer 8 8) }
            poisonSight ws
            sightRasterized env pageId ws `shouldReturn` False

        -- The whole reason binding is not a verb of its own. With a
        -- public bind-this-ground-item API, a caller could spawn or
        -- pick out an unrelated item of the right definition, bind it,
        -- and take THAT — the location would never spawn its own
        -- guaranteed item (a bound slot is skipped) and the unrelated
        -- pickup would clear the ruin. Definition and duplicate-identity
        -- checks cannot see it: the substitute is exactly the right
        -- kind of item.
        it "gives Lua no way to choose WHICH item fills a slot — an \
           \unrelated ground item of the very same definition cannot \
           \satisfy an obligation" $ \env → do
            let pageId = WorldPageId "sig_no_substitution"
            ws ← newSignificantPage env pageId LifecycleDiscovered Nothing
                     [ (owed 1 0) { lsiInstanceId = Nothing } ]
            -- A decoy of the RIGHT definition, lying on the same page.
            decoy ← dropOnGround ws 5101
            spawnSignificant env pageId 1 1 (8, 8) `shouldReturn` True
            bound ← boundIds ws
            -- The binding names the item the ENGINE just made, never
            -- the decoy that was already there.
            bound `shouldSatisfy` (≢ [Just 5101])
            bound `shouldSatisfy` all isJust

            -- Taking the decoy therefore latches nothing and clears
            -- nothing, however identical it looks.
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 811)
                    (testUnit pageId FactionPlayer 8 8) }
            pickupGroundOnPage env ws (UnitId 811) decoy `shouldReturn` True
            takenFlags ws `shouldReturn` [False]
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])

        it "refuses an unknown slot, an unknown instance, and a slot \
           \already filled — and a refusal leaves NO item on the ground, \
           \so a retry cannot orphan one" $ \env → do
            let pageId = WorldPageId "sig_spawn_refusals"
            ws ← newSignificantPage env pageId LifecycleDiscovered Nothing
                     [ (owed 1 0) { lsiInstanceId = Nothing } ]
            spawnSignificant env pageId 1 7 (8, 8) `shouldReturn` False
            spawnSignificant env pageId 99 1 (8, 8) `shouldReturn` False
            boundIds ws `shouldReturn` [Nothing]
            groundCount ws `shouldReturn` 0

            spawnSignificant env pageId 1 1 (8, 8) `shouldReturn` True
            filled ← boundIds ws
            groundCount ws `shouldReturn` 1
            -- Write-once: a second call for the same slot is refused,
            -- the first binding stands, and no second item is spawned.
            spawnSignificant env pageId 1 1 (8, 8) `shouldReturn` False
            boundIds ws `shouldReturn` filled
            groundCount ws `shouldReturn` 1

        it "holds a location with a completed encounter uncleared while \
           \its guaranteed item is still on the floor, then clears it \
           \exactly once when the item is taken" $ \env → do
            let pageId = WorldPageId "sig_both_conditions"
            -- Zero-nomad: #916's half is satisfied from the outset, so
            -- what this pins is that the ITEM half alone still gates it.
            ws ← newSignificantPage env pageId LifecycleUnknown (Just [])
                     [owed 1 5001]
            gid ← dropOnGround ws 5001
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 801)
                    (testUnit pageId FactionPlayer 8 8) }

            -- Sight discovers it, but it is NOT cleared: the item is
            -- still there. One event, not two.
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])
            eventsOnPage env "sig_both_conditions"
                >>= (\evs → map peCategory evs `shouldBe` ["location_discovery"])

            -- A real pickup through the authoritative ground boundary
            -- latches the obligation.
            pickupGroundOnPage env ws (UnitId 801) gid `shouldReturn` True
            takenFlags ws `shouldReturn` [True]

            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])
            -- Re-ticking cannot announce it twice.
            tickLocationDiscovery env pageId ws
            tickLocationDiscovery env pageId ws
            evs ← eventsOnPage env "sig_both_conditions"
            map peCategory evs `shouldBe`
                ["location_discovery", "location_clearance"]

        it "needs EVERY obligation: taking one of two leaves the location \
           \discovered" $ \env → do
            let pageId = WorldPageId "sig_two_items"
            ws ← newSignificantPage env pageId LifecycleDiscovered Nothing
                     [owed 1 5011, owed 2 5012]
            gidA ← dropOnGround ws 5011
            _    ← dropOnGround ws 5012
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 802)
                    (testUnit pageId FactionPlayer 8 8) }

            pickupGroundOnPage env ws (UnitId 802) gidA `shouldReturn` True
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])
            takenFlags ws `shouldReturn` [True, False]
            eventsOnPage env "sig_two_items" `shouldReturn` []

        it "latches for a NON-PLAYER faction's pickup too — the location \
           \was looted whoever did it" $ \env → do
            let pageId = WorldPageId "sig_hostile_pickup"
            ws ← newSignificantPage env pageId LifecycleDiscovered Nothing
                     [owed 1 5021]
            gid ← dropOnGround ws 5021
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 803)
                    (testUnit pageId FactionHostile 8 8) }

            pickupGroundOnPage env ws (UnitId 803) gid `shouldReturn` True
            takenFlags ws `shouldReturn` [True]
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])

        it "does NOT latch a pickup that failed and rolled back" $ \env → do
            let pageId = WorldPageId "sig_failed_pickup"
            ws ← newSignificantPage env pageId LifecycleDiscovered Nothing
                     [owed 1 5031]
            gid ← dropOnGround ws 5031
            -- No such unit: the item is removed, the insert fails, and
            -- the rollback re-spawns it under a NEW ground id.
            writeIORef (unitManagerRef env) emptyUnitManager

            pickupGroundOnPage env ws (UnitId 999) gid `shouldReturn` False
            takenFlags ws `shouldReturn` [False]
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])

            -- The restored item has a new ground id but the SAME
            -- physical identity, so picking it up now does latch —
            -- which is exactly why provenance is keyed on the physical
            -- id rather than on the ground id.
            gids ← HM.keys ∘ gisItems <$> readIORef (wsGroundItemsRef ws)
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 804)
                    (testUnit pageId FactionPlayer 8 8) }
            case gids of
                [gid'] → do
                    gid' `shouldNotBe` gid
                    pickupGroundOnPage env ws (UnitId 804) gid'
                        `shouldReturn` True
                other → expectationFailure
                    ("expected one restored ground item, got " <> show other)
            takenFlags ws `shouldReturn` [True]

        it "ignores an ordinary salvage pickup — an item no obligation \
           \owns changes nothing" $ \env → do
            let pageId = WorldPageId "sig_incidental_pickup"
            ws ← newSignificantPage env pageId LifecycleDiscovered Nothing
                     [owed 1 5041]
            _        ← dropOnGround ws 5041
            lootGid  ← dropOnGround ws 5042
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 805)
                    (testUnit pageId FactionPlayer 8 8) }

            pickupGroundOnPage env ws (UnitId 805) lootGid `shouldReturn` True
            takenFlags ws `shouldReturn` [False]
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])
            eventsOnPage env "sig_incidental_pickup" `shouldReturn` []

        it "never clears a location that authors NEITHER condition, \
           \however long the tick polls it" $ \env → do
            let pageId = WorldPageId "sig_no_condition"
            ws ← newSignificantPage env pageId LifecycleUnknown Nothing []
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 806)
                    (testUnit pageId FactionPlayer 8 8) }

            tickLocationDiscovery env pageId ws
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleDiscovered])
            evs ← eventsOnPage env "sig_no_condition"
            map peCategory evs `shouldBe` ["location_discovery"]

        it "keeps a pre-discovery recovery private, then announces the \
           \deferred clearance exactly once on first sight" $ \env → do
            let pageId = WorldPageId "sig_hidden_recovery"
            ws ← newSignificantPage env pageId LifecycleUnknown Nothing
                     [owed 1 5051]
            gid ← dropOnGround ws 5051
            -- A hostile scavenger takes it while the site is unknown to
            -- the player: nothing is revealed and nothing is announced.
            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.singleton (UnitId 807)
                    (testUnit pageId FactionHostile 8 8) }
            pickupGroundOnPage env ws (UnitId 807) gid `shouldReturn` True
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleUnknown])
            eventsOnPage env "sig_hidden_recovery" `shouldReturn` []

            writeIORef (unitManagerRef env) $ emptyUnitManager
                { umInstances = HM.fromList
                    [ (UnitId 807, testUnit pageId FactionHostile 8 8)
                    , (UnitId 808, testUnit pageId FactionPlayer 8 8) ] }
            tickLocationDiscovery env pageId ws
            tickLocationDiscovery env pageId ws
            readIORef (wsGenParamsRef ws) >>= (\p →
                lifecyclesOf p `shouldBe` Just [LifecycleCleared])
            evs ← eventsOnPage env "sig_hidden_recovery"
            map peCategory evs `shouldBe`
                ["location_discovery", "location_clearance"]
