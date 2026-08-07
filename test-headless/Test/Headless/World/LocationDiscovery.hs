{-# LANGUAGE Strict #-}
-- | IO-level coverage for "Location discovery" (#780) that
--   'Test.Headless.Location.Discovery's pure spec can't reach: the
--   real 'World.Thread.Discovery.tickLocationDiscovery' promoting a
--   placed location's instance lifecycle (#911) through
--   'wsGenParamsRef' and emitting a player event through the real
--   'Engine.PlayerEvent.Emit' surface.
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
import Data.IORef (writeIORef, readIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status (beginLoad, failLoad)
import Engine.PlayerEvent (PlayerEvent(..))
import Engine.PlayerEvent.Emit (readEventLog)
import Location.Types
    ( LocationDef(..), LocationNaming(..), LocationRegistry
    , emptyLocationRegistry, registerLocation
    )
import Location.Overlay.Types (LocationOverlay)
import Location.Instance
    ( LocationInstance(..), LocationLifecycle(..), buildLocationInstances
    , instancesToList )
import Location.Bounds (RelBounds(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..), WorldManager(..), emptyWorldState)
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
--   anchor (8,8), physical bounds (6,6)..(10,10), margin-6 expanded
--   bounds (0,0)..(16,16).

registry1 ∷ LocationRegistry
registry1 = registerLocation LocationDef
    { ldId = "loc1", ldLabel = "Small Ruin", ldType = "ruin"
    , ldBuilder = "room_small", ldAnchor = [], ldMaxCount = 0
    , ldMinSpacing = 0, ldContents = []
    , ldBounds = RelBounds (-2) (-2) 2 2, ldDiscoveryMargin = 6
    , ldMapIcons = Nothing, ldNaming = testNaming
    } emptyLocationRegistry

overlay1 ∷ LocationOverlay
overlay1 = HM.singleton (ChunkCoord 0 0) "loc1"

-- | Minimal unit fixture — only the fields 'World.Thread.Discovery'
--   reads (uiPage, uiFactionId, uiGridX/Y) matter; the rest are inert
--   placeholders, mirroring 'Test.Headless.Unit.LineOfSight.testUnit'.
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

-- | A fresh page carrying loc1's overlay, registered as the sole
--   (visible) page in a hand-built WorldManager.
newPage ∷ EngineEnv → WorldPageId → IO WorldState
newPage env pageId = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) $ Just pageParams
    writeIORef (worldManagerRef env) $ WorldManager [(pageId, ws)] [pageId]
    pure ws

-- | The gen params every page below starts from: loc1's overlay plus
--   the instance table world init would build from it (#911).
pageParams ∷ WorldGenParams
pageParams = defaultWorldGenParams
    { wgpLocationOverlay   = overlay1
    , wgpLocationInstances = buildLocationInstances Nothing registry1 overlay1
    }

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
eventsFor env uid = filter ((≡ Just uid) . peUid) ⊚ readEventLog env

spec ∷ Spec
spec = beforeAll initEnv $
    describe "Location discovery (#780) — tickLocationDiscovery" $ do

        it "a player-faction unit entering the margin marks the location \
           \discovered and emits exactly one attributable event; \
           \re-ticking with the same unit still inside emits no duplicate" $ \env → do
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

        it "a non-player unit standing inside the margin discovers \
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

        it "a DEBUG-faction unit standing inside the margin discovers \
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
                WorldManager [(pageActive, wsActive), (pageHidden, wsHidden)]
                             [pageActive]
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
           \standing in, even though the same tick would normally \
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
