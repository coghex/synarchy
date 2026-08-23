{-# LANGUAGE OverloadedStrings, TupleSections #-}
-- | The three remaining unit-owned simulation paths resolve their world
--   from the UNIT's own @uiPage@, not from whichever page is active
--   (#1593) — the follow-ups #797 (line of sight, combat awareness) and
--   #1208 (ground pickup and drops) left behind.
--
--   Movement pathing, the teleport / re-ground surface lookup, and wound
--   infection climate each used to pick ONE active-or-first-visible page
--   and apply it to every unit. This gate builds two pages with
--   deliberately OPPOSED terrain heights and climates, puts a unit at the
--   SAME numeric coordinates on each, and pins three properties across
--   all three paths:
--
--     * each unit resolves its OWN page (a low-page unit never sees the
--       high page's z, a cold-page unit never gets the hot page's
--       climate);
--     * reordering @wmVisible@ — including emptying it, so the owning
--       page is loaded but HIDDEN — changes nothing; and
--     * the negative fallbacks are honest: a page absent from
--       @wmWorlds@, a page with no loaded chunk at the coordinates, a
--       page with no generation parameters, and a movement sim state
--       whose unit instance is gone all get NOTHING rather than
--       borrowing another page's answer.
--
--   Everything is driven against real manager refs through the real
--   entry points ('tickAllMovement', 'handleUnitTeleportCommand',
--   'handleUnitReGroundCommand', 'tickAllWounds'): the defect lived in
--   those functions' page resolution, which a pure test structurally
--   cannot see. Pages are in-memory 'emptyWorldState's carrying one
--   hand-built chunk, so two live worlds cost no worldgen.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "unit simulation page ownership"'@.
module Test.Headless.Unit.SimPageOwnership (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Random (mkStdGen)
import Combat.Wounds.Tick (tickAllWounds)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Infection.Types
    (InfectionDef(..), InfectionManager(..))
import Structure.Types (emptyChunkStructures)
import Unit.Faction (Faction(..))
import Unit.Sim.Types
import Unit.Thread.Command.Lifecycle
    (handleUnitReGroundCommand, handleUnitTeleportCommand, lookupSurfaceZ)
import Unit.Thread.Movement (tickAllMovement)
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), Wound(..), defaultNaturalResistance
    , emptyUnitManager )
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Tile.Types (WorldTileData(..), emptyWorldTileData)
import World.Weather.Types (ClimateState(..), initClimateState)

-- * Fixture identities

-- | A loaded page whose ground sits HIGH and whose climate is hot and
--   therefore infects FAST.
pageHigh ∷ WorldPageId
pageHigh = WorldPageId "sim_ownership_high"

-- | A loaded page whose ground sits LOW and whose climate is cold and
--   therefore infects SLOWLY. Deliberately opposed to 'pageHigh' on
--   every axis, so borrowing the wrong page is never a tie.
pageLow ∷ WorldPageId
pageLow = WorldPageId "sim_ownership_low"

-- | A loaded page with NO chunk at the fixture coordinates and NO
--   generation parameters — the "present but has no answer" page.
pageBare ∷ WorldPageId
pageBare = WorldPageId "sim_ownership_bare"

-- | A page id with no entry in @wmWorlds@ at all.
pageGhost ∷ WorldPageId
pageGhost = WorldPageId "sim_ownership_ghost"

uidHigh, uidLow, uidBare, uidGhost, uidOrphan ∷ UnitId
uidHigh  = UnitId 1
uidLow   = UnitId 2
uidBare  = UnitId 3
uidGhost = UnitId 4
-- | A sim state with NO unit instance behind it: a mover that outlived
--   its unit. It owns no page and so must get no terrain.
uidOrphan = UnitId 5

-- | Terrain z of each page's single chunk. Every unit stands at the same
--   numeric coordinates, so these are the only thing telling the two
--   pages' answers apart.
highZ, lowZ ∷ Int
highZ = 20
lowZ  = 5

-- | The one tile everything happens on, and its centre.
tileX, tileY ∷ Int
tileX = 4
tileY = 4

tileCentre ∷ (Float, Float)
tileCentre = (fromIntegral tileX + 0.5, fromIntegral tileY + 0.5)

-- | Where a mover starts: same tile row, two tiles west of the target,
--   so one fast tick arrives.
moverStart ∷ (Float, Float)
moverStart = (fromIntegral tileX - 1.5, fromIntegral tileY + 0.5)

-- | The z every unit is planted at before a tick, matching NEITHER
--   page's surface, so any snap is visible and attributable.
neutralZ ∷ Int
neutralZ = 0

-- * World fixtures

-- | One origin chunk at a uniform terrain z.
flatChunkAt ∷ Int → LoadedChunk
flatChunkAt z =
    let area  = chunkSize * chunkSize
        terrV = VU.replicate area z
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.empty
        , lcSurfaceMap        = terrV
        , lcTerrainSurfaceMap = terrV
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

tilesAt ∷ Int → WorldTileData
tilesAt z = WorldTileData
    { wtdChunks = HM.singleton (ChunkCoord 0 0) (flatChunkAt z)
    , wtdMaxChunks = 1 }

-- | Generation parameters whose ONLY interesting content is the global
--   mean temperature. The climate grid is left empty, so
--   'World.Weather.Lookup.lookupLocalClimate' falls back to
--   @csGlobalTemp@ with a fixed 0.5 humidity in both pages — which makes
--   temperature the single axis separating the two climates, and makes
--   'Combat.Wounds.Infection.climateOnsetFactor' exactly computable.
paramsAtTemp ∷ Float → WorldGenParams
paramsAtTemp t = defaultWorldGenParams
    { wgpWorldSize    = climateWorldSize
    , wgpClimateState = (initClimateState climateWorldSize)
                            { csGlobalTemp = t } }

climateWorldSize ∷ Int
climateWorldSize = 64

-- | 30 °C: warm, so infection sets in FASTER than with no climate at all.
hotTemp ∷ Float
hotTemp = 30

-- | -10 °C: cold, so infection sets in SLOWER than with no climate at
--   all. The two bracket the 1.0 no-climate factor from both sides, so
--   "borrowed the other page" and "found no page" are distinguishable
--   outcomes rather than one lumped failure.
coldTemp ∷ Float
coldTemp = -10

-- * Unit fixtures

-- | Enough of a def to make a live, tickable unit with one body part the
--   wound tick can find.
minimalDef ∷ UnitDef
minimalDef = UnitDef
    { udName = "acolyte", udNamePool = Nothing
    , udDisplayName = Just "acolyte"
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 100.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

mkUnit ∷ WorldPageId → [Wound] → UnitInstance
mkUnit page wounds = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0
    , uiGridX = fst tileCentre, uiGridY = snd tileCentre
    , uiGridZ = neutralZ, uiRealZ = fromIntegral neutralZ, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = wounds
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | One open, undressed, un-disinfected slash old enough to be past the
--   infection grace period at 'fixtureNow'. Identical for every unit, so
--   the ONLY thing that can separate two units' infection outcomes is
--   their pages' climates.
freshWound ∷ Wound
freshWound = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = 0.5
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

-- | Game time the fixture pins, comfortably past the 60 s grace period.
fixtureNow ∷ Double
fixtureNow = 1000

-- * Infection catalogue

-- | Two competing surface infections with disjoint temperature bands and
--   identical everything else — so the ONLY thing steering selection is
--   the local climate, and the growth arithmetic (aggressiveness ×
--   infectability = 1) is unaffected by which one is picked.
infectionCatalogue ∷ InfectionManager
infectionCatalogue = InfectionManager $ HM.fromList
    [ ("hotbug",  bandedInfection "hotbug"  25    40)
    , ("coldbug", bandedInfection "coldbug" (-40)  0) ]

bandedInfection ∷ Text → Float → Float → InfectionDef
bandedInfection iid tmin tmax = InfectionDef
    { infId = iid, infName = iid, infIcon = "", infCategory = "bacterial"
    , infSites = ["surface"], infBaseWeight = 1.0
    , infTempMin = tmin, infTempMax = tmax
    , infMoistMin = 0.0, infMoistMax = 1.0
    , infAggressiveness = 1.0, infInfectability = 1.0
    , infCurableBy = [], infCureRate = 1.0, infWoundInfectable = True
    , infEffects = [], infTransmissibility = 0, infTransmission = [] }

-- * Scene construction

-- | How this example wants @wmVisible@ ordered. Every assertion below is
--   run under all three, because the whole point is that none of them
--   may change an outcome — including 'Hidden', where the owning pages
--   are loaded but nothing is visible at all.
data Visibility = HighFirst | LowFirst | Hidden
    deriving (Show, Eq, Enum, Bounded)

visibleFor ∷ Visibility → [WorldPageId]
visibleFor HighFirst = [pageHigh, pageLow, pageBare]
visibleFor LowFirst  = [pageBare, pageLow, pageHigh]
visibleFor Hidden    = []

-- | Install the two opposed pages, the bare page, and the five units.
--   Every unit that gets a wound gets the SAME one, and the stat RNG is
--   reset to a fixed generator, so two scenes differing only in
--   'Visibility' are otherwise bit-identical starting states.
resetScene ∷ EngineEnv → Visibility → [Wound] → IO ()
resetScene env vis wounds = do
    wsHigh ← emptyWorldState
    wsLow  ← emptyWorldState
    wsBare ← emptyWorldState
    writeIORef (wsTilesRef wsHigh) (tilesAt highZ)
    writeIORef (wsTilesRef wsLow)  (tilesAt lowZ)
    -- Deliberately EMPTY: loaded page, no chunk at the coordinates.
    writeIORef (wsTilesRef wsBare) emptyWorldTileData
    writeIORef (wsGenParamsRef wsHigh) (Just (paramsAtTemp hotTemp))
    writeIORef (wsGenParamsRef wsLow)  (Just (paramsAtTemp coldTemp))
    -- Deliberately absent: a loaded page that cannot answer a climate.
    writeIORef (wsGenParamsRef wsBare) Nothing
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [ (pageHigh, wsHigh), (pageLow, wsLow)
                      , (pageBare, wsBare) ]
        , wmVisible = visibleFor vis }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" minimalDef
        , umInstances = HM.fromList
            [ (uidHigh,  mkUnit pageHigh  wounds)
            , (uidLow,   mkUnit pageLow   wounds)
            , (uidBare,  mkUnit pageBare  wounds)
            , (uidGhost, mkUnit pageGhost wounds) ]
        }
    writeIORef (infectionManagerRef env) infectionCatalogue
    writeIORef (gameTimeRef env) fixtureNow
    writeIORef (statRNGRef env) (mkStdGen 20260823)

-- | Every uid the scene installs a sim state for, including the orphan
--   that has no unit instance.
allSimUids ∷ [UnitId]
allSimUids = [uidHigh, uidLow, uidBare, uidGhost, uidOrphan]

-- | A sim-state map with one entry per 'allSimUids', all identical.
simStatesOf ∷ (UnitId → UnitSimState) → IO (IORef UnitThreadState)
simStatesOf f = newIORef emptyUnitThreadState
    { utsSimStates = HM.fromList [ (uid, f uid) | uid ← allSimUids ] }

-- | A standing mover heading for 'tileCentre' fast enough to arrive in
--   one tick, planted at 'neutralZ' so an arrival snap is unambiguous.
mover ∷ MoveHazardPolicy → UnitSimState
mover hazard = baseSimState
    { usRealX = fst moverStart, usRealY = snd moverStart
    , usTarget = Just MoveTarget
        { mtTargetX = fst tileCentre, mtTargetY = snd tileCentre
        , mtSpeed = 100, mtHazard = hazard }
    , usState = Walking
    }

-- | An idle unit already standing on the tile, at a z that matches
--   NEITHER page's surface — what re-ground is supposed to correct.
idleOnTile ∷ UnitSimState
idleOnTile = baseSimState
    { usRealX = fst tileCentre, usRealY = snd tileCentre }

baseSimState ∷ UnitSimState
baseSimState = UnitSimState
    { usRealX = fst tileCentre, usRealY = snd tileCentre
    , usGridZ = neutralZ, usRealZ = fromIntegral neutralZ
    , usTarget = Nothing
    , usPose = Standing, usState = Idle, usFacing = DirE
    , usLocalPath = []
    , usDrinkUntil = Nothing, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Nothing, usTransitionStride = 1
    , usPostTransition = []
    , usClimbFromTile = Nothing, usClimbToTile = Nothing
    , usClimbStartTime = Nothing, usClimbSlipAt = Nothing
    , usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0
    }

-- * Readers

simOf ∷ IORef UnitThreadState → UnitId → IO (Maybe UnitSimState)
simOf utsRef uid = HM.lookup uid . utsSimStates <$> readIORef utsRef

zOf ∷ IORef UnitThreadState → UnitId → IO (Maybe Int)
zOf utsRef uid = fmap usGridZ <$> simOf utsRef uid

targetOf ∷ IORef UnitThreadState → UnitId → IO (Maybe (Maybe MoveTarget))
targetOf utsRef uid = fmap usTarget <$> simOf utsRef uid

instanceOf ∷ EngineEnv → UnitId → IO (Maybe UnitInstance)
instanceOf env uid =
    HM.lookup uid . umInstances <$> readIORef (unitManagerRef env)

mirrorZOf ∷ EngineEnv → UnitId → IO (Maybe Int)
mirrorZOf env uid = fmap uiGridZ <$> instanceOf env uid

-- | Every unit's (infection level, infection type) after a wound tick,
--   in a stable uid order so two orderings compare directly.
infectionRows ∷ EngineEnv → IO [(UnitId, Float, Text)]
infectionRows env = do
    um ← readIORef (unitManagerRef env)
    pure [ (uid, lvl, ty)
         | uid ← [uidHigh, uidLow, uidBare, uidGhost]
         , Just inst ← [HM.lookup uid (umInstances um)]
         , (lvl, ty) ← case uiWounds inst of
             (w : _) → [(woundInfection w, woundInfectionType w)]
             []      → [] ]

levelFor ∷ [(UnitId, Float, Text)] → UnitId → Float
levelFor rows uid = case [ l | (u, l, _) ← rows, u ≡ uid ] of
    (l : _) → l
    []      → -1

typeFor ∷ [(UnitId, Float, Text)] → UnitId → Text
typeFor rows uid = case [ t | (u, _, t) ← rows, u ≡ uid ] of
    (t : _) → t
    []      → "<missing>"

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "unit simulation page ownership" $ do

    describe "movement pathing" $ do

        it "paths each mover against its OWN page under every ordering" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis []
                utsRef ← simStatesOf (const (mover FallPermitted))
                tickAllMovement 0.1 env utsRef
                -- Each unit landed on ITS page's surface. Under the
                -- pre-#1593 head-of-wmVisible snapshot these two were
                -- always the same number.
                (vis, ) <$> zOf utsRef uidHigh
                    `shouldReturn` (vis, Just highZ)
                (vis, ) <$> zOf utsRef uidLow
                    `shouldReturn` (vis, Just lowZ)

        it "gives a mover on an UNLOADED page no terrain at all" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis []
                utsRef ← simStatesOf (const (mover FallPermitted))
                tickAllMovement 0.1 env utsRef
                -- pageGhost is in no wmWorlds entry: nothing to ground
                -- against, so the arrival snap leaves z where it was.
                (vis, ) <$> zOf utsRef uidGhost
                    `shouldReturn` (vis, Just neutralZ)
                -- Same for a loaded page with no chunk at the tile.
                (vis, ) <$> zOf utsRef uidBare
                    `shouldReturn` (vis, Just neutralZ)

        it "gives a sim state with no unit instance no terrain" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis []
                utsRef ← simStatesOf (const (mover FallPermitted))
                tickAllMovement 0.1 env utsRef
                -- The orphan owns no page, so it may not borrow the
                -- active one's heightmap — it keeps its own z.
                (vis, ) <$> zOf utsRef uidOrphan
                    `shouldReturn` (vis, Just neutralZ)

        it "keeps the fail-closed outcome for protected requests" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis []
                utsRef ← simStatesOf (const (mover FallProhibited))
                tickAllMovement 0.1 env utsRef
                -- #1217's rule, unchanged: no verified own-page terrain
                -- ⇒ the request is abandoned rather than judged against
                -- another world.
                (vis, ) <$> targetOf utsRef uidGhost
                    `shouldReturn` (vis, Just Nothing)
                (vis, ) <$> targetOf utsRef uidBare
                    `shouldReturn` (vis, Just Nothing)
                (vis, ) <$> targetOf utsRef uidOrphan
                    `shouldReturn` (vis, Just Nothing)
                -- …while a mover WITH its own page still moves, even
                -- when that page is not visible.
                (vis, ) <$> zOf utsRef uidHigh
                    `shouldReturn` (vis, Just highZ)
                (vis, ) <$> zOf utsRef uidLow
                    `shouldReturn` (vis, Just lowZ)

        it "is bit-identical across every wmVisible ordering" $ \env → do
            let runUnder vis = do
                    resetScene env vis []
                    utsRef ← simStatesOf (const (mover FallPermitted))
                    tickAllMovement 0.1 env utsRef
                    uts ← readIORef utsRef
                    pure (utsSimStates uts)
            first ← runUnder HighFirst
            forM_ [LowFirst, Hidden] $ \vis → do
                other ← runUnder vis
                (vis, other) `shouldBe` (vis, first)

    describe "teleport and re-ground surface lookup" $ do

        it "resolves lookupSurfaceZ against the NAMED page only" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis []
                (vis, ) <$> lookupSurfaceZ env pageHigh tileX tileY
                    `shouldReturn` (vis, Just highZ)
                (vis, ) <$> lookupSurfaceZ env pageLow tileX tileY
                    `shouldReturn` (vis, Just lowZ)
                -- Absent page, and loaded page with no chunk there:
                -- Nothing, never a neighbour page's z.
                (vis, ) <$> lookupSurfaceZ env pageGhost tileX tileY
                    `shouldReturn` (vis, Nothing)
                (vis, ) <$> lookupSurfaceZ env pageBare tileX tileY
                    `shouldReturn` (vis, Nothing)

        it "teleports each unit onto its OWN page's surface" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis []
                utsRef ← simStatesOf (const idleOnTile)
                forM_ [uidHigh, uidLow, uidBare, uidGhost] $ \uid →
                    handleUnitTeleportCommand env utsRef uid
                        (fst tileCentre) (snd tileCentre) Nothing
                (vis, ) <$> zOf utsRef uidHigh
                    `shouldReturn` (vis, Just highZ)
                (vis, ) <$> zOf utsRef uidLow
                    `shouldReturn` (vis, Just lowZ)
                -- No page, or no chunk: the documented z = 0 fallback,
                -- not the other page's surface.
                (vis, ) <$> zOf utsRef uidGhost `shouldReturn` (vis, Just 0)
                (vis, ) <$> zOf utsRef uidBare  `shouldReturn` (vis, Just 0)
                -- The render mirror follows the same page resolution.
                (vis, ) <$> mirrorZOf env uidHigh
                    `shouldReturn` (vis, Just highZ)
                (vis, ) <$> mirrorZOf env uidLow
                    `shouldReturn` (vis, Just lowZ)

        it "teleports a vanished unit nowhere and touches nothing" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis []
                utsRef ← simStatesOf (const idleOnTile)
                handleUnitTeleportCommand env utsRef uidOrphan
                    (fst tileCentre) (snd tileCentre) Nothing
                -- The orphan's sim state moves to the requested x/y at
                -- the no-page z, and nobody else is disturbed.
                (vis, ) <$> zOf utsRef uidOrphan `shouldReturn` (vis, Just 0)
                (vis, ) <$> zOf utsRef uidHigh
                    `shouldReturn` (vis, Just neutralZ)
                (vis, ) <$> zOf utsRef uidLow
                    `shouldReturn` (vis, Just neutralZ)

        it "re-grounds only the EDITED page's units" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis []
                utsRef ← simStatesOf (const idleOnTile)
                -- Both units are idle at the SAME numeric coordinates;
                -- the page tag is the only thing separating them.
                handleUnitReGroundCommand env utsRef pageLow tileX tileY
                (vis, ) <$> zOf utsRef uidLow
                    `shouldReturn` (vis, Just lowZ)
                (vis, ) <$> zOf utsRef uidHigh
                    `shouldReturn` (vis, Just neutralZ)
                (vis, ) <$> zOf utsRef uidBare
                    `shouldReturn` (vis, Just neutralZ)
                (vis, ) <$> zOf utsRef uidGhost
                    `shouldReturn` (vis, Just neutralZ)
                (vis, ) <$> zOf utsRef uidOrphan
                    `shouldReturn` (vis, Just neutralZ)
                -- Only the matching unit's render mirror moved.
                (vis, ) <$> mirrorZOf env uidLow
                    `shouldReturn` (vis, Just lowZ)
                (vis, ) <$> mirrorZOf env uidHigh
                    `shouldReturn` (vis, Just neutralZ)
                -- …and the other page's edit is symmetric.
                handleUnitReGroundCommand env utsRef pageHigh tileX tileY
                (vis, ) <$> zOf utsRef uidHigh
                    `shouldReturn` (vis, Just highZ)
                (vis, ) <$> zOf utsRef uidLow
                    `shouldReturn` (vis, Just lowZ)

        it "re-grounds nobody for a page that cannot answer" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis []
                utsRef ← simStatesOf (const idleOnTile)
                handleUnitReGroundCommand env utsRef pageGhost tileX tileY
                handleUnitReGroundCommand env utsRef pageBare tileX tileY
                forM_ allSimUids $ \uid →
                    (vis, uid, ) <$> zOf utsRef uid
                        `shouldReturn` (vis, uid, Just neutralZ)

    describe "wound infection climate" $ do

        it "resolves each unit's climate from its OWN page" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis [freshWound]
                tickAllWounds env 10
                rows ← infectionRows env
                let hot  = levelFor rows uidHigh
                    cold = levelFor rows uidLow
                    none = levelFor rows uidBare
                -- The hot page infects fastest, the cold page slowest,
                -- and a page with no generation parameters keeps the
                -- neutral no-climate factor BETWEEN them — so borrowing
                -- another page's climate and finding no climate are
                -- distinguishable, not one lumped failure.
                (vis, hot > none)  `shouldBe` (vis, True)
                (vis, none > cold) `shouldBe` (vis, True)
                (vis, cold > 0)    `shouldBe` (vis, True)

        it "leaves a page with no generation parameters untyped" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis [freshWound]
                tickAllWounds env 10
                rows ← infectionRows env
                -- No climate ⇒ no selection, and the untyped default
                -- onset factor — never the neighbouring page's bug.
                (vis, typeFor rows uidBare)  `shouldBe` (vis, "")
                (vis, typeFor rows uidGhost) `shouldBe` (vis, "")
                (vis, levelFor rows uidBare)
                    `shouldBe` (vis, levelFor rows uidGhost)

        it "picks each page's own climate-favoured infection" $
            \env → forM_ [minBound .. maxBound] $ \vis → do
                resetScene env vis [freshWound]
                tickAllWounds env 10
                rows ← infectionRows env
                (vis, typeFor rows uidHigh) `shouldBe` (vis, "hotbug")
                (vis, typeFor rows uidLow)  `shouldBe` (vis, "coldbug")

        it "is unchanged by every wmVisible ordering" $ \env → do
            -- Fresh equivalent wounds and the SAME initial RNG for each
            -- ordering: infection type is sticky once set, so comparing
            -- a reorder AFTER a first tick would be vacuous.
            let runUnder vis = do
                    resetScene env vis [freshWound]
                    tickAllWounds env 10
                    infectionRows env
            first ← runUnder HighFirst
            forM_ [LowFirst, Hidden] $ \vis → do
                other ← runUnder vis
                (vis, other) `shouldBe` (vis, first)
