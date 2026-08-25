{-# LANGUAGE Strict #-}
-- | "craft bill claimant reconciliation" (#1680): the ENGINE half of
--   the dead-claimant sweep — that production actually runs it, on
--   every loaded page, while paused.
--
--   'Test.Headless.Craft.Bills' covers the pure transition
--   ('Craft.Bills.reconcileBillClaimants') exhaustively. What it cannot
--   show is the three properties the defect actually turned on, all of
--   which are about WHERE the call is wired:
--
--   * It reaches a page that is loaded but NOT visible, and a page with
--     no power nodes at all — the two bands
--     'World.Thread.Power.tickPowerNetworks' skips, and the reason
--     hooking the sweep there would have left the bug alive
--     (requirement 6).
--   * It runs while the engine is globally PAUSED. This is the
--     save/load lifecycle boundary requirement 7 names: bills restore
--     VERBATIM (#763 — 'World.Load.Stage' never prunes them, and the
--     component validator deliberately tolerates a claimant absent from
--     the whole session), and 'World.Load.Publish' brings the session up
--     paused, so a dangling claimed-and-working bill would keep the
--     station drawing power for a worker that does not exist until the
--     player happened to unpause. Requirement 7 is satisfied by the
--     AFTER-publish arm: the codec is untouched, the live state is
--     repaired by the first ordinary world tick.
--   * It is driven by the REAL 'World.Thread.Time.tickWorldTime' on the
--     REAL world thread the harness starts — nothing here calls the
--     tick itself, so a future edit that unhooks it fails this gate.
--
--   The engine is this spec's own (@aroundAll withHeadlessEngine@ in
--   @Spec.hs@) for the same reason 'Test.Headless.Power.Demolition' has
--   one: it installs its own two-page world manager and rewrites the
--   unit manager. Both pages are in-memory 'emptyWorldState' pages
--   carrying a synthetic flat chunk (the live world thread renders the
--   visible one every tick, and an empty column crashes it), so nothing
--   here costs worldgen.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "craft bill claimant reconciliation"'@.
module Test.Headless.Craft.BillReconcile (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, writeIORef)
import Building.Types (BuildingId(..))
import Craft.Bills
    ( BillId, CraftBill(..), CraftBills(..), addBill, addBillProgress
    , claimBill, lookupBill, setBillPaused, setBillWorking )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Structure.Types (emptyChunkStructures)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import Power.Types (PowerNodes(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Entities (fromBillQueueDTO, toBillQueueDTO)
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Tile.Types (WorldTileData(..))

-- * Fixture identities

-- | The visible page and a page that is LOADED but hidden. The hidden
--   one is the whole point of requirement 6: it is skipped by every
--   visible-only tick, power included.
homePage, hiddenPage ∷ WorldPageId
homePage   = WorldPageId "cbr_home"
hiddenPage = WorldPageId "cbr_hidden"

-- | Only 'liveUid' is ever written into the unit manager. 'deadUid'
--   names a worker that has been destroyed — the state a crafter killed
--   mid-cycle leaves behind.
liveUid, deadUid ∷ UnitId
liveUid = UnitId 1
deadUid = UnitId 2

station ∷ BuildingId
station = BuildingId 7

-- * Fixtures

-- | Mirrors 'Test.Headless.Power.Demolition.minimalDef': only the
--   fields this path reads carry any weight, and this path reads none
--   of them — the unit exists solely to be present in @umInstances@.
minimalDef ∷ UnitDef
minimalDef = UnitDef
    { udName = "acolyte", udNamePool = Nothing
    , udDisplayName = Just "Acolyte"
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
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

mkUnit ∷ WorldPageId → UnitInstance
mkUnit page = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 4, uiGridY = 6, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" 100
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | A flat, fluid-free chunk at (0,0), carrying a REAL per-tile column
--   vector: the visible page is rendered by the live world thread every
--   tick and an empty column crashes it (same reason
--   'Test.Headless.Power.Demolition.flatChunk' carries one).
flatChunk ∷ LoadedChunk
flatChunk =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.singleton 1
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0
            }
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.replicate area col
        , lcSurfaceMap        = VU.replicate area 0
        , lcTerrainSurfaceMap = VU.replicate area 0
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate area 0
        , lcWaterTableMap     = VU.replicate area 0
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

flatTiles ∷ WorldTileData
flatTiles = WorldTileData
    { wtdChunks    = HM.singleton (lcCoord flatChunk) flatChunk
    , wtdMaxChunks = 1
    }

-- | One PAUSED bill, mid-cycle (progress 0.4, 'cbWorking' True), held
--   by @holder@ — the exact shape a crafter destroyed while working
--   leaves behind, and the one no takeover can ever repair.
--
--   Paused deliberately, per the issue's own reading: unpausing only
--   makes a bill CLAIMABLE, so an unpaused stale bill can still be
--   repaired incidentally by a live rival. A paused one cannot be
--   repaired by anything but this sweep, which is what makes it the
--   honest fixture.
workingBill ∷ UnitId → (CraftBills, BillId)
workingBill holder =
    let (b0, bid) = addBill station "bill_reconcile_probe" 3 emptyBills
        (b1, _)   = claimBill 10 30 (const True) bid holder b0
        (b2, _)   = setBillWorking bid True b1
        (b3, _)   = setBillPaused bid True b2
    in (b3, bid)
  where emptyBills = CraftBills HM.empty 1

-- * Scene

-- | Two loaded pages, only 'homePage' visible; exactly one live unit in
--   the registry; the engine PAUSED, as 'World.Load.Publish' leaves a
--   freshly loaded session.
--
--   Neither page is given any power nodes: the sweep must repair a page
--   with no grid at all (requirement 6), which is precisely what
--   'World.Thread.Power.tickPowerNetworks' short-circuits away from.
resetScene ∷ EngineEnv → IO (WorldState, WorldState)
resetScene env = do
    wsHome   ← emptyWorldState
    wsHidden ← emptyWorldState
    writeIORef (wsTilesRef wsHome) flatTiles
    writeIORef (wsTilesRef wsHidden) flatTiles
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(homePage, wsHome), (hiddenPage, wsHidden)]
        , wmVisible = [homePage]
        }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton "acolyte" minimalDef
        , umInstances = HM.singleton liveUid (mkUnit homePage)
        }
    writeIORef (enginePausedRef env) True
    pure (wsHome, wsHidden)

-- | Poll @ws@'s live bill queue until @done@ holds, or give up. The
--   world thread ticks about every 16 ms, so this normally settles on
--   the first pass; the deadline only exists so a REGRESSION fails
--   instead of hanging.
awaitBill ∷ WorldState → BillId → (CraftBill → Bool) → IO (Maybe CraftBill)
awaitBill ws bid done = go (300 ∷ Int)
  where
    go 0 = current
    go n = do
        mBill ← current
        case mBill of
            Just bill | done bill → pure (Just bill)
            _ → threadDelay 20000 >> go (n - 1)
    current = lookupBill bid ⊚ readIORef (wsCraftBillsRef ws)

disowned ∷ CraftBill → Bool
disowned bill = isNothing (cbClaimant bill)

spec ∷ SpecWith EngineEnv
spec = describe "craft bill claimant reconciliation (#1680)" $ do
    it "the live world tick disowns a dead claimant's PAUSED, working \
       \bill on the VISIBLE page, while the engine is paused" $ \env → do
        (wsHome, _) ← resetScene env
        let (bills, bid) = workingBill deadUid
        writeIORef (wsCraftBillsRef wsHome) bills
        mBill ← awaitBill wsHome bid disowned
        stillPaused ← readIORef (enginePausedRef env)
        stillPaused `shouldBe` True
        (cbClaimant ⊚ mBill) `shouldBe` Just Nothing
        (cbWorking ⊚ mBill) `shouldBe` Just False

    it "and on a page that is LOADED but NOT visible, and has no power \
       \nodes at all" $ \env → do
        (_, wsHidden) ← resetScene env
        let (bills, bid) = workingBill deadUid
        writeIORef (wsCraftBillsRef wsHidden) bills
        nodesBefore ← readIORef (wsPowerNodesRef wsHidden)
        mBill ← awaitBill wsHidden bid disowned
        (cbClaimant ⊚ mBill) `shouldBe` Just Nothing
        (cbWorking ⊚ mBill) `shouldBe` Just False
        -- The page genuinely had no grid, so this cannot have been the
        -- power tick doing the work.
        nodesAfter ← readIORef (wsPowerNodesRef wsHidden)
        HM.null (pnsNodes nodesBefore) `shouldBe` True
        HM.null (pnsNodes nodesAfter) `shouldBe` True

    it "keeps progress, remaining, pause and station across the \
       \reconciliation" $ \env → do
        (wsHome, _) ← resetScene env
        let (b0, bid)  = workingBill deadUid
            (bills, _) = addBillProgress bid 0.4 b0
        writeIORef (wsCraftBillsRef wsHome) bills
        mBill ← awaitBill wsHome bid disowned
        (cbClaimant ⊚ mBill) `shouldBe` Just Nothing
        (cbWorking ⊚ mBill) `shouldBe` Just False
        (cbProgress ⊚ mBill) `shouldBe` Just 0.4
        (cbRemaining ⊚ mBill) `shouldBe` Just 3
        (cbPaused ⊚ mBill) `shouldBe` Just True
        (cbStation ⊚ mBill) `shouldBe` Just station

    it "leaves a LIVE claimant's bill alone across many ticks" $ \env → do
        (wsHome, _) ← resetScene env
        let (bills, bid) = workingBill liveUid
        writeIORef (wsCraftBillsRef wsHome) bills
        -- No settling predicate to wait on here (the assertion is that
        -- nothing happens), so give the world thread a generous window
        -- of real ticks before reading back.
        threadDelay 500000
        mBill ← lookupBill bid ⊚ readIORef (wsCraftBillsRef wsHome)
        (cbClaimant ⊚ mBill) `shouldBe` Just (Just liveUid)
        (cbWorking ⊚ mBill) `shouldBe` Just True

    it "the save/load boundary: a bill restored VERBATIM from the save \
       \codec still carries the dangling claim, and the first live tick \
       \after publish is what clears it" $ \env → do
        (wsHome, _) ← resetScene env
        let (bills, bid) = workingBill deadUid
        -- The wire half: through the component's own DTO and encoding,
        -- exactly as 'World.Save.Component.Entities.craftBillsCodec'
        -- writes and reads a page's queue. Nothing about #1680 changes
        -- it — the claim and the working flag come back untouched, which
        -- is the #763 verbatim-restore contract this must not disturb.
        restored ← case S.decode (S.encode (toBillQueueDTO bills)) of
            Left err  → expectationFailure ("bill queue decode: " <> err)
                          >> pure bills
            Right dto → pure (fromBillQueueDTO dto)
        (cbClaimant ⊚ lookupBill bid restored) `shouldBe` Just (Just deadUid)
        (cbWorking ⊚ lookupBill bid restored) `shouldBe` Just True
        (cbProgress ⊚ lookupBill bid restored)
            `shouldBe` (cbProgress ⊚ lookupBill bid bills)
        -- The live half: publishing that restored queue onto a page of a
        -- PAUSED session (what 'World.Load.Publish' leaves behind) is
        -- repaired by the ordinary world tick, with no unpause.
        writeIORef (wsCraftBillsRef wsHome) restored
        mBill ← awaitBill wsHome bid disowned
        (cbClaimant ⊚ mBill) `shouldBe` Just Nothing
        (cbWorking ⊚ mBill) `shouldBe` Just False
