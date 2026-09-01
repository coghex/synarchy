-- | Exact-attempt lifecycle, the material receipt, and load
--   reconciliation for construction designations (#1844 requirements
--   11–22).
--
--   A construction designation used to be addressed by page and
--   canonical tile alone, so every delayed operation the build AI issues
--   — claim, status, progress, payment, cancellation, completion, slope
--   cleanup — landed on whatever job happened to be at that coordinate
--   when it arrived. Cancel one and designate a successor at the same
--   tile and the old job's in-flight work silently mutated the new one.
--
--   Payment had a second problem of its own: the AI removed inventory
--   FIRST and set a durable boolean afterwards, so a cancellation
--   between the two refunded nothing for a cost that had already been
--   spent — and the boolean could not say WHAT was spent, so a refund
--   after a pack's costs changed gave back the wrong thing.
--
--   This suite drives the real handlers and the real Lua verbs against a
--   synthetic page.
module Test.Headless.Construct.AttemptIdentity (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Log (LoggerState)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Data.IORef (newIORef)
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Serialize as S
import Item.Ground (GroundItem(..), GroundItems(..), emptyGroundItems)
import World.Construct.Reconcile
    (ConstructReconcileError, reconcileStagedConstructDesignations)
import World.Construct.Revalidate (constructRefundDeps)
import World.Save.Component.Page
    ( ConstructDesignationDTO(..), ConstructDesignationDTOv1(..)
    , ConstructTargetDTO(..), GroundItemsDTO(..), PageActivityDTO(..)
    , StructurePieceDTO(..), fromConstructDTO, migrateConstructDesignations
    , toConstructDTO )
import Item.Types (ItemInstance(..))
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Structure.Types
    ( StructureSlot(..), StructurePieceData(..), StructureStageToken(..)
    , emptyChunkStructures, recordDeclinedAttempt )
import World.Thread.Command (handleWorldCommand)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..)
                  , emptyUnitManager)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Attempt
    ( ConstructAttemptId(..), advanceConstructAttemptsPast
    , firstConstructAttemptId, takeConstructAttempts )
import World.Construct.Receipt
    ( ConstructPayment(..), mkMaterialReceipt, receiptEntries
    , receiptItems )
import World.Construct.Types
    ( ConstructDesignation(..), ConstructStatus(..), ConstructTarget(..)
    , StructurePiece(..), constructDesignationPaid
    , constructDesignationReceipt, newConstructDesignation )
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Thread.Command.Cursor
    ( handleWorldAddConstructProgressCommand
    , handleWorldCancelConstructCommand
    , handleWorldDesignateConstructCommand
    , handleWorldSetConstructStatusCommand )
import World.Tile.Types (WorldTileData(..))

import Test.Headless.Construct.Fixture
    ( artOnlyPackName, fixtureItem, fixtureItems, fixturePackName
    , payerUnit, registerArtOnlyPack, registerFixturePacks )

worldSize, zSlice ∷ Int
worldSize = 64
zSlice    = 10

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "construct_attempt_identity"

pageText ∷ Text
pageText = "construct_attempt_identity"

tile ∷ (Int, Int)
tile = (5, 5)

floorPiece, wallPiece ∷ ConstructTarget
floorPiece = CtStructure (StructurePiece fixturePackName "floor" Nothing)
wallPiece  = CtStructure (StructurePiece fixturePackName "wall" (Just "ne"))

payerUid ∷ UnitId
payerUid = UnitId 1

spec ∷ Spec
spec = beforeAll setUp $ describe "construct attempt identity" $ do
    allocatorSpec
    guardSpec
    paymentSpec
    receiptSpec
    reconcileSpec
    persistenceSpec

-- * The allocator

allocatorSpec ∷ SpecWith Scene
allocatorSpec = describe "the allocator" $ do

    it "hands every candidate its own ascending id" $ \sc → do
        ws ← resetScene sc
        designate sc ws (5, 5) (7, 5) floorPiece
        ids ← attemptsOf ws
        ids `shouldBe` [ConstructAttemptId 1, ConstructAttemptId 2
                       , ConstructAttemptId 3]

    it "never reissues an id, not even after every job is cancelled" $
        \sc → do
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        first ← attemptsOf ws
        cancel sc ws tile Nothing
        designate sc ws tile tile wallPiece
        second ← attemptsOf ws
        second `shouldSatisfy` all (`notElem` first)

    it "allocates only for candidates the resolver ACCEPTED" $ \sc → do
        -- A tile the resolver already refused never reaches the
        -- allocator, so it takes no id at all. (Uniqueness is the
        -- contract and density is not, so an id burned later — by the
        -- atomic insert's own backstop refusal — would be fine too; this
        -- pins that the common case does not burn one.)
        ws ← resetScene sc
        writeIORef (wsConstructDesignationsRef ws) $ HM.singleton (6, 5)
            (newConstructDesignation zSlice floorPiece
                 (ConstructAttemptId 99))
        designate sc ws (5, 5) (6, 5) floorPiece
        readIORef (wsConstructAttemptRef ws)
            `shouldReturn` ConstructAttemptId 2

    it "advances past every id a payload already carries" $ \_ → do
        -- The load-time repair: a payload whose cursor sits below one of
        -- its own designations must not be able to reissue that id.
        advanceConstructAttemptsPast [ConstructAttemptId 9]
                                     (ConstructAttemptId 3)
            `shouldBe` ConstructAttemptId 10
        advanceConstructAttemptsPast [ConstructAttemptId 2]
                                     (ConstructAttemptId 40)
            `shouldBe` ConstructAttemptId 40

    it "hands out a contiguous block, in order" $ \_ →
        takeConstructAttempts 3 firstConstructAttemptId
            `shouldBe` ( [ConstructAttemptId 1, ConstructAttemptId 2
                         , ConstructAttemptId 3]
                       , ConstructAttemptId 4 )

-- * The guard

guardSpec ∷ SpecWith Scene
guardSpec = describe "a delayed operation from a removed attempt" $ do

    it "cannot mutate a SUCCESSOR at the same canonical tile" $ \sc → do
        -- Requirement 14's exact sequence: cancel, immediately designate
        -- a successor at the same tile, then deliver every delayed
        -- operation the old attempt could still have in flight. The
        -- successor must come out byte-identical.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [old] ← attemptsOf ws
        cancel sc ws tile Nothing
        designate sc ws tile tile wallPiece
        before ← designationAt ws
        let logger = scLogger sc
            env = scEnv sc
        handleWorldSetConstructStatusCommand env logger fixturePage
            (fst tile) (snd tile) CsClaimed old Nothing
        handleWorldAddConstructProgressCommand env logger fixturePage
            (fst tile) (snd tile) 0.75 old
        handleWorldSetConstructStatusCommand env logger fixturePage
            (fst tile) (snd tile) CsComplete old Nothing
        cancel sc ws tile (Just old)
        _ ← evalLua sc (T.concat
                [ "return tostring(construction.payMaterials('", pageText
                , "', 5, 5, ", tshow (raw old), ", 1))" ])
        _ ← evalLua sc (T.concat
                [ "return tostring(construction.beginPlacement('", pageText
                , "', 5, 5, ", tshow (raw old), "))" ])
        after ← designationAt ws
        after `shouldBe` before

    it "still applies when the attempt DOES match" $ \sc → do
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        handleWorldSetConstructStatusCommand (scEnv sc) (scLogger sc)
            fixturePage (fst tile) (snd tile) CsClaimed aid Nothing
        cdStatus <$> designationAt ws `shouldReturn` CsClaimed

    it "takes the placement hand-off only for the exact attempt" $
        \sc → do
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        beginPlacement sc (raw aid + 1) `shouldReturn` "false"
        cdStatus <$> designationAt ws `shouldReturn` CsPending
        beginPlacement sc (raw aid) `shouldReturn` "true"
        cdStatus <$> designationAt ws `shouldReturn` CsPlacing

    it "reports the attempt on every job table the AI reads" $ \sc → do
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        evalLua sc (T.concat
            [ "local j = construction.getDesignationAt('", pageText
            , "', 5, 5); return tostring(j and j.attempt)" ])
            `shouldReturn` tshow (raw aid)

    it "answers the resolver for ONE exact attempt, and only that one" $
        \sc → do
        -- Requirement 10: the worker re-checks the plan before claiming,
        -- paying and placing, through the SAME resolver admission used —
        -- not the three ad-hoc checks it used to make.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        resolvePlan sc (raw aid) `shouldReturn` "valid"
        -- A foreign attempt names no job here, which is what the AI
        -- reads as "my job is gone".
        resolvePlan sc (raw aid + 1) `shouldReturn` "nil"
        -- Its own designation is excluded from the occupancy check, so a
        -- claimed job stays valid; a PLACED piece is not.
        writeIORef (wsTilesRef ws) (withFloorAt tile)
        resolvePlan sc (raw aid) `shouldReturn` "visible-invalid"
        -- Terrain that is merely gone is not a refusal.
        writeIORef (wsTilesRef ws) emptyTiles
        resolvePlan sc (raw aid) `shouldReturn` "unresolved-terrain"

    it "answers nothing for a BUILDING — DTV-10's scope" $ \sc → do
        ws ← resetScene sc
        designate sc ws tile tile (CtBuilding "cargo_hold_S")
        [aid] ← attemptsOf ws
        resolvePlan sc (raw aid) `shouldReturn` "nil"

    it "registers an EMPTY materials table as a cost of nothing, not as \
       \no cost at all" $ \sc → do
        -- A pack's own buildability rule asks only that the `materials`
        -- field EXIST, and a receipt of no materials is a valid paid
        -- state. Collapsing the empty table into "no cost" would make a
        -- zero-material kind permanently resolver-invalid and
        -- impossible to build — while a genuinely ABSENT table must
        -- still register nothing.
        _ ← resetScene sc
        _ ← evalLua sc (T.concat
            [ "return tostring(structure.registerPackArt{ pack='freebie', "
            , "kinds={{kind='floor', buildable=true, build_work=1.0, "
            , "materials={}}}, art={{kind='floor', texture='a.png', "
            , "texHandle=41, facemap='f.png', faceHandle=42}} })" ])
        evalLua sc (T.concat
            [ "local c = structure.packBuildCost('freebie', 'floor'); "
            , "if not c then return 'nil' end; local n = 0; "
            , "for _ in pairs(c.materials) do n = n + 1 end; "
            , "return tostring(c.build_work) .. '/' .. n" ])
            `shouldReturn` "1.0/0"
        _ ← evalLua sc (T.concat
            [ "return tostring(structure.registerPackArt{ pack='costless', "
            , "kinds={{kind='floor', buildable=true, build_work=1.0}}, "
            , "art={{kind='floor', texture='a.png', texHandle=41, "
            , "facemap='f.png', faceHandle=42}} })" ])
        evalLua sc (T.concat
            [ "return tostring(structure.packBuildCost('costless', 'floor'))" ])
            `shouldReturn` "nil"

-- * Payment and the receipt

paymentSpec ∷ SpecWith Scene
paymentSpec = describe "payment" $ do

    it "removes the registered cost and records it as the receipt" $
        \sc → do
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        pay sc aid `shouldReturn` "true"
        cd ← designationAt ws
        constructDesignationPaid cd `shouldBe` True
        constructDesignationReceipt cd
            `shouldBe` Just (mkMaterialReceipt [("steel_plate", 1)])
        inventoryOf sc `shouldReturn` ["wiring"]

    it "removes NOTHING when the unit cannot cover the cost" $ \sc → do
        ws ← resetSceneWith sc []
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        pay sc aid `shouldReturn` "false"
        cdPayment <$> designationAt ws `shouldReturn` CpUnpaid
        inventoryOf sc `shouldReturn` []

    it "restores the EXACT instances when a cancellation wins the race" $
        \sc → do
        -- The lossless-winner contract: the pop removed the designation
        -- before the receipt CAS could land, so the caller must put back
        -- what it took — the same instances, in the same order, not
        -- freshly minted equals.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        before ← inventoryIdsOf sc
        cancel sc ws tile (Just aid)
        pay sc aid `shouldReturn` "false"
        inventoryIdsOf sc `shouldReturn` before

    it "refuses a SECOND payment for the same attempt" $ \sc → do
        ws ← resetSceneWith sc [ fixtureItem "steel_plate" 1
                               , fixtureItem "steel_plate" 2 ]
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        pay sc aid `shouldReturn` "true"
        pay sc aid `shouldReturn` "false"
        inventoryOf sc `shouldReturn` ["steel_plate"]

    it "hands the receipt to the ONE caller whose pop wins" $ \sc → do
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        evalLua sc (T.concat
            [ "local j = construction.cancelDesignationForRefund('"
            , pageText, "', 5, 5, ", tshow (raw aid), "); "
            , "local r = j and j.receipt and j.receipt[1]; "
            , "return tostring(r and (r.name .. 'x' .. r.count))" ])
            `shouldReturn` "steel_platex1"
        evalLua sc (T.concat
            [ "local j = construction.cancelDesignationForRefund('"
            , pageText, "', 5, 5, ", tshow (raw aid), "); "
            , "return tostring(j)" ])
            `shouldReturn` "nil"

    it "withholds a completion whose staged placement was DECLINED, and \
       \refunds it instead" $ \sc → do
        -- structure.place returning true means STAGED AND QUEUED, not
        -- committed: the world thread declines a queued placement whose
        -- target chunk evicted in between, retracting the staged entry
        -- and recording its token. A completion carrying that placement's
        -- commit window must therefore not simply delete a PAID
        -- designation, or the attempt ends with neither a structure nor
        -- its materials.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        beginPlacement sc (raw aid) `shouldReturn` "true"
        writeIORef (wsTilesRef ws) (withFloorAt tile)
        declineToken ws 0
        completeWithWindow sc aid 0 1
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0
        groundNames ws `shouldReturn` ["steel_plate"]

    it "completes when the piece really landed" $ \sc → do
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        beginPlacement sc (raw aid) `shouldReturn` "true"
        writeIORef (wsTilesRef ws) (withFloorAt tile)
        completeWithWindow sc aid 0 1
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0
        -- The receipt was SPENT on a real piece, so nothing comes back.
        groundNames ws `shouldReturn` []

    it "refuses to complete when NO piece is at the target slot" $
        \sc → do
        -- A window with no declines proves only that nothing was
        -- retracted; the completion is bound to the committed overlay
        -- itself. Without that, a caller could delete a paid designation
        -- for a placement that never happened.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        beginPlacement sc (raw aid) `shouldReturn` "true"
        completeWithWindow sc aid 0 1
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0
        groundNames ws `shouldReturn` ["steel_plate"]

    it "refuses an EMPTY commit window, which stages nothing" $ \sc → do
        -- 'StructureCommitWindow t t' has no declines by construction,
        -- and the public verb can supply one.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        beginPlacement sc (raw aid) `shouldReturn` "true"
        writeIORef (wsTilesRef ws) (withFloorAt tile)
        completeWithWindow sc aid 1 1
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0
        groundNames ws `shouldReturn` ["steel_plate"]

    it "lets the CLAIMANT abort a hand-off that staged nothing" $
        \sc → do
        -- Ordinary cancellation is refused while a designation is
        -- placing; without an abort the claimant would be left holding a
        -- paid, placing job with no completion coming.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        beginPlacement sc (raw aid) `shouldReturn` "true"
        evalLua sc (T.concat
            [ "local j = construction.abortPlacement('", pageText
            , "', 5, 5, ", tshow (raw aid), "); "
            , "local r = j and j.receipt and j.receipt[1]; "
            , "return tostring(r and r.name)" ])
            `shouldReturn` "steel_plate"
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0

    it "refuses a cancellation whose attempt is MALFORMED, rather than \
       \reading it as the player's erase" $ \sc → do
        -- The coordinate-only form is real (the player's right-click
        -- erases whatever is at a tile), so a supplied-but-invalid
        -- attempt must not collapse into it: it would remove and refund
        -- a SUCCESSOR, which is the confusion the identity exists to
        -- prevent.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        forM_ ["0", "-3", "'47'", "1.5"] $ \bad → do
            _ ← evalLua sc (T.concat
                [ "return tostring(construction.cancelDesignationForRefund('"
                , pageText, "', 5, 5, ", bad, "))" ])
                `shouldReturn` "nil"
            HM.size <$> readIORef (wsConstructDesignationsRef ws)
                `shouldReturn` 1
        -- …while the OMITTED form still erases, as it always did.
        _ ← evalLua sc (T.concat
            [ "return tostring(construction.cancelDesignationForRefund('"
            , pageText, "', 5, 5) ~= nil)" ])
            `shouldReturn` "true"
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0

    it "refuses an abort for a job that never took the hand-off, or for \
       \another attempt" $ \sc → do
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        abortPlacement sc (raw aid) `shouldReturn` "nil"
        beginPlacement sc (raw aid) `shouldReturn` "true"
        abortPlacement sc (raw aid + 1) `shouldReturn` "nil"
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 1

    it "refuses to CANCEL a designation inside its placement hand-off" $
        \sc → do
        -- The claimant has by then staged its piece and queued the world
        -- command that commits it. Popping here would refund the receipt
        -- while that command still lands, leaving the player with both
        -- the structure and its materials back — so cancellation simply
        -- loses the race, and the completion settles the attempt either
        -- way.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        beginPlacement sc (raw aid) `shouldReturn` "true"
        evalLua sc (T.concat
            [ "return tostring(construction.cancelDesignationForRefund('"
            , pageText, "', 5, 5, ", tshow (raw aid), "))" ])
            `shouldReturn` "nil"
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 1
        groundNames ws `shouldReturn` []
        -- The player's coordinate-only erase loses it too: what matters
        -- is the hand-off, not which form named it.
        cancel sc ws tile Nothing
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 1

    it "still CANCELS a completion whose site drifted during the \
       \hand-off" $ \sc → do
        -- The hand-off exempts the worker's OWN staged piece from the
        -- occupancy check and nothing else. A terrain, fluid or
        -- catalogue mutation the world thread drains inside that window
        -- must still cancel and refund rather than complete.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        beginPlacement sc (raw aid) `shouldReturn` "true"
        writeIORef (wsTilesRef ws) (surfaceAt (zSlice + 1))
        completeWithWindow sc aid 0 1
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0
        groundNames ws `shouldReturn` ["steel_plate"]

    it "refuses a STRUCTURE completion that offers no commit window" $
        \sc → do
        -- Without a window there is nothing to check, so deleting would
        -- lose a paid designation's receipt for a piece that may never
        -- have landed. The public verb permits the windowless form (a
        -- BUILDING has no window to give), so the handler is where the
        -- distinction has to be made.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        _ ← evalLua sc (T.concat
            [ "construction.setJobStatus('", pageText, "', 5, 5, "
            , "'complete', ", tshow (raw aid), "); return 'ok'" ])
        drainWorldQueue (scEnv sc) (scLogger sc)
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 1
        groundNames ws `shouldReturn` []

    it "still completes a BUILDING with no window — it has none to give" $
        \sc → do
        -- A building stakes through building.spawn, which reports its own
        -- success synchronously; there is no staged placement to confirm.
        ws ← resetScene sc
        designate sc ws tile tile (CtBuilding "cargo_hold_S")
        [aid] ← attemptsOf ws
        _ ← evalLua sc (T.concat
            [ "construction.setJobStatus('", pageText, "', 5, 5, "
            , "'complete', ", tshow (raw aid), "); return 'ok'" ])
        drainWorldQueue (scEnv sc) (scLogger sc)
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0

    it "grandfathers a paid job across a build-cost change" $ \sc → do
        -- Requirement 17: the receipt is never regenerated. Re-reading
        -- the pack is exactly what could not reproduce what was spent.
        ws ← resetScene sc
        designate sc ws tile tile floorPiece
        [aid] ← attemptsOf ws
        _ ← pay sc aid
        before ← constructDesignationReceipt <$> designationAt ws
        -- Whatever the catalogue says now, the record does not move.
        after ← constructDesignationReceipt <$> designationAt ws
        after `shouldBe` before
        after `shouldBe` Just (mkMaterialReceipt [("steel_plate", 1)])

-- * The receipt value itself

receiptSpec ∷ SpecWith Scene
receiptSpec = describe "the receipt" $ do

    it "is canonical: summed per material, ascending, positives only" $
        \_ → do
        let r = mkMaterialReceipt
                    [("wiring", 1), ("steel_plate", 2), ("wiring", 3)
                    , ("nothing", 0)]
        receiptEntries r `shouldBe` [("steel_plate", 2), ("wiring", 4)]
        -- Two orders of the same multiset are the SAME receipt, which is
        -- what makes the encoding deterministic.
        r `shouldBe` mkMaterialReceipt
                         [("wiring", 4), ("steel_plate", 2)]

    it "flattens to one element per unit, in its own order" $ \_ →
        receiptItems (mkMaterialReceipt [("b", 1), ("a", 2)])
            `shouldBe` ["a", "a", "b"]

    it "distinguishes an EMPTY receipt from no receipt at all" $ \_ → do
        constructDesignationPaid (paid (mkMaterialReceipt []))
            `shouldBe` True
        constructDesignationPaid unpaidJob `shouldBe` False
  where
    paid r = (newConstructDesignation 0 floorPiece firstConstructAttemptId)
                 { cdPayment = CpPaid r }
    unpaidJob = newConstructDesignation 0 floorPiece firstConstructAttemptId

-- * Scene

data Scene = Scene
    { scEnv    ∷ EngineEnv
    , scLua    ∷ LuaBackendState
    , scLogger ∷ LoggerState
    }

setUp ∷ IO Scene
setUp = do
    EngineInitResult env ← initializeEngineHeadless
    ls ← newBareLuaBackend env
    logger ← readIORef (loggerRef env)
    pure (Scene env ls logger)

-- | A page with one flat loaded chunk, the fixture packs registered, and
--   one payer holding @inv@. Rebuilt per example so no case inherits
--   another's designations, ids or inventory.
resetSceneWith ∷ Scene → [ItemInstance] → IO WorldState
resetSceneWith sc inv = do
    let env = scEnv sc
    registerFixturePacks env
    writeIORef (itemManagerRef env) fixtureItems
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.singleton payerUid (payerUnit fixturePage inv) }
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws) flatTiles
    writeIORef (wsGroundItemsRef ws) emptyGroundItems
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    pure ws

resetScene ∷ Scene → IO WorldState
resetScene sc = resetSceneWith sc
    [fixtureItem "steel_plate" 1, fixtureItem "wiring" 2]

designate ∷ Scene → WorldState → (Int, Int) → (Int, Int) → ConstructTarget
          → IO ()
designate sc _ws (x1, y1) (x2, y2) tgt =
    handleWorldDesignateConstructCommand (scEnv sc) (scLogger sc)
        fixturePage x1 y1 x2 y2 tgt Nothing

cancel ∷ Scene → WorldState → (Int, Int) → Maybe ConstructAttemptId → IO ()
cancel sc _ws (gx, gy) mAttempt =
    handleWorldCancelConstructCommand (scEnv sc) (scLogger sc) fixturePage
        gx gy mAttempt

attemptsOf ∷ WorldState → IO [ConstructAttemptId]
attemptsOf ws =
    sort ∘ map cdAttempt ∘ HM.elems <$> readIORef (wsConstructDesignationsRef ws)

designationAt ∷ WorldState → IO ConstructDesignation
designationAt ws = do
    m ← readIORef (wsConstructDesignationsRef ws)
    case HM.lookup tile m of
        Just cd → pure cd
        Nothing → fail "no construction designation at the fixture tile"

raw ∷ ConstructAttemptId → Word64
raw (ConstructAttemptId n) = n

pay ∷ Scene → ConstructAttemptId → IO Text
pay sc aid = evalLua sc $ T.concat
    [ "return tostring(construction.payMaterials('", pageText
    , "', 5, 5, ", tshow (raw aid), ", 1))" ]

resolvePlan ∷ Scene → Word64 → IO Text
resolvePlan sc n = evalLua sc $ T.concat
    [ "return tostring(construction.resolvePlan('", pageText
    , "', 5, 5, ", tshow n, "))" ]

-- | The fixture geography with a floor already PLACED on a tile.
withFloorAt ∷ (Int, Int) → WorldTileData
withFloorAt (gx, gy) = flatTiles
    { wtdChunks = HM.adjust addIt (ChunkCoord 0 0) (wtdChunks flatTiles) }
  where
    addIt lc = lc { lcStructures = HM.insert key piece (lcStructures lc) }
    key = (gx, gy, fromIntegral (fromEnum SFloor) ∷ Word8)
    piece = StructurePieceData 1 2 (zSlice + 1)

-- | Record one staged attempt as DECLINED, which is exactly what the
--   world thread does when a queued placement's target chunk has
--   evicted.
declineToken ∷ WorldState → Word64 → IO ()
declineToken ws n =
    atomicModifyIORef' (wsStructureStageRef ws) $ \st →
        (recordDeclinedAttempt (StructureStageToken n) st, ())

completeWithWindow ∷ Scene → ConstructAttemptId → Word64 → Word64 → IO ()
completeWithWindow sc aid lo hi = do
    _ ← evalLua sc $ T.concat
        [ "construction.setJobStatus('", pageText, "', 5, 5, 'complete', "
        , tshow (raw aid), ", ", tshow lo, ", ", tshow hi, "); return 'ok'" ]
    -- The verb queues; drain the world thread so the assertion reads the
    -- settled state rather than a command still in flight.
    drainWorldQueue (scEnv sc) (scLogger sc)

drainWorldQueue ∷ EngineEnv → LoggerState → IO ()
drainWorldQueue env logger = go (200 ∷ Int)
  where
    go 0 = pure ()
    go n = do
        m ← Q.tryReadQueue (wsWorldQueue (toWorldSimCapability env))
        case m of
            Nothing → pure ()
            Just c  → handleWorldCommand env logger c ≫ go (n - 1)

abortPlacement ∷ Scene → Word64 → IO Text
abortPlacement sc n = evalLua sc $ T.concat
    [ "return tostring(construction.abortPlacement('", pageText
    , "', 5, 5, ", tshow n, "))" ]

beginPlacement ∷ Scene → Word64 → IO Text
beginPlacement sc n = evalLua sc $ T.concat
    [ "return tostring(construction.beginPlacement('", pageText
    , "', 5, 5, ", tshow n, "))" ]

inventoryOf ∷ Scene → IO [Text]
inventoryOf sc = map iiDefName <$> inventoryList sc

inventoryIdsOf ∷ Scene → IO [(Text, Word64)]
inventoryIdsOf sc =
    map (\i → (iiDefName i, iiInstanceId i)) <$> inventoryList sc

inventoryList ∷ Scene → IO [ItemInstance]
inventoryList sc = do
    um ← readIORef (unitManagerRef (scEnv sc))
    pure (maybe [] uiInventory (HM.lookup payerUid (umInstances um)))

-- | Run one debug-console line and return its value with the console's
--   own surrounding quotes stripped. @executeDebugLua@ renders a STRING
--   return as a quoted Lua literal, which every assertion here would
--   otherwise have to spell.
evalLua ∷ Scene → Text → IO Text
evalLua sc src = unquote <$> executeDebugLua (lbsLuaState (scLua sc)) src
  where
    unquote t
        | T.length t ≥ 2, T.head t ≡ '"', T.last t ≡ '"' =
            T.dropEnd 1 (T.drop 1 t)
        | otherwise = t

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

flatTiles ∷ WorldTileData
flatTiles = surfaceAt zSlice

surfaceAt ∷ Int → WorldTileData
surfaceAt z =
    let coord = ChunkCoord 0 0
        area  = chunkSize * chunkSize
        col   = ColumnTiles
                  { ctStartZ = 0
                  , ctMats   = VU.replicate 20 1
                  , ctSlopes = VU.replicate 20 0
                  , ctVeg    = VU.replicate 20 0
                  }
        lc = LoadedChunk
               { lcCoord = coord
               , lcTiles = V.replicate area col
               , lcSurfaceMap = VU.replicate area z
               , lcTerrainSurfaceMap = VU.replicate area z
               , lcFluidMap = V.replicate area Nothing
               , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
               , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
               , lcMagma = Nothing, lcStructures = emptyChunkStructures
               }
    in WorldTileData { wtdChunks = HM.singleton coord lc, wtdMaxChunks = 200 }

-- * Load reconciliation

reconcileSpec ∷ SpecWith Scene
reconcileSpec = describe "load reconciliation" $ do

    it "keeps a job whose pack still resolves" $ \sc → do
        ws ← resetScene sc
        seedSaved sc ws floorPiece CpUnpaid CsPending
        reconcile sc ws `shouldReturn` Right ()
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 1

    it "self-clears a job whose art has gone, and REFUNDS its receipt \
       \into the staged page's own ground items" $ \sc → do
        -- The refund must not go through a live-session verb: a load
        -- stages a replacement session and swaps it in, so anything
        -- deposited into the session being REPLACED is lost at
        -- publication.
        ws ← resetScene sc
        seedSaved sc ws ghostPiece
            (CpPaid (mkMaterialReceipt [("steel_plate", 2)])) CsPending
        reconcile sc ws `shouldReturn` Right ()
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0
        groundNames ws `shouldReturn` ["steel_plate", "steel_plate"]

    it "self-clears a job whose BUILD METADATA has gone" $ \sc → do
        ws ← resetScene sc
        registerArtOnlyPack (scEnv sc)
        seedSaved sc ws artOnlyPiece CpUnpaid CsPending
        reconcile sc ws `shouldReturn` Right ()
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 0

    it "retains a job whose terrain is merely unloaded" $ \sc → do
        -- The common case: a load publishes with almost nothing
        -- resident, so terrain-dependent reconciliation belongs to the
        -- chunk-publication hook, not here.
        ws ← resetScene sc
        writeIORef (wsTilesRef ws) emptyTiles
        seedSaved sc ws floorPiece CpUnpaid CsPending
        reconcile sc ws `shouldReturn` Right ()
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 1

    it "detaches a restored PLACING job back to pending" $ \sc → do
        -- #1329 empties the claim registry on every load, so nothing can
        -- finish a restored hand-off. Left as-is it would also be
        -- permanently invisible to revalidation, which skips that state.
        ws ← resetScene sc
        seedSaved sc ws floorPiece CpUnpaid CsPlacing
        reconcile sc ws `shouldReturn` Right ()
        cdStatus <$> designationAt ws `shouldReturn` CsPending

    it "REJECTS a load whose receipt names a missing item definition" $
        \sc → do
        -- A lossless refund is impossible, so publishing would silently
        -- destroy the player's materials.
        ws ← resetScene sc
        seedSaved sc ws floorPiece
            (CpPaid (mkMaterialReceipt [("unobtainium", 1)])) CsPending
        reconcile sc ws `shouldSatisfy'` isLeft'

    it "reconstructs a LEGACY paid job's receipt from current metadata" $
        \sc → do
        ws ← resetScene sc
        seedSaved sc ws floorPiece CpLegacyPaid CsPending
        reconcile sc ws `shouldReturn` Right ()
        constructDesignationReceipt <$> designationAt ws
            `shouldReturn` Just (mkMaterialReceipt [("steel_plate", 1)])

    it "REJECTS a legacy paid job whose metadata cannot reconstruct it" $
        \sc → do
        -- The alternatives are inventing a refund and silently losing
        -- the materials; both are wrong, so the load stops.
        ws ← resetScene sc
        seedSaved sc ws ghostPiece CpLegacyPaid CsPending
        reconcile sc ws `shouldSatisfy'` isLeft'

    it "leaves a LEGACY UNPAID job with no receipt at all" $ \sc → do
        ws ← resetScene sc
        seedSaved sc ws floorPiece CpUnpaid CsPending
        reconcile sc ws `shouldReturn` Right ()
        cdPayment <$> designationAt ws `shouldReturn` CpUnpaid

    it "leaves a BUILDING job alone, receipt-less and unresolved" $
        \sc → do
        ws ← resetScene sc
        seedSaved sc ws (CtBuilding "no_such_building") CpUnpaid CsPending
        reconcile sc ws `shouldReturn` Right ()
        HM.size <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` 1

-- * Persistence

persistenceSpec ∷ SpecWith Scene
persistenceSpec = describe "persistence" $ do

    it "round-trips attempt ids, the allocator and receipts through the \
       \real codec" $ \_ → do
        let cd = (newConstructDesignation zSlice floorPiece
                      (ConstructAttemptId 41))
                     { cdStatus = CsClaimed, cdProgress = 0.25
                     , cdPayment = CpPaid (mkMaterialReceipt
                           [("steel_plate", 2), ("wood_log", 1)]) }
            page = savePage (HM.singleton tile cd) (ConstructAttemptId 42)
        roundTripActivity page `shouldBe` Right (HM.singleton tile cd
                                                , ConstructAttemptId 42)

    it "encodes the same session to the same bytes every time" $ \_ → do
        let cd = (newConstructDesignation zSlice floorPiece
                      (ConstructAttemptId 3))
                     { cdPayment = CpPaid (mkMaterialReceipt
                           [("wood_log", 1), ("steel_plate", 2)]) }
            page = savePage (HM.singleton tile cd) (ConstructAttemptId 4)
        encodeActivity page `shouldBe` encodeActivity page

    it "migrates a v3 payload deterministically, by ascending tile key" $
        \_ → do
        -- A pre-#1844 payload records no identity at all. Hash order
        -- would make a migrated save differ run to run, so the
        -- assignment walks the keys in order and the allocator lands one
        -- past the highest id issued.
        let legacy = HM.fromList
                [ ((7, 1), legacyDTO False)
                , ((2, 9), legacyDTO True)
                , ((2, 1), legacyDTO False) ]
            (migrated, next) = migrateConstructDesignations legacy
        [ (k, cdiAttempt d) | (k, d) ← L.sortOn fst (HM.toList migrated) ]
            `shouldBe` [ ((2, 1), ConstructAttemptId 1)
                       , ((2, 9), ConstructAttemptId 2)
                       , ((7, 1), ConstructAttemptId 3) ]
        next `shouldBe` ConstructAttemptId 4
        -- The paid one becomes the migration-only state staging
        -- resolves; the unpaid ones record nothing.
        [ cdiPayment d | (_, d) ← L.sortOn fst (HM.toList migrated) ]
            `shouldBe` [CpUnpaid, CpLegacyPaid, CpUnpaid]

    it "raises a payload's allocator past every id it carries" $ \_ → do
        let cd = newConstructDesignation zSlice floorPiece
                     (ConstructAttemptId 12)
            page = savePage (HM.singleton tile cd) (ConstructAttemptId 3)
        (snd <$> roundTripActivity page) `shouldBe` Right (ConstructAttemptId 13)

-- * Reconciliation / persistence helpers

ghostPiece, artOnlyPiece ∷ ConstructTarget
ghostPiece   = CtStructure (StructurePiece "no_such_pack" "floor" Nothing)
artOnlyPiece = CtStructure (StructurePiece artOnlyPackName "floor" Nothing)

-- | Seed one page with a SAVED designation, as staging finds it.
seedSaved ∷ Scene → WorldState → ConstructTarget → ConstructPayment
          → ConstructStatus → IO ()
seedSaved _sc ws tgt payment st =
    writeIORef (wsConstructDesignationsRef ws) $ HM.singleton tile
        ((newConstructDesignation zSlice tgt (ConstructAttemptId 5))
             { cdPayment = payment, cdStatus = st })

reconcile ∷ Scene → WorldState
          → IO (Either ConstructReconcileError ())
reconcile sc ws = do
    deps ← constructRefundDeps (scEnv sc)
    cat ← readIORef (structureArtCatalogRef (scEnv sc))
    reconcileStagedConstructDesignations deps cat (scLogger sc) ws

groundNames ∷ WorldState → IO [Text]
groundNames ws =
    sort ∘ map (iiDefName ∘ giInst) ∘ HM.elems ∘ gisItems
        <$> readIORef (wsGroundItemsRef ws)

-- | hspec's 'shouldSatisfy' wants 'Show'; 'ConstructReconcileError' has
--   it, so this is only here to keep the predicate readable at the call
--   sites and to say what a Left MEANS.
shouldSatisfy' ∷ IO (Either ConstructReconcileError ())
               → (Either ConstructReconcileError () → Bool) → Expectation
shouldSatisfy' act p = act ⌦ (`shouldSatisfy` p)

isLeft' ∷ Either ConstructReconcileError () → Bool
isLeft' (Left _) = True
isLeft' _        = False

emptyTiles ∷ WorldTileData
emptyTiles = WorldTileData { wtdChunks = HM.empty, wtdMaxChunks = 200 }

-- | One page's activity slice carrying just the construction state, at
--   the CURRENT wire shape.
savePage ∷ HM.HashMap (Int, Int) ConstructDesignation → ConstructAttemptId
         → PageActivityDTO
savePage designs next = PageActivityDTO
    { padPageId = fixturePage
    , padMine = HM.empty
    , padConstruct = toConstructDTO designs
    , padChop = HM.empty, padTill = HM.empty, padPlant = HM.empty
    , padFloraHarvests = HM.empty, padCropPlots = HM.empty
    , padGroundItems = GroundItemsDTO 0 HM.empty
    , padSpoilPiles = HM.empty
    , padConstructNextAttempt = next
    }

encodeActivity ∷ PageActivityDTO → BS.ByteString
encodeActivity = S.encode

-- | Encode, decode, and apply the SAME allocator repair the codec's own
--   assembly step performs.
roundTripActivity
    ∷ PageActivityDTO
    → Either String (HM.HashMap (Int, Int) ConstructDesignation
                    , ConstructAttemptId)
roundTripActivity page = do
    back ← S.decode (encodeActivity page)
    pure ( fromConstructDTO (padConstruct back)
         , advanceConstructAttemptsPast
               (map cdiAttempt (HM.elems (padConstruct back)))
               (padConstructNextAttempt back) )

legacyDTO ∷ Bool → ConstructDesignationDTOv1
legacyDTO wasPaid = ConstructDesignationDTOv1
    { cdi1Z = zSlice
    , cdi1Target = CtStructureD (StructurePieceDTO fixturePackName "floor"
                                                   Nothing)
    , cdi1Status = CsPending
    , cdi1Progress = 0
    , cdi1MaterialsPaid = wasPaid
    }
