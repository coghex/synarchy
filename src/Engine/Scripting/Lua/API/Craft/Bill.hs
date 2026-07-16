{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Craft bills (#329) — standing production orders against a work
--   station. The bill queue lives per world page (wsCraftBillsRef) and
--   has no world-thread side effects, so every verb below is one
--   synchronous atomicModifyIORef' — claims between two crafters
--   resolve atomically and callers read results back immediately (no
--   queue round-trip). The pure transitions live in Craft.Bills; this
--   layer adds argument decoding, the add-time validation, and the Lua
--   table shape. Work is data for the craft AI (#329).
module Engine.Scripting.Lua.API.Craft.Bill
    ( craftAddBillFn
    , craftCancelBillFn
    , craftGetBillFn
    , craftGetBillsFn
    , craftClaimBillFn
    , craftReleaseBillFn
    , craftAddBillProgressFn
    , craftCompleteBillCycleFn
    , craftSetBillPausedFn
    , craftSetBillWorkingFn
    , craftReorderBillFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Data.List (sortOn)
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import Craft.Types
import Craft.Bills
import Unit.Types (UnitId(..), UnitManager(..))
import World.State.Types (WorldState(..))
import Building.Types (BuildingId(..), BuildingInstance(..), BuildingDef(..),
                       BuildingManager(..))

-- | craft.addBill(bid, recipeId [, count [, untilTarget]]) → billId |
--   nil, err. Queue a standing order at a station. @untilTarget@, when
--   given as a positive integer, requests an UNTIL-STOCK bill (#795):
--   @count@ is ignored and the bill instead crafts @recipeId@ while the
--   live ground stock of the recipe's first output stays below
--   @untilTarget@ (Craft.Bills.addUntilStockBill) — see
--   scripts/unit_ai_craft.lua for the claim-time/cycle-boundary stock
--   re-evaluation this mode relies on. Without @untilTarget@, @count@
--   keeps its original meaning: omitted or < 1 = repeat forever.
--   Validated here so a malformed bill can never enter the queue: the
--   recipe must exist and not be repair-tagged, the station must exist
--   on the active page, its def must offer the recipe's station
--   operation, and (until-stock only) the recipe must have at least
--   one output to target. The station does NOT need to be Built yet —
--   queueing bills on an under-construction station is fine; the craft
--   AI only works Built ones.
craftAddBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftAddBillFn env = do
    bidArg    ← Lua.tointeger 1
    ridArg    ← Lua.tostring 2
    countArg  ← Lua.tointeger 3
    targetArg ← Lua.tointeger 4
    case (bidArg, ridArg) of
        (Just b, Just ridBS) → do
            let bid     = BuildingId (fromIntegral b)
                rid     = TE.decodeUtf8Lenient ridBS
                count   = maybe (-1) fromIntegral countArg
                mTarget = targetArg ≫= \t →
                    if t > 0 then Just (fromIntegral t ∷ Int) else Nothing
            result ← Lua.liftIO $ do
                mPage ← activeWorldPage env
                rm    ← readIORef (recipeManagerRef env)
                bm    ← readIORef (buildingManagerRef env)
                let gate = do
                        (pageId, ws) ← note "no active world" mPage
                        recipe ← note ("unknown recipe " <> rid)
                                      (lookupRecipe rid rm)
                        when (rdRepairAxis recipe ≢ Nothing) $
                            Left (rid <> " is a repair recipe — bills run \
                                         \craft recipes only")
                        inst ← note "no such building"
                                    (HM.lookup bid (bmInstances bm))
                        def  ← note ("unknown building def " <> biDefName inst)
                                    (HM.lookup (biDefName inst) (bmDefs bm))
                        unless (biPage inst ≡ pageId) $
                            Left "station is on another world page"
                        unless (rdStation recipe `elem` bdOperations def) $
                            Left ("station " <> biDefName inst
                                  <> " does not offer " <> rdStation recipe)
                        Right (ws, recipe)
                case gate of
                    Left err → return (Left err)
                    Right (ws, recipe) → case mTarget of
                        Just target → case rdOutputs recipe of
                            [] → return
                                (Left (rid <> " has no outputs — cannot run \
                                             \an until-stock bill"))
                            (out:_) → do
                                billId ← atomicModifyIORef'
                                    (wsCraftBillsRef ws) $
                                    addUntilStockBill bid rid target
                                                       (riItem out)
                                return (Right billId)
                        Nothing → do
                            billId ← atomicModifyIORef' (wsCraftBillsRef ws) $
                                addBill bid rid count
                            return (Right billId)
            case result of
                Right (BillId n) → do
                    Lua.pushinteger (fromIntegral n)
                    return 1
                Left err → do
                    Lua.pushnil
                    Lua.pushstring (TE.encodeUtf8 err)
                    return 2
        _ → do
            Lua.pushnil
            Lua.pushstring "craft.addBill: expected (buildingId, recipeId\
                           \ [, count [, untilTarget]])"
            return 2
  where
    note e = maybe (Left e) Right

-- | craft.cancelBill(billId) → true | false.
craftCancelBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftCancelBillFn env = withBillId env $ \ws billId →
    atomicModifyIORef' (wsCraftBillsRef ws) (removeBill billId)

-- | craft.releaseBill(billId) → true | false. Back to pending; cycle
--   progress is kept (see Craft.Bills.releaseBill).
craftReleaseBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftReleaseBillFn env = withBillId env $ \ws billId →
    atomicModifyIORef' (wsCraftBillsRef ws) (releaseBill billId)

-- | craft.setBillPaused(billId, paused) → true | false. Pausing blocks
--   fresh claims (Craft.Bills.claimAvailable) without ripping a
--   claimant already mid-cycle off its work — the #330 panel's pause
--   control. That claimant still only rides through to the end of the
--   in-flight cycle: the craft_job AI (scripts/unit_ai_craft.lua)
--   aborts on its own if it observes the pause before reaching its
--   "working" phase, and Craft.Bills.completeBillCycle releases the
--   claim on cycle completion rather than rolling into another one
--   (#796).
craftSetBillPausedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftSetBillPausedFn env = do
    idArg     ← Lua.tointeger 1
    pausedArg ← Lua.toboolean 2
    ok ← case idArg of
        Nothing → return False
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return False
                Just (_, ws) →
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        setBillPaused (BillId (fromIntegral n)) pausedArg
    Lua.pushboolean ok
    return 1

-- | craft.setBillWorking(billId, working) → true | false (#590). The
--   craft_job AI's marker for "I am standing at the station actively
--   pouring work into this bill right now" — Power.Network.
--   activeCraftConsumersOn keys a station's live power demand off this,
--   not off the claim alone (see Craft.Bills.cbWorking). Set True on
--   entering the working phase, False on leaving it early; completion
--   and release already clear it on their own.
craftSetBillWorkingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftSetBillWorkingFn env = do
    idArg      ← Lua.tointeger 1
    workingArg ← Lua.toboolean 2
    ok ← case idArg of
        Nothing → return False
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return False
                Just (_, ws) →
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        setBillWorking (BillId (fromIntegral n)) workingArg
    Lua.pushboolean ok
    return 1

-- | craft.reorderBill(billId, "up" | "down") → true | false. Swaps
--   with the immediate neighbour at the same station in the current
--   listing order (Craft.Bills.reorderBill) — the #330 panel's manual
--   reorder control. False at either end of the queue, for an unknown
--   bill, or for any direction string but "up"/"down".
craftReorderBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftReorderBillFn env = do
    idArg  ← Lua.tointeger 1
    dirArg ← Lua.tostring 2
    ok ← case (idArg, dirArg >>= toDirection) of
        (Just n, Just dir) → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return False
                Just (_, ws) →
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        reorderBill dir (BillId (fromIntegral n))
        _ → return False
    Lua.pushboolean ok
    return 1
  where
    toDirection bs = case TE.decodeUtf8Lenient bs of
        "up"   → Just MoveUp
        "down" → Just MoveDown
        _      → Nothing

-- | Shared decode + bool-return shape for the (billId) → bool verbs.
withBillId ∷ EngineEnv → (WorldState → BillId → IO Bool)
           → Lua.LuaE Lua.Exception Lua.NumResults
withBillId env act = do
    idArg ← Lua.tointeger 1
    ok ← case idArg of
        Nothing → return False
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return False
                Just (_, ws) → act ws (BillId (fromIntegral n))
    Lua.pushboolean ok
    return 1

-- | craft.claimBill(billId, uid, timeout) → true | false. Atomic
--   claim-or-refresh: succeeds when the bill is unclaimed, already
--   ours, or the standing claim is stale (dead claimant, or no refresh
--   within @timeout@ game-seconds — the AI passes its
--   craft_claim_timeout param so the tunable stays Lua-side with the
--   others). Exactly one of two simultaneous claimants wins.
craftClaimBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftClaimBillFn env = do
    idArg      ← Lua.tointeger 1
    uidArg     ← Lua.tointeger 2
    timeoutArg ← Lua.tonumber 3
    ok ← case (idArg, uidArg, timeoutArg) of
        (Just n, Just u, Just tmo) → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return False
                Just (_, ws) → do
                    now ← readIORef (gameTimeRef env)
                    um  ← readIORef (unitManagerRef env)
                    let alive c = HM.member c (umInstances um)
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        claimBill now (realToFrac tmo) alive
                                  (BillId (fromIntegral n))
                                  (UnitId (fromIntegral u))
        _ → return False
    Lua.pushboolean ok
    return 1

-- | craft.addBillProgress(billId, delta) → newProgress | nil. Pours
--   work into the current cycle (clamped to [0, 1]) and returns the
--   post-add progress synchronously — the AI acts on 1.0 by firing
--   craft.executeAt + craft.completeBillCycle itself, so consumption
--   and production stay in the executeAt verb (one authority).
craftAddBillProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftAddBillProgressFn env = do
    idArg    ← Lua.tointeger 1
    deltaArg ← Lua.tonumber 2
    result ← case (idArg, deltaArg) of
        (Just n, Just delta) → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) →
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        addBillProgress (BillId (fromIntegral n))
                                        (realToFrac delta)
        _ → return Nothing
    case result of
        Just p  → Lua.pushnumber (Lua.Number (realToFrac p)) >> return 1
        Nothing → Lua.pushnil >> return 1

-- | craft.completeBillCycle(billId) → remaining | nil. One craft
--   cycle done: progress resets, finite counts tick down (the bill is
--   removed at 0), repeat bills return -1 forever.
craftCompleteBillCycleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftCompleteBillCycleFn env = do
    idArg ← Lua.tointeger 1
    result ← case idArg of
        Nothing → return Nothing
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) →
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        completeBillCycle (BillId (fromIntegral n))
    case result of
        Just r  → Lua.pushinteger (fromIntegral r) >> return 1
        Nothing → Lua.pushnil >> return 1

-- | craft.getBill(billId) → table | nil.
craftGetBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftGetBillFn env = do
    idArg ← Lua.tointeger 1
    mBill ← case idArg of
        Nothing → return Nothing
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) → do
                    bills ← readIORef (wsCraftBillsRef ws)
                    return (lookupBill (BillId (fromIntegral n)) bills)
    case mBill of
        Just bill → pushBill bill >> return 1
        Nothing   → Lua.pushnil >> return 1

-- | craft.getBills([bid]) → array of bill tables on the active world,
--   oldest first — every bill, or one station's queue when @bid@ is
--   given (the #330 station panel's view).
craftGetBillsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftGetBillsFn env = do
    bidArg ← Lua.tointeger 1
    billList ← Lua.liftIO $ do
        mPage ← activeWorldPage env
        case mPage of
            Nothing      → return []
            Just (_, ws) → do
                bills ← readIORef (wsCraftBillsRef ws)
                return $ case bidArg of
                    Just b  → billsForStation (BuildingId (fromIntegral b))
                                              bills
                    Nothing → sortOn cbId (HM.elems (cbsBills bills))
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] billList) $ \(i, bill) → do
        pushBill bill
        Lua.rawseti (-2) (fromIntegral i)
    return 1

-- | Push one bill as a Lua table: { id, station, recipe, remaining,
--   progress, seq, paused, working, mode, target?, outputItem?,
--   claimant?, claimedAt? }. remaining -1 = repeat forever OR
--   until-stock (see mode to tell them apart); seq is the
--   manual-reorder sort key (billsForStation / #330's reorderBill);
--   working (#590) is true only while the claimant is actively pouring
--   work, not merely holding the claim (Craft.Bills.cbWorking). mode is
--   "fixed" | "repeat" | "until" (Craft.Bills.BillMode); target and
--   outputItem (#795) are present only for "until" — the persisted
--   stock target and the item def name it counts against, which the
--   craft AI and the #330 panel both re-derive live stock from via
--   item.listGround() (one shared scope: ground stock, unbounded, no
--   fetch-ladder rungs) so the two stay in lockstep.
pushBill ∷ CraftBill → Lua.LuaE Lua.Exception ()
pushBill bill = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                   >> Lua.setfield (-2) k
        putS k v = Lua.pushstring (TE.encodeUtf8 v) >> Lua.setfield (-2) k
    putI "id"        (unBillId (cbId bill))
    putI "station"   (unBuildingId (cbStation bill))
    putS "recipe"    (cbRecipe bill)
    Lua.pushinteger (fromIntegral (cbRemaining bill))
    Lua.setfield (-2) "remaining"
    putN "progress"  (cbProgress bill)
    putI "seq"       (cbSeq bill)
    Lua.pushboolean (cbPaused bill)
    Lua.setfield (-2) "paused"
    Lua.pushboolean (cbWorking bill)
    Lua.setfield (-2) "working"
    putS "mode" (billModeName (cbMode bill))
    when (cbMode bill ≡ UntilStock) $ do
        putI "target" (cbTarget bill)
        putS "outputItem" (cbOutputItem bill)
    forM_ (cbClaimant bill) $ \(UnitId u) → do
        putI "claimant" u
        putN "claimedAt" (cbClaimedAt bill)

-- | The Lua-facing spelling of 'BillMode' (#795) — round-trips with
--   the panel/AI's "fixed" | "repeat" | "until" vocabulary.
billModeName ∷ BillMode → Text
billModeName FixedCount    = "fixed"
billModeName RepeatForever = "repeat"
billModeName UntilStock    = "until"
