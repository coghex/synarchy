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

-- | craft.addBill(bid, recipeId [, count]) → billId | nil, err.
--   Queue a standing order at a station: run @recipeId@ @count@ times
--   (omitted or < 1 = repeat forever). Validated here so a malformed
--   bill can never enter the queue: the recipe must exist and not be
--   repair-tagged, the station must exist on the active page, and its
--   def must offer the recipe's station operation. The station does
--   NOT need to be Built yet — queueing bills on an under-construction
--   station is fine; the craft AI only works Built ones.
craftAddBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftAddBillFn env = do
    bidArg   ← Lua.tointeger 1
    ridArg   ← Lua.tostring 2
    countArg ← Lua.tointeger 3
    case (bidArg, ridArg) of
        (Just b, Just ridBS) → do
            let bid   = BuildingId (fromIntegral b)
                rid   = TE.decodeUtf8 ridBS
                count = maybe (-1) fromIntegral countArg
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
                        Right ws
                case gate of
                    Left err → return (Left err)
                    Right ws → do
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
                           \ [, count])"
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
--   fresh claims (Craft.Bills.claimAvailable) without touching a
--   claimant already mid-cycle — the #330 panel's pause control.
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
    toDirection bs = case TE.decodeUtf8 bs of
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
--   progress, seq, paused, claimant?, claimedAt? }. remaining -1 =
--   repeat forever; seq is the manual-reorder sort key
--   (billsForStation / #330's reorderBill).
pushBill ∷ CraftBill → Lua.LuaE Lua.Exception ()
pushBill bill = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                   >> Lua.setfield (-2) k
    putI "id"        (unBillId (cbId bill))
    putI "station"   (unBuildingId (cbStation bill))
    Lua.pushstring (TE.encodeUtf8 (cbRecipe bill))
    Lua.setfield (-2) "recipe"
    Lua.pushinteger (fromIntegral (cbRemaining bill))
    Lua.setfield (-2) "remaining"
    putN "progress"  (cbProgress bill)
    putI "seq"       (cbSeq bill)
    Lua.pushboolean (cbPaused bill)
    Lua.setfield (-2) "paused"
    forM_ (cbClaimant bill) $ \(UnitId u) → do
        putI "claimant" u
        putN "claimedAt" (cbClaimedAt bill)
