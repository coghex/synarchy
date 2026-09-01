-- | The LIVE Lua surface of DURABLE transfer orders (#1247, epic
--   #1013 slice UIT-2B): @unit.createTransferOrder@,
--   @unit.getTransferOrders@, @unit.advanceTransferOrder@,
--   @unit.commitTransferOrder@ and @unit.failTransferOrder@ driven
--   through the REAL registered production API against REAL manager
--   refs and a REAL per-page @wsTransferOrdersRef@ — plus #1253's
--   @unit.cancelTransferOrder@ / @unit.pruneTransferOrder@ and the
--   orphan cleanup 'Unit.Thread.Command.Lifecycle' performs when a
--   carrier is destroyed.
--
--   Sibling of 'Test.Headless.Unit.TransferApi', whose fixture
--   constructors this reuses; what it adds is the geometry an ORDER
--   needs and an immediate transfer never has — a counterpart twenty
--   tiles away, a multi-tile footprint approached from its far end, a
--   second world page, and a world that CHANGES between the request and
--   the commit.
--
--   Registered under a describe beginning "Unit transfer Lua API" so
--   @--match "Unit transfer Lua API"@ covers the contract verbs and
--   their order-executor siblings in one gate.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Unit transfer Lua API (orders"'@.
module Test.Headless.Unit.TransferOrderApi (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Building.Types
    ( BuildingId(..), BuildingInstance(..), BuildingManager(..)
    , emptyBuildingManager )
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemInstance(..), emptyItemManager)
import Unit.Faction (Faction(..))
import Unit.Sim.Types (emptyUnitThreadState)
import Unit.Thread.Command.Lifecycle (handleUnitDestroyCommand)
import Unit.Thread.Command.Pose (handleUnitKillCommand)
import Unit.Transfer.Orders (TransferOrderId(..), TransferOrders(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Page.Types (WorldPageId(..))
import World.Save.Integrity
    ( IntegrityError(..), PageEntities(..), danglingOrderRefErrors
    , transferOrderRefs )
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )
import Test.Headless.Unit.TransferApi
    ( evalDebug, mkBuilding, mkItem, mkUnit, minimalDef, newBareLuaBackend
    , storageDef )

-- * Fixture ids

acolyteUid, muleUid, wolfUid, farSideUid, offPageUid ∷ UnitId
acolyteUid  = UnitId 1
muleUid     = UnitId 2
wolfUid     = UnitId 3
farSideUid  = UnitId 4
offPageUid  = UnitId 5

-- | 1x1, Built, capacity 200, anchored twenty tiles east of the
--   acolyte: FAR out of the contract's Chebyshev-1 reach, which is the
--   whole point — every order created against it exercises
--   'ReachDeferred'.
farHold ∷ BuildingId
farHold = BuildingId 7

-- | 1x1, Built, capacity 200, ADJACENT to the acolyte. The control for
--   "reach-deferred creation did not simply stop checking things".
nearHold ∷ BuildingId
nearHold = BuildingId 8

-- | 3x1, Built, capacity 200, occupying (20,10)…(22,10). Its ANCHOR is
--   three tiles from 'farSideUid' at (23,10) while its RECTANGLE is
--   one, so an anchor-distance arrival rule would refuse a commit this
--   one must accept.
wideHold ∷ BuildingId
wideHold = BuildingId 9

pageA, pageB ∷ WorldPageId
pageA = WorldPageId "transfer_order_page"
pageB = WorldPageId "transfer_order_page_b"

-- * Fixture

-- | Reset every manager plus TWO live pages, each with its own order
--   store.
--
--   Geometry (page A unless noted):
--
--     * acolyte    uid 1 at (10, 10), player, capacity 100
--     * mule       uid 2 at (11, 11), player, capacity 250.5
--     * wolf       uid 3 at (11, 11), WILDLIFE (ineligible either role)
--     * far-side   uid 4 at (23, 10), player, capacity 100
--     * off-page   uid 5 at (10, 10) on PAGE B, player, capacity 100,
--                  holding ration #501 — so it can be an ENDPOINT of a
--                  cross-page request, which is what isolates the
--                  carrier-page rule from the carrier-is-an-endpoint one
--     * farHold    bid 7 at (30, 10), 1x1
--     * nearHold   bid 8 at (11, 10), 1x1, adjacent to the acolyte
--     * wideHold   bid 9 at (20, 10), 3x1
--
--   The acolyte's own capacity is deliberately NOT a constraint on what
--   it may hold here: only a DESTINATION's capacity is gated, so a
--   scenario can hand it three hundred kilos to push at a hold that has
--   room for two hundred.
resetOrderWorld ∷ EngineEnv → [ItemInstance] → [ItemInstance] → IO ()
resetOrderWorld env acolyteInv holdStorage = do
    wsA ← emptyWorldState
    wsB ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(pageA, wsA), (pageB, wsB)], wmVisible = [pageA] }
    writeIORef (itemManagerRef env) emptyItemManager
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("acolyte", minimalDef "acolyte" "Acolyte")
            , ("technomule", minimalDef "technomule" "Technomule")
            , ("wolf", minimalDef "wolf" "Wolf") ]
        , umInstances = HM.fromList
            [ (acolyteUid, onPage pageA
                  (mkUnit "acolyte" FactionPlayer (10, 10) 100 acolyteInv []))
            , (muleUid, onPage pageA
                  (mkUnit "technomule" FactionPlayer (11, 11) 250.5 [] []))
            , (wolfUid, onPage pageA
                  (mkUnit "wolf" FactionWildlife (11, 11) 100 [] []))
            , (farSideUid, onPage pageA
                  (mkUnit "acolyte" FactionPlayer (23, 10) 100 [] []))
            , (offPageUid, onPage pageB
                  (mkUnit "acolyte" FactionPlayer (10, 10) 100
                          [mkItem "ration" 501 0.5] [])) ]
        }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList
            [ ("hold", storageDef "hold" "Cargo Hold" (1, 1) 0 200)
            , ("wide", storageDef "wide" "Wide Hold" (3, 1) 0 200) ]
        , bmInstances = HM.fromList
            [ (farHold, onPageB pageA (mkBuilding "hold" (30, 10) (1, 1)
                                                  holdStorage))
            , (nearHold, onPageB pageA (mkBuilding "hold" (11, 10) (1, 1) []))
            , (wideHold, onPageB pageA (mkBuilding "wide" (20, 10) (3, 1) [])) ]
        }
  where
    onPage  p u = u { uiPage = p }
    onPageB p b = b { biPage = p }

-- * Live-state readers

unitLoose ∷ EngineEnv → UnitId → IO [Word64]
unitLoose env uid = do
    um ← readIORef (unitManagerRef env)
    pure $ maybe [] (map iiInstanceId ∘ uiInventory)
                    (HM.lookup uid (umInstances um))

buildingLoose ∷ EngineEnv → BuildingId → IO [Word64]
buildingLoose env bid = do
    bm ← readIORef (buildingManagerRef env)
    pure $ maybe [] (map iiInstanceId ∘ biStorage)
                    (HM.lookup bid (bmInstances bm))

-- | One page's live order store, read straight out of the ref the verbs
--   write — the only way to see what pruning actually left behind, since
--   @unit.getTransferOrders@ resolves the store FROM the acting unit and
--   a destroyed carrier can no longer reach it (#1253's orphan case).
pageOrders ∷ EngineEnv → WorldPageId → IO TransferOrders
pageOrders env pid = do
    wm ← readIORef (worldManagerRef env)
    case lookup pid (wmWorlds wm) of
        Just ws → readIORef (wsTransferOrdersRef ws)
        Nothing → error "fixture: page is not live"

-- * Lua plumbing

-- | Flatteners for the two shapes this spec asserts on. Same discipline
--   as 'Test.Headless.Unit.TransferApi''s @__fmt@: compare formatted
--   strings, never decoded JSON, because an empty Lua array serialises
--   as a JSON object and would make every "nothing happened" assertion
--   ambiguous.
--
--   @__ord@ →
--   @"<id>|<terminal>|<approachRole>|<approach>|<entries>"@, an entry
--   being @<instanceId>:<defName>:<state>[:<reason>][\/<cause>]@ and
--   the approach being @<kind>#<id>@<x>,<y> <w>x<h>@ or @none@.
--   @__ordres@ prefixes a create/commit result with its order id.
orderFormatterLua ∷ Text
orderFormatterLua = T.concat
    [ "_G.__ord = function(o) "
    , "  if o == nil then return 'nil' end; "
    , "  local parts = {}; "
    , "  for i, e in ipairs(o.entries or {}) do "
    , "    parts[i] = tostring(e.instanceId) .. ':' .. tostring(e.defName) "
    , "      .. ':' .. tostring(e.state) "
    , "      .. (e.reason and (':' .. e.reason) or '') "
    , "      .. (e.cause and ('/' .. e.cause) or ''); "
    , "  end; "
    , "  local a = o.approach; "
    , "  return tostring(o.id) .. '|' .. tostring(o.terminal) "
    , "    .. '|' .. tostring(o.approachRole) "
    , "    .. '|' .. (a and (tostring(a.kind) .. '#' .. tostring(a.id) "
    , "                      .. '@' .. tostring(a.gridX) .. ',' "
    , "                      .. tostring(a.gridY) .. ' ' "
    , "                      .. tostring(a.tileW) .. 'x' "
    , "                      .. tostring(a.tileH)) or 'none') "
    , "    .. '|' .. table.concat(parts, ' '); "
    , "end; "
    , "_G.__ords = function(l) "
    , "  if l == nil then return 'nil' end; "
    , "  local rows = {}; "
    , "  for i, o in ipairs(l) do rows[i] = _G.__ord(o); end; "
    , "  return tostring(#l) .. ';' .. table.concat(rows, ';'); "
    , "end; "
    , "_G.__ordres = function(r) "
    , "  if r == nil then return 'nil' end; "
    , "  return (r.orderId and ('#' .. tostring(r.orderId)) or '#none') "
    , "    .. ' ' .. _G.__fmt(r); "
    , "end; "
    , "return 'ok'"
    ]

backend ∷ EngineEnv → IO LuaBackendState
backend env = do
    ls ← newBareLuaBackend env
    _  ← evalDebug ls orderFormatterLua
    pure ls

q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | @{ source = …, destination = …, items = { … } }@.
req ∷ Text → Int → Text → Int → Text → Text
req srcKind srcId dstKind dstId items = T.concat
    [ "{ source = { kind = '", srcKind, "', id = ", tshow srcId, " }, "
    , "destination = { kind = '", dstKind, "', id = ", tshow dstId, " }, "
    , "items = { ", items, " } }" ]

itemLit ∷ Int → Text → Text
itemLit iid defName =
    "{ instanceId = " <> tshow iid <> ", defName = '" <> defName <> "' }"

itemLits ∷ [Int] → Text → Text
itemLits iids defName = T.intercalate ", " (map (`itemLit` defName) iids)

create ∷ LuaBackendState → Int → Text → IO Text
create ls uid request = evalDebug ls $ T.concat
    [ "return _G.__ordres(unit.createTransferOrder(", tshow uid, ", "
    , request, "))" ]

orders ∷ LuaBackendState → Int → IO Text
orders ls uid =
    evalDebug ls ("return _G.__ords(unit.getTransferOrders(" <> tshow uid <> "))")

advance ∷ LuaBackendState → Int → Int → Text → IO Text
advance ls uid oid st = evalDebug ls $ T.concat
    [ "return tostring(unit.advanceTransferOrder(", tshow uid, ", "
    , tshow oid, ", '", st, "'))" ]

commitOrder ∷ LuaBackendState → Int → Int → IO Text
commitOrder ls uid oid = evalDebug ls $ T.concat
    [ "return _G.__fmt(unit.commitTransferOrder(", tshow uid, ", "
    , tshow oid, "))" ]

-- | Create, then walk the order all the way to the arrival gate without
--   committing — the state every "what happens at arrival" scenario
--   starts from.
readyOrder ∷ LuaBackendState → Int → Text → IO Int
readyOrder ls uid request = do
    _ ← create ls uid request
    _ ← advance ls uid 1 "in_transit"
    _ ← advance ls uid 1 "ready_to_commit"
    pure 1

spec ∷ SpecWith EngineEnv
spec = describe "Unit transfer Lua API (orders, #1247)" $ do

    describe "creation at a distance" $ do
        it "queues an order the immediate contract refuses on range" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            -- The SAME request, through the immediate verb, is still
            -- out of range: creation relaxed adjacency for orders only,
            -- and unit.checkTransfer's public behavior is untouched.
            immediate ← evalDebug ls
                ("return _G.__fmt(unit.checkTransfer("
                 <> req "unit" 1 "building" 7 (itemLit 101 "ration") <> "))")
            immediate `shouldBe` q "none|101:ration:failed:out_of_range"
            r ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            r `shouldBe` q "#1 all|101:ration:queued"

        it "still refuses a CROSS-PAGE order — a carrier cannot walk \
           \to another world page" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            r ← create ls 1 (req "unit" 1 "unit" 5 (itemLit 101 "ration"))
            r `shouldBe` q "#none none|101:ration:failed:out_of_range"
            orders ls 1 `shouldReturn` q "0;"

        it "refuses a request whose endpoints agree with each other but \
           \not with the ACTING unit's page" $ \env → do
            -- The carrier IS an endpoint (so this is purely about the
            -- page), and it stands on page B while its counterpart is on
            -- page A. The order would be stored in page B's store — the
            -- store this verb selects FROM THE ACTOR — where
            -- World.Save.Integrity scopes the acting unit and both
            -- endpoints as BLOCKING wrong-page errors. Accepting it
            -- would leave a session whose every later save fails, so
            -- creation must refuse rather than store it.
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            r ← create ls 5 (req "unit" 5 "building" 8 (itemLit 501 "ration"))
            r `shouldBe` q "#none none|501:ration:failed:out_of_range"
            -- Neither page's store gained anything: not the actor's,
            -- which is the one that would have been written…
            orders ls 5 `shouldReturn` q "0;"
            -- …nor the endpoints' own.
            orders ls 1 `shouldReturn` q "0;"
            -- And the item never moved.
            unitLoose env offPageUid `shouldReturn` [501]

        it "accepts the same shape from a carrier on the endpoints' own \
           \page, so the refusal above is the page and nothing else" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            r ← create ls 1 (req "unit" 1 "building" 8 (itemLit 101 "ration"))
            r `shouldBe` q "#1 all|101:ration:queued"

    describe "a queued executable order always has somewhere to walk" $ do
        it "refuses a unit↔building pair carried by a BYSTANDER" $ \env → do
            -- uid 4 is on the right page and perfectly healthy, but it
            -- is neither endpoint — so approachEndpoint would answer
            -- nothing, the job would skip the order on every tick, and
            -- nothing in this slice could cancel it. It would sit
            -- pending forever and ride every save. Refuse at creation,
            -- which is the last point where that is cheap.
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            r ← create ls 4 (req "unit" 1 "building" 8 (itemLit 101 "ration"))
            r `shouldBe` q "nil"
            orders ls 4 `shouldReturn` q "0;"
            orders ls 1 `shouldReturn` q "0;"
            unitLoose env acolyteUid `shouldReturn` [101]

        it "refuses a unit↔unit pair carried by a BYSTANDER" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            r ← create ls 4 (req "unit" 1 "unit" 2 (itemLit 101 "ration"))
            r `shouldBe` q "nil"
            orders ls 4 `shouldReturn` q "0;"

        it "still accepts D-10's building→building pair, whose carrier is \
           \neither end BY DESIGN" $ \env → do
            -- The one shape allowed to store an order with no approach:
            -- it is deliberately inert until a later slice ferries
            -- between two buildings (S1), not an accident.
            resetOrderWorld env [] [mkItem "ration" 201 0.5]
            ls ← backend env
            r ← create ls 1 (req "building" 7 "building" 8 (itemLit 201 "ration"))
            r `shouldBe` q "#1 all|201:ration:queued"
            o ← orders ls 1
            o `shouldBe` q "1;1|false|nil|none|201:ration:queued"

        it "accepts a carrier that is the DESTINATION, not only the \
           \source" $ \env → do
            resetOrderWorld env [] [mkItem "ration" 201 0.5]
            ls ← backend env
            r ← create ls 1 (req "building" 7 "unit" 1 (itemLit 201 "ration"))
            r `shouldBe` q "#1 all|201:ration:queued"

        it "still applies every non-range precondition at a distance" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            -- Wildlife destination: ineligible, not merely far away.
            r1 ← create ls 1 (req "unit" 1 "unit" 3 (itemLit 101 "ration"))
            r1 `shouldBe` q "#none none|101:ration:failed:receiver_ineligible"
            -- An instance the source does not hold.
            r2 ← create ls 1 (req "unit" 1 "building" 7 (itemLit 999 "ration"))
            r2 `shouldBe` q "#none none|999:ration:failed:instance_missing"

        it "reaches the CAPACITY verdict at a distance, which is the \
           \point of deferring range" $ \env → do
            -- Twelve 25 kg rations at a 200 kg hold: room for exactly
            -- eight. Under the immediate rule every one of them would
            -- report out_of_range instead, and the create-time gate
            -- that refuses a doomed trip could never fire (D-1).
            resetOrderWorld env [mkItem "ration" (100 + i) 25 | i ← [1 .. 12]] []
            ls ← backend env
            r ← create ls 1 (req "unit" 1 "building" 7
                                 (itemLits [101 .. 112] "ration"))
            let expected = "#1 partial|" <> T.unwords
                    ([tshow i <> ":ration:queued" | i ← [101 .. 108]]
                     ⧺ [tshow i <> ":ration:failed:receiver_full"
                       | i ← [109 .. 112]])
            r `shouldBe` q expected

    describe "refusal means nothing was queued" $ do
        it "a WHOLE-REQUEST rejection stores no order" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            empty' ← create ls 1 (req "unit" 1 "building" 7 "")
            empty' `shouldBe` q "#none rejected:empty_batch|0"
            dup ← create ls 1 (req "unit" 1 "building" 7
                                   (itemLits [101, 101] "ration"))
            dup `shouldBe` q "#none rejected:duplicate_instance|0"
            orders ls 1 `shouldReturn` q "0;"

        it "a batch where NOTHING fits stores no order either" $ \env → do
            -- One 500 kg crate at a 200 kg hold: the batch parses, every
            -- entry refuses, and there is nothing to make the trip for.
            resetOrderWorld env [mkItem "crate" 101 500] []
            ls ← backend env
            r ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "crate"))
            r `shouldBe` q "#none none|101:crate:failed:receiver_full"
            orders ls 1 `shouldReturn` q "0;"

        it "a PARTIAL batch is an acceptance: it queues, keeping its \
           \create-time refusals as terminal entries" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5, mkItem "crate" 102 500]
                                []
            ls ← backend env
            r ← create ls 1 (req "unit" 1 "building" 7
                                 (itemLit 101 "ration" <> ", "
                                  <> itemLit 102 "crate"))
            r `shouldBe` q "#1 partial|101:ration:queued 102:crate:failed:receiver_full"

    describe "which endpoint the carrier approaches" $ do
        it "reports the counterpart, with its live footprint" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 9 (itemLit 101 "ration"))
            o ← orders ls 1
            o `shouldBe` q "1;1|false|destination|building#9@20,10 3x1|101:ration:queued"

        it "reports the SOURCE when the carrier is the receiving end" $ \env → do
            resetOrderWorld env [] [mkItem "ration" 201 0.5]
            ls ← backend env
            _ ← create ls 1 (req "building" 7 "unit" 1 (itemLit 201 "ration"))
            o ← orders ls 1
            o `shouldBe` q "1;1|false|source|building#7@30,10 1x1|201:ration:queued"

        it "claims NO approach for a building→building order (D-10)" $ \env → do
            resetOrderWorld env [] [mkItem "ration" 201 0.5]
            ls ← backend env
            -- Storable and perfectly valid — the acting unit is simply
            -- neither end, so this executor leaves it alone.
            r ← create ls 1 (req "building" 7 "building" 8 (itemLit 201 "ration"))
            r `shouldBe` q "#1 all|201:ration:queued"
            o ← orders ls 1
            o `shouldBe` q "1;1|false|nil|none|201:ration:queued"

        it "drops the approach, keeping the role, when the counterpart \
           \VANISHES" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            modifyIORef' (buildingManagerRef env) $ \bm →
                bm { bmInstances = HM.delete farHold (bmInstances bm) }
            o ← orders ls 1
            -- approachRole survives (it is the batch's own shape) while
            -- approach does not: exactly the pair the unit job needs to
            -- retire this as receiver_missing rather than source_missing.
            o `shouldBe` q "1;1|false|destination|none|101:ration:queued"

    describe "the stored lifecycle" $ do
        it "advances queued → in_transit → ready_to_commit → completed, \
           \leaving create-time failures untouched throughout" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5, mkItem "crate" 102 500]
                                []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7
                                 (itemLit 101 "ration" <> ", "
                                  <> itemLit 102 "crate"))
            entries ← orders ls 1
            entries `shouldBe`
                q "1;1|false|destination|building#7@30,10 1x1|\
                  \101:ration:queued 102:crate:failed:receiver_full"
            advance ls 1 1 "in_transit" `shouldReturn` q "true"
            e2 ← orders ls 1
            e2 `shouldBe`
                q "1;1|false|destination|building#7@30,10 1x1|\
                  \101:ration:in_transit 102:crate:failed:receiver_full"
            advance ls 1 1 "ready_to_commit" `shouldReturn` q "true"
            e3 ← orders ls 1
            e3 `shouldBe`
                q "1;1|false|destination|building#7@30,10 1x1|\
                  \101:ration:ready_to_commit 102:crate:failed:receiver_full"
            -- Walk it into reach, then commit.
            modifyIORef' (unitManagerRef env) $ \um → um
                { umInstances = HM.adjust (\u → u { uiGridX = 29, uiGridY = 10 })
                                          acolyteUid (umInstances um) }
            c ← commitOrder ls 1 1
            c `shouldBe` q "partial|101:ration:completed \
                           \102:crate:failed:receiver_full"
            e4 ← orders ls 1
            e4 `shouldBe`
                q "1;1|true|destination|building#7@30,10 1x1|\
                  \101:ration:completed 102:crate:failed:receiver_full"
            unitLoose env acolyteUid `shouldReturn` [102]
            buildingLoose env farHold `shouldReturn` [101]

        it "refuses an unknown transition and moves nothing" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 8 (itemLit 101 "ration"))
            r ← advance ls 1 1 "completed"
            r `shouldBe` q "nil"
            o ← orders ls 1
            o `shouldBe` q "1;1|false|destination|building#8@11,10 1x1|\
                           \101:ration:queued"

        it "only the ACTING unit may advance or commit its order" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 8 (itemLit 101 "ration"))
            -- Same page, same id space, different carrier.
            advance ls 4 1 "in_transit" `shouldReturn` q "false"
            c ← commitOrder ls 4 1
            c `shouldBe` q "nil"
            unitLoose env acolyteUid `shouldReturn` [101]

    describe "the arrival commit" $ do
        it "submits ONLY ready entries — a queued one moves nothing" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 8 (itemLit 101 "ration"))
            c ← commitOrder ls 1 1
            c `shouldBe` q "none|101:ration:queued"
            unitLoose env acolyteUid `shouldReturn` [101]
            buildingLoose env nearHold `shouldReturn` []

        it "commits EXACTLY ONCE: a second call moves nothing and \
           \reports the same terminal batch" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            oid ← readyOrder ls 1 (req "unit" 1 "building" 8
                                       (itemLit 101 "ration"))
            c1 ← commitOrder ls 1 oid
            c1 `shouldBe` q "all|101:ration:completed"
            buildingLoose env nearHold `shouldReturn` [101]
            c2 ← commitOrder ls 1 oid
            c2 `shouldBe` q "all|101:ration:completed"
            -- The item is in the hold ONCE, and the acolyte did not get
            -- a second copy of it from anywhere.
            buildingLoose env nearHold `shouldReturn` [101]
            unitLoose env acolyteUid `shouldReturn` []

        it "measures arrival against the FOOTPRINT, not the anchor" $ \env → do
            -- uid 4 stands at (23,10). wideHold occupies (20..22, 10):
            -- three tiles from its anchor, ONE from its rectangle.
            resetOrderWorld env [] []
            modifyIORef' (unitManagerRef env) $ \um → um
                { umInstances = HM.adjust
                    (\u → u { uiInventory = [mkItem "ration" 301 0.5] })
                    farSideUid (umInstances um) }
            ls ← backend env
            oid ← readyOrder ls 4 (req "unit" 4 "building" 9
                                       (itemLit 301 "ration"))
            c ← commitOrder ls 4 oid
            c `shouldBe` q "all|301:ration:completed"
            buildingLoose env wideHold `shouldReturn` [301]

        it "records an instance that vanished during the walk as \
           \became_stale with its cause" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5, mkItem "ration" 102 0.5]
                                []
            ls ← backend env
            oid ← readyOrder ls 1 (req "unit" 1 "building" 8
                                       (itemLits [101, 102] "ration"))
            -- Instance 101 leaves the carrier's hands mid-walk; 102 is
            -- untouched and must still land.
            modifyIORef' (unitManagerRef env) $ \um → um
                { umInstances = HM.adjust
                    (\u → u { uiInventory = filter ((≢ 101) ∘ iiInstanceId)
                                                   (uiInventory u) })
                    acolyteUid (umInstances um) }
            c ← commitOrder ls 1 oid
            c `shouldBe` q "partial|101:ration:failed:became_stale/instance_missing \
                           \102:ration:completed"
            buildingLoose env nearHold `shouldReturn` [102]

        it "re-gates CAPACITY at the moment of truth, as became_stale" $ \env → do
            -- The order fits when it is made; the hold fills during the
            -- walk. This is the arrival capacity gate (requirement 2):
            -- the commit's own atomic re-check, reported with its cause.
            resetOrderWorld env [mkItem "ration" 101 100] []
            ls ← backend env
            oid ← readyOrder ls 1 (req "unit" 1 "building" 8
                                       (itemLit 101 "ration"))
            modifyIORef' (buildingManagerRef env) $ \bm → bm
                { bmInstances = HM.adjust
                    (\b → b { biStorage = [mkItem "ballast" 900 190] })
                    nearHold (bmInstances bm) }
            c ← commitOrder ls 1 oid
            c `shouldBe` q "none|101:ration:failed:became_stale/receiver_full"
            unitLoose env acolyteUid `shouldReturn` [101]

        it "refuses to commit out of range, so a stalled carrier cannot \
           \reach across the map" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            oid ← readyOrder ls 1 (req "unit" 1 "building" 7
                                       (itemLit 101 "ration"))
            c ← commitOrder ls 1 oid
            c `shouldBe` q "none|101:ration:failed:became_stale/out_of_range"
            unitLoose env acolyteUid `shouldReturn` [101]
            buildingLoose env farHold `shouldReturn` []

    describe "self-termination" $ do
        it "retires PENDING entries with the given reason, leaving \
           \terminal ones alone and moving nothing" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5, mkItem "crate" 102 500]
                                []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7
                                 (itemLit 101 "ration" <> ", "
                                  <> itemLit 102 "crate"))
            _ ← advance ls 1 1 "in_transit"
            r ← evalDebug ls
                "return tostring(unit.failTransferOrder(1, 1, 'out_of_range'))"
            r `shouldBe` q "true"
            o ← orders ls 1
            o `shouldBe`
                q "1;1|true|destination|building#7@30,10 1x1|\
                  \101:ration:failed:out_of_range \
                  \102:crate:failed:receiver_full"
            unitLoose env acolyteUid `shouldReturn` [101, 102]

        it "records a vanished counterpart as became_stale with the \
           \side that went missing" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            r ← evalDebug ls
                "return tostring(unit.failTransferOrder(1, 1, 'became_stale', \
                \'receiver_missing'))"
            r `shouldBe` q "true"
            o ← orders ls 1
            o `shouldBe` q "1;1|true|destination|building#7@30,10 1x1|\
                           \101:ration:failed:became_stale/receiver_missing"

        it "refuses a reason outside the contract's vocabulary rather \
           \than writing a guess into durable state" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            r1 ← evalDebug ls
                "return tostring(unit.failTransferOrder(1, 1, 'gave_up'))"
            r1 `shouldBe` q "nil"
            r2 ← evalDebug ls
                "return tostring(unit.failTransferOrder(1, 1, 'out_of_range', \
                \'nowhere'))"
            r2 `shouldBe` q "nil"
            o ← orders ls 1
            o `shouldBe` q "1;1|false|destination|building#7@30,10 1x1|\
                           \101:ration:queued"

    describe "cancellation and terminal pruning (#1253)" $ do
        it "cancels PENDING entries only, leaves terminal ones alone, and \
           \moves nothing" $ \env → do
            -- The crate cannot fit (500 kg into a 200 kg hold), so it is
            -- already terminal when the order is stored. A cancel that
            -- overwrote terminal entries would rewrite that recorded
            -- refusal as an abandonment nobody chose — and after a
            -- PARTIAL commit the same bug would claim delivered items
            -- were abandoned.
            resetOrderWorld env [mkItem "ration" 101 0.5, mkItem "crate" 102 500]
                                []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7
                                 (itemLit 101 "ration" <> ", "
                                  <> itemLit 102 "crate"))
            _ ← advance ls 1 1 "in_transit"
            r ← evalDebug ls
                "return tostring(unit.cancelTransferOrder(1, 1))"
            r `shouldBe` q "true"
            o ← orders ls 1
            o `shouldBe`
                q "1;1|true|destination|building#7@30,10 1x1|\
                  \101:ration:cancelled \
                  \102:crate:failed:receiver_full"
            unitLoose env acolyteUid `shouldReturn` [101, 102]
            buildingLoose env farHold `shouldReturn` []

        it "is scoped to the ACTING unit, so one unit cannot cancel \
           \another's order" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            -- Same page, same live store, same id — only the acting unit
            -- differs, which is the whole check.
            wrong ← evalDebug ls
                "return tostring(unit.cancelTransferOrder(4, 1))"
            wrong `shouldBe` q "false"
            missing ← evalDebug ls
                "return tostring(unit.cancelTransferOrder(1, 99))"
            missing `shouldBe` q "false"
            o ← orders ls 1
            o `shouldBe` q "1;1|false|destination|building#7@30,10 1x1|\
                           \101:ration:queued"

        it "refuses to prune a LIVE order — abandoning queued work with \
           \no cancelled entry and no event is the one thing pruning \
           \must never do" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            r1 ← evalDebug ls "return tostring(unit.pruneTransferOrder(1, 1))"
            r1 `shouldBe` q "false"
            -- Not even part-way: a batch is terminal only when EVERY
            -- entry is, and this one still has a live entry after the
            -- walk began.
            _ ← advance ls 1 1 "in_transit"
            r2 ← evalDebug ls "return tostring(unit.pruneTransferOrder(1, 1))"
            r2 `shouldBe` q "false"
            o ← orders ls 1
            o `shouldBe` q "1;1|false|destination|building#7@30,10 1x1|\
                           \101:ration:in_transit"

        it "prunes a cancelled order, and a second prune is inert rather \
           \than an error" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            _ ← evalDebug ls "return tostring(unit.cancelTransferOrder(1, 1))"
            first ← evalDebug ls "return tostring(unit.pruneTransferOrder(1, 1))"
            first `shouldBe` q "true"
            orders ls 1 `shouldReturn` q "0;"
            -- Idempotence is what lets the executor prune on the tick
            -- that terminalised the order without recording whether it
            -- has already done so.
            again ← evalDebug ls "return tostring(unit.pruneTransferOrder(1, 1))"
            again `shouldBe` q "false"
            orders ls 1 `shouldReturn` q "0;"

        it "prunes a COMPLETED order, and the item really did move" $ \env → do
            -- The ordinary happy path's cleanup: pruning is not a
            -- failure-only mechanism, and a completed haul must not ride
            -- the next save either.
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            oid ← readyOrder ls 1 (req "unit" 1 "building" 8
                                       (itemLit 101 "ration"))
            c ← commitOrder ls 1 oid
            c `shouldBe` q "all|101:ration:completed"
            p ← evalDebug ls "return tostring(unit.pruneTransferOrder(1, 1))"
            p `shouldBe` q "true"
            orders ls 1 `shouldReturn` q "0;"
            unitLoose env acolyteUid `shouldReturn` []
            buildingLoose env nearHold `shouldReturn` [101]

        it "refuses to prune another unit's terminal order" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            _ ← evalDebug ls "return tostring(unit.cancelTransferOrder(1, 1))"
            wrong ← evalDebug ls "return tostring(unit.pruneTransferOrder(4, 1))"
            wrong `shouldBe` q "false"
            o ← orders ls 1
            o `shouldBe` q "1;1|true|destination|building#7@30,10 1x1|\
                           \101:ration:cancelled"

        it "leaves the integrity sweep nothing to report once a terminal \
           \order naming a demolished endpoint is pruned" $ \env → do
            -- Requirement 5's "the integrity sweep stays quiet", against
            -- the real graph: the destination is torn down mid-order, so
            -- every reference the order carries becomes a tolerated
            -- dangling diagnostic on every later save and load — until
            -- the order is gone.
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            modifyIORef' (buildingManagerRef env) $ \bm → bm
                { bmInstances = HM.delete farHold (bmInstances bm) }
            -- Only the acolyte and its ration resolve; the hold is gone.
            let entities = PageEntities
                    { peUnits     = HS.singleton acolyteUid
                    , peBuildings = HS.empty
                    , peItems     = HS.singleton 101
                    -- Not on the ground: this fixture's ration is in
                    -- the acolyte's inventory. Only #917's significant
                    -- provenance reads this set, and no order carries
                    -- one, so it is empty here.
                    , peGroundItems = HS.empty }
            before ← pageOrders env pageA
            -- Four: the acting unit, both endpoints, and the one entry.
            length (transferOrderRefs pageA before) `shouldBe` 4
            map ieRefValue (danglingOrderRefErrors pageA entities before)
                `shouldBe` ["7"]
            _ ← evalDebug ls
                "return tostring(unit.failTransferOrder(1, 1, 'became_stale', \
                \'receiver_missing'))"
            _ ← evalDebug ls "return tostring(unit.pruneTransferOrder(1, 1))"
            after ← pageOrders env pageA
            transferOrderRefs pageA after `shouldBe` []
            danglingOrderRefErrors pageA entities after `shouldBe` []

        it "destroying the CARRIER retires its orders, and only its own" $ \env → do
            -- The one retirement the executor can never perform: a
            -- destroyed unit never ticks again, so without this its
            -- orders would sit pending in the store forever and report a
            -- dangling acting unit on every save.
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            -- A second order carried by a DIFFERENT unit, so "retired
            -- everything" cannot pass by accident. Its source is the
            -- doomed acolyte, which makes it the deliberate control for
            -- the troUnit-only rule: a unit that is merely an ENDPOINT of
            -- somebody else's order is not orphaned by its own death, and
            -- that order's live carrier retires it on its next tick with
            -- the reason recorded.
            _ ← create ls 4 (req "unit" 1 "unit" 4 (itemLit 101 "ration"))
            before ← pageOrders env pageA
            HM.size (trosOrders before) `shouldBe` 2
            utsRef ← newIORef emptyUnitThreadState
            handleUnitDestroyCommand env utsRef acolyteUid
            after ← pageOrders env pageA
            map (unTransferOrderId ∘ fst) (HM.toList (trosOrders after))
                `shouldBe` [2]
            -- The allocator is NOT rewound, so a retired id can never be
            -- handed to a later order.
            trosNextId after `shouldBe` 3
            -- It survives with its own approach now UNRESOLVED, which is
            -- precisely the signal its live carrier retires it on.
            orders ls 4 `shouldReturn`
                q "1;2|false|source|none|101:ration:queued"

        it "KILLING the carrier retires its orders too — a dead unit \
           \never ticks again" $ \env → do
            -- Review round 1: ordinary gameplay death is UnitKill, not
            -- UnitDestroy. It leaves the instance in place, so every
            -- reference still resolves and the integrity sweep stays
            -- quiet — but scripts/unit_ai.lua short-circuits a `dead`
            -- pose before any action scores ("Dead pose: terminal. No
            -- AI, no resources, no revival"), so the executor can never
            -- reach the terminal transition that would prune the order.
            -- Without this it sits pending forever and rides every save.
            resetOrderWorld env [mkItem "ration" 101 0.5] []
            ls ← backend env
            _ ← create ls 1 (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            _ ← create ls 4 (req "unit" 1 "unit" 4 (itemLit 101 "ration"))
            utsRef ← newIORef emptyUnitThreadState
            handleUnitKillCommand env utsRef acolyteUid
            after ← pageOrders env pageA
            map (unTransferOrderId ∘ fst) (HM.toList (trosOrders after))
                `shouldBe` [2]
            -- The corpse is still a live instance, unlike the destroy
            -- case — which is exactly why nothing else would have
            -- noticed the order was dead.
            um ← readIORef (unitManagerRef env)
            HM.member acolyteUid (umInstances um) `shouldBe` True
            orders ls 1 `shouldReturn` q "0;"

    describe "order identity" $
        it "ids are allocated per page and every verb is scoped to the \
           \acting unit's own page" $ \env → do
            resetOrderWorld env [mkItem "ration" 101 0.5, mkItem "ration" 102 0.5]
                                []
            ls ← backend env
            a ← create ls 1 (req "unit" 1 "building" 8 (itemLit 101 "ration"))
            b ← create ls 1 (req "unit" 1 "building" 8 (itemLit 102 "ration"))
            a `shouldBe` q "#1 all|101:ration:queued"
            b `shouldBe` q "#2 all|102:ration:queued"
            -- Page B's own store is untouched and starts at 1 in its
            -- own right, which is what makes an id meaningless without
            -- a page and is why every verb takes the acting unit.
            orders ls 5 `shouldReturn` q "0;"
