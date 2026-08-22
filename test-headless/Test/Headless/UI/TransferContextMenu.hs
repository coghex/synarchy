-- | The "Transfer" context-menu entry (#1014, epic #1013 phase B1):
--   the first player-facing entry point into A1's (#1000) unit ->
--   container transfer contract.
--
--   Same bare-Lua-backend technique as 'Test.Headless.World.SelectTileZ'
--   ("UI wiring" block) and 'Test.Headless.UI.InputOwnership': a real
--   Lua backend with the full Lua API registered (so
--   'scripts.init_context_menu' and 'scripts.transfer_session' pull in
--   exactly the real production code any caller's @require@ would),
--   but no live world/units/buildings — the handful of world-facing
--   globals a scenario needs ('building.hitTestAt',
--   'unit.transferEndpointInfo', 'unit.getSelected', ...) are stubbed
--   per test, mirroring how SelectTileZ stubs 'world.pickTile'. This
--   proves the WIRING (menu construction, session creation) rather
--   than re-deriving A1's own eligibility policy, which
--   'Test.Headless.Unit.Transfer' already covers directly.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Transfer context menu"'@.
module Test.Headless.UI.TransferContextMenu (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket)
import Data.Aeson (FromJSON(..), decode, withObject, (.:))
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.Page.Types (WorldPageId(..))

spec ∷ SpecWith EngineEnv
spec = describe "Transfer context menu" $ do
    describe "building receiver (scripts/init_context_menu.lua tryBuildingMenu)" $ do
        it "one selected acolyte + built storage building -> Transfer appears" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 501 "built" 200 [] <> selectedStub [7]
                    <> endpointInfoStub "building" 501 True "Cargo Hold")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Transfer"
            labels `shouldSatisfy` T.isInfixOf "Contents"

        it "an under-construction storage building -> Transfer does not appear (no menu at all)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 502 "under_construction" 200 [] <> selectedStub [7]
                    <> endpointInfoStub "building" 502 False "")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "false"

        it "a Built station with zero storage capacity -> Transfer does not appear (Bills still does)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 503 "built" 0 ["smelt"] <> selectedStub [7]
                    <> endpointInfoStub "building" 503 False "")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Bills"
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

        it "no selected source -> Transfer does not appear" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 504 "built" 200 [] <> selectedStub []
                    <> endpointInfoStub "building" 504 True "Cargo Hold")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Contents"
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

        -- #1239 (D-8, superseding #1014's rule via D-11): a multi-unit
        -- selection is no longer refused -- the NEAREST eligible unit
        -- becomes the source. The farther unit is listed FIRST and
        -- carries the LOWER uid, so neither "first in selection order"
        -- nor "lowest uid" could produce the right answer by accident.
        it "multiple selected sources -> Transfer appears and the NEAREST is the source" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 505 "built" 200 [] <> selectedStub [7, 8]
                    <> unitInfoStub [(7, (10, 25)), (8, (10, 22))]
                    <> endpointInfoStubAt "building" 505 True "Cargo Hold" (10, 20))
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Contents"
            labels `shouldSatisfy` T.isInfixOf "Transfer"
            -- Requirement 4 / review: assert the SESSION's source, not
            -- just the resolver, by driving the menu's own callback.
            invoke ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            invoke `shouldNotSatisfy` isLuaError
            sess ← decodeOr =<< evalDebug ls "return require('scripts.transfer_session').get()"
            spSourceId sess `shouldBe` 8

        it "an exact distance tie resolves to the LOWEST uid, not selection order" $ \env → do
            -- The higher uid is listed FIRST and both sit at squared
            -- distance 4, so a "first candidate wins" comparison (the
            -- '#920 Pick up' precedent's own gap) would answer 9.
            -- unit.getSelected converts a HashSet, so selection order
            -- is not contractual and this tiebreak is what stops two
            -- equidistant acolytes from racing (D-8).
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 516 "built" 200 [] <> selectedStub [9, 4]
                    <> unitInfoStub [(9, (10, 22)), (4, (10, 18))]
                    <> endpointInfoStubAt "building" 516 True "Cargo Hold" (10, 20))
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            _ ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            sess ← decodeOr =<< evalDebug ls "return require('scripts.transfer_session').get()"
            spSourceId sess `shouldBe` 4

            -- ...and the OTHER input order answers identically, which
            -- is the whole point of not trusting HashSet iteration.
            run ls (selectedStub [4, 9])
            _ ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            _ ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            sess2 ← decodeOr =<< evalDebug ls "return require('scripts.transfer_session').get()"
            spSourceId sess2 `shouldBe` 4

        it "a non-commandable selected unit is skipped even when it is the CLOSEST" $ \env → do
            -- Filtering must precede ranking: uid 5 is adjacent to the
            -- endpoint but wildlife, uid 6 is five tiles out but
            -- player-commandable. Ranking first would answer 5.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 517 "built" 200 [] <> selectedStub [5, 6]
                    <> unitInfoStub [(5, (10, 21)), (6, (10, 25))]
                    <> unitFactionStub [(5, "wildlife"), (6, "player")]
                    <> endpointInfoStubAt "building" 517 True "Cargo Hold" (10, 20))
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Transfer"
            _ ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            sess ← decodeOr =<< evalDebug ls "return require('scripts.transfer_session').get()"
            spSourceId sess `shouldBe` 6

        it "a selection of ONLY non-commandable units still omits Transfer (never a disabled row)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 518 "built" 200 [] <> selectedStub [5, 6]
                    <> unitInfoStub [(5, (10, 21)), (6, (10, 25))]
                    <> unitFactionStub [(5, "wildlife"), (6, "wildlife")]
                    <> endpointInfoStubAt "building" 518 True "Cargo Hold" (10, 20))
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Contents"
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

        it "a selected uid whose live position or faction vanished is skipped, not fatal" $ \env → do
            -- unit.getSelected filters live ids, but a Lua read a
            -- moment later can still observe a deletion. uid 3 would
            -- have been nearest but has no live info; uid 11 has info
            -- but no live faction; uid 12 survives and must be chosen
            -- without any arithmetic/indexing error.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 519 "built" 200 [] <> selectedStub [3, 11, 12]
                    <> unitInfoStub [(11, (10, 21)), (12, (10, 26))]
                    <> unitFactionStub [(3, "player"), (12, "player")]
                    <> endpointInfoStubAt "building" 519 True "Cargo Hold" (10, 20))
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldNotSatisfy` isLuaError
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Transfer"
            _ ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            sess ← decodeOr =<< evalDebug ls "return require('scripts.transfer_session').get()"
            spSourceId sess `shouldBe` 12

        it "every selected uid vanishing leaves zero candidates -> Transfer omitted, no error" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 520 "built" 200 [] <> selectedStub [3, 11]
                    <> unitInfoStub [] <> unitFactionStub []
                    <> endpointInfoStubAt "building" 520 True "Cargo Hold" (10, 20))
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldNotSatisfy` isLuaError
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Contents"
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

        it "a FRACTIONAL unit position is floored to its tile, matching the endpoint's own frame" $ \env → do
            -- Review round 1: unit.getInfo reports the CONTINUOUS
            -- position (uiGridX is a Float), while transferEndpointInfo
            -- reports a whole tile it derived with FLOOR, and
            -- world.localizeTile ROUNDS its inputs. Ranking raw
            -- positions therefore mixed two frames.
            --
            -- uid 12 stands at x = 10.6 -- inside tile 10, which is the
            -- destination's own tile, so it must win at distance 0.
            -- uid 3 stands at x = 9.6, genuinely one tile away. Under
            -- the rounding bug the pair inverts (12 -> tile 11 at
            -- distance 1, 3 -> tile 10 at distance 0) and 3 wins; 3 is
            -- also the LOWER uid, so the tiebreak cannot rescue the
            -- right answer either.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 522 "built" 200 [] <> selectedStub [12, 3]
                    <> unitInfoStubF [(12, (10.6, 20.0)), (3, (9.6, 20.0))]
                    <> endpointInfoStubAt "building" 522 True "Cargo Hold" (10, 20))
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Transfer"
            _ ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            sess ← decodeOr =<< evalDebug ls "return require('scripts.transfer_session').get()"
            spSourceId sess `shouldBe` 12

        it "distance is measured in the TARGET's local alias frame, so a seam-adjacent unit wins" $ \env → do
            -- Requirement 3 (#1175's selection-gate rule), kept
            -- self-contained: a --match run initializes no world, and
            -- the real world.localizeTile is identity with none
            -- visible, so this installs a faithful seam-aware stub for
            -- a wrapping world (alias step 128 tiles) instead.
            --
            -- uid 20 sits at canonical (131, -126) -- one alias step
            -- away from (3, 2), i.e. physically ONE tile from the
            -- endpoint at (2, 2). uid 10 sits at (7, 2), genuinely
            -- five tiles out. In raw canonical coords uid 20 measures a
            -- whole world away, so a seam-blind ranking answers 10 --
            -- and 10 is also the lower uid, so the tiebreak cannot
            -- rescue it either.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (seamLocalizeStub 128)
            run ls (buildingStub 521 "built" 200 [] <> selectedStub [10, 20]
                    <> unitInfoStub [(10, (7, 2)), (20, (131, -126))]
                    <> endpointInfoStubAt "building" 521 True "Cargo Hold" (2, 2))
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            _ ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            sess ← decodeOr =<< evalDebug ls "return require('scripts.transfer_session').get()"
            spSourceId sess `shouldBe` 20

            -- Argument ORIENTATION: the anchor defining the frame is
            -- the TARGET endpoint on every call, and the re-expressed
            -- coord is the CANDIDATE. Swapping the pairs would localize
            -- the target into each unit's frame instead, which is a
            -- different (and wrong) question.
            anchors ← evalDebug ls (T.concat
                [ "local s = {}; for _, c in ipairs(_G.__localizeCalls) do "
                , "s[#s+1] = string.format('%d:%d', c.ax, c.ay) end; "
                , "return table.concat(s, ',')" ])
            anchors `shouldBe` "\"2:2,2:2\""
            tiles ← evalDebug ls (T.concat
                [ "local s = {}; for _, c in ipairs(_G.__localizeCalls) do "
                , "s[#s+1] = string.format('%d:%d', c.gx, c.gy) end; "
                , "table.sort(s); return table.concat(s, ',')" ])
            tiles `shouldBe` "\"131:-126,7:2\""

        it "the eligibility query, not building.getStorageCapacity, decides Transfer (no policy duplication)" $ \env → do
            -- #1014 review's discriminating test: a positive storage
            -- capacity alone must NOT be enough. Stub the engine
            -- eligibility query to INELIGIBLE while getStorageCapacity
            -- still reports capacity > 0 -- if the menu ever fell back
            -- to a local hasStorage-style check for Transfer, this
            -- would wrongly show it.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 506 "built" 200 [] <> selectedStub [7]
                    <> endpointInfoStub "building" 506 False "")
            claimed1 ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed1 `shouldBe` "true"
            labels1 ← capturedLabels ls
            labels1 `shouldSatisfy` T.isInfixOf "Contents"
            labels1 `shouldNotSatisfy` T.isInfixOf "Transfer"

            -- Same capacity, same activity -- only the query's answer
            -- flips, and now Transfer appears.
            run ls (endpointInfoStub "building" 506 True "Cargo Hold")
            claimed2 ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed2 `shouldBe` "true"
            labels2 ← capturedLabels ls
            labels2 `shouldSatisfy` T.isInfixOf "Transfer"

    describe "unit receiver (scripts/init_context_menu.lua tryUnitMenu)" $ do
        it "one selected acolyte + technomule -> Transfer appears (alongside Info/Attack)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 99 <> selectedStub [7]
                    <> endpointInfoStub "unit" 99 True "Technomule")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Info"
            labels `shouldSatisfy` T.isInfixOf "Attack"
            labels `shouldSatisfy` T.isInfixOf "Transfer"

        it "a non-transfer-capable unit -> Transfer does not appear" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 42 <> selectedStub [7]
                    <> endpointInfoStub "unit" 42 False "")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Info"
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

        it "selecting only the target itself -> no self-transfer" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 99 <> selectedStub [99]
                    <> endpointInfoStub "unit" 99 True "Technomule")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

        -- #1239 requirement 4: the unit-target path gets nearest-of-N
        -- too, and the self-exclusion composes with it. The target
        -- itself is listed FIRST and sits at distance 0, the lowest
        -- uid is the FARTHEST candidate -- so excluding the target,
        -- taking the first, or taking the lowest uid each answer
        -- differently, and only "nearest of the rest" answers 8.
        it "a multi-unit selection including the target picks the nearest of the REST" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 99 <> selectedStub [99, 7, 8]
                    <> unitInfoStub [(99, (10, 20)), (7, (10, 25)), (8, (10, 22))]
                    <> endpointInfoStubAt "unit" 99 True "Technomule" (10, 20))
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Transfer"
            invoke ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            invoke `shouldNotSatisfy` isLuaError
            sess ← decodeOr =<< evalDebug ls "return require('scripts.transfer_session').get()"
            spSourceId sess `shouldBe` 8
            spDestinationKind sess `shouldBe` "unit"
            spDestinationId sess `shouldBe` 99

        -- #1085 §9's deliberate widening, proved against the REAL
        -- engine query and REAL faction data rather than a Boolean
        -- stub: 'liveEndpointInfo' puts unit.transferEndpointInfo back,
        -- and 'withLiveUnits' installs live instances the projection
        -- reads uiFactionId out of.
        it "an ordinary player acolyte target -> Transfer appears (A2 widening)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 4242 <> selectedStub [7] <> liveEndpointInfo)
            withLiveUnits env [(UnitId 4242, liveUnit FactionPlayer)] $ do
                claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
                claimed `shouldBe` "true"
                labels ← capturedLabels ls
                labels `shouldSatisfy` T.isInfixOf "Transfer"

        it "a non-player-commandable target -> Transfer does not appear (A2 widening)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 4243 <> selectedStub [7] <> liveEndpointInfo)
            withLiveUnits env [(UnitId 4243, liveUnit FactionWildlife)] $ do
                claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
                claimed `shouldBe` "true"
                labels ← capturedLabels ls
                labels `shouldSatisfy` T.isInfixOf "Info"
                labels `shouldNotSatisfy` T.isInfixOf "Transfer"

    -- #1253 requirement 1: the way out of a durable order (#1246) the
    -- player queued. Stubs unit.getTransferOrders / cancelTransferOrder /
    -- pruneTransferOrder because this bare backend has no live page and
    -- therefore no order store at all -- what is under test here is the
    -- MENU's gate and the gesture's own sequencing, not the engine verbs
    -- (which 'Test.Headless.Unit.TransferOrderApi' drives against a real
    -- store).
    describe "cancel transfer (#1253)" $ do
        it "a target carrying an active order offers Cancel transfer" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 77 <> selectedStub [7]
                    <> transferOrderStub 77 [(4, False)])
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Cancel transfer"

        it "a target with NO order omits the entry entirely (never a \
           \disabled row)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 78 <> selectedStub [7]
                    <> transferOrderStub 78 [])
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Info"
            labels `shouldNotSatisfy` T.isInfixOf "Cancel transfer"

        it "a target whose only order is already TERMINAL omits the entry" $ \env → do
            -- A terminal order is finished work awaiting its prune, not
            -- something the player can still call off. Offering it would
            -- promise a cancellation that cancels nothing.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 79 <> selectedStub [7]
                    <> transferOrderStub 79 [(4, True)])
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldNotSatisfy` T.isInfixOf "Cancel transfer"

        it "the entry gates on the TARGET's own orders, not the selection's" $ \env → do
            -- The right-clicked unit is the one carrying the order and
            -- the one the menu describes; a selected bystander's haul is
            -- somebody else's business.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 80 <> selectedStub [7]
                    <> transferOrderStub 7 [(4, False)])
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldNotSatisfy` T.isInfixOf "Cancel transfer"

        it "invoking it cancels the order, prunes it, and releases the \
           \carrier -- in that order" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 81 <> selectedStub [7]
                    <> transferOrderStub 81 [(4, False), (9, False)])
            -- The carrier is mid-haul, so the gesture must stop its walk.
            -- scripts.unit_ai_core reads the singleton out of
            -- package.loaded["scripts.unit_ai"], so the entry module has
            -- to be pulled in first -- exactly as any real caller does.
            run ls "require('scripts.unit_ai'); \
                   \require('scripts.unit_ai_core').ensureState(81)\
                   \.currentAction = 'transfer_order'; "
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            invoke ← evalDebug ls "_G.__cancelCallback(); return 'ok'"
            invoke `shouldNotSatisfy` isLuaError
            -- EVERY non-terminal order, because the entry promises to
            -- release the unit and releasing it from one of two queued
            -- hauls would send it straight off on the other.
            trace ← evalDebug ls "return table.concat(_G.__orderCalls, ',')"
            trace `shouldBe` "\"stop:81,cancel:4,event,prune:4,\
                             \cancel:9,event,prune:9\""
            -- The event really is player-visible and attributed, and the
            -- prune follows it rather than racing it.
            evs ← evalDebug ls "return table.concat(_G.__emitted, ' | ')"
            evs `shouldSatisfy` T.isInfixOf "unit_event"
            evs `shouldSatisfy` T.isInfixOf "cancelled"
            evs `shouldSatisfy` T.isInfixOf "uid=81"

        it "cancelling a unit that is NOT running the transfer job leaves \
           \its current action alone" $ \env → do
            -- Requirement: cancellation stops transfer-directed movement
            -- and nothing else. A unit interrupted mid-meal keeps eating;
            -- its queued order is still cancelled.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 82 <> selectedStub [7]
                    <> transferOrderStub 82 [(4, False)])
            run ls "require('scripts.unit_ai'); \
                   \require('scripts.unit_ai_core').ensureState(82)\
                   \.currentAction = 'eat_from_inventory'; "
            _ ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            _ ← evalDebug ls "_G.__cancelCallback(); return 'ok'"
            trace ← evalDebug ls "return table.concat(_G.__orderCalls, ',')"
            trace `shouldBe` "\"cancel:4,event,prune:4\""

    -- Lives in this file because it needs exactly this bare-Lua fixture
    -- and the executor (scripts/unit_ai_transfer.lua) has no headless
    -- home of its own -- everything else about it is probe-gated. What
    -- is pinned here is the RULE, deterministically; the probe's
    -- partial-batch phase pins the resulting occurrence count against a
    -- real engine.
    -- #1254 requirement 3, signed off 2026-08-11: a PLAYER order to a
    -- unit a Mode A session is holding ends that session and then
    -- proceeds. The rule itself, and the two-sided release, are pinned
    -- against a real scene in 'Test.Headless.UI.TransferSession'; what
    -- these cases add is that the player's own INGRESS actually reaches
    -- it — and that it does so BEFORE the command, which is the half a
    -- test of the boundary alone cannot see.
    --
    -- Two DIFFERENT orders on purpose (the issue review's "not
    -- move-only"): Attack from the unit menu and Move here from the
    -- item menu, which between them cover both of
    -- 'scripts/init_context_menu.lua''s command families. The
    -- right-click move order in 'scripts/init_mouse.lua' is the same
    -- one-line call against the same boundary.
    describe "a player order ends a held session (#1254)" $ do
        let sessionOn ∷ Text
            sessionOn = "require('scripts.transfer_session')\
                        \.create(7, 'unit', 99); "
            -- Replace the command with a recorder that reports what the
            -- session looked like AT THE MOMENT it ran. "nil" there is
            -- the ordering proof: the teardown had already finished, so
            -- nothing it does can undo the order about to be issued.
            recorder ∷ Text → Text
            recorder verb = T.concat
                [ "require('scripts.unit_ai'); "
                , "package.loaded['scripts.unit_ai'].", verb
                , " = function(uid, a, b) _G.__ordered = { uid = uid, "
                , "  session = tostring("
                , "    require('scripts.transfer_session').get()) } end; " ]

        it "Attack ends it first, then commands — the boundary is not \
           \move-only" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 99 <> selectedStub [7]
                    <> endpointInfoStub "unit" 99 True "Technomule")
            run ls sessionOn
            held ← evalDebug ls
                "return tostring(require('scripts.transfer_session')\
                \.holdsUnit(7))"
            held `shouldBe` "\"true\""
            run ls (recorder "commandAttack")
            claimed ← evalDebug ls
                "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            invoke ← evalDebug ls "_G.__callbacks['Attack'](); return 'ok'"
            invoke `shouldNotSatisfy` isLuaError
            ordered ← evalDebug ls "return _G.__ordered"
            ordered `shouldSatisfy` T.isInfixOf "\"uid\":7"
            ordered `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone ← evalDebug ls
                "return tostring(require('scripts.transfer_session').get())"
            gone `shouldBe` "\"nil\""

        it "Move here does the same, and a unit no session holds is \
           \commanded exactly as before" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (selectedStub [7] <> endpointInfoStub "unit" 99 True "Mule"
                    <> groundItemStub 55 (3, 4))
            run ls sessionOn
            run ls (recorder "commandMove")
            claimed ← evalDebug ls
                "return require('scripts.init_context_menu').tryItemMenu(10, 20)"
            claimed `shouldBe` "true"
            _ ← evalDebug ls "_G.__callbacks['Move here'](); return 'ok'"
            ordered ← evalDebug ls "return _G.__ordered"
            ordered `shouldSatisfy` T.isInfixOf "\"uid\":7"
            ordered `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            -- With no session left, the identical gesture still issues
            -- the identical command: the boundary adds a teardown, it
            -- does not gate the order.
            _ ← evalDebug ls "_G.__ordered = nil; \
                             \_G.__callbacks['Move here'](); return 'ok'"
            again ← evalDebug ls "return _G.__ordered"
            again `shouldSatisfy` T.isInfixOf "\"uid\":7"
            again `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""

    describe "outcome reporting is exactly-once (#1253)" $ do
        it "an arrival report SKIPS failures the command-time gate \
           \already surfaced, and reports the ones it did not" $ \env → do
            -- Twelve into room for eight: the four create-time
            -- receiver_full refusals were warned about when the order was
            -- queued, and the commit result carries them again alongside
            -- the eight it just decided. Reporting the whole list would
            -- file a second warning for one refusal.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls emitSpyLua
            run ls "require('scripts.unit_ai'); "
            reported ← evalDebug ls (T.concat
                [ "local o = require('scripts.unit_ai_transfer_outcome'); "
                -- The order as it stood BEFORE the commit: one entry
                -- still walking, one already refused at create time.
                , "local pre = { entries = { "
                , "  { instanceId = 1, state = 'in_transit' }, "
                , "  { instanceId = 2, state = 'failed', reason = 'receiver_full' } "
                , "} }; "
                , "local settled = o.settledIds(pre); "
                -- …and what the commit reports: BOTH, per pushBatchResult.
                -- The two carry DIFFERENT causes so the emitted text says
                -- which one survived the filter -- reportOutcomes quotes
                -- the cause, since for an arrival refusal that is the
                -- explanation.
                , "local outcomes = { "
                , "  { instanceId = 1, defName = 'ration', state = 'failed', "
                , "    reason = 'became_stale', cause = 'instance_missing' }, "
                , "  { instanceId = 2, defName = 'ration', state = 'failed', "
                , "    reason = 'receiver_full' } "
                , "}; "
                , "return o.reportOutcomes(7, outcomes, 'couldn\\'t transfer', "
                , "                        settled)" ])
            reported `shouldBe` "1"
            evs ← evalDebug ls "return table.concat(_G.__emitted, ' | ')"
            evs `shouldSatisfy` T.isInfixOf "instance_missing"
            evs `shouldNotSatisfy` T.isInfixOf "receiver_full"
            -- ONE warning, for the entry THIS commit refused, and with no
            -- "(and 1 more)" tail claiming the excluded one alongside it.
            evs `shouldNotSatisfy` T.isInfixOf " | "
            evs `shouldNotSatisfy` T.isInfixOf "more)"

        it "with no exclusion set every failure is reported, so the \
           \command-time gate itself still says everything" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls emitSpyLua
            run ls "require('scripts.unit_ai'); "
            reported ← evalDebug ls (T.concat
                [ "local o = require('scripts.unit_ai_transfer_outcome'); "
                , "return o.reportOutcomes(7, { "
                , "  { instanceId = 1, defName = 'ration', state = 'failed', "
                , "    reason = 'receiver_full' }, "
                , "  { instanceId = 2, defName = 'ration', state = 'failed', "
                , "    reason = 'receiver_full' } "
                , "}, 'can\\'t transfer')" ])
            -- Two refusals, still ONE bounded warning naming the first
            -- and counting the rest.
            reported `shouldBe` "2"
            evs ← evalDebug ls "return table.concat(_G.__emitted, ' | ')"
            evs `shouldSatisfy` T.isInfixOf "(and 1 more)"

        it "settledIds names every terminal state and nothing pending" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls "require('scripts.unit_ai'); "
            ids ← evalDebug ls (T.concat
                [ "local o = require('scripts.unit_ai_transfer_outcome'); "
                , "local s = o.settledIds({ entries = { "
                , "  { instanceId = 1, state = 'queued' }, "
                , "  { instanceId = 2, state = 'in_transit' }, "
                , "  { instanceId = 3, state = 'ready_to_commit' }, "
                , "  { instanceId = 4, state = 'completed' }, "
                , "  { instanceId = 5, state = 'cancelled' }, "
                , "  { instanceId = 6, state = 'failed' } } }); "
                , "local out = {}; for i = 1, 6 do "
                , "  out[i] = s[i] and '1' or '0' end; "
                , "return table.concat(out, '')" ])
            ids `shouldBe` "\"000111\""

    describe "session creation (scripts/transfer_session.lua)" $ do
        it "selecting Transfer creates a session with the exact endpoint identities" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 507 "built" 200 [] <> selectedStub [7]
                    <> endpointInfoStub "building" 507 True "Cargo Hold")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            invoke ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            invoke `shouldNotSatisfy` isLuaError
            r ← evalDebug ls "return require('scripts.transfer_session').get()"
            sess ← decodeOr r
            spSourceKind sess `shouldBe` "unit"
            spSourceId sess `shouldBe` 7
            spDestinationKind sess `shouldBe` "building"
            spDestinationId sess `shouldBe` 507
            spDestinationDisplayName sess `shouldBe` "Cargo Hold"
            spContractState sess `shouldBe` "queued"

        -- #1246 requirement 6 / epic decision D-3: durable transfer
        -- ORDERS now persist in their own save component, and the Mode A
        -- session must stay exactly as transient as it was. This asserts
        -- the hook itself — that 'transfer_session.init' registers one
        -- under its own id, and that running it drops the live session.
        -- The other half of the chain (a real load runs EVERY registered
        -- reset hook, once every component has committed) is
        -- 'Test.Headless.Lua.SaveModules''s existing applyAll coverage;
        -- the two together are what makes "no session survives a load"
        -- true, and neither on its own would be.
        it "the Mode A session is cleared by its save-load reset hook, \
           \and contributes NO save component of its own (#1246 D-3)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (endpointInfoStub "unit" 99 True "Technomule")
            run ls "require('scripts.transfer_session').init('transfer_session'); "
            created ← evalDebug ls
                "local s = require('scripts.transfer_session').create(7, 'unit', 99); return s"
            created `shouldNotSatisfy` isLuaError
            -- Registered as a RESET hook, never as a persistent
            -- component: a session in saveModules.registry would mean
            -- Mode A state riding into a save.
            hookKind ← evalDebug ls
                "local sm = require('scripts.lib.save_modules'); \
                \return type(sm.resetHooks['transfer_session']) \
                \  .. '/' .. type(sm.registry['transfer_session'])"
            -- Debug-console return values are JSON-serialized, so a
            -- string comes back quoted.
            hookKind `shouldBe` "\"function/nil\""
            cleared ← evalDebug ls
                "local sm = require('scripts.lib.save_modules'); \
                \sm.resetHooks['transfer_session'](); \
                \return tostring(require('scripts.transfer_session').get())"
            cleared `shouldBe` "\"nil\""

        it "is reusable independent of the context-menu callback (requirement 8)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (endpointInfoStub "unit" 99 True "Technomule")
            -- Calls transfer_session.create directly -- no
            -- init_context_menu, no tryUnitMenu, no click simulation.
            r ← evalDebug ls
                "local s = require('scripts.transfer_session').create(7, 'unit', 99); return s"
            sess ← decodeOr r
            spSourceKind sess `shouldBe` "unit"
            spSourceId sess `shouldBe` 7
            spDestinationKind sess `shouldBe` "unit"
            spDestinationId sess `shouldBe` 99

        it "a stale/missing receiver yields no session and mutates nothing" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            -- No unit.transferEndpointInfo stub for id 999 -> the
            -- shared base stub returns nil for any unrecognised id
            -- (see endpointInfoStub / baseSetupLua's default).
            run ls
                "unit.commitTransfer = function() error('B1 must never commit a transfer') end;"
            before ← evalDebug ls "return require('scripts.transfer_session').get()"
            before `shouldBe` "null"
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session').create(7, 'building', 999); return reason"
            r `shouldNotSatisfy` isLuaError
            r `shouldBe` "\"receiver_missing\""
            after ← evalDebug ls "return require('scripts.transfer_session').get()"
            after `shouldBe` "null"

        it "an ineligible receiver yields no session" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (endpointInfoStub "building" 508 False "")
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session').create(7, 'building', 508); return reason"
            r `shouldBe` "\"receiver_ineligible\""
            after ← evalDebug ls "return require('scripts.transfer_session').get()"
            after `shouldBe` "null"

        -- #1415: the source and PAIR rules 'M.resolveSource' had always
        -- enforced and 'M.create' trusted its caller for. Driven
        -- DIRECTLY -- no init_context_menu, no tryUnitMenu, no click --
        -- because the whole claim being hardened is that this boundary
        -- is reusable by a surface that never resolved a source.

        it "a source that no longer projects as an endpoint yields no \
           \session, reports source_missing, and warns the source once" $
           \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls warningSpy
            -- Answers for the DESTINATION only. 'unit.exists' still
            -- says yes (baseSetupLua's default), so the top guard
            -- passes and it is the PROJECTION that comes back empty --
            -- which is the ordering Unit.Transfer.planItemWith uses
            -- too, classifying an absent source ahead of every pair
            -- rule.
            run ls (pairStub ("building", 530, "p") Nothing)
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session')\
                \.create(7, 'building', 530); \
                \return { made = tostring(s), reason = tostring(reason), \
                \         warns = _G.__warnCount(7) }"
            r `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            r `shouldSatisfy` T.isInfixOf "\"reason\":\"source_missing\""
            r `shouldSatisfy` T.isInfixOf "\"warns\":1"
            after ← evalDebug ls "return require('scripts.transfer_session').get()"
            after `shouldBe` "null"

        it "an INELIGIBLE source is reported as contract_unavailable when \
           \the live contract has dropped source_ineligible (#1014 review \
           \round 2's rule, applied to the id #1415 adds)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (pairStub ("building", 531, "p") (Just (7, False, "p")))
            -- Everything the module names EXCEPT the id this branch
            -- wants, so a fallback to the unverified string would show
            -- up as 'source_ineligible' here.
            run ls (contractStub [ "source_missing", "receiver_missing"
                                 , "receiver_ineligible", "out_of_range" ])
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session')\
                \.create(7, 'building', 531); return reason"
            r `shouldBe` "\"contract_unavailable\""
            after ← evalDebug ls "return require('scripts.transfer_session').get()"
            after `shouldBe` "null"

        it "…and a CROSS-PAGE pair the same way when out_of_range is gone" $
           \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            -- Identical coordinates on both sides, so the only thing
            -- separating these endpoints is page identity.
            run ls (pairStub ("building", 532, "p") (Just (7, True, "q")))
            run ls (contractStub [ "source_missing", "source_ineligible"
                                 , "receiver_missing", "receiver_ineligible" ])
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session')\
                \.create(7, 'building', 532); return reason"
            r `shouldBe` "\"contract_unavailable\""
            after ← evalDebug ls "return require('scripts.transfer_session').get()"
            after `shouldBe` "null"

        it "the boot-time vocabulary check names source_ineligible and \
           \out_of_range, so drift in either is visible at startup" $
           \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls "_G.__logged = {}; engine.logWarn = function(m) \
                   \_G.__logged[#_G.__logged + 1] = tostring(m) end; "
            -- A contract advertising every id the module named BEFORE
            -- #1415 and neither of the two it names now.
            run ls (contractStub [ "source_missing", "receiver_missing"
                                 , "receiver_ineligible" ])
            drifted ← evalDebug ls
                "require('scripts.transfer_session').init('transfer_session'); \
                \return table.concat(_G.__logged, '\\n')"
            drifted `shouldSatisfy` T.isInfixOf "'source_ineligible'"
            drifted `shouldSatisfy` T.isInfixOf "'out_of_range'"
            -- The same check against a contract that still advertises
            -- them warns about neither, so the assertion above is about
            -- DRIFT rather than about a check that fires unconditionally.
            run ls "_G.__logged = {}; "
            run ls (contractStub [ "source_missing", "source_ineligible"
                                 , "receiver_missing", "receiver_ineligible"
                                 , "out_of_range" ])
            clean ← evalDebug ls
                "require('scripts.transfer_session').init('transfer_session'); \
                \return table.concat(_G.__logged, '\\n')"
            clean `shouldNotSatisfy` T.isInfixOf "'source_ineligible'"
            clean `shouldNotSatisfy` T.isInfixOf "'out_of_range'"

        it "the endpoint-kind/state identity is validated against the live contract by MEMBERSHIP (#1085 §11)" $ \env → do
            -- #1014 round 1 read operations[1]/states[1] POSITIONALLY
            -- because the contract was three flat arrays. A2 removed the
            -- operation concept and publishes endpointKinds as a named
            -- SET, so a contract that reorders or drops an array
            -- position must not change what a session records -- only
            -- membership may.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (endpointInfoStub "building" 509 True "Cargo Hold")
            run ls (T.concat
                [ "unit.transferContract = function() return { "
                , "reasons = {'receiver_ineligible','receiver_missing',"
                , "'source_missing'}, "
                -- 'queued' deliberately LAST, and the kinds are a set.
                , "states = {'failed','completed','queued'}, "
                , "endpointKinds = { building = true, unit = true } } end;"
                ])
            r ← evalDebug ls
                "local s = require('scripts.transfer_session').create(7, 'building', 509); return s"
            sess ← decodeOr r
            spDestinationKind sess `shouldBe` "building"
            spContractState sess `shouldBe` "queued"

        it "a DESTINATION kind the live contract does not advertise fails cleanly" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (endpointInfoStub "building" 513 True "Cargo Hold")
            run ls (T.concat
                [ "unit.transferContract = function() return { "
                , "reasons = {'receiver_ineligible','receiver_missing',"
                , "'source_missing'}, states = {'queued'}, "
                , "endpointKinds = { unit = true } } end;"
                ])
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session').create(7, 'building', 513); return reason"
            r `shouldBe` "\"contract_unavailable\""
            after ← evalDebug ls "return require('scripts.transfer_session').get()"
            after `shouldBe` "null"

        it "a SOURCE kind the live contract does not advertise fails cleanly too" $ \env → do
            -- The inverse of the case above. B1's gesture always makes
            -- the source a unit, but "always a unit" is still an id this
            -- module NAMES, so it goes through the same membership gate
            -- as everything else -- a contract that dropped 'unit' must
            -- not still mint a session whose source kind no engine verb
            -- recognises, even though the DESTINATION kind resolves.
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (endpointInfoStub "building" 514 True "Cargo Hold")
            run ls (T.concat
                [ "unit.transferContract = function() return { "
                , "reasons = {'receiver_ineligible','receiver_missing',"
                , "'source_missing'}, states = {'queued'}, "
                , "endpointKinds = { building = true } } end;"
                ])
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session').create(7, 'building', 514); return reason"
            r `shouldBe` "\"contract_unavailable\""
            after ← evalDebug ls "return require('scripts.transfer_session').get()"
            after `shouldBe` "null"

        it "records the source kind RESOLVED from the contract, not a literal" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (endpointInfoStub "building" 515 True "Cargo Hold")
            r ← evalDebug ls
                "local s = require('scripts.transfer_session').create(7, 'building', 515); return s"
            sess ← decodeOr r
            spSourceKind sess `shouldBe` "unit"
            spDestinationKind sess `shouldBe` "building"

        it "an unavailable live contract fails session creation cleanly, no session" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (endpointInfoStub "building" 510 True "Cargo Hold")
            -- Simulate a malformed/unavailable contract -- the resolve*
            -- helpers must refuse to fall back to a guessed
            -- endpoint-kind/state string.
            run ls "unit.transferContract = function() return nil end;"
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session').create(7, 'building', 510); return reason"
            r `shouldBe` "\"contract_unavailable\""
            after ← evalDebug ls "return require('scripts.transfer_session').get()"
            after `shouldBe` "null"

        it "a reason id missing from the live contract is reported as contract_unavailable, not the unverified string (#1014 review round 2)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            -- No unit.transferEndpointInfo stub for id 512 -> M.create's
            -- receiver-missing branch fires, and would normally report
            -- resolveReason("receiver_missing"). Simulate a contract
            -- that has DRIFTED and no longer advertises that reason id.
            run ls (T.concat
                [ "unit.transferContract = function() return { "
                , "reasons = {'source_missing'}, "
                , "states = {'queued'}, "
                , "endpointKinds = { unit = true, building = true } } end;"
                ])
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session').create(7, 'building', 512); return reason"
            r `shouldBe` "\"contract_unavailable\""

        it "Exit to Menu clears a pending session (#1014 review round 1: the save-load reset hook alone misses this path)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (endpointInfoStub "building" 511 True "Cargo Hold")
            -- world.destroyAll is real and engine-wide: this suite's
            -- worldgen specs share ONE memoized world across the whole
            -- surrounding `aroundAll` block (Test.Headless.Harness.
            -- sharedWorld), so calling the REAL destroyAll here would
            -- tear that down for every later spec in the same block
            -- (#1014 review round 3). Stub it -- this test is only
            -- about the Lua WIRING (does onExitToMenu clear the
            -- session?), not about exercising a real world teardown.
            run ls "world.destroyAll = function() end;"
            _ ← evalDebug ls
                "require('scripts.transfer_session').create(7, 'building', 511); return 'ok'"
            before ← evalDebug ls "return require('scripts.transfer_session').get()"
            before `shouldNotBe` "null"
            exitR ← evalDebug ls "require('scripts.pause_menu').onExitToMenu(); return 'ok'"
            exitR `shouldNotSatisfy` isLuaError
            after ← evalDebug ls "return require('scripts.transfer_session').get()"
            after `shouldBe` "null"

-- * Real-Lua-backend helper (mirrors
--   Test.Headless.World.SelectTileZ.newBareLuaBackend / Test.Headless.
--   UI.InputOwnership's copy of the same recipe): a real Lua backend
--   with the full Lua API registered and nothing preloaded, so
--   scripts.init_context_menu and scripts.transfer_session are pulled
--   in the same way any real caller's @require@ would.

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

-- | Run a setup statement and fail loudly if it errors -- every setup
-- block below is plumbing, not the behavior under test, so a broken
-- stub should never masquerade as a passing (or silently-wrong)
-- assertion later in the same test.
run ∷ LuaBackendState → Text → IO ()
run ls stmt = do
    r ← evalDebug ls stmt
    r `shouldNotSatisfy` isLuaError

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- * Shared stubs

-- | Spy on contextMenu.show (capturing every row's label into
-- @_G.__lastLabels@, joined -- an EMPTY Lua array serialises as a JSON
-- OBJECT, not @[]@, so a joined string sidesteps that ambiguity
-- entirely, mirroring Test.Headless.UI.TutorialHud's rowIds
-- convention) and the "Transfer" row's own callback into
-- @_G.__transferCallback@, so it can be invoked in a LATER evalDebug
-- call without ever serializing a Lua function back to Haskell
-- (mirrors SelectTileZ's @_G.__infoCallback@). Also stubs
-- faction.isPlayerCommandable/canAttack to the fixed vocabulary every
-- scenario below uses ("player" = commandable), and gives
-- unit.transferEndpointInfo/unit.exists/unit.getInfo/unit.getFaction
-- safe defaults a per-test stub then overrides for the ids it cares
-- about.
baseSetupLua ∷ Text
baseSetupLua = T.concat
    [ "local contextMenu = require('scripts.ui.context_menu'); "
    , "_G.__lastLabels = nil; "
    , "_G.__transferCallback = nil; "
    , "_G.__cancelCallback = nil; "
    , "_G.__callbacks = {}; "
    , "contextMenu.show = function(items, mx, my) "
    , "  local labels = {}; "
    , "  _G.__callbacks = {}; "
    , "  for i, it in ipairs(items) do "
    , "    labels[i] = it.label; "
    -- #1254 needs rows the two named captures below never covered
    -- (Attack, Move here), and by LABEL rather than by adding a third
    -- one-off global for each.
    , "    _G.__callbacks[it.label] = it.callback; "
    , "    if it.label == 'Transfer' then _G.__transferCallback = it.callback end; "
    , "    if it.label == 'Cancel transfer' then "
    , "      _G.__cancelCallback = it.callback end "
    , "  end; "
    , "  _G.__lastLabels = labels; "
    , "end; "
    , "faction.isPlayerCommandable = function(f) return f == 'player' end; "
    , "faction.canAttack = function(a, b) return true end; "
    , "unit.exists = function(uid) return true end; "
    -- A real 'unit.getInfo' always reports the unit's def name, and
    -- since #1250 the source rule reads it (to ask whether that species
    -- can run the escort at all), so a stub that omitted it would model
    -- a unit no engine can produce.
    , "unit.getInfo = function(uid) "
    , "  return { gridX = 1, gridY = 2, defName = 'acolyte' } end; "
    , "unit.getFaction = function(uid) return 'player' end; "
    , "unit.getWounds = function(uid) return {} end; "
    , "unit.getKnowledge = function(uid, k) return false end; "
    -- Captured BEFORE the stub replaces it, so the widening cases
    -- below can put the REAL engine query back (see liveEndpointInfo).
    , "_G.__realTransferEndpointInfo = unit.transferEndpointInfo; "
    , "unit.transferEndpointInfo = function(ep) return nil end; "
    ]

-- | @unit.transferEndpointInfo@ answering for exactly one DESTINATION
-- and (optionally) one SOURCE unit, each on a page of the caller's
-- choosing -- the stub #1415's own cases need, because each of them
-- turns on a property of the source projection that
-- 'endpointInfoStubAt' deliberately makes uniform.
--
-- Both endpoints report the SAME grid position, so a case that
-- separates them by page is proving page identity and nothing else: at
-- equal coordinates a distance rule could not tell them apart.
-- @Nothing@ leaves the source unanswered, which is how a unit whose
-- projection has stopped resolving reads.
pairStub ∷ (Text, Int, Text) → Maybe (Int, Bool, Text) → Text
pairStub (dstKind, dstId, dstPage) mSrc = T.concat
    [ "unit.transferEndpointInfo = function(ep) "
    , "  if ep and ep.kind == '", dstKind, "' and ep.id == ", tshow dstId
    , " then "
    , "    return { eligible = true, displayName = 'Cargo Hold', page = '"
    , dstPage, "', gridX = 10, gridY = 20, capacity = 200, "
    , "             storedWeight = 0, contents = {} } "
    , "  end; "
    , case mSrc of
        Nothing → ""
        Just (srcId, srcEligible, srcPage) → T.concat
            [ "  if ep and ep.kind == 'unit' and ep.id == ", tshow srcId
            , " then "
            , "    return { eligible = "
            , if srcEligible then "true" else "false"
            , ", displayName = 'Acolyte', page = '", srcPage
            , "', gridX = 10, gridY = 20, capacity = 200, "
            , "             storedWeight = 0, contents = {} } "
            , "  end; " ]
    , "  return nil end; "
    ]

-- | @unit.transferContract()@ advertising exactly these reason ids (and
-- the state/endpoint-kind vocabulary every case here keeps whole), so a
-- case can remove ONE id and see what the module does without it.
contractStub ∷ [Text] → Text
contractStub reasons = T.concat
    [ "unit.transferContract = function() return { reasons = {"
    , T.intercalate ", " [ T.concat [ "'", r, "'" ] | r ← reasons ]
    , "}, states = {'queued'}, "
    , "endpointKinds = { unit = true, building = true } } end; "
    ]

-- | Record the uid every @unit_warning@ is attributed to, so a refusal
-- can be checked for emitting exactly ONE and for aiming it at the
-- SOURCE (#1415 requirement 4). Replaces the verb outright: this
-- backend has no event log to read back through, and what is under test
-- is the attribution, not the engine's own emit.
warningSpy ∷ Text
warningSpy = T.concat
    [ "_G.__warns = {}; "
    , "engine.emitEventForUnit = function(cat, text, uid) "
    , "  if cat == 'unit_warning' then "
    , "    _G.__warns[#_G.__warns + 1] = tostring(uid) end "
    , "end; "
    , "_G.__warnCount = function(uid) "
    , "  local n = 0; "
    , "  for _, u in ipairs(_G.__warns) do "
    , "    if u == tostring(uid) then n = n + 1 end "
    , "  end; return n end; "
    ]

-- | @unit.getSelected()@ returning exactly this fixed uid list.
selectedStub ∷ [Int] → Text
selectedStub uids = "unit.getSelected = function() return {"
    <> T.intercalate "," (map (T.pack . show) uids) <> "} end; "

-- | @unit.transferEndpointInfo({ kind, id })@ answering for this one
-- named endpoint, plus a generic live projection for every OTHER unit
-- id -- which is what a selected SOURCE reads as. A building id this
-- stub does not name still falls through to baseSetupLua's default nil,
-- matching a genuinely stale/missing target.
--
-- The source half is not decoration (#1415): since @M.create@ projects
-- its source endpoint too, a stub answering for the destination alone
-- would make every unchanged success case below refuse with
-- @source_missing@ -- and it would be refusing against a world no
-- engine can produce, in which the unit the menu just ranked has no
-- projection at all. A case that needs the source to be missing,
-- ineligible or elsewhere says so with its own stub rather than
-- relying on this one's silence.
endpointInfoStub ∷ Text → Int → Bool → Text → Text
endpointInfoStub kind rid eligible displayName =
    endpointInfoStubAt kind rid eligible displayName (10, 20)

-- | The same stub with the endpoint's own grid position spelled out --
-- since #1239 that position is the point selected units are ranked
-- against, so a nearest-of-N case has to control it.
endpointInfoStubAt ∷ Text → Int → Bool → Text → (Int, Int) → Text
endpointInfoStubAt kind rid eligible displayName (gx, gy) = T.concat
    [ "unit.transferEndpointInfo = function(ep) "
    , "  if ep and ep.kind == '", kind, "' and ep.id == "
    , tshow rid, " then "
    , "    return { eligible = ", if eligible then "true" else "false"
    , ", displayName = '", displayName, "', page = 'p', gridX = ", tshow gx
    , ", gridY = ", tshow gy
    , ", capacity = 200, storedWeight = 0, contents = {} } "
    , "  end; "
    -- Same page ('p') as the named endpoint on purpose: these scenarios
    -- are all one-world, so a source that projected elsewhere would
    -- refuse every one of them as cross-page.
    , "  if ep and ep.kind == 'unit' then "
    , "    return { eligible = true, displayName = 'Acolyte', "
    , "             page = 'p', gridX = 1, gridY = 2, capacity = 200, "
    , "             storedWeight = 0, contents = {} } "
    , "  end; return nil end; "
    ]

-- | @unit.getInfo@ answering a fixed per-uid grid position. A uid with
-- NO entry returns nil -- exactly how a unit destroyed between
-- selection and resolution reads, which 'resolveSource' must skip
-- rather than trip over.
unitInfoStub ∷ [(Int, (Int, Int))] → Text
unitInfoStub entries = unitInfoStubF
    [ (uid, (fromIntegral gx, fromIntegral gy)) | (uid, (gx, gy)) ← entries ]

-- | The same stub at the CONTINUOUS resolution 'unit.getInfo' actually
-- reports: @Unit.Types.Instance@'s @uiGridX@/@uiGridY@ are Floats
-- pushed with @Lua.pushnumber@, so a unit standing anywhere but a tile
-- corner has a fractional position. 'resolveSource' has to floor those
-- into the whole-tile frame @unit.transferEndpointInfo@ reports.
unitInfoStubF ∷ [(Int, (Double, Double))] → Text
unitInfoStubF entries = T.concat
    [ "unit.getInfo = function(uid) return ({"
    , T.intercalate ", "
        [ T.concat [ "[", tshow uid, "]={gridX=", dshow gx
                   , ",gridY=", dshow gy, ",defName='acolyte'}" ]
        | (uid, (gx, gy)) ← entries ]
    , "})[uid] end; "
    ]

-- | @unit.getFaction@ answering a fixed per-uid faction; an unlisted
-- uid returns nil (the live faction vanished).
unitFactionStub ∷ [(Int, Text)] → Text
unitFactionStub entries = T.concat
    [ "unit.getFaction = function(uid) return ({"
    , T.intercalate ", "
        [ T.concat [ "[", tshow uid, "]='", f, "'" ] | (uid, f) ← entries ]
    , "})[uid] end; "
    ]

-- | A faithful Lua transcription of 'World.Generate.Coordinates.
-- localizeTileToAnchor' for a wrapping world whose u-alias step is
-- @step@ TILES: pick the alias @(gx ± step, gy ∓ step)@ with the
-- smallest Chebyshev distance to the anchor, an exact tie keeping the
-- coord as supplied.
--
-- A @--match "Transfer context menu"@ run initializes no world, and the
-- real @world.localizeTile@ resolves against the VISIBLE page's world
-- size -- identity when there is none. So the seam case installs this
-- rather than depending on another spec's shared world. It also records
-- every call into @_G.__localizeCalls@, which is what lets the case
-- pin the anchor/coord argument ORIENTATION as well as the ranking.
seamLocalizeStub ∷ Int → Text
seamLocalizeStub step = T.concat
    [ "_G.__localizeCalls = {}; "
    , "world.localizeTile = function(ax, ay, gx, gy) "
    , "  table.insert(_G.__localizeCalls, "
    , "    { ax = ax, ay = ay, gx = gx, gy = gy }); "
    , "  local step = ", tshow step, "; "
    , "  local bx, by = gx, gy; "
    , "  local bd = math.max(math.abs(gx - ax), math.abs(gy - ay)); "
    , "  for _, k in ipairs({-1, 1}) do "
    , "    local cx, cy = gx + k * step, gy - k * step; "
    , "    local d = math.max(math.abs(cx - ax), math.abs(cy - ay)); "
    , "    if d < bd then bd, bx, by = d, cx, cy end "
    , "  end; "
    , "  return bx, by "
    , "end; "
    ]

dshow ∷ Double → Text
dshow = T.pack . show

-- | @building.hitTestAt@ (fixed to this bid), @getActivity@,
-- @getStorageCapacity@, @getOperations@ for one building.
buildingStub ∷ Int → Text → Int → [Text] → Text
buildingStub bid activity capacity ops = T.concat
    [ "building.hitTestAt = function(x, y) return ", T.pack (show bid), " end; "
    , "building.getActivity = function(b) return '", activity, "' end; "
    , "building.getStorageCapacity = function(b) return ", T.pack (show capacity), " end; "
    , "building.getOperations = function(b) return {"
    , T.intercalate "," (map (\o → "'" <> o <> "'") ops) <> "} end; "
    ]

-- | Capture every @engine.emitEventForUnit@ call into @_G.__emitted@,
-- joined ' | ' by the reader below, so "how many warnings did that file"
-- is answerable without a real event log.
emitSpyLua ∷ Text
emitSpyLua = T.concat
    [ "_G.__emitted = {}; "
    , "engine.emitEventForUnit = function(cat, msg, u) "
    , "  table.insert(_G.__emitted, cat .. ':' .. msg "
    , "    .. ' uid=' .. tostring(u)) end; "
    ]

-- | @unit.hitTestAt@ fixed to this target uid.
unitTargetStub ∷ Int → Text
unitTargetStub uid = "unit.hitTestAt = function(x, y) return "
    <> T.pack (show uid) <> " end; "

-- | @item.hitTestAt@ / @item.listGround@ answering for ONE ground
-- item, which is all @tryItemMenu@ reads to build its Pick up / Move
-- here rows.
groundItemStub ∷ Int → (Int, Int) → Text
groundItemStub gid (gx, gy) = T.concat
    [ "item.hitTestAt = function(x, y) return ", tshow gid, " end; "
    , "item.listGround = function() return { { id = ", tshow gid
    , ", x = ", tshow gx, ", y = ", tshow gy
    , ", defName = 'rope', weight = 1.0 } } end; " ]

-- | The #1253 order surface for ONE unit: @unit.getTransferOrders@
-- answering the given @(orderId, terminal)@ list for @uid@ and an empty
-- list for every other unit, plus recording stubs for the three verbs
-- the cancel gesture drives.
--
-- Stubbed rather than live because this bare backend has no world page
-- and therefore no order store to create an order IN; what these cases
-- gate is the menu's own condition and the gesture's SEQUENCING (stop,
-- then per order: cancel -> surface -> prune), which is exactly what
-- @_G.__orderCalls@ records. The verbs' own behaviour is driven against
-- a real store in 'Test.Headless.Unit.TransferOrderApi'.
transferOrderStub ∷ Int → [(Int, Bool)] → Text
transferOrderStub uid entries = T.concat
    [ "_G.__orderCalls = {}; _G.__emitted = {}; "
    , "unit.getTransferOrders = function(u) "
    , "  if u ~= ", tshow uid, " then return {} end; "
    , "  return {"
    , T.intercalate ", "
        [ T.concat [ "{ id = ", tshow oid, ", terminal = "
                   , if term then "true" else "false", " }" ]
        | (oid, term) ← entries ]
    , "} end; "
    , "unit.cancelTransferOrder = function(u, oid) "
    , "  table.insert(_G.__orderCalls, 'cancel:' .. tostring(oid)); "
    , "  return true end; "
    , "unit.pruneTransferOrder = function(u, oid) "
    , "  table.insert(_G.__orderCalls, 'prune:' .. tostring(oid)); "
    , "  return true end; "
    , "unit.stop = function(u) "
    , "  table.insert(_G.__orderCalls, 'stop:' .. tostring(u)) end; "
    , "engine.emitEventForUnit = function(cat, msg, u) "
    , "  table.insert(_G.__orderCalls, 'event'); "
    , "  table.insert(_G.__emitted, cat .. ':' .. msg "
    , "    .. ' uid=' .. tostring(u)) end; "
    ]

-- | Put the REAL @unit.transferEndpointInfo@ back after baseSetupLua
-- stubbed it out, so a case can exercise the engine's own
-- faction-derived eligibility instead of a Boolean stub (#1085 §9).
liveEndpointInfo ∷ Text
liveEndpointInfo = "unit.transferEndpointInfo = _G.__realTransferEndpointInfo; "

-- | A live unit of the given faction. Only 'uiFactionId' matters to
-- the endpoint projection's eligibility rule; with no matching entry
-- in @umDefs@ the display name falls back to the prettified def name,
-- which is exactly what a real unmapped unit would report.
liveUnit ∷ Faction → UnitInstance
liveUnit f = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = WorldPageId "ctx_menu_page"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" 100
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = f, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | Install live unit instances for the duration of one case and
-- RESTORE the manager afterwards. This spec shares its engine with the
-- worldgen block (Spec.hs), so a mutation it left behind would leak
-- into every later spec.
withLiveUnits ∷ EngineEnv → [(UnitId, UnitInstance)] → IO α → IO α
withLiveUnits env insts = bracket acquire (writeIORef ref) . const
  where
    ref = unitManagerRef env
    acquire = do
        saved ← readIORef ref
        writeIORef ref saved
            { umInstances = HM.union (HM.fromList insts) (umInstances saved) }
        pure saved

-- | The joined labels string captured by the spy above, or "" if
-- contextMenu.show was never called (the menu didn't open at all).
capturedLabels ∷ LuaBackendState → IO Text
capturedLabels ls = evalDebug ls
    "return _G.__lastLabels and table.concat(_G.__lastLabels, ',') or ''"

-- * Decoded probe shape

data SessionProbe = SessionProbe
    { spSourceKind             ∷ Text
    , spSourceId               ∷ Int
    , spDestinationKind        ∷ Text
    , spDestinationId          ∷ Int
    , spDestinationDisplayName ∷ Text
    , spContractState          ∷ Text
    } deriving (Show, Eq)

-- | #1085 replaced the sourceUid / receiverKind / receiverId triple
-- with NAMED endpoints on both sides, and deleted the session's
-- @contract.operation@ (direction is derived from the endpoint pair,
-- so there is no independent operation left to record).
instance FromJSON SessionProbe where
    parseJSON = withObject "SessionProbe" $ \o → do
        sourceObj      ← o .: "source"
        destinationObj ← o .: "destination"
        contractObj    ← o .: "contract"
        SessionProbe
            <$> sourceObj .: "kind"
            <*> sourceObj .: "id"
            <*> destinationObj .: "kind"
            <*> destinationObj .: "id"
            <*> o .: "destinationDisplayName"
            <*> contractObj .: "state"

decodeOr ∷ FromJSON a ⇒ Text → IO a
decodeOr t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just v  → pure v
    Nothing → do
        expectationFailure ("failed to decode: " ⧺ T.unpack t)
        fail "unreachable"
