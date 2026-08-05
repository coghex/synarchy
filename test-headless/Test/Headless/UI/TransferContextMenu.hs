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

        it "multiple selected sources -> no silent source choice (Transfer omitted)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 505 "built" 200 [] <> selectedStub [7, 8]
                    <> endpointInfoStub "building" 505 True "Cargo Hold")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Contents"
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

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

        it "an endpoint kind the live contract does not advertise fails cleanly" $ \env → do
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
    , "contextMenu.show = function(items, mx, my) "
    , "  local labels = {}; "
    , "  for i, it in ipairs(items) do "
    , "    labels[i] = it.label; "
    , "    if it.label == 'Transfer' then _G.__transferCallback = it.callback end "
    , "  end; "
    , "  _G.__lastLabels = labels; "
    , "end; "
    , "faction.isPlayerCommandable = function(f) return f == 'player' end; "
    , "faction.canAttack = function(a, b) return true end; "
    , "unit.exists = function(uid) return true end; "
    , "unit.getInfo = function(uid) return { gridX = 1, gridY = 2 } end; "
    , "unit.getFaction = function(uid) return 'player' end; "
    , "unit.getWounds = function(uid) return {} end; "
    , "unit.getKnowledge = function(uid, k) return false end; "
    -- Captured BEFORE the stub replaces it, so the widening cases
    -- below can put the REAL engine query back (see liveEndpointInfo).
    , "_G.__realTransferEndpointInfo = unit.transferEndpointInfo; "
    , "unit.transferEndpointInfo = function(ep) return nil end; "
    ]

-- | @unit.getSelected()@ returning exactly this fixed uid list.
selectedStub ∷ [Int] → Text
selectedStub uids = "unit.getSelected = function() return {"
    <> T.intercalate "," (map (T.pack . show) uids) <> "} end; "

-- | @unit.transferEndpointInfo({ kind, id })@ answering ONLY for this
-- one endpoint -- every other endpoint falls through to baseSetupLua's
-- default nil, matching a genuinely stale/missing target.
endpointInfoStub ∷ Text → Int → Bool → Text → Text
endpointInfoStub kind rid eligible displayName = T.concat
    [ "unit.transferEndpointInfo = function(ep) "
    , "  if ep and ep.kind == '", kind, "' and ep.id == "
    , T.pack (show rid), " then "
    , "    return { eligible = ", if eligible then "true" else "false"
    , ", displayName = '", displayName, "', page = 'p', gridX = 10, gridY = 20"
    , ", capacity = 200, storedWeight = 0, contents = {} } "
    , "  end; return nil end; "
    ]

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

-- | @unit.hitTestAt@ fixed to this target uid.
unitTargetStub ∷ Int → Text
unitTargetStub uid = "unit.hitTestAt = function(x, y) return "
    <> T.pack (show uid) <> " end; "

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
