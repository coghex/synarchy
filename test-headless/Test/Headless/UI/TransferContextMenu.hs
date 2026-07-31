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
--   'unit.transferReceiverInfo', 'unit.getSelected', ...) are stubbed
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
import Data.Aeson (FromJSON(..), decode, withObject, (.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))

spec ∷ SpecWith EngineEnv
spec = describe "Transfer context menu" $ do
    describe "building receiver (scripts/init_context_menu.lua tryBuildingMenu)" $ do
        it "one selected acolyte + built storage building -> Transfer appears" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 501 "built" 200 [] <> selectedStub [7]
                    <> receiverInfoStub "building" 501 True "Cargo Hold")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Transfer"
            labels `shouldSatisfy` T.isInfixOf "Contents"

        it "an under-construction storage building -> Transfer does not appear (no menu at all)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 502 "under_construction" 200 [] <> selectedStub [7]
                    <> receiverInfoStub "building" 502 False "")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "false"

        it "a Built station with zero storage capacity -> Transfer does not appear (Bills still does)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 503 "built" 0 ["smelt"] <> selectedStub [7]
                    <> receiverInfoStub "building" 503 False "")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Bills"
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

        it "no selected source -> Transfer does not appear" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 504 "built" 200 [] <> selectedStub []
                    <> receiverInfoStub "building" 504 True "Cargo Hold")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Contents"
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

        it "multiple selected sources -> no silent source choice (Transfer omitted)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 505 "built" 200 [] <> selectedStub [7, 8]
                    <> receiverInfoStub "building" 505 True "Cargo Hold")
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
                    <> receiverInfoStub "building" 506 False "")
            claimed1 ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed1 `shouldBe` "true"
            labels1 ← capturedLabels ls
            labels1 `shouldSatisfy` T.isInfixOf "Contents"
            labels1 `shouldNotSatisfy` T.isInfixOf "Transfer"

            -- Same capacity, same activity -- only the query's answer
            -- flips, and now Transfer appears.
            run ls (receiverInfoStub "building" 506 True "Cargo Hold")
            claimed2 ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed2 `shouldBe` "true"
            labels2 ← capturedLabels ls
            labels2 `shouldSatisfy` T.isInfixOf "Transfer"

    describe "unit receiver (scripts/init_context_menu.lua tryUnitMenu)" $ do
        it "one selected acolyte + technomule -> Transfer appears (alongside Info/Attack)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 99 <> selectedStub [7]
                    <> receiverInfoStub "unit" 99 True "Technomule")
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
                    <> receiverInfoStub "unit" 42 False "")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldSatisfy` T.isInfixOf "Info"
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

        it "selecting only the target itself -> no self-transfer" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (unitTargetStub 99 <> selectedStub [99]
                    <> receiverInfoStub "unit" 99 True "Technomule")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
            claimed `shouldBe` "true"
            labels ← capturedLabels ls
            labels `shouldNotSatisfy` T.isInfixOf "Transfer"

    describe "session creation (scripts/transfer_session.lua)" $ do
        it "selecting Transfer creates a session with the exact source/receiver identities" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (buildingStub 507 "built" 200 [] <> selectedStub [7]
                    <> receiverInfoStub "building" 507 True "Cargo Hold")
            claimed ← evalDebug ls "return require('scripts.init_context_menu').tryBuildingMenu(10, 20)"
            claimed `shouldBe` "true"
            invoke ← evalDebug ls "_G.__transferCallback(); return 'ok'"
            invoke `shouldNotSatisfy` isLuaError
            r ← evalDebug ls "return require('scripts.transfer_session').get()"
            sess ← decodeOr r
            spSourceUid sess `shouldBe` 7
            spReceiverKind sess `shouldBe` "building"
            spReceiverId sess `shouldBe` 507
            spReceiverDisplayName sess `shouldBe` "Cargo Hold"
            spContractOperation sess `shouldBe` "unit_to_building_storage"
            spContractState sess `shouldBe` "queued"

        it "is reusable independent of the context-menu callback (requirement 8)" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            run ls (receiverInfoStub "unit" 99 True "Technomule")
            -- Calls transfer_session.create directly -- no
            -- init_context_menu, no tryUnitMenu, no click simulation.
            r ← evalDebug ls
                "local s = require('scripts.transfer_session').create(7, 'unit', 99); return s"
            sess ← decodeOr r
            spSourceUid sess `shouldBe` 7
            spReceiverKind sess `shouldBe` "unit"
            spReceiverId sess `shouldBe` 99
            spContractOperation sess `shouldBe` "unit_to_unit_inventory"

        it "a stale/missing receiver yields no session and mutates nothing" $ \env → do
            ls ← newBareLuaBackend env
            run ls baseSetupLua
            -- No unit.transferReceiverInfo stub for id 999 -> the
            -- shared base stub returns nil for any unrecognised id
            -- (see receiverInfoStub / baseSetupLua's default).
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
            run ls (receiverInfoStub "building" 508 False "")
            r ← evalDebug ls
                "local s, reason = require('scripts.transfer_session').create(7, 'building', 508); return reason"
            r `shouldBe` "\"receiver_ineligible\""
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
-- unit.transferReceiverInfo/unit.exists/unit.getInfo/unit.getFaction
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
    , "unit.transferReceiverInfo = function(kind, id) return nil end; "
    ]

-- | @unit.getSelected()@ returning exactly this fixed uid list.
selectedStub ∷ [Int] → Text
selectedStub uids = "unit.getSelected = function() return {"
    <> T.intercalate "," (map (T.pack . show) uids) <> "} end; "

-- | @unit.transferReceiverInfo(kind, id)@ answering ONLY for this one
-- (kind, id) pair -- every other pair falls through to
-- baseSetupLua's default nil, matching a genuinely stale/missing
-- target.
receiverInfoStub ∷ Text → Int → Bool → Text → Text
receiverInfoStub kind rid eligible displayName = T.concat
    [ "unit.transferReceiverInfo = function(k, id) "
    , "  if k == '", kind, "' and id == ", T.pack (show rid), " then "
    , "    return { eligible = ", if eligible then "true" else "false"
    , ", displayName = '", displayName, "', page = 'p', gridX = 10, gridY = 20 } "
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

-- | The joined labels string captured by the spy above, or "" if
-- contextMenu.show was never called (the menu didn't open at all).
capturedLabels ∷ LuaBackendState → IO Text
capturedLabels ls = evalDebug ls
    "return _G.__lastLabels and table.concat(_G.__lastLabels, ',') or ''"

-- * Decoded probe shape

data SessionProbe = SessionProbe
    { spSourceUid           ∷ Int
    , spReceiverKind        ∷ Text
    , spReceiverId          ∷ Int
    , spReceiverDisplayName ∷ Text
    , spContractOperation   ∷ Text
    , spContractState       ∷ Text
    } deriving (Show, Eq)

instance FromJSON SessionProbe where
    parseJSON = withObject "SessionProbe" $ \o → do
        contractObj ← o .: "contract"
        SessionProbe
            <$> o .: "sourceUid"
            <*> o .: "receiverKind"
            <*> o .: "receiverId"
            <*> o .: "receiverDisplayName"
            <*> contractObj .: "operation"
            <*> contractObj .: "state"

decodeOr ∷ FromJSON a ⇒ Text → IO a
decodeOr t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just v  → pure v
    Nothing → do
        expectationFailure ("failed to decode: " ⧺ T.unpack t)
        fail "unreachable"
