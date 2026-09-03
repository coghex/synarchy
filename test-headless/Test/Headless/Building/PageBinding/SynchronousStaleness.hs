{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | SYNCHRONOUS staleness for the "Build placement page binding"
--   (#1602) gate: a page-selection change landing at an exact point
--   INSIDE one @handleMouseDown@ call — between the pick and the
--   validation, or between the validation and the commit — must cost
--   the placement, on the starting-building branch, the
--   @construction.designate@ branch and the persistent remote-warning
--   confirmation (#844) alike.
--
--   The two @__pageSwitch@-driving Lua wrappers below are what choose
--   WHEN the switch happens; the binding they hand on is always the one
--   the engine's own pick produced. They are private to this owner —
--   only these examples land a switch mid-call.
--
--   These are fixture-CONSUMING fragments: the engine, the Lua backend
--   and the isolated resource root are the façade's
--   ("Test.Headless.Building.PageBinding").
module Test.Headless.Building.PageBinding.SynchronousStaleness
    ( staleSpec
    ) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import qualified Data.Text as T

import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Building.PageBinding.Support
    ( aimAt, armBuildTool, clearStubs, clickAt, commitOutcomes
    , committedPlacements, designationKeys, evalDebug, expectStale, pageA
    , placeTile, portalName, resetScene, selectionGen, shedName
    , terrainZA )
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..))

-- | Wrap @world.pickTile@ so a page-selection change lands between the
--   REAL pick and the validation that follows it. The binding handed to
--   the validation is the one the engine's own pick produced — the stub
--   only chooses WHEN the switch happens, never what the token is.
stubPickThenSwitch ∷ LuaBackendState → Text → IO Text
stubPickThenSwitch ls mode = evalDebug ls $ T.concat
    [ "world.pickTile = function(px, py) "
    , "  local gx, gy, gz, page, gen = _G.__realPickTile(px, py); "
    , "  if gx then __pageSwitch('", mode, "') end; "
    , "  return gx, gy, gz, page, gen "
    , "end; return 'stubbed'" ]

-- | Wrap @building.canPlaceAt@ so the change lands between validation
--   and commit instead. The validation itself runs for real, against
--   the still-current page, and answers before the switch.
stubValidateThenSwitch ∷ LuaBackendState → Text → IO Text
stubValidateThenSwitch ls mode = evalDebug ls $ T.concat
    [ "building.canPlaceAt = function(...) "
    , "  local ok, why, stale = _G.__realCanPlaceAt(...); "
    , "  __pageSwitch('", mode, "'); "
    , "  return ok, why, stale "
    , "end; return 'stubbed'" ]
-- | Every drained outcome as @kind|outcome|reason@ — the remote-warning
--   scenarios need the kind too.
allOutcomes ∷ LuaBackendState → IO [Text]
allOutcomes ls = do
    raw ← evalDebug ls $ T.concat
        [ "local rows = {}; "
        , "for _, o in ipairs(debug.drainActionOutcomes() or {}) do "
        , "  rows[#rows+1] = tostring(o.kind) .. '|' .. tostring(o.outcome) "
        , "    .. '|' .. tostring(o.reason) end; "
        , "return table.concat(rows, ';')" ]
    pure $ filter (not . T.null) (T.splitOn ";" raw)
staleSpec ∷ SpecWith (EngineEnv, LuaBackendState)
staleSpec = describe "a moved page selection rejects the placement" $ do

  describe "the starting-building branch (#1602 r5)" $ do

    it "commits exactly once on the captured page when nothing moves" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls portalName True
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            outs ← commitOutcomes ls
            outs `shouldBe` ["accepted|nil"]
            committedPlacements env `shouldReturn`
                [(portalName, fst placeTile, snd placeTile, pageA)]
            designationKeys wsA wsB `shouldReturn` ([], [])

    it "rejects a switch between the pick and the validation" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls portalName True
            _ ← stubPickThenSwitch ls "toB"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "rejects a switch between the validation and the commit" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls portalName True
            _ ← stubValidateThenSwitch ls "toB"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "rejects an A→B→A switch despite the page id matching" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls portalName True
            _ ← stubPickThenSwitch ls "aba"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            mgr ← readIORef (worldManagerRef env)
            wmVisible mgr `shouldBe` [pageA]
            expectStale env wsA wsB ls

  describe "the construction.designate branch (#1602 r6)" $ do

    it "designates on the captured page when nothing moves" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            outs ← commitOutcomes ls
            outs `shouldBe` ["accepted|routed to construction.designate"]
            -- Applying the world queue turns the enqueued designation
            -- into a real one on the captured page, and only there.
            committedPlacements env `shouldReturn` []
            designationKeys wsA wsB `shouldReturn` ([placeTile], [])

    it "rejects a switch between the pick and the validation" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            _ ← stubPickThenSwitch ls "toB"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "rejects a switch between the validation and the designation" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            _ ← stubValidateThenSwitch ls "toB"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "rejects an A→B→A switch despite the page id matching" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            _ ← stubValidateThenSwitch ls "aba"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

  describe "the remote-warning confirmation branch (#1602 r5)" $ do

    it "carries the ORIGINAL click binding and refuses a stale \
       \confirmation without any commitPlacement record" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "local rw = require('scripts.build_tool_remote_warning'); "
            , "rw.init(0, 0, 0, 1280, 720); "
            , "rw.open('", portalName, "', ", tshow (fst placeTile), ", "
            , tshow (snd placeTile), ", nil, 128, '", unWorldPageId pageA
            , "', ", tshow gen, "); return tostring(rw.isOpen())" ]
        _ ← allOutcomes ls   -- discard the 'presented' record
        _ ← evalDebug ls "__pageSwitch('aba'); return 'switched'"
        _ ← evalDebug ls
            "local rw = require('scripts.build_tool_remote_warning'); \
            \rw.establishHere(); return tostring(rw.isOpen())"
        outs ← allOutcomes ls
        outs `shouldBe`
            [ "buildTool.remoteWarning|confirmed|nil"
            , "buildTool.remoteWarning|revalidationRejected|page binding changed" ]
        committedPlacements env `shouldReturn` []
        designationKeys wsA wsB `shouldReturn` ([], [])
