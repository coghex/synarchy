{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TypeApplications #-}
-- | The "Lua faction model" gate (#912): the @faction@ global as unit AI
--   actually sees it, plus the one AI consumer whose meaning changed —
--   @unitAi.groupEffectivenessVs@.
--
--   Same pattern as 'Test.Headless.Lua.SaveModules': a standalone Lua VM
--   (no engine, no world/unit threads) with the REAL
--   'Engine.Scripting.Lua.API.Register.Faction.registerFactionAPI' table
--   installed — it needs no 'Engine.Core.State.EngineEnv', which is
--   exactly why it can be exercised here. Assertions live inside Lua via
--   @assert()@; a non-OK 'Lua.Status' becomes an hspec failure carrying
--   the Lua message.
--
--   The relation/property TABLE itself is pinned in
--   'Test.Headless.Unit.Faction'; what this gate adds is that Lua reads
--   the same answers, and that the scripts consuming them behave.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua faction model"'@.
module Test.Headless.Lua.Faction (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Engine.Scripting.Lua.API.Register.Faction (registerFactionAPI)

-- | Run one self-contained Lua chunk in a fresh interpreter with the
--   stdlib and the real @faction@ table, plus whatever extra globals the
--   case installs first (as Lua source, so a stub can be a table
--   literal).
runsOkWith ∷ Text → Text → Expectation
runsOkWith preludeText chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        registerFactionAPI
        _ ← Lua.dostring (TE.encodeUtf8 preludeText)
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

runsOk ∷ Text → Expectation
runsOk = runsOkWith ""

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | A stub @unit@ table plus the @engine@/@world@ globals
--   @unit_ai_core.lua@ touches, driven by a @UNITS@ table each case
--   fills in: @{ [uid] = { faction=…, gridX=…, gridY=… } }@. Enough for
--   'unitAi.groupEffectivenessVs', which reads exists/getInfo/
--   getFaction/getAllIds and nothing else.
--
--   @unitAi.combatEffectiveness@ is REPLACED with a per-unit constant so
--   the assertion is about which units are counted — the question this
--   gate exists to answer — rather than about the strength heuristic,
--   which has its own coverage.
aiPrelude ∷ Text
aiPrelude = lns
    [ "UNITS = {}"
    , "ORDER = {}"
    , "unit = {"
    , "  exists = function(uid) return UNITS[uid] ~= nil end,"
    , "  getAllIds = function() return ORDER end,"
    , "  getFaction = function(uid)"
    , "      local u = UNITS[uid]; return u and u.faction or nil end,"
    , "  getInfo = function(uid)"
    , "      local u = UNITS[uid]"
    , "      if not u then return nil end"
    , "      return { gridX = u.gridX, gridY = u.gridY } end,"
    , "}"
    , "engine = { emitEventForUnit = function() end,"
    , "           logWarn = function() end }"
    , "world = {}"
      -- unit_ai_core extends the singleton the real unit_ai.lua registers
      -- before requiring it; stand that singleton up the same way.
    , "package.loaded['scripts.unit_ai'] = {}"
    , "local core = require('scripts.unit_ai_core')"
    , "unitAi = package.loaded['scripts.unit_ai']"
    , "aiState = unitAi.aiState"
    , "unitAi.combatEffectiveness = function(uid)"
    , "    local u = UNITS[uid]; return u and u.power or 0 end"
    , "function addUnit(uid, faction, gx, gy, power)"
    , "    UNITS[uid] = { faction = faction, gridX = gx, gridY = gy,"
    , "                   power = power }"
    , "    ORDER[#ORDER + 1] = uid"
    , "end"
    , "function commitTo(uid, targetUid)"
    , "    aiState[uid] = aiState[uid] or {}"
    , "    aiState[uid].attackTargetUid = targetUid"
    , "end"
    ]

spec ∷ Spec
spec = describe "Lua faction model" $ do

    describe "faction properties" $ do
        it "only player is player-owned; debug notably is not" $ runsOk $ lns
            [ "assert(faction.isPlayerOwned('player') == true)"
            , "assert(faction.isPlayerOwned('debug') == false)"
            , "assert(faction.isPlayerOwned('wildlife') == false)"
            , "assert(faction.isPlayerOwned('hostile') == false)"
            , "assert(faction.isPlayerOwned('neutral') == false)"
            ]

        it "player and debug are player-commandable" $ runsOk $ lns
            [ "assert(faction.isPlayerCommandable('player') == true)"
            , "assert(faction.isPlayerCommandable('debug') == true)"
            , "assert(faction.isPlayerCommandable('wildlife') == false)"
            , "assert(faction.isPlayerCommandable('hostile') == false)"
            , "assert(faction.isPlayerCommandable('neutral') == false)"
            ]

        it "only debug has unrestricted combat" $ runsOk $ lns
            [ "assert(faction.hasUnrestrictedCombat('debug') == true)"
            , "for _, f in ipairs({'player','wildlife','hostile','neutral'}) do"
            , "  assert(faction.hasUnrestrictedCombat(f) == false, f)"
            , "end"
            ]

    describe "faction relation" $ do
        it "player and debug are allied; player and wildlife are hostile" $
            runsOk $ lns
            [ "assert(faction.relation('player', 'debug') == 'ally')"
            , "assert(faction.relation('debug', 'player') == 'ally')"
            , "assert(faction.areAllies('player', 'debug') == true)"
            , "assert(faction.relation('player', 'wildlife') == 'hostile')"
            , "assert(faction.relation('wildlife', 'player') == 'hostile')"
            , "assert(faction.areAllies('player', 'wildlife') == false)"
            ]

        it "a faction is allied with itself, and neutral pairs are neutral" $
            runsOk $ lns
            [ "for _, f in ipairs({'player','wildlife','hostile','neutral','debug'}) do"
            , "  assert(faction.relation(f, f) == 'ally', f)"
            , "end"
            , "assert(faction.relation('neutral', 'player') == 'neutral')"
            , "assert(faction.relation('neutral', 'hostile') == 'neutral')"
            ]

        it "an unrecognized tag reads as the engine's neutral fallback, \
           \so Lua and Haskell can't disagree about it" $ runsOk $ lns
            [ "assert(faction.relation('made_up', 'player') == 'neutral')"
            , "assert(faction.isPlayerOwned('made_up') == false)"
            , "assert(faction.isPlayerCommandable('made_up') == false)"
            , "assert(faction.canAttack('player', 'made_up') == false)"
              -- a nil faction (destroyed unit) degrades, never errors
            , "assert(faction.isPlayerOwned(nil) == false)"
            , "assert(faction.relation(nil, nil) == 'ally')"
            ]

    describe "attack permission" $
        it "preserves exactly the directions the context menu allows today" $
            runsOk $ lns
            [ "assert(faction.canAttack('player', 'debug') == true)"
            , "assert(faction.canAttack('debug', 'player') == true)"
            , "assert(faction.canAttack('debug', 'debug') == true)"
            , "assert(faction.canAttack('player', 'wildlife') == true)"
            , "assert(faction.canAttack('player', 'hostile') == true)"
            , "assert(faction.canAttack('player', 'player') == false)"
            ]

    describe "groupEffectivenessVs (unit_ai_core)" $ do
        it "counts the subject and its allies, and nobody else — a \
           \third faction fighting the same threat is not our side" $
            runsOkWith aiPrelude $ lns
            [ "addUnit(1, 'player',   0, 0, 10)"   -- subject, near threat
            , "addUnit(2, 'player',   1, 1, 20)"   -- ally, near threat
            , "addUnit(3, 'debug',    1, 0, 40)"   -- ally (player↔debug)
            , "addUnit(4, 'hostile',  1, 1, 80)"   -- third faction, near
            , "addUnit(9, 'wildlife', 0, 0, 999)"  -- the threat itself
              -- The hostile unit is ALSO attacking the threat. Under the
              -- old \"not the threat's faction\" rule it counted toward
              -- our swarm; it is not our side.
            , "commitTo(4, 9)"
            , "local eff = unitAi.groupEffectivenessVs(9, 1)"
            , "assert(eff == 70, 'expected 70, got ' .. tostring(eff))"
            ]

        it "excludes the threat itself even when it is an ally of the \
           \subject — the staged same-faction debug fight" $
            runsOkWith aiPrelude $ lns
            [ "addUnit(1, 'debug', 0, 0, 10)"
            , "addUnit(2, 'debug', 0, 0, 25)"   -- the opponent, allied
            , "local eff = unitAi.groupEffectivenessVs(2, 1)"
            , "assert(eff == 10, 'expected 10, got ' .. tostring(eff))"
            ]

        it "still counts a committed ally that is out of rally range" $
            runsOkWith aiPrelude $ lns
            [ "addUnit(1, 'player', 0, 0, 10)"
            , "addUnit(2, 'player', 500, 500, 7)"  -- far, but committed
            , "addUnit(9, 'wildlife', 0, 0, 999)"
            , "commitTo(2, 9)"
            , "local eff = unitAi.groupEffectivenessVs(9, 1)"
            , "assert(eff == 17, 'expected 17, got ' .. tostring(eff))"
            ]

        it "ignores an uncommitted ally standing outside rally range" $
            runsOkWith aiPrelude $ lns
            [ "addUnit(1, 'player', 0, 0, 10)"
            , "addUnit(2, 'player', 500, 500, 7)"
            , "addUnit(9, 'wildlife', 0, 0, 999)"
            , "local eff = unitAi.groupEffectivenessVs(9, 1)"
            , "assert(eff == 10, 'expected 10, got ' .. tostring(eff))"
            ]

        it "is zero without a subject, rather than silently measuring \
           \everyone who isn't the threat" $
            runsOkWith aiPrelude $ lns
            [ "addUnit(1, 'player', 0, 0, 10)"
            , "addUnit(9, 'wildlife', 0, 0, 999)"
            , "assert(unitAi.groupEffectivenessVs(9) == 0)"
            , "assert(unitAi.groupEffectivenessVs(9, 404) == 0)"
            ]
