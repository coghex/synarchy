{-# LANGUAGE TypeApplications #-}
-- | The "Lua faction model" gate (#912): the string-taking @faction@
--   global, whose answers stay exactly as they were for the
--   compatibility window.
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
--   the same answers. These string-taking verbs are the compatibility
--   window: since #2521 no script calls them, and the consuming
--   scripts are exercised through the unit-id verbs with real units in
--   "Test.Headless.Lua.DirectedRelations" (same gate name).
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua faction model"'@.
module Test.Headless.Lua.Faction (spec) where

import UPrelude
import Engine.Scripting.Lua.CallStats (newLuaCallStats)
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
        callStats ← Lua.liftIO newLuaCallStats
        registerFactionAPI callStats
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
