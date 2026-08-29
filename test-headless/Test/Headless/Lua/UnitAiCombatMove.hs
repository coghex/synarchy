{-# LANGUAGE Strict #-}
-- | Player move-order arbitration during combat (#916). Runs the shipped
--   @unit_ai_combat_move.lua@ in a bare Lua VM and pins the exact utility
--   ladder, the committed/high-confidence exceptions, and the two-second
--   proof that an impossible escape yields once with one warning.
module Test.Headless.Lua.UnitAiCombatMove (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    maybe (pure ()) (expectationFailure . T.unpack) result

lns ∷ [Text] → Text
lns = T.intercalate "\n"

prelude ∷ Text
prelude = lns
    [ "NOW, WARNINGS, STOPS, MOVES, CLEARS = 0, 0, 0, 0, 0"
    , "POS = { [1] = { gridX=0, gridY=0, page='world' },"
    , "        [2] = { gridX=2, gridY=0, page='world' } }"
    , "POSE = { [1]='standing', [2]='standing' }"
    , "EFFECT = { [1]=1, [2]=1 }"
    , "engine = { gameTime=function() return NOW end,"
    , "  emitEventForUnit=function(cat)"
    , "    if cat == 'unit_warning' then WARNINGS=WARNINGS+1 end end }"
    , "unit = { exists=function(uid) return POS[uid] ~= nil end,"
    , "  getPose=function(uid) return POSE[uid] end,"
    , "  getInfo=function(uid) return POS[uid] end,"
    , "  getAttackRange=function() return 1.5 end,"
    , "  getActivity=function() return ACTIVITY or 'idle' end,"
    , "  clearAnimOverride=function() CLEARS=CLEARS+1 end,"
    , "  stop=function() STOPS=STOPS+1 end,"
    , "  moveTo=function() MOVES=MOVES+1 end }"
    , "package.loaded['scripts.unit_ai'] = {"
    , "  combatEffectiveness=function(uid) return EFFECT[uid] end }"
    , "package.loaded['scripts.unit_ai_core'] = {"
    , "  isGoalActive=function(s,name) return s.activeGoal == name end,"
    , "  reportFailure=function() WARNINGS=WARNINGS+1 end }"
    , "package.loaded['scripts.unit_ai_pace'] = {"
    , "  initialPaceMode=function() return 'push' end,"
    , "  paceSpeed=function() return 1.0 end }"
    , "package.loaded['scripts.unit_ai_stall'] = { noteWalk=function() end }"
    , "local move = require('scripts.unit_ai_combat_move')"
    ]

spec ∷ Spec
spec = describe "player move orders during combat (#916)" $ do
    it "scores a human move at 9 above engage/attack 8, while ordinary \
       \and internal moves remain in the hold band at 7" $
        runsOk $ lns
            [ prelude
            , "local combat = { activeGoal='attack', attackTargetUid=2 }"
            , "combat.commandedTask = { x=10,y=0,player=true }"
            , "assert(move.followCommandUtility(1,combat) == 9)"
            , "assert(move.followCommandUtility(1,combat) > 8)"
            , "combat.commandedTask.player = nil"
            , "assert(move.followCommandUtility(1,combat) == 7)"
            , "local ordinary = { commandedTask={x=10,y=0,player=true} }"
            , "assert(move.followCommandUtility(1,ordinary) == 7)"
            ]

    it "lets committed swings and transitions finish, then releases the \
       \combat animation before movement takes control" $
        runsOk $ lns
            [ prelude
            , "local s = { activeGoal='attack', attackTargetUid=2,"
            , "  commandedTask={x=10,y=0,player=true}, attackSwingUntil=1 }"
            , "assert(move.followCommandUtility(1,s) == 7)"
            , "s.attackSwingUntil=0; s.lungePhase='air'"
            , "assert(move.followCommandUtility(1,s) == 7)"
            , "s.lungePhase=nil; ACTIVITY='transitioning'"
            , "assert(move.followCommandUtility(1,s) == 7)"
            , "ACTIVITY='idle'"
            , "assert(move.followCommandUtility(1,s) == 9)"
            , "move.followCommandExecute(1,s)"
            , "assert(CLEARS == 1 and s.commandedTask.combatMoveControlAt == 0)"
            , "NOW=2; POS[2].gridX=1; EFFECT[1]=4; EFFECT[2]=1"
            , "assert(move.followCommandUtility(1,s) == 7)"
            , "EFFECT[1]=3.99"
            , "assert(move.followCommandUtility(1,s) == 9)"
            , "POS[2].gridX=2"
            , "assert(move.followCommandUtility(1,s) == 9)"
            ]

    it "does not steal an active retreat: retreat keeps its own score \
       \above the ordinary move's 7" $
        runsOk $ lns
            [ prelude
            , "local s = { activeGoal='retreat', attackTargetUid=2,"
            , "  commandedTask={x=10,y=0,player=true} }"
            , "assert(move.followCommandUtility(1,s) == 7)"
            , "assert(move.followCommandUtility(1,s) < 8)"
            ]

    it "abandons an impossible combat move after two seconds without \
       \displacement, warns exactly once, and releases combat to resume" $
        runsOk $ lns
            [ prelude
            , "local s = { activeGoal='attack', attackTargetUid=2,"
            , "  commandedTask={x=10,y=0,player=true} }"
            , "assert(move.followCommandUtility(1,s) == 9)"
            , "move.followCommandExecute(1,s)"
            , "assert(MOVES == 1 and s.commandedTask.combatMoveControlAt == 0)"
            , "NOW=1.99; assert(move.followCommandUtility(1,s) == 9)"
            , "NOW=2.0; assert(move.followCommandUtility(1,s) == -math.huge)"
            , "assert(s.commandedTask == nil and WARNINGS == 1 and STOPS == 1)"
            , "assert(move.followCommandUtility(1,s) == -math.huge)"
            , "assert(WARNINGS == 1, 'the failed episode warns once')"
            ]

    it "accepts demonstrated displacement and starts a fresh proof window" $
        runsOk $ lns
            [ prelude
            , "local s = { activeGoal='attack', attackTargetUid=2,"
            , "  commandedTask={x=10,y=0,player=true} }"
            , "move.followCommandExecute(1,s)"
            , "POS[1].gridX=0.2; NOW=2"
            , "assert(move.followCommandUtility(1,s) == 9)"
            , "assert(s.commandedTask ~= nil)"
            , "assert(s.commandedTask.combatMoveControlAt == nil)"
            ]
