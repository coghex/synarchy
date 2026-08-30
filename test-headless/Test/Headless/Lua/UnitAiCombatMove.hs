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
    [ "NOW, WARNINGS, STOPS, MOVES, CLEARS, ATTACKS = 0, 0, 0, 0, 0, 0"
    , "POS = { [1] = { gridX=0, gridY=0, page='world' },"
    , "        [2] = { gridX=2, gridY=0, page='world' } }"
    , "POSE = { [1]='standing', [2]='standing' }"
    , "EFFECT = { [1]=1, [2]=1 }"
    , "engine = { gameTime=function() return NOW end, logDebug=function() end,"
    , "  emitEventForUnit=function(cat)"
    , "    if cat == 'unit_warning' then WARNINGS=WARNINGS+1 end end }"
    , "unit = { exists=function(uid) return POS[uid] ~= nil end,"
    , "  getPose=function(uid) return POSE[uid] end,"
    , "  getInfo=function(uid) return POS[uid] end,"
    , "  getAttackRange=function() return 1.5 end,"
    , "  getLastAttacker=function() return LAST_ATTACKER end,"
    , "  getActivity=function() return ACTIVITY or 'idle' end,"
    , "  clearAnimOverride=function() CLEARS=CLEARS+1 end,"
    , "  stop=function() STOPS=STOPS+1 end,"
    , "  moveTo=function() MOVES=MOVES+1 end }"
    , "CURRENT_STATE=nil"
    , "package.loaded['scripts.unit_ai'] = {"
    , "  combatEffectiveness=function(uid) return EFFECT[uid] end,"
    , "  commandAttack=function(_,target) ATTACKS=ATTACKS+1"
    , "    CURRENT_STATE.activeGoal='attack'; CURRENT_STATE.attackTargetUid=target"
    , "  end }"
    , "package.loaded['scripts.unit_ai_core'] = {"
    , "  isGoalActive=function(s,name) return s.activeGoal == name end,"
    , "  markGoalAccomplished=function(s,name)"
    , "    s.goalStatus=s.goalStatus or {}; s.goalStatus[name]='accomplished'"
    , "    if s.activeGoal == name then s.activeGoal=nil end end,"
    , "  reportFailure=function() WARNINGS=WARNINGS+1 end }"
    , "package.loaded['scripts.unit_ai_pace'] = {"
    , "  initialPaceMode=function() return 'push' end,"
    , "  paceSpeed=function() return 1.0 end }"
    , "package.loaded['scripts.movement_speed'] = {"
    , "  comfort=function() return 1.0 end }"
    , "LUNGE_CLEARS=0"
    , "package.loaded['scripts.unit_ai_combat_lunge'] = { clear=function()"
    , "  LUNGE_CLEARS=LUNGE_CLEARS+1 end }"
    , "local stall = require('scripts.unit_ai_stall')"
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
            , "assert(s.activeGoal == 'attack' and s.attackTargetUid == 2,"
            , "  'failed movement proof must release the original combat')"
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

    it "holds after arrival despite the pre-withdrawal hit, then re-engages on a new hit" $
        runsOk $ lns
            [ prelude
            , "local hold = require('scripts.unit_ai_hold')"
            , "local combat = require('scripts.unit_ai_combat')"
            , "local s = { activeGoal='attack', attackTargetUid=2, committed=true,"
            , "  attackLastMoveTo={x=2,y=0}, goalStatus={attack='in_progress'},"
            , "  commandedTask={x=10,y=0,player=true} }"
            , "CURRENT_STATE=s; LAST_ATTACKER={uid=2,at=3}"
            , "assert(move.followCommandUtility(1,s) == 9)"
            , "move.followCommandExecute(1,s)"
            , "assert(s.commandedTask.combatWithdrawal == true)"
            , "POS[1].gridX=0.2; NOW=2"
            , "assert(move.followCommandUtility(1,s) == 9)"
            , "assert(s.commandedTask.combatMoveControlAt == nil)"
            , "POS[1].gridX=10; NOW=9"
            , "local completed = stall.maintainTask(1,s)"
            , "move.completeCommandedTask(1,s,completed)"
            , "assert(s.commandedTask == nil and s.holdAnchor.x == 10)"
            , "assert(s.holdAnchor.combatWithdrawalCompletedAt == 9)"
            , "assert(s.attackTargetUid == nil and s.committed == nil)"
            , "assert(s.attackLastMoveTo == nil and s.activeGoal == nil)"
            , "assert(s.goalStatus.attack == 'accomplished' and LUNGE_CLEARS == 1)"
            , "local candidates = {"
            , "  {name='engage', utility=combat.engageUtility, execute=combat.engageExecute},"
            , "  {name='hold_position', utility=hold.holdUtility, execute=hold.holdExecute} }"
            , "local function arbitrate()"
            , "  local best, score = nil, -math.huge"
            , "  for _,a in ipairs(candidates) do local n=a.utility(1,s,{})"
            , "    if n > score then best,score=a,n end end"
            , "  best.execute(1,s,{}); return best.name"
            , "end"
            , "assert(arbitrate() == 'hold_position',"
            , "  'a hit six seconds before arrival must not override the hold')"
            , "assert(STOPS == 2, 'handoff stops once, then hold maintains position')"
            , "LAST_ATTACKER={uid=2,at=9.1}; NOW=9.1"
            , "assert(arbitrate() == 'engage', 'a post-arrival hit must reopen combat')"
            , "assert(ATTACKS == 1 and s.activeGoal == 'attack'"
            , "  and s.attackTargetUid == 2)"
            ]
