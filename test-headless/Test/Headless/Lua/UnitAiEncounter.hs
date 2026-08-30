{-# LANGUAGE Strict #-}
-- | Persistent ruin-guard behavior (#916). Runs the shipped encounter AI in
--   a bare Lua VM and pins exact hostility, deterministic target selection,
--   leash/memory disengagement, return-home suppression, and episode notices.
module Test.Headless.Lua.UnitAiEncounter (spec) where

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
    [ "NOW, EVENTS, STOPS, MOVES, CLEARS = 0, 0, 0, 0, 0"
    , "EVENT_MESSAGES, LAST_ATTACKER = {}, nil"
    , "COMMAND, STATES, EPISODES = nil, {}, {}"
    , "VISIBLE = { {x=5,y=5}, {x=7,y=5} }"
    , "IDS = {3,1,2}"
    , "INFO = {"
    , "  [1]={gridX=5,gridY=5,page='world',name='guard'},"
    , "  [2]={gridX=5,gridY=5,page='world',name='near'},"
    , "  [3]={gridX=7,gridY=5,page='world',name='far'} }"
    , "POSE = { [1]='standing', [2]='standing', [3]='standing' }"
    , "REL = { [2]='hostile', [3]='hostile' }"
    , "OCCUPANT = { uid=1, home_x=5, home_y=5,"
    , "  engaged=false, returning=false }"
    , "LOCATION = { instance_id=41, id='ruin_small', name='Old Ruin',"
    , "  lifecycle='discovered', discovered=true,"
    , "  bounds={min_x=0,max_x=10,min_y=0,max_y=10},"
    , "  encounter={cleared=false,activated=false,episode_active=false,"
    , "    aggression_announced=false,disengage_announced=false,"
    , "    occupants={OCCUPANT}} }"
    , "engine = { gameTime=function() return NOW end,"
    , "  emitEventForUnit=function(_,message,uid,x,y) EVENTS=EVENTS+1;"
    , "    table.insert(EVENT_MESSAGES,{message=message,uid=uid,x=x,y=y}) end }"
    , "world = { listPlacedLocations=function() return {LOCATION} end,"
    , "  setLocationEncounterOccupantState=function(i,u,e,r,p)"
    , "    table.insert(STATES,{i=i,u=u,e=e,r=r,p=p}) end,"
    , "  setLocationEncounterEpisodeState=function(i,a,g,d,p)"
    , "    table.insert(EPISODES,{i=i,a=a,g=g,d=d,p=p}) end }"
    , "faction = { relation=function(_,targetFaction)"
    , "  return REL[targetFaction] or 'neutral' end }"
    , "unit = { getInfo=function(uid) return INFO[uid] end,"
    , "  exists=function(uid) return INFO[uid] ~= nil end,"
    , "  getPose=function(uid) return POSE[uid] end,"
    , "  getFaction=function(uid) return uid end,"
    , "  getLastAttacker=function() return LAST_ATTACKER end,"
    , "  getVisibleTiles=function() return VISIBLE end,"
    , "  getAllIds=function() return IDS end,"
    , "  clearAnimOverride=function() CLEARS=CLEARS+1 end,"
    , "  stop=function() STOPS=STOPS+1 end,"
    , "  moveTo=function(_,x,y) MOVES=MOVES+1; LAST_MOVE={x=x,y=y} end }"
    , "REGISTER_OPTIONS=nil"
    , "package.loaded['scripts.unit_ai'] = {"
    , "  commandAttack=function(_,target) COMMAND=target end,"
    , "  setConfig=function() end, registerActions=function(_,_,options)"
    , "    REGISTER_OPTIONS=options end }"
    , "package.loaded['scripts.unit_ai_core'] = {"
    , "  isGoalActive=function(s,name) return s.activeGoal == name end,"
    , "  markGoalAccomplished=function(s,name)"
    , "    if s.activeGoal == name then s.activeGoal=nil end end }"
    , "package.loaded['scripts.unit_ai_page'] = {"
    , "  same=function(a,b) return a == b end }"
    , "package.loaded['scripts.movement_speed'] = { comfort=function() return 1 end }"
    , "package.loaded['scripts.unit_ai_combat_lunge'] = {"
    , "  clear=function() CLEARS=CLEARS+1 end }"
    , "local encounter = require('scripts.unit_ai_encounter')"
    ]

spec ∷ Spec
spec = describe "persistent ruin encounter AI (#916)" $ do
    it "selects the lowest-id visible same-page exact hostile and announces aggression once per encounter episode" $
        runsOk $ lns
            [ prelude
            , "local s = {}"
            , "assert(encounter.engageUtility(1,s) == 8)"
            , "encounter.engageExecute(1,s)"
            , "assert(COMMAND == 2, 'target selection must be deterministic')"
            , "assert(s.ruinEncounterCombat == true)"
            , "assert(EVENTS == 1 and #STATES == 1 and #EPISODES == 1)"
            , "assert(string.find(EVENT_MESSAGES[1].message, 'Old Ruin', 1, true))"
            , "assert(STATES[1].i == 41 and STATES[1].u == 1)"
            , "assert(STATES[1].e and not STATES[1].r)"
            , "assert(EPISODES[1].a and EPISODES[1].g and not EPISODES[1].d)"
            , "encounter.engageExecute(1,s)"
            , "assert(EVENTS == 1, 'aggression repeats within one episode')"
            , "INFO[4]={gridX=6,gridY=5,page='world',name='other guard'}"
            , "POSE[4]='standing'"
            , "local other={uid=4,home_x=6,home_y=5,engaged=false,returning=false}"
            , "table.insert(LOCATION.encounter.occupants,other)"
            , "encounter.engageExecute(4,{})"
            , "assert(EVENTS == 1, 'a second occupant repeated aggression')"
            , "LOCATION.encounter.episode_active=true"
            , "encounter.engageExecute(1,{})"
            , "assert(EVENTS == 1, 'persisted active episode was ignored')"
            ]

    it "keeps pre-discovery aggression and disengagement events private" $
        runsOk $ lns
            [ prelude
            , "LOCATION.lifecycle='unknown'; LOCATION.discovered=false"
            , "local s={}"
            , "encounter.engageExecute(1,s)"
            , "assert(COMMAND == 2 and EVENTS == 0)"
            , "assert(#EPISODES == 1 and EPISODES[1].a"
            , "  and not EPISODES[1].g and not EPISODES[1].d)"
            , "LOCATION.encounter.episode_active=true"
            , "LOCATION.encounter.aggression_announced=false"
            , "OCCUPANT.engaged=true"
            , "INFO[1].gridX=23; s.activeGoal='attack'; s.attackTargetUid=2"
            , "assert(encounter.guardUtility(1,s) == 10)"
            , "encounter.guardExecute(1,s)"
            , "assert(EVENTS == 0, 'hidden disengagement leaked the ruin')"
            , "local episode=EPISODES[#EPISODES]"
            , "assert(not episode.a and not episode.g and not episode.d)"
            ]

    it "rejects neutral, invisible, wrong-page, dead, and cleared targets" $
        runsOk $ lns
            [ prelude
            , "IDS={1,2}; REL[2]='neutral'"
            , "assert(encounter.engageUtility(1,{}) == -math.huge)"
            , "REL[2]='hostile'; INFO[2].page='other'"
            , "assert(encounter.engageUtility(1,{}) == -math.huge)"
            , "INFO[2].page='world'; VISIBLE={}"
            , "assert(encounter.engageUtility(1,{}) == -math.huge)"
            , "VISIBLE={{x=5,y=5}}; POSE[2]='dead'"
            , "assert(encounter.engageUtility(1,{}) == -math.huge)"
            , "POSE[2]='standing'; LOCATION.encounter.cleared=true"
            , "assert(encounter.engageUtility(1,{}) == -math.huge)"
            ]

    it "retaliates against a fresh same-page hostile hit without requiring sight" $
        runsOk $ lns
            [ prelude
            , "VISIBLE={}; LAST_ATTACKER={uid=2,at=0}; NOW=5"
            , "local s={}"
            , "assert(encounter.engageUtility(1,s) == 8)"
            , "encounter.engageExecute(1,s)"
            , "assert(COMMAND == 2, 'recent attacker was not acquired')"
            , "assert(s.ruinLastSeenAt == 5 and s.ruinLastSeenX == 5"
            , "  and s.ruinLastSeenY == 5)"
            , "assert(s.ruinEncounterCombat == true)"
            , "local function rejected(mut)"
            , "  INFO[2]={gridX=5,gridY=5,page='world',name='near'}"
            , "  POSE[2]='standing'; REL[2]='hostile'; NOW=5"
            , "  LAST_ATTACKER={uid=2,at=0}; mut()"
            , "  assert(encounter.engageUtility(1,{}) == -math.huge)"
            , "end"
            , "rejected(function() REL[2]='neutral' end)"
            , "rejected(function() INFO[2].page='other' end)"
            , "rejected(function() INFO[2].gridX=23 end)"
            , "rejected(function() POSE[2]='dead' end)"
            , "rejected(function() NOW=10.01 end)"
            , "rejected(function() LOCATION.encounter.cleared=true end)"
            ]

    it "uses an inclusive 12-tile Chebyshev leash and suppresses reacquisition until home" $
        runsOk $ lns
            [ prelude
            , "INFO[2].gridX=22; VISIBLE={{x=22,y=5}}"
            , "assert(encounter.engageUtility(1,{}) == 8)"
            , "INFO[2].gridX=22.01; VISIBLE={{x=22,y=5}}"
            , "assert(encounter.engageUtility(1,{}) == -math.huge)"
            , "INFO[2].gridX=5; VISIBLE={{x=5,y=5}}"
            , "INFO[1].gridX=22.01"
            , "LOCATION.encounter.episode_active=true"
            , "LOCATION.encounter.aggression_announced=true"
            , "local s={activeGoal='attack',attackTargetUid=2}"
            , "assert(encounter.guardUtility(1,s) == 10)"
            , "encounter.guardExecute(1,s)"
            , "assert(s.ruinReturning and s.attackTargetUid == nil)"
            , "assert(EVENTS == 1 and MOVES == 1 and CLEARS == 2)"
            , "assert(STATES[1].r and not STATES[1].e)"
            , "assert(#EPISODES == 1 and not EPISODES[1].a"
            , "  and EPISODES[1].g and EPISODES[1].d)"
            , "assert(encounter.engageUtility(1,s) == -math.huge)"
            , "encounter.guardExecute(1,s)"
            , "assert(EVENTS == 1, 'disengage repeats within one episode')"
            , "INFO[1].gridX=5; INFO[1].gridY=5"
            , "encounter.guardExecute(1,s)"
            , "assert(not s.ruinReturning and not s.ruinEncounterCombat)"
            , "assert(STOPS == 2)"
            , "local last=STATES[#STATES]"
            , "assert(not last.e and not last.r and not last.a and not last.d)"
            , "assert(encounter.engageUtility(1,s) == 8)"
            ]

    it "re-enters return-home cleanup from persisted returning state after load" $
        runsOk $ lns
            [ prelude
            , "INFO[1].gridX=9; OCCUPANT.returning=true"
            , "LOCATION.encounter.episode_active=true"
            , "LOCATION.encounter.aggression_announced=true"
            , "local s={activeGoal='attack',attackTargetUid=2}"
            , "assert(encounter.guardUtility(1,s) == 10)"
            , "encounter.guardExecute(1,s)"
            , "assert(s.ruinReturning and s.attackTargetUid == nil)"
            , "assert(MOVES == 1 and CLEARS == 2)"
            ]

    it "pursues only the last seen tile for ten seconds, then disengages \
       \for every invalid target condition" $
        runsOk $ lns
            [ prelude
            , "IDS={1,2}; local s={activeGoal='attack',attackTargetUid=2}"
            , "assert(encounter.guardUtility(1,s) == -math.huge)"
            , "assert(s.ruinLastSeenX == 5 and s.ruinLastSeenY == 5)"
            , "VISIBLE={}; INFO[1].gridX=0; INFO[2].gridX=9; INFO[2].gridY=8; NOW=10"
            , "assert(encounter.guardUtility(1,s) == -math.huge)"
            , "assert(encounter.memoryUtility(1,s) == 8.5)"
            , "encounter.memoryExecute(1,s)"
            , "assert(LAST_MOVE.x == 5 and LAST_MOVE.y == 5,"
            , "  'memory pursuit followed the hidden live target')"
            , "NOW=10.01; assert(encounter.guardUtility(1,s) == 10)"
            , "assert(encounter.memoryUtility(1,s) == -math.huge)"
            , "local function invalid(mut)"
            , "  INFO[2]={gridX=5,gridY=5,page='world',name='near'}"
            , "  POSE[2]='standing'; REL[2]='hostile'; VISIBLE={{x=5,y=5}}"
            , "  local x={activeGoal='attack',attackTargetUid=2,ruinLastSeenAt=NOW}"
            , "  mut(); assert(encounter.guardUtility(1,x) == 10)"
            , "end"
            , "invalid(function() INFO[2]=nil end)"
            , "invalid(function() POSE[2]='dead' end)"
            , "invalid(function() INFO[2].page='other' end)"
            , "invalid(function() REL[2]='friendly' end)"
            ]

    it "keeps generic retaliation excluded after adding encounter-owned hit acquisition" $
        runsOk $ lns
            [ prelude
            , "encounter.register({idleUtility=function() return 0 end,"
            , "  idleExecute=function() end})"
            , "assert(REGISTER_OPTIONS ~= nil)"
            , "assert(REGISTER_OPTIONS.excludeUniversal.engage == true)"
            ]
