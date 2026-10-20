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
    -- #2521: the guard's DIRECTED regard of each target, through the
    -- unit-id relation verb the encounter now asks via the policy seam.
    , "unit = { relationFromTo=function(_,target)"
    , "    return REL[target] or 'neutral' end,"
    , "  getInfo=function(uid) return INFO[uid] end,"
    , "  exists=function(uid) return INFO[uid] ~= nil end,"
    , "  getPose=function(uid) return POSE[uid] end,"
    -- HITS, when a case sets it, is per unit; otherwise every unit
    -- reads the one LAST_ATTACKER the older cases share.
    , "  getLastAttacker=function(uid)"
    , "    if HITS then return HITS[uid] end return LAST_ATTACKER end,"
    , "  getVisibleTiles=function() return VISIBLE end,"
    , "  getAllIds=function() return IDS end,"
    , "  clearAnimOverride=function() CLEARS=CLEARS+1 end,"
    , "  stop=function() STOPS=STOPS+1 end,"
    , "  moveTo=function(_,x,y) MOVES=MOVES+1; LAST_MOVE={x=x,y=y} end }"
    , "REGISTER_OPTIONS=nil"
    -- STATE[uid], when a case registers one, is that guard's AI state:
    -- commandAttack then models the real one (target, then setGoal), so
    -- the seam's goal occurrences and cutoffs run exactly as in core.
    , "STATE = {}"
    , "POLICY = require('scripts.unit_ai_combat_policy')"
    , "package.loaded['scripts.unit_ai'] = {"
    , "  commandAttack=function(uid,target) COMMAND=target"
    , "    local s=STATE[uid]; if s then s.attackTargetUid=target"
    , "      POLICY.goalChanged(s,'attack'); s.activeGoal='attack' end end,"
    , "  setConfig=function() end, registerActions=function(_,_,options)"
    , "    REGISTER_OPTIONS=options end }"
    , "package.loaded['scripts.unit_ai_core'] = {"
    , "  isGoalActive=function(s,name) return s.activeGoal == name end,"
    , "  markGoalAccomplished=function(s,name)"
    , "    s.goalStatus=s.goalStatus or {}; s.goalStatus[name]='accomplished';"
    , "    if s.activeGoal == name then POLICY.goalChanged(s,nil);"
    , "      s.activeGoal=nil end end }"
    , "package.loaded['scripts.unit_ai_page'] = {"
    , "  same=function(a,b) return a == b end }"
    , "package.loaded['scripts.movement_speed'] = { comfort=function() return 1 end }"
    , "package.loaded['scripts.unit_ai_combat_lunge'] = {"
    , "  clear=function() CLEARS=CLEARS+1 end }"
    , "local encounter = require('scripts.unit_ai_encounter')"
    ]

-- | @start(hitAt, now)@: guard 1 takes a hit from unit 2 at @hitAt@ and
--   engages at @now@ with no hostile visible, through the real seam. The
--   returned state is registered in STATE so commandAttack and the core
--   stub run the seam's goal hooks.
selfDefenseHelpers ∷ Text
selfDefenseHelpers = lns
    [ "HITS = HITS or {}"
    , "function start(hitAt, now)"
    , "  local s = {}; STATE[1] = s"
    , "  HITS[1] = {uid=2,at=hitAt}; VISIBLE = {}; NOW = now; EVENTS = 0"
    , "  EVENT_MESSAGES = {}"
    , "  assert(encounter.engageUtility(1,s) == 8, 'self-defense not offered')"
    , "  encounter.engageExecute(1,s)"
    , "  assert(COMMAND == 2 and s.activeGoal == 'attack', 'no attack goal')"
    , "  VISIBLE = {{x=5,y=5}}"
    , "  return s"
    , "end"
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
            -- #2521: a NEUTRAL fresh attacker is now acquired through
            -- local self-defense (the self-defense sequences below);
            -- what still refuses it is every other eligibility rule.
            , "rejected(function() REL[2]='neutral'; INFO[2].page='other' end)"
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

    it "cancels an active retreat when return takes ownership and after load" $
        runsOk $ lns
            [ prelude
            , "INFO[1].gridX=23"
            , "local s={ruinEncounterCombat=true,"
            , "  activeGoal='retreat',retreatThreatUid=2,"
            , "  retreatLastMoveTo={x=20,y=5}}"
            , "assert(encounter.guardUtility(1,s) == 10)"
            , "encounter.guardExecute(1,s)"
            , "assert(s.ruinReturning and s.activeGoal == nil)"
            , "assert(s.retreatThreatUid == nil and s.retreatLastMoveTo == nil)"
            , "assert(s.goalStatus.retreat == 'accomplished')"
            , "assert(s.ruinEncounterCombat == nil)"
            , "assert(MOVES == 1 and CLEARS == 2)"
            , "assert(#STATES == 1 and STATES[1].r and not STATES[1].e)"
            , "INFO[1].gridX=9; OCCUPANT.returning=true"
            , "MOVES=0; CLEARS=0; STATES={}"
            , "local loaded={ruinReturning=true,ruinEncounterCombat=true,"
            , "  activeGoal='retreat',retreatThreatUid=2,"
            , "  retreatLastMoveTo={x=20,y=5}}"
            , "assert(encounter.guardUtility(1,loaded) == 10)"
            , "encounter.guardExecute(1,loaded)"
            , "assert(loaded.ruinReturning and loaded.activeGoal == nil)"
            , "assert(loaded.retreatThreatUid == nil"
            , "  and loaded.retreatLastMoveTo == nil)"
            , "assert(loaded.goalStatus.retreat == 'accomplished')"
            , "assert(loaded.ruinEncounterCombat == nil)"
            , "assert(MOVES == 1 and CLEARS == 2 and #STATES == 0)"
            ]

    it "ends a partial-death episode, permits re-entry, and reconciles it after load" $
        runsOk $ lns
            [ prelude
            , "INFO[4]={gridX=6,gridY=5,page='world',name='survivor'}"
            , "POSE[1]='dead'; POSE[4]='standing'; IDS={1,2,4}"
            , "OCCUPANT.engaged=true"
            , "local survivor={uid=4,home_x=6,home_y=5,"
            , "  engaged=false,returning=false}"
            , "table.insert(LOCATION.encounter.occupants,survivor)"
            , "LOCATION.encounter.episode_active=true"
            , "LOCATION.encounter.aggression_announced=true"
            , "local s={}"
            , "assert(encounter.guardUtility(4,s) == 10)"
            , "encounter.guardExecute(4,s)"
            , "assert(#EPISODES == 1 and not EPISODES[1].a"
            , "  and EPISODES[1].g and EPISODES[1].d)"
            , "LOCATION.encounter.episode_active=false"
            , "LOCATION.encounter.disengage_announced=true"
            , "encounter.engageExecute(4,s)"
            , "assert(EVENTS == 2, 'a later re-entry suppressed aggression')"
            , "assert(EPISODES[#EPISODES].a and EPISODES[#EPISODES].g"
            , "  and not EPISODES[#EPISODES].d)"
            , "require('scripts.unit_ai_claims').resetAll()"
            , "EVENTS=0; STATES={}; EPISODES={}"
            , "LOCATION.encounter.episode_active=true"
            , "LOCATION.encounter.aggression_announced=true"
            , "LOCATION.encounter.disengage_announced=false"
            , "survivor.engaged=false; survivor.returning=false"
            , "local loaded={}"
            , "assert(encounter.guardUtility(4,loaded) == 10)"
            , "encounter.guardExecute(4,loaded)"
            , "assert(#EPISODES == 1 and not EPISODES[1].a"
            , "  and EPISODES[1].g and EPISODES[1].d)"
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

    describe "local self-defense through the shared policy seam (#2521)" $ do
        forM_ ["ally", "neutral"] $ \rel →
            it ("answers a recent " <> T.unpack rel <> " attacker across later \
                \decisions, inclusive at ten seconds, refreshed by a hit, \
                \without a peer joining or any hostile claim") $
                runsOk $ lns
                    [ prelude, selfDefenseHelpers
                    , "REL[2]='" <> rel <> "'; REL[3]='neutral'"
                    , "local s = start(0, 1)"
                    , "assert(EVENTS == 1, 'expected one aggression notice')"
                    , "assert(EVENT_MESSAGES[1].message =="
                    , "  'guard at Old Ruin attacks near in self-defense after a recent attack',"
                    , "  EVENT_MESSAGES[1].message)"
                    , "assert(POLICY.isHostile(1, 2) == false, 'no relation moved')"
                    , "NOW=10; assert(encounter.guardUtility(1,s) == -math.huge,"
                    , "  'the fight did not survive to exactly ten seconds')"
                    , "HITS[1]={uid=2,at=9.5}"
                    , "NOW=19.5; assert(encounter.guardUtility(1,s) == -math.huge,"
                    , "  'a new hit did not refresh the window')"
                    , "NOW=19.51; assert(encounter.guardUtility(1,s) == 10)"
                    , "assert(s.ruinDisengageReason == 'self-defense ended',"
                    , "  tostring(s.ruinDisengageReason))"
                    -- An uninjured fellow guard does not join.
                    , "INFO[4]={gridX=5,gridY=5,page='world',name='peer'}; POSE[4]='standing'"
                    , "table.insert(LOCATION.encounter.occupants,"
                    , "  {uid=4,home_x=5,home_y=5,engaged=false,returning=false})"
                    , "assert(encounter.engageUtility(4,{}) == -math.huge)"
                    ]

        it "refuses a self-defense hit just beyond ten seconds" $
            runsOk $ lns
                [ prelude, selfDefenseHelpers
                , "REL[2]='ally'; HITS={[1]={uid=2,at=0}}; VISIBLE={}"
                , "NOW=10.01; assert(encounter.engageUtility(1,{}) == -math.huge)"
                , "NOW=10; assert(encounter.engageUtility(1,{}) == 8)"
                ]

        it "keeps the hostile notice for an ordinary hostile acquisition" $
            runsOk $ lns
                [ prelude
                , "encounter.engageExecute(1,{})"
                , "assert(EVENT_MESSAGES[1].message =="
                , "  'guard at Old Ruin attacks near because their faction relation is hostile',"
                , "  EVENT_MESSAGES[1].message)"
                ]

        it "keeps a hidden encounter's self-defense private and its episode \
           \notices exactly-once across guards" $
            runsOk $ lns
                [ prelude, selfDefenseHelpers
                , "LOCATION.lifecycle='unknown'; LOCATION.discovered=false"
                , "REL[2]='ally'"
                , "local s = start(0, 1)"
                , "assert(EVENTS == 0, 'a hidden encounter announced self-defense')"
                , "LOCATION.lifecycle='discovered'; LOCATION.discovered=true"
                , "INFO[4]={gridX=5,gridY=5,page='world',name='peer'}; POSE[4]='standing'"
                , "table.insert(LOCATION.encounter.occupants,"
                , "  {uid=4,home_x=5,home_y=5,engaged=false,returning=false})"
                , "HITS[4]={uid=2,at=1}; local s4={}; STATE[4]=s4"
                , "encounter.engageExecute(4,s4)"
                , "assert(EVENTS == 0, 'the same episode announced on a second guard')"
                ]

        it "with hostility cleared by a fixture, needs a hit strictly after the \
           \clear, and cannot sustain a fight on pre-clear damage" $
            runsOk $ lns
                [ prelude, selfDefenseHelpers
                , "REL[2]='neutral'; VISIBLE={}"
                , "POLICY.fixtureInputs=function(uid)"
                , "  if uid == 1 then return {diplomacyClearAt=5} end end"
                , "NOW=6"
                , "HITS={[1]={uid=2,at=4.5}}; assert(encounter.engageUtility(1,{}) == -math.huge)"
                , "HITS={[1]={uid=2,at=5}};   assert(encounter.engageUtility(1,{}) == -math.huge)"
                , "HITS={[1]={uid=2,at=5.01}}; assert(encounter.engageUtility(1,{}) == 8)"
                , "POLICY.fixtureInputs=function() return nil end"
                , "local s = start(4, 4.5)"
                , "POLICY.fixtureInputs=function(uid)"
                , "  if uid == 1 then return {diplomacyClearAt=5} end end"
                , "VISIBLE={{x=5,y=5}}; NOW=6"
                , "assert(encounter.guardUtility(1,s) == 10, 'pre-clear damage sustained it')"
                ]

        it "lets a fixture authorization keep only the original goal and \
           \target fighting, and refuses an absent or mismatched one" $
            runsOk $ lns
                [ prelude, selfDefenseHelpers
                , "REL[2]='ally'"
                , "local cases = {"
                , "  {fields=function(o) return {uid=1,target=2,occurrence=o} end, keeps=true},"
                , "  {fields=function(o) return nil end, keeps=false},"
                , "  {fields=function(o) return {uid=4,target=2,occurrence=o} end, keeps=false},"
                , "  {fields=function(o) return {uid=1,target=3,occurrence=o} end, keeps=false},"
                , "  {fields=function(o) return {uid=1,target=2,occurrence=o+1} end, keeps=false} }"
                , "for i, c in ipairs(cases) do"
                , "  LOCATION.encounter.episode_active=false"
                , "  local s = start(0, 1)"
                , "  local auth = c.fields(POLICY.occurrence(s))"
                , "  POLICY.fixtureInputs=function(uid)"
                , "    if uid == 1 and auth then return {authorization=auth} end end"
                , "  NOW=30; VISIBLE={{x=5,y=5}}"
                , "  local u = encounter.guardUtility(1,s)"
                , "  assert((u == -math.huge) == c.keeps, 'case ' .. i)"
                , "  POLICY.fixtureInputs=function() return nil end"
                , "end"
                ]

        it "voids the authorization at every terminating boundary and never \
           \carries it to a replacement goal against the same target" $
            runsOk $ lns
                [ prelude, selfDefenseHelpers
                , "REL[2]='ally'"
                , "local boundaries = {"
                , "  {'target death', function() POSE[2]='dead' end},"
                , "  {'target disappearance', function() INFO[2]=nil end},"
                , "  {'page departure', function() INFO[2].page='other' end},"
                , "  {'target leash', function() INFO[2].gridX=23 end},"
                , "  {'guard leash', function() INFO[1].gridX=-12.5 end},"
                , "  {'sight memory', function() VISIBLE={}; NOW=41 end},"
                , "  {'goal completion', function(s)"
                , "      package.loaded['scripts.unit_ai_core'].markGoalAccomplished(s,'attack')"
                , "      s.attackTargetUid=nil end} }"
                , "for _, b in ipairs(boundaries) do"
                , "  INFO[1]={gridX=5,gridY=5,page='world',name='guard'}"
                , "  INFO[2]={gridX=5,gridY=5,page='world',name='near'}; POSE[2]='standing'"
                , "  LOCATION.encounter.episode_active=false"
                , "  local s = start(0, 1)"
                , "  local auth = {uid=1,target=2,occurrence=POLICY.occurrence(s)}"
                , "  POLICY.fixtureInputs=function(uid)"
                , "    if uid == 1 then return {authorization=auth} end end"
                , "  NOW=30; VISIBLE={{x=5,y=5}}"
                , "  assert(encounter.guardUtility(1,s) == -math.huge, b[1] .. ': not authorized')"
                , "  b[2](s)"
                , "  if encounter.guardUtility(1,s) == 10 then encounter.guardExecute(1,s) end"
                , "  assert(POLICY.occurrence(s) == nil, b[1] .. ': the goal survived')"
                -- Back at the post with the target restored: neither a fresh
                -- acquisition nor a replacement goal may use the old one.
                , "  INFO[1]={gridX=5,gridY=5,page='world',name='guard'}"
                , "  INFO[2]={gridX=5,gridY=5,page='world',name='near'}; POSE[2]='standing'"
                -- The clock only moves forward: a boundary may have advanced it.
                , "  VISIBLE={{x=5,y=5}}; local T = math.max(NOW, 31); NOW = T"
                -- The guard finishes returning through the real executor,
                -- so only the seam can refuse what follows.
                , "  encounter.guardExecute(1,s)"
                , "  assert(not s.ruinReturning, b[1] .. ': still returning')"
                , "  assert(encounter.engageUtility(1,s) == -math.huge, b[1] .. ': reacquired')"
                , "  HITS[1]={uid=2,at=T+0.5}; NOW=T+1"
                , "  assert(encounter.engageUtility(1,s) == 8, b[1] .. ': a later hit was refused')"
                , "  HITS[1]={uid=2,at=0}"
                , "  package.loaded['scripts.unit_ai'].commandAttack(1, 2)"
                , "  assert(encounter.guardUtility(1,s) == 10, b[1] .. ': carried to a replacement')"
                , "  POLICY.fixtureInputs=function() return nil end"
                , "end"
                ]

        it "after a goal ends while its attacker is still recent, refuses old \
           \and equal-time hits, accepts a strictly later one, and still \
           \lets return-home win; a hostile attacker is not cut off" $
            runsOk $ lns
                [ prelude, selfDefenseHelpers
                , "REL[2]='ally'"
                , "local s = start(0, 1)"
                , "NOW=2; package.loaded['scripts.unit_ai_core'].markGoalAccomplished(s,'attack')"
                , "s.attackTargetUid=nil; NOW=3"
                , "assert(POLICY.cutoff(s) == 2)"
                , "assert(encounter.engageUtility(1,s) == -math.huge, 'old hit restarted it')"
                , "HITS[1]={uid=2,at=2}"
                , "assert(encounter.engageUtility(1,s) == -math.huge, 'equal-time hit restarted it')"
                , "HITS[1]={uid=2,at=2.5}"
                , "s.ruinReturning=true"
                , "assert(encounter.engageUtility(1,s) == -math.huge, 'return-home lost')"
                , "s.ruinReturning=nil"
                , "assert(encounter.engageUtility(1,s) == 8, 'a strictly later hit was refused')"
                -- Out of sight, so only the recent-attacker path can answer.
                , "REL[2]='hostile'; HITS[1]={uid=2,at=1}; VISIBLE={}"
                , "assert(encounter.engageUtility(1,s) == 8, 'hostility was cut off')"
                ]

    it "keeps generic retaliation excluded after adding encounter-owned hit acquisition" $
        runsOk $ lns
            [ prelude
            , "encounter.register({idleUtility=function() return 0 end,"
            , "  idleExecute=function() end})"
            , "assert(REGISTER_OPTIONS ~= nil)"
            , "assert(REGISTER_OPTIONS.excludeUniversal.engage == true)"
            ]
