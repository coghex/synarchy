{-# LANGUAGE Strict #-}
-- | "Lua faction model" › "directed relation consumers (FTS-5)" (#2521):
--   the unit-id relation, alliance and attack-permission verbs through
--   their PRODUCTION registration, driving the real consuming scripts —
--   @unit_ai_medic.lua@, @unit_ai_core.lua@'s @groupEffectivenessVs@,
--   @init_context_menu.lua@'s Attack row, and the generic combat
--   decisions in @unit_ai_combat.lua@ / @unit_ai_combat_attack.lua@
--   through the shared @unit_ai_combat_policy.lua@ seam — against real
--   unit instances on a headless engine.
--
--   What is stubbed is never a relation answer: the catalogue is the
--   shipped @data\/factions\/base.yaml@ loaded through the real
--   @engine.loadFactionYaml@, a live overlay is installed by calling the
--   production unit registrar 'registerUnitAPIWith' with fixture causes, and the
--   last-attacker memory is written into the real unit manager. Only
--   inputs no relation question reads are stubbed: @engine.gameTime@
--   (so a case owns the clock), medic knowledge/wounds, per-unit combat
--   power, and the menu's hit test and selection.
--
--   Own engine: every case rewrites the unit manager ref.
module Test.Headless.Lua.DirectedRelations (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified HsLua as Lua
import Data.IORef (atomicModifyIORef', writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.CallStats (newLuaCallStats)
import Engine.Scripting.Lua.API.Register.Unit (registerUnitAPIWith)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, mkUnit, newBareLuaBackend)
import Unit.Faction
    ( Faction(..), FactionRelation(..), canAttack, factionRelation
    , factionTag, relationTag )
import Unit.Faction.Membership
    ( MembershipSource(..), TagMembership(..), UnitFactionProfile
    , localController, mkUnitFactionProfile, resolveLegacyFaction )
import Unit.Faction.Profile
    ( FactionCapability(..), LiveRelationCause(..)
    , mkFactionTag, relationCauseId, tagAcolyte )
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "fts5_page"

-- * Profiles

-- | A live profile from tag text, every tag a definition default. Fails
--   loudly on a bad fixture tag rather than silently dropping it.
prof ∷ Bool → [Text] → [FactionCapability] → UnitFactionProfile
prof controlled ts = mkUnitFactionProfile
    (if controlled then Just localController else Nothing)
    [ TagMembership (fromMaybe (error ("bad tag " ⧺ show t)) (mkFactionTag t))
                    MemberDefinitionDefault
    | t ← ts ]

-- | The approved D-26 legacy profile for a legacy faction value: the
--   acolyte definition's @acolyte@ default carried by @player@, no
--   defaults for the rest (so @wildlife@ falls back to its own tag).
legacy ∷ Faction → UnitFactionProfile
legacy f = resolveLegacyFaction (if f ≡ FactionPlayer then [tagAcolyte] else []) f

legacyOrder ∷ [Faction]
legacyOrder = [FactionPlayer, FactionWildlife, FactionHostile, FactionNeutral, FactionDebug]

-- | The issue's requirement-9 matrix, actor rows × target columns, in
--   'legacyOrder'.
expectedRelations ∷ [[FactionRelation]]
expectedRelations =
    [ [RelAlly,    RelHostile, RelHostile, RelNeutral, RelNeutral]
    , [RelHostile, RelAlly,    RelHostile, RelNeutral, RelNeutral]
    , [RelHostile, RelHostile, RelAlly,    RelNeutral, RelNeutral]
    , [RelNeutral, RelNeutral, RelNeutral, RelNeutral, RelNeutral]
    , [RelNeutral, RelNeutral, RelNeutral, RelNeutral, RelNeutral] ]

-- * World fixtures

-- | Replace the roster with @rows@ (uid, profile, position), all on the
--   one visible page.
resetWorld ∷ EngineEnv → [(Int, UnitFactionProfile, (Float, Float))] → IO ()
resetWorld env rows = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList [("acolyte", minimalDef "acolyte" "Acolyte")]
        , umInstances = HM.fromList
            [ ( UnitId (fromIntegral uid)
              , (mkUnit "acolyte" FactionNeutral pos 100 [] [])
                    { uiFaction = p, uiPage = fixturePage } )
            | (uid, p, pos) ← rows ]
        , umNextId = 1000 }

-- | Record that @attacker@ struck @victim@ at game time @at@, as
--   Combat.Resolution does.
hit ∷ EngineEnv → Int → Int → Double → IO ()
hit env victim attacker at = atomicModifyIORef' (unitManagerRef env) $ \um →
    ( um { umInstances = HM.adjust
             (\u → u { uiLastAttackerUid = Just (fromIntegral attacker)
                     , uiLastAttackerAt  = at })
             (UnitId (fromIntegral victim)) (umInstances um) }
    , () )

setPose ∷ EngineEnv → Int → Text → IO ()
setPose env uid pose = atomicModifyIORef' (unitManagerRef env) $ \um →
    ( um { umInstances = HM.adjust (\u → u { uiPose = pose })
                                   (UnitId (fromIntegral uid)) (umInstances um) }
    , () )

removeUnit ∷ EngineEnv → Int → IO ()
removeUnit env uid = atomicModifyIORef' (unitManagerRef env) $ \um →
    (um { umInstances = HM.delete (UnitId (fromIntegral uid)) (umInstances um) }, ())

-- * Lua session

-- | A fresh Lua state with the full production API, the shipped faction
--   catalogue, the real unit AI loaded, an owned clock, and helpers.
session ∷ EngineEnv → IO LuaBackendState
session env = do
    ls ← newBareLuaBackend env
    ok ls $ T.unlines
        [ "assert(engine.loadFactionYaml('data/factions/base.yaml') ~= false)"
        , "AI = require('scripts.unit_ai')"
        , "CORE = require('scripts.unit_ai_core')"
        , "COMBAT = require('scripts.unit_ai_combat')"
        , "ATTACK = require('scripts.unit_ai_combat_attack')"
        , "POLICY = require('scripts.unit_ai_combat_policy')"
        -- A fixture that drops a unit's AI row to start it afresh also
        -- drops the seam's uid-keyed bookkeeping with CLAIMS.resetAll().
        , "CLAIMS = require('scripts.unit_ai_claims')"
        , "NOW = 0; engine.gameTime = function() return NOW end"
        , "function S(uid) return CORE.ensureState(uid) end"
        -- One autonomous engage decision; the chosen target or false.
        , "function engage(uid) local s = S(uid)"
        , "  if COMBAT.engageUtility(uid, s, {}) == -math.huge then return false end"
        , "  COMBAT.engageExecute(uid, s, {}); return s.attackTargetUid or false end"
        -- One attack_target decision; the target still held, or false.
        , "function tick(uid) local s = S(uid)"
        , "  ATTACK.attackTargetExecute(uid, s, {})"
        , "  if not CORE.isGoalActive(s, 'attack') then return false end"
        , "  return s.attackTargetUid or false end"
        , "return true"
        ]
    pure ls

-- | Install a live overlay through the PRODUCTION unit registrar,
--   re-registering the whole @unit@ namespace with @causes@ (stubs a
--   case needs are installed after this).
withOverlay ∷ EngineEnv → LuaBackendState → [LiveRelationCause] → IO ()
withOverlay env ls causes = Lua.runWith (lbsLuaState ls) $ do
    cs ← Lua.liftIO newLuaCallStats
    registerUnitAPIWith causes cs env

cause ∷ Text → Text → Text → FactionRelation → LiveRelationCause
cause cid s t r = LiveRelationCause
    { lrcCause = relationCauseId cid
    , lrcSource = fromMaybe (error "tag") (mkFactionTag s)
    , lrcTarget = fromMaybe (error "tag") (mkFactionTag t)
    , lrcRelation = r }

-- | Run a chunk that must return @true@.
ok ∷ LuaBackendState → Text → IO ()
ok ls chunk = evalDebug ls chunk `shouldReturn` "true"

-- | Evaluate an expression.
val ∷ LuaBackendState → Text → IO Text
val ls expr = evalDebug ls ("return " <> expr)

-- * Spec

spec ∷ SpecWith EngineEnv
spec = describe "Lua faction model" $
    describe "directed relation consumers (FTS-5)" $ do
        relationSpec
        medicSpec
        groupSpec
        selfDefenseSpec
        authorizationSpec

-- ** Relations and permission

relationSpec ∷ SpecWith EngineEnv
relationSpec = do
    it "shared-controller units are allied and may not attack each other \
       \unless unrestricted combat applies" $ \env → do
        -- Same controller outranks the shipped acolyte/nomad hostility.
        resetWorld env
            [ (1, prof True ["acolyte"] [], (0, 0))
            , (2, prof True ["nomad"] [], (0, 0))
            , (3, legacy FactionDebug, (0, 0)) ]
        ls ← session env
        val ls "unit.relationFromTo(1, 2) .. ',' .. unit.relationFromTo(2, 1)"
            `shouldReturn` "\"ally,ally\""
        val ls "tostring(unit.canAttack(1, 2)) .. ',' .. tostring(unit.canAttack(2, 1))"
            `shouldReturn` "\"false,false\""
        -- Debug staging: unrestricted on either side, whatever the relation.
        val ls "unit.relationFromTo(1, 3) .. ',' .. tostring(unit.canAttack(1, 3))\
               \ .. ',' .. tostring(unit.canAttack(3, 1))"
            `shouldReturn` "\"neutral,true,true\""

    it "acolyte and nomad units without a higher-precedence rule are \
       \mutually hostile" $ \env → do
        resetWorld env
            [ (1, prof False ["acolyte"] [], (0, 0))
            , (2, prof False ["nomad"] [], (0, 0))
            , (3, legacy FactionPlayer, (0, 0)) ]
        ls ← session env
        val ls "unit.relationFromTo(1, 2) .. ',' .. unit.relationFromTo(2, 1)\
               \ .. ',' .. unit.relationFromTo(3, 2)"
            `shouldReturn` "\"hostile,hostile,hostile\""
        val ls "tostring(unit.canAttack(1, 2)) .. ',' .. tostring(unit.canAttack(2, 1))"
            `shouldReturn` "\"true,true\""

    it "an injected fight_team_A → fight_team_B overlay lets A attack while \
       \B still regards A as an ally and is refused; the Attack row follows, \
       \and debug staging stays permitted" $ \env → do
        resetWorld env
            [ (1, prof True ["acolyte", "fight_team_A"] [], (0, 0))
            , (2, prof True ["acolyte", "fight_team_B"] [], (0, 0))
            , (3, legacy FactionDebug, (0, 0))
            , (4, legacy FactionDebug, (0, 0)) ]
        ls ← session env
        withOverlay env ls
            [cause "fixture_fight" "fight_team_A" "fight_team_B" RelHostile]
        val ls "unit.relationFromTo(1, 2) .. ',' .. unit.relationFromTo(2, 1)"
            `shouldReturn` "\"hostile,ally\""
        val ls "tostring(unit.canAttack(1, 2)) .. ',' .. tostring(unit.canAttack(2, 1))"
            `shouldReturn` "\"true,false\""
        attackRow ls 1 2 `shouldReturn` "\"true\""
        attackRow ls 2 1 `shouldReturn` "\"false\""
        attackRow ls 3 4 `shouldReturn` "\"true\""   -- debug vs debug
        attackRow ls 3 1 `shouldReturn` "\"true\""   -- debug vs roster
        -- Without the overlay the same roster is one side again.
        withOverlay env ls []
        attackRow ls 1 2 `shouldReturn` "\"false\""

    it "answers all 25 legacy pairs per the approved matrix, keeps attack \
       \permission compatible on every pair, and leaves the string verbs' \
       \answers unchanged" $ \env → do
        -- Two units per legacy value, so the diagonal is a real pair.
        let uidOf f k = 10 * (fromEnum' f + 1) + k
            fromEnum' f = fromMaybe 0 (lookup f (zip legacyOrder [0 ..]))
        resetWorld env [ (uidOf f k, legacy f, (0, 0))
                       | f ← legacyOrder, k ← [1, 2 ∷ Int] ]
        ls ← session env
        forM_ (zip legacyOrder expectedRelations) $ \(a, row) →
            forM_ (zip legacyOrder row) $ \(b, expected) → do
                let pair = T.pack (show (uidOf a 1)) <> ", " <> T.pack (show (uidOf b 2))
                    ctx = T.unpack (factionTag a <> "→" <> factionTag b)
                r ← val ls ("unit.relationFromTo(" <> pair <> ")")
                (ctx, r) `shouldBe` (ctx, q (relationTag expected))
                p ← val ls ("unit.canAttack(" <> pair <> ")")
                (ctx, p) `shouldBe` (ctx, if canAttack a b then "true" else "false")
                -- The compatibility-window verbs are untouched.
                s ← val ls ("faction.relation('" <> factionTag a <> "', '"
                            <> factionTag b <> "')")
                (ctx, s) `shouldBe` (ctx, q (relationTag (factionRelation a b)))
        -- Exactly the eight intended changes, and nothing else.
        let changed = [ (a, b) | (a, row) ← zip legacyOrder expectedRelations
                               , (b, r) ← zip legacyOrder row
                               , r ≢ factionRelation a b ]
        changed `shouldMatchList`
            [ (FactionPlayer, FactionDebug), (FactionWildlife, FactionDebug)
            , (FactionHostile, FactionDebug), (FactionNeutral, FactionNeutral)
            , (FactionDebug, FactionPlayer), (FactionDebug, FactionWildlife)
            , (FactionDebug, FactionHostile), (FactionDebug, FactionDebug) ]

    it "fails closed for anything that names no live unit" $ \env → do
        resetWorld env [(1, legacy FactionPlayer, (0, 0))]
        ls ← session env
        val ls "unit.relationFromTo(1, 404) .. ',' .. unit.relationFromTo('1', 1)\
               \ .. ',' .. tostring(unit.isFriendlyToward(1, nil))\
               \ .. ',' .. tostring(unit.canAttack(1.5, 1))\
               \ .. ',' .. tostring(unit.isFriendlyToward(1, 1))"
            `shouldReturn` "\"neutral,neutral,false,false,true\""
  where
    q t = "\"" <> t <> "\""

-- | The Attack row's enabled state from the real init_context_menu, with
--   @attacker@ selected and @target@ under the cursor. Commandability and
--   permission are the production verbs.
attackRow ∷ LuaBackendState → Int → Int → IO Text
attackRow ls attacker target = do
    ok ls $ T.unlines
        [ "local cm = require('scripts.ui.context_menu')"
        , "_G.__rows = {}"
        , "cm.show = function(items) for _, it in ipairs(items) do"
        , "  _G.__rows[it.label] = tostring(it.enabled ~= false) end end"
        , "unit.hitTestAt = function() return " <> tshow target <> " end"
        , "unit.getSelected = function() return {" <> tshow attacker <> "} end"
        , "assert(require('scripts.init_context_menu').tryUnitMenu(10, 20))"
        , "return true" ]
    val ls "tostring(_G.__rows['Attack'])"

-- ** Medic

medicSpec ∷ SpecWith EngineEnv
medicSpec = do
    it "a medic treats the patient IT regards as an ally (medic → patient), \
       \and is refused when only the patient regards the medic so" $ \env → do
        -- medic_team → patient_team ally; the reverse is neutral.
        resetWorld env
            [ (1, prof False ["medic_team"] [], (0, 0))
            , (2, prof False ["patient_team"] [], (1, 0)) ]
        ls ← session env
        withOverlay env ls
            [cause "fixture_ally" "medic_team" "patient_team" RelAlly]
        val ls "unit.relationFromTo(1, 2) .. ',' .. unit.relationFromTo(2, 1)"
            `shouldReturn` "\"ally,neutral\""
        medicSetup ls
        treats ls 1 2 `shouldReturn` "true"
        -- The converse: unit 2 as the medic, unit 1 bleeding.
        treats ls 2 1 `shouldReturn` "false"

    it "applies the eight intended matrix changes to medic eligibility" $ \env → do
        let cases =
                [ (FactionPlayer, FactionPlayer, True)
                , (FactionDebug, FactionPlayer, False)   -- was allied
                , (FactionPlayer, FactionDebug, False)   -- was allied
                , (FactionDebug, FactionDebug, False)    -- was allied
                , (FactionNeutral, FactionNeutral, False) -- was allied
                , (FactionWildlife, FactionWildlife, True) ]
        forM_ cases $ \(m, p, expected) → do
            resetWorld env [(1, legacy m, (0, 0)), (2, legacy p, (1, 0))]
            ls ← session env
            medicSetup ls
            r ← treats ls 1 2
            (factionTag m, factionTag p, r)
                `shouldBe` (factionTag m, factionTag p, if expected then "true" else "false")
  where
    treats ls medic patient = evalDebug ls $ T.unlines
        [ "MEDICS = { [" <> tshow medic <> "] = true }"
        , "BLEEDING = { [" <> tshow patient <> "] = true }"
        , "return MEDIC.treatAllyUtility(" <> tshow medic <> ", S("
            <> tshow medic <> "), P) ~= -math.huge" ]

-- | Medic inputs no relation question reads: who knows bleed control,
--   who is bleeding.
medicSetup ∷ LuaBackendState → IO ()
medicSetup ls = ok ls $ T.unlines
    [ "MEDIC = require('scripts.unit_ai_medic')"
    , "MEDICS, BLEEDING = {}, {}"
    , "P = { treat_scan_range = 60, treat_min_seep = 0.05,"
    , "      treat_lock_utility = 9, treat_base_utility = 7 }"
    , "unit.getKnowledge = function(uid, k)"
    , "  if k == 'bleed_control' and MEDICS[uid] then return 50 end return 0 end"
    , "unit.getWounds = function(uid)"
    , "  if BLEEDING[uid] then"
    , "    return { { kind = 'cut', bandage = 1, clot = 0, infection = 0 } } end"
    , "  return {} end"
    , "return true" ]

-- ** Group effectiveness

groupSpec ∷ SpecWith EngineEnv
groupSpec = do
    it "counts a candidate that regards the subject as an ally (candidate → \
       \subject), excludes the converse, and still excludes the threat" $ \env → do
        resetWorld env
            [ (1, prof False ["subj_team"] [], (0, 0))      -- subject
            , (2, prof False ["cand_team"] [], (1, 0))      -- candidate
            , (9, prof False ["subj_team"] [], (0, 1)) ]    -- threat, allied
        ls ← session env
        withOverlay env ls
            [cause "fixture_ally" "cand_team" "subj_team" RelAlly]
        powers ls
        -- Subject 1: itself (shared tag) + candidate 2 (cand → subj ally);
        -- the threat shares the subject's tag and is excluded anyway.
        val ls "AI.groupEffectivenessVs(9, 1)" `shouldReturn` "30"
        -- Subject 2: itself only — subject 1 regards 2 as neutral.
        val ls "AI.groupEffectivenessVs(9, 2)" `shouldReturn` "20"

    it "keeps the range and commitment rules" $ \env → do
        resetWorld env
            [ (1, legacy FactionPlayer, (0, 0))
            , (2, legacy FactionPlayer, (500, 500))   -- far, committed
            , (3, legacy FactionPlayer, (600, 600))   -- far, uncommitted
            , (9, legacy FactionWildlife, (0, 0)) ]
        ls ← session env
        powers ls
        ok ls "S(2).attackTargetUid = 9; return true"
        val ls "AI.groupEffectivenessVs(9, 1)" `shouldReturn` "30"
        val ls "AI.groupEffectivenessVs(9) .. ',' .. AI.groupEffectivenessVs(9, 404)"
            `shouldReturn` "\"0,0\""

    it "applies the eight intended matrix changes to group strength" $ \env → do
        resetWorld env
            [ (1, legacy FactionPlayer, (0, 0))
            , (2, legacy FactionDebug, (1, 0))
            , (3, legacy FactionDebug, (0, 1))
            , (9, legacy FactionWildlife, (0, 0)) ]
        ls ← session env
        powers ls
        -- A debug unit no longer backs a player subject (debug → player is
        -- neutral now), and debug units back nobody, themselves included.
        val ls "AI.groupEffectivenessVs(9, 1)" `shouldReturn` "10"
        val ls "AI.groupEffectivenessVs(9, 2)" `shouldReturn` "0"
  where
    powers ls = ok ls
        "AI.combatEffectiveness = function(uid) return 10 * (uid < 9 and uid or 0) end\
        \ return true"

-- ** Local self-defense (generic combat path)

selfDefenseSpec ∷ SpecWith EngineEnv
selfDefenseSpec = do
    let roster =
            [ (1, legacy FactionPlayer, (0, 0))          -- the struck unit
            , (2, legacy FactionPlayer, (0, 0))          -- an uninjured peer
            , (3, prof False ["acolyte"] [], (0, 0))     -- allied attacker
            , (4, prof False ["nomad"] [], (0, 0))       -- hostile attacker
            , (7, legacy FactionNeutral, (0, 0)) ]       -- neutral attacker

    it "lets an allied or neutral recent attacker be fought in local \
       \self-defense, keeps the fight going for exactly ten seconds, and a \
       \new hit refreshes it" $ \env → forM_ [3, 7] $ \attacker → do
        resetWorld env roster
        ls ← session env
        val ls ("unit.relationFromTo(1, " <> tshow attacker <> ") ~= 'hostile'")
            `shouldReturn` "true"
        hit env 1 attacker 0
        ok ls "NOW = 1; return true"
        val ls "engage(1)" `shouldReturn` tshow attacker
        ok ls "NOW = 10; return true"
        val ls "tick(1)" `shouldReturn` tshow attacker      -- exactly 10 s
        hit env 1 attacker 9.5
        ok ls "NOW = 19.5; return true"
        val ls "tick(1)" `shouldReturn` tshow attacker      -- refreshed
        ok ls "NOW = 19.51; return true"
        val ls "tick(1)" `shouldReturn` "false"             -- just beyond
        -- Nothing team-wide moved, and the peer never joined.
        val ls ("unit.relationFromTo(2, " <> tshow attacker <> ") ~= 'hostile'")
            `shouldReturn` "true"
        val ls "engage(2)" `shouldReturn` "false"

    it "refuses an attacker just beyond the window" $ \env → do
        resetWorld env roster
        ls ← session env
        hit env 1 3 0
        ok ls "NOW = 10.01; return true"
        val ls "engage(1)" `shouldReturn` "false"
        ok ls "NOW = 10; return true"
        val ls "engage(1)" `shouldReturn` "3"

    it "with hostility cleared by a fixture, pre-clear damage is not enough, \
       \a hit exactly at the cutoff is not either, and a strictly later hit \
       \is" $ \env → do
        resetWorld env roster
        ls ← session env
        ok ls "POLICY.fixtureInputs = function(uid)\
              \ if uid == 1 then return { diplomacyClearAt = 5 } end end\
              \ return true"
        forM_ [(4.5, "false"), (5, "false"), (5.01, "3")] $ \(at, expected) → do
            ok ls "AI.aiState[1] = nil; CLAIMS.resetAll(); return true"
            hit env 1 3 at
            ok ls "NOW = 6; return true"
            r ← val ls "engage(1)"
            (at, r) `shouldBe` (at, expected)
        -- A non-hostile fight acquired before the clear cannot be sustained
        -- by that pre-clear damage once it is cleared.
        ok ls "POLICY.fixtureInputs = function() return nil end\
              \ AI.aiState[1] = nil; CLAIMS.resetAll(); return true"
        hit env 1 3 4
        ok ls "NOW = 4.5; return true"
        val ls "engage(1)" `shouldReturn` "3"
        ok ls "POLICY.fixtureInputs = function(uid)\
              \ if uid == 1 then return { diplomacyClearAt = 5 } end end\
              \ NOW = 6; return true"
        val ls "tick(1)" `shouldReturn` "false"

    it "acquires a hostile attacker whose hit predates a goal's termination, \
       \while an allied attacker with the same timing is refused" $ \env → do
        forM_ [(4, "4"), (3, "false")] $ \(attacker, expected) → do
            resetWorld env roster
            ls ← session env
            hit env 1 attacker 2
            ok ls "NOW = 3; return true"
            val ls "engage(1)" `shouldReturn` tshow attacker
            ok ls "NOW = 4; CORE.markGoalAccomplished(S(1), 'attack'); NOW = 5; return true"
            r ← val ls "engage(1)"
            (attacker, r) `shouldBe` (attacker, expected)

    it "after a goal ends while its attacker is still recent, an old or \
       \equal-time hit cannot restart it — including one landed after a \
       \diplomacy clear — and a strictly later hit may, unless retreat or a \
       \player withdrawal still wins" $ \env → do
        resetWorld env roster
        ls ← session env
        ok ls "POLICY.fixtureInputs = function(uid)\
              \ if uid == 1 then return { diplomacyClearAt = 1 } end end\
              \ return true"
        hit env 1 3 2                                   -- after the clear
        ok ls "NOW = 3; return true"
        val ls "engage(1)" `shouldReturn` "3"
        ok ls "NOW = 4; CORE.markGoalAccomplished(S(1), 'attack'); NOW = 5; return true"
        val ls "POLICY.cutoff(1)" `shouldReturn` "4"
        val ls "engage(1)" `shouldReturn` "false"       -- old hit
        hit env 1 3 4
        val ls "engage(1)" `shouldReturn` "false"       -- equal-time hit
        hit env 1 3 4.5
        -- A player withdrawal at 4.6 still outranks the fresh hit...
        ok ls "S(1).holdAnchor = { combatWithdrawalCompletedAt = 4.6 }; return true"
        val ls "engage(1)" `shouldReturn` "false"
        -- ...and so does an active retreat.
        ok ls "S(1).holdAnchor = nil; CORE.setGoal(S(1), 'retreat'); return true"
        val ls "engage(1)" `shouldReturn` "false"
        ok ls "CORE.markGoalAccomplished(S(1), 'retreat'); return true"
        val ls "engage(1)" `shouldReturn` "3"           -- strictly later

    it "answers a newer non-hostile attacker in reach by swapping onto it, \
       \with neither hostility nor an authorization keeping the old target, \
       \and stamps no cutoff" $ \env → do
        resetWorld env roster
        ls ← session env
        hit env 1 3 0
        ok ls "NOW = 1; return true"
        val ls "engage(1)" `shouldReturn` "3"
        ok ls "OCC = POLICY.occurrence(1); return true"
        hit env 1 7 1.9                          -- 3 is no longer the last attacker
        ok ls "NOW = 2; return true"
        val ls "tick(1)" `shouldReturn` "7"      -- swapped, not ended
        val ls "POLICY.cutoff(1) == nil and POLICY.occurrence(1) == OCC"
            `shouldReturn` "true"
        ok ls "NOW = 11.9; return true"
        val ls "tick(1)" `shouldReturn` "7"      -- 7's own window holds

    it "stamps no cutoff when a committed order or a lash-out ends, so a \
       \recent hit from another attacker still opens self-defense" $ \env → do
        resetWorld env roster
        ls ← session env
        -- A committed player order whose target dies.
        ok ls "NOW = 1; AI.commandAttack(1, 3, true); NOW = 2; return true"
        setPose env 3 "dead"
        val ls "tick(1)" `shouldReturn` "false"
        val ls "POLICY.cutoff(1) == nil" `shouldReturn` "true"
        hit env 1 7 1.5                          -- before that termination
        ok ls "NOW = 3; return true"
        val ls "engage(1)" `shouldReturn` "7"
        -- A lash-out episode, ended the way unit_ai_mental ends it: the
        -- flag and target are dropped BEFORE the goal is accomplished.
        setPose env 3 "standing"
        ok ls "AI.aiState[1] = nil; CLAIMS.resetAll(); local s = S(1)\
              \ s.mentalLashoutActive = true; CORE.setGoal(s, 'attack', 'lash_out')\
              \ s.attackTargetUid = 3; s.committed = nil\
              \ NOW = 4; s.attackTargetUid = nil; s.mentalLashoutActive = nil\
              \ CORE.markGoalAccomplished(s, 'attack'); return POLICY.cutoff(1) == nil"
        hit env 1 7 3.5
        ok ls "NOW = 5; return true"
        val ls "engage(1)" `shouldReturn` "7"
        -- The governed control: an autonomous goal's end DOES cut off.
        ok ls "NOW = 6; CORE.markGoalAccomplished(S(1), 'attack'); return true"
        val ls "POLICY.cutoff(1)" `shouldReturn` "6"

    it "stamps the cutoff when a committed order replaces an autonomous \
       \goal, so a hit before that replacement cannot reopen self-defense" $ \env → do
        resetWorld env roster
        ls ← session env
        hit env 1 3 0.5
        ok ls "NOW = 1; return true"
        val ls "engage(1)" `shouldReturn` "3"           -- autonomous self-defense
        hit env 1 7 1.5                                 -- a neutral hit mid-fight
        -- The player orders an attack on the hostile 4: commandAttack sets
        -- the REPLACEMENT's committed flag before the goal changes.
        ok ls "NOW = 2; AI.commandAttack(1, 4, true); return true"
        val ls "POLICY.cutoff(1)" `shouldReturn` "2"
        setPose env 4 "dead"
        ok ls "NOW = 3; return true"
        val ls "tick(1)" `shouldReturn` "false"
        val ls "POLICY.cutoff(1)" `shouldReturn` "2" -- the order's end stamps none
        val ls "engage(1)" `shouldReturn` "false"       -- 7's hit predates cutoff 2
        hit env 1 7 3.1                                 -- strictly after it
        val ls "engage(1)" `shouldReturn` "7"

    it "stops the pursuit when permission expires out of range" $ \env → do
        resetWorld env [ (1, legacy FactionPlayer, (0, 0))
                       , (3, prof False ["acolyte"] [], (20, 0)) ]
        ls ← session env
        hit env 1 3 0
        ok ls "NOW = 1; return true"
        val ls "engage(1)" `shouldReturn` "3"
        ok ls "NOW = 2; return true"
        val ls "tick(1)" `shouldReturn` "3"
        val ls "S(1).attackLastMoveTo ~= nil" `shouldReturn` "true"   -- pursuing
        ok ls "STOPS = 0; local stop = unit.stop\
              \ unit.stop = function(u) if u == 1 then STOPS = STOPS + 1 end\
              \ return stop(u) end; return true"
        ok ls "NOW = 10.01; return true"
        val ls "tick(1)" `shouldReturn` "false"
        val ls "STOPS >= 1 and S(1).attackLastMoveTo == nil" `shouldReturn` "true"

    it "leaves the production behaviour unchanged while the fixture provider \
       \is unset" $ \env → do
        resetWorld env roster
        ls ← session env
        val ls "POLICY.fixtureInputs(1) == nil and POLICY.fixtureInputs(3) == nil"
            `shouldReturn` "true"
        let run = do
                ok ls "AI.aiState[1] = nil; CLAIMS.resetAll(); return true"
                hit env 1 3 0
                a ← ok ls "NOW = 1; return true" >> val ls "engage(1)"
                b ← ok ls "NOW = 10; return true" >> val ls "tick(1)"
                c ← ok ls "NOW = 10.01; return true" >> val ls "tick(1)"
                pure [a, b, c]
        unset ← run
        ok ls "POLICY.fixtureInputs = function() return {} end; return true"
        empty ← run
        unset `shouldBe` ["3", "3", "false"]
        empty `shouldBe` unset

-- ** Grandfathered authorization

authorizationSpec ∷ SpecWith EngineEnv
authorizationSpec = do
    let roster =
            [ (1, legacy FactionPlayer, (0, 0))
            , (2, legacy FactionPlayer, (0, 0))
            , (3, prof False ["acolyte"] [], (0, 0))
            , (4, prof False ["nomad"] [], (0, 0))
            , (7, legacy FactionNeutral, (0, 0)) ]
        -- Start a self-defense goal against 3 and capture its occurrence.
        start env ls = do
            resetWorld env roster
            hit env 1 3 0
            ok ls "AI.aiState[1] = nil; CLAIMS.resetAll(); NOW = 1; return true"
            val ls "engage(1)" `shouldReturn` "3"
            ok ls "OCC = POLICY.occurrence(1); return OCC ~= nil"
        authorize ls fields = ok ls $
            "POLICY.fixtureInputs = function(uid) if uid == 1 then\
            \ return { authorization = { " <> fields <> " } } end end; return true"

    it "keeps only the original goal and target fighting after awareness \
       \expiry, and refuses an absent or mismatched authorization" $ \env → do
        ls ← session env
        forM_ [ ("uid = 1, target = 3, occurrence = OCC", "3")
              , ("", "false")
              , ("uid = 2, target = 3, occurrence = OCC", "false")
              , ("uid = 1, target = 7, occurrence = OCC", "false")
              , ("uid = 1, target = 3, occurrence = OCC + 1", "false") ]
          $ \(fields, expected) → do
            start env ls
            if T.null fields
                then ok ls "POLICY.fixtureInputs = function() return nil end; return true"
                else authorize ls fields
            ok ls "NOW = 30; return true"
            r ← val ls "tick(1)"
            (fields, r) `shouldBe` (fields, expected)
        -- It grants nobody else permission to join.
        val ls "engage(2)" `shouldReturn` "false"

    it "is unusable after every terminating boundary, and never carries to \
       \a replacement goal against the same target" $ \env → do
        ls ← session env
        let boundaries =
                [ ("target death", setPose env 3 "dead")
                , ("target disappearance", removeUnit env 3)
                , ("goal completion", ok ls "CORE.markGoalAccomplished(S(1), 'attack'); return true")
                , ("retreat", ok ls "CORE.setGoal(S(1), 'retreat'); return true")
                , ("replacement goal", ok ls "AI.commandAttack(1, 3); return true") ]
        forM_ boundaries $ \(name, cross) → do
            start env ls
            authorize ls "uid = 1, target = 3, occurrence = OCC"
            ok ls "NOW = 30; return true"
            cross
            _ ← val ls "tick(1)"
            -- A replacement goal against the same target, if 3 still lives:
            -- a new occurrence the old authorization does not name.
            ok ls "if unit.exists(3) and unit.getPose(3) ~= 'dead' then\
                  \ AI.commandAttack(1, 3) end; return true"
            r ← val ls "tick(1)"
            (name ∷ Text, r) `shouldBe` (name, "false")

    it "does not follow a mid-fight retaliation swap to another attacker, \
       \nor revive on a swap back" $ \env → do
        ls ← session env
        start env ls
        authorize ls "uid = 1, target = 3, occurrence = OCC"
        ok ls "NOW = 30; return true"
        val ls "tick(1)" `shouldReturn` "3"            -- authorized
        -- A neutral attacker lands a fresh hit in reach: the swap is its
        -- own self-defense, not the authorization's.
        hit env 1 7 29.5
        val ls "tick(1)" `shouldReturn` "7"
        hit env 1 3 29.6
        ok ls "S(1).attackTargetUid = 3; NOW = 45; return true"
        -- Back on 3 with no fresh hit: the voided authorization is gone.
        val ls "tick(1)" `shouldReturn` "false"

    it "the generic swap refuses an attacker whose hit predates the last \
       \goal's termination, and accepts one strictly after it" $ \env → do
        ls ← session env
        start env ls
        ok ls "NOW = 2; CORE.markGoalAccomplished(S(1), 'attack'); return true"
        -- A hostile fight, so continuation never rests on the one-slot
        -- last-attacker memory the swap candidates overwrite.
        hit env 1 4 2.5
        ok ls "NOW = 3; return true"
        val ls "engage(1)" `shouldReturn` "4"
        hit env 1 7 1.5                                -- predates cutoff 2
        ok ls "NOW = 3.2; return true"
        val ls "tick(1)" `shouldReturn` "4"            -- no swap onto 7
        hit env 1 7 3.1                                -- strictly after it
        val ls "tick(1)" `shouldReturn` "7"
