{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Medical
  ( unitFrostbiteFn
  , unitTreatInfectionFn
  , unitTreatBleedingFn
  )
    where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import qualified Data.List as L
import qualified System.Random as Random
import Engine.Core.State (EngineEnv(..))
import Infection.Types (InfectionDef(..), lookupInfection)
import Unit.Types
import Combat.Wounds (kindBleedFactor)
import Unit.Stats (applySkillXP)
import Item.Types (ItemInstance(..))
import Engine.Scripting.Lua.API.Units.Stats (getEffectiveStat)


-- ----- Treat action (C MVP) -----

-- | Outcome of a treat attempt. `trSeep` is the wound's new
--   `woundBandage` (fraction of bleed that still seeps): 0 = perfectly
--   sealed, 0.05 = a competent dressing, 1.0 = untreated (on failure).
data TreatResult = TreatResult
    { trOk        ∷ !Bool
    , trSeep      ∷ !Float
    , trBandages  ∷ !Int     -- bandages consumed (incl. wasted on failed tries)
    , trAttempts  ∷ !Int
    , trPart      ∷ !Text
    , trKind      ∷ !Text
    , trMessage   ∷ !Text
    , trMethod    ∷ !Text     -- "" / "bandage" / "tourniquet"
    }

treatFail ∷ Text → TreatResult
treatFail msg = TreatResult False 1.0 0 0 "" "" msg ""

bandageItemName ∷ Text
bandageItemName = "bandage"

-- | The mechanic behind `unit.treatBleeding`. A medic who KNOWS
--   bleed-control dresses the patient's worst-bleeding wound using
--   bandages drawn from a first-aid kit (carried by `owner`, default
--   the medic). Capability = (bleed_control / 100) × intelligence ×
--   tool-condition factor:
--     * each attempt succeeds with probability ≈ 0.15 + capability
--       (experts essentially never miss; the poorly trained may need
--       several tries, wasting a bandage each fail);
--     * on success the dressing's seep fraction is 0.6·(1−cap)² — an
--       expert with high intelligence reaches 0 (bleed stopped dead),
--       a competent hand ~5%, a poor one ~50%.
--   Worn tweezers/scissors in the kit drag the capability down. Tools
--   are reusable (never consumed); only bandages are spent.
treatBleedingIO ∷ EngineEnv → UnitId → UnitId → Maybe UnitId → IO TreatResult
treatBleedingIO env medic patient mOwner = do
    um0 ← readIORef (unitManagerRef env)
    let owner = fromMaybe medic mOwner
    case ( HM.lookup medic   (umInstances um0)
         , HM.lookup patient (umInstances um0)
         , HM.lookup owner   (umInstances um0) ) of
      (Just med, Just pat, Just own) →
        case HM.lookup "bleed_control" (uiKnowledge med) of
          Nothing → pure (treatFail "medic lacks bleed-control knowledge")
          Just level → do
            -- Worst bleeding wound — needed by BOTH the kit-dressing and
            -- the no-supplies tourniquet path, so it's found up front.
            let parts = case HM.lookup (uiDefName pat) (umDefs um0) of
                    Just d  → HM.fromList [(bpId bp, bp) | bp ← udBodyParts d]
                    Nothing → HM.empty
                -- Mirror Combat.Wounds.bleedRateFor: a wound that has
                -- already self-clotted (or been dressed) isn't really
                -- bleeding, so the (1 − clot) and dressing factors must be
                -- in here too. Without them the medic could rank a high-
                -- severity but clotted wound above the one actually seeping
                -- and waste a bandage on it. Severity is the EFFECTIVE
                -- severity (healing eases it, necrosis floors it) — the
                -- same source of truth bleedRateFor squares — so a mostly-
                -- healed wound no longer outranks a fresh seeping one.
                scoreOf w = let effSev = woundEffSeverity w
                            in (effSev * effSev)
                          * kindBleedFactor (woundKind w)
                          * maybe 1.0 bpBleedFactor
                                (HM.lookup (woundPart w) parts)
                          * woundBandage w
                          * (1 - woundClot w)
                bleeders = [ (w, scoreOf w) | w ← uiWounds pat
                           , woundBandage w > 0.02, scoreOf w > 1.0e-4 ]
            case bleeders of
              [] → pure (treatFail "no bleeding wound to treat")
              _  → do
                let (worst, _) = L.foldl1'
                        (\a@(_, sa) b@(_, sb) → if sb > sa then b else a)
                        bleeders
                    targetKey = (woundPart worst, woundKind worst, woundAt worst)
                mIntel ← getEffectiveStat env medic "intelligence"
                let intel    = fromMaybe 1.0 mIntel
                    nLevel   = max 0 (min 1 (level / 100))
                    baseComp = nLevel * intel   -- skill × intelligence (no tools)
                    kits = [ it | it ← uiInventory own
                                , any ((≡ bandageItemName) . iiDefName)
                                      (iiContents it) ]
                case kits of
                  (kit:_) → do
                    -- PROPER DRESSING from the kit (the C-MVP attempt cycle).
                    let bandageCount = length
                            [ () | c ← iiContents kit, iiDefName c ≡ bandageItemName ]
                        toolConds = [ iiCondition c | c ← iiContents kit
                                    , iiDefName c ≡ "tweezers"
                                      ∨ iiDefName c ≡ "scissors" ]
                        toolCond01 = if null toolConds
                            then 0.5
                            else (sum toolConds / fromIntegral (length toolConds)) / 100
                        toolFactor = 0.7 + 0.3 * toolCond01
                        competence = max 0 (min 1.1 (baseComp * toolFactor))
                        pSucc      = max 0.05 (min 0.99 (0.15 + competence))
                        capClamp   = min 1 competence
                        seepBase   = 0.6 * (1 - capClamp) * (1 - capClamp)
                        maxAttempts = 8 ∷ Int
                    localGen ← atomicModifyIORef' (statRNGRef env) Random.splitGen
                    let go gen attemptsLeft used
                          | used >= bandageCount = (False, used, gen)
                          | attemptsLeft ≤ 0     = (False, used, gen)
                          | otherwise =
                              let (r, gen') = Random.randomR (0, 1) gen
                                                ∷ (Float, Random.StdGen)
                                  used' = used + 1
                              in if r < pSucc
                                   then (True,  used', gen')
                                   else go gen' (attemptsLeft - 1) used'
                        (success, consumed, gen2) = go localGen maxAttempts 0
                        (jr, _) = Random.randomR (0, 1) gen2
                                    ∷ (Float, Random.StdGen)
                        seep = if success
                                 then max 0 (min 0.6 (seepBase * (0.9 + 0.2 * jr)))
                                 else 1.0
                        treatXp = if consumed ≤ 0 then 0
                                  else if success then 2.0 else 1.0
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        let um1 = consumeBandages owner consumed um
                            um2 = if success
                                    then setWoundDressing patient targetKey
                                             seep "bandage" um1
                                    else um1
                            um3 = grantKnowledgeXP medic "bleed_control"
                                                   treatXp um2
                            -- PREVENTION: if the kit holds antiseptic, a
                            -- successful dressing also disinfects the wound —
                            -- it won't accumulate infection. Consumes a dose.
                            um4 = if success
                                     ∧ kitHasFill owner antisepticItemName
                                                  antisepticDose um3
                                    then setWoundClean patient targetKey True
                                             (consumeKitFill owner
                                                antisepticItemName
                                                antisepticDose um3)
                                    else um3
                        in (um4, ())
                    let msg = if success then "treated"
                                         else "failed — out of material"
                    pure (TreatResult success seep consumed
                            (max consumed 1) (woundPart worst)
                            (woundKind worst) msg "bandage")
                  [] → do
                    -- NO SUPPLIES → improvise a makeshift tourniquet. Crude
                    -- but better than nothing: it always goes on, consumes
                    -- no material, and stops the bleed only "somewhat" — a
                    -- poor seep (~0.4–0.58, a touch better with skill). Still
                    -- trains the medic a little.
                    let tqSeep = max 0.4 (min 0.58 (0.58 - 0.2 * min 1 baseComp))
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        let um1 = setWoundDressing patient targetKey
                                      tqSeep "tourniquet" um
                            um2 = grantKnowledgeXP medic "bleed_control" 1.0 um1
                        in (um2, ())
                    pure (TreatResult True tqSeep 0 1 (woundPart worst)
                            (woundKind worst) "makeshift tourniquet" "tourniquet")
      _ → pure (treatFail "medic, patient, or kit owner not found")

-- | Drop the first `n` bandage instances from the first inventory item
--   (kit) that holds any. Leaves tools / other contents untouched.
consumeBandages ∷ UnitId → Int → UnitManager → UnitManager
consumeBandages _     0 um = um
consumeBandages owner n um =
    case HM.lookup owner (umInstances um) of
        Nothing  → um
        Just own →
            let inv' = dropFromFirstKit (uiInventory own)
            in um { umInstances =
                      HM.insert owner (own { uiInventory = inv' })
                                (umInstances um) }
  where
    dropFromFirstKit [] = []
    dropFromFirstKit (it:rest)
        | any ((≡ bandageItemName) . iiDefName) (iiContents it) =
            it { iiContents = dropN n (iiContents it) } : rest
        | otherwise = it : dropFromFirstKit rest
    dropN 0 cs = cs
    dropN _ [] = []
    dropN k (c:cs)
        | iiDefName c ≡ bandageItemName = dropN (k - 1) cs
        | otherwise                     = c : dropN k cs

-- | Apply XP to one of a unit's KNOWN knowledges (same diminishing-
--   returns curve as skill XP: newLevel = level + xp / max(level², ε)).
--   No-op if the unit doesn't exist, doesn't know it, or xp ≤ 0 — a
--   knowledge is only trained once a unit actually has it.
grantKnowledgeXP ∷ UnitId → Text → Float → UnitManager → UnitManager
grantKnowledgeXP uid name xp um
    | xp ≤ 0 = um
    | otherwise = case HM.lookup uid (umInstances um) of
        Nothing → um
        Just inst → case HM.lookup name (uiKnowledge inst) of
            Nothing  → um
            Just lvl →
                let inst' = inst { uiKnowledge =
                        HM.insert name (applySkillXP lvl xp)
                                  (uiKnowledge inst) }
                in um { umInstances =
                          HM.insert uid inst' (umInstances um) }

-- | Dress the matching wound on the patient: set its seep (woundBandage)
--   and the dressing type ("bandage" / "tourniquet"). Matched by
--   part+kind+inflicted-time so a concurrent wound-list reshuffle can't
--   dress the wrong wound.
setWoundDressing
    ∷ UnitId → (Text, Text, Double) → Float → Text
    → UnitManager → UnitManager
setWoundDressing patient (pPart, pKind, pAt) seep dressing um =
    case HM.lookup patient (umInstances um) of
        Nothing  → um
        Just pat →
            let ws' = map (\w → if woundPart w ≡ pPart
                                   ∧ woundKind w ≡ pKind
                                   ∧ woundAt w ≡ pAt
                                  then w { woundBandage  = seep
                                         , woundDressing = dressing }
                                  else w)
                          (uiWounds pat)
            in um { umInstances =
                      HM.insert patient (pat { uiWounds = ws' })
                                (umInstances um) }

-- Medical-supply item def-names + per-use doses.
antisepticItemName, antibioticsItemName ∷ Text
antisepticItemName  = "antiseptic"
antibioticsItemName = "antibiotics"

antisepticDose ∷ Float
antisepticDose = 0.05   -- litres per wound disinfection (a 1 L bottle ≈ 20 uses)

antibioticsDose ∷ Float
antibioticsDose = 1.0   -- one pill per cure dose (a 60-pill bottle = 60 doses)

-- | True if the owner carries a kit holding `name` with at least `dose`
--   of fill remaining (antiseptic litres / antibiotic pills).
kitHasFill ∷ UnitId → Text → Float → UnitManager → Bool
kitHasFill owner name dose um = case HM.lookup owner (umInstances um) of
    Nothing  → False
    Just own → any (\it → any hasIt (iiContents it)) (uiInventory own)
  where hasIt c = iiDefName c ≡ name ∧ iiCurrentFill c ≥ dose

-- | Spend `dose` of fill from the first matching item in the first kit
--   (inventory item whose contents hold one with enough). No-op if none.
consumeKitFill ∷ UnitId → Text → Float → UnitManager → UnitManager
consumeKitFill owner name dose um = case HM.lookup owner (umInstances um) of
    Nothing  → um
    Just own →
        let inv' = goInv (uiInventory own)
        in um { umInstances =
                  HM.insert owner (own { uiInventory = inv' }) (umInstances um) }
  where
    enough c = iiDefName c ≡ name ∧ iiCurrentFill c ≥ dose
    goInv [] = []
    goInv (it:rest)
        | any enough (iiContents it) =
            it { iiContents = goContents (iiContents it) } : rest
        | otherwise = it : goInv rest
    goContents [] = []
    goContents (c:cs)
        | enough c  = c { iiCurrentFill = iiCurrentFill c - dose } : cs
        | otherwise = c : goContents cs

-- | Mark the matching wound clean/disinfected (antiseptic prevention) —
--   a clean wound never accumulates infection. Same part+kind+at match as
--   setWoundDressing so a concurrent reshuffle can't hit the wrong wound.
setWoundClean ∷ UnitId → (Text, Text, Double) → Bool → UnitManager → UnitManager
setWoundClean patient (pPart, pKind, pAt) cln um =
    case HM.lookup patient (umInstances um) of
        Nothing  → um
        Just pat →
            let ws' = map (\w → if woundPart w ≡ pPart ∧ woundKind w ≡ pKind
                                   ∧ woundAt w ≡ pAt
                                  then w { woundClean = cln } else w)
                          (uiWounds pat)
            in um { umInstances =
                      HM.insert patient (pat { uiWounds = ws' })
                                (umInstances um) }

-- | Nudge a unit's systemic immune response up by `boost` (clamped to 1).
--   Antibiotics "speed up the response ticker" through this on top of the
--   bacterial-specific knockdown.
bumpImmuneResponse ∷ UnitId → Float → UnitManager → UnitManager
bumpImmuneResponse uid boost um =
    case HM.lookup uid (umInstances um) of
        Nothing  → um
        Just inst →
            let r' = max 0 (min 1 (uiImmuneResponse inst + boost))
            in um { umInstances =
                      HM.insert uid (inst { uiImmuneResponse = r' })
                                (umInstances um) }

-- | Set the matching wound's infection level (antibiotics cure). Same
--   match key as the other wound mutators.
setWoundInfection ∷ UnitId → (Text, Text, Double) → Float → UnitManager → UnitManager
setWoundInfection patient (pPart, pKind, pAt) inf um =
    case HM.lookup patient (umInstances um) of
        Nothing  → um
        Just pat →
            let ws' = map (\w → if woundPart w ≡ pPart ∧ woundKind w ≡ pKind
                                   ∧ woundAt w ≡ pAt
                                  then w { woundInfection = inf } else w)
                          (uiWounds pat)
            in um { umInstances =
                      HM.insert patient (pat { uiWounds = ws' })
                                (umInstances um) }

-- | unit.frostbite(uid, part, necDelta) → new necrosis level (0..1) | nil.
--   Grow (or create) a "frostbite" wound on `part`, adding `necDelta` to its
--   woundNecrosis. The Lua thermo tick calls this for extremities when the unit
--   is cold + poorly perfused (cold-killed tissue = necrosis). The engine's
--   existing necrosis machinery then handles the consequences for free: at
--   necrosis 1.0 a non-vital extremity rots off (propagateSevering), the open
--   wound can get infected (the infection system), and it shows as rot in the
--   Status panel. Returns the frostbite wound's new necrosis level.
unitFrostbiteFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitFrostbiteFn env = do
    idArg   ← Lua.tointeger 1
    partArg ← Lua.tostring 2
    deltaArg ← Lua.tonumber 3
    case (idArg, partArg, deltaArg) of
        (Just n, Just partBS, Just (Lua.Number d)) → do
            now ← Lua.liftIO $ readIORef (gameTimeRef env)
            let uid   = UnitId (fromIntegral n)
                part  = TE.decodeUtf8Lenient partBS
                delta = max 0 (realToFrac d)
                isFb w = woundKind w ≡ "frostbite" ∧ woundPart w ≡ part
            result ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, Nothing)
                    Just inst →
                        let ws = uiWounds inst
                            grown = [ if isFb w
                                        then w { woundNecrosis =
                                                   min 1 (woundNecrosis w + delta) }
                                        else w | w ← ws ]
                            newW = Wound
                                { woundPart = part, woundKind = "frostbite"
                                , woundSeverity = 0.3, woundAt = now
                                , woundBandage = 1.0, woundClot = 0.0
                                , woundHeal = 0.0, woundDressing = ""
                                , woundInfection = 0.0, woundClean = False
                                , woundInfectionType = ""
                                , woundNecrosis = min 1 delta }
                            ws' = if any isFb ws then grown else newW : ws
                            inst' = inst { uiWounds = ws' }
                            lvl = maximum (min 1 delta
                                  : [ woundNecrosis w | w ← ws', isFb w ])
                        in ( um { umInstances =
                                    HM.insert uid inst' (umInstances um) }
                           , Just lvl )
            case result of
                Just lvl → Lua.pushnumber (Lua.Number (realToFrac lvl)) >> return 1
                Nothing  → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | The mechanic behind `unit.treatInfection` — the CURE half of the
--   medical loop (antiseptic prevention is folded into treatBleeding). A
--   medic who knows INFECTION-CONTROL administers antibiotics from the kit
--   to the patient's worst-infected wound, cutting its infection by an
--   amount scaled by capability (infection_control / 100 × intelligence),
--   and marking it clean so it won't re-infect. Consumes one antibiotic
--   pill per call.
--   Reuses TreatResult: trSeep carries the wound's NEW infection level,
--   trMethod = "antibiotics".
treatInfectionIO ∷ EngineEnv → UnitId → UnitId → Maybe UnitId → IO TreatResult
treatInfectionIO env medic patient mOwner = do
    um0 ← readIORef (unitManagerRef env)
    infMgr ← readIORef (infectionManagerRef env)
    let owner = fromMaybe medic mOwner
        -- A wound is antibiotic-curable if its infection is bacterial — i.e.
        -- the def lists "antibiotics" in curable_by. An untyped infection
        -- (woundInfectionType "") is treated as bacterial (the default).
        curableW w = case lookupInfection (woundInfectionType w) infMgr of
            Nothing  → True   -- untyped / unknown → assume bacterial
            Just inf → antibioticsItemName `elem` infCurableBy inf
        cureRateW w = maybe 1.0 infCureRate
                        (lookupInfection (woundInfectionType w) infMgr)
    case ( HM.lookup medic   (umInstances um0)
         , HM.lookup patient (umInstances um0) ) of
      (Just med, Just pat) →
        case HM.lookup "infection_control" (uiKnowledge med) of
          Nothing → pure (treatFail "medic lacks infection-control knowledge")
          Just level →
            case [ w | w ← uiWounds pat, woundInfection w > 0.05 ] of
              [] → pure (treatFail "no infected wound to treat")
              infected
                | null (filter curableW infected) →
                    pure (treatFail "infection not treatable with antibiotics")
                | not (kitHasFill owner antibioticsItemName antibioticsDose um0) →
                    pure (treatFail "no antibiotics in kit")
                | otherwise → do
                    let worst = L.foldl1'
                            (\a b → if woundInfection b > woundInfection a
                                    then b else a) (filter curableW infected)
                        targetKey = ( woundPart worst, woundKind worst
                                    , woundAt worst )
                    mIntel ← getEffectiveStat env medic "intelligence"
                    let intel     = fromMaybe 1.0 mIntel
                        nLevel    = max 0 (min 1 (level / 100))
                        cap       = max 0 (min 1.1 (nLevel * intel))
                        -- cure strength scales with capability AND the
                        -- infection's own cure_rate (some bugs resist more).
                        reduction = max 0.15 (min 0.85 (0.2 + 0.6 * cap))
                                    * cureRateW worst
                        newInf    = max 0 (woundInfection worst - reduction)
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        let um1 = consumeKitFill owner antibioticsItemName
                                                 antibioticsDose um
                            um2 = setWoundInfection patient targetKey newInf um1
                            um3 = setWoundClean patient targetKey True um2
                            um4 = grantKnowledgeXP medic "infection_control" 1.5 um3
                            -- Antibiotics also speed up the systemic immune
                            -- response (helps clear other bacterial foci).
                            um5 = bumpImmuneResponse patient
                                    (min 0.5 (0.3 * cap)) um4
                        in (um5, ())
                    pure (TreatResult True newInf 1 1 (woundPart worst)
                            (woundKind worst) "antibiotics administered"
                            "antibiotics")
      _ → pure (treatFail "medic or patient not found")

-- | unit.treatInfection(medicUid, patientUid [, kitOwnerUid]) →
--     { ok, infection, part, kind, message, method } | nil
unitTreatInfectionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTreatInfectionFn env = do
    medicArg ← Lua.tointeger 1
    patArg   ← Lua.tointeger 2
    ownerArg ← Lua.tointeger 3
    case (medicArg, patArg) of
        (Just m, Just p) → do
            let mOwner = (UnitId . fromIntegral) <$> ownerArg
            res ← Lua.liftIO $ treatInfectionIO env
                    (UnitId (fromIntegral m)) (UnitId (fromIntegral p)) mOwner
            Lua.newtable
            Lua.pushboolean (trOk res)
            Lua.setfield (-2) "ok"
            Lua.pushnumber (Lua.Number (realToFrac (trSeep res)))
            Lua.setfield (-2) "infection"
            Lua.pushstring (TE.encodeUtf8 (trPart res))
            Lua.setfield (-2) "part"
            Lua.pushstring (TE.encodeUtf8 (trKind res))
            Lua.setfield (-2) "kind"
            Lua.pushstring (TE.encodeUtf8 (trMessage res))
            Lua.setfield (-2) "message"
            Lua.pushstring (TE.encodeUtf8 (trMethod res))
            Lua.setfield (-2) "method"
            return 1
        _ → Lua.pushnil >> return 1

-- | unit.treatBleeding(medicUid, patientUid [, kitOwnerUid]) →
--     { ok, seep, bandagesUsed, attempts, part, kind, message } | nil
--
--   One full treatment attempt-cycle (the medic keeps trying until the
--   wound is dressed or the kit runs out of bandages). `kitOwnerUid`
--   defaults to the medic — pass the technomule's id to draw from its
--   kit while a different acolyte administers. nil only when the
--   id args are missing; all other failures come back as ok=false with
--   a message.
unitTreatBleedingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTreatBleedingFn env = do
    medicArg ← Lua.tointeger 1
    patArg   ← Lua.tointeger 2
    ownerArg ← Lua.tointeger 3
    case (medicArg, patArg) of
        (Just m, Just p) → do
            let mOwner = (UnitId . fromIntegral) <$> ownerArg
            res ← Lua.liftIO $ treatBleedingIO env
                    (UnitId (fromIntegral m)) (UnitId (fromIntegral p)) mOwner
            Lua.newtable
            Lua.pushboolean (trOk res)
            Lua.setfield (-2) "ok"
            Lua.pushnumber (Lua.Number (realToFrac (trSeep res)))
            Lua.setfield (-2) "seep"
            Lua.pushinteger (fromIntegral (trBandages res))
            Lua.setfield (-2) "bandagesUsed"
            Lua.pushinteger (fromIntegral (trAttempts res))
            Lua.setfield (-2) "attempts"
            Lua.pushstring (TE.encodeUtf8 (trPart res))
            Lua.setfield (-2) "part"
            Lua.pushstring (TE.encodeUtf8 (trKind res))
            Lua.setfield (-2) "kind"
            Lua.pushstring (TE.encodeUtf8 (trMessage res))
            Lua.setfield (-2) "message"
            Lua.pushstring (TE.encodeUtf8 (trMethod res))
            Lua.setfield (-2) "method"
            return 1
        _ → Lua.pushnil >> return 1
