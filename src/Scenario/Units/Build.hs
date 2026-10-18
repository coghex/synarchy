{-# LANGUAGE Strict #-}
-- | Constructing scenario units (#2710, SCN-10 of epic #2698): a
--   validated 'Scenario.Types.UnitEntry' becomes a complete
--   'Unit.Types.UnitInstance', its matching 'Unit.Sim.Types.UnitSimState',
--   its item trees and the identity and tag assignments a later commit
--   installs ("Scenario.Units.Install"). Nothing here touches a live
--   manager.
--
--   __Initialization order.__ Each step reads only what the steps before
--   it finished, so no value is computed twice or from a stale input:
--
--   1. the definition, the column z;
--   2. stats: every omitted template from the unit's private generator
--      ("Scenario.Units.Defaults"), the explicit values over them;
--   3. the body, ONCE, through the body authority
--      ("Unit.Thread.Command.Body"): explicit current masses bypass the
--      spawn seed and its viability margins and are re-derived by
--      'recomputeBodyDerivedStats' (@strength_base@ present, so nothing
--      is promoted twice); otherwise the seed inputs take the spawn path,
--      'seedBodyComposition';
--   4. skills, knowledge, name, facing;
--   5. health: wounds, scars, immunity, then the death check — a unit
--      whose stated health already meets an existing death condition is
--      rejected, never built as a corpse;
--   6. items: the explicit collections through "Scenario.Items.Owners",
--      the omitted ones from the definition's kit, neither shed nor
--      topped up (D-57);
--   7. modifiers, built ONCE as spawn builds them
--      ('Unit.Thread.Command.Spawn.spawnModifierMap': innate modifiers
--      with every worn accessory's buffs at its current condition), then
--      the authored ones whose source neither of those supplies;
--   8. physiology: the omitted pools through the one seeding authority
--      spawn and load share, 'Unit.Physiology.Transition.reconcileStoredKeys',
--      whose missing-only rule leaves every explicit pool exactly as
--      stated (the manager's premise ruling of 2026-10-18 on the review's
--      resource-pool addition);
--   9. blood and the health-dependent pose.
--
--   __Explicit values are authoritative.__ A stated value, including 0 or
--   an empty collection, is installed as stated. One that is invalid
--   against the LIVE definition is a 'FieldRejected' diagnostic and takes
--   its omission value; an invalid combination of related fields is
--   rejected as a group with a diagnostic, never resolved by whichever
--   was applied last.
module Scenario.Units.Build
    ( UnitBuildEnv(..)
    , BuiltUnit(..)
    , UnitBuild(..)
    , buildUnit
    , builtItemForest
    ) where

import UPrelude
import Data.IORef (newIORef)
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Random (mkStdGen)
import Combat.Resolution.Common (painFor)
import Combat.Wounds.Constants (unconsciousFraction)
import Combat.Wounds.Infection (necrosisLethalVital)
import Equipment.Types (EquipmentClass(..), EquipmentClassManager, EquipmentSlot(..),
                        lookupEquipmentClass)
import Gameplay.Tags.Types (GameplayTag)
import Infection.Types (InfectionManager, lookupInfection)
import Item.Materialize (ItemSpec(..), DefaultContentNote(..), DefaultContentIssue(..),
                         materializeSpec)
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager, lookupItemDef)
import Scenario.Decode.Monad (indexPath, keyPath)
import Scenario.Items.Build (ItemBuildEnv(..), ItemBuild(..))
import Scenario.Items.Owners (UnitItems(..), buildUnitItems)
import Scenario.Items.Refs (itemRefTextError, maxItemRefLength)
import Scenario.Units.Catalog
import Scenario.Units.Defaults
import Scenario.Types
import Unit.Blood (bloodViewOf)
import Unit.Faction.Membership (resolveSpawnFaction)
import Unit.Physiology (PhysioContext(..), evalHealthy, resolveMode, bloodSystem)
import Unit.Physiology.Transition (canonicalBlood, reconcileStoredKeys)
import Unit.Physiology.Types
import Unit.Sim.Types
import Unit.Stats (effectiveStat)
import Unit.Thread.Command.Body (bloodSeedFromStats, recomputeBodyDerivedStats,
                                 seedBodyComposition)
import Unit.Thread.Command.Spawn (spawnModifierMap)
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- | What construction reads from the engine. Plain values and the item
--   allocator: no gameplay random generator and no logger.
data UnitBuildEnv = UnitBuildEnv
    { ubeDefs       ∷ !(HM.HashMap Text UnitDef)
    , ubeItems      ∷ !ItemBuildEnv
    , ubeEquipment  ∷ !EquipmentClassManager
    , ubePhysiology ∷ !PhysiologyRegistry
    , ubeInfections ∷ !InfectionManager
    , ubeNow        ∷ !Double
      -- ^ the game time the scenario starts at: a wound's @age@ and a
      --   modifier's @remaining@ are relative to it
    , ubePage       ∷ !WorldPageId
    , ubeSurfaceZ   ∷ Float → Float → Maybe Int
      -- ^ the target page's column surface under a tile, for an omitted
      --   @z@; 'Nothing' when no column is loaded there
    }

-- | One constructed unit, not yet installed.
data BuiltUnit = BuiltUnit
    { buRef       ∷ !Text
      -- ^ the unit's scenario reference ('scenarioIdText')
    , buPath      ∷ !Text
    , buInstance  ∷ !UnitInstance
    , buSimState  ∷ !UnitSimState
    , buTags      ∷ !(Set.Set GameplayTag)
    , buItems     ∷ !ItemBuild
      -- ^ the explicit collections' build: their reference mapping and
      --   tags (default-kit items carry neither)
    , buEncounter ∷ !(Maybe ScenarioId)
      -- ^ the location whose encounter roster the unit joins; installing
      --   the roster is the location adapter's (SCN-12)
    }

data UnitBuild = UnitBuild
    { ubUnit        ∷ !(Maybe BuiltUnit)   -- ^ 'Nothing': the unit was rejected
    , ubDiagnostics ∷ ![ScenarioDiagnostic]
    }

-- | Every item tree the built unit holds, in collection order.
builtItemForest ∷ UnitInstance → [ItemInstance]
builtItemForest u = uiInventory u ⧺ map snd (sortOn fst (HM.toList (uiEquipment u)))
                  ⧺ uiAccessories u

diagAt ∷ Text → DiagnosticReason → DiagnosticEffect → ScenarioDiagnostic
diagAt = ScenarioDiagnostic

-- | Every item entry a rejected unit owns, each cascade-rejected.
cascadeUnitItems ∷ Text → UnitEntry → [ScenarioDiagnostic]
cascadeUnitItems path u = concat
    [ go (keyPath path "inventory") (authoredOr [] (ueInventory u))
    , concat [ one (keyPath (keyPath path "equipment") slot) e
             | (slot, e) ← sortOn fst (HM.toList (authoredOr HM.empty (ueEquipment u))) ]
    , go (keyPath path "accessories") (authoredOr [] (ueAccessories u)) ]
  where
    go base es = concat [ one (indexPath base i) e | (i, e) ← zip [0 ..] es ]
    one p e = diagAt p (OwnerRejected path) CascadeRejected
              : go (keyPath p "contents") (authoredOr [] (ieContents e))

-- | Build one unit. @path@ is its document path (@units[2]@).
buildUnit ∷ UnitBuildEnv → Text → UnitEntry → IO UnitBuild
buildUnit env path u = case HM.lookup (ueDefinition u) (ubeDefs env) of
    Nothing → pure (rejected (UnknownDefinition (ueDefinition u)))
    -- an identity the ordinary save could not carry would load and then
    -- fail the first save (the item adapters' rule)
    Just _ | Just _ ← itemRefTextError (scenarioIdText (ueId u)) →
        pure (rejected (InvalidValue ("an id of at most " <> tshow maxItemRefLength
                                      <> " letters, digits and '_ - : . [ ]'")))
    Just def → case authoredOr' (ueZ u) (ubeSurfaceZ env (ueX u) (ueY u)) of
        Nothing → pure (rejected (InvalidValue "an explicit z or a loaded column under the unit"))
        Just z → buildWith env path u def z
  where
    authoredOr' (Authored v) _ = Just v
    authoredOr' Omitted m = m
    rejected why = UnitBuild Nothing
        (diagAt path why EntryRejected : cascadeUnitItems path u)

buildWith ∷ UnitBuildEnv → Text → UnitEntry → UnitDef → Int → IO UnitBuild
buildWith env path u def z
    | Just why ← deathCondition = pure $ UnitBuild Nothing
        (statDiags ⧺ woundDiags ⧺ [diagAt path (InvalidValue why) EntryRejected]
         ⧺ cascadeUnitItems path u)
    | otherwise = do
        -- 6. items
        let (eqDiags, u') = checkEquipmentKinds items mClass path u
        (unitItems, built) ← buildUnitItems (ubeItems env) path u'
        let DefaultLoadout dInv dEq dAcc = defaultLoadoutSpecs items mClass ref def
        (inv, invNotes) ← collection (uitInventory unitItems) dInv
        (eq, eqNotes) ← case uitEquipment unitItems of
            Authored m → pure (m, [])
            Omitted → do
                (xs, ns) ← mintAll (map snd dEq)
                pure (HM.fromList [ (s, x) | ((s, _), Just x) ← zip dEq xs ], ns)
        (acc, accNotes) ← collection (uitAccessories unitItems) dAcc
        -- 7. modifiers
        let innate = spawnModifierMap items def acc
            innateSources = HS.fromList [ smSource m | ms ← HM.elems innate, m ← ms ]
            (modDiags, mods) = authoredModifiers path (ubeNow env) innateSources
                                   (authoredOr HM.empty (ueModifiers u)) innate
        -- 8. physiology, 9. blood
        let (poolDiags, stats1) = checkPools path reg def mods (ubeNow env) stats0
            bloodSim = maybe False (\bs → resolveMode bs (udPhysiology def) False Nothing
                                             ≡ ModeSimulated)
                             (bloodSystem reg)
            -- the canonical blood reads the body, never the blood itself
            ctx0 = PhysioContext
                { pcRegistry = reg, pcPhysiology = udPhysiology def
                , pcOverrides = emptyPhysioOverrides, pcNow = ubeNow env
                , pcStats = stats1, pcModifiers = mods
                , pcBlood = bloodViewOf stats1 (bloodSeedFromStats stats1) 0
                , pcPain = painFor probe }
            blood0 = authoredOr (fromMaybe (bloodSeedFromStats stats1) (canonicalBlood ctx0))
                                (if bloodSim then ueBlood u else Omitted)
            seedCtx = ctx0 { pcBlood = bloodViewOf stats1 blood0 0 }
            bloodDiags = [ diagAt (keyPath path "blood")
                               (InvalidValue "no blood: this definition does not simulate blood")
                               FieldRejected
                         | isAuthored (ueBlood u), not bloodSim ]
            maxBlood = HM.lookupDefault 70 "body_mass" stats1 * bloodMassRatio
            collapsed = bloodSim ∧ blood0 < maxBlood * unconsciousFraction
            probe = baseInstance { uiWounds = wounds }
        case reconcileStoredKeys seedCtx of
            Left (sid, key, why) → pure $ UnitBuild Nothing
                (statDiags ⧺ woundDiags ⧺ poolDiags
                 ⧺ [diagAt path (InvalidValue ("seedable physiology: system '" <> sid
                                               <> "' key " <> key <> ": " <> why))
                           EntryRejected]
                 ⧺ cascadeUnitItems path u)
            Right (stats2, _) → do
                let pose = if collapsed then Collapsed else Standing
                    inst = baseInstance
                        { uiStats = stats2, uiModifiers = mods
                        , uiInventory = inv, uiEquipment = eq, uiAccessories = acc
                        , uiWounds = wounds, uiBlood = blood0
                        , uiPose = if collapsed then "collapsed" else "standing" }
                    ss = (spawnSimState (ueX u) (ueY u) z facing) { usPose = pose }
                    notes = map (noteDiag path) (invNotes ⧺ eqNotes ⧺ accNotes)
                pure UnitBuild
                    { ubUnit = Just BuiltUnit
                        { buRef = ref, buPath = path, buInstance = inst
                        , buSimState = ss, buTags = Set.fromList (ueTags u)
                        , buItems = built, buEncounter = ueEncounter u }
                    , ubDiagnostics = sortOn sdPath $
                        statDiags ⧺ woundDiags ⧺ eqDiags ⧺ ibDiagnostics built ⧺ notes
                        ⧺ modDiags ⧺ poolDiags ⧺ bloodDiags }
  where
    ref = scenarioIdText (ueId u)
    reg = ubePhysiology env
    items = ibeItems (ubeItems env)
    mClass = udEquipmentClass def ⌦ (`lookupEquipmentClass` ubeEquipment env)
    facing = authoredOr DirS (ueFacing u)
    -- 2./3. stats and body
    (statDiags, stats0) = resolveStats reg def ref path (ueStats u)
    -- 4. skills, knowledge, name
    skills = HM.union (ueSkills u) (rollTemplatesFor ref "skill" (udSkillTemplates def))
    knowledge = authoredOr (rollTemplatesFor ref "knowledge" (udKnowledgeTemplates def))
                           (ueKnowledge u)
    name = authoredOr (rollName ref def) (ueName u)
    -- 5. health
    (woundDiags, wounds) = resolveWounds env path (authoredOr [] (ueWounds u))
    parts = HM.fromList [ (bpId p, p) | p ← udBodyParts def ]
    vital w = maybe False bpVital (HM.lookup (woundPart w) parts)
    deathCondition
        | any (\w → vital w ∧ woundSeverity w ≥ 1.0) wounds =
            Just "a living unit: a wound of severity 1 or more on a vital part is lethal"
        | any (\w → vital w ∧ woundNecrosis w ≥ necrosisLethalVital) wounds =
            Just ("a living unit: necrosis of " <> tshow necrosisLethalVital
                  <> " or more on a vital part is death by gangrene")
        | Authored b ← ueBlood u, b ≤ 0, bloodSimulated =
            Just "a living unit: no blood is death by exsanguination"
        | otherwise = Nothing
    bloodSimulated = maybe False (\bs → resolveMode bs (udPhysiology def) False Nothing
                                           ≡ ModeSimulated)
                           (bloodSystem reg)
    baseInstance = UnitInstance
        { uiDefName = udName def, uiName = name, uiPage = ubePage env
        , uiTexture = udTexture def, uiDirSprites = udDirSprites def
        , uiBaseWidth = udBaseWidth def
        , uiGridX = ueX u, uiGridY = ueY u, uiGridZ = z, uiRealZ = fromIntegral z
        , uiFacing = facing, uiCurrentAnim = "", uiAnimStart = 0
        , uiAnimReverse = False, uiActivity = "idle", uiPose = "standing"
        , uiAnimStride = 1, uiStats = stats0, uiModifiers = HM.empty
        , uiSkills = skills, uiKnowledge = knowledge
        , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
        , uiFaction = resolveSpawnFaction (udFactionTags def) Nothing
        , uiWounds = wounds
        , uiScars = map (scarOf (ubeNow env)) (authoredOr [] (ueScars u))
        , uiImmuneResponse = authoredOr 0 (ueImmuneResponse u)
        , uiImmunities = authoredOr HM.empty (ueImmunities u)
        , uiBlood = 0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
        , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
        , uiClimbDest = Nothing, uiPhysioOverrides = emptyPhysioOverrides
        , uiTrailState = Nothing }
    -- An omitted collection takes the definition's kit, minted with
    -- the unit's private rolls; an explicit one is exactly itself.
    collection (Authored xs) _ = pure (xs, [])
    collection Omitted specs = do
        (xs, ns) ← mintAll specs
        pure (catMaybes xs, ns)
    mintAll specs = do
        scratch ← newIORef (mkStdGen 0)
        rs ← forM specs $ \s → case lookupItemDef (spDef s) items of
            Nothing → pure (Nothing, [DefaultContentNote (spPath s)
                                         (DefaultUnknownDefinition (spDef s))])
            Just _ → materializeSpec items scratch (ibeAllocId (ubeItems env)) s
        pure (map fst rs, concatMap snd rs)

-- | A default-kit problem, reported at the unit's own collection.
noteDiag ∷ Text → DefaultContentNote → ScenarioDiagnostic
noteDiag path (DefaultContentNote p issue) = case issue of
    DefaultUnknownDefinition n →
        diagAt (path <> ".default." <> p) (UnknownDefinition n) EntryRejected
    DefaultCyclic closed →
        diagAt (path <> ".default." <> p) (CyclicContents (T.intercalate " → " closed))
               EntryRejected

-- | The sim state a fresh spawn starts in, at the unit's own position
--   and facing ('Unit.Thread.Command.Spawn').
spawnSimState ∷ Float → Float → Int → Direction → UnitSimState
spawnSimState x y z facing = UnitSimState
    { usRealX = x, usRealY = y, usGridZ = z, usRealZ = fromIntegral z
    , usTarget = Nothing, usState = Idle, usFacing = facing, usLocalPath = []
    , usPose = Standing, usDrinkUntil = Nothing, usEatUntil = Nothing
    , usPickupUntil = Nothing, usTransitionUntil = Nothing, usTransitionStride = 1
    , usPostTransition = [], usClimbFromTile = Nothing, usClimbToTile = Nothing
    , usClimbStartTime = Nothing, usClimbSlipAt = Nothing, usFallFromTile = Nothing
    , usFallToTile = Nothing, usPendingClimbXP = 0, usGetUpAt = Nothing
    , usPendingFallDrop = Nothing, usJumpApex = Nothing, usMoveGrade = 0 }

-- * Stats and body

-- | Steps 2 and 3: the unit's stored stats before physiology.
resolveStats ∷ PhysiologyRegistry → UnitDef → Text → Text → HM.HashMap Text Float
             → ([ScenarioDiagnostic], HM.HashMap Text Float)
resolveStats reg def ref path explicit0 = (ruleDiags ⧺ valueDiags ⧺ groupDiags, stats)
  where
    rules = unitStatRules reg def
    statPath k = keyPath (keyPath path "stats") k
    -- against the LIVE definition: the decoder's catalog may be older
    ruleDiags =
        [ diagAt (statPath k) (maybe UnknownField (const DerivedValue) r) FieldRejected
        | (k, _) ← sortOn fst (HM.toList explicit0)
        , let r = HM.lookup k rules
        , r ≢ Just AuthorableStat ]
    ruled = HM.filterWithKey (\k _ → HM.lookup k rules ≡ Just AuthorableStat) explicit0
    valueDiags = [ diagAt (statPath k) (InvalidValue why) FieldRejected
                 | (k, v) ← sortOn fst (HM.toList ruled), Just why ← [badValue k v] ]
    valued = HM.filterWithKey (\k v → isNothing (badValue k v)) ruled
    body = isBodyBearing def
    -- the four current masses come together or not at all, and a fresh
    -- body's seed inputs cannot be stated beside them
    statedMasses = [ k | k ← bodyMassStats, HM.member k valued ]
    massesOk = body ∧ length statedMasses ≡ length bodyMassStats ∧ massesCoherent
    massesCoherent = HM.lookupDefault 0 "lean_mass" valued + HM.lookupDefault 0 "fat_mass" valued
                     ≤ HM.lookupDefault 0 "body_mass" valued
    groupDiags =
        [ diagAt (statPath k)
            (InvalidValue "all four of frame_mass, body_mass, lean_mass and fat_mass, \
                          \with lean_mass + fat_mass ≤ body_mass, or none")
            FieldRejected
        | not massesOk, not (null statedMasses), k ← statedMasses ]
        ⧺ [ diagAt (statPath k)
              (InvalidValue "not beside explicit body masses: it only seeds a fresh body")
              FieldRejected
          | massesOk, k ← ["bulk", "bodyfat"], HM.member k valued ]
    explicit
        | massesOk = HM.filterWithKey (\k _ → k `notElem` ["bulk", "bodyfat"]) valued
        | otherwise = HM.filterWithKey (\k _ → k `notElem` bodyMassStats) valued
    -- an explicit strength_base is the body unit's strength potential;
    -- otherwise the strength template rolls it, as spawn promotes it
    templates = rollTemplatesFor ref "stat" (udStatTemplates def)
    rolledStats
        | body ∧ HM.member "strength_base" explicit = HM.delete "strength" templates
        | otherwise = templates
    stats
        | not body = HM.union explicit rolledStats
        | massesOk = recomputeBodyDerivedStats (HM.union explicit rolledStats)
        | otherwise = seedBodyComposition $ HM.unions
            [ explicit, rolledStats, rollTemplatesFor ref "body" (udBodyTemplates def) ]
    badValue k v
        | isNaN v ∨ isInfinite v = Just "a finite number"
        | k `elem` ["height", "bulk", "frame_mass", "body_mass"], v ≤ 0 = Just "a number > 0"
        | k ≡ "bodyfat", v < 0 ∨ v ≥ 1 = Just "a fraction in [0, 1)"
        | v < 0 = Just "a number ≥ 0"
        | otherwise = Nothing

-- | Explicit physiology pools above their maximum (the healthy value of
--   a pool sized by an input, @hydration@ ≤ @max_hydration@) are
--   rejected; the seeding authority then fills them like an omission.
checkPools ∷ Text → PhysiologyRegistry → UnitDef → HM.HashMap Text [StatModifier] → Double
           → HM.HashMap Text Float → ([ScenarioDiagnostic], HM.HashMap Text Float)
checkPools path reg def mods now stats = (diags, foldl' (flip HM.delete) stats (map fst over))
  where
    ctx = PhysioContext
        { pcRegistry = reg, pcPhysiology = udPhysiology def
        , pcOverrides = emptyPhysioOverrides, pcNow = now, pcStats = stats
        , pcModifiers = mods, pcBlood = bloodViewOf stats 0 0, pcPain = 0 }
    over = [ (okKey o, cap)
           | s ← simulatedSystems reg def, o ← psOwns s, isNothing (okField o)
           , Just v ← [HM.lookup (okKey o) stats]
           , capped (okHealthy o)
           , Right cap ← [evalHealthy ctx (okHealthy o)]
           , effectiveStat now v (HM.lookupDefault [] (okKey o) mods) > cap + 1.0e-4 ]
    capped r = case r of { HealthyConst _ → False; _ → True }
    diags = [ diagAt (keyPath (keyPath path "stats") k)
                  (InvalidValue ("at most its maximum, " <> tshow cap)) FieldRejected
            | (k, cap) ← over ]

-- * Health

resolveWounds ∷ UnitBuildEnv → Text → [WoundSpec] → ([ScenarioDiagnostic], [Wound])
resolveWounds env path specs = (concatMap fst rs, map snd rs)
  where
    rs = [ woundOf (indexPath (keyPath path "wounds") i) w | (i, w) ← zip [0 ..] specs ]
    woundOf wp w =
        let itype = authoredOr "" (wsInfectionType w)
            known = T.null itype ∨ isJust (lookupInfection itype (ubeInfections env))
        in ( [ diagAt (keyPath wp "infection_type") (UnknownDefinition itype) FieldRejected
             | not known ]
           , Wound
               { woundPart = wsPart w, woundKind = wsKind w, woundSeverity = wsSeverity w
               , woundAt = ubeNow env - realToFrac (authoredOr 0 (wsAge w))
               , woundBandage = authoredOr 1 (wsBandage w)
               , woundClot = authoredOr 0 (wsClot w)
               , woundHeal = authoredOr 0 (wsHeal w)
               , woundDressing = authoredOr "" (wsDressing w)
               , woundInfection = authoredOr 0 (wsInfection w)
               , woundClean = authoredOr False (wsClean w)
               , woundInfectionType = if known then itype else ""
               , woundNecrosis = authoredOr 0 (wsNecrosis w) } )

scarOf ∷ Double → ScarSpec → Scar
scarOf now s = Scar
    { scarPart = ssPart s, scarKind = ssKind s, scarSeverity = ssSeverity s
    , scarAt = now - realToFrac (authoredOr 0 (ssAge s)) }

-- * Items and modifiers

-- | The existing equipment rule, checked before anything is minted: an
--   item whose kind the slot does not accept is rejected with what it
--   contains. Unknown definitions are left to the item builder, which
--   reports them.
checkEquipmentKinds ∷ ItemManager → Maybe EquipmentClass → Text → UnitEntry
                    → ([ScenarioDiagnostic], UnitEntry)
checkEquipmentKinds items mClass path u = case ueEquipment u of
    Omitted → ([], u)
    Authored m →
        let bad = [ (slot, e, k, want)
                  | (slot, e) ← sortOn fst (HM.toList m)
                  , Just idef ← [lookupItemDef (ieDefinition e) items]
                  , let k = idKind idef
                  , let want = HM.lookup slot slotKinds
                  , want ≢ Just k ]
            badSlots = HS.fromList [ s | (s, _, _, _) ← bad ]
            diags = concat
                [ diagAt sp (InvalidValue ("an item of kind " <> fromMaybe "?" want
                                           <> " (this one is " <> k <> ")"))
                         FieldRejected
                  : descendants sp e
                | (slot, e, k, want) ← bad
                , let sp = keyPath (keyPath path "equipment") slot ]
        in (diags, u { ueEquipment = Authored (HM.filterWithKey (\s _ → not (HS.member s badSlots)) m) })
  where
    slotKinds = HM.fromList [ (esId s, esKind s) | Just cls ← [mClass], s ← ecSlots cls ]
    descendants sp e = concat
        [ diagAt cp (OwnerRejected sp) CascadeRejected : descendants' sp cp c
        | (i, c) ← zip [0 ..] (authoredOr [] (ieContents e))
        , let cp = indexPath (keyPath sp "contents") i ]
    descendants' owner p e = concat
        [ diagAt cp (OwnerRejected owner) CascadeRejected : descendants' owner cp c
        | (i, c) ← zip [0 ..] (authoredOr [] (ieContents e))
        , let cp = indexPath (keyPath p "contents") i ]

-- | Fold the authored modifiers onto the reconstructed map. A source the
--   definition or a worn accessory already supplies belongs to that owner
--   (it is rebuilt from the owner, never stored twice) and is rejected;
--   so is a second modifier from one source on one stat.
authoredModifiers ∷ Text → Double → HS.HashSet Text → HM.HashMap Text [ModifierSpec]
                  → HM.HashMap Text [StatModifier]
                  → ([ScenarioDiagnostic], HM.HashMap Text [StatModifier])
authoredModifiers path now owned authored base = foldl' step ([], base) entries
  where
    entries = [ (stat, i, m) | (stat, ms) ← sortOn fst (HM.toList authored)
                             , (i, m) ← zip [0 ∷ Int ..] ms ]
    step (ds, acc) (stat, i, m)
        | HS.member (msSource m) owned =
            (ds ⧺ [diag "a source the definition or a worn accessory does not supply"], acc)
        | any ((≡ msSource m) ∘ smSource) (HM.lookupDefault [] stat acc) =
            (ds ⧺ [diag "one modifier per source on a stat"], acc)
        | otherwise =
            (ds, HM.insertWith (flip (⧺)) stat [toModifier m] acc)
      where diag why = diagAt (indexPath (keyPath (keyPath path "modifiers") stat) i)
                              (InvalidValue why) FieldRejected
    toModifier m = StatModifier
        { smDelta = msDelta m, smSource = msSource m
        , smExpiry = (now +) ∘ realToFrac ⊚ msRemaining m
        , smPercent = msPercent m }
