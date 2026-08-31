{-# LANGUAGE Strict #-}
module Unit.Thread.Command.Spawn
    ( handleUnitSpawnCommand
    , spawnModifierMap
    , spawnEffectiveCapacity
    , shedPlan
    , ShedEvent(..)
      -- * The three starting-loadout materialisers
      --
      -- Exported so the fresh-item contract (#1421) can be checked
      -- against the real builders rather than a restatement of them;
      -- 'handleUnitSpawnCommand' is their only production caller.
    , buildStartingInventory
    , buildStartingEquipment
    , buildStartingAccessories
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv, freshItemInstanceId, loggerRef)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Log (logDebug, logInfo, logWarn, LogCategory(..), LoggerState)
import Unit.Types
import Unit.Faction (Faction(..))
import Unit.Sim.Types
import Unit.Stats (rollStat, pickName, applyItemBuffs, effectiveStat)
import Unit.Thread.Command.Body (seedBodyComposition, bloodSeedFromStats)
import Equipment.Types (EquipmentClass(..), EquipmentSlot(..),
                        lookupEquipmentClass)
import Item.Materialize (materializeItem, pristineItem, filledItem)
import Item.Types (ItemDef(..), ItemInstance(..)
                  , ItemManager(..), lookupItemDef
                  , itemTotalWeight)
import World.Types (WorldManager(..))
import World.Page.Types (WorldPageId(..))

handleUnitSpawnCommand ∷ EngineEnv → IORef UnitThreadState → UnitId → Text
                       → Float → Float → Int → Faction → WorldPageId → IO ()
handleUnitSpawnCommand env utsRef uid defName gx gy gz faction pageId = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    -- Drop the spawn if its world no longer exists. A spawn queued before
    -- world.destroyAll (Exit to Menu) would otherwise be drained after
    -- teardown and re-insert an orphan unit into the cleared manager (#58).
    wmgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    let worldGone = pageId `notElem` map fst (wmWorlds wmgr)
    case HM.lookup defName (umDefs um) of
        _ | worldGone → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatThread
                "UnitSpawn: dropping spawn for a destroyed world (teardown)"
        Nothing → do
            logger ← readIORef (loggerRef env)
            logWarn logger CatThread $
                "UnitSpawn: unknown def '" <> defName <> "'"
        Just def → do
            initialStats ←
                if udEagerStats def
                then atomicModifyIORef' (ucStatRNGRef (toUnitCombatCapability env)) $ \g0 →
                    let (rolled, g')   = HM.foldlWithKey'
                            (\(acc, g) name (b, r) →
                                let (v, g'') = rollStat b r g
                                in (HM.insert name v acc, g''))
                            (HM.empty, g0)
                            (udStatTemplates def)
                        -- Roll bulk + bodyfat from the body templates.
                        -- Merge them in so seedBodyComposition can see
                        -- all three of height/bulk/bodyfat; it then
                        -- drops bulk + bodyfat from the result so they
                        -- never end up in uiStats (a getStat for
                        -- "bulk" later returns nil — the plan's
                        -- contract).
                        (rolledB, g'') = HM.foldlWithKey'
                            (\(acc, g) name (b, r) →
                                let (v, gn) = rollStat b r g
                                in (HM.insert name v acc, gn))
                            (HM.empty, g')
                            (udBodyTemplates def)
                        merged = HM.union rolled rolledB
                        seeded = seedBodyComposition merged
                    in (g'', seeded)
                else return HM.empty
            -- Skills always roll at spawn so they have a starting
            -- level for the addSkillXP formula to operate on.
            initialSkills ← atomicModifyIORef' (ucStatRNGRef (toUnitCombatCapability env)) $ \g0 →
                let (rolled, g') = HM.foldlWithKey'
                        (\(acc, g) name (b, r) →
                            let (v, g'') = rollStat b r g
                            in (HM.insert name v acc, g''))
                        (HM.empty, g0)
                        (udSkillTemplates def)
                in (g', rolled)
            -- Knowledge the unit spawns KNOWING, rolled like skills.
            initialKnowledge ← atomicModifyIORef' (ucStatRNGRef (toUnitCombatCapability env)) $ \g0 →
                let (rolled, g') = HM.foldlWithKey'
                        (\(acc, g) name (b, r) →
                            let (v, g'') = rollStat b r g
                            in (HM.insert name v acc, g''))
                        (HM.empty, g0)
                        (udKnowledgeTemplates def)
                in (g', rolled)
            -- Persistent personal name (#264): draw from the def's name
            -- pool if it has one (humanoids); animals stay unnamed ("").
            initialName ← case udNamePool def of
                Nothing   → return ""
                Just pool → atomicModifyIORef' (ucStatRNGRef (toUnitCombatCapability env)) $ \g0 →
                    let (nm, g') = pickName pool g0 in (g', nm)
            -- Starting inventory: look each entry up in the ItemManager
            -- and build an ItemInstance. Unknown names are dropped
            -- with a warning (load-order issue: items loaded after
            -- units that reference them). Both catalogues are reached
            -- through the `content-registries` READER view (#890,
            -- narrowed to read-only handles by #1896); the unit state
            -- they materialize into is still broad EngineEnv.
            let regs = toContentRegistriesViewCapability env
            itemMgr ← readReadOnlyRef (crvItemManagerRef regs)
            logger  ← readIORef (loggerRef env)
            taggedInventory ← buildStartingInventory env logger itemMgr
                                  (udStartingInventory def)
            -- Pre-equipped items declared by the unit def's
            -- starting_equipment. Resolved against the EquipmentClass
            -- so each item's kind can be validated against the slot.
            ecMgr ← readReadOnlyRef (crvEquipmentClassManagerRef regs)
            let mClass = udEquipmentClass def ⌦ (`lookupEquipmentClass` ecMgr)
            initialEquipment ← buildStartingEquipment env logger itemMgr mClass
                                  (udStartingEquipment def)
            initialAccessories ← buildStartingAccessories env logger itemMgr
                                  (udStartingAccessories def)
            -- The one spawn-time modifier map: the def's innate
            -- modifiers (technomule's "cybernetic enhancements") plus
            -- the just-built accessories' buffs (same effect as if the
            -- player had right-click-equipped each). Built ONCE here so
            -- the capacity shed decision below and the constructed
            -- instance's uiModifiers can never drift (#1213).
            let spawnMods = spawnModifierMap itemMgr def initialAccessories
            -- Spawn-time capacity check. Armor / weapons / survival
            -- kit always arrive; inventory entries with a drop
            -- priority shed (highest first — pick before shovel)
            -- until the loadout fits the EFFECTIVE carrying_capacity —
            -- the rolled base with spawnMods applied, the same measure
            -- every live gameplay policy (pickup gating, the strict
            -- transfer policy) judges against (#1213).
            -- Weights mirror getCarryingWeight: instance weight + fill
            -- (at the container's per-unit fill weight) + container
            -- contents, worn gear at full mass.
            now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
            let itemW = itemTotalWeight itemMgr
                fixedW = sum (map itemW (HM.elems initialEquipment))
                       + sum (map itemW initialAccessories)
            initialInventory ← case spawnEffectiveCapacity now initialStats
                                                           spawnMods of
                Nothing  → return (map fst taggedInventory)
                Just cap → shedToCapacity logger uid itemW cap fixedW
                                          taggedInventory
            let inst = UnitInstance
                    { uiDefName    = defName
                    , uiName       = initialName
                    , uiPage       = pageId
                    , uiTexture    = udTexture def
                    , uiDirSprites = udDirSprites def
                    , uiBaseWidth  = udBaseWidth def
                    , uiGridX      = gx
                    , uiGridY      = gy
                    , uiGridZ      = gz
                    , uiRealZ      = fromIntegral gz
                    , uiFacing     = DirS -- Default facing south
                    , uiCurrentAnim = ""  -- resolved every tick by Unit.Thread.publishToRender
                    , uiAnimStart   = 0
                    , uiAnimReverse = False
                    , uiActivity    = "idle"
                    , uiPose        = "standing"
                    , uiAnimStride  = 1
                    , uiStats       = initialStats
                    -- The same map the capacity decision above used —
                    -- one binding, so they agree by construction.
                    , uiModifiers   = spawnMods
                    , uiSkills      = initialSkills
                    , uiKnowledge   = initialKnowledge
                    , uiInventory   = initialInventory
                    , uiEquipment   = initialEquipment
                    , uiAccessories = initialAccessories
                    , uiFactionId   = faction
                    , uiWounds      = []
                    , uiScars       = []
                    , uiImmuneResponse = 0
                    , uiImmunities  = HM.empty
                    , uiBlood       = bloodSeedFromStats initialStats
                    , uiLastAttackerUid = Nothing
                    , uiLastAttackerAt  = 0
                    , uiAnimOverride = ""
                    , uiFrozen      = False
                    , uiForceLoop   = False
                    , uiClimbDest   = Nothing
                    , uiTrailState  = Nothing
                    }
            atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um' →
                (um' { umInstances = HM.insert uid inst (umInstances um') }, ())

            let ss = UnitSimState
                    { usRealX     = gx
                    , usRealY     = gy
                    , usGridZ     = gz
                    , usRealZ     = fromIntegral gz
                    , usTarget    = Nothing
                    , usState     = Idle
                    , usFacing    = DirS
                    , usLocalPath = []
                    , usPose         = Standing
                    , usDrinkUntil   = Nothing
                    , usEatUntil     = Nothing
                    , usPickupUntil  = Nothing
                    , usTransitionUntil  = Nothing
                    , usTransitionStride = 1
                    , usPostTransition   = []
                    , usClimbFromTile    = Nothing
                    , usClimbToTile      = Nothing
                    , usClimbStartTime   = Nothing
                    , usClimbSlipAt      = Nothing
                    , usFallFromTile     = Nothing
                    , usFallToTile       = Nothing
                    , usPendingClimbXP   = 0
                    , usGetUpAt          = Nothing
                    , usPendingFallDrop = Nothing
                    , usJumpApex         = Nothing
                    , usMoveGrade        = 0
                    }
            atomicModifyIORef' utsRef $ \uts →
                (uts { utsSimStates = HM.insert uid ss (utsSimStates uts) }, ())

-- | Effective carrying capacity at spawn: the rolled base stat with
--   the spawn modifier map applied — the same
--   'Unit.Stats.effectiveStat' measure live gameplay (pickup gating,
--   the strict transfer policy) uses. Nothing when the def rolls no
--   carrying_capacity stat at all (wildlife), which skips the shed
--   entirely, exactly as before.
spawnEffectiveCapacity ∷ Double → HM.HashMap Text Float
                       → HM.HashMap Text [StatModifier] → Maybe Float
spawnEffectiveCapacity now stats mods =
    (\base → effectiveStat now base
                 (HM.lookupDefault [] "carrying_capacity" mods))
      ⊚ HM.lookup "carrying_capacity" stats

-- | One entry of a spawn-shed decision's audit trail — pure so tests
--   can assert exactly what the spawn path would log.
data ShedEvent
    = ShedDrop !Text !Float
      -- ^ item def name left behind; total loadout weight before the
      --   drop (an informational message, not a warning)
    | ShedOverCapacity !Float
      -- ^ still over capacity at this total with nothing left to shed
      --   (the warning case)
    deriving (Show, Eq)

-- | Pure core of the spawn-time capacity shed: drop tagged inventory
--   items (priority descending, so the acolyte's pick goes before its
--   shovel) until the total loadout — fixedW (worn equipment +
--   accessories) plus the remaining inventory — fits the effective
--   carrying capacity. Untagged items (priority 0) are never shed; if
--   the loadout still doesn't fit after every sheddable item is gone,
--   the unit spawns over capacity (the pickup/store gates simply
--   refuse until it lightens) and the trail ends in ShedOverCapacity.
shedPlan ∷ (ItemInstance → Float) → Float → Float
         → [(ItemInstance, Int)] → ([ItemInstance], [ShedEvent])
shedPlan itemW cap fixedW = go
  where
    totalOf xs = fixedW + sum (map (itemW . fst) xs)
    go xs
        | totalOf xs ≤ cap = (map fst xs, [])
        | otherwise =
            let prios = [ p | (_, p) ← xs, p > 0 ]
            in case prios of
                [] → (map fst xs, [ShedOverCapacity (totalOf xs)])
                _  →
                    let top = maximum prios
                        (name, rest) = removeFirstByPrio top xs
                        (kept, evs)  = go rest
                    in (kept, ShedDrop name (totalOf xs) : evs)

    removeFirstByPrio _ [] = ("?", [])
    removeFirstByPrio p ((it, q) : rest)
        | q ≡ p     = (iiDefName it, rest)
        | otherwise = let (n, rest') = removeFirstByPrio p rest
                      in (n, (it, q) : rest')

-- | IO wrapper over 'shedPlan': runs the pure decision and logs its
--   audit trail (drops as info, the nothing-left-to-shed case as a
--   warning).
shedToCapacity ∷ LoggerState → UnitId → (ItemInstance → Float)
               → Float → Float → [(ItemInstance, Int)]
               → IO [ItemInstance]
shedToCapacity logger uid itemW cap fixedW tagged = do
    let (kept, events) = shedPlan itemW cap fixedW tagged
    forM_ events $ \ev → case ev of
        ShedDrop name total →
            logInfo logger CatThread $
                "UnitSpawn " <> tshow uid
                <> ": over capacity ("
                <> tshow total <> " > "
                <> tshow cap
                <> " kg) — leaving " <> name <> " behind"
        ShedOverCapacity total →
            logWarn logger CatThread $
                "UnitSpawn " <> tshow uid
                <> ": loadout "
                <> tshow total
                <> " kg exceeds capacity "
                <> tshow cap
                <> " kg with nothing left to shed"
    return kept

-- | Resolve a unit def's starting_inventory into concrete ItemInstance
--   list, each tagged with its capacity-shed drop priority. Unknown
--   item names log a warning and are dropped. Every value below the
--   entry's own fill is "Item.Materialize"'s to decide (#1418).
buildStartingInventory ∷ EngineEnv → LoggerState → ItemManager
                       → [(Text, Maybe Float, Int)]
                       → IO [(ItemInstance, Int)]
buildStartingInventory env logger itemMgr entries = do
    mInsts ← mapM resolve entries
    return [i | Just i ← mInsts]
  where
    resolve (name, mFill, prio) = do
        mi ← rollInstance env logger itemMgr name mFill
        case mi of
            Nothing → do
                logWarn logger CatThread $
                    "Unit starting_inventory: unknown item '" <> name
                      <> "' — skipping"
                return Nothing
            Just inst → return (Just (inst, prio))

-- | Build one rolled ItemInstance from a def name, its authored default
--   contents included — so a first-aid kit / toolbox spawns already
--   holding its bandages, tools, etc. Returns Nothing for an unknown
--   item name.
--
--   Since #1418 this is a thin adapter over "Item.Materialize", the ONE
--   mint boundary; the caller's explicit fill is the single root-scoped
--   override starting inventory contributes.
rollInstance ∷ EngineEnv → LoggerState → ItemManager → Text → Maybe Float
             → IO (Maybe ItemInstance)
rollInstance env logger itemMgr name mFill =
    materializeItem itemMgr logger
                    (ucStatRNGRef (toUnitCombatCapability env))
                    (freshItemInstanceId env)
                    (filledItem mFill)
                    name

-- | Resolve a unit def's starting_equipment into a slot→ItemInstance
--   map, validating each item's `idKind` against the slot's accepted
--   `esKind`. Unknown items / kind mismatches / unknown slots log a
--   warning and are dropped. Every instance value is
--   "Item.Materialize"'s to decide (#1418) — this path contributes no
--   override at all, so an equipped container now takes its
--   definition's own default_fill and default contents instead of the
--   hardcoded empty it used to get.
buildStartingEquipment ∷ EngineEnv → LoggerState → ItemManager
                       → Maybe EquipmentClass
                       → HM.HashMap Text Text
                       → IO (HM.HashMap Text ItemInstance)
buildStartingEquipment env logger itemMgr mClass entries =
    case mClass of
        Nothing
            | HM.null entries → return HM.empty
            | otherwise → do
                logWarn logger CatThread $
                    "Unit has starting_equipment but no equipment_class"
                    <> " — skipping all entries"
                return HM.empty
        Just cls → do
            let slotIndex = HM.fromList
                    [ (esId s, s) | s ← ecSlots cls ]
            HM.foldlWithKey'
                (\acc slotId itemName → do
                    m ← acc
                    case HM.lookup slotId slotIndex of
                        Nothing → do
                            logWarn logger CatThread $
                                "starting_equipment: unknown slot '"
                                <> slotId <> "' on class '"
                                <> ecName cls <> "' — skipping"
                            return m
                        Just slot → case lookupItemDef itemName itemMgr of
                            Nothing → do
                                logWarn logger CatThread $
                                    "starting_equipment: unknown item '"
                                    <> itemName <> "' for slot '"
                                    <> slotId <> "' — skipping"
                                return m
                            Just iDef
                                | idKind iDef ≢ esKind slot → do
                                    logWarn logger CatThread $
                                        "starting_equipment: item '"
                                        <> itemName <> "' (kind="
                                        <> idKind iDef
                                        <> ") doesn't match slot '"
                                        <> slotId <> "' (kind="
                                        <> esKind slot <> ") — skipping"
                                    return m
                                | otherwise → do
                                    mInst ← materializeItem itemMgr logger
                                              (ucStatRNGRef
                                                 (toUnitCombatCapability env))
                                              (freshItemInstanceId env)
                                              pristineItem itemName
                                    return $ case mInst of
                                        Nothing   → m
                                        Just inst → HM.insert slotId inst m
                ) (return HM.empty) entries

-- | The one spawn-time modifier map (#1213): the def's innate
--   modifiers with the starting accessories' buffs folded on. Both
--   the capacity shed decision and the constructed instance's
--   uiModifiers read this — same set, same composition semantics.
spawnModifierMap ∷ ItemManager → UnitDef → [ItemInstance]
                 → HM.HashMap Text [StatModifier]
spawnModifierMap itemMgr def =
    foldl' (applyAccessoryBuffs itemMgr) (defModifierMap def)

-- | Seed map of a def's innate modifiers (yaml `modifiers:` block) —
--   the spawn-time base that accessory buffs then fold onto. Same
--   dedup-by-source rule as everything else: later entries on the
--   same (stat, source) pair win.
defModifierMap ∷ UnitDef → HM.HashMap Text [StatModifier]
defModifierMap def = foldl' insertOne HM.empty (udModifiers def)
  where
    insertOne acc (stat, m) =
        let existing = HM.lookupDefault [] stat acc
            others   = filter (\x → smSource x ≢ smSource m) existing
        in HM.insert stat (m : others) acc

-- | Fold an accessory's buffs into a modifier map: def lookup + the
--   shared Unit.Stats.applyItemBuffs (which handles condition scaling,
--   the percent axis, and same-source collapse). Items without a def
--   in scope contribute nothing.
applyAccessoryBuffs ∷ ItemManager
                    → HM.HashMap Text [StatModifier]
                    → ItemInstance
                    → HM.HashMap Text [StatModifier]
applyAccessoryBuffs itemMgr mods inst =
    case lookupItemDef (iiDefName inst) itemMgr of
        Nothing   → mods
        Just iDef → applyItemBuffs (idDisplayName iDef)
                                   (iiCondition inst)
                                   (idBuffs iDef) mods

-- | Resolve a unit def's starting_accessories into ItemInstances.
--   Unknown items log a warning and are dropped. Like starting
--   equipment, this path contributes no override: every value is
--   "Item.Materialize"'s (#1418).
buildStartingAccessories ∷ EngineEnv → LoggerState → ItemManager
                         → [Text] → IO [ItemInstance]
buildStartingAccessories env logger itemMgr names = do
    mInsts ← mapM resolve names
    return [i | Just i ← mInsts]
  where
    resolve name = case lookupItemDef name itemMgr of
        Nothing → do
            logWarn logger CatThread $
                "starting_accessories: unknown item '" <> name
                <> "' — skipping"
            return Nothing
        Just _ → materializeItem itemMgr logger
                                 (ucStatRNGRef (toUnitCombatCapability env))
                                 (freshItemInstanceId env)
                                 pristineItem name
