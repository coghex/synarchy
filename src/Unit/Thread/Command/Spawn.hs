{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Command.Spawn
    ( handleUnitSpawnCommand
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Engine.Core.Log (logDebug, logInfo, logWarn, LogCategory(..), LoggerState)
import Unit.Types
import Unit.Sim.Types
import Unit.Stats (rollStat, pickName, applyItemBuffs)
import Unit.Thread.Command.Body (seedBodyComposition, bloodSeedFromStats)
import Equipment.Types (EquipmentClass(..), EquipmentSlot(..),
                        lookupEquipmentClass)
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types (ItemDef(..), ItemContainer(..), ItemInstance(..)
                  , ItemManager(..), lookupItemDef
                  , itemTotalWeight)
import World.Types (WorldManager(..))
import World.Page.Types (WorldPageId(..))

handleUnitSpawnCommand ∷ EngineEnv → IORef UnitThreadState → UnitId → Text
                       → Float → Float → Int → Text → WorldPageId → IO ()
handleUnitSpawnCommand env utsRef uid defName gx gy gz factionId pageId = do
    um ← readIORef (unitManagerRef env)
    -- Drop the spawn if its world no longer exists. A spawn queued before
    -- world.destroyAll (Exit to Menu) would otherwise be drained after
    -- teardown and re-insert an orphan unit into the cleared manager (#58).
    wmgr ← readIORef (worldManagerRef env)
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
                then atomicModifyIORef' (statRNGRef env) $ \g0 →
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
            initialSkills ← atomicModifyIORef' (statRNGRef env) $ \g0 →
                let (rolled, g') = HM.foldlWithKey'
                        (\(acc, g) name (b, r) →
                            let (v, g'') = rollStat b r g
                            in (HM.insert name v acc, g''))
                        (HM.empty, g0)
                        (udSkillTemplates def)
                in (g', rolled)
            -- Knowledge the unit spawns KNOWING, rolled like skills.
            initialKnowledge ← atomicModifyIORef' (statRNGRef env) $ \g0 →
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
                Just pool → atomicModifyIORef' (statRNGRef env) $ \g0 →
                    let (nm, g') = pickName pool g0 in (g', nm)
            -- Starting inventory: look each entry up in the ItemManager
            -- and build an ItemInstance. Unknown names are dropped
            -- with a warning (load-order issue: items loaded after
            -- units that reference them).
            itemMgr ← readIORef (itemManagerRef env)
            logger  ← readIORef (loggerRef env)
            taggedInventory ← buildStartingInventory env logger itemMgr
                                  (udStartingInventory def)
            -- Pre-equipped items declared by the unit def's
            -- starting_equipment. Resolved against the EquipmentClass
            -- so each item's kind can be validated against the slot.
            ecMgr ← readIORef (equipmentClassManagerRef env)
            let mClass = udEquipmentClass def >>= (`lookupEquipmentClass` ecMgr)
            initialEquipment ← buildStartingEquipment env logger itemMgr mClass
                                  (udStartingEquipment def)
            initialAccessories ← buildStartingAccessories env logger itemMgr
                                  (udStartingAccessories def)
            -- Spawn-time capacity check. Armor / weapons / survival
            -- kit always arrive; inventory entries with a drop
            -- priority shed (highest first — pick before shovel)
            -- until the loadout fits the rolled carrying_capacity.
            -- Weights mirror getCarryingWeight: instance weight + fill
            -- (at the container's per-unit fill weight) + container
            -- contents, worn gear at full mass.
            let itemW = itemTotalWeight itemMgr
                fixedW = sum (map itemW (HM.elems initialEquipment))
                       + sum (map itemW initialAccessories)
            initialInventory ← case HM.lookup "carrying_capacity"
                                              initialStats of
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
                    -- Seed modifiers from the def's innate modifiers
                    -- (technomule's "cybernetic enhancements") plus
                    -- the just-built accessories so their buffs are
                    -- active at spawn (same effect as if the player
                    -- had right-click-equipped each).
                    , uiModifiers   = foldl' (applyAccessoryBuffs itemMgr)
                                             (defModifierMap def)
                                             initialAccessories
                    , uiSkills      = initialSkills
                    , uiKnowledge   = initialKnowledge
                    , uiInventory   = initialInventory
                    , uiEquipment   = initialEquipment
                    , uiAccessories = initialAccessories
                    , uiFactionId   = factionId
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
            atomicModifyIORef' (unitManagerRef env) $ \um' →
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

-- | Spawn-time capacity shed: drop tagged inventory items (priority
--   descending, so the acolyte's pick goes before its shovel) until
--   the total loadout — fixedW (worn equipment + accessories) plus
--   the remaining inventory — fits the rolled carrying_capacity.
--   Untagged items (priority 0) are never shed; if the loadout still
--   doesn't fit after every sheddable item is gone, the unit spawns
--   over capacity (the pickup/store gates simply refuse until it
--   lightens) and we log it.
shedToCapacity ∷ LoggerState → UnitId → (ItemInstance → Float)
               → Float → Float → [(ItemInstance, Int)]
               → IO [ItemInstance]
shedToCapacity logger uid itemW cap fixedW = go
  where
    totalOf xs = fixedW + sum (map (itemW . fst) xs)
    go xs
        | totalOf xs ≤ cap = return (map fst xs)
        | otherwise =
            let prios = [ p | (_, p) ← xs, p > 0 ]
            in case prios of
                [] → do
                    logWarn logger CatThread $
                        "UnitSpawn " <> T.pack (show uid)
                        <> ": loadout "
                        <> T.pack (show (totalOf xs))
                        <> " kg exceeds capacity "
                        <> T.pack (show cap)
                        <> " kg with nothing left to shed"
                    return (map fst xs)
                _  → do
                    let top = maximum prios
                        (name, rest) = removeFirstByPrio top xs
                    logInfo logger CatThread $
                        "UnitSpawn " <> T.pack (show uid)
                        <> ": over capacity ("
                        <> T.pack (show (totalOf xs)) <> " > "
                        <> T.pack (show cap)
                        <> " kg) — leaving " <> name <> " behind"
                    go rest

    removeFirstByPrio _ [] = ("?", [])
    removeFirstByPrio p ((it, q) : rest)
        | q ≡ p     = (iiDefName it, rest)
        | otherwise = let (n, rest') = removeFirstByPrio p rest
                      in (n, (it, q) : rest')

-- | Resolve a unit def's starting_inventory into concrete ItemInstance
--   list, each tagged with its capacity-shed drop priority. Unknown
--   item names log a warning and are dropped. Fill is clamped to the
--   container's capacity; non-container items ignore the fill arg and
--   get 0. Quality + condition are rolled from the def's spec
--   (defaults to 100 when unset).
buildStartingInventory ∷ EngineEnv → LoggerState → ItemManager
                       → [(Text, Maybe Float, Int)]
                       → IO [(ItemInstance, Int)]
buildStartingInventory env logger itemMgr entries = do
    mInsts ← mapM resolve entries
    return [i | Just i ← mInsts]
  where
    resolve (name, mFill, prio) = do
        mi ← rollInstance env itemMgr name mFill
        case mi of
            Nothing → do
                logWarn logger CatThread $
                    "Unit starting_inventory: unknown item '" <> name
                      <> "' — skipping"
                return Nothing
            Just inst → return (Just (inst, prio))

-- | Build one rolled ItemInstance from a def name (quality / condition /
--   weight rolled from the def's specs, fill clamped to capacity), and
--   RECURSIVELY materialise its default contents — so a first-aid kit /
--   toolbox spawns already holding its bandages, tools, etc. Returns
--   Nothing for an unknown item name.
rollInstance ∷ EngineEnv → ItemManager → Text → Maybe Float
             → IO (Maybe ItemInstance)
rollInstance env itemMgr name mFill =
    case lookupItemDef name itemMgr of
        Nothing → return Nothing
        Just def → do
            -- Explicit fill wins; otherwise the def's default_fill (a
            -- quinoa sack spawns full, a canteen defaults empty). Both
            -- clamp to capacity; non-containers always 0.
            let fill = case idContainer def of
                    Just c  → max 0 (min (icCapacity c)
                                (fromMaybe (icDefaultFill c) mFill))
                    Nothing → 0
            qual ← rollItemSpec (idQualitySpec def)   (statRNGRef env)
            cond ← rollItemSpec (idConditionSpec def) (statRNGRef env)
            wght ← rollItemWeight def (statRNGRef env)
            -- Expand each (name, count, fill) content entry into `count`
            -- rolled instances, then drop unknown names.
            let reqs = [ (cName, cFill)
                       | (cName, cCount, cFill) ← idDefaultContents def
                       , _ ← [1 .. max 0 cCount] ]
            contentMaybes ← mapM (\(cn, cf) → rollInstance env itemMgr cn cf) reqs
            let contents = [ c | Just c ← contentMaybes ]
            iid ← freshItemInstanceId env
            return $ Just ItemInstance
                { iiDefName     = name
                , iiCurrentFill = fill
                , iiQuality     = qual
                , iiCondition   = cond
                , iiWeight      = wght
                , iiSharpness   = 100.0
                , iiContents    = contents
                , iiInstanceId  = iid
                , iiTemp        = Nothing
                }

-- | Resolve a unit def's starting_equipment into a slot→ItemInstance
--   map, validating each item's `idKind` against the slot's accepted
--   `esKind`. Unknown items / kind mismatches / unknown slots log a
--   warning and are dropped. Containers in equipment slots get fill=0
--   (Phase 2 doesn't bother seeding canteens-as-equipment). Quality +
--   condition rolled from the def's spec like starting_inventory.
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
                                    qual ← rollItemSpec
                                             (idQualitySpec iDef)
                                             (statRNGRef env)
                                    cond ← rollItemSpec
                                             (idConditionSpec iDef)
                                             (statRNGRef env)
                                    wght ← rollItemWeight iDef
                                             (statRNGRef env)
                                    iid ← freshItemInstanceId env
                                    return $ HM.insert slotId
                                        ItemInstance
                                          { iiDefName     = itemName
                                          , iiCurrentFill = 0
                                          , iiQuality     = qual
                                          , iiCondition   = cond
                                          , iiWeight      = wght
                                          , iiSharpness   = 100.0
                                          , iiContents    = []
                                          , iiInstanceId  = iid
                                          , iiTemp        = Nothing
                                          }
                                        m
                ) (return HM.empty) entries

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
--   Unknown items log a warning and are dropped. Quality + condition
--   roll from the def's spec; defaults to 100 when absent (matches
--   "no roll" — robes / habits etc. typically don't have a quality
--   distribution at all).
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
        Just def → do
            qual ← rollItemSpec (idQualitySpec def)   (statRNGRef env)
            cond ← rollItemSpec (idConditionSpec def) (statRNGRef env)
            wght ← rollItemWeight def (statRNGRef env)
            iid ← freshItemInstanceId env
            return $ Just ItemInstance
                { iiDefName     = name
                , iiCurrentFill = 0
                , iiQuality     = qual
                , iiCondition   = cond
                , iiWeight      = wght
                , iiSharpness   = 100.0
                , iiContents    = []
                , iiInstanceId  = iid
                , iiTemp        = Nothing
                }
