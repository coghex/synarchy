{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Command
    ( processAllUnitCommands
    , recomputeBodyDerivedStats
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Data.List (foldl')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.State (EngineEnv(..))
import Unit.Anim (stateKey)
import Engine.Core.Log (logDebug, logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Sim.Types
import Unit.Stats (rollStat)
import Unit.Direction (Direction(..))
import Unit.Command.Types (UnitCommand(..))
import Equipment.Types (EquipmentClass(..), EquipmentSlot(..),
                        EquipmentClassManager, lookupEquipmentClass)
import Item.Roll (rollItemSpec)
import Item.Types (ItemDef(..), ItemContainer(..), ItemInstance(..)
                  , ItemBuff(..), ItemManager(..), lookupItemDef)
import Engine.Core.Log (LoggerState)
import World.Types (WorldManager(..), WorldState(..), WorldTileData(..),
                    LoadedChunk(..), ChunkCoord(..), columnIndex, lookupChunk)
import World.Generate (globalToChunk)

processAllUnitCommands ∷ EngineEnv → IORef UnitThreadState → IO ()
processAllUnitCommands env utsRef = do
    mCmd ← Q.tryReadQueue (unitQueue env)
    case mCmd of
        Just cmd → do
            handleUnitCommand env utsRef cmd
            processAllUnitCommands env utsRef
        Nothing → return ()

handleUnitCommand ∷ EngineEnv → IORef UnitThreadState → UnitCommand → IO ()
handleUnitCommand env utsRef (UnitSpawn uid defName gx gy gz) = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup defName (umDefs um) of
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
            -- Starting inventory: look each entry up in the ItemManager
            -- and build an ItemInstance. Unknown names are dropped
            -- with a warning (load-order issue: items loaded after
            -- units that reference them).
            itemMgr ← readIORef (itemManagerRef env)
            logger  ← readIORef (loggerRef env)
            initialInventory ← buildStartingInventory env logger itemMgr
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
            let inst = UnitInstance
                    { uiDefName    = defName
                    , uiTexture    = udTexture def
                    , uiDirSprites = udDirSprites def
                    , uiBaseWidth  = udBaseWidth def
                    , uiGridX      = gx
                    , uiGridY      = gy
                    , uiGridZ      = gz
                    , uiFacing     = DirS -- Default facing south
                    , uiCurrentAnim = ""  -- Phase 1: anim triggers wired in Phase 3
                    , uiAnimStart   = 0
                    , uiAnimReverse = False
                    , uiActivity    = "idle"
                    , uiPose        = "standing"
                    , uiAnimStride  = 1
                    , uiStats       = initialStats
                    -- Seed modifiers from the just-built accessories so
                    -- their buffs are active at spawn (same effect as
                    -- if the player had right-click-equipped each).
                    , uiModifiers   = foldl' (applyAccessoryBuffs itemMgr)
                                             HM.empty initialAccessories
                    , uiSkills      = initialSkills
                    , uiInventory   = initialInventory
                    , uiEquipment   = initialEquipment
                    , uiAccessories = initialAccessories
                    }
            atomicModifyIORef' (unitManagerRef env) $ \um' →
                (um' { umInstances = HM.insert uid inst (umInstances um') }, ())

            let ss = UnitSimState
                    { usRealX     = gx
                    , usRealY     = gy
                    , usGridZ     = gz
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
                    }
            atomicModifyIORef' utsRef $ \uts →
                (uts { utsSimStates = HM.insert uid ss (utsSimStates uts) }, ())

handleUnitCommand env utsRef (UnitDestroy uid) = do
    -- Single atomic modify removes the unit from instances AND clears
    -- it from the selection set, so no observer ever sees a "selected
    -- but dead" state.
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.delete uid (umInstances um)
            , umSelected  = HS.delete uid (umSelected um)
            }, ())
    atomicModifyIORef' utsRef $ \uts →
        (uts { utsSimStates = HM.delete uid (utsSimStates uts) }, ())

handleUnitCommand env utsRef (UnitTeleport uid gx gy mGz) = do
    gz ← case mGz of
        Just z  → return z
        Nothing → do
            let gxi = floor gx ∷ Int
                gyi = floor gy ∷ Int
            mSurf ← lookupSurfaceZ env gxi gyi
            case mSurf of
                Just z  → return z
                Nothing → return 0

    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usRealX     = gx
                             , usRealY     = gy
                             , usGridZ     = gz
                             , usTarget    = Nothing
                             , usState     = Idle
                             , usLocalPath = []
                             , usDrinkUntil      = Nothing
                             , usEatUntil        = Nothing
                             , usPickupUntil     = Nothing
                             , usTransitionUntil = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

    atomicModifyIORef' (unitManagerRef env) $ \um →
        let insts = umInstances um
        in case HM.lookup uid insts of
            Nothing → (um, ())
            Just inst →
                let inst' = inst { uiGridX = gx
                                 , uiGridY = gy
                                 , uiGridZ = gz
                                 }
                in (um { umInstances = HM.insert uid inst' insts }, ())

handleUnitCommand env utsRef (UnitMoveTo uid tx ty speed) = do
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                -- Only Standing units can move. Crouching / Crawling /
                -- Collapsed all refuse moves until they transition back
                -- up. (Will relax when per-pose walk assets exist.)
                -- In-progress transitions also ignore moves so a
                -- right-click can't yank a unit out of mid-transition.
                | usPose ss ≢ Standing → (uts, ())
                | isTransitioning (usState ss) → (uts, ())
                | otherwise →
                    let ss' = ss { usTarget    = Just (MoveTarget tx ty speed)
                                 , usState     = Walking
                                 , usLocalPath = []
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitStop uid) = do
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usTarget    = Nothing
                             , usState     = Idle
                             , usLocalPath = []
                             , usDrinkUntil      = Nothing
                             , usEatUntil        = Nothing
                             , usPickupUntil     = Nothing
                             , usTransitionUntil = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitCollapse uid) = do
    -- Snap to Collapsed pose. Fall animation is deferred — when the
    -- standing→collapsed composite is authored, this handler will
    -- instead queue a TransitioningTo Collapsed.
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usTarget    = Nothing
                             , usPose      = Collapsed
                             , usState     = Idle
                             , usLocalPath = []
                             , usDrinkUntil      = Nothing
                             , usEatUntil        = Nothing
                             , usPickupUntil     = Nothing
                             , usTransitionUntil = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitKill uid) = do
    -- Terminal: snap to Dead pose and clear all in-flight state.
    -- No animation chain — just an instant transition. Dead units
    -- are filtered out by AI / movement / drink / pickup via the
    -- non-Standing guards and the Lua-side dead-pose short-circuit.
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usPose             = Dead
                             , usState            = Idle
                             , usTarget           = Nothing
                             , usLocalPath        = []
                             , usDrinkUntil       = Nothing
                             , usEatUntil         = Nothing
                             , usPickupUntil      = Nothing
                             , usTransitionUntil  = Nothing
                             , usTransitionStride = 1
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitRevive uid) = do
    -- Snap to Standing pose. Per the orthogonal-pose plan, a real
    -- revive eventually chains Collapsed → Crawling → Crouching →
    -- Standing reverse transitions; for now (no transition assets yet)
    -- it just snaps. Only acts on Collapsed units.
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usPose ss ≢ Collapsed → (uts, ())
                | otherwise →
                    let ss' = ss { usPose            = Standing
                                 , usState           = Idle
                                 , usDrinkUntil      = Nothing
                                 , usEatUntil        = Nothing
                                 , usPickupUntil     = Nothing
                                 , usTransitionUntil = Nothing
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitDrink uid) = do
    -- Only acts on Idle units. Drinking blocks movement; an explicit
    -- precondition stops us from interrupting another animated state
    -- (Walking moves are easy to interrupt, Collapsed/Reviving are
    -- not, so we just require Idle for simplicity).
    um ← readIORef (unitManagerRef env)
    let duration = case HM.lookup uid (umInstances um) of
            Nothing   → 0
            Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
                Nothing  → 0
                Just def →
                    let key      = "drinking" ∷ Text
                        animName = HM.lookupDefault key key (udStateAnims def)
                    in case HM.lookup animName (udAnimations def) of
                        Nothing → 0
                        Just a  →
                            let counts = V.length <$> Map.elems (aFrames a)
                                maxN   = if null counts then 0 else maximum counts
                                fps    = aFps a
                            in if fps > 0 ∧ maxN > 0
                               then fromIntegral maxN / realToFrac fps ∷ Double
                               else 0
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usState ss ≢ Idle → (uts, ())
                | duration ≤ 0 → (uts, ())  -- no anim → no-op
                | otherwise →
                    let ss' = ss { usState      = Drinking
                                 , usTarget     = Nothing
                                 , usLocalPath  = []
                                 , usDrinkUntil = Just (now + duration)
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitEat uid) = do
    -- Same shape as UnitDrink. Plays the "eat" state animation and
    -- auto-reverts to Idle when the timer expires. Nutrition/inventory
    -- changes are applied Lua-side before this command is issued.
    um ← readIORef (unitManagerRef env)
    uts0 ← readIORef utsRef
    let mPose = usPose <$> HM.lookup uid (utsSimStates uts0)
        duration = case (HM.lookup uid (umInstances um), mPose) of
            (Just inst, Just curPose) → case HM.lookup (uiDefName inst) (umDefs um) of
                Nothing  → 0
                Just def →
                    let key      = stateKey curPose Eating
                        animName = HM.lookupDefault key key (udStateAnims def)
                    in case HM.lookup animName (udAnimations def) of
                        Nothing → 0
                        Just a  →
                            let counts = V.length <$> Map.elems (aFrames a)
                                maxN   = if null counts then 0 else maximum counts
                                fps    = aFps a
                            in if fps > 0 ∧ maxN > 0
                               then fromIntegral maxN / realToFrac fps ∷ Double
                               else 0
            _ → 0
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usState ss ≢ Idle → (uts, ())
                | duration ≤ 0 → (uts, ())
                | otherwise →
                    let ss' = ss { usState      = Eating
                                 , usTarget     = Nothing
                                 , usLocalPath  = []
                                 , usEatUntil   = Just (now + duration)
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitPickup uid) = do
    -- Same shape as UnitDrink. Plays the "pickup" state animation
    -- briefly, then auto-reverts to Idle.
    um ← readIORef (unitManagerRef env)
    let duration = case HM.lookup uid (umInstances um) of
            Nothing   → 0
            Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
                Nothing  → 0
                Just def →
                    let key      = "pickup" ∷ Text
                        animName = HM.lookupDefault key key (udStateAnims def)
                    in case HM.lookup animName (udAnimations def) of
                        Nothing → 0
                        Just a  →
                            let counts = V.length <$> Map.elems (aFrames a)
                                maxN   = if null counts then 0 else maximum counts
                                fps    = aFps a
                            in if fps > 0 ∧ maxN > 0
                               then fromIntegral maxN / realToFrac fps ∷ Double
                               else 0
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usState ss ≢ Idle → (uts, ())
                | duration ≤ 0 → (uts, ())
                | otherwise →
                    let ss' = ss { usState       = Picking
                                 , usTarget      = Nothing
                                 , usLocalPath   = []
                                 , usPickupUntil = Just (now + duration)
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitTransitionTo uid target stride) = do
    -- Initiate a pose transition. Stride ≥ 2 skips frames at render
    -- time and proportionally shortens the duration — used by the AI
    -- when chaining multiple transitions back-to-back.
    let s = max 1 stride
    um  ← readIORef (unitManagerRef env)
    uts0 ← readIORef utsRef
    let mCurrentPose = usPose <$> HM.lookup uid (utsSimStates uts0)
        duration = case (HM.lookup uid (umInstances um), mCurrentPose) of
            (Just inst, Just curPose) →
                case HM.lookup (uiDefName inst) (umDefs um) of
                    Nothing  → 0
                    Just def →
                        let key      = stateKey curPose (TransitioningTo target)
                            animName = HM.lookupDefault key key (udStateAnims def)
                        in case HM.lookup animName (udAnimations def) of
                            Nothing → 0
                            Just a  →
                                let counts = V.length <$> Map.elems (aFrames a)
                                    maxN   = if null counts then 0 else maximum counts
                                    fps    = aFps a
                                    -- frames shown when stride S = ((n-1) `div` S) + 1
                                    visible = (maxN - 1) `div` s + 1
                                in if fps > 0 ∧ maxN > 0
                                   then fromIntegral visible / realToFrac fps ∷ Double
                                   else 0
            _ → 0
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usPose ss ≡ target → (uts, ())  -- already there
                | isTransitioning (usState ss) → (uts, ())  -- already mid-transition
                | otherwise →
                    let ss' = ss { usState             = TransitioningTo target
                                 , usTarget            = Nothing
                                 , usLocalPath         = []
                                 , usTransitionUntil   = Just (now + duration)
                                 , usTransitionStride  = s
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

isTransitioning ∷ UnitActivity → Bool
isTransitioning (TransitioningTo _) = True
isTransitioning _                   = False

-- | Spawn-time body composition. Reads rolled height/bulk/bodyfat,
--   computes body_mass / lean_mass (skeletal muscle only) / fat_mass,
--   clamps lean/fat at the viability floors so a tall thin combo can't
--   spawn at or below the death thresholds, then drops bulk/bodyfat
--   from the map (they're spawn-time inputs, not live stats).
--
--   Finally re-runs `recomputeBodyDerivedStats` to fill in strength /
--   max_hydration / max_hunger / carrying_capacity / strength_base.
--   No-op if any of height/bulk/bodyfat is missing (e.g. for unit
--   types that don't declare a body block).
seedBodyComposition ∷ HM.HashMap Text Float → HM.HashMap Text Float
seedBodyComposition rolled =
    case (HM.lookup "height" rolled, HM.lookup "bulk" rolled,
          HM.lookup "bodyfat" rolled) of
        (Just h, Just b, Just f) →
            let bodyMass  = 22 * h * h * b
                fatMass0  = bodyMass * f
                -- Skeletal muscle is ~50 % of non-fat tissue in real
                -- adult composition; the rest is bones / organs /
                -- water / skin (lumped together as implicit organ
                -- mass, never tracked separately).
                leanMass0 = bodyMass * (1 - f) * 0.5
                -- Viability clamps: a tall-thin spawn (h=2.2,
                -- bulk=0.5, bodyfat=0.05) sits right at min_lean(h);
                -- +1 kg keeps it above the death floor with margin.
                minFat    = 0.44 * h * h
                minLean   = 4.4 * h * h
                fatMass   = max (minFat + 1) fatMass0
                leanMass  = max (minLean + 1) leanMass0
                withBody  = HM.insert "body_mass" bodyMass
                          $ HM.insert "lean_mass" leanMass
                          $ HM.insert "fat_mass"  fatMass
                          $ HM.delete "bulk"
                          $ HM.delete "bodyfat" rolled
            in recomputeBodyDerivedStats withBody
        _ → rolled

-- | Recompute body-driven derived stats from live body composition.
--   Reads height + body_mass + lean_mass + strength_base, writes
--   strength / max_hydration / max_hunger / carrying_capacity.
--
--   strength_base is the un-scaled potential rolled at spawn; on the
--   first call (no strength_base yet) we promote the rolled "strength"
--   into "strength_base" so future re-computes don't compound. This
--   is what makes "a wasted unit physically weakens" emergent —
--   Phase 4 catabolism shrinks lean_mass, this recompute drops the
--   strength derived from it, and carrying_capacity follows.
--
--   avg_skeletal_muscle_at_height(h) = 22 · h² · 0.8 · 0.5 = 8.8 · h²
--   assumes acolyte-like means (bulk=1, bodyfat=0.2). Different
--   species would want a species-specific average; punt until then.
recomputeBodyDerivedStats ∷ HM.HashMap Text Float → HM.HashMap Text Float
recomputeBodyDerivedStats s =
    case (HM.lookup "height" s, HM.lookup "body_mass" s,
          HM.lookup "lean_mass" s) of
        (Just h, Just bm, Just lm) →
            let avgLean    = 8.8 * h * h
                ratio      = if avgLean > 0 then lm / avgLean else 1
                -- First call: no strength_base yet — promote the
                -- rolled "strength" into the base slot, then derive.
                strBase    = case HM.lookup "strength_base" s of
                                 Just b  → b
                                 Nothing → HM.lookupDefault 0 "strength" s
                strength   = strBase * (ratio ** 0.7)
                maxHydration = bm * 0.6
                maxHunger    = bm * 20
                carryCap     = strength * 5
            in HM.insert "strength_base"     strBase
             $ HM.insert "strength"          strength
             $ HM.insert "max_hydration"     maxHydration
             $ HM.insert "max_hunger"        maxHunger
             $ HM.insert "carrying_capacity" carryCap s
        _ → s

-- | Resolve a unit def's starting_inventory into concrete ItemInstance
--   list. Unknown item names log a warning and are dropped. Fill is
--   clamped to the container's capacity; non-container items ignore
--   the fill arg and get 0. Quality + condition are rolled from the
--   def's spec (defaults to 100 when unset).
buildStartingInventory ∷ EngineEnv → LoggerState → ItemManager
                       → [(Text, Maybe Float)] → IO [ItemInstance]
buildStartingInventory env logger itemMgr entries = do
    mInsts ← mapM resolve entries
    return [i | Just i ← mInsts]
  where
    resolve (name, mFill) =
        case lookupItemDef name itemMgr of
            Nothing → do
                logWarn logger CatThread $
                    "Unit starting_inventory: unknown item '" <> name
                      <> "' — skipping"
                return Nothing
            Just def → do
                let fill = case (mFill, idContainer def) of
                        (Just f,  Just c) → max 0 (min f (icCapacity c))
                        (Nothing, _     ) → 0
                        (Just _,  Nothing) → 0
                qual ← rollItemSpec (idQualitySpec def)   (statRNGRef env)
                cond ← rollItemSpec (idConditionSpec def) (statRNGRef env)
                return $ Just ItemInstance
                    { iiDefName     = name
                    , iiCurrentFill = fill
                    , iiQuality     = qual
                    , iiCondition   = cond
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
                                    return $ HM.insert slotId
                                        ItemInstance
                                          { iiDefName     = itemName
                                          , iiCurrentFill = 0
                                          , iiQuality     = qual
                                          , iiCondition   = cond
                                          }
                                        m
                ) (return HM.empty) entries

-- | Fold an accessory's buffs into a modifier map. Mirrors
--   Engine.Scripting.Lua.API.Equipment.applyItemBuffs but kept here to
--   avoid the import cycle between Unit.Thread.Command and Equipment.
--   Source string = item display_name; same-source entries collapse.
applyAccessoryBuffs ∷ ItemManager
                    → HM.HashMap Text [StatModifier]
                    → ItemInstance
                    → HM.HashMap Text [StatModifier]
applyAccessoryBuffs itemMgr mods inst =
    case lookupItemDef (iiDefName inst) itemMgr of
        Nothing → mods
        Just iDef → foldl' (applyOne iDef) mods (idBuffs iDef)
  where
    applyOne iDef acc b =
        let cond  = iiCondition inst
            delta = if ibScalesWithCondition b
                      then ibAmount b * (cond / 100)
                      else ibAmount b
            src   = idDisplayName iDef
            m     = StatModifier { smDelta = delta
                                 , smSource = src
                                 , smExpiry = Nothing }
            existing = HM.lookupDefault [] (ibStat b) acc
            others   = filter (\x → smSource x ≢ src) existing
        in HM.insert (ibStat b) (m : others) acc

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
            return $ Just ItemInstance
                { iiDefName     = name
                , iiCurrentFill = 0
                , iiQuality     = qual
                , iiCondition   = cond
                }

lookupSurfaceZ ∷ EngineEnv → Int → Int → IO (Maybe Int)
lookupSurfaceZ env gx gy = do
    wm ← readIORef (worldManagerRef env)
    go (wmVisible wm) (wmWorlds wm)
  where
    (chunkCoord, (lx, ly)) = globalToChunk gx gy
    go [] _ = return Nothing
    go (pageId:rest) worlds =
        case lookup pageId worlds of
            Nothing → go rest worlds
            Just ws → do
                td ← readIORef (wsTilesRef ws)
                case lookupChunk chunkCoord td of
                    Just lc → return $ Just ((lcSurfaceMap lc) VU.! columnIndex lx ly)
                    Nothing → go rest worlds
