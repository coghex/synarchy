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
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Unit.Anim (stateKey)
import Engine.Core.Log (logDebug, logInfo, logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Sim.Types
import Unit.Stats (rollStat, pickName, applyItemBuffs)
import Unit.Command.Types (UnitCommand(..))
import Unit.Thread.Movement (startJump, jumpMaxTiles)
import Equipment.Types (EquipmentClass(..), EquipmentSlot(..),
                        lookupEquipmentClass)
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types (ItemDef(..), ItemContainer(..), ItemInstance(..)
                  , ItemManager(..), lookupItemDef
                  , itemTotalWeight)
import Engine.Core.Log (LoggerState)
import World.Types (WorldManager(..), WorldState(..), LoadedChunk(..), columnIndex, lookupChunk)
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
handleUnitCommand env utsRef (UnitSpawn uid defName gx gy gz factionId pageId) = do
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

handleUnitCommand env utsRef UnitClearAll = do
    -- Wipe all units + selection + sim state. Processed in queue order, so
    -- it runs AFTER any UnitSpawns queued before Exit to Menu — those
    -- insert first, then this clears, leaving no orphans (#58).
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.empty, umSelected = HS.empty }, ())
    atomicModifyIORef' utsRef $ \uts →
        (uts { utsSimStates = HM.empty }, ())

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
                             , usRealZ     = fromIntegral gz
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
                                 , uiRealZ = fromIntegral gz
                                 }
                in (um { umInstances = HM.insert uid inst' insts }, ())

handleUnitCommand env utsRef (UnitReGround gx gy) = do
    -- Terrain at (gx, gy) changed under our feet (dig / delete-tile).
    -- Re-snap idle units standing on that tile to the new surface;
    -- moving units re-ground themselves on every tile crossing, and
    -- transitioning/falling/climbing units are mid-state-machine and
    -- must not be teleport-snapped.
    mSurf ← lookupSurfaceZ env gx gy
    case mSurf of
        Nothing → pure ()
        Just z → do
            snapped ← atomicModifyIORef' utsRef $ \uts →
                let simStates = utsSimStates uts
                    affects ss =
                        floor (usRealX ss) ≡ gx
                      ∧ floor (usRealY ss) ≡ gy
                      ∧ usState ss ≡ Idle
                      ∧ usGridZ ss ≢ z
                    snap ss
                        | affects ss = ss { usGridZ = z
                                          , usRealZ = fromIntegral z }
                        | otherwise  = ss
                    hit = [ uid | (uid, ss) ← HM.toList simStates
                                , affects ss ]
                in (uts { utsSimStates = HM.map snap simStates }, hit)
            -- Mirror into the render-facing instances so the visual z
            -- updates this frame, same as UnitTeleport does.
            forM_ snapped $ \uid →
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, ())
                        Just inst →
                            let inst' = inst { uiGridZ = z
                                             , uiRealZ = fromIntegral z }
                            in (um { umInstances =
                                    HM.insert uid inst' (umInstances um) }, ())

handleUnitCommand env utsRef (UnitMoveTo uid tx ty speed) = do
    -- Apply the injury speed multiplier on receipt so EVERY move
    -- command — commanded, wander, attack-pursuit, retreat — gets
    -- scaled the same way without the AI caller having to know.
    --
    -- Note: the umRef read below is NOT atomic with the utsRef
    -- modify. If the wound subsystem (10 Hz) lands a new wound
    -- between the two, this move commits with the pre-wound
    -- multiplier and the unit travels its current path segment at
    -- the stale speed. The next move command picks up the fresh
    -- state. Effect is bounded (one segment of slightly-too-fast
    -- movement, ≲1% per-command hit rate) and not worth merging the
    -- two refs to close — kept here so the next reader doesn't
    -- mistake the separation for an oversight.
    um ← readIORef (unitManagerRef env)
    let (effSpeed, isRunning) = case HM.lookup uid (umInstances um) of
            Nothing   → (speed, False)
            Just inst →
                let sp     = speed * injurySpeedMult inst
                    (maxSp, runFrac) = case HM.lookup (uiDefName inst) (umDefs um) of
                        Just d  → (udMaxSpeed d, udRunThreshold d)
                        Nothing → (3.0, 0.6)
                    runCut = maxSp * runFrac   -- per-unit run-anim threshold
                in (sp, sp > runCut)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                -- Standing AND Crawling units can move (a crawling unit
                -- crawls slowly toward the goal — the mover caps its
                -- speed). Crouching / Collapsed refuse moves until they
                -- transition back up. In-progress transitions also ignore
                -- moves so a right-click can't yank a unit out of a
                -- mid-transition.
                | usPose ss ≢ Standing ∧ usPose ss ≢ Crawling → (uts, ())
                | isTransitioning (usState ss) → (uts, ())
                | otherwise →
                    -- A crawling unit is always Walking-gait (there's no
                    -- crawling-run anim, and the mover caps its speed); only
                    -- a standing unit can break into a Running activity.
                    let activity = if isRunning ∧ usPose ss ≡ Standing
                                   then Running else Walking
                        ss' = ss { usTarget    = Just (MoveTarget tx ty effSpeed)
                                 , usState     = activity
                                 , usLocalPath = []
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand env utsRef (UnitJump uid tgx tgy) = do
    now ← readIORef (gameTimeRef env)
    um  ← readIORef (unitManagerRef env)
    -- Reach = learned jumping skill blended with agility/strength stats
    -- (the skill/stat split). Unknown unit → 0 reach (can't leap).
    let maxTiles = case HM.lookup uid (umInstances um) of
            Nothing   → 0
            Just inst →
                let bm = HM.lookupDefault 1.0 "body_mass" (uiStats inst)
                    fm = HM.lookupDefault 0.0 "fat_mass"  (uiStats inst)
                    fatFrac = if bm > 0 then fm / bm else 0
                in jumpMaxTiles (HM.lookupDefault 0.0 "jumping"  (uiSkills inst))
                                (HM.lookupDefault 1.0 "agility"  (uiStats  inst))
                                (HM.lookupDefault 1.0 "strength" (uiStats  inst))
                                fatFrac
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                -- Only a standing, non-transitioning unit can leap.
                | usPose ss ≢ Standing         → (uts, ())
                | isTransitioning (usState ss)  → (uts, ())
                | otherwise →
                    let dstX = fromIntegral tgx + 0.5
                        dstY = fromIntegral tgy + 0.5
                        dx   = dstX - usRealX ss
                        dy   = dstY - usRealY ss
                        d    = sqrt (dx * dx + dy * dy)
                    -- Refuse a leap beyond reach (or a no-op onto self); the
                    -- unit just stays put — slice 1 has no "fall short" yet.
                    in if d < 0.001 ∨ d > maxTiles
                       then (uts, ())
                       else let ss' = startJump now ss tgx tgy
                            in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand _env utsRef (UnitStop uid) = do
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
                             , usGetUpAt         = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand _env utsRef (UnitCollapse uid) = do
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
                             -- A survival/explicit collapse is NOT a fall
                             -- knockdown: clear any getup timer so this
                             -- collapse stays resource-gated (recovers via
                             -- checkRevive), not auto-stood by the movement
                             -- tick.
                             , usGetUpAt         = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand _env utsRef (UnitCrawl uid) = do
    -- Drop to a sustained Crawling pose. Unlike Collapsed this KEEPS the
    -- in-flight move target + walking state, so a unit maimed mid-stride
    -- keeps crawling toward its goal (the mover caps its speed to a
    -- crawl). Only clears mid-transition/getup timers so it can't be both
    -- crawling and mid-climb. No-op if already crawling/collapsed/dead.
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usPose ss ≡ Crawling ∨ usPose ss ≡ Dead → (uts, ())
                | otherwise →
                    -- Preserve a Walking/Idle move state (so it crawls on
                    -- toward the goal); drop a stranded transition to Idle.
                    let st  = if isTransitioning (usState ss)
                              then Idle else usState ss
                        ss' = ss { usPose            = Crawling
                                 , usState           = st
                                 , usTransitionUntil = Nothing
                                 , usGetUpAt         = Nothing
                                 , usDrinkUntil      = Nothing
                                 , usEatUntil        = Nothing
                                 , usPickupUntil     = Nothing
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand _env utsRef (UnitKill uid) = do
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
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCommand _env utsRef (UnitRevive uid) = do
    -- Snap to Standing pose. Per the orthogonal-pose plan, a real
    -- revive eventually chains Collapsed → Crawling → Crouching →
    -- Standing reverse transitions; for now (no transition assets yet)
    -- it just snaps. Acts on Collapsed (waking) AND Crawling (legs healed,
    -- standing back up) units.
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usPose ss ≢ Collapsed ∧ usPose ss ≢ Crawling → (uts, ())
                | otherwise →
                    let ss' = ss { usPose            = Standing
                                 , usState           = Idle
                                 , usDrinkUntil      = Nothing
                                 , usEatUntil        = Nothing
                                 , usPickupUntil     = Nothing
                                 , usTransitionUntil = Nothing
                                 , usGetUpAt         = Nothing
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
                                    -- The renderer (Unit.Render.pickFrame)
                                    -- plays strided frames 0, s, 2s, … and
                                    -- clamps the last step to the destination
                                    -- frame (maxN-1). It first reaches that
                                    -- frame at raw step ceil((maxN-1)/s), and
                                    -- the transition must last ONE interval
                                    -- longer so that final frame is actually
                                    -- shown — expiry runs before the render
                                    -- publish, so a duration that ends exactly
                                    -- on that step publishes the target pose
                                    -- over it (truncating non-divisor strides,
                                    -- e.g. maxN=9, s=3 → 0,3,6,8). A stride
                                    -- larger than the whole animation has no
                                    -- in-between frames to show and collapses
                                    -- to an instant (zero-duration) transition.
                                    visible
                                      | s > maxN  = 0
                                      | otherwise = ((maxN - 1 + s - 1) `div` s) + 1
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
                | duration ≤ 0 →
                    -- No frames to play (stride skipped past the whole
                    -- animation, or no transition anim exists): resolve
                    -- immediately to the target pose rather than forcing a
                    -- one-frame TransitioningTo state. Clear the move target
                    -- and local path just like the normal branch — otherwise
                    -- the unit keeps moving in the same tick after the
                    -- "instant" pose switch (commands run before
                    -- tickAllMovement), which can leave e.g. a crouching unit
                    -- walking.
                    let ss' = ss { usPose      = target
                                 , usState     = Idle
                                 , usTarget    = Nothing
                                 , usLocalPath = []
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())
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
-- | Spawn-time blood-volume seed in litres ('bloodMassRatio' of
--   body_mass) — applied via the rolled stats map so units without a
--   body block (no body_mass) start at 0 and won't bleed.
--   The wound-tick code computes max_blood lazily from current
--   body_mass on each read, so wasting/regrowth carries through.
bloodSeedFromStats ∷ HM.HashMap Text Float → Float
bloodSeedFromStats s = case HM.lookup "body_mass" s of
    Just bm → bm * bloodMassRatio
    Nothing → 0.0

-- | Movement-speed multiplier derived from the unit's wounds + blood.
--
--   Three terms compose multiplicatively:
--     * leg/foot wound severity × 0.6 — wounds on legs/feet hit
--       harder because that's literally what walks.
--     * torso wound severity × 0.3 — concussive / vital-area damage
--       slows the unit too but less than a leg.
--     * blood fraction → 0.5 + 0.5 × frac — at full blood we're 1.0,
--       at 0 blood we're 0.5 (matches "unconscious bleeding out is
--       still capable of some movement before collapse").
--   Result clamped to [0.1, 1.0] so a unit never literally stops.
--   Applied to UnitMoveTo's speed parameter so all movement gets
--   scaled the same way.
injurySpeedMult ∷ UnitInstance → Float
injurySpeedMult inst =
    let isLegPart p = p == "l_leg" || p == "r_leg"
                   || p == "l_foot" || p == "r_foot"
                   || p == "l_fore_leg" || p == "r_fore_leg"
                   || p == "l_hind_leg" || p == "r_hind_leg"
        -- EFFECTIVE severity (heal eases it, necrosis floors it) so a
        -- limping unit regains speed as its leg wound mends, in step
        -- with the bleed/pain/anim consumers that share the same helper.
        legSev = sum [ woundEffSeverity w
                     | w ← uiWounds inst, isLegPart (woundPart w) ]
        torsoSev = sum [ woundEffSeverity w
                       | w ← uiWounds inst, woundPart w == "torso" ]
        bodyMass = HM.lookupDefault 70.0 "body_mass" (uiStats inst)
        maxBlood = bodyMass * bloodMassRatio
        bloodFrac = if maxBlood > 0
                    then max 0 (min 1 (uiBlood inst / maxBlood))
                    else 1
        -- legCut multiplier 1.2 (was 0.6): a severity-0.5 leg wound
        -- cuts speed 60%, sev-0.8 cuts ~96%, sev-1.0 hits the floor.
        -- Limps are decisively visible — wounded acolytes shuffle.
        -- torsoCut stays gentler — a torso wound hurts but doesn't
        -- mechanically prevent leg movement.
        legCut   = min 1.0 (legSev   * 1.2)
        torsoCut = min 1.0 (torsoSev * 0.3)
        bloodMul = 0.5 + 0.5 * bloodFrac
        raw      = (1 - legCut) * (1 - torsoCut) * bloodMul
    in max 0.1 (min 1.0 raw)

-- Body-composition viability floors, as FRACTIONS of frame mass
-- (frame = 22·h²·bulk), so they scale to ANY creature size — mouse to
-- dragon. Calibrated to reproduce the historical height-only human floors
-- at bulk 1.0: 0.02·(22h²) = 0.44h² and 0.20·(22h²) = 4.4h². The spawn
-- margins seat a fresh roll comfortably above the death floors and are
-- themselves frame-proportional (≈ the old flat +1 kg at human scale, but
-- scaling correctly for tiny/huge bodies instead of swamping them).
minFatFrac, minLeanFrac, spawnFatMargin, spawnLeanMargin ∷ Float
minFatFrac      = 0.02
minLeanFrac     = 0.20
spawnFatMargin  = 1.7
spawnLeanMargin = 1.07

seedBodyComposition ∷ HM.HashMap Text Float → HM.HashMap Text Float
seedBodyComposition rolled =
    case (HM.lookup "height" rolled, HM.lookup "bulk" rolled,
          HM.lookup "bodyfat" rolled) of
        (Just h, Just b, Just f) →
            let bodyMass  = 22 * h * h * b   -- the frame: structural size
                fatMass0  = bodyMass * f
                -- Skeletal muscle is ~50 % of non-fat tissue in real
                -- adult composition; the rest is bones / organs /
                -- water / skin (lumped together as implicit organ
                -- mass, never tracked separately).
                leanMass0 = bodyMass * (1 - f) * 0.5
                -- Frame-proportional viability floors (see the *Frac /
                -- *Margin constants above). These scale with the unit's
                -- own frame mass, so a tiny animal's healthy lean/fat are
                -- never clamped above its own body mass — body composition
                -- stays coherent at ANY size (mouse → dragon), which keeps
                -- every mass-derived quantity (blood = body·bloodMassRatio,
                -- max_salt = body·k, BMR = 22·(body−fat)+…) sane. At human
                -- bulk these reproduce the old 0.44h² / 4.4h² floors.
                minFat    = minFatFrac  * bodyMass
                minLean   = minLeanFrac * bodyMass
                fatMass   = max (minFat  * spawnFatMargin)  fatMass0
                leanMass  = max (minLean * spawnLeanMargin) leanMass0
                -- frame_mass = the stable structural reference. body_mass
                -- can later shrink via catabolism/wasting; frame_mass does
                -- not, so the organ-failure / starvation death thresholds
                -- (unit_resources) read it for a size-correct floor.
                withBody  = HM.insert "frame_mass" bodyMass
                          $ HM.insert "body_mass"  bodyMass
                          $ HM.insert "lean_mass"  leanMass
                          $ HM.insert "fat_mass"   fatMass
                          $ HM.delete "bulk"
                          $ HM.delete "bodyfat" rolled
            in recomputeBodyDerivedStats withBody
        _ → rolled

-- | Recompute body-driven derived stats from live body composition.
--   Reads height + body_mass + lean_mass + strength_base, writes
--   strength / max_hydration / max_hunger / max_calories /
--   carrying_capacity.
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
                -- Two-layer food model (#93): max_hunger is STOMACH
                -- capacity (kcal of undigested food — ~713 kcal for a
                -- default acolyte, a few meals); max_calories is the
                -- energy STORE digestion feeds (kept at the old bm*20
                -- pool size, ~1426 kcal ≈ a day of idle burn, so the
                -- starvation timelines carry over). Both are seeded for
                -- any body unit — only units with a LIVE hunger/calories
                -- stat (unit_resources config) actually run the system.
                maxHunger    = bm * 10
                maxCalories  = bm * 20
                -- Carrying capacity from muscle: more lean mass AND
                -- more strength = more capacity, sub-linearly in both
                -- so the product doesn't explode at the tails.
                -- Calibration (acolyte body block): average roll
                -- (lm ≈ 28.5, strength 1.0) → ~23 kg; an exceptional
                -- 2-sigma strongman (strength_base 1.4, lm ≈ 46)
                -- → ~41 kg. The weakest rolls (~11 kg) sit just under
                -- the full starting kit (~12 kg) — by design, the
                -- spawn-time capacity check sheds the pick/shovel
                -- instead of flooring the formula. Extrapolates to
                -- pack species: the technomule body (lm ≈ 87,
                -- strength ≈ 8) → ~163 kg base, before its +50%
                -- percentage modifier.
                carryCap     = 3.2 * ((lm * strength) ** 0.6)
            in HM.insert "strength_base"     strBase
             $ HM.insert "strength"          strength
             $ HM.insert "max_hydration"     maxHydration
             $ HM.insert "max_hunger"        maxHunger
             $ HM.insert "max_calories"      maxCalories
             $ HM.insert "carrying_capacity" carryCap s
        _ → s

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
