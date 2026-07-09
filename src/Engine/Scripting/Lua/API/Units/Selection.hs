{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Selection
  ( unitSelectFn
  , unitDeselectAllFn
  , unitGetSelectedFn
  , unitIsSelectedFn
  , unitHitTestAtFn
  , unitHitTestInRectFn
  , unitSetSelectionFn
  , unitSetAnimFn
  , unitSetAnimOverrideFn
  , unitClearAnimOverrideFn
  , unitSetFacingFn
  , unitSetFrozenFn
  , unitSetForceLoopFn
  )
    where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Unit.Types
import Unit.Direction (Direction(..))
import qualified Unit.Selection as Sel
import qualified Unit.HitTest as HitTest


-- * Selection

-- | unit.select(id) — replace the selection with a single unit.
--   Returns true if the unit exists, false if not.
unitSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSelectFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            ok ← Lua.liftIO $ Sel.selectUnit env uid
            Lua.pushboolean ok
            return 1

-- | unit.deselectAll() — empty the selection. Always returns true.
unitDeselectAllFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDeselectAllFn env = do
    Lua.liftIO $ Sel.clearSelection env
    Lua.pushboolean True
    return 1

-- | unit.getSelected() — returns a Lua array of integer unit IDs.
--   Filtered to only live units.
unitGetSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetSelectedFn env = do
    selected ← Lua.liftIO $ Sel.getSelected env
    let ids = HS.toList selected
    Lua.newtable
    forM_ (zip [1..] ids) $ \(i, uid) → do
        Lua.pushinteger (fromIntegral (unUnitId uid))
        Lua.rawseti (-2) i
    return 1

-- | unit.isSelected(id) — bool.
unitIsSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitIsSelectedFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            sel ← Lua.liftIO $ Sel.isSelected env uid
            Lua.pushboolean sel
            return 1

-- | unit.hitTestAt(screenX, screenY) — returns the unit ID under the
--   given framebuffer-pixel coordinates, or nil if no unit is hit.
--
--   Lua side passes the raw GLFW mouse position (window-space pixels,
--   pre-DPI-scaling). `Unit.HitTest.hitTestUnitAt` does the projection
--   and per-unit AABB test against the sprite quad.
unitHitTestAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitHitTestAtFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just (Lua.Number x), Just (Lua.Number y)) → do
            mUid ← Lua.liftIO $ HitTest.hitTestUnitAt env
                                  (realToFrac x) (realToFrac y)
            case mUid of
                Just uid → do
                    Lua.pushinteger (fromIntegral (unUnitId uid))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.hitTestInRect(x1, y1, x2, y2) — returns a Lua array of unit
--   IDs whose sprite-quad center falls inside the screen rect (window
--   pixels). Used by drag-box selection.
unitHitTestInRectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitHitTestInRectFn env = do
    x1A ← Lua.tonumber 1
    y1A ← Lua.tonumber 2
    x2A ← Lua.tonumber 3
    y2A ← Lua.tonumber 4
    case (x1A, y1A, x2A, y2A) of
        (Just (Lua.Number a), Just (Lua.Number b),
         Just (Lua.Number c), Just (Lua.Number d)) → do
            ids ← Lua.liftIO $ HitTest.hitTestUnitsInRect env
                                  (realToFrac a) (realToFrac b)
                                  (realToFrac c) (realToFrac d)
            Lua.newtable
            forM_ (zip [1..] ids) $ \(i, uid) → do
                Lua.pushinteger (fromIntegral (unUnitId uid))
                Lua.rawseti (-2) i
            return 1
        _ → do
            Lua.newtable
            return 1

-- | unit.setSelection(idTable) — replace the selection with the given
--   array of unit IDs. IDs not corresponding to live units are filtered
--   out by the underlying setSelection.
unitSetSelectionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetSelectionFn env = do
    n ← Lua.rawlen 1   -- 0 if arg 1 isn't a table; loop body skips
    let go i acc
            | i > fromIntegral n = return acc
            | otherwise = do
                _ ← Lua.rawgeti 1 i
                m ← Lua.tointeger (-1)
                Lua.pop 1
                case m of
                    Just k  → go (i + 1) (UnitId (fromIntegral k) : acc)
                    Nothing → go (i + 1) acc
    ids ← go 1 []
    Lua.liftIO $ Sel.setSelection env (HS.fromList ids)
    Lua.pushboolean True
    return 1

-- | unit.setAnim(id, name) — sets the raw animation name on a unit and
--   resets its start time to now. Empty string clears the animation back
--   to T-pose. No state-name resolution; that's Phase 3's `setUnitAnim`.
--   Returns true on success, false if the unit doesn't exist.
unitSetAnimFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetAnimFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
            ok ← Lua.liftIO $ do
                now ← readIORef (gameTimeRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let inst' = inst { uiCurrentAnim = name
                                             , uiAnimStart   = now
                                             }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.setAnimOverride(uid, name) — Lua-driven animation that wins
--   over the engine's state-driven anim resolution. Use this for combat
--   swings (one-shot attack anims), posture animations (sit/lie/sleep
--   for bears), or any visual that doesn't map to a sim (pose, activity)
--   pair. Survives publishToRender each tick. Pass an empty string OR
--   call clearAnimOverride to release.
unitSetAnimOverrideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetAnimOverrideFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
            ok ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let inst' = inst { uiAnimOverride = name }
                            in (um { umInstances =
                                    HM.insert uid inst' (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.clearAnimOverride(uid) — releases the override so the engine's
--   state-driven anim resolution resumes on the next sim tick.
unitClearAnimOverrideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitClearAnimOverrideFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            let uid = UnitId (fromIntegral n)
            ok ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let inst' = inst { uiAnimOverride = "" }
                            in (um { umInstances =
                                    HM.insert uid inst' (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.setFacing(uid, dirStr) — force the unit's facing direction.
--   `dirStr` is one of "S", "SW", "W", "NW", "N", "NE", "E", "SE"
--   (case-insensitive; long forms "south", "south-east" also accepted).
--   Used by the debug anim panel to cycle a unit through all 8
--   directions while previewing an animation. Returns true on
--   success, false on unknown direction or missing unit.
unitSetFacingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetFacingFn env = do
    idArg  ← Lua.tointeger 1
    dirArg ← Lua.tostring 2
    case (idArg, dirArg) of
        (Just n, Just dirBS) → do
            let uid    = UnitId (fromIntegral n)
                dirTxt = T.toUpper (TE.decodeUtf8Lenient dirBS)
            case parseDir dirTxt of
                Nothing → do
                    Lua.pushboolean False
                    return 1
                Just dir → do
                    ok ← Lua.liftIO $ atomicModifyIORef'
                                        (unitManagerRef env) $ \um →
                        case HM.lookup uid (umInstances um) of
                            Nothing → (um, False)
                            Just inst →
                                let inst' = inst { uiFacing = dir }
                                in (um { umInstances = HM.insert uid inst'
                                                         (umInstances um) }
                                   , True)
                    Lua.pushboolean ok
                    return 1
        _ → do
            Lua.pushboolean False
            return 1
  where
    parseDir t = case t of
        "S"           → Just DirS
        "SOUTH"       → Just DirS
        "SW"          → Just DirSW
        "SOUTH-WEST"  → Just DirSW
        "SOUTH_WEST"  → Just DirSW
        "W"           → Just DirW
        "WEST"        → Just DirW
        "NW"          → Just DirNW
        "NORTH-WEST"  → Just DirNW
        "NORTH_WEST"  → Just DirNW
        "N"           → Just DirN
        "NORTH"       → Just DirN
        "NE"          → Just DirNE
        "NORTH-EAST"  → Just DirNE
        "NORTH_EAST"  → Just DirNE
        "E"           → Just DirE
        "EAST"        → Just DirE
        "SE"          → Just DirSE
        "SOUTH-EAST"  → Just DirSE
        "SOUTH_EAST"  → Just DirSE
        _             → Nothing

-- | unit.setFrozen(uid, on) — toggle the debug freeze flag. While
--   frozen, the unit thread's `publishToRender` does NOT update the
--   unit's position / facing / anim / activity / pose from sim state
--   — Lua scripts hold full control of those via setAnim / setFacing
--   / setPos. AI keeps ticking but its commands have no visible effect
--   until the flag is cleared.
--
--   Used by the debug anim panel so previewed animations aren't
--   stomped between frames by the still-running AI / sim loop. Don't
--   leave units frozen in saved worlds — the flag is runtime-only
--   (defaults to False on load) but a frozen unit ignores its own AI.
unitSetFrozenFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetFrozenFn env = do
    idArg ← Lua.tointeger 1
    onArg ← Lua.toboolean 2
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                on  = onArg
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiFrozen = on }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1

-- | unit.setForceLoop(uid, on) — debug-only override that makes the
--   renderer loop the current animation even when its YAML
--   `loop: false`. Used by the anim panel so one-shot animations
--   (attacks, transitions, death) play continuously while previewed
--   inside a direction window. Clear when leaving preview to restore
--   normal one-shot behaviour.
unitSetForceLoopFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetForceLoopFn env = do
    idArg ← Lua.tointeger 1
    onArg ← Lua.toboolean 2
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                on  = onArg
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiForceLoop = on }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
