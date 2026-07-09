{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.List
  ( unitGetAllIdsFn
  , unitListFn
  , unitListDefsFn
  , unitListAnimationsFn
  , unitGetInfoFn
  , unknownUnitTexture
  , unknownUnitAnimFrame
  , unitGetFrameTextureFn
  , unitGetPortraitTextureFn
  )
    where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import Unit.Types
import Unit.Direction (Direction(..))
import Unit.Render (pickFrame)
import Unit.Sim.Types (UnitActivity(..), UnitSimState(..), MoveTarget(..), UnitThreadState(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (Camera2D(..))


-- | unit.getAllIds() — return a Lua array of every live unit's
--   integer id. Useful for per-tick iteration in scripts that don't
--   want to parse the human-readable string from unit.list.
-- | Instances of the ACTIVE world only — the world-scoping boundary for
--   listing / selection so a unit in another world never leaks into the
--   current one (#78). Empty when no world is active.
activeUnits ∷ EngineEnv → IO (HM.HashMap UnitId UnitInstance)
activeUnits env = do
    um ← readIORef (unitManagerRef env)
    mActive ← activeWorldPage env
    pure $ case mActive of
        Just (pid, _) → unitsOnPage pid (umInstances um)
        Nothing       → HM.empty

-- | Prettify a def name for UI display when no explicit display_name is
--   set: underscores → spaces, each word capitalised. "bear_brown" →
--   "Bear Brown", "acolyte" → "Acolyte".
prettifyDefName ∷ Text → Text
prettifyDefName = T.unwords . map capWord . T.words . T.map underToSpace
  where
    underToSpace c = if c ≡ '_' then ' ' else c
    capWord w = case T.uncons w of
        Nothing      → w
        Just (c, cs) → T.cons (toUpperC c) cs
    toUpperC c = if c ≥ 'a' ∧ c ≤ 'z'
                 then toEnum (fromEnum c - 32)
                 else c

unitGetAllIdsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAllIdsFn env = do
    ids ← Lua.liftIO $ HM.keys <$> activeUnits env
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] ids) $ \(i, uid) → do
        Lua.pushinteger (fromIntegral (unUnitId uid))
        Lua.rawseti (-2) (fromIntegral i)
    return 1

unitListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListFn env = do
    result ← Lua.liftIO $ do
        -- Active world page only — consistent with unit.getAllIds, so a
        -- unit on another world page never leaks into this listing (#377).
        entries ← HM.toList <$> activeUnits env
        if null entries
        then return "No units spawned"
        else return $ T.unpack $ T.intercalate "\n" $
            map (\(uid, inst) →
                "id=" <> T.pack (show (unUnitId uid))
                <> " " <> uiDefName inst
                <> " (" <> T.pack (show (uiGridX inst))
                <> ", " <> T.pack (show (uiGridY inst))
                <> ", " <> T.pack (show (uiGridZ inst)) <> ")"
            ) entries
    Lua.pushstring (TE.encodeUtf8 (T.pack result))
    return 1

-- | unit.listDefs() — Lua array of available unit definition names.
--   These are the keys loadable into `unit.spawn(name, ...)`.
unitListDefsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListDefsFn env = do
    names ← Lua.liftIO $ do
        um ← readIORef (unitManagerRef env)
        return $ HM.keys (umDefs um)
    Lua.newtable
    forM_ (zip [1..] names) $ \(i, name) → do
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.rawseti (-2) i
    return 1

-- | unit.listAnimations(uid) — Lua array of animation names declared
--   for the unit's def. Used by the debug anim panel to enumerate
--   playable animations for the selected unit. Returns nil if the
--   unit or its def can't be found.
unitListAnimationsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListAnimationsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mNames ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    def  ← HM.lookup (uiDefName inst) (umDefs um)
                    pure (HM.keys (udAnimations def))
            case mNames of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just names → do
                    Lua.newtable
                    forM_ (zip [1..] names) $ \(i, name) → do
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.rawseti (-2) i
                    return 1

-- | unit.getInfo(id) — returns a Lua table with the unit's render-visible
--   attributes, or nil if the unit doesn't exist. Used by the info panel.
unitGetInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetInfoFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPair ← Lua.liftIO $ do
                um  ← readIORef (unitManagerRef env)
                uts ← readIORef (utsRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    let mDef = HM.lookup (uiDefName inst) (umDefs um)
                        -- Current locomotion speed (tiles/s) for the
                        -- stamina tick: the active move target's speed
                        -- while the unit is actually walking/running,
                        -- else 0 (idle / transitioning / no target).
                        moveSpeed = case HM.lookup uid (utsSimStates uts) of
                            Just ss | isLocomoting (usState ss) →
                                maybe 0 mtSpeed (usTarget ss)
                            _ → 0
                        -- Signed slope grade under the moving unit (#375):
                        -- positive = heading uphill (1.0 = straight up a
                        -- ramp), negative = downhill. Same locomotion gate
                        -- as moveSpeed, so a stationary unit reads 0.
                        moveGrade = case HM.lookup uid (utsSimStates uts) of
                            Just ss | isLocomoting (usState ss) →
                                usMoveGrade ss
                            _ → 0
                        -- True while the unit is in a fall KNOCKDOWN (a
                        -- self-timed getup is pending). Lets the survival
                        -- revive logic leave knockdowns to the movement
                        -- tick, and the status panel explain why a unit is
                        -- down ("Knocked down" vs an exhaustion collapse).
                        knockedDown = case HM.lookup uid (utsSimStates uts) of
                            Just ss → maybe False (const True) (usGetUpAt ss)
                            _       → False
                    pure (inst, mDef, moveSpeed, moveGrade, knockedDown)
            case mPair of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just (inst, mDef, moveSpeed, moveGrade, knockedDown) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (uiDefName inst))
                    Lua.setfield (-2) "defName"
                    -- Persistent personal name; "" for unnamed units (#264).
                    Lua.pushstring (TE.encodeUtf8 (uiName inst))
                    Lua.setfield (-2) "name"
                    -- Species label: the def's display_name, else a
                    -- prettified def name ("bear_brown" → "Bear Brown").
                    Lua.pushstring (TE.encodeUtf8
                        (fromMaybe (prettifyDefName (uiDefName inst))
                                   (mDef >>= udDisplayName)))
                    Lua.setfield (-2) "displayName"
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridX inst)))
                    Lua.setfield (-2) "gridX"
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridY inst)))
                    Lua.setfield (-2) "gridY"
                    Lua.pushinteger (fromIntegral (uiGridZ inst))
                    Lua.setfield (-2) "gridZ"
                    -- Continuous vertical position. Equal to gridZ
                    -- except during climbs, where it lerps smoothly
                    -- from start-z to top-z.
                    Lua.pushnumber (Lua.Number (realToFrac (uiRealZ inst)))
                    Lua.setfield (-2) "realZ"
                    Lua.pushstring (TE.encodeUtf8 (dirToText (uiFacing inst)))
                    Lua.setfield (-2) "facing"
                    Lua.pushnumber (Lua.Number (realToFrac (uiBaseWidth inst)))
                    Lua.setfield (-2) "baseWidth"
                    Lua.pushstring (TE.encodeUtf8 (uiCurrentAnim inst))
                    Lua.setfield (-2) "currentAnim"
                    Lua.pushnumber (Lua.Number (realToFrac (uiAnimStart inst)))
                    Lua.setfield (-2) "animStart"
                    Lua.pushnumber (Lua.Number (realToFrac moveSpeed))
                    Lua.setfield (-2) "moveSpeed"
                    Lua.pushnumber (Lua.Number (realToFrac moveGrade))
                    Lua.setfield (-2) "moveGrade"
                    Lua.pushboolean knockedDown
                    Lua.setfield (-2) "knockedDown"
                    -- equipmentClass is per-def, not per-instance. Only
                    -- present in the table when the def declares one.
                    case mDef >>= udEquipmentClass of
                        Just cls → do
                            Lua.pushstring (TE.encodeUtf8 cls)
                            Lua.setfield (-2) "equipmentClass"
                        Nothing → pure ()
                    return 1

-- | True for activities where the unit is translating across the ground
--   (so its move speed feeds stamina drain). Transitions / idle / drink
--   etc. are stationary.
isLocomoting ∷ UnitActivity → Bool
isLocomoting Walking = True
isLocomoting Running = True
isLocomoting _       = False

dirToText ∷ Direction → Text
dirToText DirS  = "S"
dirToText DirSW = "SW"
dirToText DirW  = "W"
dirToText DirNW = "NW"
dirToText DirN  = "N"
dirToText DirNE = "NE"
dirToText DirE  = "E"
dirToText DirSE = "SE"

-- | Folder-name spelling shared by every unknown-unit fallback asset
--   (static rotations and, since #485, the animated frame sets).
unknownUnitDirName ∷ Direction → String
unknownUnitDirName DirS  = "south"
unknownUnitDirName DirSW = "south-west"
unknownUnitDirName DirW  = "west"
unknownUnitDirName DirNW = "north-west"
unknownUnitDirName DirN  = "north"
unknownUnitDirName DirNE = "north-east"
unknownUnitDirName DirE  = "east"
unknownUnitDirName DirSE = "south-east"

-- | Static per-direction placeholder for a unit whose declared
--   sprite/portrait/animation-frame texture is missing on disk (#478) —
--   one flat pose per compass direction. Used as-is for non-directional
--   slots (base sprite, portrait, directional sprite) and as the final
--   fallback for any animation `unknownUnitAnimFrame` doesn't cover.
unknownUnitTexture ∷ Direction → FilePath
unknownUnitTexture dir =
    "assets/textures/units/unknown_unit/rotations/"
    <> unknownUnitDirName dir <> ".png"

-- | Frame counts of the unknown-unit fallback's own authored animation
--   clips (#485), keyed by the SAME animation names real units declare
--   in their `animations:` block. Only covers the "core" clips the issue
--   scoped in (idle, walk) — anything else (attacks, death, ...) still
--   falls back to the single static pose via 'unknownUnitTexture'.
unknownUnitAnimFrameCounts ∷ [(Text, Int)]
unknownUnitAnimFrameCounts = [("idle", 4), ("walk", 6)]

-- | Per-animation-frame placeholder for a unit's missing animation
--   texture (#485): when the requesting animation is one of
--   'unknownUnitAnimFrameCounts', cycle through the unknown-unit's own
--   authored clip (index modulo its frame count) instead of freezing on
--   one pose for every frame, so a placeholder unit reads as "alive"
--   during headless/GUI iteration. Any other animation name — or a
--   direction the fallback clip doesn't have — falls back to the
--   static rotation.
unknownUnitAnimFrame ∷ Text → Direction → Int → FilePath
unknownUnitAnimFrame animName dir frameIdx =
    case lookup animName unknownUnitAnimFrameCounts of
        Just n  → "assets/textures/units/unknown_unit/animations/"
                  <> T.unpack animName <> "/" <> unknownUnitDirName dir
                  <> "/frame_" <> pad3 (frameIdx `mod` n) <> ".png"
        Nothing → unknownUnitTexture dir
  where
    pad3 k = let s = show k in replicate (3 - length s) '0' <> s

-- | unit.getFrameTexture(uid) → texture handle integer (0 if missing).
--   Returns the texture for the unit's current animation frame at the
--   active camera facing — re-query each tick to follow the animation.
--   Used by the v2 info pane to mirror the unit's sprite as a portrait.
unitGetFrameTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetFrameTextureFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushinteger 0
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mTex ← Lua.liftIO $ do
                um  ← readIORef (unitManagerRef env)
                cam ← readIORef (cameraRef env)
                now ← readIORef (gameTimeRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → Nothing
                    Just inst →
                        case HM.lookup (uiDefName inst) (umDefs um) of
                            Nothing  → Nothing
                            -- Lua only needs the texture handle; the flipX
                            -- flag from `pickFrame` is consumed by the
                            -- renderer at draw time, not here.
                            Just def → Just (fst (pickFrame now (camFacing cam) inst def))
            case mTex of
                Just (TextureHandle k) → Lua.pushinteger (fromIntegral k)
                Nothing → Lua.pushinteger 0
            return 1

-- | unit.getPortraitTexture(uid) → texture handle integer (0 if the
--   unit is missing or its def declares no authored `portrait:`).
--   The info pane prefers this static authored portrait and falls back
--   to `getFrameTexture` (the live animation frame) when it returns 0.
unitGetPortraitTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPortraitTextureFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushinteger 0
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mTex ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → Nothing
                    Just inst →
                        case HM.lookup (uiDefName inst) (umDefs um) of
                            Nothing  → Nothing
                            Just def → udPortrait def
            case mTex of
                Just (TextureHandle k) → Lua.pushinteger (fromIntegral k)
                Nothing → Lua.pushinteger 0
            return 1
