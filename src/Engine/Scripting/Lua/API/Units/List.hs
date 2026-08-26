{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Units.List
  ( unitGetAllIdsFn
  , unitListFn
  , unitListDefsFn
  , unitListAnimationsFn
  , unitGetInfoFn
  , unknownUnitTexture
  , unitGetFrameTextureFn
  , unitGetFrameSampleFn
  , unitGetPortraitTextureFn
  , prettifyDefName
  )
    where

import UPrelude
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv, activeWorldPageFrom)
import World.Page.Types (WorldPageId(..))
import Unit.Types
import Unit.Direction (Direction(..))
import Unit.Render (pickFrame)
import Unit.Sim.Types (UnitActivity(..), UnitSimState(..), MoveTarget(..), UnitThreadState(..))
import Unit.Pathing.Hazard (moveHazardPolicyToken)
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
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    mActive ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
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
                "id=" <> tshow (unUnitId uid)
                <> " " <> uiDefName inst
                <> " (" <> tshow (uiGridX inst)
                <> ", " <> tshow (uiGridY inst)
                <> ", " <> tshow (uiGridZ inst) <> ")"
            ) entries
    Lua.pushstring (TE.encodeUtf8 (T.pack result))
    return 1

-- | unit.listDefs() — Lua array of available unit definition names.
--   These are the keys loadable into `unit.spawn(name, ...)`.
unitListDefsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListDefsFn env = do
    names ← Lua.liftIO $ do
        um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
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
                um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
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
--
--   Resolves GLOBALLY, not on the active page: the table it answers with
--   describes the unit named, wherever it lives. Since #1673 that
--   includes a @page@ field carrying the instance's own @uiPage@, the
--   counterpart to @building.getInfo@'s (#76/#196), so a caller can pair
--   an actor with a candidate on the ACTOR'S page instead of trusting
--   whichever page was active when some other query ran.
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
                um  ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
                uts ← readIORef (ucUtsRef (toUnitCombatCapability env))
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
                        -- The IN-FLIGHT request's damaging-drop policy
                        -- (#1217), or Nothing when the unit has no move
                        -- target. Observing it is what lets a test tell a
                        -- protected ambient wander from an ordinary
                        -- command on the SAME unit.
                        moveHazard = case HM.lookup uid (utsSimStates uts) of
                            Just ss → moveHazardPolicyToken . mtHazard
                                          <$> usTarget ss
                            _       → Nothing
                    pure ( inst, mDef, moveSpeed, moveGrade, knockedDown
                         , moveHazard )
            case mPair of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just ( inst, mDef, moveSpeed, moveGrade, knockedDown
                     , moveHazard ) → do
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
                                   (mDef ⌦ udDisplayName)))
                    Lua.setfield (-2) "displayName"
                    -- The world page this unit lives on (#1673),
                    -- read off the instance's own uiPage — the same
                    -- ownership field building.getInfo reports as
                    -- "page" (#76/#196). Additive: it lets a caller
                    -- pair an actor with a candidate on the ACTOR's
                    -- page instead of trusting the active one, which
                    -- unit.getAllIds / building.getActiveIds /
                    -- craft.getBills each snapshot independently.
                    Lua.pushstring (TE.encodeUtf8 (case uiPage inst of
                        WorldPageId p → p))
                    Lua.setfield (-2) "page"
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
                    -- Only present while a move target exists; absent
                    -- rather than a made-up default when the unit is
                    -- standing still, since a policy with no request
                    -- would be a fiction.
                    forM_ moveHazard $ \tok → do
                        Lua.pushstring (TE.encodeUtf8 tok)
                        Lua.setfield (-2) "moveHazard"
                    -- equipmentClass is per-def, not per-instance. Only
                    -- present in the table when the def declares one.
                    case mDef ⌦ udEquipmentClass of
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
--   sprite/portrait/directional-sprite texture is missing on disk
--   (#478) — one flat pose per compass direction.
--
--   These are the DIRECT single-texture families, which D-8 leaves on
--   ordinary loading, so #478's policy is unchanged for them: a missing
--   visual degrades to this placeholder rather than failing the load.
--
--   There is no longer a per-animation-FRAME sibling. #485's
--   `unknownUnitAnimFrame` cycled the unknown-unit's own idle/walk clip
--   for a missing animation frame, and its one production caller was
--   the per-frame animation loader #1261 retired. An animation is now
--   one compiled atlas, and a missing or unusable atlas rejects the
--   whole unit definition with the artifact named (#1259) rather than
--   drawing a placeholder in its place — a deliberate difference,
--   because a broken compile that still renders is what that contract
--   exists to prevent. `unknown_unit`'s own idle/walk art is untouched;
--   since #1261 it is an ordinary registered unit's animation set.
unknownUnitTexture ∷ Direction → FilePath
unknownUnitTexture dir =
    "assets/textures/units/unknown_unit/rotations/"
    <> unknownUnitDirName dir <> ".png"

-- | unit.getFrameTexture(uid) → texture handle integer (0 if missing).
--   Returns the texture for the unit's current animation frame at the
--   active camera facing — re-query each tick to follow the animation.
--
--   A handle ALONE cannot describe an atlas-backed frame (#1259), so
--   nothing that displays a unit's live frame uses this any more: the
--   v2 info pane moved to 'unitGetFrameSampleFn'. It remains for
--   callers that genuinely want only the handle.
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
                um  ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
                cam ← readIORef (rvCameraRef (toRenderViewCapability env))
                now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → Nothing
                    Just inst →
                        case HM.lookup (uiDefName inst) (umDefs um) of
                            Nothing  → Nothing
                            -- Handle only. Enough for a WHOLE-IMAGE
                            -- sample (a T-pose's direct sprite), and
                            -- never enough for an animation frame,
                            -- which since #1261 is always an atlas cell
                            -- and needs the sub-rect too — such a
                            -- caller must use `unit.getFrameSample`.
                            Just def → Just (fsTexture (pickFrame now (camFacing cam) inst def))
            case mTex of
                Just (TextureHandle k) → Lua.pushinteger (fromIntegral k)
                Nothing → Lua.pushinteger 0
            return 1

-- | unit.getFrameSample(uid) → the unit's current animation frame as a
--   table, or nil when the unit or its def is missing.
--
--   @{ texture, u0, v0, u1, v1, flipX, width, height }@ — the stable
--   texture handle, the frame's own UV endpoints WITHIN that texture,
--   the mirror flag, and the frame's pixel dimensions when the sample
--   knows them (@width@\/@height@ are absent only for a WHOLE-IMAGE
--   sample — a T-pose's direct sprite, whose image IS the frame and
--   whose size the UI already gets from the texture itself).
--
--   'unitGetFrameTextureFn' remains for callers that genuinely only
--   want a handle, but it CANNOT describe an atlas-backed frame (#1259):
--   an atlas handle names the whole animation sheet, so a UI that pushed
--   it straight into @UI.setSpriteTexture@ would draw every direction
--   and every frame at once. Anything DISPLAYING a unit's live frame
--   must come through here and publish the whole sample with
--   @UI.setSpriteFrame@ — texture, sub-rect and mirror in ONE manager
--   transition. Setting them one at a time is not equivalent: the render
--   thread reads the manager concurrently, so a reader landing between
--   the writes gets the new atlas handle paired with the previous
--   frame's rect.
unitGetFrameSampleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetFrameSampleFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mSample ← Lua.liftIO $ do
                um  ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
                cam ← readIORef (rvCameraRef (toRenderViewCapability env))
                now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → Nothing
                    Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
                        Nothing  → Nothing
                        Just def → Just (pickFrame now (camFacing cam) inst def)
            case mSample of
                Nothing → Lua.pushnil
                Just smp → do
                    let (u0, v0, u1, v1) = fsUV smp
                        TextureHandle rawTex = fsTexture smp
                    Lua.newtable
                    pushIntField "texture" (fromIntegral rawTex)
                    pushNumField "u0" (realToFrac u0)
                    pushNumField "v0" (realToFrac v0)
                    pushNumField "u1" (realToFrac u1)
                    pushNumField "v1" (realToFrac v1)
                    Lua.pushstring "flipX" >> Lua.pushboolean (fsFlipX smp)
                        >> Lua.rawset (-3)
                    case fsCell smp of
                        Nothing → pure ()
                        Just (w, h) → do
                            pushIntField "width" (fromIntegral w)
                            pushIntField "height" (fromIntegral h)
            return 1
  where
    pushIntField k v = Lua.pushstring k >> Lua.pushinteger v >> Lua.rawset (-3)
    pushNumField k v = Lua.pushstring k >> Lua.pushnumber (Lua.Number v)
                           >> Lua.rawset (-3)

-- | unit.getPortraitTexture(uid) → texture handle integer (0 if the
--   unit is missing or its def declares no authored `portrait:`).
--   The info pane prefers this static authored portrait and falls back
--   to `getFrameSample` (the live animation frame, with its UV sub-rect
--   and mirror flag) when it returns 0.
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
                um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
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
