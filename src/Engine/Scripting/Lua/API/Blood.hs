{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Debug/headless Lua surface for the blood decal model (#604):
--   spawn a decal with explicit parameters, inspect the decal list and
--   the texture-descriptor FIFO, and clear both. No rendering, no
--   combat hook, no real GPU texture generation, no save/load — see
--   Blood.Types and docs/blood_decals.md.
module Engine.Scripting.Lua.API.Blood
    ( bloodSpawnFn
    , bloodGetDecalFn
    , bloodListDecalsFn
    , bloodGetTextureFn
    , bloodListTexturesFn
    , bloodGetTextureCapFn
    , bloodClearFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, atomicModifyIORef')
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Page.Types (WorldPageId(..))
import World.Types (WorldManager(..), WorldState(..))
import Unit.Types (UnitId(..))
import Blood.Types

-- | Resolve which world page a blood op targets: a named page (any in
--   wmWorlds, even hidden/non-active) when a page id is given, else
--   the active world — mirrors
--   Engine.Scripting.Lua.API.Items.resolveItemPage.
resolveBloodPage ∷ EngineEnv → Maybe Text → IO (Maybe (WorldPageId, WorldState))
resolveBloodPage env (Just pid) = do
    wm ← readIORef (worldManagerRef env)
    let target = WorldPageId pid
    pure $ (\ws → (target, ws)) <$> lookup target (wmWorlds wm)
resolveBloodPage env Nothing = activeWorldPage env

-- | blood.spawn(gx, gy, woundKind, severity [, props]) → decalId,
--   textureId, isNewTexture on success, or nil, reason on failure.
--   gx/gy are world tile-space floats (Item.Ground convention).
--   woundKind is free-form text (no closed WoundKind type exists
--   engine-side — mirrors Combat.Wounds.woundKind); severity is one of
--   "minor"|"moderate"|"severe"|"catastrophic".
--
--   Optional @props@ table: style ("pool"|"drops"|"spatter"|"streak"|
--   "smear", default derived from woundKind), footprint
--   ("small"|"medium"|"large", default "medium"), anisotropy
--   ("none"|"low"|"high", default "none"), edge
--   ("smooth"|"moderate"|"rough", default "moderate"), seed (int,
--   default 0), surfaceZ (int, default 0), offsetX/offsetY (default
--   0), rotation (default 0), scale (default 1), opacity (default 1),
--   sourceUnit (unit id), pageId (defaults to the active world).
--
--   Any of style/footprint/anisotropy/edge/severity given but
--   unrecognised fails the call outright (nil, reason) rather than
--   silently substituting a default — a typo in an explicit bucket
--   should not masquerade as a different, valid mark.
bloodSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodSpawnFn env = do
    xArg    ← Lua.tonumber 1
    yArg    ← Lua.tonumber 2
    windArg ← Lua.tostring 3
    sevArg  ← Lua.tostring 4
    propsTy ← Lua.ltype 5
    let getStrProp ∷ Lua.Name → Lua.LuaE Lua.Exception (Maybe Text)
        getStrProp key = case propsTy of
            Lua.TypeTable → do
                _ ← Lua.getfield 5 key
                mv ← Lua.tostring Lua.top
                Lua.pop 1
                pure (TE.decodeUtf8Lenient ⊚ mv)
            _ → pure Nothing
        getNumProp ∷ Lua.Name → Lua.LuaE Lua.Exception (Maybe Double)
        getNumProp key = case propsTy of
            Lua.TypeTable → do
                _ ← Lua.getfield 5 key
                mv ← Lua.tonumber Lua.top
                Lua.pop 1
                pure $ case mv of
                    Just (Lua.Number n) → Just (realToFrac n)
                    _ → Nothing
            _ → pure Nothing
        getFloatProp ∷ Lua.Name → Float → Lua.LuaE Lua.Exception Float
        getFloatProp key def = maybe def realToFrac ⊚ getNumProp key
        getIntProp ∷ Lua.Name → Int → Lua.LuaE Lua.Exception Int
        getIntProp key def = maybe def round ⊚ getNumProp key
    mStyleStr ← getStrProp "style"
    mFootStr  ← getStrProp "footprint"
    mAnisoStr ← getStrProp "anisotropy"
    mEdgeStr  ← getStrProp "edge"
    seedI     ← getIntProp "seed" 0
    surfaceZ  ← getIntProp "surfaceZ" 0
    offX      ← getFloatProp "offsetX" 0
    offY      ← getFloatProp "offsetY" 0
    rot       ← getFloatProp "rotation" 0
    scl       ← getFloatProp "scale" 1
    opac      ← getFloatProp "opacity" 1
    mUnitN    ← getNumProp "sourceUnit"
    mPageStr  ← getStrProp "pageId"
    let fail_ msg = Lua.pushnil >> Lua.pushstring msg >> return 2
    case (xArg, yArg, windArg, sevArg) of
        (Just xN, Just yN, Just windBS, Just sevBS) → do
            let woundKind = TE.decodeUtf8Lenient windBS
                parsedSeverity  = parseSeverity (TE.decodeUtf8Lenient sevBS)
                parsedStyle     = maybe (Just (defaultStyleForWound woundKind))
                                        parseStyle mStyleStr
                parsedFootprint = maybe (Just FootprintMedium) parseFootprint mFootStr
                parsedAniso     = maybe (Just AnisotropyNone) parseAnisotropy mAnisoStr
                parsedEdge      = maybe (Just EdgeModerate) parseEdge mEdgeStr
            case (parsedSeverity, parsedStyle, parsedFootprint, parsedAniso, parsedEdge) of
                (Just severity, Just style, Just footprint, Just aniso, Just edge) → do
                    let req = BloodTextureRequest
                            { btrStyle      = style
                            , btrWoundKind  = woundKind
                            , btrSeverity   = severity
                            , btrFootprint  = footprint
                            , btrAnisotropy = aniso
                            , btrEdge       = edge
                            , btrSeed       = seedI
                            }
                        mSourceUnit = UnitId . round ⊚ mUnitN
                    result ← Lua.liftIO $ do
                        mTarget ← resolveBloodPage env mPageStr
                        case mTarget of
                            Nothing → pure Nothing
                            Just (pid, ws) → do
                                now ← readIORef (gameTimeRef env)
                                let mkSpec tid = BloodDecalSpec
                                        { bspTexture    = tid
                                        , bspPage       = pid
                                        , bspX          = realToFrac xN
                                        , bspY          = realToFrac yN
                                        , bspSurfaceZ   = surfaceZ
                                        , bspOffsetX    = offX
                                        , bspOffsetY    = offY
                                        , bspRotation   = rot
                                        , bspScale      = scl
                                        , bspCreatedAt  = now
                                        , bspWoundKind  = woundKind
                                        , bspSeverity   = severity
                                        , bspSourceUnit = mSourceUnit
                                        , bspOpacity    = opac
                                        }
                                Just ⊚ atomicModifyIORef' (wsBloodStoreRef ws)
                                    (\store →
                                        let (store', did, tid, isNew) =
                                                spawnDecal req mkSpec store
                                        in (store', (did, tid, isNew)))
                    case result of
                        Just (did, tid, isNew) → do
                            Lua.pushinteger (fromIntegral (unBloodDecalId did))
                            Lua.pushinteger (fromIntegral (unBloodTextureId tid))
                            Lua.pushboolean isNew
                            return 3
                        Nothing → fail_ "blood.spawn: no active world"
                _ → fail_ "blood.spawn: unknown style/severity/footprint/\
                          \anisotropy/edge value"
        _ → fail_ "blood.spawn: expected (gx, gy, woundKind, severity\
                  \ [, props])"

-- | blood.getDecal(decalId) → table | nil.
bloodGetDecalFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodGetDecalFn env = do
    idArg ← Lua.tointeger 1
    mDecal ← case idArg of
        Nothing → return Nothing
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) → do
                    now   ← readIORef (gameTimeRef env)
                    store ← readIORef (wsBloodStoreRef ws)
                    pure $ (\d → (d, now)) ⊚
                        lookupDecal (BloodDecalId (fromIntegral n)) (bstDecals store)
    case mDecal of
        Just (d, now) → pushDecal now d >> return 1
        Nothing       → Lua.pushnil >> return 1

-- | blood.listDecals() → array of decal tables on the active world,
--   oldest first.
bloodListDecalsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodListDecalsFn env = do
    (now, decalList) ← Lua.liftIO $ do
        mPage ← activeWorldPage env
        case mPage of
            Nothing      → pure (0, [])
            Just (_, ws) → do
                now   ← readIORef (gameTimeRef env)
                store ← readIORef (wsBloodStoreRef ws)
                pure (now, allDecals (bstDecals store))
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] decalList) $ \(i, d) → do
        pushDecal now d
        Lua.rawseti (-2) (fromIntegral i)
    return 1

-- | blood.getTexture(textureId) → table | nil.
bloodGetTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodGetTextureFn env = do
    idArg ← Lua.tointeger 1
    mTex ← case idArg of
        Nothing → return Nothing
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) → do
                    store ← readIORef (wsBloodStoreRef ws)
                    pure $ lookupTexture (BloodTextureId (fromIntegral n))
                                          (bstPool store)
    case mTex of
        Just d  → pushTexture 0 d >> return 1
        Nothing → Lua.pushnil >> return 1

-- | blood.listTextures() → array of descriptor tables on the active
--   world's texture pool, oldest (front of the FIFO) first. Each entry
--   carries its 0-based FIFO rank so callers can see eviction order
--   without separately tracking insertion sequence.
bloodListTexturesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodListTexturesFn env = do
    texList ← Lua.liftIO $ do
        mPage ← activeWorldPage env
        case mPage of
            Nothing      → return []
            Just (_, ws) → allTextures . bstPool ⊚ readIORef (wsBloodStoreRef ws)
    Lua.newtable
    forM_ (zip [0 ∷ Int ..] texList) $ \(rank, d) → do
        pushTexture rank d
        Lua.rawseti (-2) (fromIntegral rank + 1)
    return 1

-- | blood.getTextureCap() → integer — the active world's configured
--   texture-pool cap (0 if no active world).
bloodGetTextureCapFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodGetTextureCapFn env = do
    cap ← Lua.liftIO $ do
        mPage ← activeWorldPage env
        case mPage of
            Nothing      → return 0
            Just (_, ws) → btpCap . bstPool ⊚ readIORef (wsBloodStoreRef ws)
    Lua.pushinteger (fromIntegral cap)
    return 1

-- | blood.clear() → true. Empties both the decal list and the texture
--   pool on the active world (issue #604 acceptance: "clear leaves
--   both descriptor and decal lists empty").
bloodClearFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodClearFn env = do
    ok ← Lua.liftIO $ do
        mPage ← activeWorldPage env
        case mPage of
            Nothing      → return False
            Just (_, ws) → do
                atomicModifyIORef' (wsBloodStoreRef ws) $ \store →
                    (clearBlood store, ())
                return True
    Lua.pushboolean ok
    return 1

-- | Push one texture descriptor: { id, order, style, woundKind,
--   severity, footprint, anisotropy, edge, seed }.
pushTexture ∷ Int → BloodTextureDescriptor → Lua.LuaE Lua.Exception ()
pushTexture rank d = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putS k v = Lua.pushstring (TE.encodeUtf8 v) >> Lua.setfield (-2) k
    putI "id"    (unBloodTextureId (btdId d))
    putI "order" rank
    putS "style"      (styleText (btdStyle d))
    putS "woundKind"  (btdWoundKind d)
    putS "severity"   (severityText (btdSeverity d))
    putS "footprint"  (footprintText (btdFootprint d))
    putS "anisotropy" (anisotropyText (btdAnisotropy d))
    putS "edge"       (edgeText (btdEdge d))
    putI "seed" (btdSeed d)

-- | Push one decal: { id, texture, page, x, y, surfaceZ, offsetX,
--   offsetY, rotation, scale, createdAt, age, woundKind, severity,
--   sourceUnit, opacity }. @age@ (design doc's "current age") is
--   derived from the caller's current game time, not stored.
pushDecal ∷ Double → BloodDecal → Lua.LuaE Lua.Exception ()
pushDecal now d = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putN k v = Lua.pushnumber (Lua.Number (realToFrac v)) >> Lua.setfield (-2) k
        putS k v = Lua.pushstring (TE.encodeUtf8 v) >> Lua.setfield (-2) k
    putI "id"      (unBloodDecalId (bdeId d))
    putI "texture" (unBloodTextureId (bdeTexture d))
    (case bdePage d of WorldPageId pageTxt → putS "page" pageTxt)
    putN "x" (bdeX d)
    putN "y" (bdeY d)
    putI "surfaceZ" (bdeSurfaceZ d)
    putN "offsetX" (bdeOffsetX d)
    putN "offsetY" (bdeOffsetY d)
    putN "rotation" (bdeRotation d)
    putN "scale"    (bdeScale d)
    putN "createdAt" (bdeCreatedAt d)
    putN "age" (max 0 (now - bdeCreatedAt d))
    putS "woundKind" (bdeWoundKind d)
    putS "severity"  (severityText (bdeSeverity d))
    (case bdeSourceUnit d of
        Just (UnitId uid) → putI "sourceUnit" uid
        Nothing           → Lua.pushnil >> Lua.setfield (-2) "sourceUnit")
    putN "opacity" (bdeOpacity d)

-- | The style a bare (no explicit style) request defaults to, keyed
--   off the wound kind — mirrors docs/blood_decals.md's "Injury
--   behavior" mapping (stab → pool, slash → directional streak, blunt/
--   arterial/severed → spatter). Anything else defaults to a small
--   drops mark rather than guessing a directional shape.
defaultStyleForWound ∷ Text → BloodStyle
defaultStyleForWound wk = case wk of
    "stab"     → StylePool
    "slash"    → StyleStreak
    "blunt"    → StyleSpatter
    "arterial" → StyleSpatter
    "severed"  → StyleSpatter
    _          → StyleDrops

parseStyle ∷ Text → Maybe BloodStyle
parseStyle "pool"    = Just StylePool
parseStyle "drops"   = Just StyleDrops
parseStyle "spatter" = Just StyleSpatter
parseStyle "streak"  = Just StyleStreak
parseStyle "smear"   = Just StyleSmear
parseStyle _         = Nothing

styleText ∷ BloodStyle → Text
styleText StylePool    = "pool"
styleText StyleDrops   = "drops"
styleText StyleSpatter = "spatter"
styleText StyleStreak  = "streak"
styleText StyleSmear   = "smear"

parseSeverity ∷ Text → Maybe SeverityBucket
parseSeverity "minor"        = Just SeverityMinor
parseSeverity "moderate"     = Just SeverityModerate
parseSeverity "severe"       = Just SeveritySevere
parseSeverity "catastrophic" = Just SeverityCatastrophic
parseSeverity _              = Nothing

severityText ∷ SeverityBucket → Text
severityText SeverityMinor        = "minor"
severityText SeverityModerate     = "moderate"
severityText SeveritySevere       = "severe"
severityText SeverityCatastrophic = "catastrophic"

parseFootprint ∷ Text → Maybe FootprintBucket
parseFootprint "small"  = Just FootprintSmall
parseFootprint "medium" = Just FootprintMedium
parseFootprint "large"  = Just FootprintLarge
parseFootprint _        = Nothing

footprintText ∷ FootprintBucket → Text
footprintText FootprintSmall  = "small"
footprintText FootprintMedium = "medium"
footprintText FootprintLarge  = "large"

parseAnisotropy ∷ Text → Maybe AnisotropyBucket
parseAnisotropy "none" = Just AnisotropyNone
parseAnisotropy "low"  = Just AnisotropyLow
parseAnisotropy "high" = Just AnisotropyHigh
parseAnisotropy _      = Nothing

anisotropyText ∷ AnisotropyBucket → Text
anisotropyText AnisotropyNone = "none"
anisotropyText AnisotropyLow  = "low"
anisotropyText AnisotropyHigh = "high"

parseEdge ∷ Text → Maybe EdgeBucket
parseEdge "smooth"   = Just EdgeSmooth
parseEdge "moderate" = Just EdgeModerate
parseEdge "rough"    = Just EdgeRough
parseEdge _          = Nothing

edgeText ∷ EdgeBucket → Text
edgeText EdgeSmooth   = "smooth"
edgeText EdgeModerate = "moderate"
edgeText EdgeRough    = "rough"
