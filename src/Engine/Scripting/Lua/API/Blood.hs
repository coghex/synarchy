{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Debug/headless Lua surface for the blood decal model (#604) and its
--   procedural texture generation + world-render records (#606): spawn
--   a decal with explicit parameters, inspect the decal list and the
--   texture-descriptor FIFO (each descriptor now also reports its
--   generated pixel data via 'Blood.Texture'), query resolved per-decal
--   render records (Blood.Render — the same data
--   'World.Render.BloodQuads' turns into world-space quads, exposed
--   here so headless callers can verify renderability without a GPU),
--   and clear both. See Blood.Types, Blood.Texture, Blood.Render, and
--   docs/blood_decals.md.
module Engine.Scripting.Lua.API.Blood
    ( bloodSpawnFn
    , bloodGetDecalFn
    , bloodListDecalsFn
    , bloodGetTextureFn
    , bloodListTexturesFn
    , bloodGetTextureCapFn
    , bloodGetRenderQuadsFn
    , bloodClearFn
    ) where

import UPrelude
import Data.List (elemIndex)
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, atomicModifyIORef')
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Page.Types (WorldPageId(..))
import World.Types (WorldManager(..), WorldState(..))
import Unit.Types (UnitId(..))
import Blood.Types
import Blood.Texture (generateBloodTexture, bloodTextureHash, btiWidth, btiHeight)
import Blood.Render (BloodRenderRecord(..), bloodRenderRecords)

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
--   wetness (0..1, default 1 — a caller can spawn an already-drying
--   mark), sourceUnit (unit id), pageId (defaults to the active
--   world).
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
    wet       ← getFloatProp "wetness" 1
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
                                        , bspInitialWetness = max 0 (min 1 wet)
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

-- | blood.getTexture(textureId) → table | nil. @order@ is the
--   descriptor's actual 0-based FIFO rank (oldest = 0), matching what
--   'listTextures' reports for the same id — not hardcoded.
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
                    let tid = BloodTextureId (fromIntegral n)
                    pool ← bstPool ⊚ readIORef (wsBloodStoreRef ws)
                    pure $ (\rank → (rank, allTextures pool !! rank))
                        ⊚ elemIndex tid (map btdId (allTextures pool))
    case mTex of
        Just (rank, d) → pushTexture rank d >> return 1
        Nothing        → Lua.pushnil >> return 1

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

-- | blood.getRenderQuads([pageId]) → array of render-record tables
--   (issue #606: the resolved per-decal data a world-space quad needs —
--   see 'Blood.Render.bloodRenderRecords'), on the given page or the
--   active world if omitted. A decal whose texture reference has been
--   evicted never appears here (it's already gone from the decal list —
--   'removeDecalsForTexture' — and 'bloodRenderRecord' re-checks the
--   pool defensively on top of that). Each record's tint/alpha reflect
--   the decal's CURRENT age at call time (Blood.Render.decalTint), so a
--   decal spawned with a low @wetness@ reports a darker, fainter tint
--   than one spawned fresh — headless-observable aging, without a GPU.
bloodGetRenderQuadsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodGetRenderQuadsFn env = do
    mPageArg ← Lua.tostring 1
    let mPageStr = TE.decodeUtf8Lenient ⊚ mPageArg
    recs ← Lua.liftIO $ do
        mTarget ← resolveBloodPage env mPageStr
        case mTarget of
            Nothing         → pure []
            Just (pid, ws)  → do
                now   ← readIORef (gameTimeRef env)
                store ← readIORef (wsBloodStoreRef ws)
                pure (bloodRenderRecords now pid store)
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] recs) $ \(i, r) → do
        pushRenderRecord r
        Lua.rawseti (-2) (fromIntegral i)
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
--   severity, footprint, anisotropy, edge, seed, width, height,
--   pixelHash }. The last three (#606) confirm generated texture data
--   actually exists for this descriptor — regenerated on demand from
--   the descriptor via 'generateBloodTexture' (pure and deterministic,
--   so this never drifts from what 'World.Render.BloodQuads' uploads)
--   rather than cached alongside it.
pushTexture ∷ Int → BloodTextureDescriptor → Lua.LuaE Lua.Exception ()
pushTexture rank d = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putS k v = Lua.pushstring (TE.encodeUtf8 v) >> Lua.setfield (-2) k
        img = generateBloodTexture d
    putI "id"    (unBloodTextureId (btdId d))
    putI "order" rank
    putS "style"      (styleText (btdStyle d))
    putS "woundKind"  (btdWoundKind d)
    putS "severity"   (severityText (btdSeverity d))
    putS "footprint"  (footprintText (btdFootprint d))
    putS "anisotropy" (anisotropyText (btdAnisotropy d))
    putS "edge"       (edgeText (btdEdge d))
    putI "seed" (btdSeed d)
    putI "width"     (btiWidth img)
    putI "height"    (btiHeight img)
    putI "pixelHash" (bloodTextureHash img)

-- | Push one decal: { id, texture, page, x, y, surfaceZ, offsetX,
--   offsetY, rotation, scale, createdAt, age, wetness, dryness,
--   woundKind, severity, sourceUnit, opacity }. @age@/@wetness@
--   (design doc's "current age/wetness/dryness") are derived from the
--   caller's current game time plus the decal's stored creation time /
--   initial wetness — see 'Blood.Types.wetnessAt' — not themselves
--   stored (no ticking system owns aging yet). @dryness@ is simply
--   @1 - wetness@, exposed directly rather than making callers compute it.
pushDecal ∷ Double → BloodDecal → Lua.LuaE Lua.Exception ()
pushDecal now d = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putN k v = Lua.pushnumber (Lua.Number (realToFrac v)) >> Lua.setfield (-2) k
        putS k v = Lua.pushstring (TE.encodeUtf8 v) >> Lua.setfield (-2) k
        wetness = wetnessAt now d
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
    putN "wetness" wetness
    putN "dryness" (1 - wetness)
    putS "woundKind" (bdeWoundKind d)
    putS "severity"  (severityText (bdeSeverity d))
    (case bdeSourceUnit d of
        Just (UnitId uid) → putI "sourceUnit" uid
        Nothing           → Lua.pushnil >> Lua.setfield (-2) "sourceUnit")
    putN "opacity" (bdeOpacity d)

-- | Push one render record: { decal, texture, page, x, y, surfaceZ,
--   offsetX, offsetY, rotation, scale, tintR, tintG, tintB, alpha } —
--   the resolved data 'World.Render.BloodQuads' turns into a world-space
--   quad (Blood.Render.BloodRenderRecord). @tintR/G/B@ and @alpha@
--   already fold in aging (Blood.Render.decalTint): a fresher decal
--   reports a brighter tint and higher alpha than an older one.
pushRenderRecord ∷ BloodRenderRecord → Lua.LuaE Lua.Exception ()
pushRenderRecord r = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putN k v = Lua.pushnumber (Lua.Number (realToFrac v)) >> Lua.setfield (-2) k
        putS k v = Lua.pushstring (TE.encodeUtf8 v) >> Lua.setfield (-2) k
    putI "decal"   (unBloodDecalId (brrDecal r))
    putI "texture" (unBloodTextureId (brrTexture r))
    (case brrPage r of WorldPageId pageTxt → putS "page" pageTxt)
    putN "x" (brrX r)
    putN "y" (brrY r)
    putI "surfaceZ" (brrSurfaceZ r)
    putN "offsetX" (brrOffsetX r)
    putN "offsetY" (brrOffsetY r)
    putN "rotation" (brrRotation r)
    putN "scale"    (brrScale r)
    putN "tintR" (brrTintR r)
    putN "tintG" (brrTintG r)
    putN "tintB" (brrTintB r)
    putN "alpha" (brrAlpha r)

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
