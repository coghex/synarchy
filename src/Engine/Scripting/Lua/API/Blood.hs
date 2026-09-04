{-# LANGUAGE Strict #-}
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
    , bloodGpuStatsFn
    , bloodGpuHandlesFn
    , bloodClearFn
    , bloodGetTrailStateFn
    ) where

import UPrelude
import Data.List (elemIndex, sortOn)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.IORef (readIORef, atomicModifyIORef')
import GHC.Float (double2Float)
import qualified HsLua as Lua
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv, unitManagerRef, activeWorldPageFrom)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Scripting.Lua.Util (isDenseArray)
import World.Page.Types (WorldPageId(..))
import World.Types (WorldManager(..), WorldState(..))
import Unit.Types (UnitId(..), UnitManager(..), UnitInstance(..), TrailState(..))
import Blood.Types
import Blood.Texture (generateBloodTexture, bloodTextureHash, btiWidth, btiHeight)
import Blood.Render (BloodRenderRecord(..), bloodRenderRecords)
import Blood.Impact (defaultStyleForWound)
import Blood.Pool (defaultPoolThresholds, poolAtBound)

-- | Resolve which world page a blood op targets: a named page (any in
--   wmWorlds, even hidden/non-active) when a page id is given, else
--   the active world — mirrors
--   Engine.Scripting.Lua.API.Items.resolveItemPage.
resolveBloodPage ∷ EngineEnv → Maybe Text → IO (Maybe (WorldPageId, WorldState))
resolveBloodPage env (Just pid) = do
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    let target = WorldPageId pid
    pure $ (\ws → (target, ws)) <$> lookup target (wmWorlds wm)
resolveBloodPage env Nothing = activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))

-- * The geometry domain (#2336)

-- | What one @blood.spawn@ geometry value may be once narrowed to the
--   'Float' 'Blood.Types.BloodDecalSpec' stores it as.
--
--   Every constructor excludes NaN and both infinities; they differ
--   only in what they additionally require of a finite value.
data BloodFloatDomain
  = BloodFinite
    -- ^ A coordinate or an offset or a rotation: meaningful at any
    --   finite magnitude, of either sign.
  | BloodPositive
    -- ^ @scale@: a multiplier, for which zero collapses the quad and a
    --   negative value mirrors it — neither is a mark anything asks for.
  | BloodUnitInterval
    -- ^ @opacity@: a blend weight, on the CLOSED interval, so a fully
    --   transparent 0 and a fully opaque 1 both stay legal.
  deriving (Eq, Show)

-- | The domain, in the words a refusal quotes.
describeBloodFloatDomain ∷ BloodFloatDomain → Text
describeBloodFloatDomain BloodFinite       = "a finite number"
describeBloodFloatDomain BloodPositive     = "a finite number above 0"
describeBloodFloatDomain BloodUnitInterval = "a finite number from 0 to 1"

-- | 'Nothing' for a value inside the domain, the refusal reason
--   otherwise.
--
--   Hand this the value the decal would STORE — the result of
--   'narrowBloodFloat' — never the source 'Double'. The narrowing is
--   itself a way out of the domain: @1e39@ is a finite Lua number and
--   an infinite @Float@, and the decal keeps the @Float@.
--
--   NaN and the infinities fail by the ordinary comparisons rather than
--   by a test of their own, except for 'BloodFinite', which has no
--   bound to fail against and so names them directly.
checkBloodFloat ∷ Text → BloodFloatDomain → Float → Maybe Text
checkBloodFloat field domain x
    | isNaN x ∨ isInfinite x = reject
    | inside                 = Nothing
    | otherwise              = reject
  where
    inside = case domain of
        BloodFinite       → True
        BloodPositive     → x > 0
        BloodUnitInterval → x ≥ 0 ∧ x ≤ 1
    reject = Just $ "blood.spawn: " <> field <> " = " <> tshow x
                <> " is outside the domain ("
                <> describeBloodFloatDomain domain <> ")"

-- | 'checkBloodFloat' in the shape the geometry sequence consumes:
--   the accepted value on the right, the refusal reason on the left.
checkedBloodFloat ∷ Text → BloodFloatDomain → Float → Either Text Float
checkedBloodFloat field domain x =
    maybe (Right x) Left (checkBloodFloat field domain x)

-- | Narrow a Lua 'Double' to the 'Float' the decal stores.
--
--   'GHC.Float.double2Float' rather than @realToFrac@: the latter
--   routes through 'Rational' unless a rewrite rule fires, which does
--   not preserve NaN or the infinities — and this conversion is exactly
--   where a finite source turns into an infinity, so it must be the
--   faithful one. Every finite in-domain value narrows identically
--   either way, so no accepted call changes.
narrowBloodFloat ∷ Double → Float
narrowBloodFloat = double2Float

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
--
--   __The geometry domain (#2336).__ Every value the decal keeps as a
--   'Float' is checked at the value it would STORE, so a finite Lua
--   number such as @1e39@ that narrows to an infinity is refused along
--   with NaN and the infinities themselves:
--
--   * @gx@, @gy@, @offsetX@, @offsetY@, @rotation@ — any finite number.
--   * @scale@ — finite and above 0: a zero collapses the quad and a
--     negative one mirrors it.
--   * @opacity@ — finite and within the CLOSED @[0, 1]@, so a fully
--     transparent 0 and a fully opaque 1 both stay legal.
--
--   Out of the domain answers @nil, reason@ before the texture request
--   is built and before any decal or texture id is allocated, so a
--   refused call leaves the whole 'Blood.Types.BloodStore' untouched.
--   An ABSENT or unconvertible optional property still takes its
--   documented default exactly as before, and @wetness@ keeps its
--   pre-existing clamp rather than gaining a refusal — every accepted
--   call behaves as it always did.
--
--   Non-finite geometry cannot be tolerated and corrected downstream:
--   it reaches "World.Render.BloodQuads"' vertex and tint arithmetic as
--   NaN without anything raising. Blood is transient by design (#884),
--   so unlike a ground item's position this never reaches a save.
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
                    -- Unwrapped, not @realToFrac@'d: 'HsLua.Number'
                    -- already wraps this exact 'Double', and a
                    -- same-type @realToFrac@ is only the identity while
                    -- its rewrite rule fires — otherwise it routes a
                    -- NaN through 'Rational', which is precisely the
                    -- value the domain check below has to see (#2336).
                    Just (Lua.Number n) → Just n
                    _ → Nothing
            _ → pure Nothing
        -- An ABSENT or unconvertible property still takes its
        -- documented default, exactly as it always did (#2336). Only a
        -- property the caller really did name a number for is held to
        -- the domain, and it is held to it AFTER narrowing, because the
        -- decal keeps the narrowed value.
        getFloatProp ∷ Text → Float → BloodFloatDomain
                     → Lua.LuaE Lua.Exception (Either Text Float)
        getFloatProp field def domain = do
            mv ← getNumProp (Lua.Name (TE.encodeUtf8 field))
            pure $ case mv of
                Nothing → Right def
                Just d  → checkedBloodFloat field domain (narrowBloodFloat d)
        getIntProp ∷ Lua.Name → Int → Lua.LuaE Lua.Exception Int
        getIntProp key def = maybe def round ⊚ getNumProp key
    mStyleStr ← getStrProp "style"
    mFootStr  ← getStrProp "footprint"
    mAnisoStr ← getStrProp "anisotropy"
    mEdgeStr  ← getStrProp "edge"
    seedI     ← getIntProp "seed" 0
    surfaceZ  ← getIntProp "surfaceZ" 0
    eOffX     ← getFloatProp "offsetX"  0 BloodFinite
    eOffY     ← getFloatProp "offsetY"  0 BloodFinite
    eRot      ← getFloatProp "rotation" 0 BloodFinite
    eScl      ← getFloatProp "scale"    1 BloodPositive
    eOpac     ← getFloatProp "opacity"  1 BloodUnitInterval
    -- Wetness was already clamped to [0, 1] before #2336 and stays
    -- clamped: it is the one control with a documented correction, so
    -- refusing it now would change an accepted call.
    wet       ← maybe 1 narrowBloodFloat ⊚ getNumProp "wetness"
    mUnitN    ← getNumProp "sourceUnit"
    mPageStr  ← getStrProp "pageId"
    let fail_ msg = Lua.pushnil >> Lua.pushstring msg >> return 2
    case (xArg, yArg, windArg, sevArg) of
        (Just (Lua.Number xN), Just (Lua.Number yN), Just windBS, Just sevBS) → do
            let woundKind = TE.decodeUtf8Lenient windBS
                parsedSeverity  = parseSeverity (TE.decodeUtf8Lenient sevBS)
                parsedStyle     = maybe (Just (defaultStyleForWound woundKind))
                                        parseStyle mStyleStr
                parsedFootprint = maybe (Just FootprintMedium) parseFootprint mFootStr
                parsedAniso     = maybe (Just AnisotropyNone) parseAnisotropy mAnisoStr
                parsedEdge      = maybe (Just EdgeModerate) parseEdge mEdgeStr
                -- Every Float-backed geometry value, the positional
                -- coordinates included, checked at the value the decal
                -- would STORE (#2336). Sequenced through Either in one
                -- place so the refusal reported is the first named here
                -- and a control added later cannot be left unchecked.
                eGeometry = do
                    x  ← checkedBloodFloat "x" BloodFinite (narrowBloodFloat xN)
                    y  ← checkedBloodFloat "y" BloodFinite (narrowBloodFloat yN)
                    ox ← eOffX
                    oy ← eOffY
                    r  ← eRot
                    s  ← eScl
                    o  ← eOpac
                    pure (x, y, ox, oy, r, s, o)
            -- Geometry is decided in the SAME match as the buckets, so
            -- nothing nests: the first alternative is the only one that
            -- spawns, and a refusal falls through to the branch that
            -- names what was wrong. Geometry is reported ahead of an
            -- unrecognised bucket only because its alternative comes
            -- first; both refuse before anything is allocated.
            case (eGeometry, parsedSeverity, parsedStyle, parsedFootprint
                 , parsedAniso, parsedEdge) of
                ( Right (gx, gy, offX, offY, rot, scl, opac), Just severity
                 , Just style, Just footprint, Just aniso, Just edge) → do
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
                                now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
                                let mkSpec tid = BloodDecalSpec
                                        { bspTexture    = tid
                                        , bspPage       = pid
                                        , bspX          = gx
                                        , bspY          = gy
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
                (Left reason, _, _, _, _, _) → fail_ (TE.encodeUtf8 reason)
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
            mPage ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) → do
                    now   ← readIORef (wsGameTimeRef (toWorldSimCapability env))
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
        mPage ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
        case mPage of
            Nothing      → pure (0, [])
            Just (_, ws) → do
                now   ← readIORef (wsGameTimeRef (toWorldSimCapability env))
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
            mPage ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
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
        mPage ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
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
        mPage ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
        case mPage of
            Nothing      → return 0
            Just (_, ws) → btpCap . bstPool ⊚ readIORef (wsBloodStoreRef ws)
    Lua.pushinteger (fromIntegral cap)
    return 1

-- | blood.gpuStats() → { bindless, texSize, bloodHandles } — GPU-side
--   resource counts for the #788 world-teardown lifecycle probe:
--   total registered bindless textures ('btsHandleMap' size), total
--   'textureSizeRef' dimension-cache entries, and the ACTIVE page's live
--   blood handle-map size (0 with no active page or no bindless system).
--   The first two are engine-wide (not blood-only), so the probe reads
--   DELTAS around a controlled blood spawn / page teardown rather than
--   absolute counts; a fixed leak would leave the post-teardown delta
--   above baseline. All three are 0 headless (nothing uploads).
bloodGpuStatsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodGpuStatsFn env = do
    (bindless, texSize, bloodHandles) ← Lua.liftIO $ do
        mSys     ← readIORef (rvTextureSystemRef (toRenderViewCapability env))
        texSizes ← readIORef (rvTextureSizeRef (toRenderViewCapability env))
        mPage    ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
        bh ← case mPage of
            Nothing      → pure (0 ∷ Int)
            Just (_, ws) → HM.size ⊚ readIORef (wsBloodTextureHandlesRef ws)
        pure (maybe 0 (Map.size . btsHandleMap) mSys, HM.size texSizes, bh)
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
    putI "bindless"     bindless
    putI "texSize"      texSize
    putI "bloodHandles" bloodHandles
    return 1

-- | Which set of texture handles 'bloodGpuHandlesFn' was asked about.
data HandleSelection
    = ActivePageBlood
      -- ^ No argument: the active page's own blood handle map.
    | ExplicitHandles [Int]
      -- ^ A caller-supplied array of raw 'TextureHandle' values.

-- | blood.gpuHandles([handles]) → array of
--   @{ handle, bindless, texSize [, id] }@ tables, or @nil@ when the
--   argument is present but is not a dense array of integers.
--
--   Purely OBSERVATIONAL (issue #1585): it reads membership in the
--   bindless handle map and the texture-size cache and mutates neither
--   those nor any blood state. Upload, FIFO pooling, disposal and
--   teardown behaviour are untouched by this verb existing.
--
--   With NO argument it reports the ACTIVE page's live blood handle map
--   ('World.State.Types.wsBloodTextureHandlesRef'), ascending by
--   'BloodTextureId', each row additionally carrying that @id@ — the
--   blood-OWNED GPU identities, which 'bloodGpuStatsFn' only counts.
--
--   With a dense array of integer texture handles it reports exactly
--   those, in the order given and WITHOUT an @id@ — an element that is
--   not a Lua number with an integer value, a numeric STRING included,
--   rejects the whole call. The handles need not
--   belong to any live page, which is the whole point. #788's lifecycle
--   probe captures a page's blood handles BEFORE a teardown and asks
--   about them after, when that page is gone and no live map could
--   answer. Handles are monotonically allocated and never reused
--   ('Engine.Asset.Manager.generateTextureHandle'), so a captured handle
--   still resident afterwards is a leak of THAT resource specifically —
--   unlike the engine-wide totals 'bloodGpuStatsFn' reports, which a
--   replacement session's own unrelated uploads also move.
--
--   @bindless@ is membership in 'btsHandleMap', @texSize@ membership in
--   'rvTextureSizeRef'. 'World.Render.BloodQuads.disposeBloodRecord'
--   drops those two SEPARATELY, so a caller must check BOTH to catch a
--   partial leak. Both read false with no bindless system (headless).
bloodGpuHandlesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodGpuHandlesFn env = do
    argTy ← Lua.ltype 1
    mSel ← if argTy ≡ Lua.TypeNone ∨ argTy ≡ Lua.TypeNil
              then pure (Just ActivePageBlood)
              else if argTy ≢ Lua.TypeTable
                     then pure Nothing
                     else do
                       dense ← isDenseArray 1
                       if not dense then pure Nothing
                                    else fmap ExplicitHandles ⊚ readHandleArray 1
    case mSel of
        Nothing  → Lua.pushnil >> return 1
        Just sel → do
            rows ← Lua.liftIO (gpuHandleRows env sel)
            Lua.newtable
            forM_ (zip [1 ∷ Int ..] rows) $ \(i, r) → do
                pushHandleRow r
                Lua.rawseti (-2) (fromIntegral i)
            return 1

-- | Read a dense Lua array of integers at @idx@, rejecting (Nothing)
--   any element that is not an integer.
--
--   The element's Lua TYPE is checked before conversion: 'Lua.tointeger'
--   coerces a convertible STRING ("47") as readily as a number, and a
--   texture handle spelled as a string is a caller mistake this verb
--   must report, not silently accept. A non-integral number (47.5) has
--   no integer representation and is rejected by 'Lua.tointeger' itself.
readHandleArray ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe [Int])
readHandleArray idx = do
    n ← Lua.rawlen idx
    let go i acc
          | i > fromIntegral n = pure (Just (reverse acc))
          | otherwise = do
              _  ← Lua.rawgeti idx i
              ty ← Lua.ltype (-1)
              mv ← if ty ≡ Lua.TypeNumber then Lua.tointeger (-1)
                                          else pure Nothing
              Lua.pop 1
              case mv of
                  Just v  → go (i + 1) (fromIntegral v : acc)
                  Nothing → pure Nothing
    go 1 []

-- | One @(id, handle, in-bindless, in-texture-size)@ row per selected
--   handle. Both registries are read ONCE for the whole batch so every
--   row in a single call describes the same observation.
gpuHandleRows ∷ EngineEnv → HandleSelection → IO [(Maybe Word32, Int, Bool, Bool)]
gpuHandleRows env sel = do
    mSys     ← readIORef (rvTextureSystemRef (toRenderViewCapability env))
    texSizes ← readIORef (rvTextureSizeRef (toRenderViewCapability env))
    let row mId h@(TextureHandle n) =
            ( mId, n
            , maybe False (Map.member h . btsHandleMap) mSys
            , HM.member h texSizes )
    case sel of
        ExplicitHandles hs → pure [ row Nothing (TextureHandle h) | h ← hs ]
        ActivePageBlood    → do
            mPage ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
            case mPage of
                Nothing      → pure []
                Just (_, ws) → do
                    known ← readIORef (wsBloodTextureHandlesRef ws)
                    pure [ row (Just (unBloodTextureId tid)) h
                         | (tid, (h, _)) ← sortOn fst (HM.toList known) ]

-- | Push one 'gpuHandleRows' row. @id@ is present only for the
--   active-page form — an explicitly named handle has no live
--   'BloodTextureId' to report.
pushHandleRow ∷ (Maybe Word32, Int, Bool, Bool) → Lua.LuaE Lua.Exception ()
pushHandleRow (mId, h, inBindless, inTexSize) = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putB k v = Lua.pushboolean v >> Lua.setfield (-2) k
    forM_ mId (putI "id")
    putI "handle" h
    putB "bindless" inBindless
    putB "texSize"  inTexSize

-- | blood.getRenderQuads([pageId]) → array of render-record tables
--   (issue #606: the resolved per-decal data a world-space quad needs —
--   see 'Blood.Render.bloodRenderRecords'), on the given page or the
--   active world if omitted. A decal whose texture reference has been
--   evicted never appears here (it's already gone from the decal list,
--   dropped by "Blood.Types"'s texture-eviction cascade, and
--   'Blood.Render.bloodRenderRecords' re-checks the pool defensively on
--   top of that). Each record's tint/alpha reflect the decal's CURRENT
--   age at call time, aged inside "Blood.Render", so a decal spawned
--   with a low @wetness@ reports a darker, fainter tint than one
--   spawned fresh — headless-observable aging, without a GPU.
bloodGetRenderQuadsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodGetRenderQuadsFn env = do
    mPageArg ← Lua.tostring 1
    let mPageStr = TE.decodeUtf8Lenient ⊚ mPageArg
    recs ← Lua.liftIO $ do
        mTarget ← resolveBloodPage env mPageStr
        case mTarget of
            Nothing         → pure []
            Just (pid, ws)  → do
                now   ← readIORef (wsGameTimeRef (toWorldSimCapability env))
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
        mPage ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
        case mPage of
            Nothing      → return False
            Just (_, ws) → do
                atomicModifyIORef' (wsBloodStoreRef ws) $ \store →
                    (clearBlood store, ())
                return True
    Lua.pushboolean ok
    return 1

-- | blood.getTrailState(uid) → { pendingVolume, distSinceMark,
--     lastMarkAt, clusterLayers, clusterAtBound [, clusterX, clusterY] }
--   | nil. Headless introspection (issue #882 requirement 7, extended by
--   issue #883 requirement 9) for the ongoing-bleeding emitter's
--   per-unit accumulator ('Unit.Types.Trail.TrailState', written by
--   'Combat.Wounds.Tick' and consumed by 'Unit.Thread.Movement' /
--   "Blood.Trail" / "Blood.Pool"). nil for a missing unit OR a unit with
--   no active accumulator (never bled externally, or cleared on
--   death/despawn/zero external bleed — see
--   'Unit.Types.Instance.uiTrailState').
--   @lastMarkAt@ is the absolute game-time seconds of the last placed
--   mark or pool layer (or of the accumulator's creation, before any
--   has fired) — both halves share the one cadence clock.
--   @clusterX@/@clusterY@ are the current pool cluster's anchor, ABSENT
--   (nil) until the movement consumer has anchored one;
--   @clusterLayers@ is how many layers this cluster has already spawned
--   and @clusterAtBound@ whether it has spent its whole
--   'Blood.Pool.ptMaxLayers' budget (after which continued bleeding in
--   place adds no further marks).
bloodGetTrailStateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
bloodGetTrailStateFn env = do
    idArg ← Lua.tointeger 1
    mTs ← case idArg of
        Nothing → return Nothing
        Just n  → Lua.liftIO $ do
            um ← readIORef (unitManagerRef env)
            pure $ HM.lookup (UnitId (fromIntegral n)) (umInstances um)
                     ⌦ uiTrailState
    case mTs of
        Just ts → do
            Lua.newtable
            let putN k v = Lua.pushnumber (Lua.Number (realToFrac v)) >> Lua.setfield (-2) k
            putN "pendingVolume" (tsPendingVolume ts)
            putN "distSinceMark" (tsDistSinceMark ts)
            putN "lastMarkAt"    (tsLastMarkAt ts)
            Lua.pushinteger (fromIntegral (tsClusterLayers ts))
            Lua.setfield (-2) "clusterLayers"
            Lua.pushboolean (poolAtBound defaultPoolThresholds ts)
            Lua.setfield (-2) "clusterAtBound"
            case tsClusterAnchor ts of
                Nothing → pure ()
                Just (ax, ay) → do
                    putN "clusterX" ax
                    putN "clusterY" ay
            return 1
        Nothing → Lua.pushnil >> return 1

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
--   quad ('Blood.Render.BloodRenderRecord'). @tintR/G/B@ and @alpha@
--   already fold in aging inside "Blood.Render": a fresher decal
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
