{-# LANGUAGE Strict #-}
-- | Lua API for the UNPLACED-piece art catalogue (#1842).
--
--   @scripts/structures.lua@ and @scripts/wire.lua@ already read every
--   pack YAML and load every texture; this is how they hand the result
--   to the engine, so the world render thread — which cannot call into
--   Lua — can answer what a construction DESIGNATION would be built
--   with. The registration is deliberately shaped like
--   @structure.registerWallFamily@ (#1712): one call per pack, refused
--   whole if it is short or malformed, idempotent on a repeat, and
--   keyed by something a save\/load cannot invalidate.
--
--   Nothing here places a piece, interns a path into the saved texture
--   palette, or touches any persisted state — see "Structure.ArtCatalog".
--
--   The wire half exists so there is ONE autotile rule rather than two:
--   @structure.wireShape@ and @structure.wireNeighbors@ are what
--   @scripts/wire.lua@ now derives a placement from, and what the render
--   pass derives a designated run from. They differ in exactly one
--   argument — whether wire DESIGNATIONS count as connecting — and in
--   nothing else.
module Engine.Scripting.Lua.API.StructureArt
    ( structureRegisterPackArtFn
    , structurePackKindBuildableFn
    , structurePackBuildCostFn
    , structureResolvePieceArtFn
    , structureWireShapeFn
    , structureWireNeighborsFn
    ) where

import UPrelude
import Control.Exception (SomeException, try)
import Data.IORef (readIORef, atomicModifyIORef')
import qualified Codec.Picture as JP
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified HsLua as Lua

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Log (LogCategory(..), logInfo, logWarn)
import Engine.Core.State (EngineEnv, loggerRef)
import Engine.Scripting.Lua.API.Structure (resolveStructurePage)
import Engine.Scripting.Lua.Util (isDenseArray)
import Structure.ArtCatalog
import Structure.Facing (WallEdge(..), wallCapsFromCode)
import Structure.Wire
    (WireNeighbors(..), wireShapeFor, wireShapeName, wireShapeFromName)
import World.Construct.Art (wallCapsAt, wireNeighborsAt)
import World.Types
    ( WorldState, pageWrapWorldSize, wsConstructDesignationsRef
    , wsStructureStageRef, wsTilesRef )

-- | @structure.registerPackArt(spec) → bool@ — declare ONE structure
--   pack's per-kind art.
--
--   @spec@ is a table:
--
--   > { pack  = "dungeon_1",
--   >   kinds = { { kind = "floor", buildable = true }, ... },
--   >   art   = { { kind = "floor", texture = p, texHandle = h,
--   >               facemap = q, faceHandle = g },
--   >             { kind = "wall", edge = "ne", caps = "00", ... },
--   >             { kind = "wire", shape = "cross", ... } } }
--
--   @construction@ (#2488) is OPTIONAL and declares one ordered frame
--   sequence per authored APPEARANCE — coarser than @art@, because a
--   wall's four cap facemaps share one sprite:
--
--   > construction = { { kind = "wall", edge = "ne", variant = nil,
--   >                    texture = <the appearance's static sprite>,
--   >                    texHandle = h,
--   >                    frames = { { texture = p1, texHandle = h1 }, … } } }
--
--   @variant@ names a pack variant and defaults to the pack's own
--   default art. A sequence is keyed to EXACTLY that appearance: a
--   variant's cannot inherit or substitute the default's, and an
--   appearance with no entry resolves none. The engine refuses the whole
--   pack — the same all-or-nothing rule the static art is under — for an
--   empty list, a duplicate or escaping frame path, an unloaded handle,
--   a wall family whose declared directions run to different lengths, a
--   last frame whose pixel dimensions differ from the static sprite's,
--   or a conflicting repeat.
--
--   @kinds@ is the EXPLICIT declared-kind inventory the completeness
--   check runs against: declaring a kind obliges the payload to carry
--   every one of its art slots (one each for @floor@\/@ceiling@\/@post@,
--   sixteen edge×cap facemaps for @wall@, sixteen connection variants
--   for @wire@). @buildable@ is MANDATORY per kind and says whether that
--   kind's @build:@ entry carries both @build_work@ and @materials@ — a
--   separate answer from whether its art resolves, so it is required
--   rather than defaulted.
--
--   Returns false and registers NOTHING on any fault, logging exactly
--   one warning naming the pack, the kind and the offending asset role
--   (and its path, when the fault has one). An identical repeat returns
--   true and changes nothing; a CONFLICTING repeat is refused and leaves
--   the stored pack exactly as it was.
structureRegisterPackArtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureRegisterPackArtFn env = do
    eReg ← readRegistration
    ok ← case eReg of
        Left fault → warn fault ≫ pure False
        Right reg0 → do
            -- Requirement 6's dimension check needs PIXELS, and the
            -- catalogue is pure. Measure here — the images are on disk
            -- and the Lua thread can read them — rather than waiting for
            -- an upload that a headless session never performs and that
            -- lands long after this call. Only the paths a declared
            -- sequence actually names are touched, so a pack with no
            -- sequences reads nothing.
            sizes ← Lua.liftIO (measureSequences (parFrames reg0))
            let reg = reg0 { parSizes = sizes }
            (outcome, cat) ← Lua.liftIO $
                atomicModifyIORef' (rhStructureArtCatalogRef
                                      (toRenderHandoffCapability env))
                                   (\c → let (c', o) = registerPackArt reg c
                                         in (c', (o, c')))
            case outcome of
                -- Requirement 8's diagnostic, on a FRESH registration
                -- only: once per (pack, appearance), never per frame and
                -- never per drawn candidate. An idempotent repeat is
                -- silent, so a pack cannot report itself twice.
                ArtRegistered → reportUndeclared cat (parPack reg) ≫ pure True
                ArtAlreadyRegistered     → pure True
                ArtRegistrationRefused f → warn f ≫ pure False
    Lua.pushboolean ok
    return 1
  where
    warn f = do
        logger ← Lua.liftIO $ readIORef (loggerRef env)
        logWarn logger CatLua (artFaultMessage f)

    reportUndeclared cat pack = case HM.lookup pack (sacPacks cat) of
        Nothing → pure ()
        Just stored → do
            logger ← Lua.liftIO $ readIORef (loggerRef env)
            forM_ (undeclaredConstructionAppearances stored) $ \ak →
                logInfo logger CatLua (missingConstructionMessage pack ak)

    -- A payload that cannot even be READ still reports through the same
    -- one-warning channel, and still names as much of pack / kind /
    -- asset role as it managed to parse — a caller whose entry lost its
    -- handle needs to be told WHICH entry, not just that something was
    -- malformed.
    fault pack mKind role reason = ArtFault
        { afPack = pack, afKind = mKind, afRole = role
        , afPath = Nothing, afReason = reason }

    readRegistration ∷ Lua.LuaE Lua.Exception
                         (Either ArtFault PackArtRegistration)
    readRegistration = do
        isT ← Lua.istable 1
        if not isT
          then pure ∘ Left $ fault "<unnamed>" Nothing "registration payload"
                                   "the registration argument is not a table"
          else do
            mPack ← fieldString 1 "pack"
            case mPack of
                Nothing → pure ∘ Left $
                    fault "<unnamed>" Nothing "pack name"
                          "the payload has no `pack` string"
                Just pack → do
                    eKinds ← arrayField pack "kinds" "declared kinds"
                                        (readKind pack)
                    eArt   ← arrayField pack "art" "art entries" (readArt pack)
                    -- #2488: OPTIONAL, so an ABSENT `construction` is an
                    -- empty declaration rather than a malformed payload
                    -- (every shipped pack is in exactly that state).
                    -- A PRESENT one must still be a dense array of
                    -- tables, like `art`.
                    eFrames ← optionalArrayField pack "construction"
                                  "construction frames" (readSequence pack)
                    pure (PackArtRegistration pack <$> eKinds <*> eArt
                            <*> eFrames <*> pure HM.empty)

    -- Push spec[name], read it as a dense array, pop. A non-table field
    -- is malformed rather than empty: an absent `art` list is not a pack
    -- with no art, it is a payload that forgot to send one.
    arrayField ∷ Text → Lua.Name → Text
               → (Int → Lua.LuaE Lua.Exception (Either ArtFault α))
               → Lua.LuaE Lua.Exception (Either ArtFault [α])
    arrayField pack name role readOne = do
        ty ← Lua.getfield 1 name
        r  ← if ty ≢ Lua.TypeTable
                then pure ∘ Left $
                    fault pack Nothing role
                          ("the payload's `" <> nameText name
                             <> "` is not an array")
                else readArray pack role readOne
        Lua.pop 1
        pure r

    -- 'arrayField' for a key a payload may legitimately omit: absent is
    -- an empty declaration, present-but-not-a-table is still malformed.
    optionalArrayField ∷ Text → Lua.Name → Text
                       → (Int → Lua.LuaE Lua.Exception (Either ArtFault α))
                       → Lua.LuaE Lua.Exception (Either ArtFault [α])
    optionalArrayField pack name role readOne = do
        ty ← Lua.getfield 1 name
        Lua.pop 1
        if ty ≡ Lua.TypeNil
            then pure (Right [])
            else arrayField pack name role readOne

    -- Every element must be a table AND parse, or the whole array is a
    -- fault: a payload assembled from the entries that happened to be
    -- well-formed is exactly the partial pack the all-or-nothing rule
    -- exists to refuse.
    readArray ∷ Text → Text
              → (Int → Lua.LuaE Lua.Exception (Either ArtFault α))
              → Lua.LuaE Lua.Exception (Either ArtFault [α])
    readArray pack role readOne = do
        -- `rawlen` reports a BORDER, not a count: a sparse table
        -- (`{[1]=a, [3]=b}`) can answer 1, and walking to that border
        -- would silently drop everything past the hole — a registration
        -- accepted as complete while missing a declared kind or an art
        -- slot, which is precisely the partial pack the all-or-nothing
        -- rule exists to refuse. Reject the whole payload instead.
        dense ← isDenseArray (-1)
        if not dense
          then pure ∘ Left $
              fault pack Nothing role
                    "the array is sparse (it has a gap in its indices), so \
                    \some entries would be silently dropped"
          else do
            n ← Lua.rawlen (-1)
            go 1 (fromIntegral n) []
      where
        go i n acc
            | i > n = pure (Right (reverse acc))
            | otherwise = do
                ty ← Lua.rawgeti (-1) (fromIntegral i)
                e  ← if ty ≢ Lua.TypeTable
                        then pure ∘ Left $
                            fault pack Nothing (role <> " " <> tshow i)
                                  "the entry is not a table"
                        else readOne i
                Lua.pop 1
                case e of
                    Left f  → pure (Left f)
                    Right v → go (i + 1) n (v : acc)

    -- The pack name is threaded in rather than defaulted: by the time a
    -- kind entry is read the payload has already NAMED its pack, and a
    -- warning that says `pack '<unnamed>'` for a named payload fails the
    -- requirement it exists to satisfy.
    readKind ∷ Text → Int
             → Lua.LuaE Lua.Exception
                   (Either ArtFault (PieceKind, Bool, Maybe BuildCost))
    readKind pack i = do
        mKind ← fieldString (-1) "kind"
        bTy   ← Lua.getfield (-1) "buildable"
        b     ← Lua.toboolean (-1)
        Lua.pop 1
        wTy   ← Lua.getfield (-1) "build_work"
        mWork ← Lua.tonumber (-1)
        Lua.pop 1
        eMats ← readMaterials pack i
        pure $ case mKind ⌦ pieceKindFromText of
            Nothing → Left $ fault pack Nothing
                ("declared kinds " <> tshow i)
                "the entry names no recognised piece kind"
            Just kind
                -- Absent or non-boolean `buildable` is malformed, never
                -- a silent default: art and buildability are independent
                -- answers and a caller must state both.
                | bTy ≢ Lua.TypeBoolean → Left $ fault pack (Just kind)
                    ("buildable (declared kinds " <> tshow i <> ")")
                    "the entry has no `buildable` boolean"
                -- #1844: the COST is optional and is read only when the
                -- payload states one. It is a separate answer from
                -- `buildable`, which keeps its own mandatory meaning: a
                -- registration that omits the numbers is not malformed,
                -- it is one the engine cannot charge a job against.
                --
                -- An EMPTY `materials` table is a real, authored cost of
                -- nothing — the pack YAML's own buildability rule asks
                -- only that the field EXIST, and a receipt of no
                -- materials is a valid paid state. So the absent case
                -- ('Nothing') and the empty one ('Just []') are kept
                -- apart here rather than both collapsing to "no cost",
                -- which would make a zero-material kind permanently
                -- resolver-invalid and impossible to build.
                | otherwise → case (wTy, mWork, eMats) of
                    (Lua.TypeNumber, Just (Lua.Number w), Right (Just mats)) →
                        Right (kind, b, Just (mkBuildCost (realToFrac w) mats))
                    _ → Right (kind, b, Nothing)

    -- `materials` is a NAME → COUNT map, so it is walked with `next`
    -- rather than as an array. A non-string key, a non-integer count and
    -- a non-positive count are each a REFUSAL of the cost: every one of
    -- them would silently change what a job costs, and the cost is what
    -- a receipt promises was removed. An absent table is not a refusal —
    -- it is a registration that states no cost at all.
    readMaterials ∷ Text → Int
                  → Lua.LuaE Lua.Exception
                        (Either ArtFault (Maybe [(Text, Int)]))
    readMaterials pack i = do
        ty ← Lua.getfield (-1) "materials"
        r ← if ty ≢ Lua.TypeTable
              -- No table at all: the payload states no cost. Distinct
              -- from a PRESENT but empty one, which is a cost of
              -- nothing (see 'readKind').
              then pure (Right Nothing)
              else do
                  Lua.pushnil
                  go []
        Lua.pop 1
        pure r
      where
        matFault = fault pack Nothing
            ("materials (declared kinds " <> tshow i <> ")")
        go acc = do
            more ← Lua.next (-2)
            if not more then pure (Right (Just (reverse acc))) else do
                mName ← Lua.tostring (-2)
                nTy   ← Lua.ltype (-1)
                mN    ← Lua.tointeger (-1)
                Lua.pop 1
                case (mName, nTy, mN) of
                    (Just nameBS, Lua.TypeNumber, Just n)
                        | n > 0 → go ((TE.decodeUtf8Lenient nameBS,
                                       fromIntegral n) : acc)
                    _ → do
                        -- Abandon the traversal cleanly: the key is
                        -- still on the stack and must come off.
                        Lua.pop 1
                        pure ∘ Left $ matFault
                            "a material entry is not a positive integer \
                            \count keyed by an item def name"

    readArt ∷ Text → Int
            → Lua.LuaE Lua.Exception (Either ArtFault (ArtKey, PieceArt))
    readArt pack i = do
        mKind  ← fieldString (-1) "kind"
        mEdge  ← fieldString (-1) "edge"
        mCaps  ← fieldString (-1) "caps"
        mShape ← fieldString (-1) "shape"
        mTex   ← fieldString (-1) "texture"
        mFace  ← fieldString (-1) "facemap"
        mTexH  ← fieldHandle (-1) "texHandle"
        mFaceH ← fieldHandle (-1) "faceHandle"
        pure $ case mKind ⌦ pieceKindFromText of
            Nothing → Left $ entryFault Nothing "" 
                "the entry names no recognised piece kind"
            Just kind → case artKeyFor kind mEdge mCaps mShape of
                Nothing → Left $ entryFault (Just kind) ""
                    "the entry's edge/caps/shape selectors do not name one \
                    \of this kind's art slots"
                Just key →
                    let role = artKeyRole key
                        need ∷ Text → Maybe α → Either ArtFault α
                        need what = maybe
                            (Left (entryFault (Just kind) (role <> " " <> what)
                                     ("the entry has no `" <> what <> "`")))
                            Right
                    in do tex   ← need "texture"    mTex
                          texH  ← need "texHandle"  mTexH
                          face  ← need "facemap"    mFace
                          faceH ← need "faceHandle" mFaceH
                          pure (key, PieceArt (ArtAsset tex texH)
                                              (ArtAsset face faceH))
      where
        entryFault mKind role =
            fault pack mKind
                  (if role ≡ "" then "art entry " <> tshow i
                                else role <> " (art entry " <> tshow i <> ")")

    -- #2488: one appearance's construction sequence. The selectors are
    -- the APPEARANCE's, so a wall entry names its edge and NOT a cap
    -- code — the four caps of one edge share the sprite the sequence
    -- builds toward, and a payload that thinks otherwise is mis-shaped.
    readSequence ∷ Text → Int
                 → Lua.LuaE Lua.Exception
                       (Either ArtFault (AppearanceKey, ConstructionSequence))
    readSequence pack i = do
        mKind    ← fieldString (-1) "kind"
        mEdge    ← fieldString (-1) "edge"
        mCaps    ← fieldString (-1) "caps"
        mShape   ← fieldString (-1) "shape"
        mVariant ← fieldString (-1) "variant"
        mTex     ← fieldString (-1) "texture"
        mTexH    ← fieldHandle (-1) "texHandle"
        eFrames  ← readFrames pack i
        pure $ case mKind ⌦ pieceKindFromText of
            Nothing → Left $ seqFault Nothing ""
                "the entry names no recognised piece kind"
            Just kind → case appearanceSlotFor kind mEdge mCaps mShape of
                Nothing → Left $ seqFault (Just kind) ""
                    "the entry's edge/shape selectors do not name one of \
                    \this kind's appearances"
                Just aslot →
                    let ak   = AppearanceKey mVariant aslot
                        role = appearanceKeyRole ak <> " construction"
                        need ∷ Text → Maybe α → Either ArtFault α
                        need what = maybe
                            (Left (seqFault (Just kind) (role <> " " <> what)
                                     ("the entry has no `" <> what <> "`")))
                            Right
                    in do tex    ← need "texture"   mTex
                          texH   ← need "texHandle" mTexH
                          frames ← eFrames
                          pure ( ak
                               , ConstructionSequence
                                   { csStatic = ArtAsset tex texH
                                   , csFrames = V.fromList frames } )
      where
        seqFault mKind role =
            fault pack mKind
                  (if role ≡ "" then "construction frames " <> tshow i
                                else role <> " (construction frames "
                                       <> tshow i <> ")")

    -- The ORDERED frame list. Dense-array-checked like every other array
    -- in this payload: a sparse `frames` would silently drop stages, and
    -- a sequence missing a stage is not a shorter sequence, it is wrong.
    readFrames ∷ Text → Int
               → Lua.LuaE Lua.Exception (Either ArtFault [ArtAsset])
    readFrames pack i = do
        ty ← Lua.getfield (-1) "frames"
        r ← if ty ≢ Lua.TypeTable
              then pure ∘ Left $ fault pack Nothing
                       ("construction frames " <> tshow i)
                       "the entry's `frames` is not an array"
              else readArray pack ("construction frames " <> tshow i)
                             (readFrame pack i)
        Lua.pop 1
        pure r

    readFrame ∷ Text → Int → Int
              → Lua.LuaE Lua.Exception (Either ArtFault ArtAsset)
    readFrame pack i j = do
        mTex  ← fieldString (-1) "texture"
        mTexH ← fieldHandle (-1) "texHandle"
        pure $ case (mTex, mTexH) of
            (Just tex, Just h) → Right (ArtAsset tex h)
            _ → Left $ fault pack Nothing
                    ("construction frames " <> tshow i <> " frame " <> tshow j)
                    "the frame has no `texture` string and `texHandle` number"

    -- The APPEARANCE a construction entry's selectors name.
    appearanceSlotFor kind mEdge mCaps mShape = do
        guard (isNothing mCaps)
        case kind of
            KFloor   → ApFloor   ⚟ guard (noneOf [mEdge, mShape])
            KCeiling → ApCeiling ⚟ guard (noneOf [mEdge, mShape])
            KPost    → ApPost    ⚟ guard (noneOf [mEdge, mShape])
            KWall    → guard (isNothing mShape)
                         ≫ (ApWall <$> (mEdge ⌦ wallEdgeFromText))
            KWire    → guard (isNothing mEdge)
                         ≫ (ApWire <$> (mShape ⌦ wireShapeFromName))
      where noneOf = all isNothing

    -- A wall entry MUST name both its edge and its cap code, a wire
    -- entry MUST name its shape, and the three simple kinds must name
    -- none of them: an entry carrying a selector its kind has no use for
    -- is a mis-shaped payload, not a harmless extra.
    artKeyFor kind mEdge mCaps mShape = case kind of
        KFloor   → simple AkFloor
        KCeiling → simple AkCeiling
        KPost    → simple AkPost
        KWall    → do
            guard (isNothing mShape)
            e ← mEdge ⌦ wallEdgeFromText
            c ← mCaps ⌦ wallCapsFromCode
            pure (AkWall e c)
        KWire    → do
            guard (isNothing mEdge ∧ isNothing mCaps)
            AkWire <$> (mShape ⌦ wireShapeFromName)
      where
        simple k = k ⚟ guard (isNothing mEdge ∧ isNothing mCaps
                                ∧ isNothing mShape)

-- | Measure every image a declared sequence's dimension check needs:
--   the appearance's static sprite and the sequence's LAST frame.
--
--   Reads the files directly rather than consulting
--   'Engine.Core.Capability.RenderView.rvTextureSizeRef'. That cache is
--   populated by a completed GPU upload, which has not happened when a
--   pack registers on the first Lua tick and never happens at all in a
--   headless session — so a check written against it would be silently
--   vacuous exactly where it is asserted. A path that cannot be read is
--   simply left out of the map, and 'registerPackArt' refuses the pack
--   for it by name.
measureSequences ∷ [(AppearanceKey, ConstructionSequence)]
                 → IO (HM.HashMap Text (Int, Int))
measureSequences seqs = HM.fromList ∘ catMaybes <$> mapM measure paths
  where
    paths = ordNub
        [ path
        | (_, cs) ← seqs
        , path ← aaPath (csStatic cs)
                   : [ aaPath (V.last fs) | let fs = csFrames cs
                                          , not (V.null fs) ] ]
    measure path = do
        r ← try (JP.readImage (T.unpack path))
        pure $ case r ∷ Either SomeException (Either String JP.DynamicImage) of
            Right (Right img) → Just (path, ( JP.dynamicMap JP.imageWidth img
                                            , JP.dynamicMap JP.imageHeight img ))
            _                 → Nothing
    ordNub = foldr (\x xs → x : filter (≢ x) xs) []

-- | @structure.isPackKindBuildable(pack, kind) → bool@ — does this
--   pack's kind carry complete @build:@ metadata? Deliberately
--   independent of whether its art resolves (#1842 requirement 5).
structurePackKindBuildableFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structurePackKindBuildableFn env = do
    mPack ← argString 1
    mKind ← argString 2
    ok ← case (mPack, mKind) of
        (Just p, Just k) → do
            cat ← Lua.liftIO $ readIORef (rhStructureArtCatalogRef
                                            (toRenderHandoffCapability env))
            pure (packKindBuildable cat p k)
        _ → pure False
    Lua.pushboolean ok
    return 1

-- | @structure.packBuildCost(pack, kind) →
--   { build_work = , materials = { \<item\> = \<count\> } } | nil@ — the
--   REGISTERED build cost of that pack's kind (#1844).
--
--   The engine's own authority for what a structure job costs: it is
--   what @construction.payMaterials@ charges and what a legacy paid
--   designation's receipt is reconstructed from at load. Exposed so the
--   build AI plans its material fetch against the SAME numbers rather
--   than re-reading the pack YAML into a second, drifting copy.
--
--   nil for an unregistered pack, an undeclared kind, or a kind whose
--   @build:@ entry is incomplete — deliberately independent of whether
--   the kind's ART resolves.
structurePackBuildCostFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structurePackBuildCostFn env = do
    mPack ← argString 1
    mKind ← argString 2
    mCost ← case (mPack, mKind) of
        (Just p, Just k) → do
            cat ← Lua.liftIO $ readIORef (rhStructureArtCatalogRef
                                            (toRenderHandoffCapability env))
            pure (packKindBuild cat p k)
        _ → pure Nothing
    case mCost of
        Nothing → Lua.pushnil ≫ return 1
        Just cost → do
            Lua.newtable
            Lua.pushnumber (Lua.Number (realToFrac (bcWork cost)))
            Lua.setfield (-2) "build_work"
            Lua.newtable
            forM_ (bcMaterials cost) $ \(name, n) → do
                Lua.pushinteger (fromIntegral n)
                Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 name))
            Lua.setfield (-2) "materials"
            return 1

-- | @structure.resolvePieceArt(pack, kind, edge, gx, gy[, page]) →
--   { texture=, texHandle=, facemap=, faceHandle= } | nil@ — the exact
--   pair @structure.place@ would be called with for an UNPLACED piece of
--   that descriptor at that tile.
--
--   @edge@ may be nil (and is ignored for every kind but @wall@). The
--   tile is what supplies the context the two variant-carrying kinds
--   need — a wall's cap state from this tile's own posts, a wire's
--   connection shape from its four neighbours INCLUDING designated ones,
--   which is the render pass's question rather than the placer's.
--
--   Returns nil for an unregistered pack, an undeclared kind, a pack
--   whose art terminally failed to load, and an unknown page or missing
--   world. Never a guess.
structureResolvePieceArtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureResolvePieceArtFn env = do
    mPack ← argString 1
    mKind ← argString 2
    mEdge ← argString 3
    gxA   ← Lua.tointeger 4
    gyA   ← Lua.tointeger 5
    mPage ← argString 6
    mArt ← case (mPack, mKind, gxA, gyA) of
        (Just pack, Just kind, Just gx, Just gy) → Lua.liftIO $ do
            cat ← readIORef (rhStructureArtCatalogRef
                               (toRenderHandoffCapability env))
            mWs ← fmap snd <$> resolveStructurePage env mPage
            case mWs of
                Nothing → pure Nothing
                Just ws → do
                    ctx ← pieceContext ws (fromIntegral gx) (fromIntegral gy)
                                       mEdge
                    pure (resolveUnplacedArt cat pack kind mEdge ctx)
        _ → pure Nothing
    case mArt of
        Nothing  → Lua.pushnil ≫ return 1
        Just art → do
            Lua.newtable
            pushAsset "texture" "texHandle" (paTexture art)
            pushAsset "facemap" "faceHandle" (paFacemap art)
            return 1
  where
    pushAsset pathKey handleKey a = do
        Lua.pushstring (TE.encodeUtf8 (aaPath a))
        Lua.setfield (-2) pathKey
        let TextureHandle h = aaHandle a
        Lua.pushinteger (fromIntegral h)
        Lua.setfield (-2) handleKey

-- | Both variant-carrying kinds' world context for one tile, read in
--   one pass so a caller cannot supply half of it.
pieceContext ∷ WorldState → Int → Int → Maybe Text → IO PieceArtContext
pieceContext ws gx gy mEdge = do
    worldSize ← pageWrapWorldSize ws
    td        ← readIORef (wsTilesRef ws)
    stage     ← readIORef (wsStructureStageRef ws)
    designs   ← readIORef (wsConstructDesignationsRef ws)
    let edge  = fromMaybe WallNE (mEdge ⌦ wallEdgeFromText)
        neigh = wireNeighborsAt worldSize td stage (Just designs) gx gy
    pure PieceArtContext
        { pacWallCaps  = wallCapsAt worldSize td stage edge gx gy
        , pacWireShape = wireShapeFor neigh }

-- | @structure.wireShape(n, e, s, w) → string@ — the connection variant
--   a tile with those four cardinal neighbours draws. THE rule: the
--   engine's render pass and @scripts/wire.lua@'s placer both go through
--   it, so there is no second sixteen-way table to drift.
structureWireShapeFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
structureWireShapeFn = do
    n ← Lua.toboolean 1
    e ← Lua.toboolean 2
    s ← Lua.toboolean 3
    w ← Lua.toboolean 4
    Lua.pushstring (TE.encodeUtf8 (wireShapeName (wireShapeFor
        (WireNeighbors n e s w))))
    return 1

-- | @structure.wireNeighbors(gx, gy[, page[, includeDesignations]]) →
--   { n=, e=, s=, w= }@ — which cardinal neighbours of a tile a wire
--   there would connect to.
--
--   @includeDesignations@ defaults to FALSE, which is the PLACEMENT
--   answer (placed and staged wire only) and exactly what
--   @scripts/wire.lua@ has always used. Passing true adds wire
--   DESIGNATIONS, which is the render pass's question. Every lookup is
--   canonicalized across the cylindrical seam, as @structure.hasAt@ is.
--
--   An unknown page or missing world answers all-false rather than nil,
--   so a caller always gets four booleans; the shape it derives is then
--   @isolated@, which is what a tile with nothing around it draws.
structureWireNeighborsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureWireNeighborsFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    mPage ← argString 3
    withDesigs ← Lua.toboolean 4
    neigh ← case (gxA, gyA) of
        (Just gx, Just gy) → Lua.liftIO $ do
            mWs ← fmap snd <$> resolveStructurePage env mPage
            case mWs of
                Nothing → pure (WireNeighbors False False False False)
                Just ws → do
                    worldSize ← pageWrapWorldSize ws
                    td    ← readIORef (wsTilesRef ws)
                    stage ← readIORef (wsStructureStageRef ws)
                    mDes  ← if withDesigs
                        then Just <$> readIORef (wsConstructDesignationsRef ws)
                        else pure Nothing
                    pure (wireNeighborsAt worldSize td stage mDes
                                          (fromIntegral gx) (fromIntegral gy))
        _ → pure (WireNeighbors False False False False)
    Lua.newtable
    forM_ [ ("n", wnNorth neigh), ("e", wnEast neigh)
          , ("s", wnSouth neigh), ("w", wnWest neigh) ] $ \(k, v) → do
        Lua.pushboolean v
        Lua.setfield (-2) k
    return 1

-- * Argument helpers

argString ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe Text)
argString i = do
    ty ← Lua.ltype i
    if ty ≢ Lua.TypeString
        then pure Nothing
        else fmap (fmap TE.decodeUtf8Lenient) (Lua.tostring i)

fieldString ∷ Lua.StackIndex → Lua.Name → Lua.LuaE Lua.Exception (Maybe Text)
fieldString idx name = do
    ty ← Lua.getfield idx name
    v  ← if ty ≢ Lua.TypeString then pure Nothing
         else fmap (fmap TE.decodeUtf8Lenient) (Lua.tostring (-1))
    Lua.pop 1
    pure v

-- | An @engine.loadTexture@ handle. Must be an actual integer field:
--   'Lua.tointeger' coerces a numeric STRING, which would let a typo'd
--   payload register art the renderer cannot draw.
fieldHandle ∷ Lua.StackIndex → Lua.Name
            → Lua.LuaE Lua.Exception (Maybe TextureHandle)
fieldHandle idx name = do
    ty ← Lua.getfield idx name
    v  ← if ty ≢ Lua.TypeNumber then pure Nothing else Lua.tointeger (-1)
    Lua.pop 1
    pure (TextureHandle ∘ fromIntegral <$> v)

-- | A 'Lua.Name' as text, for a diagnostic that names the field it
--   could not read.
nameText ∷ Lua.Name → Text
nameText (Lua.Name bs) = TE.decodeUtf8Lenient bs

wallEdgeFromText ∷ Text → Maybe WallEdge
wallEdgeFromText t = case T.toLower t of
    "ne" → Just WallNE
    "nw" → Just WallNW
    "se" → Just WallSE
    "sw" → Just WallSW
    _    → Nothing
