{-# LANGUAGE Strict #-}
-- | The UNPLACED-piece art contract (#1842): given only a construction
--   designation's abstract descriptor — pack, kind, optional wall edge
--   ('World.Construct.Types.StructurePiece') — which texture and facemap
--   would the build AI actually place there?
--
--   Why a registry at all. A designation is deliberately art-free so it
--   stays save-stable across art changes, and the pack YAML's
--   kind → texture\/facemap translation lives only in
--   @scripts/structures.lua@ and @scripts/wire.lua@. The world render
--   thread cannot call into Lua, so the answer has to be handed over
--   up front. That is exactly what "Structure.WallCatalog" already does
--   for the four wall directions of a PLACED piece (#1712); this
--   generalizes the same mechanism to "every kind a pack offers, before
--   anything is placed".
--
--   Consequences worth knowing:
--
--     * Registration is ALL OR NOTHING per pack, against an EXPLICIT
--       declared-kind inventory: the payload names the picker kinds the
--       pack offers, and every required art role of every declared kind
--       must be present or the whole pack is refused. Half a pack would
--       let a ghost show art for the kinds that happened to parse and
--       nothing for the rest — the same reason 'registerWallFamily'
--       refuses a short family.
--     * Registration is keyed by PACK NAME and holds texture PATHS, so —
--       like the wall catalogue — it survives the wholesale palette
--       replacement a load performs and never needs redoing. An
--       IDENTICAL repeat is an idempotent no-op; a CONFLICTING repeat
--       is refused and leaves the stored pack exactly as it was, so
--       registration order cannot decide what a pack means.
--     * Nothing here interns a path into the saved 'Structure.Palette'
--       or touches any persisted state. Merely KNOWING a piece's future
--       art must not make a save carry palette entries for art nobody
--       built (#1675 established that invariant for a rejected
--       placement; it holds here for art that was never placed at all).
--     * A terminal texture-load failure AFTER registration
--       ('failPackArtPath') makes the whole affected pack resolve
--       nothing, because the pack's art is only meaningful as a set —
--       the same all-or-nothing rule, applied at the other end of the
--       asset's life. Each (pack, path) failure is recorded once, so the
--       caller's warning is emitted once per failed asset and never per
--       candidate or per frame.
--     * BUILDABILITY is a separate answer from ART. A pack's @build:@
--       block is keyed by kind and may omit one the art covers, so
--       'packKindBuildable' is deliberately independent of
--       'resolveUnplacedArt' — including for a pack whose art has
--       failed, since a texture that would not load says nothing about
--       what a job costs.
--     * An unregistered pack, an undeclared kind, and a failed pack all
--       resolve NOTHING. Never a guess, never another pack's art, never
--       a fallback.
module Structure.ArtCatalog
    ( -- * Vocabulary
      PieceKind(..)
    , pieceKindName
    , pieceKindFromText
    , allPieceKinds
    , ArtKey(..)
    , artKeyKind
    , artKeyRole
    , requiredArtKeys
      -- * Art
    , ArtAsset(..)
    , PieceArt(..)
      -- * Construction appearances (#2488)
    , AppearanceSlot(..)
    , AppearanceKey(..)
    , appearanceSlotKind
    , appearanceSlotRole
    , appearanceKeyRole
    , defaultAppearance
    , artKeyAppearanceSlot
    , ConstructionSequence(..)
    , constructionFrameIndex
    , constructionFrameAt
      -- * Build cost
    , BuildCost(..)
    , mkBuildCost
      -- * The catalogue
    , PackArt(..)
    , StructureArtCatalog(..)
    , emptyStructureArtCatalog
      -- * Registration
    , PackArtRegistration(..)
    , RegistrationOutcome(..)
    , registerPackArt
      -- * Failure
    , ArtFault(..)
    , artFaultMessage
    , ArtAssetFailure(..)
    , artAssetFailureMessage
    , ArtFailureReport(..)
    , failPackArtPath
      -- * Resolution
    , PieceArtContext(..)
    , defaultPieceArtContext
    , resolveUnplacedArt
    , resolveConstructionSequence
    , resolveConstructionFrame
    , undeclaredConstructionAppearances
    , missingConstructionMessage
    , packKindBuildable
    , packKindBuild
    , packArtResolves
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import Structure.Facing (WallEdge(..), WallCaps(..), wallCapsCode)
import Structure.Wire (WireShape(..), wireShapeName, allWireShapes)

-- * Vocabulary

-- | The piece kinds a pack's build picker offers — the exact key space
--   a pack YAML's @build:@ block uses (the four wall edges share the one
--   @wall@ entry, and a post's corner is not part of its identity
--   because a pack carries ONE post sprite).
data PieceKind = KFloor | KCeiling | KPost | KWall | KWire
    deriving (Show, Eq, Ord, Enum, Bounded)

allPieceKinds ∷ [PieceKind]
allPieceKinds = [minBound .. maxBound]

-- | The designation's own spelling ('World.Construct.Types.spKind') and
--   the pack YAML's @build:@ key. One vocabulary, not two.
pieceKindName ∷ PieceKind → Text
pieceKindName k = case k of
    KFloor   → "floor"
    KCeiling → "ceiling"
    KPost    → "post"
    KWall    → "wall"
    KWire    → "wire"

pieceKindFromText ∷ Text → Maybe PieceKind
pieceKindFromText t = lookup t [ (pieceKindName k, k) | k ← allPieceKinds ]

-- | One addressable art slot. A kind with variants (a wall's cap state,
--   a wire's connection shape) has one key per variant, because that is
--   the granularity the builder actually picks at.
data ArtKey
    = AkFloor
    | AkCeiling
    | AkPost
    | AkWall !WallEdge !WallCaps
    | AkWire !WireShape
    deriving (Show, Eq, Ord)

artKeyKind ∷ ArtKey → PieceKind
artKeyKind k = case k of
    AkFloor    → KFloor
    AkCeiling  → KCeiling
    AkPost     → KPost
    AkWall _ _ → KWall
    AkWire _   → KWire

-- | The human name of an art slot, for the one warning a failure emits.
--   Names the ROLE, not a path, so it still identifies the asset when
--   the fault is that the asset is MISSING and there is no path to name.
artKeyRole ∷ ArtKey → Text
artKeyRole k = case k of
    AkFloor      → "floor"
    AkCeiling    → "ceiling"
    AkPost       → "post"
    AkWall e c   → "wall " <> wallEdgeCode e <> " cap " <> wallCapsCode c
    AkWire s     → "wire connection " <> wireShapeName s

-- | Every art slot a declared kind MUST carry. This is the inventory the
--   all-or-nothing rule is checked against: omit one of these for a kind
--   the payload declares and the whole pack is refused.
requiredArtKeys ∷ PieceKind → [ArtKey]
requiredArtKeys k = case k of
    KFloor   → [AkFloor]
    KCeiling → [AkCeiling]
    KPost    → [AkPost]
    KWall    → [ AkWall e c | e ← [minBound .. maxBound]
                            , c ← [ WallCaps l r | l ← [False, True]
                                                 , r ← [False, True] ] ]
    KWire    → [ AkWire s | s ← allWireShapes ]

-- * Art

-- | One texture: the PATH the builder would place with, and the runtime
--   handle Lua already loaded for it. Both, for the same reason the wall
--   catalogue carries both — the palette's id→handle table cannot answer
--   for art nothing has placed.
data ArtAsset = ArtAsset
    { aaPath   ∷ !Text
    , aaHandle ∷ !TextureHandle
    } deriving (Show, Eq)

-- | The exact pair @structure.place@ would be called with.
data PieceArt = PieceArt
    { paTexture ∷ !ArtAsset
    , paFacemap ∷ !ArtAsset
    } deriving (Show, Eq)

-- * Construction appearances (#2488)

-- | Which authored STATIC APPEARANCE a construction sequence belongs to.
--
--   Deliberately coarser than 'ArtKey': a wall's four cap facemaps all
--   draw ONE sprite ('walls.\<edge\>.texture'), so a pack declares one
--   construction sequence per wall EDGE and not one per (edge, cap).
--   Every other kind's appearance and its art slot coincide.
data AppearanceSlot
    = ApFloor
    | ApCeiling
    | ApPost
    | ApWall !WallEdge
    | ApWire !WireShape
    deriving (Show, Eq, Ord)

appearanceSlotKind ∷ AppearanceSlot → PieceKind
appearanceSlotKind s = case s of
    ApFloor   → KFloor
    ApCeiling → KCeiling
    ApPost    → KPost
    ApWall _  → KWall
    ApWire _  → KWire

-- | The human name of an appearance, for the one diagnostic a missing or
--   refused sequence emits. Names the appearance, never a frame path.
appearanceSlotRole ∷ AppearanceSlot → Text
appearanceSlotRole s = case s of
    ApFloor   → "floor"
    ApCeiling → "ceiling"
    ApPost    → "post"
    ApWall e  → "wall " <> wallEdgeCode e
    ApWire w  → "wire connection " <> wireShapeName w

wallEdgeCode ∷ WallEdge → Text
wallEdgeCode e = case e of
    WallNE → "ne"
    WallNW → "nw"
    WallSE → "se"
    WallSW → "sw"

-- | One addressable appearance of one pack: the variant it belongs to
--   ('Nothing' = the pack's own default art) and the slot.
--
--   The variant is part of the KEY rather than a fallback chain, which is
--   requirement 1's "a variant's override cannot inherit or substitute
--   the default appearance's frames". A pack variant that declares no
--   sequence resolves none — never the default's.
data AppearanceKey = AppearanceKey
    { apVariant ∷ !(Maybe Text)
    , apSlot    ∷ !AppearanceSlot
    } deriving (Show, Eq, Ord)

appearanceKeyRole ∷ AppearanceKey → Text
appearanceKeyRole k =
    maybe "" (\v → "variant '" <> v <> "' ") (apVariant k)
      <> appearanceSlotRole (apSlot k)

-- | The appearance an art slot is drawn as in the pack's DEFAULT art.
--   The only appearance a construction DESIGNATION can ever select: a
--   designation carries pack \/ kind \/ edge and no variant, exactly as
--   @scripts\/unit_ai_construct.lua@ builds it.
defaultAppearance ∷ ArtKey → AppearanceKey
defaultAppearance = AppearanceKey Nothing ∘ artKeyAppearanceSlot

artKeyAppearanceSlot ∷ ArtKey → AppearanceSlot
artKeyAppearanceSlot k = case k of
    AkFloor    → ApFloor
    AkCeiling  → ApCeiling
    AkPost     → ApPost
    AkWall e _ → ApWall e
    AkWire w   → ApWire w

-- | One appearance's authored construction playback: the ordered frames
--   and the STATIC sprite the last of them hands off to.
--
--   'csStatic' is carried rather than derived because a VARIANT's
--   appearance has no entry in 'pkArt' at all — the art catalogue stores
--   default art only — and requirement 6's final-frame dimension check
--   needs to name the sprite the sequence ends on either way.
data ConstructionSequence = ConstructionSequence
    { csStatic ∷ !ArtAsset
    , csFrames ∷ !(V.Vector ArtAsset)
      -- ^ Ordered, NON-EMPTY (registration refuses an empty list).
    } deriving (Show, Eq)

-- | The frame index a progress fraction selects, in the same convention
--   'Building.Visual.pickBuildingFrame' uses: @floor (progress * n)@,
--   clamped, so 0.0 selects the first frame and 1.0 the last.
--
--   A non-finite progress selects the FIRST frame rather than an
--   arbitrary one: @floor@ of a NaN is unspecified, and a site whose
--   progress has gone wrong must not pick a frame at random.
constructionFrameIndex ∷ Int → Float → Int
constructionFrameIndex n progress
    | n ≤ 1          = 0
    | isNaN progress = 0
    | otherwise      = max 0 (min (n - 1) raw)
  where
    raw = floor (realToFrac progress * fromIntegral n ∷ Double) ∷ Int

-- | The frame this sequence shows at @progress@.
constructionFrameAt ∷ Float → ConstructionSequence → ArtAsset
constructionFrameAt progress cs =
    csFrames cs V.! constructionFrameIndex (V.length (csFrames cs)) progress

-- * Build cost

-- | One kind's COMPLETE @build:@ entry, as the engine needs it (#1844):
--   worker-seconds of effort and the exact material multiset a job
--   consumes when construction starts.
--
--   Both halves are required for a kind to be buildable at all, which is
--   why this is one value rather than two independently-present fields —
--   @scripts\/structures.lua@'s @buildableKind@ has always meant exactly
--   "@build_work@ AND @materials@".
data BuildCost = BuildCost
    { bcWork      ∷ !Double
      -- ^ @build_work@: worker-seconds at build rate 1.0.
    , bcMaterials ∷ ![(Text, Int)]
      -- ^ (item def name, count) — summed per name, positive counts
      --   only, ASCENDING by name. Canonical because a reconstructed
      --   material receipt must be deterministic and a Lua table's
      --   iteration order is not; build it with 'mkBuildCost'.
    } deriving (Show, Eq)

-- | Canonicalise a raw cost: counts summed per material, non-positive
--   totals dropped, ascending by name.
mkBuildCost ∷ Double → [(Text, Int)] → BuildCost
mkBuildCost work raw = BuildCost
    { bcWork      = work
    , bcMaterials = [ (n, c) | (n, c) ← M.toAscList (M.fromListWith (+) raw)
                             , c > 0 ]
    }

-- * The catalogue

-- | One registered pack.
data PackArt = PackArt
    { pkKinds     ∷ !(S.Set PieceKind)
      -- ^ The picker kinds this pack declares. A kind absent here
      --   resolves nothing, whatever art happens to be stored.
    , pkBuildable ∷ !(S.Set PieceKind)
      -- ^ The declared kinds whose @build:@ entry is complete
      --   (@build_work@ AND @materials@). A subset of 'pkKinds', and
      --   deliberately independent of whether the kind's art resolves.
    , pkBuild     ∷ !(M.Map PieceKind BuildCost)
      -- ^ #1844: the exact COST of each kind whose registration supplied
      --   one. A separate answer from 'pkBuildable', which stays the
      --   pack's own declaration: the engine needs the numbers because
      --   it charges structure jobs and reconstructs a pre-#1844 paid
      --   designation's material receipt itself, and the world thread
      --   cannot call into Lua to read a pack YAML. A payload that
      --   declares a kind buildable without stating its cost is not
      --   refused — it simply cannot be PAID for, which is what a
      --   registration carrying no numbers honestly means.
    , pkArt       ∷ !(M.Map ArtKey PieceArt)
    , pkFrames    ∷ !(M.Map AppearanceKey ConstructionSequence)
      -- ^ #2488: the CONSTRUCTION playback each authored appearance
      --   declares, keyed by ('AppearanceKey') variant and appearance —
      --   never inherited, never substituted. An appearance absent here
      --   resolves no sequence, which is requirement 8's "the site draws
      --   nothing" and is what every shipped pack does today.
    , pkFailures  ∷ !(HM.HashMap Text Text)
      -- ^ Terminal texture-load failures: the failed PATH → its reason,
      --   recorded once each. Non-empty means the whole pack resolves
      --   nothing; the map is also what makes the warning fire once per
      --   failed asset rather than once per attempt or per lookup.
    } deriving (Show, Eq)

-- | Every registered pack by NAME. Written by @LuaThread@ at pack load,
--   read by the render pass; never cleared, and — like the wall
--   catalogue — deliberately not session-replaced, because it is keyed
--   by pack name and holds paths, neither of which a load invalidates.
newtype StructureArtCatalog = StructureArtCatalog
    { sacPacks ∷ HM.HashMap Text PackArt
    } deriving (Show, Eq)

emptyStructureArtCatalog ∷ StructureArtCatalog
emptyStructureArtCatalog = StructureArtCatalog HM.empty

-- * Failure

-- | One refused registration or failed asset, carrying everything the
--   single warning must name: the pack, the kind when the fault has one,
--   the asset ROLE (always — a missing asset has no path), and the
--   offending path when there is one.
data ArtFault = ArtFault
    { afPack   ∷ !Text
    , afKind   ∷ !(Maybe PieceKind)
    , afRole   ∷ !Text
    , afPath   ∷ !(Maybe Text)
    , afReason ∷ !Text
    } deriving (Show, Eq)

-- | The ONE warning line. Every field the issue's requirement 7 names is
--   in it, in a fixed order, so a test can assert on the parts rather
--   than on a prose sentence.
artFaultMessage ∷ ArtFault → Text
artFaultMessage f = mconcat
    [ "structure art: pack '", afPack f, "'"
    , maybe "" (\k → " kind '" <> pieceKindName k <> "'") (afKind f)
    , " asset '", afRole f, "'"
    , maybe "" (\p → " (" <> p <> ")") (afPath f)
    , ": ", afReason f ]

-- * Registration

-- | One pack's complete art declaration.
data PackArtRegistration = PackArtRegistration
    { parPack    ∷ !Text
    , parKinds   ∷ ![(PieceKind, Bool, Maybe BuildCost)]
      -- ^ The declared picker kinds, each with whether its @build:@
      --   entry is complete and — since #1844 — that entry's exact cost
      --   when the payload states one. Declaring a kind is what obliges
      --   the payload to carry all of its 'requiredArtKeys'.
      --
      --   @buildable@ is MANDATORY per kind (art and buildability are
      --   independent answers); the COST is optional, because a
      --   registration that omits it is not malformed, it is simply one
      --   the engine cannot charge against.
    , parEntries ∷ ![(ArtKey, PieceArt)]
    , parFrames  ∷ ![(AppearanceKey, ConstructionSequence)]
      -- ^ #2488: the construction sequences this pack declares, at most
      --   one per appearance. Empty for a pack that declares none, which
      --   is every shipped pack today.
    , parSizes   ∷ !(HM.HashMap Text (Int, Int))
      -- ^ Measured pixel dimensions, by path, for requirement 6's
      --   final-frame check. The caller measures because the catalogue is
      --   pure and the images are on disk; only the paths a declared
      --   sequence names are ever consulted, so a pack with no sequences
      --   needs none. A sequence whose static sprite or last frame is
      --   NOT in here is refused — an unmeasurable image is a fault, not
      --   a check to skip.
    } deriving (Show, Eq)

data RegistrationOutcome
    = ArtRegistered
      -- ^ Stored. The pack was not present.
    | ArtAlreadyRegistered
      -- ^ An identical repeat: nothing changed, and that is success.
    | ArtRegistrationRefused !ArtFault
      -- ^ Malformed, incomplete, or a CONFLICTING repeat. The catalogue
      --   is unchanged.
    deriving (Show, Eq)

-- | Register one pack, all or nothing.
--
--   The checks run in a fixed order so the reported fault is the FIRST
--   thing wrong rather than an arbitrary one: the payload's own shape,
--   then each entry's assets, then the declared-kind inventory, then the
--   conflict with anything already stored.
registerPackArt ∷ PackArtRegistration → StructureArtCatalog
                → (StructureArtCatalog, RegistrationOutcome)
registerPackArt reg cat = case validate of
    Left fault → (cat, ArtRegistrationRefused fault)
    Right pack → case HM.lookup name (sacPacks cat) of
        Nothing → ( StructureArtCatalog (HM.insert name pack (sacPacks cat))
                  , ArtRegistered )
        Just existing
            | sameDeclaration existing pack → (cat, ArtAlreadyRegistered)
            | otherwise →
                (cat, ArtRegistrationRefused (conflictFault existing pack))
  where
    name = parPack reg

    -- A repeat is IDENTICAL by what it declares. Recorded failures are
    -- not part of that comparison: an asset that failed to load stays
    -- failed, and re-declaring the same art is not evidence it now
    -- loads.
    sameDeclaration a b = pkKinds a ≡ pkKinds b
                        ∧ pkBuildable a ≡ pkBuildable b
                        ∧ pkBuild a ≡ pkBuild b
                        ∧ pkArt a ≡ pkArt b
                        ∧ pkFrames a ≡ pkFrames b

    -- Name WHAT differs rather than dumping both declarations: the
    -- kinds usually match and the art is where a conflicting repeat
    -- actually diverges, so printing the kind lists side by side reads
    -- as two identical halves and says nothing.
    conflictFault existing pack = ArtFault
        { afPack   = name
        , afKind   = Nothing
        , afRole   = "pack declaration"
        , afPath   = Nothing
        , afReason = "a different registration for this pack is already "
                     <> "stored (differs in: " <> differences <> "); the "
                     <> "stored one is kept" }
      where
        differences = T.intercalate ", "
            [ what
            | (what, differs) ←
                [ ("declared kinds", pkKinds existing ≢ pkKinds pack)
                , ("buildable kinds", pkBuildable existing ≢ pkBuildable pack)
                , ("build costs", pkBuild existing ≢ pkBuild pack)
                , ("art", pkArt existing ≢ pkArt pack)
                , ("construction frames", pkFrames existing ≢ pkFrames pack) ]
            , differs ]

    fault mKind role mPath reason = ArtFault
        { afPack = name, afKind = mKind, afRole = role
        , afPath = mPath, afReason = reason }

    validate ∷ Either ArtFault PackArt
    validate = do
        when (name ≡ "") $
            Left (fault Nothing "pack name" Nothing "the pack name is empty")
        let kinds     = [ k | (k, _, _) ← parKinds reg ]
            kindSet   = S.fromList kinds
            buildable = S.fromList [ k | (k, True, _) ← parKinds reg ]
            buildMap  = M.fromList [ (k, c) | (k, _, Just c) ← parKinds reg ]
        when (null kinds) $
            Left (fault Nothing "declared kinds" Nothing
                        "the registration declares no piece kinds")
        when (S.size kindSet ≢ length kinds) $
            Left (fault Nothing "declared kinds" Nothing
                        "a piece kind is declared more than once")
        -- Every entry must be well-formed on its own…
        forM_ (parEntries reg) $ \(key, art) → do
            checkAsset key "texture" (paTexture art)
            checkAsset key "facemap" (paFacemap art)
            unless (artKeyKind key `S.member` kindSet) $
                Left (fault (Just (artKeyKind key)) (artKeyRole key) Nothing
                            "art was supplied for a kind the registration \
                            \does not declare")
        let artMap = M.fromList (parEntries reg)
        when (M.size artMap ≢ length (parEntries reg)) $
            Left (fault Nothing "art entries" Nothing
                        "the same art slot is supplied more than once")
        -- …and every declared kind must be COMPLETE.
        forM_ kinds $ \k → forM_ (requiredArtKeys k) $ \key →
            unless (M.member key artMap) $
                Left (fault (Just k) (artKeyRole key) Nothing
                            "the registration supplies no art for this slot")
        -- #2488: the construction sequences, under the same
        -- all-or-nothing rule. Checked AFTER the static inventory so a
        -- pack that is short of a sprite is reported as that rather than
        -- as a sequence pointing at a slot it never declared.
        frameMap ← validateFrames kindSet artMap
        pure PackArt { pkKinds     = kindSet
                     , pkBuildable = buildable
                     , pkBuild     = buildMap
                     , pkArt       = artMap
                     , pkFrames    = frameMap
                     , pkFailures  = HM.empty }

    -- #2488's registration rules, in a fixed order so the reported fault
    -- is the first thing wrong: each sequence's own shape, then the
    -- default-variant cross-check against the static art it hands off
    -- to, then the final-frame dimensions, then the per-family length
    -- agreement, then duplicate appearances.
    validateFrames ∷ S.Set PieceKind → M.Map ArtKey PieceArt
                   → Either ArtFault (M.Map AppearanceKey ConstructionSequence)
    validateFrames kindSet artMap = do
        forM_ (parFrames reg) $ \(ak, cs) → do
            let kind  = appearanceSlotKind (apSlot ak)
                role  = appearanceKeyRole ak <> " construction"
                sFault = fault (Just kind) role
            unless (kind `S.member` kindSet) $
                Left (sFault Nothing
                        "construction frames were supplied for a kind the \
                        \registration does not declare")
            when (V.null (csFrames cs)) $
                Left (sFault Nothing "the construction frame list is empty")
            checkFramePath kind role "static sprite" (csStatic cs)
            forM_ (zip [1 ∷ Int ..] (V.toList (csFrames cs))) $ \(i, a) →
                checkFramePath kind (role <> " frame " <> tshow i) "frame" a
            let paths = map aaPath (V.toList (csFrames cs))
            when (S.size (S.fromList paths) ≢ length paths) $
                Left (sFault Nothing
                        "the construction frame list names the same image \
                        \more than once")
            -- The DEFAULT art's sequence must hand off to the very
            -- sprite this registration declares for the appearance. A
            -- variant's cannot be cross-checked — the catalogue stores
            -- default art only — so its static sprite is taken on the
            -- payload's word and only measured.
            when (isNothing (apVariant ak)) $
                forM_ [ paTexture art
                      | (key, art) ← M.toList artMap
                      , artKeyAppearanceSlot key ≡ apSlot ak ] $ \declared →
                    when (aaPath declared ≢ aaPath (csStatic cs)) $
                        Left (sFault (Just (aaPath (csStatic cs)))
                                ("the sequence hands off to a sprite this \
                                 \pack does not declare for the appearance \
                                 \("
                                   <> aaPath declared <> ")"))
            -- Requirement 6: the handoff is continuous only if the last
            -- frame occupies exactly the static sprite's canvas.
            lastDim   ← measured sFault (aaPath (V.last (csFrames cs)))
            staticDim ← measured sFault (aaPath (csStatic cs))
            when (lastDim ≢ staticDim) $
                Left (sFault (Just (aaPath (V.last (csFrames cs))))
                        ("the last construction frame is " <> dims lastDim
                           <> " but its static sprite " <> aaPath (csStatic cs)
                           <> " is " <> dims staticDim))
        -- A wall family's four directions must run to the SAME length,
        -- or one progress value would select different stages of the
        -- build at different camera facings (requirement 5). A direction
        -- the pack never declared stays absent instead (requirement 8).
        forM_ (M.toList wallLengths) $ \(variant, lens) →
            when (S.size (S.fromList (map snd lens)) > 1) $
                Left (fault (Just KWall)
                        (appearanceKeyRole (AppearanceKey variant (ApWall WallNE))
                           <> " construction")
                        Nothing
                        ("this wall family's declared directions run to \
                         \different lengths ("
                           <> T.intercalate ", "
                                [ wallEdgeCode e <> ": " <> tshow n
                                | (e, n) ← sortOn (wallEdgeCode ∘ fst) lens ]
                           <> "), so one progress value would select \
                              \different stages at different facings"))
        let frameMap = M.fromList (parFrames reg)
        when (M.size frameMap ≢ length (parFrames reg)) $
            Left (fault Nothing "construction frames" Nothing
                        "the same appearance declares construction frames \
                        \more than once")
        pure frameMap
      where
        wallLengths = M.fromListWith (++)
            [ (apVariant ak, [(e, V.length (csFrames cs))])
            | (ak, cs) ← parFrames reg, ApWall e ← [apSlot ak] ]

    dims (w, h) = tshow w <> "x" <> tshow h

    measured sFault path = case HM.lookup path (parSizes reg) of
        Just d  → Right d
        Nothing → Left (sFault (Just path)
                          "the image could not be measured, so the handoff \
                          \dimensions cannot be checked")

    -- A frame path is a RELATIVE resource path and nothing else. An
    -- absolute path, a Windows separator, a drive\/scheme colon or a
    -- `..` segment would let a pack YAML name an image outside the
    -- resource root, which is not art this game ships.
    checkFramePath kind role what asset = do
        when (aaPath asset ≡ "") $
            Left (fault (Just kind) (role <> " " <> what) Nothing
                        "the asset path is empty")
        when (escapingPath (aaPath asset)) $
            Left (fault (Just kind) (role <> " " <> what) (Just (aaPath asset))
                        "the asset path escapes the resource root")
        let TextureHandle h = aaHandle asset
        unless (h > 0) $
            Left (fault (Just kind) (role <> " " <> what) (Just (aaPath asset))
                        ("the texture handle is not a loaded handle ("
                          <> tshow h <> ")"))

    checkAsset key role asset = do
        when (aaPath asset ≡ "") $
            Left (fault (Just (artKeyKind key)) (artKeyRole key <> " " <> role)
                        Nothing "the asset path is empty")
        let TextureHandle h = aaHandle asset
        unless (h > 0) $
            Left (fault (Just (artKeyKind key)) (artKeyRole key <> " " <> role)
                        (Just (aaPath asset))
                        ("the texture handle is not a loaded handle ("
                          <> tshow h <> ")"))

-- | Does this path leave the resource root? A leading separator, a
--   Windows separator, a scheme\/drive colon or any @.@ \/ @..@ segment
--   all do. Pure and total, so a pack's declaration is refused at
--   registration rather than at the point some loader would have opened
--   the file.
escapingPath ∷ Text → Bool
escapingPath path =
    T.isPrefixOf "/" path
      ∨ T.isPrefixOf "~" path
      ∨ T.isInfixOf "\\" path
      ∨ T.isInfixOf ":" path
      ∨ any (\seg → seg ≡ ".." ∨ seg ≡ "." ∨ T.null seg)
            (T.splitOn "/" path)

-- | One terminal asset failure, coalesced across every pack it newly
--   invalidated. ONE value, not one per pack: a facemap can legitimately
--   be shared between packs (@dungeon_1@'s floor and every @wire@
--   connection both draw @facemap/floorface.png@), and a per-pack
--   warning would then report a single load failure two or more times.
--   The packs are listed inside the one line, so nothing is lost.
data ArtAssetFailure = ArtAssetFailure
    { aafPath   ∷ !Text
    , aafReason ∷ !Text
    , aafPacks  ∷ ![(Text, Maybe PieceKind, Text)]
      -- ^ Pack, the kind that lost art, and the asset ROLE within it —
      --   in pack-name order, so the line is stable across runs rather
      --   than following hash order.
    } deriving (Show, Eq)

-- | The ONE warning line a terminal asset failure emits. Requirement
--   7's pack \/ kind \/ asset triple is present for every pack the path
--   belongs to, and the path itself is named once.
artAssetFailureMessage ∷ ArtAssetFailure → Text
artAssetFailureMessage f = mconcat
    [ "structure art: texture '", aafPath f, "' failed to load ("
    , aafReason f, ") -- these packs now resolve nothing: "
    , T.intercalate "; "
        [ mconcat [ "pack '", pack, "'"
                  , maybe "" (\k → " kind '" <> pieceKindName k <> "'") mKind
                  , " asset '", role, "'" ]
        | (pack, mKind, role) ← aafPacks f ] ]

-- | What 'failPackArtPath' observed.
data ArtFailureReport = ArtFailureReport
    { afrTracked ∷ !Bool
      -- ^ Is this path registered art of at least one pack? The caller
      --   uses this to decide whether the catalogue owns the diagnostic
      --   at all — an untracked path keeps whatever generic reporting it
      --   already had.
    , afrFailure ∷ !(Maybe ArtAssetFailure)
      -- ^ The single warning to emit, or 'Nothing' when every pack this
      --   path belongs to had already recorded it — which is what makes
      --   the diagnostic fire once per failed asset rather than once per
      --   attempt, per lookup, or per frame.
    } deriving (Show, Eq)

-- | Record a terminal texture-load failure by path. Every pack whose
--   registered art names it stops resolving anything; a pack that
--   already recorded this exact path is left alone and contributes
--   nothing to the warning.
failPackArtPath ∷ Text → Text → StructureArtCatalog
                → (StructureArtCatalog, ArtFailureReport)
failPackArtPath path reason cat =
    ( StructureArtCatalog (HM.union (HM.fromList updated) (sacPacks cat))
    , ArtFailureReport
        { afrTracked = not (null affected)
        , afrFailure = if null fresh then Nothing else Just ArtAssetFailure
            { aafPath = path, aafReason = reason, aafPacks = fresh } } )
  where
    -- Sorted by pack name so the one line — and any test reading it —
    -- sees a deterministic order rather than the hash map's.
    affected = sortOn fst
        [ (n, p) | (n, p) ← HM.toList (sacPacks cat)
                 , isJust (slotFor p) ∨ isJust (frameSlotFor p) ]
    fresh = [ (n, kindFor p, roleFor p)
            | (n, p) ← affected, not (HM.member path (pkFailures p)) ]
    updated = [ (n, p { pkFailures = HM.insert path reason (pkFailures p) })
              | (n, p) ← affected ]
    -- A STATIC slot names the path first; a construction frame answers
    -- only when no static slot does, so the familiar diagnostic is
    -- unchanged for every pack that declares no frames (#2488).
    kindFor p = case slotFor p of
        Just (key, _) → Just (artKeyKind key)
        Nothing       → appearanceSlotKind ∘ apSlot ∘ fst <$> frameSlotFor p
    roleFor p = case slotFor p of
        Just (key, half) → artKeyRole key <> " " <> half
        Nothing → case frameSlotFor p of
            Just (ak, i) → appearanceKeyRole ak <> " construction frame "
                             <> tshow i
            Nothing      → "registered art"
    -- The first slot of this pack that names the path, and whether the
    -- path is that slot's texture, its facemap, or both — so the warning
    -- can say WHICH kind lost WHICH half. A path shared by several slots
    -- (a facemap reused across a pack's kinds) names the lowest one; the
    -- pack is invalidated whole either way, so the choice is diagnostic
    -- only.
    slotFor p = listToMaybe
        [ (key, half)
        | (key, a) ← M.toAscList (pkArt p)
        , let isTex  = aaPath (paTexture a) ≡ path
              isFace = aaPath (paFacemap a) ≡ path
        , isTex ∨ isFace
        , let half | isTex ∧ isFace = "texture and facemap"
                   | isTex          = "texture"
                   | otherwise      = "facemap" ]
    -- The lowest appearance whose construction sequence names the path,
    -- and that frame's 1-based position. Like 'slotFor', diagnostic only:
    -- one failed frame invalidates the whole pack, exactly as a failed
    -- static sprite does, because the sequence is only meaningful whole.
    frameSlotFor p = listToMaybe
        [ (ak, i)
        | (ak, cs) ← M.toAscList (pkFrames p)
        , (i, a) ← zip [1 ∷ Int ..] (V.toList (csFrames cs))
        , aaPath a ≡ path ]

-- * Resolution

-- | The world context a resolution needs beyond the descriptor. A wall
--   needs the cap state its two end corners give it; a wire needs the
--   connection shape its neighbours give it. Every other kind ignores
--   both — the fields are not optional so a caller cannot forget one for
--   a kind that does need it.
data PieceArtContext = PieceArtContext
    { pacWallCaps  ∷ !WallCaps
    , pacWireShape ∷ !WireShape
    } deriving (Show, Eq)

-- | Uncapped wall, isolated wire — what a tile with no posts and no
--   wired neighbours resolves to. A default for callers that have
--   already established there is no such context, never a stand-in for
--   one that was not looked up.
defaultPieceArtContext ∷ PieceArtContext
defaultPieceArtContext = PieceArtContext (WallCaps False False) WireIsolated

-- | Does this pack resolve anything at all right now? False for an
--   unregistered pack and for one whose art has terminally failed.
packArtResolves ∷ StructureArtCatalog → Text → Bool
packArtResolves cat pack = case HM.lookup pack (sacPacks cat) of
    Nothing → False
    Just p  → HM.null (pkFailures p)

-- | The exact texture and facemap the build AI would place for an
--   UNPLACED piece, or 'Nothing'.
--
--   The edge argument is the designation's own @spEdge@. For a WALL an
--   absent edge resolves to 'WallNE', which is not a guess but parity:
--   both @World.Thread.Command.Cursor.Construct.structurePieceSlot@ and
--   @scripts/unit_ai_construct.lua@'s @placeStructurePiece@ default an
--   edgeless wall to @ne@, so that is genuinely the art such a
--   designation would be BUILT with. An edge that is present but not one
--   of the four resolves nothing, because the builder would ask
--   @scripts/structures.lua@ for art it does not have.
--
--   A post's edge is its corner and is deliberately ignored: a pack
--   carries one post sprite.
resolveUnplacedArt ∷ StructureArtCatalog → Text → Text → Maybe Text
                   → PieceArtContext → Maybe PieceArt
resolveUnplacedArt cat pack kindText mEdge ctx = do
    p ← HM.lookup pack (sacPacks cat)
    guard (HM.null (pkFailures p))
    kind ← pieceKindFromText kindText
    guard (kind `S.member` pkKinds p)
    key ← artKeyFor kind
    M.lookup key (pkArt p)
  where
    artKeyFor k = case k of
        KFloor   → Just AkFloor
        KCeiling → Just AkCeiling
        KPost    → Just AkPost
        KWire    → Just (AkWire (pacWireShape ctx))
        KWall    → (\e → AkWall e (pacWallCaps ctx))
                     <$> maybe (Just WallNE) wallEdgeFromText mEdge
    wallEdgeFromText t = case t of
        "ne" → Just WallNE
        "nw" → Just WallNW
        "se" → Just WallSE
        "sw" → Just WallSW
        _    → Nothing

-- | The construction playback this pack declares for that exact
--   appearance, or 'Nothing'.
--
--   'Nothing' is requirement 8: an unregistered pack, a pack whose art
--   has terminally failed, and an appearance with no declaration all
--   resolve NOTHING — never another appearance's frames, never the
--   default's for a variant that declared none, and never a substitute.
resolveConstructionSequence ∷ StructureArtCatalog → Text → AppearanceKey
                            → Maybe ConstructionSequence
resolveConstructionSequence cat pack ak = do
    p ← HM.lookup pack (sacPacks cat)
    guard (HM.null (pkFailures p))
    M.lookup ak (pkFrames p)

-- | The single frame that appearance shows at @progress@ (#2488), or
--   'Nothing' when it declares no sequence.
resolveConstructionFrame ∷ StructureArtCatalog → Text → AppearanceKey → Float
                         → Maybe ArtAsset
resolveConstructionFrame cat pack ak progress =
    constructionFrameAt progress <$> resolveConstructionSequence cat pack ak

-- | Every DEFAULT appearance this stored pack has static art for but no
--   construction sequence, ascending.
--
--   Requirement 8's diagnostic granularity, computed from the pack the
--   registration actually stored: once per (pack, appearance), never per
--   frame and never per drawn candidate. The caller emits it on a FRESH
--   registration only, so an idempotent repeat says nothing.
undeclaredConstructionAppearances ∷ PackArt → [AppearanceKey]
undeclaredConstructionAppearances p = S.toAscList $ S.fromList
    [ ak
    | key ← M.keys (pkArt p)
    , let ak = defaultAppearance key
    , not (M.member ak (pkFrames p)) ]

-- | The ONE line a missing declaration emits.
missingConstructionMessage ∷ Text → AppearanceKey → Text
missingConstructionMessage pack ak = mconcat
    [ "structure art: pack '", pack, "' appearance '", appearanceKeyRole ak
    , "' declares no construction frames -- a paid site of it draws "
    , "nothing until the piece appears" ]

-- | Does this pack's kind have COMPLETE build metadata (@build_work@ and
--   @materials@)? Deliberately independent of 'resolveUnplacedArt': a
--   kind can have art and no @build:@ entry (a debug\/stamp-only kind
--   the AI skips), and a pack whose textures failed to load still costs
--   what its YAML says it costs.
packKindBuildable ∷ StructureArtCatalog → Text → Text → Bool
packKindBuildable cat pack kindText = fromMaybe False $ do
    p    ← HM.lookup pack (sacPacks cat)
    kind ← pieceKindFromText kindText
    pure (kind `S.member` pkBuildable p)

-- | That kind's COMPLETE build cost, or 'Nothing' when the pack is
--   unregistered, the kind undeclared, or its @build:@ entry incomplete.
--
--   The engine-side authority for what a structure job costs (#1844).
--   Like 'packKindBuildable' it ignores art failures entirely: a texture
--   that would not load says nothing about what a job costs, and a job
--   already PAID never consults this at all — its receipt does.
packKindBuild ∷ StructureArtCatalog → Text → Text → Maybe BuildCost
packKindBuild cat pack kindText = do
    p    ← HM.lookup pack (sacPacks cat)
    kind ← pieceKindFromText kindText
    M.lookup kind (pkBuild p)
