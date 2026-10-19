{-# LANGUAGE Strict #-}
-- | Constructing and capturing scenario structure pieces (#2712, SCN-12
--   of epic #2698): floors, wall edges, corner posts, ceilings and wire
--   from a structure pack.
--
--   A structure piece is a durable per-chunk edit ('WeSetStructure'): a
--   tile, a slot, the palette ids of its sprite and facemap, and a z.
--   The scenario states none of those but the tile, the slot, the pack
--   and (since format v3) an optional pack VARIANT. Everything else is
--   derived HERE, from the registered pack art ("Structure.ArtCatalog" —
--   the one registry the placers' own art flows through), never from a
--   second reading of the pack file:
--
--   * the sprite and facemap of the art slot (@floor@, @ceiling@,
--     @post@; a wall edge; a wire connection), by pack and variant
--     ('Structure.ArtCatalog.resolveVariantArt' for a variant);
--   * a wall's two end caps — whether a corner post ON THE SAME TILE
--     carves a notch out of that end — from the posts the scenario places
--     (the rule @scripts/structures.lua@'s @placeWall@ applies, so a wall
--     stamped before its post and one stamped after converge);
--   * a wire's connection shape from which cardinal neighbours also carry
--     wire ('Structure.Wire.wireShapeFor', the rule the Lua placer calls);
--   * an omitted @z@ from the column, at the level each placer uses:
--     one above the terrain surface for a floor, a wall or wire, two above
--     for a ceiling, and the tile's own floor level for a post.
--
--   Nothing is interned or queued here: the build is pure and returns the
--   texture PATHS, which the commit ("Scenario.Placement.Install") interns
--   into the live palette only once the page is known to admit the pieces
--   (an interned path is persisted whether or not anything uses it).
--
--   Capture reads the same registries backwards: a placed piece's sprite
--   PATH is the only appearance identity it carries, and
--   'Structure.ArtCatalog.sacAppearanceByPath' resolves it to the pack and
--   variant that declare it. A path no pack declares, or two packs claim,
--   is reported and the piece left out — never guessed.
module Scenario.Placement.Structure
    ( StructureBuildEnv(..)
    , BuiltPiece(..)
    , StructureBuild(..)
    , buildStructures
    , pieceKey
    , slotName
    , slotOfTag
    , slotTagOf
    , StructureCaptureCtx(..)
    , StructureCapture(..)
    , captureStructures
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Scenario.Types
import Scenario.World.Capture (uniqueName)
import Structure.ArtCatalog
    ( ArtAsset(..), ArtKey(..), PackArt(..), PieceArt(..), PieceKind(..), StructureArtCatalog(..)
    , placedArtPairs, resolveVariantArt )
import Structure.Facing
    ( PostCorner(..), WallCaps(..), postCornerOfSlot, wallEdgeEnds
    , wallEdgeOfSlot )
import Structure.Palette (TexPalette, lookupPath)
import Structure.Types
    ( StructureSlot(..), StructurePieceData(..), slotFromText )
import Structure.Wire (WireNeighbors(..), wireShapeFor)
import World.Arena.Bounds (ArenaBounds, arenaAdmitsTile)
import World.Chunk.Types (ChunkCoord, LoadedChunk(..))

-- * Construction

-- | What construction reads: the page's finite bounds, the registered
--   pack art, and the column surface under a tile (for an omitted @z@).
data StructureBuildEnv = StructureBuildEnv
    { sbeBounds   ∷ !(Maybe ArenaBounds)
    , sbeArt      ∷ !StructureArtCatalog
    , sbeSurfaceZ ∷ (Int, Int) → Int
    }

-- | One piece ready to commit: where it goes and the art it is placed
--   with. The palette ids are the commit's to assign.
data BuiltPiece = BuiltPiece
    { bpId      ∷ !ScenarioId
    , bpPath    ∷ !Text
    , bpX       ∷ !Int
    , bpY       ∷ !Int
    , bpSlotTag ∷ !Word8
    , bpZ       ∷ !Int
    , bpTexture ∷ !Text
    , bpFacemap ∷ !Text
    } deriving (Show, Eq)

data StructureBuild = StructureBuild
    { sbPieces      ∷ ![BuiltPiece]
      -- ^ accepted pieces, in authored order
    , sbDiagnostics ∷ ![ScenarioDiagnostic]
    } deriving (Show, Eq)

-- | The slot a piece occupies, as 'WeSetStructure' keys it.
slotTagOf ∷ StructureSlot → Word8
slotTagOf = fromIntegral ∘ fromEnum

slotOfTag ∷ Word8 → Maybe StructureSlot
slotOfTag t
    | fromIntegral t ≤ fromEnum (maxBound ∷ StructureSlot) = Just (toEnum (fromIntegral t))
    | otherwise = Nothing

-- | The tile-and-slot a piece is keyed by.
pieceKey ∷ BuiltPiece → (Int, Int, Word8)
pieceKey p = (bpX p, bpY p, bpSlotTag p)

-- | The scenario's spelling of a slot (the inverse of 'slotFromText').
slotName ∷ StructureSlot → Text
slotName s = case s of
    SFloor   → "floor"
    SCeiling → "ceiling"
    SWallNE  → "wall_ne"
    SWallNW  → "wall_nw"
    SWallSE  → "wall_se"
    SWallSW  → "wall_sw"
    SPostN   → "post_n"
    SPostE   → "post_e"
    SPostS   → "post_s"
    SPostW   → "post_w"
    SWire    → "wire"

-- | The pack piece KIND a slot draws from (the four wall edges share
--   @wall@, the four corners @post@).
slotPieceKind ∷ StructureSlot → PieceKind
slotPieceKind s = case s of
    SFloor   → KFloor
    SCeiling → KCeiling
    SWire    → KWire
    _ | isJust (wallEdgeOfSlot s) → KWall
      | otherwise                 → KPost

-- | A piece that passed its own checks, before art and z are derived.
data Draft = Draft
    { dEntry ∷ !StructurePiece
    , dPath  ∷ !Text
    , dSlot  ∷ !StructureSlot
    , dPack  ∷ !PackArt
    }

buildStructures ∷ StructureBuildEnv → [(Text, StructurePiece)] → StructureBuild
buildStructures env entries =
    let checked = map (checkEntry env) entries
        drafts0 = [ d | Right d ← checked ]
        -- the same tile and slot stated twice: the later entry replaces
        -- the earlier one, which is reported (it is not silently shadowed)
        keyOf d = (spX (dEntry d), spY (dEntry d), slotTagOf (dSlot d))
        lastOf = M.fromList [ (keyOf d, dPath d) | d ← drafts0 ]
        superseded = [ (dPath d, lastOf M.! keyOf d)
                     | d ← drafts0, lastOf M.! keyOf d ≢ dPath d ]
        drafts = [ d | d ← drafts0, lastOf M.! keyOf d ≡ dPath d ]
        posts = S.fromList [ (spX e, spY e, c)
                           | d ← drafts, let e = dEntry d
                           , Just c ← [postCornerOfSlot (dSlot d)] ]
        wires = S.fromList [ (spX e, spY e) | d ← drafts, let e = dEntry d, dSlot d ≡ SWire ]
        floors = M.fromList [ ((spX e, spY e), zOf d)
                            | d ← drafts, let e = dEntry d, dSlot d ≡ SFloor ]
        surface = sbeSurfaceZ env
        zOf d = case spZ (dEntry d) of
            Authored z → z
            Omitted → defaultZ d
        defaultZ d =
            let e = dEntry d
                s = surface (spX e, spY e)
            in case dSlot d of
                SCeiling → s + 2
                slot | isJust (postCornerOfSlot slot) → M.findWithDefault (s + 1) (spX e, spY e) floors
                     | otherwise → s + 1
        built = map (finish posts wires zOf) drafts
    in StructureBuild
        { sbPieces = [ p | Right p ← built ]
        , sbDiagnostics =
            [ d | Left d ← checked ]
            ⧺ [ ScenarioDiagnostic p
                    (InvalidValue ("a piece of the same tile and slot, replaced by " <> by))
                    EntryRejected
              | (p, by) ← superseded ]
            ⧺ concat [ ds | Left ds ← built ]
            ⧺ tagDiagnostics entries
        }

-- | A structure piece has no tag owner (tags are for units, buildings,
--   locations, plants, items and tiles): its @tags@ are dropped, reported
--   once per piece, and the piece kept.
tagDiagnostics ∷ [(Text, StructurePiece)] → [ScenarioDiagnostic]
tagDiagnostics entries =
    [ ScenarioDiagnostic (p <> ".tags")
        (InvalidValue "no tags: a structure piece has no gameplay-tag owner") FieldRejected
    | (p, sp) ← entries, not (null (spTags sp)) ]

checkEntry ∷ StructureBuildEnv → (Text, StructurePiece) → Either ScenarioDiagnostic Draft
checkEntry env (path, sp) = do
    pack ← maybe (Left (diag "pack" (UnknownDefinition (spPack sp))))
                 Right (HM.lookup (spPack sp) (sacPacks (sbeArt env)))
    slot ← maybe (Left (diag "piece" (InvalidValue "a structure slot"))) Right
                 (slotFromText (spPiece sp))
    unless (slotPieceKind slot `S.member` pkKinds pack) $
        Left (diag "piece" (UnknownDefinition (spPack sp <> ":" <> spPiece sp)))
    unless (arenaAdmitsTile (sbeBounds env) (spX sp) (spY sp)) $
        Left (ScenarioDiagnostic path OutsideBounds EntryRejected)
    pure (Draft sp path slot pack)
  where diag field reason = ScenarioDiagnostic (path <> "." <> field) reason EntryRejected

-- | Derive a draft's art and z. 'Left' carries the diagnostics of a piece
--   whose art cannot be resolved, which is then left out.
finish ∷ S.Set (Int, Int, PostCorner) → S.Set (Int, Int) → (Draft → Int) → Draft
       → Either [ScenarioDiagnostic] BuiltPiece
finish posts wires zOf d = do
    let sp = dEntry d
        x = spX sp
        y = spY sp
        slot = dSlot d
        key = artKeyOf posts wires (x, y) slot
        variant = case spVariant sp of
            Authored v → Just v
            Omitted    → Nothing
        pack = dPack d
        resolved = case variant of
            Nothing → M.lookup key (pkArt pack)
            Just v  → resolveVariantArt pack v key
        missing = [ ScenarioDiagnostic (dPath d <> maybe "" (const ".variant") variant)
                      (UnknownDefinition (spPack sp <> ":" <> maybe (spPiece sp) id variant))
                      EntryRejected ]
    art ← maybe (Left missing) Right resolved
    pure BuiltPiece
        { bpId = spId sp, bpPath = dPath d, bpX = x, bpY = y
        , bpSlotTag = slotTagOf slot, bpZ = zOf d
        , bpTexture = aaPath (paTexture art), bpFacemap = aaPath (paFacemap art) }

-- | The art slot a placed piece draws: fixed for a floor, ceiling or post;
--   for a wall the cap state the same tile's posts give its two ends; for
--   wire the connection shape its cardinal neighbours give it.
artKeyOf ∷ S.Set (Int, Int, PostCorner) → S.Set (Int, Int) → (Int, Int) → StructureSlot → ArtKey
artKeyOf posts wires (x, y) slot = case slot of
    SFloor   → AkFloor
    SCeiling → AkCeiling
    SWire    → AkWire (wireShapeFor (WireNeighbors
                   { wnNorth = S.member (x, y - 1) wires
                   , wnEast  = S.member (x + 1, y) wires
                   , wnSouth = S.member (x, y + 1) wires
                   , wnWest  = S.member (x - 1, y) wires }))
    _ → case wallEdgeOfSlot slot of
        Just e →
            let (l, r) = wallEdgeEnds e
            in AkWall e (WallCaps (S.member (x, y, l) posts) (S.member (x, y, r) posts))
        Nothing → AkPost

-- * Capture

-- | What capture reads besides the chunks: the registered art and the
--   palette that turns a stored texture id back into its path, from the
--   same instant.
data StructureCaptureCtx = StructureCaptureCtx
    { sccArt     ∷ !StructureArtCatalog
    , sccPalette ∷ !TexPalette
    , sccBounds  ∷ !(Maybe ArenaBounds)
    }

data StructureCapture = StructureCapture
    { scPieces      ∷ ![StructurePiece]
      -- ^ by (x, y, slot)
    , scDiagnostics ∷ ![ScenarioDiagnostic]
    , scTaken       ∷ !(S.Set Text)
      -- ^ the explicit ids written, for the rest of the document
    }

-- | Read every placed piece of the given chunk states back into entries.
--   Every supported value is written explicitly (the pack, the slot, the
--   tile, the level, and the variant when the art is one). @reserved@ are
--   the ids of the document's other entries.
captureStructures ∷ StructureCaptureCtx → M.Map ChunkCoord LoadedChunk → S.Set Text
                  → StructureCapture
captureStructures ctx chunks reserved =
    let pieces = sortOn (\(k, _) → k)
            [ (k, d)
            | lc ← M.elems chunks
            , (k, d) ← HM.toList (lcStructures lc)
            , arenaAdmitsTile (sccBounds ctx) (fst3 k) (snd3 k) ]
        fst3 (a, _, _) = a
        snd3 (_, b, _) = b
        step (acc, diags, used) (k@(x, y, _), d) =
            case identify k d of
                Left ds → (acc, diags ⧺ ds, used)
                Right (pack, slot, variant) →
                    let base = T.intercalate "." ([ "structures", pack, slotName slot, tshow x, tshow y ])
                        (name, used') = uniqueName used base
                    in ( StructurePiece (ExplicitId name) [] pack (slotName slot) x y
                            (Authored (spdGridZ d))
                            (maybe Omitted Authored variant) : acc
                       , diags, used' )
        (revPieces, ds, taken) = foldl' step ([], [], reserved) pieces
    in StructureCapture
        { scPieces = reverse revPieces
        , scDiagnostics = ds
        , scTaken = taken }
  where
    -- the art index, once: a placed piece's (sprite, facemap) pair → every
    -- (pack, variant, art slot) that places exactly that pair
    index = HM.fromListWith (⧺) [ (pair, [c]) | (pair, c) ← placedArtPairs (sccArt ctx) ]
    -- a placed piece's identity: the (pack, variant) whose art it is
    -- drawn with, which must be art of the kind the slot occupies. Art
    -- that renders identically under several claimants (a variant that
    -- inherits the default's sprite and facemap) is the DEFAULT's when it
    -- is one of them, else the least (pack, variant): the same rendering
    -- either way, and one fixed spelling of it.
    identify (x, y, tag) d = do
        let where_ = "structures at " <> tshow (x, y) <> " slot " <> tshow tag
        slot ← maybe (bad where_ "a structure slot") Right (slotOfTag tag)
        path ← maybe (bad where_ "a sprite the palette holds") Right
                     (lookupPath (spdTexId d) (sccPalette ctx))
        face ← maybe (bad where_ "a facemap the palette holds") Right
                     (lookupPath (spdFaceId d) (sccPalette ctx))
        let candidates = [ (pack, variant) | (pack, variant, key) ← HM.lookupDefault [] (path, face) index
                         , artFits slot key ]
        case sortOn (\(pk, v) → (pk, isJust v, v)) candidates of
            [] → bad where_ ("art a registered pack places for this slot (" <> path <> " with " <> face <> ")")
            (pack, variant) : _ → pure (pack, slot, variant)
    artFits slot key = case key of
        AkFloor → slot ≡ SFloor
        AkCeiling → slot ≡ SCeiling
        AkPost → isJust (postCornerOfSlot slot)
        AkWall e _ → wallEdgeOfSlot slot ≡ Just e
        AkWire _ → slot ≡ SWire
    bad ∷ Text → Text → Either [ScenarioDiagnostic] β
    bad at why = Left [ScenarioDiagnostic at (InvalidValue why) EntryRejected]
