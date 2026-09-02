{-# LANGUAGE Strict #-}
-- | Construction ghosts for STRUCTURE pieces (#1846): the two states
--   D-19 defines, drawn with the piece's own art.
--
--   Before this, both states drew one 96x64 category placeholder
--   (@construct_designate_structure.png@) at the designation's raw
--   surface z, and there was no preview at all before the first anchor
--   click. A floor, a ceiling, four wall edges, four posts and wire were
--   the same red diamond, one z-level below where any of them would
--   land.
--
--   Nothing here decides anything. Every question a ghost could ask has
--   an owner already, and this module is the consumer of all three:
--
--     * WHICH candidates — "World.Construct.Extent" (#1844), the same
--       bounded-drag helper the commit enumerates, so the preview and
--       the click cannot disagree about what the player drew.
--     * WHETHER a candidate is buildable and WHERE it would sit —
--       "World.Construct.Plan"'s 'resolveStructurePlan' (#1844), whose
--       four outcomes map onto D-25's four presentations. There is no
--       second eligibility rule in this file, and adding one would be
--       the exact drift #1844 exists to prevent.
--     * WHAT it looks like — 'World.Construct.Plan.resolvePlanPieceArt'
--       over #1842's registered catalogue, which resolves a wall's cap
--       facemap from that tile's own posts and a wire's variant from its
--       neighbours, exactly as the placer does.
--
--   The geometry is 'Structure.Render.structurePieceQuadsResolved' — the
--   same body a PLACED piece goes through, so #1712's camera rotation,
--   #415's front-wall depth strips and 'postToQuad'\'s per-vertex inset
--   are not reimplemented and cannot drift. The two callers differ only
--   in where the art came from and in the tint.
--
--   Three rules worth keeping in view while editing:
--
--     * NO PALETTE RESIDUE. The art arrives as @(path, handle)@ pairs
--       from the catalogue and is never interned into the saved
--       'Structure.Palette'. Knowing what a piece would look like must
--       not make a save carry entries for art nobody built (#1675).
--     * THE RESOLVER'S POSITION OR NOTHING. A candidate draws only where
--       the resolver reported a final grid z. A catalogue-stage refusal
--       and an unloaded tile both carry none, and both are correctly
--       absent (D-25: "a position whose world location cannot be
--       resolved ... remains absent").
--     * SEAM. Chunks are stored u-wrapped, so a ghost is drawn through
--       the nearest visible alias exactly as a placed piece is: the quad
--       is BUILT at the tile's own coordinates and then translated by
--       'isChunkVisibleWrapped'\'s screen offset
--       ('Structure.Render.translateQuad'), which leaves sort keys and
--       quad payloads untouched.
module World.Render.StructureGhost
    ( GhostEnv(..)
    , previewGhostAlpha
    , designatedGhostAlpha
    , ghostPieceTint
    , structureDesignationGhosts
    , structurePreviewGhosts
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Building.Render (ghostTint)
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import Engine.Scene.Types (SortableQuad(..))
import Structure.ArtCatalog (ArtAsset(..), PieceArt(..))
import Structure.Render
    (ResolvedPieceArt(..), structurePieceQuadsResolved, translateQuad)
import Structure.Types (StructureSlot)
import Structure.WallCatalog (StructureWallCatalog)
import World.Construct.Plan
    ( PlanOp(..), PlanOutcome(..), PlanResult(..), PlanWorld(..)
    , resolvePlanPieceArt, resolveStructurePlan )
import World.Construct.Types
    ( ConstructDesignation(..), ConstructTarget(..), StructurePiece(..)
    , constructDesignationPaid )
import World.Generate.Coordinates (canonicalTile, globalToChunk)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.ViewBounds (ViewBounds)

-- | Everything a ghost pass reads, taken once by the caller so every
--   candidate in a frame is judged against the same world — the same
--   discipline 'PlanWorld' itself exists for.
data GhostEnv = GhostEnv
    { geCatalog   ∷ !StructureWallCatalog
      -- ^ #1712's wall-rotation catalogue, shared with the placed pass.
    , geLookupSlot ∷ !(TextureHandle → Word32)
    , geTexSizes  ∷ !(HM.HashMap TextureHandle (Int, Int))
    , geFacing    ∷ !CameraFacing
    , geZSlice    ∷ !Int
    , geEffDepth  ∷ !Int
    , geTileAlpha ∷ !Float
      -- ^ The frame's existing depth\/fade alpha. D-19's 25 % and 60 %
      --   are lifecycle MULTIPLIERS over it, not replacements for it.
    , geViewBounds ∷ !ViewBounds
    , geCamX      ∷ !Float
    , geCamY      ∷ !Float
    , gePlan      ∷ !PlanWorld
    }

-- | D-19's two lifecycle opacity factors. Named rather than spelled at
--   the two call sites, because the whole point of the pair is that the
--   preview is LIGHTER than the commitment — a relationship two loose
--   literals would not state.
previewGhostAlpha, designatedGhostAlpha ∷ Float
previewGhostAlpha    = 0.25
designatedGhostAlpha = 0.60

-- | The tint one ghost draws with: D-19's lifecycle factor over the
--   frame's own @tileAlpha@, and — for an INVALID preview only (D-20) —
--   the RGB 'Building.Render.ghostTint' already uses for an invalid
--   building placement.
--
--   The RGB is read off that function rather than restated, so the two
--   build-tool families warn in one colour by construction. Its alpha is
--   deliberately discarded: a building ghost has one opacity, a
--   structure ghost has two, and D-19 owns which.
ghostPieceTint ∷ Float   -- ^ frame @tileAlpha@
               → Float   -- ^ lifecycle factor
               → Bool    -- ^ valid? (invalid ⇒ red)
               → Vec4
ghostPieceTint tileAlpha factor valid =
    let Vec4 r g b _ = ghostTint valid
    in Vec4 r g b (tileAlpha * factor)

-- | The quads for one candidate, or none.
--
--   Draws exactly when the resolver both resolved ART and stated a final
--   grid Z. That single condition IS requirement 7's four outcomes:
--   'PlanMissingArt' resolves no art, 'PlanUnresolvedTerrain' states no
--   z, and the two that do draw are 'PlanValid' and the terrain-stage
--   'PlanVisibleInvalid'.
ghostQuadsFor ∷ GhostEnv → Vec4 → StructurePiece → (Int, Int) → PlanResult
              → [SortableQuad]
ghostQuadsFor ge tint piece tile@(gx, gy) pr = fromMaybe [] $ do
    slot   ← prSlot pr
    gridZ  ← prFinalZ pr
    art    ← resolvePlanPieceArt (gePlan ge) piece tile
    wrapOff ← isChunkVisibleWrapped (geFacing ge)
                  (pwWorldSize (gePlan ge)) (geViewBounds ge)
                  (geCamX ge) (geCamY ge) (fst (globalToChunk gx gy))
    pure $ map (translateQuad wrapOff) $
        structurePieceQuadsResolved (geCatalog ge) (geLookupSlot ge)
            (geTexSizes ge) (geFacing ge) (geZSlice ge) (geEffDepth ge)
            tint gx gy (slot ∷ StructureSlot) (resolvedArt art) gridZ

-- | #1842's catalogue answer, in the shape the shared render body takes.
--   Both paths are always present here — that is the whole reason a
--   ghost can rotate a wall without a texture palette.
resolvedArt ∷ PieceArt → ResolvedPieceArt
resolvedArt pa = ResolvedPieceArt
    { rpaTexture     = aaHandle (paTexture pa)
    , rpaFacemap     = aaHandle (paFacemap pa)
    , rpaTexturePath = Just (aaPath (paTexture pa))
    , rpaFacemapPath = Just (aaPath (paFacemap pa))
    }

-- | The DESIGNATED state (D-19): every committed structure designation
--   that has not yet been paid for, at 60 % and never tinted.
--
--   Paid IS the durable transition (D-15\/D-16, and #1844 replaced
--   @cdMaterialsPaid@ with the receipt whose presence is the paid
--   state): from payment until the finished piece appears, a structure
--   site draws nothing at all.
--
--   Each designation is resolved on behalf of its OWN attempt
--   ('PlanForAttempt'), or every one of them would count itself as the
--   outstanding designation that refuses it.
--
--   Buildings are not here. They keep the category-marker path in
--   "World.Render.CursorQuads" until DTV-10 (#1845) gives them their own.
structureDesignationGhosts ∷ GhostEnv → (Int, V.Vector SortableQuad)
structureDesignationGhosts ge
    | HM.null designs = (0, V.empty)
    | otherwise       = (length candidates, V.fromList (concatMap quads candidates))
  where
    designs = pwDesignations (gePlan ge)
    tint = ghostPieceTint (geTileAlpha ge) designatedGhostAlpha True
    candidates =
        [ (tile, cd, piece)
        | (tile, cd) ← HM.toList designs
        , CtStructure piece ← [cdTarget cd]
        , not (constructDesignationPaid cd)
        ]
    quads (tile, cd, piece) = ghostQuadsFor ge tint piece tile $
        resolveStructurePlan (gePlan ge) (PlanForAttempt (cdAttempt cd))
                             (cdZ cd) piece tile

-- | The PREVIEW state (D-19\/D-25): the armed piece drawn over every
--   candidate of the current gesture at 25 %, red where the shared
--   resolver would refuse it.
--
--   The candidate list is the caller's — one hovered tile before the
--   first click, 'World.Construct.Extent.structureDragExtent'\'s tiles
--   after it — and arrives in the anchor's own local alias frame, which
--   is the frame each quad's screen position is computed in.
--
--   Two passes, for wire's sake (D-22). Outcomes are resolved first;
--   then the candidates that came back 'PlanValid' become the proposed
--   set the ART pass resolves against, so a dragged run previews as one
--   connected line instead of a row of isolated stubs. An INVALID
--   candidate never joins that set: it is not going to be built, so it
--   must not complete a neighbour's shape. Splitting the passes is
--   sound because a wire's connection variant cannot change an outcome —
--   a pack registers every variant of a kind or none of them.
structurePreviewGhosts
    ∷ GhostEnv
    → StructurePiece
    → Int            -- ^ required surface z (the anchor's, or the tile's own)
    → [(Int, Int)]   -- ^ candidates, anchor-local frame
    → (Int, V.Vector SortableQuad)
structurePreviewGhosts ge piece requiredZ tiles =
    (length tiles, V.fromList (concatMap quads resolved))
  where
    resolved = [ (tile, resolveStructurePlan (gePlan ge) PlanForPlacement
                            requiredZ piece tile)
               | tile ← tiles ]
    proposedWire
        | spKind piece ≢ "wire" = HS.empty
        | otherwise = HS.fromList
            [ canonicalTile (pwWorldSize (gePlan ge)) gx gy
            | ((gx, gy), pr) ← resolved, prOutcome pr ≡ PlanValid ]
    -- The art pass sees the proposed run; the outcomes above did not,
    -- and did not need to.
    geArt = ge { gePlan = (gePlan ge) { pwProposedWire = proposedWire } }
    quads (tile, pr) =
        ghostQuadsFor geArt (tintFor (prOutcome pr)) piece tile pr
    tintFor outcome =
        ghostPieceTint (geTileAlpha ge) previewGhostAlpha
                       (outcome ≡ PlanValid)
