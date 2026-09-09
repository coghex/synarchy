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
    , structureConstructionGhosts
    , structurePreviewGhosts
    , constructionAppearanceAt
    , drawnWallEdge
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Building.Visual
    (designatedGhostAlpha, ghostPieceTint, previewGhostAlpha)
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec4)
import Engine.Scene.Types (SortableQuad(..))
import Structure.ArtCatalog
    ( AppearanceKey(..), AppearanceSlot(..), ArtAsset(..)
    , PieceArt(..), PieceArtContext(..), resolveConstructionFrame )
import Structure.Facing (WallEdge, screenWallEdge)
import Structure.Render
    ( ResolvedPieceArt(..), opaqueTint, structurePieceQuadsResolved
    , translateQuad )
import Structure.Types (StructureSlot)
import Structure.WallCatalog (StructureWallCatalog, rotatedWallArt)
import World.Construct.Art (structureCommittedAt)
import World.Construct.Plan
    ( PlanOp(..), PlanOutcome(..), PlanResult(..), PlanWorld(..)
    , resolvePlanPieceArt, resolveStructurePlan, structurePieceArtContext
    , structurePieceSlot, structurePieceWallEdge )
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

-- The three names re-exported above are 'Building.Visual's since
-- #1845: a BUILDING designation is now the same two-state ghost a
-- structure piece has been since #1846, so D-19's factors and its
-- valid/invalid tint stopped being this module's private convention and
-- moved beside the geometry both families measure with. Re-exported
-- here because this is where a structure-side reader looks for them.

-- | The quads for one candidate, or none.
--
--   Draws exactly when the resolver both resolved ART and stated a final
--   grid Z. That single condition IS requirement 7's four outcomes:
--   'PlanMissingArt' resolves no art, 'PlanUnresolvedTerrain' states no
--   z, and the two that do draw are 'PlanValid' and the terrain-stage
--   'PlanVisibleInvalid'.
ghostQuadsFor ∷ GhostEnv → Vec4 → StructurePiece → (Int, Int) → PlanResult
              → [SortableQuad]
ghostQuadsFor ge = ghostQuadsWith ge Nothing

-- | 'ghostQuadsFor' with an optional LIFECYCLE resolver (#2488).
--
--   'Nothing' is the two ghost states, which draw the static sprite.
--   @Just f@ is the construction pass, for which the frame is not an
--   embellishment but the whole reason to draw: if @f@ resolves nothing
--   the candidate emits NOTHING, because requirement 8's answer for an
--   appearance with no declaration is that the site keeps drawing
--   nothing — not that it falls back to the finished sprite.
ghostQuadsWith ∷ GhostEnv → Maybe (PieceArt → Maybe ArtAsset) → Vec4
               → StructurePiece → (Int, Int) → PlanResult → [SortableQuad]
ghostQuadsWith ge lifecycle tint piece tile@(gx, gy) pr = fromMaybe [] $ do
    slot   ← prSlot pr
    gridZ  ← prFinalZ pr
    art    ← resolvePlanPieceArt (gePlan ge) piece tile
    mFrame ← case lifecycle of
        Nothing → pure Nothing
        Just f  → Just <$> f art
    wrapOff ← isChunkVisibleWrapped (geFacing ge)
                  (pwWorldSize (gePlan ge)) (geViewBounds ge)
                  (geCamX ge) (geCamY ge) (fst (globalToChunk gx gy))
    pure $ map (translateQuad wrapOff) $
        structurePieceQuadsResolved (geCatalog ge) (geLookupSlot ge)
            (geTexSizes ge) (geFacing ge) (geZSlice ge) (geEffDepth ge)
            tint gx gy (slot ∷ StructureSlot)
            (resolvedArt art) { rpaLifecycle = aaHandle <$> mFrame }
            gridZ

-- | #1842's catalogue answer, in the shape the shared render body takes.
--   Both paths are always present here — that is the whole reason a
--   ghost can rotate a wall without a texture palette.
resolvedArt ∷ PieceArt → ResolvedPieceArt
resolvedArt pa = ResolvedPieceArt
    { rpaTexture     = aaHandle (paTexture pa)
    , rpaFacemap     = aaHandle (paFacemap pa)
    , rpaTexturePath = Just (aaPath (paTexture pa))
    , rpaFacemapPath = Just (aaPath (paFacemap pa))
    , rpaLifecycle   = Nothing
    }

-- | The DESIGNATED state (D-19): every committed structure designation
--   that has not yet been paid for, at 60 % and never tinted.
--
--   Paid IS the durable transition (D-15\/D-16, and #1844 replaced
--   @cdMaterialsPaid@ with the receipt whose presence is the paid
--   state), so this pass ends exactly where payment lands. What happens
--   from there is 'structureConstructionGhosts' (#2488): the authored
--   frame the site's own progress selects, or — for an appearance that
--   declares none, which is every shipped one today — still nothing.
--
--   Each designation is resolved on behalf of its OWN attempt
--   ('PlanForAttempt'), or every one of them would count itself as the
--   outstanding designation that refuses it.
--
--   Buildings are not here, and no longer for want of their own art:
--   since #1845 a building designation draws one sprite of its own
--   definition, from "World.Render.CursorQuads". The two passes stay
--   separate because a structure ghost's POSITION is resolved
--   ('World.Construct.Plan' places a floor, a wall or a ceiling at
--   different offsets above the same surface) while a building's is its
--   own anchor tile.
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

-- | The UNDER-CONSTRUCTION state (#2488): every PAID structure
--   designation whose resolved appearance declares construction frames,
--   drawn solid at the frame its own 'cdProgress' selects.
--
--   Before this a paid site drew NOTHING between the moment its
--   materials were spent and the moment the finished piece appeared,
--   which for a wall is most of the job. The gap was never a decision —
--   #1846 had no art to fill it with, and the generic blueprint it
--   replaced is gone.
--
--   Four things this pass is deliberately NOT:
--
--     * Not a ghost. It draws at 'Structure.Render.opaqueTint', the very
--       tint a placed piece uses, because a site being physically built
--       is not a proposal. D-19's 25 % \/ 60 % lifecycle multipliers
--       belong to the two states that ARE proposals, and both are
--       untouched.
--     * Not a second geometry path. The frame goes through
--       'structurePieceQuadsResolved' exactly as the ghost and the
--       placed piece do, so #1712's rotation, #415's front-wall strips
--       and 'postToQuad'\'s inset are not reimplemented (requirement 3).
--     * Not a fallback. An appearance with no declaration resolves no
--       frame and the site keeps drawing nothing, which is requirement
--       8 and is what every shipped pack does today. Nothing substitutes
--       a blueprint, a fade, another appearance's frames or the static
--       sprite.
--     * Not a duplicate of the finished piece. A site stops drawing the
--       moment its piece is COMMITTED to the overlay the structure pass
--       renders — and not a moment earlier: 'structureCommittedAt'
--       ignores the staging cache precisely because a staged-but-
--       uncommitted piece is on screen nowhere, and blanking the tile
--       for the width of that hand-off is the intermediate empty frame
--       requirement 6 forbids.
structureConstructionGhosts ∷ GhostEnv → (Int, V.Vector SortableQuad)
structureConstructionGhosts ge
    | HM.null designs = (0, V.empty)
    | otherwise = (length candidates, V.fromList (concatMap quads candidates))
  where
    designs = pwDesignations (gePlan ge)
    tint = opaqueTint (geTileAlpha ge)
    candidates =
        [ (tile, cd, piece)
        | (tile, cd) ← HM.toList designs
        , CtStructure piece ← [cdTarget cd]
        , constructDesignationPaid cd
        , not (alreadyBuilt piece tile)
        ]
    -- Resolved on behalf of its OWN attempt, exactly as the designated
    -- state is: every other outstanding designation is still a conflict,
    -- but this one is not its own.
    quads (tile, cd, piece) =
        ghostQuadsWith ge (Just (frameFor piece tile (cdProgress cd)))
            tint piece tile $
            resolveStructurePlan (gePlan ge) (PlanForAttempt (cdAttempt cd))
                                 (cdZ cd) piece tile
    frameFor piece tile progress art = do
        ak ← constructionAppearanceAt (gePlan ge) (geCatalog ge) (geFacing ge)
                 piece tile art
        resolveConstructionFrame (pwCatalog (gePlan ge)) (spPack piece) ak
                                 progress
    alreadyBuilt piece (gx, gy) = fromMaybe False $ do
        slot ← structurePieceSlot piece
        pure (structureCommittedAt (pwWorldSize (gePlan ge))
                  (pwTiles (gePlan ge)) slot gx gy)

-- | Which authored appearance this candidate is DRAWN as at @facing@.
--
--   For every kind but a wall it is the kind itself, and the camera
--   cannot change it. A WALL's authored edge does not move but the
--   screen edge it occupies does ('Structure.Facing.screenWallEdge',
--   #1712), and the sprite drawn is the family's art for THAT edge — so
--   the construction sequence has to be that edge's too, or a turning
--   camera would show one direction's build stages on another
--   direction's wall. The frame INDEX is unaffected, which is what
--   registration's equal-length rule for a wall family guarantees
--   (requirement 5).
--
--   …but only where the wall really is rotated. 'rotatedWallArt' answers
--   'Nothing' for art no registered family carries and for a path two
--   families contest, and 'Structure.Render' then draws the piece
--   exactly as authored — so this asks the SAME function, with the same
--   arguments, and follows its answer. A screen-edge frame over an
--   authored-edge cap mask would pair two different appearances, which
--   is the one thing the shared-rotation discipline exists to prevent.
--
--   A wire's variant comes from the shared plan context, so a run being
--   built resolves the same connection shape the placer will use.
constructionAppearanceAt
    ∷ PlanWorld → StructureWallCatalog → CameraFacing → StructurePiece
    → (Int, Int) → PieceArt → Maybe AppearanceKey
constructionAppearanceAt pw catalog facing piece tile art =
    AppearanceKey Nothing <$> case spKind piece of
        "floor"   → Just ApFloor
        "ceiling" → Just ApCeiling
        "post"    → Just ApPost
        "wall"    → Just (ApWall (drawnWallEdge catalog facing
                                      (structurePieceWallEdge piece) art))
        "wire"    → Just (ApWire (pacWireShape
                                      (structurePieceArtContext pw piece tile)))
        _         → Nothing

-- | The wall edge whose art is actually DRAWN for a piece authored on
--   @edge@ at @facing@: the screen edge when the catalogue rotates this
--   exact pair, and the authored edge when it declines to.
--
--   Exported for the same reason 'constructionAppearanceAt' is: a spec
--   has to be able to name the answer without restating the rule.
drawnWallEdge ∷ StructureWallCatalog → CameraFacing → WallEdge → PieceArt
              → WallEdge
drawnWallEdge catalog facing edge art
    | isJust rotated = screenWallEdge facing edge
    | otherwise      = edge
  where
    rotated = rotatedWallArt catalog facing edge
        (aaPath (paTexture art), aaHandle (paTexture art))
        (aaPath (paFacemap art), aaHandle (paFacemap art))

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
