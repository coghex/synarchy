{-# LANGUAGE Strict #-}
-- | Render structural pieces (walls / floors / ceilings) as iso-sorted
--   quads. Closely mirrors "Building.Render": each piece is a full-tile
--   96×64 sprite drawn at its tile, but with the piece's OWN facemap slot
--   (wall sun-shading) and a per-slot sort key so near-edge walls draw in
--   front and far-edge walls behind.
--
--   FRONT walls are an exception: a single billboard quad collapses
--   a wall's whole iso-depth RANGE to one sort key, which makes it draw in
--   front of higher terrain it should sit behind (a dug room's rim — #415).
--   A vertical wall has the handy property that screen-x maps to position
--   along its edge maps to iso-depth, independent of height (rising in z only
--   moves screen-y). So a front wall is sliced into 'wallStripCount' vertical
--   strips, each sorted at its own edge depth; the painter's z-term then
--   resolves occlusion against terrain per-strip — the far (edge) strips sit
--   behind a high side rim while the near (south) strips still cover their
--   own footing. See 'frontWallStrips'.
--
--   Rotation (#1712): a piece's stored slot is its PHYSICAL world edge
--   and never moves, but the screen position that edge occupies does.
--   "Structure.Facing" holds the one mapping from an authored edge (or
--   post corner) to the screen edge (corner) it occupies at the current
--   facing, and EVERY decision here reads it: which sprite and cap
--   facemap are drawn ("Structure.WallCatalog"), whether the #415 strip
--   path applies ('isScreenFrontWall'), the sort anchors, the tie-breaks,
--   and where a post stands. At 'FaceSouth' the mapping is the identity,
--   so the output is what it has always been.
--
--   Sort anchors rotate too, and NOT by rotating a fixed grid point:
--   see 'depthAnchor'. A structure's key has to sit a fixed number of
--   painter-depth steps in front of the depth the TERRAIN pass keys the
--   same tile at, and that pass keys every tile from its raw grid coords
--   ('World.Render.TileQuads' — @applyFacing facing gx gy@). Feeding a
--   fixed offset like the tile centre through 'applyFacingF' does not
--   preserve that gap: (+0.5,+0.5) is one step in FRONT at 'FaceSouth'
--   and one step BEHIND at 'FaceNorth', which is why a rotated floor used
--   to vanish under its own terrain tile. 'depthAnchor' offsets along the
--   facing's own depth axis instead, so the gap is the same at all four.
module Structure.Render
    ( renderStructureQuads
    , renderStructureQuadsScanned
    , structureChunkQuads
    , structureChunkQuadsScanned
    , structurePieceQuads
    , isScreenFrontWall
    , wallTieBreak
    , frontWallDepthSteps
    , pieceWithinSliceBand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.RenderHandoff
  (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..)
                                           , QuadCorners(..), QuadUV(..)
                                           , QuadPayload(..), quadVertices
                                           , rectCorners, fullQuadUV
                                           , tileWorldUV)
import World.Grid (tileWidth
                   , tileHeight
                   , tileSideHeight
                   , worldLayer
                   , baseTileW
                   , baseTileH
                   , applyFacingF
                   , unapplyFacingF
                   , gridToScreen)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.ViewBounds (ViewBounds, computeViewBounds)
import World.Types (WorldState, wsTilesRef, wsGenParamsRef
                   , WorldGenParams(..))
import World.Tile.Types (WorldTileData(..))
import World.Chunk.Types (LoadedChunk(..))
import Structure.Facing
import Structure.Palette (TexPalette, lookupPath)
import Structure.Types
import Structure.WallCatalog (StructureWallCatalog, rotatedWallArt)

-- | Gather structure quads for ONE world's state. The caller iterates the
--   visible-world list (same source of truth as the terrain / ground-item /
--   spoil passes) rather than this picking a world itself — historically it
--   grabbed the head of @wmWorlds@ and could render a hidden world's
--   structures over the visible one (#72).
--
--   SEAM-AWARE since #1706. Chunks are stored u-wrapped, so a chunk whose
--   nearest image is across the cylindrical seam has to be drawn through
--   that image — the same nearest u-alias
--   'World.Render.ChunkCulling.isChunkVisibleWrapped' hands the terrain,
--   spoil, blood, cursor, ground-item and hit-test passes. This pass used
--   to make no wrap decision at all and no visibility test either, so it
--   emitted every loaded chunk's pieces at their raw canonical position:
--   the floor wrapped into view and the walls standing on it stayed a
--   whole world away (76.8 screen-world units in X at south/north, 38.4
--   in Y at west/east, for a 64-chunk world).
--
--   The culling inputs are read HERE rather than taken from the caller,
--   matching the per-frame ground-item pass: the camera and framebuffer
--   off 'EngineEnv', and the wrap period's world size off THIS page's own
--   'wsGenParamsRef' so two visible pages can never borrow each other's.
renderStructureQuads ∷ EngineEnv → WorldState → CameraFacing → Int → Int → Float
                     → IO (V.Vector SortableQuad)
renderStructureQuads env ws facing zSlice effDepth tileAlpha =
    snd ⊚ renderStructureQuadsScanned env ws facing zSlice effDepth tileAlpha

-- | 'renderStructureQuads' with the scene-assembly telemetry (#1921)
--   this pass contributes: the structure-PIECE records examined after
--   their loaded chunk passed chunk-visibility culling, paired with the
--   quads it produced.
--
--   Unlike units and buildings, the texture-system check here sits
--   BEFORE any piece is enumerated, so a GPU-free headless run
--   legitimately reports zero scanned as well as zero emitted. One
--   piece can expand into several wall strips, so emitted is not
--   bounded by scanned.
renderStructureQuadsScanned
    ∷ EngineEnv → WorldState → CameraFacing → Int → Int → Float
    → IO (Int, V.Vector SortableQuad)
renderStructureQuadsScanned env ws facing zSlice effDepth tileAlpha = do
            td      ← readIORef (wsTilesRef ws)
            let handoff = toRenderHandoffCapability env
            handles ← readIORef (rhTexPaletteHandlesRef handoff)
            palette ← readIORef (rhTexPaletteRef handoff)
            catalog ← readIORef (rhStructureWallCatalogRef handoff)
            -- Chunks that hold no piece at all cost nothing further; the
            -- early-out also keeps a world with no structures off the
            -- camera / view-bounds reads entirely, as before.
            let chunks = [ lc | lc ← HM.elems (wtdChunks td)
                             , not (HM.null (lcStructures lc)) ]
            if null chunks then return (0, V.empty) else do
                let rv = toRenderViewCapability env
                texSizes ← readIORef (rvTextureSizeRef rv)
                camera   ← readIORef (rvCameraRef rv)
                (fbW, fbH) ← readIORef (rvFramebufferSizeRef rv)
                paramsM  ← readIORef (wsGenParamsRef ws)
                mBts ← readIORef (rvTextureSystemRef rv)
                case mBts of
                    Nothing → return (0, V.empty)
                    Just _bts →
                        -- Bake stable handle ids (tile + its own face map);
                        -- resolved to live slots in the shader (#286).
                        let lookupSlot h = fromIntegral (toInt h) ∷ Word32
                            worldSize = maybe 128 wgpWorldSize paramsM
                            vb = computeViewBounds camera fbW fbH effDepth
                            (camX, camY) = camPosition camera
                            (scanned, quads) =
                                structureChunkQuadsScanned catalog palette
                                    handles lookupSlot texSizes facing zSlice
                                    effDepth tileAlpha worldSize vb camX camY
                                    chunks
                        in return (scanned, V.fromList quads)

-- | The seam-aware per-chunk half of the pass, pure so a headless spec can
--   drive the real geometry (the IO pass above deliberately emits nothing
--   until the texture system exists, and headless never has one).
--
--   ONE decision per chunk, taken from the chunk's OWN 'lcCoord' — the
--   owner 'HM.elems' used to discard while flattening straight to pieces.
--   'isChunkVisibleWrapped' judges visibility against bounds already
--   translated by the offset it returns, so a chunk is drawn at exactly
--   the alias that made it visible and is skipped entirely otherwise; a
--   piece can never be emitted through two aliases, because only one is
--   ever chosen.
--
--   The offset is applied as a pure SCREEN translation of the emitted
--   vertices — never fed into the producers' grid arithmetic — so UVs,
--   @qpWorldUV@, texture and facemap payloads, layers and above all
--   'sqSortKey' are exactly what they were. Painter depth stays
--   grid-derived (that is what keeps 'World.Render.Quads.structureFrontWallClear'
--   reproducible), and the @(0, 0)@ case skips the translation outright so
--   an interior chunk's vertices are bit-identical, not merely equal.
structureChunkQuads
    ∷ StructureWallCatalog
    → TexPalette
    → HM.HashMap Int TextureHandle             -- ^ palette id → runtime handle
    → (TextureHandle → Word32)                 -- ^ handle → bindless slot id
    → HM.HashMap TextureHandle (Int, Int)      -- ^ texture pixel sizes
    → CameraFacing → Int → Int → Float
    → Int                                      -- ^ world size in chunks
    → ViewBounds
    → Float → Float                            -- ^ camera screen position
    → [LoadedChunk]
    → [SortableQuad]
structureChunkQuads catalog palette handles lookupSlot texSizes
                    facing zSlice effDepth tileAlpha worldSize vb
                    camX camY chunks =
    snd (structureChunkQuadsScanned catalog palette handles lookupSlot texSizes
             facing zSlice effDepth tileAlpha worldSize vb camX camY chunks)

-- | 'structureChunkQuads' paired with the scene-assembly telemetry
--   (#1921) it contributes: the structure-PIECE records examined once
--   their chunk has passed the visibility test.
--
--   ONE traversal answers both halves, because requirement 9 forbids
--   instrumentation that allocates in proportion to the sources it
--   counts and a world can legitimately hold one piece per chunk. So
--   there is no shared list of visible chunks to sum over, and no
--   second 'isChunkVisibleWrapped' either: that helper builds corner
--   and candidate lists of its own, so calling it twice per chunk is
--   itself the forbidden allocation. Each chunk's visibility is decided
--   exactly once and its result feeds the count and the quads together.
--
--   The quads stay bit-identical: same chunk order, same
--   'HM.toList' piece order within a chunk, same
--   'structurePieceQuads' order within a piece, built by the same
--   right-nested concatenation a comprehension would produce.
structureChunkQuadsScanned
    ∷ StructureWallCatalog
    → TexPalette
    → HM.HashMap Int TextureHandle             -- ^ palette id → runtime handle
    → (TextureHandle → Word32)                 -- ^ handle → bindless slot id
    → HM.HashMap TextureHandle (Int, Int)      -- ^ texture pixel sizes
    → CameraFacing → Int → Int → Float
    → Int                                      -- ^ world size in chunks
    → ViewBounds
    → Float → Float                            -- ^ camera screen position
    → [LoadedChunk]
    → (Int, [SortableQuad])
structureChunkQuadsScanned catalog palette handles lookupSlot texSizes
                           facing zSlice effDepth tileAlpha worldSize vb
                           camX camY chunks =
    go 0 chunks
  where
    -- The count is a strict argument, so it never builds a thunk chain,
    -- and 'HM.size' is a traversal with no allocation of its own.
    go ∷ Int → [LoadedChunk] → (Int, [SortableQuad])
    go !scanned [] = (scanned, [])
    go !scanned (lc : rest) =
        case isChunkVisibleWrapped facing worldSize vb camX camY (lcCoord lc) of
            Nothing  → go scanned rest
            Just off →
                let (scanned', quads) =
                        go (scanned + HM.size (lcStructures lc)) rest
                in (scanned', chunkQuads off lc ++ quads)

    chunkQuads off lc =
        [ translateQuad off sq
        | ((gx, gy, slotTag), spd) ← HM.toList (lcStructures lc)
        , sq ← structurePieceQuads catalog palette handles lookupSlot texSizes
                   facing zSlice effDepth tileAlpha gx gy
                   (toEnum (fromIntegral slotTag) ∷ StructureSlot) spd
        ]

-- | Move a quad's four vertex POSITIONS by a screen-space offset, leaving
--   every other field — UVs, tint, atlas/facemap slots, flags, packed
--   world UV, texture, layer and sort key — untouched. The identity
--   offset returns the quad itself so nothing is rebuilt away from the
--   seam.
translateQuad ∷ (Float, Float) → SortableQuad → SortableQuad
translateQuad (offX, offY) sq
    | offX ≡ 0 ∧ offY ≡ 0 = sq
    | otherwise = sq { sqV0 = move (sqV0 sq), sqV1 = move (sqV1 sq)
                     , sqV2 = move (sqV2 sq), sqV3 = move (sqV3 sq) }
  where
    move v = let Vec2 x y = pos v in v { pos = Vec2 (x + offX) (y + offY) }

-- | The whole per-piece pipeline, pure: resolve the piece's palette ids to
--   runtime handles, rotate a wall's art onto the sprite its edge is drawn
--   with at this facing, then emit the quad(s) the slot calls for — a
--   corner post, a front wall's depth strips, or a single quad.
--
--   Emits nothing when either palette id has no runtime handle yet (right
--   after a load, before Lua's re-resolve tick): the piece simply draws
--   once its handles exist, which is not an error.
structurePieceQuads
    ∷ StructureWallCatalog
    → TexPalette
    → HM.HashMap Int TextureHandle             -- ^ palette id → runtime handle
    → (TextureHandle → Word32)                 -- ^ handle → bindless slot id
    → HM.HashMap TextureHandle (Int, Int)      -- ^ texture pixel sizes
    → CameraFacing → Int → Int → Float
    → Int → Int → StructureSlot → StructurePieceData
    → [SortableQuad]
structurePieceQuads catalog palette handles lookupSlot texSizes
                    facing zSlice effDepth tileAlpha gx gy slot spd =
    case ( HM.lookup (spdTexId spd) handles
         , HM.lookup (spdFaceId spd) handles ) of
        (Just th, Just fh) →
            let (th', fh') = rotatedArt th fh
                piece = StructurePiece th' fh' (spdGridZ spd)
            in if isPost slot
               then toList $ postToQuad lookupSlot facing zSlice effDepth
                                        tileAlpha gx gy slot piece texSizes
               else if isScreenFrontWall facing slot
               then frontWallStrips lookupSlot facing zSlice effDepth
                                    tileAlpha gx gy slot piece texSizes
               else toList $ structureToQuad lookupSlot facing zSlice effDepth
                                             tileAlpha gx gy slot piece texSizes
        _ → []
  where
    isPost s = s ≡ SPostN ∨ s ≡ SPostE ∨ s ≡ SPostS ∨ s ≡ SPostW
    toList = maybe [] (:[])
    -- A wall's sprite + cap facemap travel together onto the screen edge
    -- its authored edge occupies (#1712). Anything the catalogue does not
    -- recognise — a non-wall slot, art from outside a registered pack, or
    -- a texture/facemap pair that disagrees about the authored edge —
    -- keeps exactly the handles it was placed with.
    rotatedArt th fh = fromMaybe (th, fh) $ do
        edge  ← wallEdgeOfSlot slot
        tPath ← lookupPath (spdTexId spd) palette
        fPath ← lookupPath (spdFaceId spd) palette
        rotatedWallArt catalog facing edge (tPath, th) (fPath, fh)

-- | The active z band a structure piece has to sit in to be emitted at
--   all: at or below the camera's slice, and no deeper than the frame's
--   zoom-derived depth window ('World.Render.Quads' @effectiveDepth@,
--   which ranges 8..250 with the zoom). INCLUSIVE at both ends. Every
--   producer in this module gates on it, so a piece outside the band
--   draws nothing.
--
--   It is named, exported and shared rather than restated because the
--   #418 billboard lift depends on the SAME answer: the lift raises a
--   flora/vegetation sprite over a front wall's whole strip range, so a
--   wall that emits no strips must produce no lift, and a wall that
--   emits strips must stay eligible for one. Before #1715
--   'World.Render.Quads.structureFrontWallClear' could not even ask —
--   it took no effective depth — so a wall above the slice, or deeper
--   than the depth window, still lifted a sprite whose wall was not on
--   screen. Two copies of the comparison could drift apart; one cannot.
pieceWithinSliceBand
    ∷ Int    -- ^ camera z-slice
    → Int    -- ^ frame's effective depth
    → Int    -- ^ the piece's stored grid z
    → Bool
pieceWithinSliceBand zSlice effDepth gridZ =
    gridZ ≤ zSlice ∧ gridZ ≥ zSlice - effDepth

-- | Does this slot's edge sit at the SCREEN front (the SE/SW pair) at
--   this facing, and so take the #415 depth-strip path? At 'FaceSouth'
--   exactly 'SWallSE' and 'SWallSW', as it always was. Also the candidate
--   test 'World.Render.Quads.structureFrontWallClear' searches with, so
--   the #418 billboard lift and the strips cannot disagree about which
--   walls are in front.
isScreenFrontWall ∷ CameraFacing → StructureSlot → Bool
isScreenFrontWall facing slot =
    maybe False (isScreenFrontEdge . screenWallEdge facing) (wallEdgeOfSlot slot)

-- | A non-front-wall structure piece (floor / ceiling / back wall) as a
--   single iso-sorted quad. Front walls go through 'frontWallStrips' instead.
structureToQuad
    ∷ (TextureHandle → Word32)
    → CameraFacing → Int → Int → Float
    → Int → Int → StructureSlot → StructurePiece
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
structureToQuad lookupSlot facing zSlice effDepth tileAlpha gx gy slot piece texSizes =
    let gridZ     = spGridZ piece
        relativeZ = gridZ - zSlice
    in if not (pieceWithinSliceBand zSlice effDepth gridZ)
       then Nothing
       else
        let texHandle = spTexture piece
            (texW, texH) = case HM.lookup texHandle texSizes of
                Just (w, h) → (fromIntegral w, fromIntegral h)
                Nothing     → (baseTileW, baseTileH)

            scaleX = texW / baseTileW
            scaleY = texH / baseTileH
            quadW = tileWidth  * scaleX
            quadH = tileHeight * scaleY

            -- Position EXACTLY like a terrain tile (gridToScreen + drawY =
            -- rawY - heightOffset, full tileWidth×tileHeight quad) so floors,
            -- ceilings and walls align to the grid. (Was on the building
            -- tile_bottom convention, which sat off-grid and clipped.)
            (rawX, rawY) = gridToScreen facing gx gy
            heightOffset = fromIntegral relativeZ * tileSideHeight
            -- Per-slot vertical offset (tileSideHeight=16px → /4 = 4px, the
            -- floor's lift/thickness):
            --   • a wall RISES 4px to sit on TOP of the floor instead of
            --     at ground level (the wall art isn't drawn lifted like the
            --     floor's), so −4px (up on screen). This is the SAME lift
            --     'frontWallStrips' applies, so it is screen-role
            --     INDEPENDENT: all four edges take it, and only the
            --     back pair reaches this function at any given facing.
            --   • a ceiling DROPS ~8px (+0.5·tileSideHeight) to rest on the
            --     wall tops without cutting them off. (The ceiling art's
            --     diamond sits 12px HIGHER in its 96×64 canvas — apex y0 vs the
            --     floor's y12 — so it needs a drop at all; tuned between 12px
            --     which buried the tops and 6px which floated ~2px high.)
            floorLift = tileSideHeight * 0.25
            slotVOffset
                | slot ≡ SCeiling              = tileSideHeight * 0.5
                | isJust (wallEdgeOfSlot slot) = negate floorLift
                | otherwise                    = 0
            drawX = rawX
            drawY = rawY - heightOffset + slotVOffset

            -- Sort by the screen-depth of the piece's ANCHOR, not the sprite
            -- position. Floors/ceilings anchor at the tile centre; a back wall
            -- at the tile centre so it stays behind the floor. applyFacingF
            -- keeps it rotation-aware.
            (saX, saY) = sortAnchor facing slot gx gy
            (faS, fbS) = applyFacingF facing saX saY
            sortKey = (faS + fbS)
                    + fromIntegral relativeZ * 0.001
                    + wallTieBreak facing slot   -- break wall-pair ties + beat terrain ties

            actualSlot = lookupSlot texHandle
            faceSlot   = fromIntegral (lookupSlot (spFaceMap piece))
            tint  = Vec4 1.0 1.0 1.0 tileAlpha
            flags = 0
            wuv   = tileWorldUV gx gy

            (v0, v1, v2, v3) =
                quadVertices (rectCorners (Vec2 drawX drawY) (Vec2 quadW quadH))
                             fullQuadUV
                             QuadPayload
                                 { qpTint      = tint
                                 , qpAtlasSlot = fromIntegral actualSlot
                                 , qpFaceMap   = faceSlot
                                 , qpFlags     = flags
                                 , qpWorldUV   = wuv
                                 }

        in Just SortableQuad
            { sqSortKey = sortKey
            , sqV0 = v0, sqV1 = v1, sqV2 = v2, sqV3 = v3
            , sqTexture = texHandle
            , sqLayer = worldLayer
            }

-- | How many vertical strips a front wall is sliced into. Each strip sits at
--   its own iso-depth along the wall's edge, so the painter's z-term resolves
--   occlusion against terrain per strip (a higher side rim occludes the wall's
--   far/edge strips while its near/south strips still cover their footing).
--   The single-quad billboard collapsed that depth range to one key — the
--   #415 bug. 16 keeps the occlusion boundary smooth; walls are sparse so the
--   extra quads are negligible against the terrain pass.
wallStripCount ∷ Int
wallStripCount = 16

-- | A SCREEN-front wall sliced into 'wallStripCount' vertical strips, each
--   carrying its own depth-sort key (#415). A vertical wall projects so that
--   a given screen-x is a fixed position along its edge — and so a fixed
--   iso-depth — at every height. So strip @i@ (UV-x @[i/k,(i+1)/k]@, full
--   height) sorts at the edge depth of its centre, and terrain at that depth
--   interleaves correctly via the existing z-term. No neighbour height lookup
--   is needed: a low tile in front loses on z (wall covers footing) and a
--   high rim wins (it occludes that strip), each at its own depth.
--
--   Edge → depth map. The strip's UV-x follows the SPRITE it is drawn with
--   — the screen edge — and its sort anchor follows the same screen edge,
--   in the facing-independent depth-step units 'depthAnchor' defines: the
--   end at the sprite's canvas CENTRE is the screen-bottom one and sorts
--   at 'frontWallDepthSteps' (2, the tile's front corner), the outer end
--   one step behind it at 1 (the tile centre's depth). An edge's art runs
--   over only HALF the 96×64 canvas (N/S at UV-x 0.5, W at 0, E at 1), so
--   the along-edge parameter is CLAMPED to [0,1] = its endpoints: the wall
--   art overhangs the canvas-centre vertex by a few px of wall thickness
--   (se bbox x41..95 vs S corner x48), and that sliver must NOT sort past
--   it or it punches forward again. At 'FaceSouth' this reproduces the
--   original two cases exactly — SE: E(gx+1,gy)→S(gx+1,gy+1),
--   SW: W(gx,gy+1)→S(gx+1,gy+1) — key for key. Strips outside the art's UV
--   range are transparent.
frontWallStrips
    ∷ (TextureHandle → Word32)
    → CameraFacing → Int → Int → Float
    → Int → Int → StructureSlot → StructurePiece
    → HM.HashMap TextureHandle (Int, Int)
    → [SortableQuad]
frontWallStrips lookupSlot facing zSlice effDepth tileAlpha gx gy slot piece texSizes =
    let gridZ     = spGridZ piece
        relativeZ = gridZ - zSlice
    in if not (pieceWithinSliceBand zSlice effDepth gridZ)
       then []
       else
        let texHandle = spTexture piece
            (texW, texH) = case HM.lookup texHandle texSizes of
                Just (w, h) → (fromIntegral w, fromIntegral h)
                Nothing     → (baseTileW, baseTileH)
            scaleX = texW / baseTileW
            scaleY = texH / baseTileH
            quadW = tileWidth  * scaleX
            quadH = tileHeight * scaleY

            (rawX, rawY) = gridToScreen facing gx gy
            heightOffset = fromIntegral relativeZ * tileSideHeight
            -- walls RISE 4px to sit on top of the floor (see structureToQuad)
            floorLift = tileSideHeight * 0.25
            drawX = rawX
            drawY = rawY - heightOffset - floorLift

            actualSlot = lookupSlot texHandle
            faceSlot   = fromIntegral (lookupSlot (spFaceMap piece))
            tint  = Vec4 1.0 1.0 1.0 tileAlpha
            flags = 0
            wuv   = tileWorldUV gx gy

            -- The screen edge this wall is drawn on and its art's canvas-x
            -- span. The end at canvas CENTRE (u 0.5) is the screen-bottom
            -- one, so it takes the deepest step; the outer end takes 1.
            screen     = screenWallEdge facing (fromMaybe WallSE (wallEdgeOfSlot slot))
            (u0, u1)   = wallEdgeCanvasSpan screen
            centreAtU1 = u1 ≡ 0.5   -- NW/SW: canvas-RIGHT end is the centre
            (kL, kR)   = if centreAtU1
                         then (1.0, fromIntegral frontWallDepthSteps)
                         else (fromIntegral frontWallDepthSteps, 1.0)
            anchorAt u =
                let t = clamp01 ((u - u0) / (u1 - u0))
                in depthAnchor facing (kL + t * (kR - kL)) gx gy

            k = wallStripCount
            strip i =
                let ua = fromIntegral i       / fromIntegral k
                    ub = fromIntegral (i + 1) / fromIntegral k
                    uc = (ua + ub) * 0.5
                    xa = drawX + ua * quadW
                    xb = drawX + ub * quadW
                    (saX, saY) = anchorAt uc
                    (faS, fbS) = applyFacingF facing saX saY
                    sortKey = (faS + fbS)
                            + fromIntegral relativeZ * 0.001
                            + wallTieBreak facing slot
                    -- The strip's positions and its U range move
                    -- together: [xa,xb] of the sprite's width samples
                    -- exactly [ua,ub] of its texture.
                    (v0, v1, v2, v3) =
                        quadVertices
                            QuadCorners
                                { qcTopLeft     = Vec2 xa drawY
                                , qcTopRight    = Vec2 xb drawY
                                , qcBottomRight = Vec2 xb (drawY + quadH)
                                , qcBottomLeft  = Vec2 xa (drawY + quadH)
                                }
                            QuadUV { quLeftU   = ua
                                   , quTopV    = 0
                                   , quRightU  = ub
                                   , quBottomV = 1
                                   }
                            QuadPayload
                                { qpTint      = tint
                                , qpAtlasSlot = fromIntegral actualSlot
                                , qpFaceMap   = faceSlot
                                , qpFlags     = flags
                                , qpWorldUV   = wuv
                                }
                in SortableQuad
                    { sqSortKey = sortKey
                    , sqV0 = v0, sqV1 = v1, sqV2 = v2, sqV3 = v3
                    , sqTexture = texHandle
                    , sqLayer = worldLayer
                    }
        in [ strip i | i ← [0 .. k - 1] ]

-- Inset of each corner post toward its own tile centre. 0 = post sits on the
-- EXACT floor corner. Tuning knob (re-add once single-tile corners are
-- confirmed, to form the touching 2×2 where four floors meet at a node).
postInset ∷ Float
postInset = 0.0

-- | A corner POST: a small sprite at one of the tile's four VERTICES
--   (N/E/S/W of the diamond), inset toward the centre, capping the wall
--   junction at that corner. Positioned at the vertex its authored corner
--   OCCUPIES at this facing (#1712 — a post keeps standing on its own
--   physical corner as the camera turns); sorted to match the walls it
--   caps (NOT the vertex, since back walls sort at the tile centre while
--   front walls sort at the front corner). No bespoke facemap → its own
--   facemap slot.
postToQuad
    ∷ (TextureHandle → Word32)
    → CameraFacing → Int → Int → Float
    → Int → Int → StructureSlot → StructurePiece
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
postToQuad lookupSlot facing zSlice effDepth tileAlpha gx gy slot piece texSizes =
    let gridZ     = spGridZ piece
        relativeZ = gridZ - zSlice
    in if not (pieceWithinSliceBand zSlice effDepth gridZ)
       then Nothing
       else
        let texHandle = spTexture piece
            (texW, texH) = case HM.lookup texHandle texSizes of
                Just (w, h) → (fromIntegral w, fromIntegral h)
                Nothing     → (32, 32)
            quadW = tileWidth  * (texW / baseTileW)
            quadH = tileHeight * (texH / baseTileH)
            heightOffset = fromIntegral relativeZ * tileSideHeight
            -- The post column's base ellipse sits at ~81% down the sprite
            -- (32px tall, base at y≈26). Anchor THAT onto the tile corner
            -- rather than guessing a drop, so the column stands ON the corner.
            postBaseAnchorY = 0.78   -- ~1px lower than the column base ellipse

            -- Position EXACTLY like a floor: gridToScreen + drawY = rawY -
            -- heightOffset (same as structureToQuad), then drop the post's
            -- base onto one of the floor diamond's four corners (offsets
            -- measured from the floor art — see postCornerOffset).
            (rawX, rawY) = gridToScreen facing gx gy
            tileTopY     = rawY - heightOffset
            (cornOffX, cornOffY) = postCornerOffset facing slot
            cornX = rawX + cornOffX
            cornY = tileTopY + cornOffY
            -- inset toward the floor diamond's real centre (48,35 in the
            -- sprite) by `postInset` screen px
            (ctrOffX, ctrOffY) = postDiamondCentre
            ctrX = rawX + ctrOffX
            ctrY = tileTopY + ctrOffY
            ddx = ctrX - cornX ; ddy = ctrY - cornY
            dlen = sqrt (ddx*ddx + ddy*ddy)
            (ux, uy) = if dlen > 0 then (ddx/dlen, ddy/dlen) else (0, 0)
            baseX = cornX + ux * postInset
            baseY = cornY + uy * postInset
            -- anchor the column's base ellipse (~81% down the sprite) on it
            drawX = baseX - quadW * 0.5
            drawY = baseY - quadH * postBaseAnchorY

            -- Sort relative to the walls this corner caps. The bias is PER
            -- SCREEN CORNER because back and front need opposite treatment
            -- (see postBias): the back post nestles BEHIND its back walls,
            -- the front post stands IN FRONT of its front walls. All biases
            -- stay above the floor (0.0002) so the post sits on the floor, and
            -- below the z-step (0.001) so they never cross a level.
            (psX, psY) = postSortAnchor facing slot gx gy
            (faS, fbS) = applyFacingF facing psX psY
            sortKey = (faS + fbS) + fromIntegral relativeZ * 0.001 + postBias facing slot

            actualSlot = lookupSlot texHandle
            faceSlot   = fromIntegral (lookupSlot (spFaceMap piece))  -- postface
            tint  = Vec4 1.0 1.0 1.0 tileAlpha
            flags = 0
            wuv   = tileWorldUV gx gy
            (v0, v1, v2, v3) =
                quadVertices (rectCorners (Vec2 drawX drawY) (Vec2 quadW quadH))
                             fullQuadUV
                             QuadPayload
                                 { qpTint      = tint
                                 , qpAtlasSlot = fromIntegral actualSlot
                                 , qpFaceMap   = faceSlot
                                 , qpFlags     = flags
                                 , qpWorldUV   = wuv
                                 }
        in Just SortableQuad
            { sqSortKey = sortKey
            , sqV0 = v0, sqV1 = v1, sqV2 = v2, sqV3 = v3
            , sqTexture = texHandle
            , sqLayer = worldLayer
            }

-- | World units per source pixel (1/96 of a tile; same for X and Y since
--   tileHeight/64 == tileWidth/96).
postPx ∷ Float
postPx = tileWidth / baseTileW

-- | Base position of each corner post within the 96×64 floor sprite, in PIXELS
--   — keyed by the SCREEN corner the post's authored corner occupies at this
--   facing, since these offsets are canvas positions (#1712).
--   The floor diamond (48px tall, drawn 12px down the canvas for the 4px lift)
--   has vertices apex(48,12) E(96,36) S(48,60) W(0,36). Each post is then inset
--   toward the centre by HALF its own footprint *along that axis* — the post is
--   14px wide × 7px tall in iso (2:1), so N/S inset 3.5px vertically and E/W
--   inset 7px horizontally. That keeps each post's footprint edge on the vertex
--   (no overhang) and, where four floors meet at a node, makes the four
--   footprints meet edge-to-edge → the touching 2×2. Result: N–S 41px apart,
--   E–W 82px apart.
postCornerOffset ∷ CameraFacing → StructureSlot → (Float, Float)
postCornerOffset facing slot = case screenPostCorner facing <$> postCornerOfSlot slot of
    Just CornerN → (48 * postPx, 15.5 * postPx)   -- apex y12 + 3.5
    Just CornerE → (89 * postPx, 36   * postPx)   -- E    x96 − 7
    Just CornerS → (48 * postPx, 56.5 * postPx)   -- S    y60 − 3.5
    Just CornerW → ( 7 * postPx, 36   * postPx)   -- W    x0  + 7
    Nothing      → (48 * postPx, 36   * postPx)   -- diamond centre

-- | The floor diamond's centre within the sprite (px).
postDiamondCentre ∷ (Float, Float)
postDiamondCentre = (48 * postPx, 36 * postPx)

-- | Per-corner sort bias (all in (floor 0.0002 .. z-step 0.001)), keyed by the
--   post's SCREEN corner. A post fills its walls' notch; whether it draws in
--   front of or behind those walls depends on which screen corner it is:
--     • the BACK (screen-N) post → BEHIND its back walls (tie-breaks
--       0.0003/0.0004), so bias below those → 0.00025 (still above the floor).
--     • the FRONT (screen-S) post → IN FRONT of its front walls (tie-breaks
--       0.0005/0.0006), so bias above those → 0.00065.
--     • the screen-E/W posts cap a back wall (depth d) AND a front wall (d+1);
--       their own anchor depth already lands between the two, so the bias only
--       needs to beat the floor — 0.00045.
postBias ∷ CameraFacing → StructureSlot → Float
postBias facing slot = case screenPostCorner facing <$> postCornerOfSlot slot of
    Just CornerN → 0.00025
    Just CornerS → 0.00065
    Just CornerE → 0.00045
    Just CornerW → 0.00045
    Nothing      → 0.00025

-- | Depth-sort anchor for a post, in 'depthAnchor' steps and keyed by the
--   post's SCREEN corner — it matches the walls it caps, and the back and
--   front walls sort one step apart. The screen-E/W posts cap one of each,
--   so they sit half a step between them. At 'FaceSouth' these reproduce
--   the original (gx+.5,gy+.5) / (gx+1,gy+1) / (gx+1,gy+.5) / (gx+.5,gy+1)
--   key for key.
postSortAnchor ∷ CameraFacing → StructureSlot → Int → Int → (Float, Float)
postSortAnchor facing slot gx gy =
    depthAnchor facing k gx gy
  where
    k = case screenPostCorner facing <$> postCornerOfSlot slot of
        Just CornerN → 1.0   -- over the back walls (the tile centre's depth)
        Just CornerS → fromIntegral frontWallDepthSteps   -- over the front walls
        Just CornerE → 1.5   -- between a back wall and a front wall
        Just CornerW → 1.5
        Nothing      → 1.0

-- | Painter-depth steps in front of a tile's own grid origin that a front
--   wall's DEEPEST strip sorts at — the tile's front corner, two steps
--   ahead of the terrain quad keyed at that origin. Facing-INDEPENDENT by
--   construction (see 'depthAnchor'), which is the whole point: it is the
--   same two steps at every facing. 'World.Render.Quads.structureFrontWallClear'
--   reproduces a wall's maximum strip key from it, so the #418 lift and the
--   #415 strips cannot disagree about how far forward a front wall reaches.
frontWallDepthSteps ∷ Int
frontWallDepthSteps = 2

-- | A grid-space sort anchor @k@ painter-depth steps in front of the
--   tile's own grid ORIGIN — the point every other world pass keys that
--   tile from ('World.Render.TileQuads', 'World.Render.FloraQuads' and
--   friends all sort at @applyFacing facing gx gy@).
--
--   One depth step is @unapplyFacingF facing 0.5 0.5@, so by construction
--   @applyFacingF@ of the result sums to the tile's own depth plus exactly
--   @k@ — at EVERY facing. That invariance is the reason this exists: the
--   obvious alternative, a fixed grid offset like the tile centre run
--   through 'applyFacingF', changes its depth gap as the camera turns
--   (+1 at 'FaceSouth', 0 at 'FaceWest'/'FaceEast', −1 at 'FaceNorth'),
--   which sank a rotated floor beneath its own terrain tile.
--
--   At 'FaceSouth' it is literally the familiar geometry: @k = 1@ is the
--   tile centre @(gx+0.5, gy+0.5)@ and @k = 2@ the front corner
--   @(gx+1, gy+1)@.
depthAnchor ∷ CameraFacing → Float → Int → Int → (Float, Float)
depthAnchor facing k gx gy =
    let (dx, dy) = unapplyFacingF facing 0.5 0.5
    in (fromIntegral gx + k * dx, fromIntegral gy + k * dy)

-- | Grid-space anchor used for DEPTH SORTING (not rendering) of non-front
--   pieces, in 'depthAnchor' steps. Floors and BACK walls sit at the tile
--   centre's depth (1 step), which is what keeps a back wall behind the
--   floor and both in front of their own terrain tile; the ceiling at the
--   front corner (2 steps) so its +1 z beats the walls beneath and it draws
--   OVER them. Both are the same number of steps at every facing (#1712).
--   (Front walls are sorted per-strip in 'frontWallStrips', not here.)
sortAnchor ∷ CameraFacing → StructureSlot → Int → Int → (Float, Float)
sortAnchor facing slot gx gy = case slot of
    SCeiling → depthAnchor facing (fromIntegral frontWallDepthSteps) gx gy
    _        → depthAnchor facing 1.0 gx gy

-- | Tiny per-slot offset (all < the 0.001 z-step, so they never cross a
--   z-level). Two purposes: break exact sort TIES between a wall pair that
--   share an anchor — the front pair would z-flicker on zoom, the back pair
--   renders an unstable corner joint — and beat exact ties with the terrain
--   tile underneath. Higher = drawn later = on top; within each corner the
--   higher value's wall overlaps the other. Flip the two numbers in a pair if
--   a joint's overlap reads backwards.
--
--   A wall's value is keyed by its SCREEN edge (#1712), so the pairing is
--   always between the two walls that actually share a screen corner, and the
--   front pair always outranks the back pair. At 'FaceSouth' this is the
--   original per-slot table. Exported because
--   'World.Render.Quads.structureFrontWallClear' has to reproduce a front
--   wall's exact strip key.
wallTieBreak ∷ CameraFacing → StructureSlot → Float
wallTieBreak facing s = case wallEdgeOfSlot s of
    Just e  → screenEdgeTieBreak (screenWallEdge facing e)
    Nothing → case s of
        SFloor   → 0.00020
        SWire    → 0.00015   -- flat like a floor, but tie-loses to one if both
                             -- ever occupy the same tile
        SCeiling → 0.00070
        _        → 0.0       -- posts compute their own sort in postToQuad
  where
    screenEdgeTieBreak e = case e of
        WallNW → 0.00030
        WallNE → 0.00040   -- NE overlaps NW at the back corner
        WallSW → 0.00050
        WallSE → 0.00060   -- SE overlaps SW at the front corner
