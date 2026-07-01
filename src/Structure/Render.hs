{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Render structural pieces (walls / floors / ceilings) as iso-sorted
--   quads. Closely mirrors "Building.Render": each piece is a full-tile
--   96×64 sprite drawn at its tile, but with the piece's OWN facemap slot
--   (wall sun-shading) and a per-slot sort key so near-edge walls draw in
--   front and far-edge walls behind.
--
--   FRONT walls (SE/SW) are an exception: a single billboard quad collapses
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
--   Rotation: sort anchors are world-space grid coords rotated by applyFacingF
--   (depth stays correct at any facing), but the per-direction SPRITE is not
--   yet remapped on rotation — a wall keeps its authored face. That remap is
--   a separate follow-up.
module Structure.Render
    ( renderStructureQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import World.Grid (tileWidth
                   , tileHeight
                   , tileSideHeight
                   , worldLayer
                   , GridConfig(..)
                   , defaultGridConfig
                   , applyFacingF
                   , gridToScreen)
import World.Types (WorldState, wsTilesRef)
import World.Tile.Types (WorldTileData(..))
import World.Chunk.Types (LoadedChunk(..))
import Structure.Types

baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)
baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)

-- | Gather structure quads for ONE world's state. The caller iterates the
--   visible-world list (same source of truth as the terrain / ground-item /
--   spoil passes) rather than this picking a world itself — historically it
--   grabbed the head of @wmWorlds@ and could render a hidden world's
--   structures over the visible one (#72).
renderStructureQuads ∷ EngineEnv → WorldState → CameraFacing → Int → Int → Float
                     → IO (V.Vector SortableQuad)
renderStructureQuads env ws facing zSlice effDepth tileAlpha = do
            td      ← readIORef (wsTilesRef ws)
            handles ← readIORef (texPaletteHandlesRef env)
            -- Gather structures across ALL loaded chunks' overlays, resolving
            -- each piece's texture/facemap PALETTE IDs → runtime handles. A
            -- piece whose id isn't resolved yet (e.g. just after a load,
            -- before the Lua re-resolve) is skipped — it draws once resolved.
            let pieces =
                    [ ( gx, gy, toEnum (fromIntegral slotTag) ∷ StructureSlot
                      , StructurePiece th fh (spdGridZ spd) )
                    | lc ← HM.elems (wtdChunks td)
                    , ((gx, gy, slotTag), spd) ← HM.toList (lcStructures lc)
                    , Just th ← [HM.lookup (spdTexId spd) handles]
                    , Just fh ← [HM.lookup (spdFaceId spd) handles] ]
            if null pieces then return V.empty else do
                texSizes ← readIORef (textureSizeRef env)
                mBts ← readIORef (textureSystemRef env)
                case mBts of
                    Nothing → return V.empty
                    Just _bts → do
                        -- Bake stable handle ids (tile + its own face map);
                        -- resolved to live slots in the shader (#286).
                        let lookupSlot h = fromIntegral (toInt h) ∷ Word32
                            defFmSlot = -1 ∷ Float  -- posts ignore this
                            -- Corner posts anchor at a tile VERTEX (small sprite,
                            -- inset toward the centre); the rest at the tile face.
                            isPost s = s ≡ SPostN ∨ s ≡ SPostE ∨ s ≡ SPostS ∨ s ≡ SPostW
                            -- Front walls slice into depth-sorted strips (#415);
                            -- everything else is a single quad.
                            isFrontWall s = s ≡ SWallSE ∨ s ≡ SWallSW
                            toList = maybe [] (:[])
                            mkQuads gx gy slot piece
                                | isPost slot      = toList $ postToQuad lookupSlot defFmSlot
                                        facing zSlice effDepth tileAlpha gx gy slot piece texSizes
                                | isFrontWall slot = frontWallStrips lookupSlot facing zSlice
                                        effDepth tileAlpha gx gy slot piece texSizes
                                | otherwise        = toList $ structureToQuad lookupSlot facing
                                        zSlice effDepth tileAlpha gx gy slot piece texSizes
                            quads = V.fromList
                                [ sq
                                | (gx, gy, slot, piece) ← pieces
                                , sq ← mkQuads gx gy slot piece ]
                        return quads

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
    in if gridZ > zSlice ∨ gridZ < (zSlice - effDepth)
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
            --   • a back wall RISES 4px to sit on TOP of the floor instead of
            --     at ground level (the wall art isn't drawn lifted like the
            --     floor's), so −4px (up on screen). (Front walls do the same in
            --     'frontWallStrips'.)
            --   • a ceiling DROPS ~8px (+0.5·tileSideHeight) to rest on the
            --     wall tops without cutting them off. (The ceiling art's
            --     diamond sits 12px HIGHER in its 96×64 canvas — apex y0 vs the
            --     floor's y12 — so it needs a drop at all; tuned between 12px
            --     which buried the tops and 6px which floated ~2px high.)
            floorLift = tileSideHeight * 0.25
            slotVOffset = case slot of
                SCeiling → tileSideHeight * 0.5
                SWallNE  → negate floorLift
                SWallNW  → negate floorLift
                _        → 0
            drawX = rawX
            drawY = rawY - heightOffset + slotVOffset

            -- Sort by the screen-depth of the piece's ANCHOR, not the sprite
            -- position. Floors/ceilings anchor at the tile centre; a back wall
            -- at the tile centre so it stays behind the floor. applyFacingF
            -- keeps it rotation-aware.
            (saX, saY) = sortAnchor slot gx gy
            (faS, fbS) = applyFacingF facing saX saY
            sortKey = (faS + fbS)
                    + fromIntegral relativeZ * 0.001
                    + tieBreak slot   -- break wall-pair ties + beat terrain ties

            actualSlot = lookupSlot texHandle
            faceSlot   = fromIntegral (lookupSlot (spFaceMap piece))
            tint  = Vec4 1.0 1.0 1.0 tileAlpha
            flags = 0

            v0 = Vertex (Vec2 drawX drawY)
                         (Vec2 0 0) tint (fromIntegral actualSlot) faceSlot flags
            v1 = Vertex (Vec2 (drawX + quadW) drawY)
                         (Vec2 1 0) tint (fromIntegral actualSlot) faceSlot flags
            v2 = Vertex (Vec2 (drawX + quadW) (drawY + quadH))
                         (Vec2 1 1) tint (fromIntegral actualSlot) faceSlot flags
            v3 = Vertex (Vec2 drawX (drawY + quadH))
                         (Vec2 0 1) tint (fromIntegral actualSlot) faceSlot flags

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

-- | A FRONT wall (SE/SW) sliced into 'wallStripCount' vertical strips, each
--   carrying its own depth-sort key (#415). A vertical wall projects so that
--   a given screen-x is a fixed position along its edge — and so a fixed
--   iso-depth — at every height. So strip @i@ (UV-x @[i/k,(i+1)/k]@, full
--   height) sorts at the edge depth of its centre, and terrain at that depth
--   interleaves correctly via the existing z-term. No neighbour height lookup
--   is needed: a low tile in front loses on z (wall covers footing) and a
--   high rim wins (it occludes that strip), each at its own depth.
--
--   Edge → depth map (from the wall art, both sprites 96×64). The edge runs
--   only over the S half of the sprite (S corner at UV-x 0.5), so the param
--   is clamped to [0,1] = its endpoints; art overhanging the S vertex must
--   sort AT it (depth gx+gy+2), not past it:
--     • SE: edge E(gx+1,gy)→S(gx+1,gy+1); depth = (gx+gy+1) + clamp01(2·(1−u)).
--     • SW: edge W(gx,gy+1)→S(gx+1,gy+1); depth = (gx+gy+1) + clamp01(2·u).
--   (applyFacingF rotates the grid anchor, so the depth stays correct at any
--   facing.) Strips outside the art's UV range are transparent.
frontWallStrips
    ∷ (TextureHandle → Word32)
    → CameraFacing → Int → Int → Float
    → Int → Int → StructureSlot → StructurePiece
    → HM.HashMap TextureHandle (Int, Int)
    → [SortableQuad]
frontWallStrips lookupSlot facing zSlice effDepth tileAlpha gx gy slot piece texSizes =
    let gridZ     = spGridZ piece
        relativeZ = gridZ - zSlice
    in if gridZ > zSlice ∨ gridZ < (zSlice - effDepth)
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

            gxf = fromIntegral gx ∷ Float
            gyf = fromIntegral gy ∷ Float
            -- grid edge position (→ sort depth) at UV-x u, per direction. The
            -- edge param is CLAMPED to [0,1] (edge endpoints E/W .. S): the
            -- wall art overhangs the S vertex by a few px of wall thickness
            -- (se bbox x41..95 vs S corner x48), and that sliver must NOT sort
            -- PAST the S vertex (depth gx+gy+2) or it punches forward again.
            anchorAt u = case slot of
                SWallSE → (gxf + 1.0,                gyf + clamp01 (2.0 * (1.0 - u)))
                _       → (gxf + clamp01 (2.0 * u),  gyf + 1.0)        -- SWallSW

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
                            + tieBreak slot
                    v0 = Vertex (Vec2 xa drawY)
                                 (Vec2 ua 0) tint (fromIntegral actualSlot) faceSlot flags
                    v1 = Vertex (Vec2 xb drawY)
                                 (Vec2 ub 0) tint (fromIntegral actualSlot) faceSlot flags
                    v2 = Vertex (Vec2 xb (drawY + quadH))
                                 (Vec2 ub 1) tint (fromIntegral actualSlot) faceSlot flags
                    v3 = Vertex (Vec2 xa (drawY + quadH))
                                 (Vec2 ua 1) tint (fromIntegral actualSlot) faceSlot flags
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
--   junction at that corner. Positioned at its inset vertex; sorted to
--   match the walls it caps (NOT the vertex, since back walls sort at the
--   tile centre while front walls sort at the front corner). No bespoke
--   facemap → its own facemap slot.
postToQuad
    ∷ (TextureHandle → Word32)
    → Float
    → CameraFacing → Int → Int → Float
    → Int → Int → StructureSlot → StructurePiece
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
postToQuad lookupSlot _defFmSlot facing zSlice effDepth tileAlpha gx gy slot piece texSizes =
    let gridZ     = spGridZ piece
        relativeZ = gridZ - zSlice
    in if gridZ > zSlice ∨ gridZ < (zSlice - effDepth)
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
            (cornOffX, cornOffY) = postCornerOffset slot
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
            -- CORNER because back and front need opposite treatment (see
            -- postBias): the back (N) post nestles BEHIND its NE/NW walls, the
            -- front (S) post stands IN FRONT of its SE/SW walls. All biases
            -- stay above the floor (0.0002) so the post sits on the floor, and
            -- below the z-step (0.001) so they never cross a level.
            (psX, psY) = postSortAnchor slot gx gy
            (faS, fbS) = applyFacingF facing psX psY
            sortKey = (faS + fbS) + fromIntegral relativeZ * 0.001 + postBias slot

            actualSlot = lookupSlot texHandle
            faceSlot   = fromIntegral (lookupSlot (spFaceMap piece))  -- postface
            tint  = Vec4 1.0 1.0 1.0 tileAlpha
            flags = 0
            v0 = Vertex (Vec2 drawX drawY)
                         (Vec2 0 0) tint (fromIntegral actualSlot) faceSlot flags
            v1 = Vertex (Vec2 (drawX + quadW) drawY)
                         (Vec2 1 0) tint (fromIntegral actualSlot) faceSlot flags
            v2 = Vertex (Vec2 (drawX + quadW) (drawY + quadH))
                         (Vec2 1 1) tint (fromIntegral actualSlot) faceSlot flags
            v3 = Vertex (Vec2 drawX (drawY + quadH))
                         (Vec2 0 1) tint (fromIntegral actualSlot) faceSlot flags
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

-- | Base position of each corner post within the 96×64 floor sprite, in PIXELS.
--   The floor diamond (48px tall, drawn 12px down the canvas for the 4px lift)
--   has vertices apex(48,12) E(96,36) S(48,60) W(0,36). Each post is then inset
--   toward the centre by HALF its own footprint *along that axis* — the post is
--   14px wide × 7px tall in iso (2:1), so N/S inset 3.5px vertically and E/W
--   inset 7px horizontally. That keeps each post's footprint edge on the vertex
--   (no overhang) and, where four floors meet at a node, makes the four
--   footprints meet edge-to-edge → the touching 2×2. Result: N–S 41px apart,
--   E–W 82px apart.
postCornerOffset ∷ StructureSlot → (Float, Float)
postCornerOffset slot = case slot of
    SPostN → (48 * postPx, 15.5 * postPx)   -- apex y12 + 3.5
    SPostE → (89 * postPx, 36   * postPx)   -- E    x96 − 7
    SPostS → (48 * postPx, 56.5 * postPx)   -- S    y60 − 3.5
    SPostW → ( 7 * postPx, 36   * postPx)   -- W    x0  + 7
    _      → (48 * postPx, 36   * postPx)   -- diamond centre

-- | The floor diamond's centre within the sprite (px).
postDiamondCentre ∷ (Float, Float)
postDiamondCentre = (48 * postPx, 36 * postPx)

-- | Per-corner sort bias (all in (floor 0.0002 .. z-step 0.001)). A post fills
--   its walls' notch; whether it draws in front of or behind those walls
--   depends on which corner it is:
--     • N (back) post → BEHIND its NE/NW walls (tieBreak 0.0003/0.0004), so
--       bias below those → 0.00025 (still above the floor).
--     • S (front) post → IN FRONT of its SE/SW walls (tieBreak 0.0005/0.0006),
--       so bias above those → 0.00065.
--     • E/W posts cap a back wall (depth d) AND a front wall (d+1); their own
--       anchor depth already lands between the two, so the bias only needs to
--       beat the floor — 0.00045.
postBias ∷ StructureSlot → Float
postBias s = case s of
    SPostN → 0.00025
    SPostS → 0.00065
    SPostE → 0.00045
    SPostW → 0.00045
    _      → 0.00025

-- | Depth-sort anchor for a post — matches the walls it caps (back walls
--   sort at the tile centre, front walls at the front corner).
postSortAnchor ∷ StructureSlot → Int → Int → (Float, Float)
postSortAnchor slot gx gy =
    let gxf = fromIntegral gx; gyf = fromIntegral gy
    in case slot of
        SPostN → (gxf + 0.5, gyf + 0.5)   -- over the back (NE/NW) walls
        SPostS → (gxf + 1.0, gyf + 1.0)   -- over the front (SE/SW) walls
        SPostE → (gxf + 1.0, gyf + 0.5)
        SPostW → (gxf + 0.5, gyf + 1.0)
        _      → (gxf + 0.5, gyf + 0.5)

-- | Grid-space anchor used for DEPTH SORTING (not rendering) of non-front
--   pieces. The BACK walls (NE/NW) anchor at the tile centre so they stay
--   BEHIND the floor; floors anchor at the tile centre; the ceiling at the
--   front corner so its +1 z beats the walls beneath and it draws OVER them.
--   (Front walls are sorted per-strip in 'frontWallStrips', not here.)
--   applyFacingF keeps these rotation-aware.
sortAnchor ∷ StructureSlot → Int → Int → (Float, Float)
sortAnchor slot gx gy =
    let gxf = fromIntegral gx
        gyf = fromIntegral gy
    in case slot of
        SWallNW → (gxf + 0.5, gyf + 0.5)   -- back walls anchor at centre so
        SWallNE → (gxf + 0.5, gyf + 0.5)   --   they draw OVER the floor
        SCeiling → (gxf + 1.0, gyf + 1.0)  -- front corner: its +1 z then beats
                                           --   the walls beneath so the ceiling
                                           --   draws OVER all of them
        _       → (gxf + 0.5, gyf + 0.5)   -- floor: tile centre
                                           --   (front walls handled elsewhere)

-- | Tiny per-slot offset (all < the 0.001 z-step, so they never cross a
--   z-level). Two purposes: break exact sort TIES between a wall pair that
--   share an anchor — the front pair (SE/SW) would z-flicker on zoom, the
--   back pair (NE/NW) renders an unstable corner joint — and beat exact
--   ties with the terrain tile underneath. Higher = drawn later = on top;
--   within each corner the higher value's wall overlaps the other. Flip the
--   two numbers in a pair if a joint's overlap reads backwards.
tieBreak ∷ StructureSlot → Float
tieBreak s = case s of
    SFloor   → 0.00020
    SWallNW  → 0.00030
    SWallNE  → 0.00040   -- NE overlaps NW at the back corner
    SWallSW  → 0.00050
    SWallSE  → 0.00060   -- SE overlaps SW at the front corner
    SCeiling → 0.00070
    _        → 0.0       -- posts compute their own sort in postToQuad