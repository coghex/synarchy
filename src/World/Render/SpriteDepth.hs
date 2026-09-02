{-# LANGUAGE Strict #-}
-- | Where a world SPRITE finally sorts (#418, shared out by #1856).
--
-- A flora or vegetation billboard standing in front of a structure's
-- front wall is lifted to draw over it. That lift is part of the
-- sprite's FINAL painter depth, so anything that has to agree with
-- what the renderer drew has to apply it too — not just the render
-- pass, but Chop's screen-space selection oracle
-- ("World.Flora.HitTest") and the designation marker anchored to the
-- sprite it selected. A picker that read the unlifted key would call a
-- lifted tree "behind" a sprite the renderer painted under it, and a
-- marker placed off the unlifted key would sink behind its own tree.
--
-- So the lift lives here, applied through one function all three call,
-- and built once per frame by 'frameFrontWallLift' from the frame's own
-- loaded chunks.
module World.Render.SpriteDepth
    ( FrontWallLift(..)
    , noFrontWallLift
    , frameFrontWallLift
    , liftSpriteSortKey
    , frontWallEpsilon
    , structureFrontWallClear
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Render
  (isScreenFrontWall, wallTieBreak, frontWallDepthSteps, pieceWithinSliceBand)
import Structure.Types (StructureSlot(..), ChunkStructures, spdGridZ)
import World.Chunk.Types (ChunkCoord, LoadedChunk(..), chunkSeamChebyshev)
import World.Generate.Coordinates (canonicalTileFrame)
import World.Grid (applyFacing)
import World.Render.ChunkLookup (canonicalChunkLookup)

-- | How far past a cleared front wall a lifted sprite sorts. The whole
--   scene separates its sprite classes with steps in this band —
--   terrain 0.0, flora +0.0003, cursor +0.0004, fluid +0.0005 — and
--   this is the one the lift itself uses.
frontWallEpsilon ∷ Float
frontWallEpsilon = 0.0001

-- | The frame's front-wall lift: a cheap per-chunk gate plus the
--   per-tile clearance probe it guards.
data FrontWallLift = FrontWallLift
    { fwlChunkNear  ∷ ChunkCoord → Bool
      -- ^ Is this chunk at or adjacent to one carrying structures? The
      --   probe is only paid where it can possibly fire (#418).
    , fwlClearance  ∷ Int → Int → Maybe Float
      -- ^ 'structureFrontWallClear' for this frame's camera.
    }

-- | The lift for a frame with no structures anywhere: every sprite
--   keeps its own key. Used by consumers that have no chunk data to
--   derive one from, and by specs pinning the unlifted case.
noFrontWallLift ∷ FrontWallLift
noFrontWallLift = FrontWallLift
    { fwlChunkNear = const False
    , fwlClearance = \_ _ → Nothing }

-- | Build the frame's lift from its loaded chunks — the ONE
--   construction, so the renderer, the picker and the marker cannot
--   derive three subtly different gates.
--
--   Adjacency (not same-chunk-only) is what lets a sprite across a
--   chunk seam from a wall still be lifted, measured with
--   'chunkSeamChebyshev' so a wall just across the cylindrical U seam
--   still qualifies (#423).
frameFrontWallLift
    ∷ CameraFacing
    → Int                                  -- ^ world size in chunks
    → Int                                  -- ^ camera z-slice
    → Int                                  -- ^ frame's effective depth
    → HM.HashMap ChunkCoord LoadedChunk    -- ^ the frame's loaded chunks
    → FrontWallLift
frameFrontWallLift facing worldSize zSlice effDepth chunks = FrontWallLift
    { fwlChunkNear = \cc →
        any (\sc → chunkSeamChebyshev worldSize sc cc ≤ 1) structured
    , fwlClearance =
        structureFrontWallClear facing worldSize zSlice effDepth structLookup
    }
  where
    structured = [ lcCoord lc
                 | lc ← HM.elems chunks, not (HM.null (lcStructures lc)) ]
    -- 'structureFrontWallClear' already canonicalises the coord it
    -- probes with, so this is idempotent — routed through the shared
    -- boundary anyway so no lookup here is the raw one.
    structLookup cc = lcStructures ⊚ canonicalChunkLookup worldSize chunks cc

-- | A sprite's FINAL painter depth: its own key, raised to sit fully in
--   front of any structure front wall it overlaps. The identity
--   wherever the chunk is nowhere near a structure, which is most of
--   the map.
liftSpriteSortKey
    ∷ FrontWallLift
    → ChunkCoord   -- ^ the chunk the sprite belongs to
    → Int → Int    -- ^ the sprite's tile
    → Float        -- ^ its own key
    → Float
liftSpriteSortKey lift coord gx gy key
    | not (fwlChunkNear lift coord) = key
    | otherwise = case fwlClearance lift gx gy of
        Just c  → max key (c + frontWallEpsilon)
        Nothing → key

-- | #418: a flora/veg billboard sitting in front of a structure's FRONT
--   wall must draw over the WHOLE wall, not slice through the wall's
--   depth-sorted strips (#417). A single-depth sprite otherwise beats
--   the wall's back strips but loses its clamped south strips → the
--   "leaf over the wall / frond cut off" straddle. This finds the
--   highest front-wall strip key the sprite at (gx,gy) is spatially IN
--   FRONT of, so the caller lifts the sprite's key just above it and it
--   clears the entire strip range as one unit. Returns Nothing when no
--   such wall is near (the sprite keeps its normal key).
--
--   The strip-key formula is not restated here: the candidate slots, the
--   tie-break each gets and how far in front of its own tile a wall's
--   clamped strips reach all come from "Structure.Render" itself
--   ('isScreenFrontWall', 'wallTieBreak', 'frontWallDepthSteps'), so the
--   lift and the strips cannot drift apart. The first two are
--   facing-DEPENDENT since #1712 — which walls are screen-front, and
--   which tie-break their rotated sprite takes — while the depth reach is
--   deliberately facing-INVARIANT, measured in painter-depth steps from
--   the tile's own grid origin rather than from a rotating vertex. All
--   three are the historical SE 0.0006 / SW 0.0005 / (wgx+1,wgy+1) at
--   'FaceSouth'. The applyFacing depth test keeps the comparison
--   rotation-correct. Wall lookups cross chunks; the per-chunk gate at
--   the call site keeps it free where there are none.
--
--   Slice-bounded (#1715): eligibility is restricted to walls the
--   structure renderer would EMIT for this same frame, by taking the
--   frame's effective depth and gating on 'pieceWithinSliceBand' — the
--   very predicate 'frontWallStrips' gates on. A wall the camera slice
--   cut away, or one deeper than the zoom-derived depth window, is on
--   screen nowhere and clears nothing, so it must not move a sprite.
--   (Renderer parity is the whole claim here, not just the visible
--   above-slice promotion: that band is zoom-dependent, and every other
--   structure decision in the frame already follows it.) Suppressions
--   the renderer makes on information this helper does not have — an
--   unresolved palette handle, an unavailable texture system — are out
--   of its reach and deliberately out of scope.
--
--   Seam-aware (#423): loaded chunks are keyed by canonical (u-wrapped)
--   coords ('World.Thread.ChunkLoading'), and a chunk's structures are
--   keyed by tile coords in that canonical frame. A neighbour probed
--   just across the cylindrical U seam therefore needs BOTH its chunk
--   coord canonicalised and its tile key shifted by the same wrap
--   delta, or the wall on the far side is silently missed.
structureFrontWallClear
    ∷ CameraFacing
    → Int                                   -- ^ world size in chunks
    → Int                                   -- ^ camera z-slice
    → Int                                   -- ^ frame's effective depth
    → (ChunkCoord → Maybe ChunkStructures)  -- ^ loaded-chunk structure lookup
    → Int → Int                             -- ^ sprite tile (gx, gy)
    → Maybe Float
structureFrontWallClear facing worldSize zSlice effDepth structLookup gx gy =
    let (fa, fb) = applyFacing facing gx gy
        spriteDepth = fa + fb
        -- The walls whose edge is drawn at the SCREEN front right now,
        -- each with the tie-break its rotated sprite gets — the same two
        -- answers Structure.Render gives, from the same functions.
        frontCands =
            [ (fromIntegral (fromEnum slot) ∷ Word8, wallTieBreak facing slot)
            | slot ← [SWallNE, SWallNW, SWallSE, SWallSW]
            , isScreenFrontWall facing slot ]
        wallKeyAt wgx wgy tag tieB = do
            let (cc, _, (dgx, dgy)) = canonicalTileFrame worldSize wgx wgy
                -- Tile key in the stored (canonical) chunk's frame. The
                -- chunk wrap shifts u by whole worlds and preserves
                -- v = cx + cy, so this is the identity away from the seam.
                sgx = wgx + dgx
                sgy = wgy + dgy
            structs ← structLookup cc
            spd ← HM.lookup (sgx, sgy, tag) structs
            -- The wall's strips sort at keys computed from its STORED
            -- coords, while the sprite's own key is in its local frame.
            -- The u-wrap preserves v = gx + gy, so at north/south facings
            -- (depth = ±v) the two frames agree and the cross-seam lift
            -- is exact. At east/west facings depth follows u, which the
            -- wrap shifts by a whole world width, so the two keys are a
            -- whole world apart: skip when the frames disagree.
            --
            -- RE-EXAMINED under #1176 and DELIBERATELY KEPT. That issue
            -- made the wrap offset two-dimensional, so the sprite and
            -- the wall across an east/west seam now do land next to each
            -- other on SCREEN — the original wording here ("the wall
            -- renders nowhere near the sprite") no longer describes the
            -- placement. It does not describe the sort key either, and
            -- the sort key is what this function returns. 'sqSortKey' is
            -- painter DEPTH derived from grid coords ('applyFacing'),
            -- which a screen-space translation cannot and does not
            -- touch, so lifting the sprite to 'scDepth' would still
            -- shove its key a whole world past every quad it is drawn
            -- among and corrupt its local ordering. Removing the guard
            -- needs the two ends to share a translated sort key, which
            -- is a change to the depth frame, not to placement.
            --
            -- (Structure quads DO carry the wrap offset since #1706 —
            -- 'Structure.Render' resolves each chunk's nearest alias
            -- through the same 'isChunkVisibleWrapped' — so a wall now
            -- lands beside the sprite on screen. That still does not
            -- change this guard's verdict, which rests on the key
            -- alone: the offset is a screen translation and leaves
            -- 'sqSortKey' grid-derived, exactly as reasoned above.)
            -- A front wall's clamped strips all land 'frontWallDepthSteps'
            -- in front of its own tile's depth (the tile's front corner,
            -- (wgx+1,wgy+1) at FaceSouth), at every facing.
            let (sa, sb) = applyFacing facing sgx sgy
                scDepth  = sa + sb + frontWallDepthSteps
                (la, lb) = applyFacing facing wgx wgy
                localDepth = la + lb + frontWallDepthSteps
            -- #1715: only a wall the structure renderer would actually
            -- EMIT this frame can be cleared. 'pieceWithinSliceBand' is
            -- the renderer's own gate, shared rather than restated, so
            -- 'frontWallStrips' and this cannot disagree for any
            -- (gridZ, zSlice, effDepth). Both ends bite. ABOVE the
            -- slice: dropping the slice below a room's walls is the
            -- ordinary way to look inside it, and those walls stop
            -- being emitted — yet 'spdGridZ spd - zSlice' is then
            -- POSITIVE, so a wall nobody can see used to lift an
            -- adjacent sprite HARDER than a drawn one would. BELOW the
            -- window: that wall is equally not on screen, and while its
            -- key happens to lose to the sprite's today, the parity is
            -- the contract, not the arithmetic that currently masks the
            -- gap. Inside the band this is a no-op, which is why the
            -- in-slice keys below are untouched.
            if not (pieceWithinSliceBand zSlice effDepth (spdGridZ spd))
               ∨ spriteDepth < localDepth   -- sprite is NOT fully in front
               ∨ scDepth ≢ localDepth     -- frames disagree (E/W seam)
               then Nothing
               else Just (fromIntegral scDepth
                          + fromIntegral (spdGridZ spd - zSlice) * 0.001
                          + tieB)
        cands = [ wallKeyAt (gx + dx) (gy + dy) tag tieB
                | dx ← [-2 .. 2], dy ← [-2 .. 2], (dx, dy) ≢ (0, 0)
                , (tag, tieB) ← frontCands ]
    in case [ k | Just k ← cands ] of
         [] → Nothing
         ks → Just (maximum ks)
