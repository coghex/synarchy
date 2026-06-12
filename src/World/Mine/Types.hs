{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Mine-designation state.
--
--   A designation marks one tile at one z level for excavation. Each
--   carries four corner-progress floats (1.0 = untouched, 0.0 = dug
--   out): as a unit digs, the corners on the digger's side drain
--   first, and the renderer maps the corner pattern onto the slope
--   tile variants so the tile visibly excavates instead of popping.
--   When all four corners reach zero the tile's surface drops one z
--   (the WeDeleteTile edit path) and the designation is removed.
--
--   Persisted in saves (sdMineDesignations, save v31) including
--   mid-dig corner progress.
module World.Mine.Types
    ( MineDesignation(..)
    , MineDesignations
    , newMineDesignation
    , designationFromSlope
    , digThreshold
    , drainCorners
    , cornersDone
    , digSlopeMask
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Bits (testBit)
import Data.List (sortOn)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM

-- | One designated tile. Corner order is grid-space:
--   @(cNW, cNE, cSE, cSW)@ = corners at (gx,gy), (gx+1,gy),
--   (gx+1,gy+1), (gx,gy+1) — i.e. clockwise from the tile's origin
--   corner. (Screen orientation varies with camera facing; everything
--   here is grid space and the renderer applies facing.)
data MineDesignation = MineDesignation
    { mdZ       ∷ !Int
      -- ^ Surface z captured at designation time (markers render from
      --   it; the designation is for removing the tile at this level).
    , mdCorners ∷ !(Float, Float, Float, Float)
      -- ^ Corner dig progress, 1.0 → 0.0 (NW, NE, SE, SW).
    , mdChunkProgress ∷ !Float
      -- ^ Hidden chunk-yield accumulator (dig yields, G2). Fills as
      --   the tile is dug, scaled by the CURRENT digger's mining
      --   skill each tick; at ≥ 1.0 a chunk item spawns and 1.0 is
      --   subtracted. Dies with the designation (tile completion
      --   discards any remainder — one tile only ever provides what
      --   was extracted from it; deliberately NOT a global bar).
      --   Field order is load-bearing (positional Generic Serialize):
      --   appended for save v35.
    } deriving (Show, Eq, Generic, Serialize, NFData)

type MineDesignations = HM.HashMap (Int, Int) MineDesignation

-- | Fresh designation: full corners, no yield progress.
newMineDesignation ∷ Int → MineDesignation
newMineDesignation z = MineDesignation z (1.0, 1.0, 1.0, 1.0) 0.0

-- | Designation for a tile that's ALREADY sloped at generation time:
--   the lowered side is pre-dug, so its corners start at zero. The
--   slope mask is the renderer's edge encoding (N=1, E=2, S=4, W=8 =
--   slopes down toward that side); an edge being lowered means both
--   of its corners are gone. Inverse of 'digSlopeMask' — designating
--   a sloped tile renders the same shape it already had, and the
--   remaining dig volume matches the remaining material.
designationFromSlope ∷ Int → Word8 → MineDesignation
designationFromSlope z slopeMask =
    let edgeN = testBit slopeMask 0
        edgeE = testBit slopeMask 1
        edgeS = testBit slopeMask 2
        edgeW = testBit slopeMask 3
        corner lowered = if lowered then 0.0 else 1.0
    in MineDesignation z
        ( corner (edgeN ∨ edgeW)   -- NW
        , corner (edgeN ∨ edgeE)   -- NE
        , corner (edgeS ∨ edgeE)   -- SE
        , corner (edgeS ∨ edgeW)   -- SW
        )
        0.0

-- | A corner below this counts as "dug out" for slope-variant
--   selection (completion still requires corners to reach 0).
digThreshold ∷ Float
digThreshold = 0.5

-- | Pour dig progress into the corners, DIGGER-SIDE FIRST: corners
--   are drained in order of distance from the digger's position, each
--   to zero before the next starts, so the tile slopes down toward
--   the unit as work progresses. @amount@ is total progress (already
--   scaled by tool × material speed).
drainCorners ∷ (Float, Float)   -- ^ digger position (tile-space)
             → (Int, Int)       -- ^ tile (gx, gy)
             → Float            -- ^ progress to apply
             → (Float, Float, Float, Float)
             → (Float, Float, Float, Float)
drainCorners (ux, uy) (gx, gy) amount (cNW, cNE, cSE, cSW) =
    let dist2 cx cy =
            let dx = fromIntegral cx - ux
                dy = fromIntegral cy - uy
            in dx * dx + dy * dy ∷ Float
        -- Corner order: NW, NE, SE, SW at (gx,gy),(gx+1,gy),
        -- (gx+1,gy+1),(gx,gy+1) — matches mdCorners.
        order = map fst $ sortOn snd
            [ (0 ∷ Int, dist2 gx gy)
            , (1, dist2 (gx + 1) gy)
            , (2, dist2 (gx + 1) (gy + 1))
            , (3, dist2 gx (gy + 1))
            ]
        pour amt vals [] = (amt, vals)
        pour amt vals (i:rest)
            | amt ≤ 0   = (0, vals)
            | otherwise =
                let v = vals !! i
                    d = min v amt
                    vals' = [ if j ≡ i then v - d else x
                            | (j, x) ← zip [0 ..] vals ]
                in pour (amt - d) vals' rest
        (_, drained) = pour amount [cNW, cNE, cSE, cSW] order
        at i = case drop i drained of
            (v:_) → v
            []    → 0
    in (at 0, at 1, at 2, at 3)

-- | All corners fully drained → the tile's surface drops one z.
cornersDone ∷ (Float, Float, Float, Float) → Bool
cornersDone (a, b, c, d) = a ≤ 0 ∧ b ≤ 0 ∧ c ≤ 0 ∧ d ≤ 0

-- | Map corner dig state onto the slope-id edge mask the renderer
--   already understands (N=1, E=2, S=4, W=8 = "slopes down toward
--   that side"; see World.Slope.computeTileSlope). An edge slopes
--   once BOTH its corners are below 'digThreshold'.
--
--   When ALL FOUR corners are below threshold the raw mask would be
--   15, which the renderer treats as flat — the tile would flash back
--   to a full cube right before the completion drop. Instead we treat
--   the most-intact corner as still undug, so the tile keeps sloping
--   away from the remaining material until the z-drop.
digSlopeMask ∷ (Float, Float, Float, Float) → Word8
digSlopeMask (cNW, cNE, cSE, cSW) =
    let dugNW = cNW < digThreshold
        dugNE = cNE < digThreshold
        dugSE = cSE < digThreshold
        dugSW = cSW < digThreshold
        maskOf nw ne se sw =
            (if nw ∧ ne then 1 else 0)
          ⌄ (if ne ∧ se then 2 else 0)
          ⌄ (if se ∧ sw then 4 else 0)
          ⌄ (if sw ∧ nw then 8 else 0) ∷ Word8
        mask = maskOf dugNW dugNE dugSE dugSW
    in if mask ≠ 15
       then mask
       else
        -- All dug: hold back the most-intact corner (tuple max —
        -- ties resolve to the later corner in NW,NE,SE,SW order;
        -- deterministic either way).
        let vals = [cNW, cNE, cSE, cSW]
            imax = snd (maximum (zip vals [0 ∷ Int, 1, 2, 3]))
        in maskOf (dugNW ∧ imax ≠ 0) (dugNE ∧ imax ≠ 1)
                  (dugSE ∧ imax ≠ 2) (dugSW ∧ imax ≠ 3)
