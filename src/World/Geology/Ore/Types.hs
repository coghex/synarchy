{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Ore-sheet deposit types.
--
--   An ore sheet is a sedimentary deposit laid down during one
--   geological Age: sediment shed by a volcanic source feature is
--   routed downhill along the age's flow field and dropped where the
--   terrain flattens out. The sheet is recorded as a normal
--   depositional 'GeoEvent' (elevation delta = thickness, intrusion =
--   thickness), so the existing strata replay buries it under
--   everything later periods lay down — early-age sheets end up deep,
--   late-age sheets shallow.
--
--   The footprint of a flow-routed fan can't be expressed by the
--   analytic centre+radius shapes the other events use, so the sheet
--   carries a small thickness raster sampled at the coarse hydrology
--   grid's spacing ('World.Hydrology.Simulation.ElevGrid'). Per-column
--   application bilinearly interpolates the raster, which also gives
--   the sheet naturally tapered edges.
module World.Geology.Ore.Types
    ( OreSheetParams(..)
    , oreSheetThicknessAt
    , OreLevers(..)
    , defaultOreLevers
    , WorldOreDeposits(..)
    , emptyWorldOreDeposits
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable)
import Data.Serialize (Serialize(..))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..))

-- | One flow-routed ore sheet. The thickness raster lives in the
--   coarse hydrology grid's index space: cell @(ix, iy)@ sits at
--   @u = (ix − gridW/2)·spacing@, @v = (iy − gridW/2)·spacing@ where
--   @u = gx − gy@, @v = gx + gy@. The window origin 'osIX0' may be
--   unwrapped (negative or ≥ gridW) for fans near the u-seam; sampling
--   wraps modulo 'osGridW'. Thickness is stored one byte per cell,
--   row-major with iy outer ('osThick' has @osW · osH@ bytes —
--   ByteString rather than an unboxed vector so the stock 'Serialize'
--   and 'Hashable' instances apply).
data OreSheetParams = OreSheetParams
    { osMat     ∷ !Word8   -- ^ Ore material id deposited by this sheet
    , osGridW   ∷ !Int     -- ^ Full coarse-grid width (cells per side)
    , osSpacing ∷ !Int     -- ^ Tile spacing between grid cells
    , osIX0     ∷ !Int     -- ^ Window origin, u-axis cell index (unwrapped)
    , osIY0     ∷ !Int     -- ^ Window origin, v-axis cell index
    , osW       ∷ !Int     -- ^ Window width in cells
    , osH       ∷ !Int     -- ^ Window height in cells
    , osThick   ∷ !BS.ByteString -- ^ Per-cell thickness (z-levels)
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

-- | Sample a sheet's deposited thickness at a global tile coordinate.
--   Bilinear interpolation of the cell raster; cells outside the
--   window read as 0, so edges taper over one grid spacing (the
--   builder pads the window with a zero margin).
{-# INLINE oreSheetThicknessAt #-}
oreSheetThicknessAt ∷ OreSheetParams → Int → Int → Float
oreSheetThicknessAt os gx gy =
    let gridW = osGridW os
        halfGrid = gridW `div` 2
        spacing = fromIntegral (osSpacing os) ∷ Float
        u = fromIntegral (gx - gy) ∷ Float
        v = fromIntegral (gx + gy) ∷ Float
        fu = u / spacing + fromIntegral halfGrid
        fv = v / spacing + fromIntegral halfGrid
        -- Window-local, wrapping the u-axis (the grid is a torus in ix).
        luRaw = fu - fromIntegral (osIX0 os)
        gw = fromIntegral gridW ∷ Float
        lu = luRaw - gw * fromIntegral (floor (luRaw / gw) ∷ Int)
        lv = fv - fromIntegral (osIY0 os)
        w = osW os
        h = osH os
        cellAt cx cy
            | cx < 0 ∨ cx ≥ w ∨ cy < 0 ∨ cy ≥ h = 0.0
            | otherwise = fromIntegral
                (BS.index (osThick os) (cy * w + cx)) ∷ Float
    in if lu < (-1.0) ∨ lu > fromIntegral w ∨ lv < (-1.0) ∨ lv > fromIntegral h
       then 0.0
       else
       let ix = floor lu ∷ Int
           iy = floor lv ∷ Int
           tx = lu - fromIntegral ix
           ty = lv - fromIntegral iy
           t00 = cellAt ix iy
           t10 = cellAt (ix + 1) iy
           t01 = cellAt ix (iy + 1)
           t11 = cellAt (ix + 1) (iy + 1)
           top = t00 * (1.0 - tx) + t10 * tx
           bot = t01 * (1.0 - tx) + t11 * tx
       in top * (1.0 - ty) + bot * ty

-- | Resource-abundance config levers (from
--   @config/world_gen_default.yaml@, @resources:@ block). Purely
--   mechanistic: they scale how much sediment each source sheds; no
--   per-world floor is enforced, so low-volcanism seeds can roll
--   ore-poor worlds by design.
data OreLevers = OreLevers
    { olGlobal ∷ !Float   -- ^ Global multiplier on all ore flux
    , olIron   ∷ !Float   -- ^ Iron-specific multiplier
    , olCopper ∷ !Float   -- ^ Copper-specific multiplier
    } deriving (Show, Eq, Generic, Serialize, NFData)
      -- Positional schema (lands in WorldGenParams' save record):
      -- new levers go at the end.

defaultOreLevers ∷ OreLevers
defaultOreLevers = OreLevers 1.0 1.0 1.0

-- | Global per-chunk ore summary, built once at timeline assembly from
--   the emitted sheets (same pattern as 'gtWorldLakes' /
--   'gtWorldLavaPools'). Volume is in tile·z units (cell thickness ×
--   the ~spacing²/2 tiles a coarse cell covers). Read by the zoom-map
--   info panel; the per-tile truth lives in the strata themselves.
newtype WorldOreDeposits = WorldOreDeposits
    { wodByChunk ∷ HM.HashMap ChunkCoord [(Word8, Int)]
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (NFData)

instance Serialize WorldOreDeposits where
    put wod = put (HM.toList (wodByChunk wod))
    get = WorldOreDeposits . HM.fromList <$> get

emptyWorldOreDeposits ∷ WorldOreDeposits
emptyWorldOreDeposits = WorldOreDeposits HM.empty
