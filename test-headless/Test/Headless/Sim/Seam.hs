{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for the cross-chunk fluid seam exchange
--   ('Sim.Fluid.Active.reconcileSeams', exercised via 'simulateActiveTick').
--
--   Setup: chunk A=(0,0) is full of water (5 levels), chunk B=(1,0) is
--   dry; both flat at z=0, both active. Before the seam fix, A's edge
--   cells had no in-chunk neighbour past the boundary, so water could
--   never cross into B (the 1-tile lip). These tests pin the two
--   properties that matter: total volume is conserved every tick, and
--   water actually crosses the seam and levels the step down.
module Test.Headless.Sim.Seam (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidType(..))
import Sim.State.Types (SimWorldState(..), SimChunkState(..))
import Sim.Fluid.Types (ActiveFluidCell(..), volumePerLevel, volumeToSurface)
import Sim.Fluid.Active (simulateActiveTick)

n ∷ Int
n = chunkSize * chunkSize

-- | An active chunk: flat terrain at z=0, given active-fluid grid.
mkChunk ∷ V.Vector (Maybe ActiveFluidCell) → SimChunkState
mkChunk active = SimChunkState
    { scsFluid       = V.replicate n Nothing
    , scsTerrain     = VU.replicate n 0
    , scsSettleTicks = 0
    , scsActive      = True
    , scsActiveFluid = active
    , scsEquilTicks  = 0
    , scsSideDeco    = VU.replicate n 0
    }

mkState ∷ [(ChunkCoord, SimChunkState)] → SimWorldState
mkState chunks = SimWorldState
    { swsChunks      = HM.fromList chunks
    , swsDirtyChunks = HS.empty
    , swsActive      = True
    }

-- | Sum of active-fluid volume across all chunks.
activeVolume ∷ SimWorldState → Int
activeVolume ss = sum
    [ fromIntegral (afcVolume afc)
    | scs ← HM.elems (swsChunks ss)
    , Just afc ← V.toList (scsActiveFluid scs) ]

allActive ∷ SimWorldState → Bool
allActive ss = all scsActive (HM.elems (swsChunks ss))

tick ∷ Int → SimWorldState → SimWorldState
tick k ss = iterate simulateActiveTick ss !! k

-- | Volume in one chunk (active grid).
chunkVolume ∷ ChunkCoord → SimWorldState → Int
chunkVolume cc ss = case HM.lookup cc (swsChunks ss) of
    Nothing  → 0
    Just scs → sum [ fromIntegral (afcVolume afc)
                   | Just afc ← V.toList (scsActiveFluid scs) ]

-- | Water surface of the cell at (lx,ly) in a chunk (terrain z=0).
surfAt ∷ ChunkCoord → Int → Int → SimWorldState → Int
surfAt cc lx ly ss = case HM.lookup cc (swsChunks ss) of
    Nothing  → 0
    Just scs → case scsActiveFluid scs V.! (ly * chunkSize + lx) of
        Nothing  → 0
        Just afc → volumeToSurface 0 (afcVolume afc)

spec ∷ Spec
spec = do
    let full = V.replicate n (Just (ActiveFluidCell Lake
                                       (fromIntegral (5 * volumePerLevel)) 0))
        dry  = V.replicate n Nothing
        st0  = mkState [ (ChunkCoord 0 0, mkChunk full)
                       , (ChunkCoord 1 0, mkChunk dry) ]
        total0 = activeVolume st0
        a = ChunkCoord 0 0
        b = ChunkCoord 1 0

    describe "seam exchange" $ do
        it "conserves total volume every tick (no water created/lost)" $
            map (\k → activeVolume (tick k st0)) [0, 1, 2, 5, 20, 100]
                `shouldBe` replicate 6 total0

        it "keeps both chunks active while equalising (< equilThreshold)" $
            allActive (tick 100 st0) `shouldBe` True

        it "spills water across the seam into the dry neighbour" $
            chunkVolume b (tick 30 st0) `shouldSatisfy` (> 0)

        it "drains the source chunk as water crosses" $
            chunkVolume a (tick 30 st0) `shouldSatisfy` (< chunkVolume a st0)

        it "shrinks the seam surface step over time (lip levels out)" $ do
            let step k = surfAt a (chunkSize - 1) 8 (tick k st0)  -- A east edge
                       - surfAt b 0               8 (tick k st0)  -- B west edge
            step 80 `shouldSatisfy` (< step 2)

        it "two identical chunks exchange nothing (no spurious flow)" $ do
            let eq = mkState [ (a, mkChunk full), (b, mkChunk full) ]
                v0 = activeVolume eq
                stN = tick 5 eq
            -- volume conserved AND each chunk unchanged at the seam
            activeVolume stN `shouldBe` v0
            surfAt a (chunkSize - 1) 8 stN `shouldBe` surfAt b 0 8 stN
