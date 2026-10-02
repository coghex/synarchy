{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, OverloadedStrings #-}
-- Read-only characterization of the REAL solver. No copied fluid algorithm.
-- Compile against lib:synarchy; the JSON records limitations, not a pass verdict.
module Main (main) where

import UPrelude
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Sim.Chunk (activateChunk, loadedChunkState)
import Sim.Fluid.Active (simulateActiveTick)
import Sim.State.Types (SimWorldState(..), SimChunkState(..), emptySimWorldState)
import Sim.Topology (SimTopology(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..), fluidVolumeOverTerrain)

type Tile = (Int, Int)

address ∷ Tile → (ChunkCoord, Int)
address (x,y) = (ChunkCoord (x `div` chunkSize) (y `div` chunkSize),
                 (y `mod` chunkSize) * chunkSize + x `mod` chunkSize)

fixture ∷ Tile → Tile → Int → Int → Int → Bool → Bool → SimWorldState
fixture source target sourceBed targetBed units targetPresent targetActive =
    emptySimWorldState
        { swsChunks = HM.fromList [(cc, make cc) | cc ← coords]
        , swsActive = True
        , swsTopology = SimFlatTopology
        }
  where
    (sourceCC, sourceI) = address source
    (targetCC, targetI) = address target
    coords = if sourceCC ≡ targetCC ∨ not targetPresent
             then [sourceCC] else [sourceCC, targetCC]
    make cc =
        let terrain = VU.replicate (chunkSize * chunkSize) 0 VU.//
                ([(sourceI, sourceBed) | cc ≡ sourceCC] <>
                 [(targetI, targetBed) | cc ≡ targetCC])
            fluid = V.replicate (chunkSize * chunkSize) Nothing V.//
                [(sourceI, Just (FluidCell River (sourceBed * 8 + units)))
                    | cc ≡ sourceCC]
            chunk = loadedChunkState fluid terrain
        in if cc ≡ sourceCC ∨ targetActive then activateChunk chunk else chunk

volumeAt ∷ Tile → SimWorldState → Int
volumeAt tile state =
    let (cc, i) = address tile
    in case HM.lookup cc (swsChunks state) of
        Nothing → 0
        Just chunk → maybe 0 (fluidVolumeOverTerrain (scsTerrain chunk VU.! i))
                            (scsFluid chunk V.! i)

total ∷ SimWorldState → Int
total state = sum
    [ fluidVolumeOverTerrain (scsTerrain chunk VU.! i) cell
    | chunk ← HM.elems (swsChunks state)
    , (i, Just cell) ← zip [0..] (V.toList (scsFluid chunk)) ]

report ∷ Text → Tile → Tile → Int → Int → Int → Bool → Bool → A.Value
report name source target sb tb units present active =
    let initial = fixture source target sb tb units present active
        states = take 11 (iterate simulateActiveTick initial)
        sample tick state = A.object
            [ "tick" A..= tick, "sourceUnits" A..= volumeAt source state
            , "targetUnits" A..= volumeAt target state
            , "totalUnits" A..= total state ]
    in A.object
        [ "name" A..= name, "source" A..= source, "target" A..= target
        , "sourceBed" A..= sb, "targetBed" A..= tb
        , "targetPresent" A..= present, "targetActive" A..= active
        , "conservedEveryTick" A..= all ((≡ units) . total) states
        , "samples" A..= zipWith sample ([0..] ∷ [Int]) states ]

main ∷ IO ()
main = BL.putStrLn $ A.encode $ A.object
    [ "kind" A..= ("production-solver-characterization" ∷ Text)
    , "wallZ" A..= (0 ∷ Int)
    , "cases" A..=
        [ report "raised-sill-interior" (7,8) (8,8) (-4) (-3) 24 True True
        , report "raised-sill-seam" (15,8) (16,8) (-4) (-3) 24 True True
        , report "downhill-control" (7,8) (8,8) (-4) (-5) 24 True True
        , report "one-level-interior" (7,8) (8,8) (-4) (-4) 8 True True
        , report "one-level-seam" (15,8) (16,8) (-4) (-4) 8 True True
        , report "inactive-neighbor" (15,8) (16,8) (-4) (-4) 24 True False
        , report "absent-neighbor" (15,8) (16,8) (-4) (-4) 24 False False
        , report "active-neighbor-control" (15,8) (16,8) (-4) (-4) 24 True True
        ]
    ]
