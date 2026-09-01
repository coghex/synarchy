{-# LANGUAGE Strict #-}

-- | Exact-equivalence coverage for the bounded breakthrough-search scratch.
--   The reference implementation deliberately retains the original
--   full-world mutable vectors so a scratch-layout change cannot alter the
--   selected ocean, path, tie ordering, range/cost boundaries, or wrap rules.
module Test.Headless.WorldGen.Breakthrough (spec) where

import UPrelude
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import qualified Data.IntMap.Strict as IM
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Test.Hspec
import World.Fluid.River.Identify.Breakthrough (dijkstraBreakthrough)

breakthroughRange ∷ Int
breakthroughRange = 40

breakthroughMaxCarve ∷ Int
breakthroughMaxCarve = 50

tile ∷ Int → Int → Int → Int
tile worldTiles x y = y * worldTiles + x

flatTerrain ∷ Int → VU.Vector Int
flatTerrain worldTiles = VU.replicate (worldTiles * worldTiles) 0

oceanAt ∷ Int → [Int] → VU.Vector Bool
oceanAt worldTiles oceanTiles =
    VU.generate (worldTiles * worldTiles) (`elem` oceanTiles)

-- | Frozen pre-optimization search. Keep global tile indices for all three
--   scratch vectors: this is intentionally not factored through production
--   helpers, because its purpose is to catch observable drift in those
--   helpers.
referenceDijkstra
    ∷ Int
    → Int
    → VU.Vector Int
    → VU.Vector Bool
    → Maybe ([Int], Int)
referenceDijkstra worldTiles startIdx terrain worldOcean = runST $ do
    let nTiles = worldTiles * worldTiles
    bestCost ← VUM.replicate nTiles (maxBound ∷ Int)
    parent   ← VUM.replicate nTiles (-1       ∷ Int)
    dist     ← VUM.replicate nTiles (maxBound ∷ Int)
    VUM.write bestCost startIdx 0
    VUM.write dist     startIdx 0
    foundRef ← newSTRef Nothing
    let neighbours i =
            let bx = i `mod` worldTiles
                by = i `div` worldTiles
                east = if bx < worldTiles - 1
                       then i + 1
                       else i + 1 - worldTiles
                west = if bx > 0
                       then i - 1
                       else i - 1 + worldTiles
                north = if by > 0
                        then Just (i - worldTiles) else Nothing
                south = if by < worldTiles - 1
                        then Just (i + worldTiles) else Nothing
            in [Just east, Just west, north, south]
        loop pq
            | IM.null pq = pure ()
            | otherwise = do
                let ((c, (i, rest)), pqAfter) = case IM.findMin pq of
                        (ck, vs) → case vs of
                            (v:rs) → ((ck, (v, rs)), IM.delete ck pq)
                            []     → ((ck, (-1, [])), IM.delete ck pq)
                    pq1 = if null rest
                          then pqAfter
                          else IM.insert c rest pqAfter
                if i < 0 then loop pq1
                else do
                  bc ← VUM.read bestCost i
                  if c > bc
                    then loop pq1
                    else do
                      done ← readSTRef foundRef
                      case done of
                        Just _ → pure ()
                        Nothing →
                          if worldOcean VU.! i
                            then writeSTRef foundRef (Just (i, c))
                            else do
                              d ← VUM.read dist i
                              if d ≥ breakthroughRange
                                then loop pq1
                                else do
                                  pq2 ← expandNeighbours i d c pq1
                                  loop pq2
        expandNeighbours i d c pq =
            foldM (tryStep i d c) pq (neighbours i)
        tryStep i d c pq mn =
            case mn of
                Nothing → pure pq
                Just nIdx → do
                    let nT = terrain VU.! nIdx
                    if nT ≡ minBound
                       then pure pq
                       else do
                         let edgeCost = max 0 (nT - terrain VU.! i) + 1
                             newCost  = c + edgeCost
                         if newCost > breakthroughMaxCarve
                            then pure pq
                            else do
                              bcN ← VUM.read bestCost nIdx
                              if newCost < bcN
                                then do
                                    VUM.write bestCost nIdx newCost
                                    VUM.write parent   nIdx i
                                    VUM.write dist     nIdx (d + 1)
                                    pure (IM.insertWith (++) newCost
                                              [nIdx] pq)
                                else pure pq
    loop (IM.singleton 0 [startIdx])
    fr ← readSTRef foundRef
    case fr of
        Nothing → pure Nothing
        Just (endIdx, c) → do
            let rebuild cur acc = do
                    p ← VUM.read parent cur
                    if p < 0 then pure (cur : acc)
                    else rebuild p (cur : acc)
            path ← rebuild endIdx []
            pure (Just (path, c))

shouldMatchReference
    ∷ Int
    → Int
    → VU.Vector Int
    → VU.Vector Bool
    → Expectation
shouldMatchReference worldTiles start terrain worldOcean =
    dijkstraBreakthrough worldTiles start terrain worldOcean
        `shouldBe` referenceDijkstra worldTiles start terrain worldOcean

spec ∷ Spec
spec = describe "dijkstraBreakthrough" $ do
    it "preserves east/west wrap and exact equal-cost tie ordering" $ do
        let worldTiles = 96
            start = tile worldTiles 48 48
            west = tile worldTiles 47 48
            east = tile worldTiles 49 48
            terrain = flatTerrain worldTiles
            worldOcean = oceanAt worldTiles [east, west]
        shouldMatchReference worldTiles start terrain worldOcean
        dijkstraBreakthrough worldTiles start terrain worldOcean
            `shouldBe` Just ([start, west], 1)

        let seamStart = tile worldTiles 0 24
            seamOcean = tile worldTiles (worldTiles - 1) 24
            seamMask = oceanAt worldTiles [seamOcean]
        shouldMatchReference worldTiles seamStart terrain seamMask
        dijkstraBreakthrough worldTiles seamStart terrain seamMask
            `shouldBe` Just ([seamStart, seamOcean], 1)

    it "accepts radius 40 exactly and rejects radius 41" $ do
        let worldTiles = 128
            start = tile worldTiles 64 64
            terrain = flatTerrain worldTiles
            atLimit = oceanAt worldTiles [tile worldTiles 104 64]
            pastLimit = oceanAt worldTiles [tile worldTiles 105 64]
        shouldMatchReference worldTiles start terrain atLimit
        shouldMatchReference worldTiles start terrain pastLimit
        fmap snd (dijkstraBreakthrough worldTiles start terrain atLimit)
            `shouldBe` Just 40
        dijkstraBreakthrough worldTiles start terrain pastLimit
            `shouldBe` Nothing

    it "keeps north/south clipped and preserves the carve-cost cap" $ do
        let worldTiles = 96
            start = tile worldTiles 48 0
            terrain = flatTerrain worldTiles
            oppositePole = oceanAt worldTiles [tile worldTiles 48 95]
        shouldMatchReference worldTiles start terrain oppositePole
        dijkstraBreakthrough worldTiles start terrain oppositePole
            `shouldBe` Nothing

        let centre = tile worldTiles 48 48
            costlyOcean = tile worldTiles 49 48
            costlyTerrain = terrain VU.// [(costlyOcean, 50)]
            costlyMask = oceanAt worldTiles [costlyOcean]
        shouldMatchReference worldTiles centre costlyTerrain costlyMask
        dijkstraBreakthrough worldTiles centre costlyTerrain costlyMask
            `shouldBe` Nothing

    it "matches the full-world search across deterministic obstacles" $ do
        let worldTiles = 96
            starts =
                [ tile worldTiles 2 2
                , tile worldTiles 47 47
                , tile worldTiles 94 70
                ]
            isStart i = i `elem` starts
            terrain = VU.generate (worldTiles * worldTiles) $ \i →
                let x = i `mod` worldTiles
                    y = i `div` worldTiles
                in if (x * 17 + y * 31) `mod` 53 ≡ 0 ∧ not (isStart i)
                   then minBound
                   else (x * 7 + y * 11) `mod` 9
            worldOcean = VU.generate (worldTiles * worldTiles) $ \i →
                let x = i `mod` worldTiles
                    y = i `div` worldTiles
                in (x + 2 * y) `mod` 71 ≡ 0
        forM_ starts $ \start →
            shouldMatchReference worldTiles start terrain worldOcean

    it "keeps exact behavior for worlds narrower than the search diameter" $ do
        let worldTiles = 32
            start = tile worldTiles 1 16
            target = tile worldTiles 29 16
            terrain = flatTerrain worldTiles
            worldOcean = oceanAt worldTiles [target]
        shouldMatchReference worldTiles start terrain worldOcean
