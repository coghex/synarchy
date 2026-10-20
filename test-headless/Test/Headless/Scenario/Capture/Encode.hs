{-# LANGUAGE Strict #-}
-- | The document encoder (#2714), checked as a PROPERTY rather than by
--   example: whatever terrain, fluid and flora the schema can state, the text
--   "Scenario.Capture.Encode" writes decodes back through the shared scenario
--   boundary, with no diagnostic, into exactly the scenario that was
--   written. This is what lets the capture service verify its candidate bytes
--   by decoding and comparing them: an entry the document cannot carry is
--   found before anything is published.
--
--   The generator is a small deterministic one (a fixed-seed linear
--   congruential stream), so a failing case is reproduced by its index and no
--   new dependency is needed. Placement, unit and item entries have their own
--   round-trip checks beside their encoders; the engine-backed examples of
--   this group cover the whole document composed from a live arena.
module Test.Headless.Scenario.Capture.Encode (spec) where

import UPrelude
import Test.Hspec
import Data.List (nub, sort)
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashSet as HS
import Gameplay.Tags.Types (GameplayTag, mkGameplayTag)
import Scenario.Bounds (TileBounds(..), mapTileBounds)
import Scenario.Capture.Encode (encodeScenario)
import Scenario.Schema (currentScenarioVersion, decodeScenarioBytes)
import Scenario.Types

-- | A deterministic pseudo-random stream.
newtype Gen = Gen Word64

-- | The next value in @[lo, hi]@, and the advanced stream.
range ∷ Int → Int → Gen → (Int, Gen)
range lo hi (Gen s) =
    let s' = s * 6364136223846793005 + 1442695040888963407
        mixed = (s' `shiftR` 33) `xor` s'
    in (lo + fromIntegral (mixed `mod` fromIntegral (hi - lo + 1)), Gen s')

pick ∷ [α] → Gen → (α, Gen)
pick xs g = let (i, g') = range 0 (length xs - 1) g in (xs !! i, g')

-- | A list of @n@ values drawn one after another.
many ∷ Int → (Gen → (α, Gen)) → Gen → ([α], Gen)
many n f g0 = go n g0 []
  where go 0 g acc = (reverse acc, g)
        go k g acc = let (x, g') = f g in go (k - 1 ∷ Int) g' (x : acc)

authoredMaybe ∷ (Gen → (α, Gen)) → Gen → (Authored α, Gen)
authoredMaybe f g =
    let (present, g1) = range 0 1 g
    in if present ≡ 0 then (Omitted, g1) else let (v, g2) = f g1 in (Authored v, g2)

tag ∷ Text → GameplayTag
tag t = fromMaybe (error "bad tag") (mkGameplayTag t)

tagsOf ∷ Gen → ([GameplayTag], Gen)
tagsOf g =
    let (n, g1) = range 0 3 g
        (picked, g2) = many n (pick ["pad", "grove", "site", "squad"]) g1
    in (map tag (sort (nub picked)), g2)

-- | A scenario of terrain, fluid and flora the decoder accepts.
scenarioOf ∷ Int → Scenario
scenarioOf k = fst $ build (Gen (fromIntegral k * 7919 + 17))
  where
    build g0 =
        let (finite, g1) = range 0 1 g0
            (w, g2) = range 4 60 g1
            (h, g3) = range 4 60 g2
            dims = if finite ≡ 1 then Just (MapDimensions w h) else Nothing
            (x0, y0, x1, y1) = case dims >>= mapTileBounds of
                Just b → (tbMinX b, tbMinY b, tbMaxX b, tbMaxY b)
                Nothing → (-30, -30, 30, 30)
            tile g = let (x, ga) = range x0 x1 g; (y, gb) = range y0 y1 ga in ((x, y), gb)
            region g =
                let (rect, ga) = range 0 1 g
                in if rect ≡ 1
                    then let (a, gb) = tile ga; (b, gc) = tile gb
                         in (RegionRect (min (fst a) (fst b)) (min (snd a) (snd b))
                                        (max (fst a) (fst b)) (max (snd a) (snd b)), gc)
                    else let (n, gb) = range 1 4 ga
                             (ts, gc) = many n tile gb
                         in (RegionTiles (nub ts), gc)
            quarter lo hi g = let (n, g') = range lo hi g in (fromIntegral n / 4 ∷ Float, g')
            eighth lo hi g = let (n, g') = range lo hi g in (fromIntegral n / 8 ∷ Float, g')
            terrain i g =
                let (tags, ga) = tagsOf g
                    (r, gb) = region ga
                    (mat, gc) = pick ["granite", "sand", "loam"] gb
                    (z, gd) = range (-5) 20 gc
                    (slope, ge) = authoredMaybe (range 0 15) gd
                in (TerrainPatch (ExplicitId ("terrain." <> tshow (i ∷ Int))) tags r mat z (fmap fromIntegral slope), ge)
            fluid i g =
                let (tags, ga) = tagsOf g
                    (r, gb) = region ga
                    (kind, gc) = pick [minBound .. maxBound] gb
                    (e, gd) = range (-40) 200 gc
                in (FluidPatch (ExplicitId ("fluid." <> tshow i)) tags r kind e, gd)
            flora i g =
                let (tags, ga) = tagsOf g
                    (sp, gb) = pick ["oak", "fern"] ga
                    ((x, y), gc) = tile gb
                    (z, gd) = authoredMaybe (range (-5) 10) gc
                    (age, ge) = authoredMaybe (quarter 0 800) gd
                    (health, gf) = authoredMaybe (eighth 0 8) ge
                    (ou, gg) = authoredMaybe (eighth (-4) 4) gf
                    (ov, gh) = authoredMaybe (eighth (-4) 4) gg
                    (variant, gi) = authoredMaybe (range 0 3) gh
                    (regrowth, gj) = authoredMaybe (quarter 0 400) gi
                in (FloraEntry (ExplicitId ("flora." <> tshow i)) tags sp x y z age health ou ov (fmap fromIntegral variant) regrowth, gj)
            (nt, g4) = range 0 6 g3
            (nf, g5) = range 0 5 g4
            (nl, g6) = range 0 6 g5
            (ts, g7) = manyI nt terrain g6
            (fs, g8) = manyI nf fluid g7
            (ls, g9) = manyI nl flora g8
            (cx, ga) = quarter (-400) 400 g9
            (cy, gb) = quarter (-400) 400 ga
        in ( Scenario
                { scSourceVersion = currentScenarioVersion, scMap = dims, scCamera = (cx, cy)
                , scTerrain = ts, scFluids = fs, scFlora = ls
                , scStructures = [], scBuildings = [], scLocations = [], scUnits = [], scGroundItems = [] }
           , gb )
    manyI n f g0 = go (0 ∷ Int) g0 []
      where go i g acc | i ≥ n = (reverse acc, g)
                       | otherwise = let (x, g') = f i g in go (i + 1) g' (x : acc)

catalog ∷ ScenarioCatalog
catalog = emptyScenarioCatalog
    { catMaterials = HS.fromList ["granite", "sand", "loam"]
    , catFlora = HS.fromList ["oak", "fern"] }

spec ∷ Spec
spec = describe "document encoding (a property over generated scenarios)" $ do

    it "decodes back, with no diagnostic, into exactly the scenario that was written (500 generated scenarios)" $
        forM_ [1 .. 500 ∷ Int] $ \k → do
            let sc = scenarioOf k
            case decodeScenarioBytes catalog (encodeScenario sc) of
                ScenarioLoaded back [] →
                    if back ≡ sc then pure ()
                    else expectationFailure ("case " ⧺ show k ⧺ " decoded into another scenario:\n"
                                             ⧺ BC.unpack (encodeScenario sc))
                ScenarioLoaded _ ds → expectationFailure ("case " ⧺ show k ⧺ " decoded with " ⧺ show ds)
                ScenarioFailed f → expectationFailure ("case " ⧺ show k ⧺ " failed: " ⧺ show f)

    it "covers the cases that matter: a finite and an expandable map, both region shapes, every fluid kind, every optional field" $ do
        let scs = map scenarioOf [1 .. 500]
        any (isJust ∘ scMap) scs `shouldBe` True
        any (isNothing ∘ scMap) scs `shouldBe` True
        any (any isRect ∘ map tpRegion ∘ scTerrain) scs `shouldBe` True
        any (any isTiles ∘ map tpRegion ∘ scTerrain) scs `shouldBe` True
        map fromEnum (nub [ fpKind f | s ← scs, f ← scFluids s ]) `shouldMatchList` map fromEnum [minBound .. maxBound ∷ FluidKind]
        any (any ((≢ Omitted) ∘ feRegrowth) ∘ scFlora) scs `shouldBe` True
        any (any ((≡ Omitted) ∘ feHealth) ∘ scFlora) scs `shouldBe` True
        any (any ((≢ []) ∘ tpTags) ∘ scTerrain) scs `shouldBe` True

    it "writes the same bytes every time, and names the version first" $
        forM_ [1 .. 50 ∷ Int] $ \k → do
            let sc = scenarioOf k
            encodeScenario sc `shouldBe` encodeScenario sc
            BC.unpack (BC.takeWhile (≢ '\n') (encodeScenario sc)) `shouldBe` "version: " ⧺ show currentScenarioVersion
  where
    isRect r = case r of RegionRect {} → True; _ → False
    isTiles r = case r of RegionTiles _ → True; _ → False
