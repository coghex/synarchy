{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for the spoil-pile deposit router (World.Spoil.Types).
--   The router takes a tile-legality predicate as a closure, so we can
--   exercise it with no world state.
--
--   The key regression here is the TILE-level no-mixing rule: a tile's
--   four corners live in four different vertices, and promotion
--   converts the whole cell to a single material, so the router must
--   never let one material land on a corner of a tile already holding a
--   different spoil material. (Before the fix, granite and loam could
--   fill different corners of one tile and promotion silently converted
--   the lot to whichever corner came first.)
module Test.Headless.World.Spoil (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import World.Material.Id (MaterialId(..))
import World.Spoil.Types

granite, loam ∷ MaterialId
granite = MaterialId 1
loam    = MaterialId 2

-- | Local slot accessor (the module's own slotGet is not exported).
sget ∷ Int → (Float, Float, Float, Float) → Float
sget 0 (a, _, _, _) = a
sget 1 (_, b, _, _) = b
sget 2 (_, _, c, _) = c
sget _ (_, _, _, d) = d

-- | All four corners of a tile, each as the fill currently held.
tileCornerFills ∷ SpoilPiles → (Int, Int) → [(MaterialId, Float)]
tileCornerFills piles tile =
    [ (spMat p, sget slot (spFill p))
    | (v, slot) ← tileCornerVertices tile
    , Just p ← [HM.lookup v piles] ]

spec ∷ Spec
spec = do
    describe "depositSpoil tile-level no-mixing" $ do
        -- Seed tile (0,0)'s NW corner with granite, then pour loam.
        let piles0  = HM.singleton (0, 0) (SpoilPile granite (0, 0, 0, 0.5))
            (piles', _leftover) =
                depositSpoil (const True) loam (0, 0) 4.0 piles0

        it "never places a foreign material on a tile already claimed" $
            -- Tile (0,0) holds granite in its NW corner; no corner of
            -- it may end up with loam fill.
            all (\(m, f) → m ≢ loam ∨ f ≡ 0)
                (tileCornerFills piles' (0, 0))
              `shouldBe` True

        it "leaves the pre-existing granite corner intact" $
            (spMat <$> HM.lookup (0, 0) piles') `shouldBe` Just granite

        it "still places the loam somewhere (deposit not wholly refused)" $
            -- Loam must go on neighbouring uncontested tiles.
            any (\(_, p) → spMat p ≡ loam ∧ spFill p ≢ (0, 0, 0, 0))
                (HM.toList piles')
              `shouldBe` True

    describe "depositSpoil same-material stacking" $ do
        -- Same material onto the seeded granite vertex must not be
        -- blocked by the no-mixing rule.
        let piles0 = HM.singleton (0, 0) (SpoilPile granite (0, 0, 0, 0.5))
            (_, leftover) =
                depositSpoil (const True) granite (0, 0) 1.0 piles0

        it "accepts more of the same material (no spurious refusal)" $
            leftover `shouldBe` 0

    describe "promotion mechanics" $ do
        -- A tile whose four corners are each at a full level is
        -- promotable, and a debit drops exactly one level per corner.
        let full = 1.0
            piles = HM.fromList
                [ ((0, 0), SpoilPile granite (0, 0, 0, full))   -- NW slot 3
                , ((1, 0), SpoilPile granite (0, 0, full, 0))   -- NE slot 2
                , ((1, 1), SpoilPile granite (full, 0, 0, 0))   -- SE slot 0
                , ((0, 1), SpoilPile granite (0, full, 0, 0))   -- SW slot 1
                ]

        it "reports a fully-filled tile as promotable" $
            promotableTiles piles (candidateVertices (0, 0))
              `shouldContain` [(0, 0)]

        it "debits one full level from each corner on promotion" $
            -- Each corner was exactly 1.0; debiting a full level empties
            -- them, so every contributing pile is dropped.
            HM.null (debitPromotedTile (0, 0) piles) `shouldBe` True
