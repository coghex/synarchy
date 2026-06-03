{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for World.Magma.Shape — the per-shape 'pointInShape'
--   inclusion test for each 'LavaShape' variant. The world wrap
--   parameter is fixed at 1024 (worldSize 64 × chunkSize 16) for
--   every test; none of the test shapes sit close enough to the
--   seam for wrapping to matter.
module Test.Headless.Magma.Shape (spec) where

import UPrelude
import Test.Hspec
import World.Magma.Types (LavaShape(..))
import World.Magma.Shape (pointInShape, shapeZBottom, shapeZTop)

-- | World size used for wrappedDeltaUV — large enough that wrap does
--   not interfere with any of the local shapes in these tests.
ws ∷ Int
ws = 64

inside, outside ∷ LavaShape → Int → Int → Int → Bool
inside  s x y z = pointInShape ws x y z s
outside s x y z = not (pointInShape ws x y z s)

spec ∷ Spec
spec = do
    describe "Cylindrical" $ do
        let cyl = Cylindrical 0 0 (-100) 50 5.0
        it "centre is inside" $
            inside cyl 0 0 0 `shouldBe` True
        it "well within radius is inside" $
            inside cyl 3 0 25 `shouldBe` True
        it "outside radius is outside" $
            outside cyl 10 0 0 `shouldBe` True
        it "exactly on radius is inside (≤)" $
            inside cyl 5 0 0 `shouldBe` True
        it "below z range is outside" $
            outside cyl 0 0 (-200) `shouldBe` True
        it "above z range is outside" $
            outside cyl 0 0 100 `shouldBe` True
        it "at z bottom is inside" $
            inside cyl 0 0 (-100) `shouldBe` True
        it "at z top is inside" $
            inside cyl 0 0 50 `shouldBe` True

    describe "Conical" $ do
        -- Cone from radius 10 at z=0 to radius 2 at z=100.
        let cone = Conical 0 0 0 100 10.0 2.0
        it "wide base — point at r=8, z=0 is inside" $
            inside cone 8 0 0 `shouldBe` True
        it "wide base — point at r=11, z=0 is outside" $
            outside cone 11 0 0 `shouldBe` True
        it "narrow top — point at r=8, z=100 is outside" $
            outside cone 8 0 100 `shouldBe` True
        it "narrow top — point at r=1, z=100 is inside" $
            inside cone 1 0 100 `shouldBe` True
        it "midpoint z=50 — r=6 (mid radius 6) is inside" $
            inside cone 6 0 50 `shouldBe` True
        it "midpoint z=50 — r=8 is outside" $
            outside cone 8 0 50 `shouldBe` True
        it "below z range is outside" $
            outside cone 0 0 (-10) `shouldBe` True

    describe "Perturbed" $ do
        -- Zero amplitude collapses to a 4-radius cylinder.
        let perfectCyl = Perturbed 0 0 0 100 4.0 0.0 0.0 0.0
        it "amp=0 acts like a cylinder — centre inside" $
            inside perfectCyl 0 0 50 `shouldBe` True
        it "amp=0 acts like a cylinder — beyond radius outside" $
            outside perfectCyl 10 0 50 `shouldBe` True
        it "below z range is outside" $
            outside perfectCyl 0 0 (-5) `shouldBe` True
        it "above z range is outside" $
            outside perfectCyl 0 0 105 `shouldBe` True
        -- With a small amplitude, the centre at z=0 stays near (0,0).
        let wiggle = Perturbed 0 0 0 100 4.0 1.0 0.5 0.0
        it "wiggle centre inside even with small amplitude" $
            inside wiggle 0 0 0 `shouldBe` True

    describe "Slot" $ do
        -- Horizontal slot along x from 0 to 100 at y=0, width 6.
        let slot = Slot 0 0 100 0 (-50) 50 6.0
        it "midpoint of slot is inside" $
            inside slot 50 0 0 `shouldBe` True
        it "near start is inside" $
            inside slot 10 0 0 `shouldBe` True
        it "near end is inside" $
            inside slot 90 0 0 `shouldBe` True
        it "off to the side beyond half-width is outside" $
            outside slot 50 5 0 `shouldBe` True
        it "on the centerline at half-width is inside" $
            inside slot 50 3 0 `shouldBe` True
        it "before start (cap) beyond half-width is outside" $
            outside slot (-10) 0 0 `shouldBe` True
        it "after end (cap) beyond half-width is outside" $
            outside slot 110 0 0 `shouldBe` True
        it "below z range is outside" $
            outside slot 50 0 (-100) `shouldBe` True
        it "above z range is outside" $
            outside slot 50 0 100 `shouldBe` True
        -- Degenerate slot: start == end → behaves as a disc.
        let dot = Slot 10 20 10 20 0 10 8.0
        it "degenerate slot — centre inside" $
            inside dot 10 20 5 `shouldBe` True
        it "degenerate slot — point beyond radius is outside" $
            outside dot 20 20 5 `shouldBe` True

    describe "EllipsoidChamber" $ do
        -- Axis-aligned ellipsoid centred at origin, rx=10 ry=5 rz=3.
        let ell = EllipsoidChamber 0 0 0 10.0 5.0 3.0
        it "centre is inside" $
            inside ell 0 0 0 `shouldBe` True
        it "along longest axis (x) at r=10 is inside" $
            inside ell 10 0 0 `shouldBe` True
        it "along longest axis (x) at r=11 is outside" $
            outside ell 11 0 0 `shouldBe` True
        it "along y axis at r=5 is inside" $
            inside ell 0 5 0 `shouldBe` True
        it "along y axis at r=6 is outside" $
            outside ell 0 6 0 `shouldBe` True
        it "along z axis at r=3 is inside" $
            inside ell 0 0 3 `shouldBe` True
        it "along z axis at r=4 is outside" $
            outside ell 0 0 4 `shouldBe` True
        -- Point at (5, 0, 0) should be inside — (5/10)² + 0 + 0 = 0.25
        it "halfway along x is inside" $
            inside ell 5 0 0 `shouldBe` True

    describe "IrregularChamber" $ do
        -- Spherical chamber with mild perturbation. With a base
        -- radius of 20 and amplitude 2, every point within radius 17
        -- (well below r-amp = 18) must be inside and every point
        -- beyond radius 23 (well above r+amp = 22) must be outside.
        let irr = IrregularChamber 0 0 0 20.0 2.0 0.1 0xDEADBEEF
        it "centre is inside" $
            inside irr 0 0 0 `shouldBe` True
        it "well inside (r=10) is inside" $
            inside irr 10 0 0 `shouldBe` True
        it "well outside (r=30) is outside" $
            outside irr 30 0 0 `shouldBe` True
        it "well inside on the y axis is inside" $
            inside irr 0 10 0 `shouldBe` True
        it "well inside on the z axis is inside" $
            inside irr 0 0 10 `shouldBe` True
        it "well outside on the y axis is outside" $
            outside irr 0 40 0 `shouldBe` True

    describe "shapeZBottom / shapeZTop" $ do
        it "Cylindrical bounds match constructor args" $ do
            let c = Cylindrical 0 0 (-50) 30 1.0
            shapeZBottom c `shouldBe` (-50)
            shapeZTop    c `shouldBe`  30
        it "EllipsoidChamber bounds account for rz" $ do
            let e = EllipsoidChamber 0 0 100 5.0 5.0 7.0
            shapeZBottom e `shouldBe` 93   -- 100 - ceiling 7
            shapeZTop    e `shouldBe` 107  -- 100 + ceiling 7
        it "IrregularChamber bounds account for r + amp" $ do
            let i = IrregularChamber 0 0 50 10.0 3.0 0.1 1
            shapeZBottom i `shouldBe` 37   -- 50 - ceiling 13
            shapeZTop    i `shouldBe` 63   -- 50 + ceiling 13
