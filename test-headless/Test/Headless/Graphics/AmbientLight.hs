{-# LANGUAGE UnicodeSyntax #-}
-- | 'computeAmbientLight' periodicity (#483 review follow-up). The
--   bindless world vertex/fragment shaders now interpolate an
--   UNWRAPPED longitude-local sun angle across a quad (no fract()
--   before interpolation — see ShaderCode.hs) and rely on this exact
--   Haskell function's GLSL port being periodic with period 1 for any
--   real input, not just [0,1). If a future change makes
--   'computeAmbientLight' non-periodic (e.g. adds a term outside
--   sin()/cos()), the shader-side wrap-avoidance silently breaks —
--   this pins the property down.
module Test.Headless.Graphics.AmbientLight (spec) where

import UPrelude
import Test.Hspec
import Engine.Loop.Frame (computeAmbientLight)

spec ∷ Spec
spec = describe "computeAmbientLight" $
    it "is periodic with period 1 for any real input (not just [0,1))" $
        mapM_ (\x → mapM_ (\n →
                    let a = computeAmbientLight (x + fromIntegral (n ∷ Int))
                        b = computeAmbientLight x
                    in abs (a - b) `shouldSatisfy` (< 1.0e-5))
                    [-3, -2, -1, 1, 2, 3])
              [0.0, 0.1, 0.25, 0.49, 0.5, 0.51, 0.75, 0.9, 0.99, -0.4, 1.4]
