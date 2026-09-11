module Test.Headless.Audio.Spatial (spec) where

import UPrelude
import Engine.Audio.Config.Runtime
import Engine.Audio.Spatial
import Engine.Audio.Types
import Engine.Graphics.Camera (CameraFacing(..))
import World.Page.Types (WorldPageId(..))
import Test.Hspec

listener ∷ ListenerSnapshot
listener = ListenerSnapshot (WorldPageId "world") 0 0 0 FaceSouth 0.25 256 0 0.25 1.2 1.6

near ∷ Float → Float → Expectation
near actual expected = abs (actual - expected) `shouldSatisfy` (< 0.0001)

spec ∷ Spec
spec = describe "Audio.Spatial" $ do
  it "uses the cylinder's shortest U alias without wrapping its finite V axis" $ do
    let observer = listener { listenerX = 63, listenerY = -63 }
        source = AudioPosition (WorldPageId "world") (-63) 63 3
    case relativePosition observer source of
      Just (x, y, z) → near x (2 * sqrt 2) >> near y 0 >> near z 3
      Nothing → expectationFailure "same-page seam source rejected"
    relativePosition listener (AudioPosition (WorldPageId "other") 0 0 0) `shouldBe` Nothing
    shortest 0 600 `shouldBe` 600

  it "pans along visible screen directions for all four camera facings" $ do
    let expected = [(FaceSouth, (sqrt 2, 0)), (FaceWest, (0, -sqrt 2)),
                    (FaceNorth, (-sqrt 2, 0)), (FaceEast, (0, sqrt 2))]
    forM_ expected $ \(facing, (ex, ey)) → do
      let (x, y) = screenFrame facing 1 (-1)
      near x ex; near y ey
      near (sqrt (x * x + y * y)) (sqrt 2)

  it "keeps an anchored source consistent through camera motion, rotation, and seam crossing" $ do
    let old = listener { listenerX = 63, listenerY = -63 }
        new = listener { listenerX = -63, listenerY = 63, listenerZ = 2, listenerFacing = FaceWest }
        position = AudioPosition (WorldPageId "world") (-61) 62 4
    case (relativePosition old position, relativePosition new position, listenerRebase old new) of
      (Just (x,y,z), Just (nx,ny,nz), Just ((a,b,c,d),(e,f,g,h),(i,j,k,l))) → do
        near (a*x+b*y+c*z+d) nx
        near (e*x+f*y+g*z+h) ny
        near (i*x+j*y+k*z+l) nz
      _ → expectationFailure "small wrapped movement should produce an affine rebase"
    listenerRebase listener (listener { listenerPage = WorldPageId "other" }) `shouldBe` Nothing
    listenerRebase listener (listener { listenerX = 200, listenerY = 200 }) `shouldBe` Nothing

  it "interpolates detail range/gain and reaches exact zero at the map fade boundary" $ do
    let (closeRange, closeGain) = zoomTargets defaultRuntimeConfig listener
        (farRange, farGain) = zoomTargets defaultRuntimeConfig (listener { listenerZoom = 1.2 })
        (_, middle) = zoomTargets defaultRuntimeConfig (listener { listenerZoom = 1.4 })
        (_, mapGain) = zoomTargets defaultRuntimeConfig (listener { listenerZoom = 1.6 })
    near closeRange 0.85; near closeGain (10 ** (1.5 / 20))
    near farRange 1.5; near farGain (10 ** ((-2) / 20))
    near middle (farGain / 2)
    mapGain `shouldBe` 0
