module Math where

import UPrelude
import Numeric (readHex)
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))

readHex' ∷ ∀ α. (Num α, Eq α) ⇒ String → α
readHex' s = case readHex s of
  [(n, "")] → n
  _         → 0
colorToVec4 ∷ String → Vec4
colorToVec4 ('#':r1:r2:g1:g2:b1:b2:[]) =
  let r = fromIntegral (readHex' ([r1, r2]) :: Int) / 255.0
      g = fromIntegral (readHex' ([g1, g2]) :: Int) / 255.0
      b = fromIntegral (readHex' ([b1, b2]) :: Int) / 255.0
  in Vec4 r g b 1.0
colorToVec4 ('#':r1:r2:g1:g2:b1:b2:a1:a2:[]) =
  let r = fromIntegral (readHex' ([r1, r2]) :: Int) / 255.0
      g = fromIntegral (readHex' ([g1, g2]) :: Int) / 255.0
      b = fromIntegral (readHex' ([b1, b2]) :: Int) / 255.0
      a = fromIntegral (readHex' ([a1, a2]) :: Int) / 255.0
  in Vec4 r g b a
colorToVec4 ("red") = Vec4 1.0 0 0 1.0
colorToVec4 ("green") = Vec4 0 1.0 0 1.0
colorToVec4 ("blue") = Vec4 0 0 1.0 1.0
colorToVec4 ("white") = Vec4 1.0 1.0 1.0 1.0
colorToVec4 ("black") = Vec4 0 0 0 1.0
colorToVec4 ("yellow") = Vec4 1.0 1.0 0 1.0
colorToVec4 ("cyan") = Vec4 0 1.0 1.0 1.0
colorToVec4 ("magenta") = Vec4 1.0 0 1.0 1.0
colorToVec4 ("transparent") = Vec4 0 0 0 0.0
colorToVec4 ("opaque") = Vec4 0 0 0 0.5
colorToVec4 ("gray") = Vec4 0.5 0.5 0.5 1.0
colorToVec4 ("orange") = Vec4 1.0 0.65 0 1.0
colorToVec4 ("purple") = Vec4 0.5 0 0.5 1.0
colorToVec4 ("pink") = Vec4 1.0 0.75 0.8 1.0
colorToVec4 ("brown") = Vec4 0.6 0.4 0.2 1.0
colorToVec4 ("lime") = Vec4 0.0 1.0 0.0 1.0
colorToVec4 ("navy") = Vec4 0.0 0.0 0.5 1.0
colorToVec4 ("teal") = Vec4 0.0 0.5 0.5 1.0
colorToVec4 ("olive") = Vec4 0.5 0.5 0.0 1.0
colorToVec4 ("maroon") = Vec4 0.5 0.0 0.0 1.0
colorToVec4 ("silver") = Vec4 0.75 0.75 0.75 1.0
colorToVec4 ("gold") = Vec4 1.0 0.84 0.0 1.0
colorToVec4 ("beige") = Vec4 0.96 0.96 0.86 1.0
colorToVec4 ("coral") = Vec4 1.0 0.5 0.31 1.0
colorToVec4 ("salmon") = Vec4 0.98 0.5 0.45 1.0
colorToVec4 ("khaki") = Vec4 0.94 0.9 0.55 1.0
colorToVec4 ("orchid") = Vec4 0.85 0.44 0.84 1.0
colorToVec4 ("plum") = Vec4 0.87 0.63 0.87 1.0
colorToVec4 ("turquoise") = Vec4 0.25 0.88 0.82 1.0
colorToVec4 ("indigo") = Vec4 0.29 0.0 0.51 1.0
colorToVec4 ("violet") = Vec4 0.93 0.51 0.93 1.0
colorToVec4 ("azure") = Vec4 0.94 1.0 1.0 1.0
colorToVec4 ("ivory") = Vec4 1.0 1.0 0.94 1.0
colorToVec4 ("charcoal") = Vec4 0.21 0.27 0.31 1.0
colorToVec4 _ = Vec4 0 0 0 1.0
