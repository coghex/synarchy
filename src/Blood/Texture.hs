{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Procedural blood texture pixel generation (#606): turns a
--   'BloodTextureDescriptor' into RGBA8 pixel data.
--
--   Deterministic from the descriptor's fields — 'descriptorSeed' folds
--   every field into one 'StdGen' seed, so the same descriptor always
--   produces byte-identical output, and any field difference (style,
--   wound kind, severity, footprint, anisotropy, edge, seed) shifts the
--   RNG stream and/or the splat layout enough to produce visibly and
--   byte-distinct pixels. Style and severity additionally drive the
--   splat COUNT/shape/color directly (not just the RNG stream), so those
--   two dimensions are guaranteed structurally different, not just
--   probably different.
--
--   Pure — no GPU/Vulkan here at all. 'World.Render.BloodQuads' uploads
--   what this produces to the bindless texture system; the headless
--   debug surface (Engine.Scripting.Lua.API.Blood) can call this
--   directly to report generation without touching the GPU. See
--   docs/blood_decals.md.
module Blood.Texture
    ( BloodTextureImage(..)
    , bloodTextureDim
    , maxBloodTextureDim
    , generateBloodTexture
    , bloodTextureHash
    ) where

import UPrelude
import Data.Hashable (hash, hashWithSalt)
import qualified Data.ByteString as BS
import System.Random (StdGen, mkStdGen, randomR)
import Blood.Types

-- | RGBA8 pixel buffer, row-major, top-to-bottom. 'btiPixels' is always
--   exactly @btiWidth * btiHeight * 4@ bytes. Fully transparent
--   ('minBound' alpha) outside the generated blood shape.
data BloodTextureImage = BloodTextureImage
    { btiWidth  ∷ !Int
    , btiHeight ∷ !Int
    , btiPixels ∷ !BS.ByteString
    } deriving (Show, Eq)

-- | Canvas side length for a footprint bucket — square textures. Small
--   enough that "generated texture size is bounded" (issue #606
--   acceptance) holds trivially; see 'maxBloodTextureDim'.
bloodTextureDim ∷ FootprintBucket → Int
bloodTextureDim FootprintSmall  = 16
bloodTextureDim FootprintMedium = 24
bloodTextureDim FootprintLarge  = 32

-- | Hard upper bound on any generated texture's width/height — the
--   largest value 'bloodTextureDim' can return.
maxBloodTextureDim ∷ Int
maxBloodTextureDim = 32

-- | One rough-edged soft circle in the texture's LOCAL coordinate frame
--   (see 'toLocal') — the shared primitive every style below composes
--   into a pool, a scatter of drops/spatter dots, or a streak/smear
--   chain. The edge-roughness harmonics ('spF1'/'spPh1'/'spF2'/'spPh2')
--   perturb the circle's radius by angle so 'EdgeRough' descriptors read
--   as jagged rather than perfectly round.
data Splat = Splat
    { spCx, spCy ∷ !Float
    , spR        ∷ !Float
    , spRough    ∷ !Float
    , spF1, spPh1, spF2, spPh2 ∷ !Float
    , spBright   ∷ !Float
    } deriving (Show, Eq)

-- | Fold every descriptor field into one seed. Two descriptors equal in
--   every field always fold to the same value; changing any one field
--   (including free-form 'btdWoundKind' text) changes it.
descriptorSeed ∷ BloodTextureDescriptor → Int
descriptorSeed d = foldl' hashWithSalt 0
    [ fromEnum (btdStyle d)
    , hash (btdWoundKind d)
    , fromEnum (btdSeverity d)
    , fromEnum (btdFootprint d)
    , fromEnum (btdAnisotropy d)
    , fromEnum (btdEdge d)
    , btdSeed d
    ]

-- | Edge-roughness amplitude as a fraction of a splat's own radius.
edgeFactor ∷ EdgeBucket → Float
edgeFactor EdgeSmooth   = 0.05
edgeFactor EdgeModerate = 0.15
edgeFactor EdgeRough    = 0.32

severityIndex ∷ SeverityBucket → Int
severityIndex = fromEnum

roll ∷ (Float, Float) → StdGen → (Float, StdGen)
roll = randomR

-- | Draw a splat's shared roughness-harmonic + brightness fields once
--   the caller has already picked its center and radius.
finishSplat ∷ Float → Float → Float → Float → StdGen → (Splat, StdGen)
finishSplat cx cy r ef g0 =
    let roughAmp  = r * ef
        (f1, g1)  = roll (3, 6) g0
        (ph1, g2) = roll (0, 2 * pi) g1
        (f2, g3)  = roll (7, 11) g2
        (ph2, g4) = roll (0, 2 * pi) g3
        (br, g5)  = roll (0, 1) g4
    in (Splat cx cy r roughAmp f1 ph1 f2 ph2 br, g5)

-- | A splat scattered within a disc of radius @maxR@ around the local
--   origin (sqrt-radius sampling for even area coverage, not a corner-
--   biased box), with its own radius drawn from @(rMin, rMax)@.
scatterSplat ∷ Float → (Float, Float) → Float → StdGen → (Splat, StdGen)
scatterSplat maxR (rMin, rMax) ef g0 =
    let (u, g1)   = roll (0, 1) g0
        (ang, g2) = roll (0, 2 * pi) g1
        rad       = sqrt u * maxR
        cx        = rad * cos ang
        cy        = rad * sin ang
        (r, g3)   = roll (rMin, rMax) g2
    in finishSplat cx cy r ef g3

genMany ∷ Int → (StdGen → (Splat, StdGen)) → StdGen → ([Splat], StdGen)
genMany n f = go n []
  where
    go 0 acc g = (reverse acc, g)
    go k acc g = let (s, g') = f g in go (k - 1) (s : acc) g'

-- | A chain of @count@ splats laid out deterministically along local
--   x from @-halfLen@ to @halfLen@ (only their roughness/brightness is
--   randomized) — the streak/smear primitive. @radiusAtT@ gives each
--   link's radius from its position @t ∈ [-1, 1]@ along the chain.
chainSplats ∷ Float → Int → (Float → Float) → Float → StdGen
           → ([Splat], StdGen)
chainSplats halfLen count radiusAtT ef = go 0 []
  where
    go i acc g
        | i ≥ count = (reverse acc, g)
        | otherwise =
            let t = if count ≤ 1 then 0
                    else 2 * fromIntegral i / fromIntegral (count - 1) - 1
                cx = t * halfLen
                r  = radiusAtT t
                (s, g') = finishSplat cx 0 r ef g
            in go (i + 1) (s : acc) g'

-- | The splat layout for one descriptor, in the texture's local
--   (pre-anisotropy) coordinate frame. Style picks the shape family;
--   severity scales count/size within it. Placement radius @pr@ leaves
--   an inset margin so most splats stay within the canvas.
styleSplats ∷ BloodTextureDescriptor → StdGen → ([Splat], StdGen)
styleSplats d g0 =
    let dim  = bloodTextureDim (btdFootprint d)
        half = fromIntegral dim / 2 ∷ Float
        pr   = half * 0.75
        ef   = edgeFactor (btdEdge d)
        sevI = severityIndex (btdSeverity d)
        sevF = fromIntegral sevI ∷ Float
    in case btdStyle d of
        StylePool →
            let n = 1 + sevI `div` 2
                sevScale = 1 + 0.12 * sevF
                rMin = pr * 0.5 * sevScale
                rMax = pr * 0.72 * sevScale
            in genMany n (scatterSplat (pr * 0.18) (rMin, rMax) ef) g0
        StyleDrops →
            let n = 3 + 2 * sevI
            in genMany n (scatterSplat pr (pr * 0.07, pr * 0.17) ef) g0
        StyleSpatter →
            let n = 15 + 10 * sevI
            in genMany n (scatterSplat pr (pr * 0.025, pr * 0.08) ef) g0
        StyleStreak →
            let n = 7
                sevScale = 1 + 0.1 * sevF
                radiusAtT t = pr * 0.09 * sevScale * (0.3 + 0.7 * cos (t * pi / 2))
            in chainSplats pr n radiusAtT ef g0
        StyleSmear →
            let n = 6
                sevScale = 1 + 0.1 * sevF
                halfLen = pr * 0.65 * sevScale
                radiusAtT t = pr * 0.20 * sevScale * (0.7 + 0.3 * cos (t * pi / 2))
            in chainSplats halfLen n radiusAtT ef g0

-- | (stretch, squash) applied along/across the texture's seeded
--   direction — the shared anisotropy transform every style's pixel
--   evaluation goes through (see 'toLocal'), so 'AnisotropyHigh' reads
--   as more directional regardless of style.
anisotropyParams ∷ AnisotropyBucket → (Float, Float)
anisotropyParams AnisotropyNone = (1.0, 1.0)
anisotropyParams AnisotropyLow  = (1.5, 0.8)
anisotropyParams AnisotropyHigh = (2.2, 0.6)

-- | Map a canvas pixel (centered, screen-space) into the splats' local
--   frame: rotate by the texture's seeded direction, then stretch/
--   squash along/across it. Dividing by @stretch@ here means a WIDER
--   screen-space range along that axis still falls inside a splat's
--   local-space radius — i.e. the rendered shape reads as elongated
--   along the seeded direction, without the splats themselves needing
--   to know about anisotropy at all.
toLocal ∷ Float → Float → Float → Float → Float → (Float, Float)
toLocal angle stretch squash x y =
    let ca = cos (-angle)
        sa = sin (-angle)
        rx = x * ca - y * sa
        ry = x * sa + y * ca
    in (rx / stretch, ry / squash)

-- | Soft-edged, roughness-perturbed coverage of @s@ at local point
--   (lx, ly): 1 well inside the splat, 0 well outside, with the
--   boundary itself pushed in/out by angle via the roughness harmonics.
splatAlpha ∷ Splat → Float → Float → Float
splatAlpha s lx ly =
    let dx  = lx - spCx s
        dy  = ly - spCy s
        d   = sqrt (dx * dx + dy * dy)
        ang = atan2 dy dx
        edgeR = spR s + spRough s *
            ( 0.6 * sin (spF1 s * ang + spPh1 s)
            + 0.4 * sin (spF2 s * ang + spPh2 s) )
        soft = 1.3
    in clamp01 (1 - (d - edgeR) / soft)

-- | Base RGB before per-pixel brightness jitter — a dark, slightly more
--   saturated red as severity rises. Aging (wet → dry) is handled
--   per-decal at render time (Blood.Render), not baked in here.
baseColor ∷ SeverityBucket → (Float, Float, Float)
baseColor sev =
    let t = fromIntegral (severityIndex sev) / 3 ∷ Float
    in (0.62 - 0.12 * t, 0.03 + 0.01 * t, 0.03)

-- | Generate the RGBA8 pixel data for @d@. Fully deterministic: the
--   only input is the descriptor itself (via 'descriptorSeed').
generateBloodTexture ∷ BloodTextureDescriptor → BloodTextureImage
generateBloodTexture d =
    let dim  = bloodTextureDim (btdFootprint d)
        half = fromIntegral dim / 2 ∷ Float
        gen0 = mkStdGen (descriptorSeed d)
        (angle, gen1)      = roll (0, 2 * pi) gen0
        (splats, _)        = styleSplats d gen1
        (stretch, squash)  = anisotropyParams (btdAnisotropy d)
        (baseR, baseG, baseB) = baseColor (btdSeverity d)

        w8 v = round (clamp01 v * 255) ∷ Word8

        pixelAt px py =
            let x = fromIntegral px - half + 0.5 ∷ Float
                y = fromIntegral py - half + 0.5 ∷ Float
                (lx, ly) = toLocal angle stretch squash x y
                step best@(bestA, _) s =
                    let av = splatAlpha s lx ly
                    in if av > bestA then (av, spBright s) else best
                (aRaw, br) = foldl' step (0, 0.5) splats
                alpha = clamp01 aRaw
            in if alpha ≤ 0.004
               then (0, 0, 0, 0)
               else
                   let shade = 0.82 + 0.32 * br
                   in ( w8 (baseR * shade), w8 (baseG * shade)
                      , w8 (baseB * shade), w8 alpha )

        bytes = concat
            [ [r, g, b, a]
            | py ← [0 .. dim - 1], px ← [0 .. dim - 1]
            , let (r, g, b, a) = pixelAt px py
            ]
    in BloodTextureImage dim dim (BS.pack bytes)

-- | A hash of a generated texture's dimensions + pixel bytes, for tests
--   that want to compare "same data" / "different data" without
--   asserting on raw bytes.
bloodTextureHash ∷ BloodTextureImage → Int
bloodTextureHash img = hash (btiWidth img, btiHeight img, btiPixels img)
