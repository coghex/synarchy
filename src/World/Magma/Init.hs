{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Magma.Init
    ( buildVolcanoCtx
    , buildSpatialIndex
    , buildMagmaSource
    , msBBoxFromShapes
    , maxChuteReach
    , unionBoxes
    , squareAt
    , padBox
    , discoverChunkLava
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal (wrapChunkCoordU)
import World.Geology.Hash (wrappedDeltaUV)
import World.Plate.Types (TectonicPlate)
import World.Plate (elevationAtGlobal)
import World.Geology.Timeline.Types
    ( PersistentFeature(..)
    , FeatureShape(..)
    , FeatureActivity(..)
    , VolcanicFeature(..)
    , EventBBox(..)
    , noBBox
    , FissureParams(..)
    , LavaTubeParams(..)
    , ShieldParams(..)
    , CinderConeParams(..)
    , LavaDomeParams(..)
    , CalderaParams(..)
    , SuperVolcanoParams(..)
    , HydrothermalParams(..)
    )
import World.Magma.Types
    ( MagmaSource(..)
    , LavaShape(..)
    , VolcanoCtx(..)
    , MagmaOverlay(..)
    )
import World.Magma.Kit (perTypeKit, hotspotBoostFor, TerrainSampler)
import World.Magma.Field (mantleNoise, mantleBaseDepth)
import World.Magma.Lookup (lookupNearSources)
import World.Magma.Shape (pointInShape, shapeZTop)
import World.Constants (seaLevel)

-- | Tiles of padding around each source's bbox in the spatial index.
--   Generous enough to catch perturbed-chute wobble that extends
--   beyond the nominal shape bbox.
maxChuteReach ∷ Int
maxChuteReach = 16

-- | Build the world's volcano context. Filters @gtFeatures@ to active
--   and dormant volcanic features and derives a 'MagmaSource' for each
--   via the per-type kit. Not persisted — call at world-init and at
--   save-load (after 'gtFeatures' and 'gtSeed' are restored).
--
--   @plates@ is needed because some kits (currently just fissures)
--   sample the surface terrain via 'elevationAtGlobal' to anchor
--   their chute zTop.
buildVolcanoCtx ∷ Word64 → Int → [TectonicPlate]
                → [PersistentFeature] → VolcanoCtx
buildVolcanoCtx seed worldSize plates features =
    let terrainAt gx gy =
            fst (elevationAtGlobal seed plates worldSize gx gy)
        sourcesList = [ src
                      | pf ← features
                      , Just src ← [buildMagmaSource terrainAt seed
                                                      worldSize pf]
                      ]
        sources = V.fromList sourcesList
        idx     = buildSpatialIndex worldSize sourcesList
        hotIdx  = buildHotspotIndex worldSize sourcesList
    in VolcanoCtx
        { vcSources      = sources
        , vcIndex        = idx
        , vcHotspotIndex = hotIdx
        , vcSeed         = seed
        , vcWorldSize    = worldSize
        }

-- | Derive a 'MagmaSource' from a 'PersistentFeature'. Returns
--   @Nothing@ for hydro features, extinct / collapsed volcanoes, and
--   variants whose kit is empty (e.g. dry fissures).
buildMagmaSource ∷ TerrainSampler → Word64 → Int → PersistentFeature
                 → Maybe MagmaSource
buildMagmaSource terrainAt worldSeed worldSize pf = case pfFeature pf of
    HydroShape _ → Nothing
    VolcanicShape vf → case pfActivity pf of
        FExtinct   → Nothing
        FCollapsed → Nothing
        activity →
            let GeoFeatureId fidInt = pfId pf
                srcSeed = worldSeed
                          `xor` (fromIntegral fidInt `shiftL` 1)
                          `xor` 0xA17A5EED00420001
                centerCoord = featureCenter worldSize vf
                GeoCoord cx cy = centerCoord
                -- Use the noise-only baseline for kit construction:
                -- chutes run from the baseline up to the surface, and
                -- 'lavaAt' applies the full noisy + hotspot-uplifted
                -- mantle ceiling at query time. (Skipping the hotspot
                -- term avoids a chicken-and-egg ordering — we are in
                -- the middle of building the source list it would
                -- iterate.)
                mZ = mantleBaselineAt worldSeed worldSize cx cy
                shapes = perTypeKit terrainAt srcSeed mZ vf
            in if null shapes
               then Nothing
               else Just MagmaSource
                    { msFeatureId    = pfId pf
                    , msType         = vf
                    , msActivity     = activity
                    , msCenter       = centerCoord
                    , msShapes       = shapes
                    , msBBox         = msBBoxFromShapes worldSize shapes
                    , msHotspotBoost = hotspotBoostFor vf
                    }

-- | Approximate mantle Z at @(gx, gy)@ for kit construction. Uses
--   only the baseline + noise terms (skips the hotspot term, since
--   it depends on the source list we're in the middle of building).
mantleBaselineAt ∷ Word64 → Int → Int → Int → Int
mantleBaselineAt worldSeed _worldSize gx gy =
    mantleBaseDepth + mantleNoise gx gy worldSeed

-- | Centre coordinate for a 'VolcanicFeature'. Wrap-aware for the
--   two-endpoint variants (fissure, lava tube): naïve @(sx+ex)/2@
--   midpoint would land on the opposite side of the world for any
--   feature whose endpoints straddle the u-axis seam.
featureCenter ∷ Int → VolcanicFeature → GeoCoord
featureCenter worldSize vf = case vf of
    ShieldVolcano    p → shCenter p
    CinderCone       p → ccCenter p
    LavaDome         p → ldCenter p
    Caldera          p → caCenter p
    SuperVolcano     p → svCenter p
    HydrothermalVent p → htCenter p
    FissureVolcano   p → segmentMidpoint worldSize (fpStart p) (fpEnd p)
    LavaTube         p → segmentMidpoint worldSize (ltStart p) (ltEnd p)

-- | Midpoint of a line segment under u-axis wrap. Walks half the
--   shortest delta from start toward end, so the result is always
--   between the two endpoints (never on the far side of the world).
segmentMidpoint ∷ Int → GeoCoord → GeoCoord → GeoCoord
segmentMidpoint worldSize (GeoCoord sx sy) (GeoCoord ex ey) =
    let (dx, dy) = wrappedDeltaUV worldSize ex ey sx sy
    in GeoCoord (sx + dx `div` 2) (sy + dy `div` 2)

-- * Bounding boxes

-- | Union of every shape's xy footprint, padded by 'maxChuteReach'
--   to absorb perturbed-chute wobble. Z extent is tracked per-shape
--   inside the shape itself.
msBBoxFromShapes ∷ Int → [LavaShape] → EventBBox
msBBoxFromShapes worldSize shapes =
    padBox (unionBoxes (map (shapeBBox worldSize) shapes)) maxChuteReach

shapeBBox ∷ Int → LavaShape → EventBBox
shapeBBox worldSize shape = case shape of
    Cylindrical x y _ _ r →
        squareAt x y (max 1 (ceiling r))
    Conical x y _ _ rb rt →
        squareAt x y (max 1 (ceiling (max rb rt)))
    Perturbed x y _ _ br amp _ _ →
        squareAt x y (max 1 (ceiling (br + amp)))
    Slot sx sy ex ey _ _ w →
        -- Wrap-aware: take the shortest-path delta from start to
        -- end so a seam-crossing fissure produces a tight bbox
        -- anchored at @sx, sy@ instead of one spanning the world.
        let hw = max 1 (ceiling (w * 0.5))
            (dx, dy) = wrappedDeltaUV worldSize ex ey sx sy
            ex' = sx + dx
            ey' = sy + dy
        in EventBBox (min sx ex' - hw) (min sy ey' - hw)
                     (max sx ex' + hw) (max sy ey' + hw)
    EllipsoidChamber x y _ rx ry _ →
        EventBBox (x - ceiling rx) (y - ceiling ry)
                  (x + ceiling rx) (y + ceiling ry)
    IrregularChamber x y _ rx ry _ amp _ _ →
        let padX = max 1 (ceiling (rx * (1.0 + amp)))
            padY = max 1 (ceiling (ry * (1.0 + amp)))
        in EventBBox (x - padX) (y - padY) (x + padX) (y + padY)

squareAt ∷ Int → Int → Int → EventBBox
squareAt x y r = EventBBox (x - r) (y - r) (x + r) (y + r)

unionBoxes ∷ [EventBBox] → EventBBox
unionBoxes [] = noBBox
unionBoxes (b:bs) = foldr merge b bs
  where
    merge (EventBBox a1 b1 c1 d1) (EventBBox a2 b2 c2 d2) =
        EventBBox (min a1 a2) (min b1 b2) (max c1 c2) (max d1 d2)

padBox ∷ EventBBox → Int → EventBBox
padBox (EventBBox a b c d) n = EventBBox (a - n) (b - n) (c + n) (d + n)

-- * Spatial index

-- | Bucket sources by chunk so per-tile lookups iterate at most a
--   handful of candidates. Build cost is @O(sources × chunks-in-bbox)@;
--   chunks-in-bbox is bounded by the bbox padded by 'maxChuteReach'.
--
--   Chunk coords are u-axis wrapped before insertion so a source near
--   the seam has the wrapped-around chunk on the other side in its
--   candidate list — without this, seam-adjacent volcanoes would
--   silently fail to produce lava in their wrapped neighbours.
buildSpatialIndex ∷ Int → [MagmaSource] → HM.HashMap ChunkCoord [Int]
buildSpatialIndex worldSize sources =
    indexFromBoxes worldSize (zipWith (\i s → (i, msBBox s)) [0..] sources)

-- | Wider index used by 'sumHotspots'. Each source's bbox is padded
--   by @3σ@ where @σ = 2 × surface radius@, i.e. @6 ×@ the surface
--   radius — beyond that distance @exp(-d²/σ²)@ contributes <1%.
buildHotspotIndex ∷ Int → [MagmaSource]
                  → HM.HashMap ChunkCoord [Int]
buildHotspotIndex worldSize sources =
    indexFromBoxes worldSize
        [ (i, padBox (msBBox s) (hotspotReach s))
        | (i, s) ← zip [0..] sources
        ]

-- | Hotspot Gaussian reach in tiles. Beyond this distance the
--   contribution is negligible (< 1% of 'msHotspotBoost').
hotspotReach ∷ MagmaSource → Int
hotspotReach s = ceiling (6.0 * surfaceRadius)
  where
    surfaceRadius = case msType s of
        ShieldVolcano    p → fromIntegral (shBaseRadius p)
        CinderCone       p → fromIntegral (ccBaseRadius p)
        LavaDome         p → fromIntegral (ldBaseRadius p)
        Caldera          p → fromIntegral (caOuterRadius p)
        SuperVolcano     p → fromIntegral (svCalderaRadius p)
        HydrothermalVent p → fromIntegral (htRadius p)
        FissureVolcano   p → fromIntegral (fpWidth p)
        LavaTube         p → fromIntegral (ltWidth p)

-- | Shared chunk-bucketing helper used by both 'buildSpatialIndex'
--   and 'buildHotspotIndex'. Wraps chunk coords on the u-axis so
--   sources near the seam appear under their canonical neighbour
--   coords.
indexFromBoxes ∷ Int → [(Int, EventBBox)]
               → HM.HashMap ChunkCoord [Int]
indexFromBoxes worldSize boxed =
    foldr addOne HM.empty boxed
  where
    addOne (i, bb) acc =
        let (cxLo, cyLo, cxHi, cyHi) = bboxChunkRange bb
        in foldr (\cc m → HM.insertWith (⧺) cc [i] m) acc
                 [ wrapChunkCoordU worldSize (ChunkCoord cx cy)
                 | cx ← [cxLo .. cxHi]
                 , cy ← [cyLo .. cyHi]
                 ]

-- | Convert a tile-space bbox to the inclusive chunk-coord rectangle
--   it touches. Uses floor-division so negative coords land in the
--   correct chunk.
bboxChunkRange ∷ EventBBox → (Int, Int, Int, Int)
bboxChunkRange (EventBBox xlo ylo xhi yhi) =
    ( xlo `floorDiv` chunkSize
    , ylo `floorDiv` chunkSize
    , xhi `floorDiv` chunkSize
    , yhi `floorDiv` chunkSize
    )
  where
    floorDiv a b = let (q, r) = a `divMod` b
                   in if r < 0 then q - 1 else q

-- * Per-chunk discovery (Phase 2 hook)

-- | Walk every surface tile in the chunk; for each candidate source
--   covering this chunk, ask whether any shape contains
--   @(gx, gy, surfaceZ)@. Returns 'Nothing' if nothing was
--   discovered (the common case — most chunks have no nearby
--   sources).
--
--   Since the pool rework ('World.Magma.Pool'), this function emits
--   only BASALT CAPS — surface lava comes from the global
--   'gtWorldLavaPools' table read in @composeFluidMap@. Per-tile
--   decision when a breach is detected:
--
--   * Surface @≤ seaLevel@ → ALWAYS cap. The cap mechanism raises
--     the terrain to @min (seaLevel-1) (localTop+1)@ and stamps
--     basalt on top. For oceanic chunks the ocean then fills above
--     the cap; for inland chunks the result is an inert basalt
--     outcrop at sea level (no water). We don't gate on the
--     chunk-level ocean BFS here because that BFS pre-dates the
--     timeline's terrain edits — a chamber whose breach geometry
--     extends into a chunk that the BFS classified as inland (but
--     whose post-event terrain dropped below sea level) would
--     otherwise emit bare sub-sea lava with no water column above.
--     Capping unconditionally is the safe default; the shell + ocean
--     fill paths below still respect 'isOceanic' for what fluid (if
--     any) fills above the cap.
--
--   * Surface below a LAKE or RIVER water surface (@waterSurfMap@,
--     from the global tables) → cap at
--     @min (waterSurf-1) (localTop+1)@ so the water column stays
--     intact above sealed basalt. This is the user-decided
--     lava-vs-lake rule (2026-06-05): water wins below its surface,
--     contact lines turn to basalt.
--
--   * Dry above-water breach → nothing here; the pool table places
--     the visible lava.
discoverChunkLava ∷ VolcanoCtx
                  → ChunkCoord
                  → VU.Vector Int  -- ^ per-tile surface elevation
                  → VU.Vector Int  -- ^ per-tile lake/river water
                                   --   surface ('minBound' = none)
                  → Maybe MagmaOverlay
discoverChunkLava ctx coord surfaceMap waterSurfMap =
    let ChunkCoord cx cy = coord
        chunkGX = cx * chunkSize
        chunkGY = cy * chunkSize
        candidates = lookupNearSources ctx chunkGX chunkGY
        ws = vcWorldSize ctx
        addTile acc (lx, ly) =
            let idx = columnIndex lx ly
                gx  = chunkGX + lx
                gy  = chunkGY + ly
                surfZ = surfaceMap VU.! idx
                wSurf = waterSurfMap VU.! idx
                breachingShapes =
                    [ s
                    | i ← candidates
                    , let src = vcSources ctx V.! i
                    , s ← msShapes src
                    , pointInShape ws gx gy surfZ s
                    ]
            in case breachingShapes of
                [] → acc
                ss →
                    let localTop = maximum
                            (map (shapeTopAtXY ws gx gy surfZ) ss)
                        subSea  = surfZ ≤ seaLevel
                        subLake = wSurf ≠ minBound ∧ surfZ < wSurf
                        capLimit
                            | subSea ∧ subLake =
                                min (seaLevel - 1) (wSurf - 1)
                            | subSea    = seaLevel - 1
                            | otherwise = wSurf - 1
                        capZ = min capLimit (localTop + 1)
                    in if subSea ∨ subLake
                       then HM.insert (gx, gy) capZ acc
                       else acc
        capMap =
            if null candidates
            then HM.empty
            else foldr (flip addTile) HM.empty
                       [ (lx, ly)
                       | ly ← [0 .. chunkSize - 1]
                       , lx ← [0 .. chunkSize - 1]
                       ]
    in if HM.null capMap
       then Nothing
       else Just MagmaOverlay
            { moSurface   = HM.empty
            , moBasaltCap = capMap
            , moRevealed  = HM.empty
            }

-- | Walk up from a known-inside @startZ@ to find the highest z at
--   which this shape still contains @(gx, gy, z)@. Used by
--   'discoverChunkLava' to make a per-tile cap-vs-lava decision —
--   the shape's overall @shapeZTop@ would overstate the local reach
--   for chambers whose top tapers off near their edges.
--
--   Walks linearly upward and stops at the first @z@ outside; for
--   typical shape sizes (10–80 tiles tall) this is a few dozen
--   'pointInShape' calls per shape per breaching tile, which is
--   plenty fast given how sparse lava tiles are.
shapeTopAtXY ∷ Int → Int → Int → Int → LavaShape → Int
shapeTopAtXY ws gx gy startZ s =
    let zMax = shapeZTop s
        go lastInside z
            | z > zMax = lastInside
            | pointInShape ws gx gy z s = go z (z + 1)
            | otherwise = lastInside
    in go startZ (startZ + 1)

