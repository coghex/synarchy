{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Erosion.Math
    ( applyErosion
    , applyErosionLerp4
    ) where

import UPrelude
import World.Types
import World.Geology.Erosion.Sediment (erosionSediment, erosionSedimentLerp4)

-- * Erosion Application

-- | Apply time-based erosion to a tile position.
--   Smooths the tile's elevation toward the average of its
--   4 cardinal neighbors, gated by material hardness.
--
--   Four erosion modes combine to produce the final rate:
--
--   * Hydraulic (rainfall/runoff): dominant mode. Scales with
--     elevation difference (steeper = faster). Drives most
--     valley carving.
--
--   * Wind (aeolian): weak but steady. Less sensitive to slope,
--     flattens exposed surfaces. Strongest in arid regions.
--
--   * Thermal (freeze-thaw): shatters steep rock faces.
--     Scales with slope squared — very aggressive on cliffs
--     but negligible on gentle terrain.
--
--   * Chemical (dissolution): slow, uniform. Increases effective
--     erodability of softer rocks (limestone, shale). Barely
--     touches granite.
--
--   All four modes produce a delta toward the neighbor average.
--   They differ in how strongly they respond to slope and
--   material, which creates distinct erosion signatures:
--     - Wet climate:  deep V-valleys from hydraulic dominance
--     - Arid climate: gentle wind-smoothed plateaus
--     - Cold climate: jagged peaks from thermal shattering
--     - Warm/wet:     rounded limestone karst from chemical
{-# INLINE applyErosion #-}
applyErosion ∷ ErosionParams
             → Int       -- ^ worldSize
             → Int       -- ^ period duration (millions of years)
             → Float     -- ^ world scale factor (worldSize / 512)
             → Word8     -- ^ surface material ID at this tile
             → Float     -- ^ material hardness (0.0-1.0)
             → Int       -- ^ this tile's post-event elevation
             → (Int, Int, Int, Int)
                         -- ^ neighbor post-event elevations (N, S, E, W)
             → GeoModification
applyErosion params worldSize duration worldScale matId hardness elev nbrs =
    applyErosionScalar
        (epIntensity params) (epHydraulic params) (epThermal params)
        (epWind params) (epChemical params) (epIsLastAge params)
        (\isDep → erosionSediment params matId elev isDep)
        worldSize duration worldScale matId hardness elev nbrs

-- | Fast variant for the chunk-loop in 'World.Generate.Timeline.applyTimelineChunk':
--   takes the 4 climate-region-corner 'ErosionParams' plus the bilinear
--   interpolation factors @(tu, tv)@.  Lerps just the 5 \"hot\" Floats
--   that the erosion math actually consumes; defers the 4 sediment-only
--   Floats (temperature, precipitation, humidity, snow) into a closure
--   that 'erosionSedimentLerp4' lerps only if the result is actually
--   used (most tile-periods don't trigger a material override).
{-# INLINE applyErosionLerp4 #-}
applyErosionLerp4
    ∷ ErosionParams → ErosionParams → ErosionParams → ErosionParams
    → Float → Float                       -- ^ tu, tv
    → Int       -- ^ worldSize
    → Int       -- ^ period duration (millions of years)
    → Float     -- ^ world scale factor (worldSize / 512)
    → Word8     -- ^ surface material ID at this tile
    → Float     -- ^ material hardness (0.0-1.0)
    → Int       -- ^ this tile's post-event elevation
    → (Int, Int, Int, Int)
                -- ^ neighbor post-event elevations (N, S, E, W)
    → GeoModification
applyErosionLerp4 ep00 ep10 ep01 ep11 tu tv
                  worldSize duration worldScale matId hardness elev nbrs =
    let lerpHot f =
            let v0 = f ep00 + tu * (f ep10 - f ep00)
                v1 = f ep01 + tu * (f ep11 - f ep01)
            in v0 + tv * (v1 - v0)
    in applyErosionScalar
        (lerpHot epIntensity) (lerpHot epHydraulic) (lerpHot epThermal)
        (lerpHot epWind) (lerpHot epChemical) (epIsLastAge ep00)
        (\isDep → erosionSedimentLerp4
            ep00 ep10 ep01 ep11 tu tv matId elev isDep)
        worldSize duration worldScale matId hardness elev nbrs

-- | The actual erosion computation, parameterised on scalar climate
--   values + a sediment-callback. 'applyErosion' and 'applyErosionLerp4'
--   are thin wrappers that supply these.  NOT inlined — the body is
--   big and we want one copy of the work code.
applyErosionScalar
    ∷ Float      -- ^ intensity
    → Float      -- ^ hydraulic
    → Float      -- ^ thermal
    → Float      -- ^ wind
    → Float      -- ^ chemical
    → Bool       -- ^ isLastAge
    → (Bool → Word8)  -- ^ sediment callback (isDeposition → material)
    → Int        -- ^ worldSize
    → Int        -- ^ period duration (millions of years)
    → Float      -- ^ world scale factor
    → Word8      -- ^ surface material ID
    → Float      -- ^ material hardness
    → Int        -- ^ tile elevation
    → (Int, Int, Int, Int) -- ^ neighbor elevations
    → GeoModification
applyErosionScalar intensity hydraulic thermal wind chemical isLastAge
                   sedimentFn _worldSize duration worldScale _matId hardness elev
                   (nN, nS, nE, nW) = if hardness ≥ 1.0
       then noModification  -- indestructible (glacier, mantle)
       else
       let -- Average of 4 cardinal neighbors
           avgNeighbor = fromIntegral (nN + nS + nE + nW) / 4.0 ∷ Float
           diff = avgNeighbor - fromIntegral elev

           -- Common scaling factors
           erodability = 1.0 - hardness
           durationScale = fromIntegral duration / 5.0 ∷ Float
           scaleFactor = sqrt (max 0.1 worldScale)

           -- Slope magnitude: |neighbor average − elev| (NOT the max
           -- per-neighbor difference — a tile in a uniform sloped
           -- plane reads near zero here, by design).
           -- Used to modulate slope-sensitive erosion modes
           absDiff = abs diff
           slopeNorm = min 1.0 (absDiff / 30.0)  -- normalize: 30 tiles = max slope

           ---------------------------------------------------------
           -- Hydraulic erosion (rainfall/runoff)
           --   The dominant carver. Proportional to slope —
           --   water flows faster on steep terrain, carries more
           --   sediment. Flat areas barely erode hydraulically.
           --   epHydraulic is high when climate is wet (lots of rain).
           ---------------------------------------------------------
           hydraulicSlopeBoost = 0.4 + 0.6 * slopeNorm
               -- even flat terrain erodes a little (sheet wash),
               -- but steep terrain erodes 2.5× faster
           hydraulicRate = hydraulic
                         * erodability
                         * hydraulicSlopeBoost

           ---------------------------------------------------------
           -- Wind erosion (aeolian)
           --   Weak but nearly slope-independent. Grinds down
           --   exposed surfaces. Strongest in arid regions
           --   (high epWind). Barely affected by slope — wind
           --   hits everything equally.
           ---------------------------------------------------------
           windSlopeBoost = 0.8 + 0.2 * slopeNorm
               -- almost flat response, slight boost on ridges
           windRate = wind
                    * erodability
                    * windSlopeBoost

           ---------------------------------------------------------
           -- Thermal erosion (freeze-thaw)
           --   Shatters steep cliffs. Proportional to slope²
           --   so it's negligible on gentle terrain but very
           --   aggressive on cliffs. epThermal peaks in climates
           --   that cycle around 0°C.
           ---------------------------------------------------------
           thermalSlopeBoost = slopeNorm * slopeNorm
               -- squared: only bites on steep terrain
           thermalRate = thermal
                       * erodability
                       * thermalSlopeBoost

           ---------------------------------------------------------
           -- Chemical erosion (dissolution)
           --   Slow, uniform weathering. Dissolves soft rocks
           --   faster than hard ones. Makes soft rocks even
           --   softer (increases effective erodability beyond
           --   the material's base value). Not slope-dependent.
           --   epChemical is high when CO2 is high (acidic rain).
           ---------------------------------------------------------
           chemicalErodability = min 1.0 (erodability + chemical * 0.3)
               -- chemical weathering softens rock beyond base hardness
               -- e.g. limestone (hardness 0.4, erodability 0.6) at
               -- epChemical 0.5: effective erodability = 0.6 + 0.15 = 0.75
           chemicalRate = chemical
                        * chemicalErodability
                        * 0.5  -- intrinsically slower than hydraulic

           ---------------------------------------------------------
           -- Combined rate
           --   Sum of all modes, then scale by duration and world.
           --   Each mode contributes independently so a wet + cold
           --   world gets both hydraulic valleys AND thermal peaks.
           ---------------------------------------------------------
           linearRate = (hydraulicRate + windRate + thermalRate + chemicalRate)
                      * durationScale
                      * scaleFactor
                      * intensity

           -- Saturating curve: K*(1 - exp(-r/K)).
           --   • Short periods (Ages, r << K): clampedRate ≈ linearRate.
           --   • Long periods (Bombardment / Era, r >> K): clampedRate → K.
           -- This replaces a hard `min 1.0` clamp, which collapsed any
           -- long-duration period to "smooth fully to neighbor average"
           -- in a single pass. With K=0.3 the worst case is "smooth 30%
           -- of the way to neighbor average", so Tier-1 ridges survive
           -- through bombardment + Era periods into the Age loop.
           -- Slope-proportional smoothing cap (drama control). Flat /
           -- uniform terrain keeps the gentle 0.3 cap so plateaus and
           -- broad relief survive, but high-deviation tiles — isolated
           -- spikes and cliff faces, which read high slopeNorm (deviation
           -- from the neighbour AVERAGE) — get a much higher cap. This lets
           -- the reduced number of erosion passes (fewer Ages, see
           -- project_timeline_depth) still knock the extreme peaks/cliffs
           -- down toward their surroundings without over-flattening the
           -- bulk. Mountain MASSES survive (a ridge tile resembles its
           -- ridge neighbours → low slopeNorm); only the jagged extremes go.
           maxSmoothing = 0.3 + 0.5 * slopeNorm ∷ Float
           clampedRate = maxSmoothing * (1.0 - exp (negate (linearRate / maxSmoothing)))

           -- Raw delta: fraction of the difference we close
           rawDelta = diff * clampedRate

           -- Round toward zero to avoid jitter on small differences
           delta = if abs rawDelta < 0.5 then 0
                   else truncateTowardZero rawDelta
           -- Local downhill drop: how far this tile stands ABOVE its
           --   lowest cardinal neighbour, in tiles. This is the DOWNHILL
           --   gradient (the tile being the high side of a step), NOT the
           --   absolute relief — a tile that merely sits at the FOOT of a
           --   tall neighbour (a valley floor / cliff base) has a small
           --   drop and so keeps its soil, which is where eroded material
           --   is supposed to settle. Unlike 'slopeNorm' (deviation from
           --   the neighbour AVERAGE, which a uniformly steep mountainside
           --   reads as ~0), this reads a steep face directly: a 45° face
           --   drops ≥1, a cliff several tiles. It drives ONLY the last-age
           --   soil veneer (below) — the erosion math keeps using the
           --   average-deviation slopeNorm so terrain relief is unchanged.
           --   (#225)
           maxDrop = maximum [ elev - nN, elev - nS
                             , elev - nE, elev - nW ] ∷ Int
           reliefNorm = min 1.0 (fromIntegral (max 0 maxDrop) / 6.0) ∷ Float

           -- Steep faces shed their soil to bare rock (real mountains do
           -- this — soil's angle of repose is ~30-37°; a downhill drop of
           -- ≥1 is already 45°). At/above 'soilShedRelief' tiles of
           -- downhill drop the column exposes rock instead of a soil cap;
           -- gentler ground keeps soil, thinned by steepness. Flat biomes
           -- (drop 0) and valley floors / cliff bases (the LOW side of a
           -- step, drop ≤ 0) are unaffected by 'exposeRock' itself, and
           -- (#812) actually gain the credit the shedding neighbour no
           -- longer caps — see 'shedCredit' below. (#225)
           soilShedRelief = 3 ∷ Int
           exposeRock = isLastAge ∧ maxDrop ≥ soilShedRelief

           -- Local soil redistribution (#812, closing out #225's
           -- redistribution requirement that PR #279 left undone): a
           -- neighbour standing 'soilShedRelief' or more tiles ABOVE this
           -- one is guaranteed to expose rock itself — its own maxDrop
           -- toward THIS tile alone already clears the threshold,
           -- whatever its other neighbours look like — so this tile can
           -- recognise a shedding donor from its OWN 1-ring stencil alone,
           -- with no wider lookahead and no drainage/flow model. Counting
           -- qualifying uphill neighbours (instead of just the downhill
           -- 'maxDrop') is exactly what a plain reliefNorm/maxDrop read
           -- misses: an uphill neighbour never raises maxDrop, so today
           -- a receiver's soil is identical whether or not that neighbour
           -- sheds. Capped at 'soilShedRelief' so a tile boxed in by
           -- shedding faces on every side still gets a bounded veneer,
           -- not an unbounded soil tower.
           shedNeighbors = length
               [ () | n ← [nN, nS, nE, nW], n - elev ≥ soilShedRelief ]
           shedCredit = min soilShedRelief shedNeighbors ∷ Int

           -- Soil depth for last-age: continuous function of relief
           -- instead of discrete thresholds (avoids visible contour lines).
           -- Steep faces (≥ soilShedRelief) get no soil; flat terrain gets
           -- full depth plus any shed credit from a steeper neighbour; in
           -- between it tapers with relief.
           soilDepth
               | not isLastAge = 0
               | exposeRock    = 0
               | otherwise     = max 1 (round
                   (4.0 * erodability * (1.0 - reliefNorm) ∷ Float))
                   + shedCredit

           -- Strata thickness bonus: longer ages deposit thicker layers.
           -- A 15-MY age adds 5 bonus tiles, a 1-MY age adds 0.
           -- This only applies to non-last-age periods (geological rock strata).
           durationBonus
               | isLastAge = 0
               | otherwise = max 0 (truncateTowardZero
                                 (fromIntegral duration / 3.0 ∷ Float))

       in if delta ≡ 0
          then if exposeRock
               -- Steep last-age face: keep the underlying rock exposed,
               -- no soil cap, no elevation change. (#225)
               then noModification
               else if isLastAge ∧ soilDepth > 0
               then GeoModification
                   { gmElevDelta        = 0
                   , gmMaterialOverride = Just (sedimentFn False)
                   , gmIntrusionDepth   = soilDepth
                   }
               else if isLastAge
               then GeoModification
                   { gmElevDelta        = 0
                   , gmMaterialOverride = Just (sedimentFn False)
                   , gmIntrusionDepth   = 0
                   }
               else noModification
          else if delta < 0
               then if exposeRock
                    -- Steep last-age face being eroded: lower it but expose
                    -- the rock beneath instead of back-filling soil. (#225)
                    then GeoModification
                        { gmElevDelta        = delta
                        , gmMaterialOverride = Nothing
                        , gmIntrusionDepth   = 0
                        }
                    else if isLastAge ∧ soilDepth > 0
                    then GeoModification
                        { gmElevDelta        = delta
                        , gmMaterialOverride = Just (sedimentFn False)
                        , gmIntrusionDepth   = soilDepth
                        }
                    else GeoModification
                        { gmElevDelta        = delta
                        , gmMaterialOverride = Just (sedimentFn False)
                        , gmIntrusionDepth   = durationBonus  -- ← erosion strata thickness
                        }
               -- Deposition: tile is lower than neighbors, receive sediment
               else GeoModification
                   { gmElevDelta        = delta
                   , gmMaterialOverride = Just (sedimentFn True)
                   , gmIntrusionDepth   = if isLastAge
                                          then max delta soilDepth
                                          else delta + durationBonus  -- ← deposition strata thickness
                   }

-- | Truncate a float toward zero (not toward negative infinity).
truncateTowardZero ∷ Float → Int
truncateTowardZero x
    | x > 0     = floor x
    | x < 0     = ceiling x
    | otherwise = 0
