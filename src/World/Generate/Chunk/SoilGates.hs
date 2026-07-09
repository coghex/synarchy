{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Post-classification soil reality checks (wetland/salt-flat
--   demotion) and the fluid-aware water table used to gate them.
--   Split out of 'World.Generate.Chunk' (#549) — a pure move, no
--   behavior change.
module World.Generate.Chunk.SoilGates
    ( demoteWetland
    , surfaceDemotion
    , wetlandKeep
    , saltFlatKeep
    , nearFlat
    , applyFluidWt
    , wtHaloRadius
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import World.Types

-- | Wetland-soil reality check (demotion only). The climate classifier
--   ('soilFromClimate') paints muck/peat wherever the climate is
--   hot/wet, with no physical context — its slope input is curvature
--   (uniform hillsides read as flat) and the water table doesn't exist
--   yet at classification time. Gate after generation instead, where
--   the final terrain and water table are ground truth: a wetland soil
--   survives only where 'wetlandKeep' holds; otherwise it demotes to
--   the classifier's own next rung. Same post-classification pattern
--   as the Coastal beach demotion (peat/muck → sand). Used by both the
--   detail path ('generateChunk') and the zoom path
--   ('generateZoomTerrain') so the two views agree.
demoteWetland ∷ Word8 → Maybe Word8
demoteWetland 62 = Just 57   -- peat       → clay loam
demoteWetland 63 = Just 58   -- mucky peat → silty clay
demoteWetland 64 = Just 50   -- muck       → clay
demoteWetland _  = Nothing

-- | All post-classification soil reality checks in one place: given a
--   surface material and the final terrain context, return the demoted
--   material if the climate-only classifier's pick can't physically
--   survive here, or 'Nothing' if it stays. Currently gates wetland
--   soils (need flat + wet ground) and salt flat (needs a flat basin
--   floor). Shared by the detail ('generateChunk') and zoom
--   ('generateZoomTerrain') paths so the two views agree.
surfaceDemotion ∷ (Int → Int → Maybe Int) → VU.Vector Int → VU.Vector Int
                → Int → Word8 → Maybe Word8
surfaceDemotion outElev terrain wt idx m
    | Just demoted ← demoteWetland m
    , not (wetlandKeep outElev terrain wt idx) = Just demoted
    | m ≡ 67                                            -- salt flat
    , not (saltFlatKeep outElev terrain idx)   = Just 66 -- → light gravel
    | otherwise                                = Nothing

-- | Keep a wetland soil only on a near-flat tile (max 4-neighbour
--   |Δterrain| ≤ 2) whose groundwater reaches the surface
--   (wt ≥ terrain−1; the climate baseline tops out at terrain−2, so
--   only tiles inside 'applyFluidWt's under-fluid bump or fresh-water
--   halo can pass — i.e. this keeps riparian and lakeside ground).
--
--   @outElev@ supplies elevations for neighbours OUTSIDE the chunk
--   interior (chunk-local coords; both call paths pass their bordered
--   post-carve vector). The flat test used to be in-chunk-lenient,
--   which was invisible while no dry wetland could pass the wet test;
--   once the halo restored shore wetlands, border tiles whose
--   cross-border neighbour sat 3+ lower slipped through as
--   wetland-on-cliff (audit WETLAND_ON_SLOPE, 29 hits seed 42 w64 —
--   all at lx/ly ∈ {0,15}).
wetlandKeep ∷ (Int → Int → Maybe Int) → VU.Vector Int → VU.Vector Int
            → Int → Bool
wetlandKeep outElev terrain wt idx =
    let tz  = terrain VU.! idx
        wet = wt VU.! idx ≥ tz - 1
    -- Sub-sea tiles are seabed, not land: the ocean-floor pass
    -- ('World.Fluid.Seabed') places muck on the deep floor by design,
    -- so the wetland demotion (a dry-hillside concern) must leave it
    -- alone — otherwise deep seabed muck (64) gets demoted to clay
    -- (50) on every steep stretch of sea floor.
    in tz ≤ seaLevel ∨ (nearFlat outElev terrain idx ∧ wet)

-- | Keep salt flat (matId 67) only on a near-flat basin floor. The
--   climate classifier paints salt flat on any cold + hyper-arid tile
--   regardless of slope, but an evaporite pan is a flat basin feature —
--   on a slope it reads as a stripe of pan up a hillside. Gate it the
--   same way as the wetland soils and demote a sloped salt flat to the
--   classifier's own next rung (light gravel, 66). No wet test: salt
--   pans are dry by definition. Sub-sea tiles are left to the seabed
--   pass, as in 'wetlandKeep'.
saltFlatKeep ∷ (Int → Int → Maybe Int) → VU.Vector Int → Int → Bool
saltFlatKeep outElev terrain idx =
    let tz = terrain VU.! idx
    in tz ≤ seaLevel ∨ nearFlat outElev terrain idx

-- | Near-flat test shared by the soil gates: max 4-neighbour
--   |Δterrain| ≤ 2. @outElev@ supplies elevations for neighbours
--   outside the chunk interior (chunk-local coords); both call paths
--   pass their bordered post-carve vector so the gate sees the same
--   neighbours the renderer does. The 'minBound' beyond-glacier
--   sentinel reads as flat (Δ 0).
nearFlat ∷ (Int → Int → Maybe Int) → VU.Vector Int → Int → Bool
nearFlat outElev terrain idx =
    let tz = terrain VU.! idx
        lx = idx `mod` chunkSize
        ly = idx `div` chunkSize
        nbrD dlx dly =
            let lx' = lx + dlx
                ly' = ly + dly
            in if lx' < 0 ∨ lx' ≥ chunkSize ∨ ly' < 0 ∨ ly' ≥ chunkSize
               then case outElev lx' ly' of
                      Just z | z ≠ minBound → abs (tz - z)
                      _                     → 0
               else abs (tz - terrain VU.! (ly' * chunkSize + lx'))
    in max (max (nbrD 1 0) (nbrD (-1) 0))
           (max (nbrD 0 1) (nbrD 0 (-1))) ≤ 2

-- | Fluid-aware water table: lift the climate baseline where surface
--   water dictates the local groundwater level.
--
--   1. UNDER-FLUID BUMP — tiles under any water body (lake, river,
--      ocean) get @wt ≥ fluid surface@, so a dig through the bed
--      exposes water. Lava pools get no bump.
--   2. FRESH-WATER HALO — dry tiles within 'wtHaloRadius' (Chebyshev)
--      of a lake or river tile get @wt = surface + 1 − distance@:
--      riparian groundwater that decays away from the shore. This is
--      what lets 'wetlandKeep' pass on dry land at all — the climate
--      baseline is at best @terrain − 2@ against a @terrain − 1@ wet
--      test, so without the halo wetland soils survive only
--      underwater. Fresh water only: ocean shores keep their beaches
--      (the Coastal pass demotes wetland soils to sand there) and
--      stay out of the wetland gate.
--
--   In-chunk only — fluid just across a chunk border doesn't halo in,
--   the same leniency convention 'wetlandKeep' uses for its flat
--   test. 'minBound' sentinel (beyond-glacier) passes through.
--   Shared by 'generateChunk' and 'generateZoomTerrain' so the two
--   views demote identically.
applyFluidWt ∷ V.Vector (Maybe FluidCell) → VU.Vector Int → VU.Vector Int
applyFluidWt fluid wtBase = VU.generate (chunkSize * chunkSize) $ \idx →
    let wt0 = wtBase VU.! idx
    in if wt0 ≡ minBound
       then wt0
       else case fluid V.! idx of
            Just fc | fcType fc ≠ Lava → max wt0 (fcSurface fc)
                    | otherwise        → wt0
            Nothing → case haloSurfs idx of
                        [] → wt0
                        ss → max wt0 (maximum ss)
  where
    haloSurfs idx =
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
        in [ fcSurface fc + 1 - d
           | dy ← [-wtHaloRadius .. wtHaloRadius]
           , dx ← [-wtHaloRadius .. wtHaloRadius]
           , let d = max (abs dx) (abs dy)
           , d > 0
           , let lx' = lx + dx
                 ly' = ly + dy
           , lx' ≥ 0, lx' < chunkSize, ly' ≥ 0, ly' < chunkSize
           , Just fc ← [fluid V.! (ly' * chunkSize + lx')]
           , fcType fc ≡ Lake ∨ fcType fc ≡ River
           ]

-- | How far the fresh-water groundwater halo reaches from a lake or
--   river tile (Chebyshev tiles). With the @surface + 1 − d@ decay, a
--   flat bank one z above the water passes the wet test at distance
--   1, and at-water-level flats pass out to distance 2.
wtHaloRadius ∷ Int
wtHaloRadius = 3
