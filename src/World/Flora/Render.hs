{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Flora.Render
    ( resolveFloraTexture
    , findActiveCycleStage  -- re-exported from World.Flora.Growth for
                            -- annual-cycle selection tests
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import World.Flora.Types
import World.Flora.Growth (floraGrowth, findActivePhase,
                           findActiveCycleStage, FloraGrowth(..))

-- * Texture Resolution

-- | Given a flora instance, the species catalog, the calendar's year
--   length, and the absolute world day ('worldAbsoluteDay'), determine
--   which texture to draw.
--
--   Lookup chain:
--
--     1. Derive the instance's growth state ('World.Flora.Growth' —
--        placement age + elapsed days scaled by health, with the
--        generational reseed wrap) and pick the active life phase
--        (highest-age phase where @lpAge <= age@)
--     2. If the species has an annual cycle:
--        find active cycle stage, check overrides, fall back to stage texture
--     3. If no annual cycle, use life phase texture
--     4. If no life phases, use @fsBaseTexture@

resolveFloraTexture ∷ FloraCatalog → Int → Int → FloraInstance → TextureHandle
resolveFloraTexture catalog daysPerYear absDay inst =
    case lookupSpecies (fiSpecies inst) catalog of
        Nothing      → TextureHandle 0
        Just species →
            let dayOfYear = absDay `mod` max 1 daysPerYear
                growth    = floraGrowth species absDay inst
            in resolveSpeciesTexture species dayOfYear (fgAge growth)

resolveSpeciesTexture ∷ FloraSpecies → Int → Float → TextureHandle
resolveSpeciesTexture species dayOfYear age =
    let phases = fsPhases species
        cycle  = fsAnnualCycle species
        overrides = fsCycleOverrides species

        -- Find the active life phase: highest-age phase where lpAge ≤ age
        activePhase = findActivePhase phases age

        -- Base texture from the active phase (or species default)
        phaseTex = case activePhase of
            Just lp → lpTexture lp
            Nothing → fsBaseTexture species

        phaseTag = case activePhase of
            Just lp → Just (lpTag lp)
            Nothing → Nothing

    in case cycle of
        [] → phaseTex  -- No annual cycle, use phase texture
        _  →
            let activeCycleStage = findActiveCycleStage cycle dayOfYear
            in case activeCycleStage of
                Nothing → phaseTex  -- Shouldn't happen if cycle is non-empty
                Just stage →
                    -- Check for a phase-specific override
                    case phaseTag of
                        Just pt →
                            let key = AnnualCycleKey pt (asTag stage)
                            in case HM.lookup key overrides of
                                Just tex → tex       -- Phase+cycle override
                                Nothing  → asTexture stage  -- Default cycle tex
                        Nothing → asTexture stage
