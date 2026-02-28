{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Flora.Render
    ( resolveFloraTexture
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import World.Flora.Types

-----------------------------------------------------------
-- Texture Resolution
--
-- Given a flora instance, the species catalog, and the
-- current world time, determine which texture to draw.
--
-- Lookup chain:
--   1. Determine active life phase (highest-age phase where
--      lpAge ≤ fiAge)
--   2. If the species has an annual cycle:
--      a. Get day-of-year from world time
--      b. Find active cycle stage (highest asStartDay ≤ day)
--      c. Check fsCycleOverrides for (phase, cycleStage)
--      d. If found → use override texture
--      e. Otherwise → use cycle stage's asTexture
--   3. If no annual cycle → use life phase's lpTexture
--   4. If no life phases → use fsBaseTexture
-----------------------------------------------------------

resolveFloraTexture ∷ FloraCatalog → Int → FloraInstance → TextureHandle
resolveFloraTexture catalog dayOfYear inst =
    case lookupSpecies (fiSpecies inst) catalog of
        Nothing      → TextureHandle 0
        Just species → resolveSpeciesTexture species dayOfYear (fiAge inst)

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

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- | Find the highest-age phase where lpAge ≤ current age.
findActivePhase ∷ HM.HashMap LifePhaseTag LifePhase → Float → Maybe LifePhase
findActivePhase phases age =
    let eligible = filter (\lp → lpAge lp ≤ age) (HM.elems phases)
    in case eligible of
        [] → Nothing
        _  → Just $ maximumByKey (lifePhaseOrder . lpTag) eligible

-- | Find the active cycle stage: highest asStartDay ≤ dayOfYear.
--   Wraps around: if dayOfYear < all start days, use the last stage
--   (it wraps from the previous year).
findActiveCycleStage ∷ [AnnualStage] → Int → Maybe AnnualStage
findActiveCycleStage [] _ = Nothing
findActiveCycleStage stages dayOfYear =
    let sorted = sortOn asStartDay stages
        eligible = filter (\s → asStartDay s ≤ dayOfYear) sorted
    in case eligible of
        [] → Just (last sorted)  -- Wrap: use the last stage from previous year
        _  → Just (last eligible)

-- | Like maximumBy but takes a key function.
maximumByKey ∷ Ord b ⇒ (a → b) → [a] → a
maximumByKey _ [x]    = x
maximumByKey f (x:xs) = foldl' (\best y → if f y > f best then y else best) x xs
maximumByKey _ []     = error "maximumByKey: empty list"
