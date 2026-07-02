{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Derived flora growth state (#332).
--
--   Flora placement is deterministic (chunks evict and regenerate to
--   byte-identical instances) and the tile's climate/soil are static, so
--   a plant's growth state doesn't need per-instance mutable state at
--   all: it DERIVES from the placement baseline and the world clock.
--
--     age(day) = fiAge + day · growthRate(fiHealth)
--
--   where @day@ is 'worldAbsoluteDay' (whole days since the world
--   epoch; the date is already persisted, so growth survives chunk
--   eviction AND save/load for free), @fiAge@ is the deterministic
--   placement age ("age at day 0"), and @fiHealth@ is the placement-time
--   habitat fitness ('speciesFitness' — soil + climate suitability).
--   Unhealthy plants grow slower; nothing here ever writes state.
--
--   Mortal lifecycles (perennial/annual/biennial) wrap generationally:
--   each instance gets a deterministic lifespan hashed from its placement
--   fields; when its running age exceeds that, the plant spends
--   'deadWindowDays' dead (the authored dead art) and then wraps to age
--   0 as the next generation — the reseed: the old plant's offspring
--   takes the spot. Evergreens never wrap.
--
--   The one piece of true mutable flora state stays where #94 put it:
--   the world-level harvest/regrowth map (World.Flora.Harvest). This
--   module adds the harvest WINDOW on top: species that author a
--   @fruiting@ annual stage (a raspberry) only yield during it — fruit
--   left unharvested is simply gone when the cycle rolls to senescing —
--   while species without one (a clover picked for leaves) stay open
--   year-round.
module World.Flora.Growth
    ( -- * Derived growth state
      FloraGrowth(..)
    , floraGrowth
    , growthRate
    , instanceLifespan
    , deadWindowDays
      -- * Phase / stage selection
    , findActivePhase
    , findActiveCycleStage
    , growthPhaseTag
    , activeStageTag
      -- * Harvest window
    , harvestOpen
      -- * Names (Lua-facing, match the YAML tag vocabulary)
    , lifePhaseText
    , annualStageText
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import World.Flora.Types

-- | A flora instance's growth state at some absolute world day, fully
--   derived from the instance's placement fields.
data FloraGrowth = FloraGrowth
    { fgAge        ∷ !Float
      -- ^ Effective age in game-days, post generational wrap — feed this
      --   to phase selection. During the dead window it is pinned to the
      --   species' dead-phase threshold so the authored dead art shows.
    , fgGeneration ∷ !Int
      -- ^ How many times this spot has reseeded (0 = the original
      --   worldgen plant). Observable so tests can pin the wrap.
    , fgDead       ∷ !Bool
      -- ^ Inside the dead window between generations.
    } deriving (Show, Eq)

-- | Health → growth speed. A plant at full habitat fitness ages one
--   game-day per game-day; one barely inside its ranges crawls at a
--   quarter speed. Never zero: placement only puts plants where
--   fitness > 0, and a stalled clock would freeze sprouts forever.
growthRate ∷ Float → Float
growthRate health = 0.25 + 0.75 * max 0.0 (min 1.0 health)

-- | Game-days a wrapped (dead) plant shows its dead art before the next
--   generation sprouts.
deadWindowDays ∷ Float
deadWindowDays = 60.0

-- | Deterministic per-instance lifespan in game-days. Nothing =
--   immortal (evergreens hold their most mature phase forever).
--   Perennials roll theirs between the species' min/max from a hash of
--   the instance's placement fields — stable across chunk regeneration
--   because placement is. Annual/biennial match the 360/720-day
--   convention 'speciesMaxAge' already uses for initial ages.
instanceLifespan ∷ FloraSpecies → FloraInstance → Maybe Float
instanceLifespan sp fi = case fsLifecycle sp of
    Evergreen              → Nothing
    Annual                 → Just 360.0
    Biennial               → Just 720.0
    Perennial minL maxL _  →
        Just (minL + instanceHashFrac fi * max 0.0 (maxL - minL))

-- | A [0,1] hash off the placement fields that vary per instance
--   (sub-tile offsets, variant, column). Only cosmetic variation — it
--   doesn't need to match the worldgen hash, just be stable.
instanceHashFrac ∷ FloraInstance → Float
instanceHashFrac fi =
    let qu = round ((fiOffU fi + 0.5) * 1023.0) ∷ Word64
        qv = round ((fiOffV fi + 0.5) * 1023.0) ∷ Word64
        h0 = qu * 2654435761
           `xor` (qv * 2246822519)
           `xor` (fromIntegral (fiTileX fi) * 73856093)
           `xor` (fromIntegral (fiTileY fi) * 19349663)
           `xor` (fromIntegral (fiVariant fi) * 83492791)
        h1 = h0 `xor` (h0 `shiftR` 16)
        h2 = h1 * 1640531527
        h3 = h2 `xor` (h2 `shiftR` 13)
    in fromIntegral (h3 .&. 0xFFFF) / 65535.0

-- | Derive an instance's growth state at an absolute world day.
floraGrowth ∷ FloraSpecies → Int → FloraInstance → FloraGrowth
floraGrowth sp absDay fi =
    let total = fiAge fi
              + fromIntegral (max 0 absDay) * growthRate (fiHealth fi)
    in case instanceLifespan sp fi of
        Nothing → FloraGrowth total 0 False
        Just lifespan →
            let cycleLen = lifespan + deadWindowDays
                gen = floor (total / cycleLen) ∷ Int
                r   = total - fromIntegral gen * cycleLen
            in if r < lifespan
               then FloraGrowth r gen False
               else FloraGrowth (deadPhaseAge sp lifespan) gen True

-- | The age to present while dead: the species' authored dead-phase
--   threshold when it has one (so 'findActivePhase' lands on the dead
--   art), otherwise the lifespan itself (holds the most mature art —
--   species without dead art just look full-grown until the wrap).
deadPhaseAge ∷ FloraSpecies → Float → Float
deadPhaseAge sp lifespan = case HM.lookup PhaseDead (fsPhases sp) of
    Just lp → max lifespan (lpAge lp)
    Nothing → lifespan

-- * Phase / stage selection

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

-- | The life-phase tag a grown instance presents (Nothing when the
--   species defines no phases).
growthPhaseTag ∷ FloraSpecies → FloraGrowth → Maybe LifePhaseTag
growthPhaseTag sp g = lpTag <$> findActivePhase (fsPhases sp) (fgAge g)

-- | The annual-cycle stage active on a given day-of-year (Nothing when
--   the species has no annual cycle).
activeStageTag ∷ FloraSpecies → Int → Maybe AnnualStageTag
activeStageTag sp dayOfYear =
    asTag <$> findActiveCycleStage (fsAnnualCycle sp) dayOfYear

-- * Harvest window

-- | Whether a plant in this growth state yields to a harvest right now
--   (the #94 regrowth timer is checked separately by the callers — this
--   is the GROWTH gate on top of it):
--
--     * dead plants and juveniles (sprout/seedling) or withering ones
--       never yield;
--     * a species whose annual cycle authors a @fruiting@ stage only
--       yields while that stage is active — the seasonal window;
--     * species without a fruiting stage (leaves/roots forage, or no
--       annual cycle at all) are open year-round.
harvestOpen ∷ FloraSpecies → Int → FloraGrowth → Bool
harvestOpen sp dayOfYear g
    | fgDead g       = False
    | phaseBlocked   = False
    | hasFruiting    = activeStageTag sp dayOfYear ≡ Just CycleFruiting
    | otherwise      = True
  where
    phaseBlocked = case growthPhaseTag sp g of
        Just t  → t `elem` [PhaseSprout, PhaseSeedling, PhaseWithering, PhaseDead]
        Nothing → False
    hasFruiting = any ((≡ CycleFruiting) . asTag) (fsAnnualCycle sp)

-- * Names

-- | Lua-facing life-phase names; the same vocabulary the flora YAML
--   authors ('parsePhaseTag' in Engine.Asset.YamlFlora).
lifePhaseText ∷ LifePhaseTag → Text
lifePhaseText PhaseSprout     = "sprout"
lifePhaseText PhaseSeedling   = "seedling"
lifePhaseText PhaseVegetating = "vegetating"
lifePhaseText PhaseBudding    = "budding"
lifePhaseText PhaseFlowering  = "flowering"
lifePhaseText PhaseRipening   = "ripening"
lifePhaseText PhaseMatured    = "matured"
lifePhaseText PhaseWithering  = "withering"
lifePhaseText PhaseDead       = "dead"

-- | Lua-facing annual-stage names ('parseCycleTag' vocabulary).
annualStageText ∷ AnnualStageTag → Text
annualStageText CycleDormant   = "dormant"
annualStageText CycleBudding   = "budding"
annualStageText CycleFlowering = "flowering"
annualStageText CycleFruiting  = "fruiting"
annualStageText CycleSenescing = "senescing"

-- | Like maximumBy but takes a key function.
--   Callers must ensure the list is non-empty (findActivePhase guards
--   with a [] → Nothing pattern match before calling this).
maximumByKey ∷ Ord b ⇒ (a → b) → [a] → a
maximumByKey _ [x]    = x
maximumByKey f (x:xs) = foldl' (\best y → if f y > f best then y else best) x xs
-- Unreachable: the only caller (findActivePhase) matches [] → Nothing
-- before reaching this, so the list is non-empty here by construction.
maximumByKey _ []     = error "maximumByKey: empty list (caller failed to guard non-empty)"
