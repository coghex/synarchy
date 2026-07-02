{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Item temperature: Newtonian cooling toward the tile's ambient (#344).
--
--   An item's effective temperature is its tile's ambient air temperature
--   ('World.Weather.Ambient.ambientTempAt') unless it carries a tracked
--   'iiTemp' — set when something creates it hot (cooked food, a
--   freshly-smelted bar) or via the Lua temp setters. The per-page world
--   tick ('World.Thread.ItemTemp') relaxes tracked temperatures toward
--   ambient with an exponential approach whose time constant scales with
--   the item's total mass (a cup of coffee equilibrates much faster than
--   an anvil), and untracks them once they arrive — so the steady state
--   is "no item tracked" and the tick is nearly free.
--
--   Everything here is pure; callers supply the ambient (they know the
--   item's location) and the game-time step.
module Item.Temperature
    ( itemTempTauPerKg
    , itemTempSnapEpsilon
    , effectiveItemTemp
    , hasTrackedTemp
    , coolItem
    ) where

import UPrelude
import Item.Types (ItemInstance(..), ItemManager, itemTotalWeight)

-- | Cooling time constant per kilogram of item mass, in GAME-seconds:
--   tau = itemTempTauPerKg × mass(kg). After tau game-seconds a tracked
--   item has closed ~63% of its gap to ambient (half-life ≈ 0.69·tau).
--   A 0.5 kg cup of coffee: half-life ~21 game-minutes; a 5 kg metal
--   bar: ~3.5 game-hours. Tuning constant — thermal mass beyond simple
--   mass scaling (per-def material overrides) can layer on later.
itemTempTauPerKg ∷ Float
itemTempTauPerKg = 3600

-- | Once a tracked temperature is within this many °C of ambient it
--   snaps to Nothing (= "at ambient") and stops costing tick work.
itemTempSnapEpsilon ∷ Float
itemTempSnapEpsilon = 0.25

-- | The temperature an item reads at: its tracked value, else the
--   ambient the caller sampled at the item's location.
effectiveItemTemp ∷ Float → ItemInstance → Float
effectiveItemTemp ambient = fromMaybe ambient . iiTemp

-- | Does this item (or anything nested in it) carry a tracked
--   temperature? The tick uses this to skip untracked containers —
--   and whole item populations — without rebuilding them.
hasTrackedTemp ∷ ItemInstance → Bool
hasTrackedTemp inst =
    isJust (iiTemp inst) ∨ any hasTrackedTemp (iiContents inst)

-- | Relax one item's tracked temperature toward @ambient@ over @dtGame@
--   game-seconds, recursing into container contents (a hot bottle inside
--   a kit cools too; the kit's own shell needs no tracking for that).
--   The exponential form is unconditionally stable, so a large dtGame
--   (high time-scale) can never overshoot ambient. Returns the input
--   unchanged (no allocation) when nothing inside is tracked.
coolItem ∷ ItemManager → Float → Float → ItemInstance → ItemInstance
coolItem im ambient dtGame inst
    | not (hasTrackedTemp inst) = inst
    | otherwise = inst { iiTemp = temp', iiContents = contents' }
  where
    contents' = map (coolItem im ambient dtGame) (iiContents inst)
    temp' = case iiTemp inst of
        Nothing → Nothing
        Just t →
            -- Thermal mass = the item's full carried mass (case + fill +
            -- contents), floored so a near-weightless item can't divide
            -- the time constant to nothing.
            let mass = max 0.1 (itemTotalWeight im inst)
                tau  = itemTempTauPerKg * mass
                t'   = ambient + (t - ambient) * exp (negate dtGame / tau)
            in if abs (t' - ambient) < itemTempSnapEpsilon
               then Nothing
               else Just t'
