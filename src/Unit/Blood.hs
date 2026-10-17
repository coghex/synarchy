{-# LANGUAGE Strict #-}
-- | The pure blood read: 'bloodView', the one interface every consumer of a
--   unit's blood volume reads through (#2836, S9; design
--   @docs\/designs\/physiology_stat_modes_design.md@ S10.2, A1-5).
--
--   __Unmoded form.__ S10 will add a Nominal branch (volume = maximum,
--   fraction 1.0, the stored 'uiBlood' ignored). Until it lands, 'bloodView'
--   is exactly what the code it replaces computed:
--
--     * @volume@ is the stored 'uiBlood', untouched;
--     * @max@ is the unit's BASE @body_mass@ times 'bloodMassRatio', with the
--       same @70 kg@ fallback every blood reader has always used (a modifier
--       on @body_mass@ never moves it: the maximum deliberately reads the
--       stored base);
--     * @fraction@ is @volume \/ max@ clamped to @0..1@, and @1.0@ when the
--       maximum is not positive (the fallback cardio's blood input always
--       had), so a caller never divides by it;
--     * @bleedRate@ is the live wound bleed in litres per second.
--
--   It is pure of everything but the unit instance, so any thread holding a
--   snapshot may call it. @unit.getBlood@ keeps its own record and its
--   current behaviour until S10 migrates it.
module Unit.Blood
    ( BloodView(..)
    , bloodView
    , bloodViewOf
    , fullBloodView
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Combat.Wounds.Bleed (bleedRateFor)
import Unit.Types (UnitDef, UnitInstance(..), bloodMassRatio)

data BloodView = BloodView
    { bvVolume    ∷ !Float
      -- ^ litres in the body now.
    , bvMax       ∷ !Float
      -- ^ litres at full: base @body_mass@ × 'bloodMassRatio'.
    , bvFraction  ∷ !Float
      -- ^ @volume \/ max@ clamped to @0..1@; @1.0@ when @max@ is not
      --   positive.
    , bvBleedRate ∷ Float
      -- ^ litres per second lost to wounds. LAZY: it sums every wound, and
      --   the circulation rule (the hot reader) never asks for it.
    } deriving (Show, Eq)

-- | The view from its raw parts: the unit's stored stats, its stored blood
--   volume and its bleed rate. For a caller that holds those but no
--   'UnitDef' (the save-load gate, a spawn that has no instance yet).
bloodViewOf ∷ HM.HashMap Text Float → Float → Float → BloodView
bloodViewOf stats volume rate = BloodView
    { bvVolume = volume, bvMax = maxBlood
    , bvFraction = fraction, bvBleedRate = rate }
  where
    maxBlood = HM.lookupDefault 70.0 "body_mass" stats * bloodMassRatio
    fraction
        | maxBlood > 0 = max 0 (min 1 (volume / maxBlood))
        | otherwise    = 1.0

-- | The view of a unit holding its full maximum and bleeding nothing: the
--   shape a fixture that does not care about blood uses.
fullBloodView ∷ HM.HashMap Text Float → BloodView
fullBloodView stats =
    bloodViewOf stats (HM.lookupDefault 70.0 "body_mass" stats * bloodMassRatio) 0

-- | A unit's blood, read through the one interface.
bloodView ∷ UnitDef → UnitInstance → BloodView
bloodView def inst =
    bloodViewOf (uiStats inst) (uiBlood inst) (bleedRateFor def inst)
