-- | The authoritative floating-point world-generation domain (#2288):
--   every bound a world-generation setting must satisfy, stated once,
--   plus the leaf check that applies it.
--
--   Before this module, @world.setGenConfig@ read each floating value
--   with @Lua.tonumber@, narrowed it with @realToFrac@ and installed it
--   unconditionally, and the YAML loader accepted whatever decoded. A
--   non-finite value then flowed straight into generation, where it does
--   not crash and is not reported: @+∞@ volcanic activity saturates
--   @min 1.0 (chance * activity)@ so every eruption fires, NaN makes
--   none fire, a non-finite erosion intensity poisons the elevation
--   grids, and every one of them is persisted in @WorldGenParams@.
--
--   Three boundaries decide validity here and nowhere else:
--   'World.Generate.Config.IO.loadWorldGenConfig' (which defaults an
--   out-of-domain leaf field by field),
--   @Engine.Scripting.Lua.API.World.GenConfig.worldSetGenConfigFn@
--   (which refuses the whole update) and
--   'World.Generate.Config.Validate.repairWorldGenParams' (which
--   defaults a save's out-of-domain stored setting before staging uses
--   it). None of them may hard-code a bound of its own.
--
--   __The domains.__ Where the tracked @config\/world_gen_default.yaml@
--   documents a range, that range IS the domain — this module does not
--   invent a new one, and every shipped default lies inside it:
--
--   * @erosion_intensity@ ∈ ['erosionIntensityMin', 'erosionIntensityMax']
--   * @volcanic_activity@ ∈ ['volcanicActivityMin', 'volcanicActivityMax']
--   * @ore_abundance@ \/ @iron_abundance@ \/ @copper_abundance@ ∈
--     ['abundanceMin', 'abundanceMax']
--   * @thermal_inertia@, @phase_offset@, @day_length@ ∈ [0, 1]
--
--   The remaining multipliers and strengths (@coriolis_scale@,
--   @wind_drag@, @orographic_scale@, @evap_scale@, @albedo_feedback@)
--   have no documented ceiling, so their domain is finite and
--   non-negative — the weakest bound that still excludes the values that
--   corrupt generation. @tilt_angle@ and @thc_threshold@ are meaningful
--   at any finite magnitude and are only required to be finite.
--
--   NaN and either infinity are outside EVERY domain, including the two
--   that are otherwise unbounded.
--
--   __Judged after narrowing.__ Every one of these settings is stored as
--   a 'Float'. A finite source number can become an infinity on the way
--   in (@1e40@ is an ordinary @Double@ and the create-world advanced
--   tab accepts forty digits), so a check applied to the source would
--   pass a value the engine then stores as @+∞@. 'narrowWorldGenFloat'
--   is the one narrowing, and 'checkWorldGenFloat' judges its result.
--
--   This module depends on nothing local but 'UPrelude', so the config
--   types, the loader, the Lua boundary and the load path can all
--   import it without a cycle.
module World.Generate.Config.Domain
  ( -- * Rejections
    WorldGenFieldRejection(..)
  , describeWorldGenRejection
    -- * The domain of one leaf
  , FloatDomain(..)
  , describeFloatDomain
  , checkWorldGenFloat
  , narrowWorldGenFloat
    -- * Bounds
  , erosionIntensityMin, erosionIntensityMax
  , volcanicActivityMin, volcanicActivityMax
  , abundanceMin, abundanceMax
  , unitIntervalMin, unitIntervalMax
    -- * Field names (the full path of each leaf)
  , fieldErosionIntensity, fieldVolcanicActivity
  , fieldTiltAngle, fieldDayLength, fieldPhaseOffset
  , fieldOreAbundance, fieldIronAbundance, fieldCopperAbundance
  , fieldCoriolisScale, fieldWindDrag, fieldThermalInertia
  , fieldOrographicScale, fieldEvapScale, fieldAlbedoFeedback
  , fieldThcThreshold
  ) where

import UPrelude
import GHC.Float (double2Float)

-- | One leaf that missed its domain: which field, the value it carried
--   (rendered for a log line or a Lua diagnostic) and the domain it had
--   to satisfy. Every boundary reports exactly this, so a rejected value
--   is always traceable to its field whichever path refused it.
data WorldGenFieldRejection = WorldGenFieldRejection
    { wgrField  ∷ Text
    , wgrValue  ∷ Text
    , wgrDomain ∷ Text
    } deriving (Show, Eq)

-- | The one-line rendering every boundary embeds.
describeWorldGenRejection ∷ WorldGenFieldRejection → Text
describeWorldGenRejection r =
    wgrField r <> " = " <> wgrValue r
      <> " is outside the domain (" <> wgrDomain r <> ")"

-- * The domain of one leaf

-- | What a floating-point world-generation setting is allowed to be.
--   Every constructor excludes NaN and both infinities; they differ only
--   in what they additionally require of a finite value.
data FloatDomain
  = InRange !Float !Float
    -- ^ Finite and within the closed interval, endpoints included.
  | FiniteNonNegative
    -- ^ Finite and @≥ 0@: a multiplier or strength with no documented
    --   ceiling, for which a negative value has no meaning.
  | AnyFinite
    -- ^ Finite, of either sign: meaningful at any finite magnitude.
  deriving (Eq, Show)

-- | The domain, in the words a rejection quotes.
describeFloatDomain ∷ FloatDomain → Text
describeFloatDomain (InRange lo hi) =
    "a finite number from " <> tshow lo <> " to " <> tshow hi
describeFloatDomain FiniteNonNegative = "a finite number at least 0"
describeFloatDomain AnyFinite         = "a finite number"

-- | Narrow a source 'Double' (a YAML number, a Lua number) to the
--   'Float' every one of these settings is stored as.
--
--   'GHC.Float.double2Float' rather than @realToFrac@: the latter routes
--   through 'Rational' unless a rewrite rule fires, which does not
--   preserve NaN or the infinities — and this narrowing is exactly where
--   a finite source turns into an infinity, so the conversion must be
--   the faithful one.
narrowWorldGenFloat ∷ Double → Float
narrowWorldGenFloat = double2Float

-- | 'Nothing' for a value inside the domain, the rejection otherwise.
--
--   Hand this the STORED 'Float' — the result of 'narrowWorldGenFloat' —
--   never the source 'Double'. A caller that knows a better rendering of
--   the source (the YAML loader reporting the number as the file spelled
--   it, before narrowing) overrides 'wgrValue'.
checkWorldGenFloat ∷ Text → FloatDomain → Float → Maybe WorldGenFieldRejection
checkWorldGenFloat field domain x
    | isNaN x ∨ isInfinite x = reject
    | inside                 = Nothing
    | otherwise              = reject
  where
    inside = case domain of
        InRange lo hi     → x ≥ lo ∧ x ≤ hi
        FiniteNonNegative → x ≥ 0
        AnyFinite         → True
    reject = Just (WorldGenFieldRejection field (tshow x)
                                          (describeFloatDomain domain))

-- * Bounds
--
-- Each pair is the range @config/world_gen_default.yaml@ documents
-- beside the setting. Changing one changes the shipped documentation
-- too; #2288 deliberately changed neither.

-- | @erosion_intensity@: "Global erosion strength multiplier (0.0-2.0)".
erosionIntensityMin, erosionIntensityMax ∷ Float
erosionIntensityMin = 0
erosionIntensityMax = 2

-- | @volcanic_activity@: "Volcano count + eruption-chance multiplier
--   (0.0-3.0)".
volcanicActivityMin, volcanicActivityMax ∷ Float
volcanicActivityMin = 0
volcanicActivityMax = 3

-- | The three @resources@ levers: "(0.0-5.0)" apiece.
abundanceMin, abundanceMax ∷ Float
abundanceMin = 0
abundanceMax = 5

-- | @thermal_inertia@ "(0.0-1.0)", @phase_offset@ "(0.0-1.0)" and
--   @day_length@, a ratio whose documented meaning ("0.5 = equal") is
--   only defined on the unit interval.
unitIntervalMin, unitIntervalMax ∷ Float
unitIntervalMin = 0
unitIntervalMax = 1

-- * Field names
--
-- The full path of each leaf, spelled the way the YAML file spells it,
-- so one diagnostic reads the same whether a file, a Lua table or a
-- save produced the value.

fieldErosionIntensity, fieldVolcanicActivity
  , fieldTiltAngle, fieldDayLength, fieldPhaseOffset
  , fieldOreAbundance, fieldIronAbundance, fieldCopperAbundance
  , fieldCoriolisScale, fieldWindDrag, fieldThermalInertia
  , fieldOrographicScale, fieldEvapScale, fieldAlbedoFeedback
  , fieldThcThreshold ∷ Text
fieldErosionIntensity = "world_gen.erosion_intensity"
fieldVolcanicActivity = "world_gen.volcanic_activity"
fieldTiltAngle        = "world_gen.sun.tilt_angle"
fieldDayLength        = "world_gen.sun.day_length"
fieldPhaseOffset      = "world_gen.moon.phase_offset"
fieldOreAbundance     = "world_gen.resources.ore_abundance"
fieldIronAbundance    = "world_gen.resources.iron_abundance"
fieldCopperAbundance  = "world_gen.resources.copper_abundance"
fieldCoriolisScale    = "world_gen.climate.coriolis_scale"
fieldWindDrag         = "world_gen.climate.wind_drag"
fieldThermalInertia   = "world_gen.climate.thermal_inertia"
fieldOrographicScale  = "world_gen.climate.orographic_scale"
fieldEvapScale        = "world_gen.climate.evap_scale"
fieldAlbedoFeedback   = "world_gen.climate.albedo_feedback"
fieldThcThreshold     = "world_gen.climate.thc_threshold"
