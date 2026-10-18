{-# LANGUAGE Strict #-}
-- | The unit half of the scenario definition catalog (#2710, SCN-10 of
--   epic #2698): which of a unit definition's values a scenario may
--   author, which are derived, and how each is owned.
--
--   "Scenario.Decode" validates a unit entry against a
--   'Scenario.Types.UnitCatalogEntry'; this module builds that entry from
--   the LIVE definition, the physiology registry and the equipment
--   classes, so the decoder, the construction adapter
--   ("Scenario.Units.Build") and capture ("Scenario.Units.Capture") read
--   one field table. The table is documented in
--   @docs/scenario_format.md@ ("Units: field ownership").
--
--   __A body-bearing definition__ is an eager one: spawn seeds its body
--   composition from @height@ / @bulk@ / @bodyfat@ and derives
--   @strength@, @strength_body@, the resource maxima and
--   @carrying_capacity@ from it ("Unit.Thread.Command.Body"). Its
--   independently authorable body values are the seed inputs (@height@,
--   @bulk@, @bodyfat@), the current masses (@frame_mass@, @body_mass@,
--   @lean_mass@, @fat_mass@) and @strength_base@; @strength@ itself is
--   derived (the trusted review's correction). A LAZY definition has no
--   body: its stat templates are all plain authorable stats, @strength@
--   included.
--
--   __Physiology.__ The owned stat keys of every system that is
--   Simulated by the definition's own defaults are authorable current
--   values (hydration, hunger, calories, stamina, exhaustion,
--   sleep_pressure, core_temp, salt, salt_store, heart_rate,
--   blood_oxygen), except the @mind@ system's, whose keys are mental
--   episode state and timers, not conditions a scenario states; omitted,
--   they take the system's own seed. Blood is the separate @blood@ field.
--   A system's derived keys are derived. Meters, an engagement marker and
--   the keys of a Nominal or Unsupported system are not scenario values.
module Scenario.Units.Catalog
    ( unitCatalogEntryOf
    , unitCatalogOf
    , unitStatRules
    , isBodyBearing
    , bodyInputStats
    , bodyMassStats
    , bodyDerivedStats
    , supportedPhysioKeys
    , simulatedSystems
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Equipment.Types (EquipmentClass(..), EquipmentClassManager, EquipmentSlot(..),
                        lookupEquipmentClass)
import Scenario.Types
import Unit.Physiology (resolveMode)
import Unit.Physiology.Types
import Unit.Types (BodyPart(..), UnitDef(..))

-- | Whether spawn seeds a body for this definition (see the module
--   header).
isBodyBearing ∷ UnitDef → Bool
isBodyBearing d =
    udEagerStats d
    ∧ HM.member "height" (udStatTemplates d)
    ∧ HM.member "bulk" (udBodyTemplates d)
    ∧ HM.member "bodyfat" (udBodyTemplates d)

-- | The seed inputs a fresh body is built from. @height@ stays a live
--   stat; @bulk@ and @bodyfat@ are consumed by the seed.
bodyInputStats ∷ [Text]
bodyInputStats = ["height", "bulk", "bodyfat"]

-- | The current body composition. Authored together or not at all.
bodyMassStats ∷ [Text]
bodyMassStats = ["frame_mass", "body_mass", "lean_mass", "fat_mass"]

-- | What 'Unit.Thread.Command.Body.recomputeBodyDerivedStats' writes,
--   except @strength_base@ (the un-scaled potential, authorable).
bodyDerivedStats ∷ [Text]
bodyDerivedStats =
    [ "strength", "strength_body", "max_hydration", "max_hunger"
    , "max_calories", "carrying_capacity" ]

-- | The systems that are Simulated for this definition with no override
--   and no applied engagement: the modes a scenario unit is built in.
simulatedSystems ∷ PhysiologyRegistry → UnitDef → [PhysiologySystem]
simulatedSystems reg d =
    [ s | s ← prSystems reg
        , resolveMode s (udPhysiology d) False Nothing ≡ ModeSimulated ]

-- | The physiology stat keys a scenario may author for this definition
--   (see the module header).
supportedPhysioKeys ∷ PhysiologyRegistry → UnitDef → [Text]
supportedPhysioKeys reg d =
    [ okKey o | s ← simulatedSystems reg d, psId s ≢ "mind"
              , o ← psOwns s, isNothing (okField o) ]

-- | The derived physiology keys of the definition's Simulated systems.
derivedPhysioKeys ∷ PhysiologyRegistry → UnitDef → [Text]
derivedPhysioKeys reg d = [ dkKey k | s ← simulatedSystems reg d, k ← psDerived s ]

-- | The catalog row of one definition.
unitCatalogEntryOf ∷ PhysiologyRegistry → EquipmentClassManager → UnitDef
                   → UnitCatalogEntry
unitCatalogEntryOf reg ecMgr d = UnitCatalogEntry
    { ucStats = unitStatRules reg d
    , ucSkills = HS.fromList (HM.keys (udSkillTemplates d))
    , ucBodyParts = HS.fromList (map bpId (udBodyParts d))
    , ucEquipmentSlots = HS.fromList
        [ esId s | Just cls ← [udEquipmentClass d ⌦ (`lookupEquipmentClass` ecMgr)]
                 , s ← ecSlots cls ]
    }

-- | Every stat name a scenario may state for this definition, and every
--   derived one it may not.
unitStatRules ∷ PhysiologyRegistry → UnitDef → HM.HashMap Text StatRule
unitStatRules reg d = HM.fromList (authorable ⧺ derived)
  where
    body = isBodyBearing d
    templates = HM.keys (udStatTemplates d)
    authorable = [ (k, AuthorableStat) | k ← authorableNames ]
    authorableNames
        | body = [ k | k ← templates, k `notElem` bodyDerivedStats ]
                 ⧺ ["bulk", "bodyfat", "strength_base"] ⧺ bodyMassStats
                 ⧺ supportedPhysioKeys reg d
        | otherwise = templates ⧺ supportedPhysioKeys reg d
    derived = [ (k, DerivedStat)
              | k ← (if body then bodyDerivedStats else []) ⧺ derivedPhysioKeys reg d
              , k `notElem` authorableNames ]

-- | The whole unit catalog for 'Scenario.Types.catUnits'.
unitCatalogOf ∷ PhysiologyRegistry → EquipmentClassManager → HM.HashMap Text UnitDef
              → HM.HashMap Text UnitCatalogEntry
unitCatalogOf reg ecMgr = HM.map (unitCatalogEntryOf reg ecMgr)
