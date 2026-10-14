{-# LANGUAGE Strict #-}
-- | The physiology system registry and the per-definition defaults it
--   validates (#2828, S2 of the physiology stat-modes epic #2827): the
--   value types behind @data\/physiology\/systems.yaml@ and the
--   @physiology:@ block of a unit definition.
--
--   __Nothing here is serialized.__ 'PhysioMode' deliberately derives no
--   @Generic Serialize@: a data type under @src\/@ with two or more
--   constructors that derives it joins
--   @tools\/enum_append_only_audit.py@'s guarded set, and the design
--   (D-3) assigns the persisted override enum, with its
--   @docs\/save_compat\/enum_baseline.json@ entry, to S3 (#2829). The
--   registry, the profiles and 'UnitPhysiology' are all /Rebuild/ content:
--   re-derived from the data files on every boot.
--
--   The registry data model is in "Unit.Physiology"'s header; this
--   module is only the shapes, so "Unit.Types.Def" can carry
--   'UnitPhysiology' without importing anything that imports it back.
module Unit.Physiology.Types
    ( -- * Modes
      PhysioMode(..)
    , physioModeText
    , parsePhysioMode
    , ResolvedMode(..)
    , resolvedModeText
      -- * Per-definition defaults
    , UnitPhysiology(..)
    , noUnitPhysiology
      -- * Registry
    , Quantity(..)
    , HealthyRule(..)
    , OwnedKey(..)
    , DerivedRule(..)
    , derivedRuleText
    , parseDerivedRule
    , DerivedKey(..)
    , Substitute(..)
    , InterfaceEntry(..)
    , PhysiologySystem(..)
    , PhysiologyProfile(..)
    , PhysiologyRegistry(..)
    , emptyPhysiologyRegistry
    , physiologyRegistryLoaded
    , knownCapabilities
    , bodyCapability
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

-- * Modes

-- | The mode a unit's system can be CHOSEN to run in: by a species
--   default, or (from S3) by a per-unit override. 'Unsupported' is not a
--   choice but a capability verdict, so it is 'ResolvedMode''s.
--
--   Not @Generic Serialize@ — see the module header.
data PhysioMode = PhysioNominal | PhysioSimulated
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | The Lua spelling of a chosen mode.
physioModeText ∷ PhysioMode → Text
physioModeText PhysioNominal   = "nominal"
physioModeText PhysioSimulated = "simulated"

-- | Exactly the two Lua spellings; anything else is not a mode.
parsePhysioMode ∷ Text → Maybe PhysioMode
parsePhysioMode "nominal"   = Just PhysioNominal
parsePhysioMode "simulated" = Just PhysioSimulated
parsePhysioMode _           = Nothing

-- | What a system RESOLVES to for one unit: the capability verdict
--   first, then the chosen mode. Never stored.
data ResolvedMode = ModeUnsupported | ModeNominal | ModeSimulated
    deriving (Show, Eq)

-- | The Lua spelling of a resolved mode. 'Nothing' is Lua's @nil@, which
--   is how 'ModeUnsupported' reaches a script.
resolvedModeText ∷ ResolvedMode → Maybe Text
resolvedModeText ModeUnsupported = Nothing
resolvedModeText ModeNominal     = Just "nominal"
resolvedModeText ModeSimulated   = Just "simulated"

-- * Per-definition defaults

-- | What a unit DEFINITION says about physiology, resolved at
--   registration (#2828 requirements 2 and 3).
data UnitPhysiology = UnitPhysiology
    { upBodyAuthored ∷ !Bool
      -- ^ whether the YAML authored a @body:@ block. The one input to
      --   the @body@ capability: not @udBodyTemplates@ (registration
      --   always fills it), and never the presence of a stored stat key.
    , upProfile ∷ !(Maybe Text)
      -- ^ the @profile:@ the block named, kept for display; the modes it
      --   implies are already folded into 'upDefaults'.
    , upDefaults ∷ !(HM.HashMap Text PhysioMode)
      -- ^ system id → authored default AFTER profile expansion, with each
      --   per-system entry overriding the profile's. A system absent from
      --   the map has no authored default, which is different from a
      --   default of 'PhysioNominal': the resolver's last-resort Nominal
      --   is not reported as one.
    } deriving (Show, Eq)

-- | A definition that authored no @body:@ and no @physiology:@ block —
--   what a bodyless shipped unit registers as, and what a hand-built
--   test fixture that does not care about physiology uses.
noUnitPhysiology ∷ UnitPhysiology
noUnitPhysiology = UnitPhysiology
    { upBodyAuthored = False, upProfile = Nothing, upDefaults = HM.empty }

-- * Registry

-- | A number written either literally or as the NAME of an entry in the
--   registry's @constants:@ table. Constants are the authoritative home
--   of the values healthy rules need (@SALT_PER_KG@, the thermo baseline,
--   #2761's reserve capacity multiplier).
data Quantity = QLit !Float | QConst !Text
    deriving (Show, Eq)

-- | What a Nominal read of an owned key returns (D-1).
data HealthyRule
    = HealthyConst !Quantity
      -- ^ a fixed value: @{ const: THERMO_BASELINE }@.
    | HealthyFrom !Text ![Quantity]
      -- ^ @{ from: body_mass, scale: [SALT_PER_KG] }@: the unit's
      --   EFFECTIVE value of the named input stat times the product of
      --   the scale factors (none = 1).
    deriving (Show, Eq)

-- | One stored key a system owns, with its healthy rule.
data OwnedKey = OwnedKey
    { okKey     ∷ !Text
    , okHealthy ∷ !HealthyRule
    } deriving (Show, Eq)

-- | The named pure function that computes a derived key.
data DerivedRule = DerivedSaltConcentration
    deriving (Show, Eq)

derivedRuleText ∷ DerivedRule → Text
derivedRuleText DerivedSaltConcentration = "salt_concentration"

parseDerivedRule ∷ Text → Maybe DerivedRule
parseDerivedRule "salt_concentration" = Just DerivedSaltConcentration
parseDerivedRule _                    = Nothing

-- | A key a system computes on demand and never stores.
data DerivedKey = DerivedKey
    { dkKey     ∷ !Text
    , dkRule    ∷ !DerivedRule
    , dkHealthy ∷ !Float
      -- ^ what the key reads while its system is Nominal.
    } deriving (Show, Eq)

-- | What a dependent sees, or what a receiver gets, while the declaring
--   system is Nominal.
data Substitute
    = SubValue !Quantity
      -- ^ a value substituted for a provided one (@1.0@, the baseline).
    | SubNone
      -- ^ nothing: a debit is discarded, an effect does not happen.
    | SubVirtualMeal
      -- ^ nutrition's meal salt credit becomes a virtual meal (SD-4).
      --   Execution is S4's; this slice only records it.
    | SubTissueRecovery
      -- ^ tissue maintenance becomes depletion-only recovery toward the
      --   healthy target (SD-5). Execution is S4's.
    deriving (Show, Eq)

-- | One @provides@ \/ @contributes@ \/ @effects@ entry and its Nominal
--   substitute. The coverage rule (D-11) is that none is without one.
data InterfaceEntry = InterfaceEntry
    { ieName       ∷ !Text
    , ieSubstitute ∷ !Substitute
    } deriving (Show, Eq)

-- | One system's full declaration (D-1).
data PhysiologySystem = PhysiologySystem
    { psId          ∷ !Text
    , psRequires    ∷ ![Text]
      -- ^ species capabilities; all four v1 systems require @body@.
    , psOwns        ∷ ![OwnedKey]
    , psDerived     ∷ ![DerivedKey]
    , psMeters      ∷ ![Text]
    , psProvides    ∷ ![InterfaceEntry]
    , psContributes ∷ ![InterfaceEntry]
    , psEffects     ∷ ![InterfaceEntry]
    } deriving (Show, Eq)

-- | A named species fidelity tier: a mode for EVERY system (D-10).
data PhysiologyProfile = PhysiologyProfile
    { ppId    ∷ !Text
    , ppModes ∷ !(Map.Map Text PhysioMode)
    } deriving (Show, Eq)

-- | Everything @data\/physiology\/@ declared. Systems keep the file's
--   order, which is the registry order the tick will use (thermo before
--   salt, as today).
data PhysiologyRegistry = PhysiologyRegistry
    { prSource    ∷ !(Maybe FilePath)
      -- ^ the file this registry was read from. 'Nothing' = none loaded.
      --   A reload of the SAME path replaces the registry; a different
      --   path is refused rather than merged, because system ids and
      --   keys are one namespace.
    , prConstants ∷ !(Map.Map Text Float)
    , prSystems   ∷ ![PhysiologySystem]
    , prProfiles  ∷ ![PhysiologyProfile]
    } deriving (Show, Eq)

emptyPhysiologyRegistry ∷ PhysiologyRegistry
emptyPhysiologyRegistry = PhysiologyRegistry
    { prSource = Nothing, prConstants = Map.empty
    , prSystems = [], prProfiles = [] }

-- | Is there a usable registry? A definition cannot be validated
--   against an empty one, so registration refuses rather than skipping
--   the check (which would let a templated physiology key pass
--   vacuously).
physiologyRegistryLoaded ∷ PhysiologyRegistry → Bool
physiologyRegistryLoaded = not ∘ null ∘ prSystems

-- | The capabilities a system may @requires@. v1 has one.
knownCapabilities ∷ [Text]
knownCapabilities = [bodyCapability]

bodyCapability ∷ Text
bodyCapability = "body"
