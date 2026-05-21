{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module Substance.Types
    ( SubstanceDef(..)
    , SubstanceManager(..)
    , emptySubstanceManager
    , lookupSubstance
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM

-- | Physical properties of a worked material — distinct from
--   World.Material (which is the tile-rendering material system).
--   This is the data combat will consume to resolve damage,
--   penetration, and fracture against armor.
--
--   Optional fields (shear / yield / hardness) default to 0 when the
--   YAML omits them — combat formulas should treat 0 as "not modelled"
--   and fall back to derived approximations (e.g. shear ≈ 0.6×tensile
--   when shear is absent).
--
--   Field prefix is `sbs` to avoid colliding with `sd*` (SaveData) and
--   `s*` (slot, sim state).
data SubstanceDef = SubstanceDef
    { sbsName              ∷ !Text   -- ^ unique key, e.g. "steel"
    , sbsDensity           ∷ !Float  -- ^ g/cm³
    , sbsTensileStrength   ∷ !Float  -- ^ ultimate, MPa
    , sbsYieldStrength     ∷ !Float  -- ^ elastic limit, MPa (0 = unspec)
    , sbsShearStrength     ∷ !Float  -- ^ MPa (0 = unspec)
    , sbsFractureToughness ∷ !Float  -- ^ MPa·√m
    , sbsHardness          ∷ !Float  -- ^ Brinell-ish, 0 = unspec
    -- Per-attack-type damage resistance — combat-side coefficients in
    -- [0, 1]. The system reads these AND the attacker's per-attack-
    -- type weapon effectiveness; damage_potential ≈ effectiveness ×
    -- (1 − resistance), so a stab attack with leather armor uses
    -- leather's stab resistance.
    , sbsStabResistance    ∷ !Float
    , sbsSlashResistance   ∷ !Float
    , sbsBluntResistance   ∷ !Float
    } deriving (Show, Eq)

-- | Engine-wide registry of substances loaded from data/substances/.
newtype SubstanceManager = SubstanceManager
    { sbmDefs ∷ HM.HashMap Text SubstanceDef
    } deriving (Show, Eq)

emptySubstanceManager ∷ SubstanceManager
emptySubstanceManager = SubstanceManager HM.empty

lookupSubstance ∷ Text → SubstanceManager → Maybe SubstanceDef
lookupSubstance n (SubstanceManager m) = HM.lookup n m
