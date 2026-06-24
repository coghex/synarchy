{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Data-driven infection catalogue. An infection DEF describes one
--   infection a wound can develop (staph, gas gangrene, …); the live
--   per-wound state is just `Wound.woundInfection` (level) +
--   `woundInfectionType` (which def, by id). Loaded from
--   data/infections/*.yaml via Engine.Asset.YamlInfection into the
--   engine-wide InfectionManager (mirrors Substance.Types).
--
--   Field prefix `inf` to avoid colliding with other record namespaces.
module Infection.Types
    ( InfectionDef(..)
    , InfectionManager(..)
    , emptyInfectionManager
    , lookupInfection
    , infectionsForSite
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM

-- | One infection type from the YAML catalogue.
data InfectionDef = InfectionDef
    { infId            ∷ !Text     -- ^ unique key, stored on the wound
    , infName          ∷ !Text     -- ^ display name ("Staph infection")
    , infIcon          ∷ !Text     -- ^ icon basename
    , infCategory      ∷ !Text     -- ^ bacterial|parasitic|fungal|viral|prion
    , infSites         ∷ ![Text]   -- ^ "surface" / "deep" wound sites it takes
    , infBaseWeight    ∷ !Float     -- ^ intrinsic selection weight in its pool
    , infTempMin       ∷ !Float     -- ^ favourable temperature band, °C
    , infTempMax       ∷ !Float
    , infMoistMin      ∷ !Float     -- ^ favourable moisture band, 0..1
    , infMoistMax      ∷ !Float
    , infAggressiveness ∷ !Float    -- ^ × Ticker-A growth speed once established
    , infInfectability  ∷ !Float    -- ^ × initial foothold + resistance to the
                                     --   immune response (how readily it takes
                                     --   hold / how hard to clear)
    , infCurableBy     ∷ ![Text]   -- ^ treatments that cut it ("antibiotics")
    , infCureRate      ∷ !Float     -- ^ × how well a cure dose works
    , infWoundInfectable ∷ !Bool    -- ^ False = never selected from a wound
    , infEffects       ∷ ![Text]   -- ^ special-effect tags (data-only for now)
    , infTransmissibility ∷ !Float  -- ^ spread distance, tiles (data-only)
    , infTransmission  ∷ ![Text]   -- ^ spread-vector tags (data-only)
    } deriving (Show, Eq)

-- | Engine-wide registry loaded from data/infections/.
newtype InfectionManager = InfectionManager
    { infmDefs ∷ HM.HashMap Text InfectionDef
    } deriving (Show, Eq)

emptyInfectionManager ∷ InfectionManager
emptyInfectionManager = InfectionManager HM.empty

lookupInfection ∷ Text → InfectionManager → Maybe InfectionDef
lookupInfection n (InfectionManager m) = HM.lookup n m

-- | All wound-infectable defs whose `site` list includes the given site
--   ("surface" / "deep"). Used to build the weighted selection pool.
infectionsForSite ∷ Text → InfectionManager → [InfectionDef]
infectionsForSite site (InfectionManager m) =
    [ d | d ← HM.elems m, infWoundInfectable d, site `elem` infSites d ]
