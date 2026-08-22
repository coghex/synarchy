{-# LANGUAGE Strict #-}
-- | The power-node declaration a building def carries (#1148): what
--   kind of node placing this building mints, and the one rating that
--   kind needs. This is CONTENT schema, not wire format — it is what
--   @data/buildings/*.yaml@ declares and what
--   'Building.Types.bdPowerDrain'\'s node-side counterpart
--   'Building.Types.bdPowerNode' holds.
--
--   It lives in its own leaf module because both ends need it and the
--   dependency only runs one way: 'Power.Types' imports
--   'Building.Types' (for 'Building.Types.BuildingId'), so
--   'Building.Types' cannot import 'Power.Types' back. Following the
--   repository's Base\/Types convention, this module has no local
--   dependencies at all, which lets 'Building.Types',
--   'Engine.Asset.YamlBuildings' and 'Power.Types' all import it.
--
--   Deliberately NOT 'Power.Types.PowerRole'. That enum is the
--   RUNTIME/persistence role, positionally serialized into the
--   @power-nodes@ component and frozen append-only by
--   @tools\/enum_append_only_audit.py@ (#1145). This type is the
--   editable content vocabulary in front of it; 'Power.Types.powerNodeRole'
--   is the one total mapping between the two. Keeping them separate is
--   what lets the YAML vocabulary grow without touching a wire enum.
--
--   The rating rides INSIDE the constructor rather than beside it, so
--   "a source with no peak watts" and "a node carrying both ratings"
--   are unrepresentable rather than merely rejected —
--   'powerNodeSpecFromYaml' is the only way to build one from content,
--   and it is where every malformed declaration is turned into an
--   error message.
module Power.Base
    ( PowerNodeSpec(..)
    , powerNodeSpecRating
    , powerNodeSpecFromYaml
    ) where

import UPrelude

-- | What kind of power node a building def declares itself to be, with
--   that kind's one meaningful rating.
data PowerNodeSpec
    = PowerNodeSource !Float
      -- ^ Nominal output at full sun, in watts (@power_peak@).
    | PowerNodeStorage !Float
      -- ^ Bank capacity, in watt-hours (@power_capacity@).
    deriving (Show, Eq)

-- | The declared rating, whichever kind it belongs to.
powerNodeSpecRating ∷ PowerNodeSpec → Float
powerNodeSpecRating (PowerNodeSource  w)  = w
powerNodeSpecRating (PowerNodeStorage wh) = wh

-- | Validate the three optional building-YAML values into a spec.
--
--   @Right Nothing@ is the ordinary case: a def that declares no role
--   is simply not a power node. @Left@ is a content mistake, reported
--   so the loader refuses the whole file rather than letting a
--   half-declared node route through ordinary building placement:
--
--     * an unknown @power_role@;
--     * a role whose own rating is missing;
--     * a rating declared for the other role (or with no role at all);
--     * a negative or non-finite rating.
--
--   A YAML @null@ reads as absent throughout (aeson's @.:?@ does the
--   same), so @power_role: null@ is "no role", not "unknown role".
powerNodeSpecFromYaml ∷ Maybe Text   -- ^ @power_role@
                      → Maybe Float  -- ^ @power_peak@
                      → Maybe Float  -- ^ @power_capacity@
                      → Either Text (Maybe PowerNodeSpec)
powerNodeSpecFromYaml mRole mPeak mCapacity = case mRole of
    Nothing → case (mPeak, mCapacity) of
        (Nothing, Nothing) → Right Nothing
        _ → Left "declares a power rating without a power_role — add\
                  \ power_role: source or power_role: storage, or drop\
                  \ the rating"
    Just "source" → do
        rejectOther "power_capacity" "source" mCapacity
        Just . PowerNodeSource ⊚ require "power_peak" "source" mPeak
    Just "storage" → do
        rejectOther "power_peak" "storage" mPeak
        Just . PowerNodeStorage ⊚ require "power_capacity" "storage" mCapacity
    Just other → Left ("has an unknown power_role " <> quoted other
                       <> " — expected \"source\" or \"storage\"")
  where
    require field role = \case
        Nothing → Left ("declares power_role: " <> role <> " but no "
                        <> field)
        Just v
            | isNaN v ∨ isInfinite v →
                Left (field <> " is not a finite number")
            | v < 0 → Left (field <> " is negative")
            | otherwise → Right v
    rejectOther field role = \case
        Nothing → Right ()
        Just _  → Left ("declares power_role: " <> role <> " but also "
                        <> field <> ", which belongs to the other role")
    quoted t = "\"" <> t <> "\""
