{-# LANGUAGE Strict #-}
-- | The closed location anchor-tag vocabulary (#801, #1681).
--
--   A location definition's @anchor:@ list is the placement constraint
--   the world-gen overlay pass (#89) filters candidate chunks on. #801
--   required one authoritative vocabulary and required an unknown or
--   unimplemented tag never to become an unconstrained match; the
--   shipped result validated at the YAML boundary but carried the tag
--   forward as free 'Text', so acceptance
--   ('Engine.Asset.YamlLocations'), terrain semantics and the #414
--   water opt-in ('Location.Overlay') were three separately
--   hand-maintained lists over one vocabulary, and the always-true
--   fallback #801 existed to remove was still reachable by adding a tag
--   to the acceptance list alone.
--
--   This module is that single authority. 'LocationAnchor' is closed —
--   one constructor per supported tag — so:
--
--     * a 'Location.Types.LocationDef' cannot carry a tag outside the
--       vocabulary, from YAML or from Haskell;
--     * every consumer's mapping is total over it, and adding a
--       constructor without giving it semantics fails the @-Werror@
--       build on @-Wincomplete-patterns@ rather than silently
--       inheriting a wildcard;
--     * the accepted-tag list a rejection diagnostic prints is DERIVED
--       ('locationAnchorTags') rather than typed out a second time.
--
--   It is deliberately a dependency-free leaf: it imports 'UPrelude'
--   and nothing else local, so 'Engine.Asset.YamlLocations' can own the
--   authored-spelling boundary without depending on runtime location or
--   placement code (the shape its sibling YAML loaders share).
--
--   Anchor tags are NOT serialized. 'Location.Types.LocationDef' is
--   reloaded from @data\/locations\/*.yaml@ on every boot and saves
--   reference definitions by id only, so this type deliberately derives
--   no 'Serialize' instance and its constructor ORDER carries no wire
--   meaning — unlike the append-only enums
--   @tools\/enum_append_only_audit.py@ guards. The order below is the
--   canonical presentation order the derived 'Bounded'\/'Enum'
--   enumeration and every derived list ('allLocationAnchors',
--   'locationAnchorTags') report in.
module Location.Anchor
    ( LocationAnchor(..)
    , allLocationAnchors
    , locationAnchorText
    , locationAnchorTags
    , parseLocationAnchor
    ) where

import UPrelude

-- | One supported anchor tag. The vocabulary is terrain\/height
--   (flat\/mountain\/highland\/lowland), ocean-distance
--   (coast\/coastal\/inland), and the #414 water-proximity opt-out
--   modifier ('AnchorWaterside').
--
--   Extending it is a deliberate, compile-checked act: a new
--   constructor must be given an authored spelling
--   ('locationAnchorText'), terrain semantics
--   ('Location.Overlay.anchorOk') and a water-proximity policy
--   ('Location.Overlay.wantsWater'), because each of those is total
--   over this type.
data LocationAnchor
    = AnchorFlat       -- ^ @flat@ — low sampled elevation SPREAD
    | AnchorMountain   -- ^ @mountain@ — high median elevation
    | AnchorHighland   -- ^ @highland@ — above the highland percentile
    | AnchorLowland    -- ^ @lowland@ — below the lowland percentile
    | AnchorCoast      -- ^ @coast@ — exactly one chunk from the ocean
    | AnchorCoastal    -- ^ @coastal@ — synonym of 'AnchorCoast'
    | AnchorInland     -- ^ @inland@ — four or more chunks from the ocean
    | AnchorWaterside  -- ^ @waterside@ — tolerate nearby water (#414),
                       --   with no terrain constraint of its own
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Every inhabitant, in the canonical constructor order. Derived from
--   the type, so it cannot fall out of step with it.
allLocationAnchors ∷ [LocationAnchor]
allLocationAnchors = [minBound .. maxBound]

-- | The exact spelling a definition authors this tag as in YAML, and
--   the exact string @engine.listLocationDefs()@ reports it as. Total
--   by construction: no wildcard, so a new constructor must be given a
--   spelling here before the tree builds.
locationAnchorText ∷ LocationAnchor → Text
locationAnchorText a = case a of
    AnchorFlat      → "flat"
    AnchorMountain  → "mountain"
    AnchorHighland  → "highland"
    AnchorLowland   → "lowland"
    AnchorCoast     → "coast"
    AnchorCoastal   → "coastal"
    AnchorInland    → "inland"
    AnchorWaterside → "waterside"

-- | The accepted-tag list, derived from the type in canonical order —
--   what a rejection diagnostic prints as "expected one of".
locationAnchorTags ∷ [Text]
locationAnchorTags = map locationAnchorText allLocationAnchors

-- | Parse one authored tag. 'Nothing' for anything outside the
--   vocabulary — a typo, or an unimplemented climate\/biome name — which
--   is what 'Engine.Asset.YamlLocations' rejects the whole definition
--   file on. Built from 'locationAnchorText' rather than a second
--   hand-written table, so acceptance and rendering cannot disagree.
parseLocationAnchor ∷ Text → Maybe LocationAnchor
parseLocationAnchor tag =
    lookup tag [ (locationAnchorText a, a) | a ← allLocationAnchors ]
