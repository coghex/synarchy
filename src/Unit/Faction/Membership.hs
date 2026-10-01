{-# LANGUAGE Strict #-}
-- | The faction profile a live unit carries (#2515, FTS-3 of the
--   faction-tag arc #2496).
--
--   "Unit.Faction.Profile" is the pure policy model: a controller, a SET
--   of tags, and capabilities, which is all a relation query needs. A
--   unit needs one thing more — for each tag it holds, WHERE that
--   membership came from (D-22). An authored definition default and a
--   tag some runtime system added are different facts: a later system
--   may remove only the memberships it owns, and authored identity
--   changes only through an explicit reclassification. That bookkeeping
--   lives here, on 'UnitFactionProfile', and 'unitPolicyProfile'
--   projects it down to the policy model's 'FactionProfile' whenever a
--   relation question is asked.
--
--   __What this slice may mint.__ Nothing but what the D-26 mapping
--   ('resolveLegacyFaction') produces from a legacy 'Faction' and the
--   unit definition's authored defaults (requirement 9): the one local
--   controller, definition-default memberships, the D-26 mapping's own
--   @wildlife@ fallback and @legacy_hostile@ memberships (owned by
--   'legacyMappingOwner'), and the two debug capabilities. Runtime tag
--   mutation, relation causes, and further controllers are FTS-6.
--
--   __The compatibility adapter.__ Consumers that have not been ported
--   to profile queries yet (FTS-4, FTS-5) read faction through
--   'legacyFactionOf' and through nothing else. It is the exact inverse
--   of D-26 on the legacy profiles and answers 'FactionNeutral' for
--   anything it does not recognize — including an uncontrolled acolyte,
--   which is what a tag-less or @wildlife@ acolyte spawn now IS (D-31).
module Unit.Faction.Membership
    ( -- * Membership provenance
      MembershipSource(..)
    , TagMembership(..)
      -- * The unit's profile
    , UnitFactionProfile
    , mkUnitFactionProfile
    , ufpController
    , ufpMemberships
    , ufpCapabilities
    , inertUnitProfile
    , unitPolicyProfile
      -- * The local controller
    , localController
      -- * Legacy compatibility
    , legacyMappingOwner
    , resolveLegacyFaction
    , resolveSpawnFaction
    , legacyFactionOf
    ) where

import UPrelude
import Data.Set (Set)
import qualified Data.Set as Set
import Unit.Faction (Faction(..), defaultSpawnFaction)
import Unit.Faction.Profile
    ( ControllerId, FactionCapability(..), FactionProfile, FactionTag
    , allFactionCapabilities
    , humanController, legacyProfile, mkProfile, profileCapabilities
    , profileController, profileTags, tagLegacyHostile, tagWildlife )

-- * Membership provenance

-- | Where one tag membership came from (D-22).
data MembershipSource
    = MemberDefinitionDefault
      -- ^ The unit definition's authored @faction_tags:@. Removing one
      --   is a reclassification, never ordinary cleanup.
    | MemberRuntimeOwner !Text
      -- ^ Added by the named runtime owner, which alone may remove it.
    deriving (Show, Eq, Ord)

-- | One tag a unit holds, and who put it there.
data TagMembership = TagMembership
    { tmTag    ∷ !FactionTag
    , tmSource ∷ !MembershipSource
    } deriving (Show, Eq, Ord)

-- * The unit's profile

-- | A live unit's faction state: an optional controller, every tag
--   membership with its provenance, and its capabilities. Sets
--   throughout, so construction order never distinguishes two profiles.
data UnitFactionProfile = UnitFactionProfile
    { ufpController   ∷ !(Maybe ControllerId)
    , ufpMemberships  ∷ !(Set TagMembership)
    , ufpCapabilities ∷ !(Set FactionCapability)
    } deriving (Show, Eq)

mkUnitFactionProfile ∷ Maybe ControllerId → [TagMembership]
                     → [FactionCapability] → UnitFactionProfile
mkUnitFactionProfile c ms cs =
    UnitFactionProfile c (Set.fromList ms) (Set.fromList cs)

-- | No controller, no memberships, no capabilities: what an
--   unrecognized legacy faction string degrades to, and what the legacy
--   @neutral@ faction maps to.
inertUnitProfile ∷ UnitFactionProfile
inertUnitProfile = mkUnitFactionProfile Nothing [] []

-- | The policy model's view: provenance dropped, tags merged.
unitPolicyProfile ∷ UnitFactionProfile → FactionProfile
unitPolicyProfile p =
    mkProfile (ufpController p)
              (map tmTag (Set.toList (ufpMemberships p)))
              (Set.toList (ufpCapabilities p))

-- * The local controller

-- | The engine's one stable local controller identity (requirement 2).
--   Every @"player"@ spawn and every migrated @player@ unit carries
--   exactly this value, and it is the only controller this slice can
--   mint. A constant rather than state: there is one local player, and
--   a save written by one session must name the same controller a later
--   session spawns its own roster under.
localController ∷ ControllerId
localController = humanController "local"

-- * Legacy compatibility

-- | The runtime owner of the memberships the D-26 mapping adds ITSELF —
--   the @wildlife@ fallback for a definition without defaults, and
--   @legacy_hostile@ — as opposed to the definition defaults it carries
--   over, which keep 'MemberDefinitionDefault'.
legacyMappingOwner ∷ Text
legacyMappingOwner = "legacy_faction"

-- | D-26 applied to a unit: 'legacyProfile' against the local
--   controller and the unit definition's authored defaults, with each
--   resulting tag's provenance recorded. A pure function of those two
--   inputs; it never reads a unit name or any registry.
resolveLegacyFaction ∷ [FactionTag] → Faction → UnitFactionProfile
resolveLegacyFaction defaults f =
    mkUnitFactionProfile (profileController p)
        [ TagMembership t (sourceOf t) | t ← Set.toList (profileTags p) ]
        (Set.toList (profileCapabilities p))
  where
    p = legacyProfile localController defaults f
    sourceOf t
        | t `elem` defaults = MemberDefinitionDefault
        | otherwise         = MemberRuntimeOwner legacyMappingOwner

-- | Spawn ingress (requirement 3, D-31): an explicit legacy tag maps by
--   D-26; an OMITTED tag is D-26's @wildlife@ row — the definition's
--   defaults, falling back to @wildlife@ only when it declares none.
resolveSpawnFaction ∷ [FactionTag] → Maybe Faction → UnitFactionProfile
resolveSpawnFaction defaults =
    resolveLegacyFaction defaults ∘ fromMaybe defaultSpawnFaction

-- | The profile → legacy 'Faction' adapter, by this precedence and no
--   other (requirement 4):
--
--   1. the local controller → 'FactionPlayer';
--   2. else BOTH debug capabilities → 'FactionDebug';
--   3. else a @legacy_hostile@ membership → 'FactionHostile';
--   4. else a @wildlife@ membership → 'FactionWildlife';
--   5. else 'FactionNeutral'.
--
--   Exact on the D-26 profiles, except that an uncontrolled profile
--   whose tags are all culture tags — an acolyte- or nomad-only
--   @wildlife@ resolution — reads 'FactionNeutral', as D-31 accepts.
legacyFactionOf ∷ UnitFactionProfile → Faction
legacyFactionOf p
    | ufpController p ≡ Just localController = FactionPlayer
    | all (`Set.member` ufpCapabilities p) allFactionCapabilities
                                             = FactionDebug
    | holds tagLegacyHostile                 = FactionHostile
    | holds tagWildlife                      = FactionWildlife
    | otherwise                              = FactionNeutral
  where
    holds t = any ((≡ t) ∘ tmTag) (Set.toList (ufpMemberships p))
