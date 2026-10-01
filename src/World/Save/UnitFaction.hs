{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | A unit's faction as a save carries it (#2515, FTS-3 of #2496): the
--   snapshot-side state, its frozen wire DTO for the @units@ component
--   (v3+), and the load-boundary resolution of legacy faction text.
--
--   __Why a snapshot can hold an UNRESOLVED legacy tag.__ The @units@
--   v1 and v2 shapes stored one faction string. D-26 turns that string
--   into a profile against the unit DEFINITION's authored default tags,
--   and a component decode is a context-free, payload-only
--   transformation ('World.Save.Component.Types.atVersion') with no
--   definitions in reach. So the migration keeps the legacy text as
--   'FactionLegacyPending', and the load stage — which already holds the
--   resolved 'Unit.Types.UnitDef' for every restored unit — finishes the
--   job in 'resolveUnitFactionSnapshot' before any unit is published.
--   Nothing infers a culture from a unit name or any other field.
--
--   A session captured from live units only ever holds
--   'FactionProfileSnap', so a save written after a load emits concrete
--   v3 profiles. The wire keeps a pending variant only so that encoding
--   is TOTAL over snapshots — a decoded-but-never-loaded v2 session
--   re-encodes to an equivalent v3 payload instead of failing.
--
--   __Wire format.__ Controller kinds and capabilities travel as text,
--   like the scalar faction did (#912), so neither closed enum becomes
--   positional on disk. Tag and capability lists are written in 'Set'
--   order, so equal profiles encode to identical bytes. Decoding is
--   exact for every profile this engine writes; text it cannot read (a
--   malformed tag, an unknown capability or controller kind — only
--   corruption produces one) is DROPPED, so damage can remove identity
--   but never invent control, alliance, or hostility.
module World.Save.UnitFaction
    ( -- * Snapshot state
      UnitFactionSnapshot(..)
    , resolveUnitFactionSnapshot
    , legacyFactionText
      -- * Wire DTO
    , UnitFactionDTO(..)
    , ControllerDTO(..)
    , MembershipDTO(..)
    , MembershipSourceDTO(..)
    , toUnitFactionDTO
    , fromUnitFactionDTO
    ) where

import UPrelude
import qualified Data.Serialize as S
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Unit.Faction (factionTag, parseFaction)
import Unit.Faction.Membership
    ( MembershipSource(..), TagMembership(..), UnitFactionProfile
    , inertUnitProfile, legacyFactionOf, mkUnitFactionProfile
    , resolveLegacyFaction, ufpCapabilities, ufpController
    , ufpMemberships )
import Unit.Faction.Profile
    ( ControllerKind(..), FactionCapability(..), FactionTag
    , aiController, controllerKind, controllerName, factionTagText
    , humanController, mkFactionTag )

-- * Snapshot state

-- | The faction a 'World.Save.Types.UnitInstanceSnapshot' carries.
data UnitFactionSnapshot
    = FactionProfileSnap !UnitFactionProfile
      -- ^ A resolved profile: everything captured from a live unit, and
      --   everything a v3 payload stored.
    | FactionLegacyPending !Text
      -- ^ A v1\/v2 faction string awaiting D-26 resolution against the
      --   unit's loaded definition ('resolveUnitFactionSnapshot').
    deriving (Show, Eq)

-- | The positional 'World.Save.Types.SaveData' bridge derives
--   'S.Serialize' over the snapshot records, so this needs an instance.
--   It goes through the component DTO, so there is one wire shape.
instance S.Serialize UnitFactionSnapshot where
    put = S.put ∘ toUnitFactionDTO
    get = fromUnitFactionDTO <$> S.get

-- | Finish a snapshot's faction against the unit definition's authored
--   default tags. A pending legacy string that names one of the five
--   legacy factions resolves by D-26; any other string yields the inert
--   profile and is returned on the 'Left' so the caller can report it
--   once per distinct value. A resolved profile passes through
--   untouched — restoration never reseeds defaults over saved
--   provenance.
resolveUnitFactionSnapshot ∷ [FactionTag] → UnitFactionSnapshot
                           → (UnitFactionProfile, Maybe Text)
resolveUnitFactionSnapshot defaults snap = case snap of
    FactionProfileSnap p → (p, Nothing)
    FactionLegacyPending t → case parseFaction t of
        Just f  → (resolveLegacyFaction defaults f, Nothing)
        Nothing → (inertUnitProfile, Just t)

-- | The single faction string the FROZEN v1 and v2 shapes hold. Only
--   their test-and-fixture encoders need it: a pending string is written
--   back verbatim, a profile as its legacy adapter tag.
legacyFactionText ∷ UnitFactionSnapshot → Text
legacyFactionText snap = case snap of
    FactionLegacyPending t → t
    FactionProfileSnap p   → factionTag (legacyFactionOf p)

-- * Wire DTO

-- | A controller as the wire holds it: kind as text (@human@ \/ @ai@)
--   and the controller's name.
data ControllerDTO = ControllerDTO
    { cdKind ∷ !Text
    , cdName ∷ !Text
    } deriving (Show, Eq, Generic, S.Serialize)

-- | 'MembershipSource' on the wire. Append-only, like every positional
--   enum ('tools/enum_append_only_audit.py').
data MembershipSourceDTO
    = SourceDefinitionDefaultDTO
    | SourceRuntimeOwnerDTO !Text
    deriving (Show, Eq, Generic, S.Serialize)

data MembershipDTO = MembershipDTO
    { mdTag    ∷ !Text
    , mdSource ∷ !MembershipSourceDTO
    } deriving (Show, Eq, Generic, S.Serialize)

-- | A unit's faction on the @units@ v3 wire. Append-only.
data UnitFactionDTO
    = UnitFactionProfileDTO !(Maybe ControllerDTO) ![MembershipDTO]
                            ![Text]
      -- ^ controller, memberships, capabilities.
    | UnitFactionLegacyDTO !Text
      -- ^ a migrated v1\/v2 string not yet resolved by a load (see the
      --   module header).
    deriving (Show, Eq, Generic, S.Serialize)

toUnitFactionDTO ∷ UnitFactionSnapshot → UnitFactionDTO
toUnitFactionDTO snap = case snap of
    FactionLegacyPending t → UnitFactionLegacyDTO t
    FactionProfileSnap p   → UnitFactionProfileDTO
        (toControllerDTO <$> ufpController p)
        (map toMembershipDTO (Set.toList (ufpMemberships p)))
        (map capabilityText (Set.toList (ufpCapabilities p)))
  where
    toControllerDTO c = ControllerDTO (kindText (controllerKind c))
                                      (controllerName c)
    toMembershipDTO m = MembershipDTO (factionTagText (tmTag m))
                                      (toSourceDTO (tmSource m))
    toSourceDTO s = case s of
        MemberDefinitionDefault → SourceDefinitionDefaultDTO
        MemberRuntimeOwner o    → SourceRuntimeOwnerDTO o

fromUnitFactionDTO ∷ UnitFactionDTO → UnitFactionSnapshot
fromUnitFactionDTO dto = case dto of
    UnitFactionLegacyDTO t → FactionLegacyPending t
    UnitFactionProfileDTO mc ms cs → FactionProfileSnap $
        mkUnitFactionProfile (fromControllerDTO =<< mc)
            (mapMaybe fromMembershipDTO ms)
            (mapMaybe parseCapability cs)
  where
    fromControllerDTO (ControllerDTO k n) = case k of
        "human" → Just (humanController n)
        "ai"    → Just (aiController n)
        _       → Nothing
    fromMembershipDTO (MembershipDTO t s) =
        (\tag → TagMembership tag (fromSourceDTO s)) <$> mkFactionTag t
    fromSourceDTO s = case s of
        SourceDefinitionDefaultDTO → MemberDefinitionDefault
        SourceRuntimeOwnerDTO o    → MemberRuntimeOwner o

kindText ∷ ControllerKind → Text
kindText k = case k of
    ControllerHuman → "human"
    ControllerAI    → "ai"

capabilityText ∷ FactionCapability → Text
capabilityText c = case c of
    CapLocalCommandable   → "local_commandable"
    CapUnrestrictedCombat → "unrestricted_combat"

parseCapability ∷ Text → Maybe FactionCapability
parseCapability t = case t of
    "local_commandable"   → Just CapLocalCommandable
    "unrestricted_combat" → Just CapUnrestrictedCombat
    _                     → Nothing
