{-# LANGUAGE Strict #-}
-- | The pure faction identity and relation policy model (#2500, FTS-1 of
--   the faction-tag arc #2496).
--
--   This module sits BESIDE the scalar "Unit.Faction" enum, not on top of
--   it. Nothing here is live: no 'Unit.Types.UnitInstance' field, no save
--   byte, no Lua verb, no consumer. "Unit.Faction" and its export list are
--   untouched, and later slices port callers over one at a time.
--
--   __The split this module exists to make.__ The scalar model answers
--   "which of five factions is this?" and derives ownership, command
--   rights, and diplomacy from that one value. Three questions are being
--   separated here (design §"Separate identity from control"):
--
--   * __Controller identity__ — which specific human or AI command
--     authority, if any, owns this unit ('ControllerId'). Equality
--     supplies a friendly default. There is deliberately no reserved
--     @none@ controller: absence is 'Nothing', and two profiles that both
--     lack a controller never count as sharing one — a fabricated @none@
--     would make every naturally occurring unit in the world allies
--     (D-5).
--   * __Relationship tags__ — opaque validated strings ('FactionTag') for
--     natural affiliations (@acolyte@), organizations (@red_tribe@),
--     session teams, and temporary conflict groups (@fight_team_A@). A
--     profile holds any number. Runtime systems mint their own; this
--     module validates a safe representation and evaluates whatever it
--     receives (D-11, D-12).
--   * __Capabilities__ — exceptional policy flags ('FactionCapability').
--     Capabilities are NOT tags: they affect ownership, commandability,
--     and attack permission, and never take part in the relation query
--     (D-24).
--
--   __Directed relation precedence__ ('relationFromTo'), and nothing
--   else. @A → B@ and @B → A@ are computed independently:
--
--   1. any live directed cause from a subject tag to a target tag,
--      reducing every match by @hostile > neutral > ally@ (D-15, D-23);
--   2. else both profiles carry the same controller → 'RelAlly' (D-2);
--   3. else the profiles share any tag → 'RelAlly' (D-9);
--   4. else the base table's directed relation for any matching tag pair,
--      reduced the same way (D-7, D-8);
--   5. else 'RelNeutral'.
--
--   Tier 1 outranking the controller is what lets one player's roster
--   fight itself through @fight_team_A → fight_team_B@; a shared tag
--   outranking the base table is what makes a red-tribe acolyte and a
--   red-tribe nomad allies. Controller INEQUALITY never creates hostility
--   on its own.
--
--   __Invalid data degrades inertly.__ A tag string that fails
--   'isValidFactionTagText' is dropped by 'profileFromTagText' rather
--   than admitted, so malformed input can only ever remove identity. It
--   can never create control, alliance, or hostility.
--
--   __Insertion order never changes an answer.__ Every reduction here is
--   a commutative max over 'relationSeverity', the live overlay is keyed
--   by @(source tag, target tag)@ and then by cause identity, and a cause
--   re-added under an identity it already occupies merges by that same
--   severity rather than overwriting. Downgrading a live relation is
--   therefore done by REMOVING its cause, never by layering a friendlier
--   value over it (D-15).
--
--   __The legacy matrix.__ Under the compatibility base table of D-28
--   (symmetric @hostile@ for @acolyte@\/@nomad@, @acolyte@\/@wildlife@,
--   @legacy_hostile@\/@acolyte@, and @legacy_hostile@\/@wildlife@, and
--   nothing else), the D-26 profiles built by 'legacyProfile' give this
--   deterministic table. Rows are the subject, columns the target;
--   @A@ = ally, @N@ = neutral, @H@ = hostile:
--
--   >             player  wildlife  hostile  neutral  debug
--   >   player       A        H         H       N       N*
--   >   wildlife     H        A         H       N       N*
--   >   hostile      H        H         A       N       N*
--   >   neutral      N        N         N       N*      N
--   >   debug        N*       N*        N*      N       N*
--
--   Every distinct pair among player, wildlife, and hostile is hostile;
--   each is allied with itself (player through its controller, the other
--   two through their shared tag). The eight starred entries are the
--   eight ordered pairs whose relation DIFFERS from
--   'Unit.Faction.factionRelation' today, and all eight are the intended
--   consequence of D-27 making the legacy @debug@ profile
--   diplomatically inert (no controller, no tags, capabilities only) and
--   of an identity-less profile sharing nothing with anything:
--
--   * @player → debug@ and @debug → player@: was ally, now neutral. The
--     declared player\/debug medic alliance ends.
--   * @wildlife → debug@ and @debug → wildlife@: was hostile, now
--     neutral.
--   * @hostile → debug@ and @debug → hostile@: was hostile, now neutral.
--   * @neutral → neutral@: was ally, now neutral.
--   * @debug → debug@: was ally, now neutral.
--
--   __Attack permission is unchanged.__ 'canProfileAttack' matches
--   'Unit.Faction.canAttack' for all 25 ordered legacy pairs. The six
--   debug pairs that stopped being hostile stay permitted through
--   'CapUnrestrictedCombat' instead, which is exactly the capability's
--   purpose, and neutral\/neutral was already forbidden.
--
--   Anything data-authored — the YAML tag catalogue, unit-definition
--   defaults, and the shipped base table — is FTS-2. The 'FactionPolicy'
--   here is built in code by its callers, and its only shipped caller is
--   this slice's spec.
module Unit.Faction.Profile
    ( -- * Controller identity
      ControllerKind(..)
    , ControllerId
    , humanController
    , aiController
    , controllerKind
    , controllerName
      -- * Relationship tags
    , FactionTag
    , isValidFactionTagText
    , mkFactionTag
    , factionTagText
    , tagAcolyte
    , tagWildlife
    , tagLegacyHostile
      -- * Capabilities
    , FactionCapability(..)
    , allFactionCapabilities
      -- * Profiles
    , FactionProfile
    , emptyProfile
    , mkProfile
    , profileFromTagText
    , profileController
    , profileTags
    , profileCapabilities
    , profileHasCapability
      -- * Properties of one profile
    , isProfilePlayerOwned
    , isProfileCommandable
    , profileHasUnrestrictedCombat
      -- * Policy context
    , RelationCauseId
    , relationCauseId
    , relationCauseIdText
    , BaseRelationEntry(..)
    , FactionPolicy
    , emptyFactionPolicy
    , mkFactionPolicy
    , baseRelationFor
    , liveCausesFor
    , addRelationCause
    , removeRelationCause
      -- * Relation and permission queries
    , relationSeverity
    , relationFromTo
    , hasLiveReverseHostility
    , canProfileAttack
      -- * Legacy compatibility profiles
    , legacyProfile
    , legacyPlayerProfile
    , legacyWildlifeProfile
    , legacyHostileProfile
    , legacyNeutralProfile
    , legacyDebugProfile
    ) where

import UPrelude
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Unit.Faction (Faction(..), FactionRelation(..))

-- * Controller identity

-- | Whether a command authority is a human player or an AI (D-10).
--
--   This is a closed enum on purpose: it distinguishes the KINDS of
--   authority the engine knows how to route orders through. The identity
--   itself ('ControllerId') stays open, because player 1, player 2, and
--   any number of AI authorities must remain distinguishable without a
--   constructor apiece.
data ControllerKind
    = ControllerHuman
    | ControllerAI
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | A specific command authority. Opaque: two controllers are the same
--   authority only when their kind AND their name agree, so an AI named
--   @"1"@ never accidentally owns the human player's roster.
data ControllerId = ControllerId ControllerKind Text
    deriving (Show, Eq, Ord)

-- | A human command authority with the given identifier.
humanController ∷ Text → ControllerId
humanController = ControllerId ControllerHuman

-- | An AI command authority with the given identifier.
aiController ∷ Text → ControllerId
aiController = ControllerId ControllerAI

controllerKind ∷ ControllerId → ControllerKind
controllerKind (ControllerId k _) = k

controllerName ∷ ControllerId → Text
controllerName (ControllerId _ n) = n

-- * Relationship tags

-- | An opaque, validated relationship tag. Build one with 'mkFactionTag'
--   and read it back with 'factionTagText'; there is no other way in, so
--   an invalid string can never reach the relation query.
newtype FactionTag = FactionTag Text
    deriving (Show, Eq, Ord)

-- | The one validation rule this module owns: a tag is a NON-EMPTY string
--   containing NO whitespace.
--
--   Deliberately permissive beyond that. An unrecognized but
--   syntactically valid string is a legitimate tag, because runtime
--   systems mint team and conflict identifiers this module has never
--   heard of (D-12); rejecting the unknown would break exactly the case
--   the tag system exists to serve. Whitespace is excluded because tags
--   are identifiers that authored data, logs, and future wire formats
--   have to round-trip unambiguously.
isValidFactionTagText ∷ Text → Bool
isValidFactionTagText t = not (T.null t) ∧ not (T.any isSpace t)

-- | 'Nothing' for a string that fails 'isValidFactionTagText'.
mkFactionTag ∷ Text → Maybe FactionTag
mkFactionTag t
    | isValidFactionTagText t = Just (FactionTag t)
    | otherwise               = Nothing

factionTagText ∷ FactionTag → Text
factionTagText (FactionTag t) = t

-- | The natural affiliation the legacy @player@ faction migrates to
--   (D-26). A stand-in for the unit definition's own default tags, which
--   FTS-2 introduces.
tagAcolyte ∷ FactionTag
tagAcolyte = FactionTag "acolyte"

-- | The natural affiliation the legacy @wildlife@ faction migrates to.
tagWildlife ∷ FactionTag
tagWildlife = FactionTag "wildlife"

-- | The compatibility affiliation the legacy @hostile@ faction migrates
--   to. Named for what it is — a migration artifact, not a culture — so
--   that authored content never adopts it by accident.
tagLegacyHostile ∷ FactionTag
tagLegacyHostile = FactionTag "legacy_hostile"

-- * Capabilities

-- | An exceptional policy flag. Capabilities answer ownership,
--   commandability, and attack-permission questions ONLY; they are never
--   consulted by 'relationFromTo' (D-24).
data FactionCapability
    = CapLocalCommandable
      -- ^ Takes local player orders without being player-owned. The
      --   legacy @debug@ profile's half of today's
      --   'Unit.Faction.isPlayerCommandable'.
    | CapUnrestrictedCombat
      -- ^ Ignores friendly-fire restrictions entirely, as attacker or as
      --   target. Staged debug fights depend on it.
    deriving (Show, Eq, Ord, Enum, Bounded)

allFactionCapabilities ∷ [FactionCapability]
allFactionCapabilities = [minBound .. maxBound]

-- * Profiles

-- | Who a unit is: an optional controller, a set of tags, and a set of
--   capabilities. Opaque — the constructor is not exported, so every
--   profile has been through tag validation.
data FactionProfile = FactionProfile
    { profileController   ∷ Maybe ControllerId
      -- ^ 'Nothing' means no strategic controller. It does NOT mean a
      --   shared @none@ authority: see 'relationFromTo'.
    , profileTags         ∷ Set FactionTag
    , profileCapabilities ∷ Set FactionCapability
    } deriving (Show, Eq)

-- | No controller, no tags, no capabilities. Neutral toward every
--   profile including another empty one, owns nothing, commands nothing,
--   and may neither attack nor be attacked except through
--   'CapUnrestrictedCombat' on the other side.
emptyProfile ∷ FactionProfile
emptyProfile = FactionProfile Nothing Set.empty Set.empty

mkProfile ∷ Maybe ControllerId → [FactionTag] → [FactionCapability]
          → FactionProfile
mkProfile c ts cs = FactionProfile c (Set.fromList ts) (Set.fromList cs)

-- | Build from raw strings, DROPPING every tag that fails validation.
--   Malformed input therefore yields a profile with less identity, never
--   one with more: it cannot create control, alliance, or hostility.
profileFromTagText ∷ Maybe ControllerId → [Text] → [FactionCapability]
                   → FactionProfile
profileFromTagText c ts = mkProfile c (catMaybes (map mkFactionTag ts))

profileHasCapability ∷ FactionCapability → FactionProfile → Bool
profileHasCapability c p = Set.member c (profileCapabilities p)

-- * Properties of one profile

-- | Is this the observing controller's OWN unit?
--
--   Observer-relative on purpose: in multiplayer a unit controlled by
--   player 2 is player-owned from player 2's session and not from player
--   1's. A profile with no controller is owned by nobody, so this is
--   'False' for it against every observer.
isProfilePlayerOwned ∷ ControllerId → FactionProfile → Bool
isProfilePlayerOwned local p = profileController p ≡ Just local

-- | May this unit receive the observing controller's orders? Ownership,
--   or the explicit capability that lets a debug spawn be commanded
--   without being owned.
isProfileCommandable ∷ ControllerId → FactionProfile → Bool
isProfileCommandable local p =
       isProfilePlayerOwned local p
    ∨ profileHasCapability CapLocalCommandable p

profileHasUnrestrictedCombat ∷ FactionProfile → Bool
profileHasUnrestrictedCombat = profileHasCapability CapUnrestrictedCombat

-- * Policy context

-- | The provenance of one live directed relation. Attack orders,
--   communicated incidents, and diplomacy each own their own causes, so
--   removing one never removes another's (D-14, D-16).
newtype RelationCauseId = RelationCauseId Text
    deriving (Show, Eq, Ord)

-- | Total: a cause identity is a bare label minted by the owning system,
--   not a diplomatic string, so it carries no validation rule of its own.
relationCauseId ∷ Text → RelationCauseId
relationCauseId = RelationCauseId

relationCauseIdText ∷ RelationCauseId → Text
relationCauseIdText (RelationCauseId t) = t

-- | One declaration in the base relation table.
data BaseRelationEntry
    = DirectedBase FactionTag FactionTag FactionRelation
      -- ^ @source → target@ only. The reverse direction stays whatever
      --   the rest of the table and the precedence say.
    | SymmetricBase FactionTag FactionTag FactionRelation
      -- ^ Shorthand expanding to BOTH directions. Ordinary mutual
      --   hostility is the common case and spelling it twice invites a
      --   half-declared pair.
    deriving (Show, Eq)

-- | The static base table plus the live directed overlay.
data FactionPolicy = FactionPolicy
    { policyBase ∷ Map (FactionTag, FactionTag) FactionRelation
    , policyLive ∷ Map (FactionTag, FactionTag)
                       (Map RelationCauseId FactionRelation)
    } deriving (Show, Eq)

-- | Base table only; the live overlay starts empty.
mkFactionPolicy ∷ [BaseRelationEntry] → FactionPolicy
mkFactionPolicy entries = FactionPolicy
    { policyBase = foldr addEntry Map.empty entries
    , policyLive = Map.empty }
  where
    addEntry (DirectedBase s t r)  = declare s t r
    addEntry (SymmetricBase a b r) = declare a b r ∘ declare b a r
    declare s t r = Map.insertWith moreSevere (s, t) r

emptyFactionPolicy ∷ FactionPolicy
emptyFactionPolicy = mkFactionPolicy []

-- | The declared base relation for one ordered tag pair, if any.
baseRelationFor ∷ FactionPolicy → FactionTag → FactionTag
                → Maybe FactionRelation
baseRelationFor pol s t = Map.lookup (s, t) (policyBase pol)

-- | Every live cause currently running from @s@ to @t@, by identity.
liveCausesFor ∷ FactionPolicy → FactionTag → FactionTag
              → Map RelationCauseId FactionRelation
liveCausesFor pol s t = Map.findWithDefault Map.empty (s, t) (policyLive pol)

-- | Install a live directed cause. Adding the same identity to the same
--   ordered pair twice leaves ONE cause; if the second add names a
--   different relation, the two merge by 'moreSevere' rather than the
--   later write winning, so insertion order cannot change the answer
--   (D-23). Softening a live relation is done with 'removeRelationCause',
--   never by re-adding a friendlier value over a hostile one (D-15).
addRelationCause ∷ RelationCauseId → FactionTag → FactionTag
                 → FactionRelation → FactionPolicy → FactionPolicy
addRelationCause cid s t r pol = pol
    { policyLive = Map.insertWith (Map.unionWith moreSevere) (s, t)
                                  (Map.singleton cid r) (policyLive pol) }

-- | Remove exactly one cause. Removing an identity that is not installed
--   is a no-op, and removing the last cause on a pair drops the pair
--   entirely so the profile falls back through the remaining precedence
--   tiers.
removeRelationCause ∷ RelationCauseId → FactionTag → FactionTag
                    → FactionPolicy → FactionPolicy
removeRelationCause cid s t pol = pol
    { policyLive = Map.update prune (s, t) (policyLive pol) }
  where
    prune causes
        | Map.null remaining = Nothing
        | otherwise          = Just remaining
      where remaining = Map.delete cid causes

-- * Relation and permission queries

-- | The deterministic severity order @hostile > neutral > ally@ (D-23),
--   spelled out rather than left to 'FactionRelation'\'s derived 'Ord' so
--   that reordering those constructors cannot silently invert every
--   reduction in this module.
relationSeverity ∷ FactionRelation → Int
relationSeverity r = case r of
    RelAlly    → 0
    RelNeutral → 1
    RelHostile → 2

-- | The commutative, associative reduction every tier uses.
moreSevere ∷ FactionRelation → FactionRelation → FactionRelation
moreSevere a b
    | relationSeverity b > relationSeverity a = b
    | otherwise                               = a

reduceRelations ∷ [FactionRelation] → Maybe FactionRelation
reduceRelations []       = Nothing
reduceRelations (r : rs) = Just (foldr moreSevere r rs)

-- | How @subject@ regards @target@, by the precedence in the module
--   header and nothing else. Directed: the reverse question is a separate
--   call with a possibly different answer.
relationFromTo ∷ FactionPolicy → FactionProfile → FactionProfile
               → FactionRelation
relationFromTo pol subject target = case reduceRelations liveMatches of
    Just live → live
    Nothing
        | sameController → RelAlly
        | sharesTag      → RelAlly
        | otherwise      → fromMaybe RelNeutral (reduceRelations baseMatches)
  where
    pairs = orderedTagPairs subject target
    liveMatches =
        concatMap (\(s, t) → Map.elems (liveCausesFor pol s t)) pairs
    baseMatches =
        catMaybes (map (\(s, t) → baseRelationFor pol s t) pairs)
    -- Two absent controllers are not a shared controller (D-5).
    sameController = case (profileController subject
                          ,profileController target) of
        (Just a, Just b) → a ≡ b
        _                → False
    sharesTag = not (Set.null (Set.intersection (profileTags subject)
                                                (profileTags target)))

-- | Every @(subject tag, target tag)@ pair, in the order the tag sets
--   yield them. Only ever consumed by an order-independent reduction.
orderedTagPairs ∷ FactionProfile → FactionProfile
                → [(FactionTag, FactionTag)]
orderedTagPairs subject target =
    [ (s, t)
    | s ← Set.toList (profileTags subject)
    , t ← Set.toList (profileTags target) ]

-- | Does any LIVE hostile cause run from a target tag back to a subject
--   tag? The reverse-hostility predicate FTS-6 builds Hold\/Cancel and
--   escalation on (D-17, D-18): before it holds, canceling an attack
--   order removes the attacker's own cause; once it holds, the fight has
--   escalated and needs the separate resolution path.
--
--   Live causes only. Base-table hostility in the reverse direction is
--   the world's standing disposition, not an installed reaction to
--   something the subject did, and 'CapUnrestrictedCombat' is not a
--   diplomatic relation at all.
hasLiveReverseHostility ∷ FactionPolicy → FactionProfile → FactionProfile
                        → Bool
hasLiveReverseHostility pol subject target =
    any (≡ RelHostile) reverseCauses
  where
    reverseCauses =
        [ r
        | (t, s) ← orderedTagPairs target subject
        , r ← Map.elems (liveCausesFor pol t s) ]

-- | May @subject@ be ordered to attack @target@? Hostility is the normal
--   permission; 'CapUnrestrictedCombat' on EITHER participant overrides
--   it. Reads 'relationFromTo' and never re-derives precedence.
canProfileAttack ∷ FactionPolicy → FactionProfile → FactionProfile → Bool
canProfileAttack pol subject target =
       relationFromTo pol subject target ≡ RelHostile
    ∨ profileHasUnrestrictedCombat subject
    ∨ profileHasUnrestrictedCombat target

-- * Legacy compatibility profiles

-- | The D-26 profile for each legacy 'Faction' value, given the local
--   controller. See the module header for the matrix these produce.
legacyProfile ∷ ControllerId → Faction → FactionProfile
legacyProfile local f = case f of
    FactionPlayer   → legacyPlayerProfile local
    FactionWildlife → legacyWildlifeProfile
    FactionHostile  → legacyHostileProfile
    FactionNeutral  → legacyNeutralProfile
    FactionDebug    → legacyDebugProfile

-- | Local controller plus 'tagAcolyte'.
legacyPlayerProfile ∷ ControllerId → FactionProfile
legacyPlayerProfile local = mkProfile (Just local) [tagAcolyte] []

-- | No controller, 'tagWildlife'.
legacyWildlifeProfile ∷ FactionProfile
legacyWildlifeProfile = mkProfile Nothing [tagWildlife] []

-- | No controller, 'tagLegacyHostile'.
legacyHostileProfile ∷ FactionProfile
legacyHostileProfile = mkProfile Nothing [tagLegacyHostile] []

-- | The inert profile: 'emptyProfile' exactly.
legacyNeutralProfile ∷ FactionProfile
legacyNeutralProfile = emptyProfile

-- | No controller and no tags — diplomatically inert (D-27) — carrying
--   both capabilities, which is what preserves every attack direction the
--   debug overlay allows today.
legacyDebugProfile ∷ FactionProfile
legacyDebugProfile = mkProfile Nothing [] allFactionCapabilities
