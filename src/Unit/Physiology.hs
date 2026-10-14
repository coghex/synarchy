{-# LANGUAGE Strict #-}
-- | Physiology system modes: the pure registry queries, the mode
--   resolver, definition validation and the READ contract (#2828, S2 of
--   the physiology stat-modes epic #2827; design
--   @docs\/designs\/physiology_stat_modes_design.md@ D-1, D-2, D-3, D-5a).
--
--   __This module is inactive in production.__ S5 (#2830) routed the
--   gameplay consumers through it, and 'prModesActive' is the inactive
--   compatibility boundary they all share: while it is 'False' (every
--   loaded registry, until S4) the mode-aware answer below is reachable
--   only from controlled tests, and every consumer read —
--   'physioConsumerView', @unit.readPhysio@ \/ @unit.readPhysioBase@, the
--   Lua facade — answers today's stored-stat semantics. 'physioReadPure'
--   itself stays mode-aware whatever the flag says: it is the pure
--   resolver, and the boundary is applied by its callers.
--
--   __The read contract in one place__ (D-2, SD-19). A key is /owned/ when
--   a registry system lists it as an owned key, a derived key or a
--   failure meter; every other key is /unowned/.
--
--     * An owned key's mode is: 'ModeUnsupported' when the definition
--       lacks a capability the system requires; otherwise the explicit
--       unit override, then the species default, then Nominal. The
--       presence of a stored key never chooses it.
--     * Nominal reads evaluate the key's healthy rule against EFFECTIVE
--       inputs, ignore modifiers on the key itself, and read a failure
--       meter as 0. Simulated reads are the stored base plus active
--       modifiers through 'effectiveStat'; the base read is the stored
--       base. A Simulated owned key with no stored value is an invariant
--       violation, never a cue to initialize it.
--     * A derived key is computed on demand and never stored.
--     * Owned reads are PURE: they roll nothing, write nothing and
--       advance no generator. An unowned key is not handled here
--       ('OutcomeUnowned'); the caller delegates it to today's
--       @getStat@, lazy roll included.
module Unit.Physiology
    ( -- * Registry queries
      registrySystem
    , registryProfile
    , KeyRole(..)
    , classifyKey
    , registryKeys
    , resolveQuantity
      -- * Mode resolution
    , unitCapabilities
    , resolveMode
      -- * Definition validation
    , resolveUnitPhysiology
    , rejectPhysiologyTemplates
      -- * Reading
    , saltConcentration
    , ReadKind(..)
    , PhysioContext(..)
    , PhysioOutcome(..)
    , modeOf
    , physioReadPure
      -- * Consumer boundary
    , physioModesActive
    , physioConsumerView
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Unit.Physiology.Types
import Unit.Stats (effectiveStat)
import Unit.Types.Def (StatModifier)

-- * Registry queries

registrySystem ∷ PhysiologyRegistry → Text → Maybe PhysiologySystem
registrySystem reg sid = listToMaybe [ s | s ← prSystems reg, psId s ≡ sid ]

registryProfile ∷ PhysiologyRegistry → Text → Maybe PhysiologyProfile
registryProfile reg pid = listToMaybe [ p | p ← prProfiles reg, ppId p ≡ pid ]

-- | Why a name is registry-owned.
data KeyRole
    = RoleOwned !OwnedKey
    | RoleDerived !DerivedKey
    | RoleMeter !Text
    deriving (Show, Eq)

-- | The system that owns a key and the role it plays there; 'Nothing'
--   for an unowned key. Registration guarantees a name has one owner.
classifyKey ∷ PhysiologyRegistry → Text → Maybe (PhysiologySystem, KeyRole)
classifyKey reg key = listToMaybe
    [ (s, role)
    | s ← prSystems reg
    , role ← [ RoleOwned o | o ← psOwns s, okKey o ≡ key ]
          ⧺ [ RoleDerived d | d ← psDerived s, dkKey d ≡ key ]
          ⧺ [ RoleMeter m | m ← psMeters s, m ≡ key ] ]

-- | Every owned, derived and meter name in the registry.
registryKeys ∷ PhysiologyRegistry → Set Text
registryKeys reg = Set.fromList $ concat
    [ map okKey (psOwns s) ⧺ map dkKey (psDerived s) ⧺ psMeters s
    | s ← prSystems reg ]

-- | A literal, or a constant looked up by name. 'Nothing' for a name the
--   registry does not declare (registration rules those out).
resolveQuantity ∷ PhysiologyRegistry → Quantity → Maybe Float
resolveQuantity _   (QLit f)   = Just f
resolveQuantity reg (QConst n) = Map.lookup n (prConstants reg)

-- * Mode resolution

-- | The capabilities a definition has. Derived from what the YAML
--   authored, never from stored stat keys (requirement 3).
unitCapabilities ∷ UnitPhysiology → Set Text
unitCapabilities up
    | upBodyAuthored up = Set.singleton bodyCapability
    | otherwise         = Set.empty

-- | The one pure mode resolver (requirement 3): capability failure
--   yields 'ModeUnsupported' whatever else is supplied; otherwise the
--   explicit unit override, then the species default, then Nominal.
--
--   The override is a @Maybe@ so that "no override" and "an override
--   equal to the default" are different inputs, which is what intent
--   means (D-3). The live override field is S3's; until then callers
--   pass 'Nothing'.
resolveMode ∷ PhysiologySystem → UnitPhysiology → Maybe PhysioMode → ResolvedMode
resolveMode sys up mOverride
    | not (all (`Set.member` unitCapabilities up) (psRequires sys)) =
        ModeUnsupported
    | otherwise = case chosen of
        PhysioSimulated → ModeSimulated
        PhysioNominal   → ModeNominal
  where
    chosen = case mOverride of
        Just m  → m
        Nothing → fromMaybe PhysioNominal (HM.lookup (psId sys) (upDefaults up))

-- * Definition validation

-- | Resolve a definition's authored @physiology:@ block against the
--   registry: the optional profile, then each per-system entry, which
--   overrides the profile's mode for that system. Left is
--   @(reason, detail)@ naming the definition and the offending name, for
--   the whole-file refusal path.
resolveUnitPhysiology
    ∷ PhysiologyRegistry
    → Text                 -- ^ definition name
    → Bool                 -- ^ whether the YAML authored a @body:@ block
    → Maybe Text           -- ^ the block's @profile:@
    → [(Text, Text)]       -- ^ the block's per-system entries, as written
    → Either (Text, Text) UnitPhysiology
resolveUnitPhysiology reg unit bodyAuthored mProfile entries = do
    profileModes ← case mProfile of
        Nothing → Right []
        Just p  → case registryProfile reg p of
            Nothing → Left ("unknown physiology profile", unit <> ": " <> p)
            Just pr → Right (Map.toList (ppModes pr))
    explicit ← traverse one entries
    pure UnitPhysiology
        { upBodyAuthored = bodyAuthored
        , upProfile      = mProfile
        , upDefaults     = HM.fromList (profileModes ⧺ explicit) }
  where
    one (sid, modeText) = case registrySystem reg sid of
        Nothing → Left ("unknown physiology system", unit <> ": " <> sid)
        Just _  → case parsePhysioMode modeText of
            Nothing → Left ( "invalid physiology mode"
                           , unit <> ": " <> sid <> ": " <> modeText )
            Just m  → Right (sid, m)

-- | The first registry-owned, derived or meter name a definition
--   templates in @stats:@ or @skills:@, as @(reason, detail)@, or
--   'Nothing'. An owned key is never a template (D-3): a template would
--   let @getOrRollStat@ recreate a key a Nominal system must not hold.
--   Unowned names, lazy templates included, are untouched.
rejectPhysiologyTemplates ∷ PhysiologyRegistry → Text → [Text] → [Text]
                          → Maybe (Text, Text)
rejectPhysiologyTemplates reg unit statKeys skillKeys = listToMaybe
    [ ("physiology key in unit template", unit <> ": " <> section <> "." <> k)
    | (section, ks) ← [("stats", statKeys), ("skills", skillKeys)]
    , k ← ks
    , k `Set.member` owned ]
  where
    owned = registryKeys reg

-- * Reading

-- | The salt concentration rule (D-5a), shared by every reader of
--   @salt_conc@: @(salt \/ maxSalt) \/ clamp(hydrationFraction, 0.05, 1)@.
--   A Nominal hydration contributes a fraction of 1.0 and a Nominal salt
--   reads 1.0 outright, both decided by the caller.
saltConcentration ∷ Float → Float → Float
saltConcentration saltFraction hydrationFraction =
    saltFraction / clamp 0.05 1.0 hydrationFraction

-- | Effective value or stored base.
data ReadKind = ReadEffective | ReadBase
    deriving (Show, Eq)

-- | Everything a pure owned read needs, all of it explicit: the registry,
--   the definition's physiology, the unit's override intent (empty in S2),
--   game time, and the unit's stored stats and modifiers.
data PhysioContext = PhysioContext
    { pcRegistry   ∷ !PhysiologyRegistry
    , pcPhysiology ∷ !UnitPhysiology
    , pcOverrides  ∷ !(HM.HashMap Text PhysioMode)
    , pcNow        ∷ !Double
    , pcStats      ∷ !(HM.HashMap Text Float)
    , pcModifiers  ∷ !(HM.HashMap Text [StatModifier])
    }

data PhysioOutcome
    = OutcomeUnowned
      -- ^ no registry system owns the key; today's @getStat@ semantics
      --   apply, and the caller delegates.
    | OutcomeUnsupported
      -- ^ the key's system needs a capability the definition lacks.
    | OutcomeValue !ResolvedMode !Float
    | OutcomeInvariant !Text
      -- ^ a violated precondition, described. The Lua verbs raise it.
    deriving (Show, Eq)

-- | The mode a system resolves to for this unit.
modeOf ∷ PhysioContext → PhysiologySystem → ResolvedMode
modeOf ctx sys =
    resolveMode sys (pcPhysiology ctx) (HM.lookup (psId sys) (pcOverrides ctx))

-- | Read a registry-owned key. Pure: see the module header.
physioReadPure ∷ ReadKind → PhysioContext → Text → PhysioOutcome
physioReadPure kind ctx key = case classifyKey (pcRegistry ctx) key of
    Nothing → OutcomeUnowned
    Just (sys, role) → case modeOf ctx sys of
        ModeUnsupported → OutcomeUnsupported
        mode → case readRole kind ctx sys mode role of
            Left why → OutcomeInvariant $
                "physiology read of '" <> key <> "' (system '" <> psId sys
                <> "'): " <> why
            Right v  → OutcomeValue mode v

readRole ∷ ReadKind → PhysioContext → PhysiologySystem → ResolvedMode
         → KeyRole → Either Text Float
readRole kind ctx _ mode role = case role of
    RoleOwned o → case mode of
        ModeSimulated → simulatedOwned kind ctx (okKey o)
        _             → evalHealthy ctx (okHealthy o)
    RoleMeter m → case mode of
        ModeSimulated → Right (fromMaybe 0 (storedValue kind ctx m))
        _             → Right 0
    RoleDerived d → case mode of
        ModeSimulated → case dkRule d of
            DerivedSaltConcentration → saltConcentrationFor ctx
        _             → Right (dkHealthy d)

-- | A stored key's base, or its effective value.
storedValue ∷ ReadKind → PhysioContext → Text → Maybe Float
storedValue kind ctx k = do
    base ← HM.lookup k (pcStats ctx)
    pure $ case kind of
        ReadBase      → base
        ReadEffective → effectiveStat (pcNow ctx) base
                            (HM.lookupDefault [] k (pcModifiers ctx))

simulatedOwned ∷ ReadKind → PhysioContext → Text → Either Text Float
simulatedOwned kind ctx k = case storedValue kind ctx k of
    Just v  → Right v
    Nothing → Left $
        "Simulated owned key '" <> k <> "' has no stored value"

-- | A stored input that a rule divides by or scales with: present, and
--   finite and positive BOTH as stored and as effective. The stored value
--   is judged first because 'effectiveStat' clamps with @max 0@, which
--   turns a stored NaN or negative into 0, and an active positive
--   modifier can lift a stored zero or negative into a plausible
--   effective value; either would let an invalid input read as a valid
--   one. Never lazily rolled, never defaulted: the Lua this replaces
--   falls back to @body_mass or 70.0@, and a fallback is exactly how a
--   bodyless unit passed for a 70 kg one.
checkedInput ∷ Text → PhysioContext → Text → Either Text Float
checkedInput missing ctx k = do
    stored ← maybe (Left missing) Right (HM.lookup k (pcStats ctx))
    judge "stored" stored
    v ← maybe (Left missing) Right (storedValue ReadEffective ctx k)
    judge "effective" v
    pure v
  where
    judge which x
        | isNaN x ∨ isInfinite x = Left $
            "input '" <> k <> "' is not finite (" <> which <> " "
            <> tshow x <> ")"
        | x ≤ 0 = Left $
            "input '" <> k <> "' is not positive (" <> which <> " "
            <> tshow x <> ")"
        | otherwise = Right ()

-- | A healthy rule's input.
capacity ∷ PhysioContext → Text → Either Text Float
capacity ctx k = checkedInput ("input '" <> k <> "' is absent") ctx k

-- | A Simulated pool a DERIVED rule divides by (the body-water pool
--   behind @salt_conc@). Unlike a plain Simulated owned read, which
--   returns whatever is stored, a derived value built on a nonsensical
--   pool would be a wrong answer with no way to tell.
dependencyPool ∷ PhysioContext → Text → Either Text Float
dependencyPool ctx k =
    checkedInput ("Simulated owned key '" <> k <> "' has no stored value")
                 ctx k

quantity ∷ PhysioContext → Quantity → Either Text Float
quantity ctx q = case resolveQuantity (pcRegistry ctx) q of
    Just f  → Right f
    Nothing → Left ("unknown constant " <> tshow q)

evalHealthy ∷ PhysioContext → HealthyRule → Either Text Float
evalHealthy ctx (HealthyConst q) = quantity ctx q
evalHealthy ctx (HealthyFrom k scales) = do
    v  ← capacity ctx k
    fs ← traverse (quantity ctx) scales
    pure (v * product fs)

-- | @salt_conc@ for a Simulated salt system: the shared pure rule over
--   the unit's effective @salt@ against its healthy maximum, and the
--   effective body-water fraction when hydration is Simulated (1.0
--   otherwise, which is F-1's intended simplification).
saltConcentrationFor ∷ PhysioContext → Either Text Float
saltConcentrationFor ctx = do
    saltRule ← ownedRule "salt"
    maxSalt  ← evalHealthy ctx saltRule
    salt     ← simulatedOwned ReadEffective ctx "salt"
    hydFrac  ← hydrationFraction
    pure (saltConcentration (salt / maxSalt) hydFrac)
  where
    reg = pcRegistry ctx
    ownedRule k = case classifyKey reg k of
        Just (_, RoleOwned o) → Right (okHealthy o)
        _ → Left ("the registry does not own '" <> k <> "'")
    hydrationFraction = case classifyKey reg "hydration" of
        Just (hsys, RoleOwned h) | modeOf ctx hsys ≡ ModeSimulated → do
            maxH ← evalHealthy ctx (okHealthy h)
            cur  ← dependencyPool ctx "hydration"
            pure (cur / maxH)
        _ → Right 1.0

-- * Consumer boundary (S5, #2830)

-- | Is the mode-aware read contract in force? 'False' until S4, whatever
--   the resolver would yield: the one switch every migrated consumer
--   shares, so S4 flips a flag instead of migrating callers again.
physioModesActive ∷ PhysiologyRegistry → Bool
physioModesActive = prModesActive

-- | A unit's stats AS A PURE GAMEPLAY CONSUMER SEES THEM, for the named
--   registry keys: the Haskell face of @physiology.get@, used by code that
--   already holds the unit instance inside a single atomic fold (the wound
--   tick) and so cannot run the IO 'physioRead'.
--
--   Inactive, this is the stored map UNCHANGED — the identical baseline
--   read the consumer made before it was migrated. Active, each named key
--   is replaced by its mode-aware EFFECTIVE value (a Nominal pool reads
--   its healthy rule), and a key whose system is Unsupported is removed so
--   the consumer's absent-pool branch applies. A violated invariant
--   leaves the stored value as it was: a pure tick must not raise, and
--   the Lua verbs report the violation.
physioConsumerView ∷ [Text] → PhysioContext → HM.HashMap Text Float
physioConsumerView keys ctx
    | not (physioModesActive (pcRegistry ctx)) = pcStats ctx
    | otherwise = foldl' step (pcStats ctx) keys
  where
    step acc k = case physioReadPure ReadEffective ctx k of
        OutcomeValue _ v     → HM.insert k v acc
        OutcomeUnsupported   → HM.delete k acc
        OutcomeUnowned       → acc
        OutcomeInvariant _   → acc
