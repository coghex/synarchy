{-# LANGUAGE Strict #-}
-- | Physiology system modes: the pure registry queries, the mode
--   resolver, definition validation and the READ contract (#2828, S2 of
--   the physiology stat-modes epic #2827; design
--   @docs\/designs\/physiology_stat_modes_design.md@ D-1, D-2, D-3, D-5a).
--
--   __Active since S4 (#2831).__ Every gameplay consumer read — the Lua
--   facade, @unit.readPhysio@ \/ @unit.readPhysioBase@, Haskell
--   'physioRead', the wound tick's 'physioConsumerView' — answers the
--   mode-aware contract below, and "Unit.Physiology.Transition" owns the
--   transitions between modes.
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
    , combatEngagedKey
    , engagementPending
    , engagementApplied
    , declaresEngagement
    , resolveMode
      -- * Definition validation
    , resolveUnitPhysiology
    , rejectPhysiologyTemplates
    , engagementSystem
      -- * Reading
    , saltConcentration
    , ReadKind(..)
    , PhysioContext(..)
    , PhysioOutcome(..)
    , modeOf
    , physioReadPure
    , evalHealthy
    , storedValue
    , checkedInput
    , checkedPool
      -- * Consumer boundary
    , physioConsumerView
      -- * Raw write gate
    , rawWriteAllowed
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Control.Monad (foldM)
import Data.Set (Set)
import qualified Data.Set as Set
import Unit.Blood (BloodView(..))
import Unit.Physiology.Circulation (CirculationInputs(..), circulationRule)
import Unit.Physiology.Types
import Unit.Stats (effectiveStat, maxStaminaOf)
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

-- | The unowned durable marker the SD-20 engagement trigger keeps in
--   @uiStats@ (#2834): absent = never engaged, 1 = pending, 2 = applied.
--   It is not a registry key (no system owns it); it is an INPUT of the
--   mode resolver, so only the combat commit (absent → 1) and
--   @unit.applyCombatEngagement@ (1 → 2) may write it.
combatEngagedKey ∷ Text
combatEngagedKey = "combat_engaged"

-- | The marker's two valid values, read from a stat map.
engagementPending, engagementApplied ∷ HM.HashMap Text Float → Bool
engagementPending stats = HM.lookup combatEngagedKey stats ≡ Just 1
engagementApplied stats = HM.lookup combatEngagedKey stats ≡ Just 2

-- | Does the definition declare an engagement trigger for any system?
declaresEngagement ∷ UnitPhysiology → Bool
declaresEngagement = not ∘ HM.null ∘ upEngagement

-- | The one pure mode resolver (requirement 3): capability failure
--   yields 'ModeUnsupported' whatever else is supplied; otherwise the
--   explicit unit override, then — only once the unit's engagement has
--   been APPLIED (@combat_engaged == 2@, the @applied@ argument) — the
--   engagement default the definition declared, then the species
--   default, then Nominal. A pending marker is not yet an engagement, so
--   a pending unit still resolves its species default and the storage
--   invariant (no Simulated mode without its seeded pool) holds.
--
--   The override is a @Maybe@ so that "no override" and "an override
--   equal to the default" are different inputs, which is what intent
--   means (D-3).
resolveMode ∷ PhysiologySystem → UnitPhysiology → Bool → Maybe PhysioMode
            → ResolvedMode
resolveMode sys up applied mOverride
    | not (all (`Set.member` unitCapabilities up) (psRequires sys)) =
        ModeUnsupported
    | otherwise = case chosen of
        PhysioSimulated → ModeSimulated
        PhysioNominal   → ModeNominal
  where
    chosen = case mOverride of
        Just m  → m
        Nothing
            | applied, Just m ← HM.lookup (psId sys) (upEngagement up) → m
            | otherwise →
                fromMaybe PhysioNominal (HM.lookup (psId sys) (upDefaults up))

-- | The one system an engagement declaration may name in v1 (SD-20): the
--   verb that applies it is stamina's.
engagementSystem ∷ Text
engagementSystem = "stamina"

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
    → [(Text, Text)]       -- ^ the block's @on_combat_engagement:@ entries
    → Either (Text, Text) UnitPhysiology
resolveUnitPhysiology reg unit bodyAuthored mProfile entries engagement = do
    profileModes ← case mProfile of
        Nothing → Right []
        Just p  → case registryProfile reg p of
            Nothing → Left ("unknown physiology profile", unit <> ": " <> p)
            Just pr → Right (Map.toList (ppModes pr))
    explicit ← traverse one entries
    let defaults = HM.fromList (profileModes ⧺ explicit)
    onEngage ← traverse (oneEngagement defaults) engagement
    pure UnitPhysiology
        { upBodyAuthored = bodyAuthored
        , upProfile      = mProfile
        , upDefaults     = defaults
        , upEngagement   = HM.fromList onEngage }
  where
    -- An engagement entry (SD-20): the named system must exist, be one
    -- the engagement verb can transition (stamina in v1), be Nominal by
    -- default (the trigger moves Nominal → Simulated, one way), and take
    -- the mode "simulated".
    oneEngagement defaults (sid, modeText) = case registrySystem reg sid of
        Nothing → Left ( "unknown physiology system"
                       , unit <> ": on_combat_engagement: " <> sid )
        Just _
            | sid ≢ engagementSystem → Left
                ( "unsupported physiology engagement system"
                , unit <> ": on_combat_engagement: " <> sid )
            | parsePhysioMode modeText ≢ Just PhysioSimulated → Left
                ( "invalid physiology engagement mode"
                , unit <> ": on_combat_engagement: " <> sid <> ": " <> modeText )
            | HM.lookup sid defaults ≡ Just PhysioSimulated → Left
                ( "physiology engagement system is not Nominal by default"
                , unit <> ": on_combat_engagement: " <> sid )
            | otherwise → Right (sid, PhysioSimulated)
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
--   Unowned names, lazy templates included, are untouched, except the
--   engagement marker (#2834): its two atomic writers are the only way
--   it may come to exist, so a template could create a value 2 with no
--   seeded pool behind it.
rejectPhysiologyTemplates ∷ PhysiologyRegistry → Text → [Text] → [Text]
                          → Maybe (Text, Text)
rejectPhysiologyTemplates reg unit statKeys skillKeys = listToMaybe
    [ ("physiology key in unit template", unit <> ": " <> section <> "." <> k)
    | (section, ks) ← [("stats", statKeys), ("skills", skillKeys)]
    , k ← ks
    , k `Set.member` owned ∨ k ≡ combatEngagedKey ]
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
    , pcBlood      ∷ !BloodView
      -- ^ the unit's blood through "Unit.Blood": the one input the derived
      --   @circulation@ reads that is not a stat (#2836, S9).
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
    resolveMode sys (pcPhysiology ctx) (engagementApplied (pcStats ctx))
                (HM.lookup (psId sys) (pcOverrides ctx))

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
        ModeSimulated → derivedRule ctx d
        _ → case dkHealthy d of
            DerivedFixed v → Right v
            -- A live key is its rule, whatever the mode (SD-21): the rule
            -- reads its own system's inputs through THEIR mode.
            DerivedLive    → derivedRule ctx d

-- | Evaluate a derived key's named rule against the unit's current state.
derivedRule ∷ PhysioContext → DerivedKey → Either Text Float
derivedRule ctx d = case dkRule d of
    DerivedSaltConcentration → saltConcentrationFor ctx
    DerivedCirculation       → circulationFor ctx

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
checkedInput = checkedWith "positive" (≤ 0)

-- | A stored POOL a rule reads as a numerator (the body-water pool behind
--   @salt_conc@): present, finite and NON-NEGATIVE as stored and as
--   effective. A present zero is a valid reading — a dehydrated unit's
--   empty pool, which the concentration rule's @clamp 0.05 1@ exists to
--   handle (S4, #2831: activation exposed it, because the pool really does
--   reach zero) — while a negative, NaN or infinite pool, or an absent
--   one, is still an invariant violation, and a positive modifier still
--   cannot lift an invalid STORED pool into a plausible effective one.
checkedPool ∷ Text → PhysioContext → Text → Either Text Float
checkedPool = checkedWith "non-negative" (< 0)

checkedWith ∷ Text → (Float → Bool) → Text → PhysioContext → Text
            → Either Text Float
checkedWith wanted invalid missing ctx k = do
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
        | invalid x = Left $
            "input '" <> k <> "' is not " <> wanted <> " (" <> which <> " "
            <> tshow x <> ")"
        | otherwise = Right ()

-- | A healthy rule's input.
capacity ∷ PhysioContext → Text → Either Text Float
capacity ctx k = checkedInput ("input '" <> k <> "' is absent") ctx k

-- | A Simulated pool a DERIVED rule reads (the body-water pool behind
--   @salt_conc@). Unlike a plain Simulated owned read, which returns
--   whatever is stored, a derived value built on a nonsensical pool would
--   be a wrong answer with no way to tell: see 'checkedPool' for what
--   counts as nonsensical.
dependencyPool ∷ PhysioContext → Text → Either Text Float
dependencyPool ctx k =
    checkedPool ("Simulated owned key '" <> k <> "' has no stored value")
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
evalHealthy ctx HealthyMaxStamina =
    case maxStaminaOf Nothing (pcNow ctx) (pcStats ctx) (pcModifiers ctx) of
        Nothing → Left "input 'max_stamina' is absent (no endurance stat)"
        Just v
            | isNaN v ∨ isInfinite v → Left $
                "input 'max_stamina' is not finite (" <> tshow v <> ")"
            | v ≤ 0 → Left $
                "input 'max_stamina' is not positive (" <> tshow v <> ")"
            | otherwise → Right v

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

-- | @circulation@ (#2836, S9): the shared pure rule over the unit's current
--   state, never stored. Inputs, each read at the context's game time:
--
--     * blood: the fraction "Unit.Blood" computed for the context;
--     * the effective @constitution@, @fat_mass@, @body_mass@ and @sepsis@,
--       with the fallbacks the Lua rule had (1.0, 0, 70, 0): reading a
--       derived value must not roll a stat, so a unit with none of them
--       takes the fallback instead of a lazy roll;
--     * the core temperature through its own read, so a Nominal or
--       Unsupported thermo substitutes the baseline and a Simulated one with
--       no stored core is an invariant violation;
--     * the heart rate: the effective stored value while cardio is
--       Simulated (an absent one is an invariant violation), and none under
--       a Nominal cardio, whose heart-rate factor the rule makes exactly 1.0.
circulationFor ∷ PhysioContext → Either Text Float
circulationFor ctx = do
    resting ← constant "RESTING_HR"
    core    ← coreTemperature
    heartRate ← case classifyKey reg "heart_rate" of
        Just (csys, RoleOwned h) | modeOf ctx csys ≡ ModeSimulated →
            Just <$> simulatedOwned ReadEffective ctx (okKey h)
        Just _  → Right Nothing
        Nothing → Left "the registry does not own 'heart_rate'"
    pure $ circulationRule resting CirculationInputs
        { ciBloodFraction = bvFraction (pcBlood ctx)
        , ciConstitution  = stat "constitution" 1.0
        , ciFatMass       = stat "fat_mass" 0.0
        , ciBodyMass      = stat "body_mass" 70.0
        , ciSepsis        = stat "sepsis" 0.0
        , ciCore          = core
        , ciHeartRate     = heartRate }
  where
    reg = pcRegistry ctx
    stat k fallback = fromMaybe fallback (storedValue ReadEffective ctx k)
    constant n = maybe (Left ("unknown constant " <> n)) Right
                       (Map.lookup n (prConstants reg))
    coreTemperature = case physioReadPure ReadEffective ctx "core_temp" of
        OutcomeValue _ v     → Right v
        OutcomeUnsupported   → constant "THERMO_BASELINE"
        OutcomeInvariant why → Left why
        OutcomeUnowned       → Left "the registry does not own 'core_temp'"

-- * Consumer boundary (S5, #2830)

-- | A unit's stats AS A PURE GAMEPLAY CONSUMER SEES THEM, for the named
--   registry keys: the Haskell face of @physiology.get@, used by code that
--   already holds the unit instance inside a single atomic fold (the wound
--   tick) and so cannot run the IO 'physioRead'.
--
--   Each named key is replaced by its mode-aware EFFECTIVE value (a
--   Nominal pool reads its healthy rule), and a key whose system is
--   Unsupported is removed so the consumer's absent-pool branch applies. @capacities@ are the
--   unowned maxima those pools are read against: active, each present
--   one is replaced by its EFFECTIVE value so a pool and its capacity
--   agree on modifiers.
--
--   A violated invariant (a Simulated pool with no stored value, a
--   healthy-rule input that is absent or not positive) is a 'Left' with
--   the description, never a stored fallback: D-2 forbids reading it as
--   healthy or absent, and the caller decides what an invariant violation
--   means (the wound tick leaves the unit untouched and logs it).
physioConsumerView ∷ [Text] → [Text] → PhysioContext
                   → Either Text (HM.HashMap Text Float)
physioConsumerView keys capacities ctx = do
    owned ← foldM step (pcStats ctx) keys
    pure (foldl' capacity owned capacities)
  where
    step acc k = case physioReadPure ReadEffective ctx k of
        OutcomeValue _ v     → Right (HM.insert k v acc)
        OutcomeUnsupported   → Right (HM.delete k acc)
        OutcomeUnowned       → Right acc
        OutcomeInvariant why → Left why
    -- A capacity (@max_calories@, @max_hydration@) is an UNOWNED input,
    -- but a pool's effective value is read against ITS effective
    -- capacity: the Lua facade divides by @get(maxKey)@, which applies
    -- the key's modifiers, so the view must too.
    capacity acc k = case storedValue ReadEffective ctx k of
        Just v  → HM.insert k v acc
        Nothing → acc

-- * Raw write gate (S4, #2831)

-- | May the raw stat verb (@unit.setStat@) write @key@ for this unit?
--   An unowned key always may, except the engagement marker (#2834): it
--   is an input of the stamina resolver, so a raw 2 would make stamina
--   Simulated with no seeded pool. A DERIVED key never may, in either
--   mode (it is computed on demand and never stored). An owned key or
--   meter may only while its system is Simulated: under Nominal the key
--   does not exist, and under Unsupported it never does. A Simulated
--   system's debug writes stay allowed (D-7). @applied@ is whether the
--   unit's engagement marker is 2.
rawWriteAllowed ∷ PhysiologyRegistry → UnitPhysiology → Bool → PhysioOverrides
                → Text → Bool
rawWriteAllowed reg up applied overrides key
    | key ≡ combatEngagedKey = False
    | otherwise = case classifyKey reg key of
        Nothing → True
        Just (sys, role) → case role of
            RoleDerived _ → False
            _ → resolveMode sys up applied (HM.lookup (psId sys) overrides)
                    ≡ ModeSimulated
