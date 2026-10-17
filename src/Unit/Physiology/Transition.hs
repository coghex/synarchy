{-# LANGUAGE Strict #-}
-- | Physiology mode TRANSITIONS and stored-key reconciliation (#2831, S4
--   of the physiology stat-modes epic #2827; design D-3, D-5, D-6).
--
--   Everything here is pure: it takes a unit's 'PhysioContext' and returns
--   the stats and overrides the unit should hold afterwards, or the reason
--   no such state exists. The engine verbs apply the result inside ONE
--   unit-manager update, so a transition is observable whole or not at
--   all (D-5), and the load boundary applies the same rule to a decoded
--   unit before anything is published (D-6).
--
--   __One seeding rule.__ 'seedTarget' is the EFFECTIVE value a key starts
--   at: its healthy rule, except @salt@, which starts at its healthy
--   maximum times the unit's clamped body-water fraction so the derived
--   concentration reads exactly 1.0 (SD-9). The value that is STORED is
--   the BASE that 'effectiveStat' maps to that target under the unit's
--   active modifiers ('invertEffectiveStat'); when no base exists the
--   whole transition is refused and nothing changes (T-6). Spawn, a
--   Nominal→Simulated transition and the load-time repair of a missing
--   Simulated key all call it, so they cannot drift apart.
--
--   __What a transition changes.__ Entering Simulated seeds every owned
--   key and zeroes every meter of the system. Leaving it deletes the
--   owned keys, the derived keys and the meters (a Nominal system holds
--   none, D-1). Staying in the same mode changes no key, except that
--   explicit @values@ to a Simulated system overwrite the named keys. The
--   override is recorded on every set, even one equal to the species
--   default, and removed on every clear (D-3).
module Unit.Physiology.Transition
    ( -- * Transitions
      Transition(..)
    , transitionChangesMode
    , planSetMode
    , planClearMode
      -- * Field-backed keys (blood, #2837)
    , canonicalBlood
      -- * Combat engagement (SD-20)
    , EngagementOutcome(..)
    , EngagementRefusal(..)
    , EngagementPlan(..)
    , planEngagement
    , engagementOutcomeText
      -- * Seeding and scrubbing
    , seedTarget
    , seedOrder
    , systemKeyNames
      -- * Load reconciliation
    , KeyRepair(..)
    , reconcileStoredKeys
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Monad (foldM)
import Data.List (partition)
import Unit.Blood (BloodView(..))
import Unit.Physiology
import Unit.Physiology.Types
import Unit.Stats (invertEffectiveStat, invertEffectiveStatWhy)

-- | The state a transition leaves a unit in.
data Transition = Transition
    { trStats     ∷ !(HM.HashMap Text Float)
    , trBlood     ∷ !(Maybe Float)
      -- ^ the new value of the dedicated @uiBlood@ field when the transition
      --   writes it (blood, #2837), 'Nothing' when it leaves it alone. Never a
      --   uiStats entry: 'trStats' holds no field-backed key.
    , trOverrides ∷ !PhysioOverrides
    , trFrom      ∷ !ResolvedMode
    , trTo        ∷ !ResolvedMode
    } deriving (Show, Eq)

-- | Did the EFFECTIVE mode change? Seeds, scrubs and the change hooks
--   run only then (D-5): an override recorded equal to the default is
--   intent, not a transition.
transitionChangesMode ∷ Transition → Bool
transitionChangesMode t = trFrom t ≢ trTo t

-- | The owned keys stored in a dedicated unit field instead of @uiStats@.
fieldKeys ∷ PhysiologySystem → [Text]
fieldKeys sys = [ okKey o | o ← psOwns sys, isJust (okField o) ]

-- | Every field-backed key of the registry.
registryFieldKeys ∷ PhysiologyRegistry → [Text]
registryFieldKeys = concatMap fieldKeys ∘ prSystems

-- | The stats a plan works on: the unit's stats with every field-backed key
--   MATERIALIZED from its field, so the generic seed, scrub and value paths
--   run unchanged. The overlay never leaves this module: 'splitFields'
--   removes it from every result, so no uiStats entry for a field-backed key
--   can come out of a plan.
overlayFields ∷ PhysioContext → HM.HashMap Text Float → HM.HashMap Text Float
overlayFields ctx stats = foldl' (\m k → HM.insert k (bvVolume (pcBlood ctx)) m)
    stats (registryFieldKeys (pcRegistry ctx))

splitFields ∷ PhysiologyRegistry → HM.HashMap Text Float → HM.HashMap Text Float
splitFields reg stats = foldl' (flip HM.delete) stats (registryFieldKeys reg)

-- | The canonical stored blood of a Nominal blood: the current maximum (base
--   body mass x 0.075). 'Nothing' for any other mode. Spawn and the load gate
--   write it, because a Nominal @uiBlood@ is serialized but ignored (#2837).
canonicalBlood ∷ PhysioContext → Maybe Float
canonicalBlood ctx = do
    sys ← bloodSystem (pcRegistry ctx)
    guard (modeOf ctx sys ≡ ModeNominal)
    o ← listToMaybe [ o | o ← psOwns sys, isJust (okField o) ]
    either (const Nothing) Just (evalHealthy ctx (okHealthy o))

-- | Every stored name a system owns: owned keys, derived keys, meters.
systemKeyNames ∷ PhysiologySystem → [Text]
systemKeyNames sys =
    map okKey (psOwns sys) ⧺ map dkKey (psDerived sys) ⧺ psMeters sys

-- | The order seeds are computed in: the system that PROVIDES the
--   body-water fraction first, because salt's starting value reads it
--   (so a pass that seeds both never computes salt against an absent
--   hydration). Otherwise registry order, which is tick order.
seedOrder ∷ PhysiologyRegistry → [PhysiologySystem]
seedOrder reg = providers ⧺ others
  where
    (providers, others) = partition provides (prSystems reg)
    provides s = any ((≡ "body_water_fraction") ∘ ieName) (psProvides s)

-- * Seeding

-- | The unit's body-water fraction for seeding salt: its EFFECTIVE
--   hydration over its effective maximum when hydration is Simulated,
--   1.0 otherwise (a Nominal pool is full by definition). Clamped like
--   the concentration rule, so a near-empty pool cannot seed salt at 0.
bodyWaterFraction ∷ PhysioContext → Either Text Float
bodyWaterFraction ctx = case classifyKey (pcRegistry ctx) "hydration" of
    Just (hsys, RoleOwned h) | modeOf ctx hsys ≡ ModeSimulated → do
        maxH ← evalHealthy ctx (okHealthy h)
        cur  ← checkedPool
                   "Simulated owned key 'hydration' has no stored value"
                   ctx "hydration"
        pure (clamp 0.05 1.0 (cur / maxH))
    _ → Right 1.0

-- | The EFFECTIVE value a key is seeded to (see the module header).
seedTarget ∷ PhysioContext → OwnedKey → Either Text Float
seedTarget ctx o = do
    healthy ← evalHealthy ctx (okHealthy o)
    if okKey o ≡ "salt" then (healthy *) <$> bodyWaterFraction ctx
                        else pure healthy

-- | The largest value an owned key may be given: its healthy value for
--   a key sized by an input (@hydration@ ≤ @max_hydration@), none for a
--   constant (@core_temp@ has no maximum).
keyCap ∷ PhysioContext → OwnedKey → Either Text (Maybe Float)
keyCap ctx o = case okHealthy o of
    HealthyConst _  → Right Nothing
    HealthyBloodMax → Just <$> evalHealthy ctx (okHealthy o)
    HealthyFrom _ _ → Just <$> evalHealthy ctx (okHealthy o)
    HealthyMaxStamina → Just <$> evalHealthy ctx (okHealthy o)

-- | The stored base that makes @key@'s effective value @target@.
storeAs ∷ PhysioContext → Text → Float → Either Text Float
storeAs ctx key target =
    case invertEffectiveStat (pcNow ctx) target
             (HM.lookupDefault [] key (pcModifiers ctx)) of
        Just base → Right base
        Nothing   → Left $
            "no base for '" <> key <> "' makes its effective value "
            <> tshow target <> " under its active modifiers (the percent \
            \multiplier is not positive, or the base would be negative \
            \or not finite)"

-- | Seed @sys@'s owned keys into @stats@. @explicit@ names the keys the
--   caller chose a value for; the rest take 'seedTarget'. With
--   @missingOnly@ a key already stored is left EXACTLY as it is (load
--   repair of a partially present system, D-6 (b)).
seedSystem ∷ PhysioContext → PhysiologySystem → [(Text, Float)] → Bool
           → HM.HashMap Text Float → Either Text (HM.HashMap Text Float)
seedSystem ctx sys explicit missingOnly = flip (foldM step) (psOwns sys)
  where
    step stats o
        | missingOnly ∧ (isJust (okField o) ∨ HM.member (okKey o) stats) = Right stats
        | otherwise = do
            let ctx' = ctx { pcStats = stats }
            target ← case lookup (okKey o) explicit of
                Just v  → Right v
                Nothing → seedTarget ctx' o
            base ← storeAs ctx' (okKey o) target
            pure (HM.insert (okKey o) base stats)

-- | Remove every key the system holds.
scrubSystem ∷ PhysiologySystem → HM.HashMap Text Float
            → HM.HashMap Text Float
scrubSystem sys stats = foldl' (flip HM.delete) stats (systemKeyNames sys)

-- | Validate caller-supplied EFFECTIVE targets: owned keys of the
--   system, finite, non-negative and not above the key's maximum.
validateValues ∷ PhysioContext → PhysiologySystem → [(Text, Float)]
               → Either Text ()
validateValues ctx sys = mapM_ one
  where
    one (k, v) = do
        o ← maybe (Left ("'" <> k <> "' is not an owned key of system '"
                         <> psId sys <> "'")) Right
                  (lookupOwned k)
        when (isNaN v ∨ isInfinite v) $
            Left ("value for '" <> k <> "' is not finite")
        when (v < 0) $ Left ("value for '" <> k <> "' is negative")
        cap ← keyCap ctx o
        case cap of
            Just c | v > c → Left $
                "value for '" <> k <> "' (" <> tshow v
                <> ") exceeds its maximum (" <> tshow c <> ")"
            _ → Right ()
    lookupOwned k = listToMaybe [ o | o ← psOwns sys, okKey o ≡ k ]

-- * Transitions

-- | @setPhysioMode@: record the override and, if the effective mode
--   changes, seed or scrub the system. 'Left' leaves everything alone.
planSetMode ∷ PhysioContext → Text → PhysioMode → Maybe [(Text, Float)]
            → Either Text Transition
planSetMode ctx sid mode mValues = do
    sys ← maybe (Left ("unknown physiology system '" <> sid <> "'")) Right
              (registrySystem (pcRegistry ctx) sid)
    let before = modeOf ctx sys
    when (before ≡ ModeUnsupported) $ Left $
        "system '" <> sid <> "' is Unsupported for this unit's definition"
    let overrides' = HM.insert sid mode (pcOverrides ctx)
        ctx'       = ctx { pcOverrides = overrides' }
        after      = modeOf ctx' sys
    (stats', blood) ← moveWithFields ctx' sys before after mValues
    pure Transition { trStats = stats', trBlood = blood
                    , trOverrides = overrides'
                    , trFrom = before, trTo = after }

-- | @clearPhysioMode@: drop the override (the unit follows its
--   definition again) and seed or scrub if that changes the effective
--   mode. Clearing a unit with no override changes nothing.
planClearMode ∷ PhysioContext → Text → Either Text Transition
planClearMode ctx sid = do
    sys ← maybe (Left ("unknown physiology system '" <> sid <> "'")) Right
              (registrySystem (pcRegistry ctx) sid)
    let before = modeOf ctx sys
    when (before ≡ ModeUnsupported) $ Left $
        "system '" <> sid <> "' is Unsupported for this unit's definition"
    let overrides' = HM.delete sid (pcOverrides ctx)
        ctx'       = ctx { pcOverrides = overrides' }
        after      = modeOf ctx' sys
    (stats', blood) ← moveWithFields ctx' sys before after Nothing
    pure Transition { trStats = stats', trBlood = blood
                    , trOverrides = overrides'
                    , trFrom = before, trTo = after }

-- | 'moveStats' over the OVERLAID stats, split back into the unit's stats and
--   the new value of the system's dedicated field (blood, #2837): seeded on
--   entering Simulated, the canonical maximum on leaving it, a caller value on
--   a Simulated write, and untouched otherwise.
moveWithFields ∷ PhysioContext → PhysiologySystem → ResolvedMode → ResolvedMode
               → Maybe [(Text, Float)]
               → Either Text (HM.HashMap Text Float, Maybe Float)
moveWithFields ctx sys before after mValues = do
    moved ← moveStats ctx { pcStats = overlayFields ctx (pcStats ctx) }
                      sys before after mValues
    let stats' = splitFields (pcRegistry ctx) moved
    blood ← case (fieldKeys sys, before, after) of
        ([], _, _) → Right Nothing
        (k : _, ModeNominal, ModeSimulated) → Right (HM.lookup k moved)
        (k : _, ModeSimulated, ModeSimulated)
            | Just v ← mValues ⌦ lookup k → Right (Just v)
        (k : _, ModeSimulated, ModeNominal) →
            Just <$> maybe (Left ("no healthy rule for '" <> k <> "'")) (evalHealthy ctx ∘ okHealthy)
                           (listToMaybe [ o | o ← psOwns sys, okKey o ≡ k ])
        _ → Right Nothing
    pure (stats', blood)

-- | The stats after moving @sys@ from @before@ to @after@ (the context
--   already carries the NEW overrides).
moveStats ∷ PhysioContext → PhysiologySystem → ResolvedMode → ResolvedMode
          → Maybe [(Text, Float)] → Either Text (HM.HashMap Text Float)
moveStats ctx sys before after mValues = case (before, after) of
    (ModeNominal, ModeSimulated) → do
        mapM_ (validateValues ctx sys) mValues
        seeded ← seedSystem ctx sys (fromMaybe [] mValues) False
                            (pcStats ctx)
        pure (foldl' (\m k → HM.insert k 0 m) seeded (psMeters sys))
    (ModeSimulated, ModeNominal) → do
        refuseValues
        pure (scrubSystem sys (pcStats ctx))
    (ModeSimulated, ModeSimulated) → case mValues of
        Nothing → Right (pcStats ctx)
        Just vs → do
            validateValues ctx sys vs
            -- Only the named keys: every other key keeps its value.
            foldM (writeOne vs) (pcStats ctx) (psOwns sys)
    _ → do
        refuseValues
        pure (pcStats ctx)
  where
    refuseValues = unless (isNothing mValues) $ Left
        "values apply only to a Simulated system"
    writeOne vs stats o = case lookup (okKey o) vs of
        Nothing → Right stats
        Just v  → do
            base ← storeAs (ctx { pcStats = stats }) (okKey o) v
            pure (HM.insert (okKey o) base stats)

-- * Combat engagement (SD-20, #2834)

-- | What @unit.applyCombatEngagement@ did for one unit.
data EngagementOutcome
    = EngagementTransitioned
      -- ^ the system was Nominal: the full effective seed, the pinned
      --   Simulated override and marker 2, published together
    | EngagementLatched
      -- ^ an explicit override of either mode already decided the mode:
      --   marker 2 only (SD-7), no seed, no override change, no hooks
    | EngagementRefusedDead
      -- ^ a dead unit stays pending and is never revisited
    | EngagementSeedUnrepresentable !Text
      -- ^ D-5's guards refused the seed (the reason code names which):
      --   nothing was written, the marker stays pending
    deriving (Show, Eq)

-- | The Lua spelling of an outcome.
engagementOutcomeText ∷ EngagementOutcome → Text
engagementOutcomeText EngagementTransitioned = "transitioned"
engagementOutcomeText EngagementLatched = "latched"
engagementOutcomeText EngagementRefusedDead = "refusedDead"
engagementOutcomeText (EngagementSeedUnrepresentable _) = "seedUnrepresentable"

-- | Why a plan was refused.
data EngagementRefusal
    = EngagementNotPending
      -- ^ nothing to apply: the species does not declare the trigger or
      --   the marker is not pending
    | EngagementSeedRefused !Text
      -- ^ D-5's guards refused the seed; the code names which
    deriving (Show, Eq)

-- | The state a successful application leaves.
data EngagementPlan = EngagementPlan
    { epStats     ∷ !(HM.HashMap Text Float)
    , epOverrides ∷ !PhysioOverrides
    , epOutcome   ∷ !EngagementOutcome
    } deriving (Show, Eq)

-- | Apply a PENDING engagement. 'Left' is either "nothing to apply" (the
--   species does not declare the trigger, or the marker is not pending)
--   or the seed refusal; a refusal changes nothing. Dead units are the
--   caller's: it asks the combat lethality rule, which this pure plan
--   cannot see.
--
--   With no explicit override the application IS the standard one-way
--   Nominal → Simulated transition ('planSetMode' with no values), so the
--   seed, the pinned override and D-5's guards (@m ≤ 0@, a negative or
--   non-finite base) are exactly a scripted @setMode@'s; the marker goes
--   to 2 in the same result.
planEngagement ∷ PhysioContext → Either EngagementRefusal EngagementPlan
planEngagement ctx
    | not (declaresEngagement (pcPhysiology ctx))
      ∨ not (engagementPending (pcStats ctx)) = Left EngagementNotPending
    | HM.member engagementSystem (pcOverrides ctx) = Right EngagementPlan
        { epStats = applied (pcStats ctx)
        , epOverrides = pcOverrides ctx
        , epOutcome = EngagementLatched }
    | otherwise = case planSetMode ctx engagementSystem PhysioSimulated Nothing of
        Left _ → Left (EngagementSeedRefused (seedRefusalCode ctx))
        Right t → Right EngagementPlan
            { epStats = applied (trStats t)
            , epOverrides = trOverrides t
            , epOutcome = EngagementTransitioned }
  where
    applied = HM.insert combatEngagedKey 2.0

-- | Which D-5 guard refused the engagement seed, as a stable code: the
--   diagnostic debounces on it.
seedRefusalCode ∷ PhysioContext → Text
seedRefusalCode ctx = case registrySystem (pcRegistry ctx) engagementSystem of
    Nothing → "unknown_system"
    Just sys → case psOwns sys of
        [] → "no_owned_key"
        (o : _) → case seedTarget ctx o of
            Left _ → "invalid_target"
            Right t → either id (const "seed_failed") $
                invertEffectiveStatWhy (pcNow ctx) t
                    (HM.lookupDefault [] (okKey o) (pcModifiers ctx))

-- * Load reconciliation

-- | One stored-key repair the load boundary made, for the single log line
--   per unit and system (D-6).
data KeyRepair
    = KeysSeeded !Text ![Text]
      -- ^ a Simulated system's missing owned keys, filled by the
      --   seeding rule.
    | KeysScrubbed !Text ![Text]
      -- ^ keys a Nominal or Unsupported system must not hold.
    | DerivedScrubbed !Text ![Text]
      -- ^ a stored derived key (@salt_conc@), removed in EVERY mode:
      --   it is computed on demand and never stored (case (i)).
    deriving (Show, Eq)

-- | Make a unit's stored keys agree with its resolved modes (D-6 (b) and
--   (i)): scrub what a Nominal or Unsupported system must not hold and
--   every stored derived key, and seed only the MISSING owned keys of a
--   Simulated system, leaving present values exactly as they are.
--   Hydration is seeded before salt. 'Left' is an invalid required seed
--   input (case (g)): the system, key and reason.
reconcileStoredKeys ∷ PhysioContext
                    → Either (Text, Text, Text)
                             (HM.HashMap Text Float, [KeyRepair])
reconcileStoredKeys ctx0 = foldM step (pcStats ctx0, []) (seedOrder reg)
  where
    reg = pcRegistry ctx0
    step (stats, repairs) sys = do
        let ctx  = ctx0 { pcStats = stats }
            mode = modeOf ctx sys
            present ks = [ k | k ← ks, HM.member k stats ]
            derivedKeys = map dkKey (psDerived sys)
            -- a field-backed key never has a uiStats entry, in any mode: a
            -- stored one would be the shadow pool the adapter forbids
            stale = case mode of
                ModeSimulated → present (derivedKeys ⧺ fieldKeys sys)
                _             → present (systemKeyNames sys)
            derivedStale = [ k | k ← stale, k `elem` derivedKeys ]
            ownedStale   = [ k | k ← stale, k `notElem` derivedKeys ]
            scrubbed = foldl' (flip HM.delete) stats stale
            note = [ DerivedScrubbed (psId sys) derivedStale
                   | not (null derivedStale) ]
                ⧺ [ KeysScrubbed (psId sys) ownedStale
                  | not (null ownedStale) ]
        case mode of
            ModeSimulated → do
                let missing = [ okKey o | o ← psOwns sys
                                        , isNothing (okField o)
                                        , not (HM.member (okKey o) scrubbed) ]
                if null missing
                    then pure (scrubbed, repairs ⧺ note)
                    else case seedSystem (ctx0 { pcStats = scrubbed }) sys []
                                         True scrubbed of
                        Left why → Left (psId sys, T.intercalate "," missing, why)
                        Right seeded → pure
                            ( seeded
                            , repairs ⧺ note ⧺ [KeysSeeded (psId sys) missing] )
            _ → pure (scrubbed, repairs ⧺ note)
