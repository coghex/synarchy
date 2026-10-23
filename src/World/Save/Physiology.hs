{-# LANGUAGE Strict #-}
-- | The physiology half of the load gate: rejection of invalid saved
--   physiology state (#2829, S3 of the physiology stat-modes epic #2827;
--   design D-6 cases (d) and (h)) and, since S4 (#2831), the deterministic
--   normalization of the rest ('normalizeSavedPhysiology', cases (b), (c),
--   (f), (g), (i), (j)). 'physiologyLoadGate' runs both, in that order.
--
--   __Where this runs, and why not in the component.__ The @units@
--   component's decode and @csValidate@ see only the decoded DTO, and
--   whether a system id or a Simulated key is meaningful depends on the
--   LOADED registry and the unit's loaded definition, which a component
--   migration must never read (content-dependent normalization is the
--   load gate's, and a migration that consulted live content would make an old save
--   decode differently per build). So the content-dependent half lives at
--   the content-aware load boundary, beside the missing-definition checks
--   in "World.Save.Types", which rejects the whole load BEFORE anything is
--   staged: the old live session is never touched. The content-independent
--   half needs no code here: an unknown 'Unit.Physiology.Types.PhysioMode'
--   tag (case (e)) fails cereal's generic sum decode, which the component
--   machinery reports as a malformed payload.
--
--   __Recognition, not migration.__ An override naming a system the
--   registry declares @retired:@ or @renamed_from:@ is neither rejected
--   (that is case (d)'s undeclared system) by 'physiologyLoadRejections':
--   its declared migration, case (c), is 'normalizeSavedPhysiology''s.
--
--   __What (h) does and does not look at.__ A non-finite stored value of
--   a SIMULATED system's durable state — its owned keys and its meters —
--   is malformed. A derived key (@salt_conc@) is never examined: a legacy
--   stored one is case (i), scrubbed in any mode, and rejecting it
--   here would reject saves that scrub is meant to repair. Finite
--   out-of-range values are retained exactly, and no unrelated stat is
--   validated.
module World.Save.Physiology
    ( PhysiologyLoadRejection(..)
    , physiologyLoadRejections
    , renderPhysiologyRejection
      -- * Deterministic normalization (S4, #2831)
    , PhysiologyNormalization(..)
    , PhysiologyLoadDefinition(..)
    , physiologyLoadDefinition
    , normalizeSavedPhysiology
    , physiologyLoadGate
      -- * The rebuilt caches (S4, #2831)
    , rebuiltCacheStats
    , withoutRebuiltCaches
    ) where

import Combat.Resolution.Common (woundsPain)
import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import Unit.Blood (bloodViewOf)
import Unit.Physiology
    (PhysioContext(..), resolveMode, engagementApplied, combatEngagedKey)
import Unit.Physiology.Transition (KeyRepair(..), canonicalBlood, reconcileStoredKeys)
import Unit.Physiology.Types
import Unit.Thread.Command.Body (recomputeBodyDerivedStats, seedBodyComposition)
import Unit.Types.Def (UnitDef(..))
import Unit.Stats (standardStat)
import Unit.Types (UnitId(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Snapshot (PageSnapshot(..), SessionSnapshot(..))
import World.Save.Types (UnitSnapshot(..), UnitInstanceSnapshot(..))

-- | Why a saved unit's physiology state rejects the load.
data PhysiologyLoadRejection
    = UndeclaredOverrideSystem
        { plrPage   ∷ !WorldPageId
        , plrUnit   ∷ !Word32
        , plrDef    ∷ !Text
        , plrSystem ∷ !Text
        }
      -- ^ an override naming a system the registry neither declares nor
      --   recognizes as retired or renamed: an active gameplay reference
      --   to a missing definition.
    | NonFiniteSimulatedValue
        { plrPage   ∷ !WorldPageId
        , plrUnit   ∷ !Word32
        , plrDef    ∷ !Text
        , plrSystem ∷ !Text
        , plrKey    ∷ !Text
        , plrValue  ∷ !Float
        }
      -- ^ a NaN or infinite stored value of a Simulated system's owned
      --   key or meter: a malformed payload.
    | SimulatedOverrideUnsupported
        { plrPage   ∷ !WorldPageId
        , plrUnit   ∷ !Word32
        , plrDef    ∷ !Text
        , plrSystem ∷ !Text
        }
      -- ^ case (f): a Simulated override for a system the unit's
      --   definition no longer supports. Honouring it would give an
      --   unsupported unit physiology state; dropping it would silently
      --   discard authored intent, so the load is refused.
    | InvalidSeedInput
        { plrPage   ∷ !WorldPageId
        , plrUnit   ∷ !Word32
        , plrDef    ∷ !Text
        , plrSystem ∷ !Text
        , plrKey    ∷ !Text
        , plrReason ∷ !Text
        }
      -- ^ case (g): an input a required seed or the Nominal-nutrition
      --   upkeep reads is zero, non-finite or missing after the declared
      --   legacy body rule and cache rebuild. The load is refused, naming
      --   the unit, the key and the input, before anything is published.
    | MalformedEngagementMarker
        { plrPage   ∷ !WorldPageId
        , plrUnit   ∷ !Word32
        , plrDef    ∷ !Text
        , plrValue  ∷ !Float
        }
      -- ^ case (h) for the SD-20 marker (#2834): a stored
      --   @combat_engaged@ that is not finite or not 1 (pending) or 2
      --   (applied). It is an input of the stamina resolver, so an
      --   out-of-set value cannot be interpreted; the load is refused
      --   before anything is published. A VALID marker on a species that
      --   does not declare the trigger is retained and has no effect.
    | MalformedBodyInputs
        { plrPage  ∷ !WorldPageId
        , plrUnit  ∷ !Word32
        , plrDef   ∷ !Text
        , plrKeys  ∷ ![Text]
        }
      -- ^ a bodied definition has only part of the persisted body. The
      --   recompute requires height, body_mass and lean_mass; partial body
      --   state is not eligible for the legacy wholly-unbodied backfill.
    deriving (Show, Eq)

renderPhysiologyRejection ∷ PhysiologyLoadRejection → Text
renderPhysiologyRejection r = case r of
    UndeclaredOverrideSystem{} →
        who <> " carries a physiology override for system '"
        <> plrSystem r <> "', which the physiology registry does not \
        \declare (nor as retired or renamed): an active gameplay \
        \definition is missing"
    NonFiniteSimulatedValue{} →
        who <> " holds a non-finite value (" <> tshow (plrValue r)
        <> ") for '" <> plrKey r <> "', which belongs to its Simulated \
        \physiology system '" <> plrSystem r <> "': malformed payload"
    SimulatedOverrideUnsupported{} →
        who <> " carries a Simulated override for system '" <> plrSystem r
        <> "', which its definition no longer supports"
    InvalidSeedInput{} →
        who <> " cannot be normalized: system '" <> plrSystem r
        <> "', key '" <> plrKey r <> "': " <> plrReason r
    MalformedEngagementMarker{} →
        who <> " holds an invalid '" <> combatEngagedKey <> "' marker ("
        <> tshow (plrValue r) <> "): only 1 (pending) and 2 (applied) are \
        \values: malformed payload"
    MalformedBodyInputs{} →
        who <> " has a malformed body: missing "
        <> T.intercalate ", " (plrKeys r)
  where
    who = "unit #" <> tshow (plrUnit r) <> " (" <> plrDef r
        <> ") on page '" <> unWorldPageId (plrPage r) <> "'"

-- | Every rejection across the saved pages, in page then unit-id order
--   (so the first message is the one an author finds first). Empty ⇒ the
--   saved physiology state may proceed to staging.
--
--   Pure: the registry and the loaded definitions' 'UnitPhysiology' come
--   from the live content at the load boundary, the pages from the
--   decoded save. A unit whose definition is not registered is skipped —
--   the missing-definition check already rejects it, naming the
--   definition, and a second diagnostic for the same unit would only
--   bury that one.
physiologyLoadRejections
    ∷ PhysiologyRegistry
    → HM.HashMap Text PhysiologyLoadDefinition -- ^ loaded definitions
    → [(WorldPageId, UnitSnapshot)]
    → [PhysiologyLoadRejection]
physiologyLoadRejections registry defs pages = concatMap pageRejections pages
  where
    declared = map psId (prSystems registry)
                 ⧺ prRetired registry
                 ⧺ concatMap psRenamedFrom (prSystems registry)

    pageRejections (pid, us) = concat
        [ unitRejections pid (unUnitId uid) u
        | (uid, u) ← sortOn (unUnitId ∘ fst) (HM.toList (usnInstances us)) ]

    unitRejections pid uid u = case HM.lookup (uisDefName u) defs of
        Nothing → []
        Just def →
          let up = pldPhysiology def
          -- the override map AS THE LOAD WILL SEE IT: a renamed system's
          -- override is remapped to its successor (and a retired one
          -- dropped) before the mode that decides whether a stored value is
          -- examined is resolved, so a recognized alias neither hides a
          -- non-finite value of what is Simulated after the remap nor
          -- rejects a value the remapped Nominal override would scrub
              resolved = fst (remapOverrides registry (uisPhysioOverrides u))
          in
            [ UndeclaredOverrideSystem pid uid (uisDefName u) sid
            | sid ← sortOn id (HM.keys (uisPhysioOverrides u))
            , sid `notElem` declared ]
            ⧺ [ NonFiniteSimulatedValue pid uid (uisDefName u) (psId sys) key v
              | sys ← prSystems registry
              , resolveMode sys up (engagementApplied (uisStats u))
                    (HM.lookup (psId sys) resolved)
                    ≡ ModeSimulated
              , (key, v) ← storedValues sys u
              , isNaN v ∨ isInfinite v ]
            ⧺ [ MalformedEngagementMarker pid uid (uisDefName u) v
              | Just v ← [HM.lookup combatEngagedKey (uisStats u)]
              , isNaN v ∨ isInfinite v ∨ (v ≢ 1 ∧ v ≢ 2) ]

-- | A system's stored values AS THE LOAD READS THEM, storage-aware (#2837): a
--   field-backed owned key (blood) is the dedicated field @uisBlood@, never a
--   @uiStats@ entry, so a malformed field is examined and rejected, and a
--   stale non-finite legacy shadow entry (which the load scrubs) is not.
storedValues ∷ PhysiologySystem → UnitInstanceSnapshot → [(Text, Float)]
storedValues sys u =
    [ (okKey o, v)
    | o ← psOwns sys
    , v ← case okField o of
        Just _  → [uisBlood u]
        Nothing → maybeToList (HM.lookup (okKey o) (uisStats u)) ]
    ⧺ [ (m, v) | m ← psMeters sys, Just v ← [HM.lookup m (uisStats u)] ]

-- * Deterministic normalization (S4, #2831; design D-6)

-- | What normalizing one saved unit changed, for ONE log line.
data PhysiologyNormalization = PhysiologyNormalization
    { pnPage  ∷ !WorldPageId
    , pnUnit  ∷ !Word32
    , pnDef   ∷ !Text
    , pnNotes ∷ ![Text]
    } deriving (Show, Eq)

-- | The loaded definition data the physiology gate needs. Keeping this
--   projection small makes its content dependency explicit: mode resolution
--   needs the physiology profile, while legacy body repair needs the authored
--   body flag and the same templates used by standard spawn.
data PhysiologyLoadDefinition = PhysiologyLoadDefinition
    { pldPhysiology    ∷ !UnitPhysiology
    , pldBodyAuthored  ∷ !Bool
    , pldStatTemplates ∷ !(HM.HashMap Text (Float, Float))
    , pldBodyTemplates ∷ !(HM.HashMap Text (Float, Float))
    } deriving (Show, Eq)

-- | Project a live loaded unit definition into the load gate's inputs.
physiologyLoadDefinition ∷ UnitDef → PhysiologyLoadDefinition
physiologyLoadDefinition def = PhysiologyLoadDefinition
    { pldPhysiology = udPhysiology def
    , pldBodyAuthored = upBodyAuthored (udPhysiology def)
    , pldStatTemplates = udStatTemplates def
    , pldBodyTemplates = udBodyTemplates def
    }

-- | Normalize every saved unit's physiology state, or reject.
--   Deterministic and idempotent: the same save yields the same result
--   every time, a second pass over its own output changes nothing, and a
--   save already in canonical form (this build wrote it) is returned
--   untouched with NO notes.
--
--   Per unit, in this order (design D-6):
--
--     * (c) an override naming a system the registry RENAMED is remapped
--       to its successor (unless that already has an override, which is
--       newer intent and wins); one naming a RETIRED system is dropped.
--       The registry declares retired systems by id only, so there is no
--       key list to dispose of: a retired system's keys are simply not
--       registry-owned any more and stay ordinary stats.
--     * (f) a Simulated override for a system the definition no longer
--       supports rejects.
--     * (#2893) a legacy unbodied unit of a bodied definition gets absent
--       body and seed inputs from its standard species templates; a partial
--       body rejects. The body caches are then rebuilt before any seed reads.
--     * (j) a bodied unit with a height and body composition but no
--       @frame_mass@ gets @frame_mass = 22·height²@, the bulk-1.0 spawn
--       frame, which keeps its legacy viability floors exactly.
--     * (g) a Nominal-nutrition unit whose Nominal-upkeep input
--       @frame_mass@ is zero, non-finite or still missing rejects.
--     * (b) and (i): stored keys are made to agree with the resolved
--       modes ('reconcileStoredKeys'); a seed that cannot be computed
--       rejects as (g).
--
--   The cases the registry data cannot make (d), (e), (h) are
--   'physiologyLoadRejections'\'s and run first.
normalizeSavedPhysiology
    ∷ PhysiologyRegistry
    → HM.HashMap Text PhysiologyLoadDefinition
    → Double                                   -- ^ the save's game time
    → [(WorldPageId, UnitSnapshot)]
    → Either [PhysiologyLoadRejection]
             ([(WorldPageId, UnitSnapshot)], [PhysiologyNormalization])
normalizeSavedPhysiology registry defs now pages
    | null rejections = Right (pages', concat notes)
    | otherwise       = Left rejections
  where
    results =
        [ (pid, us', rs, ns)
        | (pid, us) ← pages
        , let (us', rs, ns) = normalizePage registry defs now pid us ]
    pages'     = [ (pid, us') | (pid, us', _, _) ← results ]
    rejections = concat [ rs | (_, _, rs, _) ← results ]
    notes      = [ ns | (_, _, _, ns) ← results ]

normalizePage ∷ PhysiologyRegistry → HM.HashMap Text PhysiologyLoadDefinition → Double
              → WorldPageId → UnitSnapshot
              → (UnitSnapshot, [PhysiologyLoadRejection], [PhysiologyNormalization])
normalizePage registry defs now pid us = (us { usnInstances = instances' }, rejs, ns)
  where
    perUnit = [ (uid, u, normalizeUnit registry defs now pid (unUnitId uid) u)
              | (uid, u) ← sortOn (unUnitId ∘ fst) (HM.toList (usnInstances us)) ]
    instances' = HM.fromList
        [ (uid, either (const u) (\(u', _) → u') r) | (uid, u, r) ← perUnit ]
    rejs = concat [ rs | (_, _, Left rs) ← perUnit ]
    ns   = [ PhysiologyNormalization pid (unUnitId uid) (uisDefName u) notes
           | (uid, u, Right (_, notes)) ← perUnit, not (null notes) ]

normalizeUnit ∷ PhysiologyRegistry → HM.HashMap Text PhysiologyLoadDefinition → Double
              → WorldPageId → Word32 → UnitInstanceSnapshot
              → Either [PhysiologyLoadRejection] (UnitInstanceSnapshot, [Text])
normalizeUnit registry defs now pid uid u = case HM.lookup (uisDefName u) defs of
    Nothing → Right (u, [])      -- the missing-definition check owns this
    Just def → do
        let up = pldPhysiology def
            (overrides, overrideNotes) = remapOverrides registry (uisPhysioOverrides u)
            unsupportedSim =
                [ SimulatedOverrideUnsupported pid uid (uisDefName u) (psId sys)
                | sys ← prSystems registry
                , Just PhysioSimulated ← [HM.lookup (psId sys) overrides]
                , resolveMode sys up (engagementApplied (uisStats u))
                    (Just PhysioSimulated) ≡ ModeUnsupported ]
        unless' (null unsupportedSim) (Left unsupportedSim)
        (statsA, bodyNotes) ← prepareBody def pid uid u
        let (statsJ, frameNote) = reconstructFrame up statsA
            (statsK, cacheNotes, promotionNotes) = rebuildBodyCaches def statsJ
            ctx = PhysioContext
                { pcRegistry = registry, pcPhysiology = up
                , pcOverrides = overrides, pcNow = now
                , pcStats = statsK, pcModifiers = uisModifiers u
                -- The load gate seeds and scrubs stored keys; nothing it
                -- does reads blood, and the wounds that price a bleed rate
                -- are not needed to repair a stat map.
                , pcBlood = bloodViewOf statsK (uisBlood u) 0
                -- the saved wounds price the pain the mood seed reads (#2838)
                , pcPain = woundsPain (uisWounds u) }
        -- (g): the Nominal-nutrition upkeep's input.
        case upkeepInputProblem registry ctx of
            Just (sid, key, why) → reject sid key why
            Nothing → Right ()
        (statsB, repairs) ← either (\(sid, key, why) → reject sid key why) Right
                                   (reconcileStoredKeys ctx)
        let notes = overrideNotes ⧺ bodyNotes ⧺ frameNote ⧺ cacheNotes
                    ⧺ promotionNotes ⧺ map renderRepair repairs
            -- A Nominal blood's stored uiBlood is SERIALIZED BUT IGNORED
            -- (#2837): reads never trust it, and the load rewrites it to the
            -- current canonical maximum (base body mass x 0.075) silently,
            -- the way the other rebuilt caches are rebuilt. It is not a
            -- logged repair: a unit saved by current code under unchanged
            -- content whose body mass moved afterwards must normalize ZERO
            -- times, and a second load finds the field already canonical.
            canonical = case canonicalBlood ctx of
                Just c | c ≢ uisBlood u → Just c
                _ → Nothing
        if null notes ∧ isNothing canonical ∧ statsB ≡ uisStats u
            then Right (u, [])
            else Right ( u { uisStats = statsB, uisPhysioOverrides = overrides
                           , uisBlood = fromMaybe (uisBlood u) canonical }
                       , notes )
  where
    unless' c e = if c then Right () else e
    reject ∷ Text → Text → Text → Either [PhysiologyLoadRejection] a
    reject sid key why = Left
        [ InvalidSeedInput pid uid (uisDefName u) sid key why ]

-- | A bodied definition's wholly unbodied legacy unit is restored from the
--   same standard template values SpawnStandard uses. Partial body state is
--   malformed and is never completed by this rule.
prepareBody
    ∷ PhysiologyLoadDefinition → WorldPageId → Word32 → UnitInstanceSnapshot
    → Either [PhysiologyLoadRejection] (HM.HashMap Text Float, [Text])
prepareBody def pid uid u
    | not (pldBodyAuthored def) = Right (uisStats u, [])
    | null present = Right (stats', [note | not (null keysAdded)])
    | not (null missingRequired) = Left
        [ MalformedBodyInputs pid uid (uisDefName u) missingRequired ]
    | otherwise = Right (uisStats u, [])
  where
    bodyKeys = ["height", "body_mass", "lean_mass", "fat_mass", "frame_mass"]
    required = ["height", "body_mass", "lean_mass"]
    present = filter (`HM.member` uisStats u) bodyKeys
    missingRequired = filter (not ∘ (`HM.member` uisStats u)) required
    statMeans = HM.map (standardStat ∘ fst) (pldStatTemplates def)
    bodyMeans = HM.map (standardStat ∘ fst) (pldBodyTemplates def)
    seeded = seedBodyComposition (HM.union statMeans bodyMeans)
    bodyBackfill = HM.fromList
        [ (k, v) | k ← bodyKeys, Just v ← [HM.lookup k seeded] ]
    strengthBackfill = case (HM.member "strength_base" (uisStats u)
                           ∨ HM.member "strength" (uisStats u)) of
        True  → HM.empty
        False → maybe HM.empty (HM.singleton "strength_base")
                    (HM.lookup "strength" statMeans)
    enduranceBackfill = maybe HM.empty (HM.singleton "endurance")
        (HM.lookup "endurance" statMeans)
    additions = HM.unions [bodyBackfill, strengthBackfill, enduranceBackfill]
    stats' = HM.union (uisStats u) additions
    keysAdded = sortOn id (HM.keys (HM.difference additions (uisStats u)))
    note = "backfilled " <> T.intercalate "," keysAdded
        <> " from the definition's species averages"

-- | Rebuild all body caches before seed evaluation. A missing cache is a
--   logged repair; a stale present cache is rewritten silently. The real
--   body recomputation remains the single source of cache values.
rebuildBodyCaches
    ∷ PhysiologyLoadDefinition → HM.HashMap Text Float
    → (HM.HashMap Text Float, [Text], [Text])
rebuildBodyCaches def stats
    | not (pldBodyAuthored def)
        ∨ any (not ∘ (`HM.member` stats)) ["height", "body_mass", "lean_mass"] =
            (stats, [], [])
    | otherwise =
        let rebuilt0 = recomputeBodyDerivedStats stats
            -- If no strength input or template exists, the recompute's
            -- default still yields canonical strength caches, but there is
            -- no durable strength_base input to promote or invent.
            noStrengthInput = not (HM.member "strength_base" stats)
                ∧ (not (HM.member "strength" stats)
                   ∨ HM.lookup "strength" stats ≡ Just 0)
                ∧ not (HM.member "strength" (pldStatTemplates def))
            rebuilt = if noStrengthInput
                then HM.delete "strength_base" rebuilt0 else rebuilt0
            missing = filter (not ∘ (`HM.member` stats)) rebuiltCacheStats
            promoted = not noStrengthInput
                ∧ not (HM.member "strength_base" stats)
                ∧ HM.member "strength" stats
            cacheNotes = ["rebuilt missing body caches: "
                          <> T.intercalate "," missing | not (null missing)]
            promotionNotes = ["promoted strength to strength_base for stable body recomputation"
                             | promoted]
        in (rebuilt, cacheNotes, promotionNotes)

-- | Case (c): rename and retirement, with one note each.
remapOverrides ∷ PhysiologyRegistry → PhysioOverrides → (PhysioOverrides, [Text])
remapOverrides registry overrides = foldl' step (overrides, []) (sortOn id (HM.keys overrides))
  where
    active = map psId (prSystems registry)
    successor old = listToMaybe [ psId s | s ← prSystems registry
                                         , old `elem` psRenamedFrom s ]
    step (acc, ns) sid
        | sid `elem` active = (acc, ns)
        | Just new ← successor sid =
            let mode = HM.lookupDefault PhysioNominal sid acc
                acc' = HM.delete sid acc
            in if HM.member new acc'
                 then (acc', ns ⧺ ["dropped the override for renamed system '"
                                   <> sid <> "' (its successor '" <> new
                                   <> "' already has one)"])
                 else (HM.insert new mode acc',
                       ns ⧺ ["remapped the override for renamed system '"
                             <> sid <> "' to '" <> new <> "'"])
        | sid `elem` prRetired registry =
            (HM.delete sid acc, ns ⧺ ["dropped the override for retired system '"
                                      <> sid <> "'"])
        | otherwise = (acc, ns)     -- undeclared: (d) already rejected it

-- | Case (j).
reconstructFrame ∷ UnitPhysiology → HM.HashMap Text Float
                 → (HM.HashMap Text Float, [Text])
reconstructFrame up stats = case HM.lookup "height" stats of
    Just h
        | upBodyAuthored up
        , not (HM.member "frame_mass" stats)
        , HM.member "fat_mass" stats ∧ HM.member "lean_mass" stats
        , HM.member "body_mass" stats
        , not (isNaN h ∨ isInfinite h), h > 0 →
            ( HM.insert "frame_mass" (22 * h * h) stats
            , ["reconstructed frame_mass = 22 * height^2 ("
               <> tshow (22 * h * h) <> ") for a pre-frame unit"] )
    _ → (stats, [])

-- | Case (g) for the upkeep: a Nominal-nutrition unit with body
--   composition needs a usable frame.
upkeepInputProblem ∷ PhysiologyRegistry → PhysioContext → Maybe (Text, Text, Text)
upkeepInputProblem registry ctx = listToMaybe
    [ ("nutrition", "frame_mass", why)
    | Just sys ← [registrySystemOf "nutrition"]
    , resolveMode sys (pcPhysiology ctx) (engagementApplied (pcStats ctx))
          (HM.lookup "nutrition" (pcOverrides ctx)) ≡ ModeNominal
    , HM.member "fat_mass" stats ∨ HM.member "lean_mass" stats
    , Just why ← [frameProblem] ]
  where
    stats = pcStats ctx
    registrySystemOf sid = listToMaybe [ s | s ← prSystems registry, psId s ≡ sid ]
    frameProblem = case HM.lookup "frame_mass" stats of
        Nothing → Just "missing, and the unit has no height to reconstruct it from"
        Just f | isNaN f ∨ isInfinite f → Just ("not finite (" <> tshow f <> ")")
               | f ≤ 0                  → Just ("not positive (" <> tshow f <> ")")
               | otherwise              → Nothing

renderRepair ∷ KeyRepair → Text
renderRepair r = case r of
    KeysSeeded sid ks     → "seeded missing " <> sid <> " keys: " <> T.intercalate "," ks
    KeysScrubbed sid ks   → "scrubbed keys the " <> sid <> " system does not hold: "
                            <> T.intercalate "," ks
    DerivedScrubbed sid ks → "scrubbed stored derived keys of " <> sid <> ": "
                            <> T.intercalate "," ks

-- | The whole physiology half of the load gate, in the order the load runs
--   it: the rejections the decoded payload alone decides ((d), (e)'s
--   decode failure aside, (h)), then the normalization ((c), (f), (g), (b),
--   (i), (j)). 'Left' is every message that rejects the load, and means
--   NOTHING is published: the caller aborts before staging, with the old
--   session untouched. 'Right' is the normalized unit tables to publish
--   and one note per unit that changed, for one log line each.
physiologyLoadGate
    ∷ PhysiologyRegistry
    → HM.HashMap Text PhysiologyLoadDefinition
    → Double
    → [(WorldPageId, UnitSnapshot)]
    → Either [Text] ([(WorldPageId, UnitSnapshot)], [PhysiologyNormalization])
physiologyLoadGate registry defs now pages
    | not (null decodeRejections) = Left (map renderPhysiologyRejection decodeRejections)
    | otherwise = case normalizeSavedPhysiology registry defs now pages of
        Left rs → Left (map renderPhysiologyRejection rs)
        Right ok → Right ok
  where
    decodeRejections = physiologyLoadRejections registry defs pages

-- * The rebuilt caches (S4, #2831)

-- | The unit stats the load REBUILDS rather than reads back: caches that
--   ride in the stat-map payload ("serialized-but-ignored",
--   @docs\/persistence_state_inventory.md@'s @UnitInstance (physiology
--   stats)@ table). Bodied-unit caches are first recomputed at the load gate
--   before physiology seeding reads them, then remain recomputed before the
--   first paused frame by @unitResources.onSaveLoaded@. @circulation@ (#2836, S9) and, since #2838 (S11a),
--   @consciousness@, @concentration@ and @state_of_mind@ are not among them:
--   they are derived on demand and never stored, and a legacy stored copy is
--   scrubbed by the load gate in every mode.
--
--   They can legitimately differ between a save and the load of that save:
--   a pass drains hydration after the recompute ran, so the saved
--   @max_hydration@ can be a tick stale and the rebuilt one is not. That is
--   not a persistence defect, so the structural save comparison
--   ('withoutRebuiltCaches') does not count them as durable state.
--   Everything NOT listed here — the pools, meters, overrides,
--   @tissue_recovering@, the integrators (@heart_rate@, @blood_oxygen@,
--   @mood@, @emotional_pain@, @caffeine@) and the mind's episode keys — is
--   durable and compared exactly.
rebuiltCacheStats ∷ [Text]
rebuiltCacheStats =
    [ "strength", "strength_body"
    , "max_hydration", "max_hunger", "max_calories", "carrying_capacity" ]

-- | A session snapshot without those caches in any unit's stat map, and
--   without the stored blood volume of a unit whose blood is pinned Nominal:
--   that dedicated field is serialized but IGNORED while Nominal and the load
--   rewrites it to the current canonical maximum (#2837), so a stale saved
--   copy and its rebuilt successor are the same durable state. A Simulated
--   unit's volume (and every other unit's, whose mode this registry-free
--   comparison cannot see) stays compared exactly. The durable half, for
--   comparing saves taken across a load.
withoutRebuiltCaches ∷ SessionSnapshot → SessionSnapshot
withoutRebuiltCaches snap = snap { snapPages = HM.map onPage (snapPages snap) }
  where
    onPage pg = pg { pgsUnits = onUnits (pgsUnits pg) }
    onUnits us = us { usnInstances = HM.map onUnit (usnInstances us) }
    onUnit u = u { uisStats = foldl' (flip HM.delete) (uisStats u) rebuiltCacheStats
                 , uisBlood = if blood u ≡ Just PhysioNominal then 0 else uisBlood u }
    blood = HM.lookup "blood" ∘ uisPhysioOverrides
