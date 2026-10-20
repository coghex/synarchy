{-# LANGUAGE Strict #-}
-- | Reading live units back into scenario entries (#2710, SCN-10 of
--   epic #2698): the capture half of the unit adapters.
--
--   Capture is a pure read of a coherent snapshot the caller supplies —
--   the unit instances, the live reference and tag lookups, the item
--   capture context and the game time, all from one instant. It advances
--   no simulation, heals or refills nothing, rerolls no live stat and
--   draws no gameplay RNG: there is no engine handle here.
--
--   __Every supported current value is written explicitly__ (D-12): the
--   authorable stats the field table names ("Scenario.Units.Catalog")
--   with their STORED values, every skill and known knowledge, the
--   wounds (an empty list for a healed unit), scars, blood, immune state
--   and the current loadout (an empty inventory stays @[]@). A supported
--   stat a lazy unit has never resolved is written as its deterministic
--   scenario fallback for the captured identity, which is exactly the
--   value a construction from that identity would give it; the live unit
--   is not touched. Derived values (strength of a body unit, the maxima,
--   carrying capacity) are left for construction to re-derive.
--
--   __Modifiers are written once, by their owner.__ A modifier whose
--   source the definition's innate modifiers or a worn accessory
--   supplies is rebuilt from that owner at construction, so it is not
--   written; an expired one is gone. Every other one is written with its
--   remaining time.
--
--   __Excluded__: orders, the AI's jobs, paths and transitions, the pose
--   (re-derived from health), render handles, the faction profile (owner
--   decision: units take their definition's), physiology overrides,
--   meters and mental episode state, encounter rosters (the location
--   adapter's, SCN-12), and dead units, which are not scenario units.
--
--   __Identity.__ Unit names follow the item capture's tier rule
--   ("Scenario.Items.Capture"): a unit built from reference @R@ is
--   captured as @R@ (an automatic path in its bracketless form), and
--   the referenced units are named, explicit before automatic and then
--   by raw reference, before any item, so their names are tier-1 ids no
--   item can take. An unreferenced unit is named @units.<k>@ by its
--   place in a STRUCTURAL order ('UnitSig', a derived 'Ord' over raw
--   values: floats as bit patterns, item signatures, never a rendered
--   string and never a unit id), so two captures of the same arena agree
--   however the managers were filled.
module Scenario.Units.Capture
    ( UnitCaptureCtx(..)
    , UnitsCapture(..)
    , captureUnits
    , unitTrees
    , UnitSig(..)
    , unitSig
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Float (castFloatToWord32, castDoubleToWord64)
import Gameplay.Tags.Types (GameplayTag)
import Item.Types (ItemInstance, ItemManager)
import Scenario.Items.Capture
import Scenario.Types
import Scenario.Units.Catalog
import Scenario.Units.Defaults (rollTemplate)
import Unit.Physiology (resolveMode, bloodSystem)
import Unit.Physiology.Types (PhysiologyRegistry, ResolvedMode(..))
import Unit.Thread.Command.Spawn (spawnModifierMap)
import Unit.Direction (Direction)
import Unit.Types

-- | What capture reads besides the instances, from the same instant.
data UnitCaptureCtx = UnitCaptureCtx
    { uccDefs       ∷ !(HM.HashMap Text UnitDef)
    , uccItems      ∷ !ItemManager
    , uccPhysiology ∷ !PhysiologyRegistry
    , uccNow        ∷ !Double
    , uccUnitRef    ∷ UnitId → Maybe Text
      -- ^ 'Unit.Types.Manager.umScenarioRefs'
    , uccUnitTags   ∷ UnitId → Set.Set GameplayTag
      -- ^ 'Unit.Types.Manager.umTags'
    , uccItemCtx    ∷ !CaptureCtx
    , uccOtherTrees ∷ [ItemInstance]
      -- ^ the item trees of every OTHER owner of the document (buildings,
      --   ground): reserved together with the units' own, so every
      --   reference in the document is named up front and no owner
      --   captured first can take a name another is entitled to keep
    , uccPreNamed   ∷ UnitId → Maybe Text
      -- ^ the final explicit id of a referenced unit when the caller named
      --   the document's references together ('nameDocumentRefs'); 'Nothing'
      --   names the unit here, against 'captureUnits'' foreign ids
    , uccEncounterOf ∷ UnitId → Maybe ScenarioId
      -- ^ the explicit id of the location whose encounter roster the unit
      --   belongs to ('Scenario.Placement.Location', #2712), which owns
      --   the roster; 'const Nothing' for a document with no locations
    }

data UnitsCapture = UnitsCapture
    { ucapEntries ∷ ![UnitEntry]
      -- ^ by captured name
    , ucapRefs    ∷ ![(UnitId, Text)]
      -- ^ unit id → the explicit id its entry carries
    , ucapItems   ∷ ![(Word64, Text)]
      -- ^ item instance id → the explicit id its entry carries
    , ucapSkipped ∷ ![UnitId]
      -- ^ dead units and units of an unknown definition, not captured
    , ucapState   ∷ !CaptureState
      -- ^ for the later owners of the same document
    }

-- | Capture @units@ together. @docIds@ are the ids of every other entry
--   of the document being written (buildings, locations, …) — not units,
--   not items.
captureUnits ∷ UnitCaptureCtx → [Text] → [(UnitId, UnitInstance)] → UnitsCapture
captureUnits ctx docIds units0 = UnitsCapture
    { ucapEntries = map snd captured
    , ucapRefs = [ (uid, n) | (uid, n, _) ← named ]
    , ucapItems = concatMap fst captured
    , ucapSkipped = sortOn id skipped
    , ucapState = stFinal }
  where
    live = [ (uid, u, d) | (uid, u) ← units0, uiPose u ≢ "dead"
                         , Just d ← [HM.lookup (uiDefName u) (uccDefs ctx)] ]
    skipped = [ uid | (uid, _) ← units0, uid `notElem` [ i | (i, _, _) ← live ] ]
    -- tier 1 for items: the referenced units, named first, explicit
    -- references before automatic ones, then by raw reference
    -- a name the caller already decided document-wide ('nameDocumentRefs')
    -- is final; the rest are named here against the foreign ids
    referenced = [ (uid, r) | (uid, _, _) ← live, Just r ← [uccUnitRef ctx uid]
                            , isNothing (uccPreNamed ctx uid) ]
    foreign0 = HS.fromList docIds
    preNamed = Map.fromList [ (uid, n) | (uid, _, _) ← live, Just n ← [uccPreNamed ctx uid] ]
    refNames = Map.union preNamed (nameReferenced (HS.union foreign0 (HS.fromList (Map.elems preNamed))) referenced)
    taken0 = HS.union foreign0 (HS.fromList (Map.elems refNames))
    -- every item tree of every captured unit, reserved together
    st0 = newCaptureState (uccItemCtx ctx) (HS.toList taken0)
              (concat [ unitTrees u | (_, u, _) ← live ] ⧺ uccOtherTrees ctx)
    -- unreferenced units in structural order, each named after the
    -- reservation so it can never take an item's reference
    unreferenced = sortOn (\(uid, u, _) → unitSig ctx uid u)
        [ x | x@(uid, _, _) ← live, not (Map.member uid refNames) ]
    (st1, genNames) = mapAccumL' (\st (k, (uid, _, _)) →
                                      let (n, st') = reserveDocumentId ("units." <> tshow k) st
                                      in (st', (uid, n)))
                                 st0 (zip [0 ∷ Int ..] unreferenced)
    nameOf = Map.union refNames (Map.fromList genNames)
    named = sortOn (\(_, n, _) → n)
        [ (uid, n, (u, d)) | (uid, u, d) ← live, Just n ← [Map.lookup uid nameOf] ]
    (stFinal, captured) = mapAccumL' (\st (uid, n, (u, d)) →
                                          let (st', e, is) = captureOne ctx st uid n u d
                                          in (st', (is, e)))
                                     st1 named

-- | Every item tree a unit holds, in capture order.
unitTrees ∷ UnitInstance → [ItemInstance]
unitTrees u = uiInventory u ⧺ map snd (sortOn fst (HM.toList (uiEquipment u))) ⧺ uiAccessories u

captureOne ∷ UnitCaptureCtx → CaptureState → UnitId → Text → UnitInstance → UnitDef
           → (CaptureState, UnitEntry, [(Word64, Text)])
captureOne ctx st0 uid name u d = (st1, entry, capRefs (ucCaptured itemsC))
  where
    (st1, itemsC) = captureUnitItems (uccItemCtx ctx) st0 name u
    reg = uccPhysiology ctx
    now = uccNow ctx
    rules = unitStatRules reg d
    authorable = [ k | (k, AuthorableStat) ← HM.toList rules ]
    -- the stored value, or the scenario fallback a construction under
    -- this identity would give a stat the live unit never resolved
    statValue k = case HM.lookup k (uiStats u) of
        Just v → Just v
        Nothing
            | k `elem` bodyInputStats ⧺ bodyMassStats ⧺ ["strength_base"] → Nothing
            | Just t ← HM.lookup k (udStatTemplates d) → Just (rollTemplate name ("stat/" <> k) t)
            | otherwise → Nothing
    stats = HM.fromList [ (k, v) | k ← authorable, k `notElem` ["bulk", "bodyfat"]
                                 , Just v ← [statValue k] ]
    owned = HS.fromList [ smSource m | ms ← HM.elems (spawnModifierMap (uccItems ctx) d (uiAccessories u))
                                     , m ← ms ]
    modStats = HS.fromList (HM.keys (udSkillTemplates d) ⧺ HM.keys rules)
    mods = HM.filter (not ∘ null) $ HM.fromList
        [ (stat, [ ModifierSpec (smSource m) (smDelta m) (smPercent m)
                       (realToFrac ∘ subtract now ⊚ smExpiry m)
                 | m ← sortOn smSource ms
                 , not (HS.member (smSource m) owned)
                 , maybe True (> now) (smExpiry m) ])
        | (stat, ms) ← HM.toList (uiModifiers u), HS.member stat modStats ]
    bloodSim = maybe False (\bs → resolveMode bs (udPhysiology d) False Nothing ≡ ModeSimulated)
                     (bloodSystem reg)
    entry = UnitEntry
        { ueId = ExplicitId name
        , ueTags = Set.toAscList (uccUnitTags ctx uid)
        , ueDefinition = uiDefName u
        , ueX = uiGridX u, ueY = uiGridY u, ueZ = Authored (uiGridZ u)
        , ueName = Authored (uiName u), ueFacing = Authored (uiFacing u)
        , ueEncounter = uccEncounterOf ctx uid
        , ueStats = stats
        , ueSkills = HM.filterWithKey (\k _ → HM.member k (udSkillTemplates d)) (uiSkills u)
        , ueKnowledge = Authored (uiKnowledge u)
        , ueModifiers = Authored mods
        , ueWounds = Authored (map (woundSpec now) (uiWounds u))
        , ueScars = Authored (map (scarSpec now) (uiScars u))
        , ueBlood = if bloodSim then Authored (uiBlood u) else Omitted
        , ueImmuneResponse = Authored (uiImmuneResponse u)
        , ueImmunities = Authored (uiImmunities u)
        , ueInventory = Authored (ucInventory itemsC)
        , ueEquipment = Authored (HM.fromList (ucEquipment itemsC))
        , ueAccessories = Authored (ucAccessories itemsC) }

woundSpec ∷ Double → Wound → WoundSpec
woundSpec now w = WoundSpec
    { wsPart = woundPart w, wsKind = woundKind w, wsSeverity = woundSeverity w
    , wsAge = Authored (realToFrac (max 0 (now - woundAt w)))
    , wsBandage = Authored (woundBandage w), wsClot = Authored (woundClot w)
    , wsHeal = Authored (woundHeal w), wsDressing = Authored (woundDressing w)
    , wsInfection = Authored (woundInfection w), wsClean = Authored (woundClean w)
    , wsInfectionType = Authored (woundInfectionType w)
    , wsNecrosis = Authored (woundNecrosis w) }

scarSpec ∷ Double → Scar → ScarSpec
scarSpec now s = ScarSpec
    { ssPart = scarPart s, ssKind = scarKind s, ssSeverity = scarSeverity s
    , ssAge = Authored (realToFrac (max 0 (now - scarAt s))) }

-- * Structural order

-- | Everything a capture exports about an unreferenced unit except its
--   name, as raw values under the derived 'Ord' (floats as bit
--   patterns). Two units tie only when they export identically.
data UnitSig = UnitSig
    { usgDef        ∷ !Text
    , usgPos        ∷ !(Word32, Word32, Int)
    , usgFacing     ∷ !Direction
    , usgName       ∷ !Text
    , usgStats      ∷ ![(Text, Word32)]
    , usgSkills     ∷ ![(Text, Word32)]
    , usgKnowledge  ∷ ![(Text, Word32)]
    , usgModifiers  ∷ ![(Text, [(Text, Word32, Word32, Maybe Word64)])]
    , usgWounds     ∷ ![[Word64]]
    , usgWoundText  ∷ ![(Text, Text, Text, Bool, Text)]
    , usgScars      ∷ ![(Text, Text, Word32, Word64)]
    , usgHealth     ∷ !(Word32, Word32, [(Text, Word32)])
    , usgTags       ∷ !(Set.Set GameplayTag)
    , usgItems      ∷ ![ItemSig]
    , usgSlots      ∷ ![Text]
    } deriving (Eq, Ord, Show)

unitSig ∷ UnitCaptureCtx → UnitId → UnitInstance → UnitSig
unitSig ctx uid u = UnitSig
    { usgDef = uiDefName u
    , usgPos = (f (uiGridX u), f (uiGridY u), uiGridZ u)
    , usgFacing = uiFacing u
    , usgName = uiName u
    , usgStats = kv (uiStats u), usgSkills = kv (uiSkills u), usgKnowledge = kv (uiKnowledge u)
    , usgModifiers = sortOn fst
        [ (k, sortOn id [ (smSource m, f (smDelta m), f (smPercent m), castDoubleToWord64 ⊚ smExpiry m)
                        | m ← ms ])
        | (k, ms) ← HM.toList (uiModifiers u) ]
    , usgWounds = [ [ castDoubleToWord64 (woundAt w), fw (woundSeverity w), fw (woundBandage w)
                    , fw (woundClot w), fw (woundHeal w), fw (woundInfection w), fw (woundNecrosis w) ]
                  | w ← uiWounds u ]
    , usgWoundText = [ (woundPart w, woundKind w, woundDressing w, woundClean w, woundInfectionType w)
                     | w ← uiWounds u ]
    , usgScars = [ (scarPart s, scarKind s, f (scarSeverity s), castDoubleToWord64 (scarAt s))
                 | s ← uiScars u ]
    , usgHealth = (f (uiBlood u), f (uiImmuneResponse u), kv (uiImmunities u))
    , usgTags = uccUnitTags ctx uid
    , usgItems = map (itemSig (uccItemCtx ctx)) (unitTrees u)
    , usgSlots = sortOn id (HM.keys (uiEquipment u))
    }
  where
    f = castFloatToWord32
    fw = fromIntegral ∘ castFloatToWord32
    kv m = sortOn fst [ (k, f v) | (k, v) ← HM.toList m ]

mapAccumL' ∷ (s → a → (s, b)) → s → [a] → (s, [b])
mapAccumL' _ s [] = (s, [])
mapAccumL' g s (x : xs) =
    let (s', y) = g s x
        (s'', ys) = mapAccumL' g s' xs
    in (s'', y : ys)
