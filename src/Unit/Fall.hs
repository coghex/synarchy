{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Physics-based fall injury model.
--
-- A fall is turned into a SET of injuries (not a single scalar outcome):
--
--   1. Drop height @h = dropZ × metresPerZ@ (metres).
--   2. Impact velocity @v = √(2·g·h)@, impact energy @E = ½·m·v² = m·g·h@
--      (joules) from the unit's body mass.
--   3. That energy is delivered through the body from the feet up. Each
--      body part resists a share of it in proportion to its BONE — the
--      bone-substance thickness in the part's tissue stack (`bpLayers`)
--      times that substance's fracture toughness — scaled by the unit's
--      toughness stat. Where the delivered load exceeds a part's bone
--      resistance, that part fractures, with severity ∝ the overload.
--   4. The head's injury is a concussion; everything else is a fracture.
--      Severity is the same continuous 0..~1.6 scale as combat wounds,
--      so a severe-enough injury to a VITAL part (head/neck/torso) is
--      lethal through the normal wound→death rule — a tall fall kills by
--      breaking the body, not by a bespoke height check.
--
-- Pure + deterministic (no RNG): which leg/arm a fall hits is cosmetic
-- variety handled by the caller, but WHAT breaks at a given height is
-- physics, so it's reproducible. Calibration constants are gathered at
-- the top; tune them against `tools`/headless dumps, not by feel.
module Unit.Fall
    ( FallInjury(..)
    , fallInjuries
    , fallStunFor
    -- * Tunables
    , metresPerZ
    , gravity
    ) where

import UPrelude
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Unit.Types (UnitDef(..), BodyPart(..))
import Unit.Injury (tissueInjuryKind, injuryFloor, capInjurySeverity
                   , allocateSubparts)
import Substance.Types (SubstanceManager)

-- | One injury produced by a fall: which part, what kind ("fracture" /
--   "concussion"), and a 0..~1.6 severity on the shared wound scale.
data FallInjury = FallInjury
    { fiPart     ∷ !Text
    , fiKind     ∷ !Text
    , fiSeverity ∷ !Float
    } deriving (Show, Eq)

-- * Tunables ---------------------------------------------------------

-- | Metres of real height per world z-level. A z-step is roughly a
--   floor/step — human-scale — so a 2-z drop ≈ 3 m. Drives the whole
--   energy calc; raise it to make every fall more dangerous.
metresPerZ ∷ Float
metresPerZ = 1.5

gravity ∷ Float
gravity = 9.81

-- | Global calibration: scales delivered impact energy into the
--   severity domain. Tuned (with the per-tissue densities below) against
--   headless drop tests so a baseline acolyte (70 kg, toughness 1.0):
--   walks off ≤1-z; is bruised + knocked down at 2–4-z; takes its first
--   ankle/leg fracture around 5–6-z; accumulates leg / hip / concussion
--   damage through ~8–11-z; and suffers a lethal (vital severity ≥1)
--   injury — death-by-injury — only on a very tall fall, after racking
--   up many fractures first.
energyToSeverity ∷ Float
energyToSeverity = 0.040

-- | Per-tissue "failure energy" density (∝ energy to injure 1 severity
--   per mm of that tissue). Higher = harder to injure. Bone resists
--   hardest (fracture only on real loads); soft tissue bruises easily
--   (but is capped shallow — see 'bruiseCap'); the brain (organ)
--   concusses from transmitted shock well before the skull breaks (it
--   uses a small EFFECTIVE thickness — see 'organEffectiveThickMm' — not
--   its full depth, because a concussion is a shock threshold, not
--   pulping the whole organ).
tissueFailureDensity ∷ Text → Float
tissueFailureDensity t = case t of
    "bone"      → 11.0
    "cartilage" → 6.0
    "organ"     → 12.0
    "nerve"     → 12.0   -- concussion threshold (with the depth cap)
    "artery"    → 14.0   -- a fall rarely ruptures a vessel
    "flesh"     → 1.2
    "skin"      → 1.2
    _           → 4.0

-- | Effective thickness (mm) used for an organ's failure energy. A
--   concussion / internal injury is a shock/transmission threshold, so a
--   thick organ (an 80 mm brain) shouldn't be proportionally harder to
--   concuss — we cap the thickness that counts. Keeps concussion
--   reachable before the skull's own fracture energy.
organEffectiveThickMm ∷ Float
organEffectiveThickMm = 12.0

-- | Effective bone (mm) given to a part that has NO modelled bone layer
--   (feet, hands, neck) so they can still fracture — small bones /
--   cartilage the tissue stack doesn't itemise. Without it an ankle or
--   wrist could never break in a fall.
softBoneEquivMm ∷ Float
softBoneEquivMm = 18.0

-- * Model ------------------------------------------------------------

-- | Compute the injuries a fall of @dropZ@ z-levels inflicts. Each body
--   part the impact loads yields a DISTRIBUTION across its tissue layers
--   — a leg gives a bruise (flesh) and, on a hard enough fall, a fracture
--   (bone); the head adds a concussion (brain) and a skull fracture. The
--   injury KINDS come from the shared 'tissueInjuryKind', so a fall-broken
--   bone is identical to a combat-broken one. Pure + RNG-free: the
--   injuries are a function of the drop energy and each part's tissues.
fallInjuries
    ∷ SubstanceManager
    → UnitDef
    → Float          -- ^ body mass (kg)
    → Float          -- ^ toughness stat (1.0 = baseline)
    → Int            -- ^ drop magnitude in z-levels (positive)
    → [FallInjury]
fallInjuries _sm def mass toughness dropZ
    | dropZ ≤ 1 = []     -- a single step down is a free walk-off
    | otherwise =
        let h      = fromIntegral dropZ * metresPerZ
            energy  = mass * gravity * h            -- joules
            tough   = max 0.2 toughness
            parts   = udBodyParts def
            -- A fall is a blunt load on each macro REGION (feet-first), and
            -- WITHIN a region it concentrates on the load-bearing structure
            -- — the same blunt allocation a combat blow uses (one bone +
            -- one transmitted organ), NOT every leaf. That's what stops a
            -- fall from shattering all ten toes. reach grows with the drop
            -- so taller falls reach the deeper structures (organs, spine).
            reach   = max 0.0 (min 1.0 (0.30 + fromIntegral dropZ * 0.05))
            macros  = filter bpTargetable parts
            subsOf m = [ p | p ← parts, bpParent p ≡ Just (bpId m)
                           , not (bpTargetable p) ]
            go m =
                let expo = exposureFor (bpId m)
                    subs = subsOf m
                in if null subs
                   then partInjuries energy tough expo m   -- simple part
                   else concatMap (partInjuries energy tough expo)
                          (allocateSubparts "blunt" reach (rollFor m) subs)
        in concatMap go macros

-- | A deterministic per-region roll in [0,1) for the subpart pick (falls
--   have no RNG — which bone takes a fall is a stable function of the
--   region id). A simple string hash folded to a fraction.
rollFor ∷ BodyPart → Float
rollFor m =
    let h = T.foldl' (\a c → a * 31 + fromEnum c) 7 (bpId m)
        x = fromIntegral (h `mod` 1000) / 1000.0
    in x

-- | The injury distribution a fall inflicts on ONE part: walk its tissue
--   layers, and for each, severity = energy reaching it / that tissue's
--   failure energy (density × thickness × toughness). A blunt fall
--   transmits through the whole stack, so every loaded tissue can injure.
partInjuries ∷ Float → Float → Float → BodyPart → [FallInjury]
partInjuries energy tough expo p =
    let load    = energy * expo * energyToSeverity
        rawLs   = if null (bpLayers p) then [("flesh", "flesh", 30.0)] else bpLayers p
        layers  = ensureBone rawLs
        -- Soft deep tissues (organ, nerve, artery) injure on a SHOCK
        -- threshold, not by destroying their full depth, so cap the
        -- thickness that counts (a concussion isn't pulping the whole
        -- 80 mm brain).
        deep t  = t ≡ "organ" ∨ t ≡ "nerve" ∨ t ≡ "artery"
        injFor (_nm, tissue, thick) = do
            k ← tissueInjuryKind tissue "blunt"
            let effThick = if deep tissue
                           then min thick organEffectiveThickMm
                           else max 1.0 thick
                fe  = tissueFailureDensity tissue * effThick * tough
                sev = capInjurySeverity k (load / max 0.001 fe)
            if sev ≥ injuryFloor
                then Just FallInjury { fiPart = bpId p, fiKind = k
                                     , fiSeverity = sev }
                else Nothing
    in mapMaybe injFor layers

-- | Guarantee a bone layer so boneless extremities (foot/hand) can still
--   fracture an ankle / wrist. If the part already has bone, leave it.
ensureBone ∷ [(Text, Text, Float)] → [(Text, Text, Float)]
ensureBone ls
    | any (\(_, m, _) → m ≡ "bone") ls = ls
    | otherwise                        = ls <> [("bone", "bone", softBoneEquivMm)]

-- | Fraction of the impact energy delivered to a part on a feet-first
--   landing. Feet and legs take the brunt; the hips/torso and braced
--   wrists take a moderate share; the head only catches significant
--   load on a hard fall (whiplash / crumple), which is what makes tall
--   falls concuss and ultimately kill. Matched on id substrings so the
--   same table works for any humanoid body layout.
exposureFor ∷ Text → Float
-- A feet-first landing loads the legs/feet most, the hip/pelvis and a
-- braced wrist moderately, and the head/neck only lightly (whiplash /
-- secondary impact) — so falls break ankles, fibulas and hips long
-- before they concuss, and only a severe fall reaches the head.
exposureFor pid
    | "foot" `T.isInfixOf` pid = 0.45
    | "leg"  `T.isInfixOf` pid = 0.45
    | "hip"  `T.isInfixOf` pid = 0.32
    | "hand" `T.isInfixOf` pid = 0.18   -- bracing the fall (wrist)
    | "arm"  `T.isInfixOf` pid = 0.10
    | "torso" ≡ pid            = 0.30   -- pelvis / spine
    | "neck"  ≡ pid            = 0.07
    | "head"  ≡ pid            = 0.09
    | otherwise                = 0.18

-- | Knockdown stun (seconds) for a fall, from its worst injury severity.
--   A clean landing (no fractures) is still a brief stagger; a bad break
--   keeps the unit down longer. The per-injury impairment (a broken leg
--   that re-collapses the unit until it heals) is layered on top in Lua;
--   this is just the initial winded/getup window.
fallStunFor ∷ Float → Double
fallStunFor worstSeverity =
    realToFrac (1.0 + 5.0 * max 0 worstSeverity)
