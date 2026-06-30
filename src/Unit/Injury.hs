{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Shared injury physics + vocabulary.
--
-- A wound's `woundKind` is the INJURY TYPE — "bruise"/"blunt",
-- "laceration"/"slash", "puncture"/"stab", "fracture", "concussion",
-- "internal", "severed" — NOT the mechanism that caused it. This module
-- is the single place both damage systems funnel through, so an injury
-- is identical regardless of source: a femur broken by a club and one
-- broken by a fall are both `"fracture"`, heal/bleed/pain the same, share
-- an icon + name, and feed the same impairment + death + severing rules.
--
-- The core idea is a TISSUE-LAYER CASCADE with NO RNG: a hit delivers
-- energy to a body part; that energy walks the part's tissue stack
-- (outer→inner, the `bpLayers` data), each layer absorbing some and
-- transmitting the rest; every layer that's loaded past a floor yields
-- its own injury, of a kind set by that tissue + the mechanism. One hit
-- thus produces a DISTRIBUTION of wounds of varying strength — a hard
-- blow to the head leaves a scalp bruise AND a concussion AND (if hard
-- enough) a skull fracture, all at once.
--
--   * 'penetrateDeposits' — the layer walk; returns the wound-weighted
--     energy deposited in each tissue layer. Blunt transmits inward
--     (bone passes shock to the brain); cuts are absorbed/blocked by
--     bone. Used by BOTH 'Combat.Resolution' and 'Unit.Fall'.
--   * 'tissueInjuryKind' — maps (tissue, mechanism, isHead) to the
--     injury kind, or Nothing for non-tissue layers (worn armour, which
--     absorbs but is not wounded).
--
-- Severity NORMALISATION (deposit → 0..1 severity) stays with each
-- caller, because combat normalises by part max-hp and the swing's
-- energy/momentum driver while a fall normalises by its own impact
-- energy — but the walk and the kind vocabulary are shared here.
module Unit.Injury
    ( -- * Tissue → injury vocabulary
      tissueInjuryKind
    , isBodyTissue
      -- * Subpart allocation (shared by combat + falls)
    , allocateSubparts
    , weightedPick
      -- * Part durability (derived from tissue, replaces max_health_factor)
    , tissueCapacityWeight
    , layerCapacity
    , defaultPartCapacity
      -- * Tissue-penetration walk (shared physics)
    , penetrateDeposits
    , penetrate
    , woundFactor
    , layerAbsorb
      -- * Severity caps
    , capInjurySeverity
    , bruiseCap
    , maxInjurySeverity
      -- * Tunables
    , injuryFloor
    , layerRefThickness
    , cutAbsorbScale
    , bluntAbsorbScale
    ) where

import UPrelude
import qualified Data.Text as T
import Data.List (sortOn, groupBy)
import Data.Function (on)
import Substance.Types (SubstanceDef(..))
import Unit.Types (BodyPart(..))

-- * Tunables ---------------------------------------------------------

-- | Minimum severity for a layer's injury to register. Below this the
--   tissue took no real injury (a faint pressure, not a wound) and we
--   drop it, so a hit leaves a legible handful of wounds rather than a
--   dusting of 0.000x scratches.
injuryFloor ∷ Float
injuryFloor = 0.1

-- | Reference layer thickness (mm). A layer this thick at resistance 1.0
--   costs one unit of the absorb scales below.
layerRefThickness ∷ Float
layerRefThickness = 10.0

-- | Energy (J) a reference-thickness layer of resistance 1.0 absorbs
--   from a cut/pierce, before the weapon's penetration divides it.
cutAbsorbScale ∷ Float
cutAbsorbScale = 40.0

-- | Momentum (kg·m/s) a reference-thickness layer of resistance 1.0
--   soaks from a blunt impact. Soft layers cushion; the rest transmits
--   inward to crush the core.
bluntAbsorbScale ∷ Float
bluntAbsorbScale = 20.0

-- * Tissue → injury vocabulary ---------------------------------------

-- | Is this layer a living body tissue (so it can be WOUNDED), as
--   opposed to worn armour (which absorbs energy but takes no wound)?
--   Fully tissue-driven so the body plan stays generic (humanoid → robot
--   → fish): the injury a layer produces is a property of its MATERIAL,
--   not of which named part it sits in.
isBodyTissue ∷ Text → Bool
isBodyTissue t = case t of
    "flesh"     → True
    "skin"      → True
    "fat"       → True
    "muscle"    → True
    "bone"      → True
    "cartilage" → True
    "organ"     → True
    "nerve"     → True   -- brain / spinal cord
    "artery"    → True   -- major vessel
    "keratin"   → True   -- horn / hide / claw-bearing
    "chitin"    → True
    "dentin"    → True
    _           → False

-- | The injury KIND a given tissue takes from a given mechanism. Nothing
--   for non-tissue (armour / unknown) layers, which absorb but are never
--   wounded. Concussion is now driven by the @nerve@ tissue (brain /
--   spinal cord), NOT by being "the head" — a creature with no nerve
--   tissue (a robot) simply can't be concussed.
--
--     bone / cartilage / dentin → "fracture"  (break / crush)
--     nerve                     → "concussion" (neural trauma)
--     organ                     → "internal"   (internal bleeding / trauma)
--     artery                    → "arterial"   (major bleed)
--     soft tissue               → mechanism:   blunt→"blunt"(bruise), slash, stab
--
--   A single subpart's tissue stack drives several of these at once as a
--   strike crosses its layers (skull: bruise + fracture + concussion).
tissueInjuryKind ∷ Text → Text → Maybe Text
tissueInjuryKind tissue mech
    | not (isBodyTissue tissue)               = Nothing
    | tissue `elem` ["bone","cartilage","dentin"] = Just "fracture"
    -- Neural tissue: a BLUNT blow is a concussion (shock); a cut/pierce
    -- that reaches it is penetrating trauma, not a concussion.
    | tissue ≡ "nerve"   = Just (if mech ≡ "blunt" then "concussion" else "internal")
    | tissue ≡ "organ"   = Just "internal"
    | tissue ≡ "artery"  = Just "arterial"
    | otherwise = Just (softTissueKind mech)  -- flesh/skin/keratin/chitin

-- | Soft-tissue surface injury for a mechanism.
softTissueKind ∷ Text → Text
softTissueKind "slash" = "slash"   -- laceration
softTissueKind "stab"  = "stab"    -- puncture
softTissueKind _       = "blunt"   -- bruise / contusion

-- * Severity caps ----------------------------------------------------

-- | A bruise / contusion is never a critical, life-threatening injury on
--   its own — the worst it caps at is a deep, ugly contusion. Without
--   this a high-energy soft-tissue deposit (a hard fall's scalp impact)
--   would read as a near-lethal "bruise" and could even kill via the
--   vital-part death rule. Structural injuries (fracture / concussion /
--   internal / severed) are what get dangerous.
bruiseCap ∷ Float
bruiseCap = 0.4

-- | Hard ceiling on any injury's severity. >1 on a vital part is lethal;
--   the cap keeps a catastrophic hit from stacking absurd numbers while
--   still allowing the lethal ≥1 (→ "crushed" / "fatal") outcome.
maxInjurySeverity ∷ Float
maxInjurySeverity = 1.6

-- | Clamp a computed severity to its kind's allowed range: bruises stay
--   shallow, everything else can climb to the lethal ceiling. Used by
--   BOTH combat and falls so the same injury kind has the same severity
--   meaning regardless of source.
capInjurySeverity ∷ Text → Float → Float
capInjurySeverity kind sev =
    let hi = if kind ≡ "blunt" then bruiseCap else maxInjurySeverity
    in max 0.0 (min hi sev)

-- * Part durability -------------------------------------------------

-- | How much a tissue contributes to a part's structural DURABILITY per
--   mm — i.e. how much it resists being destroyed. Replaces the old
--   hand-authored `max_health_factor`: a part's "max HP" is now derived
--   from what it's MADE of. Bone is the structure (high); soft tissue
--   moderate; organs/nerve/artery are squishy and add little (so a deep
--   organ isn't artificially hard to destroy once reached — the overlying
--   tissue's protection is already in the penetration walk, not here).
tissueCapacityWeight ∷ Text → Float
tissueCapacityWeight m = case m of
    "bone"      → 1.0
    "dentin"    → 0.9
    "chitin"    → 0.7
    "keratin"   → 0.6
    "cartilage" → 0.5
    "muscle"    → 0.45
    "flesh"     → 0.4
    "skin"      → 0.4
    "fat"       → 0.25
    "organ"     → 0.15
    "nerve"     → 0.1
    "artery"    → 0.1
    _           → 0.4

-- | A part's structural capacity = Σ(tissue weight × thickness mm) over
--   its layers. The combat severity normaliser scales this into "max HP".
layerCapacity ∷ [(Text, Text, Float)] → Float
layerCapacity layers = sum [ tissueCapacityWeight m * t | (_, m, t) ← layers ]

-- | Fallback capacity for a part with no declared layers (≈ 40 mm of
--   flesh) — keeps creatures whose YAML hasn't been given tissue layers
--   (bear/technomule) at a sane uniform durability.
defaultPartCapacity ∷ Float
defaultPartCapacity = tissueCapacityWeight "flesh" * 40.0

-- * Tissue-penetration walk ------------------------------------------

-- | How lethal it is to deposit damage into a given tissue, per kind.
--   The vital innermost core (organ) is deadly; flesh bleeds; bone is
--   structural (a nicked bone is minor, a fractured one under a blunt
--   blow is serious); a hit landing on armour/skin-thin tissue barely
--   counts as a wound.
woundFactor ∷ Text → Text → Float
woundFactor mat kind = case mat of
    "organ"     → 1.5
    "nerve"     → 1.6   -- brain / cord: destroying it is catastrophic
    "artery"    → 1.3   -- opening it is a fast bleed-out
    "flesh"     → if kind ≡ "blunt" then 0.3 else 0.7
    "skin"      → if kind ≡ "blunt" then 0.3 else 0.7
    "fat"       → if kind ≡ "blunt" then 0.2 else 0.5
    "muscle"    → if kind ≡ "blunt" then 0.3 else 0.7
    "bone"      → if kind ≡ "blunt" then 0.7 else 0.2
    "cartilage" → if kind ≡ "blunt" then 0.6 else 0.4
    "dentin"    → if kind ≡ "blunt" then 0.6 else 0.2
    "keratin"   → 0.3
    "chitin"    → 0.2
    _           → 0.1   -- skin-thin / armour plate: the wound is deeper in

-- | Energy (cut/pierce) or momentum (blunt) one tissue layer soaks from
--   a strike. Cut/pierce: proportional to the layer's kind-resistance ×
--   thickness, made cheaper by weapon penetration. Blunt: soft layers
--   cushion (resistance × thickness), no penetration discount.
layerAbsorb ∷ Maybe SubstanceDef → Float → Text → Float → Float
layerAbsorb msub thick kind wp =
    let r = case kind of
            "stab"  → maybe 0.05 sbsStabResistance  msub
            "slash" → maybe 0.05 sbsSlashResistance msub
            _       → maybe 0.15 sbsBluntResistance msub
        thickF = max 0.0 thick / layerRefThickness
    in case kind of
        "blunt" → r * thickF * bluntAbsorbScale
        _       → r * thickF * cutAbsorbScale / max 0.05 wp

-- | Drive a strike through a body part's tissue stack (outer→inner) and
--   return the wound-weighted energy DEPOSITED in each layer, paired
--   with that layer's tissue name. The strike spends its @budget@
--   crossing each layer; outer layers absorb (and can stop it before the
--   core), while a strike that powers through dumps its remainder into
--   the deepest tissue. Energy deposited ≈ tissue destroyed.
--
--   This is the per-layer decomposition the injury distribution is built
--   from; 'penetrate' sums it back to the scalar severity-driver combat
--   has always used (so that calibration is unchanged by construction).
penetrateDeposits
    ∷ [(Maybe SubstanceDef, Float)] → Float → Float → Text
    → [(Text, Float)]
penetrateDeposits layers0 budget0 wp kind = go layers0 budget0
  where
    go [] _ = []
    go ((msub, thick) : rest) budget
        | budget ≤ 0 = []
        | otherwise =
            let absorb  = layerAbsorb msub thick kind wp
                isLast  = null rest
                dep     = if isLast then budget else min absorb budget
                tissue  = maybe "" sbsName msub
                contrib = woundFactor tissue kind * dep
            in (tissue, contrib)
                 : (if isLast then [] else go rest (budget - dep))

-- | Scalar severity-driver: the wound-weighted energy summed over all
--   layers. Identical to the pre-distribution `penetrate` — combat's
--   severity + wear calibration is preserved.
penetrate ∷ [(Maybe SubstanceDef, Float)] → Float → Float → Text → Float
penetrate layers budget wp kind =
    sum (map snd (penetrateDeposits layers budget wp kind))

-- * Subpart allocation ----------------------------------------------

-- | Choose which subparts a hit on a macro-part injures, by mechanism +
--   how deep it reached. SLASH cuts through ONE structure per depth band
--   (a deep neck slash: windpipe + carotid + spine → sever); STAB drives
--   into one structure in the deepest band reached; BLUNT fractures ONE
--   structural subpart (bone/cartilage — the 50/50 skull-vs-jaw,
--   weighted-random by area weight) AND transmits shock to one deep
--   neural/organ subpart (the concussion / internal injury, regardless of
--   which bone broke). Subparts at the SAME depth are PARALLEL (pick one —
--   limb segments, organs side by side); different depths are in SERIES
--   (a cut passes through them). Always returns at least one subpart so a
--   connecting hit never whiffs. Pure; the @roll@ in [0,1) drives the
--   weighted picks (combat passes its RNG roll; falls a deterministic
--   per-part value).
allocateSubparts ∷ Text → Float → Float → [BodyPart] → [BodyPart]
allocateSubparts _ _ _ [] = []
allocateSubparts kind reach roll subs =
    let reached = filter (\p → bpDepth p ≤ reach) subs
        pool    = if null reached then take 1 (sortOn bpDepth subs) else reached
        bands   = groupBy ((≡) `on` bpDepth) (sortOn bpDepth pool)  -- shallow→deep
        structural p = any (\(_, m, _) → m ≡ "bone" ∨ m ≡ "cartilage") (bpLayers p)
        deepSoft   p = any (\(_, m, _) → m ≡ "nerve" ∨ m ≡ "organ")     (bpLayers p)
        rollFor i = let x = roll * fromIntegral (i + 3) * 1.61803
                    in x - fromIntegral (floor x ∷ Int)
        -- Context label threaded into weightedPick so a (caller-guarded,
        -- so unreachable) empty-pool abort names the allocation path that
        -- violated the invariant — kind, reach, and which sub-pool.
        pick lbl = weightedPick (T.unpack kind ⧺ "/" ⧺ lbl
                                 ⧺ " reach=" ⧺ show reach)
    in case kind of
        "slash" → [ pick ("slash-band" ⧺ show i) (rollFor i) band
                  | (i, band) ← zip [0 ..] bands ]
        "stab"  → case reverse bands of
            (deepest : _) → [ pick "stab-deepest" roll deepest ]
            []            → []
        _       → let struct = filter structural pool
                      soft   = filter deepSoft pool
                      sPick  = case struct of [] → []; _ → [pick "blunt-struct" roll struct]
                      tPick  = case soft   of [] → []; _ → [pick "blunt-soft" (rollFor 1) soft]
                      picks  = sPick ++ tPick
                  -- An all-soft part (no bone/cartilage, no nerve/organ —
                  -- e.g. a hand of skin+fat+muscle subparts) has neither a
                  -- structural nor a deep target; honour the "always ≥1"
                  -- contract by bruising one subpart from the pool.
                  in if null picks then [ pick "blunt-allsoft" roll pool ] else picks

-- | Pick one part weighted by area weight, from a roll in [0,1). Total.
--   The @ctx@ label identifies the allocation path for the abort message
--   (the empty-list case is caller-guarded — bands from groupBy are
--   non-empty, and the stab / structural / soft paths case-match []
--   before calling — so it should never fire).
weightedPick ∷ String → Float → [BodyPart] → BodyPart
weightedPick ctx roll [] = error $ "weightedPick: empty list (ctx=" ⧺ ctx
                                 ⧺ " roll=" ⧺ show roll ⧺ ")"
weightedPick _ roll ps@(p0 : _) =
    let total  = sum (map bpAreaWeight ps)
        target = roll * total
        go _   []          = p0
        go _   [p]         = p
        go acc (p : rest)
            | acc + bpAreaWeight p ≥ target = p
            | otherwise = go (acc + bpAreaWeight p) rest
    in go 0 ps
