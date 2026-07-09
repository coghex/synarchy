{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Strike resolution: the hit-roll skill/evasion terms, the active
--   dodge save, and the joint body-part + wound-kind picker. Split
--   (issue #550) out of "Combat.Resolution"; see that module's haddock
--   for the overall formula summary and resolution flow this feeds.
module Combat.Resolution.Strike
    ( computeAttackerSkill
    , computeDefenderEvasion
    , defenderDodgeChance
    , naturalAttackName
    , pickPartKind
    ) where

import UPrelude
import qualified Data.Text as T
import qualified System.Random as Random
import Combat.Types (AttackMode(..))
import Item.Types (ItemWeapon(..))
import Unit.Types (UnitInstance(..), UnitDef(..), BodyPart(..)
                  , NaturalResistance(..), NaturalWeapon(..)
                  , StrikeProfile(..))
import Combat.Resolution.Common (statOr, skillOr, weightedReachFactor)
import Combat.Resolution.Constants
    ( dodgeBase, dodgeAgiScale, dodgeSkillScale, dodgePainScale
    , dodgeLungeMult, dodgeMaxChance, kindSeverityFactor )

computeAttackerSkill
    ∷ UnitInstance → Maybe ItemWeapon → Maybe NaturalWeapon
    → Float → Float → AttackMode → Float
computeAttackerSkill atk mWeapon natW bladeCm pain mode =
    let wepClass = case mWeapon of
            Just w  → iwWeaponClass w
            Nothing → maybe "unarmed" nwWeaponClass natW
        skill    = skillOr wepClass 0.0 atk
        dex      = statOr "dexterity" 1.0 atk
        perc     = statOr "perception" 1.0 atk
        str      = statOr "strength" 1.0 atk
        bodyMass = statOr "body_mass" 70.0 atk
        strClip  = min 2.0 (str * 0.5)
        massExc  = max 0.0 (bodyMass - 70.0)
        reach    = weightedReachFactor bladeCm
        -- Heavy commits the body forward — you can't redirect mid-swing
        -- the way you can with a quick stab/jab — so the dexterity
        -- contribution halves. The "control" of the swing degrades
        -- proportional to your dex; a high-dex unit pays more for
        -- going heavy than a low-dex unit does (which had less to lose).
        dexMult = case mode of
            Quick → 1.0
            Heavy → 0.5
    in   0.35 * (skill / 100.0)
       + 0.25 * dex * dexMult
       + 0.10 * perc
       + 0.10 * strClip
       + 0.10 * reach
       - 0.001 * massExc
       - 0.30 * pain

computeDefenderEvasion ∷ UnitInstance → Float → Float
computeDefenderEvasion tgt pain =
    let agi      = statOr "agility" 1.0 tgt
        refl     = statOr "reflexes" 1.0 tgt
        -- Balance is a trained skill, not a stat — read from
        -- uiSkills and normalise so level 50 ≈ 1.0 (matches the
        -- old stat-at-mean-1.0 contribution to evasion).
        bal      = skillOr "balance" 50.0 tgt / 50.0
        perc     = statOr "perception" 1.0 tgt
        dex      = statOr "dexterity" 1.0 tgt
        bodyMass = statOr "body_mass" 70.0 tgt
        massExc  = max 0.0 (bodyMass - 70.0)
    in   0.25 * agi
       + 0.25 * refl
       + 0.15 * bal
       + 0.15 * perc
       + 0.10 * dex
       - 0.001 * massExc
       - 0.30 * pain

-- | Active-dodge chance BEFORE the awareness scale (caller multiplies by
--   'Unit.LineOfSight.unitAwareness'). Agility + the learned `dodge`
--   skill drive it; pain slows it; a telegraphed lunge reads easier.
--   Clamped to 'dodgeMaxChance'.
defenderDodgeChance ∷ UnitInstance → Bool → Float → Float
defenderDodgeChance tgt isLunge pain =
    let agi   = statOr "agility" 1.0 tgt
        dodge = skillOr "dodge" 0.0 tgt / 100.0
        base  = dodgeBase
              + dodgeAgiScale   * (agi - 1.0)
              + dodgeSkillScale * dodge
        lungeM = if isLunge then dodgeLungeMult else 1.0
    in clamp 0.0 dodgeMaxChance (base * lungeM - dodgePainScale * pain)

-- | Joint body-part + wound-kind picker. Intelligence-blended:
--
--   * random_score(p, k) = bpAreaWeight[p] × weapon.eff[k]
--     — "what the body and weapon naturally do." Big targets + the
--     weapon's preferred motion dominate.
--   * smart_score(p, k)  = bpTacticalValue[p]
--                        × (1 - target.natural_resistance[k])
--                        × kindSeverityFactor[k]
--                        × bpBleedFactor[p]
--     — "expected damage if I'm thinking it through." Vital parts
--     with low resistance to a sharp-enough kind win, weighted up
--     by bleed potential (a neck cut wins over a head bash).
--   * score(p, k) = random × (1 - intel_factor) + smart × intel_factor
--     where intel_factor = clamp(intelligence / 2.0, 0, 1).
--
--   Weighted-random over all (p, k) pairs where the weapon has
--   non-zero effectiveness for that kind. Single RNG draw.
-- | Display name of the natural attack that delivered a given kind. A
--   combo paw (slash/blunt trigger) reads as the blunt facet's name
--   ("paw"); a stab is the stab facet ("fangs"); a plain slash the slash
--   facet ("claws"). Falls back to "fists" when unnamed.
naturalAttackName ∷ Maybe NaturalWeapon → Text → Bool → Text
naturalAttackName Nothing _ _ = "fists"
naturalAttackName (Just nw) kind combo =
    let nm sp = let n = spName sp in if T.null n then "fists" else n
    in if combo ∧ (kind ≡ "slash" ∨ kind ≡ "blunt")
       then nm (nwBlunt nw)
       else case kind of
           "stab"  → nm (nwStab nw)
           "slash" → nm (nwSlash nw)
           _       → nm (nwBlunt nw)

pickPartKind
    ∷ Random.StdGen → UnitInstance → UnitDef
    → Maybe ItemWeapon → Maybe NaturalWeapon
    → [BodyPart] → ((Text, Text), Random.StdGen)
pickPartKind rng atk tdef mWeapon natW parts =
    let intel = clamp 0.0 1.0 (statOr "intelligence" 1.0 atk / 2.0)
        (wepStab, wepSlash, wepBlunt) = case mWeapon of
            Just w  → (iwStabEff w, iwSlashEff w, iwBluntEff w)
            Nothing → case natW of
                Just nw → ( spEff (nwStab nw)
                          , spEff (nwSlash nw)
                          , spEff (nwBlunt nw) )
                Nothing → (0.0, 0.0, 1.0)
        nres = udNaturalResistance tdef
        kindEff k = case k of
            "stab"  → wepStab
            "slash" → wepSlash
            "blunt" → wepBlunt
            _       → 0.0
        kindResistance k = case k of
            "stab"  → nrStab  nres
            "slash" → nrSlash nres
            "blunt" → nrBlunt nres
            _       → 0.0
        kinds = ["stab", "slash", "blunt"]
        pairs =
            [ ((bpId p, k), randomScore, smartScore)
            | p ← parts
            , k ← kinds
            , let eff = kindEff k
            , eff > 0.0    -- skip kinds the weapon can't deliver
            , let randomScore = bpAreaWeight p * eff
            , let smartScore =
                    bpTacticalValue p
                    * (1.0 - kindResistance k)
                    * kindSeverityFactor k
                    * bpBleedFactor p
            ]
        scored =
            [ (pk, randomScore * (1 - intel) + smartScore * intel)
            | (pk, randomScore, smartScore) ← pairs ]
        total = sum (map snd scored)
        (r, rng') = Random.uniformR (0.0 ∷ Float, max 0.001 total) rng
        -- Safety fallback for `scored == []` (weapon has zero
        -- effectiveness in every kind) or floating-point drift past
        -- the final cumulative weight. Pick a part the target
        -- actually has — literal "torso" was wrong for species that
        -- declare different body-part ids — and the weapon's best
        -- kind, defaulting to "blunt" only when nothing is set.
        fallbackPart = case parts of
            (p:_) → bpId p
            []    → "torso"
        fallbackKind
            | wepBlunt ≥ wepSlash, wepBlunt ≥ wepStab = "blunt"
            | wepSlash ≥ wepStab                      = "slash"
            | otherwise                               = "stab"
        pick _ [] = (fallbackPart, fallbackKind)
        pick acc ((pk, w) : rest)
            | acc + w ≥ r = pk
            | otherwise   = pick (acc + w) rest
    in (pick 0 scored, rng')
