{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | The Tier 3 real-units damage model: weapon/natural-attack facet
--   resolution, swing kinematics, penetration, and the per-layer
--   tissue-cascade severity calculation. Split (issue #550) out of
--   "Combat.Resolution"; see that module's haddock for the overall
--   formula summary and derivation this implements.
module Combat.Resolution.Damage
    ( ResolvedStrike(..)
    , resolveStrike
    , weaponPenetration
    , resolvePartLayersNamed
    , resolvePartLayers
    , defenderArmor
    , swingKinematics
    , computeSeverity
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Combat.Types (AttackMode(..))
import Item.Types (ItemDef(..), ItemWeapon(..), ItemArmor(..), ItemInstance(..)
                  , ItemManager(..), lookupItemDef)
import Substance.Types (SubstanceManager, SubstanceDef(..), lookupSubstance)
import Unit.Types (UnitInstance(..), UnitDef(..), BodyPart(..)
                  , NaturalResistance(..), NaturalWeapon(..)
                  , StrikeProfile(..))
import Unit.Injury (penetrate, penetrateDeposits, tissueInjuryKind
                   , injuryFloor, capInjurySeverity, allocateSubparts
                   , tissueCapacityWeight, defaultPartCapacity)
import Combat.Resolution.Common (statOr, skillOr, painFor, maxStaminaFor
                                 , bodyPartIndex)
import Combat.Resolution.Constants
    ( eHuman, modeWork, armMassFrac, armLengthFrac, vHuman, modeSpeed
    , refSharpness, refHardness, refShearWeapon
    , energyPerHp, momentumPerHp, lungeMomentumScale
    , capacityHpScale, refBodyMass, refFatMass, refLeanMass, layerHpScale
    , kindSeverityFactor )

-- | A weapon facet resolved for ONE attack kind: material properties +
--   geometry + per-instance state, unified across manufactured and
--   natural weapons. The Tier 3 formula only ever sees one of these —
--   it doesn't know or care whether the source was a steel dagger or a
--   set of keratin claws.
data ResolvedStrike = ResolvedStrike
    { rsSub        ∷ !(Maybe SubstanceDef)  -- ^ material (Nothing ⇒ unknown)
    , rsBladeCm    ∷ !Float                 -- ^ edge/point length (stab, slash)
    , rsSharpness  ∷ !Float                 -- ^ lower = sharper (stab, slash)
    , rsImpactArea ∷ !Float                 -- ^ mm² contact patch (blunt)
    , rsMass       ∷ !Float                 -- ^ kg mass of the striking head
    , rsLength     ∷ !Float                 -- ^ cm; lever length of the implement
    , rsCoM        ∷ !Float                 -- ^ 0..1 centre-of-mass along length
    , rsEff        ∷ !Float                  -- ^ 0..1 weapon suitability for kind
    , rsQuality    ∷ !Float                  -- ^ 0..1 build quality
    , rsCondition  ∷ !Float                  -- ^ 0..1 wear
    }

-- | Resolve the strike for the chosen kind from whichever weapon the
--   attacker is using.
resolveStrike
    ∷ SubstanceManager
    → Maybe (ItemInstance, ItemDef, ItemWeapon) → Maybe NaturalWeapon
    → Text → Float → ResolvedStrike
resolveStrike sm mEquipped natW kind bodyMass = case mEquipped of
    Just (inst, idef, w) →
        let eff0 = case kind of
                "stab"  → iwStabEff w
                "slash" → iwSlashEff w
                _       → iwBluntEff w
            -- Effective sharpness: the def's base value, dulled by the
            -- instance's edge wear (100 = factory edge → base value;
            -- lower → a higher value = duller = less penetration).
            sharpFrac = clamp 10.0 100.0 (iiSharpness inst)
            effSharp0 = iwBaseSharpness w * (100.0 / sharpFrac)
            -- Broken (condition 0): a jagged stub — dull and feeble, but
            -- still in hand (per design: stays equipped, near-useless).
            broken    = iiCondition inst ≤ 0
            effSharp  = if broken then effSharp0 * 4.0 else effSharp0
            eff       = if broken then eff0 * 0.15 else eff0
        in ResolvedStrike
            { rsSub        = lookupSubstance (idMaterial idef) sm
            , rsBladeCm    = iwBladeLength w
            , rsSharpness  = effSharp
            -- Manufactured weapons don't declare a blunt contact patch;
            -- approximate from head mass (heavier ⇒ broader striking face).
            , rsImpactArea = max 1.0 (iiWeight inst * 50.0)
            , rsMass       = iiWeight inst
            , rsLength     = if iwLength w > 0 then iwLength w else iwBladeLength w
            , rsCoM        = iwCenterOfMass w
            , rsEff        = eff
            , rsQuality    = clamp 0.0 1.0 (iiQuality inst / 100.0)
            , rsCondition  = clamp 0.0 1.0 (iiCondition inst / 100.0)
            }
    Nothing → case natW of
        Just nw →
            let sp = case kind of
                    "stab"  → nwStab nw
                    "slash" → nwSlash nw
                    _       → nwBlunt nw
            in ResolvedStrike
                { rsSub        = lookupSubstance (spMaterial sp) sm
                , rsBladeCm    = spBladeCm sp
                , rsSharpness  = if spSharpness sp > 0 then spSharpness sp else 1000.0
                , rsImpactArea = if spImpactArea sp > 0 then spImpactArea sp else 30.0
                -- Appendage (paw/fang) head mass; the swinging LIMB mass
                -- is inferred from body_mass in the kinematics, so this
                -- is just the striking head (small).
                , rsMass       = if spMass sp > 0 then spMass sp else bodyMass * 0.015
                , rsLength     = if spLength sp > 0 then spLength sp
                                 else max 5.0 (spBladeCm sp)
                , rsCoM        = spCenterOfMass sp
                , rsEff        = spEff sp
                , rsQuality    = 1.0    -- natural weapons are "as grown"
                , rsCondition  = 1.0
                }
        -- Bare unarmed, no natural weapon (e.g. a human fist): a soft,
        -- dull blunt strike through flesh.
        Nothing → ResolvedStrike
            { rsSub = lookupSubstance "flesh" sm, rsBladeCm = 0.0
            , rsSharpness = 1000.0, rsImpactArea = 60.0
            , rsMass = bodyMass * 0.01, rsLength = 8.0, rsCoM = 0.5
            , rsEff = 0.3, rsQuality = 1.0, rsCondition = 1.0 }

-- | Weapon penetrating power for cut/pierce: how cheaply the strike
--   opens a channel through tissue (divides each layer's absorb cost).
--   A keen, hard/strong, well-kept, well-made edge penetrates more.
--   >1 = better than the reference weapon. Blunt ignores this (it
--   crushes, it doesn't cut a channel).
weaponPenetration ∷ ResolvedStrike → Text → Float
weaponPenetration rs kind =
    let hardness = maybe 0.0 sbsHardness      (rsSub rs)
        shear    = maybe 0.0 sbsShearStrength (rsSub rs)
        -- rsSharpness is the EFFECTIVE sharpness (the def's base value
        -- already dulled by the instance's edge wear, in resolveStrike),
        -- so condition no longer enters here — a cracked-but-keen blade
        -- still cuts; a dull one doesn't.
        sharpF   = refSharpness / max 1.0 (rsSharpness rs)
        qualityF = 0.6 + 0.4 * rsQuality rs
        matF     = case kind of
            "stab"  → sqrt (max 0.0 hardness / refHardness)   -- hard tip
            "slash" → sqrt (max 0.0 shear    / refShearWeapon) -- strong edge
            _       → 1.0
    in max 0.05 (sharpF * matF * qualityF)

-- | Resolve a body part's tissue layers (outer→inner), keeping each
--   layer's DISPLAY NAME alongside the resolved substance + thickness so
--   the combat log can name the structures the strike crossed.
resolvePartLayersNamed ∷ SubstanceManager → UnitDef → Text
                       → [(Text, Maybe SubstanceDef, Float)]
resolvePartLayersNamed sm tdef partId =
    let raw = case HM.lookup partId (bodyPartIndex tdef) of
                Just bp | not (null (bpLayers bp)) → bpLayers bp
                _                                  → [("flesh", "flesh", 40.0)]
    in [ (nm, lookupSubstance m sm, t) | (nm, m, t) ← raw ]

-- | Substance + thickness only — the physics path (penetrate) doesn't
--   need the display names.
resolvePartLayers ∷ SubstanceManager → UnitDef → Text
                  → [(Maybe SubstanceDef, Float)]
resolvePartLayers sm tdef partId =
    [ (msub, t) | (_, msub, t) ← resolvePartLayersNamed sm tdef partId ]

-- | Intact worn armour covering the struck part — the outer layers to
--   prepend to its tissue stack (and the instances to wear). Broken
--   armour (condition 0) no longer protects, so it's excluded.
defenderArmor ∷ SubstanceManager → ItemManager → UnitInstance → Text
              → [(Text, ItemInstance, Maybe SubstanceDef, Float)]
defenderArmor sm im tgt partId =
    [ (slot, it, lookupSubstance (idMaterial d) sm, iaThickness a)
    | (slot, it) ← HM.toList (uiEquipment tgt)
    , Just d ← [lookupItemDef (iiDefName it) im]
    , Just a ← [idArmor d]
    , partId `elem` iaCovers a
    , iiCondition it > 0
    ]

-- | Swing kinematics as a rotating lever about the shoulder/pivot. The
--   limb (arm/foreleg) is a rod of @armMass@ × @armLen@; the implement
--   is a point mass @wMass@ at its centre of mass (@wCoM@ along its
--   @wLen@), held at the end of the limb. Muscular @work@ (J) spins the
--   system: @ω = √(2·work/I)@, capped by how fast the limb tip can move
--   (@vMax@). Contact is at the tip (radius @R = armLen + wLen@).
--
--   Returns @(kineticEnergy [J], momentum [kg·m/s])@ — stab/slash spend
--   the energy, blunt spends the momentum. All lengths in cm (converted
--   to m here so energy lands in joules).
--
--   Centre of mass matters: a head-heavy implement (wCoM→1) sits at a
--   larger radius → larger moment of inertia → for the same work, lower
--   ω but MORE momentum (@p = √(2·work·I)/R@) than a balanced one of
--   equal mass. That's why a hammer crushes harder than an equal-weight
--   pipe.
swingKinematics
    ∷ Float    -- ^ muscular work (J)
    → Float    -- ^ implement mass (kg)
    → Float    -- ^ implement length (cm)
    → Float    -- ^ implement centre of mass (0..1 from grip)
    → Float    -- ^ limb (arm) length (cm)
    → Float    -- ^ limb (arm) mass (kg)
    → Float    -- ^ dexterity
    → AttackMode
    → (Float, Float)
swingKinematics work wMass wLen wCoM armLen armMass dexterity mode =
    let lw   = wLen   / 100.0                       -- m
        la   = armLen / 100.0                       -- m
        rCoM = la + clamp 0.0 1.0 wCoM * lw          -- implement CoM radius
        bigR = max 0.05 (la + lw)                    -- contact (tip) radius
        iArm = (1.0 / 3.0) * armMass * la * la        -- rod about one end
        iWep = wMass * rCoM * rCoM                    -- point mass at CoM
        inertia = max 1.0e-4 (iArm + iWep)
        vMax = vHuman * modeSpeed mode * (0.6 + 0.4 * dexterity)
        omegaMax = vMax / bigR
        omega = min omegaMax (sqrt (2.0 * work / inertia))
        vTip  = omega * bigR
        mEff  = inertia / (bigR * bigR)
    in (0.5 * inertia * omega * omega, mEff * vTip)

-- | Severity of one landed blow, via the Tier 3 kinematic swing +
--   layered-penetration target model. Returns @(severity, driver,
--   sevDriver, load, weaponHardness)@ — @driver@ is the swing's raw
--   energy (J, stab/slash) or momentum (kg·m/s, blunt); @sevDriver@ the
--   tissue-weighted damage that survived the layer stack; @load@ the
--   reaction stress on the weapon (feeds 'Combat.Resolution.Wear.weaponWear');
--   @weaponHardness@ the attacker material's hardness (feeds armour wear).
computeSeverity
    ∷ SubstanceManager → ItemManager
    → UnitInstance → UnitDef
    → Maybe (ItemInstance, ItemDef, ItemWeapon) → Maybe NaturalWeapon
    → UnitInstance → Text → Text → AttackMode → Float → Float → Float
    → ( Float, Float, Float, Float, Float
      , [(Text, Text, Float)]            -- wound distribution (subpart, kind, sev)
      , [(Text, Text, Text, Float)] )    -- log detail (subpartName, layer, material, sev)
-- The trailing Floats are `allocRoll`, `kindWeight` (the fraction of the
-- swing's energy delivered as THIS mechanism — 1.0 for a single-kind attack;
-- a combo "paw" splits across slash/blunt/stab components that each call in),
-- and `lungeSpeed` (m/s of the attacker's body at impact — 0 for a normal
-- swing; a lunge adds its full-body momentum on top of the swing).
computeSeverity sm im atk tdef mEquipped natW tgt partId kind mode allocRoll kindWeight lungeSpeed =
    let -- Muscular work committed to the swing (J).
        str      = statOr "strength" 1.0 atk
        wepClass = case mEquipped of
            Just (_, _, w) → iwWeaponClass w
            Nothing        → maybe "unarmed" nwWeaponClass natW
        skill    = skillOr wepClass 0.0 atk
        skillEff = 0.6 + 0.4 * clamp 0.0 1.0 (skill / 100.0)
        -- A winded fighter still hits (floor 0.3); absent stamina ⇒ full.
        staminaFrac = case HM.lookup "stamina" (uiStats atk) of
            Nothing → 1.0
            Just s  → let maxS = maxStaminaFor atk
                      in if maxS ≤ 0 then 1.0 else clamp 0.3 1.0 (s / maxS)
        pain     = painFor atk
        work     = eHuman * str * modeWork mode * skillEff * staminaFrac
                          * (1.0 - pain)

        -- Resolve the weapon facet for this kind.
        bodyMassA = statOr "body_mass" 70.0 atk
        dexA      = statOr "dexterity" 1.0 atk
        heightA   = statOr "height" 1.8 atk           -- metres
        strike    = resolveStrike sm mEquipped natW kind bodyMassA

        -- Swing kinematics: the implement on the lever of an inferred
        -- limb (mass ≈ 5% body, length ≈ 0.4·height). Yields the
        -- delivered energy + momentum.
        armLen  = armLengthFrac * heightA * 100.0     -- cm
        armMass = armMassFrac * bodyMassA             -- kg
        (eKin, pImp) = swingKinematics work (rsMass strike) (rsLength strike)
                                       (rsCoM strike) armLen armMass dexA mode

        -- LUNGE: the whole body slams in at `lungeSpeed` (m/s) on top of the
        -- limb's swing — FULL body mass, not the limb's. Adds ½·m·v² of
        -- energy (stab/slash channel) and m·v of momentum (blunt channel),
        -- which then flow through the SAME attenuation as the swing below
        -- (weapon eff, quality, hide, toughness, and kindWeight via budget).
        -- 0 lungeSpeed ⇒ no contribution (an ordinary swing). Scaled so a
        -- leap is a heavy hit without being a guaranteed one-shot.
        lungeKE = lungeMomentumScale * 0.5 * bodyMassA * lungeSpeed * lungeSpeed
        lungeP  = lungeMomentumScale * bodyMassA * lungeSpeed

        -- Stab/slash spend ENERGY; blunt spends MOMENTUM.
        (driver, perHp) = if kind ≡ "blunt"
                          then (pImp + lungeP,  momentumPerHp)
                          else (eKin + lungeKE, energyPerHp)

        -- Weapon suitability + build set how much of the driver enters
        -- the body; the creature's hide (per-unit natural_resistance) is
        -- the outermost attenuation layer; toughness a general resilience.
        --
        -- INTENTIONAL — do NOT "fix" as double-counting: build quality
        -- (rsQuality) deliberately enters TWO distinct physical channels.
        -- (1) here in 'budget' (energy/momentum coupling into the body),
        -- and (2) again inside 'weaponPenetration' below (how cleanly a
        -- cutting EDGE concentrates that energy to open a channel). The
        -- penetration channel is edged-only — 'layerAbsorb' ignores wp
        -- for blunt — so quality compounds for stab/slash but applies
        -- only once for blunt. That asymmetry is the point: a blade gets
        -- markedly worse as quality drops, but a low-quality club is
        -- still a club (it crushes with its mass regardless of finish).
        -- weaponPenetration's quality term is pinned by a unit test
        -- ("a better-made weapon penetrates more", Combat/Damage.hs).
        qualityF = 0.6 + 0.4 * rsQuality strike
        natRes = case kind of
            "slash" → nrSlash (udNaturalResistance tdef)
            "stab"  → nrStab  (udNaturalResistance tdef)
            "blunt" → nrBlunt (udNaturalResistance tdef)
            _       → 0.0
        toughness = statOr "toughness" 1.0 tgt
        toughCut  = clamp 0.0 0.5 (toughness * 0.05)
        budget = driver * rsEff strike * qualityF
                        * (1.0 - natRes) * (1.0 - toughCut) * kindWeight

        -- Drive it through the struck part's tissue stack — with any
        -- worn armour covering that part prepended as the OUTERMOST
        -- layer(s), so the strike must beat the armour before tissue.
        wp         = weaponPenetration strike kind
        armorLayers = [ (msub, th)
                      | (_, _, msub, th) ← defenderArmor sm im tgt partId ]

        -- Body composition → per-LAYER thickness. The yaml authors
        -- REFERENCE-HUMAN base thicknesses (70 kg, 15 % fat); each layer is
        -- scaled to the actual target: skin/bone/organ by linear size
        -- (∛mass), fat additionally by relative bodyfat, muscle by relative
        -- leanness. A 280 kg bear thus grows a thick fat/muscle barrier from
        -- the SAME template a human uses — "thickness from fat/muscle mass".
        -- The seeded body-composition stats (body_mass = 22·h²·bulk,
        -- fat_mass, lean_mass = skeletal muscle). Fat/muscle layer thickness
        -- is the tissue mass spread over the body's surface area
        -- (∝ mass^⅔); skin/bone/organ scale with linear size (∝ mass^⅓).
        -- Normalised to a reference acolyte so the yaml authors human-scale
        -- thicknesses and every other species derives from its own mass.
        bodyMassT = statOr "body_mass" refBodyMass tgt
        fatMassT  = statOr "fat_mass"  (0.20 * bodyMassT) tgt
        leanMassT = statOr "lean_mass" (0.40 * bodyMassT) tgt
        surfArea m  = m ** (2.0/3.0)
        sizeF       = (bodyMassT / refBodyMass) ** (1.0/3.0)
        fatScale    = (fatMassT  / surfArea bodyMassT) / (refFatMass  / surfArea refBodyMass)
        muscleScale = (leanMassT / surfArea bodyMassT) / (refLeanMass / surfArea refBodyMass)
        layerThickScale mat = case mat of
            "fat"    → fatScale
            "muscle" → muscleScale
            _        → sizeF
        scalePair (msub, th) =
            (msub, th * layerThickScale (maybe "" sbsName msub))
        scaledLayers pid = map scalePair (resolvePartLayers sm tdef pid)

        -- Per-tissue-stack injury distribution. Each layer's wound severity
        -- is normalised by THAT LAYER'S own capacity (capWeight·thickness),
        -- not the whole part — so a dagger that fully pierces thin skin
        -- registers a real skin laceration even behind a 150 kg of bear,
        -- while the deep organ (its capacity ≈ the whole part) still needs a
        -- powerful strike. The whole-part scalar `sev` below (death / reach)
        -- is unchanged, so lethality calibration is preserved.
        layerHpMat mat th =
            max 0.05 (tissueCapacityWeight mat * th * layerHpScale)
        -- Each wound KIND is normalised by the summed capacity of the
        -- layers that produced it: the soft-tissue cut over all the
        -- skin/fat/muscle it crossed, the fracture over the bone, etc. (Not
        -- per-individual-layer — that double-counts a thin layer's tiny
        -- capacity and would let a graze "destroy" the skin.)
        distOf layerSet =
            let deps = penetrateDeposits layerSet budget wp kind
                perKind = HM.fromListWith
                    (\(c1, h1) (c2, h2) → (c1 + c2, h1 + h2))
                    [ (k, (c * kindSeverityFactor kind, layerHpMat tissue th))
                    | ((_, th), (tissue, c)) ← zip layerSet deps
                    , Just k ← [tissueInjuryKind tissue kind] ]
            in [ (k, capInjurySeverity k s)
               | (k, (csum, hsum)) ← HM.toList perKind
               , let s = csum / (max 0.05 hsum * perHp)
               , capInjurySeverity k s ≥ injuryFloor ]
        -- Part "max HP" — the severity normaliser — DERIVED from the part's
        -- COMPOSITION-SCALED tissue layers. Size enters through the layer
        -- THICKNESSES (above), not a separate body-mass multiplier — the old
        -- `massScale` factor double-counted it and made a big animal a sponge
        -- (a 280 kg bear was 4× HP, so a dagger couldn't register a wound).
        partHpId pid =
            let cap = sum [ tissueCapacityWeight (maybe "" sbsName msub) * th
                          | (msub, th) ← scaledLayers pid ]
                cap' = if cap ≤ 0 then defaultPartCapacity * sizeF else cap
            in max 0.5 (cap' * capacityHpScale)

        -- MACRO-part severity (the strike's reach + the death/wear scalar),
        -- on the macro-part's representative tissue stack.
        macroLayers = armorLayers ++ scaledLayers partId
        sevDriver   = penetrate macroLayers budget wp kind

        -- Reaction load on the weapon: strike force × target-vs-weapon
        -- hardness (feeds wear). `driver` is energy (cut) or momentum
        -- (blunt) — intentionally one wear proxy for both.
        weaponHardness = maybe 1.0 sbsHardness (rsSub strike)
        targetHardness = maximum (1.0 : [ maybe 0.0 sbsHardness msub
                                        | (msub, _) ← macroLayers ])
        load = driver * clamp 0.0 2.0 (targetHardness / max 1.0 weaponHardness)

        sev = sevDriver * kindSeverityFactor kind / (partHpId partId * perHp)
        -- Reach: how deep the strike penetrates (0 = surface, 1 = deepest
        -- subpart), from the macro severity. A solid hit reaches the deep
        -- structures; a glance only the shallow ones.
        reach = clamp 0.0 1.0 sev

        -- ALLOCATION to subparts. A hit on the macro-part is distributed to
        -- its (non-targetable) subparts; each runs its OWN tissue cascade
        -- and the wounds are stored on the subpart. A body plan with no
        -- subparts falls back to one wound-set on the macro-part itself
        -- (unchanged behaviour). The WHICH-subpart is weighted-random
        -- ("aim within the part" — the 50/50 skull/jaw), severity stays
        -- deterministic physics.
        subparts = [ p | p ← udBodyParts tdef
                       , bpParent p ≡ Just partId, not (bpTargetable p) ]
        selected = allocateSubparts kind reach allocRoll subparts
        subInjuries p =
            let sl = armorLayers ++ scaledLayers (bpId p)
            in [ (bpId p, k, s) | (k, s) ← distOf sl ]
        -- Scale a named layer's thickness the same way scalePair does.
        scaleNamed (nm, msub, th) =
            (nm, msub, th * layerThickScale (maybe "" sbsName msub))
        injuries = if null subparts
                   then [ (partId, k, s) | (k, s) ← distOf macroLayers ]
                   else concatMap subInjuries selected

        -- PER-LAYER log detail (for the combat-log narration): every named
        -- tissue layer the strike actually crossed in each injured subpart,
        -- with its display name + material + severity. (The `injuries` above
        -- aggregate by kind for the wound list; the log wants the layers
        -- individually — skin, fat, muscle, radius, ulna — so it can say
        -- "lacerating the forearm's skin, fat, and muscle and breaking the
        -- radius".) Returns (subpartDisplayName, layerName, material, sev).
        bpNameOf pid = maybe pid bpName (HM.lookup pid (bodyPartIndex tdef))
        -- One layer-detail path for both the per-subpart case (called with
        -- each selected subpart's id) and the no-subpart macro fallback
        -- (called with the macro part id). The two used to be structural
        -- clones differing only in which part id they resolved.
        layerDetailOfPart pid =
            let named = map scaleNamed (resolvePartLayersNamed sm tdef pid)
                armorNamed = [ ("armor", msub, th) | (msub, th) ← armorLayers ]
                full  = armorNamed ++ named
                deps  = penetrateDeposits [ (s, t) | (_, s, t) ← full ] budget wp kind
                sub   = bpNameOf pid
            in [ (sub, lname, lmat, capInjurySeverity k sv)
               | ((lname, _, th), (lmat, contrib)) ← zip full deps
               , Just k ← [tissueInjuryKind lmat kind]
               , let sv = capInjurySeverity k
                            (contrib * kindSeverityFactor kind
                               / (layerHpMat lmat th * perHp))
               , sv ≥ injuryFloor ]
        logDetail = if null subparts
                    then layerDetailOfPart partId
                    else concatMap (layerDetailOfPart . bpId) selected
    in (clamp 0.0 1.0 sev, driver, sevDriver, load, weaponHardness, injuries, logDetail)
