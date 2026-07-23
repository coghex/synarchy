{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Wound and scar records, split out of "Unit.Types" (#575) — re-exported
--   there so the public API is unchanged.
module Unit.Types.Wound
    ( Wound(..)
    , woundEffSeverity
    , bloodMassRatio
    , Scar(..)
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

-- | A single wound record on a unit. Slim by design: every derived
--   property (bleed rate, heal rate, pain contribution) is computed
--   live from `woundKind` + `woundSeverity` + body-part metadata, so
--   the schema doesn't have to expand when those formulas change.
--
--   Severity is continuous 0..1; ≥1.0 on a vital part triggers
--   instant death; wounds with severity below 0.01 are removed by
--   the wound tick. `woundAt` lets the UI sort newest-first and
--   show "X seconds ago" labels.
data Wound = Wound
    { woundPart     ∷ !Text
      -- ^ body-part id (matches a `BodyPart.bpId` in the unit-def).
    , woundKind     ∷ !Text
      -- ^ "slash" / "stab" / "blunt" — drives bleed_factor, pain
      --   factor, healing characteristics, and combat-log flavor.
    , woundSeverity ∷ !Float
      -- ^ inflicted severity 0..1, static for the wound's lifetime.
      --   The EFFECTIVE severity used elsewhere (pain, bleed,
      --   impairment) is `woundSeverity × (1 - woundHeal)`, which
      --   eases as `woundHeal` advances. A new hit always appends a
      --   fresh 'Wound'; existing wounds on the same part aren't
      --   merged.
    , woundAt       ∷ !Double
      -- ^ gameTime (seconds) when inflicted.
    , woundBandage  ∷ !Float
      -- ^ first-aid bleed multiplier: the FRACTION of this wound's
      --   natural bleed that still seeps after dressing. 1.0 =
      --   untreated (full bleed, the default for a fresh wound); 0.05
      --   = a competent bandage (5% seeps); 0.0 = a perfect dressing
      --   that stops the bleed entirely. Set by `unit.treatBleeding`
      --   (Combat.Wounds multiplies the per-wound bleed by this). A
      --   well-dressed wound's effective bleed drops below the clot
      --   threshold, so it then heals naturally.
    , woundClot     ∷ !Float
      -- ^ clotting progress 0..1. Fills over time in the wound tick
      --   (Combat.Wounds); the per-wound bleed is multiplied by
      --   (1 − woundClot), so at 1.0 the bleed has stopped entirely.
      --   Starts at 0 on a fresh wound. The clot rate ACCELERATES as
      --   it forms, scales DOWN with severity and dangerous kinds
      --   (arterial/severed barely self-clot), and is boosted by a
      --   bandage (pressure). Distinct from healing: clotting stops the
      --   bleed, healing then mends the tissue.
    , woundHeal     ∷ !Float
      -- ^ healing progress 0..1. Fills slowly in the wound tick once
      --   the wound has clotted (the rate scales with woundClot; an
      --   open wound barely heals). woundSeverity is the INFLICTED
      --   severity (static); the wound's EFFECTIVE severity — what
      --   drives bleed magnitude, pain, and impairment — is
      --   woundSeverity × (1 − woundHeal), so a unit recovers function
      --   as it heals. When effective severity falls below cleanup
      --   threshold the wound is removed, leaving a Scar if it was
      --   severe enough. Severed wounds don't heal (the limb is gone).
    , woundDressing ∷ !Text
      -- ^ what first-aid is on the wound: "" (none), "bandage" (a
      --   proper kit dressing), or "tourniquet" (the improvised
      --   no-supplies fallback — crude, poor seep). Cosmetic/record:
      --   the seep that actually scales bleed is woundBandage. Set by
      --   `unit.treatBleeding`; drives the Status-tab label.
    , woundInfection ∷ !Float
      -- ^ infection level 0..1. Grows in the wound tick (Combat.Wounds)
      --   on an open, undressed wound — proportional to severity and the
      --   wound kind (deep/dirty stab/severed worst; closed
      --   fracture/concussion least). An infected wound barely heals and,
      --   past a threshold, WORSENS (woundHeal reverses → effective
      --   severity climbs above the inflicted value). DETERMINISTIC: a
      --   wound marked `woundClean` (disinfected with antiseptic) never
      --   grows it. Drives the systemic SEPSIS failure-meter (a delayed
      --   death pathway) and is the thing ANTIBIOTICS cure.
    , woundClean   ∷ !Bool
      -- ^ disinfected: True once antiseptic has been applied (during
      --   treatBleeding, or the antibiotics cure). A clean wound does not
      --   accumulate `woundInfection`. This is the PREVENTION half of the
      --   medical loop — promptly cleaning a wound stops infection before
      --   it starts; antibiotics are the CURE for one that already set in.
    , woundInfectionType ∷ !Text
      -- ^ which infection took hold, as an InfectionDef id ("staph",
      --   "clostridium", …) or "" before any. Selected (weighted-random
      --   by the wound's site + the local climate) the moment infection
      --   first festers; drives the per-type growth aggressiveness, the
      --   cure rule (antibiotics treat only `category: bacterial`), and the
      --   display name/icon. Data-driven from data/infections/*.yaml.
    , woundNecrosis  ∷ !Float
      -- ^ dead tissue 0..1 from a NECROTIC infection (one whose def has the
      --   "necrosis" effect — gangrene). Accrues while such an infection is
      --   established (∝ infection level; "gas" rots faster) and is
      --   PERMANENT (dead tissue never recovers — clearing the infection
      --   only stops further rot). Raises effective severity; at 1.0 a
      --   non-vital part rots off (severing cascade), a vital part → death
      --   by gangrene. Future: a debridement action removes it.
    } deriving (Show, Eq, Generic, Serialize)

-- | The authoritative EFFECTIVE severity of a wound — what actually
--   drives bleed magnitude, pain, impairment, medic priority, and the
--   injured-animation flag. Healing eases it (@sev × (1 − heal)@, which
--   climbs back above the inflicted value when a festering wound's heal
--   goes negative), while necrosis (dead tissue) is a PERMANENT floor:
--   a rotting wound is at least as bad as the fraction of tissue that
--   has died. Mirrors the per-tick @effSev@ in
--   'Combat.Wounds.tickOneUnit' (the source of truth) so every
--   downstream consumer stays in lockstep with it.
woundEffSeverity ∷ Wound → Float
woundEffSeverity w =
    max (woundSeverity w * (1 - woundHeal w)) (woundNecrosis w)

-- | Blood volume as a fraction of body mass (litres per kg) — the
--   real-world ~7.5 % ratio. Single source of truth for every
--   max-blood computation (spawn seed, wound tick, speed penalty,
--   Lua info panel); max_blood is always recomputed live from the
--   CURRENT body_mass so wasting/regrowth carries through.
bloodMassRatio ∷ Float
bloodMassRatio = 0.075

-- | A healed-over wound left as a permanent mark. Descriptive record
--   for now (shown in the unit's Status tab); the data is here to hang
--   visuals or minor gameplay effects on later. Created when a wound
--   finishes healing and its inflicted severity was above the scar
--   threshold (scratches heal clean).
data Scar = Scar
    { scarPart     ∷ !Text     -- ^ body-part id the wound was on
    , scarKind     ∷ !Text     -- ^ the wound kind that left it (slash/…)
    , scarSeverity ∷ !Float     -- ^ the wound's INFLICTED severity (how bad)
    , scarAt       ∷ !Double    -- ^ gameTime when it finished healing
    } deriving (Show, Eq, Generic, Serialize)
