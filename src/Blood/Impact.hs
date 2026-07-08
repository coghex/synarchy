{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Impact blood (#607): the one-shot mark a FRESH wound creates on
--   landing — not the ongoing bleeding-trail system (that's a later
--   issue). Two halves:
--
--   * The pure decision — 'impactBloodForWound' — maps a wound's kind
--     + severity onto a blood request (or 'Nothing'), reusing
--     existing severity signals from elsewhere in the codebase rather
--     than inventing a new "blunt severity" threshold: the T1..T4
--     tier boundaries 'scripts/injury_log.lua' already narrates
--     wounds with (0.25 / 0.50 / 0.85), and
--     'Combat.Wounds.destroyThreshold' (1.0), the existing
--     structural-destruction cutoff.
--   * 'spawnImpactBlood' — the shared IO glue every production wound
--     producer (combat resolution, fall injury, @unit.injure@) calls
--     with the wound's kind/severity/position to place the resulting
--     decal, via the SAME 'Blood.Types.spawnDecal' entry point the
--     #604 debug surface uses.
--
--   See docs/blood_decals.md for the full mapping rationale.
module Blood.Impact
    ( ImpactBlood(..)
    , defaultStyleForWound
    , catastrophicBluntThreshold
    , impactSeverityBucket
    , impactFootprint
    , impactOpacity
    , impactFallbackAngle
    , impactBloodForWound
    , pickImpactWound
    , spawnImpactBlood
    ) where

import UPrelude
import Data.List (maximumBy)
import Data.IORef (readIORef, atomicModifyIORef')
import Combat.Wounds (destroyThreshold)
import Engine.Core.State (EngineEnv(..))
import World.Page.Types (WorldPageId)
import World.State.Types (WorldManager(..), WorldState(..))
import Unit.Types (UnitId)
import Blood.Types

-- | The resolved visual weight of an impact mark, before the caller
--   supplies world position/rotation/seed.
data ImpactBlood = ImpactBlood
    { ibStyle      ∷ !BloodStyle
    , ibSeverity   ∷ !SeverityBucket
    , ibFootprint  ∷ !FootprintBucket
    , ibAnisotropy ∷ !AnisotropyBucket
    , ibOpacity    ∷ !Float
    } deriving (Show, Eq)

-- | Severity at/above which BLUNT-family trauma (blunt/concussion)
--   reads as catastrophic enough to draw blood — reused from
--   'scripts/injury_log.lua's own T4 tier boundary, the point at
--   which that script's narration switches to "crushing" (bone),
--   "pulverizing" (nerve — concussion's own family), or "pulping"
--   (soft tissue). Below this, a blunt hit is "strikes"/"clubs"/
--   "bashes" — no external blood, per issue #607 requirement 5.
catastrophicBluntThreshold ∷ Float
catastrophicBluntThreshold = 0.85

-- | The style a wound kind defaults to. Mirrors what
--   'Engine.Scripting.Lua.API.Blood.bloodSpawnFn' infers for a bare
--   (no explicit style) debug @blood.spawn@ call, so an automatic
--   impact mark and a manually-spawned one for the same wound kind
--   read as the same family of mark — that Lua module reuses this
--   definition instead of keeping its own copy.
defaultStyleForWound ∷ Text → BloodStyle
defaultStyleForWound wk = case wk of
    "stab"       → StylePool
    "slash"      → StyleStreak
    "blunt"      → StyleSpatter
    "concussion" → StyleSpatter
    "fracture"   → StyleSpatter
    "arterial"   → StyleSpatter
    "severed"    → StyleSpatter
    _            → StyleDrops

-- | Severity bucket for the generated mark — mirrors
--   'scripts/injury_log.lua's tier() boundaries (T1<0.25, T2<0.5,
--   T3<0.85, T4>=0.85) so "how strong the mark looks" agrees with
--   "how the wound narrates" (issue #607 requirement 6).
impactSeverityBucket ∷ Float → SeverityBucket
impactSeverityBucket s
    | s ≥ 0.85  = SeverityCatastrophic
    | s ≥ 0.50  = SeveritySevere
    | s ≥ 0.25  = SeverityModerate
    | otherwise = SeverityMinor

-- | Footprint scales with severity too — a graze doesn't paint as
--   wide a mark as a maiming hit.
impactFootprint ∷ Float → FootprintBucket
impactFootprint s
    | s ≥ 0.5   = FootprintLarge
    | s ≥ 0.25  = FootprintMedium
    | otherwise = FootprintSmall

-- | Opacity climbs with severity (0.4 floor so even a minor mark is
--   visible, capped at 1). Wound severity can exceed 1 (falls reach
--   ~1.6 on the shared scale); 'clamp01' caps the result.
impactOpacity ∷ Float → Float
impactOpacity s = clamp01 (0.4 + 0.6 * s)

-- | Deterministic fallback direction (radians) when no real attack
--   direction is available (issue #607 requirement 7) — a pure
--   function of the caller-supplied seed, so the same (unit, part,
--   time) always reads the same way rather than needing real RNG.
impactFallbackAngle ∷ Int → Float
impactFallbackAngle seed =
    let m = abs seed `mod` 3600
    in 2 * pi * fromIntegral m / 3600

-- | Decide whether a fresh wound creates an impact mark, and if so
--   with what visual weight. 'Nothing' covers issue #607's "no direct
--   external ground blood" cases: "internal" always; ordinary
--   blunt/concussion below 'catastrophicBluntThreshold'; ordinary
--   fracture below 'destroyThreshold'. "arterial"/"severed" are
--   floored at 'SeverityModerate' — always at least a strong mark,
--   per requirement 4's "high-volume" framing — but still SCALE
--   further with severity above that floor (requirement 6).
impactBloodForWound ∷ Text → Float → Maybe ImpactBlood
impactBloodForWound kind severity = case kind of
    "internal"   → Nothing
    "blunt"      | severity ≥ catastrophicBluntThreshold → Just mk
                 | otherwise                             → Nothing
    "concussion" | severity ≥ catastrophicBluntThreshold → Just mk
                 | otherwise                             → Nothing
    "fracture"   | severity ≥ destroyThreshold → Just mk
                 | otherwise                   → Nothing
    "stab"       → Just mk
    "slash"      → Just mk
    "arterial"   → Just mk { ibSeverity = max SeverityModerate (ibSeverity mk) }
    "severed"    → Just mk { ibSeverity = max SeverityModerate (ibSeverity mk) }
    _            → Nothing
  where
    style = defaultStyleForWound kind
    mk = ImpactBlood
        { ibStyle      = style
        , ibSeverity   = impactSeverityBucket severity
        , ibFootprint  = impactFootprint severity
        , ibAnisotropy = case style of
              StyleStreak  → AnisotropyHigh
              StyleSpatter → AnisotropyLow
              _            → AnisotropyNone
        , ibOpacity    = impactOpacity severity
        }

-- | Pick the ONE wound (of several a single attack/fall produced) whose
--   impact-blood request should represent the whole event — the shared
--   selection every multi-wound producer uses to satisfy requirement 9
--   ("bounded per event, not per wound") without losing a wound kind's
--   own gating/flooring in the process.
--
--   Ranked by the resulting severity BUCKET first, opacity only as a
--   tiebreaker — NOT by raw input severity, and not by opacity alone.
--   Either of those would undo 'impactBloodForWound's own kind-specific
--   handling: an ordinary wound with merely-higher raw severity would
--   silently outrank a low-nominal-severity arterial/severed wound that
--   should read as the more significant injury (arterial/severed's
--   'SeverityModerate' floor only means anything if the RANKING looks at
--   the floored bucket); worse, selecting purely by raw severity BEFORE
--   checking which wounds actually qualify can pick a non-qualifying
--   wound (say a fracture at 0.9, still under 'destroyThreshold') over a
--   lower-severity wound that DOES qualify (a concussion right at
--   'catastrophicBluntThreshold'), losing the mark entirely even though
--   a qualifying candidate existed.
--
--   'Nothing' when none of the candidates create blood at all.
pickImpactWound ∷ [(Text, Float)] → Maybe (Text, Float, ImpactBlood)
pickImpactWound candidates =
    case [ (k, s, ib) | (k, s) ← candidates, Just ib ← [impactBloodForWound k s] ] of
        []         → Nothing
        qualifying → Just $ maximumBy
            (\(_, _, a) (_, _, b) → compare
                (ibSeverity a, ibOpacity a) (ibSeverity b, ibOpacity b))
            qualifying

-- | Place one impact-blood decal for a fresh wound, or do nothing —
--   either because 'impactBloodForWound' says this wound shouldn't
--   bleed, or because @page@ names a world that isn't currently
--   loaded (issue #607 requirement 8: skip silently, never crash or
--   corrupt the wound that was just created). @rotation@ is radians —
--   the caller resolves real attack direction or
--   'impactFallbackAngle' before calling this (requirement 7).
--   Bounding "one attack/fall = at most one decal" (requirement 9) is
--   the CALLER's job (pick one headline wound per event) — this
--   function always places exactly one decal per call.
spawnImpactBlood
    ∷ EngineEnv
    → WorldPageId
    → Float           -- ^ gx
    → Float           -- ^ gy
    → Int             -- ^ surfaceZ
    → Text            -- ^ wound kind
    → Float           -- ^ wound severity
    → Float           -- ^ rotation, radians
    → Int             -- ^ seed
    → Maybe UnitId    -- ^ source unit
    → Double          -- ^ game time
    → IO ()
spawnImpactBlood env page gx gy z kind severity rotation seed mSrc now =
    case impactBloodForWound kind severity of
        Nothing → pure ()
        Just ib → do
            wm ← readIORef (worldManagerRef env)
            case lookup page (wmWorlds wm) of
                Nothing → pure ()
                Just ws → do
                    let req = BloodTextureRequest
                            { btrStyle      = ibStyle ib
                            , btrWoundKind  = kind
                            , btrSeverity   = ibSeverity ib
                            , btrFootprint  = ibFootprint ib
                            , btrAnisotropy = ibAnisotropy ib
                            , btrEdge       = EdgeModerate
                            , btrSeed       = seed
                            }
                        mkSpec tid = BloodDecalSpec
                            { bspTexture        = tid
                            , bspPage           = page
                            , bspX              = gx
                            , bspY              = gy
                            , bspSurfaceZ       = z
                            , bspOffsetX        = 0
                            , bspOffsetY        = 0
                            , bspRotation       = rotation
                            , bspScale          = 1
                            , bspCreatedAt      = now
                            , bspInitialWetness = 1
                            , bspWoundKind      = kind
                            , bspSeverity       = ibSeverity ib
                            , bspSourceUnit     = mSrc
                            , bspOpacity        = ibOpacity ib
                            }
                    atomicModifyIORef' (wsBloodStoreRef ws) $ \store →
                        let (store', _, _, _) = spawnDecal req mkSpec store
                        in (store', ())
