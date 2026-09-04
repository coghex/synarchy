{-# LANGUAGE Strict #-}

-- | Commit-time admission checks for one queued strike (#2328).
--
--   `combat.attack` only ENQUEUES; the combat worker drains the queue
--   on its own 60 Hz tick, so every precondition the Lua AI checked at
--   admission ('scripts/unit_ai_combat_attack.lua') is up to a tick
--   stale by the time "Combat.Resolution" commits effects. Those Lua
--   gates remain the admission and animation drivers they always were,
--   but they are ADVISORY: this module is the authority, re-read from
--   the live 'UnitInstance's immediately before the strike commits.
--
--   Three preconditions are physical rather than tactical, so they are
--   the ones re-checked here:
--
--     * SAME PAGE — the #1673 floor every other cross-entity verb
--       already holds. #797 made 'Unit.LineOfSight.unitAwareness'
--       resolve from the unit's own page and zeroed it across pages,
--       which made a cross-page strike UNDODGEABLE rather than
--       impossible; refusing admission is the other half of that fix.
--     * HORIZONTAL REACH — the vertical reach band inside
--       'Combat.Resolution.runResolution' only picks which body parts
--       are targetable; nothing ever consulted the separation.
--     * STANCE — 'Combat.Resolution.Wear.applyStaminaDrain' SPENDS
--       stance at resolution and floors it at 0, so two swings admitted
--       against one stance budget could both commit.
--
--   Cooldown, faction permission and the collapsed-but-living pose are
--   deliberately NOT here: they are pacing and policy questions the AI
--   and the UI own, not physical preconditions (#2328 out-of-scope).
module Combat.Resolution.Admission
    ( AttackRefusal(..)
    , refusalReason
    , attackRangeTiles
    , noHeightAttackRange
    , chebyshevSeparation
    , attackerStance
    , checkAdmission
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Combat.Types (AttackMode(..))
import Combat.Resolution.Constants (stanceAttackCost)
import Unit.Types (UnitInstance(..))

-- | Why a queued strike was refused at commit. Each maps to one stable
--   'refusalReason' the combat log and any other event consumer can
--   distinguish from a miss — a strike that never happened is not a
--   swing that missed.
data AttackRefusal
    = RefusedDifferentPage
      -- ^ attacker and target are no longer on the same world page
    | RefusedOutOfReach
      -- ^ horizontal separation exceeds the request's own reach
    | RefusedInsufficientStance
      -- ^ the attacker cannot pay the requested mode's stance cost
    deriving (Show, Eq)

-- | The stable payload token for a refusal. These strings are part of
--   the combat-event vocabulary ('scripts/combat_log.lua' formats each
--   one distinctly); rename one only with its consumers.
refusalReason ∷ AttackRefusal → Text
refusalReason RefusedDifferentPage      = "different_page"
refusalReason RefusedOutOfReach         = "out_of_reach"
refusalReason RefusedInsufficientStance = "insufficient_stance"

-- | Melee reach in TILES: `(height / 2.4) + (blade_length / 100)`,
--   height in metres and blade length in centimetres.
--
--   This is the ONE definition of the measure the AI admits a swing
--   on. 'Engine.Scripting.Lua.API.Units.Query.unitGetAttackRangeFn'
--   (`unit.getAttackRange`) reports it to Lua and
--   'Combat.Resolution.resolveAttack' revalidates against it, so the
--   admission gate and the commit gate cannot drift apart.
attackRangeTiles ∷ Float → Float → Float
attackRangeTiles heightM bladeCm = heightM / 2.4 + bladeCm / 100.0

-- | What a caller gets when the unit has no rolled `height` stat.
--   `unit.getAttackRange` returns nil there and every Lua call site
--   spells `or 1.0`, so the commit gate has to fall back to the same
--   number or it would refuse strikes the AI legitimately admitted.
noHeightAttackRange ∷ Float
noHeightAttackRange = 1.0

-- | Horizontal separation in tiles, the Chebyshev metric
--   `unit_ai_combat_attack.lua` admits on — deliberately the same
--   raw-coordinate difference the AI computes from `unit.getInfo`'s
--   `gridX`/`gridY`, so a gate that passed at admission cannot fail
--   here for a reason the AI could never have seen. (Cross-seam
--   separation is not localized, for exactly that reason: the AI does
--   not localize it either, and a commit gate stricter than its own
--   admission gate would refuse live strikes.)
chebyshevSeparation ∷ UnitInstance → UnitInstance → Float
chebyshevSeparation a b =
    max (abs (uiGridX a - uiGridX b)) (abs (uiGridY a - uiGridY b))

-- | The attacker's live stance. Absent ⇒ 1.0, the same default
--   'Combat.Resolution.Wear.staminaDrainStats' spends against.
attackerStance ∷ UnitInstance → Float
attackerStance = HM.lookupDefault 1.0 "stance" ∘ uiStats

-- | The commit-time verdict for one queued strike: 'Nothing' to
--   proceed, or the refusal that fires first.
--
--   @range@ is the attacker's live 'attackRangeTiles' and @reachBonus@
--   the REQUEST's own bonus (metres of extra strike reach a lunge
--   declared at launch), so a lunge's extended strike still lands —
--   the bound is never tighter than what the request was admitted on.
--   A negative bonus cannot tighten it below the plain range.
--
--   Order is fixed and total: page, then reach, then stance. Page
--   first because a cross-page pair has no meaningful separation to
--   measure; stance last because it is the only one that would
--   otherwise be SPENT.
checkAdmission
    ∷ Float          -- ^ attacker's live attack range, in tiles
    → Float          -- ^ the request's own reach bonus
    → AttackMode
    → UnitInstance   -- ^ attacker
    → UnitInstance   -- ^ target
    → Maybe AttackRefusal
checkAdmission range reachBonus mode atk tgt
    | uiPage atk ≢ uiPage tgt                  = Just RefusedDifferentPage
    | chebyshevSeparation atk tgt > reachBound = Just RefusedOutOfReach
    | attackerStance atk < stanceAttackCost mode
                                               = Just RefusedInsufficientStance
    | otherwise                                = Nothing
  where
    reachBound = range + max 0 reachBonus
