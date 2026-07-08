{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Leap mechanics: max horizontal distance and vertical strike-height
--   envelopes derived from jumping skill + stats, the arc timing shared
--   with "Unit.Thread.Movement.Fall", and starting a leap (a controlled
--   Standing→Falling arc that lands the unit on its feet).
module Unit.Thread.Movement.Leap
    ( metresPerTile
    , jumpMaxTiles
    , maxJumpHeight
    , strikeReach
    , jumpFlightTime
    , lungeImpactSpeed
    , startJump
    ) where

import UPrelude
import Unit.Sim.Types
import Unit.Fall (gravity)
import Unit.Thread.Movement.Types (fallGravityZ, vectorToDirection)

-- | Metres per tile at UNIT scale (combat/leaps). Not the worldgen
--   tectonic 10 m/tile — at the sprite scale a tile reads as ~2-3 m, so
--   a 1-tile-gap leap looks athletic-but-possible. Lets the leap's
--   horizontal reach (tiles) be related to strike heights (metres).
metresPerTile ∷ Float
metresPerTile = 2.5

-- | Max horizontal leap distance (tiles) from the leap skill + stats,
--   PENALISED by body-fat fraction (power-to-weight: a fat unit launches
--   the same legs against more mass). The skill/stat split: LEARNED
--   `jumping` sets the base, agility/strength extend it, fat cuts it.
--   Tuned so only an athletic acolyte clears a 1-tile gap (~2-tile leap)
--   and a fat one falls short; a squirrel leaps well past it.
jumpMaxTiles ∷ Float → Float → Float → Float → Float
             -- jumping skill, agility, strength, fat fraction
jumpMaxTiles jumpSkill agility strength fatFrac =
    (1.8 + 1.2 * (jumpSkill / 100)
         + 1.0 * max 0 (agility - 1)
         + 0.4 * max 0 (strength - 1))
    * (1 - 0.6 * clamp01F fatFrac)

-- | Max vertical strike height (metres above foot) a unit can reach by
--   leaping STRAIGHT UP — the hard cap that stops a squirrel headshotting
--   a giraffe. Same skill/stat/fat blend as the distance; squirrel ~1.5 m,
--   athletic acolyte ~1.0 m, bear ~1.8 m.
maxJumpHeight ∷ Float → Float → Float → Float → Float
              -- jumping skill, agility, strength, fat fraction
maxJumpHeight jumpSkill agility strength fatFrac =
    (0.6 + 1.0 * (jumpSkill / 100)
         + 0.5 * max 0 (agility - 1)
         + 0.2 * max 0 (strength - 1))
    * (1 - 0.6 * clamp01F fatFrac)

-- | Highest a unit can land a strike at horizontal leap distance d
--   (tiles), given its max distance and max vertical reach: the reach
--   ENVELOPE. Full height at d=0 (a near, steep-pitched leap), tapering
--   to 0 at max distance (a long, flat leap stays low). Capped ≥ 0.
strikeReach ∷ Float → Float → Float → Float
            -- d (tiles), maxDist (tiles), maxHeight (m)
strikeReach d maxDist maxHeight
    | maxDist ≤ 0 = maxHeight
    | otherwise   = max 0 (maxHeight * (1 - (d / maxDist) * (d / maxDist)))

clamp01F ∷ Float → Float
clamp01F x = max 0 (min 1 x)

-- | Arc apex (z above launch) for a leap of horizontal distance d —
--   taller for longer leaps, clamped so the arc stays readable.
jumpApexFor ∷ Float → Float
jumpApexFor d = max 0.5 (min 2.0 (0.5 + 0.3 * d))

-- | Time of flight (s) for a symmetric leap reaching apex `a`: up then
--   down, each leg √(2a/g). Shares fallGravityZ so a leap and a fall
--   obey the exact same gravity.
jumpFlightTime ∷ Float → Double
jumpFlightTime a = realToFrac (2 * sqrt (2 * max 0.01 a / fallGravityZ))

-- | Impact speed (m/s) of a lunge — how fast the body is travelling when it
--   hits, used by combat to size the full-body momentum a lunge adds to its
--   strike. A real horizontal leap (d > 0): the horizontal travel speed
--   (distance ÷ flight time). An in-place vertical pounce (d ≈ 0): the
--   downward speed gained falling from the unit's max jump height
--   (√(2·g·h)). Both rise with how committed the leap is.
lungeImpactSpeed ∷ Float → Float → Float    -- distTiles, maxHeight (m)
lungeImpactSpeed distTiles maxHeight
    | distTiles ≤ 0.01 = sqrt (2 * gravity * max 0 maxHeight)
    | otherwise        =
        let t = max 0.05 (realToFrac (jumpFlightTime (jumpApexFor distTiles)))
        in distTiles * metresPerTile / t

-- | Set up a LEAP: a Standing→Falling arc transition from the unit's
--   current position to (tgx,tgy) at the same z (slice 1 = flat leaps).
--   usJumpApex is what makes tickFallZ arc up-then-down + lerp xy, and
--   what makes handleTransitionExpiry land the unit STANDING (a leap is
--   a controlled jump, not a fall). Reuses the falling/landing anims.
startJump ∷ Double → UnitSimState → Int → Int → UnitSimState
startJump now ss tgx tgy =
    let fromX = usRealX ss
        fromY = usRealY ss
        z     = usGridZ ss
        dstX  = fromIntegral tgx + 0.5
        dstY  = fromIntegral tgy + 0.5
        dx    = dstX - fromX
        dy    = dstY - fromY
        d     = sqrt (dx * dx + dy * dy)
        apex  = jumpApexFor d
        dur   = jumpFlightTime apex
    in ss { usState            = TransitioningTo Falling
          , usTransitionUntil  = Just (now + dur)
          , usTransitionStride = 1
          , usFacing           = vectorToDirection dx dy
          , usFallFromTile     = Just (fromX, fromY, z)
          , usFallToTile       = Just (dstX, dstY, z)
          , usJumpApex         = Just apex
          , usPostTransition   = []
          , usTarget           = Nothing
          , usLocalPath        = []
          }
