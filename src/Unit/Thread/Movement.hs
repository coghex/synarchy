{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Movement
    ( tickAllMovement
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, writeIORef)
import Unit.Types (UnitId(..))
import Unit.Sim.Types

-----------------------------------------------------------
-- Movement Tick
-----------------------------------------------------------

-- | Advance all units with active move targets.
--   Z is left unchanged — terrain-aware Z comes later.
tickAllMovement ∷ Double → IORef UnitThreadState → IO ()
tickAllMovement dt utsRef = do
    uts ← readIORef utsRef
    let simStates  = utsSimStates uts
        simStates' = HM.map (tickUnit dt) simStates
    writeIORef utsRef (uts { utsSimStates = simStates' })

tickUnit ∷ Double → UnitSimState → UnitSimState
tickUnit dt us = case usTarget us of
    Nothing → us
    Just mt →
        let dx   = mtTargetX mt - usRealX us
            dy   = mtTargetY mt - usRealY us
            dist = sqrt (dx * dx + dy * dy)
            step = mtSpeed mt * realToFrac dt
        in if dist ≤ step
            -- Arrived
            then us { usRealX  = mtTargetX mt
                    , usRealY  = mtTargetY mt
                    , usTarget = Nothing
                    , usState  = Idle
                    }
            -- Still moving
            else let nx   = dx / dist
                     ny   = dy / dist
                     newX = usRealX us + nx * step
                     newY = usRealY us + ny * step
                 in us { usRealX  = newX
                       , usRealY  = newY
                       , usFacing = vectorToDirection nx ny
                       , usState  = Walking
                       }

-----------------------------------------------------------
-- Direction from movement vector
-----------------------------------------------------------

vectorToDirection ∷ Float → Float → Direction
vectorToDirection nx ny
    | angle < 22.5   = DirE
    | angle < 67.5   = if ny > 0 then DirSE else DirNE
    | angle < 112.5  = if ny > 0 then DirS  else DirN
    | angle < 157.5  = if ny > 0 then DirSW else DirNW
    | otherwise      = DirW
  where
    angle = abs (atan2 ny nx * (180.0 / pi))
