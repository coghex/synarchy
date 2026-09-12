-- | Runtime-only semantic requests. No audio type participates in a save codec.
module Engine.Audio.Types where

import UPrelude
import qualified Data.Text as Text
import Engine.Graphics.Camera (CameraFacing)
import World.Page.Types (WorldPageId(..))

data AudioPosition = AudioPosition
  { apPage ∷ WorldPageId, apX ∷ Float, apY ∷ Float, apZ ∷ Float }
  deriving (Eq, Show)

data TriggerOptions = TriggerOptions
  { triggerPosition ∷ Maybe AudioPosition
  , triggerGainDb ∷ Float, triggerPitch ∷ Float }
  deriving (Eq, Show)

defaultTriggerOptions ∷ TriggerOptions
defaultTriggerOptions = TriggerOptions Nothing 0 0

data LoopOptions = LoopOptions
  { loopPosition ∷ Maybe AudioPosition, loopGainDb ∷ Maybe Float }
  deriving (Eq, Show)

data ListenerSnapshot = ListenerSnapshot
  { listenerPage ∷ WorldPageId
  , listenerX ∷ Float, listenerY ∷ Float, listenerZ ∷ Float
  , listenerFacing ∷ CameraFacing, listenerZoom ∷ Float
  -- U = X-Y and V = X+Y periods in tiles; zero means no wrapping.
  , listenerWrapU ∷ Float, listenerWrapV ∷ Float
  , listenerZoomFloor ∷ Float, listenerFadeStart ∷ Float, listenerFadeEnd ∷ Float
  } deriving (Eq, Show)

validSoundId ∷ Text → Bool
validSoundId value = Text.length value ≤ 64 ∧ case Text.uncons value of
  Nothing → False
  Just (first, rest) → lower first ∧ Text.all (\c → lower c ∨ (c ≥ '0' ∧ c ≤ '9') ∨ c ≡ '_') rest
  where lower c = c ≥ 'a' ∧ c ≤ 'z'

validLoopId ∷ Text → Bool
validLoopId name = not (Text.null name) ∧ Text.length name ≤ 128 ∧ not (Text.any (≡ '\0') name)

finite ∷ Float → Bool
finite value = not (isNaN value ∨ isInfinite value)

validPosition ∷ AudioPosition → Bool
validPosition position = not (Text.null $ unWorldPageId $ apPage position)
  ∧ all finite [apX position, apY position, apZ position]

validTrigger ∷ TriggerOptions → Bool
validTrigger options = maybe True validPosition (triggerPosition options)
  ∧ inRange (-48) 12 (triggerGainDb options) ∧ inRange (-24) 24 (triggerPitch options)

validLoopOptions ∷ LoopOptions → Bool
validLoopOptions options = maybe True validPosition (loopPosition options)
  ∧ maybe True (inRange (-48) 12) (loopGainDb options)

inRange ∷ Float → Float → Float → Bool
inRange low high value = finite value ∧ value ≥ low ∧ value ≤ high

validListener ∷ ListenerSnapshot → Bool
validListener listener = not (Text.null $ unWorldPageId $ listenerPage listener)
  ∧ all finite [listenerX listener, listenerY listener, listenerZ listener,
      listenerZoom listener, listenerWrapU listener, listenerWrapV listener,
      listenerZoomFloor listener, listenerFadeStart listener, listenerFadeEnd listener]
  ∧ listenerWrapU listener ≥ 0 ∧ listenerWrapV listener ≥ 0
  ∧ listenerZoomFloor listener > 0 ∧ listenerFadeStart listener > listenerZoomFloor listener
  ∧ listenerFadeEnd listener > listenerFadeStart listener
