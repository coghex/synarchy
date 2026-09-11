-- | Copied authoring metadata; no native handles or playback state.
module Engine.Audio.Preview.Types where

import UPrelude

data PreviewAudioEntry = PreviewAudioEntry
  { paeId ∷ Text, paeLabel ∷ Text, paeCategory ∷ Text
  , paePath ∷ Maybe FilePath, paePlayable ∷ Bool
  } deriving (Eq, Show)

-- | The optional external file was explicitly selected by the CLI, before
-- changing the resource root. Only the preview worker consumes this authority.
newtype PreviewAudioConfig = PreviewAudioConfig { pacFile ∷ Maybe FilePath }
  deriving (Eq, Show)
