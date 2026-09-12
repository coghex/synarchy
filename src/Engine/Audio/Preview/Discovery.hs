module Engine.Audio.Preview.Discovery
  ( isAudioFile, resolvePreviewFile, discoverAudioFiles ) where

import UPrelude
import Control.Exception (IOException, try)
import Data.Char (toLower)
import Data.List (sort)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, doesFileExist, doesDirectoryExist,
  listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>), isAbsolute, takeExtension)

isAudioFile ∷ FilePath → Bool
isAudioFile path = map toLower (takeExtension path) `elem` [".wav", ".flac", ".mp3"]

-- Explicit local audio-file selection is allowed outside assets/audio. Texture
-- category containment stays unchanged. Relative files belong to the caller's
-- directory, not a subsequently selected --resource-root.
resolvePreviewFile ∷ FilePath → FilePath → IO (Either Text FilePath)
resolvePreviewFile cwd path
  | not (isAudioFile path) = pure (Left "expected a WAV, FLAC or MP3 file")
  | '\0' `elem` path = pure (Left "audio path contains NUL")
  | otherwise = do
      result ← try @IOException $ do
        let candidate = if isAbsolute path then path else cwd </> path
        exists ← doesFileExist candidate
        if exists then Right <$> canonicalizePath candidate
        else pure (Left $ "audio file does not exist: " <> Text.pack path)
      pure $ either (Left ∘ tshow) id result

-- A bounded, stable catalog for the authoring browser. Never follows child
-- symlinks or scans unrelated directories. Decode budgets apply separately.
discoverAudioFiles ∷ FilePath → IO [FilePath]
discoverAudioFiles root = do
  exists ← doesDirectoryExist root
  if not exists then pure [] else walk 256 root
  where
    walk remaining directory = do
      names ← sort <$> listDirectory directory
      collect remaining directory names
    collect remaining _ _ | remaining ≤ 0 = pure []
    collect _ _ [] = pure []
    collect remaining directory (name:names) = do
      let path = directory </> name
      symlink ← pathIsSymbolicLink path
      found ← if symlink ∨ take 1 name ≡ "." then pure [] else do
        dir ← doesDirectoryExist path
        if dir then walk remaining path else do
          file ← doesFileExist path
          pure [path | file ∧ isAudioFile path]
      rest ← collect (remaining - length found) directory names
      pure (found <> rest)
