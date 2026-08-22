module Engine.Core.Log.Format
  ( writeLogEntry
  , writeThreadLogEntry
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import Control.Concurrent (ThreadId)
import GHC.Stack (SrcLoc(..))
import System.IO (hFlush)
import Engine.Core.Log.Types (LogBackend(..), LogEntry(..), LogLevel(..), LogCategory)

-- | Which optional components a formatted entry includes: normal output
--   shows thread id/context/source location, thread output omits all
--   three (CH-8, #944 — was two independently maintained assemblies).
data FormatPolicy = NormalFormat | ThreadFormat

writeLogEntry ∷ LogBackend → LogEntry → IO ()
writeLogEntry = writeEntryWith NormalFormat

writeThreadLogEntry ∷ LogBackend → LogEntry → IO ()
writeThreadLogEntry = writeEntryWith ThreadFormat

-- | Shared backend dispatch.
writeEntryWith ∷ FormatPolicy → LogBackend → LogEntry → IO ()
writeEntryWith policy backend entry = case backend of
  LogToHandle h → TIO.hPutStrLn h (formatEntry policy entry) >> hFlush h
  LogToCallback cb → cb entry

formatEntry ∷ FormatPolicy → LogEntry → Text
formatEntry policy LogEntry{..} =
  T.intercalate " " $ filter (not . T.null)
    [ formatTimestamp leTimestamp
    , formatLevel leLevel
    , formatCategory leCategory
    , optional formatThread leThreadId
    , optional formatContext leContext
    , optional formatLocation leSrcLoc
    , leMessage
    , formatFields leFields
    ]
  where
    optional ∷ (a → Text) → a → Text
    optional f x = case policy of
      NormalFormat → f x
      ThreadFormat → ""

formatTimestamp ∷ Clock.UTCTime → Text
formatTimestamp t = T.pack $ TimeFormat.formatTime TimeFormat.defaultTimeLocale "%Y-%m-%d %H:%M:%S" t

formatLevel ∷ LogLevel → Text
formatLevel LevelDebug = "[DEBUG]"
formatLevel LevelInfo  = "[INFO]"
formatLevel LevelWarn  = "[WARN]"
formatLevel LevelError = "[ERROR]"

formatCategory ∷ LogCategory → Text
formatCategory cat = "[" <> T.pack (drop 3 (show cat)) <> "]"

formatThread ∷ ThreadId → Text
formatThread tid = "[Θ:" <> T.pack (drop 9 (show tid)) <> "]"

formatContext ∷ [Text] → Text
formatContext [] = ""
formatContext ctx = "[" <> T.intercalate " > " ctx <> "]"

formatLocation ∷ Maybe SrcLoc → Text
formatLocation Nothing = ""
formatLocation (Just loc) =
  let modName = T.pack $ srcLocModule loc
      fileName = T.takeWhileEnd (≢ '.') modName  -- Get last component
  in "[" <> fileName <> ":" <> tshow (srcLocStartLine loc) <> "]"

formatFields ∷ Map.Map Text Text → Text
formatFields fields
  | Map.null fields = ""
  | otherwise = "{" <> T.intercalate ", " (map formatField $ Map.toList fields) <> "}"
  where
    formatField (k, v) = k <> "=" <> v
