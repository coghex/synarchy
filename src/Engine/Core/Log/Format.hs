{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, UnicodeSyntax #-}
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

writeLogEntry ∷ LogBackend → LogEntry → IO ()
writeLogEntry backend entry = case backend of
  LogToHandle h → TIO.hPutStrLn h (formatLogEntry entry) >> hFlush h
  LogToFile path → appendFile path (T.unpack $ formatLogEntry entry <> "\n")
  LogToCallback cb → cb entry
  LogMulti backends → mapM_ (`writeLogEntry` entry) backends

writeThreadLogEntry ∷ LogBackend → LogEntry → IO ()
writeThreadLogEntry backend entry = case backend of
  LogToHandle h → TIO.hPutStrLn h (formatThreadLogEntry entry) >> hFlush h
  LogToFile path → appendFile path (T.unpack $ formatThreadLogEntry entry <> "\n")
  LogToCallback cb → cb entry
  LogMulti backends → mapM_ (`writeLogEntry` entry) backends

formatLogEntry ∷ LogEntry → Text
formatLogEntry LogEntry{..} =
  T.intercalate " " $ filter (not . T.null)
    [ formatTimestamp leTimestamp
    , formatLevel leLevel
    , formatCategory leCategory
    , formatThread leThreadId
    , formatContext leContext
    , formatLocation leSrcLoc
    , leMessage
    , formatFields leFields
    ]

formatThreadLogEntry ∷ LogEntry → Text
formatThreadLogEntry LogEntry{..} =
  T.intercalate " " $ filter (not . T.null)
    [ formatTimestamp leTimestamp
    , formatLevel leLevel
    , formatCategory leCategory
    , ""
    , ""
    , ""
    , leMessage
    , formatFields leFields
    ]

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
      fileName = T.takeWhileEnd (/= '.') modName  -- Get last component
  in "[" <> fileName <> ":" <> T.pack (show (srcLocStartLine loc)) <> "]"

formatFields ∷ Map.Map Text Text → Text
formatFields fields
  | Map.null fields = ""
  | otherwise = "{" <> T.intercalate ", " (map formatField $ Map.toList fields) <> "}"
  where
    formatField (k, v) = k <> "=" <> v
