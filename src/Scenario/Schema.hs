{-# LANGUAGE Strict #-}
-- | The scenario file boundary (#2699, SCN-01): read → parse → version
--   dispatch → in-memory migration → content validation.
--
--   'loadScenarioFile' is the one entry point the later slices (CLI,
--   Lua, the arena menu) share. It never writes: the source file's bytes
--   are only ever read (D-24). Its result is either an unsuccessful
--   'ScenarioFailed' — unreadable file, invalid YAML, a missing or
--   malformed @version@, an unsupported version, or a failed migration,
--   none of which may lead to construction (D-16, D-23, D-44) — or a
--   'ScenarioLoaded' with the usable content and every recoverable
--   diagnostic (D-14).
--
--   Versions: the format version is independent of ordinary save
--   versions (D-27, D-46). A file at version @v@ is supported when a
--   migration exists for every step from @v@ to the current version;
--   each step rewrites the parsed document in memory before the current
--   decoder sees it.
module Scenario.Schema
    ( -- * Format
      currentScenarioVersion
    , ScenarioFormat(..)
    , scenarioFormat
      -- * Loading
    , loadScenarioFile
    , loadScenarioFileWith
    , decodeScenarioBytes
    , decodeScenarioBytesWith
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Yaml as Yaml
import Data.List (sortOn)
import Control.Exception (IOException, try)
import Scenario.Types
import Scenario.Decode (decodeContent)
import Scenario.Decode.Monad (DecEnv(..), runDec)
import Scenario.Validate (Analysis(..), analyseNodes)

-- | The scenario format version this build writes and decodes natively.
currentScenarioVersion ∷ Int
currentScenarioVersion = 1

-- | A format: its current version, and the migration that lifts a
--   document from version @k@ to @k + 1@, for every released @k@ below
--   it. Every released version keeps its step forever (D-44).
data ScenarioFormat = ScenarioFormat
    { sfCurrent    ∷ !Int
    , sfMigrations ∷ ![(Int, A.Object → Either Text A.Object)]
    }

-- | The shipped format: v1 is current and the first release, so there
--   is nothing to migrate yet.
scenarioFormat ∷ ScenarioFormat
scenarioFormat = ScenarioFormat currentScenarioVersion []

loadScenarioFile ∷ ScenarioCatalog → FilePath → IO ScenarioOutcome
loadScenarioFile = loadScenarioFileWith scenarioFormat

loadScenarioFileWith ∷ ScenarioFormat → ScenarioCatalog → FilePath → IO ScenarioOutcome
loadScenarioFileWith fmt cat path = do
    r ← try (BS.readFile path)
    pure $ case r of
        Left (e ∷ IOException) → ScenarioFailed (ScenarioUnreadable (tshow e))
        Right bytes → decodeScenarioBytesWith fmt cat bytes

decodeScenarioBytes ∷ ScenarioCatalog → BS.ByteString → ScenarioOutcome
decodeScenarioBytes = decodeScenarioBytesWith scenarioFormat

decodeScenarioBytesWith ∷ ScenarioFormat → ScenarioCatalog → BS.ByteString → ScenarioOutcome
decodeScenarioBytesWith fmt cat bytes =
    case Yaml.decodeEither' bytes of
        Left e → ScenarioFailed (ScenarioSyntaxError (T.pack (Yaml.prettyPrintParseException e)))
        Right (A.Object o) → case readVersion o of
            Left f → ScenarioFailed f
            Right v → case migrate fmt v o of
                Left f → ScenarioFailed f
                Right current → validate cat v current
        Right _ → ScenarioFailed ScenarioNotAMapping
  where
    readVersion o = case KM.lookup "version" o of
        Nothing → Left ScenarioVersionMissing
        Just n@(A.Number _) → case A.fromJSON n of
            A.Success (v ∷ Int) → Right v
            A.Error _ → Left (ScenarioVersionMalformed (tshow n))
        Just other → Left (ScenarioVersionMalformed (tshow other))

-- | Lift a document to the current version, one step at a time.
migrate ∷ ScenarioFormat → Int → A.Object → Either ScenarioFailure A.Object
migrate fmt v o
    | v ≡ sfCurrent fmt = Right o
    | v > sfCurrent fmt ∨ v < 1 = Left (ScenarioVersionUnsupported v)
    | otherwise = case lookup v (sfMigrations fmt) of
        Nothing → Left (ScenarioVersionUnsupported v)
        Just stepFn → case stepFn o of
            Left why → Left (ScenarioMigrationFailed v why)
            Right o' → migrate fmt (v + 1) o'

-- | Two decoding passes around the cross-entry analysis (see
--   "Scenario.Decode").
validate ∷ ScenarioCatalog → Int → A.Object → ScenarioOutcome
validate cat sourceVersion o =
    let env1 = DecEnv cat Nothing False HS.empty HS.empty
        (_, localDiags, nodes) = runDec env1 (decodeContent sourceVersion o)
        an = analyseNodes nodes
        env2 = env1 { envFinal = True
                    , envRejected = anRejected an
                    , envDropped = anDropped an }
        (scenario, _, _) = runDec env2 (decodeContent sourceVersion o)
    in ScenarioLoaded scenario (sortOn sdPath (localDiags ⧺ anDiagnostics an))
