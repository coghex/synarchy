-- | Regression coverage for #944 (CH-8): 'Engine.Core.Log.logMessage'
--   and 'logThreadMessage' were unified into one shared
--   entry-construction assembly, and 'Engine.Core.Log.Format's
--   normal/thread formatters into one policy-parameterized assembly.
--   The only prior coverage (#889, 'Test.Headless.Core.LogMonad') uses
--   'LogToCallback', which captures the structured 'LogEntry' BEFORE
--   formatting and so cannot see formatted-text drift between the
--   normal and thread paths — this module drives a real 'LogToHandle'
--   backend and reads the formatted line back to prove parity.
module Test.Headless.Core.LogParity (spec) where

import UPrelude
import Test.Hspec
import Data.Char (isDigit)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.IORef (newIORef, readIORef, modifyIORef', writeIORef)
import Control.Exception (finally)
import System.IO (IOMode(WriteMode), openFile, hClose)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Stack (SrcLoc(..))
import Engine.Core.Log
  ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
  , LogCategory(..), LogEntry(..), LoggerState(..), LogContext(..)
  , logInfoS, logThreadInfo
  )
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

spec ∷ Spec
spec = describe "normal and thread logging parity" $ do

  it "handle-formats normal output with timestamp, level, category, \
     \thread id, context, source location, message, and fields — in \
     \that order, with explicit fields winning over contextual ones" $ do
    line ← withLoggedLine "normal.log" $ \logger → do
      writeIORef (lsContext logger) LogContext
        { lcFields = Map.fromList [("shared", "fromContext"), ("ctxOnly", "c")]
        , lcBreadcrumbs = ["a", "b"]
        }
      logInfoS logger CatTest "normal-message-marker" [("shared", "fromExplicit")]

    isDigit (T.head line) `shouldBe` True
    line `shouldContainInOrder`
      [ "[INFO]", "[Test]", "[\920:", "[a > b]", "[LogParity:"
      , "normal-message-marker", "{ctxOnly=c, shared=fromExplicit}"
      ]

  it "handle-formats thread output with timestamp, level, category, \
     \message, and fields, omitting thread id, context, and source \
     \location even when the logger context is populated" $ do
    line ← withLoggedLine "thread.log" $ \logger → do
      writeIORef (lsContext logger) LogContext
        { lcFields = Map.singleton "ctxOnly" "c"
        , lcBreadcrumbs = ["a", "b"]
        }
      logThreadInfo logger CatTest "thread-message-marker"

    isDigit (T.head line) `shouldBe` True
    line `shouldContainInOrder`
      [ "[INFO]", "[Test]", "thread-message-marker", "{ctxOnly=c}" ]
    line `shouldNotSatisfy` ("[\920:" `T.isInfixOf`)
    line `shouldNotSatisfy` ("[a > b]" `T.isInfixOf`)
    line `shouldNotSatisfy` ("[LogParity:" `T.isInfixOf`)

  it "attributes a direct logThreadInfo call to its own call site, not \
     \an internal Log helper — thread formatted output omits location, \
     \so this checks the raw captured entry instead" $ do
    capturedRef ← newIORef []
    logger ← initLogger defaultLogConfig
      { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :)) }
    logThreadInfo logger CatTest "thread-srcloc-marker"

    entries ← readIORef capturedRef
    case entries of
      [entry] → case leSrcLoc entry of
        Nothing → expectationFailure "expected logThreadInfo to capture a source location"
        Just loc → do
          srcLocFile loc `shouldSatisfy` ("Test/Headless/Core/LogParity.hs" `isInfixOf`)
          srcLocFile loc `shouldNotSatisfy` ("Engine/Core/Log.hs" `isInfixOf`)
      other → expectationFailure $
        "expected exactly one captured log entry, got " ⧺ show (length other)

-- | Runs one logging call against a real 'LogToHandle' backend and
--   returns the single formatted line it wrote.
withLoggedLine ∷ FilePath → (LoggerState → IO ()) → IO Text
withLoggedLine name useLogger =
  withExclusiveTempDirectory "synarchy-log-parity" $ \tmp → do
    let path = tmp </> ("synarchy-log-parity-" ⧺ name)
    h ← openFile path WriteMode
    logger ← initLogger defaultLogConfig { lcBackend = LogToHandle h }
    useLogger logger `finally` hClose h
    contents ← TIO.readFile path
    case T.lines contents of
      [line] → pure line
      other → do
        expectationFailure $ "expected exactly one logged line, got " ⧺ show (length other)
        pure ""

shouldContainInOrder ∷ Text → [Text] → Expectation
shouldContainInOrder line markers = do
  mapM_ (\m → line `shouldSatisfy` (m `T.isInfixOf`)) markers
  let positions = map (\m → T.length (fst (T.breakOn m line))) markers
  positions `shouldSatisfy` \ps → and (zipWith (<) ps (drop 1 ps))
