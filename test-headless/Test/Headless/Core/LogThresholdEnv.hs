-- | Regression coverage for #1918: 'loadCategoryLevelsFromEnv'
--   documented its per-category threshold contract as
--   @ENGINE_LOG_\<CATEGORY\>@ but built the key from @show cat@, which
--   is the internal constructor name. The documented
--   @ENGINE_LOG_VULKAN@ therefore did nothing at all, silently, and
--   only the undocumented @ENGINE_LOG_CATVULKAN@ had any effect —
--   while 'Engine.Core.Log.isEnabled' reads the resulting map on every
--   non-debug log call, so the surface was live and just unreachable
--   by its advertised name.
--
--   The key now derives from 'categoryEnvName', the same @Cat@-stripped
--   spelling 'parseCategory' accepts and 'Engine.Core.Log.Format'
--   displays. This module sweeps @[minBound .. maxBound]@ so a
--   constructor added later is covered without editing a list, and
--   pins the documented spellings against a hand-transcribed table
--   rather than restating the derivation and agreeing with itself.
--
--   Process environment is global and the headless suite initializes
--   several loggers, so every example runs inside a 'bracket' that
--   snapshots EVERY variable this loader consults — both spellings of
--   every category, plus @ENGINE_LOG_LEVEL@ and @ENGINE_DEBUG@ — and
--   restores each one's original value or its original absence however
--   the example ends. That also makes the assertions independent of a
--   developer's ambient logging environment.
module Test.Headless.Core.LogThresholdEnv (spec) where

import UPrelude
import Test.Hspec
import Data.Char (toUpper)
import Data.List (sort)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Control.Exception (bracket)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Engine.Core.Log (initLogger)
import Engine.Core.Log.Types
  ( LogBackend(..), LogCategory(..), LogConfig(..), LogLevel(..)
  , LoggerState(..), allLogCategories, defaultLogConfig )
import Engine.Core.Log.Env (loadCategoryLevelsFromEnv)

spec ∷ Spec
spec = describe "category log threshold env" $ do

  describe "documented ENGINE_LOG_<CATEGORY> spelling" $ do

    it "reaches CatVulkan through ENGINE_LOG_VULKAN, the spelling the \
       \contract has always advertised and the one that used to do \
       \nothing at all" $
      withLogEnv [("ENGINE_LOG_VULKAN", "error")] loadFromEmpty
        `shouldReturn` Map.singleton CatVulkan LevelError

    it "names every category by ENGINE_LOG_ plus its displayed, \
       \Cat-stripped name — a frozen transcription, so a change to the \
       \derivation fails here instead of agreeing with itself" $ do
      map snd documentedEnvVars `shouldBe` allLogCategories
      sort (map fst documentedEnvVars)
        `shouldBe` sort (map (("ENGINE_LOG_" <>) ∘ displayName)
                             allLogCategories)

    it "sets exactly its own category's threshold — never another's, \
       \never more than one — for every category over [minBound .. \
       \maxBound]" $
      mapM_ (\(var, cat) →
              withLogEnv [(var, "warn")] loadFromEmpty
                `shouldReturn` Map.singleton cat LevelWarn)
            documentedEnvVars

    it "leaves the map untouched when no category variable is set, so an \
       \unmentioned category falls through to the global minimum" $ do
      withLogEnv [] loadFromEmpty `shouldReturn` Map.empty
      withLogEnv [] (loadCategoryLevelsFromEnv seeded)
        `shouldReturn` seeded

    it "overrides a supplied entry for the category it names and only \
       \that one" $
      withLogEnv [("ENGINE_LOG_LUA", "error")]
                 (loadCategoryLevelsFromEnv seeded)
        `shouldReturn` Map.insert CatLua LevelError seeded

  describe "legacy ENGINE_LOG_CAT<CONSTRUCTOR> spelling" $ do

    it "no longer has any effect for any category: the internal \
       \constructor prefix is not part of the environment contract" $
      mapM_ (\cat →
              withLogEnv [(legacyEnvVar cat, "error")] loadFromEmpty
                `shouldReturn` Map.empty)
            allLogCategories

    it "loses to the documented spelling when both are set, rather than \
       \overriding or merging with it" $
      withLogEnv [ ("ENGINE_LOG_VULKAN", "warn")
                 , ("ENGINE_LOG_CATVULKAN", "error") ]
                 loadFromEmpty
        `shouldReturn` Map.singleton CatVulkan LevelWarn

  describe "ENGINE_LOG_LEVEL stays the global minimum" $ do

    it "collides with no category's derived variable name, so the two \
       \can share this namespace" $
      filter (≡ "ENGINE_LOG_LEVEL") (map fst documentedEnvVars)
        `shouldBe` []

    it "moves lsMinLevel through initLogger while contributing no \
       \per-category entry" $
      withLogEnv [("ENGINE_LOG_LEVEL", "error")] $ do
        st ← initLogger quietConfig
        readIORef (lsMinLevel st) `shouldReturn` LevelError
        readIORef (lsCategoryLevels st) `shouldReturn` Map.empty

    it "is not itself consumed as a category threshold when a category \
       \variable is set alongside it" $
      withLogEnv [ ("ENGINE_LOG_LEVEL", "error")
                 , ("ENGINE_LOG_UI", "warn") ] $ do
        st ← initLogger quietConfig
        readIORef (lsMinLevel st) `shouldReturn` LevelError
        readIORef (lsCategoryLevels st)
          `shouldReturn` Map.singleton CatUI LevelWarn

  describe "level values parse exactly as parseLogLevel does" $ do

    it "accepts debug/info/warn/error case-insensitively" $
      mapM_ (\(value, level) →
              withLogEnv [("ENGINE_LOG_WORLD", value)] loadFromEmpty
                `shouldReturn` Map.singleton CatWorld level)
            [ ("debug", LevelDebug), ("DEBUG", LevelDebug)
            , ("info", LevelInfo),   ("Info",  LevelInfo)
            , ("warn", LevelWarn),   ("WaRn",  LevelWarn)
            , ("error", LevelError), ("ERROR", LevelError) ]

    -- An empty value is deliberately absent here: base's 'setEnv'
    -- unsets a variable given @""@, so the case is unreachable through
    -- this fixture rather than untested behavior.
    it "falls back to LevelInfo on an unrecognized value, recording the \
       \entry rather than dropping it — today's contract, deliberately \
       \preserved" $
      mapM_ (\value →
              withLogEnv [("ENGINE_LOG_WORLD", value)] loadFromEmpty
                `shouldReturn` Map.singleton CatWorld LevelInfo)
            ["nosuchlevel", "trace", "0", " warn "]

  describe "thresholds and ENGINE_DEBUG stay separate" $

    it "fills lsCategoryLevels and lsDebugEnabled independently when \
       \both name the same category" $
      withLogEnv [ ("ENGINE_LOG_VULKAN", "error")
                 , ("ENGINE_DEBUG", "vulkan") ] $ do
        st ← initLogger quietConfig
        readIORef (lsCategoryLevels st)
          `shouldReturn` Map.singleton CatVulkan LevelError
        readIORef (lsDebugEnabled st)
          `shouldReturn` Map.singleton CatVulkan True

-- | The complete documented variable→constructor table, transcribed by
--   hand in constructor order. Frozen on purpose: it states the public
--   contract independently of the code that derives it.
documentedEnvVars ∷ [(String, LogCategory)]
documentedEnvVars =
  [ ("ENGINE_LOG_VULKAN", CatVulkan)
  , ("ENGINE_LOG_GRAPHICS", CatGraphics)
  , ("ENGINE_LOG_RENDER", CatRender)
  , ("ENGINE_LOG_SHADER", CatShader)
  , ("ENGINE_LOG_DESCRIPTOR", CatDescriptor)
  , ("ENGINE_LOG_SWAPCHAIN", CatSwapchain)
  , ("ENGINE_LOG_TEXTURE", CatTexture)
  , ("ENGINE_LOG_FONT", CatFont)
  , ("ENGINE_LOG_ASSET", CatAsset)
  , ("ENGINE_LOG_RESOURCE", CatResource)
  , ("ENGINE_LOG_LUA", CatLua)
  , ("ENGINE_LOG_SCRIPT", CatScript)
  , ("ENGINE_LOG_INPUT", CatInput)
  , ("ENGINE_LOG_SCENE", CatScene)
  , ("ENGINE_LOG_UI", CatUI)
  , ("ENGINE_LOG_WORLD", CatWorld)
  , ("ENGINE_LOG_UNIT", CatUnit)
  , ("ENGINE_LOG_THREAD", CatThread)
  , ("ENGINE_LOG_SYSTEM", CatSystem)
  , ("ENGINE_LOG_INIT", CatInit)
  , ("ENGINE_LOG_STATE", CatState)
  , ("ENGINE_LOG_GENERAL", CatGeneral)
  , ("ENGINE_LOG_TEST", CatTest)
  , ("ENGINE_LOG_EVENT", CatEvent)
  ]

-- | The category's displayed name — its constructor without the @Cat@
--   prefix — uppercased, which is the spelling the table above states.
displayName ∷ LogCategory → String
displayName = map toUpper ∘ drop 3 ∘ show

-- | The pre-#1918 key: the whole constructor name, uppercased. Nothing
--   may consult this any more.
legacyEnvVar ∷ LogCategory → String
legacyEnvVar cat = "ENGINE_LOG_" <> map toUpper (show cat)

-- | A non-empty starting map, so an example can tell "left alone" apart
--   from "rebuilt empty".
seeded ∷ Map.Map LogCategory LogLevel
seeded = Map.fromList [(CatFont, LevelWarn), (CatLua, LevelDebug)]

-- | The category map @loadCategoryLevelsFromEnv@ builds from nothing, so
--   an assertion on the whole map says exactly which categories the
--   environment reached.
loadFromEmpty ∷ IO (Map.Map LogCategory LogLevel)
loadFromEmpty = loadCategoryLevelsFromEnv Map.empty

-- | A logger that writes nowhere, so 'initLogger' examples add no output
--   to the suite.
quietConfig ∷ LogConfig
quietConfig = defaultLogConfig { lcBackend = LogToCallback (\_ → return ()) }

-- | Every variable this loader consults, in one list: both spellings of
--   every category plus the two globals that share the namespace.
managedVars ∷ [String]
managedVars =
  "ENGINE_LOG_LEVEL" : "ENGINE_DEBUG"
    : concatMap (\cat → [ "ENGINE_LOG_" <> displayName cat
                        , legacyEnvVar cat ])
                allLogCategories

-- | Run an action with exactly the given variables set and every other
--   managed variable unset, restoring each one's prior value — or its
--   prior absence — however the action ends.
withLogEnv ∷ [(String, String)] → IO α → IO α
withLogEnv assignments action =
  bracket snapshot restore $ \_ → do
    mapM_ unsetEnv managedVars
    mapM_ (uncurry setEnv) assignments
    action
  where
    snapshot = mapM (\var → (,) var ⊚ lookupEnv var) managedVars
    restore = mapM_ (\(var, mValue) → case mValue of
                                        Nothing → unsetEnv var
                                        Just v  → setEnv var v)
