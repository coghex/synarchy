-- | Regression coverage for #1915: which categories @ENGINE_DEBUG@ can
--   enable used to be decided by two hand-written subsets of
--   'LogCategory' — a 22-branch 'parseCategory' table and a 21-entry
--   @"all"@ literal — and both had drifted past constructors added
--   later. 'Engine.Core.Log.isEnabled' gates @LevelDebug@ on membership
--   of that map, so a missing category silently suppressed every one of
--   its debug messages.
--
--   Both surfaces now derive from the bounded type itself, so this
--   module sweeps @[minBound .. maxBound]@ rather than a list of its
--   own: a constructor added without the corresponding behavior fails
--   here instead of quietly becoming unnameable.
--
--   @ENGINE_DEBUG@ is process-global and the headless suite initializes
--   several loggers, so every example restores the variable's prior
--   state — an existing value or its absence — through 'bracket', on
--   the failing and the throwing path alike.
module Test.Headless.Core.LogCategoryEnv (spec) where

import UPrelude
import Test.Hspec
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Exception (bracket)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Engine.Core.Log.Types
  ( LogCategory(..), allLogCategories, categoryEnvName, parseCategory )
import Engine.Core.Log.Env (loadDebugCategoriesFromEnv)

spec ∷ Spec
spec = describe "log category env" $ do

  describe "derived category names" $ do

    it "derives a non-empty, Cat-prefixed, collision-free name for every \
       \category, so no constructor can join the type with a name that \
       \shadows another or that the Cat-stripping derivation mangles" $ do
      [ cat | cat ← allLogCategories, take 3 (show cat) ≢ "Cat" ]
        `shouldBe` []
      [ cat | cat ← allLogCategories, T.null (categoryEnvName cat) ]
        `shouldBe` []
      let names = map categoryEnvName allLogCategories
      sort (nub names) `shouldBe` sort names

    it "still resolves every one of the 22 names the former hand-written \
       \table accepted, to the same constructor and case-insensitively" $
      mapM_ (\(name, cat) → do
              parseCategory name `shouldBe` Just cat
              parseCategory (T.toUpper name) `shouldBe` Just cat)
            legacyParserTable

    it "resolves no name the former table accepted to a different \
       \constructor, and leaves an unrecognized name unparsed" $ do
      parseCategory "nosuchcategory" `shouldBe` Nothing
      parseCategory "" `shouldBe` Nothing

  describe "ENGINE_DEBUG=<category>" $

    it "enables exactly its own category — never more, never fewer — for \
       \every category, spelled lowercase, in display case, and uppercase" $
      mapM_ (\cat → do
              let expected = Map.singleton cat True
                  lower    = categoryEnvName cat
                  -- The spelling Engine.Core.Log.Format displays, which
                  -- is this name before lowercasing: always non-lowercase,
                  -- since every constructor name is capitalized.
                  display  = T.pack (drop 3 (show cat))
              enabledFor (T.unpack lower)   `shouldReturn` expected
              enabledFor (T.unpack display) `shouldReturn` expected
              enabledFor (T.unpack (T.toUpper lower)) `shouldReturn` expected)
            allLogCategories

  describe "ENGINE_DEBUG=all" $

    it "enables the complete set of categories, CatRender, CatWorld and \
       \CatUnit — the three the hand-written literal had drifted past — \
       \included" $ do
      enabled ← enabledFor "all"
      enabled `shouldBe`
        Map.fromList [(cat, True) | cat ← allLogCategories]
      mapM_ (\cat → Map.lookup cat enabled `shouldBe` Just True)
            [CatRender, CatWorld, CatUnit]

  -- Requirement 6: everything about this loader other than which
  -- categories are reachable is unchanged. These pin the surrounding
  -- behavior so the derivation cannot quietly alter it.
  describe "unchanged loader behavior" $ do

    it "retains the supplied defaults when ENGINE_DEBUG is unset" $
      withEngineDebug Nothing
        (loadDebugCategoriesFromEnv [CatLua, CatVulkan])
        `shouldReturn` Map.fromList [(CatLua, True), (CatVulkan, True)]

    it "splits on commas and trims surrounding whitespace" $
      enabledFor "  vulkan , lua ,ui  "
        `shouldReturn`
        Map.fromList [(CatVulkan, True), (CatLua, True), (CatUI, True)]

    it "replaces the defaults rather than merging with them" $
      withEngineDebug (Just "vulkan")
        (loadDebugCategoriesFromEnv [CatLua])
        `shouldReturn` Map.singleton CatVulkan True

    it "drops an unrecognized name silently, keeping the recognized ones \
       \and never falling back to the defaults" $ do
      withEngineDebug (Just "vulkan,nosuchcategory,lua")
        (loadDebugCategoriesFromEnv [CatTest])
        `shouldReturn` Map.fromList [(CatVulkan, True), (CatLua, True)]
      withEngineDebug (Just "nosuchcategory")
        (loadDebugCategoriesFromEnv [CatTest])
        `shouldReturn` Map.empty

    it "matches the \"all\" keyword exactly, so \"All\" stays an ordinary \
       \unrecognized name — today's contract, deliberately preserved" $
      enabledFor "All" `shouldReturn` Map.empty

-- | The complete name→constructor table 'parseCategory' carried before
--   the derivation replaced it, transcribed by hand. It is frozen on
--   purpose: it proves the derived names reproduce the old ones rather
--   than restating the derivation and agreeing with itself.
legacyParserTable ∷ [(Text, LogCategory)]
legacyParserTable =
  [ ("vulkan", CatVulkan), ("graphics", CatGraphics), ("render", CatRender)
  , ("shader", CatShader), ("descriptor", CatDescriptor)
  , ("swapchain", CatSwapchain), ("texture", CatTexture), ("font", CatFont)
  , ("asset", CatAsset), ("resource", CatResource), ("lua", CatLua)
  , ("script", CatScript), ("input", CatInput), ("scene", CatScene)
  , ("ui", CatUI), ("thread", CatThread), ("system", CatSystem)
  , ("init", CatInit), ("state", CatState), ("general", CatGeneral)
  , ("test", CatTest), ("event", CatEvent)
  ]

-- | The debug map @ENGINE_DEBUG=<value>@ produces from an empty default
--   set, so an assertion on the whole map says exactly which categories
--   the value enables.
enabledFor ∷ String → IO (Map.Map LogCategory Bool)
enabledFor value =
  withEngineDebug (Just value) (loadDebugCategoriesFromEnv [])

-- | Run an action with @ENGINE_DEBUG@ set to the given value (or unset),
--   restoring whichever state it was in beforehand however the action
--   ends.
withEngineDebug ∷ Maybe String → IO α → IO α
withEngineDebug value action =
  bracket (lookupEnv "ENGINE_DEBUG") apply $ \_ → do
    apply value
    action
  where
    apply Nothing  = unsetEnv "ENGINE_DEBUG"
    apply (Just v) = setEnv "ENGINE_DEBUG" v
