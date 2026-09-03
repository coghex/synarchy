{-# LANGUAGE OverloadedStrings #-}
-- | Item-definition discovery (#1232, PLC-1 of epic #1231): items are
--   the one data family whose definitions may be organized into logical
--   subdirectories, so @data\/items\/@ is walked recursively and loaded
--   in one canonical order, while every other family keeps
--   @engine.listFiles@'s flat enumeration untouched. Flora is flat too,
--   but no longer OS-ordered: #2241 gave it @canonicalFileOrder@ at its
--   own call site, because its sequential ids are what a save's numeric
--   flora references name ('Test.Headless.Startup.AssetLogging' owns
--   that half).
--
--   Three layers, because the contract is split across three of them and
--   an assertion at one layer cannot see a break in another:
--
--   * the WALK ('Engine.Asset.Discovery.walkFilesWithExtension') against
--     a real temp tree — depth, the extension predicate, symlinks, and
--     termination;
--   * the ORDER and the QUEUE, driving the REAL
--     @scripts\/startup_loader.lua@ in a bare Lua VM whose
--     @engine.listFilesRecursive@ is stubbed. Stubbing the enumeration
--     is the whole point: it is the only way to hand the same tree back
--     in two DIFFERENT underlying orders and prove the canonical order
--     is a deterministic transformation over that list rather than
--     whatever a temp directory happened to yield (requirement 11);
--   * the REGISTRY, through 'registerItemDefs' — the function
--     'loadItemYamlFn' delegates to with only argument marshalling
--     between them — so the duplicate winner, its diagnostic, and the
--     repeated-call contract are asserted on what the production path
--     actually did.
--
--   Fixtures only: no tracked item YAML moves for this issue, so every
--   tree here is built under the system temp directory.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Item definition discovery"'@.
module Test.Headless.Item.Discovery (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Directory
    ( createDirectoryIfMissing, createDirectoryLink, createFileLink
    , doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive )
import System.FilePath ((</>), takeDirectory)
import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Asset.Types (defaultAssetPool)
import Engine.Asset.YamlItems (loadItemYaml)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )
import Engine.Core.State (EngineEnv)
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API.Items.Defs
    (registerItemDefs, itemDuplicateMessage)
import Item.Types (ItemDef(..), ItemManager(..), emptyItemManager)

-----------------------------------------------------------------------
-- Fixture trees
-----------------------------------------------------------------------

-- | Build a throwaway tree from @(relative path, contents)@ pairs,
--   creating intermediate directories, and remove it afterwards. Every
--   path is relative and @/@-separated; the root is created under the
--   system temp directory, never inside the checkout, so the #1257
--   inventory walk and the shipped-corpus counts never see it.
withTree ∷ String → [(FilePath, String)] → (FilePath → IO α) → IO α
withTree label files action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> ("synarchy-1232-" ⧺ label)
    removeIfPresent root
    createDirectoryIfMissing True root
    forM_ files $ \(rel, contents) → do
        let full = root </> rel
        createDirectoryIfMissing True (takeDirectory full)
        writeFile full contents
    action root `finally` removeIfPresent root
  where
    removeIfPresent p = do
        present ← doesDirectoryExist p
        when present $ removeDirectoryRecursive p

-- | A minimal valid item definition: exactly the four fields the loader
--   REQUIRES (a missing or non-positive @bulk@ is rejected outright,
--   #1233), so nothing here can fail to parse for an unrelated reason.
itemYaml ∷ [(Text, Float)] → String
itemYaml defs = unlines $ "items:" : concat
    [ [ "  - name: \"" ⧺ T.unpack n ⧺ "\""
      , "    sprite: \"assets/textures/items/probe.png\""
      , "    weight: " ⧺ show w
      , "    bulk: 1.0"
      ]
    | (n, w) ← defs ]

-----------------------------------------------------------------------
-- The Lua half: canonical order + the startup queue
-----------------------------------------------------------------------

-- | Everything @scripts/startup_loader.lua@ reaches for outside its own
--   module, with @listFilesRecursive@ the ONLY enumeration whose order
--   matters. Every flat YAML family gets one healthy placeholder: since
--   #2203 an absent family is a terminal startup failure, so the item-order
--   fixture must satisfy those unrelated readiness preconditions before
--   the queue can reach the item tree it is testing.
luaPrelude ∷ [Text] → Text
luaPrelude enumeration = T.unlines
    [ "local recorded = {}"
    , "local function healthyYaml() return 0, true end"
    , "engine = {"
    , "  logInfo = function() end, logWarn = function() end,"
    , "  logError = function() end,"
    , "  listFiles = function(_, ext)"
    , "      if ext == '.yaml' then return { 'fixture.yaml' } end"
    , "      return nil end,"
    , "  loadTexture = function() end,"
    , "  loadTutorialDir = function() end,"
    , "  loadMaterialYaml = healthyYaml,"
    , "  loadVegetationYaml = healthyYaml,"
    , "  loadFloraYaml = healthyYaml,"
    , "  loadSubstanceYaml = healthyYaml,"
    , "  loadInfectionYaml = healthyYaml,"
    , "  loadRecipeYaml = healthyYaml,"
    , "  loadEquipmentYaml = healthyYaml,"
    , "  loadBuildingYaml = healthyYaml,"
    , "  loadUnitYaml = healthyYaml,"
    , "  loadLootTableYaml = healthyYaml,"
    , "  loadLocationYaml = healthyYaml,"
    , "  loadItemYaml = function(p)"
    , "      recorded[#recorded + 1] = p; return 1, true end,"
    , "}"
    , "local enumerations = { " <> luaList enumeration <> " }"
    , "local order = 1"
    , "engine.listFilesRecursive = function(dir, ext)"
    , "  if dir == 'data/items' and ext == '.yaml' then"
    , "    return enumerations[order] end"
    , "  return nil"
    , "end"
    , "local SL = require('scripts.startup_loader')"
    -- Run one whole profile against enumeration `which`, returning the
    -- ordered item paths the queue actually loaded.
    , "function runProfile(profile, which)"
    , "  order = which"
    , "  recorded = {}"
    , "  SL.build(profile)"
    , "  local guard = 0"
    , "  while not SL.isDone() do"
    , "    SL.tick(0)"
    , "    local failure = SL.getFailure()"
    , "    assert(not failure, failure and failure.message)"
    , "    guard = guard + 1"
    , "    assert(guard < 100000, 'the startup queue never drained')"
    , "  end"
    , "  return recorded"
    , "end"
    , "function joined(t) return table.concat(t, '|') end"
    , "canonicalFileOrder = SL.canonicalFileOrder"
    ]
  where
    -- Each enumeration is one Lua array of relative paths.
    luaList orders = T.intercalate ", "
        [ "{ " <> T.intercalate ", " [ "'" <> p <> "'" | p ← splitOn '|' o ]
          <> " }"
        | o ← orders ]
    splitOn c t = T.splitOn (T.singleton c) t

-- | Run one Lua chunk on top of 'luaPrelude', failing the example with
--   the Lua message when it raises.
runsOk ∷ [Text] → [Text] → Expectation
runsOk enumeration body = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        pre ← Lua.dostring (TE.encodeUtf8 (luaPrelude enumeration))
        case pre of
            Lua.OK → do
                status ← Lua.dostring (TE.encodeUtf8 (T.unlines body))
                case status of
                    Lua.OK → pure Nothing
                    _      → Just <$> luaMessage
            _ → Just ∘ ("prelude: " <>) <$> luaMessage
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)
  where
    luaMessage = do
        err ← Lua.tostring (-1)
        pure (maybe "<no message>" TE.decodeUtf8Lenient err)

-- | One fixture tree, deliberately mixing depths AND cases: @Apple@
--   sorting before @apple@ is what pins the order to raw UTF-8 bytes
--   rather than anything case- or locale-aware.
fixtureEnumeration ∷ Text
fixtureEnumeration =
    "steel_bar.yaml|crates/wooden_crate.yaml|apple.yaml\
    \|crates/metal/locker.yaml|Apple.yaml"

-- | The same five files, enumerated in a completely different order.
shuffledEnumeration ∷ Text
shuffledEnumeration =
    "crates/metal/locker.yaml|Apple.yaml|steel_bar.yaml\
    \|apple.yaml|crates/wooden_crate.yaml"

-- | What both of the above must load, in this order.
canonicalPaths ∷ Text
canonicalPaths =
    "data/items/Apple.yaml|data/items/apple.yaml\
    \|data/items/crates/metal/locker.yaml\
    \|data/items/crates/wooden_crate.yaml|data/items/steel_bar.yaml"

-----------------------------------------------------------------------
-- The registry half
-----------------------------------------------------------------------

-- | A logger whose backend records every emitted entry.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :))
        , lcDebugCategories = [CatAsset]
        }
    pure (logger, entriesRef)

warnings ∷ IORef [LogEntry] → IO [Text]
warnings entriesRef =
    reverse ∘ map leMessage ∘ filter ((≡ LevelWarn) ∘ leLevel)
        <$> readIORef entriesRef

-- | Load a fixture tree's files through the production path — the real
--   parser, then the real registration — in the order given, into a
--   registry of this call's own. Returns the manager plus every warning
--   emitted along the way.
--
--   The manager is FRESH rather than the shared engine's: these
--   fixtures must never reach the registry the rest of the suite reads.
loadInOrder
    ∷ EngineEnv → FilePath → [FilePath] → IO (ItemManager, [Text])
loadInOrder env root rels = do
    (logger, entriesRef) ← callbackLogger
    poolRef ← newIORef =≪ defaultAssetPool
    q ← Q.newQueue
    managerRef ← newIORef emptyItemManager
    forM_ rels $ \rel → do
        let path = root </> rel
        defs ← loadItemYaml logger path
        _ ← registerItemDefs env logger poolRef q managerRef path defs
        pure ()
    (,) <$> readIORef managerRef <*> warnings entriesRef

defNames ∷ ItemManager → [Text]
defNames = sort ∘ HM.keys ∘ imDefs

-----------------------------------------------------------------------

spec ∷ SpecWith EngineEnv
spec = describe "Item definition discovery" $ do

    describe "the recursive walk (requirement 1)" $ do

        it "finds every .yaml at every depth, relative and /-separated" $
            \_ → withTree "walk-depth"
                [ ("top.yaml", "")
                , ("crates/mid.yaml", "")
                , ("crates/metal/deep.yaml", "")
                , ("crates/metal/deeper/deepest.yaml", "")
                ] $ \root → do
                found ← walkFilesWithExtension root ".yaml"
                sort found `shouldBe`
                    [ "crates/metal/deep.yaml"
                    , "crates/metal/deeper/deepest.yaml"
                    , "crates/mid.yaml"
                    , "top.yaml"
                    ]

        it "accepts EXACTLY the extension asked for — recursion changed \
           \the depth of the walk, not which files it accepts" $
            \_ → withTree "walk-extension"
                [ ("keep.yaml", ""), ("skip.yml", "")
                , ("skip.YAML", ""), ("nested/keep.yaml", "")
                , ("nested/skip.txt", "")
                ] $ \root → do
                found ← walkFilesWithExtension root ".yaml"
                sort found `shouldBe` ["keep.yaml", "nested/keep.yaml"]

        it "returns nothing — never an error — for a directory that \
           \does not exist" $ \_ → do
            tmp ← getTemporaryDirectory
            found ← walkFilesWithExtension
                        (tmp </> "synarchy-1232-absent") ".yaml"
            found `shouldBe` []

        it "skips a symlinked FILE rather than discovering the same \
           \definition twice" $ \_ → withTree "walk-file-link"
                [ ("real.yaml", "") ] $ \root → do
                createFileLink (root </> "real.yaml") (root </> "alias.yaml")
                found ← walkFilesWithExtension root ".yaml"
                found `shouldBe` ["real.yaml"]

        it "skips a symlinked DIRECTORY, so the walk never reaches a \
           \file outside the tree" $ \_ → withTree "walk-dir-link"
                [ ("inside/real.yaml", "") ] $ \root → do
                withTree "walk-dir-link-outside"
                    [ ("outside.yaml", "") ] $ \other → do
                    createDirectoryLink other (root </> "escape")
                    found ← walkFilesWithExtension root ".yaml"
                    found `shouldBe` ["inside/real.yaml"]

        it "terminates on a symlink CYCLE instead of recursing forever" $
            \_ → withTree "walk-cycle" [ ("real.yaml", "") ] $ \root → do
                createDirectoryLink root (root </> "loop")
                found ← walkFilesWithExtension root ".yaml"
                found `shouldBe` ["real.yaml"]

    describe "the canonical order (requirements 2 and 11)" $ do

        it "is ascending UTF-8 BYTES of the tree-relative path, so an \
           \uppercase name sorts before a lowercase one" $ \_ →
            runsOk [fixtureEnumeration]
                [ "local o = canonicalFileOrder({ 'b.yaml', 'a/z.yaml',"
                , "  'B.yaml', 'a-x.yaml', 'a/A.yaml' })"
                , "assert(joined(o) == 'B.yaml|a-x.yaml|a/A.yaml|a/z.yaml|b.yaml',"
                , "       joined(o))"
                ]

        it "leaves the caller's enumeration untouched — it is a pure \
           \transformation, not an in-place sort" $ \_ →
            runsOk [fixtureEnumeration]
                [ "local input = { 'b.yaml', 'a.yaml' }"
                , "local o = canonicalFileOrder(input)"
                , "assert(joined(input) == 'b.yaml|a.yaml', joined(input))"
                , "assert(joined(o) == 'a.yaml|b.yaml', joined(o))"
                ]

        it "enqueues every nested file exactly once, in canonical order" $
            \_ → runsOk [fixtureEnumeration]
                [ "local loaded = runProfile('normal', 1)"
                , "assert(#loaded == 5, tostring(#loaded))"
                , "assert(joined(loaded) == '" <> canonicalPaths <> "',"
                , "       joined(loaded))"
                ]

        it "loads the identical ordered sequence from two DIFFERENT \
           \underlying enumeration orders" $ \_ →
            runsOk [fixtureEnumeration, shuffledEnumeration]
                [ "local first  = runProfile('normal', 1)"
                , "local second = runProfile('normal', 2)"
                , "assert(joined(first) == joined(second),"
                , "       joined(first) .. ' vs ' .. joined(second))"
                , "assert(joined(first) == '" <> canonicalPaths <> "',"
                , "       joined(first))"
                ]

    describe "normal and arena startup agree (requirement 3)" $

        it "enqueues the identical ordered item-path SEQUENCE, not \
           \merely the same set" $ \_ →
            runsOk [fixtureEnumeration, shuffledEnumeration]
                [ "local normal = runProfile('normal', 1)"
                , "local arena  = runProfile('arena', 2)"
                , "assert(joined(normal) == joined(arena),"
                , "       joined(normal) .. ' vs ' .. joined(arena))"
                , "assert(joined(arena) == '" <> canonicalPaths <> "',"
                , "       joined(arena))"
                ]

    describe "a definition's id comes from its name, never its path \
             \(requirement 5)" $

        it "keeps the identical id after the file moves under a \
           \subdirectory — only its recorded provenance changes" $ \env → do
            let body = itemYaml [("probe_ingot", 4.0)]
            (flatM, _) ← withTree "id-flat" [("ingots.yaml", body)] $
                \root → loadInOrder env root ["ingots.yaml"]
            (nestedM, _) ← withTree "id-nested" [("metal/ingots.yaml", body)] $
                \root → loadInOrder env root ["metal/ingots.yaml"]
            defNames flatM   `shouldBe` ["probe_ingot"]
            defNames nestedM `shouldBe` ["probe_ingot"]
            let flatDef   = HM.lookup "probe_ingot" (imDefs flatM)
                nestedDef = HM.lookup "probe_ingot" (imDefs nestedM)
            (idName <$> nestedDef)        `shouldBe` (idName <$> flatDef)
            (idDisplayName <$> nestedDef) `shouldBe` (idDisplayName <$> flatDef)
            (idWeight <$> nestedDef)      `shouldBe` (idWeight <$> flatDef)
            (idBulk <$> nestedDef)        `shouldBe` (idBulk <$> flatDef)
            -- The recorded source path is the ONE observable difference a
            -- move is allowed to make, and it is provenance, not identity.
            (idSourcePath <$> nestedDef) `shouldSatisfy`
                maybe False (T.isInfixOf "metal/ingots.yaml")

    describe "duplicate ids are last-write-wins (requirements 6 and 7)" $ do

        it "within ONE file, the later authored entry wins and the \
           \diagnostic names that file on both sides" $ \env →
            withTree "dup-intra"
                [ ("dupes.yaml", itemYaml [ ("probe_dupe", 1.0)
                                          , ("probe_dupe", 2.0) ]) ] $
                \root → do
                (m, warns) ← loadInOrder env root ["dupes.yaml"]
                let path = T.pack (root </> "dupes.yaml")
                idWeight <$> HM.lookup "probe_dupe" (imDefs m)
                    `shouldBe` Just 2.0
                warns `shouldBe`
                    [itemDuplicateMessage "probe_dupe" path path]

        it "across SIBLING files, the later file in canonical order \
           \wins and the diagnostic names both files" $ \env →
            withTree "dup-siblings"
                [ ("a_first.yaml",  itemYaml [("probe_dupe", 1.0)])
                , ("b_second.yaml", itemYaml [("probe_dupe", 2.0)])
                ] $ \root → do
                (m, warns) ← loadInOrder env root
                                ["a_first.yaml", "b_second.yaml"]
                idWeight <$> HM.lookup "probe_dupe" (imDefs m)
                    `shouldBe` Just 2.0
                warns `shouldBe`
                    [ itemDuplicateMessage "probe_dupe"
                        (T.pack (root </> "a_first.yaml"))
                        (T.pack (root </> "b_second.yaml")) ]

        it "across NESTED directories, the winner is decided by the \
           \same canonical order and nothing else" $ \env →
            withTree "dup-nested"
                [ ("crates/metal/locker.yaml", itemYaml [("probe_dupe", 3.0)])
                , ("crates/wooden.yaml",       itemYaml [("probe_dupe", 4.0)])
                , ("apple.yaml",               itemYaml [("probe_dupe", 5.0)])
                ] $ \root → do
                -- The canonical order of these three is apple, then
                -- crates/metal/locker, then crates/wooden.
                enumerated ← walkFilesWithExtension root ".yaml"
                sort enumerated `shouldBe`
                    [ "apple.yaml"
                    , "crates/metal/locker.yaml"
                    , "crates/wooden.yaml" ]
                (m, warns) ← loadInOrder env root (sort enumerated)
                idWeight <$> HM.lookup "probe_dupe" (imDefs m)
                    `shouldBe` Just 4.0
                warns `shouldBe`
                    [ itemDuplicateMessage "probe_dupe"
                        (T.pack (root </> "apple.yaml"))
                        (T.pack (root </> "crates/metal/locker.yaml"))
                    , itemDuplicateMessage "probe_dupe"
                        (T.pack (root </> "crates/metal/locker.yaml"))
                        (T.pack (root </> "crates/wooden.yaml")) ]

        it "leaves every unrelated definition alone" $ \env →
            withTree "dup-unrelated"
                [ ("a.yaml", itemYaml [ ("probe_dupe", 1.0)
                                      , ("probe_other", 9.0) ])
                , ("b.yaml", itemYaml [("probe_dupe", 2.0)])
                ] $ \root → do
                (m, _) ← loadInOrder env root ["a.yaml", "b.yaml"]
                defNames m `shouldBe` ["probe_dupe", "probe_other"]
                idWeight <$> HM.lookup "probe_other" (imDefs m)
                    `shouldBe` Just 9.0

    describe "engine.loadItemYaml stays repeatable (requirement 8)" $

        it "loading the same valid file twice is a reload, not a \
           \duplicate-id failure: same count, same registry" $ \env →
            withTree "repeat"
                [ ("tools.yaml", itemYaml [ ("probe_hammer", 1.5)
                                          , ("probe_saw", 2.5) ]) ] $
                \root → do
                let path = root </> "tools.yaml"
                (logger, entriesRef) ← callbackLogger
                poolRef ← newIORef =≪ defaultAssetPool
                q ← Q.newQueue
                managerRef ← newIORef emptyItemManager
                let once = do
                        defs ← loadItemYaml logger path
                        registerItemDefs env logger poolRef q managerRef
                            path defs
                first  ← once
                second ← once
                first  `shouldBe` 2
                second `shouldBe` first
                m ← readIORef managerRef
                defNames m `shouldBe` ["probe_hammer", "probe_saw"]
                idWeight <$> HM.lookup "probe_saw" (imDefs m)
                    `shouldBe` Just 2.5
                -- The reload replaces each of the file's OWN definitions,
                -- so the churn diagnostic fires once per definition with
                -- both paths equal. That is expected, and is NOT a
                -- collision.
                warns ← warnings entriesRef
                let p = T.pack path
                warns `shouldBe`
                    [ itemDuplicateMessage "probe_hammer" p p
                    , itemDuplicateMessage "probe_saw" p p ]

    describe "a nested parse failure still names the whole path \
             \(requirement 9)" $

        it "diagnoses the complete nested path and leaves the \
           \definitions that did load available" $ \env →
            withTree "nested-failure"
                [ ("good.yaml", itemYaml [("probe_good", 1.0)])
                , ("crates/broken.yaml", "items:\n  - name: \"probe_bad\"\n")
                ] $ \root → do
                (m, warns) ← loadInOrder env root
                                ["good.yaml", "crates/broken.yaml"]
                defNames m `shouldBe` ["probe_good"]
                let nested = T.pack (root </> "crates/broken.yaml")
                warns `shouldSatisfy` any (T.isInfixOf nested)
