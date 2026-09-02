{-# LANGUAGE OverloadedStrings #-}
-- | Startup asset logging ownership (#1930).
--
--   Two layers own two halves of one contract, and neither layer can
--   see the other's break:
--
--   * the ENGINE bindings own per-file success DETAIL, at @CatAsset@
--     Debug, carrying the full path and the authoritative count they
--     actually returned to Lua — and emit nothing at Info;
--   * @scripts\/startup_loader.lua@ owns exactly ONE Info aggregate per
--     registry family, summing what those calls returned.
--
--   So the Lua half runs the REAL @scripts\/startup_loader.lua@ in a
--   bare Lua VM against stubbed enumeration and loader doubles (the
--   technique 'Test.Headless.Item.Discovery' established) — stubbing is
--   what makes each file's return distinguishable, which is the only
--   way an aggregate can be proved to be that family's OWN sum rather
--   than a plausible number. The engine half drives the twelve real
--   @engine.load*Yaml@ bindings through a real Lua backend on a private
--   headless engine, against real shipped data, with a capturing logger.
--
--   The two halves also live in different log CATEGORIES, deliberately
--   asserted as such: the per-file detail is @CatAsset@, while the
--   loader's aggregate goes out through @engine.logInfo@ and therefore
--   lands under @CatLua@. Acceptance locates an aggregate by its stable
--   family identifier, never by a category-wide line count.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Startup asset logging"'@.
module Test.Headless.Startup.AssetLogging (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.Char (isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory
    , removeDirectoryRecursive )
import System.FilePath ((</>))
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..) )
import Engine.Core.State
    ( loggerRef, luaToEngineQueue, luaQueue, assetPoolRef
    , nextObjectIdRef, inputStateRef )
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))

-----------------------------------------------------------------------
-- The twelve scoped families
-----------------------------------------------------------------------

-- | One registry family, as the startup loader enqueues it.
data Fam = Fam
    { famDir   ∷ Text  -- ^ the directory the loader enumerates
    , famId    ∷ Text  -- ^ the stable identifier its aggregate reports under
    , famVerb  ∷ Text  -- ^ the @engine.load*Yaml@ binding
    , famTree  ∷ Bool  -- ^ enumerated by @listFilesRecursive@ (items only)
    }

-- | Normal startup's twelve, in @queueNormalProfile@'s own order. The
--   tutorial tree is deliberately absent: it is ONE directory-level
--   @engine.loadTutorialDir@ call, not a per-file YAML family (#1930's
--   scope), and the texture-only phases are out of scope entirely.
normalFams ∷ [Fam]
normalFams =
    [ Fam "data/materials"   "material"   "loadMaterialYaml"   False
    , Fam "data/vegetation"  "vegetation" "loadVegetationYaml" False
    , Fam "data/flora"       "flora"      "loadFloraYaml"      False
    , Fam "data/substances"  "substance"  "loadSubstanceYaml"  False
    , Fam "data/infections"  "infection"  "loadInfectionYaml"  False
    , Fam "data/recipes"     "recipe"     "loadRecipeYaml"     False
    , Fam "data/items"       "item"       "loadItemYaml"       True
    , Fam "data/equipment"   "equipment"  "loadEquipmentYaml"  False
    , Fam "data/buildings"   "building"   "loadBuildingYaml"   False
    , Fam "data/units"       "unit"       "loadUnitYaml"       False
    , Fam "data/loot_tables" "loot_table" "loadLootTableYaml"  False
    , Fam "data/locations"   "location"   "loadLocationYaml"   False
    ]

-- | Arena startup's eleven: the same inventory minus flora
--   (@queueArenaProfile@), and minus the tutorial.
arenaFams ∷ [Fam]
arenaFams = [ f | f ← normalFams, famId f ≢ "flora" ]

-- | The two files every family is given, as paths RELATIVE to the
--   family directory. Items get a nested second file so the recursive
--   walk is exercised too, handed back in a deliberately non-canonical
--   enumeration order so the sort is asserted rather than assumed.
famRels ∷ Fam → [Text]
famRels f
    | famTree f = ["nested/f2.yaml", "f1.yaml"]
    | otherwise = ["f1.yaml", "f2.yaml"]

-- | The order the queue must actually load them in: as enumerated for a
--   flat directory, canonically sorted for a tree.
famLoadOrder ∷ Fam → [Text]
famLoadOrder f
    | famTree f = ["f1.yaml", "nested/f2.yaml"]
    | otherwise = ["f1.yaml", "f2.yaml"]

-- | One file's stubbed return value: distinguishable per family AND per
--   file, so no aggregate can be right by coincidence and no family's
--   sum can borrow another's.
famCount ∷ Int → Text → Int
famCount ix rel = 10 * ix + (if "f1.yaml" `T.isSuffixOf` rel then 1 else 2)

-- | @(family index, family)@ for the twelve, indices fixed by
--   'normalFams' so arena's eleven keep the same numbers.
indexedFams ∷ [(Int, Fam)]
indexedFams = zip [1 ..] normalFams

famIndex ∷ Fam → Int
famIndex f =
    sum [ i | (i, g) ← indexedFams, famId g ≡ famId f ]

famPaths ∷ Fam → [Text]
famPaths f = [ famDir f <> "/" <> rel | rel ← famLoadOrder f ]

famSum ∷ Fam → Int
famSum f = sum [ famCount (famIndex f) rel | rel ← famLoadOrder f ]

-----------------------------------------------------------------------
-- The Lua half: the real startup_loader against doubles
-----------------------------------------------------------------------

-- | How one scenario deviates from "every family has its two files and
--   every file returns its own count".
data Scenario = Scenario
    { scAbsent ∷ [Text]  -- ^ family directories whose enumeration answers nil
    , scZero   ∷ [Text]  -- ^ family directories whose files all return 0
    }

fullScenario ∷ Scenario
fullScenario = Scenario [] []

luaQuoted ∷ Text → Text
luaQuoted t = "'" <> t <> "'"

luaTable ∷ [(Text, Text)] → Text
luaTable kvs = "{ " <> T.intercalate ", "
    [ "[" <> luaQuoted k <> "] = " <> v | (k, v) ← kvs ] <> " }"

-- | Everything @scripts/startup_loader.lua@ reaches for outside its own
--   module. Every loader records the path it was handed and answers
--   that file's count as a Lua FLOAT — which is exactly what
--   @Lua.pushnumber@ hands back in production, and is why the loader
--   has to floor before it formats.
luaPrelude ∷ Scenario → Text
luaPrelude sc = T.unlines $
    [ "local infos, warns, calls = {}, {}, {}"
    , "local files = " <> filesTable
    , "local counts = " <> countsTable
    , "engine = {}"
    , "engine.logInfo = function(m) infos[#infos + 1] = m end"
    , "engine.logWarn = function(m) warns[#warns + 1] = m end"
    , "engine.loadTexture = function() end"
    , "engine.loadTutorialDir = function() end"
    , "engine.listFiles = function(dir, ext) return files[dir] end"
    , "engine.listFilesRecursive = function(dir, ext) return files[dir] end"
    , "local function loader(dir)"
    , "  return function(p)"
    , "    calls[#calls + 1] = p"
    , "    local n = counts[p]"
    , "    if n == nil then error('unexpected path: ' .. tostring(p)) end"
    , "    return n + 0.0"
    , "  end"
    , "end"
    ]
    ⧺ [ "engine." <> famVerb f <> " = loader(" <> luaQuoted (famDir f) <> ")"
      | f ← normalFams ]
    ⧺ [ "local SL = require('scripts.startup_loader')"
      , "function runProfile(profile)"
      , "  infos, warns, calls = {}, {}, {}"
      , "  SL.build(profile)"
      , "  local guard = 0"
      , "  while not SL.isDone() do"
      , "    SL.tick(0)"
      , "    guard = guard + 1"
      , "    assert(guard < 100000, 'the startup queue never drained')"
      , "  end"
      , "  return table.concat(infos, '\\n') .. '\\n@@WARNS@@\\n'"
      , "      .. table.concat(warns, '\\n') .. '\\n@@CALLS@@\\n'"
      , "      .. table.concat(calls, '\\n')"
      , "end"
      ]
  where
    filesTable = luaTable
        [ (famDir f, "{ " <> T.intercalate ", "
              (map luaQuoted (famRels f)) <> " }")
        | f ← normalFams, famDir f `notElem` scAbsent sc ]
    countsTable = luaTable
        [ ( famDir f <> "/" <> rel
          , tshow (if famDir f `elem` scZero sc
                       then 0 else famCount (famIndex f) rel) )
        | f ← normalFams, rel ← famLoadOrder f ]

-- | One profile run's observable output.
data RunResult = RunResult
    { rrInfos ∷ [Text]  -- ^ every @engine.logInfo@ message, in order
    , rrWarns ∷ [Text]  -- ^ every @engine.logWarn@ message, in order
    , rrCalls ∷ [Text]  -- ^ every path handed to a loader, in order
    } deriving (Show, Eq)

-- | Drive one profile end to end. A Lua failure surfaces as a
--   RunResult whose infos hold the Lua message, so the example fails
--   naming it rather than on an opaque empty comparison.
runProfile ∷ Scenario → Text → IO RunResult
runProfile sc profile = do
    out ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        pre ← Lua.dostring (TE.encodeUtf8 (luaPrelude sc))
        case pre of
            Lua.OK → do
                st ← Lua.dostring
                        (TE.encodeUtf8 ("return runProfile('" <> profile <> "')"))
                case st of
                    Lua.OK → maybe "" TE.decodeUtf8Lenient <$> Lua.tostring (-1)
                    _      → ("lua error: " <>) <$> luaMessage
            _ → ("lua prelude error: " <>) <$> luaMessage
    pure (parseRun out)
  where
    luaMessage = do
        err ← Lua.tostring (-1)
        pure (maybe "<no message>" TE.decodeUtf8Lenient err)

parseRun ∷ Text → RunResult
parseRun out =
    RunResult (nonEmpty infoPart) (nonEmpty warnPart) (nonEmpty callPart)
  where
    (infoPart, rest0) = T.breakOn warnMark out
    (warnPart, rest1) = T.breakOn callMark (T.drop (T.length warnMark) rest0)
    callPart          = T.drop (T.length callMark) rest1
    warnMark = "@@WARNS@@"
    callMark = "@@CALLS@@"
    nonEmpty t = [ l | l ← T.lines t, not (T.null l) ]

-- | The aggregate lines only, located by their stable prefix — never by
--   a line count over everything the loader logged.
aggregates ∷ RunResult → [Text]
aggregates = filter ("Startup assets: " `T.isPrefixOf`) ∘ rrInfos

-- | The aggregate line the loader must emit for one family.
expectedAggregate ∷ Fam → Int → Int → Text
expectedAggregate f total files =
    "Startup assets: " <> famId f <> " loaded " <> tshow total
    <> " from " <> tshow files <> " file(s)"

-----------------------------------------------------------------------
-- The engine half: the twelve real bindings
-----------------------------------------------------------------------

-- | A private headless engine plus a real Lua backend with the whole
--   engine API registered, and a logger that records every entry with
--   @CatAsset@ Debug switched on (off by default, which is exactly the
--   state in which a moved line would otherwise vanish).
--
--   PRIVATE deliberately: these examples register real shipped
--   definitions into content registries, and the suite's shared engine
--   must not inherit them.
data Bindings = Bindings
    { bnLua ∷ LuaBackendState
    , bnLog ∷ IORef [LogEntry]
    }

newBindings ∷ IO Bindings
newBindings = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend         = LogToCallback (\e → modifyIORef' ref (e :))
        , lcDebugCategories = [CatAsset]
        }
    writeIORef (loggerRef env) logger
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure (Bindings ls ref)

-- | Call one binding through the real Lua API and return what it handed
--   back to Lua alongside every entry it logged.
--
--   The count is read back through @string.format('%d', ...)@ because
--   the binding pushes a Lua NUMBER (a double): asking Lua to render it
--   is what makes "the value returned to Lua" and "the value asserted
--   here" the same quantity.
callBinding ∷ Bindings → Text → Text → IO (Int, [LogEntry])
callBinding b verb path = do
    writeIORef (bnLog b) []
    out ← executeDebugLua (lbsLuaState (bnLua b))
        ("return string.format('%d', engine." <> verb
         <> "('" <> path <> "'))")
    entries ← reverse <$> readIORef (bnLog b)
    pure (digits out, entries)
  where
    digits t = case T.filter isDigit t of
        d | T.null d  → -1
          | otherwise → read (T.unpack d)

entriesAt ∷ LogLevel → [LogEntry] → [Text]
entriesAt lvl = map leMessage ∘ filter ((≡ lvl) ∘ leLevel)

-- | Entries mentioning one binding, at one level.
mentioning ∷ Text → LogLevel → [LogEntry] → [Text]
mentioning verb lvl = filter (verb `T.isInfixOf`) ∘ entriesAt lvl

-- | One family's Debug detail contract: the phrase carrying the
--   AUTHORITATIVE count that binding returned. Spelled per family
--   because the quantity is not the same one across the twelve —
--   materials, vegetation and flora count TEXTURES, loot tables count
--   0 or 1 per file, and the rest count definitions.
detailFor ∷ Text → Int → Text
detailFor verb n = case verb of
    "loadMaterialYaml"   → "loaded " <> tshow n <> " textures"
    "loadVegetationYaml" → "loaded " <> tshow n <> " textures"
    "loadFloraYaml"      → "(" <> tshow n <> " textures)"
    "loadSubstanceYaml"  → "loaded " <> tshow n <> " substances"
    "loadInfectionYaml"  → "loaded " <> tshow n <> " infections"
    "loadRecipeYaml"     → "loaded " <> tshow n <> " recipes"
    "loadItemYaml"       → "loaded " <> tshow n <> " item definitions"
    "loadEquipmentYaml"  → "loaded " <> tshow n <> " equipment classes"
    "loadBuildingYaml"   → "loaded " <> tshow n <> " building definitions"
    "loadUnitYaml"       → "loaded " <> tshow n <> " unit definitions"
    "loadLootTableYaml"  → "loaded " <> tshow n <> " loot table"
    "loadLocationYaml"   → "loaded " <> tshow n <> " locations"
    _                    → "loaded " <> tshow n

-- | One real shipped file per family — real data through the real
--   binding, so the assertion is about production's own behaviour
--   rather than a fixture's.
shippedFile ∷ Text → Text
shippedFile verb = case verb of
    "loadMaterialYaml"   → "data/materials/glacial.yaml"
    "loadVegetationYaml" → "data/vegetation/grasses.yaml"
    "loadFloraYaml"      → "data/flora/saguaro.yaml"
    "loadSubstanceYaml"  → "data/substances/metals.yaml"
    "loadInfectionYaml"  → "data/infections/bacteria.yaml"
    "loadRecipeYaml"     → "data/recipes/basic.yaml"
    "loadItemYaml"       → "data/items/axe_steel.yaml"
    "loadEquipmentYaml"  → "data/equipment/humanoid.yaml"
    "loadBuildingYaml"   → "data/buildings/furnace.yaml"
    "loadUnitYaml"       → "data/units/acolyte.yaml"
    "loadLootTableYaml"  → "data/loot_tables/ruin_common.yaml"
    "loadLocationYaml"   → "data/locations/ruin_small.yaml"
    _                    → ""

-- | Other bindings a family's shipped file must be loaded AFTER, as
--   @(verb, path)@ pairs called first.
--
--   Only locations have one, and it is a REAL production ordering, not
--   a test convenience: since #917 a location's guaranteed significant
--   content must resolve against the item registry, so
--   @engine.loadLocationYaml@ rejects the whole file when the item it
--   names is unregistered. @scripts/startup_loader.lua@ already loads
--   items before locations (and @data/locations/*.yaml@'s own header
--   says so); this establishes the same order for a case that
--   otherwise calls one binding in isolation.
shippedPrereqs ∷ Text → [(Text, Text)]
shippedPrereqs verb = case verb of
    "loadLocationYaml" → [("loadItemYaml", "data/items/processing_unit.yaml")]
    _                  → []

-----------------------------------------------------------------------
-- Fixture files for the warning cases
-----------------------------------------------------------------------

withFixtureDir ∷ String → [(FilePath, String)] → (FilePath → IO α) → IO α
withFixtureDir label files action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> ("synarchy-1930-" ⧺ label)
    removeIfPresent root
    createDirectoryIfMissing True root
    forM_ files $ \(rel, contents) → writeFile (root </> rel) contents
    action root `finally` removeIfPresent root
  where
    removeIfPresent p = do
        present ← doesDirectoryExist p
        when present $ removeDirectoryRecursive p

-- | A material whose three textures do not exist, so the missing-texture
--   substitution warning fires three times per definition.
missingTextureMaterial ∷ String
missingTextureMaterial = unlines
    [ "materials:"
    , "  - id: 250"
    , "    name: probe_missing_tex_1930"
    , "    move_cost: 1.0"
    , "    hardness: 0.5"
    , "    density: 2.0"
    , "    albedo: 0.2"
    , "    drainage: 0.2"
    , "    pick_speed: 0.5"
    , "    shovel_speed: 0.5"
    , "    tile: \"assets/textures/probe_1930_absent_tile.png\""
    , "    zoom: \"assets/textures/probe_1930_absent_zoom.png\""
    , "    bg:   \"assets/textures/probe_1930_absent_bg.png\""
    ]

-- | A flora species naming a soil no material registry holds.
unresolvedSoilFlora ∷ String
unresolvedSoilFlora = unlines
    [ "flora:"
    , "  - name: probe_soil_1930"
    , "    type: groundcover"
    , "    texDir: \"assets/textures/flora\""
    , "    worldGen:"
    , "      category: groundcover"
    , "      soils: [\"probe_1930_no_such_soil\"]"
    , "      minTemp: 0"
    , "      maxTemp: 40"
    , "      idealTemp: 20"
    , "      minPrecip: 0"
    , "      maxPrecip: 100"
    , "      idealPrecip: 50"
    ]

-- | One item definition, written to two different files so loading both
--   exercises the duplicate-replacement diagnostic.
duplicateItem ∷ String
duplicateItem = unlines
    [ "items:"
    , "  - name: \"probe_dup_1930\""
    , "    sprite: \"assets/textures/items/probe.png\""
    , "    weight: 1.0"
    , "    bulk: 1.0"
    ]

-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "Startup asset logging" $ do

    ------------------------------------------------------------------
    describe "the loader's per-family aggregates (requirements 2-4)" $ do

        it "normal startup emits exactly the twelve scoped aggregates, \
           \each the sum of that family's own returned values" $ do
            r ← runProfile fullScenario "normal"
            aggregates r `shouldBe`
                [ expectedAggregate f (famSum f) 2 | f ← normalFams ]

        it "arena startup emits exactly eleven — the same inventory with \
           \NO flora aggregate" $ do
            r ← runProfile fullScenario "arena"
            aggregates r `shouldBe`
                [ expectedAggregate f (famSum f) 2 | f ← arenaFams ]
            filter ("Startup assets: flora " `T.isPrefixOf`) (rrInfos r)
                `shouldBe` []

        it "renders each sum as an INTEGER, though every binding returns \
           \a Lua float" $ do
            r ← runProfile fullScenario "normal"
            [ l | l ← aggregates r, "." `T.isInfixOf` l ] `shouldBe` []

        it "loads every file exactly once, in the existing family and \
           \per-family order (requirement 7)" $ do
            r ← runProfile fullScenario "normal"
            rrCalls r `shouldBe` concatMap famPaths normalFams

        it "keeps arena's own family order and file order too" $ do
            r ← runProfile fullScenario "arena"
            rrCalls r `shouldBe` concatMap famPaths arenaFams

        it "reports a family whose files all return zero as a zero \
           \aggregate over the files it still loaded — and warns about \
           \nothing (requirements 4 and 5)" $ do
            let sc = Scenario [] ["data/recipes"]
            r ← runProfile sc "normal"
            [ l | l ← aggregates r, "recipe " `T.isInfixOf` l ]
                `shouldBe` [ "Startup assets: recipe loaded 0 from 2 file(s)" ]
            rrWarns r `shouldBe` []

        it "still emits an aggregate for a family whose directory yields \
           \NO files at all (requirement 4)" $ do
            let sc = Scenario ["data/infections"] []
            r ← runProfile sc "normal"
            [ l | l ← aggregates r, "infection " `T.isInfixOf` l ]
                `shouldBe`
                [ "Startup assets: infection loaded 0 from 0 file(s)" ]
            length (aggregates r) `shouldBe` 12
            rrWarns r `shouldBe` []
            -- and the family is simply skipped, not loaded with a phantom path
            [ p | p ← rrCalls r, "data/infections" `T.isPrefixOf` p ]
                `shouldBe` []

        it "keeps its own queued-count line, which is not an aggregate" $ do
            r ← runProfile fullScenario "normal"
            length [ l | l ← rrInfos r
                       , "Startup loader queued " `T.isPrefixOf` l ]
                `shouldBe` 1

    ------------------------------------------------------------------
    describe "the engine bindings' per-file detail (requirement 1)"
        $ beforeAll newBindings $ do

        forM_ normalFams $ \f →
            it (T.unpack (famVerb f) ⧺ " logs its full path and the count \
                \it returned at Debug, and nothing at Info") $ \b → do
                let path = shippedFile (famVerb f)
                forM_ (shippedPrereqs (famVerb f)) $ \(verb, dep) →
                    void (callBinding b verb dep)
                (n, entries) ← callBinding b (famVerb f) path
                n `shouldSatisfy` (> 0)
                mentioning (famVerb f) LevelInfo entries `shouldBe` []
                let debugs = mentioning (famVerb f <> ": loaded")
                                        LevelDebug entries
                length debugs `shouldBe` 1
                [ m | m ← debugs, path `T.isInfixOf` m
                    , detailFor (famVerb f) n `T.isInfixOf` m ]
                    `shouldBe` debugs

    ------------------------------------------------------------------
    describe "existing diagnostics keep their level and meaning \
             \(requirements 5 and 6)" $ beforeAll newBindings $ do

        it "a YAML parse failure still warns, and the binding still \
           \reports its zero at Debug" $ \b →
            withFixtureDir "parse-failure"
                [ ("broken.yaml", "materials:\n  - id: [unclosed\n") ]
                $ \root → do
                let path = T.pack (root </> "broken.yaml")
                (n, entries) ← callBinding b "loadMaterialYaml" path
                n `shouldBe` 0
                length [ m | m ← entriesAt LevelWarn entries
                           , "Failed to parse material YAML" `T.isInfixOf` m ]
                    `shouldBe` 1
                mentioning "loadMaterialYaml" LevelInfo entries `shouldBe` []
                mentioning "loadMaterialYaml: loaded 0 textures"
                           LevelDebug entries `shouldSatisfy` (not ∘ null)

        it "a valid file with zero entries does NOT warn, and keeps its \
           \path and zero at Debug" $ \b →
            withFixtureDir "empty-list" [ ("empty.yaml", "materials: []\n") ]
                $ \root → do
                let path = T.pack (root </> "empty.yaml")
                (n, entries) ← callBinding b "loadMaterialYaml" path
                n `shouldBe` 0
                entriesAt LevelWarn entries `shouldBe` []
                entriesAt LevelInfo entries `shouldBe` []
                let debugs = mentioning "loadMaterialYaml: loaded"
                                        LevelDebug entries
                length debugs `shouldBe` 1
                [ m | m ← debugs, path `T.isInfixOf` m
                    , "loaded 0 textures" `T.isInfixOf` m ] `shouldBe` debugs

        it "missing-texture substitution still warns, once per absent \
           \texture" $ \b →
            withFixtureDir "missing-texture"
                [ ("mat.yaml", missingTextureMaterial) ] $ \root → do
                let path = T.pack (root </> "mat.yaml")
                (_, entries) ← callBinding b "loadMaterialYaml" path
                length [ m | m ← entriesAt LevelWarn entries
                           , "texture missing:" `T.isInfixOf` m
                           , "substituting" `T.isInfixOf` m ]
                    `shouldBe` 3

        it "an unresolved flora soil reference still warns" $ \b →
            withFixtureDir "flora-soil"
                [ ("flora.yaml", unresolvedSoilFlora) ] $ \root → do
                let path = T.pack (root </> "flora.yaml")
                (_, entries) ← callBinding b "loadFloraYaml" path
                length [ m | m ← entriesAt LevelWarn entries
                           , "probe_1930_no_such_soil" `T.isInfixOf` m
                           , "does not match any registered material name"
                                 `T.isInfixOf` m ]
                    `shouldBe` 1

        it "a duplicate item definition still warns on the replacement" $ \b →
            withFixtureDir "duplicate-item"
                [ ("one.yaml", duplicateItem)
                , ("two.yaml", duplicateItem) ] $ \root → do
                (_, first) ← callBinding b "loadItemYaml"
                                 (T.pack (root </> "one.yaml"))
                length [ m | m ← entriesAt LevelWarn first
                           , "duplicate item definition id" `T.isInfixOf` m ]
                    `shouldBe` 0
                (_, second) ← callBinding b "loadItemYaml"
                                  (T.pack (root </> "two.yaml"))
                length [ m | m ← entriesAt LevelWarn second
                           , "duplicate item definition id \"probe_dup_1930\""
                                 `T.isInfixOf` m ]
                    `shouldBe` 1
