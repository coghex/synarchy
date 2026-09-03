{-# LANGUAGE OverloadedStrings #-}
-- | Startup READINESS, not startup dispatch (#2203).
--
--   Before this the startup loader measured only whether it had called
--   everything it queued: a @data/@ family whose directory had vanished
--   cost one log line, and a file that failed to parse was
--   indistinguishable from one that legitimately held nothing, because
--   both reached Lua as the same @0@. Either way the bar hit 100%, the
--   loader reported complete, and the main menu opened over a
--   half-populated registry set.
--
--   Three layers own three halves of the fix, and this module pins each
--   against the real production code:
--
--   * the twelve @engine.load*Yaml@ BINDINGS answer a decode failure
--     distinguishably — but only when asked, so the single numeric
--     result every other caller reads is untouched;
--   * @scripts\/startup_loader.lua@ turns a family that discovered no
--     files, or that had any file fail to parse, into a TERMINAL failed
--     state — after emitting that family's #1930 aggregate unchanged;
--   * @scripts\/loading_screen.lua@ and @scripts\/ui_manager_boot.lua@
--     surface it and refuse to finish the boot, on BOTH profiles.
--
--   The Lua halves run the REAL scripts in a bare Lua VM against
--   stubbed engine doubles — the technique 'Test.Headless.Item.Discovery'
--   established and 'Test.Headless.Startup.AssetLogging' already uses
--   for the same two files. Stubbing is what makes each file's parse
--   outcome selectable, which is the only way "the queue stopped at the
--   family that broke" can be told from "the queue stopped".
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Startup readiness"'@.
module Test.Headless.Startup.Readiness (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.Char (isDigit)
import Data.IORef (IORef, newIORef, writeIORef, modifyIORef')
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory
    , removeDirectoryRecursive )
import System.FilePath ((</>))
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Harness.Isolation
    ( isInsideIsolatedResourceRoot, withIsolatedResourceRoot )
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogEntry(..) )
import Engine.Core.State
    ( loggerRef, luaToEngineQueue, luaQueue, assetPoolRef
    , nextObjectIdRef, inputStateRef )
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))

-----------------------------------------------------------------------
-- The families the two profiles queue
-----------------------------------------------------------------------

-- | One registry family: the directory the loader enumerates, the
--   stable identifier its aggregate reports under, and its binding.
data Fam = Fam
    { famDir  ∷ Text
    , famId   ∷ Text
    , famVerb ∷ Text
    }

-- | @queueNormalProfile@'s twelve, in its own order.
normalFams ∷ [Fam]
normalFams =
    [ Fam "data/materials"   "material"   "loadMaterialYaml"
    , Fam "data/vegetation"  "vegetation" "loadVegetationYaml"
    , Fam "data/flora"       "flora"      "loadFloraYaml"
    , Fam "data/substances"  "substance"  "loadSubstanceYaml"
    , Fam "data/infections"  "infection"  "loadInfectionYaml"
    , Fam "data/recipes"     "recipe"     "loadRecipeYaml"
    , Fam "data/items"       "item"       "loadItemYaml"
    , Fam "data/equipment"   "equipment"  "loadEquipmentYaml"
    , Fam "data/buildings"   "building"   "loadBuildingYaml"
    , Fam "data/units"       "unit"       "loadUnitYaml"
    , Fam "data/loot_tables" "loot_table" "loadLootTableYaml"
    , Fam "data/locations"   "location"   "loadLocationYaml"
    ]

-- | @queueArenaProfile@'s eleven: the same inventory minus flora.
arenaFams ∷ [Fam]
arenaFams = [ f | f ← normalFams, famId f ≢ "flora" ]

profileFams ∷ Text → [Fam]
profileFams "arena" = arenaFams
profileFams _       = normalFams

-- | Every family is given two files, each returning 3.
famFiles ∷ [Text]
famFiles = ["a.yaml", "b.yaml"]

perFileCount ∷ Int
perFileCount = 3

-----------------------------------------------------------------------
-- The bare-VM harness
-----------------------------------------------------------------------

-- | How one scenario deviates from "every family has its two files and
--   every one of them parses".
data Scenario = Scenario
    { scEmpty  ∷ [Text]  -- ^ family directories that enumerate NOTHING
    , scBroken ∷ [Text]  -- ^ full file paths whose loader reports a
                         --   failed parse
    } deriving (Show, Eq)

healthy ∷ Scenario
healthy = Scenario [] []

quoted ∷ Text → Text
quoted t = "'" <> t <> "'"

luaList ∷ [Text] → Text
luaList xs = "{ " <> T.intercalate ", " (map quoted xs) <> " }"

luaSet ∷ [Text] → Text
luaSet xs = "{ " <> T.intercalate ", "
    [ "[" <> quoted x <> "] = true" | x ← xs ] <> " }"

-- | The engine surface both scripts reach for. Every loader records the
--   path it was handed and answers as production's bindings do: one Lua
--   FLOAT by default, and the count plus a parse flag when the caller
--   opts in with a truthy second argument.
enginePrelude ∷ Scenario → [Text]
enginePrelude sc =
    [ "infos, warns, errors, calls = {}, {}, {}, {}"
    , "local files  = " <> filesTable
    , "local broken = " <> luaSet (scBroken sc)
    , "engine = {}"
    , "engine.logInfo  = function(m) infos[#infos + 1] = m end"
    , "engine.logWarn  = function(m) warns[#warns + 1] = m end"
    , "engine.logError = function(m) errors[#errors + 1] = m end"
    , "engine.logDebug = function() end"
    , "engine.loadTexture = function() return 1 end"
    , "engine.loadTutorialDir = function() end"
    , "engine.listFiles = function(dir) return files[dir] end"
    , "engine.listFilesRecursive = function(dir) return files[dir] end"
    , "local function loader(p, wantOutcome)"
    , "  calls[#calls + 1] = p"
    , "  if wantOutcome then return "
        <> tshow perFileCount <> ".0, not broken[p] end"
    , "  return " <> tshow perFileCount <> ".0"
    , "end"
    ]
    ⧺ [ "engine." <> famVerb f <> " = loader" | f ← normalFams ]
  where
    filesTable = "{ " <> T.intercalate ", "
        [ "[" <> quoted (famDir f) <> "] = "
            <> (if famDir f `elem` scEmpty sc then "{}" else luaList famFiles)
        | f ← normalFams ] <> " }"

-- | Everything the loader-only half needs on top of the engine double.
loaderPrelude ∷ Scenario → Text
loaderPrelude sc = T.unlines $ enginePrelude sc ⧺
    [ "SL = require('scripts.startup_loader')"
    -- Bounded: #2203 gave the queue a SECOND terminal state, and a
    -- drain that respects only `done` is exactly the arena hang the
    -- guard below would otherwise hide.
    , "function drain()"
    , "  local guard = 0"
    , "  while not SL.isDone() and not SL.isFailed() do"
    , "    SL.tick(0)"
    , "    guard = guard + 1"
    , "    assert(guard < 100000, 'the startup queue never drained')"
    , "  end"
    , "end"
    , "function report()"
    , "  local f = SL.getFailure()"
    , "  local progress = SL.getProgress()"
    , "  return table.concat({"
    , "      tostring(SL.isDone()), tostring(SL.isFailed()),"
    , "      f and f.family or '-', f and f.kind or '-',"
    , "      f and (f.path or f.dir) or '-',"
    , "      f and tostring(f.failedCount) or '-',"
    , "      f and tostring(f.files) or '-',"
    , "      f and f.message or '-',"
    , "      string.format('%.6f', progress),"
    , "      tostring(#errors),"
    , "      tostring(#calls),"
    , "    }, '\\1')"
    , "end"
    ]

-- | One drained (or failed) run's observable state.
data Run = Run
    { rDone     ∷ Bool
    , rFailed   ∷ Bool
    , rFamily   ∷ Text  -- ^ @-@ when nothing failed
    , rKind     ∷ Text  -- ^ @empty@ | @parse@ | @-@
    , rWhere    ∷ Text  -- ^ the failing FILE, or the empty family's DIR
    , rFailed'  ∷ Text  -- ^ how many of the family's files failed
    , rFiles    ∷ Text  -- ^ how many the family discovered
    , rMessage  ∷ Text
    , rProgress ∷ Text
    , rErrors   ∷ Int   -- ^ error-level lines logged
    , rCalls    ∷ Int   -- ^ loader invocations
    , rInfos    ∷ [Text]
    } deriving (Show, Eq)

aggregatesOf ∷ Run → [Text]
aggregatesOf = filter ("Startup assets: " `T.isPrefixOf`) ∘ rInfos

-- | Run one Lua chunk against the bare VM, returning its string result.
inVM ∷ Text → Text → IO Text
inVM prelude code = Lua.run @Lua.Exception $ do
    Lua.openlibs
    pre ← Lua.dostring (TE.encodeUtf8 prelude)
    case pre of
        Lua.OK → do
            st ← Lua.dostring (TE.encodeUtf8 code)
            case st of
                Lua.OK → maybe "" TE.decodeUtf8Lenient <$> Lua.tostring (-1)
                _      → ("lua error: " <>) <$> message
        _ → ("lua prelude error: " <>) <$> message
  where
    message = do
        err ← Lua.tostring (-1)
        pure (maybe "<no message>" TE.decodeUtf8Lenient err)

-- | Build one profile, drain it, and read back everything about how it
--   ended. Extra Lua statements run between the build and the drain.
runProfileWith ∷ Scenario → Text → [Text] → IO Run
runProfileWith sc profile extra = do
    out ← inVM (loaderPrelude sc) $ T.unlines $
        [ "SL.build('" <> profile <> "')" ] ⧺ extra ⧺
        [ "drain()"
        , "return report() .. '\\1' .. table.concat(infos, '\\2')"
        ]
    pure (parseRun out)

runProfile ∷ Scenario → Text → IO Run
runProfile sc profile = runProfileWith sc profile []

parseRun ∷ Text → Run
parseRun out = case T.splitOn "\1" out of
    (dn : fl : fam : knd : whr : fc : fs : msg : prog : errs : cls : rest) →
        Run { rDone     = dn ≡ "true"
            , rFailed   = fl ≡ "true"
            , rFamily   = fam
            , rKind     = knd
            , rWhere    = whr
            , rFailed'  = fc
            , rFiles    = fs
            , rMessage  = msg
            , rProgress = prog
            , rErrors   = readCount errs
            , rCalls    = readCount cls
            , rInfos    = case rest of
                  (infos : _) → [ l | l ← T.splitOn "\2" infos, not (T.null l) ]
                  _           → []
            }
    _ → Run False False "?" "?" out "?" "?" out "?" (-1) (-1) [out]
  where
    readCount t | T.all isDigit t, not (T.null t) = read (T.unpack t)
                | otherwise                       = -1

-- | The aggregate line spelling @scripts/startup_loader.lua@ owns,
--   restated here only so a change to it fails loudly (#1930).
aggregateLine ∷ Fam → Int → Int → Text
aggregateLine f total files =
    "Startup assets: " <> famId f <> " loaded " <> tshow total
    <> " from " <> tshow files <> " file(s)"

-- | Every aggregate a profile emits when it runs to the end.
allAggregates ∷ Text → [Text]
allAggregates profile =
    [ aggregateLine f (perFileCount * length famFiles) (length famFiles)
    | f ← profileFams profile ]

-----------------------------------------------------------------------
-- The UI integration harness: the real loading screen and boot module
-----------------------------------------------------------------------

-- | @scripts/ui_manager_boot.lua@ pulls in most of the UI tree at load
--   time. Everything here is a recording no-op so the two modules under
--   test — @scripts/loading_screen.lua@ and
--   @scripts/startup_loader.lua@ — are the only real ones in the graph.
--
--   @scripts.ui.scale@, @.responsive@ and @.text_wrap@ are deliberately
--   REAL: the loading screen's own @createUI@ does arithmetic on what
--   they return, so a stub would only prove the stub.
bootStubs ∷ [Text]
bootStubs =
    [ "scripts.ui.box_textures", "scripts.ui.textbox", "scripts.ui.checkbox"
    , "scripts.ui.button", "scripts.ui.dropdown", "scripts.ui.tabbar"
    , "scripts.ui.randbox", "scripts.ui.toggle", "scripts.ui.list"
    , "scripts.ui.focus_indicator", "scripts.ui.item_list", "scripts.popup"
    , "scripts.event_log", "scripts.combat_log", "scripts.injury_log_panel"
    , "scripts.unit_log", "scripts.ui.context_menu", "scripts.unit_info_v2"
    , "scripts.main_menu", "scripts.settings_menu", "scripts.create_world_menu"
    , "scripts.world_manager", "scripts.world_view", "scripts.save_browser"
    , "scripts.hud", "scripts.test_arena", "scripts.pause_menu"
    , "scripts.build_tool_remote_warning", "scripts.lib.session_teardown"
    , "scripts.ui.panel", "scripts.debug"
    ]

-- | Boot the real @uiManager@ the way the engine does, on one profile.
bootPrelude ∷ Scenario → Text → Text
bootPrelude sc profile = T.unlines $ enginePrelude sc ⧺
    [ "menus = {}"
    , "engine.getBootProfile = function() return '" <> profile <> "' end"
    , "engine.getUIScale = function() return 1.0 end"
    , "engine.getTooltipDwellMs = function() return 0 end"
    , "engine.getTooltipHintDelayMs = function() return 0 end"
    , "engine.getMousePosition = function() return nil end"
    , "engine.getWindowSize = function() return 800, 600 end"
    , "local nextFont = 100"
    , "engine.loadFont = function() nextFont = nextFont + 1 "
        <> "return nextFont end"
    -- Any stub member is a function answering numbers, so the real
    -- createUI's layout arithmetic runs instead of erroring on nil.
    , "local function anyFn() return function() return 0, 0 end end"
    , "nextWidget, shownPages = 0, {}"
    , "UI = setmetatable({"
    , "  newPage  = function() nextWidget = nextWidget + 1"
    , "                        return nextWidget end,"
    , "  showPage = function(p) shownPages[#shownPages + 1] = p end,"
    , "}, {__index = anyFn})"
    ]
    ⧺ [ "package.preload['" <> m <> "'] = function()"
        <> " return setmetatable({}, {__index = anyFn}) end"
      | m ← bootStubs ]
    -- These two are polled unconditionally in uiManager.update and must
    -- answer "nothing is being dragged" rather than a truthy 0.
    -- Recording doubles: every label and bar gets its OWN id, so the
    -- status line and the progress the screen actually carries can be
    -- read back rather than inferred from the loader's own state.
    ⧺ [ "labelTexts, barProgress = {}, {}"
      , "package.preload['scripts.ui.label'] = function()"
      , "  return setmetatable({"
      , "    new = function() nextWidget = nextWidget + 1"
      , "                     return nextWidget end,"
      , "    setText = function(id, t) labelTexts[id] = t end,"
      , "  }, {__index = anyFn}) end"
      , "package.preload['scripts.ui.bar'] = function()"
      , "  return setmetatable({"
      , "    new = function() nextWidget = nextWidget + 1"
      , "                     return nextWidget end,"
      , "    setProgress = function(id, p) barProgress[id] = p end,"
      , "  }, {__index = anyFn}) end"
      , "for _, m in ipairs({'scripts.ui.slider', 'scripts.ui.scrollbar'}) do"
      , "  package.preload[m] = function()"
      , "    return setmetatable({getDraggingId = function() return nil end},"
      , "                        {__index = anyFn})"
      , "  end"
      , "end"
      , "uiManager = { moduleReady = {}, fbW = 0, fbH = 0 }"
      , "uiManager.showMenu = function(n) menus[#menus + 1] = tostring(n) end"
      , "package.loaded['scripts.ui_manager'] = uiManager"
      , "require('scripts.ui_manager_boot')"
      -- The real post-resize reflow set boot's own resize handler
      -- calls; every module it pulls in is already stubbed above.
      , "require('scripts.ui_manager_resize')"
      , "SL = require('scripts.startup_loader')"
      , "LS = require('scripts.loading_screen')"
      -- The real boot sequence: init, a framebuffer, then both fonts
      -- arriving is what makes checkReady act.
      -- The one rebuild entry point every resize and UI-scale reflow
      -- reaches (scripts/ui/responsive.lua fans them all through it).
      , "function resize(w, h) uiManager.onFramebufferResize(w, h) end"
      , "function boot(frames)"
      , "  uiManager.init(1)"
      , "  uiManager.onFramebufferResize(800, 600)"
      , "  uiManager.onAssetLoaded('font', 101)"
      , "  uiManager.onAssetLoaded('font', 102)"
      , "  for _ = 1, frames do uiManager.update(0.016) end"
      , "end"
      , "function bootReport()"
      , "  local f = SL.getFailure()"
      , "  local complete = 0"
      , "  for _, m in ipairs(infos) do"
      , "    if m == 'Startup loader complete' then complete = complete + 1 end"
      , "  end"
      , "  return table.concat({"
      , "      tostring(LS.phase), table.concat(menus, ','),"
      , "      tostring(uiManager.startupBootDone), tostring(complete),"
      , "      tostring(SL.isDone()), tostring(SL.isFailed()),"
      , "      f and f.message or '-', tostring(#errors),"
      , "      labelTexts[LS.statusLabelId] or '-',"
      , "      string.format('%.6f', barProgress[LS.barId] or -1),"
      -- The page currently on screen, or '-' when the last thing shown
      -- is not the page the screen now owns (a rebuild that was never
      -- re-shown leaves exactly that).
      , "      (shownPages[#shownPages] == LS.page) and 'shown'"
      , "        or 'hidden',"
      , "    }, '\\1')"
      , "end"
      ]

-- | What a full boot ended up doing.
data Boot = Boot
    { bPhase     ∷ Text  -- ^ the loading screen's terminal phase
    , bMenus     ∷ [Text]
    , bBootDone  ∷ Bool  -- ^ @uiManager.startupBootDone@
    , bComplete  ∷ Int   -- ^ \"Startup loader complete\" lines logged
    , bLoadDone  ∷ Bool
    , bFailed    ∷ Bool
    , bMessage   ∷ Text
    , bErrors    ∷ Int
    , bStatus    ∷ Text  -- ^ the text the status LABEL is left carrying
    , bBar       ∷ Text  -- ^ the progress the BAR is left carrying
    , bShown     ∷ Text  -- ^ @shown@ when the page it owns is on screen
    } deriving (Show, Eq)

runBoot ∷ Scenario → Text → IO Boot
runBoot = runBootThen ""

-- | Boot, then run one extra Lua statement before reporting.
runBootThen ∷ Text → Scenario → Text → IO Boot
runBootThen after sc profile = do
    out ← inVM (bootPrelude sc profile)
              ("boot(400) " <> after <> " return bootReport()")
    pure $ case T.splitOn "\1" out of
        [ph, ms, bd, cp, ld, fl, msg, errs, status, barAt, shown] →
            Boot { bPhase    = ph
                 , bMenus    = [ m | m ← T.splitOn "," ms, not (T.null m) ]
                 , bBootDone = bd ≡ "true"
                 , bComplete = readInt cp
                 , bLoadDone = ld ≡ "true"
                 , bFailed   = fl ≡ "true"
                 , bMessage  = msg
                 , bErrors   = readInt errs
                 , bStatus   = status
                 , bBar      = barAt
                 , bShown    = shown
                 }
        _ → Boot out [] False (-1) False False out (-1) out out out
  where
    readInt t | T.all isDigit t, not (T.null t) = read (T.unpack t)
              | otherwise                       = -1

-----------------------------------------------------------------------
-- The engine half: the twelve real bindings
-----------------------------------------------------------------------

-- | A private headless engine plus a real Lua backend with the whole
--   engine API registered. PRIVATE deliberately: these examples
--   register real shipped definitions into content registries, and the
--   suite's shared engine must not inherit them.
data Bindings = Bindings
    { bnLua ∷ LuaBackendState
    , bnLog ∷ IORef [LogEntry]
    }

-- | Keep the scratch resource root alive for the entire binding group.
--   The isolation must wrap engine initialization itself (#1357): init
--   materializes local config overrides before any example receives the
--   fixture, so isolating only the returned 'Bindings' would be too late.
withBindings ∷ (Bindings → IO α) → IO α
withBindings action = withIsolatedResourceRoot $ do
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
    action (Bindings ls ref)

luaEval ∷ Bindings → Text → IO Text
luaEval b code = do
    writeIORef (bnLog b) []
    T.strip ∘ T.filter (≢ '"') <$> executeDebugLua (lbsLuaState (bnLua b)) code

-- | How many values one binding call actually hands back to Lua.
arityOf ∷ Bindings → Text → Text → IO Text
arityOf b verb argList =
    luaEval b ("return string.format('%d', select('#', engine."
               <> verb <> "(" <> argList <> ")))")

-- | @(count, parsed)@ as the opt-in call reports them.
outcomeOf ∷ Bindings → Text → Text → IO (Int, Bool)
outcomeOf b verb path = do
    out ← luaEval b ("return string.format('%d/%s', engine." <> verb
                     <> "('" <> path <> "', true))")
    let (nTxt, rest) = T.breakOn "/" out
    pure ( if T.null nTxt ∨ T.any (not ∘ isDigit) nTxt
               then -1 else read (T.unpack nTxt)
         , T.drop 1 rest ≡ "true" )

-- | One real shipped file per family — real data through the real
--   binding, so the assertion is about production's own behaviour.
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

-- | Locations resolve their guaranteed significant content against the
--   live item registry (#917), so the shipped file needs its item
--   loaded first — production's own order, not a test convenience.
shippedPrereqs ∷ Text → [(Text, Text)]
shippedPrereqs "loadLocationYaml" =
    [("loadItemYaml", "data/items/processing_unit.yaml")]
shippedPrereqs _ = []

-- | The top-level key each list family decodes, so an EMPTY one can be
--   written for it. Loot tables are absent on purpose: that file holds
--   exactly one definition rather than a list, so "parsed, and empty"
--   is not a state it has.
emptyListFile ∷ Text → Maybe String
emptyListFile verb = case verb of
    "loadMaterialYaml"   → Just "materials: []\n"
    "loadVegetationYaml" → Just "vegetation: []\n"
    "loadFloraYaml"      → Just "flora: []\n"
    "loadSubstanceYaml"  → Just "substances: []\n"
    "loadInfectionYaml"  → Just "infections: []\n"
    "loadRecipeYaml"     → Just "recipes: []\n"
    "loadItemYaml"       → Just "items: []\n"
    "loadEquipmentYaml"  → Just "classes: []\n"
    "loadBuildingYaml"   → Just "buildings: []\n"
    "loadUnitYaml"       → Just "units: []\n"
    "loadLocationYaml"   → Just "locations: []\n"
    _                    → Nothing

-- | Broken YAML SYNTAX, so the decode fails for every family the same
--   way — including loot tables, whose loader bypasses 'loadYamlList'
--   entirely and had the identical indistinguishable-zero problem.
malformedFile ∷ String
malformedFile = "entries:\n  - id: [unclosed\n"

withFixtureDir ∷ String → [(FilePath, String)] → (FilePath → IO α) → IO α
withFixtureDir label files action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> ("synarchy-2203-" ⧺ label)
    removeIfPresent root
    createDirectoryIfMissing True root
    forM_ files $ \(rel, contents) → writeFile (root </> rel) contents
    action root `finally` removeIfPresent root
  where
    removeIfPresent p = do
        present ← doesDirectoryExist p
        when present $ removeDirectoryRecursive p

-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "Startup readiness" $ do

    ------------------------------------------------------------------
    describe "the loader's terminal failure (requirements 2-4)" $ do

        it "drains clean and reports done when every family parses" $ do
            r ← runProfile healthy "normal"
            (rDone r, rFailed r) `shouldBe` (True, False)
            aggregatesOf r `shouldBe` allAggregates "normal"
            rErrors r `shouldBe` 0

        it "fails on a family whose directory yields NO files, naming \
           \the family and the DIRECTORY it looked in" $ do
            r ← runProfile (Scenario ["data/recipes"] []) "normal"
            (rDone r, rFailed r) `shouldBe` (False, True)
            (rFamily r, rKind r, rWhere r)
                `shouldBe` ("recipe", "empty", "data/recipes")
            rMessage r `shouldBe`
                "Startup failed: recipe discovered no YAML files in \
                \data/recipes"
            rErrors r `shouldBe` 1

        it "emits that family's own zero aggregate BEFORE failing, and \
           \no later family's" $ do
            r ← runProfile (Scenario ["data/recipes"] []) "normal"
            let upTo = takeWhile ((≢ "recipe") ∘ famId) normalFams
            aggregatesOf r `shouldBe`
                [ aggregateLine f (perFileCount * length famFiles)
                                  (length famFiles)
                | f ← upTo ]
                ⧺ [ "Startup assets: recipe loaded 0 from 0 file(s)" ]
            -- and nothing after it ran at all
            rCalls r `shouldBe` perFileCount' (length upTo)

        it "fails on a file that did not parse, naming the family and \
           \the FILE — after the family's aggregate carries its healthy \
           \count and its original discovered-file count" $ do
            r ← runProfile (Scenario [] ["data/recipes/b.yaml"]) "normal"
            (rDone r, rFailed r) `shouldBe` (False, True)
            (rFamily r, rKind r, rWhere r)
                `shouldBe` ("recipe", "parse", "data/recipes/b.yaml")
            (rFailed' r, rFiles r) `shouldBe` ("1", "2")
            rMessage r `shouldBe`
                "Startup failed: recipe could not parse \
                \data/recipes/b.yaml (1 of 2 file(s) failed)"
            -- The aggregate is #1930's, unchanged: BOTH files ran, so
            -- it reports both counts and both files -- and it is the
            -- LAST one, with not one queued item after it having run.
            aggregatesOf r `shouldBe`
                [ aggregateLine f (perFileCount * length famFiles)
                                  (length famFiles)
                | f ← takeWhile ((≢ "recipe") ∘ famId) normalFams ]
                ⧺ [ "Startup assets: recipe loaded 6 from 2 file(s)" ]
            rCalls r `shouldBe` perFileCount' 6
            rErrors r `shouldBe` 1

        it "counts EVERY failing file in the family and reports the \
           \first in queue order" $ do
            r ← runProfile (Scenario []
                    ["data/recipes/a.yaml", "data/recipes/b.yaml"]) "normal"
            (rWhere r, rFailed' r, rFiles r)
                `shouldBe` ("data/recipes/a.yaml", "2", "2")
            paths ← inVM (loaderPrelude
                              (Scenario [] [ "data/recipes/a.yaml"
                                           , "data/recipes/b.yaml" ]))
                        "SL.build('normal') drain() \
                        \return table.concat(SL.getFailure().paths, ',')"
            paths `shouldBe` "data/recipes/a.yaml,data/recipes/b.yaml"

        it "keeps today's behaviour for a family whose files all parse \
           \and all return zero (requirement 4)" $ do
            -- Zero is not a failure: only the PARSE flag fails a
            -- family, so this boot still drains and still reports the
            -- honest zero aggregate over the two files it loaded.
            runZeroFamily `shouldReturn`
                ("true", "false", "0",
                 "Startup assets: recipe loaded 0 from 2 file(s)")

        it "treats a binding that reports NO parse outcome as a failure \
           \rather than assuming success" $ do
            out ← inVM (T.unlines (enginePrelude healthy ⧺
                    [ "SL = require('scripts.startup_loader')"
                    -- the pre-#2203 shape: one number, no outcome
                    , "engine.loadRecipeYaml = function(p) return 6.0 end"
                    ]))
                    "SL.build('normal') \
                    \local g = 0 \
                    \while not SL.isDone() and not SL.isFailed() do \
                    \  SL.tick(0) g = g + 1 assert(g < 100000) end \
                    \return SL.getFailure().family"
            out `shouldBe` "recipe"

    ------------------------------------------------------------------
    describe "the failed state is terminal (requirement 3)" $ do

        it "keeps isDone false and never advances progress or re-logs, \
           \however many more ticks arrive" $ do
            let sc = Scenario [] ["data/recipes/b.yaml"]
            -- 200 further ticks past the failure change nothing: not
            -- progress, not the loader calls made, not the one error.
            after ← inVM (loaderPrelude sc)
                "SL.build('normal') \
                \local g = 0 \
                \while not SL.isDone() and not SL.isFailed() do \
                \  SL.tick(0) g = g + 1 assert(g < 100000) end \
                \local p0 = SL.getProgress() \
                \local c0, e0 = #calls, #errors \
                \for _ = 1, 200 do SL.tick(0) end \
                \return string.format('%s/%s/%s/%s/%s', \
                \  tostring(SL.isDone()), tostring(SL.getProgress() == p0), \
                \  tostring(#calls == c0), tostring(#errors == e0), \
                \  tostring(SL.isFailed()))"
            after `shouldBe` "false/true/true/true/true"

        it "lets runAll RETURN on a failure instead of spinning on done \
           \(the arena profile's only exit)" $ do
            out ← inVM (loaderPrelude (Scenario [] ["data/buildings/a.yaml"]))
                "SL.build('arena') SL.runAll() \
                \return string.format('%s/%s/%s', tostring(SL.isDone()), \
                \  tostring(SL.isFailed()), SL.getFailure().family)"
            out `shouldBe` "false/true/building"

        it "clears the failure on build and on reset" $ do
            out ← inVM (loaderPrelude (Scenario [] ["data/recipes/b.yaml"]))
                "SL.build('normal') drain() \
                \local afterFail = SL.isFailed() \
                \SL.reset() \
                \local afterReset = SL.isFailed() \
                \SL.build('normal') \
                \local afterBuild = SL.isFailed() \
                \return string.format('%s/%s/%s', tostring(afterFail), \
                \  tostring(afterReset), tostring(afterBuild))"
            out `shouldBe` "true/false/false"

        it "fails the arena profile on ITS own family inventory" $ do
            r ← runProfile (Scenario ["data/units"] []) "arena"
            (rDone r, rFailed r, rFamily r) `shouldBe` (False, True, "unit")
            -- arena has no flora family, so its aggregates stop one
            -- family earlier than normal's would
            length (aggregatesOf r) `shouldBe` 9

    ------------------------------------------------------------------
    describe "the boot surfaces it and refuses to finish (requirement 3)"
        $ do

        it "normal boot: a clean queue still completes and opens the \
           \main menu" $ do
            b ← runBoot healthy "normal"
            bPhase b    `shouldBe` "done"
            bMenus b    `shouldBe` ["main"]
            bBootDone b `shouldBe` True
            bComplete b `shouldBe` 1
            bStatus b   `shouldBe` "Complete!"
            bBar b      `shouldBe` "1.000000"
            bShown b    `shouldBe` "shown"

        it "normal boot: a parse failure stops at 'failed', logs no \
           \completion, runs no finishStartupBoot and shows no menu" $ do
            b ← runBoot (Scenario [] ["data/recipes/b.yaml"]) "normal"
            bPhase b    `shouldBe` "failed"
            bMenus b    `shouldBe` []
            bBootDone b `shouldBe` False
            bComplete b `shouldBe` 0
            (bLoadDone b, bFailed b) `shouldBe` (False, True)
            bMessage b  `shouldBe`
                "Startup failed: recipe could not parse \
                \data/recipes/b.yaml (1 of 2 file(s) failed)"
            bErrors b   `shouldBe` 1
            -- The screen carries the message in place of "Complete!",
            -- and the bar is left short of full.
            bStatus b   `shouldBe` bMessage b
            bBar b      `shouldSatisfy` (< "1.000000")
            bShown b    `shouldBe` "shown"

        it "normal boot: an empty family directory does the same" $ do
            b ← runBoot (Scenario ["data/units"] []) "normal"
            (bPhase b, bMenus b, bBootDone b) `shouldBe` ("failed", [], False)
            bMessage b `shouldBe`
                "Startup failed: unit discovered no YAML files in data/units"
            bStatus b  `shouldBe` bMessage b

        it "arena boot: a clean queue still finishes and opens the arena" $ do
            b ← runBoot healthy "arena"
            bMenus b    `shouldBe` ["test_arena"]
            bBootDone b `shouldBe` True

        it "arena boot: a failure in the SYNCHRONOUS drain shows the \
           \retained message and never runs finishArenaBoot" $ do
            b ← runBoot (Scenario [] ["data/buildings/a.yaml"]) "arena"
            bPhase b    `shouldBe` "failed"
            bMenus b    `shouldBe` []
            bBootDone b `shouldBe` False
            bComplete b `shouldBe` 0
            bMessage b  `shouldBe`
                "Startup failed: building could not parse \
                \data/buildings/a.yaml (1 of 2 file(s) failed)"
            -- Arena never shows the screen at all until it fails, so
            -- this is also the proof that runArenaStartup showed it.
            bStatus b   `shouldBe` bMessage b
            bBar b      `shouldSatisfy` (< "1.000000")
            bShown b    `shouldBe` "shown"

        -- Round-1 review: a resize (and, through the same one entry
        -- point, a UI-scale reflow) DESTROYS and rebuilds this page.
        -- Only "loading" counted as visible, and createUI's fresh
        -- widgets carry statusText and an empty bar -- so a failed
        -- startup came back as a hidden page that, if shown, would have
        -- read like a boot still in progress.
        forM_ [ ("normal", Scenario [] ["data/recipes/b.yaml"])
              , ("arena",  Scenario [] ["data/buildings/a.yaml"]) ]
            $ \(profile, sc) →
            it (T.unpack profile ⧺ " boot: a resize after the failure \
                \keeps the message, the frozen bar and the page on \
                \screen") $ do
                before ← runBoot sc profile
                b ← runBootThen "resize(1024, 768)" sc profile
                bShown b  `shouldBe` "shown"
                bStatus b `shouldBe` bMessage b
                bBar b    `shouldBe` bBar before
                -- and it is still terminal on the other side of the
                -- rebuild
                bPhase b    `shouldBe` "failed"
                bMenus b    `shouldBe` []
                bBootDone b `shouldBe` False
                bComplete b `shouldBe` 0

    ------------------------------------------------------------------
    describe "every binding's result tells a broken file from an empty \
             \one (requirement 1)" $ aroundAll withBindings $ do

        it "runs inside the scratch resource root, never the checkout \
           \(#1357)" $ \_ →
            isInsideIsolatedResourceRoot `shouldReturn` True

        forM_ normalFams $ \f →
            it (T.unpack (famVerb f) ⧺ " answers one number by default \
                \and the parse outcome only when asked") $ \b → do
                let verb = famVerb f
                    path = shippedFile verb
                forM_ (shippedPrereqs verb) $ \(v, dep) →
                    void (luaEval b ("return engine." <> v
                                     <> "('" <> dep <> "')"))
                -- The legacy shape is load-bearing: executeDebugLua
                -- tab-joins every returned value, so an unconditional
                -- second result would rewrite what a bare
                -- `return engine.load*Yaml(p)` reads back.
                arityOf b verb (quoted path) `shouldReturn` "1"
                arityOf b verb (quoted path <> ", true") `shouldReturn` "2"
                (n, parsed) ← outcomeOf b verb path
                n `shouldSatisfy` (> 0)
                parsed `shouldBe` True

        forM_ normalFams $ \f →
            it (T.unpack (famVerb f)
                ⧺ " reports a malformed file as NOT parsed, at zero") $ \b →
                withFixtureDir ("malformed-" ⧺ T.unpack (famId f))
                    [ ("broken.yaml", malformedFile) ] $ \root → do
                    let path = T.pack (root </> "broken.yaml")
                    outcomeOf b (famVerb f) path `shouldReturn` (0, False)
                    -- and still just one value when not asked
                    arityOf b (famVerb f) (quoted path) `shouldReturn` "1"

        forM_ [ (f, contents)
              | f ← normalFams
              , Just contents ← [emptyListFile (famVerb f)] ] $ \(f, contents) →
            it (T.unpack (famVerb f)
                ⧺ " reports a valid but EMPTY file as parsed, at the \
                  \same zero") $ \b →
                withFixtureDir ("empty-" ⧺ T.unpack (famId f))
                    [ ("empty.yaml", contents) ] $ \root → do
                    let path = T.pack (root </> "empty.yaml")
                    outcomeOf b (famVerb f) path `shouldReturn` (0, True)

        it "a path-less call reports the same not-parsed zero" $ \b → do
            outcome ← luaEval b
                "return string.format('%d/%s', engine.loadMaterialYaml(nil, \
                \true))"
            outcome `shouldBe` "0/false"

-- | Loader calls made by the families BEFORE the one that failed.
perFileCount' ∷ Int → Int
perFileCount' familiesBefore = familiesBefore * length famFiles

-- | The all-zero-but-parsed family, driven on its own so the zero
--   aggregate and the continued boot are read from one run.
runZeroFamily ∷ IO (Text, Text, Text, Text)
runZeroFamily = do
    out ← inVM (T.unlines (enginePrelude healthy ⧺
            [ "SL = require('scripts.startup_loader')"
            , "engine.loadRecipeYaml = function(p, w)"
            , "  if w then return 0.0, true end return 0.0 end"
            ]))
            "SL.build('normal') \
            \local g = 0 \
            \while not SL.isDone() and not SL.isFailed() do \
            \  SL.tick(0) g = g + 1 assert(g < 100000) end \
            \local agg = '-' \
            \for _, m in ipairs(infos) do \
            \  if m:find('Startup assets: recipe ', 1, true) == 1 then \
            \    agg = m end \
            \end \
            \return table.concat({tostring(SL.isDone()), \
            \  tostring(SL.isFailed()), tostring(#errors), agg}, '\\1')"
    pure $ case T.splitOn "\1" out of
        [a, b, c, d] → (a, b, c, d)
        _            → (out, "", "", "")
