{-# LANGUAGE TypeApplications #-}
-- | Player-facing Create World controls (#706). These source-level
--   contracts keep the Lua UI's declarations explicit without booting a
--   graphical engine, while the generation assertions protect the hidden
--   default payload that must remain backward compatible.
--
--   The world-name handoff (#1105) is covered BEHAVIOURALLY rather than
--   textually: a standalone Lua VM (the same pattern as
--   "Test.Headless.Lua.SaveModules") requires the real
--   @scripts/world_view.lua@ and @scripts/world_manager.lua@ against
--   stub globals and records what @world.init@ actually receives. The
--   player's name was captured by the Create World screen and then
--   dropped twice on its way to the engine, so a grep-shaped assertion
--   would not have caught it — only running the chain does.
module Test.Headless.UI.CreateWorldControls (spec) where

import UPrelude
import Test.Hspec
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Directory (doesFileExist)

menuPath, settingsPath, generalPath, generationPath, climatePath ∷ FilePath
menuPath       = "scripts/create_world_menu.lua"
settingsPath   = "scripts/create_world/settings_tab.lua"
generalPath    = "scripts/create_world/general_tab.lua"
generationPath = "scripts/create_world/generation.lua"
climatePath    = "scripts/create_world/climate_tab.lua"

quotedAfter ∷ Text → Text → Maybe Text
quotedAfter marker line =
    let (_, suffix) = T.breakOn marker line
        afterMarker = T.drop (T.length marker) suffix
        afterQuote  = T.dropWhile (/= '"') afterMarker
    in if T.null suffix ∨ T.null afterQuote
       then Nothing
       else Just $ T.takeWhile (/= '"') $ T.drop 1 afterQuote

blockAfter ∷ Text → Text → Text → Text
blockAfter start end source =
    let (_, suffix) = T.breakOn start source
        body        = T.drop (T.length start) suffix
    in fst $ T.breakOn end body

tabEntries ∷ Text → [(Text, Text)]
tabEntries source = mapMaybe entry $ T.lines block
  where
    block = blockAfter "local tabDefs = {" "\n}" source
    entry line = do
        key  ← quotedAfter "key" line
        name ← quotedAfter "name" line
        pure (key, name)

settingsLabels ∷ Text → [Text]
settingsLabels = mapMaybe (quotedAfter "text     =") ∘ T.lines

generalLabels ∷ Text → [Text]
generalLabels = mapMaybe (quotedAfter "addRow(") ∘ T.lines

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | Run one self-contained Lua chunk in a fresh interpreter, with
--   'handoffPrelude' loaded first. The chunk signals failure through
--   Lua's own @assert()@/@error()@; a non-OK 'Lua.Status' becomes an
--   hspec failure carrying the Lua message.
--
--   @cabal test@ runs with its CWD at the repo root, so
--   @require('scripts.*')@ resolves through Lua's default
--   @package.path@ with no extra setup — the same assumption
--   "Test.Headless.Lua.SaveModules" already relies on.
runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        preludeStatus ← Lua.dostring (TE.encodeUtf8 handoffPrelude)
        case preludeStatus of
            Lua.OK → do
                status ← Lua.dostring (TE.encodeUtf8 chunkText)
                case status of
                    Lua.OK → return Nothing
                    _      → Just ⊚ luaError
            _ → Just ∘ ("prelude: " <>) ⊚ luaError
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)
  where
    luaError = do
        err ← Lua.tostring (-1)
        return (maybe "<no message>" TE.decodeUtf8Lenient err)

-- | Stubs for every global the two real modules reach outside an engine
--   boot, plus a recorder capturing @world.init@'s COMPLETE argument
--   list. 'table.pack' is what makes the distinction this issue turns
--   on observable: its @n@ field counts a trailing nil, so "argument 5
--   was passed as nil" and "only four arguments were passed" stay
--   distinguishable — and both must behave identically to the engine.
handoffPrelude ∷ Text
handoffPrelude = lns
    [ "initCalls = {}"
    , "world = {"
    , "  init = function(...) initCalls[#initCalls + 1] = table.pack(...) end,"
    , "  setTexture = function() end,"
    , "  show = function() end,"
    , "}"
    , "engine = {"
    , "  logInfo = function() end, logWarn = function() end,"
    , "  loadTexture = function() return 1 end,"
    -- -1 means "no handle", so the material/vegetation texture sweeps
    -- stay no-ops instead of needing a full texture registry here.
    , "  getTextureHandle = function() return -1 end,"
    , "}"
    -- world_manager resets tutorial progress through require(); there is
    -- no engine here to back that module's own dependencies.
    , "package.loaded['scripts.tutorial_progress'] ="
    , "  { reset = function() end }"
    , "worldView = require('scripts.world_view')"
    , "worldManager = require('scripts.world_manager')"
    -- Drive the REAL path the Create World screen takes: generation.lua
    -- publishes worldView.worldParams, then worldView.createWorld()
    -- funnels through worldManager.createWorld into world.init.
    , "function generate(params)"
    , "  worldView.worldParams = params"
    , "  worldManager.active = false"
    , "  worldView.createWorld()"
    , "  return assert(initCalls[#initCalls], 'world.init was never called')"
    , "end"
    ]

-- | The parameter table @scripts/create_world/generation.lua@ publishes,
--   with the world name under test substituted in as raw Lua source.
genParams ∷ Text → Text
genParams nameExpr =
    "{ seed = 7, worldSize = 64, plateCount = 3, worldName = "
    <> nameExpr <> " }"

spec ∷ Spec
spec = do
    menuSource       ← runIO $ TIO.readFile menuPath
    settingsSource   ← runIO $ TIO.readFile settingsPath
    generalSource    ← runIO $ TIO.readFile generalPath
    generationSource ← runIO $ TIO.readFile generationPath

    it "exposes exactly General, Geology, and Timeline" $
        tabEntries menuSource `shouldBe`
            [ ("settings", "General")
            , ("advanced", "Geology")
            , ("timeline", "Timeline")
            ]

    it "exposes exactly the five active General controls" $
        settingsLabels settingsSource ++ generalLabels generalSource
            `shouldBe` ["Name", "Seed", "Size", "Days / Month", "Months / Year"]

    it "removes the obsolete Climate UI module and all orchestration wiring" $ do
        doesFileExist climatePath `shouldReturn` False
        T.isInfixOf "climateTab" menuSource `shouldBe` False
        T.isInfixOf "climateTab" generationSource `shouldBe` False

    -- #1105: the name the player types was captured into
    -- menu.pending.worldName and copied onto worldView.worldParams, then
    -- silently dropped by world_view's createWorld payload and again by
    -- world_manager's four-argument world.init call. These run the real
    -- chain and assert on what the engine would actually have received.
    it "carries the player's world name into world.init's fifth argument" $
        runsOk $ lns
            [ "local call = generate(" <> genParams "'Vaelthorn Reach'" <> ")"
            , "assert(call[1] == 'main_world', 'page id: '"
            , "       .. tostring(call[1]))"
            , "assert(call[2] == 7,  'seed: '   .. tostring(call[2]))"
            , "assert(call[3] == 64, 'size: '   .. tostring(call[3]))"
            , "assert(call[4] == 3,  'plates: ' .. tostring(call[4]))"
            , "assert(call.n >= 5, 'world.init still takes only '"
            , "       .. tostring(call.n) .. ' arguments')"
            , "assert(call[5] == 'Vaelthorn Reach',"
            , "       'display name: ' .. tostring(call[5]))"
            ]

    -- Requirement 5 / the reviewer's normalization-ownership note:
    -- mkWorldIdentity strips and rejects blank names, so the Lua side
    -- must forward the raw value and let the engine decide. A Lua-side
    -- trim would be a second, drifting opinion about what is empty.
    it "forwards names verbatim, leaving trimming to the engine" $
        runsOk $ lns
            [ "local spaced = generate(" <> genParams "'  Vaelthorn  '" <> ")"
            , "assert(spaced[5] == '  Vaelthorn  ',"
            , "       'trimmed in Lua: ' .. string.format('%q', spaced[5]))"
            , "local blank = generate(" <> genParams "'   '" <> ")"
            , "assert(blank[5] == '   ',"
            , "       'whitespace-only rewritten in Lua: '"
            , "       .. string.format('%q', blank[5]))"
            ]

    -- Requirement 2: a name the player explicitly cleared stays cleared.
    -- Nothing on this path may substitute a generated or default name.
    it "keeps an emptied world name empty rather than synthesizing one" $
        runsOk $ lns
            [ "local call = generate(" <> genParams "''" <> ")"
            , "assert(call[5] == '',"
            , "       'empty name replaced with: '"
            , "       .. string.format('%q', tostring(call[5])))"
            ]

    -- Requirement 3 / epic #708 criterion 7: with no name captured at
    -- all, world.init must be indistinguishable from its historical
    -- four-argument form — argument 5 nil, and no gloss invented for
    -- argument 6. Arena, debug, and console callers ride on this.
    it "leaves the identity arguments unset when no name was captured" $
        runsOk $ lns
            [ "local call = generate(" <> genParams "nil" <> ")"
            , "assert(call[1] == 'main_world', 'page id changed')"
            , "assert(call[5] == nil,"
            , "       'invented a name: ' .. tostring(call[5]))"
            , "assert(call[6] == nil,"
            , "       'invented a gloss: ' .. tostring(call[6]))"
            ]

    -- The first link of the chain, which already worked and must keep
    -- working: generation.lua publishing the captured name onto the
    -- params table world_view reads.
    it "publishes the captured name onto worldView.worldParams" $
        generationSource `shouldSatisfy` T.isInfixOf "worldName  = p.worldName"

    it "continues submitting hidden clock, astronomy, and climate defaults" $
        forM_ [ "hours_per_day"
              , "minutes_per_hour"
              , "tilt_angle"
              , "day_length"
              , "cycle_days"
              , "phase_offset"
              , "iterations"
              , "coriolis_scale"
              , "wind_drag"
              , "thermal_inertia"
              , "orographic_scale"
              , "evap_scale"
              , "albedo_feedback"
              , "thc_threshold"
              ] $ \field → generationSource `shouldSatisfy` T.isInfixOf field
