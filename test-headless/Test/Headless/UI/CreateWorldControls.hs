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
import Data.IORef (newIORef)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Directory (doesFileExist)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Font.Repertoire (generatedNameFonts)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

menuPath, settingsPath, generalPath, generationPath, climatePath ∷ FilePath
menuPath       = "scripts/create_world_menu.lua"
settingsPath   = "scripts/create_world/settings_tab.lua"
generalPath    = "scripts/create_world/general_tab.lua"
generationPath = "scripts/create_world/generation.lua"
climatePath    = "scripts/create_world/climate_tab.lua"

randboxPath, bootPath ∷ FilePath
randboxPath = "scripts/ui/randbox.lua"
bootPath    = "scripts/ui_manager_boot.lua"

quotedAfter ∷ Text → Text → Maybe Text
quotedAfter marker line =
    let (_, suffix) = T.breakOn marker line
        afterMarker = T.drop (T.length marker) suffix
        afterQuote  = T.dropWhile (/= '"') afterMarker
    in if T.null suffix ∨ T.null afterQuote
       then Nothing
       else Just $ T.takeWhile (/= '"') $ T.drop 1 afterQuote

-- | 'quotedAfter' restricted to a marker whose value is a string
--   LITERAL — the first thing after it, ignoring spaces, must be the
--   opening quote.
--
--   Without that restriction any @field = someExpression .. "x"@ line
--   would be read as declaring @x@, so a field assigned a computed
--   value (#1106's world-name gloss label, whose text comes from the
--   current suggestion) would silently enter a list of authored labels.
--   Requiring the literal is what keeps these source-level assertions
--   from depending on how a line happens to be written.
literalAfter ∷ Text → Text → Maybe Text
literalAfter marker line = do
    let (_, suffix) = T.breakOn marker line
    if T.null suffix then Nothing else do
        let value = T.stripStart (T.drop (T.length marker) suffix)
        rest ← T.stripPrefix "\"" value
        pure (T.takeWhile (/= '"') rest)

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
settingsLabels = mapMaybe (literalAfter "text     =") ∘ T.lines

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
runsOk = runsWith handoffPrelude

-- | 'runsOk' with a different prelude, for the suggestion-state chunks
--   that need no world_view/world_manager wiring at all.
runsSuggest ∷ Text → Expectation
runsSuggest = runsWith suggestPrelude

-- | 'runsOk' against the REAL @scripts/ui/randbox.lua@ over a synthetic
--   text-buffer backend, for the keystroke handlers themselves.
runsRandbox ∷ Text → Expectation
runsRandbox = runsWith randboxPrelude

runsWith ∷ Text → Text → Expectation
runsWith prelude chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        preludeStatus ← Lua.dostring (TE.encodeUtf8 prelude)
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

-- | Run a Create World lifecycle against the real headless UI API. This
--   deliberately uses the same bare-backend shape as ResponsiveMenus, but
--   keeps the fixture here because the regression belongs to this screen's
--   player-facing identity contract rather than to responsive geometry.
--
--   The isolated resource root wraps engine creation because production
--   startup may materialize local config overlays; no focused UI test may
--   write into the checkout's real @config/@ directory (#1357).
runsCreateWorldLifecycle ∷ Text → Expectation
runsCreateWorldLifecycle chunkText =
    withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                   (assetPoolRef env) (nextObjectIdRef env)
                                   (inputStateRef env) (loggerRef env)
        stateRef ← newIORef ThreadRunning
        registerLuaAPI (lbsLuaState ls) env ls stateRef
        result ← executeDebugLua (lbsLuaState ls) chunkText
        when ("error:" `T.isPrefixOf` result
              ∨ "syntax error:" `T.isPrefixOf` result) $
            expectationFailure ("Lua error: " ⧺ T.unpack result)
        result `shouldBe` "true"

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

-- | The same table plus the identity a live generated-language
--   SUGGESTION carries (#1106): its English gloss and the #1092
--   provenance of the language that rendered it.
genSuggestedParams ∷ Text
genSuggestedParams = lns
    [ "{ seed = 7, worldSize = 64, plateCount = 3,"
    , "  worldName = 'Karadun', worldGloss = 'Ashen Land',"
    , "  languageSeed = '18446744073709551615', languageVersion = 5 }"
    ]

-- | Stubs for @scripts/create_world/name_suggest.lua@'s only outside
--   dependency, plus a fresh `pending` table.
--
--   @world.suggestName@ is replaced by a deterministic fake whose whole
--   job is to be DISTINGUISHABLE per (seed, ordinal) — the real
--   generator's own determinism and non-repetition are pinned by
--   "Test.Headless.Language.Suggest". What is under test here is which
--   of its results the screen keeps, and when it throws them away.
suggestPrelude ∷ Text
suggestPrelude = lns
    [ "suggestCalls = {}"
    , "suggestFails = false"
    , "world = {"
    , "  suggestName = function(seed, ordinal)"
    , "    suggestCalls[#suggestCalls + 1] = { seed = seed, ordinal = ordinal }"
    , "    if suggestFails then return nil, 'no catalogue' end"
    , "    return {"
    , "      name = 'N' .. tostring(seed) .. '_' .. tostring(ordinal),"
    , "      gloss = 'G' .. tostring(seed) .. '_' .. tostring(ordinal),"
    , "      expr = 'Modifier:M' .. tostring(seed) .. ':H' .. tostring(ordinal),"
    , "      language = { seed = tostring(seed) .. '000', version = 5 },"
    , "    }"
    , "  end,"
    , "}"
    -- The real repertoire's shape in miniature: ASCII letters, an
    -- extended letter, and both marks a rendered name can carry.
    , "world.generatedNameCharacters = function()"
    , "  return \"abcxyzABCXYZ\\u{00E9}\\u{0163}-'\""
    , "end"
    , "engine = { logWarn = function() end, logInfo = function() end,"
    , "           logDebug = function() end }"
    , "nameSuggest = require('scripts.create_world.name_suggest')"
    , "function newPending() return { worldName = '', seed = '' } end"
    ]

-- | A synthetic text-buffer backend for the REAL randbox module.
--
--   Every UI verb randbox reaches for is a no-op by default (the
--   metatable), with only the text-input model implemented for real:
--   handles, buffers, cursors, and focus. That is exactly the state the
--   keystroke handlers manipulate, so a chunk can press Backspace at
--   offset 0 and observe what randbox reported — which is the whole
--   question here, and one the state module cannot answer because it
--   never sees the buffer.
randboxPrelude ∷ Text
randboxPrelude = lns
    [ "local buffers, cursors, focused, nextHandle = {}, {}, nil, 1"
    , "local function newHandle()"
    , "  local h = nextHandle; nextHandle = h + 1"
    , "  buffers[h] = ''; cursors[h] = 0; return h"
    , "end"
    , "UI = setmetatable({}, { __index = function() return function() end end })"
    , "UI.newBox = function() return newHandle() end"
    , "UI.newText = function() return newHandle() end"
    , "UI.newSprite = function() return newHandle() end"
    -- Cursor offsets are CODEPOINTS, exactly as UI.TextBuffer defines
    -- them — a byte-indexed stand-in would split an extended letter and
    -- report a corruption the real engine cannot produce.
    , "local function at(t, cp) return utf8.offset(t, cp + 1) end"
    , "UI.setTextInput = function(h, t) buffers[h] = t or ''"
    , "  cursors[h] = utf8.len(buffers[h]) end"
    , "UI.getTextInput = function(h) return buffers[h] or '' end"
    , "UI.setCursor = function(h, p) cursors[h] = p end"
    , "UI.getCursor = function(h) return cursors[h] or 0 end"
    , "UI.setFocus = function(h) focused = h end"
    , "UI.clearFocus = function() focused = nil end"
    , "UI.hasFocus = function(h) return focused == h end"
    , "UI.insertChar = function(h, c)"
    , "  local t, p = buffers[h] or '', cursors[h] or 0"
    , "  local b = at(t, p)"
    , "  buffers[h] = t:sub(1, b - 1) .. c .. t:sub(b); cursors[h] = p + 1"
    , "end"
    , "UI.deleteBackward = function(h)"
    , "  local t, p = buffers[h] or '', cursors[h] or 0"
    , "  if p > 0 then"
    , "    buffers[h] = t:sub(1, at(t, p - 1) - 1) .. t:sub(at(t, p))"
    , "    cursors[h] = p - 1"
    , "  end"
    , "end"
    , "UI.deleteForward = function(h)"
    , "  local t, p = buffers[h] or '', cursors[h] or 0"
    , "  if p < utf8.len(t) then"
    , "    buffers[h] = t:sub(1, at(t, p) - 1) .. t:sub(at(t, p + 1))"
    , "  end"
    , "end"
    , "engine = {"
    , "  loadTexture = function() return 1 end,"
    , "  getTextWidth = function() return 0 end,"
    , "  getUIScale = function() return 1 end,"
    , "  logDebug = function() end, logInfo = function() end,"
    , "  logWarn = function() end,"
    , "}"
    , "package.loaded['scripts.ui.box_textures'] ="
    , "  { load = function() return {} end }"
    , "randbox = require('scripts.ui.randbox')"
    , "randbox.init()"
    -- One NAME randbox holding a suggestion, focused for editing, with
    -- every user-edit report recorded.
    , "edits = {}"
    , "function newNameBox(text)"
    , "  local id = randbox.new{"
    , "    name = 'world_name', page = 1, font = 1,"
    , "    randType = randbox.Type.NAME, default = text,"
    , "    autoGenerate = false,"
    , "    onUserEdit = function(v) edits[#edits + 1] = v end,"
    , "  }"
    , "  randbox.focus(id)"
    , "  return id"
    , "end"
    ]

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

    -- #1978: production first enters this screen by ensuring it (init),
    -- then immediately showing it (a second createUI). The two controls
    -- could display the generated identity while the persistent pending
    -- table had already been restored to its pre-construction blanks, so
    -- untouched Generate silently created an unnamed seed-0 world.
    it "keeps the first-entry visible name, seed, and suggestion identity through untouched Generate" $
        runsCreateWorldLifecycle $ lns
            [ "require('scripts.ui.randbox').init()"
            , "require('scripts.ui.textbox').init()"
            , "require('scripts.ui.dropdown').init()"
            , "local randbox = require('scripts.ui.randbox')"
            -- Make the screen's fresh identity deterministic while
            -- retaining the real settings/menu/generation modules.
            , "randbox.newHexSeed = function() return 'B' end"
            , "world.suggestName = function(seed, ordinal)"
            , "  return { name = 'N' .. tostring(seed) .. '_' .. tostring(ordinal),"
            , "    gloss = 'G' .. tostring(seed) .. '_' .. tostring(ordinal),"
            , "    expr = 'Modifier:M' .. tostring(seed) .. ':H' .. tostring(ordinal),"
            , "    language = { seed = tostring(seed) .. '000', version = 5 } }"
            , "end"
            , "world.generatedNameCharacters = function() return 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-' end"
            , "world.setGenConfig = function() end"
            , "debug.recordOutcome = function() end"
            , "local worldView = { startGeneration = function() end }"
            , "package.loaded['scripts.world_view'] = worldView"
            , "package.loaded['scripts.world_manager'] = {"
            , "  isActive = function() return false end,"
            , "  destroyWorld = function() end,"
            , "}"
            , "local menu = require('scripts.create_world_menu')"
            -- Mirrors ui_manager_menu's ensureCreateWorldMenu()+show().
            , "menu.init(1, 2, 3, 1280, 720)"
            , "menu.show()"
            , "local settings = require('scripts.create_world.settings_tab')"
            , "local shownName = randbox.getValue(settings.nameRandBoxId)"
            , "local shownSeed = randbox.getValue(settings.seedRandBoxId)"
            , "assert(shownName == 'N11_0', 'visible name: ' .. tostring(shownName))"
            , "assert(shownSeed == 'B', 'visible seed: ' .. tostring(shownSeed))"
            , "assert(menu.pending.worldName == shownName,"
            , "  'pending name: ' .. tostring(menu.pending.worldName))"
            , "assert(menu.pending.seed == shownSeed,"
            , "  'pending seed: ' .. tostring(menu.pending.seed))"
            , "local identity = {"
            , "  gloss = menu.pending.nameGloss, expr = menu.pending.nameExpr,"
            , "  languageSeed = menu.pending.nameLanguageSeed,"
            , "  languageVersion = menu.pending.nameLanguageVersion,"
            , "  ordinal = menu.pending.nameOrdinal,"
            , "}"
            , "assert(identity.gloss == 'G11_0', tostring(identity.gloss))"
            , "assert(identity.expr == 'Modifier:M11:H0', tostring(identity.expr))"
            , "assert(identity.languageSeed == '11000', tostring(identity.languageSeed))"
            , "assert(identity.languageVersion == 5, tostring(identity.languageVersion))"
            , "assert(identity.ordinal == 1, tostring(identity.ordinal))"
            , "menu.onGenerateWorld()"
            , "local generated = worldView.worldParams"
            , "assert(generated.seed == tonumber(shownSeed, 16), tostring(generated.seed))"
            , "assert(generated.worldName == shownName, tostring(generated.worldName))"
            , "assert(generated.worldGloss == identity.gloss, tostring(generated.worldGloss))"
            , "assert(generated.nameExpr == identity.expr, tostring(generated.nameExpr))"
            , "assert(generated.languageSeed == identity.languageSeed, tostring(generated.languageSeed))"
            , "assert(generated.languageVersion == identity.languageVersion, tostring(generated.languageVersion))"
            , "assert(menu.pending.nameOrdinal == identity.ordinal, tostring(menu.pending.nameOrdinal))"
            , "return true"
            ]

    -- #1106 requirement 9: the dummy generator is gone, not merely
    -- bypassed. Its word lists had no language, no meaning, and no
    -- gloss, and it drew from Lua's global RNG (#708 principle 9).
    describe "the dummy world-name generator (#1106)" $ do
        randboxSource ← runIO $ TIO.readFile randboxPath

        it "no longer exists" $
            forM_ ["randomName", "local prefixes", "local middles"
                  , "local suffixes"] $ \dead →
                (dead, T.isInfixOf dead randboxSource) `shouldBe` (dead, False)

        it "leaves the NAME control classified for validation" $
            randboxSource `shouldSatisfy` T.isInfixOf "NAME = \"name\""

        -- The seed field still rolls hex from math.random, which is
        -- fine and unrelated; what must never come back is a NAME value
        -- produced anywhere but the injected generator. Checked against
        -- generateRandom's own extracted body, not the whole file —
        -- randbox.Type.NAME legitimately appears in the validation and
        -- length-limit branches, which are not generators.
        it "gives randbox.Type.NAME no built-in generator at all" $ do
            let body = blockAfter "local function generateRandom(rb)"
                                  "\nend" randboxSource
            body `shouldSatisfy` (not ∘ T.null)
            body `shouldSatisfy` T.isInfixOf "rb.generate"
            body `shouldSatisfy` (not ∘ T.isInfixOf "NAME")

    -- Requirement 10: generated names may carry extended-Latin letters
    -- (#1100), so the font Create World actually renders text in has to
    -- be one of the fonts that inventory was proven against.
    it "renders menus in a font cleared for generated names" $ do
        bootSource ← TIO.readFile bootPath
        let menuFontLines =
                filter (T.isInfixOf "menuFontHandle = engine.loadFont")
                       (T.lines bootSource)
            namesAFontFor l =
                any (\f → T.isInfixOf (T.pack f) l) generatedNameFonts
        menuFontLines `shouldSatisfy` (not ∘ null)
        forM_ menuFontLines $ \l → l `shouldSatisfy` namesAFontFor

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

    -- #1106 requirement 5: a name that is still a live SUGGESTION
    -- carries its gloss and the language that rendered it into the
    -- identity path, so world.getIdentity/getLanguageProvenance can
    -- answer for it later and the world's locations can be named in the
    -- same language.
    it "carries a suggestion's gloss and provenance into world.init" $
        runsOk $ lns
            [ "local call = generate(" <> genSuggestedParams <> ")"
            , "assert(call[5] == 'Karadun', 'name: ' .. tostring(call[5]))"
            , "assert(call[6] == 'Ashen Land', 'gloss: ' .. tostring(call[6]))"
            -- A decimal STRING, not a number: the top of the Word64
            -- range survives neither Lua's signed integer nor its double.
            , "assert(call[7] == '18446744073709551615',"
            , "       'language seed: ' .. tostring(call[7]))"
            , "assert(call[8] == 5, 'generator version: ' .. tostring(call[8]))"
            ]

    -- The first link of the chain, which already worked and must keep
    -- working: generation.lua publishing the captured name onto the
    -- params table world_view reads.
    it "publishes the captured name onto worldView.worldParams" $
        generationSource `shouldSatisfy` T.isInfixOf "worldName  = p.worldName"

    it "publishes a suggestion's gloss and provenance alongside it" $
        forM_ ["worldGloss = nameGloss", "languageSeed    = langSeed"
              , "languageVersion = langVersion"] $ \field →
            generationSource `shouldSatisfy` T.isInfixOf field

    ---------------------------------------------------------------
    -- #1106: which names are suggestions, and what erases that
    ---------------------------------------------------------------
    describe "world-name suggestion state (#1106)" $ do
        -- Requirement 2: the dice advances a sequence, so successive
        -- presses ask for successive ordinals of ONE seed's language.
        it "advances the reroll ordinal on every press" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "assert(nameSuggest.suggest(p, 11) == 'N11_0')"
                , "assert(nameSuggest.suggest(p, 11) == 'N11_1')"
                , "assert(nameSuggest.suggest(p, 11) == 'N11_2')"
                , "assert(p.worldName == 'N11_2', p.worldName)"
                , "assert(p.nameGloss == 'G11_2', tostring(p.nameGloss))"
                , "assert(#suggestCalls == 3, tostring(#suggestCalls))"
                , "assert(suggestCalls[1].seed == 11)"
                , "assert(suggestCalls[3].ordinal == 2)"
                ]

        it "records provenance for a suggestion" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "assert(nameSuggest.isSuggested(p))"
                , "local gloss, seed, version = nameSuggest.identity(p)"
                , "assert(gloss == 'G11_0', tostring(gloss))"
                , "assert(seed == '11000', tostring(seed))"
                , "assert(version == 5, tostring(version))"
                ]

        -- Requirement 4 / #708 principle 7: typing makes the name the
        -- player's, and it stops meaning anything. Retyping the very
        -- text that was suggested counts — authorship, not spelling.
        it "drops gloss and provenance the moment the player edits" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "nameSuggest.clear(p)"
                , "assert(not nameSuggest.isSuggested(p))"
                , "assert(nameSuggest.gloss(p) == nil)"
                , "local gloss, seed, version = nameSuggest.identity(p)"
                , "assert(gloss == nil and seed == nil and version == nil,"
                , "       'provenance survived a manual edit')"
                ]

        -- #1104: the semantic expression rides with the gloss and the
        -- provenance, all the way to world.init. Without it an accepted
        -- suggestion would record WHICH language named the world but not
        -- WHAT the name means, and its etymology would be permanently
        -- unavailable — the one case #1104 requirement 1 exists to
        -- prevent for newly created worlds.
        it "records the name EXPRESSION alongside the provenance, and \
           \forwards it as identity's fourth value" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "assert(p.nameExpr == 'Modifier:M11:H0', tostring(p.nameExpr))"
                , "local gloss, seed, version, expr = nameSuggest.identity(p)"
                , "assert(expr == 'Modifier:M11:H0', tostring(expr))"
                , "assert(gloss == 'G11_0' and seed == '11000' and version == 5)"
                ]

        it "drops the expression the moment the player edits, with the \
           \rest of the generated identity" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "nameSuggest.clear(p)"
                , "assert(p.nameExpr == nil, tostring(p.nameExpr))"
                , "local _, _, _, expr = nameSuggest.identity(p)"
                , "assert(expr == nil, 'an expression survived a manual edit')"
                ]

        it "carries the expression through a responsive rebuild's \
           \snapshot/restore, so a resize cannot silently strip a \
           \world's etymology before it is created" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "local snap = nameSuggest.snapshot(p)"
                , "assert(snap.expr == 'Modifier:M11:H0', tostring(snap.expr))"
                , "local q = newPending()"
                , "nameSuggest.restore(q, snap)"
                , "assert(q.nameExpr == 'Modifier:M11:H0', tostring(q.nameExpr))"
                , "local _, _, _, expr = nameSuggest.identity(q)"
                , "assert(expr == 'Modifier:M11:H0', tostring(expr))"
                ]

        it "keeps a manual name out of the identity path entirely" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "p.worldName = 'Handtyped'"
                , "local gloss, seed, version = nameSuggest.identity(p)"
                , "assert(gloss == nil and seed == nil and version == nil,"
                , "       'invented provenance for a typed name')"
                ]

        -- Requirement 3: a new seed is a new language, and the sequence
        -- restarts in it.
        it "restarts the sequence in a new language on a seed change" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "nameSuggest.suggest(p, 11)"
                , "assert(nameSuggest.reseed(p, 22) == 'N22_0',"
                , "       'after reseed: ' .. tostring(p.worldName))"
                , "assert(p.nameGloss == 'G22_0', tostring(p.nameGloss))"
                , "local _, seed = nameSuggest.identity(p)"
                , "assert(seed == '22000', tostring(seed))"
                ]

        it "leaves a manually typed name untouched by a seed change" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "p.worldName = 'Handtyped'"
                , "nameSuggest.clear(p)"
                , "assert(nameSuggest.reseed(p, 22) == nil)"
                , "assert(p.worldName == 'Handtyped', p.worldName)"
                , "assert(not nameSuggest.isSuggested(p))"
                ]

        -- The guard that keeps a resize rebuild from being mistaken for
        -- an edit: restoreAll re-fires the seed control's onChange with
        -- the SAME text, and re-rolling the name off that would destroy
        -- exactly the state the rebuild is preserving.
        it "ignores a seed re-notification that did not change the seed" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "local before = p.worldName"
                , "assert(nameSuggest.reseed(p, 11) == nil)"
                , "assert(nameSuggest.reseed(p, 11) == nil)"
                , "assert(p.worldName == before, p.worldName)"
                , "assert(#suggestCalls == 1, tostring(#suggestCalls))"
                ]

        -- Requirement 7: a failed suggestion reports and changes
        -- nothing. There is no fallback generator left to fall back to.
        it "leaves the name and metadata alone when suggestion fails" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "local name, gloss = p.worldName, p.nameGloss"
                , "suggestFails = true"
                , "local value, err = nameSuggest.suggest(p, 11)"
                , "assert(value == nil, tostring(value))"
                , "assert(err == 'no catalogue', tostring(err))"
                , "assert(p.worldName == name, p.worldName)"
                , "assert(p.nameGloss == gloss, tostring(p.nameGloss))"
                , "assert(nameSuggest.isSuggested(p))"
                ]

        -- Requirement 8 / the reviewer's rebuild clause: the state a
        -- resize must preserve lives on `pending`, which outlives the
        -- widgets, so a rebuild that re-reads it sees the same
        -- suggestion, gloss, provenance, and sequence position.
        it "keeps suggestion state on the table a rebuild preserves" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "nameSuggest.suggest(p, 11)"
                , "local snapshot = {"
                , "  name = p.worldName, gloss = p.nameGloss,"
                , "  seed = p.nameLanguageSeed, version = p.nameLanguageVersion,"
                , "  ordinal = p.nameOrdinal, suggested = p.nameSuggested,"
                , "}"
                , "assert(snapshot.ordinal == 2, tostring(snapshot.ordinal))"
                , "for k, v in pairs(snapshot) do"
                , "  assert(v ~= nil, 'rebuild would lose ' .. k)"
                , "end"
                ]

        -- Review round 5: requirement 4's "type over the suggestion" is
        -- impossible if the field rejects the characters the generator
        -- just put in it. The admissible set comes from the engine
        -- (outputInventory), so it cannot drift from what is rendered.
        it "admits every character a generated name can carry" $
            runsSuggest $ lns
                [ "for _, c in ipairs({ 'a', 'Z', '\\u{00E9}', '\\u{0163}',"
                , "                     '-', \"'\" }) do"
                , "  assert(nameSuggest.isNameChar(c),"
                , "         'rejected a generated-name character: ' .. c)"
                , "end"
                , "for _, c in ipairs({ '7', ' ', '/', '\\u{4E2D}' }) do"
                , "  assert(not nameSuggest.isNameChar(c),"
                , "         'admitted a non-name character: ' .. c)"
                , "end"
                ]

        it "falls back to ASCII letters when the engine cannot answer" $
            runsSuggest $ lns
                [ "package.loaded['scripts.create_world.name_suggest'] = nil"
                , "world.generatedNameCharacters = nil"
                , "local ns = require('scripts.create_world.name_suggest')"
                , "assert(ns.isNameChar('a') and ns.isNameChar('-'))"
                , "assert(not ns.isNameChar('7'))"
                ]

        it "forgets everything on Defaults" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "nameSuggest.reset(p)"
                , "assert(p.nameOrdinal == nil)"
                , "assert(p.nameSeedNum == nil)"
                , "assert(not nameSuggest.isSuggested(p))"
                ]

        -- Review round 1: the ordinal indexes ONE language's sequence.
        -- Carrying it into a new language would drop the next dice press
        -- partway into a language the player has never heard a word of,
        -- so the reset happens on every changed seed — including one
        -- that reached here past a manual edit, which takes the early
        -- return before any re-suggestion.
        it "restarts the sequence even when the name is manual" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "nameSuggest.suggest(p, 11)"
                , "nameSuggest.clear(p)"
                , "assert(nameSuggest.reseed(p, 22) == nil)"
                , "assert(p.nameOrdinal == 0, tostring(p.nameOrdinal))"
                , "assert(nameSuggest.suggest(p, 22) == 'N22_0',"
                , "       'dice landed mid-sequence: ' .. tostring(p.worldName))"
                ]

        -- Review round 1: a name can only wear the meaning of the
        -- suggestion that rendered it. reconcile is the guard for
        -- PROGRAMMATIC sets, which onUserEdit cannot see.
        it "drops a meaning that belongs to a different suggestion" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "nameSuggest.suggest(p, 11)"
                , "assert(nameSuggest.reconcile(p, p.worldName),"
                , "       'the suggestion disowned its own text')"
                , "assert(nameSuggest.gloss(p) == 'G11_0')"
                , "assert(nameSuggest.reconcile(p, 'Something Else') == false)"
                , "assert(p.worldName == 'Something Else', p.worldName)"
                , "assert(nameSuggest.gloss(p) == nil,"
                , "       'kept another expression\\'s gloss')"
                , "local _, seed = nameSuggest.identity(p)"
                , "assert(seed == nil, 'kept another language\\'s provenance')"
                ]

        -- Review round 1, the flow that made reconcile necessary: a
        -- resize with an unsubmitted seed edit. Teardown submits the
        -- seed (re-suggesting in a NEW language), then the widget
        -- restore puts the old name back. Snapshotting the meaning
        -- alongside is what keeps the pair honest.
        it "survives a rebuild that submits a pending seed edit" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "p.seed = 'B'"
                , "nameSuggest.suggest(p, 11)"
                , "local snap = nameSuggest.snapshot(p)"
                , "local shownName = p.worldName"
                -- Teardown: the seed control unfocuses, committing the
                -- edit the player never submitted.
                , "p.seed = 'C'"
                , "nameSuggest.reseed(p, 22)"
                , "assert(p.worldName ~= shownName, 'reseed did nothing')"
                -- Rebuild: restoreAll puts the old text back, and the
                -- name control's onChange reconciles against it.
                , "nameSuggest.reconcile(p, shownName)"
                , "assert(nameSuggest.gloss(p) == nil,"
                , "       'paired the old name with the new language')"
                -- Then the (seed, name, meaning) snapshot is restored.
                , "nameSuggest.restore(p, snap)"
                , "assert(p.worldName == shownName, p.worldName)"
                , "assert(nameSuggest.gloss(p) == 'G11_0', tostring(p.nameGloss))"
                , "local _, seed = nameSuggest.identity(p)"
                , "assert(seed == '11000', tostring(seed))"
                , "assert(p.nameSeedNum == 11, tostring(p.nameSeedNum))"
                ]

        -- Review round 2: the seed that produced a name is part of the
        -- tuple. Restoring the name without it would leave generation
        -- reading the seed the teardown committed while the identity
        -- describes the language the name came from.
        it "restores the committed seed alongside the name it produced" $
            runsSuggest $ lns
                [ "local p = newPending()"
                , "p.seed = 'B'"
                , "nameSuggest.suggest(p, 11)"
                , "local snap = nameSuggest.snapshot(p)"
                -- Teardown commits an edit the player never submitted.
                , "p.seed = 'C'"
                , "nameSuggest.reseed(p, 22)"
                , "nameSuggest.restore(p, snap)"
                , "assert(p.seed == 'B', 'committed seed: ' .. tostring(p.seed))"
                , "assert(p.nameSeedNum == 11, tostring(p.nameSeedNum))"
                -- Submitting the still-pending edit now re-suggests
                -- exactly as it would have without the rebuild.
                , "p.seed = 'C'"
                , "assert(nameSuggest.reseed(p, 22) == 'N22_0',"
                , "       'pending edit no longer re-suggests: '"
                , "       .. tostring(p.worldName))"
                ]

    -- Review round 1: a keystroke that leaves the buffer untouched is
    -- not an edit. Reporting one would strip a suggested name's gloss
    -- and provenance while the name on screen never changed, so
    -- generating it would persist it as though the player had typed it.
    describe "randbox user-edit reporting (#1106)" $ do
        it "stays silent on a backspace at the start of the field" $
            runsRandbox $ lns
                [ "local id = newNameBox('Karadun')"
                , "randbox.setCursor(id, 0)"
                , "assert(randbox.onBackspace())"
                , "assert(#edits == 0, 'reported ' .. tostring(edits[1]))"
                , "assert(randbox.getValue(id) == 'Karadun',"
                , "       randbox.getValue(id))"
                ]

        it "stays silent on a delete at the end of the field" $
            runsRandbox $ lns
                [ "local id = newNameBox('Karadun')"
                , "randbox.setCursor(id, 7)"
                , "assert(randbox.onDelete())"
                , "assert(#edits == 0, 'reported ' .. tostring(edits[1]))"
                , "assert(randbox.getValue(id) == 'Karadun',"
                , "       randbox.getValue(id))"
                ]

        it "reports a keystroke that does change the text" $
            runsRandbox $ lns
                [ "local id = newNameBox('Karadun')"
                , "randbox.setCursor(id, 7)"
                , "assert(randbox.onBackspace())"
                , "assert(randbox.onCharInput('x'))"
                , "assert(#edits == 2, tostring(#edits))"
                , "assert(edits[1] == 'Karadu', edits[1])"
                , "assert(edits[2] == 'Karadux', edits[2])"
                ]

        -- A rejected character is consumed but changes nothing, so it
        -- is not an edit either.
        it "stays silent on a rejected character" $
            runsRandbox $ lns
                [ "local id = newNameBox('Karadun')"
                , "assert(randbox.onCharInput('7'))"
                , "assert(#edits == 0, 'reported ' .. tostring(edits[1]))"
                ]

        -- Review round 3: an empty field is a real state, reached
        -- whenever the World Name a player cleared is rebuilt — no
        -- default to restore, and autoGenerate off so the rebuild can't
        -- refill it. Everything downstream of the initial value needs a
        -- string, so nil must not survive construction.
        it "constructs an empty field when there is no value to show" $
            runsRandbox $ lns
                [ "local id = newNameBox(nil)"
                , "assert(id, 'construction failed')"
                , "assert(randbox.getValue(id) == '', randbox.getValue(id))"
                ]

        -- Review round 5: the field consults an injected validator, so
        -- a World Name can admit exactly what the generator produces.
        -- The default NAME class is ASCII-only, which is why the
        -- injection had to exist rather than the class being widened
        -- for every randbox.
        it "honours an injected character validator" $
            runsRandbox $ lns
                [ "local id = randbox.new{"
                , "  name = 'world_name', page = 1, font = 1,"
                , "  randType = randbox.Type.NAME, default = 'Kara',"
                , "  autoGenerate = false,"
                , "  validateChar = function(c)"
                , "    return c == '\\u{00E9}' or c == \"'\" or c == '-'"
                , "  end,"
                , "}"
                , "randbox.focus(id)"
                , "randbox.setCursor(id, 4)"
                , "assert(randbox.onCharInput('\\u{00E9}'))"
                , "assert(randbox.onCharInput(\"'\"))"
                , "assert(randbox.onCharInput('7'))"
                , "assert(randbox.getValue(id) == 'Kara\\u{00E9}\\u{0027}',"
                , "       randbox.getValue(id))"
                ]

        it "rejects a generated name's own letters without one" $
            runsRandbox $ lns
                [ "local id = newNameBox('Kara')"
                , "randbox.setCursor(id, 4)"
                , "assert(randbox.onCharInput('\\u{00E9}'))"
                , "assert(randbox.getValue(id) == 'Kara',"
                , "       'the ASCII default is no longer ASCII-only: '"
                , "       .. randbox.getValue(id))"
                ]

        -- The other way in: the initial suggestion failed, so the
        -- generator declined. Same requirement — an empty field, not a
        -- crashed menu and not an invented name.
        it "constructs an empty field when the generator declines" $
            runsRandbox $ lns
                [ "local reported"
                , "local id = randbox.new{"
                , "  name = 'world_name', page = 1, font = 1,"
                , "  randType = randbox.Type.NAME,"
                , "  generate = function() return nil end,"
                , "  onChange = function(v) reported = v end,"
                , "}"
                , "assert(id, 'construction failed')"
                , "assert(randbox.getValue(id) == '', randbox.getValue(id))"
                , "assert(reported == '', tostring(reported))"
                ]

    -- The Create World screen and generation.lua must normalize the
    -- seed text identically, or the language a name is suggested in is
    -- not the language the generated world records.
    it "derives the language seed the same way generation.lua does" $ do
        settingsSource `shouldSatisfy` T.isInfixOf "tonumber(seedText or \"\", 16) or 0"
        generationSource `shouldSatisfy` T.isInfixOf "tonumber(p.seed, 16) or 0"

    -- The validator only helps the player if the World Name control
    -- actually gets it; both halves are behaviourally covered above,
    -- and this is the wire between them.
    it "gives the World Name control the generated-name validator" $
        settingsSource `shouldSatisfy`
            T.isInfixOf "validateChar = nameSuggest.isNameChar"

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
