-- | #1932: the canonical save order @engine.listSaves()@ publishes has
--   to survive the main menu.
--
--   'World.Save.Serialize.saveListingOrder' sorts by descending
--   normalized timestamp with an ascending slot-name tiebreak, and
--   'Engine.Scripting.Lua.API.Save' marshals that listing into a dense
--   Lua array without reordering it. @scripts\/main_menu.lua@ then held
--   the last word: it re-sorted the array with @a.timestamp >
--   b.timestamp@, a comparator with no tiebreak, through Lua's
--   deliberately unstable @table.sort@. Rows sharing a timestamp could
--   come out in any permutation, and both player-facing consumers read
--   that permuted list — @mainMenu.latestSave@ is the Continue target,
--   and @scripts\/ui_manager_menu.lua@ hands the very same table to
--   @saveBrowser.show@, which renders in received order.
--
--   The defect this pins is the loss of the engine's canonical order,
--   not recency: which of two tied legacy saves is truly newer is
--   unknowable, and nothing here guesses it. What the engine already
--   guarantees is that one saves directory has one order, and that is
--   what the menu must present.
--
--   So every expectation below is computed by CALLING
--   'saveListingOrder' on the fixture rather than by restating what it
--   ought to produce: a test carrying its own copy of the comparator
--   would keep passing if the two ever disagreed, which is the whole
--   failure mode.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "main-menu save order"'@.
module Test.Headless.Save.MenuListingOrder (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import World.Save.Serialize (SaveListing(..), saveListingOrder)
import World.Save.Types (SaveMetadata(..))

-- | One listing row. Only the slot name and the timestamp matter to the
--   ordering under test; every other field is held constant so a
--   permutation cannot hide behind an incidental difference.
row ∷ Text → Text → SaveListing
row slot timestamp = SaveListing
    { slName      = slot
    , slRecovered = False
    , slMetadata  = SaveMetadata
        { smName       = slot
        , smSeed       = 42
        , smWorldSize  = 64
        , smPlateCount = 3
        , smTimestamp  = timestamp
        , smWorldName  = Nothing
        , smWorldGloss = Nothing
        , smAutosave   = False
        , smGeneratedWorldIds = []
        }
    }

-- | The fixture, in an arbitrary order standing in for whatever
--   'listSaves' found on disk before it sorted. It mixes the three
--   cases that matter:
--
--   * two distinct timestamps that fix the coarse newest-first order;
--   * a four-way tie on one identical timestamp string — reachable
--     because two legacy second-precision saves both normalize to the
--     same @…00.000000Z@, and because the per-process monotonic clamp
--     on new save timestamps cannot separate saves written by two
--     different engine processes;
--   * a four-way tie on a string 'normalizeTimestamp' could not parse
--     and therefore passed through untouched. Compared as raw strings
--     it sorts to the TOP (@\'s\' > \'2\'@), so Continue's own target is
--     drawn from inside a tie group — deliberately, since that is the
--     row a permutation costs the player most.
--
--   The tie groups are deliberately not in slot-name order here, so
--   holding the engine's order is distinguishable from holding the
--   input's.
fixture ∷ [SaveListing]
fixture =
    [ row "kestrel"     "2026-08-19T09:00:00.000000Z"
    , row "delta_camp"  "2026-08-19T12:00:00.000000Z"
    , row "bad_clock_b" "sometime last winter"
    , row "aerie"       "2026-08-19T09:00:00.000000Z"
    , row "harrow"      "2026-08-19T08:00:00.000000Z"
    , row "gannet"      "2026-08-19T09:00:00.000000Z"
    , row "bad_clock_a" "sometime last winter"
    , row "cinder"      "2026-08-19T09:00:00.000000Z"
    , row "bad_clock_d" "sometime last winter"
    , row "bad_clock_c" "sometime last winter"
    ]

-- | What @engine.listSaves()@ would really return for 'fixture': the
--   producer's own comparator, called rather than paraphrased.
canonical ∷ [SaveListing]
canonical = saveListingOrder fixture

canonicalNames ∷ [Text]
canonicalNames = map slName canonical

-- | 'canonical' rendered as the Lua array literal @engine.listSaves()@
--   marshals — dense, sequential, already in the engine's order.
listingLiteral ∷ [SaveListing] → Text
listingLiteral listings =
    "{" <> T.intercalate "," (map one listings) <> "}"
  where
    one l = "{name='" <> slName l <> "',timestamp='"
                <> smTimestamp (slMetadata l) <> "',autosave=false}"

-- | Install the stub and require the real module. Nothing here calls
--   @mainMenu.init@: 'buildMenuItems' is pure list work over the stub's
--   return value, and keeping the UI out of it is what lets this gate
--   read the order without a page, a font or a texture.
menuSetup ∷ Text
menuSetup = T.unwords
    [ "engine.listSaves = function() return"
    , listingLiteral canonical
    , "end;"
    , "local m = require('scripts.main_menu');"
    ]

-- | #1357: engine initialization is itself a @config\/@ writer, so the
--   filesystem boundary goes OUTSIDE 'withHeadlessEngine'. Nothing in
--   this gate writes config deliberately; the wrapper is here so that
--   stays true of the engine boot each example performs.
withOrderEngine ∷ (EngineEnv → IO α) → IO α
withOrderEngine = withIsolatedResourceRoot ∘ withHeadlessEngine

spec ∷ Spec
spec = around withOrderEngine $
    describe "main-menu save order (#1932)" $ do
        it "holds engine.listSaves()' order verbatim, including the \
           \name tiebreak among tied timestamps" $ \env → do
            ls ← newBareLuaBackend env
            names ← evalNames ls $ T.unwords
                [ menuSetup
                , "m.buildMenuItems();"
                , "local out = {};"
                , "for _, s in ipairs(m.saves) do out[#out+1] = s.name end;"
                , "return table.concat(out, ',')"
                ]
            names `shouldBe` canonicalNames

            -- Stated separately so a fixture that stopped containing
            -- ties would fail here rather than silently weaken the
            -- case above: both four-row groups really do share one
            -- timestamp string, and both really are name-ascending.
            tiedGroup "2026-08-19T09:00:00.000000Z"
                `shouldBe` ["aerie", "cinder", "gannet", "kestrel"]
            tiedGroup "sometime last winter"
                `shouldBe` [ "bad_clock_a", "bad_clock_b"
                           , "bad_clock_c", "bad_clock_d" ]

        it "picks Continue's target from the engine's first row" $ \env → do
            ls ← newBareLuaBackend env
            latest ← evalDebug ls $ T.unwords
                [ menuSetup, "m.buildMenuItems();", "return m.latestSave" ]
            -- executeDebugLua quotes a string return value.
            case canonicalNames of
                (first : _) → latest `shouldBe` "\"" <> first <> "\""
                []          → expectationFailure "empty fixture"

        it "builds the same order every time from the same listing" $ \env → do
            ls ← newBareLuaBackend env
            names ← evalNames ls $ T.unwords
                [ menuSetup
                , "local seen = nil;"
                , "for _ = 1, 8 do"
                , "  m.buildMenuItems();"
                , "  local out = {};"
                , "  for _, s in ipairs(m.saves) do out[#out+1] = s.name end;"
                , "  local joined = table.concat(out, ',');"
                , "  if seen and seen ~= joined then return 'UNSTABLE' end;"
                , "  seen = joined;"
                , "end;"
                , "return seen"
                ]
            names `shouldBe` canonicalNames

        it "renders the save browser's rows in that same order" $ \env → do
            ls ← newBareLuaBackend env
            let browserSetup = T.unwords
                    [ "require('scripts.ui.list').init();"
                    , menuSetup
                    , "m.buildMenuItems();"
                    -- Exactly what scripts/ui_manager_menu.lua line 92
                    -- does: the menu's own table, forwarded unchanged.
                    , "local b = require('scripts.save_browser');"
                    , "b.init(1,2,3,1280,720);"
                    , "b.show(m.saves, function() end, function() end);"
                    ]
            -- Row position -> the slot name that row dispatches, which
            -- is the load key #1107 pinned. Read by index rather than
            -- from the rendered dump so the whole list is covered even
            -- when the panel's height fit leaves some rows scrolled
            -- out of view.
            values ← evalNames ls $ T.unwords
                [ browserSetup
                , "local list = require('scripts.ui.list');"
                , "local out = {};"
                , "for i = 1, #m.saves do"
                , "  list.selectItem(b.listId, i);"
                , "  out[#out+1] = list.getSelectedValue(b.listId)"
                , "end;"
                , "return table.concat(out, ',')"
                ]
            values `shouldBe` canonicalNames

            -- And the rows actually on screen carry those same names,
            -- in the same order, so this is the displayed sequence and
            -- not just the backing array's.
            ls' ← newBareLuaBackend env
            rendered ← evalJoined "|" ls' $ T.unwords
                [ browserSetup
                , "local out = {};"
                , "for _, e in ipairs(require('scripts.ui.list').dump()) do"
                , "  out[#out+1] = e.label"
                , "end;"
                , "return table.concat(out, '|')"
                ]
            length rendered `shouldSatisfy` (> 1)
            zipWith T.isInfixOf canonicalNames rendered
                `shouldBe` replicate (length rendered) True

-- | The slot names 'canonical' assigns to one tied timestamp, in the
--   order it put them.
tiedGroup ∷ Text → [Text]
tiedGroup timestamp =
    [ slName l | l ← canonical, smTimestamp (slMetadata l) ≡ timestamp ]

-- * Real-Lua-backend helpers (the same bare backend
--   'Test.Headless.UI.Slider' and 'Test.Headless.UI.InputOwnership'
--   build: the full Lua API registered, nothing preloaded, so
--   scripts/main_menu.lua is pulled in exactly as a real caller's
--   @require@ would pull it).

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls code = do
    out ← executeDebugLua (lbsLuaState ls) code
    when (isLuaError out) $
        expectationFailure ("Lua error: " ⧺ T.unpack out)
    pure out

-- | Read back a list Lua joined on @sep@. It comes over as one string
--   because 'executeDebugLua' quotes a string result; splitting here
--   keeps a failure's output a readable list rather than one long line.
evalJoined ∷ Text → LuaBackendState → Text → IO [Text]
evalJoined sep ls code = do
    out ← evalDebug ls code
    pure (T.splitOn sep (T.dropAround (≡ '"') out))

-- | Slot names. Every name in 'fixture' is comma-free, which is what
--   makes a comma a safe join separator here.
evalNames ∷ LuaBackendState → Text → IO [Text]
evalNames = evalJoined ","

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t
