{-# LANGUAGE ScopedTypeVariables #-}
-- | The "Lua shared helpers" gate (issue #1158): @scripts/lib/numeric.lua@'s
--   @clamp@ and @scripts/lib/game_time.lua@'s @formatHMS@, the two helpers
--   that used to be copy-pasted into fifteen modules (eleven physiology
--   copies of @clamp@, four log-panel copies of @formatGameTimeHMS@).
--
--   Two independent gates live here, because consolidation can regress in
--   two unrelated ways:
--
--   1. __Behaviour__ -- each helper is exercised in a standalone Lua VM
--      (stdlib only, no engine, no world/unit threads), the pattern
--      "Test.Headless.Lua.SaveModules" already uses: one self-contained
--      chunk per 'it' via 'Lua.dostring', asserting inside Lua. The VM
--      deliberately defines NO @engine@ global, so a helper that ever
--      reached for one would fail here rather than in a boot.
--
--   2. __Shape__ -- the tree itself is read back: exactly one
--      implementation of each helper, under @scripts/lib/@; zero surviving
--      inline copies anywhere under @scripts/@; and an import in every one
--      of the fifteen consumers. That half is what stops a future edit
--      from quietly reintroducing a sixteenth private copy, which is
--      exactly how the eleven arose. It is spelled as a structural check
--      rather than a @local function@ grep on purpose: the library style
--      exports table members, so a definition-count grep would be blind to
--      the very shape this ships.
--
--   Runs with @cabal test@'s CWD at the repo root, like every other
--   repo-root-relative Lua path in this codebase, so @require("scripts.lib.…")@
--   resolves through Lua's default @package.path@ with no extra setup.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua shared helpers"'@.
module Test.Headless.Lua.SharedHelpers (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE

-- | Run one self-contained Lua chunk in a fresh stdlib-only interpreter.
--   The chunk signals failure through Lua's own @assert()@/@error()@; a
--   non-OK 'Lua.Status' becomes an hspec failure carrying the Lua message.
--
--   No @engine@ stub is installed, unlike 'Test.Headless.Lua.SaveModules':
--   these two modules are leaves that must reach nothing but @math@ and
--   @string@, so the absence is itself part of the assertion.
runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | The eleven modules that carried a private @clamp@ (issue #1158's list).
clampConsumers ∷ [FilePath]
clampConsumers =
    [ "scripts/brain.lua", "scripts/cardio.lua", "scripts/circulation.lua"
    , "scripts/consumable.lua", "scripts/exhaustion.lua"
    , "scripts/mental_state.lua", "scripts/movement_speed.lua"
    , "scripts/salts.lua", "scripts/starvation.lua", "scripts/thermo.lua"
    , "scripts/thoughts.lua" ]

-- | The four modules that carried a private @formatGameTimeHMS@.
formatterConsumers ∷ [FilePath]
formatterConsumers =
    [ "scripts/combat_log.lua", "scripts/injury_log_panel.lua"
    , "scripts/thought_log.lua", "scripts/unit_log.lua" ]

-- | Every @.lua@ file the shape checks read: the fifteen consumers plus
--   the two library modules plus the deliberately-untouched
--   @scripts/event_log.lua@ (whose own formatter is a different display
--   contract and must survive).
scannedScripts ∷ [FilePath]
scannedScripts =
    clampConsumers <> formatterConsumers
        <> [ "scripts/lib/numeric.lua", "scripts/lib/game_time.lua"
           , "scripts/event_log.lua" ]

readScript ∷ FilePath → IO Text
readScript = TIO.readFile

spec ∷ Spec
spec = do

    describe "clamp behaviour" $ do
        it "clamps below, within, and above the range, floats included" $
            runsOk $ lns
                [ "local clamp = require('scripts.lib.numeric').clamp"
                , "assert(clamp(-5, 0, 10) == 0, 'below the range must give lo')"
                , "assert(clamp(0, 0, 10) == 0, 'the lower bound is inclusive')"
                , "assert(clamp(4, 0, 10) == 4, 'inside the range is identity')"
                , "assert(clamp(10, 0, 10) == 10, 'the upper bound is inclusive')"
                , "assert(clamp(99, 0, 10) == 10, 'above the range must give hi')"
                , "assert(clamp(0.25, 0.0, 1.0) == 0.25, 'fractions pass through')"
                , "assert(clamp(-0.5, 0.0, 1.0) == 0.0, 'fractions clamp low')"
                , "assert(clamp(1.5, 0.0, 1.0) == 1.0, 'fractions clamp high')"
                , "assert(clamp(-3, -2, 5) == -2, 'negative bounds work')"
                ]

        it "keeps the eleven copies' degenerate max-wins-last ordering" $
            -- math.max(lo, math.min(hi, x)) with lo > hi always yields lo.
            -- No physiology caller does this, but the eleven copies all
            -- behaved this way, so "behaviour is unchanged" pins it.
            runsOk $ lns
                [ "local clamp = require('scripts.lib.numeric').clamp"
                , "assert(clamp(0, 10, 5) == 10, 'inverted bounds must still give lo')"
                , "assert(clamp(99, 10, 5) == 10, 'inverted bounds ignore x entirely')"
                ]

    describe "formatHMS behaviour" $ do
        it "formats an ordinary time zero-padded in all three fields" $
            runsOk $ lns
                [ "local fmt = require('scripts.lib.game_time').formatHMS"
                , "assert(fmt(0) == '00:00:00', 'zero is fully padded')"
                , "assert(fmt(1) == '00:00:01', 'seconds pad')"
                , "assert(fmt(61) == '00:01:01', 'minutes carry and pad')"
                , "assert(fmt(3661) == '01:01:01', 'hours carry and pad')"
                , "assert(fmt(45296) == '12:34:56', 'a plain HMS reading')"
                ]

        it "reads a nil time as zero rather than raising" $
            runsOk $ lns
                [ "local fmt = require('scripts.lib.game_time').formatHMS"
                , "assert(fmt(nil) == '00:00:00', 'the t or 0 guard must survive')"
                , "assert(fmt() == '00:00:00', 'a missing argument is the same guard')"
                ]

        it "floors a negative time at zero" $
            runsOk $ lns
                [ "local fmt = require('scripts.lib.game_time').formatHMS"
                , "assert(fmt(-1) == '00:00:00', 'a negative second floors at zero')"
                , "assert(fmt(-99999) == '00:00:00', 'so does a large negative')"
                ]

        it "truncates a fractional time toward zero" $
            runsOk $ lns
                [ "local fmt = require('scripts.lib.game_time').formatHMS"
                , "assert(fmt(1.9) == '00:00:01', 'fractions floor, never round')"
                , "assert(fmt(59.999) == '00:00:59', 'a fraction never carries a minute')"
                , "assert(fmt(-0.5) == '00:00:00', 'a negative fraction floors to zero')"
                ]

        it "keeps counting hours past 24 instead of wrapping" $
            runsOk $ lns
                [ "local fmt = require('scripts.lib.game_time').formatHMS"
                , "assert(fmt(86400) == '24:00:00', 'exactly one day does not wrap')"
                , "assert(fmt(90061) == '25:01:01', 'past a day keeps accumulating')"
                , "assert(fmt(259200) == '72:00:00', 'a three-day session reads 72 hours')"
                , "assert(fmt(360000) == '100:00:00', 'three digits are not truncated')"
                ]

    describe "the helpers are pure standard-library leaves" $ do
        it "loads both with no engine global present at all" $
            -- Requirement 5's load-order guarantee, made mechanical: if
            -- either module reached an engine global or required a
            -- consumer, this stdlib-only VM would fail here.
            runsOk $ lns
                [ "assert(rawget(_G, 'engine') == nil, 'this VM must have no engine')"
                , "require('scripts.lib.numeric')"
                , "require('scripts.lib.game_time')"
                , "for name in pairs(package.loaded) do"
                , "  assert(name == 'scripts.lib.numeric'"
                , "      or name == 'scripts.lib.game_time'"
                , "      or not name:match('^scripts%.'),"
                , "    'a library module pulled in ' .. name)"
                , "end"
                ]

    describe "one definition each, imported by every consumer" $ do
        it "defines each helper exactly once, and only under scripts/lib" $ do
            numeric  ← readScript "scripts/lib/numeric.lua"
            gameTime ← readScript "scripts/lib/game_time.lua"
            occurrences "function numeric.clamp(" numeric `shouldBe` 1
            occurrences "function gameTime.formatHMS(" gameTime `shouldBe` 1

        it "leaves no inline copy of either helper anywhere under scripts" $ do
            -- The fifteen consumers plus event_log: a reintroduced private
            -- copy in any of them fails here.
            forM_ scannedScripts $ \path → do
                body ← readScript path
                (path, occurrences "local function clamp" body) `shouldBe` (path, 0)
                (path, occurrences "local function formatGameTimeHMS" body)
                    `shouldBe` (path, 0)

        it "imports clamp from the library in all eleven physiology modules" $
            forM_ clampConsumers $ \path → do
                body ← readScript path
                (path, T.isInfixOf "require(\"scripts.lib.numeric\").clamp" body)
                    `shouldBe` (path, True)

        it "imports formatHMS from the library in all four log modules" $
            forM_ formatterConsumers $ \path → do
                body ← readScript path
                (path, T.isInfixOf "require(\"scripts.lib.game_time\").formatHMS" body)
                    `shouldBe` (path, True)

        it "leaves the three different-contract formatters alone" $ do
            -- Explicitly OUT of this consolidation: formatGameTimeHM is the
            -- coarser HH:MM tab title, and event_log's formatGameTime emits
            -- MM:SS below an hour with an unpadded hour above one. Folding
            -- either into formatHMS would be a visible behaviour change.
            combat ← readScript "scripts/combat_log.lua"
            injury ← readScript "scripts/injury_log_panel.lua"
            events ← readScript "scripts/event_log.lua"
            occurrences "local function formatGameTimeHM(" combat `shouldBe` 1
            occurrences "local function formatGameTimeHM(" injury `shouldBe` 1
            occurrences "local function formatGameTime(" events `shouldBe` 1

-- | Non-overlapping occurrence count of a literal needle.
occurrences ∷ Text → Text → Int
occurrences needle haystack = length (T.breakOnAll needle haystack)
