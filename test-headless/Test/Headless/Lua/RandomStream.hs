{-# LANGUAGE TypeApplications #-}
-- | Who owns Lua's process-global random stream (#1330).
--
--   A Lua state has exactly ONE @math.random@ stream and gameplay draws
--   from it — AI decision cadence, thoughts, mental state, wildlife
--   wander, sleep, water scanning, location rolls. @scripts/ui/randbox.lua@
--   used to both SEED that stream (@math.randomseed(os.time())@ inside
--   @randbox.init@, which every boot mode reaches) and DRAW from it
--   (eight draws per suggested world seed), so two engines launched in
--   the same second simulated identically and clicking randomize shifted
--   every later simulation decision.
--
--   These are BEHAVIOURAL contracts, run against the real
--   @scripts/ui/randbox.lua@ over a synthetic UI backend, because a
--   grep-shaped assertion cannot see either defect: the stream's
--   position is not a property of any line of source. The two source
--   guards at the end exist for the opposite reason — to stop the same
--   defect reappearing in a file these behavioural chunks never boot.
module Test.Headless.Lua.RandomStream (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | Everything the real randbox module reaches outside an engine boot,
--   plus the helpers the chunks below drive it with.
--
--   @os.time@ is PINNED first, before anything is required. That is what
--   makes "two instances started in the same second" a property of the
--   fixture rather than of how fast the suite happens to run — and it is
--   what makes the two-state case below fail if @randbox.init@ ever goes
--   back to seeding from it.
prelude ∷ Text
prelude = lns
    [ "os.time = function() return 1755300000 end"
    , "local buffers, cursors, nextHandle = {}, {}, 1"
    , "local function newHandle()"
    , "  local h = nextHandle; nextHandle = h + 1"
    , "  buffers[h] = ''; cursors[h] = 0; return h"
    , "end"
    , "UI = setmetatable({}, { __index = function() return function() end end })"
    , "UI.newBox = function() return newHandle() end"
    , "UI.newText = function() return newHandle() end"
    , "UI.newSprite = function() return newHandle() end"
    , "UI.setTextInput = function(h, t) buffers[h] = t or '' end"
    , "UI.getTextInput = function(h) return buffers[h] or '' end"
    , "UI.setCursor = function(h, p) cursors[h] = p end"
    , "UI.getCursor = function(h) return cursors[h] or 0 end"
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
    , "function seedBox()"
    , "  return randbox.new{ name = 'seed', page = 1, font = 1,"
    , "                      randType = randbox.Type.HEX_SEED }"
    , "end"
    , "function numberBox(lo, hi)"
    , "  return randbox.new{ name = 'num', page = 1, font = 1,"
    , "                      randType = randbox.Type.NUMBER,"
    , "                      randParams = { min = lo, max = hi } }"
    , "end"
    -- %.17g round-trips a double exactly, so two streams that have
    -- diverged can never compare equal through this.
    , "function draw() return string.format('%.17g', math.random()) end"
    ]

-- | Run one Lua program in the given state and return the string it
--   returns. The state is NOT closed here: the two-state case needs both
--   of its states alive at once.
runProgram ∷ Lua.State → Text → IO (Either Text Text)
runProgram st src = Lua.runWith @Lua.Exception st $ do
    Lua.openlibs
    status ← Lua.dostring (TE.encodeUtf8 (prelude <> "\n" <> src))
    case status of
        Lua.OK → do
            value ← Lua.tostring (-1)
            pure $ maybe (Left "chunk returned no string")
                         (Right ∘ TE.decodeUtf8Lenient) value
        _ → do
            err ← Lua.tostring (-1)
            pure ∘ Left $ maybe "<no message>" TE.decodeUtf8Lenient err

expectOk ∷ Either Text Text → IO Text
expectOk (Right value) = pure value
expectOk (Left msg)    = expectationFailure (T.unpack msg) ⌦ \_ → pure ""

-- | Run one program in a fresh state of its own.
evalFresh ∷ Text → IO Text
evalFresh src =
    bracket Lua.newstate Lua.close (\st → runProgram st src) ⌦ expectOk

-- | Both states stay open for the whole action, which is what makes
--   their addresses — and so Lua's own per-state auto-seed — differ.
withTwoStates ∷ (Lua.State → Lua.State → IO α) → IO α
withTwoStates act =
    bracket Lua.newstate Lua.close $ \first →
    bracket Lua.newstate Lua.close $ \second →
    act first second

-- | Lines where @token@ appears as live code rather than inside a
--   comment. Judged by whether @--@ opens before it on the same line,
--   which is what lets the modules under test explain the rule in prose
--   while still being held to it.
callSites ∷ Text → Text → [Int]
callSites token source =
    [ number
    | (number, line) ← zip [1 ..] (T.lines source)
    , let (before, rest) = T.breakOn token line
    , not (T.null rest)
    , not ("--" `T.isInfixOf` before)
    ]

luaSources ∷ FilePath → IO [FilePath]
luaSources root = do
    entries ← listDirectory root
    concat ⊚ forM entries (\entry → do
        let path = root ⊘ entry
        isDir ← doesDirectoryExist path
        if isDir
            then luaSources path
            else pure [path | takeExtension path ≡ ".lua"])

offenders ∷ Text → FilePath → IO [String]
offenders token root = do
    paths ← luaSources root
    concat ⊚ forM paths (\path → do
        source ← TIO.readFile path
        pure [path <> ":" <> show number | number ← callSites token source])

spec ∷ Spec
spec = do
    -- Requirement 1, measured against a state that pins the stream and
    -- then never boots the widget at all. Comparing the two randbox
    -- programs only to EACH OTHER would miss a restored
    -- `math.randomseed(os.time())`, which moves both of them by the same
    -- amount; the baseline is what makes init's own side effects visible.
    it "randbox neither seeds nor advances the gameplay stream" $ do
        let pinned = "math.randomseed(20260816)"
        baseline ← evalFresh $ lns
            [ pinned
            , "return draw()"
            ]
        afterInit ← evalFresh $ lns
            [ pinned
            , "randbox.init()"
            , "return draw()"
            ]
        afterUse ← evalFresh $ lns
            [ pinned
            , "randbox.init()"
            , "local id = seedBox()"
            , "randbox.randomize(id)"
            , "randbox.randomize(id)"
            , "randbox.newHexSeed()"
            , "local n = numberBox(0, 9999)"
            , "randbox.randomize(n)"
            , "return draw()"
            ]
        (afterInit, afterUse) `shouldBe` (baseline, baseline)

    -- Requirements 3 and 4. No seed is pinned here: this is the boot
    -- path's own entropy, measured with os.time held equal. Restoring
    -- `math.randomseed(os.time())` to randbox.init collapses the two
    -- gameplay draws onto each other; a UI stream seeded from the clock
    -- alone would collapse the two suggested seeds.
    it "two states brought up in the same second stay independent" $
        withTwoStates $ \first second → do
            let probe = lns
                    [ "randbox.init()"
                    , "local gameplay = draw()"
                    , "local id = seedBox()"
                    , "randbox.randomize(id)"
                    , "return gameplay .. '|' .. randbox.getValue(id)"
                    ]
            firstOut  ← runProgram first probe ⌦ expectOk
            secondOut ← runProgram second probe ⌦ expectOk
            let split = T.breakOn "|"
                (firstDraw, firstSeed)   = split firstOut
                (secondDraw, secondSeed) = split secondOut
            firstDraw `shouldNotBe` secondDraw
            firstSeed `shouldNotBe` secondSeed

    -- Requirement 4, through the player-facing path: randomize, not
    -- newHexSeed. The format is the one Create World has always shown.
    it "successive randomize presses roll fresh eight-digit hex seeds" $ do
        out ← evalFresh $ lns
            [ "randbox.init()"
            , "local id = seedBox()"
            , "local seen = {}"
            , "for _ = 1, 4 do"
            , "  randbox.randomize(id)"
            , "  seen[#seen + 1] = randbox.getValue(id)"
            , "end"
            , "return table.concat(seen, ' ')"
            ]
        let seeds = T.words out
        length seeds `shouldBe` 4
        forM_ seeds $ \seed →
            (seed, T.length seed, T.all (`elem` ("0123456789ABCDEF" ∷ String)) seed)
                `shouldBe` (seed, 8, True)
        length (nubText seeds) `shouldBe` 4

    -- Requirement 4's other half: the NUMBER generator keeps math.random's
    -- inclusive-at-both-ends range contract on the new stream.
    it "a NUMBER randbox still covers exactly its declared range" $ do
        out ← evalFresh $ lns
            [ "randbox.init()"
            , "local n = numberBox(7, 11)"
            , "local seen = {}"
            , "for _ = 1, 200 do"
            , "  randbox.randomize(n)"
            , "  seen[#seen + 1] = randbox.getValue(n)"
            , "end"
            , "return table.concat(seen, ' ')"
            ]
        let values = T.words out
        length values `shouldBe` 200
        nubText values `shouldMatchList` ["7", "8", "9", "10", "11"]

    -- Requirement 2, and the reason it is stated as a repository rule
    -- rather than a randbox rule: the defect's natural next home is
    -- whichever boot module calls randbox.init.
    it "no script under scripts/ reseeds the gameplay stream" $
        offenders "math.randomseed" "scripts" ⌦ (`shouldBe` [])

    it "no UI module draws from the gameplay stream" $
        offenders "math.random" ("scripts" ⊘ "ui") ⌦ (`shouldBe` [])

-- | Distinct values, order-independent — the lists here are tiny, so a
--   quadratic pass is cheaper to read than an Ord-keyed one.
nubText ∷ [Text] → [Text]
nubText = foldl (\seen value → if value `elem` seen then seen else seen <> [value]) []
