{-# LANGUAGE Strict #-}
-- | Farm designation claim selection (issue #2534).
--
--   Tilling and planting used to score the UNCONDITIONAL nearest
--   designation and then return @-math.huge@ whenever another worker
--   held it. Clustered farmers therefore reported no farming work at
--   all while one colleague worked their shared nearest tile, however
--   many free designations sat just beyond it. The fix is the pair of
--   claim-aware engine queries @till.nearestFreeDesignation@ /
--   @plant.nearestFreeDesignation@, driven from
--   @scripts/unit_ai_claims.lua@'s @nearestFree@.
--
--   WHAT IS REAL HERE. The selection is the SHIPPING code end to end:
--   the production @scripts/unit_ai_farm.lua@ utility and execute, the
--   production claim registry and its exclusion pass, and the REAL
--   registered @till.*@ / @plant.*@ verbs reading a real 'WorldState''s
--   designation maps through 'registerLuaAPI'. Nothing hands the AI a
--   pre-filtered candidate — a fixture that did could not tell the fix
--   from the bug, since the bug is precisely that the engine query
--   knows nothing of claims.
--
--   Stubbed: @unit.*@ (there is no unit manager behind a synthetic
--   page), @engine.gameTime@ (claim expiry must be driven, not waited
--   on), and @scripts/movement_speed@ (it contributes one number to the
--   @unit.moveTo@ stub, while its real body drags in the whole
--   injury/salt/exhaustion chain no claim registry touches). Everything
--   the selection itself consults is real.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "farm designation claim selection"'@.
module Test.Headless.Lua.FarmDesignationClaim (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import World.Chunk.Types (chunkSize)
import World.Flora.Types (FloraId(..))
import World.Generate.Coordinates (canonicalTile)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Plant.Types (newPlantDesignation)
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Till.Types (newTillDesignation)

-- * Geometry
--
--   Two pages' worth of geometry, both installed under the same page id
--   one example at a time.
--
--   The PLAIN cases run on a non-wrapping (size 0) page, where every
--   seam helper is the identity — the issue's own repro shape, with the
--   worker on the tile corner at (0.5, 0.5) and its work in a row to
--   the east.
--
--   The SEAM cases run at world size 64 with the geometry
--   "Test.Headless.World.DesignationSeam" pins: 'seamNear' is four
--   tiles from 'seamOrigin' but is STORED a whole world away, so any
--   selection comparing raw coordinates ranks the genuinely distant
--   'seamFar' ahead of it.

zSlice ∷ Int
zSlice = 10

fixturePage ∷ WorldPageId
fixturePage = WorldPageId pageText

pageText ∷ Text
pageText = "farm_claim_probe"

-- | The page a foreign claim is recorded under. EXACTLY as long as
--   'pageText' on purpose: the claim key is @<page>:<x>,<y>@, so a
--   shorter or longer id would misalign the coordinate tail and be
--   dropped for the wrong reason, leaving the page test itself unproven.
--   Same length means only the page comparison can separate the two.
foreignPageText ∷ Text
foreignPageText = "farm_claim_other"

probeCrop ∷ FloraId
probeCrop = FloraId 1

-- | The worker's standing position for every plain case, and the row of
--   designations east of it. Distances from it are 0.71, 1.58, 2.55 and
--   29.50 tiles — strictly distinct, and the last one alone outside
--   'scanRange'.
plainOrigin ∷ (Double, Double)
plainOrigin = (0.5, 0.5)

near1, near2, near3, outOfRange ∷ (Int, Int)
near1      = (1, 0)
near2      = (2, 0)
near3      = (3, 0)
outOfRange = (30, 0)

-- | The scan range every example runs with, as @PARAMS@ carries it.
scanRange ∷ Double
scanRange = 20

-- | Two designations at EXACTLY equal distance from @(0, 0)@, one along
--   each axis. Hash order is not an ordering, so the verb's own
--   tie-break — ascending canonical @(x, y)@ — is what must settle it,
--   and 'tieWinner' is the tile that rule names.
tieWinner, tieLoser ∷ (Int, Int)
tieWinner = (0, 4)
tieLoser  = (4, 0)

seamWorldSize ∷ Int
seamWorldSize = 64

-- | The worker's tile, two tiles short of the seam.
seamOrigin ∷ (Int, Int)
seamOrigin = (16 * chunkSize + 14, seamRowY)

seamRowY ∷ Int
seamRowY = (-15) * chunkSize + 8

-- | Four tiles east of 'seamOrigin', across the seam — and therefore
--   stored under a canonical key a whole world away.
seamNear ∷ (Int, Int)
seamNear = canonicalTile seamWorldSize (17 * chunkSize + 2) seamRowY

-- | The SAME physical tile as 'seamNear' in the worker's own raw frame:
--   the alias a caller not reading stored data can hold. A claim
--   recorded under this spelling must exclude 'seamNear'.
seamNearAlias ∷ (Int, Int)
seamNearAlias = (17 * chunkSize + 2, seamRowY)

-- | The inner chunk's corner: sqrt 260 ≈ 16.12 tiles from 'seamOrigin',
--   genuinely farther than 'seamNear' yet nearer in raw arithmetic.
seamFar ∷ (Int, Int)
seamFar = (16 * chunkSize, (-15) * chunkSize)

-- * The spec

spec ∷ Spec
spec = beforeAll setup $
    describe "farm designation claim selection" $ do
        mapM_ actionSpec [tillAction, plantAction]
        sharedSpec
  where
    setup = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        _ ← executeDebugLua (lbsLuaState ls) prelude
        pure Fixture { fxEnv = env, fxLua = ls }

-- | Everything an example needs: one headless engine and one Lua state
--   carrying the production farming modules, reused across examples.
--   Each example installs its own synthetic page and resets the Lua
--   side, so nothing survives from the previous one.
data Fixture = Fixture
    { fxEnv ∷ EngineEnv
    , fxLua ∷ LuaBackendState
    }

-- | The two farming actions differ only in which namespace they call,
--   which designation map that namespace reads, and how their tunables
--   are spelled. Every example below is written once against this.
data Action = Action
    { acName    ∷ Text                                    -- ^ "till" / "plant"
    , acInstall ∷ WorldState → [(Int, Int)] → IO ()       -- ^ designate these
    }

tillAction, plantAction ∷ Action
tillAction = Action
    { acName = "till"
    , acInstall = \ws tiles → writeIORef (wsTillDesignationsRef ws) $
        HM.fromList [ (t, newTillDesignation zSlice) | t ← tiles ]
    }
plantAction = Action
    { acName = "plant"
    , acInstall = \ws tiles → writeIORef (wsPlantDesignationsRef ws) $
        HM.fromList [ (t, newPlantDesignation zSlice probeCrop) | t ← tiles ]
    }

actionSpec ∷ Action → SpecWith Fixture
actionSpec act = describe (T.unpack (acName act)) $ do

  it "gives two clustered workers two different tiles while the first \
     \claim is fresh" $ \fx → do
    plainPage fx act [near1, near2]
    -- The issue's exact repro: both workers stand on one tile, both
    -- score, and the closer designation goes to whoever asks first.
    runAt fx act 1 plainOrigin `shouldReturn` tile near1
    runAt fx act 2 plainOrigin `shouldReturn` tile near2

  it "skips SEVERAL claimed nearer tiles for the nearest one left" $
      \fx → do
    plainPage fx act [near1, near2, near3]
    runAt fx act 1 plainOrigin `shouldReturn` tile near1
    runAt fx act 2 plainOrigin `shouldReturn` tile near2
    runAt fx act 3 plainOrigin `shouldReturn` tile near3

  it "selects nothing once every in-range designation is claimed" $
      \fx → do
    plainPage fx act [near1, near2]
    runAt fx act 1 plainOrigin `shouldReturn` tile near1
    runAt fx act 2 plainOrigin `shouldReturn` tile near2
    runAt fx act 3 plainOrigin `shouldReturn` "none"

  it "does not reach past the scan range for the only free designation" $
      \fx → do
    plainPage fx act [near1, outOfRange]
    runAt fx act 1 plainOrigin `shouldReturn` tile near1
    -- The range gate is preserved, not widened: skipping the claimed
    -- nearer tile must not promote a designation the worker was never
    -- allowed to walk to.
    runAt fx act 2 plainOrigin `shouldReturn` "none"

  it "frees the nearest tile again when its claim times out" $ \fx → do
    plainPage fx act [near1, near2]
    runAt fx act 1 plainOrigin `shouldReturn` tile near1
    advance fx (claimTimeout + 1)
    runAt fx act 3 plainOrigin `shouldReturn` tile near1

  it "frees the nearest tile again when its claimant disappears" $
      \fx → do
    plainPage fx act [near1, near2]
    runAt fx act 1 plainOrigin `shouldReturn` tile near1
    kill fx 1
    runAt fx act 3 plainOrigin `shouldReturn` tile near1

  it "leaves a claim taken between scoring and execution alone, and \
     \finds another tile next time" $ \fx → do
    plainPage fx act [near1, near2]
    -- Worker 2 scores first and stashes near1 as its candidate, THEN
    -- worker 1 takes it. Execute must find the fresh claim and decline
    -- rather than overwrite it.
    _ ← scoreAt fx act 2 plainOrigin
    runAt fx act 1 plainOrigin `shouldReturn` tile near1
    execOnly fx act 2 `shouldReturn` "none"
    claimant fx act near1 `shouldReturn` "1"
    -- And the loser is not stuck: its next decision finds free work.
    runAt fx act 2 plainOrigin `shouldReturn` tile near2

  it "ignores a claim recorded for the same coordinates on another \
     \page" $ \fx → do
    T.length foreignPageText `shouldBe` T.length pageText
    plainPage fx act [near1, near2]
    foreignClaim fx act near1
    runAt fx act 1 plainOrigin `shouldReturn` tile near1

  it "breaks an exact distance tie on canonical coordinate order" $
      \fx → do
    plainPage fx act [tieWinner, tieLoser]
    runAt fx act 1 (0, 0) `shouldReturn` tile tieWinner

  it "scores the tile it actually selected, not the one it skipped" $
      \fx → do
    -- Worker 2's utility with near1 claimed must be the utility a lone
    -- worker scores when near2 is the only designation there is —
    -- reporting the rejected tile's distance would leave it unchanged
    -- from worker 1's instead.
    plainPage fx act [near1, near2]
    uNear ← scoreAt fx act 1 plainOrigin
    _     ← runAt fx act 1 plainOrigin
    uSkip ← scoreAt fx act 2 plainOrigin
    plainPage fx act [near2]
    uAlone ← scoreAt fx act 3 plainOrigin
    uSkip `shouldBe` uAlone
    (uSkip < uNear) `shouldBe` True

  it "selects across the seam by physical distance, and excludes the \
     \claimed tile through its canonical identity" $ \fx → do
    seamPage fx act [seamNear, seamFar]
    -- seamNear is four tiles away and seamFar sixteen; only a
    -- seam-aware compare ranks them that way.
    runAt fx act 1 (tileCentre seamOrigin) `shouldReturn` tile seamNear
    runAt fx act 2 (tileCentre seamOrigin) `shouldReturn` tile seamFar
    -- Now the same claim spelled as the worker's own raw alias: it must
    -- still name the one physical designation, or two workers could
    -- hold one tile under two keys.
    seamPage fx act [seamNear, seamFar]
    aliasClaim fx act seamNearAlias
    runAt fx act 1 (tileCentre seamOrigin) `shouldReturn` tile seamFar

-- | Cases that are about the query itself rather than either action's
--   wiring, so they are stated once.
sharedSpec ∷ SpecWith Fixture
sharedSpec = describe "the engine query" $ do

  it "excludes nothing when handed no exclusion argument" $ \fx → do
    plainPage fx tillAction [near1, near2]
    evalLua fx (T.concat
        [ "local x, y = till.nearestFreeDesignation('", pageText
        , "', 0.5, 0.5); return x .. ',' .. y" ])
      `shouldReturn` tile near1

  it "honours its own distance bound independently of the caller" $
      \fx → do
    plainPage fx tillAction [outOfRange]
    evalLua fx (T.concat
        [ "local x = till.nearestFreeDesignation('", pageText
        , "', 0.5, 0.5, 20); return tostring(x)" ])
      `shouldReturn` "nil"
    evalLua fx (T.concat
        [ "local x, y = till.nearestFreeDesignation('", pageText
        , "', 0.5, 0.5, 40); return x .. ',' .. y" ])
      `shouldReturn` tile outOfRange

  it "answers nothing at all for a page it cannot resolve" $ \fx → do
    plainPage fx tillAction [near1]
    evalLua fx (T.concat
        [ "local x = till.nearestFreeDesignation('no_such_page'"
        , ", 0.5, 0.5, 20); return tostring(x)" ])
      `shouldReturn` "nil"

-- * Driving the production AI

-- | @<ns>.utility@ for @uid@ standing at @(x, y)@, as a number.
scoreAt ∷ Fixture → Action → Int → (Double, Double) → IO Double
scoreAt fx act uid (x, y) = do
    out ← evalLua fx $ T.concat
        [ "return string.format('%.6f', score('", acName act, "', "
        , tshow uid, ", ", tshow x, ", ", tshow y, "))" ]
    pure (read (T.unpack out))

-- | One whole decision — score, then execute if the score admits the
--   action — reporting the tile the worker ended up holding.
runAt ∷ Fixture → Action → Int → (Double, Double) → IO Text
runAt fx act uid (x, y) = evalLua fx $ T.concat
    [ "return run('", acName act, "', ", tshow uid, ", "
    , tshow x, ", ", tshow y, ")" ]

-- | Execute WITHOUT scoring again, so the candidate a previous
--   'scoreAt' stashed is the one execute tries to claim.
execOnly ∷ Fixture → Action → Int → IO Text
execOnly fx act uid = evalLua fx $ T.concat
    [ "return exec('", acName act, "', ", tshow uid, ")" ]

-- | Who holds the claim on this page's tile, as a uid string.
claimant ∷ Fixture → Action → (Int, Int) → IO Text
claimant fx act (gx, gy) = evalLua fx $ T.concat
    [ "return claimant('", acName act, "', ", tshow gx, ", ", tshow gy, ")" ]

-- | Record a live claim on the same COORDINATES under a different page
--   id — the shape #1329's page-qualified key exists to keep separate.
foreignClaim ∷ Fixture → Action → (Int, Int) → IO ()
foreignClaim fx act (gx, gy) = void $ evalLua fx $ T.concat
    [ "return foreignClaim('", acName act, "', ", tshow gx, ", "
    , tshow gy, ")" ]

-- | Record a live claim on THIS page under the given (possibly
--   non-canonical) spelling of the tile.
aliasClaim ∷ Fixture → Action → (Int, Int) → IO ()
aliasClaim fx act (gx, gy) = void $ evalLua fx $ T.concat
    [ "return aliasClaim('", acName act, "', ", tshow gx, ", "
    , tshow gy, ")" ]

advance ∷ Fixture → Double → IO ()
advance fx dt = void $ evalLua fx $
    T.concat [ "NOW = NOW + ", tshow dt, "; return 'ok'" ]

kill ∷ Fixture → Int → IO ()
kill fx uid = void $ evalLua fx $
    T.concat [ "LIVE[", tshow uid, "] = false; return 'ok'" ]

-- * Fixture plumbing

-- | The claim timeout both namespaces run with here, matching @PARAMS@.
claimTimeout ∷ Double
claimTimeout = 30

-- | Install a NON-WRAPPING page carrying exactly these designations,
--   and reset every piece of per-example Lua state.
plainPage ∷ Fixture → Action → [(Int, Int)] → IO ()
plainPage fx = installPage fx 0

-- | Install the wrapping seam page carrying exactly these designations.
seamPage ∷ Fixture → Action → [(Int, Int)] → IO ()
seamPage fx = installPage fx seamWorldSize

installPage ∷ Fixture → Int → Action → [(Int, Int)] → IO ()
installPage fx size act tiles = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = size })
    -- Both maps are installed every time: an example must never inherit
    -- the other action's leftovers, and the empty one proves the
    -- namespace under test is reading its own.
    acInstall tillAction ws (if acName act ≡ "till" then tiles else [])
    acInstall plantAction ws (if acName act ≡ "plant" then tiles else [])
    writeIORef (worldManagerRef (fxEnv fx)) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    void $ evalLua fx "return reset()"

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | One console command, with the JSON quoting the console applies to a
--   returned string stripped.
evalLua ∷ Fixture → Text → IO Text
evalLua fx src =
    T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState (fxLua fx)) src

tile ∷ (Int, Int) → Text
tile (gx, gy) = T.concat [tshow gx, ",", tshow gy]

tileCentre ∷ (Int, Int) → (Double, Double)
tileCentre (gx, gy) = (fromIntegral gx + 0.5, fromIntegral gy + 0.5)

-- | The one-time Lua side: stub only what has no synthetic-page answer,
--   load the PRODUCTION farming modules, and expose the four driver
--   helpers the examples call.
prelude ∷ Text
prelude = T.intercalate "\n"
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "NOW, POS, LIVE, STATE = 1000, {}, {}, {}"
    -- unit.* has no unit manager behind this page; everything else the
    -- selection touches is the registered engine verb.
    , "unit.getInfo = function(u) return POS[u] end"
    , "unit.exists = function(u) return LIVE[u] ~= false end"
    , "unit.getStat = function() return 1.0 end"
    , "unit.getSkill = function() return 50.0 end"
    , "unit.moveTo = function() end"
    , "unit.stop = function() end"
    , "unit.addXP = function() end"
    , "unit.setAnimOverride = function() end"
    , "unit.clearAnimOverride = function() end"
    -- Claim expiry must be driven by the example, not waited on.
    , "engine.gameTime = function() return NOW end"
    , "require('scripts.unit_ai_farm')"
    , "require('scripts.movement_speed').comfort = function() return 1.0 end"
    , "unitAi = package.loaded['scripts.unit_ai']"
    , "CLAIMS = require('scripts.unit_ai_claims')"
    -- Generated from the Haskell constants, so the range an example
    -- reasons about and the range the AI reads cannot drift apart.
    , T.concat [ "PARAMS = { till_scan_range = ", tshow scanRange
               , ", till_claim_timeout = ", tshow claimTimeout
               , ", plant_scan_range = ", tshow scanRange
               , ", plant_claim_timeout = ", tshow claimTimeout, "," ]
    , "  till_base_utility = 2.0, till_lock_utility = 6.0,"
    , "  till_rate = 0.5, till_equip_seconds = 1.0,"
    , "  till_equip_anim = 'e', till_work_anim = 'w',"
    , "  till_xp_per_till = 0.0,"
    , "  plant_base_utility = 2.0, plant_lock_utility = 6.0,"
    , "  plant_rate = 0.5, plant_equip_seconds = 1.0,"
    , "  plant_equip_anim = 'e', plant_work_anim = 'w',"
    , "  plant_xp_per_plant = 0.0 }"
    -- Fresh per-worker AI state and a fresh claim registry per example;
    -- resetAll is #1329's own in-place reset, so the public
    -- unitAi.<ns>.claims tables keep their identity.
    , "function reset()"
    , "  NOW, POS, LIVE, STATE = 1000, {}, {}, {}"
    , "  CLAIMS.resetAll()"
    , "  return 'ok'"
    , "end"
    , "local function st(u)"
    , "  STATE[u] = STATE[u] or {}"
    , "  return STATE[u]"
    , "end"
    , "function score(ns, u, x, y)"
    , "  POS[u] = { gridX = x, gridY = y }"
    , "  LIVE[u] = LIVE[u] ~= false"
    , "  return unitAi[ns].utility(u, st(u), PARAMS)"
    , "end"
    -- The tile this worker now holds, exactly as scripts/unit_ai.lua
    -- would leave it: execute only runs when the score admits it.
    , "local function held(ns, u)"
    , "  local job = st(u)[ns .. 'Job']"
    , "  return job and (job.x .. ',' .. job.y) or 'none'"
    , "end"
    , "function exec(ns, u)"
    , "  unitAi[ns].execute(u, st(u), PARAMS)"
    , "  return held(ns, u)"
    , "end"
    , "function run(ns, u, x, y)"
    , "  if score(ns, u, x, y) <= -math.huge then return 'none' end"
    , "  return exec(ns, u)"
    , "end"
    , "function claimant(ns, x, y)"
    , "  local wid = world.getActiveWorldId()"
    , "  local c = unitAi[ns].claims[CLAIMS.key(wid, x, y)]"
    , "  return c and tostring(c.uid) or 'none'"
    , "end"
    , "function foreignClaim(ns, x, y)"
    , "  LIVE[99] = true"
    , T.concat [ "  unitAi[ns].claims[CLAIMS.key('", foreignPageText
               , "', x, y)] = { uid = 99, at = NOW }" ]
    , "  return 'ok'"
    , "end"
    , "function aliasClaim(ns, x, y)"
    , "  LIVE[99] = true"
    , "  local wid = world.getActiveWorldId()"
    , "  unitAi[ns].claims[CLAIMS.key(wid, x, y)] = { uid = 99, at = NOW }"
    , "  return 'ok'"
    , "end"
    , "return 'ok'"
    ]
