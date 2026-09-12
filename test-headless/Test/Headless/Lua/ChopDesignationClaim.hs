{-# LANGUAGE Strict #-}
-- | Chop designation claim selection (issue #2536).
--
--   Fresh chopping-job selection used to score the UNCONDITIONAL
--   nearest designation and then return @-math.huge@ whenever another
--   worker held it. Because @chop.nearestDesignation@ breaks an exact
--   distance tie on the LOWEST instance id (#1854), two designated
--   trees sharing one tile always nominated the same one — so a
--   woodcutter beside a claimed tree reported no chopping work at all,
--   however many free designations stood on that tile or just beyond
--   it. The fix is the claim-aware engine query
--   @chop.nearestFreeDesignation@, driven from
--   @scripts/unit_ai_claims.lua@'s @nearestFreeInstance@.
--
--   WHAT IS REAL HERE. The selection is the SHIPPING code end to end:
--   the production @scripts/unit_ai_chop.lua@ utility and execute, the
--   production claim registry and its exclusion pass, and the REAL
--   registered @chop.*@ verbs reading a real 'WorldState''s designation
--   map through 'registerLuaAPI'. Nothing hands the AI a pre-filtered
--   candidate — a fixture that did could not tell the fix from the bug,
--   since the bug is precisely that the engine query knows nothing of
--   claims.
--
--   Stubbed: @unit.*@ and @item.listGround@ (there is no unit manager
--   or ground-item store behind a synthetic page), @engine.gameTime@
--   (claim expiry must be driven, not waited on),
--   @world.getFloraGrowthAt@ (regrowth is deliberately NOT part of the
--   selection predicate, and the one case that proves execute still
--   refuses a regrowing stump has to be able to say so), and
--   @scripts/movement_speed@ (it contributes one number to the
--   @unit.moveTo@ stub, while its real body drags in the whole
--   injury\/salt\/exhaustion chain no claim registry touches).
--   Everything the selection itself consults is real.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "chop designation claim selection"'@.
module Test.Headless.Lua.ChopDesignationClaim (spec) where

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
import World.Chop.Types (newChopDesignation)
import World.Chunk.Types (chunkSize)
import World.Flora.Identity
    (FloraInstanceId, floraInstanceIdToLua, plantedFloraInstanceId)
import World.Generate.Coordinates (canonicalTile)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)

-- * Trees
--
--   Ids are allocated through 'plantedFloraInstanceId' because it is
--   the only total constructor: 'FloraInstanceId' is exported
--   abstractly, and 'floraInstanceIdFromLua' refuses anything outside
--   the two namespaces. Its payload is @plantedBit ⌄ n@, so ASCENDING
--   @n@ is ascending id — which is what lets a case reason about the
--   instance-id tie-break by writing @treeA@ before @treeB@.

treeA, treeB, treeC ∷ FloraInstanceId
treeA = plantedFloraInstanceId 101
treeB = plantedFloraInstanceId 102
treeC = plantedFloraInstanceId 103

zSlice ∷ Int
zSlice = 10

fixturePage ∷ WorldPageId
fixturePage = WorldPageId pageText

pageText ∷ Text
pageText = "chop_claim_probe"

-- | The page a foreign claim is recorded under. Chop claim keys are
--   @\<page\>:#\<iid\>@, so — unlike the farming registries' coordinate
--   keys — no length coincidence is needed to keep the tail parseable;
--   only the page comparison can separate the two keys, which is
--   exactly what the case wants to test.
foreignPageText ∷ Text
foreignPageText = "chop_claim_other"

-- * Geometry
--
--   The PLAIN cases run on a non-wrapping (size 0) page, where every
--   seam helper is the identity — the issue's own repro shape, with the
--   worker on the tile corner at (0.5, 0.5) and its work in a row to
--   the east. Distances from there to 'near1', 'near2', 'near3' and
--   'outOfRange' are 0.71, 1.58, 2.55 and 29.50 tiles; only the last is
--   outside 'scanRange'.
--
--   The SEAM cases run at world size 64 with the geometry
--   "Test.Headless.World.DesignationSeam" pins: 'seamNear' is four
--   tiles from 'seamOrigin' but is STORED a whole world away, so any
--   selection comparing raw coordinates ranks the genuinely distant
--   'seamFar' ahead of it.

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

-- | The inclusive range boundary, measured from 'rangeOrigin' so the
--   arithmetic is exact in 'Float': @atRange@ is EXACTLY 'scanRange'
--   away and must stay eligible (the live comparison is @dist >
--   range@), @pastRange@ is strictly beyond it.
rangeOrigin ∷ (Double, Double)
rangeOrigin = (0, 0)

atRange, pastRange ∷ (Int, Int)
atRange   = (20, 0)
pastRange = (21, 0)

-- | Two trees at EXACTLY equal distance from 'rangeOrigin', one along
--   each axis. Hash order is not an ordering, so a tie-break must
--   settle it — and chop's is the LOWER INSTANCE ID (#1854), not the
--   canonical @(x, y)@ the tile-keyed farming query uses. The lower id
--   is deliberately parked on the coordinate-later tile, so a query
--   that quietly adopted the farming tie-break would pick 'tieLoser'.
tieWinnerTile, tieLoserTile ∷ (Int, Int)
tieWinnerTile = (4, 0)
tieLoserTile  = (0, 4)

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

-- | The inner chunk's corner: sqrt 260 ≈ 16.12 tiles from 'seamOrigin',
--   genuinely farther than 'seamNear' yet nearer in raw arithmetic.
seamFar ∷ (Int, Int)
seamFar = (16 * chunkSize, (-15) * chunkSize)

-- * The spec

spec ∷ Spec
spec = beforeAll setup $
    describe "chop designation claim selection" $ do
        selectionSpec
        registrySpec
        querySpec
  where
    setup = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        _ ← executeDebugLua (lbsLuaState ls) prelude
        pure Fixture { fxEnv = env, fxLua = ls }

-- | Everything an example needs: one headless engine and one Lua state
--   carrying the production chop module, reused across examples. Each
--   example installs its own synthetic page and resets the Lua side, so
--   nothing survives from the previous one.
data Fixture = Fixture
    { fxEnv ∷ EngineEnv
    , fxLua ∷ LuaBackendState
    }

selectionSpec ∷ SpecWith Fixture
selectionSpec = describe "fresh selection" $ do

  it "gives two clustered workers the two trees SHARING one tile" $
      \fx → do
    -- The issue's exact repro: two designated trees on one tile, both
    -- workers beside them. Before #2536 the tie-break handed both
    -- workers tree A and the second one scored -inf.
    plainPage fx [(treeA, near1), (treeB, near1)]
    runAt fx 1 plainOrigin `shouldReturn` iid treeA
    runAt fx 2 plainOrigin `shouldReturn` iid treeB

  it "leaves the next tree on a FARTHER tile selectable once the \
     \nearest is claimed" $ \fx → do
    plainPage fx [(treeA, near1), (treeB, near2)]
    runAt fx 1 plainOrigin `shouldReturn` iid treeA
    runAt fx 2 plainOrigin `shouldReturn` iid treeB

  it "skips SEVERAL claimed nearer trees for the nearest one left" $
      \fx → do
    -- Two co-tenants on the nearest tile and one farther: the farther
    -- tree must survive both nearer claims, not just one.
    plainPage fx [(treeA, near1), (treeB, near1), (treeC, near3)]
    runAt fx 1 plainOrigin `shouldReturn` iid treeA
    runAt fx 2 plainOrigin `shouldReturn` iid treeB
    runAt fx 3 plainOrigin `shouldReturn` iid treeC

  it "selects nothing once every in-range tree is claimed" $ \fx → do
    plainPage fx [(treeA, near1), (treeB, near2)]
    runAt fx 1 plainOrigin `shouldReturn` iid treeA
    runAt fx 2 plainOrigin `shouldReturn` iid treeB
    -- The verdict is about CHOPPING, not about what the unit does
    -- instead: a -inf score can never win arbitration, so the assertion
    -- is the score and the unset candidate, nothing further.
    scoreAt fx 3 plainOrigin `shouldReturn` refusal
    candidate fx 3 `shouldReturn` "none"

  it "does not reach past the scan range for the only free tree" $
      \fx → do
    plainPage fx [(treeA, near1), (treeB, outOfRange)]
    runAt fx 1 plainOrigin `shouldReturn` iid treeA
    -- The range gate is preserved, not widened: skipping the claimed
    -- nearer tree must not promote a tree the worker was never allowed
    -- to walk to.
    scoreAt fx 2 plainOrigin `shouldReturn` refusal
    candidate fx 2 `shouldReturn` "none"

  it "keeps the range boundary INCLUSIVE on both sides of the fix" $
      \fx → do
    plainPage fx [(treeA, atRange)]
    runAt fx 1 rangeOrigin `shouldReturn` iid treeA
    plainPage fx [(treeA, pastRange)]
    scoreAt fx 1 rangeOrigin `shouldReturn` refusal

  it "frees a tree again when its claim times out" $ \fx → do
    plainPage fx [(treeA, near1), (treeB, near2)]
    runAt fx 1 plainOrigin `shouldReturn` iid treeA
    advance fx (claimTimeout + 1)
    runAt fx 3 plainOrigin `shouldReturn` iid treeA

  it "frees a tree again when its claimant disappears" $ \fx → do
    plainPage fx [(treeA, near1), (treeB, near2)]
    runAt fx 1 plainOrigin `shouldReturn` iid treeA
    kill fx 1
    runAt fx 3 plainOrigin `shouldReturn` iid treeA

  it "breaks an exact distance tie on the lower INSTANCE ID" $ \fx → do
    plainPage fx [(treeA, tieWinnerTile), (treeB, tieLoserTile)]
    runAt fx 1 rangeOrigin `shouldReturn` iid treeA
    -- And the loser is still there to be taken, at the same distance.
    runAt fx 2 rangeOrigin `shouldReturn` iid treeB

  it "scores the tree it actually selected, not the one it skipped" $
      \fx → do
    -- Worker 2's utility with treeA claimed must be the utility a lone
    -- worker scores when treeB is the only designation there is —
    -- reporting the rejected tree's distance would leave it unchanged
    -- from worker 1's instead.
    plainPage fx [(treeA, near1), (treeB, near2)]
    uNear ← utilityAt fx 1 plainOrigin
    _     ← runAt fx 1 plainOrigin
    uSkip ← utilityAt fx 2 plainOrigin
    plainPage fx [(treeB, near2)]
    uAlone ← utilityAt fx 3 plainOrigin
    uSkip `shouldBe` uAlone
    (uSkip < uNear) `shouldBe` True

  it "does not treat regrowth as a selection filter, and still refuses \
     \a regrowing stump at execute" $ \fx → do
    -- Eligibility is claims and range ONLY. The regrowing tree is still
    -- the nearest thing selected; the existing execute-time guard is
    -- what declines it, exactly as before #2536.
    plainPage fx [(treeA, near1), (treeB, near2)]
    regrow fx near1 treeA 30
    _ ← utilityAt fx 1 plainOrigin
    candidate fx 1 `shouldReturn` candidateOf treeA near1
    execOnly fx 1 `shouldReturn` "none"
    claimant fx treeA `shouldReturn` "none"

  it "selects across the seam by physical distance and reports the \
     \canonical tile" $ \fx → do
    seamPage fx [(treeA, seamNear), (treeB, seamNear), (treeC, seamFar)]
    -- seamNear is four tiles away and seamFar sixteen; only a
    -- seam-aware compare ranks them that way. Its two co-tenants stay
    -- independently claimable across the seam, and the job records the
    -- STORED canonical tile rather than the worker's own raw alias.
    runAt fx 1 (tileCentre seamOrigin) `shouldReturn` iid treeA
    jobTile fx 1 `shouldReturn` tile seamNear
    runAt fx 2 (tileCentre seamOrigin) `shouldReturn` iid treeB
    jobTile fx 2 `shouldReturn` tile seamNear
    runAt fx 3 (tileCentre seamOrigin) `shouldReturn` iid treeC
    jobTile fx 3 `shouldReturn` tile seamFar

registrySpec ∷ SpecWith Fixture
registrySpec = describe "the claim registry" $ do

  it "writes no claim while merely SCORING a tree" $ \fx → do
    -- A unit that loses action arbitration this tick must leave nothing
    -- behind: a scoring-time claim would mask the tree from everyone
    -- for a whole chop_claim_timeout.
    plainPage fx [(treeA, near1), (treeB, near2)]
    _ ← utilityAt fx 1 plainOrigin
    claimant fx treeA `shouldReturn` "none"
    runAt fx 2 plainOrigin `shouldReturn` iid treeA
    claimant fx treeA `shouldReturn` "2"

  it "leaves a claim taken between scoring and execution alone, and \
     \finds another tree next time" $ \fx → do
    plainPage fx [(treeA, near1), (treeB, near2)]
    -- Worker 2 scores first and stashes treeA as its candidate, THEN
    -- worker 1 takes it. Execute must find the fresh claim and decline
    -- for this tick rather than overwrite it — and must not substitute
    -- another tree of its own accord either.
    _ ← utilityAt fx 2 plainOrigin
    runAt fx 1 plainOrigin `shouldReturn` iid treeA
    execOnly fx 2 `shouldReturn` "none"
    claimant fx treeA `shouldReturn` "1"
    -- And the loser is not stuck: its next decision finds free work.
    runAt fx 2 plainOrigin `shouldReturn` iid treeB

  it "ignores a claim recorded for the same tree on another page" $
      \fx → do
    plainPage fx [(treeA, near1), (treeB, near2)]
    foreignClaim fx treeA
    runAt fx 1 plainOrigin `shouldReturn` iid treeA

  it "ignores a COORDINATE-shaped key that names no instance" $ \fx → do
    -- claimedInstances' anchored "<wid>:#" test is the mirror of
    -- claimedTiles' "<x>,<y>" tail test: neither registry's key shape
    -- may leak into the other's exclusion array. The coordinate is
    -- chosen ADVERSARIALLY — its y is treeA's own id — so a tail match
    -- that forgot to anchor past the "#" would read a real, live
    -- instance out of it and wrongly exclude the tree.
    plainPage fx [(treeA, near1), (treeB, near2)]
    tileClaim fx (0, fromIntegral (floraInstanceIdToLua treeA))
    runAt fx 1 plainOrigin `shouldReturn` iid treeA

  it "still lets two restored jobs adopt distinct trees on one tile" $
      \fx → do
    -- #1854's restored-job adoption is untouched by the fix: a job
    -- saved with its tile but not its instance id re-resolves through
    -- chop.getDesignationsAt, claiming as it adopts.
    plainPage fx [(treeA, near1), (treeB, near1)]
    restoreJob fx 1 near1
    restoreJob fx 2 near1
    _ ← utilityAt fx 1 plainOrigin
    _ ← utilityAt fx 2 plainOrigin
    jobIid fx 1 `shouldReturn` iid treeA
    jobIid fx 2 `shouldReturn` iid treeB

-- | Cases about the engine verb itself rather than the AI's wiring.
querySpec ∷ SpecWith Fixture
querySpec = describe "the engine query" $ do

  it "excludes nothing when handed no exclusion argument" $ \fx → do
    plainPage fx [(treeA, near1), (treeB, near2)]
    freeQuery fx "0.5, 0.5" `shouldReturn` answer treeA near1

  it "excludes nothing when handed a non-table exclusion argument" $
      \fx → do
    plainPage fx [(treeA, near1)]
    evalLua fx (T.concat
        [ "local x, y, _, i = chop.nearestFreeDesignation('", pageText
        , "', 0.5, 0.5, 20, 'not a table')"
        , "; return x .. ',' .. y .. '#' .. i" ])
      `shouldReturn` answer treeA near1

  it "honours its own distance bound independently of the caller" $
      \fx → do
    plainPage fx [(treeA, outOfRange)]
    evalLua fx (T.concat
        [ "local x = chop.nearestFreeDesignation('", pageText
        , "', 0.5, 0.5, 20); return tostring(x)" ])
      `shouldReturn` "nil"
    freeQuery fx "0.5, 0.5, 40" `shouldReturn` answer treeA outOfRange

  it "admits nothing at all under a negative bound" $ \fx → do
    plainPage fx [(treeA, near1)]
    evalLua fx (T.concat
        [ "local x = chop.nearestFreeDesignation('", pageText
        , "', 0.5, 0.5, -1); return tostring(x)" ])
      `shouldReturn` "nil"

  it "answers nil — not an empty table — for a page with no \
     \designations" $ \fx → do
    plainPage fx []
    evalLua fx (T.concat
        [ "local x = chop.nearestFreeDesignation('", pageText
        , "', 0.5, 0.5, 20); return tostring(x)" ])
      `shouldReturn` "nil"

  it "answers nothing at all for a page it cannot resolve" $ \fx → do
    plainPage fx [(treeA, near1)]
    evalLua fx
        "local x = chop.nearestFreeDesignation('no_such_page'\
        \, 0.5, 0.5, 20); return tostring(x)"
      `shouldReturn` "nil"

  it "skips an excluded instance without skipping its co-tenant" $
      \fx → do
    plainPage fx [(treeA, near1), (treeB, near1)]
    evalLua fx (T.concat
        [ "local x, y, _, i = chop.nearestFreeDesignation('", pageText
        , "', 0.5, 0.5, 20, { ", iid treeA, " })"
        , "; return x .. ',' .. y .. '#' .. i" ])
      `shouldReturn` answer treeB near1

-- * Driving the production AI

-- | @chopUtility@ for @uid@ standing at @(x, y)@, as the fixture
--   spells it: 'refusal' for the @-math.huge@ that can never win
--   arbitration, and six decimal places otherwise. A refusal is
--   reported as a WORD rather than parsed as a number because
--   @string.format@ renders it @-inf@, which Haskell's 'read' rejects
--   — and because the cases that assert it are asserting a refusal, not
--   a magnitude.
scoreAt ∷ Fixture → Int → (Double, Double) → IO Text
scoreAt fx uid (x, y) = evalLua fx $ T.concat
    [ "return scoreText(", tshow uid, ", ", tshow x, ", ", tshow y, ")" ]

-- | 'scoreAt' for a case that needs the magnitude. Fails loudly on a
--   refusal rather than folding it into a number.
utilityAt ∷ Fixture → Int → (Double, Double) → IO Double
utilityAt fx uid pos = do
    out ← scoreAt fx uid pos
    if out ≡ refusal
        then fail ("expected a finite chop utility, got " <> T.unpack out)
        else pure (read (T.unpack out))

-- | What 'scoreAt' answers when @chopUtility@ returned @-math.huge@.
refusal ∷ Text
refusal = "refused"

-- | One whole decision — score, then execute if the score admits the
--   action — reporting the INSTANCE the worker ended up holding.
runAt ∷ Fixture → Int → (Double, Double) → IO Text
runAt fx uid (x, y) = evalLua fx $ T.concat
    [ "return run(", tshow uid, ", ", tshow x, ", ", tshow y, ")" ]

-- | Execute WITHOUT scoring again, so the candidate a previous
--   'scoreAt' stashed is the one execute tries to claim.
execOnly ∷ Fixture → Int → IO Text
execOnly fx uid =
    evalLua fx $ T.concat [ "return exec(", tshow uid, ")" ]

-- | The candidate a score stashed, as @\<iid\>\@\<x\>,\<y\>@.
candidate ∷ Fixture → Int → IO Text
candidate fx uid =
    evalLua fx $ T.concat [ "return candidate(", tshow uid, ")" ]

candidateOf ∷ FloraInstanceId → (Int, Int) → Text
candidateOf i t = T.concat [iid i, "@", tile t]

-- | The tile this worker's held job names.
jobTile ∷ Fixture → Int → IO Text
jobTile fx uid =
    evalLua fx $ T.concat [ "return jobTile(", tshow uid, ")" ]

-- | The instance this worker's held job resolved to.
jobIid ∷ Fixture → Int → IO Text
jobIid fx uid =
    evalLua fx $ T.concat [ "return jobIid(", tshow uid, ")" ]

-- | Who holds the claim on this tree, as a uid string.
claimant ∷ Fixture → FloraInstanceId → IO Text
claimant fx i =
    evalLua fx $ T.concat [ "return claimant(", iid i, ")" ]

-- | Record a live claim on the same TREE under a different page id —
--   the shape #1329's page-qualified key exists to keep separate.
foreignClaim ∷ Fixture → FloraInstanceId → IO ()
foreignClaim fx i = void $ evalLua fx $
    T.concat [ "return foreignClaim(", iid i, ")" ]

-- | Record a live claim under a COORDINATE key on this page — the shape
--   the other registries use, which must contribute no exclusion here.
tileClaim ∷ Fixture → (Int, Int) → IO ()
tileClaim fx (gx, gy) = void $ evalLua fx $
    T.concat [ "return tileClaim(", tshow gx, ", ", tshow gy, ")" ]

-- | Give @uid@ a job restored from a save: its tile, but no instance id.
restoreJob ∷ Fixture → Int → (Int, Int) → IO ()
restoreJob fx uid (gx, gy) = void $ evalLua fx $ T.concat
    [ "return restoreJob(", tshow uid, ", ", tshow gx, ", "
    , tshow gy, ")" ]

-- | Put this tree inside a post-fell regrowth window.
regrow ∷ Fixture → (Int, Int) → FloraInstanceId → Int → IO ()
regrow fx (gx, gy) i remaining = void $ evalLua fx $ T.concat
    [ "return regrow(", tshow gx, ", ", tshow gy, ", ", iid i, ", "
    , tshow remaining, ")" ]

-- | The engine verb straight, with no exclusion set.
freeQuery ∷ Fixture → Text → IO Text
freeQuery fx args = evalLua fx $ T.concat
    [ "local x, y, _, i = chop.nearestFreeDesignation('", pageText
    , "', ", args, "); return x .. ',' .. y .. '#' .. i" ]

advance ∷ Fixture → Double → IO ()
advance fx dt = void $ evalLua fx $
    T.concat [ "NOW = NOW + ", tshow dt, "; return 'ok'" ]

kill ∷ Fixture → Int → IO ()
kill fx uid = void $ evalLua fx $
    T.concat [ "LIVE[", tshow uid, "] = false; return 'ok'" ]

-- * Fixture plumbing

-- | The claim timeout every example runs with, matching @PARAMS@.
claimTimeout ∷ Double
claimTimeout = 30

-- | Install a NON-WRAPPING page carrying exactly these designations,
--   and reset every piece of per-example Lua state.
plainPage ∷ Fixture → [(FloraInstanceId, (Int, Int))] → IO ()
plainPage fx = installPage fx 0

-- | Install the wrapping seam page carrying exactly these designations.
seamPage ∷ Fixture → [(FloraInstanceId, (Int, Int))] → IO ()
seamPage fx = installPage fx seamWorldSize

installPage ∷ Fixture → Int → [(FloraInstanceId, (Int, Int))] → IO ()
installPage fx size trees = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = size })
    writeIORef (wsChopDesignationsRef ws) $ HM.fromList
        [ (i, newChopDesignation zSlice gx gy) | (i, (gx, gy)) ← trees ]
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

-- | A tree's id as Lua spells it.
iid ∷ FloraInstanceId → Text
iid = tshow . floraInstanceIdToLua

tile ∷ (Int, Int) → Text
tile (gx, gy) = T.concat [tshow gx, ",", tshow gy]

-- | The engine verb's four returns, as the query cases read them back.
answer ∷ FloraInstanceId → (Int, Int) → Text
answer i t = T.concat [tile t, "#", iid i]

tileCentre ∷ (Int, Int) → (Double, Double)
tileCentre (gx, gy) = (fromIntegral gx + 0.5, fromIntegral gy + 0.5)

-- | The one-time Lua side: stub only what has no synthetic-page answer,
--   load the PRODUCTION chop module, and expose the driver helpers the
--   examples call.
prelude ∷ Text
prelude = T.intercalate "\n"
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "NOW, POS, LIVE, STATE, GROWTH = 1000, {}, {}, {}, {}"
    -- unit.* and item.listGround have no manager behind this page;
    -- everything the SELECTION touches is the registered engine verb.
    , "unit.getInfo = function(u) return POS[u] end"
    , "unit.exists = function(u) return LIVE[u] ~= false end"
    , "unit.getStat = function() return 1.0 end"
    , "unit.getSkill = function() return 50.0 end"
    , "unit.getInventory = function() return {} end"
    , "unit.moveTo = function() end"
    , "unit.stop = function() end"
    , "unit.addXP = function() end"
    , "unit.setAnimOverride = function() end"
    , "unit.clearAnimOverride = function() end"
    , "item.listGround = function() return {} end"
    -- Claim expiry must be driven by the example, not waited on.
    , "engine.gameTime = function() return NOW end"
    -- Regrowth is EXECUTE's guard, not a selection filter; a synthetic
    -- page has no growth state, so the one case that needs a regrowing
    -- stump has to write one.
    , "world.getFloraGrowthAt = function(x, y)"
    , "  return GROWTH[x .. ',' .. y] end"
    , "CHOP = require('scripts.unit_ai_chop')"
    , "require('scripts.movement_speed').comfort = function() return 1.0 end"
    , "CLAIMS = require('scripts.unit_ai_claims')"
    -- Generated from the Haskell constants, so the range an example
    -- reasons about and the range the AI reads cannot drift apart.
    , T.concat [ "PARAMS = { chop_scan_range = ", tshow scanRange
               , ", chop_claim_timeout = ", tshow claimTimeout, "," ]
    , "  chop_base_utility = 2.0, chop_lock_utility = 6.0,"
    , "  chop_rate = 0.5, chop_bare_speed = 1.0, chop_tools = {},"
    , "  chop_equip_seconds = 1.0, chop_equip_anim = 'e',"
    , "  chop_work_anim = 'w', chop_stock_target = 10,"
    , "  chop_stock_floor = 0.1, chop_xp_per_fell = 0.0 }"
    -- Fresh per-worker AI state and a fresh claim registry per example;
    -- resetAll is #1329's own in-place reset, so CHOP.claims keeps its
    -- identity.
    , "function reset()"
    , "  NOW, POS, LIVE, STATE, GROWTH = 1000, {}, {}, {}, {}"
    , "  CLAIMS.resetAll()"
    , "  return 'ok'"
    , "end"
    , "local function st(u)"
    , "  STATE[u] = STATE[u] or {}"
    , "  return STATE[u]"
    , "end"
    , "function score(u, x, y)"
    , "  POS[u] = { gridX = x, gridY = y }"
    , "  LIVE[u] = LIVE[u] ~= false"
    , "  return CHOP.chopUtility(u, st(u), PARAMS)"
    , "end"
    -- -math.huge formats as '-inf', which Haskell's read rejects; the
    -- cases that expect it are asserting a REFUSAL, so name it one.
    , "function scoreText(u, x, y)"
    , "  local v = score(u, x, y)"
    , T.concat [ "  if v <= -math.huge then return '", refusal, "' end" ]
    , "  return string.format('%.6f', v)"
    , "end"
    -- The TREE this worker now holds, exactly as scripts/unit_ai.lua
    -- would leave it: execute only runs when the score admits it.
    , "local function held(u)"
    , "  local job = st(u).chopJob"
    , "  return job and job.iid and tostring(job.iid) or 'none'"
    , "end"
    , "function exec(u)"
    , "  CHOP.chopExecute(u, st(u), PARAMS)"
    , "  return held(u)"
    , "end"
    , "function run(u, x, y)"
    , "  if score(u, x, y) <= -math.huge then return 'none' end"
    , "  return exec(u)"
    , "end"
    , "function candidate(u)"
    , "  local c = st(u).chopCandidate"
    , "  return c and (c.iid .. '@' .. c.x .. ',' .. c.y) or 'none'"
    , "end"
    , "function jobTile(u)"
    , "  local job = st(u).chopJob"
    , "  return job and (job.x .. ',' .. job.y) or 'none'"
    , "end"
    , "function jobIid(u) return held(u) end"
    , "function claimant(i)"
    , "  local wid = world.getActiveWorldId()"
    , "  local c = CHOP.claims[CLAIMS.instanceKey(wid, i)]"
    , "  return c and tostring(c.uid) or 'none'"
    , "end"
    , "function foreignClaim(i)"
    , "  LIVE[99] = true"
    , T.concat [ "  CHOP.claims[CLAIMS.instanceKey('", foreignPageText
               , "', i)] = { uid = 99, at = NOW }" ]
    , "  return 'ok'"
    , "end"
    , "function tileClaim(x, y)"
    , "  LIVE[99] = true"
    , "  local wid = world.getActiveWorldId()"
    , "  CHOP.claims[CLAIMS.key(wid, x, y)] = { uid = 99, at = NOW }"
    , "  return 'ok'"
    , "end"
    , "function restoreJob(u, x, y)"
    , "  st(u).chopJob = { x = x, y = y }"
    , "  return 'ok'"
    , "end"
    , "function regrow(x, y, i, remaining)"
    , "  GROWTH[x .. ',' .. y] ="
    , "    { { instanceId = i, regrowthRemaining = remaining } }"
    , "  return 'ok'"
    , "end"
    , "return 'ok'"
    ]
