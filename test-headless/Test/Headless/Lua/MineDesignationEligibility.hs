{-# LANGUAGE Strict #-}
-- | Mine designation eligibility selection (issue #2538).
--
--   Fresh mining-job selection used to score the UNCONDITIONAL nearest
--   designation and then return @-math.huge@ for the whole action the
--   moment that one tile failed any of its gates — another worker's
--   claim, an unloaded chunk, nowhere for the spoil to go, or no
--   carried tool that cuts the material. One unusable nearest tile
--   therefore hid every workable designation behind it, and a miner
--   standing beside a boxed-in tile reported no mining work at all. The
--   fix is @world.nearestWorkableMineDesignation@, which walks
--   candidates in ascending seam-aware distance and answers the first
--   that survives the whole rejection set.
--
--   WHAT IS REAL HERE. Selection is the SHIPPING code end to end: the
--   production @scripts/unit_ai_dig.lua@ utility and execute, the
--   production claim registry and its exclusion pass, and the REAL
--   registered @world.*@ verbs — the new selector, @world.getDigInfoAt@,
--   @world.getMineDesignationAt@, @world.getSurfaceAt@ and
--   @world.getFluidAt@ — reading a real 'WorldState'. The designations
--   are made by the REAL @world.designateMine@ command handler over a
--   real chunk store, and blocked spoil is real page state: piles of a
--   conflicting material at the very vertices
--   'World.Spoil.Logic.spoilStartVertex' and
--   'World.Spoil.Types.candidateVertices' name. Every decisive case
--   asserts the engine's own @spoilBlocked@ before and after, so the
--   rejection is an engine answer rather than a hard-coded Boolean.
--
--   Nothing hands the AI a pre-filtered candidate: a fixture that did
--   could not tell the fix from the bug, the bug being precisely that
--   the engine query knew nothing of claims or workability.
--
--   Stubbed: @unit.*@ (there is no unit manager behind a synthetic
--   page), @engine.gameTime@ (claim expiry must be driven, not waited
--   on), and @scripts/movement_speed@ (it contributes one number to the
--   @unit.moveTo@ stub while its real body drags in the whole
--   injury\/salt\/exhaustion chain no claim registry touches).
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "mine designation eligibility selection"'@.
module Test.Headless.Lua.MineDesignationEligibility (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Generate.Coordinates
    (canonicalTile, canonicalTileFrame, chunkToGlobal)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Fluid.Types (emptyIceMap)
import World.Material
    (MaterialId(..), MaterialProps(..), MaterialRegistry
    , defaultMaterialProps, emptyMaterialRegistry, registerMaterial)
import World.Page.Types (WorldPageId(..))
import World.Spoil.Logic (spoilStartVertex)
import World.Spoil.Types (SpoilPile(..), SpoilPiles, candidateVertices)
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import Structure.Types (emptyChunkStructures)
import World.Thread.Command.Cursor.Mine (handleWorldDesignateMineCommand)
import World.Tile.Types (WorldTileData(..))

-- * Materials
--
--   Four registered materials separate the four things a dig-info
--   answer can say. Speeds are deliberately asymmetric so a score can
--   name which tile it described, and 'tie' exists only to pin the
--   shovel-wins-an-exact-tie half of the tool rule.

granite, loam, adamant, rubble, clinker, tie ∷ Word8
granite = 1   -- pick only
loam    = 2   -- shovel only
adamant = 3   -- neither: designated, loaded, unblocked, unworkable
rubble  = 4   -- what digging granite and loam produces
clinker = 5   -- a DIFFERENT spoil material, the blocking lever
tie     = 6   -- pick and shovel exactly equal

-- | Speeds the score arithmetic is written against.
graniteSpeed, loamSpeed ∷ Double
graniteSpeed = 1.0
loamSpeed    = 0.5

fixtureRegistry ∷ MaterialRegistry
fixtureRegistry = foldl' (\r (i, p) → registerMaterial i p r)
                         emptyMaterialRegistry
    [ (granite, spoiling "granite" (realToFrac graniteSpeed) 0)
    , (loam,    spoiling "loam"    0 (realToFrac loamSpeed))
      -- No dig_spoil at all, so it can never be spoil-blocked: the ONLY
      -- thing that rejects this tile is that neither tool cuts it.
    , (adamant, defaultMaterialProps { mpName = "adamant"
                                     , mpPickSpeed = 0, mpShovelSpeed = 0 })
    , (rubble,  defaultMaterialProps { mpName = "rubble" })
    , (clinker, defaultMaterialProps { mpName = "clinker" })
    , (tie,     spoiling "tie" 0.75 0.75)
    ]
  where
    spoiling name pick shovel = defaultMaterialProps
        { mpName = name, mpPickSpeed = pick, mpShovelSpeed = shovel
        , mpDigSpoil = Just "rubble" }

-- * Geometry
--
--   The PLAIN cases run on a non-wrapping (size 0) page, where every
--   seam helper is the identity — the issue's own repro shape, with the
--   miner on the tile corner at (0.5, 0.5) and its work in a row to the
--   east. 'seamTileDist2' measures to the tile's integer coord, so
--   'near1' … 'near4' sit 0.71, 1.58, 2.55 and 3.54 tiles away.
--
--   'farChunk' and 'beyondChunk' are in chunks of their own (16.5 and
--   32.5 tiles out) so one of them can be genuinely UNLOADED without
--   taking the miner's own chunk with it.

plainOrigin ∷ (Double, Double)
plainOrigin = (0.5, 0.5)

near1, near2, near3, near4, farChunk, beyondChunk, outOfRange ∷ (Int, Int)
near1       = (1, 0)
near2       = (2, 0)
near3       = (3, 0)
near4       = (4, 0)
farChunk    = (17, 0)
beyondChunk = (33, 0)
outOfRange  = (60, 0)

-- | The chunk 'farChunk' lives in — removed from the tile store whole,
--   which is the real way dig information becomes unavailable.
farChunkCoord ∷ ChunkCoord
farChunkCoord = ChunkCoord 1 0

-- | The scan range every example runs with, as @PARAMS@ carries it.
--   Wider than the shipped 30 so 'beyondChunk' is still reachable.
scanRange ∷ Double
scanRange = 40

-- | The claim timeout every example runs with, matching @PARAMS@.
claimTimeout ∷ Double
claimTimeout = 30

-- | The inclusive range boundary, measured from 'rangeOrigin' so the
--   arithmetic is exact in 'Float': @atRange@ is EXACTLY 'scanRange'
--   away and must stay eligible (the live comparison is @dist >
--   range@), @pastRange@ is strictly beyond it.
rangeOrigin ∷ (Double, Double)
rangeOrigin = (0, 0)

atRange, pastRange ∷ (Int, Int)
atRange   = (40, 0)
pastRange = (41, 0)

-- | Two tiles at EXACTLY equal distance from 'rangeOrigin', one along
--   each axis. Hash order is not an ordering, so a tie-break must
--   settle it, and this query's is ascending canonical @(x, y)@ — the
--   tile-keyed identity 'nearestFreeDesignationOn' already uses. The
--   y-axis tile therefore wins.
tieWinner, tieLoser ∷ (Int, Int)
tieWinner = (0, 4)
tieLoser  = (4, 0)

zSlice ∷ Int
zSlice = 10

fixturePage ∷ WorldPageId
fixturePage = WorldPageId pageText

pageText ∷ Text
pageText = "mine_eligibility_probe"

-- | The page a foreign claim is recorded under. Dig claim keys are
--   @\<page\>:\<x\>,\<y\>@, so only the page comparison separates this
--   from a claim here — which is exactly what the case tests.
foreignPageText ∷ Text
foreignPageText = "mine_eligibility_other"

seamWorldSize ∷ Int
seamWorldSize = 64

-- | The miner's tile, two tiles short of the seam (the geometry
--   "Test.Headless.World.DesignationSeam" pins).
seamOrigin ∷ (Int, Int)
seamOrigin = (16 * chunkSize + 14, seamRowY)

seamRowY ∷ Int
seamRowY = (-15) * chunkSize + 8

-- | Four tiles east of 'seamOrigin', across the seam — and therefore
--   STORED under a canonical key a whole world away.
seamNear ∷ (Int, Int)
seamNear = canonicalTile seamWorldSize (17 * chunkSize + 2) seamRowY

-- | The inner chunk's corner: sqrt 260 ≈ 16.12 tiles from 'seamOrigin',
--   genuinely farther than 'seamNear' yet nearer in raw arithmetic.
seamFar ∷ (Int, Int)
seamFar = (16 * chunkSize, (-15) * chunkSize)

-- * The spec

spec ∷ Spec
spec = beforeAll setup $
    describe "mine designation eligibility selection" $ do
        selectionSpec
        recoverySpec
        registrySpec
        querySpec
  where
    setup = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        _  ← executeDebugLua (lbsLuaState ls) prelude
        pure Fixture { fxEnv = env, fxLua = ls }

-- | Everything an example needs: one headless engine and one Lua state
--   carrying the production dig module, reused across examples. Each
--   example installs its own synthetic page and resets the Lua side, so
--   nothing survives from the previous one.
data Fixture = Fixture
    { fxEnv ∷ EngineEnv
    , fxLua ∷ LuaBackendState
    }

selectionSpec ∷ SpecWith Fixture
selectionSpec = describe "fresh selection" $ do

  it "gives two clustered miners distinct tiles while the first \
     \miner's claim stays fresh" $ \fx → do
    -- Before #2538 the second miner scored -inf: the nearest
    -- designation was claimed, and that ended the action.
    _ ← plainPage fx (page [near1, near2])
    runAt fx 1 plainOrigin `shouldReturn` tile near1
    runAt fx 2 plainOrigin `shouldReturn` tile near2
    -- And the first claim really is still live, not quietly expired.
    claimant fx near1 `shouldReturn` "1"

  it "skips a nearest tile whose SPOIL DISPOSAL is blocked and claims \
     \the next workable designation" $ \fx → do
    -- The issue's second repro: no competing worker at all. The engine
    -- itself must say the near tile is blocked and the far one is not,
    -- or this case proves nothing about selection.
    _ ← plainPage fx (page [near1, near2]) { pgBlocked = [near1] }
    spoilBlockedAtTile fx near1 `shouldReturn` "true"
    spoilBlockedAtTile fx near2 `shouldReturn` "false"
    runAt fx 1 plainOrigin `shouldReturn` tile near2

  it "does not let SEVERAL nearer rejected tiles mask the nearest \
     \eligible one, whatever mixture rejected them" $ \fx → do
    -- One of each cause, in ascending distance: claimed, spoil-blocked,
    -- and made of something no carried tool cuts.
    _ ← plainPage fx (page [near1, near2, near3, near4])
        { pgBlocked   = [near2]
        , pgMaterials = [(near3, adamant)] }
    runAt fx 1 plainOrigin `shouldReturn` tile near1
    spoilBlockedAtTile fx near2 `shouldReturn` "true"
    runAt fx 2 plainOrigin `shouldReturn` tile near4

  it "skips a tile whose dig information is UNAVAILABLE for the next \
     \eligible one" $ \fx → do
    -- Genuinely unloaded, not a fixture flag: the designation is made
    -- while the chunk is resident and the chunk is then dropped, which
    -- is how an unloaded dig job actually arises.
    _ ← plainPage fx (page [farChunk, beyondChunk])
        { pgUnload = [farChunkCoord] }
    digInfoAtTile fx farChunk    `shouldReturn` "nil"
    digInfoAtTile fx beyondChunk `shouldReturn` "loaded"
    runAt fx 1 plainOrigin `shouldReturn` tile beyondChunk

  it "skips a tile no CARRIED tool can cut for the next eligible one" $
      \fx → do
    -- A pick-carrying miner and a loam tile: loaded, unblocked,
    -- unclaimed, and still not workable BY THIS WORKER.
    _ ← plainPage fx (page [near1, near2]) { pgMaterials = [(near1, loam)] }
    runAt fx 1 plainOrigin `shouldReturn` tile near2
    -- The same tile IS workable to a miner carrying a shovel, which is
    -- what makes this a tool rejection rather than a broken tile.
    carrying fx 2 ["shovel_steel"]
    runAt fx 2 plainOrigin `shouldReturn` tile near1

  it "selects nothing once every in-range candidate is rejected" $
      \fx → do
    _ ← plainPage fx (page [near1, near2]) { pgBlocked = [near2] }
    runAt fx 1 plainOrigin `shouldReturn` tile near1
    -- The verdict is about MINING, not about what the unit does
    -- instead: a -inf score can never win arbitration, so the assertion
    -- is the score and the unset candidate, nothing further.
    scoreAt fx 2 plainOrigin `shouldReturn` refusal
    candidate fx 2 `shouldReturn` "none"

  it "selects nothing when the miner carries no digging tool" $ \fx → do
    _ ← plainPage fx (page [near1, near2])
    carrying fx 1 []
    scoreAt fx 1 plainOrigin `shouldReturn` refusal
    candidate fx 1 `shouldReturn` "none"
    -- Nor does an empty-handed miner take a tile a colleague could
    -- have had: refusing must not consume anything.
    claimant fx near1 `shouldReturn` "none"
    runAt fx 2 plainOrigin `shouldReturn` tile near1

  it "selects nothing when only OUT-OF-RANGE candidates remain" $
      \fx → do
    _ ← plainPage fx (page [near1, outOfRange])
    runAt fx 1 plainOrigin `shouldReturn` tile near1
    -- The range gate is preserved, not widened: skipping the claimed
    -- nearer tile must not promote one the worker may not walk to.
    scoreAt fx 2 plainOrigin `shouldReturn` refusal
    candidate fx 2 `shouldReturn` "none"

  it "keeps the range boundary INCLUSIVE on both sides of the fix" $
      \fx → do
    _ ← plainPage fx (page [atRange])
    runAt fx 1 rangeOrigin `shouldReturn` tile atRange
    _ ← plainPage fx (page [pastRange])
    scoreAt fx 1 rangeOrigin `shouldReturn` refusal

  it "scores the tile it actually selected, not the one it skipped" $
      \fx → do
    -- Miner 2's utility with near1 claimed must equal what a lone miner
    -- scores when near2 is the only designation there is; reporting the
    -- REJECTED tile's distance would leave it at miner 1's figure.
    _ ← plainPage fx (page [near1, near2])
    uNear ← utilityAt fx 1 plainOrigin
    _     ← runAt fx 1 plainOrigin
    uSkip ← utilityAt fx 2 plainOrigin
    _ ← plainPage fx (page [near2])
    uAlone ← utilityAt fx 3 plainOrigin
    uSkip `shouldBe` uAlone
    (uSkip < uNear) `shouldBe` True

  it "scores the SELECTED tile's tool speed, not the skipped tile's" $
      \fx → do
    -- Same tile, same distance term either way: what separates these
    -- two numbers is only which material's speed reached the score. The
    -- miner carries both classes so neither material can be refused for
    -- want of a tool.
    _ ← plainPage fx (page [near2]) { pgMaterials = [(near2, loam)] }
    carrying fx 1 bothTools
    uLoam ← utilityAt fx 1 plainOrigin
    _ ← plainPage fx (page [near2])
    carrying fx 2 bothTools
    uGranite ← utilityAt fx 2 plainOrigin
    -- min(speed, 1.0) is the shipped clamp, so loam scores exactly half.
    (uLoam / uGranite) `shouldSatisfy` nearly (loamSpeed / graniteSpeed)
    -- And a miner that had to skip a blocked GRANITE tile to reach the
    -- loam one scores the loam figure, with the loam tile's own tool —
    -- reporting the skipped tile's speed would leave it at uGranite.
    _ ← plainPage fx (page [near1, near2])
        { pgBlocked = [near1], pgMaterials = [(near2, loam)] }
    carrying fx 3 bothTools
    uSkipped ← utilityAt fx 3 plainOrigin
    uSkipped `shouldSatisfy` nearly uLoam
    candidateTool fx 3 `shouldReturn` "shovel"

  it "selects across the seam by physical distance and reports the \
     \canonical tile" $ \fx → do
    -- seamNear is four tiles away and seamFar sixteen; only a
    -- seam-aware compare ranks them that way. The job records the
    -- STORED canonical tile rather than the miner's own raw alias.
    _ ← seamPage fx (page [seamNear, seamFar])
    runAt fx 1 (tileCentre seamOrigin) `shouldReturn` tile seamNear
    runAt fx 2 (tileCentre seamOrigin) `shouldReturn` tile seamFar

  it "lets a seam ALIAS claim exclude the tile it names" $ \fx → do
    -- One physical designation holds exactly one claim slot: the engine
    -- canonicalises an exclusion handed to it, so a claim filed under
    -- an alias of seamNear still hides seamNear itself.
    _ ← seamPage fx (page [seamNear, seamFar])
    aliasClaim fx (17 * chunkSize + 2, seamRowY)
    runAt fx 1 (tileCentre seamOrigin) `shouldReturn` tile seamFar

recoverySpec ∷ SpecWith Fixture
recoverySpec = describe "a rejected tile becoming eligible again" $ do

  it "frees a tile when its claim times out" $ \fx → do
    _ ← plainPage fx (page [near1, near2])
    runAt fx 1 plainOrigin `shouldReturn` tile near1
    advance fx (claimTimeout + 1)
    runAt fx 3 plainOrigin `shouldReturn` tile near1

  it "frees a tile when its claimant disappears" $ \fx → do
    _ ← plainPage fx (page [near1, near2])
    runAt fx 1 plainOrigin `shouldReturn` tile near1
    kill fx 1
    runAt fx 3 plainOrigin `shouldReturn` tile near1

  it "frees a tile when its spoil blockage CLEARS, the engine query \
     \flipping with it" $ \fx → do
    ws ← plainPage fx (page [near1, near2]) { pgBlocked = [near1] }
    spoilBlockedAtTile fx near1 `shouldReturn` "true"
    runAt fx 1 plainOrigin `shouldReturn` tile near2
    -- Clear the real piles; the same real query must now answer false.
    writeIORef (wsSpoilRef ws) HM.empty
    spoilBlockedAtTile fx near1 `shouldReturn` "false"
    runAt fx 3 plainOrigin `shouldReturn` tile near1

  it "frees a tile when the chunk carrying its dig information loads" $
      \fx → do
    ws ← plainPage fx (page [farChunk, beyondChunk])
        { pgUnload = [farChunkCoord] }
    runAt fx 1 plainOrigin `shouldReturn` tile beyondChunk
    reloadChunk ws farChunkCoord (pgMaterialAt (page []))
    digInfoAtTile fx farChunk `shouldReturn` "loaded"
    runAt fx 3 plainOrigin `shouldReturn` tile farChunk

registrySpec ∷ SpecWith Fixture
registrySpec = describe "the claim registry" $ do

  it "writes no claim while merely SCORING a tile" $ \fx → do
    -- A unit that loses action arbitration this tick must leave nothing
    -- behind: a scoring-time claim would mask the tile from everyone
    -- for a whole dig_claim_timeout.
    _ ← plainPage fx (page [near1, near2])
    _ ← utilityAt fx 1 plainOrigin
    claimant fx near1 `shouldReturn` "none"
    runAt fx 2 plainOrigin `shouldReturn` tile near1
    claimant fx near1 `shouldReturn` "2"

  it "leaves a claim taken between scoring and execution alone, and \
     \finds another tile next time" $ \fx → do
    _ ← plainPage fx (page [near1, near2])
    -- Miner 2 scores first and stashes near1 as its candidate, THEN
    -- miner 1 takes it. Execute must find the fresh claim and decline
    -- for this tick rather than overwrite it, and must not substitute
    -- another tile of its own accord either.
    _ ← utilityAt fx 2 plainOrigin
    runAt fx 1 plainOrigin `shouldReturn` tile near1
    execOnly fx 2 `shouldReturn` "none"
    claimant fx near1 `shouldReturn` "1"
    -- And the loser is not stuck: its next decision finds free work.
    runAt fx 2 plainOrigin `shouldReturn` tile near2

  it "clears a stale candidate when this tick rejects it" $ \fx → do
    -- Before #2538 every refusal left s.digCandidate untouched, which
    -- was inert only because a -inf score could not reach execute. Now
    -- that selection can accept a different tile, a candidate from an
    -- earlier tick must never be the tile execute takes.
    _ ← plainPage fx (page [near1])
    _ ← utilityAt fx 1 plainOrigin
    candidate fx 1 `shouldReturn` tile near1
    -- Miner 2 takes it; miner 1's NEXT evaluation has nothing left.
    runAt fx 2 plainOrigin `shouldReturn` tile near1
    scoreAt fx 1 plainOrigin `shouldReturn` refusal
    candidate fx 1 `shouldReturn` "none"
    execOnly fx 1 `shouldReturn` "none"
    claimant fx near1 `shouldReturn` "2"

  it "ignores a claim recorded for the same tile on another page" $
      \fx → do
    _ ← plainPage fx (page [near1, near2])
    foreignClaim fx near1
    runAt fx 1 plainOrigin `shouldReturn` tile near1

-- | Cases about the engine verb itself rather than the AI's wiring.
querySpec ∷ SpecWith Fixture
querySpec = describe "the engine query" $ do

  it "excludes nothing and restricts no tool when handed neither \
     \argument" $ \fx → do
    _ ← plainPage fx (page [near1, near2])
    workableQuery fx "0.5, 0.5, 40" `shouldReturn` answer near1 "pick"

  it "treats a non-table exclusion or toolset as no restriction" $
      \fx → do
    _ ← plainPage fx (page [near1])
    query fx "0.5, 0.5, 40, 'not a table', 'not a table'"
      `shouldReturn` answer near1 "pick"

  it "honours its own distance bound independently of the caller" $
      \fx → do
    _ ← plainPage fx (page [outOfRange])
    query fx "0.5, 0.5, 40" `shouldReturn` "nil"
    workableQuery fx "0.5, 0.5, 80" `shouldReturn` answer outOfRange "pick"

  it "admits nothing at all under a negative bound" $ \fx → do
    _ ← plainPage fx (page [near1])
    query fx "0.5, 0.5, -1" `shouldReturn` "nil"

  it "answers nil — not an empty table — for a page with no \
     \designations" $ \fx → do
    _ ← plainPage fx (page [])
    query fx "0.5, 0.5, 40" `shouldReturn` "nil"

  it "answers nothing at all for a page it cannot resolve" $ \fx → do
    _ ← plainPage fx (page [near1])
    evalLua fx "local x = world.nearestWorkableMineDesignation(\
               \'no_such_page', 0.5, 0.5, 40); return tostring(x)"
      `shouldReturn` "nil"

  it "answers nil when the toolset carries neither class" $ \fx → do
    _ ← plainPage fx (page [near1])
    query fx "0.5, 0.5, 40, {}, { pick = false, shovel = false }"
      `shouldReturn` "nil"

  it "breaks an exact distance tie on ascending canonical (x, y)" $
      \fx → do
    _ ← plainPage fx (page [tieWinner, tieLoser])
    workableQuery fx "0, 0, 40" `shouldReturn` answer tieWinner "pick"
    -- And the loser is still there to be taken, at the same distance.
    query fx (T.concat [ "0, 0, 40, { ", tile tieWinner, " }" ])
      `shouldReturn` answer tieLoser "pick"

  it "reports the tool and speed of the tile it chose" $ \fx → do
    _ ← plainPage fx (page [near1]) { pgMaterials = [(near1, loam)] }
    evalLua fx (T.concat
        [ "local _, _, _, t, s = world.nearestWorkableMineDesignation('"
        , pageText, "', 0.5, 0.5, 40)"
        , "; return t .. '@' .. string.format('%.2f', s)" ])
      `shouldReturn` "shovel@0.50"

  it "gives an exact speed tie to the SHOVEL, as bestDigTool did" $
      \fx → do
    -- The rule moved into the engine with #2538; it must not change on
    -- the way. bestDigTool set the shovel first and let a pick override
    -- only on a STRICT >, so an equal-speed material stays the shovel's.
    _ ← plainPage fx (page [near1]) { pgMaterials = [(near1, tie)] }
    workableQuery fx "0.5, 0.5, 40" `shouldReturn` answer near1 "shovel"
    -- Carrying only a pick still takes it, at the same speed.
    query fx "0.5, 0.5, 40, {}, { pick = true, shovel = false }"
      `shouldReturn` answer near1 "pick"

  it "never loads a chunk while ranking candidates" $ \fx → do
    -- Selection is a synchronous read of resident state: walking PAST
    -- an unloaded candidate must not queue the chunk it could not read,
    -- or ranking many candidates becomes a chunk-load amplifier.
    ws ← plainPage fx (page [farChunk, beyondChunk])
        { pgUnload = [farChunkCoord] }
    before ← chunkCount ws
    _ ← utilityAt fx 1 plainOrigin
    chunkCount ws `shouldReturn` before

-- * Driving the production AI

-- | @digUtility@ for @uid@ standing at @(x, y)@, as the fixture spells
--   it: 'refusal' for the @-math.huge@ that can never win arbitration,
--   and six decimal places otherwise. A refusal is reported as a WORD
--   rather than parsed as a number because @string.format@ renders it
--   @-inf@, which Haskell's 'read' rejects — and because the cases that
--   assert it are asserting a refusal, not a magnitude.
scoreAt ∷ Fixture → Int → (Double, Double) → IO Text
scoreAt fx uid (x, y) = evalLua fx $ T.concat
    [ "return scoreText(", tshow uid, ", ", tshow x, ", ", tshow y, ")" ]

-- | 'scoreAt' for a case that needs the magnitude. Fails loudly on a
--   refusal rather than folding it into a number.
utilityAt ∷ Fixture → Int → (Double, Double) → IO Double
utilityAt fx uid pos = do
    out ← scoreAt fx uid pos
    if out ≡ refusal
        then fail ("expected a finite dig utility, got " <> T.unpack out)
        else pure (read (T.unpack out))

-- | What 'scoreAt' answers when @digUtility@ returned @-math.huge@.
refusal ∷ Text
refusal = "refused"

-- | One whole decision — score, then execute if the score admits the
--   action — reporting the TILE the miner ended up holding.
runAt ∷ Fixture → Int → (Double, Double) → IO Text
runAt fx uid (x, y) = evalLua fx $ T.concat
    [ "return run(", tshow uid, ", ", tshow x, ", ", tshow y, ")" ]

-- | Execute WITHOUT scoring again, so the candidate a previous
--   'scoreAt' stashed is the one execute tries to claim.
execOnly ∷ Fixture → Int → IO Text
execOnly fx uid = evalLua fx $ T.concat [ "return exec(", tshow uid, ")" ]

-- | The tile a score stashed as this miner's candidate.
candidate ∷ Fixture → Int → IO Text
candidate fx uid =
    evalLua fx $ T.concat [ "return candidate(", tshow uid, ")" ]

-- | The tool that candidate named.
candidateTool ∷ Fixture → Int → IO Text
candidateTool fx uid =
    evalLua fx $ T.concat [ "return candidateTool(", tshow uid, ")" ]

-- | Who holds the claim on this tile, as a uid string.
claimant ∷ Fixture → (Int, Int) → IO Text
claimant fx (gx, gy) = evalLua fx $
    T.concat [ "return claimant(", tshow gx, ", ", tshow gy, ")" ]

-- | Record a live claim on the same tile under a different page id —
--   the shape #1329's page-qualified key exists to keep separate.
foreignClaim ∷ Fixture → (Int, Int) → IO ()
foreignClaim fx (gx, gy) = void $ evalLua fx $
    T.concat [ "return foreignClaim(", tshow gx, ", ", tshow gy, ")" ]

-- | Record a live claim under a raw ALIAS of a seam tile, which the
--   engine must canonicalise onto the key the designation is stored at.
aliasClaim ∷ Fixture → (Int, Int) → IO ()
aliasClaim fx (gx, gy) = void $ evalLua fx $
    T.concat [ "return tileClaim(", tshow gx, ", ", tshow gy, ")" ]

-- | Give this miner exactly these item def names. Every miner carries a
--   steel pick unless an example says otherwise.
carrying ∷ Fixture → Int → [Text] → IO ()
carrying fx uid defs = void $ evalLua fx $ T.concat
    [ "TOOLS[", tshow uid, "] = { "
    , T.intercalate ", " [ T.concat ["'", d, "'"] | d ← defs ]
    , " }; return 'ok'" ]

-- | A miner carrying one of each class, for a case where the material
--   rather than the toolset must decide.
bothTools ∷ [Text]
bothTools = ["pick_steel", "shovel_steel"]

-- | The engine's own @spoilBlocked@ for a tile, through the REAL
--   world.getDigInfoAt — what makes a blocked-spoil case an engine
--   answer rather than a fixture assertion about itself.
spoilBlockedAtTile ∷ Fixture → (Int, Int) → IO Text
spoilBlockedAtTile fx (gx, gy) = evalLua fx $ T.concat
    [ "local _, _, _, b = world.getDigInfoAt('", pageText, "', "
    , tshow gx, ", ", tshow gy, "); return tostring(b)" ]

-- | Whether world.getDigInfoAt can answer for a tile at all.
digInfoAtTile ∷ Fixture → (Int, Int) → IO Text
digInfoAtTile fx (gx, gy) = evalLua fx $ T.concat
    [ "local m = world.getDigInfoAt('", pageText, "', "
    , tshow gx, ", ", tshow gy, "); return m and 'loaded' or 'nil'" ]

-- | The engine verb straight, as @\<x\>,\<y\>#\<tool\>@.
workableQuery ∷ Fixture → Text → IO Text
workableQuery = query

query ∷ Fixture → Text → IO Text
query fx args = evalLua fx $ T.concat
    [ "local x, y, _, t = world.nearestWorkableMineDesignation('"
    , pageText, "', ", args, ")"
    , "; if not x then return 'nil' end"
    , "; return x .. ',' .. y .. '#' .. t" ]

answer ∷ (Int, Int) → Text → Text
answer t tool = T.concat [tile t, "#", tool]

advance ∷ Fixture → Double → IO ()
advance fx dt = void $ evalLua fx $
    T.concat [ "NOW = NOW + ", tshow dt, "; return 'ok'" ]

kill ∷ Fixture → Int → IO ()
kill fx uid = void $ evalLua fx $
    T.concat [ "LIVE[", tshow uid, "] = false; return 'ok'" ]

-- * Fixture plumbing

-- | What one example's page holds.
data PageSpec = PageSpec
    { pgDesignate ∷ [(Int, Int)]
      -- ^ Tiles handed to the REAL world.designateMine handler.
    , pgMaterials ∷ [((Int, Int), Word8)]
      -- ^ Per-tile material overrides; everything else is granite.
    , pgBlocked   ∷ [(Int, Int)]
      -- ^ Tiles whose spoil disposal is blocked, by real piles.
    , pgUnload    ∷ [ChunkCoord]
      -- ^ Chunks dropped AFTER designating, so a real designation ends
      --   up with no resident dig information.
    }

page ∷ [(Int, Int)] → PageSpec
page tiles = PageSpec
    { pgDesignate = tiles, pgMaterials = [], pgBlocked = [], pgUnload = [] }

pgMaterialAt ∷ PageSpec → (Int, Int) → Word8
pgMaterialAt spec t = maybe granite id (lookup t (pgMaterials spec))

-- | Install a NON-WRAPPING page and reset every piece of per-example
--   Lua state.
plainPage ∷ Fixture → PageSpec → IO WorldState
plainPage fx = installPage fx 0

-- | Install the wrapping seam page.
seamPage ∷ Fixture → PageSpec → IO WorldState
seamPage fx = installPage fx seamWorldSize

installPage ∷ Fixture → Int → PageSpec → IO WorldState
installPage fx worldSize spec = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws) WorldTileData
        { wtdChunks = HM.fromList
            [ (c, fixtureChunk worldSize c (pgMaterialAt spec))
            | c ← chunksFor worldSize (relevantTiles worldSize spec) ]
        , wtdMaxChunks = 400 }
    writeIORef (materialRegistryRef (fxEnv fx)) fixtureRegistry
    writeIORef (worldManagerRef (fxEnv fx)) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    -- The REAL designate command, one tile at a time, so each entry's z
    -- and corner state come from the same handler the mine tool drives.
    logger ← readIORef (loggerRef (fxEnv fx))
    forM_ (pgDesignate spec) $ \(gx, gy) →
        handleWorldDesignateMineCommand (fxEnv fx) logger fixturePage
            gx gy gx gy
    -- Blocking piles go in AFTER designating, at exactly the vertices
    -- the spoil router would search from this tile.
    writeIORef (wsSpoilRef ws) $
        HM.unions (blockingPiles <$> pgBlocked spec)
    forM_ (pgUnload spec) $ \c →
        modifyChunks ws (HM.delete c)
    void $ evalLua fx "return reset()"
    pure ws

-- | Piles of a DIFFERENT spoil material at every vertex the router
--   would consider for this tile. 'slotUsable' refuses a slot whose
--   vertex already holds another material with positive fill, so the
--   capacity around the tile is exactly zero and the real
--   'World.Spoil.Logic.spoilBlockedAt' answers true.
blockingPiles ∷ (Int, Int) → SpoilPiles
blockingPiles t@(gx, gy) = HM.fromList
    [ (v, SpoilPile (MaterialId clinker) (1, 1, 1, 1))
    | v ← candidateVertices (spoilStartVertex centre t) ]
  where centre = (fromIntegral gx + 0.5, fromIntegral gy + 0.5) ∷ (Float, Float)

-- | Put a dropped chunk back, so a case can watch an unloaded
--   designation become workable again.
reloadChunk ∷ WorldState → ChunkCoord → ((Int, Int) → Word8) → IO ()
reloadChunk ws c materialAt =
    modifyChunks ws (HM.insert c (fixtureChunk 0 c materialAt))

modifyChunks ∷ WorldState → (HM.HashMap ChunkCoord LoadedChunk
                             → HM.HashMap ChunkCoord LoadedChunk) → IO ()
modifyChunks ws f = do
    td ← readIORef (wsTilesRef ws)
    writeIORef (wsTilesRef ws) td { wtdChunks = f (wtdChunks td) }

chunkCount ∷ WorldState → IO Int
chunkCount ws = HM.size . wtdChunks <$> readIORef (wsTilesRef ws)

-- | Every tile an example's page must have terrain for: the designated
--   ones, both origins, and the whole neighbourhood the spoil search
--   (radius 4 vertices) and the corner approach read around them.
relevantTiles ∷ Int → PageSpec → [(Int, Int)]
relevantTiles _ spec = concat
    [ pgDesignate spec
    , fst <$> pgMaterials spec
    , pgBlocked spec
    , [ (0, 0), (40, 0), (60, 0), seamOrigin, seamFar, seamNear ]
    ]

-- | Resolve the tiles to the chunks that STORE them, expanded by a
--   margin wide enough for every neighbouring read.
chunksFor ∷ Int → [(Int, Int)] → [ChunkCoord]
chunksFor worldSize tiles = nub
    [ coord
    | (tx, ty) ← tiles
    , dx ← [-margin, 0, margin], dy ← [-margin, 0, margin]
    , let (coord, _, _) = canonicalTileFrame worldSize (tx + dx) (ty + dy) ]
  where margin = 8

-- | A chunk whose every column is solid to z 19, flat at 'zSlice', dry,
--   and made of whatever @materialAt@ says for that CANONICAL tile.
fixtureChunk ∷ Int → ChunkCoord → ((Int, Int) → Word8) → LoadedChunk
fixtureChunk worldSize coord materialAt = LoadedChunk
    { lcCoord             = coord
    , lcTiles             = V.generate area column
    , lcSurfaceMap        = VU.replicate area zSlice
    , lcTerrainSurfaceMap = VU.replicate area zSlice
    , lcFluidMap          = V.replicate area Nothing
    , lcIceMap            = emptyIceMap
    , lcFlora             = emptyFloraChunkData
    , lcSideDeco          = VU.empty
    , lcWaterTableMap     = VU.empty
    , lcMagma             = Nothing
    , lcStructures        = emptyChunkStructures
    }
  where
    area = chunkSize * chunkSize
    column i =
        let lx  = i `mod` chunkSize
            ly  = i `div` chunkSize
            gxy = uncurry (canonicalTile worldSize)
                      (chunkToGlobal coord lx ly)
        in ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.replicate 20 (materialAt gxy)
            , ctSlopes = VU.replicate 20 0
            , ctVeg    = VU.replicate 20 0
            }

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

-- | Float equality with the slack one Lua round trip through six
--   decimal places needs.
nearly ∷ Double → Double → Bool
nearly expected actual = abs (expected - actual) < 1.0e-5

-- | The one-time Lua side: stub only what has no synthetic-page answer,
--   load the PRODUCTION dig module, and expose the driver helpers the
--   examples call.
prelude ∷ Text
prelude = T.intercalate "\n"
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "NOW, POS, LIVE, STATE, TOOLS = 1000, {}, {}, {}, {}"
    -- unit.* has no manager behind this page; everything the SELECTION
    -- touches is a registered engine verb.
    , "unit.getInfo = function(u) return POS[u] end"
    , "unit.exists = function(u) return LIVE[u] ~= false end"
    , "unit.getStat = function() return 1.0 end"
    , "unit.getSkill = function() return 50.0 end"
    , "unit.getInventory = function(u)"
    , "  local out = {}"
    , "  for _, d in ipairs(TOOLS[u] or { 'pick_steel' }) do"
    , "    out[#out + 1] = { defName = d }"
    , "  end"
    , "  return out"
    , "end"
    , "unit.moveTo = function() end"
    , "unit.stop = function() end"
    , "unit.addXP = function() end"
    , "unit.setAnimOverride = function() end"
    , "unit.clearAnimOverride = function() end"
    -- Claim expiry must be driven by the example, not waited on.
    , "engine.gameTime = function() return NOW end"
    , "DIG = require('scripts.unit_ai_dig')"
    , "require('scripts.movement_speed').comfort = function() return 1.0 end"
    , "CLAIMS = require('scripts.unit_ai_claims')"
    -- Generated from the Haskell constants, so the numbers an example
    -- reasons about and the ones the AI reads cannot drift apart.
    , T.concat [ "PARAMS = { dig_scan_range = ", tshow scanRange
               , ", dig_claim_timeout = ", tshow claimTimeout, "," ]
    , "  dig_base_utility = 2.0, dig_lock_utility = 6.0,"
    , "  dig_rate = 0.5, dig_arrival_tiles = 0.4,"
    , "  dig_equip_seconds = 1.0, dig_xp_per_tile = 0.0,"
    , "  dig_tools = {"
    , "    shovel = { defs = { shovel_steel = true },"
    , "               equip_anim = 'e_shovel', work_anim = 'w_shovel' },"
    , "    pick   = { defs = { pick_steel = true },"
    , "               equip_anim = 'e_pick', work_anim = 'w_pick' } } }"
    -- Fresh per-miner AI state and a fresh claim registry per example;
    -- resetAll is #1329's own in-place reset, so DIG.claims keeps its
    -- identity.
    , "function reset()"
    , "  NOW, POS, LIVE, STATE, TOOLS = 1000, {}, {}, {}, {}"
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
    , "  return DIG.digUtility(u, st(u), PARAMS)"
    , "end"
    -- -math.huge formats as '-inf', which Haskell's read rejects; the
    -- cases that expect it are asserting a REFUSAL, so name it one.
    , "function scoreText(u, x, y)"
    , "  local v = score(u, x, y)"
    , T.concat [ "  if v <= -math.huge then return '", refusal, "' end" ]
    , "  return string.format('%.6f', v)"
    , "end"
    -- The TILE this miner now holds, exactly as scripts/unit_ai.lua
    -- would leave it: execute only runs when the score admits it.
    , "local function held(u)"
    , "  local job = st(u).digJob"
    , "  return job and (job.x .. ',' .. job.y) or 'none'"
    , "end"
    , "function exec(u)"
    , "  DIG.digExecute(u, st(u), PARAMS)"
    , "  return held(u)"
    , "end"
    , "function run(u, x, y)"
    , "  if score(u, x, y) <= -math.huge then return 'none' end"
    , "  return exec(u)"
    , "end"
    , "function candidate(u)"
    , "  local c = st(u).digCandidate"
    , "  return c and (c.x .. ',' .. c.y) or 'none'"
    , "end"
    , "function candidateTool(u)"
    , "  local c = st(u).digCandidate"
    , "  return c and tostring(c.tool) or 'none'"
    , "end"
    , "function claimant(x, y)"
    , "  local wid = world.getActiveWorldId()"
    , "  local c = DIG.claims[CLAIMS.key(wid, x, y)]"
    , "  return c and tostring(c.uid) or 'none'"
    , "end"
    , "function foreignClaim(x, y)"
    , "  LIVE[99] = true"
    , T.concat [ "  DIG.claims[CLAIMS.key('", foreignPageText
               , "', x, y)] = { uid = 99, at = NOW }" ]
    , "  return 'ok'"
    , "end"
    , "function tileClaim(x, y)"
    , "  LIVE[99] = true"
    , "  local wid = world.getActiveWorldId()"
    , "  DIG.claims[CLAIMS.key(wid, x, y)] = { uid = 99, at = NOW }"
    , "  return 'ok'"
    , "end"
    , "return 'ok'"
    ]
