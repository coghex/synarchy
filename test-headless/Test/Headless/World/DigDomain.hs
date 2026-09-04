{-# LANGUAGE Strict #-}
-- | The numeric domain of a dig, at all three boundaries that decide it
--   (#2338).
--
--   @world.digTile@ used to narrow every argument with a bare @round@ or
--   @realToFrac@ and enqueue unconditionally. Nothing downstream raises
--   on the result — GHC's @floor@ answers 0 for a non-finite input — so
--   the damage was silent AND durable: a NaN amount makes
--   'World.Mine.Types.drainCorners' miss its own @amt ≤ 0@ stop and turn
--   every corner NaN, after which 'World.Mine.Types.cornersDone' can
--   never hold, no digger can ever finish the tile, and the poisoned
--   designation round-trips through the save verbatim.
--
--   Three groups, one per boundary, because each is reachable without
--   the others:
--
--     * the domains themselves, as pure predicates;
--     * @world.digTile@'s admission, driven through the REAL registered
--       closure in a bare interpreter, against a queue and a logger this
--       module owns — so \"nothing was queued\" is observed rather than
--       assumed;
--     * the world thread's own defensive check, driven by calling
--       'handleWorldDigTileCommand' DIRECTLY, which is the only way to
--       reach it now that the verb refuses first;
--     * and the load repair, driven through the real
--       'World.Load.Stage.stageSession' rather than a DTO conversion,
--       because staging is where the policy actually lives.
--
--   Every refusal example is paired with a control that DOES change the
--   thing it claims stays unchanged, so a passing refusal cannot be
--   passing because the fixture could never have moved.
module Test.Headless.World.DigDomain (spec, engineSpec) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua
import System.Random (StdGen, mkStdGen)

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Log
    ( LogBackend(..), LogConfig(..), LogEntry(..), LogLevel(..)
    , LoggerState(..), defaultLogConfig, initLogger )
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.World.Edit (worldDigTileFn)
import Engine.Graphics.Camera (CameraFacing(..))
import Item.Types (ItemDef(..), ItemManager(..))
import Item.Ground (GroundItems(..))
import Structure.Palette (emptyTexPalette)
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Unit.Command.Types (UnitCommand)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Material
    ( MaterialId(..), MaterialProps(..), MaterialRegistry
    , defaultMaterialProps, emptyMaterialRegistry, registerMaterial )
import World.Mine.Types (MineDesignation(..), cornersDone, drainCorners)
import World.Save.Component.PageCore (blankPageSnapshot)
import World.Save.Snapshot
    (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Spoil.Types (SpoilPile(..))
import World.Thread.Command.Edit.Dig (handleWorldDigTileCommand)
-- The world-side vocabulary this fixture needs — chunk, tile, page,
-- gen-param, edit and world-state types, plus 'WorldCommand' and the
-- dig domains stated beside it — all arrive through this one
-- re-export, which is how the production modules under test reach them
-- too.
import World.Types

-- * Geometry
--
--   A single non-wrapping chunk of flat, uniform stone. Nothing about
--   the seam frame is under test here, so worldSize 0 keeps every
--   canonicalisation the identity and the tile keys read as written.

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "dig_domain_probe"

-- | The surface z every column in the fixture chunk sits at.
zSlice ∷ Int
zSlice = 10

-- | The designated tile, comfortably inside the one chunk so every
--   spoil vertex around it is inside it too.
digTile ∷ (Int, Int)
digTile = (8, 8)

-- | A second designation the repair examples use as the untouched
--   control: whatever happens to a poisoned sibling must not happen to
--   this one.
healthyTile ∷ (Int, Int)
healthyTile = (12, 12)

-- | The digger's tile-space position for every example that supplies a
--   valid one.
diggerPos ∷ (Float, Float)
diggerPos = (7.5, 8.5)

-- * Materials
--
--   Two registered materials, so the dig has real spoil to route and a
--   real chunk item to yield. Both are what make the CONTROLS able to
--   move state at all — with the default registry a dig routes no spoil
--   and yields nothing, and every \"unchanged\" assertion below would
--   pass on a fixture that could never have changed.

stoneId, gravelId ∷ Word8
stoneId  = 1
gravelId = 2

chunkItemName ∷ Text
chunkItemName = "dig_domain_chunk"

digRegistry ∷ MaterialRegistry
digRegistry =
    registerMaterial gravelId
        defaultMaterialProps { mpName = "dig_domain_gravel" }
  $ registerMaterial stoneId
        defaultMaterialProps
            { mpName       = "dig_domain_stone"
            , mpDigSpoil   = Just "dig_domain_gravel"
            , mpDigBulking = 1.2
            , mpDigChunk   = Just chunkItemName
            , mpDigGems    = False
            }
        emptyMaterialRegistry

-- | The item def the dig's chunk yield materializes. Without it
--   'World.Thread.Command.Edit.Dig.spawnYieldItems' warns and drops,
--   and the ground-item half of every control below would be vacuous.
chunkItemManager ∷ ItemManager
chunkItemManager = ItemManager (HM.singleton chunkItemName ItemDef
    { idName = chunkItemName, idDisplayName = "Dig domain chunk"
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 1, idWeightSpec = Nothing, idBulk = 1
    , idStorage = Nothing, idKind = "misc", idCategory = "Misc"
    , idMake = "", idMaterial = "", idQualitySpec = Nothing
    , idQualityTiers = [], idContainer = Nothing
    , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
    , idArmor = Nothing, idUnequippable = False, idBuffs = []
    , idInsulation = 0, idSourcePath = "test-fixture"
    })

-- * Chunk fixture

fixtureChunk ∷ ChunkCoord → LoadedChunk
fixtureChunk coord =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
                 { ctStartZ = 0
                 , ctMats   = VU.replicate 20 stoneId
                 , ctSlopes = VU.replicate 20 0
                 , ctVeg    = VU.replicate 20 0
                 }
    in LoadedChunk
        { lcCoord = coord
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area zSlice
        , lcTerrainSurfaceMap = VU.replicate area zSlice
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        -- Both sized to the chunk, not left empty: completing a tile
        -- runs the real delete-tile edit path, which reads the water
        -- table by column index.
        , lcSideDeco = VU.replicate area 0
        , lcWaterTableMap = VU.replicate area (zSlice - 2)
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

fixtureTiles ∷ WorldTileData
fixtureTiles = WorldTileData
    { wtdChunks = HM.singleton (ChunkCoord 0 0) (fixtureChunk (ChunkCoord 0 0))
    , wtdMaxChunks = 200
    }

-- | An arena-shaped page: seed 0 and an empty timeline, which is
--   'World.Generate.Types.isArenaParams' — so 'stageSession' takes the
--   cheap flat-chunk path instead of generating real terrain.
arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams { wgpSeed = 0, wgpWorldSize = 0 }

-- | The designation every handler example starts from: untouched
--   corners at one z, nothing dug yet.
freshDesignation ∷ MineDesignation
freshDesignation = MineDesignation zSlice (1, 1, 1, 1) 0

-- * Pure domain

spec ∷ Spec
spec = do

  describe "dig argument domains" $ do

    it "admits an amount of zero and rejects a negative one" $ do
      -- Zero is a stalled tick, not a bad command; negative pours
      -- nothing yet reports a completed pour.
      digAmountInDomain 0 `shouldBe` True
      digAmountInDomain 0.5 `shouldBe` True
      digAmountInDomain (-0.0001) `shouldBe` False

    it "admits both skill endpoints and nothing outside them" $ do
      digSkillInDomain 0 `shouldBe` True
      digSkillInDomain 100 `shouldBe` True
      digSkillInDomain (-0.0001) `shouldBe` False
      digSkillInDomain 100.0001 `shouldBe` False
      digSkillInDomain 1e9 `shouldBe` False

    it "rejects NaN and both infinities in every domain" $
      -- One table so a domain cannot be added later and quietly miss
      -- the finiteness half.
      [ inDomain v
      | inDomain ← [ digPositionInDomain, digAmountInDomain
                   , digSkillInDomain, digPerceptionInDomain ]
      , v ← [0 / 0, 1 / 0, -1 / 0] ]
        `shouldSatisfy` all not

    it "leaves perception unbounded above and below, but finite" $ do
      -- World.Gem.gemChanceAt already clamps and saturates it, so a
      -- large or negative finite perception is a bounded one-shot
      -- effect. A non-finite one is not: max 0 NaN is NaN.
      digPerceptionInDomain (-5) `shouldBe` True
      digPerceptionInDomain 1e30 `shouldBe` True
      digPerceptionInDomain (0 / 0) `shouldBe` False

  describe "dig tile-coordinate representability" $ do

    it "rounds an ordinary coordinate to the nearest tile" $ do
      digTileCoordinate 2.6 `shouldBe` Just 3
      digTileCoordinate (-2.6) `shouldBe` Just (-3)
      digTileCoordinate 0 `shouldBe` Just 0

    it "names no tile for NaN or either infinity" $ do
      digTileCoordinate (0 / 0) `shouldBe` Nothing
      digTileCoordinate (1 / 0) `shouldBe` Nothing
      digTileCoordinate (-1 / 0) `shouldBe` Nothing

    it "is exact at Int's own bounds, not at a convenient approximation" $ do
      -- The largest Double strictly below 2^63 is 2^63 - 1024, and it is
      -- the largest value that rounds into Int at all; 2^63 itself is
      -- one past maxBound. minBound is exactly -2^63, which IS a Double.
      let maxD = 9223372036854774784 ∷ Double   -- 2^63 - 1024
          overD = 9223372036854775808 ∷ Double  -- 2^63
          minD = -9223372036854775808 ∷ Double  -- -2^63
      digTileCoordinate maxD `shouldBe` Just 9223372036854774784
      digTileCoordinate overD `shouldBe` Nothing
      digTileCoordinate minD `shouldBe` Just (minBound ∷ Int)
      digTileCoordinate (minD - 4096) `shouldBe` Nothing

    it "rejects a finite coordinate that is merely absurd" $
      digTileCoordinate 1e30 `shouldBe` Nothing

-- * Engine-backed

engineSpec ∷ Spec
engineSpec = beforeAll setup $ do

  luaBoundarySpec
  handlerSpec
  stagingSpec

setup ∷ IO EngineEnv
setup = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    pure env

-- * The Lua boundary

-- | One @world.digTile@ call through the REAL registered closure,
--   against a queue and a logger this module owns.
--
--   The queue is SEEDED with a sentinel command first, so \"queued
--   nothing\" is the observation that the flush returns exactly the
--   sentinel — not merely that it returned no dig, which an empty queue
--   would satisfy even if the verb had drained one.
data DigCallResult = DigCallResult
    { dcrSentinels ∷ Int
      -- ^ How many of the seeded sentinel commands came back.
    , dcrDigs      ∷ [(Text, Int, Int, Float, Float, Float, Float, Float)]
      -- ^ Every 'WorldDigTile' the call queued, flattened — 'WorldCommand'
      --   has no 'Eq', and every field has to be compared or a payload
      --   could be checked on the one slot the implementation happens to
      --   preserve.
    , dcrWarnings  ∷ [Text]
    } deriving (Show, Eq)

digCall ∷ EngineEnv → Text → IO DigCallResult
digCall env argList = do
    queue ← Q.newQueue
    Q.writeQueue queue (WorldDeleteTile fixturePage 0 0)
    (loggerRef, readWarnings) ← captureWarnings
    let cc  = (toCoreCapability env) { ccLoggerRef = loggerRef }
        wsc = (toWorldSimCapability env) { wsWorldQueue = queue }
    Lua.run @Lua.Exception $ do
        Lua.openlibs
        Lua.newtable
        registerLuaFunction "digTile" (worldDigTileFn cc wsc)
        Lua.setglobal "world"
        st ← Lua.dostring
                (TE.encodeUtf8 ("world.digTile(" <> argList <> ")"))
        case st of
            Lua.OK → pure ()
            _      → do
                err ← Lua.tostring (-1)
                Lua.liftIO $ expectationFailure $
                    "Lua chunk failed: "
                      ⧺ maybe "<no message>" (T.unpack . TE.decodeUtf8Lenient)
                                             err
    queued ← Q.flushQueue queue
    warnings ← readWarnings
    pure DigCallResult
        { dcrSentinels = length [ () | WorldDeleteTile{} ← queued ]
        , dcrDigs =
            [ (unWorldPageId p, gx, gy, ux, uy, amt, skill, percep)
            | WorldDigTile p gx gy ux uy amt skill percep ← queued ]
        , dcrWarnings = warnings
        }

-- | A logger whose warning-or-worse entries land in an 'IORef'. The
--   engine's own logger is untouched, so a quiet fixture stays quiet.
captureWarnings ∷ IO (IORef LoggerState, IO [Text])
captureWarnings = do
    capturedRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback $ \e →
            when (leLevel e ≥ LevelWarn) $
                atomicModifyIORef' capturedRef
                    (\es → (leMessage e : es, ())) }
    writeIORef (lsMinLevel logger) LevelDebug
    writeIORef (lsEnabled logger) True
    ref ← newIORef logger
    pure (ref, reverse <$> readIORef capturedRef)

-- | The argument list every refusal example varies exactly one slot of.
--   Its own acceptance is pinned first, so a refusal below cannot be
--   passing because the BASE call was already broken.
validArgs ∷ Text
validArgs = "'dig_domain_probe', 8, 8, 7.5, 8.5, 0.5, 50, 2"

-- | 'validArgs' with the @n@th argument (1-based, as Lua counts them)
--   replaced.
withArg ∷ Int → Text → Text
withArg n replacement =
    T.intercalate ", "
        [ if i ≡ n then replacement else a
        | (i, a) ← zip [1 ..] (map T.strip (T.splitOn "," validArgs)) ]

-- | Assert that a call was refused: nothing queued past the sentinel,
--   exactly one warning, and that warning names the verb and the
--   argument it is about.
shouldRefuse ∷ HasCallStack ⇒ DigCallResult → Text → Expectation
shouldRefuse result argName = do
    dcrDigs result `shouldBe` []
    dcrSentinels result `shouldBe` 1
    case dcrWarnings result of
        [w] → do
            T.unpack w `shouldSatisfy` isInfixOf "world.digTile"
            T.unpack w `shouldSatisfy` isInfixOf ("'" ⧺ T.unpack argName ⧺ "'")
            T.unpack w `shouldSatisfy` isInfixOf "call refused"
        ws → expectationFailure $
            "expected exactly one warning, got " ⧺ show ws

luaBoundarySpec ∷ SpecWith EngineEnv
luaBoundarySpec = describe "world.digTile admission (#2338)" $ do

  it "queues the call it was given, unnarrowed and unrounded" $ \env → do
    result ← digCall env validArgs
    dcrDigs result `shouldBe`
        [("dig_domain_probe", 8, 8, 7.5, 8.5, 0.5, 50, 2)]
    dcrSentinels result `shouldBe` 1
    dcrWarnings result `shouldBe` []

  it "rounds a fractional tile coordinate rather than refusing it" $ \env → do
    result ← digCall env (withArg 2 "8.6")
    dcrDigs result `shouldBe`
        [("dig_domain_probe", 9, 8, 7.5, 8.5, 0.5, 50, 2)]
    dcrWarnings result `shouldBe` []

  describe "a NaN in any numeric slot refuses the call" $
    -- Every numeric slot, so a later argument cannot be added without
    -- an entry here. 0/0 rather than a literal: Lua has no NaN literal.
    forM_ [ (2 ∷ Int, "gx"), (3, "gy"), (4, "ux"), (5, "uy")
          , (6, "amount"), (7, "minerSkill"), (8, "perception") ] $
      \(slot, name) →
        it (T.unpack name) $ \env →
            digCall env (withArg slot "0/0") ⌦ (`shouldRefuse` name)

  describe "an infinity in any numeric slot refuses the call" $
    forM_ [ (2 ∷ Int, "gx"), (3, "gy"), (4, "ux"), (5, "uy")
          , (6, "amount"), (7, "minerSkill"), (8, "perception") ] $
      \(slot, name) →
        it (T.unpack name) $ \env → do
            digCall env (withArg slot "math.huge") ⌦ (`shouldRefuse` name)
            digCall env (withArg slot "-math.huge") ⌦ (`shouldRefuse` name)

  it "refuses a finite Lua number that becomes infinite as a Float" $
    \env → do
      -- 1e39 is an ordinary finite Double and Infinity as a Float, which
      -- is the value the command actually carries. Checking before the
      -- narrowing would let exactly this through.
      digCall env (withArg 4 "1e39") ⌦ (`shouldRefuse` "ux")
      digCall env (withArg 8 "1e39") ⌦ (`shouldRefuse` "perception")

  it "reports the number the author wrote, not the narrowing of it" $
    \env → do
      result ← digCall env (withArg 4 "1e39")
      case dcrWarnings result of
        [w] → do
            T.unpack w `shouldSatisfy` isInfixOf "1.0e39"
            T.unpack w `shouldSatisfy` \s → not (isInfixOf "Infinity" s)
        ws → expectationFailure ("expected one warning, got " ⧺ show ws)

  it "refuses a finite tile coordinate that names no tile" $ \env → do
    digCall env (withArg 2 "1e30") ⌦ (`shouldRefuse` "gx")
    digCall env (withArg 3 "-1e30") ⌦ (`shouldRefuse` "gy")

  it "refuses a negative amount" $ \env →
    digCall env (withArg 6 "-0.5") ⌦ (`shouldRefuse` "amount")

  it "admits an amount of exactly zero" $ \env → do
    result ← digCall env (withArg 6 "0")
    dcrDigs result `shouldBe`
        [("dig_domain_probe", 8, 8, 7.5, 8.5, 0, 50, 2)]
    dcrWarnings result `shouldBe` []

  it "refuses a skill outside the 0-100 scale, at either end" $ \env → do
    digCall env (withArg 7 "-1") ⌦ (`shouldRefuse` "minerSkill")
    digCall env (withArg 7 "101") ⌦ (`shouldRefuse` "minerSkill")
    -- The value that would have spawned millions of ground items in one
    -- world-thread tick.
    digCall env (withArg 7 "1e9") ⌦ (`shouldRefuse` "minerSkill")

  it "admits both skill endpoints" $ \env → do
    zeroSkill ← digCall env (withArg 7 "0")
    dcrDigs zeroSkill `shouldBe`
        [("dig_domain_probe", 8, 8, 7.5, 8.5, 0.5, 0, 2)]
    maxSkill ← digCall env (withArg 7 "100")
    dcrDigs maxSkill `shouldBe`
        [("dig_domain_probe", 8, 8, 7.5, 8.5, 0.5, 100, 2)]
    map dcrWarnings [zeroSkill, maxSkill] `shouldBe` [[], []]

  it "keeps the documented defaults for an omitted optional argument" $
    \env → do
      result ← digCall env "'dig_domain_probe', 8, 8, 7.5, 8.5, 0.5"
      dcrDigs result `shouldBe`
          [("dig_domain_probe", 8, 8, 7.5, 8.5, 0.5, 0, 1)]
      dcrWarnings result `shouldBe` []

  it "keeps them for an explicit nil too" $ \env → do
    result ← digCall env "'dig_domain_probe', 8, 8, 7.5, 8.5, 0.5, nil, nil"
    dcrDigs result `shouldBe`
        [("dig_domain_probe", 8, 8, 7.5, 8.5, 0.5, 0, 1)]
    dcrWarnings result `shouldBe` []

  it "refuses a supplied optional argument that is not a number, \
     \rather than silently inheriting the default" $ \env → do
    -- The distinction Lua.tonumber alone cannot draw: a typo'd seventh
    -- argument used to become skill 0 and dig on.
    digCall env (withArg 7 "{}") ⌦ (`shouldRefuse` "minerSkill")
    digCall env (withArg 7 "true") ⌦ (`shouldRefuse` "minerSkill")
    digCall env (withArg 8 "{}") ⌦ (`shouldRefuse` "perception")

  it "refuses a numeric STRING in a slot documented as a number" $
    \env → do
      digCall env (withArg 7 "'50'") ⌦ (`shouldRefuse` "minerSkill")
      digCall env (withArg 6 "'0.5'") ⌦ (`shouldRefuse` "amount")

  it "refuses a required argument that is missing or the wrong type" $
    \env → do
      digCall env "'dig_domain_probe', 8, 8" ⌦ (`shouldRefuse` "ux")
      digCall env (withArg 4 "{}") ⌦ (`shouldRefuse` "ux")

  it "emits ONE warning for a call with several bad arguments" $ \env → do
    -- One mistake, one line: several would make the log harder to read
    -- without telling the caller anything the first does not.
    result ← digCall env "'dig_domain_probe', 0/0, 0/0, 0/0, 0/0, 0/0"
    length (dcrWarnings result) `shouldBe` 1
    dcrDigs result `shouldBe` []

  it "still does nothing, quietly, without a page id" $ \env → do
    -- Unchanged pre-#2338 behaviour, pinned rather than altered: an
    -- absent page id names no call to refuse.
    result ← digCall env "nil, 8, 8, 7.5, 8.5, 0.5"
    dcrDigs result `shouldBe` []
    dcrSentinels result `shouldBe` 1
    dcrWarnings result `shouldBe` []

-- * The world thread's own defence

-- | Everything one 'handleWorldDigTileCommand' call could have touched,
--   captured together so an example asserts on all of it at once rather
--   than on the one field it happened to remember.
data DigWorldState = DigWorldState
    { dwsDesignations ∷ [((Int, Int), Int, (Float, Float, Float, Float), Float)]
    , dwsSpoil        ∷ [((Int, Int), Word8, (Float, Float, Float, Float))]
    , dwsGroundItems  ∷ Int
    , dwsEdits        ∷ Int
    , dwsUnitCommands ∷ Word64
    } deriving (Show, Eq)

captureDigWorld ∷ WorldState → Q.Queue UnitCommand → IO DigWorldState
captureDigWorld ws unitQ = do
    desigs ← readIORef (wsMineDesignationsRef ws)
    piles  ← readIORef (wsSpoilRef ws)
    items  ← readIORef (wsGroundItemsRef ws)
    edits  ← readIORef (wsEditsRef ws)
    stats  ← Q.queueStats unitQ
    pure DigWorldState
        { dwsDesignations = sort
            [ (k, mdZ md, mdCorners md, mdChunkProgress md)
            | (k, md) ← HM.toList desigs ]
        , dwsSpoil = sort
            [ (v, unMaterialId (spMat p), spFill p)
            | (v, p) ← HM.toList piles ]
        , dwsGroundItems = HM.size (gisItems items)
        , dwsEdits = sum (map length (HM.elems edits))
        , dwsUnitCommands = Q.qsEnqueued stats
        }

-- | Install a fresh single-chunk page carrying one untouched
--   designation on 'digTile', with this fixture's material registry and
--   item manager in place so a dig has real spoil to route and a real
--   chunk to yield.
resetDigPage ∷ EngineEnv → IO (WorldState, IORef StdGen, Q.Queue UnitCommand)
resetDigPage env = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just arenaParams)
    writeIORef (wsTilesRef ws) fixtureTiles
    writeIORef (wsMineDesignationsRef ws)
        (HM.singleton digTile freshDesignation)
    writeIORef (materialRegistryRef env) digRegistry
    writeIORef (itemManagerRef env) chunkItemManager
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    rngRef ← newIORef (mkStdGen 20260904)
    unitQ  ← Q.newQueue
    pure (ws, rngRef, unitQ)

-- | Run one 'handleWorldDigTileCommand' against a fresh page and report
--   what the world looked like before and after, plus what was logged.
runDigCommand
    ∷ EngineEnv → (Float, Float, Float, Float, Float)
    → IO (DigWorldState, DigWorldState, [Text])
runDigCommand env (ux, uy, amount, skill, percep) = do
    (ws, rngRef, unitQ) ← resetDigPage env
    (loggerRef, readWarnings) ← captureWarnings
    logger ← readIORef loggerRef
    before ← captureDigWorld ws unitQ
    handleWorldDigTileCommand env rngRef unitQ logger fixturePage
        (fst digTile) (snd digTile) ux uy amount skill percep
    after ← captureDigWorld ws unitQ
    warnings ← readWarnings
    pure (before, after, warnings)

handlerSpec ∷ SpecWith EngineEnv
handlerSpec = describe "the WorldDigTile handler's own guard (#2338)" $ do

  -- The controls come first: every "unchanged" claim below is only
  -- worth anything because these prove the fixture CAN change.
  describe "a valid payload moves the state the refusals must not" $ do

    it "drains corners, accrues chunk progress and routes spoil" $
      \env → do
        (before, after, warnings) ←
            runDigCommand env (7.5, 8.5, 0.5, 50, 2)
        warnings `shouldBe` []
        dwsDesignations after `shouldNotBe` dwsDesignations before
        dwsSpoil after `shouldNotBe` dwsSpoil before
        -- Exactly half a corner-unit came out of the digger-side corner,
        -- and the accumulator took drained × (0.5 + 50/100) / 4.
        dwsDesignations after `shouldBe`
            [(digTile, zSlice, (0.5, 1, 1, 1), 0.125)]

    it "completes the tile, removing the designation and logging an edit" $
      \env → do
        (before, after, _) ← runDigCommand env (7.5, 8.5, 4.0, 50, 2)
        dwsDesignations after `shouldBe` []
        dwsEdits after `shouldSatisfy` (> dwsEdits before)

    it "spawns the yield items a whole chunk earns" $ \env → do
      -- drained 4 × (0.5 + 100/100)/4 = 1.5 → one whole chunk spawns.
      (before, after, _) ← runDigCommand env (7.5, 8.5, 4.0, 100, 2)
      dwsGroundItems before `shouldBe` 0
      dwsGroundItems after `shouldSatisfy` (> 0)

  describe "an out-of-domain payload is dropped whole" $ do

    let dropsOn name payload field = it name $ \env → do
          (before, after, warnings) ← runDigCommand env payload
          -- Nothing moved: the guard runs before the page is even
          -- looked up, so the designation map, the spoil piles, the
          -- ground items, the edit log and the unit queue are all
          -- exactly what they were.
          after `shouldBe` before
          case warnings of
            [w] → do
                T.unpack w `shouldSatisfy` isInfixOf "WorldDigTile"
                T.unpack w `shouldSatisfy` isInfixOf (T.unpack field)
                T.unpack w `shouldSatisfy` isInfixOf "command dropped"
            ws → expectationFailure
                    ("expected exactly one warning, got " ⧺ show ws)

    dropsOn "a NaN digger x" (0 / 0, 8.5, 0.5, 50, 2) "uxPos"
    dropsOn "a NaN digger y" (7.5, 0 / 0, 0.5, 50, 2) "uyPos"
    dropsOn "an infinite digger x" (1 / 0, 8.5, 0.5, 50, 2) "uxPos"
    dropsOn "a NaN amount" (7.5, 8.5, 0 / 0, 50, 2) "amount"
    dropsOn "an infinite amount" (7.5, 8.5, 1 / 0, 50, 2) "amount"
    dropsOn "a negative amount" (7.5, 8.5, -0.5, 50, 2) "amount"
    dropsOn "a NaN skill" (7.5, 8.5, 0.5, 0 / 0, 2) "minerSkill"
    dropsOn "a skill above the scale" (7.5, 8.5, 0.5, 1e9, 2) "minerSkill"
    dropsOn "a negative skill" (7.5, 8.5, 0.5, -1, 2) "minerSkill"
    dropsOn "a NaN perception" (7.5, 8.5, 0.5, 50, 0 / 0) "perception"
    dropsOn "an infinite perception" (7.5, 8.5, 0.5, 50, -1 / 0) "perception"

  it "names the FIRST offending field only, once" $ \env → do
    (_, _, warnings) ← runDigCommand env (0 / 0, 0 / 0, 0 / 0, 0 / 0, 0 / 0)
    case warnings of
        [w] → T.unpack w `shouldSatisfy` isInfixOf "uxPos"
        ws  → expectationFailure
                  ("expected exactly one warning, got " ⧺ show ws)

  it "would otherwise have poisoned every corner beyond recovery" $
    \_env → do
      -- The measured damage this guard exists to prevent, pinned
      -- against the primitive rather than the handler: a NaN amount
      -- misses drainCorners' own amt <= 0 stop, min v NaN is NaN, and
      -- cornersDone can then never hold for the resulting designation.
      let poisoned = drainCorners diggerPos digTile (0 / 0) (1, 1, 1, 1)
          (a, b, c, d) = poisoned
      map isNaN [a, b, c, d] `shouldBe` [True, True, True, True]
      cornersDone poisoned `shouldBe` False

-- * The load repair

-- | A one-page save carrying exactly the mine designations given,
--   through the same 'World.Save.Snapshot.Adapter' bridge a real decode
--   produces — so what staging sees here is shaped like what it sees in
--   production, not a record hand-built to suit the assertion.
saveWith ∷ [((Int, Int), MineDesignation)] → SaveData
saveWith desigs = snapshotToSaveData
    SaveRequestMeta { srmSlotName = "dig_domain_repair"
                    , srmTimestamp = "2026-09-04T00:00:00.000000Z"
                    , srmAutosave = False }
    SessionSnapshot
        { snapGameTime = 0
        , snapTexPalette = emptyTexPalette
        , snapNextItemId = 1
        , snapNextBuildingId = 1
        , snapNextUnitId = 1
        , snapActivePage = fixturePage
        , snapVisiblePages = [fixturePage]
        , snapLiveCamera = LiveCameraSnapshot
            { lcsOwnerPage = Just fixturePage, lcsX = 0, lcsY = 0
            , lcsZoom = 1, lcsFacing = FaceNorth }
        , snapPages = HM.singleton fixturePage
            (blankPageSnapshot fixturePage arenaParams)
                { pgsMineDesignations = HM.fromList desigs }
        }

-- | Stage that save and report the designations the staged (never
--   published) page ended up with, plus what staging logged.
stagedDesignations
    ∷ EngineEnv → SaveData
    → IO ([((Int, Int), Int, (Float, Float, Float, Float), Float)], [Text])
stagedDesignations env sd = do
    (loggerRef, readWarnings) ← captureWarnings
    logger ← readIORef loggerRef
    registry ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger sd registry
    warnings ← readWarnings
    case staged of
        Left err → do
            expectationFailure
                ("staging failed: " ⧺ T.unpack (renderStageError err))
            pure ([], warnings)
        Right session → case [ p | p ← ssPages session
                                 , spPageId p ≡ fixturePage ] of
            [] → do
                expectationFailure "staged session has no fixture page"
                pure ([], warnings)
            (p : _) → do
                desigs ← readIORef (wsMineDesignationsRef (spWorldState p))
                pure ( sort [ (k, mdZ md, mdCorners md, mdChunkProgress md)
                            | (k, md) ← HM.toList desigs ]
                     , warnings )

-- | A designation the repair must leave completely alone: partially dug,
--   with a real remainder, and every number finite.
healthyDesignation ∷ MineDesignation
healthyDesignation = MineDesignation 7 (0.25, 1, 0.5, 1) 0.75

stagingSpec ∷ SpecWith EngineEnv
stagingSpec = describe "restored mine designations stay finite (#2338)" $ do

  it "leaves a wholly finite save untouched, and logs nothing about it" $
    \env → do
      (desigs, warnings) ← stagedDesignations env
          (saveWith [ (digTile, freshDesignation)
                    , (healthyTile, healthyDesignation) ])
      desigs `shouldBe` sort
          [ (digTile, zSlice, (1, 1, 1, 1), 0)
          , (healthyTile, 7, (0.25, 1, 0.5, 1), 0.75) ]
      filter (isInfixOf "mine designation" . T.unpack) warnings
          `shouldBe` []

  it "repairs a non-finite CORNER, keeping the key and the z, and \
     \leaves a valid sibling exactly as it was" $ \env → do
    let poisoned = MineDesignation zSlice (0.5, 0 / 0, 1, 1) 0.4
    (desigs, warnings) ← stagedDesignations env
        (saveWith [(digTile, poisoned), (healthyTile, healthyDesignation)])
    desigs `shouldBe` sort
        -- Corners to undug and progress to zero; mdZ and the tile key
        -- survive, because neither was what got corrupted.
        [ (digTile, zSlice, (1, 1, 1, 1), 0)
        , (healthyTile, 7, (0.25, 1, 0.5, 1), 0.75) ]
    case filter (isInfixOf "mine designation" . T.unpack) warnings of
        [w] → do
            T.unpack w `shouldSatisfy` isInfixOf "dig_domain_probe"
            T.unpack w `shouldSatisfy` isInfixOf "8,8"
        ws → expectationFailure
                ("expected one designation warning, got " ⧺ show ws)

  it "repairs a non-finite chunk PROGRESS even when every corner is a \
     \perfectly good partial dig" $ \env → do
    let poisoned = MineDesignation zSlice (0.25, 0.5, 1, 1) (0 / 0)
    (desigs, warnings) ← stagedDesignations env
        (saveWith [(digTile, poisoned), (healthyTile, healthyDesignation)])
    desigs `shouldBe` sort
        [ (digTile, zSlice, (1, 1, 1, 1), 0)
        , (healthyTile, 7, (0.25, 1, 0.5, 1), 0.75) ]
    length (filter (isInfixOf "mine designation" . T.unpack) warnings)
        `shouldBe` 1

  it "repairs an INFINITE corner and an infinite progress alike" $
    \env → do
      (desigs, _) ← stagedDesignations env
          (saveWith [ (digTile, MineDesignation zSlice (1, 1 / 0, 1, 1) 0)
                    , (healthyTile, MineDesignation 7 (1, 1, 1, 1) (-1 / 0)) ])
      desigs `shouldBe` sort
          [ (digTile, zSlice, (1, 1, 1, 1), 0)
          , (healthyTile, 7, (1, 1, 1, 1), 0) ]

  it "never fails the load over one — the rest of the save is good" $
    \env → do
      (desigs, _) ← stagedDesignations env
          (saveWith [(digTile, MineDesignation zSlice (0 / 0, 0 / 0, 0 / 0, 0 / 0) (0 / 0))])
      -- Reaching an assertion at all means stageSession returned Right;
      -- stagedDesignations fails the example on a Left.
      desigs `shouldBe` [(digTile, zSlice, (1, 1, 1, 1), 0)]

  it "hands the repaired designation to the slope stamp, not the \
     \poisoned one" $ \env → do
    -- Staging stamps dig slopes from the map this repair writes, so a
    -- NaN corner would otherwise reach applyDigSlopes on the arena path.
    (desigs, _) ← stagedDesignations env
        (saveWith [(digTile, MineDesignation zSlice (0, 0, 0, 0 / 0) 0)])
    case desigs of
        [(_, _, (a, b, c, d), _)] →
            map isNaN [a, b, c, d] `shouldBe` [False, False, False, False]
        other → expectationFailure ("unexpected designations: " ⧺ show other)
