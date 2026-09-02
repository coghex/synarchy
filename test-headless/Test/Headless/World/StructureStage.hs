{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "Declined structure placements retract their stage" (#1674).
--
--   @structure.place@ checks chunk residency on the LUA thread, writes a
--   read-your-writes entry into 'wsStructureStageRef', and queues a
--   'WorldSetStructure'. The world thread checks residency AGAIN, and a
--   load pass can evict the chunk in between (it inserts and evicts in
--   one atomic 'wsTilesRef' update). Before #1674 the declined command
--   logged and returned, leaving the staged entry behind as a phantom
--   every structure query reported as real — absent from the edit log,
--   and gone after a save/load, so one query answered two ways across a
--   round trip.
--
--   Nothing here needs the race. Every example stages through the REAL
--   @structure.place@, dequeues the command that call actually emitted
--   ('Q.tryReadQueue' — never an independently constructed equivalent,
--   because the attempt token that separates one placement from the next
--   lives on that value), removes the target chunk by hand, and
--   dispatches the captured command through the production
--   'handleWorldCommand'.
--
--   The engine is this module's own 'initializeEngineHeadlessQuiet' (the
--   'Test.Headless.Building.PageBinding' shape): it runs NO worker
--   threads, so a queued command waits to be dequeued here rather than
--   being raced away by a drainer. Both pages are in-memory
--   'emptyWorldState' pages carrying synthetic flat chunks — no
--   worldgen.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "retract their stage"'@ (the issue's own
--   @--match "structure"@ runs it together with every other
--   structure-named example).
module Test.Headless.World.StructureStage (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort, sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.Log (LoggerState)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Types
    ( StructurePieceData(..), StructureSlot(..), StructureStage(..)
    , StructureStageToken, emptyChunkStructures, stgToken )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Command.Types (WorldCommand(..))
import World.Edit.Types (WorldEdit(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile, globalToChunk, tileAliasStep)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Thread.Command (handleWorldCommand)
import World.Tile.Types (WorldTileData(..))

-- * Fixture identity

-- | The page every placement targets, and the page that exists only to
--   prove the retraction is per-world (#1674 r7). Both are registered;
--   only 'mainPage' is visible, so the page-less verbs
--   (@structure.clear@, @structure.count@) resolve to it.
mainPage, otherPage ∷ WorldPageId
mainPage  = WorldPageId "stage_main"
otherPage = WorldPageId "stage_other"

-- | World size in chunks. Even and non-zero, so the u-wrap is real and
--   'aliasOf' below names a genuinely different coord for one tile.
worldSizeChunks ∷ Int
worldSizeChunks = 8

-- | The tile every scenario places on, and the slot it uses. Chunk
--   (0,0), already canonical under 'worldSizeChunks'.
targetTile ∷ (Int, Int)
targetTile = (3, 3)

targetSlot ∷ StructureSlot
targetSlot = SFloor

targetSlotName ∷ Text
targetSlotName = "floor"

targetSlotTag ∷ Word8
targetSlotTag = fromIntegral (fromEnum targetSlot)

-- | The staged/stored key for 'targetTile' — canonical, exactly as
--   'structure.place' writes it and 'lcStructures' stores it.
targetKey ∷ (Int, Int, Word8)
targetKey = (fst targetTile, snd targetTile, targetSlotTag)

-- | The chunk that stores 'targetTile'.
targetChunk ∷ ChunkCoord
targetChunk = fst (globalToChunk (fst targetTile) (snd targetTile))

-- | A u-alias of a tile: the same physical tile named one wrap away.
--   Shifting u by the world's half-width moves (gx, gy) by
--   (+step, -step), which preserves @v = gx + gy@.
aliasOf ∷ (Int, Int) → (Int, Int)
aliasOf (gx, gy) = (gx + tileAliasStep worldSizeChunks
                   , gy - tileAliasStep worldSizeChunks)

-- | The z every placement uses, and the texture paths it interns. The
--   palette ids those paths resolve to are read back off the emitted
--   command rather than assumed: the palette is engine-global and
--   accumulates across examples.
placeZ ∷ Int
placeZ = 4

texPath, facePath ∷ Text
texPath  = "assets/textures/structures/stage_test_floor.png"
facePath = "assets/textures/structures/stage_test_face.png"

-- * Terrain fixtures

-- | A flat chunk with a real per-tile column vector and no structures.
flatChunkAt ∷ ChunkCoord → LoadedChunk
flatChunkAt coord =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.singleton 1
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0
            }
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = V.replicate area col
        , lcSurfaceMap        = VU.replicate area 0
        , lcTerrainSurfaceMap = VU.replicate area 0
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate area 0
        , lcWaterTableMap     = VU.replicate area 0
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

tilesFrom ∷ [LoadedChunk] → WorldTileData
tilesFrom chunks = WorldTileData
    { wtdChunks    = HM.fromList [ (lcCoord c, c) | c ← chunks ]
    , wtdMaxChunks = max 1 (length chunks)
    }

loadedTiles ∷ WorldTileData
loadedTiles = tilesFrom [flatChunkAt targetChunk]

-- | The eviction: the same page with 'targetChunk' gone. A load pass
--   producing this is exactly what #1674's race is.
evictedTiles ∷ WorldTileData
evictedTiles = tilesFrom []

genParams ∷ WorldGenParams
genParams = defaultWorldGenParams { wgpWorldSize = worldSizeChunks }

-- * Scene

-- | Both pages installed with 'targetChunk' loaded, 'mainPage' visible,
--   both stages and edit logs empty, the world queue drained.
resetScene ∷ EngineEnv → IO (WorldState, WorldState)
resetScene env = do
    wsMain  ← emptyWorldState
    wsOther ← emptyWorldState
    forM_ [wsMain, wsOther] $ \ws → do
        writeIORef (wsTilesRef ws) loadedTiles
        writeIORef (wsGenParamsRef ws) (Just genParams)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(mainPage, wsMain), (otherPage, wsOther)]
        , wmVisible = [mainPage] }
    _ ← drainWorldQueue env
    pure (wsMain, wsOther)

-- | Evict 'targetChunk' from a page, the way a load pass would.
evictTargetChunk ∷ WorldState → IO ()
evictTargetChunk ws = writeIORef (wsTilesRef ws) evictedTiles

-- * Queue + dispatch

drainWorldQueue ∷ EngineEnv → IO [WorldCommand]
drainWorldQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (worldQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

-- | The one command the preceding call emitted. Fails loudly on any
--   other count, so an example can never silently dispatch the wrong
--   attempt.
takeOneCommand ∷ EngineEnv → IO WorldCommand
takeOneCommand env = do
    cmds ← drainWorldQueue env
    case cmds of
        [cmd] → pure cmd
        other → fail $ "expected exactly one queued world command, got "
                         <> show (length other) <> ": " <> show other

logger ∷ EngineEnv → IO LoggerState
logger env = readIORef (loggerRef env)

-- | Run one captured command through the REAL world-thread dispatcher.
dispatch ∷ EngineEnv → WorldCommand → IO ()
dispatch env cmd = do
    lg ← logger env
    handleWorldCommand env lg cmd

-- | The command's payload with its attempt token removed — what two
--   placements of the same piece at the same tile agree on.
setPayload ∷ WorldCommand → Maybe (WorldPageId, Int, Int, Word8, Int, Int, Int)
setPayload (WorldSetStructure p gx gy slotTag texId faceId z _) =
    Just (p, gx, gy, slotTag, texId, faceId, z)
setPayload _ = Nothing

-- | The command's attempt token.
setToken ∷ WorldCommand → Maybe StructureStageToken
setToken (WorldSetStructure _ _ _ _ _ _ _ tok) = Just tok
setToken _ = Nothing

-- | The WeSetStructure edit a successful commit of this command must
--   append — derived from the command, since the palette ids are
--   whatever the engine-global palette interned.
expectedSetEdit ∷ WorldCommand → Maybe WorldEdit
expectedSetEdit (WorldSetStructure _ gx gy slotTag texId faceId z _) =
    Just (WeSetStructure gx gy slotTag texId faceId z)
expectedSetEdit _ = Nothing

-- * Live-state readers

stageOf ∷ WorldState → IO StructureStage
stageOf ws = readIORef (wsStructureStageRef ws)

stageKeys ∷ WorldState → IO [(Int, Int, Word8)]
stageKeys ws = sort . HM.keys . ssEntries <$> stageOf ws

stageTokenAt ∷ WorldState → (Int, Int, Word8) → IO (Maybe StructureStageToken)
stageTokenAt ws key = fmap stgToken . HM.lookup key . ssEntries <$> stageOf ws

-- | Every structure edit the page's log holds, in order.
structureEdits ∷ WorldState → IO [WorldEdit]
structureEdits ws = do
    es ← readIORef (wsEditsRef ws)
    pure [ e | (_, edits) ← sortOn fst (HM.toList es)
             , e ← edits, isStructureEdit e ]
  where
    isStructureEdit (WeSetStructure {})   = True
    isStructureEdit (WeClearStructure {}) = True
    isStructureEdit _                     = False

-- | The authoritative overlay entry for 'targetKey' on a page.
overlayAt ∷ WorldState → IO (Maybe StructurePieceData)
overlayAt ws = do
    td ← readIORef (wsTilesRef ws)
    pure $ HM.lookup targetChunk (wtdChunks td)
             ⌦ HM.lookup targetKey . lcStructures

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src

pageArg ∷ Maybe WorldPageId → Text
pageArg Nothing               = ""
pageArg (Just (WorldPageId p)) = T.concat [", '", p, "'"]

-- | The REAL @structure.place@, at an explicit tile, on an explicit
--   page. Every scenario stages through this — never by writing the
--   stage directly, which would be the test asserting its own writes.
placeAt ∷ LuaBackendState → (Int, Int) → Maybe WorldPageId → IO Text
placeAt ls (gx, gy) mPage = evalDebug ls $ T.concat
    [ "return tostring(structure.place("
    , tshow gx, ", ", tshow gy, ", '", targetSlotName, "', 11, 12, "
    , tshow placeZ, ", '", texPath, "', '", facePath, "'"
    , pageArg mPage, "))" ]

hasAt ∷ LuaBackendState → (Int, Int) → Maybe WorldPageId → IO Text
hasAt ls (gx, gy) mPage = evalDebug ls $ T.concat
    [ "return tostring(structure.hasAt("
    , tshow gx, ", ", tshow gy, ", '", targetSlotName, "'"
    , pageArg mPage, "))" ]

floorZAt ∷ LuaBackendState → (Int, Int) → Maybe WorldPageId → IO Text
floorZAt ls (gx, gy) mPage = evalDebug ls $ T.concat
    [ "return tostring(structure.floorZAt("
    , tshow gx, ", ", tshow gy, pageArg mPage, "))" ]

getAtZ ∷ LuaBackendState → (Int, Int) → Maybe WorldPageId → IO Text
getAtZ ls (gx, gy) mPage = evalDebug ls $ T.concat
    [ "local p = structure.getAt("
    , tshow gx, ", ", tshow gy, ", '", targetSlotName, "'"
    , pageArg mPage, "); if p == nil then return 'nil' end; return tostring(p.z)" ]

countAll ∷ LuaBackendState → IO Text
countAll ls = evalDebug ls "return tostring(structure.count())"

clearAt ∷ LuaBackendState → (Int, Int) → IO Text
clearAt ls (gx, gy) = evalDebug ls $ T.concat
    [ "return tostring(structure.clear("
    , tshow gx, ", ", tshow gy, ", '", targetSlotName, "'))" ]

clearAll ∷ LuaBackendState → IO Text
clearAll ls = evalDebug ls "structure.clearAll(); return 'ok'"

-- | Every query the issue names, on the active page, as one tuple:
--   (hasAt, floorZAt, getAt().z, count). Requirement 1 is that all four
--   read exactly as they did before the placement was staged.
allQueries ∷ LuaBackendState → (Int, Int) → IO (Text, Text, Text, Text)
allQueries ls tile = (,,,)
    <$> hasAt ls tile Nothing
    <*> floorZAt ls tile Nothing
    <*> getAtZ ls tile Nothing
    <*> countAll ls

-- | What 'allQueries' reads for a tile carrying no piece at all.
emptyQueries ∷ (Text, Text, Text, Text)
emptyQueries = ("false", "nil", "nil", "0")

-- | What it reads while the placement is live.
placedQueries ∷ (Text, Text, Text, Text)
placedQueries = ("true", tshow placeZ, tshow placeZ, "1")

-- * Spec

spec ∷ Spec
spec = describe "Declined structure placements retract their stage (#1674)"
     $ aroundAll setup $ do
    fixtureSpec
    declineSpec
    attemptIdentitySpec
    commitSpec
    clearSpec
    isolationSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer, so a scratch root established afterwards is too late.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        act (env, ls)

-- | The fixture's discriminators really discriminate: a fresh scene
-- reads as empty (so "back to baseline" is a real assertion rather than
-- a coincidence), the alias really is a different name for the target
-- tile, and the target chunk really is the one eviction removes.
fixtureSpec ∷ SpecWith (EngineEnv, LuaBackendState)
fixtureSpec = describe "the fixture" $ do

    it "starts with every structure query empty" $ \(env, ls) → do
        _ ← resetScene env
        allQueries ls targetTile `shouldReturn` emptyQueries

    it "names one physical tile by two different coords" $ \_ → do
        aliasOf targetTile `shouldNotBe` targetTile
        uncurry (canonicalTile worldSizeChunks) (aliasOf targetTile)
            `shouldBe` targetTile

    it "evicts exactly the chunk that stores the target tile" $ \_ → do
        HM.member targetChunk (wtdChunks loadedTiles)  `shouldBe` True
        HM.member targetChunk (wtdChunks evictedTiles) `shouldBe` False

-- | Requirement 1: a declined commit leaves nothing behind.
declineSpec ∷ SpecWith (EngineEnv, LuaBackendState)
declineSpec = describe "a declined commit" $ do

    it "leaves every structure query reading as it did before staging" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            before ← allQueries ls targetTile
            before `shouldBe` emptyQueries

            placeAt ls targetTile Nothing `shouldReturn` "true"
            cmd ← takeOneCommand env
            allQueries ls targetTile `shouldReturn` placedQueries

            evictTargetChunk wsMain
            dispatch env cmd

            allQueries ls targetTile `shouldReturn` before
            stageKeys wsMain `shouldReturn` []

    it "appends nothing to the edit log and places nothing in the overlay" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            placeAt ls targetTile Nothing `shouldReturn` "true"
            cmd ← takeOneCommand env
            evictTargetChunk wsMain
            dispatch env cmd
            structureEdits wsMain `shouldReturn` []
            overlayAt wsMain `shouldReturn` Nothing

    it "retracts a placement staged through a u-alias at its canonical key" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            let alias = aliasOf targetTile
            placeAt ls alias Nothing `shouldReturn` "true"
            -- staged and queued canonically, whichever alias was typed
            stageKeys wsMain `shouldReturn` [targetKey]
            cmd ← takeOneCommand env
            setPayload cmd `shouldBe`
                setPayloadFor (fst targetTile) (snd targetTile) cmd

            evictTargetChunk wsMain
            dispatch env cmd

            stageKeys wsMain `shouldReturn` []
            hasAt ls alias      Nothing `shouldReturn` "false"
            hasAt ls targetTile Nothing `shouldReturn` "false"
  where
    -- The payload the command WOULD carry were its coords canonical:
    -- equal to the real payload exactly when they are.
    setPayloadFor gx gy (WorldSetStructure p _ _ slotTag texId faceId z _) =
        Just (p, gx, gy, slotTag, texId, faceId, z)
    setPayloadFor _ _ _ = Nothing

-- | Requirement 2: the retraction names ONE attempt, not a key and not a
--   payload. Every example here stages two placements that agree on
--   every byte the command carries except the token.
attemptIdentitySpec ∷ SpecWith (EngineEnv, LuaBackendState)
attemptIdentitySpec = describe "attempt identity" $ do

    it "gives two identical placements at one key different tokens" $
        \(env, ls) → do
            _ ← resetScene env
            placeAt ls targetTile Nothing `shouldReturn` "true"
            cmdA ← takeOneCommand env
            placeAt ls targetTile Nothing `shouldReturn` "true"
            cmdB ← takeOneCommand env
            setPayload cmdA `shouldBe` setPayload cmdB
            setToken cmdA `shouldNotBe` setToken cmdB

    it "keeps the newer placement when the older one is declined" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            placeAt ls targetTile Nothing `shouldReturn` "true"
            cmdA ← takeOneCommand env
            placeAt ls targetTile Nothing `shouldReturn` "true"
            cmdB ← takeOneCommand env
            stageTokenAt wsMain targetKey `shouldReturn` setToken cmdB

            evictTargetChunk wsMain
            dispatch env cmdA
            allQueries ls targetTile `shouldReturn` placedQueries
            stageTokenAt wsMain targetKey `shouldReturn` setToken cmdB

            dispatch env cmdB
            allQueries ls targetTile `shouldReturn` emptyQueries
            stageKeys wsMain `shouldReturn` []

    it "never reissues a token a clearAll retired" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        placeAt ls targetTile Nothing `shouldReturn` "true"
        cmdA ← takeOneCommand env
        clearAll ls `shouldReturn` "ok"
        _ ← drainWorldQueue env   -- the WorldClearAllStructures it queues
        placeAt ls targetTile Nothing `shouldReturn` "true"
        cmdB ← takeOneCommand env
        setToken cmdA `shouldNotBe` setToken cmdB

        evictTargetChunk wsMain
        dispatch env cmdA
        allQueries ls targetTile `shouldReturn` placedQueries

-- | Requirements 3 and 5: the branches that were already correct.
commitSpec ∷ SpecWith (EngineEnv, LuaBackendState)
commitSpec = describe "a commit whose chunk is still loaded" $ do

    it "applies the overlay and appends exactly one WeSetStructure" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            placeAt ls targetTile Nothing `shouldReturn` "true"
            cmd ← takeOneCommand env
            dispatch env cmd
            structureEdits wsMain `shouldReturn`
                maybe [] (: []) (expectedSetEdit cmd)
            overlayAt wsMain `shouldReturn`
                Just (StructurePieceData (texIdOf cmd) (faceIdOf cmd) placeZ)
            allQueries ls targetTile `shouldReturn` placedQueries

    it "still records WeClearStructure when the chunk is NOT loaded" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            clearAt ls targetTile `shouldReturn` "true"
            cmd ← takeOneCommand env
            evictTargetChunk wsMain
            dispatch env cmd
            structureEdits wsMain `shouldReturn`
                [ WeClearStructure (fst targetTile) (snd targetTile)
                                   targetSlotTag ]
  where
    texIdOf (WorldSetStructure _ _ _ _ texId _ _ _) = texId
    texIdOf _ = -1
    faceIdOf (WorldSetStructure _ _ _ _ _ faceId _ _) = faceId
    faceIdOf _ = -1

-- | Requirement 4: the clear verbs' stage behaviour is untouched.
clearSpec ∷ SpecWith (EngineEnv, LuaBackendState)
clearSpec = describe "the clear verbs" $ do

    it "drops the staged entry at the cleared key immediately" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            placeAt ls targetTile Nothing `shouldReturn` "true"
            _ ← takeOneCommand env
            clearAt ls targetTile `shouldReturn` "true"
            _ ← takeOneCommand env
            stageKeys wsMain `shouldReturn` []
            allQueries ls targetTile `shouldReturn` emptyQueries

    it "drops every staged entry on clearAll" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        placeAt ls targetTile Nothing `shouldReturn` "true"
        _ ← takeOneCommand env
        clearAll ls `shouldReturn` "ok"
        _ ← drainWorldQueue env
        stageKeys wsMain `shouldReturn` []
        allQueries ls targetTile `shouldReturn` emptyQueries

-- | Requirement 7: the retraction is per-world. The control entry on
--   'otherPage' shares the target's exact key, so a retraction that
--   reached across pages would delete it.
isolationSpec ∷ SpecWith (EngineEnv, LuaBackendState)
isolationSpec = describe "per-world isolation" $ do

    it "leaves an equal-key staged entry on another page alone" $
        \(env, ls) → do
            (wsMain, wsOther) ← resetScene env
            placeAt ls targetTile (Just otherPage) `shouldReturn` "true"
            _ ← takeOneCommand env
            placeAt ls targetTile (Just mainPage) `shouldReturn` "true"
            cmdMain ← takeOneCommand env
            stageKeys wsOther `shouldReturn` [targetKey]

            evictTargetChunk wsMain
            dispatch env cmdMain

            stageKeys wsMain  `shouldReturn` []
            stageKeys wsOther `shouldReturn` [targetKey]
            hasAt ls targetTile (Just mainPage)  `shouldReturn` "false"
            hasAt ls targetTile (Just otherPage) `shouldReturn` "true"
