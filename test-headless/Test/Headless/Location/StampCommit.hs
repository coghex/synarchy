{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "location stamp commit gating" (#2051): the durable stamped marker
--   records that a location's geometry COMMITTED, not that its builder's
--   placements were accepted.
--
--   @structure.place@ returns true as soon as the piece is staged and its
--   'WorldSetStructure' queued; the world thread checks chunk residency
--   AGAIN before committing, and an eviction in that window makes it
--   decline — retracting the staged entry (#1674), appending no edit and
--   sending nothing back. The Lua stamper had already read that true as
--   "materialized" and queued an unconditional marker, so a chunk whose
--   geometry never landed was durably recorded as complete and never
--   retried. #1719 gated the marker on the SYNCHRONOUS answer and put
--   this post-acceptance path explicitly out of scope.
--
--   Nothing here stubs the path it is testing. Every example drives the
--   REAL @structure.place@ / @structure.stageWatermark@ /
--   @world.markLocationStamped@ through a registered Lua API, dequeues
--   the commands those calls actually emitted ('Q.tryReadQueue'), mutates
--   chunk residency the way a load pass does, and dispatches through the
--   production 'handleWorldCommand'. The sibling
--   "Test.Headless.Location.Stamping" gate covers the Lua aggregation
--   layer with synchronous stubs, which is exactly why it cannot see this
--   race.
--
--   The engine is 'initializeEngineHeadless' with NO worker threads (the
--   "Test.Headless.World.StructureStage" shape), so a queued command
--   waits to be dequeued here rather than being raced away by a drainer,
--   and queue order is asserted rather than hoped for.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "location stamp commit"'@ (the issue's own
--   @--match "location stamp"@ runs it alongside the aggregation gate).
module Test.Headless.Location.StampCommit (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Types
    ( StructurePieceData(..), StructureSlot(..), StructureStage(..)
    , emptyChunkStructures )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Command.Types (WorldCommand(..))
import World.Edit.Types (WorldEdit(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (globalToChunk)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Thread.Command (handleWorldCommand)
import World.Tile.Types (WorldTileData(..))

-- * Fixture identity

-- | The page every stamp targets, plus a second live page so the
--   per-page token counter this gate reads is demonstrably not the only
--   one in the manager.
stampPage, otherPage ∷ WorldPageId
stampPage = WorldPageId "stamp_main"
otherPage = WorldPageId "stamp_other"

worldSizeChunks ∷ Int
worldSizeChunks = 8

-- | The anchor tile of the stamped location, and a tile in a DIFFERENT
--   chunk — the "the room straddles a chunk boundary" shape that makes a
--   mixed committed/declined result possible at all.
hostTile, neighborTile ∷ (Int, Int)
hostTile     = (3, 3)
neighborTile = (chunkSize + 3, 3)

hostChunk, neighborChunk ∷ ChunkCoord
hostChunk     = fst (globalToChunk (fst hostTile) (snd hostTile))
neighborChunk = fst (globalToChunk (fst neighborTile) (snd neighborTile))

slotName ∷ Text
slotName = "floor"

slotTag ∷ Word8
slotTag = fromIntegral (fromEnum SFloor)

keyOf ∷ (Int, Int) → (Int, Int, Word8)
keyOf (gx, gy) = (gx, gy, slotTag)

placeZ ∷ Int
placeZ = 4

texPath, facePath ∷ Text
texPath  = "assets/textures/structures/stamp_commit_floor.png"
facePath = "assets/textures/structures/stamp_commit_face.png"

-- * Terrain fixtures

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

tilesFrom ∷ [ChunkCoord] → WorldTileData
tilesFrom coords = WorldTileData
    { wtdChunks    = HM.fromList [ (c, flatChunkAt c) | c ← coords ]
    , wtdMaxChunks = max 1 (length coords)
    }

genParams ∷ WorldGenParams
genParams = defaultWorldGenParams { wgpWorldSize = worldSizeChunks }

-- * Scene

-- | Both chunks loaded on both pages, nothing stamped, queue drained.
resetScene ∷ EngineEnv → IO WorldState
resetScene env = do
    wsMain  ← emptyWorldState
    wsOther ← emptyWorldState
    forM_ [wsMain, wsOther] $ \ws → do
        writeIORef (wsTilesRef ws) (tilesFrom [hostChunk, neighborChunk])
        writeIORef (wsGenParamsRef ws) (Just genParams)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(stampPage, wsMain), (otherPage, wsOther)]
        , wmVisible = [stampPage] }
    _ ← drainWorldQueue env
    pure wsMain

-- | Set exactly which chunks a page has loaded, the way a load pass
--   would — it inserts and evicts in one atomic 'wsTilesRef' update,
--   which is why the race this gate covers exists.
setLoadedChunks ∷ WorldState → [ChunkCoord] → IO ()
setLoadedChunks ws coords = writeIORef (wsTilesRef ws) (tilesFrom coords)

-- * Queue + dispatch

drainWorldQueue ∷ EngineEnv → IO [WorldCommand]
drainWorldQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (worldQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

dispatch ∷ EngineEnv → WorldCommand → IO ()
dispatch env cmd = do
    lg ← readIORef (loggerRef env)
    handleWorldCommand env lg cmd

isSetCmd ∷ WorldCommand → Bool
isSetCmd (WorldSetStructure {}) = True
isSetCmd _                      = False

-- | The tile a queued placement names.
setTile ∷ WorldCommand → Maybe (Int, Int)
setTile (WorldSetStructure _ gx gy _ _ _ _ _) = Just (gx, gy)
setTile _ = Nothing

-- | The overlay entry a successful commit of this command must produce,
--   and the edit it must append — both DERIVED from the command, since
--   the texture palette is engine-global and accumulates across
--   examples, so the ids are whatever it interned.
expectedPiece ∷ WorldCommand → Maybe StructurePieceData
expectedPiece (WorldSetStructure _ _ _ _ texId faceId z _) =
    Just (StructurePieceData texId faceId z)
expectedPiece _ = Nothing

expectedEdit ∷ WorldCommand → Maybe WorldEdit
expectedEdit (WorldSetStructure _ gx gy tag texId faceId z _) =
    Just (WeSetStructure gx gy tag texId faceId z)
expectedEdit _ = Nothing

-- * Live-state readers

overlayAt ∷ WorldState → (Int, Int) → IO (Maybe StructurePieceData)
overlayAt ws tile = do
    td ← readIORef (wsTilesRef ws)
    let (coord, _) = globalToChunk (fst tile) (snd tile)
    pure $ HM.lookup coord (wtdChunks td) ⌦ HM.lookup (keyOf tile) . lcStructures

-- | Every structure-SET edit the page's log holds, in chunk order.
structureEdits ∷ WorldState → IO [WorldEdit]
structureEdits ws = do
    es ← readIORef (wsEditsRef ws)
    pure [ e | (_, edits) ← sortOn fst (HM.toList es), e ← edits, isSet e ]
  where
    isSet (WeSetStructure {}) = True
    isSet _                   = False

-- | Whether a chunk carries the durable completion marker (#424).
isStamped ∷ WorldState → ChunkCoord → IO Bool
isStamped ws coord = do
    mParams ← readIORef (wsGenParamsRef ws)
    pure $ maybe False (HS.member coord . wgpLocationStamped) mParams

-- | Declines the page has recorded but not yet had consumed (#2051).
declinedCount ∷ WorldState → IO Int
declinedCount ws = length . ssDeclined <$> readIORef (wsStructureStageRef ws)

-- | Drop the durable marker from a page's gen params. Used only to set
--   up the "same key, a different attempt" comparison; #424 is why no
--   player edit can reach this state.
clearStampedMarker ∷ WorldState → IO ()
clearStampedMarker ws = modifyIORef' (wsGenParamsRef ws) $
    fmap (\p → p { wgpLocationStamped = HS.empty })

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

-- | The REAL @structure.stageWatermark@ on the stamped page.
watermark ∷ LuaBackendState → IO Int
watermark ls = do
    raw ← evalDebug ls $ T.concat
        [ "return tostring(structure.stageWatermark('"
        , unWorldPageId stampPage, "'))" ]
    case reads (T.unpack raw) of
        [(n, "")] → pure n
        _         → fail $ "structure.stageWatermark returned " <> T.unpack raw

-- | The REAL @structure.place@ at a tile on the stamped page.
placeAt ∷ LuaBackendState → (Int, Int) → IO Text
placeAt ls (gx, gy) = evalDebug ls $ T.concat
    [ "return tostring(structure.place("
    , tshow gx, ", ", tshow gy, ", '", slotName, "', 11, 12, "
    , tshow placeZ, ", '", texPath, "', '", facePath, "', '"
    , unWorldPageId stampPage, "'))" ]

-- | The REAL @world.markLocationStamped@, carrying a commit window.
markWithWindow ∷ LuaBackendState → (Int, Int) → Int → Int → IO ()
markWithWindow ls (gx, gy) lo hi = void $ evalDebug ls $ T.concat
    [ "world.markLocationStamped(", tshow gx, ", ", tshow gy, ", '"
    , unWorldPageId stampPage, "', ", tshow lo, ", ", tshow hi
    , "); return 'ok'" ]

-- | The same verb with no window — the console shape, which must keep
--   marking unconditionally.
markWithoutWindow ∷ LuaBackendState → (Int, Int) → IO ()
markWithoutWindow ls (gx, gy) = void $ evalDebug ls $ T.concat
    [ "world.markLocationStamped(", tshow gx, ", ", tshow gy, ", '"
    , unWorldPageId stampPage, "'); return 'ok'" ]

-- | One whole stamp invocation, exactly as @scripts/location_stamper.lua@
--   runs it: read the watermark, issue the builder's placements, read it
--   again, and queue the marker only when every synchronous answer was
--   true (#1719's gate, unchanged). Returns those answers.
stampInvocation ∷ LuaBackendState → (Int, Int) → [(Int, Int)] → IO [Text]
stampInvocation ls anchor tiles = do
    lo      ← watermark ls
    results ← mapM (placeAt ls) tiles
    hi      ← watermark ls
    when (all (≡ "true") results) $ markWithWindow ls anchor lo hi
    pure results

-- * Spec

spec ∷ Spec
spec = describe "location stamp commit gating (#2051)" $ aroundAll setup $ do
    fixtureSpec
    declinedSpec
    mixedSpec
    retrySpec
    successSpec
    windowPrecisionSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer, so a scratch root established afterwards is too late.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadless
        ls ← newBareLuaBackend env
        act (env, ls)

-- | The discriminators really discriminate, so "unmarked" and "no piece"
--   are assertions rather than coincidences.
fixtureSpec ∷ SpecWith (EngineEnv, LuaBackendState)
fixtureSpec = describe "the fixture" $ do

    it "starts unstamped and empty, with the two tiles in distinct chunks" $
        \(env, _) → do
            ws ← resetScene env
            hostChunk `shouldNotBe` neighborChunk
            isStamped ws hostChunk `shouldReturn` False
            overlayAt ws hostTile  `shouldReturn` Nothing
            structureEdits ws      `shouldReturn` []

    it "reads a real, advancing per-page watermark off the live stage" $
        \(env, ls) → do
            _ ← resetScene env
            before ← watermark ls
            placeAt ls hostTile `shouldReturn` "true"
            watermark ls `shouldReturn` (before + 1)
            _ ← drainWorldQueue env
            pure ()

-- | Requirements 1 and 2, and the issue's headline acceptance: a stamp
--   whose placement is declined after queuing leaves no piece and no
--   marker.
declinedSpec ∷ SpecWith (EngineEnv, LuaBackendState)
declinedSpec = describe "a stamp declined after its placements were queued" $ do

    it "leaves no authoritative piece, no edit, and no completion marker" $
        \(env, ls) → do
            ws ← resetScene env
            results ← stampInvocation ls hostTile [hostTile]
            -- Every synchronous answer was true: this is exactly the
            -- state #1719's gate reads as a complete stamp.
            results `shouldBe` ["true"]

            cmds ← drainWorldQueue env
            length (filter isSetCmd cmds) `shouldBe` 1

            -- The load pass evicts the host chunk between the Lua
            -- residency check and the world thread's own.
            setLoadedChunks ws [neighborChunk]
            mapM_ (dispatch env) cmds

            overlayAt ws hostTile  `shouldReturn` Nothing
            structureEdits ws      `shouldReturn` []
            isStamped ws hostChunk `shouldReturn` False

    it "queues the marker even so — the Lua answer alone cannot see the \
       \decline" $ \(env, ls) → do
            ws ← resetScene env
            _ ← stampInvocation ls hostTile [hostTile]
            cmds ← drainWorldQueue env
            -- The marker IS queued (the synchronous gate passed) and it
            -- carries the window that lets the world thread refuse it.
            length [ () | WorldMarkLocationStamped _ _ _ (Just _) ← cmds ]
                `shouldBe` 1
            setLoadedChunks ws [neighborChunk]
            mapM_ (dispatch env) cmds
            isStamped ws hostChunk `shouldReturn` False

    it "consumes the decline it acted on, so the record does not grow" $
        \(env, ls) → do
            ws ← resetScene env
            _ ← stampInvocation ls hostTile [hostTile]
            cmds ← drainWorldQueue env
            setLoadedChunks ws [neighborChunk]
            mapM_ (dispatch env) cmds
            declinedCount ws `shouldReturn` 0

-- | The mixed-result case: one accepted placement dispatched while its
--   target is still loaded, one declined after its target evicts. The
--   committed piece and its edit stay — partial progress is deliberately
--   retained and safely overwritten by the retry — the declined one
--   leaves neither, and the host chunk is still unmarked.
mixedSpec ∷ SpecWith (EngineEnv, LuaBackendState)
mixedSpec = describe "a stamp with one committed and one declined placement" $

    it "keeps the committed piece and its edit, and withholds the marker" $
        \(env, ls) → do
            ws ← resetScene env
            results ← stampInvocation ls hostTile [neighborTile, hostTile]
            results `shouldBe` ["true", "true"]
            cmds ← drainWorldQueue env
            let sets = filter isSetCmd cmds
            length sets `shouldBe` 2
            map setTile sets `shouldBe` [Just neighborTile, Just hostTile]

            -- Only the HOST chunk evicts; the neighbor's placement
            -- commits in the same dispatch pass.
            setLoadedChunks ws [neighborChunk]
            mapM_ (dispatch env) cmds

            case sets of
                [committed, _declined] → do
                    overlayAt ws neighborTile
                        `shouldReturn` expectedPiece committed
                    structureEdits ws
                        `shouldReturn` maybe [] (: []) (expectedEdit committed)
                _ → expectationFailure "expected two queued placements"
            overlayAt ws hostTile  `shouldReturn` Nothing
            isStamped ws hostChunk `shouldReturn` False

-- | Requirement 2's second half: the retry commits and marks exactly
--   once, and the marker lands only after every placement it names is in
--   BOTH the overlay and the edit log.
retrySpec ∷ SpecWith (EngineEnv, LuaBackendState)
retrySpec = describe "the retry after a declined stamp" $

    it "commits every placement and marks exactly once, in that order" $
        \(env, ls) → do
            ws ← resetScene env
            -- Attempt 1: declined.
            _ ← stampInvocation ls hostTile [hostTile]
            cmds1 ← drainWorldQueue env
            setLoadedChunks ws [neighborChunk]
            mapM_ (dispatch env) cmds1
            isStamped ws hostChunk `shouldReturn` False

            -- Attempt 2: the every-load dispatch retries with residency
            -- restored, on a fresh window above the retired one.
            setLoadedChunks ws [hostChunk, neighborChunk]
            _ ← stampInvocation ls hostTile [hostTile]
            cmds2 ← drainWorldQueue env
            -- FIFO: every placement precedes the marker on the queue.
            case span isSetCmd cmds2 of
                ([theSet], [marker]) → do
                    mapM_ (dispatch env) [theSet]
                    let edits = maybe [] (: []) (expectedEdit theSet)
                    overlayAt ws hostTile  `shouldReturn` expectedPiece theSet
                    structureEdits ws      `shouldReturn` edits
                    isStamped ws hostChunk `shouldReturn` False

                    dispatch env marker
                    isStamped ws hostChunk `shouldReturn` True

                    -- Exactly once: a repeat dispatch of the same marker
                    -- changes nothing, and adds no second piece or edit.
                    dispatch env marker
                    isStamped ws hostChunk `shouldReturn` True
                    structureEdits ws      `shouldReturn` edits
                other → expectationFailure $
                    "expected one placement then one marker, got " <> show other

-- | Requirement 5: nothing changes for a clean stamp, and the
--   window-less console verb keeps its old unconditional behaviour.
successSpec ∷ SpecWith (EngineEnv, LuaBackendState)
successSpec = describe "a stamp with no eviction" $ do

    it "commits every piece and marks the host chunk once" $
        \(env, ls) → do
            ws ← resetScene env
            results ← stampInvocation ls hostTile [hostTile, neighborTile]
            results `shouldBe` ["true", "true"]
            cmds ← drainWorldQueue env
            mapM_ (dispatch env) cmds
            case filter isSetCmd cmds of
                [hostSet, neighborSet] → do
                    overlayAt ws hostTile
                        `shouldReturn` expectedPiece hostSet
                    overlayAt ws neighborTile
                        `shouldReturn` expectedPiece neighborSet
                _ → expectationFailure "expected two queued placements"
            structureEdits ws ⌦ \es → length es `shouldBe` 2
            isStamped ws hostChunk `shouldReturn` True
            declinedCount ws `shouldReturn` 0

    it "still marks unconditionally when no window is supplied" $
        \(env, ls) → do
            ws ← resetScene env
            -- A declined placement is on record …
            _ ← placeAt ls hostTile
            cmds ← drainWorldQueue env
            setLoadedChunks ws [neighborChunk]
            mapM_ (dispatch env) cmds
            declinedCount ws `shouldReturn` 1
            -- … and the window-less verb marks anyway, exactly as it has
            -- since #424.
            markWithoutWindow ls hostTile
            drainWorldQueue env ⌦ mapM_ (dispatch env)
            isStamped ws hostChunk `shouldReturn` True

-- | The window names THIS invocation's attempts and no others.
windowPrecisionSpec ∷ SpecWith (EngineEnv, LuaBackendState)
windowPrecisionSpec = describe "the commit window" $ do

    it "ignores a decline that happened before it opened" $
        \(env, ls) → do
            ws ← resetScene env
            -- An earlier, unrelated placement is declined.
            _ ← placeAt ls hostTile
            earlier ← drainWorldQueue env
            setLoadedChunks ws [neighborChunk]
            mapM_ (dispatch env) earlier
            declinedCount ws `shouldReturn` 1

            -- A later stamp on a restored chunk commits cleanly. Its
            -- window opens above that retired attempt, so the earlier
            -- decline is not its verdict.
            setLoadedChunks ws [hostChunk, neighborChunk]
            _ ← stampInvocation ls hostTile [hostTile]
            drainWorldQueue env ⌦ mapM_ (dispatch env)
            isStamped ws hostChunk `shouldReturn` True

    it "is not satisfied by an existing byte-identical piece at the same \
       \key" $ \(env, ls) → do
            ws ← resetScene env
            -- A first stamp commits a piece at the host tile.
            _ ← stampInvocation ls hostTile [hostTile]
            drainWorldQueue env ⌦ mapM_ (dispatch env)
            isStamped ws hostChunk `shouldReturn` True
            committed ← overlayAt ws hostTile
            committed `shouldNotBe` Nothing

            before ← structureEdits ws
            length before `shouldBe` 1

            -- Clear the marker so the next stamp is judged on its own
            -- attempts, then run one whose byte-identical placement is
            -- declined. The first commit's edit is still on record — a
            -- completion check that asked "is there a WeSetStructure for
            -- this key?" would pass on it. Identity is what decides.
            clearStampedMarker ws
            _ ← stampInvocation ls hostTile [hostTile]
            cmds ← drainWorldQueue env
            setLoadedChunks ws [neighborChunk]
            mapM_ (dispatch env) cmds
            structureEdits ws      `shouldReturn` before
            isStamped ws hostChunk `shouldReturn` False
