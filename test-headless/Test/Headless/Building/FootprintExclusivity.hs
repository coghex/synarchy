{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "Building footprint exclusivity" (#2326): the tiles a building is
--   admitted on belong to it from the moment it is admitted until the
--   moment it commits or is dropped.
--
--   @building.spawn@ validates against a manager snapshot and QUEUES;
--   the insertion happens later, on another thread. Before #2326 that
--   window admitted two requests onto the same footprint — both read the
--   same free tiles, both were told yes, and
--   'Building.Thread.Command.applyBuildingSpawn' inserted both without
--   ever re-asking. These examples drive that window directly: every
--   competing pair is admitted against ONE pre-commit snapshot, with no
--   drain in between, exactly as two @building.spawn@ calls from one Lua
--   evaluation would be.
--
--   Both halves of the fix are pinned:
--
--     * __Admission takes the claim.__ The loser is refused
--       synchronously with the ordinary occupancy reason and consumes no
--       'Building.Types.BuildingId', so nothing is leaked and the caller
--       learns the placement failed instead of holding an id for a
--       building that never appears.
--     * __The commit verifies it.__ A spawn that reaches
--       'Building.Thread.Command.applyBuildingSpawn' holding no claim
--       inserts nothing — on the ordinary queue drain AND on #1602's
--       bound world-thread route, which calls the same body.
--
--   The comparison is the CANONICAL one (#1175) and page-scoped (#76):
--   a seam alias of a claimed tile conflicts, partial overlap between
--   two multi-tile footprints conflicts, and the same anchor on two
--   different pages does not.
--
--   The engine is this module's own ('initializeEngineHeadlessQuiet',
--   like "Test.Headless.Building.PageBinding"): it runs NO worker
--   threads, so an admitted spawn stays in its queue until an example
--   drains it deliberately, which is what makes "against one pre-commit
--   snapshot" a controlled state rather than a race with a drainer.
--   Neither page costs worldgen — both are in-memory 'emptyWorldState'
--   pages carrying synthetic flat chunks.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Building footprint exclusivity"'@.
module Test.Headless.Building.FootprintExclusivity (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)

import Building.Command.Types (BuildingCommand(..))
import Building.Schema
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), FootprintReservation(..), emptyBuildingManager )
import Building.Thread.Command (processAllBuildingCommands)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.Building (toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (toContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Command.Types (WorldCommand(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile, tileAliasStep)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState
    , settleSelectionProjection )
import World.Thread.Command (handleWorldCommand)
import World.Thread.Command.UI
    (handleWorldHideCommand, handleWorldShowCommand)
import World.Tile.Types (WorldTileData(..))

-- * Fixture identity

-- | The page every competing pair is admitted onto, and a second page
--   that exists only to prove the claim does not cross one (#76).
pageA, pageB ∷ WorldPageId
pageA = WorldPageId "foot_page_a"
pageB = WorldPageId "foot_page_b"

-- | The SAME world size on both pages, deliberately: it makes
--   'contestedTile' name the same canonical tile on each, so "these two
--   placements did not conflict" can only be the page scoping and never
--   a coordinate that happened to differ.
worldSize ∷ Int
worldSize = 8

-- | The tile every uncontested and contested placement anchors at. Well
--   inside the loaded chunk, so nothing here is ever refused for terrain.
contestedTile ∷ (Int, Int)
contestedTile = (4, 4)

-- | An anchor one tile diagonally on from 'contestedTile': a 1x1 there
--   is free, but a 2x2 there overlaps a 2x2 at 'contestedTile' in
--   exactly one tile — the partial overlap an anchor comparison cannot
--   see.
diagonalTile ∷ (Int, Int)
diagonalTile = (5, 5)

-- | A tile whose footprint touches neither of the two above.
farTile ∷ (Int, Int)
farTile = (12, 12)

-- | An anchor in the LAST canonical chunk column, so a 2x2 there steps
--   off the end of the canonical u range: its far column lands in a
--   chunk that is stored under a different coordinate entirely. This is
--   the case a raw rectangle comparison cannot see — the anchor is
--   canonical, and only the tiles derived from it wrap.
seamAnchor ∷ (Int, Int)
seamAnchor = (63, 0)

-- | The canonical image of 'seamAnchor'\'s far column, proven as a
--   fixture precondition below. A 1x1 here and a 2x2 at 'seamAnchor'
--   are the same physical tile, at coordinates that share no digit.
seamOverflow ∷ (Int, Int)
seamOverflow = (0, 64)

-- | The u-alias of a tile in this world's frame (#1175). Canonicalising
--   it lands back on the original tile, which the fixture precondition
--   below proves rather than assumes.
aliasOf ∷ (Int, Int) → (Int, Int)
aliasOf (gx, gy) = (gx + tileAliasStep worldSize, gy - tileAliasStep worldSize)

-- * Building definitions

smallName, wideName ∷ Text
smallName = "foot_small"   -- 1x1
wideName  = "foot_wide"    -- 2x2

mkDef ∷ Text → Int → Int → BuildingDef
mkDef name w h = BuildingDef
    { bdName            = name
    , bdDisplayName     = name
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTextures        = legacyAssets (TextureHandle 0)
    , bdIconTexture     = TextureHandle 0
    , bdTileW           = w
    , bdTileH           = h
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = False
    , bdRace            = "acolyte"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdRoleAnims       = Map.empty
    , bdVisualClass     = FreestandingInstallation
    , bdPowerDrain      = 0
    , bdPowerNode       = Nothing
    }

-- * Terrain

flatChunkAt ∷ ChunkCoord → LoadedChunk
flatChunkAt coord =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
            { ctStartZ = 0, ctMats = VU.singleton 1
            , ctSlopes = VU.singleton 0, ctVeg = VU.singleton 0 }
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

-- | Chunks (0,0) and (1,0) carry every ordinary fixture tile; (3,0) and
--   (0,4) are the pair 'seamAnchor'\'s 2x2 straddles once its far column
--   is resolved into the stored frame.
flatTiles ∷ WorldTileData
flatTiles =
    let chunks = [ flatChunkAt (ChunkCoord 0 0), flatChunkAt (ChunkCoord 1 0)
                 , flatChunkAt (ChunkCoord 3 0), flatChunkAt (ChunkCoord 0 4) ]
    in WorldTileData
        { wtdChunks    = HM.fromList [ (lcCoord c, c) | c ← chunks ]
        , wtdMaxChunks = length chunks }

-- * Scene

-- | Both pages loaded and page A visible, both carrying the same flat
--   terrain and the same two defs, and the building manager EMPTY —
--   every occupied tile below is one an example itself admitted.
resetScene ∷ EngineEnv → IO ()
resetScene env = do
    wsA ← emptyWorldState
    wsB ← emptyWorldState
    forM_ [wsA, wsB] $ \ws → do
        writeIORef (wsTilesRef ws) flatTiles
        writeIORef (wsGenParamsRef ws) $ Just defaultWorldGenParams
            { wgpWorldSize = worldSize }
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageA, wsA), (pageB, wsB)]
        , wmVisible = [pageA] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList
            [ (smallName, mkDef smallName 1 1)
            , (wideName,  mkDef wideName  2 2) ] }
    _ ← drainBuildingQueue env
    _ ← drainWorldQueue env
    pure ()

-- * Queue plumbing

drainBuildingQueue ∷ EngineEnv → IO [BuildingCommand]
drainBuildingQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (buildingQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

drainWorldQueue ∷ EngineEnv → IO [WorldCommand]
drainWorldQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (worldQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

-- | Run the REAL building-command drain — the same
--   'processAllBuildingCommands' the unit thread runs. Nothing is
--   reimplemented, so what lands is what the engine would land.
applyQueuedBuildings ∷ EngineEnv → IO ()
applyQueuedBuildings env =
    processAllBuildingCommands (loggerRef env)
        (toWorldSimCapability env)
        (toContentRegistriesViewCapability env)
        (toBuildingCapability env)

-- | Run the world queue through the REAL world-thread dispatcher, which
--   is where a page-BOUND placement is both checked and inserted
--   (#1602).
runWorldQueue ∷ EngineEnv → IO ()
runWorldQueue env = do
    cmds ← drainWorldQueue env
    logger ← readIORef (loggerRef env)
    forM_ cmds $ \cmd → do
        handleWorldCommand env logger cmd
        atomicModifyIORef' (worldManagerRef env) $ \mgr →
            (settleSelectionProjection mgr, ())

-- | Everything that actually COMMITTED, as (defName, anchor, page).
--   Both dispatchers run first, so this answers "did it land", never
--   "was it enqueued".
placed ∷ EngineEnv → IO [(Text, (Int, Int), WorldPageId)]
placed env = do
    runWorldQueue env
    applyQueuedBuildings env
    bm ← readIORef (buildingManagerRef env)
    pure [ (biDefName b, (biAnchorX b, biAnchorY b), biPage b)
         | b ← HM.elems (bmInstances bm) ]

-- | The manager's id allocator. A refused request must not move it.
nextId ∷ EngineEnv → IO Word32
nextId env = bmNextId <$> readIORef (buildingManagerRef env)

-- | How many footprint claims are outstanding. Zero after every drain
--   is what makes the collection consumable rather than merely bounded.
outstandingClaims ∷ EngineEnv → IO Int
outstandingClaims env =
    HM.size ∘ bmReservations <$> readIORef (buildingManagerRef env)

selectionGen ∷ EngineEnv → IO Word64
selectionGen env = wmSelectionGen <$> readIORef (worldManagerRef env)

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    loaded ← executeDebugLua (lbsLuaState ls) formatterLua
    loaded `shouldNotSatisfy` T.isInfixOf "error"
    pure ls

-- | @__bs(id, reason)@ folds @building.spawn@'s two return values into
--   one string — the debug console reports only the first.
formatterLua ∷ Text
formatterLua = T.concat
    [ "_G.__bs = function(a, b) "
    , "  if a == nil then return 'nil|' .. tostring(b) end; "
    , "  return 'id|' .. tostring(a); "
    , "end; return 'ok'" ]

-- | Debug-console return values arrive JSON-encoded, so a Lua string
--   comes back quoted.
q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | @building.spawn(def, gx, gy, page [, bindGen])@ through the
--   registered production API, folded to @id|N@ or @nil|reason@.
spawn ∷ LuaBackendState → Text → (Int, Int) → WorldPageId → Maybe Word64
      → IO Text
spawn ls defName (gx, gy) (WorldPageId pg) mGen =
    executeDebugLua (lbsLuaState ls) $ T.concat
        [ "return _G.__bs(building.spawn('", defName, "', "
        , tshow gx, ", ", tshow gy, ", '", pg, "'"
        , maybe "" (\g → ", " <> tshow g) mGen
        , "))" ]

-- * Spec

spec ∷ Spec
spec = describe "Building footprint exclusivity (#2326)" $ aroundAll setup $ do
    fixtureSpec
    admissionSpec
    commitSpec
    releaseSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer, so a scratch root established afterwards is already late.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        act (env, ls)

-- | The discriminators every assertion below leans on, proven rather
--   than assumed — a fixture whose alias is not an alias, or whose two
--   pages disagree about a coordinate, would let the real examples pass
--   for the wrong reason.
fixtureSpec ∷ SpecWith (EngineEnv, LuaBackendState)
fixtureSpec = describe "fixture preconditions" $ do

    it "the alias tile is a genuine alias of the contested tile" $ \_ → do
        aliasOf contestedTile `shouldNotBe` contestedTile
        uncurry (canonicalTile worldSize) (aliasOf contestedTile)
            `shouldBe` contestedTile

    it "a 2x2 at the diagonal tile overlaps a 2x2 at the contested one \
       \in exactly one tile, and their anchors differ" $ \_ → do
        let rect (ax, ay) = [ (x, y) | x ← [ax, ax + 1], y ← [ay, ay + 1] ]
            shared = [ t | t ← rect contestedTile, t `elem` rect diagonalTile ]
        diagonalTile `shouldNotBe` contestedTile
        shared `shouldBe` [diagonalTile]

    it "a 2x2 at the seam anchor covers the overflow tile ONLY once its \
       \tiles are canonicalised" $ \_ → do
        let raw = [ (x, y) | x ← [fst seamAnchor, fst seamAnchor + 1]
                           , y ← [snd seamAnchor, snd seamAnchor + 1] ]
            canon = map (uncurry (canonicalTile worldSize)) raw
        -- The anchor itself is already canonical, so nothing here is
        -- explained by the anchor resolution `building.spawn` does.
        uncurry (canonicalTile worldSize) seamAnchor `shouldBe` seamAnchor
        uncurry (canonicalTile worldSize) seamOverflow `shouldBe` seamOverflow
        seamOverflow `shouldNotSatisfy` (`elem` raw)
        seamOverflow `shouldSatisfy` (`elem` canon)

    it "the far tile's 2x2 touches neither" $ \_ → do
        let rect (ax, ay) = [ (x, y) | x ← [ax, ax + 1], y ← [ay, ay + 1] ]
        filter (`elem` rect contestedTile) (rect farTile) `shouldBe` []
        filter (`elem` rect diagonalTile) (rect farTile) `shouldBe` []

-- | The admission half: one claim per footprint, taken with the id.
admissionSpec ∷ SpecWith (EngineEnv, LuaBackendState)
admissionSpec = describe "admission claims the footprint" $ do

    it "admits an uncontested placement exactly as before" $ \(env, ls) → do
        resetScene env
        spawn ls smallName contestedTile pageA Nothing `shouldReturn` q "id|1"
        placed env `shouldReturn` [(smallName, contestedTile, pageA)]
        outstandingClaims env `shouldReturn` 0

    it "refuses a second spawn admitted against the same pre-commit \
       \snapshot, and commits exactly one" $ \(env, ls) → do
        resetScene env
        -- No drain between the two: both read a manager in which
        -- neither has committed, which is the window #2326 is about.
        first ← spawn ls smallName contestedTile pageA Nothing
        second ← spawn ls smallName contestedTile pageA Nothing
        first `shouldBe` q "id|1"
        second `shouldBe` q "nil|tile already occupied"
        placed env `shouldReturn` [(smallName, contestedTile, pageA)]

    it "consumes no building id for the request that lost" $ \(env, ls) → do
        resetScene env
        before ← nextId env
        _ ← spawn ls smallName contestedTile pageA Nothing
        won ← nextId env
        _ ← spawn ls smallName contestedTile pageA Nothing
        after ← nextId env
        -- Exactly one id spent between them: the loser leaked nothing.
        (won - before, after - won) `shouldBe` (1, 0)

    it "conflicts on PARTIAL overlap between two multi-tile footprints" $
        \(env, ls) → do
            resetScene env
            spawn ls wideName contestedTile pageA Nothing
                `shouldReturn` q "id|1"
            -- Different anchor, one shared tile. An anchor comparison
            -- would admit this.
            spawn ls wideName diagonalTile pageA Nothing
                `shouldReturn` q "nil|tile already occupied"
            placed env `shouldReturn` [(wideName, contestedTile, pageA)]

    it "conflicts with a SEAM ALIAS of the claimed anchor" $ \(env, ls) → do
        resetScene env
        spawn ls smallName contestedTile pageA Nothing `shouldReturn` q "id|1"
        spawn ls smallName (aliasOf contestedTile) pageA Nothing
            `shouldReturn` q "nil|tile already occupied"
        placed env `shouldReturn` [(smallName, contestedTile, pageA)]

    it "conflicts on a footprint tile that STEPS ACROSS the seam" $
        \(env, ls) → do
            resetScene env
            -- The 1x1 claims the overflow tile at its canonical
            -- coordinates; the 2x2 reaches it only through its far
            -- column, which is outside the canonical u range as written.
            -- Nothing is drained between them, so the committed-instance
            -- check cannot be what refuses the second.
            spawn ls smallName seamOverflow pageA Nothing
                `shouldReturn` q "id|1"
            spawn ls wideName seamAnchor pageA Nothing
                `shouldReturn` q "nil|tile already occupied"
            placed env `shouldReturn` [(smallName, seamOverflow, pageA)]

    it "does NOT conflict across pages at the identical anchor" $
        \(env, ls) → do
            resetScene env
            spawn ls smallName contestedTile pageA Nothing
                `shouldReturn` q "id|1"
            spawn ls smallName contestedTile pageB Nothing
                `shouldReturn` q "id|2"
            rows ← placed env
            rows `shouldMatchList`
                [ (smallName, contestedTile, pageA)
                , (smallName, contestedTile, pageB) ]

    it "leaves a non-overlapping placement entirely alone" $ \(env, ls) → do
        resetScene env
        spawn ls wideName contestedTile pageA Nothing `shouldReturn` q "id|1"
        spawn ls wideName farTile pageA Nothing `shouldReturn` q "id|2"
        rows ← placed env
        rows `shouldMatchList`
            [ (wideName, contestedTile, pageA), (wideName, farTile, pageA) ]

    it "applies the same claim on #1602's BOUND world-thread route" $
        \(env, ls) → do
            resetScene env
            gen ← selectionGen env
            first ← spawn ls smallName contestedTile pageA (Just gen)
            second ← spawn ls smallName contestedTile pageA (Just gen)
            first `shouldBe` q "id|1"
            second `shouldBe` q "nil|tile already occupied"
            -- Nothing reached the building queue: a bound placement is
            -- inserted by the world thread itself.
            runWorldQueue env
            leftover ← drainBuildingQueue env
            map show leftover `shouldBe` []
            rows ← placed env
            rows `shouldBe` [(smallName, contestedTile, pageA)]

    it "one route's claim blocks the other's admission" $ \(env, ls) → do
        resetScene env
        gen ← selectionGen env
        -- Bound first, unbound second, neither committed yet.
        spawn ls smallName contestedTile pageA (Just gen)
            `shouldReturn` q "id|1"
        spawn ls smallName contestedTile pageA Nothing
            `shouldReturn` q "nil|tile already occupied"
        placed env `shouldReturn` [(smallName, contestedTile, pageA)]

-- | The commit half: a spawn that holds no claim inserts nothing, on
--   either route into 'Building.Thread.Command.applyBuildingSpawn'.
commitSpec ∷ SpecWith (EngineEnv, LuaBackendState)
commitSpec = describe "the commit verifies the claim" $ do

    it "inserts nothing for an unclaimed spawn on the queue drain" $
        \(env, _) → do
            resetScene env
            Q.writeQueue (buildingQueue env) $
                BuildingSpawn (BuildingId 99) smallName
                    (fst contestedTile) (snd contestedTile) 0 pageA
            placed env `shouldReturn` []

    it "inserts nothing for an unclaimed spawn on the bound route" $
        \(env, _) → do
            resetScene env
            gen ← selectionGen env
            Q.writeQueue (worldQueue env) $
                WorldSpawnBoundBuilding (BuildingId 99) smallName
                    (fst contestedTile) (snd contestedTile) 0 pageA gen
            placed env `shouldReturn` []

    it "inserts nothing when the claim names a DIFFERENT anchor" $
        \(env, ls) → do
            resetScene env
            spawn ls smallName contestedTile pageA Nothing
                `shouldReturn` q "id|1"
            -- The claim exists, but for other tiles: replaying its id at
            -- the far tile must not ride in on it.
            _ ← drainBuildingQueue env
            Q.writeQueue (buildingQueue env) $
                BuildingSpawn (BuildingId 1) smallName
                    (fst farTile) (snd farTile) 0 pageA
            placed env `shouldReturn` []

    it "inserts nothing when the claim names a DIFFERENT page" $
        \(env, ls) → do
            resetScene env
            spawn ls smallName contestedTile pageA Nothing
                `shouldReturn` q "id|1"
            _ ← drainBuildingQueue env
            Q.writeQueue (buildingQueue env) $
                BuildingSpawn (BuildingId 1) smallName
                    (fst contestedTile) (snd contestedTile) 0 pageB
            placed env `shouldReturn` []

    it "cannot RESURRECT a demolished building by replaying its \
       \already-committed spawn" $ \(env, ls) → do
            resetScene env
            spawn ls smallName contestedTile pageA Nothing
                `shouldReturn` q "id|1"
            queued ← drainBuildingQueue env
            length queued `shouldBe` 1
            -- Commit it, then demolish it. `bmInstances` is keyed by
            -- BuildingId, so a replay at the SAME anchor while the
            -- building is still live would be invisible — the
            -- demolition is what makes a second insert observable.
            forM_ queued (Q.writeQueue (buildingQueue env))
            placed env `shouldReturn` [(smallName, contestedTile, pageA)]
            Q.writeQueue (buildingQueue env) $ BuildingDestroy (BuildingId 1)
            placed env `shouldReturn` []
            -- The claim was consumed by that first commit, so replaying
            -- the very command it produced brings nothing back.
            forM_ queued (Q.writeQueue (buildingQueue env))
            placed env `shouldReturn` []

-- | Every non-committing outcome retires the claim, so a request that
--   will never land cannot hold tiles against the next one.
releaseSpec ∷ SpecWith (EngineEnv, LuaBackendState)
releaseSpec = describe "a dropped request releases its claim" $ do

    it "releases when the spawn's world is gone (#58)" $ \(env, ls) → do
        resetScene env
        spawn ls smallName contestedTile pageA Nothing `shouldReturn` q "id|1"
        outstandingClaims env `shouldReturn` 1
        -- world.destroyAll's effect: the page leaves wmWorlds while the
        -- spawn is still queued.
        atomicModifyIORef' (worldManagerRef env) $ \mgr →
            (mgr { wmWorlds = [], wmVisible = [] }, ())
        applyQueuedBuildings env
        outstandingClaims env `shouldReturn` 0

    it "releases when the def is unknown" $ \(env, _) → do
        resetScene env
        -- Reach the unknown-def arm with a claim outstanding: take one
        -- through the real admission, then rename the def out from
        -- under the queued command.
        Q.writeQueue (buildingQueue env) $
            BuildingSpawn (BuildingId 1) "no_such_def"
                (fst contestedTile) (snd contestedTile) 0 pageA
        atomicModifyIORef' (buildingManagerRef env) $ \bm →
            ( bm { bmReservations = HM.singleton (BuildingId 1) $
                    reservationAt pageA contestedTile }, () )
        applyQueuedBuildings env
        outstandingClaims env `shouldReturn` 0

    it "releases when a BOUND placement's binding went stale (#1602)" $
        \(env, ls) → do
            resetScene env
            gen ← selectionGen env
            spawn ls smallName contestedTile pageA (Just gen)
                `shouldReturn` q "id|1"
            logger ← readIORef (loggerRef env)
            let wsc = toWorldSimCapability env
            -- Selection moves AFTER the command was enqueued.
            handleWorldHideCommand wsc logger pageA
            handleWorldShowCommand wsc logger pageB
            runWorldQueue env
            placed env `shouldReturn` []
            outstandingClaims env `shouldReturn` 0
            -- And the tiles are genuinely free again for the next click.
            spawn ls smallName contestedTile pageA Nothing
                `shouldReturn` q "id|2"

    it "retires a MATCHING claim even when the commit is refused" $
        \(env, ls) → do
            resetScene env
            spawn ls smallName contestedTile pageA Nothing
                `shouldReturn` q "id|1"
            -- Something else takes the tiles before the claim's own
            -- command is drained. The commit refuses — and must not
            -- leave the claim behind holding tiles forever.
            forceInstanceAt env (BuildingId 50) pageA contestedTile
            rows ← placed env
            rows `shouldBe` [(smallName, contestedTile, pageA)]
            HM.keys ∘ bmInstances <$> readIORef (buildingManagerRef env)
                `shouldReturn` [BuildingId 50]
            outstandingClaims env `shouldReturn` 0

    it "leaves a NON-matching claim alone when a mis-addressed command \
       \is refused" $ \(env, ls) → do
            resetScene env
            spawn ls smallName contestedTile pageA Nothing
                `shouldReturn` q "id|1"
            real ← drainBuildingQueue env
            -- Replay that id at the WRONG anchor: refused, and it must
            -- not cancel the claim the real command still depends on.
            Q.writeQueue (buildingQueue env) $
                BuildingSpawn (BuildingId 1) smallName
                    (fst farTile) (snd farTile) 0 pageA
            placed env `shouldReturn` []
            outstandingClaims env `shouldReturn` 1
            -- The real command still commits.
            forM_ real (Q.writeQueue (buildingQueue env))
            placed env `shouldReturn` [(smallName, contestedTile, pageA)]

    it "retires every claim on BuildingClearAll" $ \(env, ls) → do
        resetScene env
        spawn ls smallName contestedTile pageA Nothing `shouldReturn` q "id|1"
        spawn ls smallName farTile pageA Nothing `shouldReturn` q "id|2"
        _ ← drainBuildingQueue env
        outstandingClaims env `shouldReturn` 2
        Q.writeQueue (buildingQueue env) BuildingClearAll
        applyQueuedBuildings env
        outstandingClaims env `shouldReturn` 0

-- | Insert a committed instance straight into the manager, bypassing
--   the queue entirely — the only way to put a building on tiles a
--   claim is already outstanding for, which nothing in production can
--   do and which is exactly the state the commit-side occupancy test
--   exists to survive.
forceInstanceAt ∷ EngineEnv → BuildingId → WorldPageId → (Int, Int) → IO ()
forceInstanceAt env bid pid (gx, gy) =
    atomicModifyIORef' (buildingManagerRef env) $ \bm →
        ( bm { bmInstances = HM.insert bid BuildingInstance
                { biDefName = smallName, biPage = pid
                , biTexture = TextureHandle 0
                , biAnchorX = gx, biAnchorY = gy, biGridZ = 0
                , biSpawnedAt = 0, biTileW = 1, biTileH = 1
                , biSpawnRemaining = 0, biBuildProgress = 0
                , biMaterialsDelivered = HM.empty, biStorage = [] }
                (bmInstances bm) }, () )

-- | A claim shaped exactly as the admission transaction writes one, for
--   the arms that cannot be reached through @building.spawn@.
reservationAt ∷ WorldPageId → (Int, Int) → FootprintReservation
reservationAt pid (gx, gy) = FootprintReservation
    { frPage = pid, frAnchorX = gx, frAnchorY = gy
    , frTileW = 1, frTileH = 1 }
