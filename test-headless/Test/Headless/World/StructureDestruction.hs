{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Structure teardown PRESENTATION (#2491), live half: what the REAL
--   world-command handlers do when a piece is cleared.
--
--   Everything here goes through production paths. A piece is placed
--   with the real @structure.place@ and removed with the real
--   @structure.clear@ / @structure.clearAll@; the commands those calls
--   emit are dequeued and dispatched through
--   'World.Thread.Command.handleWorldCommand'; the queries are the real
--   Lua verbs; expiry is 'World.Thread.Destruction.pruneStructureDestructions',
--   the step the world tick runs; and the telemetry is read off a
--   completed 'World.Render.updateWorldTiles' pass. Nothing asserts a
--   value this module wrote itself.
--
--   The engine is the 'Test.Headless.World.StructureStage' shape: it
--   runs NO worker threads, so a queued command waits to be dequeued
--   here rather than being raced away by a drainer, and the game clock
--   only moves when an example moves it — which is precisely the state
--   a PAUSED session is in, since the unit thread's @+ dt@ is gated on
--   'wsEnginePausedRef'.
--
--   The pack is "Test.Headless.Structure.DestructionFixture"'s, written
--   straight into the catalogue every example, so the once-per-(pack,
--   appearance) report is a fresh question each time rather than one the
--   previous example already answered.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "structure destruction presentation lifecycle"'@
module Test.Headless.World.StructureDestruction (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort, sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Scene.Stats
    (SceneCategory(..), SceneCategoryStat(..), SceneStats(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.ArtCatalog
    ( AppearanceKey(..), AppearanceSlot(..), PackArt(..)
    , StructureArtCatalog(..), noteMissingDestruction )
import Structure.Destruction
import Structure.Types
    ( StructurePieceData(..), StructureSlot(..), emptyChunkStructures )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Command.Types (WorldCommand(..))
import World.Edit.Types (WorldEdit(..), emptyWorldEdits)
import World.Edit.Apply (applyEdit)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (globalToChunk, tileAliasStep)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Grid (gridToWorld)
import World.Page.Types (WorldPageId(..))
import World.Render (updateWorldTiles)
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Envelope
    (LuaComponentSpec(..), decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata)
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Thread.Command (handleWorldCommand)
import World.Thread.Destruction (pruneStructureDestructions)
import World.Tile.Types (WorldTileData(..))
import Item.Knowledge (emptyPortableKnowledge)

import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Render.StructureGhostFixture (handleForPath)
import Test.Headless.Structure.ConstructionFixture
    (fixturePack, fixtureWallCatalog, staticPathFor)
import Test.Headless.Structure.DestructionFixture
    (wreckCatalog, wreckFps, wreckFrameCount)

-- * Fixture identity

mainPage, hiddenPage ∷ WorldPageId
mainPage   = WorldPageId "teardown_main"
hiddenPage = WorldPageId "teardown_hidden"

worldSizeChunks ∷ Int
worldSizeChunks = 8

targetTile ∷ (Int, Int)
targetTile = (3, 3)

targetChunk ∷ ChunkCoord
targetChunk = fst (globalToChunk (fst targetTile) (snd targetTile))

placeZ ∷ Int
placeZ = 4

-- | The appearance every scenario tears down: the fixture pack's
--   default FLOOR, which declares a clip.
floorAppearance ∷ AppearanceKey
floorAppearance = AppearanceKey Nothing ApFloor

-- | …and the one that declares NONE, which is requirement 6's state.
ceilingAppearance ∷ AppearanceKey
ceilingAppearance = AppearanceKey Nothing ApCeiling

slotNameOf ∷ AppearanceKey → Text
slotNameOf ak = case apSlot ak of
    ApCeiling → "ceiling"
    _         → "floor"

slotOf ∷ AppearanceKey → StructureSlot
slotOf ak = case apSlot ak of
    ApCeiling → SCeiling
    _         → SFloor

slotTagOf ∷ AppearanceKey → Word8
slotTagOf = fromIntegral ∘ fromEnum ∘ slotOf

texPathOf ∷ AppearanceKey → Text
texPathOf = staticPathFor

facePathOf ∷ AppearanceKey → Text
facePathOf ak = case apSlot ak of
    ApCeiling → "fx/ceilingface.png"
    _         → "fx/floorface.png"

keyOf ∷ AppearanceKey → (Int, Int, Word8)
keyOf ak = (fst targetTile, snd targetTile, slotTagOf ak)

-- | The clip's full length in game seconds, from the fixture's own
--   declaration rather than from the effect under test.
clipDuration ∷ AppearanceKey → Double
clipDuration ak = case wreckFrameCount ak of
    Just n  → fromIntegral n / wreckFps ak
    Nothing → error "fixture appearance declares no clip"

aliasOf ∷ (Int, Int) → (Int, Int)
aliasOf (gx, gy) = (gx + tileAliasStep worldSizeChunks
                   , gy - tileAliasStep worldSizeChunks)

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

tilesFrom ∷ [LoadedChunk] → WorldTileData
tilesFrom chunks = WorldTileData
    { wtdChunks    = HM.fromList [ (lcCoord c, c) | c ← chunks ]
    , wtdMaxChunks = max 1 (length chunks) }

loadedTiles, evictedTiles ∷ WorldTileData
loadedTiles  = tilesFrom [flatChunkAt targetChunk]
evictedTiles = tilesFrom []

genParams ∷ WorldGenParams
genParams = defaultWorldGenParams { wgpWorldSize = worldSizeChunks }

-- * Scene

-- | Two pages installed with 'targetChunk' loaded, only 'mainPage'
--   visible, the fixture pack freshly in the catalogue, the clock at
--   zero and the world queue drained.
resetScene ∷ EngineEnv → IO (WorldState, WorldState)
resetScene env = do
    wsMain   ← emptyWorldState
    wsHidden ← emptyWorldState
    forM_ [wsMain, wsHidden] $ \ws → do
        writeIORef (wsTilesRef ws) loadedTiles
        writeIORef (wsGenParamsRef ws) (Just genParams)
        writeIORef (wsQuadCacheRef ws) Nothing
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(mainPage, wsMain), (hiddenPage, wsHidden)]
        , wmVisible = [mainPage] }
    -- A FRESH catalogue every example: 'pkMissingDestruction' is
    -- deliberately sticky, so a shared one would let one example answer
    -- another's "reported once" question.
    writeIORef (structureArtCatalogRef env) wreckCatalog
    writeIORef (structureWallCatalogRef env) fixtureWallCatalog
    writeIORef (texPaletteHandlesRef env) HM.empty
    writeIORef (gameTimeRef env) 0
    writeIORef (cameraRef env) (cameraAt FaceSouth targetTile)
    writeIORef (framebufferSizeRef env) (800, 600)
    writeIORef (windowSizeRef env) (800, 600)
    _ ← drainWorldQueue env
    pure (wsMain, wsHidden)

-- | The camera the telemetry examples run at: centred on the target
--   tile, at 'defaultCamera''s own GAMEPLAY zoom. Zoom matters — past
--   the zoom-map threshold every tile-space category is bypassed by its
--   activation guard and reports zero, so a zoomed-out camera would let
--   the effect counts pass by not being taken at all.
cameraAt ∷ CameraFacing → (Int, Int) → Camera2D
cameraAt facing (gx, gy) =
    let (wx, wy) = gridToWorld facing gx gy
    in defaultCamera { camPosition  = (wx, wy)
                     , camFacing    = facing
                     -- 'World.Grid.zoomFadeStart': at this zoom the
                     -- tile passes are fully opaque and the zoom map
                     -- sits behind its activation guard. Past it every
                     -- tile-space category — 'ScStructures' included —
                     -- reports zero without being taken at all, which
                     -- would let the effect counts below pass
                     -- vacuously.
                     , camZoom      = 1.2
                     , camZSlice    = placeZ
                     -- With tracking on, 'updateWorldTiles' rewrites
                     -- 'camZSlice' from the fixture's surface map, and
                     -- the piece's own z would no longer be in the
                     -- band.
                     , camZTracking = False }

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

-- | Place and clear one piece on a page that is NOT the active one.
--
--   @structure.place@ takes an explicit page, so the placement is the
--   real verb. @structure.clear@ does not — it always targets the
--   active page — so the removal is the command that verb queues,
--   built here and dispatched through the same production handler.
--   Unlike a @WorldSetStructure@ that would be wrong to rebuild (it
--   carries the attempt token #1674 matches on), a
--   @WorldClearStructure@ is page, canonical tile and slot and nothing
--   else, so this IS the command, not an equivalent of it.
placeAndClearOnPage ∷ EngineEnv → LuaBackendState → WorldPageId
                    → AppearanceKey → IO ()
placeAndClearOnPage env ls page ak = do
    _ ← evalDebug ls $ T.concat
        [ "return tostring(structure.place("
        , tshow (fst targetTile), ", ", tshow (snd targetTile)
        , ", '", slotNameOf ak, "', 11, 12, ", tshow placeZ
        , ", '", texPathOf ak, "', '", facePathOf ak
        , "', '", unWorldPageId page, "'))" ]
    _ ← settle env
    lg ← readIORef (loggerRef env)
    handleWorldCommand env lg
        (WorldClearStructure page (fst targetTile) (snd targetTile)
                             (slotTagOf ak))

-- | Dispatch every command the preceding Lua call emitted, in order,
--   through the REAL world-thread dispatcher.
settle ∷ EngineEnv → IO [WorldCommand]
settle env = do
    cmds ← drainWorldQueue env
    lg   ← readIORef (loggerRef env)
    forM_ cmds (handleWorldCommand env lg)
    pure cmds

-- * Live-state readers

effectsOf ∷ WorldState → IO StructureDestructions
effectsOf = readIORef ∘ wsStructureDestructionsRef

effectKeys ∷ WorldState → IO [(Int, Int, Word8)]
effectKeys ws = sort ∘ HM.keys <$> effectsOf ws

effectAt ∷ WorldState → AppearanceKey → IO (Maybe StructureDestructionEffect)
effectAt ws ak = HM.lookup (keyOf ak) <$> effectsOf ws

structureEdits ∷ WorldState → IO [WorldEdit]
structureEdits ws = do
    es ← readIORef (wsEditsRef ws)
    pure [ e | (_, edits) ← sortOn fst (HM.toList es)
             , e ← edits, isStructureEdit e ]
  where
    isStructureEdit (WeSetStructure {})   = True
    isStructureEdit (WeClearStructure {}) = True
    isStructureEdit _                     = False

overlayAt ∷ WorldState → AppearanceKey → IO (Maybe StructurePieceData)
overlayAt ws ak = do
    td ← readIORef (wsTilesRef ws)
    pure $ HM.lookup targetChunk (wtdChunks td)
             ⌦ HM.lookup (keyOf ak) ∘ lcStructures

reportedGaps ∷ EngineEnv → IO [AppearanceKey]
reportedGaps env = do
    cat ← readIORef (structureArtCatalogRef env)
    pure $ case HM.lookup fixturePack (sacPacks cat) of
        Nothing → []
        Just p  → toList (pkMissingDestruction p)
  where toList = foldr (:) []

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

placeAt ∷ LuaBackendState → AppearanceKey → (Int, Int) → IO Text
placeAt ls ak (gx, gy) = evalDebug ls $ T.concat
    [ "return tostring(structure.place("
    , tshow gx, ", ", tshow gy, ", '", slotNameOf ak, "', 11, 12, "
    , tshow placeZ, ", '", texPathOf ak, "', '", facePathOf ak, "'))" ]

clearAt ∷ LuaBackendState → AppearanceKey → (Int, Int) → IO Text
clearAt ls ak (gx, gy) = evalDebug ls $ T.concat
    [ "return tostring(structure.clear("
    , tshow gx, ", ", tshow gy, ", '", slotNameOf ak, "'))" ]

clearAllPieces ∷ LuaBackendState → IO Text
clearAllPieces ls = evalDebug ls "structure.clearAll(); return 'ok'"

hasAt ∷ LuaBackendState → AppearanceKey → IO Text
hasAt ls ak = evalDebug ls $ T.concat
    [ "return tostring(structure.hasAt("
    , tshow (fst targetTile), ", ", tshow (snd targetTile)
    , ", '", slotNameOf ak, "'))" ]

floorZAt ∷ LuaBackendState → IO Text
floorZAt ls = evalDebug ls $ T.concat
    [ "return tostring(structure.floorZAt("
    , tshow (fst targetTile), ", ", tshow (snd targetTile), "))" ]

getAtZ ∷ LuaBackendState → AppearanceKey → IO Text
getAtZ ls ak = evalDebug ls $ T.concat
    [ "local p = structure.getAt("
    , tshow (fst targetTile), ", ", tshow (snd targetTile)
    , ", '", slotNameOf ak
    , "'); if p == nil then return 'nil' end; return tostring(p.z)" ]

countAll ∷ LuaBackendState → IO Text
countAll ls = evalDebug ls "return tostring(structure.count())"

-- | Every occupancy query the issue names, as one tuple. Wall-cap and
--   Wire shape resolution are built out of exactly these
--   (@scripts/structures.lua@ counts neighbours with @structure.hasAt@),
--   so a piece absent here is absent to them too.
allQueries ∷ LuaBackendState → AppearanceKey → IO (Text, Text, Text, Text)
allQueries ls ak = (,,,)
    <$> hasAt ls ak
    <*> floorZAt ls
    <*> getAtZ ls ak
    <*> countAll ls

emptyQueries ∷ (Text, Text, Text, Text)
emptyQueries = ("false", "nil", "nil", "0")

placedQueries ∷ (Text, Text, Text, Text)
placedQueries = ("true", tshow placeZ, tshow placeZ, "1")

-- * A placed, then cleared, piece

-- | Place the appearance and commit it. Answers the palette ids the
--   engine interned, read off the command it emitted rather than
--   assumed (the palette is engine-global and accumulates).
placeAndCommit ∷ EngineEnv → LuaBackendState → AppearanceKey → (Int, Int)
               → IO (Int, Int)
placeAndCommit env ls ak tile = do
    placeAt ls ak tile `shouldReturn'` "true"
    cmds ← settle env
    case [ (texId, faceId)
         | WorldSetStructure _ _ _ _ texId faceId _ _ ← cmds ] of
        (ids : _) → pure ids
        []        → fail "structure.place emitted no WorldSetStructure"

clearAndCommit ∷ EngineEnv → LuaBackendState → AppearanceKey → (Int, Int)
               → IO ()
clearAndCommit env ls ak tile = do
    _ ← clearAt ls ak tile
    _ ← settle env
    pure ()

shouldReturn' ∷ (Show α, Eq α) ⇒ IO α → α → IO ()
shouldReturn' = shouldReturn

-- * Telemetry

statsOf ∷ EngineEnv → IO SceneStats
statsOf env = do
    _ ← updateWorldTiles env
    mStats ← readIORef (sceneStatsRef env)
    case mStats of
        Just stats → pure stats
        Nothing    → fail "updateWorldTiles published no scene stats"

rowFor ∷ SceneCategory → SceneStats → SceneCategoryStat
rowFor cat stats = case filter ((≡ cat) ∘ scsCategory) (ssCategories stats) of
    (row : _) → row
    []        → error ("no telemetry row for " <> show cat)

-- * Spec

spec ∷ Spec
spec = describe "structure destruction presentation lifecycle"
     $ aroundAll setup $ do
    captureSpec
    querySpec
    silentSpec
    bulkSpec
    expirySpec
    telemetrySpec
    persistenceSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer, so a scratch root established afterwards is too late.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        act (env, ls)

-- * Capture

captureSpec ∷ SpecWith (EngineEnv, LuaBackendState)
captureSpec = describe "clearing a placed piece" $ do

    it "removes the overlay entry, records the clear, and leaves the \
       \effect standing over a tile the piece has already left" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            _ ← placeAndCommit env ls floorAppearance targetTile
            overlayAt wsMain floorAppearance `shouldNotReturn'` Nothing
            effectKeys wsMain `shouldReturn` []

            clearAndCommit env ls floorAppearance targetTile
            -- The persistent authority landed…
            edits ← structureEdits wsMain
            edits `shouldSatisfy` any isClear
            -- …the live overlay entry is gone…
            overlayAt wsMain floorAppearance `shouldReturn` Nothing
            -- …and only then does anything about the piece survive.
            effectKeys wsMain `shouldReturn` [keyOf floorAppearance]

    it "captures the piece's former identity, clip and clock" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            (texId, faceId) ← placeAndCommit env ls floorAppearance targetTile
            writeIORef (gameTimeRef env) 42.5
            clearAndCommit env ls floorAppearance targetTile
            Just eff ← effectAt wsMain floorAppearance
            sdePage eff       `shouldBe` mainPage
            (sdeGX eff, sdeGY eff) `shouldBe` targetTile
            sdeSlotTag eff    `shouldBe` slotTagOf floorAppearance
            sdeGridZ eff      `shouldBe` placeZ
            sdeTexId eff      `shouldBe` texId
            sdeFaceId eff     `shouldBe` faceId
            sdeTexPath eff    `shouldBe` texPathOf floorAppearance
            sdeFacePath eff   `shouldBe` Just (facePathOf floorAppearance)
            sdePack eff       `shouldBe` fixturePack
            sdeAppearance eff `shouldBe` floorAppearance
            sdeFps eff        `shouldBe` wreckFps floorAppearance
            sdeStartedAt eff  `shouldBe` 42.5

    it "keys the effect at the CANONICAL tile, even when the clear names \
       \a u-alias" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        let alias = aliasOf targetTile
        alias `shouldNotBe` targetTile
        _ ← placeAndCommit env ls floorAppearance alias
        clearAndCommit env ls floorAppearance alias
        effectKeys wsMain `shouldReturn` [keyOf floorAppearance]

    it "records the handles when the palette map has them, and captures \
       \the effect anyway when it does not" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        (texId, faceId) ← placeAndCommit env ls floorAppearance targetTile
        writeIORef (texPaletteHandlesRef env) $ HM.fromList
            [ (texId,  handleForPath (texPathOf floorAppearance))
            , (faceId, handleForPath (facePathOf floorAppearance)) ]
        clearAndCommit env ls floorAppearance targetTile
        Just resolved ← effectAt wsMain floorAppearance
        sdeTexHandle resolved
            `shouldBe` Just (handleForPath (texPathOf floorAppearance))

        -- …and the state a post-load session is briefly in, where the
        -- palette's ids exist but nothing has resolved them yet.
        -- Emptied AFTER the placement, because @structure.place@ is
        -- itself one of the writers that fills the map.
        (wsAgain, _) ← resetScene env
        _ ← placeAndCommit env ls floorAppearance targetTile
        writeIORef (texPaletteHandlesRef env) HM.empty
        clearAndCommit env ls floorAppearance targetTile
        Just bare ← effectAt wsAgain floorAppearance
        sdeTexHandle bare  `shouldBe` Nothing
        sdeFaceHandle bare `shouldBe` Nothing
        sdeFrameCount bare `shouldBe` fromJustInt (wreckFrameCount floorAppearance)

-- * The queries

querySpec ∷ SpecWith (EngineEnv, LuaBackendState)
querySpec = describe "a piece whose effect is still playing" $ do

    it "reads as ABSENT from every occupancy query" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        allQueries ls floorAppearance `shouldReturn` emptyQueries
        _ ← placeAndCommit env ls floorAppearance targetTile
        allQueries ls floorAppearance `shouldReturn` placedQueries

        clearAndCommit env ls floorAppearance targetTile
        -- The effect exists…
        effectKeys wsMain `shouldReturn` [keyOf floorAppearance]
        -- …and changes nothing any query can see. Wall-cap and Wire
        -- shape resolution are built out of exactly these.
        allQueries ls floorAppearance `shouldReturn` emptyQueries

    it "captures nothing on a SECOND clear of the same slot" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            _ ← placeAndCommit env ls floorAppearance targetTile
            writeIORef (gameTimeRef env) 5
            clearAndCommit env ls floorAppearance targetTile
            Just first ← effectAt wsMain floorAppearance

            writeIORef (gameTimeRef env) 6
            clearAndCommit env ls floorAppearance targetTile
            Just again ← effectAt wsMain floorAppearance
            -- Not restarted, not duplicated: the slot was already empty,
            -- so there was nothing to capture.
            sdeStartedAt again `shouldBe` sdeStartedAt first
            effectKeys wsMain `shouldReturn` [keyOf floorAppearance]

    it "captures nothing when the chunk is NOT resident, while still \
       \recording the clear" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        _ ← placeAndCommit env ls floorAppearance targetTile
        evictTargetChunk wsMain
        clearAndCommit env ls floorAppearance targetTile
        structureEdits wsMain `shouldSatisfy'` any isClear
        effectKeys wsMain `shouldReturn` []

-- * The undeclared appearance

silentSpec ∷ SpecWith (EngineEnv, LuaBackendState)
silentSpec = describe "an appearance that declares no clip" $ do

    it "captures nothing, reports once, and stays silent on a repeat" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            reportedGaps env `shouldReturn` []

            _ ← placeAndCommit env ls ceilingAppearance targetTile
            clearAndCommit env ls ceilingAppearance targetTile
            effectKeys wsMain `shouldReturn` []
            reportedGaps env `shouldReturn` [ceilingAppearance]

            -- A second teardown of the same appearance says nothing more
            -- — which is what "once per (pack, appearance)" means for a
            -- player who can demolish the same kind of piece forever.
            _ ← placeAndCommit env ls ceilingAppearance targetTile
            clearAndCommit env ls ceilingAppearance targetTile
            reportedGaps env `shouldReturn` [ceilingAppearance]
            cat ← readIORef (structureArtCatalogRef env)
            snd (noteMissingDestruction fixturePack ceilingAppearance cat)
                `shouldBe` Nothing

    it "leaves the piece removed exactly as it always was" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            _ ← placeAndCommit env ls ceilingAppearance targetTile
            clearAndCommit env ls ceilingAppearance targetTile
            overlayAt wsMain ceilingAppearance `shouldReturn` Nothing
            allQueries ls ceilingAppearance `shouldReturn` emptyQueries

-- * Bulk paths

bulkSpec ∷ SpecWith (EngineEnv, LuaBackendState)
bulkSpec = describe "the bulk paths" $ do

    it "clearAll captures nothing and drops every pending effect" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            _ ← placeAndCommit env ls floorAppearance targetTile
            clearAndCommit env ls floorAppearance targetTile
            effectKeys wsMain `shouldReturn` [keyOf floorAppearance]

            -- A second piece is standing when the wipe lands, so the
            -- example covers both halves at once: the pending effect
            -- goes, and the live piece leaves none behind.
            _ ← placeAndCommit env ls ceilingAppearance targetTile
            _ ← clearAllPieces ls
            _ ← settle env
            effectKeys wsMain `shouldReturn` []
            overlayAt wsMain ceilingAppearance `shouldReturn` Nothing
            countAll ls `shouldReturn` "0"

    it "gives a freshly constructed page none, so a replaced session \
       \cannot inherit any" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        _ ← placeAndCommit env ls floorAppearance targetTile
        clearAndCommit env ls floorAppearance targetTile
        effectKeys wsMain `shouldReturn` [keyOf floorAppearance]

        -- The whole-session replacement shape: brand-new WorldStates
        -- installed under the same page ids, exactly as load publish
        -- does it.
        wsNew    ← emptyWorldState
        wsNewAlt ← emptyWorldState
        forM_ [wsNew, wsNewAlt] $ \ws → do
            writeIORef (wsTilesRef ws) loadedTiles
            writeIORef (wsGenParamsRef ws) (Just genParams)
        writeIORef (worldManagerRef env) emptyWorldManager
            { wmWorlds  = [(mainPage, wsNew), (hiddenPage, wsNewAlt)]
            , wmVisible = [mainPage] }
        effectKeys wsNew    `shouldReturn` []
        effectKeys wsNewAlt `shouldReturn` []

-- * Expiry

expirySpec ∷ SpecWith (EngineEnv, LuaBackendState)
expirySpec = describe "expiry" $ do

    it "retires the effect at the clip's end, with no texture system \
       \and no render pass in sight" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        _ ← placeAndCommit env ls floorAppearance targetTile
        clearAndCommit env ls floorAppearance targetTile
        let d = clipDuration floorAppearance

        writeIORef (gameTimeRef env) (d - 0.001)
        pruneStructureDestructions env
        effectKeys wsMain `shouldReturn` [keyOf floorAppearance]

        writeIORef (gameTimeRef env) d
        pruneStructureDestructions env
        effectKeys wsMain `shouldReturn` []

    it "retires an effect whose chunk was evicted after the capture" $
        \(env, ls) → do
            (wsMain, _) ← resetScene env
            _ ← placeAndCommit env ls floorAppearance targetTile
            clearAndCommit env ls floorAppearance targetTile
            evictTargetChunk wsMain
            effectKeys wsMain `shouldReturn` [keyOf floorAppearance]
            writeIORef (gameTimeRef env) (clipDuration floorAppearance)
            pruneStructureDestructions env
            effectKeys wsMain `shouldReturn` []

    it "retires an effect on a HIDDEN page on the same schedule" $
        \(env, ls) → do
            (_, wsHidden) ← resetScene env
            -- Torn down on the HIDDEN page, so the effect can only have
            -- been reached by a pass that walks every page.
            placeAndClearOnPage env ls hiddenPage floorAppearance
            effectKeys wsHidden `shouldReturn` [keyOf floorAppearance]
            writeIORef (gameTimeRef env) (clipDuration floorAppearance)
            pruneStructureDestructions env
            effectKeys wsHidden `shouldReturn` []

    it "keeps the effect frozen while the clock does not advance — a \
       \paused session's state" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        _ ← placeAndCommit env ls floorAppearance targetTile
        clearAndCommit env ls floorAppearance targetTile
        -- The unit thread's `+ dt` is gated on 'wsEnginePausedRef', so a
        -- paused session's clock reads the same value every tick. Many
        -- ticks at one reading must therefore change nothing at all.
        forM_ [1 .. 20 ∷ Int] $ \_ → pruneStructureDestructions env
        effectKeys wsMain `shouldReturn` [keyOf floorAppearance]
        Just eff ← effectAt wsMain floorAppearance
        destructionEffectExpired 0 eff `shouldBe` False

-- * Telemetry

telemetrySpec ∷ SpecWith (EngineEnv, LuaBackendState)
telemetrySpec = describe "ScStructures telemetry" $ do

    it "counts an effects-only pass, with no resident structure chunk \
       \and no texture system" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        _ ← placeAndCommit env ls floorAppearance targetTile
        clearAndCommit env ls floorAppearance targetTile
        evictTargetChunk wsMain
        stats ← statsOf env
        -- One effect examined; nothing drawn, because headless has no
        -- texture system. Before #2491 this pass reported an empty
        -- frame.
        scsScanned (rowFor ScStructures stats) `shouldBe` 1
        scsEmitted (rowFor ScStructures stats) `shouldBe` 0

    it "counts an effect on a HIDDEN page without drawing it" $
        \(env, ls) → do
            (_, wsHidden) ← resetScene env
            placeAndClearOnPage env ls hiddenPage floorAppearance
            effectKeys wsHidden `shouldReturn` [keyOf floorAppearance]
            stats ← statsOf env
            scsScanned (rowFor ScStructures stats) `shouldBe` 1
            scsEmitted (rowFor ScStructures stats) `shouldBe` 0

    it "reports zero once every effect has expired" $ \(env, ls) → do
        (_, _) ← resetScene env
        _ ← placeAndCommit env ls floorAppearance targetTile
        clearAndCommit env ls floorAppearance targetTile
        writeIORef (gameTimeRef env) (clipDuration floorAppearance)
        pruneStructureDestructions env
        stats ← statsOf env
        scsScanned (rowFor ScStructures stats) `shouldBe` 0
        scsEmitted (rowFor ScStructures stats) `shouldBe` 0

-- * Persistence

persistenceSpec ∷ SpecWith (EngineEnv, LuaBackendState)
persistenceSpec = describe "a save taken mid-playback" $ do

    it "restores neither the piece nor its effect, through the real \
       \codec" $ \(env, ls) → do
        (wsMain, _) ← resetScene env
        _ ← placeAndCommit env ls floorAppearance targetTile
        clearAndCommit env ls floorAppearance targetTile
        -- Mid-playback: the effect is live right now.
        effectKeys wsMain `shouldReturn` [keyOf floorAppearance]

        edits   ← readIORef (wsEditsRef wsMain)
        palette ← readIORef (texPaletteRef env)
        -- #2021: every persistable page carries one, minted by
        -- 'emptyWorldState'. Taken from the LIVE page rather than
        -- invented, so the snapshot is this page's.
        generated ← readIORef (wsGeneratedIdRef wsMain)
        let page = (blankPageSnapshot mainPage genParams)
                       { pgsEdits = edits
                       , pgsGeneratedId = Just generated }
            globals = SessionGlobals
                { sgGameTime          = 1
                , sgTexPalette        = palette
                , sgNextItemId        = 1
                , sgNextBuildingId    = 1
                , sgNextUnitId        = 1
                , sgActivePage        = mainPage
                , sgVisiblePages      = [mainPage]
                , sgLiveCamera        = LiveCameraSnapshot
                    { lcsOwnerPage = Just mainPage, lcsX = 0, lcsY = 0
                    , lcsZoom = 1, lcsFacing = FaceSouth }
                , sgPortableKnowledge = emptyPortableKnowledge
                }
        snap ← case captureSessionSnapshot globals [page] of
            Right s   → pure s
            Left errs → fail ("mid-playback snapshot invalid: " <> show errs)
        let req = SaveRequestMeta { srmSlotName = "teardown_test"
                                  , srmTimestamp = "ts", srmAutosave = False }
            meta = snapshotSaveMetadata req snap
            encoded = encodeSessionSnapshot meta snap ([] ∷ [LuaComponentSpec])
        case decodeSessionEnvelope HS.empty HS.empty encoded of
            Left err → expectationFailure (show err)
            Right (_meta, restored, _lua, _migrated) → do
                restoredPage ← case HM.lookup mainPage (snapPages restored) of
                    Just p  → pure p
                    Nothing → fail "the saved page did not come back"
                -- The edit log came back, the clear included…
                pgsEdits restoredPage `shouldNotBe` emptyWorldEdits
                -- …and replaying it rebuilds NO piece, because
                -- WeClearStructure is the authority the effect never
                -- was.
                let replayed = foldl (flip applyEdit)
                                     (flatChunkAt targetChunk)
                                     [ e | (_, es) ← sortOn fst
                                               (HM.toList (pgsEdits restoredPage))
                                         , e ← es ]
                HM.lookup (keyOf floorAppearance) (lcStructures replayed)
                    `shouldBe` Nothing
                -- The effect itself has no field to come back IN: a
                -- PageSnapshot carries none, which is requirement 9.
                lcStructures replayed `shouldBe` emptyChunkStructures

-- * Small helpers

-- | Is this edit the authoritative removal? Top level rather than a
--   local @where@, because it is asked by two examples in two different
--   groups and a statement inside a @do@ block carries no @where@.
isClear ∷ WorldEdit → Bool
isClear (WeClearStructure {}) = True
isClear _                     = False

fromJustInt ∷ Maybe Int → Int
fromJustInt (Just n) = n
fromJustInt Nothing  = error "fixture appearance declares no clip"

shouldNotReturn' ∷ (Show α, Eq α) ⇒ IO α → α → IO ()
shouldNotReturn' action expected = action ≫= (`shouldNotBe` expected)

shouldSatisfy' ∷ Show α ⇒ IO α → (α → Bool) → IO ()
shouldSatisfy' action p = action ≫= (`shouldSatisfy` p)
