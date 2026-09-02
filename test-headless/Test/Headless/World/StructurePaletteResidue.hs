{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "A rejected structure placement interns nothing" (#1675).
--
--   @structure.place@'s own contract promises it returns false "and does
--   nothing" when the paths are omitted, there is no active world, or the
--   target chunk isn't loaded. Two of those three used to do something:
--   both texture paths were interned into the save-level palette and both
--   ids registered in the handle map BEFORE the target was validated at
--   all, and neither rejection branch rolled either mutation back.
--
--   That residue is DURABLE. @texture-palette@ is a required persistent
--   component whose DTO mirrors the live palette verbatim, and snapshot
--   integrity deliberately checks only the forward direction (every
--   'WeSetStructure' edit's ids exist in the palette) — it never rejects
--   or prunes a palette entry no edit references. So a path interned by a
--   call that placed nothing rode into every later save.
--
--   The fix orders the existing operations validate → intern → stage →
--   queue: the ids exist only to ride the queued edit, so nothing ahead
--   of the rejection branches needs them.
--
--   Every example here drives the REAL @structure.place@ through the real
--   Lua API — never by interning by hand, which would be the test
--   asserting its own writes. The engine is this module's own
--   'initializeEngineHeadlessQuiet' (the 'Test.Headless.World.StructureStage'
--   shape): it runs NO worker threads, so a queued command stays in the
--   queue and "nothing was queued" is an assertion on the queue itself.
--   Both pages are in-memory 'emptyWorldState' pages carrying synthetic
--   flat chunks — no worldgen.
--
--   The palette is engine-GLOBAL and accumulates across examples, so every
--   scenario uses its own unique path pair and reads the ids it was given
--   off the emitted command rather than assuming them.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "interns nothing"'@ (the issue's own
--   @--match "structure"@ runs it together with every other
--   structure-named example).
module Test.Headless.World.StructurePaletteResidue (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find, nub, sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Palette (TexPalette(..), lookupPath)
import Structure.Types
    ( StructurePieceData(..), StructureSlot(..), StructureStage(..)
    , StructureStageToken, StagedStructurePiece(..), emptyChunkStructures )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Command.Types (WorldCommand(..))
import World.Edit.Types (WorldEdits)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile, globalToChunk, tileAliasStep)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Save.Component (encodeComponentSpecs)
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Component.Session (TexPaletteDTO(..))
import World.Save.Component.Types (texPaletteComponentId)
import World.Save.Snapshot
    ( LiveCameraSnapshot(..), SessionGlobals(..), captureSessionSnapshot )
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Tile.Types (WorldTileData(..))

-- * Fixture identity

-- | The visible page every page-less call resolves to, and a registered
--   but HIDDEN page an explicit page argument can name (#89 stamping,
--   requirement 6). 'missingPage' is registered nowhere — naming it is
--   the "unknown explicit page" rejection.
visiblePage, hiddenPage, missingPage ∷ WorldPageId
visiblePage = WorldPageId "residue_visible"
hiddenPage  = WorldPageId "residue_hidden"
missingPage = WorldPageId "residue_absent"

-- | World size in chunks. Even and non-zero, so the u-wrap is real and
--   'aliasOf' names a genuinely different coord for one tile.
worldSizeChunks ∷ Int
worldSizeChunks = 8

targetTile ∷ (Int, Int)
targetTile = (3, 3)

targetSlot ∷ StructureSlot
targetSlot = SFloor

targetSlotName ∷ Text
targetSlotName = "floor"

targetSlotTag ∷ Word8
targetSlotTag = fromIntegral (fromEnum targetSlot)

-- | The staged/queued key for 'targetTile' — canonical, exactly as
--   'structure.place' writes it.
targetKey ∷ (Int, Int, Word8)
targetKey = (fst targetTile, snd targetTile, targetSlotTag)

targetChunk ∷ ChunkCoord
targetChunk = fst (globalToChunk (fst targetTile) (snd targetTile))

-- | A u-alias of a tile: the same physical tile named one wrap away.
aliasOf ∷ (Int, Int) → (Int, Int)
aliasOf (gx, gy) = (gx + tileAliasStep worldSizeChunks
                   , gy - tileAliasStep worldSizeChunks)

placeZ ∷ Int
placeZ = 4

-- | The handles the builder passes in. Distinct, so a swapped
--   id→handle registration is visible.
texHandleArg, faceHandleArg ∷ Int
texHandleArg  = 11
faceHandleArg = 12

-- | One scenario's own path pair. Unique per example: the palette is
--   engine-global and accumulates, so a shared pair would let one
--   example's interning satisfy another's assertion.
scenarioPaths ∷ Text → (Text, Text)
scenarioPaths tag =
    ( T.concat ["assets/textures/structures/residue_", tag, "_tex.png"]
    , T.concat ["assets/textures/structures/residue_", tag, "_face.png"] )

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
    , wtdMaxChunks = max 1 (length chunks)
    }

loadedTiles ∷ WorldTileData
loadedTiles = tilesFrom [flatChunkAt targetChunk]

-- | The same page with 'targetChunk' gone — the "target chunk isn't
--   loaded" rejection.
evictedTiles ∷ WorldTileData
evictedTiles = tilesFrom []

genParams ∷ WorldGenParams
genParams = defaultWorldGenParams { wgpWorldSize = worldSizeChunks }

-- * Scene

-- | Both pages installed with 'targetChunk' loaded, only 'visiblePage'
--   visible, both stages and edit logs empty, the world queue drained.
resetScene ∷ EngineEnv → IO (WorldState, WorldState)
resetScene env = do
    wsVisible ← emptyWorldState
    wsHidden  ← emptyWorldState
    forM_ [wsVisible, wsHidden] $ \ws → do
        writeIORef (wsTilesRef ws) loadedTiles
        writeIORef (wsGenParamsRef ws) (Just genParams)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(visiblePage, wsVisible), (hiddenPage, wsHidden)]
        , wmVisible = [visiblePage] }
    _ ← drainWorldQueue env
    pure (wsVisible, wsHidden)

-- | Deregister every page: 'resolveActiveWorld' falls through
--   'wmVisible' to 'wmWorlds', so "no active world" means BOTH empty.
--   The two 'WorldState' values stay alive so their stage and edit log
--   are still observable.
detachEveryPage ∷ EngineEnv → IO ()
detachEveryPage env = writeIORef (worldManagerRef env) emptyWorldManager

evictTargetChunk ∷ WorldState → IO ()
evictTargetChunk ws = writeIORef (wsTilesRef ws) evictedTiles

-- * Queue

drainWorldQueue ∷ EngineEnv → IO [WorldCommand]
drainWorldQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (worldQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

-- | The one command the preceding call emitted. Fails loudly on any
--   other count.
takeOneCommand ∷ EngineEnv → IO WorldCommand
takeOneCommand env = do
    cmds ← drainWorldQueue env
    case cmds of
        [cmd] → pure cmd
        other → fail $ "expected exactly one queued world command, got "
                         <> show (length other) <> ": " <> show other

-- * The residue under test

-- | Everything a rejected call must leave byte-identical: the two
--   engine-global palette refs, plus each observed page's staging cache
--   and edit log. Compared as ONE value so an example cannot silently
--   check three of the five.
data Residue = Residue
    { resPalette ∷ TexPalette
    , resHandles ∷ HM.HashMap Int TextureHandle
    , resStages  ∷ [StructureStage]
    , resEdits   ∷ [WorldEdits]
    } deriving (Show, Eq)

captureResidue ∷ EngineEnv → [WorldState] → IO Residue
captureResidue env pages = Residue
    <$> readIORef (texPaletteRef env)
    <*> readIORef (texPaletteHandlesRef env)
    <*> mapM (readIORef ∘ wsStructureStageRef) pages
    <*> mapM (readIORef ∘ wsEditsRef) pages

-- | Run one rejected call and assert it changed nothing at all: the
--   return value is false, every 'Residue' field is identical, and the
--   world queue gained no command. The queue is drained BEFORE the call
--   so the emptiness afterwards is this call's own.
rejectionChangesNothing
    ∷ EngineEnv → [WorldState] → IO Text → Expectation
rejectionChangesNothing env pages act = do
    _      ← drainWorldQueue env
    before ← captureResidue env pages
    act `shouldReturn` "false"
    after  ← captureResidue env pages
    after `shouldBe` before
    -- 'WorldCommand' has no 'Eq'; compare renderings so a leftover
    -- command is NAMED in the failure rather than reported as a count.
    leftover ← drainWorldQueue env
    map show leftover `shouldBe` []

-- * Live-state readers

stageEntries ∷ WorldState → IO (HM.HashMap (Int, Int, Word8) StagedStructurePiece)
stageEntries ws = ssEntries <$> readIORef (wsStructureStageRef ws)

stageKeys ∷ WorldState → IO [(Int, Int, Word8)]
stageKeys ws = sort . HM.keys <$> stageEntries ws

stagedPieceAt ∷ WorldState → (Int, Int, Word8) → IO (Maybe StructurePieceData)
stagedPieceAt ws key = fmap stgPiece . HM.lookup key <$> stageEntries ws

stagedTokenAt ∷ WorldState → (Int, Int, Word8) → IO (Maybe StructureStageToken)
stagedTokenAt ws key = fmap stgToken . HM.lookup key <$> stageEntries ws

palette ∷ EngineEnv → IO TexPalette
palette env = readIORef (texPaletteRef env)

handleFor ∷ EngineEnv → Int → IO (Maybe TextureHandle)
handleFor env i = HM.lookup i <$> readIORef (texPaletteHandlesRef env)

-- | The command's payload, ids included, with its attempt token split
--   off — the token is compared against the staged entry separately.
setPayload ∷ WorldCommand
           → Maybe ((WorldPageId, Int, Int, Word8, Int, Int, Int), StructureStageToken)
setPayload (WorldSetStructure p gx gy slotTag texId faceId z tok) =
    Just ((p, gx, gy, slotTag, texId, faceId, z), tok)
setPayload _ = Nothing

-- | The palette ids the emitted command carries — read off the command
--   rather than assumed, since the palette is engine-global.
commandIds ∷ WorldCommand → Maybe (Int, Int)
commandIds (WorldSetStructure _ _ _ _ texId faceId _ _) = Just (texId, faceId)
commandIds _ = Nothing

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
pageArg Nothing                = ""
pageArg (Just (WorldPageId p)) = T.concat [", '", p, "'"]

-- | A Lua literal for one path argument: a quoted string, or @nil@ for
--   an omitted one (requirement 5).
pathArg ∷ Maybe Text → Text
pathArg Nothing  = "nil"
pathArg (Just p) = T.concat ["'", p, "'"]

-- | The REAL @structure.place@, with each texture path independently
--   present or omitted, at an explicit tile, on an explicit page.
placeWith ∷ LuaBackendState → (Int, Int) → (Maybe Text, Maybe Text)
          → Maybe WorldPageId → IO Text
placeWith ls (gx, gy) (mTex, mFace) mPage = evalDebug ls $ T.concat
    [ "return tostring(structure.place("
    , tshow gx, ", ", tshow gy, ", '", targetSlotName, "', "
    , tshow texHandleArg, ", ", tshow faceHandleArg, ", "
    , tshow placeZ, ", ", pathArg mTex, ", ", pathArg mFace
    , pageArg mPage, "))" ]

-- | The both-paths-present call every success and every target-side
--   rejection uses.
placeAt ∷ LuaBackendState → (Int, Int) → (Text, Text) → Maybe WorldPageId
        → IO Text
placeAt ls tile (tp, fp) = placeWith ls tile (Just tp, Just fp)

-- * The encoded save component

-- | Every texture path the REQUIRED @texture-palette@ component would
--   carry into a save right now, taken through the component's own
--   encode and read back off the decoded DTO — not off the live
--   'TexPalette', which is the thing the encode is being checked
--   against.
encodedPalettePaths ∷ EngineEnv → IO [Text]
encodedPalettePaths env = do
    pal ← palette env
    let globals = SessionGlobals
            { sgGameTime       = 0
            , sgTexPalette     = pal
            , sgNextItemId     = 1
            , sgNextBuildingId = 1
            , sgNextUnitId     = 1
            , sgActivePage     = visiblePage
            , sgVisiblePages   = [visiblePage]
            , sgLiveCamera     = LiveCameraSnapshot
                { lcsOwnerPage = Just visiblePage
                , lcsX = 0, lcsY = 0, lcsZoom = 1, lcsFacing = FaceNorth }
            }
        pages = [ blankPageSnapshot visiblePage genParams ]
    snap ← case captureSessionSnapshot globals pages of
        Right s   → pure s
        Left errs → fail ("snapshot invalid: " <> show errs)
    payload ← case findComponent texPaletteComponentId (encodeComponentSpecs snap) of
        Just bytes → pure bytes
        Nothing    → fail "encodeComponentSpecs emitted no texture-palette component"
    case S.decode payload of
        Right dto  → pure (map fst (tpdPairs dto))
        Left  err  → fail ("texture-palette payload did not decode: " <> err)
  where
    findComponent cid specs =
        (\(_, _, _, bytes) → bytes)
            <$> find (\(i, _, _, _) → i ≡ cid) specs

-- * Spec

spec ∷ Spec
spec = describe "A rejected structure placement interns nothing (#1675)"
     $ aroundAll setup $ do
    fixtureSpec
    rejectionSpec
    omittedPathSpec
    successSpec
    seamSpec
    saveSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer, so a scratch root established afterwards is too late.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        act (env, ls)

-- | The fixture's discriminators really discriminate.
fixtureSpec ∷ SpecWith (EngineEnv, LuaBackendState)
fixtureSpec = describe "the fixture" $ do

    it "names one physical tile by two different coords" $ \_ → do
        aliasOf targetTile `shouldNotBe` targetTile
        uncurry (canonicalTile worldSizeChunks) (aliasOf targetTile)
            `shouldBe` targetTile

    it "evicts exactly the chunk that stores the target tile" $ \_ → do
        HM.member targetChunk (wtdChunks loadedTiles)  `shouldBe` True
        HM.member targetChunk (wtdChunks evictedTiles) `shouldBe` False

    it "gives every scenario its own path pair" $ \_ → do
        let allPaths = concat
                [ [tp, fp]
                | tag ← [ "noworld", "unknownpage", "unloaded", "notex"
                        , "noface", "success", "reuse", "seam"
                        , "save", "savekept" ]
                , let (tp, fp) = scenarioPaths tag ]
        length (nub allPaths) `shouldBe` length allPaths

-- | Requirements 1-3: each of the three target-side rejection modes
--   leaves the palette, the handle map, both stages, both edit logs and
--   the world queue exactly as it found them.
rejectionSpec ∷ SpecWith (EngineEnv, LuaBackendState)
rejectionSpec = describe "a rejected placement" $ do

    it "interns nothing when there is no active world" $ \(env, ls) → do
        (wsVisible, wsHidden) ← resetScene env
        detachEveryPage env
        rejectionChangesNothing env [wsVisible, wsHidden] $
            placeAt ls targetTile (scenarioPaths "noworld") Nothing

    it "interns nothing when the named page does not exist" $ \(env, ls) → do
        (wsVisible, wsHidden) ← resetScene env
        rejectionChangesNothing env [wsVisible, wsHidden] $
            placeAt ls targetTile (scenarioPaths "unknownpage") (Just missingPage)

    it "interns nothing when the target chunk is not loaded" $ \(env, ls) → do
        (wsVisible, wsHidden) ← resetScene env
        evictTargetChunk wsVisible
        rejectionChangesNothing env [wsVisible, wsHidden] $
            placeAt ls targetTile (scenarioPaths "unloaded") Nothing

-- | Requirement 5: an omitted path still places nothing and interns
--   nothing — checked once per path, since a single both-omitted case
--   would not distinguish them. The target is fully valid here, so the
--   omission is the only reason for the rejection.
omittedPathSpec ∷ SpecWith (EngineEnv, LuaBackendState)
omittedPathSpec = describe "an omitted path" $ do

    it "interns nothing when the texture path is omitted" $ \(env, ls) → do
        (wsVisible, wsHidden) ← resetScene env
        let (_, fp) = scenarioPaths "notex"
        rejectionChangesNothing env [wsVisible, wsHidden] $
            placeWith ls targetTile (Nothing, Just fp) Nothing

    it "interns nothing when the facemap path is omitted" $ \(env, ls) → do
        (wsVisible, wsHidden) ← resetScene env
        let (tp, _) = scenarioPaths "noface"
        rejectionChangesNothing env [wsVisible, wsHidden] $
            placeWith ls targetTile (Just tp, Nothing) Nothing

-- | Requirement 4: the success path is byte-for-byte what it was —
--   ids allocated, both handles registered against them, the piece
--   staged, and the command queued carrying the same ids and the staged
--   entry's own token.
successSpec ∷ SpecWith (EngineEnv, LuaBackendState)
successSpec = describe "an accepted placement" $ do

    it "interns both paths, registers both handles, stages and queues" $
        \(env, ls) → do
            (wsVisible, wsHidden) ← resetScene env
            let paths@(tp, fp) = scenarioPaths "success"
            placeAt ls targetTile paths Nothing `shouldReturn` "true"
            cmd ← takeOneCommand env
            (texId, faceId) ← case commandIds cmd of
                Just ids → pure ids
                Nothing  → fail ("not a WorldSetStructure: " <> show cmd)

            -- the ids resolve back to the paths that were interned
            pal ← palette env
            lookupPath texId  pal `shouldBe` Just tp
            lookupPath faceId pal `shouldBe` Just fp
            HM.lookup tp (tpPathToId pal) `shouldBe` Just texId
            HM.lookup fp (tpPathToId pal) `shouldBe` Just faceId
            (texId < tpNextId pal ∧ faceId < tpNextId pal) `shouldBe` True

            -- and to the handles the builder passed, not each other's
            handleFor env texId  `shouldReturn`
                Just (TextureHandle texHandleArg)
            handleFor env faceId `shouldReturn`
                Just (TextureHandle faceHandleArg)

            -- staged at the canonical key, with those same ids
            stageKeys wsVisible `shouldReturn` [targetKey]
            stagedPieceAt wsVisible targetKey `shouldReturn`
                Just (StructurePieceData texId faceId placeZ)

            -- queued for the resolved page, carrying the staged token
            tok ← stagedTokenAt wsVisible targetKey
            setPayload cmd `shouldBe`
                ((,) ( visiblePage, fst targetTile, snd targetTile
                     , targetSlotTag, texId, faceId, placeZ ) <$> tok)

            -- the untargeted page is untouched
            stageKeys wsHidden `shouldReturn` []

    it "reuses the ids and handles a repeat of the same paths" $
        \(env, ls) → do
            (wsVisible, _) ← resetScene env
            let paths = scenarioPaths "reuse"
            placeAt ls targetTile paths Nothing `shouldReturn` "true"
            first ← takeOneCommand env
            palAfterFirst ← palette env

            placeAt ls targetTile paths Nothing `shouldReturn` "true"
            second ← takeOneCommand env
            palAfterSecond ← palette env

            commandIds second `shouldBe` commandIds first
            palAfterSecond `shouldBe` palAfterFirst
            stageKeys wsVisible `shouldReturn` [targetKey]

-- | Requirement 6: explicit hidden-page placement and canonical wrapped
--   resolution (#1175) survive the reorder. The alias is typed; only the
--   HIDDEN page gains the canonical key, and the command carries the
--   canonical coords.
seamSpec ∷ SpecWith (EngineEnv, LuaBackendState)
seamSpec = describe "a hidden-page placement through a u-alias" $ do

    it "stages and queues the canonical tile on that page alone" $
        \(env, ls) → do
            (wsVisible, wsHidden) ← resetScene env
            let alias = aliasOf targetTile
            placeAt ls alias (scenarioPaths "seam") (Just hiddenPage)
                `shouldReturn` "true"
            cmd ← takeOneCommand env
            (texId, faceId) ← case commandIds cmd of
                Just ids → pure ids
                Nothing  → fail ("not a WorldSetStructure: " <> show cmd)

            stageKeys wsHidden  `shouldReturn` [targetKey]
            stageKeys wsVisible `shouldReturn` []
            tok ← stagedTokenAt wsHidden targetKey
            setPayload cmd `shouldBe`
                ((,) ( hiddenPage, fst targetTile, snd targetTile
                     , targetSlotTag, texId, faceId, placeZ ) <$> tok)

-- | The durability half: the rejected call's paths never reach the
--   encoded, REQUIRED @texture-palette@ component, while an accepted
--   call's do — so the assertion cannot pass by the component being
--   empty or unreachable.
saveSpec ∷ SpecWith (EngineEnv, LuaBackendState)
saveSpec = describe "the encoded texture-palette component" $ do

    it "omits a rejected call's paths and keeps an accepted call's" $
        \(env, ls) → do
            (wsVisible, _) ← resetScene env
            let (rejTex, rejFace) = scenarioPaths "save"
                accepted          = scenarioPaths "savekept"

            evictTargetChunk wsVisible
            placeAt ls targetTile (rejTex, rejFace) Nothing
                `shouldReturn` "false"

            _ ← resetScene env
            placeAt ls targetTile accepted Nothing `shouldReturn` "true"
            _ ← takeOneCommand env

            encoded ← encodedPalettePaths env
            filter (`elem` encoded) [rejTex, rejFace] `shouldBe` []
            filter (`elem` encoded) [fst accepted, snd accepted]
                `shouldBe` [fst accepted, snd accepted]
