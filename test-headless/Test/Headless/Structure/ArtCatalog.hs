{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The unplaced-piece art catalogue (#1842).
--
--   A construction designation stores only @pack + kind + optional
--   edge@, and the pack YAML's translation of that into a texture and a
--   facemap lived only in @scripts/structures.lua@ / @scripts/wire.lua@.
--   The world render thread cannot call into Lua, so #1842 hands the
--   translation over up front. This is that capability's gate.
--
--   == How parity is proven
--
--   The resolution examples never compare against a table written here.
--   Each one drives the REAL Lua builder (@scripts/structures.lua@,
--   @scripts/wire.lua@, reading the real
--   @data/structure_packs/*.yaml@) and compares the engine's answer to
--   the texture and facemap that builder's own @structure.place@ call
--   actually interned — read back off the queued @WorldSetStructure@
--   through the live palette, plus the runtime handle each id resolved
--   to. A hand-written expectation would certify art the builder does
--   not use, which is the one thing this catalogue must never do.
--
--   Each example resolves while the piece is still UNPLACED, then
--   places it, then compares: that ordering is the feature (the ghost
--   asks before anything exists), and it also makes the placement
--   incapable of seeding the answer.
--
--   The engine is this module's own 'initializeEngineHeadless' (the
--   'Test.Headless.World.StructurePaletteResidue' shape): it runs NO
--   worker threads, so a queued @WorldSetStructure@ stays in the queue
--   and is readable as evidence, and the read-your-writes staging cache
--   is what makes a post visible to the wall placed after it.
--
--   == Why the palette-residue example lives here
--
--   #1675's own spec ('Test.Headless.World.StructurePaletteResidue')
--   proves a REJECTED PLACEMENT interns nothing, on a fixture with no
--   Lua modules and a deliberately unique-path scheme per example.
--   #1842's requirement 9 is the stronger statement — a session that
--   registers and resolves EVERY REAL PACK's art without placing
--   anything leaves the palette untouched — and it needs exactly what
--   that fixture does not have: the real pack modules loaded. It is
--   asserted here, against the same two engine-global refs.
--
--   Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match "Structure.ArtCatalog"'@
--   and @--match "wire autotile parity"@.
module Test.Headless.Structure.ArtCatalog (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation
    (withExclusiveTempDirectory, withIsolatedResourceRoot)
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (nub, sort)
import System.FilePath ((</>))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Log
    ( LogBackend(..), LogConfig(..), LogEntry(..), LogLevel(..)
    , defaultLogConfig, initLogger )
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..))
import Structure.ArtCatalog (StructureArtCatalog(..))
import Structure.Facing
    (WallEdge(..), PostCorner(..), wallEdgeEnds)
import Structure.Palette (TexPalette(..), lookupPath)
import Structure.Types (emptyChunkStructures)
import Structure.Wire
    (WireNeighbors(..), allWireNeighbors, wireShapeFor, wireShapeName)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Command.Types (WorldCommand(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile, globalToChunk)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Construct.Attempt (firstConstructAttemptId)
import World.Construct.Types
    (ConstructTarget(..), StructurePiece(..), newConstructDesignation)
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Tile.Types (WorldTileData(..))

-- * Fixture

-- | 16 chunks a side, so 'World.Generate.Coordinates.tileAliasStep' is
--   128 tiles and every tile this module touches is its own canonical
--   name. The seam itself is 'Test.Headless.World.Render.StructureSeam'
--   and 'Test.Headless.World.DesignationSeam' business; what matters
--   here is that no scenario is accidentally aliased into another's.
worldSizeChunks ∷ Int
worldSizeChunks = 16

artPage ∷ WorldPageId
artPage = WorldPageId "art_catalog_page"

dungeonPack, wirePack ∷ Text
dungeonPack = "dungeon_1"
wirePack    = "wire"

-- | Every tile any scenario reads or writes, including the wire
--   scenarios' four neighbours. Drives which chunks the fixture loads.
allScenarioTiles ∷ [(Int, Int)]
allScenarioTiles =
    [ simpleTile k | k ← [0 .. 2] ]
    ⧺ [ wallTile i | i ← [0 .. 15] ]
    ⧺ concat [ (x, y) : [ (x + dx, y + dy)
                        | (dx, dy) ← [(0,-1),(1,0),(0,1),(-1,0)] ]
             | i ← [0 .. 15], let (x, y) = wireTile i ]

-- | The three variant-free kinds, one tile each.
simpleTile ∷ Int → (Int, Int)
simpleTile k = (2 + 2 * k, 2)

-- | One tile per (edge, cap) pair — sixteen, spaced so no tile's posts
--   can be mistaken for a neighbour's.
wallTile ∷ Int → (Int, Int)
wallTile i = (2 + 2 * i, 6)

-- | One tile per wire neighbour combination, on two rows so the whole
--   sweep stays well inside the canonical frame.
wireTile ∷ Int → (Int, Int)
wireTile i = (3 + 4 * (i `mod` 8), 12 + 4 * (i `div` 8))

flatChunkAt ∷ ChunkCoord → LoadedChunk
flatChunkAt coord =
    let area = chunkSize * chunkSize
        col  = ColumnTiles { ctStartZ = 0
                           , ctMats   = VU.singleton 1
                           , ctSlopes = VU.singleton 0
                           , ctVeg    = VU.singleton 0 }
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

loadedTiles ∷ WorldTileData
loadedTiles = WorldTileData
    { wtdChunks    = HM.fromList [ (c, flatChunkAt c) | c ← coords ]
    , wtdMaxChunks = max 1 (length coords) }
  where
    coords = foldr insertUnique []
                   [ fst (globalToChunk gx gy) | (gx, gy) ← allScenarioTiles ]
    insertUnique c cs | c `elem` cs = cs
                      | otherwise   = c : cs

genParams ∷ WorldGenParams
genParams = defaultWorldGenParams { wgpWorldSize = worldSizeChunks }

-- | One page, visible, every scenario chunk loaded, the world queue
--   drained.
installScene ∷ EngineEnv → IO WorldState
installScene env = do
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) loadedTiles
    writeIORef (wsGenParamsRef ws) (Just genParams)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(artPage, ws)], wmVisible = [artPage] }
    _ ← drainWorldQueue env
    pure ws

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- * Lua plumbing

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src

-- | Run a statement for its effect, failing loudly on a Lua error
--   rather than letting a silent nil turn into a confusing mismatch.
runLua ∷ LuaBackendState → Text → IO ()
runLua ls src = do
    r ← evalDebug ls (src <> " return 'ok'")
    unless (r ≡ "ok") $ fail ("lua statement failed: " <> T.unpack src
                                <> " -> " <> T.unpack r)

-- * Queue readers

drainWorldQueue ∷ EngineEnv → IO [WorldCommand]
drainWorldQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (worldQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

-- | One resolved art asset: the PATH it names and the runtime texture
--   handle that path is drawn with.
type Asset = (Text, Int)

-- | The pair the REAL builder placed at @tile@: read off the queued
--   'WorldSetStructure' (palette ids → paths through the live palette,
--   ids → handles through the live translation table), never assumed.
--   The LAST placement at that tile wins, since a wire commit re-caps
--   its neighbours and a post re-caps its own tile's walls.
placedArtAt ∷ EngineEnv → LuaBackendState → Text → (Int, Int) → IO (Asset, Asset)
placedArtAt env ls placeExpr (gx, gy) = do
    _   ← drainWorldQueue env
    res ← evalDebug ls ("return tostring(" <> placeExpr <> ")")
    unless (res ≡ "true") $
        fail ("the real builder refused " <> T.unpack placeExpr
                <> ": " <> T.unpack res)
    cmds ← drainWorldQueue env
    pal  ← readIORef (texPaletteRef env)
    handles ← readIORef (texPaletteHandlesRef env)
    let mine = [ (t, f) | WorldSetStructure _ cx cy _ t f _ _ ← cmds
                        , (cx, cy) ≡ (gx, gy) ]
        asset i = (,) <$> lookupPath i pal
                      <*> ((\(TextureHandle h) → h) <$> HM.lookup i handles)
    case reverse mine of
        ((t, f) : _) → case (asset t, asset f) of
            (Just a, Just b) → pure (a, b)
            _ → fail ("placed palette ids did not resolve for "
                        <> T.unpack placeExpr)
        [] → fail ("no structure placement queued at " <> show (gx, gy)
                     <> " by " <> T.unpack placeExpr)

-- | What the ENGINE says an unplaced piece of that descriptor at that
--   tile would be built with, through the real Lua-facing verb.
resolvedArtAt ∷ LuaBackendState → Text → Text → Maybe Text → (Int, Int)
              → IO (Maybe (Asset, Asset))
resolvedArtAt ls pack kind mEdge (gx, gy) = do
    r ← evalDebug ls $ T.concat
        [ "local a = structure.resolvePieceArt('", pack, "','", kind, "',"
        , maybe "nil" (\e → T.concat ["'", e, "'"]) mEdge
        , ",", tshow gx, ",", tshow gy, "); "
        , "if not a then return 'nil' end; "
        , "return a.texture .. '|' .. a.texHandle .. '|' "
        , "    .. a.facemap .. '|' .. a.faceHandle" ]
    if r ≡ "nil" then pure Nothing else case T.splitOn "|" r of
        [tp, th, fp, fh] → pure (Just ((tp, readInt th), (fp, readInt fh)))
        _ → fail ("resolvePieceArt returned an unreadable value: " <> T.unpack r)
  where
    -- engine.loadTexture handles come back through Lua's number
    -- formatting, so "12.0" is the same handle as "12".
    readInt = truncate ∘ (read ∷ String → Double) ∘ T.unpack

-- * Log capture

-- | Swap in a callback logger for the duration, so an example can
--   assert on the warnings a step ACTUALLY emitted rather than on its
--   own reading of the code.
withCapturedLog ∷ EngineEnv → IO α → IO (α, [LogEntry])
withCapturedLog env act = do
    capturedRef ← newIORef []
    original ← readIORef (loggerRef env)
    capturing ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :)) }
    result ← (writeIORef (loggerRef env) capturing ≫ act)
                 `finally` writeIORef (loggerRef env) original
    entries ← readIORef capturedRef
    pure (result, reverse entries)

warningsOf ∷ [LogEntry] → [Text]
warningsOf = map leMessage ∘ filter ((≡ LevelWarn) ∘ leLevel)

-- * Scenario vocabulary

edgeName ∷ WallEdge → Text
edgeName e = case e of
    WallNE → "ne"; WallNW → "nw"; WallSE → "se"; WallSW → "sw"

cornerName ∷ PostCorner → Text
cornerName c = case c of
    CornerN → "n"; CornerE → "e"; CornerS → "s"; CornerW → "w"

allEdges ∷ [WallEdge]
allEdges = [WallNE, WallNW, WallSE, WallSW]

-- | The four cap codes, as the pack YAML spells them:
--   @"\<left\>\<right\>"@.
allCapCodes ∷ [(Bool, Bool)]
allCapCodes = [(l, r) | l ← [False, True], r ← [False, True]]

capCode ∷ (Bool, Bool) → Text
capCode (l, r) = bit l <> bit r
  where bit b = if b then "1" else "0"

luaXY ∷ (Int, Int) → Text
luaXY (x, y) = tshow x <> "," <> tshow y

-- * Spec

spec ∷ Spec
spec = aroundAll setup $ do
    catalogSpec
    wireSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer. It also gives the scratch root symlinked `scripts/`,
    -- `data/` and `assets/`, so the real pack modules load exactly as
    -- they do from the checkout.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadless
        ls ← newBareLuaBackend env
        _  ← installScene env
        -- The production entry points, called exactly as
        -- `scripts/init.lua` calls them each tick.
        runLua ls "require('scripts.structures').registerPackArt();"
        runLua ls "require('scripts.wire').registerPackArt();"
        act (env, ls)

catalogSpec ∷ SpecWith (EngineEnv, LuaBackendState)
catalogSpec = describe "Structure.ArtCatalog (#1842)" $ do
    fixtureSpec
    paritySpec
    wallCapSpec
    refusalSpec
    buildMetadataSpec
    warningSpec
    paletteSpec

-- | The fixture's discriminators really discriminate.
fixtureSpec ∷ SpecWith (EngineEnv, LuaBackendState)
fixtureSpec = describe "the fixture" $ do

    it "gives every scenario its own canonical, non-overlapping tile" $ \_ → do
        let tiles = allScenarioTiles
        length (nub tiles) `shouldBe` length tiles
        [ t | t ← tiles
            , uncurry (canonicalTile worldSizeChunks) t ≢ t ] `shouldBe` []

    it "loads a chunk for every tile a scenario touches" $ \_ →
        [ t | t ← allScenarioTiles
            , not (HM.member (fst (globalToChunk (fst t) (snd t)))
                             (wtdChunks loadedTiles)) ]
            `shouldBe` []

    it "registered both real packs" $ \(env, _) → do
        cat ← readIORef (structureArtCatalogRef env)
        sort (HM.keys (sacPacks cat)) `shouldBe` sort [dungeonPack, wirePack]

-- | Requirement 3 and the acceptance's resolution-parity bullet, for the
--   three variant-free kinds. The wall and wire kinds get their own
--   sweeps below because their slot is chosen from world context.
paritySpec ∷ SpecWith (EngineEnv, LuaBackendState)
paritySpec = describe "an unplaced piece" $
    forM_ (zip [0 ..] simpleKinds) $ \(i, (kind, setupLua, placeLua)) →
        it ("resolves the art the builder places for " <> T.unpack kind) $
            \(env, ls) → do
                let tile = simpleTile i
                forM_ (setupLua tile) (runLua ls)
                resolved ← resolvedArtAt ls dungeonPack kind Nothing tile
                placed   ← placedArtAt env ls (placeLua tile) tile
                resolved `shouldBe` Just placed
  where
    simpleKinds =
        [ ( "floor", const []
          , \t → "require('scripts.structures').floor(" <> luaXY t <> ")" )
        , ( "ceiling", const []
          , \t → "require('scripts.structures').ceiling(" <> luaXY t <> ")" )
        -- A post only renders the corners of an existing FLOOR and takes
        -- its z, so the floor is a precondition of the placement rather
        -- than part of this comparison.
        , ( "post"
          , \t → ["require('scripts.structures').floor(" <> luaXY t <> ");"]
          , \t → "require('scripts.structures').post(" <> luaXY t <> ",'n')" )
        ]

-- | Requirement 4 and the acceptance's wall-cap bullet: all four post
--   combinations, for all four edges, resolve the cap facemap
--   @placeWall@ actually selects.
wallCapSpec ∷ SpecWith (EngineEnv, LuaBackendState)
wallCapSpec = describe "an unplaced wall" $
    forM_ (zip [0 ..] [ (e, c) | e ← allEdges, c ← allCapCodes ]) $
        \(i, (edge, caps)) →
            it ("resolves the cap the builder uses for " <> T.unpack (edgeName edge)
                  <> " with posts " <> T.unpack (capCode caps)) $
                \(env, ls) → do
                    let tile = wallTile i
                        (l, r) = wallEdgeEnds edge
                        wanted = [ cornerName c
                                 | (c, on) ← [(l, fst caps), (r, snd caps)], on ]
                    runLua ls ("require('scripts.structures').floor("
                                 <> luaXY tile <> ");")
                    forM_ wanted $ \c → runLua ls $ T.concat
                        [ "require('scripts.structures').post("
                        , luaXY tile, ",'", c, "');" ]
                    resolved ← resolvedArtAt ls dungeonPack "wall"
                                             (Just (edgeName edge)) tile
                    placed ← placedArtAt env ls
                        (T.concat [ "require('scripts.structures').wall("
                                  , luaXY tile, ",'", edgeName edge, "')" ])
                        tile
                    resolved `shouldBe` Just placed

-- | Requirements 2 and 6, plus the review's atomicity clarification: a
--   short payload, a malformed one, and a CONFLICTING repeat each leave
--   the catalogue exactly as they found it; an unregistered pack and an
--   undeclared kind resolve nothing.
refusalSpec ∷ SpecWith (EngineEnv, LuaBackendState)
refusalSpec = describe "registration" $ do

    it "refuses a pack missing one of a declared kind's slots" $ \(env, ls) → do
        before ← readIORef (structureArtCatalogRef env)
        -- `wall` declared, but only fifteen of its sixteen cap facemaps.
        evalDebug ls (registerWallish "short_pack" (drop 1 wallArtEntries))
            `shouldReturn` "false"
        after ← readIORef (structureArtCatalogRef env)
        sacPacks after `shouldBe` sacPacks before

    it "refuses a pack carrying a malformed entry" $ \(env, ls) → do
        before ← readIORef (structureArtCatalogRef env)
        -- Complete, except that one entry's texture handle is the zero
        -- sentinel `engine.loadTexture` never mints.
        corrupted ← case wallArtEntries of
            (e : rest) → pure (T.replace "texHandle=41" "texHandle=0" e : rest)
            []         → fail "wallArtEntries must not be empty"
        evalDebug ls (registerWallish "malformed_pack" corrupted)
            `shouldReturn` "false"
        after ← readIORef (structureArtCatalogRef env)
        sacPacks after `shouldBe` sacPacks before

    it "keeps the stored pack when a repeat conflicts, and no-ops on an \
       \identical one" $ \(env, ls) → do
        evalDebug ls (registerFloorPack "repeat_pack" "a.png" True)
            `shouldReturn` "true"
        stored ← readIORef (structureArtCatalogRef env)
        evalDebug ls (registerFloorPack "repeat_pack" "a.png" True)
            `shouldReturn` "true"
        idempotent ← readIORef (structureArtCatalogRef env)
        sacPacks idempotent `shouldBe` sacPacks stored
        evalDebug ls (registerFloorPack "repeat_pack" "b.png" True)
            `shouldReturn` "false"
        conflicted ← readIORef (structureArtCatalogRef env)
        sacPacks conflicted `shouldBe` sacPacks stored
        -- and the stored art is still the FIRST registration's
        resolvedArtAt ls "repeat_pack" "floor" Nothing (simpleTile 0)
            `shouldReturn` Just (("a.png", 41), ("face.png", 42))

    it "refuses a sparse `kinds` or `art` array rather than silently \
       \dropping what is past the gap" $ \(env, ls) → do
        -- `rawlen` answers a BORDER, so `{[1]=a, [3]=b}` can report 1.
        -- Walking to that border would accept a pack that is missing a
        -- declared kind (or an art slot) as if it were complete — the
        -- partial registration the all-or-nothing rule refuses.
        let sparse label body = do
                before ← readIORef (structureArtCatalogRef env)
                (r, entries) ← withCapturedLog env $ evalDebug ls body
                r `shouldBe` "false"
                readIORef (structureArtCatalogRef env)
                    ⌦ \after → sacPacks after `shouldBe` sacPacks before
                case warningsOf entries of
                    [w] → forM_ ["pack 'sparse_pack'", "sparse"] $ \needle →
                        unless (needle `T.isInfixOf` w) $ expectationFailure
                            (label <> ": the warning does not name "
                               <> show needle <> ": " <> T.unpack w)
                    other → expectationFailure
                        (label <> ": expected exactly one warning, got: "
                           <> show other)
        sparse "sparse kinds" $ T.concat
            [ "return tostring(structure.registerPackArt{ pack='sparse_pack', "
            , "kinds={[1]={kind='floor', buildable=true}, "
            ,        "[3]={kind='ceiling', buildable=true}}, "
            , "art={{kind='floor', texture='a.png', texHandle=41, "
            , "facemap='f.png', faceHandle=42}} })" ]
        sparse "sparse art" $ T.concat
            [ "return tostring(structure.registerPackArt{ pack='sparse_pack', "
            , "kinds={{kind='floor', buildable=true}}, "
            , "art={[1]={kind='floor', texture='a.png', texHandle=41, "
            ,           "facemap='f.png', faceHandle=42}, "
            ,      "[3]={kind='ceiling', texture='c.png', texHandle=41, "
            ,           "facemap='f.png', faceHandle=42}} })" ]
        resolvedArtAt ls "sparse_pack" "floor" Nothing (simpleTile 0)
            `shouldReturn` Nothing

    it "resolves nothing for an unregistered pack" $ \(_, ls) →
        resolvedArtAt ls "no_such_pack" "floor" Nothing (simpleTile 0)
            `shouldReturn` Nothing

    it "resolves nothing for a kind the pack does not declare" $ \(_, ls) → do
        -- `wire` is a real, healthy, registered pack — it simply has no
        -- floor, and that must not fall back to dungeon_1's.
        resolvedArtAt ls wirePack "floor" Nothing (simpleTile 0)
            `shouldReturn` Nothing
        resolvedArtAt ls dungeonPack "wire" Nothing (simpleTile 0)
            `shouldReturn` Nothing

-- | Requirement 5 and the review's clarification: art and buildability
--   are separate answers, and incomplete build metadata does not reject
--   otherwise complete art.
buildMetadataSpec ∷ SpecWith (EngineEnv, LuaBackendState)
buildMetadataSpec = describe "build metadata" $ do

    it "reports the real packs' costed kinds as buildable" $ \(_, ls) →
        forM_ ([ (dungeonPack, k) | k ← ["floor", "ceiling", "post", "wall"] ]
                 ⧺ [(wirePack, "wire")]) $ \(pack, kind) →
            buildable ls pack kind `shouldReturn` True

    it "reports a kind with art and no build entry as not buildable" $
        \(_, ls) → do
            evalDebug ls (registerFloorPack "artonly_pack" "art.png" False)
                `shouldReturn` "true"
            resolvedArtAt ls "artonly_pack" "floor" Nothing (simpleTile 0)
                `shouldReturn` Just (("art.png", 41), ("face.png", 42))
            buildable ls "artonly_pack" "floor" `shouldReturn` False

    it "reports nothing buildable for an unregistered pack" $ \(_, ls) →
        buildable ls "no_such_pack" "floor" `shouldReturn` False

-- | Requirement 7 and the review's "one TOTAL warning" correction.
warningSpec ∷ SpecWith (EngineEnv, LuaBackendState)
warningSpec = describe "a failure" $ do

    it "warns exactly once, naming the pack, kind and missing asset role" $
        \(env, ls) → do
            (r, entries) ← withCapturedLog env $
                evalDebug ls (registerWallish "warn_short" (drop 1 wallArtEntries))
            r `shouldBe` "false"
            case warningsOf entries of
                [w] → do
                    ("pack 'warn_short'" `T.isInfixOf` w) `shouldBe` True
                    ("kind 'wall'" `T.isInfixOf` w) `shouldBe` True
                    ("wall ne cap 00" `T.isInfixOf` w) `shouldBe` True
                other → expectationFailure
                    ("expected exactly one warning, got: " <> show other)

    it "names the pack, kind and role when an entry is unreadable" $
        \(env, ls) → do
            -- The other half of requirement 7: a payload that cannot be
            -- READ still reports through the same one-warning channel,
            -- and a missing asset has no path to name — so the ROLE is
            -- what identifies it.
            before ← readIORef (structureArtCatalogRef env)
            (r, entries) ← withCapturedLog env $ evalDebug ls $ T.concat
                [ "return tostring(structure.registerPackArt{ "
                , "pack='warn_unreadable', "
                , "kinds={{kind='floor', buildable=true}}, "
                , "art={{kind='floor', texture='a.png', "
                , "facemap='f.png', faceHandle=42}} })" ]
            r `shouldBe` "false"
            readIORef (structureArtCatalogRef env)
                ⌦ \after → sacPacks after `shouldBe` sacPacks before
            case warningsOf entries of
                [w] → do
                    ("pack 'warn_unreadable'" `T.isInfixOf` w) `shouldBe` True
                    ("kind 'floor'" `T.isInfixOf` w) `shouldBe` True
                    ("texHandle" `T.isInfixOf` w) `shouldBe` True
                other → expectationFailure
                    ("expected exactly one warning, got: " <> show other)

    it "names the pack for every malformed declared-kind entry" $
        \(env, ls) → do
            -- The `kinds` list is read BEFORE any art, and by then the
            -- payload has already named its pack — so a fault here must
            -- still name it. Both malformations of a kind entry are
            -- covered: an unrecognised `kind`, and the missing
            -- `buildable` boolean that must never be silently defaulted.
            let cases =
                    [ ( "unrecognised kind"
                      , "{kind='doorway', buildable=true}"
                      , Nothing )
                    , ( "absent buildable"
                      , "{kind='floor'}"
                      , Just "kind 'floor'" )
                    , ( "non-boolean buildable"
                      , "{kind='floor', buildable='yes'}"
                      , Just "kind 'floor'" ) ]
            forM_ cases $ \(label, kindEntry, mKindText) → do
                before ← readIORef (structureArtCatalogRef env)
                (r, entries) ← withCapturedLog env $ evalDebug ls $ T.concat
                    [ "return tostring(structure.registerPackArt{ "
                    , "pack='warn_kinds', kinds={", kindEntry, "}, "
                    , "art={{kind='floor', texture='a.png', texHandle=41, "
                    , "facemap='f.png', faceHandle=42}} })" ]
                r `shouldBe` "false"
                readIORef (structureArtCatalogRef env)
                    ⌦ \after → sacPacks after `shouldBe` sacPacks before
                case warningsOf entries of
                    [w] → forM_ ("pack 'warn_kinds'" : maybe [] pure mKindText)
                        $ \needle → unless (needle `T.isInfixOf` w)
                            $ expectationFailure
                                (label <> ": the warning does not name "
                                   <> show needle <> ": " <> T.unpack w)
                    other → expectationFailure
                        (label <> ": expected exactly one warning, got: "
                           <> show other)

    it "replaces the generic asset warning for a tracked texture, once" $
        \(env, ls) → do
            evalDebug ls (registerFloorPack "failing_pack" "doomed.png" True)
                `shouldReturn` "true"
            (_, first) ← withCapturedLog env $ assetFailed env ls "doomed.png"
            case warningsOf first of
                [w] → do
                    ("pack 'failing_pack'" `T.isInfixOf` w) `shouldBe` True
                    ("kind 'floor'" `T.isInfixOf` w) `shouldBe` True
                    ("doomed.png" `T.isInfixOf` w) `shouldBe` True
                    -- the generic #1690 line must NOT also appear
                    ("Asset load failed" `T.isInfixOf` w) `shouldBe` False
                other → expectationFailure
                    ("expected exactly one warning, got: " <> show other)
            -- the pack now resolves nothing at all…
            resolvedArtAt ls "failing_pack" "floor" Nothing (simpleTile 0)
                `shouldReturn` Nothing
            -- …silently, however often it is asked, and a repeat of the
            -- same failed asset says nothing more either.
            (_, again) ← withCapturedLog env $ do
                assetFailed env ls "doomed.png"
                replicateM_ 3 $
                    resolvedArtAt ls "failing_pack" "floor" Nothing (simpleTile 0)
            warningsOf again `shouldBe` []

    it "coalesces a SHARED texture's failure into one warning naming \
       \every pack it invalidated" $ \(env, ls) → do
            -- A facemap legitimately belongs to more than one pack, so a
            -- warning per affected pack would report ONE load failure
            -- twice. Two synthetic packs share `shared_face.png`, which
            -- is the shape the shipped packs really have (asserted
            -- against the two pack YAMLs in the example below).
            forM_ ["shared_a", "shared_b"] $ \pack →
                evalDebug ls (T.concat
                    [ "return tostring(structure.registerPackArt{ pack='"
                    , pack, "', kinds={{kind='floor', buildable=true}}, "
                    , "art={{kind='floor', texture='", pack, "_tex.png', "
                    , "texHandle=41, facemap='shared_face.png', "
                    , "faceHandle=42}} })" ])
                    `shouldReturn` "true"
            (_, entries) ← withCapturedLog env $
                assetFailed env ls "shared_face.png"
            case warningsOf entries of
                [w] → forM_ [ "shared_face.png", "pack 'shared_a'"
                            , "pack 'shared_b'", "kind 'floor'"
                            , "floor facemap" ] $ \needle →
                    unless (needle `T.isInfixOf` w) $ expectationFailure
                        ("the warning does not name " <> show needle
                           <> ": " <> T.unpack w)
                other → expectationFailure
                    ("expected exactly one warning, got: " <> show other)
            -- both packs are invalidated, not just the one named first
            forM_ ["shared_a", "shared_b"] $ \pack →
                resolvedArtAt ls pack "floor" Nothing (simpleTile 0)
                    `shouldReturn` Nothing

    it "shares a facemap between the two shipped packs, which is what \
       \makes that coalescing a real case" $ \(_, ls) → do
        shared ← evalDebug ls
            "local d = engine.loadYaml('data/structure_packs/dungeon_1.yaml'); \
            \local w = engine.loadYaml('data/structure_packs/wire.yaml'); \
            \return tostring(d.pieces.floor.facemap == w.facemap)"
        shared `shouldBe` "true"

    it "tells every Lua listener the diagnostic is already reported" $
        \(env, ls) → do
            -- The duplicate this closes: `onAssetFailed` still reaches
            -- `scripts/world_view.lua` through `ui_manager`, and that
            -- handler logs its own "World texture failed to load" line.
            -- One tracked failure must still be ONE observable warning,
            -- so the broadcast carries a fifth `reported` argument.
            --
            -- Observed through the REAL broadcast, by a module
            -- registered the REAL way (`engine.loadScript`, the same
            -- route `scripts/init_loader.lua` uses), which echoes the
            -- flag it was handed. The echo is this fixture's own line —
            -- the assertion separates it from the catalogue's.
            evalDebug ls (registerFloorPack "listener_pack" "listener.png" True)
                `shouldReturn` "true"
            withExclusiveTempDirectory "artcatalog_listener" $ \dir → do
                let observer = dir </> "asset_failure_observer.lua"
                writeFile observer $ unlines
                    [ "local M = {}"
                    , "function M.onAssetFailed(t, h, path, reason, reported)"
                    , "  engine.logWarn('observer ' .. tostring(path)"
                    , "    .. ' reported=' .. tostring(reported))"
                    , "end"
                    , "return M" ]
                -- Killed at the end: a loadScript'd module lives for the
                -- session, and a lingering observer would add its line to
                -- every later example's warning count.
                sid ← evalDebug ls $ T.concat
                    [ "return tostring(engine.loadScript('", T.pack observer
                    , "', 3600.0))" ]
                sid `shouldNotBe` "nil"
                let isEcho = T.isInfixOf "observer "
                    -- The logger prefixes each line with its source, so
                    -- compare from the echo's own first word.
                    echoes = map (snd ∘ T.breakOn "observer ")
                           ∘ filter isEcho
                    others = filter (not ∘ isEcho)
                -- A registered path: the catalogue owns the diagnostic,
                -- and Lua is told so.
                (_, first) ← withCapturedLog env $
                    assetFailed env ls "listener.png"
                echoes (warningsOf first)
                    `shouldBe` ["observer listener.png reported=true"]
                length (others (warningsOf first)) `shouldBe` 1
                -- A REPEAT emits no catalogue line at all, and Lua is
                -- still told the asset is spoken for — otherwise the
                -- suppressed duplicate comes back by the other route.
                (_, again) ← withCapturedLog env $
                    assetFailed env ls "listener.png"
                echoes (warningsOf again)
                    `shouldBe` ["observer listener.png reported=true"]
                others (warningsOf again) `shouldBe` []
                -- An untracked path keeps the generic engine line, and
                -- Lua keeps its own.
                (_, untracked) ← withCapturedLog env $
                    assetFailed env ls "not_registered_anywhere.png"
                echoes (warningsOf untracked) `shouldBe`
                    ["observer not_registered_anywhere.png reported=false"]
                length (others (warningsOf untracked)) `shouldBe` 1
                runLua ls ("engine.killScript(" <> sid <> ");")

    it "keeps world_view's readiness accounting while suppressing its \
       \duplicate line" $ \(env, ls) → do
        -- The production handler itself (`scripts/world_view.lua`), not
        -- a paraphrase of it. `reported` must suppress the LINE and
        -- nothing else: the count it advances is the gate #1690 exists
        -- to settle, and a failure that stopped counting would stall
        -- boot forever.
        runLua ls "local wv = require('scripts.world_view'); \
                  \wv.texturesNeeded = 1000; wv.texturesLoadedCount = 0; \
                  \wv.seenHandles = {}; wv.allHandles = {};"
        (_, quiet) ← withCapturedLog env $ runLua ls
            "require('scripts.world_view')\
            \  .onAssetFailed('texture', 7001, 'wv_quiet.png', 'boom', true);"
        warningsOf quiet `shouldBe` []
        evalDebug ls "return require('scripts.world_view').texturesLoadedCount"
            `shouldReturn` "1"
        (_, loud) ← withCapturedLog env $ runLua ls
            "require('scripts.world_view')\
            \  .onAssetFailed('texture', 7002, 'wv_loud.png', 'boom', false);"
        case warningsOf loud of
            [w] → ("World texture failed to load: wv_loud.png"
                     `T.isInfixOf` w) `shouldBe` True
            other → expectationFailure
                ("expected world_view's own warning, got: " <> show other)
        evalDebug ls "return require('scripts.world_view').texturesLoadedCount"
            `shouldReturn` "2"

    it "keeps the generic warning for a texture no pack registered" $
        \(env, ls) → do
            (_, entries) ← withCapturedLog env $
                assetFailed env ls "assets/textures/never_registered.png"
            case warningsOf entries of
                [w] → ("Asset load failed" `T.isInfixOf` w) `shouldBe` True
                other → expectationFailure
                    ("expected exactly one warning, got: " <> show other)

-- | Requirement 9: registering and resolving art for every real pack
--   interns nothing into the SAVED texture palette, and adds no runtime
--   handle entry either. Both refs are engine-global, so this is read
--   around the work rather than from a clean start.
paletteSpec ∷ SpecWith (EngineEnv, LuaBackendState)
paletteSpec = describe "registering and resolving unplaced art" $

    it "interns nothing into the texture palette" $ \(env, ls) → do
        palBefore ← readIORef (texPaletteRef env)
        hBefore   ← readIORef (texPaletteHandlesRef env)
        _ ← drainWorldQueue env
        runLua ls "require('scripts.structures').registerPackArt();"
        runLua ls "require('scripts.wire').registerPackArt();"
        forM_ [ (dungeonPack, "floor", Nothing)
              , (dungeonPack, "ceiling", Nothing)
              , (dungeonPack, "post", Nothing)
              , (dungeonPack, "wall", Just "ne")
              , (dungeonPack, "wall", Just "sw")
              , (wirePack, "wire", Nothing) ] $ \(p, k, e) → do
            got ← resolvedArtAt ls p k e (simpleTile 0)
            got `shouldSatisfy` isJust
        palAfter ← readIORef (texPaletteRef env)
        hAfter   ← readIORef (texPaletteHandlesRef env)
        tpPathToId palAfter `shouldBe` tpPathToId palBefore
        tpIdToPath palAfter `shouldBe` tpIdToPath palBefore
        tpNextId   palAfter `shouldBe` tpNextId   palBefore
        hAfter `shouldBe` hBefore
        leftover ← drainWorldQueue env
        map show leftover `shouldBe` []

-- | Requirement 8 and its acceptance bullet.
wireSpec ∷ SpecWith (EngineEnv, LuaBackendState)
wireSpec = describe "wire autotile parity (#1842)" $ do

    -- The Lua rule now DELEGATES to the engine, so asserting the two
    -- agree would assert nothing. What must hold is that the shared
    -- rule still produces the mapping #359 shipped -- so the
    -- expectation below is written out by hand from the naming
    -- convention (a tee names its MISSING side; corners and straights
    -- name the two sides they connect), not derived from either
    -- implementation. An edit to the rule and an edit to this table
    -- both fail.
    it "reproduces the sixteen-way mapping over the whole neighbour \
       \space, in Haskell and through Lua" $ \(_, ls) → do
        map fst expectedWireShapes `shouldMatchList` allWireNeighbors
        forM_ expectedWireShapes $ \(n, expected) → do
            wireShapeName (wireShapeFor n) `shouldBe` expected
            got ← evalDebug ls $ T.concat
                [ "return structure.wireShape(", luaBool (wnNorth n), ","
                , luaBool (wnEast n), ",", luaBool (wnSouth n), ","
                , luaBool (wnWest n), ")" ]
            got `shouldBe` expected

    it "names only connection variants the wire pack actually declares" $
        \(_, ls) → do
            declared ← evalDebug ls
                "local p = engine.loadYaml('data/structure_packs/wire.yaml'); \
                \local k = {}; for name, _ in pairs(p.connections) do \
                \k[#k+1] = name end; table.sort(k); \
                \return table.concat(k, ',')"
            sort (T.splitOn "," declared)
                `shouldBe` sort (map snd expectedWireShapes)

    it "resolves the art the builder places, for every neighbour \
       \combination" $ \(env, ls) →
        forM_ (zip [0 ..] allWireNeighbors) $ \(i, n) → do
            let tile@(x, y) = wireTile i
                neighbours = [ (x + dx, y + dy)
                             | (on, (dx, dy)) ←
                                 zip [wnNorth n, wnEast n, wnSouth n, wnWest n]
                                     [(0,-1),(1,0),(0,1),(-1,0)]
                             , on ]
            forM_ neighbours $ \t →
                runLua ls ("require('scripts.wire').place(" <> luaXY t <> ");")
            resolved ← resolvedArtAt ls wirePack "wire" Nothing tile
            placed   ← placedArtAt env ls
                ("require('scripts.wire').place(" <> luaXY tile <> ")") tile
            resolved `shouldBe` Just placed

    it "counts a DESIGNATED neighbour as connected, and only when asked" $
        \(env, ls) → do
            -- A tile with no placed wire anywhere near it: the engine's
            -- own designation map is the only thing that can connect it.
            let tile@(x, y) = (60, 30)
            ws ← installScene env
            writeIORef (wsTilesRef ws) (tilesAround tile)
            designateWire ws (x + 1, y)
            placedOnly ← wireNeighbours ls tile False
            withDesigns ← wireNeighbours ls tile True
            placedOnly `shouldBe` "ffff"
            withDesigns `shouldBe` "ftff"

    it "reads the same neighbour set through a u-alias of the tile" $
        \(env, ls) → do
            let tile@(x, y) = (60, 34)
                step = (worldSizeChunks `div` 2) * chunkSize
                alias = (x + step, y - step)
            ws ← installScene env
            writeIORef (wsTilesRef ws) (tilesAround tile)
            designateWire ws (x, y - 1)
            canonical ← wireNeighbours ls tile True
            aliased   ← wireNeighbours ls alias True
            canonical `shouldBe` "tfff"
            aliased   `shouldBe` canonical

-- * Scenario helpers

-- | @structure.wireNeighbors@'s four booleans as a @tf@ string, in
--   @n e s w@ order.
wireNeighbours ∷ LuaBackendState → (Int, Int) → Bool → IO Text
wireNeighbours ls tile withDesigns = evalDebug ls $ T.concat
    [ "local n = structure.wireNeighbors(", luaXY tile, ", nil, "
    , luaBool withDesigns, "); local s = ''; "
    , "for _, k in ipairs({'n','e','s','w'}) do "
    , "s = s .. (n[k] and 't' or 'f') end; return s" ]

-- | A wire designation written straight into the page's own map, which
--   is where 'World.Thread.Command.Cursor.Construct' puts one — the
--   designation TOOL is not what this example is about.
designateWire ∷ WorldState → (Int, Int) → IO ()
designateWire ws tile =
    writeIORef (wsConstructDesignationsRef ws) $ HM.singleton
        (canonicalTile worldSizeChunks (fst tile) (snd tile))
        (newConstructDesignation 0
            (CtStructure (StructurePiece wirePack "wire" Nothing))
            firstConstructAttemptId)

-- | Loaded chunks around one tile and its four neighbours, for the two
--   examples that step outside 'allScenarioTiles'.
tilesAround ∷ (Int, Int) → WorldTileData
tilesAround (x, y) = WorldTileData
    { wtdChunks = HM.fromList [ (c, flatChunkAt c) | c ← coords ]
    , wtdMaxChunks = max 1 (length coords) }
  where
    around = [ (x + dx, y + dy)
             | dx ← [-1 .. 1], dy ← [-1 .. 1] ]
    step = (worldSizeChunks `div` 2) * chunkSize
    coords = foldr ins [] [ fst (globalToChunk gx gy)
                          | (gx, gy) ← around
                                       ⧺ [ (gx + step, gy - step)
                                         | (gx, gy) ← around ] ]
    ins c cs | c `elem` cs = cs
             | otherwise   = c : cs

buildable ∷ LuaBackendState → Text → Text → IO Bool
buildable ls pack kind = do
    r ← evalDebug ls $ T.concat
        [ "return tostring(structure.isPackKindBuildable('", pack, "','"
        , kind, "'))" ]
    pure (r ≡ "true")

luaBool ∷ Bool → Text
luaBool b = if b then "true" else "false"


-- | Every four-neighbour combination and the connection variant it must
--   draw, written out from @data/structure_packs/wire.yaml@'s own key
--   names and #359's convention rather than from 'wireShapeFor' or from
--   @scripts/wire.lua@. Sixteen rows, in @(N, E, S, W)@ order.
expectedWireShapes ∷ [(WireNeighbors, Text)]
expectedWireShapes =
    [ (n False False False False, "isolated")
    , (n True  False False False, "end_n")
    , (n False True  False False, "end_e")
    , (n False False True  False, "end_s")
    , (n False False False True , "end_w")
    , (n True  False True  False, "straight_ns")
    , (n False True  False True , "straight_ew")
    , (n True  True  False False, "corner_ne")
    , (n True  False False True , "corner_nw")
    , (n False True  True  False, "corner_se")
    , (n False False True  True , "corner_sw")
      -- a tee is named by the side it does NOT connect to
    , (n False True  True  True , "tee_n")
    , (n True  False True  True , "tee_e")
    , (n True  True  False True , "tee_s")
    , (n True  True  True  False, "tee_w")
    , (n True  True  True  True , "cross")
    ]
  where n = WireNeighbors

-- | Deliver one terminal texture failure exactly as the asset thread
--   does — through the real engine-to-Lua dispatch, so the catalogue's
--   interaction with #1690's generic warning is the thing under test.
assetFailed ∷ EngineEnv → LuaBackendState → Text → IO ()
assetFailed env ls path = do
    stateRef ← newIORef ThreadRunning
    processLuaMsg env ls stateRef
        (LuaAssetFailed "texture" 4242 path "file not found")

-- * Synthetic registrations

-- | A one-kind pack: complete floor art, and whether its `build:` block
--   is complete. Handles 41/42 are arbitrary positive ids — the
--   catalogue only records them.
registerFloorPack ∷ Text → Text → Bool → Text
registerFloorPack pack tex isBuildable = T.concat
    [ "return tostring(structure.registerPackArt{ pack='", pack, "', "
    , "kinds={{kind='floor', buildable=", luaBool isBuildable, "}}, "
    , "art={{kind='floor', texture='", tex, "', texHandle=41, "
    , "facemap='face.png', faceHandle=42}} })" ]

-- | The sixteen wall art entries a complete `wall` declaration needs.
wallArtEntries ∷ [Text]
wallArtEntries =
    [ T.concat [ "{kind='wall', edge='", edgeName e, "', caps='", capCode c
               , "', texture='w_", edgeName e, ".png', texHandle=41, "
               , "facemap='f_", edgeName e, "_", capCode c
               , ".png', faceHandle=42}" ]
    | e ← allEdges, c ← allCapCodes ]

-- | A pack declaring `wall` with exactly the entries given — so a
--   caller can hand it a short or corrupted list.
registerWallish ∷ Text → [Text] → Text
registerWallish pack entries = T.concat
    [ "return tostring(structure.registerPackArt{ pack='", pack, "', "
    , "kinds={{kind='wall', buildable=true}}, art={"
    , T.intercalate "," entries, "} })" ]
