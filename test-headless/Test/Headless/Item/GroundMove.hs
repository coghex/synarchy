{-# LANGUAGE OverloadedStrings #-}
-- | @item.debugMoveGround(gid, instanceId, x, y [, pageId])@ relocates
--   ONE ground item in place, on the page it resolves, and refuses
--   everything else (#2486).
--
--   The primitive #2484's F8 grab gesture drives. A remove-then-respawn
--   is not an alternative and is why this exists at all:
--   'Item.Ground.spawnGroundItem' always mints a NEW gid, so the round
--   trip retires the id the caller, the selection and every persisted
--   reference already name. So every example here asserts the identity
--   as well as the position — a move that produced the right
--   coordinates under a new gid, or under a fresh instance, would look
--   identical from the boolean.
--
--   Driven through the REAL registered Lua verb against REAL manager
--   refs, the bare-Lua-backend technique of
--   'Test.Headless.Item.GroundSelection' and
--   'Test.Headless.Item.GroundPageOwnership': the page resolution, the
--   argument decoding and the destination rule all live at that
--   boundary, and a pure test structurally cannot see them. Pages are
--   in-memory 'World.State.Types.emptyWorldState's carrying
--   hand-built chunks, so two live worlds with discriminating terrain
--   cost no worldgen.
--
--   The terrain fixture is built so the three candidate elevations
--   DIFFER. 'surfaceZ' is each column's own @lcTerrainSurfaceMap@
--   entry — the elevation
--   'World.Render.GroundItemQuads.itemGeometry' rests a ground item at,
--   and by requirement 4 the only input to destination validity.
--   'decoyZ' is the camera's z-slice (really installed on the engine's
--   camera) and stands in for a pointer hit as well. 'restColumn' has
--   material ONLY at 'surfaceZ' and 'decoyColumn' ONLY at 'decoyZ', so
--   an implementation that validated against the camera slice or a
--   pointer elevation would accept and refuse exactly the opposite set.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Ground item move"'@.
module Test.Headless.Item.GroundMove (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.List (sort)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Camera (Camera2D(..), defaultCamera)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem)
import Item.Types (ItemInstance(..), emptyItemManager)
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Item.GroundSelection (runUnderHeldLock)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize, columnIndex)
import World.Cursor.Types (CursorState(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.GroundItems (moveGroundItemOnPage, takeGroundItemOnPage)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState )
import World.Tile.Types (WorldTileData(..))

-- * The fixture's terrain

-- | Chunks are stored u-wrapped into @[-32, 32)@ at this size, which is
--   what makes the seam-alias destination below a real alias rather
--   than a contrivance. Same size 'Test.Headless.World.Render.GroundItemSeam'
--   uses, for the same reason.
worldSize ∷ Int
worldSize = 64

-- | Every fixture column's own terrain surface: the ONLY elevation a
--   destination's validity may be read at.
surfaceZ ∷ Int
surfaceZ = 12

-- | The camera's z-slice, installed for real, and the elevation a
--   pointer hit would have reported. Deliberately not 'surfaceZ'.
decoyZ ∷ Int
decoyZ = 10

-- | Column z-range every fixture column spans.
columnDepth ∷ Int
columnDepth = 20

-- | A column whose material is nonzero at exactly the listed zs.
columnWith ∷ [Int] → ColumnTiles
columnWith zs = ColumnTiles
    { ctStartZ = 0
    , ctMats   = VU.generate columnDepth
                     (\z → if z `elem` zs then 1 else 0)
    , ctSlopes = VU.replicate columnDepth 0
    , ctVeg    = VU.replicate columnDepth 0
    }

-- | Material at the terrain surface and NOWHERE else: the one column
--   kind a move may land on.
restColumn ∷ ColumnTiles
restColumn = columnWith [surfaceZ]

-- | Material at the camera slice / pointer elevation and nowhere else.
--   An implementation reading either of those would accept this.
decoyColumn ∷ ColumnTiles
decoyColumn = columnWith [decoyZ]

-- | A column whose stored z-range is trimmed to @[5, 8)@ while its
--   terrain surface still reads 'surfaceZ' — the out-of-range index
--   'World.GroundItems.groundRestShift' must treat as no material
--   rather than indexing past @ctMats@.
trimmedColumn ∷ ColumnTiles
trimmedColumn = ColumnTiles
    { ctStartZ = 5
    , ctMats   = VU.replicate 3 1
    , ctSlopes = VU.replicate 3 0
    , ctVeg    = VU.replicate 3 0
    }

-- | Local coords, within any loaded chunk, of the two columns that must
--   refuse. Everything else in a fixture chunk is a 'restColumn'.
decoyLocal, trimmedLocal ∷ (Int, Int)
decoyLocal   = (1, 0)
trimmedLocal = (2, 0)

fixtureChunk ∷ ChunkCoord → LoadedChunk
fixtureChunk coord = LoadedChunk
    { lcCoord = coord
    , lcTiles = V.generate area $ \i →
        if      i ≡ uncurry columnIndex decoyLocal   then decoyColumn
        else if i ≡ uncurry columnIndex trimmedLocal then trimmedColumn
        else                                              restColumn
    , lcSurfaceMap        = VU.replicate area surfaceZ
    , lcTerrainSurfaceMap = VU.replicate area surfaceZ
    , lcFluidMap = V.replicate area Nothing
    , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
    , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
    , lcMagma = Nothing, lcStructures = emptyChunkStructures
    }
  where area = chunkSize * chunkSize

-- | The chunk holding the origin tiles, the negative-coordinate chunk,
--   and the one the seam alias resolves to. All three are already in
--   the canonical frame at 'worldSize', so the seam case is the only
--   one whose lookup shifts.
loadedCoords ∷ [ChunkCoord]
loadedCoords = [ChunkCoord 0 0, ChunkCoord (-1) (-1), seamStored]

-- | Chunk @(17, -15)@ has @u = 32@, one past the canonical range, and
--   is STORED here. The tile shift between the two frames is a whole
--   world: @(-512, +512)@.
seamStored ∷ ChunkCoord
seamStored = ChunkCoord (-15) 17

-- | A raw (alias-frame) tile inside that chunk, and the canonical tile
--   storing it.
seamRawTile, seamCanonTile ∷ (Int, Int)
seamRawTile   = (17 * chunkSize, (-15) * chunkSize)
seamCanonTile = ((-15) * chunkSize, 17 * chunkSize)

fixtureTiles ∷ WorldTileData
fixtureTiles = WorldTileData
    { wtdChunks = HM.fromList [(c, fixtureChunk c) | c ← loadedCoords]
    , wtdMaxChunks = 200
    }

-- * Fixture identities

pageActive, pageHidden ∷ WorldPageId
pageActive = WorldPageId "ground_move_active"
pageHidden = WorldPageId "ground_move_hidden"

-- | The item under test, and a bystander that must never move. Both
--   pages allocate from their own zero, so the SAME gids live on both —
--   the page-local collision (#1208) is the default here, not a
--   contrivance.
movedGid, bystanderGid ∷ Int
movedGid = 0
bystanderGid = 1

-- | Instance ids. The hidden page's row wears the same gid as the
--   active page's and a DIFFERENT instance, which is what a cross-page
--   lookup would confuse.
activeIid, bystanderIid, hiddenIid ∷ Word64
activeIid = 700
bystanderIid = 701
hiddenIid = 900

-- | Where both pages' items start: an ordinary interior tile of the
--   loaded origin chunk.
originX, originY ∷ Float
originX = 5.5
originY = 5.5

-- * The spec

spec ∷ Spec
spec = around (withIsolatedResourceRoot . withHeadlessEngineNoWorld) $ do

    describe "a move that takes" $ do

        it "repositions the row in place, preserving its gid, its whole \
           \instance, the allocator, every other row and the selection" $
            \env → do
                (ls, sc) ← moveBackend env
                before ← groundOf (scActive sc)
                move ls movedGid activeIid 0.25 0.75 Nothing
                    `shouldReturn` "true"
                after ← groundOf (scActive sc)
                -- The allocator never rewinds and never advances: this
                -- is not a spawn.
                gisNextId after `shouldBe` gisNextId before
                sort (HM.keys (gisItems after))
                    `shouldBe` [movedGid, bystanderGid]
                let moved = gisItems after HM.! movedGid
                (giX moved, giY moved) `shouldBe` (0.25, 0.75)
                -- The instance is carried over WHOLE — contents, fill,
                -- quality, condition and temperature included.
                giInst moved `shouldBe` giInst (gisItems before HM.! movedGid)
                gisItems after HM.! bystanderGid
                    `shouldBe` gisItems before HM.! bystanderGid
                selectionOn (scActive sc) `shouldReturn` Just movedGid

        it "answers exactly one value" $ \env → do
            (ls, _) ← moveBackend env
            evalOk ls "return select('#', item.debugMoveGround(0, 700, \
                      \0.25, 0.75)) == 1" `shouldReturn` "true"
            -- …and the refusal shape is one value too, which is what
            -- lets the debug console's tab-separated serialization be
            -- parsed as a bare boolean.
            evalOk ls "return select('#', item.debugMoveGround(0, 700, \
                      \0.25, 0.75, 'no_such_page')) == 1" `shouldReturn` "true"

    describe "page resolution" $ do

        it "moves on an explicitly named HIDDEN page, leaving the active \
           \page alone" $ \env → do
            (ls, sc) ← moveBackend env
            beforeActive ← groundOf (scActive sc)
            move ls movedGid hiddenIid 0.25 0.75 (Just pageHidden)
                `shouldReturn` "true"
            positionOf (scHidden sc) movedGid `shouldReturn` Just (0.25, 0.75)
            groundOf (scActive sc) `shouldReturn` beforeActive

        it "refuses an unknown page, touching neither live page" $ \env → do
            (ls, sc) ← moveBackend env
            refusalLeavesEverything sc $
                move ls movedGid activeIid 0.25 0.75
                     (Just (WorldPageId "ground_move_nowhere"))

        it "refuses everything when no page is active" $ \env → do
            (ls, sc) ← moveBackend env
            writeIORef (worldManagerRef env) emptyWorldManager
            move ls movedGid activeIid 0.25 0.75 Nothing
                `shouldReturn` "false"
            -- The pages are held directly, so they are still readable
            -- after the manager stopped naming them.
            positionOf (scActive sc) movedGid
                `shouldReturn` Just (originX, originY)

        it "re-resolves the active page on EVERY call, so a switch \
           \between two moves lands on the page that is active now" $
            \env → do
                (ls, sc) ← moveBackend env
                move ls movedGid activeIid 0.25 0.75 Nothing
                    `shouldReturn` "true"
                showOnly env pageHidden
                -- Same gid, but now it is the hidden page's row, so the
                -- hidden page's instance is the one that matches.
                move ls movedGid activeIid 3.5 3.5 Nothing
                    `shouldReturn` "false"
                move ls movedGid hiddenIid 3.5 3.5 Nothing
                    `shouldReturn` "true"
                positionOf (scActive sc) movedGid
                    `shouldReturn` Just (0.25, 0.75)
                positionOf (scHidden sc) movedGid
                    `shouldReturn` Just (3.5, 3.5)

        it "never selects across pages when the gid collides: the \
           \instance decides which page's row was meant" $ \env → do
            (ls, sc) ← moveBackend env
            beforeActive ← groundOf (scActive sc)
            beforeHidden ← groundOf (scHidden sc)
            -- The hidden page's instance, resolved against the ACTIVE
            -- page: that gid is live on both, and only a cross-page
            -- lookup would find the instance it names.
            move ls movedGid hiddenIid 0.25 0.75 Nothing
                `shouldReturn` "false"
            -- …and the active page's instance against the HIDDEN page.
            move ls movedGid activeIid 0.25 0.75 (Just pageHidden)
                `shouldReturn` "false"
            groundOf (scActive sc) `shouldReturn` beforeActive
            groundOf (scHidden sc) `shouldReturn` beforeHidden

    describe "argument refusals" $ do

        it "refuses a gid or an instanceId that is a numeric STRING" $
            \env → do
                (ls, sc) ← moveBackend env
                refusalLeavesEverything sc $ evalOk ls
                    "return item.debugMoveGround('0', 700, 0.25, 0.75)"
                refusalLeavesEverything sc $ evalOk ls
                    "return item.debugMoveGround(0, '700', 0.25, 0.75)"

        it "refuses a missing or unconvertible required argument" $
            \env → do
                (ls, sc) ← moveBackend env
                forM_ [ "return item.debugMoveGround()"
                      , "return item.debugMoveGround(0)"
                      , "return item.debugMoveGround(0, 700)"
                      , "return item.debugMoveGround(0, 700, 0.25)"
                      , "return item.debugMoveGround({}, 700, 0.25, 0.75)"
                      , "return item.debugMoveGround(0, {}, 0.25, 0.75)"
                      , "return item.debugMoveGround(0, 700, {}, 0.75)"
                      , "return item.debugMoveGround(0, 700, 0.25, {})"
                      , "return item.debugMoveGround(0, 700, 'x', 0.75)"
                      ] $ \src →
                    refusalLeavesEverything sc (evalOk ls src)

        it "refuses a gid the page has no row for, and a live gid whose \
           \instance does not match" $ \env → do
            (ls, sc) ← moveBackend env
            refusalLeavesEverything sc $
                move ls 77 activeIid 0.25 0.75 Nothing
            refusalLeavesEverything sc $
                move ls movedGid (activeIid + 1) 0.25 0.75 Nothing
            -- A negative instanceId is no instance: iiInstanceId is a
            -- Word64, and a wrapping conversion would make -1 name the
            -- largest one.
            refusalLeavesEverything sc $ evalOk ls
                "return item.debugMoveGround(0, -1, 0.25, 0.75)"

    describe "coordinate refusals" $ do

        it "refuses a non-finite coordinate on EITHER axis" $ \env → do
            (ls, sc) ← moveBackend env
            forM_ [ "0/0", "1/0", "-1/0" ] $ \bad → do
                refusalLeavesEverything sc $ evalOk ls
                    ("return item.debugMoveGround(0, 700, " <> bad
                                                <> ", 0.75)")
                refusalLeavesEverything sc $ evalOk ls
                    ("return item.debugMoveGround(0, 700, 0.25, " <> bad
                                                <> ")")

        it "refuses a finite Lua number whose narrowing to Float is not" $
            \env → do
                (ls, sc) ← moveBackend env
                refusalLeavesEverything sc $ evalOk ls
                    "return item.debugMoveGround(0, 700, 1e39, 0.75)"
                refusalLeavesEverything sc $ evalOk ls
                    "return item.debugMoveGround(0, 700, 0.25, -1e39)"

        it "refuses a coordinate past the frame bound, rather than \
           \wrapping into a chunk that happens to be loaded" $
            \env → do
                (ls, sc) ← moveBackend env
                -- THE discriminating pair. 2^24 is where a Float's
                -- spacing reaches a whole tile, so from there up a
                -- value names a RUN of tiles rather than one — and
                -- this particular pair is chosen so that flooring it
                -- anyway lands on chunk (1048576, -1048576), whose u
                -- is an exact multiple of the world and therefore
                -- wraps onto ChunkCoord 0 0: a LOADED chunk, at its
                -- local (0, 0), which is a resting column. Without the
                -- bound this call succeeds and teleports the item to
                -- tile (0, 0) — a destination the caller never named
                -- and the wrap fabricated.
                refusalLeavesEverything sc $ evalOk ls
                    "return item.debugMoveGround(0, 700, 16777216, \
                    \-16777216)"
                -- The rest of the domain above the bound, on each axis
                -- independently, including magnitudes where `floor`
                -- into an Int overflows outright.
                forM_ [ "16777216", "-16777216", "1e30", "3e38" ] $
                    \bad → do
                        refusalLeavesEverything sc $ evalOk ls
                            ("return item.debugMoveGround(0, 700, " <> bad
                                                        <> ", 0.75)")
                        refusalLeavesEverything sc $ evalOk ls
                            ("return item.debugMoveGround(0, 700, 0.25, "
                                                        <> bad <> ")")

        it "refuses a destination whose chunk is not loaded, without \
           \loading one" $ \env → do
            (ls, sc) ← moveBackend env
            refusalLeavesEverything sc $
                move ls movedGid activeIid 100.5 100.5 Nothing

        it "refuses when material exists only at the camera slice and \
           \the pointer elevation, and takes when it exists only at the \
           \destination's own terrain surface" $ \env → do
            (ls, sc) ← moveBackend env
            let (dx, dy) = decoyLocal
            refusalLeavesEverything sc $
                move ls movedGid activeIid
                     (fromIntegral dx + 0.5) (fromIntegral dy + 0.5) Nothing
            -- The accepting column has material at surfaceZ and NOTHING
            -- at the camera slice, so this is the same discrimination
            -- taken the other way.
            move ls movedGid activeIid 0.5 0.5 Nothing `shouldReturn` "true"
            positionOf (scActive sc) movedGid `shouldReturn` Just (0.5, 0.5)

        it "refuses a column whose terrain surface is outside its stored \
           \z-range, without raising" $ \env → do
            (ls, sc) ← moveBackend env
            let (tx, ty) = trimmedLocal
            refusalLeavesEverything sc $
                move ls movedGid activeIid
                     (fromIntegral tx + 0.5) (fromIntegral ty + 0.5) Nothing

    describe "canonical destinations" $ do

        it "stores a seam alias in the canonical frame, keeping its \
           \sub-tile fraction" $ \env → do
            (ls, sc) ← moveBackend env
            let (rawX, rawY) = seamRawTile
                (canX, canY) = seamCanonTile
            move ls movedGid activeIid
                 (fromIntegral rawX + 0.25) (fromIntegral rawY + 0.25)
                 Nothing `shouldReturn` "true"
            -- The shift is whole tiles, so the expected value is the
            -- narrowed input plus that shift — exactly, because both
            -- the shift and .25 are dyadic at this magnitude.
            positionOf (scActive sc) movedGid `shouldReturn`
                Just (fromIntegral canX + 0.25, fromIntegral canY + 0.25)

        it "stores a negative destination as given, keeping its fraction" $
            \env → do
                (ls, sc) ← moveBackend env
                move ls movedGid activeIid (-0.75) (-0.25) Nothing
                    `shouldReturn` "true"
                positionOf (scActive sc) movedGid
                    `shouldReturn` Just (-0.75, -0.25)

    -- The lock claim, gated rather than asserted, exactly as
    -- 'Test.Headless.Item.GroundSelection' gates the selection's: with
    -- the lock held, a lock-free implementation finishes immediately
    -- and a locked one cannot finish at all.
    describe "the page's ground-item lock" $ do

        it "makes a move wait for the lock, then commits against the map \
           \as it is at commit time" $ \env → do
            (_, sc) ← moveBackend env
            let ws = scActive sc
            committed ← runUnderHeldLock ws
                (moveGroundItemOnPage ws movedGid activeIid 0.25 0.75) $ do
                    -- Still where it started: nothing committed while
                    -- this thread holds the lock.
                    positionOf ws movedGid
                        `shouldReturn` Just (originX, originY)
                    -- Two writers that take NO lock land in the window:
                    -- a temperature update to the very row being moved,
                    -- and an unrelated insertion.
                    atomicModifyIORef' (wsGroundItemsRef ws) $ \gis →
                        ( gis { gisItems = HM.adjust
                                    (\gi → gi { giInst = (giInst gi)
                                        { iiTemp = Just 88 } })
                                    movedGid (gisItems gis) }
                        , () )
                    void $ atomicModifyIORef' (wsGroundItemsRef ws) $
                        spawnGroundItem (mkItem 702) 7 7
            committed `shouldBe` True
            after ← groundOf ws
            let moved = gisItems after HM.! movedGid
            (giX moved, giY moved) `shouldBe` (0.25, 0.75)
            -- The intervening instance update survives: the commit
            -- re-read the live row rather than writing back the copy it
            -- could have captured before the lock.
            iiTemp (giInst moved) `shouldBe` Just 88
            -- …as does the unrelated insertion, and its allocation.
            sort (HM.keys (gisItems after))
                `shouldBe` [movedGid, bystanderGid, 2]
            gisNextId after `shouldBe` 3

        it "refuses the second of two moves when the row is removed \
           \between them, without recreating it" $ \env → do
            (ls, sc) ← moveBackend env
            let ws = scActive sc
            move ls movedGid activeIid 0.25 0.75 Nothing
                `shouldReturn` "true"
            picked ← takeGroundItemOnPage ws movedGid
            isJust picked `shouldBe` True
            move ls movedGid activeIid 3.5 3.5 Nothing `shouldReturn` "false"
            -- The pickup is neither undone nor compensated for.
            (sort ∘ HM.keys ∘ gisItems <$> groundOf ws)
                `shouldReturn` [bystanderGid]
            (gisNextId <$> groundOf ws) `shouldReturn` 2

-- * Refusal discipline

-- | Run a refusing call and assert the WHOLE observable world is
--   exactly as it was: one @false@, both pages' rows and allocators,
--   the selection, and — the part a "just look it up" implementation
--   gets wrong — the loaded chunks and the chunk init queue of both
--   pages, which is what proves the verb never loaded or requested
--   terrain to make a destination valid.
refusalLeavesEverything ∷ Scene → IO Text → Expectation
refusalLeavesEverything sc act = do
    before ← snapshot sc
    got ← act
    got `shouldBe` "false"
    snapshot sc `shouldReturn` before

-- | Everything a refusal must not change, for both pages.
data PageSnapshot = PageSnapshot
    { psGround ∷ GroundItems
    , psSelect ∷ Maybe Int
    , psChunks ∷ WorldTileData
    , psQueue  ∷ [ChunkCoord]
    } deriving Eq

-- | Hand-written so a failure names the loaded chunk COORDS rather
--   than printing three whole chunks of column vectors.
instance Show PageSnapshot where
    show ps = "PageSnapshot { ground = " ⧺ show (psGround ps)
            ⧺ ", selection = " ⧺ show (psSelect ps)
            ⧺ ", loadedChunks = "
            ⧺ show (sort (HM.keys (wtdChunks (psChunks ps))))
            ⧺ ", initQueue = " ⧺ show (psQueue ps) ⧺ " }"

snapshot ∷ Scene → IO (PageSnapshot, PageSnapshot)
snapshot sc = (,) <$> onePage (scActive sc) <*> onePage (scHidden sc)
  where
    onePage ws = PageSnapshot
        <$> readIORef (wsGroundItemsRef ws)
        <*> (selectedGroundItem <$> readIORef (wsCursorRef ws))
        <*> readIORef (wsTilesRef ws)
        <*> readIORef (wsInitQueueRef ws)

-- * Driving the boundary

move ∷ LuaBackendState → Int → Word64 → Double → Double
     → Maybe WorldPageId → IO Text
move ls gid iid x y mPage = evalOk ls $
    "return item.debugMoveGround(" <> tshow gid <> ", " <> tshow iid
        <> ", " <> tshow x <> ", " <> tshow y
        <> maybe "" (\(WorldPageId p) → ", '" <> p <> "'") mPage <> ")"

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls src = do
    got ← executeDebugLua (lbsLuaState ls) src
    when ("error:" `T.isPrefixOf` got ∨ "syntax error:" `T.isPrefixOf` got) $
        expectationFailure ("Lua error from " ⧺ show src ⧺ ": " ⧺ T.unpack got)
    pure got

-- * Live-state readers

groundOf ∷ WorldState → IO GroundItems
groundOf ws = readIORef (wsGroundItemsRef ws)

positionOf ∷ WorldState → Int → IO (Maybe (Float, Float))
positionOf ws gid = do
    gis ← groundOf ws
    pure $ (\gi → (giX gi, giY gi)) <$> HM.lookup gid (gisItems gis)

selectionOn ∷ WorldState → IO (Maybe Int)
selectionOn ws = selectedGroundItem <$> readIORef (wsCursorRef ws)

-- * Fixture

data Scene = Scene { scActive ∷ WorldState, scHidden ∷ WorldState }

-- | Make @pid@ the only visible page, which is how the active page is
--   switched between two calls.
showOnly ∷ EngineEnv → WorldPageId → IO ()
showOnly env pid = atomicModifyIORef' (worldManagerRef env) $ \wm →
    (wm { wmVisible = [pid] }, ())

installPages ∷ EngineEnv → IO Scene
installPages env = do
    wsA ← emptyWorldState
    wsH ← emptyWorldState
    forM_ [wsA, wsH] $ \ws → do
        writeIORef (wsTilesRef ws) fixtureTiles
        writeIORef (wsGenParamsRef ws) $
            Just defaultWorldGenParams { wgpWorldSize = worldSize }
    -- Both pages allocate from their own zero, so the same gids are
    -- live on both by default rather than by contrivance.
    aIds ← forM [activeIid, bystanderIid] $ \iid →
        atomicModifyIORef' (wsGroundItemsRef wsA) $
            spawnGroundItem (mkItem iid) originX originY
    hIds ← forM [hiddenIid, hiddenIid + 1] $ \iid →
        atomicModifyIORef' (wsGroundItemsRef wsH) $
            spawnGroundItem (mkItem iid) originX originY
    -- Pins what every identity below assumes about a fresh page's
    -- allocator.
    aIds `shouldBe` [movedGid, bystanderGid]
    hIds `shouldBe` [movedGid, bystanderGid]
    -- A standing selection every refusal — and every success — has to
    -- leave exactly as it is.
    atomicModifyIORef' (wsCursorRef wsA) $ \cs →
        (cs { selectedGroundItem = Just movedGid }, ())
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageActive, wsA), (pageHidden, wsH)]
        , wmVisible = [pageActive] }
    writeIORef (itemManagerRef env) emptyItemManager
    -- The camera slice is really installed, at the elevation the decoy
    -- column has its material at: a verb reading it would accept that
    -- column and refuse the resting one.
    writeIORef (cameraRef env) defaultCamera { camZSlice = decoyZ }
    pure (Scene wsA wsH)

moveBackend ∷ EngineEnv → IO (LuaBackendState, Scene)
moveBackend env = do
    sc ← installPages env
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure (ls, sc)

-- | Every field is carried over by a move, so they are all given
--   distinguishable values rather than defaults.
mkItem ∷ Word64 → ItemInstance
mkItem iid = ItemInstance
    { iiDefName = "ground_move_marker", iiCurrentFill = 3
    , iiQuality = 62, iiCondition = 44, iiWeight = 1.5
    , iiSharpness = 37, iiContents = [], iiInstanceId = iid
    , iiTemp = Just 21, iiBulk = Just 1.0, iiStorage = Nothing
    }
