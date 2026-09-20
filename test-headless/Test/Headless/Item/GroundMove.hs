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
--   The terrain fixture is built so the three candidate elevations are
--   three DIFFERENT numbers, and so that each of the two wrong ones is
--   really in force rather than merely named:
--
--   * 'surfaceZ' (12) is every fixture column's own
--     @lcTerrainSurfaceMap@ entry — the elevation
--     'World.Render.GroundItemQuads.itemGeometry' rests a ground item
--     at, and by requirement 4 the only input to destination validity;
--   * 'pointerZ' (14) is what a REAL pointer hit resolves to. The
--     fixture installs a camera, a window and a framebuffer, finds the
--     screen pixel that actually hits the decoy tile, drives the
--     shipped @world.pickTile@ at that pixel, asserts the z it answers
--     is 'pointerZ', and records it through @world.selectTile@ — so
--     'World.Cursor.Types.worldSelectedTile' carries a genuine
--     pointer hit at that elevation while the move under test runs;
--   * 'cameraZ' (16) is the camera's own z-slice, really installed on
--     'Engine.Core.State.cameraRef'.
--
--   'restColumn' has material ONLY at 'surfaceZ', 'pointerDecoyColumn'
--   ONLY at 'pointerZ' and 'cameraDecoyColumn' ONLY at 'cameraZ', so an
--   implementation reading the pointer hit, or the camera slice, accepts
--   a column this verb must refuse — including one that consults the
--   pointer only when real pointer state is present, because it is.
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
import Engine.Core.State (EngineEnv(..), activeWorldStateFrom)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Camera (Camera2D(..), defaultCamera)
import World.Generate (viewDepth)
import World.Render.HitTest (pickWorldTile)
import World.Render.ViewBounds (computeViewBounds)
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
import World.Generate.Types
    (WorldGenParams(..), defaultWorldGenParams, isArenaParams)
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

-- | The elevation a REAL pointer hit resolves to on
--   'pointerDecoyColumn': below the camera slice, above the terrain
--   surface, and equal to neither.
pointerZ ∷ Int
pointerZ = 14

-- | The camera's own z-slice, really installed on
--   'Engine.Core.State.cameraRef'. Above everything else, so
--   'World.Render.HitTest.pickWorldTile''s downward search from it
--   reaches each fixture column's single material band.
cameraZ ∷ Int
cameraZ = 16

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

-- | Material at the elevation a real pointer hit reports, and nowhere
--   else. An implementation validating at the pointer's z accepts this.
pointerDecoyColumn ∷ ColumnTiles
pointerDecoyColumn = columnWith [pointerZ]

-- | Material at the camera's z-slice and nowhere else. An
--   implementation validating at the camera slice accepts this.
cameraDecoyColumn ∷ ColumnTiles
cameraDecoyColumn = columnWith [cameraZ]

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

-- | Local coords, within any loaded chunk, of the three columns that
--   must refuse. Everything else in a fixture chunk is a 'restColumn',
--   including local (0, 0) — the one destination that must be accepted.
pointerDecoyLocal, cameraDecoyLocal, trimmedLocal ∷ (Int, Int)
pointerDecoyLocal = (1, 0)
cameraDecoyLocal  = (2, 0)
trimmedLocal      = (3, 0)

fixtureChunk ∷ ChunkCoord → LoadedChunk
fixtureChunk coord = LoadedChunk
    { lcCoord = coord
    , lcTiles = V.generate area $ \i →
        if      i ≡ uncurry columnIndex pointerDecoyLocal
            then pointerDecoyColumn
        else if i ≡ uncurry columnIndex cameraDecoyLocal
            then cameraDecoyColumn
        else if i ≡ uncurry columnIndex trimmedLocal
            then trimmedColumn
        else     restColumn
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

pageActive, pageHidden, pageArena ∷ WorldPageId
pageActive = WorldPageId "ground_move_active"
pageHidden = WorldPageId "ground_move_hidden"

-- | A live page whose params are a real ARENA's: seed 0 and an empty
--   timeline, so 'World.Generate.Types.isArenaParams' recognises it,
--   and a @wgpWorldSize@ of 100000 that is a SENTINEL rather than an
--   extent.
pageArena = WorldPageId "ground_move_arena"

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
activeIid, bystanderIid, hiddenIid, arenaIid ∷ Word64
activeIid = 700
bystanderIid = 701
hiddenIid = 900
arenaIid = 1100

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

        it "refuses a column whose material sits only at the elevation a \
           \REAL pointer hit reports, with that hit established and \
           \three elevations in play" $ \env → do
            (ls, sc) ← moveBackend env
            -- A genuine hit-test, through the shipped verb, at the
            -- pixel that really resolves to this tile — then recorded
            -- as the pointer's selection, so worldSelectedTile carries
            -- a live pointer hit at pointerZ while the move runs.
            hitZ ← establishPointerHit env ls pointerDecoyLocal
            hitZ `shouldBe` pointerZ
            -- Three DIFFERENT numbers, which is what makes the verdict
            -- below attributable.
            sort [surfaceZ, pointerZ, cameraZ] `shouldBe` [12, 14, 16]
            refusalLeavesEverything sc $ moveToLocal ls pointerDecoyLocal

        it "refuses a column whose material sits only at the camera's \
           \z-slice" $ \env → do
            (ls, sc) ← moveBackend env
            -- On this column the downward search from the slice stops
            -- at the slice itself, so the pointer hit and the camera
            -- slice coincide here and the terrain surface is the odd
            -- one out.
            hitZ ← establishPointerHit env ls cameraDecoyLocal
            hitZ `shouldBe` cameraZ
            refusalLeavesEverything sc $ moveToLocal ls cameraDecoyLocal

        it "takes when material sits at the destination's own terrain \
           \surface, with the same pointer hit and camera slice \
           \standing" $ \env → do
            (ls, sc) ← moveBackend env
            -- The pointer's recorded hit is left pointing at the DECOY
            -- tile, at pointerZ, so acceptance here cannot be read off
            -- the pointer state either.
            _ ← establishPointerHit env ls pointerDecoyLocal
            moveToLocal ls restLocal `shouldReturn` "true"
            positionOf (scActive sc) movedGid `shouldReturn` Just (0.5, 0.5)

        it "refuses a column whose terrain surface is outside its stored \
           \z-range, without raising" $ \env → do
            (ls, sc) ← moveBackend env
            refusalLeavesEverything sc $ moveToLocal ls trimmedLocal

        it "refuses a far ARENA destination rather than wrapping it by \
           \the arena's sentinel world size onto a loaded chunk" $
            \env → do
                (ls, sc) ← moveBackend env
                -- An arena's wgpWorldSize is the sentinel 100000, not an
                -- extent, and the chunk loader stores an arena's chunks
                -- under their own coords: canonicalChunkCoord answers
                -- identity for it. Wrapping by the sentinel instead
                -- takes chunk (50000, -50000) — u = 100000, an exact
                -- multiple of it — straight onto ChunkCoord 0 0, which
                -- this page HAS loaded, at local (0, 0), a resting
                -- column. So tile (800000, -800000) would be accepted
                -- and the item stored 800000 tiles away from where the
                -- caller put it, at (0, 0), on a page with no wrap at
                -- all.
                (arenaFarX, arenaFarY) `shouldBe` (800000, -800000)
                refusalLeavesEverything sc $ move ls movedGid arenaIid
                    (fromIntegral arenaFarX + 0.5)
                    (fromIntegral arenaFarY + 0.5) (Just pageArena)
                -- The arena page is otherwise perfectly usable, so the
                -- refusal above is the coordinate's and not the page's.
                move ls movedGid arenaIid 0.5 0.5 (Just pageArena)
                    `shouldReturn` "true"
                positionOf (scArena sc) movedGid
                    `shouldReturn` Just (0.5, 0.5)

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

-- * Destinations and the pointer hit

-- | The one local column a move may land on.
restLocal ∷ (Int, Int)
restLocal = (0, 0)

-- | Move the item onto the centre of a local column of the origin
--   chunk, whose local coords are also its global tile coords.
moveToLocal ∷ LuaBackendState → (Int, Int) → IO Text
moveToLocal ls (lx, ly) = move ls movedGid activeIid
    (fromIntegral lx + 0.5) (fromIntegral ly + 0.5) Nothing

-- | Establish a REAL pointer hit on the given local tile and answer the
--   elevation it resolved to.
--
--   Not a hand-written number: it finds the screen pixel that the
--   shipped hit-test actually resolves to that tile, then drives the
--   shipped @world.pickTile@ verb at that pixel — the synchronous
--   screen-pixel hit-test a click path runs — and asserts the tile and
--   the z it answers.
--
--   That answer is then installed into the cursor fields a click
--   leaves behind: 'World.Cursor.Types.worldSelectedTile' (carrying the
--   hit's own z), 'worldHoverTile' and 'worldHoverPos'. Written
--   directly rather than through @world.selectTile@ because that verb
--   only ENQUEUES @WorldSelectTileByCoord@ for the world thread, which
--   this fixture does not run; the state installed here is the state
--   that handler writes. So a live pointer hit, at an elevation that is
--   neither the terrain surface nor the camera slice, is present for
--   the whole of the move that follows.
establishPointerHit ∷ EngineEnv → LuaBackendState → (Int, Int) → IO Int
establishPointerHit env ls tile = do
    mPix ← pixelForTile env tile
    case mPix of
        Nothing → do
            expectationFailure ("no screen pixel resolves to tile "
                                   ⧺ show tile)
            error "unreachable"
        Just (px, py) → do
            got ← evalOk ls $
                "local gx, gy, z = world.pickTile(" <> tshow px <> ", "
                    <> tshow py <> "); if not gx then return 'none' end; "
                    <> "return gx .. ',' .. gy .. ',' .. z"
            (gx, gy, z) ← case T.splitOn "," (T.filter (≢ '"') got) of
                [a, b, c] → pure ( read (T.unpack a) ∷ Int
                                 , read (T.unpack b) ∷ Int
                                 , read (T.unpack c) ∷ Int )
                _ → do
                    expectationFailure
                        ("world.pickTile answered " ⧺ show got)
                    error "unreachable"
            (gx, gy) `shouldBe` tile
            ws ← activePage env
            atomicModifyIORef' (wsCursorRef ws) $ \cs →
                ( cs { worldSelectedTile = Just (gx, gy, z)
                     , worldHoverTile    = Just (gx, gy)
                     , worldHoverPos     = Just ( fromIntegral gx + 0.5
                                                , fromIntegral gy + 0.5 ) }
                , () )
            pure z

-- | The screen pixel whose hit-test resolves to @tile@, found by
--   running the very function @world.pickTile@ runs over the installed
--   viewport. Coarse steps: any pixel inside the tile answers.
pixelForTile ∷ EngineEnv → (Int, Int) → IO (Maybe (Int, Int))
pixelForTile env tile = do
    camera ← readIORef (cameraRef env)
    (winW, winH) ← readIORef (windowSizeRef env)
    (fbW, fbH) ← readIORef (framebufferSizeRef env)
    let zoom = camZoom camera
        (camX, camY) = camPosition camera
        effectiveDepth = min viewDepth
            (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
        vb = computeViewBounds camera fbW fbH effectiveDepth
        hits = [ (px, py)
               | py ← [0 .. winH - 1], px ← [0 .. winW - 1]
               , Just (gx, gy, _, _, _) ←
                     [pickWorldTile (camFacing camera) zoom
                          (camZSlice camera) camX camY fbW fbH winW winH
                          worldSize effectiveDepth vb fixtureTiles px py]
               , (gx, gy) ≡ tile ]
    pure $ listToMaybe hits

-- | The page @world.pickTile@ and the verb under test both resolve to.
activePage ∷ EngineEnv → IO WorldState
activePage env = do
    mWs ← activeWorldStateFrom (worldManagerRef env)
    case mWs of
        Just ws → pure ws
        Nothing → do
            expectationFailure "the fixture has no active page"
            error "unreachable"

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

snapshot ∷ Scene → IO (PageSnapshot, PageSnapshot, PageSnapshot)
snapshot sc = (,,) <$> onePage (scActive sc) <*> onePage (scHidden sc)
                   <*> onePage (scArena sc)
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

data Scene = Scene
    { scActive ∷ WorldState
    , scHidden ∷ WorldState
    , scArena  ∷ WorldState
    }

-- | Make @pid@ the only visible page, which is how the active page is
--   switched between two calls.
showOnly ∷ EngineEnv → WorldPageId → IO ()
showOnly env pid = atomicModifyIORef' (worldManagerRef env) $ \wm →
    (wm { wmVisible = [pid] }, ())

installPages ∷ EngineEnv → IO Scene
installPages env = do
    wsA ← emptyWorldState
    wsH ← emptyWorldState
    wsR ← emptyWorldState
    forM_ [wsA, wsH, wsR] $ \ws →
        writeIORef (wsTilesRef ws) fixtureTiles
    forM_ [wsA, wsH] $ \ws →
        writeIORef (wsGenParamsRef ws) $
            -- Seed 42, so 'isArenaParams' is False and these are
            -- ordinary wrapping pages.
            Just defaultWorldGenParams { wgpWorldSize = worldSize }
    -- The arena's params verbatim from
    -- 'World.Thread.Command.Init.handleWorldInitArenaCommand'.
    writeIORef (wsGenParamsRef wsR) $ Just arenaParams
    -- Both pages allocate from their own zero, so the same gids are
    -- live on both by default rather than by contrivance.
    aIds ← forM [activeIid, bystanderIid] $ \iid →
        atomicModifyIORef' (wsGroundItemsRef wsA) $
            spawnGroundItem (mkItem iid) originX originY
    hIds ← forM [hiddenIid, hiddenIid + 1] $ \iid →
        atomicModifyIORef' (wsGroundItemsRef wsH) $
            spawnGroundItem (mkItem iid) originX originY
    rIds ← forM [arenaIid, arenaIid + 1] $ \iid →
        atomicModifyIORef' (wsGroundItemsRef wsR) $
            spawnGroundItem (mkItem iid) originX originY
    -- Pins what every identity below assumes about a fresh page's
    -- allocator.
    aIds `shouldBe` [movedGid, bystanderGid]
    hIds `shouldBe` [movedGid, bystanderGid]
    rIds `shouldBe` [movedGid, bystanderGid]
    -- The fixture really is an arena by the engine's own predicate, not
    -- by resembling one.
    isArenaParams arenaParams `shouldBe` True
    -- A standing selection every refusal — and every success — has to
    -- leave exactly as it is.
    atomicModifyIORef' (wsCursorRef wsA) $ \cs →
        (cs { selectedGroundItem = Just movedGid }, ())
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [ (pageActive, wsA), (pageHidden, wsH)
                      , (pageArena, wsR) ]
        , wmVisible = [pageActive] }
    writeIORef (itemManagerRef env) emptyItemManager
    -- The camera's z-slice is really installed, at the elevation
    -- 'cameraDecoyColumn' has its material at, and a viewport goes with
    -- it so the shipped hit-test can run for real against this page.
    writeIORef (cameraRef env) defaultCamera
        { camZSlice = cameraZ, camZoom = fixtureZoom }
    writeIORef (windowSizeRef env) (viewportW, viewportH)
    writeIORef (framebufferSizeRef env) (viewportW, viewportH)
    pure (Scene wsA wsH wsR)

moveBackend ∷ EngineEnv → IO (LuaBackendState, Scene)
moveBackend env = do
    sc ← installPages env
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure (ls, sc)

-- | A far arena tile whose chunk, wrapped by the arena SENTINEL, lands
--   exactly on the loaded origin chunk: chunk (50000, -50000) has
--   u = 100000, an exact multiple of the sentinel, so it wraps to
--   ChunkCoord 0 0 at local (0, 0).
arenaFarX, arenaFarY ∷ Int
arenaFarX = 50000 * chunkSize
arenaFarY = (-50000) * chunkSize

-- | A real arena's generation params.
arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams
    { wgpSeed = 0, wgpWorldSize = arenaSentinelSize }

-- | The sentinel an arena records instead of an extent.
arenaSentinelSize ∷ Int
arenaSentinelSize = 100000

-- | The installed viewport. Square, so the hit-test's aspect is 1 and
--   the scan below covers the same span on both axes.
viewportW, viewportH ∷ Int
viewportW = 640
viewportH = 640

-- | Zoomed in far enough that one tile spans many pixels, so the scan
--   above lands inside the small cluster of fixture columns.
fixtureZoom ∷ Float
fixtureZoom = 8.0

-- | Every field is carried over by a move, so they are all given
--   distinguishable values rather than defaults.
mkItem ∷ Word64 → ItemInstance
mkItem iid = ItemInstance
    { iiDefName = "ground_move_marker", iiCurrentFill = 3
    , iiQuality = 62, iiCondition = 44, iiWeight = 1.5
    , iiSharpness = 37, iiContents = [], iiInstanceId = iid
    , iiTemp = Just 21, iiBulk = Just 1.0, iiStorage = Nothing
    }
