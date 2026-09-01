{-# LANGUAGE Strict #-}

-- | Dig-progress edit handler and its spoil-pile bookkeeping (yield
--   item spawning, full-pile terrain promotion). Split out of
--   "World.Thread.Command.Edit" (issue #563).
module World.Thread.Command.Edit.Dig
    ( handleWorldDigTileCommand
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv, freshItemInstanceId)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Unit.Command.Types (UnitCommand(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Generate.Coordinates (globalToChunk, canonicalTileFrame)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Edit.Apply (applyEdit)
import World.Material (MaterialProps(..), getMaterialProps
                      , materialIdByName)
import World.Material.Id (MaterialId(..))
import World.Mine.Apply (applyDigSlopeToChunk)
import World.Mine.Types (MineDesignation(..), drainCorners, cornersDone)
import World.Gem (gemChanceAt)
import World.Spoil.Logic (spoilTileOk, spoilStartVertex)
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem)
import Item.Materialize (materializeItem, pristineItem)
import Item.Types (lookupItemDef)
import System.Random (StdGen, randomR)
import World.Spoil.Types (SpoilPile(..), spoilCapacity, depositSpoil
                         , candidateVertices, promotableTiles
                         , debitPromotedTile, tileCornerVertices)
import World.Thread.Command.Edit.Terrain (handleWorldDeleteTileCommand)
import World.Thread.Command.Edit.Sync (syncEditToSim)
import World.Plant.Validate (revalidatePlantDesignations)
import World.Construct.Revalidate
    (ConstructScope(..), revalidateConstructDesignations)
import World.Flora.Designation (replaceChunkForgettingFlora)

-- | Apply dig progress to the designated tile at (gx, gy).
--
--   The digger's position picks the drain order (digger-side corners
--   first — 'drainCorners'); the partial state writes its slope-mask
--   override into the loaded chunk ('applyDigSlopeToChunk') so the
--   tile renders progressively excavated. When every corner reaches
--   zero, the tile drops one z through the regular delete-tile path
--   (edit log + replay + save survival all included) and the
--   designation is removed.
--
--   Spoil: when the dug material declares dig_spoil, the excavated
--   volume × dig_bulking is routed into the vertex piles around the
--   dig (World.Spoil). If the surrounding piles can't absorb the
--   tick's spoil, the dig REFUSES (no drain — material never
--   vanishes; the AI sees the blocked flag via getDigInfoAt). Tiles
--   whose four corners complete a full pile level are promoted to
--   real terrain through the WeAddTile edit path.
--
--   No-ops when the tile isn't designated (e.g. two diggers raced and
--   one finished it) or its chunk isn't loaded.
handleWorldDigTileCommand ∷ EngineEnv → IORef StdGen → Q.Queue UnitCommand
    → LoggerState → WorldPageId
    → Int → Int → Float → Float → Float → Float → Float → IO ()
handleWorldDigTileCommand env rngRef unitQ logger pageId rawGX rawGY rawUX rawUY
                          amount skill percep = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for dig tile: " <> unWorldPageId pageId
        Just ws → do
            -- #1175: the mine designation this consumes is stored under a
            -- CANONICAL key, so the tile is resolved into that frame before
            -- anything is looked up — a dig job coord restored from a
            -- pre-#1175 save can still be a u-alias, and a raw lookup would
            -- silently find no designation and never progress it. The
            -- digger's own position takes the SAME whole-tile shift: the wrap
            -- is an isometry, so "which corner of the tile is the digger
            -- nearest" (spoilStartVertex) stays the answer it was. Identity
            -- away from the seam.
            worldSize ← pageWrapWorldSize ws
            let (digCoord, (digLx, digLy), (dgx, dgy)) =
                    canonicalTileFrame worldSize rawGX rawGY
                digIdx = columnIndex digLx digLy
                gx = rawGX + dgx
                gy = rawGY + dgy
                ux = rawUX + fromIntegral dgx
                uy = rawUY + fromIntegral dgy
            desigs ← readIORef (wsMineDesignationsRef ws)
            case HM.lookup (gx, gy) desigs of
                Nothing → pure ()
                Just md → do
                    td0 ← readIORef (wsTilesRef ws)
                    registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
                    piles ← readIORef (wsSpoilRef ws)
                    let oldCorners = mdCorners md
                        sumC (a, b, c, d) = a + b + c + d
                        -- Properties of the dug material (the
                        -- column's cell at the designation z).
                        mDigProps = do
                            lc ← lookupChunk digCoord td0
                            let col  = lcTiles lc V.! digIdx
                                relZ = mdZ md - ctStartZ col
                            matId ← if relZ ≥ 0
                                       ∧ relZ < VU.length (ctMats col)
                                    then Just (ctMats col VU.! relZ)
                                    else Nothing
                            pure (getMaterialProps registry
                                      (MaterialId matId))
                        mSpoil = do
                            props ← mDigProps
                            spoilName ← mpDigSpoil props
                            spoilId ← materialIdByName registry spoilName
                            pure (spoilId, mpDigBulking props)
                        mChunkItem = mDigProps ⌦ mpDigChunk
                        tileOk = spoilTileOk td0 desigs (mdZ md)
                        startV = spoilStartVertex (ux, uy) (gx, gy)
                        -- Refusal gate: this tick's worst-case spoil
                        -- must fit before anything drains.
                        plannedSpoil = case mSpoil of
                            Nothing → 0
                            Just (_, bulking) →
                                min amount (sumC oldCorners) * bulking
                        capacity = case mSpoil of
                            Nothing → 0
                            Just (spoilId, _) →
                                spoilCapacity tileOk spoilId startV piles
                        blocked = plannedSpoil > 0 ∧ capacity < plannedSpoil
                    if blocked
                      then logDebug logger CatWorld $
                             "Dig blocked (no spoil room) at "
                               <> tshow gx <> "," <> tshow gy
                      else do
                        let corners' = drainCorners (ux, uy) (gx, gy)
                                                    amount oldCorners
                            drained  = sumC oldCorners - sumC corners'
                        -- Route the spoil before the tile mutates so
                        -- the legality predicate sees the pre-dig
                        -- world (the dig tile is excluded by its own
                        -- designation either way).
                        case mSpoil of
                            Nothing → pure ()
                            Just (spoilId, bulking) | drained > 0 → do
                                let (piles', leftover) = depositSpoil
                                        tileOk spoilId startV
                                        (drained * bulking) piles
                                when (leftover > 0.001) $
                                    logWarn logger CatWorld $
                                        "Spoil leftover "
                                          <> tshow leftover
                                          <> " despite capacity check at "
                                          <> tshow gx <> ","
                                          <> tshow gy
                                writeIORef (wsSpoilRef ws) piles'
                                -- Promote any tile whose corners
                                -- completed a full level.
                                promoteFullSpoilTiles env unitQ logger
                                    pageId ws startV
                            _ → pure ()
                        -- Chunk-yield accumulator: deterministic, per
                        -- tile, scaled by the CURRENT digger's mining
                        -- skill each tick (0.5 + skill/100 chunks per
                        -- full tile = 4 corner-units). Whole chunks
                        -- spawn as ground items at the dig site; the
                        -- fractional remainder rides on the
                        -- designation (and dies with it — one tile
                        -- only provides what was extracted from it).
                        chunkRemainder ← case mChunkItem of
                            Nothing → pure (mdChunkProgress md)
                            Just chunkDef | drained > 0 → do
                                let rate = (0.5 + skill / 100) / 4
                                    p    = mdChunkProgress md
                                         + drained * rate
                                    n    = floor p ∷ Int
                                when (n > 0) $
                                    spawnYieldItems env rngRef logger ws
                                        chunkDef (gx, gy) n
                                pure (p - fromIntegral n)
                            _ → pure (mdChunkProgress md)
                        if cornersDone corners'
                          then do
                            -- Gem roll, once per COMPLETED tile: the
                            -- seeded region field says which gem (if
                            -- any) this area hosts and how rich it
                            -- runs; the finishing digger's PERCEPTION
                            -- scales the find chance (spotting the
                            -- glint — deliberately not mining skill).
                            when (maybe False mpDigGems mDigProps) $ do
                                paramsM ← readIORef (wsGenParamsRef ws)
                                let seed = maybe 0 (fromIntegral ∘ wgpSeed)
                                                 paramsM
                                case gemChanceAt seed (gx, gy) percep of
                                    Nothing → pure ()
                                    Just (gemDef, chance) → do
                                        roll ← atomicModifyIORef'
                                            rngRef $ \g →
                                            let (v, g') = randomR
                                                    (0, 1 ∷ Float) g
                                            in (g', v)
                                        when (roll < chance) $
                                            spawnYieldItems env rngRef
                                                logger ws gemDef (gx, gy) 1
                            atomicModifyIORef' (wsMineDesignationsRef ws) $ \m →
                                (HM.delete (gx, gy) m, ())
                            handleWorldDeleteTileCommand env logger pageId gx gy
                          else do
                            let md' = md { mdCorners = corners'
                                         , mdChunkProgress = chunkRemainder }
                            atomicModifyIORef' (wsMineDesignationsRef ws) $ \m →
                                (HM.insert (gx, gy) md' m, ())
                            td ← readIORef (wsTilesRef ws)
                            case lookupChunk digCoord td of
                                Nothing → pure ()
                                Just lc → do
                                    let lc' = applyDigSlopeToChunk (gx, gy) md' lc
                                    -- #1854 requirement 16: an edit that takes the tile's
                                    -- rooted flora with it must take that plant's
                                    -- designation and regrowth timer too, or an orphan
                                    -- entry outlives the plant it addressed.
                                    replaceChunkForgettingFlora ws lc lc'
                                    bumpQuadCacheGen ws
                                    writeIORef (wsZoomQuadCacheRef ws) Nothing
                                    writeIORef (wsBgQuadCacheRef ws)   Nothing
                                    -- #1858: a PARTIAL dig sheds the
                                    -- tile's surface vegetation as soon
                                    -- as one corner drops
                                    -- ('applyDigSlopeToChunk'), and mine
                                    -- admission does not exclude a tile
                                    -- carrying a plant designation — so
                                    -- this write, not the eventual tile
                                    -- deletion, is where such a tile
                                    -- stops being tilled soil.
                                    _ ← revalidatePlantDesignations logger ws
                                    -- #1844: the dig moved this tile's
                                    -- resolved surface, so its own
                                    -- structure designation (if any) is
                                    -- re-checked against the 'cdZ' it
                                    -- captured. Scoped to the dug tile.
                                    _ ← revalidateConstructDesignations
                                            env logger ws
                                            (ConstructKeys [(gx, gy)])
                                    pure ()

-- | Spawn @n@ yield items (chunks, gems) as ground items scattered
--   on the dig tile. Each gets a random sub-tile position, retried a
--   few times to keep ≥ 0.15 tiles from existing ground items so
--   finds lay out as a scatter instead of a stack.
--
--   Every instance VALUE is "Item.Materialize"'s to decide (#1418) —
--   this path contributes no override. @rngRef@ stays the explicit
--   narrow parameter it has always been (see
--   "Engine.Core.Capability.UnitCombat"), and the materializer takes it
--   as a parameter for exactly that reason: the world thread must not
--   acquire unit/combat access by way of minting an item.
spawnYieldItems ∷ EngineEnv → IORef StdGen → LoggerState → WorldState → Text
                → (Int, Int) → Int → IO ()
spawnYieldItems env rngRef logger ws defName (gx, gy) n = do
    -- Dig-yield item defs come through the `content-registries`
    -- READER view (#890, narrowed to read-only handles by #1896).
    itemMgr ← readReadOnlyRef
        (crvItemManagerRef (toContentRegistriesViewCapability env))
    case lookupItemDef defName itemMgr of
        Nothing →
            logWarn logger CatWorld $
                "Dig yield: unknown item def '" <> defName
                  <> "' — dropping " <> tshow n
        Just _ → forM_ [1 .. n] $ \_ → do
            mInst ← materializeItem itemMgr logger rngRef
                        (freshItemInstanceId env) pristineItem defName
            case mInst of
                Nothing → pure ()
                Just inst → do
                    gis ← readIORef (wsGroundItemsRef ws)
                    (px, py) ← pickScatterPos gis
                    _ ← atomicModifyIORef' (wsGroundItemsRef ws) $
                            spawnGroundItem inst px py
                    pure ()
  where
    -- Up to 6 candidate offsets inside the tile; first one clear of
    -- existing items wins, last candidate is the fallback.
    pickScatterPos gis = go (6 ∷ Int)
      where
        clearOf (px, py) = all (\gi →
            let dx = giX gi - px
                dy = giY gi - py
            in dx * dx + dy * dy ≥ 0.15 * 0.15)
            (HM.elems (gisItems gis))
        go k = do
            ox ← atomicModifyIORef' rngRef $ \g →
                let (v, g') = randomR (0.15, 0.85 ∷ Float) g in (g', v)
            oy ← atomicModifyIORef' rngRef $ \g →
                let (v, g') = randomR (0.15, 0.85 ∷ Float) g in (g', v)
            let pos = (fromIntegral gx + ox, fromIntegral gy + oy)
            if k ≤ 1 ∨ clearOf pos
              then pure pos
              else go (k - 1)

-- | Compact every spoil tile around @startV@ whose four corners hold
--   a full level: raise the terrain one z via the WeAddTile edit
--   (live mutation + log append, same single-source applyEdit as
--   delete) and debit the contributing piles. Loops because a debit
--   never re-fills a corner — one pass per promoted tile is enough,
--   but promoting one tile can't complete another, so a single sweep
--   over the candidate set suffices.
promoteFullSpoilTiles ∷ EngineEnv → Q.Queue UnitCommand → LoggerState
    → WorldPageId → WorldState → (Int, Int) → IO ()
promoteFullSpoilTiles env unitQ logger pageId ws startV = do
    piles ← readIORef (wsSpoilRef ws)
    _registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
    let ready = promotableTiles piles (candidateVertices startV)
    forM_ ready $ \tile@(tx, ty) → do
        ps ← readIORef (wsSpoilRef ws)
        -- Material of the promoted cell = the pile material at the
        -- tile's first corner. All four corners are guaranteed to share
        -- one material: slotUsable refuses to fill a tile's corner with
        -- a material that differs from spoil already on the tile.
        let mMat = listToMaybe
                [ spMat p
                | (v, _) ← tileCornerVertices tile
                , Just p ← [HM.lookup v ps] ]
        case mMat of
            Nothing → pure ()
            Just mat → do
                let (coord, _) = globalToChunk tx ty
                    edit = WeAddTile tx ty mat
                td ← readIORef (wsTilesRef ws)
                case lookupChunk coord td of
                    Nothing → pure ()
                    Just lc → do
                        let lc' = applyEdit edit lc
                        -- #1854 requirement 16: an edit that takes the tile's
                        -- rooted flora with it must take that plant's
                        -- designation and regrowth timer too, or an orphan
                        -- entry outlives the plant it addressed.
                        replaceChunkForgettingFlora ws lc lc'
                        atomicModifyIORef' (wsEditsRef ws) $ \es →
                            (appendEdit coord edit es, ())
                        atomicModifyIORef' (wsSpoilRef ws) $ \sp →
                            (debitPromotedTile tile sp, ())
                        -- The same WeAddTile the ordinary add-tile path
                        -- applies, so it joins the same re-seed +
                        -- freshness handoff (#1596): it raises
                        -- lcTerrainSurfaceMap, which a sim writeback
                        -- overwrites wholesale. Without this the sim kept
                        -- simulating the pre-promotion terrain and its
                        -- next batch flattened the promoted tile back.
                        syncEditToSim (toWorldSimCapability env) pageId
                                      ws lc'
                        bumpQuadCacheGen ws
                        writeIORef (wsZoomQuadCacheRef ws) Nothing
                        writeIORef (wsBgQuadCacheRef ws)   Nothing
                        -- #1858: the promotion raises the surface, so
                        -- re-run the tilled-soil check.
                        _ ← revalidatePlantDesignations logger ws
                        -- #1844: and the structure-plan check, for the
                        -- one tile the promotion raised.
                        _ ← revalidateConstructDesignations env logger ws
                                (ConstructKeys [(tx, ty)])
                        -- Anything standing on the tile rides up.
                        Q.writeQueue unitQ (UnitReGround pageId tx ty)
                        logDebug logger CatWorld $
                            "Spoil promoted to terrain at "
                              <> tshow tx <> "," <> tshow ty
