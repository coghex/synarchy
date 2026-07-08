{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Dig-progress edit handler and its spoil-pile bookkeeping (yield
--   item spawning, full-pile terrain promotion). Split out of
--   "World.Thread.Command.Edit" (issue #563).
module World.Thread.Command.Edit.Dig
    ( handleWorldDigTileCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Unit.Command.Types (UnitCommand(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Generate.Coordinates (globalToChunk)
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
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types (ItemDef(..), ItemInstance(..), lookupItemDef)
import System.Random (randomR)
import World.Spoil.Types (SpoilPile(..), spoilCapacity, depositSpoil
                         , candidateVertices, promotableTiles
                         , debitPromotedTile, tileCornerVertices)
import World.Thread.Command.Edit.Terrain (handleWorldDeleteTileCommand)
import World.Thread.Helpers (unWorldPageId)

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
handleWorldDigTileCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Float → Float → Float → Float → Float → IO ()
handleWorldDigTileCommand env logger pageId gx gy ux uy amount skill percep = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for dig tile: " <> unWorldPageId pageId
        Just ws → do
            desigs ← readIORef (wsMineDesignationsRef ws)
            case HM.lookup (gx, gy) desigs of
                Nothing → pure ()
                Just md → do
                    td0 ← readIORef (wsTilesRef ws)
                    registry ← readIORef (materialRegistryRef env)
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
                        mChunkItem = mDigProps >>= mpDigChunk
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
                               <> T.pack (show gx) <> "," <> T.pack (show gy)
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
                                          <> T.pack (show leftover)
                                          <> " despite capacity check at "
                                          <> T.pack (show gx) <> ","
                                          <> T.pack (show gy)
                                writeIORef (wsSpoilRef ws) piles'
                                -- Promote any tile whose corners
                                -- completed a full level.
                                promoteFullSpoilTiles env logger pageId
                                    ws startV
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
                                    spawnYieldItems env logger ws
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
                                            (statRNGRef env) $ \g →
                                            let (v, g') = randomR
                                                    (0, 1 ∷ Float) g
                                            in (g', v)
                                        when (roll < chance) $
                                            spawnYieldItems env logger ws
                                                gemDef (gx, gy) 1
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
                                    atomicModifyIORef' (wsTilesRef ws) $ \w →
                                        (insertChunk lc' w, ())
                                    bumpQuadCacheGen ws
                                    writeIORef (wsZoomQuadCacheRef ws) Nothing
                                    writeIORef (wsBgQuadCacheRef ws)   Nothing
  where
    (digCoord, (digLx, digLy)) = globalToChunk gx gy
    digIdx = columnIndex digLx digLy

-- | Spawn @n@ yield items (chunks, gems) as ground items scattered
--   on the dig tile. Each gets a random sub-tile position, retried a
--   few times to keep ≥ 0.15 tiles from existing ground items so
--   finds lay out as a scatter instead of a stack. Quality/condition
--   roll from the item def's spec like any other instance.
spawnYieldItems ∷ EngineEnv → LoggerState → WorldState → Text
                → (Int, Int) → Int → IO ()
spawnYieldItems env logger ws defName (gx, gy) n = do
    itemMgr ← readIORef (itemManagerRef env)
    case lookupItemDef defName itemMgr of
        Nothing →
            logWarn logger CatWorld $
                "Dig yield: unknown item def '" <> defName
                  <> "' — dropping " <> T.pack (show n)
        Just iDef → forM_ [1 .. n] $ \_ → do
            qual ← rollItemSpec (idQualitySpec iDef)   (statRNGRef env)
            cond ← rollItemSpec (idConditionSpec iDef) (statRNGRef env)
            wght ← rollItemWeight iDef (statRNGRef env)
            iid ← freshItemInstanceId env
            let inst = ItemInstance
                    { iiDefName     = defName
                    , iiCurrentFill = 0
                    , iiQuality     = qual
                    , iiCondition   = cond
                    , iiWeight      = wght
                    , iiSharpness   = 100.0
                    , iiContents    = []
                    , iiInstanceId  = iid
                    , iiTemp        = Nothing
                    }
            gis ← readIORef (wsGroundItemsRef ws)
            (px, py) ← pickScatterPos env gis
            _ ← atomicModifyIORef' (wsGroundItemsRef ws) $
                    spawnGroundItem inst px py
            pure ()
  where
    -- Up to 6 candidate offsets inside the tile; first one clear of
    -- existing items wins, last candidate is the fallback.
    pickScatterPos env' gis = go (6 ∷ Int)
      where
        clearOf (px, py) = all (\gi →
            let dx = giX gi - px
                dy = giY gi - py
            in dx * dx + dy * dy ≥ 0.15 * 0.15)
            (HM.elems (gisItems gis))
        go k = do
            ox ← atomicModifyIORef' (statRNGRef env') $ \g →
                let (v, g') = randomR (0.15, 0.85 ∷ Float) g in (g', v)
            oy ← atomicModifyIORef' (statRNGRef env') $ \g →
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
promoteFullSpoilTiles ∷ EngineEnv → LoggerState → WorldPageId
    → WorldState → (Int, Int) → IO ()
promoteFullSpoilTiles env logger _pageId ws startV = do
    piles ← readIORef (wsSpoilRef ws)
    _registry ← readIORef (materialRegistryRef env)
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
                        atomicModifyIORef' (wsTilesRef ws) $ \w →
                            (insertChunk lc' w, ())
                        atomicModifyIORef' (wsEditsRef ws) $ \es →
                            (appendEdit coord edit es, ())
                        atomicModifyIORef' (wsSpoilRef ws) $ \sp →
                            (debitPromotedTile tile sp, ())
                        bumpQuadCacheGen ws
                        writeIORef (wsZoomQuadCacheRef ws) Nothing
                        writeIORef (wsBgQuadCacheRef ws)   Nothing
                        -- Anything standing on the tile rides up.
                        Q.writeQueue (unitQueue env) (UnitReGround tx ty)
                        logDebug logger CatWorld $
                            "Spoil promoted to terrain at "
                              <> T.pack (show tx) <> "," <> T.pack (show ty)
