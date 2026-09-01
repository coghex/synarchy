{-# LANGUAGE Strict #-}
-- | The ONE owning operation for per-instance flora state (#1854).
--
--   Two authorities address a single plant, and this module is the only
--   thing allowed to move either of them:
--
--     * the DURABLE identity-keyed map 'wsChopDesignationsRef' (and its
--       persisted twin @wpsChopDesignations@), which survives chunk
--       eviction because generated chunk data does not; and
--     * the LOADED mirror 'World.Flora.Types.fiChopDesignated' on each
--       'FloraInstance', which the render and query paths read.
--
--   Requirement 8 is that those two can never drift, so they are never
--   written separately: 'setChopDesignations' writes the durable map
--   and re-hydrates exactly the chunks the change touched, in one
--   operation, and 'admitChunkFlora' hydrates a chunk from the durable
--   map before that chunk is ever exposed to a runtime consumer.
--
--   The module also owns the two legacy-migration drains (requirements
--   12, 13 and 15) and the removal sweep (requirement 16).
module World.Flora.Designation
    ( -- * The owning write
      setChopDesignations
    , designateChopInstances
    , cancelChopForInstance
    , cancelChopAtTile
      -- * Chunk admission
    , admitChunkFlora
    , admitChunkFloraBatch
    , hydrateChunkChopFlags
      -- * Removal
    , forgetFloraInstances
    , forgetFloraDroppedSince
    , replaceChunkForgettingFlora
      -- * Shared selection rules
    , legacyChopTargetOnTile
    , harvestableInstancesOnTile
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Log (logWarn, LogCategory(..), LoggerState)
import World.Chop.Types
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..),
                          wrapChunkCoordU)
import World.Flora.Identity (FloraInstanceId)
import World.Flora.Types
import World.Generate.Coordinates (canonicalTile, globalToChunk)
import World.State.Types (WorldState(..), pageWrapWorldSize)
import World.Tile.Types (WorldTileData(..), insertChunk)

-- * The owning write

-- | Write a batch of designation changes: @Just cd@ designates the
--   named instance, @Nothing@ cancels it. The durable map and every
--   affected loaded chunk's 'fiChopDesignated' mirror move together, so
--   no caller can update one and forget the other (requirement 8).
--
--   Each entry carries the plant's CANONICAL tile so only the chunks
--   actually involved are re-hydrated — an id alone would force a scan
--   of every resident chunk on every felled tree.
setChopDesignations
    ∷ WorldState
    → [(FloraInstanceId, (Int, Int), Maybe ChopDesignation)]
    → IO ()
setChopDesignations _ [] = pure ()
setChopDesignations ws changes = do
    worldSize ← pageWrapWorldSize ws
    desigs ← atomicModifyIORef' (wsChopDesignationsRef ws) $ \m →
        let m' = foldl' step m changes
        in (m', m')
    let coords = touchedChunks worldSize [ tile | (_, tile, _) ← changes ]
    rehydrateChunks ws desigs coords
  where
    step m (iid, _, Just cd) = HM.insert iid cd m
    step m (iid, _, Nothing) = HM.delete iid m

-- | Designate every listed instance. The tile each carries is
--   canonicalised into the designation itself, so a marker and a
--   nearest-designation scan always read one frame (#1175).
designateChopInstances
    ∷ WorldState → [(FloraInstanceId, Int, Int, Int)] → IO ()
designateChopInstances ws entries = do
    worldSize ← pageWrapWorldSize ws
    setChopDesignations ws
        [ let (cgx, cgy) = canonicalTile worldSize gx gy
          in (iid, (cgx, cgy), Just (newChopDesignation z cgx cgy))
        | (iid, gx, gy, z) ← entries ]

-- | Cancel exactly one plant's designation (the chop AI's completion
--   and the exact-instance player cancel).
cancelChopForInstance ∷ WorldState → FloraInstanceId → IO ()
cancelChopForInstance ws iid = do
    desigs ← readIORef (wsChopDesignationsRef ws)
    case HM.lookup iid desigs of
        Nothing → pure ()
        Just cd → setChopDesignations ws
            [(iid, chopDesignationTile cd, Nothing)]

-- | Cancel every designation standing on one tile — the player's
--   tile-granularity cancel gesture, which predates instance identity
--   and must keep clearing what the player pointed at. Accepts any
--   u-alias of the tile (#1175).
cancelChopAtTile ∷ WorldState → Int → Int → IO ()
cancelChopAtTile ws gx gy = do
    worldSize ← pageWrapWorldSize ws
    desigs ← readIORef (wsChopDesignationsRef ws)
    -- The pending legacy map is tile-keyed and must be cancellable too:
    -- leaving an entry there would resurrect a designation the player
    -- cancelled the moment its chunk loaded.
    let tile = canonicalTile worldSize gx gy
    atomicModifyIORef' (wsPendingChopMigrationRef ws) $ \m →
        (HM.delete tile m, ())
    setChopDesignations ws
        [ (iid, chopDesignationTile cd, Nothing)
        | (iid, cd) ← HM.toList desigs
        , chopDesignationTile cd ≡ tile ]

-- * Chunk admission

-- | Hydrate one chunk's 'fiChopDesignated' mirrors from the durable
--   map. Pure, so the chunk-load pipeline can fold it in beside its
--   other per-chunk repairs.
hydrateChunkChopFlags ∷ ChopDesignations → LoadedChunk → LoadedChunk
hydrateChunkChopFlags desigs lc
    | HM.null desigs
    , not (any fiChopDesignated (fcdInstances (lcFlora lc))) = lc
    | otherwise = lc { lcFlora = FloraChunkData (map hydrate insts) }
  where
    insts = fcdInstances (lcFlora lc)
    hydrate fi =
        let designated = HM.member (fiInstanceId fi) desigs
        in if designated ≡ fiChopDesignated fi
           then fi else fi { fiChopDesignated = designated }

-- | Admit one freshly generated (or freshly replayed) chunk to
--   residency: drain any pending legacy migration whose tiles this
--   chunk owns, then hydrate the designation mirrors.
--
--   Requirement 15: this must run BEFORE the chunk is inserted into
--   'wsTilesRef', so no Chop, forage, render or regrowth consumer can
--   ever observe a resident chunk whose per-instance state has not been
--   resolved.
admitChunkFlora
    ∷ WorldState → FloraCatalog → LoggerState → LoadedChunk
    → IO LoadedChunk
admitChunkFlora ws cat logger lc = do
    resolveLegacyChop ws cat logger lc
    resolveLegacyHarvests ws cat logger lc
    desigs ← readIORef (wsChopDesignationsRef ws)
    pure (hydrateChunkChopFlags desigs lc)

-- | 'admitChunkFlora' over a batch, in list order.
admitChunkFloraBatch
    ∷ WorldState → FloraCatalog → LoggerState → [LoadedChunk]
    → IO [LoadedChunk]
admitChunkFloraBatch ws cat logger = mapM (admitChunkFlora ws cat logger)

-- * Legacy migration drains

-- | Pre-#1854 chop designations were tile-keyed. Each resolves to the
--   SINGLE instance the old wood-tagged harvest would have felled:
--   'legacyChopTargetOnTile' reproduces that selection exactly.
--   A tile that resolves with no matching plant is discarded with a
--   diagnostic rather than left pending for ever (requirement 12).
resolveLegacyChop
    ∷ WorldState → FloraCatalog → LoggerState → LoadedChunk → IO ()
resolveLegacyChop ws cat logger lc = do
    pending ← readIORef (wsPendingChopMigrationRef ws)
    unless (HM.null pending) $ do
        worldSize ← pageWrapWorldSize ws
        let mine = [ (tile, cd)
                   | (tile, cd) ← HM.toList pending
                   , chunkOwnsTile worldSize lc tile ]
        unless (null mine) $ do
            resolved ← forM mine $ \(tile@(gx, gy), cd) →
                case legacyChopTargetOnTile cat lc worldSize gx gy of
                    Just fi → pure (Just (fiInstanceId fi, tile, Just cd))
                    Nothing → do
                        logWarn logger CatWorld $
                            "Discarding legacy chop designation at "
                            <> tshow gx <> "," <> tshow gy
                            <> ": the tile holds no wood-tagged flora"
                        pure Nothing
            atomicModifyIORef' (wsPendingChopMigrationRef ws) $ \m →
                (foldl' (flip HM.delete) m (map fst mine), ())
            setChopDesignations ws (catMaybes resolved)

-- | Pre-#1854 regrowth timers were tile-keyed and applied to EVERY
--   harvestable plant on the tile — that is what the render pass did
--   with them, so the migration reproduces it: one legacy timer becomes
--   the same remaining time on every harvestable instance there, and
--   decorative co-tenants stay untouched (requirement 13).
resolveLegacyHarvests
    ∷ WorldState → FloraCatalog → LoggerState → LoadedChunk → IO ()
resolveLegacyHarvests ws cat logger lc = do
    pending ← readIORef (wsPendingFloraHarvestsRef ws)
    unless (HM.null pending) $ do
        worldSize ← pageWrapWorldSize ws
        let mine = [ (tile, t)
                   | (tile, t) ← HM.toList pending
                   , chunkOwnsTile worldSize lc tile ]
        unless (null mine) $ do
            expanded ← fmap concat $ forM mine $ \((gx, gy), t) →
                case harvestableInstancesOnTile cat lc worldSize gx gy of
                    [] → do
                        logWarn logger CatWorld $
                            "Discarding legacy flora regrowth timer at "
                            <> tshow gx <> "," <> tshow gy
                            <> ": the tile holds no harvestable flora"
                        pure []
                    fis → pure [ (fiInstanceId fi, t) | fi ← fis ]
            atomicModifyIORef' (wsPendingFloraHarvestsRef ws) $ \m →
                (foldl' (flip HM.delete) m (map fst mine), ())
            atomicModifyIORef' (wsFloraHarvestsRef ws) $ \m →
                (foldl' (\acc (iid, t) → HM.insert iid t acc) m expanded, ())

-- * Shared selection rules

-- | Every harvestable-SPECIES instance standing on a tile, in the
--   chunk's own deterministic instance order.
harvestableInstancesOnTile
    ∷ FloraCatalog → LoadedChunk → Int → Int → Int → [FloraInstance]
harvestableInstancesOnTile cat lc worldSize gx gy =
    [ fi
    | fi ← instancesOnTile lc worldSize gx gy
    , Just sp ← [lookupSpecies (fiSpecies fi) cat]
    , isJust (fsHarvest sp)
    ]

-- | The plant a pre-#1854 @world.harvestFlora(gx, gy, "wood")@ would
--   have taken: the FIRST instance on the tile, in the chunk's stored
--   order, whose species carries the @wood@ harvest tag. Deterministic
--   chunk data, never hash order — the same choice
--   "Engine.Scripting.Lua.API.Forage.Harvest" made with @listToMaybe@.
legacyChopTargetOnTile
    ∷ FloraCatalog → LoadedChunk → Int → Int → Int → Maybe FloraInstance
legacyChopTargetOnTile cat lc worldSize gx gy = listToMaybe
    [ fi
    | fi ← instancesOnTile lc worldSize gx gy
    , Just sp ← [lookupSpecies (fiSpecies fi) cat]
    , Just fh ← [fsHarvest sp]
    , "wood" `elem` fhTags fh
    ]

instancesOnTile ∷ LoadedChunk → Int → Int → Int → [FloraInstance]
instancesOnTile lc worldSize gx gy =
    let (cgx, cgy) = canonicalTile worldSize gx gy
        (_, (lx, ly)) = globalToChunk cgx cgy
    in filter (floraInstanceOnTile lx ly) (fcdInstances (lcFlora lc))

-- | Does this chunk own the canonical image of the given tile? Compared
--   through the wrapped chunk key, so a seam page's stored chunk still
--   claims the tiles a raw coordinate names.
chunkOwnsTile ∷ Int → LoadedChunk → (Int, Int) → Bool
chunkOwnsTile worldSize lc (gx, gy) =
    let (cgx, cgy) = canonicalTile worldSize gx gy
        (coord, _) = globalToChunk cgx cgy
    in wrapChunkCoordU worldSize coord ≡ lcCoord lc

-- * Removal

-- | Drop every trace of the named instances: their durable designation,
--   their regrowth timer, and (through the designation) the Lua chop
--   claim, which releases itself the moment its designation is gone.
--   No orphan identity-keyed entry may outlive the plant (requirement
--   16).
forgetFloraInstances ∷ WorldState → [FloraInstanceId] → IO ()
forgetFloraInstances _ [] = pure ()
forgetFloraInstances ws iids = do
    atomicModifyIORef' (wsFloraHarvestsRef ws) $ \m →
        (foldl' (flip HM.delete) m iids, ())
    desigs ← readIORef (wsChopDesignationsRef ws)
    setChopDesignations ws
        [ (iid, chopDesignationTile cd, Nothing)
        | iid ← iids, Just cd ← [HM.lookup iid desigs] ]

-- | Forget the per-instance state of every plant that was in the
--   ADMITTED chunks but is gone from @final@ — the tile data the chunk
--   pipeline is about to commit.
--
--   'admitChunkFlora' runs BEFORE a chunk is inserted (requirement 15),
--   but the passes that follow the insert are not all non-destructive:
--   'World.Mine.Apply.applyDigSlopesTd' and
--   'World.Construct.Apply.applyConstructSlopesTd' re-apply mid-dig and
--   build-progress corner masks, and a progressed tile sheds its rooted
--   flora. Most of the time that plant's state was already cleared when
--   the dig actually happened, and the regenerated copy simply goes
--   again — but a legacy PENDING entry that admission just resolved onto
--   it would be left addressing a plant this very transaction removed.
--   This is the sweep that closes that window, and it runs on the
--   committed result rather than guessing which pass dropped what.
--   A chunk that is ABSENT from @final@ was EVICTED in the same
--   transaction, not stripped of its flora, and is skipped: an evicted
--   plant still exists and keeps its designation and its timer, which is
--   the whole reason those maps are world-level rather than chunk-local.
forgetFloraDroppedSince
    ∷ WorldState → [LoadedChunk] → WorldTileData → IO ()
forgetFloraDroppedSince ws admitted final =
    forgetFloraInstances ws
        [ fiInstanceId fi
        | lc ← admitted
        , Just lc' ← [HM.lookup (lcCoord lc) (wtdChunks final)]
        , let kept = HM.fromList
                  [ (fiInstanceId f, ()) | f ← fcdInstances (lcFlora lc') ]
        , fi ← fcdInstances (lcFlora lc)
        , not (HM.member (fiInstanceId fi) kept)
        ]

-- | Replace a chunk in the tile store, forgetting the per-instance
--   state of every plant the replacement dropped.
--
--   Every in-place chunk mutation goes through here rather than a bare
--   'insertChunk': digging a tile out, burying it under a spoil pile
--   and mining its corners all take the rooted flora with them, and a
--   designation or regrowth timer left behind would address a plant
--   that no longer exists. Chunk EVICTION is deliberately not this
--   path — an evicted plant still exists and keeps its state.
replaceChunkForgettingFlora
    ∷ WorldState → LoadedChunk → LoadedChunk → IO ()
replaceChunkForgettingFlora ws old new = do
    atomicModifyIORef' (wsTilesRef ws) $ \w → (insertChunk new w, ())
    let kept = HM.fromList
            [ (fiInstanceId fi, ()) | fi ← fcdInstances (lcFlora new) ]
        gone = [ fiInstanceId fi
               | fi ← fcdInstances (lcFlora old)
               , not (HM.member (fiInstanceId fi) kept) ]
    forgetFloraInstances ws gone

-- * Internals

touchedChunks ∷ Int → [(Int, Int)] → [ChunkCoord]
touchedChunks worldSize tiles = HM.keys $ HM.fromList
    [ (wrapChunkCoordU worldSize coord, ())
    | (gx, gy) ← tiles
    , let (cgx, cgy)  = canonicalTile worldSize gx gy
          (coord, _)  = globalToChunk cgx cgy
    ]

rehydrateChunks ∷ WorldState → ChopDesignations → [ChunkCoord] → IO ()
rehydrateChunks _ _ [] = pure ()
rehydrateChunks ws desigs coords =
    atomicModifyIORef' (wsTilesRef ws) $ \td →
        (td { wtdChunks = foldl' rehydrate (wtdChunks td) coords }, ())
  where
    rehydrate m coord = case HM.lookup coord m of
        Nothing → m
        Just lc → HM.insert coord (hydrateChunkChopFlags desigs lc) m
