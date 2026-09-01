-- | Construction designation tool (#95). Mirrors the mine designation
--   tool: an anchor→rectangle commit that stores per-tile designations
--   (build target + status + progress) in wsConstructDesignationsRef.
--   The build AI (#96) is the consumer. Split out of
--   "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Construct
    ( handleWorldSetConstructAnchorCommand
    , handleWorldClearConstructAnchorCommand
    , handleWorldDesignateConstructCommand
    , handleWorldCancelConstructCommand
    , handleWorldSetConstructStatusCommand
    , handleWorldAddConstructProgressCommand
    , handleWorldSetConstructDesignateTextureCommand
    , handleWorldSetConstructLineModeCommand
    , popConstructDesignation
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Generate (globalToChunk)
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import World.Construct.Types ( ConstructTarget(..), ConstructStatus(..)
                             , ConstructDesignation(..)
                             , StructurePiece(..)
                             , newConstructDesignation
                             , constructTargetCategory )
import World.Plant.Validate (revalidatePlantDesignations)
import World.Construct.Apply ( applyConstructSlopeToChunk
                             , clearConstructSlope )
import World.Thread.Command.Cursor.Common
    (designateRect, recordDesignationOutcome, recordMissingWorldOutcome)
import Structure.Types (StructureSlot, slotFromText)
import World.Flora.Designation (replaceChunkForgettingFlora)

handleWorldSetConstructAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetConstructAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: canonical anchor, rectangle formed in its frame.
            worldSize ← pageWrapWorldSize worldState
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Just (canonicalTile worldSize gx gy) }, ())
        Nothing → pure ()

handleWorldClearConstructAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → IO ()
handleWorldClearConstructAnchorCommand env _logger pageId = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Nothing }, ())
        Nothing → pure ()

-- | Which structure slot a designation targets, mirroring
--   scripts/unit_ai_construct.lua's placeStructurePiece slot derivation
--   (a wall with no recorded edge defaults to "ne", a post to "n" — the
--   designation tool has no corner picker yet) so occupancy is checked
--   against the exact slot the worker will eventually place into (#805).
structurePieceSlot ∷ StructurePiece → Maybe StructureSlot
structurePieceSlot (StructurePiece _ kind edge) = case kind of
    "floor"   → slotFromText "floor"
    "ceiling" → slotFromText "ceiling"
    "wire"    → slotFromText "wire"
    "wall"    → slotFromText ("wall_" <> fromMaybe "ne" edge)
    "post"    → slotFromText ("post_" <> fromMaybe "n" edge)
    _         → Nothing

-- | Is a structure piece already placed at this (tile, slot)? Reads only
--   the authoritative per-chunk overlay ('lcStructures') — this handler
--   runs on the world thread's single command queue, so any
--   WorldSetStructure queued earlier (e.g. a worker's prior piece
--   placement) has already applied by the time this command runs; there
--   is no need to also consult the Lua read-your-writes staging cache
--   ('wsStructureStageRef'), which exists only for same-tick reads from
--   the debug builder (#805).
--
--   #1175: the tile arrives in the drag's anchor-local alias frame, so
--   both the chunk and the per-tile key are canonicalised — 'lcStructures'
--   is keyed by global tile coord inside the chunk that stores it.
structureOccupiedAt ∷ Int → WorldTileData → Int → Int → StructureSlot → Bool
structureOccupiedAt worldSize tileData gx gy slot =
    let (coord, _, (dgx, dgy)) = canonicalTileFrame worldSize gx gy
        key = (gx + dgx, gy + dgy, fromIntegral (fromEnum slot) ∷ Word8)
    in maybe False (HM.member key . lcStructures) (lookupChunk coord tileData)

-- | Commit a construction designation. Per-z-level like mining: only
--   tiles at the anchor's surface z are taken. STRUCTURE targets fill the
--   whole rectangle (paint a floor / wall run), skipping any tile whose
--   requested slot is already occupied by a placed piece (#805 — a
--   structure designation must never spawn a job that would overwrite an
--   existing floor/ceiling/wall edge/post corner/wire; compatible slots
--   on the same tile, e.g. a floor and a wall, coexist once PLACED, but
--   only one designation at a time may be OUTSTANDING on a tile, because
--   'ConstructDesignations' is keyed by tile coordinate alone — #1595);
--   BUILDING targets mark only the anchor tile (one footprint, not a
--   grid of buildings) and are unaffected by the placed-slot check,
--   though not by the outstanding-designation one.
--   Unloaded-chunk tiles are skipped. Clears the anchor afterwards.
handleWorldDesignateConstructCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Int → ConstructTarget → Maybe Word64 → IO ()
handleWorldDesignateConstructCommand env logger pageId gx1 gy1 gx2 gy2 tgt
                                     mBindGen = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    -- #1602: the page BINDING is re-checked here, not merely where the
    -- command was enqueued. This is the exact commit point for a
    -- designation, and it is EXACTLY serialized against page selection:
    -- world.show / world.hide are world-thread commands drained from the
    -- same queue, so a selection change enqueued before this designation
    -- has already been applied to the snapshot above, and one enqueued
    -- after is genuinely after the commit. A stale binding writes
    -- nothing at all — not on the captured page, not on the newly
    -- selected one. An unbound designation (every AI caller, and the
    -- two-click structure rectangle) is unaffected.
    let bindingMoved = maybe False (≢ wmSelectionGen mgr) mBindGen
    case (if bindingMoved then Nothing else lookup pageId (wmWorlds mgr)) of
        Nothing | bindingMoved →
            logDebug logger CatWorld $
                "Construct designation dropped: page binding stale on "
                <> unWorldPageId pageId
        Nothing → recordMissingWorldOutcome env "construction.designate"
            pageId gx1 gy1
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            worldSize ← pageWrapWorldSize worldState
            -- #1175: canonicalised column read, so an anchor-local alias
            -- resolves the chunk that stores the tile. Identity inland.
            let surfaceZAt gx gy = do
                    let (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
                    lc ← lookupChunk coord tileData
                    pure (lcSurfaceMap lc VU.! columnIndex lx ly)
                ((xLo, yLo), (xHi, yHi)) =
                    designateRect worldSize (gx1, gy1) (gx2, gy2)
                -- A building only ever targets its single anchor tile
                -- (never the swept rectangle), so it always "requests"
                -- exactly 1 regardless of the two-click rectangle size.
                requested = case tgt of
                    CtBuilding _  → 1
                    CtStructure _ → (xHi - xLo + 1) * (yHi - yLo + 1)
                candidates = case surfaceZAt gx1 gy1 of
                    Nothing → []   -- anchor chunk unloaded: nothing
                    Just anchorZ → case tgt of
                        -- A building is a single footprint: only the
                        -- anchor tile, at its own surface z.
                        CtBuilding _ →
                            [ ( canonicalTile worldSize gx1 gy1
                              , newConstructDesignation anchorZ tgt ) ]
                        -- Structure pieces tile the rectangle, per-z-level,
                        -- skipping any tile whose target slot is occupied.
                        CtStructure piece →
                            [ ( canonicalTile worldSize gx gy
                              , newConstructDesignation z tgt )
                            | gx ← [xLo .. xHi]
                            , gy ← [yLo .. yHi]
                            , Just z ← [surfaceZAt gx gy]
                            , z ≡ anchorZ
                            , maybe True
                                (not . structureOccupiedAt worldSize tileData gx gy)
                                (structurePieceSlot piece)
                            ]
                -- #1595: the map is keyed by tile coordinate alone, so a
                -- plain 'HM.insert' would REPLACE whatever job the tile
                -- already carries — silently discarding a claimed and
                -- possibly already-paid designation without the refund and
                -- 'constructAi.abandonClaim' the cancel path
                -- (scripts/build_tool.lua) performs for exactly that
                -- state. Admission therefore treats ANY existing entry as
                -- occupying the tile, whatever its status, progress,
                -- payment marker or target category, and both target
                -- categories go through it.
                addOne (m, n) (k, v)
                    | HM.member k m = (m, n)
                    | otherwise     = (HM.insert k v m, n + 1)
            -- The test-and-insert runs INSIDE the atomicModifyIORef' that
            -- publishes it: 'popConstructDesignation' and the synchronous
            -- Lua verbs mutate this same ref off the world thread, so a
            -- read-then-insert pair would be exactly the race the atomic
            -- delete exists to close.
            applied ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
                \m → foldl' addOne (m, 0 ∷ Int) candidates
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Construct designation: +" <> tshow applied
                <> " tiles (" <> constructTargetCategory tgt <> ")"
            -- Nothing landed and something was excluded post-filter ⇒ every
            -- otherwise-eligible tile was blocked by an existing job, so
            -- say that rather than blaming the placed-slot check (which
            -- for a still-empty tile would be false).
            let blocked = length candidates - applied
            recordDesignationOutcome env "construction.designate"
                (if blocked > 0
                    then "tile already carries an outstanding construction \
                         \designation"
                    else "anchor tile ineligible, unloaded, or requested \
                         \slot already occupied")
                xLo yLo requested applied

handleWorldCancelConstructCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldCancelConstructCommand env _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → void $ popConstructDesignation worldState (gx, gy)
        Nothing → pure ()

-- | Atomically remove a construction designation and reset its
--   corner-progress display, returning the removed designation if any
--   was present. Factored out of 'handleWorldCancelConstructCommand'
--   so a SYNCHRONOUS caller (construction.cancelDesignationForRefund,
--   #799) gets the SAME atomic pop-and-return the
--   queued command does — the atomicModifyIORef' delete is what
--   actually serializes competing cancellations (a rapid double
--   right-click, or a cancel racing the build AI's own CsComplete
--   removal): whichever caller's delete runs first sees Just the
--   removed designation; every other caller (this tick or later) sees
--   Nothing, since there is nothing left to remove. No Lua-side timing
--   heuristic can replicate that guarantee. Since #1595 a NEW
--   designation is not one of those racers — admission refuses a tile
--   that already carries a job rather than replacing it — but the
--   atomicity is what lets the refusal itself be a safe
--   test-and-insert against these same off-thread callers.
--
--   #1175: the tile is canonicalised HERE, once, so both callers — the
--   queued cancel command and the synchronous refund verb — accept any
--   u-alias and resolve the one stored key.
popConstructDesignation ∷ WorldState → (Int, Int) → IO (Maybe ConstructDesignation)
popConstructDesignation worldState (rawGX, rawGY) = do
    worldSize ← pageWrapWorldSize worldState
    let key = canonicalTile worldSize rawGX rawGY
    mCd ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
        \m → (HM.delete key m, HM.lookup key m)
    forM_ mCd $ resetConstructSlope worldState key
    pure mCd

-- | Build AI hook (#96): set a designation's status. Complete removes it
--   (and resets the corner-progress display back to flat ground — the
--   placed piece takes over from there).
handleWorldSetConstructStatusCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → ConstructStatus → IO ()
handleWorldSetConstructStatusCommand env _logger pageId gx gy st = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: a build-AI job coord is a point op like any other.
            worldSize ← pageWrapWorldSize worldState
            let key = canonicalTile worldSize gx gy
            mCd ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
                \m → case st of
                    CsComplete → (HM.delete key m, HM.lookup key m)
                    _          → (HM.adjust (\cd → cd { cdStatus = st })
                                           key m, Nothing)
            forM_ mCd $ resetConstructSlope worldState key
        Nothing → pure ()

-- | Build AI hook (#96): pour progress into a designation. Deltas are
--   normalised to the job's total work (1.0 = done); the accumulated
--   value is clamped to [0, 1]. Completion is NOT triggered here — the
--   build AI watches the value and places the piece itself, then sends
--   CsComplete. Each application re-stamps the tile's corner-progress
--   display (the mining slope-mask pipeline, 'World.Construct.Apply')
--   so the site visibly works corner-by-corner.
handleWorldAddConstructProgressCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Float → IO ()
handleWorldAddConstructProgressCommand env logger pageId gx gy delta = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: a build-AI job coord is a point op like any other.
            worldSize ← pageWrapWorldSize worldState
            let key = canonicalTile worldSize gx gy
            mUpd ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
                \m → case HM.lookup key m of
                    Nothing → (m, Nothing)
                    Just cd →
                        let cd' = cd { cdProgress = max 0.0 (min 1.0
                                          (cdProgress cd + delta)) }
                        in ( HM.insert key cd' m
                           , Just (cdProgress cd, cd') )
            forM_ mUpd $ \(prevProgress, cd') →
                withConstructChunk worldState key $
                    applyConstructSlopeToChunk key prevProgress cd'
            -- #1858: 'applyCornerSlopeToChunk' sheds the tile's surface
            -- vegetation the moment any corner has progressed, so a
            -- build site's own progress write is a way a tile stops
            -- being tilled soil with no vegetation or terrain EDIT
            -- anywhere. ('resetConstructSlope' passes full corners and
            -- therefore never touches ctVeg — see there.)
            _ ← revalidatePlantDesignations logger worldState
            pure ()
        Nothing → pure ()

-- | Run a chunk transform for the designation tile's loaded chunk and
--   invalidate the render caches — the same writeback the live dig
--   path uses ('handleWorldDigTileCommand'). No-op when the chunk
--   isn't loaded (the load path re-derives the display instead).
--
--   Takes a CANONICAL tile coord (#1175): every caller canonicalises
--   before touching the designation map, and the transform it passes
--   ('applyConstructSlopeToChunk' / 'clearConstructSlope') indexes the
--   resolved chunk with the same coord.
withConstructChunk ∷ WorldState → (Int, Int)
                   → (LoadedChunk → LoadedChunk) → IO ()
withConstructChunk worldState (gx, gy) f = do
    let (coord, _) = globalToChunk gx gy
    td ← readIORef (wsTilesRef worldState)
    case lookupChunk coord td of
        Nothing → pure ()
        Just lc → do
            let lc' = f lc
            -- #1854 requirement 16: an edit that takes the tile's
            -- rooted flora with it must take that plant's
            -- designation and regrowth timer too, or an orphan
            -- entry outlives the plant it addressed.
            replaceChunkForgettingFlora worldState lc lc'
            bumpQuadCacheGen worldState
            writeIORef (wsZoomQuadCacheRef worldState) Nothing
            writeIORef (wsBgQuadCacheRef worldState)   Nothing

-- | Reset a removed designation's corner-progress display to flat
--   (guarded inside 'clearConstructSlope' to the designation's own
--   mask, so natural/authored slopes are untouched).
-- Deliberately NOT a #1858 revalidation point: this passes FULL
-- corners, so 'applyCornerSlopeToChunk' leaves 'ctVeg' alone — the
-- vegetation a site shed during prep stays shed, and nothing here can
-- change a tile's tilled-soil answer in either direction.
resetConstructSlope ∷ WorldState → (Int, Int) → ConstructDesignation → IO ()
resetConstructSlope worldState (gx, gy) cd =
    withConstructChunk worldState (gx, gy) $ clearConstructSlope (gx, gy) cd

handleWorldSetConstructDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → Text → TextureHandle → IO ()
handleWorldSetConstructDesignateTextureCommand env _logger pageId cat tid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                case cat of
                    "building" → (cs { constructBuildingTexture = Just tid }, ())
                    _          → (cs { constructStructTexture = Just tid }, ())
        Nothing → pure ()

-- | Wire path tool (#359): toggle the anchor→hover preview between the
--   default filled rectangle and a straight 1-wide line.
handleWorldSetConstructLineModeCommand ∷ EngineEnv → LoggerState
    → WorldPageId → Bool → IO ()
handleWorldSetConstructLineModeCommand env _logger pageId enabled = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructLineMode = enabled }, ())
        Nothing → pure ()
